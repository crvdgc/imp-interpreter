# Interpreter for the IMP language

## Usage

### Provided tests

```sh
$ stack install; stack test
```

This will run a test for three `.imp` files in `test/files/`.

### Write a new program

Since we are not parsing the `.imp` file, a new `.imp` file needs to be written in `test/Test/Files.hs`.

There are already three test files from `test/files`. The syntax is defined in `src/IMP/Syntax.hs`. It also provides some helper functions (macros) to write a syntax tree.


```hs
sumPgm :: Pgm T.Text
sumPgm = Pgm
  { decls = ["n", "sum"]
  , stmt  = stmts
    [ "n" `assignInt` 100
    , "sum" `assignInt` 0
    , SWhile (BNeg $ "n" `leInt` 0) . Just $ stmts
      [ SAssign "sum" $ AVar "sum" `AAdd` AVar "n"
      , SAssign "n" $ AVar "n" `AAdd` ANeg 1
      ]
    ]
  }
```

In the above example, a program is a record of variable declarations and a statement.

All variable that's used in the program must be declared first.

A non-empty block `{S}` is represented by `Just S`, and an empty one `{}` is `Nothing`.

`stmts` will collect a list of statements and `SSeq` them into one. `leInt` is the comparison of a variable and an Int. Outside macro functions, all literal values must be wrapped with the corresponding constructor.

After you wrote a new program, export it. In `test/Spec.hs`, you can import them and write tests.

### Run a new program

#### Test against a reference

To do this, first you need to provide a reference result set.

```hs
sumRes :: [(T.Text, KResult)]
sumRes = [ ("sum", KRInt 5050)
         ]
```

This asserts a subset of variable values that the final state must has.

In the `functionalTests` group, add a line like

```hs
  , testCase "test sum" . assertBool "sum is consistant with result" $
      primesPgm `checkAgainst` primesRes
```

to check the interpreted program with the result.

#### Inspect the final result

To the entire final result, add a line like

```hs
  , testCase "print sum" $ assertBool (show $ interpret sumPgm) False
```

This will make the test case fail and prints out the resulting configuration.

You can also use

```sh
$ stack ghci imp-interpreter:test:imp-interpreter-test
```

to enter a testing repl. And then

```
> interpret sumPgm
```

#### Inspect execution trace

It is also possible to show the trace of program execution, enter the testing repl as above

```sh
$ stack ghci imp-interpreter:test:imp-interpreter-test
```

Then

```hs
> s = iteratively nextComputationStep . fst . initializeConfig $ sumPgm
```

will give you an infinite list of execution configurations. You can use `take 5 s` like

```hs
> take 5 s
[Just (Config {kCell = $0 = 100;
$1 = 0;
while (!$0 <= 0)
{Just $1 = $1 + $0;
$0 = $0 + -1;}, state = fromList [(0,KRInt 0),(1,KRInt 0)]}), ...
```

to inspect the first 5 steps. After there are no more rules to apply, the stream will become a stream of `Nothing`.

This will only give you computational rule steps. To also see the structural rule steps, use

```
> s = iteratively (applyRules $ structuralRules ++ computationRules) . fst . initializeConfig $ sumPgm
```

## Implementation

### High-level structure

The module contains four parts: `IMP.Syntax`, `IMP.Semantics`, `IMP.Pattern`, and `IMP.Exception`.

The syntax and exception modules are pretty straightforward. The `IMP.Pattern` defines a class of *lens* like combinators for pattern matching on the syntax tree, which is used in `IMP.Semantics`.

### Pattern combinators

I suspect they are actually *prisms*, however, I haven't reached the prism part of the book *Optics by Examples*, so I don't know for sure.

```hs
type MatchInto s t a b = (a -> Maybe b) -> s -> Maybe t
type MatchInto' s a = MatchInto s s a a
type MatchSelf a = MatchInto' a a
```

If the structure `s` has a substructure `a`, it can use a function to potentially produce a value `b`. If this succeeds, the overall structure will change to `t`.

If `a` is not in `s` or the function fails, the overall structure construction will also fail.

This captures the idea of pattern matching and evaluation.

We can use the usual function composition `(.)` to combine `MatchInto'`

```hs
aToB :: MatchInto' a b
bToC :: MatchInto' b c

aToC :: MatchInto' a c
aToC = aToB . bToC
```

`MatchSelf` just says that the pattern is of the sum type of the overall structure. They are not necessarily the same. (e.g. `aLit :: MatchSelf (AExp v)`)

A special case is when the structure is recursively defined, then we can recursively match all the occurrences of the substructure.

```hs
class RecursiveMatch a where
  recursiveMatch :: MatchSelf a
  recursiveMatch f e = f e <|> subMatch (recursiveMatch f) e

  subMatch       :: MatchSelf a
```

The minimal complete definition is `subMatch` which only specifies the substructures, `recursiveMatch` is the fixed point of anamorphism matching on `subMatch`.

We can then use them to construct composed patterns


```hs
-- | succeeds if any pattern succeeds
possibly :: [MatchInto' s a] -> MatchInto' s a
possibly patterns f s = foldr ((<|>) . (\p -> s & p f)) Nothing patterns

-- | All possible ways to match from @AExp@ to @AVar@, the others are similarly named
aExpAVar :: MatchInto' (AExp v) (AExp v)
aExpAVar = recursiveMatch . aVar
```

Then the implementation of semantics is straightforward. For example:

```hs
type Rule = Config (Stmt IdKey) -> Maybe (Config (Stmt IdKey))

-- | int division
ruleIntDiv :: Rule
ruleIntDiv = matchK . stmtAExp $ \case
  t@(ADiv (ALit _) (ALit 0)) -> throw $ IEDividedByZero (show t)
  (ADiv (ALit i1) (ALit i2)) -> Just $ ALit (i1 `div` i2)
  _                          -> Nothing
```

We use the pattern `matchK . stmtAExp` to match all `AExp` in a statement that's wrapped by a K cell.

Then it's only a match if the pattern in the host language matches, then we deal with it.

Here I use the literal value constructors `ALit` and `BLit` to store evaluation results, which helps to avoid writing a partially evaluated syntax tree, but is not actually general.

The interpreter just iteratively apply these rules until it either converges or throw an exception.

