Runtime Verification develops the [K Framework](https://runtimeverification.com/k/),
a system for formally specifying the syntax and semantics of programming languages.
From the definition of a language, we can _derive_ parsers, compilers, interpreters, and program verification tools in a language-agnostic way.

We are hiring for a role on the team that develops the symbolic execution engine of the K Framework, called Kore.
Kore powers compiler optimization, program verification, symbolic model checking, and more.

In this technical assessment, you will write an interpreter in Haskell for a simple imperative programming language called IMP.

The syntax and semantics of IMP are [defined](https://github.com/kframework/k/blob/aae444d74cfc838b499d7cbb52a49b870baeb27c/k-distribution/pl-tutorial/1_k/2_imp/lesson_5/imp.md) in a literate-style K definition.
As you read the definition of IMP, here are three kinds of statements you should pay attention to:

- `syntax` statements describe the syntax of the language in BNF.
  The `Pgm` (or "program") statement defines the root of the syntax tree.
- The `configuration` statement describes the initial state of the interpreter using an XML-like syntax.
- `rule` statements give the rewrite rules that comprise the semantics of IMP.
  The left-hand side of the rule is a pattern to match in the configuration
  and the right-hand side is the updated configuration.
  Rules may refer to cells in the configuration.
  If a rule does not refer to cells in the configuration, it is assumed to apply to the `<k>` cell.

The definition comes with several [programs](https://github.com/kframework/k/tree/a50909a2ccc426b551aa7f84d7387880b25d1c39/k-distribution/pl-tutorial/1_k/2_imp/lesson_5/tests) to test your interpreter:

- `collatz.imp`: A demonstration of the Collatz conjecture.
- `primes.imp`: Count prime numbers within a bound.
- `sum.imp`: Compute the sum of consecutive integers.

Your submission should include an automated test suite which tests at least these programs.
We will test your submission against other programs.
You should not write a parser: the test programs are simple and you should "parse" the programs by hand for testing purposes.
Your interpreter should take a syntax tree representing an IMP program and return the final program state ("configuration") after execution.
In other words, the interface to your interpreter may be as simple as a function,

```.hs
interpret :: ImpProgram -> Configuration  -- Feel free to change this! Don't be constrained.
```

This project should not be a large undertaking.
We are not looking for an advanced solution:
use your best judgement about what language features are appropriate, but don't make this complicated.
We want to see that you are proficient with Haskell,
that you are familiar with designing and implementing programming languages,
and that you are comfortable with engineering best practices (e.g. good error handling, test-driven development).

You can send your solution to me; my contact information is below.
Please submit a compressed `tar` archive containing your project;
it should be buildable with Cabal or Stack.

Please do not hesitate to contact me with any questions!

---

Thomas Tuegel<br/>
Software Engineer<br/>
<thomas.tuegel@runtimeverification.com><br/>
