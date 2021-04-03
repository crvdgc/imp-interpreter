# Interpreter for the IMP language

## Install

Build and install with

```sh
$ stack install
```

## Usage

```sh
$ imp-interpret sum.impast
```

Notice, the input file format is not an IMP program, but a already parsed syntax tree. It will be directly read as `IMP.Syntax`

See the [defininition of IMP](https://github.com/kframework/k/blob/aae444d74cfc838b499d7cbb52a49b870baeb27c/k-distribution/pl-tutorial/1_k/2_imp/lesson_5/imp.md).

