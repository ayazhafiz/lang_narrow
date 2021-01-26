# lang_narrow

A language with flow typing and structural type narrowing. [Try it out](https://ayazhafiz.com/lang_narrow).

See [this blog post](https://ayazhafiz.com/articles/21/lang-narrow) for a
thorough overview, or the rest of the readme for a simpler one.

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Introduction](#introduction)
- [Examples](#examples)
- [Motivations + timeline + contributing](#motivations--timeline--contributing)
- [Running the compiler, and its targets](#running-the-compiler-and-its-targets)
- [Appendix](#appendix)
  - [Types](#types)
  - [Language syntax](#language-syntax)
  - [Points of interest in the compiler code](#points-of-interest-in-the-compiler-code)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Introduction

This is a simple language that exhibits structural type narrowing. Narrowing is
done at branch sites, can be performed

- using the `is` narrower, which narrows exactly against a type
- using the `in` narrower, which narrows partially against a record field

For example, in this language the `is` structural narrower works in like

```
// suppose a :: string|nat|bool
if a is string|nat
   then ... // a is a string|nat in this branch
   else ... // a is a bool in this branch
```

and the `in` narrower works like

```
// suppose a :: string|{fa: nat, data: nat}|{fb: nat, data: string}
if data in a
   then ... // a is a {fa: nat, data: nat}|{fb: nat, data: string} in this branch
   else ... // a is a string in this branch
```

With these constructs, we can construct more powerful type narrowing programs.
See the section on examples for more details.

The rest of the README delves deeper into the operation of the language and
usage of the compiler.

If you would like to contribute to this project, please feel free to do so
(as I will mention many times in the README!); there are some ideas of where to
get started in the timeline section, but obviously you are welcome to contribute
anything of any kind.

If you have any questions, feel free to open an issue.

## Examples

See the [examples/](./examples) folder, which contains some language examples in
`.nl` files along with their output by way of evaluation, c codegen, and emit of
generated c program.

See the section on running the compiler for an annotated example of running a
program.

## Motivations + timeline + contributing

One motivation is to come up with a sound type system for [`gb`](https://github.com/ayazhafiz/hgb).
Another is because this kind of type system is a fun thing to explore :-).
Much of this language is inspired by TypeScript, but the goals are smaller in
scope and strive to be [more sound](https://githhub.com/ayazhafiz/rats).

There's no real timeline because this is clearly a toy/exploratory project, but
some ideas are

- Formal writeup of typing rules (read: a latex pdf)
- (*) Linear, relevant, and quantitive types
- (*) Named (aliased) types
- (*) Recursive types (nearly trivial with the above)
- (*) Built-in functions and operators (string concat, addition, etc)
- (*) Fresh identifier generation (see TODOs in [codegen](./src/codegenC.ml))
- (*) Any/all of the TODOs in the code

If you would like to contribute to this project, (1) any contributions are
welcome and (2) any of the above ideas, especially those marked with (*) may be
fun/relatively easy places to start!

But before that, maybe you'd like to know how to run the compiler...

## Running the compiler, and its targets

We will assume the alias `alias run=dune exec src/main.exe`. This must be run
from the repository root!

To compile a single file, do `run <path_to_file>`. There are three compilation
modes, each settable with via `-M` flag:

- `eval`: no compilation actually done; emits typechecked program functions and
    evaluates the program.
- `codegenC`: typechecks the program and emits a C program compilable when
    appended to the [runtime](./src/runtime.c).
- `codegenC-rt`: like `codegenC`, but also prepends the runtime.

To start the compiler as a repl, execute `run` without a file argument. Here is
an annotated sample repl usage, also demonstrating the `eval` and `codegenC`
output:

```
> fn defaultNat(): nat {
  1729
}

fn readNat(n: nat|string): nat {
  if n is nat
     then n
     else defaultNat()
}

fn narrowB(p: nat|{a: bool, b: nat}|{b: string, c: nat}): nat {
  if b in p
     then readNat(p.b)
     else p
}

narrowB({b: "not a nat" , c: 9})
;; // <-- terminate your program with ;;, or Ctrl-D
   // let's call the above program <first_program>

// mode: eval output
defaultNat :: (): nat
readNat :: (nat|string): nat
narrowB :: (nat|{a: bool, b: nat}|{b: string, c: nat}): nat
1729 :: nat

> :mode codegenC;; // sets the mode to codegenC. Again, terminate wth ;;

Set mode "codegenC"

> <first_program>;;

> tagged_any _defaultNat() {
  return make_nat(1729);
}
tagged_any _readNat(tagged_any _n) {
  tagged_any _fresh_0;
    if (is(_n, NAT)) {
      _fresh_0 = _n;
  } else {
      _fresh_0 = _defaultNat();
  }
  return _fresh_0;
}
tagged_any _narrowB(tagged_any _p) {
  tagged_any _fresh_1;
    if (in(_p, "b")) {
      _fresh_1 = _readNat(record_proj(_p, "b"));
  } else {
      _fresh_1 = _p;
  }
  return _fresh_1;
}
int main() {
  tagged_any __main_result = _narrowB(make_record(2, "b", make_string("not a nat"), "c", make_nat(9)));
  print(__main_result);
}
```

## Appendix

### Types

- `unknown`: the top type, inhabited by all terms
- `never`: the bottom type, inhabited by no terms
- `nat`: a natural number
- `string`: a string
- `bool`: a boolean
- record types, of the form `{ fa: tyA, fb: tyB }`
- union types, of the form `tyA|tyB|tyC`

There are also some hidden (compiler-internal) types. See the [`type`
datatype](./src/language.ml) for more.

### Language syntax

See the [parser](./src/parser.mly) and [`expr`/`fn` datatypes](./src/language.ml)
for the full language specification. As a brief overview:

- functions of the form `fn a(paramA: tyA, paramB: tyB): tyReturn { <expr> }`
- expressions
   - variables, for example `paramA` if used in the function above
   - nat/string/boolean literals, for example `10`/`"hi"`/`true`
   - function applications, for example `a(10, "hi")`
   - conditionals, of the form `if <cond> then <exprLeft> else <exprRight>`
       `<cond>` can be a narrowing expression (see below) or of a boolean type
   - record constructions, for example `{fa: exprA, fb: exprB}`
   - record projections, for example `{fa: exprA, fb: exprB}.fa`
- narrowing expression
   - type narrow, for example `a is string|nat`, which when used in a conditional
       narrows `a` to `string|nat` in the left branch and the narrows `a` to the
       type of `a` excluding `string|nat` in the right branch
   - record field narrow, for example `fa in r` which when used in a conditional
       narrows `a` to its composed types containing the field `fa` in the left
       branch and narrows `a` to its composed types not containing the field `fa`
       in the right branch
- compilation modes (only relevant in the repl)
   - `:mode eval` sets the compiler mode to direct evaluation of a program
   - `:mode codegenC` compiler mode that generates C code for a program,
       excluding the runtime preamble
   - `:mode codegenC-rt` like `:mode codegenC`, but includes the runtime
       preamble

### Points of interest in the compiler code

Obviously, by far the most interesting part of this compiler is the [typechecker](./src/typecheck.ml),
which performs subtyping, structural typing, meets, joins, exclusions, and all
other kinds of type manipulation galore. Honestly, the typechecker is part that
makes me the most pumped about the entire language. Of course, there are things
to fix; see the timeline section above. You are very welcome to fix them; just
please upstream me a patch, I would adore you so much!

The [C code generator](./src/codegenC.ml) and [C runtime](./src/runtime.c) are
kind of interesting, in that I didn't expect the runtime and code generation to
be so simple! As expected the runtime does all the heavy lifting, but even then
it's not that heavy: we just chuck all values into an `any` union and extract
them by checking a type tag.

The [evaluator](./src/eval.ml) is maybe interesting, but probably not; I guess
the only point I have to make here is that it's really small. But then again, so
is the language.

As mentioned before, I would love to have your contributions if you have any to
make! I hope this has been interesting and fun. If you have any questions, feel
free to open an issue.
