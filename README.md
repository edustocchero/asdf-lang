# ASDF Lang

ASDF is a ML like experimental language, it has a hindley-milner type system and can evaluate expressions.

## Running

With `opam` and `dune` installed you can follow the steps below:

Clone the repo:
```bash
git clone git@github.com:edustocchero/asdf-lang.git
```

Enter in the folder:
```bash
cd asdf-lang
```

Create a new opam switch:
```bash
opam switch create .
```

Execute with dune:
```bash
dune exec asdf
```

## Examples

```ml
> true
true

> let id = |x| x in id 2
2

> let res = inc (2 * 3 + 4) in res
11
```

### Roadmap

- [x] Parsing
- [x] Basic literal types
- [x] Evaluate Expressions
- [x] Type checking
- [ ] More literal types
- [ ] Useful built-in functions
- [ ] Top level expressions
- [ ] Algebraic Data Types
- [ ] Modules
