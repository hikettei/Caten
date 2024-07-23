# Caten

~~Programmable Deep Learning Framework~~

a place to test my idea.

## Project Structure (WIP)

Everything is a syntax sugar for ./source/air.

```
------------------------------------------------------------------------------------
./source/air     | An optimized pattern matcher for DAG, the core of IR system.
./source/aasm    | Abstract Internal IR for aVM 
./source/avm     | Abstract+VM, an extensible simulator for aasm (but enough fast)
./source/ajit    | it lowers aasm into more lower irs, generating the kernel codes.
--- core ---------------------------------------------------------------------------
./source/lang    | a compiler from lisp to aIR (helping the complicated subscript notation like Conv/einsum) 
./source/api     | Syntax sugar for core notations, including AbstractTensor, autodiff engine.
./source/nn      | a syntax sugar which implements nn ops and optimizers
--- external -----------------------------------------------------------------------
./external/avm/clang  | avm implementation using clang
./external/ajit/clang | ajit implementation using clang
...
------------------------------------------------------------------------------------
```

## TODO

- pre-commit

## Testing

```sh
$ COVERAGE=1 make test
```

## The Goal

- Export To C Mode

- "compilation time" graph simplification and type inference

- beautiful docgen system

- Numpy Style + Trace from lisp mode

- Implement approxs for sin/log2/exp2

## Installing (WIP)

```sh
$ ros install hikettei/caten
$ caten --help
```

