# Caten

~~Programmable Deep Learning Framework~~

a place to test my idea.

## Project Structure (WIP)

Everything is a syntax sugar for ./source/air.

```
--- core ---------------------------------------------------------------------------
0 | ./source/air     | An optimized pattern matcher for DAG, the core of IR system.
1 | ./source/aasm    | Abstract Internal IR for aVM 
2 | ./source/avm     | Abstract+VM, an extensible simulator for aasm (but enough fast)
3 | ./source/ajit    | it lowers aasm into more lower irs, generating the kernel codes.
--- frontend -----------------------------------------------------------------------
4 | ./source/lang    | a compiler from lisp to aIR (helping the complicated subscript notation like Conv/einsum) 
5 | ./source/api     | Syntax sugar for core notations, including AbstractTensor, autodiff engine. (-> ./source/package.lisp?)
6 | ./source/nn      | a syntax sugar which implements nn ops and optimizers
--- external -----------------------------------------------------------------------
./external/avm/clang  | avm implementation using clang
./external/ajit/clang | ajit implementation using clang
...
------------------------------------------------------------------------------------
```

```
Composite (e.g.: Linera, Conv2d) -> [Lower] -> Function (e.g.: Add) -> [Lower] -> ASM (e.g.: %add)
-> [Lower] -> AJit
or
-> AVM
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

- Symbolic Compilation

- (In the future) reimplement the frontend in Coalton

- MoEのGatingとかKV CacheをうまくCにExportできるIRを考える

    - `:Graph`の名前空間にLoweringしないやつを作る (If/For/DefModule)

## Installing (WIP)

```sh
$ ros install hikettei/caten
$ caten --help
```

## Dependencies

- trivia

- alexandria

- rove

- cl-ppcre