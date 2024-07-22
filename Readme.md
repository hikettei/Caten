# Caten

~~Programmable Deep Learning Framework~~

Pattern Matcher Based DL Compiler wip???

## Project

```
./source/api     | principle apis for caten (syntax sugar for air) no shape/dtype check is running at this level !!
./source/air     | An optimized pattern matcher for DAG
./source/codegen | codegen
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