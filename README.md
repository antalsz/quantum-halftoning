# quantum-halftoning/haskell

This directory features a Haskell program for quantum halftoning images.  More
documentation forthcoming.

## Setup (run once)

```
stack setup
```

## Build and run

```
stack build
stack exec quantum-halftone -- --help
```

## Sample command lines

```
stack exec quantum-halftone -- \
  --expand-by=3 --start=white --refresh=pixel --count=20 \
  input.png 'output*.png'
```

```
stack exec quantum-halftone -- -e2 -sw -rp -n20 input.png 'output*.png'
```
