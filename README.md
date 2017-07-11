# Schrödinger’s Zebra: Applying Mutual Information Maximization to Graphical Halftoning — Source Code

This code is the implementation of the quantum halftoning algorithm from
“Schrödinger’s Zebra: Information Theory, Quantum Mechanics, and Halftoning”, by
Antal Spector-Zabusky and Donald Spector, presented at [MaxEnt 2017].

The program is written in [Haskell], and requires [GHC] 8.  It uses the [Stack]
tool for installing dependencies (including GHC itself) and building.

## Building and running the program

* **Setup (once only):** Install [Stack], make sure it is in your path, and
  execute the command `stack setup` in this directory.

* **Build:** Execute the command `stack build` in this directory.

* **Run:** Run the command `./quantum-halftone` in this directory; for usage
  information, run `./quantum-halftone --help`.  (You can also run `stack exec
  quantum-halftone -- --help`; it’s the same thing.)

## Sample command lines

The following two sample command lines run the quantum halftoning algorithm with
the same parameters:

  * Each grayscale inpixel is expanded to a 3×3 grid of black and white
    outpixels.
  * The grayscale input file is `input.png`.
  * Twenty output files are produced, named `output01.png` through
    `output20.png`.
  * The initial output image is a quantum halftoning of the whole image (instead
    of a blank white or black canvas).
  * The whole image is re-halftoned to produce each of the output images
    (instead of re-halftoning a single pixel at a time).

With the long forms of the comand-line arguments, this can be run as

```
./quantum-halftone --expand-by=3 --start=image --refresh=image --count=20 \
                   input.png 'output*.png'
```

With the short forms of the comand-line arguments, this can be run as

```
./quantum-halftone -e3 -si -ri -n20 input.png 'output*.png'
```

## Licensing

This code is available under the permissive open-source 3-clause BSD license;
see the `LICENSE` file for more information.

[MaxEnt 2017]: http://www.gis.des.ufscar.br/meetings/2017maxent/index.php
[Haskell]:     https://www.haskell.org/
[GHC]:         https://www.haskell.org/ghc/
[Stack]:       https://www.haskellstack.org/
