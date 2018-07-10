# BitVisuals

A simple tool for visualizing bitwise manipulation inside the Haskell REPL (GHCi).

```bash
$ stack ghci
...
bitvisuals-0.1.0.0: initial-build-steps (lib)
Configuring GHCi with the following packages: bitvisuals
GHCi, version 8.2.2: http://www.haskell.org/ghc/  :? for help
...
Loaded GHCi configuration from ~/.ghc/ghci.conf
[1 of 5] Compiling Internal.BitOps  ( ~/bitvisuals/src/Internal/BitOps.hs, interpreted )
[2 of 5] Compiling BitOps           ( ~/bitvisuals/src/BitOps.hs, interpreted )
[3 of 5] Compiling BitPrinters      ( ~/bitvisuals/src/BitPrinters.hs, interpreted )
[4 of 5] Compiling BitAlgos         ( ~/bitvisuals/src/BitAlgos.hs, interpreted )
[5 of 5] Compiling Welcome          ( ~/bitvisuals/src/Welcome.hs, interpreted )
Ok, five modules loaded.
λ: printBinXor 222 111
	Decimal      Binary
	  222       11011110
^	  111       01101111
------------------------------
λ:
```

Note: Gifs outdated, but it's all roughly the same

![BitVisuals Demo](./extra/bitvis_demo_small.gif)


If you are stuck, trying loading running "main" in the Welcome module. You should see something like the following:

![BitVisuals Methods](./extra/bitvis_methods.png)
