                    Baseline Cool Benchmarks for Optimizer

This archive contains a number of Cool programs that serve as an indicative
benchmark suite for your optimizer. Many of the programs require input;
two example inputs are provided. Example usage:

$ cool --asm if-boolean.cl
$ cool --profile if-boolean.cl-asm < generic.input
        (* note Reference result *)
$ cool --type if-boolean.cl
$ my-optimizer if-boolean.cl-type
$ cool --profile if-boolean.cl-asm < generic.input
        (* note My result *)

The Benchmarks:                         Input used:

cells-40-21.cl                          (none)
hairy-scary-double.cl                   (none)
if-boolean.cl                           generic.input OR simple.input
int-basic-block.cl                      generic.input OR simple.input
int-temporary.cl                        generic.input OR simple.input
list-20.cl                              (none)
loop-int.cl                             generic.input OR simple.input
matrix-multiply-5.cl                    generic.input OR simple.input
pi-100.cl                               (none)
primes-500.cl                           (none)

        - Wes
