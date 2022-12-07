# gen-256
genetic algorithm library for (hopefully) finding reasonable bitcoin scripts that do things

## Why?

The recent taproot upgrade for bitcoin [increased the resources](https://github.com/bitcoin/bips/blob/master/bip-0342.mediawiki#Resource_limits)
available to scripts by asignificant amount.

This means that we can write really large scripts to do some complex calculations.

Writing/finding efficient expressions for such scripts is hard.

Let's have a computer do it for us.
## A Goal - `fn_codes`

### `fn_add_mod_n` - 256-bit Modular Arithmetic

Bitcoin has various op_codes for manipulating the stack (and the altstack). 

If we express a 256 bit number as 8 32-bit numbers, and we want to perform
arithmetic modulo `n` (which is another 32-bit number), then what we seek is
a function code `fn_add_mod_n = ... OP_xxx ...`. 


## Running

Not much to see here yet.
1. `$ ./mill -i generator.runMain vzxplnhqr.RandomBitcoinScripts` will generate a random (likely invalid) bitcoin script every time you press a key. You will need to `ctrl+c` to exit the program.

2. `$ ./mill -i generator.run` will run a naive genetic algorithm which starts from a population of two strings, and mutates them to ultimately arrive at a third target string.

## Status
* Can generate random bitcoin script of arbitrary length. But that is about it so far. The generated script is very likely to be invalid.
* Not searching for bitcoin scripts yet, just trying to make the genetic algorithm itself work efficiently for strings first.

## References / Acknowledgements / Notes
* Using typeclasses to (inefficiently) code up a genetic algorithm was [shown here](https://www.youtube.com/watch?v=lshIBfmsktk).
