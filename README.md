# gen-256
genetic algorithm library for (hopefully) finding reasonable bitcoin scripts that do things

## Why?

The recent taproot upgrade for bitcoin [increased the resources](https://github.com/bitcoin/bips/blob/master/bip-0342.mediawiki#Resource_limits)
available to scripts by asignificant amount.

This means that we can write really large scripts to do some complex calculations.

Writing/finding efficient expressions for such scripts is hard.

Let's have a computer do it for us.

## Running

1. `$ ./mill -i generator.run`

## Status
* Can generate randome script of arbitrary length. But that is about it so far. The generated script is very likely to be invalid.
