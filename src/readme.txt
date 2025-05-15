This is our compiler pipeline:

InputIr ->
Trac Variable ->
Cfg Trac Variable ->
Cfg Trac SsaVariable ->
Trac SsaVariable ->
Trac Variable ->
Twac Variable ->
TwacR Variable ->
Assembly

where Trac is "three address code", Twac is "two address code", and TwacR is "two address code with explicit register allocation"

We initially intended to do proper register allocation, but we ran out of time.

We run (dead code elimination, constant folding) until fixpoint.

We also have an interpreter that runs over the InputIr. If it terminates within 10 seconds and doesn't require input, then we "constant fold" the whole program to a single printf.
