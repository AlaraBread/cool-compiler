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

We have an interpreter that runs over the InputIr. If it terminates within 10 seconds and doesn't require input, then we "constant fold" the whole program to a single printf.

Benchmark 1 tests edge cases for out_string.

Benchmark 2 is julia's rosetta, but with a test case inserted into it as constants. This takes advantage of our interpreter.

The control flow graph is stored as a map from labels to lists of statements, as well as a map from parents to their child nodes and a map from child nodes to their parent nodes. Cfg construction is relatively simple, we just add statements one at a time.

To construct minimal SSA, we followed the algorithm described in Cooper, Keith D., Harvey, Timothy J. and Kennedy, Ken. "A simple, fast dominance algorithm." to find dominance frontiers, and an algorithm described in [this pdf](https://sites.cs.ucsb.edu/~yufeiding/cs293s/slides/293S_06_SSA.pdf) for constructing SSA.

For our dataflow analysis, we wrote a fairly generic system that uses a Lattice typeclass and abstract interpretation typeclass so that we can use the same abstract interpretation code everywhere. We have a fairly straightforward functional version of the work list algorithm for it. We handle reverse data flow by simply reversing the CFG. We store our known abstract values as a statement type wrapping the underlying statement type. This means that we should be able to use the same lattice on multiple IRs simply by writing new transfer functions. 

We would repeat (constant folding, dead code elimination) until fixpoint. Our constant folding keeps track of a bunch of things, such as integer/bool/string constants and whether or not an object is known to be void. Our dead code elimination also removes unreachable blocks and combine basic blocks when possible, which occurs when constant folding turns a conditional jump into an unconditional one. Unfortunately, our Cfg construction is mildly buggy, which completely destroys our correctness, so we have that turned off for this submission :<.
