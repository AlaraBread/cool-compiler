(I reflowed this for you Dr. Kellog, M-q is a cool shortcut :3)

Our compiler goes through five IR stages: InputIr, Trac, Twac, TwacR, and
AssemblyIr.

InputIr is the annotated AST from the semantic analyzer as a set of Haskell
datatypes.

Trac is three-address code. Honestly, this is mostly here because it was
required; however, it is relatively straightforward to convert InputIr to it. At
this level, we dissect all control flow into conditional/unconditional jumps. We
also insert error handling (dispatch on void, division by zero, case on void,
missing case branch) here. Case statements produce a label for each branch,
their corresponding Trac, and produces a map between Types and Labels that later
gets lowered into a jump table. Thanks not having separate compilation! Here, we
handle Let, by assigning each binding variable a temporary, and outputting the
Trac to generate them in the correct order, before generating the Trac for the
body. We have a Dispatch opcode in Trac, but we generate the Trac to evaluate
the arguments/receiver here. We also unify dynamic and self dispatch, by setting
the receiver to self. We construct constructors here, giving each object a ..new
method, which defaults initialize every attribute and then runs the
initializers. This design was directly inspired by the reference compiler.

Twac is a very straightforward conversion of Trac into two-address code. This
matches x86 better. Comparisons are still represented with three parameters,
because they are complicated enough that the assembly we have to generate is
non-trivial anyways.

TwacR is Twac but with register/temporary space allocation. This includes
pushing/popping from the stack, making the parameters for opcodes registers,
etc. We implemented this by having Twac be polymorphic over its Variable type,
so we do not have to do too much duplication from Twac. We do incredibly naive
register allocation, by loading/storing everything before every operation and
using a stack to keep track of free registers. We also reserve some registers,
e.g. %r15 for a pointer to self, and some others for codegen.

AssemblyIr is "just assembly", i.e. a subset of x86-64 assembly as a Haskell
datatype. Converting from TwacR to assembly is mostly straightforward, but we
have some nice tools to get correct addresses for
temporaries/attributes/parameters at compile time. Our object
layouts/vtables/activation records are very standard. A couple of TwacR
instructions are annoying to convert; namely, comparisons. We dealt with this by
having comparisons call out to a built-in function.

For non-SELF_TYPE new, since we know the type at compile time, we use calloc,
assign the correct size/type tag/vtable pointer, then calls ..new. For SELF_TYPE
new, we look at the size of the current object to allocate, then copy the
size/type/tag/vtable pointer of the self object. Then, we call ..new.

For static dispatch, we just produce a call instruction the correct method; to
match this, in the code, we have a comment saying "this is easy! We like this."
For dynamic (and self) disptach, we first do a vtable lookup, and then call the
address found there.

One thing we do throughout is we use the State monad to ensure that we pick
(globally) unique labels and (locally) unique temporary numbers, by only
accessing it through functions that increment those two things appropriately.


Test 1 tests:
- initializers using the variable they are initializing
- initialization order
- that we default-construct initialized variables before calling any initializers.
- that the default initializer for Ints is 0, and the default for objects is void.

Test 2 tests:
- recursive functions
- many parameters (i.e. memory parameters)
- out_string
- integer <

Test 3 tests:
- in_string
- in_int
- out_string
- substr
- parameter/receiver execution order
While this appears simple, by passing in the right input, we can test:
- all possible substring errors.
- strange inputs to in_string.
- strange inputs to in_int
This test caught a bug where in_int did not consume the relevant newline.

Test 4 tests:
- string literal handling of escape sequences
- out_string escape sequence handling
This is a personal favorite because of the use of fuzzing; this also caught
multiple bugs.
