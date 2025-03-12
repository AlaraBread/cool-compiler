class A {
	a: Int <- 10;
	a(): Int {a};
	one(): Int {{a <- a + 1; 1+a;}};
};

class B inherits A {
	one(): Int {2};
};

class C inherits B {
	two(): SELF_TYPE {{a <- a + 10; self;}};
};

class Main inherits IO {
	main(): Object {{
		out_int(new C.one());
		out_int(new C.two().a());
		out_int(new C.two()@A.one());
	}};
};
