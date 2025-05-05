class A inherits IO {
	x: Int <- {
		out_string("initializing x in a\n");
		foo(x);
		1;
	};
	g: Int <- 54;
	foo(x: Int) : Object {{
		out_string("a foo:\n");
		out_int(x);
		out_string("\n");
	}};
};

class B inherits A {
	y: Int <- {
		foo(x);
		3;
	};
	z: Int <- 54;
	foo(x: Int) : Object {{
		out_string("b foo:\n");
		out_int(x*2);
		out_string("\n");
	}};
	bar(): SELF_TYPE {
		new SELF_TYPE
	};
};

class Main inherits IO {
	a: Int <- 4;
	a(): Int {a};
	main(): Object {{
		a <- 6;
		out_int(a);
		out_int(new SELF_TYPE.a());
		out_int(a());
		out_string("\n");
		new B.bar();
		out_string("\n");
	}};
};
