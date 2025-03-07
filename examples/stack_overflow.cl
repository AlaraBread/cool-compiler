class A {
	a: A <- new A;
	a(i: Int): Int {i + 1};
};

class Main inherits IO {
	main(): Object {
		if new A.a(2) < 3 then out_string("a") else out_string("b") fi
	};
};
