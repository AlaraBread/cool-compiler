class Main inherits IO {
	b(b: Bool): SELF_TYPE {
		if b then out_string("true") else out_string("false") fi
	};
	m: Main;
	m(): Main {m};
	main(): Object {{
		b(isvoid self.copy().m());
		m <- new Main;
		b(isvoid m());
		b(isvoid self.copy().m());
		b(isvoid new A.copy().a());
	}};
};

class A inherits Main {
	a: A <- {
		b(isvoid m());
		b(isvoid out_string("initializing a").a());
		out_string("done initializing a");
	};
	a2: A <- {
		out_string("initializing a2");
		b(isvoid m());
		b(isvoid a());
		b(isvoid a2());
	};
	a(): A {a};
	a2(): A {a2};
};
