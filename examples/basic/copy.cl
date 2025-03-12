class Main inherits IO {
	out_bool(b: Bool): Object {if b then out_string("true") else out_string("false") fi};
	m: Main;
	m(): Main {m};
	main(): Object {{
		out_bool(isvoid self.copy().m());
		m <- new Main;
		out_bool(isvoid m());
		out_bool(isvoid self.copy().m());
	}};
};