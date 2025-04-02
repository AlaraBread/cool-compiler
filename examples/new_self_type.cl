class Main inherits IO {
	a: Int <- 4;
	a(): Int {a};
	main(): Object {{
		a <- 6;
		out_int(a);
		out_int(new SELF_TYPE.a());
		out_int(a());
		out_string("\n");
	}};
};
