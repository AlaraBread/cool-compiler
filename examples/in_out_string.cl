class Main inherits IO {
	main(): Object {let s: String <- in_string() in {
		out_string(s);
		abort();
		out_string("after abort\n");
	}};
};
