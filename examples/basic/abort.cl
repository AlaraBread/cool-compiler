class Main inherits IO {
	main(): Object {{
		out_string("1");
		if in_int() = 1 then abort() else 1 fi;
		out_string("2");
		abort();
	}};
};