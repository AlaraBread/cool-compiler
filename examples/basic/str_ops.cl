class Main inherits IO {
	main(): Object {{
		out_string("foo".concat("bar"));
		out_int("foo".length());
		out_string("abcdefg".substr(0, 100));
	}};
};