class Main inherits IO {
	main(): Object {{
		out_string("foo".concat("bar"));
		out_int("foo".length());
		out_string("\n\n");
		out_string("abcdefg".substr(8, ~1));
		out_string("\n");
		out_string(new String);
	}};
};