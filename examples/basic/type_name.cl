class Main inherits IO {
	main(): Object {{
		out_string(let m: IO <- self in m.type_name());
	}};
};