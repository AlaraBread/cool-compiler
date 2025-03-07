class Main inherits IO {
	out_bool(b: Bool): Object {if b then out_string("true") else out_string("false") fi};
	main(): Object {
		let m: Main,
		i: Int,
		b: Bool,
		s: String in {
			out_bool(isvoid m);
			out_bool(isvoid i);
			out_bool(isvoid b);
			out_bool(isvoid s);
		}
	};
};