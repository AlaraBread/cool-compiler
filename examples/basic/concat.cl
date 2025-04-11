class Main inherits IO {
	a: String <- "arf";
	main(): Object {let v: String <- "hey" in {
		out_string("hello".concat("").concat(" ").concat("world").concat("!").concat("\n"));
		out_string(v.concat(v).concat(v).concat(v).concat("\n"));
		a <- a.concat(a);
		a <- a.concat(a);
		out_string(a.concat("\n"));
	}};
};
