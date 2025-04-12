class Main inherits IO {
	b(b: Bool): Int {if b then 1 else 0 fi};
	main(): Object {
		let void: Main,
		void2: Main,
		m1: Main <- new Main,
		m2: Main <- new Main,
		m3: Object <- new Main,
		s1: Object <- "hey" in {
			out_int(b(m1 <= m1));
			out_int(b(m1 < m1));
			out_int(b(m1 = m1));
			out_int(b(m1 = m2));
			out_int(b(m1 < m2));
			out_int(b(m1 <= m2));
			out_int(b(void <= m2));
			out_int(b(void <= void2));
			out_int(b(void < void2));
			out_int(b(void = void2));
			out_int(b(m3 = s1));
			out_int(b(m3 < s1));
			out_int(b(m3 <= s1));
			out_int(b(s1 < m3));
			out_int(b(s1 <= m3));
		}
	};
};
