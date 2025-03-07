class A {
	one(): Int {1};
};

class B inherits A {
	one(): Int {2};
};

class C inherits B {};

class D {};

class Main inherits IO {
	main(): Object {{
		out_int(
			case new C of
				a: A => a.one();
				b: B => b.one();
			esac
		);
		out_int(
			case new C of
				d: D => 1;
			esac
		);
	}};
};
