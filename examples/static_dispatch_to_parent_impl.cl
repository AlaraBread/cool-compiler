class A {
	foo(): Int {1};
};

class B inherits A {
};

class Main {
	main(): Object {{
		new B@B.foo();
	}};
};