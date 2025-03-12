class Main {
	main(): Object {
		case new Object of
			a: Int => a.abort();
			b: Bool => b.abort();
		esac
	};
};
