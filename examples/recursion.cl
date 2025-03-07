class Main inherits IO {
	i: Int <- 0;
	main(): Object {{
		i <- i + 1;
		out_int(i);
		if i < 10 then
			self.main()
		else 0 fi;
	}};
};
