class Main inherits IO {
  v: Main;

  p(b: Bool): Object {
    if b then
      out_string("true\n")
    else
      out_string("false\n")
    fi
  };

  main(): Object {{
    out_string("ints:\n");
    p(0 = 1);
    p(0 = 0);
    p(1 = 0);
    p(~1 = 1);
    p(1 = ~1);

    out_string("\nbools:\n");
    p(false = false);
    p(false = true);
    p(true = false);
    p(true = true);

    out_string("\nstrings:\n");
    p("foo" = "foobar");
    p("foobar" = "foo");
    p("bark" = "meow");
    p("meow" = "bark");
    p("bark" = "bark");

    out_string("\nobjects:\n");
    p(v = v);
    p(v = self);
    p(self = v);
    p(self = self);
  }};
};
