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

    out_string("\nevil with ints:\n");
    let x: Object <- 1, y: Object <- 2 in p(x = y);
    let x: Object <- 2, y: Object <- 1 in p(x = y);
    let x: Object <- 1, y: Object <- self in p(x = y);
    let x: Object <- self, y: Object <- 1 in p(x = y);

    out_string("\nevil with bools:\n");
    let x: Object <- false, y: Object <- true in p(x = y);
    let x: Object <- true, y: Object <- false in p(x = y);
    let x: Object <- true, y: Object <- self in p(x = y);
    let x: Object <- self, y: Object <- true in p(x = y);

    out_string("\nevil with strings:\n");
    let x: Object <- "foo", y: Object <- "foobar" in p(x = y);
    let x: Object <- "foobar", y: Object <- "foo" in p(x = y);
    let x: Object <- "arf", y: Object <- self in p(x = y);
    let x: Object <- self, y: Object <- "arf" in p(x = y);

    out_string("\nevil with bools and ints:\n");
    let x: Object <- 0, y: Object <- true in p(x = y);
    let x: Object <- true, y: Object <- 0 in p(x = y);
    let x: Object <- false, y: Object <- 1 in p(x = y);
    let x: Object <- 1, y: Object <- false in p(x = y);
  }};
};
