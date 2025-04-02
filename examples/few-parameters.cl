class Main inherits IO {
  f(p0: String, p1: String, p2: String, p3: String, p4: String, p5: String): Object {{
    out_string(p0);
    out_string("\n");
    out_string(p1);
    out_string("\n");
    out_string(p2);
    out_string("\n");
    out_string(p3);
    out_string("\n");
    out_string(p4);
    out_string("\n");
    out_string(p5);
    out_string("\n");
  }};

  main(): Object {
    f("0", "1", "2", "3", "4", "5")
  };
};
