class Main inherits IO {
  main(): Object { let x: Int <- 0 in {
    while false loop out_int(0) pool;
    while x < 10 loop {out_int(x); out_string("\n"); x <- x + 1;} pool;
  }};
};
