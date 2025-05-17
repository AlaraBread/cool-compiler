class Main inherits IO {
  i: Int <- 0;
  j: Int <- 0;
  main(): Object {
    while i < 10 loop {
      j <- 0;
      while j < i loop {
        j <- j + 1;
        out_int(i);
        out_string(",");
        out_int(j);
        out_string("\n");
      } pool;
      i <- i + 1;
    } pool
  };
};
