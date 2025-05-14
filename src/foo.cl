class Main {
  x: Int <- 0;
  y: Int <- 1;
  main(): Object {{
    if true then x <- 0 else x <- 1 fi;
    y <- 3;
    while true loop y <- 0 pool;
  }};
};
