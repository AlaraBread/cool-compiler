class Main inherits IO {
  main(): Object {{
    out_string("Maximum number to go to: ");
    let x: Int <- 1, max: Int <- in_int() in
      while x <= max loop {
        if (x - 15*(x/15)) = 0 then
          out_string("fizzbuzz\n")
        else
          if (x - 3*(x/3)) = 0 then
            out_string("fizz\n")
          else
            if (x - 5*(x/5)) = 0 then
              out_string("buzz\n")
            else
              {out_int(x); out_string("\n");}
            fi
          fi
        fi;
        x <- x + 1;
      } pool;
  }};
};
