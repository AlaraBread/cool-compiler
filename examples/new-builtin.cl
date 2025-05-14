class Main inherits IO {
  main(): Object { {
    out_int(new Int);
    out_int(new Int + 1);
    out_string((new String).concat("arf"));
    out_string(if (new Bool) then "???" else "yay" fi);
  }};
};
