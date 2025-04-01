class Main inherits IO {
  f(n: Int, p0: String, p1: String, p2: String, p3: String, p4: String, p5: String, p6: String, p7: String, p8: String, p9: String, p10: String, p11: String, p12: String, p13: String, p14: String, p15: String): Object {
  if n < 0 then 0 else {
    out_int(n);
    out_string(": ");
    out_string(p0);
    out_string(p1);
    out_string(p2);
    out_string(p3);
    out_string(p4);
    out_string(p5);
    out_string(p6);
    out_string(p7);
    out_string(p8);
    out_string(p9);
    out_string(p10);
    out_string(p11);
    out_string(p12);
    out_string(p13);
    out_string(p14);
    out_string(p15);
    out_string("\n");
    f(n-1, p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15);
  } fi};

  main(): Object {
    f(8, "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15")
  };
};
