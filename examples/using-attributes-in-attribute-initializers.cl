class A {};

class Main inherits IO {
    b: Int <- a + a;
    a: Int <- 1;
    c: Int <- 3;
    d: Int <- c + c;
    e: Int <- e + e;
    f: Bool <- isvoid g;
    g: A <- new A;
    h: Bool <- isvoid g;
    

    out_int_n(n: Int): Object {{out_int(n); out_string("\n");}};
    out_b(b: Bool): Object {out_string(if b then "true\n" else "false\n" fi)};

    main(): Object {{
        out_int_n(a);
        out_int_n(b);
        out_int_n(c);
        out_int_n(d);
        out_int_n(e);
        out_b(f);
        out_b(f);
    }};
};
