class Main inherits IO {
    b: Int <- a + a;
    a: Int <- 1;
    c: Int <- 3;
    d: Int <- c + c;
    e: Int <- e + e;

    out_int_n(n: Int): Object {{out_int(n); out_string("\n");}};

    main(): Object {{
        out_int_n(a);
        out_int_n(b);
        out_int_n(c);
        out_int_n(d);
        out_int_n(e);
    }};
};
