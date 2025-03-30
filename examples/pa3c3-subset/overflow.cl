class Main inherits IO {
    main(): Object { {
        out_int(2147483647 + 1);
        out_string("\n");
        out_int((~214748364 - 1) - 1);
        out_string("\n");
        out_int(2147483647 + 2414);
        out_string("\n");
        out_int((~214748364 - 1) - 2414);
        out_string("\n");

        out_int(13414141 * 2414);
        out_string("\n");
        out_int(~13414141 * 2414);
        out_string("\n");
        out_int(13414141 * ~2414);
        out_string("\n");
        out_int(~13414141 * ~2414);
        out_string("\n");
    }};
};
