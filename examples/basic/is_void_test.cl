class Main inherits IO {
    m: Main;
    main(): Object {{
       out_string(if isvoid self then "void\n" else "not void\n" fi);
       out_string(if isvoid m then "void\n" else "not void\n" fi);
    }};
};
