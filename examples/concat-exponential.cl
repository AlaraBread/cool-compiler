class Main inherits IO {
    x: String <- "arf";
    i: Int <- 0;
    main(): Object {{
        while i < 16 loop { x <- x.concat(x); i <- i + 1; } pool;
        out_string(x);
        out_string("\n");
    }};
};
