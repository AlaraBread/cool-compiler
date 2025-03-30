class Main inherits IO {
    main(): Object {
        let x: Int, y: Int in {
            x <- 1;
            out_int(x);
            y <- x;
            out_int(y);
            y <- y + 1;
            out_int(y);

            out_int(x);
            out_int(y);
        }
    };
};
