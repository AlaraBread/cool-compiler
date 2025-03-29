class Main inherits IO {
    main(): Object {
        let x: Int, y: Int in {
            x <- 1;
            y <- x;
            y <- y + 1;
            out_int(y);
        }
    };
};
