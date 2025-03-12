class Main inherits IO {
    i: Int <- 1;

    main(): Object {
        while i < 101 loop {
            if (mod(i, 15) = 0) then
                out_string("FizzBuzz\n")
            else
                if mod(i, 5) = 0 then
                    out_string("Fizz\n")
                else
                    if mod(i, 3) = 0 then
                        out_string("Buzz\n")
                    else {
                        out_int(i);
                        out_string("\n");
                    }
                    fi
                fi
            fi;
            i <- i + 1;
        } pool
    };
    -- I *guess* you *could* be reasonable
    mod(a: Int, b: Int): Int { a - (a / b) * b };

};
