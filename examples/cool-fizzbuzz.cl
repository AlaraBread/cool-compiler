class Main inherits IO {
    -- we encode this in base 15 to make FizzBuzz easier
    ones: Int <- 1;
    fifteens: Int <- 0;

    or(a: Bool, b: Bool): Bool {
        if a then a else b fi
    };

    -- does base 15 incrementing
    inc(): Object {
        if ones = 14 then { ones <- 0; fifteens <- fifteens + 1; } else ones <- ones + 1 fi
    };

    main(): Object {
        while fifteens * 15 + ones < 101 loop {
            if (ones = 0) then
                out_string("FizzBuzz\n")
            else
                (if or(ones=3, or(ones=6, or(ones=9, ones=12))) then
                    out_string("Fizz\n")
                else
                    if or(ones=5, ones=10) then
                        out_string("Buzz\n")
                    else {
                        out_int(fifteens*15+ones);
                        out_string("\n");
                    } fi
                fi)
            fi;
            inc();
        } pool
    };
};
