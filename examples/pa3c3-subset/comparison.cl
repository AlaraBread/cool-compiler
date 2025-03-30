class Main inherits IO {
    main(): Object { {
        out_string("<=\n");
        if 1 <= ~1 then out_string("uh oh :<\n") else out_string("yay! :>\n") fi;
        if 1 <= 1 then out_string("yay! :>\n") else out_string("uh oh :<\n") fi;
        if 1 <= 5 then out_string("yay! :>\n") else out_string("uh oh :<\n") fi;

        out_string("<\n");
        if 1 < 1 then out_string("uh oh :<\n") else out_string("yay! :>\n") fi;
        if 5 < 1 then out_string("uh oh :<\n") else out_string("yay! :>\n") fi;

        out_string("=\n");
        if 1 = 1 then out_string("yay! :>\n") else out_string("uh oh :<\n") fi;
        if 1 = 2 then out_string("uh oh :<\n") else out_string("yay! :>\n") fi;

        out_string("<=\n");
        if false <= false then out_string("yay! :>\n") else out_string("uh oh :<\n") fi;
        if false <= true then out_string("yay! :>\n") else out_string("uh oh :<\n") fi;
        if true <= false then out_string("uh oh :<\n") else out_string("yay! :>\n") fi;
        if true <= true then out_string("yay! :>\n") else out_string("uh oh :<\n") fi;

        out_string("<\n");
        if false < false then out_string("uh oh :<\n") else out_string("yay! :>\n") fi;
        if false < true then out_string("yay! :>\n") else out_string("uh oh :<\n") fi;
        if true < false then out_string("uh oh :<\n") else out_string("yay! :>\n") fi;
        if true < true then out_string("uh oh :<\n") else out_string("yay! :>\n") fi;

        out_string("=\n");
        if false = false then out_string("yay! :>\n") else out_string("uh oh :<\n") fi;
        if false = true then out_string("uh oh :<\n") else out_string("yay! :>\n") fi;
        if true = false then out_string("uh oh :<\n") else out_string("yay! :>\n") fi;
        if true = true then out_string("yay! :>\n") else out_string("uh oh :<\n") fi;
    }};
};
