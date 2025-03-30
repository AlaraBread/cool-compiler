class Main inherits IO {
  main(): Object {{
    out_int(~0);
    out_string("\n");
    out_int(~18);
    out_string("\n");
    out_int(~324);
    out_string("\n");
    out_int(~~324);
    out_string("\n");
    out_int(~~~324);
    out_string("\n");
    if not true then out_string("oh no :<\n") else out_string("yay! :>\n") fi;
    if not false then out_string("yay! :>\n") else out_string("oh no :<\n") fi;
    if not not false then out_string("oh no :<\n") else out_string("yay! :>\n") fi;
    if not not true then out_string("yay! :>\n") else out_string("oh no :<\n") fi;
    if not not not true then out_string("oh no :<\n") else out_string("yay! :>\n") fi;
    if not not not false then out_string("yay! :>\n") else out_string("oh no :<\n") fi;
  }};
};
