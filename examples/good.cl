class A {
    -- self referencing attr
    attr: Int <- attr;
    foo(x: Object): Object { 0 };

    -- method named self
    self(): Object { 0 };
};

class B inherits A {
    -- referencing parent attr
    foo: Int <- attr;
    -- rename parameter
    -- same name as atrtibute
    foo(renameParameter: Object): Object { 0 };
};

class C inherits A {
    c(): Object { 0 };
    -- test basic expressions

    -- test lub
    test1(): A {
        if true then new A else new B fi
    };

    -- test scope rules
    x: Int;
    test2(): Bool {
        let x: String, x: Bool in x
    };

    -- test blocks
    test3(): Bool {
        { 0; ""; true; }
    };

    -- test lub with SELF_TYPE
    test4(): A {
        if true then (new A).copy() else (new B).copy() fi
    };
};

class Main {
    -- test self type
    main(): C { (new C).copy() };
};
