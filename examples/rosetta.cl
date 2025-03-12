(* a basic linked list [Int] *)
class IntList inherits IO {
    prepend(elem: Int) : IntList {
        new IntNil -- placeholder; this gets overriden in both children
    };

    append(elem: Int) : IntList {
        new IntNil -- placeholder; this gets overriden in both children
    };

    empty() : Bool {
        true -- placeholder
    };

    get(i: Int): Int {
        if i = 0 then
            case self of
                c: IntCons => c.head();
                n: IntNil => 0-2; -- error value
            esac
        else
            case self of
                c: IntCons => c.tail().get(i-1);
                n: IntNil => 0-2; -- error value
            esac
        fi
    };

    -- returns a version of the list with the element at this index removed
    remove(i: Int): IntList { self };

    print(): Object { "" };
};
class IntNil inherits IntList {
    prepend(elem: Int) : IntList {
        (new IntCons).init(elem, self)
    };

    append(elem: Int) : IntList {
        prepend(elem)
    };
};

class IntCons inherits IntList {
    head: Int;
    tail: IntList;

    init(h: Int, t: IntList) : IntCons {{
        head<-h;
        tail<-t;
        self;
    }};

    head() : Int {
        head
    };
    decrement(): Object {
        head <- head - 1
    };
    tail() : IntList {
        tail
    };

    prepend(elem: Int) : IntList {
        new IntCons.init(elem, self)
    };

    append(elem: Int) : IntList {{
        tail <- tail.append(elem);
        self;
    }};

    empty() : Bool {
        false
    };

    remove(i: Int): IntList {
        if i = 0 then
            tail
        else {
            tail <- tail.remove(i-1);
            self;
        } fi
    };

    print(): Object {{
        out_int(head);
        out_string("\n");
        tail.print();
    }};
};

(* a basic linked list [String] *)
class StringList inherits IO {
    prepend(elem: String) : StringList {
        new StringNil -- placeholder; this gets overriden in both children
    };

    append(elem: String) : StringList {
        new StringNil -- placeholder; this gets overriden in both children
    };

    empty() : Bool {
        true -- placeholder
    };

    print(): Object { "" };

    remove(i: Int): StringList { self };

    get(i: Int): String {
        if i = 0 then
            case self of
                c : StringCons => c.head();
                n : StringNil => ":<";
            esac
        else
            case self of
                c : StringCons => c.tail().get(i-1);
                n : StringNil => ":<";
            esac
        fi
    };

    contains(elem: String): Bool { false };

    -- counts how many matching elements are in alternating positions
    countAlternating(elem: String, considerThis: Bool): Int { 0 };
};
class StringNil inherits StringList {
    prepend(elem: String) : StringList {
        (new StringCons).init(elem, self)
    };

    append(elem: String) : StringList {
        prepend(elem)
    };

    empty() : Bool {
        true
    };

    print(): Object { "" };

    contains(elem: String): Bool { false };

    countAlternating(elem: String, considerThis: Bool): Int { 0 };
};

class StringCons inherits StringList {
    head: String;
    tail: StringList;

    init(h: String, t: StringList) : StringCons {{
        head<-h;
        tail<-t;
        self;
    }};

    head() : String {
        head
    };
    tail() : StringList {
        tail
    };

    prepend(elem: String) : StringList {
        new StringCons.init(elem, self)
    };

    append(elem: String) : StringList {{
        tail <- tail.append(elem);
        self;
    }};

    empty() : Bool {
        false
    };

    print(): Object {{
        out_string(head);
        out_string("\n");
        tail.print();
    }};

    remove(i: Int): StringList {
        if i = 0 then
            tail
        else {
            tail <- tail.remove(i-1);
            self;
        } fi
    };

    contains(elem: String): Bool {
        if head = elem then
            true
        else
            tail.contains(elem)
        fi
    };

    countAlternating(elem: String, considerThis: Bool): Int {
        (if considerThis then (if elem = head then 1 else 0 fi) else 0 fi)
            + tail.countAlternating(elem, not considerThis)
    };
};

class Main inherits IO {
    -- stores user input lines in reverse order
    input: StringList;
    -- stores all of the nodes, uniquely
    nodes: StringList;
    -- parent counts of the nodes, in the same order
    parentCounts: IntList;
    -- is there a known cycle?
    knownCycle: Bool <- false;
    -- output list
    output: StringList <- new StringNil;

    main() : Object { {
        -- read input into a list. note, this list is backwards
        input <- let list: StringList <- new StringNil,
                     str: String <- "nonempty" in {
            while not (str <- in_string()).length() = 0 loop {
                list<-list.prepend(str);
            } pool;

            list;
        };

        -- construct a unique list of nodes
        nodes <- let list: StringList <- input, ns: StringList <- new StringNil in {
            while not list.empty() loop case list of
                sc: StringCons => let s: String <- sc.head() in {
                    ns <- if ns.contains(s) then ns else ns.prepend(s) fi;
                    list <- sc.tail();
                };
            esac pool;
            ns;
        };

        -- construct the parent counts
        parentCounts <- let ns: StringList <- nodes, pcs: IntList <- new IntNil in {
            -- we always keep ncs and pcs in step
            while not ns.empty() loop case ns of
                sc: StringCons => let node: String <- sc.head() in {
                    pcs <- pcs.append(
                        -- note: we start with not counting because input
                        -- is reversed
                        input.countAlternating(node, false)
                    );
                    ns <- sc.tail();
                };
            esac pool;
            pcs;
        };

        output <- {
            while and(not nodes.empty(), not knownCycle) loop
                let nextIndex: Int <- findNextIndex() in {
                    if nextIndex = 0-1 then
                        knownCycle <- true
                    else {
                        -- properly modify parentCounts
                        -- we iterate overs nodes and parentCounts in tandem,
                        -- subtracting one from the parent count if an edge is present
                        let next: String <- nodes.get(nextIndex),
                            ns: StringList <- nodes,
                            pcs: IntList <- parentCounts in {
                                while not pcs.empty() loop
                                case pcs of pc: IntCons => case ns of n: StringCons => {
                                    if isEdgePresent(n.head(), next) then
                                        pc.decrement()
                                    else "" fi;

                                    pcs <- pc.tail();
                                    ns <- n.tail();
                                }; esac; esac pool;
                        };

                        output <- output.append(nodes.get(nextIndex));
                        nodes <- nodes.remove(nextIndex);
                        parentCounts <- parentCounts.remove(nextIndex);
                    } fi;
                } pool;

            output;
        };

        if knownCycle then
            out_string("cycle\n")
        else
            output.print()
        fi;
    }};

    -- finds index of next output of toposort.
    -- returns -1 if none found.
    findNextIndex(): Int {
        let candidateIndex: Int <- 0-1,
            candidate: String <- "",
            i: Int <- 0,
            pcs: IntList <- parentCounts,
            ns: StringList <- nodes in {
                while not pcs.empty() loop
                    case pcs of pc: IntCons => case ns of n: StringCons => {
                    if pc.head() = 0 then {
                        if or(candidateIndex = 0-1, n.head() < candidate) then {
                            candidateIndex <- i;
                            candidate <- n.head();
                        } else "" fi;
                    } else "" fi;

                    i <- i + 1;
                    pcs <- pc.tail();
                    ns <- n.tail();
                }; esac; esac pool;
                candidateIndex;
            }
    };

    -- checks if a given edge is present in input
    isEdgePresent(child: String, parent: String): Bool {
        let input: StringList <- input,
            present: Bool <- false in {
            while and(not input.empty(), not present) loop
                case input of p: StringCons =>
                    case p.tail() of c: StringCons => {
                        present <- or(present, and(p.head()=parent, c.head()=child));
                        input <- c.tail();
                    }; esac;
                esac
            pool;
            present;
        }
    };

    -- binary and. you know the deal.
    and(a: Bool, b: Bool): Bool {
        if a then b else false fi
    };

    -- binary or. you know the deal.
    or(a: Bool, b: Bool): Bool {
        if a then true else b fi
    };
};
