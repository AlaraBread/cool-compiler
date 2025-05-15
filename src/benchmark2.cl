class Util inherits IO {
	and(a: Bool, b: Bool): Bool {
		if a then if b then true else false fi else false fi
	};
	or(a: Bool, b: Bool): Bool {
		if a then true else if b then true else false fi fi
	};
};

class Deps inherits Util {
	head: DepItem;
	tail: DepItem;
	add(task: String, dep: String): Object {
		let cur: DepItem <- head, done: Bool <- false in
		if isvoid cur then
		let n: DepItem <- new DepItem,
		deps: StringList <- new StringList in
		{
			n.set_task(task);
			deps.set_add(dep);
			n.set_deps(deps);
			if isvoid tail then {
				tail <- n;
				head <- n;
			} else {
				tail.set_next(n);
				tail <- n;
			} fi;
		} else while and(not done, not isvoid cur) loop
		let next: DepItem <- cur.get_next() in {
			if cur.get_task() = task then {
				done <- true;
				cur.get_deps().set_add(dep);
			} else 0 fi;
			done <- if done then true else
				let void: Bool <- isvoid next in {
					if void then
					let n: DepItem <- new DepItem,
						deps: StringList <- new StringList in {
						n.set_task(task);
						deps.set_add(dep);
						n.set_deps(deps);
						cur.set_next(n);
					} else 0 fi;
					void;
				} fi;
			cur <- next;
		}
		pool fi
	};
	remove(task: String): Object {
		let cur: DepItem <- head in
		while not isvoid cur loop
		{
			let deps: StringList <- cur.get_deps() in {
				deps.remove(task);
			};
			cur <- cur.get_next();
		}
		pool
	};
	has_dependencies(task: String): Bool {
		let cur: DepItem <- head,
		result: Bool <- false in
		{
			while not isvoid cur loop
			let t: String <- cur.get_task() in {
				if t = task then 
					let deps: StringList <- cur.get_deps() in {
						result <- not deps.is_empty();
					}
				else 0 fi;
				cur <- cur.get_next();
			}
			pool;
			result;
		}
	};
	print(): Object {
		let cur: DepItem <- head in
		while not isvoid cur loop
		{
			out_string("task: ".concat(cur.get_task().concat("\n")));
			cur.get_deps().print();
			out_string("\n");
			cur <- cur.get_next();
		}
		pool
	};
};

class DepItem {
	task: String;
	dep_list: StringList;
	get_task(): String {
		task
	};
	set_task(t: String): Object {
		task <- t
	};
	get_deps(): StringList {
		dep_list
	};
	set_deps(deps: StringList): Object {
		dep_list <- deps
	};
	next: DepItem;
	get_next(): DepItem {
		next
	};
	set_next(n: DepItem): Object {
		next <- n
	};
};

class StringList inherits Util {
	head: StringListItem;
	tail: StringListItem;
	append(v: String): Object {
		let n: StringListItem <- new StringListItem in
		{
			n.set_value(v);
			if isvoid tail then {
				tail <- n;
				head <- n;
			} else {
				tail.set_next(n);
				tail <- n;
			} fi;
		}
	};
	set_add(v: String): Object {
		if is_empty() then append(v) else {
			let cur: StringListItem <- head, done: Bool <- false in
			while not done loop
			let next: StringListItem <- cur.get_next() in {
				if cur.get_value() = v then done <- true else 0 fi;
				done <- if done then true else
					let void: Bool <- isvoid next in {
						if void then
						let n: StringListItem <- new StringListItem in {
							n.set_value(v);
							cur.set_next(n);
						} else 0 fi;
						void;
					} fi;
				cur <- next;
			}
			pool;
		} fi
	};
	remove(v: String): Object {
		let cur: StringListItem <- head, done: Bool <- false in
		if isvoid cur then 0 else if cur.get_value() = v then {
			head <- head.get_next();
			tail <- if isvoid head then head else tail fi;
		} else while not done loop
		let next: StringListItem <- cur.get_next() in {
			if not isvoid next then if next.get_value() = v then
			let next_next: StringListItem <- next.get_next() in {
				cur.set_next(next_next);
				if isvoid next_next then {
					tail <- cur;
				} else 0 fi;
				done <- true;
			} else 0 fi else 0 fi;
			cur <- next;
			done <- if done then true else isvoid cur fi;
		}
		pool fi fi
	};
	print(): Object {
		let cur: StringListItem <- head in
		while not isvoid cur loop
		{
			out_string(cur.get_value().concat("\n"));
			cur <- cur.get_next();
		}
		pool
	};
	is_empty(): Bool {
		isvoid head
	};
	contains(v: String): Bool {
		let cur: StringListItem <- head,
		found: Bool <- false in
		{
			while not isvoid cur loop
			{
				if cur.get_value() = v then found <- true else 0 fi;
				cur <- cur.get_next();
			}
			pool;
			found;
		}
	};
	select_smallest(): String {
		let cur: StringListItem <- head,
		found: String <- head.get_value() in
		{
			while not isvoid cur loop
			{
				if cur.get_value() < found then found <- cur.get_value() else 0 fi;
				cur <- cur.get_next();
			}
			pool;
			found;
		}
	};
	sorted(): StringList {
		let sorted: StringList <- new StringList in
		{
			while not is_empty() loop
			let smallest: String <- select_smallest() in {
				sorted.append(smallest);
				remove(smallest);
			} pool;
			sorted;
		}
	};
	pop_doable(deps: Deps): String {
		let cur: StringListItem <- head,
		done: Bool <- false,
		result: String <- "" in
		{
			while and(not isvoid cur, not done) loop
			{
				let v: String <- cur.get_value() in
				if not deps.has_dependencies(v) then {
					done <- true;
					result <- v;
					remove(v);
				} else 0 fi;
				cur <- cur.get_next();
			}
			pool;
			result;
		}
	};
	pop() : String {
		let cur: StringListItem <- head
		in {
			if isvoid head then 0 else head <- head.get_next() fi;
			if isvoid cur then "" else cur.get_value() fi;
		}
	};
};

class StringListItem {
	value: String;
	get_value(): String {
		value
	};
	set_value(v: String): Object {
		value <- v
	};
	next: StringListItem;
	get_next(): StringListItem {
		next
	};
	set_next(n: StringListItem): Object {
		next <- n
	};
};

class Main inherits Util {
	main(): Object {
		let input_list : StringList <- let l: StringList <- new StringList in {
			l.append("CA");
			l.append("CACM");
			l.append("Cabot");
			l.append("Cadillac");
			l.append("Cady");
			l.append("Caesar");
			l.append("Cahill");
			l.append("Cain");
			l.append("Caine");
			l.append("Cairo");
			l.append("Cal");
			l.append("Calais");
			l.append("Calcutta");
			l.append("Calder");
			l.append("Caldwell");
			l.append("Caleb");
			l.append("Calgary");
			l.append("Calhoun");
			l.append("California");
			l.append("Calkins");
			l.append("Callaghan");
			l.append("Callahan");
			l.append("Callisto");
			l.append("Calumet");
			l.append("c");
			l.append("cab");
			l.append("cabal");
			l.append("cabana");
			l.append("cabaret");
			l.append("cabbage");
			l.append("cabdriver");
			l.append("cabin");
			l.append("cabinet");
			l.append("cabinetmake");
			l.append("cabinetry");
			l.append("cable");
			l.append("cacao");
			l.append("cachalot");
			l.append("cache");
			l.append("cackle");
			l.append("cacophonist");
			l.append("cacophony");
			l.append("cacti");
			l.append("cactus");
			l.append("cadaver");
			l.append("cadaverous");
			l.append("caddis");
			l.append("caddy");
			l.append("cadent");
			l.append("cadenza");
			l.append("cadet");
			l.append("cadmium");
			l.append("cadre");
			l.append("cafe");
			l.append("cafeteria");
			l.append("cage");
			l.append("cagey");
			l.append("cahoot");
			l.append("caiman");
			l.append("cairn");
			l.append("cajole");
			l.append("cake");
			l.append("calamitous");
			l.append("calamity");
			l.append("calamus");
			l.append("calcareous");
			l.append("calcify");
			l.append("calcine");
			l.append("calcite");
			l.append("calcium");
			l.append("calculable");
			l.append("calculate");
			l.append("calculi");
			l.append("calculus");
			l.append("caldera");
			l.append("calendar");
			l.append("calendrical");
			l.append("calf");
			l.append("calfskin");
			l.append("caliber");
			l.append("calibrate");
			l.append("calibre");
			l.append("calico");
			l.append("californium");
			l.append("caliper");
			l.append("caliph");
			l.append("caliphate");
			l.append("calisthenic");
			l.append("call");
			l.append("calla");
			l.append("caller");
			l.append("calligraph");
			l.append("calligraphy");
			l.append("calliope");
			l.append("callous");
			l.append("callus");
			l.append("calm");
			l.append("caloric");
			l.append("calorie");
			l.append("calorimeter");
			l;
		},
		input: String <- input_list.pop(),
		prev_input: String <- "",
		i: Bool <- false,
		tasks: StringList <- new StringList,
		doable_list: StringList <- new StringList,
		deps: Deps <- new Deps in {
			while not input.length() = 0 loop
			{
				tasks.set_add(input);
				if i then {
					deps.add(prev_input, input);
				} else 0 fi;
				prev_input <- input;
				input <- input_list.pop();
				i <- not i;
			}
			pool;
			tasks <- tasks.sorted();
			let doable: String <- tasks.pop_doable(deps) in
			while not doable.length() = 0 loop {
				doable_list.append(doable);
				deps.remove(doable);
				doable <- tasks.pop_doable(deps);
			} pool;
			if tasks.is_empty() then {
				doable_list.print();
			} else out_string("cycle\n") fi;
		}
	};
};
