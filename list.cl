class List inherits IO {

    (* TODO: store data *)
    isEmpty() : Bool { true };

    hd(): Object {
        {
            abort();
            self;
        }
    };

    tl(): List {
        {
            abort();
            self;
        }
    };

    add(o: Object): List {
        new Cons.init(o, self)
    };

    append(l2 : List) : List { l2 };

    cons(o: Object): List {
        new Cons.init(o, self)
    };

    contains(o: Object): Bool {
        false
    };

    filterBy(f: Filter): List {
        self
    };

    getIndex(x: Int): Object {
        {
            abort();
            "ret";
        }
    };

    merge(other: List): List {
        append(other)
    };

    removeIndex(i: Int): List {
        {
            abort();
            self;
        }
    };

    reverse() : List { { abort(); self; } };

    sortBy(c: Comparator, asc: Bool): List {
        self
    };

    toString(): String { "[]" };
};

class Cons inherits List {

    hd: Object;
    tl: List;
    converter: A2I <- new A2I;

    init(h : Object, t : List) : Cons {
        {
            hd <- h;
            tl <- t;
            self;
        }
    };

    isEmpty() : Bool { false };

    hd() : Object { hd };

    tl() : List { tl };

    add(o : Object) : List {
		let reverseList : List <- self.reverse(),
			newList : List <- reverseList.cons(o)
		in
    		newList.reverse()
    };

    append(l2 : List) : List {
        if l2.isEmpty() then
            self
        else
            let reverseList : List <- self.reverse(),
                newList : List <- reverseList.cons(l2.hd()),
                aux : List <- l2.tl()
            in
            {
                while (not aux.isEmpty()) loop
                    {
                        newList <- newList.cons(aux.hd());
                        aux <- aux.tl();
                    }
                pool;
                newList.reverse();
            }
        fi
    };

    contains(o: Object): Bool {
        let aux: List <- self,
            ret: Bool <- false
		in
            {
                while (not aux.isEmpty()) loop
                    {
                        if aux.hd() = o then
                            ret <- true
                        else
                            1
                        fi;
                        aux <- aux.tl();
                    }
                pool;
                ret;
            }
    };

    filterBy(f: Filter): List {
        let newList : List <- new List,
    		aux : List <- tl()
		in
			{
                if (f.filter(hd)) then
                    newList <- newList.add(hd)
                else
                    1
                fi;

	    		while (not aux.isEmpty()) loop
	    			{
                        if (f.filter(aux.hd())) then
                            newList <- newList.add(aux.hd())
                        else
                            1
                        fi;
	    				aux <- aux.tl();
	    			}
	    		pool;
	    		newList;
	    	}
    };

    getIndex(i: Int): Object {
        let current_idx: Int <- 0,
            ret: Object,
            str: String,
            aux: List <- self
		in
            {
                while (not aux.isEmpty()) loop
                    {
                        if current_idx = i then
                            ret <- aux.hd()
                        else
                            1
                        fi;
                        aux <- aux.tl();
                        current_idx <- current_idx + 1;
                    }
                pool;

                ret;
            }
    };

    removeIndex(i: Int): List {
        let current_idx: Int <- 0,
            ret: List <- new List,
            aux: List <- self
		in
            {
                while (not aux.isEmpty()) loop
                    {
                        if current_idx = i then
                            1
                        else
                            ret <- ret.add(aux.hd())
                        fi;
                        aux <- aux.tl();
                        current_idx <- current_idx + 1;
                    }
                pool;
                ret;
            }
    };

    reverse() : List {
    	let newList : List <- new List.cons(hd()),
    		aux : List <- tl()
		in
			{
	    		while (not aux.isEmpty()) loop
	    			{
	    				newList <- newList.cons(aux.hd());
	    				aux <- aux.tl();
	    			}
	    		pool;
	    		newList;
	    	}
    };

    sortBy(c: Comparator, asc: Bool): List {
        let new_list: List <- new List,
            l1: List,
            l2: List
        in
            {
                if isEmpty() then
                    new_list <- new List
                else
                {
                    if asc then {
                        l1 <- tl().filterBy(new LesserFilter.init(hd(), c));
                        l2 <- tl().filterBy(new BiggerFilter.init(hd(), c));
                    } else {
                        l2 <- tl().filterBy(new LesserFilter.init(hd(), c));
                        l1 <- tl().filterBy(new BiggerFilter.init(hd(), c));
                    } fi;

                    new_list <- l1.sortBy(c, asc).append(new List.add(hd())).append(l2.sortBy(c, asc));
                }
                fi;
                new_list;
            }
    };

    toString(): String {
        {
            let return: String <- "[ ",
                hd: Object,
                aux: List <- self
            in
                {
                    while (not aux.isEmpty()) loop
                        {
                            hd <- aux.hd();
                            case hd of
                                x: Int => return <- return.concat("Int(").concat(converter.i2a(x)).concat(")");
                                x: String => return <- return.concat("String(").concat(x).concat(")");
                                x: Product => return <- return.concat(x.toString());
                                x: Rank => return <- return.concat(x.toString());
                                x: IO => return <- return.concat("IO()");
                                x: Bool => return <- return.concat("Bool(").concat(converter.b2s(x)).concat(")");
                                x: List => return <- return.concat(x.toString());
                            esac;

                            aux <- aux.tl();

                            if not aux.isEmpty() then
                                return <- return.concat(", ")
                            else
                                return <- return.concat(" ]")
                            fi;
                        }
	    		    pool;
                    return;
                };
        }
    };
};
