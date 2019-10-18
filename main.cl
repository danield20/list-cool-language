class Main inherits IO{
    lists : List <- new List;
    looping : Bool <- true;
    somestr : String;
    converter: A2I <- new A2I;

    dispatch_object(str: String): Object {
        let ret: Object
        in
            {
                if str = "Edible" then
                    ret <- new Edible
                else
                    if str = "Soda" then
                        ret <- new Soda
                    else
                        if str = "Coffee" then
                            ret <- new Coffee
                        else
                            if str = "Laptop" then
                                ret <- new Laptop
                            else
                                if str = "Router" then
                                    ret <- new Router
                                else
                                    1
                                fi
                            fi
                        fi
                    fi
                fi;

                if str = "Private" then
                    ret <- new Private
                else
                    if str = "Corporal" then
                        ret <- new Corporal
                    else
                        if str = "Sergent" then
                            ret <- new Sergent
                        else
                            if str = "Officer" then
                                ret <- new Officer
                            else
                                1
                            fi
                        fi
                    fi
                fi;

                ret;
            }
    };

    help(): IO {
        {
            out_string("Available commands:\n");
            out_string("-help, load, print\n");
            out_string("-merge i1 i2, filterBy index {ProductFilter,RankFilter,SamePriceFilter}\n");
            out_string("-sortBy index {PriceComparator,RankComparator,AlphabeticComparator} {ascendent,descendent}\n");
        }
    };

    load(): String {
        let current_obj_str: String <- "",
            current_obj: Object,
            tokenizer: StringTokenizer <- new StringTokenizer,
            obj_type: String,
            current_line: String,
            loops: Bool <- true,
            current_list: List <- new List
        in
            {
                while loops loop {
                    current_line <- in_string();
                    out_string("OBJ: ".concat(current_line).concat("\n"));
                    if (current_line = "END") then
                        loops <- false
                    else
                        {
                            tokenizer <- tokenizer.init(current_line, " ");

                            case tokenizer.getTokens().hd() of
                                x: String => current_obj_str <- x;
                            esac;

                            current_obj <- dispatch_object(current_obj_str);

                            case current_obj of
                                x: Product =>
                                    let n: String,
                                        m: String,
                                        p: Int,
                                        current_prod: Product
                                    in
                                        {
                                            case tokenizer.getTokens().getIndex(1) of
                                                x: String => n <- x;
                                            esac;

                                            case tokenizer.getTokens().getIndex(2) of
                                                x: String => m <- x;
                                            esac;

                                            case tokenizer.getTokens().getIndex(3) of
                                                x: String => p <- converter.a2i(x);
                                            esac;

                                            current_prod <- x.init(n, m, p);
                                            current_list <- current_list.add(current_prod);
                                        };
                                x: Rank =>
                                     let n: String,
                                         current_rank: Rank
                                    in
                                        {
                                            case tokenizer.getTokens().getIndex(1) of
                                                x: String => n <- x;
                                            esac;

                                            current_rank <- x.init(n);
                                            current_list <- current_list.add(current_rank);
                                        };
                            esac;
                        }
                    fi;
                } pool;
                lists <- lists.add(current_list);
                current_obj_str;
            }
    };

    print(): Int {
        let idx: Int <- 1,
            aux: List <- lists
        in
            {
                while (not aux.isEmpty()) loop
                {
                    out_int(idx).out_string(": ");
                    case aux.hd() of
                        x: List => out_string(x.toString().concat("\n"));
                    esac;
                    aux <- aux.tl();
                    idx <- idx + 1;
                }
	    		pool;
                idx;
            }
    };

    merge(i1: Int, i2: Int): Int {
        let new_lists: List,
            aux_list: List
        in
            {
                case lists.getIndex(i1 - 1) of
                    x: List => aux_list <- x;
                esac;

                case lists.getIndex(i2 - 1) of
                    x: List => aux_list <- aux_list.merge(x);
                esac;

                lists <- lists.removeIndex(i1 - 1);
                if i1 < i2 then
                    i2 <- i2 -1
                else
                    1
                fi;

                lists <- lists.removeIndex(i2 - 1);
                lists <- lists.add(aux_list);
                1;
            }
    };

    filterBy(i: Int, filter: Filter): Int {
        let aux_lists: List <- new List,
            aux: List <- lists,
            current_list: List,
            current_idx: Int <- 0
        in
            {
                while (not aux.isEmpty()) loop
                {
                    if i - 1 = current_idx then
                    {
                        case lists.getIndex(current_idx) of
                            x: List => current_list <- x;
                        esac;
                        current_list <- current_list.filterBy(filter);
                        aux_lists <- aux_lists.add(current_list);
                    }
                    else
                        aux_lists <- aux_lists.add(lists.getIndex(current_idx))
                    fi;

                    aux <- aux.tl();
                    current_idx <- current_idx + 1;
                }
	    		pool;
                lists <- aux_lists;
                i;
            }
    };

    sortBy(i: Int, comp: Comparator, asc: Bool): Int {
        let aux_lists: List <- new List,
            aux: List <- lists,
            current_list: List,
            current_idx: Int <- 0
        in
            {
                while (not aux.isEmpty()) loop
                {
                    if i - 1 = current_idx then
                    {
                        case lists.getIndex(current_idx) of
                            x: List => current_list <- x;
                        esac;
                        current_list <- current_list.sortBy(comp, asc);
                        aux_lists <- aux_lists.add(current_list);
                    }
                    else
                        aux_lists <- aux_lists.add(lists.getIndex(current_idx))
                    fi;

                    aux <- aux.tl();
                    current_idx <- current_idx + 1;
                }
	    		pool;
                lists <- aux_lists;
                i;
            }
    };

    getFilter(f: String): Filter {
        let ret: Filter
        in
            {
                if f = "ProductFilter" then
                    ret <- new ProductFilter
                else
                    if f = "RankFilter" then
                        ret <- new RankFilter
                    else
                        if f = "SamePriceFilter" then
                            ret <- new SamePriceFilter
                        else
                            1
                        fi
                    fi
                fi;
                ret;
            }
    };

    getComparator(c: String): Comparator {
        let ret: Comparator
        in
            {
                if c = "PriceComparator" then
                    ret <- new PriceComparator
                else
                    if c = "RankComparator" then
                        ret <- new RankComparator
                    else
                        if c = "AlphabeticComparator" then
                            ret <- new AlphabeticComparator
                        else
                            1
                        fi
                    fi
                fi;
                ret;
            }
    };

    process_cmd(cmd: String, tokenizer: StringTokenizer): Int {{
        if cmd = "help" then
            help()
        else
            if cmd = "print" then
                print()
            else
                if cmd = "load" then
                    load()
                else
                    if cmd = "merge" then
                        let i1: String,
                            i2: String
                        in
                        {
                            case tokenizer.getTokens().getIndex(1) of
                                x: String => i1 <- x;
                            esac;

                            case tokenizer.getTokens().getIndex(2) of
                                x: String => i2 <- x;
                            esac;

                            merge(converter.a2i(i1), converter.a2i(i2));
                        }
                    else
                        if cmd = "filterBy" then
                            let i: String,
                                filter: String,
                                flt: Filter
                            in
                            {
                                case tokenizer.getTokens().getIndex(1) of
                                    x: String => i <- x;
                                esac;

                                case tokenizer.getTokens().getIndex(2) of
                                    x: String => filter <- x;
                                esac;

                                flt <- getFilter(filter);
                                filterBy(converter.a2i(i), flt);
                            }
                        else
                            if cmd = "sortBy" then
                                let i: String,
                                    comparator: String,
                                    order: String,
                                    comp: Comparator,
                                    asc: Bool
                                in
                                {
                                    case tokenizer.getTokens().getIndex(1) of
                                        x: String => i <- x;
                                    esac;

                                    case tokenizer.getTokens().getIndex(2) of
                                        x: String => comparator <- x;
                                    esac;

                                    case tokenizer.getTokens().getIndex(3) of
                                        x: String => order <- x;
                                    esac;

                                    if order = "ascendent" then
                                        asc <- true
                                    else
                                        asc <- false
                                    fi;

                                    comp <- getComparator(comparator);

                                    sortBy(converter.a2i(i), comp, asc);
                                }
                            else
                                1
                            fi
                        fi
                    fi
                fi
            fi
        fi;
        1;
    }};

    main():Object {
            let current_cmd: String <- "",
                tokenizer: StringTokenizer <- new StringTokenizer,
                cmd_type: String
	        in
	            {
                    load();
                    out_string("Loading over\n\n");
                    while looping loop {
                        out_string("CMD: ".concat(somestr));
                        somestr <- in_string();
                        if (somestr = "END") then
                            looping <- false
                        else
                            {
                                tokenizer <- tokenizer.init(somestr, " ");
                                case tokenizer.getTokens().hd() of
                                    x: String => cmd_type <- x;
                                esac;

                                process_cmd(cmd_type, tokenizer);
                            }
                        fi;
                        somestr <- "";
                    } pool;
	            }
    };
};