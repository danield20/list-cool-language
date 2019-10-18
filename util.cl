(* Think of these as abstract classes *)
class Comparator {
    compareTo(o1 : Object, o2 : Object):Int {0};
};

class PriceComparator inherits Comparator {
    compareTo(o1 : Object, o2 : Object): Int {
        {
            case o1 of x: Product =>
                case o2 of y: Product =>
                    x.getprice() - y.getprice();
                esac;
            esac;
        }
    };
};

class RankComparator inherits Comparator {
    compareTo(o1 : Object, o2 : Object): Int {
        let r1: Int <- 0,
            r2: Int <- 0
        in
            {
                case o1 of
                    x: Private => r1 <- 1;
                    x: Corporal => r1 <- 2;
                    x: Sergent => r1 <- 3;
                    x: Officer => r1 <- 4;
                    x: Object => r1 <- 5;
                esac;

                case o2 of
                    x: Private => r2 <- 1;
                    x: Corporal => r2 <- 2;
                    x: Sergent => r2 <- 3;
                    x: Officer => r2 <- 4;
                    x: Object => r2 <- 5;
                esac;

                if r1 = 5 then {
                    abort();
                    0;
                } else
                    if r2 = 5 then {
                        abort();
                        0;
                    } else
                        r1 - r2
                    fi
                fi;
            }
    };
};

class AlphabeticComparator inherits Comparator {
    compareTo(o1 : Object, o2 : Object): Int {
            case o1 of x: String =>
                case o2 of y: String =>
                    if x < y then
                        0-1
                    else
                        if y < x then
                            1
                        else
                            0
                        fi
                    fi;
                esac;
            esac
    };
};

class Filter {
    filter(o : Object):Bool {true};
};

class ProductFilter inherits Filter {
    filter(o: Object): Bool {
        case o of
            x: Product => true;
            x: Object => false;
        esac
    };
};

class RankFilter inherits Filter {
    filter(o: Object): Bool {
        case o of
            x: Rank => true;
            x: Object => false;
        esac
    };
};

class SamePriceFilter inherits Filter {
    filter(o: Object): Bool {
        case o of
            x: Product =>
                let p1: Int <- x.getprice(),
                    p2: Int <- x@Product.getprice()
                in
                    {
                        if p1 = p2 then
                            true
                        else
                            false
                        fi;
                    };
            x: Object => false;
        esac
    };
};

class LesserFilter inherits Filter {
    o: Object;
    c: Comparator;

    init(obj: Object, comp: Comparator): SELF_TYPE {{
        o <- obj;
        c <- comp;
        self;
    }};

    filter(o1: Object): Bool {
        if c.compareTo(o, o1) = 1 then
            true
        else
            if c.compareTo(o, o1) = 0 then
                true
            else
                false
            fi
        fi
    };
};

class BiggerFilter inherits Filter {
    o: Object;
    c: Comparator;

    init(obj: Object, comp: Comparator): SELF_TYPE {{
        o <- obj;
        c <- comp;
        self;
    }};

    filter(o1: Object): Bool {
        if c.compareTo(o, o1) = 1 then
            false
        else
            if c.compareTo(o, o1) = 0 then
                false
            else
                true
            fi
        fi
    };
};

class StringUtils inherits IO {
    indexOfChar(s1: String, s2: String): Int {
        let i: Int <- 0,
            auxStr: String <- s1,
            size: Int <- auxStr.length(),
            currentChar: String,
            return: Int <- 0 - 1,
            found: Bool <- false
        in
            {
                while (not i = size) loop
                    {
                        currentChar <- auxStr.substr(0, 1);
                        if currentChar = s2 then
                            if not found then
                                {
                                    return <- i;
                                    found <- true;
                                }
                            else
                                1
                            fi
                        else
                            1
                        fi;
                        i <- i + 1;
                        auxStr <- auxStr.substr(1, size - i);
                    }
                pool;
                return;
            }
    };
};

class StringTokenizer inherits StringUtils {
    separators: String;
    tokenList: List;

    init(s: String, sep: String): SELF_TYPE {
        {
            tokenList <- new List;
            separators <- sep;
            let i: Int <- 0,
                auxStr: String <- s,
                currentToken: String,
                size: Int <- auxStr.length()
            in
                {
                    while not auxStr = "" loop
                        {
                            i <- indexOfChar(auxStr, sep);
                            if i = (0 - 1) then
                                {
                                    tokenList <- tokenList.add(auxStr);
                                    auxStr <- "";
                                }
                            else
                                if i = 0 then {
                                    auxStr <- auxStr.substr(1,auxStr.length() - 1);
                                }
                                else
                                    {
                                        currentToken <- auxStr.substr(0, i);
                                        auxStr <- auxStr.substr(i + 1, auxStr.length() - i - 1);
                                        if not currentToken.length() = 0 then
                                            tokenList <- tokenList.add(currentToken)
                                        else
                                            1
                                        fi;
                                    }
                                fi
                            fi;
                        }
                    pool;
                    self;
                };
        }
    };

    printTokens(): IO {
        out_string(tokenList.toString())
    };

    getTokens(): List {
        tokenList
    };
};
