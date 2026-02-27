import string;
import std.internal.gc;

enum Tuple<A, B> {
    Pair(x: A, y: B)
}

impl fn (t: Tuple<A, B>) show_prec<A, B>(prec: int) -> String {
    if t is Pair(let a, let b) {
        "("
            + a.show_prec(prec + 1)
            + ", "
            + b.show_prec(prec + 1)
            + ")"
    } else {
        "<invalid tuple>"
    }
}

impl fn (t: Tuple<A, B>) fst<A, B>() -> A {
    if t is Pair(let a, let _) {
        return a;
    } else {
        GC.panic("Invalid tuple in fst");
    }
}

impl fn (t: Tuple<A, B>) snd<A, B>() -> B {
    if t is Pair(let _, let b) {
        return b;
    } else {
        GC.panic("Invalid tuple in snd");
    }
}

impl fn (t: Tuple<A, B>) map_first<A, B, C>(f: fn(x: A) -> C) -> Tuple<C, B> {
    if t is Pair(let a, let b) {
        return Pair(f(a), b);
    } else {
        GC.panic("Invalid tuple in map_first");
    }
}

impl fn (t: Tuple<A, B>) map_second<A, B, C>(f: fn(x: B) -> C) -> Tuple<A, C> {
    if t is Pair(let a, let b) {
        return Pair(a, f(b));
    } else {
        GC.panic("Invalid tuple in map_second");
    }
}

impl fn (t: Tuple<A, B>) map<A, B, C, D>(f: fn(x: A) -> C, g: fn(x: B) -> D) -> Tuple<C, D> {
    if t is Pair(let a, let b) {
        return Pair(f(a), g(b));
    } else {
        GC.panic("Invalid tuple in map");
    }
}

impl fn (t: Tuple<A, B>) clone<A, B>() -> Tuple<A, B> {
    if t is Pair(let a, let b) {
        return Pair(a.clone(), b.clone());
    } else {
        GC.panic("Invalid tuple in clone");
    }
}
