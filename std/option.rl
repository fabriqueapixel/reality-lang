import string;

enum Option<T> {
    Some(val: T),
    None
}

impl fn (c: Option<T>) show_prec<T>(i: int) -> String {
    if c is Some(let v) {
        "Some(" + show_prec(v, i + 1) + ")"
    } else {
        "None"
    }
}

mod Option {
    fn map<A, B>(c: Option<A>, f: fn(x: A) -> B) -> Option<B> {
        if c is Some(let v) {
            Some(f(v))
        } else {
            None
        }
    }

    impl fn (c: Option<A>) or_else<A>(d: Option<A>) -> Option<A> {
        if c is Some(let v) {
            Some(v)
        } else {
            d
        }
    }

    impl fn (c: Option<A>) unwrap<A>() -> A {
        if c is Some(let v) {
            v
        } else {
            GC.panic("Called unwrap on a None value");
        }
    }

    impl fn (c: Option<A>) get_or_else<A>(default: A) -> A {
        if c is Some(let v) {
            v
        } else {
            default
        }
    }

    impl fn (c: Option<A>) is_some<A>() -> bool {
        if c is Some(let x) {
            true
        } else {
            false
        }
    }

    impl fn (c: Option<A>) is_none<A>() -> bool {
        if c is None {
            true
        } else {
            false
        }
    }
}
