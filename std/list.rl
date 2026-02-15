import string;
import option;
import std.internal.gc;
import tuple;

type List<A> = { data: *A, length: int, capacity: int };

type size = int;


impl fn (c: *A) get_index<A>(index: int) -> A {
    let ptr = ptr_add(c, index * sizeof(A));
    *ptr
}

impl fn (c: List<A>) get_index<A>(index: int) -> A {
    if index >= c.length {
        GC.panic("Index out of bounds in List.get_index, "
            + show_prec(index, 0)
            + " (<index>) must be less than "
            + show_prec(c.length, 0)
            + " (<length>)"
        );
    };

    return c.data.get_index(index)
}

impl fn (x: String) get_index(index: int) -> char {
    let ptr = ptr_add(x.data, index * sizeof(char));
    *ptr as char
}

impl fn (c: *A) get_index_mut<A>(index: int) -> *A {
    let ptr = ptr_add(c, index * sizeof(A));
    ptr as *A
}

impl fn (c: List<A>) get_index_mut<A>(index: int) -> *A {
    c.data.get_index_mut(index)
}

mod List {
    fn init<A>() -> List<A> {
        {
            data: GC.calloc<A>(10),
            length: 0,
            capacity: 10
        }
    }

    impl fn (list: *List<A>) push<A>(value: A) -> *List<A> {
        if list->length == list->capacity {
            list->capacity = list->capacity + 10;
            list->data = GC.realloc(list->data, list->capacity * sizeof(A));
        };

        *list->data.get_index_mut(list->length) = value;
        list->length = list->length + 1;

        list
    }

    impl fn (list: List<A>) pop_at<A>(index: int) -> A {
        if index < 0 || index >= list.length {
            GC.panic("Index out of bounds in List.pop_at, "
                + index.show()
                + " (<index>) must be between 0 and "
                + (list.length - 1).show()
                + " (inclusive)"
            );
        };

        let value = list[index];
        let i = index;

        while i < list.length - 1 {
            let current_value = list[i + 1];
            *(list.data).get_index_mut(i) = current_value;
            i = i + 1;
        }

        list.length = list.length - 1;
        value
    }

    impl fn (list: List<A>) map<A, B>(f: fn(A) -> B) -> List<B> {
        let init_list = &List.init<B>();

        let i = 0;

        while i < list.length {
            let value = list[i];
            init_list.push(f(value));
            i = i + 1;
        };

        *init_list
    }

    impl fn (list: List<A>) extend<A>(other: List<A>) -> List<A> {
        let init_list = new List.init<A>();

        let i = 0;

        while i < list.length {
            let value = list[i];
            init_list.push(value);
            i = i + 1;
        };

        let j = 0;

        while j < other.length {
            let value = other[j];
            init_list.push(value);
            j = j + 1;
        };

        *init_list
    }

    impl fn (list: *List<A>) extend_mut<A>(other: List<A>) -> unit {
        let j = 0;

        while j < other.length {
            let value = other[j];
            list.push(value);
            j = j + 1;
        };

        unit
    }

    impl fn (l: List<A>) clone<A>() -> List<A> {
        let init_list = &List.init<A>();

        let i = 0;

        while i < l.length {
            let value = l[i];
            init_list.push(value.clone());
            i = i + 1;
        };

        *init_list
    }

    fn from_pointer<A>(ptr: *A, count: int) -> *List<A> {
        let list = new List.init<A>();

        let i = 0;

        while i < count {
            let value = ptr[i];
            list.push(value);
            i = i + 1;
        };

        list
    }

    impl fn (list: List<A>) show_prec<A>(prec: int) -> String {
        let result = "[";

        let i = 0;

        while i < list.length {
            let value = list[i];
            result = result + show_prec(value, prec + 1);

            if i < list.length - 1 {
                result = result + ", ";
            };

            i = i + 1;
        };

        result = result + "]";

        result
    }

    impl fn (list: List<A>) slice<A>(start: int, end: int) -> List<A> {
        let init_list = new List.init<A>();

        let i = start;

        while i < end && i < list.length {
            let value = list[i];
            init_list.push(value);
            i = i + 1;
        };

        *init_list
    }

    impl fn (list: List<A>) equals<A>(other: List<A>) -> bool {
        if list.length != other.length {
            return false;
        };

        let i = 0;

        while i < list.length {
            if list[i] != other[i] {
                return false;
            };
            i = i + 1;
        };

        return true;
    }

    impl fn (list: *List<A>) pop<A>() -> Option<A> {
        if list->length == 0 {
            return None;
        };

        list->length = list->length - 1;
        let value = (list->data)[list->length];

        Some(value)
    }

    /// Removes and returns the first element of the list, if it exists.
    impl fn (list: *List<A>) pop_front<A>() -> Option<A> {
        if list->length == 0 {
            return None;
        };

        let value = (list->data)[0];

        let i = 1;
        while i < list->length {
            let current_value = (list->data)[i];
            *(list->data).get_index_mut(i - 1) = current_value;
            i = i + 1;
        };

        list->length = list->length - 1;

        Some(value)
    }

    impl fn (list: List<A>) filter_map<A, B>(f: fn(A) -> Option<B>) -> List<B> {
        let init_list = new List.init<B>();

        let i = 0;

        while i < list.length {
            let value = list[i];
            let mapped = f(value);
            if mapped is Some(let v) {
                init_list.push(v);
            };
            i = i + 1;
        };

        *init_list
    }

    impl fn (list: List<A>) filter<A>(f: fn(A) -> bool) -> List<A> {
        let init_list = new List.init<A>();

        let i = 0;

        while i < list.length {
            let value = list[i];
            if f(value) {
                init_list.push(value);
            };
            i = i + 1;
        };

        *init_list
    }

    impl fn (c: String) starts_with(prefix: String) -> bool {
        if prefix.length > c.length {
            return false;
        };

        let i = 0;
        while i < prefix.length {
            if c[i] != prefix[i] {
                return false;
            };
            i = i + 1;
        };

        return true;
    }

    impl fn (c: List<A>) take_while<A>(predicate: fn(A) -> bool) -> List<A> {
        let result = new List.init<A>();

        let i = 0;
        while i < c.length {
            let value = c[i];
            if predicate(value) {
                result.push(value);
            } else {
                break;
            };
            i = i + 1;
        };

        return *result;
    }

    impl fn (c: List<A>) drop_while<A>(predicate: fn(A) -> bool) -> List<A> {
        let result = new List.init<A>();

        let i = 0;
        while i < c.length {
            let value = c[i];
            if not(predicate(value)) {
                break;
            };
            i = i + 1;
        };

        while i < c.length {
            let value = c[i];
            result.push(value);
            i = i + 1;
        };

        return *result;
    }

    impl fn (c: String) split(sep: char) -> List<String> {
        let result = new List.init<String>();

        let currentPart = "";
        let i = 0;
        while i < c.length {
            let c_char = c[i];
            if c_char == sep {
                result.push(currentPart);
                currentPart = "";
            } else {
                currentPart = currentPart + String.init(GC.allocate(c_char));
            };
            i = i + 1;
        };

        result.push(currentPart);

        return *result;
    }

    impl fn (c: List<A>) partition<A>(predicate: fn(A) -> bool) -> Tuple<List<A>, List<A>> {
        let trueList = &List.init<A>();
        let falseList = &List.init<A>();

        let i = 0;
        while i < c.length {
            let value = c[i];
            if predicate(value) {
                trueList.push(value);
            } else {
                falseList.push(value);
            };
            i = i + 1;
        };

        return Pair(*trueList, *falseList);
    }
}

impl fn (x: char) show_prec(_: int) -> String {
    let data = GC.allocate(x);

    "'" + String.init(data) + "'"
}

pub fn getArgs(argc: int, argv: *string) -> List<String> {
    List.from_pointer(argv, argc)->map(String.init)
}

mod String {
    fn from_chars(chars: List<char>) -> String {
        let result = "";
        let i = 0;
        while i < chars.length {
            let c = chars[i];
            result = result + String.init(GC.allocate(c));
            i = i + 1;
        };
        result
    }
}

impl fn (x: List<String>) join(separator: String) -> String {
    if x.length == 0 {
        return "";
    };

    let result = x[0];
    let i = 1;
    while i < x.length {
        result = result + separator + x[i];
        i = i + 1;
    };

    return result;
}
