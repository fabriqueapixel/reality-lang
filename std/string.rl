import std.internal.gc;

type string = *char;

extern fn malloc_string(s: string) -> string;
#[intrinsic] {
    extern fn strcat(x: string, y: string) -> string;
    extern fn puts(s: string) -> int;
    extern fn strlen(s: string) -> int;
}
extern fn concat_strings(a: string, b: string) -> string;
extern fn char_eq(a: char, b: char) -> bool;
extern fn string_to_int(s: string) -> int;

impl fn (x: string) length() -> int {
    strlen(x)
}

impl fn (x: string) equals(y: string) -> bool {
    string_eq(x, y)
}

type String = { data: string, length: int };

impl fn (x: string) get_index(index: int) -> char {
    let ptr = ptr_add(x, index * sizeof(char));
    *ptr as char
}

impl fn (x: string) get_index_mut(index: int) -> *char {
    let ptr = ptr_add(x, index * sizeof(char));
    ptr as *char
}

impl fn (x: String) get_index(index: int) -> char {
    let ptr = ptr_add(x.data, index * sizeof(char));
    *ptr as char
}

impl fn (x: String) get_index_mut(index: int) -> *char {
    let ptr = ptr_add(x.data, index * sizeof(char));
    ptr as *char
}

mod String {
    fn init(data: string) -> String {
        let string_mem = malloc_string(data);

        {
            data: string_mem,
            length: strlen(string_mem)
        }
    }

    fn from_char(c: char) -> String {
        let s = "0";
        *s.data.get_index_mut(0) = c;

        s
    }
}

impl fn (x: String) add(y: String) -> String {
    let result = concat_strings(x.data, y.data);

    {
        data: result,
        length: strlen(result)
    }
}

property show_prec<A>(x: A, i: int) -> String;

impl fn (x: String) show_prec(i: int) -> String {
    if i > 0 {
        "\"" + x + "\""
    } else {
        x
    }
}

impl fn (x: int) show_prec(_: int) -> String {
    String.init(int_to_string(x))
}

impl fn (x: float) show_prec(i: int) -> String {
    String.init(float_to_string(x))
}

impl fn (x: int) show_prec(_: int) -> String {
    String.init(number_to_string(x))
}

impl fn (x: bool) show_prec(_: int) -> String {
    if (x) { "true" } else { "false" }
}

impl fn (x: *A) show_prec<A>(i: int) -> String {
    show_prec(*x, i)
}

fn show<A>(x: A) -> String {
    show_prec(x, 0)
}

fn print<A>(x: A) -> int {
    puts(show(x).data)
}

impl fn (x: String) equals(y: String) -> bool {
    string_eq(x.data, y.data)
}

impl fn (x: char) equals(y: char) -> bool {
    char_eq(x, y)
}

extern fn int_to_char(x: int) -> char;
extern fn char_to_int(c: char) -> int;
extern fn string_to_float(str: string) -> float;
extern fn string_to_int(str: string) -> int;

impl fn (x: String) into_float() -> float {
    string_to_float(x.data)
}

impl fn (x: String) into_int() -> int {
    string_to_int(x.data)
}

impl fn (x: int) to_char() -> char {
    int_to_char(x)
}

impl fn (x: char) from_char() -> int {
    char_to_int(x)
}

impl fn (x: String) to_int() -> int {
    string_to_int(x.data)
}


mod GC {
    fn red(message: String) -> String {
        "\x1b[31m" + message + "\x1b[0m"
    }

    pub fn panic<A>(message: A) -> never {
        let msg_prefix = GC.red("[Panic]: ");
        let full_message = msg_prefix + message.show();
        panic_ext(full_message.data)

        undefined()
    }
}

impl fn (x: String) repeat(n: int) -> String {
    let result = "";
    let i = 0;

    while i < n {
        result = result + x;
        i = i + 1;
    };

    result
}

impl fn (x: String) slice(start: int, end: int) -> String {
    // for instance "hello".slice(1,4) = "ell"
    let length = if end > x.length { x.length } else { end };
    let result = "";
    let i = start;
    while i < length {
        let c = x[i];
        result = result + String.init(GC.allocate(c));
        i = i + 1;
    };

    result
}

impl fn (x: String) trim_left() -> String {
    let start = 0;
    while start < x.length && x.get_index(start) == ' ' {
        start = start + 1;
    };

    let result = "";
    let i = start;
    while i < x.length {
        result = result + String.from_char(x.get_index(i));
        i = i + 1;
    };

    return result;
}

impl fn (x: String) trim_right() -> String {
    let end = x.length - 1;
    while end >= 0 && x.get_index(end) == ' ' {
        end = end - 1;
    };

    let result = "";
    let i = 0;
    while i <= end {
        result = result + String.from_char(x.get_index(i));
        i = i + 1;
    };

    return result;
}

impl fn (x: String) trim() -> String {
    return x.trim_left().trim_right();
}

impl fn (x: String) clone() -> String {
    let result = "";
    let i = 0;
    while i < x.length {
        result = result + String.from_char(x.get_index(i));
        i = i + 1;
    };

    return result;
}

impl fn (x: String) ends_with(suffix: String) -> bool {
    if suffix.length > x.length {
        return false;
    };

    let start = x.length - suffix.length;
    let i = 0;
    while i < suffix.length {
        if x.get_index(start + i) != suffix.get_index(i) {
            return false;
        };

        i = i + 1;
    };

    return true;
}

impl fn (x: String) starts_with(prefix: String) -> bool {
    if prefix.length > x.length {
        return false;
    };

    let i = 0;
    while i < prefix.length {
        if x.get_index(i) != prefix.get_index(i) {
            return false;
        };

        i = i + 1;
    };

    return true;
}

impl fn (x: String) replace(old: String, new_str: String) -> String {
    let result = "";
    let i = 0;

    while i < x.length {
        if x.slice(i, i + old.length).equals(old) {
            result = result + new_str;
            i = i + old.length;
        } else {
            result = result + String.from_char(x.get_index(i));
            i = i + 1;
        };
    };

    return result;
}
