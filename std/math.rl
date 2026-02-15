type string = *char;

property and<A>(a: A, b: A) -> A;

impl fn (a: bool) and(b: bool) -> bool {
    if a {
        b
    } else {
        false
    }
}

property or<A>(a: A, b: A) -> A; 

impl fn (a: bool) or(b: bool) -> bool {
    if a {
        true
    } else {
        b
    }
}

property not<A>(a: A) -> A;

impl fn (a: bool) not() -> bool {
    if a {
        false
    } else {
        true
    }
}


property add<A>(x: A, y: A) -> A;
property sub<A>(x: A, y: A) -> A;
property mul<A>(x: A, y: A) -> A;
property div<A>(x: A, y: A) -> A;
property modulo<A>(x: A, y: A) -> A;
property greater<A>(x: A, y: A) -> bool;
property lesser<A>(x: A, y: A) -> bool;

property equals<A>(x: A, y: A) -> bool;
extern fn number_to_string(n: int) -> string;
extern fn pointer_to_string<A>(p: A) -> string;
extern fn add_number(a: int, b: int) -> int;
extern fn sub_number(a: int, b: int) -> int;
extern fn mul_number(a: int, b: int) -> int;
extern fn div_number(a: int, b: int) -> int;
extern fn equals_number(a: int, b: int) -> bool;
extern fn greater_number(a: int, b: int) -> bool;
extern fn less_number(a: int, b: int) -> bool;
extern fn mod_number(a: int, b: int) -> int;
extern fn string_eq(a: string, b: string) -> bool;

fn int_to_string(n: int) -> string {
    number_to_string(n)
}

impl fn (x: int) add(y: int) -> int {
    add_number(x, y)
}

impl fn (x: int) sub(y: int) -> int {
    sub_number(x, y)
}

impl fn (x: int) mul(y: int) -> int {
    mul_number(x, y)
}

impl fn (x: int) div(y: int) -> int {
    div_number(x, y)
}

impl fn (x: int) equals(y: int) -> bool {
    equals_number(x, y)
}

impl fn (x: int) greater(y: int) -> bool {
    greater_number(x, y)
}

impl fn (x: int) lesser(y: int) -> bool {
    less_number(x, y)
}

impl fn (x: int) modulo(y: int) -> int {
    mod_number(x, y)
}

fn great_equals<A>(x: A, y: A) -> bool {
    or(greater(x, y), equals(x, y))
}

fn less_equals<A>(x: A, y: A) -> bool {
    or(lesser(x, y), equals(x, y))
}

fn not_equals<A>(x: A, y: A) -> bool {
    not(equals(x, y))
}

property into_int<A>(x: A) -> int;
property into_float<A>(x: A) -> float;

impl fn (x: int) into_int() -> int {
    x
}

impl fn (x: float) into_float() -> float {
    x
}

extern fn int_to_float(x: int) -> float;
extern fn float_to_int(x: float) -> int;


impl fn (x: int) into_float() -> float {
    int_to_float(x)
}

impl fn (x: float) into_int() -> int {
    float_to_int(x)
}

fn float<A>(x: A) -> float {
    return x.into_float();
}

fn int<A>(x: A) -> int {
    return x.into_int();
}

extern fn add_float_(a: float, b: float) -> float;
extern fn sub_float_(a: float, b: float) -> float;
extern fn mul_float_(a: float, b: float) -> float;
extern fn div_float_(a: float, b: float) -> float;
extern fn equals_float_(a: float, b: float) -> bool;
extern fn greater_float_(a: float, b: float) -> bool;
extern fn less_float_(a: float, b: float) -> bool;
extern fn mod_float_(a: float, b: float) -> float;
extern fn float_to_string(f: float) -> string;

impl fn (x: float) add(y: float) -> float {
    add_float_(x, y)
}
impl fn (x: float) sub(y: float) -> float {
    sub_float_(x, y)
}

impl fn (x: float) mul(y: float) -> float {
    mul_float_(x, y)
}

impl fn (x: float) div(y: float) -> float {
    div_float_(x, y)
}

impl fn (x: float) equals(y: float) -> bool {
    equals_float_(x, y)
}

impl fn (x: float) greater(y: float) -> bool {
    greater_float_(x, y)
}

impl fn (x: float) lesser(y: float) -> bool {
    less_float_(x, y)
}

impl fn (x: float) negate() -> float {
    0.0.sub(x)
}

extern fn sqrt_float(x: float) -> float;

fn sqrt<A>(x: A) -> float {
    sqrt_float(x.into_float())
}

extern fn pow_float(base: float, exponent: float) -> float;

fn pow<A, B>(base: A, exponent: B) -> float {
    pow_float(base.into_float(), exponent.into_float())
}
