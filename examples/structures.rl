import std.prelude;

fn ackermann(m: int, n: int) -> int {
    if m == 0 {
        return n + 1;
    }

    if m > 0 && n == 0 {
        return ackermann(m - 1, 1);
    }
    
    return ackermann(m - 1, ackermann(m, n - 1));
}
