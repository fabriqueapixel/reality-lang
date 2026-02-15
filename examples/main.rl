import std.prelude;
import structures;

fn main(args: List<String>) -> int {
    let m = int(args[1]);
    let n = int(args[2]);

    print(f"Ackermann({m}, {n}) = {ackermann(m, n)}");

    return 0;
}
