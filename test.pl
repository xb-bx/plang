import builtins


fn main(args: []string): void {
    if a > b {
        builtins.println("A > B")
    } else {
        builtins.println("A <= B")
    };
    while a > 0 {
        a = box a
    };
    return a * b
}
struct Point {
    x: i64;
    y: i64
}
