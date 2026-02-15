import std.math;

extern let intrinsic: void;

extern fn ptr_add<A>(ptr: A, offset: int) -> A;
extern fn fetch_ptr<A>(ptr: A, index: int) -> A;

enum unit {
    unit
}


mod GC {
    type GarbageCollector = { };

    extern fn undefined() -> never;
    extern fn exit_program(code: int) -> never;

    fn exit(code: int) -> never {
        exit_program(code)
    }

    #[intrinsic] {
        extern fn GC_malloc<A>(size: int) -> A;
        extern fn GC_free<A>(ptr: A) -> unit;
        extern fn GC_realloc<A>(ptr: A, size: int) -> A;
    }

    extern fn panic_ext(message: string) -> unit;

    pub fn malloc<A>() -> *A {
        GC_malloc<*A>(sizeof(A))
    }

    pub fn allocate<A>(value: A) -> *A {
        let ptr = GC.malloc<A>();
        *ptr = value;
        ptr
    }

    pub fn calloc<A>(count: int) -> *A {
        GC_malloc(count * sizeof(A))
    }

    pub fn realloc<A>(ptr: *A, count: int) -> *A {
        GC_realloc(ptr, count)
    }

    pub fn free<A>(ptr: *A) -> unit {
        GC_free(ptr)
    }
}
