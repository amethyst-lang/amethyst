# amethyst
Amethyst is a systems language aimed at being simple, small, portable, and safe.

## STOP MAKING LANGUAGES!!!!
Nope. Never will. I love making languages that are unfinished and you can't stop me.

## Hhhh fine, so what is this language?
From the r/ProgLangs discord server:
 - simple: implementing this language should be super simple. the design of this language should be super simple. it should be super simple to learn this language if youre familiar with other system langs
 - small: the base implementation should be small and with few external dependencies. the stdlib should be small but expandable
 - portable: nothing in the language should be specific to one architecture. arch specific external libs developed for the language are allowed, but if theres a backend for it, any code written in amethyst should work on it. this also means no inline assembly because *dear god how the heck do you port that*
 - safe: ~~damn how did you know i love rust~~ code should be as safe as possible without sacrificing any of the above points. whilst safety is a goal of amethyst, it is not the main goal (simplicity is)

## Cool, what does it look like?
Let's look at an example:
```
// a vector/list/whatever you wanna call it
struct Vec['a, 'A] { clone; debug } {
    // default value
    usize len = 0;

    // @ is a fat pointer, mut indicates its a fat pointer to mutable data
    // as opposed to *'a vals, which is a const pointer
    @mut 'a vals = null {
        // this readonly modifier makes it that external namespaces can only read it but not write. the current namespace can still write to it
        // you can also do something like
        // 'a name { public }
        // for a field that can be globally read and written to (default is only the current module can read/write to)
        // and
        // 'a name { writeonly }
        // for a field that can only be written to globally
        readonly
    }

    // the allocator has to be stored with the struct
    // structs are embedded in other structs
    'A alloc
}

Vec['a, 'A] alloc('A alloc) {
    return Vec['a, 'A] {
        alloc = alloc
    }
}

// everything is passed by reference except for structs annotated with copy and numbers of width less than usize that are not marked as mutable
void push(mut Vec['a, 'A] v, 'a item) {
    if v.len >= v.vals.len {
        let usize cap = if v.vals.cap == 0 {
            8
        } else v.vals.cap * 2;

        v.vals = v.alloc.array_realloc['a](v.vals, v.cap) {
            catch panic("allocation failed")
        }
    }

    v.vals.set(v.len, item);
    v.len += 1
}

// enums are error values
enum opt {
    none
}

// you can attach an error value to your function by appending the return type with !(enum name)
void!opt get(Vec['a, 'A] v, usize i) {
    i < v.len {
        // and return the error value using else (or return)
        else !none
    }
    return v.vals.get(i)
}

void assert_eq('a a, 'a b) {
    a != b {
        // else can also be used for other error related things on a false condition
        else panic("assertion failed: {} != {}", (a, b))
    }
}

// easy tests!
test access {
    // number types are not restricted to powers of 2
    mut Vec[u42, _] v = global_allocator().alloc();
    v.push(234);
    v.push(765);
    v.push(198);
    assert_eq(
        v.get(1) {
            catch panic("out of bounds")
        },
        765
    )
}

// lets pretend we have a map function on our vector
// then we can do something like this
void main() {
    mut Vec[u32, _] v = global_allocator().alloc();

    // nice inline function syntax :D
    // note this is not a closure (thats too complicated to implement properly)
    // instead it takes in a reference to data
    // in this case its void because it doesn't need anything extra
    v.map(void) {
        |u32 val, void _| val + 1
    };
}

// also externs and stuff
extern C {
    i32 puts(*u8 str)
}
```
