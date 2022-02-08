# amethyst
Amethyst is a systems language aimed at being simple, small, portable, and safe.

## STOP MAKING LANGUAGES!!!!
Nope. Never will. I love making languages that are unfinished and you can't stop me.

## Hhhh fine, so what is this language?
From the r/ProgLangs discord server:
 - simple: implementing this language should be super simple. the design of this language should be super simple. it should be super simple to learn this language if youre familiar with other system langs
 - small: the base implementation should be small and with few external dependencies. the stdlib should be small but expandable, with no external dependencies
 - portable: nothing in the language should be specific to one architecture. arch specific external libs developed for the language are allowed, but if theres a backend for it, any code written in amethyst should work on it. this also means no inline assembly because *dear god how the heck do you port that*
 - safe: ~~damn how did you know i love rust~~ code should be as safe as possible without sacrificing any of the above points. whilst safety is a goal of amethyst, it is not the main goal (simplicity is)

## Cool, what does it look like?
Let's look at an example:
```
// a vector/list/whatever you wanna call it
(defstruct (Vec 'a 'A)
    // default value
    // this readonly modifier makes it that external namespaces can only read it but not write. the current namespace can still write to it
    // you can also do something like
    // 'a name { public }
    // for a field that can be globally read and written to (default is only the current module can read/write to)
    // and
    // 'a name { writeonly }
    // for a field that can only be written to globally
    ((len readonly) usize 0)

    // @ is a fat pointer, mut indicates its a fat pointer to mutable data
    // as opposed to *'a vals, which is a const pointer
    (vals (@ mut 'a) null)

    // the allocator has to be stored with the struct
    // structs are embedded in other structs
    (alloc 'A))


(defunc alloc (alloc 'A)
    (inst (Vec 'a 'A)
        :alloc alloc))

void push(mut Vec['a, 'A] v, 'a item) {
    if v.len >= v.vals.len {
        let usize cap = if v.vals.cap == 0 {
            8
        } else v.vals.cap * 2;

        v.vals = v.alloc.array_realloc['a](v.vals, v.cap)
            catch panic("allocation failed")
    }

    v.vals.set(v.len, item);
    v.len += 1
}

// enums are error values
(enum opt
    // you can attach a numerical value to the enum variant by doing
    // (variant-name value)
    // otherwise you can just specify a name for a number to be generated automatically
    none)

// you can attach an error value to your function by appending the return type with !(enum name)
(defunc (get ('a ! opt)) (v (Vec 'a 'A)) (i usize)
    (cond
        (< i v.len) (v.vals.get i)
        else !none))

(defunc assert_eq (a 'a) (b 'a)
    (if (!= a b)
        (panic "assertion failed: {} != {}" '(a b))))

// easy tests!
(test access
    (seq
        (mut (v (Vec u42 _)) (alloc global_allocator))
        (v.push 234)
        (v.push 765)
        (v.push 198)
        (assert_eq
            (try (v.get 1)
                catch (panic "out of bounds"))
            765)))

(defunc main
    (seq
        (mut (v (Vec u32 _)) (alloc global_allocator))
        (v.map (defunc (_ u32) (val u32)
            (+ val 1)))))

// also externs and stuff
(extern C
    defunc (puts i32) (str (* u8)))
(extern C
    let (errno i32))

// macros
(defmacro while (con stats)
    (loop
        (cond
            con stats
            else break)))
(defmacro while (con stats :else elsy)
    (loop
        (cond
            con stats
            else break elsy)))

(defunc other-things
    (seq
        (loop
            (break 2))

        (while (some condition)
            (break "uwu"))))
```

## Features
 - compile time reference counting (akin to lobster)
     - function argument = increment reference counter in current function and decrement if not returned
     - out of scope = decrement reference counter
     - variable assignment = increment reference counter
     - variable no longer used/out of scope = decrement reference counter
     - reference counter reaches 0 = free
 - RAII
