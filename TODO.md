# TODO
- operator overloading using typeclasses
  - operators are syntax sugar for function calls
- linear types and pointers
  - linear types are for resources not allocated values
  - pointers are not linear
  - pointers have a lifetime that is maximally the scope of the allocator
  - datatypes have lifetimes if they have pointers inside them unless they are linear, in which case their lifetime doesn't matter
  - datatypes with lifetimes can only have the contents of their pointers mutated
  - if a datatype with lifetimes needs to be mutated otherwise, then other copies of it are destroyed
  - pointers are local to threads
  - datatypes with pointers can be transferred between threads if they implement the typeclass `Send`
  - all datatypes with linear in them, regardless of whether they have pointers in them or not, implement `Send`
  - TODO: make `Send` typeclass safe somehow (this is an stdlib issue)
- strings
- external functions
- modules
- algebraic effects
- token locations for errors instead of spans

# Standard library things
- `Args x xs` datatype that lets you do varargs via syntax sugar
```ocaml
type Args x xs = Args x xs
```
  - `Args 2 (Args true "uwu")` has type `Args i32 (Args bool string)`
  - uses `func (a, b, c)` syntax sugar
  - `Args x xs` is an instance of typeclass `T` only if `x` and `xs` are both instances of `T`
