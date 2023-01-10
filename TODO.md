# Things to do:
 - Fix ref (only act on lvalues, which are variables, attributes, dereferences, and array accesses)
    - Fix passing structs to functions
 - Add `array.(index)` syntax
 - Add `'(1 2 3)'` syntax for arrays
 - Add tuples and disambiguate `'(1 2 3)'` syntax between tuples and arrays (tuples fields are accessed with `tuple.0`, `tuple.1`, etc like Rust)

 - Modules (probably will pull a Zig and make them structures that contain stuff)
 - Algebraic data types
 - Typeclasses
 - Tuple generics
 - Expand macro capabilities
 - Compile time reference counting or linear types
 - Capabilities
 - Standard library
     - Steal stuff from APL
 - Multiple backends
