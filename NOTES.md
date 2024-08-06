# NOTES

## Design priorities

- Compliance: the bindings must reflect the specifications as closely as possible.
- Performance: the should be no overhead or minimal overhead in the generated code.
  - Minimize conversions
- Safetey: the  bindings should provide type-safety and reduce the number of unchecked conversions.
  - Dedicated types
- Ergonomics: the bindings must be easy to use and follow consistent/predictable conventions.

This means that:
- The low-level API of the bindings closely follows JavaScript/DOM conventions.
- The low-level APi of the bindings might be more verbose.
- Recursive modules generate `update_module` calls and seem to have a runtime overhead.
