# Within

`Within` is a type for simply scoping well-typed paths. A `Within a t` is
just a `Path Rel t` inside a `Path a Dir`. This is useful for when you want
to keep track of a filepath within a parent folder and need the extra degree
of articulation.

Early release, subject to change.
