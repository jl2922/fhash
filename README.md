# fhash
Fast hash map implementation in fortran

## Description
Implemention of the GCC hashmap structure in Fortran. It supports any types of keys and values, as long as you set the following macros:

* `KEY_TYPE` and `VALUE_TYPE` with corresponding use statements `KEY_USE` and `VALUE_USE`,

and, optionally,

* `KEYS_EQUAL_FUNC`: the comparison operator for the keys (defaults to either `a == b` or `all(a == b)`, depending on whether the key is a scalar;

* `HASH_FUNC`, which takes a key and returns a hash integer. There are defaults for integers and integer arrays;

* `VALUE_POINTER`: when defined the values are assumed to be pointers.

## Benchmarks

Here are the benchmarks between my Fortran implementation and GCC 4.8 standard library:

For 14 integer array as the key, double precision floating point as the value, 10M entries:

Fortran hash:

> Insert: 1.80 s
>
> Clean: 1.70 s
>
> 1.59 GB

GCC unordered_map:

> Insert: 2.02 s
> 
> Clean: 0.61 s
> 
> 1.38 GB

For 2 integer array as the key, double precision floating point as the value, 20M entries:

Fortran hash:

> Insert: 2.66 s
> 
> Clean: 2.54 s
> 
> 2.57 GB

GCC unordered_map:

> Insert: 3.60 s
> 
> Clean: 1.07 s
> 
> 2.16 GB