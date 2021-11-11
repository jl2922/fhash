# fhash
Fast hash map implementation in fortran

## Description
Implemention of the GCC hashmap structure in Fortran. It supports any types of keys and values, as long as you set the following macros:

* `FHASH_NAME`;

* `KEY_TYPE` and `VALUE_TYPE` with corresponding use statements `KEY_USE` and `VALUE_USE`,

and, optionally,

* `KEYS_EQUAL_FUNC`: the comparison operator for the keys (defaults to either `a == b` or `all(a == b)`, depending on whether the key is a scalar;

* `HASH_FUNC`, which takes a key and returns a hash integer. There are defaults for integers and integer arrays;

* `VALUE_POINTER`: when defined the values are assumed to be pointers.

## Benchmarks

For

* key: integer array of size 2;

* value: double precision (64-bit) floating point;

* 10M entries,

on

* Intel(R) Core(TM) i7-6820HQ CPU @ 2.70GHz;

* Ubuntu 20.04.3 LTS,

I got

|           |               | ifort 2021 | gfortran 9 |
|-----------|---------------|------------|------------|
| *insert*  | **fhash**     | 2.99       | 2.38       |
|           | **C++ (STL)** | 2.80       | 2.69       |
| *clear*   | **fhash**     | 1.24       | 0.391      |
|           | **C++ (STL)** | 0.37       | 0.328      |
