# fhash
Fast hash map implementation in fortran

## Description
Implemention of the GCC hashmap structure in Fortran. With the usage of macros, it can support any types of keys and values, as long as you implement (or the compiler provides) the corresponding equal operator(==), assignment operator(=) and the hash_value interface of the key type and the assignment operator of the value type.

## Benchmarks

Here are the benchmarks between my Fortran implementation and GCC 4.8 standard library:

For 14 integer array as the key, double precision floating point as the value, 10M entries:

Fortran hash:
Insert: 1.80 s
Clean: 1.70 s
1.59 GB

GCC unordered_map:
Insert: 2.02 s
Clean: 0.61 s
1.38 GB

For 2 integer array as the key, double precision floating point as the value, 20M entries:
Fortran hash:
Insert: 2.66 s
Clean: 2.54 s
2.57 GB

GCC
Insert: 3.60 s
Clean: 1.07 s
2.16 GB