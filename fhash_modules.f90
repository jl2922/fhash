#define KEY_TYPE type(integer), allocatable
#define VALUE_TYPE real
#define VALUE_USE use, intrinsic :: iso_fortran_env
#define SHORTNAME ints_real
#include "fhash.f90"
#undef KEY_TYPE
#undef VALUE_TYPE
#undef SHORTNAME
#undef VALUE_USE