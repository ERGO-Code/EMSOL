# EMSOL
EMSOL Primal simplex LP solver

EMSOL was written 1992-2013 by @jajhall, with rather ad-hoc versioning. 

`original`
---------

This is the last whole version of EMSOL source code. 
* It contains bug fixes for "EMSOL v2.16".
* It lacks a driver program
* It won't compile with modern `f77` (specifically `gcc version 9.4.0 - Ubuntu 9.4.0-1ubuntu1~20.04`) due to some FORTRAN usage that's now illegal.

Specifically, the subroutines `ems_cp_rl_a` and `ems_cp_i_a` were written to operate in different modes: copying the values in a real (or integer) array; copying in a reversed loop; setting array entries equal to their index; setting all array entries to a fixed value. The second parameter (the origin of the values) was dimensioned in the subroutine as an arrray, but sometimes a scalar value was passed in when setting all array entries to a fixed value.

`master`
-------

This includes 
* A driver program
* Edits to allow it to be compiled with modern `f77`. Specifically the abuse of FORTRAN (above) has been side-stepped by creating special versions of `ems_cp_rl_a` and `ems_cp_i_a` for the cases where whole arrays are copied, and the original subroutines modified to deal with the case where an array is initialised to a scalar value.
* A bug fix in `dvx.f` that may have been communicated to users
* Full implementation of code to guard against "division by zero" exceptions in `dan_r_d.f`, `sed_r_s.f`, `sed_r_d.f`, `dvx_r_s.f`, `dvx_r_d.f` and `dan_r_s.f`.

As such, this represents the "best" version of EMSOL that @jajhall can come up with

Note
* To compile EMSOL may require the`-fno-range-check` flag to cope with the mechanism used in `EMSV.INC` to set the 32nd bit in a 32-bit integer
* EMSOL will only read files in `EMS` format. Instances for checking are provided
* No version of EMSOL is supported in any way
* Anyone using EMSOL is strongly advised to continue using the verion that hey have!

