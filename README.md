# EMSOL
EMSOL Primal simplex LP solver

EMSOL was written 1992-2013 by @jajhall, with rather ad-hoc versioning. 

`original`
---------

This is the last whole version of EMSOL source code. 
* It contains bug fixes for "EMSOL v2.16`.
* It lacks a driver program
* It won't compile with modern `f77` (specifically `gcc version 9.4.0 - Ubuntu 9.4.0-1ubuntu1~20.04`) due to some FORTRAN usage that's now illegal.

`master`
-------

This includes 
* A driver program
* Edits to allow it to be compiled with modern `f77`
* A bug fix in `dvx.f` that may have been communicated to users
* Full implementation of code to guard against "division by zero" exceptions in `dan_r_d.f`, `sed_r_s.f`, `sed_r_d.f`, `dvx_r_s.f`, `dvx_r_d.f` and `dan_r_s.f`.

As such, this represents the "best" version of EMSOL that @jajhall can come up with

Note
* To compile EMSOL may require the`-fno-range-check` flag to cope with the mechanism used in `EMSV.INC` to set the 32nd bit in a 32-bit integer
* EMSOL will only read files in `EMS` format. Instances for checking are provided
* No version of EMSOL is supported in any way
* Anyone using EMSOL is strongly advised to continue using the verion that hey have!

