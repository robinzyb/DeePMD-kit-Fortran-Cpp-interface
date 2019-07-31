# DeePMD-kit-Fortran-Cpp-interface

## A Test Code for constructing Fortran/C++ Interface of DeePMD-kit

### Overall
Fortran doesn't provide direct C++ interface but C interface through ISO_C_BINDING module.
Therefore, the C/C++ interface is needed when being connected to Fortran.

Note: using this code when installing the DeePMD-kit successfully

### C/C++ Interface coding
This Interface includes file: **c_wrapper.h c_wrapper.cpp call_potential.c**

c_wrapper.h: header file for **exposing** wrapper function to C. (i.e. can be called by C)

c_wrapper.cpp: C++ code for **wrapping** C++ Function. 

call_potential.c: a simple C program for testing C/C++ Interface.

#### The syntax of header file for Interface
##### Basic stucture
```
#ifdef __cplusplus
extern "C" {
#endif

<user writing function called by C>

#ifdef __cplusplus
}
#endif
```
