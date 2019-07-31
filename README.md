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

```
#ifdef __cplusplus
extern "C" {
#endif

<This field is for declaring functions called by C>

#ifdef __cplusplus
}
#endif
```
The most important part here is `extern "C"` which ensures the declared function is visible by C program.
Function declared here should use basic data type called by C (e.g. Int, Double, Pointer..)

#### The syntax of cpp wrapper file for Interface

The motivation of wrapping C++ function to C function is to avoid directly using libraries or functions which C don't have.
For example, the vector library, the constructor and destructor functions in C++, cannot be used by C.



