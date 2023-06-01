# DeePMD-kit-Fortran-Cpp-interface

## A Test Code for constructing Fortran/C++ Interface of DeePMD-kit

### Overall
Fortran doesn't provide direct C++ interface but C interface through ISO_C_BINDING module.
Therefore, the C/C++ interface is needed when being connected to Fortran.

First, user should download libdeepmd_c from DeePMD-kit [release page](https://github.com/deepmodeling/deepmd-kit/releases), and uncompress it into some place. **In the following, the path of `libdeepmd_c` is noted as `<libdeepmd_c>`.**

```bash
cd /some/place
wget https://github.com/deepmodeling/deepmd-kit/releases/download/v2.2.2/libdeepmd_c.tar.gz
tar xf libdeepmd_c.tar.gz
```

### C Interface
This Interface includes file: `call_potential.c` 

call_potential.c: a simple C program for testing C/C++ Interface.

#### The compilation

Compile the c program
```
gcc -o call call_potential.c -I <libdeepmd_c>/include -lstdc++ -L <libdeepmd_c>/lib -Wl,--no-as-needed -ldeepmd_op -ldeepmd_c -Wl,-rpath,<libdeepmd_c>/lib
```
The PATH is set according to your installation path. Use the program "call" to call the deepmd through C. The `graph.pb` is [DPA-1 OC20M potential](https://www.aissquare.com/models/detail?pageType=models&name=DPA_1_OC2M) used by C program.


### Fortran/C Interface coding

This Interface includes file: `deepmd_wrapper.f90` `fortran_call.f90`

`deepmd_wrapper.f90`: Fortran wrapper for c function

`fortran_call.f90`: Fortran test program to call C(of course, finally call deepmd)

#### The compilation
Compile the fortran test program\

```
gfortran -o fortran_call deepmd_wrapper.f90 fortran_call.f90 -I <libdeepmd_c>/include -lstdc++ -L <libdeepmd_c>/lib -ldeepmd_op -ldeepmd_c -Wl,-rpath,<libdeepmd_c>/lib
```
