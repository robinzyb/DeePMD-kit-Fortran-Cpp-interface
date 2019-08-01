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

The following text is full code for c_wrapper.cpp, the usage and comment is added in code.

```
#include <iostream>
// include DeePMD-kit function
#include "NNPInter.h"
// include header file
#include "c_wrapper.h"

//define a new data type, this struct allows us to use it a safe way in C.
struct NNP{
// obj is the pointer where we store the NNPInter object.
       void *obj;
};

// create wrapper function for constructor in NNPInter
nnp *create_nnp(char *model)
{      
       nnp *n;
       //define the NNPInter object
       NNPInter *obj;
       //dynamic allocate memory for n
       n = (typeof(n))malloc(sizeof(*n));
       //initial the NNPInter object and return it to obj.
       obj    = new NNPInter(model);
       n->obj = obj;
       cout << "this means initial successfully" << endl;
       return n;
}
// create wrapper function for destructor in NNPInter
void delete_nnp(nnp *n)
{
free(n);
       cout << "this means destruct suceessfully" << endl;
}
// create wrapper function for "compute function" in NNPInter
void compute_nnp(nnp *n,
                int *vecsize,
                double *dener,
                double *dforce,
                double *dvirial,
                double *datom_ener,
                double *datom_virial,
                double *dcoord_,
                int *datype_,
                double *dbox)
{

        NNPInter *obj;
        // check if the n is null pointer
        if (n==NULL)
                return;
        obj = static_cast<NNPInter *>(n->obj);
        //vector library can't used by C, we initial the vector here then covert to array.
        ENERGYTYPE ener = 0.0;
        int vsize = *vecsize;
        //define vector
        std::vector<double>  force_(vsize*3, 0.0);
        std::vector<double>  virial(9, 0.0);
        std::vector<double>  atom_energy_(vsize, 0.0);
        std::vector<double>  atom_virial_(vsize*9, 0.0);
        //define vector and pass the array address to vector.  
        std::vector<double>  coord_(dcoord_, dcoord_ + vsize*3);
        std::vector<double>  box(dbox, dbox + 9);
        std::vector<int> atype_(datype_, datype_ + vsize);
       cout << "define ok" << endl;
        //use compute function in NNPInter
        obj -> compute(ener,
                       force_,
                       virial,
                       atom_energy_,
                       atom_virial_,
                       coord_,
                       atype_,
                       box);
      cout << "input ok" << endl;
      // pass the local value to the adrress where the dener store value
        *dener = ener;
      cout << "energy is " << *dener << endl;
      // pass the vector value to array
        for (int i=0; i<vsize*3; i++){
                dforce[i]=force_[i];
        }
        for (int i=0; i<9; i++){
                dvirial[i]=virial[i];
        }
        for (int i=0; i<vsize; i++){
                datom_ener[i]=atom_energy_[i];
        }
        for (int i=0; i<vsize*9; i++){
                datom_virial[i]=atom_virial_[i];
        }
      cout << "this means vector function wrap successfully" << endl;
      // when exit the function, the vector and other local variables destuct automatically
}
```
#### The compilation
compile the wrapper
```
g++ -c c_wrapper.cpp -I @DEEPMD_HEADER_FILE_PATH -I @TENSORFLOW_HEADER_FILE_PATH -DHIGH_PREC
```
compile the c program
```
gcc -o call call_potential.c c_wrapper.o -lstdc++ -L @DEEPMD_LIB_FILE_PATH -L @TENSORFLOW_LIB_FILE_PATH -Wl,--no-as-needed -ldeepmd_op -ldeepmd -ltensorflow_cc -ltensorflow_framework -Wl,-rpath,@DEEPMD_LIB_FILE_PATH -Wl,-rpath,@TENSORFLOW_LIB_FILE_PATH
```
The PATH is set according to your installation path. Use the program "call" to use the deepmd through C. The "graph.pb" is a example potential(provided by Feng Wang) used by C program.


### Fortran/C Interface coding

This Interface includes file: **deepmd_wrapper.f90 fortran_call.f90**

deepmd_wrapper.f90: Fortran wrapper for c function
fortran_call.f90: Fortran test program to call C(of course, finally call deepmd)

#### The syntax of Fortran wrapper file for C Interface
Usage and comments are added in code block, please see below
```
MODULE deepmd_wrapper
! ISO_C_BINDING module in fortran is for C interface, for more detail, look for gfortran manual.
   USE ISO_C_BINDING,                   ONLY: C_PTR,&
                                              C_CHAR,&
                                              C_DOUBLE,&
                                              C_INT,&
                                              C_NULL_CHAR,&
                                              C_LOC

   IMPLICIT NONE   
   PRIVATE
   PUBLIC :: nnp, create_nnp, delete_nnp_c, compute_nnp
! This block is used to pass value and pointer to C function.
   INTERFACE
      FUNCTION create_nnp_c(model) BIND(C, name="create_nnp")
         USE ISO_C_BINDING
         IMPLICIT NONE
         TYPE(C_PTR)                    :: create_nnp_c
         CHARACTER(LEN=1, KIND=C_CHAR)         :: model(*)
      END FUNCTION
      SUBROUTINE delete_nnp_c(nnp) BIND(C, name="delete_nnp")
         USE ISO_C_BINDING
         IMPLICIT NONE
!for pass c pointer(i.e. data_type *variable in C) to C function, the VALUE attribute is syntax requirement.
         TYPE(C_PTR), INTENT(IN), VALUE   :: nnp 
      END SUBROUTINE
      SUBROUTINE compute_nnp_c(nnp, vecsize, &
                   dener, dforce, dvirial, datom_ener, &
                   datom_virial, dcoord, datype, dbox) BIND(C, name="compute_nnp")
         USE ISO_C_BINDING
         IMPLICIT NONE
         TYPE(C_PTR), INTENT(IN), VALUE :: nnp
         TYPE(C_PTR), INTENT(IN), VALUE :: vecsize
         TYPE(C_PTR), INTENT(IN), VALUE :: dener
         TYPE(C_PTR), INTENT(IN), VALUE :: dforce
         TYPE(C_PTR), INTENT(IN), VALUE :: dvirial
         TYPE(C_PTR), INTENT(IN), VALUE :: datom_ener
         TYPE(C_PTR), INTENT(IN), VALUE :: datom_virial
         TYPE(C_PTR), INTENT(IN), VALUE :: dcoord
         TYPE(C_PTR), INTENT(IN), VALUE :: datype
         TYPE(C_PTR), INTENT(IN), VALUE :: dbox
      END SUBROUTINE
   END INTERFACE
     TYPE nnp
             TYPE(C_PTR) :: ptr
     END TYPE
     
CONTAINS

   FUNCTION create_nnp(model)
      IMPLICIT NONE
      TYPE(nnp) :: create_nnp
      CHARACTER(len=*), INTENT(IN), TARGET :: model
      CHARACTER(len=1, kind=C_CHAR) :: c_model(LEN_TRIM(model) + 1)
      INTEGER   :: N,i 
!Fortran string is not terminated by null character while C and C++ is. Therefore we need to add the null character to fortran string at last.
             N = LEN_TRIM(model)
             DO i = 1, N
                c_model(i) = model(i:i)
             END DO
             c_model(N+1) = C_NULL_CHAR
             create_nnp%ptr = create_nnp_c(c_model)
   END FUNCTION

   SUBROUTINE compute_nnp(pot, vecsize, dener, dforce, dvirial, datom_ener, datom_virial, dcoord, datype, dbox)
      IMPLICIT NONE
      TYPE(C_PTR) :: pot
      INTEGER(C_INT), TARGET  :: vecsize
      REAL(C_DOUBLE), POINTER :: dener
      REAL(C_DOUBLE), POINTER :: dforce(:)
      REAL(C_DOUBLE), POINTER :: dvirial(:)
      REAL(C_DOUBLE), POINTER :: datom_ener(:)
      REAL(C_DOUBLE), POINTER :: datom_virial(:)
      REAL(C_DOUBLE), POINTER :: dcoord(:)
      INTEGER(C_INT), POINTER :: datype(:)
      REAL(C_DOUBLE), POINTER :: dbox(:)
! C_LOC() is the function that extracts the C address of Fortran Pointer. For array pointer, just extracts the first element address and pass to C array.
      CALL compute_nnp_c(pot, C_LOC(vecsize), C_LOC(dener), C_LOC(dforce(1)), &
                               C_LOC(dvirial(1)), C_LOC(datom_ener(1)), C_LOC(datom_virial(1)),&
                               C_LOC(dcoord(1)), C_LOC(datype(1)), C_LOC(dbox(1)))
   END SUBROUTINE
END MODULE deepmd_wrapper
```
