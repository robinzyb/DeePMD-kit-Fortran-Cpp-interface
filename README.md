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
The PATH is set according to your installation path.



