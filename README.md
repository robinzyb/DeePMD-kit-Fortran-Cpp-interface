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

```
#include <iostream>
#include "NNPInter.h"
#include "c_wrapper.h"

struct NNP{
       void *obj;
};

nnp *create_nnp(char *model)
{      
       nnp *n;
       NNPInter *obj;
       n = (typeof(n))malloc(sizeof(*n));
       obj    = new NNPInter(model);
       n->obj = obj;
       cout << "this means initial successfully" << endl;
       return n;
}
void delete_nnp(nnp *n)
{
free(n);
       cout << "this means destruct suceessfully" << endl;
}
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
        if (n==NULL)
                return;
        obj = static_cast<NNPInter *>(n->obj);
        //covert array to vector
        ENERGYTYPE ener = 0.0;
        int vsize = *vecsize;
        std::vector<double>  force_(vsize*3, 0.0);
        std::vector<double>  virial(9, 0.0);
        std::vector<double>  atom_energy_(vsize, 0.0);
        std::vector<double>  atom_virial_(vsize*9, 0.0);
        std::vector<double>  coord_(dcoord_, dcoord_ + vsize*3);
        std::vector<double>  box(dbox, dbox + 9);
        std::vector<int> atype_(datype_, datype_ + vsize);
       cout << "define ok" << endl;

        obj -> compute(ener,
                       force_,
                       virial,
                       atom_energy_,
                       atom_virial_,
                       coord_,
                       atype_,
                       box);
      cout << "input ok" << endl;
        *dener = ener;
      cout << "energy is " << *dener << endl;
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
}
```

