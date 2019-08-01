! **************************************************************************************************
!> \brief Interface to the DeePMD-kit or a c++ wrapper.
!> \par History
!>      07.2019 created [Yongbin Zhuang]
!> \author Yongbin Zhuang
! **************************************************************************************************


MODULE deepmd_wrapper

   USE ISO_C_BINDING,                   ONLY: C_PTR,&
                                              C_CHAR,&
                                              C_DOUBLE,&
                                              C_INT,&
                                              C_NULL_CHAR,&
                                              C_LOC

   IMPLICIT NONE   
   PRIVATE
   PUBLIC :: nnp, create_nnp, delete_nnp_c, compute_nnp

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
      CALL compute_nnp_c(pot, C_LOC(vecsize), C_LOC(dener), C_LOC(dforce(1)), &
                               C_LOC(dvirial(1)), C_LOC(datom_ener(1)), C_LOC(datom_virial(1)),&
                               C_LOC(dcoord(1)), C_LOC(datype(1)), C_LOC(dbox(1)))
   END SUBROUTINE
END MODULE deepmd_wrapper
