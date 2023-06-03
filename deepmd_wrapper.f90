! **************************************************************************************************
!> \brief Interface to the DeePMD-kit or a c++ wrapper.
!> \par History
!>      07.2019 created [Yongbin Zhuang]
!>      06.2023 adapt to DeePMD-kit C Interface [Yunpei Liu]
!> \author Yongbin Zhuang
! **************************************************************************************************


MODULE deepmd_wrapper

   USE ISO_C_BINDING, ONLY: C_PTR,&
                            C_CHAR,&
                            C_DOUBLE,&
                            C_INT,&
                            C_NULL_CHAR,&
                            C_LOC

   IMPLICIT NONE   
   PRIVATE
   PUBLIC :: dp_deep_pot, dp_deep_pot_compute

   INTERFACE
      FUNCTION dp_deep_pot_c(model) BIND(C, name="DP_NewDeepPot")
         USE ISO_C_BINDING
         IMPLICIT NONE
         TYPE(C_PTR)                    :: dp_deep_pot_c
         CHARACTER(LEN=1, KIND=C_CHAR)  :: model(*)
      END FUNCTION

      SUBROUTINE dp_deep_pot_compute_c(dp, vecsize, &
                   dcoord, datype, dbox, &
                   dener, dforce, dvirial, &
                   datom_ener, datom_virial) BIND(C, name="DP_DeepPotCompute")
         USE ISO_C_BINDING
         IMPLICIT NONE
         TYPE(C_PTR), INTENT(IN), VALUE :: dp
         INTEGER, INTENT(IN), VALUE     :: vecsize
         TYPE(C_PTR), INTENT(IN), VALUE :: dcoord
         TYPE(C_PTR), INTENT(IN), VALUE :: datype
         TYPE(C_PTR), INTENT(IN), VALUE :: dbox
         TYPE(C_PTR), INTENT(IN), VALUE :: dener
         TYPE(C_PTR), INTENT(IN), VALUE :: dforce
         TYPE(C_PTR), INTENT(IN), VALUE :: dvirial
         TYPE(C_PTR), INTENT(IN), VALUE :: datom_ener
         TYPE(C_PTR), INTENT(IN), VALUE :: datom_virial
      END SUBROUTINE
   END INTERFACE
     
CONTAINS

   FUNCTION dp_deep_pot(model) RESULT(pot)
      IMPLICIT NONE
      TYPE(C_PTR) :: pot
      CHARACTER(len=*), INTENT(IN), TARGET :: model
      CHARACTER(len=1, kind=C_CHAR) :: c_model(LEN_TRIM(model) + 1)
      INTEGER :: N,i 
      N = LEN_TRIM(model)
      DO i = 1, N
         c_model(i) = model(i:i)
      END DO
      c_model(N+1) = C_NULL_CHAR
      pot = dp_deep_pot_c(c_model)
   END FUNCTION dp_deep_pot

   SUBROUTINE dp_deep_pot_compute(pot, vecsize, dcoord, datype, dbox, dener, dforce, dvirial, datom_ener, datom_virial)
      IMPLICIT NONE
      TYPE(C_PTR) :: pot
      INTEGER(C_INT) :: vecsize
      REAL(C_DOUBLE), POINTER :: dcoord(:)
      INTEGER(C_INT), POINTER :: datype(:)
      REAL(C_DOUBLE), POINTER :: dbox(:)
      REAL(C_DOUBLE), POINTER :: dener
      REAL(C_DOUBLE), POINTER :: dforce(:)
      REAL(C_DOUBLE), POINTER :: dvirial(:)
      REAL(C_DOUBLE), POINTER :: datom_ener(:)
      REAL(C_DOUBLE), POINTER :: datom_virial(:)
      CALL dp_deep_pot_compute_c(pot, vecsize, C_LOC(dcoord(1)), C_LOC(datype(1)), C_LOC(dbox(1)), &
                               C_LOC(dener), C_LOC(dforce(1)), C_LOC(dvirial(1)), &
                               C_LOC(datom_ener(1)), C_LOC(datom_virial(1)))
   END SUBROUTINE dp_deep_pot_compute
END MODULE deepmd_wrapper
