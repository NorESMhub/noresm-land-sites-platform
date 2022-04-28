!=======================================================================
!BOP
!
! !MODULE: glc_kinds_mod - defines variable precision
!
! !DESCRIPTION:
!
! Defines variable precision for all common data types
!
! !REVISION HISTORY:
!  Adapted by William Lipscomb from ice_kinds_mod.F90 in CICE
!
! !INTERFACE:
!
      module glc_kinds_mod
!
! !USES:
        use shr_kind_mod, only : SHR_KIND_CS, SHR_KIND_CL
!
!EOP
!=======================================================================

      implicit none
      save

      integer, parameter :: i4        = selected_int_kind(6), &
                            i8        = selected_int_kind(13), &
                            r4        = selected_real_kind(6), &
                            r8        = selected_real_kind(13), &
                            r16       = selected_real_kind(20)

      integer, parameter :: char_len  = SHR_KIND_CS, &
                            char_len_long  = SHR_KIND_CL, &
                            int_kind  = kind(1), &
                            log_kind  = kind(.true.), &
                            real_kind = selected_real_kind(6), &
                            dbl_kind  = selected_real_kind(13), &
                            quad_kind = selected_real_kind(20)

!=======================================================================

      end module glc_kinds_mod

!=======================================================================
