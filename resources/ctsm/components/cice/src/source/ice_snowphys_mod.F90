

! Modifications of amount and physical properties of snow on ice.

! author: Jens Boldingh Debernard, MET-Norway


  module ice_snowphys

    use ice_kinds_mod
    use ice_blocks, only: nx_block, ny_block
    use ice_domain_size, only: ncat
    use ice_constants
    use ice_domain_size, only: max_blocks
    
    implicit none
    save
    
    private
    public :: snowphys_snowfonice

    character (len=char_len), public :: &
         blowingsnow              ! pond refreezing parameterization


    real (kind=dbl_kind), dimension (nx_block,ny_block,ncat,max_blocks), public :: &
         snowfonicen,      & ! Fraction of snow kept on ice during snowfall
         snow2ocnn           ! Mass fluks of snow blowing indirectly into ocean

    real (kind=dbl_kind), dimension (nx_block,ny_block,max_blocks), public :: &
         snowfonice,      & ! Fraction of snow kept on ice during snowfall (agregate)
         snow2ocn         ! Mass fluks of snow blowing indirectly into ocean (agreate)

  contains

    subroutine snowphys_snowfonice(icells, indxi, indxj, snwfonice,ai)
      integer (kind=int_kind), intent(in) :: &
         icells              ! number of cells with aicen > puny

      integer (kind=int_kind), dimension(nx_block*ny_block), &
         intent(in) :: &
         indxi, indxj    ! compressed indices for cells with aicen > puny


      real (kind=dbl_kind), dimension (nx_block,ny_block),intent(in) :: ai
      real (kind=dbl_kind), dimension (nx_block,ny_block),intent(inout) :: &
           snwfonice

      integer (kind=int_kind) :: ij, i,j

      do ij=1,icells
         i=indxi(ij)
         j=indxj(ij)

!jd            snwfonice(i,j)=0.9
!         if (aice(i,j,iblk) > p01 ) &
         snwfonice(i,j)=(c1 - max(c0,c1-ai(i,j))**p6 )/ai(i,j)
      enddo

    end subroutine snowphys_snowfonice

  end module ice_snowphys
