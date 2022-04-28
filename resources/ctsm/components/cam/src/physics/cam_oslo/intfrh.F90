
subroutine intfrh (lchnk, ncol, v3so4, v3insol, v3oc, v3ss, relh, frh)

!   Written by Alf Kirkevaag in November 2011, based on interpol1to3 in optinterpol.F90

   use ppgrid
   use shr_kind_mod, only: r8 => shr_kind_r8
!o   use opttab,       only: nbmodes, rh
   use opttab,       only: rh
!   use aerosoldef, only: nmodes
   use commondefinitions, only: nmodes

   implicit none

! Relative humidity intries from opttab.F90:  
!!                                  rh = (/ 0.0_r8,  0.37_r8, 0.47_r8, 0.65_r8, 0.75_r8, &
!!                                          0.8_r8,  0.85_r8, 0.9_r8,  0.95_r8, 0.995_r8 /)
! Humidity growth factors which are consistent with the aerosol optics look-up tables:
   real(r8), dimension(10) :: fh_SO4   = (/ 1.00_r8, 1.34_r8, 1.40_r8, 1.53_r8, 1.64_r8, &
                                            1.71_r8, 1.81_r8, 1.98_r8, 2.39_r8, 5.04_r8 /)
   real(r8), dimension(10) :: fh_insol = (/ 1.00_r8, 1.01_r8, 1.01_r8, 1.02_r8, 1.02_r8, &
                                            1.02_r8, 1.02_r8, 1.02_r8, 1.02_r8, 1.02_r8 /)
   real(r8), dimension(10) :: fh_OC    = (/ 1.00_r8, 1.02_r8, 1.05_r8, 1.14_r8, 1.19_r8, &
                                            1.22_r8, 1.27_r8, 1.36_r8, 1.59_r8, 3.18_r8 /)
   real(r8), dimension(10) :: fh_SS    = (/ 1.00_r8, 1.01_r8, 1.02_r8, 1.56_r8, 1.87_r8, &
                                            1.97_r8, 2.12_r8, 2.35_r8, 2.88_r8, 6.08_r8 /)
!
! Input arguments
!
   integer, intent(in)  :: lchnk                      ! chunk identifier
   integer, intent(in)  :: ncol                       ! number of atmospheric columns
!o   real(r8), intent(in) :: v3so4(pcols,pver,nbmodes)  ! Modal mass fraction of Sulfate
!o   real(r8), intent(in) :: v3insol(pcols,pver,nbmodes)! Modal mass fraction of BC and dust
!o   real(r8), intent(in) :: v3oc(pcols,pver,nbmodes)   ! Modal mass fraction of OC (POM)
!o   real(r8), intent(in) :: v3ss(pcols,pver,nbmodes)   ! Modal mass fraction of sea-salt
   real(r8), intent(in) :: v3so4(pcols,pver,nmodes)  ! Modal mass fraction of Sulfate
   real(r8), intent(in) :: v3insol(pcols,pver,nmodes)! Modal mass fraction of BC and dust
   real(r8), intent(in) :: v3oc(pcols,pver,nmodes)   ! Modal mass fraction of OC (POM)
   real(r8), intent(in) :: v3ss(pcols,pver,nmodes)   ! Modal mass fraction of sea-salt
   real(r8), intent(in) :: relh(pcols,pver)           ! Ambient relatve humidity (fraction)
!
! Output arguments
!
!o   real(r8), intent(out) :: frh(pcols,pver,nbmodes)    ! Modal humidity growth factor 
   real(r8), intent(out) :: frh(pcols,pver,nmodes)    ! Modal humidity growth factor 

!
!---------------------------Local variables-----------------------------
!
      integer i, ierr, irelh, kcomp, k, icol
      integer irh1(pcols,pver), irh2(pcols,pver)
      real(r8) a, b, e, fso4, finsol, foc, fss
      real(r8) xrh(pcols,pver)
      parameter (e=2.718281828)

!     Temporary storage of often used array elements
      integer t_irh1, t_irh2
      real(r8) t_xrh, t_rh1, t_rh2

!      write(*,*) 'Before xrh-loop'
      do k=1,pver
        do icol=1,ncol
!test          xrh(icol,k)  = 0.8
          xrh(icol,k)  = min(max(relh(icol,k),rh(1)),rh(10))
        end do 
      end do

!      write(*,*) 'Before rh-loop'
      do irelh=1,9
       do k=1,pver
        do icol=1,ncol
           if(xrh(icol,k) >= rh(irelh).and. &
             xrh(icol,k)<=rh(irelh+1)) then
             irh1(icol,k)=irelh
             irh2(icol,k)=irelh+1
           endif
         end do
       end do
      end do

!o     Loop over all relevant background modes (kcomp=1,2,4-10)
!o      do kcomp=1,10
!     Loop over all relevant modes (kcomp=1,2,4-11,13,14)
!     (mode 3 is no longer included, and 12 is insoluble)

      do kcomp=1,14

        do icol=1,ncol
          do k=1,pver
            frh(icol,k,kcomp)=0.0_r8
          end do
        end do

!o        if(kcomp.ne.3) then
        if(kcomp.ne.3.and.kcomp.ne.12) then
         
        do k=1,pver
          do icol=1,ncol

!      Collect all the vector elements into temporary storage
!      to avoid cache conflicts and excessive cross-referencing

      t_irh1 = irh1(icol,k)
      t_irh2 = irh2(icol,k)

!      write(*,*) 't_irh1,t_irh2=',t_irh1,t_irh2

      t_rh1  = rh(t_irh1)
      t_rh2  = rh(t_irh2)

      t_xrh  = xrh(icol,k)

!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc
    if(t_xrh <= 0.37) then  !    linear averaging w.r.t. small RH:
      fso4  = ((t_rh2-t_xrh)*fh_SO4(t_irh1)+(t_xrh-t_rh1)*fh_SO4(t_irh2)) &
                          /(t_rh2-t_rh1)    
      finsol= ((t_rh2-t_xrh)*fh_insol(t_irh1)+(t_xrh-t_rh1)*fh_insol(t_irh2)) &
                          /(t_rh2-t_rh1)    
      foc   = ((t_rh2-t_xrh)*fh_OC(t_irh1)+(t_xrh-t_rh1)*fh_OC(t_irh2)) &
                          /(t_rh2-t_rh1)    
      fss   = ((t_rh2-t_xrh)*fh_SS(t_irh1)+(t_xrh-t_rh1)*fh_SS(t_irh2)) &
                          /(t_rh2-t_rh1)    
      else                  !    exponential averaging w.r.t. large RH:   
        a=(log(fh_SO4(t_irh2))-log(fh_SO4(t_irh1)))/(t_rh2-t_rh1)
        b=(t_rh2*log(fh_SO4(t_irh1))-t_rh1*log(fh_SO4(t_irh2)))/(t_rh2-t_rh1)
        fso4=e**(a*t_xrh+b)
        a=(log(fh_insol(t_irh2))-log(fh_insol(t_irh1)))/(t_rh2-t_rh1)
        b=(t_rh2*log(fh_insol(t_irh1))-t_rh1*log(fh_insol(t_irh2)))/(t_rh2-t_rh1)
        finsol=e**(a*t_xrh+b)
        a=(log(fh_OC(t_irh2))-log(fh_OC(t_irh1)))/(t_rh2-t_rh1)
        b=(t_rh2*log(fh_OC(t_irh1))-t_rh1*log(fh_OC(t_irh2)))/(t_rh2-t_rh1)
        foc=e**(a*t_xrh+b)
        a=(log(fh_SS(t_irh2))-log(fh_SS(t_irh1)))/(t_rh2-t_rh1)
        b=(t_rh2*log(fh_SS(t_irh1))-t_rh1*log(fh_SS(t_irh2)))/(t_rh2-t_rh1)
        fss=e**(a*t_xrh+b)
    endif
!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc

!  linear interpolation w.r.t. mass fractions of each internally mixed component
!  (this assumption is only used here, while the full Koehler equation are solved
!  for the look-up tables for log-normal size distributions and aerosol optics):

      frh(icol,k,kcomp) = v3so4(icol,k,kcomp)*fso4+v3insol(icol,k,kcomp)*finsol &
                        + v3oc(icol,k,kcomp) *foc +v3ss(icol,k,kcomp)   *fss

!      write(*,*) 'frh =', frh(icol,k,kcomp)

          end do ! icol
        end do   ! k

        endif    ! kcomp.ne.3.and.kcomp.ne.12

      end do   ! kcomp

      return
end subroutine intfrh
