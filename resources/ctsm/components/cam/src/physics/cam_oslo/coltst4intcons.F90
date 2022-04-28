
subroutine coltst4intcons (lchnk, ncol, qm1, deltah_km, rhoda, fnbc, &
            dload_mi, dload_ss, dload_s4, dload_oc, dload_bc, &
            dload_bc_0, dload_bc_2, dload_bc_4, dload_bc_12, dload_bc_14, dload_bc_ac, &
            dload_oc_4, dload_oc_14, dload_oc_ac, dload_s4_a, dload_s4_1, dload_s4_5)

!      Testing column burdens for internal consistency between intdrypar 
!      (use of aerodryk*.out look-up tables) and calculations directly 
!      from the qm1 array. Made by Alf Kirkevag 8/12-2015.

!      Due to a problem with initialization of some values (seemingly), 
!      the output variables COLR* (column burden ratio for tracers *)
!      should not be checked for the first output file from an initial run.
!      Initial test results after coorecting a bug in AeroTab October 2016:
!      Results from month 5 in a test simulation with 2000 aerosol emissions
!      and f10_f10 resolution (10x15_10x15) gave the following globally 
!      averaged COLR* values:
!
! COLRBC0 = 1.000015 ;
! COLRBC12 = 0.9991855 ;
! COLRBC14 = 0.9992678 ;
! COLRBC2 = 0.9991855 ;
! COLRBC4 = 0.9997123 ;
! COLRBCAC = 1.000379 ;
! COLROC14 = 0.9989312 ;
! COLROC4 = 0.9995964 ;
! COLROCAC = 0.9993698 ;
! COLRSUL1 = 1.034586 ;
! COLRSUL5 = 1.03905 ;
! COLRSULA = 1.000236 ;
!
! with regional variations within 0.01 for all tracers except for the 
! externally mixed tracers so4_na (COLRSUL1 = 1.02 - 1.04) and so4_pr 
! (COLRSUL5 = 1.035 - 1.039). The biases for COLRSUL1 and COLRSUL5 are
! consistent with a ratio between mass density for sulfuric acid and 
! ammonium sulfate (1841/1769=1.041), and that CAM5-Olso does not take 
! into account the former. 

   use ppgrid
   use shr_kind_mod, only: r8 => shr_kind_r8
   use constituents, only: pcnst
   use aerosoldef
   use cam_history,  only: outfld

   implicit none

!
! Input arguments
!
   integer,  intent(in) :: lchnk                  ! chunk identifier
   integer,  intent(in) :: ncol                   ! number of atmospheric columns
   real(r8), intent(in) :: qm1(pcols,pver,pcnst)  ! Specific humidity and tracers (kg/kg)
   real(r8), intent(in) :: deltah_km(pcols,pver)  ! Layer thickness, unit km
   real(r8), intent(in) :: rhoda(pcols,pver)
   real(r8), intent(in) :: fnbc(pcols,pver)
   real(r8), intent(in) :: dload_mi(pcols)
   real(r8), intent(in) :: dload_ss(pcols)
   real(r8), intent(in) :: dload_s4(pcols)
   real(r8), intent(in) :: dload_oc(pcols)
   real(r8), intent(in) :: dload_bc(pcols)
   real(r8), intent(in) :: dload_bc_0(pcols)
   real(r8), intent(in) :: dload_bc_2(pcols)
   real(r8), intent(in) :: dload_bc_4(pcols)
   real(r8), intent(in) :: dload_bc_12(pcols)
   real(r8), intent(in) :: dload_bc_14(pcols)
   real(r8), intent(in) :: dload_bc_ac(pcols)
   real(r8), intent(in) :: dload_oc_4(pcols)
   real(r8), intent(in) :: dload_oc_14(pcols)
   real(r8), intent(in) :: dload_oc_ac(pcols)
   real(r8), intent(in) :: dload_s4_a(pcols)
   real(r8), intent(in) :: dload_s4_1(pcols)
   real(r8), intent(in) :: dload_s4_5(pcols)
!
!---------------------------Local variables-----------------------------
!
   integer  icol, k
   real(r8) columnb(pcols), colratio(pcols)
!  strict test, only expected to apply for some externally mixed modes:
!   real(r8), parameter :: oneplus  = 1.003_r8
!   real(r8), parameter :: oneminus = 0.997_r8
!  less strict test, expected to apply for externally mixed modes, except
!  in the first time-steps, seemingly due to problem with initialization:
   real(r8), parameter :: oneplus  = 1.05_r8
   real(r8), parameter :: oneminus = 0.95_r8
!
!
!---------------------------Test calculations---------------------------

!BC:

      do icol=1,ncol
        columnb(icol) = 0.0_r8
        colratio(icol) = 0.0_r8
      end do
      do icol=1,ncol
        do k=1,pver
         columnb(icol) = columnb(icol)+deltah_km(icol,k) &
                       * 1.e9*qm1(icol,k,l_bc_ax)*rhoda(icol,k)
         colratio(icol) = dload_bc_0(icol)/columnb(icol)
        end do
!        if(colratio(icol).lt.oneminus.or.colratio(icol).gt.oneplus) then
!         write(99,*) 'my bc 0 ratio =', icol, colratio(icol)
!        endif
      end do

      call outfld('COLRBC0 ', colratio, pcols,lchnk)

      do icol=1,ncol
        columnb(icol) = 0.0_r8
        colratio(icol) = 0.0_r8
      end do
      do icol=1,ncol
        do k=1,pver
          columnb(icol) = columnb(icol)+deltah_km(icol,k) &
                        * 1.e9*qm1(icol,k,l_bc_a)*rhoda(icol,k)
          colratio(icol) = dload_bc_2(icol)/columnb(icol)
        end do
!        if(colratio(icol).lt.oneminus.or.colratio(icol).gt.oneplus) then
!          write(99,*) 'my bc 2 ratio =', icol, colratio(icol) 
!        endif
      end do

      call outfld('COLRBC2 ', colratio, pcols,lchnk)

      do icol=1,ncol
        columnb(icol) = 0.0_r8
        colratio(icol) = 0.0_r8
      end do
      do icol=1,ncol
        do k=1,pver
          columnb(icol) = columnb(icol)+deltah_km(icol,k) &
                        * 1.e9*qm1(icol,k,l_bc_ai)*rhoda(icol,k)
          colratio(icol) = dload_bc_4(icol)/columnb(icol)
        end do
!        if(colratio(icol).lt.oneminus.or.colratio(icol).gt.oneplus) then
!          write(99,*) 'my bc 4 ratio =', icol, colratio(icol)
!        endif 
      end do

      call outfld('COLRBC4 ', colratio, pcols,lchnk)

      do icol=1,ncol
        columnb(icol) = 0.0_r8
        colratio(icol) = 0.0_r8
      end do
      do icol=1,ncol
        do k=1,pver
          columnb(icol) = columnb(icol)+deltah_km(icol,k) &
                        * 1.e9*qm1(icol,k,l_bc_n)*rhoda(icol,k)
          colratio(icol) = dload_bc_12(icol)/columnb(icol)
        end do
!        if(colratio(icol).lt.oneminus.or.colratio(icol).gt.oneplus) then
!          write(99,*) 'my bc 12 ratio =', icol, colratio(icol) 
!        endif
      end do

      call outfld('COLRBC12', colratio, pcols,lchnk)

      do icol=1,ncol
        columnb(icol) = 0.0_r8
        colratio(icol) = 0.0_r8
      end do
      do icol=1,ncol
        do k=1,pver
          columnb(icol) = columnb(icol)+deltah_km(icol,k) &
                        * 1.e9*qm1(icol,k,l_bc_ni)*rhoda(icol,k)
          colratio(icol) = dload_bc_14(icol)/columnb(icol)
        end do
!        if(colratio(icol).lt.oneminus.or.colratio(icol).gt.oneplus) then
!          write(99,*) 'my bc 14 ratio =', icol, colratio(icol)
!        endif 
      end do

      call outfld('COLRBC14 ', colratio, pcols,lchnk)

      do icol=1,ncol
        columnb(icol) = 0.0_r8
        colratio(icol) = 0.0_r8
      end do
      do icol=1,ncol
        do k=1,pver
          columnb(icol) = columnb(icol)+deltah_km(icol,k) &
                        * 1.e9*qm1(icol,k,l_bc_ac)*rhoda(icol,k)
          colratio(icol) = dload_bc_ac(icol)/columnb(icol)
        end do
!        if(colratio(icol).lt.oneminus.or.colratio(icol).gt.oneplus) then
!          write(99,*) 'my bc ac ratio =', icol, colratio(icol) 
!        endif
      end do

      call outfld('COLRBCAC', colratio, pcols,lchnk)

!OC:

      do icol=1,ncol
        columnb(icol) = 0.0_r8
        colratio(icol) = 0.0_r8
      end do
      do icol=1,ncol
        do k=1,pver
          columnb(icol) = columnb(icol)+deltah_km(icol,k) &
                        * 1.e9*qm1(icol,k,l_om_ai)*rhoda(icol,k)
          colratio(icol) = dload_oc_4(icol)/columnb(icol)
        end do
!        if(colratio(icol).lt.oneminus.or.colratio(icol).gt.oneplus) then
!          write(99,*) 'my oc 4 ratio =', icol, colratio(icol)
!        endif 
      end do

      call outfld('COLROC4 ', colratio, pcols,lchnk)

      do icol=1,ncol
        columnb(icol) = 0.0_r8
        colratio(icol) = 0.0_r8
      end do
      do icol=1,ncol
        do k=1,pver
          columnb(icol) = columnb(icol)+deltah_km(icol,k) &
                        * 1.e9*qm1(icol,k,l_om_ni)*rhoda(icol,k)
          colratio(icol) = dload_oc_14(icol)/columnb(icol)
        end do
!        if(colratio(icol).lt.oneminus.or.colratio(icol).gt.oneplus) then
!          write(99,*) 'my oc 14 ratio =', icol, colratio(icol)
!        endif 
      end do

      call outfld('COLROC14', colratio, pcols,lchnk)

      do icol=1,ncol
        columnb(icol) = 0.0_r8
        colratio(icol) = 0.0_r8
      end do
      do icol=1,ncol
        do k=1,pver
          columnb(icol) = columnb(icol)+deltah_km(icol,k) &
             * 1.e9*(qm1(icol,k,l_om_ac)+qm1(icol,k,l_soa_a1))*rhoda(icol,k)
          colratio(icol) = dload_oc_ac(icol)/columnb(icol)
        end do
!        if(colratio(icol).lt.oneminus.or.colratio(icol).gt.oneplus) then
!          write(99,*) 'my oc ac and soa a1 ratio =', icol, colratio(icol) 
!        endif
      end do

      call outfld('COLROCAC', colratio, pcols,lchnk)

!Sulfate:

      do icol=1,ncol
        columnb(icol) = 0.0_r8
        colratio(icol) = 0.0_r8
      end do
      do icol=1,ncol
        do k=1,pver
          columnb(icol) = columnb(icol)+deltah_km(icol,k) &
                        * 1.e9*(qm1(icol,k,l_so4_a1) &
                              + qm1(icol,k,l_so4_a2) &
                              + qm1(icol,k,l_so4_ac))*rhoda(icol,k)
          colratio(icol) = dload_s4_a(icol)/columnb(icol)
        end do
!        if(colratio(icol).lt.oneminus.or.colratio(icol).gt.oneplus) then
!          write(99,*) 'my sulfate a ratio =', icol, colratio(icol)
!        endif 
      end do

      call outfld('COLRSULA', colratio, pcols,lchnk)

      do icol=1,ncol
        columnb(icol) = 0.0_r8
        colratio(icol) = 0.0_r8
      end do
      do icol=1,ncol
        do k=1,pver
          columnb(icol) = columnb(icol)+deltah_km(icol,k) &
                        * 1.e9*(qm1(icol,k,l_so4_na))*rhoda(icol,k)
          colratio(icol) = dload_s4_1(icol)/columnb(icol)
        end do
!        if(colratio(icol).lt.oneminus.or.colratio(icol).gt.oneplus) then
!          write(99,*) 'my sulfate 1 ratio =', icol, colratio(icol)
!        endif 
      end do

      call outfld('COLRSUL1', colratio, pcols,lchnk)

      do icol=1,ncol
        columnb(icol) = 0.0_r8
        colratio(icol) = 0.0_r8
      end do
      do icol=1,ncol
        do k=1,pver
          columnb(icol) = columnb(icol)+deltah_km(icol,k) &
                        * 1.e9*(qm1(icol,k,l_so4_pr))*rhoda(icol,k)
          colratio(icol) = dload_s4_5(icol)/columnb(icol)
        end do
!        if(colratio(icol).lt.oneminus.or.colratio(icol).gt.oneplus) then
!          write(99,*) 'my sulfate 5 ratio =', icol, colratio(icol)
!        endif 
      end do

      call outfld('COLRSUL5', colratio, pcols,lchnk)

      return
end subroutine coltst4intcons
