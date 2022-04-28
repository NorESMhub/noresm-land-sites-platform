
 subroutine checkTableHeader (ifil)

! This subroutine reads the header-text in a look-up table (in file with iu=ifil). 
! Later: use it to also check AeroTab - CAM5-Oslo consistency w.r.t. assumed modal
! radii, mass densities, etc...

      integer, intent(in) :: ifil
      character*80 headertext
      character*12 text0, text1 


      text0='X-CHECK LUT'
      text1='none       '
      do while (text1(2:12).ne.text0(2:12))
        read(ifil,1000) headertext
        text1=headertext(2:12) 
!        write(*,*) 'text0, text1 =', text0, text1
      enddo


 1000 format(A)

      return
 end subroutine checkTableHeader