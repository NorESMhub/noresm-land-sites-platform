      subroutine condsub (r, imax, diff, mfv, th, alpha, Dm)
       
c **********************************************************************************
c     Created by Alf Kirkevåg.
c **********************************************************************************

c     Calculation of radius dependent diffusion coefficients for sulfate or SOA (OM)

      implicit none

      INTEGER i, imax
      REAL diff, mfv, th, r(0:100), Dm(0:100), alpha

      do i=0,imax
        Dm(i)=diff/(r(i)/(r(i)+mfv)+4.0*diff/(alpha*th*r(i)))   ! unit um^2/s
c       write(*,*) r(i), r(i)*Dm(i)
      enddo

      return
      end
