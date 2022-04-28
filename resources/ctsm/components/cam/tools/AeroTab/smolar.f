      subroutine smolar (ismolar, imax, d, dncny, dip)  

c **********************************************************************************
c     Created by Alf Kirkevåg.
c **********************************************************************************

      implicit none

      INTEGER i, imax, ismolar, it
      REAL d, eps, dip(0:100), dad(0:100), 
     $ dncny(0:100), dncinc(100)
      PARAMETER (eps=1.e-50)

      do i=0,imax
         dad(i)=dip(i)
      enddo

        do it=1,ismolar

      do i=0,imax
         dad(i)=(abs(dad(i))-dad(i)*dad(i)/d)
     $         *(dncny(i+1)-dncny(i))
     $         /(dncny(i)+dncny(i+1)+eps)
      enddo
      dad(imax)=0.0
      do i=1,imax
        dncinc(i)=-(dncny(i)*(dad(i)+abs(dad(i)))
     $             +dncny(i+1)*(dad(i)-abs(dad(i)))
     $             -dncny(i-1)*(dad(i-1)+abs(dad(i-1)))
     $             -dncny(i)*(dad(i-1)-abs(dad(i-1))))/(2.0*d)
      enddo
      do i=1,imax       
        dncny(i)=dncny(i)+dncinc(i)
      enddo

        enddo  ! it

      return
      end  
