module oslo_utils

   use ppgrid, only : pcols, pver
   use shr_kind_mod, only: r8 => shr_kind_r8
!   use commondefinitions, only: nmodes, nbmodes 
   use commondefinitions 
!   use aerosoldef, only: nmodes, getDryDensity,  & 
!                        getNumberOfBackgroundTracersInMode &
!                        ,getTracerIndex, originalNumberMedianRadius
  use aerosoldef, only: getDryDensity,  &
                        getNumberOfBackgroundTracersInMode &
                        ,getTracerIndex
   use const, only : volumeToNumber, rbinMidPoint, rbinEdge, nBinsTab
   use physconst, only : pi
   use constituents, only: pcnst
   
contains

   subroutine calculateNumberConcentration(ncol, q, rho_air, numberConcentration)
      implicit none
      integer, intent(in)   :: ncol                                !number of columns used
      real(r8), intent(in)  :: q(pcols,pver,pcnst)                 ![kg/kg] mass mixing ratios
      real(r8), intent(in)  :: rho_air(pcols,pver)                 ![kg/m3] air density
      real(r8), intent(out) :: numberConcentration(pcols,pver,0:nmodes)   ![#/m3] number concentration

      integer :: m, l, mm, k

      numberConcentration(:,:,:) = 0.0_r8

      do m = 0, nmodes

         do l=1,getNumberOfBackgroundTracersInMode(m)
            mm = getTracerIndex(m,l,.false.)

               do k=1,pver
                  numberConcentration(:ncol,k,m) = numberConcentration(:ncol,k,m) & 
                     + ( q(:ncol,k,mm) / getDryDensity(m,l))  !Volume of this tracer
               end do

         end do
      end do

      !until now, the variable "numberConcentration" actually contained "volume mixing ratio"
      !the next couple of lines fixes this!
      do m= 0, nmodes
         do k=1,pver
            numberConcentration(:ncol,k,m) = numberConcentration(:ncol,k,m) * rho_air(:ncol,k) * volumeToNumber(m)
         end do
      end do
      
      return

   end subroutine calculateNumberConcentration


   !Note the "nmodes" here
   subroutine calculateNumberMedianRadius(numberConcentration &
                                       , volumeConcentration &
                                       , lnSigma             &
                                       , numberMedianRadius  &
                                       , ncol                )

      implicit none
      real(r8), intent(in)  :: numberConcentration(pcols,pver,0:nmodes)   ![#/m3] number concentration
      real(r8), intent(in)  :: volumeConcentration(pcols,pver,nmodes)     ![kg/kg] mass mixing ratios
      real(r8), intent(in)  :: lnSigma(pcols,pver,nmodes)                 ![kg/m3] air density
      integer, intent(in)   :: ncol                                       !number of columns used

      real(r8), intent(out) :: numberMedianRadius(pcols,pver,nmodes)      ![m] 

      real(r8), parameter :: aThird = 1.0_r8/3.0_r8

      integer :: n,k

      do n=1,nmodes
         do k=1,pver
            where(volumeConcentration(:ncol,k,n) .gt. 1.e-20_r8)
               numberMedianRadius(:ncol, k, n) = 0.5_r8 &                  !diameter ==> radius 
                 * (volumeConcentration(:ncol,k,n)       &                 !conversion formula
                 * 6.0_r8/pi/numberConcentration(:ncol,k,n) &            
                 *DEXP(-4.5_r8*lnsigma(:ncol,k,n)*lnsigma(:ncol,k,n)))**aThird
            elsewhere
               numberMedianRadius(:ncol,k,n) = originalNumberMedianRadius(n)
            end where
         end do
      end do

   end subroutine calculateNumberMedianRadius


   function calculateEquivalentDensityOfFractalMode( emissionDensity           &  ![kg/m3] density at point of emission
                                                   , emissionRadius            &  ![kg/m3] radius at point of emission
                                                   , fractalDimension          &  ![kg/m3] fractal dimension of mode
                                                   , modeNumberMedianRadius    &  ![m] number median radius of mode
                                                   , modeStandardDeviation     &  ![m] standard deviation of mode
                                                   ) result (equivalentDensityOfFractal)

      !Purpose: output equivalent density of a fractal mode 
      implicit none
      real(r8), intent(in) :: emissionDensity
      real(r8), intent(in) :: emissionRadius
      real(r8), intent(in) :: fractalDimension
      real(r8), intent(in) :: modeNumberMedianRadius
      real(r8), intent(in) :: modeStandardDeviation

      real(r8)             :: sumVolume 
      real(r8)             :: sumMass 
      real(r8)             :: dN, dNdLogR, dLogR
      real(r8)             :: densityBin
      integer              :: i

      !output
      real(r8)             :: equivalentDensityOfFractal

      sumVolume = 0.0_r8
      sumMass   = 0.0_r8
      do i=1, nbinsTab
         dLogR = log(rBinEdge(i+1)/rBinEdge(i))
         dNdLogR = calculatedNdLogR(rBinMidPoint(i), modeNumberMedianRadius, modeStandardDeviation)

         !Equivalent density (decreases with size since larger particles are long
         !"hair like" threads..)
         if(rBinMidPoint(i) < emissionRadius)then
            densityBin = emissionDensity
         else
            densityBin = emissionDensity*(emissionRadius/rBinMidPoint(i))**(3.0 - fractalDimension)
         endif

         !number concentration in this bin
         dN = dNdLogR * dLogR

         !sum up volume and mass (factor of 4*pi/3 omitted since in both numerator and nominator)
         sumVolume = sumVolume + dN * (rBinMidPoint(i)**3)
         sumMass   = sumMass + dN * densityBin * (rBinMidPoint(i)**3)

      end do

      !Equivalent density is mass by volume
      equivalentDensityOfFractal = sumMass / sumVolume

   end function calculateEquivalentDensityOfFractalMode 



   function calculatedNdLogR(actualRadius, numberMedianRadius, sigma) result (dNdLogR)
      implicit none
      real(r8), intent(in)   :: actualRadius
      real(r8), intent(in)   :: numberMedianRadius
      real(r8), intent(in)   :: sigma

      real(r8)              :: logSigma
      real(r8)   :: dNdLogR

      logSigma = log(sigma)

      !This is the formula for the lognormal distribution
      dNdLogR = 1.0_r8/(sqrt(2.0_r8*pi)*log(sigma)) &
         * DEXP(-0.5_r8*(log(actualRadius/numberMedianRadius))**2/(logSigma**2))
      
      return
  end function calculatedNdLogR

   !http://en.wikipedia.org/wiki/Log-normal_distribution#Cumulative_distribution_function
   function calculateLognormalCDF(actualRadius, numberMedianRadius, sigma) result(CDF)
      implicit none
      real(r8), intent(in) :: actualRadius
      real(r8), intent(in) :: numberMedianRadius
      real(r8), intent(in) :: sigma

      real(r8)             :: argument
      real(r8)             :: CDF

      argument = -1.0_r8*(log(actualRadius/numberMedianRadius) / log(sigma) / sqrt(2.0_r8))
      CDF = 0.5_r8 * erfc(argument)
      return
    end function calculateLognormalCDF


end module oslo_utils
