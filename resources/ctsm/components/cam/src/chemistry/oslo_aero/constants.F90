
subroutine constants
!
! A number of constants used in the emission and size-calculation in CAM-Oslo
! �S Jan 2011.
! Updated by Alf Kirkev�g May 2013.
! Updated by Alf Grini February 2014.
!
        use shr_kind_mod, only: r8 => shr_kind_r8
        use physconst,    only: pi
        use const
        use aerosoldef
        use koagsub, only : initializeCoagulationReceivers, initializeCoagulationCoefficients & 
                            , initializeCoagulationOutput

        use oslo_utils
   implicit none


   integer kcomp,i
	real(r8),dimension(0:nmodes) :: rhob        !density of background aerosol in mode

   real(r8)                      :: rhorbc   !This has to do with fractal dimensions of bc, come back to this!!
   real(r8)                      :: sumnormnk
   real(r8)                      :: totalLogDelta
   real(r8)                      :: logDeltaBin
   real(r8)                      :: logNextEdge

   rhob(:)=-1.0_r8
   volumeToNumber(:)=-1.0_r8
   numberToSurface(:)=-1.0_r8
   !Prepare modal properties
   do i=0, nmodes
         
         if(getNumberOfTracersInMode(i) .gt. 0)then

            !Approximate density of mode
            rhob(i)  = rhopart(getTracerIndex(i,1,.false.))          !density of mode is density of first species in mode

            !REPLACE THE EFACT-VARIABLE WITH THIS!!
            volumeToNumber(i) = 1.0_r8                                 & 
                                 /                                  &
                            ( DEXP ( 4.5_r8 * ( log(originalSigma(i)) * log(originalSigma(i)) ) ) &
                            *(4.0_r8/3.0_r8)*pi*(originalNumberMedianRadius(i))**3 )  

            numberToSurface(i) = 4.0_r8*pi*lifeCycleNumberMedianRadius(i)*lifeCycleNumberMedianRadius(i)&
                                 *DEXP(log(lifeCycleSigma(i))*log(lifeCycleSigma(i)))
         end if
   end do

    
      !Find radius in edges and midpoints of bin
      rBinEdge(1) = rTabMin
      totalLogDelta = log(rTabMax/rTabMin)
      logDeltaBin = totalLogDelta / nBinsTab
      do i=2,nBinsTab+1
         logNextEdge = log(rBinEdge(i-1)) + logDeltaBin
         rBinEdge(i) = DEXP(logNextEdge)
         rBinMidPoint(i-1) = sqrt(rBinEdge(i)*rBinEdge(i-1))
      end do

      !Calculate the fraction of a mode which goes to aquous chemstry
      numberFractionAvailableAqChem(:)=0.0_r8
      do i=1,nbmodes
         if(isTracerInMode(i,l_so4_a2))then
            numberFractionAvailableAqChem(i) =  1.0_r8 - calculateLognormalCDF(rMinAquousChemistry &
                                                                     , originalNumberMedianRadius(i) &
                                                                     , originalSigma(i) &
                                                                     )
         end if
      end do

      !Set the density of the fractal mode ==> we get lesser density
      !than the emitted density, so for a given mass emitted, we get
      !more number-concentration!! This is a way of simulating that the 
      !aerosols take up more space
      rhorbc = calculateEquivalentDensityOfFractalMode(      &
                                             rhopart(l_bc_n) &                             !emitted density
                                            ,originalNumberMedianRadius(MODE_IDX_BC_NUC) & !emitted size
                                            ,2.5_r8                                      & !fractal dim
                                            ,originalNumberMedianRadius(MODE_IDX_BC_EXT_AC) & !diameter of mode
                                            ,originalSigma(MODE_IDX_BC_EXT_AC))               !sigma mode

      rhopart(l_bc_ax) = rhorbc
      !fxm: not the right place for this change of value, 
      !but anyway.. this re-calculateion of tracer density
      !influences density of mode used in coagulation
      rhob(MODE_IDX_BC_EXT_AC)=rhorbc

      !Size distribution of the modes!
      !Unclear if this should use the radii assuming growth or not!
      !Mostly used in code where it is sensible to assume some growth has
      !happened, so it is used here
      do kcomp = 0,nmodes
        do i=1,nBinsTab
          !dN/dlogR (does not sum to one over size range)
          nk(kcomp,i) = calculatedNdLogR(rBinMidPoint(i), lifeCycleNumberMedianRadius(kcomp), lifeCycleSigma(kcomp)) 
          !dN (sums to one) over the size range             
          normnk(kcomp,i) =logDeltaBin*nk(kcomp,i)
        enddo 
      enddo  ! kcomp

      !++test: Normalized size distribution must sum to one (accept 2% error)
      do kcomp=0,nmodes
        sumNormNk = sum(normnk(kcomp,:))
        if(abs(sum(normnk(kcomp,:)) - 1.0_r8) .gt. 2.0e-2_r8)then
            print*, "sum normnk", sum(normnk(kcomp,:))
            stop
        endif
      enddo
      !--test

      !Initialize coagulation
      call initializeCoagulationReceivers()

      !Calculate the coagulation coefficients Note: Inaccurate density used!
      call initializeCoagulationCoefficients(rhob, lifeCycleNumberMedianRadius)


      call initializeCoagulationOutput()

      return
      end subroutine constants
