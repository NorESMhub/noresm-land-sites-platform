      subroutine tabrefind (kcomp, ib, xlam, cref) 

c **********************************************************************************
c     Created by Alf Kirkevåg.
c **********************************************************************************

c      Here wavelength dependent complex rafractive indices (cref) 
c      are found from tabulated values for each aerosol component. 

      implicit none

      INTEGER  j, kcomp, ib, iband
      REAL     xlam(31), lam, lamg, nr, nrg, ni, nig
      COMPLEX  crin, cref(5,31)

!DiBagio-test+
      INTEGER  ilam, ist
      REAL     lambda(601)
      REAL     nrl(19,601), nil(19,601), nrla(601), nila(601)
!test-
      
c     initializing cref-array:
        do iband = 1, 31
      do j=1,5
        cref(j,iband) = 0.0
      enddo
        enddo 

        do iband = 1, ib

      do 300 j=1,5

c     Refractive indices for background aerosol components (internal mixing
c     is taken into account separately in refind.f for bacground modes which
c     consist of two constituents): 
       if(j.eq.1) then
         if(kcomp.eq.1.or.kcomp.eq.5) then
           open(10, file='input/suso_gads.inp', status='old') 
         elseif(kcomp.eq.2) then
           open(10, file='input/sot_bond.inp', status='old') 
         elseif(kcomp.eq.3.or.kcomp.eq.4) then
           open(10, file='input/waso_gads.inp', status='old') 
         elseif(kcomp.eq.6.or.kcomp.eq.7) then
            open(10, file='input/mineral_nymix.inp', status='old')
         elseif(kcomp.eq.8.or.kcomp.eq.9.or.kcomp.eq.10) then
           open(10, file='input/ss_gads.inp', status='old')
         else
           goto 300
         endif
c      Refractive indices for added components to be internally mixed 
c      with the background aerosol:
       elseif(j.eq.2) then
         open(10, file='input/suso_gads.inp', status='old')
       elseif(j.eq.3) then
           open(10, file='input/sot_bond.inp', status='old') 
       elseif(j.eq.4) then
         open(10, file='input/water_gads.inp', status='old')
       elseif(j.eq.5) then ! assumed OC refractive index
         open(10, file='input/waso_gads.inp', status='old')
       endif

       lamg=0
       nrg=0
       nig=0

       read(10,*)
       read(10,*)
       read(10,*)
       read(10,*)
       read(10,*)
 100   read(10,*) lam, nr, ni
       if(lam.gt.xlam(iband)) then
         cref(j,iband)=
     $  (1e0,0e0)*(xlam(iband)*(nr-nrg)+(nrg*lam-nr*lamg))/(lam-lamg)
     $ -(0e0,1e0)*(xlam(iband)*(ni-nig)+(nig*lam-ni*lamg))/(lam-lamg)
         goto 200
       else           
         lamg=lam
         nrg=nr
         nig=ni
            goto 100
       endif
 200   close(10)
 
 300  continue

        enddo ! iband


      goto 400  
!test+ Di Bagio et al. (2017)
      do ist=1,19
       if(ist.eq.1) then
      open(10, file='CRI_results_Tunisia.txt',status='old') 
       elseif(ist.eq.2) then
      open(10, file='CRI_results_Taklimakan.txt',status='old')   
       elseif(ist.eq.3) then
      open(10, file='CRI_results_SaudiArabia.txt',status='old') 
       elseif(ist.eq.4) then
      open(10, file='CRI_results_Patagonia.txt',status='old') 
       elseif(ist.eq.5) then
      open(10, file='CRI_results_Niger.txt',status='old') 
       elseif(ist.eq.6) then
      open(10, file='CRI_results_Namib-2.txt',status='old') 
       elseif(ist.eq.7) then
      open(10, file='CRI_results_Namib-1.txt',status='old') 
       elseif(ist.eq.8) then
      open(10, file='CRI_results_Morocco.txt',status='old') 
       elseif(ist.eq.9) then
      open(10, file='CRI_results_Mauritania.txt',status='old') 
       elseif(ist.eq.10) then
      open(10, file='CRI_results_Mali.txt',status='old') 
       elseif(ist.eq.11) then
      open(10, file='CRI_results_Libya.txt',status='old') 
       elseif(ist.eq.12) then
      open(10, file='CRI_results_Kuwait.txt',status='old') 
       elseif(ist.eq.13) then
      open(10, file='CRI_results_Gobi.txt',status='old') 
       elseif(ist.eq.14) then
      open(10, file='CRI_results_Ethiopia.txt',status='old') 
       elseif(ist.eq.15) then
      open(10, file='CRI_results_Bodele.txt',status='old') 
       elseif(ist.eq.16) then
      open(10, file='CRI_results_Australia.txt',status='old') 
       elseif(ist.eq.17) then
      open(10, file='CRI_results_Atacama.txt',status='old') 
       elseif(ist.eq.18) then
      open(10, file='CRI_results_Arizona.txt',status='old') 
       elseif(ist.eq.19) then
      open(10, file='CRI_results_Algeria.txt',status='old') 
       endif
      read(10,*)
       do j=1,601
         read(10,*)  lambda(j), nrl(ist,j), nil(ist,j)
         write(66,*) lambda(j), nrl(ist,j)
         write(67,*) lambda(j), nil(ist,j)
       enddo
      close(10)
      enddo
      
      do j=1,601
         nrla(j)=0.0
         nila(j)=0.0
      enddo
      do ist=1,19
         do j=1,601
           nrla(j)=nrla(j)+nrl(ist,j) 
           nila(j)=nila(j)+nil(ist,j) 
         enddo            
      enddo ! ist
      do j=1,601
        nrla(j)=nrla(j)/19.0 
        nila(j)=nila(j)/19.0 
        write(68,*) lambda(j), nrla(j)
        write(69,*) lambda(j), nila(j)
        write(70,*) lambda(j), nrla(j), nila(j)
      enddo            
ctest- 400  write(*,*) 'Di Bagio-loop off'

 400  end










