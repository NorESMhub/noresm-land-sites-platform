!  SVN:$Id: CICE.F90 700 2013-08-15 19:17:39Z eclare $
!=======================================================================
! Copyright 2013, LANSLLC. All rights reserved.
! Unless otherwise indicated, this information has been authored by an 
! employee or employees of the Los Alamos National Security, LLC (LANS), 
! operator of the Los Alamos National Laboratory under Contract No. 
! DE-AC52-06NA25396 with the U.S. Department of Energy. The U.S. Government 
! has rights to use, reproduce, and distribute this information. The public 
! may copy and use this information without charge, provided that this 
! Notice and any statement of authorship are reproduced on all copies. 
! Neither the Government nor LANS makes any warranty, express or implied, 
! or assumes any liability or responsibility for the use of this 
! information.
!
! CICE is developed and maintained by Elizabeth C. Hunke (eclare@lanl.gov)
! Group T-3 (Fluid Dynamics and Solid Mechanics), Los Alamos National
! Laboratory, with support from the Earth System Modeling and Regional and
! Global Climate Modeling programs of the Office of Biological and
! Environmental Research within the U.S. Department of Energy's Office of
! Science.  Los Alamos National Laboratory is operated by the DOE National
! Nuclear Security Administration under Contract DE-AC52-06NA25396.
! 
! Numerous researchers have contributed to this effort, especially members
! of the CESM Polar Climate Working Group and the sea ice modeling team
! at UK Met Office Hadley Centre -- thanks to all! 
! 
!=======================================================================
#ifndef popcice
!
! Main driver routine for CICE.  Initializes and steps through the model.
! This program should be compiled if CICE is run as a separate executable,
!  but not if CICE subroutines are called from another program (e.g., CAM).
!
! authors Elizabeth C. Hunke and William H. Lipscomb, LANL
!
! 2006: Converted to free form source (F90) by Elizabeth Hunke
! 2008: E. Hunke moved ESMF code to its own driver
!
      program icemodel

      use CICE_InitMod
      use CICE_RunMod
      use CICE_FinalMod

      implicit none

      !-----------------------------------------------------------------
      ! Initialize CICE
      !-----------------------------------------------------------------

      call CICE_Initialize

      !-----------------------------------------------------------------
      ! Run CICE
      !-----------------------------------------------------------------

      call CICE_Run

      !-----------------------------------------------------------------
      ! Finalize CICE 
      !-----------------------------------------------------------------

      call CICE_Finalize

      end program icemodel

#endif
!=======================================================================
!
! Wrapper for the print_state debugging routine.
! Useful for debugging in the main driver (see ice.F_debug)
! ip, jp, mtask are set in ice_diagnostics.F
!
! author Elizabeth C. Hunke, LANL
!
      subroutine debug_ice(iblk, plabeld)

      use ice_kinds_mod
      use ice_calendar, only: istep1
      use ice_communicate, only: my_task
      use ice_diagnostics, only: check_step, iblkp, ip, jp, mtask, print_state
      use ice_domain, only: nblocks
      use ice_blocks, only: nx_block, ny_block

      character (char_len), intent(in) :: plabeld
      integer (kind=int_kind), intent(in) :: iblk

      ! local 
      integer (kind=int_kind) :: i, j

      if (istep1 >= check_step .and. &
          iblk==iblkp .and. my_task==mtask) then

      do j = 1, ny_block
      do i = 1, nx_block
         if (i==ip .and. j==jp) call print_state(plabeld,i,j,iblk)
      enddo
      enddo

      endif

      end subroutine debug_ice

!=======================================================================
