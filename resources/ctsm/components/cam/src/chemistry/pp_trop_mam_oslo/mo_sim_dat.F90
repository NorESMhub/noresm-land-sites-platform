
      module mo_sim_dat

      private
      public :: set_sim_dat

      contains

      subroutine set_sim_dat

      use chem_mods,     only : clscnt, cls_rxt_cnt, clsmap, permute, adv_mass, fix_mass, crb_mass
      use chem_mods,     only : diag_map
      use chem_mods,     only : phtcnt, rxt_tag_cnt, rxt_tag_lst, rxt_tag_map
      use chem_mods,     only : pht_alias_lst, pht_alias_mult
      use chem_mods,     only : extfrc_lst, inv_lst, slvd_lst
      use chem_mods,     only : enthalpy_cnt, cph_enthalpy, cph_rid, num_rnts, rxntot
      use cam_abortutils,only : endrun
      use mo_tracname,   only : solsym
      use chem_mods,     only : frc_from_dataset
      use chem_mods,     only : is_scalar, is_vector
      use shr_kind_mod,  only : r8 => shr_kind_r8
      use cam_logfile,   only : iulog

      implicit none

!--------------------------------------------------------------
!      ... local variables
!--------------------------------------------------------------
      integer :: ios

      is_scalar = .true.
      is_vector = .false.

      clscnt(:) = (/      0,     0,     0,    30,     0 /)

      cls_rxt_cnt(:,4) = (/      1,    12,     0,    30 /)

      solsym(: 30) = (/ 'SO2             ','H2SO4           ','DMS             ','H2O2            ','SO4_NA          ', &
                        'SO4_A1          ','SO4_A2          ','SO4_AC          ','SO4_PR          ','BC_N            ', &
                        'BC_AX           ','BC_NI           ','BC_A            ','BC_AI           ','BC_AC           ', &
                        'OM_NI           ','OM_AI           ','OM_AC           ','DST_A2          ','DST_A3          ', &
                        'SS_A1           ','SS_A2           ','SS_A3           ','SOA_NA          ','SOA_A1          ', &
                        'SOA_LV          ','SOA_SV          ','monoterp        ','isoprene        ','H2O             ' /)

      adv_mass(: 30) = (/    64.064800_r8,    98.078400_r8,    62.132400_r8,    34.013600_r8,    98.078400_r8, &
                             98.078400_r8,   115.107340_r8,    98.078400_r8,    98.078400_r8,    12.011000_r8, &
                             12.011000_r8,    12.011000_r8,    12.011000_r8,    12.011000_r8,    12.011000_r8, &
                             12.011000_r8,    12.011000_r8,    12.011000_r8,   135.064039_r8,   135.064039_r8, &
                             58.442468_r8,    58.442468_r8,    58.442468_r8,   168.227200_r8,   168.227200_r8, &
                            168.227200_r8,   168.227200_r8,   136.228400_r8,    68.114200_r8,    18.014200_r8 /)

      crb_mass(: 30) = (/     0.000000_r8,     0.000000_r8,    24.022000_r8,     0.000000_r8,     0.000000_r8, &
                              0.000000_r8,     0.000000_r8,     0.000000_r8,     0.000000_r8,    12.011000_r8, &
                             12.011000_r8,    12.011000_r8,    12.011000_r8,    12.011000_r8,    12.011000_r8, &
                             12.011000_r8,    12.011000_r8,    12.011000_r8,     0.000000_r8,     0.000000_r8, &
                              0.000000_r8,     0.000000_r8,     0.000000_r8,   120.110000_r8,   120.110000_r8, &
                            120.110000_r8,   120.110000_r8,   120.110000_r8,    60.055000_r8,     0.000000_r8 /)

      fix_mass(:  7) = (/ 0.00000000_r8, 28.0134800_r8, 31.9988000_r8, 47.9982000_r8, 17.0068000_r8, &
                          62.0049400_r8, 33.0062000_r8 /)

      clsmap(: 30,4) = (/    3,   1,   4,   5,   6,   7,   8,   9,  10,  11, &
                            12,  13,  14,  15,  16,  17,  18,  19,  20,  21, &
                            22,  23,   2,  24,  25,  26,  27,  28,  29,  30 /)

      permute(: 30,4) = (/    1,   2,   3,   4,   5,   6,   7,   8,   9,  10, &
                             11,  12,  13,  14,  15,  16,  17,  18,  19,  20, &
                             21,  22,  23,  24,  25,  26,  27,  28,  29,  30 /)

      diag_map(: 30) = (/    1,   5,   7,   9,  10,  11,  12,  13,  14,  15, &
                            16,  17,  18,  19,  20,  21,  22,  23,  24,  25, &
                            26,  27,  28,  29,  30,  31,  32,  35,  37,  38 /)

      extfrc_lst(:  7) = (/ 'SO2             ','BC_NI           ','BC_AX           ','BC_N            ','OM_NI           ', &
                            'SO4_PR          ','H2O             ' /)

      frc_from_dataset(:  7) = (/ .true., .true., .true., .true., .true., &
                                  .true., .true. /)

      inv_lst(:  7) = (/ 'M               ', 'N2              ', 'O2              ', 'O3              ', 'OH              ', &
                         'NO3             ', 'HO2             ' /)

      if( allocated( rxt_tag_lst ) ) then
         deallocate( rxt_tag_lst )
      end if
      allocate( rxt_tag_lst(rxt_tag_cnt),stat=ios )
      if( ios /= 0 ) then
         write(iulog,*) 'set_sim_dat: failed to allocate rxt_tag_lst; error = ',ios
         call endrun
      end if
      if( allocated( rxt_tag_map ) ) then
         deallocate( rxt_tag_map )
      end if
      allocate( rxt_tag_map(rxt_tag_cnt),stat=ios )
      if( ios /= 0 ) then
         write(iulog,*) 'set_sim_dat: failed to allocate rxt_tag_map; error = ',ios
         call endrun
      end if
      rxt_tag_lst(     1:     3) = (/ 'jh2o2                           ', 'usr_HO2_HO2                     ', &
                                      'usr_DMS_OH                      ' /)
      rxt_tag_map(:rxt_tag_cnt) = (/    1,   2,   7 /)
      if( allocated( pht_alias_lst ) ) then
         deallocate( pht_alias_lst )
      end if
      allocate( pht_alias_lst(phtcnt,2),stat=ios )
      if( ios /= 0 ) then
         write(iulog,*) 'set_sim_dat: failed to allocate pht_alias_lst; error = ',ios
         call endrun
      end if
      if( allocated( pht_alias_mult ) ) then
         deallocate( pht_alias_mult )
      end if
      allocate( pht_alias_mult(phtcnt,2),stat=ios )
      if( ios /= 0 ) then
         write(iulog,*) 'set_sim_dat: failed to allocate pht_alias_mult; error = ',ios
         call endrun
      end if
      pht_alias_lst(:,1) = (/ '                ' /)
      pht_alias_lst(:,2) = (/ '                ' /)
      pht_alias_mult(:,1) = (/ 1._r8 /)
      pht_alias_mult(:,2) = (/ 1._r8 /)
      allocate( num_rnts(rxntot-phtcnt),stat=ios )
      if( ios /= 0 ) then
         write(iulog,*) 'set_sim_dat: failed to allocate num_rnts; error = ',ios
         call endrun
      end if
      num_rnts(:) = (/      2,     2,     2,     2,     3,     2,     2,     2,     2,     2, &
                            2,     2 /)

      end subroutine set_sim_dat

      end module mo_sim_dat
