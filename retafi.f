CM
      subroutine ems_iz_sus_fwd_tran_pm(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      integer is(0:is_n_en_m1)
      integer eta_srch_mode
 
      if (iand(inv_alg_msk, inv_alg_sus) .ne. 0) then
c
c     Set the max supersparse ops: Fwd col-U-eta
c
         sus_eta_srch_co_mu = 1d-2
c         print*, ' Enter supersparse eta search cost multiplier'
c         read*, sus_eta_srch_co_mu
         is(p_up_eta_pv_in_r) = n_r
         if (sus_eta_srch_co_mu .eq. zero) then
c            print*, ' Enter max supersparse ops: Fwd C-U-eta/R-L-eta'
c            read*, is(p_up_eta_pv_in_r)
            if (is(p_up_eta_pv_in_r) .lt. -1) is(p_up_eta_pv_in_r) = n_r
         endif
c
c     Set the max supersparse ops: Fwd col-L-eta
c
         is(p_lo_eta_pv_in_r) = n_r
         if (is(p_up_eta_pv_in_r) .lt. 0) then
c            print*, ' Enter max supersparse ops: Fwd C-L-eta/R-U-eta'
c            read*, is(p_lo_eta_pv_in_r)
            if (is(p_lo_eta_pv_in_r) .lt. -1) is(p_lo_eta_pv_in_r) = n_r
         endif
      else
c
c     The first of these values is used in ems_tom_inv. The second must
c     be set to prevent supersparse fwd TRAN with U-etas.
c     ????? Is supersparse fwd TRAN with L-etas prevented ????
c
         is(p_lo_eta_pv_in_r) = n_r
         is(p_up_eta_pv_in_r) = -1
      endif
      if (iand(eta_fi_mode_msk, eta_fi_r_eta_y_bt) .ne. 0 .or.
     &     iand(eta_fi_mode_msk, eta_fi_r_eta_poss_bt) .ne. 0) then
         eta_srch_mode = 1
c         print*, ' Enter supersparse Fwd TRAN pivot search mode:'
c         print*, '    0 => Search all:',
c     &        ' 1 => Search eta list:',
c     &        ' 2 => Search eta buckets'
c         read*, eta_srch_mode
         if (eta_srch_mode .le. 0) then
            sus_eta_srch_al_t_ls_ix_lm = n_r+1
            sus_eta_srch_ls_t_buk_ix_lm = n_r+1
         else if (eta_srch_mode .eq. 1) then
            sus_eta_srch_al_t_ls_ix_lm = 0
            sus_eta_srch_ls_t_buk_ix_lm = n_r+1
         else
            sus_eta_srch_al_t_ls_ix_lm = 0
            sus_eta_srch_ls_t_buk_ix_lm = 0
         endif
      endif
      if (iand(eta_fi_mode_msk, eta_fi_bwd_p_y_bt) .ne. 0 .or.
     &     iand(eta_fi_mode_msk, eta_fi_bwd_p_poss_bt) .ne. 0) then
         is(p_eta_w_l_en_in_r) = 1
      endif
      return
      end
 
      subroutine ems_iz_r_eta_fi_pm(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      integer is(0:is_n_en_m1)
c
c     Take the max supersparse ops: Fwd row-U-eta/row-L-eta from the
c     corresponding maxima for col-L-eta/col-U-eta.
c
      is(p_lo_eta_pv_in_c) = is(p_up_eta_pv_in_r)
      is(p_up_eta_pv_in_c) = is(p_lo_eta_pv_in_r)
      return
      end
