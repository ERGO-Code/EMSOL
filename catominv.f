C->>> --------------------------------------------> ems_ca_tom_inv <<<
c     Determines the Tomlin invert according to vr_in_r.
c
      subroutine ems_ca_tom_inv(
     &     vr_in_r,
     &     hdl_eta_grp,
     &     ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'SLAPCS.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMIHDL.INC'
      include 'TOMHDL.INC'
      include 'TOMINV.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
CM      IF (emsol_km .EQ. 1) THEN
C?      include 'EMSKM.INC'
CM      ENDIF
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      include 'EMSMMGRI.INC'
      double precision ds(0:ds_n_en_m1)
      integer vr_in_r(0:n_r)
      integer hdl_eta_grp(0:hdl_z_m1)
      integer is(0:is_n_en_m1)
      integer p_eta_grp
      integer p_eta_v, p_eta_ix, p_eta_rec, p_eta_sa
c      integer p_tom_inv_rl_wk
c      integer p_tom_inv_bs_c_at
c      integer p_tom_inv_r_k_a
c      integer p_tom_inv_pv_r_n_or_mrt
c      integer p_tom_inv_og_vr_in_r
c      integer p_tom_inv_og_st
      integer rt_cod, rp_cn, rp_lvl
      integer n_eta, n_eta_el, n_lo_eta
      integer n_wo, n_fr_wo
      integer cu_is_n_en, cu_rq_is_n_en, mx_rq_is_n_en
      integer prev_inv_mx_n_eta, prev_inv_mx_n_eta_el
      integer sv_sus_fwd_tran_mx_n_op
      double precision fill_fac
      integer mem_mgr_rt_cod
      logical no_po
      logical g_r_eta_fi
      logical alw_f7_wr, er_fd
      integer rl_wk_a_ix
CM      IF (emsol_dev .EQ. 1) THEN
C?      character*20 fi_nm
CM      ENDIF
CM      IF (emsol_deb .EQ. 1) THEN
C?      integer baso_si_it_n
C?      common/ems_baso_si_it_n_com/baso_si_it_n
CM      ENDIF
 
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_inv_lvl1) call ems_tt_rec(tom_inv_tt, n_bs)
CM      ENDIF
      alw_f7_wr = ems_msg_no_prt_fm .ge. 1
c
c     Allocate space for the basis INVERT pointers
c
      if (iand(ml_blk_st_msk, ml_blk_st_ml_bs_inv_p) .eq. 0) then
         call ems_iz_blk_ml_bs_inv_p(is)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      endif
      call ems_iz_blk_tom_inv_wk(mx_n_c, n_r, is)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
 
CM      IF (emsol_deb .EQ. 1) THEN
C?c      print*, 'Checking vr_in_r'
C?c      call ems_ck_rpt_vr(er_fd, n_r, mx_n_c+n_r, vr_in_r(1))
C?c      if (er_fd) then
C?c         rt_cod = ior(rt_cod, tom_inv_er_bt)
C?c         goto 7000
C?c      endif
CM      ENDIF
CM      IF (emsol_dev .EQ. 1) THEN
C?      baso_si_it_n = n_si_it-1
C?      if (n_si_it .eq. baso_si_it_n) then
C?         print*, ' Enter file for basis '
C?         read*, fi_nm
C?         open(unit = 26, file = fi_nm)
C?         call ems_baso(rt_cod, ds, 26, 1)
C?         close(26)
C?         print*, ' Enter next baso_si_it_n'
C?         read*, baso_si_it_n
C?      endif
CM      ENDIF
      inv_mx_n_eta = max(
     &     inv_mx_n_eta,
     &     2*n_r,
     &     int(float(eta_fi_n_inv_eta)*1.2))
      inv_mx_n_eta_el = max(
     &     inv_mx_n_eta_el,
     &     int(two*float(n_a_el)*float(min(n_r, n_c))/float(n_c)) + 1,
     &     int(float(eta_fi_n_inv_ix)*1.2))
      rp_lvl = 0
CM      IF (emsol_deb .EQ. 1) THEN
C?      tom_inv_rpt_n = 0
CM      ENDIF
 100  continue
c      if (eta_fi_n_eta .eq. 0)
c     &     write(*, 9000) n_r, n_c, n_a_el,
c     &     inv_mx_n_eta, inv_mx_n_eta_el
c 9000 format('TOMINV: ', 5(1x,i9))
c     if (ems_msg_no_prt_fm .ge. 1) write(ems_li, STAR)
c     &     'tom_inv mx_n_eta =   ', inv_mx_n_eta,
c     &     '        mx_n_eta_el =', inv_mx_n_eta_el
c     call ems_msg_wr_li(inv_msg_n)
      call ems_ope_eta_grp(
     &     no_po, n_wo, n_fr_wo,
     &     hdl_eta_grp,
     &     rsmi_eta_grp_ty, sto_pk_eta_v, pk_c_eta_se_ty,
     &     inv_mx_n_eta, inv_mx_n_eta_el, ds, is)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      if (no_po) goto 8000
 
      call ems_mem_mgr_g_p4(mem_mgr_rt_cod, is,
     &     hdl_eta_grp, p_eta_grp)
      if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) goto 8800
      call ems_mem_mgr_g_p8(mem_mgr_rt_cod, is,
     &     is(p_eta_grp + eta_grp_os_hdl_v), p_eta_v)
      if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) goto 8800
      call ems_mem_mgr_g_p4(mem_mgr_rt_cod, is,
     &     is(p_eta_grp + eta_grp_os_hdl_ix), p_eta_ix)
      if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) goto 8800
      call ems_mem_mgr_g_p4(mem_mgr_rt_cod, is,
     &     is(p_eta_grp + eta_grp_os_hdl_rec), p_eta_rec)
      if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) goto 8800
 
      p_eta_sa = p_eta_rec + eta_se_rec_os_ze_sa_en
c
c     Get the pointers from the handles in case blocks were moved in
c     getting more space for the INVERT
c
      call ems_mem_mgr_g_p8(mem_mgr_rt_cod, is,
     &     hdl_tom_inv_rl_wk, p_tom_inv_rl_wk)
      if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) goto 8800
      call ems_mem_mgr_g_p4(mem_mgr_rt_cod, is,
     &     hdl_tom_inv_bs_c_at, p_tom_inv_bs_c_at)
      if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) goto 8800
      call ems_mem_mgr_g_p4(mem_mgr_rt_cod, is,
     &     hdl_tom_inv_r_k_a, p_tom_inv_r_k_a)
      if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) goto 8800
      call ems_mem_mgr_g_p4(mem_mgr_rt_cod, is,
     &     hdl_tom_inv_pv_r_n_or_mrt, p_tom_inv_pv_r_n_or_mrt)
      if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) goto 8800
      call ems_mem_mgr_g_p4(mem_mgr_rt_cod, is,
     &     hdl_tom_inv_og_vr_in_r, p_tom_inv_og_vr_in_r)
      if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) goto 8800
      call ems_mem_mgr_g_p4(mem_mgr_rt_cod, is,
     &     hdl_tom_inv_og_st, p_tom_inv_og_st)
      if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) goto 8800
c
c     Copy the original vr_in_r and st in case recovered vectors need
c     to be checked (or copied from them)
c
      call ems_cp_vr_in_r_and_st(n_c, mx_n_c, n_r,
     &     vr_in_r, is(p_tom_inv_og_vr_in_r),
     &     is(p_st), is(p_tom_inv_og_st))
c
c     Make sure that there is no reporting if ems_msg_no_prt_fm <= 0
c
      rp_cn = -1
      if (ems_msg_no_prt_fm .ge. 1) rp_cn = ems_msg_wr_cn
CM      IF (emsol_km .EQ. 1) THEN
C?c
C?c     Report to the screen so KM can see the singularity action.
C?c
C?      rp_cn = 6
CM      ENDIF
c
c     Set the singularity action to correspond to the original INVERT
c
      tom_inv_sing_msk = 0
CM      IF (emsol_km .EQ. 1) THEN
C?c
C?c     Allow KM to over-rule this setting
C?c
C?      tom_inv_sing_msk = km_tom_inv_sing_msk
CM      ENDIF
CM      IF (emsol_deb .EQ. 1) THEN
C?      if (tom_inv_rpt_n .eq. 1) then
C?c
C?c     On the first repeat, set the report level to 1 and ensure that the
C?c     report channel is positive
C?c
C?         rp_lvl = 1
C?         rp_cn = max(0, rp_cn)
C?      else if (tom_inv_rpt_n .eq. 2) then
C?c
C?c     On the second repeat, set the report level to 1, ensure that the
C?c     report channel is positive and ensure that the singularity
C?c     action corresponds to the original INVERT
C?c
C?         rp_lvl = 1
C?         rp_cn = max(0, rp_cn)
C?         tom_inv_sing_msk = 0
C?      endif
CM      ENDIF
      if (iand(inv_alg_msk, inv_alg_sus) .ne. 0) then
         sv_sus_fwd_tran_mx_n_op = is(p_lo_eta_pv_in_r)
         is(p_lo_eta_pv_in_r) = n_r
      else
         is(p_lo_eta_pv_in_r) = -1
      endif
      call ems_g_rsmi_rl_wk_a_ix(rl_wk_a_ix)
      if (rl_wk_a_ix .lt. 0) go to 8010
      call ems_tom_inv(
     &     rt_cod, rp_cn, rp_lvl, g_tt_da,
     &     n_r, mx_n_c, n_c, n_a_el, n_inv_sing, fill_fac,
     &     vr_in_r,
     &     ds(p_mtx_r_v), is(p_mtx_r_ix), is(p_mtx_c_sa),
     &     inv_mx_n_eta, inv_mx_n_eta_el, n_eta, n_eta_el, n_lo_eta,
     &     ds(p_eta_v), is(p_eta_ix), is(p_eta_sa),
     &     is(p_lo_eta_pv_in_r), is(p_up_eta_pv_in_r),
     &     inv_pv_tl, inv_wr_eta_tl, inv_c_rlv_tl, inv_unit_v_tl,
     &     inv_l_pv_rsdu_er_tl,
     &     ds(p_tom_inv_rl_wk),
     &     is(p_tom_inv_bs_c_at),
     &     is(p_tom_inv_r_k_a),
     &     is(p_tom_inv_pv_r_n_or_mrt),
     &     ds(p_rsmi_rl_wk_a(rl_wk_a_ix)),
     &     ds, is)
      call ems_fr_rsmi_rl_wk_a_ix(rl_wk_a_ix)
      if (iand(inv_alg_msk, inv_alg_sus) .ne. 0)
     &     is(p_lo_eta_pv_in_r) = sv_sus_fwd_tran_mx_n_op
      if (iand(rt_cod, tom_inv_er_bt) .ne. 0) goto 8990
      if (iand(rt_cod, tom_inv_no_po_bt) .ne. 0) then
c
c     Out of space for eta file
c
         prev_inv_mx_n_eta = inv_mx_n_eta
         prev_inv_mx_n_eta_el = inv_mx_n_eta_el
         inv_mx_n_eta = (inv_mx_n_eta*3)/2
         inv_mx_n_eta_el = (inv_mx_n_eta_el*3)/2
CM      IF (emsol_deb .EQ. 1) THEN
C?         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9410)
C?     &        prev_inv_mx_n_eta, prev_inv_mx_n_eta_el,
C?     &        inv_mx_n_eta, inv_mx_n_eta_el
C?         call ems_msg_wr_li(warn_msg_n)
CM      ENDIF
c         call ems_g_vr_in_r_fm_st(is(p_st), vr_in_r)
c         call ems_ck_vr_in_r_and_st(-1, n_c, mx_n_c, n_r,
         call ems_cp_vr_in_r_and_st(n_c, mx_n_c, n_r,
     &        is(p_tom_inv_og_vr_in_r), vr_in_r,
     &        is(p_tom_inv_og_st), is(p_st))
c
c     Changed 20/03/97
         call ems_mem_mgr_rm_blk(mem_mgr_rt_cod, is,
     &        hdl_eta_grp(hdl_os_blk_n))
         if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
            ems_msg_cod = ems_msg_lvl_serious
            goto 7100
         endif
         go to 100
      end if
CM      IF (emsol_deb .EQ. 1) THEN
C?      if (iand(rt_cod, tom_inv_rpt_bt) .ne. 0) then
C?c
C?c     Repeat INVERT with reporting and singularity mask set according
C?c     to the repeat number (NB repeats due to insufficient space don't
C?c     count as repeats in this sense.)
C?c
C?         tom_inv_rpt_n = tom_inv_rpt_n + 1
C?c
C?c     Make sure that only two repeats can occur
C?c
C?         if (tom_inv_rpt_n .le. 2) then
C?c            call ems_g_vr_in_r_fm_st(is(p_st), vr_in_r)
C?c            call ems_ck_vr_in_r_and_st(-1, n_c, mx_n_c, n_r,
C?            call ems_cp_vr_in_r_and_st(n_c, mx_n_c, n_r,
C?     &        is(p_tom_inv_og_vr_in_r), vr_in_r,
C?     &        is(p_tom_inv_og_st), is(p_st))
C?            go to 100
C?         endif
C?      end if
CM      ENDIF
      if (n_inv_sing .gt. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9400)n_inv_sing
         call ems_msg_wr_li(warn_msg_n)
      endif
c      write(*, '(a, 2i9, 2x, f5.2)')
c     &     '####INVERT####: ', n_eta, n_eta_el
c      write(*, '(a, 2i9, 2x, f5.2)')
c     &     '####INVERT####: ', inv_mx_n_eta, inv_mx_n_eta_el, fill_fac
      n_lo_c_eta = n_lo_eta
      if (n_eta .eq. 0) then
c
c     If a logical basis is passed to tom_inv then there are no etas and
c     the start of the n+1'st is not set so do it now.
c
         is(p_eta_sa+1) = 1
         goto 1000
      endif
c
c     Reset eta_fi_da_st_msk and then determine/calculate available data
c
      eta_fi_da_st_msk = 0
c
c     If a supersparse INVERT has been performed then the forward
c     pointers into the col-wise eta file are known
c
      if (iand(inv_alg_msk, inv_alg_sus) .ne. 0)
     &     eta_fi_da_st_msk = ior(eta_fi_da_st_msk, eta_fi_da_st_fwd_p)
c
c     Determine whether a row-wise eta file should be formed
c
      if (iand(eta_fi_mode_msk, eta_fi_r_eta_y_bt) .ne. 0) then
c
c     If a row-wise eta file should always be formed then do it.
c
         g_r_eta_fi = .true.
      else if (iand(eta_fi_mode_msk, eta_fi_r_eta_poss_bt) .ne. 0) then
c
c     If a row-wise eta file may be formed then do it if the following
c     conditions hold
c
c     fill-in is sufficiently low
c
c     the average density of the btran solution is low enough to make it
c     look worthwhile
c
         g_r_eta_fi = fill_fac .lt. 1.5d0 .and.
     &       av_btran_sol_dse .le. 1.5d0*tl_fwd_tran_dse_rhs
      else
         g_r_eta_fi = .false.
      endif
      if (g_r_eta_fi) then
CM      IF (emsol_tt .EQ. 1) THEN
C?         call ems_tt_rec(g_r_eta_fi_tt, n_bs)
CM      ENDIF
c
c     Set up row-wise eta file
c
         if (iand(rsmi_blk_st_msk, rsmi_blk_st_r_eta_fi_p) .eq. 0) then
            call ems_iz_blk_r_eta_fi_p(is)
            if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
         endif
         call ems_iz_r_eta_fi_pm(is)
         if (iand(rsmi_blk_st_msk, rsmi_blk_st_r_eta_fi_en) .ne. 0)
     &        call ems_rm_blk_r_eta_fi_en(is)
         call ems_g_r_eta_fi_en_p(
     &        alw_f7_wr, ems_msg_wr_cn, er_fd,
     &        n_r, n_eta, n_eta_el, n_lo_c_eta,
     &        n_lo_r_eta, n_lo_r_eta_el,
     &        n_up_r_eta, n_up_r_eta_el,
     &        ds(p_eta_v), is(p_eta_ix), is(p_eta_sa),
     &        is(p_lo_eta_r_sa), is(p_up_eta_r_sa),
     &        is(p_lo_eta_pv_in_r), is(p_up_eta_pv_in_r),
     &        is(p_lo_eta_pv_in_c), is(p_up_eta_pv_in_c),
     &        is(p_tom_inv_bs_c_at), is(p_tom_inv_r_k_a))
         call ems_iz_blk_r_eta_fi_en(is)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
         call ems_g_r_eta_fi_en(
     &        alw_f7_wr, ems_msg_wr_cn, er_fd,
     &        n_r, n_eta, n_eta_el, n_lo_c_eta,
     &        n_lo_r_eta, n_lo_r_eta_el,
     &        n_up_r_eta, n_up_r_eta_el,
     &        ds(p_eta_v), is(p_eta_ix), is(p_eta_sa),
     &        ds(p_lo_eta_c_v), is(p_lo_eta_c_ix), is(p_lo_eta_r_sa),
     &        ds(p_up_eta_c_v), is(p_up_eta_c_ix), is(p_up_eta_r_sa),
     &        is(p_lo_eta_pv_in_r), is(p_up_eta_pv_in_r),
     &        is(p_lo_eta_pv_in_c), is(p_up_eta_pv_in_c),
     &        is(p_tom_inv_bs_c_at), is(p_tom_inv_r_k_a))
CM      IF (emsol_tt .EQ. 1) THEN
C?         call ems_tt_rec(-g_r_eta_fi_tt, n_bs)
CM      ENDIF
         eta_fi_da_st_msk = ior(eta_fi_da_st_msk, eta_fi_da_st_r_eta)
c
c     Otherwise, if backward pointers into the eta file are always to be
c     calculated or they may be calculated and indices are being
c     maintained in (unit) BTRANs then form them
c
      else
c
c     There is no row-wise representation of the eta file so remove the
c     block of pointers
c
         if (iand(rsmi_blk_st_msk, rsmi_blk_st_r_eta_fi_p) .ne. 0)
     &        call ems_rm_blk_r_eta_fi_p(is)
c
c     Possibly switch back to storing the nonzeros in BTRAN---
c     particularly if it has just been deemed uncompetitive to maintain
c     a row-wise representation of the eta file.
c
         if (sto_btran_ix_mode .eq. sto_ix_poss) then
            if (av_btran_sol_dse .gt. 1.5d0*tl_bwd_tran_dse_rhs) then
               sto_btran_ix = sto_ix_no
            else
               sto_btran_ix = sto_ix_y
            endif
         endif
         if (iand(eta_fi_mode_msk, eta_fi_bwd_p_y_bt) .ne. 0 .or.
     &        (iand(eta_fi_mode_msk, eta_fi_bwd_p_poss_bt) .ne. 0 .and.
     &        sto_btran_ix .eq. sto_ix_y)) then
CM      IF (emsol_tt .EQ. 1) THEN
C?            call ems_tt_rec(g_bwd_tran_c_eta_p_tt, n_bs)
CM      ENDIF
            call ems_g_bwd_tran_c_eta_p(
     &           n_r, n_eta, n_eta_el,
     &           is(p_eta_ix), is(p_eta_sa),
     &           is(p_eta_w_l_en_in_r), is(p_eta_w_lm1_en_in_r))
CM      IF (emsol_tt .EQ. 1) THEN
C?            call ems_tt_rec(-g_bwd_tran_c_eta_p_tt, n_bs)
CM      ENDIF
            eta_fi_da_st_msk = ior(eta_fi_da_st_msk, eta_fi_da_st_bwd_p)
         endif
      endif
 1000 continue
      call ems_g_inv_eta_rec(
     &     inv_mx_n_eta,
     &     n_eta, n_eta_el,
     &     is(p_eta_grp),
     &     ds(p_eta_v),
     &     is(p_eta_ix),
     &     is(p_eta_rec))
CM      IF (emsol_da .EQ. 1) THEN
C?c      if (n_r .gt. 40000)
C?c     &     call ems_wr_eta_grp_n_wo(is(p_eta_grp), is)
CM      ENDIF
c
c     Assign a value to the n_eta_el+1'st entry to avoid an unassigned
c     variable violation for the assembler ftran.
c
      is(p_eta_ix+n_eta_el+1) = 0
c      call ems_ck_sus_btran_da(
c     &     n_r, n_eta, n_eta_el,
c     &     is(p_eta_ix), is(p_eta_sa),
c     &     is(p_eta_w_l_en_in_r), is(p_eta_w_lm1_en_in_r),
c     &     is(p_tom_inv_bs_c_at), is(p_tom_inv_r_k_a))
c      call ems_ck_sus_ftran_da(
c     &     n_r, n_eta, n_eta_el,
c     &     is(p_eta_ix), is(p_eta_sa),
c     &     is(p_lo_eta_pv_in_r), is(p_up_eta_pv_in_r),
c     &     is(p_tom_inv_bs_c_at), is(p_tom_inv_r_k_a))
 7000 continue
 7100 continue
      call ems_mem_mgr_rm_blk(mem_mgr_rt_cod, is,
     &     tom_inv_wk_blk)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         goto 7200
      endif
 7200 continue
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_inv_lvl1) call ems_tt_rec(-tom_inv_tt, n_bs)
CM      ENDIF
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)n_si_it
      call ems_msg_wr_li(serious_msg_n)
      call ems_mem_mgr_g_rq_is_n_en(mem_mgr_rt_cod, is,
     &     n_wo, cu_rq_is_n_en, mx_rq_is_n_en)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 7100
      call ems_mem_mgr_rp_rq_ws_n_en(mem_mgr_rt_cod, is, -1,
     &     cu_rq_is_n_en, mx_rq_is_n_en, rl_wo_z, 'double precision')
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 7100
      cu_is_n_en = is(ix_l_mgr_p) + 1
      is(ix_n_xa_i_wo_rq) = max(cu_rq_is_n_en-cu_is_n_en, 1)
      goto 7100
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 8800 continue
      ems_msg_cod = ems_msg_lvl_serious
      goto 7100
 8990 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9899)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9400 format('INVERT has discovered ', i7, ' singularities')
c 9410 format('INVERT did not have enough space for required etas: ',
c     &     'increasing mx_n_eta(_el) ',
c     &     ' from ', i5, '(', i7, ')',
c     &     ' to ', i5, '(', i7, ')')
 9800 format('Iteration ', i7,
     &     ': Insufficient space for INVERT')
 9801 format('RSMI workspace not available in ems_ca_tom_inv')
 9899 format('SLAP bug: Logical error in INVERT')
      end
 
C->>> ---------------------------------------> ems_cp_vr_in_r_and_st <<<
c     Copy from one vr_in_r/st to another
c
      subroutine ems_cp_vr_in_r_and_st(n_c, mx_n_c, n_r,
     &     fm_vr_in_r, t_vr_in_r,
     &     fm_st, t_st)
      implicit none
      integer n_c, mx_n_c, n_r
      integer fm_vr_in_r(0:n_r), t_vr_in_r(0:n_r)
      integer fm_st(0:mx_n_c+n_r), t_st(0:mx_n_c+n_r)
      integer r_n, c_n
 
      do 10, c_n = 1, n_c
         t_st(c_n) = fm_st(c_n)
 10   continue
      do 20, r_n = 1, n_r
         t_st(mx_n_c+r_n) = fm_st(mx_n_c+r_n)
         t_vr_in_r(r_n) = fm_vr_in_r(r_n)
 20   continue
      return
      end
CM      IF (emsol_dev .EQ. 1) THEN
C?C->>> -------------------------------------> ems_ck_vr_in_r_and_st <<<
C?c     Check one t_vr_in_r/st against fm_vr_in_r/st, correcting it if
C?c     necessary and reporting if desired.
C?c
C?      subroutine ems_ck_vr_in_r_and_st(rp_cn, n_c, mx_n_c, n_r,
C?     &     fm_vr_in_r, t_vr_in_r,
C?     &     fm_st, t_st)
C?      implicit none
C?      integer rp_cn, n_c, mx_n_c, n_r
C?      integer fm_vr_in_r(0:n_r), t_vr_in_r(0:n_r)
C?      integer fm_st(0:mx_n_c+n_r), t_st(0:mx_n_c+n_r)
C?      integer r_n, c_n
C?
C?      do 10, c_n = 1, n_c
C?         if (t_st(c_n) .ne. fm_st(c_n)) then
C?            if (rp_cn .ge. 0) write(rp_cn, 9000)'st',
C?     &           c_n, t_st(c_n), fm_st(c_n)
C?            t_st(c_n) = fm_st(c_n)
C?         endif
C? 10   continue
C?      do 20, r_n = 1, n_r
C?         if (t_st(mx_n_c+r_n) .ne. fm_st(mx_n_c+r_n)) then
C?            if (rp_cn .ge. 0) write(rp_cn, 9000)'st',
C?     &           mx_n_c+r_n, t_st(mx_n_c+r_n), fm_st(mx_n_c+r_n)
C?            t_st(mx_n_c+r_n) = fm_st(mx_n_c+r_n)
C?         endif
C?         if (t_vr_in_r(r_n) .ne. fm_vr_in_r(r_n)) then
C?            if (rp_cn .ge. 0) write(rp_cn, 9000)'vr_in_r',
C?     &           r_n, t_vr_in_r(r_n), fm_vr_in_r(r_n)
C?            t_vr_in_r(r_n) = fm_vr_in_r(r_n)
C?         endif
C? 20   continue
C?      return
C? 9000 format(a8, ': Entry ', i7, ' is ', i9, ' not ', i9)
C?      end
CM      ENDIF
C->>> ---------------------------------------> ems_iz_blk_tom_inv_wk <<<
c     Allocates the workspace for the Tomlin INVERT.
c
      subroutine ems_iz_blk_tom_inv_wk(mx_n_c, n_r, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'TOMHDL.INC'
      include 'EMSMSG.INC'
      integer mx_n_c, n_r, is(0:*)
      integer mem_mgr_rt_cod
      integer n_wo
      integer r_cf, c_cf, a_el_cf, cs
 
      call ems_g_blk_tom_inv_wk_n_wo(n_r, mx_n_c,
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
c      n_wo = r_cf*n_r + c_cf*mx_n_c + cs
c      n_wo = r_cf*n_r + c_cf*mx_n_c + a_el_cf*n_a_el + cs
c      n_wo = rl_wo_z*(1+n_r) + 4*i_wo_z*(1+n_r) + i_wo_z*(1+mx_n_c+n_r)
      call ems_mem_mgr_ope_blk(mem_mgr_rt_cod, is,
     &     n_wo, ope_blk_at_l_fr_p,
     &     0, tom_inv_wk_blk_id, tom_inv_wk_blk)
      if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
         if (mem_mgr_rt_cod .eq. mem_mgr_rt_cod_serious_no_po) then
            ems_msg_cod = ems_msg_lvl_serious
            go to 7000
         else if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
             ems_msg_cod = ems_msg_lvl_serious
            go to 7000
         endif
      endif
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     tom_inv_wk_blk, 1+n_r, rl_wo_z,
     &     hdl_tom_inv_rl_wk)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     tom_inv_wk_blk, 1+n_r, i_wo_z,
     &     hdl_tom_inv_bs_c_at)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     tom_inv_wk_blk, 1+n_r, i_wo_z,
     &     hdl_tom_inv_r_k_a)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     tom_inv_wk_blk, 1+n_r, i_wo_z,
     &     hdl_tom_inv_pv_r_n_or_mrt)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     tom_inv_wk_blk, 1+n_r, i_wo_z,
     &     hdl_tom_inv_og_vr_in_r)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     tom_inv_wk_blk, 1+mx_n_c+n_r, i_wo_z,
     &     hdl_tom_inv_og_st)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
 7000 continue
      return
      end
 
C->>> -----------------------------------> ems_g_blk_tom_inv_wk_n_wo <<<
      subroutine ems_g_blk_tom_inv_wk_n_wo(n_r, n_c,
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      implicit none
      include 'EMSV.INC'
      integer n_r, n_c, r_cf, c_cf, a_el_cf, cs, n_wo
      r_cf =     rl_wo_z + 5*i_wo_z
      c_cf =                 i_wo_z
      a_el_cf = 0
      cs =       rl_wo_z + 5*i_wo_z
      n_wo = r_cf*n_r + c_cf*n_c + cs
      return
      end
 
C->>> -----------------------------------------> ems_g_vr_in_r_fm_st <<<
      subroutine ems_g_vr_in_r_fm_st(st, vr_in_r)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICS.INC'
      include 'ICTVR.INC'
      integer st(0:mx_n_c+n_r), vr_in_r(0:n_r)
      integer k, i
 
      i = 0
      do k = 1, n_c
         if (iand(st(k), bc_bt) .ne. 0) then
            i = i + 1
            vr_in_r(i) = k
         end if
      end do
 
      do k = mx_n_c + 1, mx_n_c + n_r
         if (iand(st(k), bc_bt) .ne. 0) then
            i = i + 1
            vr_in_r(i) = k
         end if
      end do
      end
 
