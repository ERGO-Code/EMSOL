CM
C->>> ----------------------------------------------------> ems_rsmi <<<
c
c     This routine solves the bounded LP problem
c
c     minimize   mx_mn*c^x
c
c     subject to l < = [ x] < = u
c                      [Ax]
c
c     where x and c are n-vectors, and A is an n*m matrix.
c
c     Parameter list
c
c     ds        double precision workspace | Equivalenced
c     is        integer workspace          |
c
      subroutine ems_rsmi(ds, is, lp_iz_mode)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMICS.INC'
      include 'RSMIHDL.INC'
      include 'RSMICOM.INC'
      include 'MORSMI.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
c      include 'CHCTVR.INC'      !Only needed when dumping basis
      include 'EMSMSG.INC'
      include 'EMSMSGN.INC'
      include 'ITXITCS.INC'
c      include 'SVMVBD.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
CM      IF (emsol_xa .EQ. 1) THEN
C?c      include 'CRASH.INC'
CM      ENDIF
c      include 'TSRGACT.INC'
      include 'EMSMMGRI.INC'
      include 'EMSRTCOD.INC'
      integer ems_rt_cod
      integer is(0:is_n_en_m1)
      double precision ds(0:ds_n_en_m1)
      integer lp_iz_mode
      integer ems_pos_mod
      logical ems_du_act_atr
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_eq_i2
CM      ENDIF
      logical du_act_sgn_ok, re_re_pc
      logical u_du_act, u_ed_wt, re_pc
      logical rp_growth, refine_pv_c, refined_pv_c
      integer reset_rsmi_n_si_it
      integer it_xit_reason, usr_rt_cod
      integer usr_msg_fq, usr_rsmi_msg_msk, usr_wr_lp_da, usr_ck_msk
CM      IF (emsol_dev .EQ. 1) THEN
C?      integer du_act_er_why
CM      ENDIF
      logical du_act_atr
      double precision alt_du_act, du_act_er
      double precision mx_ed_wt_er, mx_du_act_er, mx_pv_er
      double precision norm_rsdu
      integer ftran_sol_n_en
      integer rhs_sgn
 
      integer ems_i_t_i_pct
      integer pct
      integer poss_sps_sto_ftran_ix_y_n_it
      integer poss_sps_sto_btran_ix_y_n_it
      integer poss_sps_sto_tbu_r_ix_y_n_it
      integer poss_sps_fwd_eta_p_n_inv
      integer poss_sps_r_eta_fi_n_inv
      integer poss_sps_bwd_eta_p_n_inv
      integer poss_sps_n_inv
      integer poss_sps_n_si_it0
 
      logical reset_loop
c      integer r_n
c      logical er_fd
c      integer ca_ems_rt_cod
 
c      integer ems_ln_t_l_ch
c      integer ml_nm_ln
      save usr_msg_fq, usr_rsmi_msg_msk, usr_wr_lp_da, usr_ck_msk
 
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(iz_bt_a_com_fg1, iz_bt_a_com_fg2))
CM      ELSE
      if (iz_bt_a_com_fg1 .eq. iz_bt_a_com_fg2)
CM      ENDIF
     &      call ems_iz_bt_a_com
CM      IF (emsol_dev .EQ. 1) THEN
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(ca_iz_mo_rsmi_fg1, ca_iz_mo_rsmi_fg2))
CM      ELSE
C?      if (ca_iz_mo_rsmi_fg1 .eq. ca_iz_mo_rsmi_fg2)
CM      ENDIF
C?     &     call ems_iz_mo_rsmi(11, 2, 0)
CM      ENDIF
 
      ems_rt_cod = ems_rt_cod_ok
      bd_swp = .false.
      fresh_pc = .false.
      re_pc = .false.
      u_du_act = .false.
      u_ed_wt = .false.
      cg_pr_wt = .false.
      rp_growth = .false.
c      n_sv_mv_bd = 0
      pc_alg = dvx_mode
      mx_ed_wt_er = tl_ed_wt_er
      mx_du_act_er = tl_du_act_er
      mx_pv_er = tl_pv_er
      if (lp_iz_mode .eq. 1) then
         if (sto_btran_ix_mode .eq. sto_ix_y) then
            sto_btran_ix = sto_ix_y
         else if (sto_btran_ix_mode .eq. sto_ix_no) then
            sto_btran_ix = sto_ix_no
         else
            sto_btran_ix = sto_ix_y
         endif
         if (sto_ftran_ix_mode .eq. sto_ix_y) then
            sto_ftran_ix = sto_ix_y
         else if (sto_ftran_ix_mode .eq. sto_ix_no) then
            sto_ftran_ix = sto_ix_no
         else
            sto_ftran_ix = sto_ix_y
         endif
      endif
c
c     Report on the initial pricing strategy
c
      if (iand(rsmi_msg_msk, rsmi_pc_li_bt) .ne. 0) then
         if (pc_alg .eq. pc_alg_dan) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &           'Dantzig'
         else if (pc_alg .eq. pc_alg_approx_dvx .or.
     &           pc_alg .eq. pc_alg_exact_dvx) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &           'Devex'
         else if (pc_alg .eq. pc_alg_sed) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &           'steepest edge'
         endif
         call ems_msg_wr_li(info_msg_n)
      endif
      reset_rsmi_n_si_it = n_si_it - 1
CM      IF (emsol_da .EQ. 1) THEN
C?c
C?c     Keep a copy of the original basis to determine how good it was
C?c
C?      call ems_rsmi_da(1, ds, is)
C?      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
C?c      ml_nm_ln = ems_ln_t_l_ch(ch_ml_nm)
C?c      open(unit = 7, file = ch_ml_nm(1:ml_nm_ln)//'.sps_da')
C?      open(unit = 7, file = 'ml.sps_da')
C?      write(7, 9370)
CM      ENDIF
c=======================================================================
c     Start of major iteration: Sections
c     CHUZC
c     (M)FTRAN
c     CHUZR
c     UPDATE
c     INVERT
c     PRICE
c
CM      IF (emsol_dev .EQ. 1) THEN
C?      if (ts_parsmi .gt. 0) call ems_ck_parsmi_v(0, ds, is)
CM      ENDIF
      poss_sps_sto_ftran_ix_y_n_it = 0
      poss_sps_sto_btran_ix_y_n_it = 0
      poss_sps_sto_tbu_r_ix_y_n_it = 0
      poss_sps_fwd_eta_p_n_inv = 0
      poss_sps_r_eta_fi_n_inv = 0
      poss_sps_bwd_eta_p_n_inv = 0
      poss_sps_n_si_it0 = n_si_it
      poss_sps_n_inv = 1
      if (iand(eta_fi_da_st_msk, eta_fi_da_st_fwd_p) .ne. 0)
     &     poss_sps_fwd_eta_p_n_inv = poss_sps_fwd_eta_p_n_inv + 1
      if (iand(eta_fi_da_st_msk, eta_fi_da_st_r_eta) .ne. 0)
     &     poss_sps_r_eta_fi_n_inv = poss_sps_r_eta_fi_n_inv + 1
      if (iand(eta_fi_da_st_msk, eta_fi_da_st_bwd_p) .ne. 0)
     &     poss_sps_bwd_eta_p_n_inv = poss_sps_bwd_eta_p_n_inv + 1
 1000 continue
CM      IF (emsol_da .EQ. 1) THEN
C?      call ems_rsmi_da(2, ds, is)
C?      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
CM      ENDIF
CM      IF (emsol_deb .EQ. 1) THEN
C?c      print*, 'Checking vr_in_r: n_si_it = ', n_si_it
C?c      call ems_ck_rpt_vr(er_fd, n_r, mx_n_c+n_r, is(p_vr_in_r+1))
C?c      if (er_fd) goto 8800
CM      ENDIF
 
      if (it_o_sw_fm_wr_df .gt. 0) then
         if (n_si_it .eq. 0) then
            usr_msg_fq = msg_fq
            usr_rsmi_msg_msk = rsmi_msg_msk
            usr_wr_lp_da = wr_lp_da
            msg_fq = i_ct_vr_df(ix_msg_fq)
            rsmi_msg_msk = i_ct_vr_df(ix_rsmi_msg_msk)
            wr_lp_da = i_ct_vr_df(ix_wr_lp_da)
         else if (n_si_it .eq. it_o_sw_fm_wr_df) then
            msg_fq = usr_msg_fq
            rsmi_msg_msk = usr_rsmi_msg_msk
            wr_lp_da = usr_wr_lp_da
         endif
      endif
      if (it_o_sw_fm_ck_df .gt. 0) then
         if (n_si_it .eq. 0) then
            usr_ck_msk = ck_msk
            ck_msk = i_ct_vr_df(ix_ck_msk)
         else if (n_si_it .eq. it_o_sw_fm_ck_df) then
            ck_msk = usr_ck_msk
         endif
      endif
      if (ck_msk .ne. 0) call ems_ck_lp_da(ds, is)
c=======================================================================
c     CHUZC section
c
      call ems_rsmi_cz_c_sn(ds, is)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
CM      IF (emsol_dev .EQ. 1) THEN
C?      if (ts_parsmi .gt. 0) call ems_ck_parsmi_v(1, ds, is)
C?      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
CM      ENDIF
c      if (ts_rg_act_en_vr .ne. 0) call ems_ts_rg_act_rn(1, ds, is)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
c
c     Allow a user exit at this point.
c
c     The dual activities may change because
c     *   the user has tightened bounds and the current vertex is not
c     .   feasible;
c     *   the user has changed costs.
c
c     The problem may no longer be optimal because the user has
c     relaxed bounds.
c
c     In either event, the rsmi_op_st_cz_c bit is un-set. Thand the
c     fact that it_xit_reason.eq.it_xit_af_cz_c, ensures that cz_1_c is
c     called within it_xit and a new value is assigned to vr_t_en_bs
c
      if (ems_pos_mod(n_si_it, it_usr_xit_fq) .eq. 0) then
         it_xit_reason = it_xit_af_cz_c
         call ems_it_xit(ds, is, it_xit_reason, usr_rt_cod)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
         if (usr_rt_cod .eq. 3) then
            prob_st = prob_st_mx_n_it
            rq_reset = rq_reset_no_rq_reset
            go to 3000
         else if (usr_rt_cod .eq. 99) then
            reset_rsmi_n_si_it = -1
            rq_reset = rq_reset_usr_rq_reset
            go to 3000
         end if
      endif
c
c     Check that CHUZC has been performed with the current reduced costs
c     and bounds on non-basic variables.
c
      if (iand(rsmi_op_st_msk, rsmi_op_st_cz_c) .eq. 0) goto 8000
      rsmi_op_st_msk = rsmi_op_st_msk - rsmi_op_st_cz_c
c
c     If optimality/infeasibility suspected then reset RSMI.
c
      if (vr_t_en_bs .eq. 0) then
         if (lp_ph .eq. 1 .and. pr_wt .gt. zero) then
c            pr_wt = pr_wt*1d-1
c            if (pr_wt .lt. rl_ct_vr_lb(ix_pr_wt)) pr_wt = zero
            pr_wt = zero
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9210)pr_wt
            call ems_msg_wr_li(info_msg_n)
            pr_co_mu = mx_mn*pr_wt
            call ems_se_bc_co(
     &           is(p_st),
     &           ds(p_rsmi_co),
     &           is(p_vr_in_r),
     &           ds(p_bc_co_v),
     &           is(p_bc_co_ix),
     &           is(p_bc_co_ix_bar))
            rq_re_pc = rq_re_pc_pr_wt_cg
            fresh_pc = .true.
            cg_pr_wt = .true.
            goto 1500
         else
            prob_st = prob_st_op
            rq_reset = rq_reset_op
            go to 3000
         end if
      endif
      du_act_o_vr_t_en_bs = ds(p_du_act+vr_t_en_bs)
      if (du_act_o_vr_t_en_bs .lt. zero) then
         mv_dir =  1
      else
         mv_dir = -1
      end if
c
c     End of CHUZC section
c=======================================================================
c     FTRAN section
c
 1200 continue
      call ems_rsmi_ftran_sn(refined_pv_c, ds, is)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
c
c     End of FTRAN section
c=======================================================================
c     CK_DU_ACT_O_VR_T_EN_BS section
c
c     Determine whether the dual activity of the variable to enter the
c     basis has the right sign and, in doing so, determine the error in
c     the dual activity.
c
      rhs_sgn = 1
      du_act_o_vr_t_en_bs = ds(p_du_act+vr_t_en_bs)
c      if (ts_rg_act_en_vr .ne. 0) go to 1225
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tt_da .gt. 0) call ems_tt_rec(ck_du_act_tt, n_bs)
CM      ENDIF
      call ems_g_alt_du_act(
     &     rhs_sgn, vr_t_en_bs,
     &     is(p_st),
     &     is(p_vr_in_r),
     &     ds(p_rsmi_co),
     &     ds(p_bc_co_v),
     &     is(p_bc_co_ix),
     &     ds(p_pv_c_v),
     &     du_act_o_vr_t_en_bs, alt_du_act, ph_1_du_act_o_vr_t_en_bs)
      if (abs(du_act_o_vr_t_en_bs) .gt. tl_du_ifs) then
         du_act_er = abs((du_act_o_vr_t_en_bs-alt_du_act)/
     &        du_act_o_vr_t_en_bs)
      else
         du_act_er = abs(du_act_o_vr_t_en_bs-alt_du_act)
      end if
c      if (du_act_er .gt. 1d-4) print*, n_si_it,
c     &     ' du_act_er = ', du_act_er
      du_act_sgn_ok = du_act_o_vr_t_en_bs*alt_du_act .gt. zero
c
c     Monitor wrong signs and increasing dual activity errors
c
      if (.not. du_act_sgn_ok .or. du_act_er .gt. mx_du_act_er) then
         mx_du_act_er = max(du_act_er, mx_du_act_er)
CM      IF (emsol_dev .EQ. 1) THEN
C?         if (du_act_sgn_ok) then
C?            du_act_er_why = du_act_er_why_er
C?         else
C?            du_act_er_why = du_act_er_why_wg_sgn
C?         endif
C?         call ems_mo_rsmi_du_act_er(
C?     &        n_si_it, du_act_er_why,
C?     &        du_act_o_vr_t_en_bs, alt_du_act, du_act_er)
CM      ENDIF
      end if
c
c     Determine the action corresponding to the sign/value error.
c
      call ems_an_du_act_er(re_pc, re_re_pc,
     &     du_act_o_vr_t_en_bs, alt_du_act, du_act_sgn_ok, du_act_er)
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tt_da .gt. 0) call ems_tt_rec(-ck_du_act_tt, n_bs)
CM      ENDIF
      if (re_re_pc) then
         rq_reset = rq_reset_re_re_pc
         goto 3000
      endif
      if (re_pc) then
         if (du_act_sgn_ok) then
            rq_re_pc = rq_re_pc_du_act_er
         else
            rq_re_pc = rq_re_pc_du_act_wg_sgn
         endif
      endif
      if (re_pc .or. rq_inv .ne. rq_inv_no_rq_inv) then
c
c     The dual activity has the wrong sign or numerical problems have
c     been detected. Abandon this iteration so zero the RHS used for
c     FTRAN.
c
         call ems_ze_pv_c_v(ds(p_pv_c_v), is(p_nw_eta_ix))
         if (re_pc) then
c nw_sgn_er_bug            mx_du_act_er = tl_du_act_er
            go to 1500
         endif
c
c     Set re_pc for u_pc=0 now or set re_pc in ems_an_du_act_er
c     Don't re-price for u_pc=0 if fresh INVERT . Maybe jump on
c     rq_inv .ne. rq_inv_no_rq_inv
c
         go to 1400
      end if
c
c     Use the dual activity which was calculated to test the sign rather
c     than the original value.
c
      ds(p_du_act+vr_t_en_bs) = alt_du_act
      du_act_o_vr_t_en_bs = alt_du_act
c 1225 continue
c
c     Indicate the direction in which the entering variable is moving.
c     Hereafter du_act_o_vr_t_en_bs is only used by wr_rsmi_lg_li and
c     L1-CHUZR.
c
      if (lp_ph .eq. 1) then
         if (en_vr_bk_bd .lt. 0) then
c
c     The entering variable is breaking through its lower bound or
c     moving below a fixed value.
c     Modify the dual activity and phase I dual activity accordingly and
c     test for phase I attractiveness.
c
            du_act_o_vr_t_en_bs = du_act_o_vr_t_en_bs - one
            ph_1_du_act_o_vr_t_en_bs = ph_1_du_act_o_vr_t_en_bs - one
            du_act_atr = ph_1_du_act_o_vr_t_en_bs .gt. tl_du_ifs
         else if (en_vr_bk_bd .gt. 0) then
c
c     The entering variable is breaking through its upper bound or
c     moving above a fixed value.
c     Modify the dual activity and phase I dual activity accordingly and
c     test for phase I attractiveness.
c
            du_act_o_vr_t_en_bs = du_act_o_vr_t_en_bs + one
            ph_1_du_act_o_vr_t_en_bs = ph_1_du_act_o_vr_t_en_bs + one
            du_act_atr = ph_1_du_act_o_vr_t_en_bs .lt. -tl_du_ifs
         else
            du_act_atr = ems_du_act_atr(is(p_st+vr_t_en_bs),
     &           ph_1_du_act_o_vr_t_en_bs, tl_du_ifs)
         endif
         if (du_act_atr) then
            cg_pr_wt = .false.
            if (ph_1_du_act_o_vr_t_en_bs .lt. zero) then
               mv_dir =  1
            else
               mv_dir = -1
            end if
c
c     If the entering variable is violating a bound then remove the
c     feasibility bit. This is only done temporarily so that it is
c     treated correctly in CHUZR. It does not actually become infeasible
c     unless a sufficiently large step is made.
c
            if (en_vr_bk_bd .ne. 0)
     &           is(p_st+vr_t_en_bs) = is(p_st+vr_t_en_bs) + ifs_bt
         else
c
c     The variable is not attractive in the pure phase 1 sense. This
c     indicates that the primal wright is too large.
c
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9200)
     &           ph_1_du_act_o_vr_t_en_bs
            call ems_msg_wr_li(info_msg_n)
            if (pr_wt .gt. zero) then
               pr_wt = pr_wt*1d-1
               if (pr_wt .lt. rl_ct_vr_lb(ix_pr_wt)) pr_wt = zero
               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9210)pr_wt
               call ems_msg_wr_li(info_msg_n)
               pr_co_mu = mx_mn*pr_wt
               call ems_se_bc_co(
     &              is(p_st),
     &              ds(p_rsmi_co),
     &              is(p_vr_in_r),
     &              ds(p_bc_co_v),
     &              is(p_bc_co_ix),
     &              is(p_bc_co_ix_bar))
               if (pc_alg .eq. pc_alg_sed)
     &              call ems_ze_pv_c_v(ds(p_pv_c_v), is(p_nw_eta_ix))
               rq_re_pc = rq_re_pc_pr_wt_cg
               fresh_pc = .true.
               cg_pr_wt = .true.
               goto 1500
            else
               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9220)
               call ems_msg_wr_li(er_msg_n)
               rq_reset = rq_reset_ze_pr_wr
               goto 3000
c               call ems_msg_wr_li(bug_msg_n)
c               goto 7000
            endif
         endif
      else
         if (du_act_o_vr_t_en_bs .lt. zero) then
            mv_dir =  1
         else
            mv_dir = -1
         end if
      endif
CM      IF (emsol_dev .EQ. 1) THEN
C?      if (ts_parsmi .gt. 0) call ems_ck_parsmi_v(6, ds, is)
CM      ENDIF
c
c     End of CK_DU_ACT_O_VR_T_EN_BS section
c=======================================================================
c     CHUZR section
c
c     Find vr_t_lv_bs which, if equal to vr_t_en_bs, indicates a bound
c     swap or detect unboundedness.
c
 1250 continue
      call ems_cz_r(
     &     rp_growth, refined_pv_c, refine_pv_c, mx_ed_wt_er,
     &     ds(p_rsmi_lb),
     &     ds(p_rsmi_ub),
     &     ds(p_pr_act),
     &     is(p_st),
     &     is(p_vr_in_r),
     &     ds(p_nw_eta_v),
     &     ds(p_pv_c_v),
     &     is(p_nw_eta_ix),
     &     is(p_cz_r_cdd_ix),
     &     ds, is)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      if (ck_msk .gt. 0) then
         call ems_ck_ftran(
     &        rhs_sgn,
     &        ds(p_nw_eta_v),
     &        ds(p_pv_c_v),
     &        is(p_nw_eta_ix),
     &        norm_rsdu,
     &        ds, is)
         if (norm_rsdu .gt. 1d-4) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9030)
     &           n_si_it+1, norm_rsdu
            call ems_msg_wr_li(info_msg_n)
         endif
      endif
      if (refine_pv_c) then
         call ems_refine_pv_c(
     &        rhs_sgn,
     &        ds(p_nw_eta_v),
     &        ds(p_pv_c_v),
     &        is(p_nw_eta_ix),
     &        is(p_vr_in_r),
     &        ds, is)
         refined_pv_c = .true.
         goto 1250
      endif
c      if (ts_rg_act_en_vr .ne. 0) then
c         call ems_ts_rg_act_rn(2, ds, is)
c         goto 7000
c      endif
      if (rq_inv .eq. rq_inv_u_growth) then
c
c     Reinvert and recalculate the pivotal column. Note that this event
c     can only occur if at least one UPDATE has been performed so an
c     infinite loop cannot be caused.
c
         if (pc_alg .eq. pc_alg_sed)
     &        call ems_ze_pv_c_v(ds(p_pv_c_v), is(p_nw_eta_ix))
         call ems_rsmi_inv(ds, is)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
         if (n_inv_sing .gt. 0) then
            rq_reset = rq_reset_sing_bs
            goto 3000
         endif
         poss_sps_n_inv = poss_sps_n_inv + 1
         if (iand(eta_fi_da_st_msk, eta_fi_da_st_fwd_p) .ne. 0)
     &        poss_sps_fwd_eta_p_n_inv = poss_sps_fwd_eta_p_n_inv + 1
         if (iand(eta_fi_da_st_msk, eta_fi_da_st_r_eta) .ne. 0)
     &        poss_sps_r_eta_fi_n_inv = poss_sps_r_eta_fi_n_inv + 1
         if (iand(eta_fi_da_st_msk, eta_fi_da_st_bwd_p) .ne. 0)
     &        poss_sps_bwd_eta_p_n_inv = poss_sps_bwd_eta_p_n_inv + 1
c
c     Maybe re-form the vector of basic costs.
c
         if (iand(inv_alg_msk, inv_alg_perm) .eq. 0)
     &        call ems_se_bc_co(
     &        is(p_st), ds(p_rsmi_co), is(p_vr_in_r),
     &        ds(p_bc_co_v), is(p_bc_co_ix), is(p_bc_co_ix_bar))
         go to 1200
      else if (alg_er) then
         prob_st = prob_st_unknown
         rq_reset = rq_reset_cz_r_alg_er
         go to 3000
      else if (un_bd) then
         prob_st = prob_st_unbd
         if (iand(ml_blk_st_msk, ml_blk_st_ml_aux_sol) .eq. 0) then
            call ems_iz_blk_ml_aux_sol(is)
            if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
         endif
         call ems_un_bd_aux_sol(
     &        is(p_vr_in_r),
     &        ds(p_nw_eta_v),
     &        is(p_nw_eta_ix),
     &        ds(p_r_aux_sol),
     &        ds(p_c_aux_sol))
         rq_reset = rq_reset_unbd
         go to 3000
      end if
c
c     Reinstate the feasibility bit for the entering variable if it was
c     attractive for it to break a bound or move from a fixed value.
c
      if (en_vr_bk_bd .ne. 0)
     &     is(p_st+vr_t_en_bs) = is(p_st+vr_t_en_bs) - ifs_bt
c
c     When using exact steepest edge weights the full length vector of
c     pivotal column values is not zeroed because it needs to be
c     BTRANned (unless there has been a bound swap, in which case it is
c     just zeroed now).
c
      if (pc_alg .eq. pc_alg_sed) then
         if (vr_t_lv_bs .eq. vr_t_en_bs) then
            call ems_ze_pv_c_v(ds(p_pv_c_v), is(p_nw_eta_ix))
         else
            call ems_btran(ds(p_pv_c_v), n_r+1, ds, is)
c
c     Calling perm_btran_sol may result in the vector pointed to by
c     hdl_pv_c_v being zeroed and swapped with the handle pointing to
c     another vector of the same length.
c
            if (iand(inv_alg_msk, inv_alg_perm) .ne. 0)
     &           call ems_perm_btran_sol(
     &           p_pv_c_v, hdl_pv_c_v, n_r+1, ds, is)
         end if
      end if
      bd_swp = vr_t_lv_bs .eq. vr_t_en_bs
      pv = rhs_sgn*ds(p_nw_eta_v)
      if (sto_ftran_ix_mode .eq. sto_ix_poss) then
         ftran_sol_n_en = nw_eta_l_ix - nw_eta_f_ix + 1
         ftran_sol_dse = float(ftran_sol_n_en)/float(n_r)
      endif
c
c     Write out a log line.
c
      if ((iand(rsmi_msg_msk, rsmi_li_bt) .ne. 0 .or.
     &     iand(rsmi_msg_msk, rsmi_pv_li_bt) .ne. 0) .and.
     &     ems_pos_mod(n_si_it, msg_fq) .eq. 0) then
         call ems_wr_rsmi_lg_li(rsmi_lg_li_mode_fq, ds, is)
      endif
CM      IF (emsol_da .EQ. 1) THEN
C?      call ems_rsmi_da(3, ds, is)
C?      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
CM      ENDIF
      if (nw_dvx_fwk) then
         if (n_dvx_fwk .gt. 0 .and.
     &        iand(rsmi_msg_msk, rsmi_dvx_li_bt) .ne. 0)
     &        call ems_wr_rsmi_lg_li(rsmi_lg_li_mode_dvx, ds, is)
         call ems_iz_dvx_fwk(
     &        is(p_vr_in_r),
     &        is(p_vr_in_c),
     &        ds(p_ed_wt),
     &        is(p_dvx_ix))
         nw_dvx_fwk = .false.
      endif
c
c     End of CHUZR section
c=======================================================================
c     UPDATE section
c
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_u_lvl0) call ems_tt_rec(al_u_tt, n_bs)
CM      ENDIF
      call ems_rsmi_u_sn(ds, is)
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_u_lvl0) call ems_tt_rec(-al_u_tt, n_bs)
CM      ENDIF
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      if (bd_swp) then
c
c     Following a bound swap go to the end of the major iteration unless
c     it has resulted in (phase 1) basic cost changes.
c
         if (is(p_pi_ix) .gt. 0 .or. fresh_pc) goto 1500
         go to 2000
      end if
c
c     End of UPDATE section.
c=======================================================================
c     INVERT section
c
 1400 continue
      if (rq_inv .ne. rq_inv_no_rq_inv) then
         call ems_rsmi_inv(ds, is)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
         if (n_inv_sing .gt. 0) then
            rq_reset = rq_reset_sing_bs
            goto 3000
         endif
         poss_sps_n_inv = poss_sps_n_inv + 1
         if (iand(eta_fi_da_st_msk, eta_fi_da_st_fwd_p) .ne. 0)
     &        poss_sps_fwd_eta_p_n_inv = poss_sps_fwd_eta_p_n_inv + 1
         if (iand(eta_fi_da_st_msk, eta_fi_da_st_r_eta) .ne. 0)
     &        poss_sps_r_eta_fi_n_inv = poss_sps_r_eta_fi_n_inv + 1
         if (iand(eta_fi_da_st_msk, eta_fi_da_st_bwd_p) .ne. 0)
     &        poss_sps_bwd_eta_p_n_inv = poss_sps_bwd_eta_p_n_inv + 1
         if (iand(inv_alg_msk, inv_alg_perm) .eq. 0) then
c
c     After an INVERT for which vr_in_r is permuted, re-form the vector
c     of basic costs, reset the pivotal row number and indicate that the
c     dual activities should be re-calculated.
c
            call ems_se_bc_co(
     &           is(p_st), ds(p_rsmi_co), is(p_vr_in_r),
     &           ds(p_bc_co_v), is(p_bc_co_ix), is(p_bc_co_ix_bar))
            if (.not. bd_swp)
     &           pv_r_n = iand(is(p_st+vr_t_en_bs), mx_mx_ml_a_dim)
            fresh_pc = .true.
            rq_re_pc = rq_re_pc_inv
c
c     >>>> Introduced 01/06/98
c
c nw_sgn_er_bug         else if (mx_pv_er .gt. tl_pv_er .or.
c nw_sgn_er_bug     &           mx_du_act_er .gt. tl_du_act_er) then
c
c     If numerical problems are suspected then indicate that the dual
c     activities should be re-calculated.
c
c nw_sgn_er_bug            mx_pv_er = tl_pv_er
c nw_sgn_er_bug            mx_du_act_er = tl_du_act_er
c nw_sgn_er_bug            fresh_pc = .true.
c nw_sgn_er_bug            if (mx_pv_er .gt. tl_pv_er) then
c nw_sgn_er_bug               rq_re_pc = rq_re_pc_pv_er
c nw_sgn_er_bug            else
c nw_sgn_er_bug               rq_re_pc = rq_re_pc_du_act_er
c nw_sgn_er_bug            endif
c
c     Introduced 01/06/98 <<<<
c
         endif
      end if
c
c     End of INVERT section
c=======================================================================
c     PRICE section
c
 1500 continue
      call ems_rsmi_pc_sn(
     &     u_du_act, u_ed_wt, re_pc,
     &     mx_pv_er,
     &     ds, is)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
c
c     End of PRICE section.
c=======================================================================
c     Update sparsity control parameters
c
c     If the FTRAN solution density is set to a legal value then update
c     the average density.
c
      if (sto_ftran_ix_mode .eq. sto_ix_poss) then
         if (ftran_sol_dse .ge. zero .and. ftran_sol_dse .le. one) then
            av_ftran_sol_dse = fac_long_prd*ftran_sol_dse +
     &           (one-fac_long_prd)*av_ftran_sol_dse
c
c     Consider whether it is worth continuing/starting to maintining
c     nonzeros in FTRAN
c
            if (av_ftran_sol_dse .gt. 1.5d0*tl_fwd_tran_dse_rhs) then
               sto_ftran_ix = sto_ix_no
            else
               sto_ftran_ix = sto_ix_y
            endif
         endif
         if (sto_ftran_ix .eq. sto_ix_y)
     &        poss_sps_sto_ftran_ix_y_n_it =
     &        poss_sps_sto_ftran_ix_y_n_it + 1
      endif
      if (sto_btran_ix_mode .eq. sto_ix_poss) then
c
c     If the BTRAN solution density is set to a legal value then update
c     the average density.
c
         if (btran_sol_dse .ge. zero .and. btran_sol_dse .le. one) then
            av_btran_sol_dse = fac_long_prd*btran_sol_dse +
     &           (one-fac_long_prd)*av_btran_sol_dse
c
c     Consider whether it is worth continuing/starting to maintining
c     nonzeros in BTRAN. Note that this depends on whether there is a
c     row-wise representation of the eta file.
c
            if (iand(eta_fi_da_st_msk, eta_fi_da_st_r_eta) .ne. 0) then
               if (av_btran_sol_dse .gt. 1.5d0*tl_fwd_tran_dse_rhs) then
                  sto_btran_ix = sto_ix_no
               else
                  sto_btran_ix = sto_ix_y
               endif
            else
               if (av_btran_sol_dse .gt. 1.5d0*tl_bwd_tran_dse_rhs) then
                  sto_btran_ix = sto_ix_no
               else
                  sto_btran_ix = sto_ix_y
               endif
            endif
         endif
         if (sto_btran_ix .eq. sto_ix_y)
     &        poss_sps_sto_btran_ix_y_n_it =
     &        poss_sps_sto_btran_ix_y_n_it + 1
      endif
      if (sto_tbu_r_ix_mode .eq. sto_ix_poss) then
c
c     If the tableau row density is set to a legal value then update
c     the average density.
c
         if (tbu_r_dse .ge. zero .and. tbu_r_dse .le. one) then
            av_tbu_r_dse = fac_long_prd*tbu_r_dse +
     &           (one-fac_long_prd)*av_tbu_r_dse
c
c     Consider whether it is worth continuing/starting to maintining
c     nonzeros in the tableau row
c
            if (av_tbu_r_dse .gt. 1.5d0*tl_fwd_tran_dse_rhs) then
               sto_tbu_r_ix = sto_ix_no
            else
               sto_tbu_r_ix = sto_ix_y
            endif
         endif
         if (sto_tbu_r_ix .eq. sto_ix_y)
     &        poss_sps_sto_tbu_r_ix_y_n_it =
     &        poss_sps_sto_tbu_r_ix_y_n_it + 1
      endif
      if (sto_ftran_ix_mode .eq. sto_ix_poss .or.
     &     sto_btran_ix_mode .eq. sto_ix_poss .or.
     &     sto_tbu_r_ix_mode .eq. sto_ix_poss) then
CM      IF (emsol_da .EQ. 1) THEN
C?         if (ems_pos_mod(n_si_it, 50) .eq. 1) write(7, 9370)
C?         write(7, 9371)
C?     &        iand(eta_fi_da_st_msk, eta_fi_da_st_fwd_p) .ne. 0,
C?     &        iand(eta_fi_da_st_msk, eta_fi_da_st_r_eta) .ne. 0,
C?     &        iand(eta_fi_da_st_msk, eta_fi_da_st_bwd_p) .ne. 0,
C?     &        ftran_sol_dse, av_ftran_sol_dse, sto_ftran_ix,
C?     &        btran_sol_dse, av_btran_sol_dse, sto_btran_ix,
C?     &        tbu_r_dse, av_tbu_r_dse, sto_tbu_r_ix, n_pc_vr
CM      ENDIF
         ftran_sol_dse = two
         btran_sol_dse = two
         tbu_r_dse = two
      endif
 
      if (re_pc .or. cg_pr_wt) go to 1000
      if (bd_swp) go to 2000
      if (rq_inv .eq. rq_inv_pv_er) then
         call ems_rsmi_inv(ds, is)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
         if (n_inv_sing .gt. 0) then
            rq_reset = rq_reset_sing_bs
            goto 3000
         endif
         poss_sps_n_inv = poss_sps_n_inv + 1
         if (iand(eta_fi_da_st_msk, eta_fi_da_st_fwd_p) .ne. 0)
     &        poss_sps_fwd_eta_p_n_inv = poss_sps_fwd_eta_p_n_inv + 1
         if (iand(eta_fi_da_st_msk, eta_fi_da_st_r_eta) .ne. 0)
     &        poss_sps_r_eta_fi_n_inv = poss_sps_r_eta_fi_n_inv + 1
         if (iand(eta_fi_da_st_msk, eta_fi_da_st_bwd_p) .ne. 0)
     &        poss_sps_bwd_eta_p_n_inv = poss_sps_bwd_eta_p_n_inv + 1
c
c     Maybe re-form the vector of basic costs.
c
         if (iand(inv_alg_msk, inv_alg_perm) .eq. 0)
     &        call ems_se_bc_co(
     &        is(p_st), ds(p_rsmi_co), is(p_vr_in_r),
     &        ds(p_bc_co_v), is(p_bc_co_ix), is(p_bc_co_ix_bar))
      endif
c
c     End of PRICE section.
c=======================================================================
c     End of major iteration.
CM      IF (emsol_dev .EQ. 1) THEN
C?      if (ts_parsmi .gt. 0) call ems_ck_parsmi_v(5, ds, is)
CM      ENDIF
 2000 continue
c
c     Allow a user exit at this point.
c
c     The dual activities may change because
c     *   the user has tightened bounds and the current vertex is not
c     .   feasible;
c     *   the user has changed costs.
c
c     The problem may no longer be optimal because the user has
c     relaxed bounds.
c
c     In either event, the rsmi_op_st_cz_c bit is un-set. This ensures
c     that cz_1_c is called at the start of the next iteration. The fact
c     that it_xit_reason.ne.it_xit_af_cz_c ensures that cz_1_c is not
c     called at in it_xit.
c
      if (ems_pos_mod(n_si_it, it_usr_xit_fq) .eq. 0) then
         it_xit_reason = it_xit_af_pr_it
         call ems_it_xit(ds, is, it_xit_reason, usr_rt_cod)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
         if (usr_rt_cod .eq. 3) then
            prob_st = prob_st_mx_n_it
            rq_reset = rq_reset_no_rq_reset
            go to 3000
         else if (usr_rt_cod .eq. 99) then
            reset_rsmi_n_si_it = -1
            rq_reset = rq_reset_usr_rq_reset
            go to 3000
         end if
      endif
c
c=======================================================================
c
      if (n_si_it .lt. mx_n_si_it) then
c
c     Have to put something here to reset if using EXPAND and tl_pr_ifs
c     is greater than wk_tl_pr_ifs.
c
         go to 1000
      else
         prob_st = prob_st_mx_n_it
         rq_reset = rq_reset_no_rq_reset
         go to 3000
      end if
c
c=======================================================================
c     Reset RSMI.
c     The code jumps to here if there is an indication of
c     infeasibility, optimality, unboundedness, status errors and other
c     non-serious errors. If simplex iterations have been performed
c     since the last such reset the nonbasic primal variables are reset,
c     the current basis is inverted and the basic primal and dual
c     variables are solved for. Otherwise the code terminates if one of
c     the following hold.
c
c     * No simplex iteration have been performed since the last reset
c       (this includes the recognition of true unboundedness ie
c       suspected unboundedness leading to a call to reset_rsmi and then
c       suspected unboundedness without a simplex iteration being
c       performed);
c
c     * The problem is optimal. (Optimality suspected and confirmed
c       within reset_rsmi);
c
c     * The problem is infeasible. (Infeasiblity suspected and confirmed
c       within reset_rsmi);
c
c     * The simplex iteration limit has been reached.
c
c
 3000 continue
      if (prob_st .ne. prob_st_mx_n_it .and.
     &     n_si_it .gt. reset_rsmi_n_si_it) then
         n_reset = n_reset + 1
         if (n_u .gt. 0) then
            rq_inv = rq_inv_reset_bs
            ml_da_st_msk =
     &           ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_inv)
         endif
CM      IF (emsol_xa .EQ. 1) THEN
C?         if (lp_iz_mode .eq. 1) then
CM      ENDIF
            call ems_reset_rsmi(reset_loop, ds, is)
CM      IF (emsol_xa .EQ. 1) THEN
C?         else
C?            call ems_nw_reset_rsmi(lp_iz_mode, reset_loop, ds, is)
C?         endif
CM      ENDIF
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      end if
      if (un_bd .and. prob_st .eq. prob_st_unknown)
     &     prob_st = prob_st_unbd
      if (reset_loop) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9700)
         call ems_msg_wr_li(er_msg_n)
      endif
      if (reset_loop .or.
     &     n_si_it .eq. reset_rsmi_n_si_it .or.
     &     prob_st .eq. prob_st_op .or.
     &     prob_st .eq. prob_st_ifs .or.
     &     prob_st .eq. prob_st_unbd .or.
     &     prob_st .eq. prob_st_mx_n_it) then
c
c     RSMI will terminate.
c
CM      IF (emsol_da .EQ. 1) THEN
C?         call ems_rsmi_da(4, ds, is)
C?         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
CM      ENDIF
c         call ems_ca_an_bs_inv(ds, is)
         go to 7000
      else
         reset_rsmi_n_si_it = n_si_it
         go to 1000
      end if
 7000 continue
CM      IF (emsol_da .EQ. 1) THEN
C?c      call ems_rsmi_da(5, ds, is)
C?c      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7100
C?      close(7)
CM      ENDIF
      poss_sps_n_si_it0 = n_si_it - poss_sps_n_si_it0
      if (poss_sps_n_si_it0 .gt. 0) then
         if (sto_ftran_ix_mode .eq. sto_ix_y) then
            pct = 100
         else
            pct = ems_i_t_i_pct(poss_sps_sto_ftran_ix_y_n_it,
     &           poss_sps_n_si_it0)
         endif
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9300)
     &        'Sparse FTRAN RHS   for ',
     &        poss_sps_sto_ftran_ix_y_n_it, poss_sps_n_si_it0,
     &        ' iterations ', pct
         call ems_msg_wr_li(info_msg_n)
         if (sto_btran_ix_mode .eq. sto_ix_y) then
            pct = 100
         else
            pct = ems_i_t_i_pct(poss_sps_sto_btran_ix_y_n_it,
     &           poss_sps_n_si_it0)
         endif
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9300)
     &        'Sparse BTRAN RHS   for ',
     &        poss_sps_sto_btran_ix_y_n_it, poss_sps_n_si_it0,
     &        ' iterations ', pct
         call ems_msg_wr_li(info_msg_n)
         if (sto_tbu_r_ix_mode .eq. sto_ix_y) then
            pct = 100
         else
            pct = ems_i_t_i_pct(poss_sps_sto_tbu_r_ix_y_n_it,
     &           poss_sps_n_si_it0)
         endif
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9300)
     &        'Sparse Tableau row for ',
     &        poss_sps_sto_tbu_r_ix_y_n_it, poss_sps_n_si_it0,
     &        ' iterations ', pct
         call ems_msg_wr_li(info_msg_n)
      endif
      if (poss_sps_n_inv .gt. 0) then
         pct = ems_i_t_i_pct(poss_sps_fwd_eta_p_n_inv, poss_sps_n_inv)
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9300)
     &        'Etas: Fwd Pointers for ',
     &        poss_sps_fwd_eta_p_n_inv, poss_sps_n_inv,
     &        ' INVERTs    ', pct
         call ems_msg_wr_li(info_msg_n)
         pct = ems_i_t_i_pct(poss_sps_r_eta_fi_n_inv, poss_sps_n_inv)
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9300)
     &        'Etas: Row-wise     for ',
     &        poss_sps_r_eta_fi_n_inv, poss_sps_n_inv,
     &        ' INVERTs    ', pct
         call ems_msg_wr_li(info_msg_n)
         pct = ems_i_t_i_pct(poss_sps_bwd_eta_p_n_inv, poss_sps_n_inv)
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9300)
     &        'Etas: Bwd Pointers for ',
     &        poss_sps_bwd_eta_p_n_inv, poss_sps_n_inv,
     &        ' INVERTs    ', pct
         call ems_msg_wr_li(info_msg_n)
      endif
c=======================================================================
c     Set the appropriate problem status if the available space has been
c     exceeded: this is the most likely cause of a serious error.
c
c 7010 continue
c 7100 continue
      if (is(ix_n_xa_i_wo_rq) .gt. 0) prob_st = prob_st_no_po
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
c 8800 continue
c      ems_msg_cod = ems_msg_lvl_serious
c      goto 7100
 9030 format('COMMENT: Iteration ', i7, ': ||b-Ax|| = ', g11.4)
 9100 format('Starting with ', a, ' pricing strategy ')
 9200 format('Variable cannot move in direction indicated',
     &     ' by a phase 1 dual activity = ', g11.4)
 9210 format('Reducing primal objective weight to ', g11.4)
 9220 format('Primal objective weight is already zero ')
 9300 format(a, i7, ' of ', i7, a, '(', i3, '%)')
CM      IF (emsol_da .EQ. 1) THEN
C? 9370 format(
C?     &     '  EtaFiDaSt         ',
C?     &     '               FTRAN           ',
C?     &     '               BTRAN           ',
C?     &     '               TBU_R           '/
C?     &     '  FwdP  REta  BwdP  ',
C?     &     ' SolDse       AvSolDse   Sps   ',
C?     &     ' SolDse       AvSolDse   Sps   ',
C?     &     '    Dse          AvDse   Sps     #En')
C? 9371 format(3(5x, l1), 3(2x, g11.4, 2x, g11.4, 2x, i2, 1x), i7)
CM      ENDIF
 9700 format('SSLV has terminated due to a reset loop')
 9800 format('CHUZC has not been performed for current ',
     &     'dual activities and bounds on non-basic variables.')
      end
C->>> --------------------------------------------> ems_rsmi_cz_c_sn <<<
c     RSMI CHUZC section
c
      subroutine ems_rsmi_cz_c_sn(ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMICS.INC'
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
c      include 'RLCTVR.INC'
      integer is(0:is_n_en_m1)
      double precision ds(0:ds_n_en_m1)
 
CM      IF (emsol_deb .EQ. 1) THEN
C?      if (wr_lp_da .ne. 0) call ems_wr_lp_da(1, ds, is)
CM      ENDIF
c
c     If CHUZC has not been performed for the current dual activities
c     and bounds on non-basic variables then do it!
c
      if (iand(rsmi_op_st_msk, rsmi_op_st_cz_c) .eq. 0) call ems_cz_1_c(
     &     is(p_vr_in_c), ds, is)
CM      IF (emsol_da .EQ. 1) THEN
C?c      if (en_bk_bd .ne. 0) cz_c_n_bk_bd = cz_c_n_bk_bd + 1
CM      ENDIF
c
c     CHUZC has been performed for the current dual activities and
c     bounds on non-basic variables: the rsmi_op_st_cz_c bit is set in
c     rsmi_op_st_msk.
c
c     Let the variable to enter the basis be the chosen variable.
c
      vr_t_en_bs = nx_vr_t_en_bs
      return
      end
C->>> -------------------------------------------> ems_rsmi_ftran_sn <<<
c     RSMI FTRAN section
c
      subroutine ems_rsmi_ftran_sn(refined_pv_c, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
c      include 'RLCTVR.INC'
      logical refined_pv_c
      integer is(0:is_n_en_m1)
      double precision ds(0:ds_n_en_m1)
      integer rhs_sgn
 
      rhs_sgn = 1
      if (sto_ftran_ix .eq. sto_ix_y) then
         is(p_nw_eta_ix) = 0
      else
         is(p_nw_eta_ix) = n_r+1
      endif
c
c     Possibly apply a sign change to the RHS before FTRAN if the
c     entering variable is decreasing in order to avoid multiplying by
c     mv_dir in cz_r
c
c     Tried 02-03/02/98---for Devex but untested code written for
c     Steepest Edge---but didn't make any difference in brief
c     experiments with 25FV47, and complicated the code!
c
c     SGN rhs_sgn = mv_dir
c     NO_SGN rhs_sgn = 1
      rhs_sgn = 1
      call ems_g_rhs(rhs_sgn, vr_t_en_bs,
     &     ds(p_pv_c_v),
     &     is(p_nw_eta_ix), ds, is)
CM      IF (emsol_deb .EQ. 1) THEN
C?      if (wr_lp_da .ne. 0) call ems_wr_lp_da(2, ds, is)
CM      ENDIF
      call ems_ftran(
     &     ds(p_pv_c_v),
     &     is(p_nw_eta_ix), ds, is)
      refined_pv_c = .false.
      nw_eta_l_ix = is(p_nw_eta_ix)
CM      IF (emsol_deb .EQ. 1) THEN
C?      if (wr_lp_da .ne. 0) call ems_wr_lp_da(2, ds, is)
CM      ENDIF
c      call ems_ca_rp_1_vr_st(1, vr_t_en_bs, ds, is)
      return
      end
C->>> -----------------------------------------------> ems_rsmi_u_sn <<<
c     RSMI UPDATE section
c
      subroutine ems_rsmi_u_sn(ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMICS.INC'
      include 'RSMICOM.INC'
CM      IF (emsol_da .EQ. 1) THEN
C?      include 'EMSDA.INC'
CM      ENDIF
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      integer is(0:is_n_en_m1)
      double precision ds(0:ds_n_en_m1)
CM      IF (emsol_dev .EQ. 1) THEN
C?      integer ems_pos_mod
C?      integer lc_n_pc_vr, lc_n_pc_el
CM      ENDIF
      integer u_bs_dse_blk_dim
      double precision ftran_er, btran_er
      integer rhs_sgn
 
      rhs_sgn = 1
c
c     Get the basis change section for the entering variable.
c
c      call ems_og_g_bs_cg_st(ca_ems_rt_cod, 0, vr_t_en_bs,
c     &     is(p_st+vr_t_en_bs),
c     &     ds(p_pr_act+vr_t_en_bs),
c     &     ds(p_rsmi_lb+vr_t_en_bs),
c     &     ds(p_rsmi_ub+vr_t_en_bs),
c     &     en_vr_fm_bs_cg_st)
c      call ems_g_bs_cg_st(ca_ems_rt_cod, 0, vr_t_en_bs,
c     &     is(p_st+vr_t_en_bs),
c     &     is(p_vr_in_c),
c     &     en_vr_fm_bs_cg_st)
c      if (ca_ems_rt_cod .ne. ems_rt_cod_ok) then
c         ems_rt_cod = max(ca_ems_rt_cod, ems_rt_cod)
cc     if (ems_rt_cod .ge. ems_rt_lvl_serious) goto 7100
c         if (ems_rt_cod .ge. ems_rt_lvl_serious) goto 8950
c      endif
c
c     Update the primal activities.
c
      if (aa .ne. zero) call ems_u_pr_act(
     &     rhs_sgn,
     &     ds(p_pr_act),
     &     is(p_vr_in_r),
     &     ds(p_nw_eta_v),
     &     is(p_nw_eta_ix))
c
c     Maybe make an iteration time record.
c
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_it_tt_fq .gt. 0) then
C?         if (ems_pos_mod(n_si_it, g_it_tt_ivl) .eq. 0)
C?     &        call ems_it_tt(n_si_it)
C?         if (ems_pos_mod(n_si_it, g_it_tt_fq) .eq. 0 .and.
C?     &        n_si_it .gt. 0) call ems_it_tt_rec(n_si_it)
C?      endif
CM      ENDIF
c
c     Update the count of simplex iterations
c
      n_si_it = n_si_it + 1
CM      IF (emsol_da .EQ. 1) THEN
C?c
C?c     Determine whether the phase 1 step was attractive for phase 2.
C?c
C?      if (pr_wt .eq. zero .and. lp_ph .eq. 1) then
C?         n_ph_1_it = n_ph_1_it + 1
C?         call ems_an_ph_2_du_act(
C?     &        is(p_st),
C?     &        ds(p_rsmi_co),
C?     &        is(p_vr_in_r),
C?     &        ds(p_nw_eta_v),
C?     &        is(p_nw_eta_ix))
C?      endif
C?c
C?c     Update the count of zero/degenerate iterations.
C?c
C?      if (abs(aa) .le. tl_pr_ifs) then
C?         if (abs(aa) .le. abs(xp_tau/pv)) then
C?            n_ze_it = n_ze_it + 1
C?         else
C?            n_dgn_it = n_dgn_it + 1
C?         endif
C?      endif
C?c
C?c     Update the count of bound swaps/basis changes.
C?c
C?      if (bd_swp) then
C?         n_bd_swp = n_bd_swp + 1
C?      else
C?         n_bs = n_bs + 1
C?      end if
CM      ELSE
c
c     Update the count of basis changes.
c
      if (.not. bd_swp) n_bs = n_bs + 1
CM      ENDIF
c
c     Update the indexing for basic variables.
c
      call ems_u_vr_in_r(
     &     vr_t_en_bs, vr_t_lv_bs, is(p_st), is(p_vr_in_r))
c
c     Update the indexing for nonbasic variables.
c
      call ems_u_vr_in_c(pv_c_n, vr_t_lv_bs,
     &     ds(p_rsmi_lb),
     &     ds(p_rsmi_co),
     &     ds(p_rsmi_ub),
     &     is(p_st),
     &     ds(p_pr_act),
     &     ds(p_du_act),
     &     tl_pr_ifs,
     &     is(p_vr_in_c),
     &     ds, is)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
CM      IF (emsol_dev .EQ. 1) THEN
C?      call ems_g_n_pc_vr_el(lc_n_pc_vr, lc_n_pc_el,
C?     &     is(p_vr_in_c), is(p_mtx_c_sa))
C?      if (lc_n_pc_vr .ne. n_pc_vr) goto 8010
C?      if (lc_n_pc_el .ne. n_pc_el) goto 8020
CM      ENDIF
c
c     Update the basic costs.
c
      call ems_u_bc_co(
     &     ds(p_rsmi_lb),
     &     ds(p_rsmi_ub),
     &     is(p_st),
     &     ds(p_rsmi_co),
     &     ds(p_pr_act),
     &     is(p_vr_in_r),
     &     ds(p_bc_co_v),
     &     is(p_bc_co_ix),
     &     is(p_bc_co_ix_bar),
     &     ds(p_pi_v),
     &     is(p_pi_ix),
     &     ds(p_du_act),
     &     is(p_cz_r_cdd_ix),
     &     is(p_vr_in_c),
     &     ds, is)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      if (lp_ph_cg) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9410)
         call ems_msg_wr_li(warn_msg_n)
      endif
      if (n_pr_ifs .gt. 0) then
         lp_ph_cg = lp_ph .eq. 2
      else
         lp_ph_cg = lp_ph .eq. 1
      endif
c
c     If a change of phase has been identified then change phase and
c     reset the basic costs.
c
      fresh_pc = lp_ph_cg
      if (lp_ph_cg) then
         rq_re_pc = rq_re_pc_ph_cg
         if (iand(ml_da_st_msk, ml_da_st_alt_lp) .eq. 0) then
c
c     If the model does not have non-standard variables then switch off
c     L1-CHUZR if moving to Phase II. Switch on L1-CHUZR if moving to
c     Phase I.
c
            if (lp_ph .eq. 1) then
               cz_r_msk = cz_r_msk - iand(cz_r_msk, cz_r_l1_bt)
            else
               cz_r_msk = ior(cz_r_msk, cz_r_l1_bt)
            endif
         endif
         if (lp_ph .eq. 1 .and.
     &        iand(cz_c_msk, cz_c_bk_bd_bt) .ne. 0) then
c
c     When leaving Phase I after allowing variables to break through
c     bounds, fix the nonbasic equalities which had, hitherto, been
c     fixed only temporarily.
c
            call ems_fx_te_fx_vr(is(p_vr_in_c), is(p_st), ds, is)
            if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
         endif
CM      IF (emsol_da .EQ. 1) THEN
C?         write(ems_msg_wr_cn, 9020)n_si_it
C?         call ems_flush(ems_msg_wr_cn)
CM      ENDIF
         if (lp_ph .eq. 1) then
            lp_ph = 2
            pr_co_mu = mx_mn
         else
            lp_ph = 1
            pr_co_mu = mx_mn*pr_wt
         endif
         call ems_se_bc_co(
     &        is(p_st),
     &        ds(p_rsmi_co),
     &        is(p_vr_in_r),
     &        ds(p_bc_co_v),
     &        is(p_bc_co_ix),
     &        is(p_bc_co_ix_bar))
         lp_ph_cg = .false.
CM      IF (emsol_da .EQ. 1) THEN
C?         if (wr_bs_fq .gt. 0 .and.
C?     &        lp_ph .eq. 2) call ems_wr_si_it_bs(.false., ds, is)
CM      ENDIF
      endif
c
c     Decide whether to reinvert according to timing model.
c     (Not implemented)
c
c        call ems_ct_re_inv
CM      IF (emsol_da .EQ. 1) THEN
C?      if (.not. bd_swp) then
C?         if (n_sc_u .lt. mx_n_sc_u) then
C?            if (iand(is(p_st+vr_t_lv_bs), inv_bs_bt) .ne. 0)
C?     &           sc_dim = sc_dim + 1
C?            if (iand(is(p_st+vr_t_en_bs), inv_bs_bt) .ne. 0)
C?     &           sc_dim = sc_dim - 1
C?            inv_tot_sc_dim = inv_tot_sc_dim + sc_dim
C?            n_sc_u = n_sc_u + 1
C?c            write(11, 11010)n_si_it-n_bd_swp, n_sc_u, sc_dim
C?         else
C?            sc_dim = 0
C?            n_sc_u = 0
C?            tot_sc_dim = tot_sc_dim + inv_tot_sc_dim
C?c            write(11, 11000)
C?c     &           (2*inv_tot_sc_dim+mx_n_sc_u)/
C?c     &           (2*mx_n_sc_u),
C?c     &           (2*tot_sc_dim+(n_si_it-n_bd_swp))/
C?c     &           (2*(n_si_it-n_bd_swp))
C?c            call ems_flush(11)
C?c            write(11, 11010)n_si_it-n_bd_swp, n_sc_u, sc_dim
C?            inv_tot_sc_dim = 0
C?            call ems_iz_inv_bs_bt(is(p_st), is(p_vr_in_r))
C?         endif
C?      endif
C?11000 format('Average SC dim: This INVERT ', i2, ' overall ', i2)
C?11010 format(i7, 2(1x, i3))
CM      ENDIF
 
c     Changed 14/03/1997
c     Bound swap when n_u=mx_n_u resulted in rq_inv=rq_inv_mx_n_u
c     being set but no invert being performed until after CHUZC and
c     FTRAN: wasted work and, error if the chosen variable is BP and
c     the bits are set correspnding to it having crossed its breakpoint
c     in preparation for CHUZR
c      if (n_u .ge. mx_n_u) then
c         rq_inv = rq_inv_mx_n_u
c         ml_da_st_msk =
c     &        ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_inv)
c      endif
c      if (.not. bd_swp .and. rq_inv .eq. rq_inv_no_rq_inv) then
 
      if (.not. bd_swp) then
         if (n_u .ge. mx_n_u) then
            rq_inv = rq_inv_mx_n_u
            ml_da_st_msk =
     &           ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_inv)
         else
            if (rq_inv .eq. rq_inv_no_rq_inv) then
c
c     Perform an UPDATE.
c
               call ems_ca_sto_u_eta(rhs_sgn, sto_u_eta_se_ty, ds, is)
               if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
               if (u_bs .eq. u_bs_pf_r_cp) then
                  u_bs_dse_blk_dim = is(p_u_bs_skt_pv_r)
                  call ems_u_pf_dse_blk(
     &                 u_bs_dse_blk_dim,
     &                 ds(p_u_bs_dse_blk),
     &                 is(p_u_bs_gthr_pv_r),
     &                 is(p_u_bs_dse_blk_pv_r_in_c),
     &                 is(p_u_bs_skt_pv_r),
     &                 is(p_u_bs_eta_msk),
     &                 ds, is)
               endif
               if ((iand(ck_msk, u_ck_bt) .ne. 0) .or.
     &              (n_u .eq. mx_n_u) .and.
     &              (iand(ck_msk, inv_ck_bt) .ne. 0)) then
                  call ems_ca_rand_ck_inv(.true.,
     &                 ftran_er, btran_er, ds, is)
                  call ems_ck_inv(ds, is)
                  if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
               end if
            end if
         end if
      end if
CM      IF (emsol_dev .EQ. 1) THEN
C?      if (ts_parsmi .gt. 0) call ems_ck_parsmi_v(2, ds, is)
CM      ENDIF
 7000 continue
      return
CM      IF (emsol_dev .EQ. 1) THEN
C? 8010 continue
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)
C?     &     lc_n_pc_vr, n_pc_vr
C?      call ems_msg_wr_li(bug_msg_n)
C?      goto 7000
C? 8020 continue
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)
C?     &     lc_n_pc_el, n_pc_el
C?      call ems_msg_wr_li(bug_msg_n)
C?      goto 7000
CM      ENDIF
c 9020 format(/'Phase change after ', i9, ' simplex iterations')
 9410 format('lp_ph_cg is true after u_bc_co')
CM      IF (emsol_dev .EQ. 1) THEN
C? 9801 format('Inconsistency between lc_n_pc_vr and n_pc_vr ',
C?     &     i7, 2x, i7)
C? 9802 format('Inconsistency between lc_n_pc_el and n_pc_el ',
C?     &     i7, 2x, i7)
CM      ENDIF
      end
C->>> ----------------------------------------------> ems_rsmi_pc_sn <<<
c     RSMI PRICE section
c
      subroutine ems_rsmi_pc_sn(
     &     u_du_act, u_ed_wt, re_pc,
     &     mx_pv_er,
     &     ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMICS.INC'
      include 'RSMIHDL.INC'
      include 'RSMICOM.INC'
CM      IF (emsol_da .EQ. 1) THEN
C?      include 'EMSDA.INC'
CM      ENDIF
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      logical u_du_act, u_ed_wt, re_pc
      double precision mx_pv_er
      integer is(0:is_n_en_m1)
      double precision ds(0:ds_n_en_m1)
      double precision pi_rhs_v_in_pv_r
      double precision sv_du_act, sv_ed_wt
      double precision rcp_alt_pv
      double precision pv_er
      integer pi_rhs_n_ix
      integer i_wk_a_ix
      integer rhs_sgn
 
      rhs_sgn = 1
      u_du_act = u_pc .eq. 1
      pi_rhs_v_in_pv_r = ds(p_du_act+vr_t_en_bs)
      fresh_pc = fresh_pc .or. u_pc .eq. 0 .or. re_pc
c
c     Set the density of the BTRAN solution and tableau row to illegal
c     value in case the a unit BTRAN and corresponding tableau row are
c     not formed.
c
      btran_sol_dse = two
      tbu_r_dse = two
CM      IF (emsol_dev .EQ. 1) THEN
C?      if (fresh_pc .and. u_pc .ne. 0)
C?     &     call ems_mo_rsmi_re_pc(n_si_it, rq_re_pc)
CM      ENDIF
c
c     Fresh prices are computed from the basic costs if
c     .  update price is not used;
c     .  the basis has been reinverted without permutations;
c     .  the updated prices appear too inaccurate due to rounding error;
c     .  a phase change has occurred.
c
      if (fresh_pc .or. is(p_pi_ix) .gt. 0) then
c
c     An additional BTRAN must be performed because the RHS for pi in
c     order to compute or update the reduced costs is not a vector with
c     a single entry in row pv_r_n.
c
         if (fresh_pc) then
c
c     There will only be nonzeros in pi if it has accumulated basic cost
c     changes for an update price which is not now being done.
c
            call ems_ze_pi_v(ds(p_pi_v), is(p_pi_ix))
            call ems_cp_nz_v_ix(n_r,
     &           is(p_bc_co_ix),
     &           ds(p_bc_co_v),
     &           is(p_pi_ix),
     &           ds(p_pi_v))
         else
c
c     Do an update price but there have been several changes in the
c     basic costs due to basic variables becoming feasible in phase 1.
c     The `tableau row' which is calculated is actually a linear
c     combination of tableau rows and it is just added in in order to
c     update the reduced costs.
c
            if (.not. bd_swp .and.
     &           abs(pi_rhs_v_in_pv_r) .gt. bwd_tran_ze) then
               ds(p_pi_v+pv_r_n) = pi_rhs_v_in_pv_r
               if (sto_btran_ix .eq. sto_ix_y) then
                  pi_rhs_n_ix = is(p_pi_ix) + 1
                  is(p_pi_ix+pi_rhs_n_ix) = pv_r_n
                  is(p_pi_ix) = pi_rhs_n_ix
               end if
            end if
         end if
CM      IF (emsol_deb .EQ. 1) THEN
C?         if (wr_lp_da .ne. 0) call ems_wr_lp_da(3, ds, is)
CM      ENDIF
         pi_rhs_n_ix = is(p_pi_ix)
         if (sto_btran_ix .eq. sto_ix_no) is(p_pi_ix) = n_r+1
CM      IF (emsol_da .EQ. 1) THEN
C?         call ems_u_tbu_r_pi_rhs_da(is(p_pi_ix))
CM      ENDIF
         call ems_btran(ds(p_pi_v), is(p_pi_ix), ds, is)
         if (iand(inv_alg_msk, inv_alg_perm) .ne. 0 .and.
     &        iand(ml_da_st_msk, ml_da_st_r_mtx) .eq. 0)
     &        call ems_perm_btran_sol(
     &        p_pi_v, hdl_pi_v, is(p_pi_ix), ds, is)
         if (is(p_pi_ix) .le. n_r .and.
     &        tbu_r_loop_mode .ge. tbu_r_loop_poss) then
c
c     Have to remove indices of repeated entries in the RHS since this
c     may lead to incorrect dual activity updates for logicals.
c     Routine ems_rm_rhs_ix_o_ze removes zeros as well---at the cost of
c     having to address the values of pi.
c
            call ems_g_rsmi_i_wk_a_ix(i_wk_a_ix)
            if (i_wk_a_ix .lt. 0) goto 8000
            call ems_rm_rhs_ix_o_ze(
     &           ds(p_pi_v), is(p_pi_ix),
     &           is(p_rsmi_i_wk_a(i_wk_a_ix)))
            call ems_fr_rsmi_i_wk_a_ix(i_wk_a_ix)
         endif
CM      IF (emsol_deb .EQ. 1) THEN
C?         if (wr_lp_da .ne. 0) call ems_wr_lp_da(4, ds, is)
CM      ENDIF
         if (pi_rhs_n_ix .eq. 1) then
c
c     pi corresponds to a unit RHS so determine whether the density of
c     pi or the tableau row needs to be determined.
c
            if (sto_btran_ix_mode .eq. sto_ix_poss) btran_sol_dse = -one
            if (sto_tbu_r_ix_mode .eq. sto_ix_poss) tbu_r_dse = -one
         endif
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl0) call ems_tt_rec(pc_tt, n_bs)
CM      ENDIF
         if (fresh_pc) then
            if (iand(inv_alg_msk, inv_alg_perm) .eq. 0 .or.
     &           iand(ml_da_st_msk, ml_da_st_r_mtx) .eq. 0) then
               call ems_g_lg_du_act(
     &              is(p_vr_in_c),
     &              ds(p_rsmi_co),
     &              ds(p_du_act),
     &              ds(p_pi_v))
            else
               call ems_perm_g_lg_du_act(
     &              is(p_vr_in_c),
     &              ds(p_rsmi_co),
     &              ds(p_du_act),
     &              ds(p_pi_v),
     &              is(p_og_t_nw_perm))
            endif
c
c     Price on all nonbasic structurals except those which are
c     permanently fixed.
c
            call ems_struc_pc(ds, is)
            call ems_g_struc_du_act(
     &           is(p_vr_in_c),
     &           ds(p_rsmi_co),
     &           ds(p_du_act),
     &           ds(p_tbu_r_v),
     &           is(p_tbu_r_ix))
            fresh_pc = .false.
            rq_re_pc = rq_re_pc_no_rq_re_pc
c
c     Indicate that the model nonbasic dual activities are now correct.
c
            ml_da_st_msk = ior(ml_da_st_msk, ml_da_st_non_bc_du_act)
         else
            sv_du_act = ds(p_du_act+vr_t_en_bs)
            if (iand(inv_alg_msk, inv_alg_perm) .eq. 0 .or.
     &           iand(ml_da_st_msk, ml_da_st_r_mtx) .eq. 0) then
               call ems_u_lg_du_act(
     &              is(p_vr_in_c),
     &              ds(p_du_act),
     &              ds(p_pi_v),
     &              is(p_pi_ix))
            else
               call ems_perm_u_lg_du_act(
     &              is(p_vr_in_c),
     &              ds(p_du_act),
     &              ds(p_pi_v),
     &              is(p_pi_ix),
     &              is(p_og_t_nw_perm),
     &              is(p_nw_t_og_perm))
            endif
            ds(p_du_act+vr_t_en_bs) = sv_du_act
c
c     Price on all nonbasic structurals except those which are
c     permanently fixed.
c
            call ems_struc_pc(ds, is)
            call ems_u_struc_du_act(
     &           is(p_vr_in_c),
     &           ds(p_du_act),
     &           ds(p_tbu_r_v),
     &           is(p_tbu_r_ix))
         end if
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl0) call ems_tt_rec(-pc_tt, n_bs)
CM      ENDIF
         if (re_pc .or. cg_pr_wt) go to 7000
         u_du_act = .false.
      end if
c
c     If there has been a bound swap then it may have been necessary to
c     price the nonbasic variables if basic variables have become
c     feasible as a result. However there is no need to update the
c     weights since the basis has not changed.
c
      if (bd_swp) go to 7000
      u_ed_wt = (
     &     pc_alg .eq. pc_alg_approx_dvx .or.
     &     pc_alg .eq. pc_alg_exact_dvx .or.
     &     pc_alg .eq. pc_alg_sed)
c     &     pc_alg .eq. pc_alg_sed) .and. .not. nw_dvx_fwk
      if (pi_rhs_v_in_pv_r .eq. zero) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9400)n_si_it
         call ems_msg_wr_li(warn_msg_n)
         u_du_act = .false.
      endif
      if (u_ed_wt .or. u_du_act) then
c
c     Unless there is a new Devex framework and the reduced costs do not
c     need to be updated, a row of the updated tableau must be computed.
c     If the reduced costs need to be updated then this is scaled by
c     pi_rhs_v_in_pv_r. This is used to update the reduced cost weights
c     and/or update the reduced costs.
c
c     Do BTRAN with RHS a single entry in row pv_r_n.
c
         if (iand(ck_msk, ze_a_ck_bt) .ne. 0)
     &        call ems_ck_ze_rl_a(n_r, ds(p_pi_v))
         if (u_du_act) then
c
c     Note that u_du_act => pi_rhs_v_in_pv_r .ne. 0
c
            ds(p_pi_v+pv_r_n) = pi_rhs_v_in_pv_r
         else
            ds(p_pi_v+pv_r_n) = one
         end if
CM      IF (emsol_deb .EQ. 1) THEN
C?         if (wr_lp_da .ne. 0) call ems_wr_lp_da(3, ds, is)
CM      ENDIF
         is(p_pi_ix+1) = pv_r_n
         is(p_pi_ix) = 1
         pi_rhs_n_ix = is(p_pi_ix)
         if (sto_btran_ix .eq. sto_ix_no) is(p_pi_ix) = n_r+1
CM      IF (emsol_da .EQ. 1) THEN
C?         call ems_u_tbu_r_pi_rhs_da(is(p_pi_ix))
CM      ENDIF
         call ems_btran(ds(p_pi_v), is(p_pi_ix), ds, is)
         if (is(p_pi_ix) .le. n_r .and.
     &        tbu_r_loop_mode .ge. tbu_r_loop_poss .and.
     &        u_du_act) then
            call ems_g_rsmi_i_wk_a_ix(i_wk_a_ix)
            if (i_wk_a_ix .lt. 0) goto 8000
            call ems_rm_rhs_ix_o_ze(
     &           ds(p_pi_v), is(p_pi_ix),
     &           is(p_rsmi_i_wk_a(i_wk_a_ix)))
            call ems_fr_rsmi_i_wk_a_ix(i_wk_a_ix)
         endif
CM      IF (emsol_deb .EQ. 1) THEN
C?         if (wr_lp_da .ne. 0) call ems_wr_lp_da(4, ds, is)
CM      ENDIF
         if (iand(inv_alg_msk, inv_alg_perm) .ne. 0) then
            call ems_perm_g_rcp_alt_pv(rcp_alt_pv, vr_t_lv_bs,
     &           ds(p_mtx_r_v),
     &           is(p_mtx_r_ix),
     &           is(p_mtx_c_sa),
     &           ds(p_pi_v),
     &           is(p_og_t_nw_perm))
         else
            call ems_g_rcp_alt_pv(rcp_alt_pv, vr_t_lv_bs,
     &           ds(p_mtx_r_v),
     &           is(p_mtx_r_ix),
     &           is(p_mtx_c_sa),
     &           ds(p_pi_v))
         endif
         if (iand(inv_alg_msk, inv_alg_perm) .ne. 0 .and.
     &        iand(ml_da_st_msk, ml_da_st_r_mtx) .eq. 0)
     &        call ems_perm_btran_sol(
     &        p_pi_v, hdl_pi_v, is(p_pi_ix), ds, is)
c
c     Pricing the result of BTRAN with the matrix column corresponding
c     to the variable which has just left the basis gives 1/pivot
c     (scaled by pi_rhs_v_in_pv_r if reduced cost weights are not
c     about to be updated).
c
c     Note that u_du_act => pi_rhs_v_in_pv_r .ne. 0
c
         if (u_du_act) rcp_alt_pv = rcp_alt_pv/pi_rhs_v_in_pv_r
         if (n_u .gt. 0) then
c
c     Unless the basis has just been reinverted---sometimes (PILOT)
c     pivot errors immediately after reinversion because updated INVERT
c     was poor---check the relative error in the pivot ehich is given by
c
c     pv_er = abs((one/pv-rcp_alt_pv)*pv)
c
c     which simplifies to
c
c     pv_er = abs(one-rcp_alt_pv*pv)
c
            pv_er = abs(one-rcp_alt_pv*pv)
            if (pv_er .gt. tl_pv_er) then
               rq_inv = rq_inv_pv_er
               ml_da_st_msk =
     &              ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_inv)
               if (pv_er .gt. mx_pv_er) then
                  mx_pv_er = pv_er
CM      IF (emsol_dev .EQ. 1) THEN
C?                  call ems_mo_rsmi_pv_er(
C?     &                 n_si_it, pv, rcp_alt_pv, pv_er)
CM      ENDIF
               endif
            end if
CM      IF (emsol_dev .EQ. 1) THEN
C?            if (u_du_act .and. abs(pv_er*pi_rhs_v_in_pv_r) .gt.
C?     &           1d-4) then
C?               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9510)n_si_it,
C?     &              pv_er, pi_rhs_v_in_pv_r, pv_er*pi_rhs_v_in_pv_r
C?               call ems_msg_wr_li(warn_msg_n)
C?            endif
CM      ENDIF
         endif
c
c     pi corresponds to a unit RHS so determine whether the density of
c     pi or the tableau row needs to be determined.
c
         if (sto_btran_ix_mode .eq. sto_ix_poss) btran_sol_dse = -one
         if (sto_tbu_r_ix_mode .eq. sto_ix_poss) tbu_r_dse = -one
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl0) call ems_tt_rec(pc_tt, n_bs)
CM      ENDIF
         if (u_ed_wt) then
c
c     Update the reduced cost weights and (possibly) the reduced costs.
c
            if (pc_alg .eq. pc_alg_sed) then
               sv_du_act = ds(p_du_act+vr_t_en_bs)
               sv_ed_wt =  ds(p_ed_wt+ vr_t_en_bs)
               if (iand(inv_alg_msk, inv_alg_perm) .eq. 0 .or.
     &              iand(ml_da_st_msk, ml_da_st_r_mtx) .eq. 0) then
                  call ems_ca_u_lg_sed_wt(
     &                 rhs_sgn,
     &                 u_du_act,
     &                 is(p_vr_in_c),
     &                 ds(p_du_act),
     &                 ds(p_pi_v),
     &                 is(p_pi_ix),
     &                 ds(p_ed_wt),
     &                 ds(p_pv_c_v))
               else
                  call ems_perm_ca_u_lg_sed_wt(
     &                 rhs_sgn,
     &                 u_du_act,
     &                 is(p_vr_in_c),
     &                 ds(p_du_act),
     &                 ds(p_pi_v),
     &                 is(p_pi_ix),
     &                 ds(p_ed_wt),
     &                 ds(p_pv_c_v),
     &                 is(p_og_t_nw_perm),
     &                 is(p_nw_t_og_perm))
               endif
               ds(p_du_act+vr_t_en_bs) = sv_du_act
               ds(p_ed_wt+ vr_t_en_bs) = sv_ed_wt
c
c     Price on all nonbasic structurals except those which are
c     permanently fixed.
c
               call ems_struc_pc(ds, is)
               call ems_ca_u_struc_sed_wt(
     &              rhs_sgn,
     &              u_du_act,
     &              is(p_vr_in_c),
     &              ds(p_du_act),
     &              ds(p_tbu_r_v),
     &              is(p_tbu_r_ix),
     &              ds(p_ed_wt),
     &              ds(p_mtx_r_v),
     &              is(p_mtx_r_ix),
     &              is(p_mtx_c_sa),
     &              ds(p_pv_c_v))
            else if (pc_alg .eq. pc_alg_exact_dvx .or.
     &              pc_alg .eq. pc_alg_approx_dvx) then
               call ems_u_dvx_ix(
     &              is(p_dvx_ix))
               sv_du_act = ds(p_du_act+vr_t_en_bs)
               sv_ed_wt =  ds(p_ed_wt+ vr_t_en_bs)
               if (iand(inv_alg_msk, inv_alg_perm) .eq. 0 .or.
     &              iand(ml_da_st_msk, ml_da_st_r_mtx) .eq. 0) then
                  call ems_ca_u_lg_dvx_wt(
     &                 u_du_act,
     &                 is(p_vr_in_c),
     &                 ds(p_du_act),
     &                 ds(p_pi_v),
     &                 is(p_pi_ix),
     &                 ds(p_ed_wt))
               else
CM      IF (emsol_dev .EQ. 1) THEN
C?                  if (ts_parsmi .gt. 0) call ems_ck_parsmi_v(3, ds, is)
CM      ENDIF
                  call ems_perm_ca_u_lg_dvx_wt(
     &                 u_du_act,
     &                 is(p_vr_in_c),
     &                 ds(p_du_act),
     &                 ds(p_pi_v),
     &                 is(p_pi_ix),
     &                 ds(p_ed_wt),
     &                 is(p_og_t_nw_perm),
     &                 is(p_nw_t_og_perm))
               endif
               ds(p_du_act+vr_t_en_bs) = sv_du_act
               ds(p_ed_wt+ vr_t_en_bs) = sv_ed_wt
c
c     Price on all nonbasic structurals except those which are
c     permanently fixed.
c
               call ems_struc_pc(ds, is)
CM      IF (emsol_dev .EQ. 1) THEN
C?               if (ts_parsmi .gt. 0) call ems_ck_parsmi_v(4, ds, is)
CM      ENDIF
               call ems_ca_u_struc_dvx_wt(
     &              u_du_act,
     &              is(p_vr_in_c),
     &              ds(p_du_act),
     &              ds(p_tbu_r_v),
     &              is(p_tbu_r_ix),
     &              ds(p_ed_wt))
            end if
         else
            sv_du_act = ds(p_du_act+vr_t_en_bs)
            if (iand(inv_alg_msk, inv_alg_perm) .eq. 0 .or.
     &           iand(ml_da_st_msk, ml_da_st_r_mtx) .eq. 0) then
               call ems_u_lg_du_act(
     &              is(p_vr_in_c),
     &              ds(p_du_act),
     &              ds(p_pi_v),
     &              is(p_pi_ix))
            else
               call ems_perm_u_lg_du_act(
     &              is(p_vr_in_c),
     &              ds(p_du_act),
     &              ds(p_pi_v),
     &              is(p_pi_ix),
     &              is(p_og_t_nw_perm),
     &              is(p_nw_t_og_perm))
            endif
            ds(p_du_act+vr_t_en_bs) = sv_du_act
c
c     Price on all nonbasic structurals except those which are
c     permanently fixed.
c
            call ems_struc_pc(ds, is)
            call ems_u_struc_du_act(
     &           is(p_vr_in_c),
     &           ds(p_du_act),
     &           ds(p_tbu_r_v),
     &           is(p_tbu_r_ix))
         end if
         u_du_act = .false.
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl0) call ems_tt_rec(-pc_tt, n_bs)
CM      ENDIF
      end if
 7000 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9400 format('Iteration ', i7,
     &     ': no need to update dual activities ',
     &     'due to cancellation between dual activity and cost change.')
CM      IF (emsol_dev .EQ. 1) THEN
C? 9510 format('Iteration ', i7,
C?     &     ': pv_er = ', g11.4, ' pi_rhs_v_in_pv_r = ', g11.4,
C?     &     ': pv_er*pi_rhs_v_in_pv_r = ', g11.4)
CM      ENDIF
 9800 format('RSMI workspace not available in ems_rsmi_pc_sn')
      end
 
C->>> -------------------------------------------------> ems_iz_rsmi <<<
c     Allocates space for rsmi and determines a basis, ensuring that
c     the basic bits in the status vector correspond to the right
c     number of basic and nonbasic variables and that the lower and
c     upper bound bits are set correctly.
c
      subroutine ems_iz_rsmi(lp_alg_mode, lp_iz_mode, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMICS.INC'
      include 'RSMIHDL.INC'
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      integer lp_alg_mode, lp_iz_mode, is(0:is_n_en_m1)
      double precision ds(0:ds_n_en_m1)
      integer lc_lp_alg_mode
CM      IF (emsol_xa .EQ. 1) THEN
C?      integer hdl_te_mtx_c_v(0:hdl_z_m1), p_te_mtx_c_v
C?      integer hdl_te_mtx_c_ix(0:hdl_z_m1), p_te_mtx_c_ix
C?      integer hdl_te_mtx_r_sa(0:hdl_z_m1), p_te_mtx_r_sa
C?      integer hdl_te_du_lo_co(0:hdl_z_m1), p_te_du_lo_co
C?      integer hdl_te_du_up_co(0:hdl_z_m1), p_te_du_up_co
C?      integer mem_mgr_rt_cod
CM      ENDIF
      integer ix_n, vr_n
c
c     Check that the u_bs_cg_bt is not set for any variables: if it is,
c     it implies that the user has made an error in setting the status.
c
      do 10, ix_n = 1, n_c+n_r
         if (ix_n .le. n_c) then
            vr_n = ix_n
         else
            vr_n = ix_n + (mx_n_c-n_c)
         endif
         if (iand(is(p_st+vr_n), u_bs_cg_bt) .ne. 0) goto 8000
 10   continue
c
c     Use the primal algorithm unless the dual algorithm is specified
c
      lc_lp_alg_mode = lp_alg_mode
      if (lp_alg_mode .eq. lp_alg_mode_cz) then
         lc_lp_alg_mode = lp_alg_mode_pr
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
         call ems_msg_wr_li(84)
      end if
CM      IF (emsol_xa .EQ. 1) THEN
C?      if (lc_lp_alg_mode .eq. lp_alg_mode_du) then
C?         if (iand(ml_blk_st_msk, ml_blk_st_ml_bs_inv_p) .ne. 0)
C?     &        call ems_rm_blk_ml_bs_inv_p(is)
C?         call ems_mem_mgr_ope_a(mem_mgr_rt_cod, is,
C?     &        1+n_a_el, rl_wo_z, hdl_te_mtx_c_v)
C?         if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
C?            if (mem_mgr_rt_cod .eq. mem_mgr_rt_cod_serious_no_po) then
C?               ems_msg_cod = ems_msg_lvl_serious
C?               go to 7000
C?            else if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
C?               ems_msg_cod = ems_msg_lvl_serious
C?               go to 7000
C?            endif
C?         endif
C?         call ems_mem_mgr_ope_a(mem_mgr_rt_cod, is,
C?     &        1+n_a_el, i_wo_z, hdl_te_mtx_c_ix)
C?         if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
C?            if (mem_mgr_rt_cod .eq. mem_mgr_rt_cod_serious_no_po) then
C?               ems_msg_cod = ems_msg_lvl_serious
C?               go to 7000
C?            else if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
C?               ems_msg_cod = ems_msg_lvl_serious
C?               go to 7000
C?            endif
C?         endif
C?         call ems_mem_mgr_ope_a(mem_mgr_rt_cod, is,
C?     &        1+n_r+1, i_wo_z, hdl_te_mtx_r_sa)
C?         if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
C?            if (mem_mgr_rt_cod .eq. mem_mgr_rt_cod_serious_no_po) then
C?               ems_msg_cod = ems_msg_lvl_serious
C?               go to 7000
C?            else if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
C?               ems_msg_cod = ems_msg_lvl_serious
C?               go to 7000
C?            endif
C?         endif
C?         call ems_mem_mgr_ope_a(mem_mgr_rt_cod, is,
C?     &        1+mx_n_c+n_r, rl_wo_z, hdl_te_du_lo_co)
C?         if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
C?            if (mem_mgr_rt_cod .eq. mem_mgr_rt_cod_serious_no_po) then
C?               ems_msg_cod = ems_msg_lvl_serious
C?               go to 7000
C?            else if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
C?               ems_msg_cod = ems_msg_lvl_serious
C?               go to 7000
C?            endif
C?         endif
C?         call ems_mem_mgr_ope_a(mem_mgr_rt_cod, is,
C?     &        1+mx_n_c+n_r, rl_wo_z, hdl_te_du_up_co)
C?         if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
C?            if (mem_mgr_rt_cod .eq. mem_mgr_rt_cod_serious_no_po) then
C?               ems_msg_cod = ems_msg_lvl_serious
C?               go to 7000
C?            else if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
C?               ems_msg_cod = ems_msg_lvl_serious
C?               go to 7000
C?            endif
C?         endif
C?         call ems_mem_mgr_g_p8(mem_mgr_rt_cod, is,
C?     &        hdl_te_mtx_c_v, p_te_mtx_c_v)
C?         if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) goto 8800
C?         call ems_mem_mgr_g_p4(mem_mgr_rt_cod, is,
C?     &        hdl_te_mtx_c_ix, p_te_mtx_c_ix)
C?         if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) goto 8800
C?         call ems_mem_mgr_g_p4(mem_mgr_rt_cod, is,
C?     &        hdl_te_mtx_r_sa, p_te_mtx_r_sa)
C?         if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) goto 8800
C?         call ems_mem_mgr_g_p8(mem_mgr_rt_cod, is,
C?     &        hdl_te_du_lo_co, p_te_du_lo_co)
C?         if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) goto 8800
C?         call ems_mem_mgr_g_p8(mem_mgr_rt_cod, is,
C?     &        hdl_te_du_up_co, p_te_du_up_co)
C?         if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) goto 8800
C?         call ems_fo_du_ml(
C?     &        ds(p_nm),
C?     &        ds(p_lbc),
C?     &        ds(p_cbp),
C?     &        ds(p_ubc),
C?     &        ds(p_scl),
C?     &        is(p_st),
C?     &        ds(p_mtx_r_v),
C?     &        is(p_mtx_r_ix),
C?     &        is(p_mtx_c_sa),
C?     &        ds(p_te_mtx_c_v),
C?     &        is(p_te_mtx_c_ix),
C?     &        is(p_te_mtx_r_sa),
C?     &        ds(p_te_du_lo_co),
C?     &        ds(p_te_du_up_co))
C?         if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) goto 8800
C?         call ems_mem_mgr_rm_a(mem_mgr_rt_cod, is, hdl_te_mtx_c_v)
C?         if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
C?         call ems_mem_mgr_rm_a(mem_mgr_rt_cod, is, hdl_te_mtx_c_ix)
C?         if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
C?         call ems_mem_mgr_rm_a(mem_mgr_rt_cod, is, hdl_te_mtx_r_sa)
C?         if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
C?         call ems_mem_mgr_rm_a(mem_mgr_rt_cod, is, hdl_te_du_lo_co)
C?         if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
C?         call ems_mem_mgr_rm_a(mem_mgr_rt_cod, is, hdl_te_du_up_co)
C?         if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
C?         lc_lp_alg_mode = lp_alg_mode_pr
C?      endif
CM      ELSE
      lc_lp_alg_mode = lp_alg_mode_pr
CM      ENDIF
      if (iand(ml_da_st_msk, ml_da_st_alt_lp) .ne. 0 .and.
     &     repl_non_std_vr_mode_msk .ne. 0) then
c
c     If the model contains non-standard variables and the replacment
c     flag is set then do so by adding structurals
c
         if (iand(ml_blk_st_msk, ml_blk_st_ml_r_mtx) .ne. 0) then
            call ems_rm_blk_ml_r_mtx(is)
            if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
         endif
c
c     Indicate that any RSMI data structures are no longer correct.
c
         rsmi_blk_ml_n = 0
CM      IF (emsol_xa .EQ. 1) THEN
C?         if (repl_non_std_vr_mode_msk .ne. 0) then
C?            call ems_ca_repl_non_std_vr(ds, is)
C?            if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
C?         endif
CM      ENDIF
         ems_fi_wr_msk = -1
c         call ems_bcdo(ems_msg_cod, ds, 25, 2, 2)
         call ems_baso(ems_msg_cod, ds, 26, 1)
      endif
      if (lc_lp_alg_mode .eq. lp_alg_mode_pr) then
         du_sol_mode = du_sol_mode_no
      else
         du_sol_mode = du_sol_mode_y
      endif
c
c     If the model contains BP or PWL variables or if variables may
c     break bounds then prevent CHUZC in U_ED_WT and force L1-CHUZR.
c
      cz_c_msk = usr_cz_c_msk
      cz_r_msk = usr_cz_r_msk
c
c     Initialise a block for L1-CHUZR if there isn't one.
c
      if (iand(rsmi_blk_st_msk, rsmi_blk_st_l1_cz_r) .eq. 0) then
         call ems_iz_blk_l1_cz_r(is)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      endif
      if (iand(ml_da_st_msk, ml_da_st_alt_lp) .ne. 0 .or.
     &     iand(cz_c_msk, cz_c_bk_bd_bt) .ne. 0) then
         cz_r_msk = ior(cz_r_msk, cz_r_l1_bt)
      endif
      if (r_pc .ne. 0) then
c
c     In order to do row pricing a row-wise copy of the matrix must be
c     available. If space for this has not already been allocated then
c     do so now but indicate that the row-wise copy is not available.
c     ?? Re-allocate this space if it clashes with change in basis type.
c
         if (iand(ml_blk_st_msk, ml_blk_st_ml_r_mtx) .eq. 0) then
            call ems_iz_blk_ml_r_mtx(is)
            if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
            ml_da_st_msk =
     &           ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_r_mtx)
         endif
      else
c
c     Row pricing is not required so de-allocate any space that was
c     allocated (and indicate that the row-wise copy is not available).
c
         if (iand(ml_blk_st_msk, ml_blk_st_ml_r_mtx) .ne. 0)
     &        call ems_rm_blk_ml_r_mtx(is)
      end if
c
c     Check that that the bounds are consistent.
c
      call ems_ck_ml_bd(n_r, n_c,
     &     ds(p_lbc+1+mx_n_c), ds(p_ubc+1+mx_n_c),
     &     ds(p_lbc+1), ds(p_ubc+1), tl_mx_iz_pr_act)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
c
c     Set up the RSMI bounds and cost according to the type of variable
c     and the activity.
c
      call ems_ca_se_rsmi(ds, is)
c
c     Indicate that the solver data are correct for this model.
c
      rsmi_blk_ml_n = cu_ml_n
c
c     Write the header for log lines.
c
      if (rsmi_msg_msk .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9400)
         call ems_msg_wr_li(38)
         if (iand(rsmi_msg_msk, rsmi_pv_li_bt) .ne. 0) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9401)
         else
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9402)
         endif
         call ems_msg_wr_li(38)
      endif
c
c     Set pv_c_n = -1 and pv_r_n = -1 to indicate that no pivotal row or
c     column has yet been chosen.
c
      pv_c_n = -1
      pv_r_n = -1
c
c     Set lp_ph_cg and u_du_act_o_vr_t_en_bs to false. The former will
c     be set to true when feasibility is achieved/lost and the latter
c     will only be set to true for the parallel solver.
c
      lp_ph_cg = .false.
      u_du_act_o_vr_t_en_bs = .false.
 7000 continue
 7100 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)vr_n
      call ems_msg_wr_li(serious_msg_n)
      goto 7100
CM      IF (emsol_xa .EQ. 1) THEN
C? 8800 continue
C?      ems_msg_cod = ems_msg_lvl_serious
C?      goto 7100
CM      ENDIF
 9000 format('Using the primal algorithm')
 9400 format('Simplex   Primal              ',
     &     'Primal ifs         ',
     &     '  Dual ifs')
 9401 format('Iter      Objective     ',
     &     '      Sum      Number',
     &     '      Sum      Number',
     &     '   du_act     entering',
     &     '   alpha      leaving   pivot')
 9402 format('Iter      Objective     ',
     &     '      Sum      Number',
     &     '      Sum      Number')
 9800 format('Illegal status on entry for variable ', i7,
     &     ' : the u_bs_cg bit is set')
      end
 
C->>> ------------------------------------------------> ems_xit_rsmi <<<
c     Clean up the dual activities and recover any non-standard
c     variables which have been replaced using artificial structurals.
c
      subroutine ems_xit_rsmi(ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMIHDL.INC'
      include 'RSMICS.INC'
      include 'RSMICOM.INC'
      include 'RLCTVR.INC'
      include 'ICTVR.INC'
      include 'CHCTVR.INC'
      double precision ds(0:ds_n_en_m1)
      integer is(0:is_n_en_m1)
      integer r_n, vr_n
CM      IF (emsol_xa .EQ. 1) THEN
C?      integer rt_cod
CM      ENDIF
c
c     Zero the basic dual activities---any which have always been basic
c     are unassigned.
c
      do 10, r_n = 1, n_r
         vr_n = is(p_vr_in_r+r_n)
         ds(p_du_act+vr_n) = zero
 10   continue
CM      IF (emsol_xa .EQ. 1) THEN
C?      if (repl_non_std_vr_st_msk .ne. 0) then
C?c
C?c     Recover any non-standard variables which have been replaced
C?c     using artificial structurals.
C?c
C?         call ems_ca_rcov_non_std_vr(ds, is)
C?      endif
CM      ENDIF
      call ems_rp_rsmi_er(ds, is)
      return
      end
c->>> ------------------------------------------------> ems_xit_rsmi <<<
c     Negate the row duals: called by nget (if r_du_act_sgn > 0) for
c     consistency with OSL and should be called (if r_du_act_sgn < 0)
c     when using previous row duals in a hot start.
c
      subroutine ems_ng_r_du_act(ds)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      double precision ds(0:ds_n_en_m1)
      integer r_n
 
      do 20, r_n = mx_n_c+1, mx_n_c+n_r
         ds(p_du_act+r_n) = -ds(p_du_act+r_n)
 20   continue
      r_du_act_sgn = -r_du_act_sgn
      return
      end
