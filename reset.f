      subroutine ems_ck_inf_pr_act(lbc, ubc, pr_act)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMSG.INC'
      include 'ICTVR.INC'
      double precision lbc(0:mx_n_c+n_r)
      double precision ubc(0:mx_n_c+n_r)
      double precision pr_act(0:mx_n_c+n_r)
      integer c_n, r_n, vr_n
      integer n_inf_c_pr_act
      integer n_inf_r_pr_act
      logical alw_f7_wr
 
      alw_f7_wr = ems_msg_no_prt_fm .ge. 1
      n_inf_c_pr_act = 0
      do 10, c_n = 1, n_c
         if (
     &        pr_act(c_n) .ge.  inf*1d-1 .or.
     &        pr_act(c_n) .le. -inf*1d-1) then
            n_inf_c_pr_act = n_inf_c_pr_act + 1
c            if (n_inf_c_pr_act .le. 100 .and.
c     &           alw_f7_wr) write(*, 9000)'Col', c_n,
c     &           lbc(c_n), pr_act(c_n), ubc(c_n)
         endif
 10   continue
      n_inf_r_pr_act = 0
      do 20, r_n = 1, n_r
         vr_n = r_n + mx_n_c-n_c
         if (
     &        pr_act(vr_n) .ge.  inf*1d-1 .or.
     &        pr_act(vr_n) .le. -inf*1d-1) then
            n_inf_r_pr_act = n_inf_r_pr_act + 1
c            if (n_inf_r_pr_act .le. 100 .and.
c     &           alw_f7_wr) write(*, 9000)'Row', r_n,
c     &           lbc(vr_n), pr_act(vr_n), ubc(vr_n)
         endif
 20   continue
      return
c 9000 format('Infinite primal activity for ', a3, 1x, i7, 3(2x, g11.4))
      end
 
      subroutine ems_an_mv_t_bd(cg_n, mv_vr_n, dl,
     &     vr_in_r, pr_act, tbu_c, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      integer cg_n, mv_vr_n
      double precision dl, pr_act(0:mx_n_c+n_r), tbu_c(0:n_r)
      integer vr_in_r(0:n_r)
      integer is(0:is_n_en_m1)
      double precision ds(0:ds_n_en_m1)
      integer r_n, vr_n, ix_n
      character*3 ch3
      integer lc_n_pr_ifs
      double precision tbu_c_norm
      double precision lc_su_pr_ifs, mx_pr_ifs, mx_rlv_pr_ifs
 
      if (cg_n .eq. 1) then
         call ems_ca_g_n_su_mx_pr_ifs(
     &        lc_n_pr_ifs, lc_su_pr_ifs, mx_pr_ifs, mx_rlv_pr_ifs,
     &        ds, is)
c         write(*, 9000)
c         write(*, 9001)
c     &        lc_n_pr_ifs, lc_su_pr_ifs, mx_pr_ifs, mx_rlv_pr_ifs
      endif
      do 1, r_n = 1, n_r
         tbu_c(r_n) = zero
 1    continue
      call ems_g_rhs(1, mv_vr_n, tbu_c, n_r+1, ds, is)
      call ems_ftran(tbu_c, n_r+1, ds, is)
      tbu_c_norm = zero
      do 10, r_n = 1, n_r
         vr_n = vr_in_r(r_n)
         pr_act(vr_n) = pr_act(vr_n) + dl*tbu_c(r_n)
         tbu_c_norm = tbu_c_norm + tbu_c(r_n)*tbu_c(r_n)
         tbu_c(r_n) = zero
 10   continue
      tbu_c_norm = sqrt(tbu_c_norm)
      call ems_ca_g_n_su_mx_pr_ifs(
     &     lc_n_pr_ifs, lc_su_pr_ifs, mx_pr_ifs, mx_rlv_pr_ifs, ds, is)
      if (mv_vr_n .le. n_c) then
         ch3 = 'Col'
         ix_n = mv_vr_n
      else
         ch3 = 'Row'
         ix_n = mv_vr_n - mx_n_c
      endif
c      write(*, 9010)cg_n, ch3, ix_n, dl, tbu_c_norm,
c     &     lc_n_pr_ifs, lc_su_pr_ifs, mx_pr_ifs, mx_rlv_pr_ifs
      return
c 9000 format(/'Analysing moves of nonbasic variables to their bounds'/
c     &     57x, 'Primal infeasibilities'/
c     &     '  MvN  NonbasicVar     Move       ||TbuC||_2',
c     &     '        N       Su        MxAbs        MxRlv')
c 9001 format(46x, i7, 3(2x, g11.4))
c 9010 format(i5, 2x, a3, 1x, i7, 2(2x, g11.4), 2x, i7, 3(2x, g11.4))
      end
 
C->>> ----------------------------------------------> ems_reset_rsmi <<<
c     Set the nonbasic primal variables to their bounds, invert the
c     current basis (if required), solve for the basic primal variables
c     and determine the phase I/II dual activities.
c
      subroutine ems_reset_rsmi(reset_loop, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMIHDL.INC'
      include 'RSMICS.INC'
      include 'RSMICOM.INC'
      include 'SVMVBD.INC'
      include 'SVBSCG.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
c      include 'CHCTVR.INC'      !Only needed when dumping basis
      include 'EMSMSG.INC'
      include 'ITXITCS.INC'
      include 'EMSRTCOD.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      integer ems_rt_cod
      logical reset_loop
      integer is(0:is_n_en_m1)
      double precision ds(0:ds_n_en_m1)
c      integer ems_pos_mod
      integer it_xit_reason, usr_rt_cod
      integer rl_wk_a_ix1
      integer rl_wk_a_ix2
      integer i_wk_a_ix
      double precision mx_pr_ifs, mx_rlv_pr_ifs
      double precision mx_du_ifs, mx_rlv_du_ifs
CM      IF (emsol_deb .EQ. 1) THEN
C?      integer p_eta_fi_hdl
CM      ENDIF
      integer ca_ems_rt_cod
      logical mv_t_bd
      logical cg_bs
c      integer ln_t_l_ch
c      character*80 ch80_txt
c      character*13 ch13_vers
 
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tt_da .gt. 0) call ems_tt_rec(reset_rsmi_tt, n_bs)
CM      ENDIF
      ems_rt_cod = ems_rt_cod_ok
CM      IF (emsol_dev .EQ. 1) THEN
C?      call ems_mo_rsmi_reset(n_si_it, rq_reset)
CM      ENDIF
c      if (n_sv_mv_bd .gt. 0)
c     &     call ems_rcov_sv_mv_bd(ds(p_rsmi_lb), ds(p_rsmi_ub))
 100  continue
c
c     Set the EXPAND step and the current primal and dual infeasibility
c     tolerances.
c
      xp_tau = wk_xp_tau
c
c     If true EXPAND is used:
c
c      tl_pr_ifs = wk_tl_pr_ifs/two
c
      if (prob_st .eq. prob_st_op) then
c
c     Take the tolerances from the user-supplied tolerance.
c
         tl_pr_ifs = usr_tl_pr_ifs
         tl_du_ifs = usr_tl_du_ifs
         cz_r_pv_tl = usr_cz_r_pv_tl
      else
c
c     Take the tolerances from the larger of the working and
c     user-supplied tolerances.
c
         tl_pr_ifs =  max(wk_tl_pr_ifs,  usr_tl_pr_ifs)
         tl_du_ifs =  max(wk_tl_du_ifs,  usr_tl_du_ifs)
         cz_r_pv_tl = max(wk_cz_r_pv_tl, usr_cz_r_pv_tl)
      endif
c
c     Reset the status and value of the non-basic primal activities.
c
      if (prob_st .eq. prob_st_op .and. iand(sslv_xit_msk,
     &     sslv_xit_no_reset_non_bc_pr_act) .eq. 0) then
c
c     If the solution (appears to be) optimal and resetting nonbasic
c     primal activities has not specifically ruled out then do so now.
c
         mv_t_bd = .true.
      else
         mv_t_bd = .false.
      endif
c      call ems_g_rsmi_rl_wk_a_ix(rl_wk_a_ix1)
c      if (rl_wk_a_ix1 .lt. 0) goto 8000
c      call ems_g_rsmi_rl_wk_a_ix(rl_wk_a_ix2)
c      if (rl_wk_a_ix2 .lt. 0) goto 8000
      call ems_reset_non_bc_pr_act(
     &     mv_t_bd,
     &     ds(p_rsmi_lb),
     &     ds(p_rsmi_co),
     &     ds(p_rsmi_ub),
     &     is(p_st),
     &     ds(p_pr_act)
c     &     ,is(p_vr_in_r),
c     &     ds(p_rsmi_rl_wk_a(rl_wk_a_ix1)),
c     &     ds(p_rsmi_rl_wk_a(rl_wk_a_ix2)), ds, is
     &     )
c      call ems_fr_rsmi_rl_wk_a_ix(rl_wk_a_ix1)
c      call ems_fr_rsmi_rl_wk_a_ix(rl_wk_a_ix2)
      call ems_ck_inf_pr_act(ds(p_lbc), ds(p_ubc), ds(p_pr_act))
c
c     Form vr_in_c. Use `is' as a dummy argument.
c     Set lp_ph = 1 if bounds can be broken in phase I so that nonbasic
c     equalities are only temporarily fixed in g_vr_in_c_sn_ty_o_vr.
c
      if (iand(cz_c_msk, cz_c_bk_bd_bt) .ne. 0) lp_ph = 1
      call ems_g_vr_in_c(n_c, is(p_st), is, is(p_vr_in_c))
CM      IF (emsol_dev .EQ. 1) THEN
C?      call ems_g_n_pc_vr_el(n_pc_vr, n_pc_el,
C?     &     is(p_vr_in_c), is(p_mtx_c_sa))
CM      ENDIF
c
c     Indicate that since a new vr_in_c has been formed the row matrix
c     is not correct.
c
      ml_da_st_msk = ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_r_mtx)
      if (iand(wr_lp_da, wr_vr_in_c_bt) .ne. 0)
     &     call ems_rp_vr_in_c(0, is(p_vr_in_c), is(p_st), ds(p_du_act))
      if (iand(ml_da_st_msk, ml_da_st_r_mtx) .eq. 0 .and.
     &     iand(ml_blk_st_msk, ml_blk_st_ml_r_mtx) .ne. 0) then
c
c     If the row-wise representation of the matrix is not correct and
c     space for it is available then form it, using the space allocated
c     for pivotal column indices as workspace.
c
         call ems_g_rsmi_i_wk_a_ix(i_wk_a_ix)
         if (i_wk_a_ix .lt. 0) go to 8000
         call ems_g_ml_r_mtx(
     &        is(p_st),
     &        is(p_vr_in_c),
     &        ds(p_mtx_r_v),
     &        is(p_mtx_r_ix),
     &        is(p_mtx_c_sa),
     &        ds(p_mtx_c_v),
     &        is(p_mtx_c_ix),
     &        is(p_mtx_r_sa),
     &        is(p_rsmi_i_wk_a(i_wk_a_ix)))
         call ems_fr_rsmi_i_wk_a_ix(i_wk_a_ix)
         ml_da_st_msk = ml_da_st_msk + ml_da_st_r_mtx
      end if
c
c     Zero the values and pointers into the list of indices for the
c     basic costs and indicate that these vectors have been zeroed.
c     (Because se_bc_co uses this value to determine whether these
c     vectors should be zeroed.
c     Do this here because ems_rsmi_inv resets the basic costs.
c
      call ems_cp_rl_a(1+n_r, zero, ds(p_bc_co_v), 0)
      call ems_cp_i_a(1+n_r, 0, is(p_bc_co_ix_bar), 0)
      is(p_bc_co_ix) = 0
      if (rq_inv .ne. rq_inv_no_rq_inv) then
c
c     Invert the current basis.
c
         call ems_rsmi_inv(ds, is)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
         if (n_inv_sing .gt. 0) then
c
c     If the basis is singular then repeat the reset.
c
            prob_st = prob_st_unknown
            goto 100
         endif
c         call ems_msg_g_ch13_vers(ch13_vers, ln_t_l_ch)
c         ch80_txt = 'EMSOL '//ch13_vers(1:ln_t_l_ch)
c     &        //': Model = '//ch_ml_nm
c     &        //': Basis = '
c         call ems_wr_mtx_w_bs(
c     &        ch80_txt, n_bs,
c     &        is(p_vr_in_r),
c     &        ds(p_mtx_r_v),
c     &        is(p_mtx_r_ix),
c     &        is(p_mtx_c_sa))
CM      IF (emsol_da .EQ. 1) THEN
C?         p_eta_fi_hdl = p_ml_bs_blk + (cu_ml_n-1)*ml_bs_blk_n_wo +
C?     &        ml_bs_blk_os_hdl+ix_eta_fi_hdl
C?         call ems_ca_an_bs_mtx_and_inv(
C?     &        .false.,
C?     &        is(p_vr_in_r),
C?     &        is(p_eta_fi_hdl),
C?     &        ds, is)
C?         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
CM      ENDIF
      end if
c
c     If the condition of the basis is questionable then test it and,
c     if it looks ill-conditioned, form a well-conditioned triangular
c     basis using as many of the original basic variables as possible.
c
      if (iand(ml_da_st_msk, ml_da_st_bs_cond_ok) .eq. 0) then
         call ems_g_bs_cond_ok(cg_bs, ds, is)
c
c     Indicate that the condition of the basis should not be questioned
c
         ml_da_st_msk = ior(ml_da_st_msk, ml_da_st_bs_cond_ok)
         if (cg_bs) then
            rq_inv  = rq_inv_nw_bs
            goto 100
         endif
      endif
CM      IF (emsol_da .EQ. 1) THEN
C?      call ems_iz_inv_bs_bt(is(p_st), is(p_vr_in_r))
CM      ENDIF
c
c     Zero the vector of pivotal column values.
c
      call ems_cp_rl_a(1+n_r, zero, ds(p_pv_c_v), 0)
c
c     Zero the real array used to permute the FTRAN RHS.
c
      if (iand(inv_alg_msk, inv_alg_perm) .ne. 0) then
         call ems_cp_rl_a(1+n_r, zero, ds(p_perm_tran_vec), 0)
      endif
c
c     If the basis which was inverted was singular then indicate that
c     no basic primal activities are known.
c
      if (n_inv_sing .gt. 0) ml_da_st_msk =
     &     ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_bc_pr_act)
c
c     Solve A_B.x_B = -A_Nx_N for the basic primal activities if this is
c     the first simplex iteration or the previous basis was singular.
c     Otherwise just refine them according to (x_B := x_B+dx_B, where
c     A_B.dx_B = -(A_N.x_N+A_B.x_B).
c
      call ems_g_rsmi_rl_wk_a_ix(rl_wk_a_ix1)
      if (rl_wk_a_ix1 .lt. 0) goto 8000
      call ems_g_rsmi_rl_wk_a_ix(rl_wk_a_ix2)
      if (rl_wk_a_ix2 .lt. 0) goto 8000
      call ems_reset_bc_pr_act(
     &     is(p_st),
     &     ds(p_pr_act),
     &     is(p_vr_in_r),
     &     ds(p_rsmi_rl_wk_a(rl_wk_a_ix1)),
     &     ds(p_rsmi_rl_wk_a(rl_wk_a_ix2)),
     &     ds(p_pv_c_v),
     &     ds, is)
      call ems_fr_rsmi_rl_wk_a_ix(rl_wk_a_ix1)
      call ems_fr_rsmi_rl_wk_a_ix(rl_wk_a_ix2)
      call ems_ck_inf_pr_act(ds(p_lbc), ds(p_ubc), ds(p_pr_act))
c
c     If the primal infeasibility tolerance was zeroed to force
c     nonbasic variables onto their bounds then reset it to the
c     user-defined value. Note that if it is zero then optimality must
c     have been suspected and the basis found to be nonsingular
c     (otherwise prob_st is reset to prob_st_unknown and the repeat
c     reset assigns tl_pr_ifs = wk_tl_pr_ifs).
c
      if (tl_pr_ifs .le. zero) tl_pr_ifs = usr_tl_pr_ifs
c
c     Determine the LP phase.
c
      call ems_g_lp_ph(
     &     ds(p_rsmi_lb),
     &     ds(p_rsmi_co),
     &     ds(p_rsmi_ub),
     &     ds(p_pr_act),
     &     is(p_st),
     &     is(p_vr_in_r),
     &     ds, is)
      ml_da_st_msk =
     &     ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_non_bc_du_act)
c
c     Set the basic cost vector.
c
      call ems_se_bc_co(
     &     is(p_st),
     &     ds(p_rsmi_co),
     &     is(p_vr_in_r),
     &     ds(p_bc_co_v),
     &     is(p_bc_co_ix),
     &     is(p_bc_co_ix_bar))
c
c     Zero the vector of pi values.
c
      call ems_cp_rl_a(1+n_r, zero, ds(p_pi_v), 0)
c
c     Form the full pi = B^{-T}c_B, using iterative refinement
c
      call ems_g_rfn_fu_pi(ds, is)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
c
c     Zero the vector into which variables are priced.
c
      call ems_cp_rl_a(1+n_c, zero, ds(p_tbu_r_v), 0)
c
c     Determine the row and column duals.
c
      if (iand(inv_alg_msk, inv_alg_perm) .ne. 0)
     &     call ems_perm_btran_sol(
     &     p_pi_v, hdl_pi_v, is(p_pi_ix), ds, is)
CM      IF (emsol_deb .EQ. 1) THEN
C?      if (wr_lp_da .ne. 0) call ems_wr_lp_da(4, ds, is)
CM      ENDIF
      rq_re_pc = rq_re_pc_reset
CM      IF (emsol_dev .EQ. 1) THEN
C?      call ems_mo_rsmi_re_pc(n_si_it, rq_re_pc)
CM      ENDIF
c
c     Zero the logical dual activities in case values of pi are just
c     added in without checking whether the variable is basic or
c     nonbasic---thus causing an unassigned variable error with EPC.
c
      call ems_cp_rl_a(n_r, zero, ds(p_du_act+mx_n_c+1), 0)
      call ems_g_du_act_fm_pi(
     &     is(p_vr_in_c),
     &     ds(p_rsmi_co),
     &     ds(p_mtx_r_v),
     &     is(p_mtx_r_ix),
     &     is(p_mtx_c_sa),
     &     ds(p_pi_v),
     &     is(p_pi_ix),
     &     ds(p_du_act))
      rq_re_pc = rq_re_pc_no_rq_re_pc
c
c     If edge weights are not correct then initialise them
c
      if (iand(ml_da_st_msk, ml_da_st_ed_wt) .eq. 0) then
         call ems_iz_ed_wt(ds, is)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      endif
c
c     Indicate that no new Devex framework is required, either because
c     Devex weights are not being used, or because they have just been
c     initialised, or because they were correct before but
c
      nw_dvx_fwk = .false.
c
c     Reset the objective function value.
c
      call ems_ca_g_ml_ob_fn_v(ob_fn_v, ds, is)
c
c     Determine the number of, sum of and max primal infeasibilities
c
      call ems_ca_g_n_su_mx_pr_ifs(
     &     n_pr_ifs, su_pr_ifs, mx_pr_ifs, mx_rlv_pr_ifs, ds, is)
c
c     Determine the number of, sum of and max dual infeasibilities
c
      call ems_ca_g_n_su_mx_du_ifs(
     &     n_du_ifs, su_du_ifs, mx_du_ifs, mx_rlv_du_ifs, ds, is)
c
c     Determine the problem status.
c
      if (n_si_it .ge. mx_n_si_it) then
         prob_st = prob_st_mx_n_it
      else
         prob_st = prob_st_unknown
      endif
      if (n_du_ifs .eq. 0) then
c
c     Indicate that dual optimality has been obtained and that CHUZC has
c     been done for the current basis.
c
         vr_t_en_bs = 0
         rsmi_op_st_msk = ior(rsmi_op_st_msk, rsmi_op_st_cz_c)
         if (n_pr_ifs .eq. 0) then
            prob_st = prob_st_op
         else if (prob_st .eq. prob_st_unbd) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9600)
            call ems_msg_wr_li(3052)
         else
            prob_st = prob_st_ifs
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9610)
            call ems_msg_wr_li(3053)
         end if
      end if
      if (rsmi_msg_msk .ne. 0)
     &     call ems_wr_rsmi_lg_li(rsmi_lg_li_mode_reset, ds, is)
c
c     Allow a user exit at this point.
c
c      if (ems_pos_mod(n_si_it, it_usr_xit_fq) .eq. 0) then
      it_xit_reason = it_xit_af_reset
      call ems_it_xit(ds, is, it_xit_reason, usr_rt_cod)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      if (usr_rt_cod .eq. 3) then
         prob_st = prob_st_mx_n_it
         go to 1000
      else if (usr_rt_cod .eq. 99) then
         go to 100
      else if (iand(rsmi_op_st_msk, rsmi_op_st_cz_c) .eq. 0) then
         prob_st = prob_st_unknown
c         reset_rsmi_n_si_it = n_si_it - 1
      end if
c      endif
c
c     Indicate that there is no chosen variable and that the pivotal
c     column is neither packed nor in place.
c
 1000 continue
c
c     (Re-)start saving basis change records.
c
      call ems_sv_bs_cg_sa(ca_ems_rt_cod)
      if (ca_ems_rt_cod .ne. ems_rt_cod_ok) then
         ems_rt_cod = max(ca_ems_rt_cod, ems_rt_cod)
c         if (ems_rt_cod .ge. ems_rt_lvl_serious) goto 7100
         if (ems_rt_cod .ge. ems_rt_lvl_serious) goto 8950
      endif
c
c     Report the saved basis change data structure
c
c      call ems_sv_bs_cg_rp(ca_ems_rt_cod)
c      if (ca_ems_rt_cod .ne. ems_rt_cod_ok) then
c         ems_rt_cod = max(ca_ems_rt_cod, ems_rt_cod)
cc         if (ems_rt_cod .ge. ems_rt_lvl_serious) goto 7100
c         if (ems_rt_cod .ge. ems_rt_lvl_serious) goto 8950
c      endif
c
c     Check the saved basis change data structure
c
c      call ems_sv_bs_cg_ck(ca_ems_rt_cod,
c     &     is(p_og_bs_cg_st),
c     &     is(p_rsmi_i_wk_a(i_wk_a_ix)))
c      if (ca_ems_rt_cod .ne. ems_rt_cod_ok) then
c         ems_rt_cod = max(ca_ems_rt_cod, ems_rt_cod)
cc         if (ems_rt_cod .ge. ems_rt_lvl_serious) goto 7100
c         if (ems_rt_cod .ge. ems_rt_lvl_serious) goto 8950
c      endif
c
c     Check for a reset loop
c
      call ems_g_rsmi_i_wk_a_ix(i_wk_a_ix)
      if (i_wk_a_ix .lt. 0) go to 8000
      call ems_ck_reset_loop(ca_ems_rt_cod, ds, is,
     &     is(p_og_bs_cg_st),
     &     is(p_rsmi_i_wk_a(i_wk_a_ix)),
     &     reset_loop)
      call ems_fr_rsmi_i_wk_a_ix(i_wk_a_ix)
      if (ca_ems_rt_cod .ne. ems_rt_cod_ok) then
         ems_rt_cod = max(ca_ems_rt_cod, ems_rt_cod)
c     if (ems_rt_cod .ge. ems_rt_lvl_serious) goto 7100
c
c     Report the data structure in the event of an error
c
         call ems_sv_bs_cg_rp(ca_ems_rt_cod)
         if (ca_ems_rt_cod .ne. ems_rt_cod_ok) then
            ems_rt_cod = max(ca_ems_rt_cod, ems_rt_cod)
c     if (ems_rt_cod .ge. ems_rt_lvl_serious) goto 7100
            if (ems_rt_cod .ge. ems_rt_lvl_serious) goto 8950
         endif
 
         if (ems_rt_cod .ge. ems_rt_lvl_serious) goto 8950
      endif
      if (reset_loop) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9500)
         call ems_msg_wr_li(er_msg_n)
         call ems_sv_bs_cg_rp(ca_ems_rt_cod)
         if (ca_ems_rt_cod .ne. ems_rt_cod_ok) then
            ems_rt_cod = max(ca_ems_rt_cod, ems_rt_cod)
c     if (ems_rt_cod .ge. ems_rt_lvl_serious) goto 7100
            if (ems_rt_cod .ge. ems_rt_lvl_serious) goto 8950
         endif
      endif
      rsmi_op_st_msk = 0
      call ems_ck_rsmi_da(ds, is)
 7000 continue
      rq_reset = rq_reset_no_rq_reset
 7100 continue
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tt_da .gt. 0) call ems_tt_rec(-reset_rsmi_tt, n_bs)
CM      ENDIF
c      call ems_ca_an_bs_inv(ds, is)
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7100
 8950 continue
      ems_msg_cod = ems_msg_lvl_serious
      goto 7100
 9500 format('RESET LOOP: Reporting sv_bs_cg data structure')
 9600 format('The dual problem is infeasible')
 9610 format('The primal problem is infeasible')
 9800 format('RSMI workspace not available in ems_rsmi_reset')
      end
 
C->>> ------------------------------------------> ems_reset_bs_fm_st <<<
c     Reset a basis according to the status vector, if possible without
c     changing the existing basis.
c
      subroutine ems_reset_bs_fm_st(rsmi_lb, rsmi_ub, st, vr_in_r)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICS.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      include 'EMSMSGN.INC'
      double precision rsmi_lb(0:mx_n_c+n_r)
      double precision rsmi_ub(0:mx_n_c+n_r)
      integer st(0:mx_n_c+n_r), vr_in_r(0:n_r)
      integer r_n, k, vr_n, vr_st, n_vr_in_r, n_vr_in_c
 
      if (rq_inv .eq. rq_inv_no_rq_inv) then
         do 10, r_n = 1, n_r
            if (iand(st(vr_in_r(r_n)), bc_bt) .eq. 0) then
               rq_inv = rq_inv_reset_bs
               ml_da_st_msk =
     &              ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_inv)
               go to 100
            end if
 10      continue
      end if
 100  continue
      n_vr_in_r = 0
      n_vr_in_c = 0
      do 110, k = 1, n_c+n_r
         if (k .le. n_c) then
            vr_n = k
         else
            vr_n = k + mx_n_c-n_c
         end if
         vr_st = st(vr_n)
         if (iand(vr_st, bc_bt) .ne. 0) then
            if (n_vr_in_r .eq. n_r) then
c
c     Make the variable nonbasic if there is already a complete basis.
c
               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9900)
               call ems_msg_wr_li(rsmi_msg_n)
               rq_inv = rq_inv_reset_bs
               ml_da_st_msk =
     &              ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_inv)
               vr_st = vr_st - bc_bt
            end if
         else
            if (n_vr_in_c .eq. n_c) then
c
c     Make the variable basic if there is already a complete non-basis.
c
               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9910)
               call ems_msg_wr_li(rsmi_msg_n)
               rq_inv = rq_inv_reset_bs
               ml_da_st_msk =
     &              ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_inv)
               vr_st = vr_st + bc_bt
            end if
         end if
         if (iand(vr_st, bc_bt) .ne. 0) then
c
c     The variable is basic.
c
            n_vr_in_r = n_vr_in_r + 1
            if (rq_inv .ne. rq_inv_no_rq_inv) vr_in_r(n_vr_in_r) = vr_n
         else
c
c     The variable is nonbasic.
c
            n_vr_in_c = n_vr_in_c + 1
         end if
         st(vr_n) = vr_st
 110  continue
      if (n_vr_in_c .ne. n_c .or. n_vr_in_r .ne. n_r) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9920)
     &        n_vr_in_c, n_c, n_vr_in_r, n_r
         call ems_msg_wr_li(bug_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?         call ems_dump
CM      ENDIF
      end if
      do 210, r_n = 1, n_r
c
c     Record where the variable appears in vr_in_r.
c
         vr_n = vr_in_r(r_n)
         st(vr_n) = st(vr_n) - iand(st(vr_n), mx_mx_ml_a_dim) + r_n
 210  continue
      return
 9900 format('Too many basic vars in status list')
 9910 format('Too many nonbasic vars in status list')
 9920 format('n_vr_in_c .ne. n_c .or. n_vr_in_r .ne. n_r',
     &     i9, i9, i9, i9)
      end
 
C->>> -----------------------------------------> ems_reset_bs_fm_act <<<
c     Determine a basis according to the primal and dual activities, if
c     possible without changing the existing set of basic variables.
c
      subroutine ems_reset_bs_fm_act
     &     (rsmi_lb, rsmi_ub, st, pr_act, du_act, vr_in_r)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer st(0:mx_n_c+n_r), vr_in_r(0:n_r)
      double precision rsmi_lb(0:mx_n_c+n_r)
      double precision rsmi_ub(0:mx_n_c+n_r)
      double precision pr_act(0:mx_n_c+n_r)
      double precision du_act(0:mx_n_c+n_r)
 
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
 9800 format('Reset from activities is not yet implemented')
      return
      end
 
C->>> -------------------------------------> ems_reset_non_bc_pr_act <<<
c     Reset the status and activity of nonbasic primal activities.
c
      subroutine ems_reset_non_bc_pr_act(mv_t_bd,
     &     rsmi_lb, rsmi_co, rsmi_ub, st, pr_act
c     &     ,vr_in_r, sv_pr_act, tbu_c, ds, is
     &     )
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      logical mv_t_bd
      integer st(0:mx_n_c+n_r)
      double precision rsmi_lb(0:mx_n_c+n_r)
      double precision rsmi_co(0:mx_n_c+n_r)
      double precision rsmi_ub(0:mx_n_c+n_r)
      double precision pr_act(0:mx_n_c+n_r)
 
c      integer vr_in_r(0:n_r)
c      double precision sv_pr_act(0:mx_n_c+n_r)
c      double precision tbu_c(0:n_r)
c      integer is(0:is_n_en_m1)
c      double precision ds(0:ds_n_en_m1)
 
      integer k, vr_n, n_cg
      double precision norm_dl, dl
 
      norm_dl = zero
      n_cg = 0
      n_pr_ifs = 0
 
c      do vr_n = 1, n_r+n_c
c         sv_pr_act(vr_n) = pr_act(vr_n)
c      enddo
      do 10, k = 1, n_c+n_r
         if (k .le. n_c) then
            vr_n = k
         else
            vr_n = k-n_c+mx_n_c
         end if
         if (iand(st(vr_n), bc_bt) .eq. 0) then
            call ems_reset_1_non_bc_pr_act(
     &           mv_t_bd,
     &           vr_n,
     &           st(vr_n),
     &           rsmi_lb(vr_n), rsmi_co(vr_n), rsmi_ub(vr_n),
     &           pr_act(vr_n),
     &           tl_pr_ifs, dl)
            if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
            if (abs(dl) .gt. zero) then
               n_cg = n_cg + 1
               norm_dl = norm_dl + dl*dl
c               sv_pr_act(vr_n) = pr_act(vr_n)
c               call ems_an_mv_t_bd(n_cg, vr_n, dl,
c     &              vr_in_r, pr_act, tbu_c, ds, is)
            endif
         endif
 10   continue
c      do vr_n = 1, n_r+n_c
c         pr_act(vr_n) = sv_pr_act(vr_n)
c      enddo
      if (n_cg .gt. 0) then
c      if (norm_dl .gt. tl_pr_ifs) then
         norm_dl = sqrt(norm_dl)
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)n_cg, norm_dl
         call ems_msg_wr_li(info_msg_n)
      else if (mv_t_bd) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)
         call ems_msg_wr_li(info_msg_n)
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9020)
         call ems_msg_wr_li(info_msg_n)
      endif
      n_non_bc_pr_ifs = n_pr_ifs
 7000 continue
      return
 9000 format('    Resetting nonbasic variables:',
     &     i7, ' changes.      |dx| = ', g11.4)
 9010 format('    Resetting nonbasic variables:',
     &     ' No changes. mv_t_bd = .true.')
 9020 format('    Resetting nonbasic variables:',
     &     ' No changes. mv_t_bd = .false.')
      end
 
C->>> --------------------------------------------> ems_g_bs_cond_ok <<<
c     Check the condition of the basis and, if it looks ill-conditioned,
c     form a well-conditioned triangular basis using as many of the
c     original basic variables as possible.
c     cg_bs indicates whether the basis has been changed in order to
c     make it well conditioned.
c
      subroutine ems_g_bs_cond_ok(cg_bs, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMIHDL.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      logical cg_bs
      integer is(0:is_n_en_m1)
      double precision ds(0:ds_n_en_m1)
      integer rl_wk_a_ix1
      integer crsh_ty
      double precision ftran_rsdu_norm, btran_rsdu_norm
 
      cg_bs = .false.
      call ems_g_rsmi_rl_wk_a_ix(rl_wk_a_ix1)
      if (rl_wk_a_ix1 .lt. 0) goto 8000
      call ems_g_rand_tran_rsdu_norm(.true.,
     &     is(p_vr_in_r),
     &     ds(p_mtx_r_v), is(p_mtx_r_ix), is(p_mtx_c_sa),
     &     ds(p_pv_c_v),
     &     ds(p_rsmi_rl_wk_a(rl_wk_a_ix1)),
     &     ftran_rsdu_norm, btran_rsdu_norm, is, ds)
      call ems_fr_rsmi_rl_wk_a_ix(rl_wk_a_ix1)
      if (max(ftran_rsdu_norm, btran_rsdu_norm) .gt.
     &     tl_iz_bs_tran_er) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9500)
     &        ftran_rsdu_norm, btran_rsdu_norm
         call ems_msg_wr_li(warn_msg_n)
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
         call ems_msg_wr_li(info_msg_n)
         crsh_ty = 0
         cg_bs = .true.
         call ems_ca_crsh_ml(crsh_ty, ds, is)
      endif
 7100 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7100
 9100 format('Using the LTSSF crash to form a well-conditioned',
     &     ' triangular basis using as many of the basic variables',
     &     ' as possible')
 9500 format('Residual errors of ',
     &     g11.4, ' solving A.x = b and ',
     &     g11.4, ' solving A^x = b',
     &     ' indicate that the basis is ill-conditioned')
 9800 format('RSMI workspace not available in ems_g_bs_cond_ok')
      end
 
C->>> -----------------------------------------> ems_reset_bc_pr_act <<<
c     Solve A_B.x_B = -A_Nx_N for the basic primal activities if this is
c     the first simplex iteration or the previous basis was singular.
c     Otherwise just refine them according to (x_B := x_B+dx_B, where
c     A_B.dx_B = -(A_N.x_N+A_B.x_B)).
c
      subroutine ems_reset_bc_pr_act(
     &     st, pr_act, vr_in_r, sol_v, rhs_v, rsdu_v, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMICOM.INC'
      include 'RSMICS.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      include 'EMSRTCOD.INC'
      integer ems_rt_cod
      integer st(0:mx_n_c+n_r), vr_in_r(0:n_r), is(0:is_n_en_m1)
      double precision pr_act(0:mx_n_c+n_r)
      double precision sol_v(0:n_r)
      double precision rhs_v(0:n_r)
      double precision rsdu_v(0:n_r)
      double precision ds(0:ds_n_en_m1)
      integer r_n, c_n, el_n
      integer ca_ems_rt_cod
      integer vr_n
      double precision pr_act_v
 
      ems_rt_cod = ems_rt_cod_ok
      if (iand(ml_da_st_msk, ml_da_st_bc_pr_act) .eq. 0) then
c
c     Force a resolve by setting the basic primal activities to zero.
c
         do 5, r_n = 1, n_r
            pr_act(vr_in_r(r_n)) = zero
 5       continue
      endif
c
c     Want A_Bx_B + A_Nx_N = 0
c
c     Form x_B = -A_B^{-1}A_Nx_N by applying FTRAN to A_Nx_N
c
      do 210, r_n = 1, n_r
         vr_n = vr_in_r(r_n)
         rhs_v(r_n) = zero
         sol_v(r_n) = pr_act(vr_n)
 210  continue
      do 230, c_n = 1, n_c
         if (iand(st(c_n), bc_bt) .eq. 0) then
            pr_act_v = pr_act(c_n)
            if (pr_act_v .eq. zero) goto 230
            do 220 el_n = is(p_mtx_c_sa+c_n), is(p_mtx_c_sa+c_n+1)-1
               r_n = is(p_mtx_r_ix+el_n)
               rhs_v(r_n) = rhs_v(r_n) + pr_act_v*ds(p_mtx_r_v+el_n)
 220        continue
         endif
 230  continue
      do 240, r_n = 1, n_r
         if (iand(st(mx_n_c+r_n), bc_bt) .eq. 0) then
            pr_act_v = pr_act(mx_n_c+r_n)
            rhs_v(r_n) = rhs_v(r_n) - pr_act_v
         endif
 240  continue
      call ems_it_rfn(
     &     ca_ems_rt_cod, is, ds, 2,
     &     .false.,
     &     vr_in_r,
     &     ds(p_mtx_r_v), is(p_mtx_r_ix), is(p_mtx_c_sa),
     &     sol_v, n_r+1,
     &     rhs_v, n_r+1,
     &     rsdu_v,
     &     mx_n_bc_pr_act_rfn_it,
     &     tl_bc_pr_act_it_rfn,
     &     bc_pr_act_it_rfn_tran_ze,
     &     'Resetting basic primal activities')
      if (ca_ems_rt_cod .ne. ems_rt_cod_ok) then
         ems_rt_cod = max(ca_ems_rt_cod, ems_rt_cod)
         if (ems_rt_cod .ge. ems_rt_lvl_serious) goto 8950
      endif
c
c     Scatter the refined basic primal activities
c
      do 110, r_n = 1, n_r
         vr_n = vr_in_r(r_n)
         pr_act(vr_n) = sol_v(r_n)
 110  continue
c
c     Indicate that the model basic primal activities are now correct.
c
      ml_da_st_msk = ior(ml_da_st_msk, ml_da_st_bc_pr_act)
c
c     Indicate that the solver's basic costs are now incorrect.
c
      rsmi_da_st_msk =
     &     rsmi_da_st_msk - iand(rsmi_da_st_msk, rsmi_da_st_bc_co)
 7100 continue
      return
 8950 continue
      ems_msg_cod = ems_msg_lvl_serious
      goto 7100
      end
 
C->>> -----------------------------------> ems_reset_1_non_bc_pr_act <<<
c     Reset the primal activity of a non-basic variable according to its
c     status and bounds. If a variable violates a bound which it is
c     thought to satisfy then it is moved onto that bound, the thinking
c     being that this is for numerical reasons. If the ifs bit is set
c     then it is "allowed" to violate the bound.
c
c     Checks that dn, up, ifs bits are consistent with values.
c
      subroutine ems_reset_1_non_bc_pr_act(mv_t_bd,
     &     vr_n, vr_st, rsmi_lb, rsmi_co, rsmi_ub,
     &     pr_act, tl_pr_ifs, dl)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      logical mv_t_bd
      integer vr_n, vr_st
      double precision rsmi_lb, rsmi_co, rsmi_ub, pr_act, tl_pr_ifs, dl
      double precision og_pr_act, rsdu
 
      og_pr_act = pr_act
      if (iand(vr_st, alt_bt) .eq. 0) then
c
c     Variable is standard
c
         if (iand(vr_st, ub_bt) .eq. 0) then
            if (iand(vr_st, lb_bt) .eq. 0) then
c
c     A FR variable: Make sure that the up and dn bits are set but not
c     the ifs bit
c
               vr_st = ior(vr_st, up_dn)
               vr_st = vr_st - iand(vr_st, ifs_bt)
            else
c
c     A LB variable: Make sure that the up bit is set
c
               vr_st = ior(vr_st, up_bt)
               rsdu = rsmi_lb - pr_act
               if (rsdu .lt. -tl_pr_ifs) then
c
c     Variable is above its lower bound
c
                  vr_st = ior(vr_st, dn_bt)
                  vr_st = vr_st - iand(vr_st, ifs_bt)
               else if (rsdu .le. tl_pr_ifs) then
c
c     Variable is at its lower bound
c
                  if (mv_t_bd) pr_act = rsmi_lb
                  vr_st = vr_st - iand(vr_st, dn_bt)
                  vr_st = vr_st - iand(vr_st, ifs_bt)
               else
c
c     Variable is below its lower bound: move it onto the bound if it
c     was thought to be feasible.
c
                  if (iand(vr_st, ifs_bt) .eq. 0) pr_act = rsmi_lb
                  vr_st = vr_st - iand(vr_st, dn_bt)
               endif
            endif
         else
            if (iand(vr_st, lb_bt) .eq. 0) then
c
c     A UB variable: Make sure that the dn bit is set
c
               vr_st = ior(vr_st, dn_bt)
               rsdu = pr_act - rsmi_ub
               if (rsdu .lt. -tl_pr_ifs) then
c
c     Variable is below its upper bound
c
                  vr_st = ior(vr_st, up_bt)
                  vr_st = vr_st - iand(vr_st, ifs_bt)
               else if (rsdu .le. tl_pr_ifs) then
c
c     Variable is at its upper bound
c
                  if (mv_t_bd) pr_act = rsmi_ub
                  vr_st = vr_st - iand(vr_st, up_bt)
                  vr_st = vr_st - iand(vr_st, ifs_bt)
               else
c
c     Variable is above its upper bound: move it onto the bound if it
c     was thought to be feasible.
c
                  if (iand(vr_st, ifs_bt) .eq. 0) pr_act = rsmi_ub
                  vr_st = vr_st - iand(vr_st, up_bt)
               endif
            else if (rsmi_lb .ne. rsmi_ub) then
c
c     A LB/UB variable
c
               rsdu = max(rsmi_lb-pr_act, pr_act-rsmi_ub)
               if (rsdu .lt. -tl_pr_ifs) then
c
c     Variable is between its bounds
c
                  vr_st = ior(vr_st, up_dn)
                  vr_st = vr_st - iand(vr_st, ifs_bt)
               else if (rsdu .le. tl_pr_ifs) then
c
c     Variable is at a bound
c
                  if (two*pr_act .le. rsmi_lb+rsmi_ub) then
c
c     Variable is at its lower bound
c
                     if (mv_t_bd) pr_act = rsmi_lb
                     vr_st = vr_st - iand(vr_st, dn_bt)
                     vr_st = ior(vr_st, up_bt)
                     vr_st = vr_st - iand(vr_st, ifs_bt)
                  else
c
c     Variable is at its upper bound
c
                     if (mv_t_bd) pr_act = rsmi_ub
                     vr_st = ior(vr_st, dn_bt)
                     vr_st = vr_st - iand(vr_st, up_bt)
                     vr_st = vr_st - iand(vr_st, ifs_bt)
                  endif
               else if (pr_act .lt. rsmi_lb) then
c
c     Variable is below its lower bound: move it onto the bound if it
c     was thought to be feasible.
c
                  if (iand(vr_st, ifs_bt) .eq. 0) pr_act = rsmi_lb
                  vr_st = vr_st - iand(vr_st, dn_bt)
                  vr_st = ior(vr_st, up_bt)
               else
c
c     Variable is above its upper bound: move it onto the bound if it
c     was thought to be feasible.
c
                  if (iand(vr_st, ifs_bt) .eq. 0) pr_act = rsmi_ub
                  vr_st = ior(vr_st, dn_bt)
                  vr_st = vr_st - iand(vr_st, up_bt)
               endif
            else
c
c     A FX variable
c
               rsdu = max(rsmi_lb-pr_act, pr_act-rsmi_ub)
               if (rsdu .le. tl_pr_ifs) then
c
c     Variable is at its value
c
                  if (mv_t_bd) pr_act = rsmi_lb
                  vr_st = vr_st - iand(vr_st, dn_bt)
                  vr_st = vr_st - iand(vr_st, up_bt)
                  vr_st = vr_st - iand(vr_st, ifs_bt)
               else
c
c     Variable is off its value: move it onto the value if it
c     was thought to be feasible.
c
                  if (iand(vr_st, ifs_bt) .eq. 0) then
                     pr_act = rsmi_lb
                     vr_st = vr_st - iand(vr_st, dn_bt)
                     vr_st = vr_st - iand(vr_st, up_bt)
                     vr_st = vr_st - iand(vr_st, ifs_bt)
                  else if (pr_act .lt. rsmi_lb) then
                     vr_st = vr_st - iand(vr_st, dn_bt)
                     vr_st = ior(vr_st, up_bt)
                     vr_st = ior(vr_st, ifs_bt)
                  else
                     vr_st = ior(vr_st, dn_bt)
                     vr_st = vr_st - iand(vr_st, up_bt)
                     vr_st = ior(vr_st, ifs_bt)
                  endif
               endif
            endif
         end if
      else if (iand(vr_st, bp_bt) .ne. 0) then
c
c     Variable is BP
c
c     The ifs bit should not be set
c
         vr_st = vr_st - iand(vr_st, ifs_bt)
         if (iand(vr_st, lb_bt) .ne. 0) then
c
c     Variable is at/above its break point.
c
c     lb:co:ub are bp:uco:lco-uco
c
c     The ub bit should not be set
c
            vr_st = vr_st - iand(vr_st, ub_bt)
c
c     The up bit should be set
c
            vr_st = ior(vr_st, up_bt)
            rsdu = rsmi_lb - pr_act
            if (rsdu .lt. -tl_pr_ifs) then
c
c     The variable is above its break point
c
               vr_st = ior(vr_st, dn_bt)
            else if (rsdu .le. tl_pr_ifs) then
c
c     The variable is at its break point
c
               if (mv_t_bd) pr_act = rsmi_lb
               vr_st = vr_st - iand(vr_st, dn_bt)
            else
c
c     Variable is below its break point: move it onto its break point
c
               pr_act = rsmi_lb
               vr_st = vr_st - iand(vr_st, dn_bt)
            endif
         else
c
c     Variable is at/below its break point.
c
c     lb:co:ub are uco-lco:lco:bp
c
c     The lb bit should not be set
c
            vr_st = vr_st - iand(vr_st, lb_bt)
c
c     The dn bit should be set
c
            vr_st = ior(vr_st, dn_bt)
            rsdu = pr_act - rsmi_ub
            if (rsdu .lt. -tl_pr_ifs) then
c
c     The variable is below its break point
c
               vr_st = ior(vr_st, up_bt)
            else if (rsdu .le. tl_pr_ifs) then
c
c     The variable is at its break point
c
               if (mv_t_bd) pr_act = rsmi_ub
               vr_st = vr_st - iand(vr_st, up_bt)
            else
c
c     Variable is above its break point: move it onto its break point
c
               pr_act = rsmi_ub
               vr_st = vr_st - iand(vr_st, up_bt)
            endif
         endif
      else
c
c     Variable is PWL
c
      endif
      if (iand(vr_st, ifs_bt) .ne. 0) n_pr_ifs = n_pr_ifs + 1
c      dl = abs(pr_act-og_pr_act)
      dl = pr_act-og_pr_act
      if (abs(dl) .gt. tl_pr_ifs) then
c         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
c     &     rsmi_lb, og_pr_act, rsmi_ub, pr_act, dl
c         call ems_msg_wr_li(info_msg_n)
      endif
      return
c 9000 format(' lb = ', g11.4, ' og_pr_act = ', g11.4, ' ub = ', g11.4,
c     &     ' leads to nw_pr_act = ', g11.4, ':a change of ', g11.4)
      end
 
C->>> ------------------------------------> ems_reset_1_non_bc_vr_st <<<
c     Set the status of a nonbasic standard variable according to its
c     activity and bounds.
c     The difference between this routine and reset_1_non_bc_pr_act is
c     that variables off their bound are never changed: their
c     infeasibility is noted
c
      subroutine ems_reset_1_non_bc_vr_st(vr_n, sn_ty, vr_st,
     &     rsmi_lb, rsmi_co, rsmi_ub,
     &     pr_act, du_act, tl_pr_ifs)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer vr_n, sn_ty, vr_st
      double precision rsmi_lb, rsmi_co, rsmi_ub
      double precision pr_act, du_act, tl_pr_ifs
      double precision rsdu
      double precision prev_dl_obj_cf, dl_obj_cf
      double precision obj_cf_cg
CM      IF (emsol_deb .EQ. 1) THEN
C?      if (iand(vr_st, alt_bt) .ne. 0) goto 8000
CM      ENDIF
c
c     If the variable is not thought to be at a feasible value then
c     reduce the number of primal infeasibilities. If it is actually
c     not at a feasible value then this will be increased again at the
c     end of the routine.
c
c     Also determine the previous modification to the objective
c     coefficient so that any change can be determined.
c
      if (iand(vr_st, ifs_bt) .ne. 0) then
         if (iand(vr_st, dn_bt) .eq. 0) then
c
c     Previously below lower bound.
c
            prev_dl_obj_cf = -one
         else
c
c     Previously above upper bound.
c
            prev_dl_obj_cf =  one
         endif
         n_pr_ifs = n_pr_ifs - 1
      else
         prev_dl_obj_cf = zero
      endif
      dl_obj_cf = zero
      if (iand(vr_st, ub_bt) .eq. 0) then
         if (iand(vr_st, lb_bt) .eq. 0) then
c
c     A FR Variable: Make sure that the up and dn bits are set but not
c     the ifs bit.
c
            vr_st = ior(vr_st, up_dn)
            vr_st = vr_st - iand(vr_st, ifs_bt)
            sn_ty = vr_in_c_sn_ty_btw
         else
c
c     A LB variable: Make sure that the up bit is set
c
            vr_st = ior(vr_st, up_bt)
            rsdu = rsmi_lb - pr_act
            if (rsdu .lt. -tl_pr_ifs) then
c
c     Variable is above its lower bound
c
               vr_st = ior(vr_st, dn_bt)
               vr_st = vr_st - iand(vr_st, ifs_bt)
               sn_ty = vr_in_c_sn_ty_btw
            else if (rsdu .le. tl_pr_ifs) then
c
c     Variable is at its lower bound
c
               vr_st = vr_st - iand(vr_st, dn_bt)
               vr_st = vr_st - iand(vr_st, ifs_bt)
               sn_ty = vr_in_c_sn_ty_at_lb
            else
c
c     Variable is below its lower bound
c
               vr_st = vr_st - iand(vr_st, dn_bt)
               vr_st = ior(vr_st, ifs_bt)
               sn_ty = vr_in_c_sn_ty_bw_lb
               dl_obj_cf = -one
            endif
         endif
      else
         if (iand(vr_st, lb_bt) .eq. 0) then
c
c     A UB variable: Make sure that the dn bit is set
c
            vr_st = ior(vr_st, dn_bt)
            rsdu = pr_act - rsmi_ub
            if (rsdu .lt. -tl_pr_ifs) then
c
c     Variable is below its upper bound
c
               vr_st = ior(vr_st, up_bt)
               vr_st = vr_st - iand(vr_st, ifs_bt)
               sn_ty = vr_in_c_sn_ty_btw
            else if (rsdu .le. tl_pr_ifs) then
c
c     Variable is at its upper bound
c
               vr_st = vr_st - iand(vr_st, up_bt)
               vr_st = vr_st - iand(vr_st, ifs_bt)
               sn_ty = vr_in_c_sn_ty_at_ub
            else
c
c     Variable is above its upper bound
c
               vr_st = vr_st - iand(vr_st, up_bt)
               vr_st = ior(vr_st, ifs_bt)
               sn_ty = vr_in_c_sn_ty_ab_ub
               dl_obj_cf = one
            endif
         else if (rsmi_lb .ne. rsmi_ub) then
c
c     A LB/UB variable
c
            rsdu = max(rsmi_lb-pr_act, pr_act-rsmi_ub)
            if (rsdu .lt. -tl_pr_ifs) then
c
c     Variable is between its bounds
c
               vr_st = ior(vr_st, up_dn)
               vr_st = vr_st - iand(vr_st, ifs_bt)
               sn_ty = vr_in_c_sn_ty_btw
            else if (rsdu .le. tl_pr_ifs) then
c
c     Variable is at a bound
c
               if (two*pr_act .le. rsmi_lb+rsmi_ub) then
c
c     Variable is at its lower bound
c
                  vr_st = vr_st - iand(vr_st, dn_bt)
                  vr_st = ior(vr_st, up_bt)
                  vr_st = vr_st - iand(vr_st, ifs_bt)
                  sn_ty = vr_in_c_sn_ty_at_lb
               else
c
c     Variable is at its upper bound
c
                  vr_st = ior(vr_st, dn_bt)
                  vr_st = vr_st - iand(vr_st, up_bt)
                  vr_st = vr_st - iand(vr_st, ifs_bt)
                  sn_ty = vr_in_c_sn_ty_at_ub
               endif
            else if (pr_act .lt. rsmi_lb) then
c
c     Variable is below its lower bound
c
               vr_st = vr_st - iand(vr_st, dn_bt)
               vr_st = ior(vr_st, up_bt)
               vr_st = ior(vr_st, ifs_bt)
               sn_ty = vr_in_c_sn_ty_bw_lb
               dl_obj_cf = -one
            else
c
c     Variable is above its upper bound
c
               vr_st = ior(vr_st, dn_bt)
               vr_st = vr_st - iand(vr_st, up_bt)
               vr_st = ior(vr_st, ifs_bt)
               sn_ty = vr_in_c_sn_ty_ab_ub
               dl_obj_cf = one
            endif
         else
c
c     A FX variable
c
            rsdu = max(rsmi_lb-pr_act, pr_act-rsmi_ub)
            if (rsdu .le. tl_pr_ifs) then
c
c     Variable is at its value
c
               vr_st = vr_st - iand(vr_st, dn_bt)
               vr_st = vr_st - iand(vr_st, up_bt)
               vr_st = vr_st - iand(vr_st, ifs_bt)
c
c     >>>>>>>> Changed 23/07/97
c
c               sn_ty = vr_in_c_sn_ty_te_fx
               if (iand(vr_st, sos_bt) .ne. 0 .or.
     &              (lp_ph .eq. 1 .and.
     &              iand(cz_c_msk, cz_c_bk_bd_bt) .ne. 0)) then
c
c     Fix temporarily if the variable is SOS or bounds can be broken in
c     Phase I
c
                  sn_ty = vr_in_c_sn_ty_te_fx
               else
                  sn_ty = vr_in_c_sn_ty_fx
               endif
c
c     <<<<<<<<
c
            else
c
c     Variable is off its value
c
               if (pr_act .lt. rsmi_lb) then
                  vr_st = vr_st - iand(vr_st, dn_bt)
                  vr_st = ior(vr_st, up_bt)
                  vr_st = ior(vr_st, ifs_bt)
                  sn_ty = vr_in_c_sn_ty_bw_lb
                  dl_obj_cf = -one
               else
                  vr_st = ior(vr_st, dn_bt)
                  vr_st = vr_st - iand(vr_st, up_bt)
                  vr_st = ior(vr_st, ifs_bt)
                  sn_ty = vr_in_c_sn_ty_ab_ub
                  dl_obj_cf = one
               endif
            endif
         endif
      end if
c
c     Incorporate any cost changes into the dual activity.
c
      obj_cf_cg = dl_obj_cf - prev_dl_obj_cf
      if (obj_cf_cg .ne. zero) then
         du_act = du_act + obj_cf_cg
      endif
      if (iand(vr_st, ifs_bt) .ne. 0) n_pr_ifs = n_pr_ifs + 1
CM      IF (emsol_deb .EQ. 1) THEN
C? 7000 continue
CM      ENDIF
      return
CM      IF (emsol_deb .EQ. 1) THEN
C? 8000 continue
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)vr_n
C?      call ems_msg_wr_li(bug_msg_n)
C?      goto 7000
C? 9800 format('ems_reset_1_non_bc_vr_st called for variable ', i7,
C?     &     ' with Alt bit set')
CM      ENDIF
      end
 
C->>> ---------------------------------> ems_reset_1_non_bc_bp_vr_st <<<
c     Set the status of a nonbasic BP variable according to its activity
c     and BP data.
c     ?? Updates rsmi_lb, rsmi_co, rsmi_ub by forming costs by adding
c     delta causing (unlikely) unnecessary rounding error if
c     upper/lower costs differ greatly in magnitude.
c
      subroutine ems_reset_1_non_bc_bp_vr_st(vr_n, sn_ty, vr_st,
     &     rsmi_lb, rsmi_co, rsmi_ub,
     &     pr_act, du_act, tl_pr_ifs)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer vr_n, sn_ty, vr_st
      double precision rsmi_lb, rsmi_co, rsmi_ub
      double precision pr_act, du_act, tl_pr_ifs
      double precision co, bp, dl_co
      double precision rsdu, obj_cf_cg
CM      IF (emsol_deb .EQ. 1) THEN
C?      if (iand(vr_st, bc_bt) .ne. 0) goto 8000
C?      if (iand(vr_st, alt_bt) .eq. 0) goto 8010
C?      if (iand(vr_st, bp_bt) .eq. 0) goto 8020
CM      ENDIF
      vr_st = vr_st - iand(vr_st, ifs_bt)
      obj_cf_cg = zero
      if (iand(vr_st, lb_bt) .ne. 0) then
c
c     Variable was above BP
c
c     lb:co:ub are bp:uco:lco-uco
c
         bp = rsmi_lb
         co = rsmi_co
         dl_co = rsmi_ub
         rsdu = bp - pr_act
         if (rsdu .lt. -tl_pr_ifs) then
c
c     Variable is vr_still distinctly above its break point
c
            vr_st = ior(vr_st, up_dn)
            sn_ty = vr_in_c_sn_ty_btw
         else if (rsdu .le. tl_pr_ifs) then
c
c     Variable is at its break point:
c
c     Keep lb:co:ub
c     Ensure that the up bit is set and that the dn bit is not set
c
            vr_st = ior(vr_st, up_bt) - iand(vr_st, dn_bt)
            sn_ty = vr_in_c_sn_ty_ab_bp
         else
c
c     Variable is now distinctly below its break point
c
c     Set lb:co:ub to uco-lco:lco:bp
c
            rsmi_lb = -dl_co
            rsmi_co = co + dl_co
            rsmi_ub = bp
            obj_cf_cg = pr_co_mu*dl_co
c
c     Ensure that the up and down bits are set, remove the lb bit and
c     add the ub bit
c
            vr_st = ior(vr_st, up_dn)
            vr_st = vr_st - lb_bt + ub_bt
            sn_ty = vr_in_c_sn_ty_btw
         endif
      else
c
c     Variable was below BP
c
c     lb:co:ub are uco-lco:lco:bp
c
         bp = rsmi_ub
         co = rsmi_co
         dl_co = rsmi_lb
         rsdu = pr_act - bp
         if (rsdu .lt. -tl_pr_ifs) then
c
c     Variable is still distinctly below its break point
c
            vr_st = ior(vr_st, up_dn)
            sn_ty = vr_in_c_sn_ty_btw
         else if (rsdu .le. tl_pr_ifs) then
c
c     Variable is at its break point:
c
c     Keep lb:co:ub
c     Ensure that the dn bit is set and that the up bit is not set
c
            vr_st = ior(vr_st, dn_bt) - iand(vr_st, up_bt)
            sn_ty = vr_in_c_sn_ty_bw_bp
         else
c
c     Variable is now distinctly above its break point
c
c     Set lb:co:ub to bp:uco:lco-uco
c
            rsmi_lb = bp
            rsmi_co = co + dl_co
            rsmi_ub = -dl_co
            obj_cf_cg = pr_co_mu*dl_co
c
c     Ensure that the up and dn bits are set, remove the ub bit and add
c     the lb bit
c
            vr_st = ior(vr_st, up_dn)
            vr_st = vr_st - ub_bt + lb_bt
            sn_ty = vr_in_c_sn_ty_btw
         endif
      endif
c
c     Incorporate any cost changes into the dual activity.
c
      if (obj_cf_cg .ne. zero) then
         du_act = du_act + obj_cf_cg
      endif
CM      IF (emsol_deb .EQ. 1) THEN
C? 7000 continue
CM      ENDIF
      return
CM      IF (emsol_deb .EQ. 1) THEN
C? 8000 continue
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)vr_n
C?      call ems_msg_wr_li(bug_msg_n)
C?      goto 7000
C? 8010 continue
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)vr_n
C?      call ems_msg_wr_li(bug_msg_n)
C?      goto 7000
C? 8020 continue
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)vr_n
C?      call ems_msg_wr_li(bug_msg_n)
C?      goto 7000
C? 9800 format('ems_reset_1_non_bc_bp_vr_st called for variable ', i7,
C?     &     ' with BC bit set')
C? 9801 format('ems_reset_1_non_bc_bp_vr_st called for variable ', i7,
C?     &     ' with Alt bit not set')
C? 9802 format('ems_reset_1_non_bc_bp_vr_st called for variable ', i7,
C?     &     ' with PWL bit set')
CM      ENDIF
      end
 
C->>> --------------------------------> ems_reset_1_non_bc_pwl_vr_st <<<
c     Set the status of a nonbasic PWL variable according to its
c     activity and PWL data.
c
      subroutine ems_reset_1_non_bc_pwl_vr_st(vr_n, sn_ty, vr_st,
     &     rsmi_lb, rsmi_co, rsmi_ub,
     &     pwl_vr_da_v, pwl_vr_ls, pwl_vr_da_sa, pwl_vr_cu_sn,
     &     pr_act, du_act, tl_pr_ifs)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer vr_n, sn_ty, vr_st
      double precision rsmi_lb, rsmi_co, rsmi_ub
      double precision pr_act, du_act, tl_pr_ifs
      integer pwl_vr_ls(0:n_pwl_vr)
      integer pwl_vr_da_sa(0:n_pwl_vr+1)
      integer pwl_vr_cu_sn(0:n_pwl_vr)
      double precision pwl_vr_da_v(0:n_pwl_vr_da_en)
      double precision prev_obj_cf, obj_cf, obj_cf_cg
CM      IF (emsol_deb .EQ. 1) THEN
C?      if (iand(vr_st, bc_bt) .ne. 0) goto 8000
C?      if (iand(vr_st, alt_bt) .eq. 0) goto 8010
C?      if (iand(vr_st, bp_bt) .ne. 0) goto 8020
CM      ENDIF
c
c     If the variable is not thought to be at a feasible value then
c     reduce the number of primal infeasibilities. If it is actually
c     not at a feasible value then this will be increased again at the
c     end of the routine.
c
c     Also determine the previous objective coefficient so that any
c     change can be determined.
c
      if (iand(vr_st, ifs_bt) .ne. 0) then
         if (iand(vr_st, dn_bt) .eq. 0) then
c
c     Previously below lower bound.
c
            prev_obj_cf = -one + pr_co_mu*rsmi_co
         else
c
c     Previously above upper bound.
c
            prev_obj_cf =  one + pr_co_mu*rsmi_co
         endif
         n_pr_ifs = n_pr_ifs - 1
      else
         prev_obj_cf = pr_co_mu*rsmi_co
      endif
c
c     ?? Determine feas and coeff changes
c
      obj_cf = pr_co_mu*rsmi_co
c
c     Incorporate any cost changes into the dual activity.
c
      obj_cf_cg = obj_cf - prev_obj_cf
      if (obj_cf_cg .ne. zero) then
         du_act = du_act + obj_cf_cg
      endif
      if (iand(vr_st, ifs_bt) .ne. 0) n_pr_ifs = n_pr_ifs + 1
CM      IF (emsol_deb .EQ. 1) THEN
C? 7000 continue
CM      ENDIF
      return
CM      IF (emsol_deb .EQ. 1) THEN
C? 8000 continue
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)vr_n
C?      call ems_msg_wr_li(bug_msg_n)
C?      goto 7000
C? 8010 continue
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)vr_n
C?      call ems_msg_wr_li(bug_msg_n)
C?      goto 7000
C? 8020 continue
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)vr_n
C?      call ems_msg_wr_li(bug_msg_n)
C?      goto 7000
C? 9800 format('ems_reset_1_non_bc_pwl_vr_st called for variable ', i7,
C?     &     ' with BC bit set')
C? 9801 format('ems_reset_1_non_bc_pwl_vr_st called for variable ', i7,
C?     &     ' with Alt bit not set')
C? 9802 format('ems_reset_1_non_bc_pwl_vr_st called for variable ', i7,
C?     &     ' with PWL bit not set')
CM      ENDIF
      end
C->>> ----------------------------------------> ems_reset_1_bc_vr_st <<<
c     Set the status of a basic standard variable according to its
c     activity and bounds.
c
      subroutine ems_reset_1_bc_vr_st(vr_n, vr_st,
     &     rsmi_lb, rsmi_co, rsmi_ub,
     &     pr_act, obj_cf, obj_cf_cg, tl_pr_ifs)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer vr_n, vr_st
      double precision rsmi_lb, rsmi_co, rsmi_ub
      double precision pr_act, obj_cf, obj_cf_cg, tl_pr_ifs
      double precision rsdu, prev_dl_obj_cf, dl_obj_cf
 
CM      IF (emsol_deb .EQ. 1) THEN
C?      if (iand(vr_st, alt_bt) .ne. 0) goto 8000
CM      ENDIF
c
c     If the variable is not thought to be at a feasible value then
c     reduce the number of primal infeasibilities. If it is actually
c     not at a feasible value then this will be increased again at the
c     end of the routine.
c
c     Also determine the previous modification to the objective
c     coefficient so that any change can be determined.
c
      if (iand(vr_st, ifs_bt) .ne. 0) then
         if (iand(vr_st, dn_bt) .eq. 0) then
c
c     Previously below lower bound.
c
            prev_dl_obj_cf = -one
         else
c
c     Previously above upper bound.
c
            prev_dl_obj_cf =  one
         endif
         n_pr_ifs = n_pr_ifs - 1
      else
         prev_dl_obj_cf = zero
      endif
      dl_obj_cf = zero
c
c     Assume the variable is feasible and between bounds (NB this
c     includes the anomalous case of FX variables at their bounds.
c
      vr_st = vr_st - iand(vr_st, ifs_bt)
      vr_st = ior(vr_st, up_dn)
      if (iand(vr_st, lb_bt) .ne. 0) then
         rsdu = rsmi_lb - pr_act
         if (rsdu .gt. zero) then
c
c     The variable is at/below its lower bound
c     NB Still check rsdu .gt. zero because this could save forming the
c     UB residual.
c
c     If variable is below its lower bound remove the down bit and set
c     the ifs bit.
c
            if (rsdu .gt. tl_pr_ifs) then
               vr_st = vr_st - dn_bt + ifs_bt
               dl_obj_cf = -one
            endif
c
c     Variable must certainly  satisfy any upper bound.
c
            go to 100
         end if
      end if
      if (iand(vr_st, ub_bt) .ne. 0) then
         rsdu = pr_act - rsmi_ub
c
c     NB Don't have to check rsdu .gt. zero because only want to know if
c     the upper bound is violated.
c
c     If variable is above its upper bound remove the up and set the ifs
c     bit.
c
         if (rsdu .gt. tl_pr_ifs) then
            vr_st = vr_st - up_bt + ifs_bt
            dl_obj_cf = one
         endif
      end if
 100  continue
      obj_cf = dl_obj_cf + pr_co_mu*rsmi_co
      obj_cf_cg = dl_obj_cf - prev_dl_obj_cf
      if (iand(vr_st, ifs_bt) .ne. 0) n_pr_ifs = n_pr_ifs + 1
CM      IF (emsol_deb .EQ. 1) THEN
C? 7000 continue
CM      ENDIF
      return
CM      IF (emsol_deb .EQ. 1) THEN
C? 8000 continue
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)vr_n
C?      call ems_msg_wr_li(bug_msg_n)
C?      goto 7000
C? 9800 format('ems_reset_1_bc_vr_st called for variable ', i7,
C?     &     ' with Alt bit set')
CM      ENDIF
      end
C->>> -------------------------------------> ems_reset_1_bc_bp_vr_st <<<
c     Set the status of a basic BP variable according to its activity
c     and BP data.
c     ?? Updates rsmi_lb, rsmi_co, rsmi_ub by forming costs by adding
c     delta causing (unlikely) unnecessary rounding error if upper/lower
c     costs differ greatly in magnitude.
c
      subroutine ems_reset_1_bc_bp_vr_st(vr_n, vr_st,
     &     rsmi_lb, rsmi_co, rsmi_ub,
c     &     lco, bp, uco, scl,
     &     pr_act, obj_cf, obj_cf_cg, tl_pr_ifs)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer vr_n, vr_st
      double precision rsmi_lb, rsmi_co, rsmi_ub
      double precision pr_act, obj_cf, obj_cf_cg, tl_pr_ifs
c      double precision lco, bp, uco, scl
      double precision co, bp, dl_co, rsdu
c      double precision rcp_scl
CM      IF (emsol_deb .EQ. 1) THEN
C?      if (iand(vr_st, bc_bt) .eq. 0) goto 8000
C?      if (iand(vr_st, alt_bt) .eq. 0) goto 8010
C?      if (iand(vr_st, bp_bt) .eq. 0) goto 8020
CM      ENDIF
 
      obj_cf_cg = zero
      vr_st = vr_st - iand(vr_st, ifs_bt)
      vr_st = ior(vr_st, up_dn)
      if (iand(vr_st, lb_bt) .ne. 0) then
c
c     lb:co:ub are bp:uco:lco-uco
c
         bp = rsmi_lb
         co = rsmi_co
         dl_co = rsmi_ub
         rsdu = rsmi_lb - pr_act
         if (rsdu .gt. tl_pr_ifs) then
c
c     Variable has moved from above to below its breakpoint.
c
c     Set lb:co:ub to uco-lco:lco:bp
c
            rsmi_lb = -dl_co
            rsmi_co = co + dl_co
            rsmi_ub = bp
            vr_st = vr_st - lb_bt + ub_bt
            obj_cf_cg = pr_co_mu*dl_co
         endif
      else
c
c     lb:co:ub are uco-lco:lco:bp
c
         bp = rsmi_ub
         co = rsmi_co
         dl_co = rsmi_lb
         rsdu = pr_act - rsmi_ub
         if (rsdu .gt. tl_pr_ifs) then
c
c     Variable has moved from below to above its breakpoint.
c
c     Set lb:co:ub to bp:uco:lco-uco
c
            rsmi_lb = bp
            rsmi_co = co + dl_co
            rsmi_ub = -dl_co
            vr_st = vr_st + lb_bt - ub_bt
            obj_cf_cg = pr_co_mu*dl_co
         endif
      endif
      obj_cf = pr_co_mu*rsmi_co
CM      IF (emsol_deb .EQ. 1) THEN
C? 7000 continue
CM      ENDIF
      return
CM      IF (emsol_deb .EQ. 1) THEN
C? 8000 continue
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)vr_n
C?      call ems_msg_wr_li(bug_msg_n)
C?      goto 7000
C? 8010 continue
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)vr_n
C?      call ems_msg_wr_li(bug_msg_n)
C?      goto 7000
C? 8020 continue
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)vr_n
C?      call ems_msg_wr_li(bug_msg_n)
C?      goto 7000
C? 9800 format('ems_reset_1_bc_bp_vr_st called for variable ', i7,
C?     &     ' with BC bit not set')
C? 9801 format('ems_reset_1_bc_bp_vr_st called for variable ', i7,
C?     &     ' with Alt bit not set')
C? 9802 format('ems_reset_1_bc_bp_vr_st called for variable ', i7,
C?     &     ' with PWL bit set')
CM      ENDIF
      end
 
C->>> ------------------------------------> ems_reset_1_bc_pwl_vr_st <<<
c     Set the status of a basic PWL variable according to its activity
c     and PWL data.
c
      subroutine ems_reset_1_bc_pwl_vr_st(vr_n, vr_st,
     &     rsmi_lb, rsmi_co, rsmi_ub,
     &     pwl_vr_da_v, pwl_vr_ls, pwl_vr_da_sa, pwl_vr_cu_sn,
     &     pr_act, obj_cf, obj_cf_cg, tl_pr_ifs)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer vr_n, vr_st
      double precision rsmi_lb, rsmi_co, rsmi_ub
      double precision pr_act, obj_cf, obj_cf_cg, tl_pr_ifs
      integer pwl_vr_ls(0:n_pwl_vr)
      integer pwl_vr_da_sa(0:n_pwl_vr+1)
      integer pwl_vr_cu_sn(0:n_pwl_vr)
      double precision pwl_vr_da_v(0:n_pwl_vr_da_en)
      double precision prev_obj_cf
      double precision dl_obj_cf
CM      IF (emsol_deb .EQ. 1) THEN
C?      if (iand(vr_st, bc_bt) .eq. 0) goto 8000
C?      if (iand(vr_st, alt_bt) .eq. 0) goto 8010
C?      if (iand(vr_st, bp_bt) .ne. 0) goto 8020
CM      ENDIF
c
c     If the variable is not thought to be at a feasible value then
c     reduce the number of primal infeasibilities. If it is actually
c     not at a feasible value then this will be increased again at the
c     end of the routine.
c
c     Also determine the previous modification to the objective
c     coefficient so that any change can be determined.
c
      if (iand(vr_st, ifs_bt) .ne. 0) then
         if (iand(vr_st, dn_bt) .eq. 0) then
c
c     Previously below lower bound.
c
            prev_obj_cf = -one + pr_co_mu*rsmi_co
         else
c
c     Previously above upper bound.
c
            prev_obj_cf =  one + pr_co_mu*rsmi_co
         endif
         n_pr_ifs = n_pr_ifs - 1
      else
         prev_obj_cf = pr_co_mu*rsmi_co
      endif
      dl_obj_cf = zero
c
c     Assume the variable is feasible and between bounds
c
      vr_st = vr_st - iand(vr_st, ifs_bt)
      vr_st = ior(vr_st, up_dn)
      obj_cf = dl_obj_cf + pr_co_mu*rsmi_co
      obj_cf_cg = obj_cf - prev_obj_cf
      if (iand(vr_st, ifs_bt) .ne. 0) n_pr_ifs = n_pr_ifs + 1
CM      IF (emsol_deb .EQ. 1) THEN
C? 7000 continue
CM      ENDIF
      return
CM      IF (emsol_deb .EQ. 1) THEN
C? 8000 continue
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)vr_n
C?      call ems_msg_wr_li(bug_msg_n)
C?      goto 7000
C? 8010 continue
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)vr_n
C?      call ems_msg_wr_li(bug_msg_n)
C?      goto 7000
C? 8020 continue
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)vr_n
C?      call ems_msg_wr_li(bug_msg_n)
C?      goto 7000
C? 9800 format('ems_reset_1_bc_pwl_vr_st called for variable ', i7,
C?     &     ' with BC bit not set')
C? 9801 format('ems_reset_1_bc_pwl_vr_st called for variable ', i7,
C?     &     ' with Alt bit not set')
C? 9802 format('ems_reset_1_bc_pwl_vr_st called for variable ', i7,
C?     &     ' with PWL bit not set')
CM      ENDIF
      end
 
C->>> ------------------------------------------> ems_g_reset_reason <<<
c     Get the appropriate phrase for why it is necessary to RESET
c
      subroutine ems_g_reset_reason(rq_reset, reset_reason)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      integer rq_reset
      character*21 reset_reason
 
      if (rq_reset .eq. rq_reset_no_rq_reset) then
         reset_reason = 'No reset required!   '
      else if (rq_reset .eq. rq_reset_op) then
         reset_reason = 'Potentially optimal  '
      else if (rq_reset .eq. rq_reset_unbd) then
         reset_reason = 'Potentially unbounded'
      else if (rq_reset .eq. rq_reset_sing_bs) then
         reset_reason = 'Singular basis       '
      else if (rq_reset .eq. rq_reset_re_re_pc) then
         reset_reason = 'Trying to re-re-price'
      else if (rq_reset .eq. rq_reset_ze_pr_wr) then
         reset_reason = 'Zero primal weight   '
      else if (rq_reset .eq. rq_reset_cz_r_alg_er) then
         reset_reason = 'Alg. error in CHUZR  '
      else if (rq_reset .eq. rq_reset_usr_rq_reset) then
         reset_reason = 'User-instigated reset'
      else
         reset_reason = 'Unknown reason       '
      endif
      return
      end
CM      IF (emsol_dev .EQ. 1) THEN
C?      subroutine ems_wr_mtx_w_bs(txt, id,
C?     &     vr_in_r, mtx_r_v, mtx_r_ix, mtx_c_sa)
C?      implicit none
C?      include 'EMSV.INC'
C?      include 'EMSPM.INC'
C?      include 'ICTVR.INC'
C?      character*(*) txt
C?      integer id
C?      integer vr_in_r(0:n_r)
C?      integer mtx_r_ix(0:n_a_el)
C?      integer mtx_c_sa(0:n_c+1)
C?      double precision mtx_r_v(0:n_a_el)
C?      integer ems_ln_t_l_ch
C?      integer ln_t_l_ch
C?      integer r_n, c_n, el_n
C?      integer wr_cn
C? 
C?      wr_cn=11
C?      open(unit=wr_cn, file='mtx.dat')
C?      ln_t_l_ch = ems_ln_t_l_ch(txt)
C?      write(wr_cn, 9000)txt(1:ln_t_l_ch), id
C?      write(wr_cn, *)n_r, n_c, n_a_el
C?      write(wr_cn, 9100)(mtx_c_sa(c_n), c_n = 1, n_c+1)
C?      write(wr_cn, 9100)(mtx_r_ix(el_n), el_n = 1, n_a_el)
C?      write(wr_cn, 9110)(mtx_r_v(el_n), el_n = 1, n_a_el)
C?      write(wr_cn, 9100)(vr_in_r(r_n), r_n = 1, n_r)
C?      close(wr_cn)
C?      return
C? 9000 format(a, 1x, i9)
C? 9100 format(10(1x, i7))
C? 9110 format(5(1x, g19.12))
C?      end
CM      ENDIF
      subroutine ems_reset_tl_pr_ifs(ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      double precision ds(0:ds_n_en_m1)
      integer is(0:is_n_en_m1)
      double precision nw_tl_pr_ifs
 
      if (prob_st .eq. prob_st_op) then
c
c     Take the tolerances from the user-supplied tolerance.
c
         nw_tl_pr_ifs = usr_tl_pr_ifs
      else
c
c     Take the tolerances from the larger of the working and
c     user-supplied tolerances.
c
         nw_tl_pr_ifs = max(wk_tl_pr_ifs, usr_tl_pr_ifs)
      endif
      if (tl_pr_ifs .eq. nw_tl_pr_ifs) goto 7000
c
c     The primal feasibility tolerance has changed
c
c     If the solution is optimal and the primal feasibility tolerance is
c     increased then the status of each variable will still correspond
c     to its primal activity and the solution remains optimal.
c
      if (prob_st .eq. prob_st_op .and.
     &     tl_pr_ifs .lt. nw_tl_pr_ifs) then
         tl_pr_ifs = nw_tl_pr_ifs
         goto 7000
      endif
c
c     If the solution is optimal or the primal feasibility tolerance is
c     decreased then the status of each variable may not correspond to
c     its primal activity. Indicate this, and the fact that the problem
c     status is not known.
c
      tl_pr_ifs = nw_tl_pr_ifs
      ml_da_st_msk =
     &     ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_vr_st_fm_act)
      prob_st = prob_st_unknown
 7000 continue
      return
      end
 
      subroutine ems_reset_tl_du_ifs(ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      double precision ds(0:ds_n_en_m1)
      integer is(0:is_n_en_m1)
      double precision nw_tl_du_ifs
 
      if (prob_st .eq. prob_st_op) then
c
c     Take the tolerances from the user-supplied tolerance.
c
         nw_tl_du_ifs = usr_tl_du_ifs
      else
c
c     Take the tolerances from the larger of the working and
c     user-supplied tolerances.
c
         nw_tl_du_ifs = max(wk_tl_du_ifs, usr_tl_du_ifs)
      endif
      if (tl_du_ifs .eq. nw_tl_du_ifs) goto 7000
c
c     The dual feasibility tolerance has changed
c
c     If the solution is optimal and the dual feasibility tolerance is
c     increased then the solution remains optimal.
c
      if (prob_st .eq. prob_st_op .and.
     &     tl_du_ifs .lt. nw_tl_du_ifs) then
         tl_du_ifs = nw_tl_du_ifs
         goto 7000
      endif
c
c     If the solution is optimal or the dual feasibility tolerance is
c     decreased then the problem status is not known.
c
      tl_du_ifs = nw_tl_du_ifs
      prob_st = prob_st_unknown
 7000 continue
      return
      end
