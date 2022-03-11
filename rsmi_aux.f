C]
C->>> ------------------------------------------> ems_rm_rhs_ix_o_ze <<<
c     Removes the indices of zeros and repeated entries in pi
c
      subroutine ems_rm_rhs_ix_o_ze(rhs_v, rhs_ix, i_wk_a)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      double precision rhs_v(0:n_r)
      integer rhs_ix(0:n_r)
      integer i_wk_a(0:n_r)
      integer rhs_n_ix, ix_n, r_n
 
      if (rhs_ix(0) .gt. n_r) goto 7000
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tt_da .gt. 0) call ems_tt_rec(rm_rhs_ix_o_ze_tt, n_bs)
CM      ENDIF
      do 10, ix_n = 1, rhs_ix(0)
         i_wk_a(rhs_ix(ix_n)) = 1
 10   continue
      rhs_n_ix = 0
      do 20, ix_n = 1, rhs_ix(0)
         r_n = rhs_ix(ix_n)
         if (i_wk_a(r_n) .ne. 0) then
            if (rhs_v(r_n) .ne. zero) then
               rhs_n_ix = rhs_n_ix + 1
               rhs_ix(rhs_n_ix) = r_n
            endif
            i_wk_a(r_n) = 0
         endif
 20   continue
c      if (rhs_ix(0) .ne. rhs_n_ix)
c     &     print*, 'Iteration ', n_si_it,
c     &     ': Removed ', rhs_ix(0)-rhs_n_ix,
c     &     ' zeros and repeated entries from RHS'
      rhs_ix(0) = rhs_n_ix
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tt_da .gt. 0) call ems_tt_rec(-rm_rhs_ix_o_ze_tt, n_bs)
CM      ENDIF
 7000 continue
      return
      end
C->>> -----------------------------------------> ems_ca_g_ml_ob_fn_v <<<
c     Black box call to get the current objective value.
c
      subroutine ems_ca_g_ml_ob_fn_v(lc_ob_fn_v, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      integer is(0:is_n_en_m1)
      double precision ds(0:ds_n_en_m1)
      double precision lc_ob_fn_v
      double precision ems_g_ml_ob_fn_v
      lc_ob_fn_v = ems_g_ml_ob_fn_v(
     &     is(p_st),
     &     ds(p_rsmi_lb),
     &     ds(p_rsmi_co),
     &     ds(p_rsmi_ub),
     &     ds(p_pr_act), ds, is)
      return
      end
 
C->>> -------------------------------------> ems_ca_g_n_su_mx_pr_ifs <<<
c     Black box call to get the current number, sum and max of primal
c     infeasibilities.
c
      subroutine ems_ca_g_n_su_mx_pr_ifs(
     &     lc_n_pr_ifs, lc_su_pr_ifs, mx_pr_ifs, mx_rlv_pr_ifs,
     &     ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      integer is(0:is_n_en_m1)
      double precision ds(0:ds_n_en_m1)
      integer lc_n_pr_ifs
      double precision lc_su_pr_ifs, mx_pr_ifs, mx_rlv_pr_ifs
      call ems_g_n_su_mx_pr_ifs(
     &     ds(p_rsmi_lb),
     &     ds(p_rsmi_ub),
     &     ds(p_pr_act),
     &     is(p_st),
     &     lc_n_pr_ifs, lc_su_pr_ifs, mx_pr_ifs, mx_rlv_pr_ifs)
      return
      end
 
C->>> -------------------------------------> ems_ca_g_n_su_mx_du_ifs <<<
c     Black box call to get the current number, sum and max of dual
c     infeasibilities.
c
      subroutine ems_ca_g_n_su_mx_du_ifs(
     &     lc_n_du_ifs, lc_su_du_ifs, mx_du_ifs, mx_rlv_du_ifs,
     &     ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      integer is(0:is_n_en_m1)
      double precision ds(0:ds_n_en_m1)
      integer lc_n_du_ifs
      double precision lc_su_du_ifs, mx_du_ifs, mx_rlv_du_ifs
      call ems_g_n_su_mx_du_ifs(
     &     is(p_vr_in_c),
     &     ds(p_rsmi_lb),
     &     ds(p_rsmi_co),
     &     ds(p_rsmi_ub),
     &     ds(p_du_act),
     &     lc_n_du_ifs, lc_su_du_ifs, mx_du_ifs, mx_rlv_du_ifs)
      return
      end
 
C->>> ----------------------------------------------> ems_du_act_atr <<<
c     Determines whether a dual activity is attractive.
c
      logical function ems_du_act_atr(vr_st, du_act, tl_du_ifs)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      integer vr_st
      double precision du_act, tl_du_ifs
      logical du_act_atr
 
      if (iand(vr_st, ifs_bt) .ne. 0) then
c
c     Infeasible nonbasic variables are always free to move up or down
c     so the up and down bits are used to indicate the bound which is
c     violated---as for basic variables. This means that the up/down
c     bits of the entering variable are interpreted correctly in the
c     ratio test.
c
         du_act_atr = abs(du_act) .gt.  tl_du_ifs
      else
         du_act_atr =
     &        (iand(vr_st, dn_bt) .ne. 0 .and. du_act .gt.  tl_du_ifs)
     &        .or.
     &        (iand(vr_st, up_bt) .ne. 0 .and. du_act .lt. -tl_du_ifs)
      endif
      ems_du_act_atr = du_act_atr
      return
      end
 
C->>> ------------------------------------------------> ems_rsmi_inv <<<
c     Perform an INVERT
c
      subroutine ems_rsmi_inv(ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMICS.INC'
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      include 'MORSMI.INC'
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_eq_i2
CM      ENDIF
      double precision ds(0:ds_n_en_m1)
      integer is(0:is_n_en_m1)
c      logical ems_rq_nw_u_eta_grp
      integer ems_g_bs_mtx_n_a_en
      integer bs_mtx_n_a_en
      integer p_hdl_eta_grp, p_eta_grp
      double precision v
      integer mem_mgr_rt_cod
      integer rt_cod
c
c     Perform an INVERT.
c
c      if (iand(rsmi_msg_msk, rsmi_inv_li_bt) .ne. 0) then
      pre_eta_fi_n_eta = eta_fi_n_eta
      pre_eta_fi_n_ix = eta_fi_n_ix
c      endif
c
c     Remove the blocks associated with the previous INVERT.
c
      call ems_rm_inv(is)
      if (iand(ml_blk_st_msk, ml_blk_st_ml_bs_inv_p) .eq. 0) then
c
c     Allocate space for INVERT pointers.
c
         call ems_iz_blk_ml_bs_inv_p(is)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
         is(p_lo_eta_pv_in_r) = n_r
      endif
      call ems_inv(ds, is)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
CM      IF (emsol_da .EQ. 1) THEN
C?      if (iand(inv_msg_msk, bt3) .ne. 0)
C?     &     call ems_wr_inv_da(inv_log_msk, ds, is)
CM      ENDIF
      p_hdl_eta_grp = p_ml_bs_blk + (cu_ml_n-1)*ml_bs_blk_n_wo +
     &     ml_bs_blk_os_hdl + ix_eta_fi_hdl + (eta_fi_n_grp-1)*hdl_z
      call ems_mem_mgr_g_p4(mem_mgr_rt_cod, is,
     &     is(p_hdl_eta_grp), p_eta_grp)
      if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) goto 8800
      eta_fi_n_inv_grp = eta_fi_n_grp
      eta_fi_n_inv_se =  eta_fi_n_se
      eta_fi_n_inv_eta = eta_fi_n_eta
      eta_fi_n_inv_v =   eta_fi_n_v
      eta_fi_n_inv_ix =  eta_fi_n_ix
      if (av_eta_dse .lt. zero) then
         av_eta_dse = float(n_a_el)/(float(n_r)*float(n_c))
         if (eta_fi_n_inv_eta .gt. 0) then
            v = float(eta_fi_n_inv_ix)/float(n_r*eta_fi_n_inv_eta)
            av_eta_dse = max(av_eta_dse, v)
         endif
      endif
      bs_mtx_n_a_en =
     &     ems_g_bs_mtx_n_a_en(is(p_vr_in_r), is(p_mtx_c_sa))
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
C?      call ems_mo_rsmi_inv(n_si_it, rq_inv,
C?     &     eta_fi_n_eta, eta_fi_n_ix, av_eta_dse,
C?     &     pre_eta_fi_n_eta, pre_eta_fi_n_ix, bs_mtx_n_a_en)
CM      ENDIF
c
c     If the maximum number of updates has increased then re-allocate
c     the space for any dense data structures used for UPDATE.
c
      mx_n_u = usr_mx_n_u
      if (u_bs_mode .eq. u_bs_pf_r_cp) then
c
c     Take a copy of u_bs_mode:
c     u_bs      may be changed by RSMI
c     u_bs_mode may be changed by the user
c
         u_bs = u_bs_mode
         if (iand(ml_blk_st_msk, ml_blk_st_ml_u_bs) .ne. 0) then
            if (mx_n_u .gt. is(p_u_bs_skt_pv_r)) then
c
c     Remove the dense block if the user has pushed up the reinversion
c     frequency.
c
               call ems_rm_blk_ml_u_bs(is)
               if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
            endif
         endif
c
c     NB Don't combine these IF statements: ems_rm_blk_ml_u_bs will
c     change iand(ml_blk_st_msk, ml_blk_st_ml_u_bs)
c
        if (iand(ml_blk_st_msk, ml_blk_st_ml_u_bs) .eq. 0) then
c
c     Allocate space for the dense block if it does not exist.
c
            call ems_iz_blk_ml_u_bs(is)
            if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
         endif
      endif
c      if (ems_rq_nw_u_eta_grp(is(p_eta_grp))) then
c
c     Open an eta group for updates if there is not enough space in the
c     group declared for INVERT.
c
         p_hdl_eta_grp = p_hdl_eta_grp + hdl_z
         call ems_ope_u_eta_grp(is(p_hdl_eta_grp), ds, is)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
         eta_fi_n_grp = eta_fi_n_grp + 1
c      endif
      call ems_g_inv_p(rt_cod, is)
      if (rt_cod .ne. 0) goto 8020
CM      IF (emsol_da .EQ. 1) THEN
C?c      if (n_r .gt. 40000)
C?c     &     call ems_wr_eta_grp_n_wo(is(p_hdl_eta_grp), is)
CM      ENDIF
c
c     Maybe write a log line reporting on the INVERT.
c
      if (iand(rsmi_msg_msk, rsmi_inv_li_bt) .ne. 0)
     &     call ems_wr_rsmi_lg_li(rsmi_lg_li_mode_inv, ds, is)
      rq_inv = rq_inv_no_rq_inv
      ml_da_st_msk = ior(ml_da_st_msk, ml_da_st_inv)
c
c     Indicate that the indices of the new eta are not known. With
c     steepest edge, the pivotal column needs to be zeroed after an
c     INVERT following the detection in cz_r of potential growth.
c
      nw_eta_l_ix = n_r+1
 7000 continue
 7100 continue
      return
 8800 continue
      ems_msg_cod = ems_msg_lvl_serious
      goto 7100
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9802 format('Error in ems_g_inv_p')
      end
 
C->>> -------------------------------------------> ems_un_bd_aux_sol <<<
c     Put the ray of unboundedness into the auxiliary solve region.
c     ?? Implications when solving DUAL
c
      subroutine ems_un_bd_aux_sol(
     &     vr_in_r, nw_eta_v, nw_eta_ix, r_aux_sol, c_aux_sol)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
      integer vr_in_r(0:n_r), nw_eta_ix(0:nw_eta_l_ix)
      double precision nw_eta_v(0:n_r)
      double precision r_aux_sol(0:n_r), c_aux_sol(0:n_c)
      integer ix_n, r_n, c_n, vr_n
 
      do 10, r_n = 1, n_r
        r_aux_sol(r_n) = zero
 10   continue
      do 20, c_n = 1, n_c
        c_aux_sol(c_n) = zero
 20   continue
      do 30, ix_n = nw_eta_l_ix, nw_eta_f_ix, -1
         vr_n = vr_in_r(nw_eta_ix(ix_n))
         if (vr_n .gt. mx_n_c) then
            r_aux_sol(vr_n-mx_n_c) = mv_dir*nw_eta_v(ix_n)
         else
            c_aux_sol(vr_n) = mv_dir*nw_eta_v(ix_n)
         endif
 30   continue
      return
      end
 
C->>> ------------------------------------------------> ems_u_pr_act <<<
      subroutine ems_u_pr_act(
     &     nw_eta_sgn,
     &     pr_act,
     &     vr_in_r,
     &     nw_eta_v,
     &     nw_eta_ix)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'RSMICS.INC'
      include 'RSMICOM.INC'
CM      IF (emsol_da .EQ. 1) THEN
C?      include 'EMSDA.INC'
CM      ENDIF
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      integer nw_eta_sgn
      integer vr_in_r(0:n_r), nw_eta_ix(0:nw_eta_l_ix)
      double precision pr_act(0:mx_n_c+n_r)
      double precision nw_eta_v(0:n_r)
      integer vr_n, ix_n
      double precision mu
 
      if (aa .eq. zero) goto 7000
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_u_lvl1) call ems_tt_rec(u_rhs_tt, n_bs)
CM      ENDIF
      pr_act(vr_t_en_bs) = pr_act(vr_t_en_bs) + aa
      mu = nw_eta_sgn*aa
      do 10, ix_n = nw_eta_f_ix, nw_eta_l_ix
         vr_n = vr_in_r(nw_eta_ix(ix_n))
         pr_act(vr_n) = pr_act(vr_n) + mu*nw_eta_v(ix_n)
 10   continue
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_u_lvl1) call ems_tt_rec(-u_rhs_tt, n_bs)
CM      ENDIF
 7000 continue
      return
      end
C->>> --------------------------------------------------> ems_cz_1_c <<<
c     Returns the best candidate variable from the set of candidates.
c     If bst_vr_n = 0 then none of the candidates is attractive.
c
      subroutine ems_cz_1_c(vr_in_c, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMICS.INC'
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      include 'RLCTVR.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      include 'EMSRTCOD.INC'
      integer vr_in_c(-vr_in_c_n_sn:n_c)
      double precision ds(0:ds_n_en_m1)
      integer is(0:is_n_en_m1)
      integer vr_n
      double precision vr_du_act
      integer bp_swp_sd, en_c_n, en_sn_n, vr_st
      logical ab_t_bw
      integer ems_rt_cod, ca_ems_rt_cod
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_cz_c_lvl0) call ems_tt_rec(cz_c_tt, n_bs)
CM      ENDIF
      nx_vr_t_en_bs = 0
      bp_swp_sd = 0
      if (pc_alg .eq. pc_alg_sed) then
         call ems_sq_ed_wt_cz_1_c(
     &        bp_swp_sd, en_c_n, en_sn_n,
     &        ds(p_rsmi_lb), ds(p_rsmi_ub), is(p_st),
     &        vr_in_c, ds(p_du_act), ds(p_ed_wt))
      else if (pc_alg .eq. pc_alg_exact_dvx .or.
     &        pc_alg .eq. pc_alg_approx_dvx) then
         call ems_ed_wt_cz_1_c(
     &        bp_swp_sd, en_c_n, en_sn_n,
     &        ds(p_rsmi_lb), ds(p_rsmi_ub), is(p_st),
     &        vr_in_c, ds(p_du_act), ds(p_ed_wt))
      else
         call ems_dan_cz_1_c(
     &        bp_swp_sd, en_c_n, en_sn_n,
     &        ds(p_rsmi_lb), ds(p_rsmi_ub), is(p_st),
     &        vr_in_c, ds(p_du_act))
      endif
      if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      if (nx_vr_t_en_bs .gt. 0) then
c
c     Get the basis change section for the entering variable.
c
         call ems_g_bs_cg_st(ca_ems_rt_cod, 0, nx_vr_t_en_bs,
     &        is(p_st+nx_vr_t_en_bs),
     &        vr_in_c,
     &        en_vr_fm_bs_cg_st)
         if (ca_ems_rt_cod .ne. ems_rt_cod_ok) then
            ems_rt_cod = max(ca_ems_rt_cod, ems_rt_cod)
c     if (ems_rt_cod .ge. ems_rt_lvl_serious) goto 7100
            if (ems_rt_cod .ge. ems_rt_lvl_serious) goto 8950
         endif
      endif
c
c     Determine whether a variable has been chosen so that it breaks
c     through its bound.
c
      en_vr_bk_bd = 0
      if (nx_vr_t_en_bs .gt. 0)  then
         vr_n = nx_vr_t_en_bs
         vr_st = is(p_st+vr_n)
         if (iand(vr_st, alt_bt) .ne. 0 .and. bp_swp_sd .ne. 0) then
            ab_t_bw = bp_swp_sd .eq. -1
            call ems_bp_swp_sd(
     &           ab_t_bw, vr_n, en_c_n, en_sn_n,
     &           ds(p_rsmi_lb+vr_n),
     &           ds(p_rsmi_co+vr_n),
     &           ds(p_rsmi_ub+vr_n),
     &           ds(p_du_act+vr_n),
     &           is(p_st), vr_in_c)
         else if (lp_ph .eq. 1 .and.
     &           iand(cz_c_msk, cz_c_bk_bd_bt) .ne. 0) then
            vr_du_act = ds(p_du_act+vr_n)
            if (iand(vr_st, up_bt) .ne. 0) then
c
c     Identify whether the entering variable is breaking through its
c     lower bound.
c
               if (vr_du_act .gt. zero) en_vr_bk_bd = -1
            else if (iand(vr_st, dn_bt) .ne. 0) then
c
c     Identify whether the entering variable is breaking through its
c     upper bound.
c
               if (vr_du_act .lt. zero) en_vr_bk_bd = 1
            else
c
c     Identify which way the entering variable is moving from a fixed
c     value.
c
               if (vr_du_act .gt. zero) then
                  en_vr_bk_bd = -1
               else
                  en_vr_bk_bd = 1
               endif
            endif
         endif
      endif
c
c     Indicate that CHUZC has been performed with the current dual
c     activities and bounds on non-basic variables.
c
      rsmi_op_st_msk = ior(rsmi_op_st_msk, rsmi_op_st_cz_c)
 7000 continue
 7100 continue
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_cz_c_lvl0) call ems_tt_rec(-cz_c_tt, n_bs)
CM      ENDIF
      return
 8950 continue
      ems_msg_cod = max(ems_msg_lvl_serious, ems_msg_cod)
      goto 7100
      end
 
C->>> -------------------------------------------> ems_wr_rsmi_lg_li <<<
c     Write out a log line. Only local values of objective value and
c     number/sum of primal/dual infeasibilities are calculated so that
c     the global values are not affected by a call to this routine.
c
      subroutine ems_wr_rsmi_lg_li(rsmi_lg_li_mode, ds, is)
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
      integer rsmi_lg_li_mode, is(0:is_n_en_m1)
      double precision ds(0:ds_n_en_m1)
      integer lc_n_pr_ifs, lc_n_du_ifs
      double precision lc_ob_fn_v
      double precision lc_su_pr_ifs, mx_pr_ifs, mx_rlv_pr_ifs
      double precision lc_su_du_ifs, mx_du_ifs, mx_rlv_du_ifs
 
      if (rsmi_msg_msk .eq. 0) goto 7000
      call ems_ca_g_ml_ob_fn_v(
     &     lc_ob_fn_v, ds, is)
      call ems_ca_g_n_su_mx_pr_ifs(
     &     lc_n_pr_ifs, lc_su_pr_ifs, mx_pr_ifs, mx_rlv_pr_ifs,
     &     ds, is)
      call ems_ca_g_n_su_mx_du_ifs(
     &     lc_n_du_ifs, lc_su_du_ifs, mx_du_ifs, mx_rlv_du_ifs,
     &     ds, is)
      if (rsmi_lg_li_mode .eq. rsmi_lg_li_mode_fq) then
         if (iand(rsmi_msg_msk, rsmi_pv_li_bt) .ne. 0) then
            if (vr_t_lv_bs .ne. vr_t_en_bs) then
               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9300)
     &              n_si_it, lc_ob_fn_v,
     &              lc_su_pr_ifs, lc_n_pr_ifs,
     &              lc_su_du_ifs, lc_n_du_ifs,
     &              du_act_o_vr_t_en_bs, vr_t_en_bs, aa, vr_t_lv_bs, pv
            else
               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9300)
     &              n_si_it, lc_ob_fn_v,
     &              lc_su_pr_ifs, lc_n_pr_ifs,
     &              lc_su_du_ifs, lc_n_du_ifs,
     &              du_act_o_vr_t_en_bs, vr_t_en_bs, aa
            endif
         else
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9300)
     &           n_si_it, lc_ob_fn_v,
     &           lc_su_pr_ifs, lc_n_pr_ifs,
     &           lc_su_du_ifs, lc_n_du_ifs
         endif
      else if (rsmi_lg_li_mode .eq. rsmi_lg_li_mode_inv) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9300)
     &        n_si_it, lc_ob_fn_v,
     &        lc_su_pr_ifs, lc_n_pr_ifs,
     &        lc_su_du_ifs, lc_n_du_ifs
      else if (rsmi_lg_li_mode .eq. rsmi_lg_li_mode_dvx) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9320)
     &        n_si_it, lc_ob_fn_v,
     &        lc_su_pr_ifs, lc_n_pr_ifs,
     &        lc_su_du_ifs, lc_n_du_ifs,
     &        n_dvx_it
      else if (rsmi_lg_li_mode .eq. rsmi_lg_li_mode_reset) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9330)
     &        n_si_it, lc_ob_fn_v,
     &        lc_su_pr_ifs, lc_n_pr_ifs,
     &        lc_su_du_ifs, lc_n_du_ifs,
     &        eta_fi_n_eta, eta_fi_n_ix,
     &        n_reset
      endif
      call ems_msg_wr_li(80)
      call ems_flush(ems_wr_cn)
 7000 continue
      return
 9300 format(i7, 1x, g15.8, 2x, 2(1x, g11.4, '(', i7, ')'),
     &     2(1x, g11.4, 2x, i7), 1x, g11.4)
 9320 format(i7, 1x, g15.8, 2x, 2(1x, g11.4, '(', i7, ')'),
     &     ': Number of Devex iterations = ', i5)
 9330 format(i7, 1x, g15.8, 2x, 2(1x, g11.4, '(', i7, ')'),
     &     ': Eta file (', i7, ', ', i9, '): Reset ', i5)
      end
 
      integer function ems_g_bs_mtx_n_a_en(vr_in_r, mtx_c_sa)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      integer vr_in_r(0:n_r)
      integer mtx_c_sa(0:n_c+1)
      integer r_n, vr_n
      integer bs_mtx_n_a_en
 
      bs_mtx_n_a_en = 0
      do 10, r_n = 1, n_r
         vr_n = vr_in_r(r_n)
         if (vr_n .le. n_c) bs_mtx_n_a_en =
     &        bs_mtx_n_a_en + mtx_c_sa(vr_n+1) - mtx_c_sa(vr_n)
 10   continue
      ems_g_bs_mtx_n_a_en = bs_mtx_n_a_en
      return
      end
 
      double precision function ems_g_ml_ob_fn_v(
     &     st,
     &     rsmi_lb,
     &     rsmi_co,
     &     rsmi_ub,
     &     pr_act, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      integer st(0:mx_n_c+n_r)
      integer is(0:is_n_en_m1)
      double precision rsmi_lb(0:mx_n_c+n_r)
      double precision rsmi_co(0:mx_n_c+n_r)
      double precision rsmi_ub(0:mx_n_c+n_r)
      double precision pr_act(0:mx_n_c+n_r)
      double precision ds(0:ds_n_en_m1)
      double precision ems_scpr
      integer ix_n, vr_n, vr_st
      double precision ml_ob_fn_v
 
c      write(*, *)
c      write(*, *)'Analysing the function evaluation:'
      if (iand(ml_da_st_msk, ml_da_st_alt_lp) .eq. 0) then
         ml_ob_fn_v = ems_scpr(ob_fn_cs, rsmi_co, pr_act, n_c)
         ml_ob_fn_v = ems_scpr(ml_ob_fn_v,
     &        rsmi_co(mx_n_c), pr_act(mx_n_c), n_r)
c         ml_ob_fn_v = ob_fn_cs
c         mx_x = 0d0
c         mx_c = 0d0
c         mx_dl_f = 0d0
c         mx_f = 0d0
c         do 1, ix_n = 1, n_c+n_r
c            if (ix_n .le. n_c) then
c               vr_n = ix_n
c            else
c               vr_n = (mx_n_c-n_c) + ix_n
c            endif
c            ml_ob_fn_v = ml_ob_fn_v + rsmi_co(vr_n)*pr_act(vr_n)
c            dl_f = pr_act(vr_n)*rsmi_co(vr_n)
c            if (abs(dl_f) .gt. zero) then
c               if (iand(st(vr_n), bc_bt) .ne. 0) then
c                  ch8 = '   Basic'
c               else
c                  ch8 = 'NonBasic'
c               endif
c               write(*, 9000) ch8, ix_n,
c     &              pr_act(vr_n), rsmi_co(vr_n), dl_f, ml_ob_fn_v
c               mx_x = max(abs(pr_act(vr_n)), mx_x)
c               mx_c = max(abs(rsmi_co(vr_n)), mx_c)
c               mx_dl_f = max(abs(dl_f), mx_dl_f)
c               mx_f = max(abs(ml_ob_fn_v), mx_f)
c            endif
c 1       continue
c         write(*, 9000)'        ', -1, mx_x, mx_c, mx_dl_f, mx_f
         goto 7000
      endif
      ml_ob_fn_v = ob_fn_cs
      do 10, ix_n = 1, n_c+n_r
         if (ix_n .le. n_c) then
            vr_n = ix_n
         else
            vr_n = (mx_n_c-n_c) + ix_n
         endif
         vr_st = st(vr_n)
         if (iand(vr_st, alt_bt) .eq. 0) then
            ml_ob_fn_v = ml_ob_fn_v + rsmi_co(vr_n)*pr_act(vr_n)
         else if (iand(vr_st, bp_bt) .ne. 0) then
            if (iand(vr_st, lb_bt) .ne. 0) then
               ml_ob_fn_v = ml_ob_fn_v +
     &              rsmi_co(vr_n)*(pr_act(vr_n)-rsmi_lb(vr_n))
            else
               ml_ob_fn_v = ml_ob_fn_v +
     &              rsmi_co(vr_n)*(pr_act(vr_n)-rsmi_ub(vr_n))
            endif
         else
         endif
 10   continue
 7000 continue
      ems_g_ml_ob_fn_v = ml_ob_fn_v
      return
c 9000 format(a8, i5, 4(2x, g11.4))
      end
 
C->>> ----------------------------------------------> ems_u_ml_bs_cg <<<
c     A black box shell for u_bs_cg.
c
      subroutine ems_u_ml_bs_cg(ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      integer is(0:is_n_en_m1)
      double precision ds(0:ds_n_en_m1)
 
      call ems_u_bs_cg(is(p_st), is(p_vr_in_r), ds, is)
      return
      end
 
C->>> -------------------------------------------------> ems_u_bs_cg <<<
c     Resolve any undresolved basis changes.
c
      subroutine ems_u_bs_cg(st, vr_in_r, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMICS.INC'
CM      IF (emsol_da .EQ. 1) THEN
C?      include 'EMSDA.INC'
CM      ENDIF
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      include 'EMSRTCOD.INC'
      integer ems_rt_cod
      double precision ds(0:ds_n_en_m1)
      integer st(0:mx_n_c+n_r), vr_in_r(0:n_r)
      integer is(0:is_n_en_m1)
      integer lv_vr_ix, en_vr_ix, lv_vr_n, en_vr_n
      integer ca_ems_rt_cod
      integer lc_st
 
      ems_rt_cod = ems_rt_cod_ok
      if (u_bs_cg .eq. 0) goto 7000
      en_vr_ix = 0
c
c     Look for a variable which has the u_bs_cg bit set and is nonbasic.
c
      do 40, lv_vr_ix = 1, n_r+n_c
         if (lv_vr_ix .le. n_c) then
            lv_vr_n = lv_vr_ix
         else
            lv_vr_n = lv_vr_ix + mx_n_c - n_c
         endif
         if (iand(st(lv_vr_n), u_bs_cg_bt) .ne. 0 .and.
     &        iand(st(lv_vr_n), bc_bt) .eq. 0) then
c
c     Look for a variable which has the u_bs_cg bit set and is basic.
c
            do 20, en_vr_ix = en_vr_ix+1, n_r+n_c
               if (en_vr_ix .le. n_c) then
                  en_vr_n = en_vr_ix
               else
                  en_vr_n = en_vr_ix + mx_n_c - n_c
               endif
               if (iand(st(en_vr_n), u_bs_cg_bt) .ne. 0 .and.
     &              iand(st(en_vr_n), bc_bt) .ne. 0) goto 30
 20         continue
            goto 8010
 30         continue
CM      IF (emsol_da .EQ. 1) THEN
C?            n_sing_bs_cg = n_sing_bs_cg + 1
CM      ENDIF
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &           en_vr_n, lv_vr_n
            call ems_msg_wr_li(info_msg_n)
            st(lv_vr_n) = st(lv_vr_n) - u_bs_cg_bt
            st(en_vr_n) = st(en_vr_n) - u_bs_cg_bt
c
c     Get the basis change sections for the entering variables
c
c     Because the entering variable is already in vr_in_r, its status
c     is already basic and the index stored within the status points
c     into vr_in_r. However, to get ems_g_bs_cg_st to treat the variable
c     as nonbasic, the basic bit is removed from a copy of the status.
c     ems_og_g_bs_cg_st finds the variable in vr_in_c by searching.
c
            lc_st = st(en_vr_n) - iand(st(en_vr_n), bc_bt)
c            call ems_og_g_bs_cg_st(ca_ems_rt_cod, 1, en_vr_n,
c     &           lc_st,
c     &           ds(p_pr_act+en_vr_n),
c     &           ds(p_rsmi_lb+en_vr_n),
c     &           ds(p_rsmi_ub+en_vr_n),
c     &           en_vr_fm_bs_cg_st)
            call ems_g_bs_cg_st(ca_ems_rt_cod, 1, en_vr_n,
     &           lc_st,
     &           is(p_vr_in_c),
     &           en_vr_fm_bs_cg_st)
            if (ca_ems_rt_cod .ne. ems_rt_cod_ok) then
               ems_rt_cod = max(ca_ems_rt_cod, ems_rt_cod)
               if (ems_rt_cod .ge. ems_rt_lvl_serious) goto 7100
            endif
            call ems_u_vr_in_r(
     &           en_vr_n, lv_vr_n, is(p_st), is(p_vr_in_r))
         endif
 40   continue
c
c     Look for any more variables which have the u_bs_cg bit set.
c
      do 110, en_vr_ix = en_vr_ix+1, n_r+n_c
         if (en_vr_ix .le. n_c) then
            en_vr_n = en_vr_ix
         else
            en_vr_n = en_vr_ix + mx_n_c - n_c
         endif
         if (iand(st(lv_vr_n), u_bs_cg_bt) .ne. 0) goto 8020
 110  continue
      u_bs_cg = 0
 7000 continue
 7100 continue
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)lv_vr_n
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)en_vr_n
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9000 format('Resolved basis change: ', i7, ' replaces ', i7)
 9801 format('No unresolved basic variable for nonbasic variable ', i7)
 9802 format('No unresolved nonbasic variable for basic variable ', i7)
      end
 
C->>> ------------------------------------------------> ems_ml_bs_cg <<<
c     A black box shell for u_vr_in_r.
c
      subroutine ems_ml_bs_cg(lc_vr_t_en_bs, lc_vr_t_lv_bs, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      integer lc_vr_t_en_bs, lc_vr_t_lv_bs, is(0:is_n_en_m1)
      double precision ds(0:ds_n_en_m1)
 
      call ems_u_vr_in_r(
     &     lc_vr_t_en_bs, lc_vr_t_lv_bs, is(p_st), is(p_vr_in_r))
      return
      end
 
C->>> -----------------------------------------------> ems_u_vr_in_r <<<
c     If lc_vr_t_en_bs=0 or lc_vr_t_lv_bs=0 then just set/unset the
c     basic bit.
c     Update vr_in_r corresponding to lc_vr_t_en_bs and lc_vr_t_lv_bs,
c     maintaining the cross-referencing information in the status
c     vector.
c
      subroutine ems_u_vr_in_r(
     &     lc_vr_t_en_bs, lc_vr_t_lv_bs, st, vr_in_r)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMSG.INC'
      include 'EMSRTCOD.INC'
      include 'RSMICOM.INC'
      include 'RSMICS.INC'
      include 'ICTVR.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      integer ems_rt_cod
      integer st(0:mx_n_c+n_r)
      integer vr_in_r(0:n_r)
      integer lc_vr_t_en_bs, lc_vr_t_lv_bs
      integer ca_ems_rt_cod
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_u_lvl1) call ems_tt_rec(u_vr_in_r_tt, n_bs)
CM      ENDIF
      ems_rt_cod = ems_rt_cod_ok
c
c     Ensure that the basic bit is set for the entering variable.
c
      if (lc_vr_t_en_bs .gt. 0) then
         st(lc_vr_t_en_bs) = ior(st(lc_vr_t_en_bs), bc_bt)
      endif
c
c     Ensure that the basic bit is not set for the leaving variable.
c
      if (lc_vr_t_lv_bs .gt. 0) then
         st(lc_vr_t_lv_bs) =
     &        st(lc_vr_t_lv_bs) - iand(st(lc_vr_t_lv_bs), bc_bt)
      endif
c
c     If a variable is entering/leaving the basis but there is nothing
c     to remove/replace it. Set a flag to indicate that such anomalies
c     must be resolved and put a marker on each such variable.
c
      if (lc_vr_t_en_bs .le. 0 .or. lc_vr_t_lv_bs .le. 0) then
         if (lc_vr_t_en_bs .gt. 0)
     &        st(lc_vr_t_en_bs) = st(lc_vr_t_en_bs) -
     &        iand(st(lc_vr_t_en_bs), u_bs_cg_bt) + u_bs_cg_bt
         if (lc_vr_t_lv_bs .gt. 0)
     &        st(lc_vr_t_lv_bs) = st(lc_vr_t_lv_bs) -
     &        iand(st(lc_vr_t_lv_bs), u_bs_cg_bt) + u_bs_cg_bt
         u_bs_cg = 1
         goto 7000
      endif
c
c     Save the basis change so that reset loops can be detected.
c
      call ems_sv_bs_cg(ca_ems_rt_cod,
     &     lc_vr_t_en_bs, lc_vr_t_lv_bs,
     &     en_vr_fm_bs_cg_st, bs_cg_st_bc)
      if (ca_ems_rt_cod .ne. ems_rt_cod_ok) then
         ems_rt_cod = max(ca_ems_rt_cod, ems_rt_cod)
c         if (ems_rt_cod .ge. ems_rt_lvl_serious) goto 7100
         if (ems_rt_cod .ge. ems_rt_lvl_serious) goto 8950
      endif
c
c     Get pv_c_n and pv_r_n from the corresponding statuses.
c
      pv_c_n = iand(st(lc_vr_t_en_bs), mx_mx_ml_a_dim)
      if (lc_vr_t_lv_bs .ne. lc_vr_t_en_bs) then
         pv_r_n = iand(st(lc_vr_t_lv_bs), mx_mx_ml_a_dim)
c
c     Update the record of which variable is solved for in which row.
c
         vr_in_r(pv_r_n) = lc_vr_t_en_bs
         st(lc_vr_t_en_bs) = st(lc_vr_t_en_bs) -
     &        iand(st(lc_vr_t_en_bs), mx_mx_ml_a_dim) + pv_r_n
      else
         pv_r_n = 0
      endif
 7000 continue
 7100 continue
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_u_lvl1) call ems_tt_rec(-u_vr_in_r_tt, n_bs)
CM      ENDIF
      return
 8950 continue
      ems_msg_cod = ems_msg_lvl_serious
      goto 7100
      end
 
C->>> -------------------------------------------------> ems_u_bc_co <<<
c     Update the costs of the basic variables.
c
      subroutine ems_u_bc_co(
     &     rsmi_lb,
     &     rsmi_ub,
     &     st,
     &     rsmi_co,
     &     pr_act,
     &     vr_in_r,
     &     bc_co_v,
     &     bc_co_ix,
     &     bc_co_ix_bar,
     &     pi_v,
     &     pi_ix,
     &     du_act,
     &     cdd_r,
     &     vr_in_c,
     &     ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      include 'RLCTVR.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      integer bc_co_ix(0:n_r)
      integer bc_co_ix_bar(0:n_r)
      integer pi_ix(0:n_r)
      integer st(0:mx_n_c+n_r), vr_in_r(0:n_r)
      integer cdd_r(0:n_r)
      integer vr_in_c(*), is(0:is_n_en_m1)
      double precision rsmi_lb(0:mx_n_c+n_r)
      double precision rsmi_ub(0:mx_n_c+n_r)
      double precision rsmi_co(0:mx_n_c+n_r)
      double precision pr_act(0:mx_n_c+n_r)
      double precision bc_co_v(0:n_r)
      double precision pi_v(0:n_r)
      double precision du_act(0:mx_n_c+n_r)
      double precision ds(0:ds_n_en_m1)
      integer r_o_l_ix, vr_n, vr_st
      integer n_cdd_r, cdd_r_n, r_n
      double precision obj_cf_o_vr_t_en_bs, obj_cf_cg_o_vr_t_en_bs
      double precision obj_cf, obj_cf_cg
      logical al_cdd_ifs
c      double precision prev_dl_obj_cf, dl_obj_cf, rsdu
c
c     The variable to enter the basis must be reset. If there has been a
c     bound swap then this will have been done in u_vr_in_c.
c
      if (vr_t_en_bs .eq. vr_t_lv_bs) goto 1000
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_u_lvl1) call ems_tt_rec(u_en_vr_co_tt, n_bs)
CM      ENDIF
      vr_st = st(vr_t_en_bs)
      if (iand(st(vr_t_en_bs), alt_bt) .eq. 0) then
         call ems_reset_1_bc_vr_st(
     &        vr_t_en_bs,
     &        st(vr_t_en_bs),
     &        rsmi_lb(vr_t_en_bs),
     &        rsmi_co(vr_t_en_bs),
     &        rsmi_ub(vr_t_en_bs),
     &        pr_act(vr_t_en_bs),
     &        obj_cf_o_vr_t_en_bs,
     &        obj_cf_cg_o_vr_t_en_bs,
     &        tl_pr_ifs)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      else if (iand(st(vr_t_en_bs), bp_bt) .ne. 0) then
         call ems_reset_1_bc_bp_vr_st(
     &        vr_t_en_bs,
     &        st(vr_t_en_bs),
     &        rsmi_lb(vr_t_en_bs),
     &        rsmi_co(vr_t_en_bs),
     &        rsmi_ub(vr_t_en_bs),
     &        pr_act(vr_t_en_bs),
     &        obj_cf_o_vr_t_en_bs,
     &        obj_cf_cg_o_vr_t_en_bs,
     &        tl_pr_ifs)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      else
         call ems_reset_1_bc_pwl_vr_st(
     &        vr_t_en_bs,
     &        st(vr_t_en_bs),
     &        rsmi_lb(vr_t_en_bs),
     &        rsmi_co(vr_t_en_bs),
     &        rsmi_ub(vr_t_en_bs),
     &        ds(p_pwl_vr_da_v),
     &        is(p_pwl_vr_ls),
     &        is(p_pwl_vr_da_sa),
     &        is(p_pwl_vr_cu_sn),
     &        pr_act(vr_t_en_bs),
     &        obj_cf_o_vr_t_en_bs,
     &        obj_cf_cg_o_vr_t_en_bs,
     &        tl_pr_ifs)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      endif
      bc_co_v(pv_r_n) = obj_cf_o_vr_t_en_bs
      du_act(vr_t_en_bs) = du_act(vr_t_en_bs) + obj_cf_cg_o_vr_t_en_bs
c
c     Update the indexing information for the basic costs as a result of
c     a change in the entry in the leaving row.
c
      if (bc_co_v(pv_r_n) .ne. zero) then
         if (bc_co_ix_bar(pv_r_n) .eq. 0) then
c
c     A zero entry has become nonzero. Add its index to the end of the
c     list and increase the length of the list by one.
c
            bc_co_ix(0) = bc_co_ix(0) + 1
            bc_co_ix(bc_co_ix(0)) = pv_r_n
            bc_co_ix_bar(pv_r_n) = bc_co_ix(0)
         endif
      else
         if (bc_co_ix_bar(pv_r_n) .ne. 0) then
c
c     A nonzero entry has become zero. Replace its index in the list by
c     the index of the last entry in the list and reduce the length of
c     the list by one.
c
            r_o_l_ix = bc_co_ix(bc_co_ix(0))
            bc_co_ix(bc_co_ix_bar(pv_r_n)) = r_o_l_ix
            bc_co_ix_bar(r_o_l_ix) = bc_co_ix_bar(pv_r_n)
            bc_co_ix_bar(pv_r_n) = 0
            bc_co_ix(0) = bc_co_ix(0) - 1
         endif
      endif
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_u_lvl1) call ems_tt_rec(-u_en_vr_co_tt, n_bs)
CM      ENDIF
 1000 continue
c
c     Find out how many variables may have passed a bound/breakpoint.
c
      n_cdd_r = iabs(cdd_r(0))
      if (n_cdd_r .eq. 0) goto 7000
c
c     There is a (non-empty) list of all row indices of variables which
c     may have passed a bound/breakpoint.
c
      if (n_cdd_r .gt. n_r) goto 8000
c
c     Following a non L1-CHUZR, if cdd_r(0) is negative then the
c     variables for the row indices in the list can be assumed to be not
c     currently feasible.
c
      al_cdd_ifs =
     &     iand(cz_r_msk, cz_r_l1_bt) .eq. 0 .and. cdd_r(0) .lt. 0
      if (al_cdd_ifs) then
c
c     The list contains only row indices for variables which are not
c     currently feasible. Use ems_g_bc_fs_cg to determine
c     basic cost changes most efficiently.
c
         call ems_g_bc_fs_cg(rsmi_lb, rsmi_co, rsmi_ub,
     &        pr_act, st, vr_in_r,
     &        bc_co_v, bc_co_ix, bc_co_ix_bar, pi_v, pi_ix, cdd_r)
      else
c
c     The list may contain rows indices for feasible or non-standard
c     variables.
c     Note that it is assumed that the index of the pivotal row does not
c     appear in this list.
c
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_u_lvl1) call ems_tt_rec(u_bc_co_tt, n_bs)
CM      ENDIF
         do 1020, cdd_r_n = 1, n_cdd_r
            r_n = cdd_r(cdd_r_n)
            vr_n = vr_in_r(r_n)
            vr_st = st(vr_n)
            if (iand(vr_st, alt_bt) .eq. 0) then
               call ems_reset_1_bc_vr_st(
     &              vr_n,
     &              st(vr_n),
     &              rsmi_lb(vr_n),
     &              rsmi_co(vr_n),
     &              rsmi_ub(vr_n),
     &              pr_act(vr_n),
     &              obj_cf,
     &              obj_cf_cg,
     &              tl_pr_ifs)
c     !!!!                         obj_cf_cg,
c     &                 tl_pr_ifs)
               if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
            else if (iand(vr_st, bp_bt) .ne. 0) then
               call ems_reset_1_bc_bp_vr_st(
     &              vr_n,
     &              st(vr_n),
     &              rsmi_lb(vr_n),
     &              rsmi_co(vr_n),
     &              rsmi_ub(vr_n),
     &              pr_act(vr_n),
     &              obj_cf,
     &              obj_cf_cg,
     &              tl_pr_ifs)
               if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
            else
               call ems_reset_1_bc_pwl_vr_st(
     &              vr_n,
     &              st(vr_n),
     &              rsmi_lb(vr_n),
     &              rsmi_co(vr_n),
     &              rsmi_ub(vr_n),
     &              ds(p_pwl_vr_da_v),
     &              is(p_pwl_vr_ls),
     &              is(p_pwl_vr_da_sa),
     &              is(p_pwl_vr_cu_sn),
     &              pr_act(vr_n),
     &              obj_cf,
     &              obj_cf_cg,
     &              tl_pr_ifs)
               if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
            endif
            if (abs(obj_cf_cg) .gt. bwd_tran_ze) then
               pi_ix(0) = pi_ix(0) + 1
               pi_v(r_n) = obj_cf_cg
               pi_ix(pi_ix(0)) = r_n
            endif
            if (obj_cf .ne. zero) then
               if (bc_co_ix_bar(r_n) .eq. 0) then
                  bc_co_ix(0) = bc_co_ix(0) + 1
                  bc_co_ix(bc_co_ix(0)) = r_n
                  bc_co_ix_bar(r_n) = bc_co_ix(0)
               endif
               bc_co_v(r_n) = obj_cf
            else
               if (bc_co_ix_bar(r_n) .ne. 0) then
                  r_o_l_ix = bc_co_ix(bc_co_ix(0))
                  bc_co_ix(bc_co_ix_bar(r_n)) = r_o_l_ix
                  bc_co_ix_bar(r_o_l_ix) = bc_co_ix_bar(r_n)
                  bc_co_ix_bar(r_n) = 0
                  bc_co_ix(0) = bc_co_ix(0) - 1
               endif
               bc_co_v(r_n) = zero
            endif
 1020    continue
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_u_lvl1) call ems_tt_rec(-u_bc_co_tt, n_bs)
CM      ENDIF
      endif
 7000 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)n_cdd_r, n_r
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9800 format('Number of candidates in list is ', i7, ' > ', i7)
      end
 
C->>> ----------------------------------------------> ems_g_bc_fs_cg <<<
c     Gets the change in feasibility of basic variables when there is a
c     list cdd_r of rows of the only infeasible basic variables which
c     can become feasible.
c     Updates the basic costs and stores changes in pi.
c
      subroutine ems_g_bc_fs_cg(
     &     rsmi_lb, rsmi_co, rsmi_ub, pr_act, st, vr_in_r,
     &     bc_co_v, bc_co_ix, bc_co_ix_bar, pi_v, pi_ix, cdd_r)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      include 'EMSMSGN.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      double precision rsmi_lb(0:mx_n_c+n_r)
      double precision rsmi_co(0:mx_n_c+n_r)
      double precision rsmi_ub(0:mx_n_c+n_r)
      double precision pr_act(0:mx_n_c+n_r)
      double precision bc_co_v(0:n_r), pi_v(0:n_r)
      integer st(0:mx_n_c+n_r), vr_in_r(0:n_r)
      integer bc_co_ix(0:n_r)
      integer bc_co_ix_bar(0:n_r)
      integer pi_ix(0:n_r)
      integer cdd_r(0:n_r)
      integer r_n, vr_n, vr_st, n_cdd_r, r_o_l_ix, cdd_r_n
      double precision rsdu
 
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_u_lvl1) call ems_tt_rec(u_bc_co_tt, n_bs)
CM      ENDIF
c
c     ems_g_bc_fs_cg should only be called when it is indicated that the
c     candidate list contains only row numbers of variables which are
c     not feasible by setting cdd_r(0) = -n_cdd_r.
c     NB, it shouldn't be called when n_cdd_r=0 either!
 
      n_cdd_r = -cdd_r(0)
      if (n_cdd_r .le. 0) goto 8000
      do 20, cdd_r_n = 1, n_cdd_r
         r_n = cdd_r(cdd_r_n)
         vr_n = vr_in_r(r_n)
         vr_st = st(vr_n)
         if (iand(vr_st, lb_bt) .ne. 0) then
            rsdu = rsmi_lb(vr_n) - pr_act(vr_n)
            if (rsdu .gt. tl_pr_ifs) then
               if (iand(vr_st, dn_bt) .eq. 0) goto 20
               if (iand(cz_r_msk, cz_r_l1_bt) .eq. 0) then
                  if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &                 n_si_it, vr_n
                  call ems_msg_wr_li(rsmi_msg_n)
               endif
               if (iand(vr_st, up_bt) .ne. 0) goto 8010
               st(vr_n) = vr_st - dn_bt + up_bt
               pi_ix(0) = pi_ix(0) + 1
               bc_co_v(r_n) = -one + pr_co_mu*rsmi_co(vr_n)
               pi_v(r_n) = -two
               pi_ix(pi_ix(0)) = r_n
               go to 10
            endif
         endif
         if (iand(vr_st, ub_bt) .ne. 0) then
            rsdu = pr_act(vr_n) - rsmi_ub(vr_n)
            if (rsdu .gt. tl_pr_ifs) then
               if (iand(vr_st, up_bt) .eq. 0) goto 20
               if (iand(cz_r_msk, cz_r_l1_bt) .eq. 0) then
                  if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &                 n_si_it, vr_n
                  call ems_msg_wr_li(rsmi_msg_n)
               endif
               if (iand(vr_st, dn_bt) .ne. 0) goto 8020
               st(vr_n) = vr_st - up_bt + dn_bt
               pi_ix(0) = pi_ix(0) + 1
               bc_co_v(r_n) = one + pr_co_mu*rsmi_co(vr_n)
               pi_v(r_n) = two
               pi_ix(pi_ix(0)) = r_n
               go to 10
            endif
         endif
         n_pr_ifs = n_pr_ifs - 1
         if (iand(vr_st, dn_bt) .ne. 0) then
            if (iand(vr_st, up_bt) .ne. 0) goto 8010
            st(vr_n) = vr_st + up_bt - ifs_bt
            pi_ix(0) = pi_ix(0) + 1
            bc_co_v(r_n) = pr_co_mu*rsmi_co(vr_n)
            pi_v(r_n) = -one
            pi_ix(pi_ix(0)) = r_n
         else if (iand(vr_st, up_bt) .ne. 0) then
            st(vr_n) = vr_st + dn_bt - ifs_bt
            pi_ix(0) = pi_ix(0) + 1
            bc_co_v(r_n) = pr_co_mu*rsmi_co(vr_n)
            pi_v(r_n) = one
            pi_ix(pi_ix(0)) = r_n
         else
            goto 8030
         endif
 10      continue
         if (bc_co_v(r_n) .ne. zero) then
            if (bc_co_ix_bar(r_n) .eq. 0) then
               bc_co_ix(0) = bc_co_ix(0) + 1
               bc_co_ix(bc_co_ix(0)) = r_n
               bc_co_ix_bar(r_n) = bc_co_ix(0)
            endif
         else
            if (bc_co_ix_bar(r_n) .ne. 0) then
               r_o_l_ix = bc_co_ix(bc_co_ix(0))
               bc_co_ix(bc_co_ix_bar(r_n)) = r_o_l_ix
               bc_co_ix_bar(r_o_l_ix) = bc_co_ix_bar(r_n)
               bc_co_ix_bar(r_n) = 0
               bc_co_ix(0) = bc_co_ix(0) - 1
            endif
         endif
 20   continue
 7000 continue
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_u_lvl1) call ems_tt_rec(-u_bc_co_tt, n_bs)
CM      ENDIF
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)n_cdd_r
      call ems_msg_wr_li(bug_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?      call ems_dump
CM      ENDIF
      goto 7000
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)vr_n
      call ems_msg_wr_li(bug_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?      call ems_dump
CM      ENDIF
      goto 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)vr_n
      call ems_msg_wr_li(bug_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?      call ems_dump
CM      ENDIF
      goto 7000
 8030 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9803)vr_n
      call ems_msg_wr_li(bug_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?      call ems_dump
CM      ENDIF
      goto 7000
 9000 format('Iteration ', i7,
     &     ': Switch of infeasibility for variable ', i7)
 9800 format('Calling ems_g_bc_fs_cg with n_cdd_r = ', i7)
 9801 format('               Up   bit is set for variable ', i7)
 9802 format('               Down bit is set for variable ', i7)
 9803 format('Neither up nor down bit is set for variable ', i7)
      end
 
C->>> ------------------------------------------------> ems_se_bc_co <<<
c     Sets the costs of the basic variables.
c
      subroutine ems_se_bc_co(st, rsmi_co, vr_in_r,
     &     bc_co_v, bc_co_ix, bc_co_ix_bar)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICS.INC'
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      double precision rsmi_co(0:mx_n_c+n_r)
      double precision bc_co_v(0:n_r)
      integer st(0:mx_n_c+n_r)
      integer vr_in_r(0:n_r), bc_co_ix(0:n_r)
      integer bc_co_ix_bar(0:n_r)
      integer r_n, vr_n, n_bc_co_ix, ix_n
c
c     Zero the nonzeros
c
      do 5, ix_n = 1, bc_co_ix(0)
         r_n = bc_co_ix(ix_n)
         bc_co_v(r_n) = zero
         bc_co_ix_bar(r_n) = 0
 5    continue
      if (iand(ck_msk, ze_a_ck_bt) .ne. 0)
     &     call ems_ck_ze_rl_a(n_r, bc_co_v)
      n_bc_co_ix = 0
      if (lp_ph .eq. 1) then
         do 10, r_n = 1, n_r
            vr_n = vr_in_r(r_n)
            if (iand(st(vr_n), ifs_bt) .eq. 0) then
               bc_co_v(r_n) =           pr_co_mu*rsmi_co(vr_n)
            else
               if (iand(st(vr_n), up_bt) .ne. 0) then
                  bc_co_v(r_n) = -one + pr_co_mu*rsmi_co(vr_n)
               else
                  bc_co_v(r_n) =  one + pr_co_mu*rsmi_co(vr_n)
               endif
            endif
            if (bc_co_v(r_n) .ne. zero) then
               n_bc_co_ix = n_bc_co_ix + 1
               bc_co_ix(n_bc_co_ix) = r_n
               bc_co_ix_bar(r_n) = n_bc_co_ix
            endif
 10      continue
      else
         do 20, r_n = 1, n_r
            vr_n = vr_in_r(r_n)
            bc_co_v(r_n) = pr_co_mu*rsmi_co(vr_n)
            if (bc_co_v(r_n) .ne. zero) then
               n_bc_co_ix = n_bc_co_ix + 1
               bc_co_ix(n_bc_co_ix) = r_n
               bc_co_ix_bar(r_n) = n_bc_co_ix
            endif
 20      continue
      endif
      bc_co_ix(0) = n_bc_co_ix
c
c     Indicate that the solver's basic costs are correct.
c
      rsmi_da_st_msk = ior(rsmi_da_st_msk, rsmi_da_st_bc_co)
c
c     Indicate that the model's nonbasic dual activities are incorrect.
c
      ml_da_st_msk =
     &     ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_non_bc_du_act)
      return
      end
 
C->>> --------------------------------------------> ems_an_du_act_er <<<
c     Analyse a dual activity error.
c
      subroutine ems_an_du_act_er(re_pc, re_re_pc,
     &     og_du_act, tru_du_act, du_act_sgn_ok, du_act_er)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICS.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      logical re_re_pc, re_pc, du_act_sgn_ok
      double precision og_du_act, tru_du_act, du_act_er
 
      re_re_pc = .false.
      if (du_act_sgn_ok) then
c
c     The dual activity has the right sign.
c     Reset re_pc now rather than after re-pricing so that re-re-pricing
c     can be detected.
c
         re_pc = .false.
c
c     When not using update pricing an error greater than the tolerance
c     indicates numerical problems in the inverse. Re-invert if the
c     INVERT is not fresh.
c     When using update pricing a stability check is done later
c     by comparing the pivot in the pivot row with the pivot in the
c     pivot column when updating the dual activities.
c
         if (u_pc .eq. 0 .and.
     &        du_act_er .gt. tl_du_act_er .and. n_u .gt. 0) then
            rq_inv = rq_inv_du_act_er
            ml_da_st_msk =
     &           ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_inv)
         endif
c
c     ?? Are fresh reduced costs calculated if using perm INVERT
c
      else
c
c     Dual activity has wrong sign so report this.
c
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9510)n_si_it
         call ems_msg_wr_li(warn_msg_n)
c     nw_sgn_er_bug had following 3 lines commented out
         if (u_pc .eq. 0 .and. n_u .gt. 0) then
            rq_inv = rq_inv_du_act_er
            ml_da_st_msk =
     &           ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_inv)
         else if (u_pc .eq. 0 .or. re_pc) then
c
c     If update pricing is not used or if no costs of the right sign
c     have been found since the last cost of the wrong sign, or if
c     update pricing is not used then re-pricing leads to an infinite
c     loop. In this case, return re_re_pc = .true. so RSMI will be
c     reset.
c     (If the next cost sign is incorrect then re_re_pc = .true. but
c     rsmi_reset will recognise that it has been called twice for the
c     same simplex iteration and cause RSMI to terminate. The dual
c     feasibility tolerance will not have been satisfied but it was
c     probably too high for the condition of the problem.
c
c     nw_sgn_er_bug had following 3 lines commented out
            goto 8010
         endif
c nw_sgn_er_bug         if (n_u .gt. 0) then
c nw_sgn_er_bug            rq_inv = rq_inv_du_act_er
c nw_sgn_er_bug         ml_da_st_msk =
c nw_sgn_er_bug     &        ml_da_st_msk -
c nw_sgn_er_bug     & iand(ml_da_st_msk, ml_da_st_inv)
c nw_sgn_er_bug         else if (u_pc .eq. 0 .or. re_pc) then
c nw_sgn_er_bug            goto 8010
c nw_sgn_er_bug         endif
c
c     If costs of the right sign have been found since the last cost of
c     the wrong sign then re_pc will be false. So set it true.
c
         re_pc = .true.
      endif
 7000 continue
      return
 8010 continue
      re_re_pc = .true.
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)
      call ems_msg_wr_li(warn_msg_n)
      goto 7000
 9510 format('Iteration ', i7, ' Dual activity has the wrong sign')
 9801 format('Trying to re-re-price so reset RSMI')
      end
 
C->>> --------------------------------------------> ems_g_alt_du_act <<<
c     Computes an alternative dual activity from the pivotal column.
c     Also computes the phase 1 dual activity.
c
      subroutine ems_g_alt_du_act(
     &     pv_c_sgn, vr_n,
     &     st,
     &     vr_in_r,
     &     rsmi_co,
     &     bc_co_v,
     &     bc_co_ix,
     &     pv_c_v,
     &     du_act, alt_du_act, ph_1_du_act)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      integer pv_c_sgn
      integer vr_n, st(0:mx_n_c+n_r), vr_in_r(0:n_r)
      integer bc_co_ix(0:n_r)
      double precision rsmi_co(0:mx_n_c+n_r)
      double precision bc_co_v(0:n_r), pv_c_v(0:n_r)
      double precision ph_1_du_act, du_act, alt_du_act
      integer n_bc_co_ix, ix_n, r_n, lc_vr_n
 
      if (iand(st(vr_n), ifs_bt) .ne. 0) then
         if (iand(st(vr_n), dn_bt) .eq. 0) then
c
c     Below lower bound
c
            alt_du_act = -one + pr_co_mu*rsmi_co(vr_n)
         else
c
c     Above upper bound
c
            alt_du_act =  one + pr_co_mu*rsmi_co(vr_n)
         endif
      else
c
c     Feasible
c
         alt_du_act = pr_co_mu*rsmi_co(vr_n)
      endif
      alt_du_act = pv_c_sgn*alt_du_act
      n_bc_co_ix = bc_co_ix(0)
      if (n_bc_co_ix .lt. bwd_tran_dse_rhs_n_r) then
c
c     The vector of basic costs is sufficiently sparse to be worth
c     exploiting its indices.
c
         do 20, ix_n = 1, n_bc_co_ix
            r_n = bc_co_ix(ix_n)
            alt_du_act = alt_du_act + pv_c_v(r_n)*bc_co_v(r_n)
 20      continue
      else
         do 30, r_n = 1, n_r
            alt_du_act = alt_du_act + pv_c_v(r_n)*bc_co_v(r_n)
 30      continue
      endif
      alt_du_act = pv_c_sgn*alt_du_act
      if (lp_ph .eq. 1 .and. pr_wt .ne. zero) then
         ph_1_du_act = zero
         do 40, r_n = 1, n_r
            lc_vr_n = vr_in_r(r_n)
            if (iand(st(lc_vr_n), ifs_bt) .ne. 0) then
               if (iand(st(lc_vr_n), up_bt) .ne. 0) then
                  ph_1_du_act = ph_1_du_act - pv_c_v(r_n)
               else
                  ph_1_du_act = ph_1_du_act + pv_c_v(r_n)
               endif
            endif
 40      continue
         ph_1_du_act = pv_c_sgn*ph_1_du_act
      else
         ph_1_du_act = alt_du_act
      endif
      return
      end
 
CM      IF (emsol_da .EQ. 1) THEN
C?      subroutine ems_an_ph_2_du_act(
C?     &     st, rsmi_co, vr_in_r, nw_eta_v, nw_eta_ix)
C?      implicit none
C?      include 'EMSV.INC'
C?      include 'EMSPM.INC'
C?      include 'RSMICOM.INC'
CM      IF (emsol_da .EQ. 1) THEN
C?      include 'EMSDA.INC'
CM      ENDIF
C?      include 'ICTVR.INC'
C?      include 'RLCTVR.INC'
C?      integer st(0:mx_n_c+n_r)
C?      integer vr_in_r(0:n_r), nw_eta_ix(0:nw_eta_l_ix)
C?      double precision rsmi_co(0:mx_n_c+n_r)
C?      double precision nw_eta_v(0:nw_eta_l_ix)
C?      double precision ph_2_du_act, lc_pr_wt
C?      integer vr_n, ix_n
C?
C?      ph_2_du_act = mx_mn*rsmi_co(vr_t_en_bs)
C?      do 10, ix_n = nw_eta_f_ix, nw_eta_l_ix
C?         vr_n = vr_in_r(nw_eta_ix(ix_n))
C?         ph_2_du_act = ph_2_du_act + nw_eta_v(ix_n)*rsmi_co(vr_n)
C? 10   continue
C?      if (mv_dir*ph_2_du_act .gt. tl_du_ifs) then
C?         n_no_ph_2_cdd = n_no_ph_2_cdd + 1
C?         n_no_ph_2_cdd_by_pr_wt(1) = n_no_ph_2_cdd_by_pr_wt(1) + 1
C?      endif
C?      lc_pr_wt = 1d1
C?      do 20, ix_n = 2, 7
C?         lc_pr_wt = lc_pr_wt*1d-2
C?         if (mv_dir*(du_act_o_vr_t_en_bs+lc_pr_wt*ph_2_du_act) .gt.
C?     &        tl_du_ifs) n_no_ph_2_cdd_by_pr_wt(ix_n) =
C?     &        n_no_ph_2_cdd_by_pr_wt(ix_n) + 1
C? 20   continue
C?      return
C?      end
CM      ENDIF
C->>> -------------------------------------------------> ems_g_lp_ph <<<
c     Determines the LP phase and sets the status of basic variables.
c
      subroutine ems_g_lp_ph(
     &     rsmi_lb, rsmi_co, rsmi_ub, pr_act, st, vr_in_r,
     &     ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      double precision rsmi_lb(0:mx_n_c+n_r)
      double precision rsmi_co(0:mx_n_c+n_r)
      double precision rsmi_ub(0:mx_n_c+n_r)
      double precision pr_act(0:mx_n_c+n_r)
      double precision ds(0:ds_n_en_m1)
      integer st(0:mx_n_c+n_r), vr_in_r(0:n_r)
      integer is(0:is_n_en_m1)
      integer r_n, vr_n, vr_st, rsmi_vr_n
      double precision pwl_lc, pwl_bp, pwl_uc, pwl_dl
c      double precision pwl_lb, pwl_co, pwl_ub
      double precision rsdu
 
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tt_da .gt. 0) call ems_tt_rec(g_lp_ph_tt, n_bs)
CM      ENDIF
      n_pr_ifs = n_non_bc_pr_ifs
      do 10, r_n = 1, n_r
         vr_n = vr_in_r(r_n)
         rsmi_vr_n = vr_n
         vr_st = st(rsmi_vr_n)
         if (iand(vr_st, alt_bt) .eq. 0) then
c
c     Variable is standard
c
c     Assume the variable is feasible and between bounds (NB this
c     includes the anomalous case of FX variables at their bounds.
c
            st(rsmi_vr_n) = st(rsmi_vr_n) - iand(st(rsmi_vr_n), ifs_bt)
            st(rsmi_vr_n) = ior(st(rsmi_vr_n), up_dn)
            if (iand(vr_st, lb_bt) .ne. 0) then
               rsdu = rsmi_lb(rsmi_vr_n) - pr_act(rsmi_vr_n)
               if (rsdu .gt. zero) then
c
c     The variable is at/below its lower bound
c
                  if (rsdu .gt. tl_pr_ifs) then
c
c     Variable is below its lower bound so remove the down bit and set
c     the ifs bit.
c
                     n_pr_ifs = n_pr_ifs + 1
                     st(rsmi_vr_n) = st(rsmi_vr_n) - dn_bt + ifs_bt
                  endif
c
c     Variable must certainly  satisfy any upper bound.
c
                  go to 10
               endif
            endif
            if (iand(vr_st, ub_bt) .ne. 0) then
               rsdu = pr_act(rsmi_vr_n) - rsmi_ub(rsmi_vr_n)
               if (rsdu .gt. zero) then
c
c     The variable is at/above its upper bound
c
                  if (rsdu .gt. tl_pr_ifs) then
c
c     Variable is above its upper bound so remove the up bit and set
c     the ifs bit.
c
                     n_pr_ifs = n_pr_ifs + 1
                     st(rsmi_vr_n) = st(rsmi_vr_n) - up_bt + ifs_bt
                  endif
               endif
            endif
         else if (iand(vr_st, bp_bt) .ne. 0) then
c
c     Variable is BP
c
            if (iand(st(rsmi_vr_n), lb_bt) .ne. 0) then
c
c     Data are set for variable is above its break point
c
               pwl_bp = rsmi_lb(rsmi_vr_n)
               pwl_uc = rsmi_co(rsmi_vr_n)
               pwl_dl = rsmi_ub(rsmi_vr_n)
               rsdu = pwl_bp - pr_act(rsmi_vr_n)
               if (rsdu .gt. tl_pr_ifs) then
c
c     Variable is now below its break point so modify data
c
                  rsmi_lb(rsmi_vr_n) = -pwl_dl
                  rsmi_co(rsmi_vr_n) =  pwl_uc + pwl_dl
                  rsmi_ub(rsmi_vr_n) =  pwl_bp
                  st(rsmi_vr_n) = st(rsmi_vr_n) -
     &                 iand(st(rsmi_vr_n), su_vr_st_bt) + bp_vr_bc_bw_bp
               else
c
c     Make sure that the status is OK for a BP variable above its break
c     point: Prompted by situation where BP logicals enter the basis due
c     to singularity and the down bit is not set.
c
                  st(rsmi_vr_n) = st(rsmi_vr_n) -
     &                 iand(st(rsmi_vr_n), su_vr_st_bt) + bp_vr_bc_ab_bp
               endif
            else if (iand(st(rsmi_vr_n), ub_bt) .ne. 0) then
c
c     Data are set for variable is below its break point
c
               pwl_dl = rsmi_lb(rsmi_vr_n)
               pwl_lc = rsmi_co(rsmi_vr_n)
               pwl_bp = rsmi_ub(rsmi_vr_n)
               rsdu = pr_act(rsmi_vr_n) - pwl_bp
               if (rsdu .gt. tl_pr_ifs) then
c
c     Variable is now above its break point so modify data
c
                  rsmi_lb(rsmi_vr_n) =  pwl_bp
                  rsmi_co(rsmi_vr_n) =  pwl_lc + pwl_dl
                  rsmi_ub(rsmi_vr_n) = -pwl_dl
                  st(rsmi_vr_n) = st(rsmi_vr_n) -
     &                 iand(st(rsmi_vr_n), su_vr_st_bt) + bp_vr_bc_ab_bp
               else
c
c     Make sure that the status is OK for a BP variable below its break
c     point: Prompted by situation where BP logicals enter the basis due
c     to singularity and the up bit is not set.
c
                  st(rsmi_vr_n) = st(rsmi_vr_n) -
     &                 iand(st(rsmi_vr_n), su_vr_st_bt) + bp_vr_bc_bw_bp
               endif
            endif
         else
c
c     Variable is PWL
c
         endif
 10   continue
      if (n_pr_ifs .gt. 0) then
         lp_ph = 1
         pr_co_mu = mx_mn*pr_wt
      else
         lp_ph = 2
         pr_co_mu = mx_mn
      endif
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tt_da .gt. 0) call ems_tt_rec(-g_lp_ph_tt, n_bs)
CM      ENDIF
      return
      end
 
C->>> ----------------------------------------> ems_g_n_su_mx_pr_ifs <<<
c     Get the number of, sum of and max primal infeasibilities.
c
      subroutine ems_g_n_su_mx_pr_ifs(
     &     rsmi_lb, rsmi_ub, pr_act, st,
     &     usr_n_pr_ifs, usr_su_pr_ifs, mx_pr_ifs, mx_rlv_pr_ifs)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      integer usr_n_pr_ifs
      double precision rsmi_lb(0:mx_n_c+n_r)
      double precision rsmi_ub(0:mx_n_c+n_r)
      integer st(0:mx_n_c+n_r)
      double precision pr_act(0:mx_n_c+n_r)
      double precision usr_su_pr_ifs, mx_pr_ifs, mx_rlv_pr_ifs
      integer vr_n
      double precision rsdu, v
 
      usr_n_pr_ifs = 0
      usr_su_pr_ifs = zero
      mx_pr_ifs = zero
      mx_rlv_pr_ifs = zero
      if (iand(ml_da_st_msk, ml_da_st_bc_pr_act) .eq. 0) goto 7000
      do 10, vr_n = 1, n_c
         if (iand(st(vr_n), alt_bt) .eq. 0) then
            rsdu = max(rsmi_lb(vr_n)-pr_act(vr_n),
     &           pr_act(vr_n)-rsmi_ub(vr_n))
            if (rsdu .gt. zero) then
               if (rsmi_lb(vr_n)-pr_act(vr_n) .gt.
     &              pr_act(vr_n)-rsmi_ub(vr_n)) then
                  v = max(abs(rsmi_lb(vr_n)), one)
               else
                  v = max(abs(rsmi_ub(vr_n)), one)
               endif
               usr_su_pr_ifs = usr_su_pr_ifs + rsdu
               mx_pr_ifs = max(rsdu, mx_pr_ifs)
               mx_rlv_pr_ifs = max(rsdu, mx_pr_ifs/v)
               if (rsdu .gt. tl_pr_ifs) usr_n_pr_ifs = usr_n_pr_ifs + 1
            endif
         else if (iand(st(vr_n), bp_bt) .eq. 0) then
c
c     By definition BP vbariables cannot be infeasible.
c
         endif
 10   continue
      do 20, vr_n = mx_n_c+1, mx_n_c+n_r
         if (iand(st(vr_n), alt_bt) .eq. 0) then
            rsdu = max(rsmi_lb(vr_n)-pr_act(vr_n),
     &           pr_act(vr_n)-rsmi_ub(vr_n))
            if (rsdu .gt. zero) then
               if (rsmi_lb(vr_n)-pr_act(vr_n) .gt.
     &              pr_act(vr_n)-rsmi_ub(vr_n)) then
                  v = max(abs(rsmi_lb(vr_n)), one)
               else
                  v = max(abs(rsmi_ub(vr_n)), one)
               endif
               usr_su_pr_ifs = usr_su_pr_ifs + rsdu
               mx_pr_ifs = max(rsdu, mx_pr_ifs)
               mx_rlv_pr_ifs = max(rsdu, mx_pr_ifs/v)
               if (rsdu .gt. tl_pr_ifs) usr_n_pr_ifs = usr_n_pr_ifs + 1
            endif
         else if (iand(st(vr_n), bp_bt) .eq. 0) then
c
c     By definition BP vbariables cannot be infeasible.
c
         endif
 20   continue
 7000 continue
      return
      end
 
C->>> ----------------------------------------> ems_g_n_su_mx_du_ifs <<<
c     Get the number of, sum of and max dual infeasibilities.
c
      subroutine ems_g_n_su_mx_du_ifs(vr_in_c,
     &     rsmi_lb, rsmi_co, rsmi_ub, du_act,
     &     usr_n_du_ifs, usr_su_du_ifs, mx_du_ifs, mx_rlv_du_ifs)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICS.INC'
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      integer usr_n_du_ifs
      integer vr_in_c(-vr_in_c_n_sn:n_c)
      double precision rsmi_lb(0:mx_n_c+n_r)
      double precision rsmi_co(0:mx_n_c+n_r)
      double precision rsmi_ub(0:mx_n_c+n_r)
      double precision du_act(0:mx_n_c+n_r)
      double precision usr_su_du_ifs, mx_du_ifs, mx_rlv_du_ifs
      integer c_n, vr_n
      double precision rsdu, v
 
      usr_n_du_ifs = 0
      usr_su_du_ifs = zero
      mx_du_ifs = zero
      mx_rlv_du_ifs = zero
      if (iand(ml_da_st_msk, ml_da_st_non_bc_du_act) .eq. 0) goto 7000
      do 10, c_n = 1, vr_in_c(os_lg_in_c_ab_bp_p)
         vr_n = vr_in_c(c_n)
         rsdu = -du_act(vr_n)
         if (rsdu .gt. zero) then
            v = max(abs(pr_co_mu*rsmi_ub(vr_n)), one)
            usr_su_du_ifs = usr_su_du_ifs + rsdu
            mx_du_ifs = max(rsdu, mx_du_ifs)
            mx_rlv_du_ifs = max(rsdu/v, mx_rlv_du_ifs)
            if (rsdu .gt. tl_du_ifs) usr_n_du_ifs = usr_n_du_ifs + 1
         else
c
c     NB: With convex cost functions it cannot be attractive to move
c     both ways.
c
            rsdu = du_act(vr_n) + pr_co_mu*rsmi_ub(vr_n)
            if (rsdu .gt. zero) then
               v = max(abs(pr_co_mu*(rsmi_lb(vr_n)+rsmi_ub(vr_n))), one)
               usr_su_du_ifs = usr_su_du_ifs + rsdu
               mx_du_ifs = max(rsdu, mx_du_ifs)
               mx_rlv_du_ifs = max(rsdu/v, mx_rlv_du_ifs)
               if (rsdu .gt. tl_du_ifs) usr_n_du_ifs = usr_n_du_ifs + 1
            endif
         endif
 10   continue
      do 20, c_n = c_n, vr_in_c(os_lg_in_c_bw_bp_p)
         vr_n = vr_in_c(c_n)
         rsdu = du_act(vr_n)
         if (rsdu .gt. zero) then
            v = max(abs(pr_co_mu*rsmi_lb(vr_n)), one)
            usr_su_du_ifs = usr_su_du_ifs + rsdu
            mx_du_ifs = max(rsdu, mx_du_ifs)
            mx_rlv_du_ifs = max(rsdu/v, mx_rlv_du_ifs)
            if (rsdu .gt. tl_du_ifs) usr_n_du_ifs = usr_n_du_ifs + 1
         else
c
c     NB: With convex cost functions it cannot be attractive to move
c     both ways.
c
            rsdu = -(du_act(vr_n)+pr_co_mu*rsmi_lb(vr_n))
            if (rsdu .gt. zero) then
               v = max(abs(pr_co_mu*(rsmi_lb(vr_n)+rsmi_ub(vr_n))), one)
               usr_su_du_ifs = usr_su_du_ifs + rsdu
               mx_du_ifs = max(rsdu, mx_du_ifs)
               mx_rlv_du_ifs = max(rsdu/v, mx_rlv_du_ifs)
               if (rsdu .gt. tl_du_ifs) usr_n_du_ifs = usr_n_du_ifs + 1
            endif
         endif
 20   continue
      do 30, c_n = c_n, vr_in_c(os_lg_in_c_up_dn_p)
         vr_n = vr_in_c(c_n)
         v = max(abs(pr_co_mu*rsmi_co(vr_n)), one)
         rsdu = abs(du_act(vr_n))
         usr_su_du_ifs = usr_su_du_ifs + rsdu
         mx_du_ifs = max(rsdu, mx_du_ifs)
         mx_rlv_du_ifs = max(rsdu/v, mx_rlv_du_ifs)
         if (rsdu .gt. tl_du_ifs) usr_n_du_ifs = usr_n_du_ifs + 1
 30   continue
      if (lp_ph .eq. 1 .and.
     &     iand(cz_c_msk, cz_c_bk_bd_bt) .ne. 0) then
         do 40, c_n = c_n, vr_in_c(os_lg_in_c_up_p)
            vr_n = vr_in_c(c_n)
            rsdu = -du_act(vr_n)
            if (rsdu .gt. zero) then
               v = max(abs(pr_co_mu*rsmi_co(vr_n)), one)
               usr_su_du_ifs = usr_su_du_ifs + rsdu
               mx_du_ifs = max(rsdu, mx_du_ifs)
               mx_rlv_du_ifs = max(rsdu/v, mx_rlv_du_ifs)
               if (rsdu .gt. tl_du_ifs) usr_n_du_ifs = usr_n_du_ifs + 1
            else
c
c     NB: It cannot be attractive to move up from a lower bound and down
c     through it.
c
               rsdu = du_act(vr_n) - one
               if (rsdu .gt. zero) then
                  v = max(abs(pr_co_mu*rsmi_co(vr_n)-one), one)
                  usr_su_du_ifs = usr_su_du_ifs + rsdu
                  mx_du_ifs = max(rsdu, mx_du_ifs)
                  mx_rlv_du_ifs = max(rsdu/v, mx_rlv_du_ifs)
                  if (rsdu .gt. tl_du_ifs)
     &                 usr_n_du_ifs = usr_n_du_ifs + 1
               endif
            endif
 40      continue
         do 45, c_n = c_n, vr_in_c(os_lg_in_c_dn_p)
            vr_n = vr_in_c(c_n)
            rsdu = du_act(vr_n)
            if (rsdu .gt. zero) then
               v = max(abs(pr_co_mu*rsmi_co(vr_n)), one)
               usr_su_du_ifs = usr_su_du_ifs + rsdu
               mx_du_ifs = max(rsdu, mx_du_ifs)
               mx_rlv_du_ifs = max(rsdu/v, mx_rlv_du_ifs)
               if (rsdu .gt. tl_du_ifs) usr_n_du_ifs = usr_n_du_ifs + 1
            else
c
c     NB: It cannot be attractive to move up from a lower bound and down
c     through it.
c
               rsdu = -(du_act(vr_n)+one)
               if (rsdu .gt. zero) then
                  v = max(abs(pr_co_mu*rsmi_co(vr_n)+one), one)
                  usr_su_du_ifs = usr_su_du_ifs + rsdu
                  mx_du_ifs = max(rsdu, mx_du_ifs)
                  mx_rlv_du_ifs = max(rsdu/v, mx_rlv_du_ifs)
                  if (rsdu .gt. tl_du_ifs)
     &                 usr_n_du_ifs = usr_n_du_ifs + 1
               endif
            endif
 45      continue
         do 47, c_n = c_n, vr_in_c(os_lg_in_c_te_fx_p)
            vr_n = vr_in_c(c_n)
            rsdu = -(du_act(vr_n)+one)
            if (rsdu .gt. zero) then
               v = max(abs(pr_co_mu*rsmi_co(vr_n)+one), one)
               usr_su_du_ifs = usr_su_du_ifs + rsdu
               mx_du_ifs = max(rsdu, mx_du_ifs)
               mx_rlv_du_ifs = max(rsdu/v, mx_rlv_du_ifs)
               if (rsdu .gt. tl_du_ifs) usr_n_du_ifs = usr_n_du_ifs + 1
            else
c
c     NB: It cannot be attractive to move up from a fixed value and down
c     from it.
c
               rsdu = du_act(vr_n) - one
               if (rsdu .gt. zero) then
                  v = max(abs(pr_co_mu*rsmi_co(vr_n)-one), one)
                  usr_su_du_ifs = usr_su_du_ifs + rsdu
                  mx_du_ifs = max(rsdu, mx_du_ifs)
                  mx_rlv_du_ifs = max(rsdu/v, mx_rlv_du_ifs)
                  if (rsdu .gt. tl_du_ifs)
     &                 usr_n_du_ifs = usr_n_du_ifs + 1
               endif
            endif
 47      continue
      else
         do 50, c_n = c_n, vr_in_c(os_lg_in_c_up_p)
            vr_n = vr_in_c(c_n)
            rsdu = -du_act(vr_n)
            if (rsdu .gt. zero) then
               v = max(abs(pr_co_mu*rsmi_co(vr_n)), one)
               usr_su_du_ifs = usr_su_du_ifs + rsdu
               mx_du_ifs = max(rsdu, mx_du_ifs)
               mx_rlv_du_ifs = max(rsdu/v, mx_rlv_du_ifs)
               if (rsdu .gt. tl_du_ifs) usr_n_du_ifs = usr_n_du_ifs + 1
            endif
 50      continue
         do 55, c_n = c_n, vr_in_c(os_lg_in_c_dn_p)
            vr_n = vr_in_c(c_n)
            rsdu = du_act(vr_n)
            if (rsdu .gt. zero) then
               v = max(abs(pr_co_mu*rsmi_co(vr_n)), one)
               usr_su_du_ifs = usr_su_du_ifs + rsdu
               mx_du_ifs = max(rsdu, mx_du_ifs)
               mx_rlv_du_ifs = max(rsdu/v, mx_rlv_du_ifs)
               if (rsdu .gt. tl_du_ifs) usr_n_du_ifs = usr_n_du_ifs + 1
            endif
 55      continue
      endif
      do 110, c_n = vr_in_c(os_struc_in_c_f_p_m1) + 1,
     &     vr_in_c(os_struc_in_c_ab_bp_p)
         vr_n = vr_in_c(c_n)
         rsdu = -du_act(vr_n)
         if (rsdu .gt. zero) then
            v = max(abs(pr_co_mu*rsmi_ub(vr_n)), one)
            usr_su_du_ifs = usr_su_du_ifs + rsdu
            mx_du_ifs = max(rsdu, mx_du_ifs)
            mx_rlv_du_ifs = max(rsdu/v, mx_rlv_du_ifs)
            if (rsdu .gt. tl_du_ifs) usr_n_du_ifs = usr_n_du_ifs + 1
         else
c
c     NB: With convex cost functions it cannot be attractive to move
c     both ways.
c
            rsdu = du_act(vr_n) + pr_co_mu*rsmi_ub(vr_n)
            if (rsdu .gt. zero) then
               v = max(abs(pr_co_mu*(rsmi_lb(vr_n)+rsmi_ub(vr_n))), one)
               usr_su_du_ifs = usr_su_du_ifs + rsdu
               mx_du_ifs = max(rsdu, mx_du_ifs)
               mx_rlv_du_ifs = max(rsdu/v, mx_rlv_du_ifs)
               if (rsdu .gt. tl_du_ifs) usr_n_du_ifs = usr_n_du_ifs + 1
            endif
         endif
 110  continue
      do 120, c_n = c_n, vr_in_c(os_struc_in_c_bw_bp_p)
         vr_n = vr_in_c(c_n)
         rsdu = du_act(vr_n)
         if (rsdu .gt. zero) then
            v = max(abs(pr_co_mu*rsmi_lb(vr_n)), one)
            usr_su_du_ifs = usr_su_du_ifs + rsdu
            mx_du_ifs = max(rsdu, mx_du_ifs)
            mx_rlv_du_ifs = max(rsdu/v, mx_rlv_du_ifs)
            if (rsdu .gt. tl_du_ifs) usr_n_du_ifs = usr_n_du_ifs + 1
         else
c
c     NB: With convex cost functions it cannot be attractive to move
c     both ways.
c
            rsdu = -(du_act(vr_n)+pr_co_mu*rsmi_lb(vr_n))
            if (rsdu .gt. zero) then
               v = max(abs(pr_co_mu*(rsmi_lb(vr_n)+rsmi_ub(vr_n))), one)
               usr_su_du_ifs = usr_su_du_ifs + rsdu
               mx_du_ifs = max(rsdu, mx_du_ifs)
               mx_rlv_du_ifs = max(rsdu/v, mx_rlv_du_ifs)
               if (rsdu .gt. tl_du_ifs) usr_n_du_ifs = usr_n_du_ifs + 1
            endif
         endif
 120  continue
      do 130, c_n = c_n, vr_in_c(os_struc_in_c_up_dn_p)
         vr_n = vr_in_c(c_n)
         v = max(abs(pr_co_mu*rsmi_co(vr_n)), one)
         rsdu = abs(du_act(vr_n))
         usr_su_du_ifs = usr_su_du_ifs + rsdu
         mx_du_ifs = max(rsdu, mx_du_ifs)
         mx_rlv_du_ifs = max(rsdu/v, mx_rlv_du_ifs)
         if (rsdu .gt. tl_du_ifs) usr_n_du_ifs = usr_n_du_ifs + 1
 130  continue
      if (lp_ph .eq. 1 .and.
     &     iand(cz_c_msk, cz_c_bk_bd_bt) .ne. 0) then
         do 140, c_n = c_n, vr_in_c(os_struc_in_c_up_p)
            vr_n = vr_in_c(c_n)
            rsdu = -du_act(vr_n)
            if (rsdu .gt. zero) then
               v = max(abs(pr_co_mu*rsmi_co(vr_n)), one)
               usr_su_du_ifs = usr_su_du_ifs + rsdu
               mx_du_ifs = max(rsdu, mx_du_ifs)
               mx_rlv_du_ifs = max(rsdu/v, mx_rlv_du_ifs)
               if (rsdu .gt. tl_du_ifs) usr_n_du_ifs = usr_n_du_ifs + 1
            else
c
c     NB: It cannot be attractive to move up from a lower bound and down
c     through it.
c
               rsdu = du_act(vr_n) - one
               if (rsdu .gt. zero) then
                  v = max(abs(pr_co_mu*rsmi_co(vr_n)-one), one)
                  usr_su_du_ifs = usr_su_du_ifs + rsdu
                  mx_du_ifs = max(rsdu, mx_du_ifs)
                  mx_rlv_du_ifs = max(rsdu/v, mx_rlv_du_ifs)
                  if (rsdu .gt. tl_du_ifs)
     &                 usr_n_du_ifs = usr_n_du_ifs + 1
               endif
            endif
 140     continue
         do 145, c_n = c_n, vr_in_c(os_struc_in_c_dn_p)
            vr_n = vr_in_c(c_n)
            rsdu = du_act(vr_n)
            if (rsdu .gt. zero) then
               v = max(abs(pr_co_mu*rsmi_co(vr_n)), one)
               usr_su_du_ifs = usr_su_du_ifs + rsdu
               mx_du_ifs = max(rsdu, mx_du_ifs)
               mx_rlv_du_ifs = max(rsdu/v, mx_rlv_du_ifs)
               if (rsdu .gt. tl_du_ifs) usr_n_du_ifs = usr_n_du_ifs + 1
            else
c
c     NB: It cannot be attractive to move up from a lower bound and down
c     through it.
c
               rsdu = -(du_act(vr_n)+one)
               if (rsdu .gt. zero) then
                  v = max(abs(pr_co_mu*rsmi_co(vr_n)+one), one)
                  usr_su_du_ifs = usr_su_du_ifs + rsdu
                  mx_du_ifs = max(rsdu, mx_du_ifs)
                  mx_rlv_du_ifs = max(rsdu/v, mx_rlv_du_ifs)
                  if (rsdu .gt. tl_du_ifs)
     &                 usr_n_du_ifs = usr_n_du_ifs + 1
               endif
            endif
 145     continue
         do 147, c_n = c_n, vr_in_c(os_struc_in_c_te_fx_p)
            vr_n = vr_in_c(c_n)
            rsdu = -(du_act(vr_n)+one)
            if (rsdu .gt. zero) then
               v = max(abs(pr_co_mu*rsmi_co(vr_n)+one), one)
               usr_su_du_ifs = usr_su_du_ifs + rsdu
               mx_du_ifs = max(rsdu, mx_du_ifs)
               mx_rlv_du_ifs = max(rsdu/v, mx_rlv_du_ifs)
               if (rsdu .gt. tl_du_ifs) usr_n_du_ifs = usr_n_du_ifs + 1
            else
c
c     NB: It cannot be attractive to move up from a fixed value and down
c     from it.
c
               rsdu = du_act(vr_n) - one
               if (rsdu .gt. zero) then
                  v = max(abs(pr_co_mu*rsmi_co(vr_n)-one), one)
                  usr_su_du_ifs = usr_su_du_ifs + rsdu
                  mx_du_ifs = max(rsdu, mx_du_ifs)
                  mx_rlv_du_ifs = max(rsdu/v, mx_rlv_du_ifs)
                  if (rsdu .gt. tl_du_ifs)
     &                 usr_n_du_ifs = usr_n_du_ifs + 1
               endif
            endif
 147     continue
      else
         do 150, c_n = c_n, vr_in_c(os_struc_in_c_up_p)
            vr_n = vr_in_c(c_n)
            rsdu = -du_act(vr_n)
            if (rsdu .gt. zero) then
               v = max(abs(pr_co_mu*rsmi_co(vr_n)), one)
               usr_su_du_ifs = usr_su_du_ifs + rsdu
               mx_du_ifs = max(rsdu, mx_du_ifs)
               mx_rlv_du_ifs = max(rsdu/v, mx_rlv_du_ifs)
               if (rsdu .gt. tl_du_ifs) usr_n_du_ifs = usr_n_du_ifs + 1
            endif
 150     continue
         do 155, c_n = c_n, vr_in_c(os_struc_in_c_dn_p)
            vr_n = vr_in_c(c_n)
            rsdu = du_act(vr_n)
            if (rsdu .gt. zero) then
               v = max(abs(pr_co_mu*rsmi_co(vr_n)), one)
               usr_su_du_ifs = usr_su_du_ifs + rsdu
               mx_du_ifs = max(rsdu, mx_du_ifs)
               mx_rlv_du_ifs = max(rsdu/v, mx_rlv_du_ifs)
               if (rsdu .gt. tl_du_ifs) usr_n_du_ifs = usr_n_du_ifs + 1
            endif
 155     continue
      endif
 7000 continue
      return
      end
 
C->>> -----------------------------------------> ems_rp_ml_da_st_msk <<<
c     Report the bits set in ml_da_st_msk
c
      subroutine ems_rp_ml_da_st_msk
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
 
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_da_st_msk, ml_da_st_ld) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Model loaded'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No model loaded'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_da_st_msk, ml_da_st_nm) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Model names'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No model names'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_da_st_msk, ml_da_st_i_vr) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Integer variables'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No integer variables'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_da_st_msk, ml_da_st_alt_lp) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Non-standard variables'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No non-standard variables'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_da_st_msk, ml_da_st_bp_vr) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Breakpoint variables'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No breakpoint variables'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_da_st_msk, ml_da_st_pwl_vr) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    PWL variables'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No PWL variables'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_da_st_msk, ml_da_st_usr_pwl_vr) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    User PWL variables'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No User PWL variables'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_da_st_msk, ml_da_st_scl_ml) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Scale factors'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No scale factors'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_da_st_msk, ml_da_st_scl_ml_mtx) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Scaled matrix'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No scaled matrix'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_da_st_msk, ml_da_st_scl_ml_sol) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Scaled solution'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No scaled solution'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_da_st_msk, ml_da_st_vr_in_r) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Up-to-date vr_in_r'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No up-to-date vr_in_r'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_da_st_msk, ml_da_st_vr_in_c) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Up-to-date vr_in_c'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No up-to-date vr_in_c'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_da_st_msk, ml_da_st_inv) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Up-to-date INVERT'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No up-to-date INVERT'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_da_st_msk, ml_da_st_bc_pr_act) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Up-to-date basic primal activities'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No up-to-date basic primal activities '
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_da_st_msk, ml_da_st_vr_st_fm_act) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Up-to-date status from activities'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No up-to-date status from activities'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_da_st_msk, ml_da_st_non_bc_du_act) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Up-to-date nonbasic dual activities'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No up-to-date nonbasic dual activities'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_da_st_msk, ml_da_st_ed_wt) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Up-to-date edge weights'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No up-to-date edge weights'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_da_st_msk, ml_da_st_r_mtx) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Row-wise matrix'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No row-wise matrix'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_da_st_msk, ml_da_st_nw_r) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    New rows'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No new rows'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_da_st_msk, ml_da_st_nw_c) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    New columns'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No new columns'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_da_st_msk, ml_da_st_rg_da) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Model ranging data'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No model ranging data'
      endif
      call ems_msg_wr_li(info_msg_n)
      return
 9000 format('Reporting ml_da_st_msk')
 9100 format(a)
      end
 
C->>> --------------------------------------> ems_rp_ml_da_no_cg_msk <<<
c     Report the bits set in ml_da_no_cg_msk
c
      subroutine ems_rp_ml_da_no_cg_msk
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
 
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_da_no_cg_msk, ml_da_no_cg_ck_bt) .eq. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No data changes recorded'
         call ems_msg_wr_li(info_msg_n)
         goto 7000
      endif
      if (iand(ml_da_no_cg_msk, ml_da_no_cg_non_bc_vr_bd) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' Not changed: Basic bounds'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '     Changed: Basic bounds'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_da_no_cg_msk, ml_da_no_cg_bc_vr_bd) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' Not changed: Nonbasic bounds'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '     Changed: Nonbasic bounds'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_da_no_cg_msk, ml_da_no_cg_non_bc_co) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' Not changed: Basic costs'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '     Changed: Basic costs'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_da_no_cg_msk, ml_da_no_cg_bc_co) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' Not changed: Nonbasic costs'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '     Changed: Nonbasic costs'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_da_no_cg_msk, ml_da_no_cg_bc_bt) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' Not changed: Basic bits'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '     Changed: Basic bits'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_da_no_cg_msk, ml_da_no_cg_up_dn_bt) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' Not changed: Up/Dn bits'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '     Changed: Up/Dn bits'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_da_no_cg_msk, ml_da_no_cg_non_bc_pr_act) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' Not changed: Basic primal activities'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '     Changed: Basic primal activities'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_da_no_cg_msk, ml_da_no_cg_bc_pr_act) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' Not changed: Nonbasic primal activities'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '     Changed: Nonbasic primal activities'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_da_no_cg_msk, ml_da_no_cg_non_bc_du_act) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' Not changed: Basic dual activities'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '     Changed: Basic dual activities'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_da_no_cg_msk, ml_da_no_cg_bc_du_act) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' Not changed: Nonbasic dual activities'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '     Changed: Nonbasic dual activities'
      endif
      call ems_msg_wr_li(info_msg_n)
 7000 continue
      return
 9000 format('Reporting ml_da_no_cg_msk')
 9100 format(a)
      end
 
C->>> ----------------------------------------> ems_rp_ml_blk_st_msk <<<
c     Report the bits set in ml_blk_st_msk
c
      subroutine ems_rp_ml_blk_st_msk
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
 
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_blk_st_msk, ml_blk_st_ml_vec) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Block for model vectors'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No block for model vectors'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_blk_st_msk, ml_blk_st_ml_lng_nm) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Block for model long names'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No block for model long names'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_blk_st_msk, ml_blk_st_ml_i_vr) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Block for model integer variables'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No block for model integer variables'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_blk_st_msk, ml_blk_st_ml_pwl_vr) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Block for model PWL variables'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No block for model PWL variables'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_blk_st_msk, ml_blk_st_ml_sol) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Block for model solution'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No block for model solution'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_blk_st_msk, ml_blk_st_ml_vr_ls) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Block for model variable lists'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No block for model variable lists'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_blk_st_msk, ml_blk_st_ml_bs_inv_p) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Block for model basis INVERT pointers'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No block for model basis INVERT pointers'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_blk_st_msk, ml_blk_st_ml_c_mtx) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Block for model column-wise matrix'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No block for model column-wise matrix'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_blk_st_msk, ml_blk_st_ml_r_mtx) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Block for model row-wise matrix'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No block for model row-wise matrix'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_blk_st_msk, ml_blk_st_ml_aux_sol) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Block for model auxiliary solution'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No block for model auxiliary solution'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_blk_st_msk, ml_blk_st_ml_aux_blk) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Block for model auxiliary block'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No block for model auxiliary block'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_blk_st_msk, ml_blk_st_ml_prsl_sv) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Block for model presolve save'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No block for model presolve save'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_blk_st_msk, ml_blk_st_ml_prsl_wk) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Block for model presolve work'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No block for model presolve work'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_blk_st_msk, ml_blk_st_ml_rg_da) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Block for model ranging data'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No block for model ranging data'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_blk_st_msk, ml_blk_st_ml_u_bs) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Block for model UPDATE basis'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No block for model UPDATE basis'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_blk_st_msk, ml_blk_st_ml_usr_c_mtx) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Block for user column-wise matrix'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No block for user column-wise matrix'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_blk_st_msk, ml_blk_st_ml_usr_r_mtx) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Block for user row-wise matrix'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No block for user row-wise matrix'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(ml_blk_st_msk, ml_blk_st_dvx) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Block for DEVEX      '
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No block for DEVEX      '
      endif
      call ems_msg_wr_li(info_msg_n)
      return
 9000 format('Reporting ml_blk_st_msk')
 9100 format(a)
      end
 
C->>> ---------------------------------------------> ems_rp_al_vr_st <<<
c     Writes the status, lower bound, activity and upper bound for all
c     the variables
c
      subroutine ems_rp_al_vr_st(ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      include 'EMSMSGN.INC'
      include 'RSMICS.INC'
      double precision ds(0:ds_n_en_m1)
      integer is(0:is_n_en_m1)
      integer ix, vr_n
 
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
      call ems_msg_wr_li(rsmi_msg_n)
      do 10, ix=1, n_r+n_c
         vr_n = ix
         if (ix .gt. n_c) vr_n = ix + mx_n_c-n_c
         call ems_ca_rp_1_vr_st(0, vr_n, ds, is)
 10   continue
      return
 9000 format('   vr_n    ix_n',
     &     '           lower bound       primal activity',
     &     '           upper bound         dual activity',
     &     '  |  decoded status')
      end
 
C->>> -------------------------------------------> ems_ca_rp_1_vr_st <<<
c     Writes the status, lower bound, activity and upper bound for a
c     particular variable
c
      subroutine ems_ca_rp_1_vr_st(mode, vr_n, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      double precision ds(0:ds_n_en_m1)
      integer mode, vr_n, is(0:is_n_en_m1)
 
      call ems_rp_1_vr_st(mode, vr_n,
     &     ds(p_rsmi_lb+vr_n),
     &     ds(p_rsmi_ub+vr_n),
     &     is(p_st+vr_n),
     &     ds(p_pr_act+vr_n),
     &     ds(p_du_act+vr_n))
      return
      end
 
C->>> ----------------------------------------------> ems_rp_1_vr_st <<<
c     Writes the status, lower bound, activity and upper bound for a
c     particular variable
c
      subroutine ems_rp_1_vr_st(
     &     mode, vr_n, rsmi_lb, rsmi_ub, st, pr_act, du_act)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      include 'EMSMSGN.INC'
      include 'RSMICS.INC'
      include 'EMSFI.INC'
      include 'PRTS.INC'
      character*20 ems_wr_rl_t_ch20
      character*30 ems_st_t_ch30
      integer mode, vr_n, st
      double precision rsmi_lb
      double precision rsmi_ub
      double precision pr_act
      double precision du_act
      integer ems_g_vr_in_c_sn_ty_o_vr
      character*20 ch_lb, ch_ub, ch_pr_act, ch_du_act
      character*30 ch_st
      character*5 ch5_sn_ty
      integer ix_n, vr_in_c_sn_ty
 
      ix_n = iand(st, mx_mx_ml_a_dim)
      ch_st = ems_st_t_ch30(st)
      ch_lb = ems_wr_rl_t_ch20(rsmi_lb)
      ch_ub = ems_wr_rl_t_ch20(rsmi_ub)
      if (iand(ml_da_st_msk, ml_da_st_bc_pr_act) .eq. 0) then
         ch_pr_act = '                    '
      else
         ch_pr_act = ems_wr_rl_t_ch20(pr_act)
      endif
      if (iand(ml_da_st_msk, ml_da_st_non_bc_du_act) .ne. 0) then
         ch_du_act = '                    '
      else if (iand(st, bc_bt) .ne. 0) then
         ch_du_act = ems_wr_rl_t_ch20(zero)
      else
         ch_du_act = ems_wr_rl_t_ch20(du_act)
      endif
      if (mode .le. 1) then
         if (iand(st, bc_bt) .ne. 0) then
            ch5_sn_ty = 'Basic'
         else
            vr_in_c_sn_ty = ems_g_vr_in_c_sn_ty_o_vr(st)
            ch5_sn_ty = ch5_vr_in_c_sn_ty(vr_in_c_sn_ty)
         endif
         if (mode .eq. 0) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)vr_n, ix_n,
     &           ch_lb, ch_pr_act, ch_ub, ch_du_act, ch_st, ch5_sn_ty
         else
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)vr_n, ix_n,
     &           ch_lb, ch_pr_act, ch_ub, ch_du_act, ch_st, ch5_sn_ty
         endif
         call ems_msg_wr_li(rsmi_msg_n)
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9020)vr_n, ix_n,
     &        ch_lb, ch_pr_act, ch_ub, ch_du_act, ch_st
         call ems_msg_wr_li(rsmi_msg_n)
      endif
      return
 9000 format(i7, 1x, i7, 4(2x, a20), 2x, '|', 2x, a30, 2x, a5)
 9010 format('vr_n:', i7, ' ix_n:', i7,
     &     ' lb: ', a20, ' pr_act: ', a20, ' ub: ', a20,
     &     ' du_act: ', a20, ' st: ', a30, 2x, a5)
 9020 format('vr_n:', i7, ' ix_n:', i7,
     &     ' lb: ', a20, ' pr_act: ', a20, ' ub: ', a20,
     &     ' du_act: ', a20, ' st: ', a30)
      end
 
C->>> --------------------------------------------------> ems_it_xit <<<
c     iteration exit.
c
      subroutine ems_it_xit(ds, is, it_xit_reason, usr_rt_cod)
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
      include 'ITXITCS.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      double precision ds(0:ds_n_en_m1)
      integer is(0:is_n_en_m1)
      integer it_xit_reason, usr_rt_cod
      integer sv_ems_msg_cod
 
      usr_rt_cod = 0
c
c     Save ems_msg_cod in case the user calls an EMSOL routine
c
      sv_ems_msg_cod = ems_msg_cod
 100  continue
      if (it_xit_reason .gt. 0) then
         call ems_itru(ds, is, it_xit_reason, usr_rt_cod)
      else if (it_xit_reason .eq. it_xit_no_usr_cg) then
         call ems_itru(ds, is, it_xit_reason, usr_rt_cod)
         goto 7000
      endif
 
      if (it_xit_reason .eq. it_xit_af_cz_c) then
         call ems_sos_xit_af_cz_c(ds, is, usr_rt_cod,
     &        vr_t_en_bs, vr_t_lv_bs, lp_ph)
      else if (it_xit_reason .eq. it_xit_af_pr_it) then
         call ems_sos_xit_af_pr_it(ds, is, usr_rt_cod,
     &        vr_t_en_bs, vr_t_lv_bs, lp_ph)
      else if (it_xit_reason .eq. it_xit_af_reset) then
         call ems_sos_xit_af_reset(ds, is, usr_rt_cod,
     &        vr_t_en_bs, vr_t_lv_bs, lp_ph)
      endif
      if (lp_ph_cg) then
c
c     If a change of phase has been identified then change phase and
c     reset the basic costs.
c
         if (lp_ph .eq. 1) then
            lp_ph = 2
c     Changed 11/03/97
c            pr_co_mu = mx_mn*pr_wt
            pr_co_mu = mx_mn
         else
            lp_ph = 1
c     Changed 11/03/97
c            pr_co_mu = mx_mn
            pr_co_mu = mx_mn*pr_wt
         endif
      endif
      fresh_pc = lp_ph_cg
      if (lp_ph_cg) then
c
c     If the phase has changed then set the basic costs.
c
         call ems_se_bc_co(
     &        is(p_st),
     &        ds(p_rsmi_co),
     &        is(p_vr_in_r),
     &        ds(p_bc_co_v),
     &        is(p_bc_co_ix),
     &        is(p_bc_co_ix_bar))
         lp_ph_cg = .false.
      endif
      fresh_pc = fresh_pc .or. u_pc .eq. 0
      if (fresh_pc .or. is(p_pi_ix) .gt. 0) then
c
c     User intervention has resulted in a change of basic costs.
c
c     If the phase has changed or update pricing is not being used then
c     compute fresh dual activities.
c
         if (fresh_pc) then
c
c     Compute the dual activities from scratch.
c
            call ems_ze_pi_v(ds(p_pi_v), is(p_pi_ix))
            call ems_cp_nz_v_ix(n_r,
     &           is(p_bc_co_ix),
     &           ds(p_bc_co_v),
     &           is(p_pi_ix),
     &           ds(p_pi_v))
         endif
CM      IF (emsol_deb .EQ. 1) THEN
C?         if (wr_lp_da .ne. 0) call ems_wr_lp_da(3, ds, is)
CM      ENDIF
         if (sto_btran_ix .eq. sto_ix_no) is(p_pi_ix) = n_r+1
         call ems_btran(ds(p_pi_v), is(p_pi_ix), ds, is)
CM      IF (emsol_deb .EQ. 1) THEN
C?         if (wr_lp_da .ne. 0) call ems_wr_lp_da(4, ds, is)
CM      ENDIF
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl0) call ems_tt_rec(pc_tt, n_bs)
CM      ENDIF
c
c     If the INVERT is permuted and there is no copy of the matrix
c     stored by rows then permute the pi vector.
c
         if (iand(inv_alg_msk, inv_alg_perm) .ne. 0 .and.
     &        iand(ml_da_st_msk, ml_da_st_r_mtx) .eq. 0)
     &        call ems_perm_btran_sol(
     &        p_pi_v, hdl_pi_v, is(p_pi_ix), ds, is)
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
     &              is(p_nw_t_og_perm))
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
         else
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
         endif
c
c     Indicate that CHUZC has not been performed for the current dual
c     activities and bounds on non-basic variables.
c
         rsmi_op_st_msk =
     &        rsmi_op_st_msk - iand(rsmi_op_st_msk, rsmi_op_st_cz_c)
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl0) call ems_tt_rec(-pc_tt, n_bs)
CM      ENDIF
      endif
      if (it_xit_reason .eq. it_xit_af_cz_c .and.
     &     iand(rsmi_op_st_msk, rsmi_op_st_cz_c) .eq. 0) then
c
c     Repeat CHUZC
c
         call ems_cz_1_c(is(p_vr_in_c), ds, is)
         vr_t_en_bs = nx_vr_t_en_bs
         if (usr_rt_cod .eq. 1) goto 100
      endif
 7000 continue
      ems_msg_cod = max(ems_msg_cod, sv_ems_msg_cod)
      return
      end
 
C->>> ---------------------------------------------------> ems_cg_bd <<<
c     Change the bounds on a variable
c     rt_cod = 0: OK
c     rt_cod = 1: Variable number out of range
c     rt_cod = 2: New bounds are inconsistent
c     rt_cod = 3: New bounds make nonbasic activity infeasible.
c
      subroutine ems_cg_bd(rt_cod, vr_n, usr_nw_ml_lb, usr_nw_ml_ub,
     &    lbc, ubc, pr_act, st, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMICS.INC'
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      integer rt_cod, vr_n
      double precision usr_nw_ml_lb
      double precision usr_nw_ml_ub
      double precision lbc
      double precision ubc
      double precision pr_act
      double precision ds(0:ds_n_en_m1)
      integer st
      integer is(0:is_n_en_m1)
      integer vr_st, nw_st, r_n, c_n
      integer n_pi_ix, pi_ix_n, n_bc_co_ix, r_o_l_ix
      double precision nw_ml_lb, nw_ml_ub, rsdu, dl_bc_co
      double precision scl_v, ml_pr_act, nw_rsmi_lb, nw_rsmi_ub
 
      rt_cod = 0
c
c     Check whether the variable number is OK
c
      if (vr_n .gt. mx_n_c) then
         r_n = vr_n - mx_n_c
         if (r_n .le. 0 .or. r_n .gt. n_r) goto 8010
      else
         c_n = vr_n
         if (c_n .le. 0 .or. c_n .gt. n_c) goto 8020
      endif
      nw_ml_lb = usr_nw_ml_lb
      nw_ml_ub = usr_nw_ml_ub
c
c     Check whether the new bounds are consistent.
c
      if (nw_ml_lb .gt. nw_ml_ub) goto 8030
c
c     Remove the lower and upper bound bits from the status and then
c     determine whether the variable has finite lower and upper bounds,
c     setting the appropriate bits.
c
      if (iand(ml_da_st_msk, ml_da_st_scl_ml) .ne. 0) then
         scl_v = ds(p_scl+vr_n)
         ml_pr_act = ds(p_pr_act+vr_n)/scl_v
      else
         scl_v = one
         ml_pr_act = ds(p_pr_act+vr_n)
      endif
      vr_st = is(p_st+vr_n)
      vr_st = vr_st - iand(vr_st, lb_ub)
      if (nw_ml_lb .gt. -inf) then
         vr_st = vr_st + lb_bt
         nw_rsmi_lb = nw_ml_lb*scl_v
      else
         nw_ml_lb = -inf
         nw_rsmi_lb = -inf
      endif
      if (nw_ml_ub .lt. inf) then
         vr_st = vr_st + ub_bt
         nw_rsmi_ub = nw_ml_ub*scl_v
      else
         nw_ml_ub = inf
         nw_rsmi_ub = inf
      endif
c
c     If the variable is nonbasic then check whether it is feasible with
c     respect to the new bounds---it should be and an error is returned
c     if it is not.
c
      if (iand(vr_st, bc_bt) .eq. 0) then
         if (iand(vr_st, lb_bt) .ne. 0) then
            if (nw_ml_lb - ml_pr_act .gt. tl_pr_ifs) goto 8040
         endif
         if (iand(vr_st, ub_bt) .ne. 0) then
            if (ml_pr_act - nw_ml_ub .gt. tl_pr_ifs) goto 8050
         endif
      endif
c
c     The model will change so switch off the mechanism which saves
c     basis changes
c
      call ems_sv_bs_cg_stop
c
c     The bound changes are acceptable so change the values of the
c     bounds (RSMI and original) and the status.
c
      ds(p_rsmi_lb+vr_n) = nw_rsmi_lb
      ds(p_rsmi_ub+vr_n) = nw_rsmi_ub
      ds(p_lbc+vr_n) = nw_ml_lb
      ds(p_ubc+vr_n) = nw_ml_ub
      is(p_st+vr_n) = vr_st
c
c     Determine the effect on basic feasibility or position in the
c     pricing list.
c
      if (iand(vr_st, bc_bt) .ne. 0) then
c
c     The variable is basic. The new up, down and feasibility bits will
c     be set in nw_st but the old settings are needed in order to
c     determine the changes.
c
         nw_st = vr_st -
     &        iand(vr_st, up_dn) -
     &        iand(vr_st, ifs_bt)
c
c     Initialise the change in basic cost and record the position of
c     this variable in vr_in_r.
c
         dl_bc_co = zero
         r_n = iand(vr_st, mx_mx_ml_a_dim)
         if (iand(vr_st, ifs_bt) .eq. 0) then
c
c     The variable was previously feasible.
c
            if (iand(vr_st, lb_bt) .ne. 0) then
               rsdu = ds(p_rsmi_lb+vr_n) - ds(p_pr_act+vr_n)
               if (rsdu .gt. tl_pr_ifs) then
c
c     The variable is below its new lower bound so set the up bit and
c     set the change in the basic cost to -1.
c
                  n_pr_ifs = n_pr_ifs + 1
                  nw_st = nw_st + up_bt + ifs_bt
                  dl_bc_co = -one
                  go to 1000
               endif
            endif
            if (iand(vr_st, ub_bt) .ne. 0) then
               rsdu = ds(p_pr_act+vr_n) - ds(p_rsmi_ub+vr_n)
               if (rsdu .gt. tl_pr_ifs) then
c
c     The variable is above its new upper bound so set the down bit and
c     set the change in the basic cost to +1.
c
                  n_pr_ifs = n_pr_ifs + 1
                  nw_st = nw_st + dn_bt + ifs_bt
                  dl_bc_co = one
                  go to 1000
               endif
            endif
         else
c
c     The variable was previously infeasible.
c
            if (iand(vr_st, lb_bt) .ne. 0) then
               rsdu = ds(p_rsmi_lb+vr_n) - ds(p_pr_act+vr_n)
               if (rsdu .gt. tl_pr_ifs) then
c
c     The variable is below its new lower bound so set the up bit.
c
                  nw_st = nw_st + up_bt + ifs_bt
c
c     If the variable was previously above its upper bound then the
c     change in the basic cost is -2.
c
                  if (iand(vr_st, dn_bt) .ne. 0) dl_bc_co = -two
                  go to 1000
               endif
            endif
            if (iand(vr_st, ub_bt) .ne. 0) then
               rsdu = ds(p_pr_act+vr_n) - ds(p_rsmi_ub+vr_n)
               if (rsdu .gt. tl_pr_ifs) then
c
c     The variable is above its new upper bound so set the down bit.
c
                  nw_st = nw_st + dn_bt + ifs_bt
c
c     If the variable was previously below its lower bound then the
c     change in the basic cost is +2.
c
                  if (iand(vr_st, up_bt) .ne. 0) dl_bc_co = two
                  go to 1000
               endif
            endif
c
c     The variable is now feasible
c
            n_pr_ifs = n_pr_ifs - 1
            if (iand(vr_st, up_bt) .ne. 0) then
c
c     The variable was previously below its lower bound so the change in
c     the basic cost is +1.
c
               dl_bc_co = one
            else if (iand(vr_st, dn_bt) .ne. 0) then
c
c     The variable was previously above its upper bound so the change in
c     the basic cost is -1.
c
               dl_bc_co = -one
            endif
         endif
 1000    continue
         is(p_st+vr_n) = nw_st
      else
c
c     The variable is nonbasic. Change the pricing list corresponding to
c     the bound change.
c
         c_n = iand(vr_st, mx_mx_ml_a_dim)
         call ems_u_vr_in_c(c_n, vr_n,
     &        ds(p_rsmi_lb),
     &        ds(p_rsmi_co),
     &        ds(p_rsmi_ub),
     &        is(p_st),
     &        ds(p_pr_act),
     &        ds(p_du_act),
     &        tl_pr_ifs, is(p_vr_in_c), ds, is)
c
c     Indicate that CHUZC has not been performed for the current dual
c     activities and bounds on non-basic variables.
c
         rsmi_op_st_msk =
     &        rsmi_op_st_msk - iand(rsmi_op_st_msk, rsmi_op_st_cz_c)
      endif
c
c     Indicate whether infeasibility during phase II or feasibility
c     during phase I has been detected. If it has then the basic costs
c     will be reset and dual activities will be calculated from scratch
c     so return.
c
      lp_ph_cg = lp_ph_cg .or.
     &     (lp_ph .eq. 2 .and. n_pr_ifs .gt. 0) .or.
     &     (lp_ph .eq. 1 .and. n_pr_ifs .eq. 0)
c
c     If the phase changes then the basic costs will be reset so don't
c     form the update pi RHS
c
      if (lp_ph_cg) goto 7000
c
c     If the variable was basic and its basic cost has changed then
c     determine the nature of the nonzeros in pi and bc_co.
c
      if (iand(vr_st, bc_bt) .ne. 0) then
         if (abs(dl_bc_co) .gt. bwd_tran_ze) then
c
c     Set pointers for updating bc_co and pi
c
c
c     Record the number of entries in pi.
c
            n_pi_ix = is(p_pi_ix)
            if (ds(p_pi_v+r_n) .ne. zero) then
c
c     The row was already in pi so find where it is in the list of
c     indices.
c
               do 1010, pi_ix_n = 1, n_pi_ix
                  if (is(p_pi_ix+pi_ix_n) .eq. r_n) goto 1020
 1010          continue
               goto 8060
 1020          continue
               ds(p_pi_v+r_n) = ds(p_pi_v+r_n) + dl_bc_co
               if (abs(ds(p_pi_v+r_n)) .le. bwd_tran_ze) then
c
c     A nonzero in pi has been zeroed so remove it from the list.
c
                  ds(p_pi_v+r_n) = zero
                  is(p_pi_ix+pi_ix_n) = is(p_pi_ix+n_pi_ix)
                  is(p_pi_ix) = n_pi_ix - 1
               endif
            else
c
c     The row was not already in pi so put it at the end of the list.
c
               pi_ix_n = n_pi_ix + 1
               ds(p_pi_v+r_n) = dl_bc_co
               is(p_pi_ix+pi_ix_n) = r_n
               is(p_pi_ix) = pi_ix_n
            endif
c
c     Determine the nature of the basic cost value.
c
            ds(p_bc_co_v+r_n) = ds(p_bc_co_v+r_n) + dl_bc_co
            n_bc_co_ix = is(p_bc_co_ix)
            if (ds(p_bc_co_v+r_n) .ne. zero) then
               if (is(p_bc_co_ix_bar+r_n) .eq. 0) then
c
c     A zero entry has become nonzero. Add its index to the end of the
c     list and increase the length of the list by one.
c
                  n_bc_co_ix = n_bc_co_ix + 1
                  is(p_bc_co_ix) = n_bc_co_ix
                  is(p_bc_co_ix+n_bc_co_ix) = r_n
                  is(p_bc_co_ix_bar+r_n) = n_bc_co_ix
               endif
            else
               if (is(p_bc_co_ix_bar+r_n) .ne. 0) then
c
c     A nonzero entry has become zero. Replace its index in the list by
c     the index of the last entry in the list and reduce the length of
c     the list by one.
c
                  r_o_l_ix = is(p_bc_co_ix+n_bc_co_ix)
                  is(p_bc_co_ix+is(p_bc_co_ix_bar+r_n)) = r_o_l_ix
                  is(p_bc_co_ix_bar+r_o_l_ix) = is(p_bc_co_ix_bar+r_n)
                  is(p_bc_co_ix_bar+r_n) = 0
                  is(p_bc_co_ix) = n_bc_co_ix - 1
               endif
            endif
         endif
      endif
 7000 continue
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)vr_n, r_n, n_r
      call ems_msg_wr_li(warn_msg_n)
      rt_cod = 1
      goto 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)vr_n, c_n, n_c
      call ems_msg_wr_li(warn_msg_n)
      rt_cod = 1
      goto 7000
 8030 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9803)
     &     nw_ml_lb, nw_ml_ub
      call ems_msg_wr_li(warn_msg_n)
      rt_cod = 2
      goto 7000
 8040 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9804)
     &     nw_ml_lb, ml_pr_act
      call ems_msg_wr_li(warn_msg_n)
      rt_cod = 3
      goto 7000
 8050 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9805)
     &     nw_ml_ub, ml_pr_act
      call ems_msg_wr_li(warn_msg_n)
      rt_cod = 3
      goto 7000
 8060 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9806)
      call ems_msg_wr_li(bug_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?      call ems_dump
CM      ENDIF
      rt_cod = 3
      goto 7000
 9801 format('Variable number of ', i7,
     &     ' corresponds to row ', i7,
     &     ' out of range 1:', i7)
 9802 format('Variable number of ', i7,
     &     ' corresponds to column ', i7,
     &     ' out of range 1:', i7)
 9803 format('New upper lower bound ', g11.4,
     &    ' inconsistent with new upper bound ', g11.4)
 9804 format('New lower bound ', g11.4,
     &    ' makes nonbasic activity ', g11.4, ' infeasible')
 9805 format('New upper bound ', g11.4,
     &    ' makes nonbasic activity ', g11.4, ' infeasible')
 9806 format('ERROR: nonzero in pi but not in list of indices')
      end
