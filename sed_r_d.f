CM      IF (dan .EQ. 1) THEN
C?C->>> ----------------------------------------------> ems_dan_cz_r <<<
C?c
CM      IF (sps_cz_r .EQ. 1) THEN
C?      subroutine ems_dan_sps_cz_r(
CM      ELSE
C?      subroutine ems_dan_cz_r(
CM      ENDIF
C?     &     rp_growth, refined_pv_c, refine_pv_c,
CM      ENDIF
CM      IF (dvx .EQ. 1) THEN
C?C->>> ----------------------------------------------> ems_dvx_cz_r <<<
C?c
CM      IF (sps_cz_r .EQ. 1) THEN
C?      subroutine ems_dvx_sps_cz_r(
CM      ELSE
C?      subroutine ems_dvx_cz_r(
CM      ENDIF
C?     &     rp_growth, refined_pv_c, refine_pv_c,
C?     &     ed_wt,
C?     &     dvx_ix,
CM      ENDIF
CM      IF (sed .EQ. 1) THEN
C->>> ----------------------------------------------> ems_sed_cz_r <<<
c
CM      IF (sps_cz_r .EQ. 1) THEN
C?      subroutine ems_sed_sps_cz_r(
CM      ELSE
      subroutine ems_sed_cz_r(
CM      ENDIF
     &     rp_growth, refined_pv_c, refine_pv_c,
     &     mx_ed_wt_er,
     &     ed_wt,
CM      ENDIF
     &     rsmi_lb, rsmi_ub, pr_act, st, vr_in_r,
     &     nw_eta_v, pv_c_v, nw_eta_ix,
     &     cdd_ix,
     &     ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMICS.INC'
      include 'RSMICOM.INC'
      include 'MORSMI.INC'
CM      IF (emsol_da .EQ. 1) THEN
C?      include 'EMSDA.INC'
CM      ENDIF
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
 
      integer rd_en_vr_n, rd_lv_vr_n
      common/ems_rd_lv_vr_n_com/rd_en_vr_n, rd_lv_vr_n
 
      logical rp_growth, refined_pv_c, refine_pv_c
      integer st(0:mx_n_c+n_r)
      integer vr_in_r(0:n_r), nw_eta_ix(0:n_r)
      integer cdd_ix(0:1+n_r), is(0:is_n_en_m1)
CM      IF (dvx .EQ. 1) THEN
C?      integer dvx_ix(0:mx_n_c+n_r)
C?      double precision ed_wt(0:mx_n_c+n_r)
CM      ENDIF
CM      IF (sed .EQ. 1) THEN
      double precision mx_ed_wt_er
      double precision ed_wt(0:mx_n_c+n_r)
CM      ENDIF
      double precision rsmi_lb(0:mx_n_c+n_r)
      double precision rsmi_ub(0:mx_n_c+n_r)
      double precision pr_act(0:mx_n_c+n_r)
      double precision nw_eta_v(0:n_r), pv_c_v(0:n_r)
      double precision ds(0:ds_n_en_m1)
c
      integer r_n, vr_n, vr_st
      integer ix_n, ix_o_f_fs_vr_t_bd, ix_o_l_ifs_vr_t_bd
      integer ix_o_vr_t_lv_bs, loop_n, n_rpt, rpt
      integer n_cdd_ix, n_cdd_ix0, cdd_ix_n
      integer cdd_ix_n_o_l_ifs_vr_t_bd, cdd_ix_n_o_f_fs_vr_t_bd
      integer n_ifs_cdd_r, n_ifs_r
      integer growth_mode
      integer n_mv_bd, n_cg_act
CM      IF (sps_cz_r .EQ. 1) THEN
C?      integer og_ix_n, og_n_ix
CM      ENDIF
      logical r_n_in_cdd_ls
      logical ck_cdd_ls
      logical al_cdd_ifs
      logical rpt_cz_r
      integer prev_n_ix
CM      IF (dvx .EQ. 1) THEN
C?      integer i_te
C?      double precision dvx_rao
CM      ENDIF
CM      IF (sed .EQ. 1) THEN
      double precision ed_wt_er
CM      ENDIF
      double precision aa_1, aa_2
      double precision rao_fs, rao_ifs
      double precision aa_fs, aa_ifs
      double precision psi
      double precision ok_pv, mx_pv
      double precision rsdu
      double precision growth
      double precision og_tl_pr_ifs
      double precision mx_dl_bd, mx_dl_act
CM      IF (sed .EQ. 1) THEN
      double precision og_ed_wt_o_vr_t_en_bs
CM      ENDIF
CM      IF (dvx .EQ. 1) THEN
C?      double precision og_ed_wt_o_vr_t_en_bs
CM      ENDIF
CM      IF (emsol_da .EQ. 1) THEN
C?c      integer i
C?c      double precision v
CM      ENDIF
c      double precision rl_null
c      save rl_null
c      data rl_null/0d0/
 
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_cz_r_lvl0) call ems_tt_rec(cz_r_tt, n_bs)
CM      ENDIF
      prev_n_ix = i_inf
 100  continue
CM      IF (dvx .EQ. 1) THEN
C?      og_ed_wt_o_vr_t_en_bs = ed_wt(vr_t_en_bs)
C?      ed_wt_o_vr_t_en_bs = zero
CM      ENDIF
CM      IF (sed .EQ. 1) THEN
      og_ed_wt_o_vr_t_en_bs = ed_wt(vr_t_en_bs)
      ed_wt_o_vr_t_en_bs = zero
CM      ENDIF
      mx_pv_c_v = zero
      mx_pv = one
      alg_er = .false.
      un_bd = .false.
      refine_pv_c = .false.
      r_n_in_cdd_ls = .false.
      ck_cdd_ls = .false.
      al_cdd_ifs = .true.
c
c     Set ix_n = -1, vr_in_r(0) = vr_t_en_bs and pv_c_v(0) = 1 so that
c     this value is packed into nw_eta_v(0) with index nw_eta_ix(0) =
c     vr_t_en_bs. Thus, ratios for bound swaps can be determined in the
c     same loop as the other ratios.
c
      ix_n = -1
      vr_in_r(0) = vr_t_en_bs
c     NO_SGN pv_c_v(0) = one
c     SGN pv_c_v(0) = mv_dir*one
CM      IF (sps_cz_r .EQ. 1) THEN
C?      og_n_ix = nw_eta_ix(0)
C?      nw_eta_ix(0) = 0
CM      ENDIF
      pv_c_v(0) = one
      nw_eta_f_ix = 1
      if (lp_ph .eq. 1) goto 500
c=======================================================================
c     Start of phase II cz_r
c
c     Ratio test with expanded bounds.
c
c=======================================================================
c
      tl_pr_ifs = tl_pr_ifs + xp_tau
      n_rpt = 0
 200  continue
      n_cdd_ix = 0
      rsdu = inf
      og_tl_pr_ifs = tl_pr_ifs
      aa_1 = inf
      ix_o_vr_t_lv_bs = -1
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_cz_r_lvl1)
CM      IF (sps_cz_r .EQ. 1) THEN
C?     &     call ems_tt_rec(cz_r_ph_2_sps_ps_1_tt, n_bs)
CM      ELSE
C?     &     call ems_tt_rec(cz_r_ph_2_dse_ps_1_tt, n_bs)
CM      ENDIF
CM      ENDIF
c      call ems_rp_cz_r(lp_ph, -1,
c     &   0, rl_null, rl_null, rl_null, rl_null)
CM      IF (sps_cz_r .EQ. 1) THEN
C?      do 210, og_ix_n = 0, og_n_ix
C?         r_n = nw_eta_ix(og_ix_n)
CM      ELSE
      do 210, r_n = 0, n_r
CM      ENDIF
         pv = pv_c_v(r_n)
CM      IF (emsol_da .EQ. 1) THEN
C?c         if (pv .ne. zero) then
C?c            v = abs(pv)
C?c            if (v .le. 1d0) v = v*1d-1
C?c            i = log10(v)
C?c            if (i .lt. mn_pv_c_v_rec_by_1)
C?c     &           i = mn_pv_c_v_rec_by_1-1 - (mn_pv_c_v_rec_by_1-i)/10
C?c            i = max(min(i, mx_pv_c_v_rec_by_1), mn_pv_c_v_rec_by_10)
C?c            pv_c_v_rec(i) = pv_c_v_rec(i) + 1
C?c            su_n_pk_pv_c_en = su_n_pk_pv_c_en + 1
C?c            if (abs(pv) .le. pk_pv_c_ze)
C?c     &           su_n_pk_pv_c_ze = su_n_pk_pv_c_ze + 1
C?c         end if
CM      ENDIF
         if (pv .eq. zero) goto 210
CM      IF (dan .EQ. 1) THEN
C?         pv_c_v(r_n) = zero
CM      ENDIF
CM      IF (dvx .EQ. 1) THEN
C?         pv_c_v(r_n) = zero
CM      ENDIF
         if (abs(pv) .le. pk_pv_c_ze) then
            pv_c_v(r_n) = zero
            goto 210
         end if
         ix_n = ix_n + 1
         mx_pv_c_v = max(abs(pv), mx_pv_c_v)
         nw_eta_v(ix_n) = pv
         nw_eta_ix(ix_n) = r_n
         vr_n = vr_in_r(r_n)
CM      IF (dvx .EQ. 1) THEN
C?         if (dvx_ix(vr_n) .gt. 0)
C?     &        ed_wt_o_vr_t_en_bs = ed_wt_o_vr_t_en_bs + pv*pv
CM      ENDIF
CM      IF (sed .EQ. 1) THEN
         ed_wt_o_vr_t_en_bs = ed_wt_o_vr_t_en_bs + pv*pv
CM      ENDIF
c
c     Now complete the CHUZR pass 1 loop.
c
c     NO_SGN pv = mv_dir*pv
         pv = mv_dir*pv
         vr_st = st(vr_n)
c         call ems_rp_cz_r(lp_ph, r_n, vr_st,
c     &        pv, rsmi_lb(vr_n), pr_act(vr_n), rsmi_ub(vr_n))
         if (pv .gt. zero) then
            if (iand(vr_st, ub_bt) .ne. 0) then
c
c     The variable is moving towards an upper bound or breakpoint.
c
               rsdu = rsmi_ub(vr_n) - pr_act(vr_n)
c     Surely a mistake!! Changed 12/02/98
c               if (rsdu .lt. aa_1*pv) then
c     Surely a mistake!! Changed 12/02/98
               if (rsdu .le. aa_1*pv) then
c
c     The standard ratio is less than the current smallest ratio with
c     respect to expanded bounds.
c
c     *    it may be a candidate in the pass 2 ratio test.
c
c     If it corresponds to a bound rather than just a breakpoint then
c
c     *    it may give a ratio with respect to the expanded bound which
c          is smaller than the current smallest;
c
c     NB It the variable is BP then there is no need to consider this.
c
                  n_cdd_ix = n_cdd_ix + 1
                  cdd_ix(n_cdd_ix) = ix_n
                  rsdu = rsdu + tl_pr_ifs
                  if (rsdu .lt. aa_1*pv) then
                     aa_1 = rsdu/pv
                     ix_o_vr_t_lv_bs = ix_n
                  end if
               end if
            else
               goto 210
            end if
         else
            if (iand(vr_st, lb_bt) .ne. 0) then
c
c     The variable is moving towards a lower bound or breakpoint.
c
c     NB the residual rsmi_lb(vr_n)-pr_act(vr_n) and the pivot are both
c     negative so reverse the sign of the inequality.
c
               rsdu = rsmi_lb(vr_n) - pr_act(vr_n)
c     Surely a mistake!! Changed 12/02/98
c               if (rsdu .gt. aa_1*pv) then
c     Surely a mistake!! Changed 12/02/98
               if (rsdu .ge. aa_1*pv) then
c
c     The standard ratio is less than the current smallest ratio with
c     respect to expanded bounds.
c
c     *    it may be a candidate in the pass 2 ratio test.
c
c     If it corresponds to a bound rather than just a breakpoint then
c
c     *    it may give a ratio with respect to the expanded bound which
c          is smaller than the current smallest;
c
c     NB It the variable is BP then there is no need to consider this.
c
                  n_cdd_ix = n_cdd_ix + 1
                  cdd_ix(n_cdd_ix) = ix_n
                  rsdu = rsdu - tl_pr_ifs
                  if (rsdu .gt. aa_1*pv) then
                     aa_1 = rsdu/pv
                     ix_o_vr_t_lv_bs = ix_n
                  end if
               end if
            else
               goto 210
            end if
         end if
 210  continue
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_cz_r_lvl1)
CM      IF (sps_cz_r .EQ. 1) THEN
C?     &     call ems_tt_rec(-cz_r_ph_2_sps_ps_1_tt, n_bs)
CM      ELSE
C?     &     call ems_tt_rec(-cz_r_ph_2_dse_ps_1_tt, n_bs)
CM      ENDIF
CM      ENDIF
      nw_eta_l_ix = ix_n
      ok_pv = mx_pv_c_v/tl_cz_r_growth
      if (ix_o_vr_t_lv_bs .lt. 0) then
         loop_n = 210
         goto 8040
      end if
      pv = nw_eta_v(ix_o_vr_t_lv_bs)
      vr_t_lv_bs = vr_in_r(nw_eta_ix(ix_o_vr_t_lv_bs))
      if (aa_1 .lt. zero) then
         call ems_consider_rpt_rao_ts(
     &        rpt, n_rpt, pv, vr_t_lv_bs, aa_1,
     &        st, rsmi_lb, rsmi_ub, pr_act, ds, is)
         if (rpt .eq. -5) then
            goto 8050
         else if (rpt .eq. -3) then
            goto 8030
         else
            goto 200
         end if
      end if
c
c     Alpha_1 should not be less than the minimum step due to primal
c     feasibility tolerance.
c
      if (aa_1 .lt. xp_tau/abs(pv)) goto 8060
      if (ix_o_vr_t_lv_bs .eq. 0) then
c
c     If the first variable to become infeasible is the entering
c     variable then the ratio test yields a bound swap. The step alpha_1
c     swaps the activity to its expanded bound so set alpha to the step
c     to the original bound (alpha < alpha_1).
c
         vr_t_lv_bs = vr_in_r(0)
         aa = inf
         if (mv_dir .gt. 0) then
            aa = rsmi_ub(vr_t_lv_bs) - pr_act(vr_t_lv_bs)
         else
            aa = pr_act(vr_t_lv_bs) - rsmi_lb(vr_t_lv_bs)
         end if
c
c     If alpha for a bound swap is less than the minimum step then this
c     suggests something very odd. Any bound swap should be at least
c     the current infeasibility tolerance which is greater than the
c     minimum step.
c
         if (aa .lt. xp_tau) goto 8070
c         call ems_rp_cz_r(lp_ph, n_r+1, 0, aa_1, aa, rl_null, rl_null)
c         call ems_rp_cz_r(lp_ph, n_r+2,
c     &   0, aa, mx_pv_c_v, rl_null, rl_null)
         goto 1000
      end if
c
c     End of first pass for phase II.
c=======================================================================
c     Start of second pass for phase II.
c
      aa_2 = inf
      mx_pv = zero
      ix_o_vr_t_lv_bs = 0
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_cz_r_lvl1) call ems_tt_rec(cz_r_ph_2_ps_2_tt, n_bs)
CM      ENDIF
      do 310, cdd_ix_n = 1, n_cdd_ix
c
c     Note that all candidates have a pivot which is sufficiently large
c     and a finite value to move to which is given by the signed pivot.
c
         ix_n = cdd_ix(cdd_ix_n)
         pv = nw_eta_v(ix_n)
c     NO_SGN pv = mv_dir*pv
         pv = mv_dir*pv
         vr_n = vr_in_r(nw_eta_ix(ix_n))
c Not used!         vr_st = st(vr_n)
         if (pv .gt. zero) then
            rsdu = rsmi_ub(vr_n) - pr_act(vr_n)
            if (rsdu .le. aa_1*pv) then
               if (pv .gt. mx_pv) then
                  aa_2 = rsdu/pv
                  ix_o_vr_t_lv_bs = ix_n
                  mx_pv = pv
               end if
            end if
         else
            rsdu = rsmi_lb(vr_n) - pr_act(vr_n)
            if (rsdu .ge. aa_1*pv) then
               if (-pv .gt. mx_pv) then
                  aa_2 = rsdu/pv
                  ix_o_vr_t_lv_bs = ix_n
                  mx_pv = -pv
               end if
            end if
         end if
 310  continue
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_cz_r_lvl1) call ems_tt_rec(-cz_r_ph_2_ps_2_tt, n_bs)
CM      ENDIF
      if (ix_o_vr_t_lv_bs .lt. 0) then
         loop_n = 310
         goto 8040
      end if
      aa = aa_2
      pv = nw_eta_v(ix_o_vr_t_lv_bs)
      if (ix_o_vr_t_lv_bs .gt. 0 .and. abs(pv) .lt. ok_pv) then
c
c     Growth will occur if this pivot is used: possibly refine the
c     pivotal column.
c
         if (iand(cz_r_msk, cz_r_refine_bt) .ne. 0 .and.
     &        .not. refined_pv_c) then
c
c     Set up the values required to monitor growth.
c
            pv_r_n = nw_eta_ix(ix_o_vr_t_lv_bs)
            vr_n = vr_in_r(pv_r_n)
            growth = abs(mx_pv_c_v/pv)
CM      IF (emsol_dev .EQ. 1) THEN
C?            call ems_mo_rsmi_growth(n_si_it, growth_act_refine_pv_c,
C?     &           pv_r_n, vr_n, pv, aa, growth, zero, 0)
CM      ENDIF
            refine_pv_c = .true.
            rp_growth = .true.
            goto 7000
         end if
      end if
c      call ems_rp_cz_r(lp_ph, n_r+1, 0, aa_1, aa_2, rl_null, rl_null)
c      call ems_rp_cz_r(lp_ph, n_r+2,
c     &   0, aa, mx_pv_c_v, rl_null, rl_null)
      goto 1000
c
c     End of second pass for phase II.
c=======================================================================
c
c     End of phase II cz_r
c=======================================================================
c     Start of phase I cz_r
 500  continue
c
c     Ratio test with expanded bounds.
c
c     Determine ix_o_f_fs_vr_t_bd, the index of the first variable to
c     become infeasible, and ix_o_l_ifs_vr_t_bd, the index of the last
c     infeasible activity to reach its nearest bound.
c
c=======================================================================
c     Start of first pass for phase I.
c
      tl_pr_ifs = tl_pr_ifs + xp_tau
      n_rpt = 0
 600  continue
      n_ifs_r = 0
      n_cdd_ix = 0
      rsdu = inf
      og_tl_pr_ifs = tl_pr_ifs
c
c     Psi is the maximum pivot for infeasible basic variables which move
c     towards their nearest bound.
c
      psi = zero
      aa_1 = inf
      ix_o_vr_t_lv_bs = -1
      vr_st = st(vr_t_en_bs)
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_cz_r_lvl1)
CM      IF (sps_cz_r .EQ. 1) THEN
C?     &     call ems_tt_rec(cz_r_ph_1_sps_ps_1_tt, n_bs)
CM      ELSE
C?     &     call ems_tt_rec(cz_r_ph_1_dse_ps_1_tt, n_bs)
CM      ENDIF
CM      ENDIF
c      call ems_rp_cz_r(lp_ph, -1,
c     &   0, rl_null, rl_null, rl_null, rl_null)
CM      IF (sps_cz_r .EQ. 1) THEN
C?      do 610, og_ix_n = 0, og_n_ix
C?         r_n = nw_eta_ix(og_ix_n)
CM      ELSE
      do 610, r_n = 0, n_r
CM      ENDIF
         pv = pv_c_v(r_n)
CM      IF (emsol_da .EQ. 1) THEN
C?c         if (pv .ne. zero) then
C?c            v = abs(pv)
C?c            if (v .le. 1d0) v = v*1d-1
C?c            i = log10(v)
C?c            if (i .lt. mn_pv_c_v_rec_by_1)
C?c     &           i = mn_pv_c_v_rec_by_1-1 - (mn_pv_c_v_rec_by_1-i)/10
C?c            i = max(min(i, mx_pv_c_v_rec_by_1), mn_pv_c_v_rec_by_10)
C?c            pv_c_v_rec(i) = pv_c_v_rec(i) + 1
C?c            su_n_pk_pv_c_en = su_n_pk_pv_c_en + 1
C?c            if (abs(pv) .le. pk_pv_c_ze)
C?c     &           su_n_pk_pv_c_ze = su_n_pk_pv_c_ze + 1
C?c         end if
CM      ENDIF
         if (pv .eq. zero) goto 610
CM      IF (dan .EQ. 1) THEN
C?         pv_c_v(r_n) = zero
CM      ENDIF
CM      IF (dvx .EQ. 1) THEN
C?         pv_c_v(r_n) = zero
CM      ENDIF
         if (abs(pv) .le. pk_pv_c_ze) then
            pv_c_v(r_n) = zero
            goto 610
         end if
         ix_n = ix_n + 1
         mx_pv_c_v = max(abs(pv), mx_pv_c_v)
         nw_eta_v(ix_n) = pv
         nw_eta_ix(ix_n) = r_n
         vr_n = vr_in_r(r_n)
CM      IF (dvx .EQ. 1) THEN
C?         if (dvx_ix(vr_n) .gt. 0)
C?     &        ed_wt_o_vr_t_en_bs = ed_wt_o_vr_t_en_bs + pv*pv
CM      ENDIF
CM      IF (sed .EQ. 1) THEN
         ed_wt_o_vr_t_en_bs = ed_wt_o_vr_t_en_bs + pv*pv
CM      ENDIF
c
c     Now complete the CHUZR pass 1 loop.
c
c     NO_SGN pv = mv_dir*pv
         pv = mv_dir*pv
         vr_st = st(vr_n)
c         call ems_rp_cz_r(lp_ph, r_n, vr_st,
c     &        pv, rsmi_lb(vr_n), pr_act(vr_n), rsmi_ub(vr_n))
c
c     Compare the residual with the scaled current smallest ratio to
c     determine whether this row will be a candidate in pass 2.
c
         if (iand(vr_st, ifs_bt) .ne. 0) then
            if (pv .gt. zero) then
               if (iand(vr_st, up_bt) .ne. 0) then
                  n_ifs_r = n_ifs_r + 1
                  psi = max(pv, psi)
                  rsdu = rsmi_lb(vr_n) - pr_act(vr_n)
c     Surely a mistake!! Changed 12/02/98
c                  if (rsdu .lt. aa_1*pv) then
c     Surely a mistake!! Changed 12/02/98
                  if (rsdu .le. aa_1*pv) then
                     n_cdd_ix = n_cdd_ix + 1
                     cdd_ix(n_cdd_ix) = ix_n
                     if (iand(vr_st, ub_bt) .ne. 0) then
                        rsdu = (rsmi_ub(vr_n) - pr_act(vr_n)) +
     &                       tl_pr_ifs
                        if (rsdu .lt. aa_1*pv) then
                           aa_1 = rsdu/pv
                           ix_o_vr_t_lv_bs = ix_n
                        end if
                     end if
                  end if
               else
                  goto 610
               end if
            else
               if (iand(vr_st, dn_bt) .ne. 0) then
                  n_ifs_r = n_ifs_r + 1
                  psi = max(-pv, psi)
                  rsdu = rsmi_ub(vr_n) - pr_act(vr_n)
c     Surely a mistake!! Changed 12/02/98
c                  if (rsdu .gt. aa_1*pv) then
c     Surely a mistake!! Changed 12/02/98
                  if (rsdu .ge. aa_1*pv) then
                     n_cdd_ix = n_cdd_ix + 1
                     cdd_ix(n_cdd_ix) = ix_n
                     if (iand(vr_st, lb_bt) .ne. 0) then
                        rsdu = (rsmi_lb(vr_n) - pr_act(vr_n)) -
     &                       tl_pr_ifs
                        if (rsdu .gt. aa_1*pv) then
                           aa_1 = rsdu/pv
                           ix_o_vr_t_lv_bs = ix_n
                        end if
                     end if
                  end if
               else
                  goto 610
               end if
            end if
         else
            if (pv .gt. zero) then
               if (iand(vr_st, ub_bt) .ne. 0) then
                  rsdu = rsmi_ub(vr_n) - pr_act(vr_n)
c     Surely a mistake!! Changed 12/02/98
c                  if (rsdu .lt. aa_1*pv) then
c     Surely a mistake!! Changed 12/02/98
                  if (rsdu .le. aa_1*pv) then
                     n_cdd_ix = n_cdd_ix + 1
                     cdd_ix(n_cdd_ix) = ix_n
                     rsdu = rsdu + tl_pr_ifs
                     if (rsdu .lt. aa_1*pv) then
                        aa_1 = rsdu/pv
                        ix_o_vr_t_lv_bs = ix_n
                     end if
                  end if
               else
                  goto 610
               end if
            else
               if (iand(vr_st, lb_bt) .ne. 0) then
                  rsdu = rsmi_lb(vr_n) - pr_act(vr_n)
c     Surely a mistake!! Changed 12/02/98
c                  if (rsdu .gt. aa_1*pv) then
c     Surely a mistake!! Changed 12/02/98
                  if (rsdu .ge. aa_1*pv) then
                     n_cdd_ix = n_cdd_ix + 1
                     cdd_ix(n_cdd_ix) = ix_n
                     rsdu = rsdu - tl_pr_ifs
                     if (rsdu .gt. aa_1*pv) then
                        aa_1 = rsdu/pv
                        ix_o_vr_t_lv_bs = ix_n
                     end if
                  else
                     goto 610
                  end if
               else
                  goto 610
               end if
            end if
         end if
 610  continue
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_cz_r_lvl1)
CM      IF (sps_cz_r .EQ. 1) THEN
C?     &     call ems_tt_rec(-cz_r_ph_1_sps_ps_1_tt, n_bs)
CM      ELSE
C?     &     call ems_tt_rec(-cz_r_ph_1_dse_ps_1_tt, n_bs)
CM      ENDIF
CM      ENDIF
      nw_eta_l_ix = ix_n
      ok_pv = mx_pv_c_v/tl_cz_r_growth
      if (psi .eq. zero .and. ix_o_vr_t_lv_bs .lt. 0) then
c
c     If there are no infeasible variables which reach a bound
c     (indicated by psi remaining zero) and no variables which become
c     infeasible then the problem is unbounded.
c
         loop_n = 610
         goto 8040
      end if
      if (ix_o_vr_t_lv_bs .lt. 0) then
         vr_t_lv_bs = 0
      else
         vr_t_lv_bs = vr_in_r(nw_eta_ix(ix_o_vr_t_lv_bs))
         pv = nw_eta_v(ix_o_vr_t_lv_bs)
      end if
c
c     If no variable can become infeasible goto pass two.
c
      if (ix_o_vr_t_lv_bs .lt. 0) goto 700
      if (aa_1 .lt. zero) then
         call ems_consider_rpt_rao_ts(
     &        rpt, n_rpt, pv, vr_t_lv_bs, aa_1,
     &        st, rsmi_lb, rsmi_ub, pr_act, ds, is)
         if (rpt .eq. -5) then
            goto 8050
         else if (rpt .eq. -3) then
            goto 8030
         else
            goto 600
         end if
      end if
      if (aa_1 .lt. xp_tau/abs(pv)) goto 8060
      if (psi .eq. zero .and. ix_o_vr_t_lv_bs .eq. 0) then
c
c     If there are no infeasible variables which reach a bound
c     (indicated by psi remaining zero) and the first variable to become
c     infeasible is the entering variable then the ratio test yields a
c     bound swap. The step alpha_1 swaps the activity to its expanded
c     bound so set alpha to the step to the original bound. NB It is
c     guaranteed that alpha<alpha_1.
c
         vr_t_lv_bs = vr_in_r(0)
         aa = inf
         if (mv_dir .gt. 0) then
            aa = rsmi_ub(vr_t_lv_bs) - pr_act(vr_t_lv_bs)
         else
            aa = pr_act(vr_t_lv_bs) - rsmi_lb(vr_t_lv_bs)
         end if
         if (aa .lt. xp_tau) goto 8070
         n_cdd_ix = 0
c         call ems_rp_cz_r(lp_ph, n_r+1, 0, aa_1, aa, -inf, rl_null)
c         call ems_rp_cz_r(lp_ph, n_r+2,
c     &   0, aa, mx_pv_c_v, rl_null, rl_null)
         goto 1000
      end if
c
c     End of first pass for phase I
c=======================================================================
c     Start of second pass for phase I.
c
 700  continue
      if (aa_1 .ge. inf) aa_1 = aa_1*0.99d0
      psi = xp_nu*psi
      mx_pv = zero
      aa_fs = inf
      aa_ifs = -inf
      ix_o_f_fs_vr_t_bd = -1
      ix_o_l_ifs_vr_t_bd = -1
      rao_fs = inf
      rao_ifs = inf
      n_cdd_ix0 = n_cdd_ix
      n_cdd_ix = 0
      cdd_ix_n_o_l_ifs_vr_t_bd = 0
      cdd_ix_n_o_f_fs_vr_t_bd = 0
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_cz_r_lvl1) call ems_tt_rec(cz_r_ph_1_ps_2_tt, n_bs)
CM      ENDIF
      do 710, cdd_ix_n = 1, n_cdd_ix0
         ix_n = cdd_ix(cdd_ix_n)
         pv = nw_eta_v(ix_n)
c     NO_SGN pv = mv_dir*pv
         pv = mv_dir*pv
         vr_n = vr_in_r(nw_eta_ix(ix_n))
         vr_st = st(vr_n)
         if (iand(vr_st, ifs_bt) .ne. 0) then
            if (pv .gt. zero) then
               rao_ifs = (rsmi_lb(vr_n)-pr_act(vr_n))/pv
               if (iand(vr_st, ub_bt) .ne. 0)
     &              rao_fs = (rsmi_ub(vr_n)-pr_act(vr_n))/pv
            else
               rao_ifs = (rsmi_ub(vr_n)-pr_act(vr_n))/pv
               if (iand(vr_st, lb_bt) .ne. 0)
     &              rao_fs = (rsmi_lb(vr_n)-pr_act(vr_n))/pv
            end if
            if (rao_ifs .le. aa_1) then
c
c     This infeasible variable may become feasible even if it does not
c     become nonbasic. Need to know to check this and change its basic
c     cost as a result.
c
               n_cdd_ix = n_cdd_ix + 1
               cdd_ix(n_cdd_ix) = ix_n
c
c     Note that for an infeasible variable rao_fs .le. aa_1 only if
c     rao_fs .le. aa_1.
c
               if (rao_ifs .gt. aa_ifs .and. abs(pv) .ge. psi) then
                  aa_ifs = rao_ifs
                  ix_o_l_ifs_vr_t_bd = ix_n
c
c     Note where the current chosen row is in the candidate list.
c
                  cdd_ix_n_o_l_ifs_vr_t_bd = n_cdd_ix
               end if
               if (rao_fs .le. aa_1) then
                  if (abs(pv) .gt. mx_pv) then
                     aa_fs = rao_fs
                     ix_o_f_fs_vr_t_bd = ix_n
c
c     Note where the current chosen row is in the candidate list.
c
                     cdd_ix_n_o_f_fs_vr_t_bd = n_cdd_ix
                     mx_pv = abs(pv)
                  end if
                  rao_fs = inf
               end if
            end if
         else
            if (pv .gt. zero) then
               rao_fs = (rsmi_ub(vr_n)-pr_act(vr_n))/pv
            else
               rao_fs = (rsmi_lb(vr_n)-pr_act(vr_n))/pv
            end if
            if (rao_fs .le. aa_1) then
               if (abs(pv) .gt. mx_pv) then
                  aa_fs = rao_fs
                  ix_o_f_fs_vr_t_bd = ix_n
                  mx_pv = abs(pv)
               end if
               rao_fs = inf
            end if
         end if
 710  continue
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_cz_r_lvl1) call ems_tt_rec(-cz_r_ph_1_ps_2_tt, n_bs)
CM      ENDIF
      if (ix_o_f_fs_vr_t_bd .lt. 0) then
c
c     If no variable can exceed its opposite bound then mx_pv will be
c     zero so set it to one so that the minimium EXPAND step is
c     well-defined.
c
         mx_pv = one
         if (ix_o_l_ifs_vr_t_bd .lt. 0) then
            loop_n = 710
            goto 8040
         end if
      end if
      n_ifs_cdd_r = n_cdd_ix
      if (ix_o_l_ifs_vr_t_bd .lt. 0) then
c
c     No infeasible variables become feasible for a step less than aa_1.
c     Simply choose the feasible variable which reaches a bound and has
c     the best pivot.
c
         ix_o_vr_t_lv_bs = ix_o_f_fs_vr_t_bd
         aa = aa_fs
      else
         if (ix_o_f_fs_vr_t_bd .lt. 0) then
c
c     No feasible variable reaches a bound. Simply choose the infeasible
c     variable which becomes feasible with biggest step and has an OK
c     pivot.
c
            ix_o_vr_t_lv_bs = ix_o_l_ifs_vr_t_bd
            aa = aa_ifs
         else if (ix_o_f_fs_vr_t_bd .eq. ix_o_l_ifs_vr_t_bd) then
c
c     The same pivot is chosen both as the last infeasible variable to
c     reach a bound with an OK pivot and as the the feasible variable
c     which reaches a bound and has the best pivot.
c
            ix_o_vr_t_lv_bs = ix_o_f_fs_vr_t_bd
            aa = aa_fs
         else if (n_ifs_cdd_r .eq. n_ifs_r) then
c
c     All the infeasible variables become feasible for a step less than
c     aa_1---before a feasible variable reaches its expanded bound.
c     Unless the feasible variable which reaches a bound and has the
c     best pivot has a pivot which is significantly better, choose the
c     infeasible variable which becomes feasible with biggest step and
c     has an OK pivot.
c
            if (abs(nw_eta_v(ix_o_l_ifs_vr_t_bd)) .gt.
     &           xp_nu*abs(nw_eta_v(ix_o_f_fs_vr_t_bd))) then
               ix_o_vr_t_lv_bs = ix_o_l_ifs_vr_t_bd
               aa = aa_ifs
            else
               ix_o_vr_t_lv_bs = ix_o_f_fs_vr_t_bd
               aa = aa_fs
            end if
         else
c
c     Not all infeasible variables become feasible for a step less than
c     aa_1. Unless the infeasible variable which reaches a bound and has
c     an OK pivot has a pivot which is significantly better, choose the
c     feasible variable which reaches a bound and has the best pivot.
c
            if (xp_nu*abs(nw_eta_v(ix_o_l_ifs_vr_t_bd)) .gt.
     &           abs(nw_eta_v(ix_o_f_fs_vr_t_bd))) then
               ix_o_vr_t_lv_bs = ix_o_l_ifs_vr_t_bd
               aa = aa_ifs
            else
               ix_o_vr_t_lv_bs = ix_o_f_fs_vr_t_bd
               aa = aa_fs
            end if
         end if
      end if
      pv = nw_eta_v(ix_o_vr_t_lv_bs)
      if (ix_o_vr_t_lv_bs .gt. 0 .and. abs(pv) .lt. ok_pv) then
c
c     Growth will occur if this pivot is used: possibly refine the
c     pivotal column.
c
         if (iand(cz_r_msk, cz_r_refine_bt) .ne. 0 .and.
     &        .not. refined_pv_c) then
c
c     Set up the values required to monitor growth.
c
            pv_r_n = nw_eta_ix(ix_o_vr_t_lv_bs)
            vr_n = vr_in_r(pv_r_n)
            growth = abs(mx_pv_c_v/pv)
CM      IF (emsol_dev .EQ. 1) THEN
C?            call ems_mo_rsmi_growth(n_si_it, growth_act_refine_pv_c,
C?     &           pv_r_n, vr_n, pv, aa, growth, zero, 0)
CM      ENDIF
            refine_pv_c = .true.
            rp_growth = .true.
            goto 7000
         end if
      end if
c      call ems_rp_cz_r(lp_ph, n_r+1, 0, aa_1, aa_fs, aa_ifs, rl_null)
c      call ems_rp_cz_r(lp_ph, n_r+2,
c     &   0, aa, mx_pv_c_v, rl_null, rl_null)
c
c     End of second pass for phase I.
c=======================================================================
c
c     End of phase I cz_r
c=======================================================================
 1000 continue
      if (abs(pv) .lt. cz_r_pv_tl) then
         call ems_ck_cz_r_pv(
     &        pv_c_v, nw_eta_v, nw_eta_ix,
     &        nw_eta_f_ix, nw_eta_l_ix,
     &        ix_o_vr_t_lv_bs,
     &        prev_n_ix, rpt_cz_r, alg_er)
         if (alg_er) goto 7000
         if (rpt_cz_r) goto 100
      endif
CM      IF (dvx .EQ. 1) THEN
C?      if (ed_wt_o_vr_t_en_bs .gt. one) then
C?         ed_wt_o_vr_t_en_bs = sqrt(ed_wt_o_vr_t_en_bs)
C?      else
C?         ed_wt_o_vr_t_en_bs = one
C?      endif
C?      dvx_rao = max(
C?     &     ed_wt_o_vr_t_en_bs/og_ed_wt_o_vr_t_en_bs,
C?     &     og_ed_wt_o_vr_t_en_bs/ed_wt_o_vr_t_en_bs)
C?      i_te = n_r/nw_dvx_fwk_fq
C?      i_te = max(mn_n_dvx_it, i_te)
C?      nw_dvx_fwk = dvx_rao .gt. tl_dvx_wt .or. n_dvx_it .gt. i_te
CM      IF (emsol_dev .EQ. 1) THEN
C?      if (nw_dvx_fwk) call ems_mo_rsmi_nw_dvx_fwk(n_si_it,
C?     &     n_dvx_it, i_te, dvx_rao, tl_dvx_wt)
CM      ENDIF
C?      ed_wt(vr_t_en_bs) = ed_wt_o_vr_t_en_bs
CM      ENDIF
CM      IF (sed .EQ. 1) THEN
      ed_wt_o_vr_t_en_bs = ed_wt_o_vr_t_en_bs*half
      ed_wt_er = abs((
     &     og_ed_wt_o_vr_t_en_bs-ed_wt_o_vr_t_en_bs)/
     &     og_ed_wt_o_vr_t_en_bs)
      if (ed_wt_er .gt. mx_ed_wt_er) then
         mx_ed_wt_er = ed_wt_er
         if (iand(rsmi_msg_msk, rsmi_er_li_bt) .ne. 0) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9500)
     &           n_si_it, 'Stpst edge wt:',
     &           ' U_STPST_ED_WT', og_ed_wt_o_vr_t_en_bs,
     &           '         FTRAN', ed_wt_o_vr_t_en_bs, ed_wt_er
            call ems_msg_wr_li(warn_msg_n)
         end if
      end if
      ed_wt(vr_t_en_bs) = ed_wt_o_vr_t_en_bs
CM      ENDIF
      vr_t_lv_bs = vr_in_r(nw_eta_ix(ix_o_vr_t_lv_bs))
CM      IF (emsol_dev .EQ. 1) THEN
C?      if (ts_parsmi .gt. 0) then
C?         if (vr_t_lv_bs .ne. rd_lv_vr_n) then
C?            do 9991, ix_n = nw_eta_f_ix, nw_eta_l_ix
C?               if (vr_in_r(nw_eta_ix(ix_n)) .eq. rd_lv_vr_n) then
C?                  vr_t_lv_bs = rd_lv_vr_n
C?                  ix_o_vr_t_lv_bs = ix_n
C?                  pv = nw_eta_v(ix_n)
C?c     NO_SGN pv = mv_dir*pv
C?                  pv = mv_dir*pv
C?                  if (pv .gt. zero) then
C?                     aa = (rsmi_ub(vr_n) - pr_act(vr_n))/pv
C?                  else
C?                     aa = (rsmi_lb(vr_n) - pr_act(vr_n))/pv
C?                  endif
C?                  goto 9992
C?               endif
C? 9991       continue
C?            print*, 'Cannot find variable ', rd_lv_vr_n
C?            stop
C? 9992       continue
C?         endif
C?      endif
CM      ENDIF
      aa = max(aa, xp_tau/mx_pv)
      if (aa .ge. inf) goto 8010
      if (aa .lt. zero) goto 8020
      if (ix_o_vr_t_lv_bs .gt. 0 .and.
     &     abs(pv) .lt. ok_pv*1d1 .and. abs(pv) .ge. ok_pv) then
c
c     Near-growth has occurred so maybe monitor it.
c
c     Set up the values required to monitor growth.
c
         pv_r_n = nw_eta_ix(ix_o_vr_t_lv_bs)
         vr_n = vr_in_r(pv_r_n)
         growth = abs(mx_pv_c_v/pv)
CM      IF (emsol_dev .EQ. 1) THEN
C?         call ems_mo_rsmi_growth(n_si_it, growth_act_nr_growth,
C?     &        pv_r_n, vr_n, pv, aa, growth, zero, 0)
CM      ENDIF
      else if (ix_o_vr_t_lv_bs .gt. 0 .and. abs(pv) .lt. ok_pv) then
c
c     Potential growth has been detected and the pivotal column has
c     already been refined if this option has been selected.
c
c     Set up the values required to monitor growth.
c
         pv_r_n = nw_eta_ix(ix_o_vr_t_lv_bs)
         vr_n = vr_in_r(pv_r_n)
         growth = abs(mx_pv_c_v/pv)
         growth_mode = iand(cz_r_msk, cz_r_growth_mode)
         if (growth_mode .eq. cz_r_growth_inv .and. n_u .gt. 0) then
c
c     Rather than accept the growth, INVERT the basis and re-calculate
c     the pivotal column.
c
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9410)n_si_it,
     &           pv, vr_t_lv_bs, abs(mx_pv_c_v/pv)
            call ems_msg_wr_li(warn_msg_n)
            rq_inv = rq_inv_u_growth
            ml_da_st_msk =
     &           ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_inv)
            rp_growth = .true.
CM      IF (emsol_dev .EQ. 1) THEN
C?            call ems_mo_rsmi_growth(n_si_it, growth_act_re_inv,
C?     &           pv_r_n, vr_n, pv, aa, growth, zero, 0)
CM      ENDIF
            goto 7000
         else if (growth_mode .eq. cz_r_growth_cg_tl) then
            og_tl_pr_ifs = tl_pr_ifs
CM      IF (emsol_dev .EQ. 1) THEN
C?            call ems_mo_rsmi_growth(n_si_it, growth_act_cg_tl,
C?     &           pv_r_n, vr_n, pv, aa, growth, tl_pr_ifs, 0)
CM      ENDIF
            call ems_cz_r_cg_tl(ix_o_vr_t_lv_bs,
     &           rsmi_lb, rsmi_ub, pr_act, st, vr_in_r,
     &           nw_eta_v, nw_eta_ix, cdd_ix)
            if (alg_er) goto 8080
            if (lp_ph .eq. 1) then
c
c     The candidate list is only relevant in phase 1.
c
c     Extract the new number of indices in the candidate list.
c
               n_cdd_ix = cdd_ix(0)
c
c     Indicate that the candidate list contains row numbers.
c
               r_n_in_cdd_ls = .true.
c
c     The variables for the row indices in the candidate list cannot be
c     assumed to be not feasible.
c
               al_cdd_ifs = .false.
c
c     If the tolerance has increased then, even if aa=0, it is necessary
c     to check all the candidates in the list.
c
               if (tl_pr_ifs .gt. og_tl_pr_ifs) ck_cdd_ls = al_cdd_ifs
            endif
c
c     Set up the values required to monitor growth.
c
            pv_r_n = nw_eta_ix(ix_o_vr_t_lv_bs)
            vr_n = vr_in_r(pv_r_n)
            growth = abs(mx_pv_c_v/pv)
CM      IF (emsol_dev .EQ. 1) THEN
C?            call ems_mo_rsmi_growth(n_si_it, growth_act_rp,
C?     &           pv_r_n, vr_n, pv, aa, growth, tl_pr_ifs, 0)
CM      ENDIF
         else if (growth_mode .eq. cz_r_growth_mv_bd) then
CM      IF (emsol_dev .EQ. 1) THEN
C?            call ems_mo_rsmi_growth(n_si_it, growth_act_mv_bd,
C?     &           pv_r_n, vr_n, pv, aa, growth, zero, 0)
CM      ENDIF
            call ems_cz_r_mv_bd(ix_o_vr_t_lv_bs,
     &           rsmi_lb, rsmi_ub, pr_act, st, vr_in_r,
     &           nw_eta_v, nw_eta_ix, cdd_ix,
     &           n_mv_bd, mx_dl_bd)
            if (alg_er) goto 8080
            if (lp_ph .eq. 1) then
c
c     The candidate list is only relevant in phase 1.
c
c     Extract the new number of indices in the candidate list.
c
               n_cdd_ix = cdd_ix(0)
c
c     Indicate that the candidate list contains row numbers.
c
               r_n_in_cdd_ls = .true.
c
c     The variables for the row indices in the candidate list cannot be
c     assumed to be not feasible.
c
               al_cdd_ifs = .false.
c
c     If the tolerance has increased then, even if aa=0, it is necessary
c     to check all the candidates in the list.
c
               if (tl_pr_ifs .gt. og_tl_pr_ifs) ck_cdd_ls = al_cdd_ifs
            endif
c
c     Set up the values required to monitor growth.
c
            pv_r_n = nw_eta_ix(ix_o_vr_t_lv_bs)
            vr_n = vr_in_r(pv_r_n)
            growth = abs(mx_pv_c_v/pv)
CM      IF (emsol_dev .EQ. 1) THEN
C?            call ems_mo_rsmi_growth(n_si_it, growth_act_rp,
C?     &           pv_r_n, vr_n, pv, aa, growth, mx_dl_bd, n_mv_bd)
CM      ENDIF
            r_n_in_cdd_ls = .true.
         else if (growth_mode .eq. cz_r_growth_cg_act) then
CM      IF (emsol_dev .EQ. 1) THEN
C?            call ems_mo_rsmi_growth(n_si_it, growth_act_cg_act,
C?     &           pv_r_n, vr_n, pv, aa, growth, zero, 0)
CM      ENDIF
            call ems_cz_r_cg_act(ix_o_vr_t_lv_bs,
     &           rsmi_lb, rsmi_ub, pr_act, st, vr_in_r,
     &           nw_eta_v, nw_eta_ix, cdd_ix,
     &           n_cg_act, mx_dl_act)
            if (alg_er) goto 8080
            if (lp_ph .eq. 1) then
c
c     The candidate list is only relevant in phase 1.
c
c     Extract the new number of indices in the candidate list.
c
               n_cdd_ix = cdd_ix(0)
c
c     Indicate that the candidate list contains row numbers.
c
               r_n_in_cdd_ls = .true.
c
c     The variables for the row indices in the candidate list cannot be
c     assumed to be not feasible.
c
               al_cdd_ifs = .false.
c
c     If the tolerance has increased then, even if aa=0, it is necessary
c     to check all the candidates in the list.
c
               if (tl_pr_ifs .gt. og_tl_pr_ifs) ck_cdd_ls = al_cdd_ifs
            endif
c
c     Set up the values required to monitor growth.
c
            pv_r_n = nw_eta_ix(ix_o_vr_t_lv_bs)
            vr_n = vr_in_r(pv_r_n)
            growth = abs(mx_pv_c_v/pv)
CM      IF (emsol_dev .EQ. 1) THEN
C?            call ems_mo_rsmi_growth(n_si_it, growth_act_rp,
C?     &           pv_r_n, vr_n, pv, aa, growth, mx_dl_act, n_cg_act)
CM      ENDIF
            r_n_in_cdd_ls = .true.
         else
c
c     No further action can be taken to prevent growth!
c
CM      IF (emsol_dev .EQ. 1) THEN
C?            call ems_mo_rsmi_growth(n_si_it, growth_act_no,
C?     &           pv_r_n, vr_n, pv, aa, growth, zero, 0)
CM      ENDIF
c            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9420)n_si_it,
c     &           pv, vr_t_lv_bs, abs(mx_pv_c_v/pv)
c            call ems_msg_wr_li(warn_msg_n)
            rp_growth = .false.
         end if
      else if (rp_growth) then
c
c     Set up the values required to monitor growth.
c
         pv_r_n = nw_eta_ix(ix_o_vr_t_lv_bs)
         vr_n = vr_in_r(pv_r_n)
         if (ix_o_vr_t_lv_bs .gt. 0) then
            growth = abs(mx_pv_c_v/pv)
         else
            growth = one
         endif
CM      IF (emsol_dev .EQ. 1) THEN
C?         call ems_mo_rsmi_growth(n_si_it, growth_act_rp,
C?     &        pv_r_n, vr_n, pv, aa, growth, zero, 0)
CM      ENDIF
         rp_growth = .false.
      end if
c
c     Indicate the status of the list of infeasible basic variables
c     (other than the leaving variable) which may become feasible.
c
      if (lp_ph .eq. 2 .or. (.not. ck_cdd_ls .and. aa .eq. zero)) then
c         cdd_ix(0) = n_r+1
         n_cdd_ix = 0
      else if (.not. r_n_in_cdd_ls) then
         if (cdd_ix_n_o_l_ifs_vr_t_bd .gt. 0) then
c
c     The leaving variable may be on the list of candidates.
c
            if (cdd_ix(cdd_ix_n_o_l_ifs_vr_t_bd) .eq.
     &           ix_o_vr_t_lv_bs) then
c
c     The leaving variable is still on the list of candidates so remove
c     it. If it becomes feasible it will be treated separately.
c
               cdd_ix(cdd_ix_n_o_l_ifs_vr_t_bd) = cdd_ix(n_cdd_ix)
               n_cdd_ix = n_cdd_ix - 1
            end if
         end if
         if (cdd_ix_n_o_f_fs_vr_t_bd .gt. 0 .and.
     &        cdd_ix_n_o_f_fs_vr_t_bd .le. n_cdd_ix) then
c
c     The leaving variable may be on the list of candidates.
c
            if (cdd_ix(cdd_ix_n_o_f_fs_vr_t_bd) .eq.
     &           ix_o_vr_t_lv_bs) then
c
c     The leaving variable is still on the list of candidates so remove
c     it. If it becomes feasible it will be treated separately.
c
               cdd_ix(cdd_ix_n_o_f_fs_vr_t_bd) = cdd_ix(n_cdd_ix)
               n_cdd_ix = n_cdd_ix - 1
            end if
         end if
         if (n_cdd_ix .gt. 0) then
            if (cdd_ix(1) .eq. 0) then
c
c     The incoming variable is an infeasible candidate so replace it in
c     the list by the candidate at the end of the list and reduce the
c     length of the list by 1.
c
               vr_n = vr_in_r(nw_eta_ix(0))
               call ems_ca_rp_1_vr_st(1, vr_n, ds, is)
               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9300)n_si_it
               call ems_msg_wr_li(info_msg_n)
               cdd_ix(1) = cdd_ix(n_cdd_ix)
               n_cdd_ix = n_cdd_ix - 1
            end if
c
c     Apply the indirection so that cdd_ix contains row numbers rather
c     than indices of nw_eta_ix.
c
            do 1100, cdd_ix_n = 1, n_cdd_ix
               r_n = nw_eta_ix(cdd_ix(cdd_ix_n))
               cdd_ix(cdd_ix_n) = r_n
 1100       continue
         else
            n_cdd_ix = 0
         end if
      end if
      if (al_cdd_ifs) then
         cdd_ix(0) = -n_cdd_ix
      else
         cdd_ix(0) = n_cdd_ix
      endif
      if (ix_o_vr_t_lv_bs .ne. 0) then
         pv = nw_eta_v(ix_o_vr_t_lv_bs)
         pv_r_n = nw_eta_ix(ix_o_vr_t_lv_bs)
c
c     Put the pivot first in the packed vector. Set nw_eta_f_ix = 0 so
c     the pivotal value is picked up in loops from nw_eta_f_ix to
c     nw_eta_l_ix.
c
         nw_eta_v(0) = pv
         nw_eta_ix(0) = pv_r_n
         nw_eta_f_ix = 0
         if (nw_eta_l_ix .le. ord_i_ls_stp_p1) then
c
c     For short etas, place the last index in the position previously
c     occupied by the index of the pivot.
c
            nw_eta_ix(ix_o_vr_t_lv_bs) = nw_eta_ix(nw_eta_l_ix)
            nw_eta_v(ix_o_vr_t_lv_bs) = nw_eta_v(nw_eta_l_ix)
c
c     Decrease the pointer to the last index.
c
            nw_eta_l_ix = nw_eta_l_ix - 1
         else
c
c     For longer etas, keep the row indices in increasing order at the
c     cost of having the index of the pivot appearing twice. Zero the
c     corresponding value so that it doesn't get added in twice. NB When
c     the new eta indices are used to scatter the new eta values, the
c     loops must be reversed to avoid scattering this zero onto the
c     pivot value scattered by nw_eta_ix(0).
c
            nw_eta_v(ix_o_vr_t_lv_bs) = zero
         end if
c
c     Determine the variable to leave the basis.
c
         vr_t_lv_bs = vr_in_r(pv_r_n)
      else
c
c     For a bound swap, define the pivot value and the pivotal row
c     number.
c
         pv = one
         pv_r_n = 0
c
c     Define the variable to leave the basis to be the variable to enter
c     the basis. This is how a bound swap will be recognised.
c
         vr_t_lv_bs = vr_t_en_bs
      end if
c
c     Give alpha a sign corresponding to the direction in which the
c     entering variable is moving.
c
      aa = mv_dir*aa
 7000 continue
CM      IF (dan .EQ. 1) THEN
C?      if (iand(ck_msk, ze_a_ck_bt) .ne. 0)
C?     &     call ems_ck_ze_rl_a(n_r, pv_c_v)
CM      ENDIF
CM      IF (dvx .EQ. 1) THEN
C?      if (iand(ck_msk, ze_a_ck_bt) .ne. 0)
C?     &     call ems_ck_ze_rl_a(n_r, pv_c_v)
CM      ENDIF
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_cz_r_lvl0) call ems_tt_rec(-cz_r_tt, n_bs)
CM      ENDIF
      return
 8010 continue
      un_bd = .true.
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810)n_si_it, aa
      call ems_msg_wr_li(warn_msg_n)
      goto 7000
 8020 continue
      alg_er = .true.
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9820)n_si_it, aa
      call ems_msg_wr_li(warn_msg_n)
      goto 7000
 8030 continue
      alg_er = .true.
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9830)
     &     n_si_it, abs(rsdu)
      call ems_msg_wr_li(warn_msg_n)
      goto 7000
 8040 continue
      un_bd = .true.
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9840)n_si_it, loop_n
      call ems_msg_wr_li(warn_msg_n)
      goto 7000
 8050 continue
      alg_er = .true.
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9850)n_si_it
      call ems_msg_wr_li(warn_msg_n)
      goto 7000
 8060 continue
      alg_er = .true.
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9860)
     &     n_si_it, aa_1, xp_tau/abs(pv)
      call ems_msg_wr_li(warn_msg_n)
      goto 7000
 8070 continue
      alg_er = .true.
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9870)
     &     n_si_it, aa, xp_tau
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 8080 continue
      alg_er = .true.
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9880)n_si_it
      call ems_msg_wr_li(warn_msg_n)
      goto 7000
 9300 format('Iteration ', i7,
     &     ': Incoming variable is still a candidate')
 9410 format('Iteration ', i7,
     &     ':         Pivot ',  g11.4, ' for variable ', i7,
     &     ' would give growth of ', g11.4, ' so INVERT')
c 9420 format('Iteration ', i7,
c     &     ':         Pivot ',  g11.4, ' for variable ', i7,
c     &     '  will give growth of ', g11.4, ' which is unavoidable')
CM      IF (sed .EQ. 1) THEN
 9500 format('Iteration ', i7, 1x, a14,
     &     2(' From ', a14, ' = ', g11.4), ' |Rel. error| = ', g11.4)
CM      ENDIF
 9810 format('Iteration ', i7,
     &     ': Step length of ', g11.4, ' suggests unboundedness ')
 9820 format('Iteration ', i7,
     &     ': Step length of ', g11.4, ' suggests algorithmic error')
 9830 format('Iteration ', i7,
     &     ': |Residual| of ', g11.4,
     &     ' gives negative ratio with expanded bounds')
 9840 format('Iteration ', i7,
     &     ': Unboundedness detected in cz_r loop ', i3)
 9850 format('Iteration ', i7,
     &     ': Cannot repeat a ratio test more than 10 times')
 9860 format('Iteration ', i7,
     &     ': Ratio test with expanded bounds gives step ', g11.4,
     &     ' less than the minimum step ', g11.4)
 9870 format('Iteration ', i7,
     &     ': Bound swap of ', g11.4,
     &     ' is less than the minimum step ', g11.4)
 9880 format('Iteration ', i7,
     &     ': Error passed down from the growth-handling routine')
      end
CM      IF (dan .EQ. 1) THEN
C?C->>> -------------------------------------------> ems_dan_l1_cz_r <<<
C?c
CM      IF (sps_cz_r .EQ. 1) THEN
C?      subroutine ems_dan_sps_l1_cz_r(
CM      ELSE
C?      subroutine ems_dan_l1_cz_r(
CM      ENDIF
C?     &     rp_growth, refined_pv_c, refine_pv_c,
CM      ENDIF
CM      IF (dvx .EQ. 1) THEN
C?C->>> -------------------------------------------> ems_dvx_l1_cz_r <<<
C?c
CM      IF (sps_cz_r .EQ. 1) THEN
C?      subroutine ems_dvx_sps_l1_cz_r(
CM      ELSE
C?      subroutine ems_dvx_l1_cz_r(
CM      ENDIF
C?     &     rp_growth, refined_pv_c, refine_pv_c,
C?     &     ed_wt,
C?     &     dvx_ix,
CM      ENDIF
CM      IF (sed .EQ. 1) THEN
C->>> -------------------------------------------> ems_sed_l1_cz_r <<<
c
CM      IF (sps_cz_r .EQ. 1) THEN
C?      subroutine ems_sed_sps_l1_cz_r(
CM      ELSE
      subroutine ems_sed_l1_cz_r(
CM      ENDIF
     &     rp_growth, refined_pv_c, refine_pv_c,
     &     mx_ed_wt_er,
     &     ed_wt,
CM      ENDIF
     &     rsmi_lb, rsmi_ub, pr_act, st, vr_in_r,
     &     nw_eta_v, pv_c_v, nw_eta_ix, cdd_ix,
     &     l1_bp,
     &     l1_dl_gd,
     &     l1_ix,
     &     ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMICS.INC'
      include 'RSMICOM.INC'
      include 'MORSMI.INC'
CM      IF (emsol_da .EQ. 1) THEN
C?      include 'EMSDA.INC'
CM      ENDIF
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      include 'CHCTVR.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      logical rp_growth, refined_pv_c, refine_pv_c
      integer st(0:mx_n_c+n_r)
      integer vr_in_r(0:n_r), nw_eta_ix(0:n_r)
      integer cdd_ix(0:1+n_r), is(0:is_n_en_m1)
CM      IF (dvx .EQ. 1) THEN
C?      integer dvx_ix(0:mx_n_c+n_r)
C?      double precision ed_wt(0:mx_n_c+n_r)
CM      ENDIF
CM      IF (sed .EQ. 1) THEN
      double precision mx_ed_wt_er
      double precision ed_wt(0:mx_n_c+n_r)
CM      ENDIF
      double precision rsmi_lb(0:mx_n_c+n_r)
      double precision rsmi_ub(0:mx_n_c+n_r)
      double precision pr_act(0:mx_n_c+n_r)
      double precision nw_eta_v(0:n_r), pv_c_v(0:n_r)
      double precision ds(0:ds_n_en_m1)
c
c     Parameters and functions only required for l1-CHUZR
c
      integer l1_ix(0:l1_cz_r_mx_n_cdd)
      double precision l1_bp(0:l1_cz_r_mx_n_cdd)
      double precision l1_dl_gd(0:l1_cz_r_mx_n_cdd)
      integer ems_l1_gd_st
c
c     Local variables only required for l1-CHUZR
c
      double precision bp, pre_bp, dl_gd
      double precision pre_bp_dn_gd, pre_bp_up_gd
      double precision l1_dn_gd, l1_up_gd
      double precision v0, l1_ob0, l1_up_gd0
      integer pre_pre_bp_gd_st, pre_bp_gd_st
      integer f_cdd_at_pre_bp, l_cdd_at_pre_bp
      integer n_l1_cdd, l1_cdd_n
      integer mn_bp_l1_cdd_n
      integer lo_bp_l1_cdd_n
      integer up_bp_l1_cdd_n
      integer mx_bp_l1_cdd_n
      integer sv_aa_bp_ix
      integer aa_1_ix, aa_2_ix
      integer ix_o_vr_t_lv_bs
      integer cdd_n_o_vr_t_lv_bs
      logical rp_l1_cz_r_df
      logical rp_l1_cz_r
      logical no_al_l1_cz_r_cdd
c
      integer r_n, vr_n, vr_st
      integer ix_n, loop_n, n_rpt, rpt
      integer n_cdd_ix, cdd_ix_n, n_cdd_ix0
      integer growth_mode
CM      IF (sps_cz_r .EQ. 1) THEN
C?      integer og_ix_n, og_n_ix
CM      ENDIF
      logical rpt_cz_r
      integer prev_n_ix
CM      IF (dvx .EQ. 1) THEN
C?      integer i_te
C?      double precision dvx_rao
CM      ENDIF
CM      IF (sed .EQ. 1) THEN
      double precision ed_wt_er
CM      ENDIF
      double precision aa_fs, aa_bp, aa_1, aa_2, mx_pv
      double precision ok_pv, aa_1_pv, aa_2_pv, aa_pv
      double precision rsdu, aa_bp_up_gd, l1_cdd_up_gd
      double precision og_tl_pr_ifs
      double precision growth
CM      IF (sed .EQ. 1) THEN
      double precision og_ed_wt_o_vr_t_en_bs
CM      ENDIF
CM      IF (dvx .EQ. 1) THEN
C?      double precision og_ed_wt_o_vr_t_en_bs
CM      ENDIF
CM      IF (emsol_da .EQ. 1) THEN
C?c      integer i
C?c      double precision v
C?      integer rp_l1_cz_r_cn
C?      save rp_l1_cz_r_cn
C?      data rp_l1_cz_r_cn/1/
CM      ENDIF
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_cz_r_lvl0) call ems_tt_rec(cz_r_tt, n_bs)
CM      ENDIF
      prev_n_ix = i_inf
 100  continue
CM      IF (dvx .EQ. 1) THEN
C?      og_ed_wt_o_vr_t_en_bs = ed_wt(vr_t_en_bs)
C?      ed_wt_o_vr_t_en_bs = zero
CM      ENDIF
CM      IF (sed .EQ. 1) THEN
      og_ed_wt_o_vr_t_en_bs = ed_wt(vr_t_en_bs)
      ed_wt_o_vr_t_en_bs = zero
CM      ENDIF
      mx_pv_c_v = zero
c
c     Initialise these indices so that ems_rp_l1_cz_r can be used for
c     both phase I and phase II.
c
      aa_1_ix = -1
      aa_2_ix = -1
 
      alg_er = .false.
      un_bd = .false.
      refine_pv_c = .false.
      rp_l1_cz_r_df = .false.
c
c     Set ix_n = -1, vr_in_r(0) = vr_t_en_bs and pv_c_v(0) = 1 so that
c     this value is packed into nw_eta_v(0) with index nw_eta_ix(0) =
c     vr_t_en_bs. Thus, ratios for bound swaps can be determined in the
c     same loop as the other ratios.
c
      ix_n = -1
      vr_in_r(0) = vr_t_en_bs
c     NO_SGN pv_c_v(0) = one
c     SGN pv_c_v(0) = mv_dir*one
CM      IF (sps_cz_r .EQ. 1) THEN
C?      og_n_ix = nw_eta_ix(0)
C?      nw_eta_ix(0) = 0
CM      ENDIF
      pv_c_v(0) = one
      nw_eta_f_ix = 1
      if (lp_ph .eq. 1) go to 400
c=======================================================================
c     Start of phase II cz_r
c
c     Ratio test with expanded bounds.
c
c=======================================================================
c
      tl_pr_ifs = tl_pr_ifs + xp_tau
      n_rpt = 0
 205  continue
      n_cdd_ix = 0
      og_tl_pr_ifs = tl_pr_ifs
c
c     aa_fs is the minimum step to an expanded bound
c
c     aa_bp is the minimum step to an expanded bound/breakpoint
c
c     sv_aa_bp_ix is used to record the index of the variable
c     corresponding to aa_bp---just to test whether aa_bp is less than
c     the minimum step (only relevant when xp_tau>0) and report when
c     repeating the ratio test.
c
c     NB aa_bp <= aa_fs.
c
      aa_fs = inf
      aa_bp = inf
      sv_aa_bp_ix = -1
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_cz_r_lvl1)
CM      IF (sps_cz_r .EQ. 1) THEN
C?     &     call ems_tt_rec(cz_r_l1_ph_2_sps_ps_1_tt, n_bs)
CM      ELSE
C?     &     call ems_tt_rec(cz_r_l1_ph_2_dse_ps_1_tt, n_bs)
CM      ENDIF
CM      ENDIF
CM      IF (sps_cz_r .EQ. 1) THEN
C?      do 210, og_ix_n = 0, og_n_ix
C?         r_n = nw_eta_ix(og_ix_n)
CM      ELSE
      do 210, r_n = 0, n_r
CM      ENDIF
         pv = pv_c_v(r_n)
CM      IF (emsol_da .EQ. 1) THEN
C?c         if (pv .ne. zero) then
C?c            v = abs(pv)
C?c            if (v .le. 1d0) v = v*1d-1
C?c            i = log10(v)
C?c            if (i .lt. mn_pv_c_v_rec_by_1)
C?c     &           i = mn_pv_c_v_rec_by_1-1 - (mn_pv_c_v_rec_by_1-i)/10
C?c            i = max(min(i, mx_pv_c_v_rec_by_1), mn_pv_c_v_rec_by_10)
C?c            pv_c_v_rec(i) = pv_c_v_rec(i) + 1
C?c            su_n_pk_pv_c_en = su_n_pk_pv_c_en + 1
C?c            if (abs(pv) .le. pk_pv_c_ze)
C?c     &           su_n_pk_pv_c_ze = su_n_pk_pv_c_ze + 1
C?c         end if
CM      ENDIF
         if (pv .eq. zero) goto 210
CM      IF (dan .EQ. 1) THEN
C?         pv_c_v(r_n) = zero
CM      ENDIF
CM      IF (dvx .EQ. 1) THEN
C?         pv_c_v(r_n) = zero
CM      ENDIF
         if (abs(pv) .le. pk_pv_c_ze) then
            pv_c_v(r_n) = zero
            go to 210
         end if
         ix_n = ix_n + 1
         mx_pv_c_v = max(abs(pv), mx_pv_c_v)
         nw_eta_v(ix_n) = pv
         nw_eta_ix(ix_n) = r_n
         vr_n = vr_in_r(r_n)
CM      IF (dvx .EQ. 1) THEN
C?         if (dvx_ix(vr_n) .gt. 0)
C?     &        ed_wt_o_vr_t_en_bs = ed_wt_o_vr_t_en_bs + pv*pv
CM      ENDIF
CM      IF (sed .EQ. 1) THEN
         ed_wt_o_vr_t_en_bs = ed_wt_o_vr_t_en_bs + pv*pv
CM      ENDIF
c
c     Now complete the Phase II CHUZR pass 1 loop.
c
c     NO_SGN pv = mv_dir*pv
         pv = mv_dir*pv
         vr_st = st(vr_n)
         if (pv .gt. zero) then
            if (iand(vr_st, ub_bt) .ne. 0) then
c
c     The variable is moving up towards an upper bound or breakpoint.
c
               rsdu = rsmi_ub(vr_n) - pr_act(vr_n)
c     Surely a mistake!! Changed 12/02/98
c               if (rsdu .lt. aa_fs*pv) then
c     Surely a mistake!! Changed 12/02/98
               if (rsdu .le. aa_fs*pv) then
c
c     The standard ratio is less than the current smallest ratio with
c     respect to expanded bounds.
c
c     *    it may be a candidate in the pass 2 ratio test.
c
c     If it corresponds to a bound rather than just a breakpoint then
c
c     *    it may give a ratio with respect to the expanded bound which
c          is smaller than the current smallest;
c
c     NB It the variable is BP then there is no need to consider this.
c
                  n_cdd_ix = n_cdd_ix + 1
                  cdd_ix(n_cdd_ix) = ix_n
                  if (iand(vr_st, alt_bt) .eq. 0) then
c
c     The variable is standard:
c
                     rsdu = rsdu + tl_pr_ifs
                     if (rsdu .lt. aa_bp*pv) then
                        bp = rsdu/pv
                        aa_fs = bp
                        aa_bp = bp
                        sv_aa_bp_ix = ix_n
                     else if (rsdu .lt. aa_fs*pv) then
                        bp = rsdu/pv
                        aa_fs = bp
                     end if
                  else if (iand(vr_st, bp_bt) .ne. 0) then
c
c     The variable is BP:
c
                     rsdu = rsdu + tl_pr_ifs
                     if (rsdu .lt. aa_bp*pv) then
                        bp = rsdu/pv
                        aa_bp = bp
                        sv_aa_bp_ix = ix_n
                     end if
                  else
c
c     The variable is PWL:
c
                  end if
               end if
            end if
         else
            if (iand(vr_st, lb_bt) .ne. 0) then
c
c     The variable is moving towards a lower bound or breakpoint.
c
c     NB the residual rsmi_lb(vr_n)-pr_act(vr_n) and the pivot are both
c     negative so reverse the sign of the inequality.
c
               rsdu = rsmi_lb(vr_n) - pr_act(vr_n)
c     Surely a mistake!! Changed 12/02/98
c               if (rsdu .gt. aa_fs*pv) then
c     Surely a mistake!! Changed 12/02/98
               if (rsdu .ge. aa_fs*pv) then
c
c     The standard ratio is less than the current smallest ratio with
c     respect to expanded bounds.
c
c     *    it may be a candidate in the pass 2 ratio test.
c
c     If it corresponds to a bound rather than just a breakpoint then
c
c     *    it may give a ratio with respect to the expanded bound which
c          is smaller than the current smallest;
c
c     NB It the variable is BP then there is no need to consider this.
c
                  n_cdd_ix = n_cdd_ix + 1
                  cdd_ix(n_cdd_ix) = ix_n
                  if (iand(vr_st, alt_bt) .eq. 0) then
c
c     The variable is standard:
c
                     rsdu = rsdu - tl_pr_ifs
                     if (rsdu .gt. aa_bp*pv) then
                        bp = rsdu/pv
                        aa_fs = bp
                        aa_bp = bp
                        sv_aa_bp_ix = ix_n
                     else if (rsdu .gt. aa_fs*pv) then
                        bp = rsdu/pv
                        aa_fs = bp
                     end if
                  else if (iand(vr_st, bp_bt) .ne. 0) then
c
c     The variable is BP:
c
                     rsdu = rsdu - tl_pr_ifs
                     if (rsdu .gt. aa_bp*pv) then
                        bp = rsdu/pv
                        aa_bp = bp
                        sv_aa_bp_ix = ix_n
                     end if
                  else
c
c     The variable is PWL:
c
                  end if
               end if
            end if
         end if
 210  continue
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_cz_r_lvl1)
CM      IF (sps_cz_r .EQ. 1) THEN
C?     &     call ems_tt_rec(-cz_r_l1_ph_2_sps_ps_1_tt, n_bs)
CM      ELSE
C?     &     call ems_tt_rec(-cz_r_l1_ph_2_dse_ps_1_tt, n_bs)
CM      ENDIF
CM      ENDIF
      nw_eta_l_ix = ix_n
      ok_pv = mx_pv_c_v/tl_cz_r_growth
      if (aa_bp .lt. zero) then
         pv = nw_eta_v(sv_aa_bp_ix)
         vr_t_lv_bs = vr_in_r(nw_eta_ix(sv_aa_bp_ix))
         call ems_consider_rpt_rao_ts(
     &        rpt, n_rpt, pv, vr_t_lv_bs, aa_fs,
     &        st, rsmi_lb, rsmi_ub, pr_act, ds, is)
         if (rpt .eq. -5) then
            go to 8050
         else if (rpt .eq. -3) then
            go to 8030
         else
            go to 205
         end if
      else if (aa_bp .lt. inf) then
c
c     Alpha_BP should not be less than the minimum step due to primal
c     feasibility tolerance.
c
         pv = nw_eta_v(sv_aa_bp_ix)
         if (aa_bp .lt. xp_tau/abs(pv)) go to 8060
      else
         loop_n = 210
         go to 8040
      end if
c
c     Bound swaps cannot be detected after pass 1 of Phase II l1-CHUZR
c     since the l1-minimizer may be less than the step to the bound.
c
c     End of first pass for phase II.
c=======================================================================
c     Start of second pass for phase II.
c
      v0 = zero
      l1_ob0 = zero
      l1_up_gd0 = mv_dir*du_act_o_vr_t_en_bs
c
c     Find the steps:
c
c     aa_1 <= aa_bp with maximum pivot aa_1_pv and index aa_1_ix
c     aa_2 <= aa_fs with maximum pivot aa_2_pv and index aa_2_ix
c
c     Find aa_bp_up_gd, the L1 up gradient at aa_bp.
c
c     Maintain l1_cdd_up_gd, a lower bound on the L1 up gradient at
c     l1_bp(1). If l1_cdd_up_gd > tl then l1_bp(1) is an upper bound on
c     the step to the L1 minimizer.
c
      aa_1_pv = zero
      aa_2_pv = zero
      aa_1_ix = -1
      aa_2_ix = -1
      aa_bp_up_gd = l1_up_gd0
      l1_cdd_up_gd = l1_up_gd0
      l1_bp(1) = inf
      l1_ix(0) = 0
      rp_l1_cz_r = rp_l1_cz_r_df
      no_al_l1_cz_r_cdd = .false.
      mx_pv = zero
      cdd_n_o_vr_t_lv_bs = -1
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_cz_r_lvl1) call ems_tt_rec(cz_r_l1_ph_2_ps_2_tt, n_bs)
CM      ENDIF
      do 220, cdd_ix_n = 1, n_cdd_ix
c
c     NB All candidates have a pivot which is sufficiently large and a
c     finite value to move to which is given by the signed pivot.
c
         ix_n = cdd_ix(cdd_ix_n)
         pv = nw_eta_v(ix_n)
c     NO_SGN pv = mv_dir*pv
         pv = mv_dir*pv
         vr_n = vr_in_r(nw_eta_ix(ix_n))
         vr_st = st(vr_n)
         if (pv .gt. zero) then
            mx_pv = max(pv, mx_pv)
            rsdu = rsmi_ub(vr_n) - pr_act(vr_n)
            if (rsdu .le. aa_fs*pv) then
               if (iand(vr_st, alt_bt) .eq. 0) then
c
c     The variable is standard:
c
c     The ratio corresponds to a move up to an upper bound.
c
                  if (aa_2_ix .ne. 0 .and. pv .gt. aa_2_pv) then
                     aa_2_ix = ix_n
                     aa_2_pv = pv
                  end if
               else if (iand(vr_st, bp_bt) .ne. 0) then
c
c     The variable is BP:
c     The ratio corresponds to a move up to a break point.
c
                  bp = rsdu/pv
                  dl_gd = pv*pr_co_mu*rsmi_lb(vr_n)
                  if (bp .le. aa_bp) then
                     aa_bp_up_gd = aa_bp_up_gd + dl_gd
                     if (aa_1_ix .ne. 0 .and. pv .gt. aa_1_pv) then
                        aa_1_ix = ix_n
                        aa_1_pv = pv
                     end if
                  end if
c
c     Don't allow moves to a distinct breakpoint if the L1 gradient is
c     not downhill.
c                  if (l1_cdd_up_gd .le. tl_du_ifs .or.
                  if (l1_cdd_up_gd .lt. zero .or.
     &                 bp .le. l1_bp(1)) then
c
c     The gradient changes at breakpoints on the heap have not yet
c     minimized the objective or the breakpoint is less than or equal to
c     the greatest value on the heap so add the breakpoint to the heap
c     ---unless it is full (ERROR).
c
                     if (l1_ix(0) .ge. l1_cz_r_mx_n_cdd) go to 225
                     l1_cdd_up_gd = l1_cdd_up_gd  + dl_gd
                     call ems_add_to_incr_heap_w_aux_v(
     &                    .false., l1_cz_r_mx_n_cdd,
     &                    l1_bp, l1_dl_gd, l1_ix,
     &                    bp, dl_gd, ix_n)
                  end if
               else
c
c     The variable is PWL:
c
               end if
            end if
         else
c
c     The pivot is negative
c
            mx_pv = max(-pv, mx_pv)
            rsdu = rsmi_lb(vr_n) - pr_act(vr_n)
            if (rsdu .ge. aa_fs*pv) then
               if (iand(vr_st, alt_bt) .eq. 0) then
c
c     The variable is standard:
c
c     The ratio corresponds to a move down to a lower bound.
c
                  if (aa_2_ix .ne. 0 .and. -pv .gt. aa_2_pv) then
                     aa_2_ix = ix_n
                     aa_2_pv = -pv
                  end if
               else if (iand(vr_st, bp_bt) .ne. 0) then
c
c     The variable is BP:
c     The ratio corresponds to a move down to a break point.
c
                  bp = rsdu/pv
                  dl_gd = pv*pr_co_mu*rsmi_ub(vr_n)
                  if (bp .le. aa_bp) then
                     aa_bp_up_gd = aa_bp_up_gd + dl_gd
                     if (aa_1_ix .ne. 0 .and. -pv .gt. aa_1_pv) then
                        aa_1_ix = ix_n
                        aa_1_pv = -pv
                     end if
                  end if
c
c     Don't allow moves to a distinct breakpoint if the L1 gradient is
c     not downhill.
c                  if (l1_cdd_up_gd .le. tl_du_ifs .or.
                  if (l1_cdd_up_gd .lt. zero .or.
     &                 bp .le. l1_bp(1)) then
                     if (l1_ix(0) .ge. l1_cz_r_mx_n_cdd) go to 225
                     l1_cdd_up_gd = l1_cdd_up_gd  + dl_gd
                     call ems_add_to_incr_heap_w_aux_v(
     &                    .false., l1_cz_r_mx_n_cdd,
     &                    l1_bp, l1_dl_gd, l1_ix,
     &                    bp, dl_gd, ix_n)
                  end if
               else
c
c     The variable is PWL:
c
               end if
            end if
         end if
 220  continue
      goto 230
 225  continue
c
c     There was not sufficient room to store all candidate breakpoints:
c     Find the L1-minimizer within the stored candidates and use this
c     as an upper bound on acceptable candidates in a subsequent pass.
c
      alg_er = .true.
c      print*, ' l1_ix(0) = ', l1_ix(0),
c     &     ' l1_cz_r_mx_n_cdd ',l1_cz_r_mx_n_cdd
c      call ems_rp_ml_da_st_msk
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9400)n_si_it
      call ems_msg_wr_li(warn_msg_n)
      go to 7000
 230  continue
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_cz_r_lvl1) call ems_tt_rec(-cz_r_l1_ph_2_ps_2_tt, n_bs)
CM      ENDIF
      n_l1_cdd = l1_ix(0)
      if (aa_1_ix .lt. 0) then
         aa_1 = inf
      else
         pv = nw_eta_v(aa_1_ix)
c     NO_SGN pv = mv_dir*pv
         pv = mv_dir*pv
         vr_n = vr_in_r(nw_eta_ix(aa_1_ix))
         vr_st = st(vr_n)
         if (iand(vr_st, alt_bt) .eq. 0 .or.
     &        iand(vr_st, bp_bt) .ne. 0) then
            if (pv .gt. zero) then
               aa_1 = (rsmi_ub(vr_n)-pr_act(vr_n))/pv
            else
               aa_1 = (rsmi_lb(vr_n)-pr_act(vr_n))/pv
            end if
         else
         end if
      end if
      if (aa_2_ix .lt. 0) then
         aa_2 = inf
      else
         pv = nw_eta_v(aa_2_ix)
c     NO_SGN pv = mv_dir*pv
         pv = mv_dir*pv
         vr_n = vr_in_r(nw_eta_ix(aa_2_ix))
         vr_st = st(vr_n)
         if (iand(vr_st, alt_bt) .eq. 0) then
            if (pv .gt. zero) then
               aa_2 = (rsmi_ub(vr_n)-pr_act(vr_n))/pv
            else
               aa_2 = (rsmi_lb(vr_n)-pr_act(vr_n))/pv
            end if
         else if (iand(vr_st, bp_bt) .ne. 0) then
            goto 8990
         else
         end if
      end if
      if (n_l1_cdd .eq. 0) then
         if (aa_2_ix .lt. 0) then
            loop_n = 220
            go to 8040
         else
            cdd_n_o_vr_t_lv_bs = -2
            ix_o_vr_t_lv_bs = aa_2_ix
            aa = aa_2
            go to 500
         end if
      end if
CM      IF (emsol_da .EQ. 1) THEN
C?      l1_cz_r_n_srt = l1_cz_r_n_srt + 1
C?      l1_cz_r_av_cdd_n = l1_cz_r_av_cdd_n + n_l1_cdd
C?      l1_cz_r_mx_cdd_n = max(l1_cz_r_mx_cdd_n, n_l1_cdd)
CM      ENDIF
c
c     Sort the heap by increasing values of l1_bp.
c
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_cz_r_lvl1) call ems_tt_rec(cz_r_l1_ph_2_sort_tt, n_bs)
CM      ENDIF
      call ems_incr_heap_srt_w_aux_v(n_l1_cdd, l1_bp, l1_dl_gd, l1_ix)
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_cz_r_lvl1) call ems_tt_rec(-cz_r_l1_ph_2_sort_tt, n_bs)
CM      ENDIF
      if (aa_bp_up_gd .gt. tl_du_ifs) then
c
c     The step aa_1 can give an L1 minimizer within the thick pencil.
c
         cdd_n_o_vr_t_lv_bs = -1
         ix_o_vr_t_lv_bs = aa_1_ix
         aa = aa_1
         go to 500
      end if
      l1_up_gd = l1_up_gd0
      pre_bp = v0
      pre_bp_gd_st = 0
c
c     Determine the entries in l1_ix of the following
c
c     The minimum acceptable ratio
c     .      (First at first BP with status >= 2)
c
c     The lower acceptable ratio
c     .      (First at first BP with status >= 3)
c
c     The upper acceptable ratio
c     .      (Last at last BP with status <= 5)
c
c     The maximum acceptable ratio
c     .      (Last at last BP with status <= 7)
c
c     Then decide on the best pivot (incorporating aa_2), only
c     considering ratios between minimum and lower acceptable values and
c     between upper and maximum acceptable values if the pivot is too
c     small.
c
      mn_bp_l1_cdd_n = n_l1_cdd + 1
      lo_bp_l1_cdd_n = n_l1_cdd + 1
      up_bp_l1_cdd_n = 0
      mx_bp_l1_cdd_n = 0
      pre_bp_dn_gd = l1_up_gd
      pre_pre_bp_gd_st = 0
      f_cdd_at_pre_bp = 1
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_cz_r_lvl1) call ems_tt_rec(cz_r_l1_ph_2_ps_3_tt, n_bs)
CM      ENDIF
      do 260, l1_cdd_n = 1, n_l1_cdd
         ix_n = l1_ix(l1_cdd_n)
         bp = l1_bp(l1_cdd_n)
c         if (bp .lt. zero) then
c            if (bp .lt. -tl_pr_ifs) then
c               if (ems_msg_no_prt_fm .ge. 1)
c     &        write(ems_li, 9220)n_si_it, bp
c               call ems_msg_wr_li(warn_msg_n)
c               rp_l1_cz_r = .true.
c            end if
c         else if (bp .gt. pre_bp) then
         if (bp .gt. pre_bp) then
c
c     Found a distinct breakpoint (which is non-negative since pre_bp is
c     initialised to zero): analyse the previous breakpoint.
c
            l_cdd_at_pre_bp = l1_cdd_n - 1
            pre_bp_up_gd = l1_up_gd
            pre_bp_gd_st = ems_l1_gd_st(
     &           pre_bp_dn_gd, pre_bp_up_gd, tl_du_ifs)
            if (pre_bp_gd_st .lt. pre_pre_bp_gd_st) then
               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9230)n_si_it,
     &              pre_bp_gd_st, pre_pre_bp_gd_st
               call ems_msg_wr_li(er_msg_n)
               rp_l1_cz_r = .true.
            end if
            if (pre_bp_gd_st .ge. 2) then
               mn_bp_l1_cdd_n =
     &              min(f_cdd_at_pre_bp, mn_bp_l1_cdd_n)
               if (pre_bp_gd_st .ge. 3) lo_bp_l1_cdd_n =
     &              min(f_cdd_at_pre_bp, lo_bp_l1_cdd_n)
               if (pre_bp_gd_st .le. 5) up_bp_l1_cdd_n =
     &              max(l_cdd_at_pre_bp, up_bp_l1_cdd_n)
               if (pre_bp_gd_st .le. 7) mx_bp_l1_cdd_n =
     &              max(l_cdd_at_pre_bp, mx_bp_l1_cdd_n)
               if (pre_bp_gd_st .ge. 8) go to 270
            end if
            pre_bp = bp
            pre_bp_dn_gd = l1_up_gd
            f_cdd_at_pre_bp = l1_cdd_n
         end if
         l1_dn_gd = l1_up_gd
         l1_up_gd = l1_up_gd + l1_dl_gd(l1_cdd_n)
 260  continue
c
c     Analyse the last breakpoint.
c
      l_cdd_at_pre_bp = n_l1_cdd
      pre_bp_up_gd = l1_up_gd
      pre_bp_gd_st = ems_l1_gd_st(
     &     pre_bp_dn_gd, pre_bp_up_gd, tl_du_ifs)
      if (pre_bp_gd_st .lt. pre_pre_bp_gd_st) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9230)n_si_it,
     &        pre_bp_gd_st, pre_pre_bp_gd_st
         call ems_msg_wr_li(er_msg_n)
         rp_l1_cz_r = .true.
      end if
      if (pre_bp_gd_st .ge. 2) then
         mn_bp_l1_cdd_n =
     &        min(f_cdd_at_pre_bp, mn_bp_l1_cdd_n)
         if (pre_bp_gd_st .ge. 3) lo_bp_l1_cdd_n =
     &        min(f_cdd_at_pre_bp, lo_bp_l1_cdd_n)
         if (pre_bp_gd_st .le. 5) up_bp_l1_cdd_n =
     &        max(l_cdd_at_pre_bp, up_bp_l1_cdd_n)
         if (pre_bp_gd_st .le. 7) mx_bp_l1_cdd_n =
     &        max(l_cdd_at_pre_bp, mx_bp_l1_cdd_n)
         if (pre_bp_gd_st .ge. 8) go to 270
      end if
 270  continue
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_cz_r_lvl1) call ems_tt_rec(-cz_r_l1_ph_2_ps_3_tt, n_bs)
CM      ENDIF
      if (lo_bp_l1_cdd_n .gt. n_l1_cdd) then
c
c     No minimizer of the l1 function has has been found. This is likely
c     to be due to the fact that the minimizer is greater than aa_2...
c
         if (aa_2_ix .ge. 0) then
c
c     ... in which case this is the step length.
c
            cdd_n_o_vr_t_lv_bs = -2
            ix_o_vr_t_lv_bs = aa_2_ix
            aa = aa_2
            go to 500
         else
c
c     Otherwise report unboundedness unless either
c
c     1. There was insufficient space to store all of the candidate
c     .  breakpoints and there was no minimizer amongst these candidates
c
c     2. The down gradient from the last breakpoint is a (negative)
c     .  perturbation of zero. This can be identified by the gradient
c     .  status of the last breakpoint being 2, in which case any of the
c     .  pivots at this breakpoint can be chosen.
c
            if (mn_bp_l1_cdd_n .gt. n_l1_cdd) then
               loop_n = 270
               un_bd = .true.
               go to 1000
            end if
c
c     Note that
c     .  lo_bp_l1_cdd_n > n_l1_cdd
c     => lo_bp_l1_cdd_n = n_l1_cdd + 1
c     => up_bp_l1_cdd_n = 0
c     => up_bp_l1_cdd_n = n_l1_cdd (below)
c     => loop 280 is vacuous
c
         end if
      end if
c
c     At least a lower limit on the interval of l1-minimizers has been
c     found.
c
      if (up_bp_l1_cdd_n .eq. 0) then
c
c     No upper limit on the interval of l1-minimizers has been found.
c
         up_bp_l1_cdd_n = n_l1_cdd
         mx_bp_l1_cdd_n = n_l1_cdd
      else if (mx_bp_l1_cdd_n .eq. 0) then
c
c     No maximum upper limit on the interval of l1-minimizers has been
c     found.
c
         mx_bp_l1_cdd_n = n_l1_cdd
      end if
      if (l1_bp(lo_bp_l1_cdd_n) .le. aa_2 .and.
     &     aa_2 .le. l1_bp(up_bp_l1_cdd_n)) then
c
c     The feasibility ratio is in the interval of l1-minimizers.
c     If it is a bound swap then take it, otherwise it becomes a
c     candidate pivot.
c
         cdd_n_o_vr_t_lv_bs = -2
         if (aa_2_ix .eq. 0) then
            ix_o_vr_t_lv_bs = aa_2_ix
            aa = aa_2
            go to 500
         end if
         aa_pv = aa_2_pv
      else
c
c     The feasibility ratio is not in the interval of l1-minimizers.
c
         cdd_n_o_vr_t_lv_bs = 0
         aa_pv = zero
      end if
c
c     Look through the l1-minimizers to find a better pivot.
c
      do 280, l1_cdd_n = lo_bp_l1_cdd_n, up_bp_l1_cdd_n
         ix_n = l1_ix(l1_cdd_n)
         pv = abs(nw_eta_v(ix_n))
         if (pv .gt. aa_pv .or. ix_n .eq. 0) then
            aa_pv = pv
            cdd_n_o_vr_t_lv_bs = l1_cdd_n
            if (ix_n .eq. 0) go to 290
         end if
 280  continue
      if (aa_pv .lt. ok_pv) then
c
c     Look through the outlying breakpoints to try to find a better
c     pivot.
c
CM      IF (emsol_da .EQ. 1) THEN
C?         l1_cz_r_n_alt_pv = l1_cz_r_n_alt_pv + 1
CM      ENDIF
         do 281, l1_cdd_n = mn_bp_l1_cdd_n, lo_bp_l1_cdd_n-1
            ix_n = l1_ix(l1_cdd_n)
            pv = abs(nw_eta_v(ix_n))
            if (pv .gt. aa_pv .or. ix_n .eq. 0) then
               aa_pv = pv
               cdd_n_o_vr_t_lv_bs = l1_cdd_n
               if (ix_n .eq. 0) go to 290
            end if
 281     continue
         do 282, l1_cdd_n = up_bp_l1_cdd_n+1, mx_bp_l1_cdd_n
            ix_n = l1_ix(l1_cdd_n)
            pv = abs(nw_eta_v(ix_n))
            if (pv .gt. aa_pv .or. ix_n .eq. 0) then
               aa_pv = pv
               cdd_n_o_vr_t_lv_bs = l1_cdd_n
               if (ix_n .eq. 0) go to 290
            end if
 282     continue
      end if
 290  continue
      if (cdd_n_o_vr_t_lv_bs .gt. 0) then
CM      IF (emsol_da .EQ. 1) THEN
C?         l1_cz_r_av_cz_cdd_n =
C?     &        l1_cz_r_av_cz_cdd_n + cdd_n_o_vr_t_lv_bs
C?         l1_cz_r_mx_cz_cdd_n =
C?     &        max(l1_cz_r_mx_cz_cdd_n, cdd_n_o_vr_t_lv_bs)
CM      ENDIF
         ix_o_vr_t_lv_bs = l1_ix(cdd_n_o_vr_t_lv_bs)
         aa = l1_bp(cdd_n_o_vr_t_lv_bs)
      else
         ix_o_vr_t_lv_bs = aa_2_ix
         aa = aa_2
      end if
      if (ix_o_vr_t_lv_bs .lt. 0) then
         loop_n = 280
         go to 8040
      end if
      go to 500
c
c     End of second pass for phase II.
c=======================================================================
c
c     End of phase II cz_r
c=======================================================================
c     Start of phase I cz_r
c
 400  continue
      tl_pr_ifs = tl_pr_ifs + xp_tau
      n_rpt = 0
 405  continue
      n_cdd_ix = 0
      og_tl_pr_ifs = tl_pr_ifs
c
c     aa_bp is the minimum step to an expanded bound/breakpoint
c
c     sv_aa_bp_ix is used to record the index of the variable
c     corresponding to aa_bp---just to test whether aa_bp is less than
c     the minimum step (only relevant when xp_tau>0) and report when
c     repeating the ratio test.
c
      aa_bp = inf
      sv_aa_bp_ix = -1
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_cz_r_lvl1)
CM      IF (sps_cz_r .EQ. 1) THEN
C?     &     call ems_tt_rec(cz_r_l1_ph_1_sps_ps_1_tt, n_bs)
CM      ELSE
C?     &     call ems_tt_rec(cz_r_l1_ph_1_dse_ps_1_tt, n_bs)
CM      ENDIF
CM      ENDIF
CM      IF (sps_cz_r .EQ. 1) THEN
C?      do 410, og_ix_n = 0, og_n_ix
C?         r_n = nw_eta_ix(og_ix_n)
CM      ELSE
      do 410, r_n = 0, n_r
CM      ENDIF
         pv = pv_c_v(r_n)
CM      IF (emsol_da .EQ. 1) THEN
C?c         if (pv .ne. zero) then
C?c            v = abs(pv)
C?c            if (v .le. 1d0) v = v*1d-1
C?c            i = log10(v)
C?c            if (i .lt. mn_pv_c_v_rec_by_1)
C?c     &           i = mn_pv_c_v_rec_by_1-1 - (mn_pv_c_v_rec_by_1-i)/10
C?c            i = max(min(i, mx_pv_c_v_rec_by_1), mn_pv_c_v_rec_by_10)
C?c            pv_c_v_rec(i) = pv_c_v_rec(i) + 1
C?c            su_n_pk_pv_c_en = su_n_pk_pv_c_en + 1
C?c            if (abs(pv) .le. pk_pv_c_ze)
C?c     &           su_n_pk_pv_c_ze = su_n_pk_pv_c_ze + 1
C?c         end if
CM      ENDIF
         if (pv .eq. zero) goto 410
CM      IF (dan .EQ. 1) THEN
C?         pv_c_v(r_n) = zero
CM      ENDIF
CM      IF (dvx .EQ. 1) THEN
C?         pv_c_v(r_n) = zero
CM      ENDIF
         if (abs(pv) .le. pk_pv_c_ze) then
            pv_c_v(r_n) = zero
            go to 410
         end if
         ix_n = ix_n + 1
         mx_pv_c_v = max(abs(pv), mx_pv_c_v)
         nw_eta_v(ix_n) = pv
         nw_eta_ix(ix_n) = r_n
         vr_n = vr_in_r(r_n)
CM      IF (dvx .EQ. 1) THEN
C?         if (dvx_ix(vr_n) .gt. 0)
C?     &        ed_wt_o_vr_t_en_bs = ed_wt_o_vr_t_en_bs + pv*pv
CM      ENDIF
CM      IF (sed .EQ. 1) THEN
         ed_wt_o_vr_t_en_bs = ed_wt_o_vr_t_en_bs + pv*pv
CM      ENDIF
c
c     Now complete the Phase I CHUZR pass 1 loop.
c
c     NO_SGN pv = mv_dir*pv
         pv = mv_dir*pv
         vr_st = st(vr_n)
         if (iand(vr_st, ifs_bt) .ne. 0) then
            if (pv .gt. zero) then
               if (iand(vr_st, up_bt) .ne. 0) then
c
c     The variable is moving up towards a lower bound
c
                  rsdu = (rsmi_lb(vr_n) - pr_act(vr_n)) + tl_pr_ifs
                  n_cdd_ix = n_cdd_ix + 1
                  cdd_ix(n_cdd_ix) = ix_n
                  if (rsdu .lt. aa_bp*pv) then
                     bp = rsdu/pv
                     aa_bp = bp
                     sv_aa_bp_ix = ix_n
                  end if
               end if
            else
c
c     The pivot is negative
c
               if (iand(vr_st, dn_bt) .ne. 0) then
c
c     The variable is moving down towards an upper bound
c
c     NB the residual rsmi_ub(vr_n)-pr_act(vr_n) and the pivot are both
c     negative so reverse the sign of the inequality.
c
                  rsdu = (rsmi_ub(vr_n) - pr_act(vr_n)) - tl_pr_ifs
                  n_cdd_ix = n_cdd_ix + 1
                  cdd_ix(n_cdd_ix) = ix_n
                  if (rsdu .gt. aa_bp*pv) then
                     bp = rsdu/pv
                     aa_bp = bp
                     sv_aa_bp_ix = ix_n
                  end if
               end if
            end if
         else
c
c     The variable is currently feasible.
c
            if (pv .gt. zero) then
               if (iand(vr_st, ub_bt) .ne. 0) then
c
c     The variable is moving up towards an upper bound or breakpoint.
c
                  rsdu = (rsmi_ub(vr_n) - pr_act(vr_n)) + tl_pr_ifs
                  n_cdd_ix = n_cdd_ix + 1
                  cdd_ix(n_cdd_ix) = ix_n
                  if (rsdu .lt. aa_bp*pv) then
                     bp = rsdu/pv
                     aa_bp = bp
                     sv_aa_bp_ix = ix_n
                  end if
               end if
            else
c
c     The pivot is negative
c
               if (iand(vr_st, lb_bt) .ne. 0) then
c
c     The variable is moving down towards a lower bound or breakpoint.
c
c     NB the residual rsmi_lb(vr_n)-pr_act(vr_n) and the pivot are both
c     negative so reverse the sign of the inequality.
c
                  rsdu = (rsmi_lb(vr_n) - pr_act(vr_n)) - tl_pr_ifs
                  n_cdd_ix = n_cdd_ix + 1
                  cdd_ix(n_cdd_ix) = ix_n
                  if (rsdu .gt. aa_bp*pv) then
                     bp = rsdu/pv
                     aa_bp = bp
                     sv_aa_bp_ix = ix_n
                  end if
               end if
            end if
         end if
 410  continue
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_cz_r_lvl1)
CM      IF (sps_cz_r .EQ. 1) THEN
C?     &     call ems_tt_rec(-cz_r_l1_ph_1_sps_ps_1_tt, n_bs)
CM      ELSE
C?     &     call ems_tt_rec(-cz_r_l1_ph_1_dse_ps_1_tt, n_bs)
CM      ENDIF
CM      ENDIF
      nw_eta_l_ix = ix_n
      ok_pv = mx_pv_c_v/tl_cz_r_growth
      if (aa_bp .lt. zero) then
         pv = nw_eta_v(sv_aa_bp_ix)
         vr_t_lv_bs = vr_in_r(nw_eta_ix(sv_aa_bp_ix))
         call ems_consider_rpt_rao_ts(
     &        rpt, n_rpt, pv, vr_t_lv_bs, aa_fs,
     &        st, rsmi_lb, rsmi_ub, pr_act, ds, is)
         if (rpt .eq. -5) then
            go to 8050
         else if (rpt .eq. -3) then
            go to 8030
         else
            go to 405
         end if
      else if (aa_bp .lt. inf) then
c
c     Alpha_BP should not be less than the minimum step due to primal
c     feasibility tolerance.
c
         pv = nw_eta_v(sv_aa_bp_ix)
         if (aa_bp .lt. xp_tau/abs(pv)) go to 8060
      else
         loop_n = 410
         go to 8040
      end if
c
c     End of first pass for phase I.
c=======================================================================
c     Start of second pass for phase I.
c
      v0 = zero
      l1_ob0 = zero
      l1_up_gd0 = mv_dir*du_act_o_vr_t_en_bs
c
c     Find the step:
c
c     aa_1 <= aa_bp with maximum pivot aa_1_pv and index aa_1_ix
c
c     Find aa_bp_up_gd, the L1 up gradient at aa_bp.
c
c     Maintain l1_cdd_up_gd, a lower bound on the L1 up gradient at
c     l1_bp(1). If l1_cdd_up_gd > tl then l1_bp(1) is an upper bound on
c     the step to the L1 minimizer.
c
      aa_1_pv = zero
      aa_1_ix = -1
      aa_bp_up_gd = l1_up_gd0
      l1_cdd_up_gd = l1_up_gd0
      l1_bp(1) = inf
      l1_ix(0) = 0
      rp_l1_cz_r = rp_l1_cz_r_df
      no_al_l1_cz_r_cdd = .false.
      mx_pv = zero
      cdd_n_o_vr_t_lv_bs = -1
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_cz_r_lvl1) call ems_tt_rec(cz_r_l1_ph_1_ps_2_tt, n_bs)
CM      ENDIF
      do 420, cdd_ix_n = 1, n_cdd_ix
c
c     NB All candidates have a pivot which is sufficiently large and a
c     finite value to move to which is given by the signed pivot.
c
         ix_n = cdd_ix(cdd_ix_n)
         pv = nw_eta_v(ix_n)
c     NO_SGN pv = mv_dir*pv
         pv = mv_dir*pv
         vr_n = vr_in_r(nw_eta_ix(ix_n))
         vr_st = st(vr_n)
         if (iand(vr_st, ifs_bt) .ne. 0) then
            if (pv .gt. zero) then
c
c     The variable is moving up towards a lower bound
c
               mx_pv = max(pv, mx_pv)
               rsdu = rsmi_lb(vr_n) - pr_act(vr_n)
               if (iand(vr_st, alt_bt) .eq. 0) then
c
c     The variable is standard:
c
                  if (iand(vr_st, ub_bt) .ne. 0) then
c
c     The variable has an upper bound.
c
                     if (rsmi_ub(vr_n) .eq. rsmi_lb(vr_n)) then
c
c     The variable is an equality so add the breakpoint to the heap with
c     double the gradient change.
c
                        bp = rsdu/pv
                        dl_gd = two*pv
                        if (bp .le. aa_bp) then
                           aa_bp_up_gd = aa_bp_up_gd + dl_gd
                           if (aa_1_ix .ne. 0 .and.
     &                          pv .gt. aa_1_pv) then
                              aa_1_ix = ix_n
                              aa_1_pv = pv
                           end if
                        end if
c
c     Don't allow moves to a distinct breakpoint if the L1 gradient is
c     not downhill.
c                  if (l1_cdd_up_gd .le. tl_du_ifs .or.
                  if (l1_cdd_up_gd .lt. zero .or.
     &                       bp .le. l1_bp(1)) then
                           if (l1_ix(0) .ge. l1_cz_r_mx_n_cdd)
     &                          go to 425
                           l1_cdd_up_gd = l1_cdd_up_gd  + dl_gd
                           call ems_add_to_incr_heap_w_aux_v(
     &                          .false., l1_cz_r_mx_n_cdd,
     &                          l1_bp, l1_dl_gd, l1_ix,
     &                          bp, dl_gd, ix_n)
                        end if
                        go to 420
                     else
c
c     Add the LB breakpoint to the heap and look at the UB breakpoint.
c
                        bp = rsdu/pv
                        dl_gd = pv
                        if (bp .le. aa_bp) then
                           aa_bp_up_gd = aa_bp_up_gd + dl_gd
                           if (aa_1_ix .ne. 0 .and.
     &                          pv .gt. aa_1_pv) then
                              aa_1_ix = ix_n
                              aa_1_pv = pv
                           end if
                        end if
c
c     Don't allow moves to a distinct breakpoint if the L1 gradient is
c     not downhill.
c                  if (l1_cdd_up_gd .le. tl_du_ifs .or.
                  if (l1_cdd_up_gd .lt. zero .or.
     &                       bp .le. l1_bp(1)) then
                           if (l1_ix(0) .ge. l1_cz_r_mx_n_cdd)
     &                          go to 425
                           l1_cdd_up_gd = l1_cdd_up_gd  + dl_gd
                           call ems_add_to_incr_heap_w_aux_v(
     &                          .false., l1_cz_r_mx_n_cdd,
     &                          l1_bp, l1_dl_gd, l1_ix,
     &                          bp, dl_gd, ix_n)
                        end if
                        rsdu = rsmi_ub(vr_n) - pr_act(vr_n)
                        bp = rsdu/pv
                        dl_gd = pv
                        if (bp .le. aa_bp) then
                           aa_bp_up_gd = aa_bp_up_gd + dl_gd
                           if (aa_1_ix .ne. 0 .and.
     &                          pv .gt. aa_1_pv) then
                              aa_1_ix = ix_n
                              aa_1_pv = pv
                           end if
                        end if
c
c     Don't allow moves to a distinct breakpoint if the L1 gradient is
c     not downhill.
c                  if (l1_cdd_up_gd .le. tl_du_ifs .or.
                  if (l1_cdd_up_gd .lt. zero .or.
     &                       bp .le. l1_bp(1)) then
                           if (l1_ix(0) .ge. l1_cz_r_mx_n_cdd)
     &                          go to 425
                           l1_cdd_up_gd = l1_cdd_up_gd  + dl_gd
                           call ems_add_to_incr_heap_w_aux_v(
     &                          .false., l1_cz_r_mx_n_cdd,
     &                          l1_bp, l1_dl_gd, l1_ix,
     &                          bp, dl_gd, ix_n)
                        end if
                     end if
                  else
c
c     The variable has no UB so just add the LB breakpoint to the heap.
c
                     bp = rsdu/pv
                     dl_gd = pv
                     if (bp .le. aa_bp) then
                        aa_bp_up_gd = aa_bp_up_gd + dl_gd
                        if (aa_1_ix .ne. 0 .and.
     &                       pv .gt. aa_1_pv) then
                           aa_1_ix = ix_n
                           aa_1_pv = pv
                        end if
                     end if
c
c     Don't allow moves to a distinct breakpoint if the L1 gradient is
c     not downhill.
c                  if (l1_cdd_up_gd .le. tl_du_ifs .or.
                  if (l1_cdd_up_gd .lt. zero .or.
     &                    bp .le. l1_bp(1)) then
                        if (l1_ix(0) .ge. l1_cz_r_mx_n_cdd) go to 425
                        l1_cdd_up_gd = l1_cdd_up_gd  + dl_gd
                        call ems_add_to_incr_heap_w_aux_v(
     &                       .false., l1_cz_r_mx_n_cdd,
     &                       l1_bp, l1_dl_gd, l1_ix,
     &                       bp, dl_gd, ix_n)
                     end if
                  end if
               else if (iand(vr_st, bp_bt) .eq. 0) then
c
c     The variable is PWL:
c
               end if
            else
c
c     The pivot is negative
c
c     The variable is moving down towards an upper bound
c
c     NB the residual rsmi_ub(vr_n)-pr_act(vr_n) and the pivot are both
c     negative so reverse the sign of the inequality.
c
               mx_pv = max(-pv, mx_pv)
               rsdu = rsmi_ub(vr_n) - pr_act(vr_n)
               if (iand(vr_st, alt_bt) .eq. 0) then
c
c     The variable is standard:
c
                  if (iand(vr_st, lb_bt) .ne. 0) then
c
c     The variable has a lower bound.
c
                     if (rsmi_ub(vr_n) .eq. rsmi_lb(vr_n)) then
c
c     The variable is an equality so add the breakpoint to the heap with
c     double the gradient change.
c
                        bp = rsdu/pv
                        dl_gd = -two*pv
                        if (bp .le. aa_bp) then
                           aa_bp_up_gd = aa_bp_up_gd + dl_gd
                           if (aa_1_ix .ne. 0 .and.
     &                          -pv .gt. aa_1_pv) then
                              aa_1_ix = ix_n
                              aa_1_pv = -pv
                           end if
                        end if
c
c     Don't allow moves to a distinct breakpoint if the L1 gradient is
c     not downhill.
c                  if (l1_cdd_up_gd .le. tl_du_ifs .or.
                  if (l1_cdd_up_gd .lt. zero .or.
     &                       bp .le. l1_bp(1)) then
                           if (l1_ix(0) .ge. l1_cz_r_mx_n_cdd)
     &                          go to 425
                           l1_cdd_up_gd = l1_cdd_up_gd  + dl_gd
                           call ems_add_to_incr_heap_w_aux_v(
     &                          .false., l1_cz_r_mx_n_cdd,
     &                          l1_bp, l1_dl_gd, l1_ix,
     &                          bp, dl_gd, ix_n)
                        end if
                        go to 420
                     else
c
c     Add the UB breakpoint to the heap and look at the LB breakpoint.
c
                        bp = rsdu/pv
                        dl_gd = -pv
                        if (bp .le. aa_bp) then
                           aa_bp_up_gd = aa_bp_up_gd + dl_gd
                           if (aa_1_ix .ne. 0 .and.
     &                          -pv .gt. aa_1_pv) then
                              aa_1_ix = ix_n
                              aa_1_pv = -pv
                           end if
                        end if
c
c     Don't allow moves to a distinct breakpoint if the L1 gradient is
c     not downhill.
c                  if (l1_cdd_up_gd .le. tl_du_ifs .or.
                  if (l1_cdd_up_gd .lt. zero .or.
     &                       bp .le. l1_bp(1)) then
                           if (l1_ix(0) .ge. l1_cz_r_mx_n_cdd)
     &                          go to 425
                           l1_cdd_up_gd = l1_cdd_up_gd  + dl_gd
                           call ems_add_to_incr_heap_w_aux_v(
     &                          .false., l1_cz_r_mx_n_cdd,
     &                          l1_bp, l1_dl_gd, l1_ix,
     &                          bp, dl_gd, ix_n)
                        end if
                        rsdu = rsmi_lb(vr_n) - pr_act(vr_n)
                        bp = rsdu/pv
                        dl_gd = -pv
                        if (bp .le. aa_bp) then
                           aa_bp_up_gd = aa_bp_up_gd + dl_gd
                           if (aa_1_ix .ne. 0 .and.
     &                          -pv .gt. aa_1_pv) then
                              aa_1_ix = ix_n
                              aa_1_pv = -pv
                           end if
                        end if
c
c     Don't allow moves to a distinct breakpoint if the L1 gradient is
c     not downhill.
c                  if (l1_cdd_up_gd .le. tl_du_ifs .or.
                  if (l1_cdd_up_gd .lt. zero .or.
     &                       bp .le. l1_bp(1)) then
                           if (l1_ix(0) .ge. l1_cz_r_mx_n_cdd)
     &                          go to 425
                           l1_cdd_up_gd = l1_cdd_up_gd  + dl_gd
                           call ems_add_to_incr_heap_w_aux_v(
     &                          .false., l1_cz_r_mx_n_cdd,
     &                          l1_bp, l1_dl_gd, l1_ix,
     &                          bp, dl_gd, ix_n)
                        end if
                     end if
                  else
c
c     The variable has no LB so just add the UB breakpoint to the heap.
c
                     bp = rsdu/pv
                     dl_gd = -pv
                     if (bp .le. aa_bp) then
                        aa_bp_up_gd = aa_bp_up_gd + dl_gd
                        if (aa_1_ix .ne. 0 .and.
     &                       -pv .gt. aa_1_pv) then
                           aa_1_ix = ix_n
                           aa_1_pv = -pv
                        end if
                     end if
c
c     Don't allow moves to a distinct breakpoint if the L1 gradient is
c     not downhill.
c                  if (l1_cdd_up_gd .le. tl_du_ifs .or.
                  if (l1_cdd_up_gd .lt. zero .or.
     &                    bp .le. l1_bp(1)) then
                        if (l1_ix(0) .ge. l1_cz_r_mx_n_cdd) go to 425
                        l1_cdd_up_gd = l1_cdd_up_gd  + dl_gd
                        call ems_add_to_incr_heap_w_aux_v(
     &                       .false., l1_cz_r_mx_n_cdd,
     &                       l1_bp, l1_dl_gd, l1_ix,
     &                       bp, dl_gd, ix_n)
                     end if
                  end if
               else if (iand(vr_st, bp_bt) .eq. 0) then
c
c     The variable is PWL:
c
               end if
            end if
         else
c
c     The variable is currently feasible.
c
            if (pv .gt. zero) then
               mx_pv = max(pv, mx_pv)
               rsdu = rsmi_ub(vr_n) - pr_act(vr_n)
               if (iand(vr_st, alt_bt) .eq. 0) then
c
c     The variable is standard:
c     The ratio corresponds to a move up to an upper bound.
c
                  bp = rsdu/pv
                  dl_gd = pv
                  if (bp .le. aa_bp) then
                     aa_bp_up_gd = aa_bp_up_gd + dl_gd
                     if (aa_1_ix .ne. 0 .and. pv .gt. aa_1_pv) then
                        aa_1_ix = ix_n
                        aa_1_pv = pv
                     end if
                  end if
c
c     Don't allow moves to a distinct breakpoint if the L1 gradient is
c     not downhill.
c                  if (l1_cdd_up_gd .le. tl_du_ifs .or.
                  if (l1_cdd_up_gd .lt. zero .or.
     &                 bp .le. l1_bp(1)) then
                     if (l1_ix(0) .ge. l1_cz_r_mx_n_cdd) go to 425
                     l1_cdd_up_gd = l1_cdd_up_gd  + dl_gd
                     call ems_add_to_incr_heap_w_aux_v(
     &                    .false., l1_cz_r_mx_n_cdd,
     &                    l1_bp, l1_dl_gd, l1_ix,
     &                    bp, dl_gd, ix_n)
                  end if
               else if (iand(vr_st, bp_bt) .ne. 0) then
c
c     The variable is BP:
c     The ratio corresponds to a move up to a break point.
c
                  bp = rsdu/pv
                  dl_gd = pv*pr_co_mu*rsmi_lb(vr_n)
                  if (bp .le. aa_bp) then
                     aa_bp_up_gd = aa_bp_up_gd + dl_gd
                     if (aa_1_ix .ne. 0 .and. pv .gt. aa_1_pv) then
                        aa_1_ix = ix_n
                        aa_1_pv = pv
                     end if
                  end if
c
c     Don't allow moves to a distinct breakpoint if the L1 gradient is
c     not downhill.
c                  if (l1_cdd_up_gd .le. tl_du_ifs .or.
                  if (l1_cdd_up_gd .lt. zero .or.
     &                 bp .le. l1_bp(1)) then
                     if (l1_ix(0) .ge. l1_cz_r_mx_n_cdd) go to 425
                     l1_cdd_up_gd = l1_cdd_up_gd  + dl_gd
                     call ems_add_to_incr_heap_w_aux_v(
     &                    .false., l1_cz_r_mx_n_cdd,
     &                    l1_bp, l1_dl_gd, l1_ix,
     &                    bp, dl_gd, ix_n)
                  end if
               else
c
c     The variable is PWL:
c
               end if
            else
c
c     The pivot is negative
c
c     The variable is moving towards a lower bound or breakpoint.
c
c     NB the residual rsmi_lb(vr_n)-pr_act(vr_n) and the pivot are both
c     negative so reverse the sign of the inequality.
c
               mx_pv = max(-pv, mx_pv)
               rsdu = rsmi_lb(vr_n) - pr_act(vr_n)
               if (iand(vr_st, alt_bt) .eq. 0) then
c
c     The variable is standard:
c     The ratio corresponds to a move down to a lower bound.
c
                  bp = rsdu/pv
                  dl_gd = -pv
                  if (bp .le. aa_bp) then
                     aa_bp_up_gd = aa_bp_up_gd + dl_gd
                     if (aa_1_ix .ne. 0 .and. -pv .gt. aa_1_pv) then
                        aa_1_ix = ix_n
                        aa_1_pv = -pv
                     end if
                  end if
c
c     Don't allow moves to a distinct breakpoint if the L1 gradient is
c     not downhill.
c                  if (l1_cdd_up_gd .le. tl_du_ifs .or.
                  if (l1_cdd_up_gd .lt. zero .or.
     &                    bp .le. l1_bp(1)) then
                     if (l1_ix(0) .ge. l1_cz_r_mx_n_cdd) go to 425
                     l1_cdd_up_gd = l1_cdd_up_gd  + dl_gd
                     call ems_add_to_incr_heap_w_aux_v(
     &                    .false., l1_cz_r_mx_n_cdd,
     &                    l1_bp, l1_dl_gd, l1_ix,
     &                    bp, dl_gd, ix_n)
                  end if
               else if (iand(vr_st, bp_bt) .ne. 0) then
c
c     The variable is BP:
c     The ratio corresponds to a move down to a break point.
c
                  bp = rsdu/pv
c     Changed 11/03/97
c                  dl_gd = -pv*pr_co_mu*rsmi_lb(vr_n)
                  dl_gd = pv*pr_co_mu*rsmi_ub(vr_n)
                  if (bp .le. aa_bp) then
                     aa_bp_up_gd = aa_bp_up_gd + dl_gd
                     if (aa_1_ix .ne. 0 .and. -pv .gt. aa_1_pv) then
                        aa_1_ix = ix_n
                        aa_1_pv = -pv
                     end if
                  end if
c
c     Don't allow moves to a distinct breakpoint if the L1 gradient is
c     not downhill.
c                  if (l1_cdd_up_gd .le. tl_du_ifs .or.
                  if (l1_cdd_up_gd .lt. zero .or.
     &                 bp .le. l1_bp(1)) then
                     if (l1_ix(0) .ge. l1_cz_r_mx_n_cdd) go to 425
                     l1_cdd_up_gd = l1_cdd_up_gd  + dl_gd
                     call ems_add_to_incr_heap_w_aux_v(
     &                    .false., l1_cz_r_mx_n_cdd,
     &                    l1_bp, l1_dl_gd, l1_ix,
     &                    bp, dl_gd, ix_n)
                  end if
               else
c
c     The variable is PWL:
c
               end if
            end if
         end if
 420  continue
      goto 430
 425  continue
c
c     There was not sufficient room to store all candidate breakpoints:
c     Find the L1-minimizer within the stored candidates and use this
c     as an upper bound on acceptable candidates in a subsequent pass.
c
      alg_er = .true.
c      print*, ' l1_ix(0) = ', l1_ix(0),
c     &     ' l1_cz_r_mx_n_cdd ',l1_cz_r_mx_n_cdd
c      call ems_rp_ml_da_st_msk
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9400)n_si_it
      call ems_msg_wr_li(warn_msg_n)
      go to 7000
 430  continue
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_cz_r_lvl1) call ems_tt_rec(-cz_r_l1_ph_1_ps_2_tt, n_bs)
CM      ENDIF
      n_l1_cdd = l1_ix(0)
      if (aa_1_ix .lt. 0) then
         aa_1 = inf
      else
         pv = nw_eta_v(aa_1_ix)
c     NO_SGN pv = mv_dir*pv
         pv = mv_dir*pv
         vr_n = vr_in_r(nw_eta_ix(aa_1_ix))
         vr_st = st(vr_n)
         if (iand(vr_st, alt_bt) .eq. 0 .or.
     &        iand(vr_st, bp_bt) .ne. 0) then
            if (iand(vr_st, ifs_bt) .ne. 0) then
               if (pv .gt. zero) then
                  aa_1 = (rsmi_lb(vr_n)-pr_act(vr_n))/pv
               else
                  aa_1 = (rsmi_ub(vr_n)-pr_act(vr_n))/pv
               end if
            else
               if (pv .gt. zero) then
                  aa_1 = (rsmi_ub(vr_n)-pr_act(vr_n))/pv
               else
                  aa_1 = (rsmi_lb(vr_n)-pr_act(vr_n))/pv
               end if
            endif
         else
         end if
      end if
      if (n_l1_cdd .eq. 0) then
         loop_n = 420
         go to 8040
      end if
CM      IF (emsol_da .EQ. 1) THEN
C?      l1_cz_r_n_srt = l1_cz_r_n_srt + 1
C?      l1_cz_r_av_cdd_n = l1_cz_r_av_cdd_n + n_l1_cdd
C?      l1_cz_r_mx_cdd_n = max(l1_cz_r_mx_cdd_n, n_l1_cdd)
CM      ENDIF
c
c     Sort the heap by increasing values of l1_bp.
c
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_cz_r_lvl1) call ems_tt_rec(cz_r_l1_ph_1_sort_tt, n_bs)
CM      ENDIF
      call ems_incr_heap_srt_w_aux_v(n_l1_cdd, l1_bp, l1_dl_gd, l1_ix)
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_cz_r_lvl1) call ems_tt_rec(-cz_r_l1_ph_1_sort_tt, n_bs)
CM      ENDIF
      if (aa_bp_up_gd .gt. tl_du_ifs) then
c
c     The step aa_1 can give an L1 minimizer within the thick pencil.
c
         cdd_n_o_vr_t_lv_bs = -1
         ix_o_vr_t_lv_bs = aa_1_ix
         aa = aa_1
         go to 500
      end if
      l1_up_gd = l1_up_gd0
      pre_bp = v0
      pre_bp_gd_st = 0
c
c     Determine the entries in l1_ix of the following
c
c     The minimum acceptable ratio
c     .      (First at first BP with status >= 2)
c
c     The lower acceptable ratio
c     .      (First at first BP with status >= 3)
c
c     The upper acceptable ratio
c     .      (Last at last BP with status <= 5)
c
c     The maximum acceptable ratio
c     .      (Last at last BP with status <= 7)
c
c     Then decide on the best pivot, only
c     considering ratios between minimum and lower acceptable values and
c     between upper and maximum acceptable values if the pivot is too
c     small.
c
      mn_bp_l1_cdd_n = n_l1_cdd + 1
      lo_bp_l1_cdd_n = n_l1_cdd + 1
      up_bp_l1_cdd_n = 0
      mx_bp_l1_cdd_n = 0
      pre_bp_dn_gd = l1_up_gd
      pre_pre_bp_gd_st = 0
      f_cdd_at_pre_bp = 1
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_cz_r_lvl1) call ems_tt_rec(cz_r_l1_ph_1_ps_3_tt, n_bs)
CM      ENDIF
      do 460, l1_cdd_n = 1, n_l1_cdd
         ix_n = l1_ix(l1_cdd_n)
         bp = l1_bp(l1_cdd_n)
c         if (bp .lt. zero) then
c            if (bp .lt. -tl_pr_ifs) then
c               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9220)
c     &        n_si_it, bp
c               call ems_msg_wr_li(warn_msg_n)
c               rp_l1_cz_r = .true.
c            end if
c         else if (bp .gt. pre_bp) then
         if (bp .gt. pre_bp) then
c
c     Found a distinct breakpoint (which is non-negative since pre_bp is
c     initialised to zero): analyse the previous breakpoint.
c
            l_cdd_at_pre_bp = l1_cdd_n - 1
            pre_bp_up_gd = l1_up_gd
            pre_bp_gd_st = ems_l1_gd_st(
     &           pre_bp_dn_gd, pre_bp_up_gd, tl_du_ifs)
            if (pre_bp_gd_st .lt. pre_pre_bp_gd_st) then
               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9230)n_si_it,
     &              pre_bp_gd_st, pre_pre_bp_gd_st
               call ems_msg_wr_li(er_msg_n)
               rp_l1_cz_r = .true.
            end if
            if (pre_bp_gd_st .ge. 2) then
               mn_bp_l1_cdd_n =
     &              min(f_cdd_at_pre_bp, mn_bp_l1_cdd_n)
               if (pre_bp_gd_st .ge. 3) lo_bp_l1_cdd_n =
     &              min(f_cdd_at_pre_bp, lo_bp_l1_cdd_n)
               if (pre_bp_gd_st .le. 5) up_bp_l1_cdd_n =
     &              max(l_cdd_at_pre_bp, up_bp_l1_cdd_n)
               if (pre_bp_gd_st .le. 7) mx_bp_l1_cdd_n =
     &              max(l_cdd_at_pre_bp, mx_bp_l1_cdd_n)
               if (pre_bp_gd_st .ge. 8) go to 470
            end if
            pre_bp = bp
            pre_bp_dn_gd = l1_up_gd
            f_cdd_at_pre_bp = l1_cdd_n
         end if
         l1_dn_gd = l1_up_gd
         l1_up_gd = l1_up_gd + l1_dl_gd(l1_cdd_n)
 460  continue
c
c     Analyse the last breakpoint.
c
      l_cdd_at_pre_bp = n_l1_cdd
      pre_bp_up_gd = l1_up_gd
      pre_bp_gd_st = ems_l1_gd_st(
     &     pre_bp_dn_gd, pre_bp_up_gd, tl_du_ifs)
      if (pre_bp_gd_st .lt. pre_pre_bp_gd_st) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9230)n_si_it,
     &        pre_bp_gd_st, pre_pre_bp_gd_st
         call ems_msg_wr_li(er_msg_n)
         rp_l1_cz_r = .true.
      end if
      if (pre_bp_gd_st .ge. 2) then
         mn_bp_l1_cdd_n =
     &        min(f_cdd_at_pre_bp, mn_bp_l1_cdd_n)
         if (pre_bp_gd_st .ge. 3) lo_bp_l1_cdd_n =
     &        min(f_cdd_at_pre_bp, lo_bp_l1_cdd_n)
         if (pre_bp_gd_st .le. 5) up_bp_l1_cdd_n =
     &        max(l_cdd_at_pre_bp, up_bp_l1_cdd_n)
         if (pre_bp_gd_st .le. 7) mx_bp_l1_cdd_n =
     &        max(l_cdd_at_pre_bp, mx_bp_l1_cdd_n)
         if (pre_bp_gd_st .ge. 8) go to 470
      end if
 470  continue
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_cz_r_lvl1) call ems_tt_rec(-cz_r_l1_ph_1_ps_3_tt, n_bs)
CM      ENDIF
      if (lo_bp_l1_cdd_n .gt. n_l1_cdd) then
c
c     No minimizer of the l1 function has has been found.
c     Report unboundedness unless either
c
c     1. There was insufficient space to store all of the candidate
c     .  breakpoints and there was no minimizer amongst these candidates
c
c     2. The down gradient from the last breakpoint is a (negative)
c     .  perturbation of zero. This can be identified by the gradient
c     .  status of the last breakpoint being 2, in which case any of the
c     .  pivots at this breakpoint can be chosen.
c
         if (mn_bp_l1_cdd_n .gt. n_l1_cdd) then
            loop_n = 470
            un_bd = .true.
            go to 1000
         endif
c
c     Note that
c     .  lo_bp_l1_cdd_n > n_l1_cdd
c     => lo_bp_l1_cdd_n = n_l1_cdd + 1
c     => up_bp_l1_cdd_n = 0
c     => up_bp_l1_cdd_n = n_l1_cdd (below)
c     => loop 480 is vacuous
c
      end if
c
c     At least a lower limit on the interval of l1-minimizers has been
c     found.
c
      if (up_bp_l1_cdd_n .eq. 0) then
c
c     No upper limit on the interval of l1-minimizers has been found.
c
         up_bp_l1_cdd_n = n_l1_cdd
         mx_bp_l1_cdd_n = n_l1_cdd
      else if (mx_bp_l1_cdd_n .eq. 0) then
c
c     No maximum upper limit on the interval of l1-minimizers has been
c     found.
c
         mx_bp_l1_cdd_n = n_l1_cdd
      end if
c
c     Look through the l1-minimizers to find the best pivot.
c
      cdd_n_o_vr_t_lv_bs = 0
      aa_pv = zero
      do 480, l1_cdd_n = lo_bp_l1_cdd_n, up_bp_l1_cdd_n
         ix_n = l1_ix(l1_cdd_n)
         pv = abs(nw_eta_v(ix_n))
         if (pv .gt. aa_pv .or. ix_n .eq. 0) then
            aa_pv = pv
            cdd_n_o_vr_t_lv_bs = l1_cdd_n
            if (ix_n .eq. 0) go to 490
         end if
 480  continue
      if (aa_pv .lt. ok_pv) then
CM      IF (emsol_da .EQ. 1) THEN
C?         l1_cz_r_n_alt_pv = l1_cz_r_n_alt_pv + 1
CM      ENDIF
         do 481, l1_cdd_n = mn_bp_l1_cdd_n, lo_bp_l1_cdd_n-1
            ix_n = l1_ix(l1_cdd_n)
            pv = abs(nw_eta_v(ix_n))
            if (pv .gt. aa_pv .or. ix_n .eq. 0) then
               aa_pv = pv
               cdd_n_o_vr_t_lv_bs = l1_cdd_n
               if (ix_n .eq. 0) go to 490
            end if
 481     continue
         do 482, l1_cdd_n = up_bp_l1_cdd_n+1, mx_bp_l1_cdd_n
            ix_n = l1_ix(l1_cdd_n)
            pv = abs(nw_eta_v(ix_n))
            if (pv .gt. aa_pv .or. ix_n .eq. 0) then
               aa_pv = pv
               cdd_n_o_vr_t_lv_bs = l1_cdd_n
               if (ix_n .eq. 0) go to 490
            end if
 482     continue
      end if
 490  continue
      if (cdd_n_o_vr_t_lv_bs .gt. 0) then
CM      IF (emsol_da .EQ. 1) THEN
C?         l1_cz_r_av_cz_cdd_n =
C?     &        l1_cz_r_av_cz_cdd_n + cdd_n_o_vr_t_lv_bs
C?         l1_cz_r_mx_cz_cdd_n =
C?     &        max(l1_cz_r_mx_cz_cdd_n, cdd_n_o_vr_t_lv_bs)
CM      ENDIF
         aa = l1_bp(cdd_n_o_vr_t_lv_bs)
         ix_o_vr_t_lv_bs = l1_ix(cdd_n_o_vr_t_lv_bs)
      else
c
c     Unboundedness cannot be detected here since at least a lower limit
c     on the interval of l1-minimizers has been found and pivots are
c     accepted however small (although they are necessarily greater than
c     the absolute tolerance and excessive growth will be identified
c     later.)
c
         loop_n = 480
         un_bd = .true.
         go to 1000
      end if
c
c     End of phase I cz_r
c=======================================================================
 500  continue
      pv = nw_eta_v(ix_o_vr_t_lv_bs)
      if (ix_o_vr_t_lv_bs .gt. 0 .and. abs(pv) .lt. ok_pv) then
c
c     Growth will occur if this pivot is used: possibly refine the
c     pivotal column.
c
         if (iand(cz_r_msk, cz_r_refine_bt) .ne. 0 .and.
     &        .not. refined_pv_c) then
            pv_r_n = nw_eta_ix(ix_o_vr_t_lv_bs)
            vr_n = vr_in_r(pv_r_n)
            growth = abs(mx_pv_c_v/pv)
CM      IF (emsol_dev .EQ. 1) THEN
C?            call ems_mo_rsmi_growth(n_si_it, growth_act_refine_pv_c,
C?     &           pv_r_n, vr_n, pv, aa, growth, zero, 0)
CM      ENDIF
            refine_pv_c = .true.
            rp_growth = .true.
            goto 7000
         end if
      end if
 1000 continue
CM      IF (emsol_da .EQ. 1) THEN
C?      if (rp_l1_cz_r) call ems_rp_l1_cz_r(
C?     &     rp_l1_cz_r_cn, n_l1_cdd,
C?     &     st, rsmi_lb, pr_act, rsmi_ub, vr_in_r,
C?     &     nw_eta_v, nw_eta_ix,
C?     &     v0, l1_ob0, l1_up_gd0,
C?     &     l1_bp, l1_dl_gd, l1_ix,
C?     &     cdd_n_o_vr_t_lv_bs,
C?     &     aa_1_ix, aa_1,
C?     &     aa_2_ix, aa_2)
CM      ENDIF
      if (un_bd) goto 8040
      pv = nw_eta_v(ix_o_vr_t_lv_bs)
      if (abs(pv) .lt. cz_r_pv_tl) then
         call ems_ck_cz_r_pv(
     &        pv_c_v, nw_eta_v, nw_eta_ix,
     &        nw_eta_f_ix, nw_eta_l_ix,
     &        ix_o_vr_t_lv_bs,
     &        prev_n_ix, rpt_cz_r, alg_er)
         if (alg_er) goto 7000
         if (rpt_cz_r) goto 100
      endif
CM      IF (dvx .EQ. 1) THEN
C?      if (ed_wt_o_vr_t_en_bs .gt. one) then
C?         ed_wt_o_vr_t_en_bs = sqrt(ed_wt_o_vr_t_en_bs)
C?      else
C?         ed_wt_o_vr_t_en_bs = one
C?      endif
C?      dvx_rao = max(
C?     &     ed_wt_o_vr_t_en_bs/og_ed_wt_o_vr_t_en_bs,
C?     &     og_ed_wt_o_vr_t_en_bs/ed_wt_o_vr_t_en_bs)
C?      i_te = n_r/nw_dvx_fwk_fq
C?      nw_dvx_fwk = dvx_rao .gt. tl_dvx_wt .or.
C?     &     n_dvx_it .gt. max(mn_n_dvx_it, i_te)
C?      ed_wt(vr_t_en_bs) = ed_wt_o_vr_t_en_bs
CM      ENDIF
CM      IF (sed .EQ. 1) THEN
      ed_wt_o_vr_t_en_bs = ed_wt_o_vr_t_en_bs*half
      ed_wt_er = abs((
     &     og_ed_wt_o_vr_t_en_bs-ed_wt_o_vr_t_en_bs)/
     &     og_ed_wt_o_vr_t_en_bs)
      if (ed_wt_er .gt. mx_ed_wt_er) then
         mx_ed_wt_er = ed_wt_er
         if (iand(rsmi_msg_msk, rsmi_er_li_bt) .ne. 0) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9500)
     &           n_si_it, 'Stpst edge wt:',
     &           ' U_STPST_ED_WT', og_ed_wt_o_vr_t_en_bs,
     &           '         FTRAN', ed_wt_o_vr_t_en_bs, ed_wt_er
            call ems_msg_wr_li(warn_msg_n)
         end if
      end if
      ed_wt(vr_t_en_bs) = ed_wt_o_vr_t_en_bs
CM      ENDIF
      vr_t_lv_bs = vr_in_r(nw_eta_ix(ix_o_vr_t_lv_bs))
      pv = nw_eta_v(ix_o_vr_t_lv_bs)
      aa = max(aa, xp_tau/mx_pv)
      if (aa .ge. inf) go to 8010
      if (aa .lt. zero) go to 8020
      if (ix_o_vr_t_lv_bs .gt. 0 .and.
     &     abs(pv) .lt. ok_pv*1d1 .and. abs(pv) .ge. ok_pv) then
c
c     Near-growth has occurred so maybe monitor it.
c
         pv_r_n = nw_eta_ix(ix_o_vr_t_lv_bs)
         vr_n = vr_in_r(pv_r_n)
         growth = abs(mx_pv_c_v/pv)
CM      IF (emsol_dev .EQ. 1) THEN
C?         call ems_mo_rsmi_growth(n_si_it, growth_act_nr_growth,
C?     &        pv_r_n, vr_n, pv, aa, growth, zero, 0)
CM      ENDIF
      else if (ix_o_vr_t_lv_bs .gt. 0 .and. abs(pv) .lt. ok_pv) then
c
c     Potential growth has been detected and the pivotal column has
c     already been refined if this option has been selected.
c
         pv_r_n = nw_eta_ix(ix_o_vr_t_lv_bs)
         vr_n = vr_in_r(pv_r_n)
         growth = abs(mx_pv_c_v/pv)
         growth_mode = iand(cz_r_msk, cz_r_growth_mode)
         if (growth_mode .eq. cz_r_growth_inv .and. n_u .gt. 0) then
c
c     Rather than accept the growth, INVERT the basis and re-calculate
c     the pivotal column.
c
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9410)n_si_it,
     &           pv, vr_t_lv_bs, abs(mx_pv_c_v/pv)
            call ems_msg_wr_li(warn_msg_n)
CM      IF (emsol_dev .EQ. 1) THEN
C?            call ems_mo_rsmi_growth(n_si_it, growth_act_re_inv,
C?     &           pv_r_n, vr_n, pv, aa, growth, zero, 0)
CM      ENDIF
            rq_inv = rq_inv_u_growth
            ml_da_st_msk =
     &           ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_inv)
            go to 7000
         else if (growth_mode .eq. cz_r_growth_cg_tl) then
         else if (growth_mode .eq. cz_r_growth_mv_bd) then
         else if (growth_mode .eq. cz_r_growth_cg_act) then
         else
c
c     No further action can be taken to prevent growth!
c
CM      IF (emsol_dev .EQ. 1) THEN
C?            call ems_mo_rsmi_growth(n_si_it, growth_act_no,
C?     &           pv_r_n, vr_n, pv, aa, growth, zero, 0)
CM      ENDIF
c            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9420)n_si_it,
c     &           pv, vr_t_lv_bs, abs(mx_pv_c_v/pv)
c            call ems_msg_wr_li(warn_msg_n)
            rp_growth = .false.
         end if
      else if (rp_growth) then
         pv_r_n = nw_eta_ix(ix_o_vr_t_lv_bs)
         vr_n = vr_in_r(pv_r_n)
         if (ix_o_vr_t_lv_bs .gt. 0) then
            growth = abs(mx_pv_c_v/pv)
         else
            growth = one
         endif
CM      IF (emsol_dev .EQ. 1) THEN
C?         call ems_mo_rsmi_growth(n_si_it, growth_act_rp,
C?     &        pv_r_n, vr_n, pv, aa, growth, zero, 0)
CM      ENDIF
         rp_growth = .false.
      end if
      n_cdd_ix0 = n_cdd_ix
      do 1110, cdd_ix_n = 1, n_cdd_ix0
         ix_n = cdd_ix(cdd_ix_n)
 1100    continue
         if (ix_n .eq. 0 .or. ix_n .eq. ix_o_vr_t_lv_bs) then
            ix_n = cdd_ix(n_cdd_ix)
            cdd_ix(cdd_ix_n) = ix_n
            n_cdd_ix = n_cdd_ix - 1
            if (cdd_ix_n .le. n_cdd_ix) goto 1100
            goto 1120
         endif
         r_n = nw_eta_ix(ix_n)
         cdd_ix(cdd_ix_n) = r_n
         if (cdd_ix_n .ge. n_cdd_ix) goto 1120
 1110 continue
 1120 continue
      cdd_ix(0) = n_cdd_ix
      if (ix_o_vr_t_lv_bs .ne. 0) then
         pv = nw_eta_v(ix_o_vr_t_lv_bs)
         pv_r_n = nw_eta_ix(ix_o_vr_t_lv_bs)
c
c     Put the pivot first in the packed vector. Set nw_eta_f_ix = 0 so
c     the pivotal value is picked up in loops from nw_eta_f_ix to
c     nw_eta_l_ix.
c
         nw_eta_v(0) = pv
         nw_eta_ix(0) = pv_r_n
         nw_eta_f_ix = 0
         if (nw_eta_l_ix .le. ord_i_ls_stp_p1) then
c
c     For short etas, place the last index in the position previously
c     occupied by the index of the pivot.
c
            nw_eta_ix(ix_o_vr_t_lv_bs) = nw_eta_ix(nw_eta_l_ix)
            nw_eta_v(ix_o_vr_t_lv_bs) = nw_eta_v(nw_eta_l_ix)
c
c     Decrease the pointer to the last index.
c
            nw_eta_l_ix = nw_eta_l_ix - 1
         else
c
c     For longer etas, keep the row indices in increasing order at the
c     cost of having the index of the pivot appearing twice. Zero the
c     corresponding value so that it doesn't get added in twice. NB When
c     the new eta indices are used to scatter the new eta values, the
c     loops must be reversed to avoid scattering this zero onto the
c     pivot value scattered by nw_eta_ix(0).
c
            nw_eta_v(ix_o_vr_t_lv_bs) = zero
         end if
c
c     Determine the variable to leave the basis.
c
         vr_t_lv_bs = vr_in_r(pv_r_n)
      else
c
c     For a bound swap, define the pivot value and the pivotal row
c     number.
c
         pv = one
         pv_r_n = 0
c
c     Define the variable to leave the basis to be the variable to enter
c     the basis. This is how a bound swap will be recognised.
c
         vr_t_lv_bs = vr_t_en_bs
      end if
c
c     Give alpha a sign corresponding to the direction in which the
c     entering variable is moving.
c
      aa = mv_dir*aa
 7000 continue
CM      IF (dan .EQ. 1) THEN
C?      if (iand(ck_msk, ze_a_ck_bt) .ne. 0)
C?     &     call ems_ck_ze_rl_a(n_r, pv_c_v)
CM      ENDIF
CM      IF (dvx .EQ. 1) THEN
C?      if (iand(ck_msk, ze_a_ck_bt) .ne. 0)
C?     &     call ems_ck_ze_rl_a(n_r, pv_c_v)
CM      ENDIF
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_cz_r_lvl0) call ems_tt_rec(-cz_r_tt, n_bs)
CM      ENDIF
      return
 8010 continue
      un_bd = .true.
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)n_si_it, aa
      call ems_msg_wr_li(warn_msg_n)
      go to 7000
 8020 continue
      alg_er = .true.
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)n_si_it, aa
      call ems_msg_wr_li(warn_msg_n)
      go to 7000
 8030 continue
      alg_er = .true.
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9803)
     &     n_si_it, abs(rsdu)
      call ems_msg_wr_li(warn_msg_n)
      go to 7000
 8040 continue
      un_bd = .true.
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9804)n_si_it, loop_n
      call ems_msg_wr_li(warn_msg_n)
      go to 7000
 8050 continue
      alg_er = .true.
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9805)n_si_it
      call ems_msg_wr_li(warn_msg_n)
      go to 7000
 8060 continue
      alg_er = .true.
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9806)
     &     n_si_it, aa_bp, xp_tau/abs(pv)
      call ems_msg_wr_li(warn_msg_n)
      go to 7000
 8990 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9899)
      call ems_msg_wr_li(bug_msg_n)
      go to 7000
 9410 format('Iteration ', i7,
     &     ':         Pivot ',  g11.4, ' for variable ', i7,
     &     ' would give growth of ', g11.4, ' so INVERT')
c 9420 format('Iteration ', i7,
c     &     ':         Pivot ',  g11.4, ' for variable ', i7,
c     &     '  will give growth of ', g11.4, ' which is unavoidable')
CM      IF (sed .EQ. 1) THEN
 9500 format('Iteration ', i7, 1x, a14,
     &     2(' From ', a14, ' = ', g11.4), ' |Rel. error| = ', g11.4)
CM      ENDIF
c 9220 format('Iteration ', i7, ': Negative ratio = ', g11.4)
 9230 format('Iteration ', i7, ': l1_CZ_R: pre_bp_gd_st = ', i1,
     &     ' .lt. pre_pre_bp_gd_st = ', i1)
 
 9400 format('Iteration ', i7,
     &     ': Insufficient space for all L1 CHUZR candidates')
 
 9801 format('Iteration ', i7,
     &     ': Step length of ', g11.4, ' suggests unboundedness ')
 9802 format('Iteration ', i7,
     &     ': Step length of ', g11.4, ' suggests algorithmic error')
 9803 format('Iteration ', i7,
     &     ': |Residual| of ', g11.4,
     &     ' gives negative ratio with expanded bounds')
 9804 format('Iteration ', i7,
     &     ': Unboundedness detected in cz_r loop ', i3)
 9805 format('Iteration ', i7,
     &     ': Cannot repeat a ratio test more than 10 times')
 9806 format('Iteration ', i7,
     &     ': Ratio test with expanded bounds gives step ', g11.4,
     &     ' less than the minimum step ', g11.4)
 9899 format('STRANGE: aa_2_ix corresponds to BP variable')
      end
