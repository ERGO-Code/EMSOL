C]
C->>> ---------------------------------------------------> ems_rg_da <<<
c     Sets up the block for model ranging information and then gets it.
c
      subroutine ems_rg_da(ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      double precision ds(0:ds_n_en_m1)
      integer is(0:is_n_en_m1)
 
      if (iand(ml_blk_st_msk, ml_blk_st_ml_rg_da) .eq. 0) then
         call ems_iz_blk_ml_rg_da(is)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      endif
c
c     Remove any negation of the row dual activities
c
      if (r_du_act_sgn .lt. 0) call ems_ng_r_du_act(ds)
      call ems_g_rg_da(
     &     is(p_vr_in_c), is(p_vr_in_r),
     &     ds(p_rsmi_co),
     &     ds(p_scl), ds(p_pr_act), ds(p_du_act),
     &     ds(p_pv_c_v),
     &     ds(p_co_rg_up_co_v), ds(p_co_rg_lo_co_v),
     &     ds(p_co_rg_up_ob_v), ds(p_co_rg_lo_ob_v),
     &     ds(p_co_rg_up_act_v), ds(p_co_rg_lo_act_v),
     &     is(p_co_rg_up_en_vr), is(p_co_rg_lo_en_vr),
     &     is(p_co_rg_up_lv_vr), is(p_co_rg_lo_lv_vr),
     &     ds(p_bd_rg_up_bd_v), ds(p_bd_rg_lo_bd_v),
     &     ds(p_bd_rg_up_ob_v), ds(p_bd_rg_lo_ob_v),
     &     is(p_bd_rg_up_en_vr), is(p_bd_rg_lo_en_vr),
     &     is(p_bd_rg_up_lv_vr), is(p_bd_rg_lo_lv_vr),
     &     ds, is)
 7000 continue
      return
      end
 
C->>> -------------------------------------------------> ems_g_rg_da <<<
c     Gets the model ranging information.
c
      subroutine ems_g_rg_da(
     &     vr_in_c, vr_in_r,
     &     co,
     &     scl, pr_act, du_act,
     &     pv_c,
     &     co_rg_up_co_v, co_rg_lo_co_v,
     &     co_rg_up_ob_v, co_rg_lo_ob_v,
     &     co_rg_up_act_v, co_rg_lo_act_v,
     &     co_rg_up_en_vr, co_rg_lo_en_vr,
     &     co_rg_up_lv_vr, co_rg_lo_lv_vr,
     &     bd_rg_up_bd_v, bd_rg_lo_bd_v,
     &     bd_rg_up_ob_v, bd_rg_lo_ob_v,
     &     bd_rg_up_en_vr, bd_rg_lo_en_vr,
     &     bd_rg_up_lv_vr, bd_rg_lo_lv_vr,
     &     ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      integer vr_in_c(-vr_in_c_n_sn:n_c)
      integer vr_in_r(0:n_r)
      double precision co(0:mx_n_c+n_r)
      double precision scl(0:mx_n_c+n_r)
      double precision pr_act(0:mx_n_c+n_r)
      double precision du_act(0:mx_n_c+n_r)
      double precision pv_c(0:n_r)
      double precision co_rg_up_co_v(0:mx_n_c+n_r)
      double precision co_rg_lo_co_v(0:mx_n_c+n_r)
      double precision co_rg_up_ob_v(0:mx_n_c+n_r)
      double precision co_rg_lo_ob_v(0:mx_n_c+n_r)
      double precision co_rg_up_act_v(0:mx_n_c+n_r)
      double precision co_rg_lo_act_v(0:mx_n_c+n_r)
      integer co_rg_up_en_vr(0:mx_n_c+n_r)
      integer co_rg_lo_en_vr(0:mx_n_c+n_r)
      integer co_rg_up_lv_vr(0:mx_n_c+n_r)
      integer co_rg_lo_lv_vr(0:mx_n_c+n_r)
      double precision bd_rg_up_bd_v(0:mx_n_c+n_r)
      double precision bd_rg_lo_bd_v(0:mx_n_c+n_r)
      double precision bd_rg_up_ob_v(0:mx_n_c+n_r)
      double precision bd_rg_lo_ob_v(0:mx_n_c+n_r)
      integer bd_rg_up_en_vr(0:mx_n_c+n_r)
      integer bd_rg_lo_en_vr(0:mx_n_c+n_r)
      integer bd_rg_up_lv_vr(0:mx_n_c+n_r)
      integer bd_rg_lo_lv_vr(0:mx_n_c+n_r)
      double precision ds(0:ds_n_en_m1)
      integer is(0:is_n_en_m1)
      integer r_n, c_n, vr_n
      integer fm_il_sn_n
      integer t_il_sn_n
      double precision aa_up, aa_lo
      integer pv_r_n_up, pv_r_n_lo
      double precision pr_act_v
      double precision du_act_v
      integer up_vr_n, lo_vr_n
      integer up_en_vr_n, lo_en_vr_n
      integer up_lv_vr_n, lo_lv_vr_n
      integer sv_vr_st
      double precision up_dl_co_v, lo_dl_co_v
      double precision up_dl_act_v, lo_dl_act_v
      double precision scl_v, rcp_scl_v
      logical ze_pr_act
      logical ze_du_act
c
c     Make sure that the basis is optimal and that the model does not
c     contain non-standard variables.
c
      if (prob_st .ne. prob_st_op) goto 8000
      if (iand(ml_da_st_msk, ml_da_st_alt_lp) .ne. 0) goto 8010
c
c     Check that there are no logical bugs in the assumed properties of
c     vr_in_c
c
      if (vr_in_c(os_lg_in_c_ab_ub_p) .gt. 0) then
         fm_il_sn_n = vr_in_c_sn_ty_ab_bp
         t_il_sn_n = vr_in_c_sn_ty_ab_ub
         goto 8990
      endif
      if (vr_in_c(os_lg_in_c_fx_p) .lt.
     &     vr_in_c(os_struc_in_c_ab_ub_p)) then
         fm_il_sn_n = vr_in_c_n_sn_ty + vr_in_c_sn_ty_ab_bp
         t_il_sn_n = vr_in_c_n_sn_ty + vr_in_c_sn_ty_ab_ub
         goto 8990
      endif
      if (vr_in_c(os_struc_in_c_fx_p) .lt. n_c) goto 8980
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tt_da .gt. 0) call ems_tt_rec(rg_da_tt, -1)
CM      ENDIF
      call ems_cp_rl_a(1+mx_n_c+n_r, zero, co_rg_up_co_v, 0)
      call ems_cp_rl_a(1+mx_n_c+n_r, zero, co_rg_lo_co_v, 0)
      call ems_cp_rl_a(1+mx_n_c+n_r, zero, co_rg_up_ob_v, 0)
      call ems_cp_rl_a(1+mx_n_c+n_r, zero, co_rg_lo_ob_v, 0)
      call ems_cp_rl_a(1+mx_n_c+n_r, zero, co_rg_up_act_v, 0)
      call ems_cp_rl_a(1+mx_n_c+n_r, zero, co_rg_lo_act_v, 0)
      call ems_cp_i_a(1+mx_n_c+n_r, 0, co_rg_up_en_vr, 0)
      call ems_cp_i_a(1+mx_n_c+n_r, 0, co_rg_lo_en_vr, 0)
      call ems_cp_i_a(1+mx_n_c+n_r, 0, co_rg_up_lv_vr, 0)
      call ems_cp_i_a(1+mx_n_c+n_r, 0, co_rg_lo_lv_vr, 0)
      call ems_cp_rl_a(1+mx_n_c+n_r, zero, bd_rg_up_bd_v, 0)
      call ems_cp_rl_a(1+mx_n_c+n_r, zero, bd_rg_lo_bd_v, 0)
      call ems_cp_rl_a(1+mx_n_c+n_r, zero, bd_rg_up_ob_v, 0)
      call ems_cp_rl_a(1+mx_n_c+n_r, zero, bd_rg_lo_ob_v, 0)
      call ems_cp_i_a(1+mx_n_c+n_r, 0, bd_rg_up_en_vr, 0)
      call ems_cp_i_a(1+mx_n_c+n_r, 0, bd_rg_lo_en_vr, 0)
      call ems_cp_i_a(1+mx_n_c+n_r, 0, bd_rg_up_lv_vr, 0)
      call ems_cp_i_a(1+mx_n_c+n_r, 0, bd_rg_lo_lv_vr, 0)
c
c     Scale the solution if scaling factors exist and it is currently
c     unscaled.
c
      if (iand(ml_da_st_msk, ml_da_st_scl_ml) .ne. 0 .and.
     &     iand(ml_da_st_msk, ml_da_st_scl_ml_sol) .eq. 0)
     &     call ems_scl_ml_sol(ds, is)
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Set vr_in_r(0) to undefined to make sure it is not touched.
C?c
C?      call ems_debug_se_i_a_undn(vr_in_r, 0, 0)
CM      ENDIF
c
c     The results of the primal ratio tests:
c
c     aa_*       are stored in bd_rg_*_bd_v
c     pv_r_n_*   are stored in bd_rg_*_lv_vr
c
c     The results of the dual ratio tests:
c
c     *_dl_co_v  are stored in co_rg_*_co_v
c     *_dl_act_v are stored in co_rg_*_ob_v
c     *_vr_n     are stored in co_rg_*_en_vr
c                    and negated if *_dl_co_v = 0.
c     +/-        are stored in co_rg_*_lv_vr
c     +              indicates that the up   ratio is used for |*_vr_n|
c     -              indicates that the down ratio is used for |*_vr_n|
c
c     Initialise the records for the dual ratio tests
c     (Strictly only needs to be over entries in vr_in_r).
c
      call ems_cp_rl_a(1+mx_n_c+n_r, inf, co_rg_up_co_v, 0)
      call ems_cp_rl_a(1+mx_n_c+n_r, inf, co_rg_lo_co_v, 0)
      call ems_cp_rl_a(1+mx_n_c+n_r, zero, co_rg_up_act_v, 0)
      call ems_cp_rl_a(1+mx_n_c+n_r, zero, co_rg_lo_act_v, 0)
      call ems_cp_i_a(1+mx_n_c+n_r, 0, co_rg_up_en_vr, 0)
      call ems_cp_i_a(1+mx_n_c+n_r, 0, co_rg_lo_en_vr, 0)
      call ems_cp_i_a(1+mx_n_c+n_r, 0, co_rg_up_lv_vr, 0)
      call ems_cp_i_a(1+mx_n_c+n_r, 0, co_rg_lo_lv_vr, 0)
c
c     Loop over all nonbasic variables.
c
      do 30, c_n = 1, vr_in_c(os_lg_in_c_up_dn_p)
         vr_n = vr_in_c(c_n)
c
c     Free to move up or down with given dual activity.
c
         call ems_g_vr_rg_da(c_n, vr_n, .true.,
     &        aa_up, aa_lo,
     &        pv_r_n_up, pv_r_n_lo,
     &        vr_in_r,
     &        pr_act, du_act,
     &        co_rg_up_co_v, co_rg_lo_co_v,
     &        co_rg_up_act_v, co_rg_lo_act_v,
     &        co_rg_up_en_vr, co_rg_lo_en_vr,
     &        co_rg_up_lv_vr, co_rg_lo_lv_vr,
     &        ds, is)
c
c     Store the results of the primal ratio tests.
c
         bd_rg_up_bd_v(vr_n) = aa_up
         bd_rg_lo_bd_v(vr_n) = aa_lo
         bd_rg_up_lv_vr(vr_n) = pv_r_n_up
         bd_rg_lo_lv_vr(vr_n) = pv_r_n_lo
c
c     Get the activities and determine whether they should be treated as
c     zero to within the tolerances.
c
         pr_act_v = pr_act(vr_n)
         du_act_v = du_act(vr_n)
         ze_pr_act = abs(pr_act_v) .le. tl_pr_ifs
         ze_du_act = abs(du_act_v) .le. tl_du_ifs
         if (.not. ze_du_act) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9600)
     &           vr_n, du_act_v
            call ems_msg_wr_li(er_msg_n)
            ze_du_act = .true.
         endif
c
c     Deduce the cost ranging data for nonbasic logical BTW
c
c     Cost upper range:
c
         co_rg_up_co_v(vr_n) = co(vr_n)
         co_rg_up_ob_v(vr_n) = ob_fn_v
         if (pv_r_n_lo .ge. 0) then
            co_rg_up_act_v(vr_n) = pr_act_v - aa_lo
            co_rg_up_en_vr(vr_n) = vr_n
            if (pv_r_n_lo .eq. 0) then
               co_rg_up_lv_vr(vr_n) = vr_n
            else
               co_rg_up_lv_vr(vr_n) = vr_in_r(pv_r_n_lo)
            endif
         else
            co_rg_up_act_v(vr_n) = inf
            co_rg_up_en_vr(vr_n) = 0
            co_rg_up_lv_vr(vr_n) = 0
         endif
c
c     Cost lower range:
c
         co_rg_lo_co_v(vr_n) = co(vr_n)
         co_rg_lo_ob_v(vr_n) = ob_fn_v
         if (pv_r_n_up .ge. 0) then
            co_rg_lo_act_v(vr_n) = pr_act_v + aa_up
            co_rg_lo_en_vr(vr_n) = vr_n
            if (pv_r_n_up .eq. 0) then
               co_rg_lo_lv_vr(vr_n) = vr_n
            else
               co_rg_lo_lv_vr(vr_n) = vr_in_r(pv_r_n_up)
            endif
         else
            co_rg_lo_act_v(vr_n) = -inf
            co_rg_lo_en_vr(vr_n) = 0
            co_rg_lo_lv_vr(vr_n) = 0
         endif
 30   continue
      do 40, c_n = c_n, vr_in_c(os_lg_in_c_up_p)
         vr_n = vr_in_c(c_n)
c
c     Free to move up (from lower bound) with given dual activity.
c
         call ems_g_vr_rg_da(c_n, vr_n, .true.,
     &        aa_up, aa_lo,
     &        pv_r_n_up, pv_r_n_lo,
     &        vr_in_r,
     &        pr_act, du_act,
     &        co_rg_up_co_v, co_rg_lo_co_v,
     &        co_rg_up_act_v, co_rg_lo_act_v,
     &        co_rg_up_en_vr, co_rg_lo_en_vr,
     &        co_rg_up_lv_vr, co_rg_lo_lv_vr,
     &        ds, is)
c
c     Store the results of the primal ratio tests.
c
         bd_rg_up_bd_v(vr_n) = aa_up
         bd_rg_lo_bd_v(vr_n) = aa_lo
         bd_rg_up_lv_vr(vr_n) = pv_r_n_up
         bd_rg_lo_lv_vr(vr_n) = pv_r_n_lo
c
c     Get the activities and determine whether they should be treated as
c     zero to within the tolerances.
c
         pr_act_v = pr_act(vr_n)
         du_act_v = du_act(vr_n)
         ze_pr_act = abs(pr_act_v) .le. tl_pr_ifs
         ze_du_act = abs(du_act_v) .le. tl_du_ifs
         if (du_act_v .lt. -tl_du_ifs) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9601)
     &           vr_n, du_act_v
            call ems_msg_wr_li(er_msg_n)
            ze_du_act = .true.
         endif
c
c     Deduce the cost ranging data for nonbasic logical at LB
c
c     Cost upper range:
c
         co_rg_up_co_v(vr_n) = inf
         co_rg_up_act_v(vr_n) = pr_act_v
         if (ze_pr_act) then
            co_rg_up_ob_v(vr_n) = ob_fn_v
         else if (pr_act_v .gt. zero) then
            co_rg_up_ob_v(vr_n) = inf
         else
            co_rg_up_ob_v(vr_n) = -inf
         endif
         co_rg_up_en_vr(vr_n) = 0
         co_rg_up_lv_vr(vr_n) = 0
c
c     Cost lower range:
c
         if (ze_du_act) then
            co_rg_lo_co_v(vr_n) = co(vr_n)
            co_rg_lo_ob_v(vr_n) = ob_fn_v
         else
            co_rg_lo_co_v(vr_n) = co(vr_n) - du_act_v
            if (ze_pr_act) then
               co_rg_lo_ob_v(vr_n) = ob_fn_v
            else
               co_rg_lo_ob_v(vr_n) = ob_fn_v - mx_mn*du_act_v*pr_act_v
            endif
         endif
         if (pv_r_n_up .ge. 0) then
            co_rg_lo_act_v(vr_n) = pr_act_v + aa_up
            co_rg_lo_en_vr(vr_n) = vr_n
            if (pv_r_n_up .eq. 0) then
               co_rg_lo_lv_vr(vr_n) = vr_n
            else
               co_rg_lo_lv_vr(vr_n) = vr_in_r(pv_r_n_up)
            endif
         else
            co_rg_lo_act_v(vr_n) = -inf
            co_rg_lo_en_vr(vr_n) = 0
            co_rg_lo_lv_vr(vr_n) = 0
         endif
 40   continue
      do 45, c_n = c_n, vr_in_c(os_lg_in_c_dn_p)
         vr_n = vr_in_c(c_n)
c
c     Free to move down (from upper bound) with given dual activity.
c
         call ems_g_vr_rg_da(c_n, vr_n, .true.,
     &        aa_up, aa_lo,
     &        pv_r_n_up, pv_r_n_lo,
     &        vr_in_r,
     &        pr_act, du_act,
     &        co_rg_up_co_v, co_rg_lo_co_v,
     &        co_rg_up_act_v, co_rg_lo_act_v,
     &        co_rg_up_en_vr, co_rg_lo_en_vr,
     &        co_rg_up_lv_vr, co_rg_lo_lv_vr,
     &        ds, is)
c
c     Store the results of the primal ratio tests.
c
         bd_rg_up_bd_v(vr_n) = aa_up
         bd_rg_lo_bd_v(vr_n) = aa_lo
         bd_rg_up_lv_vr(vr_n) = pv_r_n_up
         bd_rg_lo_lv_vr(vr_n) = pv_r_n_lo
c
c     Get the activities and determine whether they should be treated as
c     zero to within the tolerances.
c
         pr_act_v = pr_act(vr_n)
         du_act_v = du_act(vr_n)
         ze_pr_act = abs(pr_act_v) .le. tl_pr_ifs
         ze_du_act = abs(du_act_v) .le. tl_du_ifs
         if (du_act_v .gt. tl_du_ifs) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9602)
     &           vr_n, du_act_v
            call ems_msg_wr_li(er_msg_n)
            ze_du_act = .true.
         endif
c
c     Deduce the cost ranging data for nonbasic logical at UB
c
c
c     Cost upper range:
c
         if (ze_du_act) then
            co_rg_up_co_v(vr_n) = co(vr_n)
            co_rg_up_ob_v(vr_n) = ob_fn_v
         else
            co_rg_up_co_v(vr_n) = co(vr_n) - du_act_v
            if (ze_pr_act) then
               co_rg_up_ob_v(vr_n) = ob_fn_v
            else
               co_rg_up_ob_v(vr_n) = ob_fn_v - mx_mn*du_act_v*pr_act_v
            endif
         endif
         if (pv_r_n_lo .ge. 0) then
            co_rg_up_en_vr(vr_n) = vr_n
            co_rg_up_act_v(vr_n) = pr_act_v - aa_lo
            if (pv_r_n_lo .eq. 0) then
               co_rg_up_lv_vr(vr_n) = vr_n
            else
               co_rg_up_lv_vr(vr_n) = vr_in_r(pv_r_n_lo)
            endif
         else
            co_rg_up_act_v(vr_n) = inf
            co_rg_up_en_vr(vr_n) = 0
            co_rg_up_lv_vr(vr_n) = 0
         endif
c
c     Cost lower range:
c
         co_rg_lo_co_v(vr_n) = -inf
         co_rg_lo_act_v(vr_n) = pr_act_v
         if (ze_pr_act) then
            co_rg_lo_ob_v(vr_n) = ob_fn_v
         else if (pr_act_v .gt. zero) then
            co_rg_lo_ob_v(vr_n) = -inf
         else
            co_rg_lo_ob_v(vr_n) = inf
         endif
         co_rg_lo_en_vr(vr_n) = 0
         co_rg_lo_lv_vr(vr_n) = 0
 45   continue
      do 47, c_n = c_n, vr_in_c(os_lg_in_c_fx_p)
         vr_n = vr_in_c(c_n)
c
c     Make the variable look as if it is free to move up and down, up,
c     or down according to whether the reduced cost is zero, positive or
c     negative but prevent the ranging on basic variables from being
c     updated.
c
         du_act_v = du_act(vr_n)
         ze_du_act = abs(du_act_v) .le. tl_du_ifs
         sv_vr_st = is(p_st+vr_n)
         if (ze_du_act) then
            is(p_st+vr_n) = ior(is(p_st+vr_n), up_dn)
         else if (du_act_v .gt. zero) then
            is(p_st+vr_n) = ior(is(p_st+vr_n), up_bt)
         else
            is(p_st+vr_n) = ior(is(p_st+vr_n), dn_bt)
         endif
c
c     Not free to move.
c
         call ems_g_vr_rg_da(c_n, vr_n, .false.,
     &        aa_up, aa_lo,
     &        pv_r_n_up, pv_r_n_lo,
     &        vr_in_r,
     &        pr_act, du_act,
     &        co_rg_up_co_v, co_rg_lo_co_v,
     &        co_rg_up_act_v, co_rg_lo_act_v,
     &        co_rg_up_en_vr, co_rg_lo_en_vr,
     &        co_rg_up_lv_vr, co_rg_lo_lv_vr,
     &        ds, is)
         is(p_st+vr_n) = sv_vr_st
c
c     Store the results of the primal ratio tests.
c
         bd_rg_up_bd_v(vr_n) = aa_up
         bd_rg_lo_bd_v(vr_n) = aa_lo
         bd_rg_up_lv_vr(vr_n) = pv_r_n_up
         bd_rg_lo_lv_vr(vr_n) = pv_r_n_lo
c
c     Get the activities and determine whether they should be treated as
c     zero to within the tolerances.
c
         pr_act_v = pr_act(vr_n)
         du_act_v = du_act(vr_n)
         ze_pr_act = abs(pr_act_v) .le. tl_pr_ifs
         ze_du_act = abs(du_act_v) .le. tl_du_ifs
c
c     Deduce the cost ranging data for nonbasic logical at FX
c
c     ===> Was
c
c     Cost upper range:
c
c         co_rg_up_co_v(vr_n) = inf
c         co_rg_up_act_v(vr_n) = pr_act_v
c         if (ze_pr_act) then
c            co_rg_up_ob_v(vr_n) = ob_fn_v
c         else if (pr_act_v .gt. zero) then
c            co_rg_up_ob_v(vr_n) = inf
c         else
c            co_rg_up_ob_v(vr_n) = -inf
c         endif
c         co_rg_up_en_vr(vr_n) = 0
c         co_rg_up_lv_vr(vr_n) = 0
c
c     Cost lower range:
c
c         co_rg_lo_co_v(vr_n) = -inf
c         co_rg_lo_act_v(vr_n) = pr_act_v
c         if (ze_pr_act) then
c            co_rg_lo_ob_v(vr_n) = ob_fn_v
c         else if (pr_act_v .gt. zero) then
c            co_rg_lo_ob_v(vr_n) = -inf
c         else
c            co_rg_lo_ob_v(vr_n) = inf
c         endif
c         co_rg_lo_en_vr(vr_n) = 0
c         co_rg_lo_lv_vr(vr_n) = 0
c
c     Was <===
c
         if (ze_du_act) then
c
c     Cost upper range:
c
            co_rg_up_co_v(vr_n) = co(vr_n)
            co_rg_up_ob_v(vr_n) = ob_fn_v
            if (pv_r_n_lo .ge. 0) then
               co_rg_up_en_vr(vr_n) = vr_n
               co_rg_up_act_v(vr_n) = pr_act_v - aa_lo
               if (pv_r_n_lo .eq. 0) then
                  co_rg_up_lv_vr(vr_n) = vr_n
               else
                  co_rg_up_lv_vr(vr_n) = vr_in_r(pv_r_n_lo)
               endif
            else
               co_rg_up_act_v(vr_n) = inf
               co_rg_up_en_vr(vr_n) = 0
               co_rg_up_lv_vr(vr_n) = 0
            endif
c
c     Cost lower range:
c
            co_rg_lo_co_v(vr_n) = co(vr_n)
            co_rg_lo_ob_v(vr_n) = ob_fn_v
            if (pv_r_n_up .ge. 0) then
               co_rg_lo_act_v(vr_n) = pr_act_v + aa_up
               co_rg_lo_en_vr(vr_n) = vr_n
               if (pv_r_n_up .eq. 0) then
                  co_rg_lo_lv_vr(vr_n) = vr_n
               else
                  co_rg_lo_lv_vr(vr_n) = vr_in_r(pv_r_n_up)
               endif
            else
               co_rg_lo_act_v(vr_n) = -inf
               co_rg_lo_en_vr(vr_n) = 0
               co_rg_lo_lv_vr(vr_n) = 0
            endif
         else if (du_act_v .gt. zero) then
c
c     Cost upper range:
c
            co_rg_up_co_v(vr_n) = inf
            co_rg_up_act_v(vr_n) = pr_act_v
            if (ze_pr_act) then
               co_rg_up_ob_v(vr_n) = ob_fn_v
            else if (pr_act_v .gt. zero) then
               co_rg_up_ob_v(vr_n) = inf
            else
               co_rg_up_ob_v(vr_n) = -inf
            endif
            co_rg_up_en_vr(vr_n) = 0
            co_rg_up_lv_vr(vr_n) = 0
c
c     Cost lower range:
c
            co_rg_lo_co_v(vr_n) = co(vr_n) - du_act_v
            if (ze_pr_act) then
               co_rg_lo_ob_v(vr_n) = ob_fn_v
            else
               co_rg_lo_ob_v(vr_n) = ob_fn_v - mx_mn*du_act_v*pr_act_v
            endif
            if (pv_r_n_up .ge. 0) then
               co_rg_lo_act_v(vr_n) = pr_act_v + aa_up
               co_rg_lo_en_vr(vr_n) = vr_n
               if (pv_r_n_up .eq. 0) then
                  co_rg_lo_lv_vr(vr_n) = vr_n
               else
                  co_rg_lo_lv_vr(vr_n) = vr_in_r(pv_r_n_up)
               endif
            else
               co_rg_lo_act_v(vr_n) = -inf
               co_rg_lo_en_vr(vr_n) = 0
               co_rg_lo_lv_vr(vr_n) = 0
            endif
         else
c
c     Cost upper range:
c
            co_rg_up_co_v(vr_n) = co(vr_n) - du_act_v
            if (ze_pr_act) then
               co_rg_up_ob_v(vr_n) = ob_fn_v
            else
               co_rg_up_ob_v(vr_n) = ob_fn_v - mx_mn*du_act_v*pr_act_v
            endif
            if (pv_r_n_lo .ge. 0) then
               co_rg_up_act_v(vr_n) = pr_act_v - aa_lo
               co_rg_up_en_vr(vr_n) = vr_n
               if (pv_r_n_lo .eq. 0) then
                  co_rg_up_lv_vr(vr_n) = vr_n
               else
                  co_rg_up_lv_vr(vr_n) = vr_in_r(pv_r_n_lo)
               endif
            else
               co_rg_up_act_v(vr_n) = inf
               co_rg_up_en_vr(vr_n) = 0
               co_rg_up_lv_vr(vr_n) = 0
            endif
c
c     Cost lower range:
c
            co_rg_lo_co_v(vr_n) = -inf
            co_rg_lo_act_v(vr_n) = pr_act_v
            if (ze_pr_act) then
               co_rg_lo_ob_v(vr_n) = ob_fn_v
            else if (pr_act_v .gt. zero) then
               co_rg_lo_ob_v(vr_n) = -inf
            else
               co_rg_lo_ob_v(vr_n) = inf
            endif
            co_rg_lo_en_vr(vr_n) = 0
            co_rg_lo_lv_vr(vr_n) = 0
         endif
 47   continue
      do 130, c_n = c_n, vr_in_c(os_struc_in_c_up_dn_p)
         vr_n = vr_in_c(c_n)
c
c     Free to move up or down with given dual activity.
c
         call ems_g_vr_rg_da(c_n, vr_n, .true.,
     &        aa_up, aa_lo,
     &        pv_r_n_up, pv_r_n_lo,
     &        vr_in_r,
     &        pr_act, du_act,
     &        co_rg_up_co_v, co_rg_lo_co_v,
     &        co_rg_up_act_v, co_rg_lo_act_v,
     &        co_rg_up_en_vr, co_rg_lo_en_vr,
     &        co_rg_up_lv_vr, co_rg_lo_lv_vr,
     &        ds, is)
c
c     Store the results of the primal ratio tests.
c
         bd_rg_up_bd_v(vr_n) = aa_up
         bd_rg_lo_bd_v(vr_n) = aa_lo
         bd_rg_up_lv_vr(vr_n) = pv_r_n_up
         bd_rg_lo_lv_vr(vr_n) = pv_r_n_lo
c
c     Get the activities and determine whether they should be treated as
c     zero to within the tolerances.
c
         pr_act_v = pr_act(vr_n)
         du_act_v = du_act(vr_n)
         ze_pr_act = abs(pr_act_v) .le. tl_pr_ifs
         ze_du_act = abs(du_act_v) .le. tl_du_ifs
         if (.not. ze_du_act) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9600)
     &           vr_n, du_act_v
            call ems_msg_wr_li(er_msg_n)
            ze_du_act = .true.
         endif
c
c     Deduce the cost ranging data for nonbasic structural BTW
c
c     Cost upper range:
c
         co_rg_up_co_v(vr_n) = co(vr_n)
         co_rg_up_ob_v(vr_n) = ob_fn_v
         if (pv_r_n_lo .ge. 0) then
            co_rg_up_act_v(vr_n) = pr_act_v - aa_lo
            co_rg_up_en_vr(vr_n) = vr_n
            if (pv_r_n_lo .eq. 0) then
               co_rg_up_lv_vr(vr_n) = vr_n
            else
               co_rg_up_lv_vr(vr_n) = vr_in_r(pv_r_n_lo)
            endif
         else
            co_rg_up_act_v(vr_n) = inf
            co_rg_up_en_vr(vr_n) = 0
            co_rg_up_lv_vr(vr_n) = 0
         endif
c
c     Cost lower range:
c
         co_rg_lo_co_v(vr_n) = co(vr_n)
         co_rg_lo_ob_v(vr_n) = ob_fn_v
         if (pv_r_n_up .ge. 0) then
            co_rg_lo_act_v(vr_n) = pr_act_v + aa_up
            co_rg_lo_en_vr(vr_n) = vr_n
            if (pv_r_n_up .eq. 0) then
               co_rg_lo_lv_vr(vr_n) = vr_n
            else
               co_rg_lo_lv_vr(vr_n) = vr_in_r(pv_r_n_up)
            endif
         else
            co_rg_lo_act_v(vr_n) = -inf
            co_rg_lo_en_vr(vr_n) = 0
            co_rg_lo_lv_vr(vr_n) = 0
         endif
 130  continue
      do 140, c_n = c_n, vr_in_c(os_struc_in_c_up_p)
         vr_n = vr_in_c(c_n)
c
c     Free to move up (from lower bound) with given dual activity.
c
         call ems_g_vr_rg_da(c_n, vr_n, .true.,
     &        aa_up, aa_lo,
     &        pv_r_n_up, pv_r_n_lo,
     &        vr_in_r,
     &        pr_act, du_act,
     &        co_rg_up_co_v, co_rg_lo_co_v,
     &        co_rg_up_act_v, co_rg_lo_act_v,
     &        co_rg_up_en_vr, co_rg_lo_en_vr,
     &        co_rg_up_lv_vr, co_rg_lo_lv_vr,
     &        ds, is)
c
c     Store the results of the primal ratio tests.
c
         bd_rg_up_bd_v(vr_n) = aa_up
         bd_rg_lo_bd_v(vr_n) = aa_lo
         bd_rg_up_lv_vr(vr_n) = pv_r_n_up
         bd_rg_lo_lv_vr(vr_n) = pv_r_n_lo
c
c     Get the activities and determine whether they should be treated as
c     zero to within the tolerances.
c
         pr_act_v = pr_act(vr_n)
         du_act_v = du_act(vr_n)
         ze_pr_act = abs(pr_act_v) .le. tl_pr_ifs
         ze_du_act = abs(du_act_v) .le. tl_du_ifs
         if (du_act_v .lt. -tl_du_ifs) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9601)
     &           vr_n, du_act_v
            call ems_msg_wr_li(er_msg_n)
            ze_du_act = .true.
         endif
c
c     Deduce the cost ranging data for nonbasic structural at LB
c
c     Cost upper range:
c
         co_rg_up_co_v(vr_n) = inf
         if (ze_pr_act) then
            co_rg_up_ob_v(vr_n) = ob_fn_v
         else if (pr_act_v .gt. zero) then
            co_rg_up_ob_v(vr_n) = inf
         else
            co_rg_up_ob_v(vr_n) = -inf
         endif
         co_rg_up_act_v(vr_n) = pr_act_v
         co_rg_up_en_vr(vr_n) = 0
         co_rg_up_lv_vr(vr_n) = 0
c
c     Cost lower range:
c
         if (ze_du_act) then
            co_rg_lo_co_v(vr_n) = co(vr_n)
            co_rg_lo_ob_v(vr_n) = ob_fn_v
         else
            co_rg_lo_co_v(vr_n) = co(vr_n) - du_act_v
            if (ze_pr_act) then
               co_rg_lo_ob_v(vr_n) = ob_fn_v
            else
               co_rg_lo_ob_v(vr_n) = ob_fn_v - mx_mn*du_act_v*pr_act_v
            endif
         endif
         if (pv_r_n_up .ge. 0) then
            co_rg_lo_act_v(vr_n) = pr_act_v + aa_up
            co_rg_lo_en_vr(vr_n) = vr_n
            if (pv_r_n_up .eq. 0) then
               co_rg_lo_lv_vr(vr_n) = vr_n
            else
               co_rg_lo_lv_vr(vr_n) = vr_in_r(pv_r_n_up)
            endif
         else
            co_rg_lo_act_v(vr_n) = -inf
            co_rg_lo_en_vr(vr_n) = 0
            co_rg_lo_lv_vr(vr_n) = 0
         endif
 140  continue
      do 145, c_n = c_n, vr_in_c(os_struc_in_c_dn_p)
         vr_n = vr_in_c(c_n)
c
c     Free to move down (from upper bound) with given dual activity.
c
         call ems_g_vr_rg_da(c_n, vr_n, .true.,
     &        aa_up, aa_lo,
     &        pv_r_n_up, pv_r_n_lo,
     &        vr_in_r,
     &        pr_act, du_act,
     &        co_rg_up_co_v, co_rg_lo_co_v,
     &        co_rg_up_act_v, co_rg_lo_act_v,
     &        co_rg_up_en_vr, co_rg_lo_en_vr,
     &        co_rg_up_lv_vr, co_rg_lo_lv_vr,
     &        ds, is)
c
c     Store the results of the primal ratio tests.
c
         bd_rg_up_bd_v(vr_n) = aa_up
         bd_rg_lo_bd_v(vr_n) = aa_lo
         bd_rg_up_lv_vr(vr_n) = pv_r_n_up
         bd_rg_lo_lv_vr(vr_n) = pv_r_n_lo
c
c     Get the activities and determine whether they should be treated as
c     zero to within the tolerances.
c
         pr_act_v = pr_act(vr_n)
         du_act_v = du_act(vr_n)
         ze_pr_act = abs(pr_act_v) .le. tl_pr_ifs
         ze_du_act = abs(du_act_v) .le. tl_du_ifs
         if (du_act_v .gt. tl_du_ifs) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9602)
     &           vr_n, du_act_v
            call ems_msg_wr_li(er_msg_n)
            ze_du_act = .true.
         endif
c
c     Deduce the cost ranging data for nonbasic structural at UB
c
c
c     Cost upper range:
c
         if (ze_du_act) then
            co_rg_up_co_v(vr_n) = co(vr_n)
            co_rg_up_ob_v(vr_n) = ob_fn_v
         else
            co_rg_up_co_v(vr_n) = co(vr_n) - du_act_v
            if (ze_pr_act) then
               co_rg_up_ob_v(vr_n) = ob_fn_v
            else
               co_rg_up_ob_v(vr_n) = ob_fn_v - mx_mn*du_act_v*pr_act_v
            endif
         endif
         if (pv_r_n_lo .ge. 0) then
            co_rg_up_act_v(vr_n) = pr_act_v - aa_lo
            co_rg_up_en_vr(vr_n) = vr_n
            if (pv_r_n_lo .eq. 0) then
               co_rg_up_lv_vr(vr_n) = vr_n
            else
               co_rg_up_lv_vr(vr_n) = vr_in_r(pv_r_n_lo)
            endif
         else
            co_rg_up_act_v(vr_n) = inf
            co_rg_up_en_vr(vr_n) = 0
            co_rg_up_lv_vr(vr_n) = 0
         endif
c
c     Cost lower range:
c
         co_rg_lo_co_v(vr_n) = -inf
         co_rg_lo_act_v(vr_n) = pr_act_v
         if (ze_pr_act) then
            co_rg_lo_ob_v(vr_n) = ob_fn_v
         else if (pr_act_v .gt. zero) then
            co_rg_lo_ob_v(vr_n) = -inf
         else
            co_rg_lo_ob_v(vr_n) = inf
         endif
         co_rg_lo_en_vr(vr_n) = 0
         co_rg_lo_lv_vr(vr_n) = 0
 145  continue
      do 147, c_n = c_n, vr_in_c(os_struc_in_c_fx_p)
         vr_n = vr_in_c(c_n)
c
c     Make the variable look as if it is free to move up and down, up,
c     or down according to whether the reduced cost is zero, positive or
c     negative but prevent the ranging on basic variables from being
c     updated.
c
         du_act_v = du_act(vr_n)
         ze_du_act = abs(du_act_v) .le. tl_du_ifs
         sv_vr_st = is(p_st+vr_n)
         if (ze_du_act) then
            is(p_st+vr_n) = ior(is(p_st+vr_n), up_dn)
         else if (du_act_v .gt. zero) then
            is(p_st+vr_n) = ior(is(p_st+vr_n), up_bt)
         else
            is(p_st+vr_n) = ior(is(p_st+vr_n), dn_bt)
         endif
c
c     Not free to move.
c
         call ems_g_vr_rg_da(c_n, vr_n, .false.,
     &        aa_up, aa_lo,
     &        pv_r_n_up, pv_r_n_lo,
     &        vr_in_r,
     &        pr_act, du_act,
     &        co_rg_up_co_v, co_rg_lo_co_v,
     &        co_rg_up_act_v, co_rg_lo_act_v,
     &        co_rg_up_en_vr, co_rg_lo_en_vr,
     &        co_rg_up_lv_vr, co_rg_lo_lv_vr,
     &        ds, is)
         is(p_st+vr_n) = sv_vr_st
c
c     Store the results of the primal ratio tests.
c
         bd_rg_up_bd_v(vr_n) = aa_up
         bd_rg_lo_bd_v(vr_n) = aa_lo
         bd_rg_up_lv_vr(vr_n) = pv_r_n_up
         bd_rg_lo_lv_vr(vr_n) = pv_r_n_lo
c
c     Get the activities and determine whether they should be treated as
c     zero to within the tolerances.
c
         pr_act_v = pr_act(vr_n)
         du_act_v = du_act(vr_n)
         ze_pr_act = abs(pr_act_v) .le. tl_pr_ifs
         ze_du_act = abs(du_act_v) .le. tl_du_ifs
c
c     Deduce the cost ranging data for nonbasic structural at FX
c
c     ===> Was
c
c
c     Cost upper range:
c
c         co_rg_up_co_v(vr_n) = inf
c         if (ze_pr_act) then
c            co_rg_up_ob_v(vr_n) = ob_fn_v
c         else if (pr_act_v .gt. zero) then
c            co_rg_up_ob_v(vr_n) = inf
c         else
c            co_rg_up_ob_v(vr_n) = -inf
c         endif
c         co_rg_up_act_v(vr_n) = pr_act_v
c         co_rg_up_en_vr(vr_n) = 0
c         co_rg_up_lv_vr(vr_n) = 0
c
c     Cost lower range:
c
c         co_rg_lo_co_v(vr_n) = -inf
c         if (ze_pr_act) then
c            co_rg_lo_ob_v(vr_n) = ob_fn_v
c         else if (pr_act_v .gt. zero) then
c            co_rg_lo_ob_v(vr_n) = -inf
c         else
c            co_rg_lo_ob_v(vr_n) = inf
c         endif
c         co_rg_lo_act_v(vr_n) = pr_act_v
c         co_rg_lo_en_vr(vr_n) = 0
c         co_rg_lo_lv_vr(vr_n) = 0
c
c     Was <===
c
         if (ze_du_act) then
c
c     Cost upper range:
c
            co_rg_up_co_v(vr_n) = co(vr_n)
            co_rg_up_ob_v(vr_n) = ob_fn_v
            if (pv_r_n_lo .ge. 0) then
               co_rg_up_en_vr(vr_n) = vr_n
               co_rg_up_act_v(vr_n) = pr_act_v - aa_lo
               if (pv_r_n_lo .eq. 0) then
                  co_rg_up_lv_vr(vr_n) = vr_n
               else
                  co_rg_up_lv_vr(vr_n) = vr_in_r(pv_r_n_lo)
               endif
            else
               co_rg_up_act_v(vr_n) = inf
               co_rg_up_en_vr(vr_n) = 0
               co_rg_up_lv_vr(vr_n) = 0
            endif
c
c     Cost lower range:
c
            co_rg_lo_co_v(vr_n) = co(vr_n)
            co_rg_lo_ob_v(vr_n) = ob_fn_v
            if (pv_r_n_up .ge. 0) then
               co_rg_lo_act_v(vr_n) = pr_act_v + aa_up
               co_rg_lo_en_vr(vr_n) = vr_n
               if (pv_r_n_up .eq. 0) then
                  co_rg_lo_lv_vr(vr_n) = vr_n
               else
                  co_rg_lo_lv_vr(vr_n) = vr_in_r(pv_r_n_up)
               endif
            else
               co_rg_lo_act_v(vr_n) = -inf
               co_rg_lo_en_vr(vr_n) = 0
               co_rg_lo_lv_vr(vr_n) = 0
            endif
         else if (du_act_v .gt. zero) then
c
c     Cost upper range:
c
            co_rg_up_co_v(vr_n) = inf
            co_rg_up_act_v(vr_n) = pr_act_v
            if (ze_pr_act) then
               co_rg_up_ob_v(vr_n) = ob_fn_v
            else if (pr_act_v .gt. zero) then
               co_rg_up_ob_v(vr_n) = inf
            else
               co_rg_up_ob_v(vr_n) = -inf
            endif
            co_rg_up_en_vr(vr_n) = 0
            co_rg_up_lv_vr(vr_n) = 0
c
c     Cost lower range:
c
            co_rg_lo_co_v(vr_n) = co(vr_n) - du_act_v
            if (ze_pr_act) then
               co_rg_lo_ob_v(vr_n) = ob_fn_v
            else
               co_rg_lo_ob_v(vr_n) = ob_fn_v - mx_mn*du_act_v*pr_act_v
            endif
            if (pv_r_n_up .ge. 0) then
               co_rg_lo_act_v(vr_n) = pr_act_v + aa_up
               co_rg_lo_en_vr(vr_n) = vr_n
               if (pv_r_n_up .eq. 0) then
                  co_rg_lo_lv_vr(vr_n) = vr_n
               else
                  co_rg_lo_lv_vr(vr_n) = vr_in_r(pv_r_n_up)
               endif
            else
               co_rg_lo_act_v(vr_n) = -inf
               co_rg_lo_en_vr(vr_n) = 0
               co_rg_lo_lv_vr(vr_n) = 0
            endif
         else
c
c     Cost upper range:
c
            co_rg_up_co_v(vr_n) = co(vr_n) - du_act_v
            if (ze_pr_act) then
               co_rg_up_ob_v(vr_n) = ob_fn_v
            else
               co_rg_up_ob_v(vr_n) = ob_fn_v - mx_mn*du_act_v*pr_act_v
            endif
            if (pv_r_n_lo .ge. 0) then
               co_rg_up_act_v(vr_n) = pr_act_v - aa_lo
               co_rg_up_en_vr(vr_n) = vr_n
               if (pv_r_n_lo .eq. 0) then
                  co_rg_up_lv_vr(vr_n) = vr_n
               else
                  co_rg_up_lv_vr(vr_n) = vr_in_r(pv_r_n_lo)
               endif
            else
               co_rg_up_act_v(vr_n) = inf
               co_rg_up_en_vr(vr_n) = 0
               co_rg_up_lv_vr(vr_n) = 0
            endif
c
c     Cost lower range:
c
            co_rg_lo_co_v(vr_n) = -inf
            co_rg_lo_act_v(vr_n) = pr_act_v
            if (ze_pr_act) then
               co_rg_lo_ob_v(vr_n) = ob_fn_v
            else if (pr_act_v .gt. zero) then
               co_rg_lo_ob_v(vr_n) = -inf
            else
               co_rg_lo_ob_v(vr_n) = inf
            endif
            co_rg_lo_en_vr(vr_n) = 0
            co_rg_lo_lv_vr(vr_n) = 0
         endif
 147  continue
c
c     Deduce the bound ranging data for nonbasic variables
c
      do 310, c_n = 1, n_c
         vr_n = vr_in_c(c_n)
         pr_act_v = pr_act(vr_n)
         du_act_v = du_act(vr_n)
         if (abs(pr_act_v) .le. tl_pr_ifs) pr_act_v = zero
         if (abs(du_act_v) .le. tl_du_ifs) du_act_v = zero
c
c     Reverse any change in sign of row duals.
c
         if (vr_n .gt. mx_n_c) du_act_v = r_du_act_sgn*du_act_v
         aa_up = bd_rg_up_bd_v(vr_n)
         pv_r_n_up = bd_rg_up_lv_vr(vr_n)
         if (aa_up .lt. inf) then
            bd_rg_up_bd_v(vr_n) = pr_act_v + aa_up
            bd_rg_up_ob_v(vr_n) = ob_fn_v + mx_mn*aa_up*du_act_v
         else
            bd_rg_up_bd_v(vr_n) = inf
            if (mx_mn .ge. zero) then
               bd_rg_up_ob_v(vr_n) = -inf
            else
               bd_rg_up_ob_v(vr_n) =  inf
            endif
         endif
         if (pv_r_n_up .ge. 0) then
            bd_rg_up_en_vr(vr_n) = vr_n
            if (pv_r_n_up .eq. 0) then
               bd_rg_up_lv_vr(vr_n) = vr_n
            else
               bd_rg_up_lv_vr(vr_n) = vr_in_r(pv_r_n_up)
            endif
         else
            bd_rg_up_en_vr(vr_n) = 0
            bd_rg_up_lv_vr(vr_n) = 0
         endif
         aa_lo = bd_rg_lo_bd_v(vr_n)
         pv_r_n_lo = bd_rg_lo_lv_vr(vr_n)
         if (aa_lo .lt. inf) then
            bd_rg_lo_bd_v(vr_n) = pr_act_v - aa_lo
            bd_rg_lo_ob_v(vr_n) = ob_fn_v - mx_mn*aa_lo*du_act_v
         else
            bd_rg_lo_bd_v(vr_n) = -inf
            if (mx_mn .ge. zero) then
               bd_rg_lo_ob_v(vr_n) = -inf
            else
               bd_rg_lo_ob_v(vr_n) =  inf
            endif
         endif
         if (pv_r_n_lo .ge. 0) then
            bd_rg_lo_en_vr(vr_n) = vr_n
            if (pv_r_n_lo .eq. 0) then
               bd_rg_lo_lv_vr(vr_n) = vr_n
            else
               bd_rg_lo_lv_vr(vr_n) = vr_in_r(pv_r_n_lo)
            endif
         else
            bd_rg_lo_en_vr(vr_n) = 0
            bd_rg_lo_lv_vr(vr_n) = 0
         endif
 310  continue
c
c     Deduce the cost and bound range data for basic variables.
c
      do 320, r_n = 1, n_r
         vr_n = vr_in_r(r_n)
         up_dl_co_v =  co_rg_up_co_v(vr_n)
         up_dl_act_v = co_rg_up_act_v(vr_n)
         up_vr_n = iabs(co_rg_up_en_vr(vr_n))
         if (up_vr_n .gt. 0) then
            up_en_vr_n =    bd_rg_up_en_vr(up_vr_n)
            if (co_rg_up_lv_vr(vr_n) .gt. 0) then
               up_lv_vr_n = bd_rg_up_lv_vr(up_vr_n)
            else
               up_lv_vr_n = bd_rg_lo_lv_vr(up_vr_n)
            endif
         else
            up_en_vr_n = 0
            up_lv_vr_n = 0
         endif
         lo_dl_co_v =  co_rg_lo_co_v(vr_n)
         lo_dl_act_v = co_rg_lo_act_v(vr_n)
         lo_vr_n = iabs(co_rg_lo_en_vr(vr_n))
         if (lo_vr_n .gt. 0) then
            lo_en_vr_n =    bd_rg_lo_en_vr(lo_vr_n)
            if (co_rg_lo_lv_vr(vr_n) .gt. 0) then
               lo_lv_vr_n = bd_rg_up_lv_vr(lo_vr_n)
            else
               lo_lv_vr_n = bd_rg_lo_lv_vr(lo_vr_n)
            endif
         else
            lo_en_vr_n = 0
            lo_lv_vr_n = 0
         endif
         pr_act_v = pr_act(vr_n)
         if (abs(pr_act_v) .le. tl_pr_ifs) pr_act_v = zero
         if (mx_mn .gt. zero) then
c
c     Increasing the basic cost by            up_dl_co_v...
c     increases the objective function by     pr_act_v*up_dl_co_v...
c     zeroes the dual activity for nonbasic   up_vr_n...
c     resulting in the basis change           up_en_vr_n <--> up_lv_vr_n
c
            if (up_dl_co_v .lt. inf) then
               co_rg_up_co_v(vr_n) = co(vr_n) + up_dl_co_v
               co_rg_up_ob_v(vr_n) = ob_fn_v + pr_act_v*up_dl_co_v
            else
               co_rg_up_co_v(vr_n) = inf
               if (pr_act_v .gt. zero) then
                  co_rg_up_ob_v(vr_n) = inf
               else if (pr_act_v .lt. zero) then
                  co_rg_up_ob_v(vr_n) = -inf
               else
                  co_rg_up_ob_v(vr_n) = ob_fn_v
               endif
            endif
            co_rg_up_en_vr(vr_n) = up_en_vr_n
            co_rg_up_lv_vr(vr_n) = up_lv_vr_n
c
c     Decreasing the basic cost by            lo_dl_co_v...
c     decreases the objective function by     pr_act_v*lo_dl_co_v...
c     zeroes the dual activity for nonbasic   lo_vr_n...
c     resulting in the basis change           lo_en_vr_n <--> lo_lv_vr_n
c
            if (lo_dl_co_v .lt. inf) then
               co_rg_lo_co_v(vr_n) = co(vr_n) - lo_dl_co_v
               co_rg_lo_ob_v(vr_n) = ob_fn_v - pr_act_v*lo_dl_co_v
            else
               co_rg_lo_co_v(vr_n) = -inf
               if (pr_act_v .gt. zero) then
                  co_rg_lo_ob_v(vr_n) = -inf
               else if (pr_act_v .lt. zero) then
                  co_rg_lo_ob_v(vr_n) = inf
               else
                  co_rg_lo_ob_v(vr_n) = ob_fn_v
               endif
            endif
            co_rg_lo_en_vr(vr_n) = lo_en_vr_n
            co_rg_lo_lv_vr(vr_n) = lo_lv_vr_n
         else
c
c     Increasing the basic cost by            lo_dl_co_v...
c     increases the objective function by     pr_act_v*lo_dl_co_v...
c     zeroes the dual activity for nonbasic   lo_vr_n...
c     resulting in the basis change           lo_en_vr_n <--> lo_lv_vr_n
c
            if (lo_dl_co_v .lt. inf) then
               co_rg_up_co_v(vr_n) = co(vr_n) + lo_dl_co_v
               co_rg_up_ob_v(vr_n) = ob_fn_v + pr_act_v*lo_dl_co_v
            else
               co_rg_up_co_v(vr_n) = inf
               if (pr_act_v .gt. zero) then
                  co_rg_up_ob_v(vr_n) = inf
               else if (pr_act_v .lt. zero) then
                  co_rg_up_ob_v(vr_n) = -inf
               else
                  co_rg_up_ob_v(vr_n) = ob_fn_v
               endif
            endif
            co_rg_up_en_vr(vr_n) = lo_en_vr_n
            co_rg_up_lv_vr(vr_n) = lo_lv_vr_n
c
c     Decreasing the basic cost by            up_dl_co_v...
c     decreases the objective function by     pr_act_v*up_dl_co_v...
c     zeroes the dual activity for nonbasic   up_vr_n...
c     resulting in the basis change           up_en_vr_n <--> up_lv_vr_n
c
            if (up_dl_co_v .lt. inf) then
               co_rg_lo_co_v(vr_n) = co(vr_n) - up_dl_co_v
               co_rg_lo_ob_v(vr_n) = ob_fn_v - pr_act_v*up_dl_co_v
            else
               co_rg_lo_co_v(vr_n) = -inf
               if (pr_act_v .gt. zero) then
                  co_rg_lo_ob_v(vr_n) = -inf
               else if (pr_act_v .lt. zero) then
                  co_rg_lo_ob_v(vr_n) = inf
               else
                  co_rg_lo_ob_v(vr_n) = ob_fn_v
               endif
            endif
            co_rg_lo_en_vr(vr_n) = up_en_vr_n
            co_rg_lo_lv_vr(vr_n) = up_lv_vr_n
         endif
c
c     Increasing the nonbasic variable        lo_vr_n...
c     increases the basic variable by         lo_dl_act_v...
c     at a cost of                            lo_dl_co_v...
c     increases the objective function by     lo_dl_co_v*lo_dl_act_v...
c     corresponding to basis change           lo_en_vr_n <--> lo_lv_vr_n
c
         if (lo_dl_act_v .lt. inf) then
            bd_rg_up_bd_v(vr_n) = pr_act(vr_n) + lo_dl_act_v
            bd_rg_up_ob_v(vr_n) = ob_fn_v + mx_mn*lo_dl_co_v*lo_dl_act_v
         else
            bd_rg_up_bd_v(vr_n) = inf
            if (mx_mn*lo_dl_co_v .gt. zero) then
               bd_rg_up_ob_v(vr_n) = inf
            else if (mx_mn*lo_dl_co_v .lt. zero) then
               bd_rg_up_ob_v(vr_n) = -inf
            else
               bd_rg_up_ob_v(vr_n) = ob_fn_v
            endif
         endif
         bd_rg_up_en_vr(vr_n) = lo_en_vr_n
         bd_rg_up_lv_vr(vr_n) = lo_lv_vr_n
c
c     Decreasing the nonbasic variable        up_vr_n...
c     decreases the basic variable by         up_dl_act_v...
c     at a cost of                            up_dl_co_v...
c     increases the objective function by     up_dl_co_v*up_dl_act_v...
c     corresponding to basis change           up_en_vr_n <--> up_lv_vr_n
c
         if (up_dl_act_v .lt. inf) then
            bd_rg_lo_bd_v(vr_n) = pr_act(vr_n) - up_dl_act_v
            bd_rg_lo_ob_v(vr_n) = ob_fn_v + mx_mn*up_dl_co_v*up_dl_act_v
         else
            bd_rg_lo_bd_v(vr_n) = -inf
            if (mx_mn*up_dl_co_v .gt. zero) then
               bd_rg_lo_ob_v(vr_n) = inf
            else if (mx_mn*up_dl_co_v .lt. zero) then
               bd_rg_lo_ob_v(vr_n) = -inf
            else
               bd_rg_lo_ob_v(vr_n) = ob_fn_v
            endif
         endif
         bd_rg_lo_en_vr(vr_n) = up_en_vr_n
         bd_rg_lo_lv_vr(vr_n) = up_lv_vr_n
c
c     For bnasic variables, the new activity corresponding to the upper
c     (lower) cost range is the lower (upper) bound range.
c
         co_rg_up_act_v(vr_n) = bd_rg_lo_bd_v(vr_n)
         co_rg_lo_act_v(vr_n) = bd_rg_up_bd_v(vr_n)
 320  continue
c
c     Convert the stored variable numbers for logicals into negated
c     row numbers.
c
      do 410, vr_n = 1, n_c
         if (co_rg_up_en_vr(vr_n) .gt. mx_n_c)
     &        co_rg_up_en_vr(vr_n) = mx_n_c - co_rg_up_en_vr(vr_n)
         if (co_rg_up_lv_vr(vr_n) .gt. mx_n_c)
     &        co_rg_up_lv_vr(vr_n) = mx_n_c - co_rg_up_lv_vr(vr_n)
         if (co_rg_lo_en_vr(vr_n) .gt. mx_n_c)
     &        co_rg_lo_en_vr(vr_n) = mx_n_c - co_rg_lo_en_vr(vr_n)
         if (co_rg_lo_lv_vr(vr_n) .gt. mx_n_c)
     &        co_rg_lo_lv_vr(vr_n) = mx_n_c - co_rg_lo_lv_vr(vr_n)
         if (bd_rg_up_en_vr(vr_n) .gt. mx_n_c)
     &        bd_rg_up_en_vr(vr_n) = mx_n_c - bd_rg_up_en_vr(vr_n)
         if (bd_rg_up_lv_vr(vr_n) .gt. mx_n_c)
     &        bd_rg_up_lv_vr(vr_n) = mx_n_c - bd_rg_up_lv_vr(vr_n)
         if (bd_rg_lo_en_vr(vr_n) .gt. mx_n_c)
     &        bd_rg_lo_en_vr(vr_n) = mx_n_c - bd_rg_lo_en_vr(vr_n)
         if (bd_rg_lo_lv_vr(vr_n) .gt. mx_n_c)
     &        bd_rg_lo_lv_vr(vr_n) = mx_n_c - bd_rg_lo_lv_vr(vr_n)
 410  continue
      do 420, vr_n = mx_n_c+1, mx_n_c+n_r
         if (co_rg_up_en_vr(vr_n) .gt. mx_n_c)
     &        co_rg_up_en_vr(vr_n) = mx_n_c - co_rg_up_en_vr(vr_n)
         if (co_rg_up_lv_vr(vr_n) .gt. mx_n_c)
     &        co_rg_up_lv_vr(vr_n) = mx_n_c - co_rg_up_lv_vr(vr_n)
         if (co_rg_lo_en_vr(vr_n) .gt. mx_n_c)
     &        co_rg_lo_en_vr(vr_n) = mx_n_c - co_rg_lo_en_vr(vr_n)
         if (co_rg_lo_lv_vr(vr_n) .gt. mx_n_c)
     &        co_rg_lo_lv_vr(vr_n) = mx_n_c - co_rg_lo_lv_vr(vr_n)
         if (bd_rg_up_en_vr(vr_n) .gt. mx_n_c)
     &        bd_rg_up_en_vr(vr_n) = mx_n_c - bd_rg_up_en_vr(vr_n)
         if (bd_rg_up_lv_vr(vr_n) .gt. mx_n_c)
     &        bd_rg_up_lv_vr(vr_n) = mx_n_c - bd_rg_up_lv_vr(vr_n)
         if (bd_rg_lo_en_vr(vr_n) .gt. mx_n_c)
     &        bd_rg_lo_en_vr(vr_n) = mx_n_c - bd_rg_lo_en_vr(vr_n)
         if (bd_rg_lo_lv_vr(vr_n) .gt. mx_n_c)
     &        bd_rg_lo_lv_vr(vr_n) = mx_n_c - bd_rg_lo_lv_vr(vr_n)
 420  continue
c
c     Finally un-scale the range data if the solution has been scaled.
c
      if (iand(ml_da_st_msk, ml_da_st_scl_ml_sol) .ne. 0) then
         do 510, vr_n = 1, n_c
            scl_v = scl(vr_n)
            rcp_scl_v = one/scl_v
            if (co_rg_up_co_v(vr_n) .lt. inf)
     &           co_rg_up_co_v(vr_n) = co_rg_up_co_v(vr_n)*scl_v
            if (co_rg_lo_co_v(vr_n) .gt. -inf)
     &           co_rg_lo_co_v(vr_n) = co_rg_lo_co_v(vr_n)*scl_v
            if (co_rg_up_act_v(vr_n) .lt. inf)
     &           co_rg_up_act_v(vr_n) = co_rg_up_act_v(vr_n)*rcp_scl_v
            if (co_rg_lo_act_v(vr_n) .gt. -inf)
     &           co_rg_lo_act_v(vr_n) = co_rg_lo_act_v(vr_n)*rcp_scl_v
            if (bd_rg_up_bd_v(vr_n) .lt. inf)
     &           bd_rg_up_bd_v(vr_n) = bd_rg_up_bd_v(vr_n)*rcp_scl_v
            if (bd_rg_lo_bd_v(vr_n) .gt. -inf)
     &           bd_rg_lo_bd_v(vr_n) = bd_rg_lo_bd_v(vr_n)*rcp_scl_v
 510     continue
         do 520, vr_n = mx_n_c+1, mx_n_c+n_r
            scl_v = scl(vr_n)
            rcp_scl_v = one/scl_v
            if (co_rg_up_co_v(vr_n) .lt. inf)
     &           co_rg_up_co_v(vr_n) = co_rg_up_co_v(vr_n)*scl_v
            if (co_rg_lo_co_v(vr_n) .gt. -inf)
     &           co_rg_lo_co_v(vr_n) = co_rg_lo_co_v(vr_n)*scl_v
            if (co_rg_up_act_v(vr_n) .lt. inf)
     &           co_rg_up_act_v(vr_n) = co_rg_up_act_v(vr_n)*rcp_scl_v
            if (co_rg_lo_act_v(vr_n) .gt. -inf)
     &           co_rg_lo_act_v(vr_n) = co_rg_lo_act_v(vr_n)*rcp_scl_v
            if (bd_rg_up_bd_v(vr_n) .lt. inf)
     &           bd_rg_up_bd_v(vr_n) = bd_rg_up_bd_v(vr_n)*rcp_scl_v
            if (bd_rg_lo_bd_v(vr_n) .gt. -inf)
     &           bd_rg_lo_bd_v(vr_n) = bd_rg_lo_bd_v(vr_n)*rcp_scl_v
 520     continue
      endif
c
c     Indicate that the model has ranging data and that the default is
c     to write out all the ranging data
c
      ml_da_st_msk = ior(ml_da_st_msk, ml_da_st_rg_da)
      wr_rg_da_msk = i_ct_vr_ub(ix_wr_rg_da_msk)
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tt_da .gt. 0) call ems_tt_rec(-rg_da_tt, -1)
CM      ENDIF
 7000 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8980 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9898)
     &     vr_in_c(os_struc_in_c_fx_p), n_c
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 8990 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9899)
     &     fm_il_sn_n, t_il_sn_n
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9600 format('Nonbasic variable ', i7,
     &     ' can move up and down but has non-zero dual activity ',
     &     g11.4, ': Treating this as zero')
 9601 format('Nonbasic variable ', i7,
     &     ' can move up but has negative dual activity ',
     &     g11.4, ': Treating this as zero')
 9602 format('Nonbasic variable ', i7,
     &     ' can move down but has positive dual activity ',
     &     g11.4, ': Treating this as zero')
 9800 format('Cannot get ranging data without an optimal basis')
 9801 format('Cannot get ranging data for models with ',
     &     'non-standard variables')
 9898 format('Have ', i7, ' nonbasic variables but ', i7,
     &     ' columns in model')
 9899 format('There are nonbasic variables in sections ',
     &     i2, ' to ', i2)
      end
 
C->>> ----------------------------------------------> ems_g_vr_rg_da <<<
c     Gets the ranging information for a variable.
c
      subroutine ems_g_vr_rg_da(c_n, en_vr_n, u_bc_rg_da,
     &     aa_up, aa_lo,
     &     pv_r_n_up, pv_r_n_lo,
     &     vr_in_r,
     &     pr_act, du_act,
     &     co_rg_up_co_v, co_rg_lo_co_v,
     &     co_rg_up_act_v, co_rg_lo_act_v,
     &     co_rg_up_en_vr, co_rg_lo_en_vr,
     &     co_rg_up_lv_vr, co_rg_lo_lv_vr,
     &     ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'ITXITCS.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'RSMICOM.INC'
      include 'EMSMSG.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      integer c_n, en_vr_n
      logical u_bc_rg_da
      double precision aa_up, aa_lo
      integer pv_r_n_up, pv_r_n_lo
      integer vr_in_r(0:n_r)
      double precision pr_act(0:mx_n_c+n_r), du_act(0:mx_n_c+n_r)
      double precision co_rg_up_co_v(0:mx_n_c+n_r)
      double precision co_rg_lo_co_v(0:mx_n_c+n_r)
      double precision co_rg_up_act_v(0:mx_n_c+n_r)
      double precision co_rg_lo_act_v(0:mx_n_c+n_r)
      integer co_rg_up_en_vr(0:mx_n_c+n_r)
      integer co_rg_lo_en_vr(0:mx_n_c+n_r)
      integer co_rg_up_lv_vr(0:mx_n_c+n_r)
      integer co_rg_lo_lv_vr(0:mx_n_c+n_r)
      double precision ds(0:ds_n_en_m1)
      integer is(0:is_n_en_m1)
      integer ems_pos_mod
      integer it_xit_reason, usr_rt_cod
      integer ftran_sol_n_en
      integer p_lc_pv_c_ix
      integer p_lc_pk_v
      integer rl_wk_a_ix
      integer i_wk_a_ix
c
c     Allow a user exit at this point but EMSOL will not check for any
c     changes in the problem data
c
      if (ems_pos_mod(c_n, it_usr_xit_fq) .eq. 0) then
         it_xit_reason = it_xit_no_usr_cg
         call ems_it_xit(ds, is, it_xit_reason, usr_rt_cod)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      endif
c
c     Solve for the tableau column.
c
      call ems_g_rsmi_rl_wk_a_ix(rl_wk_a_ix)
      if (rl_wk_a_ix .lt. 0) goto 8000
      call ems_g_rsmi_i_wk_a_ix(i_wk_a_ix)
      if (i_wk_a_ix .lt. 0) go to 8000
      p_lc_pk_v = p_rsmi_rl_wk_a(rl_wk_a_ix)
      p_lc_pv_c_ix = p_rsmi_i_wk_a(i_wk_a_ix)
      if (sto_ftran_ix .eq. sto_ix_y) then
         is(p_lc_pv_c_ix) = 0
      else
         is(p_lc_pv_c_ix) = n_r + 1
      endif
      call ems_g_rhs(1, en_vr_n, ds(p_pv_c_v), is(p_lc_pv_c_ix), ds, is)
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tt_da .gt. 0) call ems_tt_rec(g_vr_rg_da_ftran_tt, -1)
CM      ENDIF
      call ems_ftran(ds(p_pv_c_v), is(p_lc_pv_c_ix), ds, is)
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tt_da .gt. 0) call ems_tt_rec(-g_vr_rg_da_ftran_tt, -1)
CM      ENDIF
 
      if (is(p_lc_pv_c_ix) .le. n_r) then
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (g_tt_da .gt. 0) call ems_tt_rec(g_vr_rg_da_sps_tt, -1)
CM      ENDIF
         call ems_g_vr_rg_da_sps(
     &        c_n, en_vr_n, u_bc_rg_da, ftran_sol_n_en,
     &        aa_up, aa_lo,
     &        pv_r_n_up, pv_r_n_lo,
     &        vr_in_r, is(p_st),
     &        ds(p_rsmi_lb), ds(p_rsmi_ub),
     &        pr_act, du_act,
     &        ds(p_pv_c_v),
     &        is(p_lc_pv_c_ix),
     &        ds(p_lc_pk_v),
     &        co_rg_up_co_v, co_rg_lo_co_v,
     &        co_rg_up_act_v, co_rg_lo_act_v,
     &        co_rg_up_en_vr, co_rg_lo_en_vr,
     &        co_rg_up_lv_vr, co_rg_lo_lv_vr)
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (g_tt_da .gt. 0) call ems_tt_rec(-g_vr_rg_da_sps_tt, -1)
CM      ENDIF
      else
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (g_tt_da .gt. 0) call ems_tt_rec(g_vr_rg_da_dse_tt, -1)
CM      ENDIF
         call ems_g_vr_rg_da_dse(
     &        c_n, en_vr_n, u_bc_rg_da, ftran_sol_n_en,
     &        aa_up, aa_lo,
     &        pv_r_n_up, pv_r_n_lo,
     &        vr_in_r, is(p_st),
     &        ds(p_rsmi_lb), ds(p_rsmi_ub),
     &        pr_act, du_act,
     &        ds(p_pv_c_v),
     &        is(p_lc_pv_c_ix),
     &        ds(p_lc_pk_v),
     &        co_rg_up_co_v, co_rg_lo_co_v,
     &        co_rg_up_act_v, co_rg_lo_act_v,
     &        co_rg_up_en_vr, co_rg_lo_en_vr,
     &        co_rg_up_lv_vr, co_rg_lo_lv_vr)
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (g_tt_da .gt. 0) call ems_tt_rec(-g_vr_rg_da_dse_tt, -1)
CM      ENDIF
      endif
      if (sto_ftran_ix_mode .eq. sto_ix_poss) then
         ftran_sol_dse = float(ftran_sol_n_en)/float(n_r)
         av_ftran_sol_dse = fac_long_prd*ftran_sol_dse +
     &        (one-fac_long_prd)*av_ftran_sol_dse
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
      call ems_fr_rsmi_rl_wk_a_ix(rl_wk_a_ix)
      call ems_fr_rsmi_i_wk_a_ix(i_wk_a_ix)
 7000 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9800 format('RSMI workspace not available in ems_g_vr_rg_da')
      end
 
C->>> -----------------------------------------> ems_iz_blk_ml_rg_da <<<
c     Sets up a block and the handles for the ranging information.
c
      subroutine ems_iz_blk_ml_rg_da(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer blk_n, r_cf, c_cf, a_el_cf, cs, n_wo, p_ml_hdl
      integer mem_mgr_rt_cod
      integer rt_cod
 
      if (iand(ml_blk_st_msk,  ml_blk_st_ml_rg_da) .ne. 0) goto 8000
c      r_cf = 10*rl_wo_z + 8*i_wo_z
c      c_cf = 10*rl_wo_z + 8*i_wo_z
c      a_el_cf = 0
c      cs = 10*rl_wo_z + 8*i_wo_z
      call ems_g_blk_ml_rg_da_n_wo(r_cf, c_cf, a_el_cf, cs, n_wo)
      call ems_mem_mgr_ope_blk(mem_mgr_rt_cod, is,
     &     n_wo, ope_blk_at_f_fr_p,
     &     cu_ml_n, ml_rg_da_blk_id, blk_n)
      if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
         if (mem_mgr_rt_cod .eq. mem_mgr_rt_cod_serious_no_po) then
            ems_msg_cod = ems_msg_lvl_serious
            go to 7000
         else if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
             ems_msg_cod = ems_msg_lvl_serious
            go to 7000
         endif
      endif
CM      IF (emsol_dev .EQ. 1) THEN
C?      call ems_mem_mgr_rp_ope_blk(
C?     &     mem_mgr_rt_cod, is, -1,
C?     &     r_cf, n_r,
C?     &     c_cf, mx_n_c,
C?     &     a_el_cf, n_a_el,
C?     &     cs, n_wo, ml_rg_da_blk_id)
C?      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
C?         ems_msg_cod = ems_msg_lvl_serious
C?         go to 7000
C?      endif
CM      ENDIF
c
c     Get handles for the arrays within the block.
c
      p_ml_hdl = p_ml_bs_blk + (cu_ml_n-1)*ml_bs_blk_n_wo +
     &     ml_bs_blk_os_hdl
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+mx_n_c+n_r, rl_wo_z,
     &     is(p_ml_hdl+ix_hdl_co_rg_up_co_v))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+mx_n_c+n_r, rl_wo_z,
     &     is(p_ml_hdl+ix_hdl_co_rg_lo_co_v))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+mx_n_c+n_r, rl_wo_z,
     &     is(p_ml_hdl+ix_hdl_co_rg_up_ob_v))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+mx_n_c+n_r, rl_wo_z,
     &     is(p_ml_hdl+ix_hdl_co_rg_lo_ob_v))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+mx_n_c+n_r, rl_wo_z,
     &     is(p_ml_hdl+ix_hdl_co_rg_up_act_v))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+mx_n_c+n_r, rl_wo_z,
     &     is(p_ml_hdl+ix_hdl_co_rg_lo_act_v))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+mx_n_c+n_r, rl_wo_z,
     &     is(p_ml_hdl+ix_hdl_bd_rg_up_bd_v))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+mx_n_c+n_r, rl_wo_z,
     &     is(p_ml_hdl+ix_hdl_bd_rg_lo_bd_v))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+mx_n_c+n_r, rl_wo_z,
     &     is(p_ml_hdl+ix_hdl_bd_rg_up_ob_v))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+mx_n_c+n_r, rl_wo_z,
     &     is(p_ml_hdl+ix_hdl_bd_rg_lo_ob_v))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+mx_n_c+n_r, i_wo_z,
     &     is(p_ml_hdl+ix_hdl_co_rg_up_en_vr))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+mx_n_c+n_r, i_wo_z,
     &     is(p_ml_hdl+ix_hdl_co_rg_lo_en_vr))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+mx_n_c+n_r, i_wo_z,
     &     is(p_ml_hdl+ix_hdl_co_rg_up_lv_vr))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+mx_n_c+n_r, i_wo_z,
     &     is(p_ml_hdl+ix_hdl_co_rg_lo_lv_vr))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+mx_n_c+n_r, i_wo_z,
     &     is(p_ml_hdl+ix_hdl_bd_rg_up_en_vr))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+mx_n_c+n_r, i_wo_z,
     &     is(p_ml_hdl+ix_hdl_bd_rg_lo_en_vr))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+mx_n_c+n_r, i_wo_z,
     &     is(p_ml_hdl+ix_hdl_bd_rg_up_lv_vr))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+mx_n_c+n_r, i_wo_z,
     &     is(p_ml_hdl+ix_hdl_bd_rg_lo_lv_vr))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_g_ml_p(rt_cod, is)
      if (rt_cod .ne. 0) goto 8020
c
c     Indicate that the model has space for ranging information.
c
      ml_blk_st_msk = ml_blk_st_msk + ml_blk_st_ml_rg_da
 7000 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9800 format('Model already has space for ranging information')
 9802 format('Error in ems_g_ml_p')
      end
 
C->>> -------------------------------------> ems_g_blk_ml_rg_da_n_wo <<<
      subroutine ems_g_blk_ml_rg_da_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      integer r_cf, c_cf, a_el_cf, cs, n_wo
      r_cf = 10*rl_wo_z + 8*i_wo_z
      c_cf = 10*rl_wo_z + 8*i_wo_z
      a_el_cf = 0
      cs =   10*rl_wo_z + 8*i_wo_z
      n_wo = r_cf*n_r + c_cf*mx_n_c + a_el_cf*n_a_el + cs
      return
      end
 
C->>> -----------------------------------------> ems_rm_blk_ml_rg_da <<<
c     Removes the block for the ranging information.
c
      subroutine ems_rm_blk_ml_rg_da(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer blk_n, p_ml_hdl
      integer mem_mgr_rt_cod
 
      if (iand(ml_blk_st_msk, ml_blk_st_ml_rg_da) .eq. 0) goto 8000
      p_ml_hdl = p_ml_bs_blk + (cu_ml_n-1)*ml_bs_blk_n_wo +
     &     ml_bs_blk_os_hdl
      blk_n = is(p_ml_hdl+ix_blk_ml_rg_da)
      call ems_mem_mgr_rm_blk(mem_mgr_rt_cod, is,
     &     blk_n)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         goto 7100
      endif
      ml_blk_st_msk = ml_blk_st_msk - ml_blk_st_ml_rg_da
 7000 continue
 7100 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9800 format(
     &     'Model does not already have space for ranging information')
      end
 
 
