      subroutine ems_rp_rsmi_er(ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      double precision ds(0:ds_n_en_m1)
      double precision mx_pr_ifs, mx_rlv_pr_ifs
      double precision mx_du_ifs, mx_rlv_du_ifs
      double precision pr_act_rsdu_norm, rlv_pr_act_rsdu_norm
      double precision bc_co_rsdu_norm, rlv_bc_co_rsdu_norm
      double precision non_bc_co_rsdu_norm, rlv_non_bc_co_rsdu_norm
      double precision du_act_rsdu_norm, rlv_du_act_rsdu_norm
      integer rl_wk_a_ix
 
      call ems_ca_g_n_su_mx_pr_ifs(
     &     n_pr_ifs, su_pr_ifs, mx_pr_ifs, mx_rlv_pr_ifs, ds, is)
      call ems_ca_g_n_su_mx_du_ifs(
     &     n_du_ifs, su_du_ifs, mx_du_ifs, mx_rlv_du_ifs, ds, is)
 
      call ems_g_rsmi_rl_wk_a_ix(rl_wk_a_ix)
      if (rl_wk_a_ix .lt. 0) goto 8000
      call ems_g_pr_act_rsdu_norm(
     &     ds(p_pr_act),
     &     is(p_st),
     &     ds(p_mtx_r_v),
     &     is(p_mtx_r_ix),
     &     is(p_mtx_c_sa),
     &     ds(p_rsmi_rl_wk_a(rl_wk_a_ix)),
     &     pr_act_rsdu_norm, rlv_pr_act_rsdu_norm)
      call ems_fr_rsmi_rl_wk_a_ix(rl_wk_a_ix)
 
      call ems_g_co_rsdu_norm(
     &     ds(p_du_act),
     &     ds(p_mtx_r_v),
     &     is(p_mtx_r_ix),
     &     is(p_mtx_c_sa),
     &     ds, is,
     &     bc_co_rsdu_norm, rlv_bc_co_rsdu_norm,
     &     non_bc_co_rsdu_norm, rlv_non_bc_co_rsdu_norm)
 
      call ems_g_du_act_rsdu_norm(
     &     ds(p_du_act),
     &     is(p_vr_in_r),
     &     ds(p_rsmi_co),
     &     ds(p_mtx_r_v),
     &     is(p_mtx_r_ix),
     &     is(p_mtx_c_sa),
     &     du_act_rsdu_norm, rlv_du_act_rsdu_norm)
 
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
      call ems_msg_wr_li(info_msg_n)
 
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9001)
     &     'Maximum primal infeasibility',
     &     mx_pr_ifs, mx_rlv_pr_ifs
      if (mx_pr_ifs .gt. tl_pr_ifs) then
         call ems_msg_wr_li(warn_msg_n)
      else
         call ems_msg_wr_li(info_msg_n)
      endif
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9001)
     &     'Maximum dual infeasibility',
     &     mx_du_ifs, mx_rlv_du_ifs
      if (mx_du_ifs .gt. tl_du_ifs) then
         call ems_msg_wr_li(warn_msg_n)
      else
         call ems_msg_wr_li(info_msg_n)
      endif
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9001)
     &     'Primal activity residual',
     &     pr_act_rsdu_norm, rlv_pr_act_rsdu_norm
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9001)
     &     'Basic cost residual',
     &     bc_co_rsdu_norm, rlv_bc_co_rsdu_norm
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9001)
     &     'Nonbasic cost residual',
     &     non_bc_co_rsdu_norm, rlv_non_bc_co_rsdu_norm
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9001)
     &     'Dual activity residual',
     &     du_act_rsdu_norm, rlv_du_act_rsdu_norm
      call ems_msg_wr_li(info_msg_n)
 
 7000 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9000 format(20x, '   Measure   Absolute     Relative')
 9001 format(a30, 2(2x, g11.4))
 9800 format('RSMI workspace not available in ems_rp_rsmi_er')
      end
 
c->>> -----------------------------------> ems_g_rand_tran_rsdu_norm <<<
c     Checks an INVERT by solving Ax=b and A^Tx=b for x either
c
c     -- with components from a uniform distribution on [0,1] (rand = T)
c
c     -- a random unit vector (rand = F)
c
      subroutine ems_g_rand_tran_rsdu_norm(
     &     rand,
     &     vr_in_r,
     &     mtx_r_v, mtx_r_ix, mtx_c_sa,
     &     rhs, sol,
     &     ftran_rsdu_norm, btran_rsdu_norm,
     &     is, ds)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      logical rand
      integer vr_in_r(0:n_r)
      integer mtx_r_ix(0:n_a_el)
      integer mtx_c_sa(0:n_c+1)
      integer is(0:is_n_en_m1)
      double precision mtx_r_v(0:n_a_el)
      double precision rhs(0:n_r)
      double precision sol(0:n_r)
      double precision ds(0:ds_n_en_m1)
      double precision ftran_rsdu_norm, btran_rsdu_norm
      integer r_n, vr_n, c_n, el_n
      double precision rhs_v, rsdu
      integer rhs_ix
      double precision ems_drand
      integer rand_i
      double precision rand_v
 
      do 10, r_n = 1, n_r
         rhs(r_n) = zero
         sol(r_n) = zero
 10   continue
      rand_v = ems_drand(1)
      if (rand) then
         do 15, r_n = 1, n_r
            rand_v = ems_drand(0)
            sol(r_n) = rand_v
 15      continue
      else
         rand_i = min(int(rand_v*n_r)+1, n_r)
         sol(rand_i+1) = one
      endif
      do 30, c_n = 1, n_r
         vr_n = vr_in_r(c_n)
         if (vr_n .le. mx_n_c) then
            do 20, el_n = mtx_c_sa(vr_n), mtx_c_sa(vr_n+1)-1
               r_n = mtx_r_ix(el_n)
               rhs(r_n) = rhs(r_n) + mtx_r_v(el_n)*sol(c_n)
 20         continue
         else
            r_n = vr_n - mx_n_c
            rhs(r_n) = rhs(r_n) - sol(c_n)
         endif
 30   continue
      rhs_ix = n_r+1
      call ems_ftran(rhs, rhs_ix, ds, is)
      ftran_rsdu_norm = zero
      do 40, r_n = 1, n_r
         rsdu = rhs(r_n) + sol(r_n)
         ftran_rsdu_norm = ftran_rsdu_norm + rsdu*rsdu
 40   continue
      ftran_rsdu_norm = sqrt(ftran_rsdu_norm)
c      if (ftran_rsdu_norm .gt. 1d-4) then
c         er_fd = .true.
c         print*, '*** RANDOM FTRAN residual error of ', ftran_rsdu_norm
c      endif
c
c     Check BTRAN
c
      do 130, c_n = 1, n_r
         rhs_v = zero
         vr_n = vr_in_r(c_n)
         if (vr_n .le. mx_n_c) then
            do 120, el_n = mtx_c_sa(vr_n), mtx_c_sa(vr_n+1)-1
               r_n = mtx_r_ix(el_n)
               rhs_v = rhs_v + mtx_r_v(el_n)*sol(r_n)
 120        continue
         else
            r_n = vr_n - mx_n_c
            rhs_v = rhs_v - sol(r_n)
         endif
         rhs(c_n) = rhs_v
 130  continue
      rhs_ix = n_r+1
      call ems_btran(rhs, rhs_ix, ds, is)
      btran_rsdu_norm = zero
      do 140, r_n = 1, n_r
         rsdu = rhs(r_n) + sol(r_n)
         btran_rsdu_norm = btran_rsdu_norm + rsdu*rsdu
         rhs(r_n) = zero
         sol(r_n) = zero
 140  continue
      btran_rsdu_norm = sqrt(btran_rsdu_norm)
c      if (btran_rsdu_norm .gt. 1d-4) then
c         er_fd = .true.
c         print*, '*** RANDOM BTRAN residual error of ', btran_rsdu_norm
c      endif
      return
      end
 
C->>> --------------------------------------> ems_g_pr_act_rsdu_norm <<<
c     Gets the norm of the absolute primal activity residuals and the
c     norm relative to that of the nonbasic primal activities.
c
      subroutine ems_g_pr_act_rsdu_norm(pr_act, st,
     &     mtx_r_v, mtx_r_ix, mtx_c_sa, rl_c_wk_vec,
     &     pr_act_rsdu_norm, rlv_pr_act_rsdu_norm)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      double precision pr_act(0:mx_n_c+n_r)
      integer st(0:mx_n_c+n_r)
      integer mtx_r_ix(0:n_a_el), mtx_c_sa(0:n_c+1)
      double precision mtx_r_v(0:n_a_el)
      double precision rl_c_wk_vec(0:n_r)
      double precision pr_act_rsdu_norm, rlv_pr_act_rsdu_norm
      integer r_n, c_n, vr_n, el_n
      double precision pr_act_v
      double precision pr_act_rsdu
      double precision non_bc_pr_act_norm
 
      pr_act_rsdu_norm = zero
      non_bc_pr_act_norm = zero
      call ems_cp_rl_a(n_r, zero, rl_c_wk_vec(1), 0)
      do 30, c_n = 1, n_c
         vr_n = c_n
         pr_act_v = pr_act(vr_n)
         if (pr_act_v .eq. zero) goto 30
         if (iand(st(vr_n), bc_bt) .eq. 0)
     &        non_bc_pr_act_norm =
     &        non_bc_pr_act_norm + pr_act_v*pr_act_v
         do 20, el_n = mtx_c_sa(c_n), mtx_c_sa(c_n+1)-1
            r_n = mtx_r_ix(el_n)
            rl_c_wk_vec(r_n) =
     &           rl_c_wk_vec(r_n) + pr_act_v*mtx_r_v(el_n)
 20      continue
 30   continue
      do 40, r_n = 1, n_r
         vr_n = mx_n_c+r_n
         pr_act_v = pr_act(vr_n)
         if (iand(st(vr_n), bc_bt) .eq. 0)
     &        non_bc_pr_act_norm =
     &        non_bc_pr_act_norm + pr_act_v*pr_act_v
         pr_act_rsdu = rl_c_wk_vec(r_n) - pr_act(mx_n_c+r_n)
         pr_act_rsdu_norm =
     &        pr_act_rsdu_norm + pr_act_rsdu*pr_act_rsdu
 40   continue
      non_bc_pr_act_norm = sqrt(non_bc_pr_act_norm)
      pr_act_rsdu_norm = sqrt(pr_act_rsdu_norm)
      rlv_pr_act_rsdu_norm =
     &     pr_act_rsdu_norm/max(tl_pr_ifs, non_bc_pr_act_norm)
      return
      end
 
C->>> --------------------------------------> ems_g_du_act_rsdu_norm <<<
c     Gets the norm of the absolute dual activity residuals and the norm
c     relative to that of the basic costs.
c
      subroutine ems_g_du_act_rsdu_norm(
     &     du_act, vr_in_r, rsmi_co,
     &     mtx_r_v, mtx_r_ix, mtx_c_sa,
     &     du_act_rsdu_norm, rlv_du_act_rsdu_norm)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      double precision du_act(0:mx_n_c+n_r)
      double precision rsmi_co(0:mx_n_c+n_r)
      integer vr_in_r(0:n_r)
      integer mtx_r_ix(0:n_a_el)
      integer mtx_c_sa(0:n_c+1)
      double precision mtx_r_v(0:n_a_el)
      double precision du_act_rsdu_norm, rlv_du_act_rsdu_norm
      integer r_n, c_n, vr_n, el_n
      double precision du_act_v
      double precision du_act_rsdu
      double precision bc_co_norm
c
c     Calculate |A^T\lambda-\pi-c|
c
      du_act_rsdu_norm = zero
      do 30, c_n = 1, n_c
         du_act_rsdu = mx_mn*rsmi_co(c_n) - du_act(c_n)
         do 20, el_n = mtx_c_sa(c_n), mtx_c_sa(c_n+1)-1
            r_n = mtx_r_ix(el_n)
            du_act_v = du_act(mx_n_c+r_n)
            du_act_rsdu = du_act_rsdu - du_act_v*mtx_r_v(el_n)
 20      continue
         du_act_rsdu_norm = du_act_rsdu_norm + du_act_rsdu*du_act_rsdu
 30   continue
c
c     Calculate |c_B|
c
      bc_co_norm = zero
      do 40, r_n = 1, n_r
         vr_n = vr_in_r(r_n)
         bc_co_norm = bc_co_norm + rsmi_co(vr_n)*rsmi_co(vr_n)
 40   continue
      bc_co_norm = sqrt(bc_co_norm)
      du_act_rsdu_norm = sqrt(du_act_rsdu_norm)
c
c     Calculate |A^T\lambda-\pi|/|c_B|
c
      rlv_du_act_rsdu_norm = du_act_rsdu_norm/max(tl_du_ifs, bc_co_norm)
      return
      end
 
C->>> ------------------------------------------> ems_g_co_rsdu_norm <<<
c     Gets the norm of the absolute basic and nonbasic cost residuals
c     and the norm relative to that of the basic costs and all costs
c     respectively.
c
      subroutine ems_g_co_rsdu_norm(
     &     du_act,
     &     mtx_r_v, mtx_r_ix, mtx_c_sa,
     &     ds, is,
     &     bc_co_rsdu_norm, rlv_bc_co_rsdu_norm,
     &     non_bc_co_rsdu_norm, rlv_non_bc_co_rsdu_norm)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'RSMICOM.INC'
      include 'EMSMSG.INC'
      include 'EMSMSGN.INC'
      double precision du_act(0:mx_n_c+n_r)
      double precision ds(0:ds_n_en_m1)
      double precision bc_co_rsdu_norm
      double precision rlv_bc_co_rsdu_norm
      double precision non_bc_co_rsdu_norm
      double precision rlv_non_bc_co_rsdu_norm
      integer mtx_r_ix(0:n_a_el)
      integer mtx_c_sa(0:n_c+1)
      integer is(0:is_n_en_m1)
      double precision mtx_r_v(0:n_a_el)
      integer r_n, c_n, vr_n, el_n, vr_st
      double precision co_v
      double precision bc_co_rsdu
      double precision bc_co_norm
      double precision non_bc_co_rsdu
      double precision co_norm
c
c     Form the full pi = B^{-T}c_B, using iterative refinement
c
      call ems_g_rfn_fu_pi(ds, is)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
c
c     Form the residual of c_B^T = pi^T.B
c
      bc_co_norm = zero
      bc_co_rsdu_norm = zero
      do 20, r_n = 1, n_r
         vr_n = is(p_vr_in_r+r_n)
         co_v = ds(p_bc_co_v+r_n)
         bc_co_norm = bc_co_norm + co_v*co_v
         bc_co_rsdu = co_v
         if (vr_n .le. n_c) then
            do 10, el_n = mtx_c_sa(vr_n), mtx_c_sa(vr_n+1)-1
               bc_co_rsdu =
     &              bc_co_rsdu + ds(p_pi_v+mtx_r_ix(el_n))*mtx_r_v(el_n)
 10         continue
         else
            bc_co_rsdu = bc_co_rsdu - ds(p_pi_v+r_n)
         endif
         bc_co_rsdu_norm = bc_co_rsdu_norm + bc_co_rsdu*bc_co_rsdu
 20   continue
c
c     Form the residual of c_N^T = pi^T.N - \hat{c}_N^T
c
      co_norm = bc_co_norm
      non_bc_co_rsdu_norm = zero
      do 120, c_n = 1, n_c
         vr_n = c_n
         vr_st = is(p_st+vr_n)
         if (iand(vr_st, bc_bt) .ne. 0) goto 120
         if (iand(vr_st, ifs_bt) .eq. 0) then
            co_v =           pr_co_mu*ds(p_rsmi_co+vr_n)
         else
            if (iand(vr_st, up_bt) .ne. 0) then
               co_v = -one + pr_co_mu*ds(p_rsmi_co+vr_n)
            else
               co_v =  one + pr_co_mu*ds(p_rsmi_co+vr_n)
            endif
         endif
         co_norm = co_norm + co_v*co_v
         non_bc_co_rsdu = co_v
         do 110, el_n = mtx_c_sa(c_n), mtx_c_sa(c_n+1)-1
            r_n = mtx_r_ix(el_n)
            non_bc_co_rsdu =
     &           non_bc_co_rsdu + ds(p_pi_v+r_n)*mtx_r_v(el_n)
 110     continue
         non_bc_co_rsdu = non_bc_co_rsdu - du_act(vr_n)
         non_bc_co_rsdu_norm =
     &        non_bc_co_rsdu_norm + non_bc_co_rsdu*non_bc_co_rsdu
 120  continue
      do 130, r_n = 1, n_r
         vr_n = mx_n_c+r_n
         vr_st = is(p_st+vr_n)
         if (iand(vr_st, bc_bt) .ne. 0) goto 125
         if (iand(vr_st, ifs_bt) .eq. 0) then
            co_v =           pr_co_mu*ds(p_rsmi_co+vr_n)
         else
            if (iand(vr_st, up_bt) .ne. 0) then
               co_v = -one + pr_co_mu*ds(p_rsmi_co+vr_n)
            else
               co_v =  one + pr_co_mu*ds(p_rsmi_co+vr_n)
            endif
         endif
         co_norm = co_norm + co_v*co_v
         non_bc_co_rsdu = co_v
         non_bc_co_rsdu = non_bc_co_rsdu - ds(p_pi_v+r_n)
         non_bc_co_rsdu = non_bc_co_rsdu - du_act(vr_n)
         non_bc_co_rsdu_norm =
     &        non_bc_co_rsdu_norm + non_bc_co_rsdu*non_bc_co_rsdu
 125     continue
c
c     Zero the entry in pi
c
        ds(p_pi_v+r_n) = zero
 130  continue
c
c     Indicate that pi has been zeroed
c
      is(p_pi_ix) = 0
c
c     Work out the norms
c
      co_norm = sqrt(co_norm)
      bc_co_norm = sqrt(bc_co_norm)
      bc_co_rsdu_norm = sqrt(bc_co_rsdu_norm)
      rlv_bc_co_rsdu_norm =
     &     bc_co_rsdu_norm/max(tl_du_ifs, bc_co_norm)
      non_bc_co_rsdu_norm = sqrt(non_bc_co_rsdu_norm)
      rlv_non_bc_co_rsdu_norm =
     &     non_bc_co_rsdu_norm/max(tl_du_ifs, co_norm)
 7000 continue
      return
      end
 
