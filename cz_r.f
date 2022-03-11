CM
C->>> ----------------------------------------------------> ems_cz_r <<<
      subroutine ems_cz_r(
     &     rp_growth, refined_pv_c, refine_pv_c, mx_ed_wt_er,
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
c      include 'RSMICS.INC'
      include 'RSMICOM.INC'
c      include 'MORSMI.INC'
      include 'ICTVR.INC'
c      include 'RLCTVR.INC'
c      include 'EMSMSG.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      logical rp_growth, refined_pv_c, refine_pv_c
      double precision mx_ed_wt_er
      integer st(0:mx_n_c+n_r)
      integer vr_in_r(0:n_r), nw_eta_ix(0:n_r)
      integer cdd_ix(0:1+n_r), is(0:is_n_en_m1)
      double precision rsmi_lb(0:mx_n_c+n_r)
      double precision rsmi_ub(0:mx_n_c+n_r)
      double precision pr_act(0:mx_n_c+n_r)
      double precision nw_eta_v(0:n_r), pv_c_v(0:n_r)
      double precision ds(0:ds_n_en_m1)
      integer sv_n_ix
 
      sv_n_ix = nw_eta_ix(0)
      if (sv_n_ix .le. n_r
     &     .and. u_bs .eq. u_bs_pf_r_cp
     &     .and. sv_n_ix .gt. ord_i_ls_stp_p1
     &     ) then
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_cz_r_lvl0) call ems_tt_rec(sort_pv_c_ix_tt, n_bs)
CM      ENDIF
c
c     If the dense row-wise copy of the update eta entries in pivotal
c     rows is to be maintained then it is necssary for the list of
c     nonzeros in the pivotal column to be ordered (if it contains more
c     then ord_i_ls_stp_p1 nonzeros) so have to sort the nonzeros now.
c     This costs O(log_2(n_nz)) which is considerably less than the
c     number of rows, so long as the limit on RHS density for sparse
c     FTRAN is not too high (Default 10%) ie compare O(log_2(n_r/10))
c     with n_r.
c
c     Have to set nw_eta_ix(0) .ne. 1 to indicate that the values are
c     completely unordered...
c
         nw_eta_ix(0) = 0
         call ems_incr_heap_srt_i_no_ix(sv_n_ix, nw_eta_ix)
c
c     ... and recover the original value
c
         nw_eta_ix(0) = sv_n_ix
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_cz_r_lvl0) call ems_tt_rec(-sort_pv_c_ix_tt, n_bs)
CM      ENDIF
      endif
      if (pc_alg .eq. pc_alg_dan) then
         if (iand(cz_r_msk, cz_r_l1_bt) .eq. 0) then
            if (nw_eta_ix(0) .le. n_r) then
               call ems_dan_sps_cz_r(
     &              rp_growth, refined_pv_c, refine_pv_c,
     &              rsmi_lb, rsmi_ub, pr_act, st, vr_in_r,
     &              nw_eta_v, pv_c_v, nw_eta_ix,
     &              cdd_ix,
     &              ds, is)
            else
               call ems_dan_cz_r(
     &              rp_growth, refined_pv_c, refine_pv_c,
     &              rsmi_lb, rsmi_ub, pr_act, st, vr_in_r,
     &              nw_eta_v, pv_c_v, nw_eta_ix,
     &              cdd_ix,
     &              ds, is)
            endif
         else
            if (nw_eta_ix(0) .le. n_r) then
               call ems_dan_sps_l1_cz_r(
     &              rp_growth, refined_pv_c, refine_pv_c,
     &              rsmi_lb, rsmi_ub, pr_act, st, vr_in_r,
     &              nw_eta_v, pv_c_v, nw_eta_ix,
     &              cdd_ix,
     &              ds(p_l1_cz_r_bp),
     &              ds(p_l1_cz_r_dl_gd),
     &              is(p_l1_cz_r_ix),
     &              ds, is)
            else
               call ems_dan_l1_cz_r(
     &              rp_growth, refined_pv_c, refine_pv_c,
     &              rsmi_lb, rsmi_ub, pr_act, st, vr_in_r,
     &              nw_eta_v, pv_c_v, nw_eta_ix,
     &              cdd_ix,
     &              ds(p_l1_cz_r_bp),
     &              ds(p_l1_cz_r_dl_gd),
     &              is(p_l1_cz_r_ix),
     &              ds, is)
            endif
         endif
      else if (pc_alg .eq. pc_alg_approx_dvx .or.
     &        pc_alg .eq. pc_alg_exact_dvx) then
         if (iand(cz_r_msk, cz_r_l1_bt) .eq. 0) then
            if (nw_eta_ix(0) .le. n_r) then
               call ems_dvx_sps_cz_r(
     &              rp_growth, refined_pv_c, refine_pv_c,
     &              ds(p_ed_wt),
     &              is(p_dvx_ix),
     &              rsmi_lb, rsmi_ub, pr_act, st, vr_in_r,
     &              nw_eta_v, pv_c_v, nw_eta_ix,
     &              cdd_ix,
     &              ds, is)
            else
               call ems_dvx_cz_r(
     &              rp_growth, refined_pv_c, refine_pv_c,
     &              ds(p_ed_wt),
     &              is(p_dvx_ix),
     &              rsmi_lb, rsmi_ub, pr_act, st, vr_in_r,
     &              nw_eta_v, pv_c_v, nw_eta_ix,
     &              cdd_ix,
     &              ds, is)
            endif
         else
            if (nw_eta_ix(0) .le. n_r) then
               call ems_dvx_sps_l1_cz_r(
     &              rp_growth, refined_pv_c, refine_pv_c,
     &              ds(p_ed_wt),
     &              is(p_dvx_ix),
     &              rsmi_lb, rsmi_ub, pr_act, st, vr_in_r,
     &              nw_eta_v, pv_c_v, nw_eta_ix,
     &              cdd_ix,
     &              ds(p_l1_cz_r_bp),
     &              ds(p_l1_cz_r_dl_gd),
     &              is(p_l1_cz_r_ix),
     &              ds, is)
            else
               call ems_dvx_l1_cz_r(
     &              rp_growth, refined_pv_c, refine_pv_c,
     &              ds(p_ed_wt),
     &              is(p_dvx_ix),
     &              rsmi_lb, rsmi_ub, pr_act, st, vr_in_r,
     &              nw_eta_v, pv_c_v, nw_eta_ix,
     &              cdd_ix,
     &              ds(p_l1_cz_r_bp),
     &              ds(p_l1_cz_r_dl_gd),
     &              is(p_l1_cz_r_ix),
     &              ds, is)
            endif
         endif
      else
         if (iand(cz_r_msk, cz_r_l1_bt) .eq. 0) then
            if (nw_eta_ix(0) .le. n_r) then
               call ems_sed_sps_cz_r(
     &              rp_growth, refined_pv_c, refine_pv_c,
     &              mx_ed_wt_er,
     &              ds(p_ed_wt),
     &              rsmi_lb, rsmi_ub, pr_act, st, vr_in_r,
     &              nw_eta_v, pv_c_v, nw_eta_ix,
     &              cdd_ix,
     &              ds, is)
            else
               call ems_sed_cz_r(
     &              rp_growth, refined_pv_c, refine_pv_c,
     &              mx_ed_wt_er,
     &              ds(p_ed_wt),
     &              rsmi_lb, rsmi_ub, pr_act, st, vr_in_r,
     &              nw_eta_v, pv_c_v, nw_eta_ix,
     &              cdd_ix,
     &              ds, is)
            endif
         else
            if (nw_eta_ix(0) .le. n_r) then
               call ems_sed_sps_l1_cz_r(
     &              rp_growth, refined_pv_c, refine_pv_c,
     &              mx_ed_wt_er,
     &              ds(p_ed_wt),
     &              rsmi_lb, rsmi_ub, pr_act, st, vr_in_r,
     &              nw_eta_v, pv_c_v, nw_eta_ix,
     &              cdd_ix,
     &              ds(p_l1_cz_r_bp),
     &              ds(p_l1_cz_r_dl_gd),
     &              is(p_l1_cz_r_ix),
     &              ds, is)
            else
               call ems_sed_l1_cz_r(
     &              rp_growth, refined_pv_c, refine_pv_c,
     &              mx_ed_wt_er,
     &              ds(p_ed_wt),
     &              rsmi_lb, rsmi_ub, pr_act, st, vr_in_r,
     &              nw_eta_v, pv_c_v, nw_eta_ix,
     &              cdd_ix,
     &              ds(p_l1_cz_r_bp),
     &              ds(p_l1_cz_r_dl_gd),
     &              is(p_l1_cz_r_ix),
     &              ds, is)
            endif
         endif
      endif
      return
      end
 
C->>> -------------------------------------> ems_consider_rpt_rao_ts <<<
c     Consider repeating the ratio test due to a negative step with
c     expanded bounds. Returns
c     rpt = -5 if the limit on repeats as been reached.
c     rpt = -3 if the -ve residual is unlikely to be due to rounding.
c     rpt =  1 if the ratio test is to be repeated.
c
      subroutine ems_consider_rpt_rao_ts(
     &        rpt, n_rpt, pv, vr_n, aa,
     &        st, rsmi_lb, rsmi_ub, pr_act, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMICS.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      integer rpt, n_rpt, vr_n
      double precision pv, aa
      integer st(0:mx_n_c+n_r), is(0:is_n_en_m1)
      double precision rsmi_lb(0:mx_n_c+n_r)
      double precision rsmi_ub(0:mx_n_c+n_r)
      double precision pr_act(0:mx_n_c+n_r)
      double precision ds(0:ds_n_en_m1)
      double precision rsdu
c
c     A negative step with expanded bounds is very surprising: it
c     suggests that a variable which was considered to be feasible (to
c     within the feasibility tolerance actually violates its bounds by
c     more than that (by > xp_tau in the case of EXPAND). Take
c     corrective action, avoiding moving into Phase I or resetting the
c     problem if possible.
c
c
      if (iand(st(vr_n), ifs_bt) .eq. 0) then
         if (pv .gt. zero) then
c
c     Negative ratio and positive pivot => negative residual.
c
            rsdu = (rsmi_ub(vr_n)-pr_act(vr_n))+tl_pr_ifs
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9200)
     &           n_si_it, '    UB',
     &           vr_n, rsmi_ub(vr_n), pr_act(vr_n),
     &           tl_pr_ifs, rsdu, pv, aa
            call ems_msg_wr_li(warn_msg_n)
            call ems_ca_rp_1_vr_st(1, vr_n, ds, is)
            if (-rsdu .lt. xp_tau) then
c
c     A small negative residual is likely to be due to rounding when
c     updating the RHS. Expand the tolerance further by xp_tau-rsdu.
c
               tl_pr_ifs = tl_pr_ifs + (xp_tau-rsdu)
               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9270)
     &              n_si_it,
     &              xp_tau-rsdu, tl_pr_ifs
            else
c
c     Large negative residual is unlikely to be due to rounding when
c     updating the RHS. (For example if a perturbed zero had been used
c     as a pivot.) Report an algorithmic error---which will result in
c     resetting RSMI.
c
               goto 8003
            end if
         else
c
c     Negative ratio and negative pivot => Positive residual
c
            rsdu = (rsmi_lb(vr_n)-pr_act(vr_n))-tl_pr_ifs
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9200)
     &           n_si_it, '    LB',
     &           vr_n, rsmi_lb(vr_n), pr_act(vr_n),
     &           tl_pr_ifs, rsdu, pv, aa
            call ems_msg_wr_li(warn_msg_n)
            call ems_ca_rp_1_vr_st(1, vr_n, ds, is)
            if (rsdu .lt. xp_tau) then
               tl_pr_ifs = tl_pr_ifs + (xp_tau+rsdu)
               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9270)
     &              n_si_it,
     &              xp_tau+rsdu, tl_pr_ifs
            else
               goto 8003
            end if
         end if
      else
         if (pv .gt. zero) then
            rsdu = (rsmi_lb(vr_n)-pr_act(vr_n))-tl_pr_ifs
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9200)
     &           n_si_it, 'IFS_LB',
     &           vr_n, rsmi_lb(vr_n), pr_act(vr_n),
     &           tl_pr_ifs, rsdu, pv, aa
            call ems_msg_wr_li(warn_msg_n)
            call ems_ca_rp_1_vr_st(1, vr_n, ds, is)
            if (-rsdu .lt. xp_tau) then
               tl_pr_ifs = tl_pr_ifs + (xp_tau-rsdu)
               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9270)
     &              n_si_it,
     &              xp_tau-rsdu, tl_pr_ifs
            else
               goto 8003
            end if
         else
            rsdu = (rsmi_ub(vr_n)-pr_act(vr_n))+tl_pr_ifs
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9200)
     &           n_si_it, 'IFS_UB',
     &           vr_n, rsmi_ub(vr_n), pr_act(vr_n),
     &           tl_pr_ifs, rsdu, pv, aa
            call ems_msg_wr_li(warn_msg_n)
            call ems_ca_rp_1_vr_st(1, vr_n, ds, is)
            if (rsdu .lt. xp_tau) then
               tl_pr_ifs = tl_pr_ifs + (xp_tau+rsdu)
               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9270)
     &              n_si_it,
     &              xp_tau+rsdu, tl_pr_ifs
            else
               goto 8003
            end if
         endif
      end if
c
c     Repeat the ratio test.
c
      n_rpt = n_rpt + 1
      if (n_rpt .ge. 10) then
         rpt = -5
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9300)
     &        n_si_it, aa, n_rpt
         call ems_msg_wr_li(warn_msg_n)
         rpt = 1
      endif
 7000 continue
      return
 8003 continue
      rpt = -3
      goto 7000
 9200 format('Iteration ', i7, ':', a6,
     &     ' vr_n, bd, pr_act, tl, rsdu, pv, aa: ', i7, 6(1x, g11.4))
 9270 format('Iteration ', i7,
     &     ': Negative ratio with expanded bounds: ',
     &     'Expand the primal tolerance by ', g11.4, ' to', g11.4)
 9300 format('Iteration ', i7,
     &     ': Negative ratio = ', g11.4, ' with expanded bounds: ',
     &     ': Repeat ratio test (', i1, ') ')
      end
 
C->>> ---------------------------------------------> ems_refine_pv_c <<<
c     Refines the pivotal column. Assumess that the pivotal column
c     supplied corresponds to FTRAN applied to vr_t_en_bs and is packed
c     with the values in nw_eta_v and indices in nw_eta_ix. The full
c     length vector pv_c_v is assumed to be zeroed (unless steepest edge
c     pricing is used) and it is used to calculate the values of the
c     refined pivotal column.
c
c     NB FTRAN applied to a_q gives pv_c = -A_B^{-1}a_q and it is this
c     that we are refining.
c
c     To get -A_B(pv_c+d) = a_q, solve -A_B.d = A_B.pv_c + a_q and add
c     d to pv_c.
c
      subroutine ems_refine_pv_c(
     &     pv_c_sgn,
     &     nw_eta_v,
     &     pv_c_v,
     &     nw_eta_ix,
     &     vr_in_r,
     &     ds, is)
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
      integer pv_c_sgn
      integer nw_eta_ix(0:nw_eta_l_ix)
      integer vr_in_r(0:n_r), is(0:is_n_en_m1)
      double precision nw_eta_v(0:n_r)
      double precision pv_c_v(0:n_r)
      double precision ds(0:ds_n_en_m1)
      integer ix_n, r_n
      integer nw_r_n, og_r_n
      double precision norm_rsdu, norm_dl
      double precision sv_fwd_tran_ze
 
      if (pc_alg .eq. pc_alg_sed) then
c
c     Zero pv_c_v if steepest edge pricing is used.
c
         do 10, ix_n = nw_eta_f_ix, nw_eta_l_ix
            pv_c_v(nw_eta_ix(ix_n)) = zero
 10      continue
      endif
      if (iand(inv_alg_msk, inv_alg_perm) .ne. 0) then
c
c     Form the residual.
c
         call ems_g_ftran_rsdu(
     &        pv_c_sgn,
     &        vr_in_r,
     &        ds(p_perm_tran_vec),
     &        nw_eta_v,
     &        nw_eta_ix,
     &        ds(p_mtx_r_v),
     &        is(p_mtx_r_ix),
     &        is(p_mtx_c_sa))
c
c     Calculate its norm and permute the RHS values in copying from
c     perm_rsdu to rsdu.
c
         norm_rsdu = zero
         do 50, og_r_n = 1, n_r
            if (ds(p_perm_tran_vec+og_r_n) .ne. zero) then
               nw_r_n = is(p_og_t_nw_perm+og_r_n)
               pv_c_v(nw_r_n) = ds(p_perm_tran_vec+og_r_n)
               ds(p_perm_tran_vec+og_r_n) = zero
               norm_rsdu = norm_rsdu + pv_c_v(nw_r_n)*pv_c_v(nw_r_n)
            end if
 50      continue
         norm_rsdu = sqrt(norm_rsdu)
      else
c
c     If not permuting INVERT.
c
c     Form the residual.
c
         call ems_g_ftran_rsdu(
     &        pv_c_sgn,
     &        vr_in_r,
     &        pv_c_v,
     &        nw_eta_v,
     &        nw_eta_ix,
     &        ds(p_mtx_r_v),
     &        is(p_mtx_r_ix),
     &        is(p_mtx_c_sa))
c
c     Calculate its norm.
c
         norm_rsdu = zero
         do 120, r_n = 1, n_r
            norm_rsdu = norm_rsdu + pv_c_v(r_n)*pv_c_v(r_n)
 120     continue
         norm_rsdu = sqrt(norm_rsdu)
      endif
c
c     Form y := -A_B^{-1}y
c
      sv_fwd_tran_ze = fwd_tran_ze
      fwd_tran_ze = zero
      call ems_ftran(pv_c_v, n_r+1, ds, is)
      fwd_tran_ze = sv_fwd_tran_ze
 
      norm_dl = zero
      do 150, r_n = 1, n_r
         norm_dl = norm_dl + pv_c_v(r_n)*pv_c_v(r_n)
 150  continue
      norm_dl = sqrt(norm_dl)
c
c     Form y: = y + pv_c
c
      do 160, ix_n = nw_eta_f_ix, nw_eta_l_ix
         r_n = nw_eta_ix(ix_n)
         pv_c_v(r_n) = pv_c_v(r_n) + nw_eta_v(ix_n)
 160  continue
c
c     Indicate that the indices of the new eta are not known.
c
      nw_eta_ix(0) = n_r+1
      nw_eta_l_ix = n_r+1
c
c     Report on the refinement.
c
c      if (norm_rsdu+norm_dl .gt. tl_pr_ifs) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     norm_rsdu, norm_dl
         call ems_msg_wr_li(warn_msg_n)
c      endif
      return
 9000 format('Refinement: |b-Ax| = ', g11.4, ' |dx|= ', g11.4)
      end
 
C->>> ------------------------------------------------> ems_ck_ftran <<<
      subroutine ems_ck_ftran(pv_c_sgn,
     &     nw_eta_v, pv_c_v, nw_eta_ix, norm_rsdu, ds, is)
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
      integer pv_c_sgn
      integer nw_eta_ix(0:nw_eta_l_ix), is(0:is_n_en_m1)
      double precision nw_eta_v(0:n_r), pv_c_v(0:n_r), norm_rsdu
      double precision ds(0:ds_n_en_m1)
      integer ix_n, r_n
 
      if (pc_alg .eq. pc_alg_sed) then
c
c     Zero pv_c_v if steepest edge pricing is used.
c
         do 10, ix_n = nw_eta_f_ix, nw_eta_l_ix
            pv_c_v(nw_eta_ix(ix_n)) = zero
 10      continue
      endif
c
c     Form the residual.
c
      call ems_g_ftran_rsdu(
     &     pv_c_sgn,
     &     is(p_vr_in_r),
     &     pv_c_v,
     &     nw_eta_v,
     &     nw_eta_ix,
     &     ds(p_mtx_r_v),
     &     is(p_mtx_r_ix),
     &     is(p_mtx_c_sa))
c
c     Calculate its norm and zero pv_c_v.
c
      norm_rsdu = zero
      do 20, r_n = 1, n_r
         norm_rsdu = norm_rsdu + pv_c_v(r_n)*pv_c_v(r_n)
         pv_c_v(r_n) = zero
 20   continue
      norm_rsdu = sqrt(norm_rsdu)
      if (pc_alg .eq. pc_alg_sed) then
c
c     Scatter nw_eta_v if steepest edge pricing is used. NB use reverse
c     order in case the pivotal row is in the index set---as well as
c     being in position nw_eta_ix(0)---but has a zero entry in the array
c     of packed values.
c
         do 30, ix_n = nw_eta_l_ix, nw_eta_f_ix, -1
            pv_c_v(nw_eta_ix(ix_n)) = nw_eta_v(ix_n)
 30      continue
      endif
c      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
c     &        n_si_it, norm_rsdu
c      call ems_msg_wr_li(info_msg_n)
c 9000 format('Iteration ', i7, ': ||b-Ax|| = ', g11.4)
      return
      end
 
C->>> --------------------------------------------> ems_g_ftran_rsdu <<<
      subroutine ems_g_ftran_rsdu(
     &     pv_c_sgn,
     &     vr_in_r, rsdu, nw_eta_v, nw_eta_ix, r_v, r_ix, c_sa)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
      integer pv_c_sgn
      integer vr_in_r(0:n_r), nw_eta_ix(0:nw_eta_l_ix)
      integer r_ix(0:n_a_el), c_sa(0:n_c+1)
      double precision nw_eta_v(0:n_r)
      double precision rsdu(0:n_r)
      double precision r_v(0:n_a_el)
      integer ix_n, r_n, vr_n, el_n
      double precision v
c
c     Form y = A_B.sol
c
      if (nw_eta_l_ix .le. n_r) then
         do 20, ix_n = nw_eta_f_ix, nw_eta_l_ix
            r_n = nw_eta_ix(ix_n)
            vr_n = vr_in_r(r_n)
            if (vr_n .le. n_c) then
               v = pv_c_sgn*nw_eta_v(ix_n)
               do 10, el_n = c_sa(vr_n), c_sa(vr_n+1)-1
                  rsdu(r_ix(el_n)) = rsdu(r_ix(el_n)) + v*r_v(el_n)
 10            continue
            else
               rsdu(vr_n-mx_n_c) =
     &              rsdu(vr_n-mx_n_c) - pv_c_sgn*nw_eta_v(ix_n)
            endif
 20      continue
      else
         do 40, r_n = 1, n_r
            vr_n = vr_in_r(r_n)
            v = pv_c_sgn*nw_eta_v(r_n)
            if (v .ne. zero) then
               if (vr_n .le. n_c) then
                  do 30, el_n = c_sa(vr_n), c_sa(vr_n+1)-1
                     rsdu(r_ix(el_n)) = rsdu(r_ix(el_n)) + v*r_v(el_n)
 30               continue
               else
                  rsdu(vr_n-mx_n_c) =
     &                 rsdu(vr_n-mx_n_c) - pv_c_sgn*nw_eta_v(r_n)
               endif
            endif
 40      continue
      endif
c
c     Form y := y + rhs
c
      if (vr_t_en_bs .le. n_c) then
         do 110, el_n = c_sa(vr_t_en_bs), c_sa(vr_t_en_bs+1)-1
            rsdu(r_ix(el_n)) =
     &           rsdu(r_ix(el_n)) + r_v(el_n)
 110     continue
      else
         rsdu(vr_t_en_bs-mx_n_c) = rsdu(vr_t_en_bs-mx_n_c) - one
      endif
      return
      end
 
C->>> ----------------------------------------------> ems_cz_r_cg_tl <<<
c     Find a pivot which avoids growth, even if it means increasing the
c     feasibility tolerance.
c
      subroutine  ems_cz_r_cg_tl(ix_o_vr_t_lv_bs,
     &     rsmi_lb, rsmi_ub, pr_act, st, vr_in_r,
     &     nw_eta_v, nw_eta_ix, cdd_r)
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
      integer ix_o_vr_t_lv_bs
      integer st(0:mx_n_c+n_r)
      integer vr_in_r(0:n_r), nw_eta_ix(0:n_r)
      integer cdd_r(0:n_r)
      double precision rsmi_lb(0:mx_n_c+n_r)
      double precision rsmi_ub(0:mx_n_c+n_r)
      double precision pr_act(0:mx_n_c+n_r)
      double precision nw_eta_v(0:n_r)
 
      double precision og_tl_pr_ifs
c      double precision og_pv, og_aa, og_growth
c      integer og_pv_r_n, og_vr_t_lv_bs
      double precision rsdu, growth, mu, ok_pv
 
      integer ix_n, vr_n, r_n, n_cdd_r
c
c     First determine the shortest step which gives an acceptable pivot.
c
      og_tl_pr_ifs = tl_pr_ifs
c      og_pv_r_n = nw_eta_ix(ix_o_vr_t_lv_bs)
c      og_vr_t_lv_bs = vr_in_r(og_pv_r_n)
c      og_pv = nw_eta_v(ix_o_vr_t_lv_bs)
c      og_aa = aa
c      og_growth = mx_pv_c_v/abs(og_pv)
      ok_pv = mx_pv_c_v/tl_cz_r_growth
      call ems_cz_r_g_ok_pv(ix_o_vr_t_lv_bs,
     &     rsmi_lb, rsmi_ub, pr_act, st, vr_in_r,
     &     nw_eta_v, nw_eta_ix)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      mu = mv_dir*aa
      pv_r_n = nw_eta_ix(ix_o_vr_t_lv_bs)
      vr_t_lv_bs = vr_in_r(pv_r_n)
      pv = nw_eta_v(ix_o_vr_t_lv_bs)
      growth = mx_pv_c_v/abs(pv)
c
c     Secondly determine how much the feasibility tolerance should be
c     increased in order to make the desired step.
c
      do 30, ix_n = nw_eta_f_ix, nw_eta_l_ix
         pv = nw_eta_v(ix_n)
c     NO_SGN pv = mv_dir*pv
         pv = mv_dir*pv
         vr_n = vr_in_r(nw_eta_ix(ix_n))
         if (iand(st(vr_n), ifs_bt) .ne. 0) goto 30
         if (pv .gt. zero) then
            rsdu = (pr_act(vr_n) + mu*nw_eta_v(ix_n)) - rsmi_ub(vr_n)
            tl_pr_ifs = max(rsdu, tl_pr_ifs)
         else
            rsdu = rsmi_lb(vr_n) - (pr_act(vr_n) + mu*nw_eta_v(ix_n))
            tl_pr_ifs = max(rsdu, tl_pr_ifs)
         end if
 30   continue
      pv = nw_eta_v(ix_o_vr_t_lv_bs)
      if (lp_ph .eq. 1) then
         if (tl_pr_ifs .le. og_tl_pr_ifs) then
c
c     If tl_pr_ifs is unchanged, check all basic variables (except the
c     leaving variable) with nonzero entries in the pivotal column.
c     It may be possible to be clever and save a few checks if, say, the
c     step is shorter than before---which is highly unlikely (if not
c     impossible?) However, the savings would be minimal and it would be
c     very hard to debug.
c
            n_cdd_r = 0
            vr_t_lv_bs = vr_in_r(nw_eta_ix(ix_o_vr_t_lv_bs))
            do 40, ix_n = nw_eta_f_ix, nw_eta_l_ix
               if (ix_n .eq. ix_o_vr_t_lv_bs) goto 40
               r_n = nw_eta_ix(ix_n)
               vr_n = vr_in_r(r_n)
               if (iand(st(vr_n), ifs_bt) .ne. 0) then
                  n_cdd_r = n_cdd_r + 1
                  cdd_r(n_cdd_r) = r_n
               endif
 40         continue
            cdd_r(0) = n_cdd_r
         else
c
c     If tl_pr_ifs has increased, all basic variables marked as
c     infeasible may become feasible---not just those with nonzero
c     entries in the pivotal column---so ems_g_bc_fs_cg should check
c     all basic variables marked as infeasible (except the leaving
c     variable).
c
            n_cdd_r = 0
            vr_t_lv_bs = vr_in_r(nw_eta_ix(ix_o_vr_t_lv_bs))
            do 41, r_n = 1, n_r
               vr_n = vr_in_r(r_n)
               if (vr_n .eq. vr_t_lv_bs) goto 41
               if (iand(st(vr_n), ifs_bt) .ne. 0) then
                  n_cdd_r = n_cdd_r + 1
                  cdd_r(n_cdd_r) = r_n
               endif
 41         continue
            cdd_r(0) = n_cdd_r
         endif
      endif
 7000 continue
      return
      end
 
C->>> ----------------------------------------------> ems_cz_r_mv_bd <<<
c     Find a pivot which avoids growth, even if it means moving the
c     bounds on some variables.
c
      subroutine  ems_cz_r_mv_bd(ix_o_vr_t_lv_bs,
     &     rsmi_lb, rsmi_ub, pr_act, st, vr_in_r,
     &     nw_eta_v, nw_eta_ix, cdd_r,
     &     n_mv_bd, mx_dl_bd)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      include 'SVMVBD.INC'
      double precision correction_fac
      parameter (correction_fac = 1d-10)
      integer ix_o_vr_t_lv_bs
      integer st(0:mx_n_c+n_r)
      integer vr_in_r(0:n_r), nw_eta_ix(0:n_r)
      integer cdd_r(0:n_r)
      double precision rsmi_lb(0:mx_n_c+n_r)
      double precision rsmi_ub(0:mx_n_c+n_r)
      double precision pr_act(0:mx_n_c+n_r)
      double precision nw_eta_v(0:n_r)
      integer n_mv_bd
      double precision mx_dl_bd
      integer og_pv_r_n, og_vr_t_lv_bs
      double precision og_pv, og_aa, og_growth, ok_pv
      double precision rsdu, growth, mu, dl_bd
      double precision u_pr_act, nw_bd, mx_abs_bd
      double precision correction
 
      integer ix_n, vr_n, r_n, n_cdd_r, sv_mv_bd_n
c
c     First determine the shortest step which gives an acceptable pivot.
c
      og_pv_r_n = nw_eta_ix(ix_o_vr_t_lv_bs)
      og_vr_t_lv_bs = vr_in_r(og_pv_r_n)
      og_pv = nw_eta_v(ix_o_vr_t_lv_bs)
      og_aa = aa
      og_growth = mx_pv_c_v/abs(og_pv)
      ok_pv = mx_pv_c_v/tl_cz_r_growth
      call ems_cz_r_g_ok_pv(ix_o_vr_t_lv_bs,
     &     rsmi_lb, rsmi_ub, pr_act, st, vr_in_r,
     &     nw_eta_v, nw_eta_ix)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      mu = mv_dir*aa
      pv_r_n = nw_eta_ix(ix_o_vr_t_lv_bs)
      vr_t_lv_bs = vr_in_r(pv_r_n)
      pv = nw_eta_v(ix_o_vr_t_lv_bs)
      growth = mx_pv_c_v/abs(pv)
c
c     Secondly determine how much individual bounds must be increased in
c     order to make the desired step.
c
      n_mv_bd = 0
      mx_dl_bd = zero
      do 30, ix_n = nw_eta_f_ix, nw_eta_l_ix
         pv = nw_eta_v(ix_n)
c     NO_SGN pv = mv_dir*pv
         pv = mv_dir*pv
         vr_n = vr_in_r(nw_eta_ix(ix_n))
         if (iand(st(vr_n), ifs_bt) .ne. 0) goto 30
         u_pr_act = pr_act(vr_n) + mu*nw_eta_v(ix_n)
         if (pv .gt. zero) then
            rsdu = (rsmi_ub(vr_n) - u_pr_act) + tl_pr_ifs
            if (rsdu .lt. zero) then
c
c     Move the upper bound. Add a correction to ensure that the updated
c     primal activity will not exceed the (expanded) bound.
c
c     For the original leaving variable, make sure that the updated
c     primal activity will not exceed the true bound---otherwise this
c     variable may well be chosen next time and cause growth.
c
               if (vr_n .eq. og_vr_t_lv_bs)
     &              rsdu = rsmi_ub(vr_n) - u_pr_act
               n_mv_bd = n_mv_bd + 1
               nw_bd = rsmi_ub(vr_n) - rsdu
               mx_abs_bd = max(one, abs(nw_bd), abs(rsmi_ub(vr_n)))
               correction = mx_abs_bd*correction_fac
               nw_bd = nw_bd + correction
               dl_bd = nw_bd - rsmi_ub(vr_n)
               mx_dl_bd = max(dl_bd, mx_dl_bd)
c
c     See if this variable has had its upper bound moved before, if not
c     save the original bound.
c
               do 10, sv_mv_bd_n = 1, n_sv_mv_bd
                  if (sv_mv_bd_vr_n(sv_mv_bd_n) .eq. vr_n) goto 15
 10            continue
               if (n_sv_mv_bd .ge. mx_n_sv_mv_bd) goto 8000
               n_sv_mv_bd = n_sv_mv_bd + 1
               sv_mv_bd_vr_n(n_sv_mv_bd) = vr_n
               sv_mv_bd_v(n_sv_mv_bd) = rsmi_ub(vr_n)
 15            continue
               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9301)n_si_it,
     &              n_mv_bd, n_sv_mv_bd,
     &              vr_n, rsmi_ub(vr_n), dl_bd, nw_bd
               call ems_msg_wr_li(warn_msg_n)
               rsmi_ub(vr_n) = nw_bd
               rsdu = (rsmi_ub(vr_n) - u_pr_act) + tl_pr_ifs
               if (rsdu .lt. zero) goto 8010
            endif
         else
            rsdu = (rsmi_lb(vr_n)-u_pr_act) - tl_pr_ifs
            if (rsdu .gt. zero) then
c
c     Move the lower bound. Subtract a correction to ensure that the
c     updated primal activity will not exceed the expanded bound.
c
c     For the original leaving variable, make sure that the updated
c     primal activity will not exceed the true bound---otherwise this
c     variable may well be chosen next time and cause growth.
c
               if (vr_n .eq. og_vr_t_lv_bs)
     &              rsdu = rsmi_lb(vr_n) - u_pr_act
               n_mv_bd = n_mv_bd + 1
               nw_bd = rsmi_lb(vr_n) - rsdu
               mx_abs_bd = max(one, abs(nw_bd), abs(rsmi_lb(vr_n)))
               correction = mx_abs_bd*correction_fac
               nw_bd = nw_bd - correction
               dl_bd = rsmi_lb(vr_n) - nw_bd
               mx_dl_bd = max(dl_bd, mx_dl_bd)
c
c     See if this variable has had its lower bound moved before, if not
c     save the original bound.
c
               do 20, sv_mv_bd_n = 1, n_sv_mv_bd
                  if (sv_mv_bd_vr_n(sv_mv_bd_n) .eq. -vr_n) goto 25
 20            continue
               if (n_sv_mv_bd .ge. mx_n_sv_mv_bd) goto 8000
               n_sv_mv_bd = n_sv_mv_bd + 1
               sv_mv_bd_vr_n(n_sv_mv_bd) = -vr_n
               sv_mv_bd_v(n_sv_mv_bd) = rsmi_lb(vr_n)
 25            continue
               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9302)n_si_it,
     &              n_mv_bd, n_sv_mv_bd,
     &              vr_n, rsmi_lb(vr_n), dl_bd, nw_bd
               call ems_msg_wr_li(warn_msg_n)
               rsmi_lb(vr_n) = nw_bd
               rsdu = (rsmi_lb(vr_n) - u_pr_act) - tl_pr_ifs
               if (rsdu .gt. zero) goto 8010
            endif
            tl_pr_ifs = max(rsdu, tl_pr_ifs)
         end if
 30   continue
      pv = nw_eta_v(ix_o_vr_t_lv_bs)
      if (lp_ph .eq. 1) then
c
c     Check all basic variables (except the leaving variable) with
c     nonzero entries in the pivotal column.
c
         n_cdd_r = 0
         vr_t_lv_bs = vr_in_r(nw_eta_ix(ix_o_vr_t_lv_bs))
         do 40, ix_n = nw_eta_f_ix, nw_eta_l_ix
            if (ix_n .eq. ix_o_vr_t_lv_bs) goto 40
            r_n = nw_eta_ix(ix_n)
            vr_n = vr_in_r(r_n)
            if (iand(st(vr_n), ifs_bt) .ne. 0) then
               n_cdd_r = n_cdd_r + 1
               cdd_r(n_cdd_r) = r_n
            endif
 40      continue
         cdd_r(0) = n_cdd_r
      endif
 7000 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(warn_msg_n)
      alg_er = .true.
      goto 7000
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)correction, rsdu
      call ems_msg_wr_li(warn_msg_n)
      alg_er = .true.
      goto 7000
 9301 format('Iteration ', i7,
     &     ': bound move ', i3, ' (', i3, ' saved)',
     &     ': Variable ', i7, ': increasing upper bound from ', g11.4,
     &     ' by ', g11.4, ' to ', g11.4)
 9302 format('Iteration ', i7,
     &     ': bound move ', i3, ' (', i3, ' saved)',
     &     ': Variable ', i7, ': decreasing lower bound from ', g11.4,
     &     ' by ', g11.4, ' to ', g11.4)
 9800 format('In ems_cz_r_mv_bd: Need to save too many original bounds')
 9801 format('In ems_cz_r_mv_bd: Rounding error correction = ', g11.4,
     &     ' does not prevent residual of = ', g11.4)
      end
 
      subroutine ems_rcov_sv_mv_bd(rsmi_lb, rsmi_ub)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'SVMVBD.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      double precision rsmi_lb(0:mx_n_c+n_r)
      double precision rsmi_ub(0:mx_n_c+n_r)
      integer sv_bd_n, vr_n
 
      do 10, sv_bd_n = 1, n_sv_mv_bd
         vr_n = sv_mv_bd_vr_n(sv_bd_n)
         if (vr_n .gt. 0) then
            rsmi_ub(vr_n) = sv_mv_bd_v(sv_bd_n)
         else if (vr_n .lt. 0) then
            rsmi_lb(-vr_n) = sv_mv_bd_v(sv_bd_n)
         else
            goto 8000
         endif
 10   continue
      n_sv_mv_bd = 0
 7000 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)vr_n
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9800 format('Saved bound for variable ', i7)
      end
 
C->>> ---------------------------------------------> ems_cz_r_cg_act <<<
c     Find a pivot which avoids growth, even if it means changing the
c     activities of some variables.
c
      subroutine  ems_cz_r_cg_act(ix_o_vr_t_lv_bs,
     &     rsmi_lb, rsmi_ub, pr_act, st, vr_in_r,
     &     nw_eta_v, nw_eta_ix, cdd_r,
     &     n_cg_act, mx_dl_act)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      double precision correction_fac
      parameter (correction_fac = 1d-10)
      integer ix_o_vr_t_lv_bs
      integer st(0:mx_n_c+n_r)
      integer vr_in_r(0:n_r), nw_eta_ix(0:n_r)
      integer cdd_r(0:n_r)
      double precision rsmi_lb(0:mx_n_c+n_r)
      double precision rsmi_ub(0:mx_n_c+n_r)
      double precision pr_act(0:mx_n_c+n_r)
      double precision nw_eta_v(0:n_r)
      integer n_cg_act
      double precision mx_dl_act
 
      integer og_pv_r_n, og_vr_t_lv_bs
      double precision og_pv, og_aa, og_growth, ok_pv
      double precision rsdu, growth, mu, dl_act
      double precision u_pr_act, nw_act, mx_abs_act
      double precision correction
 
      integer ix_n, vr_n, r_n, n_cdd_r
c
c     First determine the shortest step which gives an acceptable pivot.
c
      og_pv_r_n = nw_eta_ix(ix_o_vr_t_lv_bs)
      og_vr_t_lv_bs = vr_in_r(og_pv_r_n)
      og_pv = nw_eta_v(ix_o_vr_t_lv_bs)
      og_aa = aa
      og_growth = mx_pv_c_v/abs(og_pv)
      ok_pv = mx_pv_c_v/tl_cz_r_growth
      call ems_cz_r_g_ok_pv(ix_o_vr_t_lv_bs,
     &     rsmi_lb, rsmi_ub, pr_act, st, vr_in_r,
     &     nw_eta_v, nw_eta_ix)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      mu = mv_dir*aa
      pv_r_n = nw_eta_ix(ix_o_vr_t_lv_bs)
      vr_t_lv_bs = vr_in_r(pv_r_n)
      pv = nw_eta_v(ix_o_vr_t_lv_bs)
      growth = mx_pv_c_v/abs(pv)
c
c     Secondly determine how much individual bounds must be increased in
c     order to make the desired step.
c
      n_cg_act = 0
      mx_dl_act = zero
      do 30, ix_n = nw_eta_f_ix, nw_eta_l_ix
         pv = nw_eta_v(ix_n)
c     NO_SGN pv = mv_dir*pv
         pv = mv_dir*pv
         vr_n = vr_in_r(nw_eta_ix(ix_n))
         if (iand(st(vr_n), ifs_bt) .ne. 0) goto 30
         u_pr_act = pr_act(vr_n) + mu*nw_eta_v(ix_n)
         if (pv .gt. zero) then
            rsdu = (rsmi_ub(vr_n) - u_pr_act) + tl_pr_ifs
            if (rsdu .lt. zero) then
c
c     Decrease the activity. Subtract a correction to ensure that the
c     updated primal activity will not exceed the expanded bound.
c
c     For the original leaving variable, make sure that the updated
c     primal activity will not exceed the true bound---otherwise this
c     variable may well be chosen next time and cause growth.
c
               if (vr_n .eq. og_vr_t_lv_bs)
     &              rsdu = rsmi_ub(vr_n) - u_pr_act
               n_cg_act = n_cg_act + 1
               nw_act = pr_act(vr_n) + rsdu
               mx_abs_act = max(one, abs(nw_act), abs(pr_act(vr_n)))
               correction = mx_abs_act*correction_fac
               nw_act = nw_act - correction
               dl_act = pr_act(vr_n) - nw_act
               mx_dl_act = max(dl_act, mx_dl_act)
               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9301)n_si_it,
     &              n_cg_act,
     &              vr_n, pr_act(vr_n), dl_act, nw_act
               call ems_msg_wr_li(warn_msg_n)
               pr_act(vr_n) = nw_act
               u_pr_act = pr_act(vr_n) + mu*nw_eta_v(ix_n)
               rsdu = (rsmi_ub(vr_n) - u_pr_act) + tl_pr_ifs
               if (rsdu .lt. zero) goto 8010
               rsdu = (rsmi_lb(vr_n) - u_pr_act) - tl_pr_ifs
               if (rsdu .gt. zero) goto 8020
            endif
         else
            rsdu = (rsmi_lb(vr_n)-u_pr_act) - tl_pr_ifs
            if (rsdu .gt. zero) then
c
c     Increase the activity. Add a correction to ensure that the
c     updated primal activity will not exceed the expanded bound.
c
c     For the original leaving variable, make sure that the updated
c     primal activity will not exceed the true bound---otherwise this
c     variable may well be chosen next time and cause growth.
c
               if (vr_n .eq. og_vr_t_lv_bs)
     &              rsdu = rsmi_lb(vr_n) - u_pr_act
               n_cg_act = n_cg_act + 1
               nw_act = pr_act(vr_n) + rsdu
               mx_abs_act = max(one, abs(nw_act), abs(pr_act(vr_n)))
               correction = mx_abs_act*correction_fac
               nw_act = nw_act + correction
               dl_act = nw_act - pr_act(vr_n)
               mx_dl_act = max(dl_act, mx_dl_act)
               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9302)n_si_it,
     &              n_cg_act,
     &              vr_n, pr_act(vr_n), dl_act, nw_act
               call ems_msg_wr_li(warn_msg_n)
               pr_act(vr_n) = nw_act
               u_pr_act = pr_act(vr_n) + mu*nw_eta_v(ix_n)
               rsdu = (rsmi_lb(vr_n) - u_pr_act) - tl_pr_ifs
               if (rsdu .gt. zero) goto 8010
               rsdu = (rsmi_ub(vr_n) - u_pr_act) + tl_pr_ifs
               if (rsdu .lt. zero) goto 8020
            endif
            tl_pr_ifs = max(rsdu, tl_pr_ifs)
         end if
 30   continue
      pv = nw_eta_v(ix_o_vr_t_lv_bs)
      if (lp_ph .eq. 1) then
c
c     Check all basic variables (except the leaving variable) with
c     nonzero entries in the pivotal column.
c
         n_cdd_r = 0
         vr_t_lv_bs = vr_in_r(nw_eta_ix(ix_o_vr_t_lv_bs))
         do 40, ix_n = nw_eta_f_ix, nw_eta_l_ix
            if (ix_n .eq. ix_o_vr_t_lv_bs) goto 40
            r_n = nw_eta_ix(ix_n)
            vr_n = vr_in_r(r_n)
            if (iand(st(vr_n), ifs_bt) .ne. 0) then
               n_cdd_r = n_cdd_r + 1
               cdd_r(n_cdd_r) = r_n
            endif
 40      continue
         cdd_r(0) = n_cdd_r
      endif
 7000 continue
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)correction, rsdu
      call ems_msg_wr_li(warn_msg_n)
      alg_er = .true.
      goto 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)rsdu
      call ems_msg_wr_li(warn_msg_n)
      alg_er = .true.
      goto 7000
 9301 format('Iteration ', i7,
     &     ': activity change ', i3,
     &     ': Variable ', i7, ': increasing activity from ', g11.4,
     &     ' by ', g11.4, ' to ', g11.4)
 9302 format('Iteration ', i7,
     &     ': activity change ', i3,
     &     ': Variable ', i7, ': decreasing activity from ', g11.4,
     &     ' by ', g11.4, ' to ', g11.4)
 9801 format('In ems_cz_r_cg_act: Rounding error correction = ', g11.4,
     &     ' does not prevent residual of = ', g11.4)
 9802 format('In ems_cz_r_cg_act: Change of activity violates ',
     &     'opposite expanded bound by ', g11.4)
      end
 
C->>> --------------------------------------------> ems_cz_r_g_ok_pv <<<
c     Find the shortest step which corresponds to a pivot which avoids
c     growth.
c
      subroutine ems_cz_r_g_ok_pv(ix_o_vr_t_lv_bs,
     &     rsmi_lb, rsmi_ub, pr_act, st, vr_in_r,
     &     nw_eta_v, nw_eta_ix)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      include 'RLCTVR.INC'
      integer ix_o_vr_t_lv_bs
      integer st(0:mx_n_c+n_r)
      integer vr_in_r(0:n_r), nw_eta_ix(0:n_r)
      double precision rsmi_lb(0:mx_n_c+n_r)
      double precision rsmi_ub(0:mx_n_c+n_r)
      double precision pr_act(0:mx_n_c+n_r)
      double precision nw_eta_v(0:n_r)
 
      double precision ok_pv, rsdu
      integer ix_n, vr_n, vr_st
 
      ok_pv = mx_pv_c_v/tl_cz_r_growth
      aa = inf
      ix_o_vr_t_lv_bs = -1
      if (lp_ph .eq. 1) then
         do 10, ix_n = nw_eta_f_ix, nw_eta_l_ix
            pv = nw_eta_v(ix_n)
            if (abs(pv) .lt. ok_pv) goto 10
c     NO_SGN pv = mv_dir*pv
            pv = mv_dir*pv
            vr_n = vr_in_r(nw_eta_ix(ix_n))
            vr_st = st(vr_n)
            if (iand(vr_st, ifs_bt) .ne. 0) then
               if (pv .gt. zero) then
                  if (iand(vr_st, up_bt) .ne. 0) then
                     rsdu = rsmi_lb(vr_n) - pr_act(vr_n)
                     if (rsdu .lt. aa*pv) then
                        aa = rsdu/pv
                        ix_o_vr_t_lv_bs = ix_n
                     end if
                  endif
               else
                  if (iand(vr_st, dn_bt) .ne. 0) then
                     rsdu = rsmi_ub(vr_n) - pr_act(vr_n)
                     if (rsdu .gt. aa*pv) then
                        aa = rsdu/pv
                        ix_o_vr_t_lv_bs = ix_n
                     end if
                  endif
               endif
            else
               if (pv .gt. zero) then
                  rsdu = rsmi_ub(vr_n) - pr_act(vr_n)
                  if (rsdu .lt. aa*pv) then
                     aa = rsdu/pv
                     ix_o_vr_t_lv_bs = ix_n
                  end if
               else
                  rsdu = rsmi_lb(vr_n) - pr_act(vr_n)
                  if (rsdu .gt. aa*pv) then
                     aa = rsdu/pv
                     ix_o_vr_t_lv_bs = ix_n
                  end if
               end if
            end if
 10      continue
      else
         do 20, ix_n = nw_eta_f_ix, nw_eta_l_ix
            pv = nw_eta_v(ix_n)
            if (abs(pv) .lt. ok_pv) goto 20
c     NO_SGN pv = mv_dir*pv
            pv = mv_dir*pv
            vr_n = vr_in_r(nw_eta_ix(ix_n))
            if (pv .gt. zero) then
               rsdu = rsmi_ub(vr_n) - pr_act(vr_n)
               if (rsdu .le. aa*pv) then
                  aa = rsdu/pv
                  ix_o_vr_t_lv_bs = ix_n
               end if
            else
               rsdu = rsmi_lb(vr_n) - pr_act(vr_n)
               if (rsdu .ge. aa*pv) then
                  aa = rsdu/pv
                  ix_o_vr_t_lv_bs = ix_n
               end if
            end if
 20      continue
      endif
      if (ix_o_vr_t_lv_bs .lt. 0 .or. aa .ge. inf) goto 8000
      if (aa .lt. zero) goto 8010
 7000 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
     &     ix_o_vr_t_lv_bs, aa
      call ems_msg_wr_li(warn_msg_n)
      alg_er = .true.
      goto 7000
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)
     &     ix_o_vr_t_lv_bs, aa
      call ems_msg_wr_li(warn_msg_n)
      alg_er = .true.
      goto 7000
 9800 format('In ems_cz_r_g_ok_pv: Unboundedness: ', i7, 2x, g11.4)
 9801 format('In ems_cz_r_g_ok_pv: Negative step: ', i7, 2x, g11.4)
      end
 
CM      IF (emsol_dev .EQ. 1) THEN
C?C->>> ------------------------------------------> ems_cz_r_g_ok_pv <<<
C?c     Write out the data required to create the picture of CHUZR.
C?c
C?      subroutine ems_rp_cz_r(lp_ph, r_n,
C?     &     i_v, rl_v1, rl_v2, rl_v3, rl_v4)
C?      implicit none
C?      include 'EMSV.INC'
C?      include 'EMSPM.INC'
C?      include 'ICTVR.INC'
C?      include 'RLCTVR.INC'
C?      integer lp_ph, r_n, i_v
C?      double precision rl_v1, rl_v2, rl_v3, rl_v4
C?      double precision lb, pr_act, ub
C?      double precision pv, rao
C?      double precision aa, aa_1, aa_2, aa_fs, aa_ifs, mx_pv_c_v
C?      integer rp_it_n
C?      integer f_ca
C?      data f_ca/1/
C?      if (f_ca .eq. 1) then
C?         f_ca = 0
C?         read(1, *)rp_it_n
C?      endif
C?      if (n_si_it .eq. rp_it_n) then
C?         if (r_n .lt. 0) then
C?            open(unit = 9, file = 'CZR.dat')
C?            write(9, *)lp_ph, tl_pr_ifs
C?         else if (r_n .lt. n_r) then
C?            pv = rl_v1
C?            lb = rl_v2
C?            pr_act = rl_v3
C?            ub = rl_v4
C?            if (pv .gt. zero) then
C?               rao = (lb - pr_act)/pv
C?               write(9, *)r_n, -1, rao, pv
C?               rao = (ub - pr_act)/pv
C?               write(9, *)r_n, 1, rao, pv
C?            else
C?               rao = (ub - pr_act)/pv
C?               write(9, *)r_n, -1, rao, pv
C?               rao = (lb - pr_act)/pv
C?               write(9, *)r_n, 1, rao, pv
C?            endif
C?         else if (r_n .eq. n_r+1) then
C?            aa_1 = rl_v1
C?            write(9, *)-1, 0, 0d0, 0
C?            if (lp_ph .eq. 1) then
C?               aa_fs = rl_v2
C?               aa_ifs = rl_v3
C?               write(9, *)aa_1, aa_fs, aa_ifs
C?            else
C?               aa_2 = rl_v3
C?               write(9, *)aa_1, aa_2
C?            endif
C?         else if (r_n .eq. n_r+2) then
C?            aa = rl_v1
C?            mx_pv_c_v = rl_v2
C?            write(9, *)aa, mx_pv_c_v
C?            close(9)
C?         endif
C?      endif
C?      return
C?      end
CM      ENDIF
      subroutine ems_ck_cz_r_pv(
     &     pv_c_v, nw_eta_v, nw_eta_ix,
     &     nw_eta_f_ix, nw_eta_l_ix,
     &     ix_o_vr_t_lv_bs,
     &     prev_n_ix, rpt_cz_r, alg_er)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      double precision pv_c_v(0:n_r), nw_eta_v(0:n_r)
      integer nw_eta_ix(0:n_r)
      integer nw_eta_f_ix, nw_eta_l_ix
      integer ix_o_vr_t_lv_bs, prev_n_ix
      logical rpt_cz_r, alg_er
      integer n_ix, r_n, ix_n
      double precision pv
 
      rpt_cz_r = .false.
      if (ix_o_vr_t_lv_bs .le. 0) then
         pv = 1d0
      else
         pv = nw_eta_v(ix_o_vr_t_lv_bs)
      endif
      if (ix_o_vr_t_lv_bs .le. 0) goto 8010
c
c     Check that the number of packed values is strictly decreasing
c     ---prevents infinite loops.
c
      n_ix = nw_eta_l_ix - nw_eta_f_ix + 1
      if (n_ix .ge. prev_n_ix) goto 8020
      prev_n_ix = n_ix
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)n_si_it, pv
      call ems_msg_wr_li(warn_msg_n)
c
c     Zero the pivotal column---it is not zeroed with steepest edge.
c
      do 10, r_n = 1, n_r
         pv_c_v(r_n) = zero
 10   continue
c
c     Scatter the packed values.
c
      do 20 ix_n = nw_eta_f_ix, nw_eta_l_ix
         pv_c_v(nw_eta_ix(ix_n)) = nw_eta_v(ix_n)
 20   continue
c
c     Zero the pivotal entry.
c
      pv_c_v(nw_eta_ix(ix_o_vr_t_lv_bs)) = zero
      rpt_cz_r = .true.
 
 7000 continue
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)
     &     n_si_it, pv
      call ems_msg_wr_li(er_msg_n)
      alg_er = .true.
      goto 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)
     &     n_si_it, pv, ix_o_vr_t_lv_bs, cz_r_pv_tl, n_ix, prev_n_ix
      call ems_msg_wr_li(er_msg_n)
      alg_er = .true.
      goto 7000
 9000 format('Iteration ', i7, ': Truncating pivot ', g11.4,
     &     ' to zero and repeating ratio test')
 9801 format('Iteration ', i7, ': Truncating pivot ', g11.4,
     &     ' for bound swap?')
 9802 format('Iteration ', i7, ': Truncating pivot ', g11.4,
     &     ' but number of indices ', i9, ' is not less than ', i9)
      end
