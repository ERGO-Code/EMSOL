CM
C->>> -------------------------------------------------> ems_tom_inv <<<
      subroutine ems_tom_inv(
     &     rt_cod, rp_cn, rp_lvl, g_tt_da,
     &     n_r, mx_n_c, n_c, n_a_el, n_inv_sing, fill_fac,
     &     vr_in_r,
     &     mtx_r_v, mtx_r_ix, mtx_c_sa,
     &     mx_n_eta, mx_n_eta_el, n_eta, n_eta_el, n_lo_eta,
     &     eta_v, eta_ix, eta_sa,
     &     lo_eta_pv_in_r, up_eta_pv_in_r,
     &     pv_tl, wr_eta_tl, c_rlv_tl, unit_v_tl,
     &     l_pv_rsdu_er_tl,
     &     wk_a, bs_c_at, r_k_a, pv_r_n_or_mrt,
     &     ck_l_pv_wk_a, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'TOMINV.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      integer rt_cod, rp_cn, rp_lvl, g_tt_da
      integer n_r, n_c, mx_n_c, n_a_el, n_inv_sing
      double precision fill_fac
      integer mx_n_eta, mx_n_eta_el, n_eta, n_eta_el, n_lo_eta
 
      integer mtx_r_ix(0:n_a_el)
      integer mtx_c_sa(0:n_c + 1)
      integer eta_ix(0:mx_n_eta_el)
      integer eta_sa(0:mx_n_eta+1)
      integer lo_eta_pv_in_r(0:n_r)
      integer up_eta_pv_in_r(0:n_r)
      integer is(0:*)
 
      double precision pv_tl, wr_eta_tl, c_rlv_tl, unit_v_tl
      double precision l_pv_rsdu_er_tl
      double precision mtx_r_v(0:n_a_el)
      double precision wk_a(0:n_r)
      double precision eta_v(0:mx_n_eta_el)
      double precision ck_l_pv_wk_a(0:n_r)
      double precision ds(0:*)
 
      integer bs_c_at(1:n_r)
      integer r_k_a(1:n_r)
      integer pv_r_n_or_mrt(1:n_r)
      integer vr_in_r(0:n_r)
 
      double precision mn_el_z, c_mx
      double precision lc_v, abs_lc_v, c_rlv_mn, neg_rcp_pv_v
      integer n_bs_non_ze
      integer n_lg, n_in_bump, n_ab_bump, n_bw_bump
      integer r_n, vr_n, i, k, c_n, el_n, inner_el_n
      integer n_in_un_pv_r, pv_r_cdd
      integer n_in_un_pv_r_m1, se_1_fh_at_sa
      integer n_redo_c
      integer lc_mtx_c_sa, lc_mtx_c_fh
      integer mrt_k, stp, mn_k, mrt_k_dl
      integer se_1_sa, se_1_fh, se_1_en_n
      integer se_2_sa, se_2_fh, se_2_en_n
      integer se_3_sa, se_3_fh, se_3_en_n
      integer f_eta_n, f_eta_el_n
      integer f_bmp_lo_eta_n, f_bmp_lo_el_n
      integer fm_bmp_lo_eta_n
      integer l_lo_eta_n, l_lo_el_n_p1, d_l_lo_el_n_p1
      integer l_up_eta_n, l_up_el_n_m1, d_l_up_el_n_m1
      integer l_eta_n, l_eta_el_n_p1, d_l_eta_el_n_p1
      integer bst_eta_el_n, l_up_el_n, l_eta_el_n, n_eta_el_qy
      integer eta_n, n_triang_ps
      integer n_mv_c
      logical pv_ok, mv_c
      integer sus_ftran_n_op
      integer sus_ftran_mx_n_op
      integer dl_el_n
CM      IF (emsol_da .EQ. 1) THEN
C?c      integer bmp_dim
C?c      integer n_ty_1_ftran
C?c      integer n_ty_2_ftran
C?c      integer n_ftran_eta
C?c      integer n_ftran_eta_skp_n
C?c      integer n_ftran_eta_skp_en
C?c      integer su_n_ftran_eta
C?c      integer n_no_apply_ftran_eta
C?c      double precision pct
CM      ENDIF
CM      IF (emsol_deb .EQ. 1) THEN
C?      integer lc_rt_cod
C?c      logical er_fd
CM      ENDIF
CM      IF (emsol_tt .EQ. 1) THEN
C?      logical g_tom_tt_lvl2
C?      logical g_tom_tt_lvl3
CM      ENDIF
c
c     pv_r_n_or_mrt is used to store the pivot row for the columns in
c     SET 2 (below the bump columns) and SET 3 (the logicals)
c     and the merit for the columns in the bump (part of  SET 1).
c     The SET 3 storage is only used for a short time as temporary
C     storage while the logicals are repositioned in vr_in_r.
 
c      if (rp_cn .ge. 0) write(rp_cn, *) 'on entry vr_in_r='
c      if (rp_cn .ge. 0) write(rp_cn, *) (vr_in_r(i), i=1, n_r)
 
c
c     Tolerances [default---see ems_iz_rl_ct_vr_df]
c
c     pv_tl [1d-6]:     The pivot tolerance: Values less than this are
c                       written to the eta file but are not considered
c                       for pivots.
c
c     wr_eta_tl [1d-9]: The drop tolerance: Values less than this are
c                       not written to the eta file
c
c     c_rlv_tl [1d-2]:  'u' in threshold pivoting
c
c     unit_v_tl [1d-6]: Used to test whether a value can be taken as 1.
c
c=======================================================================
c
c     The value of tom_inv_sing_msk determines the tests and subsequent
c     action in rtelation to the final pivot.
c
c     if iand(tom_inv_sing_msk, *) then
c
c     tom_inv_sing_tl_ck:  a test is done if the pivot is small
c
c     tom_inv_sing_al_ck:  a test is always done
c
c     tom_inv_sing_tl_alw: small pivots passing the test are allowed
c
c     tom_inv_sing_al_alw: small pivots are always allowed
c
c     NB
c     tom_inv_sing_al_ck  => tom_inv_sing_tl_ck
c     tom_inv_sing_al_alw => tom_inv_sing_tl_alw
c
c     Suggested values
c
c     tom_inv_sing_msk =        0: Reverts to old Tomlin INVERT
c     tom_inv_sing_msk =        1: Tests small pivots
c     tom_inv_sing_msk = 1+2 =  3: Tests all pivots
c     tom_inv_sing_msk = 1+4 =  5: Tests small and allows OK pivots
c     tom_inv_sing_msk = 3+4 =  7: Tests all pivots and allows OK pivots
c     tom_inv_sing_msk = 4+8 = 12: Allows all pivots without testing
c     tom_inv_sing_msk = 1+12= 13: Allows all pivots, testing small
c     tom_inv_sing_msk = 3+12= 15: Allows all pivots, testing all
c
c=======================================================================
CM      IF (emsol_tt .EQ. 1) THEN
C?      g_tom_tt_lvl2 = iand(g_tt_da, bt11) .ne. 0
C?      g_tom_tt_lvl3 = iand(g_tt_da, bt12) .ne. 0
CM      ENDIF
      rt_cod = 0
      n_eta = 0
      n_eta_el = 0
      n_ab_bump = 0
      n_in_bump = 0
      n_bw_bump = 0
      n_inv_sing = 0
      n_redo_c = 0
      fill_fac = one
 
      l_lo_eta_n = 0
 
      f_eta_el_n = 1
      l_eta_el_n_p1 = f_eta_el_n
      f_eta_n = 1
      l_eta_n = 0
      se_1_sa = 1
      se_1_fh = se_1_sa - 1
      se_3_sa = n_r + 1
      se_3_fh = se_3_sa - 1
c
c     se_1_sa remains 1
c     se_3_fh remains n_r
c
      sus_ftran_mx_n_op = lo_eta_pv_in_r(0)
CM      IF (emsol_da .EQ. 1) THEN
C?c      n_ty_1_ftran = 0
C?c      n_ty_2_ftran = 0
C?c      n_ftran_eta_skp_n = 0
C?c      n_ftran_eta_skp_en = 0
C?c      su_n_ftran_eta = 0
CM      ENDIF
c
c     Pull out logicals for SET 3; rest in SET 1
c
      do 110, r_n = 1, n_r
c
c     Initialise lo_eta_pv_in_r (up_eta_pv_in_r) which contains the eta
c     number of any L-eta (U-eta) which has a pivot in that row. Those
c     rows in which no L-eta (U-eta) has a pivot must have a large eta
c     number since the minimum eta number will be required to start
c     FTRAN.
c
         lo_eta_pv_in_r(r_n) = mx_n_eta+1
         up_eta_pv_in_r(r_n) = mx_n_eta+1
         if (vr_in_r(r_n).gt.mx_n_c) then
            se_3_sa = se_3_sa - 1
            pv_r_n_or_mrt(se_3_sa) = vr_in_r(r_n)
            bs_c_at(se_3_sa) = vr_in_r(r_n)
         else
            se_1_fh = se_1_fh + 1
            bs_c_at(se_1_fh) = vr_in_r(r_n)
         end if
         r_k_a(r_n) = -1
C     vr_in_r(r_n) = 0
 110  continue
c
c     Set 2 is initially empty: It will be formed between sets 1 and 3!
c
      se_2_fh = se_3_sa - 1
      se_2_sa = se_3_sa
c
c     Loop through the logicals:
c     * setting vr_in_r so they are solved for in their home row;
c     * setting r_k_a = 0 for each row with a basic logical.
c
      do 210, se_3_en_n = se_3_sa, se_3_fh
         vr_n = pv_r_n_or_mrt(se_3_en_n)
         r_k_a(vr_n-mx_n_c) = 0
         vr_in_r(vr_n-mx_n_c) = vr_n
CM      IF (emsol_deb .EQ. 1) THEN
C?         if (rp_lvl .gt. 0) then
C?            if (rp_cn .ge. 0) write(rp_cn, 9100)
C?     &           ' Logical: ', vr_n-mx_n_c,
C?     &           ' Pivoting in row ', vr_n-mx_n_c
C?         endif
CM      ENDIF
 210  continue
c
c     The initialisation of bs_c_at and r_k_a is complete:
c
c     bs_c_at(se_en_n) is structural for se_en_n = 1, se_1_fh
c     bs_c_at(se_en_n) is logical    for se_en_n = se_3_sa, se_3_fh
c
c     r_k_a(r_n) =  0 if bs_c_at(r_n) is logical.
c     r_k_a(r_n) = -1 if bs_c_at(r_n) is structural
c
c     pv_r_n_or_mrt is partially initialised:
c
c     pv_r_n_or_mrt(se_en_n) for se_en_n = se_3_sa, se_3_fh
c     .                      is the logical variable number (NOT its
c     .                      pivotal row number---why?)
c
c     In 1 pass pull out some vectors below bump and get row counts
c
      n_lg = se_3_fh - se_3_sa + 1
      n_bs_non_ze = n_lg
      n_triang_ps = 1
      if (se_1_fh .eq. 0) go to 3700
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tom_tt_lvl2) call ems_tt_rec(tom_triang_tt, -1)
CM      ENDIF
c
c     Cycle through the set 1 entries.
c
      se_1_en_n = se_1_sa
c
c     ==================================================================
c     Top of first triangularisation section
c
c     For each column in set 1
c     * Accumulate the number of nonzeros in the basis---not used
c     * Count the number of entries in rows for which a pivot has not
c     . yet been decided (rows with basic logical) and update the row
c     . count for rows with basic structural.
c     . o If there is exactly one entry in non-pivotal rows then the
c     .   column can be moved into set 2 with a pivot in this row.
c     . o If there is more than one entry in non-pivotal rows then the
c     .   column remains in set 1.
c     . o If there is no entry in non-pivotal rows then the matrix is
c     .   singular and the column is flagged for removal.
c
 300  continue
      c_n = bs_c_at(se_1_en_n)
      n_in_un_pv_r = 0
      n_bs_non_ze = n_bs_non_ze + (mtx_c_sa(c_n+1)-mtx_c_sa(c_n))
      do 310, el_n = mtx_c_sa(c_n), mtx_c_sa(c_n+1)-1
         r_n = mtx_r_ix(el_n)
         if (r_k_a(r_n) .ge. 0) go to 310
         n_in_un_pv_r = n_in_un_pv_r + 1
         r_k_a(r_n) = r_k_a(r_n) - 1
         pv_r_cdd = r_n
 310  continue
 
      n_in_un_pv_r_m1 = n_in_un_pv_r - 1
      if (n_in_un_pv_r_m1 .eq. 0) then
c
c     Column singleton:
c     Exactly one nonzero in rows which have not yet been pivoted on.
c     Move entry into set 2 by swapping it with the last entry in set 1
c     and shifting the finish of set 1 and start of set 2 by 1.
c
         bs_c_at(se_1_en_n) = bs_c_at(se_1_fh)
         se_1_fh = se_1_fh - 1
         se_2_sa = se_2_sa - 1
         bs_c_at(se_2_sa) = c_n
c
c     Indicate that this set entry has been pivoted on by recording its
c     pivotal row number, zeroing its row count and that it is solved
c     for in this row.
c
         pv_r_n_or_mrt(se_2_sa) = pv_r_cdd
         r_k_a(pv_r_cdd) = 0
         vr_in_r(pv_r_cdd) = c_n
CM      IF (emsol_deb .EQ. 1) THEN
C?         if (rp_lvl .gt. 0) then
C?            if (rp_cn .ge. 0) write(rp_cn, 9100)
C?     &           ' Column singleton: ', c_n,
C?     &           ' Pivoting in row ', pv_r_cdd
C?         endif
CM      ENDIF
c
c     Go back to consider next column in set 1---the one which was
c     swapped in to the current set 1 entry---unless set 1 is finished
c     (in which case the column had been at the end of set 1 entry and
c     was swapped with itself).
c
         if (se_1_en_n .gt. se_1_fh) go to 400
         go to 300
      else if (n_in_un_pv_r_m1 .gt. 0) then
c
c     More than one nonzero in rows which have not yet been pivoted on.
c     Column remains in set 1. Go back to consider next column in set 1
c     unless set 1 is finished.
c
         if (se_1_en_n .ge. se_1_fh)  go to 400
         se_1_en_n = se_1_en_n + 1
         go to 300
      else
c
c     No nonzero in rows which have not yet been pivoted on.
c     Matrix is singular so mark the column as one to be removed.
c     Column is removed from set 1 by swapping it with the last entry in
c     set 1 and shifting the finish of set 1.
c
         n_inv_sing = n_inv_sing + 1
         call ems_se_iv_ty(1, rp_cn, c_n, ds, is)
         bs_c_at(se_1_en_n) = bs_c_at(se_1_fh)
         se_1_fh = se_1_fh - 1
         if (se_1_en_n .gt. se_1_fh)  go to 400
         go to 300
      end if
 400  continue
c
c     Set 2 now contains structurals which can be pivoted on after the
c     bump and before the logicals.
c
      n_bw_bump = se_2_fh - se_2_sa + 1
CM      IF (emsol_deb .EQ. 1) THEN
C?      if (rp_lvl .gt. 0) then
C?         if (rp_cn .ge. 0) write(rp_cn, 9100)
C?     &        'Triang', n_triang_ps, ' n_ab_bump =', n_ab_bump,
C?     &        ' n_bw_bump =', n_bw_bump
C?      endif
CM      ENDIF
c
c     Bottom of first triangularisation section
c     ==================================================================
c     Top of second and subsequent triangularisation section
c
c     Before starting the second and subsequent triangularisation pass,
c     see whether INVERT has finished.
c
 410  continue
      if (se_1_fh .eq. 0) then
c
c     All structurals are below the bump (ie there is no bump!) This
c     happens in network problems.
c
         n_bw_bump = se_2_fh - se_2_sa + 1
c
c     The triangularisation phase is finished.
c
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (g_tom_tt_lvl2) call ems_tt_rec(-tom_triang_tt, -1)
CM      ENDIF
         eta_sa(l_eta_n+1) = l_eta_el_n_p1
c
c     Record the (current) last lower triangular eta number and element
c     number+1
c
         l_lo_eta_n = l_eta_n
         l_lo_el_n_p1 = l_eta_el_n_p1
c
c     Write in the etas for columns below the bump.
c
         go to 3700
      end if
c
c     Have to do the triangularisation pass so increment the number of
c     passes.
c
      n_triang_ps = n_triang_ps + 1
c
c     Record the finish of set 1 at the start of this pass and start
c     with the first entry in set 1.
c
      se_1_fh_at_sa = se_1_fh
      se_1_en_n = se_1_sa
 420  continue
      c_n = bs_c_at(se_1_en_n)
      n_in_un_pv_r = 0
      lc_mtx_c_sa = mtx_c_sa(c_n)
      lc_mtx_c_fh = mtx_c_sa(c_n+1)-1
      el_n = lc_mtx_c_sa
      go to 510
c
c     Look for an entry in the column which is in a singleton row
c     ---a row for which r_k_a(r_n) = -2 (since r_k_a(r_n) is
c     initialised to -1)
c
 500  continue
      if (el_n .ge. lc_mtx_c_fh) go to 600
      el_n = el_n + 1
 510  continue
      r_n = mtx_r_ix(el_n)
      if (r_k_a(r_n) .ge.  0) go to 500
      if (r_k_a(r_n) .eq. -2) then
c
c     Row singleton:
c     The column will appear before the bump so its L-eta can be
c     written now.
c
         n_ab_bump = n_ab_bump + 1
c
c     Check that there is enough space to write the eta
c
         i = lc_mtx_c_fh - lc_mtx_c_sa
         if (l_eta_el_n_p1 + i .gt. mx_n_eta_el) go to 4000
c
c     Increment the number of the last eta (in the file to date),
c     store the start of this eta, the pivotal row and the negative
c     reciprocal of the pivot, and then increment the record of the
c     number of the last eta element (in the file to date).
c
         l_eta_n = l_eta_n + 1
         eta_sa(l_eta_n) = l_eta_el_n_p1
         eta_ix(l_eta_el_n_p1) = r_n
         eta_v(l_eta_el_n_p1) = -1/mtx_r_v(el_n)
         l_eta_el_n_p1 = l_eta_el_n_p1 + 1
         lo_eta_pv_in_r(r_n) = l_eta_n
c
c     Set the row count to 1---why?
c
         r_k_a(r_n) = 1
         vr_in_r(r_n) = c_n
CM      IF (emsol_deb .EQ. 1) THEN
C?         if (rp_lvl .gt. 0) then
C?            if (rp_cn .ge. 0) write(rp_cn, 9100)
C?     &           ' Row singleton: ', c_n,
C?     &           ' Pivoting in row ', r_n
C?         endif
CM      ENDIF
         el_n = el_n - 1
 520     continue
c
c     Store the entries in the eta either
c     * up to the pivot---in the first pass
c     * or after the pivot---in the second pass when
c     . lc_mtx_c_sa = el_n + 2 and el_n = lc_mtx_c_fh.
c
         do 530, inner_el_n = lc_mtx_c_sa, el_n
            r_n = mtx_r_ix(inner_el_n)
c
c     If the row is non-pivotal, update the count of entries in
c     columns which may form the bump since the current column
c     is now before the bump.
c
            if (r_k_a(r_n) .lt. 0) r_k_a(r_n) = r_k_a(r_n) + 1
            eta_ix(l_eta_el_n_p1) = r_n
            eta_v(l_eta_el_n_p1) = mtx_r_v(inner_el_n)
            l_eta_el_n_p1 = l_eta_el_n_p1 + 1
 530     continue
         if (el_n .lt. lc_mtx_c_fh) then
c
c     If there are still entries to store then change the loop limits
c     and make a second pass.
c
            lc_mtx_c_sa = el_n + 2
            el_n = lc_mtx_c_fh
            go to 520
         end if
c
c     Remove the column from set 1 by swapping it with the last entry
c     in set 1 and decreasing the finish of set 1.
c     Jump to a line which is just a jump back to start with the next
c     set 1 entry---if there are any more.
c
         bs_c_at(se_1_en_n) = bs_c_at(se_1_fh)
         se_1_fh = se_1_fh - 1
         go to 700
      end if
c
c     Increment the count of entries in this column in non-pivotal rows
c     which, if equal to 0 (1) indicates singularity (column singleton).
c
      n_in_un_pv_r = n_in_un_pv_r + 1
      pv_r_cdd = r_n
c
c     Jump back to consider the next entry in the column.
c
      go to 500
c
c     All the entries in the column have been considered: none is in
c     a singleton row so see whether this column is a singleton.
c
 600  continue
      n_in_un_pv_r_m1 = n_in_un_pv_r - 1
      if (n_in_un_pv_r_m1 .eq. 0) then
c
c     Column singleton:
c     Exactly one nonzero in rows which have not yet been pivoted on.
c     Move entry into set 2 by swapping it with the last entry in set 1
c     and shifting the finish of set 1 and start of set 2 by 1.
c
         bs_c_at(se_1_en_n) = bs_c_at(se_1_fh)
         se_1_fh = se_1_fh - 1
         se_2_sa = se_2_sa - 1
         bs_c_at(se_2_sa) = c_n
c
c     Indicate that this set entry has been pivoted on by recording its
c     pivotal row number, zeroing its row count and that it is solved
c     for in this row.
c
         pv_r_n_or_mrt(se_2_sa) = pv_r_cdd
         r_k_a(pv_r_cdd) = 0
         vr_in_r(pv_r_cdd) = c_n
CM      IF (emsol_deb .EQ. 1) THEN
C?         if (rp_lvl .gt. 0) then
C?            if (rp_cn .ge. 0) write(rp_cn, 9100)
C?     &           ' Column singleton: ', c_n,
C?     &           ' Pivoting in row ', pv_r_cdd
C?         endif
CM      ENDIF
         go to 700
      else if (n_in_un_pv_r_m1 .gt. 0) then
c
c     More than one nonzero in rows which have not yet been pivoted on.
c     Column remains in set 1. Go back to consider next column in set 1
c     unless set 1 is finished.
c
         if (se_1_en_n .ge. se_1_fh) go to 710
         se_1_en_n = se_1_en_n + 1
         go to 420
      else
c
c     No nonzero in rows which have not yet been pivoted on.
c     Matrix is singular so mark the column as one to be removed.
c     Column is removed from set 1 by swapping it with the last entry in
c     set 1 and shifting the finish of set 1.
c
         n_inv_sing = n_inv_sing + 1
         call ems_se_iv_ty(2, rp_cn, c_n, ds, is)
         bs_c_at(se_1_en_n) = bs_c_at(se_1_fh)
         se_1_fh = se_1_fh - 1
      end if
 700  continue
c
c     If not all the columns in set 1 have been considered then jump
c     back to consider the next
c
      if (se_1_en_n .le. se_1_fh) go to 420
c
c     Jump to here if set 1 is known to have been finished when the
c     column is known to be a non-singleton.
c     Saves just an increase in se_1_en_n to se_1_fh+1.
c
 710  continue
CM      IF (emsol_deb .EQ. 1) THEN
C?      if (rp_lvl .gt. 0) then
C?         if (rp_cn .ge. 0) write(rp_cn, 9100)
C?     &        'Triang', n_triang_ps, ' n_ab_bump =', n_ab_bump,
C?     &        ' n_bw_bump =', se_2_fh - se_2_sa + 1
C?      endif
CM      ENDIF
c
c     If the number of columns in set 1 has been reduced on this pass,
c     repeat the triangularisation pass.
c
      if (se_1_fh .ne. se_1_fh_at_sa) go to 410
c
c     Store the end of the last eta (in the file to date).
c
      eta_sa(l_eta_n+1) = l_eta_el_n_p1
c
c     The triangularisation phase is finished.
c
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tom_tt_lvl2) call ems_tt_rec(-tom_triang_tt, -1)
CM      ENDIF
c
c     Bottom of second and subsequent triangularisation section.
c     ==================================================================
c     The bump is now finalised:
c     Set 1 contains the structurals in the bump
c     Set 2 contains the structurals after the bump (column singletons).
c     Set 3 contains the logicals
c
c     Between sets 1 and 2 are the structurals which are before the bump
c     (row singletons) and singular structurals.
c
      n_in_bump = se_1_fh - se_1_sa + 1
      n_bw_bump = se_2_fh - se_2_sa + 1
c
c     Record the (current) last lower triangular eta number and element
c     number+1
c
      l_lo_eta_n = l_eta_n
      l_lo_el_n_p1 = l_eta_el_n_p1
c
c     There is no bump so write in the etas for columns below the bump.
c
      if (se_1_fh .eq. 0) go to 3700
c
c     Get column merit counts for set 1: this is the sum of counts of
c     the non-pivotal rows in each column.
c
      do 820, se_1_en_n = se_1_sa, se_1_fh
         c_n = bs_c_at(se_1_en_n)
         mrt_k = 0
         do 810, el_n = mtx_c_sa(c_n), mtx_c_sa(c_n+1)-1
            r_n = mtx_r_ix(el_n)
            if (r_k_a(r_n) .lt. 0) mrt_k = mrt_k - (r_k_a(r_n)+1)
 810      continue
         pv_r_n_or_mrt(se_1_en_n) = mrt_k
 820  continue
CM      IF (emsol_deb .EQ. 1) THEN
C?      if (rp_lvl .gt. 2) then
C?         do 830, se_1_en_n = se_1_sa, se_1_fh
C?            if (rp_cn .ge. 0) write(rp_cn, 9110)'before_sort = ',
C?     &           bs_c_at(se_1_en_n), pv_r_n_or_mrt(se_1_en_n)
C? 830     continue
C?      endif
CM      ENDIF
c     =================================================================
c     Top of shell sort of set 1 columns by increasing merit count.
c
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tom_tt_lvl2) call ems_tt_rec(tom_srt_tt, -1)
CM      ENDIF
      stp = 1
 910  continue
      stp = 3*stp + 1
      if (se_1_fh .ge. stp) go to 910
      do 940, k = 1, i_inf
         stp = stp/3
         if (stp .lt. 1) go to 950
         do 930, i = stp+1, se_1_fh
            mrt_k = pv_r_n_or_mrt(i)
            if (pv_r_n_or_mrt(i-stp) .le. mrt_k) go to 930
            c_n = bs_c_at(i)
            se_1_en_n = i
 920        continue
            pv_r_n_or_mrt(se_1_en_n) = pv_r_n_or_mrt(se_1_en_n-stp)
            bs_c_at(se_1_en_n) = bs_c_at(se_1_en_n-stp)
            se_1_en_n = se_1_en_n - stp
            if (se_1_en_n .gt. stp) then
               if (pv_r_n_or_mrt(se_1_en_n-stp) .gt. mrt_k)  go to 920
            end if
            pv_r_n_or_mrt(se_1_en_n) = mrt_k
            bs_c_at(se_1_en_n) = c_n
 930      continue
 940  continue
 950  continue
CM      IF (emsol_deb .EQ. 1) THEN
C?      if (rp_lvl .gt. 2) then
C?         do 960, se_1_en_n = se_1_sa, se_1_fh
C?            if (rp_cn .ge. 0) write(rp_cn, 9110)'after_sort =',
C?     &           bs_c_at(se_1_en_n), pv_r_n_or_mrt(se_1_en_n)
C? 960     continue
C?      endif
CM      ENDIF
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tom_tt_lvl2) call ems_tt_rec(-tom_srt_tt, -1)
CM      ENDIF
c
c     Bottom of shell sort of set 1 columns by increasing merit count.
c     =================================================================
c
c     Set up for writing bump etas; below bump not yet written out.
c     Bump lo elements written to continuation of lo part of file.
c     Bump up elements written to end of file. Temporary values assigned
c     to l_eta_n, to use FTRAN as half-tran. Proper
c     values later copied back.
c
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tom_tt_lvl2) call ems_tt_rec(tom_bmp_inv_tt, -1)
CM      ENDIF
c      f_bmp_lo_eta_n = l_eta_n + 1
c      f_bmp_lo_el_n = l_eta_el_n_p1
      f_bmp_lo_eta_n = l_lo_eta_n + 1
      f_bmp_lo_el_n = l_lo_el_n_p1
c
c     Zero the work vector
c
      do 1010, r_n = 1, n_r
         wk_a(r_n) = 0
 1010 continue
c
c     n_eta_el_qy is ?
c
      n_eta_el_qy = 0
 
      l_up_el_n_m1 = mx_n_eta_el
      l_up_eta_n = mx_n_eta + 1
      eta_sa(mx_n_eta+1) = mx_n_eta_el+1
c
c     Loop over the sorted columns in the bump.
c
      n_mv_c = 0
CM      IF (emsol_da .EQ. 1) THEN
C?c      bmp_dim = se_1_fh - se_1_sa + 1
CM      ENDIF
      do 1500, se_1_en_n = se_1_sa, se_1_fh
 1100    continue
         c_n = bs_c_at(se_1_en_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?         if (rp_lvl .gt. 0) then
C?            if (rp_cn .ge. 0) write(rp_cn, 9000)
C?     &           '1: Set 2 entry ', se_1_en_n, ' Column ', c_n
C?         endif
CM      ENDIF
CM      IF (emsol_deb .EQ. 1) THEN
C?        if (rp_lvl .gt. 0) then
C?           if (rp_cn .ge. 0) write(rp_cn, 9110)'after sort =', c_n
C?        endif
CM      ENDIF
         d_l_up_el_n_m1 = l_up_el_n_m1
         d_l_lo_el_n_p1 = l_lo_el_n_p1
c
c     Check that there is space for n_r+1 eta elements.
c
CM      IF (emsol_deb .EQ. 1) THEN
C?        if (rp_lvl .gt. 0) then
C?           if (rp_cn .ge. 0) write(rp_cn, 9100)
C?     &          ' d_l_up_el_n_m1 =', d_l_up_el_n_m1,
C?     &          ' d_l_lo_el_n_p1 =', d_l_lo_el_n_p1,
C?     &          ' n_r =', n_r
C?        endif
CM      ENDIF
         if (d_l_up_el_n_m1 - d_l_lo_el_n_p1 .lt. n_r) then
CM      IF (emsol_tt .EQ. 1) THEN
C?            if (g_tom_tt_lvl2) call ems_tt_rec(tom_bmp_inv_tt, -1)
CM      ENDIF
            go to 4000
         endif
c
c     ==================================================================
c     Top of FIRST unpack matrix column section
c
c     Unpack the matrix column, storing its values in the work array and
c     each row index in the U-eta or L-eta, according to whether the row
c     is pivotal or not.
c
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (g_tom_tt_lvl3) call ems_tt_rec(tom_unpk_mtx_c1_tt, -1)
CM      ENDIF
         fm_bmp_lo_eta_n = l_lo_eta_n+1
c        i = f_bmp_lo_el_n
         do 1110, el_n = mtx_c_sa(c_n), mtx_c_sa(c_n+1)-1
            r_n = mtx_r_ix(el_n)
            wk_a(r_n) = mtx_r_v(el_n)
            fm_bmp_lo_eta_n = min(lo_eta_pv_in_r(r_n), fm_bmp_lo_eta_n)
            if (r_k_a(r_n) .ge. 0) then
c
c     Row is pivotal so this entry is part of the U-eta.
c
               if (d_l_up_el_n_m1 .lt. 0) then
CM      IF (emsol_deb .EQ. 1) THEN
C?                  call ems_baso(lc_rt_cod, ds, 26, 1)
C?                  if (rp_lvl .gt. 0) then
C?                     if (rp_cn .ge. 0) write(rp_cn, 9899)
C?     &                    eta_ix(d_l_up_el_n_m1)
C?                  endif
CM      ENDIF
                  rt_cod = ior(rt_cod, tom_inv_er_bt)
               endif
               eta_ix(d_l_up_el_n_m1) = r_n
CM      IF (emsol_deb .EQ. 1) THEN
C?               if (rp_lvl .gt. 0) then
C?                  if (rp_cn .ge. 0) write(rp_cn, 9220)
C?     &                 'unpk_upper  i, eta_ix(i), wk_a(eta_ix(i))',
C?     &                 d_l_up_el_n_m1, r_n, wk_a(r_n)
C?               endif
CM      ENDIF
               d_l_up_el_n_m1 = d_l_up_el_n_m1 - 1
               go to 1110
            end if
c
c     Row is non-pivotal so this is entry is part of the L-eta or pivot
c
            eta_ix(d_l_lo_el_n_p1) = r_n
CM      IF (emsol_deb .EQ. 1) THEN
C?            if (rp_lvl .gt. 0) then
C?               if (rp_cn .ge. 0) write(rp_cn, 9220)
C?     &              'unpk_lower  i, r_n, wk_a(r_n)',
C?     &              d_l_lo_el_n_p1, r_n, wk_a(r_n)
C?            endif
CM      ENDIF
            d_l_lo_el_n_p1 = d_l_lo_el_n_p1 + 1
 1110    continue
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (g_tom_tt_lvl3) call ems_tt_rec(-tom_unpk_mtx_c1_tt, -1)
CM      ENDIF
CM      IF (emsol_deb .EQ. 1) THEN
C?        if (rp_lvl .gt. 0) then
C?           if (rp_cn .ge. 0) write(rp_cn, 9100)
C?     &          ' d_l_up_el_n_m1 =', d_l_up_el_n_m1,
C?     &          ' d_l_lo_el_n_p1 =', d_l_lo_el_n_p1
C?        endif
CM      ENDIF
c
c     End of FIRST unpack matrix column section
c     ==================================================================
c
c     ==================================================================
c     Start of FIRST FTRAN section
c
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (g_tom_tt_lvl3) call ems_tt_rec(tom_ftran1_tt, -1)
CM      ENDIF
         n_eta_el_qy = 4*n_eta_el_qy
CM      IF (emsol_da .EQ. 1) THEN
C?c         n_ty_1_ftran = n_ty_1_ftran + 1
CM      ENDIF
         if (sus_ftran_mx_n_op .lt. 0) then
c
c     Don't do any super-sparse FTRAN operations
c
            fm_bmp_lo_eta_n = f_bmp_lo_eta_n
            goto 1250
         else
c
c     Jump to the end of FTRAN if no operations need be applied.
c
            if (fm_bmp_lo_eta_n .gt. l_lo_eta_n) goto 1275
c
c     Skip the super-sparse FTRAN operation if none is to be applied
c     (ie all that is being done is modify the first parameter of the
c     normal loop).
c
            if (sus_ftran_mx_n_op .eq. 0) goto 1250
         endif
c
c     Apply the first sus_ftran_mx_n_op etas by exploiting
c     lo_eta_pv_in_r to skip etas
c
         sus_ftran_n_op = 0
 1200    continue
         sus_ftran_n_op = sus_ftran_n_op + 1
CM      IF (emsol_da .EQ. 1) THEN
C?c
C?c     Decrementing n_ftran_eta_skp_n is necessary since all etas before
C?c     the ultimate value of fm_bmp_lo_eta_n are assumed to be skipped
C?c     in the accounting below 1275
C?c
C?c         n_ftran_eta_skp_n = n_ftran_eta_skp_n - 1
CM      ENDIF
         eta_n = fm_bmp_lo_eta_n
         el_n = eta_sa(eta_n)
         r_n = eta_ix(el_n)
c
c     Better to hit the occasional zero in the RHS than have to test for
c     zero every time in loop 1220
c
         if (wk_a(r_n) .eq. zero) goto 1210
         lc_v = wk_a(r_n)*eta_v(el_n)
         wk_a(r_n) = lc_v
         n_eta_el_qy =
     &        n_eta_el_qy + eta_sa(eta_n+1)-1 - eta_sa(eta_n)
         do 1205, el_n = el_n+1, eta_sa(eta_n+1)-1
            r_n = eta_ix(el_n)
            if (wk_a(r_n) .eq. zero) then
               wk_a(r_n) = lc_v*eta_v(el_n)
               if (r_k_a(r_n) .ge. 0) then
CM      IF (emsol_deb .EQ. 1) THEN
C?                  if (rp_lvl .gt. 1) then
C?                     if (rp_cn .ge. 0) write(rp_cn, 9220)
C?     &                    ' New Upper ',
C?     &                    d_l_up_el_n_m1, r_n, wk_a(r_n)
C?                  endif
CM      ENDIF
                  eta_ix(d_l_up_el_n_m1) = r_n
                  d_l_up_el_n_m1 = d_l_up_el_n_m1 - 1
                  go to 1205
               end if
CM      IF (emsol_deb .EQ. 1) THEN
C?               if (rp_lvl .gt. 1) then
C?                  if (rp_cn .ge. 0) write(rp_cn, 9220)
C?     &                 ' New Lower ',
C?     &                 d_l_lo_el_n_p1, r_n, wk_a(r_n)
C?               endif
CM      ENDIF
               eta_ix(d_l_lo_el_n_p1) = r_n
               d_l_lo_el_n_p1 = d_l_lo_el_n_p1 + 1
               go to 1205
            end if
            wk_a(r_n) = wk_a(r_n) + lc_v*eta_v(el_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?            if (rp_lvl .gt. 2) then
C?               if (rp_cn .ge. 0) write(rp_cn, 9220)
C?     &              ' Updated value ',
C?     &              d_l_lo_el_n_p1, r_n, wk_a(r_n)
C?            endif
CM      ENDIF
            if (wk_a(r_n) .ne. zero) then
               if (lo_eta_pv_in_r(r_n) .gt. eta_n) fm_bmp_lo_eta_n =
     &              min(lo_eta_pv_in_r(r_n), fm_bmp_lo_eta_n)
            endif
 1205    continue
 1210    continue
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (g_tom_tt_lvl3) call ems_tt_rec(tom_ftran_eta_srch_tt, -1)
CM      ENDIF
         fm_bmp_lo_eta_n = l_lo_eta_n+1
c
c     Go through the entries which are (potentially) in the U-eta,
c     looking to see which eta (if any) needs to be applied next
c     No entries which are (potentially) in the L-eta can have a pivot
c     in that row.
c
         do 1220, el_n = l_up_el_n_m1, d_l_up_el_n_m1+1, -1
            r_n = eta_ix(el_n)
            if (lo_eta_pv_in_r(r_n) .gt. eta_n) fm_bmp_lo_eta_n =
     &              min(lo_eta_pv_in_r(r_n), fm_bmp_lo_eta_n)
 1220    continue
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (g_tom_tt_lvl3) call ems_tt_rec(-tom_ftran_eta_srch_tt, -1)
CM      ENDIF
c
c     Jump to the end of FTRAN if no further operations need be applied.
c
         if (fm_bmp_lo_eta_n .gt. l_lo_eta_n) goto 1275
c
c     Jump to the normal loop if the limit on the number of super-sparse
c     FTRAN operations has been reached
c
         if (sus_ftran_n_op .lt. sus_ftran_mx_n_op) goto 1200
 1250    continue
         do 1260, eta_n = fm_bmp_lo_eta_n, l_lo_eta_n
            el_n = eta_sa(eta_n)
            r_n = eta_ix(el_n)
CM      IF (emsol_da .EQ. 1) THEN
C?            if (wk_a(r_n) .eq. zero) then
C?c               n_ftran_eta_skp_en = n_ftran_eta_skp_en + 1
C?               goto 1260
C?            endif
CM      ELSE
            if (wk_a(r_n) .eq. zero) go to 1260
CM      ENDIF
            lc_v = wk_a(r_n)*eta_v(el_n)
            wk_a(r_n) = lc_v
            n_eta_el_qy =
     &           n_eta_el_qy + eta_sa(eta_n+1)-1 - eta_sa(eta_n)
            do 1255, el_n = el_n + 1, eta_sa(eta_n+1)-1
               r_n = eta_ix(el_n)
               if (wk_a(r_n) .eq. zero) then
c
c     new element
c
                  wk_a(r_n) = lc_v*eta_v(el_n)
                  if (r_k_a(r_n) .ge. 0) then
CM      IF (emsol_deb .EQ. 1) THEN
C?                     if (rp_lvl .gt. 1) then
C?                        if (rp_cn .ge. 0) write(rp_cn, 9220)
C?     &                       ' New Upper ',
C?     &                       d_l_up_el_n_m1, r_n, wk_a(r_n)
C?                     endif
CM      ENDIF
c
c     part of upper
c
                     eta_ix(d_l_up_el_n_m1) = r_n
                     d_l_up_el_n_m1 = d_l_up_el_n_m1 - 1
                     go to 1255
                  end if
c
c     pivot element or part of lower
c
CM      IF (emsol_deb .EQ. 1) THEN
C?                  if (rp_lvl .gt. 1) then
C?                     if (rp_cn .ge. 0) write(rp_cn, 9220)
C?     &                    ' New Lower ',
C?     &                    d_l_lo_el_n_p1, r_n, wk_a(r_n)
C?                  endif
CM      ENDIF
                  eta_ix(d_l_lo_el_n_p1) = r_n
                  d_l_lo_el_n_p1 = d_l_lo_el_n_p1 + 1
                  go to 1255
               end if
c
c     element previously recorded
c
               wk_a(r_n) = wk_a(r_n) + lc_v*eta_v(el_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?               if (rp_lvl .gt. 2) then
C?                  if (rp_cn .ge. 0) write(rp_cn, 9220)
C?     &                 ' Updated value ',
C?     &                 d_l_lo_el_n_p1, r_n, wk_a(r_n)
C?               endif
CM      ENDIF
 1255       continue
 1260    continue
c
c     Jump to here if no etas need to be applied (in the loop)
c
 1275    continue
CM      IF (emsol_da .EQ. 1) THEN
C?c         if (fm_bmp_lo_eta_n .lt. f_bmp_lo_eta_n) then
C?c            print*, 'fm_bmp_lo_eta_n .lt. f_bmp_lo_eta_n',
C?c     &           fm_bmp_lo_eta_n, f_bmp_lo_eta_n
C?c            stop
C?c         endif
C?c         n_ftran_eta = l_lo_eta_n - f_bmp_lo_eta_n + 1
C?c         if (fm_bmp_lo_eta_n .le. l_lo_eta_n) then
C?c            n_ftran_eta_skp_n =
C?c     &           n_ftran_eta_skp_n + (fm_bmp_lo_eta_n-f_bmp_lo_eta_n)
C?c         else
C?c            n_ftran_eta_skp_n = n_ftran_eta_skp_n + n_ftran_eta
C?c         endif
C?c         su_n_ftran_eta = su_n_ftran_eta + n_ftran_eta
CM      ENDIF
c
c     Maintain a running average of the number of eta entries inspected
c
         n_eta_el_qy = n_eta_el_qy/5
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (g_tom_tt_lvl3) call ems_tt_rec(-tom_ftran1_tt, -1)
CM      ENDIF
CM      IF (emsol_deb .EQ. 1) THEN
C?        if (rp_lvl .gt. 0) then
C?           if (rp_cn .ge. 0) write(rp_cn, 9100)
C?     &          ' d_l_up_el_n_m1 =', d_l_up_el_n_m1,
C?     &          ' d_l_lo_el_n_p1 =', d_l_lo_el_n_p1
C?        endif
CM      ENDIF
c
c     End of FIRST FTRAN section
c     ==================================================================
c
c     Copy values into eta_v(el_n) and zero entries in wk_a(r)
c
CM      IF (emsol_deb .EQ. 1) THEN
C?        if (rp_lvl .gt. 0) then
C?           if (rp_cn .ge. 0) write(rp_cn, 9100)
C?     &          ' l_up_el_n =', l_up_el_n
C?        endif
CM      ENDIF
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (g_tom_tt_lvl3) call ems_tt_rec(tom_bmp_cp1_tt, -1)
CM      ENDIF
c
c     Although l_up_el_n is updated in the copying code below, it is
c     not necessary to preserve the value once d_l_up_el_n_m1 has been
c     assigned: it is this value which is used above to insert the
c     indices of potential U-eta entries
c
         l_up_el_n = d_l_up_el_n_m1 + 1
         el_n = l_up_el_n_m1
 1300    if (el_n .ge. l_up_el_n) then
            r_n = eta_ix(el_n)
            lc_v = wk_a(r_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?            if (rp_lvl .gt. 0) then
C?               if (rp_cn .ge. 0) write(rp_cn, 9220)
C?     &              'upper  el_n, r_n, wk_a(r_n)',
C?     &              el_n, r_n, lc_v
C?            endif
CM      ENDIF
            wk_a(r_n) = 0
            if (abs(lc_v) .lt. wr_eta_tl) then
c
c     Element too small delete it and replace with last element
c
               eta_ix(el_n) = eta_ix(l_up_el_n)
               l_up_el_n = l_up_el_n + 1
               go to 1300
            end if
            eta_v(el_n) = lc_v
            el_n = el_n - 1
            go to 1300
         end if
c
c     Copy values into eta_v(el_n), find max element and lowest row
c     count and zero entries in wk_a(r_n).
c
         mn_el_z = pv_tl
         c_mx = zero
         mn_k = -i_inf
         l_eta_el_n = d_l_lo_el_n_p1 - 1
         el_n = l_lo_el_n_p1
CM      IF (emsol_deb .EQ. 1) THEN
C?        if (rp_lvl .gt. 0) then
C?           if (rp_cn .ge. 0) write(rp_cn, 9100)
C?     &          ' l_eta_el_n =', l_eta_el_n
C?        endif
CM      ENDIF
CM      IF (emsol_deb .EQ. 1) THEN
C?         if (rp_lvl .gt. 0) then
C?            if (rp_cn .ge. 0) write(rp_cn, 9110)
C?     &           ' Before 1310: el_n = ', el_n
C?         endif
CM      ENDIF
 1310    if (el_n .le. l_eta_el_n) then
            r_n = eta_ix(el_n)
            lc_v = wk_a(r_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?            if (rp_lvl .gt. 0) then
C?               if (rp_cn .ge. 0) write(rp_cn, 9220)
C?     &              'lower  el_n, r_n, wk_a(r_n)',
C?     &              el_n, r_n, lc_v
C?            endif
CM      ENDIF
            abs_lc_v = abs(lc_v)
            wk_a(r_n) = 0
            if (abs_lc_v .lt. wr_eta_tl) then
c
c     Element too small.  Replace with element at l_eta_el_n.
c
               eta_ix(el_n) = eta_ix(l_eta_el_n)
               l_eta_el_n = l_eta_el_n - 1
CM      IF (emsol_deb .EQ. 1) THEN
C?               if (rp_lvl .gt. 0) then
C?                  if (rp_cn .ge. 0) write(rp_cn, 9100)
C?     &                 ' Jumping back to 1310: el_n = ', el_n,
C?     &                 ' l_eta_el_n = ', l_eta_el_n
C?               endif
CM      ENDIF
               go to 1310
            end if
            eta_v(el_n) = lc_v
            el_n = el_n + 1
CM      IF (emsol_deb .EQ. 1) THEN
C?            if (rp_lvl .gt. 0) then
C?               if (rp_cn .ge. 0) write(rp_cn, 9200)
C?     &              ' abs_lc_v, mn_el_z = ', abs_lc_v, mn_el_z
C?            endif
CM      ENDIF
            if (abs_lc_v .le. mn_el_z) go to 1310
            if (abs_lc_v .gt. c_mx)  c_mx = abs_lc_v
CM      IF (emsol_deb .EQ. 1) THEN
C?            if (rp_lvl .gt. 0) then
C?               if (rp_cn .ge. 0) write(rp_cn, 9110)
C?     &              ' r_k_a(r_n), mn_k ', r_k_a(r_n), mn_k
C?            endif
CM      ENDIF
            if (r_k_a(r_n) .le. mn_k)  go to 1310
            mn_k = r_k_a(r_n)
            bst_eta_el_n = el_n - 1
CM      IF (emsol_deb .EQ. 1) THEN
C?            if (rp_lvl .gt. 0) then
C?               if (rp_cn .ge. 0) write(rp_cn, 9110)
C?     &              ' mn_k, bst_eta_el_n = ', mn_k, bst_eta_el_n
C?            endif
CM      ENDIF
            go to 1310
         end if
         if (tom_inv_sing_msk .gt. 0) then
            if (se_1_en_n .eq. se_1_fh .and.
     &           n_inv_sing .eq. 0 .and.
     &           el_n-1 .ge. l_lo_el_n_p1) then
               if (el_n-1 .ne. l_lo_el_n_p1) then
CM      IF (emsol_deb .EQ. 1) THEN
C?                  if (rp_lvl .gt. 0) then
C?                     if (rp_cn .ge. 0) write(rp_cn, 9400)
C?     &                    el_n-1, l_lo_el_n_p1
C?                  endif
C?                  rt_cod = ior(rt_cod, tom_inv_rpt_bt)
CM      ELSE
                  rt_cod = ior(rt_cod, tom_inv_er_bt)
CM      ENDIF
                  goto 7000
               endif
            endif
         endif
         if (mn_k .eq. -i_inf) then
CM      IF (emsol_deb .EQ. 1) THEN
C?            if (rp_lvl .gt. 0) then
C?               if (rp_cn .ge. 0) write(rp_cn, 9930)c_n, mn_el_z
C?               if (rp_cn .ge. 0) write(rp_cn, 9940)
C?               do 1320, r_n = 1, n_r
C?                  if (r_k_a(r_n) .ne. 0 .or. wk_a(r_n) .ne. 0) then
C?                     if (rp_cn .ge. 0) write(rp_cn, 9950)
C?     &                    r_n, r_k_a(r_n), wk_a(r_n)
C?                  end if
C? 1320          continue
C?            endif
CM      ENDIF
            if (
     &           n_inv_sing .eq. 0 .and.
     &           el_n-1 .eq. l_lo_el_n_p1) then
c
c     There have been no previous singularities---NB Should be able to
c     handle situation when structural singularities are detected
c     before/after the bump.
c     There is a nonzero in the pivotal position.
c
               if (
     &              iand(tom_inv_sing_msk, tom_inv_sing_tl_ck) .ne. 0
     &              .or.
     &              iand(tom_inv_sing_msk, tom_inv_sing_tl_alw) .ne. 0)
     &              then
                  call ems_tom_inv_ck_alw_l_pv(
     &                 pv_ok, mv_c, .true.,
     &                 rp_cn, 1, n_mv_c,
     &                 n_r, n_c, mx_n_c, n_a_el, mx_n_eta, mx_n_eta_el,
     &                 eta_v(l_lo_el_n_p1), eta_ix(l_lo_el_n_p1), c_n,
     &                 l_up_eta_n, l_up_el_n,
     &                 se_1_en_n, se_1_fh,
     &                 se_2_sa, se_2_fh,
     &                 vr_in_r, bs_c_at, pv_r_n_or_mrt,
     &                 eta_v, eta_ix, eta_sa,
     &                 mtx_r_v, mtx_r_ix, mtx_c_sa,
     &                 wk_a, ck_l_pv_wk_a,
     &                 l_pv_rsdu_er_tl)
                  if (mv_c) goto 1100
                  if (pv_ok) then
                     r_n = eta_ix(l_lo_el_n_p1)
                     lc_v = eta_v(l_lo_el_n_p1)
                     abs_lc_v = abs(lc_v)
                     c_mx = abs_lc_v
                     mn_k = r_k_a(r_n)
                     bst_eta_el_n = l_lo_el_n_p1
                     goto 1325
                  endif
               endif
            endif
            n_inv_sing = n_inv_sing + 1
CM      IF (emsol_tt .EQ. 1) THEN
C?            if (g_tom_tt_lvl3) call ems_tt_rec(-tom_bmp_cp1_tt, -1)
CM      ENDIF
            call ems_se_iv_ty(3, rp_cn, c_n, ds, is)
            go to 1500
         end if
c
c     No singularity is suspected
c
         if (se_1_en_n .eq. se_1_fh .and.
     &        iand(tom_inv_sing_msk, tom_inv_sing_al_ck) .eq.
     &        tom_inv_sing_al_ck) then
c
c     Check the last pivot.
c
            call ems_tom_inv_ck_alw_l_pv(
     &           pv_ok, mv_c, .false.,
     &           rp_cn, 1, n_mv_c,
     &           n_r, n_c, mx_n_c, n_a_el, mx_n_eta, mx_n_eta_el,
     &           eta_v(l_lo_el_n_p1), eta_ix(l_lo_el_n_p1), c_n,
     &           l_up_eta_n, l_up_el_n,
     &           se_1_en_n, se_1_fh,
     &           se_2_sa, se_2_fh,
     &           vr_in_r, bs_c_at, pv_r_n_or_mrt,
     &           eta_v, eta_ix, eta_sa,
     &           mtx_r_v, mtx_r_ix, mtx_c_sa,
     &           wk_a, ck_l_pv_wk_a,
     &           l_pv_rsdu_er_tl)
         endif
c
c    No singularity is suspected or the pivot has been checked
c
 1325    continue
         c_rlv_mn = c_rlv_tl * c_mx
         if (abs(eta_v(bst_eta_el_n)) .lt. c_rlv_mn) then
            mn_el_z = c_rlv_mn
            n_redo_c = n_redo_c + 1
            mn_k = -i_inf
            do 1330, el_n = l_lo_el_n_p1,  l_eta_el_n
               if (r_k_a(eta_ix(el_n)) .le. mn_k
     &              .or. abs(eta_v(el_n)) .le. mn_el_z)  go to 1330
               mn_k = r_k_a(eta_ix(el_n))
               bst_eta_el_n = el_n
 1330       continue
         end if
         d_l_lo_el_n_p1 = l_eta_el_n + 1
         d_l_up_el_n_m1 = l_up_el_n - 1
         neg_rcp_pv_v = -1/eta_v(bst_eta_el_n)
         pv_r_cdd = eta_ix(bst_eta_el_n)
         vr_in_r(pv_r_cdd) = c_n
 
         if (d_l_up_el_n_m1 .lt. l_up_el_n_m1) then
c
c     There is an upper eta
c
            l_up_eta_n = l_up_eta_n - 1
            eta_sa(l_up_eta_n) = d_l_up_el_n_m1
            eta_ix(d_l_up_el_n_m1) = pv_r_cdd
            if (d_l_lo_el_n_p1 .gt. l_lo_el_n_p1+1) then
c
c     There is a lower eta (in addition to the pivot)
c
               eta_v(d_l_up_el_n_m1) = 1
               l_up_el_n_m1 = d_l_up_el_n_m1 - 1
               go to 1400
            end if
c
c     No lower eta.  Put pivot in upper
c
            eta_v(d_l_up_el_n_m1) = neg_rcp_pv_v
            l_up_el_n_m1 = d_l_up_el_n_m1 - 1
            go to 1430
         end if
c
c     There is no upper eta.  There is a lower eta.
c
         if (d_l_lo_el_n_p1 .eq. l_lo_el_n_p1+1) then
c
c     Singleton column
c
            if (abs(neg_rcp_pv_v - one) .lt. unit_v_tl) then
CM      IF (emsol_deb .EQ. 1) THEN
C?               if (rp_lvl .gt. 0) then
C?                  if (rp_cn .ge. 0) write(rp_cn, 9100)
C?     &                 'unit column=', c_n, ' row=', pv_r_cdd
C?               endif
CM      ENDIF
               bs_c_at(se_1_en_n) = -pv_r_cdd
               go to 1430
            end if
            eta_v(l_lo_el_n_p1) = neg_rcp_pv_v
c           eta_ix(l_lo_el_n_p1) is already ok.
            go to 1410
         end if
c
c     Non singleton in lower
c
 1400    continue
         eta_v(bst_eta_el_n) = eta_v(l_lo_el_n_p1)
         eta_ix(bst_eta_el_n) = eta_ix(l_lo_el_n_p1)
         eta_ix(l_lo_el_n_p1) = pv_r_cdd
         eta_v(l_lo_el_n_p1) = neg_rcp_pv_v
 
 1410    continue
         l_lo_eta_n = l_lo_eta_n + 1
         eta_sa(l_lo_eta_n+1) = d_l_lo_el_n_p1
         lo_eta_pv_in_r(pv_r_cdd) = l_lo_eta_n
c
c     Update row counts
c
         mrt_k_dl = r_k_a(pv_r_cdd) + 3
         if (mrt_k_dl .lt. se_1_en_n - se_1_fh)
     &        mrt_k_dl = se_1_en_n - se_1_fh
         do 1420, el_n = l_lo_el_n_p1+1, d_l_lo_el_n_p1-1
            r_n = eta_ix(el_n)
            r_k_a(r_n) = r_k_a(r_n) + mrt_k_dl
            if (r_k_a(r_n) .ge. 0) r_k_a(r_n) = -1
 1420    continue
 
         l_lo_el_n_p1 = d_l_lo_el_n_p1
 1430    continue
         r_k_a(pv_r_cdd) = 0
CM      IF (emsol_deb .EQ. 1) THEN
C?         if (rp_lvl .gt. 0) then
C?            if (rp_cn .ge. 0) write(rp_cn, 9100)
C?     &           ' Bump: ', c_n,
C?     &           ' Pivoting in row ', pv_r_cdd
C?         endif
CM      ENDIF
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (g_tom_tt_lvl3) call ems_tt_rec(-tom_bmp_cp1_tt, -1)
CM      ENDIF
         if (n_eta_el_qy .gt. n_r)  go to 2000
 1500 continue
      go to 3600
 
 2000 continue
CM      IF (emsol_deb .EQ. 1) THEN
C?      if (rp_lvl .gt. 0) then
C?         if (rp_cn .ge. 0) write(rp_cn, 9100)
C?     &        'entering 2nd part bump.  n_1st, n_2nd =',
C?     &        se_1_en_n - se_1_sa + 1, se_1_fh - se_1_en_n + 1
C?      endif
CM      ENDIF
      do 2500, se_1_en_n = se_1_en_n+1, se_1_fh
 2100    continue
         c_n = bs_c_at(se_1_en_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?         if (rp_lvl .gt. 0) then
C?            if (rp_cn .ge. 0) write(rp_cn, 9000)
C?     &           '2: Set 2 entry ', se_1_en_n, ' Column ', c_n
C?         endif
CM      ENDIF
CM      IF (emsol_deb .EQ. 1) THEN
C?         if (rp_lvl .gt. 0) then
C?            if (rp_cn .ge. 0) write(rp_cn, 9100)'After sort =', c_n
C?         endif
CM      ENDIF
 
c     ==================================================================
c     Start of SECOND unpack matrix column section
c
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tom_tt_lvl3) call ems_tt_rec(tom_unpk_mtx_c2_tt, -1)
CM      ENDIF
         do 2110, el_n = mtx_c_sa(c_n), mtx_c_sa(c_n+1)-1
            wk_a(mtx_r_ix(el_n)) = mtx_r_v(el_n)
 2110    continue
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tom_tt_lvl3) call ems_tt_rec(-tom_unpk_mtx_c2_tt, -1)
CM      ENDIF
c
c     End of SECOND unpack matrix column section
c     ==================================================================
 
c     ==================================================================
c     Start of SECOND FTRAN section
c
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (g_tom_tt_lvl3) call ems_tt_rec(tom_ftran2_tt, -1)
CM      ENDIF
CM      IF (emsol_da .EQ. 1) THEN
C?c         n_ty_2_ftran = n_ty_2_ftran + 1
C?c         n_ftran_eta = l_lo_eta_n - f_bmp_lo_eta_n + 1
C?c         su_n_ftran_eta = su_n_ftran_eta + n_ftran_eta
CM      ENDIF
         el_n = f_bmp_lo_el_n
         do 2260, eta_n = f_bmp_lo_eta_n, l_lo_eta_n
            el_n = eta_sa(eta_n)
            r_n = eta_ix(el_n)
CM      IF (emsol_da .EQ. 1) THEN
C?            if (wk_a(r_n) .eq. zero) then
C?c               n_ftran_eta_skp_en = n_ftran_eta_skp_en + 1
C?               goto 2260
C?            endif
CM      ELSE
            if (wk_a(r_n) .eq. zero) go to 2260
CM      ENDIF
            lc_v = wk_a(r_n)*eta_v(el_n)
            wk_a(r_n) = lc_v
            do 2255, el_n = el_n+1, eta_sa(eta_n+1)-1
               r_n = eta_ix(el_n)
               wk_a(r_n) = wk_a(r_n) + lc_v*eta_v(el_n)
 2255       continue
 2260    continue
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (g_tom_tt_lvl3) call ems_tt_rec(-tom_ftran2_tt, -1)
CM      ENDIF
c
c     End of SECOND FTRAN section
c     ==================================================================
c
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (g_tom_tt_lvl3) call ems_tt_rec(tom_bmp_cp2_tt, -1)
CM      ENDIF
         mn_el_z = pv_tl
c 2300    continue
         c_mx = zero
         mn_k = -i_inf
         d_l_lo_el_n_p1 = l_lo_el_n_p1
         d_l_up_el_n_m1 = l_up_el_n_m1
c
c        Check that there is space for n_r + 1 eta elements.
c
         if (d_l_up_el_n_m1 - d_l_lo_el_n_p1 .lt. n_r) then
CM      IF (emsol_tt .EQ. 1) THEN
C?            if (g_tom_tt_lvl3) call ems_tt_rec(-tom_bmp_cp2_tt, -1)
C?            if (g_tom_tt_lvl2) call ems_tt_rec(-tom_bmp_inv_tt, -1)
CM      ENDIF
            go to 4000
         endif
         do 2310, r_n = 1, n_r
            if (wk_a(r_n) .eq. zero) go to 2310
            lc_v = abs(wk_a(r_n))
            if (lc_v .lt. wr_eta_tl)  go to 2305
            if (r_k_a(r_n) .ge. 0) then
c
c     Part of upper
c
               eta_ix(d_l_up_el_n_m1) = r_n
               eta_v(d_l_up_el_n_m1) = wk_a(r_n)
               d_l_up_el_n_m1 = d_l_up_el_n_m1 - 1
               go to 2305
            end if
c
c     Pivot element or part of lower
c
            eta_ix(d_l_lo_el_n_p1) = r_n
            eta_v(d_l_lo_el_n_p1) = wk_a(r_n)
            d_l_lo_el_n_p1 = d_l_lo_el_n_p1 + 1
            if (lc_v .le. mn_el_z)  go to 2305
            if (lc_v .gt. c_mx)  c_mx = lc_v
            if (r_k_a(r_n) .gt. mn_k) then
               mn_k = r_k_a(r_n)
               bst_eta_el_n = d_l_lo_el_n_p1 - 1
            end if
 2305       continue
            wk_a(r_n) = 0
 2310    continue
         if (tom_inv_sing_msk .gt. 0) then
            if (se_1_en_n .eq. se_1_fh .and.
     &           n_inv_sing .eq. 0 .and.
     &           el_n-1 .ge. l_lo_el_n_p1) then
               if (el_n-1 .ne. l_lo_el_n_p1) then
CM      IF (emsol_deb .EQ. 1) THEN
C?                  if (rp_lvl .gt. 0) then
C?                     if (rp_cn .ge. 0) write(rp_cn, 9400)
C?     &                    el_n-1, l_lo_el_n_p1
C?                  endif
C?                  rt_cod = ior(rt_cod, tom_inv_rpt_bt)
CM      ELSE
                  rt_cod = ior(rt_cod, tom_inv_er_bt)
CM      ENDIF
                  goto 7000
               endif
            endif
         endif
         if (mn_k .eq. -i_inf) then
CM      IF (emsol_deb .EQ. 1) THEN
C?            if (rp_lvl .gt. 0) then
C?               if (rp_cn .ge. 0) write(rp_cn, 9930)c_n, mn_el_z
C?               if (rp_cn .ge. 0) write(rp_cn, 9940)
C?               do 2320, r_n = 1, n_r
C?                  if (rp_cn .ge. 0) write(rp_cn, 9950)
C?     &                 r_n, r_k_a(r_n), wk_a(r_n)
C? 2320          continue
C?            endif
CM      ENDIF
            if (
     &           n_inv_sing .eq. 0 .and.
     &           el_n-1 .eq. l_lo_el_n_p1) then
c
c     There have been no previous singularities---NB Should be able to
c     handle situation when structural singularities are detected
c     before/after the bump.
c     There is a nonzero in the pivotal position.
c
               if (
     &              iand(tom_inv_sing_msk, tom_inv_sing_tl_ck) .ne. 0
     &              .or.
     &              iand(tom_inv_sing_msk, tom_inv_sing_tl_alw) .ne. 0)
     &              then
                  l_up_el_n = d_l_up_el_n_m1 + 1
                  call ems_tom_inv_ck_alw_l_pv(
     &                 pv_ok, mv_c, .true.,
     &                 rp_cn, 2, n_mv_c,
     &                 n_r, n_c, mx_n_c, n_a_el, mx_n_eta, mx_n_eta_el,
     &                 eta_v(l_lo_el_n_p1), eta_ix(l_lo_el_n_p1), c_n,
     &                 l_up_eta_n, l_up_el_n,
     &                 se_1_en_n, se_1_fh,
     &                 se_2_sa, se_2_fh,
     &                 vr_in_r, bs_c_at, pv_r_n_or_mrt,
     &                 eta_v, eta_ix, eta_sa,
     &                 mtx_r_v, mtx_r_ix, mtx_c_sa,
     &                 wk_a, ck_l_pv_wk_a,
     &                 l_pv_rsdu_er_tl)
                  if (mv_c) goto 2100
                  if (pv_ok) then
                     r_n = eta_ix(l_lo_el_n_p1)
                     lc_v = eta_v(l_lo_el_n_p1)
                     abs_lc_v = abs(lc_v)
                     c_mx = abs_lc_v
                     mn_k = r_k_a(r_n)
                     bst_eta_el_n = l_lo_el_n_p1
                     goto 2340
                  endif
               endif
            endif
            n_inv_sing = n_inv_sing + 1
CM      IF (emsol_tt .EQ. 1) THEN
C?            if (g_tom_tt_lvl3) call ems_tt_rec(-tom_bmp_cp2_tt, -1)
CM      ENDIF
            call ems_se_iv_ty(4, rp_cn, c_n, ds, is)
            go to 2500
         end if
c
c     No singularity is suspected
c
         if (se_1_en_n .eq. se_1_fh .and.
     &        iand(tom_inv_sing_msk, tom_inv_sing_al_ck) .eq.
     &        tom_inv_sing_al_ck) then
c
c     Check the last pivot.
c
            l_up_el_n = d_l_up_el_n_m1 + 1
            call ems_tom_inv_ck_alw_l_pv(
     &           pv_ok, mv_c, .false.,
     &           rp_cn, 2, n_mv_c,
     &           n_r, n_c, mx_n_c, n_a_el, mx_n_eta, mx_n_eta_el,
     &           eta_v(l_lo_el_n_p1), eta_ix(l_lo_el_n_p1), c_n,
     &           l_up_eta_n, l_up_el_n,
     &           se_1_en_n, se_1_fh,
     &           se_2_sa, se_2_fh,
     &           vr_in_r, bs_c_at, pv_r_n_or_mrt,
     &           eta_v, eta_ix, eta_sa,
     &           mtx_r_v, mtx_r_ix, mtx_c_sa,
     &           wk_a, ck_l_pv_wk_a,
     &           l_pv_rsdu_er_tl)
         endif
c
c    No singularity is suspected or the pivot has been checked
c
 2340    continue
         c_rlv_mn = c_rlv_tl*c_mx
         if (abs(eta_v(bst_eta_el_n)) .lt. c_rlv_mn) then
            mn_el_z = c_rlv_mn
            n_redo_c = n_redo_c + 1
            mn_k = -i_inf
            do 2350, el_n = l_lo_el_n_p1, d_l_lo_el_n_p1 - 1
               if (r_k_a(eta_ix(el_n)) .le. mn_k
     &              .or. abs(eta_v(el_n)) .le. mn_el_z)  go to 2350
               mn_k = r_k_a(eta_ix(el_n))
               bst_eta_el_n = el_n
 2350       continue
         end if
         neg_rcp_pv_v = -1/eta_v(bst_eta_el_n)
         pv_r_cdd = eta_ix(bst_eta_el_n)
         vr_in_r(pv_r_cdd) = c_n
 
         if (d_l_up_el_n_m1 .lt. l_up_el_n_m1) then
c
c     There is an upper eta
c
            l_up_eta_n = l_up_eta_n - 1
            eta_sa(l_up_eta_n) = d_l_up_el_n_m1
            eta_ix(d_l_up_el_n_m1) = pv_r_cdd
            if (d_l_lo_el_n_p1 .gt. l_lo_el_n_p1+1) then
c
c     There is a lower eta (in addition to the pivot)
c
               eta_v(d_l_up_el_n_m1) = 1
               l_up_el_n_m1 = d_l_up_el_n_m1 - 1
               go to 2400
            end if
c
c     no lower eta.  put pivot in upper
c
            eta_v(d_l_up_el_n_m1) = neg_rcp_pv_v
            l_up_el_n_m1 = d_l_up_el_n_m1 - 1
            go to 2430
         end if
c
c     There is no upper eta.  there is a lower eta.
c
         if (d_l_lo_el_n_p1 .eq. l_lo_el_n_p1+1) then
c
c     singleton column
c
            if (abs(neg_rcp_pv_v - one) .lt. unit_v_tl) then
CM      IF (emsol_deb .EQ. 1) THEN
C?               if (rp_lvl .gt. 0) then
C?                  if (rp_cn .ge. 0) write(rp_cn, 9100)
C?     &                 '***unit column', c_n, ' row=', pv_r_cdd
C?               endif
CM      ENDIF
               bs_c_at(se_1_en_n) = -pv_r_cdd
               go to 2430
            end if
            eta_v(l_lo_el_n_p1) = neg_rcp_pv_v
c           eta_ix(l_lo_el_n_p1) is already ok.
            go to 2410
         end if
c
c     Non singleton in lower
c
 2400    continue
         eta_v(bst_eta_el_n) = eta_v(l_lo_el_n_p1)
         eta_ix(bst_eta_el_n) = eta_ix(l_lo_el_n_p1)
         eta_ix(l_lo_el_n_p1) = pv_r_cdd
         eta_v(l_lo_el_n_p1) = neg_rcp_pv_v
 
 2410    continue
         l_lo_eta_n = l_lo_eta_n + 1
         eta_sa(l_lo_eta_n+1) = d_l_lo_el_n_p1
         lo_eta_pv_in_r(pv_r_cdd) = l_lo_eta_n
c
c     Update row counts
c
         mrt_k_dl = r_k_a(pv_r_cdd) + 3
         if (mrt_k_dl .lt. se_1_en_n - se_1_fh)
     &        mrt_k_dl = se_1_en_n - se_1_fh
         do 2420, el_n = l_lo_el_n_p1 + 1, d_l_lo_el_n_p1 -  1
            r_n = eta_ix(el_n)
            r_k_a(r_n) = r_k_a(r_n) + mrt_k_dl
            if (r_k_a(r_n) .ge. 0)  r_k_a(r_n) = -1
 2420    continue
 
         l_lo_el_n_p1 = d_l_lo_el_n_p1
 2430    continue
         r_k_a(pv_r_cdd) = 0
CM      IF (emsol_deb .EQ. 1) THEN
C?         if (rp_lvl .gt. 0) then
C?            if (rp_cn .ge. 0) write(rp_cn, 9100)
C?     &           ' Bump: ', c_n,
C?     &           ' Pivoting in row ', pv_r_cdd
C?         endif
CM      ENDIF
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (g_tom_tt_lvl3) call ems_tt_rec(-tom_bmp_cp2_tt, -1)
CM      ENDIF
 2500 continue
 
 3600 continue
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tom_tt_lvl2) call ems_tt_rec(-tom_bmp_inv_tt, -1)
CM      ENDIF
c
c     Check that the code always passes through here if there has been
c     a (non-trivial) bump.
c
c     Assign l_eta_n and l_eta_el_n_p1. These will be truly the last of
c     their kind correct once the U-etas have been moved.
c
      l_eta_n = l_lo_eta_n
      l_eta_el_n_p1 = l_lo_el_n_p1
c
c     Merge Bump U with L etas
c
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tom_tt_lvl2) call ems_tt_rec(tom_cp_u_tt, -1)
CM      ENDIF
      dl_el_n = l_eta_el_n_p1 - (l_up_el_n_m1+1)
      do 3610, eta_n = l_up_eta_n, mx_n_eta
         l_eta_n = l_eta_n + 1
         r_n = eta_ix(eta_sa(eta_n))
         up_eta_pv_in_r(r_n) = l_eta_n
         eta_sa(l_eta_n) = eta_sa(eta_n) + dl_el_n
 3610 continue
      eta_sa(l_eta_n+1) = mx_n_eta_el+1 + dl_el_n
      do 3620, el_n = l_up_el_n_m1+1, mx_n_eta_el
         eta_v(l_eta_el_n_p1) = eta_v(el_n)
         eta_ix(l_eta_el_n_p1) = eta_ix(el_n)
         l_eta_el_n_p1 = l_eta_el_n_p1 + 1
 3620 continue
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tom_tt_lvl2) call ems_tt_rec(-tom_cp_u_tt, -1)
CM      ENDIF
 3700 continue
c
c     Write in below the bump U etas
c
      if (se_2_sa.gt.se_2_fh) go to 3800
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tom_tt_lvl2) call ems_tt_rec(tom_bw_wr_tt, -1)
CM      ENDIF
      do 3720, se_2_en_n = se_2_sa, se_2_fh
         c_n = bs_c_at(se_2_en_n)
         pv_r_cdd = pv_r_n_or_mrt(se_2_en_n)
         lc_mtx_c_sa = mtx_c_sa(c_n)
         lc_mtx_c_fh = mtx_c_sa(c_n+1)
         if (l_eta_el_n_p1 + lc_mtx_c_fh - lc_mtx_c_sa .gt.
     &        mx_n_eta_el) then
CM      IF (emsol_tt .EQ. 1) THEN
C?            if (g_tom_tt_lvl2) call ems_tt_rec(-tom_bw_wr_tt, -1)
CM      ENDIF
            go to 4000
         end if
         lc_mtx_c_fh = lc_mtx_c_fh - 1
         if (lc_mtx_c_fh .eq. lc_mtx_c_sa) then
            if (abs(mtx_r_v(lc_mtx_c_sa) + one) .le. unit_v_tl) then
CM      IF (emsol_deb .EQ. 1) THEN
C?               if (rp_lvl .gt. 0) then
C?                  if (rp_cn .ge. 0) write(rp_cn, 9100)
C?     &                 ' ?? unit col ', c_n, ' row=', pv_r_cdd
C?               endif
CM      ENDIF
               bs_c_at(se_2_en_n) = -pv_r_cdd
               go to 3720
            end if
         end if
         l_eta_n = l_eta_n + 1
         d_l_eta_el_n_p1 = l_eta_el_n_p1
         l_eta_el_n_p1 = l_eta_el_n_p1 + 1
         do 3710, el_n = lc_mtx_c_sa, lc_mtx_c_fh
            r_n = mtx_r_ix(el_n)
            if (r_n .ne. pv_r_cdd) then
               eta_ix(l_eta_el_n_p1) = r_n
               eta_v(l_eta_el_n_p1) = mtx_r_v(el_n)
               l_eta_el_n_p1 = l_eta_el_n_p1 + 1
            else
               eta_ix(d_l_eta_el_n_p1) = r_n
               eta_v(d_l_eta_el_n_p1) = -1/mtx_r_v(el_n)
               up_eta_pv_in_r(r_n) = l_eta_n
            end if
 3710    continue
         eta_sa(l_eta_n+1) = l_eta_el_n_p1
 3720 continue
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tom_tt_lvl2) call ems_tt_rec(-tom_bw_wr_tt, -1)
CM      ENDIF
 3800 continue
c
c     Insert logicals for deleted columns
c
      n_eta = l_eta_n
      if (n_inv_sing .gt. 0) then
CM      IF (emsol_deb .EQ. 1) THEN
C?         if (rp_lvl .gt. 0) then
C?            if (rp_cn .ge. 0) write(rp_cn, 9510)n_inv_sing
C?         endif
C?         if (rp_lvl .gt. 2) then
C?            if (rp_cn .ge. 0) write(rp_cn, 9100)'vr_in_r='
C?            if (rp_cn .ge. 0) write(rp_cn, 9300)(vr_in_r(i), i=1, n_r)
C?         endif
CM      ENDIF
         do 3810, r_n = 1, n_r
c1          if (vr_in_r(r_n) .eq. mx_n_c + r_n) then
c1            bs_c_at(r_n) = 0
c1          else
c1             bs_c_at(r_n) = 1
c1          end if
            if (vr_in_r(r_n) .ne. mx_n_c + r_n) then
               vr_in_r(r_n) = - vr_in_r(r_n)
            end if
 3810    continue
CM      IF (emsol_deb .EQ. 1) THEN
C?         if (rp_lvl .gt. 2) then
C?            if (rp_cn .ge. 0) write(rp_cn, 9100)'bs_c_at='
C?            if (rp_cn .ge. 0) write(rp_cn, 9300)(bs_c_at(i), i=1, n_r)
C?            if (rp_cn .ge. 0) write(rp_cn, 9100)'vr_in_r='
C?            if (rp_cn .ge. 0) write(rp_cn, 9300)(vr_in_r(i), i=1, n_r)
C?         endif
C?         if (rp_lvl .gt. 0) then
C?            if (rp_cn .ge. 0) write(rp_cn, 9100)'n_eta=', n_eta
C?         endif
CM      ENDIF
         do 3820, eta_n = 1, n_eta
c1          bs_c_at(eta_ix(eta_sa(eta_n))) = 0
            vr_in_r(eta_ix(eta_sa(eta_n))) =
     &           abs(vr_in_r(eta_ix(eta_sa(eta_n))))
 3820    continue
CM      IF (emsol_deb .EQ. 1) THEN
C?         if (rp_lvl .gt. 2) then
C?            if (rp_cn .ge. 0) write(rp_cn, 9100)'bs_c_at='
C?            if (rp_cn .ge. 0) write(rp_cn, 9300)(bs_c_at(i), i=1, n_r)
C?            if (rp_cn .ge. 0) write(rp_cn, 9100)'vr_in_r='
C?            if (rp_cn .ge. 0) write(rp_cn, 9300)(vr_in_r(i), i=1, n_r)
C?         endif
CM      ENDIF
         do 3830, r_n = 1, n_r
            if (bs_c_at(r_n) .lt. 0) then
               vr_in_r(-bs_c_at(r_n)) = -vr_in_r(-bs_c_at(r_n))
            end if
 3830    continue
CM      IF (emsol_deb .EQ. 1) THEN
C?         if (rp_lvl .gt. 2) then
C?            if (rp_cn .ge. 0) write(rp_cn, 9100)'bs_c_at='
C?            if (rp_cn .ge. 0) write(rp_cn, 9300)(bs_c_at(i), i=1, n_r)
C?            if (rp_cn .ge. 0) write(rp_cn, 9100)'vr_in_r='
C?            if (rp_cn .ge. 0) write(rp_cn, 9300)(vr_in_r(i), i=1, n_r)
C?         endif
CM      ENDIF
         n_inv_sing = n_lg
         do 3840, r_n = 1, n_r
c1          if (bs_c_at(r_n).ne.0) then
            if (vr_in_r(r_n).lt.0) then
               vr_in_r(r_n) = mx_n_c + r_n
               n_lg = n_lg + 1
               call ems_se_dv_ty(rp_cn, mx_n_c + r_n, ds, is)
c              line below would reset entire array to 0
c              bs_c_at(r_n) = 0
            end if
 3840    continue
         n_inv_sing = n_lg - n_inv_sing
CM      IF (emsol_deb .EQ. 1) THEN
C?         if (rp_lvl .gt. 2) then
C?            if (rp_cn .ge. 0) write(rp_cn, 9100)'vr_in_r='
C?            if (rp_cn .ge. 0)
C?     &           write(rp_cn, 9300)(vr_in_r(r_n), r_n=1, n_r)
C?         endif
CM      ENDIF
      end if
CM      IF (emsol_da .EQ. 1) THEN
C?c     if (su_n_ftran_eta .gt. 0) then
C?c         pct = 1d2*float(bmp_dim)/float(n_r)
C?c         write(rp_cn, 9250)bmp_dim, pct, n_ty_1_ftran, n_ty_2_ftran
C?c         n_no_apply_ftran_eta = n_ftran_eta_skp_n +
C?c     &        n_ftran_eta_skp_en
C?c         pct = 1d2*float(n_no_apply_ftran_eta)/float(su_n_ftran_eta)
C?c         write(rp_cn, 9260)n_no_apply_ftran_eta, su_n_ftran_eta, pct
C?c         if (n_no_apply_ftran_eta .ne. 0) then
C?c            pct = 1d2*float(n_ftran_eta_skp_n)/
C?c     &           float(n_no_apply_ftran_eta)
C?c            write(rp_cn, 9261)n_ftran_eta_skp_n,
C?c     &           n_no_apply_ftran_eta, pct
C?c            pct = 1d2*float(n_ftran_eta_skp_en)/
C?c     &           float(n_no_apply_ftran_eta)
C?c            write(rp_cn, 9262)n_ftran_eta_skp_en,
C?c     &           n_no_apply_ftran_eta, pct
C?c         endif
C?c      endif
CM      ENDIF
CM      IF (emsol_deb .EQ. 1) THEN
C?      if (rp_lvl .gt. 0) then
C?         if (rp_cn .ge. 0) write(rp_cn, 9100)'n_lg        =',
C?     &        n_lg
C?         if (rp_cn .ge. 0) write(rp_cn, 9100)'n_in_bump   =',
C?     &        n_in_bump
C?         if (rp_cn .ge. 0) write(rp_cn, 9100)'n_ab_bump   =',
C?     &        n_ab_bump
C?         if (rp_cn .ge. 0) write(rp_cn, 9100)'n_bw_bump   =',
C?     &        n_bw_bump
C?         if (rp_cn .ge. 0) write(rp_cn, 9100)'n_redo      =',
C?     &        n_redo_c
C?         if (rp_cn .ge. 0) write(rp_cn, 9100)'n_triang_ps =',
C?     &        n_triang_ps
C?      endif
CM      ENDIF
      go to 5000
 
 4000 continue
CM      IF (emsol_deb .EQ. 1) THEN
C?      if (rp_cn .ge. 0) write(rp_cn, 9520)
CM      ENDIF
      rt_cod = ior(rt_cod, tom_inv_no_po_bt)
      go to 7000
 
 5000 continue
      n_eta_el = l_eta_el_n_p1 - 1
      if (n_bs_non_ze .le. n_lg) then
         fill_fac = one
      else
         fill_fac = float(n_eta_el)/float(n_bs_non_ze-n_lg)
      endif
 7000 continue
      n_lo_eta = l_lo_eta_n
      return
CM      IF (emsol_deb .EQ. 1) THEN
C? 9000 format(/3(a, 2x, i9))
C? 9100 format(3(a, 2x, i9))
C? 9110 format(a, 2(2x, i9))
C? 9200 format(a, 2(2x, g11.4))
C? 9220 format(a, 2x, i7, 2x, i7, 2x, g11.4)
CM      ENDIF
CM      IF (emsol_da .EQ. 1) THEN
C?c 9250 format('Bump of dimension ', i7, ' (', f6.2, '%) required ',
C?c     &     /i7, ' type 1 FTRANs',
C?c     &     /i7, ' type 2 FTRANs')
C?c 9260 format('Avoided applying ', i9, ' of ', i9, ' L-etas in FTRAN (',
C?c     &     f6.2, '%)')
C?c 9261 format('                 ', i9, ' of ', i9, ' avoided (',
C?c     &     f6.2, '%) by skipping the eta number')
C?c 9262 format('                 ', i9, ' of ', i9, ' avoided (',
C?c     &     f6.2, '%) by skipping the eta entries')
CM      ENDIF
CM      IF (emsol_deb .EQ. 1) THEN
C? 9300 format((/10(2x, i7)))
C? 9510 format('Tomlin INVERT has discovered ', i7, ' singularities ')
C? 9520 format('No space for etas. Increasing space and reinverting')
C? 9910 format('Tomlin: Pass_1  c_n = ', i7)
C? 9920 format('Tomlin: Pass_2  c_n = ', i7)
C? 9930 format('Tomlin: In bump c_n = ', i7, '  mn_el_z =', g11.4)
C? 9940 format('      i   r_k_a(i)     wk_a(i)')
C? 9950 format(i7, 2x, i9, 2x, g11.4)
C? 9400 format('Tomlin: Choice of more than one last pivot!', 2(2x, i9))
C? 9899 format('Tomlin: d_l_up_el_n_m1 .lt. 0: ',
C?     &     'eta_ix(d_l_up_el_n_m1) = ', g11.4)
CM      ENDIF
      end
CM      IF (emsol_deb .EQ. 1) THEN
C?C->>> -----------------------------------------------> ems_ck_rpt_vr <<<
C?c     Checks that there are no repeat entries in a list of distinct
C?c     variables.
C?c
C?c     Not used (15/01/99)
C?c
C?      subroutine ems_ck_rpt_vr(er_fd, n_en, mx_vr_n, vr_ls)
C?      implicit none
C?      logical er_fd
C?      integer n_en, mx_vr_n, vr_ls(n_en)
C?      integer lc_mx_vr_n
C?      parameter (lc_mx_vr_n = 10000)
C?      integer te_i_a(lc_mx_vr_n)
C?      integer en_n, vr_n
C?
C?      er_fd = .false.
C?      if (mx_vr_n .gt. lc_mx_vr_n) goto 8000
C?      do 10, vr_n = 1, mx_vr_n
C?         te_i_a(vr_n) = 0
C? 10   continue
C?      do 20 en_n = 1, n_en
C?         vr_n = vr_ls(en_n)
C?         if (te_i_a(vr_n) .ne. 0) then
C?            er_fd = .true.
C?            write(*, 9000)vr_n, en_n, te_i_a(vr_n)
C?         endif
C?         te_i_a(vr_n) = en_n
C? 20   continue
C? 7100 continue
C?      return
C? 8000 continue
C?c      print*, 'Exceeded max variable number in ems_ck_rpt_vr'
C?      er_fd = .true.
C?      goto 7100
C? 9000 format(' Repeat of variable ', i7, ' in entry ', i7,
C?     &     ': Previously in entry ', i7)
C?      end
CM      ENDIF
 
C->>> -------------------------------------> ems_tom_inv_ck_alw_l_pv <<<
c     Checks and determines whether the last pivot should/would be
c     allowed.
c
      subroutine ems_tom_inv_ck_alw_l_pv(
     &     pv_ok, mv_c, poss_sing,
     &     rp_cn, ftran_n, n_mv_c,
     &     n_r, n_c, mx_n_c, n_a_el,
     &     mx_n_eta, mx_n_eta_el,
     &     l_pv_v, l_pv_r, l_pv_c,
     &     l_up_eta_n, l_up_el_n,
     &     se_1_en_n, se_1_fh,
     &     se_2_sa, se_2_fh,
     &     vr_in_r, bs_c_at, pv_r_n_or_mrt,
     &     eta_v, eta_ix, eta_sa,
     &     mtx_r_v, mtx_r_ix, mtx_c_sa,
     &     wk_a, ck_l_pv_wk_a,
     &     tl_rsdu_er)
      implicit none
      include 'EMSV.INC'
      include 'TOMINV.INC'
      logical pv_ok, mv_c, poss_sing
      integer rp_cn, ftran_n, n_mv_c
      integer n_r, n_c, mx_n_c, n_a_el
      integer mx_n_eta, mx_n_eta_el
      double precision l_pv_v
      integer l_pv_r, l_pv_c
      integer l_up_eta_n, l_up_el_n
      integer se_1_en_n, se_1_fh
      integer se_2_sa, se_2_fh
      integer vr_in_r(0:n_r)
      integer bs_c_at(1:n_r)
      integer pv_r_n_or_mrt(1:n_r)
      double precision eta_v(0:mx_n_eta_el), mtx_r_v(0:n_a_el)
      integer eta_ix(0:mx_n_eta_el), eta_sa(0:mx_n_eta+1)
      integer mtx_r_ix(0:n_a_el), mtx_c_sa(0:n_c+1)
      double precision wk_a(0:n_r)
      double precision ck_l_pv_wk_a(0:n_r)
      double precision tl_rsdu_er
      double precision rsdu_er
      integer en_n
 
      mv_c = .false.
      pv_ok = .false.
c
c     First consider the cases when the potential singularity occurs
c     before the last column in the bump.
c
      if (poss_sing) then
         if (se_1_en_n .lt. se_1_fh-n_mv_c) then
c
c     Can cycle round the bump columns to put this column last.
c
            mv_c = .true.
            n_mv_c = n_mv_c + 1
            do 10, en_n = se_1_en_n, se_1_fh-1
               bs_c_at(en_n) = bs_c_at(en_n+1)
 10         continue
            bs_c_at(se_1_fh) = l_pv_c
            go to 7000
         else if (se_1_en_n .lt. se_1_fh) then
c
c     This is not the last column in the bump and can't cycle round the
c     bump columns to put this column last---since this column has
c     already been moved. Basis has at least two singularities.
c
            goto 7000
         endif
      endif
c
c     Check that this is the last column in the bump and that the pivot
c     to be checked is not zero.
c
      if (se_1_en_n .ne. se_1_fh) goto 8000
      if (l_pv_v .eq. zero) goto 8010
      if (iand(tom_inv_sing_msk, tom_inv_sing_tl_ck) .ne. 0) then
         call ems_ck_tom_inv_l_pv(
     &        rp_cn, rsdu_er,
     &        l_pv_v, l_pv_r, l_pv_c,
     &        l_up_eta_n, l_up_el_n,
     &        mx_n_eta, mx_n_eta_el,
     &        se_2_sa, se_2_fh,
     &        vr_in_r, bs_c_at, pv_r_n_or_mrt,
     &        eta_v, eta_ix, eta_sa,
     &        mtx_r_v, mtx_r_ix, mtx_c_sa,
     &        wk_a, ck_l_pv_wk_a)
      else
c
c     Have to set rsdu_er to a value to handle the (strange) situation
c     where the pivot has not been tested and pivots are being allowed
c     subject to the tolerance on the test.
c
         rsdu_er = zero
      endif
      if (poss_sing) then
         if (iand(tom_inv_sing_msk, tom_inv_sing_tl_ck) .ne. 0) then
c
c     The pivot has been checked.
c
            if (rsdu_er .le. tl_rsdu_er) then
c
c     The pivot is OK.
c
               if (iand(tom_inv_sing_msk, tom_inv_sing_al_alw) .eq.
     &              tom_inv_sing_al_alw) then
c
c     All pivots are being allowed and this one is OK.
c
                  pv_ok = .true.
CM      IF (emsol_dev .EQ. 1) THEN
C?                  if (rp_cn .ge. 0) write(rp_cn, 9500)
C?     &                 ftran_n, l_pv_v, l_pv_c, rsdu_er
CM      ENDIF
               else if (iand(tom_inv_sing_msk, tom_inv_sing_tl_alw)
     &                 .ne. 0) then
c
c     All OK pivots are being allowed and this one is OK.
c
                  pv_ok = .true.
CM      IF (emsol_dev .EQ. 1) THEN
C?                  if (rp_cn .ge. 0) write(rp_cn, 9501)
C?     &                 ftran_n, l_pv_v, l_pv_c, rsdu_er
CM      ENDIF
               else
c
c     Small pivots are not being allowed but this one is OK.
c
CM      IF (emsol_dev .EQ. 1) THEN
C?                  if (rp_cn .ge. 0) write(rp_cn, 9502)
C?     &                 ftran_n, l_pv_v, l_pv_c, rsdu_er
CM      ENDIF
               endif
            else
c
c     The pivot is not OK.
c
               if (iand(tom_inv_sing_msk, tom_inv_sing_al_alw) .eq.
     &              tom_inv_sing_al_alw) then
c
c     All pivots are being allowed but this one is not OK
c
                  pv_ok = .true.
CM      IF (emsol_dev .EQ. 1) THEN
C?                  if (rp_cn .ge. 0) write(rp_cn, 9503)
C?     &                 ftran_n, l_pv_v, l_pv_c, rsdu_er
CM      ENDIF
               else if (iand(tom_inv_sing_msk, tom_inv_sing_tl_alw)
     &                 .ne. 0) then
c
c     All OK pivots are being allowed but this one is not OK
c
CM      IF (emsol_dev .EQ. 1) THEN
C?                  if (rp_cn .ge. 0) write(rp_cn, 9504)
C?     &                 ftran_n, l_pv_v, l_pv_c, rsdu_er
CM      ENDIF
               else
c
c     Small pivots are not being allowed and this one is not OK.
c
CM      IF (emsol_dev .EQ. 1) THEN
C?                  if (rp_cn .ge. 0) write(rp_cn, 9505)
C?     &                 ftran_n, l_pv_v, l_pv_c, rsdu_er
CM      ENDIF
               endif
            endif
         else
c
c     The pivot has not been checked
c
            if (iand(tom_inv_sing_msk, tom_inv_sing_al_alw) .eq.
     &           tom_inv_sing_al_alw) then
c
c     All pivots are being allowed.
c
               pv_ok = .true.
CM      IF (emsol_dev .EQ. 1) THEN
C?               if (rp_cn .ge. 0) write(rp_cn, 9510)
C?     &              ftran_n, l_pv_v, l_pv_c
CM      ENDIF
            else
c
c     Small pivots are not being allowed.
c
CM      IF (emsol_dev .EQ. 1) THEN
C?               if (rp_cn .ge. 0) write(rp_cn, 9512)
C?     &              ftran_n, l_pv_v, l_pv_c
CM      ENDIF
            endif
         endif
      else
c
c     Just testing pivot
c
         if (iand(tom_inv_sing_msk, tom_inv_sing_tl_ck) .ne. 0) then
            if (rsdu_er .le. tl_rsdu_er) then
c
c     All pivots are being allowed and this one is OK.
c
CM      IF (emsol_dev .EQ. 1) THEN
C?               if (rp_cn .ge. 0) write(rp_cn, 9500)
C?     &              ftran_n, l_pv_v, l_pv_c, rsdu_er
CM      ENDIF
            else
c
c     All pivots are being allowed but this one is not OK.
c
CM      IF (emsol_dev .EQ. 1) THEN
C?               if (rp_cn .ge. 0) write(rp_cn, 9503)
C?     &              ftran_n, l_pv_v, l_pv_c, rsdu_er
CM      ENDIF
            endif
         else
c
c     All pivots are being allowed
c
CM      IF (emsol_dev .EQ. 1) THEN
C?            if (rp_cn .ge. 0) write(rp_cn, 9510)
C?     &           ftran_n, l_pv_v, l_pv_c
CM      ENDIF
         endif
      endif
 7000 continue
      return
 8000 continue
CM      IF (emsol_dev .EQ. 1) THEN
C?      if (rp_cn .ge. 0) write(rp_cn, 9800)se_1_en_n, se_1_fh
CM      ENDIF
      goto 7000
 8010 continue
CM      IF (emsol_dev .EQ. 1) THEN
C?      if (rp_cn .ge. 0) write(rp_cn, 9801)
CM      ENDIF
      goto 7000
CM      IF (emsol_dev .EQ. 1) THEN
C? 9500 format('Tomlin INVERT (FTRAN ', i1,
C?     &     ') final pivot of ', g11.4, ': Variable ', i7,
C?     &     '     Allowed   and   ||A(A^inv.e_p)-e_p||_2 = ', g10.4)
C? 9501 format('Tomlin INVERT (FTRAN ', i1,
C?     &     ') final pivot of ', g11.4, ': Variable ', i7,
C?     &     '     Allowed since   ||A(A^inv.e_p)-e_p||_2 = ', g10.4)
C? 9502 format('Tomlin INVERT (FTRAN ', i1,
C?     &     ') final pivot of ', g11.4, ': Variable ', i7,
C?     &     ' Not allowed **BUT** ||A(A^inv.e_p)-e_p||_2 = ', g10.4)
C? 9503 format('Tomlin INVERT (FTRAN ', i1,
C?     &     ') final pivot of ', g11.4, ': Variable ', i7,
C?     &     '     Allowed **BUT** ||A(A^inv.e_p)-e_p||_2 = ', g10.4)
C? 9504 format('Tomlin INVERT (FTRAN ', i1,
C?     &     ') final pivot of ', g11.4, ': Variable ', i7,
C?     &     ' Not allowed since   ||A(A^inv.e_p)-e_p||_2 = ', g10.4)
C? 9505 format('Tomlin INVERT (FTRAN ', i1,
C?     &     ') final pivot of ', g11.4, ': Variable ', i7,
C?     &     ' Not allowed   and   ||A(A^inv.e_p)-e_p||_2 = ', g10.4)
C? 9510 format('Tomlin INVERT (FTRAN ', i1,
C?     &     ') final pivot of ', g11.4, ': Variable ', i7,
C?     &     '     Allowed')
C? 9512 format('Tomlin INVERT (FTRAN ', i1,
C?     &     ') final pivot of ', g11.4, ': Variable ', i7,
C?     &     ' Not allowed')
C? 9800 format('Not testing pivot: Set 1 entry = ', i7, ' < ', i7)
C? 9801 format('Not testing pivot: Value = zero')
CM      ENDIF
      end
C->>> -----------------------------------------> ems_ck_tom_inv_l_pv <<<
      subroutine ems_ck_tom_inv_l_pv(
     &     rp_cn, rsdu_er,
     &     l_pv_v, l_pv_r, l_pv_c,
     &     l_up_eta_n, l_up_el_n,
     &     mx_n_eta, mx_n_eta_el,
     &     se_2_sa, se_2_fh,
     &     vr_in_r, bs_c_at, pv_r_n_or_mrt,
     &     eta_v, eta_ix, eta_sa,
     &     mtx_r_v, mtx_r_ix, mtx_c_sa,
     &     wk_a, sol)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'TOMINV.INC'
      include 'ICTVR.INC'
      double precision rsdu_er, l_pv_v
      integer rp_cn
      integer l_pv_r, l_pv_c
      integer l_up_eta_n, l_up_el_n
      integer mx_n_eta, mx_n_eta_el, se_2_sa, se_2_fh
      integer vr_in_r(0:n_r)
      integer bs_c_at(1:n_r)
      integer pv_r_n_or_mrt(1:n_r)
      double precision eta_v(0:mx_n_eta_el), mtx_r_v(0:n_a_el)
      integer eta_ix(0:mx_n_eta_el), eta_sa(0:mx_n_eta+1)
      integer mtx_r_ix(0:n_a_el), mtx_c_sa(0:n_c+1)
      double precision wk_a(0:n_r)
      double precision sol(0:n_r)
      integer el_n, r_n, eta_n, pv_r_n, se_2_en_n, c_n, vr_n
      double precision wk_a_v, rsdu
      double precision sv_eta_v
      integer sv_eta_sa, sv_eta_ix, sv_vr_in_r
c
c     Complete the INVERT to save extra loops/questions
c
      l_up_eta_n = l_up_eta_n - 1
      l_up_el_n = l_up_el_n - 1
      call ems_sv_poss_undn_rl_v(sv_eta_v, eta_v(l_up_el_n))
      call ems_sv_poss_undn_i_v(sv_eta_ix, eta_ix(l_up_el_n))
      call ems_sv_poss_undn_i_v(sv_eta_sa, eta_sa(l_up_eta_n))
      call ems_sv_poss_undn_i_v(sv_vr_in_r, vr_in_r(l_pv_r))
      eta_sa(l_up_eta_n) = l_up_el_n
      eta_v(l_up_el_n) = -one/l_pv_v
      eta_ix(l_up_el_n) = l_pv_r
      vr_in_r(l_pv_r) = l_pv_c
c
c     Form A^inv.e_p
c
 
c     There are no L-etas to apply since p is the last pivotal row.
c
      wk_a(l_pv_r) = one
c
c     Apply the U-etas in the bump.
c
      el_n = eta_sa(l_up_eta_n)
      do 20, eta_n = l_up_eta_n, mx_n_eta
         pv_r_n = eta_ix(el_n)
         wk_a(pv_r_n) = wk_a(pv_r_n)*eta_v(el_n)
         do 10, el_n = el_n+1, eta_sa(eta_n+1)-1
            r_n = eta_ix(el_n)
            wk_a(r_n) = wk_a(r_n) + eta_v(el_n)*wk_a(pv_r_n)
 10      continue
 20   continue
c
c     Apply the U-etas after the bump
c
      do 50, se_2_en_n = se_2_sa, se_2_fh
         c_n = bs_c_at(se_2_en_n)
         pv_r_n = pv_r_n_or_mrt(se_2_en_n)
         do 30, el_n = mtx_c_sa(c_n), mtx_c_sa(c_n+1)-1
            r_n = mtx_r_ix(el_n)
            if (r_n .eq. pv_r_n) goto 35
 30      continue
c
c     Cannot find pivotal row in this column: error so revert to
c     standard Tomlin INVERT and return.
c
         tom_inv_sing_msk = 0
c
c     Probably don't have to set this.
c
         rsdu_er = zero
CM      IF (emsol_dev .EQ. 1) THEN
C?         if (rp_cn .ge. 0) write(rp_cn, 9400)pv_r_n, c_n
CM      ENDIF
         goto 7100
 35      continue
         wk_a(pv_r_n) = -wk_a(pv_r_n)/mtx_r_v(el_n)
         do 50, el_n = mtx_c_sa(c_n), mtx_c_sa(c_n+1)-1
            r_n = mtx_r_ix(el_n)
            if (r_n .eq. pv_r_n) goto 40
            wk_a(r_n) = wk_a(r_n) + wk_a(pv_r_n)*mtx_r_v(el_n)
 40      continue
 50   continue
c
c     Form A(A^inv.e_p)
c
      do 100, r_n = 1, n_r
         sol(r_n) = zero
 100  continue
      do 120, c_n = 1, n_r
         wk_a_v = wk_a(c_n)
         wk_a(c_n) = zero
         vr_n = vr_in_r(c_n)
         if (vr_n .le. n_c) then
            do 110, el_n = mtx_c_sa(vr_n), mtx_c_sa(vr_n+1)-1
               r_n = mtx_r_ix(el_n)
               sol(r_n) = sol(r_n) - mtx_r_v(el_n)*wk_a_v
 110        continue
         else
            r_n = vr_n - mx_n_c
            sol(r_n) = sol(r_n) + wk_a_v
         endif
 120  continue
      rsdu_er = zero
      do 200, r_n = 1, n_r
         if (r_n .eq. l_pv_r) then
            rsdu = sol(r_n)-one
         else
            rsdu = sol(r_n)
         endif
         rsdu_er = rsdu_er + rsdu*rsdu
c         write(*, 9000)r_n, sol(r_n), rsdu, rsdu_er
 200  continue
      rsdu_er = sqrt(rsdu_er)
 7100 continue
c
c     Recover the modified values
c
      call ems_rcov_poss_undn_rl_v(sv_eta_v, eta_v(l_up_el_n))
      call ems_rcov_poss_undn_i_v(sv_eta_ix, eta_ix(l_up_el_n))
      call ems_rcov_poss_undn_i_v(sv_eta_sa, eta_sa(l_up_eta_n))
      call ems_rcov_poss_undn_i_v(sv_vr_in_r, vr_in_r(l_pv_r))
      l_up_eta_n = l_up_eta_n + 1
      l_up_el_n = l_up_el_n + 1
      return
c 9000 format(i5, 3(2x, g11.4))
CM      IF (emsol_dev .EQ. 1) THEN
C? 9400 format('In ems_ck_tom_inv_l_pv: Cannot find pivotal row ', i7,
C?     &     ' in column ', i7)
CM      ENDIF
      end
 
      subroutine ems_se_iv_ty(ca_n, rp_cn, c_n, ds, is)
      implicit none
      integer ca_n, rp_cn, c_n
      integer is(*)
      double precision ds(*)
 
CM      IF (emsol_dev .EQ. 1) THEN
C?      if (rp_cn .ge. 0) write(rp_cn, 9000)ca_n, c_n
CM      ENDIF
      call ems_ml_bs_cg(0, c_n, ds, is)
      return
CM      IF (emsol_dev .EQ. 1) THEN
C? 9000 format('Call ', i1, ' to ems_se_iv_ty for c_n = ', i7)
CM      ENDIF
      end
 
      subroutine ems_se_dv_ty(rp_cn, c_n, ds, is)
      implicit none
      integer rp_cn, c_n
      integer is(*)
      double precision ds(*)
 
CM      IF (emsol_dev .EQ. 1) THEN
C?      if (rp_cn .ge. 0) write(rp_cn, 9000)c_n
CM      ENDIF
      call ems_ml_bs_cg(c_n, 0, ds, is)
      return
CM      IF (emsol_dev .EQ. 1) THEN
C? 9000 format('Call   to ems_se_dv_ty for c_n = ', i7)
CM      ENDIF
      end
