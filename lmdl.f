C->>> -------------------------------------------> ems_cg_ml_nm_n_ch <<<
c     Align any existing names onto the new boundaries following change
c     in ml_nm_n_ch

      subroutine ems_cg_ml_nm_n_ch(sv_ml_nm_n_ch, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      double precision ds(0:ds_n_en_m1)
      integer is(0:is_n_en_m1)
      integer sv_ml_nm_n_ch
      integer sv_ml_nm_n_rl
      double precision rl_vr_nm
      character*8 ch8_vr_nm
      equivalence (rl_vr_nm, ch8_vr_nm)
      double precision rl_vr_nm2
      character*8 ch8_vr_nm2
      equivalence (rl_vr_nm2, ch8_vr_nm2)

      sv_ml_nm_n_rl = ml_nm_n_rl
      ml_nm_n_rl = (ml_nm_n_ch+7)/8
      if (ml_nm_n_rl .gt. 1) then
c
c     Long names to be used
c
         if (sv_ml_nm_n_rl .gt. 1 .and.
     &        ml_nm_n_rl .ne. sv_ml_nm_n_rl) then
c
c     Change in length of long names: Remove the current data structure
c     for long names and allocate one with the new name length
c
            call ems_rm_blk_ml_lng_nm(is)
            if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
            call ems_iz_blk_ml_lng_nm(is)
            if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
         else if (sv_ml_nm_n_rl .le. 1) then
c
c     Previously using short names: Allocate the data structure for long
c     names.
c
            call ems_iz_blk_ml_lng_nm(is)
            if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
         endif
      else
c
c     Short names to be used
c
         if (sv_ml_nm_n_rl .gt. 1) then
c
c     Previously using long names:  Remove the data structure for long
c     names
c
            call ems_rm_blk_ml_lng_nm(is)
            if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
         endif
      endif
 7000 continue
      return
      end
C->>> ---------------------------------------------------> ems_iz_ml <<<
c     Sets the dimensions of a model and allocates space for its
c     storage.
c
      subroutine ems_iz_ml(poss_iz_rsmi_blk,
     &     is, usr_n_r, usr_n_c, usr_n_el)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMIHDL.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      logical poss_iz_rsmi_blk
      integer is(0:*), usr_n_r, usr_n_c, usr_n_el
 
      n_r = usr_n_r
      n_c = usr_n_c
      n_a_el = usr_n_el
      mx_n_r = max(n_r, max(mx_n_r, n_r-mx_n_r))
      if (mx_n_r .gt. mx_mx_ml_a_dim) go to 8010
      mx_n_c = max(n_c, max(mx_n_c, n_c-mx_n_c))
      if (mx_n_c .gt. mx_mx_ml_a_dim) go to 8020
      mx_n_a_el = max(n_a_el, max(mx_n_a_el, n_a_el-mx_n_a_el))
c
c     Initialise the record of the block move with which this model is
c     up-to-date. This value is only updated if ems_g_ml_p is called.
c
      ml_blk_mv_k = is(ix_blk_mv_k)
      ml_blk_st_msk = 0
c
c     Allocate storage for the model matrix.
c
      call ems_iz_blk_ml_c_mtx(is)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
c
c     Allocate storage for the model vectors.
c
      call ems_iz_blk_ml_vec(is)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      ml_nm_n_ch = 8
c
c     Possibly re-allocate space for the solver if the data structures
c     are not large enough for the current model
c
      if (poss_iz_rsmi_blk .and. (
     &     mx_n_r .gt. rsmi_blk_mx_n_r .or.
     &     mx_n_c .gt. rsmi_blk_mx_n_c)) then
         call ems_iz_al_rsmi_blk(is)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      endif
 7000 continue
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)
     &     mx_n_r, mx_mx_ml_a_dim
      call ems_msg_wr_li(serious_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?      call ems_dump
CM      ENDIF
      go to 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)
     &     mx_n_c, mx_mx_ml_a_dim
      call ems_msg_wr_li(serious_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?      call ems_dump
CM      ENDIF
      go to 7000
 9801 format('Maximum number of rows = ', i9,
     &     ' exceeds limit = ', i7)
 9802 format('Maximum number of columns = ', i9,
     &     ' exceeds limit = ', i7)
      end
 
C->>> ---------------------------------------------------> ems_ld_ml <<<
c     Checks the validity of a user-defined model and then loads it into
c     internal arrays.
c
      subroutine ems_ld_ml(usr_mtx_fmt,
     &     usr_n_r, usr_n_c, usr_n_el,
     &     usr_co, usr_r_lb, usr_r_ub, usr_c_lb, usr_c_ub,
     &     usr_mtx_r_i, usr_mtx_c_i, usr_mtx_v, ds, is)
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
      integer usr_mtx_fmt, usr_n_r, usr_n_c, usr_n_el
      integer usr_mtx_r_i(usr_n_el), usr_mtx_c_i(usr_n_el)
      integer is(0:is_n_en_m1)
      double precision usr_co(usr_n_c), usr_mtx_v(usr_n_el)
      double precision usr_r_lb(usr_n_r), usr_r_ub(usr_n_r)
      double precision usr_c_lb(usr_n_c), usr_c_ub(usr_n_c)
      double precision ds(0:ds_n_en_m1)
      integer tru_n_el
      integer rl_wk_a_ix
c      integer mem_mgr_rt_cod
c
c     Check that that the bounds are consistent.
c
      call ems_ck_ml_bd(usr_n_r, usr_n_c,
     &     usr_r_lb, usr_r_ub, usr_c_lb, usr_c_ub, tl_mx_iz_pr_act)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
c
c     Check that that the matrix definition is consistent.
c
      call ems_ck_ml_mtx(usr_mtx_fmt,
     &     usr_n_r, usr_n_c, usr_n_el, tru_n_el,
     &     usr_mtx_r_i, usr_mtx_c_i)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
c
c     Allocate space for the model: Use tru_n_el rather than usr_n_el
c     since the loading of the model is driven by the row/col start
c     array and tru_n_el is derived from this---unless usr_mtx_fmt =
c     ml_mtx_fmt_by_ix, in which case tru_n_el = usr_n_el.
c
      call ems_iz_ml(.true., is, usr_n_r, usr_n_c, tru_n_el)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
c
c     Load the model bounds and costs
c
      call ems_ld_ml_vec(usr_co,
     &     usr_r_lb, usr_r_ub, usr_c_lb, usr_c_ub,
     &     ds(p_lbc), ds(p_ubc), ds(p_cbp))
c
c     Load the model matrix:
c
      call ems_g_rsmi_rl_wk_a_ix(rl_wk_a_ix)
      if (rl_wk_a_ix .lt. 0) goto 8000
      call ems_ld_ml_c_mtx(usr_mtx_fmt,
     &     usr_mtx_v, usr_mtx_r_i, usr_mtx_c_i,
     &     ds(p_mtx_r_v), is(p_mtx_r_ix), is(p_mtx_c_sa),
     &     ds(p_rsmi_rl_wk_a(rl_wk_a_ix)))
      call ems_fr_rsmi_rl_wk_a_ix(rl_wk_a_ix)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
 7000 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9800 format('RSMI workspace not available in ems_ld_ml')
      end
 
C->>> -----------------------------------------------> ems_ck_ml_mtx <<<
c     Checks that that the matrix definition is consistent.
c
      subroutine ems_ck_ml_mtx(mtx_fmt,
     &     n_r, n_c, n_el, tru_n_el,
     &     mtx_r_i, mtx_c_i)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMSG.INC'
      integer mtx_fmt
      integer n_r, n_c, n_el, tru_n_el
      integer mtx_r_i(1:*), mtx_c_i(1:*)
      integer r_n, c_n, el_n
 
      if (mtx_fmt .eq. ml_mtx_fmt_by_c) then
         tru_n_el = mtx_c_i(n_c+1)-1
         if (n_el .gt. mtx_c_i(n_c+1)-1) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9400)
     &           n_el, tru_n_el
            call ems_msg_wr_li(warn_msg_n)
         else if (n_el .lt. tru_n_el) then
            goto 8000
         endif
         do 110, c_n = 1, n_c
            if (mtx_c_i(c_n) .gt. mtx_c_i(c_n+1)) goto 8010
 110     continue
         do 120, el_n = 1, tru_n_el
            r_n = mtx_r_i(el_n)
            if (r_n .lt. 1 .or. r_n .gt. n_r) goto 8020
 120     continue
      else if (mtx_fmt .eq. ml_mtx_fmt_by_ix) then
         tru_n_el = n_el
         do 220, el_n = 1, tru_n_el
            r_n = mtx_r_i(el_n)
            c_n = mtx_c_i(el_n)
            if (r_n .lt. 1 .or. r_n .gt. n_r) goto 8020
            if (c_n .lt. 1 .or. c_n .gt. n_c) goto 8030
 220     continue
      else
         tru_n_el = mtx_r_i(n_r+1)-1
         if (n_el .gt. tru_n_el) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9400)
     &           n_el, tru_n_el
            call ems_msg_wr_li(warn_msg_n)
         else if (n_el .lt. tru_n_el) then
            goto 8000
         endif
         do 310, r_n = 1, n_r
            if (mtx_r_i(r_n) .gt. mtx_r_i(r_n+1)) goto 8040
 310     continue
         do 320, el_n = 1, tru_n_el
            c_n = mtx_c_i(el_n)
            if (c_n .lt. 1 .or. c_n .gt. n_c) goto 8030
 320     continue
      endif
 7000 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)n_el, tru_n_el
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)
     &     c_n, mtx_c_i(c_n), c_n+1, mtx_c_i(c_n+1)
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)el_n, r_n, n_r
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8030 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9803)el_n, c_n, n_c
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8040 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9804)
     &     r_n, mtx_r_i(r_n), r_n+1, mtx_r_i(r_n+1)
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 9400 format('Expected ', i9,
     &     ' elements in the model matrix, but only ',
     &     i9, ' elements were found')
 9800 format('Expected ', i9,
     &     ' elements in the model matrix, but ',
     &     i9, ' elements were found')
 9801 format('Row indices for column ', i7,
     &     ' start at element ', i9,
     &     ' after row indices for column ', i7,
     &     ' which start at element ', i9)
 9802 format('Element ', i9, ' has row index ', i9,
     &     ' which is out of range: valid range is 1 to ', i7)
 9803 format('Element ', i9, ' has column index ', i9,
     &     ' which is out of range: valid range is 1 to ', i7)
 9804 format('Column indices for row ', i7,
     &     ' start at element ', i9,
     &     ' after column indices for row ', i7,
     &     ' which start at element ', i9)
      end
 
C->>> -----------------------------------------------> ems_ld_ml_vec <<<
c     Puts the supplied the objective vector and bounds into EMSOL
c
      subroutine ems_ld_ml_vec(usr_co,
     &     usr_r_lb, usr_r_ub, usr_c_lb, usr_c_ub,
     &     lb, ub, co)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      double precision usr_co(1:n_c), usr_r_lb(1:n_r), usr_r_ub(1:n_r)
      double precision usr_c_lb(1:n_c), usr_c_ub(1:n_c)
      double precision lb(0:mx_n_c+mx_n_r), ub(0:mx_n_c+mx_n_r)
      double precision co(0:mx_n_c+mx_n_r)
      integer r_n, c_n
 
      do 10, c_n = 1, n_c
         co(c_n) = usr_co(c_n)
         lb(c_n) = usr_c_lb(c_n)
         ub(c_n) = usr_c_ub(c_n)
 10   continue
      do 15, c_n = n_c + 1, mx_n_c
         co(c_n) = zero
         lb(c_n) = zero
         ub(c_n) = zero
 15   continue
         do 20, r_n = 1, n_r
         co(mx_n_c+r_n) = zero
         lb(mx_n_c+r_n) = usr_r_lb(r_n)
         ub(mx_n_c+r_n) = usr_r_ub(r_n)
 20   continue
      do 25, r_n = n_r + 1, mx_n_r
         co(mx_n_c+r_n) = zero
         lb(mx_n_c+r_n) = -inf
         ub(mx_n_c+r_n) = inf
 25   continue
      return
      end
 
C->>> ---------------------------------------------> ems_ld_ml_c_mtx <<<
c     Load a user-supplied matrix in column-wise storage.
c
      subroutine ems_ld_ml_c_mtx(usr_mtx_fmt,
     &     usr_mtx_v, usr_mtx_r_i, usr_mtx_c_i,
     &     mtx_r_v , mtx_r_ix, mtx_c_sa,
     &     te_rl_a)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      integer usr_mtx_fmt, usr_mtx_r_i(1:*), usr_mtx_c_i(1:*)
      integer mtx_r_ix(0:n_a_el), mtx_c_sa(0:n_c+1)
      double precision usr_mtx_v(1:n_a_el), mtx_r_v(0:n_a_el)
      double precision te_rl_a(0:n_r)
      integer r_n, c_n, el_n
      double precision v
      integer n_rpt_en, n_ze_mtx_v, tru_n_a_el
      integer c_sa
 
      n_rpt_en = 0
      n_ze_mtx_v = 0
c
c     Zero the array which is used to accumulate the column entries.
c
      call ems_cp_rl_a(1+n_r, zero, te_rl_a, 0)
      if (usr_mtx_fmt .eq. ml_mtx_fmt_by_c) then
c
c     The matrix is supplied in column form
c
         tru_n_a_el = 0
         do 130, c_n = 1, n_c
            c_sa = tru_n_a_el + 1
            do 110, el_n = usr_mtx_c_i(c_n), usr_mtx_c_i(c_n+1)-1
               r_n = usr_mtx_r_i(el_n)
               if (te_rl_a(r_n) .ne. zero) n_rpt_en = n_rpt_en + 1
               te_rl_a(r_n) = te_rl_a(r_n) + usr_mtx_v(el_n)
 110        continue
            do 120, el_n = usr_mtx_c_i(c_n), usr_mtx_c_i(c_n+1)-1
               r_n = usr_mtx_r_i(el_n)
               v = te_rl_a(r_n)
               if (abs(v) .ge. mps_ze) then
                  tru_n_a_el = tru_n_a_el + 1
                  mtx_r_v(tru_n_a_el) = v
                  mtx_r_ix(tru_n_a_el) = r_n
               else
                  n_ze_mtx_v = n_ze_mtx_v + 1
               endif
               te_rl_a(r_n) = zero
 120        continue
            mtx_c_sa(c_n) = c_sa
 130     continue
         mtx_c_sa(n_c+1) = tru_n_a_el + 1
         n_a_el = tru_n_a_el
      else
c
c     The matrix is supplied in index or row form so determine the
c     column form.
c
         call ems_cp_i_a(n_c+1, 0, mtx_c_sa(1), 0)
         do 210, el_n = 1, n_a_el
            c_n = usr_mtx_c_i(el_n)
            mtx_c_sa(c_n) = mtx_c_sa(c_n) + 1
 210     continue
c
c     Transform the lengths into starts.
c
         mtx_c_sa(n_c+1) = n_a_el + 1
         do 220, c_n = n_c, 1, -1
            mtx_c_sa(c_n) = mtx_c_sa(c_n+1) - mtx_c_sa(c_n)
 220     continue
         if (usr_mtx_fmt .eq. ml_mtx_fmt_by_ix) then
            do 230, el_n = 1, n_a_el
               c_n = usr_mtx_c_i(el_n)
               mtx_r_ix(mtx_c_sa(c_n)) = usr_mtx_r_i(el_n)
               mtx_r_v(mtx_c_sa(c_n)) = usr_mtx_v(el_n)
               mtx_c_sa(c_n) = mtx_c_sa(c_n) + 1
 230        continue
         else
            do 250, r_n = 1, n_r
               do 240, el_n = usr_mtx_r_i(r_n), usr_mtx_r_i(r_n+1)-1
                  c_n = usr_mtx_c_i(el_n)
                  mtx_r_ix(mtx_c_sa(c_n)) = r_n
                  mtx_r_v(mtx_c_sa(c_n)) = usr_mtx_v(el_n)
                  mtx_c_sa(c_n) = mtx_c_sa(c_n) + 1
 240           continue
 250        continue
         endif
         do 260, c_n = n_c, 2, -1
            mtx_c_sa(c_n) = mtx_c_sa(c_n-1)
 260     continue
         mtx_c_sa(1) = 1
c
c     Now analyse the column form in the same way as above but taking
c     the data from the internal arrays---this essentially compresses
c     them to remove any repeated or zero entries.
c
         tru_n_a_el = 0
         do 330, c_n = 1, n_c
            c_sa = tru_n_a_el + 1
            do 310, el_n = mtx_c_sa(c_n), mtx_c_sa(c_n+1)-1
               r_n = mtx_r_ix(el_n)
               if (te_rl_a(r_n) .ne. zero) n_rpt_en = n_rpt_en + 1
               te_rl_a(r_n) = te_rl_a(r_n) + mtx_r_v(el_n)
 310        continue
            do 320, el_n = mtx_c_sa(c_n), mtx_c_sa(c_n+1)-1
               r_n = mtx_r_ix(el_n)
               v = te_rl_a(r_n)
               if (abs(v) .ge. mps_ze) then
                  tru_n_a_el = tru_n_a_el + 1
                  mtx_r_v(tru_n_a_el) = v
                  mtx_r_ix(tru_n_a_el) = r_n
               else
                  n_ze_mtx_v = n_ze_mtx_v + 1
               endif
               te_rl_a(r_n) = zero
 320        continue
            mtx_c_sa(c_n) = c_sa
 330     continue
         mtx_c_sa(n_c+1) = tru_n_a_el + 1
         n_a_el = tru_n_a_el
      end if
      if (n_rpt_en .gt. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)n_rpt_en
         call ems_msg_wr_li(info_msg_n)
      endif
      if (n_ze_mtx_v .gt. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)
     &        n_ze_mtx_v, mps_ze
         call ems_msg_wr_li(info_msg_n)
      endif
      mtx_c_sa(0) = n_c
      return
 9000 format('Model matrix has at least ', i9, ' repeated entries')
 9010 format('Model matrix has at least ', i9, ' entries less than ',
     &     g11.4, ' in magnitude (as original values or',
     &     ' due to cancellation in repeated values):',
     &     ' these entries have been removed')
      end
 
C->>> ------------------------------------------------> ems_se_vr_nm <<<
      subroutine ems_se_vr_nm(
     &     usr_n_r_nm, usr_r_nm, usr_sa_r,
     &     usr_n_c_nm, usr_c_nm, usr_sa_c, ems_nm, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSPM.INC'
      include 'EMSFI.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer usr_n_r_nm, usr_sa_r, usr_n_c_nm, usr_sa_c, ems_nm
      double precision ds(0:ds_n_en_m1)
      integer is(0:is_n_en_m1)
      character*1 usr_r_nm(ml_nm_n_ch, usr_n_r_nm)
      character*1 usr_c_nm(ml_nm_n_ch, usr_n_c_nm)
      double precision ems_ch8_t_rl
      character*(ml_nm_mx_n_ch) mu_ch
      integer p_r_nm, p_c_nm
      integer r_n, sa_r, end_r, c_n, sa_c, end_c
c
c     Assign a name to variable zero
c
      if (ml_nm_n_ch .eq. 8) then
         ds(p_nm) = ems_ch8_t_rl('********')
      else
         call ems_iz_ch(mu_ch, '*')
         call ems_ch_t_rl(ml_nm_n_ch, mu_ch, ds(p_lng_nm))
      endif
 
      if (usr_n_r_nm .le. 0) then
         if (usr_n_r_nm .lt. 0) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9110)usr_n_r_nm
            call ems_msg_wr_li(warn_msg_n)
         end if
         sa_r =  mx_n_r+1
         end_r = mx_n_r
      else
         sa_r = usr_sa_r
         if (usr_sa_r .lt. 1) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9112)usr_sa_r
            call ems_msg_wr_li(warn_msg_n)
            sa_r = 1
         end if
         end_r = sa_r + usr_n_r_nm - 1
         if (end_r .gt. mx_n_r) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9114)
     &           mx_n_r-end_r
            call ems_msg_wr_li(warn_msg_n)
            end_r = mx_n_r
         end if
      end if
      if (usr_n_c_nm .le. 0) then
         if (usr_n_c_nm .lt. 0) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9120)usr_n_c_nm
            call ems_msg_wr_li(warn_msg_n)
         end if
         sa_c =  mx_n_c+1
         end_c = mx_n_c
      else
         sa_c = usr_sa_c
         if (usr_sa_c .lt. 1) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9122)usr_sa_c
            call ems_msg_wr_li(warn_msg_n)
            sa_c = 1
         end if
         end_c = sa_c + usr_n_c_nm - 1
         if (end_c .gt. mx_n_c) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9124)
     &           mx_n_c-end_c
            call ems_msg_wr_li(warn_msg_n)
            end_c = mx_n_c
         end if
      end if
 
      if (ml_nm_n_ch .eq. 8) then
         p_c_nm = p_nm + 1
      else
         p_c_nm = p_lng_nm + ml_nm_n_rl
         call ems_iz_ch(mu_ch, ' ')
      endif
      p_r_nm = p_c_nm + ml_nm_n_rl*mx_n_c
 
      p_c_nm = p_c_nm + (sa_c-1)*ml_nm_n_rl
      p_r_nm = p_r_nm + (sa_r-1)*ml_nm_n_rl
 
      if (ems_nm .eq. 1) then
         if (sa_c .le. end_c) call ems_iz_df_nm('C',
     &           ml_nm_n_ch, ml_nm_n_rl, sa_c, end_c, ds(p_c_nm))
         if (sa_r .le. end_r) call ems_iz_df_nm('R',
     &           ml_nm_n_ch, ml_nm_n_rl, sa_r, end_r, ds(p_r_nm))
      else
         do 110, c_n = sa_c, end_c
            if (ml_nm_n_ch .eq. 8) then
               ds(p_c_nm) = ems_ch8_t_rl(usr_c_nm(1, c_n-sa_c+1))
            else
               call ems_ch_t_rl(ml_nm_n_ch,
     &              usr_c_nm(1, c_n-sa_c+1), ds(p_c_nm))
            endif
            p_c_nm = p_c_nm + ml_nm_n_rl
 110     continue
         do 120, r_n = sa_r, end_r
            if (ml_nm_n_ch .eq. 8) then
               ds(p_r_nm) = ems_ch8_t_rl(usr_r_nm(1, r_n-sa_r+1))
            else
               call ems_ch_t_rl(ml_nm_n_ch,
     &              usr_r_nm(1, r_n-sa_r+1), ds(p_r_nm))
            endif
            p_r_nm = p_r_nm + ml_nm_n_rl
 120     continue
      end if
c
c     Indicate that the model now has names
c
      ml_da_st_msk = ior(ml_da_st_msk, ml_da_st_nm)
      return
 9110 format('Number of row names = ', i9,
     &     ' so no user-defined row names will be used')
 9112 format('Start row number = ', i9,
     &     ' so user-defined row names will start with row 1')
 9114 format('User-defined row names ex',
     &     'ceed maximum number of rows so the last ', i9,
     &     ' will be ignored')
 9120 format('Number of column names = ', i9,
     &     ' so no user-defined column names will be used')
 9122 format('Start column number = ', i9,
     &     ' so user-defined column names will start with column 1')
 9124 format('User-defined column names ex',
     &     'ceed maximum number of columns so the last ', i9,
     &     ' will be ignored')
      end
 
C->>> ----------------------------------------------> ems_g_ml_r_mtx <<<
c     Form a copy of the matrix stored by rows to be used for pricing.
c
      subroutine ems_g_ml_r_mtx(st, vr_in_c,
     &     mtx_r_v, mtx_r_ix, mtx_c_sa,
     &     mtx_c_v, mtx_c_ix, mtx_r_sa, rsmi_i_wk_a1)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICS.INC'
      include 'ICTVR.INC'
      integer st(0:mx_n_c+n_r)
      integer vr_in_c(-vr_in_c_n_sn:n_c)
      double precision mtx_r_v(0:n_a_el)
      integer mtx_r_ix(0:n_a_el)
      integer mtx_c_sa(0:n_c+1)
      double precision mtx_c_v(0:n_a_el)
      integer mtx_c_ix(0:n_a_el)
      integer mtx_r_sa(0:n_r+1)
      integer rsmi_i_wk_a1(0:n_r)
      integer r_n, vr_n, el_n, r_el_n, c_n, l_c_n
 
      call ems_cp_i_a(n_r, 0, rsmi_i_wk_a1(1), 0)
c
c     Use rsmi_i_wk_a1 to compute the lengths.
c
      do 10, vr_n = 1, n_c
c
c     Miss out the columns which are nonbasic but have zero index---are
c     not in vr_in_c.
c
         if (iand(st(vr_n), bc_bt) .eq. 0 .and.
     &        iand(st(vr_n), mx_mx_ml_a_dim) .eq. 0) goto 10
         do 20, el_n = mtx_c_sa(vr_n), mtx_c_sa(vr_n+1)-1
            rsmi_i_wk_a1(mtx_r_ix(el_n)) =
     &           rsmi_i_wk_a1(mtx_r_ix(el_n)) + 1
 20      continue
 10   continue
c
c     Compute the starts from the lengths.
c
      mtx_r_sa(1) = 1
      do 30, r_n = 1, n_r
         mtx_r_sa(r_n+1) = mtx_r_sa(r_n) + rsmi_i_wk_a1(r_n)
 30   continue
c
c     Make a row copy of all the rows of the matrix, with the entries of
c     each row ordered so that those columns which are in the list of
c     pricing variables come first, setting the last such index to be
c     negative to show where the divide comes.
c
c     Assign values and indices for the columns for pricing.
c
      l_c_n = vr_in_c(os_struc_in_c_l_pc_p)
      do 120, c_n = vr_in_c(os_struc_in_c_f_p_m1) + 1, l_c_n
         vr_n = vr_in_c(c_n)
         do 110, el_n = mtx_c_sa(vr_n), mtx_c_sa(vr_n+1)-1
            r_el_n = mtx_r_sa(mtx_r_ix(el_n))
            mtx_c_v(r_el_n) = mtx_r_v(el_n)
            mtx_c_ix(r_el_n) = vr_n
            mtx_r_sa(mtx_r_ix(el_n)) = r_el_n + 1
 110     continue
 120  continue
c
c     Take a copy of the current row starts in order to find the end of
c     the list of row indices in the pricing list.
c
      call ems_cp_i_a1(n_r, mtx_r_sa(1), rsmi_i_wk_a1(1))
c
c     Now go through all the columns adding to the section corresponding
c     to columns not used for pricing. Note that nonbasic variables not
c     in vr_in_c should have zero index in their status.
c
      do 150, vr_n = 1, n_c
c
c     Miss out the columns which are nonbasic but have zero index---are
c     not in vr_in_c.
c
         if (iand(st(vr_n), bc_bt) .eq. 0 .and.
     &        iand(st(vr_n), mx_mx_ml_a_dim) .le. l_c_n) goto 150
         do 160, el_n = mtx_c_sa(vr_n), mtx_c_sa(vr_n+1)-1
            r_el_n = mtx_r_sa(mtx_r_ix(el_n))
            mtx_c_v(r_el_n) = mtx_r_v(el_n)
            mtx_c_ix(r_el_n) = vr_n
            mtx_r_sa(mtx_r_ix(el_n)) = r_el_n + 1
 160     continue
 150  continue
c
c     All the row starts have been incremented upto the start of the
c     next row (as part of the process of defining the entries) so
c     reset them all.
c
      mtx_r_sa(n_r+1) = mtx_r_sa(n_r)
      mtx_r_sa(0) = 1
      do 210, r_n = n_r, 1, -1
         mtx_r_sa(r_n) = mtx_r_sa(r_n-1)
         if (mtx_r_sa(r_n) .eq. rsmi_i_wk_a1(r_n)) then
c
c     There are no indices of columns in the pricing list for this row.
c     Negate the start to indicate this.
c
            mtx_r_sa(r_n) = -mtx_r_sa(r_n)
         else
c
c     Negate the index of the last column in the pricing list for this
c     row.
c
            r_el_n = rsmi_i_wk_a1(r_n) - 1
            mtx_c_ix(r_el_n) = -mtx_c_ix(r_el_n)
         endif
 210  continue
c
c     Set the start of the 0th row to zero for the assembler row price.
c
      mtx_r_sa(0) = 0
      return
      end
 
C->>> ----------------------------------------------> ems_u_ml_r_mtx <<<
c     Update the copy of the matrix stored by rows to be used for
c     pricing, subject to vr_t_en_bs entering the basis and vr_t_lv_bs
c     leaving the basis. If vr_t_lv_bs=0 this indicates that the column
c     should not be included in the set of columns for pricing.
c
      subroutine ems_u_ml_r_mtx(vr_t_en_bs, vr_t_lv_bs,
     &     mtx_r_ix, mtx_c_sa,
     &     mtx_c_v, mtx_c_ix, mtx_r_sa)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICS.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      integer vr_t_en_bs, vr_t_lv_bs
      integer mtx_r_sa(0:n_r+1), mtx_c_ix(0:n_a_el+n_r)
      integer mtx_c_sa(0:n_c+1), mtx_r_ix(0:n_a_el)
      double precision mtx_c_v(0:n_a_el+n_r)
      integer r_n, el_n, r_el_n, f_el_n, l_el_n, c_el_n
      double precision rl_v
 
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_u_lvl2) call ems_tt_rec(u_r_rep_tt, n_bs)
CM      ENDIF
      if (vr_t_en_bs .le. 0) go to 100
      if (vr_t_en_bs .le. n_c) then
c
c     A structural variable has entered the basis so loop over the
c     entries in the column in order to know which rows need this entry
c     to be swapped with the entry on the border and the border shifted
c     left by one.
c
         do 20, el_n = mtx_c_sa(vr_t_en_bs),
     &        mtx_c_sa(vr_t_en_bs+1) - 1
            r_n = mtx_r_ix(el_n)
            c_el_n = 0
            f_el_n = mtx_r_sa(r_n)
            if (f_el_n .lt. 0) goto 8000
            r_el_n = f_el_n
 10         continue
            if (mtx_c_ix(r_el_n) .lt. 0) then
c
c     Record the element number of the last column in the pricing list
c     for this row and negate the column index since the marker index
c     will be reset.
c
               l_el_n = r_el_n
               mtx_c_ix(r_el_n) = -mtx_c_ix(r_el_n)
c
c     Record the element number of the column in the pricing list which
c     is entering the basis.
c
               if (mtx_c_ix(r_el_n) .eq. vr_t_en_bs) c_el_n = r_el_n
            else
c
c     Record the element number of the column in the pricing list which
c     is entering the basis. Increase the row element number and
c     consider this column.
c
               if (mtx_c_ix(r_el_n) .eq. vr_t_en_bs) c_el_n = r_el_n
               r_el_n = r_el_n + 1
               if (r_el_n .ge. abs(mtx_r_sa(r_n+1))) goto 8010
               goto 10
            endif
            if (c_el_n .eq. 0) goto 8010
            if (f_el_n .eq. l_el_n) then
c
c     There was only one column index in the pricing list for this row
c     and now there is none so negate the row start to indicate this.
c
               mtx_r_sa(r_n) = -f_el_n
            else if (c_el_n .eq. l_el_n) then
c
c     The column in the pricing list which is entering the basis was the
c     last so just negate the index of the previous column index to
c     indicate that this is now the last.
c
               mtx_c_ix(c_el_n-1) = -mtx_c_ix(c_el_n-1)
            else
c
c     Swap the value and index of the column in the pricing list which
c     is entering the basis with those of the (current) last index and
c     negate the previous column index to indicate that this is now the
c     last.
c
               if (c_el_n .ne. l_el_n) then
                  rl_v = mtx_c_v(c_el_n)
                  mtx_c_v(c_el_n) = mtx_c_v(l_el_n)
                  mtx_c_v(l_el_n) = rl_v
                  mtx_c_ix(c_el_n) = mtx_c_ix(l_el_n)
                  mtx_c_ix(l_el_n) = vr_t_en_bs
               endif
               mtx_c_ix(l_el_n-1) = -mtx_c_ix(l_el_n-1)
            endif
 20      continue
      end if
 100  continue
      if (vr_t_lv_bs .le. 0) go to 7000
c
c     Loop over the entries in the column in order to know which rows
c     need this entry to be swapped with the entry on the border
c     (shifted right by one).
c
      if (vr_t_lv_bs .le. n_c) then
         do 140, el_n = mtx_c_sa(vr_t_lv_bs),
     &        mtx_c_sa(vr_t_lv_bs+1) - 1
            r_n = mtx_r_ix(el_n)
            c_el_n = 0
c
c     First find the position where the index of the column leaving the
c     basis will move to. This position will be the new `last pricing
c     element number'.
c
            f_el_n = mtx_r_sa(r_n)
            if (f_el_n .lt. 0) then
c
c     There are currently no columns in the pricing list for this row so
c     the new `last pricing element number' will be the first. Set the
c     row start to be positive again because there will now be a column
c     index in the pricing list for this row.
c
               l_el_n = -f_el_n
               r_el_n = l_el_n
               mtx_r_sa(r_n) = r_el_n
               goto 120
            endif
            r_el_n = f_el_n
 110        continue
            if (mtx_c_ix(r_el_n) .lt. 0) then
               l_el_n = r_el_n + 1
               mtx_c_ix(r_el_n) = -mtx_c_ix(r_el_n)
            else
               r_el_n = r_el_n + 1
               if (r_el_n .ge. abs(mtx_r_sa(r_n+1))) goto 8020
               goto 110
            endif
            r_el_n = r_el_n + 1
c
c     Now find the element number of the column leaving the basis.
c
 120        continue
            if (mtx_c_ix(r_el_n) .eq. vr_t_lv_bs) then
               c_el_n = r_el_n
               goto 130
            endif
            r_el_n = r_el_n + 1
            if (r_el_n .ge. abs(mtx_r_sa(r_n+1))) goto 8030
            goto 120
c
c     Now swap the index of the column leaving the basis with the index
c     in the position of the new `last pricing element number'.
c
 130        continue
            if (l_el_n .ne. c_el_n) then
               rl_v = mtx_c_v(c_el_n)
               mtx_c_v(c_el_n) = mtx_c_v(l_el_n)
               mtx_c_v(l_el_n) = rl_v
               mtx_c_ix(c_el_n) = mtx_c_ix(l_el_n)
            endif
            mtx_c_ix(l_el_n) = -vr_t_lv_bs
 140     continue
      end if
 7000 continue
c      write(*, 9000)vr_t_en_bs, vr_t_lv_bs
c      do 210, r_n = 1, n_r
c         if (mtx_r_sa(r_n) .lt. 0) then
c            write(*,9010)r_n, mtx_r_sa(r_n)
c         else
c            write(*,9010)r_n, mtx_r_sa(r_n), (mtx_c_ix(el_n),
c     &           el_n = mtx_r_sa(r_n), abs(mtx_r_sa(r_n+1)-1))
c 9000 format(/'Vr_t_en ', i7, ' Vr_t_lv ' ,i7)
c 9010 format(i3, ':', i3, ':', 40(1x, i3))
c         endif
c 210  continue
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_u_lvl2) call ems_tt_rec(-u_r_rep_tt, n_bs)
CM      ENDIF
      return
 8000 continue
c
c     The row appears to have no column indices in the pricing list.
c     This indicates a bug since the column from which this row was
c     determined was in the pricing list.
c
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)r_n
      call ems_msg_wr_li(bug_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?      call ems_dump
CM      ENDIF
      goto 7000
 8010 continue
c
c     The start of the next row has been reached without finding the
c     column which has entered the basis.
c
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)vr_t_en_bs, r_n
      call ems_msg_wr_li(bug_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?      call ems_dump
CM      ENDIF
      goto 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)r_n
      call ems_msg_wr_li(bug_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?      call ems_dump
CM      ENDIF
      goto 7000
 8030 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9803)vr_t_en_bs, r_n
      call ems_msg_wr_li(bug_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?      call ems_dump
CM      ENDIF
      goto 7000
 9800 format('Row ', i7, ' has no columns in the pricing list')
 9801 format('vr_t_en_bs = ', i7, ' is not in row ', i7)
 9802 format('Row ', i7, ' has all columns in the pricing list')
 9803 format('vr_t_lv_bs = ', i7, ' is not in row ', i7)
      end
 
C->>> ---------------------------------------> ems_ca_g_ml_usr_c_mtx <<<
c     Calls the routine to form a copy of the matrix stored by cols for
c     the user.
c
      subroutine ems_ca_g_ml_usr_c_mtx(ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      double precision ds(0:ds_n_en_m1)
      integer is(0:is_n_en_m1)
      integer lc_p_scl
      if (iand(ml_da_st_msk, ml_da_st_scl_ml_mtx) .eq. 0) then
         lc_p_scl = 0
      else
         lc_p_scl = p_scl
      endif
      call ems_g_ml_usr_c_mtx(
     &     ds(p_mtx_r_v), is(p_mtx_r_ix), is(p_mtx_c_sa), ds(lc_p_scl),
     &     ds(p_usr_mtx_r_v), is(p_usr_mtx_r_ix), is(p_usr_mtx_c_sa))
      return
      end
 
C->>> ------------------------------------------> ems_g_ml_usr_c_mtx <<<
c     Form a copy of the matrix stored by cols for the user.
c
      subroutine ems_g_ml_usr_c_mtx(
     &     mtx_r_v, mtx_r_ix, mtx_c_sa, scl,
     &     usr_mtx_r_v, usr_mtx_r_ix, usr_mtx_c_sa)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      integer usr_mtx_c_sa(0:n_c+1), usr_mtx_r_ix(0:n_a_el)
      integer mtx_c_sa(0:n_c+1), mtx_r_ix(0:n_a_el)
      double precision usr_mtx_r_v(0:n_a_el)
      double precision mtx_r_v(0:n_a_el)
      double precision scl(0:mx_n_c+n_r)
      integer c_n, r_n, el_n
      double precision c_scl_v
 
      if (iand(ml_da_st_msk, ml_da_st_scl_ml_mtx) .eq. 0) then
         call ems_cp_i_a1(n_c+1, mtx_c_sa(1), usr_mtx_c_sa(1))
         call ems_cp_i_a1(n_a_el, mtx_r_ix(1), usr_mtx_r_ix(1))
         call ems_cp_rl_a1(n_a_el, mtx_r_v(1), usr_mtx_r_v(1))
      else
         do 10, c_n = 1, n_c
            usr_mtx_c_sa(c_n) = mtx_c_sa(c_n)
            c_scl_v = scl(c_n)
            do 20, el_n = mtx_c_sa(c_n), mtx_c_sa(c_n+1)-1
               r_n = mtx_r_ix(el_n)
               usr_mtx_r_v(el_n) = mtx_r_v(el_n)*c_scl_v/scl(mx_n_c+r_n)
               usr_mtx_r_ix(el_n) = r_n
 20         continue
 10      continue
         usr_mtx_c_sa(n_c+1) = mtx_c_sa(n_c+1)
      endif
      return
      end
 
C->>> ------------------------------------------> ems_g_ml_usr_r_mtx <<<
c     Calls the routine to form a copy of the matrix stored by rows for
c     the user.
c
      subroutine ems_ca_g_ml_usr_r_mtx(ds, is)
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
      integer lc_p_scl
      integer i_wk_a_ix
      if (iand(ml_da_st_msk, ml_da_st_scl_ml_mtx) .eq. 0) then
         lc_p_scl = 0
      else
         lc_p_scl = p_scl
      endif
      call ems_g_rsmi_i_wk_a_ix(i_wk_a_ix)
      if (i_wk_a_ix .lt. 0) goto 8000
      call ems_g_ml_usr_r_mtx(
     &     ds(p_mtx_r_v), is(p_mtx_r_ix), is(p_mtx_c_sa), ds(lc_p_scl),
     &     ds(p_usr_mtx_c_v), is(p_usr_mtx_c_ix), is(p_usr_mtx_r_sa),
     &     is(p_rsmi_i_wk_a(i_wk_a_ix)))
      call ems_fr_rsmi_i_wk_a_ix(i_wk_a_ix)
 7000 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9800 format('RSMI workspace not available in ems_ca_g_ml_usr_r_mtx')
      end
 
C->>> ------------------------------------------> ems_g_ml_usr_r_mtx <<<
c     Form a copy of the matrix stored by rows for the user.
c
      subroutine ems_g_ml_usr_r_mtx(
     &     mtx_r_v, mtx_r_ix, mtx_c_sa, scl,
     &     usr_mtx_c_v, usr_mtx_c_ix, usr_mtx_r_sa,
     &     rsmi_i_wk_a1)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      integer usr_mtx_r_sa(0:n_r+1), usr_mtx_c_ix(0:n_a_el)
      integer mtx_c_sa(0:n_c+1), mtx_r_ix(0:n_a_el)
      integer rsmi_i_wk_a1(0:n_r)
      double precision usr_mtx_c_v(0:n_a_el)
      double precision mtx_r_v(0:n_a_el)
      double precision scl(0:mx_n_c+n_r)
      double precision c_scl_v
      integer r_n, c_n, el_n, r_el_n
 
      call ems_cp_i_a(n_r, 0, rsmi_i_wk_a1(1), 0)
c
c     Use rsmi_i_wk_a1 to compute the lengths.
c
      do 10, c_n = 1, n_c
         do 20, el_n = mtx_c_sa(c_n), mtx_c_sa(c_n+1)-1
            rsmi_i_wk_a1(mtx_r_ix(el_n)) =
     &           rsmi_i_wk_a1(mtx_r_ix(el_n)) + 1
 20      continue
 10   continue
c
c     Compute the starts from the lengths.
c
      usr_mtx_r_sa(1) = 1
      do 30, r_n = 1, n_r
         usr_mtx_r_sa(r_n+1) =
     &        usr_mtx_r_sa(r_n) + rsmi_i_wk_a1(r_n)
 30   continue
 
      if (iand(ml_da_st_msk, ml_da_st_scl_ml_mtx) .eq. 0) then
         do 120, c_n = 1, n_c
            do 110, el_n = mtx_c_sa(c_n), mtx_c_sa(c_n+1)-1
               r_n = mtx_r_ix(el_n)
               r_el_n = usr_mtx_r_sa(r_n)
               usr_mtx_c_v(r_el_n) = mtx_r_v(el_n)
               usr_mtx_c_ix(r_el_n) = c_n
               usr_mtx_r_sa(r_n) = r_el_n + 1
 110        continue
 120     continue
      else
         do 140, c_n = 1, n_c
            c_scl_v = scl(c_n)
            do 130, el_n = mtx_c_sa(c_n), mtx_c_sa(c_n+1)-1
               r_n = mtx_r_ix(el_n)
               r_el_n = usr_mtx_r_sa(r_n)
               usr_mtx_c_v(r_el_n) =
     &              mtx_r_v(el_n)*c_scl_v/scl(mx_n_c+r_n)
               usr_mtx_c_ix(r_el_n) = c_n
               usr_mtx_r_sa(r_n) = r_el_n + 1
 130        continue
 140     continue
      endif
c
c     All the row starts have been incremented upto the start of the
c     next row (as part of the process of defining the entries) so
c     reset them all.
c
      usr_mtx_r_sa(n_r+1) = usr_mtx_r_sa(n_r)
      usr_mtx_r_sa(0) = 1
      do 210, r_n = n_r, 1, -1
         usr_mtx_r_sa(r_n) = usr_mtx_r_sa(r_n-1)
 210  continue
      return
      end
 
C->>> ---------------------------------------------------> ems_rm_ml <<<
c     Removes the data structures associated with the current model.
c
      subroutine ems_rm_ml(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'ICTVR.INC'
      integer is(0:is_n_en_m1)
 
      if (iand(ml_blk_st_msk, ml_blk_st_ml_vec) .ne. 0)
     &     call ems_rm_blk_ml_vec(is)
      if (iand(ml_blk_st_msk, ml_blk_st_ml_lng_nm) .ne. 0)
     &     call ems_rm_blk_ml_lng_nm(is)
CM      IF (emsol_xa .EQ. 1) THEN
C?      if (iand(ml_blk_st_msk, ml_blk_st_ml_pk_bs_os) .ne. 0)
C?     &     call ems_rm_blk_ml_pk_bs_os(is)
C?      if (iand(ml_blk_st_msk, ml_blk_st_ml_pwl_vr) .ne. 0)
C?     &     call ems_rm_blk_ml_pwl_vr(is)
CM      ENDIF
      if (iand(ml_blk_st_msk, ml_blk_st_ml_sol) .ne. 0)
     &     call ems_rm_blk_ml_sol(is)
      if (iand(ml_blk_st_msk, ml_blk_st_ml_vr_ls) .ne. 0)
     &     call ems_rm_blk_ml_vr_ls(is)
      if (iand(ml_blk_st_msk, ml_blk_st_ml_bs_inv_p) .ne. 0)
     &     call ems_rm_blk_ml_bs_inv_p(is)
      if (iand(ml_blk_st_msk, ml_blk_st_ml_c_mtx) .ne. 0)
     &     call ems_rm_blk_ml_c_mtx(is)
      call ems_rm_inv(is)
      if (iand(ml_blk_st_msk, ml_blk_st_ml_r_mtx) .ne. 0)
     &     call ems_rm_blk_ml_r_mtx(is)
      if (iand(ml_blk_st_msk, ml_blk_st_dvx) .ne. 0)
     &     call ems_rm_blk_dvx(is)
      if (iand(ml_blk_st_msk, ml_blk_st_ml_aux_sol) .ne. 0)
     &     call ems_rm_blk_ml_aux_sol(is)
      if (iand(ml_blk_st_msk, ml_blk_st_ml_aux_blk) .ne. 0)
     &     call ems_rm_blk_ml_aux_blk(is)
CM      IF (emsol_xa .EQ. 1) THEN
C?      if (iand(ml_blk_st_msk, ml_blk_st_ml_prsl_sv) .ne. 0)
C?     &     call ems_rm_blk_ml_prsl_sv(is)
CM      ENDIF
      if (iand(ml_blk_st_msk, ml_blk_st_ml_rg_da) .ne. 0)
     &     call ems_rm_blk_ml_rg_da(is)
      if (iand(ml_blk_st_msk, ml_blk_st_ml_u_bs) .ne. 0)
     &     call ems_rm_blk_ml_u_bs(is)
      if (iand(ml_blk_st_msk, ml_blk_st_ml_usr_c_mtx) .ne. 0)
     &     call ems_rm_blk_ml_usr_c_mtx(is)
      if (iand(ml_blk_st_msk, ml_blk_st_ml_usr_r_mtx) .ne. 0)
     &     call ems_rm_blk_ml_usr_r_mtx(is)
      call ems_iz_ml_ct_vr
      return
      end
 
C->>> ---------------------------------------------> ems_iz_ml_ct_vr <<<
c     Initialise the control variables following removal of a model.
c
      subroutine ems_iz_ml_ct_vr
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
 
      n_si_it = 0
      n_el_in_aux_blk = 0
      n_aux_blk = 0
      n_i_fs_sol = 0
      n_sos_se = 0
      ml_blk_st_msk = 0
      ml_da_st_msk = 0
      ml_da_no_cg_msk = 0
      du_sol_mode = 0
      sslv_en_msk = 0
      n_bs = 0
      n_reset = 0
      n_dvx_fwk = 0
      r_du_act_sgn = 0
      pc_alg = 0
      inv_mx_n_eta = 0
      inv_mx_n_eta_el = 0
      n_inv_sing = 0
      rq_inv = 1
      l1_cz_r_mx_n_cdd = 0
      eta_fi_n_grp = 0
      eta_fi_n_eta = 0
      eta_fi_n_ix = 0
      n_lo_c_eta = 0
      return
      end
