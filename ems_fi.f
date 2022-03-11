CM
C->>> ------------------------------------------------> ems_rd_ml_fi <<<
      subroutine ems_rd_ml_fi(fm_rd_ml_fi_rn, ems_fi_cn,
     &     ml_mtx_fmt, i_da_wr_cn, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'CHCTVR.INC'
      include 'RLCTVR.INC'
      include 'ICTVR.INC'
      include 'EMSFI.INC'
      include 'EMSMSG.INC'
      integer fm_rd_ml_fi_rn, ems_fi_cn
      integer ml_mtx_fmt, i_da_wr_cn, is(0:is_n_en_m1)
      double precision ds(0:ds_n_en_m1)
      double precision ems_ch8_t_rl
      integer fi_io
      integer fi_fmt_vers_n, fi_fmt_rv_n
      character*(ml_nm_mx_n_ch) mu_ch
      integer rd_n_r, rd_n_c, ix_n, vr_n
      logical rd_nx_ky
      character*(ems_fi_ky_mx_n_ch) ems_fi_ky
c
c     Assume that the file is formatted
c
      fi_io = fi_io_fmt
      ems_fi_ml_nm = 'UNNAMED '
      rd_nx_ky = .false.
      call ems_rd_ems_fi_ky(ems_fi_cn, ems_fi_ky)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 8000
      if (ems_fi_ky(1:ems_fi_mps_ml_nm_ky_n_ch) .eq.
     &     ems_fi_mps_ml_nm_ky) then
CM      IF (emsol_xa .EQ. 1) THEN
C?         backspace ems_fi_cn
C?         if (fm_rd_ml_fi_rn .eq. rd_ml_fi_rn_mps) then
C?c
C?c     If there is any existing model then remove it.
C?c
C?            if (ml_blk_st_msk .ne. 0 .or.
C?     &           ml_da_st_msk .ne. 0) call ems_rm_ml(is)
C?            call ems_rd_mps_ml(ems_fi_cn, ds, is)
C?         else
C?c
C?c     If there is any existing basis then remove it?
C?c
C?            call ems_rd_mps_bs(ems_fi_cn, ds, is)
C?         endif
C?         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 8000
CM      ELSE
         goto 8990
CM      ENDIF
      else
         if (ems_fi_ky(1:ems_fi_fmt_ky_n_ch) .eq. ems_fi_fmt_ky) then
            read(ems_fi_cn, *, err = 8020, end = 8010)
     &           fi_fmt_vers_n, fi_fmt_rv_n
            call ems_rd_ems_fi(ems_fi_cn, fi_io, fi_fmt_vers_n, ds, is)
            if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 8000
         else
            fi_fmt_vers_n = 0
            fi_fmt_rv_n = 0
            backspace ems_fi_cn
            if (fm_rd_ml_fi_rn .eq. rd_ml_fi_rn_basi) then
               n_bp_vr = 0
               read(ems_fi_cn, 9000, err = 8020, end = 8010)ch_bs_nm
               read(ems_fi_cn, *, err = 8020, end = 8010)rd_n_r, rd_n_c
               if (rd_n_r .ne. n_r) goto 8030
               if (rd_n_c .ne. n_c) goto 8040
               do 10, ix_n = 1, n_c + n_r
                  if (ix_n .le. n_c) then
                     vr_n = ix_n
                  else
                     vr_n = ix_n + mx_n_c - n_c
                  endif
                  read(ems_fi_cn, *, err = 8020, end = 8010)
     &                 is(p_st+vr_n), ds(p_pr_act+vr_n)
                  if (iand(is(p_st+vr_n), alt_bt) .ne. 0) then
                     ml_da_st_msk = ior(ml_da_st_msk, ml_da_st_alt_lp)
                     if (iand(is(p_st+vr_n), bp_bt) .ne. 0) then
                        ml_da_st_msk = ior(ml_da_st_msk, ml_da_st_bp_vr)
                        n_bp_vr = n_bp_vr + 1
                     endif
                  endif
 10            continue
            else
               call ems_rd_ems_fi(ems_fi_cn, fi_io, fi_fmt_vers_n,
     &              ds, is)
               if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 8000
            endif
         endif
      endif
c
c     Indicate that the condition of the basis should not be questioned
c
c      ml_da_st_msk = ior(ml_da_st_msk, ml_da_st_bs_cond_ok)
c
c     Indicate that the condition of the basis may be questionable.
c
      ml_da_st_msk =
     &     ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_bs_cond_ok)
      if (fm_rd_ml_fi_rn .eq. rd_ml_fi_rn_mps) then
c
c     Record the number of columns in the original matrix. This can be
c     useful when forming a new matrix after adding columns.
c
         is(p_mtx_c_sa) = n_c
c
c     Check that that the bounds are consistent.
c
         call ems_ck_ml_bd(n_r, n_c,
     &        ds(p_lbc+1+mx_n_c), ds(p_ubc+1+mx_n_c),
     &        ds(p_lbc+1), ds(p_ubc+1), tl_mx_iz_pr_act)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 8000
c
c     Assign a name to variable zero
c
         if (ml_nm_n_ch .eq. 8) then
            ds(p_nm) = ems_ch8_t_rl('********')
         else
            call ems_iz_ch(mu_ch, '*')
            call ems_ch_t_rl(ml_nm_n_ch, mu_ch, ds(p_lng_nm))
         endif
c
c     Store the model name
c
         rl_ml_nm = ems_ch8_t_rl(ch_ml_nm)
         if (iand(ml_blk_st_msk, ml_blk_st_ml_sol) .eq. 0) then
c
c     If there are no data structures for the solution then allocate
c     them and set up a logical basis.
c
            call ems_iz_blk_ml_sol(is)
            if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 8000
            call ems_iz_blk_ml_vr_ls(is)
            if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 8000
            call ems_reset_pc_alg(dvx_mode, is)
            if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 8000
            call ems_ca_iz_lg_bs(ds, is)
         endif
      endif
 7000 continue
      return
 8000 continue
      call ems_rm_ml(is)
      goto 7000
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8030 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9803)rd_n_r, n_r
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8040 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9804)rd_n_c, n_c
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
CM      IF (emsol_xa .EQ. 1) THEN
CM      ELSE
 8990 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9899)
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
CM      ENDIF
 9000 format(a8)
 9801 format('Premature end of file for model ', a8)
 9802 format('Error reading file for model ', a8)
 9803 format('Number of rows in basis file ', i9,
     &     ' is inconsistent with number of rows in model ', i7)
 9804 format('Number of columns in basis file ', i9,
     &     ' is inconsistent with number of columns in model ', i7)
CM      IF (emsol_xa .EQ. 1) THEN
CM      ELSE
 9899 format('Cannot read model file in this format')
CM      ENDIF
      end
 
C->>> ------------------------------------------------> ems_wr_ml_fi <<<
c     Splits ds and is in order to writes the current model as an MPS
c     file or just writes an index-driven file.
c
      subroutine ems_wr_ml_fi(fm_wr_ml_fi_rn, ems_fi_cn,
     &     ty, n_fld, wr_v, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSFI.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'EMSMSG.INC'
      include 'ICTVR.INC'
      include 'CHCTVR.INC'
      integer fm_wr_ml_fi_rn, ems_fi_cn, ty, n_fld, wr_v
      double precision ds(0:ds_n_en_m1)
      integer is(0:is_n_en_m1)
      integer fi_io
 
CM      IF (emsol_xa .EQ. 1) THEN
C?      if (ems_fi_wr_msk .ge. 0) then
CM      ENDIF
c
c     Assume that the file is formatted
c
         fi_io = fi_io_fmt
c
c     Set default values for the ems_fi_io_msk if no bits are set in
c     ems_fi_wr_msk
c
         if (ems_fi_wr_msk .eq. 0) then
            if (fm_wr_ml_fi_rn .eq. wr_ml_fi_rn_bcdo) then
               ems_fi_io_msk = ems_fi_io_mn_ml_msk
c
c     Add more bits according to whether the model has names, row costs,
c     BP vars, PWL vars, etc
c
            else
               ems_fi_io_msk = ems_fi_io_bs_bt
            endif
         endif
         call ems_wr_ems_fi(ems_fi_cn, fi_io, ds, is)
CM      IF (emsol_xa .EQ. 1) THEN
C?      else
C?c
C?c     Before writing out an MPS file, make sure that the model has names
C?c     and that they are 8-character names
C?c
C?         if (iand(ml_da_st_msk, ml_da_st_nm) .eq. 0 .or.
C?     &        ml_nm_n_ch .ne. 8) then
C?            call ems_iz_df_nm('C', 8, 1, 1, n_c, ds(p_nm+1))
C?            call ems_iz_df_nm('R', 8, 1, 1, n_r, ds(p_nm+mx_n_c+1))
C?            ml_da_st_msk = ior(ml_da_st_msk, ml_da_st_nm)
C?         endif
C?         if (fm_wr_ml_fi_rn .eq. wr_ml_fi_rn_bcdo) then
C?            call ems_wr_mps_ml(
C?     &           ds(p_lbc+mx_n_c), ds(p_ubc+mx_n_c), ds(p_nm+mx_n_c),
C?     &           ds(p_lbc), ds(p_ubc), ds(p_nm), ds(p_cbp), ds(p_scl),
C?     &           ds(p_mtx_r_v), is(p_mtx_r_ix), is(p_mtx_c_sa),
C?     &           ems_fi_cn)
C?         else
C?            call ems_wr_mps_bs(
C?     &           ds(p_lbc), ds(p_ubc), ds(p_nm), is(p_st),
C?     &           ds(p_pr_act),
C?     &           ds(p_scl),
C?     &           ems_fi_cn, wr_v)
C?         endif
C?      endif
CM      ENDIF
      return
      end
 
C->>> -----------------------------------------------> ems_rd_ems_fi <<<
      subroutine ems_rd_ems_fi(ems_fi_cn, fi_io, fi_fmt_vers_n, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSFI.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'CHCTVR.INC'
      include 'RLCTVR.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer ems_fi_cn, fi_io, fi_fmt_vers_n, is(0:is_n_en_m1)
      double precision ds(0:ds_n_en_m1)
      integer c_n, r_n
      integer rd_n_r, rd_n_c, rd_n_a_el
      character*(ml_nm_mx_n_ch) mu_ch
      integer p_r_nm, p_c_nm
      logical rd_fi
      logical ca_ems_iz_ml
      logical rd_nx_ky
      logical scl_pr_act
      character*(ems_fi_ky_mx_n_ch) ems_fi_ky
 
      rd_fi = .true.
      ems_fi_ml_nm = 'UNNAMED '
      rd_nx_ky = .false.
      ems_fi_io_msk = 0
      call ems_fi_io_ml_hdr(
     &     ems_fi_cn, rd_fi, fi_io,
     &     ch_ml_nm, ch_ob_nm, ch_rhs_nm, ch_rg_nm, ch_bd_nm)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      call ems_fi_io_ml_dim(
     &     ems_fi_cn, rd_fi, fi_io, fi_fmt_vers_n,
     &     rd_nx_ky, ems_fi_ky,
     &     rd_n_r, rd_n_c, rd_n_a_el)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      if (fi_fmt_vers_n .eq. 0) then
         ca_ems_iz_ml = ml_blk_st_msk .ne. 0 .or. ml_da_st_msk .ne. 0
         if (ca_ems_iz_ml) then
c
c     Remove the existing model
c
            call ems_rm_ml(is)
            ca_ems_iz_ml = .false.
         endif
         if (.not.ca_ems_iz_ml) then
c
c     Initialise the model
c
            call ems_iz_ml(.true., is, rd_n_r, rd_n_c, rd_n_a_el)
            if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
c
c     Zero the row costs if they are not read in
c
            call ems_cp_rl_a(n_r, zero, ds(p_cbp+mx_n_c+1), 0)
            ca_ems_iz_ml = .true.
         endif
c
c     If a matrix is about to be read in, indicate that any scaling
c     has not been applied.
c
         ml_da_st_msk =
     &        ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_scl_ml_mtx)
         call ems_fi_io_ml_mtx(
     &        ems_fi_cn, rd_fi, fi_io,
     &        n_r, n_c, n_a_el,
     &        ds(p_mtx_r_v+1),
     &        is(p_mtx_r_ix+1),
     &        is(p_mtx_c_sa+1))
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
c
c     If there are scaling factors then scale the matrix.
c
         if (iand(ml_da_st_msk, ml_da_st_scl_ml) .ne. 0)
     &        call ems_scl_ml_mtx(ds, is)
         call ems_fi_io_ml_bd(
     &        ems_fi_cn, rd_fi, fi_io,
     &        n_r, n_c,
     &        ds(p_lbc+1), ds(p_lbc+mx_n_c+1),
     &        ds(p_ubc+1), ds(p_ubc+mx_n_c+1))
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
         call ems_fi_io_ml_c_co(
     &        ems_fi_cn, rd_fi, fi_io,
     &        n_c,
     &        ds(p_cbp+1))
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
c
c     Indicate that sufficient data have been read in to define a model.
c
         ml_da_st_msk = ior(ml_da_st_msk, ml_da_st_ld)
         goto 1000
      endif
c
c     Read the rest of the file according to keywords---whole file if
c     file version is >= 0
c
c     Look for a keyword
c
 100  continue
      if (.not. rd_nx_ky) then
         call ems_rd_ems_fi_ky(ems_fi_cn, ems_fi_ky)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      endif
      if (ems_fi_ky(1:ems_fi_e_lin_ky_n_ch) .eq. ems_fi_e_lin_ky)
     &     goto 1000
      rd_nx_ky = .false.
      if (
     &     ems_fi_ky(1:ems_fi_mtx_ky_n_ch) .eq.  ems_fi_mtx_ky .or.
     &     ems_fi_ky(1:ems_fi_c_bd_ky_n_ch) .eq. ems_fi_c_bd_ky .or.
     &     ems_fi_ky(1:ems_fi_r_bd_ky_n_ch) .eq. ems_fi_r_bd_ky .or.
     &     ems_fi_ky(1:ems_fi_c_co_ky_n_ch) .eq. ems_fi_c_co_ky .or.
     &     ems_fi_ky(1:ems_fi_nm_ky_n_ch) .eq.   ems_fi_nm_ky .or.
     &     ems_fi_ky(1:ems_fi_r_co_ky_n_ch) .eq. ems_fi_r_co_ky) then
c
c     If any of these sections is about to be read in then, if the
c     row/column dimensions in the file are not the same as those of
c     the current model, it is removed. Otherwise, the current data
c     structures are simply over-written.
c
         ca_ems_iz_ml = ml_blk_st_msk .ne. 0 .or. ml_da_st_msk .ne. 0
         if (ca_ems_iz_ml) then
            if (rd_n_r .ne. n_r .or. rd_n_c .ne. n_c) then
c
c     A row/column dimension has changed so remove the current model.
c
               call ems_rm_ml(is)
               ca_ems_iz_ml = .false.
            endif
         endif
         if (.not.ca_ems_iz_ml) then
c
c     No model is loaded so initialise the model
c
            call ems_iz_ml(.true., is, rd_n_r, rd_n_c, rd_n_a_el)
            if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
c
c     Zero the row costs if they are not read in
c
            call ems_cp_rl_a(n_r, zero, ds(p_cbp+mx_n_c+1), 0)
            ca_ems_iz_ml = .true.
         endif
      endif
      if (
     &     ems_fi_ky(1:ems_fi_bs_ky_n_ch) .eq. ems_fi_bs_ky .or.
     &     ems_fi_ky(1:ems_fi_bs_in_alt_fmt_ky_n_ch) .eq.
     &     ems_fi_bs_in_alt_fmt_ky) then
c
c     If either of these sections is about to be read in then, if the
c     row/column dimensions in the file are not the same as those of
c     the current model---or if there is no current model---an error is
c     returned.
c     ?? could read partial name-driven basis format?
c
         if (iand(ml_da_st_msk, ml_da_st_ld) .eq. 0) goto 8030
         if (rd_n_r .ne. n_r) goto 8040
         if (rd_n_c .ne. n_c) goto 8050
c
c     If no solution vectors have been allocated then do so.
c
         if (iand(ml_blk_st_msk, ml_blk_st_ml_sol) .eq. 0) then
            call ems_iz_blk_ml_sol(is)
            if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
            call ems_reset_pc_alg(dvx_mode, is)
            if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
         endif
         if (iand(ml_blk_st_msk, ml_blk_st_ml_vr_ls) .eq. 0) then
            call ems_iz_blk_ml_vr_ls(is)
            if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
         endif
c
c     Indicate that no invert exists for the current basis.
c
         rq_inv = rq_inv_nw_bs
         ml_da_st_msk =
     &        ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_inv)
      endif
      if (ems_fi_ky(1:ems_fi_mtx_ky_n_ch) .eq. ems_fi_mtx_ky) then
c
c     Remove any existing matrix data structures and re-allocate them
c
         if (iand(ml_blk_st_msk, ml_blk_st_ml_c_mtx) .ne. 0)
     &        call ems_rm_blk_ml_c_mtx(is)
         if (iand(ml_blk_st_msk, ml_blk_st_ml_r_mtx) .ne. 0)
     &        call ems_rm_blk_ml_r_mtx(is)
         mx_n_a_el = max(n_a_el, max(mx_n_a_el, n_a_el-mx_n_a_el))
c
c     Allocate storage for the model matrix.
c
         call ems_iz_blk_ml_c_mtx(is)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
c
c     If a matrix is about to be read in, indicate that any scaling
c     has not been applied.
c
         ml_da_st_msk =
     &        ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_scl_ml_mtx)
         call ems_fi_io_ml_mtx(
     &        ems_fi_cn, rd_fi, fi_io,
     &        n_r, n_c, n_a_el,
     &        ds(p_mtx_r_v+1),
     &        is(p_mtx_r_ix+1),
     &        is(p_mtx_c_sa+1))
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
c
c     If there are scaling factors then scale the matrix.
c
         if (iand(ml_da_st_msk, ml_da_st_scl_ml) .ne. 0)
     &        call ems_scl_ml_mtx(ds, is)
      else if (ems_fi_ky(1:ems_fi_c_bd_ky_n_ch) .eq.
     &        ems_fi_c_bd_ky) then
         call ems_fi_io_ml_c_bd(
     &        ems_fi_cn, rd_fi, fi_io,
     &        n_c,
     &        ds(p_lbc+1), ds(p_ubc+1))
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      else if (ems_fi_ky(1:ems_fi_r_bd_ky_n_ch) .eq.
     &        ems_fi_r_bd_ky) then
         call ems_fi_io_ml_r_bd(
     &        ems_fi_cn, rd_fi, fi_io,
     &        n_r,
     &        ds(p_lbc+mx_n_c+1), ds(p_ubc+mx_n_c+1))
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      else if (ems_fi_ky(1:ems_fi_c_co_ky_n_ch) .eq.
     &        ems_fi_c_co_ky) then
         call ems_fi_io_ml_c_co(
     &        ems_fi_cn, rd_fi, fi_io,
     &        n_c,
     &        ds(p_cbp+1))
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      else if (ems_fi_ky(1:ems_fi_nm_ky_n_ch) .eq. ems_fi_nm_ky) then
         read(ems_fi_cn, *, err = 8020, end = 8010)ml_nm_n_ch
         ml_nm_n_rl = (ml_nm_n_ch+7)/8
         if (ml_nm_n_ch .eq. 8) then
            p_c_nm = p_nm
         else
c
c     Remove any existing block for long names
c
            if (iand(ml_blk_st_msk, ml_blk_st_ml_lng_nm) .ne. 0)
     &           call ems_rm_blk_ml_lng_nm(is)
c
c     Open a block for long names
c
c     ?? First truncate block with vector for 8-character names
c
            call ems_iz_blk_ml_lng_nm(is)
            if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
            p_c_nm = p_lng_nm
         endif
         p_r_nm = p_c_nm + ml_nm_n_rl*mx_n_c
         call ems_fi_io_ml_nm(
     &        rd_nx_ky, ems_fi_ky,
     &        ems_fi_cn, rd_fi, fi_io,
     &        n_r, n_c,
     &        ml_nm_n_ch, ml_nm_n_rl,
     &        mu_ch, ds(p_c_nm+ml_nm_n_rl), ds(p_r_nm+ml_nm_n_rl))
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      else if (ems_fi_ky(1:ems_fi_ob_fn_cs_ky_n_ch) .eq.
     &        ems_fi_ob_fn_cs_ky) then
         call ems_fi_io_ml_ob_fn_cs(
     &        ems_fi_cn, rd_fi, fi_io,
     &        ob_fn_cs)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      else if (ems_fi_ky(1:ems_fi_r_co_ky_n_ch) .eq.
     &        ems_fi_r_co_ky) then
         call ems_fi_io_ml_r_co(
     &        ems_fi_cn, rd_fi, fi_io,
     &        n_r,
     &        ds(p_cbp+mx_n_c+1))
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      else if (ems_fi_ky(1:ems_fi_pwl_ky_n_ch) .eq.
     &        ems_fi_pwl_ky) then
      else if (ems_fi_ky(1:ems_fi_bs_ky_n_ch) .eq.
     &        ems_fi_bs_ky) then
c
c     When reading in a basis, apply to the primal activities any
c     scaling (which is currently applied).
c
         scl_pr_act = iand(ml_da_st_msk , ml_da_st_scl_ml_sol) .ne. 0
         call ems_fi_io_ml_bs(
     &        ems_fi_cn, rd_fi, fi_io,
     &        n_r, n_c,
     &        is(p_st+1), is(p_st+mx_n_c+1),
     &        ds(p_pr_act+1), ds(p_pr_act+mx_n_c+1),
     &        scl_pr_act, ds(p_scl+1), ds(p_scl+mx_n_c+1))
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
c
c     Indicate that the condition of the basis should not be questioned
c
         ml_da_st_msk = ior(ml_da_st_msk, ml_da_st_bs_cond_ok)
      else if (ems_fi_ky(1:ems_fi_bs_in_alt_fmt_ky_n_ch) .eq.
     &        ems_fi_bs_in_alt_fmt_ky) then
         call ems_fi_io_ml_bs_in_alt_fmt(
     &        ems_fi_cn, rd_fi, fi_io,
     &        n_r, n_c,
     &        ds(p_lbc+1), ds(p_lbc+mx_n_c+1),
     &        ds(p_ubc+1), ds(p_ubc+mx_n_c+1),
     &        is(p_st+1), is(p_st+mx_n_c+1),
     &        ds(p_pr_act+1), ds(p_pr_act+mx_n_c+1),
     &        su_vr_bs_bt, bc_vr_bs_st, non_bc_vr_bs_st)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      else
c
c     If the keyword is not recognised, assume that it is the first of
c     the old format names
c
         ml_nm_n_ch = 8
         ml_nm_n_rl = 1
         p_c_nm = p_nm
         p_r_nm = p_c_nm + mx_n_c
         call ems_fi_io_ml_nm_ol(
     &        ems_fi_cn, rd_fi, fi_io,
     &        n_r, n_c,
     &        ds(p_c_nm+1), ds(p_r_nm+1))
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
         goto 1000
      endif
c
c     If no model is currently loaded, determine whether sufficient data
c     have been read in to define a model
c
      if (iand(ml_da_st_msk, ml_da_st_ld) .eq. 0) then
         if (iand(ems_fi_io_msk, ems_fi_io_mn_ml_msk) .eq.
     &        ems_fi_io_mn_ml_msk) then
            ml_da_st_msk = ml_da_st_msk + ml_da_st_ld
         endif
      endif
      goto 100
 1000 continue
c
c     Change to maximize according to the objective name
c
      ems_fi_ky = ch_ob_nm
      call ems_str_t_lo_case(ems_fi_ky)
      if (ems_fi_ky(1:ems_fi_mn_ky_n_ch) .eq. ems_fi_mn_ky) then
         mx_mn = one
      else if (ems_fi_ky(1:ems_fi_mx_ky_n_ch) .eq. ems_fi_mx_ky) then
         mx_mn = -one
      endif
c
c     Complete any names not read in---unless none have been read
c
      if (ml_nm_n_ch .eq. 8) then
         p_c_nm = p_nm
      else
         p_c_nm = p_lng_nm
      endif
      p_r_nm = p_c_nm + ml_nm_n_rl*mx_n_c
      if (iand(ems_fi_io_nm_msk, ems_fi_io_msk) .ne. 0) then
c
c     Some names have been read in.
c     make sure that they are complete
c     ?? What if the model had (say) row names before and only column
c     names have been read in?
c
         if (iand(ems_fi_io_c_nm_bt, ems_fi_io_msk) .eq. 0) then
c
c     Assign names for columns
c
            call ems_iz_df_nm('C',
     &           ml_nm_n_ch, ml_nm_n_rl, 1, n_c, ds(p_c_nm+1))
         else if (iand(ems_fi_io_r_nm_bt, ems_fi_io_msk) .eq. 0) then
c
c     Assign names for rows
c
            call ems_iz_df_nm('R',
     &           ml_nm_n_ch, ml_nm_n_rl, 1, n_r, ds(p_r_nm+1))
         endif
c
c     Indicate that the model has names.
c
         ml_da_st_msk = ior(ml_da_st_msk, ml_da_st_nm)
      endif
c
c     Assign values for the extra rows
c
      call ems_iz_df_nm('R',
     &     ml_nm_n_ch, ml_nm_n_rl, n_r+1, mx_n_r, ds(p_r_nm+1))
      do 1210, r_n = n_r+1, mx_n_r
         ds(p_lbc+mx_n_c+r_n) = -inf
         ds(p_ubc+mx_n_c+r_n) = inf
         ds(p_cbp+mx_n_c+r_n) = zero
 1210 continue
c
c     Assign values for the extra columns
c
      call ems_iz_df_nm('C',
     &     ml_nm_n_ch, ml_nm_n_rl, n_c+1, mx_n_c, ds(p_c_nm+1))
      do 1220, c_n = n_c+1, mx_n_c
         ds(p_lbc+c_n) = -inf
         ds(p_ubc+c_n) = inf
         ds(p_cbp+c_n) = zero
 1220 continue
 7000 continue
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8030 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9803)
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8040 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9804)rd_n_r, n_r
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8050 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9805)rd_n_c, n_c
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 9801 format('Premature end of file for model ', a8)
 9802 format('Error reading file for model ', a8)
 9803 format('Cannot read a basis: no model loaded')
 9804 format('Number of rows in EMS file (', i9,
     &     ') and current model (', i7, ') are inconsistent')
 9805 format('Number of columns in EMS file (', i9,
     &     ') and current model (', i7, ') are inconsistent')
      end
 
C->>> -----------------------------------------------> ems_rd_ems_fi <<<
      subroutine ems_wr_ems_fi(ems_fi_cn, fi_io, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSFI.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'EMSMSG.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'CHCTVR.INC'
      integer ems_fi_cn, fi_io
      double precision ds(0:ds_n_en_m1)
      integer is(0:is_n_en_m1)
      character*(ml_nm_mx_n_ch) mu_ch
      integer p_r_nm, p_c_nm
      logical rd_fi
      logical scl_pr_act
      logical rd_nx_ky
      character*(ems_fi_ky_mx_n_ch) ems_fi_ky
 
      rd_fi = .false.
      ems_fi_ml_nm = ch_ml_nm
      write(ems_fi_cn, 9000, err = 8030)'EMS_file_format'
      write(ems_fi_cn, *, err = 8030)
     &     ems_wr_fi_fmt_vers_n, ems_wr_fi_fmt_rv_n
c
c     Indicate max/min in the objective name
c
      if (mx_mn .eq. one) then
         ch_ob_nm = ems_fi_mn_ky
      else if (mx_mn .eq. -one) then
         ch_ob_nm = ems_fi_mx_ky
      endif
      call ems_fi_io_ml_hdr(
     &     ems_fi_cn, rd_fi, fi_io,
     &     ch_ml_nm, ch_ob_nm, ch_rhs_nm, ch_rg_nm, ch_bd_nm)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      call ems_fi_io_ml_dim(
     &     ems_fi_cn, rd_fi, fi_io, ems_wr_fi_fmt_vers_n,
     &     rd_nx_ky, ems_fi_ky,
     &     n_r, n_c, n_a_el)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      if (iand(ems_fi_io_msk, ems_fi_io_mtx_bt) .ne. 0) then
c
c     Have to un-scale and then re-scale the matrix values since they
c     are written out un-scaled as a single array.
c
         if (iand(ml_da_st_msk, ml_da_st_scl_ml_mtx) .ne. 0)
     &        call ems_un_scl_ml_mtx(ds, is)
         call ems_fi_io_ml_mtx(
     &        ems_fi_cn, rd_fi, fi_io,
     &        n_r, n_c, n_a_el,
     &        ds(p_mtx_r_v+1),
     &        is(p_mtx_r_ix+1),
     &        is(p_mtx_c_sa+1))
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
         if (iand(ml_da_st_msk, ml_da_st_scl_ml) .ne. 0)
     &        call ems_scl_ml_mtx(ds, is)
      endif
      if (iand(ems_fi_io_msk, ems_fi_io_c_bd_msk) .ne. 0) then
         call ems_fi_io_ml_c_bd(
     &        ems_fi_cn, rd_fi, fi_io,
     &        n_c,
     &        ds(p_lbc+1), ds(p_ubc+1))
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      endif
      if (iand(ems_fi_io_msk, ems_fi_io_r_bd_msk) .ne. 0) then
         call ems_fi_io_ml_r_bd(
     &        ems_fi_cn, rd_fi, fi_io,
     &        n_r,
     &        ds(p_lbc+mx_n_c+1), ds(p_ubc+mx_n_c+1))
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      endif
      if (iand(ems_fi_io_msk, ems_fi_io_c_co_bt) .ne. 0) then
         call ems_fi_io_ml_c_co(
     &        ems_fi_cn, rd_fi, fi_io,
     &        n_c,
     &        ds(p_cbp+1))
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      endif
      if (iand(ems_fi_io_msk, ems_fi_io_nm_msk) .ne. 0) then
         if (ml_nm_n_ch .eq. 8) then
            p_c_nm = p_nm
         else
            p_c_nm = p_lng_nm
         endif
         p_r_nm = p_c_nm + ml_nm_n_rl*mx_n_c
         call ems_fi_io_ml_nm(
     &        rd_nx_ky, ems_fi_ky,
     &        ems_fi_cn, rd_fi, fi_io,
     &        n_r, n_c,
     &        ml_nm_n_ch, ml_nm_n_rl,
     &        mu_ch, ds(p_c_nm+ml_nm_n_rl), ds(p_r_nm+ml_nm_n_rl))
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      endif
c      if () then
c         call ems_fi_io_ml_ob_fn_cs(
c     &        ems_fi_cn, rd_fi, fi_io,
c     &        ob_fn_cs)
c         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
c      endif
      if (iand(ems_fi_io_msk, ems_fi_io_r_co_bt) .ne. 0) then
         call ems_fi_io_ml_r_co(
     &        ems_fi_cn, rd_fi, fi_io,
     &        n_r,
     &        ds(p_cbp+mx_n_c+1))
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      endif
      if (iand(ems_fi_io_msk, ems_fi_io_pwl_bt) .ne. 0) then
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      endif
      if (iand(ems_fi_io_msk, ems_fi_io_bs_bt) .ne. 0) then
c
c     When writing out a basis, remove any scaling from the primal
c     activities.
c
         scl_pr_act = iand(ml_da_st_msk , ml_da_st_scl_ml_sol) .ne. 0
         call ems_fi_io_ml_bs(
     &        ems_fi_cn, rd_fi, fi_io,
     &        n_r, n_c,
     &        is(p_st+1), is(p_st+mx_n_c+1),
     &        ds(p_pr_act+1), ds(p_pr_act+mx_n_c+1),
     &        scl_pr_act, ds(p_scl+1), ds(p_scl+mx_n_c+1))
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      endif
      if (iand(ems_fi_io_msk, ems_fi_io_bs_in_alt_fmt_bt) .ne. 0) then
         call ems_fi_io_ml_bs_in_alt_fmt(
     &        ems_fi_cn, rd_fi, fi_io,
     &        n_r, n_c,
     &        ds(p_lbc+1), ds(p_lbc+mx_n_c+1),
     &        ds(p_ubc+1), ds(p_ubc+mx_n_c+1),
     &        is(p_st+1), is(p_st+mx_n_c+1),
     &        ds(p_pr_act+1), ds(p_pr_act+mx_n_c+1),
     &        su_vr_bs_bt, bc_vr_bs_st, non_bc_vr_bs_st)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      endif
      write(ems_fi_cn, 9000, err = 8030)ems_fi_e_lin_ky
 7000 continue
      return
 8030 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9803)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 9000 format(a)
 9803 format('Error writing file for model ', a8)
      end
C->>> ------------------------------------------------> ems_cp_rl_nm <<<
c     Copies a real name which may consist of mode than one real value
c
      subroutine ems_cp_rl_nm(fm_rl_nm, t_rl_nm)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSFI.INC'
      include 'ICTVR.INC'
      double precision fm_rl_nm(ml_nm_n_rl)
      double precision t_rl_nm(ml_nm_n_rl)
      integer rl_n
      do 10, rl_n = 1, ml_nm_n_rl
         t_rl_nm(rl_n) = fm_rl_nm(rl_n)
 10   continue
      return
      end
 
