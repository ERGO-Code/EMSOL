C->>> ------------------------------------------------> ems_wr_rg_da <<<
c     Report the ranging data
c
      subroutine ems_wr_rg_da(
     &     rl_nm, st, pr_act, scl, co,
     &     co_rg_up_co_v, co_rg_lo_co_v,
     &     co_rg_up_ob_v, co_rg_lo_ob_v,
     &     co_rg_up_act_v, co_rg_lo_act_v,
     &     co_rg_up_en_vr, co_rg_lo_en_vr,
     &     co_rg_up_lv_vr, co_rg_lo_lv_vr,
     &     bd_rg_up_bd_v, bd_rg_lo_bd_v,
     &     bd_rg_up_ob_v, bd_rg_lo_ob_v,
     &     bd_rg_up_en_vr, bd_rg_lo_en_vr,
     &     bd_rg_up_lv_vr, bd_rg_lo_lv_vr)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSFI.INC'
      include 'PRTS.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer st(0:mx_n_c+n_r)
      double precision rl_nm(ml_nm_n_rl, 0:mx_n_c+n_r)
      double precision pr_act(0:mx_n_c+n_r)
      double precision scl(0:mx_n_c+n_r)
      double precision co(0:n_c)
      double precision co_rg_up_co_v(0:n_c)
      double precision co_rg_lo_co_v(0:n_c)
      double precision co_rg_up_ob_v(0:n_c)
      double precision co_rg_lo_ob_v(0:n_c)
      double precision co_rg_up_act_v(0:n_c)
      double precision co_rg_lo_act_v(0:n_c)
      integer co_rg_up_en_vr(0:n_c)
      integer co_rg_lo_en_vr(0:n_c)
      integer co_rg_up_lv_vr(0:n_c)
      integer co_rg_lo_lv_vr(0:n_c)
      double precision bd_rg_up_bd_v(0:mx_n_c+n_r)
      double precision bd_rg_lo_bd_v(0:mx_n_c+n_r)
      double precision bd_rg_up_ob_v(0:mx_n_c+n_r)
      double precision bd_rg_lo_ob_v(0:mx_n_c+n_r)
      integer bd_rg_up_en_vr(0:mx_n_c+n_r)
      integer bd_rg_lo_en_vr(0:mx_n_c+n_r)
      integer bd_rg_up_lv_vr(0:mx_n_c+n_r)
      integer bd_rg_lo_lv_vr(0:mx_n_c+n_r)
      integer ix_n, vr_ix, vr_n, te_vr_n, ems_vr_ty
      double precision rl_up_en_vr_nm(ml_nm_mx_n_rl)
      double precision rl_up_lv_vr_nm(ml_nm_mx_n_rl)
      double precision rl_lo_en_vr_nm(ml_nm_mx_n_rl)
      double precision rl_lo_lv_vr_nm(ml_nm_mx_n_rl)
      double precision co_v, pr_act_v
      double precision up_rg_v, up_ob_rg_v, lo_rg_v, lo_ob_rg_v
      double precision up_act_v, lo_act_v
 
      logical nw_pg
 
      if (iand(ml_da_st_msk, ml_da_st_rg_da) .eq. 0) goto 7000
c
c     Start by writing out rows:
c     Start a new page
c     The dual activities may have had their sign changed and are
c     written out with a sign change.
c
      nw_pg = .true.
      ems_vr_ty = ems_vr_ty_c
      do 10, ix_n = 1, n_c
         vr_ix = ix_n
         vr_n = ix_n
         if (nw_pg)
     &        call ems_wr_prts_hdr(prts_hdr_ty_co_rg_da, ems_vr_ty)
         co_v = co(vr_n)
         up_rg_v =    co_rg_up_co_v(vr_n)
         up_ob_rg_v = co_rg_up_ob_v(vr_n)
         up_act_v =   co_rg_up_act_v(vr_n)
         lo_rg_v =    co_rg_lo_co_v(vr_n)
         lo_ob_rg_v = co_rg_lo_ob_v(vr_n)
         lo_act_v =   co_rg_lo_act_v(vr_n)
c         if (iand(ml_da_st_msk, ml_da_st_scl_ml_sol) .ne. 0) then
c            up_rg_v = up_rg_v*scl(vr_n)
c            lo_rg_v = lo_rg_v*scl(vr_n)
c         endif
         te_vr_n = co_rg_up_en_vr(vr_n)
         if (te_vr_n .ge. 0) then
            call ems_cp_rl_nm(rl_nm(1, te_vr_n), rl_up_en_vr_nm)
         else
            call ems_cp_rl_nm(rl_nm(1, mx_n_c-te_vr_n), rl_up_en_vr_nm)
         endif
         te_vr_n = co_rg_up_lv_vr(vr_n)
         if (te_vr_n .ge. 0) then
            call ems_cp_rl_nm(rl_nm(1, te_vr_n), rl_up_lv_vr_nm)
         else
            call ems_cp_rl_nm(rl_nm(1, mx_n_c-te_vr_n), rl_up_lv_vr_nm)
         endif
         te_vr_n = co_rg_lo_en_vr(vr_n)
         if (te_vr_n .ge. 0) then
            call ems_cp_rl_nm(rl_nm(1, te_vr_n), rl_lo_en_vr_nm)
         else
            call ems_cp_rl_nm(rl_nm(1, mx_n_c-te_vr_n), rl_lo_en_vr_nm)
         endif
         te_vr_n = co_rg_lo_lv_vr(vr_n)
         if (te_vr_n .ge. 0) then
            call ems_cp_rl_nm(rl_nm(1, te_vr_n), rl_lo_lv_vr_nm)
         else
            call ems_cp_rl_nm(rl_nm(1, mx_n_c-te_vr_n), rl_lo_lv_vr_nm)
         endif
         call ems_wr_1_co_rg_da(vr_ix,
     &        rl_nm(1, vr_n), st(vr_n), co_v,
     &        up_rg_v, up_ob_rg_v, up_act_v,
     &        rl_up_en_vr_nm, rl_up_lv_vr_nm,
     &        lo_rg_v, lo_ob_rg_v, lo_act_v,
     &        rl_lo_en_vr_nm, rl_lo_lv_vr_nm,
     &        ems_vr_ty)
         nw_pg = ems_msg_pg_li_n .eq. n_pg_li-1
 10   continue
      nw_pg = .true.
      ems_vr_ty = ems_vr_ty_r
      do 20, ix_n = 1, n_r + n_c
         if (ix_n .eq. n_r+1) then
c
c     Switch to writing out columns:
c     Start a new page
c
            nw_pg = .true.
            ems_vr_ty = ems_vr_ty_c
         endif
         if (ix_n .le. n_r) then
            vr_ix = ix_n
            vr_n = ix_n + mx_n_c
         else
            vr_ix = ix_n - n_r
            vr_n = vr_ix
         endif
         if (nw_pg)
     &        call ems_wr_prts_hdr(prts_hdr_ty_bd_rg_da, ems_vr_ty)
         pr_act_v = pr_act(vr_n)
         up_rg_v =    bd_rg_up_bd_v(vr_n)
         up_ob_rg_v = bd_rg_up_ob_v(vr_n)
         lo_rg_v =    bd_rg_lo_bd_v(vr_n)
         lo_ob_rg_v = bd_rg_lo_ob_v(vr_n)
         if (iand(ml_da_st_msk, ml_da_st_scl_ml_sol) .ne. 0) then
            pr_act_v = pr_act_v/scl(vr_n)
c            up_rg_v = up_rg_v/scl(vr_n)
c            lo_rg_v = lo_rg_v/scl(vr_n)
         endif
         te_vr_n = bd_rg_up_en_vr(vr_n)
         if (te_vr_n .ge. 0) then
            call ems_cp_rl_nm(rl_nm(1, te_vr_n), rl_up_en_vr_nm)
         else
            call ems_cp_rl_nm(rl_nm(1, mx_n_c-te_vr_n), rl_up_en_vr_nm)
         endif
         te_vr_n = bd_rg_up_lv_vr(vr_n)
         if (te_vr_n .ge. 0) then
            call ems_cp_rl_nm(rl_nm(1, te_vr_n), rl_up_lv_vr_nm)
         else
            call ems_cp_rl_nm(rl_nm(1, mx_n_c-te_vr_n), rl_up_lv_vr_nm)
         endif
         te_vr_n = bd_rg_lo_en_vr(vr_n)
         if (te_vr_n .ge. 0) then
            call ems_cp_rl_nm(rl_nm(1, te_vr_n), rl_lo_en_vr_nm)
         else
            call ems_cp_rl_nm(rl_nm(1, mx_n_c-te_vr_n), rl_lo_en_vr_nm)
         endif
         te_vr_n = bd_rg_lo_lv_vr(vr_n)
         if (te_vr_n .ge. 0) then
            call ems_cp_rl_nm(rl_nm(1, te_vr_n), rl_lo_lv_vr_nm)
         else
            call ems_cp_rl_nm(rl_nm(1, mx_n_c-te_vr_n), rl_lo_lv_vr_nm)
         endif
         call ems_wr_1_rg_da(vr_ix,
     &        rl_nm(1, vr_n), st(vr_n), pr_act_v,
     &        up_rg_v, up_ob_rg_v, rl_up_en_vr_nm, rl_up_lv_vr_nm,
     &        lo_rg_v, lo_ob_rg_v, rl_lo_en_vr_nm, rl_lo_lv_vr_nm,
     &        ems_vr_ty)
         nw_pg = ems_msg_pg_li_n .eq. n_pg_li-1
 20   continue
 7000 continue
      return
      end
 
C->>> -------------------------------------------> ems_wr_1_co_rg_da <<<
c     Writes the cost range data for a particular variable.
c
      subroutine ems_wr_1_co_rg_da(ix,
     &     rl_vr_nm, vr_st, v,
     &     up_rg, up_ob_rg, up_act_v, rl_up_en_vr_nm, rl_up_lv_vr_nm,
     &     lo_rg, lo_ob_rg, lo_act_v, rl_lo_en_vr_nm, rl_lo_lv_vr_nm,
     &     ems_vr_ty)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSFI.INC'
      include 'PRTS.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer ix, vr_st, ems_vr_ty
      double precision rl_vr_nm(ml_nm_n_rl), v
      double precision up_rg, up_ob_rg, up_act_v
      double precision lo_rg, lo_ob_rg, lo_act_v
      double precision rl_up_en_vr_nm(ml_nm_n_rl)
      double precision rl_up_lv_vr_nm(ml_nm_n_rl)
      double precision rl_lo_en_vr_nm(ml_nm_n_rl)
      double precision rl_lo_lv_vr_nm(ml_nm_n_rl)
      character*(ml_nm_mx_n_ch) mu_ch_vr_nm
      character*(ml_nm_mx_n_ch) mu_ch_en_vr_nm
      character*(ml_nm_mx_n_ch) mu_ch_lv_vr_nm
      character*20 ems_wr_rl_t_ch20
      character*4 ems_wr_st_t_ch4
      integer t_ch_n
 
      call ems_rl_t_ch(ml_nm_n_ch, rl_vr_nm, mu_ch_vr_nm)
      t_ch_n = 0
      call ems_rl_t_ch(ml_nm_n_ch, rl_up_en_vr_nm, mu_ch_en_vr_nm)
      call ems_rl_t_ch(ml_nm_n_ch, rl_up_lv_vr_nm, mu_ch_lv_vr_nm)
      vr_li(t_ch_n+1:t_ch_n+2+ml_nm_n_ch) =
     &     '  '//mu_ch_vr_nm(1:ml_nm_n_ch)
      t_ch_n = t_ch_n + 2+ml_nm_n_ch
      vr_li(t_ch_n+1:t_ch_n+6) = '  '//ems_wr_st_t_ch4(vr_st)
      t_ch_n = t_ch_n + 6
      vr_li(t_ch_n+1:t_ch_n+prts_rl_fld_ln) =
     &     '  '//ems_wr_rl_t_ch20(v)
      t_ch_n = t_ch_n + prts_rl_fld_ln
c
      vr_li(t_ch_n+1:t_ch_n+prts_rl_fld_ln) =
     &     '  '//ems_wr_rl_t_ch20(up_rg)
      t_ch_n = t_ch_n + prts_rl_fld_ln
c
      vr_li(t_ch_n+1:t_ch_n+prts_rl_fld_ln) =
     &     '  '//ems_wr_rl_t_ch20(up_ob_rg)
      t_ch_n = t_ch_n + prts_rl_fld_ln
c
      vr_li(t_ch_n+1:t_ch_n+prts_rl_fld_ln) =
     &     '  '//ems_wr_rl_t_ch20(up_act_v)
      t_ch_n = t_ch_n + prts_rl_fld_ln
c
      vr_li(t_ch_n+1:t_ch_n+2+ml_nm_n_ch) =
     &     '  '//mu_ch_en_vr_nm(1:ml_nm_n_ch)
      t_ch_n = t_ch_n + 2+ml_nm_n_ch
c
      vr_li(t_ch_n+1:t_ch_n+2+ml_nm_n_ch) =
     &     '  '//mu_ch_lv_vr_nm(1:ml_nm_n_ch)
      t_ch_n = t_ch_n + 2+ml_nm_n_ch
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     ix, vr_li(1:t_ch_n)
      call ems_msg_wr_li(64)
 
      t_ch_n = 0
      call ems_rl_t_ch(ml_nm_n_ch, rl_lo_en_vr_nm, mu_ch_en_vr_nm)
      call ems_rl_t_ch(ml_nm_n_ch, rl_lo_lv_vr_nm, mu_ch_lv_vr_nm)
      vr_li(t_ch_n+1:t_ch_n+2+ml_nm_n_ch) = '          '
      t_ch_n = t_ch_n + 2+ml_nm_n_ch
      vr_li(t_ch_n+1:t_ch_n+6) = '      '
      t_ch_n = t_ch_n + 6
      vr_li(t_ch_n+1:t_ch_n+prts_rl_fld_ln) =
     &     '                      '
      t_ch_n = t_ch_n + prts_rl_fld_ln
c
      vr_li(t_ch_n+1:t_ch_n+prts_rl_fld_ln) =
     &     '  '//ems_wr_rl_t_ch20(lo_rg)
      t_ch_n = t_ch_n + prts_rl_fld_ln
c
      vr_li(t_ch_n+1:t_ch_n+prts_rl_fld_ln) =
     &     '  '//ems_wr_rl_t_ch20(lo_ob_rg)
      t_ch_n = t_ch_n + prts_rl_fld_ln
c
      vr_li(t_ch_n+1:t_ch_n+prts_rl_fld_ln) =
     &     '  '//ems_wr_rl_t_ch20(lo_act_v)
      t_ch_n = t_ch_n + prts_rl_fld_ln
c
      vr_li(t_ch_n+1:t_ch_n+2+ml_nm_n_ch) =
     &     '  '//mu_ch_en_vr_nm(1:ml_nm_n_ch)
      t_ch_n = t_ch_n + 2+ml_nm_n_ch
c
      vr_li(t_ch_n+1:t_ch_n+2+ml_nm_n_ch) =
     &     '  '//mu_ch_lv_vr_nm(1:ml_nm_n_ch)
      t_ch_n = t_ch_n + 2+ml_nm_n_ch
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9001)vr_li(1:t_ch_n)
      call ems_msg_wr_li(64)
      return
 9000 format(1x, i7, a)
 9001 format(1x, 7x, a)
      end
 
C->>> ----------------------------------------------> ems_wr_1_rg_da <<<
c     Writes the range data for a particular variable.
c
      subroutine ems_wr_1_rg_da(ix,
     &     rl_vr_nm, vr_st, v,
     &     up_rg, up_ob_rg, rl_up_en_vr_nm, rl_up_lv_vr_nm,
     &     lo_rg, lo_ob_rg, rl_lo_en_vr_nm, rl_lo_lv_vr_nm,
     &     ems_vr_ty)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSFI.INC'
      include 'PRTS.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer ix, vr_st, ems_vr_ty
      double precision rl_vr_nm(ml_nm_n_rl)
      double precision v, up_rg, up_ob_rg, lo_rg, lo_ob_rg
      double precision rl_up_en_vr_nm(ml_nm_n_rl)
      double precision rl_up_lv_vr_nm(ml_nm_n_rl)
      double precision rl_lo_en_vr_nm(ml_nm_n_rl)
      double precision rl_lo_lv_vr_nm(ml_nm_n_rl)
      character*(ml_nm_mx_n_ch) mu_ch_vr_nm
      character*(ml_nm_mx_n_ch) mu_ch_en_vr_nm
      character*(ml_nm_mx_n_ch) mu_ch_lv_vr_nm
      character*20 ems_wr_rl_t_ch20
      character*4 ems_wr_st_t_ch4
      integer t_ch_n
 
      call ems_rl_t_ch(ml_nm_n_ch, rl_vr_nm, mu_ch_vr_nm)
      t_ch_n = 0
      call ems_rl_t_ch(ml_nm_n_ch, rl_up_en_vr_nm, mu_ch_en_vr_nm)
      call ems_rl_t_ch(ml_nm_n_ch, rl_up_lv_vr_nm, mu_ch_lv_vr_nm)
      vr_li(t_ch_n+1:t_ch_n+2+ml_nm_n_ch) =
     &     '  '//mu_ch_vr_nm(1:ml_nm_n_ch)
      t_ch_n = t_ch_n + 2+ml_nm_n_ch
      vr_li(t_ch_n+1:t_ch_n+6) = '  '//ems_wr_st_t_ch4(vr_st)
      t_ch_n = t_ch_n + 6
      vr_li(t_ch_n+1:t_ch_n+prts_rl_fld_ln) =
     &     '  '//ems_wr_rl_t_ch20(v)
      t_ch_n = t_ch_n + prts_rl_fld_ln
c
      vr_li(t_ch_n+1:t_ch_n+prts_rl_fld_ln) =
     &     '  '//ems_wr_rl_t_ch20(up_rg)
      t_ch_n = t_ch_n + prts_rl_fld_ln
c
      vr_li(t_ch_n+1:t_ch_n+prts_rl_fld_ln) =
     &     '  '//ems_wr_rl_t_ch20(up_ob_rg)
      t_ch_n = t_ch_n + prts_rl_fld_ln
c
      vr_li(t_ch_n+1:t_ch_n+2+ml_nm_n_ch) =
     &     '  '//mu_ch_en_vr_nm(1:ml_nm_n_ch)
      t_ch_n = t_ch_n + 2+ml_nm_n_ch
c
      vr_li(t_ch_n+1:t_ch_n+2+ml_nm_n_ch) =
     &     '  '//mu_ch_lv_vr_nm(1:ml_nm_n_ch)
      t_ch_n = t_ch_n + 2+ml_nm_n_ch
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     ix, vr_li(1:t_ch_n)
      call ems_msg_wr_li(64)
 
      t_ch_n = 0
      call ems_rl_t_ch(ml_nm_n_ch, rl_lo_en_vr_nm, mu_ch_en_vr_nm)
      call ems_rl_t_ch(ml_nm_n_ch, rl_lo_lv_vr_nm, mu_ch_lv_vr_nm)
      vr_li(t_ch_n+1:t_ch_n+2+ml_nm_n_ch) = '          '
      t_ch_n = t_ch_n + 2+ml_nm_n_ch
      vr_li(t_ch_n+1:t_ch_n+6) = '      '
      t_ch_n = t_ch_n + 6
      vr_li(t_ch_n+1:t_ch_n+prts_rl_fld_ln) =
     &     '                      '
      t_ch_n = t_ch_n + prts_rl_fld_ln
c
      vr_li(t_ch_n+1:t_ch_n+prts_rl_fld_ln) =
     &     '  '//ems_wr_rl_t_ch20(lo_rg)
      t_ch_n = t_ch_n + prts_rl_fld_ln
c
      vr_li(t_ch_n+1:t_ch_n+prts_rl_fld_ln) =
     &     '  '//ems_wr_rl_t_ch20(lo_ob_rg)
      t_ch_n = t_ch_n + prts_rl_fld_ln
c
      vr_li(t_ch_n+1:t_ch_n+2+ml_nm_n_ch) =
     &     '  '//mu_ch_en_vr_nm(1:ml_nm_n_ch)
      t_ch_n = t_ch_n + 2+ml_nm_n_ch
c
      vr_li(t_ch_n+1:t_ch_n+2+ml_nm_n_ch) =
     &     '  '//mu_ch_lv_vr_nm(1:ml_nm_n_ch)
      t_ch_n = t_ch_n + 2+ml_nm_n_ch
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9001)vr_li(1:t_ch_n)
      call ems_msg_wr_li(64)
      return
 9000 format(1x, i7, a)
 9001 format(1x, 7x, a)
      end
 
C->>> ---------------------------------------------------> ems_lp_rs <<<
c     Writes out data from the LP solver.
c
      subroutine ems_lp_rs(ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'EMSFI.INC'
      include 'PRTS.INC'
      include 'ICTVR.INC'
      double precision ds(0:ds_n_en_m1)
      integer is(0:is_n_en_m1)
      integer p_c_nm
 
      if (wr_ml_da_msk .eq. 0) goto 7000
      if (iand(wr_ml_da_msk, wr_ml_da_ml_z_bt) .ne. 0) call ems_wr_ml_z
      if (iand(wr_ml_da_msk, wr_ml_da_sol_st_bt) .ne. 0)
     &     call ems_wr_sol_st
      if (iand(ml_da_st_msk, ml_da_st_ld) .eq. 0) goto 7000
      if (ml_nm_n_ch .eq. 8) then
         p_c_nm = p_nm
      else
         p_c_nm = p_lng_nm
      endif
      if (wr_ml_da_msk .ge. wr_ml_da_nm_bt) then
         call ems_wr_al_vr_st(
     &        ds(p_c_nm), is(p_st), ds(p_pr_act), ds(p_du_act),
     &        ds(p_lbc), ds(p_ubc), ds(p_scl))
      endif
CM      IF (emsol_xa .EQ. 1) THEN
C?      if (iand(ml_da_st_msk, ml_da_st_alt_lp) .ne. 0) then
C?c         if (iand(ml_da_st_msk, ml_da_st_bp_vr) .ne. 0 .and.
C?c     &        (wr_ml_st_msk .eq. wr_ml_st_al .or.
C?c     &        iand(wr_ml_st_msk, wr_ml_st_bp_bt) .ne. 0)) then
C?         if (iand(ml_da_st_msk, ml_da_st_bp_vr) .ne. 0 .and.
C?     &        iand(wr_ml_st_msk, wr_ml_st_bp_bt) .ne. 0) then
C?            call ems_wr_al_bp_vr_st(
C?     &           ds(p_c_nm), is(p_st), ds(p_pr_act), ds(p_du_act),
C?     &           ds(p_lbc), ds(p_cbp), ds(p_ubc), ds(p_scl))
C?         endif
C?         if (iand(ml_da_st_msk, ml_da_st_pwl_vr) .ne. 0 .and.
C?     &        (wr_ml_st_msk .eq. wr_ml_st_al .or.
C?     &        iand(wr_ml_st_msk, wr_ml_st_pwl_bt) .ne. 0)) then
C?            call ems_wr_al_pwl_vr_st(
C?     &           ds(p_c_nm), is(p_st), ds(p_pr_act), ds(p_du_act),
C?     &           ds(p_scl),
C?     &           is(p_pwl_vr_ls),
C?     &           ds(p_pwl_vr_rf_pr_act_v),
C?     &           ds(p_pwl_vr_rf_ob_fn_v),
C?     &           ds(p_pwl_vr_da_v),
C?     &           is(p_pwl_vr_da_sa),
C?     &           is(p_pwl_vr_cu_sn),
C?     &           is(p_pwl_vr_ix))
C?         endif
C?      endif
CM      ENDIF
 7000 continue
      return
      end
 
C->>> -------------------------------------------------> ems_wr_ml_z <<<
      subroutine ems_wr_ml_z
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'CHCTVR.INC'
      include 'EMSFI.INC'
      include 'PRTS.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
 
      if (iand(ml_da_st_msk, ml_da_st_ld) .eq. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
         call ems_msg_wr_li(warn_msg_n)
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)ch_ml_nm
         call ems_msg_wr_li(8)
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9110)
     &        n_r, n_c, n_a_el
         call ems_msg_wr_li(16)
         if (iand(ml_da_st_msk, ml_da_st_alt_lp) .ne. 0) then
            if (iand(ml_da_st_msk, ml_da_st_bp_vr) .ne. 0) then
               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9120)n_bp_vr
               call ems_msg_wr_li(info_msg_n)
            endif
            if (iand(ml_da_st_msk, ml_da_st_pwl_vr) .ne. 0) then
               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9130)
     &              n_pwl_vr, n_pwl_vr_da_en
               call ems_msg_wr_li(info_msg_n)
            endif
         endif
      endif
      return
 9000 format('No model has been loaded')
 9100 format('Description of problem ', a8)
 9110 format('        Matrix has ', i7, ' rows, ', i7,
     &       ' columns and ', i9, ' entries ')
 9120 format('        Problem has ', i7, ' breakpoint variables')
 9130 format('        Problem has ', i7,
     &     ' piecewise linear variables with ', i7, ' data entries')
      end
 
C->>> -----------------------------------------------> ems_wr_sol_st <<<
      subroutine ems_wr_sol_st
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      character*24 ems_wr_prob_st_t_ch24
 
      if (iand(ml_da_st_msk, ml_da_st_ld) .eq. 0) goto 7000
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9200)
      call ems_msg_wr_li(9)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9210)n_si_it,
     &     ob_fn_v, ems_wr_prob_st_t_ch24(prob_st)
      call ems_msg_wr_li(1)
      if (prob_st .ne. prob_st_unknown) then
         if (n_pr_ifs .gt. 0) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9300)
     &           n_pr_ifs, su_pr_ifs
            call ems_msg_wr_li(3)
         end if
         if (n_du_ifs .gt. 0) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9310)
     &           n_du_ifs, su_du_ifs
            call ems_msg_wr_li(4)
         end if
      endif
 7000 continue
      return
 9200 format('Problem Status ')
 9210 format('Iteration number: ', i7,
     &     ' Objective value: ', g15.8, 2x, a)
 9300 format('Number of primal infeasibilities: ', i7,
     &     ' Sum of primal infeasibilities: ', g11.4)
 9310 format('Number of dual   infeasibilities: ', i7,
     &     ' Sum of dual   infeasibilities: ', g11.4)
      end
 
C->>> ---------------------------------------> ems_wr_prob_st_t_ch24 <<<
c     Determines a character string according to the value of prob_st
c
      character*24 function ems_wr_prob_st_t_ch24(prob_st)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      integer prob_st
      if (prob_st .eq. prob_st_unknown) then
         ems_wr_prob_st_t_ch24 = 'Unknown status'
      else if (prob_st .eq. prob_st_op) then
         ems_wr_prob_st_t_ch24 = 'Optimal'
      else if (prob_st .eq. prob_st_ifs) then
         ems_wr_prob_st_t_ch24 = 'infeasible'
      else if (prob_st .eq. prob_st_unbd) then
         ems_wr_prob_st_t_ch24 = 'Unbounded problem'
      else if (prob_st .eq. prob_st_mx_n_it) then
         ems_wr_prob_st_t_ch24 = 'Iteration limit exceeded'
      else if (prob_st .eq. prob_st_no_sol) then
         ems_wr_prob_st_t_ch24 = 'No solution'
      else if (prob_st .eq. prob_st_mx_n_sol) then
         ems_wr_prob_st_t_ch24 = 'Subroblem limit exceeded'
      else if (prob_st .eq. prob_st_no_po) then
         ems_wr_prob_st_t_ch24 = 'Out of space'
      else
         ems_wr_prob_st_t_ch24 = 'Unknown problem status'
      end if
      return
      end
C->>> ---------------------------------------------> ems_wr_al_vr_st <<<
c     Writes primal/dual activity and lower/upper bound data for each
c     row and column---except for any BP or PWL variables.
c
      subroutine ems_wr_al_vr_st(rl_nm, st, pr_act, du_act, lb, ub, scl)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSFI.INC'
      include 'PRTS.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer st(0:mx_n_c+n_r)
      double precision rl_nm(ml_nm_n_rl, 0:mx_n_c+n_r)
      double precision pr_act(0:mx_n_c+n_r), du_act(0:mx_n_c+n_r)
      double precision lb(0:mx_n_c+n_r), ub(0:mx_n_c+n_r)
      double precision scl(0:mx_n_c+n_r)
      double precision pr_act_v, du_act_v, lb_v, ub_v
      integer ix_n, vr_ix, vr_n, ems_vr_ty
      integer du_act_sgn, wr_du_act_sgn
      logical nw_pg
      logical ml_pr_act
      logical ml_du_act
      logical scl_ml_sol
 
      if (iand(ml_da_st_msk, ml_da_st_ld) .eq. 0) goto 7000
c
c     Start by writing out rows:
c     Start a new page
c     The dual activities may have had their sign changed and are
c     written out with a sign change.
c
      nw_pg = .true.
      ml_pr_act =  iand(ml_da_st_msk, ml_da_st_bc_pr_act) .ne. 0
      ml_du_act =  iand(ml_da_st_msk, ml_da_st_non_bc_du_act) .ne. 0
      scl_ml_sol = iand(ml_da_st_msk, ml_da_st_scl_ml_sol) .ne. 0
      ems_vr_ty = ems_vr_ty_r
      du_act_sgn = r_du_act_sgn
      wr_du_act_sgn = -1
c
c     If the model does not have primal and/or dual activities then the
c     undefined value is passed and a blank is printed.
c
      pr_act_v = undn
      du_act_v = undn
      do 10, ix_n = 1, n_r + n_c
         if (ix_n .eq. n_r+1) then
c
c     Switch to writing out columns:
c     Start a new page
c     The dual activities have not had their sign changed and are not
c     written out with a sign change.
c
            nw_pg = .true.
            ems_vr_ty = ems_vr_ty_c
            du_act_sgn = 1
            wr_du_act_sgn = 1
         endif
         if (ix_n .le. n_r) then
            vr_ix = ix_n
            vr_n = ix_n + mx_n_c
         else
            vr_ix = ix_n - n_r
            vr_n = vr_ix
         endif
c
c     Extract the primal and dual activities, correcting any sign change
c     for the dual activity, and then scaling them both (if necessary).
c
         if (ml_pr_act) then
            pr_act_v = pr_act(vr_n)
            if (scl_ml_sol) pr_act_v = pr_act_v/scl(vr_n)
         endif
         if (ml_du_act) then
            du_act_v = du_act_sgn*du_act(vr_n)
            if (scl_ml_sol) du_act_v = du_act_v*scl(vr_n)
c
c     Possibly change the sign of the dual activity to be written.
c
            du_act_v = wr_du_act_sgn*du_act_v
         endif
         if (wr_ml_st_msk .eq. wr_ml_st_al .or.
     &        iand(wr_ml_st_msk, wr_ml_st_r_bt) .ne. 0 .or.
     &        (iand(wr_ml_st_msk, wr_ml_st_nz_bt) .ne. 0 .and.
     &        du_act_v .ne. zero) .or.
     &        (iand(wr_ml_st_msk, wr_ml_st_ifs_bt) .ne. 0 .and.
     &        iand(st(vr_n), ifs_bt) .ne. 0)) then
            if (nw_pg)
     &           call ems_wr_prts_hdr(prts_hdr_ty_std_vr, ems_vr_ty)
            lb_v = lb(vr_n)
            ub_v = ub(vr_n)
            if (iand(st(vr_n), su_non_std_vr_bt) .eq. bp_vr_msk) then
               lb_v = -inf
               ub_v = inf
            endif
            call ems_wr_1_vr_st(vr_ix,
     &           rl_nm(1, vr_n), st(vr_n), pr_act_v, du_act_v,
     &           lb_v, ub_v, ems_vr_ty)
            nw_pg = ems_msg_pg_li_n .eq. n_pg_li-1
         endif
 10   continue
 7000 continue
      return
      end
 
C->>> ---------------------------------------------> ems_wr_prts_hdr <<<
c     Writes the header for printing solution, costs, range data etc.
c
      subroutine ems_wr_prts_hdr(prts_hdr_ty, ems_vr_ty)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'EMSFI.INC'
      include 'PRTS.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer prts_hdr_ty, ems_vr_ty
      character*50 ch50
      integer t_ch_n
      logical nm
      integer r_sn_n_ch, c_sn_n_ch
      integer bp_vr_txt_n_ch, pwl_vr_txt_n_ch
      integer co_rg_txt_n_ch, r_bd_rg_txt_n_ch, c_bd_rg_txt_n_ch
      save r_sn_n_ch, c_sn_n_ch
      save bp_vr_txt_n_ch, pwl_vr_txt_n_ch
      save co_rg_txt_n_ch, r_bd_rg_txt_n_ch, c_bd_rg_txt_n_ch
      data r_sn_n_ch/16/c_sn_n_ch/19/
      data bp_vr_txt_n_ch/22/pwl_vr_txt_n_ch/28/
      data co_rg_txt_n_ch/33/r_bd_rg_txt_n_ch/25/c_bd_rg_txt_n_ch/28/
 
      ems_msg_pg_n = ems_msg_pg_n + 1
      ems_msg_pg_li_n = 0
c
c     Construct the first line
c
      t_ch_n = 0
      if (
     &     prts_hdr_ty .eq. prts_hdr_ty_std_vr .or.
     &     prts_hdr_ty .eq. prts_hdr_ty_bp_vr .or.
     &     prts_hdr_ty .eq. prts_hdr_ty_pwl_vr .or.
     &     prts_hdr_ty .eq. prts_hdr_ty_std_co_da .or.
     &     prts_hdr_ty .eq. prts_hdr_ty_bp_da
     &     ) then
         if (ems_vr_ty .eq. ems_vr_ty_r) then
            ch50(t_ch_n+1:r_sn_n_ch) = '   Rows Section'
            t_ch_n = t_ch_n + r_sn_n_ch
         else
            ch50(t_ch_n+1:c_sn_n_ch) = '   Columns Section'
            t_ch_n = t_ch_n + c_sn_n_ch
         endif
         if (
     &        prts_hdr_ty .eq. prts_hdr_ty_std_vr .or.
     &        prts_hdr_ty .eq. prts_hdr_ty_std_co_da
     &        ) then
         else if (
     &        prts_hdr_ty .eq. prts_hdr_ty_bp_vr .or.
     &        prts_hdr_ty .eq. prts_hdr_ty_bp_da
     &        ) then
            ch50(t_ch_n+1:t_ch_n+bp_vr_txt_n_ch) =
     &           ': Breakpoint Variables'
            t_ch_n = t_ch_n + bp_vr_txt_n_ch
         else if (
     &           prts_hdr_ty .eq. prts_hdr_ty_pwl_vr
     &        ) then
            ch50(t_ch_n+1:t_ch_n+pwl_vr_txt_n_ch) =
     &           ': Piecewise Linear Variables'
            t_ch_n = t_ch_n + pwl_vr_txt_n_ch
         endif
      else if (prts_hdr_ty .eq. prts_hdr_ty_co_rg_da) then
         ch50(t_ch_n+1:t_ch_n+co_rg_txt_n_ch) =
     &        'Sensitivity to Objective Function'
         t_ch_n = t_ch_n + co_rg_txt_n_ch
      else if (prts_hdr_ty .eq. prts_hdr_ty_bd_rg_da) then
         if (ems_vr_ty .eq. ems_vr_ty_r) then
            ch50(t_ch_n+1:t_ch_n+r_bd_rg_txt_n_ch) =
     &           'Sensitivity to Row Bounds'
            t_ch_n = t_ch_n + r_bd_rg_txt_n_ch
         else
            ch50(t_ch_n+1:t_ch_n+c_bd_rg_txt_n_ch) =
     &           'Sensitivity to Column Bounds'
            t_ch_n = t_ch_n + c_bd_rg_txt_n_ch
         endif
      endif
      ch50(t_ch_n+1:50) =
     &     '                                                  '
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     ch50, ems_msg_pg_n
      call ems_msg_wr_li(info_msg_n)
c
c     Now construct the second line
c
      nm = iand(wr_ml_da_msk, wr_ml_da_nm_bt) .ne. 0
      t_ch_n = 0
      if (
     &     prts_hdr_ty .eq. prts_hdr_ty_std_vr .or.
     &     prts_hdr_ty .eq. prts_hdr_ty_bp_vr .or.
     &     prts_hdr_ty .eq. prts_hdr_ty_pwl_vr) then
         if (iand(wr_ml_da_msk, wr_ml_da_fu_li) .eq.
     &        wr_ml_da_fu_li) then
            if (prts_hdr_ty .eq. prts_hdr_ty_std_vr) then
               vr_li(t_ch_n+1:t_ch_n+fu_vr_li_ln+ml_nm_n_ch-8) =
     &              mu_ch_prts_c_hd_nm(1:2+ml_nm_n_ch)
     &              //ch6_prts_c_hd_st
     &              //ch22_prts_c_hd_pr_act
     &              //ch22_prts_c_hd_du_act
     &              //ch22_prts_c_hd_lb
     &              //ch22_prts_c_hd_ub
               t_ch_n = t_ch_n + fu_vr_li_ln+ml_nm_n_ch-8
            else if (prts_hdr_ty .eq. prts_hdr_ty_bp_vr) then
               vr_li(t_ch_n+1:t_ch_n+fu_vr_li_ln+ml_nm_n_ch-8) =
     &              mu_ch_prts_c_hd_nm(1:2+ml_nm_n_ch)
     &              //ch6_prts_c_hd_st
     &              //ch22_prts_c_hd_pr_act
     &              //ch22_prts_c_hd_lo_du_act
     &              //ch22_prts_c_hd_bp
     &              //ch22_prts_c_hd_up_du_act
               t_ch_n = t_ch_n + fu_vr_li_ln+ml_nm_n_ch-8
            else
               vr_li(t_ch_n+1:t_ch_n+fu_vr_li_ln+ml_nm_n_ch-8-6) =
     &              mu_ch_prts_c_hd_nm(1:2+ml_nm_n_ch)
     &              //ch6_prts_c_hd_st
     &              //ch22_prts_c_hd_pr_act
     &              //ch22_prts_c_hd_lo_du_act
     &              //ch22_prts_c_hd_up_du_act
     &              //ch22_prts_c_hd_lo_bp
     &              //ch22_prts_c_hd_up_bp
c     &           //ch6_prts_c_hd_sn
               t_ch_n = t_ch_n + fu_vr_li_ln+ml_nm_n_ch-8 - 6
            endif
            go to 100
         end if
         if (iand(wr_ml_da_msk, wr_ml_da_nm_bt) .ne. 0) then
            vr_li(t_ch_n+1:t_ch_n+2+ml_nm_n_ch) =
     &           mu_ch_prts_c_hd_nm(1:2+ml_nm_n_ch)
            t_ch_n = t_ch_n + 2+ml_nm_n_ch
         end if
         if (iand(wr_ml_da_msk, wr_ml_da_st_bt) .ne. 0) then
            vr_li(t_ch_n+1:t_ch_n+6) = ch6_prts_c_hd_st
            t_ch_n = t_ch_n + 6
         end if
         if (iand(wr_ml_da_msk, wr_ml_da_pr_act_bt) .ne. 0) then
            vr_li(t_ch_n+1:t_ch_n+prts_rl_fld_ln) =
     &           ch22_prts_c_hd_pr_act
            t_ch_n = t_ch_n + prts_rl_fld_ln
         end if
         if (prts_hdr_ty .eq. prts_hdr_ty_std_vr) then
            if (iand(wr_ml_da_msk, wr_ml_da_du_act_bt) .ne. 0) then
               vr_li(t_ch_n+1:t_ch_n+prts_rl_fld_ln) =
     &              ch22_prts_c_hd_du_act
               t_ch_n = t_ch_n + prts_rl_fld_ln
            end if
            if (iand(wr_ml_da_msk, wr_ml_da_lb_bt) .ne. 0) then
               vr_li(t_ch_n+1:t_ch_n+prts_rl_fld_ln) =
     &              ch22_prts_c_hd_lb
               t_ch_n = t_ch_n + prts_rl_fld_ln
            end if
            if (iand(wr_ml_da_msk, wr_ml_da_ub_bt) .ne. 0) then
               vr_li(t_ch_n+1:t_ch_n+prts_rl_fld_ln) =
     &              ch22_prts_c_hd_ub
               t_ch_n = t_ch_n + prts_rl_fld_ln
            end if
         else if (prts_hdr_ty .eq. prts_hdr_ty_bp_vr) then
            vr_li(t_ch_n+1:t_ch_n+3*prts_rl_fld_ln) =
     &           ch22_prts_c_hd_lo_du_act
     &           //ch22_prts_c_hd_bp
     &           //ch22_prts_c_hd_up_du_act
            t_ch_n = t_ch_n + 3*prts_rl_fld_ln
         else
            vr_li(t_ch_n+1:t_ch_n+4*prts_rl_fld_ln) =
     &           ch22_prts_c_hd_lo_du_act
     &           //ch22_prts_c_hd_up_du_act
     &           //ch22_prts_c_hd_lo_bp
     &           //ch22_prts_c_hd_up_bp
            t_ch_n = t_ch_n + 4*prts_rl_fld_ln
c     vr_li(t_ch_n+1:t_ch_n+5) = ch6_prts_c_hd_sn
c     t_ch_n = t_ch_n + 6
         endif
      else if (prts_hdr_ty .eq. prts_hdr_ty_std_co_da) then
         if (nm) then
            vr_li(t_ch_n+1:t_ch_n+2+ml_nm_n_ch) =
     &           mu_ch_prts_c_hd_nm(1:2+ml_nm_n_ch)
            t_ch_n = t_ch_n + 2+ml_nm_n_ch
         endif
         vr_li(t_ch_n+1:t_ch_n+prts_rl_fld_ln) = ch22_prts_c_hd_co
         t_ch_n = t_ch_n + prts_rl_fld_ln
      else if (prts_hdr_ty .eq. prts_hdr_ty_bp_da) then
         if (nm) then
            vr_li(t_ch_n+1:t_ch_n+2+ml_nm_n_ch) =
     &           mu_ch_prts_c_hd_nm(1:2+ml_nm_n_ch)
            t_ch_n = t_ch_n + 2+ml_nm_n_ch
         endif
         vr_li(t_ch_n+1:t_ch_n+3*prts_rl_fld_ln) =
     &        ch22_prts_c_hd_lo_co
     &        //ch22_prts_c_hd_bp
     &        //ch22_prts_c_hd_up_co
         t_ch_n = t_ch_n + 3*prts_rl_fld_ln
      else if (prts_hdr_ty .eq. prts_hdr_ty_co_rg_da) then
         vr_li(t_ch_n+1:
     &        t_ch_n+2+ml_nm_n_ch+6+4*prts_rl_fld_ln+2*(2+ml_nm_n_ch)) =
     &        mu_ch_prts_c_hd_nm(1:2+ml_nm_n_ch)
     &        //ch6_prts_c_hd_st
     &        //ch22_prts_c_hd_og_ob
     &        //ch22_prts_c_hd_rg
     &        //ch22_prts_c_hd_ob_rg
     &        //ch22_prts_c_hd_pr_act
     &        //mu_ch_prts_c_hd_en_nm(1:2+ml_nm_n_ch)
     &        //mu_ch_prts_c_hd_lv_nm(1:2+ml_nm_n_ch)
         t_ch_n =
     &        t_ch_n + 2+ml_nm_n_ch+6+4*prts_rl_fld_ln+2*(2+ml_nm_n_ch)
      else if (prts_hdr_ty .eq. prts_hdr_ty_bd_rg_da) then
         vr_li(t_ch_n+1:
     &        t_ch_n+2+ml_nm_n_ch+6+3*prts_rl_fld_ln+2*(2+ml_nm_n_ch)) =
     &        mu_ch_prts_c_hd_nm(1:2+ml_nm_n_ch)
     &        //ch6_prts_c_hd_st
     &        //ch22_prts_c_hd_v
     &        //ch22_prts_c_hd_rg
     &        //ch22_prts_c_hd_ob_rg
     &        //mu_ch_prts_c_hd_en_nm(1:2+ml_nm_n_ch)
     &        //mu_ch_prts_c_hd_lv_nm(1:2+ml_nm_n_ch)
         t_ch_n = t_ch_n +
     &        2+ml_nm_n_ch+6+3*prts_rl_fld_ln+2*(2+ml_nm_n_ch)
      endif
 100  continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)vr_li(1:t_ch_n)
      call ems_msg_wr_li(info_msg_n)
      return
 9000 format(a50, 1x, 'Page ', i4)
 9010 format(1x, 7x, a)
      end
 
C->>> ----------------------------------------------> ems_wr_1_vr_st <<<
c     Writes the status for a particular variable.
c
      subroutine ems_wr_1_vr_st(ix, rl_nm,
     &     vr_st, pr_act, du_act, lb, ub, ems_vr_ty)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSFI.INC'
      include 'PRTS.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer ix, vr_st, ems_vr_ty
      double precision rl_nm(ml_nm_n_rl), pr_act, du_act, lb, ub
      character*20 ems_wr_rl_t_ch20
      character*4 ems_wr_st_t_ch4
      character*(ml_nm_mx_n_ch) mu_ch_vr_nm
      integer vr_li_ln
 
      if (iand(wr_ml_da_msk, wr_ml_da_fu_li) .eq. wr_ml_da_fu_li) then
         call ems_rl_t_ch(ml_nm_n_ch, rl_nm, mu_ch_vr_nm)
         vr_li(1:fu_vr_li_ln+ml_nm_n_ch-8) =
     &        '  '//mu_ch_vr_nm(1:ml_nm_n_ch)
     &        //'  '//ems_wr_st_t_ch4(vr_st)
     &        //'  '//ems_wr_rl_t_ch20(pr_act)
     &        //'  '//ems_wr_rl_t_ch20(du_act)
     &        //'  '//ems_wr_rl_t_ch20(lb)
     &        //'  '//ems_wr_rl_t_ch20(ub)
         vr_li_ln = fu_vr_li_ln + ml_nm_n_ch-8
         go to 100
      end if
      vr_li_ln = 0
      if (iand(wr_ml_da_msk, wr_ml_da_nm_bt) .ne. 0) then
         call ems_rl_t_ch(ml_nm_n_ch, rl_nm, mu_ch_vr_nm)
         vr_li(vr_li_ln+1:vr_li_ln+2+ml_nm_n_ch) =
     &        '  '//mu_ch_vr_nm(1:ml_nm_n_ch)
         vr_li_ln = vr_li_ln + 2+ml_nm_n_ch
      end if
      if (iand(wr_ml_da_msk, wr_ml_da_st_bt) .ne. 0) then
         vr_li(vr_li_ln+1:vr_li_ln+5) = '  '//ems_wr_st_t_ch4(vr_st)
         vr_li_ln = vr_li_ln + 6
      end if
      if (iand(wr_ml_da_msk, wr_ml_da_pr_act_bt) .ne. 0) then
         vr_li(vr_li_ln+1:vr_li_ln+prts_rl_fld_ln) =
     &        '  '//ems_wr_rl_t_ch20(pr_act)
         vr_li_ln = vr_li_ln + prts_rl_fld_ln
      end if
      if (iand(wr_ml_da_msk, wr_ml_da_du_act_bt) .ne. 0) then
         vr_li(vr_li_ln+1:vr_li_ln+prts_rl_fld_ln) =
     &        '  '//ems_wr_rl_t_ch20(du_act)
         vr_li_ln = vr_li_ln + prts_rl_fld_ln
      end if
      if (iand(wr_ml_da_msk, wr_ml_da_lb_bt) .ne. 0) then
         vr_li(vr_li_ln+1:vr_li_ln+prts_rl_fld_ln) =
     &        '  '//ems_wr_rl_t_ch20(lb)
         vr_li_ln = vr_li_ln + prts_rl_fld_ln
      end if
      if (iand(wr_ml_da_msk, wr_ml_da_ub_bt) .ne. 0) then
         vr_li(vr_li_ln+1:vr_li_ln+prts_rl_fld_ln) =
     &        '  '//ems_wr_rl_t_ch20(ub)
         vr_li_ln = vr_li_ln + prts_rl_fld_ln
      end if
 100  continue
      if (ems_vr_ty .eq. ems_vr_ty_r) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &        ix, vr_li(1:vr_li_ln)
         call ems_msg_wr_li(62)
      else if (ems_vr_ty .eq. ems_vr_ty_c) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &        ix, vr_li(1:vr_li_ln)
         call ems_msg_wr_li(64)
      end if
      return
 9000 format(1x, i7, a)
      end
 
C->>> ---------------------------------------------> ems_wr_st_t_ch4 <<<
c     Determines a character*4 string for a given status bit mask.
c
      character*4 function ems_wr_st_t_ch4(vr_st)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSFI.INC'
      include 'PRTS.INC'
      integer vr_st
      character*4 ch4
      if (iand(vr_st, ifs_bt) .ne. 0) then
         ch4 = ch4_prts_st_ifs
      else if (iand(vr_st, bc_bt) .ne. 0) then
         ch4 = ch4_prts_st_bc
      else if (iand(vr_st, alt_bt) .ne. 0) then
         if (iand(vr_st, bp_bt) .ne. 0) then
            ch4 = ch4_prts_st_bp
         else
c
c     ?? Can't distinguish at LB/UB from at BP for PWL
c
            ch4 = ch4_prts_st_pwl
         endif
      else if (iand(vr_st, up_dn) .eq. up_dn) then
         ch4 = ch4_prts_st_fr
      else if (iand(vr_st, up_bt) .ne. 0) then
         ch4 = ch4_prts_st_at_lb
      else if (iand(vr_st, dn_bt) .ne. 0) then
         ch4 = ch4_prts_st_at_ub
      else
         ch4 = ch4_prts_st_fx
      end if
      ems_wr_st_t_ch4 = ch4
      return
      end
 
C->>> ---------------------------------------------> ems_ca_wr_lp_co <<<
c     Calls the routine which writes out the LP costs.
c
      subroutine ems_ca_wr_lp_co(ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSFI.INC'
      include 'PRTS.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      double precision ds(0:ds_n_en_m1)
      integer is(0:is_n_en_m1)
      integer p_c_nm
 
      if (iand(ml_da_st_msk, ml_da_st_ld) .eq. 0) goto 7000
      if (ml_nm_n_ch .eq. 8) then
         p_c_nm = p_nm
      else
         p_c_nm = p_lng_nm
      endif
      if (iand(wr_ml_da_msk, wr_ml_da_co_bt) .ne. 0)
     &     call ems_wr_lp_co(ds(p_c_nm), ds(p_cbp))
CM      IF (emsol_xa .EQ. 1) THEN
C?      if (iand(ml_da_st_msk, ml_da_st_alt_lp) .ne. 0) then
C?         if (iand(ml_da_st_msk, ml_da_st_bp_vr) .ne. 0 .and.
C?     &        iand(wr_ml_st_msk, wr_ml_st_bp_bt) .ne. 0)
C?     &        call ems_wr_lp_bp(
C?     &        ds(p_c_nm),
C?     &        is(p_st),
C?     &        ds(p_lbc),
C?     &        ds(p_cbp),
C?     &        ds(p_ubc))
C?         if (iand(ml_da_st_msk, ml_da_st_pwl_vr) .ne. 0 .and.
C?     &        iand(wr_ml_st_msk, wr_ml_st_pwl_bt) .ne. 0)
C?     &        call ems_rp_al_pwl_vr_da(
C?     &        .false.,
C?     &        mx_n_c+n_r, mx_n_c, n_pwl_vr, n_pwl_vr_da_en,
C?     &        is(p_pwl_vr_ls+1),
C?     &        ds(p_pwl_vr_rf_pr_act_v+1),
C?     &        ds(p_pwl_vr_rf_ob_fn_v+1),
C?     &        ds(p_pwl_vr_da_v+1),
C?     &        is(p_pwl_vr_da_sa+1))
C?      endif
CM      ENDIF
 7000 continue
      return
      end
 
C->>> ------------------------------------------------> ems_wr_lp_co <<<
c     Writes out the LP costs.
c
      subroutine ems_wr_lp_co(rl_c_nm, c_co)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSFI.INC'
      include 'PRTS.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      character*20 ems_wr_rl_t_ch20
      double precision rl_c_nm(ml_nm_n_rl, 0:n_c)
      double precision c_co(0:n_c)
      character*(ml_nm_mx_n_ch) mu_ch_c_nm
      integer c_n
      logical nm, nw_pg
      character*20 ch20
      double precision co_v
c
c     Writes row/column names if the 3-bit of wr_ml_da_msk is set.
c
      nm = iand(wr_ml_da_msk, wr_ml_da_nm_bt) .ne. 0
      nw_pg = .true.
      do 10 c_n = 1, n_c
         if (nw_pg)
     &        call ems_wr_prts_hdr(prts_hdr_ty_std_co_da, ems_vr_ty_c)
         co_v = c_co(c_n)
         ch20 = ems_wr_rl_t_ch20(co_v)
         if (nm) then
            call ems_rl_t_ch(ml_nm_n_ch, rl_c_nm(1, c_n), mu_ch_c_nm)
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9030)
     &           c_n, mu_ch_c_nm(1:ml_nm_n_ch), ch20
            call ems_msg_wr_li(64)
         else
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9031)c_n, ch20
            call ems_msg_wr_li(64)
         end if
         nw_pg = ems_msg_pg_li_n .eq. n_pg_li-1
 10   continue
      return
 9030 format(1x, i7, 2x, a, 2x, a20)
 9031 format(1x, i7, 2x, a20)
      end
 
C->>> --------------------------------------------> ems_ca_wr_cu_mtx <<<
c     Calls the routine which writes out the current matrix.
c
      subroutine ems_ca_wr_cu_mtx(ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSFI.INC'
      include 'PRTS.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      double precision ds(0:ds_n_en_m1)
      integer is(0:is_n_en_m1)
      integer p_c_nm
 
      if (ml_nm_n_ch .eq. 8) then
         p_c_nm = p_nm
      else
         p_c_nm = p_lng_nm
      endif
      if (iand(ml_da_st_msk, ml_da_st_ld) .eq. 0) goto 7000
      if (iand(wr_ml_da_msk, wr_ml_da_mtx_bt) .ne. 0) then
         call ems_wr_cu_mtx(
     &        ds(p_c_nm+mx_n_c*ml_nm_n_rl),
     &        ds(p_c_nm),
     &        is(p_mtx_r_ix),
     &        ds(p_mtx_r_v),
     &        is(p_mtx_c_sa),
     &        ds(p_scl))
      endif
 7000 continue
      return
      end
C->>> ---------------------------------------------> ems_ca_wr_rg_da <<<
c     Calls the routine which writes out the current matrix.
c
      subroutine ems_ca_wr_rg_da(ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSFI.INC'
      include 'PRTS.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      double precision ds(0:ds_n_en_m1)
      integer is(0:is_n_en_m1)
      integer p_c_nm
 
      if (ml_nm_n_ch .eq. 8) then
         p_c_nm = p_nm
      else
         p_c_nm = p_lng_nm
      endif
      if (iand(ml_da_st_msk, ml_da_st_ld) .eq. 0) goto 7000
      if (wr_rg_da_msk .gt. 0 .and.
     &     iand(ml_da_st_msk, ml_da_st_rg_da) .ne. 0) call ems_wr_rg_da(
     &     ds(p_c_nm), is(p_st), ds(p_pr_act), ds(p_scl), ds(p_cbp),
     &     ds(p_co_rg_up_co_v), ds(p_co_rg_lo_co_v),
     &     ds(p_co_rg_up_ob_v), ds(p_co_rg_lo_ob_v),
     &     ds(p_co_rg_up_act_v), ds(p_co_rg_lo_act_v),
     &     is(p_co_rg_up_en_vr), is(p_co_rg_lo_en_vr),
     &     is(p_co_rg_up_lv_vr), is(p_co_rg_lo_lv_vr),
     &     ds(p_bd_rg_up_bd_v), ds(p_bd_rg_lo_bd_v),
     &     ds(p_bd_rg_up_ob_v), ds(p_bd_rg_lo_ob_v),
     &     is(p_bd_rg_up_en_vr), is(p_bd_rg_lo_en_vr),
     &     is(p_bd_rg_up_lv_vr), is(p_bd_rg_lo_lv_vr))
 7000 continue
      return
      end
 
C->>> -----------------------------------------------> ems_wr_cu_mtx <<<
c     Writes out the current matrix.
c
      subroutine ems_wr_cu_mtx(rl_r_nm, rl_c_nm,
     &     mtx_r_ix, mtx_r_v, mtx_c_sa, scl)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSFI.INC'
      include 'PRTS.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      character*20 ems_wr_rl_t_ch20
      double precision rl_r_nm(ml_nm_n_rl, 0:n_r)
      double precision rl_c_nm(ml_nm_n_rl, 0:n_c)
      integer mtx_c_sa(0:n_c+1), mtx_r_ix(0:n_a_el)
      double precision mtx_r_v(0:n_a_el)
      double precision scl(0:mx_n_c+n_r)
      character*(ml_nm_mx_n_ch) mu_ch_r_nm
      character*(ml_nm_mx_n_ch) mu_ch_c_nm
      integer r_n, c_n, el_n
      logical nm
      character*20 ch20
      double precision mtx_v, c_scl
c
c     Writes row/column names if the 3-bit of wr_ml_da_msk is set.
c
      nm = iand(wr_ml_da_msk, wr_ml_da_nm_bt) .ne. 0
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9110)
      call ems_msg_wr_li(7)
      if (nm) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9120)
     &        ch8_prts_c_hd_c,
     &        mu_ch_prts_c_hd_nm(1:2+ml_nm_n_ch),
     &        ch8_prts_c_hd_r,
     &        mu_ch_prts_c_hd_nm(1:2+ml_nm_n_ch),
     &        ch22_prts_c_hd_v
         call ems_msg_wr_li(63)
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9121)
     &        ch8_prts_c_hd_c,
     &        ch8_prts_c_hd_r,
     &        ch22_prts_c_hd_v
         call ems_msg_wr_li(63)
      end if
      do 110 c_n = 1, n_c
         if (iand(ml_da_st_msk, ml_da_st_scl_ml_mtx) .ne. 0)
     &        c_scl = scl(c_n)
         do 120 el_n = mtx_c_sa(c_n), mtx_c_sa(c_n+1)-1
            r_n = mtx_r_ix(el_n)
            mtx_v = mtx_r_v(el_n)
            if (iand(ml_da_st_msk, ml_da_st_scl_ml_mtx) .ne. 0)
     &           mtx_v = mtx_v*c_scl/scl(mx_n_c+r_n)
            ch20 = ems_wr_rl_t_ch20(mtx_v)
            if (nm) then
               call ems_rl_t_ch(ml_nm_n_ch, rl_c_nm(1, c_n), mu_ch_c_nm)
               call ems_rl_t_ch(ml_nm_n_ch, rl_r_nm(1, r_n), mu_ch_r_nm)
               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9130)
     &              c_n, mu_ch_c_nm(1:ml_nm_n_ch),
     &              r_n, mu_ch_r_nm(1:ml_nm_n_ch), ch20
               call ems_msg_wr_li(13)
            else
               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9131)
     &              c_n, r_n, ch20
               call ems_msg_wr_li(13)
            end if
 120     continue
 110  continue
      return
 9110 format('Matrix entries ')
 9120 format(2(a8, a), a22)
 9121 format(2 a8,     a22)
 9130 format(1x, i7, 2x, a, 1x, i7, 2x, a, 2x, a20)
 9131 format(1x, i7, 1x, i7, 2x, a20)
      end
 
C->>> ---------------------------------------------> ems_iz_prts_com <<<
c     Initialise the common blocks for PRTS
c
      subroutine ems_iz_prts_com
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSFI.INC'
      include 'ICTVR.INC'
      include 'PRTS.INC'
c
c     Assign values to data printed at column headings.
c
      ch6_prts_c_hd_st =         '  Stat'
      ch6_prts_c_hd_sn =         '  Sect'
      ch8_prts_c_hd_r =          '     Row'
      ch8_prts_c_hd_c =          '  Column'
      call ems_iz_ch(mu_ch_prts_c_hd_nm, '.')
      mu_ch_prts_c_hd_nm(1:2) =    '  '
      mu_ch_prts_c_hd_nm(2+1:2+8) =    '..Name..'
      call ems_iz_ch(mu_ch_prts_c_hd_en_nm, '.')
      mu_ch_prts_c_hd_en_nm(1:2) =    '  '
      mu_ch_prts_c_hd_en_nm(2+1:2+8) = 'Entering'
      call ems_iz_ch(mu_ch_prts_c_hd_lv_nm, '.')
      mu_ch_prts_c_hd_lv_nm(1:2) =    '  '
      mu_ch_prts_c_hd_lv_nm(2+1:2+8) = 'Leaving.'
      ch22_prts_c_hd_v =         '  .......Value........'
      ch22_prts_c_hd_pr_act =    '  ..Primal.Activity...'
      ch22_prts_c_hd_lo_du_act = '  ...Lower.Dual.......'
      ch22_prts_c_hd_du_act =    '  ...Dual.Activity....'
      ch22_prts_c_hd_up_du_act = '  ...Upper.Dual.......'
      ch22_prts_c_hd_lb =        '  ...Lower.Bound......'
      ch22_prts_c_hd_co =        '  ........Cost........'
      ch22_prts_c_hd_ub =        '  ....Upper.Bound.....'
      ch22_prts_c_hd_bp =        '  ....Break.Point.....'
      ch22_prts_c_hd_lo_co =     '  .....Lower.Cost.....'
      ch22_prts_c_hd_up_co =     '  .....Upper.Cost.....'
      ch22_prts_c_hd_lo_bp =     '  ..Lower.Breakpoint..'
      ch22_prts_c_hd_up_bp =     '  ..Upper.Breakpoint..'
      ch22_prts_c_hd_og_ob =     '  .Original_Objective.'
      ch22_prts_c_hd_rg =        '  ........Range.......'
      ch22_prts_c_hd_ob_rg =     '  ...Objective_Range..'
 
      ch4_prts_st_ifs =   ' ** '
      ch4_prts_st_bp =    ' BP '
      ch4_prts_st_pwl =   ' PWL'
      ch4_prts_st_bc =    ' BS '
      ch4_prts_st_fr =    ' FR '
      ch4_prts_st_at_lb = ' LB '
      ch4_prts_st_at_ub = ' UB '
      ch4_prts_st_fx =    ' FX '
 
      ch5_vr_in_c_sn_ty_ab_bp = 'Ab Bp'
      ch5_vr_in_c_sn_ty_bw_bp = 'Bw Bp'
      ch5_vr_in_c_sn_ty_bw_lb = 'Bw Lb'
      ch5_vr_in_c_sn_ty_ab_ub = 'Ab Ub'
      ch5_vr_in_c_sn_ty_btw =   'BTW  '
      ch5_vr_in_c_sn_ty_at_lb = 'At Lb'
      ch5_vr_in_c_sn_ty_at_ub = 'At Ub'
      ch5_vr_in_c_sn_ty_te_fx = 'Te Fx'
      ch5_vr_in_c_sn_ty_fx =    'Fx   '
c
c     Indicate that the prts common blocks have been initialised.
c
      iz_prts_com_fg1 = 1
      iz_prts_com_fg2 = 2
      return
      end
