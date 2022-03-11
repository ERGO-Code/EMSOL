CM
C->>> ----------------------------------------------> ems_ca_crsh_ml <<<
c     Call the routines to allocate space for, perform and deallocate
c     space for a symbolic crash.
c
      subroutine ems_ca_crsh_ml(crsh_ty, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      include 'CRASH.INC'
      integer crsh_ty, is(0:is_n_en_m1)
      double precision ds(0:ds_n_en_m1)
c      integer rl_wk_a_ix
c      double precision ftran_rsdu_norm, btran_rsdu_norm
      integer rl_wk_a_ix1
      integer rl_wk_a_ix2
      integer i_wk_a_ix1
      integer i_wk_a_ix2
 
      if (crsh_ty .le. 1) then
         Call ems_ltssf_iz_blk_crsh(is)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
         call ems_iz_crsh_com(crsh_ty)
         tl_crsh_abs_pv_v = usr_tl_crsh_abs_pv_v
         tl_crsh_rlv_pv_v = usr_tl_crsh_rlv_pv_v
         call ems_iz_ltssf_crsh_vr_ty(crsh_ty,
     &        ds(p_lbc),
     &        ds(p_ubc),
     &        is(p_st),
     &        is(p_crsh_r_ty),
     &        is(p_crsh_c_ty))
         call ems_g_rsmi_rl_wk_a_ix(rl_wk_a_ix1)
         if (rl_wk_a_ix1 .lt. 0) goto 8000
         call ems_g_rsmi_rl_wk_a_ix(rl_wk_a_ix2)
         if (rl_wk_a_ix2 .lt. 0) goto 8000
         call ems_g_rsmi_i_wk_a_ix(i_wk_a_ix1)
         if (i_wk_a_ix1 .lt. 0) goto 8000
         call ems_g_rsmi_i_wk_a_ix(i_wk_a_ix2)
         if (i_wk_a_ix2 .lt. 0) goto 8000
         Call ems_ltssf_crsh(
     &        ds(p_lbc),
     &        ds(p_ubc),
     &        is(p_st),
     &        ds(p_pr_act),
     &        is(p_vr_in_r),
     &        is(p_vr_in_c+vr_in_c_n_sn),
     &        ds(p_mtx_r_v),
     &        is(p_mtx_r_ix),
     &        is(p_mtx_c_sa),
     &        is(p_crsh_r_ty),
     &        is(p_crsh_r_pri_lkb),
     &        is(p_crsh_r_pri_lkf),
     &        is(p_crsh_c_ty),
     &        is(p_crsh_r_n_act_en),
     &        is(p_crsh_r_n_act_en_hdr),
     &        is(p_crsh_r_n_act_en_lkb),
     &        is(p_crsh_r_n_act_en_lkf),
     &        is(p_crsh_c_n_act_en),
     &        ds(p_crsh_mtx_c_mx_abs_v),
     &        ds(p_crsh_mtx_c_v),
     &        is(p_crsh_mtx_c_ix),
     &        is(p_crsh_mtx_r_sa),
     &        is(p_crsh_r_st),
     &        is(p_crsh_c_st),
     &        is(p_rsmi_i_wk_a(i_wk_a_ix1)),
     &        is(p_rsmi_i_wk_a(i_wk_a_ix2)),
     &        ds(p_rsmi_rl_wk_a(rl_wk_a_ix1)),
     &        ds(p_rsmi_rl_wk_a(rl_wk_a_ix2)))
         call ems_fr_rsmi_rl_wk_a_ix(rl_wk_a_ix1)
         call ems_fr_rsmi_rl_wk_a_ix(rl_wk_a_ix2)
         call ems_fr_rsmi_i_wk_a_ix(i_wk_a_ix1)
         call ems_fr_rsmi_i_wk_a_ix(i_wk_a_ix2)
c         call ems_rsmi_inv(ds, is)
c         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
c         call ems_g_rsmi_rl_wk_a_ix(rl_wk_a_ix)
c         if (rl_wk_a_ix .lt. 0) goto 8000
c         call ems_g_rand_tran_rsdu_norm(.true.,
c     &        is(p_vr_in_r),
c     &        ds(p_mtx_r_v), is(p_mtx_r_ix), is(p_mtx_c_sa),
c     &        ds(p_pv_c_v),
c     &        ds(p_rsmi_rl_wk_a(rl_wk_a_ix)),
c     &        ftran_rsdu_norm, btran_rsdu_norm, is, ds)
c         call ems_fr_rsmi_rl_wk_a_ix(rl_wk_a_ix)
c         if (max(ftran_rsdu_norm, btran_rsdu_norm) .gt.
c     &        tl_iz_bs_tran_er) then
c            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9510)
c     &           ftran_rsdu_norm, btran_rsdu_norm
c            call ems_msg_wr_li(warn_msg_n)
c         endif
      else
         call ems_ltsf_iz_blk_crsh(is)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
         call ems_iz_crsh_com(crsh_ty)
         call ems_ltsf_crsh(
     &        ds(p_lbc),
     &        ds(p_ubc),
     &        is(p_st),
     &        is(p_vr_in_r),
     &        is(p_vr_in_c+vr_in_c_n_sn),
     &        is(p_mtx_r_ix),
     &        is(p_mtx_c_sa),
     &        is(p_crsh_r_ty),
     &        is(p_crsh_c_ty),
     &        is(p_crsh_r_n_act_en),
     &        is(p_crsh_c_n_act_en),
     &        is(p_crsh_mtx_c_ix),
     &        is(p_crsh_mtx_r_sa),
     &        is(p_crsh_r_st),
     &        is(p_crsh_c_st))
      endif
      call ems_rm_blk_crsh(is)
c
c     Indicate that the following are not correct for the model:
c
c     vr_in_c, INVERT, status/primal activities, basic primal
c     activities, edge weights and row-wise representation of matrix
c     columns being priced.
c
      ml_da_st_msk = ml_da_st_msk
     &     - iand(ml_da_st_msk, ml_da_st_vr_in_c)
     &     - iand(ml_da_st_msk, ml_da_st_inv)
     &     - iand(ml_da_st_msk, ml_da_st_vr_st_fm_act)
     &     - iand(ml_da_st_msk, ml_da_st_bc_pr_act)
     &     - iand(ml_da_st_msk, ml_da_st_ed_wt)
     &     - iand(ml_da_st_msk, ml_da_st_r_mtx)
 7000 continue
c 7100 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
c 9510 format('Residual errors of ',
c     &     g11.4, ' solving A.x = b and ',
c     &     g11.4, ' solving A^x = b',
c     &     ' indicate that the basis is ill-conditioned')
 9800 format('RSMI workspace not available in ems_ca_crsh_ml')
      end
C->>> -----------------------------------------------> ems_ltsf_crsh <<<
c     Perform LTSF crash.
c
      subroutine ems_ltsf_crsh(
     &     lbc,
     &     ubc,
     &     st,
     &     vr_in_r,
     &     vr_in_c,
     &     mtx_r_ix,
     &     mtx_c_sa,
     &     crsh_r_ty,
     &     crsh_c_ty,
     &     crsh_r_n_act_en,
     &     crsh_c_n_act_en,
     &     crsh_mtx_c_ix,
     &     crsh_mtx_r_sa,
     &     crsh_r_st,
     &     crsh_c_st)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'CRASH.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      double precision lbc(0:mx_n_c+n_r)
      double precision ubc(0:mx_n_c+n_r)
      integer st(0:mx_n_c+n_r)
      integer vr_in_r(0:n_r)
      integer vr_in_c(0:n_c)
      integer mtx_r_ix(0:n_a_el)
      integer mtx_c_sa(0:n_c+1)
      integer crsh_r_ty(0:n_r)
      integer crsh_c_ty(0:n_c)
      integer crsh_r_n_act_en(0:n_r)
      integer crsh_c_n_act_en(0:n_c)
      integer crsh_mtx_c_ix(0:n_a_el)
      integer crsh_mtx_r_sa(0:n_r+1)
      integer crsh_r_st(0:n_r)
      integer crsh_c_st(0:n_c)
CM      IF (emsol_dev .EQ. 1) THEN
C?      integer ems_i_t_i_pct
C?      integer pct
CM      ENDIF
      integer r_n, c_n, vr_n, el_n, r_el_n, lg_vr_n
      integer cz_r_n, r_pri_fn_v, mx_r_pri_fn_v
      integer cz_c_n, c_pri_fn_v, mx_c_pri_fn_v
      integer vr_st
      integer n_vr_in_r, n_vr_in_c
      logical ze_fs
      integer n_crsh_bs_cg, crsh_vr_ty
 
      ze_fs = iand(crsh_msk, crsh_msk_ze_fs_bt) .ne. 0
      do 2, crsh_vr_ty = crsh_f_vr_ty, crsh_l_vr_ty
         crsh_vr_ty_og_n_r(crsh_vr_ty) = 0
         crsh_vr_ty_og_n_c(crsh_vr_ty) = 0
         crsh_vr_ty_rm_n_r(crsh_vr_ty) = 0
         crsh_vr_ty_add_n_c(crsh_vr_ty) = 0
 2    continue
      do 10, r_n = 1, n_r
         vr_n = mx_n_c + r_n
         vr_st = st(vr_n)
         if (ubc(vr_n) .ge. inf) then
            if (lbc(vr_n) .le. -inf) then
c     FR:
               crsh_r_ty(r_n) = crsh_vr_ty_fr
            else
c     LB:
               crsh_r_ty(r_n) = crsh_vr_ty_1_sd
               if (ze_fs .and. lbc(vr_n) .le. zero)
     &              crsh_r_ty(r_n) = crsh_vr_ty_1_sd_ze_fs
            endif
         else
            if (lbc(vr_n) .le. -inf) then
c     UB:
               crsh_r_ty(r_n) = crsh_vr_ty_1_sd
               if (ze_fs .and. ubc(vr_n) .ge. zero)
     &              crsh_r_ty(r_n) = crsh_vr_ty_1_sd_ze_fs
            else
               if (lbc(vr_n) .ne. ubc(vr_n)) then
c     LB/UB:
                  crsh_r_ty(r_n) = crsh_vr_ty_2_sd
                  if (ze_fs .and.
     &                 lbc(vr_n) .le. zero .and.
     &                 ubc(vr_n) .ge. zero)
     &                 crsh_r_ty(r_n) = crsh_vr_ty_2_sd_ze_fs
               else
c     FX:
                  crsh_r_ty(r_n) = crsh_vr_ty_fx
                  if (ze_fs .and.
     &                 lbc(vr_n) .le. zero .and.
     &                 ubc(vr_n) .ge. zero)
     &                 crsh_r_ty(r_n) = crsh_vr_ty_fx_ze_fs
               endif
            endif
         endif
         if (crsh_r_ty(r_n) .eq. crsh_vr_ty_fr) then
            crsh_r_st(r_n) = crsh_vr_st_no_act
         else
            crsh_r_st(r_n) = crsh_vr_st_act
         endif
c
c     Ensure that the logical is basic.
c
         st(vr_n) = ior(vr_st, bc_vr_bs_st)
c
c     Initialise the count for later accumulation
c
         crsh_r_n_act_en(r_n) = 0
c
c     Keep a count of the original number of rows of each type
c
         crsh_vr_ty_og_n_r(crsh_r_ty(r_n)) =
     &        crsh_vr_ty_og_n_r(crsh_r_ty(r_n)) + 1
 10   continue
      do 30, c_n = 1, n_c
         vr_n = c_n
         vr_st = st(vr_n)
         if (ubc(vr_n) .ge. inf) then
            if (lbc(vr_n) .le. -inf) then
c     FR:
               crsh_c_ty(c_n) = crsh_vr_ty_fr
            else
c     LB:
               crsh_c_ty(c_n) = crsh_vr_ty_1_sd
               if (ze_fs .and. lbc(vr_n) .le. zero)
     &              crsh_c_ty(c_n) = crsh_vr_ty_1_sd_ze_fs
            endif
         else
            if (lbc(vr_n) .le. -inf) then
c     UB:
               crsh_c_ty(c_n) = crsh_vr_ty_1_sd
               if (ze_fs .and. ubc(vr_n) .ge. zero)
     &              crsh_c_ty(c_n) = crsh_vr_ty_1_sd_ze_fs
            else
               if (lbc(vr_n) .ne. ubc(vr_n)) then
c     LB/UB:
                  crsh_c_ty(c_n) = crsh_vr_ty_2_sd
                  if (ze_fs .and.
     &                 lbc(vr_n) .le. zero .and.
     &                 ubc(vr_n) .ge. zero)
     &                 crsh_c_ty(c_n) = crsh_vr_ty_2_sd_ze_fs
               else
c     FX:
                  crsh_c_ty(c_n) = crsh_vr_ty_fx
                  if (ze_fs .and.
     &                 lbc(vr_n) .le. zero .and.
     &                 ubc(vr_n) .ge. zero)
     &                 crsh_c_ty(c_n) = crsh_vr_ty_fx_ze_fs
               endif
            endif
         endif
         if (crsh_c_ty(c_n) .eq. crsh_vr_ty_fx) then
            crsh_c_st(c_n) = crsh_vr_st_no_act
         else
            crsh_c_st(c_n) = crsh_vr_st_act
         endif
         crsh_c_n_act_en(c_n) = 0
c
c     Ensure that the structural is nonbasic.
c
         st(vr_n) = vr_st - iand(vr_st, su_vr_bs_bt) + non_bc_vr_bs_st
c
c     Keep a count of the original number of columns of each type
c
         crsh_vr_ty_og_n_c(crsh_c_ty(c_n)) =
     &        crsh_vr_ty_og_n_c(crsh_c_ty(c_n)) + 1
 
         if (crsh_c_ty(c_n) .eq. crsh_vr_ty_fx) goto 30
         do 20, el_n = mtx_c_sa(c_n), mtx_c_sa(c_n+1)-1
            r_n = mtx_r_ix(el_n)
            if (crsh_r_ty(r_n) .eq. crsh_vr_ty_fr) goto 20
            crsh_c_n_act_en(c_n) = crsh_c_n_act_en(c_n) + 1
            crsh_r_n_act_en(r_n) = crsh_r_n_act_en(r_n) + 1
 20      continue
         if (crsh_c_n_act_en(c_n) .eq. 0) then
            crsh_c_st(c_n) = crsh_vr_st_no_act
c            write(crsh_ou_cn, 9400)'Col', c_n
         endif
 30   continue
      crsh_mtx_r_sa(1) = 1
      do 40, r_n = 1, n_r
         crsh_mtx_r_sa(r_n+1) =
     &        crsh_mtx_r_sa(r_n) + crsh_r_n_act_en(r_n)
         if (crsh_r_n_act_en(r_n) .eq. 0) then
            crsh_r_st(r_n) = crsh_vr_st_no_act
c            write(crsh_ou_cn, 9400)'Row', r_n
         endif
 40   continue
c
c     Used to get infeasible basis for FIT2P.
c
c      do 60, c_n = n_c, 1, -1
c
      do 60, c_n = 1, n_c
         if (crsh_c_ty(c_n) .eq. crsh_vr_ty_fx) goto 60
         do 50, el_n = mtx_c_sa(c_n), mtx_c_sa(c_n+1)-1
            r_n = mtx_r_ix(el_n)
            crsh_mtx_c_ix(crsh_mtx_r_sa(r_n)) = c_n
            crsh_mtx_r_sa(r_n) = crsh_mtx_r_sa(r_n) + 1
 50      continue
 60   continue
      do 70, r_n = 1, n_r
         crsh_mtx_r_sa(r_n) = crsh_mtx_r_sa(r_n) - crsh_r_n_act_en(r_n)
 70   continue
CM      IF (emsol_dev .EQ. 1) THEN
C?      write(crsh_ou_cn, 9000)
C?      write(crsh_ou_cn, 9010)
C?      call ems_rp_crsh_vr_ty(crsh_ou_cn, st, crsh_r_ty, crsh_c_ty)
CM      ENDIF
      n_crsh_bs_cg = 0
c      write(crsh_ou_cn, 9050)
c      call ems_flush(crsh_ou_cn)
 
 100  continue
 
CM      IF (emsol_dev .EQ. 1) THEN
C?      call ems_rp_crsh_act_mtx(crsh_ou_cn,
C?     &     crsh_r_n_act_en,
C?     &     crsh_c_n_act_en,
C?     &     crsh_r_ty,
C?     &     crsh_c_ty,
C?     &     crsh_r_st,
C?     &     crsh_c_st,
C?     &     crsh_mtx_c_ix, crsh_mtx_r_sa,
C?     &     mtx_r_ix, mtx_c_sa)
CM      ENDIF
      cz_r_n = 0
      mx_r_pri_fn_v = -i_inf
c      write(crsh_ou_cn, 9100)
      do 110, r_n = 1, n_r
         if (crsh_r_st(r_n) .eq. crsh_vr_st_no_act) go to 110
         r_pri_fn_v = crsh_r_ty_pri_v(crsh_r_ty(r_n)) -
     &        crsh_fn_dn*crsh_r_n_act_en(r_n)
         if (r_pri_fn_v .gt. mx_r_pri_fn_v) then
            cz_r_n = r_n
            mx_r_pri_fn_v = r_pri_fn_v
         endif
c         write(crsh_ou_cn, 9110)r_n,
c     &        crsh_r_n_act_en(r_n),
c     &        ch3_crsh_vr_ty(crsh_r_ty(r_n)),
c     &        crsh_r_ty_pri_v(crsh_r_ty(r_n)),
c     &        r_pri_fn_v,
c     &        cz_r_n,
c     &        mx_r_pri_fn_v
 110  continue
      if (cz_r_n .eq. 0) goto 1000
c      write(crsh_ou_cn, 9110)cz_r_n,
c     &        crsh_r_n_act_en(cz_r_n),
c     &        ch3_crsh_vr_ty(crsh_r_ty(cz_r_n)),
c     &        crsh_r_ty_pri_v(crsh_r_ty(cz_r_n)),
c     &        mx_r_pri_fn_v
      cz_c_n = 0
      mx_c_pri_fn_v = -i_inf
c      write(crsh_ou_cn, 9120)
c      if (cz_r_n .gt. 1500) then
         do 120, el_n = crsh_mtx_r_sa(cz_r_n), crsh_mtx_r_sa(cz_r_n+1)-1
            c_n = crsh_mtx_c_ix(el_n)
            if (crsh_c_st(c_n) .eq. crsh_vr_st_no_act) go to 120
            c_pri_fn_v = crsh_c_ty_pri_v(crsh_c_ty(c_n)) -
     &           crsh_fn_dn*crsh_c_n_act_en(c_n)
            if (c_pri_fn_v .gt. mx_c_pri_fn_v) then
               cz_c_n = c_n
               mx_c_pri_fn_v = c_pri_fn_v
            endif
c         write(crsh_ou_cn, 9110)c_n,
c     &        crsh_c_n_act_en(c_n),
c     &        ch3_crsh_vr_ty(crsh_c_ty(c_n)),
c     &        crsh_c_ty_pri_v(crsh_c_ty(c_n)),
c     &        c_pri_fn_v,
c     &        cz_c_n,
c     &        mx_c_pri_fn_v
 120     continue
c      else
c         do 121, el_n =
c     &        crsh_mtx_r_sa(cz_r_n+1)-1,
c     &        crsh_mtx_r_sa(cz_r_n), -1
c            c_n = crsh_mtx_c_ix(el_n)
c            if (crsh_c_st(c_n) .eq. crsh_vr_st_no_act) go to 121
c            c_pri_fn_v = crsh_c_ty_pri_v(crsh_c_ty(c_n)) -
c     &           crsh_fn_dn*crsh_c_n_act_en(c_n)
c            if (c_pri_fn_v .gt. mx_c_pri_fn_v) then
c               cz_c_n = c_n
c               mx_c_pri_fn_v = c_pri_fn_v
c            endif
c 121     continue
c      endif
      if (cz_c_n .eq. 0) goto 8000
c      write(crsh_ou_cn, 9110)cz_c_n,
c     &        crsh_c_n_act_en(cz_c_n),
c     &        ch3_crsh_vr_ty(crsh_c_ty(cz_c_n)),
c     &        crsh_c_ty_pri_v(crsh_c_ty(cz_c_n)),
c     &        mx_c_pri_fn_v
c      write(crsh_ou_cn, 9060)cz_r_n,
c     &     crsh_r_n_act_en(cz_r_n),
c     &     ch3_crsh_vr_ty(crsh_r_ty(cz_r_n)),
c     &     crsh_r_ty_pri_v(crsh_r_ty(cz_r_n)),
c     &     mx_r_pri_fn_v,
c     &     cz_c_n,
c     &     crsh_c_n_act_en(cz_c_n),
c     &     ch3_crsh_vr_ty(crsh_c_ty(cz_c_n)),
c     &     crsh_c_ty_pri_v(crsh_c_ty(cz_c_n)),
c     &     mx_c_pri_fn_v
c      call ems_flush(crsh_ou_cn)
c
c     Make the chosen logical nonbasic and the chosen structural basic.
c
      n_crsh_bs_cg = n_crsh_bs_cg + 1
 
      crsh_vr_ty = crsh_r_ty(cz_r_n)
      crsh_vr_ty_rm_n_r(crsh_vr_ty) =
     &     crsh_vr_ty_rm_n_r(crsh_vr_ty) + 1
      crsh_vr_ty = crsh_c_ty(cz_c_n)
      crsh_vr_ty_add_n_c(crsh_vr_ty) =
     &     crsh_vr_ty_add_n_c(crsh_vr_ty) + 1
 
      st(cz_c_n) = st(cz_c_n) + bc_bt
      lg_vr_n = mx_n_c + cz_r_n
      st(lg_vr_n) = st(lg_vr_n) - bc_bt
c
c     Update the row and column counts
c
      do 140, r_el_n = crsh_mtx_r_sa(cz_r_n), crsh_mtx_r_sa(cz_r_n+1)-1
         c_n = crsh_mtx_c_ix(r_el_n)
         if (crsh_c_st(c_n) .eq. crsh_vr_st_no_act) go to 140
         do 130, el_n = mtx_c_sa(c_n), mtx_c_sa(c_n+1)-1
            r_n = mtx_r_ix(el_n)
c            if (crsh_r_st(r_n) .eq. crsh_vr_st_no_act) go to 130
            crsh_r_n_act_en(r_n) = crsh_r_n_act_en(r_n) - 1
            if (crsh_r_n_act_en(r_n) .eq. 0)
     &           crsh_r_st(r_n) = crsh_vr_st_no_act
 130     continue
         crsh_c_st(c_n) = crsh_vr_st_no_act
 140  continue
      if (crsh_r_st(cz_r_n) .ne. crsh_vr_st_no_act) goto 8010
      if (crsh_c_st(cz_c_n) .ne. crsh_vr_st_no_act) goto 8020
      go to 100
 1000 continue
CM      IF (emsol_dev .EQ. 1) THEN
C?      if (n_crsh_bs_cg .eq. 0) then
C?         write(crsh_ou_cn, 9200)
C?      else
C?         write(crsh_ou_cn, 9210)n_crsh_bs_cg
C?         do 1010, crsh_vr_ty = crsh_f_vr_ty, crsh_l_vr_ty
C?            if (crsh_vr_ty_og_n_r(crsh_vr_ty) .gt. 0) then
C?               pct = ems_i_t_i_pct(crsh_vr_ty_rm_n_r(crsh_vr_ty),
C?     &              crsh_vr_ty_og_n_r(crsh_vr_ty))
C?               write(crsh_ou_cn, 9220)' Removed ',
C?     &              crsh_vr_ty_rm_n_r(crsh_vr_ty),
C?     &              crsh_vr_ty_og_n_r(crsh_vr_ty),
C?     &              pct, 'rows   ', ch3_crsh_vr_ty(crsh_vr_ty)
C?            end if
C? 1010    continue
C?         do 1020, crsh_vr_ty = crsh_f_vr_ty, crsh_l_vr_ty
C?            if (crsh_vr_ty_og_n_c(crsh_vr_ty) .gt. 0) then
C?               pct = ems_i_t_i_pct(crsh_vr_ty_add_n_c(crsh_vr_ty),
C?     &              crsh_vr_ty_og_n_c(crsh_vr_ty))
C?               write(crsh_ou_cn, 9220)' Added ',
C?     &              crsh_vr_ty_add_n_c(crsh_vr_ty),
C?     &              crsh_vr_ty_og_n_c(crsh_vr_ty),
C?     &              pct, 'columns', ch3_crsh_vr_ty(crsh_vr_ty)
C?            end if
C? 1020    continue
C?         write(crsh_ou_cn, 9230)
C?         call ems_rp_crsh_vr_ty(
C?     &        crsh_ou_cn, st, crsh_r_ty, crsh_c_ty)
C?      end if
C?      call ems_flush(crsh_ou_cn)
CM      ENDIF
c
c     Get lists of basic and nonbasic variables from the status
c
      n_vr_in_r = 0
      n_vr_in_c = 0
      do 1110, vr_n = 1, n_c
         if (iand(st(vr_n), bc_bt) .ne. 0) then
            n_vr_in_r = n_vr_in_r + 1
            vr_in_r(n_vr_in_r) = vr_n
         else
            n_vr_in_c = n_vr_in_c + 1
            vr_in_c(n_vr_in_c) = vr_n
         endif
 1110 continue
      do 1120, vr_n = mx_n_c+1, mx_n_c+n_r
         if (iand(st(vr_n), bc_bt) .ne. 0) then
            n_vr_in_r = n_vr_in_r + 1
            vr_in_r(n_vr_in_r) = vr_n
         else
            n_vr_in_c = n_vr_in_c + 1
            vr_in_c(n_vr_in_c) = vr_n
         endif
 1120 continue
      if (n_vr_in_r .ne. n_r) goto 8030
      if (n_vr_in_c .ne. n_c) goto 8040
 7000 continue
CM      IF (emsol_dev .EQ. 1) THEN
C?      call ems_flush(crsh_ou_cn)
CM      ENDIF
c
c     Indicate that the condition of the basis may be questionable.
c
      ml_da_st_msk =
     &     ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_bs_cond_ok)
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)
     &     crsh_r_st(cz_r_n)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)
     &     crsh_c_st(cz_c_n)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 8030 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9803)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 8040 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9804)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
CM      IF (emsol_dev .EQ. 1) THEN
C? 9000 format(//'LTSF Crash')
C? 9010 format(//'Before crash:')
CM      ENDIF
c 9050 format(
c     &     '    Row Count   Ty  Pri   Fn_v | ',
c     &     '    Col Count   Ty  Pri   Fn_v')
c 9060 format(
c     &     i7, 2x, i4, 2x, a3, 2x, i3, 2x, i5, ' | ',
c     &     i7, 2x, i4, 2x, a3, 2x, i3, 2x, i5)
c 9100 format('    Row Count   Ty  Pri   Fn_v      Cz_r  Mx_Fn_v')
c 9110 format(i7, 2x, i4, 2x, a3, 2x, i3, 2x, i5, 3x, i7, 4x, i5)
c 9120 format('    Col Count   Ty  Pri   Fn_v      Cz_c  Mx_Fn_v')
CM      IF (emsol_dev .EQ. 1) THEN
C? 9200 format(//'Crash made no basis changes')
C? 9210 format(//'LTSF Crash made ', i7, ' basis changes')
C? 9220 format(a9, i7, ' of ', i7,
C?     &     ' (', i3, '%) ', a7, ' of type ', a3)
C? 9230 format(//'After crash:')
CM      ENDIF
 9800 format('STRANGE: cz_c_n = 0')
 9801 format('STRANGE: crsh_r_st(cz_r_n) = ', i7)
 9802 format('STRANGE: crsh_c_st(cz_c_n) = ', i7)
 9803 format('CRASH: n_vr_in_r .ne. n_r')
 9804 format('CRASH: n_vr_in_c .ne. n_c')
      end
 
C->>> -------------------------------------> ems_iz_ltssf_crsh_vr_ty <<<
c     Initialise row and column types according to crash type
c
      subroutine ems_iz_ltssf_crsh_vr_ty(crsh_ty,
     &     lbc,
     &     ubc,
     &     st,
     &     crsh_r_ty,
     &     crsh_c_ty)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'CRASH.INC'
      integer crsh_ty
      double precision lbc(0:mx_n_c+n_r)
      double precision ubc(0:mx_n_c+n_r)
      integer st(0:mx_n_c+n_r)
      integer crsh_r_ty(0:n_r)
      integer crsh_c_ty(0:n_c)
      integer r_n, c_n, vr_n, crsh_vr_ty
 
      if (crsh_ty .eq. 0) then
         do 10, r_n = 1, n_r
            vr_n = mx_n_c + r_n
            if (iand(st(vr_n), bc_bt) .ne. 0) then
               crsh_vr_ty = crsh_vr_ty_bc
            else
               crsh_vr_ty = crsh_vr_ty_non_bc
            endif
            crsh_r_ty(r_n) = crsh_vr_ty
 10      continue
         do 20, c_n = 1, n_c
            vr_n = c_n
            if (iand(st(vr_n), bc_bt) .ne. 0) then
               crsh_vr_ty = crsh_vr_ty_bc
            else
               crsh_vr_ty = crsh_vr_ty_non_bc
            endif
            crsh_c_ty(c_n) = crsh_vr_ty
 20      continue
      else
         do 110, r_n = 1, n_r
            vr_n = mx_n_c + r_n
            if (ubc(vr_n) .ge. inf) then
               if (lbc(vr_n) .le. -inf) then
c     FR:
                  crsh_vr_ty = crsh_vr_ty_fr
               else
c     LB:
                  crsh_vr_ty = crsh_vr_ty_1_sd
               end if
            else
               if (lbc(vr_n) .le. -inf) then
c     UB:
                  crsh_vr_ty = crsh_vr_ty_1_sd
               else
                  if (lbc(vr_n) .lt. ubc(vr_n)) then
c     LB/UB:
                     crsh_vr_ty = crsh_vr_ty_2_sd
                  else
c     FX:
                     crsh_vr_ty = crsh_vr_ty_fx
                  end if
               end if
            end if
            crsh_r_ty(r_n) = crsh_vr_ty
 110     continue
         do 120, c_n = 1, n_c
            vr_n = c_n
            if (ubc(vr_n) .ge. inf) then
               if (lbc(vr_n) .le. -inf) then
c     FR:
                  crsh_vr_ty = crsh_vr_ty_fr
               else
c     LB:
                  crsh_vr_ty = crsh_vr_ty_1_sd
               end if
            else
               if (lbc(vr_n) .le. -inf) then
c     UB:
                  crsh_vr_ty = crsh_vr_ty_1_sd
               else
                  if (lbc(vr_n) .ne. ubc(vr_n)) then
c     LB/UB:
                     crsh_vr_ty = crsh_vr_ty_2_sd
                  else
c     FX:
                     crsh_vr_ty = crsh_vr_ty_fx
                  end if
               end if
            end if
            crsh_c_ty(c_n) = crsh_vr_ty
 120     continue
      endif
      return
      end
 
C->>> ---------------------------------------------> ems_iz_crsh_com <<<
      subroutine ems_iz_crsh_com(crsh_ty)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'CRASH.INC'
      integer crsh_ty
      integer crsh_vr_ty
 
      crsh_ou_cn = 6
c
c     Initialise the array entries so that something is assigned for all
c     of them
c
c      do crsh_vr_ty = crsh_f_vr_ty, crsh_l_vr_ty
 
      if (crsh_ty .eq. 0) then
c
c     Basis-preserving LTSSF crash
c
         ch3_crsh_vr_ty(crsh_vr_ty_bc) =      ' Bc'
         ch3_crsh_vr_ty(crsh_vr_ty_non_bc) =  'NBc'
 
         crsh_r_ty_pri_v(crsh_vr_ty_non_bc) = 6
         crsh_r_ty_pri_v(crsh_vr_ty_bc) =     0
 
         crsh_c_ty_pri_v(crsh_vr_ty_non_bc) = 0
         crsh_c_ty_pri_v(crsh_vr_ty_bc) =     6
 
      else if (crsh_ty .eq. 1) then
c
c     Standard LTSSF crash
c
         ch3_crsh_vr_ty(crsh_vr_ty_fx) =         ' Fx'
         ch3_crsh_vr_ty(crsh_vr_ty_2_sd) =       ' 2s'
         ch3_crsh_vr_ty(crsh_vr_ty_1_sd) =       ' 1s'
         ch3_crsh_vr_ty(crsh_vr_ty_fr) =         ' Fr'
 
         crsh_r_ty_pri_v(crsh_vr_ty_fx) =         6
         crsh_r_ty_pri_v(crsh_vr_ty_2_sd) =       4
         crsh_r_ty_pri_v(crsh_vr_ty_1_sd) =       2
         crsh_r_ty_pri_v(crsh_vr_ty_fr) =         0
 
         crsh_c_ty_pri_v(crsh_vr_ty_fx) =         0
         crsh_c_ty_pri_v(crsh_vr_ty_2_sd) =       2
         crsh_c_ty_pri_v(crsh_vr_ty_1_sd) =       4
         crsh_c_ty_pri_v(crsh_vr_ty_fr) =         6
 
      else if (crsh_ty .eq. 2) then
c
c     LTSF crash
c
         ch3_crsh_vr_ty(crsh_vr_ty_fx) =         ' Fx'
         ch3_crsh_vr_ty(crsh_vr_ty_fx_ze_fs) =   '0Fx'
         ch3_crsh_vr_ty(crsh_vr_ty_2_sd) =       ' 2s'
         ch3_crsh_vr_ty(crsh_vr_ty_2_sd_ze_fs) = '02s'
         ch3_crsh_vr_ty(crsh_vr_ty_1_sd) =       ' 1s'
         ch3_crsh_vr_ty(crsh_vr_ty_1_sd_ze_fs) = '01s'
         ch3_crsh_vr_ty(crsh_vr_ty_fr) =         ' Fr'
 
         crsh_r_ty_pri_v(crsh_vr_ty_fx) =         6
         crsh_r_ty_pri_v(crsh_vr_ty_fx_ze_fs) =   6
         crsh_r_ty_pri_v(crsh_vr_ty_2_sd) =       4
         crsh_r_ty_pri_v(crsh_vr_ty_2_sd_ze_fs) = 4
         crsh_r_ty_pri_v(crsh_vr_ty_1_sd) =       2
         crsh_r_ty_pri_v(crsh_vr_ty_1_sd_ze_fs) = 2
         crsh_r_ty_pri_v(crsh_vr_ty_fr) =         0
 
         crsh_c_ty_pri_v(crsh_vr_ty_fx) =         0
         crsh_c_ty_pri_v(crsh_vr_ty_fx_ze_fs) =   0
         crsh_c_ty_pri_v(crsh_vr_ty_2_sd) =       2
         crsh_c_ty_pri_v(crsh_vr_ty_2_sd_ze_fs) = 2
         crsh_c_ty_pri_v(crsh_vr_ty_1_sd) =       4
         crsh_c_ty_pri_v(crsh_vr_ty_1_sd_ze_fs) = 4
         crsh_c_ty_pri_v(crsh_vr_ty_fr) =         6
      endif
c
c     Cross-check that the priority values are within the range of
c     header indices
c
      do crsh_vr_ty = crsh_f_vr_ty, crsh_l_vr_ty
         if (
     &        crsh_c_ty_pri_v(crsh_vr_ty) .lt. crsh_mn_pri_v .or.
     &        crsh_c_ty_pri_v(crsh_vr_ty) .gt. crsh_mx_pri_v) then
c            write(*, *)'Crash column type ', crsh_vr_ty,
c     &           '(', ch3_crsh_vr_ty(crsh_vr_ty), ')',
c     &           ' has priority value ', crsh_c_ty_pri_v(crsh_vr_ty),
c     &           ' which is not in valid range from ',
c     &           crsh_mn_pri_v, ' to ', crsh_mx_pri_v
         end if
         if (
     &        crsh_r_ty_pri_v(crsh_vr_ty) .lt. crsh_mn_pri_v .or.
     &        crsh_r_ty_pri_v(crsh_vr_ty) .gt. crsh_mx_pri_v) then
c            write(*, *)'Crash row type ', crsh_vr_ty,
c     &           '(', ch3_crsh_vr_ty(crsh_vr_ty), ')',
c     &           ' has priority value ', crsh_r_ty_pri_v(crsh_vr_ty),
c     &           ' which is not in valid range from ',
c     &           crsh_mn_pri_v, ' to ', crsh_mx_pri_v
         end if
      end do
      return
      end
 
C->>> ----------------------------------------------> ems_ltssf_crsh <<<
c     Perform LTSSF crash.
c
      subroutine ems_ltssf_crsh(
     &     lbc,
     &     ubc,
     &     st,
     &     pr_act,
     &     vr_in_r,
     &     vr_in_c,
     &     mtx_r_v,
     &     mtx_r_ix,
     &     mtx_c_sa,
     &     crsh_r_ty,
     &     crsh_r_pri_lkb,
     &     crsh_r_pri_lkf,
     &     crsh_c_ty,
     &     crsh_r_n_act_en,
     &     crsh_r_n_act_en_hdr,
     &     crsh_r_n_act_en_lkb,
     &     crsh_r_n_act_en_lkf,
     &     crsh_c_n_act_en,
     &     crsh_mtx_c_mx_abs_v,
     &     crsh_mtx_c_v,
     &     crsh_mtx_c_ix,
     &     crsh_mtx_r_sa,
     &     crsh_r_st,
     &     crsh_c_st,
     &     pv_r_ls,
     &     pv_a_el,
     &     rhs, sol)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'CRASH.INC'
      include 'EMSMSG.INC'
      double precision lbc(0:mx_n_c+n_r)
      double precision ubc(0:mx_n_c+n_r)
      integer st(0:mx_n_c+n_r)
      double precision pr_act(0:mx_n_c+n_r)
      integer vr_in_r(0:n_r)
      integer vr_in_c(0:n_c)
      double precision mtx_r_v(0:n_a_el)
      integer mtx_r_ix(0:n_a_el)
      integer mtx_c_sa(0:n_c+1)
      integer crsh_r_ty(0:n_r)
      integer crsh_r_pri_lkb(0:n_r)
      integer crsh_r_pri_lkf(0:n_r)
      integer crsh_c_ty(0:n_c)
      integer crsh_r_n_act_en(0:n_r)
      integer crsh_r_n_act_en_hdr(0:n_c)
      integer crsh_r_n_act_en_lkb(0:n_r)
      integer crsh_r_n_act_en_lkf(0:n_r)
      integer crsh_c_n_act_en(0:n_c)
      double precision crsh_mtx_c_mx_abs_v(0:n_c)
      double precision crsh_mtx_c_v(0:n_a_el)
      integer crsh_mtx_c_ix(0:n_a_el)
      integer crsh_mtx_r_sa(0:n_r+1)
      integer crsh_r_st(0:n_r)
      integer crsh_c_st(0:n_c)
      integer pv_r_ls(0:n_r)
      integer pv_a_el(0:n_r)
      double precision rhs(0:n_r)
      double precision sol(0:n_r)
      integer r_n, c_n, vr_n, el_n, r_el_n, lg_vr_n, struc_vr_n
      integer cz_r_n, r_pri_fn_v, mx_r_pri_fn_v, lm_r_pri_fn_v
      integer cz_c_n, c_pri_fn_v, mx_c_pri_fn_v
c      integer ck_lvl
      integer crsh_it_n, n_crsh_bs_cg
      integer crsh_vr_ty
      integer crsh_pri_v, r_pri_v
      integer crsh_n_act_en, r_n_act_en
      integer mx_r_pri_v
      integer prev_r_n, nx_r_n
      integer n_vr_in_r, n_vr_in_c
      integer crsh_ps_mod_2
      double precision pv_v
      double precision abs_c_v, abs_pv_v
      double precision rlv_pv_v
      double precision mn_abs_pv_v
      double precision mn_rlv_pv_v
      double precision nw_tl_crsh_abs_pv_v
      double precision nw_tl_crsh_rlv_pv_v
      double precision ftran_rsdu_norm, btran_rsdu_norm
      integer n_abs_pv_no_ok
      integer n_rlv_pv_no_ok
      logical abs_pv_v_ok, rlv_pv_v_ok, pv_ok
CM      IF (emsol_dev .EQ. 1) THEN
C?      integer ems_i_t_i_pct
C?      integer pct
C?      logical cz_r_rp_lvl_2_li
CM      ENDIF
c      logical er_fd
      integer crsh_fn_cf_pri_v
      integer crsh_fn_cf_n_act_en
      parameter (
     &     crsh_fn_cf_pri_v = 1,
     &     crsh_fn_cf_n_act_en = 10)
 
      n_crsh_ps = 1
 10   continue
      call ems_ltssf_iz_da_str(
     &     lbc, ubc, st,
     &     mtx_r_v, mtx_r_ix, mtx_c_sa,
     &     crsh_r_ty, crsh_r_pri_lkb, crsh_r_pri_lkf,
     &     crsh_c_ty,
     &     crsh_r_n_act_en, crsh_r_n_act_en_hdr,
     &     crsh_r_n_act_en_lkb, crsh_r_n_act_en_lkf,
     &     crsh_c_n_act_en,
     &     crsh_mtx_c_mx_abs_v,
     &     crsh_mtx_c_v, crsh_mtx_c_ix, crsh_mtx_r_sa,
     &     crsh_r_st, crsh_c_st)
 
CM      IF (emsol_dev .EQ. 1) THEN
C?      write(crsh_ou_cn, 9000)n_crsh_ps,
C?     &     tl_crsh_abs_pv_v, tl_crsh_rlv_pv_v
C?      if (n_crsh_ps .eq. 1) then
C?         write(crsh_ou_cn, 9010)
C?         call ems_rp_crsh_vr_ty(
C?     &        crsh_ou_cn, st, crsh_r_ty, crsh_c_ty)
C?         cz_r_rp_lvl_2_li = .false.
C?         call ems_flush(crsh_ou_cn)
C?      endif
CM      ENDIF
      crsh_it_n = 0
      n_crsh_bs_cg = 0
      n_vr_in_r = 0
      mn_abs_pv_v = inf
      mn_rlv_pv_v = inf
      n_abs_pv_no_ok = 0
      n_rlv_pv_no_ok = 0
c
c     Get the maximum row priority value
c
      mx_r_pri_v = -i_inf
      do crsh_pri_v = crsh_mn_pri_v, crsh_mx_pri_v
         if (crsh_r_pri_v_hdr(crsh_pri_v) .gt. 0)
     &        mx_r_pri_v = crsh_pri_v
      end do
      if (mx_r_pri_v .eq. -i_inf) go to 1000
 100  continue
CM      IF (emsol_dev .EQ. 1) THEN
C?      if (mod(crsh_it_n, 1000) .eq. 0) then
C?         ck_lvl = 1
C?         call ems_ltssf_ck_da_str(er_fd, ck_lvl,
C?     &        st,
C?     &        mtx_r_v, mtx_r_ix, mtx_c_sa,
C?     &        crsh_r_ty, crsh_r_pri_lkb, crsh_r_pri_lkf,
C?     &        crsh_c_ty,
C?     &        crsh_r_n_act_en, crsh_r_n_act_en_hdr,
C?     &        crsh_r_n_act_en_lkb, crsh_r_n_act_en_lkf,
C?     &        crsh_c_n_act_en,
C?     &        crsh_mtx_c_mx_abs_v,
C?     &        crsh_mtx_c_v, crsh_mtx_c_ix, crsh_mtx_r_sa,
C?     &        crsh_r_st, crsh_c_st)
C?         if (er_fd) go to 1000
C?      end if
C?      call ems_rp_crsh_act_mtx(crsh_ou_cn,
C?     &     crsh_r_n_act_en,
C?     &     crsh_c_n_act_en,
C?     &     crsh_r_ty,
C?     &     crsh_c_ty,
C?     &     crsh_r_st,
C?     &     crsh_c_st,
C?     &     crsh_mtx_c_ix, crsh_mtx_r_sa,
C?     &     mtx_r_ix, mtx_c_sa)
CM      ENDIF
      crsh_it_n = crsh_it_n + 1
c
c     Choose a row with maximium priority function
c
c      cz_r_n = 0
c      mx_r_pri_fn_v = -i_inf
c
c     According to the relative size of crsh_fn_cf_pri_v and
c     crsh_fn_cf_n_act_en, search over decreasing row priority or
c     increasing row count
c
c      do 101, r_n = 1, n_r
c         if (crsh_r_st(r_n) .eq. crsh_vr_st_no_act) go to 101
c         crsh_pri_v = crsh_r_ty_pri_v(crsh_r_ty(r_n))
c         crsh_n_act_en = crsh_r_n_act_en(r_n)
c         r_pri_fn_v =
c     &        crsh_fn_cf_pri_v*crsh_pri_v -
c     &        crsh_fn_cf_n_act_en*crsh_n_act_en
c         if (r_pri_fn_v .gt. mx_r_pri_fn_v) then
c            cz_r_n = r_n
c            mx_r_pri_fn_v = r_pri_fn_v
c         end if
c 101  continue
c      sv_cz_r_n = cz_r_n
c      sv_mx_r_pri_fn_v = mx_r_pri_fn_v
c
c     Choose a row with maximium priority function
c
      cz_r_n = 0
      mx_r_pri_fn_v = -i_inf
      if (crsh_fn_cf_pri_v .gt. crsh_fn_cf_n_act_en) then
         do 110, crsh_pri_v = crsh_mx_pri_v, crsh_mn_pri_v, -1
            r_n = crsh_r_pri_v_hdr(crsh_pri_v)
            if (r_n .le. 0) go to 110
c
c     Note the limiting priority function value so that the search can
c     be stopped if it is attained---corresponds to a row of current
c     priority value and count 1
c
            lm_r_pri_fn_v =
     &           crsh_fn_cf_pri_v*crsh_pri_v -
     &           crsh_fn_cf_n_act_en
 105        continue
            crsh_n_act_en = crsh_r_n_act_en(r_n)
            r_pri_fn_v =
     &           crsh_fn_cf_pri_v*crsh_pri_v -
     &           crsh_fn_cf_n_act_en*crsh_n_act_en
            if (r_pri_fn_v .gt. mx_r_pri_fn_v) then
               cz_r_n = r_n
               mx_r_pri_fn_v = r_pri_fn_v
               if (mx_r_pri_fn_v .eq. lm_r_pri_fn_v) go to 120
            end if
            nx_r_n = crsh_r_pri_lkf(r_n)
            if (nx_r_n .gt. 0) then
               r_n = nx_r_n
               go to 105
            end if
            if (cz_r_n .gt. 0) go to 120
 110     continue
 120     continue
         if (cz_r_n .eq. 0) go to 1000
         r_pri_v = crsh_r_ty_pri_v(crsh_r_ty(cz_r_n))
         if (r_pri_v .ne. crsh_pri_v) then
CM      IF (emsol_dev .EQ. 1) THEN
C?            write(*, *)'ERROR: chosen row has true priority ',
C?     &           r_pri_v, ' not ', crsh_pri_v
CM      ENDIF
            go to 1000
         end if
      else
         do 130, crsh_n_act_en = 1, n_c
            r_n = crsh_r_n_act_en_hdr(crsh_n_act_en)
            if (r_n .le. 0) go to 130
c
c     Note the limiting priority function value so that the search can
c     be stopped if it is attained---corresponds to a maximum priority
c
            lm_r_pri_fn_v =
     &           crsh_fn_cf_pri_v*mx_r_pri_v -
     &           crsh_fn_cf_n_act_en*crsh_n_act_en
 125        continue
            crsh_pri_v = crsh_r_ty_pri_v(crsh_r_ty(r_n))
            r_pri_fn_v =
     &           crsh_fn_cf_pri_v*crsh_pri_v -
     &           crsh_fn_cf_n_act_en*crsh_n_act_en
            if (r_pri_fn_v .gt. mx_r_pri_fn_v) then
               cz_r_n = r_n
               mx_r_pri_fn_v = r_pri_fn_v
               if (mx_r_pri_fn_v .eq. lm_r_pri_fn_v) go to 145
            end if
            nx_r_n = crsh_r_n_act_en_lkf(r_n)
            if (nx_r_n .gt. 0) then
               r_n = nx_r_n
               go to 125
            end if
            if (cz_r_n .gt. 0) go to 145
 130     continue
 145     continue
         if (cz_r_n .eq. 0) go to 1000
         r_n_act_en = crsh_r_n_act_en(cz_r_n)
         if (r_n_act_en .ne. crsh_n_act_en) then
CM      IF (emsol_dev .EQ. 1) THEN
C?            write(*, *)'ERROR: chosen row has true priority ',
C?     &           r_n_act_en, ' not ', crsh_n_act_en
CM      ENDIF
            go to 1000
         end if
      end if
c      if (mx_r_pri_fn_v .ne. sv_mx_r_pri_fn_v) then
c         write(*, 9999)'Row     ', cz_r_n, sv_cz_r_n
c         write(*, 9999)'Function',
c     &        mx_r_pri_fn_v,
c     &        sv_mx_r_pri_fn_v
c         write(*, 9999)'Priority',
c     &        crsh_r_ty_pri_v(crsh_r_ty(cz_r_n)),
c     &        crsh_r_ty_pri_v(crsh_r_ty(sv_cz_r_n))
c         write(*, 9999)'Count   ',
c     &        crsh_r_n_act_en(cz_r_n),
c     &        crsh_r_n_act_en(sv_cz_r_n)
c 9999    format(a8, 2(2x, i7))
c      end if
c
c     Choose a column which has maximium priority function amongst those
c     with entries in the selected row---making sure that the pivot is
c     acceptable numerically.
c
c     Note
c
c     *  Numerical checking is switched off by setting the tolerances to
c     .  zero.
c
c     *  Make sure that tl_crsh_rlv_pv_v < 1 otherwise test
c     .  abs_c_v/crsh_mtx_c_mx_abs_v(c_n) .gt. tl_crsh_rlv_pv_v
c     .  will never be satisfied
c
      cz_c_n = 0
      mx_c_pri_fn_v = -i_inf
      do 150, el_n = crsh_mtx_r_sa(cz_r_n), crsh_mtx_r_sa(cz_r_n+1)-1
         c_n = crsh_mtx_c_ix(el_n)
         if (crsh_c_st(c_n) .eq. crsh_vr_st_no_act) go to 150
c
c     Don't allow the row to be replaced by a column whose priority to
c     remain nonbasic is the same or greater.
c
         if (crsh_c_ty_pri_v(crsh_c_ty(c_n))+crsh_pri_v .le.
     &        crsh_mx_pri_v) go to 150
         c_pri_fn_v =
     &        crsh_fn_cf_pri_v*crsh_c_ty_pri_v(crsh_c_ty(c_n)) -
     &        crsh_fn_cf_n_act_en*crsh_c_n_act_en(c_n)
         if (c_pri_fn_v .gt. mx_c_pri_fn_v) then
            abs_c_v = abs(crsh_mtx_c_v(el_n))
            abs_pv_v_ok =
     &           abs_c_v .gt. tl_crsh_abs_pv_v
            rlv_pv_v_ok =
     &           abs_c_v .gt. tl_crsh_rlv_pv_v*crsh_mtx_c_mx_abs_v(c_n)
            if (.not. abs_pv_v_ok) n_abs_pv_no_ok = n_abs_pv_no_ok + 1
            if (.not. rlv_pv_v_ok) n_rlv_pv_no_ok = n_rlv_pv_no_ok + 1
            pv_ok = abs_pv_v_ok .and. rlv_pv_v_ok
            if (pv_ok) then
               cz_c_n = c_n
               mx_c_pri_fn_v = c_pri_fn_v
               pv_v = crsh_mtx_c_v(el_n)
            end if
         end if
 150  continue
      if (cz_c_n .gt. 0) then
c
c     Check the pivot numerically
c
         abs_pv_v = abs(pv_v)
         rlv_pv_v = abs_pv_v/crsh_mtx_c_mx_abs_v(cz_c_n)
         abs_pv_v_ok = abs_pv_v .gt. tl_crsh_abs_pv_v
         rlv_pv_v_ok = rlv_pv_v .gt. tl_crsh_rlv_pv_v
         pv_ok = abs_pv_v_ok .and. rlv_pv_v_ok
         if (pv_ok) then
c
c     Make the chosen logical nonbasic and the chosen structural basic.
c
            n_crsh_bs_cg = n_crsh_bs_cg + 1
 
            struc_vr_n = cz_c_n
            st(struc_vr_n) = st(struc_vr_n) + bc_bt
c
c     Make sure the structurals come first in vr_in_r and store the
c     corresponding pivotal row number
c
            n_vr_in_r = n_vr_in_r + 1
            vr_in_r(n_vr_in_r) = struc_vr_n
            pv_r_ls(n_vr_in_r) = cz_r_n
 
            lg_vr_n = mx_n_c + cz_r_n
            st(lg_vr_n) = st(lg_vr_n) - bc_bt
 
            mn_abs_pv_v = min(abs_pv_v, mn_abs_pv_v)
            mn_rlv_pv_v = min(rlv_pv_v, mn_rlv_pv_v)
            crsh_vr_ty = crsh_r_ty(cz_r_n)
            crsh_vr_ty_rm_n_r(crsh_vr_ty) =
     &           crsh_vr_ty_rm_n_r(crsh_vr_ty) + 1
            crsh_vr_ty = crsh_c_ty(cz_c_n)
            crsh_vr_ty_add_n_c(crsh_vr_ty) =
     &           crsh_vr_ty_add_n_c(crsh_vr_ty) + 1
         else
CM      IF (emsol_dev .EQ. 1) THEN
C?            write(*, *)
C?     &           'STRANGE: Pivot checked numerically in row pass'
CM      ENDIF
         end if
c         if (.not. cz_r_rp_lvl_2_li) then
c            write(crsh_ou_cn, 9050)
c            cz_r_rp_lvl_2_li = .true.
c         end if
c         write(crsh_ou_cn, 9060)crsh_it_n,
c     &        cz_r_n, crsh_r_n_act_en(cz_r_n),
c     &        ch3_crsh_vr_ty(crsh_r_ty(cz_r_n)),
c     &        crsh_r_ty_pri_v(crsh_r_ty(cz_r_n)),
c     &        mx_r_pri_fn_v,
c     &        cz_c_n, crsh_c_n_act_en(cz_c_n),
c     &        ch3_crsh_vr_ty(crsh_c_ty(cz_c_n)),
c     &        crsh_c_ty_pri_v(crsh_c_ty(cz_c_n)),
c     &        mx_c_pri_fn_v,
c     &        abs_pv_v, abs_pv_v_ok, rlv_pv_v, rlv_pv_v_ok
c         call ems_flush(crsh_ou_cn)
      else
         pv_ok = .false.
      end if
      if (pv_ok) then
c
c     The basis has changed. The row becomes inactive, as does each
c     column in the chosen row (so that the basis remains triangular).
c     For each row with entries in these columns, the number of active
c     entries is reduced correspondingly. If the number of active
c     entries in a row is zeroed as a result, the row becomes inactive.
c     NB This must happen (at least) for the chosen row.
c
         do 220, r_el_n = crsh_mtx_r_sa(cz_r_n),
     &        crsh_mtx_r_sa(cz_r_n+1)-1
            c_n = crsh_mtx_c_ix(r_el_n)
            if (crsh_c_st(c_n) .eq. crsh_vr_st_no_act) go to 220
            do 210, el_n = mtx_c_sa(c_n), mtx_c_sa(c_n+1)-1
               r_n = mtx_r_ix(el_n)
               if (crsh_r_st(r_n) .eq. crsh_vr_st_no_act) go to 210
c
c     Record the element number of the pivot
c
               if (c_n .eq. cz_c_n .and. r_n .eq. cz_r_n)
     &              pv_a_el(n_vr_in_r) = el_n
c
c     Remove the row from the linked list with this number of active
c     entries
c
               r_n_act_en = crsh_r_n_act_en(r_n)
               nx_r_n = crsh_r_n_act_en_lkf(r_n)
               if (r_n .eq. crsh_r_n_act_en_hdr(r_n_act_en)) then
                  prev_r_n = -1
                  crsh_r_n_act_en_hdr(r_n_act_en) = nx_r_n
               else
                  prev_r_n = crsh_r_n_act_en_lkb(r_n)
                  crsh_r_n_act_en_lkf(prev_r_n) = nx_r_n
               end if
               if (nx_r_n .gt. 0) crsh_r_n_act_en_lkb(nx_r_n) = prev_r_n
c
c     Reduce the number of active entries in this row by one and...
c
               r_n_act_en = r_n_act_en - 1
               crsh_r_n_act_en(r_n) = r_n_act_en
               if (r_n_act_en .gt. 0) then
c
c     ... either add the row as the header of the list with one fewer
c     number of active entries...
c
                  nx_r_n = crsh_r_n_act_en_hdr(r_n_act_en)
                  crsh_r_n_act_en_hdr(r_n_act_en) = r_n
                  crsh_r_n_act_en_lkb(r_n) = -1
                  crsh_r_n_act_en_lkf(r_n) = nx_r_n
                  if (nx_r_n .gt. 0) crsh_r_n_act_en_lkb(nx_r_n) = r_n
               else
c
c     ...or, if the count is zero, the row leaves the active submatrix
c     and the priority value data structure
c
                  crsh_r_st(r_n) = crsh_vr_st_no_act
                  crsh_pri_v = crsh_r_ty_pri_v(crsh_r_ty(r_n))
                  nx_r_n = crsh_r_pri_lkf(r_n)
                  if (r_n .eq. crsh_r_pri_v_hdr(crsh_pri_v)) then
                     prev_r_n = -1
                     crsh_r_pri_v_hdr(crsh_pri_v) = nx_r_n
                  else
                     prev_r_n = crsh_r_pri_lkb(r_n)
                     crsh_r_pri_lkf(prev_r_n) = nx_r_n
                  end if
                  if (nx_r_n .gt. 0) crsh_r_pri_lkb(nx_r_n) = prev_r_n
               end if
 210        continue
c
c     The column leaves the active submatrix
c
            crsh_c_st(c_n) = crsh_vr_st_no_act
 220     continue
      else
c
c     The basis has not changed. The row becomes inactive and the number
c     of active entries in each column in which it has entries is
c     reduced by one. If the number of active entries in any column is
c     zeroed as a result, then the corresponding column becomes
c     inactive.
c
         do 230, r_el_n = crsh_mtx_r_sa(cz_r_n),
     &        crsh_mtx_r_sa(cz_r_n+1)-1
            c_n = crsh_mtx_c_ix(r_el_n)
            if (crsh_c_st(c_n) .eq. crsh_vr_st_no_act) go to 230
            crsh_c_n_act_en(c_n) = crsh_c_n_act_en(c_n) - 1
            if (crsh_c_n_act_en(c_n) .eq. 0)
     &           crsh_c_st(c_n) = crsh_vr_st_no_act
 230     continue
         r_n = cz_r_n
c
c     Remove the row from the linked list with this number of active
c     entries
c
         r_n_act_en = crsh_r_n_act_en(r_n)
         nx_r_n = crsh_r_n_act_en_lkf(r_n)
         if (r_n .eq. crsh_r_n_act_en_hdr(r_n_act_en)) then
            prev_r_n = -1
            crsh_r_n_act_en_hdr(r_n_act_en) = nx_r_n
         else
            prev_r_n = crsh_r_n_act_en_lkb(r_n)
            crsh_r_n_act_en_lkf(prev_r_n) = nx_r_n
         end if
         if (nx_r_n .gt. 0) crsh_r_n_act_en_lkb(nx_r_n) = prev_r_n
c
c     the row leaves the active submatrix and the priority value data
c     structure
c
         crsh_r_st(r_n) = crsh_vr_st_no_act
         crsh_pri_v = crsh_r_ty_pri_v(crsh_r_ty(r_n))
         nx_r_n = crsh_r_pri_lkf(r_n)
         if (r_n .eq. crsh_r_pri_v_hdr(crsh_pri_v)) then
            prev_r_n = -1
            crsh_r_pri_v_hdr(crsh_pri_v) = nx_r_n
         else
            prev_r_n = crsh_r_pri_lkb(r_n)
            crsh_r_pri_lkf(prev_r_n) = nx_r_n
         end if
         if (nx_r_n .gt. 0) crsh_r_pri_lkb(nx_r_n) = prev_r_n
      end if
c
c     If there are no more rows with the current maximum row priority
c     then get the new maximum row priority value
c
      if (crsh_r_pri_v_hdr(crsh_pri_v) .le. 0 .and.
     &     crsh_pri_v .eq. mx_r_pri_v) then
         mx_r_pri_v = -i_inf
         do crsh_pri_v = crsh_mn_pri_v, crsh_mx_pri_v
            if (crsh_r_pri_v_hdr(crsh_pri_v) .gt. 0)
     &           mx_r_pri_v = crsh_pri_v
         end do
      end if
      if (crsh_r_st(cz_r_n) .ne. crsh_vr_st_no_act) go to 8010
      if (cz_c_n .gt. 0) then
         if (crsh_c_st(cz_c_n) .ne. crsh_vr_st_no_act) go to 8020
      end if
      go to 100
 1000 continue
CM      IF (emsol_dev .EQ. 1) THEN
C?      if (n_crsh_bs_cg .eq. 0) then
C?         write(crsh_ou_cn, 9200)
C?      else
C?         write(crsh_ou_cn, 9210)n_crsh_bs_cg,
C?     &        n_abs_pv_no_ok, mn_abs_pv_v,
C?     &        n_rlv_pv_no_ok, mn_rlv_pv_v
C?         do 1010, crsh_vr_ty = crsh_f_vr_ty, crsh_l_vr_ty
C?            if (crsh_vr_ty_og_n_r(crsh_vr_ty) .gt. 0) then
C?               pct = ems_i_t_i_pct(crsh_vr_ty_rm_n_r(crsh_vr_ty),
C?     &              crsh_vr_ty_og_n_r(crsh_vr_ty))
C?               write(crsh_ou_cn, 9220)' Removed ',
C?     &              crsh_vr_ty_rm_n_r(crsh_vr_ty),
C?     &              crsh_vr_ty_og_n_r(crsh_vr_ty),
C?     &              pct, 'rows   ', ch3_crsh_vr_ty(crsh_vr_ty)
C?            end if
C? 1010    continue
C?         do 1020, crsh_vr_ty = crsh_f_vr_ty, crsh_l_vr_ty
C?            if (crsh_vr_ty_og_n_c(crsh_vr_ty) .gt. 0) then
C?               pct = ems_i_t_i_pct(crsh_vr_ty_add_n_c(crsh_vr_ty),
C?     &              crsh_vr_ty_og_n_c(crsh_vr_ty))
C?               write(crsh_ou_cn, 9220)' Added ',
C?     &              crsh_vr_ty_add_n_c(crsh_vr_ty),
C?     &              crsh_vr_ty_og_n_c(crsh_vr_ty),
C?     &              pct, 'columns', ch3_crsh_vr_ty(crsh_vr_ty)
C?            end if
C? 1020    continue
C?         write(crsh_ou_cn, 9230)
C?         call ems_rp_crsh_vr_ty(
C?     &        crsh_ou_cn, st, crsh_r_ty, crsh_c_ty)
C?      end if
C?      call ems_flush(crsh_ou_cn)
CM      ENDIF
c
c     Record the number of pivotal rows in the list
c
      pv_r_ls(0) = n_vr_in_r
c
c     Get list of nonbasic variables from the status and complete the
c     list of basic variables.
c
      n_vr_in_c = 0
      do 1110, vr_n = 1, n_c
         if (iand(st(vr_n), bc_bt) .ne. 0) then
c
c     Basic structurals are in vr_in_r(1:n_vr_in_r)
c
         else
            n_vr_in_c = n_vr_in_c + 1
            vr_in_c(n_vr_in_c) = vr_n
         endif
 1110 continue
      do 1120, vr_n = mx_n_c+1, mx_n_c+n_r
         if (iand(st(vr_n), bc_bt) .ne. 0) then
            n_vr_in_r = n_vr_in_r + 1
            vr_in_r(n_vr_in_r) = vr_n
         else
            n_vr_in_c = n_vr_in_c + 1
            vr_in_c(n_vr_in_c) = vr_n
         endif
 1120 continue
      if (n_vr_in_r .ne. n_r) goto 8030
      if (n_vr_in_c .ne. n_c) goto 8040
c
c     Complete the list of pivotal rows
c
      do 1130, r_n = pv_r_ls(0)+1, n_r
         pv_r_ls(r_n) = vr_in_r(r_n)-mx_n_c
 1130 continue
c
c     Check the condition of the basis by forming a random system
c
      call ems_g_tri_bs_rand_tran_rsdu_norm(
     &     .true.,
     &     vr_in_r,
     &     pv_r_ls,
     &     pv_a_el,
     &     mtx_r_v, mtx_r_ix, mtx_c_sa,
     &     rhs, sol,
     &     ftran_rsdu_norm, btran_rsdu_norm)
      if (max(ftran_rsdu_norm, btran_rsdu_norm) .gt.
     &     tl_iz_bs_tran_er) then
         crsh_ps_mod_2 = n_crsh_ps/2 - 1
         crsh_ps_mod_2 = n_crsh_ps-1 - 2*crsh_ps_mod_2
         nw_tl_crsh_abs_pv_v =     mn_abs_pv_v*1d1
         nw_tl_crsh_rlv_pv_v = min(mn_rlv_pv_v*1d1, 0.999d0)
c
c     Increase the tolerance which resulted in fewer pivots being
c     rejected.
c
CM      IF (emsol_dev .EQ. 1) THEN
C?         write(crsh_ou_cn, 9300)ftran_rsdu_norm, btran_rsdu_norm,
C?     &        'suggest ill-conditioning '
CM      ENDIF
         if (n_abs_pv_no_ok .lt. n_rlv_pv_no_ok .or.
     &        tl_crsh_rlv_pv_v .ge. 0.999d0) then
            tl_crsh_abs_pv_v = nw_tl_crsh_abs_pv_v
CM      IF (emsol_dev .EQ. 1) THEN
C?            write(crsh_ou_cn, 9310)'absolute', tl_crsh_abs_pv_v
CM      ENDIF
         else
            tl_crsh_rlv_pv_v = nw_tl_crsh_rlv_pv_v
CM      IF (emsol_dev .EQ. 1) THEN
C?            write(crsh_ou_cn, 9310)'relative', tl_crsh_rlv_pv_v
CM      ENDIF
         endif
c         if (crsh_ps_mod_2 .eq. 1) then
c            if (nw_tl_crsh_abs_pv_v .lt. tl_crsh_abs_pv_v) then
c               tl_crsh_abs_pv_v = nw_tl_crsh_abs_pv_v
c            else
c               tl_crsh_rlv_pv_v = nw_tl_crsh_rlv_pv_v
c            endif
c         else
c            if (nw_tl_crsh_rlv_pv_v .lt. tl_crsh_rlv_pv_v) then
c               tl_crsh_rlv_pv_v = nw_tl_crsh_rlv_pv_v
c            else
c               tl_crsh_abs_pv_v = nw_tl_crsh_abs_pv_v
c            endif
c         endif
         n_crsh_ps = n_crsh_ps + 1
         goto 10
      else
CM      IF (emsol_dev .EQ. 1) THEN
C?         write(crsh_ou_cn, 9300)ftran_rsdu_norm, btran_rsdu_norm,
C?     &        'are OK'
CM      ENDIF
      endif
c
c     Set the up and down bits for the nonbasic variables
c
      do 1210, c_n = 1, n_c
         vr_n = vr_in_c(c_n)
         st(vr_n) = st(vr_n) - iand(st(vr_n), up_dn)
         if (ubc(vr_n) .ge. inf) then
            if (lbc(vr_n) .le. -inf) then
c     FR:
               st(vr_n) = st(vr_n) + up_dn
               pr_act(vr_n) = zero
            else
c     LB:
               st(vr_n) = st(vr_n) + up_bt
               pr_act(vr_n) = lbc(vr_n)
            end if
         else
            if (lbc(vr_n) .le. -inf) then
c     UB:
               st(vr_n) = st(vr_n) + dn_bt
               pr_act(vr_n) = ubc(vr_n)
            else
               if (lbc(vr_n) .lt. ubc(vr_n)) then
c     LB/UB:
                  if (abs(lbc(vr_n)) .lt. abs(ubc(vr_n))) then
                     st(vr_n) = st(vr_n) + up_bt
                     pr_act(vr_n) = lbc(vr_n)
                  else
                     st(vr_n) = st(vr_n) + dn_bt
                     pr_act(vr_n) = ubc(vr_n)
                  endif
               else
c     FX:
C                  st(vr_n) = st(vr_n)
                  pr_act(vr_n) = lbc(vr_n)
               end if
            end if
         endif
 1210 continue
 7000 continue
CM      IF (emsol_dev .EQ. 1) THEN
C?      call ems_flush(crsh_ou_cn)
CM      ENDIF
c
c     Indicate that the condition of the basis should not be questioned
c
      ml_da_st_msk = ior(ml_da_st_msk, ml_da_st_bs_cond_ok)
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)
     &     crsh_r_st(cz_r_n)
      call ems_msg_wr_li(bug_msg_n)
      go to 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)
     &     crsh_c_st(cz_c_n)
      call ems_msg_wr_li(bug_msg_n)
      go to 7000
 8030 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9803)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 8040 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9804)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
CM      IF (emsol_dev .EQ. 1) THEN
C? 9000 format(//'LTSSF Crash: Pass ', i2,
C?     &     /'   Using absolute pivot tolerance = ', 1pg10.4,
C?     &     /'   Using relative pivot tolerance = ', 1pg10.4)
C? 9010 format(//'Before crash:')
CM      ENDIF
c 9050 format('     It  ',
c     &     '    Row Count   Ty  Pri   Fn_v | ',
c     &     '    Col Count   Ty  Pri   Fn_v | ',
c     &     '   AbsPvV      Abs   RlvPvV      Rlv')
c 9060 format(i7, 2x,
c     &     i7, 2x, i4, 2x, a3, 2x, i3, 2x, i5, ' | ',
c     &     i7, 2x, i4, 2x, a3, 2x, i3, 2x, i5, ' | ',
c     &     2(2x, g11.4, 2x, l3))
CM      IF (emsol_dev .EQ. 1) THEN
C? 9200 format(//'Crash made no basis changes')
C? 9210 format(//'LTSSF Crash made ', i7, ' basis changes', /
C?     &     '   Absolute tolerance: Rejected ', i7,
C?     &     ' pivots: min absolute pivot value = ', 1pg11.4, /
C?     &     '   Relative tolerance: Rejected ', i7,
C?     &     ' pivots: min relative pivot value = ', 1pg11.4)
C? 
C? 9220 format(a9, i7, ' of ', i7,
C?     &     ' (', i3, '%) ', a7, ' of type ', a3)
C? 9230 format(//'After crash:')
C? 9300 format('Residual errors of ',
C?     &     g11.4, ' for FTRAN and ', g11.4,
C?     &     ' for BTRAN ', a)
C? 9310 format('Increasing ', a9, ' pivot tolerance to ', g11.4,
C?     &     ' and repeating LTSSF crash')
CM      ENDIF
 9801 format('STRANGE: crsh_r_st(cz_r_n) = ', i7)
 9802 format('STRANGE: crsh_c_st(cz_c_n) = ', i7)
 9803 format('CRASH: n_vr_in_r .ne. n_r')
 9804 format('CRASH: n_vr_in_c .ne. n_c')
      end
 
c->>> ----------------------------> ems_g_tri_bs_rand_tran_rsdu_norm <<<
c     Checks a triangular basis by solving Ax=b and A^Tx=b for x either
c
c     -- with components from a uniform distribution on [0,1] (rand = T)
c
c     -- a random unit vector (rand = F)
c
c     NB Assumes that the structurals and pivotal rows are in
c
c     vr_in_r(1:pv_r_ls(0)) and pv_r_ls(1:pv_r_ls(0)) respectively.
c
      subroutine ems_g_tri_bs_rand_tran_rsdu_norm(
     &     rand,
     &     vr_in_r,
     &     pv_r_ls,
     &     pv_a_el,
     &     mtx_r_v, mtx_r_ix, mtx_c_sa,
     &     rhs, sol,
     &     ftran_rsdu_norm, btran_rsdu_norm)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      logical rand
      integer vr_in_r(0:n_r)
      integer pv_r_ls(0:n_r)
      integer pv_a_el(0:n_r)
      integer mtx_r_ix(0:n_a_el)
      integer mtx_c_sa(0:n_c+1)
      double precision mtx_r_v(0:n_a_el)
      double precision rhs(0:n_r)
      double precision sol(0:n_r)
      double precision ftran_rsdu_norm, btran_rsdu_norm
      integer r_n, vr_n, c_n, el_n
      double precision rhs_v, rsdu
      double precision ems_drand
      integer rand_i
      double precision rand_v
      integer n_pv_r, pv_r_n, pv_el_n
      double precision pv, su
 
c 1    continue
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
c         print*, ' Enter rand_i: random value is ', rand_i
c         read*, rand_i
         if (rand_i .eq. 0) then
            do r_n = 1, n_r
               sol(r_n) = one
            enddo
         else
            sol(rand_i) = one
         endif
      endif
      n_pv_r = pv_r_ls(0)
c
c     Form the RHS for FTRAN
c
      do 30, c_n = 1, n_pv_r
         vr_n = vr_in_r(c_n)
         do 20, el_n = mtx_c_sa(vr_n), mtx_c_sa(vr_n+1)-1
            r_n = mtx_r_ix(el_n)
            rhs(r_n) = rhs(r_n) + mtx_r_v(el_n)*sol(c_n)
 20      continue
 30   continue
      do 40, c_n = n_pv_r+1, n_r
         vr_n = vr_in_r(c_n)
         r_n = vr_n - mx_n_c
         rhs(r_n) = rhs(r_n) - sol(c_n)
 40   continue
c
c     FTRAN
c
      do 60, c_n = 1, n_pv_r
         vr_n = vr_in_r(c_n)
         pv_r_n = pv_r_ls(c_n)
         pv_el_n = pv_a_el(c_n)
         pv = mtx_r_v(pv_el_n)
         mtx_r_v(pv_el_n) = zero
         rhs(pv_r_n) = -rhs(pv_r_n)/pv
         do 50, el_n = mtx_c_sa(vr_n), mtx_c_sa(vr_n+1)-1
            r_n = mtx_r_ix(el_n)
            rhs(r_n) = rhs(r_n) + mtx_r_v(el_n)*rhs(pv_r_n)
 50      continue
         mtx_r_v(pv_el_n) = pv
 60   continue
 
      ftran_rsdu_norm = zero
      do 70, r_n = 1, n_r
         rsdu = rhs(pv_r_ls(r_n)) + sol(r_n)
         ftran_rsdu_norm = ftran_rsdu_norm + rsdu*rsdu
 70   continue
      ftran_rsdu_norm = sqrt(ftran_rsdu_norm)
c      print*, '*** RANDOM FTRAN residual error of ', ftran_rsdu_norm
c
c     Form the RHS for BTRAN
c
      do 130, c_n = 1, n_pv_r
         rhs_v = zero
         vr_n = vr_in_r(c_n)
         do 120, el_n = mtx_c_sa(vr_n), mtx_c_sa(vr_n+1)-1
            r_n = mtx_r_ix(el_n)
            rhs_v = rhs_v + mtx_r_v(el_n)*sol(r_n)
 120     continue
         rhs(pv_r_ls(c_n)) = rhs_v
 130  continue
      do 140, c_n = n_pv_r+1, n_r
         vr_n = vr_in_r(c_n)
         r_n = vr_n - mx_n_c
         rhs(pv_r_ls(c_n)) = -sol(r_n)
 140  continue
c
c     BTRAN
c
      do 160, c_n = n_pv_r, 1, -1
         vr_n = vr_in_r(c_n)
         pv_r_n = pv_r_ls(c_n)
         pv_el_n = pv_a_el(c_n)
         pv = mtx_r_v(pv_el_n)
         mtx_r_v(pv_el_n) = zero
         su = zero
         do 150, el_n = mtx_c_sa(vr_n), mtx_c_sa(vr_n+1)-1
            r_n = mtx_r_ix(el_n)
            su = su + mtx_r_v(el_n)*rhs(r_n)
 150     continue
         rhs(pv_r_n) = -(rhs(pv_r_n)+su)/pv
         mtx_r_v(pv_el_n) = pv
 160  continue
      btran_rsdu_norm = zero
      do 170, r_n = 1, n_r
         rsdu = rhs(r_n) + sol(r_n)
         btran_rsdu_norm = btran_rsdu_norm + rsdu*rsdu
         rhs(r_n) = zero
         sol(r_n) = zero
 170  continue
      btran_rsdu_norm = sqrt(btran_rsdu_norm)
c      print*, '*** RANDOM BTRAN residual error of ', btran_rsdu_norm
c      if (.not. rand) goto 1
      return
      end
 
      subroutine ems_ltssf_iz_da_str(
     &     lbc, ubc, st,
     &     mtx_r_v, mtx_r_ix, mtx_c_sa,
     &     crsh_r_ty, crsh_r_pri_lkb, crsh_r_pri_lkf,
     &     crsh_c_ty,
     &     crsh_r_n_act_en, crsh_r_n_act_en_hdr,
     &     crsh_r_n_act_en_lkb, crsh_r_n_act_en_lkf,
     &     crsh_c_n_act_en,
     &     crsh_mtx_c_mx_abs_v,
     &     crsh_mtx_c_v, crsh_mtx_c_ix, crsh_mtx_r_sa,
     &     crsh_r_st, crsh_c_st)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'CRASH.INC'
      include 'ICTVR.INC'
      double precision lbc(0:mx_n_c+n_r)
      double precision ubc(0:mx_n_c+n_r)
      integer st(0:mx_n_c+n_r)
      double precision mtx_r_v(0:n_a_el)
      integer mtx_r_ix(0:n_a_el)
      integer mtx_c_sa(0:n_c+1)
      integer crsh_r_ty(0:n_r)
      integer crsh_r_pri_lkb(0:n_r)
      integer crsh_r_pri_lkf(0:n_r)
      integer crsh_c_ty(0:n_c)
      integer crsh_r_n_act_en(0:n_r)
      integer crsh_r_n_act_en_hdr(0:n_c)
      integer crsh_r_n_act_en_lkb(0:n_r)
      integer crsh_r_n_act_en_lkf(0:n_r)
      integer crsh_c_n_act_en(0:n_c)
      double precision crsh_mtx_c_mx_abs_v(0:n_c)
      double precision crsh_mtx_c_v(0:n_a_el)
      integer crsh_mtx_c_ix(0:n_a_el)
      integer crsh_mtx_r_sa(0:n_r+1)
      integer crsh_r_st(0:n_r)
      integer crsh_c_st(0:n_c)
      integer r_n, c_n, vr_n, el_n, vr_st
      integer crsh_pri_v, crsh_vr_ty
      integer nx_r_n
      integer c_n_en, r_n_act_en
 
      do 10, crsh_pri_v = crsh_mn_pri_v, crsh_mx_pri_v
         crsh_r_pri_v_hdr(crsh_pri_v) = -1
 10   continue
      do 12, crsh_vr_ty = crsh_f_vr_ty, crsh_l_vr_ty
         crsh_vr_ty_og_n_r(crsh_vr_ty) = 0
         crsh_vr_ty_og_n_c(crsh_vr_ty) = 0
         crsh_vr_ty_rm_n_r(crsh_vr_ty) = 0
         crsh_vr_ty_add_n_c(crsh_vr_ty) = 0
 12   continue
      do 14, c_n = 1, n_c
         crsh_r_n_act_en_hdr(c_n) = -1
 14   continue
c
c     Determine the status and type of each row
c
      do 20, r_n = 1, n_r
         vr_n = mx_n_c + r_n
         vr_st = st(vr_n)
c
c     Extract the type
c
         crsh_vr_ty = crsh_r_ty(r_n)
c
c     Assign the status
c
         crsh_pri_v = crsh_r_ty_pri_v(crsh_vr_ty)
c         if (crsh_vr_ty .eq. crsh_vr_ty_fr) then
         if (crsh_pri_v .eq. crsh_no_act_pri_v) then
            crsh_r_st(r_n) = crsh_vr_st_no_act
         else
            crsh_r_st(r_n) = crsh_vr_st_act
         end if
c
c     Ensure that the logical is basic.
c
         st(vr_n) = ior(vr_st, bc_vr_bs_st)
c
c     Initialise the count for later accumulation
c
         crsh_r_n_act_en(r_n) = 0
c
c     Keep a count of the original number of rows of each type
c
         crsh_vr_ty_og_n_r(crsh_vr_ty) =
     &        crsh_vr_ty_og_n_r(crsh_vr_ty) + 1
 20   continue
c
c     Determine the status and type of each column and calculate the
c     number of active entries in each row and column
c
      do 40, c_n = 1, n_c
         vr_n = c_n
         vr_st = st(vr_n)
c
c     Extract the type
c
         crsh_vr_ty = crsh_c_ty(c_n)
c
c     Assign the status
c
         c_n_en = mtx_c_sa(c_n+1) - mtx_c_sa(c_n)
         crsh_pri_v = crsh_c_ty_pri_v(crsh_vr_ty)
c         if (crsh_vr_ty .eq. crsh_vr_ty_fx .or.
         if (crsh_pri_v .eq. crsh_no_act_pri_v .or.
     &        c_n_en .eq. 0) then
c
c     Fixed columns and zero columns are not active---the latter would
c     be avoided anyway since they would not appear in any row search.
c
            crsh_c_st(c_n) = crsh_vr_st_no_act
         else
            crsh_c_st(c_n) = crsh_vr_st_act
         end if
c
c     Ensure that the structural is nonbasic.
c
         st(vr_n) = vr_st - iand(vr_st, su_vr_bs_bt) + non_bc_vr_bs_st
c
c     Keep a count of the original number of columns of each type
c
         crsh_vr_ty_og_n_c(crsh_vr_ty) =
     &        crsh_vr_ty_og_n_c(crsh_vr_ty) + 1
c
c     If not active then nothing more needs to be done---the type is
c     only needed when the break-down of columns types is reported.
c
         if (crsh_c_st(c_n) .eq. crsh_vr_st_no_act) go to 40
c
c     Determine the number of active entries in the column and update
c     the number of active entries in each row with entries in this
c     column.
c
         crsh_c_n_act_en(c_n) = 0
         do 30, el_n = mtx_c_sa(c_n), mtx_c_sa(c_n+1)-1
            r_n = mtx_r_ix(el_n)
            if (crsh_r_st(r_n) .eq. crsh_vr_st_no_act) go to 30
            crsh_c_n_act_en(c_n) = crsh_c_n_act_en(c_n) + 1
            crsh_r_n_act_en(r_n) = crsh_r_n_act_en(r_n) + 1
 30      continue
 40      continue
c
c     Now that the row counts are known, make any zero rows non-active.
c     Form linked list of active rows with each priority.
c
      do 110, r_n = 1, n_r
         if (crsh_r_st(r_n) .eq. crsh_vr_st_no_act) go to 110
         if (crsh_r_n_act_en(r_n) .le. 0) then
            crsh_r_st(r_n) = crsh_vr_st_no_act
            go to 110
         end if
c
c     Add as the header of the appropriate priority value list
c
         crsh_pri_v = crsh_r_ty_pri_v(crsh_r_ty(r_n))
         nx_r_n = crsh_r_pri_v_hdr(crsh_pri_v)
         crsh_r_pri_v_hdr(crsh_pri_v) = r_n
         crsh_r_pri_lkb(r_n) = -1
         crsh_r_pri_lkf(r_n) = nx_r_n
         if (nx_r_n .gt. 0) crsh_r_pri_lkb(nx_r_n) = r_n
c
c     Add as the header of the appropriate list of rows with a given
c     count
c
         r_n_act_en = crsh_r_n_act_en(r_n)
         nx_r_n = crsh_r_n_act_en_hdr(r_n_act_en)
         crsh_r_n_act_en_hdr(r_n_act_en) = r_n
         crsh_r_n_act_en_lkb(r_n) = -1
         crsh_r_n_act_en_lkf(r_n) = nx_r_n
         if (nx_r_n .gt. 0) crsh_r_n_act_en_lkb(nx_r_n) = r_n
 110  continue
c
c     Set up the row-wise matrix.
c     Although not essiential, it is convenient to avoid columns which
c     are not active since the row counts determined above can be used.
c
      crsh_mtx_r_sa(1) = 1
      do 210, r_n = 1, n_r
         crsh_mtx_r_sa(r_n+1) =
     &        crsh_mtx_r_sa(r_n) + crsh_r_n_act_en(r_n)
 210   continue
c
c     Used to get infeasible basis for FIT2P.
c
c      do 230, c_n = n_c, 1, -1
c
      do 230, c_n = 1, n_c
         if (crsh_c_st(c_n) .eq. crsh_vr_st_no_act) go to 230
         crsh_mtx_c_mx_abs_v(c_n) = zero
         do 220, el_n = mtx_c_sa(c_n), mtx_c_sa(c_n+1)-1
            r_n = mtx_r_ix(el_n)
            crsh_mtx_c_mx_abs_v(c_n) =
     &           max(abs(mtx_r_v(el_n)), crsh_mtx_c_mx_abs_v(c_n))
            if (crsh_r_st(r_n) .eq. crsh_vr_st_no_act) go to 220
            crsh_mtx_c_ix(crsh_mtx_r_sa(r_n)) = c_n
            crsh_mtx_c_v(crsh_mtx_r_sa(r_n)) = mtx_r_v(el_n)
            crsh_mtx_r_sa(r_n) = crsh_mtx_r_sa(r_n) + 1
 220     continue
 230  continue
      do 240, r_n = 1, n_r
         crsh_mtx_r_sa(r_n) = crsh_mtx_r_sa(r_n) - crsh_r_n_act_en(r_n)
 240  continue
      return
      end
 
      subroutine ems_ltssf_ck_da_str(er_fd, ck_lvl,
     &     st,
     &     mtx_r_v, mtx_r_ix, mtx_c_sa,
     &     crsh_r_ty, crsh_r_pri_lkb, crsh_r_pri_lkf,
     &     crsh_c_ty,
     &     crsh_r_n_act_en, crsh_r_n_act_en_hdr,
     &     crsh_r_n_act_en_lkb, crsh_r_n_act_en_lkf,
     &     crsh_c_n_act_en,
     &     crsh_mtx_c_mx_abs_v,
     &     crsh_mtx_c_v, crsh_mtx_c_ix, crsh_mtx_r_sa,
     &     crsh_r_st, crsh_c_st)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMSG.INC'
      include 'CRASH.INC'
      include 'ICTVR.INC'
      logical er_fd
      integer ck_lvl
      integer st(0:mx_n_c+n_r)
      double precision mtx_r_v(0:n_a_el)
      integer mtx_r_ix(0:n_a_el)
      integer mtx_c_sa(0:n_c+1)
      integer crsh_r_ty(0:n_r)
      integer crsh_r_pri_lkb(0:n_r)
      integer crsh_r_pri_lkf(0:n_r)
      integer crsh_c_ty(0:n_c)
      integer crsh_r_n_act_en(0:n_r)
      integer crsh_r_n_act_en_hdr(0:n_c)
      integer crsh_r_n_act_en_lkb(0:n_r)
      integer crsh_r_n_act_en_lkf(0:n_r)
      integer crsh_c_n_act_en(0:n_c)
      double precision crsh_mtx_c_mx_abs_v(0:n_c)
      double precision crsh_mtx_c_v(0:n_a_el)
      integer crsh_mtx_c_ix(0:n_a_el)
      integer crsh_mtx_r_sa(0:n_r+1)
      integer crsh_r_st(0:n_r)
      integer crsh_c_st(0:n_c)
      integer r_n, c_n, el_n
      integer k
      integer pri_v, r_pri_v
      integer prev_r_n, nx_r_n
      integer r_n_act_en, tru_r_n_act_en
 
      er_fd = .false.
      if (ck_lvl .eq. 0) go to 7000
      do 20, r_n = 1, n_r
         if (crsh_r_st(r_n) .eq. crsh_vr_st_no_act) go to 20
         k = 0
         do 10, el_n = crsh_mtx_r_sa(r_n), crsh_mtx_r_sa(r_n+1)-1
            c_n = crsh_mtx_c_ix(el_n)
            if (crsh_c_st(c_n) .eq. crsh_vr_st_act) k = k + 1
 10      continue
         if (k .ne. crsh_r_n_act_en(r_n)) then
            er_fd = .true.
CM      IF (emsol_dev .EQ. 1) THEN
C?            write(*, *)'Row ', r_n, ' k = ', k,
C?     &           ' .ne. crsh_r_n_act_en(r_n) = ',
C?     &           crsh_r_n_act_en(r_n)
CM      ENDIF
         end if
 20   continue
      do 120, c_n = 1, n_c
         if (crsh_c_st(c_n) .eq. crsh_vr_st_no_act) go to 120
         k = 0
         do 110, el_n = mtx_c_sa(c_n), mtx_c_sa(c_n+1)-1
            r_n = mtx_r_ix(el_n)
            if (crsh_r_st(r_n) .eq. crsh_vr_st_act) k = k + 1
 110      continue
         if (k .ne. crsh_c_n_act_en(c_n)) then
            er_fd = .true.
CM      IF (emsol_dev .EQ. 1) THEN
C?            write(*, *)'Column ', c_n, ' k = ', k,
C?     &           ' .ne. crsh_c_n_act_en(c_n) = ',
C?     &           crsh_c_n_act_en(c_n)
CM      ENDIF
         end if
 120  continue
      do 210, pri_v = crsh_mn_pri_v, crsh_mx_pri_v
         r_n = crsh_r_pri_v_hdr(pri_v)
         if (r_n .lt. 0) go to 210
         prev_r_n = crsh_r_pri_lkb(r_n)
         if (prev_r_n .gt. 0) then
            er_fd = .true.
CM      IF (emsol_dev .EQ. 1) THEN
C?            write(*, *)'Link back from header = ', r_n,
C?     &           ' is row ', prev_r_n, ' not -1 '
CM      ENDIF
         end if
 205     continue
         r_pri_v = crsh_r_ty_pri_v(crsh_r_ty(r_n))
         if (r_pri_v .ne. pri_v) then
            er_fd = .true.
CM      IF (emsol_dev .EQ. 1) THEN
C?            write(*, *)'Row ', r_n, ' has priority value ', r_pri_v,
C?     &           ' but is in linked list for priority value ', pri_v
CM      ENDIF
         end if
         nx_r_n = crsh_r_pri_lkf(r_n)
         if (nx_r_n .gt. 0) then
            prev_r_n = crsh_r_pri_lkb(nx_r_n)
            if (prev_r_n .ne. r_n) then
               er_fd = .true.
CM      IF (emsol_dev .EQ. 1) THEN
C?               write(*, *)'Link back from nx_r_n = ', nx_r_n,
C?     &              ' is row ', prev_r_n, ' not n_r = ', n_r
CM      ENDIF
            end if
            r_n = nx_r_n
            go to 205
         end if
 210  continue
      do 310, r_n_act_en = 1, n_c
         r_n = crsh_r_n_act_en_hdr(r_n_act_en)
         if (r_n .lt. 0) go to 310
         prev_r_n = crsh_r_n_act_en_lkb(r_n)
         if (prev_r_n .gt. 0) then
            er_fd = .true.
CM      IF (emsol_dev .EQ. 1) THEN
C?            write(*, *)'Link back from n_act_en header = ', r_n,
C?     &           ' is row ', prev_r_n, ' not -1 '
CM      ENDIF
         end if
 305     continue
         tru_r_n_act_en = crsh_r_n_act_en(r_n)
         if (tru_r_n_act_en .ne. r_n_act_en) then
            er_fd = .true.
CM      IF (emsol_dev .EQ. 1) THEN
C?            write(*, *)'Row ', r_n,
C?     &           ' has number of active entries ', tru_r_n_act_en,
C?     &           ' but is in linked list for number of',
C?     &           ' active entries ',
C?     &           r_n_act_en
CM      ENDIF
         end if
         nx_r_n = crsh_r_n_act_en_lkf(r_n)
         if (nx_r_n .gt. 0) then
            prev_r_n = crsh_r_n_act_en_lkb(nx_r_n)
            if (prev_r_n .ne. r_n) then
               er_fd = .true.
CM      IF (emsol_dev .EQ. 1) THEN
C?               write(*, *)'Link back from n_act_en nx_r_n = ',
C?     &              nx_r_n,
C?     &              ' is row ', prev_r_n, ' not n_r = ', n_r
CM      ENDIF
            end if
            r_n = nx_r_n
            go to 305
         end if
 310  continue
      if (ck_lvl .le. 1) go to 7000
      do 420, r_n = 1, n_r
         if (crsh_r_st(r_n) .eq. crsh_vr_st_no_act) go to 420
         pri_v = crsh_r_ty_pri_v(crsh_r_ty(r_n))
         nx_r_n = crsh_r_pri_v_hdr(pri_v)
         if (nx_r_n .le. 0) go to 410
 405     continue
         if (nx_r_n .eq. r_n) go to 420
         nx_r_n = crsh_r_pri_lkf(nx_r_n)
         if (nx_r_n .gt. 0) go to 405
 410     continue
         er_fd = .true.
CM      IF (emsol_dev .EQ. 1) THEN
C?         write(*, *)'Row', r_n,
C?     &        ' is active but not in the list with priority ', pri_v
CM      ENDIF
 420  continue
      do 520, r_n = 1, n_r
         if (crsh_r_st(r_n) .eq. crsh_vr_st_no_act) go to 520
         r_n_act_en = crsh_r_n_act_en(r_n)
         nx_r_n = crsh_r_n_act_en_hdr(r_n_act_en)
         if (nx_r_n .le. 0) go to 510
 505     continue
         if (nx_r_n .eq. r_n) go to 520
         nx_r_n = crsh_r_n_act_en_lkf(nx_r_n)
         if (nx_r_n .gt. 0) go to 505
 510     continue
         er_fd = .true.
CM      IF (emsol_dev .EQ. 1) THEN
C?         write(*, *)'Row', r_n,
C?     &        ' is active but not in the list with n_act_en = ',
C?     &        r_n_act_en
CM      ENDIF
 520  continue
 
 7000 continue
      return
      end
 
CM      IF (emsol_dev .EQ. 1) THEN
C?      subroutine ems_rp_crsh_vr_ty(ou_cn, st, crsh_r_ty, crsh_c_ty)
C?      implicit none
C?      include 'EMSV.INC'
C?      include 'EMSMMGR.INC'
C?      include 'EMSPM.INC'
C?      include 'CRASH.INC'
C?      include 'ICTVR.INC'
C?      include 'EMSMSG.INC'
C?      integer ou_cn
C?      integer st(0:mx_n_c+n_r)
C?      integer crsh_r_ty(0:n_r)
C?      integer crsh_c_ty(0:n_c)
C?      integer ems_i_t_i_pct
C?      integer crsh_n_bc_r_o_ty(crsh_f_vr_ty:crsh_l_vr_ty)
C?      integer crsh_n_bc_c_o_ty(crsh_f_vr_ty:crsh_l_vr_ty)
C?      integer crsh_n_non_bc_r_o_ty(crsh_f_vr_ty:crsh_l_vr_ty)
C?      integer crsh_n_non_bc_c_o_ty(crsh_f_vr_ty:crsh_l_vr_ty)
C?      integer crsh_vr_ty, r_n, c_n, vr_n, n_lg, n_struc
C?
C?      do 1, crsh_vr_ty = crsh_f_vr_ty, crsh_l_vr_ty
C?         crsh_n_bc_r_o_ty(crsh_vr_ty) = 0
C?         crsh_n_bc_c_o_ty(crsh_vr_ty) = 0
C?         crsh_n_non_bc_r_o_ty(crsh_vr_ty) = 0
C?         crsh_n_non_bc_c_o_ty(crsh_vr_ty) = 0
C? 1    continue
C?      n_lg = 0
C?      do 10, r_n = 1, n_r
C?         vr_n = mx_n_c + r_n
C?         if (iand(st(vr_n), bc_bt) .ne. 0) then
C?            n_lg = n_lg + 1
C?            crsh_n_bc_r_o_ty(crsh_r_ty(r_n)) =
C?     &           crsh_n_bc_r_o_ty(crsh_r_ty(r_n)) + 1
C?         else
C?            crsh_n_non_bc_r_o_ty(crsh_r_ty(r_n)) =
C?     &           crsh_n_non_bc_r_o_ty(crsh_r_ty(r_n)) + 1
C?         endif
C? 10   continue
C?      n_struc = 0
C?      do 20, c_n = 1, n_c
C?         vr_n = c_n
C?         if (iand(st(vr_n), bc_bt) .ne. 0) then
C?            n_struc = n_struc + 1
C?            crsh_n_bc_c_o_ty(crsh_c_ty(c_n)) =
C?     &           crsh_n_bc_c_o_ty(crsh_c_ty(c_n)) + 1
C?         else
C?            crsh_n_non_bc_c_o_ty(crsh_c_ty(c_n)) =
C?     &           crsh_n_non_bc_c_o_ty(crsh_c_ty(c_n)) + 1
C?         endif
C? 20   continue
C?      write(ou_cn, 9000)n_r
C?      if (n_struc .gt. 0) then
C?         write(ou_cn, 9010)n_struc, ems_i_t_i_pct(n_struc, n_r)
C?         do 30, crsh_vr_ty = crsh_f_vr_ty, crsh_l_vr_ty
C?            if (crsh_n_bc_c_o_ty(crsh_vr_ty) .eq. 0) goto 30
C?            write(ou_cn, 9015)ch3_crsh_vr_ty(crsh_vr_ty),
C?     &           crsh_n_bc_c_o_ty(crsh_vr_ty)
C? 30      continue
C?      endif
C?      if (n_lg .gt. 0) then
C?         write(ou_cn, 9020)n_lg, ems_i_t_i_pct(n_lg, n_r)
C?         do 40, crsh_vr_ty = crsh_f_vr_ty, crsh_l_vr_ty
C?            if (crsh_n_bc_r_o_ty(crsh_vr_ty) .eq. 0) goto 40
C?            write(ou_cn, 9015)ch3_crsh_vr_ty(crsh_vr_ty),
C?     &           crsh_n_bc_r_o_ty(crsh_vr_ty)
C? 40      continue
C?      endif
C?      n_struc = n_c - n_struc
C?      n_lg = n_r - n_lg
C?      write(ou_cn, 9100)n_c
C?      if (n_struc .gt. 0) then
C?         write(ou_cn, 9010)n_struc, ems_i_t_i_pct(n_struc, n_c)
C?         do 130, crsh_vr_ty = crsh_f_vr_ty, crsh_l_vr_ty
C?            if (crsh_n_non_bc_c_o_ty(crsh_vr_ty) .eq. 0) goto 130
C?            write(ou_cn, 9015)ch3_crsh_vr_ty(crsh_vr_ty),
C?     &           crsh_n_non_bc_c_o_ty(crsh_vr_ty)
C? 130     continue
C?      endif
C?      if (n_lg .gt. 0) then
C?         write(ou_cn, 9020)n_lg, ems_i_t_i_pct(n_lg, n_c)
C?         do 140, crsh_vr_ty = crsh_f_vr_ty, crsh_l_vr_ty
C?            if (crsh_n_non_bc_r_o_ty(crsh_vr_ty) .eq. 0) goto 140
C?            write(ou_cn, 9015)ch3_crsh_vr_ty(crsh_vr_ty),
C?     &           crsh_n_non_bc_r_o_ty(crsh_vr_ty)
C? 140     continue
C?      endif
C?      return
C? 9000 format(/'Basis contains ', i7, ' variables, of which')
C? 9010 format(i7, ' are structural, a proportion of ', i3, '%;')
C? 9015 format(23x, ' Type ', a3, ': ', i7)
C? 9020 format(i7, ' are logical,    a proportion of ', i3, '%;')
C? 9100 format(/'There are ', i7, ' nonbasic variables, of which')
C?      end
C?      subroutine ems_rp_crsh_act_mtx(cn_n,
C?     &     crsh_r_n_act_en,
C?     &     crsh_c_n_act_en,
C?     &     crsh_r_ty,
C?     &     crsh_c_ty,
C?     &     crsh_r_st,
C?     &     crsh_c_st,
C?     &     crsh_mtx_c_ix, crsh_mtx_r_sa,
C?     &     mtx_r_ix, mtx_c_sa)
C?      implicit none
C?      include 'EMSV.INC'
C?      include 'EMSPM.INC'
C?      include 'EMSMMGR.INC'
C?      include 'CRASH.INC'
C?      include 'ICTVR.INC'
C?      integer mx_rp_n_el
C?      parameter (mx_rp_n_el = 1000)
C?      integer rp_el(mx_rp_n_el)
C?      integer cn_n
C?      integer crsh_r_n_act_en(0:n_r)
C?      integer crsh_c_n_act_en(0:n_c)
C?      integer crsh_r_ty(0:n_r)
C?      integer crsh_c_ty(0:n_c)
C?      integer crsh_r_st(0:n_r)
C?      integer crsh_c_st(0:n_c)
C?      integer crsh_mtx_c_ix(0:n_a_el)
C?      integer crsh_mtx_r_sa(0:n_r+1)
C?      integer mtx_r_ix(0:n_a_el)
C?      integer mtx_c_sa(0:n_c+1)
C?      integer r_n, c_n, rp_n_el, el_n
C?
C?      if (n_c .gt. mx_rp_n_el) goto 7000
C?      if (n_r .gt. mx_rp_n_el) goto 7000
C?      write(cn_n, 9000)
C?      do 10, r_n = 1, n_r
C?         if (crsh_r_st(r_n) .eq. crsh_vr_st_no_act) goto 10
C?         rp_n_el = 0
C?         do 5, el_n = crsh_mtx_r_sa(r_n), crsh_mtx_r_sa(r_n+1)-1
C?            c_n = crsh_mtx_c_ix(el_n)
C?            if (crsh_c_st(c_n) .eq. crsh_vr_st_no_act) goto 5
C?            rp_n_el = rp_n_el + 1
C?            rp_el(rp_n_el) = el_n
C? 5       continue
C?         write(cn_n, 9100)r_n, crsh_r_n_act_en(r_n), crsh_r_ty(r_n),
C?     &        (crsh_mtx_c_ix(rp_el(el_n)), el_n = 1, rp_n_el)
C? 10   continue
C?      write(cn_n, 9010)
C?      do 20, c_n = 1, n_c
C?         if (crsh_c_st(c_n) .eq. crsh_vr_st_no_act) goto 20
C?         rp_n_el = 0
C?         do 15, el_n = mtx_c_sa(c_n), mtx_c_sa(c_n+1)-1
C?            r_n = mtx_r_ix(el_n)
C?            if (crsh_r_st(r_n) .eq. crsh_vr_st_no_act) goto 15
C?            rp_n_el = rp_n_el + 1
C?            rp_el(rp_n_el) = el_n
C? 15      continue
C?         write(cn_n, 9100)c_n, crsh_c_n_act_en(c_n), crsh_c_ty(c_n),
C?     &        (mtx_r_ix(rp_el(el_n)), el_n = 1, rp_n_el)
C? 20      continue
C? 7000 continue
C?      return
C? 9000 format(/'    Row Count  Ty | ')
C? 9010 format(/'    Col Count  Ty | ')
C? 9100 format(i7, 2x, i4, 2x, i2, ' | ', 10i5, (/14x, ' | ', 10i5))
C?      end
CM      ENDIF
      subroutine ems_g_n_fx_vr_in_bs(
     &     lg_bs, vr_in_r,
     &     n_fx_vr_in_bs,
     &     lbc, ubc)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      logical lg_bs
      integer vr_in_r(0:n_r)
      integer n_fx_vr_in_bs
      double precision lbc(0:n_r+n_c)
      double precision ubc(0:n_r+n_c)
      integer r_n, vr_n
 
      n_fx_vr_in_bs = 0
      if (lg_bs) then
         do 10, r_n = 1, n_r
            vr_n = mx_n_c + r_n
            if (lbc(vr_n) .eq. ubc(vr_n))
     &           n_fx_vr_in_bs = n_fx_vr_in_bs + 1
 10      continue
      else
         do 30, r_n = 1, n_r
            vr_n = vr_in_r(r_n)
            if (lbc(vr_n) .eq. ubc(vr_n))
     &           n_fx_vr_in_bs = n_fx_vr_in_bs + 1
 30      continue
      end if
      return
      end
 
C->>> ---------------------------------------> ems_ltssf_iz_blk_crsh <<<
c     Sets up a block and the handles for LTSSF crash.
c
      subroutine ems_ltssf_iz_blk_crsh(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'CRASH.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer r_cf, c_cf, a_el_cf, cs, n_wo
      integer mem_mgr_rt_cod
 
      call ems_g_ltssf_blk_crsh_n_wo(r_cf, c_cf, a_el_cf, cs, n_wo)
c      r_cf =    8*i_wo_z
c      c_cf =    4*i_wo_z +   rl_wo_z
c      a_el_cf =   i_wo_z +   rl_wo_z
c      cs =     14*i_wo_z + 2*rl_wo_z
c      n_wo =
c     &     rl_wo_z*(
c     &     (1+n_c) + (1+n_a_el)) +
c     &     i_wo_z*(
c     &     (1+n_r) + (1+n_r) + (1+n_r) +
c     &     (1+n_c) +
c     &     (1+n_r) + (1+n_c) + (1+n_r) + (1+n_r) +
c     &     (1+n_c) +
c     &     (1+n_a_el) + (1+n_r+1) + (1+n_r) + (1+n_c))
c      n_wo = r_cf*n_r + c_cf*n_c + a_el_cf*n_a_el + cs
      call ems_mem_mgr_ope_blk(mem_mgr_rt_cod, is,
     &     n_wo, ope_blk_at_l_fr_p,
     &     1, crsh_blk_id, blk_crsh)
      if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
         if (mem_mgr_rt_cod .eq. mem_mgr_rt_cod_serious_no_po) then
            ems_msg_cod = ems_msg_lvl_serious
            go to 7000
         else if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
             ems_msg_cod = ems_msg_lvl_serious
            go to 7000
         endif
      endif
      call ems_mem_mgr_rp_ope_blk(
     &     mem_mgr_rt_cod, is, -1,
     &     r_cf, n_r,
     &     c_cf, n_c,
     &     a_el_cf, n_a_el,
     &     cs, n_wo, crsh_blk_id)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
c
c     Get handles for the arrays within the block.
c
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_crsh, 1+n_a_el, rl_wo_z,
     &     hdl_crsh_mtx_c_v)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_crsh, 1+n_c, rl_wo_z,
     &     hdl_crsh_mtx_c_mx_abs_v)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_crsh, 1+n_r,  i_wo_z,
     &     hdl_crsh_r_ty)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_crsh, 1+n_r,  i_wo_z,
     &     hdl_crsh_r_pri_lkb)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_crsh, 1+n_r,  i_wo_z,
     &     hdl_crsh_r_pri_lkf)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_crsh, 1+n_c,  i_wo_z,
     &     hdl_crsh_c_ty)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_crsh, 1+n_r,  i_wo_z,
     &     hdl_crsh_r_n_act_en)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_crsh, 1+n_c,  i_wo_z,
     &     hdl_crsh_r_n_act_en_hdr)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_crsh, 1+n_r,  i_wo_z,
     &     hdl_crsh_r_n_act_en_lkb)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_crsh, 1+n_r,  i_wo_z,
     &     hdl_crsh_r_n_act_en_lkf)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_crsh, 1+n_c,  i_wo_z,
     &     hdl_crsh_c_n_act_en)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_crsh, 1+n_a_el,  i_wo_z,
     &     hdl_crsh_mtx_c_ix)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_crsh, 1+n_r+1,  i_wo_z,
     &     hdl_crsh_mtx_r_sa)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_crsh, 1+n_r,  i_wo_z,
     &     hdl_crsh_r_st)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_crsh, 1+n_c,  i_wo_z,
     &     hdl_crsh_c_st)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
c
c     Get the pointers for the arrays within the block.
c
      p_crsh_r_ty =       hdl_crsh_r_ty(hdl_os_p)
      p_crsh_r_pri_lkb =  hdl_crsh_r_pri_lkb(hdl_os_p)
      p_crsh_r_pri_lkf =  hdl_crsh_r_pri_lkf(hdl_os_p)
      p_crsh_c_ty =       hdl_crsh_c_ty(hdl_os_p)
      p_crsh_r_n_act_en = hdl_crsh_r_n_act_en(hdl_os_p)
      p_crsh_r_n_act_en_hdr =
     &     hdl_crsh_r_n_act_en_hdr(hdl_os_p)
      p_crsh_r_n_act_en_lkb =
     &     hdl_crsh_r_n_act_en_lkb(hdl_os_p)
      p_crsh_r_n_act_en_lkf =
     &     hdl_crsh_r_n_act_en_lkf(hdl_os_p)
      p_crsh_c_n_act_en = hdl_crsh_c_n_act_en(hdl_os_p)
      p_crsh_mtx_c_mx_abs_v =
     &     hdl_crsh_mtx_c_mx_abs_v(hdl_os_p)
      p_crsh_mtx_c_v =    hdl_crsh_mtx_c_v(hdl_os_p)
      p_crsh_mtx_c_ix =   hdl_crsh_mtx_c_ix(hdl_os_p)
      p_crsh_mtx_r_sa =   hdl_crsh_mtx_r_sa(hdl_os_p)
      p_crsh_r_st =       hdl_crsh_r_st(hdl_os_p)
      p_crsh_c_st =       hdl_crsh_c_st(hdl_os_p)
 7000 continue
      return
      end
 
C->>> -----------------------------------> ems_g_ltssf_blk_crsh_n_wo <<<
      subroutine ems_g_ltssf_blk_crsh_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      integer r_cf, c_cf, a_el_cf, cs, n_wo
      r_cf =    8*i_wo_z
      c_cf =    4*i_wo_z +   rl_wo_z
      a_el_cf =   i_wo_z +   rl_wo_z
      cs =     14*i_wo_z + 2*rl_wo_z
      n_wo = r_cf*n_r + c_cf*n_c + a_el_cf*n_a_el + cs
      return
      end
 
C->>> ----------------------------------------> ems_ltsf_iz_blk_crsh <<<
c     Sets up a block and the handles for LTSF Crash
c
      subroutine ems_ltsf_iz_blk_crsh(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'CRASH.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer r_cf, c_cf, a_el_cf, cs, n_wo
      integer mem_mgr_rt_cod
 
c      r_cf =    4*i_wo_z
c      c_cf =    3*i_wo_z
c      a_el_cf =   i_wo_z
c      cs =      9*i_wo_z
c      n_wo = i_wo_z*(
c     &     (1+n_r) + (1+n_c) + (1+n_r) + (1+n_c) +
c     &     (1+n_a_el) + (1+n_r+1) + (1+n_r) + (1+n_c))
c      if (n_wo .ne. r_cf*n_r + c_cf*n_c + a_el_cf*n_a_el + cs)
c     &     go to 8000
      call ems_g_ltssf_blk_crsh_n_wo(r_cf, c_cf, a_el_cf, cs, n_wo)
      call ems_mem_mgr_ope_blk(mem_mgr_rt_cod, is,
     &     n_wo, ope_blk_at_l_fr_p,
     &     cu_ml_n, crsh_blk_id, blk_crsh)
      if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
         if (mem_mgr_rt_cod .eq. mem_mgr_rt_cod_serious_no_po) then
            ems_msg_cod = ems_msg_lvl_serious
            go to 7000
         else if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
             ems_msg_cod = ems_msg_lvl_serious
            go to 7000
         endif
      endif
      call ems_mem_mgr_rp_ope_blk(
     &     mem_mgr_rt_cod, is, -1,
     &     r_cf, n_r,
     &     c_cf, n_c,
     &     a_el_cf, n_a_el,
     &     cs, n_wo, crsh_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         go to 7000
      endif
c
c     Get handles for the arrays within the block.
c
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_crsh, 1+n_r,  i_wo_z,
     &     hdl_crsh_r_ty)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_crsh, 1+n_c,  i_wo_z,
     &     hdl_crsh_c_ty)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_crsh, 1+n_r,  i_wo_z,
     &     hdl_crsh_r_n_act_en)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_crsh, 1+n_c,  i_wo_z,
     &     hdl_crsh_c_n_act_en)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_crsh, 1+n_a_el,  i_wo_z,
     &     hdl_crsh_mtx_c_ix)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_crsh, 1+n_r+1,  i_wo_z,
     &     hdl_crsh_mtx_r_sa)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_crsh, 1+n_r,  i_wo_z,
     &     hdl_crsh_r_st)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_crsh, 1+n_c,  i_wo_z,
     &     hdl_crsh_c_st)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
 
      call ems_mem_mgr_g_p4(mem_mgr_rt_cod, is,
     &     hdl_crsh_r_ty, p_crsh_r_ty)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_g_p4(mem_mgr_rt_cod, is,
     &     hdl_crsh_c_ty, p_crsh_c_ty)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_g_p4(mem_mgr_rt_cod, is,
     &     hdl_crsh_r_n_act_en, p_crsh_r_n_act_en)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_g_p4(mem_mgr_rt_cod, is,
     &     hdl_crsh_c_n_act_en, p_crsh_c_n_act_en)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_g_p4(mem_mgr_rt_cod, is,
     &     hdl_crsh_mtx_c_ix, p_crsh_mtx_c_ix)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_g_p4(mem_mgr_rt_cod, is,
     &     hdl_crsh_mtx_r_sa, p_crsh_mtx_r_sa)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_g_p4(mem_mgr_rt_cod, is,
     &     hdl_crsh_r_st, p_crsh_r_st)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_g_p4(mem_mgr_rt_cod, is,
     &     hdl_crsh_c_st, p_crsh_c_st)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
 
 7000 continue
      return
c 8000 continue
c      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)n_wo,
c     &     r_cf*n_r + c_cf*n_c + a_el_cf*n_a_el + cs
c      call ems_msg_wr_li(bug_msg_n)
c      goto 7000
c 9800 format('n_wo = ', i9, ' does not match ', i9,
c     &     ' from r/c/a_el_cf, n_r/c/a_el and cs')
      end
 
C->>> ------------------------------------> ems_g_ltsf_blk_crsh_n_wo <<<
      subroutine ems_g_ltsf_blk_crsh_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      integer r_cf, c_cf, a_el_cf, cs, n_wo
      r_cf =    4*i_wo_z
      c_cf =    3*i_wo_z
      a_el_cf =   i_wo_z
      cs =      9*i_wo_z
      n_wo = r_cf*n_r + c_cf*n_c + a_el_cf*n_a_el + cs
      return
      end
 
C->>> ---------------------------------------------> ems_rm_blk_crsh <<<
c     Removes the block for LTS(S)F crash
c
      subroutine ems_rm_blk_crsh(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'CRASH.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer mem_mgr_rt_cod
 
      call ems_mem_mgr_rm_blk(mem_mgr_rt_cod, is,
     &     blk_crsh)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         goto 7100
      endif
c 7000 continue
 7100 continue
      return
      end
 
