C->>> ---------------------------------------------------> ems_btran <<<
c
      subroutine ems_btran(rhs_v, rhs_ix, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'SLAPCS.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMICS.INC'
CM      IF (emsol_da .EQ. 1) THEN
C?      include 'EMSDA.INC'
C?      include 'TRANDA.INC'
CM      ENDIF
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      integer rhs_ix(0:n_r), is(0:is_n_en_m1)
      double precision rhs_v(0:n_r), ds(0:ds_n_en_m1)
      integer p_eta_fi_p_a
      integer p_eta_grp, p_eta_v, p_eta_ix, p_eta_rec, p_eta_sa
      integer eta_grp_n, eta_se_ty, eta_se_n_eta
      integer eta_se_n_el
      integer eta_sa_0, eta_ix_0
      integer eta_se_n
      integer u_bs_dse_blk_dim
      logical applied_all_u_etas
      logical tran_sps_rhs
      logical alw_f7_wr, er_fd
      integer btran_ty
      integer btran_ty_unit_rhs
      integer btran_ty_sps_rhs
      integer btran_ty_dse_rhs
      parameter (btran_ty_unit_rhs = 0)
      parameter (btran_ty_sps_rhs = 1)
      parameter (btran_ty_dse_rhs = 2)
CM      IF (emsol_da .EQ. 1) THEN
C?      integer i_wk_a_ix
CM      ENDIF
 
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_btran_lvl0) call ems_tt_rec(btran_tt, n_bs)
CM      ENDIF
      alw_f7_wr = ems_msg_no_prt_fm .ge. 1
      if (rhs_ix(0) .eq. 1) then
         btran_ty = btran_ty_unit_rhs
      else if (rhs_ix(0) .le. n_r) then
         btran_ty = btran_ty_sps_rhs
      else
         btran_ty = btran_ty_dse_rhs
      endif
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (btran_ty .eq. btran_ty_unit_rhs) then
C?         if (ems_tt_btran_lvl1) call ems_tt_rec(btran_unit_rhs_tt, n_bs)
C?      else if (btran_ty .eq. btran_ty_sps_rhs) then
C?         if (ems_tt_btran_lvl1) call ems_tt_rec(btran_sps_rhs_tt, n_bs)
C?      else
C?         if (ems_tt_btran_lvl1) call ems_tt_rec(btran_dse_rhs_tt, n_bs)
C?      endif
CM      ENDIF
CM      IF (emsol_da .EQ. 1) THEN
C?      if (btran_ty .eq. btran_ty_unit_rhs) then
C?         call ems_iz_tran_da(tran_da_tran_ty_btran_unit_rhs)
C?      else if (btran_ty .eq. btran_ty_sps_rhs) then
C?         call ems_iz_tran_da(tran_da_tran_ty_btran_sps_rhs)
C?      else
C?         call ems_iz_tran_da(tran_da_tran_ty_ftran_dse_rhs)
C?      endif
C?      call ems_g_rsmi_i_wk_a_ix(i_wk_a_ix)
C?      if (i_wk_a_ix .lt. 0) goto 8010
C?      call ems_u_tran_rhs_da(tran_da_loc_bf_tran,
C?     &     rhs_v, rhs_ix, is(p_rsmi_i_wk_a(i_wk_a_ix)))
CM      ENDIF
      tran_sps_rhs = rhs_ix(0) .lt. bwd_tran_dse_rhs_n_r
      if (tran_sps_rhs) then
c
c     Don't apply any etas if the RHS is a zero vector.
c
         if (rhs_ix(0) .eq. 0) go to 2000
c
c     Treat the RHS as a sparse vector.
c
      else if (rhs_ix(0) .le. n_r) then
c
c     The indices have been supplied but the RHS is not sufficiently
c     sparse for a list to be maintained so indicate that the list of
c     indices is to be ignored.
c
         rhs_ix(0) = n_r + 1
      end if
CM      IF (emsol_da .EQ. 1) THEN
C?c      tot_n_bwd_tran = tot_n_bwd_tran + 1
C?c      if (tran_sps_rhs) tot_n_sps_btran = tot_n_sps_btran + 1
CM      ENDIF
      applied_all_u_etas = .false.
      if (u_bs .eq. u_bs_pf_r_cp .and. n_u .gt. 0) then
         if (rhs_ix(0) .eq. 1) then
            if (is(p_u_bs_gthr_pv_r+rhs_ix(1)) .gt. 0) then
c
c     If there is a row copy of the pivotal rows in UPDATE etas and the
c     only nonzero in the RHS is in a pivotal row then use the block to
c     apply the UPDATE etas.
c
CM      IF (emsol_tt .EQ. 1) THEN
C?               if (ems_tt_btran_lvl2)
C?     &              call ems_tt_rec(btran_unit_rhs_dse_blk_tt, n_bs)
CM      ENDIF
               u_bs_dse_blk_dim = is(p_u_bs_skt_pv_r)
               call ems_btran_dse_blk_pf_u(
     &              u_bs_dse_blk_dim,
     &              ds(p_u_bs_dse_blk),
     &              is(p_u_bs_gthr_pv_r),
     &              is(p_u_bs_dse_blk_pv_r_in_c),
     &              is(p_u_bs_skt_pv_r),
     &              is(p_u_bs_eta_msk),
     &              rhs_v, rhs_ix)
CM      IF (emsol_tt .EQ. 1) THEN
C?               if (ems_tt_btran_lvl2)
C?     &              call ems_tt_rec(-btran_unit_rhs_dse_blk_tt, n_bs)
CM      ENDIF
c
c     All the UPDATE etas have been applied so set eta_grp/se_n to
c     correspond to the INVERT etas.
c
               applied_all_u_etas = .true.
            endif
         endif
      endif
      if (applied_all_u_etas) then
         eta_grp_n = eta_fi_n_inv_grp
         eta_se_n = eta_fi_n_inv_se
      else
         eta_grp_n = eta_fi_n_grp
         eta_se_n = eta_fi_n_se
      endif
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_btran_lvl2) then
C?         if (eta_se_n .gt. eta_fi_n_inv_se) then
C?c
C?c     There is at least one set of UPDATE etas to apply so start
C?c     timing them.
C?c
C?            if (btran_ty .eq. btran_ty_unit_rhs) then
C?               call ems_tt_rec(btran_unit_rhs_u_eta_tt, n_bs)
C?            else if (btran_ty .eq. btran_ty_sps_rhs) then
C?               call ems_tt_rec(btran_sps_rhs_u_eta_tt, n_bs)
C?            else
C?               call ems_tt_rec(btran_dse_rhs_u_eta_tt, n_bs)
C?            end if
C?         end if
C?      endif
CM      ENDIF
      if (eta_grp_n .eq. 0) goto 2000
      do 10, eta_grp_n = eta_grp_n, 1, -1
         p_eta_fi_p_a = (eta_grp_n-1)*eta_fi_p_a_rec_z
         p_eta_grp = eta_fi_p_a(p_eta_fi_p_a+eta_fi_p_a_os_p_eta_grp)
         p_eta_v =   eta_fi_p_a(p_eta_fi_p_a+eta_fi_p_a_os_p_eta_v)
         p_eta_ix =  eta_fi_p_a(p_eta_fi_p_a+eta_fi_p_a_os_p_eta_ix)
         p_eta_rec = eta_fi_p_a(p_eta_fi_p_a+eta_fi_p_a_os_p_eta_rec)
         p_eta_rec = p_eta_rec + is(p_eta_grp+eta_grp_os_n_rec) - 1
c
c     ?? Coded for eta groups containing multiple eta sets but not
c     tested.
c
 2       continue
         eta_se_ty = is(p_eta_rec+eta_se_rec_bwd_os_eta_ty)
         if (eta_se_ty .eq. no_eta_se_ty) goto 10
         eta_se_n_eta = is(p_eta_rec+eta_se_rec_bwd_os_n_eta)
         p_eta_rec = p_eta_rec -
     &        (eta_se_rec_z + eta_se_n_eta + 1 + eta_se_rec_z)
c
c     If all UPDATE etas have been applied then just have to skip
c     through the eta records until the first INVERT eta group is found.
c
         if (applied_all_u_etas .and. eta_se_ty .ne. inv_eta_se_ty)
     &        goto 2
         p_eta_sa = p_eta_rec + eta_se_rec_os_ze_sa_en
         eta_se_n_el = is(p_eta_sa+eta_se_n_eta+1) - 1
CM      IF (emsol_da .EQ. 1) THEN
C?c         bwd_tran_tot_n_eta = bwd_tran_tot_n_eta + eta_se_n_eta
C?c         if (tran_sps_rhs) sps_bwd_tran_tot_n_eta =
C?c     &         sps_bwd_tran_tot_n_eta + eta_se_n_eta
CM      ENDIF
         if (eta_se_n .eq. eta_fi_n_inv_se) then
CM      IF (emsol_da .EQ. 1) THEN
C?c            call ems_u_btran_rhs_da(.true., n_r, rhs_v, rhs_ix)
CM      ENDIF
CM      IF (emsol_tt .EQ. 1) THEN
C?            if (ems_tt_btran_lvl2) then
C?               if (eta_se_n .eq. eta_fi_n_inv_se) then
C?c
C?c     Start timing the INVERT etas.
C?c
C?                  if (btran_ty .eq. btran_ty_unit_rhs) then
C?                     call ems_tt_rec(btran_unit_rhs_inv_eta_tt, n_bs)
C?                  else if (btran_ty .eq. btran_ty_sps_rhs) then
C?                     call ems_tt_rec(btran_sps_rhs_inv_eta_tt, n_bs)
C?                  else
C?                     call ems_tt_rec(btran_dse_rhs_inv_eta_tt, n_bs)
C?                  endif
C?               endif
C?            endif
CM      ENDIF
CM      IF (emsol_da .EQ. 1) THEN
C?            call ems_u_tran_rhs_da(tran_da_loc_af_u_eta,
C?     &           rhs_v, rhs_ix, is(p_rsmi_i_wk_a(i_wk_a_ix)))
C?            call ems_u_tran_op_da(tran_da_eta_ty_u)
CM      ENDIF
c
c     Apply any U-etas
c
            if (n_lo_c_eta .lt. eta_se_n_eta) then
CM      IF (emsol_tt .EQ. 1) THEN
C?               if (ems_tt_btran_lvl3) then
C?c
C?c     Start timing the INVERT U-etas.
C?c
C?                  if (btran_ty .eq. btran_ty_unit_rhs) then
C?                     call ems_tt_rec(btran_unit_rhs_up_eta_tt, n_bs)
C?                  else if (btran_ty .eq. btran_ty_sps_rhs) then
C?                     call ems_tt_rec(btran_sps_rhs_up_eta_tt, n_bs)
C?                  else
C?                     call ems_tt_rec(btran_dse_rhs_up_eta_tt, n_bs)
C?                  endif
C?               endif
CM      ENDIF
               if (iand(eta_fi_da_st_msk, eta_fi_da_st_r_eta) .ne. 0
     &              ) then
                  call ems_fwd_tran_eta_se(
     &                 alw_f7_wr, ems_msg_wr_cn, er_fd,
     &                 1, n_up_r_eta, n_up_r_eta_el,
     &                 ds(p_up_eta_c_v),
     &                 is(p_up_eta_c_ix),
     &                 is(p_up_eta_r_sa),
     &                 is(p_up_eta_pv_in_c),
     &                 rhs_v, rhs_ix)
               else if (iand(eta_fi_da_st_msk, eta_fi_da_st_bwd_p)
     &                 .ne. 0) then
                  is(p_eta_w_lm1_en_in_r) = is(p_eta_w_l_en_in_r)
                  call ems_bwd_tran_eta_se(
     &                 n_lo_c_eta+1, eta_se_n_eta, eta_se_n_el,
     &                 ds(p_eta_v),
     &                 is(p_eta_ix),
     &                 is(p_eta_sa),
     &                 is(p_eta_w_l_en_in_r),
     &                 is(p_eta_w_lm1_en_in_r),
     &                 rhs_v, rhs_ix)
               else
                  call ems_bwd_tran_eta_se(
     &                 n_lo_c_eta+1, eta_se_n_eta, eta_se_n_el,
     &                 ds(p_eta_v),
     &                 is(p_eta_ix),
     &                 is(p_eta_sa),
     &                 -1, -1,
     &                 rhs_v, rhs_ix)
               endif
CM      IF (emsol_tt .EQ. 1) THEN
C?               if (ems_tt_btran_lvl3) then
C?c
C?c     Stop timing the INVERT U-etas.
C?c
C?                  if (btran_ty .eq. btran_ty_unit_rhs) then
C?                     call ems_tt_rec(-btran_unit_rhs_up_eta_tt, n_bs)
C?                  else if (btran_ty .eq. btran_ty_sps_rhs) then
C?                     call ems_tt_rec(-btran_sps_rhs_up_eta_tt, n_bs)
C?                  else
C?                     call ems_tt_rec(-btran_dse_rhs_up_eta_tt, n_bs)
C?                  endif
C?               endif
CM      ENDIF
CM      IF (emsol_da .EQ. 1) THEN
C?               call ems_u_tran_op_da(tran_da_eta_ty_up)
C?               call ems_u_tran_rhs_da(tran_da_loc_af_up_eta,
C?     &              rhs_v, rhs_ix, is(p_rsmi_i_wk_a(i_wk_a_ix)))
CM      ENDIF
            endif
c
c     Apply any L-etas
c
            if (n_lo_c_eta .gt. 0) then
CM      IF (emsol_tt .EQ. 1) THEN
C?               if (ems_tt_btran_lvl3) then
C?c
C?c     Start timing the INVERT L-etas.
C?c
C?                  if (btran_ty .eq. btran_ty_unit_rhs) then
C?                     call ems_tt_rec(btran_unit_rhs_lo_eta_tt, n_bs)
C?                  else if (btran_ty .eq. btran_ty_sps_rhs) then
C?                     call ems_tt_rec(btran_sps_rhs_lo_eta_tt, n_bs)
C?                  else
C?                     call ems_tt_rec(btran_dse_rhs_lo_eta_tt, n_bs)
C?                  endif
C?               endif
CM      ENDIF
               if (iand(eta_fi_da_st_msk, eta_fi_da_st_r_eta) .ne. 0
     &              ) then
                  call ems_fwd_tran_eta_se(
     &                 alw_f7_wr, ems_msg_wr_cn, er_fd,
     &                 1, n_lo_r_eta, n_lo_r_eta_el,
     &                 ds(p_lo_eta_c_v),
     &                 is(p_lo_eta_c_ix),
     &                 is(p_lo_eta_r_sa),
     &                 is(p_lo_eta_pv_in_c),
     &                 rhs_v, rhs_ix)
               else if (iand(eta_fi_da_st_msk, eta_fi_da_st_bwd_p)
     &                 .ne. 0) then
                  call ems_bwd_tran_eta_se(
     &                 1, n_lo_c_eta, eta_se_n_el,
     &                 ds(p_eta_v),
     &                 is(p_eta_ix),
     &                 is(p_eta_sa),
     &                 is(p_eta_w_l_en_in_r),
     &                 is(p_eta_w_lm1_en_in_r),
     &                 rhs_v, rhs_ix)
               else
                  call ems_bwd_tran_eta_se(
     &                 1, n_lo_c_eta, eta_se_n_el,
     &                 ds(p_eta_v),
     &                 is(p_eta_ix),
     &                 is(p_eta_sa),
     &                 -1, -1,
     &                 rhs_v, rhs_ix)
               endif
CM      IF (emsol_tt .EQ. 1) THEN
C?               if (ems_tt_btran_lvl3) then
C?c
C?c     Stop timing the INVERT L-etas.
C?c
C?                  if (btran_ty .eq. btran_ty_unit_rhs) then
C?                     call ems_tt_rec(-btran_unit_rhs_lo_eta_tt, n_bs)
C?                  else if (btran_ty .eq. btran_ty_sps_rhs) then
C?                     call ems_tt_rec(-btran_sps_rhs_lo_eta_tt, n_bs)
C?                  else
C?                     call ems_tt_rec(-btran_dse_rhs_lo_eta_tt, n_bs)
C?                  endif
C?               endif
CM      ENDIF
            endif
CM      IF (emsol_tt .EQ. 1) THEN
C?            if (ems_tt_btran_lvl2) then
C?c
C?c     Stop timing INVERT etas.
C?c
C?               if (btran_ty .eq. btran_ty_unit_rhs) then
C?                  call ems_tt_rec(-btran_unit_rhs_inv_eta_tt, n_bs)
C?               else if (btran_ty .eq. btran_ty_sps_rhs) then
C?                  call ems_tt_rec(-btran_sps_rhs_inv_eta_tt, n_bs)
C?               else
C?                  call ems_tt_rec(-btran_dse_rhs_inv_eta_tt, n_bs)
C?               endif
C?            endif
CM      ENDIF
CM      IF (emsol_da .EQ. 1) THEN
C?c            call ems_u_btran_rhs_da(.false., n_r, rhs_v, rhs_ix)
CM      ENDIF
         else if (iand(asm_msk, asm_btran) .eq. 0 .or.
     &           tran_sps_rhs) then
            call ems_bwd_tran_eta_se(
     &           1, eta_se_n_eta, eta_se_n_el,
     &           ds(p_eta_v),
     &           is(p_eta_ix),
     &           is(p_eta_sa),
     &           -1, -1,
     &           rhs_v, rhs_ix)
         else
            eta_se_n_el = is(p_eta_sa+eta_se_n_eta+1)-1
            eta_sa_0 = is(p_eta_sa)
c
c           make the "previous" eta 1 long.
c
            is(p_eta_sa) = is(p_eta_sa+1)-1
c
c           make this "previous" eta have a zero pivot row.
c
            eta_ix_0 = is(p_eta_ix+is(p_eta_sa))
            is(p_eta_ix+is(p_eta_sa)) = 0
CM      IF (emsol_asm .EQ. 1) THEN
C?c
C?c     ?? Does this need eta_se_n_el/f_el_n.
C?c     Could it ever be called for a partial eta group and would any
C?c     changes have to be made
C?c
C?               call ems_apply_eta_bwd(
C?     &              eta_se_n_eta,
C?     &              is(p_eta_sa), is(p_eta_ix), ds(p_eta_v),
C?     &              rhs_v)
CM      ELSE
            call ems_bwd_tran_eta_se(
     &           1, eta_se_n_eta, eta_se_n_el,
     &           ds(p_eta_v),
     &           is(p_eta_ix),
     &           is(p_eta_sa),
     &           -1, -1,
     &           rhs_v, rhs_ix)
CM      ENDIF
            is(p_eta_ix+is(p_eta_sa)) = eta_ix_0
            is(p_eta_sa) = eta_sa_0
         endif
         eta_se_n = eta_se_n - 1
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_btran_lvl2) then
C?            if (eta_se_n .eq. eta_fi_n_inv_se) then
C?c
C?c     All the UPDATE etas have been applied so stop timing them.
C?c
C?               if (btran_ty .eq. btran_ty_unit_rhs) then
C?                  call ems_tt_rec(-btran_unit_rhs_u_eta_tt, n_bs)
C?               else if (btran_ty .eq. btran_ty_sps_rhs) then
C?                  call ems_tt_rec(-btran_sps_rhs_u_eta_tt, n_bs)
C?               else
C?                  call ems_tt_rec(-btran_dse_rhs_u_eta_tt, n_bs)
C?               endif
C?            endif
C?         endif
CM      ENDIF
         goto 2
 10   continue
CM      IF (emsol_da .EQ. 1) THEN
C?      call ems_u_tran_op_da(tran_da_eta_ty_lo)
C?      call ems_u_tran_rhs_da(tran_da_loc_af_lo_eta,
C?     &     rhs_v, rhs_ix, is(p_rsmi_i_wk_a(i_wk_a_ix)))
CM      ENDIF
 2000 continue
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_btran_lvl1) then
C?         if (btran_ty .eq. btran_ty_unit_rhs) then
C?            call ems_tt_rec(-btran_unit_rhs_tt, n_bs)
C?         else if (btran_ty .eq. btran_ty_sps_rhs) then
C?            call ems_tt_rec(-btran_sps_rhs_tt, n_bs)
C?         else
C?            call ems_tt_rec(-btran_dse_rhs_tt, n_bs)
C?         end if
C?      endif
C?      if (ems_tt_btran_lvl0) call ems_tt_rec(-btran_tt, n_bs)
CM      ENDIF
CM      IF (emsol_da .EQ. 1) THEN
C?      call ems_fr_rsmi_i_wk_a_ix(i_wk_a_ix)
C? 7000 continue
CM      ENDIF
      return
CM      IF (emsol_da .EQ. 1) THEN
C? 8010 continue
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)
C?      call ems_msg_wr_li(bug_msg_n)
C?      goto 7000
C? 9801 format('RSMI workspace not available in ems_ftran')
CM      ENDIF
      end
 
C->>> ---------------------------------------------------> ems_ftran <<<
c     Warning: Following FTRAN with a packed RHS, the indexing may be
c     incorrect since there may be pointers to zero entries (due to
c     cancellation) or, more seriously, more than one pointer to the
c     same nonzero entry (due to cancellation zeros becoming nonzero).
c     Call g_pk_vec to correct the indexing and pack the values into a
c     parallel array.
c
      subroutine ems_ftran(rhs_v, rhs_ix, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'SLAPCS.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
CM      IF (emsol_da .EQ. 1) THEN
C?      include 'EMSDA.INC'
C?      include 'TRANDA.INC'
CM      ENDIF
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      integer rhs_ix(0:n_r), is(0:is_n_en_m1)
      double precision rhs_v(0:n_r), ds(0:ds_n_en_m1)
      integer p_eta_fi_p_a
      integer p_eta_grp, p_eta_v, p_eta_ix, p_eta_rec, p_eta_sa
      integer eta_grp_n, eta_se_ty, eta_se_n_eta
      integer eta_se_n
      integer eta_se_n_el
      integer sv_ix, sv_sa
      logical tran_sps_rhs
      logical alw_f7_wr, er_fd
CM      IF (emsol_tt .EQ. 1) THEN
C?      logical sa_u_tt
CM      ENDIF
      integer ftran_ty
      integer ftran_ty_sps_rhs
      integer ftran_ty_dse_rhs
      parameter (ftran_ty_sps_rhs = 1)
      parameter (ftran_ty_dse_rhs = 2)
      double precision sv_rhs_v
CM      IF (emsol_asm .EQ. 1) THEN
C?      integer ti_1, ti_2
CM      ENDIF
CM      IF (emsol_da .EQ. 1) THEN
C?      integer i_wk_a_ix
CM      ENDIF
 
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_ftran_lvl0) call ems_tt_rec(ftran_tt, n_bs)
CM      ENDIF
      alw_f7_wr = ems_msg_no_prt_fm .ge. 1
      if (rhs_ix(0) .le. n_r) then
         ftran_ty = ftran_ty_sps_rhs
      else
         ftran_ty = ftran_ty_dse_rhs
      endif
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_ftran_lvl1) then
C?         if (ftran_ty .eq. ftran_ty_sps_rhs) then
C?            call ems_tt_rec(ftran_sps_rhs_tt, n_bs)
C?         else
C?            call ems_tt_rec(ftran_dse_rhs_tt, n_bs)
C?         endif
C?      endif
CM      ENDIF
CM      IF (emsol_da .EQ. 1) THEN
C?      if (ftran_ty .eq. ftran_ty_sps_rhs) then
C?         call ems_iz_tran_da(tran_da_tran_ty_ftran_sps_rhs)
C?      else
C?         call ems_iz_tran_da(tran_da_tran_ty_ftran_dse_rhs)
C?      endif
C?      call ems_g_rsmi_i_wk_a_ix(i_wk_a_ix)
C?      if (i_wk_a_ix .lt. 0) goto 8010
C?      call ems_u_tran_rhs_da(tran_da_loc_bf_tran,
C?     &     rhs_v, rhs_ix, is(p_rsmi_i_wk_a(i_wk_a_ix)))
CM      ENDIF
      tran_sps_rhs = rhs_ix(0) .lt. fwd_tran_dse_rhs_n_r
      if (tran_sps_rhs) then
c
c     Don't apply any etas if the RHS is a zero vector.
c
         if (rhs_ix(0) .eq. 0) go to 2000
c
c     Maintain a list of indices of (possible) nonzeros in the RHS.
c
      else if (rhs_ix(0) .le. n_r) then
c
c     The indices have been supplied but the RHS is not sufficiently
c     sparse for a list to be maintained so indicate that the list of
c     indices is to be ignored.
c
         rhs_ix(0) = n_r + 1
      end if
CM      IF (emsol_da .EQ. 1) THEN
C?c      tot_n_ftran = tot_n_ftran + 1
C?c      if (tran_sps_rhs) tot_n_sps_ftran = tot_n_sps_ftran + 1
CM      ENDIF
      eta_se_n = 0
      do 10, eta_grp_n = 1, eta_fi_n_grp
c
c     If the RHS becomes dense then stop maintaining the indices of its
c     nonzeros.
c
         p_eta_fi_p_a = (eta_grp_n-1)*eta_fi_p_a_rec_z
         p_eta_grp = eta_fi_p_a(p_eta_fi_p_a+eta_fi_p_a_os_p_eta_grp)
         p_eta_v = eta_fi_p_a(p_eta_fi_p_a+eta_fi_p_a_os_p_eta_v)
         p_eta_ix = eta_fi_p_a(p_eta_fi_p_a+eta_fi_p_a_os_p_eta_ix)
         p_eta_rec = eta_fi_p_a(p_eta_fi_p_a+eta_fi_p_a_os_p_eta_rec)
c
c     ?? Coded for eta groups containing multiple eta sets but not
c     tested.
c
 2       continue
         eta_se_ty = is(p_eta_rec+eta_se_rec_os_eta_ty)
         if (eta_se_ty .eq. no_eta_se_ty) goto 10
         eta_se_n = eta_se_n + 1
         eta_se_n_eta = is(p_eta_rec+eta_se_rec_os_n_eta)
         p_eta_sa = p_eta_rec + eta_se_rec_os_ze_sa_en
         eta_se_n_el = is(p_eta_sa+eta_se_n_eta+1)-1
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_ftran_lvl2) then
C?            if (eta_se_n .eq. eta_fi_n_inv_se+1) then
C?               if (ftran_ty .eq. ftran_ty_sps_rhs) then
C?                  call ems_tt_rec(ftran_sps_rhs_u_eta_tt, n_bs)
C?               else
C?                  call ems_tt_rec(ftran_dse_rhs_u_eta_tt, n_bs)
C?               endif
C?               sa_u_tt = .true.
C?            endif
C?         endif
CM      ENDIF
CM      IF (emsol_da .EQ. 1) THEN
C?c         ftran_tot_n_eta = ftran_tot_n_eta + eta_se_n_eta
C?c         if (tran_sps_rhs)
C?c     &        sps_ftran_tot_n_eta = sps_ftran_tot_n_eta + eta_se_n_eta
CM      ENDIF
         if (eta_se_n .eq. eta_fi_n_inv_se) then
CM      IF (emsol_tt .EQ. 1) THEN
C?            sa_u_tt = .false.
C?            if (ems_tt_ftran_lvl2) then
C?               if (ftran_ty .eq. ftran_ty_sps_rhs) then
C?                  call ems_tt_rec(ftran_sps_rhs_inv_eta_tt, n_bs)
C?               else
C?                  call ems_tt_rec(ftran_dse_rhs_inv_eta_tt, n_bs)
C?               endif
C?            endif
CM      ENDIF
c
c     Apply any L-etas
c
            if (n_lo_c_eta .gt. 0) then
CM      IF (emsol_tt .EQ. 1) THEN
C?c
C?c     Start timing the L-etas
C?c
C?               if (ems_tt_ftran_lvl3) then
C?                  if (ftran_ty .eq. ftran_ty_sps_rhs) then
C?                     call ems_tt_rec(ftran_sps_rhs_lo_eta_tt, n_bs)
C?                  else
C?                     call ems_tt_rec(ftran_dse_rhs_lo_eta_tt, n_bs)
C?                  endif
C?               endif
CM      ENDIF
               call ems_fwd_tran_eta_se(
     &              alw_f7_wr, ems_msg_wr_cn, er_fd,
     &              1, n_lo_c_eta, eta_se_n_el,
     &              ds(p_eta_v),
     &              is(p_eta_ix),
     &              is(p_eta_sa),
     &              is(p_lo_eta_pv_in_r),
     &              rhs_v, rhs_ix)
CM      IF (emsol_tt .EQ. 1) THEN
C?c
C?c     Stop timing the L-etas
C?c
C?               if (ems_tt_ftran_lvl3) then
C?                  if (ftran_ty .eq. ftran_ty_sps_rhs) then
C?                     call ems_tt_rec(-ftran_sps_rhs_lo_eta_tt, n_bs)
C?                  else
C?                     call ems_tt_rec(-ftran_dse_rhs_lo_eta_tt, n_bs)
C?                  endif
C?               endif
CM      ENDIF
CM      IF (emsol_da .EQ. 1) THEN
C?               call ems_u_tran_rhs_da(tran_da_loc_af_lo_eta,
C?     &              rhs_v, rhs_ix, is(p_rsmi_i_wk_a(i_wk_a_ix)))
C?               call ems_u_tran_op_da(tran_da_eta_ty_lo)
CM      ENDIF
            endif
c
c     Apply any U-etas
c
            if (n_lo_c_eta .lt. eta_se_n_eta) then
CM      IF (emsol_tt .EQ. 1) THEN
C?c
C?c     Start timing the U-etas
C?c
C?               if (ems_tt_ftran_lvl3) then
C?                  if (ftran_ty .eq. ftran_ty_sps_rhs) then
C?                     call ems_tt_rec(ftran_sps_rhs_up_eta_tt, n_bs)
C?                  else
C?                     call ems_tt_rec(ftran_dse_rhs_up_eta_tt, n_bs)
C?                  endif
C?               endif
CM      ENDIF
               call ems_fwd_tran_eta_se(
     &              alw_f7_wr, ems_msg_wr_cn, er_fd,
     &              n_lo_c_eta+1, eta_se_n_eta, eta_se_n_el,
     &              ds(p_eta_v),
     &              is(p_eta_ix),
     &              is(p_eta_sa),
     &              is(p_up_eta_pv_in_r),
     &              rhs_v, rhs_ix)
CM      IF (emsol_tt .EQ. 1) THEN
C?c
C?c     Stop timing the U-etas
C?c
C?               if (ems_tt_ftran_lvl3) then
C?                  if (ftran_ty .eq. ftran_ty_sps_rhs) then
C?                     call ems_tt_rec(-ftran_sps_rhs_up_eta_tt, n_bs)
C?                  else
C?                     call ems_tt_rec(-ftran_dse_rhs_up_eta_tt, n_bs)
C?                  endif
C?               endif
CM      ENDIF
CM      IF (emsol_da .EQ. 1) THEN
C?               call ems_u_tran_op_da(tran_da_eta_ty_up)
C?               call ems_u_tran_rhs_da(tran_da_loc_af_up_eta,
C?     &              rhs_v, rhs_ix, is(p_rsmi_i_wk_a(i_wk_a_ix)))
CM      ENDIF
            endif
CM      IF (emsol_tt .EQ. 1) THEN
C?            if (ems_tt_ftran_lvl2) then
C?               if (eta_se_n .eq. eta_fi_n_inv_se) then
C?                  if (ftran_ty .eq. ftran_ty_sps_rhs) then
C?                     call ems_tt_rec(-ftran_sps_rhs_inv_eta_tt, n_bs)
C?                  else
C?                     call ems_tt_rec(-ftran_dse_rhs_inv_eta_tt, n_bs)
C?                  endif
C?               endif
C?            endif
CM      ENDIF
CM      IF (emsol_da .EQ. 1) THEN
C?c            call ems_u_ftran_rhs_da(.false., n_r, rhs_v, rhs_ix)
CM      ENDIF
         else
            if (iand(asm_msk, asm_ftran) .eq. 0) then
               call ems_fwd_tran_eta_se(
     &              alw_f7_wr, ems_msg_wr_cn, er_fd,
     &              1, eta_se_n_eta, eta_se_n_el,
     &              ds(p_eta_v),
     &              is(p_eta_ix),
     &              is(p_eta_sa),
     &              -1,
     &              rhs_v, rhs_ix)
            else
               sv_ix = is(p_eta_ix+eta_se_n_el+1)
               is(p_eta_ix+eta_se_n_el+1) = 0
               sv_sa = is(p_eta_sa+eta_se_n_eta+2)
               is(p_eta_sa+eta_se_n_eta+2) = 0
               sv_rhs_v = rhs_v(0)
               rhs_v(0) = 1.0d0
CM      IF (emsol_asm .EQ. 1) THEN
C?               call ems_apply_eta_fwd(
C?     &              eta_se_n_eta,
C?     &              is(p_eta_sa), is(p_eta_ix), ds(p_eta_v),
C?     &              rhs_v, ti_1, ti_2 )
CM      ELSE
               call ems_fwd_tran_eta_se(
     &              alw_f7_wr, ems_msg_wr_cn, er_fd,
     &              1, eta_se_n_eta, eta_se_n_el,
     &              ds(p_eta_v),
     &              is(p_eta_ix),
     &              is(p_eta_sa),
     &              -1,
     &              rhs_v, rhs_ix)
CM      ENDIF
               is(p_eta_ix+eta_se_n_el+1) = sv_ix
               is(p_eta_sa+eta_se_n_eta+2) = sv_sa
               rhs_v(0) = sv_rhs_v
            endif
         endif
         p_eta_rec = p_eta_rec +
     &        (eta_se_rec_z + eta_se_n_eta + 1 + eta_se_rec_z)
         goto 2
 10   continue
CM      IF (emsol_da .EQ. 1) THEN
C?      call ems_u_tran_op_da(tran_da_eta_ty_u)
C?      call ems_u_tran_rhs_da(tran_da_loc_af_u_eta,
C?     &     rhs_v, rhs_ix, is(p_rsmi_i_wk_a(i_wk_a_ix)))
CM      ENDIF
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_ftran_lvl2) then
C?         if (sa_u_tt) then
C?            if (ftran_ty .eq. ftran_ty_sps_rhs) then
C?               call ems_tt_rec(-ftran_sps_rhs_u_eta_tt, n_bs)
C?            else
C?               call ems_tt_rec(-ftran_dse_rhs_u_eta_tt, n_bs)
C?            endif
C?         endif
C?      endif
CM      ENDIF
      if (.not. tran_sps_rhs) rhs_ix(0) = n_r + 1
 2000 continue
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_ftran_lvl1) then
C?         if (ftran_ty .eq. ftran_ty_sps_rhs) then
C?            call ems_tt_rec(-ftran_sps_rhs_tt, n_bs)
C?         else
C?            call ems_tt_rec(-ftran_dse_rhs_tt, n_bs)
C?         endif
C?      endif
C?      if (ems_tt_ftran_lvl0) call ems_tt_rec(-ftran_tt, n_bs)
CM      ENDIF
CM      IF (emsol_da .EQ. 1) THEN
C?      call ems_fr_rsmi_i_wk_a_ix(i_wk_a_ix)
C? 7000 continue
CM      ENDIF
      return
CM      IF (emsol_da .EQ. 1) THEN
C? 8010 continue
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)
C?      call ems_msg_wr_li(bug_msg_n)
C?      goto 7000
C? 9801 format('RSMI workspace not available in ems_btran')
CM      ENDIF
      end
 
C->>> --------------------------------------> ems_btran_dse_blk_pf_u <<<
c
      subroutine ems_btran_dse_blk_pf_u(
     &     dse_blk_dim,
     &     u_bs_dse_blk, u_bs_gthr_pv_r, u_bs_dse_blk_pv_r_in_c,
     &     u_bs_skt_pv_r, u_bs_eta_msk,
     &     rhs_v, rhs_ix)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
CM      IF (emsol_da .EQ. 1) THEN
C?      include 'EMSDA.INC'
CM      ENDIF
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      integer dse_blk_dim
      double precision u_bs_dse_blk(0:dse_blk_dim, 0:dse_blk_dim)
      double precision rhs_v(0:n_r)
      integer u_bs_gthr_pv_r(0:n_r)
      integer u_bs_dse_blk_pv_r_in_c(0:dse_blk_dim)
      integer u_bs_skt_pv_r(0:dse_blk_dim)
      integer u_bs_eta_msk(0:((dse_blk_dim-1)/dse_blk_u_sn_ln+1)*n_r)
      integer rhs_ix(0:n_r)
      integer dse_blk_n_r, dse_blk_n_c
      integer dse_blk_r_n, dse_blk_c_n, dse_blk_sn_c_n
      integer dse_blk_pv_r_n, r_n
      integer rhs_n_ix, rhs_ix_n, rhs_dse_blk_f_ix_n
      integer c_ls
      integer fm_dse_blk_c_n, t_dse_blk_c_n
      integer dse_blk_sn_n, dse_blk_sn_os
      double precision su
CM      IF (emsol_deb .EQ. 1) THEN
C?      integer wr_dse_blk_pf_da
C?      save wr_dse_blk_pf_da
C?      data wr_dse_blk_pf_da/0/
C?
C?      if (wr_dse_blk_pf_da .gt. 0) call ems_wr_dse_blk_pf_da(
C?     &     dse_blk_dim,
C?     &     u_bs_dse_blk, u_bs_gthr_pv_r,
C?     &     u_bs_dse_blk_pv_r_in_c, u_bs_skt_pv_r, u_bs_eta_msk)
CM      ENDIF
 
c      pv_r_n = rhs_ix(1)
      su = zero
      dse_blk_sn_n = (n_u-1)/dse_blk_u_sn_ln + 1
      dse_blk_sn_os = (dse_blk_sn_n-1)*n_r
      dse_blk_n_r = u_bs_gthr_pv_r(0)
      dse_blk_n_c = u_bs_dse_blk_pv_r_in_c(0)
c
c     Gather the nonzero(s) from the RHS---currently only copes with a
c     single nonzero
c
      rhs_n_ix = rhs_ix(0)
      if (rhs_n_ix .eq. 1) then
c
c     Only zeroed so that the scattering code does not assume that there
c     are only nonzeros in pivotal rows.
c
         rhs_ix(0) = 0
         r_n = rhs_ix(1)
         dse_blk_r_n = u_bs_gthr_pv_r(r_n)
         if (dse_blk_r_n .gt. 0) then
            u_bs_dse_blk(dse_blk_r_n, 0) = rhs_v(r_n)
            rhs_v(r_n) = zero
            rhs_ix(1) = dse_blk_r_n
         else
            goto 8000
         endif
      else
         goto 8000
      endif
      rhs_dse_blk_f_ix_n = 1
      dse_blk_c_n = dse_blk_n_c + 1
 10   continue
c
c     Form c_ls, a bit mask of the columns to be considered (in this
c     section.)
c
      c_ls = 0
      do 20, rhs_ix_n = rhs_dse_blk_f_ix_n, rhs_n_ix
         dse_blk_r_n = rhs_ix(rhs_ix_n)
         r_n = u_bs_skt_pv_r(dse_blk_r_n)
         c_ls = ior(c_ls, u_bs_eta_msk(dse_blk_sn_os+r_n))
 20   continue
      t_dse_blk_c_n = (dse_blk_sn_n-1)*dse_blk_u_sn_ln + 1
 100  continue
c
c     Determine the next eta to be applied.
c
      fm_dse_blk_c_n = dse_blk_c_n - 1
      dse_blk_sn_c_n = (fm_dse_blk_c_n-t_dse_blk_c_n+1) + 1
      do 110, dse_blk_c_n = fm_dse_blk_c_n, t_dse_blk_c_n, -1
         dse_blk_sn_c_n = dse_blk_sn_c_n - 1
         if (iand(c_ls, bt_a(dse_blk_sn_c_n)) .ne. 0) goto 120
 110  continue
c
c     No more etas to be applied in this section.
c
      if (dse_blk_sn_n .gt. 1) then
c
c     More sections so go back and apply the next one.
c
         dse_blk_sn_n = dse_blk_sn_n - 1
         dse_blk_sn_os = dse_blk_sn_os - n_r
         dse_blk_c_n = t_dse_blk_c_n
         goto 10
      else
c
c     No more sections so scatter solution.
c
         goto 200
      endif
 120  continue
      do 130, rhs_ix_n = rhs_dse_blk_f_ix_n, rhs_n_ix
         dse_blk_r_n = rhs_ix(rhs_ix_n)
         su = su + u_bs_dse_blk(dse_blk_r_n, dse_blk_c_n)*
     &        u_bs_dse_blk(dse_blk_r_n, 0)
 130  continue
c
c     The pivot is stored in entry zero of the dense block column.
c
      dse_blk_pv_r_n = u_bs_dse_blk_pv_r_in_c(dse_blk_c_n)
      if (su .eq. zero) then
         u_bs_dse_blk(dse_blk_pv_r_n, 0) =
     &        u_bs_dse_blk(0, dse_blk_c_n)*
     &        u_bs_dse_blk(dse_blk_pv_r_n, 0)
      else
         if (u_bs_dse_blk(dse_blk_pv_r_n, 0) .eq. zero) then
c
c     Fill-in in the RHS
c
            u_bs_dse_blk(dse_blk_pv_r_n, 0) =
     &           u_bs_dse_blk(0, dse_blk_c_n)*su
            rhs_n_ix = rhs_n_ix + 1
            rhs_ix(rhs_n_ix) = dse_blk_pv_r_n
            r_n = u_bs_skt_pv_r(dse_blk_pv_r_n)
            c_ls = ior(c_ls, u_bs_eta_msk(dse_blk_sn_os+r_n))
         else
            u_bs_dse_blk(dse_blk_pv_r_n, 0) =
     &           u_bs_dse_blk(0, dse_blk_c_n)*
     &           (u_bs_dse_blk(dse_blk_pv_r_n, 0)+su)
            if (abs(u_bs_dse_blk(dse_blk_pv_r_n, 0)) .le.
     &           bwd_tran_ze) then
c
c     Cancellation in the RHS
c
               u_bs_dse_blk(dse_blk_pv_r_n, 0) = zero
               do 140, rhs_ix_n = rhs_dse_blk_f_ix_n, rhs_n_ix
                  if (rhs_ix(rhs_ix_n) .eq. dse_blk_pv_r_n) then
                     rhs_ix(rhs_ix_n) = rhs_ix(rhs_n_ix)
                     rhs_n_ix = rhs_n_ix - 1
                     goto 150
                  endif
 140           continue
 150           continue
            endif
         endif
         su = zero
      endif
      goto 100
c
c     Scatter the nonzero values.
c
 200  continue
      do 210, rhs_ix_n = rhs_dse_blk_f_ix_n, rhs_n_ix
         dse_blk_r_n = rhs_ix(rhs_ix_n)
         r_n = u_bs_skt_pv_r(dse_blk_r_n)
         rhs_ix(0) = rhs_ix(0) + 1
         rhs_ix(rhs_ix(0)) = r_n
         rhs_v(r_n) = u_bs_dse_blk(dse_blk_r_n, 0)
         u_bs_dse_blk(dse_blk_r_n, 0) = zero
 210  continue
 7000 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9800 format(
     &     'ERROR: Trying to use dense block for non-pivotal RHS index')
      end
 
C->>> ---------------------------------------------------> ems_g_rhs <<<
c     Assigns the RHS vector for a given variable number.
c
      subroutine ems_g_rhs(rhs_sgn, vr_n, rhs_v, rhs_ix, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMICS.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      integer rhs_sgn
      integer vr_n, rhs_ix(0:n_r), is(0:is_n_en_m1)
      double precision rhs_v(0:n_r), ds(0:ds_n_en_m1)
      integer el_n, r_n, n_ix
 
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tt_da .gt. 0) call ems_tt_rec(g_rhs_tt, n_bs)
CM      ENDIF
c
c     Check that the RHS is zeroed on entry
c
      if (iand(ck_msk, ze_a_ck_bt) .ne. 0)
     &     call ems_ck_ze_rl_a(n_r, rhs_v)
      if (rhs_ix(0) .lt. fwd_tran_dse_rhs_n_r) then
         if (iand(inv_alg_msk, inv_alg_perm) .eq. 0) then
c
c     If not permuting INVERT.
c
            if (vr_n .le. n_c) then
               n_ix = 0
               do 10, el_n = is(p_mtx_c_sa+vr_n),
     &              is(p_mtx_c_sa+vr_n+1)-1
                  r_n = is(p_mtx_r_ix+el_n)
                  rhs_v(r_n) = rhs_sgn*ds(p_mtx_r_v+el_n)
                  n_ix = n_ix + 1
                  rhs_ix(n_ix) = r_n
 10            continue
               rhs_ix(0) = n_ix
            else
               r_n = vr_n - mx_n_c
               rhs_v(r_n) = -rhs_sgn
               rhs_ix(0) = 1
               rhs_ix(1) = r_n
            end if
         else
            if (vr_n .le. n_c) then
               n_ix = 0
               do 20, el_n = is(p_mtx_c_sa+vr_n),
     &              is(p_mtx_c_sa+vr_n+1)-1
                  r_n = is(p_og_t_nw_perm+is(p_mtx_r_ix+el_n))
                  rhs_v(r_n) = rhs_sgn*ds(p_mtx_r_v+el_n)
                  n_ix = n_ix + 1
                  rhs_ix(n_ix) = r_n
 20            continue
               rhs_ix(0) = n_ix
            else
               r_n = is(p_og_t_nw_perm+vr_n-mx_n_c)
               rhs_v(r_n) = -rhs_sgn
               rhs_ix(0) = 1
               rhs_ix(1) = r_n
            end if
         endif
      else
         if (iand(inv_alg_msk, inv_alg_perm) .eq. 0) then
c
c     If not permuting INVERT.
c
            if (vr_n .le. n_c) then
               do 110, el_n = is(p_mtx_c_sa+vr_n),
     &              is(p_mtx_c_sa+vr_n+1)-1
                  r_n = is(p_mtx_r_ix+el_n)
                  rhs_v(r_n) = rhs_sgn*ds(p_mtx_r_v+el_n)
 110           continue
            else
               r_n = vr_n - mx_n_c
               rhs_v(r_n) = -rhs_sgn
            end if
         else
            if (vr_n .le. n_c) then
               do 120, el_n = is(p_mtx_c_sa+vr_n),
     &              is(p_mtx_c_sa+vr_n+1)-1
                  r_n = is(p_og_t_nw_perm+is(p_mtx_r_ix+el_n))
                  rhs_v(r_n) = rhs_sgn*ds(p_mtx_r_v+el_n)
 120           continue
            else
               r_n = is(p_og_t_nw_perm+vr_n-mx_n_c)
               rhs_v(r_n) = -rhs_sgn
            end if
         endif
      end if
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tt_da .gt. 0) call ems_tt_rec(-g_rhs_tt, n_bs)
CM      ENDIF
      return
      end
 
C->>> --------------------------------------------------> ems_g_bs_r <<<
c     Assigns a given row of the basis matrix which has been inverted.
c
      subroutine ems_g_bs_r(vr_in_r, g_bs_r_n, rhs_v, rhs_ix, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMICS.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer vr_in_r(0:n_r), g_bs_r_n, rhs_ix(0:n_r), is(0:is_n_en_m1)
      double precision rhs_v(0:n_r), ds(0:ds_n_en_m1)
      integer r_n, c_n, vr_n, el_n, n_rhs_el
c
c     Check that the RHS is zeroed on entry
c
      if (iand(ck_msk, ze_a_ck_bt) .ne. 0)
     &     call ems_ck_ze_rl_a(n_r, rhs_v)
      if (iand(inv_alg_msk, inv_alg_perm) .eq. 0) then
         r_n = g_bs_r_n
      else
         r_n = is(p_nw_t_og_perm+g_bs_r_n)
      endif
      if (rhs_ix(0) .lt. bwd_tran_dse_rhs_n_r) then
         n_rhs_el = 0
         do 20, c_n = 1, n_r
            rhs_v(c_n) = zero
            vr_n = vr_in_r(c_n)
            if (vr_n .le. n_c) then
               do 10, el_n = is(p_mtx_c_sa+vr_n),
     &              is(p_mtx_c_sa+vr_n+1)-1
                  if (is(p_mtx_r_ix+el_n) .eq. r_n) then
                     n_rhs_el = n_rhs_el + 1
                     rhs_v(c_n) = ds(p_mtx_r_v+el_n)
                     rhs_ix(n_rhs_el) = c_n
                     go to 20
                  end if
 10            continue
            else if (vr_n-mx_n_c .eq. r_n) then
               n_rhs_el = n_rhs_el + 1
               rhs_v(c_n) = -one
               rhs_ix(n_rhs_el) = c_n
            end if
 20      continue
         rhs_ix(0) = n_rhs_el
      else
         do 120, c_n = 1, n_r
            rhs_v(c_n) = zero
            vr_n = vr_in_r(c_n)
            if (vr_n .le. n_c) then
               do 110, el_n = is(p_mtx_c_sa+vr_n),
     &              is(p_mtx_c_sa+vr_n+1)-1
                  if (is(p_mtx_r_ix+el_n) .eq. r_n) then
                     rhs_v(c_n) = ds(p_mtx_r_v+el_n)
                     go to 120
                  end if
 110           continue
            else if (vr_n-mx_n_c .eq. r_n) then
               rhs_v(c_n) = -one
            end if
 120     continue
      endif
      return
      end
 
