CM
C->>> -----------------------------------------------------> ems_inv <<<
c     Calls the Tomlin or Markowitz INVERT according to the value
c     of inv_alg_msk.
c
      subroutine ems_inv(ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMIHDL.INC'
      include 'RSMICOM.INC'
      include 'RSMICS.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      include 'EMSMSGN.INC'
      include 'MORSMI.INC'
c
c     >>>> Only needed when dumping basis
c
c      include 'CHCTVR.INC'
c      integer ln_t_l_ch
c      character*80 ch80_txt
c      character*13 ch13_vers
c
c     Only needed when dumping basis <<<<
c
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_eq_i2
CM      ENDIF
      double precision ds(0:ds_n_en_m1)
      integer is(0:is_n_en_m1)
      integer p_eta_fi_hdl
      double precision ftran_er, btran_er
c      double precision rsdu_er
      integer rt_cod
 
c      integer sw_baso
c      sw_baso = 0
c      if (sw_baso .ne. 0) then
c         call ems_baso(rt_cod, ds, 26, 1)
c         if (rt_cod .ge. ems_msg_lvl_serious) then
c            ems_msg_cod = rt_cod
c            goto 7000
c         endif
c      endif
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_inv_lvl0) call ems_tt_rec(inv_tt, n_bs)
CM      ENDIF
      if (iand(inv_alg_msk, inv_alg_mwz) .ne. 0) goto 8000
      p_eta_fi_hdl = p_ml_bs_blk + (cu_ml_n-1)*ml_bs_blk_n_wo +
     &     ml_bs_blk_os_hdl+ix_eta_fi_hdl
c
c     >>>> Dump out the matrix
c
c      call ems_msg_g_ch13_vers(ch13_vers, ln_t_l_ch)
c      ch80_txt = 'EMSOL '//ch13_vers(1:ln_t_l_ch)
c     &     //': Model = '//ch_ml_nm
c     &     //': Basis = '
c      call ems_wr_mtx_w_bs(
c     &     ch80_txt, n_bs,
c     &     is(p_vr_in_r),
c     &     ds(p_mtx_r_v),
c     &     is(p_mtx_r_ix),
c     &     is(p_mtx_c_sa))
c
c     Dump out the matrix <<<<
c
c 1000 continue
      if (iand(inv_alg_msk, inv_alg_perm) .ne. 0) then
c
c     Copy the vr_in_r to be inverted so that the permutation can be
c     deduced. Use nw_t_og_perm as workspace. Then INVERT.
c
         call ems_ICOPY(n_r,
     &        is(p_vr_in_r+1), 1,
     &        is(p_nw_t_og_perm+1), 1)
         call ems_ca_tom_inv(
     &        is(p_nw_t_og_perm),
     &        is(p_eta_fi_hdl),
     &        ds, is)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      else
c
c     INVERT and then set the indexing into vr_in_r.
c
         call ems_ca_tom_inv(
     &        is(p_vr_in_r),
     &        is(p_eta_fi_hdl),
     &        ds, is)
         call ems_u_vr_in_r_ix(is(p_st), is(p_vr_in_r))
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      endif
c
c     The basis corresponding to vr_in_r has been inverted so zero the
c     number of updates.
c
      n_u = 0
c
c     Resolve any incomplete basis changes caused by singularity.
c
      if (u_bs_cg .gt. 0) call ems_u_ml_bs_cg(ds, is)
CM      IF (emsol_da .EQ. 1) THEN
C?      if (iand(inv_msg_msk, bt1) .ne. 0)
C?     &     call ems_wr_inv_da(inv_log_msk, ds, is)
CM      ENDIF
      call ems_g_inv_p(rt_cod, is)
      if (rt_cod .ne. 0) goto 8020
      if (iand(inv_alg_msk, inv_alg_perm) .ne. 0) then
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_inv_lvl1) call ems_tt_rec(perm_inv_tt, n_bs)
CM      ENDIF
c
c     Remember that nw_t_og_perm is the permuted vr_in_r that was
c     inverted.
c
         call ems_g_vr_in_r_perm(
     &        is(p_nw_t_og_perm),
     &        is(p_st),
     &        is(p_og_t_nw_perm))
         call ems_g_inv_o_perm(n_r,
     &        is(p_og_t_nw_perm),
     &        is(p_nw_t_og_perm))
         call ems_perm_inv(is(p_og_t_nw_perm), is)
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_inv_lvl1) call ems_tt_rec(-perm_inv_tt, n_bs)
CM      ENDIF
      endif
c     >>>>>>>>>>
c     Placed after `get perm' by JAJH on 07/07/97
c
      if (iand(ck_msk, inv_ck_bt) .ne. 0) then
         call ems_ca_rand_ck_inv(.true., ftran_er, btran_er, ds, is)
c         call ems_ck_inv(ds, is)
c         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      end if
      if (n_inv_sing .gt. 0) then
CM      IF (emsol_dev .EQ. 1) THEN
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?         if (ems_i1_eq_i2(ca_iz_mo_rsmi_fg1, ca_iz_mo_rsmi_fg2))
CM      ELSE
C?         if (ca_iz_mo_rsmi_fg1 .eq. ca_iz_mo_rsmi_fg2)
CM      ENDIF
C?     &        call ems_iz_mo_rsmi(11, 2, 0)
C?         call ems_mo_rsmi_sing_bs(n_si_it, n_inv_sing)
CM      ENDIF
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)n_inv_sing
         call ems_msg_wr_li(warn_msg_n)
c
c     If singularities have been detected then, if using steepest edge,
c     switch to Devex.
c
         if (pc_alg .eq. pc_alg_sed) then
            pc_alg = pc_alg_exact_dvx
            dvx_mode = pc_alg_exact_dvx
            if (iand(ml_blk_st_msk, ml_blk_st_dvx) .eq. 0) then
               call ems_iz_blk_dvx(is)
               if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
            endif
c
c     Initialise the counter of framework changes and report the change
c     in pricing method.
c
            n_dvx_fwk = 0
            if (iand(rsmi_msg_msk, rsmi_pc_li_bt) .ne. 0) then
               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9110)
               call ems_msg_wr_li(rsmi_msg_n)
            end if
         end if
c
c     Indicate that the following are not correct for the model:
c
c     vr_in_c, basic primal activities, edge weights and
c     row-wise representation of matrix columns being priced.
c
         ml_da_st_msk = ml_da_st_msk
     &        - iand(ml_da_st_msk, ml_da_st_vr_in_c)
     &        - iand(ml_da_st_msk, ml_da_st_bc_pr_act)
     &        - iand(ml_da_st_msk, ml_da_st_ed_wt)
     &        - iand(ml_da_st_msk, ml_da_st_r_mtx)
      end if
 7000 continue
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_inv_lvl0) call ems_tt_rec(-inv_tt, n_bs)
CM      ENDIF
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9100 format(i7, ' variables have been replaced by logical',
     &     ' variables due to singularity')
 9110 format('Switching from steepest edge to Devex',
     &     ' pricing strategy ')
 9800 format('Markowitz INVERT not available')
 9802 format('Error in ems_g_inv_p')
      end
 
C->>> --------------------------------------------> ems_u_vr_in_r_ix <<<
c     Set the indexing into vr_in_r.
c
      subroutine ems_u_vr_in_r_ix(st, vr_in_r)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer st(0:mx_n_c+n_r), vr_in_r(0:n_r)
      integer r_n, c_n, vr_n, f_ix_n, ix_n
c      character*30 ems_st_t_ch30
c      character*30 ch30
 
      if (n_inv_sing .eq. 0) then
         do 10, r_n = 1, n_r
            vr_n = vr_in_r(r_n)
            st(vr_n) = st(vr_n) - iand(st(vr_n), mx_mx_ml_a_dim) + r_n
 10      continue
      else
 
c         print*, '  ix_n      c_n           Decoded status'
c         do ix_n = 1, n_c+n_r
c            if (ix_n .le. n_c) then
c               c_n = ix_n
c            else
c               c_n = ix_n+mx_n_c-n_c
c            endif
c            ch30 = ems_st_t_ch30(st(c_n))
c            write(*, '(i7, 2x, i7, 3(2x, l1), 2x, a30)')ix_n, c_n,
c     &           iand(st(c_n), u_bs_cg_bt) .ne. 0,
c     &           iand(st(c_n), bc_bt) .eq. 0,
c     &           iand(st(c_n), u_bs_cg_bt) .ne. 0 .and.
c     &           iand(st(c_n), bc_bt) .eq. 0,
c     &           ch30
c         enddo
c         print*, '  ix_n     vr_n           Vr_in_r'
c         do r_n = 1, n_r
c            write(*, '(i7, 2x, i7)')r_n, vr_in_r(r_n)
c         enddo
 
         f_ix_n = 1
         do 110, r_n = 1, n_r
            vr_n = vr_in_r(r_n)
            if (iand(st(vr_n), u_bs_cg_bt) .ne. 0) then
c
c     This is a logical which has entered the basis due to singularity.
c     Find a variable which has left the basis due to singularity and
c     update ITS position in vr_in_r. This will leave all the variables
c     which were in vr_in_r before INVERT with their positions in the
c     new vr_in_r or the position of a logical which could have replaced
c     them.
c
c               print*,'Trying to match basic ', vr_n,
c     &              ' f_ix_n = ', f_ix_n
               do 105, ix_n = f_ix_n, n_c+n_r
                  if (ix_n .le. n_c) then
                     c_n = ix_n
                  else
                     c_n = ix_n+mx_n_c-n_c
                  endif
                  if (iand(st(c_n), u_bs_cg_bt) .ne. 0 .and.
     &                 iand(st(c_n), bc_bt) .eq. 0) then
c                     print*,'Found ', c_n, ' to match basic ', vr_n
                     f_ix_n = ix_n + 1
                     goto 107
                  endif
 105           continue
               goto 8000
 107           continue
               st(c_n) = st(c_n) - iand(st(c_n), mx_mx_ml_a_dim) + r_n
            else
               st(vr_n) = st(vr_n) - iand(st(vr_n), mx_mx_ml_a_dim) +
     &              r_n
            endif
 110     continue
      endif
 7000 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
c      print*, 'Searched indices ', f_ix_n, ' to ', n_c+n_r
c      print*, ' to find match for basic variable ', vr_n,
c     &     ' in row ', r_n
      goto 7000
 9800 format('No matching nonbasic variable which left the basis due',
     &     ' to singularity')
      end
 
C->>> -------------------------------------------> ems_g_inv_eta_rec <<<
c     Brings the invert eta group and eta set records up-to-date.
c     Analyses the invert to identify special eta sets which would allow
c     it to be applied more efficiently.
c
      subroutine ems_g_inv_eta_rec(
     &     mx_n_eta,
     &     n_eta, n_eta_el,
     &     eta_grp, eta_v, eta_ix, eta_rec)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'SLAPCS.INC'
      include 'EMSMEM.INC'
      include 'ICTVR.INC'
      integer mx_n_eta
      integer n_eta, n_eta_el
      integer eta_grp(0:mx_eta_grp_rec_z)
      double precision eta_v(0:n_eta_el) !Not used
      integer eta_ix(0:n_eta_el) !Not used
      integer eta_rec(0:mx_n_eta+(2*eta_se_rec_z+1)*mx_n_eta_se+1)
      integer p, n_eta_se
 
      eta_rec(eta_se_rec_os_eta_ty) = inv_eta_se_ty
      eta_rec(eta_se_rec_os_n_eta) = n_eta
      p = eta_se_rec_z + n_eta + 1 + eta_se_rec_z
      eta_rec(p+eta_se_rec_bwd_os_n_eta) = n_eta
      eta_rec(p+eta_se_rec_bwd_os_eta_ty) = inv_eta_se_ty
      n_eta_se = 1
c
c     Initialise the first record in the next set to correspond to a
c     null set.
c
      eta_rec(p+eta_se_rec_os_eta_ty) = no_eta_se_ty
c
c     Record the number of etas, values, indices and records in the eta
c     group.
c
      eta_grp(eta_grp_os_n_eta) = n_eta
      eta_grp(eta_grp_os_n_v) = n_eta_el
      eta_grp(eta_grp_os_n_ix) = n_eta_el
      eta_grp(eta_grp_os_n_rec) = n_eta + (2*eta_se_rec_z+1)*n_eta_se+1
      eta_fi_n_grp = 1
      eta_fi_n_se =  1
      eta_fi_n_eta = n_eta
      eta_fi_n_v =   n_eta_el
      eta_fi_n_ix =  n_eta_el
      return
      end
 
C->>> --------------------------------------------------> ems_rm_inv <<<
c     Removes the data structures associated with the current invert.
c
      subroutine ems_rm_inv(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMIHDL.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer eta_grp_n
      integer p_hdl_eta_grp
      integer mem_mgr_rt_cod
 
      p_hdl_eta_grp = p_ml_bs_blk + (cu_ml_n-1)*ml_bs_blk_n_wo +
     &     ml_bs_blk_os_hdl + ix_eta_fi_hdl
      do 10, eta_grp_n = 1, eta_fi_n_grp
         call ems_mem_mgr_rm_blk(mem_mgr_rt_cod, is,
     &        is(p_hdl_eta_grp))
         if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
            ems_msg_cod = ems_msg_lvl_serious
            goto 7100
         endif
         p_hdl_eta_grp = p_hdl_eta_grp + hdl_z
 10   continue
      eta_fi_n_grp = 0
      if (iand(rsmi_blk_st_msk, rsmi_blk_st_r_eta_fi_en) .ne. 0)
     &     call ems_rm_blk_r_eta_fi_en(is)
 7100 continue
      return
      end
 
C->>> -------------------------------------------> ems_se_inv_p_no_p <<<
c     Set the INVERT pointers to be `no pointers'.
c     Returns:
c     rt_cod = 0 Since nothing can go wrong
c
      subroutine ems_se_inv_p_no_p(rt_cod)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'SLAPCS.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
c      include 'ICTVR.INC'
      integer rt_cod, eta_grp_n, p_eta_grp, p_eta_fi_p_a
 
      rt_cod = 0
      p_eta_fi_p_a = -eta_fi_p_a_rec_z
      do 10, eta_grp_n = 1, eta_fi_mx_n_eta_grp
         p_eta_fi_p_a = p_eta_fi_p_a + eta_fi_p_a_rec_z
         p_eta_grp = no_p
         eta_fi_p_a(p_eta_fi_p_a+eta_fi_p_a_os_p_eta_grp) = p_eta_grp
         eta_fi_p_a(p_eta_fi_p_a+eta_fi_p_a_os_p_eta_v) = no_p
         eta_fi_p_a(p_eta_fi_p_a+eta_fi_p_a_os_p_eta_ix) = no_p
         eta_fi_p_a(p_eta_fi_p_a+eta_fi_p_a_os_p_eta_rec) = no_p
 10   continue
      p_nw_eta_v = no_p
      p_nw_eta_ix = no_p
      p_nw_eta_rec = no_p
      return
      end
 
C->>> -------------------------------------------------> ems_g_inv_p <<<
c     Gets the pointers to the eta groups, and arrays of values, indices
c     and starts in the current invert.
c     Returns:
c     rt_cod = 1 if mem_mgr_rt_cod is serious
c     rt_cod = 0 Otherwise
c
      subroutine ems_g_inv_p(rt_cod, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'SLAPCS.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer rt_cod, is(0:is_n_en_m1)
      integer eta_grp_n, p_hdl_eta_grp, p_eta_grp, p_eta_fi_p_a
      integer mem_mgr_rt_cod
 
      rt_cod = 0
      if (eta_fi_n_grp .eq. 0) goto 7000
      p_eta_fi_p_a = -eta_fi_p_a_rec_z
      p_hdl_eta_grp = p_ml_bs_blk + (cu_ml_n-1)*ml_bs_blk_n_wo +
     &     ml_bs_blk_os_hdl + ix_eta_fi_hdl
      do 10, eta_grp_n = 1, eta_fi_n_grp
         p_eta_fi_p_a = p_eta_fi_p_a + eta_fi_p_a_rec_z
         call ems_mem_mgr_g_p4(mem_mgr_rt_cod, is,
     &        is(p_hdl_eta_grp), p_eta_grp)
         if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) goto 8800
         eta_fi_p_a(p_eta_fi_p_a+eta_fi_p_a_os_p_eta_grp) = p_eta_grp
         call ems_mem_mgr_g_p8(mem_mgr_rt_cod, is,
     &        is(p_eta_grp+eta_grp_os_hdl_v),
     &        eta_fi_p_a(p_eta_fi_p_a+eta_fi_p_a_os_p_eta_v))
         if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) goto 8800
         call ems_mem_mgr_g_p4(mem_mgr_rt_cod, is,
     &        is(p_eta_grp+eta_grp_os_hdl_ix),
     &        eta_fi_p_a(p_eta_fi_p_a+eta_fi_p_a_os_p_eta_ix))
         if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) goto 8800
         call ems_mem_mgr_g_p4(mem_mgr_rt_cod, is,
     &        is(p_eta_grp+eta_grp_os_hdl_rec),
     &        eta_fi_p_a(p_eta_fi_p_a+eta_fi_p_a_os_p_eta_rec))
         if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) goto 8800
         p_hdl_eta_grp = p_hdl_eta_grp + hdl_z
 10   continue
      p_nw_eta_v = eta_fi_p_a(p_eta_fi_p_a+eta_fi_p_a_os_p_eta_v) +
     &     is(p_eta_grp+eta_grp_os_n_v) + 1
      p_nw_eta_ix = eta_fi_p_a(p_eta_fi_p_a+eta_fi_p_a_os_p_eta_ix) +
     &     is(p_eta_grp+eta_grp_os_n_ix) + 1
      p_nw_eta_rec = eta_fi_p_a(p_eta_fi_p_a+eta_fi_p_a_os_p_eta_rec) +
     &     is(p_eta_grp+eta_grp_os_n_rec)
 7000 continue
 7100 continue
      return
 8800 continue
      ems_msg_cod = ems_msg_lvl_serious
      rt_cod = 1
      goto 7100
      end
 
C->>> --------------------------------------> ems_iz_blk_ml_bs_inv_p <<<
c     Sets up a block and the handles for the model basis INVERT
c     pointers.
c
      subroutine ems_iz_blk_ml_bs_inv_p(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer blk_n, r_cf, c_cf, a_el_cf, cs, n_wo, p_ml_hdl
      integer mem_mgr_rt_cod
      integer rt_cod
 
      if (iand(ml_blk_st_msk,  ml_blk_st_ml_bs_inv_p) .ne. 0) goto 8000
      call ems_g_blk_ml_bs_inv_p_n_wo(r_cf, c_cf, a_el_cf, cs, n_wo)
c      r_cf = 2*i_wo_z
c      cs =   2*i_wo_z
c      if (iand(inv_alg_msk, inv_alg_sus) .ne. 0) then
c         r_cf = r_cf + 2*i_wo_z
c         cs =   cs +   2*i_wo_z
c      endif
c      if (iand(inv_alg_msk, inv_alg_perm) .ne. 0) then
c         r_cf = r_cf + 2*i_wo_z
c         cs =   cs +   2*i_wo_z
c      endif
c      if (iand(eta_fi_mode_msk, eta_fi_bwd_p_y_bt) .ne. 0 .or.
c     &     iand(eta_fi_mode_msk, eta_fi_bwd_p_poss_bt) .ne. 0) then
c         r_cf = r_cf + 2*i_wo_z
c         cs =   cs +   2*i_wo_z
c      endif
      call ems_mem_mgr_ope_blk(mem_mgr_rt_cod, is,
     &     n_wo, ope_blk_at_f_fr_p,
     &     cu_ml_n, ml_bs_inv_p_blk_id, blk_n)
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
     &     r_cf, mx_n_r,
     &     0, n_c,
     &     0, n_a_el,
     &     cs, n_wo, ml_bs_inv_p_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         go to 7000
      endif
c
c     Get handles for the arrays within the block.
c
      p_ml_hdl = p_ml_bs_blk + (cu_ml_n-1)*ml_bs_blk_n_wo +
     &     ml_bs_blk_os_hdl
      if (iand(inv_alg_msk, inv_alg_sus) .ne. 0) then
         call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &        blk_n, 1+               mx_n_r, i_wo_z,
     &        is(p_ml_hdl+ix_hdl_lo_eta_pv_in_r))
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
         call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &        blk_n, 1+               mx_n_r, i_wo_z,
     &        is(p_ml_hdl+ix_hdl_up_eta_pv_in_r))
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      endif
      if (iand(eta_fi_mode_msk, eta_fi_bwd_p_y_bt) .ne. 0 .or.
     &     iand(eta_fi_mode_msk, eta_fi_bwd_p_poss_bt) .ne. 0) then
         call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &        blk_n, 1+               mx_n_r, i_wo_z,
     &        is(p_ml_hdl+ix_hdl_eta_w_l_en_in_r))
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
         call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &        blk_n, 1+               mx_n_r, i_wo_z,
     &        is(p_ml_hdl+ix_hdl_eta_w_lm1_en_in_r))
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      endif
      if (iand(inv_alg_msk, inv_alg_perm) .ne. 0) then
         call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &        blk_n, 1+            mx_n_r, i_wo_z,
     &        is(p_ml_hdl+ix_hdl_nw_t_og_perm))
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
         call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &        blk_n, 1+            mx_n_r, i_wo_z,
     &        is(p_ml_hdl+ix_hdl_og_t_nw_perm))
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      endif
      call ems_g_ml_p(rt_cod, is)
      if (rt_cod .ne. 0) goto 8020
      call ems_iz_sus_fwd_tran_pm(is)
c
c     Indicate that the model now has space for the basis iNVERT
c     pointers.
c
      ml_blk_st_msk = ml_blk_st_msk + ml_blk_st_ml_bs_inv_p
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
 9800 format('Model already has space for basis INVERT pointers')
 9802 format('Error in ems_g_ml_p')
      end
 
C->>> ----------------------------------> ems_g_blk_ml_bs_inv_p_n_wo <<<
      subroutine ems_g_blk_ml_bs_inv_p_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      integer r_cf, c_cf, a_el_cf, cs, n_wo
 
      r_cf =    2*i_wo_z
      c_cf =    0
      a_el_cf = 0
      cs =      2*i_wo_z
      if (iand(inv_alg_msk, inv_alg_sus) .ne. 0) then
         r_cf = r_cf + 2*i_wo_z
         cs =   cs +   2*i_wo_z
      endif
      if (iand(inv_alg_msk, inv_alg_perm) .ne. 0) then
         r_cf = r_cf + 2*i_wo_z
         cs =   cs +   2*i_wo_z
      endif
      if (iand(eta_fi_mode_msk, eta_fi_bwd_p_y_bt) .ne. 0 .or.
     &     iand(eta_fi_mode_msk, eta_fi_bwd_p_poss_bt) .ne. 0) then
         r_cf = r_cf + 2*i_wo_z
         cs =   cs +   2*i_wo_z
      endif
      n_wo = r_cf*mx_n_r + c_cf*mx_n_c + a_el_cf*mx_n_a_el + cs
      return
      end
 
C->>> --------------------------------------> ems_rm_blk_ml_bs_inv_p <<<
c     Removes the block for the linear model basis INVERT pointers.
c
      subroutine ems_rm_blk_ml_bs_inv_p(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer blk_n, p_ml_hdl
      integer mem_mgr_rt_cod
 
      if (iand(ml_blk_st_msk,  ml_blk_st_ml_bs_inv_p) .eq. 0) goto 8000
      p_ml_hdl = p_ml_bs_blk + (cu_ml_n-1)*ml_bs_blk_n_wo +
     &     ml_bs_blk_os_hdl
      blk_n = is(p_ml_hdl+ix_blk_ml_bs_inv_p)
      call ems_mem_mgr_rm_blk(mem_mgr_rt_cod, is, blk_n)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         goto 7100
      endif
      ml_blk_st_msk = ml_blk_st_msk - ml_blk_st_ml_bs_inv_p
 7000 continue
 7100 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9800 format('Model does not already have space for ',
     &     'basis INVERT pointers')
      end
 
CM      IF (emsol_dev .EQ. 1) THEN
C?c->>> ------------------------------------> ems_ca_an_bs_mtx_and_inv <<<
C?c     Calls the routine to analyse the current basis and its INVERT.
C?c
C?      subroutine ems_ca_an_bs_mtx_and_inv(
C?     &     inv_pic,
C?     &     vr_in_r,
C?     &     hdl_eta_grp,
C?     &     ds, is)
C?      implicit none
C?      include 'EMSV.INC'
C?      include 'EMSPM.INC'
C?      include 'EMSMMGR.INC'
C?      include 'SLAPCS.INC'
C?      include 'EMSMEM.INC'
C?      include 'EMSP.INC'
C?      include 'ICTVR.INC'
C?      include 'EMSMSG.INC'
C?      logical inv_pic
C?      double precision ds(0:ds_n_en_m1)
C?      integer vr_in_r(0:n_r)
C?      integer hdl_eta_grp(0:hdl_z_m1)
C?      integer is(0:is_n_en_m1)
C?      integer p_eta_grp
C?      integer p_eta_v, p_eta_ix, p_eta_rec, p_eta_sa
C?      integer hdl_lc_i_wk_a(0:hdl_z-1)
C?      integer p_lc_i_wk_a, lc_i_wk_a_n_en
C?      double precision bs_mtx_1_norm
C?      integer n_lg, n_bs_mtx_nz_el
C?      integer wr_msk
C?      integer i_wk_a_ix
C?      integer mem_mgr_rt_cod
C?
C?      call ems_mem_mgr_g_p4(mem_mgr_rt_cod, is,
C?     &     hdl_eta_grp, p_eta_grp)
C?      if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) goto 8800
C?      call ems_mem_mgr_g_p8(mem_mgr_rt_cod, is,
C?     &     is(p_eta_grp + eta_grp_os_hdl_v), p_eta_v)
C?      if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) goto 8800
C?      call ems_mem_mgr_g_p4(mem_mgr_rt_cod, is,
C?     &     is(p_eta_grp + eta_grp_os_hdl_ix), p_eta_ix)
C?      if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) goto 8800
C?      call ems_mem_mgr_g_p4(mem_mgr_rt_cod, is,
C?     &     is(p_eta_grp + eta_grp_os_hdl_rec), p_eta_rec)
C?      if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) goto 8800
C?      p_eta_sa = p_eta_rec + eta_se_rec_os_ze_sa_en
C?
C?      lc_i_wk_a_n_en = max(1+eta_fi_n_eta, 1+n_r)
C?      call ems_mem_mgr_ope_a(mem_mgr_rt_cod, is,
C?     &     lc_i_wk_a_n_en, i_wo_z, hdl_lc_i_wk_a)
C?      if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
C?         if (mem_mgr_rt_cod .eq. mem_mgr_rt_cod_serious_no_po) then
C?            ems_msg_cod = ems_msg_lvl_serious
C?            go to 7000
C?         else if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
C?             ems_msg_cod = ems_msg_lvl_serious
C?            go to 7000
C?         endif
C?      endif
C?      call ems_mem_mgr_g_p4(mem_mgr_rt_cod, is,
C?     &     hdl_lc_i_wk_a, p_lc_i_wk_a)
C?      if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) goto 8800
C?
C?      wr_msk = 1
C?
C?      call ems_an_bs_mtx(wr_msk,
C?     &     n_r, mx_n_c, n_a_el,
C?     &     vr_in_r,
C?     &     is(p_mtx_c_sa), is(p_mtx_r_ix), ds(p_mtx_r_v),
C?     &     bs_mtx_1_norm, n_lg, n_bs_mtx_nz_el)
C?      call ems_g_rsmi_i_wk_a_ix(i_wk_a_ix)
C?      if (i_wk_a_ix .lt. 0) goto 8000
C?      call ems_an_mtx_inv(inv_pic, wr_msk,
C?     &     n_r, eta_fi_n_eta, eta_fi_n_ix, n_lo_c_eta, lc_i_wk_a_n_en,
C?     &     ds(p_eta_v), is(p_eta_ix), is(p_eta_sa),
C?     &     ds(p_pv_c_v), is(p_rsmi_i_wk_a(i_wk_a_ix)), is(p_lc_i_wk_a),
C?     &     bs_mtx_1_norm, n_lg, n_bs_mtx_nz_el)
C?      call ems_fr_rsmi_i_wk_a_ix(i_wk_a_ix)
C?      call ems_mem_mgr_rm_a(mem_mgr_rt_cod, is, hdl_lc_i_wk_a)
C?      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
C? 7000 continue
C? 7100 continue
C?      return
C? 8000 continue
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
C?      call ems_msg_wr_li(bug_msg_n)
C?      goto 7000
C? 8800 continue
C?      ems_msg_cod = ems_msg_lvl_serious
C?      goto 7100
C? 9800 format('RSMI workspace not available in ems_ca_an_bs_mtx_and_inv')
C?      end
C?
C?c->>> ---------------------------------------------> ems_an_bs_mtx <<<
C?c     Analyses a basis matrix.
C?c
C?      subroutine ems_an_bs_mtx(wr_msk,
C?     &     n_r, mx_n_c, n_a_el,
C?     &     vr_in_r,
C?     &     mtx_c_sa, mtx_r_ix, mtx_r_v,
C?     &     bs_mtx_1_norm, n_lg, n_bs_mtx_nz_el)
C?      implicit none
C?      include 'EMSV.INC'
C?      include 'EMSMSG.INC'
C?      integer wr_msk, n_r, mx_n_c, n_a_el
C?      integer vr_in_r(0:n_r)
C?      integer mtx_c_sa(0:mx_n_c+1)
C?      integer mtx_r_ix(0:n_a_el)
C?      double precision mtx_r_v(0:n_a_el)
C?      double precision bs_mtx_1_norm
C?      integer n_bs_mtx_nz_el, n_lg
C?      integer ems_i_t_i_pct
C?      integer r_n, vr_n, el_n, c_k
C?      integer n_struc, n_bs_mtx_ze_el, n_bs_mtx_el
C?      integer n_struc_c_nz_el
C?      integer mn_c_k, mx_c_k, su_c_k, su_c_k_sq
C?      double precision abs_mtx_v
C?      double precision av_c_k, dev_c_k
C?      double precision mn_c_norm, mx_c_norm, su_c_norm, su_c_norm_sq
C?      double precision av_c_norm, dev_c_norm
C?      double precision mn_mtx_v, mx_mtx_v, su_mtx_v, su_mtx_v_sq
C?      double precision av_mtx_v, dev_mtx_v
C?      double precision c_norm
C?      double precision mx_c_v, mn_c_v
C?c
C?c     Find the 1-norm of B
C?c
C?      if (iand(wr_msk, bt2) .ne. 0) then
C?         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9200)
C?         call ems_msg_wr_li(info_msg_n)
C?      endif
C?      n_struc = 0
C?      n_lg = 0
C?      n_bs_mtx_nz_el = 0
C?      n_bs_mtx_ze_el = 0
C?      bs_mtx_1_norm = zero
C?      mx_c_k = 0
C?      mn_c_k = n_r+1
C?      su_c_k = 0
C?      su_c_k_sq = 0
C?      mn_c_norm = inf
C?      mx_c_norm = zero
C?      su_c_norm = zero
C?      su_c_norm_sq = zero
C?      mx_mtx_v = zero
C?      mn_mtx_v = inf
C?      su_mtx_v = zero
C?      su_mtx_v_sq = zero
C?      do 20, r_n = 1, n_r
C?         vr_n = vr_in_r(r_n)
C?         if (vr_n .le. mx_n_c) then
C?            n_struc = n_struc + 1
C?            mx_c_v = zero
C?            mn_c_v = inf
C?            c_k = mtx_c_sa(vr_n+1) - mtx_c_sa(vr_n)
C?            mx_c_k = max(c_k, mx_c_k)
C?            mn_c_k = min(c_k, mn_c_k)
C?            su_c_k = su_c_k + c_k
C?            su_c_k_sq = su_c_k_sq + c_k*c_k
C?            c_norm = zero
C?            do 10, el_n = mtx_c_sa(vr_n), mtx_c_sa(vr_n+1)-1
C?               if (mtx_r_v(el_n) .eq. zero) then
C?                  n_bs_mtx_ze_el = n_bs_mtx_ze_el + 1
C?               else
C?                  abs_mtx_v = abs(mtx_r_v(el_n))
C?                  n_bs_mtx_nz_el = n_bs_mtx_nz_el + 1
C?                  c_norm = c_norm + abs_mtx_v
C?                  mx_c_v = max(abs_mtx_v, mx_c_v)
C?                  mn_c_v = min(abs_mtx_v, mn_c_v)
C?                  su_mtx_v = su_mtx_v + abs_mtx_v
C?                  su_mtx_v_sq = su_mtx_v_sq + abs_mtx_v*abs_mtx_v
C?               endif
C? 10         continue
C?            if (iand(wr_msk, bt2) .ne. 0) then
C?               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9210)
C?     &              r_n, vr_n, c_k, mx_c_v, mn_c_v, c_norm
C?               call ems_msg_wr_li(info_msg_n)
C?            endif
C?            mx_c_norm = max(c_norm, mx_c_norm)
C?            mn_c_norm = min(c_norm, mn_c_norm)
C?            su_c_norm = su_c_norm + c_norm
C?            su_c_norm_sq = su_c_norm_sq + c_norm*c_norm
C?         else
C?            n_lg = n_lg + 1
C?            n_bs_mtx_nz_el = n_bs_mtx_nz_el + 1
C?            mx_c_v = one
C?            mn_c_v = one
C?            c_k = 1
C?            c_norm = one
C?         endif
C?         mx_mtx_v = max(mx_c_v, mx_mtx_v)
C?         mn_mtx_v = min(mn_c_v, mn_mtx_v)
C?         bs_mtx_1_norm = max(c_norm, bs_mtx_1_norm)
C? 20   continue
C?      n_bs_mtx_el = n_bs_mtx_nz_el + n_bs_mtx_ze_el
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)n_r
C?      call ems_msg_wr_li(info_msg_n)
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9101)
C?     &     n_struc, ems_i_t_i_pct(n_struc, n_r)
C?      call ems_msg_wr_li(info_msg_n)
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9102)
C?     &     n_lg, ems_i_t_i_pct(n_lg, n_r)
C?      call ems_msg_wr_li(info_msg_n)
C?      if (n_struc .gt. 0) then
C?         n_struc_c_nz_el = n_bs_mtx_nz_el - n_lg
C?         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9110)
C?     &        n_struc, n_struc_c_nz_el
C?         call ems_msg_wr_li(info_msg_n)
C?         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9111)
C?         call ems_msg_wr_li(info_msg_n)
C?         av_c_k = su_c_k/float(n_struc)
C?         dev_c_k = su_c_k_sq/float(n_struc) - av_c_k*av_c_k
C?         dev_c_k = sqrt(abs(dev_c_k))
C?         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9112)'Counts ',
C?     &        mn_c_k, mx_c_k, av_c_k, dev_c_k
C?         call ems_msg_wr_li(info_msg_n)
C?         av_mtx_v = su_mtx_v/float(n_struc_c_nz_el)
C?         dev_mtx_v = su_mtx_v_sq/float(n_struc_c_nz_el) -
C?     &        av_mtx_v*av_mtx_v
C?         dev_mtx_v = sqrt(abs(dev_mtx_v))
C?         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9113)'Values ',
C?     &        mn_mtx_v, mx_mtx_v, av_mtx_v, dev_mtx_v
C?         call ems_msg_wr_li(info_msg_n)
C?         av_c_norm = su_c_norm/float(n_struc)
C?         dev_c_norm = su_c_norm_sq/float(n_struc) - av_c_norm*av_c_norm
C?         dev_c_norm = sqrt(abs(dev_c_norm))
C?         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9113)'Norms  ',
C?     &        mn_c_norm, mx_c_norm, av_c_norm, dev_c_norm
C?         call ems_msg_wr_li(info_msg_n)
C?      endif
C?      if (n_bs_mtx_ze_el .gt. 0) then
C?         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9120)
C?     &        n_bs_mtx_el, n_bs_mtx_ze_el,
C?     &        ems_i_t_i_pct(n_bs_mtx_ze_el, n_bs_mtx_el)
C?         call ems_msg_wr_li(info_msg_n)
C?      else
C?         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9121)
C?     &        n_bs_mtx_el
C?         call ems_msg_wr_li(info_msg_n)
C?      endif
C?      return
C? 9100 format('Basis contains ', i7, ' variables, of which')
C? 9101 format(i7, ' are structural, a proportion of ', i3, '%;')
C? 9102 format(i7, ' are logical,    a proportion of ', i3, '%;')
C? 9110 format('Structural columns (', i7, ') have ',
C?     &     i7, ' nonzero entries')
C? 9111 format('              Min          Max      Average    Deviation')
C? 9112 format(a7, ':', 2(2x, i7, 4x), 2(2x, g11.4))
C? 9113 format(a7, ':', 2(2x, g11.4),  2(2x, g11.4))
C? 9120 format('Basis matrix has ', i7, ' entries',
C?     &     ', of which ', i7, ' are zero, a proportion of ', i3, '%')
C? 9121 format('Basis matrix has ', i7, ' entries---all nonzero')
C? 9200 format('      c_n     vr_n      c_k',
C?     &     '     mx_v         mn_v       1_norm')
C? 9210 format(3(2x, i7), 3(2x, g11.4))
C?      end
C?
C?c->>> ----------------------------------------------> ems_an_mtx_inv <<<
C?c     Analyses a matrix INVERT.
C?c
C?      subroutine ems_an_mtx_inv(inv_pic, wr_msk,
C?     &     n_r, n_eta, l_eta_ix, n_lo_eta, lc_i_wk_a_n_en,
C?     &     eta_v, eta_ix, eta_sa,
C?     &     rhs_v, rhs_ix, lc_i_wk_a,
C?     &     mtx_1_norm, n_lg, n_mtx_nz_el)
C?      implicit none
C?      include 'EMSV.INC'
C?      include 'EMSMSG.INC'
C?      logical inv_pic
C?      integer wr_msk, n_r, n_eta, l_eta_ix, n_lo_eta, lc_i_wk_a_n_en
C?      integer eta_ix(0:l_eta_ix)
C?      integer eta_sa(0:n_eta+1)
C?      integer rhs_ix(0:n_r)
C?      integer lc_i_wk_a(0:lc_i_wk_a_n_en)
C?      double precision eta_v(0:l_eta_ix), rhs_v(0:n_r)
C?      double precision mtx_1_norm
C?      integer n_lg, n_mtx_nz_el
C?      integer r_n, eta_n, el_n, pv_r, eta_c_k
C?      integer n_eta_el
C?      double precision su, x_norm, y_norm
C?      double precision cond
C?      integer n_lo_eta_el
C?      integer f_up_eta, n_up_eta, n_up_eta_el
C?      integer mn_eta_c_k, mx_eta_c_k, su_eta_c_k, su_eta_c_k_sq
C?      double precision av_eta_c_k, dev_eta_c_k
C?      double precision mx_eta_c_v, pv_v, abs_pv_v, abs_eta_v, eta_growth
C?      double precision mn_eta_v, mx_eta_v
C?      double precision su_eta_v, su_eta_v_sq
C?      double precision av_eta_v, dev_eta_v
C?      double precision mn_eta_pv_v, mx_eta_pv_v
C?      double precision su_eta_pv_v, su_eta_pv_v_sq
C?      double precision av_eta_pv_v, dev_eta_pv_v
C?      double precision mn_eta_growth, mx_eta_growth
C?      double precision su_eta_growth, su_eta_growth_sq
C?      double precision av_eta_growth, dev_eta_growth
C?      logical alw_f7_wr
C?
C?      alw_f7_wr = ems_msg_no_prt_fm .ge. 1
C?c
C?c     Zero the vector of values in case it is not a zeroed vector.
C?c     Initialise rhs_ix which is used to mark whether the
C?c     corresponding value in b has been set.
C?c
C?      do 10, r_n = 1, n_r
C?         rhs_v(r_n) = one
C?         rhs_ix(r_n) = 0
C? 10   continue
C?c
C?c     Solve B^Tx = b, choosing entries in b to be 1 or -1 so as to
C?c     make x large.
C?c
C?      n_eta_el = eta_sa(n_eta+1) - eta_sa(1)
C?      el_n = eta_sa(n_eta+1)
C?      do 30, eta_n = n_eta, 1, -1
C?         su = zero
C?         do 20, el_n = el_n-1, eta_sa(eta_n)+1, -1
C?            rhs_ix(eta_ix(el_n)) = 1
C?            su = su + rhs_v(eta_ix(el_n))*eta_v(el_n)
C? 20      continue
C?         pv_r = eta_ix(el_n)
C?         if (rhs_ix(pv_r) .eq. 0) then
C?            if (su .ge. zero) then
C?               su = one + su
C?            else
C?               su = -one + su
C?            endif
C?            rhs_ix(pv_r) = 1
C?         else
C?            su = rhs_v(pv_r) + su
C?         endif
C?         rhs_v(pv_r) = su*eta_v(el_n)
C? 30   continue
C?c
C?c     Find the 1-norm of x, setting to 1 any RHS values which are
C?c     zero---no eta has an index in this row.
C?c
C?      x_norm = zero
C?      do 40, r_n = 1, n_r
C?         x_norm = x_norm + abs(rhs_v(r_n))
C? 40   continue
C?c
C?c     Solve Ay = x.
C?c
C?      do 60, eta_n = 1, n_eta
C?         pv_r = eta_ix(el_n)
C?         rhs_v(pv_r) = rhs_v(pv_r)*eta_v(el_n)
C?         do 50, el_n = el_n+1, eta_sa(eta_n+1)-1
C?            rhs_v(eta_ix(el_n)) =
C?     &           rhs_v(eta_ix(el_n)) + rhs_v(pv_r)*eta_v(el_n)
C? 50      continue
C? 60   continue
C?c
C?c     Find the 1-norm of y and zero the vector of values in case it is
C?c     a zeroed vector.
C?c
C?      y_norm = zero
C?      do 70, r_n = 1, n_r
C?         y_norm = y_norm + abs(rhs_v(r_n))
C?         rhs_v(r_n) = zero
C? 70   continue
C?      cond = mtx_1_norm*y_norm/x_norm
C?c
C?c     Investigate the structure of the INVERT
C?c
C?      if (alw_f7_wr) write(ems_li, 9000)n_r
C?      call ems_msg_wr_li(info_msg_n)
C?c
C?c     Analyse the L-etas
C?c
C?      mn_eta_c_k = n_r+1
C?      mx_eta_c_k = 0
C?      su_eta_c_k = 0
C?      su_eta_c_k_sq = 0
C?      mn_eta_v = inf
C?      mx_eta_v = zero
C?      su_eta_v = zero
C?      su_eta_v_sq = zero
C?      mn_eta_pv_v = inf
C?      mx_eta_pv_v = zero
C?      su_eta_pv_v = zero
C?      su_eta_pv_v_sq = zero
C?      mn_eta_growth = inf
C?      mx_eta_growth = zero
C?      su_eta_growth = zero
C?      su_eta_growth_sq = zero
C?      if (n_eta .gt. 0 .and. iand(wr_msk, bt3) .ne. 0) then
C?         if (alw_f7_wr) write(ems_li, 9300)
C?         call ems_msg_wr_li(info_msg_n)
C?      endif
C?      do 130, eta_n = 1, n_lo_eta
C?         el_n = eta_sa(eta_n)
C?         pv_r = eta_ix(el_n)
C?         pv_v = one/eta_v(el_n)
C?         abs_pv_v = abs(pv_v)
C?         mn_eta_pv_v = min(mn_eta_pv_v, abs_pv_v)
C?         mx_eta_pv_v = max(mx_eta_pv_v, abs_pv_v)
C?         su_eta_pv_v = su_eta_pv_v + abs_pv_v
C?         su_eta_pv_v_sq = su_eta_pv_v_sq + abs_pv_v*abs_pv_v
C?         eta_c_k = eta_sa(eta_n+1)-eta_sa(eta_n)
C?         mx_eta_c_v = zero
C?         do 120, el_n = eta_sa(eta_n)+1, eta_sa(eta_n+1)-1
C?            abs_eta_v = abs(eta_v(el_n))
C?            mx_eta_c_v = max(mx_eta_c_v, abs_eta_v)
C?            mn_eta_v = min(mn_eta_v, abs_eta_v)
C?            mx_eta_v = max(mx_eta_v, abs_eta_v)
C?            su_eta_v = su_eta_v + abs_eta_v
C?            su_eta_v_sq = su_eta_v_sq + abs_eta_v*abs_eta_v
C? 120     continue
C?         eta_growth = mx_eta_c_v/abs_pv_v
C?         if (iand(wr_msk, bt3) .ne. 0) then
C?            if (alw_f7_wr) write(ems_li, 9310)
C?     &           eta_n, eta_c_k,
C?     &           mx_eta_c_v, pv_v, eta_growth
C?            call ems_msg_wr_li(info_msg_n)
C?         endif
C?         mn_eta_c_k = min(eta_c_k, mn_eta_c_k)
C?         mx_eta_c_k = max(eta_c_k, mx_eta_c_k)
C?         su_eta_c_k = su_eta_c_k + eta_c_k
C?         su_eta_c_k_sq = su_eta_c_k_sq + eta_c_k*eta_c_k
C?         mn_eta_growth = min(eta_growth, mn_eta_growth)
C?         mx_eta_growth = max(eta_growth, mx_eta_growth)
C?         su_eta_growth = su_eta_growth + eta_growth
C?         su_eta_growth_sq = su_eta_growth_sq + eta_growth*eta_growth
C? 130  continue
C?      n_lo_eta_el = eta_sa(n_lo_eta+1)-1
C?      f_up_eta = n_lo_eta + 1
C?      if (alw_f7_wr) write(ems_li, 9110)'     L-',
C?     &     n_lo_eta, n_lo_eta_el
C?      call ems_msg_wr_li(info_msg_n)
C?      if (n_lo_eta .gt. 0) then
C?         if (alw_f7_wr) write(ems_li, 9111)
C?         call ems_msg_wr_li(info_msg_n)
C?         av_eta_c_k = su_eta_c_k/float(n_lo_eta)
C?         dev_eta_c_k = su_eta_c_k_sq/float(n_lo_eta) -
C?     &        av_eta_c_k*av_eta_c_k
C?         dev_eta_c_k = sqrt(abs(dev_eta_c_k))
C?         if (alw_f7_wr) write(ems_li, 9112)'Counts ',
C?     &        mn_eta_c_k, mx_eta_c_k, av_eta_c_k, dev_eta_c_k
C?         call ems_msg_wr_li(info_msg_n)
C?         av_eta_v = su_eta_v/float(n_lo_eta_el-n_lo_eta)
C?         dev_eta_v = su_eta_v_sq/float(n_lo_eta_el-n_lo_eta) -
C?     &        av_eta_v*av_eta_v
C?         dev_eta_v = sqrt(abs(dev_eta_v))
C?         if (alw_f7_wr) write(ems_li, 9113)'Values ',
C?     &        mn_eta_v, mx_eta_v,
C?     &        av_eta_v, dev_eta_v
C?         call ems_msg_wr_li(info_msg_n)
C?         av_eta_pv_v = su_eta_pv_v/float(n_lo_eta)
C?         dev_eta_pv_v = su_eta_pv_v_sq/float(n_lo_eta) -
C?     &        av_eta_pv_v*av_eta_pv_v
C?         dev_eta_pv_v = sqrt(abs(dev_eta_pv_v))
C?         if (alw_f7_wr) write(ems_li, 9113)'Pivots ',
C?     &        mn_eta_pv_v, mx_eta_pv_v,
C?     &        av_eta_pv_v, dev_eta_pv_v
C?         call ems_msg_wr_li(info_msg_n)
C?         av_eta_growth = su_eta_growth/float(n_lo_eta)
C?         dev_eta_growth = su_eta_growth_sq/float(n_lo_eta) -
C?     &        av_eta_growth*av_eta_growth
C?         dev_eta_growth = sqrt(abs(dev_eta_growth))
C?         if (alw_f7_wr) write(ems_li, 9113)'Growth ',
C?     &        mn_eta_growth, mx_eta_growth,
C?     &        av_eta_growth, dev_eta_growth
C?         call ems_msg_wr_li(info_msg_n)
C?      endif
C?c
C?c     Analyse the U-etas
C?c
C?      mn_eta_c_k = n_r+1
C?      mx_eta_c_k = 0
C?      su_eta_c_k = 0
C?      su_eta_c_k_sq = 0
C?      mn_eta_v = inf
C?      mx_eta_v = zero
C?      su_eta_v = zero
C?      su_eta_v_sq = zero
C?      mn_eta_pv_v = inf
C?      mx_eta_pv_v = zero
C?      su_eta_pv_v = zero
C?      su_eta_pv_v_sq = zero
C?      mn_eta_growth = inf
C?      mx_eta_growth = zero
C?      su_eta_growth = zero
C?      su_eta_growth_sq = zero
C?      if (n_eta .gt. 0 .and. iand(wr_msk, bt3) .ne. 0) then
C?         if (alw_f7_wr) write(ems_li, 9300)
C?         call ems_msg_wr_li(info_msg_n)
C?      endif
C?      do 230, eta_n = f_up_eta, n_eta
C?         el_n = eta_sa(eta_n)
C?         pv_r = eta_ix(el_n)
C?         pv_v = one/eta_v(el_n)
C?         abs_pv_v = abs(pv_v)
C?         mn_eta_pv_v = min(mn_eta_pv_v, abs_pv_v)
C?         mx_eta_pv_v = max(mx_eta_pv_v, abs_pv_v)
C?         su_eta_pv_v = su_eta_pv_v + abs_pv_v
C?         su_eta_pv_v_sq = su_eta_pv_v_sq + abs_pv_v*abs_pv_v
C?         eta_c_k = eta_sa(eta_n+1)-eta_sa(eta_n)
C?         mx_eta_c_v = zero
C?         do 220, el_n = eta_sa(eta_n)+1, eta_sa(eta_n+1)-1
C?            abs_eta_v = abs(eta_v(el_n))
C?            mx_eta_c_v = max(mx_eta_c_v, abs_eta_v)
C?            mn_eta_v = min(mn_eta_v, abs_eta_v)
C?            mx_eta_v = max(mx_eta_v, abs_eta_v)
C?            su_eta_v = su_eta_v + abs_eta_v
C?            su_eta_v_sq = su_eta_v_sq + abs_eta_v*abs_eta_v
C? 220     continue
C?         eta_growth = mx_eta_c_v/abs_pv_v
C?         if (iand(wr_msk, bt3) .ne. 0) then
C?            if (alw_f7_wr) write(ems_li, 9310)
C?     &           eta_n, eta_c_k,
C?     &           mx_eta_c_v, pv_v, eta_growth
C?            call ems_msg_wr_li(info_msg_n)
C?         endif
C?         mn_eta_c_k = min(eta_c_k, mn_eta_c_k)
C?         mx_eta_c_k = max(eta_c_k, mx_eta_c_k)
C?         su_eta_c_k = su_eta_c_k + eta_c_k
C?         su_eta_c_k_sq = su_eta_c_k_sq + eta_c_k*eta_c_k
C?         mn_eta_growth = min(eta_growth, mn_eta_growth)
C?         mx_eta_growth = max(eta_growth, mx_eta_growth)
C?         su_eta_growth = su_eta_growth + eta_growth
C?         su_eta_growth_sq = su_eta_growth_sq + eta_growth*eta_growth
C? 230  continue
C?      n_up_eta = n_eta - n_lo_eta
C?      n_up_eta_el = n_eta_el - eta_sa(f_up_eta) + 1
C?      if (alw_f7_wr) write(ems_li, 9110)'     U-',
C?     &     n_up_eta, n_up_eta_el
C?      call ems_msg_wr_li(info_msg_n)
C?      if (n_up_eta .gt. 0) then
C?         if (alw_f7_wr) write(ems_li, 9111)
C?         call ems_msg_wr_li(info_msg_n)
C?         av_eta_c_k = su_eta_c_k/float(n_up_eta)
C?         dev_eta_c_k = su_eta_c_k_sq/float(n_up_eta) -
C?     &        av_eta_c_k*av_eta_c_k
C?         dev_eta_c_k = sqrt(abs(dev_eta_c_k))
C?         if (alw_f7_wr) write(ems_li, 9112)'Counts ',
C?     &        mn_eta_c_k, mx_eta_c_k, av_eta_c_k, dev_eta_c_k
C?         call ems_msg_wr_li(info_msg_n)
C?         av_eta_v = su_eta_v/float(n_up_eta_el-n_up_eta)
C?         dev_eta_v = su_eta_v_sq/float(n_up_eta_el-n_up_eta) -
C?     &        av_eta_v*av_eta_v
C?         dev_eta_v = sqrt(abs(dev_eta_v))
C?         if (alw_f7_wr) write(ems_li, 9113)'Values ',
C?     &        mn_eta_v, mx_eta_v,
C?     &        av_eta_v, dev_eta_v
C?         call ems_msg_wr_li(info_msg_n)
C?         av_eta_pv_v = su_eta_pv_v/float(n_up_eta)
C?         dev_eta_pv_v = su_eta_pv_v_sq/float(n_up_eta) -
C?     &        av_eta_pv_v*av_eta_pv_v
C?         dev_eta_pv_v = sqrt(abs(dev_eta_pv_v))
C?         if (alw_f7_wr) write(ems_li, 9113)'Pivots ',
C?     &        mn_eta_pv_v, mx_eta_pv_v,
C?     &        av_eta_pv_v, dev_eta_pv_v
C?         call ems_msg_wr_li(info_msg_n)
C?         av_eta_growth = su_eta_growth/float(n_up_eta)
C?         dev_eta_growth = su_eta_growth_sq/float(n_up_eta) -
C?     &        av_eta_growth*av_eta_growth
C?         dev_eta_growth = sqrt(abs(dev_eta_growth))
C?         if (alw_f7_wr) write(ems_li, 9113)'Growth ',
C?     &        mn_eta_growth, mx_eta_growth,
C?     &        av_eta_growth, dev_eta_growth
C?         call ems_msg_wr_li(info_msg_n)
C?      endif
C?      if (alw_f7_wr) write(ems_li, 9110)'INVERT-',
C?     &     n_eta, n_eta_el
C?      call ems_msg_wr_li(info_msg_n)
C?      if (alw_f7_wr) write(ems_li, 9121)
C?     &     n_r-n_lg, n_mtx_nz_el-n_lg, mtx_1_norm
C?      call ems_msg_wr_li(info_msg_n)
C?      if (alw_f7_wr) write(ems_li, 9122)
C?     &     n_eta, n_eta_el, y_norm/x_norm
C?      call ems_msg_wr_li(info_msg_n)
C?      if (n_mtx_nz_el .gt. n_lg) then
C?         if (alw_f7_wr) write(ems_li, 9123)
C?     &        float(n_eta_el)/float(n_mtx_nz_el-n_lg), cond
C?         call ems_msg_wr_li(info_msg_n)
C?      endif
C?      if (inv_pic .and. n_eta .le. 2000) then
C?c
C?c     rhs_ix can now be used as workspace
C?c
C?         call ems_inv_pic(
C?     &        alw_f7_wr, ems_msg_wr_cn,
C?     &        'eta.pic', .true.,
C?     &        n_r, lc_i_wk_a_n_en,
C?     &        n_lo_eta, n_eta_el,
C?     &        n_lo_eta+1, n_eta, n_eta_el,
C?     &        rhs_ix, lc_i_wk_a,
C?     &        eta_v, eta_ix, eta_sa,
C?     &        eta_v, eta_ix, eta_sa,
C?     &        -1)
C?      endif
C?      return
C? 9000 format('Factored inverse of matrix of dimension ', i7, ':')
C?
C? 9110 format(a7, 'etas (', i7, ') have ', i7, ' nonzero entries')
C? 9111 format('              Min          Max      Average    Deviation')
C? 9112 format(a7, ':', 2(2x, i7, 4x), 2(2x, g11.4))
C? 9113 format(a7, ':', 2(2x, g11.4),  2(2x, g11.4))
C?
C? 9121 format('Matrix has ',
C?     &     i7, ' structural columns and ',
C?     &     i9, ' entries: ||B|| =           ', g10.4)
C? 9122 format('INVERT has ',
C?     &     i7, ' etas               and ',
C?     &     i9, ' entries: ||B^{-1}|| approx ', g10.4)
C? 9123 format('Fill-factor = ', f7.2, ' Condition approx ', g10.4)
C? 9300 format('    eta_n           eta_c_k',
C?     &     '     mx_v        pivot       growth')
C? 9310 format(2x, i7, 11x, i7, 3(2x, g11.4))
C?      end
C?
CM      ENDIF
