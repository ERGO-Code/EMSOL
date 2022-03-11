C->>> --------------------------------------------> ems_g_rq_ds_n_en <<<
      subroutine ems_g_mem_rq(
     &     usr_n_ml, usr_n_r, usr_n_c, usr_n_a_el, op_msk,
     &     rq_mem_lb, rq_mem_est, rq_mem_ub)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMSG.INC'
      include 'RSMIHDL.INC'
      include 'SLAPCS.INC'
      include 'ICTVR.INC'
      integer usr_n_ml, usr_n_r, usr_n_c, usr_n_a_el, op_msk
      integer rq_mem_lb, rq_mem_est, rq_mem_ub
      integer r_cf, c_cf, a_el_cf, cs, n_wo
c      integer alt_r_cf, alt_c_cf, alt_a_el_cf, alt_cs, alt_n_wo
      integer tot_r_cf, tot_c_cf, tot_a_el_cf
      integer sv_n_r, sv_n_c, sv_n_a_el
      integer sv_mx_n_r, sv_mx_n_c, sv_mx_n_a_el, sv_mx_n_u
      integer bs_mtx_n_el, mx_n_eta, mx_n_el
      integer sv_rsmi_blk_mx_n_r, sv_rsmi_blk_mx_n_c
      integer sv_n_lo_r_eta_el, sv_n_up_r_eta_el
      double precision est_fill_fac
      parameter (est_fill_fac = 2d0)
 
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
      call ems_msg_wr_li(info_msg_n)
c      print*, 'Enter est_fill_fac'
c      read*, est_fill_fac
      sv_n_r = n_r
      sv_n_c = n_c
      sv_n_a_el = n_a_el
      sv_mx_n_r = mx_n_r
      sv_mx_n_c = mx_n_c
      sv_mx_n_a_el = mx_n_a_el
      sv_mx_n_u = mx_n_u
      sv_rsmi_blk_mx_n_r = rsmi_blk_mx_n_r
      sv_rsmi_blk_mx_n_c = rsmi_blk_mx_n_c
      sv_n_lo_r_eta_el = n_lo_r_eta_el
      sv_n_up_r_eta_el = n_up_r_eta_el
 
      n_r = usr_n_r
      n_c = usr_n_c
      n_a_el = usr_n_a_el
      mx_n_r = usr_n_r
      mx_n_c = usr_n_c
      mx_n_a_el = usr_n_a_el
      mx_n_u = usr_mx_n_u
      rsmi_blk_mx_n_r = mx_n_r
      rsmi_blk_mx_n_c = mx_n_c
 
      tot_r_cf = 0
      tot_c_cf = 0
      tot_a_el_cf = 0
c
c     Initialise the memory requirements to an appropriate constant.
c
      rq_mem_lb =  2000
      rq_mem_est = 2000
      rq_mem_ub =  2000
c=======================================================================
c     Temporary workspace for the Tomlin INVERT
c
c      call ems_g_blk_tom_inv_wk_n_wo(n_r, mx_n_c,
c     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      tot_r_cf = tot_r_cf + r_cf
      tot_c_cf = tot_c_cf + c_cf
      tot_a_el_cf = tot_a_el_cf + a_el_cf
      rq_mem_lb = rq_mem_lb + n_wo
      rq_mem_est = rq_mem_est + n_wo
c      rq_mem_ub = rq_mem_ub + n_wo !Accommodated in Ub for UPDATE
c=======================================================================
c     Temporary workspace for the LTS(S)F crash
c     Uses less space than INVERT and never allocated simultaneously
c
c      call ems_g_ltssf_blk_crsh_n_wo(r_cf, c_cf, a_el_cf, cs, n_wo)
c      call ems_g_ltsf_blk_crsh_n_wo(
c     &     alt_r_cf, alt_c_cf, alt_a_el_cf, alt_cs, alt_n_wo)
c      r_cf = max(r_cf, alt_r_cf)
c      c_cf = max(c_cf, alt_c_cf)
c      a_el_cf = max(a_el_cf, alt_a_el_cf)
c      n_wo = max(n_wo, alt_n_wo)
 
CM      IF (emsol_xa .EQ. 1) THEN
C?c=======================================================================
C?c     Space for presolve data to save
C?c
C?      call ems_g_blk_ml_prsl_sv_n_wo(r_cf, c_cf, a_el_cf, cs, n_wo)
C?      tot_r_cf = tot_r_cf + r_cf
C?      tot_c_cf = tot_c_cf + c_cf
C?      tot_a_el_cf = tot_a_el_cf + a_el_cf
C?      rq_mem_lb = rq_mem_lb + n_wo
C?      rq_mem_est = rq_mem_est + n_wo
C?      rq_mem_ub = rq_mem_ub + n_wo
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)
C?     &     'ml_prsl_sv     ', r_cf, c_cf, a_el_cf, cs,
C?     &     n_wo, rq_mem_lb, rq_mem_est, rq_mem_ub
C?      call ems_msg_wr_li(info_msg_n)
CM      ENDIF
c=======================================================================
c     Space for linear model vectors.
c
      call ems_g_blk_ml_vec_n_wo(r_cf, c_cf, a_el_cf, cs, n_wo)
      tot_r_cf = tot_r_cf + r_cf
      tot_c_cf = tot_c_cf + c_cf
      tot_a_el_cf = tot_a_el_cf + a_el_cf
      rq_mem_lb = rq_mem_lb + n_wo
      rq_mem_est = rq_mem_est + n_wo
      rq_mem_ub = rq_mem_ub + n_wo
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)
     &     'ml_vec         ', r_cf, c_cf, a_el_cf, cs,
     &     n_wo, rq_mem_lb, rq_mem_est, rq_mem_ub
      call ems_msg_wr_li(info_msg_n)
 
c=======================================================================
c     Space for long model names.
c
      if (ml_nm_n_ch .ne. 8) then
         call ems_g_blk_ml_lng_nm_n_wo(r_cf, c_cf, a_el_cf, cs, n_wo)
         tot_r_cf = tot_r_cf + r_cf
         tot_c_cf = tot_c_cf + c_cf
         tot_a_el_cf = tot_a_el_cf + a_el_cf
         rq_mem_lb = rq_mem_lb + n_wo
         rq_mem_est = rq_mem_est + n_wo
         rq_mem_ub = rq_mem_ub + n_wo
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)
     &        'ml_lng_nm      ', r_cf, c_cf, a_el_cf, cs,
     &        n_wo, rq_mem_lb, rq_mem_est, rq_mem_ub
         call ems_msg_wr_li(info_msg_n)
      endif
 
c=======================================================================
c     Space for model solution.
c
      call ems_g_blk_ml_sol_n_wo(r_cf, c_cf, a_el_cf, cs, n_wo)
      tot_r_cf = tot_r_cf + r_cf
      tot_c_cf = tot_c_cf + c_cf
      tot_a_el_cf = tot_a_el_cf + a_el_cf
      rq_mem_lb = rq_mem_lb + n_wo
      rq_mem_est = rq_mem_est + n_wo
      rq_mem_ub = rq_mem_ub + n_wo
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)
     &     'ml_sol         ', r_cf, c_cf, a_el_cf, cs,
     &     n_wo, rq_mem_lb, rq_mem_est, rq_mem_ub
      call ems_msg_wr_li(info_msg_n)
 
c=======================================================================
c     Space for model variable lists.
c
      call ems_g_blk_ml_vr_ls_n_wo(r_cf, c_cf, a_el_cf, cs, n_wo)
      tot_r_cf = tot_r_cf + r_cf
      tot_c_cf = tot_c_cf + c_cf
      tot_a_el_cf = tot_a_el_cf + a_el_cf
      rq_mem_lb = rq_mem_lb + n_wo
      rq_mem_est = rq_mem_est + n_wo
      rq_mem_ub = rq_mem_ub + n_wo
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)
     &     'ml_vr_ls       ', r_cf, c_cf, a_el_cf, cs,
     &     n_wo, rq_mem_lb, rq_mem_est, rq_mem_ub
      call ems_msg_wr_li(info_msg_n)
 
c=======================================================================
c     Space for column-wise constraint matrix.
c
      call ems_g_blk_ml_c_mtx_n_wo(r_cf, c_cf, a_el_cf, cs, n_wo)
      tot_r_cf = tot_r_cf + r_cf
      tot_c_cf = tot_c_cf + c_cf
      tot_a_el_cf = tot_a_el_cf + a_el_cf
      rq_mem_lb = rq_mem_lb + n_wo
      rq_mem_est = rq_mem_est + n_wo
      rq_mem_ub = rq_mem_ub + n_wo
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)
     &     'ml_c_mtx       ', r_cf, c_cf, a_el_cf, cs,
     &     n_wo, rq_mem_lb, rq_mem_est, rq_mem_ub
      call ems_msg_wr_li(info_msg_n)
 
c=======================================================================
c     Space for row-wise constraint matrix.
c
      if (r_pc .ne. 0) then
         call ems_g_blk_ml_r_mtx_n_wo(r_cf, c_cf, a_el_cf, cs, n_wo)
         tot_r_cf = tot_r_cf + r_cf
         tot_c_cf = tot_c_cf + c_cf
         tot_a_el_cf = tot_a_el_cf + a_el_cf
         rq_mem_lb = rq_mem_lb + n_wo
         rq_mem_est = rq_mem_est + n_wo
         rq_mem_ub = rq_mem_ub + n_wo
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)
     &        'ml_r_mtx       ', r_cf, c_cf, a_el_cf, cs,
     &        n_wo, rq_mem_lb, rq_mem_est, rq_mem_ub
         call ems_msg_wr_li(info_msg_n)
      endif
 
c=======================================================================
c     Space for the auxiliary solve region.
c
      call ems_g_blk_ml_aux_sol_n_wo(r_cf, c_cf, a_el_cf, cs, n_wo)
c      tot_r_cf = tot_r_cf + r_cf
c      tot_c_cf = tot_c_cf + c_cf
c      tot_a_el_cf = tot_a_el_cf + a_el_cf
c      rq_mem_lb = rq_mem_lb + n_wo
c      rq_mem_est = rq_mem_est + n_wo
c      rq_mem_ub = rq_mem_ub + n_wo
c      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)
c     &     'ml_aux_sol     ', r_cf, c_cf, a_el_cf, cs,
c     &     n_wo, rq_mem_lb, rq_mem_est, rq_mem_ub
c      call ems_msg_wr_li(info_msg_n)
 
c=======================================================================
c     Space for the auxiliary block.
c
      call ems_g_blk_ml_aux_blk_n_wo(r_cf, c_cf, a_el_cf, cs, n_wo)
c      tot_r_cf = tot_r_cf + r_cf
c      tot_c_cf = tot_c_cf + c_cf
c      tot_a_el_cf = tot_a_el_cf + a_el_cf
c      rq_mem_lb = rq_mem_lb + n_wo
c      rq_mem_est = rq_mem_est + n_wo
c      rq_mem_ub = rq_mem_ub + n_wo
c      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)
c     &     'ml_aux_blk     ', r_cf, c_cf, a_el_cf, cs,
c     &     n_wo, rq_mem_lb, rq_mem_est, rq_mem_ub
c      call ems_msg_wr_li(info_msg_n)
 
c=======================================================================
c     Space for a column-wise copy of the constraint matrix for the user
c
      call ems_g_blk_ml_usr_c_mtx_n_wo(r_cf, c_cf, a_el_cf, cs, n_wo)
c      tot_r_cf = tot_r_cf + r_cf
c      tot_c_cf = tot_c_cf + c_cf
c      tot_a_el_cf = tot_a_el_cf + a_el_cf
c      rq_mem_lb = rq_mem_lb + n_wo
c      rq_mem_est = rq_mem_est + n_wo
c      rq_mem_ub = rq_mem_ub + n_wo
c      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)
c     &     'ml_usr_c_mtx   ', r_cf, c_cf, a_el_cf, cs,
c     &     n_wo, rq_mem_lb, rq_mem_est, rq_mem_ub
c      call ems_msg_wr_li(info_msg_n)
 
c=======================================================================
c     Space for a row-wise copy of the constraint matrix for the user.
c
      call ems_g_blk_ml_usr_r_mtx_n_wo(r_cf, c_cf, a_el_cf, cs, n_wo)
c      tot_r_cf = tot_r_cf + r_cf
c      tot_c_cf = tot_c_cf + c_cf
c      tot_a_el_cf = tot_a_el_cf + a_el_cf
c      rq_mem_lb = rq_mem_lb + n_wo
c      rq_mem_est = rq_mem_est + n_wo
c      rq_mem_ub = rq_mem_ub + n_wo
c      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)
c     &     'ml_usr_r_mtx   ', r_cf, c_cf, a_el_cf, cs,
c     &     n_wo, rq_mem_lb, rq_mem_est, rq_mem_ub
c      call ems_msg_wr_li(info_msg_n)
 
c=======================================================================
c     Space for Devex indices---may have to switch to Devex even when
c     using steepest edge
c
      if (dvx_mode .ne. pc_alg_dan) then
         call ems_g_blk_dvx_n_wo(r_cf, c_cf, a_el_cf, cs, n_wo)
         tot_r_cf = tot_r_cf + r_cf
         tot_c_cf = tot_c_cf + c_cf
         tot_a_el_cf = tot_a_el_cf + a_el_cf
         rq_mem_lb = rq_mem_lb + n_wo
         rq_mem_est = rq_mem_est + n_wo
         rq_mem_ub = rq_mem_ub + n_wo
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)
     &        'dvx            ', r_cf, c_cf, a_el_cf, cs,
     &        n_wo, rq_mem_lb, rq_mem_est, rq_mem_ub
         call ems_msg_wr_li(info_msg_n)
      endif
c=======================================================================
c     Space for INVERT:
c
c     For lower bound:
c     Assume basis contains constraint matrix entries pro rata
c     Assume no fill-in
c
      bs_mtx_n_el = int(float(n_a_el)*float(min(n_r, n_c))/float(n_c))
      mx_n_eta = 2*min(n_r, n_c)
      mx_n_el = bs_mtx_n_el
      call ems_g_eta_grp_n_wo(
     &     rsmi_eta_grp_ty, sto_pk_eta_v, mx_n_eta, mx_n_el, n_wo)
      rq_mem_lb = rq_mem_lb + n_wo
c
c     Space for row-wise eta file:
c
      if (iand(eta_fi_mode_msk, eta_fi_r_eta_y_bt) .ne. 0 .or.
     &     iand(eta_fi_mode_msk, eta_fi_r_eta_poss_bt) .ne. 0) then
         n_lo_r_eta_el = 0
         n_up_r_eta_el = mx_n_el
         call ems_g_blk_r_eta_fi_en_n_wo(
     &        r_cf, c_cf, a_el_cf, cs, n_wo)
         rq_mem_lb = rq_mem_lb + n_wo
      endif
c
c     For estimate:
c     Assume basis contains constraint matrix entries pro rata
c     Assume fill-factor of est_fill_fac
c
      bs_mtx_n_el = int(float(n_a_el)*float(min(n_r, n_c))/float(n_c))
      mx_n_eta = 2*min(n_r, n_c)
      mx_n_el = est_fill_fac*bs_mtx_n_el
      call ems_g_eta_grp_n_wo(
     &     rsmi_eta_grp_ty, sto_pk_eta_v, mx_n_eta, mx_n_el, n_wo)
c      write(*, '(a, 3i9, 2x, f5.2, i9)')
c     &     '###EMSMEM INVERT###: ', bs_mtx_n_el, mx_n_eta, mx_n_el,
c     &     est_fill_fac, n_wo
      rq_mem_est = rq_mem_est + n_wo
c
c     Space for row-wise eta file:
c
      if (iand(eta_fi_mode_msk, eta_fi_r_eta_y_bt) .ne. 0 .or.
     &     iand(eta_fi_mode_msk, eta_fi_r_eta_poss_bt) .ne. 0) then
         n_lo_r_eta_el = 0
         n_up_r_eta_el = mx_n_el
         call ems_g_blk_r_eta_fi_en_n_wo(
     &        r_cf, c_cf, a_el_cf, cs, n_wo)
         rq_mem_est = rq_mem_est + n_wo
      endif
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)
     &     'INVERT eta file', r_cf, c_cf, a_el_cf, cs,
     &     n_wo, rq_mem_lb, rq_mem_est, rq_mem_ub
      call ems_msg_wr_li(info_msg_n)
      if (iand(eta_fi_mode_msk, eta_fi_r_eta_y_bt) .ne. 0 .or.
     &     iand(eta_fi_mode_msk, eta_fi_r_eta_poss_bt) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)
     &        'r_eta_fi_en    ', r_cf, c_cf, a_el_cf, cs,
     &        n_wo, rq_mem_lb, rq_mem_est, rq_mem_ub
         call ems_msg_wr_li(info_msg_n)
      endif
c
c     For upper bound:
c     Assume basis contains all constraint matrix entries
c     Assume total fill-in in structural columns
c
      bs_mtx_n_el = n_a_el
      mx_n_eta = 2*min(n_r, n_c)
      mx_n_el = n_r*min(n_r, n_c)
      call ems_g_eta_grp_n_wo(
     &     rsmi_eta_grp_ty, sto_pk_eta_v, mx_n_eta, mx_n_el, n_wo)
      rq_mem_ub = rq_mem_ub + n_wo
c
c     Space for row-wise eta file:
c
      if (iand(eta_fi_mode_msk, eta_fi_r_eta_y_bt) .ne. 0 .or.
     &     iand(eta_fi_mode_msk, eta_fi_r_eta_poss_bt) .ne. 0) then
         n_lo_r_eta_el = 0
         n_up_r_eta_el = mx_n_el
         call ems_g_blk_r_eta_fi_en_n_wo(
     &        r_cf, c_cf, a_el_cf, cs, n_wo)
         rq_mem_ub = rq_mem_ub + n_wo
      endif
c=======================================================================
c     Space for INVERT pointers
c
      call ems_g_blk_ml_bs_inv_p_n_wo(r_cf, c_cf, a_el_cf, cs, n_wo)
      tot_r_cf = tot_r_cf + r_cf
      tot_c_cf = tot_c_cf + c_cf
      tot_a_el_cf = tot_a_el_cf + a_el_cf
      rq_mem_lb = rq_mem_lb + n_wo
      rq_mem_est = rq_mem_est + n_wo
      rq_mem_ub = rq_mem_ub + n_wo
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)
     &     'ml_bs_inv_p    ', r_cf, c_cf, a_el_cf, cs,
     &     n_wo, rq_mem_lb, rq_mem_est, rq_mem_ub
      call ems_msg_wr_li(info_msg_n)
c=======================================================================
c     Space for UPDATE:
c
c     For lower bound:
c     Assume update etas are same density as constraint matrix columns
c
      mx_n_eta = mx_n_u
      mx_n_el = int(float(mx_n_u)*float(n_a_el)/float(n_c))
      call ems_g_eta_grp_n_wo(
     &     rsmi_eta_grp_ty, sto_pk_eta_v, mx_n_eta, mx_n_el, n_wo)
      rq_mem_lb = rq_mem_lb + n_wo
c
c     For estimate:
c     Assume update etas are full
c
      mx_n_eta = mx_n_u
      mx_n_el = mx_n_u*n_r + n_r
      call ems_g_eta_grp_n_wo(
     &     rsmi_eta_grp_ty, sto_pk_eta_v, mx_n_eta, mx_n_el, n_wo)
      rq_mem_est = rq_mem_est + n_wo
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)
     &     'UPDATE eta file', r_cf, c_cf, a_el_cf, cs,
     &     n_wo, rq_mem_lb, rq_mem_est, rq_mem_ub
      call ems_msg_wr_li(info_msg_n)
c      write(*, '(a, 3i9, 2x, f5.2)')
c     &     '###EMSMEM UPDATE###: ', mx_n_eta, mx_n_el, n_wo
c
c     For upper bound:
c     Assume update etas are full
c
      mx_n_eta = mx_n_u
      mx_n_el = mx_n_u*n_r
      call ems_g_eta_grp_n_wo(
     &     rsmi_eta_grp_ty, sto_pk_eta_v, mx_n_eta, mx_n_el, n_wo)
      rq_mem_ub = rq_mem_ub + n_wo
c=======================================================================
c     Space for UPDATE dense block
c
      call ems_g_blk_ml_u_bs_n_wo(r_cf, c_cf, a_el_cf, cs, n_wo)
      tot_r_cf = tot_r_cf + r_cf
      tot_c_cf = tot_c_cf + c_cf
      tot_a_el_cf = tot_a_el_cf + a_el_cf
      rq_mem_lb = rq_mem_lb + n_wo
      rq_mem_est = rq_mem_est + n_wo
      rq_mem_ub = rq_mem_ub + n_wo
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)
     &     'ml_u_bs        ', r_cf, c_cf, a_el_cf, cs,
     &     n_wo, rq_mem_lb, rq_mem_est, rq_mem_ub
      call ems_msg_wr_li(info_msg_n)
 
c=======================================================================
c     Space for RSMI vectors.
c
      call ems_g_blk_rsmi_n_wo(r_cf, c_cf, a_el_cf, cs, n_wo)
      tot_r_cf = tot_r_cf + r_cf
      tot_c_cf = tot_c_cf + c_cf
      tot_a_el_cf = tot_a_el_cf + a_el_cf
      rq_mem_lb = rq_mem_lb + n_wo
      rq_mem_est = rq_mem_est + n_wo
      rq_mem_ub = rq_mem_ub + n_wo
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)
     &     'rsmi           ', r_cf, c_cf, a_el_cf, cs,
     &     n_wo, rq_mem_lb, rq_mem_est, rq_mem_ub
      call ems_msg_wr_li(info_msg_n)
 
c=======================================================================
c     Space for RSMI workspace.
c
      call ems_g_blk_rsmi_wk_n_wo(r_cf, c_cf, a_el_cf, cs, n_wo)
      tot_r_cf = tot_r_cf + r_cf
      tot_c_cf = tot_c_cf + c_cf
      tot_a_el_cf = tot_a_el_cf + a_el_cf
      rq_mem_lb = rq_mem_lb + n_wo
      rq_mem_est = rq_mem_est + n_wo
      rq_mem_ub = rq_mem_ub + n_wo
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)
     &     'rsmi_wk        ', r_cf, c_cf, a_el_cf, cs,
     &     n_wo, rq_mem_lb, rq_mem_est, rq_mem_ub
      call ems_msg_wr_li(info_msg_n)
 
c=======================================================================
c     Space for RSMI bound vectors.
c
      call ems_g_blk_rsmi_bd_n_wo(r_cf, c_cf, a_el_cf, cs, n_wo)
      tot_r_cf = tot_r_cf + r_cf
      tot_c_cf = tot_c_cf + c_cf
      tot_a_el_cf = tot_a_el_cf + a_el_cf
      rq_mem_lb = rq_mem_lb + n_wo
      rq_mem_est = rq_mem_est + n_wo
      rq_mem_ub = rq_mem_ub + n_wo
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)
     &     'rsmi_bd        ', r_cf, c_cf, a_el_cf, cs,
     &     n_wo, rq_mem_lb, rq_mem_est, rq_mem_ub
      call ems_msg_wr_li(info_msg_n)
 
c=======================================================================
c     Space for L1_CZ_R.
c
      call ems_g_blk_l1_cz_r_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      tot_r_cf = tot_r_cf + r_cf
      tot_c_cf = tot_c_cf + c_cf
      tot_a_el_cf = tot_a_el_cf + a_el_cf
      rq_mem_lb = rq_mem_lb + n_wo
      rq_mem_est = rq_mem_est + n_wo
      rq_mem_ub = rq_mem_ub + n_wo
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)
     &     'l1_cz_r        ', r_cf, c_cf, a_el_cf, cs,
     &     n_wo, rq_mem_lb, rq_mem_est, rq_mem_ub
      call ems_msg_wr_li(info_msg_n)
 
c=======================================================================
c     Space for row-wise eta file pointers
c
      if (iand(eta_fi_mode_msk, eta_fi_r_eta_y_bt) .ne. 0 .or.
     &     iand(eta_fi_mode_msk, eta_fi_r_eta_poss_bt) .ne. 0) then
         call ems_g_blk_r_eta_fi_p_n_wo(
     &        r_cf, c_cf, a_el_cf, cs, n_wo)
         tot_r_cf = tot_r_cf + r_cf
         tot_c_cf = tot_c_cf + c_cf
         tot_a_el_cf = tot_a_el_cf + a_el_cf
         rq_mem_lb = rq_mem_lb + n_wo
         rq_mem_est = rq_mem_est + n_wo
         rq_mem_ub = rq_mem_ub + n_wo
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)
     &        'r_eta_fi_p     ', r_cf, c_cf, a_el_cf, cs,
     &        n_wo, rq_mem_lb, rq_mem_est, rq_mem_ub
         call ems_msg_wr_li(info_msg_n)
      endif
 
c=======================================================================
c     Space for ranging data
c
      call ems_g_blk_ml_rg_da_n_wo(r_cf, c_cf, a_el_cf, cs, n_wo)
      tot_r_cf = tot_r_cf + r_cf
      tot_c_cf = tot_c_cf + c_cf
      tot_a_el_cf = tot_a_el_cf + a_el_cf
      rq_mem_lb = rq_mem_lb + n_wo
      rq_mem_est = rq_mem_est + n_wo
      rq_mem_ub = rq_mem_ub + n_wo
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)
     &     'ml_rg_da       ', r_cf, c_cf, a_el_cf, cs,
     &     n_wo, rq_mem_lb, rq_mem_est, rq_mem_ub
      call ems_msg_wr_li(info_msg_n)
c
c     Turn the memory requirements into doubleword requirements
c
      rq_mem_lb = rq_mem_lb/rl_wo_z
      rq_mem_est = rq_mem_est/rl_wo_z
      rq_mem_ub = rq_mem_ub/rl_wo_z
c
c     Recover control variables
c
      n_r = sv_n_r
      n_c = sv_n_c
      n_a_el = sv_n_a_el
      mx_n_r = sv_mx_n_r
      mx_n_c = sv_mx_n_c
      mx_n_a_el = sv_mx_n_a_el
      mx_n_u = sv_mx_n_u
      rsmi_blk_mx_n_r = sv_rsmi_blk_mx_n_r
      rsmi_blk_mx_n_c = sv_rsmi_blk_mx_n_c
      n_lo_r_eta_el = sv_n_lo_r_eta_el
      n_up_r_eta_el = sv_n_up_r_eta_el
      return
 9000 format('Block             R   C  El       Cs        NWo',
     &     '             Lb            Est             Ub')
 9010 format(a15, 3i4, i9, 2x, i9, 3(i15))
      end
C->>> --------------------------------------------> ems_g_rq_ds_n_en <<<
c     Returns the required maximum dspace which would allow a block of
c     n_wo fullwords to be allocated.
c
c     Possible return code values set in this routine:
c     -----------------------------------------------
c
c     Routines called:
c     ---------------
c
      subroutine ems_g_rq_ds_n_en(rt_cod, is,
     &     n_wo, cu_rq_ds_n_en, mx_rq_ds_n_en)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      integer rt_cod, is(0:*), n_wo, cu_rq_ds_n_en, mx_rq_ds_n_en
      logical ems_mem_mgr_no_ca_iz
      integer mem_mgr_rt_cod
      integer cu_rq_is_n_en, mx_rq_is_n_en
 
      rt_cod = 0
      if (ems_mem_mgr_no_ca_iz(is)) goto 8000
      call ems_mem_mgr_g_rq_is_n_en(mem_mgr_rt_cod, is,
     &     n_wo, cu_rq_is_n_en, mx_rq_is_n_en)
      if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
         if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8900
      endif
      cu_rq_ds_n_en = (cu_rq_is_n_en+rl_wo_z-1)/rl_wo_z
      mx_rq_ds_n_en = (mx_rq_is_n_en+rl_wo_z-1)/rl_wo_z
c 7000 continue
 7100 continue
      return
 8000 continue
      rt_cod = max(mem_mgr_rt_cod_no_ca_iz,
     &     rt_cod)
      goto 7100
 8900 continue
      rt_cod = max(mem_mgr_rt_cod,
     &     rt_cod)
      goto 7100
      end
