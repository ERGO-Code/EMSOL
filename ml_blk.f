CM
C->>> -------------------------------------------> ems_iz_blk_ml_vec <<<
c     Sets up block and handles for the linear model vectors.
c
      subroutine ems_iz_blk_ml_vec(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer blk_n, r_cf, c_cf, a_el_cf, cs, n_wo, p_ml_hdl
      integer mem_mgr_rt_cod
      integer rt_cod
 
      if (iand(ml_blk_st_msk,  ml_blk_st_ml_vec) .ne. 0) goto 8000
      call ems_g_blk_ml_vec_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      call ems_mem_mgr_ope_blk(mem_mgr_rt_cod, is,
     &     n_wo, ope_blk_at_f_fr_p,
     &     cu_ml_n, ml_vec_blk_id, blk_n)
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
     &     c_cf, mx_n_c,
     &     0, n_a_el,
     &     cs, n_wo, ml_vec_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         go to 7000
      endif
c
c     Get handles for the arrays within the block.
c
      p_ml_hdl = p_ml_bs_blk + (cu_ml_n-1)*ml_bs_blk_n_wo +
     &     ml_bs_blk_os_hdl
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+mx_n_c+mx_n_r, rl_wo_z,
     &     is(p_ml_hdl+ix_hdl_lbc))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+mx_n_c+mx_n_r, rl_wo_z,
     &     is(p_ml_hdl+ix_hdl_ubc))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+mx_n_c+mx_n_r, rl_wo_z,
     &     is(p_ml_hdl+ix_hdl_cbp))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+mx_n_c+mx_n_r, ch_wo_z,
     &     is(p_ml_hdl+ix_hdl_nm))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_g_ml_p(rt_cod, is)
      if (rt_cod .ne. 0) goto 8020
c
c     Indicate that the model has space for vectors
c
      ml_blk_st_msk = ml_blk_st_msk + ml_blk_st_ml_vec
c
c     Indicate that no model has yet been loaded
c
      ml_da_st_msk = ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_ld)
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
 9800 format('Model already has space for vectors')
 9802 format('Error in ems_g_ml_p')
      end
 
C->>> ---------------------------------------> ems_g_blk_ml_vec_n_wo <<<
      subroutine ems_g_blk_ml_vec_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      integer r_cf, c_cf, a_el_cf, cs, n_wo
      r_cf = 4*rl_wo_z
      c_cf = 4*rl_wo_z
      a_el_cf = 0
      cs =   4*rl_wo_z
      n_wo = r_cf*mx_n_r + c_cf*mx_n_c + cs
      return
      end
 
C->>> ----------------------------------------> ems_iz_blk_ml_lng_nm <<<
c     Sets up block and handles for long model names.
c
      subroutine ems_iz_blk_ml_lng_nm(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer blk_n, r_cf, c_cf, a_el_cf, cs, n_wo, p_ml_hdl
      integer mem_mgr_rt_cod
      integer rt_cod
 
      if (iand(ml_blk_st_msk,  ml_blk_st_ml_lng_nm) .ne. 0) goto 8000
      call ems_g_blk_ml_lng_nm_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      call ems_mem_mgr_ope_blk(mem_mgr_rt_cod, is,
     &     n_wo, ope_blk_at_f_fr_p,
     &     cu_ml_n, ml_lng_nm_blk_id, blk_n)
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
     &     c_cf, mx_n_c,
     &     0, n_a_el,
     &     cs, n_wo, ml_lng_nm_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         go to 7000
      endif
c
c     Get handles for the arrays within the block.
c
      p_ml_hdl = p_ml_bs_blk + (cu_ml_n-1)*ml_bs_blk_n_wo +
     &     ml_bs_blk_os_hdl
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, ml_nm_n_rl*(1+mx_n_c+mx_n_r), rl_wo_z,
     &     is(p_ml_hdl+ix_hdl_lng_nm))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_g_ml_p(rt_cod, is)
      if (rt_cod .ne. 0) goto 8020
      ml_blk_st_msk = ml_blk_st_msk + ml_blk_st_ml_lng_nm
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
 9800 format('Model already has space for long names')
 9802 format('Error in ems_g_ml_p')
      end
 
C->>> ------------------------------------> ems_g_blk_ml_lng_nm_n_wo <<<
      subroutine ems_g_blk_ml_lng_nm_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      integer r_cf, c_cf, a_el_cf, cs, n_wo
      r_cf = ml_nm_n_rl*rl_wo_z
      c_cf = ml_nm_n_rl*rl_wo_z
      a_el_cf = 0
      cs =   ml_nm_n_rl*rl_wo_z
      n_wo = r_cf*mx_n_r + c_cf*mx_n_c + cs
      return
      end
C->>> -------------------------------------------> ems_iz_blk_ml_sol <<<
c     Sets up block and handles for the model solution.
c
      subroutine ems_iz_blk_ml_sol(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer blk_n, r_cf, c_cf, a_el_cf, cs, n_wo, p_ml_hdl
      integer mem_mgr_rt_cod
      integer rt_cod
 
      if (iand(ml_blk_st_msk,  ml_blk_st_ml_sol) .ne. 0) goto 8000
      call ems_g_blk_ml_sol_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      call ems_mem_mgr_ope_blk(mem_mgr_rt_cod, is,
     &     n_wo, ope_blk_at_f_fr_p,
     &     cu_ml_n, ml_sol_blk_id, blk_n)
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
     &     c_cf, mx_n_c,
     &     0, n_a_el,
     &     cs, n_wo, ml_sol_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         go to 7000
      endif
c
c     Get handles for the arrays within the block.
c
      p_ml_hdl = p_ml_bs_blk + (cu_ml_n-1)*ml_bs_blk_n_wo +
     &     ml_bs_blk_os_hdl
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+mx_n_c+mx_n_r, rl_wo_z,
     &     is(p_ml_hdl+ix_hdl_scl))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+mx_n_c+mx_n_r, rl_wo_z,
     &     is(p_ml_hdl+ix_hdl_pr_act))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+mx_n_c+mx_n_r, rl_wo_z,
     &     is(p_ml_hdl+ix_hdl_du_act))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+mx_n_c+mx_n_r, rl_wo_z,
     &     is(p_ml_hdl+ix_hdl_ed_wt))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+mx_n_c+mx_n_r, i_wo_z,
     &     is(p_ml_hdl+ix_hdl_st))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_g_ml_p(rt_cod, is)
      if (rt_cod .ne. 0) goto 8020
c
c     Indicate that the model now has space for the solution.
c
      ml_blk_st_msk = ml_blk_st_msk + ml_blk_st_ml_sol
c
c     Indicate that the model has no basic primal activities, that the
c     status is not consistent with the activities and that it has no
c     nonbasic dual activities or edge weights.
c
      ml_da_st_msk = ml_da_st_msk
     &     - iand(ml_da_st_msk, ml_da_st_bc_pr_act)
     &     - iand(ml_da_st_msk, ml_da_st_vr_st_fm_act)
     &     - iand(ml_da_st_msk, ml_da_st_non_bc_du_act)
     &     - iand(ml_da_st_msk, ml_da_st_ed_wt)
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
 9800 format('Model already has space for vectors')
 9802 format('Error in ems_g_ml_p')
      end
 
C->>> ---------------------------------------> ems_g_blk_ml_sol_n_wo <<<
      subroutine ems_g_blk_ml_sol_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      integer r_cf, c_cf, a_el_cf, cs, n_wo
      r_cf = 4*rl_wo_z + i_wo_z
      c_cf = 4*rl_wo_z + i_wo_z
      a_el_cf = 0
      cs =   4*rl_wo_z + i_wo_z
      n_wo = r_cf*mx_n_r + c_cf*mx_n_c + cs
      return
      end
 
C->>> -----------------------------------------> ems_iz_blk_ml_vr_ls <<<
c     Sets up block and handles for the model variable lists.
c
      subroutine ems_iz_blk_ml_vr_ls(is)
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
      integer blk_n, r_cf, c_cf, a_el_cf, cs, n_wo, p_ml_hdl
      integer mem_mgr_rt_cod
      integer rt_cod
 
      if (iand(ml_blk_st_msk,  ml_blk_st_ml_vr_ls) .ne. 0) goto 8000
      call ems_g_blk_ml_vr_ls_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      call ems_mem_mgr_ope_blk(mem_mgr_rt_cod, is,
     &     n_wo, ope_blk_at_f_fr_p,
     &     cu_ml_n, ml_vr_ls_blk_id, blk_n)
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
     &     c_cf, mx_n_c,
     &     0, n_a_el,
     &     cs, n_wo, ml_vr_ls_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         go to 7000
      endif
c
c     Get handles for the arrays within the block.
c
      p_ml_hdl = p_ml_bs_blk + (cu_ml_n-1)*ml_bs_blk_n_wo +
     &     ml_bs_blk_os_hdl
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+               mx_n_r, i_wo_z,
     &     is(p_ml_hdl+ix_hdl_vr_in_r))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n,
     &     vr_in_c_n_sn+1+mx_n_c+1, i_wo_z,
     &     is(p_ml_hdl+ix_hdl_vr_in_c))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_g_ml_p(rt_cod, is)
      if (rt_cod .ne. 0) goto 8020
c
c     Indicate that the model now has space for the variable lists.
c
      ml_blk_st_msk = ml_blk_st_msk + ml_blk_st_ml_vr_ls
c
c     Indicate the dimension of vr_in_c because there may be other
c     objects with the same structure (in future) which have smaller
c     dimension.
c
      is(p_vr_in_c+vr_in_c_n_sn) = mx_n_c
c
c     Assign the limits on the number of indices of nonzeros which will
c     be maintained in various vectors.
c
      bwd_tran_dse_rhs_n_r = tl_bwd_tran_dse_rhs*n_r
      fwd_tran_dse_rhs_n_r = tl_fwd_tran_dse_rhs*n_r
c
c     Indicate that the model has no vr_in_r or vr_in_c
c
      ml_da_st_msk = ml_da_st_msk
     &     - iand(ml_da_st_msk, ml_da_st_vr_in_r)
     &     - iand(ml_da_st_msk, ml_da_st_vr_in_c)
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
 9800 format('Model already has space for variable lists')
 9802 format('Error in ems_g_ml_p')
      end
 
C->>> -------------------------------------> ems_g_blk_ml_vr_ls_n_wo <<<
      subroutine ems_g_blk_ml_vr_ls_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      integer r_cf, c_cf, a_el_cf, cs, n_wo
      r_cf = i_wo_z
      c_cf = i_wo_z
      a_el_cf = 0
      cs =   (3+vr_in_c_n_sn)*i_wo_z
      n_wo = r_cf*mx_n_r + c_cf*mx_n_c + cs
      return
      end
 
C->>> -----------------------------------------> ems_iz_blk_ml_c_mtx <<<
c     Sets up block and handles for column-wise constraint matrix.
c
      subroutine ems_iz_blk_ml_c_mtx(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer blk_n, r_cf, c_cf, a_el_cf, cs, n_wo, p_ml_hdl
      integer mem_mgr_rt_cod
      integer rt_cod
 
      if (iand(ml_blk_st_msk,  ml_blk_st_ml_c_mtx) .ne. 0) goto 8000
      call ems_g_blk_ml_c_mtx_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      call ems_mem_mgr_ope_blk(mem_mgr_rt_cod, is,
     &     n_wo, ope_blk_at_f_fr_p,
     &     cu_ml_n, ml_c_mtx_blk_id, blk_n)
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
     &     c_cf, mx_n_c,
     &     a_el_cf, mx_n_a_el,
     &     cs, n_wo, ml_c_mtx_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         go to 7000
      endif
c
c     Get handles for the arrays within the block.
c
      p_ml_hdl = p_ml_bs_blk + (cu_ml_n-1)*ml_bs_blk_n_wo +
     &     ml_bs_blk_os_hdl
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+mx_n_a_el, rl_wo_z,
     &     is(p_ml_hdl+ix_hdl_mtx_r_v))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+mx_n_a_el,  i_wo_z,
     &     is(p_ml_hdl+ix_hdl_mtx_r_ix))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
c      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
c     &     blk_n,  1+mx_n_c+1, i_wo_z,
c
c     Changed so that mtx_c_sa can be used to accumulate data for
c     rows---in nwmt?
c
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n,  1+mx_n_c+mx_n_r+1, i_wo_z,
     &     is(p_ml_hdl+ix_hdl_mtx_c_sa))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
c      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
c     &     blk_n,  1+mx_n_c, i_wo_z,
c     &     is(p_ml_hdl+ix_hdl_mtx_c_ln))
c
c     Changed so that mtx_c_ln can be used to store column starts in
c     ems_nw_mtx_in_place.
c
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n,  1+mx_n_c+1, i_wo_z,
     &     is(p_ml_hdl+ix_hdl_mtx_c_ln))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_g_ml_p(rt_cod, is)
      if (rt_cod .ne. 0) goto 8020
c
c     Indicate that the model now has space for a matrix.
c
      ml_blk_st_msk = ml_blk_st_msk + ml_blk_st_ml_c_mtx
c
c     Indicate that no model has yet been loaded
c
      ml_da_st_msk = ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_ld)
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
 9800 format('Model already has space for a matrix')
 9802 format('Error in ems_g_ml_p')
      end
 
C->>> -------------------------------------> ems_g_blk_ml_c_mtx_n_wo <<<
      subroutine ems_g_blk_ml_c_mtx_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      integer r_cf, c_cf, a_el_cf, cs, n_wo
c      r_cf = 0
      r_cf = i_wo_z
      c_cf = 2*i_wo_z
      a_el_cf = rl_wo_z + i_wo_z
      cs = rl_wo_z + 5*i_wo_z
      n_wo = r_cf*mx_n_r + c_cf*mx_n_c + a_el_cf*mx_n_a_el + cs
      return
      end
 
C->>> -----------------------------------------> ems_iz_blk_ml_r_mtx <<<
c     Sets up block and handles for row-wise constraint matrix.
c
      subroutine ems_iz_blk_ml_r_mtx(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer blk_n, r_cf, c_cf, a_el_cf, cs, n_wo, p_ml_hdl
      integer mem_mgr_rt_cod
      integer rt_cod
 
      if (iand(ml_blk_st_msk,  ml_blk_st_ml_r_mtx) .ne. 0) goto 8000
      call ems_g_blk_ml_r_mtx_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      call ems_mem_mgr_ope_blk(mem_mgr_rt_cod, is,
     &     n_wo, ope_blk_at_f_fr_p,
     &     cu_ml_n, ml_r_mtx_blk_id, blk_n)
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
     &     cs, n_wo, ml_r_mtx_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         go to 7000
      endif
c
c     Get handles for the arrays within the block.
c
      p_ml_hdl = p_ml_bs_blk + (cu_ml_n-1)*ml_bs_blk_n_wo +
     &     ml_bs_blk_os_hdl
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+n_a_el, rl_wo_z,
     &     is(p_ml_hdl+ix_hdl_mtx_c_v))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+n_a_el, i_wo_z,
     &     is(p_ml_hdl+ix_hdl_mtx_c_ix))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+n_r+1, i_wo_z,
     &     is(p_ml_hdl+ix_hdl_mtx_r_sa))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_g_ml_p(rt_cod, is)
      if (rt_cod .ne. 0) goto 8020
c
c     Indicate that the model has space for a row copy of the matrix.
c
      ml_blk_st_msk = ml_blk_st_msk + ml_blk_st_ml_r_mtx
c
c     Indicate that the row copy of the matrix is not correct.
c
      ml_da_st_msk =
     &     ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_r_mtx)
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
 9800 format('Model already has space for a row copy of the matrix')
 9802 format('Error in ems_g_ml_p')
      end
 
C->>> -------------------------------------> ems_g_blk_ml_r_mtx_n_wo <<<
      subroutine ems_g_blk_ml_r_mtx_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      integer r_cf, c_cf, a_el_cf, cs, n_wo
      r_cf = i_wo_z
      c_cf = 0
      a_el_cf = rl_wo_z + i_wo_z
      cs = rl_wo_z + 3*i_wo_z
      n_wo = r_cf*n_r + c_cf*n_c + a_el_cf*n_a_el + cs
      return
      end
 
C->>> ---------------------------------------> ems_iz_blk_ml_aux_sol <<<
c     Sets up block and handles for the auxiliary solve region.
c     Only called in the event of infeasibility or unboundedness.
c
      subroutine ems_iz_blk_ml_aux_sol(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer blk_n, r_cf, c_cf, a_el_cf, cs, n_wo, p_ml_hdl
      integer mem_mgr_rt_cod
      integer rt_cod
 
      if (iand(ml_blk_st_msk, ml_blk_st_ml_aux_sol) .ne. 0) goto 8000
      call ems_g_blk_ml_aux_sol_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      call ems_mem_mgr_ope_blk(mem_mgr_rt_cod, is,
     &     n_wo, ope_blk_anywhere,
     &     cu_ml_n, ml_aux_sol_blk_id, blk_n)
      if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
         if (mem_mgr_rt_cod .eq. mem_mgr_rt_cod_serious_no_po) then
            ems_msg_cod = ems_msg_lvl_serious
            go to 7000
         else if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
             ems_msg_cod = ems_msg_lvl_serious
            go to 7000
         endif
      endif
       p_ml_hdl = p_ml_bs_blk + (cu_ml_n-1)*ml_bs_blk_n_wo +
     &     ml_bs_blk_os_hdl
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &      blk_n, mx_n_c+1, rl_wo_z,
     &      is(p_ml_hdl+ix_hdl_c_aux_sol))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, mx_n_r+1, rl_wo_z,
     &     is(p_ml_hdl+ix_hdl_r_aux_sol))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_g_ml_p(rt_cod, is)
      if (rt_cod .ne. 0) goto 8020
c
c     Indicate that the model now has an auxiliary solve region.
c
      ml_blk_st_msk = ml_blk_st_msk + ml_blk_st_ml_aux_sol
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
 9800 format('Model already has an auxiliary solve region')
 9802 format('Error in ems_g_ml_p')
      end
 
C->>> -----------------------------------> ems_g_blk_ml_aux_sol_n_wo <<<
      subroutine ems_g_blk_ml_aux_sol_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      integer r_cf, c_cf, a_el_cf, cs, n_wo
      r_cf = rl_wo_z
      c_cf = rl_wo_z
      a_el_cf = 0
      cs =   2*rl_wo_z
      n_wo = r_cf*mx_n_r + c_cf*mx_n_c + cs
c      n_wo = rl_wo_z*(mx_n_r+mx_n_c+2)
      return
      end
 
C->>> ---------------------------------------> ems_iz_blk_ml_aux_blk <<<
c     Sets up block and handles for the auxiliary block.
c
      subroutine ems_iz_blk_ml_aux_blk(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer blk_n, r_cf, c_cf, a_el_cf, cs, n_wo, p_ml_hdl
      integer mem_mgr_rt_cod
      integer rt_cod
c
c     ?? This should go when linked aux blocks are introduced.
c
      if (iand(ml_blk_st_msk, ml_blk_st_ml_aux_blk) .ne. 0) goto 8000
      call ems_g_blk_ml_aux_blk_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      call ems_mem_mgr_ope_blk(mem_mgr_rt_cod, is,
     &     n_wo, ope_blk_anywhere,
     &     cu_ml_n, ml_aux_blk_blk_id, blk_n)
      if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
         if (mem_mgr_rt_cod .eq. mem_mgr_rt_cod_serious_no_po) then
            ems_msg_cod = ems_msg_lvl_serious
            go to 7000
         else if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
             ems_msg_cod = ems_msg_lvl_serious
            go to 7000
         endif
      endif
      p_ml_hdl = p_ml_bs_blk + (cu_ml_n-1)*ml_bs_blk_n_wo +
     &     ml_bs_blk_os_hdl
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, mx_n_el_in_aux_blk+1, rl_wo_z,
     &     is(p_ml_hdl+ix_hdl_aux_blk_v))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, mx_n_el_in_aux_blk+1, i_wo_z,
     &     is(p_ml_hdl+ix_hdl_aux_blk_r_ix))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, mx_n_el_in_aux_blk+1, i_wo_z,
     &     is(p_ml_hdl+ix_hdl_aux_blk_c_ix))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_g_ml_p(rt_cod, is)
      if (rt_cod .ne. 0) goto 8020
      is(p_aux_blk_r_ix) = mx_n_el_in_aux_blk
      n_aux_blk = n_aux_blk + 1
      n_el_in_aux_blk = 0
c
c     Indicate that the model now has (at least one) auxiliary block.
c
      ml_blk_st_msk = ior(ml_blk_st_msk,  ml_blk_st_ml_aux_blk)
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
 9800 format('Model already has space for an aux block')
 9802 format('Error in ems_g_ml_p')
      end
 
C->>> -----------------------------------> ems_g_blk_ml_aux_blk_n_wo <<<
      subroutine ems_g_blk_ml_aux_blk_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      integer r_cf, c_cf, a_el_cf, cs, n_wo
      r_cf =    0
      c_cf =    0
      a_el_cf = 0
      cs = rl_wo_z*(1+mx_n_el_in_aux_blk) +
     &     i_wo_z*2*(1+mx_n_el_in_aux_blk)
      n_wo = r_cf*mx_n_r + c_cf*mx_n_c + cs
c      n_wo = rl_wo_z*(1+mx_n_el_in_aux_blk) +
c     &        i_wo_z*2*(1+mx_n_el_in_aux_blk)
      return
      end
 
C->>> -------------------------------------> ems_iz_blk_ml_usr_c_mtx <<<
c     Sets up block and handles for a column-wise copy of the constraint
c     matrix for the user.
c
      subroutine ems_iz_blk_ml_usr_c_mtx(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'RSMICS.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer blk_n, r_cf, c_cf, a_el_cf, cs, n_wo, p_ml_hdl
      integer mem_mgr_rt_cod
      integer rt_cod
 
      if (iand(ml_blk_st_msk,  ml_blk_st_ml_usr_c_mtx) .ne. 0) goto 8000
      call ems_g_blk_ml_usr_c_mtx_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      call ems_mem_mgr_ope_blk(mem_mgr_rt_cod, is,
     &     n_wo, ope_blk_at_f_fr_p,
     &     cu_ml_n, ml_usr_c_mtx_blk_id, blk_n)
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
     &     cs, n_wo, ml_usr_c_mtx_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         go to 7000
      endif
c
c     Get handles for the arrays within the block.
c
      p_ml_hdl = p_ml_bs_blk + (cu_ml_n-1)*ml_bs_blk_n_wo +
     &     ml_bs_blk_os_hdl
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+n_a_el, rl_wo_z,
     &     is(p_ml_hdl+ix_hdl_usr_mtx_r_v))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+n_a_el, i_wo_z,
     &     is(p_ml_hdl+ix_hdl_usr_mtx_r_ix))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+n_c+1, i_wo_z,
     &     is(p_ml_hdl+ix_hdl_usr_mtx_c_sa))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_g_ml_p(rt_cod, is)
      if (rt_cod .ne. 0) goto 8020
c
c     Indicate that the model has space for a row copy of the matrix.
c
      ml_blk_st_msk = ml_blk_st_msk + ml_blk_st_ml_usr_c_mtx
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
 9800 format('Model already has space for a col copy of the matrix for',
     &    ' the user')
 9802 format('Error in ems_g_ml_p')
      end
 
C->>> ---------------------------------> ems_g_blk_ml_usr_c_mtx_n_wo <<<
      subroutine ems_g_blk_ml_usr_c_mtx_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      integer r_cf, c_cf, a_el_cf, cs, n_wo
      r_cf = 0
      c_cf = i_wo_z
      a_el_cf = rl_wo_z + i_wo_z
      cs = rl_wo_z + 3*i_wo_z
      n_wo = r_cf*n_r + c_cf*n_c + a_el_cf*n_a_el + cs
      return
      end
 
C->>> -------------------------------------> ems_iz_blk_ml_usr_r_mtx <<<
c     Sets up block and handles for a row-wise copy of the constraint
c     matrix for the user.
c
      subroutine ems_iz_blk_ml_usr_r_mtx(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'RSMICS.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer blk_n, r_cf, c_cf, a_el_cf, cs, n_wo, p_ml_hdl
      integer mem_mgr_rt_cod
      integer rt_cod
 
      if (iand(ml_blk_st_msk,  ml_blk_st_ml_usr_r_mtx) .ne. 0) goto 8000
      call ems_g_blk_ml_usr_r_mtx_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      call ems_mem_mgr_ope_blk(mem_mgr_rt_cod, is,
     &     n_wo, ope_blk_at_f_fr_p,
     &     cu_ml_n, ml_usr_r_mtx_blk_id, blk_n)
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
     &     cs, n_wo, ml_usr_r_mtx_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         go to 7000
      endif
c
c     Get handles for the arrays within the block.
c
      p_ml_hdl = p_ml_bs_blk + (cu_ml_n-1)*ml_bs_blk_n_wo +
     &     ml_bs_blk_os_hdl
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+n_a_el, rl_wo_z,
     &     is(p_ml_hdl+ix_hdl_usr_mtx_c_v))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+n_a_el, i_wo_z,
     &     is(p_ml_hdl+ix_hdl_usr_mtx_c_ix))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+n_r+1, i_wo_z,
     &     is(p_ml_hdl+ix_hdl_usr_mtx_r_sa))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_g_ml_p(rt_cod, is)
      if (rt_cod .ne. 0) goto 8020
c
c     Indicate that the model has space for a row copy of the matrix.
c
      ml_blk_st_msk = ml_blk_st_msk + ml_blk_st_ml_usr_r_mtx
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
 9800 format('Model already has space for a row copy of the matrix for',
     &    ' the user')
 9802 format('Error in ems_g_ml_p')
      end
 
C->>> ---------------------------------> ems_g_blk_ml_usr_r_mtx_n_wo <<<
      subroutine ems_g_blk_ml_usr_r_mtx_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      integer r_cf, c_cf, a_el_cf, cs, n_wo
      r_cf = i_wo_z
      c_cf = 0
      a_el_cf = rl_wo_z + i_wo_z
      cs = rl_wo_z + 3*i_wo_z
      n_wo = r_cf*n_r + c_cf*n_c + a_el_cf*n_a_el + cs
      return
      end
 
C->>> -------------------------------------------> ems_rm_blk_ml_vec <<<
c     Removes the block for linear model vectors
c
      subroutine ems_rm_blk_ml_vec(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer blk_n, p_ml_hdl
      integer mem_mgr_rt_cod
 
      if (iand(ml_blk_st_msk,  ml_blk_st_ml_vec) .eq. 0) goto 8000
      p_ml_hdl = p_ml_bs_blk + (cu_ml_n-1)*ml_bs_blk_n_wo +
     &     ml_bs_blk_os_hdl
      blk_n = is(p_ml_hdl+ix_blk_ml_vec)
      call ems_mem_mgr_rm_blk(mem_mgr_rt_cod, is,
     &     blk_n)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         goto 7100
      endif
      ml_blk_st_msk = ml_blk_st_msk - ml_blk_st_ml_vec
      ml_da_st_msk = ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_ld)
 7000 continue
 7100 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9800 format('Model does not already have space for vectors')
      end
 
C->>> ----------------------------------------> ems_rm_blk_ml_lng_nm <<<
c     Removes the block for the long model names.
c
      subroutine ems_rm_blk_ml_lng_nm(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer blk_n, p_ml_hdl
      integer mem_mgr_rt_cod
 
      if (iand(ml_blk_st_msk,  ml_blk_st_ml_lng_nm) .eq. 0) goto 8000
      p_ml_hdl = p_ml_bs_blk + (cu_ml_n-1)*ml_bs_blk_n_wo +
     &     ml_bs_blk_os_hdl
      blk_n = is(p_ml_hdl+ix_blk_ml_lng_nm)
      call ems_mem_mgr_rm_blk(mem_mgr_rt_cod, is,
     &     blk_n)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         goto 7100
      endif
      ml_blk_st_msk = ml_blk_st_msk - ml_blk_st_ml_lng_nm
      ml_da_st_msk = ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_ld)
 7000 continue
 7100 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9800 format('Model does not already have space for long names')
      end
 
C->>> -------------------------------------------> ems_rm_blk_ml_sol <<<
c     Removes the block for the model solution.
c
      subroutine ems_rm_blk_ml_sol(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer blk_n, p_ml_hdl
      integer mem_mgr_rt_cod
 
      if (iand(ml_blk_st_msk,  ml_blk_st_ml_sol) .eq. 0) goto 8000
      p_ml_hdl = p_ml_bs_blk + (cu_ml_n-1)*ml_bs_blk_n_wo +
     &     ml_bs_blk_os_hdl
      blk_n = is(p_ml_hdl+ix_blk_ml_sol)
      call ems_mem_mgr_rm_blk(mem_mgr_rt_cod, is,
     &     blk_n)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         goto 7100
      endif
      ml_blk_st_msk = ml_blk_st_msk - ml_blk_st_ml_sol
      ml_da_st_msk = ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_ld)
 7000 continue
 7100 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9800 format('Model does not already have space for solution')
      end
 
C->>> -----------------------------------------> ems_rm_blk_ml_vr_ls <<<
c     Removes the block for the model variable lists.
c
      subroutine ems_rm_blk_ml_vr_ls(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer blk_n, p_ml_hdl
      integer mem_mgr_rt_cod
 
      if (iand(ml_blk_st_msk,  ml_blk_st_ml_vr_ls) .eq. 0) goto 8000
      p_ml_hdl = p_ml_bs_blk + (cu_ml_n-1)*ml_bs_blk_n_wo +
     &     ml_bs_blk_os_hdl
      blk_n = is(p_ml_hdl+ix_blk_ml_vr_ls)
      call ems_mem_mgr_rm_blk(mem_mgr_rt_cod, is,
     &     blk_n)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         goto 7100
      endif
      ml_blk_st_msk = ml_blk_st_msk - ml_blk_st_ml_vr_ls
      ml_da_st_msk = ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_ld)
 7000 continue
 7100 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9800 format('Model does not already have space for variable lists')
      end
 
C->>> -----------------------------------------> ems_rm_blk_ml_c_mtx <<<
c     Removes the block for the linear model matrix.
c
      subroutine ems_rm_blk_ml_c_mtx(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer blk_n, p_ml_hdl
      integer mem_mgr_rt_cod
 
      if (iand(ml_blk_st_msk,  ml_blk_st_ml_c_mtx) .eq. 0) goto 8000
      p_ml_hdl = p_ml_bs_blk + (cu_ml_n-1)*ml_bs_blk_n_wo +
     &     ml_bs_blk_os_hdl
      blk_n = is(p_ml_hdl+ix_blk_ml_c_mtx)
      call ems_mem_mgr_rm_blk(mem_mgr_rt_cod, is,
     &     blk_n)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         goto 7100
      endif
      ml_blk_st_msk = ml_blk_st_msk - ml_blk_st_ml_c_mtx
      ml_da_st_msk = ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_ld)
 7000 continue
 7100 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9800 format('Model does not already have space for a matrix')
      end
 
C->>> -----------------------------------------> ems_rm_blk_ml_r_mtx <<<
c     Removes the block for the row copy of the constraint matrix.
c
      subroutine ems_rm_blk_ml_r_mtx(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer blk_n, p_ml_hdl
      integer mem_mgr_rt_cod
 
      if (iand(ml_blk_st_msk,  ml_blk_st_ml_r_mtx) .eq. 0) goto 8000
      p_ml_hdl = p_ml_bs_blk + (cu_ml_n-1)*ml_bs_blk_n_wo +
     &     ml_bs_blk_os_hdl
      blk_n = is(p_ml_hdl+ix_blk_ml_r_mtx)
      call ems_mem_mgr_rm_blk(mem_mgr_rt_cod, is,
     &     blk_n)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         goto 7100
      endif
      ml_blk_st_msk = ml_blk_st_msk - ml_blk_st_ml_r_mtx
 7000 continue
 7100 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9800 format('Model does not already have space for a row copy ',
     &     'of the matrix')
      end
 
C->>> ---------------------------------------> ems_rm_blk_ml_aux_sol <<<
c     Removes the block for the auxiliary solve region.
c
      subroutine ems_rm_blk_ml_aux_sol(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer blk_n, p_ml_hdl
      integer mem_mgr_rt_cod
 
      if (iand(ml_blk_st_msk,  ml_blk_st_ml_aux_sol) .eq. 0) goto 8000
      p_ml_hdl = p_ml_bs_blk + (cu_ml_n-1)*ml_bs_blk_n_wo +
     &     ml_bs_blk_os_hdl
      blk_n = is(p_ml_hdl+ix_blk_ml_aux_sol)
      call ems_mem_mgr_rm_blk(mem_mgr_rt_cod, is,
     &     blk_n)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         goto 7100
      endif
      ml_blk_st_msk = ml_blk_st_msk - ml_blk_st_ml_aux_sol
 7000 continue
 7100 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9800 format('Model does not already have an auxiliary solve region')
      end
 
C->>> ---------------------------------------> ems_rm_blk_ml_aux_blk <<<
c     Removes the block for the auxiliary elements.
c
      subroutine ems_rm_blk_ml_aux_blk(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer blk_n, p_ml_hdl
      integer mem_mgr_rt_cod
 
      if (iand(ml_blk_st_msk, ml_blk_st_ml_aux_blk) .eq. 0) goto 8000
 
c     ?? This will change when linked aux blocks are introduced.
 
      p_ml_hdl = p_ml_bs_blk + (cu_ml_n-1)*ml_bs_blk_n_wo +
     &     ml_bs_blk_os_hdl
      blk_n = is(p_ml_hdl+ix_blk_ml_aux_blk)
      call ems_mem_mgr_rm_blk(mem_mgr_rt_cod, is,
     &     blk_n)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         goto 7100
      endif
      n_el_in_aux_blk = 0
      n_aux_blk = 0
      ml_blk_st_msk = ml_blk_st_msk - ml_blk_st_ml_aux_blk
 7000 continue
 7100 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9800 format('Model does not already have space for an aux block')
      end
 
C->>> -------------------------------------> ems_rm_blk_ml_usr_c_mtx <<<
c     Removes the block for the row copy of the constraint matrix.
c
      subroutine ems_rm_blk_ml_usr_c_mtx(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer blk_n, p_ml_hdl
      integer mem_mgr_rt_cod
 
      if (iand(ml_blk_st_msk,  ml_blk_st_ml_usr_c_mtx) .eq. 0) goto 8000
      p_ml_hdl = p_ml_bs_blk + (cu_ml_n-1)*ml_bs_blk_n_wo +
     &     ml_bs_blk_os_hdl
      blk_n = is(p_ml_hdl+ix_blk_ml_usr_c_mtx)
      call ems_mem_mgr_rm_blk(mem_mgr_rt_cod, is,
     &     blk_n)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         goto 7100
      endif
      ml_blk_st_msk = ml_blk_st_msk - ml_blk_st_ml_usr_c_mtx
 7000 continue
 7100 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9800 format('Model does not already have space for a col copy ',
     &     'of the matrix for the user.')
      end
 
C->>> -------------------------------------> ems_rm_blk_ml_usr_r_mtx <<<
c     Removes the block for the row copy of the constraint matrix.
c
      subroutine ems_rm_blk_ml_usr_r_mtx(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer blk_n, p_ml_hdl
      integer mem_mgr_rt_cod
 
      if (iand(ml_blk_st_msk,  ml_blk_st_ml_usr_r_mtx) .eq. 0) goto 8000
      p_ml_hdl = p_ml_bs_blk + (cu_ml_n-1)*ml_bs_blk_n_wo +
     &     ml_bs_blk_os_hdl
      blk_n = is(p_ml_hdl+ix_blk_ml_usr_r_mtx)
      call ems_mem_mgr_rm_blk(mem_mgr_rt_cod, is,
     &     blk_n)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         goto 7100
      endif
      ml_blk_st_msk = ml_blk_st_msk - ml_blk_st_ml_usr_r_mtx
 7000 continue
 7100 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9800 format('Model does not already have space for a row copy ',
     &     'of the matrix for the user.')
      end
 
C->>> --------------------------------------------> ems_se_ml_p_no_p <<<
c     Set the model pointers to be `no pointers'.
c     Returns:
c     rt_cod = 1 if mem_mgr_rt_cod is serious
c     rt_cod = 0 Otherwise
c
      subroutine ems_se_ml_p_no_p(rt_cod, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      integer rt_cod, is(0:is_n_en_m1)
      integer ml_n, p_ml_hdl, p_n
      integer mem_mgr_rt_cod
 
      rt_cod = 0
      do 10, ml_n = 1, is(ix_n_ml)
         p_ml_hdl = p_ml_bs_blk +
     &        (ml_n-1)*ml_bs_blk_n_wo + ml_bs_blk_os_hdl
         call ems_mem_mgr_se_hdl_p_no_p(mem_mgr_rt_cod, is,
     &        n_ml_hdl, is(p_ml_hdl))
         if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8900
 10   continue
      do 20, p_n = 1, n_emsol_p
         emsol_p(p_n) = no_p
 20   continue
 7100 continue
      return
 8900 continue
      rt_cod = 1
      goto 7100
      end
 
C->>> --------------------------------------------------> ems_g_ml_p <<<
c     Gets the current model pointers
c     Returns:
c     rt_cod = 1 if mem_mgr_rt_cod is serious
c     rt_cod = 0 Otherwise
 
      subroutine ems_g_ml_p(rt_cod, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      integer rt_cod, is(0:is_n_en_m1)
      integer p_ml_hdl
      integer mem_mgr_rt_cod
 
      rt_cod = 0
      p_ml_hdl = p_ml_bs_blk +
     &     (cu_ml_n-1)*ml_bs_blk_n_wo + ml_bs_blk_os_hdl
      call ems_mem_mgr_g_hdl_p(mem_mgr_rt_cod, is,
     &     n_ml_hdl, is(p_ml_hdl))
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8900
      call ems_cp_ml_p(is)
      ml_blk_mv_k = is(ix_blk_mv_k)
 7100 continue
      return
 8900 continue
      rt_cod = 1
      goto 7100
      end
 
C->>> -------------------------------------------------> ems_cp_ml_p <<<
c     Copy the model pointers from the handles
c
      subroutine ems_cp_ml_p(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      integer is(0:is_n_en_m1)
      integer p_ml_hdl
 
      p_ml_hdl = p_ml_bs_blk +
     &     (cu_ml_n-1)*ml_bs_blk_n_wo + ml_bs_blk_os_hdl
 
      p_lbc = is(p_ml_hdl+ix_hdl_lbc+hdl_os_p)
      p_cbp = is(p_ml_hdl+ix_hdl_cbp+hdl_os_p)
      p_ubc = is(p_ml_hdl+ix_hdl_ubc+hdl_os_p)
      p_nm =  is(p_ml_hdl+ix_hdl_nm+hdl_os_p)
 
      p_lng_nm =  is(p_ml_hdl+ix_hdl_lng_nm+hdl_os_p)
 
      p_pk_bs_os =  is(p_ml_hdl+ix_hdl_pk_bs_os+hdl_os_p)
 
      p_pwl_vr_rf_pr_act_v =
     &     is(p_ml_hdl+ix_hdl_pwl_vr_rf_pr_act_v+hdl_os_p)
      p_pwl_vr_rf_ob_fn_v =
     &     is(p_ml_hdl+ix_hdl_pwl_vr_rf_ob_fn_v+hdl_os_p)
      p_pwl_vr_da_v =  is(p_ml_hdl+ix_hdl_pwl_vr_da_v+hdl_os_p)
      p_pwl_vr_ls =    is(p_ml_hdl+ix_hdl_pwl_vr_ls+hdl_os_p)
      p_pwl_vr_da_sa = is(p_ml_hdl+ix_hdl_pwl_vr_da_sa+hdl_os_p)
      p_pwl_vr_cu_sn = is(p_ml_hdl+ix_hdl_pwl_vr_cu_sn+hdl_os_p)
      p_pwl_vr_ix =    is(p_ml_hdl+ix_hdl_pwl_vr_ix+hdl_os_p)
      p_pwl_vr_usr_bp =    is(p_ml_hdl+ix_hdl_pwl_vr_usr_bp+hdl_os_p)
      p_pwl_vr_usr_sn =    is(p_ml_hdl+ix_hdl_pwl_vr_usr_sn+hdl_os_p)
      p_pwl_vr_usr_ls =    is(p_ml_hdl+ix_hdl_pwl_vr_usr_ls+hdl_os_p)
      p_pwl_vr_usr_da_sa = is(p_ml_hdl+ix_hdl_pwl_vr_usr_da_sa+hdl_os_p)
      p_pwl_vr_usr_cu_sn = is(p_ml_hdl+ix_hdl_pwl_vr_usr_cu_sn+hdl_os_p)
 
      p_scl =    is(p_ml_hdl+ix_hdl_scl+hdl_os_p)
      p_pr_act = is(p_ml_hdl+ix_hdl_pr_act+hdl_os_p)
      p_du_act = is(p_ml_hdl+ix_hdl_du_act+hdl_os_p)
      p_ed_wt = is(p_ml_hdl+ix_hdl_ed_wt+hdl_os_p)
      p_st =     is(p_ml_hdl+ix_hdl_st+hdl_os_p)
 
      p_vr_in_r = is(p_ml_hdl+ix_hdl_vr_in_r+hdl_os_p)
      p_vr_in_c = is(p_ml_hdl+ix_hdl_vr_in_c+hdl_os_p)
 
      p_lo_eta_pv_in_r = is(p_ml_hdl+ix_hdl_lo_eta_pv_in_r+hdl_os_p)
      p_up_eta_pv_in_r = is(p_ml_hdl+ix_hdl_up_eta_pv_in_r+hdl_os_p)
      p_eta_w_l_en_in_r = is(p_ml_hdl+ix_hdl_eta_w_l_en_in_r+hdl_os_p)
      p_eta_w_lm1_en_in_r =
     &     is(p_ml_hdl+ix_hdl_eta_w_lm1_en_in_r+hdl_os_p)
      p_og_t_nw_perm = is(p_ml_hdl+ix_hdl_og_t_nw_perm+hdl_os_p)
      p_nw_t_og_perm = is(p_ml_hdl+ix_hdl_nw_t_og_perm+hdl_os_p)
 
      p_dvx_ix = is(p_ml_hdl+ix_hdl_dvx_ix+hdl_os_p)
 
      p_mtx_r_v = is(p_ml_hdl+ix_hdl_mtx_r_v+hdl_os_p)
      p_mtx_r_ix = is(p_ml_hdl+ix_hdl_mtx_r_ix+hdl_os_p)
      p_mtx_c_sa = is(p_ml_hdl+ix_hdl_mtx_c_sa+hdl_os_p)
      p_mtx_c_ln = is(p_ml_hdl+ix_hdl_mtx_c_ln+hdl_os_p)
 
      p_mtx_c_v = is(p_ml_hdl+ix_hdl_mtx_c_v+hdl_os_p)
      p_mtx_c_ix = is(p_ml_hdl+ix_hdl_mtx_c_ix+hdl_os_p)
      p_mtx_r_sa = is(p_ml_hdl+ix_hdl_mtx_r_sa+hdl_os_p)
 
      p_aux_blk_v = is(p_ml_hdl+ix_hdl_aux_blk_v+hdl_os_p)
      p_aux_blk_r_ix = is(p_ml_hdl+ix_hdl_aux_blk_r_ix+hdl_os_p)
      p_aux_blk_c_ix = is(p_ml_hdl+ix_hdl_aux_blk_c_ix+hdl_os_p)
 
      p_r_aux_sol = is(p_ml_hdl+ix_hdl_r_aux_sol+hdl_os_p)
      p_c_aux_sol = is(p_ml_hdl+ix_hdl_c_aux_sol+hdl_os_p)
 
      p_prsl_sv_ml_lbc = is(p_ml_hdl+ix_hdl_prsl_sv_ml_lbc+hdl_os_p)
      p_prsl_sv_ml_ubc = is(p_ml_hdl+ix_hdl_prsl_sv_ml_ubc+hdl_os_p)
      p_prsl_sv_ml_cbp = is(p_ml_hdl+ix_hdl_prsl_sv_ml_cbp+hdl_os_p)
      p_prsl_sv_mtx_r_v = is(p_ml_hdl+ix_hdl_prsl_sv_mtx_r_v+hdl_os_p)
      p_prsl_sv_mtx_r_ix = is(p_ml_hdl+ix_hdl_prsl_sv_mtx_r_ix+hdl_os_p)
      p_prsl_sv_mtx_c_sa = is(p_ml_hdl+ix_hdl_prsl_sv_mtx_c_sa+hdl_os_p)
      p_prsl_rm_vr_ls = is(p_ml_hdl+ix_hdl_prsl_rm_vr_ls+hdl_os_p)
      p_prsl_rm_vr_st = is(p_ml_hdl+ix_hdl_prsl_rm_vr_st+hdl_os_p)
      p_prsl_rm_vr_pr_act =
     &     is(p_ml_hdl+ix_hdl_prsl_rm_vr_pr_act+hdl_os_p)
 
 
      p_co_rg_up_co_v =  is(p_ml_hdl+ix_hdl_co_rg_up_co_v +hdl_os_p)
      p_co_rg_lo_co_v =  is(p_ml_hdl+ix_hdl_co_rg_lo_co_v +hdl_os_p)
      p_co_rg_up_ob_v =  is(p_ml_hdl+ix_hdl_co_rg_up_ob_v +hdl_os_p)
      p_co_rg_lo_ob_v =  is(p_ml_hdl+ix_hdl_co_rg_lo_ob_v +hdl_os_p)
      p_co_rg_up_act_v = is(p_ml_hdl+ix_hdl_co_rg_up_act_v+hdl_os_p)
      p_co_rg_lo_act_v = is(p_ml_hdl+ix_hdl_co_rg_lo_act_v+hdl_os_p)
      p_co_rg_up_en_vr = is(p_ml_hdl+ix_hdl_co_rg_up_en_vr+hdl_os_p)
      p_co_rg_lo_en_vr = is(p_ml_hdl+ix_hdl_co_rg_lo_en_vr+hdl_os_p)
      p_co_rg_up_lv_vr = is(p_ml_hdl+ix_hdl_co_rg_up_lv_vr+hdl_os_p)
      p_co_rg_lo_lv_vr = is(p_ml_hdl+ix_hdl_co_rg_lo_lv_vr+hdl_os_p)
      p_bd_rg_up_bd_v =  is(p_ml_hdl+ix_hdl_bd_rg_up_bd_v +hdl_os_p)
      p_bd_rg_lo_bd_v =  is(p_ml_hdl+ix_hdl_bd_rg_lo_bd_v +hdl_os_p)
      p_bd_rg_up_ob_v =  is(p_ml_hdl+ix_hdl_bd_rg_up_ob_v +hdl_os_p)
      p_bd_rg_lo_ob_v =  is(p_ml_hdl+ix_hdl_bd_rg_lo_ob_v +hdl_os_p)
      p_bd_rg_up_en_vr = is(p_ml_hdl+ix_hdl_bd_rg_up_en_vr+hdl_os_p)
      p_bd_rg_lo_en_vr = is(p_ml_hdl+ix_hdl_bd_rg_lo_en_vr+hdl_os_p)
      p_bd_rg_up_lv_vr = is(p_ml_hdl+ix_hdl_bd_rg_up_lv_vr+hdl_os_p)
      p_bd_rg_lo_lv_vr = is(p_ml_hdl+ix_hdl_bd_rg_lo_lv_vr+hdl_os_p)
 
      p_u_bs_dse_blk =   is(p_ml_hdl+ix_hdl_u_bs_dse_blk+hdl_os_p)
      p_u_bs_gthr_pv_r = is(p_ml_hdl+ix_hdl_u_bs_gthr_pv_r+hdl_os_p)
      p_u_bs_dse_blk_pv_r_in_c =
     &     is(p_ml_hdl+ix_hdl_u_bs_dse_blk_pv_r_in_c+hdl_os_p)
      p_u_bs_skt_pv_r =  is(p_ml_hdl+ix_hdl_u_bs_skt_pv_r+hdl_os_p)
      p_u_bs_eta_msk =   is(p_ml_hdl+ix_hdl_u_bs_eta_msk+hdl_os_p)
 
      p_usr_mtx_r_v =  is(p_ml_hdl+ix_hdl_usr_mtx_r_v+ hdl_os_p)
      p_usr_mtx_r_ix = is(p_ml_hdl+ix_hdl_usr_mtx_r_ix+hdl_os_p)
      p_usr_mtx_c_sa = is(p_ml_hdl+ix_hdl_usr_mtx_c_sa+hdl_os_p)
      p_usr_mtx_c_v =  is(p_ml_hdl+ix_hdl_usr_mtx_c_v+ hdl_os_p)
      p_usr_mtx_c_ix = is(p_ml_hdl+ix_hdl_usr_mtx_c_ix+hdl_os_p)
      p_usr_mtx_r_sa = is(p_ml_hdl+ix_hdl_usr_mtx_r_sa+hdl_os_p)
 
      return
      end
