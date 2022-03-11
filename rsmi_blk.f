C->>> ------------------------------------------> ems_iz_al_rsmi_blk <<<
c     (Re-)allocate blocks for RSMI data structures
c
      subroutine ems_iz_al_rsmi_blk(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMIHDL.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer sv_rsmi_blk_st_msk
c
c     Save rsmi_blk_st_msk so that the optional blocks (L1-CHUZR and
c     Row eta file pointers) are only re-allocated if necessary.
c
      sv_rsmi_blk_st_msk = rsmi_blk_st_msk
c
c     Remove any blocks currently allocated for the solver.
c
      if (iand(rsmi_blk_st_msk, rsmi_blk_st_rsmi) .ne. 0)
     &     call ems_rm_blk_rsmi(is)
      if (iand(rsmi_blk_st_msk, rsmi_blk_st_rsmi_wk) .ne. 0)
     &     call ems_rm_blk_rsmi_wk(is)
      if (iand(rsmi_blk_st_msk, rsmi_blk_st_rsmi_bd) .ne. 0)
     &     call ems_rm_blk_rsmi_bd(is)
      if (iand(rsmi_blk_st_msk, rsmi_blk_st_l1_cz_r) .ne. 0)
     &     call ems_rm_blk_l1_cz_r(is)
      if (iand(rsmi_blk_st_msk, rsmi_blk_st_r_eta_fi_p) .ne. 0)
     &     call ems_rm_blk_r_eta_fi_p(is)
      if (iand(rsmi_blk_st_msk, rsmi_blk_st_r_eta_fi_en) .ne. 0)
     &     call ems_rm_blk_r_eta_fi_en(is)
c
c     Indicate that the data in the solver arrays are not correct.
c
      rsmi_blk_ml_n = 0
c
c     Update the maximum dimensions for solver arrays.
c
      rsmi_blk_mx_n_r = mx_n_r
      rsmi_blk_mx_n_c = mx_n_c
c
c     Re-allocate the blocks---except for the row-wise eta file entries
c     which will be allocated after INVERT.
c
      call ems_iz_blk_rsmi(is)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_iz_blk_rsmi_wk(is)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_iz_blk_rsmi_bd(is)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      if (iand(sv_rsmi_blk_st_msk, rsmi_blk_st_l1_cz_r) .ne. 0) then
         call ems_iz_blk_l1_cz_r(is)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      endif
      if (iand(rsmi_blk_st_msk, rsmi_blk_st_r_eta_fi_p) .eq. 0) then
         call ems_iz_blk_r_eta_fi_p(is)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      endif
 7000 continue
      return
      end
 
C->>> --------------------------------------> ems_rp_rsmi_blk_st_msk <<<
c     Report the bits set in rsmi_blk_st_msk
c
      subroutine ems_rp_rsmi_blk_st_msk
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      include 'RSMIHDL.INC'
 
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
      call ems_msg_wr_li(info_msg_n)
      if (iand(rsmi_blk_st_msk, rsmi_blk_st_rsmi) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Block for RSMI'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No block for RSMI'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(rsmi_blk_st_msk, rsmi_blk_st_rsmi_bd) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Block for RSMI workspace'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No block for RSMI workspace'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(rsmi_blk_st_msk, rsmi_blk_st_rsmi_bd) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Block for RSMI bounds'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No block for RSMI bounds'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(rsmi_blk_st_msk, rsmi_blk_st_l1_cz_r) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Block for L1-CHUZR'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No block for L1-CHUZR'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(rsmi_blk_st_msk, rsmi_blk_st_r_eta_fi_p) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Block for row-wise eta file pointers'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No block for row-wise eta file pointers'
      endif
      call ems_msg_wr_li(info_msg_n)
      if (iand(rsmi_blk_st_msk, rsmi_blk_st_r_eta_fi_en) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        '    Block for row-wise eta file entries'
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        ' No block for row-wise eta file entries'
      endif
      call ems_msg_wr_li(info_msg_n)
      return
 9000 format('Reporting rsmi_blk_st_msk')
 9100 format(a)
      end
 
C->>> ---------------------------------------------> ems_iz_blk_rsmi <<<
c     Sets up block and handles for the RSMI vectors.
c
      subroutine ems_iz_blk_rsmi(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'RSMICS.INC'
      include 'RSMIHDL.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer r_cf, c_cf, a_el_cf, cs, n_wo
      integer mem_mgr_rt_cod
      integer rt_cod
 
      if (iand(rsmi_blk_st_msk,  rsmi_blk_st_rsmi) .ne. 0) goto 8000
      call ems_g_blk_rsmi_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      call ems_mem_mgr_ope_blk(mem_mgr_rt_cod, is,
     &     n_wo, ope_blk_at_l_fr_p,
     &     cu_ml_n, rsmi_blk_id, blk_rsmi)
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
     &     r_cf, rsmi_blk_mx_n_r,
     &     c_cf, rsmi_blk_mx_n_c,
     &     0, n_a_el,
     &     cs, n_wo, rsmi_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         go to 7000
      endif
c
c     Get handles for the arrays within the block.
c
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_rsmi, 1+rsmi_blk_mx_n_c, rl_wo_z,
     &     hdl_tbu_r_v)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_rsmi, 1+       rsmi_blk_mx_n_r, rl_wo_z,
     &     hdl_bc_co_v)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_rsmi, 1+       rsmi_blk_mx_n_r, rl_wo_z,
     &     hdl_pi_v)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_rsmi, 1+       rsmi_blk_mx_n_r, rl_wo_z,
     &     hdl_pv_c_v)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      if (iand(inv_alg_msk, inv_alg_perm) .ne. 0) then
         call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &        blk_rsmi, 1+    rsmi_blk_mx_n_r, rl_wo_z,
     &        hdl_perm_tran_vec)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      else
         hdl_perm_tran_vec(hdl_os_blk_n) = 0
      endif
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_rsmi, 1+       rsmi_blk_mx_n_c, i_wo_z,
     &     hdl_tbu_r_ix)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_rsmi, 1+       rsmi_blk_mx_n_r, i_wo_z,
     &     hdl_bc_co_ix)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_rsmi, 1+       rsmi_blk_mx_n_r, i_wo_z,
     &     hdl_bc_co_ix_bar)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_rsmi, 1+       rsmi_blk_mx_n_r, i_wo_z,
     &     hdl_pi_ix)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_rsmi, 1+rsmi_blk_mx_n_r+1, i_wo_z,
     &     hdl_cz_r_cdd_ix)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_rsmi, 1+rsmi_blk_mx_n_c+rsmi_blk_mx_n_r, i_wo_z,
     &     hdl_og_bs_cg_st)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_g_rsmi_p(rt_cod, is)
      if (rt_cod .ne. 0) goto 8020
c
c     Indicate that the model now has space for the solver.
c
      rsmi_blk_st_msk = rsmi_blk_st_msk + rsmi_blk_st_rsmi
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
 9800 format('Model already has space for solver')
 9802 format('Error in ems_g_rsmi_p')
      end
 
C->>> -----------------------------------------> ems_g_blk_rsmi_n_wo <<<
      subroutine ems_g_blk_rsmi_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'RSMIHDL.INC'
      integer r_cf, c_cf, a_el_cf, cs, n_wo
 
      r_cf = 3*rl_wo_z + 4*i_wo_z
      c_cf =   rl_wo_z +   i_wo_z
      a_el_cf = 0
      cs =   4*rl_wo_z + 6*i_wo_z
c      n_wo = rl_wo_z*(
c     &     1+rsmi_blk_mx_n_c +
c     &     1+rsmi_blk_mx_n_r +
c     &     1+rsmi_blk_mx_n_r +
c     &     1+rsmi_blk_mx_n_r) +
c     &     i_wo_z*(
c     &     1+rsmi_blk_mx_n_c +
c     &     1+rsmi_blk_mx_n_r +
c     &     1+rsmi_blk_mx_n_r +
c     &     1+rsmi_blk_mx_n_r+1 +
c     &     1+rsmi_blk_mx_n_r)
      if (iand(inv_alg_msk, inv_alg_perm) .ne. 0) then
         r_cf = r_cf + rl_wo_z
         cs =   cs +   rl_wo_z
c         n_wo = n_wo + rl_wo_z*(1+rsmi_blk_mx_n_r)
      endif
c
c     Account for og_bs_cg_st
c
      cs =   cs +   i_wo_z*(1+rsmi_blk_mx_n_c+rsmi_blk_mx_n_r)
c      n_wo = n_wo + i_wo_z*(1+rsmi_blk_mx_n_c+rsmi_blk_mx_n_r)
 
      n_wo = r_cf*rsmi_blk_mx_n_r + c_cf*rsmi_blk_mx_n_c + cs
      return
      end
 
C->>> ------------------------------------------> ems_iz_blk_rsmi_wk <<<
c     Sets up block and handles for the RSMI workspace.
c
      subroutine ems_iz_blk_rsmi_wk(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMICS.INC'
      include 'RSMIHDL.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer r_cf, c_cf, a_el_cf, cs, n_wo
      integer mem_mgr_rt_cod
      integer rt_cod
 
      if (iand(rsmi_blk_st_msk,
     &     rsmi_blk_st_rsmi_wk) .ne. 0) goto 8000
      call ems_g_blk_rsmi_wk_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      call ems_mem_mgr_ope_blk(mem_mgr_rt_cod, is,
     &     n_wo, ope_blk_at_l_fr_p,
     &     cu_ml_n, rsmi_wk_blk_id, blk_rsmi_wk)
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
     &     r_cf, rsmi_blk_mx_n_r,
     &     c_cf, rsmi_blk_mx_n_c,
     &     0, n_a_el,
     &     cs, n_wo, rsmi_wk_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         go to 7000
      endif
c
c     Get handles for the arrays within the block.
c
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_rsmi_wk, 1+rsmi_blk_mx_n_c+rsmi_blk_mx_n_r, rl_wo_z,
     &     hdl_rsmi_rl_wk_a1)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_rsmi_wk, 1+rsmi_blk_mx_n_c+rsmi_blk_mx_n_r, rl_wo_z,
     &     hdl_rsmi_rl_wk_a2)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_rsmi_wk, 1+rsmi_blk_mx_n_c+rsmi_blk_mx_n_r, rl_wo_z,
     &     hdl_rsmi_rl_wk_a3)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_rsmi_wk, 1+rsmi_blk_mx_n_c+rsmi_blk_mx_n_r, i_wo_z,
     &     hdl_rsmi_i_wk_a1)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_rsmi_wk, 1+rsmi_blk_mx_n_c+rsmi_blk_mx_n_r, i_wo_z,
     &     hdl_rsmi_i_wk_a2)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_g_rsmi_p(rt_cod, is)
      if (rt_cod .ne. 0) goto 8020
c
c     Indicate that the pointers are free
c
      use_rsmi_rl_wk_a1_ok = .true.
      use_rsmi_rl_wk_a2_ok = .true.
      use_rsmi_rl_wk_a3_ok = .true.
      use_rsmi_i_wk_a1_ok = .true.
      use_rsmi_i_wk_a2_ok = .true.
c
c     Indicate that the model now has space for RSMI workspace
c
      rsmi_blk_st_msk = rsmi_blk_st_msk + rsmi_blk_st_rsmi_wk
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
 9800 format('Model already has space for RSMI workspace')
 9802 format('Error in ems_g_rsmi_p')
      end
 
C->>> --------------------------------------> ems_g_blk_rsmi_wk_n_wo <<<
      subroutine ems_g_blk_rsmi_wk_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'RSMIHDL.INC'
      integer r_cf, c_cf, a_el_cf, cs, n_wo
 
      r_cf = 3*rl_wo_z + 2*i_wo_z
      c_cf = 3*rl_wo_z + 2*i_wo_z
      a_el_cf = 0
      cs =   3*rl_wo_z + 2*i_wo_z
c      n_wo = rl_wo_z*(
c     &     1+rsmi_blk_mx_n_c+rsmi_blk_mx_n_r +
c     &     1+rsmi_blk_mx_n_c+rsmi_blk_mx_n_r +
c     &     1+rsmi_blk_mx_n_c+rsmi_blk_mx_n_r) +
c     &     i_wo_z*(
c     &     1+rsmi_blk_mx_n_c+rsmi_blk_mx_n_r +
c     &     1+rsmi_blk_mx_n_c+rsmi_blk_mx_n_r)
      n_wo = r_cf*rsmi_blk_mx_n_r + c_cf*rsmi_blk_mx_n_c + cs
      return
      end
 
      subroutine ems_g_rsmi_rl_wk_a_ix(ix)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      integer ix
      if (use_rsmi_rl_wk_a1_ok) then
         ix = 1
         use_rsmi_rl_wk_a1_ok = .false.
      else if (use_rsmi_rl_wk_a2_ok) then
         ix = 2
         use_rsmi_rl_wk_a2_ok = .false.
      else if (use_rsmi_rl_wk_a3_ok) then
         ix = 3
         use_rsmi_rl_wk_a3_ok = .false.
      else
         ix = -1
      endif
      return
      end
 
      subroutine ems_fr_rsmi_rl_wk_a_ix(ix)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      integer ix
      if (ix .eq. 1) then
         use_rsmi_rl_wk_a1_ok = .true.
      else if (ix .eq. 2) then
         use_rsmi_rl_wk_a2_ok = .true.
      else if (ix .eq. 3) then
         use_rsmi_rl_wk_a3_ok = .true.
      endif
      return
      end
 
      subroutine ems_g_rsmi_i_wk_a_ix(ix)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      integer ix
      if (use_rsmi_i_wk_a1_ok) then
         ix = 1
         use_rsmi_i_wk_a1_ok = .false.
      else if (use_rsmi_i_wk_a2_ok) then
         ix = 2
         use_rsmi_i_wk_a2_ok = .false.
      else
         ix = -1
      endif
      return
      end
 
      subroutine ems_fr_rsmi_i_wk_a_ix(ix)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      integer ix
      if (ix .eq. 1) then
         use_rsmi_i_wk_a1_ok = .true.
      else if (ix .eq. 2) then
         use_rsmi_i_wk_a2_ok = .true.
      endif
      return
      end
 
C->>> ------------------------------------------> ems_iz_blk_rsmi_bd <<<
c     Sets up block and handles for the RSMI bound vectors.
c
      subroutine ems_iz_blk_rsmi_bd(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'RSMICS.INC'
      include 'RSMIHDL.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer r_cf, c_cf, a_el_cf, cs, n_wo
      integer mem_mgr_rt_cod
      integer rt_cod
 
      if (iand(rsmi_blk_st_msk,
     &     rsmi_blk_st_rsmi_bd) .ne. 0) goto 8000
      call ems_g_blk_rsmi_bd_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      call ems_mem_mgr_ope_blk(mem_mgr_rt_cod, is,
     &     n_wo, ope_blk_at_l_fr_p,
     &     cu_ml_n, rsmi_bd_blk_id, blk_rsmi_bd)
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
     &     r_cf, rsmi_blk_mx_n_r,
     &     c_cf, rsmi_blk_mx_n_c,
     &     0, n_a_el,
     &     cs, n_wo, rsmi_bd_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         go to 7000
      endif
c
c     Get handles for the arrays within the block.
c
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_rsmi_bd, 1+rsmi_blk_mx_n_c+rsmi_blk_mx_n_r, rl_wo_z,
     &     hdl_rsmi_lb)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_rsmi_bd, 1+rsmi_blk_mx_n_c+rsmi_blk_mx_n_r, rl_wo_z,
     &     hdl_rsmi_co)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_rsmi_bd, 1+rsmi_blk_mx_n_c+rsmi_blk_mx_n_r, rl_wo_z,
     &     hdl_rsmi_ub)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_g_rsmi_p(rt_cod, is)
      if (rt_cod .ne. 0) goto 8020
c
c     Indicate that the model now has space for the solver bounds.
c
      rsmi_blk_st_msk = rsmi_blk_st_msk + rsmi_blk_st_rsmi_bd
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
 9800 format('Model already has space for solver bounds')
 9802 format('Error in ems_g_rsmi_p')
      end
 
C->>> --------------------------------------> ems_g_blk_rsmi_bd_n_wo <<<
      subroutine ems_g_blk_rsmi_bd_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'RSMIHDL.INC'
      integer r_cf, c_cf, a_el_cf, cs, n_wo
 
      r_cf = 3*rl_wo_z
      c_cf = 3*rl_wo_z
      a_el_cf = 0
      cs = 3*rl_wo_z
c      n_wo = rl_wo_z*(
c     &     1+rsmi_blk_mx_n_c+rsmi_blk_mx_n_r +
c     &     1+rsmi_blk_mx_n_c+rsmi_blk_mx_n_r +
c     &     1+rsmi_blk_mx_n_c+rsmi_blk_mx_n_r)
      n_wo = r_cf*rsmi_blk_mx_n_r + c_cf*rsmi_blk_mx_n_c + cs
      return
      end
 
C->>> ------------------------------------------> ems_iz_blk_l1_cz_r <<<
c     Sets up block and handles for L1_CZ_R.
c
      subroutine ems_iz_blk_l1_cz_r(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'RSMICS.INC'
      include 'RSMIHDL.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer r_cf, c_cf, a_el_cf, cs, n_wo
      integer mem_mgr_rt_cod
      integer rt_cod
      integer l1_cz_r_mx_n_cdd_cf
      data l1_cz_r_mx_n_cdd_cf/2/
 
      if (iand(rsmi_blk_st_msk,
     &     rsmi_blk_st_l1_cz_r) .ne. 0) goto 8000
      l1_cz_r_mx_n_cdd_cf = 2
      l1_cz_r_mx_n_cdd = l1_cz_r_mx_n_cdd_cf*rsmi_blk_mx_n_r
      call ems_g_blk_l1_cz_r_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      call ems_mem_mgr_ope_blk(mem_mgr_rt_cod, is,
     &     n_wo, ope_blk_at_l_fr_p,
     &     cu_ml_n, l1_cz_r_blk_id, blk_l1_cz_r)
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
     &     r_cf, rsmi_blk_mx_n_r,
     &     c_cf, rsmi_blk_mx_n_c,
     &     0, 0,
     &     cs, n_wo, l1_cz_r_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         go to 7000
      endif
c
c     Get handles for the arrays within the block.
c
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_l1_cz_r, 1+l1_cz_r_mx_n_cdd, rl_wo_z,
     &     hdl_l1_cz_r_bp)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_l1_cz_r, 1+l1_cz_r_mx_n_cdd, rl_wo_z,
     &     hdl_l1_cz_r_dl_gd)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_l1_cz_r, 1+l1_cz_r_mx_n_cdd,  i_wo_z,
     &     hdl_l1_cz_r_ix)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_g_rsmi_p(rt_cod, is)
      if (rt_cod .ne. 0) goto 8020
c
c     Indicate that the model now has space for the solver.
c
      rsmi_blk_st_msk = rsmi_blk_st_msk + rsmi_blk_st_l1_cz_r
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
 9800 format('Model already has space for l1-CHUZR')
 9802 format('Error in ems_g_rsmi_p')
      end
 
C->>> --------------------------------------> ems_g_blk_l1_cz_r_n_wo <<<
      subroutine ems_g_blk_l1_cz_r_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'RSMIHDL.INC'
      integer r_cf, c_cf, a_el_cf, cs, n_wo
      integer l1_cz_r_mx_n_cdd_cf
 
      l1_cz_r_mx_n_cdd_cf = 2
      l1_cz_r_mx_n_cdd = l1_cz_r_mx_n_cdd_cf*rsmi_blk_mx_n_r
      r_cf = l1_cz_r_mx_n_cdd_cf*(2*rl_wo_z + i_wo_z)
      c_cf = 0
      a_el_cf = 0
      cs =   2*rl_wo_z + i_wo_z
      n_wo = r_cf*rsmi_blk_mx_n_r + c_cf*rsmi_blk_mx_n_c + cs
c      n_wo =
c     &     2*rl_wo_z*(1+l1_cz_r_mx_n_cdd_cf*rsmi_blk_mx_n_r) +
c     &     i_wo_z*(1+l1_cz_r_mx_n_cdd_cf*rsmi_blk_mx_n_r)
      return
      end
 
C->>> --------------------------------------> ems_iz_blk_r_eta_fi_p <<<
c     Sets up block and handles for the row eta file pointers
c
      subroutine ems_iz_blk_r_eta_fi_p(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
c      include 'RSMICS.INC'
      include 'RSMIHDL.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer r_cf, c_cf, a_el_cf, cs, n_wo
      integer mem_mgr_rt_cod
      integer rt_cod
 
      if (iand(rsmi_blk_st_msk,
     &     rsmi_blk_st_r_eta_fi_p) .ne. 0) goto 8000
      call ems_g_blk_r_eta_fi_p_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      call ems_mem_mgr_ope_blk(mem_mgr_rt_cod, is,
     &     n_wo, ope_blk_at_l_fr_p,
     &     cu_ml_n, r_eta_fi_p_blk_id, blk_r_eta_fi_p)
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
     &     r_cf, rsmi_blk_mx_n_r,
     &     0, 0,
     &     0, 0,
     &     cs, n_wo, r_eta_fi_p_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         go to 7000
      endif
c
c     Get handles for the arrays within the block.
c
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_r_eta_fi_p, 1+rsmi_blk_mx_n_r, i_wo_z,
     &     hdl_lo_eta_r_sa)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_r_eta_fi_p, 1+rsmi_blk_mx_n_r, i_wo_z,
     &     hdl_up_eta_r_sa)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_r_eta_fi_p, 1+rsmi_blk_mx_n_r, i_wo_z,
     &     hdl_lo_eta_pv_in_c)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_r_eta_fi_p, 1+rsmi_blk_mx_n_r, i_wo_z,
     &     hdl_up_eta_pv_in_c)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_g_rsmi_p(rt_cod, is)
      if (rt_cod .ne. 0) goto 8020
c
c     Indicate that the model now has space for the row eta file
c     pointers
c
      rsmi_blk_st_msk = rsmi_blk_st_msk + rsmi_blk_st_r_eta_fi_p
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
 9800 format('Model already has space for row eta file pointers')
 9802 format('Error in ems_g_rsmi_p')
      end
 
C->>> -----------------------------------> ems_g_blk_r_eta_fi_p_n_wo <<<
      subroutine ems_g_blk_r_eta_fi_p_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'RSMIHDL.INC'
      integer r_cf, c_cf, a_el_cf, cs, n_wo
 
      r_cf = 4*i_wo_z
      c_cf = 0
      a_el_cf = 0
      cs =   4*i_wo_z
c      n_wo = i_wo_z*(
c     &     1+rsmi_blk_mx_n_r +
c     &     1+rsmi_blk_mx_n_r +
c     &     1+rsmi_blk_mx_n_r +
c     &     1+rsmi_blk_mx_n_r)
      n_wo = r_cf*rsmi_blk_mx_n_r + cs
      return
      end
 
C->>> --------------------------------------> ems_iz_blk_r_eta_fi_en <<<
c     Sets up block and handles for the row eta file entries
c
      subroutine ems_iz_blk_r_eta_fi_en(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
c      include 'RSMICS.INC'
      include 'RSMIHDL.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer r_cf, c_cf, a_el_cf, cs, n_wo
      integer mem_mgr_rt_cod
      integer rt_cod
 
      if (iand(rsmi_blk_st_msk,
     &     rsmi_blk_st_r_eta_fi_en) .ne. 0) goto 8000
      call ems_g_blk_r_eta_fi_en_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      call ems_mem_mgr_ope_blk(mem_mgr_rt_cod, is,
     &     n_wo, ope_blk_at_l_fr_p,
     &     cu_ml_n, r_eta_fi_en_blk_id, blk_r_eta_fi_en)
      if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
         if (mem_mgr_rt_cod .eq. mem_mgr_rt_cod_serious_no_po) then
            ems_msg_cod = ems_msg_lvl_serious
            go to 7000
         else if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
             ems_msg_cod = ems_msg_lvl_serious
            go to 7000
         endif
      endif
c      call ems_mem_mgr_rp_ope_blk(
c     &     mem_mgr_rt_cod, is, -1,
c     &     0, 0,
c     &     0, 0,
c     &     0, 0,
c     &     cs, n_wo, r_eta_fi_en_blk_id)
c      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
c         ems_msg_cod = ems_msg_lvl_serious
c         go to 7000
c      endif
c
c     Get handles for the arrays within the block.
c
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_r_eta_fi_en, 1+n_lo_r_eta_el, rl_wo_z,
     &     hdl_lo_eta_c_v)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_r_eta_fi_en, 1+n_up_r_eta_el, rl_wo_z,
     &     hdl_up_eta_c_v)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_r_eta_fi_en, 1+n_lo_r_eta_el, i_wo_z,
     &     hdl_lo_eta_c_ix)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_r_eta_fi_en, 1+n_up_r_eta_el, i_wo_z,
     &     hdl_up_eta_c_ix)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_g_rsmi_p(rt_cod, is)
      if (rt_cod .ne. 0) goto 8020
c
c     Indicate that the model now has space for the row eta file entries
c
      rsmi_blk_st_msk = rsmi_blk_st_msk + rsmi_blk_st_r_eta_fi_en
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
 9800 format('Model already has space for row eta file entries')
 9802 format('Error in ems_g_r_eta_fi_en_p')
      end
 
C->>> ----------------------------------> ems_g_blk_r_eta_fi_en_n_wo <<<
      subroutine ems_g_blk_r_eta_fi_en_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'RSMIHDL.INC'
      integer r_cf, c_cf, a_el_cf, cs, n_wo
 
      r_cf = 0
      c_cf = 0
      a_el_cf = 0
      cs =   rl_wo_z*(1+n_lo_r_eta_el + 1+n_up_r_eta_el) +
     &      i_wo_z*(1+n_lo_r_eta_el + 1+n_up_r_eta_el)
c      n_wo = cs
c     &     rl_wo_z*(1+n_lo_r_eta_el + 1+n_up_r_eta_el) +
c     &      i_wo_z*(1+n_lo_r_eta_el + 1+n_up_r_eta_el)
      n_wo = cs
      return
      end
 
C->>> ---------------------------------------------> ems_rm_blk_rsmi <<<
c     Removes the block for the solver
c
      subroutine ems_rm_blk_rsmi(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'RSMIHDL.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer mem_mgr_rt_cod
 
      if (iand(rsmi_blk_st_msk,
     &     rsmi_blk_st_rsmi) .eq. 0) goto 8000
      call ems_mem_mgr_rm_blk(mem_mgr_rt_cod, is,
     &     blk_rsmi)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         goto 7100
      endif
      rsmi_blk_st_msk = rsmi_blk_st_msk - rsmi_blk_st_rsmi
 7000 continue
 7100 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9800 format('Model does not already have space for solver')
      end
 
C->>> ------------------------------------------> ems_rm_blk_rsmi_wk <<<
c     Removes the block for the solver workspace
c
      subroutine ems_rm_blk_rsmi_wk(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'RSMIHDL.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer mem_mgr_rt_cod
 
      if (iand(rsmi_blk_st_msk,
     &     rsmi_blk_st_rsmi_wk) .eq. 0) goto 8000
      call ems_mem_mgr_rm_blk(mem_mgr_rt_cod, is,
     &     blk_rsmi_wk)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         goto 7100
      endif
      rsmi_blk_st_msk = rsmi_blk_st_msk - rsmi_blk_st_rsmi_wk
 7000 continue
 7100 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9800 format('Model does not already have space for solver workspace')
      end
 
C->>> ------------------------------------------> ems_rm_blk_rsmi_bd <<<
c     Removes the block for the solver bounds
c
      subroutine ems_rm_blk_rsmi_bd(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'RSMIHDL.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer mem_mgr_rt_cod
 
      if (iand(rsmi_blk_st_msk,
     &     rsmi_blk_st_rsmi_bd) .eq. 0) goto 8000
      call ems_mem_mgr_rm_blk(mem_mgr_rt_cod, is,
     &     blk_rsmi_bd)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         goto 7100
      endif
      rsmi_blk_st_msk = rsmi_blk_st_msk - rsmi_blk_st_rsmi_bd
 7000 continue
 7100 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9800 format('Model does not already have space for solver bounds')
      end
 
C->>> ------------------------------------------> ems_rm_blk_l1_cz_r <<<
c     Removes the block for l1-CHUZR
c
      subroutine ems_rm_blk_l1_cz_r(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'RSMIHDL.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer mem_mgr_rt_cod
 
      if (iand(rsmi_blk_st_msk,
     &     rsmi_blk_st_l1_cz_r) .eq. 0) goto 8000
      call ems_mem_mgr_rm_blk(mem_mgr_rt_cod, is,
     &     blk_l1_cz_r)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         goto 7100
      endif
      rsmi_blk_st_msk = rsmi_blk_st_msk - rsmi_blk_st_l1_cz_r
 7000 continue
 7100 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9800 format('Model does not already have space for l1-CHUZR')
      end
 
C->>> ---------------------------------------> ems_rm_blk_r_eta_fi_p <<<
c     Removes the block for the row eta file pointers
c
      subroutine ems_rm_blk_r_eta_fi_p(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'RSMIHDL.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer mem_mgr_rt_cod
 
      if (iand(rsmi_blk_st_msk,
     &     rsmi_blk_st_r_eta_fi_p) .eq. 0) goto 8000
      call ems_mem_mgr_rm_blk(mem_mgr_rt_cod, is,
     &     blk_r_eta_fi_p)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         goto 7100
      endif
      rsmi_blk_st_msk = rsmi_blk_st_msk - rsmi_blk_st_r_eta_fi_p
 7000 continue
 7100 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9800 format('Model does not already have space for ',
     &     'the row eta file pointers')
      end
 
C->>> --------------------------------------> ems_rm_blk_r_eta_fi_en <<<
c     Removes the block for the row eta file entries
c
      subroutine ems_rm_blk_r_eta_fi_en(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'RSMIHDL.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer mem_mgr_rt_cod
 
      if (iand(rsmi_blk_st_msk,
     &     rsmi_blk_st_r_eta_fi_en) .eq. 0) goto 8000
      call ems_mem_mgr_rm_blk(mem_mgr_rt_cod, is,
     &     blk_r_eta_fi_en)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         goto 7100
      endif
      rsmi_blk_st_msk = rsmi_blk_st_msk - rsmi_blk_st_r_eta_fi_en
 7000 continue
 7100 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9800 format('Model does not already have space for the ',
     &     'row eta file entries')
      end
C->>> ------------------------------------------> ems_se_rsmi_p_no_p <<<
c     Sets the rsmi pointers to be no pointers
c     Returns:
c     rt_cod = 0 Since nothing can go wrong!
c
      subroutine ems_se_rsmi_p_no_p(rt_cod)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      integer rt_cod
 
      rt_cod = 0
      p_bc_co_v = no_p
      p_bc_co_ix = no_p
      p_bc_co_ix_bar = no_p
      p_pi_v = no_p
      p_pi_ix = no_p
      p_tbu_r_v = no_p
      p_tbu_r_ix = no_p
      p_pv_c_v = no_p
      p_perm_tran_vec = no_p
      p_cz_r_cdd_ix = no_p
      p_og_bs_cg_st = no_p
 
      p_l1_cz_r_bp = no_p
      p_l1_cz_r_dl_gd = no_p
      p_l1_cz_r_ix = no_p
 
      p_rsmi_lb = no_p
      p_rsmi_co = no_p
      p_rsmi_ub = no_p
 
      p_lo_eta_r_sa = no_p
      p_up_eta_r_sa = no_p
      p_lo_eta_pv_in_c = no_p
      p_up_eta_pv_in_c = no_p
 
      p_lo_eta_c_v = no_p
      p_lo_eta_c_ix = no_p
      p_up_eta_c_v = no_p
      p_up_eta_c_ix = no_p
      return
      end
 
C->>> ------------------------------------------------> ems_g_rsmi_p <<<
c     Gets the current rsmi pointers
c     Returns:
c     rt_cod = 1 if mem_mgr_rt_cod is serious
c     rt_cod = 0 Otherwise
c
      subroutine ems_g_rsmi_p(rt_cod, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMIHDL.INC'
      include 'ICTVR.INC'
      integer rt_cod, is(0:is_n_en_m1)
      integer mem_mgr_rt_cod
 
      rt_cod = 0
      call ems_mem_mgr_g_hdl_p(mem_mgr_rt_cod, is,
     &     n_rsmi_hdl, rsmi_hdl)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8900
      p_bc_co_v =       rsmi_hdl(ix_hdl_bc_co_v+hdl_os_p)
      p_bc_co_ix =      rsmi_hdl(ix_hdl_bc_co_ix+hdl_os_p)
      p_bc_co_ix_bar =  rsmi_hdl(ix_hdl_bc_co_ix_bar+hdl_os_p)
      p_pi_v =          rsmi_hdl(ix_hdl_pi_v+hdl_os_p)
      p_pi_ix =         rsmi_hdl(ix_hdl_pi_ix+hdl_os_p)
      p_tbu_r_v =       rsmi_hdl(ix_hdl_tbu_r_v+hdl_os_p)
      p_tbu_r_ix =      rsmi_hdl(ix_hdl_tbu_r_ix+hdl_os_p)
      p_pv_c_v =        rsmi_hdl(ix_hdl_pv_c_v+hdl_os_p)
      p_perm_tran_vec = rsmi_hdl(ix_hdl_perm_tran_vec+hdl_os_p)
      p_cz_r_cdd_ix =   rsmi_hdl(ix_hdl_cz_r_cdd_ix+hdl_os_p)
      p_og_bs_cg_st =   rsmi_hdl(ix_hdl_og_bs_cg_st+hdl_os_p)
 
      call ems_mem_mgr_g_hdl_p(mem_mgr_rt_cod, is,
     &     n_l1_cz_r_hdl, l1_cz_r_hdl)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8900
      p_l1_cz_r_bp =    l1_cz_r_hdl(ix_hdl_l1_cz_r_bp+hdl_os_p)
      p_l1_cz_r_dl_gd = l1_cz_r_hdl(ix_hdl_l1_cz_r_dl_gd+hdl_os_p)
      p_l1_cz_r_ix =    l1_cz_r_hdl(ix_hdl_l1_cz_r_ix+hdl_os_p)
 
      call ems_mem_mgr_g_hdl_p(mem_mgr_rt_cod, is,
     &     n_rsmi_wk_hdl, rsmi_wk_hdl)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8900
      p_rsmi_rl_wk_a1 = rsmi_wk_hdl(ix_hdl_rsmi_rl_wk_a1+hdl_os_p)
      p_rsmi_rl_wk_a2 = rsmi_wk_hdl(ix_hdl_rsmi_rl_wk_a2+hdl_os_p)
      p_rsmi_rl_wk_a3 = rsmi_wk_hdl(ix_hdl_rsmi_rl_wk_a3+hdl_os_p)
      p_rsmi_i_wk_a1 =  rsmi_wk_hdl(ix_hdl_rsmi_i_wk_a1+hdl_os_p)
      p_rsmi_i_wk_a2 =  rsmi_wk_hdl(ix_hdl_rsmi_i_wk_a2+hdl_os_p)
 
      call ems_mem_mgr_g_hdl_p(mem_mgr_rt_cod, is,
     &     n_rsmi_bd_hdl, rsmi_bd_hdl)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8900
      p_rsmi_lb = rsmi_bd_hdl(ix_hdl_rsmi_lb+hdl_os_p)
      p_rsmi_co = rsmi_bd_hdl(ix_hdl_rsmi_co+hdl_os_p)
      p_rsmi_ub = rsmi_bd_hdl(ix_hdl_rsmi_ub+hdl_os_p)
 
      call ems_mem_mgr_g_hdl_p(mem_mgr_rt_cod, is,
     &     n_r_eta_fi_p_hdl, r_eta_fi_p_hdl)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8900
      p_lo_eta_r_sa =    r_eta_fi_p_hdl(ix_hdl_lo_eta_r_sa+hdl_os_p)
      p_up_eta_r_sa =    r_eta_fi_p_hdl(ix_hdl_up_eta_r_sa+hdl_os_p)
      p_lo_eta_pv_in_c = r_eta_fi_p_hdl(ix_hdl_lo_eta_pv_in_c+hdl_os_p)
      p_up_eta_pv_in_c = r_eta_fi_p_hdl(ix_hdl_up_eta_pv_in_c+hdl_os_p)
 
      call ems_mem_mgr_g_hdl_p(mem_mgr_rt_cod, is,
     &     n_r_eta_fi_en_hdl, r_eta_fi_en_hdl)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8900
      p_lo_eta_c_v = r_eta_fi_en_hdl(ix_hdl_lo_eta_c_v+hdl_os_p)
      p_lo_eta_c_ix = r_eta_fi_en_hdl(ix_hdl_lo_eta_c_ix+hdl_os_p)
      p_up_eta_c_v = r_eta_fi_en_hdl(ix_hdl_up_eta_c_v+hdl_os_p)
      p_up_eta_c_ix = r_eta_fi_en_hdl(ix_hdl_up_eta_c_ix+hdl_os_p)
 7100 continue
      return
 8900 continue
      rt_cod = 1
      goto 7100
      end
 
