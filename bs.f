CM
C->>> -------------------------------------------> ems_dn_ems_blk_id <<<
      subroutine ems_dn_ems_blk_id(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSMSG.INC'
      integer is(0:*)
      integer mem_mgr_rt_cod
 
      call ems_mem_mgr_dn_blk_id(mem_mgr_rt_cod, is,
     &     'Model data vectors', ml_vec_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
      call ems_mem_mgr_dn_blk_id(mem_mgr_rt_cod, is,
     &     'Model long names', ml_lng_nm_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
      call ems_mem_mgr_dn_blk_id(mem_mgr_rt_cod, is,
     &     'Model  packed basis offset', ml_pk_bs_os_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
      call ems_mem_mgr_dn_blk_id(mem_mgr_rt_cod, is,
     &     'Model solution vectors', ml_sol_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
      call ems_mem_mgr_dn_blk_id(mem_mgr_rt_cod, is,
     &     'Model variable lists', ml_vr_ls_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
      call ems_mem_mgr_dn_blk_id(mem_mgr_rt_cod, is,
     &     'Model column-wise matrix ', ml_c_mtx_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
      call ems_mem_mgr_dn_blk_id(mem_mgr_rt_cod, is,
     &     'Model row-wise matrix', ml_r_mtx_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
      call ems_mem_mgr_dn_blk_id(mem_mgr_rt_cod, is,
     &     'Auxiliary solve data', ml_aux_sol_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
      call ems_mem_mgr_dn_blk_id(mem_mgr_rt_cod, is,
     &     'Auxiliary block', ml_aux_blk_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
      call ems_mem_mgr_dn_blk_id(mem_mgr_rt_cod, is,
     &     'Presolve save block', ml_prsl_sv_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
      call ems_mem_mgr_dn_blk_id(mem_mgr_rt_cod, is,
     &     'Presolve work block', ml_prsl_wk_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
      call ems_mem_mgr_dn_blk_id(mem_mgr_rt_cod, is,
     &     'User column-wise matrix', ml_usr_c_mtx_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
      call ems_mem_mgr_dn_blk_id(mem_mgr_rt_cod, is,
     &     'User row-wise matrix', ml_usr_r_mtx_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
      call ems_mem_mgr_dn_blk_id(mem_mgr_rt_cod, is,
     &     'Model basis invert pointers', ml_bs_inv_p_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
      call ems_mem_mgr_dn_blk_id(mem_mgr_rt_cod, is,
     &     'Hash tables', ml_hsh_a_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
      call ems_mem_mgr_dn_blk_id(mem_mgr_rt_cod, is,
     &     'Model PWL variables', ml_pwl_vr_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
      call ems_mem_mgr_dn_blk_id(mem_mgr_rt_cod, is,
     &     'Model ranging data', ml_rg_da_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
      call ems_mem_mgr_dn_blk_id(mem_mgr_rt_cod, is,
     &     'Solver vectors', rsmi_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
      call ems_mem_mgr_dn_blk_id(mem_mgr_rt_cod, is,
     &     'Solver workspace', rsmi_wk_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
      call ems_mem_mgr_dn_blk_id(mem_mgr_rt_cod, is,
     &     'Solver bound vectors', rsmi_bd_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
      call ems_mem_mgr_dn_blk_id(mem_mgr_rt_cod, is,
     &     'Eta group data structure', eta_grp_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
      call ems_mem_mgr_dn_blk_id(mem_mgr_rt_cod, is,
     &     'Crash procedure', crsh_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
      call ems_mem_mgr_dn_blk_id(mem_mgr_rt_cod, is,
     &     'Devex pricing', dvx_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
      call ems_mem_mgr_dn_blk_id(mem_mgr_rt_cod, is,
     &     'l1-CHUZR vectors', l1_cz_r_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
      call ems_mem_mgr_dn_blk_id(mem_mgr_rt_cod, is,
     &     'Row-wise eta file pointers', r_eta_fi_p_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
      call ems_mem_mgr_dn_blk_id(mem_mgr_rt_cod, is,
     &     'Row-wise eta file entries', r_eta_fi_en_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
      call ems_mem_mgr_dn_blk_id(mem_mgr_rt_cod, is,
     &     'MPS-read data structures', mps_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
      call ems_mem_mgr_dn_blk_id(mem_mgr_rt_cod, is,
     &     'PARSMI work space', parsmi_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
      call ems_mem_mgr_dn_blk_id(mem_mgr_rt_cod, is,
     &     'PARSMI INVERT work', parsmi_inv_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
      call ems_mem_mgr_dn_blk_id(mem_mgr_rt_cod, is,
     &     'UPDATE dense data structures', u_bs_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
      call ems_mem_mgr_dn_blk_id(mem_mgr_rt_cod, is,
     &     'INVERT work vectors', tom_inv_wk_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
 7100 continue
      return
 8800 continue
      ems_msg_cod = ems_msg_lvl_serious
      goto 7100
      end
 
C->>> ---------------------------------------------> ems_ca_iz_lg_bs <<<
      subroutine ems_ca_iz_lg_bs(ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSPM.INC'
      include 'EMSP.INC'
      include 'EMSMSG.INC'
      include 'ICTVR.INC'
      integer is(0:is_n_en_m1)
      double precision ds(0:ds_n_en_m1)
 
      call ems_iz_lg_bs_vr_st(
     &     ds(p_lbc), ds(p_cbp), ds(p_ubc),
     &     is(p_st), ds(p_pr_act), ds(p_du_act), ds(p_scl),
     &     is(p_vr_in_r), is(p_vr_in_c))
      return
      end
 
C->>> ----------------------------------------------> ems_ca_se_rsmi <<<
      subroutine ems_ca_se_rsmi(ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      integer is(0:is_n_en_m1)
      double precision ds(0:ds_n_en_m1)
      call ems_se_rsmi(
     &     ds(p_cbp),
     &     ds(p_lbc),
     &     ds(p_ubc),
     &     ds(p_scl),
     &     is(p_st),
     &     ds(p_pr_act),
     &     is(p_vr_in_r),
     &     ds(p_rsmi_co),
     &     ds(p_rsmi_lb),
     &     ds(p_rsmi_ub))
      return
      end
 
