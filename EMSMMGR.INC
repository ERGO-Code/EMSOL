CM
c
c=======================================================================
c
c     Value of a `no pointer'---any negative value will do, but this is
c     -(integer infinity) for EMSOL
c
      integer no_p
c      parameter (no_p = -88288287)
      parameter (no_p = -2147483647)
c
c=======================================================================
c
c     Parameter for the entry in is(0:*) which contains the memory
c     manager block move counter.
c
      integer ix_blk_mv_k
      parameter (ix_blk_mv_k = 0)
c
c=======================================================================
c
c     Parameters for memory manager handle data structure
c
      integer hdl_os_blk_n, hdl_os_blk_os
      integer hdl_os_wo_z, hdl_os_p, hdl_os_blk_mv_k
      integer hdl_z, hdl_z_m1
      parameter (
     &     hdl_os_blk_n =  0,
     &     hdl_os_blk_os = hdl_os_blk_n + 1,
     &     hdl_os_wo_z = hdl_os_blk_os + 1,
     &     hdl_os_p = hdl_os_wo_z + 1,
     &     hdl_os_blk_mv_k = hdl_os_p + 1,
     &     hdl_z = hdl_os_blk_mv_k + 1,
     &     hdl_z_m1 =  hdl_z-1)
c
c=======================================================================
c
c     Parameters for where to open blocks in memory manager
c
      integer ope_blk_anywhere
      integer ope_blk_at_f_fr_p
      integer ope_blk_at_l_fr_p
      parameter (
     &     ope_blk_anywhere =  0,
     &     ope_blk_at_f_fr_p = 1,
     &     ope_blk_at_l_fr_p = 2)
c
c=======================================================================
c
c     Memory manager return codes (levels and IDs)
c
c     Memory manager return levels
c
      integer mem_mgr_rt_lvl_bs
      integer mem_mgr_rt_lvl_info
      integer mem_mgr_rt_lvl_warn
      integer mem_mgr_rt_lvl_er
      integer mem_mgr_rt_lvl_serious
      parameter (
     &     mem_mgr_rt_lvl_bs =        100,
     &     mem_mgr_rt_lvl_info =    0*mem_mgr_rt_lvl_bs,
     &     mem_mgr_rt_lvl_warn =    1*mem_mgr_rt_lvl_bs,
     &     mem_mgr_rt_lvl_er =      2*mem_mgr_rt_lvl_bs,
     &     mem_mgr_rt_lvl_serious = 3*mem_mgr_rt_lvl_bs)
c
c     Memory manager return IDs
c
      integer mem_mgr_rt_id_ok
      integer mem_mgr_rt_id_mv_blk
      integer mem_mgr_rt_id_no_po
      integer mem_mgr_rt_id_g_p_wg_wo_z
      integer mem_mgr_rt_id_mx_blk_id
 
      integer mem_mgr_rt_id_f7_wr_er
 
      integer mem_mgr_rt_id_no_ca_iz
      integer mem_mgr_rt_id_pm_er
      integer mem_mgr_rt_id_no_blk
      integer mem_mgr_rt_id_blk_no_po
      integer mem_mgr_rt_id_g_p_no_blk
 
      integer mem_mgr_rt_id_int_er
      integer mem_mgr_rt_id_blk_mv_lck_on
      integer mem_mgr_rt_id_n_fr_wo_er
      integer mem_mgr_rt_id_blk_n_fr_ls_er
 
      integer mem_mgr_rt_id_emsol_p_er
      integer mem_mgr_rt_id_usr_p_er
 
      integer mem_mgr_rt_id_un_idf_rp_rt_cod
 
      parameter (
     &     mem_mgr_rt_id_ok =           0,
     &     mem_mgr_rt_id_mv_blk =       1,
     &     mem_mgr_rt_id_no_po =        2,
     &     mem_mgr_rt_id_g_p_wg_wo_z  = 3,
     &     mem_mgr_rt_id_mx_blk_id =    4)
 
      parameter (
     &     mem_mgr_rt_id_f7_wr_er =    99)
 
      parameter (
     &     mem_mgr_rt_id_no_ca_iz =   100,
     &     mem_mgr_rt_id_pm_er =      101,
     &     mem_mgr_rt_id_no_blk =     102,
     &     mem_mgr_rt_id_blk_no_po =  103,
     &     mem_mgr_rt_id_g_p_no_blk = 104)
 
      parameter (
     &     mem_mgr_rt_id_int_er =         110,
     &     mem_mgr_rt_id_blk_mv_lck_on =  111,
     &     mem_mgr_rt_id_n_fr_wo_er =     112,
     &     mem_mgr_rt_id_blk_n_fr_ls_er = 113)
 
      parameter (
     &     mem_mgr_rt_id_emsol_p_er = 120,
     &     mem_mgr_rt_id_usr_p_er =   121)
 
      parameter (
     &     mem_mgr_rt_id_un_idf_rp_rt_cod = 999)
c
c     Memory manager return codes
c
c     Informational:
c
      integer mem_mgr_rt_cod_ok
      integer mem_mgr_rt_cod_mv_blk
      parameter (mem_mgr_rt_cod_ok = mem_mgr_rt_id_ok +
     &     mem_mgr_rt_lvl_info)
      parameter (mem_mgr_rt_cod_mv_blk = mem_mgr_rt_id_mv_blk +
     &     mem_mgr_rt_lvl_info)
c
c     Warning:
c
      integer mem_mgr_rt_cod_g_p_wg_wo_z
      integer mem_mgr_rt_cod_mx_blk_id
      parameter (
     &     mem_mgr_rt_cod_g_p_wg_wo_z = mem_mgr_rt_id_g_p_wg_wo_z +
     &     mem_mgr_rt_lvl_warn)
      parameter (
     &     mem_mgr_rt_cod_mx_blk_id = mem_mgr_rt_id_mx_blk_id +
     &     mem_mgr_rt_lvl_warn)
c
c     Serious:
c
      integer mem_mgr_rt_cod_serious_no_po
      integer mem_mgr_rt_cod_serious_f7_wr_er
      integer mem_mgr_rt_cod_no_ca_iz
      integer mem_mgr_rt_cod_pm_er
      integer mem_mgr_rt_cod_no_blk
      integer mem_mgr_rt_cod_blk_no_po
      integer mem_mgr_rt_cod_g_p_no_blk
      integer mem_mgr_rt_cod_int_er
      integer mem_mgr_rt_cod_blk_mv_lck_on
      integer mem_mgr_rt_cod_n_fr_wo_er
      integer mem_mgr_rt_cod_blk_n_fr_ls_er
      integer mem_mgr_rt_cod_un_idf_rp_rt_cod
      integer mem_mgr_rt_cod_emsol_p_er
      integer mem_mgr_rt_cod_usr_p_er
      parameter (
     &     mem_mgr_rt_cod_serious_no_po = mem_mgr_rt_id_no_po +
     &     mem_mgr_rt_lvl_serious)
      parameter (
     &     mem_mgr_rt_cod_serious_f7_wr_er = mem_mgr_rt_id_f7_wr_er +
     &     mem_mgr_rt_lvl_serious)
      parameter (
     &     mem_mgr_rt_cod_no_ca_iz =
     &     mem_mgr_rt_id_no_ca_iz + mem_mgr_rt_lvl_serious)
      parameter (
     &     mem_mgr_rt_cod_pm_er =
     &     mem_mgr_rt_id_pm_er + mem_mgr_rt_lvl_serious)
      parameter (
     &     mem_mgr_rt_cod_no_blk =
     &     mem_mgr_rt_id_no_blk + mem_mgr_rt_lvl_serious)
      parameter (
     &     mem_mgr_rt_cod_blk_no_po =
     &     mem_mgr_rt_id_blk_no_po + mem_mgr_rt_lvl_serious)
      parameter (
     &     mem_mgr_rt_cod_g_p_no_blk =
     &     mem_mgr_rt_id_g_p_no_blk + mem_mgr_rt_lvl_serious)
      parameter (
     &     mem_mgr_rt_cod_int_er =
     &     mem_mgr_rt_id_int_er + mem_mgr_rt_lvl_serious)
      parameter (
     &     mem_mgr_rt_cod_blk_mv_lck_on =
     &     mem_mgr_rt_id_blk_mv_lck_on + mem_mgr_rt_lvl_serious)
      parameter (
     &     mem_mgr_rt_cod_n_fr_wo_er =
     &     mem_mgr_rt_id_n_fr_wo_er + mem_mgr_rt_lvl_serious)
      parameter (
     &     mem_mgr_rt_cod_blk_n_fr_ls_er =
     &     mem_mgr_rt_id_blk_n_fr_ls_er + mem_mgr_rt_lvl_serious)
      parameter (
     &     mem_mgr_rt_cod_un_idf_rp_rt_cod =
     &     mem_mgr_rt_id_un_idf_rp_rt_cod + mem_mgr_rt_lvl_serious)
      parameter (
     &     mem_mgr_rt_cod_emsol_p_er =
     &     mem_mgr_rt_id_emsol_p_er + mem_mgr_rt_lvl_serious)
      parameter (
     &     mem_mgr_rt_cod_usr_p_er =
     &     mem_mgr_rt_id_usr_p_er + mem_mgr_rt_lvl_serious)
 
