CM
      integer tom_inv_wk_blk
      integer hdl_tom_inv_rl_wk(0:hdl_z_m1),      p_tom_inv_rl_wk
      integer hdl_tom_inv_bs_c_at(0:hdl_z_m1),    p_tom_inv_bs_c_at
      integer hdl_tom_inv_r_k_a(0:hdl_z_m1),      p_tom_inv_r_k_a
      integer hdl_tom_inv_pv_r_n_or_mrt(0:hdl_z_m1)
      integer p_tom_inv_pv_r_n_or_mrt
      integer hdl_tom_inv_og_vr_in_r(0:hdl_z_m1), p_tom_inv_og_vr_in_r
      integer hdl_tom_inv_og_st(0:hdl_z_m1),      p_tom_inv_og_st
 
      common/ems_com_tom_inv/
     &     hdl_tom_inv_rl_wk,
     &     hdl_tom_inv_bs_c_at,
     &     hdl_tom_inv_r_k_a,
     &     hdl_tom_inv_pv_r_n_or_mrt,
     &     hdl_tom_inv_og_vr_in_r,
     &     hdl_tom_inv_og_st
 
      equivalence (tom_inv_wk_blk,    hdl_tom_inv_rl_wk(hdl_os_blk_n))
      equivalence (p_tom_inv_rl_wk,   hdl_tom_inv_rl_wk(hdl_os_p))
      equivalence (p_tom_inv_bs_c_at, hdl_tom_inv_bs_c_at(hdl_os_p))
      equivalence (p_tom_inv_r_k_a, hdl_tom_inv_r_k_a(hdl_os_p))
      equivalence (p_tom_inv_pv_r_n_or_mrt,
     &     hdl_tom_inv_pv_r_n_or_mrt(hdl_os_p))
      equivalence (p_tom_inv_og_vr_in_r,
     &     hdl_tom_inv_og_vr_in_r(hdl_os_p))
      equivalence (p_tom_inv_og_st,   hdl_tom_inv_og_st(hdl_os_p))
