CM
      integer n_rsmi_blk_ct_vr
      parameter (n_rsmi_blk_ct_vr=4)
      integer rsmi_blk_ct_vr(n_rsmi_blk_ct_vr)
      common/rsmi_blk_ct_vr_com/rsmi_blk_ct_vr
 
      integer rsmi_blk_ml_n
      integer rsmi_blk_mx_n_r
      integer rsmi_blk_mx_n_c
      integer rsmi_blk_st_msk
      equivalence (rsmi_blk_ml_n,             rsmi_blk_ct_vr(1))
      equivalence (rsmi_blk_mx_n_r,           rsmi_blk_ct_vr(2))
      equivalence (rsmi_blk_mx_n_c,           rsmi_blk_ct_vr(3))
      equivalence (rsmi_blk_st_msk,           rsmi_blk_ct_vr(4))
 
      integer rsmi_blk_st_rsmi
      integer rsmi_blk_st_rsmi_wk
      integer rsmi_blk_st_rsmi_bd
      integer rsmi_blk_st_l1_cz_r
      integer rsmi_blk_st_r_eta_fi_p
      integer rsmi_blk_st_r_eta_fi_en
      parameter (
     &     rsmi_blk_st_rsmi =         bt1,
     &     rsmi_blk_st_rsmi_wk =      bt2,
     &     rsmi_blk_st_rsmi_bd =      bt3,
     &     rsmi_blk_st_l1_cz_r =      bt4,
     &     rsmi_blk_st_r_eta_fi_p =   bt5,
     &     rsmi_blk_st_r_eta_fi_en =  bt6)
 
      integer n_rsmi_hdl, ln_rsmi_hdl
      integer n_rsmi_wk_hdl, ln_rsmi_wk_hdl
      integer n_rsmi_bd_hdl, ln_rsmi_bd_hdl
      integer n_l1_cz_r_hdl, ln_l1_cz_r_hdl
      integer n_r_eta_fi_p_hdl, ln_r_eta_fi_p_hdl
      integer n_r_eta_fi_en_hdl, ln_r_eta_fi_en_hdl
      parameter (n_rsmi_hdl=11, ln_rsmi_hdl=n_rsmi_hdl*hdl_z)
      parameter (n_rsmi_wk_hdl=5, ln_rsmi_wk_hdl=n_rsmi_wk_hdl*hdl_z)
      parameter (n_rsmi_bd_hdl=3, ln_rsmi_bd_hdl=n_rsmi_bd_hdl*hdl_z)
      parameter (n_l1_cz_r_hdl=3, ln_l1_cz_r_hdl=n_l1_cz_r_hdl*hdl_z)
      parameter (
     &     n_r_eta_fi_p_hdl =  4,
     &     ln_r_eta_fi_p_hdl = n_r_eta_fi_p_hdl*hdl_z)
      parameter (
     &     n_r_eta_fi_en_hdl =  4,
     &     ln_r_eta_fi_en_hdl = n_r_eta_fi_en_hdl*hdl_z)
      integer rsmi_hdl(ln_rsmi_hdl)
      integer rsmi_wk_hdl(ln_rsmi_wk_hdl)
      integer rsmi_bd_hdl(ln_rsmi_bd_hdl)
      integer l1_cz_r_hdl(ln_l1_cz_r_hdl)
      integer r_eta_fi_p_hdl(ln_r_eta_fi_p_hdl)
      integer r_eta_fi_en_hdl(ln_r_eta_fi_en_hdl)
      common/ems_com_rsmi_hdl/
     &     rsmi_hdl,
     &     rsmi_wk_hdl,
     &     rsmi_bd_hdl,
     &     l1_cz_r_hdl,
     &     r_eta_fi_p_hdl,
     &     r_eta_fi_en_hdl
c
      integer blk_rsmi
      integer hdl_bc_co_v(0:hdl_z_m1)
      integer ix_hdl_bc_co_v
      parameter (ix_hdl_bc_co_v =       1)
      equivalence (hdl_bc_co_v(0),      rsmi_hdl(ix_hdl_bc_co_v))
      equivalence (blk_rsmi,
     &     rsmi_hdl(ix_hdl_bc_co_v+hdl_os_blk_n))
c
      integer hdl_bc_co_ix(0:hdl_z_m1)
      integer ix_hdl_bc_co_ix
      parameter (ix_hdl_bc_co_ix =      ix_hdl_bc_co_v + hdl_z)
      equivalence (hdl_bc_co_ix(0),     rsmi_hdl(ix_hdl_bc_co_ix))
c
      integer hdl_bc_co_ix_bar(0:hdl_z_m1)
      integer ix_hdl_bc_co_ix_bar
      parameter (ix_hdl_bc_co_ix_bar =  ix_hdl_bc_co_ix + hdl_z)
      equivalence (hdl_bc_co_ix_bar(0), rsmi_hdl(ix_hdl_bc_co_ix_bar))
c
      integer hdl_pi_v(0:hdl_z_m1)
      integer ix_hdl_pi_v
      parameter (ix_hdl_pi_v =          ix_hdl_bc_co_ix_bar + hdl_z)
      equivalence (hdl_pi_v(0),         rsmi_hdl(ix_hdl_pi_v))
c
      integer hdl_pi_ix(0:hdl_z_m1)
      integer ix_hdl_pi_ix
      parameter (ix_hdl_pi_ix =         ix_hdl_pi_v + hdl_z)
      equivalence (hdl_pi_ix(0),        rsmi_hdl(ix_hdl_pi_ix))
c
      integer hdl_tbu_r_v(0:hdl_z_m1)
      integer ix_hdl_tbu_r_v
      parameter (ix_hdl_tbu_r_v =       ix_hdl_pi_ix + hdl_z)
      equivalence (hdl_tbu_r_v(0),      rsmi_hdl(ix_hdl_tbu_r_v))
c
      integer hdl_tbu_r_ix(0:hdl_z_m1)
      integer ix_hdl_tbu_r_ix
      parameter (ix_hdl_tbu_r_ix =       ix_hdl_tbu_r_v + hdl_z)
      equivalence (hdl_tbu_r_ix(0),      rsmi_hdl(ix_hdl_tbu_r_ix))
c
      integer hdl_pv_c_v(0:hdl_z_m1)
      integer ix_hdl_pv_c_v
      parameter (ix_hdl_pv_c_v =        ix_hdl_tbu_r_ix + hdl_z)
      equivalence (hdl_pv_c_v(0),       rsmi_hdl(ix_hdl_pv_c_v))
c
      integer hdl_perm_tran_vec(0:hdl_z_m1)
      integer ix_hdl_perm_tran_vec
      parameter (ix_hdl_perm_tran_vec = ix_hdl_pv_c_v + hdl_z)
      equivalence (hdl_perm_tran_vec(0), rsmi_hdl(ix_hdl_perm_tran_vec))
c
      integer hdl_cz_r_cdd_ix(0:hdl_z_m1)
      integer ix_hdl_cz_r_cdd_ix
      parameter (ix_hdl_cz_r_cdd_ix = ix_hdl_perm_tran_vec + hdl_z)
      equivalence (hdl_cz_r_cdd_ix(0), rsmi_hdl(ix_hdl_cz_r_cdd_ix))
c
      integer hdl_og_bs_cg_st(0:hdl_z_m1)
      integer ix_hdl_og_bs_cg_st
      parameter (ix_hdl_og_bs_cg_st = ix_hdl_cz_r_cdd_ix + hdl_z)
      equivalence (hdl_og_bs_cg_st(0), rsmi_hdl(ix_hdl_og_bs_cg_st))
c
      integer blk_rsmi_wk
      integer hdl_rsmi_rl_wk_a1(0:hdl_z_m1)
      integer ix_hdl_rsmi_rl_wk_a1
      parameter (ix_hdl_rsmi_rl_wk_a1 =       1)
      equivalence (hdl_rsmi_rl_wk_a1(0),
     &     rsmi_wk_hdl(ix_hdl_rsmi_rl_wk_a1))
      equivalence (blk_rsmi_wk,
     &     rsmi_wk_hdl(ix_hdl_rsmi_rl_wk_a1+hdl_os_blk_n))
c
      integer hdl_rsmi_rl_wk_a2(0:hdl_z_m1)
      integer ix_hdl_rsmi_rl_wk_a2
      parameter (ix_hdl_rsmi_rl_wk_a2 = ix_hdl_rsmi_rl_wk_a1 + hdl_z)
      equivalence (hdl_rsmi_rl_wk_a2(0),
     &     rsmi_wk_hdl(ix_hdl_rsmi_rl_wk_a2))
c
      integer hdl_rsmi_rl_wk_a3(0:hdl_z_m1)
      integer ix_hdl_rsmi_rl_wk_a3
      parameter (ix_hdl_rsmi_rl_wk_a3 = ix_hdl_rsmi_rl_wk_a2 + hdl_z)
      equivalence (hdl_rsmi_rl_wk_a3(0),
     &     rsmi_wk_hdl(ix_hdl_rsmi_rl_wk_a3))
c
      integer hdl_rsmi_i_wk_a1(0:hdl_z_m1)
      integer ix_hdl_rsmi_i_wk_a1
      parameter (ix_hdl_rsmi_i_wk_a1 = ix_hdl_rsmi_rl_wk_a3 + hdl_z)
      equivalence (hdl_rsmi_i_wk_a1(0),
     &     rsmi_wk_hdl(ix_hdl_rsmi_i_wk_a1))
c
      integer hdl_rsmi_i_wk_a2(0:hdl_z_m1)
      integer ix_hdl_rsmi_i_wk_a2
      parameter (ix_hdl_rsmi_i_wk_a2 = ix_hdl_rsmi_i_wk_a1 + hdl_z)
      equivalence (hdl_rsmi_i_wk_a2(0),
     &     rsmi_wk_hdl(ix_hdl_rsmi_i_wk_a2))
c
      integer blk_rsmi_bd
      integer hdl_rsmi_lb(0:hdl_z_m1)
      integer ix_hdl_rsmi_lb
      parameter (ix_hdl_rsmi_lb =       1)
      equivalence (hdl_rsmi_lb(0),      rsmi_bd_hdl(ix_hdl_rsmi_lb))
      equivalence (blk_rsmi_bd,
     &     rsmi_bd_hdl(ix_hdl_rsmi_lb+hdl_os_blk_n))
c
      integer hdl_rsmi_co(0:hdl_z_m1)
      integer ix_hdl_rsmi_co
      parameter (ix_hdl_rsmi_co =       ix_hdl_rsmi_lb + hdl_z)
      equivalence (hdl_rsmi_co(0),      rsmi_bd_hdl(ix_hdl_rsmi_co))
c
      integer hdl_rsmi_ub(0:hdl_z_m1)
      integer ix_hdl_rsmi_ub
      parameter (ix_hdl_rsmi_ub =       ix_hdl_rsmi_co + hdl_z)
      equivalence (hdl_rsmi_ub(0),      rsmi_bd_hdl(ix_hdl_rsmi_ub))
c
      integer blk_l1_cz_r
      integer hdl_l1_cz_r_bp(0:hdl_z_m1)
      integer ix_hdl_l1_cz_r_bp
      parameter (ix_hdl_l1_cz_r_bp = 1)
      equivalence (hdl_l1_cz_r_bp(0),
     &     l1_cz_r_hdl(ix_hdl_l1_cz_r_bp))
      equivalence (blk_l1_cz_r,
     &     l1_cz_r_hdl(ix_hdl_l1_cz_r_bp+hdl_os_blk_n))
c
      integer hdl_l1_cz_r_dl_gd(0:hdl_z_m1)
      integer ix_hdl_l1_cz_r_dl_gd
      parameter (ix_hdl_l1_cz_r_dl_gd = ix_hdl_l1_cz_r_bp + hdl_z)
      equivalence (hdl_l1_cz_r_dl_gd(0),
     &     l1_cz_r_hdl(ix_hdl_l1_cz_r_dl_gd))
c
      integer hdl_l1_cz_r_ix(0:hdl_z_m1)
      integer ix_hdl_l1_cz_r_ix
      parameter (ix_hdl_l1_cz_r_ix = ix_hdl_l1_cz_r_dl_gd + hdl_z)
      equivalence (hdl_l1_cz_r_ix(0),
     &     l1_cz_r_hdl(ix_hdl_l1_cz_r_ix))
c
      integer blk_r_eta_fi_p
      integer hdl_lo_eta_r_sa(0:hdl_z_m1)
      integer ix_hdl_lo_eta_r_sa
      parameter (ix_hdl_lo_eta_r_sa = 1)
      equivalence (hdl_lo_eta_r_sa(0),
     &     r_eta_fi_p_hdl(ix_hdl_lo_eta_r_sa))
      equivalence (blk_r_eta_fi_p,
     &     r_eta_fi_p_hdl(ix_hdl_lo_eta_r_sa+hdl_os_blk_n))
c
      integer hdl_up_eta_r_sa(0:hdl_z_m1)
      integer ix_hdl_up_eta_r_sa
      parameter (ix_hdl_up_eta_r_sa = ix_hdl_lo_eta_r_sa + hdl_z)
      equivalence (hdl_up_eta_r_sa(0),
     &     r_eta_fi_p_hdl(ix_hdl_up_eta_r_sa))
c
      integer hdl_lo_eta_pv_in_c(0:hdl_z_m1)
      integer ix_hdl_lo_eta_pv_in_c
      parameter (ix_hdl_lo_eta_pv_in_c = ix_hdl_up_eta_r_sa + hdl_z)
      equivalence (hdl_lo_eta_pv_in_c(0),
     &     r_eta_fi_p_hdl(ix_hdl_lo_eta_pv_in_c))
c
      integer hdl_up_eta_pv_in_c(0:hdl_z_m1)
      integer ix_hdl_up_eta_pv_in_c
      parameter (ix_hdl_up_eta_pv_in_c = ix_hdl_lo_eta_pv_in_c + hdl_z)
      equivalence (hdl_up_eta_pv_in_c(0),
     &     r_eta_fi_p_hdl(ix_hdl_up_eta_pv_in_c))
c
      integer blk_r_eta_fi_en
      integer hdl_lo_eta_c_v(0:hdl_z_m1)
      integer ix_hdl_lo_eta_c_v
      parameter (ix_hdl_lo_eta_c_v =       1)
      equivalence (hdl_lo_eta_c_v(0),
     &     r_eta_fi_en_hdl(ix_hdl_lo_eta_c_v))
      equivalence (blk_r_eta_fi_en,
     &     r_eta_fi_en_hdl(ix_hdl_lo_eta_c_v+hdl_os_blk_n))
c
      integer hdl_lo_eta_c_ix(0:hdl_z_m1)
      integer ix_hdl_lo_eta_c_ix
      parameter (ix_hdl_lo_eta_c_ix =      ix_hdl_lo_eta_c_v + hdl_z)
      equivalence (hdl_lo_eta_c_ix(0),
     &     r_eta_fi_en_hdl(ix_hdl_lo_eta_c_ix))
c
      integer hdl_up_eta_c_v(0:hdl_z_m1)
      integer ix_hdl_up_eta_c_v
      parameter (ix_hdl_up_eta_c_v =       ix_hdl_lo_eta_c_ix + hdl_z)
      equivalence (hdl_up_eta_c_v(0),
     &     r_eta_fi_en_hdl(ix_hdl_up_eta_c_v))
c
      integer hdl_up_eta_c_ix(0:hdl_z_m1)
      integer ix_hdl_up_eta_c_ix
      parameter (ix_hdl_up_eta_c_ix =      ix_hdl_up_eta_c_v + hdl_z)
      equivalence (hdl_up_eta_c_ix(0),
     &     r_eta_fi_en_hdl(ix_hdl_up_eta_c_ix))
