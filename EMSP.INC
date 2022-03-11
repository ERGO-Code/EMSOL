CM
c     NB:
c     cp_o_eta_fi_p_a_rec_z should be identical to eta_fi_p_a_rec_z in
c     SLAPCS.INC: a copy is made here to avoid having to include
c     SLAPCS.INC in EMSP.INC just to get this value.
c
      integer cp_o_eta_fi_p_a_rec_z
      parameter (cp_o_eta_fi_p_a_rec_z = 4)
 
      integer ix_p_lbc
      integer ix_p_cbp
      integer ix_p_ubc
      integer ix_p_nm
 
      integer ix_p_lng_nm
 
      integer ix_p_pk_bs_os
 
      integer ix_p_pwl_vr_rf_pr_act_v
      integer ix_p_pwl_vr_rf_ob_fn_v
      integer ix_p_pwl_vr_da_v
      integer ix_p_pwl_vr_ls
      integer ix_p_pwl_vr_da_sa
      integer ix_p_pwl_vr_cu_sn
      integer ix_p_pwl_vr_ix
      integer ix_p_pwl_vr_usr_bp
      integer ix_p_pwl_vr_usr_ls
      integer ix_p_pwl_vr_usr_sn
      integer ix_p_pwl_vr_usr_da_sa
      integer ix_p_pwl_vr_usr_cu_sn
 
      integer ix_p_scl
      integer ix_p_pr_act
      integer ix_p_du_act
      integer ix_p_ed_wt
      integer ix_p_st
 
      integer ix_p_vr_in_r
      integer ix_p_vr_in_c
 
      integer ix_p_lo_eta_pv_in_r
      integer ix_p_up_eta_pv_in_r
      integer ix_p_eta_w_l_en_in_r
      integer ix_p_eta_w_lm1_en_in_r
      integer ix_p_og_t_nw_perm
      integer ix_p_nw_t_og_perm
 
      integer ix_p_dvx_ix
 
      integer ix_p_mtx_r_v
      integer ix_p_mtx_r_ix
      integer ix_p_mtx_c_sa
      integer ix_p_mtx_c_ln
 
      integer ix_p_mtx_c_v
      integer ix_p_mtx_c_ix
      integer ix_p_mtx_r_sa
 
      integer ix_p_aux_blk_v
      integer ix_p_aux_blk_r_ix
      integer ix_p_aux_blk_c_ix
 
      integer ix_p_r_aux_sol
      integer ix_p_c_aux_sol
 
      integer ix_p_prsl_sv_ml_lbc
      integer ix_p_prsl_sv_ml_ubc
      integer ix_p_prsl_sv_ml_cbp
      integer ix_p_prsl_sv_mtx_r_v
      integer ix_p_prsl_sv_mtx_r_ix
      integer ix_p_prsl_sv_mtx_c_sa
      integer ix_p_prsl_rm_vr_ls
      integer ix_p_prsl_rm_vr_st
      integer ix_p_prsl_rm_vr_pr_act
 
      integer ix_p_co_rg_up_co_v
      integer ix_p_co_rg_lo_co_v
      integer ix_p_co_rg_up_ob_v
      integer ix_p_co_rg_lo_ob_v
      integer ix_p_co_rg_up_act_v
      integer ix_p_co_rg_lo_act_v
      integer ix_p_co_rg_up_en_vr
      integer ix_p_co_rg_lo_en_vr
      integer ix_p_co_rg_up_lv_vr
      integer ix_p_co_rg_lo_lv_vr
      integer ix_p_bd_rg_up_bd_v
      integer ix_p_bd_rg_lo_bd_v
      integer ix_p_bd_rg_up_ob_v
      integer ix_p_bd_rg_lo_ob_v
      integer ix_p_bd_rg_up_en_vr
      integer ix_p_bd_rg_lo_en_vr
      integer ix_p_bd_rg_up_lv_vr
      integer ix_p_bd_rg_lo_lv_vr
 
      integer ix_p_u_bs_dse_blk
      integer ix_p_u_bs_gthr_pv_r
      integer ix_p_u_bs_dse_blk_pv_r_in_c
      integer ix_p_u_bs_skt_pv_r
      integer ix_p_u_bs_eta_msk
 
      integer ix_p_usr_mtx_r_v
      integer ix_p_usr_mtx_r_ix
      integer ix_p_usr_mtx_c_sa
 
      integer ix_p_usr_mtx_c_v
      integer ix_p_usr_mtx_c_ix
      integer ix_p_usr_mtx_r_sa
 
      integer ix_p_bc_co_v
      integer ix_p_bc_co_ix
      integer ix_p_bc_co_ix_bar
      integer ix_p_pi_v
      integer ix_p_pi_ix
      integer ix_p_tbu_r_v
      integer ix_p_tbu_r_ix
      integer ix_p_pv_c_v
      integer ix_p_perm_tran_vec
      integer ix_p_cz_r_cdd_ix
      integer ix_p_og_bs_cg_st
 
      integer ix_p_rsmi_rl_wk_a1
      integer ix_p_rsmi_rl_wk_a2
      integer ix_p_rsmi_rl_wk_a3
      integer ix_p_rsmi_i_wk_a1
      integer ix_p_rsmi_i_wk_a2
 
      integer ix_p_rsmi_lb
      integer ix_p_rsmi_co
      integer ix_p_rsmi_ub
 
      integer ix_p_l1_cz_r_bp
      integer ix_p_l1_cz_r_dl_gd
      integer ix_p_l1_cz_r_ix
 
      integer ix_p_lo_eta_r_sa
      integer ix_p_up_eta_r_sa
      integer ix_p_lo_eta_pv_in_c
      integer ix_p_up_eta_pv_in_c
      integer ix_p_lo_eta_c_v
      integer ix_p_lo_eta_c_ix
      integer ix_p_up_eta_c_v
      integer ix_p_up_eta_c_ix
 
      integer ix_p_nw_eta_v
      integer ix_p_nw_eta_ix
      integer ix_p_nw_eta_rec
      integer ix_eta_fi_p_a
      integer eta_fi_p_a_ln
      integer n_emsol_p
 
      parameter (ix_p_lbc = 1)
      parameter (ix_p_cbp = ix_p_lbc + 1)
      parameter (ix_p_ubc = ix_p_cbp + 1)
      parameter (ix_p_nm = ix_p_ubc + 1)
 
      parameter (ix_p_lng_nm = ix_p_nm + 1)
 
      parameter (ix_p_pk_bs_os = ix_p_lng_nm + 1)
 
      parameter (ix_p_pwl_vr_rf_pr_act_v = ix_p_pk_bs_os + 1)
      parameter (ix_p_pwl_vr_rf_ob_fn_v =  ix_p_pwl_vr_rf_pr_act_v + 1)
      parameter (ix_p_pwl_vr_da_v =        ix_p_pwl_vr_rf_ob_fn_v + 1)
      parameter (ix_p_pwl_vr_ls =          ix_p_pwl_vr_da_v + 1)
      parameter (ix_p_pwl_vr_da_sa =       ix_p_pwl_vr_ls + 1)
      parameter (ix_p_pwl_vr_cu_sn =       ix_p_pwl_vr_da_sa + 1)
      parameter (ix_p_pwl_vr_ix =          ix_p_pwl_vr_cu_sn + 1)
      parameter (ix_p_pwl_vr_usr_bp =      ix_p_pwl_vr_ix + 1)
      parameter (ix_p_pwl_vr_usr_ls =      ix_p_pwl_vr_usr_bp + 1)
      parameter (ix_p_pwl_vr_usr_sn =      ix_p_pwl_vr_usr_ls + 1)
      parameter (ix_p_pwl_vr_usr_da_sa =   ix_p_pwl_vr_usr_sn + 1)
      parameter (ix_p_pwl_vr_usr_cu_sn =   ix_p_pwl_vr_usr_da_sa + 1)
 
      parameter (ix_p_scl =                ix_p_pwl_vr_usr_cu_sn + 1)
      parameter (ix_p_pr_act =             ix_p_scl + 1)
      parameter (ix_p_du_act =             ix_p_pr_act + 1)
      parameter (ix_p_ed_wt =              ix_p_du_act + 1)
      parameter (ix_p_st =                 ix_p_ed_wt + 1)
 
      parameter (ix_p_vr_in_r =            ix_p_st + 1)
      parameter (ix_p_vr_in_c =            ix_p_vr_in_r + 1)
 
      parameter (ix_p_lo_eta_pv_in_r =     ix_p_vr_in_c + 1)
      parameter (ix_p_up_eta_pv_in_r =     ix_p_lo_eta_pv_in_r + 1)
      parameter (ix_p_eta_w_l_en_in_r =    ix_p_up_eta_pv_in_r + 1)
      parameter (ix_p_eta_w_lm1_en_in_r =  ix_p_eta_w_l_en_in_r + 1)
      parameter (ix_p_og_t_nw_perm =       ix_p_eta_w_lm1_en_in_r + 1)
      parameter (ix_p_nw_t_og_perm =       ix_p_og_t_nw_perm + 1)
 
      parameter (ix_p_dvx_ix =             ix_p_nw_t_og_perm + 1)
 
      parameter (ix_p_mtx_r_v =            ix_p_dvx_ix + 1)
      parameter (ix_p_mtx_r_ix =           ix_p_mtx_r_v + 1)
      parameter (ix_p_mtx_c_sa =           ix_p_mtx_r_ix + 1)
      parameter (ix_p_mtx_c_ln =           ix_p_mtx_c_sa + 1)
      parameter (ix_p_mtx_c_v =            ix_p_mtx_c_ln + 1)
      parameter (ix_p_mtx_c_ix =           ix_p_mtx_c_v + 1)
      parameter (ix_p_mtx_r_sa =           ix_p_mtx_c_ix + 1)
      parameter (ix_p_aux_blk_v =          ix_p_mtx_r_sa + 1)
      parameter (ix_p_aux_blk_r_ix =       ix_p_aux_blk_v + 1)
      parameter (ix_p_aux_blk_c_ix =       ix_p_aux_blk_r_ix + 1)
      parameter (ix_p_r_aux_sol =          ix_p_aux_blk_c_ix + 1)
      parameter (ix_p_c_aux_sol =          ix_p_r_aux_sol + 1)
 
      parameter (ix_p_prsl_sv_ml_lbc =     ix_p_c_aux_sol + 1)
      parameter (ix_p_prsl_sv_ml_ubc =     ix_p_prsl_sv_ml_lbc + 1)
      parameter (ix_p_prsl_sv_ml_cbp =     ix_p_prsl_sv_ml_ubc + 1)
      parameter (ix_p_prsl_sv_mtx_r_v =    ix_p_prsl_sv_ml_cbp + 1)
      parameter (ix_p_prsl_sv_mtx_r_ix =   ix_p_prsl_sv_mtx_r_v + 1)
      parameter (ix_p_prsl_sv_mtx_c_sa =   ix_p_prsl_sv_mtx_r_ix + 1)
      parameter (ix_p_prsl_rm_vr_ls =      ix_p_prsl_sv_mtx_c_sa + 1)
      parameter (ix_p_prsl_rm_vr_st =      ix_p_prsl_rm_vr_ls + 1)
      parameter (ix_p_prsl_rm_vr_pr_act =  ix_p_prsl_rm_vr_st + 1)
 
      parameter (ix_p_co_rg_up_co_v =  ix_p_prsl_rm_vr_pr_act + 1)
      parameter (ix_p_co_rg_lo_co_v =  ix_p_co_rg_up_co_v + 1)
      parameter (ix_p_co_rg_up_ob_v =  ix_p_co_rg_lo_co_v + 1)
      parameter (ix_p_co_rg_lo_ob_v =  ix_p_co_rg_up_ob_v + 1)
      parameter (ix_p_co_rg_up_act_v = ix_p_co_rg_lo_ob_v + 1)
      parameter (ix_p_co_rg_lo_act_v = ix_p_co_rg_up_act_v + 1)
      parameter (ix_p_co_rg_up_en_vr = ix_p_co_rg_lo_act_v + 1)
      parameter (ix_p_co_rg_lo_en_vr = ix_p_co_rg_up_en_vr + 1)
      parameter (ix_p_co_rg_up_lv_vr = ix_p_co_rg_lo_en_vr + 1)
      parameter (ix_p_co_rg_lo_lv_vr = ix_p_co_rg_up_lv_vr + 1)
      parameter (ix_p_bd_rg_up_bd_v =  ix_p_co_rg_lo_lv_vr + 1)
      parameter (ix_p_bd_rg_lo_bd_v =  ix_p_bd_rg_up_bd_v + 1)
      parameter (ix_p_bd_rg_up_ob_v =  ix_p_bd_rg_lo_bd_v + 1)
      parameter (ix_p_bd_rg_lo_ob_v =  ix_p_bd_rg_up_ob_v + 1)
      parameter (ix_p_bd_rg_up_en_vr = ix_p_bd_rg_lo_ob_v + 1)
      parameter (ix_p_bd_rg_lo_en_vr = ix_p_bd_rg_up_en_vr + 1)
      parameter (ix_p_bd_rg_up_lv_vr = ix_p_bd_rg_lo_en_vr + 1)
      parameter (ix_p_bd_rg_lo_lv_vr = ix_p_bd_rg_up_lv_vr + 1)
 
      parameter (ix_p_u_bs_dse_blk =       ix_p_bd_rg_lo_lv_vr+ 1)
      parameter (ix_p_u_bs_gthr_pv_r =     ix_p_u_bs_dse_blk + 1)
      parameter (ix_p_u_bs_dse_blk_pv_r_in_c =
     &     ix_p_u_bs_gthr_pv_r + 1)
      parameter (ix_p_u_bs_skt_pv_r =
     &     ix_p_u_bs_dse_blk_pv_r_in_c + 1)
      parameter (ix_p_u_bs_eta_msk =       ix_p_u_bs_skt_pv_r + 1)
      parameter (ix_p_usr_mtx_r_v =        ix_p_u_bs_eta_msk + 1)
      parameter (ix_p_usr_mtx_r_ix =       ix_p_usr_mtx_r_v + 1)
      parameter (ix_p_usr_mtx_c_sa =       ix_p_usr_mtx_r_ix + 1)
      parameter (ix_p_usr_mtx_c_v =        ix_p_usr_mtx_c_sa + 1)
      parameter (ix_p_usr_mtx_c_ix =       ix_p_usr_mtx_c_v + 1)
      parameter (ix_p_usr_mtx_r_sa =       ix_p_usr_mtx_c_ix + 1)
 
      parameter (ix_p_bc_co_v =            ix_p_usr_mtx_r_sa + 1)
      parameter (ix_p_bc_co_ix =           ix_p_bc_co_v + 1)
      parameter (ix_p_bc_co_ix_bar =       ix_p_bc_co_ix + 1)
      parameter (ix_p_pi_v =               ix_p_bc_co_ix_bar + 1)
      parameter (ix_p_pi_ix =              ix_p_pi_v + 1)
      parameter (ix_p_tbu_r_v =            ix_p_pi_ix + 1)
      parameter (ix_p_tbu_r_ix =           ix_p_tbu_r_v + 1)
      parameter (ix_p_pv_c_v =             ix_p_tbu_r_ix + 1)
      parameter (ix_p_perm_tran_vec =      ix_p_pv_c_v + 1)
      parameter (ix_p_cz_r_cdd_ix =        ix_p_perm_tran_vec + 1)
      parameter (ix_p_og_bs_cg_st =        ix_p_cz_r_cdd_ix + 1)
 
      parameter (ix_p_rsmi_rl_wk_a1 =      ix_p_og_bs_cg_st + 1)
      parameter (ix_p_rsmi_rl_wk_a2 =      ix_p_rsmi_rl_wk_a1 + 1)
      parameter (ix_p_rsmi_rl_wk_a3 =      ix_p_rsmi_rl_wk_a2 + 1)
      parameter (ix_p_rsmi_i_wk_a1 =       ix_p_rsmi_rl_wk_a3 + 1)
      parameter (ix_p_rsmi_i_wk_a2 =       ix_p_rsmi_i_wk_a1 + 1)
 
      parameter (ix_p_rsmi_lb =            ix_p_rsmi_i_wk_a2 + 1)
      parameter (ix_p_rsmi_co =            ix_p_rsmi_lb + 1)
      parameter (ix_p_rsmi_ub =            ix_p_rsmi_co + 1)
 
      parameter (ix_p_l1_cz_r_bp =         ix_p_rsmi_ub + 1)
      parameter (ix_p_l1_cz_r_dl_gd =      ix_p_l1_cz_r_bp + 1)
      parameter (ix_p_l1_cz_r_ix =         ix_p_l1_cz_r_dl_gd + 1)
 
      parameter (ix_p_lo_eta_r_sa =        ix_p_l1_cz_r_ix + 1)
      parameter (ix_p_up_eta_r_sa =        ix_p_lo_eta_r_sa + 1)
      parameter (ix_p_lo_eta_pv_in_c =     ix_p_up_eta_r_sa + 1)
      parameter (ix_p_up_eta_pv_in_c =     ix_p_lo_eta_pv_in_c + 1)
      parameter (ix_p_lo_eta_c_v =         ix_p_up_eta_pv_in_c + 1)
      parameter (ix_p_lo_eta_c_ix =        ix_p_lo_eta_c_v + 1)
      parameter (ix_p_up_eta_c_v =         ix_p_lo_eta_c_ix + 1)
      parameter (ix_p_up_eta_c_ix =        ix_p_up_eta_c_v + 1)
 
      parameter (ix_p_nw_eta_v =           ix_p_up_eta_c_ix + 1)
      parameter (ix_p_nw_eta_ix =          ix_p_nw_eta_v + 1)
      parameter (ix_p_nw_eta_rec =         ix_p_nw_eta_ix + 1)
      parameter (ix_eta_fi_p_a =           ix_p_nw_eta_rec + 1)
      parameter (eta_fi_p_a_ln =
     &     cp_o_eta_fi_p_a_rec_z*eta_fi_mx_n_eta_grp)
      parameter (n_emsol_p = ix_eta_fi_p_a + eta_fi_p_a_ln - 1)
 
      integer p_lbc
      integer p_cbp
      integer p_ubc
      integer p_nm
      integer p_lng_nm
      integer p_pk_bs_os
      integer p_pwl_vr_rf_pr_act_v
      integer p_pwl_vr_rf_ob_fn_v
      integer p_pwl_vr_da_v
      integer p_pwl_vr_ls
      integer p_pwl_vr_da_sa
      integer p_pwl_vr_cu_sn
      integer p_pwl_vr_ix
      integer p_pwl_vr_usr_bp
      integer p_pwl_vr_usr_ls
      integer p_pwl_vr_usr_sn
      integer p_pwl_vr_usr_da_sa
      integer p_pwl_vr_usr_cu_sn
      integer p_scl
      integer p_pr_act
      integer p_du_act
      integer p_ed_wt
      integer p_st
      integer p_vr_in_r
      integer p_vr_in_c
      integer p_lo_eta_pv_in_r
      integer p_up_eta_pv_in_r
      integer p_eta_w_l_en_in_r
      integer p_eta_w_lm1_en_in_r
      integer p_og_t_nw_perm
      integer p_nw_t_og_perm
      integer p_dvx_ix
      integer p_mtx_r_v
      integer p_mtx_r_ix
      integer p_mtx_c_sa
      integer p_mtx_c_ln
      integer p_mtx_c_v
      integer p_mtx_c_ix
      integer p_mtx_r_sa
      integer p_aux_blk_v
      integer p_aux_blk_r_ix
      integer p_aux_blk_c_ix
      integer p_r_aux_sol
      integer p_c_aux_sol
      integer p_prsl_sv_ml_lbc
      integer p_prsl_sv_ml_ubc
      integer p_prsl_sv_ml_cbp
      integer p_prsl_sv_mtx_r_v
      integer p_prsl_sv_mtx_r_ix
      integer p_prsl_sv_mtx_c_sa
      integer p_prsl_rm_vr_ls
      integer p_prsl_rm_vr_st
      integer p_prsl_rm_vr_pr_act
      integer p_co_rg_up_co_v
      integer p_co_rg_lo_co_v
      integer p_co_rg_up_ob_v
      integer p_co_rg_lo_ob_v
      integer p_co_rg_up_act_v
      integer p_co_rg_lo_act_v
      integer p_co_rg_up_en_vr
      integer p_co_rg_lo_en_vr
      integer p_co_rg_up_lv_vr
      integer p_co_rg_lo_lv_vr
      integer p_bd_rg_up_bd_v
      integer p_bd_rg_lo_bd_v
      integer p_bd_rg_up_ob_v
      integer p_bd_rg_lo_ob_v
      integer p_bd_rg_up_en_vr
      integer p_bd_rg_lo_en_vr
      integer p_bd_rg_up_lv_vr
      integer p_bd_rg_lo_lv_vr
      integer p_u_bs_dse_blk
      integer p_u_bs_gthr_pv_r
      integer p_u_bs_dse_blk_pv_r_in_c
      integer p_u_bs_skt_pv_r
      integer p_u_bs_eta_msk
      integer p_usr_mtx_r_v
      integer p_usr_mtx_r_ix
      integer p_usr_mtx_c_sa
      integer p_usr_mtx_c_v
      integer p_usr_mtx_c_ix
      integer p_usr_mtx_r_sa
      integer p_bc_co_v
      integer p_bc_co_ix
      integer p_bc_co_ix_bar
      integer p_pi_v
      integer p_pi_ix
      integer p_tbu_r_v
      integer p_tbu_r_ix
      integer p_pv_c_v
      integer p_perm_tran_vec
      integer p_cz_r_cdd_ix
      integer p_og_bs_cg_st
      integer p_rsmi_rl_wk_a1
      integer p_rsmi_rl_wk_a2
      integer p_rsmi_rl_wk_a3
      integer p_rsmi_i_wk_a1
      integer p_rsmi_i_wk_a2
      integer p_rsmi_lb
      integer p_rsmi_co
      integer p_rsmi_ub
      integer p_l1_cz_r_bp
      integer p_l1_cz_r_dl_gd
      integer p_l1_cz_r_ix
      integer p_lo_eta_r_sa
      integer p_up_eta_r_sa
      integer p_lo_eta_pv_in_c
      integer p_up_eta_pv_in_c
      integer p_lo_eta_c_v
      integer p_lo_eta_c_ix
      integer p_up_eta_c_v
      integer p_up_eta_c_ix
      integer p_nw_eta_v
      integer p_nw_eta_ix
      integer p_nw_eta_rec
      integer eta_fi_p_a(0:eta_fi_p_a_ln-1)
      integer emsol_p(n_emsol_p)
      common/ems_com_emsol_p/emsol_p
 
      equivalence (p_lbc, emsol_p(ix_p_lbc))
      equivalence (p_cbp, emsol_p(ix_p_cbp))
      equivalence (p_ubc, emsol_p(ix_p_ubc))
      equivalence (p_nm,  emsol_p(ix_p_nm))
      equivalence (p_lng_nm,  emsol_p(ix_p_lng_nm))
      equivalence (p_pk_bs_os,  emsol_p(ix_p_pk_bs_os))
      equivalence (p_pwl_vr_rf_pr_act_v,
     &     emsol_p(ix_p_pwl_vr_rf_pr_act_v))
      equivalence (p_pwl_vr_rf_ob_fn_v, emsol_p(ix_p_pwl_vr_rf_ob_fn_v))
      equivalence (p_pwl_vr_da_v,       emsol_p(ix_p_pwl_vr_da_v))
      equivalence (p_pwl_vr_ls,         emsol_p(ix_p_pwl_vr_ls))
      equivalence (p_pwl_vr_da_sa,      emsol_p(ix_p_pwl_vr_da_sa))
      equivalence (p_pwl_vr_cu_sn,      emsol_p(ix_p_pwl_vr_cu_sn))
      equivalence (p_pwl_vr_ix,         emsol_p(ix_p_pwl_vr_ix))
      equivalence (p_pwl_vr_usr_bp,     emsol_p(ix_p_pwl_vr_usr_bp))
      equivalence (p_pwl_vr_usr_ls,     emsol_p(ix_p_pwl_vr_usr_ls))
      equivalence (p_pwl_vr_usr_sn,     emsol_p(ix_p_pwl_vr_usr_sn))
      equivalence (p_pwl_vr_usr_da_sa,  emsol_p(ix_p_pwl_vr_usr_da_sa))
      equivalence (p_pwl_vr_usr_cu_sn,  emsol_p(ix_p_pwl_vr_usr_cu_sn))
      equivalence (p_scl, emsol_p(ix_p_scl))
      equivalence (p_pr_act, emsol_p(ix_p_pr_act))
      equivalence (p_du_act, emsol_p(ix_p_du_act))
      equivalence (p_ed_wt, emsol_p(ix_p_ed_wt))
      equivalence (p_st, emsol_p(ix_p_st))
      equivalence (p_vr_in_r, emsol_p(ix_p_vr_in_r))
      equivalence (p_vr_in_c, emsol_p(ix_p_vr_in_c))
      equivalence (p_lo_eta_pv_in_r, emsol_p(ix_p_lo_eta_pv_in_r))
      equivalence (p_up_eta_pv_in_r, emsol_p(ix_p_up_eta_pv_in_r))
      equivalence (p_eta_w_l_en_in_r, emsol_p(ix_p_eta_w_l_en_in_r))
      equivalence (p_eta_w_lm1_en_in_r, emsol_p(ix_p_eta_w_lm1_en_in_r))
      equivalence (p_og_t_nw_perm, emsol_p(ix_p_og_t_nw_perm))
      equivalence (p_nw_t_og_perm, emsol_p(ix_p_nw_t_og_perm))
      equivalence (p_dvx_ix, emsol_p(ix_p_dvx_ix))
      equivalence (p_mtx_r_v, emsol_p(ix_p_mtx_r_v))
      equivalence (p_mtx_r_ix, emsol_p(ix_p_mtx_r_ix))
      equivalence (p_mtx_c_sa, emsol_p(ix_p_mtx_c_sa))
      equivalence (p_mtx_c_ln, emsol_p(ix_p_mtx_c_ln))
      equivalence (p_mtx_c_v, emsol_p(ix_p_mtx_c_v))
      equivalence (p_mtx_c_ix, emsol_p(ix_p_mtx_c_ix))
      equivalence (p_mtx_r_sa, emsol_p(ix_p_mtx_r_sa))
      equivalence (p_aux_blk_v, emsol_p(ix_p_aux_blk_v))
      equivalence (p_aux_blk_r_ix, emsol_p(ix_p_aux_blk_r_ix))
      equivalence (p_aux_blk_c_ix, emsol_p(ix_p_aux_blk_c_ix))
      equivalence (p_r_aux_sol, emsol_p(ix_p_r_aux_sol))
      equivalence (p_c_aux_sol, emsol_p(ix_p_c_aux_sol))
      equivalence (p_prsl_sv_ml_lbc, emsol_p(ix_p_prsl_sv_ml_lbc))
      equivalence (p_prsl_sv_ml_ubc, emsol_p(ix_p_prsl_sv_ml_ubc))
      equivalence (p_prsl_sv_ml_cbp, emsol_p(ix_p_prsl_sv_ml_cbp))
      equivalence (p_prsl_sv_mtx_r_v, emsol_p(ix_p_prsl_sv_mtx_r_v))
      equivalence (p_prsl_sv_mtx_r_ix, emsol_p(ix_p_prsl_sv_mtx_r_ix))
      equivalence (p_prsl_sv_mtx_c_sa, emsol_p(ix_p_prsl_sv_mtx_c_sa))
      equivalence (p_prsl_rm_vr_ls, emsol_p(ix_p_prsl_rm_vr_ls))
      equivalence (p_prsl_rm_vr_st, emsol_p(ix_p_prsl_rm_vr_st))
      equivalence (p_prsl_rm_vr_pr_act, emsol_p(ix_p_prsl_rm_vr_pr_act))
      equivalence (p_co_rg_up_co_v, emsol_p(ix_p_co_rg_up_co_v))
      equivalence (p_co_rg_lo_co_v, emsol_p(ix_p_co_rg_lo_co_v))
      equivalence (p_co_rg_up_ob_v, emsol_p(ix_p_co_rg_up_ob_v))
      equivalence (p_co_rg_lo_ob_v, emsol_p(ix_p_co_rg_lo_ob_v))
      equivalence (p_co_rg_up_act_v, emsol_p(ix_p_co_rg_up_act_v))
      equivalence (p_co_rg_lo_act_v, emsol_p(ix_p_co_rg_lo_act_v))
      equivalence (p_co_rg_up_en_vr, emsol_p(ix_p_co_rg_up_en_vr))
      equivalence (p_co_rg_lo_en_vr, emsol_p(ix_p_co_rg_lo_en_vr))
      equivalence (p_co_rg_up_lv_vr, emsol_p(ix_p_co_rg_up_lv_vr))
      equivalence (p_co_rg_lo_lv_vr, emsol_p(ix_p_co_rg_lo_lv_vr))
      equivalence (p_bd_rg_up_bd_v, emsol_p(ix_p_bd_rg_up_bd_v))
      equivalence (p_bd_rg_lo_bd_v, emsol_p(ix_p_bd_rg_lo_bd_v))
      equivalence (p_bd_rg_up_ob_v, emsol_p(ix_p_bd_rg_up_ob_v))
      equivalence (p_bd_rg_lo_ob_v, emsol_p(ix_p_bd_rg_lo_ob_v))
      equivalence (p_bd_rg_up_en_vr, emsol_p(ix_p_bd_rg_up_en_vr))
      equivalence (p_bd_rg_lo_en_vr, emsol_p(ix_p_bd_rg_lo_en_vr))
      equivalence (p_bd_rg_up_lv_vr, emsol_p(ix_p_bd_rg_up_lv_vr))
      equivalence (p_bd_rg_lo_lv_vr, emsol_p(ix_p_bd_rg_lo_lv_vr))
      equivalence (p_u_bs_dse_blk, emsol_p(ix_p_u_bs_dse_blk))
      equivalence (p_u_bs_gthr_pv_r, emsol_p(ix_p_u_bs_gthr_pv_r))
      equivalence (p_u_bs_dse_blk_pv_r_in_c,
     &     emsol_p(ix_p_u_bs_dse_blk_pv_r_in_c))
      equivalence (p_u_bs_skt_pv_r, emsol_p(ix_p_u_bs_skt_pv_r))
      equivalence (p_u_bs_eta_msk, emsol_p(ix_p_u_bs_eta_msk))
      equivalence (p_usr_mtx_r_v, emsol_p(ix_p_usr_mtx_r_v))
      equivalence (p_usr_mtx_r_ix, emsol_p(ix_p_usr_mtx_r_ix))
      equivalence (p_usr_mtx_c_sa, emsol_p(ix_p_usr_mtx_c_sa))
      equivalence (p_usr_mtx_c_v, emsol_p(ix_p_usr_mtx_c_v))
      equivalence (p_usr_mtx_c_ix, emsol_p(ix_p_usr_mtx_c_ix))
      equivalence (p_usr_mtx_r_sa, emsol_p(ix_p_usr_mtx_r_sa))
      equivalence (p_bc_co_v, emsol_p(ix_p_bc_co_v))
      equivalence (p_bc_co_ix, emsol_p(ix_p_bc_co_ix))
      equivalence (p_bc_co_ix_bar, emsol_p(ix_p_bc_co_ix_bar))
      equivalence (p_pi_v, emsol_p(ix_p_pi_v))
      equivalence (p_pi_ix, emsol_p(ix_p_pi_ix))
      equivalence (p_tbu_r_v, emsol_p(ix_p_tbu_r_v))
      equivalence (p_tbu_r_ix, emsol_p(ix_p_tbu_r_ix))
      equivalence (p_pv_c_v, emsol_p(ix_p_pv_c_v))
      equivalence (p_perm_tran_vec, emsol_p(ix_p_perm_tran_vec))
      equivalence (p_cz_r_cdd_ix, emsol_p(ix_p_cz_r_cdd_ix))
      equivalence (p_og_bs_cg_st, emsol_p(ix_p_og_bs_cg_st))
      equivalence (p_rsmi_lb, emsol_p(ix_p_rsmi_lb))
      equivalence (p_rsmi_co, emsol_p(ix_p_rsmi_co))
      equivalence (p_rsmi_ub, emsol_p(ix_p_rsmi_ub))
      equivalence (p_rsmi_rl_wk_a1, emsol_p(ix_p_rsmi_rl_wk_a1))
      equivalence (p_rsmi_rl_wk_a2, emsol_p(ix_p_rsmi_rl_wk_a2))
      equivalence (p_rsmi_rl_wk_a3, emsol_p(ix_p_rsmi_rl_wk_a3))
      equivalence (p_rsmi_i_wk_a1, emsol_p(ix_p_rsmi_i_wk_a1))
      equivalence (p_rsmi_i_wk_a2, emsol_p(ix_p_rsmi_i_wk_a2))
      equivalence (p_l1_cz_r_bp, emsol_p(ix_p_l1_cz_r_bp))
      equivalence (p_l1_cz_r_dl_gd, emsol_p(ix_p_l1_cz_r_dl_gd))
      equivalence (p_l1_cz_r_ix, emsol_p(ix_p_l1_cz_r_ix))
      equivalence (p_lo_eta_r_sa, emsol_p(ix_p_lo_eta_r_sa))
      equivalence (p_up_eta_r_sa, emsol_p(ix_p_up_eta_r_sa))
      equivalence (p_lo_eta_pv_in_c, emsol_p(ix_p_lo_eta_pv_in_c))
      equivalence (p_up_eta_pv_in_c, emsol_p(ix_p_up_eta_pv_in_c))
      equivalence (p_lo_eta_c_v, emsol_p(ix_p_lo_eta_c_v))
      equivalence (p_lo_eta_c_ix, emsol_p(ix_p_lo_eta_c_ix))
      equivalence (p_up_eta_c_v, emsol_p(ix_p_up_eta_c_v))
      equivalence (p_up_eta_c_ix, emsol_p(ix_p_up_eta_c_ix))
      equivalence (p_nw_eta_v, emsol_p(ix_p_nw_eta_v))
      equivalence (p_nw_eta_ix, emsol_p(ix_p_nw_eta_ix))
      equivalence (p_nw_eta_rec, emsol_p(ix_p_nw_eta_rec))
      equivalence (eta_fi_p_a, emsol_p(ix_eta_fi_p_a))
c
c     The work arrays can be used anywhere but it is handy to have
c     a flag to indicate that they are being used so that they are not
c     re-used accidentally by another routine. Routines
c     ems_g(fr)_rsmi_rl(i)_wk_a_ix are used to get and free indices of
c     workspace arrays. These are used to index the arrays of
c     (equivalenced) pointers---safe since getting and freeing pointers
c     is vulnerable to block moves.
c
      integer p_rsmi_rl_wk_a(3)
      integer p_rsmi_i_wk_a(2)
      equivalence (p_rsmi_rl_wk_a(1), p_rsmi_rl_wk_a1)
c      equivalence (p_rsmi_rl_wk_a(2), p_rsmi_rl_wk_a2)
c      equivalence (p_rsmi_rl_wk_a(3), p_rsmi_rl_wk_a3)
      equivalence (p_rsmi_i_wk_a(1), p_rsmi_i_wk_a1)
c      equivalence (p_rsmi_i_wk_a(2), p_rsmi_i_wk_a2)
      logical use_rsmi_rl_wk_a1_ok
      logical use_rsmi_rl_wk_a2_ok
      logical use_rsmi_rl_wk_a3_ok
      logical use_rsmi_i_wk_a1_ok
      logical use_rsmi_i_wk_a2_ok
      common/ems_use_rsmi_wk_a_ok_com/
     &     use_rsmi_rl_wk_a1_ok,
     &     use_rsmi_rl_wk_a2_ok,
     &     use_rsmi_rl_wk_a3_ok,
     &     use_rsmi_i_wk_a1_ok,
     &     use_rsmi_i_wk_a2_ok
