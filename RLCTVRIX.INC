CM
      integer ix_usr_tl_pr_ifs, ix_usr_tl_du_ifs
      integer ix_mx_mn, ix_pr_wt
      integer ix_bb_ub
      integer ix_ob_fn_v
      integer ix_su_pr_ifs, ix_su_du_ifs
      integer ix_mps_ze
      integer ix_dga_scl
      integer ix_bst_i_fs_ob_fn_v
      integer ix_i_ifs_wt
      integer ix_tl_i_fs_ob_fn_v_imp
      integer ix_i_fs_ob_fn_v_tgt
      integer ix_tl_i_ifs
      integer ix_bst_ob_fn_v
      integer ix_bst_est_ob_fn_v
 
      integer ix_wk_tl_pr_ifs, ix_wk_tl_du_ifs, ix_wk_xp_tau
      integer ix_tl_pr_ifs, ix_tl_du_ifs, ix_xp_tau
      integer ix_tl_bwd_tran_dse_rhs
      integer ix_tl_fwd_tran_dse_rhs
      integer ix_sus_eta_srch_co_mu
      integer ix_tl_dse_tbu_r
 
      integer ix_usr_tl_crsh_abs_pv_v
      integer ix_usr_tl_crsh_rlv_pv_v
      integer ix_tl_iz_bs_tran_er
      integer ix_tl_mx_iz_pr_act
 
      integer ix_tl_du_act_er, ix_tl_pv_er
      integer ix_tl_ed_wt_er, ix_tl_tran_er
      integer ix_tl_cz_r_growth
      integer ix_fac_long_prd, ix_fac_short_prd
 
      integer ix_bwd_tran_ze
      integer ix_pc_ze
      integer ix_fwd_tran_ze
      integer ix_u_ed_wt_ze
      integer ix_pk_pv_c_ze
      integer ix_scl_v_ze
 
      integer ix_av_eta_dse
      integer ix_av_ftran_sol_dse
      integer ix_av_btran_sol_dse
      integer ix_av_tbu_r_dse
      integer ix_tl_dvx_wt
      integer ix_nw_dvx_fwk_fq
 
      integer ix_inv_pv_tl
      integer ix_inv_wr_eta_tl
      integer ix_inv_c_rlv_tl
      integer ix_inv_unit_v_tl
      integer ix_inv_l_pv_rsdu_er_tl
      integer ix_usr_cz_r_pv_tl
      integer ix_wk_cz_r_pv_tl
      integer ix_cz_r_pv_tl
 
      integer ix_tl_bc_pr_act_it_rfn
      integer ix_tl_reset_pi_it_rfn
      integer ix_tl_pv_c_it_rfn
      integer ix_tl_full_pi_it_rfn
      integer ix_tl_u_pi_it_rfn
      integer ix_bc_pr_act_it_rfn_tran_ze
      integer ix_reset_pi_it_rfn_tran_ze
      integer ix_pv_c_it_rfn_tran_ze
      integer ix_full_pi_it_rfn_tran_ze
      integer ix_u_pi_it_rfn_tran_ze
 
      integer ix_ob_fn_cs
      integer ix_su_non_bc_pr_ifs
 
      integer ix_rl_ml_nm
      integer n_ems_rl_ct_vr
      integer n_osl_rl_ct_vr
 
      parameter (ix_usr_tl_pr_ifs =    1)
      parameter (ix_usr_tl_du_ifs =    2)
      parameter (ix_mx_mn =            3)
      parameter (ix_pr_wt =           14)
      parameter (ix_bb_ub =           16)
      parameter (ix_ob_fn_v =         18)
      parameter (ix_su_pr_ifs =       19)
      parameter (ix_su_du_ifs =       20)
      parameter (ix_mps_ze =              21)
      parameter (ix_dga_scl =             22)
      parameter (ix_bst_i_fs_ob_fn_v =    23)
      parameter (ix_i_ifs_wt =            24)
      parameter (ix_tl_i_fs_ob_fn_v_imp = 25)
      parameter (ix_i_fs_ob_fn_v_tgt =    26)
      parameter (ix_tl_i_ifs =            27)
      parameter (ix_bst_ob_fn_v =         28)
      parameter (ix_bst_est_ob_fn_v =     29)
 
      parameter (ix_wk_tl_pr_ifs =        50)
      parameter (ix_wk_tl_du_ifs =        ix_wk_tl_pr_ifs + 1)
      parameter (ix_wk_xp_tau =           ix_wk_tl_du_ifs + 1)
      parameter (ix_tl_pr_ifs =           ix_wk_xp_tau + 1)
      parameter (ix_tl_du_ifs =           ix_tl_pr_ifs + 1)
      parameter (ix_xp_tau =              ix_tl_du_ifs + 1)
      parameter (ix_tl_bwd_tran_dse_rhs = ix_xp_tau + 1)
      parameter (ix_tl_fwd_tran_dse_rhs = ix_tl_bwd_tran_dse_rhs + 1)
      parameter (ix_sus_eta_srch_co_mu =  ix_tl_fwd_tran_dse_rhs + 1)
      parameter (ix_tl_dse_tbu_r =        ix_sus_eta_srch_co_mu + 1)
 
      parameter (ix_usr_tl_crsh_abs_pv_v = 60)
      parameter (ix_usr_tl_crsh_rlv_pv_v = ix_usr_tl_crsh_abs_pv_v + 1)
      parameter (ix_tl_iz_bs_tran_er =     ix_usr_tl_crsh_rlv_pv_v + 1)
      parameter (ix_tl_mx_iz_pr_act =
     &     ix_tl_iz_bs_tran_er + 1)
 
      parameter (ix_tl_du_act_er =        70)
      parameter (ix_tl_pv_er =            ix_tl_du_act_er + 1)
      parameter (ix_tl_ed_wt_er =         ix_tl_pv_er + 1)
      parameter (ix_tl_tran_er =          ix_tl_ed_wt_er + 1)
      parameter (ix_tl_cz_r_growth =      ix_tl_tran_er + 1)
      parameter (ix_fac_long_prd =        ix_tl_cz_r_growth + 1)
      parameter (ix_fac_short_prd =       ix_fac_long_prd + 1)
 
      parameter (ix_bwd_tran_ze =         80)
      parameter (ix_pc_ze =               ix_bwd_tran_ze + 1)
      parameter (ix_fwd_tran_ze =         ix_pc_ze + 1)
      parameter (ix_u_ed_wt_ze =          ix_fwd_tran_ze + 4)
      parameter (ix_pk_pv_c_ze =          ix_u_ed_wt_ze + 1)
      parameter (ix_scl_v_ze =            ix_pk_pv_c_ze + 1) ! 88
 
      parameter (ix_av_eta_dse =          90)
      parameter (ix_av_ftran_sol_dse =    ix_av_eta_dse + 1)
      parameter (ix_av_btran_sol_dse =    ix_av_ftran_sol_dse + 1)
      parameter (ix_av_tbu_r_dse =        ix_av_btran_sol_dse + 1)
      parameter (ix_tl_dvx_wt =           ix_av_tbu_r_dse + 1)
      parameter (ix_nw_dvx_fwk_fq =       ix_tl_dvx_wt + 1)
 
      parameter (ix_inv_pv_tl =           100)
      parameter (ix_inv_wr_eta_tl =       ix_inv_pv_tl + 1)
      parameter (ix_inv_c_rlv_tl =        ix_inv_wr_eta_tl + 1)
      parameter (ix_inv_unit_v_tl =       ix_inv_c_rlv_tl + 1)
      parameter (ix_inv_l_pv_rsdu_er_tl = ix_inv_unit_v_tl + 1)
 
      parameter (ix_usr_cz_r_pv_tl =      110)
      parameter (ix_wk_cz_r_pv_tl =       ix_usr_cz_r_pv_tl + 1)
      parameter (ix_cz_r_pv_tl =          ix_wk_cz_r_pv_tl + 1)
 
      parameter (ix_tl_bc_pr_act_it_rfn = 120)
      parameter (ix_tl_reset_pi_it_rfn =  ix_tl_bc_pr_act_it_rfn + 1)
      parameter (ix_tl_pv_c_it_rfn =      ix_tl_reset_pi_it_rfn + 1)
      parameter (ix_tl_full_pi_it_rfn =   ix_tl_pv_c_it_rfn + 1)
      parameter (ix_tl_u_pi_it_rfn =      ix_tl_full_pi_it_rfn + 1)
      parameter (ix_bc_pr_act_it_rfn_tran_ze =
     &     ix_tl_u_pi_it_rfn + 1)
      parameter (ix_reset_pi_it_rfn_tran_ze =
     &     ix_bc_pr_act_it_rfn_tran_ze + 1)
      parameter (ix_pv_c_it_rfn_tran_ze =
     &     ix_reset_pi_it_rfn_tran_ze + 1)
      parameter (ix_full_pi_it_rfn_tran_ze =
     &     ix_pv_c_it_rfn_tran_ze + 1)
      parameter (ix_u_pi_it_rfn_tran_ze =
     &     ix_full_pi_it_rfn_tran_ze + 1)
 
      parameter (ix_rl_ml_nm =            130)
 
      parameter (ix_ob_fn_cs =            140)
      parameter (ix_su_non_bc_pr_ifs =    ix_ob_fn_cs + 1)
 
      parameter (n_ems_rl_ct_vr =         150)
      parameter (n_osl_rl_ct_vr =          47)
