CM
      include 'RLCTVRIX.INC'
      double precision usr_tl_pr_ifs, usr_tl_du_ifs
      double precision mx_mn, pr_wt
      double precision bb_ub
      double precision ob_fn_v
      double precision su_pr_ifs, su_du_ifs
      double precision mps_ze
      double precision dga_scl
      double precision bst_i_fs_ob_fn_v
      double precision i_ifs_wt
      double precision tl_i_fs_ob_fn_v_imp
      double precision i_fs_ob_fn_v_tgt
      double precision tl_i_ifs
      double precision bst_ob_fn_v
      double precision bst_est_ob_fn_v
      double precision wk_tl_pr_ifs, wk_tl_du_ifs, wk_xp_tau
      double precision tl_pr_ifs, tl_du_ifs, xp_tau
      double precision tl_bwd_tran_dse_rhs
      double precision tl_fwd_tran_dse_rhs
      double precision sus_eta_srch_co_mu
      double precision tl_dse_tbu_r
 
      double precision usr_tl_crsh_abs_pv_v
      double precision usr_tl_crsh_rlv_pv_v
      double precision tl_iz_bs_tran_er
      double precision tl_mx_iz_pr_act
 
      double precision tl_du_act_er, tl_pv_er
      double precision tl_ed_wt_er, tl_tran_er
      double precision tl_cz_r_growth
      double precision fac_long_prd, fac_short_prd
 
      double precision bwd_tran_ze
      double precision pc_ze
      double precision fwd_tran_ze
      double precision u_ed_wt_ze
      double precision pk_pv_c_ze
      double precision scl_v_ze
 
      double precision av_eta_dse
      double precision av_ftran_sol_dse
      double precision av_btran_sol_dse
      double precision av_tbu_r_dse
      double precision tl_dvx_wt
      double precision nw_dvx_fwk_fq
 
      double precision inv_pv_tl
      double precision inv_wr_eta_tl
      double precision inv_c_rlv_tl
      double precision inv_unit_v_tl
      double precision inv_l_pv_rsdu_er_tl
 
      double precision usr_cz_r_pv_tl
      double precision wk_cz_r_pv_tl
      double precision cz_r_pv_tl
 
      double precision tl_bc_pr_act_it_rfn
      double precision tl_reset_pi_it_rfn
      double precision tl_pv_c_it_rfn
      double precision tl_full_pi_it_rfn
      double precision tl_u_pi_it_rfn
      double precision bc_pr_act_it_rfn_tran_ze
      double precision reset_pi_it_rfn_tran_ze
      double precision pv_c_it_rfn_tran_ze
      double precision full_pi_it_rfn_tran_ze
      double precision u_pi_it_rfn_tran_ze
 
      double precision ob_fn_cs
      double precision su_non_bc_pr_ifs
 
      double precision rl_ml_nm
 
      double precision rl_ct_vr(n_ems_rl_ct_vr)
      double precision rl_ct_vr_lb(n_ems_rl_ct_vr)
      double precision rl_ct_vr_ub(n_ems_rl_ct_vr)
      double precision rl_ct_vr_df(n_ems_rl_ct_vr)
      logical rl_ct_vr_lg(n_ems_rl_ct_vr)
      common/ems_com_rl_ct_vr/rl_ct_vr, rl_ct_vr_lb, rl_ct_vr_ub,
     &     rl_ct_vr_df, rl_ct_vr_lg
 
      character*(mx_ct_vr_nm_ln) ems_rl_ct_vr_nm(n_ems_rl_ct_vr)
      character*(mx_ct_vr_nm_ln) osl_rl_ct_vr_nm(n_osl_rl_ct_vr)
      common/ems_com_rl_ct_vr_nm/ems_rl_ct_vr_nm, osl_rl_ct_vr_nm
 
      equivalence (usr_tl_pr_ifs,    rl_ct_vr(ix_usr_tl_pr_ifs))
      equivalence (usr_tl_du_ifs,    rl_ct_vr(ix_usr_tl_du_ifs))
      equivalence (mx_mn,            rl_ct_vr(ix_mx_mn))
      equivalence (pr_wt,            rl_ct_vr(ix_pr_wt))
      equivalence (bb_ub,            rl_ct_vr(ix_bb_ub))
      equivalence (ob_fn_v,          rl_ct_vr(ix_ob_fn_v))
      equivalence (su_pr_ifs,        rl_ct_vr(ix_su_pr_ifs))
      equivalence (su_du_ifs,        rl_ct_vr(ix_su_du_ifs))
      equivalence (mps_ze,           rl_ct_vr(ix_mps_ze))
      equivalence (dga_scl,          rl_ct_vr(ix_dga_scl))
      equivalence (bst_i_fs_ob_fn_v, rl_ct_vr(ix_bst_i_fs_ob_fn_v))
      equivalence (i_ifs_wt,         rl_ct_vr(ix_i_ifs_wt))
      equivalence (tl_i_fs_ob_fn_v_imp,
     &     rl_ct_vr(ix_tl_i_fs_ob_fn_v_imp))
      equivalence (i_fs_ob_fn_v_tgt, rl_ct_vr(ix_i_fs_ob_fn_v_tgt))
      equivalence (tl_i_ifs,         rl_ct_vr(ix_tl_i_ifs))
      equivalence (bst_ob_fn_v,      rl_ct_vr(ix_bst_ob_fn_v))
      equivalence (bst_est_ob_fn_v,  rl_ct_vr(ix_bst_est_ob_fn_v))
      equivalence (wk_tl_pr_ifs,     rl_ct_vr(ix_wk_tl_pr_ifs))
      equivalence (wk_tl_du_ifs,     rl_ct_vr(ix_wk_tl_du_ifs))
      equivalence (wk_xp_tau,        rl_ct_vr(ix_wk_xp_tau))
      equivalence (tl_pr_ifs,        rl_ct_vr(ix_tl_pr_ifs))
      equivalence (tl_du_ifs,        rl_ct_vr(ix_tl_du_ifs))
      equivalence (xp_tau,           rl_ct_vr(ix_xp_tau))
      equivalence (tl_bwd_tran_dse_rhs,
     &     rl_ct_vr(ix_tl_bwd_tran_dse_rhs))
      equivalence (tl_fwd_tran_dse_rhs,
     &     rl_ct_vr(ix_tl_fwd_tran_dse_rhs))
      equivalence (sus_eta_srch_co_mu, rl_ct_vr(ix_sus_eta_srch_co_mu))
      equivalence (tl_dse_tbu_r,     rl_ct_vr(ix_tl_dse_tbu_r))
 
      equivalence (usr_tl_crsh_abs_pv_v,
     &     rl_ct_vr(ix_usr_tl_crsh_abs_pv_v))
      equivalence (usr_tl_crsh_rlv_pv_v,
     &     rl_ct_vr(ix_usr_tl_crsh_rlv_pv_v))
      equivalence (tl_iz_bs_tran_er,
     &     rl_ct_vr(ix_tl_iz_bs_tran_er))
      equivalence (tl_mx_iz_pr_act,
     &     rl_ct_vr(ix_tl_mx_iz_pr_act))
 
      equivalence (tl_du_act_er,     rl_ct_vr(ix_tl_du_act_er))
      equivalence (tl_pv_er,         rl_ct_vr(ix_tl_pv_er))
      equivalence (tl_ed_wt_er,      rl_ct_vr(ix_tl_ed_wt_er))
      equivalence (tl_tran_er,       rl_ct_vr(ix_tl_tran_er))
      equivalence (tl_cz_r_growth,   rl_ct_vr(ix_tl_cz_r_growth))
      equivalence (fac_long_prd,     rl_ct_vr(ix_fac_long_prd))
      equivalence (fac_short_prd,    rl_ct_vr(ix_fac_short_prd))
 
      equivalence (bwd_tran_ze,         rl_ct_vr(ix_bwd_tran_ze))
      equivalence (pc_ze,            rl_ct_vr(ix_pc_ze))
      equivalence (fwd_tran_ze,         rl_ct_vr(ix_fwd_tran_ze))
      equivalence (u_ed_wt_ze,       rl_ct_vr(ix_u_ed_wt_ze))
      equivalence (pk_pv_c_ze,       rl_ct_vr(ix_pk_pv_c_ze))
      equivalence (scl_v_ze,         rl_ct_vr(ix_scl_v_ze))
 
      equivalence (av_eta_dse,       rl_ct_vr(ix_av_eta_dse))
      equivalence (av_ftran_sol_dse, rl_ct_vr(ix_av_ftran_sol_dse))
      equivalence (av_btran_sol_dse, rl_ct_vr(ix_av_btran_sol_dse))
      equivalence (av_tbu_r_dse,     rl_ct_vr(ix_av_tbu_r_dse))
      equivalence (tl_dvx_wt,        rl_ct_vr(ix_tl_dvx_wt))
      equivalence (nw_dvx_fwk_fq,    rl_ct_vr(ix_nw_dvx_fwk_fq))
 
      equivalence (inv_pv_tl,        rl_ct_vr(ix_inv_pv_tl))
      equivalence (inv_wr_eta_tl,    rl_ct_vr(ix_inv_wr_eta_tl))
      equivalence (inv_c_rlv_tl,     rl_ct_vr(ix_inv_c_rlv_tl))
      equivalence (inv_unit_v_tl,    rl_ct_vr(ix_inv_unit_v_tl))
      equivalence (inv_l_pv_rsdu_er_tl,
     &     rl_ct_vr(ix_inv_l_pv_rsdu_er_tl))
 
      equivalence (usr_cz_r_pv_tl,   rl_ct_vr(ix_usr_cz_r_pv_tl))
      equivalence (wk_cz_r_pv_tl,    rl_ct_vr(ix_wk_cz_r_pv_tl))
      equivalence (cz_r_pv_tl,       rl_ct_vr(ix_cz_r_pv_tl))
 
      equivalence (tl_bc_pr_act_it_rfn,
     &     rl_ct_vr(ix_tl_bc_pr_act_it_rfn))
      equivalence (tl_reset_pi_it_rfn, rl_ct_vr(ix_tl_reset_pi_it_rfn))
      equivalence (tl_pv_c_it_rfn,     rl_ct_vr(ix_tl_pv_c_it_rfn))
      equivalence (tl_full_pi_it_rfn,  rl_ct_vr(ix_tl_full_pi_it_rfn))
      equivalence (tl_u_pi_it_rfn,     rl_ct_vr(ix_tl_u_pi_it_rfn))
      equivalence (bc_pr_act_it_rfn_tran_ze,
     &     rl_ct_vr(ix_bc_pr_act_it_rfn_tran_ze))
      equivalence (reset_pi_it_rfn_tran_ze,
     &     rl_ct_vr(ix_reset_pi_it_rfn_tran_ze))
      equivalence (pv_c_it_rfn_tran_ze,
     &     rl_ct_vr(ix_pv_c_it_rfn_tran_ze))
      equivalence (full_pi_it_rfn_tran_ze,
     &     rl_ct_vr(ix_full_pi_it_rfn_tran_ze))
      equivalence (u_pi_it_rfn_tran_ze,
     &     rl_ct_vr(ix_u_pi_it_rfn_tran_ze))
 
      equivalence (ob_fn_cs,         rl_ct_vr(ix_ob_fn_cs))
      equivalence (su_non_bc_pr_ifs, rl_ct_vr(ix_su_non_bc_pr_ifs))
 
      equivalence (rl_ml_nm,         rl_ct_vr(ix_rl_ml_nm))
