CM
      include 'ICTVRIX.INC'
 
      integer msg_fq
      integer ems_wr_cn
      integer usr_mx_n_u
      integer n_si_it
      integer mx_n_si_it
      integer rsmi_msg_msk
      integer mx_n_r
      integer mx_n_c
      integer ml_nm_n_ch
      integer dvx_mode
      integer it_usr_xit_fq
      integer n_el_in_aux_blk
      integer n_aux_blk
      integer n_pg_li
      integer n_r
      integer n_c
      integer n_pr_ifs
      integer n_du_ifs
      integer wr_ml_da_msk
      integer wr_ml_st_msk
      integer mx_n_el_in_aux_blk
      integer wr_li_ln
      integer mx_nd_n
      integer i_lp_msg_msk
      integer mx_n_nd
      integer mx_n_i_fs_sol
      integer n_i_fs_sol
      integer n_i_vr
      integer n_sos_se
      integer n_i_vr_no_i
      integer prob_st
      integer wr_rg_da_msk
      integer mx_n_i_vr
      integer mx_n_sos_se
      integer i_alg
      integer mx_n_i_da_en
      integer crsh_msk
      integer crsh_tl_dl_pri
      integer crsh_fn_dn
      integer ems_er_cn
 
      integer ts_parsmi
 
      integer ems_fi_wr_msk
      integer scl_mode
      integer scl_bd
      integer mx_n_a_el
      integer n_a_el
      integer ml_blk_st_msk
      integer ml_da_st_msk
      integer ml_da_no_cg_msk
      integer du_sol_mode
      integer ml_nm_n_rl
      integer sslv_en_msk
      integer sslv_xit_msk
      integer n_bs
      integer n_reset
      integer rq_reset
      integer u_pc
      integer r_pc
      integer eta_fi_mode_msk
      integer eta_fi_da_st_msk
      integer pc_alg
      integer n_dvx_it
      integer mn_n_dvx_it
      integer n_dvx_fwk
      integer r_du_act_sgn
      integer inv_alg_msk
      integer inv_msg_msk
      integer inv_log_msk
      integer inv_mx_n_eta
      integer inv_mx_n_eta_el
      integer n_inv_sing
      integer u_bs_cg
      integer rq_inv
      integer sto_ftran_ix_mode
      integer sto_btran_ix_mode
      integer sto_tbu_r_ix_mode
      integer sto_ftran_ix
      integer sto_btran_ix
      integer sto_tbu_r_ix
      integer tbu_r_loop_mode
      integer bwd_tran_dse_rhs_n_r
      integer fwd_tran_dse_rhs_n_r
      integer dse_tbu_r_n_c
      integer usr_cz_c_msk
      integer usr_cz_r_msk
      integer l1_cz_r_mx_n_cdd
      integer n_pc_vr
      integer n_pc_el
      integer n_non_bc_pr_ifs
      integer rq_re_pc
      integer cz_c_msk
      integer cz_r_msk
      integer u_bs_mode
      integer sto_u_eta_v_mode
      integer u_bs
      integer sto_u_eta_v
      integer sto_u_eta_se_ty
      integer mx_n_u
      integer n_u
      integer lo_sto_mode
      integer asm_msk
      integer ck_msk
      integer it_o_sw_fm_wr_df
      integer it_o_sw_fm_ck_df
      integer wr_lp_da
      integer wr_bs_fq
      integer g_tt_da
      integer g_it_tt_fq
      integer g_it_tt_ivl
      integer mx_n_bc_pr_act_rfn_it
      integer mx_n_reset_pi_rfn_it
      integer mx_n_pv_c_rfn_it
      integer mx_n_full_pi_rfn_it
      integer mx_n_u_pi_rfn_it
      integer eta_fi_n_grp
      integer eta_fi_n_inv_grp
      integer eta_fi_n_se
      integer eta_fi_n_inv_se
      integer eta_fi_n_eta
      integer eta_fi_n_inv_eta
      integer eta_fi_n_v
      integer eta_fi_n_inv_v
      integer eta_fi_n_ix
      integer eta_fi_n_inv_ix
      integer mx_n_pwl_vr
      integer mx_n_pwl_vr_da_en
      integer repl_non_std_vr_mode_msk
      integer repl_non_std_vr_st_msk
      integer n_bp_vr
      integer n_pwl_vr
      integer n_pwl_vr_da_en
      integer cu_is_n
      integer cu_ml_n
      integer ml_blk_mv_k
      integer ds_n_en_m1
      integer is_n_en_m1
      integer ns_n_en_m1
      integer sv_ml_ct_vr
      integer n_lo_c_eta
      integer n_lo_r_eta
      integer n_lo_r_eta_el
      integer n_up_r_eta
      integer n_up_r_eta_el
      integer n_sus_eta_srch_op_cf
      integer sus_eta_srch_al_t_ls_ix_lm
      integer sus_eta_srch_ls_t_buk_ix_lm
      integer sus_eta_srch_dbl_n_buk_ix_lm
 
      integer i_ct_vr(n_ems_i_ct_vr)
      integer i_ct_vr_lb(n_ems_i_ct_vr)
      integer i_ct_vr_ub(n_ems_i_ct_vr)
      integer i_ct_vr_df(n_ems_i_ct_vr)
      logical i_ct_vr_lg(n_ems_i_ct_vr)
      common/ems_com_i_ct_vr/i_ct_vr, i_ct_vr_lb, i_ct_vr_ub,
     &     i_ct_vr_df, i_ct_vr_lg
 
      character*(mx_ct_vr_nm_ln) ems_i_ct_vr_nm(n_ems_i_ct_vr)
      character*(mx_ct_vr_nm_ln) osl_i_ct_vr_nm(n_osl_i_ct_vr)
      common/ems_com_i_ct_vr_nm/ems_i_ct_vr_nm, osl_i_ct_vr_nm
 
      equivalence (msg_fq, i_ct_vr(ix_msg_fq))
      equivalence (ems_wr_cn, i_ct_vr(ix_ems_wr_cn))
      equivalence (usr_mx_n_u, i_ct_vr(ix_usr_mx_n_u))
      equivalence (n_si_it, i_ct_vr(ix_n_si_it))
      equivalence (mx_n_si_it, i_ct_vr(ix_mx_n_si_it))
      equivalence (rsmi_msg_msk, i_ct_vr(ix_rsmi_msg_msk))
      equivalence (mx_n_r, i_ct_vr(ix_mx_n_r))
      equivalence (mx_n_c, i_ct_vr(ix_mx_n_c))
      equivalence (ml_nm_n_ch, i_ct_vr(ix_ml_nm_n_ch))
      equivalence (dvx_mode, i_ct_vr(ix_dvx_mode))
      equivalence (it_usr_xit_fq, i_ct_vr(ix_it_usr_xit_fq))
      equivalence (n_el_in_aux_blk, i_ct_vr(ix_n_el_in_aux_blk))
      equivalence (n_aux_blk, i_ct_vr(ix_n_aux_blk))
      equivalence (n_pg_li, i_ct_vr(ix_n_pg_li))
      equivalence (n_r, i_ct_vr(ix_n_r))
      equivalence (n_c, i_ct_vr(ix_n_c))
      equivalence (n_pr_ifs, i_ct_vr(ix_n_pr_ifs))
      equivalence (n_du_ifs, i_ct_vr(ix_n_du_ifs))
      equivalence (wr_ml_da_msk, i_ct_vr(ix_wr_ml_da_msk))
      equivalence (wr_ml_st_msk, i_ct_vr(ix_wr_ml_st_msk))
      equivalence (mx_n_el_in_aux_blk, i_ct_vr(ix_mx_n_el_in_aux_blk))
      equivalence (wr_li_ln, i_ct_vr(ix_wr_li_ln))
      equivalence (mx_nd_n, i_ct_vr(ix_mx_nd_n))
      equivalence (i_lp_msg_msk, i_ct_vr(ix_i_lp_msg_msk))
      equivalence (mx_n_nd, i_ct_vr(ix_mx_n_nd))
      equivalence (mx_n_i_fs_sol, i_ct_vr(ix_mx_n_i_fs_sol))
      equivalence (n_i_fs_sol, i_ct_vr(ix_n_i_fs_sol))
      equivalence (n_i_vr, i_ct_vr(ix_n_i_vr))
      equivalence (n_sos_se, i_ct_vr(ix_n_sos_se))
      equivalence (n_i_vr_no_i, i_ct_vr(ix_n_i_vr_no_i))
      equivalence (prob_st, i_ct_vr(ix_prob_st))
      equivalence (wr_rg_da_msk, i_ct_vr(ix_wr_rg_da_msk))
      equivalence (mx_n_i_vr, i_ct_vr(ix_mx_n_i_vr))
      equivalence (mx_n_sos_se, i_ct_vr(ix_mx_n_sos_se))
      equivalence (i_alg, i_ct_vr(ix_i_alg))
      equivalence (mx_n_i_da_en, i_ct_vr(ix_mx_n_i_da_en))
 
      equivalence (crsh_msk, i_ct_vr(ix_crsh_msk))
      equivalence (crsh_tl_dl_pri, i_ct_vr(ix_crsh_tl_dl_pri))
      equivalence (crsh_fn_dn, i_ct_vr(ix_crsh_fn_dn))
 
      equivalence (ems_er_cn, i_ct_vr(ix_ems_er_cn))
      equivalence (ts_parsmi, i_ct_vr(ix_ts_parsmi))
 
      equivalence (ems_fi_wr_msk, i_ct_vr(ix_ems_fi_wr_msk))
      equivalence (scl_mode, i_ct_vr(ix_scl_mode))
      equivalence (scl_bd, i_ct_vr(ix_scl_bd))
      equivalence (mx_n_a_el, i_ct_vr(ix_mx_n_a_el))
      equivalence (n_a_el, i_ct_vr(ix_n_a_el))
      equivalence (ml_blk_st_msk, i_ct_vr(ix_ml_blk_st_msk))
      equivalence (ml_da_st_msk, i_ct_vr(ix_ml_da_st_msk))
      equivalence (ml_da_no_cg_msk, i_ct_vr(ix_ml_da_no_cg_msk))
      equivalence (du_sol_mode, i_ct_vr(ix_du_sol_mode))
      equivalence (ml_nm_n_rl, i_ct_vr(ix_ml_nm_n_rl))
      equivalence (sslv_en_msk, i_ct_vr(ix_sslv_en_msk))
      equivalence (sslv_xit_msk, i_ct_vr(ix_sslv_xit_msk))
      equivalence (n_bs, i_ct_vr(ix_n_bs))
      equivalence (n_reset, i_ct_vr(ix_n_reset))
      equivalence (rq_reset, i_ct_vr(ix_rq_reset))
      equivalence (u_pc, i_ct_vr(ix_u_pc))
      equivalence (r_pc, i_ct_vr(ix_r_pc))
      equivalence (eta_fi_mode_msk, i_ct_vr(ix_eta_fi_mode_msk))
      equivalence (eta_fi_da_st_msk, i_ct_vr(ix_eta_fi_da_st_msk))
      equivalence (pc_alg, i_ct_vr(ix_pc_alg))
      equivalence (n_dvx_it, i_ct_vr(ix_n_dvx_it))
      equivalence (mn_n_dvx_it, i_ct_vr(ix_mn_n_dvx_it))
      equivalence (n_dvx_fwk, i_ct_vr(ix_n_dvx_fwk))
      equivalence (r_du_act_sgn, i_ct_vr(ix_r_du_act_sgn))
      equivalence (inv_alg_msk, i_ct_vr(ix_inv_alg_msk))
      equivalence (inv_msg_msk, i_ct_vr(ix_inv_msg_msk))
      equivalence (inv_log_msk, i_ct_vr(ix_inv_log_msk))
      equivalence (inv_mx_n_eta, i_ct_vr(ix_inv_mx_n_eta))
      equivalence (inv_mx_n_eta_el, i_ct_vr(ix_inv_mx_n_eta_el))
      equivalence (n_inv_sing, i_ct_vr(ix_n_inv_sing))
      equivalence (u_bs_cg, i_ct_vr(ix_u_bs_cg))
      equivalence (rq_inv, i_ct_vr(ix_rq_inv))
      equivalence (sto_ftran_ix_mode, i_ct_vr(ix_sto_ftran_ix_mode))
      equivalence (sto_btran_ix_mode, i_ct_vr(ix_sto_btran_ix_mode))
      equivalence (sto_tbu_r_ix_mode, i_ct_vr(ix_sto_tbu_r_ix_mode))
      equivalence (sto_ftran_ix, i_ct_vr(ix_sto_ftran_ix))
      equivalence (sto_btran_ix, i_ct_vr(ix_sto_btran_ix))
      equivalence (sto_tbu_r_ix, i_ct_vr(ix_sto_tbu_r_ix))
      equivalence (tbu_r_loop_mode, i_ct_vr(ix_tbu_r_loop_mode))
      equivalence (bwd_tran_dse_rhs_n_r,
     &     i_ct_vr(ix_bwd_tran_dse_rhs_n_r))
      equivalence (fwd_tran_dse_rhs_n_r,
     &     i_ct_vr(ix_fwd_tran_dse_rhs_n_r))
      equivalence (dse_tbu_r_n_c, i_ct_vr(ix_dse_tbu_r_n_c))
      equivalence (usr_cz_c_msk, i_ct_vr(ix_usr_cz_c_msk))
      equivalence (usr_cz_r_msk, i_ct_vr(ix_usr_cz_r_msk))
      equivalence (l1_cz_r_mx_n_cdd, i_ct_vr(ix_l1_cz_r_mx_n_cdd))
      equivalence (n_pc_vr, i_ct_vr(ix_n_pc_vr))
      equivalence (n_pc_el, i_ct_vr(ix_n_pc_el))
      equivalence (n_non_bc_pr_ifs, i_ct_vr(ix_n_non_bc_pr_ifs))
      equivalence (rq_re_pc, i_ct_vr(ix_rq_re_pc))
      equivalence (cz_c_msk, i_ct_vr(ix_cz_c_msk))
      equivalence (cz_r_msk, i_ct_vr(ix_cz_r_msk))
      equivalence (u_bs_mode, i_ct_vr(ix_u_bs_mode))
      equivalence (sto_u_eta_v_mode, i_ct_vr(ix_sto_u_eta_v_mode))
      equivalence (u_bs, i_ct_vr(ix_u_bs))
      equivalence (sto_u_eta_v, i_ct_vr(ix_sto_u_eta_v))
      equivalence (sto_u_eta_se_ty, i_ct_vr(ix_sto_u_eta_se_ty))
      equivalence (mx_n_u, i_ct_vr(ix_mx_n_u))
      equivalence (n_u, i_ct_vr(ix_n_u))
      equivalence (lo_sto_mode, i_ct_vr(ix_lo_sto_mode))
      equivalence (asm_msk, i_ct_vr(ix_asm_msk))
      equivalence (ck_msk, i_ct_vr(ix_ck_msk))
      equivalence (it_o_sw_fm_wr_df, i_ct_vr(ix_it_o_sw_fm_wr_df))
      equivalence (it_o_sw_fm_ck_df, i_ct_vr(ix_it_o_sw_fm_ck_df))
      equivalence (wr_lp_da, i_ct_vr(ix_wr_lp_da))
      equivalence (wr_bs_fq, i_ct_vr(ix_wr_bs_fq))
      equivalence (g_tt_da, i_ct_vr(ix_g_tt_da))
      equivalence (g_it_tt_fq, i_ct_vr(ix_g_it_tt_fq))
      equivalence (g_it_tt_ivl, i_ct_vr(ix_g_it_tt_ivl))
      equivalence (mx_n_bc_pr_act_rfn_it,
     &     i_ct_vr(ix_mx_n_bc_pr_act_rfn_it))
      equivalence (mx_n_reset_pi_rfn_it,
     &     i_ct_vr(ix_mx_n_reset_pi_rfn_it))
      equivalence (mx_n_pv_c_rfn_it,
     &     i_ct_vr(ix_mx_n_pv_c_rfn_it))
      equivalence (mx_n_full_pi_rfn_it,
     &     i_ct_vr(ix_mx_n_full_pi_rfn_it))
      equivalence (mx_n_u_pi_rfn_it,
     &     i_ct_vr(ix_mx_n_u_pi_rfn_it))
      equivalence (eta_fi_n_grp,     i_ct_vr(ix_eta_fi_n_grp))
      equivalence (eta_fi_n_inv_grp, i_ct_vr(ix_eta_fi_n_inv_grp))
      equivalence (eta_fi_n_se,      i_ct_vr(ix_eta_fi_n_se))
      equivalence (eta_fi_n_inv_se,  i_ct_vr(ix_eta_fi_n_inv_se))
      equivalence (eta_fi_n_eta,     i_ct_vr(ix_eta_fi_n_eta))
      equivalence (eta_fi_n_inv_eta, i_ct_vr(ix_eta_fi_n_inv_eta))
      equivalence (eta_fi_n_v,       i_ct_vr(ix_eta_fi_n_v))
      equivalence (eta_fi_n_inv_v,   i_ct_vr(ix_eta_fi_n_inv_v))
      equivalence (eta_fi_n_ix,      i_ct_vr(ix_eta_fi_n_ix))
      equivalence (eta_fi_n_inv_ix,  i_ct_vr(ix_eta_fi_n_inv_ix))
      equivalence (mx_n_pwl_vr,       i_ct_vr(ix_mx_n_pwl_vr))
      equivalence (mx_n_pwl_vr_da_en, i_ct_vr(ix_mx_n_pwl_vr_da_en))
      equivalence (repl_non_std_vr_mode_msk,
     &     i_ct_vr(ix_repl_non_std_vr_mode_msk))
      equivalence (repl_non_std_vr_st_msk,
     &     i_ct_vr(ix_repl_non_std_vr_st_msk))
      equivalence (n_bp_vr,           i_ct_vr(ix_n_bp_vr))
      equivalence (n_pwl_vr,          i_ct_vr(ix_n_pwl_vr))
      equivalence (n_pwl_vr_da_en,    i_ct_vr(ix_n_pwl_vr_da_en))
      equivalence (cu_is_n, i_ct_vr(ix_cu_is_n))
      equivalence (cu_ml_n, i_ct_vr(ix_cu_ml_n))
      equivalence (ml_blk_mv_k, i_ct_vr(ix_ml_blk_mv_k))
      equivalence (ds_n_en_m1, i_ct_vr(ix_ds_n_en_m1))
      equivalence (is_n_en_m1, i_ct_vr(ix_is_n_en_m1))
      equivalence (ns_n_en_m1, i_ct_vr(ix_ns_n_en_m1))
      equivalence (sv_ml_ct_vr, i_ct_vr(ix_sv_ml_ct_vr))
      equivalence (n_lo_c_eta, i_ct_vr(ix_n_lo_c_eta))
      equivalence (n_lo_r_eta, i_ct_vr(ix_n_lo_r_eta))
      equivalence (n_lo_r_eta_el, i_ct_vr(ix_n_lo_r_eta_el))
      equivalence (n_up_r_eta, i_ct_vr(ix_n_up_r_eta))
      equivalence (n_up_r_eta_el, i_ct_vr(ix_n_up_r_eta_el))
      equivalence (n_sus_eta_srch_op_cf,
     &     i_ct_vr(ix_n_sus_eta_srch_op_cf))
      equivalence (sus_eta_srch_al_t_ls_ix_lm,
     &     i_ct_vr(ix_sus_eta_srch_al_t_ls_ix_lm))
      equivalence (sus_eta_srch_ls_t_buk_ix_lm,
     &     i_ct_vr(ix_sus_eta_srch_ls_t_buk_ix_lm))
      equivalence (sus_eta_srch_dbl_n_buk_ix_lm,
     &     i_ct_vr(ix_sus_eta_srch_dbl_n_buk_ix_lm))
