CM
C->>> -------------------------------------------> ems_se_i_ct_vr_rg <<<
c     Set the ranges for the integer control variables.
c
      subroutine ems_se_i_ct_vr_rg
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSFI.INC'
      include 'EMSMEM.INC'
CM      IF (emsol_xa .EQ. 1) THEN
C?      include 'CRASH.INC'
CM      ENDIF
      include 'ICTVR.INC'
 
      call ems_cp_i_a(n_ems_i_ct_vr, 0, i_ct_vr_lb, 0)
      call ems_cp_i_a(n_ems_i_ct_vr, 0, i_ct_vr_ub, 0)
      i_ct_vr_lb(ix_msg_fq) = 1
      i_ct_vr_ub(ix_msg_fq) = i_inf
      i_ct_vr_ub(ix_ems_wr_cn) = 99
      i_ct_vr_ub(ix_usr_mx_n_u) = 999
      i_ct_vr_ub(ix_n_si_it) = i_inf
      i_ct_vr_ub(ix_mx_n_si_it) = i_inf
      i_ct_vr_ub(ix_rsmi_msg_msk) = 31
      i_ct_vr_lb(ix_mx_n_r) = -mx_mx_ml_a_dim
      i_ct_vr_ub(ix_mx_n_r) =  mx_mx_ml_a_dim
      i_ct_vr_lb(ix_mx_n_c) = -mx_mx_ml_a_dim
      i_ct_vr_ub(ix_mx_n_c) =  mx_mx_ml_a_dim
      i_ct_vr_lb(ix_ml_nm_n_ch) = 8
      i_ct_vr_ub(ix_ml_nm_n_ch) = ml_nm_mx_n_ch
      i_ct_vr_ub(ix_dvx_mode) = pc_alg_sed
      i_ct_vr_lb(ix_it_usr_xit_fq) = 1
      i_ct_vr_ub(ix_it_usr_xit_fq) = i_inf
      i_ct_vr_ub(ix_n_el_in_aux_blk) = i_inf
      i_ct_vr_ub(ix_n_aux_blk) = i_inf
      i_ct_vr_lb(ix_n_pg_li) = 10
      i_ct_vr_ub(ix_n_pg_li) = i_inf
      i_ct_vr_ub(ix_n_r) = mx_n_r
      i_ct_vr_ub(ix_n_c) = mx_n_c
      i_ct_vr_ub(ix_n_pr_ifs) = n_r
      i_ct_vr_ub(ix_n_du_ifs) = n_c
      i_ct_vr_ub(ix_wr_ml_da_msk) = 1023
      i_ct_vr_ub(ix_wr_ml_st_msk) = 15 + 16 + 32
      i_ct_vr_lb(ix_mx_n_el_in_aux_blk) = 1
      i_ct_vr_ub(ix_mx_n_el_in_aux_blk) = i_inf
      i_ct_vr_lb(ix_wr_li_ln) = 60
      i_ct_vr_ub(ix_wr_li_ln) = i_inf
      i_ct_vr_ub(ix_mx_nd_n) = i_inf
      i_ct_vr_ub(ix_mx_n_nd) = i_inf
      i_ct_vr_ub(ix_mx_n_i_fs_sol) = i_inf
      i_ct_vr_ub(ix_n_i_fs_sol) = i_inf
      i_ct_vr_ub(ix_n_i_vr) = i_inf
      i_ct_vr_ub(ix_n_sos_se) = i_inf
      i_ct_vr_ub(ix_n_i_vr_no_i) = i_inf
      i_ct_vr_lb(ix_prob_st) = -1
      i_ct_vr_ub(ix_prob_st) =  6
      i_ct_vr_ub(ix_wr_rg_da_msk) = 1023
      i_ct_vr_lb(ix_mx_n_i_vr) = -i_inf
      i_ct_vr_ub(ix_mx_n_i_vr) = i_inf
      i_ct_vr_lb(ix_mx_n_sos_se) = -i_inf
      i_ct_vr_ub(ix_mx_n_sos_se) = i_inf
      i_ct_vr_ub(ix_i_alg) = 3
      i_ct_vr_lb(ix_mx_n_i_da_en) = -i_inf
      i_ct_vr_ub(ix_mx_n_i_da_en) = i_inf
c
CM      IF (emsol_xa .EQ. 1) THEN
C?      i_ct_vr_ub(ix_crsh_msk) = crsh_msk_ze_fs_bt +
C?     &     crsh_msk_ze_pr_act_bt
C?      i_ct_vr_lb(ix_crsh_tl_dl_pri) = -i_inf
C?      i_ct_vr_ub(ix_crsh_tl_dl_pri) = i_inf
C?      i_ct_vr_lb(ix_crsh_fn_dn) = -i_inf
C?      i_ct_vr_ub(ix_crsh_fn_dn) = i_inf
CM      ENDIF
 
      i_ct_vr_lb(ix_ems_er_cn) = -1
      i_ct_vr_ub(ix_ems_er_cn) = 99
      i_ct_vr_ub(ix_ts_parsmi) = i_inf
 
      i_ct_vr_lb(ix_ems_fi_wr_msk) = -1
      i_ct_vr_ub(ix_ems_fi_wr_msk) = 2
      i_ct_vr_ub(ix_scl_mode) = 1
      i_ct_vr_ub(ix_scl_bd) = i_inf
      i_ct_vr_lb(ix_mx_n_a_el) = -i_inf
      i_ct_vr_ub(ix_mx_n_a_el) = i_inf
      i_ct_vr_ub(ix_n_a_el) = i_inf
      i_ct_vr_ub(ix_ml_blk_st_msk) = bt16-1
      i_ct_vr_ub(ix_ml_da_st_msk) = bt16-1
      i_ct_vr_ub(ix_du_sol_mode) = mx_du_sol_mode
      i_ct_vr_ub(ix_ml_nm_n_rl) = ml_nm_mx_n_ch/8
 
      i_ct_vr_ub(ix_sslv_en_msk) = sslv_en_no_bd_cg + sslv_en_no_co_cg
      i_ct_vr_ub(ix_sslv_xit_msk) =
     &     sslv_xit_no_inv +
     &     sslv_xit_no_reset_non_bc_pr_act +
     &     sslv_xit_no_reset_bc_pr_act +
     &     sslv_xit_no_reset_non_bc_du_act
      i_ct_vr_ub(ix_n_bs) = i_inf
      i_ct_vr_ub(ix_n_reset) = i_inf
 
      i_ct_vr_ub(ix_u_pc) = 1
      i_ct_vr_ub(ix_r_pc) = 1
      i_ct_vr_ub(ix_eta_fi_mode_msk) =
     &     eta_fi_fwd_p_poss_bt +
     &     eta_fi_r_eta_poss_bt +
     &     eta_fi_bwd_p_poss_bt +
     &     eta_fi_fwd_p_y_bt +
     &     eta_fi_r_eta_y_bt +
     &     eta_fi_bwd_p_y_bt
      i_ct_vr_ub(ix_eta_fi_da_st_msk) =
     &     eta_fi_da_st_fwd_p + eta_fi_da_st_r_eta + eta_fi_da_st_bwd_p
      i_ct_vr_ub(ix_pc_alg) = 3
      i_ct_vr_ub(ix_n_dvx_it) = i_inf
      i_ct_vr_ub(ix_mn_n_dvx_it) = i_inf
      i_ct_vr_ub(ix_n_dvx_fwk) = i_inf
      i_ct_vr_lb(ix_r_du_act_sgn) = -1
      i_ct_vr_ub(ix_r_du_act_sgn) =  1
 
      i_ct_vr_lb(ix_inv_alg_msk) = inv_alg_tom
      i_ct_vr_ub(ix_inv_alg_msk) = inv_alg_tom + inv_alg_perm +
     &     inv_alg_mwz + inv_alg_sus
      i_ct_vr_ub(ix_inv_msg_msk) = 7
      i_ct_vr_ub(ix_inv_log_msk) = 7
      i_ct_vr_ub(ix_inv_mx_n_eta) = i_inf
      i_ct_vr_ub(ix_inv_mx_n_eta_el) = i_inf
      i_ct_vr_ub(ix_n_inv_sing) = i_inf
      i_ct_vr_ub(ix_u_bs_cg) = 1
      i_ct_vr_ub(ix_rq_inv) = 1
 
      i_ct_vr_lb(ix_sto_ftran_ix_mode) = -1
      i_ct_vr_ub(ix_sto_ftran_ix_mode) =  1
      i_ct_vr_lb(ix_sto_btran_ix_mode) = -1
      i_ct_vr_ub(ix_sto_btran_ix_mode) =  1
      i_ct_vr_lb(ix_sto_tbu_r_ix_mode) = -1
      i_ct_vr_ub(ix_sto_tbu_r_ix_mode) =  1
      i_ct_vr_lb(ix_tbu_r_loop_mode) =   -1
      i_ct_vr_ub(ix_tbu_r_loop_mode) =    1
      i_ct_vr_lb(ix_sto_ftran_ix) = -1
      i_ct_vr_ub(ix_sto_ftran_ix) =  1
      i_ct_vr_lb(ix_sto_btran_ix) = -1
      i_ct_vr_ub(ix_sto_btran_ix) =  1
      i_ct_vr_lb(ix_sto_tbu_r_ix) = -1
      i_ct_vr_ub(ix_sto_tbu_r_ix) =  1
      i_ct_vr_ub(ix_bwd_tran_dse_rhs_n_r) = i_inf
      i_ct_vr_ub(ix_fwd_tran_dse_rhs_n_r) = i_inf
      i_ct_vr_ub(ix_dse_tbu_r_n_c) = i_inf
 
      i_ct_vr_ub(ix_usr_cz_c_msk) = cz_c_bk_bd_bt
      i_ct_vr_ub(ix_usr_cz_r_msk) =
     &     cz_r_l1_bt + cz_r_refine_bt + cz_r_growth_mode
      i_ct_vr_lb(ix_l1_cz_r_mx_n_cdd) = 1
      i_ct_vr_ub(ix_l1_cz_r_mx_n_cdd) = i_inf
      i_ct_vr_ub(ix_n_pc_vr) = i_inf
      i_ct_vr_ub(ix_n_pc_el) = i_inf
      i_ct_vr_ub(ix_n_non_bc_pr_ifs) = i_inf
      i_ct_vr_ub(ix_rq_re_pc) = i_inf
      i_ct_vr_ub(ix_cz_c_msk) = i_ct_vr_ub(ix_usr_cz_c_msk)
      i_ct_vr_ub(ix_cz_r_msk) = i_ct_vr_ub(ix_usr_cz_r_msk)
 
      i_ct_vr_ub(ix_u_bs_mode) = 2
      i_ct_vr_ub(ix_sto_u_eta_v_mode) = 2
      i_ct_vr_ub(ix_u_bs) = 2
      i_ct_vr_ub(ix_sto_u_eta_v) = 1
      i_ct_vr_ub(ix_sto_u_eta_se_ty) = n_eta_se_ty
      i_ct_vr_ub(ix_mx_n_u) = 999
      i_ct_vr_ub(ix_n_u) = i_inf
 
      i_ct_vr_ub(ix_lo_sto_mode) = 1
      i_ct_vr_ub(ix_asm_msk) = 15
      i_ct_vr_ub(ix_ck_msk) = 255
      i_ct_vr_ub(ix_it_o_sw_fm_wr_df) = i_inf
      i_ct_vr_ub(ix_it_o_sw_fm_ck_df) = i_inf
      i_ct_vr_ub(ix_wr_lp_da) = 1023
      i_ct_vr_ub(ix_wr_bs_fq) = i_inf
 
      i_ct_vr_lb(ix_g_tt_da) = -i_inf
      i_ct_vr_ub(ix_g_tt_da) = bt15-1
      i_ct_vr_ub(ix_g_it_tt_fq) = i_inf
      i_ct_vr_ub(ix_g_it_tt_ivl) = i_inf
 
      i_ct_vr_ub(ix_mx_n_bc_pr_act_rfn_it) = i_inf
      i_ct_vr_ub(ix_mx_n_reset_pi_rfn_it) = i_inf
      i_ct_vr_ub(ix_mx_n_pv_c_rfn_it) = i_inf
      i_ct_vr_ub(ix_mx_n_full_pi_rfn_it) = i_inf
      i_ct_vr_ub(ix_mx_n_u_pi_rfn_it) = i_inf
 
      i_ct_vr_ub(ix_eta_fi_n_grp) = eta_fi_mx_n_eta_grp
      i_ct_vr_ub(ix_eta_fi_n_inv_grp) = eta_fi_mx_n_eta_grp
      i_ct_vr_ub(ix_eta_fi_n_se) = i_inf
      i_ct_vr_ub(ix_eta_fi_n_inv_se) = i_inf
      i_ct_vr_ub(ix_eta_fi_n_eta) = i_inf
      i_ct_vr_ub(ix_eta_fi_n_inv_eta) = i_inf
      i_ct_vr_ub(ix_eta_fi_n_v) = i_inf
      i_ct_vr_ub(ix_eta_fi_n_inv_v) = i_inf
      i_ct_vr_ub(ix_eta_fi_n_ix) = i_inf
      i_ct_vr_ub(ix_eta_fi_n_inv_ix) = i_inf
 
      i_ct_vr_lb(ix_mx_n_pwl_vr) = -i_inf
      i_ct_vr_ub(ix_mx_n_pwl_vr) = i_inf
      i_ct_vr_lb(ix_mx_n_pwl_vr_da_en) = -i_inf
      i_ct_vr_ub(ix_mx_n_pwl_vr_da_en) = i_inf
      i_ct_vr_ub(ix_repl_non_std_vr_mode_msk) =
     &     repl_non_std_vr_bp_bt + repl_non_std_vr_pwl_bt
      i_ct_vr_ub(ix_n_bp_vr) = i_inf
      i_ct_vr_ub(ix_n_pwl_vr) = i_inf
      i_ct_vr_ub(ix_n_pwl_vr_da_en) = i_inf
c
      i_ct_vr_ub(ix_cu_is_n) = i_inf
      i_ct_vr_ub(ix_cu_ml_n) = i_inf
      i_ct_vr_ub(ix_ml_blk_mv_k) = i_inf
      i_ct_vr_ub(ix_ds_n_en_m1) = i_inf
      i_ct_vr_ub(ix_is_n_en_m1) = i_inf
      i_ct_vr_ub(ix_sv_ml_ct_vr) = 1
 
      i_ct_vr_ub(ix_n_lo_c_eta) = i_inf
      i_ct_vr_ub(ix_n_lo_r_eta) = i_inf
      i_ct_vr_ub(ix_n_lo_r_eta_el) = i_inf
      i_ct_vr_ub(ix_n_up_r_eta) = i_inf
      i_ct_vr_ub(ix_n_up_r_eta_el) = i_inf
      i_ct_vr_ub(ix_n_sus_eta_srch_op_cf) = i_inf
      i_ct_vr_ub(ix_sus_eta_srch_al_t_ls_ix_lm) = i_inf
      i_ct_vr_ub(ix_sus_eta_srch_ls_t_buk_ix_lm) = i_inf
      i_ct_vr_ub(ix_sus_eta_srch_dbl_n_buk_ix_lm) = i_inf
      return
      end
C->>> -------------------------------------------> ems_iz_i_ct_vr_df <<<
c     Initialise the settable logical and default values for the
c     integer control variables.
c
      subroutine ems_iz_i_ct_vr_df
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
 
      call ems_cp_i_a(n_ems_i_ct_vr, 0, i_ct_vr_df, 0)
      i_ct_vr_df(ix_msg_fq) = 999999
      i_ct_vr_df(ix_ems_wr_cn) = 6
      i_ct_vr_df(ix_usr_mx_n_u) = 25
      i_ct_vr_df(ix_n_si_it) = 0
      i_ct_vr_df(ix_mx_n_si_it) = 999999
      i_ct_vr_df(ix_rsmi_msg_msk) = 16
      i_ct_vr_df(ix_mx_n_r) = 0
      i_ct_vr_df(ix_mx_n_c) = 0
      i_ct_vr_df(ix_ml_nm_n_ch) = 8
      i_ct_vr_df(ix_dvx_mode) = pc_alg_approx_dvx
      i_ct_vr_df(ix_it_usr_xit_fq) = 1
      i_ct_vr_df(ix_n_el_in_aux_blk) = 0
      i_ct_vr_df(ix_n_aux_blk) = 0
      i_ct_vr_df(ix_n_pg_li) = 999999
      i_ct_vr_df(ix_n_r) = 0
      i_ct_vr_df(ix_n_c) = 0
      i_ct_vr_df(ix_n_pr_ifs) = 0
      i_ct_vr_df(ix_n_du_ifs) = 0
      i_ct_vr_df(ix_wr_ml_da_msk) = 3
      i_ct_vr_df(ix_wr_ml_st_msk) = 0
      i_ct_vr_df(ix_mx_n_el_in_aux_blk) = 1000
      i_ct_vr_df(ix_wr_li_ln) = 80
      i_ct_vr_df(ix_mx_n_nd) = 9999999
      i_ct_vr_df(ix_mx_n_i_fs_sol) = 9999999
      i_ct_vr_df(ix_prob_st) = -1
      i_ct_vr_df(ix_wr_rg_da_msk) = 0
      i_ct_vr_df(ix_i_alg) = 1
 
CM      IF (emsol_xa .EQ. 1) THEN
C?      i_ct_vr_df(ix_crsh_msk) = 0
C?      i_ct_vr_df(ix_crsh_tl_dl_pri) = -10
C?      i_ct_vr_df(ix_crsh_fn_dn) = 10
CM      ENDIF
 
CM      IF (emsol_xa .EQ. 1) THEN
C?      i_ct_vr_df(ix_ems_fi_wr_msk) = 0
CM      ELSE
      i_ct_vr_df(ix_ems_fi_wr_msk) = 1
CM      ENDIF
 
      i_ct_vr_df(ix_scl_mode) = 0
      i_ct_vr_df(ix_scl_bd) = 20
      i_ct_vr_df(ix_mx_n_a_el) = -1000
      i_ct_vr_df(ix_mx_n_a_el) = 0
      i_ct_vr_df(ix_ml_nm_n_rl) = 1
 
      i_ct_vr_df(ix_u_pc) = 1
      i_ct_vr_df(ix_r_pc) = 1
      i_ct_vr_df(ix_eta_fi_mode_msk) =
     &     eta_fi_fwd_p_poss_bt +
     &     eta_fi_r_eta_poss_bt +
     &     eta_fi_bwd_p_poss_bt
      i_ct_vr_df(ix_mn_n_dvx_it) = 25
      i_ct_vr_df(ix_r_du_act_sgn) = 1
 
      i_ct_vr_df(ix_inv_alg_msk) = inv_alg_tom
     &     + inv_alg_sus
      i_ct_vr_df(ix_rq_inv) = 1
 
      i_ct_vr_df(ix_sto_ftran_ix_mode) = sto_ix_poss
      i_ct_vr_df(ix_sto_btran_ix_mode) = sto_ix_poss
      i_ct_vr_df(ix_sto_tbu_r_ix_mode) = sto_ix_poss
      i_ct_vr_df(ix_tbu_r_loop_mode) = tbu_r_loop_poss
      i_ct_vr_df(ix_sto_ftran_ix) = i_ct_vr_df(ix_sto_ftran_ix_mode)
      i_ct_vr_df(ix_sto_btran_ix) = i_ct_vr_df(ix_sto_btran_ix_mode)
      i_ct_vr_df(ix_sto_tbu_r_ix) = i_ct_vr_df(ix_sto_tbu_r_ix_mode)
 
      i_ct_vr_df(ix_usr_cz_c_msk) = 0
      i_ct_vr_df(ix_usr_cz_r_msk) = cz_r_refine_bt + cz_r_growth_inv
      i_ct_vr_df(ix_l1_cz_r_mx_n_cdd) = df_l1_cz_r_mx_n_cdd
      i_ct_vr_df(ix_cz_c_msk) = i_ct_vr_df(ix_usr_cz_c_msk)
      i_ct_vr_df(ix_cz_r_msk) = i_ct_vr_df(ix_usr_cz_r_msk)
 
c      i_ct_vr_df(ix_u_bs_mode) = u_bs_pf
      i_ct_vr_df(ix_u_bs_mode) = u_bs_pf_r_cp
      i_ct_vr_df(ix_sto_u_eta_v_mode) = sto_pk_eta_v
      i_ct_vr_df(ix_sto_u_eta_v) = i_ct_vr_df(ix_sto_u_eta_v_mode)
      i_ct_vr_df(ix_u_bs) = u_bs_pf
      i_ct_vr_df(ix_sto_u_eta_se_ty) = pk_c_eta_se_ty
 
      i_ct_vr_df(ix_g_tt_da) = 0
 
      i_ct_vr_df(ix_mx_n_bc_pr_act_rfn_it) = 3
      i_ct_vr_df(ix_mx_n_reset_pi_rfn_it) = 3
 
      i_ct_vr_df(ix_mx_n_pwl_vr_da_en) = 0
c
c     0 => Replace nothing
c     1 => Replace BP
c     2 => Replace PWL
c
c      i_ct_vr_df(ix_repl_non_std_vr_mode_msk) = 0
      i_ct_vr_df(ix_repl_non_std_vr_mode_msk) = repl_non_std_vr_pwl_bt
 
      i_ct_vr_df(ix_cu_is_n) = 1
      i_ct_vr_df(ix_cu_ml_n) = 1
      i_ct_vr_df(ix_sv_ml_ct_vr) = 1
 
      i_ct_vr_df(ix_n_sus_eta_srch_op_cf) = 1
 
      call ems_cp_lg_a(n_ems_i_ct_vr, .false., i_ct_vr_lg, 0)
      i_ct_vr_lg(ix_msg_fq) = .true.
      i_ct_vr_lg(ix_ems_wr_cn) = .true.
      i_ct_vr_lg(ix_usr_mx_n_u) = .true.
      i_ct_vr_lg(ix_mx_n_si_it) = .true.
      i_ct_vr_lg(ix_rsmi_msg_msk) = .true.
      i_ct_vr_lg(ix_mx_n_r) = .true.
      i_ct_vr_lg(ix_mx_n_c) = .true.
      i_ct_vr_lg(ix_ml_nm_n_ch) = .true.
      i_ct_vr_lg(ix_dvx_mode) = .true.
      i_ct_vr_lg(ix_it_usr_xit_fq) = .true.
      i_ct_vr_lg(ix_n_pg_li) = .true.
      i_ct_vr_lg(ix_wr_ml_da_msk) = .true.
      i_ct_vr_lg(ix_wr_ml_st_msk) = .true.
c      i_ct_vr_lg(ix_mx_n_el_in_aux_blk) = .true.
      i_ct_vr_lg(ix_wr_li_ln) = .true.
      i_ct_vr_lg(ix_wr_rg_da_msk) = .true.
      i_ct_vr_lg(ix_mx_n_nd) = .true.
      i_ct_vr_lg(ix_mx_n_i_fs_sol) = .true.
      i_ct_vr_lg(ix_mx_n_i_vr) = .true.
      i_ct_vr_lg(ix_mx_n_sos_se) = .true.
      i_ct_vr_lg(ix_mx_n_i_da_en) = .true.
c
c     Made user-accessible on 21/11/97
c
      i_ct_vr_lg(ix_mx_n_a_el) = .true.
      i_ct_vr_lg(ix_ems_fi_wr_msk) = .true.
      i_ct_vr_lg(ix_repl_non_std_vr_mode_msk) = .true.
 
CM      IF (emsol_dev .EQ. 1) THEN
C?      i_ct_vr_lg(ix_crsh_msk) = .true.
C?      i_ct_vr_lg(ix_crsh_tl_dl_pri) = .true.
C?      i_ct_vr_lg(ix_crsh_fn_dn) = .true.
C?
C?      i_ct_vr_lg(ix_ems_er_cn) = .true.
C?      i_ct_vr_lg(ix_ts_parsmi) = .true.
C?
C?      i_ct_vr_lg(ix_scl_mode) = .true.
C?c      i_ct_vr_lg(ix_scl_bd) = .true.
C?
C?      i_ct_vr_lg(ix_sslv_en_msk) = .true.
C?      i_ct_vr_lg(ix_sslv_xit_msk) = .true.
C?
C?      i_ct_vr_lg(ix_u_pc) = .true.
C?      i_ct_vr_lg(ix_mn_n_dvx_it) = .true.
C?
C?      i_ct_vr_lg(ix_inv_msg_msk) = .true.
C?      i_ct_vr_lg(ix_inv_log_msk) = .true.
C?
C?      i_ct_vr_lg(ix_r_pc) = .true.
C?      i_ct_vr_lg(ix_eta_fi_mode_msk) = .true.
C?      i_ct_vr_lg(ix_inv_alg_msk) = .true.
C?      i_ct_vr_lg(ix_sto_btran_ix_mode) = .true.
C?      i_ct_vr_lg(ix_sto_ftran_ix_mode) = .true.
C?      i_ct_vr_lg(ix_sto_tbu_r_ix_mode) = .true.
C?      i_ct_vr_lg(ix_tbu_r_loop_mode) = .true.
C?      i_ct_vr_lg(ix_usr_cz_r_msk) = .true.
C?      i_ct_vr_lg(ix_mx_n_bc_pr_act_rfn_it) = .true.
C?      i_ct_vr_lg(ix_mx_n_reset_pi_rfn_it) = .true.
C?
C?      i_ct_vr_lg(ix_usr_cz_c_msk) = .true.
C?      i_ct_vr_lg(ix_usr_cz_r_msk) = .true.
C?      i_ct_vr_lg(ix_l1_cz_r_mx_n_cdd) = .true.
C?
C?      i_ct_vr_lg(ix_u_bs_mode) = .true.
C?c      i_ct_vr_lg(ix_sto_u_eta_v_mode) = .true.
C?
C?      i_ct_vr_lg(ix_lo_sto_mode) = .true.
C?      i_ct_vr_lg(ix_asm_msk) = .true.
C?      i_ct_vr_lg(ix_ck_msk) = .true.
C?      i_ct_vr_lg(ix_it_o_sw_fm_wr_df) = .true.
C?      i_ct_vr_lg(ix_it_o_sw_fm_ck_df) = .true.
C?      i_ct_vr_lg(ix_wr_lp_da) = .true.
C?      i_ct_vr_lg(ix_wr_bs_fq) = .true.
C?
C?      i_ct_vr_lg(ix_g_tt_da) = .true.
C?      i_ct_vr_lg(ix_g_it_tt_fq) = .true.
C?      i_ct_vr_lg(ix_g_it_tt_ivl) = .true.
C?
C?      i_ct_vr_lg(ix_mx_n_pv_c_rfn_it) = .true.
C?      i_ct_vr_lg(ix_mx_n_full_pi_rfn_it) = .true.
C?      i_ct_vr_lg(ix_mx_n_u_pi_rfn_it) = .true.
C?
C?      i_ct_vr_lg(ix_mx_n_pwl_vr) = .true.
C?      i_ct_vr_lg(ix_mx_n_pwl_vr_da_en) = .true.
CM      ENDIF
      i_ct_vr_lg(ix_n_sus_eta_srch_op_cf) = .true.
      i_ct_vr_lg(ix_sus_eta_srch_al_t_ls_ix_lm) = .true.
      i_ct_vr_lg(ix_sus_eta_srch_ls_t_buk_ix_lm) = .true.
      i_ct_vr_lg(ix_sus_eta_srch_dbl_n_buk_ix_lm) = .true.
      return
      end
 
C->>> ----------------------------------------------> ems_wr_i_ct_vr <<<
      subroutine ems_wr_i_ct_vr
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer ix
      do 10, ix = 1, n_ems_i_ct_vr
         if (i_ct_vr_lg(ix)) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &           ix, i_ct_vr_lb(ix), i_ct_vr_ub(ix),
     &           i_ct_vr_df(ix), i_ct_vr(ix)
            call ems_msg_wr_li(info_msg_n)
         else
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)
     &           ix, i_ct_vr_lb(ix), i_ct_vr_ub(ix),
     &           i_ct_vr_df(ix), i_ct_vr(ix)
            call ems_msg_wr_li(info_msg_n)
         end if
         if (i_ct_vr(ix) .lt. i_ct_vr_lb(ix) .or.
     &        i_ct_vr(ix) .gt. i_ct_vr_ub(ix)) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)ix
            call ems_msg_wr_li(er_msg_n)
         end if
 10   continue
      return
 9000 format('i_ct_vr ', i3, '     is settable: range [', i9, ', ',
     &     i9, ']; default ', i9, '; value ', i9)
 9010 format('i_ct_vr ', i3, ' is not settable: range [', i9, ', ',
     &     i9, ']; default ', i9, '; value ', i9)
 9100 format('i_ct_vr ', i3, ' violates its range ')
      end
 
C->>> ------------------------------------------> ems_se_rl_ct_vr_rg <<<
c     Set the ranges for the real control variables.
c
      subroutine ems_se_rl_ct_vr_rg
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RLCTVR.INC'
 
      call ems_cp_rl_a(n_ems_rl_ct_vr, zero, rl_ct_vr_lb, 0)
      call ems_cp_rl_a(n_ems_rl_ct_vr, zero, rl_ct_vr_ub, 0)
      rl_ct_vr_lb(ix_usr_tl_pr_ifs) = 1d-12
      rl_ct_vr_ub(ix_usr_tl_pr_ifs) = 1d-1
      rl_ct_vr_lb(ix_usr_tl_du_ifs) = 1d-12
      rl_ct_vr_ub(ix_usr_tl_du_ifs) = 1d-1
      rl_ct_vr_lb(ix_mx_mn) = -1d0
      rl_ct_vr_ub(ix_mx_mn) =  1d0
      rl_ct_vr_lb(ix_pr_wt) = zero
      rl_ct_vr_ub(ix_pr_wt) = 1d+10
      rl_ct_vr_lb(ix_bb_ub) = -1d20
      rl_ct_vr_ub(ix_bb_ub) = undn
      rl_ct_vr_lb(ix_ob_fn_v) = -undn
      rl_ct_vr_ub(ix_ob_fn_v) =  undn
      rl_ct_vr_lb(ix_su_pr_ifs) = -undn
      rl_ct_vr_ub(ix_su_pr_ifs) =  undn
      rl_ct_vr_lb(ix_su_du_ifs) = -undn
      rl_ct_vr_ub(ix_su_du_ifs) =  undn
      rl_ct_vr_lb(ix_mps_ze) = -undn
      rl_ct_vr_ub(ix_mps_ze) =  undn
      rl_ct_vr_ub(ix_dga_scl) = undn
      rl_ct_vr_lb(ix_bst_i_fs_ob_fn_v) = -undn
      rl_ct_vr_ub(ix_bst_i_fs_ob_fn_v) = undn
      rl_ct_vr_ub(ix_i_ifs_wt) = undn
      rl_ct_vr_lb(ix_tl_i_fs_ob_fn_v_imp) = -undn
      rl_ct_vr_ub(ix_tl_i_fs_ob_fn_v_imp) = undn
      rl_ct_vr_lb(ix_i_fs_ob_fn_v_tgt) = -undn
      rl_ct_vr_ub(ix_i_fs_ob_fn_v_tgt) = undn
      rl_ct_vr_lb(ix_tl_i_ifs) = 1d-12
      rl_ct_vr_ub(ix_tl_i_ifs) = 1d-1
      rl_ct_vr_lb(ix_bst_ob_fn_v) = -undn
      rl_ct_vr_ub(ix_bst_ob_fn_v) = undn
      rl_ct_vr_lb(ix_bst_est_ob_fn_v) = -undn
      rl_ct_vr_ub(ix_bst_est_ob_fn_v) = undn
 
      rl_ct_vr_lb(ix_wk_tl_pr_ifs) = 1d-12
      rl_ct_vr_ub(ix_wk_tl_pr_ifs) = 1d-1
c
c     ?? Temporary change to allow experiments
c
      rl_ct_vr_lb(ix_wk_tl_pr_ifs) = 0d0
      rl_ct_vr_ub(ix_wk_tl_pr_ifs) = inf
 
      rl_ct_vr_lb(ix_wk_tl_du_ifs) = 1d-12
      rl_ct_vr_ub(ix_wk_tl_du_ifs) = 1d-1
      rl_ct_vr_ub(ix_wk_xp_tau) = 1d-4
      rl_ct_vr_lb(ix_tl_pr_ifs) = 1d-12
      rl_ct_vr_ub(ix_tl_pr_ifs) = 1d-1
      rl_ct_vr_lb(ix_tl_du_ifs) = 1d-12
      rl_ct_vr_ub(ix_tl_du_ifs) = 1d-1
      rl_ct_vr_ub(ix_xp_tau) = 1d-4
      rl_ct_vr_lb(ix_tl_bwd_tran_dse_rhs) = zero
      rl_ct_vr_ub(ix_tl_bwd_tran_dse_rhs) = one
      rl_ct_vr_lb(ix_tl_fwd_tran_dse_rhs) = zero
      rl_ct_vr_ub(ix_tl_fwd_tran_dse_rhs) = one
      rl_ct_vr_lb(ix_tl_dse_tbu_r) = zero
      rl_ct_vr_ub(ix_tl_dse_tbu_r) = one
 
      rl_ct_vr_lb(ix_usr_tl_crsh_abs_pv_v) = 1d-16
      rl_ct_vr_ub(ix_usr_tl_crsh_abs_pv_v) = inf
      rl_ct_vr_lb(ix_usr_tl_crsh_rlv_pv_v) = 1d-16
      rl_ct_vr_ub(ix_usr_tl_crsh_rlv_pv_v) = one
      rl_ct_vr_lb(ix_tl_iz_bs_tran_er) = zero
      rl_ct_vr_ub(ix_tl_iz_bs_tran_er) = inf
      rl_ct_vr_lb(ix_tl_mx_iz_pr_act) = zero
      rl_ct_vr_ub(ix_tl_mx_iz_pr_act) = inf
 
      rl_ct_vr_ub(ix_tl_du_act_er) = undn
      rl_ct_vr_ub(ix_tl_pv_er) = undn
      rl_ct_vr_ub(ix_tl_ed_wt_er) = undn
      rl_ct_vr_ub(ix_tl_tran_er) = undn
      rl_ct_vr_lb(ix_tl_cz_r_growth) = one
      rl_ct_vr_ub(ix_tl_cz_r_growth) = undn
      rl_ct_vr_ub(ix_fac_long_prd) = one
      rl_ct_vr_ub(ix_fac_short_prd) = one
 
      rl_ct_vr_ub(ix_bwd_tran_ze) = undn
      rl_ct_vr_ub(ix_pc_ze) = undn
      rl_ct_vr_ub(ix_fwd_tran_ze) = undn
      rl_ct_vr_ub(ix_u_ed_wt_ze) = undn
      rl_ct_vr_ub(ix_pk_pv_c_ze) = undn
      rl_ct_vr_ub(ix_scl_v_ze) = undn
 
      rl_ct_vr_lb(ix_av_eta_dse) = -undn
      rl_ct_vr_ub(ix_av_eta_dse) =  undn
      rl_ct_vr_lb(ix_av_ftran_sol_dse) = -undn
      rl_ct_vr_ub(ix_av_ftran_sol_dse) =  undn
      rl_ct_vr_lb(ix_av_btran_sol_dse) = -undn
      rl_ct_vr_ub(ix_av_btran_sol_dse) =  undn
      rl_ct_vr_lb(ix_av_tbu_r_dse) = -undn
      rl_ct_vr_ub(ix_av_tbu_r_dse) =  undn
      rl_ct_vr_ub(ix_tl_dvx_wt) =  undn
      rl_ct_vr_ub(ix_nw_dvx_fwk_fq) = undn
 
      rl_ct_vr_lb(ix_inv_pv_tl) =     1d-16
      rl_ct_vr_ub(ix_inv_pv_tl) =     inf
      rl_ct_vr_lb(ix_inv_wr_eta_tl) = 1d-19
      rl_ct_vr_ub(ix_inv_wr_eta_tl) = inf
      rl_ct_vr_lb(ix_inv_c_rlv_tl) =  1d-6
      rl_ct_vr_ub(ix_inv_c_rlv_tl) =  1d0
      rl_ct_vr_lb(ix_inv_unit_v_tl) = 0d0
      rl_ct_vr_ub(ix_inv_unit_v_tl) = 1d0
      rl_ct_vr_lb(ix_inv_l_pv_rsdu_er_tl) = 0d0
      rl_ct_vr_ub(ix_inv_l_pv_rsdu_er_tl) = 1d0
 
      rl_ct_vr_lb(ix_usr_cz_r_pv_tl) = 0d0
      rl_ct_vr_ub(ix_usr_cz_r_pv_tl) = 1d0
      rl_ct_vr_lb(ix_wk_cz_r_pv_tl) =  rl_ct_vr_lb(ix_usr_cz_r_pv_tl)
      rl_ct_vr_ub(ix_wk_cz_r_pv_tl) =  rl_ct_vr_ub(ix_usr_cz_r_pv_tl)
      rl_ct_vr_lb(ix_cz_r_pv_tl) =     rl_ct_vr_lb(ix_usr_cz_r_pv_tl)
      rl_ct_vr_ub(ix_cz_r_pv_tl) =     rl_ct_vr_ub(ix_usr_cz_r_pv_tl)
 
      rl_ct_vr_ub(ix_tl_bc_pr_act_it_rfn) = undn
      rl_ct_vr_ub(ix_tl_reset_pi_it_rfn) = undn
      rl_ct_vr_ub(ix_tl_pv_c_it_rfn) = undn
      rl_ct_vr_ub(ix_tl_full_pi_it_rfn) = undn
      rl_ct_vr_ub(ix_tl_u_pi_it_rfn) = undn
      rl_ct_vr_ub(ix_bc_pr_act_it_rfn_tran_ze) = undn
      rl_ct_vr_ub(ix_reset_pi_it_rfn_tran_ze) = undn
      rl_ct_vr_ub(ix_pv_c_it_rfn_tran_ze) = undn
      rl_ct_vr_ub(ix_full_pi_it_rfn_tran_ze) = undn
      rl_ct_vr_ub(ix_u_pi_it_rfn_tran_ze) = undn
 
      rl_ct_vr_lb(ix_ob_fn_cs) = -undn
      rl_ct_vr_ub(ix_ob_fn_cs) = undn
      rl_ct_vr_ub(ix_su_non_bc_pr_ifs) = undn
 
      return
      end
 
C->>> ------------------------------------------> ems_iz_rl_ct_vr_df <<<
c     Initialise the settable logical and default values for the
c     real control variables.
c
      subroutine ems_iz_rl_ct_vr_df
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
 
      call ems_cp_rl_a(n_ems_rl_ct_vr, zero, rl_ct_vr_df, 0)
      rl_ct_vr_df(ix_usr_tl_pr_ifs) = 1d-6
      rl_ct_vr_df(ix_usr_tl_du_ifs) = 1d-6
      rl_ct_vr_df(ix_mx_mn) =         one
      rl_ct_vr_df(ix_pr_wt) =         zero
      rl_ct_vr_df(ix_bb_ub) =         inf
      rl_ct_vr_df(ix_dga_scl) =       one
      rl_ct_vr_df(ix_mps_ze) =        1d-12
      rl_ct_vr_df(ix_bst_i_fs_ob_fn_v) = inf
      rl_ct_vr_df(ix_i_ifs_wt) =      one
      rl_ct_vr_df(ix_tl_i_fs_ob_fn_v_imp) = 1d-5
      rl_ct_vr_df(ix_tl_i_ifs) = 1d-6
 
      rl_ct_vr_df(ix_wk_tl_pr_ifs) =   1d-6
      rl_ct_vr_df(ix_wk_tl_du_ifs) =   1d-6
      rl_ct_vr_df(ix_wk_xp_tau) =      0d0
      rl_ct_vr_df(ix_tl_pr_ifs) =      rl_ct_vr_df(ix_wk_tl_pr_ifs)
      rl_ct_vr_df(ix_tl_du_ifs) =      rl_ct_vr_df(ix_wk_tl_du_ifs)
      rl_ct_vr_df(ix_xp_tau) =         rl_ct_vr_df(ix_wk_xp_tau)
      rl_ct_vr_df(ix_tl_bwd_tran_dse_rhs) = 4d-1
      rl_ct_vr_df(ix_tl_fwd_tran_dse_rhs) = 1d-1
      rl_ct_vr_df(ix_tl_dse_tbu_r) =        1d-1
 
      rl_ct_vr_df(ix_usr_tl_crsh_abs_pv_v) =      1d-4
      rl_ct_vr_df(ix_usr_tl_crsh_rlv_pv_v) =      1d-2
      rl_ct_vr_df(ix_tl_iz_bs_tran_er) =          1d-10
      rl_ct_vr_df(ix_tl_mx_iz_pr_act) = 1d+10
 
      rl_ct_vr_df(ix_tl_du_act_er) =     1d-6
      rl_ct_vr_df(ix_tl_pv_er) =         1d-6
      rl_ct_vr_df(ix_tl_ed_wt_er) =      1d-2
      rl_ct_vr_df(ix_tl_tran_er) =       1d-6
      rl_ct_vr_df(ix_tl_cz_r_growth) =   1d8
      rl_ct_vr_df(ix_fac_long_prd) =     5d-2
      rl_ct_vr_df(ix_fac_short_prd) =    2d-1
 
      rl_ct_vr_df(ix_bwd_tran_ze) =         1d-12
      rl_ct_vr_df(ix_pc_ze) =            1d-12
      rl_ct_vr_df(ix_fwd_tran_ze) =         1d-12
      rl_ct_vr_df(ix_u_ed_wt_ze) =       1d-12
      rl_ct_vr_df(ix_pk_pv_c_ze) =       1d-12
      rl_ct_vr_ub(ix_scl_v_ze) =         0d0
 
      rl_ct_vr_df(ix_av_eta_dse) =       1d0
      rl_ct_vr_df(ix_av_ftran_sol_dse) =
     &     rl_ct_vr_df(ix_tl_fwd_tran_dse_rhs)
      rl_ct_vr_df(ix_av_btran_sol_dse) =
     &     rl_ct_vr_df(ix_tl_fwd_tran_dse_rhs)
      rl_ct_vr_df(ix_av_tbu_r_dse) =
     &     rl_ct_vr_df(ix_tl_fwd_tran_dse_rhs)
      rl_ct_vr_df(ix_tl_dvx_wt) =        3d0
      rl_ct_vr_df(ix_nw_dvx_fwk_fq) =    1d-2
 
      rl_ct_vr_df(ix_inv_pv_tl) =     1d-6
      rl_ct_vr_df(ix_inv_wr_eta_tl) = 1d-9
      rl_ct_vr_df(ix_inv_c_rlv_tl) =  1d-2
      rl_ct_vr_df(ix_inv_unit_v_tl) = 1d-6
      rl_ct_vr_df(ix_inv_l_pv_rsdu_er_tl) = 1d-6
 
      rl_ct_vr_df(ix_usr_cz_r_pv_tl) = 0d0
      rl_ct_vr_df(ix_wk_cz_r_pv_tl) =  rl_ct_vr_df(ix_usr_cz_r_pv_tl)
      rl_ct_vr_df(ix_cz_r_pv_tl) =     rl_ct_vr_df(ix_usr_cz_r_pv_tl)
 
      rl_ct_vr_df(ix_tl_bc_pr_act_it_rfn) = rl_ct_vr_df(ix_tl_pr_ifs)
      rl_ct_vr_df(ix_tl_reset_pi_it_rfn) =  rl_ct_vr_df(ix_tl_du_ifs)
      rl_ct_vr_df(ix_tl_pv_c_it_rfn) =      rl_ct_vr_df(ix_tl_pr_ifs)
      rl_ct_vr_df(ix_tl_full_pi_it_rfn) =   rl_ct_vr_df(ix_tl_du_ifs)
      rl_ct_vr_df(ix_tl_u_pi_it_rfn) =      rl_ct_vr_df(ix_tl_du_ifs)
      rl_ct_vr_df(ix_bc_pr_act_it_rfn_tran_ze) = zero
      rl_ct_vr_df(ix_reset_pi_it_rfn_tran_ze) =  zero
      rl_ct_vr_df(ix_pv_c_it_rfn_tran_ze) =      zero
      rl_ct_vr_df(ix_full_pi_it_rfn_tran_ze) =   zero
      rl_ct_vr_df(ix_u_pi_it_rfn_tran_ze) =      zero
 
      call ems_cp_lg_a(n_ems_rl_ct_vr, .false., rl_ct_vr_lg, 0)
      rl_ct_vr_lg(ix_usr_tl_pr_ifs) = .true.
      rl_ct_vr_lg(ix_usr_tl_du_ifs) = .true.
      rl_ct_vr_lg(ix_mx_mn) = .true.
      rl_ct_vr_lg(ix_pr_wt) = .true.
      rl_ct_vr_lg(ix_bb_ub) = .true.
      rl_ct_vr_lg(ix_dga_scl) = .true.
      rl_ct_vr_lg(ix_mps_ze) = .true.
      rl_ct_vr_lg(ix_i_ifs_wt) = .true.
      rl_ct_vr_lg(ix_tl_i_fs_ob_fn_v_imp) = .true.
      rl_ct_vr_lg(ix_i_fs_ob_fn_v_tgt) = .true.
      rl_ct_vr_lg(ix_tl_i_ifs) = .true.
 
      rl_ct_vr_lg(ix_tl_iz_bs_tran_er) =  .true.
 
      rl_ct_vr_lg(ix_tl_bc_pr_act_it_rfn) = .true.
      rl_ct_vr_lg(ix_tl_reset_pi_it_rfn) = .true.
      rl_ct_vr_lg(ix_tl_pv_c_it_rfn) = .true.
      rl_ct_vr_lg(ix_tl_full_pi_it_rfn) = .true.
      rl_ct_vr_lg(ix_tl_u_pi_it_rfn) = .true.
      rl_ct_vr_lg(ix_bc_pr_act_it_rfn_tran_ze) = .true.
      rl_ct_vr_lg(ix_reset_pi_it_rfn_tran_ze) = .true.
      rl_ct_vr_lg(ix_pv_c_it_rfn_tran_ze) = .true.
      rl_ct_vr_lg(ix_full_pi_it_rfn_tran_ze) = .true.
      rl_ct_vr_lg(ix_u_pi_it_rfn_tran_ze) = .true.
 
CM      IF (emsol_dev .EQ. 1) THEN
C?      rl_ct_vr_lg(ix_wk_tl_pr_ifs) = .true.
C?      rl_ct_vr_lg(ix_wk_tl_du_ifs) = .true.
C?      rl_ct_vr_lg(ix_wk_xp_tau) = .true.
C?      rl_ct_vr_lg(ix_tl_bwd_tran_dse_rhs) = .true.
C?      rl_ct_vr_lg(ix_tl_fwd_tran_dse_rhs) = .true.
C?      rl_ct_vr_lg(ix_tl_dse_tbu_r) = .true.
C?
C?      rl_ct_vr_lg(ix_usr_tl_crsh_abs_pv_v) = .true.
C?      rl_ct_vr_lg(ix_usr_tl_crsh_rlv_pv_v) = .true.
C?      rl_ct_vr_lg(ix_tl_mx_iz_pr_act) =  .true.
C?
C?      rl_ct_vr_lg(ix_tl_du_act_er) = .true.
C?      rl_ct_vr_lg(ix_tl_pv_er) = .true.
C?      rl_ct_vr_lg(ix_tl_ed_wt_er) = .true.
C?      rl_ct_vr_lg(ix_tl_tran_er) = .true.
C?      rl_ct_vr_lg(ix_tl_cz_r_growth) = .true.
C?
C?      rl_ct_vr_lg(ix_bwd_tran_ze) = .true.
C?      rl_ct_vr_lg(ix_pc_ze) = .true.
C?      rl_ct_vr_lg(ix_fwd_tran_ze) = .true.
C?      rl_ct_vr_lg(ix_u_ed_wt_ze) = .true.
C?      rl_ct_vr_lg(ix_pk_pv_c_ze) = .true.
C?      rl_ct_vr_lg(ix_scl_v_ze) = .true.
C?
C?      rl_ct_vr_lg(ix_tl_dvx_wt) =  .true.
C?      rl_ct_vr_lg(ix_nw_dvx_fwk_fq) = .true.
C?
C?      rl_ct_vr_lg(ix_inv_pv_tl) =  .true.
C?      rl_ct_vr_lg(ix_inv_wr_eta_tl) =  .true.
C?      rl_ct_vr_lg(ix_inv_c_rlv_tl) =  .true.
C?      rl_ct_vr_lg(ix_inv_unit_v_tl) =  .true.
C?      rl_ct_vr_lg(ix_inv_l_pv_rsdu_er_tl) =  .true.
C?
C?      rl_ct_vr_lg(ix_usr_cz_r_pv_tl) =  .true.
C?      rl_ct_vr_lg(ix_wk_cz_r_pv_tl) =  .true.
C?
CM      ENDIF
      return
      end
 
C->>> -----------------------------------------> ems_iz_osl_ct_vr_nm <<<
c     Set up an array of OSL names for integer and real control
c     variables.
c
      subroutine ems_iz_osl_ct_vr_nm
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
c
c     Set the integer control variable names
c
      osl_i_ct_vr_nm( 1) = 'ILOGFREQ'
      osl_i_ct_vr_nm( 2) = 'IPRINTUNIT'
      osl_i_ct_vr_nm( 3) = 'IMAXFACTOR'
      osl_i_ct_vr_nm( 4) = 'IITERNUM'
      osl_i_ct_vr_nm( 5) = 'IMAXITER'
      osl_i_ct_vr_nm( 6) = 'ILOGLEVEL'
      osl_i_ct_vr_nm( 7) = 'IONEOBJ'
      osl_i_ct_vr_nm( 8) = 'IQPARNUMITER'
      osl_i_ct_vr_nm( 9) = 'IMAXROWS'
      osl_i_ct_vr_nm(10) = 'IMAXCOLS'
      osl_i_ct_vr_nm(11) = 'INUMCHAR'
      osl_i_ct_vr_nm(12) = 'ISTOPMASK'
      osl_i_ct_vr_nm(13) = 'IMAXITERB'
      osl_i_ct_vr_nm(14) = 'IADJACTYPE'
      osl_i_ct_vr_nm(15) = 'IFORMNTYPE'
      osl_i_ct_vr_nm(16) = 'IDENSECOL'
      osl_i_ct_vr_nm(17) = 'IDEVEXMODE'
      osl_i_ct_vr_nm(18) = 'INULLCHECK'
      osl_i_ct_vr_nm(19) = 'IDROPROWCT'
      osl_i_ct_vr_nm(20) = 'IITERUFREQ'
      osl_i_ct_vr_nm(21) = 'IPOSSBASIS'
      osl_i_ct_vr_nm(22) = 'IMAXPROJNS'
      osl_i_ct_vr_nm(23) = 'INUMELS'
      osl_i_ct_vr_nm(24) = 'INUMBLOCKS'
      osl_i_ct_vr_nm(25) = 'IMSGPOS'
      osl_i_ct_vr_nm(26) = 'IPAGELINES'
      osl_i_ct_vr_nm(27) = 'INUMROWS'
      osl_i_ct_vr_nm(28) = 'INUMCOLS'
      osl_i_ct_vr_nm(29) = 'INUMPINF'
      osl_i_ct_vr_nm(30) = 'INUMDINF'
      osl_i_ct_vr_nm(31) = 'IMODELMASK'
      osl_i_ct_vr_nm(32) = 'IPRTINFOMASK'
      osl_i_ct_vr_nm(33) = 'ISOLMASK'
      osl_i_ct_vr_nm(33) = 'IPRTMTRXMASK'
      osl_i_ct_vr_nm(34) = 'IEXTRABLK'
      osl_i_ct_vr_nm(35) = 'IQPARMAXITER'
      osl_i_ct_vr_nm(36) = 'ILINELEN'
      osl_i_ct_vr_nm(37) = 'INUMNODES'
      osl_i_ct_vr_nm(38) = 'IINTMASK'
      osl_i_ct_vr_nm(39) = 'IFASTITS'
      osl_i_ct_vr_nm(40) = 'IMAXNODES'
      osl_i_ct_vr_nm(41) = 'IMAXSOLS'
      osl_i_ct_vr_nm(42) = 'INUMSOLS'
      osl_i_ct_vr_nm(43) = 'INUMINTS'
      osl_i_ct_vr_nm(44) = 'INUMSETS'
      osl_i_ct_vr_nm(45) = 'INUMUNSAT'
      osl_i_ct_vr_nm(46) = 'IVECTOR'
      osl_i_ct_vr_nm(47) = 'IPROBSTAT'
      osl_i_ct_vr_nm(48) = 'IMAJORITS'
      osl_i_ct_vr_nm(49) = 'IPRINTSENS'
      osl_i_ct_vr_nm(50) = 'IMAXINTS'
      osl_i_ct_vr_nm(51) = 'IMAXSETS'
      osl_i_ct_vr_nm(52) = 'ISTRATEGY'
      osl_i_ct_vr_nm(53) = 'IMAXINTINFO'
      osl_i_ct_vr_nm(54) = 'ITHRESHOLD'
      osl_i_ct_vr_nm(55) = 'IHEURPASS'
      osl_i_ct_vr_nm(56) = 'ISUPERTOL'
      osl_i_ct_vr_nm(57) = 'IROWORD'
      osl_i_ct_vr_nm(58) = 'IORDUNIT'
      osl_i_ct_vr_nm(59) = 'IMIPLENGTH'
      osl_i_ct_vr_nm(60) = 'IITERBNUM'
      osl_i_ct_vr_nm(61) = 'IPRICETYPE'
      osl_i_ct_vr_nm(62) = 'INUMCPU'
      osl_i_ct_vr_nm(63) = 'IPROBSTAT2'
      osl_i_ct_vr_nm(64) = 'ISMDLTYPEMASK'
      osl_i_ct_vr_nm(65) = 'ITOTALCPU'
      osl_i_ct_vr_nm(66) = 'IWHICHCPU'
      osl_i_ct_vr_nm(67) = 'ISTRIPES'
      osl_i_ct_vr_nm(69) = 'INUMELQ'
      osl_i_ct_vr_nm(70) = 'IORDTHRSH'
      osl_i_ct_vr_nm(71) = 'ILPDCFLAG'
c
c     Set the real control variable names
c
      osl_rl_ct_vr_nm( 1) = 'RTOLPINF'
      osl_rl_ct_vr_nm( 2) = 'RTOLDINF'
      osl_rl_ct_vr_nm( 3) = 'RMAXMIN'
      osl_rl_ct_vr_nm( 4) = 'RMUFACTOR'
      osl_rl_ct_vr_nm( 5) = 'RMULIMIT'
      osl_rl_ct_vr_nm( 6) = 'RRGFACTOR'
      osl_rl_ct_vr_nm( 7) = 'RRGLIMIT'
      osl_rl_ct_vr_nm( 8) = 'RFIXVAR1'
      osl_rl_ct_vr_nm( 9) = 'RFIXVAR2'
      osl_rl_ct_vr_nm(10) = 'RCHOLABSTOL'
      osl_rl_ct_vr_nm(11) = 'RCHOLRELTOL'
      osl_rl_ct_vr_nm(12) = 'RMULINFAC'
      osl_rl_ct_vr_nm(13) = 'RPROJTOL'
      osl_rl_ct_vr_nm(14) = 'RPWEIGHT'
      osl_rl_ct_vr_nm(15) = 'RCHANGEWEIGHT'
      osl_rl_ct_vr_nm(16) = 'RBBCUTOFF'
      osl_rl_ct_vr_nm(17) = 'RDWEIGHT'
      osl_rl_ct_vr_nm(18) = 'ROBJVALUE'
      osl_rl_ct_vr_nm(19) = 'RSUMPINF'
      osl_rl_ct_vr_nm(20) = 'RSUMDINF'
      osl_rl_ct_vr_nm(21) = 'RTOLMPS'
      osl_rl_ct_vr_nm(22) = 'RDEGSCALE'
      osl_rl_ct_vr_nm(23) = 'RBESTSOL'
      osl_rl_ct_vr_nm(24) = 'RIWEIGHT'
      osl_rl_ct_vr_nm(25) = 'RIMPROVE'
      osl_rl_ct_vr_nm(26) = 'RTARGET'
      osl_rl_ct_vr_nm(27) = 'RTOLINT'
      osl_rl_ct_vr_nm(28) = 'RBESTPOSS'
      osl_rl_ct_vr_nm(29) = 'RBESTEST'
      osl_rl_ct_vr_nm(30) = 'RSTEPMULT'
      osl_rl_ct_vr_nm(31) = 'RMUINIT'
      osl_rl_ct_vr_nm(32) = 'RDENSETHR'
      osl_rl_ct_vr_nm(33) = 'ROBJWEIGHT'
      osl_rl_ct_vr_nm(34) = 'RLAMBDAVAL'
      osl_rl_ct_vr_nm(35) = 'RDCCUTOFF'
      osl_rl_ct_vr_nm(36) = 'RDOBJVAL'
      osl_rl_ct_vr_nm(37) = 'RSLAMBDA'
      osl_rl_ct_vr_nm(38) = 'RSLAMBDALIM'
      osl_rl_ct_vr_nm(39) = 'RSLAMBDADELTA'
      osl_rl_ct_vr_nm(40) = 'RTHRESHOLD'
      osl_rl_ct_vr_nm(41) = 'RPDGAPTOL'
      osl_rl_ct_vr_nm(42) = 'RPDSTEPMULT'
      osl_rl_ct_vr_nm(43) = 'RPERTDIAG'
      osl_rl_ct_vr_nm(44) = 'RNETSAMP'
      osl_rl_ct_vr_nm(45) = 'RPRINTCPU'
      return
      end
C->>> -----------------------------------------> ems_iz_ems_ct_vr_nm <<<
c     Set up an array of EMSOL names for integer and real control
c     variables.
c
      subroutine ems_iz_ems_ct_vr_nm
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
c
c     Set the integer control variable names
c
      ems_i_ct_vr_nm(ix_msg_fq) = 'msg_fq'
      ems_i_ct_vr_nm(ix_ems_wr_cn) = 'ems_wr_cn'
      ems_i_ct_vr_nm(ix_usr_mx_n_u) = 'usr_mx_n_u'
      ems_i_ct_vr_nm(ix_n_si_it) = 'n_si_it'
      ems_i_ct_vr_nm(ix_mx_n_si_it) = 'mx_n_si_it'
      ems_i_ct_vr_nm(ix_rsmi_msg_msk) = 'rsmi_msg_msk'
      ems_i_ct_vr_nm(ix_mx_n_r) = 'mx_n_r'
      ems_i_ct_vr_nm(ix_mx_n_c) = 'mx_n_c'
      ems_i_ct_vr_nm(ix_ml_nm_n_ch) = 'ml_nm_n_ch'
      ems_i_ct_vr_nm(ix_dvx_mode) = 'dvx_mode'
      ems_i_ct_vr_nm(ix_it_usr_xit_fq) = 'it_usr_xit_fq'
      ems_i_ct_vr_nm(ix_n_el_in_aux_blk) = 'n_el_in_aux_blk'
      ems_i_ct_vr_nm(ix_n_aux_blk) = 'n_aux_blk'
      ems_i_ct_vr_nm(ix_n_pg_li) = 'n_pg_li'
      ems_i_ct_vr_nm(ix_n_r) = 'n_r'
      ems_i_ct_vr_nm(ix_n_c) = 'n_c'
      ems_i_ct_vr_nm(ix_n_pr_ifs) = 'n_pr_ifs'
      ems_i_ct_vr_nm(ix_n_du_ifs) = 'n_du_ifs'
      ems_i_ct_vr_nm(ix_wr_ml_da_msk) = 'wr_ml_da_msk'
      ems_i_ct_vr_nm(ix_wr_ml_st_msk) = 'wr_ml_st_msk'
      ems_i_ct_vr_nm(ix_mx_n_el_in_aux_blk) = 'mx_n_el_in_aux_blk'
      ems_i_ct_vr_nm(ix_wr_li_ln) = 'wr_li_ln'
      ems_i_ct_vr_nm(ix_mx_nd_n) = 'mx_nd_n'
      ems_i_ct_vr_nm(ix_i_lp_msg_msk) = 'i_lp_msg_msk'
      ems_i_ct_vr_nm(ix_mx_n_nd) = 'mx_n_nd'
      ems_i_ct_vr_nm(ix_mx_n_i_fs_sol) = 'mx_n_i_fs_sol'
      ems_i_ct_vr_nm(ix_n_i_fs_sol) = 'n_i_fs_sol'
      ems_i_ct_vr_nm(ix_n_i_vr) = 'n_i_vr'
      ems_i_ct_vr_nm(ix_n_sos_se) = 'n_sos_se'
      ems_i_ct_vr_nm(ix_n_i_vr_no_i) = 'n_i_vr_no_i'
      ems_i_ct_vr_nm(ix_prob_st) = 'prob_st'
      ems_i_ct_vr_nm(ix_wr_rg_da_msk) = 'wr_rg_da_msk'
      ems_i_ct_vr_nm(ix_mx_n_i_vr) = 'mx_n_i_vr'
      ems_i_ct_vr_nm(ix_mx_n_sos_se) = 'mx_n_sos_se'
      ems_i_ct_vr_nm(ix_i_alg) = 'i_alg'
      ems_i_ct_vr_nm(ix_mx_n_i_da_en) = 'mx_n_i_da_en'
      ems_i_ct_vr_nm(ix_crsh_msk) = 'crsh_msk'
      ems_i_ct_vr_nm(ix_crsh_tl_dl_pri) = 'crsh_tl_dl_pri'
      ems_i_ct_vr_nm(ix_crsh_fn_dn) = 'crsh_fn_dn'
      ems_i_ct_vr_nm(ix_ems_er_cn) = 'ems_er_cn'
      ems_i_ct_vr_nm(ix_ts_parsmi) = 'ts_parsmi'
      ems_i_ct_vr_nm(ix_ems_fi_wr_msk) = 'ems_fi_wr_msk'
      ems_i_ct_vr_nm(ix_scl_mode) = 'scl_mode'
      ems_i_ct_vr_nm(ix_scl_bd) = 'scl_bd'
      ems_i_ct_vr_nm(ix_mx_n_a_el) = 'mx_n_a_el'
      ems_i_ct_vr_nm(ix_n_a_el) = 'n_a_el'
      ems_i_ct_vr_nm(ix_ml_blk_st_msk) = 'ml_blk_st_msk'
      ems_i_ct_vr_nm(ix_ml_da_st_msk) = 'ml_da_st_msk'
      ems_i_ct_vr_nm(ix_ml_da_no_cg_msk) = 'ml_da_no_cg_msk'
      ems_i_ct_vr_nm(ix_du_sol_mode) = 'du_sol_mode'
      ems_i_ct_vr_nm(ix_ml_nm_n_rl) = 'ml_nm_n_rl'
      ems_i_ct_vr_nm(ix_sslv_en_msk) = 'sslv_en_msk'
      ems_i_ct_vr_nm(ix_sslv_xit_msk) = 'sslv_xit_msk'
      ems_i_ct_vr_nm(ix_n_bs) = 'n_bs'
      ems_i_ct_vr_nm(ix_n_reset) = 'n_reset'
      ems_i_ct_vr_nm(ix_rq_reset) = 'rq_reset'
      ems_i_ct_vr_nm(ix_u_pc) = 'u_pc'
      ems_i_ct_vr_nm(ix_r_pc) = 'r_pc'
      ems_i_ct_vr_nm(ix_eta_fi_mode_msk) = 'eta_fi_mode_msk'
      ems_i_ct_vr_nm(ix_eta_fi_da_st_msk) = 'eta_fi_da_st_msk'
      ems_i_ct_vr_nm(ix_mn_n_dvx_it) = 'mn_n_dvx_it'
      ems_i_ct_vr_nm(ix_pc_alg) = 'pc_alg'
      ems_i_ct_vr_nm(ix_n_dvx_it) = 'n_dvx_it'
      ems_i_ct_vr_nm(ix_n_dvx_fwk) = 'n_dvx_fwk'
      ems_i_ct_vr_nm(ix_r_du_act_sgn) = 'r_du_act_sgn'
      ems_i_ct_vr_nm(ix_inv_alg_msk) = 'inv_alg_msk'
      ems_i_ct_vr_nm(ix_inv_msg_msk) = 'inv_msg_msk'
      ems_i_ct_vr_nm(ix_inv_log_msk) = 'inv_log_msk'
      ems_i_ct_vr_nm(ix_inv_mx_n_eta) = 'inv_mx_n_eta'
      ems_i_ct_vr_nm(ix_inv_mx_n_eta_el) = 'inv_mx_n_eta_el'
      ems_i_ct_vr_nm(ix_n_inv_sing) = 'n_inv_sing'
      ems_i_ct_vr_nm(ix_u_bs_cg) = 'u_bs_cg'
      ems_i_ct_vr_nm(ix_rq_inv) = 'rq_inv'
      ems_i_ct_vr_nm(ix_sto_ftran_ix_mode) = 'sto_ftran_ix_mode'
      ems_i_ct_vr_nm(ix_sto_btran_ix_mode) = 'sto_btran_ix_mode'
      ems_i_ct_vr_nm(ix_sto_tbu_r_ix_mode) = 'sto_tbu_r_ix_mode'
      ems_i_ct_vr_nm(ix_tbu_r_loop_mode) = 'tbu_r_loop_mode'
      ems_i_ct_vr_nm(ix_sto_ftran_ix) = 'sto_ftran_ix'
      ems_i_ct_vr_nm(ix_sto_btran_ix) = 'sto_btran_ix'
      ems_i_ct_vr_nm(ix_sto_tbu_r_ix) = 'sto_tbu_r_ix'
      ems_i_ct_vr_nm(ix_bwd_tran_dse_rhs_n_r) = 'bwd_tran_dse_rhs_n_r'
      ems_i_ct_vr_nm(ix_fwd_tran_dse_rhs_n_r) = 'fwd_tran_dse_rhs_n_r'
      ems_i_ct_vr_nm(ix_dse_tbu_r_n_c) = 'dse_tbu_r_n_c'
      ems_i_ct_vr_nm(ix_usr_cz_c_msk) = 'usr_cz_c_msk'
      ems_i_ct_vr_nm(ix_usr_cz_r_msk) = 'usr_cz_r_msk'
      ems_i_ct_vr_nm(ix_l1_cz_r_mx_n_cdd) = 'l1_cz_r_mx_n_cdd'
      ems_i_ct_vr_nm(ix_n_pc_vr) = 'n_pc_vr'
      ems_i_ct_vr_nm(ix_n_pc_el) = 'n_pc_el'
      ems_i_ct_vr_nm(ix_n_non_bc_pr_ifs) = 'n_non_bc_pr_ifs'
      ems_i_ct_vr_nm(ix_rq_re_pc) = 'rq_re_pc'
      ems_i_ct_vr_nm(ix_cz_c_msk) = 'cz_c_msk'
      ems_i_ct_vr_nm(ix_cz_r_msk) = 'cz_r_msk'
      ems_i_ct_vr_nm(ix_u_bs_mode) = 'u_bs_mode'
      ems_i_ct_vr_nm(ix_sto_u_eta_v_mode) = 'sto_u_eta_v_mode'
      ems_i_ct_vr_nm(ix_u_bs) = 'u_bs'
      ems_i_ct_vr_nm(ix_sto_u_eta_v) = 'sto_u_eta_v'
      ems_i_ct_vr_nm(ix_sto_u_eta_se_ty) = 'sto_u_eta_se_ty'
      ems_i_ct_vr_nm(ix_mx_n_u) = 'mx_n_u'
      ems_i_ct_vr_nm(ix_n_u) = 'n_u'
      ems_i_ct_vr_nm(ix_lo_sto_mode) = 'lo_sto_mode'
      ems_i_ct_vr_nm(ix_asm_msk) = 'asm_msk'
      ems_i_ct_vr_nm(ix_ck_msk) = 'ck_msk'
      ems_i_ct_vr_nm(ix_it_o_sw_fm_wr_df) = 'it_o_sw_fm_wr_df'
      ems_i_ct_vr_nm(ix_it_o_sw_fm_ck_df) = 'it_o_sw_fm_ck_df'
      ems_i_ct_vr_nm(ix_wr_lp_da) = 'wr_lp_da'
      ems_i_ct_vr_nm(ix_wr_bs_fq) = 'wr_bs_fq'
      ems_i_ct_vr_nm(ix_g_tt_da) = 'g_tt_da'
      ems_i_ct_vr_nm(ix_g_it_tt_fq) = 'g_it_tt_fq'
      ems_i_ct_vr_nm(ix_g_it_tt_ivl) = 'g_it_tt_ivl'
      ems_i_ct_vr_nm(ix_mx_n_bc_pr_act_rfn_it) = 'mx_n_bc_pr_act_rfn_it'
      ems_i_ct_vr_nm(ix_mx_n_reset_pi_rfn_it) = 'mx_n_reset_pi_rfn_it'
      ems_i_ct_vr_nm(ix_mx_n_pv_c_rfn_it) = 'mx_n_pv_c_rfn_it'
      ems_i_ct_vr_nm(ix_mx_n_full_pi_rfn_it) = 'mx_n_full_pi_rfn_it'
      ems_i_ct_vr_nm(ix_mx_n_u_pi_rfn_it) = 'mx_n_u_pi_rfn_it'
      ems_i_ct_vr_nm(ix_eta_fi_n_grp) = 'eta_fi_n_grp'
      ems_i_ct_vr_nm(ix_eta_fi_n_inv_grp) = 'eta_fi_n_inv_grp'
      ems_i_ct_vr_nm(ix_eta_fi_n_se) = 'eta_fi_n_se'
      ems_i_ct_vr_nm(ix_eta_fi_n_inv_se) = 'eta_fi_n_inv_se'
      ems_i_ct_vr_nm(ix_eta_fi_n_eta) = 'eta_fi_n_eta'
      ems_i_ct_vr_nm(ix_eta_fi_n_inv_eta) = 'eta_fi_n_inv_eta'
      ems_i_ct_vr_nm(ix_eta_fi_n_v) = 'eta_fi_n_v'
      ems_i_ct_vr_nm(ix_eta_fi_n_inv_v) = 'eta_fi_n_inv_v'
      ems_i_ct_vr_nm(ix_eta_fi_n_ix) = 'eta_fi_n_ix'
      ems_i_ct_vr_nm(ix_eta_fi_n_inv_ix) = 'eta_fi_n_inv_ix'
      ems_i_ct_vr_nm(ix_mx_n_pwl_vr) = 'mx_n_pwl_vr'
      ems_i_ct_vr_nm(ix_mx_n_pwl_vr_da_en) = 'mx_n_pwl_vr_da_en'
      ems_i_ct_vr_nm(ix_repl_non_std_vr_mode_msk) =
     &     'repl_non_std_vr_mode_msk'
      ems_i_ct_vr_nm(ix_repl_non_std_vr_st_msk) =
     &     'repl_non_std_vr_st_msk'
      ems_i_ct_vr_nm(ix_n_bp_vr) = 'n_bp_vr'
      ems_i_ct_vr_nm(ix_n_pwl_vr) = 'n_pwl_vr'
      ems_i_ct_vr_nm(ix_n_pwl_vr_da_en) = 'n_pwl_vr_da_en'
      ems_i_ct_vr_nm(ix_cu_is_n) = 'cu_is_n'
      ems_i_ct_vr_nm(ix_cu_ml_n) = 'cu_ml_n'
      ems_i_ct_vr_nm(ix_ml_blk_mv_k) = 'ml_blk_mv_k'
      ems_i_ct_vr_nm(ix_ds_n_en_m1) = 'ds_n_en_m1'
      ems_i_ct_vr_nm(ix_is_n_en_m1) = 'is_n_en_m1'
      ems_i_ct_vr_nm(ix_ns_n_en_m1) = 'ns_n_en_m1'
      ems_i_ct_vr_nm(ix_sv_ml_ct_vr) = 'sv_ml_ct_vr'
      ems_i_ct_vr_nm(ix_n_lo_c_eta) = 'n_lo_c_eta'
      ems_i_ct_vr_nm(ix_n_lo_r_eta) = 'n_lo_r_eta'
      ems_i_ct_vr_nm(ix_n_lo_r_eta_el) = 'n_lo_r_eta_el'
      ems_i_ct_vr_nm(ix_n_up_r_eta) = 'n_up_r_eta'
      ems_i_ct_vr_nm(ix_n_up_r_eta_el) = 'n_up_r_eta_el'
      ems_i_ct_vr_nm(ix_n_sus_eta_srch_op_cf) = 'n_sus_eta_srch_op_cf'
      ems_i_ct_vr_nm(ix_sus_eta_srch_al_t_ls_ix_lm) =
     &     'sus_eta_srch_al_t_ls_ix_lm'
      ems_i_ct_vr_nm(ix_sus_eta_srch_ls_t_buk_ix_lm) =
     &     'sus_eta_srch_ls_t_buk_ix_lm'
      ems_i_ct_vr_nm(ix_sus_eta_srch_dbl_n_buk_ix_lm) =
     &     'sus_eta_srch_dbl_n_buk_ix_lm'
c
c     Set the real control variable names
c
      ems_rl_ct_vr_nm(ix_usr_tl_pr_ifs) = 'usr_tl_pr_ifs'
      ems_rl_ct_vr_nm(ix_usr_tl_du_ifs) = 'usr_tl_du_ifs'
      ems_rl_ct_vr_nm(ix_mx_mn) = 'mx_mn'
      ems_rl_ct_vr_nm(ix_pr_wt) = 'pr_wt'
      ems_rl_ct_vr_nm(ix_bb_ub) = 'bb_ub'
      ems_rl_ct_vr_nm(ix_ob_fn_v) = 'ob_fn_v'
      ems_rl_ct_vr_nm(ix_su_pr_ifs) = 'su_pr_ifs'
      ems_rl_ct_vr_nm(ix_su_du_ifs) = 'su_du_ifs'
      ems_rl_ct_vr_nm(ix_mps_ze) = 'mps_ze'
      ems_rl_ct_vr_nm(ix_dga_scl) = 'dga_scl'
      ems_rl_ct_vr_nm(ix_bst_i_fs_ob_fn_v) = 'bst_i_fs_ob_fn_v'
      ems_rl_ct_vr_nm(ix_i_ifs_wt) = 'i_ifs_wt'
      ems_rl_ct_vr_nm(ix_tl_i_fs_ob_fn_v_imp) = 'tl_i_fs_ob_fn_v_imp'
      ems_rl_ct_vr_nm(ix_i_fs_ob_fn_v_tgt) = 'i_fs_ob_fn_v_tgt'
      ems_rl_ct_vr_nm(ix_tl_i_ifs) = 'tl_i_ifs'
      ems_rl_ct_vr_nm(ix_bst_ob_fn_v) = 'bst_ob_fn_v'
      ems_rl_ct_vr_nm(ix_bst_est_ob_fn_v) = 'bst_est_ob_fn_v'
      ems_rl_ct_vr_nm(ix_wk_tl_pr_ifs) = 'wk_tl_pr_ifs'
      ems_rl_ct_vr_nm(ix_wk_tl_du_ifs) = 'wk_tl_du_ifs'
      ems_rl_ct_vr_nm(ix_wk_xp_tau) = 'wk_xp_tau'
      ems_rl_ct_vr_nm(ix_tl_pr_ifs) = 'tl_pr_ifs'
      ems_rl_ct_vr_nm(ix_tl_du_ifs) = 'tl_du_ifs'
      ems_rl_ct_vr_nm(ix_xp_tau) = 'xp_tau'
      ems_rl_ct_vr_nm(ix_tl_bwd_tran_dse_rhs) = 'tl_bwd_tran_dse_rhs'
      ems_rl_ct_vr_nm(ix_tl_fwd_tran_dse_rhs) = 'tl_fwd_tran_dse_rhs'
      ems_rl_ct_vr_nm(ix_sus_eta_srch_co_mu) = 'sus_eta_srch_co_mu'
      ems_rl_ct_vr_nm(ix_tl_dse_tbu_r) = 'tl_dse_tbu_r'
      ems_rl_ct_vr_nm(ix_usr_tl_crsh_abs_pv_v) = 'usr_tl_crsh_abs_pv_v'
      ems_rl_ct_vr_nm(ix_usr_tl_crsh_rlv_pv_v) = 'usr_tl_crsh_rlv_pv_v'
      ems_rl_ct_vr_nm(ix_tl_iz_bs_tran_er) = 'tl_iz_bs_tran_er'
      ems_rl_ct_vr_nm(ix_tl_mx_iz_pr_act) = 'tl_mx_iz_pr_act'
      ems_rl_ct_vr_nm(ix_tl_du_act_er) = 'tl_du_act_er'
      ems_rl_ct_vr_nm(ix_tl_pv_er) = 'tl_pv_er'
      ems_rl_ct_vr_nm(ix_tl_ed_wt_er) = 'tl_ed_wt_er'
      ems_rl_ct_vr_nm(ix_tl_tran_er) = 'tl_tran_er'
      ems_rl_ct_vr_nm(ix_tl_cz_r_growth) = 'tl_cz_r_growth'
      ems_rl_ct_vr_nm(ix_fac_long_prd) = 'fac_long_prd'
      ems_rl_ct_vr_nm(ix_fac_short_prd) = 'fac_short_prd'
      ems_rl_ct_vr_nm(ix_bwd_tran_ze) = 'bwd_tran_ze'
      ems_rl_ct_vr_nm(ix_pc_ze) = 'pc_ze'
      ems_rl_ct_vr_nm(ix_fwd_tran_ze) = 'fwd_tran_ze'
      ems_rl_ct_vr_nm(ix_u_ed_wt_ze) = 'u_ed_wt_ze'
      ems_rl_ct_vr_nm(ix_pk_pv_c_ze) = 'pk_pv_c_ze'
      ems_rl_ct_vr_nm(ix_av_eta_dse) = 'av_eta_dse'
      ems_rl_ct_vr_nm(ix_av_ftran_sol_dse) = 'av_ftran_sol_dse'
      ems_rl_ct_vr_nm(ix_av_btran_sol_dse) = 'av_btran_sol_dse'
      ems_rl_ct_vr_nm(ix_av_tbu_r_dse) = 'av_tbu_r_dse'
      ems_rl_ct_vr_nm(ix_scl_v_ze) = 'scl_v_ze'
      ems_rl_ct_vr_nm(ix_tl_dvx_wt) = 'tl_dvx_wt'
      ems_rl_ct_vr_nm(ix_nw_dvx_fwk_fq) = 'nw_dvx_fwk_fq'
      ems_rl_ct_vr_nm(ix_inv_pv_tl) = 'inv_pv_tl'
      ems_rl_ct_vr_nm(ix_inv_wr_eta_tl) = 'inv_wr_eta_tl'
      ems_rl_ct_vr_nm(ix_inv_c_rlv_tl) = 'inv_c_rlv_tl'
      ems_rl_ct_vr_nm(ix_inv_unit_v_tl) = 'inv_unit_v_tl'
      ems_rl_ct_vr_nm(ix_inv_l_pv_rsdu_er_tl) = 'inv_l_pv_rsdu_er_tl'
      ems_rl_ct_vr_nm(ix_usr_cz_r_pv_tl) = 'usr_cz_r_pv_tl'
      ems_rl_ct_vr_nm(ix_wk_cz_r_pv_tl) = 'wk_cz_r_pv_tl'
      ems_rl_ct_vr_nm(ix_cz_r_pv_tl) = 'cz_r_pv_tl'
      ems_rl_ct_vr_nm(ix_tl_bc_pr_act_it_rfn) = 'tl_bc_pr_act_it_rfn'
      ems_rl_ct_vr_nm(ix_tl_reset_pi_it_rfn) = 'tl_reset_pi_it_rfn'
      ems_rl_ct_vr_nm(ix_tl_pv_c_it_rfn) = 'tl_pv_c_it_rfn'
      ems_rl_ct_vr_nm(ix_tl_full_pi_it_rfn) = 'tl_full_pi_it_rfn'
      ems_rl_ct_vr_nm(ix_tl_u_pi_it_rfn) = 'tl_u_pi_it_rfn'
      ems_rl_ct_vr_nm(ix_bc_pr_act_it_rfn_tran_ze) =
     &     'bc_pr_act_it_rfn_tran_ze'
      ems_rl_ct_vr_nm(ix_reset_pi_it_rfn_tran_ze) =
     &     'reset_pi_it_rfn_tran_ze'
      ems_rl_ct_vr_nm(ix_pv_c_it_rfn_tran_ze) =
     &     'pv_c_it_rfn_tran_ze'
      ems_rl_ct_vr_nm(ix_full_pi_it_rfn_tran_ze) =
     &     'full_pi_it_rfn_tran_ze'
      ems_rl_ct_vr_nm(ix_u_pi_it_rfn_tran_ze) = 'u_pi_it_rfn_tran_ze'
      ems_rl_ct_vr_nm(ix_rl_ml_nm) = 'rl_ml_nm'
      ems_rl_ct_vr_nm(ix_ob_fn_cs) = 'ob_fn_cs'
      ems_rl_ct_vr_nm(ix_su_non_bc_pr_ifs) = 'su_non_bc_pr_ifs'
      return
      end
 
C->>> ---------------------------------------------> ems_wr_rl_ct_vr <<<
      subroutine ems_wr_rl_ct_vr
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      integer ix
      do 10, ix = 1, n_ems_rl_ct_vr
         if (rl_ct_vr_lg(ix)) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &           ix, rl_ct_vr_lb(ix), rl_ct_vr_ub(ix),
     &           rl_ct_vr_df(ix), rl_ct_vr(ix)
            call ems_msg_wr_li(info_msg_n)
         else
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)
     &           ix, rl_ct_vr_lb(ix), rl_ct_vr_ub(ix),
     &           rl_ct_vr_df(ix), rl_ct_vr(ix)
            call ems_msg_wr_li(info_msg_n)
         end if
         if (rl_ct_vr(ix) .lt. rl_ct_vr_lb(ix) .or.
     &        rl_ct_vr(ix) .gt. rl_ct_vr_ub(ix)) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)ix
            call ems_msg_wr_li(er_msg_n)
         end if
 10   continue
      return
 9000 format('rl_ct_vr ', i3, '     is settable: range [', g11.4, ', ',
     &     g11.4, ']; default ', g11.4, '; value ', g11.4)
 9010 format('rl_ct_vr ', i3, ' is not settable: range [', g11.4, ', ',
     &     g11.4, ']; default ', g11.4, '; value ', g11.4)
 9100 format('rl_ct_vr ', i3, ' violates its range ')
      end
 
C->>> ---------------------------------------------> ems_iz_ch_ct_vr <<<
c     Initialise the character control variables.
c
      subroutine ems_iz_ch_ct_vr
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'CHCTVR.INC'
      ch_ml_nm =  ch_un_nm
      ch_ob_nm =  ch_un_nm
      ch_rhs_nm = ch_un_nm
      ch_rg_nm =  ch_un_nm
      ch_bd_nm =  ch_un_nm
      ch_bs_nm =  ch_un_nm
      return
      end
 
C->>> ---------------------------------> ems_cp_ct_vr_t_or_fm_bs_blk <<<
c     Copies the control variables
c     to   the base block (if t=.true. ) or
c     from the base block (if t=.false.).
c
      subroutine ems_cp_ct_vr_t_or_fm_bs_blk(t, p_bs_blk, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'CHCTVR.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      logical t
      double precision ds(0:*)
      integer is(0:*), p_bs_blk
      integer ems_mem_mgr_nx_i_n_en_p
      integer ln, og_p_bs_blk, p, ch_n
 
      og_p_bs_blk = p_bs_blk
      p_bs_blk = ems_mem_mgr_nx_i_n_en_p(p_bs_blk, rl_wo_z)
      p = p_bs_blk/rl_wo_z
      ln = n_ems_rl_ct_vr*rl_wo_z
      if (t) then
         call ems_cp_rl_a1(n_ems_rl_ct_vr, rl_ct_vr, ds(p))
      else
         call ems_cp_rl_a1(n_ems_rl_ct_vr, ds(p), rl_ct_vr)
      end if
      p_bs_blk = p_bs_blk + ln
c
      p_bs_blk = ems_mem_mgr_nx_i_n_en_p(p_bs_blk, ch_wo_z)
      p = p_bs_blk/i_wo_z
      ln = n_ch_ct_vr*ch_wo_z
      if (t) then
         do 10, ch_n = 1, n_ch_ct_vr
            call ems_ch8_t_i(ch_ct_vr(ch_n), is(p))
            p = p + ch_wo_z
 10      continue
      else
         do 20, ch_n = 1, n_ch_ct_vr
            call ems_i_t_ch8(is(p), ch_ct_vr(ch_n))
            p = p + ch_wo_z
 20      continue
      end if
      p_bs_blk = p_bs_blk + ln
c
      p_bs_blk = ems_mem_mgr_nx_i_n_en_p(p_bs_blk, i_wo_z)
      p = p_bs_blk/i_wo_z
      ln = n_ems_i_ct_vr*i_wo_z
      if (t) then
         call ems_cp_i_a1(n_ems_i_ct_vr, i_ct_vr, is(p))
      else
         call ems_cp_i_a1(n_ems_i_ct_vr, is(p), i_ct_vr)
      end if
      p_bs_blk = p_bs_blk + ln
c
      if (p_bs_blk-og_p_bs_blk .gt. ct_vr_n_wo) go to 8102
      if (.not. t) then
         ems_msg_wr_cn = ems_wr_cn
         ems_msg_er_cn = ems_er_cn
         ems_msg_wr_li_ln = wr_li_ln
         ems_msg_n_pg_li = n_pg_li
      endif
 7000 continue
      return
 8102 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9820)
     &     p_bs_blk-og_p_bs_blk, ct_vr_n_wo
      call ems_msg_wr_li(serious_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?      call ems_dump
CM      ENDIF
      go to 7000
 9820 format(i9, ' words required for ct_vr rather than ', i9)
      end
