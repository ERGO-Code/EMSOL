CM
      integer ix_blk_ml_c_mtx
      integer ix_hdl_mtx_r_v
      integer ix_hdl_mtx_r_ix
      integer ix_hdl_mtx_c_sa
      integer ix_hdl_mtx_c_ln
 
      integer ix_blk_ml_vec
      integer ix_hdl_lbc
      integer ix_hdl_ubc
      integer ix_hdl_cbp
      integer ix_hdl_nm
 
      integer ix_blk_ml_lng_nm
      integer ix_hdl_lng_nm
 
      integer ix_blk_ml_pk_bs_os
      integer ix_hdl_pk_bs_os
 
      integer ix_blk_ml_pwl_vr
      integer ix_hdl_pwl_vr_ls
      integer ix_hdl_pwl_vr_rf_pr_act_v
      integer ix_hdl_pwl_vr_rf_ob_fn_v
      integer ix_hdl_pwl_vr_da_v
      integer ix_hdl_pwl_vr_da_sa
      integer ix_hdl_pwl_vr_cu_sn
      integer ix_hdl_pwl_vr_ix
      integer ix_hdl_pwl_vr_usr_bp
      integer ix_hdl_pwl_vr_usr_ls
      integer ix_hdl_pwl_vr_usr_sn
      integer ix_hdl_pwl_vr_usr_da_sa
      integer ix_hdl_pwl_vr_usr_cu_sn
 
      integer ix_blk_ml_sol
      integer ix_hdl_scl
      integer ix_hdl_pr_act
      integer ix_hdl_du_act
      integer ix_hdl_ed_wt
      integer ix_hdl_st
 
      integer ix_blk_ml_vr_ls
      integer ix_hdl_vr_in_r
      integer ix_hdl_vr_in_c
 
      integer ix_blk_ml_bs_inv_p
      integer ix_hdl_lo_eta_pv_in_r
      integer ix_hdl_up_eta_pv_in_r
      integer ix_hdl_eta_w_l_en_in_r
      integer ix_hdl_eta_w_lm1_en_in_r
      integer ix_hdl_og_t_nw_perm
      integer ix_hdl_nw_t_og_perm
 
      integer ix_blk_dvx
      integer ix_hdl_dvx_ix
 
      integer ix_blk_ml_r_mtx
      integer ix_hdl_mtx_c_v
      integer ix_hdl_mtx_c_ix
      integer ix_hdl_mtx_r_sa
 
      integer ix_blk_ml_aux_blk
      integer ix_hdl_aux_blk_v
      integer ix_hdl_aux_blk_r_ix
      integer ix_hdl_aux_blk_c_ix
 
      integer ix_blk_ml_aux_sol
      integer ix_hdl_r_aux_sol
      integer ix_hdl_c_aux_sol
 
      integer ix_blk_ml_prsl_sv
      integer ix_hdl_prsl_sv_ml_lbc
      integer ix_hdl_prsl_sv_ml_ubc
      integer ix_hdl_prsl_sv_ml_cbp
      integer ix_hdl_prsl_sv_mtx_r_v
      integer ix_hdl_prsl_sv_mtx_r_ix
      integer ix_hdl_prsl_sv_mtx_c_sa
      integer ix_hdl_prsl_rm_vr_ls
      integer ix_hdl_prsl_rm_vr_st
      integer ix_hdl_prsl_rm_vr_pr_act
 
      integer ix_blk_ml_rg_da
      integer ix_hdl_co_rg_up_co_v
      integer ix_hdl_co_rg_lo_co_v
      integer ix_hdl_co_rg_up_ob_v
      integer ix_hdl_co_rg_lo_ob_v
      integer ix_hdl_co_rg_up_act_v
      integer ix_hdl_co_rg_lo_act_v
      integer ix_hdl_co_rg_up_en_vr
      integer ix_hdl_co_rg_lo_en_vr
      integer ix_hdl_co_rg_up_lv_vr
      integer ix_hdl_co_rg_lo_lv_vr
      integer ix_hdl_bd_rg_up_bd_v
      integer ix_hdl_bd_rg_lo_bd_v
      integer ix_hdl_bd_rg_up_ob_v
      integer ix_hdl_bd_rg_lo_ob_v
      integer ix_hdl_bd_rg_up_en_vr
      integer ix_hdl_bd_rg_lo_en_vr
      integer ix_hdl_bd_rg_up_lv_vr
      integer ix_hdl_bd_rg_lo_lv_vr
 
      integer ix_blk_ml_u_bs
      integer ix_hdl_u_bs_dse_blk
      integer ix_hdl_u_bs_gthr_pv_r
      integer ix_hdl_u_bs_dse_blk_pv_r_in_c
      integer ix_hdl_u_bs_skt_pv_r
      integer ix_hdl_u_bs_eta_msk
 
      integer ix_blk_ml_usr_c_mtx
      integer ix_hdl_usr_mtx_r_v
      integer ix_hdl_usr_mtx_r_ix
      integer ix_hdl_usr_mtx_c_sa
 
      integer ix_blk_ml_usr_r_mtx
      integer ix_hdl_usr_mtx_c_v
      integer ix_hdl_usr_mtx_c_ix
      integer ix_hdl_usr_mtx_r_sa
 
      integer ix_eta_fi_hdl
      integer eta_fi_mx_n_eta_grp, ln_ml_hdl, n_ml_hdl
 
      parameter (ix_hdl_lbc =     0)
      parameter (ix_blk_ml_vec =  ix_hdl_lbc + hdl_os_blk_n)
      parameter (ix_hdl_ubc =     ix_hdl_lbc + hdl_z)
      parameter (ix_hdl_cbp =     ix_hdl_ubc + hdl_z)
      parameter (ix_hdl_nm =      ix_hdl_cbp + hdl_z)
 
      parameter (ix_hdl_lng_nm =  ix_hdl_nm + hdl_z)
      parameter (ix_blk_ml_lng_nm =  ix_hdl_lng_nm + hdl_os_blk_n)
 
      parameter (ix_hdl_pk_bs_os =  ix_hdl_lng_nm + hdl_z)
      parameter (ix_blk_ml_pk_bs_os =  ix_hdl_pk_bs_os + hdl_os_blk_n)
 
      parameter (ix_hdl_pwl_vr_rf_pr_act_v = ix_hdl_pk_bs_os + hdl_z)
      parameter (ix_blk_ml_pwl_vr =
     &     ix_hdl_pwl_vr_rf_pr_act_v + hdl_os_blk_n)
      parameter (ix_hdl_pwl_vr_rf_ob_fn_v =
     &     ix_hdl_pwl_vr_rf_pr_act_v + hdl_z)
      parameter (ix_hdl_pwl_vr_da_v =  ix_hdl_pwl_vr_rf_ob_fn_v + hdl_z)
      parameter (ix_hdl_pwl_vr_ls =    ix_hdl_pwl_vr_da_v + hdl_z)
      parameter (ix_hdl_pwl_vr_da_sa = ix_hdl_pwl_vr_ls + hdl_z)
      parameter (ix_hdl_pwl_vr_cu_sn = ix_hdl_pwl_vr_da_sa + hdl_z)
      parameter (ix_hdl_pwl_vr_ix =    ix_hdl_pwl_vr_cu_sn + hdl_z)
      parameter (ix_hdl_pwl_vr_usr_bp = ix_hdl_pwl_vr_ix + hdl_z)
      parameter (ix_hdl_pwl_vr_usr_ls = ix_hdl_pwl_vr_usr_bp + hdl_z)
      parameter (ix_hdl_pwl_vr_usr_sn = ix_hdl_pwl_vr_usr_ls + hdl_z)
      parameter (ix_hdl_pwl_vr_usr_da_sa = ix_hdl_pwl_vr_usr_sn + hdl_z)
      parameter (ix_hdl_pwl_vr_usr_cu_sn =
     &     ix_hdl_pwl_vr_usr_da_sa + hdl_z)
 
      parameter (ix_hdl_scl =     ix_hdl_pwl_vr_usr_cu_sn + hdl_z)
      parameter (ix_blk_ml_sol =  ix_hdl_scl + hdl_os_blk_n)
      parameter (ix_hdl_pr_act =  ix_hdl_scl + hdl_z)
      parameter (ix_hdl_du_act =  ix_hdl_pr_act + hdl_z)
      parameter (ix_hdl_ed_wt =   ix_hdl_du_act + hdl_z)
      parameter (ix_hdl_st =      ix_hdl_ed_wt + hdl_z)
 
      parameter (ix_hdl_vr_in_r =  ix_hdl_st + hdl_z)
      parameter (ix_blk_ml_vr_ls = ix_hdl_vr_in_r + hdl_os_blk_n)
      parameter (ix_hdl_vr_in_c =  ix_hdl_vr_in_r + hdl_z)
 
      parameter (ix_hdl_lo_eta_pv_in_r = ix_hdl_vr_in_c + hdl_z)
      parameter (ix_blk_ml_bs_inv_p =
     &     ix_hdl_lo_eta_pv_in_r + hdl_os_blk_n)
      parameter (ix_hdl_up_eta_pv_in_r =  ix_hdl_lo_eta_pv_in_r + hdl_z)
      parameter (ix_hdl_eta_w_l_en_in_r = ix_hdl_up_eta_pv_in_r + hdl_z)
      parameter (ix_hdl_eta_w_lm1_en_in_r =
     &     ix_hdl_eta_w_l_en_in_r + hdl_z)
      parameter (ix_hdl_og_t_nw_perm = ix_hdl_eta_w_lm1_en_in_r + hdl_z)
      parameter (ix_hdl_nw_t_og_perm =   ix_hdl_og_t_nw_perm + hdl_z)
 
      parameter (ix_hdl_dvx_ix =
     &     ix_hdl_nw_t_og_perm + hdl_z)
      parameter (ix_blk_dvx =   ix_hdl_dvx_ix + hdl_os_blk_n)
 
      parameter (ix_hdl_mtx_r_v = ix_hdl_dvx_ix + hdl_z)
      parameter (ix_blk_ml_c_mtx = ix_hdl_mtx_r_v + hdl_os_blk_n)
      parameter (ix_hdl_mtx_r_ix = ix_hdl_mtx_r_v + hdl_z)
      parameter (ix_hdl_mtx_c_sa = ix_hdl_mtx_r_ix + hdl_z)
      parameter (ix_hdl_mtx_c_ln = ix_hdl_mtx_c_sa + hdl_z)
 
      parameter (ix_hdl_mtx_c_v =  ix_hdl_mtx_c_ln + hdl_z)
      parameter (ix_blk_ml_r_mtx = ix_hdl_mtx_c_v + hdl_os_blk_n)
      parameter (ix_hdl_mtx_c_ix = ix_hdl_mtx_c_v + hdl_z)
      parameter (ix_hdl_mtx_r_sa = ix_hdl_mtx_c_ix + hdl_z)
 
      parameter (ix_hdl_aux_blk_v = ix_hdl_mtx_r_sa + hdl_z)
      parameter (ix_blk_ml_aux_blk =   ix_hdl_aux_blk_v + hdl_os_blk_n)
      parameter (ix_hdl_aux_blk_r_ix = ix_hdl_aux_blk_v + hdl_z)
      parameter (ix_hdl_aux_blk_c_ix = ix_hdl_aux_blk_r_ix + hdl_z)
 
      parameter (ix_hdl_r_aux_sol = ix_hdl_aux_blk_c_ix + hdl_z)
      parameter (ix_blk_ml_aux_sol = ix_hdl_r_aux_sol + hdl_os_blk_n)
      parameter (ix_hdl_c_aux_sol =  ix_hdl_r_aux_sol + hdl_z)
 
      parameter (ix_hdl_prsl_sv_ml_lbc = ix_hdl_c_aux_sol + hdl_z)
      parameter (ix_blk_ml_prsl_sv = ix_hdl_prsl_sv_ml_lbc +
     &     hdl_os_blk_n)
      parameter (ix_hdl_prsl_sv_ml_ubc =  ix_hdl_prsl_sv_ml_lbc + hdl_z)
      parameter (ix_hdl_prsl_sv_ml_cbp =  ix_hdl_prsl_sv_ml_ubc + hdl_z)
      parameter (ix_hdl_prsl_sv_mtx_r_v = ix_hdl_prsl_sv_ml_cbp + hdl_z)
      parameter (ix_hdl_prsl_sv_mtx_r_ix =
     &     ix_hdl_prsl_sv_mtx_r_v + hdl_z)
      parameter (ix_hdl_prsl_sv_mtx_c_sa =
     &     ix_hdl_prsl_sv_mtx_r_ix + hdl_z)
      parameter (ix_hdl_prsl_rm_vr_ls =
     &     ix_hdl_prsl_sv_mtx_c_sa + hdl_z)
      parameter (ix_hdl_prsl_rm_vr_st =
     &     ix_hdl_prsl_rm_vr_ls + hdl_z)
      parameter (ix_hdl_prsl_rm_vr_pr_act =
     &     ix_hdl_prsl_rm_vr_st + hdl_z)
 
      parameter (ix_hdl_co_rg_up_co_v =
     &     ix_hdl_prsl_rm_vr_pr_act + hdl_z)
      parameter (ix_blk_ml_rg_da = ix_hdl_co_rg_up_co_v + hdl_os_blk_n)
      parameter (ix_hdl_co_rg_lo_co_v = ix_hdl_co_rg_up_co_v + hdl_z)
      parameter (ix_hdl_co_rg_up_ob_v = ix_hdl_co_rg_lo_co_v + hdl_z)
      parameter (ix_hdl_co_rg_lo_ob_v = ix_hdl_co_rg_up_ob_v + hdl_z)
      parameter (ix_hdl_co_rg_up_act_v = ix_hdl_co_rg_lo_ob_v + hdl_z)
      parameter (ix_hdl_co_rg_lo_act_v = ix_hdl_co_rg_up_act_v + hdl_z)
      parameter (ix_hdl_co_rg_up_en_vr = ix_hdl_co_rg_lo_act_v + hdl_z)
      parameter (ix_hdl_co_rg_lo_en_vr = ix_hdl_co_rg_up_en_vr + hdl_z)
      parameter (ix_hdl_co_rg_up_lv_vr = ix_hdl_co_rg_lo_en_vr + hdl_z)
      parameter (ix_hdl_co_rg_lo_lv_vr = ix_hdl_co_rg_up_lv_vr + hdl_z)
      parameter (ix_hdl_bd_rg_up_bd_v = ix_hdl_co_rg_lo_lv_vr + hdl_z)
      parameter (ix_hdl_bd_rg_lo_bd_v = ix_hdl_bd_rg_up_bd_v + hdl_z)
      parameter (ix_hdl_bd_rg_up_ob_v = ix_hdl_bd_rg_lo_bd_v + hdl_z)
      parameter (ix_hdl_bd_rg_lo_ob_v = ix_hdl_bd_rg_up_ob_v + hdl_z)
      parameter (ix_hdl_bd_rg_up_en_vr = ix_hdl_bd_rg_lo_ob_v + hdl_z)
      parameter (ix_hdl_bd_rg_lo_en_vr = ix_hdl_bd_rg_up_en_vr + hdl_z)
      parameter (ix_hdl_bd_rg_up_lv_vr = ix_hdl_bd_rg_lo_en_vr + hdl_z)
      parameter (ix_hdl_bd_rg_lo_lv_vr = ix_hdl_bd_rg_up_lv_vr + hdl_z)
 
      parameter (ix_hdl_u_bs_dse_blk = ix_hdl_bd_rg_lo_lv_vr + hdl_z)
      parameter (ix_blk_ml_u_bs = ix_hdl_u_bs_dse_blk + hdl_os_blk_n)
      parameter (ix_hdl_u_bs_gthr_pv_r = ix_hdl_u_bs_dse_blk + hdl_z)
      parameter (ix_hdl_u_bs_dse_blk_pv_r_in_c =
     &     ix_hdl_u_bs_gthr_pv_r + hdl_z)
      parameter (ix_hdl_u_bs_skt_pv_r =
     &     ix_hdl_u_bs_dse_blk_pv_r_in_c + hdl_z)
      parameter (ix_hdl_u_bs_eta_msk = ix_hdl_u_bs_skt_pv_r + hdl_z)
 
      parameter (ix_hdl_usr_mtx_r_v = ix_hdl_u_bs_eta_msk + hdl_z)
      parameter (ix_blk_ml_usr_c_mtx = ix_hdl_usr_mtx_r_v +
     &     hdl_os_blk_n)
      parameter (ix_hdl_usr_mtx_r_ix = ix_hdl_usr_mtx_r_v + hdl_z)
      parameter (ix_hdl_usr_mtx_c_sa = ix_hdl_usr_mtx_r_ix + hdl_z)
 
      parameter (ix_hdl_usr_mtx_c_v = ix_hdl_usr_mtx_c_sa + hdl_z)
      parameter (ix_blk_ml_usr_r_mtx = ix_hdl_usr_mtx_c_v +
     &     hdl_os_blk_n)
      parameter (ix_hdl_usr_mtx_c_ix = ix_hdl_usr_mtx_c_v + hdl_z)
      parameter (ix_hdl_usr_mtx_r_sa = ix_hdl_usr_mtx_c_ix + hdl_z)
 
      parameter (ix_eta_fi_hdl = ix_hdl_usr_mtx_r_sa + hdl_z)
      parameter (eta_fi_mx_n_eta_grp = 4)
      parameter (ln_ml_hdl = ix_eta_fi_hdl + eta_fi_mx_n_eta_grp*hdl_z)
      parameter (n_ml_hdl = ln_ml_hdl/hdl_z)
 
      integer ml_vec_blk_id
      integer ml_lng_nm_blk_id
      integer ml_pk_bs_os_blk_id
      integer ml_sol_blk_id
      integer ml_c_mtx_blk_id
      integer ml_r_mtx_blk_id
      integer ml_aux_sol_blk_id
      integer ml_aux_blk_blk_id
      integer ml_prsl_sv_blk_id
      integer ml_prsl_wk_blk_id
      integer ml_usr_c_mtx_blk_id
      integer ml_usr_r_mtx_blk_id
      integer ml_vr_ls_blk_id
      integer ml_bs_inv_p_blk_id
      integer ml_hsh_a_blk_id
      integer ml_pwl_vr_blk_id
      integer ml_rg_da_blk_id
      integer rsmi_blk_id
      integer rsmi_wk_blk_id
      integer rsmi_bd_blk_id
      integer eta_grp_blk_id
      integer crsh_blk_id
      integer dvx_blk_id
      integer l1_cz_r_blk_id
      integer r_eta_fi_p_blk_id
      integer r_eta_fi_en_blk_id
      integer mps_blk_id
      integer parsmi_blk_id
      integer parsmi_inv_blk_id
      integer u_bs_blk_id
      integer tom_inv_wk_blk_id
 
      common/ems_blk_id_com/
     &     ml_vec_blk_id, ml_lng_nm_blk_id, ml_pk_bs_os_blk_id,
     &     ml_sol_blk_id, ml_c_mtx_blk_id,
     &     ml_r_mtx_blk_id,
     &     ml_prsl_sv_blk_id, ml_prsl_wk_blk_id,
     &     ml_aux_sol_blk_id, ml_aux_blk_blk_id,
     &     ml_usr_c_mtx_blk_id, ml_usr_r_mtx_blk_id,
     &     ml_vr_ls_blk_id, ml_bs_inv_p_blk_id,
     &     ml_hsh_a_blk_id, ml_pwl_vr_blk_id, ml_rg_da_blk_id,
     &     rsmi_blk_id, rsmi_wk_blk_id, rsmi_bd_blk_id,
     &     eta_grp_blk_id, crsh_blk_id,
     &     dvx_blk_id, l1_cz_r_blk_id, r_eta_fi_p_blk_id,
     &     r_eta_fi_en_blk_id, mps_blk_id,
     &     parsmi_blk_id, parsmi_inv_blk_id,
     &     u_bs_blk_id, tom_inv_wk_blk_id
 
      integer ct_vr_n_wo, is_bs_blk_n_wo
      parameter (
     &     ct_vr_n_wo =         584,
     &     is_bs_blk_n_wo =     ct_vr_n_wo)
 
      integer ml_bs_blk_os_ca_dscm_fg, ml_bs_blk_os_ca_ptmi_fg
      integer ml_bs_blk_n_fg, ml_bs_blk_tru_n_wo, ml_bs_blk_n_wo
      integer ml_bs_blk_os_ct_vr, ml_bs_blk_os_hdl
      parameter (
     &     ml_bs_blk_os_ca_dscm_fg = 0,
     &     ml_bs_blk_os_ca_ptmi_fg = 1,
     &     ml_bs_blk_n_fg =     ml_bs_blk_os_ca_ptmi_fg + 1,
     &     ml_bs_blk_os_ct_vr = ml_bs_blk_n_fg,
     &     ml_bs_blk_os_hdl =   ml_bs_blk_os_ct_vr + ct_vr_n_wo,
     &     ml_bs_blk_tru_n_wo = ml_bs_blk_os_hdl + ln_ml_hdl + 1,
     &     ml_bs_blk_n_wo = 2*((ml_bs_blk_tru_n_wo+1)/2))
c
c     Ensure that there is an even number of flags.
c
c     The expression 2*((ml_bs_blk_tru_n_wo+1)/2) is used to make
c     ml_bs_blk_n_wo even so that ml_bs_blk's finish just before a
c     doubleword boundary (since they start at one).
c
 
c
c     These are the hard-coded parameters for pointers to flags and
c     data at the beginning of is.
c
c     NB:
c     cp_o_mem_mgr_da_l_is_en should be identical to mem_mgr_da_l_is_en
c     in EMSMMGR.INC: a copy is made here to avoid having to include
c     EMSMMGRI.INC just to get this value.
c
      integer cp_o_mem_mgr_da_l_is_en
      integer ix_ca_init_fg1
      integer ix_ca_init_fg2
      integer ix_is_n, ix_ca_dsca, ix_n_ml, ix_ca_dscm
      integer ix_cu_ca_ems_rn_fg1, ix_cu_ca_ems_rn_fg2
      parameter (
     &     cp_o_mem_mgr_da_l_is_en =   33,
     &     ix_ca_init_fg1 =      cp_o_mem_mgr_da_l_is_en + 1,
     &     ix_ca_init_fg2 =      ix_ca_init_fg1 + 1,
     &     ix_is_n =             ix_ca_init_fg2 + 1,
     &     ix_ca_dsca =          ix_is_n + 1,
     &     ix_n_ml =             ix_ca_dsca + 1,
     &     ix_ca_dscm =          ix_n_ml + 1,
     &     ix_cu_ca_ems_rn_fg1 = ix_ca_dscm + 1,
     &     ix_cu_ca_ems_rn_fg2 = ix_cu_ca_ems_rn_fg1 + 1)
c
c     16/11/95: ix_ca_dscm =          36
c     05/05/97: ix_cu_ca_ems_rn =     37
c     21/05/97: ix_cu_ca_ems_rn =     36
c     07/07/97: ix_cu_ca_ems_rn_fg2 = 38
c     06/06/98: ix_cu_ca_ems_rn_fg2 = 39
c     08/06/98: ix_cu_ca_ems_rn_fg2 = 41
c
      integer p_is_bs_blk, p_ml_bs_blk
      parameter (
     &     p_is_bs_blk = 50,
     &     p_ml_bs_blk = p_is_bs_blk + is_bs_blk_n_wo)
c
c     16/11/95: p_ml_bs_blk =  394
c     08/06/98: p_ml_bs_blk =  404
c
