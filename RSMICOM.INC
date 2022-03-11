CM
      double precision mx_pv_c_v, pv, aa
      double precision du_act_o_vr_t_en_bs
      double precision ph_1_du_act_o_vr_t_en_bs
      double precision ed_wt_o_vr_t_en_bs
      double precision pr_co_mu
      double precision pr_co_scl_v
      double precision btran_sol_dse
      double precision ftran_sol_dse
      double precision tbu_r_dse
      integer nw_eta_f_ix, nw_eta_l_ix, mv_dir, lp_ph, pv_c_n, pv_r_n
      integer vr_t_en_bs, vr_t_lv_bs, nx_vr_t_en_bs
      integer pre_eta_fi_n_eta, pre_eta_fi_n_ix
      integer en_vr_bk_bd
      integer en_vr_fm_bs_cg_st
      integer rsmi_da_st_msk
      integer rsmi_op_st_msk
      logical un_bd, alg_er, nw_dvx_fwk, bd_swp
      logical fresh_pc, lp_ph_cg, u_du_act_o_vr_t_en_bs
      logical cg_pr_wt
      common/ems_com_rsmi/
     &     mx_pv_c_v, pv, aa,
     &     du_act_o_vr_t_en_bs,
     &     ph_1_du_act_o_vr_t_en_bs,
     &     ed_wt_o_vr_t_en_bs,
     &     pr_co_mu, pr_co_scl_v,
     &     btran_sol_dse, ftran_sol_dse, tbu_r_dse,
     &     nw_eta_f_ix, nw_eta_l_ix, mv_dir, lp_ph, pv_c_n, pv_r_n,
     &     vr_t_en_bs, vr_t_lv_bs, nx_vr_t_en_bs,
     &     pre_eta_fi_n_eta, pre_eta_fi_n_ix,
     &     en_vr_bk_bd,
     &     en_vr_fm_bs_cg_st,
     &     rsmi_da_st_msk,
     &     rsmi_op_st_msk,
     &     un_bd, alg_er, nw_dvx_fwk, bd_swp,
     &     fresh_pc, lp_ph_cg, u_du_act_o_vr_t_en_bs,
     &     cg_pr_wt
