      integer mo_rsmi_msk_rp_af_bt
      integer mo_rsmi_msk_rp_run_bt
      integer mo_rsmi_rec_inv_no_std
      integer mo_rsmi_rec_inv_std
      parameter (
     &     mo_rsmi_msk_rp_af_bt = bt1,
     &     mo_rsmi_msk_rp_run_bt = bt2)
      parameter (
     &     mo_rsmi_rec_inv_no_std = 0,
     &     mo_rsmi_rec_inv_std = 1)
 
      integer ca_iz_mo_rsmi_fg1, ca_iz_mo_rsmi_fg2
      integer mo_rsmi_rp_cn, mo_rsmi_rp_msk, mo_rsmi_rec_inv
      logical mo_rsmi_rec
      common/ems_mo_rsmi_ct_vr_com/
     &     ca_iz_mo_rsmi_fg1, ca_iz_mo_rsmi_fg2,
     &     mo_rsmi_rp_cn, mo_rsmi_rp_msk, mo_rsmi_rec_inv,
     &     mo_rsmi_rec
 
      integer mo_rsmi_nw_dvx_fwk_mx_n_rec
      parameter (mo_rsmi_nw_dvx_fwk_mx_n_rec = 100)
      integer nw_dvx_fwk_rec_n_si_it
      integer nw_dvx_fwk_rec_n_dvx_it
      integer nw_dvx_fwk_rec_dvx_it_lm
      integer nw_dvx_fwk_i_rec_z
      parameter (
     &     nw_dvx_fwk_rec_n_si_it = 1,
     &     nw_dvx_fwk_rec_n_dvx_it = nw_dvx_fwk_rec_n_si_it + 1,
     &     nw_dvx_fwk_rec_dvx_it_lm = nw_dvx_fwk_rec_n_dvx_it + 1,
     &     nw_dvx_fwk_i_rec_z = nw_dvx_fwk_rec_dvx_it_lm)
      integer nw_dvx_fwk_rec_dvx_rao
      integer nw_dvx_fwk_rec_dvx_rao_lm
      integer nw_dvx_fwk_rl_rec_z
      parameter (
     &     nw_dvx_fwk_rec_dvx_rao = 1,
     &     nw_dvx_fwk_rec_dvx_rao_lm = nw_dvx_fwk_rec_dvx_rao + 1,
     &     nw_dvx_fwk_rl_rec_z = nw_dvx_fwk_rec_dvx_rao_lm)
 
      integer mo_rsmi_inv_mx_n_rec
      parameter (mo_rsmi_inv_mx_n_rec = 100)
      integer inv_rec_why
      integer inv_rec_n_si_it
      integer inv_rec_pre_n_eta
      integer inv_rec_pre_n_ix
      integer inv_rec_n_a_en
      integer inv_rec_n_eta
      integer inv_rec_n_ix
      integer inv_i_rec_z
      integer inv_rec_av_eta_dse
      integer inv_rl_rec_z
      parameter (
     &     inv_rec_why = 1,
     &     inv_rec_n_si_it = inv_rec_why + 1,
     &     inv_rec_pre_n_eta = inv_rec_n_si_it + 1,
     &     inv_rec_pre_n_ix = inv_rec_pre_n_eta + 1,
     &     inv_rec_n_a_en = inv_rec_pre_n_ix + 1,
     &     inv_rec_n_eta = inv_rec_n_a_en + 1,
     &     inv_rec_n_ix = inv_rec_n_eta + 1,
     &     inv_i_rec_z = inv_rec_n_ix)
      parameter (
     &     inv_rec_av_eta_dse = 1,
     &     inv_rl_rec_z = inv_rec_av_eta_dse)
 
      integer mo_rsmi_sing_bs_mx_n_rec
      parameter (mo_rsmi_sing_bs_mx_n_rec = 100)
      integer sing_bs_rec_n_si_it
      integer sing_bs_rec_n_inv_sing
      integer sing_bs_rec_z
      parameter (
     &     sing_bs_rec_n_si_it = 1,
     &     sing_bs_rec_n_inv_sing = sing_bs_rec_n_si_it + 1,
     &     sing_bs_rec_z = sing_bs_rec_n_inv_sing)
 
      integer mo_rsmi_reset_mx_n_rec
      parameter (mo_rsmi_reset_mx_n_rec = 100)
      integer reset_why_sing_bs
      integer reset_why_op
      parameter (
     &     reset_why_sing_bs = 0,
     &     reset_why_op = 1)
      integer reset_rec_why
      integer reset_rec_n_si_it
      integer reset_rec_z
      parameter (
     &     reset_rec_why = 1,
     &     reset_rec_n_si_it = reset_rec_why + 1,
     &     reset_rec_z = reset_rec_n_si_it)
 
      integer mo_rsmi_re_pc_mx_n_rec
      parameter (mo_rsmi_re_pc_mx_n_rec = 100)
      integer re_pc_rec_why
      integer re_pc_rec_n_si_it
      integer re_pc_rec_z
      parameter (
     &     re_pc_rec_why = 1,
     &     re_pc_rec_n_si_it = re_pc_rec_why + 1,
     &     re_pc_rec_z = re_pc_rec_n_si_it)
 
      integer mo_rsmi_re_rhs_mx_n_rec
      parameter (mo_rsmi_re_rhs_mx_n_rec = 100)
      integer rq_re_rhs_no_rq_re_rhs
      integer rq_re_rhs_reset
      parameter (
     &     rq_re_rhs_no_rq_re_rhs = 0,
     &     rq_re_rhs_reset = 1)
      integer re_rhs_rec_why
      integer re_rhs_rec_n_si_it
      integer re_rhs_rec_z
      parameter (
     &     re_rhs_rec_why = 1,
     &     re_rhs_rec_n_si_it = re_rhs_rec_why + 1,
     &     re_rhs_rec_z = re_rhs_rec_n_si_it)
 
      integer mo_rsmi_pv_er_mx_n_rec
      parameter (mo_rsmi_pv_er_mx_n_rec = 100)
      integer pv_er_rec_n_si_it
      integer pv_er_i_rec_z
      parameter (
     &     pv_er_rec_n_si_it = 1,
     &     pv_er_i_rec_z = pv_er_rec_n_si_it)
      integer pv_er_rec_pv
      integer pv_er_rec_rcp_alt_pv
      integer pv_er_rec_pv_er
      integer pv_er_rl_rec_z
      parameter (
     &     pv_er_rec_pv = 1,
     &     pv_er_rec_rcp_alt_pv = pv_er_rec_pv + 1,
     &     pv_er_rec_pv_er = pv_er_rec_rcp_alt_pv + 1,
     &     pv_er_rl_rec_z = pv_er_rec_pv_er)
 
      integer mo_rsmi_du_act_er_mx_n_rec
      parameter (mo_rsmi_du_act_er_mx_n_rec = 100)
      integer du_act_er_why_er
      integer du_act_er_why_wg_sgn
      parameter (
     &     du_act_er_why_er = 0,
     &     du_act_er_why_wg_sgn = 1)
      integer du_act_er_rec_why
      integer du_act_er_rec_n_si_it
      integer du_act_er_i_rec_z
      parameter (
     &     du_act_er_rec_why = 1,
     &     du_act_er_rec_n_si_it = du_act_er_rec_why + 1,
     &     du_act_er_i_rec_z = du_act_er_rec_n_si_it)
      integer du_act_er_rec_du_act
      integer du_act_er_rec_alt_du_act
      integer du_act_er_rec_du_act_er
      integer du_act_er_rl_rec_z
      parameter (
     &     du_act_er_rec_du_act = 1,
     &     du_act_er_rec_alt_du_act = du_act_er_rec_du_act + 1,
     &     du_act_er_rec_du_act_er = du_act_er_rec_alt_du_act + 1,
     &     du_act_er_rl_rec_z = du_act_er_rec_du_act_er)
 
      integer mo_rsmi_growth_mx_n_rec
      parameter (mo_rsmi_growth_mx_n_rec = 1000)
      integer growth_act_rp
      integer growth_act_no
      integer growth_act_refine_pv_c
      integer growth_act_re_inv
      integer growth_act_cg_tl
      integer growth_act_mv_bd
      integer growth_act_cg_act
      integer growth_act_nr_growth
      integer growth_f_act_ty
      integer growth_l_act_ty
      parameter (
     &     growth_act_rp =          0,
     &     growth_act_no =          1,
     &     growth_act_refine_pv_c = 2,
     &     growth_act_re_inv =      3,
     &     growth_act_cg_tl =       4,
     &     growth_act_mv_bd =       5,
     &     growth_act_cg_act =      6,
     &     growth_act_nr_growth =   7,
     &     growth_f_act_ty =        0,
     &     growth_l_act_ty =        growth_act_nr_growth)
      integer growth_rec_act
      integer growth_rec_n_si_it
      integer growth_rec_pv_r_n
      integer growth_rec_vr_n
      integer growth_rec_i_v
      integer growth_i_rec_z
      integer growth_rec_pv
      integer growth_rec_aa
      integer growth_rec_growth
      integer growth_rec_rl_v
      integer growth_rl_rec_z
      parameter (
     &     growth_rec_act = 1,
     &     growth_rec_n_si_it = growth_rec_act + 1,
     &     growth_rec_pv_r_n = growth_rec_n_si_it + 1,
     &     growth_rec_vr_n = growth_rec_pv_r_n + 1,
     &     growth_rec_i_v = growth_rec_vr_n + 1,
     &     growth_i_rec_z = growth_rec_i_v)
      parameter (
     &     growth_rec_pv = 1,
     &     growth_rec_aa = growth_rec_pv + 1,
     &     growth_rec_growth = growth_rec_aa + 1,
     &     growth_rec_rl_v = growth_rec_growth + 1,
     &     growth_rl_rec_z = growth_rec_rl_v)
 
      integer mo_rsmi_n_nw_dvx_fwk, mo_rsmi_nw_dvx_fwk_n_rec
      integer mo_rsmi_n_inv, mo_rsmi_inv_n_rec
      integer mo_rsmi_n_reset, mo_rsmi_reset_n_rec
      integer mo_rsmi_n_re_pc, mo_rsmi_re_pc_n_rec
      integer mo_rsmi_n_re_rhs, mo_rsmi_re_rhs_n_rec
      integer mo_rsmi_n_pv_er, mo_rsmi_pv_er_n_rec
      integer mo_rsmi_n_du_act_er, mo_rsmi_du_act_er_n_rec
      integer mo_rsmi_n_sing_bs, mo_rsmi_sing_bs_n_rec
      integer mo_rsmi_n_growth, mo_rsmi_growth_n_rec
      common/ems_mo_rsmi_n_rec_com/
     &     mo_rsmi_n_nw_dvx_fwk, mo_rsmi_nw_dvx_fwk_n_rec,
     &     mo_rsmi_n_inv, mo_rsmi_inv_n_rec,
     &     mo_rsmi_n_reset, mo_rsmi_reset_n_rec,
     &     mo_rsmi_n_re_pc, mo_rsmi_re_pc_n_rec,
     &     mo_rsmi_n_re_rhs, mo_rsmi_re_rhs_n_rec,
     &     mo_rsmi_n_pv_er, mo_rsmi_pv_er_n_rec,
     &     mo_rsmi_n_du_act_er, mo_rsmi_du_act_er_n_rec,
     &     mo_rsmi_n_sing_bs, mo_rsmi_sing_bs_n_rec,
     &     mo_rsmi_n_growth, mo_rsmi_growth_n_rec
 
      logical mo_rsmi_nw_dvx_fwk_xa_rec
      logical mo_rsmi_inv_xa_rec
      logical mo_rsmi_reset_xa_rec
      logical mo_rsmi_re_pc_xa_rec
      logical mo_rsmi_re_rhs_xa_rec
      logical mo_rsmi_pv_er_xa_rec
      logical mo_rsmi_du_act_er_xa_rec
      logical mo_rsmi_sing_bs_xa_rec
      logical mo_rsmi_growth_xa_rec
      common/ems_mo_rsmi_xa_rec_com/
     &     mo_rsmi_nw_dvx_fwk_xa_rec,
     &     mo_rsmi_inv_xa_rec,
     &     mo_rsmi_reset_xa_rec,
     &     mo_rsmi_re_pc_xa_rec,
     &     mo_rsmi_re_rhs_xa_rec,
     &     mo_rsmi_pv_er_xa_rec,
     &     mo_rsmi_du_act_er_xa_rec,
     &     mo_rsmi_sing_bs_xa_rec,
     &     mo_rsmi_growth_xa_rec
 
      integer mo_rsmi_nw_dvx_fwk_i_rec(nw_dvx_fwk_i_rec_z,
     &     mo_rsmi_nw_dvx_fwk_mx_n_rec)
      double precision mo_rsmi_nw_dvx_fwk_rl_rec(nw_dvx_fwk_rl_rec_z,
     &     mo_rsmi_nw_dvx_fwk_mx_n_rec)
      integer mo_rsmi_inv_i_rec(inv_i_rec_z,
     &     mo_rsmi_inv_mx_n_rec)
      double precision mo_rsmi_inv_rl_rec(inv_rl_rec_z,
     &     mo_rsmi_inv_mx_n_rec)
      integer mo_rsmi_reset_rec(reset_rec_z,
     &     mo_rsmi_reset_mx_n_rec)
      integer mo_rsmi_re_pc_rec(re_pc_rec_z,
     &     mo_rsmi_re_pc_mx_n_rec)
      integer mo_rsmi_re_rhs_rec(re_rhs_rec_z,
     &     mo_rsmi_re_rhs_mx_n_rec)
      integer mo_rsmi_pv_er_i_rec(pv_er_i_rec_z,
     &     mo_rsmi_pv_er_mx_n_rec)
      double precision mo_rsmi_pv_er_rl_rec(pv_er_rl_rec_z,
     &     mo_rsmi_pv_er_mx_n_rec)
      integer mo_rsmi_du_act_er_i_rec(du_act_er_i_rec_z,
     &     mo_rsmi_du_act_er_mx_n_rec)
      double precision mo_rsmi_du_act_er_rl_rec(du_act_er_rl_rec_z,
     &     mo_rsmi_du_act_er_mx_n_rec)
      integer mo_rsmi_growth_i_rec(growth_i_rec_z,
     &     mo_rsmi_growth_mx_n_rec)
      double precision mo_rsmi_growth_rl_rec(growth_rl_rec_z,
     &     mo_rsmi_growth_mx_n_rec)
      integer mo_rsmi_sing_bs_rec(sing_bs_rec_z,
     &     mo_rsmi_sing_bs_mx_n_rec)
      common/ems_mo_rsmi_rec_com/
     &     mo_rsmi_nw_dvx_fwk_rl_rec,
     &     mo_rsmi_pv_er_rl_rec,
     &     mo_rsmi_du_act_er_rl_rec,
     &     mo_rsmi_growth_rl_rec,
     &     mo_rsmi_inv_rl_rec,
     &     mo_rsmi_nw_dvx_fwk_i_rec,
     &     mo_rsmi_inv_i_rec,
     &     mo_rsmi_reset_rec,
     &     mo_rsmi_re_pc_rec,
     &     mo_rsmi_re_rhs_rec,
     &     mo_rsmi_pv_er_i_rec,
     &     mo_rsmi_du_act_er_i_rec,
     &     mo_rsmi_growth_i_rec,
     &     mo_rsmi_sing_bs_rec
 
