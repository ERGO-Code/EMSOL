CM
      integer bs_cg_st_null
      integer bs_cg_st_bc
      integer bs_cg_st_fr_vr
      integer bs_cg_st_vr_bw_lb
      integer bs_cg_st_vr_at_lb
      integer bs_cg_st_vr_ab_lb
      integer bs_cg_st_vr_btw_bd
      integer bs_cg_st_vr_bw_ub
      integer bs_cg_st_vr_at_ub
      integer bs_cg_st_vr_ab_ub
      integer bs_cg_st_fx_vr_at_bd
      integer bs_cg_st_bp_vr_bw_bp
      integer bs_cg_st_bp_vr_at_bp
      integer bs_cg_st_bp_vr_ab_bp
      integer prev_bs_cg_st_fac
c
c     Use bs_cg_st_null in case setting a null basis change status to 0
c     causes problems later.
c
      parameter (bs_cg_st_null =         0)
      parameter (bs_cg_st_bc =           1)
      parameter (bs_cg_st_fr_vr =       12)
      parameter (bs_cg_st_vr_bw_lb =    10)
      parameter (bs_cg_st_vr_at_lb =    11)
      parameter (bs_cg_st_vr_ab_lb =    12)
      parameter (bs_cg_st_vr_btw_bd =   12)
      parameter (bs_cg_st_vr_bw_ub =    12)
      parameter (bs_cg_st_vr_at_ub =    13)
      parameter (bs_cg_st_vr_ab_ub =    14)
      parameter (bs_cg_st_fx_vr_at_bd = 12)
      parameter (bs_cg_st_bp_vr_bw_bp = 110)
      parameter (bs_cg_st_bp_vr_at_bp = 111)
      parameter (bs_cg_st_bp_vr_ab_bp = 112)
      parameter (prev_bs_cg_st_fac =    1000)
 
      integer rsmi_op_st_cz_c
      parameter (rsmi_op_st_cz_c = bt1)
 
      integer rsmi_da_st_bc_co
      parameter (rsmi_da_st_bc_co = bt1)
 
      integer st_ck_bt, bc_co_ck_bt
      integer pr_act_ck_bt, du_act_ck_bt
      integer inv_ck_bt, u_ck_bt, ed_wt_ck_bt, ze_a_ck_bt
c
      parameter (
     &     st_ck_bt =          bt1,
     &     bc_co_ck_bt =       bt2,
     &     pr_act_ck_bt =      bt3,
     &     du_act_ck_bt =      bt4,
     &     inv_ck_bt =         bt5,
     &     u_ck_bt =           bt6,
     &     ed_wt_ck_bt =       bt7,
     &     ze_a_ck_bt =        bt8)
 
      integer rsmi_lg_li_mode_fq
      integer rsmi_lg_li_mode_inv
      integer rsmi_lg_li_mode_dvx
      integer rsmi_lg_li_mode_reset
      parameter (
     &     rsmi_lg_li_mode_fq = 0,
     &     rsmi_lg_li_mode_inv = 1,
     &     rsmi_lg_li_mode_dvx = 2,
     &     rsmi_lg_li_mode_reset = 3)
 
      integer rsmi_li_bt
      integer rsmi_pc_li_bt, rsmi_inv_li_bt
      integer rsmi_pv_li_bt, rsmi_er_li_bt
      integer rsmi_dvx_li_bt, rsmi_u_li_bt
      parameter (
     &     rsmi_li_bt =        bt1,
     &     rsmi_pc_li_bt =     bt2,
     &     rsmi_inv_li_bt =    bt2,
     &     rsmi_pv_li_bt =     bt3,
     &     rsmi_dvx_li_bt =  bt4,
     &     rsmi_u_li_bt =      bt4,
     &     rsmi_er_li_bt =     bt5)
 
      integer wr_bc_co_bt, wr_vr_in_c_bt, wr_pr_act_bt, wr_du_act_bt
      integer wr_ed_wt_bt
      integer wr_pv_c_bt, wr_vr_in_r_bt
      integer wr_pi_rhs_bt, wr_pi_bt
      parameter (
     &     wr_pr_act_bt =  bt1,
     &     wr_du_act_bt =  bt2,
     &     wr_vr_in_r_bt = bt3,
     &     wr_vr_in_c_bt = bt4,
     &     wr_ed_wt_bt =   bt5,
     &     wr_bc_co_bt =   bt6,
     &     wr_pi_rhs_bt =  bt7,
     &     wr_pi_bt =      bt8,
     &     wr_pv_c_bt =    bt9)
 
      double precision xp_nu
      parameter (xp_nu = 1d-3)
 
 
