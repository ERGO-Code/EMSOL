CM
      integer prts_hdr_ty_std_vr
      integer prts_hdr_ty_bp_vr
      integer prts_hdr_ty_pwl_vr
      integer prts_hdr_ty_std_co_da
      integer prts_hdr_ty_bp_da
      integer prts_hdr_ty_co_rg_da
      integer prts_hdr_ty_bd_rg_da
      parameter (
     &     prts_hdr_ty_std_vr =    1,
     &     prts_hdr_ty_bp_vr =     2,
     &     prts_hdr_ty_pwl_vr =    3,
     &     prts_hdr_ty_std_co_da = 4,
     &     prts_hdr_ty_bp_da =     5,
     &     prts_hdr_ty_co_rg_da =  6,
     &     prts_hdr_ty_bd_rg_da =  7)
 
      integer wr_ml_da_ml_z_bt
      integer wr_ml_da_sol_st_bt
      integer wr_ml_da_nm_bt
      integer wr_ml_da_st_bt
      integer wr_ml_da_pr_act_bt
      integer wr_ml_da_du_act_bt
      integer wr_ml_da_lb_bt
      integer wr_ml_da_ub_bt
      integer wr_ml_da_co_bt
      integer wr_ml_da_mtx_bt
      parameter (
     &     wr_ml_da_ml_z_bt =   bt1,
     &     wr_ml_da_sol_st_bt = bt2,
     &     wr_ml_da_nm_bt =     bt3,
     &     wr_ml_da_st_bt =     bt4,
     &     wr_ml_da_pr_act_bt = bt5,
     &     wr_ml_da_du_act_bt = bt6,
     &     wr_ml_da_lb_bt =     bt7,
     &     wr_ml_da_ub_bt =     bt8,
     &     wr_ml_da_co_bt =     bt9,
     &     wr_ml_da_mtx_bt =    bt10)
      integer wr_ml_da_fu_li
      parameter (wr_ml_da_fu_li =
     &     wr_ml_da_nm_bt +     wr_ml_da_st_bt +
     &     wr_ml_da_pr_act_bt + wr_ml_da_du_act_bt +
     &     wr_ml_da_lb_bt +     wr_ml_da_ub_bt)
 
      integer wr_ml_st_al
      integer wr_ml_st_r_bt
      integer wr_ml_st_c_bt
      integer wr_ml_st_nz_bt
      integer wr_ml_st_ifs_bt
      integer wr_ml_st_bp_bt
      integer wr_ml_st_pwl_bt
      parameter (
     &     wr_ml_st_al =      0,
     &     wr_ml_st_r_bt =    bt1,
     &     wr_ml_st_c_bt =    bt2,
     &     wr_ml_st_nz_bt =   bt3,
     &     wr_ml_st_ifs_bt =  bt4,
     &     wr_ml_st_bp_bt =   bt5,
     &     wr_ml_st_pwl_bt =  bt6)
 
      integer wr_rg_da_co_rg_co_v
      integer wr_rg_da_co_rg_ob_v
      integer wr_rg_da_co_rg_en_vr
      integer wr_rg_da_co_rg_lv_vr
      integer wr_rg_da_bd_rg_co_v
      integer wr_rg_da_bd_rg_ob_v
      integer wr_rg_da_bd_rg_en_vr
      integer wr_rg_da_bd_rg_lv_vr
      integer wr_rg_da_bd_rg_r_da
      integer wr_rg_da_bd_rg_c_da
      parameter (
     &     wr_rg_da_co_rg_co_v =  bt1,
     &     wr_rg_da_co_rg_ob_v =  bt2,
     &     wr_rg_da_co_rg_en_vr = bt3,
     &     wr_rg_da_co_rg_lv_vr = bt4,
     &     wr_rg_da_bd_rg_co_v =  bt5,
     &     wr_rg_da_bd_rg_ob_v =  bt6,
     &     wr_rg_da_bd_rg_en_vr = bt7,
     &     wr_rg_da_bd_rg_lv_vr = bt8,
     &     wr_rg_da_bd_rg_r_da =  bt9,
     &     wr_rg_da_bd_rg_c_da =  bt10)
 
      integer prts_c_hd_ix_v
      integer prts_c_hd_ix_pr_act
      integer prts_c_hd_ix_lo_du_act
      integer prts_c_hd_ix_du_act
      integer prts_c_hd_ix_up_du_act
      integer prts_c_hd_ix_lb
      integer prts_c_hd_ix_co
      integer prts_c_hd_ix_ub
      integer prts_c_hd_ix_bp
      integer prts_c_hd_ix_lo_co
      integer prts_c_hd_ix_up_co
      integer prts_c_hd_ix_lo_bp
      integer prts_c_hd_ix_up_bp
      integer prts_c_hd_ix_og_ob
      integer prts_c_hd_ix_rg
      integer prts_c_hd_ix_ob_rg
      integer prts_c_hd_n_ix
 
      parameter (
     &     prts_c_hd_ix_v = 1,
     &     prts_c_hd_ix_pr_act = prts_c_hd_ix_v + 1,
     &     prts_c_hd_ix_lo_du_act = prts_c_hd_ix_pr_act + 1,
     &     prts_c_hd_ix_du_act = prts_c_hd_ix_lo_du_act + 1,
     &     prts_c_hd_ix_up_du_act = prts_c_hd_ix_du_act + 1,
     &     prts_c_hd_ix_lb = prts_c_hd_ix_up_du_act + 1,
     &     prts_c_hd_ix_co = prts_c_hd_ix_lb + 1,
     &     prts_c_hd_ix_ub = prts_c_hd_ix_co + 1,
     &     prts_c_hd_ix_bp = prts_c_hd_ix_ub + 1,
     &     prts_c_hd_ix_lo_co = prts_c_hd_ix_bp + 1,
     &     prts_c_hd_ix_up_co = prts_c_hd_ix_lo_co + 1,
     &     prts_c_hd_ix_lo_bp = prts_c_hd_ix_up_co + 1,
     &     prts_c_hd_ix_up_bp = prts_c_hd_ix_lo_bp + 1,
     &     prts_c_hd_ix_og_ob = prts_c_hd_ix_up_bp + 1,
     &     prts_c_hd_ix_rg =    prts_c_hd_ix_og_ob + 1,
     &     prts_c_hd_ix_ob_rg = prts_c_hd_ix_rg + 1,
     &     prts_c_hd_n_ix = prts_c_hd_ix_ob_rg)
 
      integer iz_prts_com_fg1, iz_prts_com_fg2
      character*6 ch6_prts_c_hd_st
      character*6 ch6_prts_c_hd_sn
      character*8 ch8_prts_c_hd_r
      character*8 ch8_prts_c_hd_c
      character*(ml_nm_mx_n_ch) mu_ch_prts_c_hd_nm
      character*(ml_nm_mx_n_ch) mu_ch_prts_c_hd_en_nm
      character*(ml_nm_mx_n_ch) mu_ch_prts_c_hd_lv_nm
      character*22 ch22_prts_c_hd(prts_c_hd_n_ix)
      common/ems_ch22_prts_c_hd_com/
     &     iz_prts_com_fg1, iz_prts_com_fg2,
     &     ch6_prts_c_hd_st,
     &     ch6_prts_c_hd_sn,
     &     ch8_prts_c_hd_r,
     &     ch8_prts_c_hd_c,
     &     mu_ch_prts_c_hd_nm,
     &     mu_ch_prts_c_hd_en_nm,
     &     mu_ch_prts_c_hd_lv_nm,
     &     ch22_prts_c_hd
 
      character*22 ch22_prts_c_hd_v
      character*22 ch22_prts_c_hd_pr_act
      character*22 ch22_prts_c_hd_lo_du_act
      character*22 ch22_prts_c_hd_du_act
      character*22 ch22_prts_c_hd_up_du_act
      character*22 ch22_prts_c_hd_lb
      character*22 ch22_prts_c_hd_co
      character*22 ch22_prts_c_hd_ub
      character*22 ch22_prts_c_hd_bp
      character*22 ch22_prts_c_hd_lo_co
      character*22 ch22_prts_c_hd_up_co
      character*22 ch22_prts_c_hd_lo_bp
      character*22 ch22_prts_c_hd_up_bp
      character*22 ch22_prts_c_hd_og_ob
      character*22 ch22_prts_c_hd_rg
      character*22 ch22_prts_c_hd_ob_rg
 
      equivalence (ch22_prts_c_hd_v,
     &     ch22_prts_c_hd(prts_c_hd_ix_v))
      equivalence (ch22_prts_c_hd_pr_act,
     &     ch22_prts_c_hd(prts_c_hd_ix_pr_act))
      equivalence (ch22_prts_c_hd_lo_du_act,
     &     ch22_prts_c_hd(prts_c_hd_ix_lo_du_act))
      equivalence (ch22_prts_c_hd_du_act,
     &     ch22_prts_c_hd(prts_c_hd_ix_du_act))
      equivalence (ch22_prts_c_hd_up_du_act,
     &     ch22_prts_c_hd(prts_c_hd_ix_up_du_act))
      equivalence (ch22_prts_c_hd_lb,
     &     ch22_prts_c_hd(prts_c_hd_ix_lb))
      equivalence (ch22_prts_c_hd_co,
     &     ch22_prts_c_hd(prts_c_hd_ix_co))
      equivalence (ch22_prts_c_hd_ub,
     &     ch22_prts_c_hd(prts_c_hd_ix_ub))
      equivalence (ch22_prts_c_hd_bp,
     &     ch22_prts_c_hd(prts_c_hd_ix_bp))
      equivalence (ch22_prts_c_hd_lo_co,
     &     ch22_prts_c_hd(prts_c_hd_ix_lo_co))
      equivalence (ch22_prts_c_hd_up_co,
     &     ch22_prts_c_hd(prts_c_hd_ix_up_co))
      equivalence (ch22_prts_c_hd_lo_bp,
     &     ch22_prts_c_hd(prts_c_hd_ix_lo_bp))
      equivalence (ch22_prts_c_hd_up_bp,
     &     ch22_prts_c_hd(prts_c_hd_ix_up_bp))
      equivalence (ch22_prts_c_hd_og_ob,
     &     ch22_prts_c_hd(prts_c_hd_ix_og_ob))
      equivalence (ch22_prts_c_hd_rg,
     &     ch22_prts_c_hd(prts_c_hd_ix_rg))
      equivalence (ch22_prts_c_hd_ob_rg,
     &     ch22_prts_c_hd(prts_c_hd_ix_ob_rg))
 
      integer prts_st_ix_ifs
      integer prts_st_ix_bp
      integer prts_st_ix_pwl
      integer prts_st_ix_bc
      integer prts_st_ix_fr
      integer prts_st_ix_at_lb
      integer prts_st_ix_at_ub
      integer prts_st_ix_fx
      integer prts_st_n_ix
      parameter (
     &     prts_st_ix_ifs = 1,
     &     prts_st_ix_bp = prts_st_ix_ifs + 1,
     &     prts_st_ix_pwl = prts_st_ix_bp + 1,
     &     prts_st_ix_bc = prts_st_ix_pwl + 1,
     &     prts_st_ix_fr = prts_st_ix_bc + 1,
     &     prts_st_ix_at_lb = prts_st_ix_fr + 1,
     &     prts_st_ix_at_ub = prts_st_ix_at_lb + 1,
     &     prts_st_ix_fx = prts_st_ix_at_ub + 1,
     &     prts_st_n_ix = prts_st_ix_fx)
 
      character*4 ch4_prts_st_ifs
      character*4 ch4_prts_st_bp
      character*4 ch4_prts_st_pwl
      character*4 ch4_prts_st_bc
      character*4 ch4_prts_st_fr
      character*4 ch4_prts_st_at_lb
      character*4 ch4_prts_st_at_ub
      character*4 ch4_prts_st_fx
      character*4 ch4_prts_st(prts_st_n_ix)
      common/ems_ch4_prts_st_com/ch4_prts_st
 
      equivalence (ch4_prts_st_ifs, ch4_prts_st(prts_st_ix_ifs))
      equivalence (ch4_prts_st_bp, ch4_prts_st(prts_st_ix_bp))
      equivalence (ch4_prts_st_pwl, ch4_prts_st(prts_st_ix_pwl))
      equivalence (ch4_prts_st_bc, ch4_prts_st(prts_st_ix_bc))
      equivalence (ch4_prts_st_fr, ch4_prts_st(prts_st_ix_fr))
      equivalence (ch4_prts_st_at_lb, ch4_prts_st(prts_st_ix_at_lb))
      equivalence (ch4_prts_st_at_ub, ch4_prts_st(prts_st_ix_at_ub))
      equivalence (ch4_prts_st_fx, ch4_prts_st(prts_st_ix_fx))
 
      integer fu_vr_li_ln
      parameter (fu_vr_li_ln = 132)
 
      character*(132+ml_nm_mx_n_ch-8) vr_li
      common/ems_com_prts/vr_li
 
      character*5 ch5_vr_in_c_sn_ty_ab_bp
      character*5 ch5_vr_in_c_sn_ty_bw_bp
      character*5 ch5_vr_in_c_sn_ty_bw_lb
      character*5 ch5_vr_in_c_sn_ty_ab_ub
      character*5 ch5_vr_in_c_sn_ty_btw
      character*5 ch5_vr_in_c_sn_ty_at_lb
      character*5 ch5_vr_in_c_sn_ty_at_ub
      character*5 ch5_vr_in_c_sn_ty_te_fx
      character*5 ch5_vr_in_c_sn_ty_fx
      character*5 ch5_vr_in_c_sn_ty(vr_in_c_n_sn_ty)
      equivalence (ch5_vr_in_c_sn_ty(vr_in_c_sn_ty_ab_bp),
     &     ch5_vr_in_c_sn_ty_ab_bp)
      equivalence (ch5_vr_in_c_sn_ty(vr_in_c_sn_ty_bw_bp),
     &     ch5_vr_in_c_sn_ty_bw_bp)
      equivalence (ch5_vr_in_c_sn_ty(vr_in_c_sn_ty_bw_lb),
     &     ch5_vr_in_c_sn_ty_bw_lb)
      equivalence (ch5_vr_in_c_sn_ty(vr_in_c_sn_ty_ab_ub),
     &     ch5_vr_in_c_sn_ty_ab_ub)
      equivalence (ch5_vr_in_c_sn_ty(vr_in_c_sn_ty_btw),
     &     ch5_vr_in_c_sn_ty_btw)
      equivalence (ch5_vr_in_c_sn_ty(vr_in_c_sn_ty_at_lb),
     &     ch5_vr_in_c_sn_ty_at_lb)
      equivalence (ch5_vr_in_c_sn_ty(vr_in_c_sn_ty_at_ub),
     &     ch5_vr_in_c_sn_ty_at_ub)
      equivalence (ch5_vr_in_c_sn_ty(vr_in_c_sn_ty_te_fx),
     &     ch5_vr_in_c_sn_ty_te_fx)
      equivalence (ch5_vr_in_c_sn_ty(vr_in_c_sn_ty_fx),
     &     ch5_vr_in_c_sn_ty_fx)
      common/ems_ch5_vr_in_c_sn_ty_com/ch5_vr_in_c_sn_ty
