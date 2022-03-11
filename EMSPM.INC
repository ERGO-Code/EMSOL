CM
      integer mx_n_i_ct_vr
      integer mx_n_rl_ct_vr
      parameter (mx_n_i_ct_vr =  250)
      parameter (mx_n_rl_ct_vr = 150)
      integer mx_ct_vr_nm_ln
      parameter (mx_ct_vr_nm_ln = 32)
 
      integer asm_ftran, asm_btran, asm_r_pc, asm_u_sed_wt
      parameter (
     &     asm_ftran = bt1,
     &     asm_btran = bt2,
     &     asm_r_pc = bt3,
     &     asm_u_sed_wt = bt4)
 
      double precision tl_dse_vec
      parameter (
     &     tl_dse_vec = 4.0d-1)
 
      integer ord_i_ls_stp, ord_i_ls_stp_p1
      parameter (
     &     ord_i_ls_stp =    16,
     &     ord_i_ls_stp_p1 = 17)
 
      character*8 ch_un_nm
      parameter (ch_un_nm = '********')
 
      integer ml_blk_st_ml_vec
      integer ml_blk_st_ml_lng_nm
      integer ml_blk_st_ml_pk_bs_os
      integer ml_blk_st_ml_i_vr
      integer ml_blk_st_ml_pwl_vr
      integer ml_blk_st_ml_sol
      integer ml_blk_st_ml_vr_ls
      integer ml_blk_st_ml_bs_inv_p
      integer ml_blk_st_ml_c_mtx
      integer ml_blk_st_ml_r_mtx
      integer ml_blk_st_ml_aux_sol
      integer ml_blk_st_ml_aux_blk
      integer ml_blk_st_ml_prsl_sv
      integer ml_blk_st_ml_prsl_wk
      integer ml_blk_st_ml_rg_da
      integer ml_blk_st_ml_u_bs
      integer ml_blk_st_ml_usr_c_mtx
      integer ml_blk_st_ml_usr_r_mtx
      integer ml_blk_st_dvx
      parameter (
     &     ml_blk_st_ml_vec =       bt1,
     &     ml_blk_st_ml_lng_nm =    bt2,
     &     ml_blk_st_ml_pk_bs_os =  bt3,
     &     ml_blk_st_ml_i_vr =      bt4,
     &     ml_blk_st_ml_pwl_vr =    bt5,
     &     ml_blk_st_ml_sol =       bt6,
     &     ml_blk_st_ml_vr_ls =     bt7,
     &     ml_blk_st_ml_bs_inv_p =  bt8,
     &     ml_blk_st_ml_c_mtx =     bt9,
     &     ml_blk_st_ml_r_mtx =     bt10,
     &     ml_blk_st_ml_aux_sol =   bt11,
     &     ml_blk_st_ml_aux_blk =   bt12,
     &     ml_blk_st_ml_prsl_sv =   bt13,
     &     ml_blk_st_ml_prsl_wk =   bt14,
     &     ml_blk_st_ml_rg_da =     bt15,
     &     ml_blk_st_ml_u_bs =      bt16,
     &     ml_blk_st_ml_usr_c_mtx = bt17,
     &     ml_blk_st_ml_usr_r_mtx = bt18,
     &     ml_blk_st_dvx =          bt19)
 
      integer ml_da_st_ld
      integer ml_da_st_nm
      integer ml_da_st_i_vr
      integer ml_da_st_alt_lp
      integer ml_da_st_bp_vr
      integer ml_da_st_usr_pwl_vr
      integer ml_da_st_pwl_vr
      integer ml_da_st_scl_ml
      integer ml_da_st_scl_ml_mtx
      integer ml_da_st_scl_ml_sol
      integer ml_da_st_vr_in_r
      integer ml_da_st_vr_in_c
      integer ml_da_st_inv
      integer ml_da_st_bs_cond_ok
      integer ml_da_st_bc_pr_act
      integer ml_da_st_vr_st_fm_act
      integer ml_da_st_non_bc_du_act
      integer ml_da_st_ed_wt
      integer ml_da_st_r_mtx
      integer ml_da_st_nw_r
      integer ml_da_st_nw_c
      integer ml_da_st_prsl
      integer ml_da_st_rg_da
      parameter (
     &     ml_da_st_ld =               bt1,
     &     ml_da_st_nm =               bt2,
     &     ml_da_st_i_vr =             bt3,
     &     ml_da_st_alt_lp =           bt4,
     &     ml_da_st_bp_vr =            bt5,
     &     ml_da_st_usr_pwl_vr =       bt6,
     &     ml_da_st_pwl_vr =           bt7,
     &     ml_da_st_scl_ml =           bt8,
     &     ml_da_st_scl_ml_mtx =       bt9,
     &     ml_da_st_scl_ml_sol =       bt10,
     &     ml_da_st_vr_in_r =          bt11,
     &     ml_da_st_vr_in_c =          bt12,
     &     ml_da_st_inv =              bt13,
     &     ml_da_st_bs_cond_ok =       bt14,
     &     ml_da_st_bc_pr_act =        bt15,
     &     ml_da_st_vr_st_fm_act =     bt16,
     &     ml_da_st_non_bc_du_act =    bt17,
     &     ml_da_st_ed_wt =            bt18,
     &     ml_da_st_r_mtx =            bt19,
     &     ml_da_st_nw_r =             bt20,
     &     ml_da_st_nw_c =             bt21,
     &     ml_da_st_prsl =             bt22,
     &     ml_da_st_rg_da =            bt23)
 
      integer eta_fi_da_st_fwd_p
      integer eta_fi_da_st_r_eta
      integer eta_fi_da_st_bwd_p
      parameter (
     &     eta_fi_da_st_fwd_p = bt1,
     &     eta_fi_da_st_r_eta = bt2,
     &     eta_fi_da_st_bwd_p = bt3)
 
      integer du_sol_mode_no
      integer du_sol_mode_y
      integer mx_du_sol_mode
      parameter (
     &     du_sol_mode_no = 0,
     &     du_sol_mode_y =  1,
     &     mx_du_sol_mode = du_sol_mode_y)
 
      integer fi_io_unknown
      integer fi_io_fmt
      integer fi_io_un_fmt
      parameter (
     &     fi_io_unknown = 0,
     &     fi_io_fmt = 1,
     &     fi_io_un_fmt = 2)
 
      integer mn_ml_mtx_fmt
      integer ml_mtx_fmt_by_ix
      integer ml_mtx_fmt_by_c
      integer ml_mtx_fmt_by_r
      integer mx_ml_mtx_fmt
      parameter (
     &     mn_ml_mtx_fmt =    1,
     &     ml_mtx_fmt_by_ix = 1,
     &     ml_mtx_fmt_by_c =  2,
     &     ml_mtx_fmt_by_r =  3,
     &     mx_ml_mtx_fmt =    3)
 
      integer mn_lp_alg_mode
      integer lp_alg_mode_cz
      integer lp_alg_mode_pr
      integer lp_alg_mode_du
      integer mx_lp_alg_mode
      parameter (
     &     mn_lp_alg_mode = 0,
     &     lp_alg_mode_cz = 0,
     &     lp_alg_mode_pr = 1,
     &     lp_alg_mode_du = 2,
     &     mx_lp_alg_mode = 2)
 
      integer mn_lp_iz_mode
      integer lp_iz_mode_bs_fm_st_0
      integer lp_iz_mode_bs_fm_st_1
      integer lp_iz_mode_bs_fm_st_2
      integer lp_iz_mode_bs_fm_st_3
      integer mx_lp_iz_mode
      parameter (
     &     mn_lp_iz_mode =         0,
     &     lp_iz_mode_bs_fm_st_0 = 0,
     &     lp_iz_mode_bs_fm_st_1 = 1,
     &     lp_iz_mode_bs_fm_st_2 = 2,
     &     lp_iz_mode_bs_fm_st_3 = 3,
     &     mx_lp_iz_mode =         3)
 
      integer sslv_en_no_bd_cg
      integer sslv_en_no_co_cg
      parameter (
     &     sslv_en_no_bd_cg = bt1,
     &     sslv_en_no_co_cg = bt2)
 
      integer sslv_xit_no_inv
      integer sslv_xit_no_reset_non_bc_pr_act
      integer sslv_xit_no_reset_bc_pr_act
      integer sslv_xit_no_reset_non_bc_du_act
      parameter (
     &     sslv_xit_no_inv =                 bt1,
     &     sslv_xit_no_reset_non_bc_pr_act = bt2,
     &     sslv_xit_no_reset_bc_pr_act =     bt3,
     &     sslv_xit_no_reset_non_bc_du_act = bt4)
 
      integer inv_alg_tom
      integer inv_alg_perm
      integer inv_alg_mwz
      integer inv_alg_sus
      parameter (
     &     inv_alg_tom =      bt1,
     &     inv_alg_perm =     bt2,
     &     inv_alg_mwz =      bt3,
     &     inv_alg_sus =      bt4)
c
c     The status vector is used to store various items of information
c     about each variable.
c
c     Bits 32-27 are the Variable Status bits.
c     ----------------------------------------
c
c     These comprise the base status bits (32-30) which are the only
c     bits which would (normally) be set by the user, the infeasibility
c     and lower/upper bound bits.
c
c     If bit 32 is set then it is basic.
c     If bit 31 is set then
c     .  If it is non-basic its primal activity can move down.
c     .  If it is basic     it is above its upper bound.
c     If bit 30 is set then
c     .  If it is non-basic its primal activity can move up.
c     .  If it is basic     it is below its lower bound.
c     If bit 29 is set then it is primal infeasible.
c     If bit 28 is set then it has a lower bound/cost.
c     If bit 27 is set then it has an upper bound/cost.
c
c
c     Bits 26-23 are the Variable Class bits.
c     ---------------------------------------
c
c     These define what class a variable is.
c
c     If bit 26 is set then it should take an integer value.
c     If bit 25 is set then it has a break point and lower/upper costs.
c     If bit 24 is set then it has a PWL cost function
c     If bit 23 is set then it is SOS variable.
c
c
c     Bits 22-21 are the Variable Algebra bits.
c     -----------------------------------------
c
c     These are used to store logical information relating to the
c     linear algebra package.
c
c     If bit 22 is set then it has become basic/nonbasic but the basis
c                           change has yet to be accommodated.
c     If bit 21 is set then the variable was in the basis for INVERT.
c     .                     This is needed for SC updates (ie just for
c     .                     rsmi_data at present.
c
c     Bits 20-1  are the Variable Index bits.
c     ---------------------------------------
c
c     These are used as a 20-bit integer to store the index of the
c     variable within vr_in_r/c (according to whether the variable is
c     basic or nonbasic).
c     This places a bound of mx_mx_ml_a_dim = 2**20-1 = 1,048,575 on the
c     maximum number of rows or columns in the matrix. For larger
c     problems this pointer could just be accurate to within 2 or 4
c     places.
c
c     The index is extracted as
c
c     iand(st, mx_mx_ml_a_dim)
c
c     NB If the variable is PWL then the index which is stored is its
c     position within the list of PWL variables. This is to enable rapid
c     access into the data structures for PWL variables. The index in
c     vr_in_r/c for a PWL variable is held in a separate array.
c
      integer f_il_ix_n, mx_mx_ml_a_dim, mx_mx_ml_a_dim_p1
      parameter (
     &     f_il_ix_n =           bt21,
     &     mx_mx_ml_a_dim_p1 =   f_il_ix_n,
     &     mx_mx_ml_a_dim =      mx_mx_ml_a_dim_p1-1)
      integer bc_bt, dn_bt, up_bt, ifs_bt
      integer lb_bt, ub_bt, i_bt, alt_bt, bp_bt
      integer sos_bt, u_bs_cg_bt, inv_bs_bt
      parameter (
     &     bc_bt =            bt32,      dn_bt =            bt31,
     &     up_bt =            bt30,      ifs_bt =           bt29,
     &     lb_bt =            bt28,      ub_bt =            bt27,
     &     i_bt =             bt26,      alt_bt =           bt25,
     &     bp_bt =            bt24,      sos_bt =           bt23,
     &     u_bs_cg_bt =       bt22,      inv_bs_bt =        bt21)
c
c     Bits 32-30 are the Variable Base Status bits.
c     ---------------------------------------------
c
c     If the variable should be basic then the basic bit and (at least)
c     one of the down and up bits should be set.
c     This is achieved if the base status is bc_vr_bs_st
c
c     If the variable is undefined then only the basic bit should be set
c     This is achieved if the base status is undn_vr_bs_st.
c     Note that this base status cannot occur within RSMI.
c
c     If the variable should be nonbasic then the basic bit should not
c     be set.
c     This is achieved if the base status is non_bc_vr_bs_st
c     Note that non_bc_vr_bs_st = dn_bt + up_bt so that switches between
c     bc_vr_bs_st and non_bc_vr_bs_st can be achieved by setting or
c     unsetting the basic bit
c
      integer su_vr_bs_bt, bc_vr_bs_st, undn_vr_bs_st, non_bc_vr_bs_st
      parameter (
     &     su_vr_bs_bt =     bc_bt + dn_bt + up_bt,
     &     bc_vr_bs_st =     bc_bt + dn_bt + up_bt,
     &     undn_vr_bs_st =   bc_bt,
     &     non_bc_vr_bs_st =         dn_bt + up_bt)
c
c     Other bit-masks which are useful for logical operations are:
c
      integer up_dn, lb_ub
      integer su_vr_st_bt
      integer su_vr_cls_bt
      integer su_non_std_vr_bt
      integer bp_vr_msk
      integer pwl_vr_msk
      parameter (
     &     up_dn = up_bt + dn_bt,
     &     lb_ub = lb_bt + ub_bt,
     &     su_vr_st_bt = su_vr_bs_bt + ifs_bt + lb_ub,
     &     su_vr_cls_bt = i_bt + alt_bt + bp_bt + sos_bt,
     &     su_non_std_vr_bt = alt_bt + bp_bt,
     &     bp_vr_msk = alt_bt + bp_bt,
     &     pwl_vr_msk = alt_bt)
 
      integer bp_vr_bc_ab_bp
      integer bp_vr_bc_bw_bp
      parameter (
     &     bp_vr_bc_ab_bp = bc_bt + up_bt + dn_bt + lb_bt,
     &     bp_vr_bc_bw_bp = bc_bt + up_bt + dn_bt + ub_bt)
 
      integer prob_st_unknown, prob_st_op
      integer prob_st_ifs, prob_st_unbd
      integer prob_st_mx_n_it, prob_st_no_sol
      integer prob_st_mx_n_sol, prob_st_no_po
      parameter (
     &     prob_st_unknown =      -1,
     &     prob_st_op =            0,
     &     prob_st_ifs =           1,
     &     prob_st_unbd =          2,
     &     prob_st_mx_n_it =       3,
     &     prob_st_no_sol =        4,
     &     prob_st_mx_n_sol =      5,
     &     prob_st_no_po =         6)
 
      integer pwl_ob_fn_dn_ty_rf_v
      integer pwl_ob_fn_dn_ty_rf_sn
      parameter (
     &     pwl_ob_fn_dn_ty_rf_v =  1,
     &     pwl_ob_fn_dn_ty_rf_sn = 2)
 
      integer repl_non_std_vr_bp_bt
      integer repl_non_std_vr_pwl_bt
      integer repl_non_std_vr_f_fr_bt
      parameter (
     &     repl_non_std_vr_bp_bt = bt1,
     &     repl_non_std_vr_pwl_bt = bt2,
     &     repl_non_std_vr_f_fr_bt = bt3)
 
      integer pc_alg_dan, pc_alg_approx_dvx
      integer pc_alg_exact_dvx, pc_alg_sed
      parameter (
     &     pc_alg_dan =        0,
     &     pc_alg_approx_dvx = 1,
     &     pc_alg_exact_dvx =  2,
     &     pc_alg_sed =        3)
c
c     vr_in_c is partitioned into sections. The section numbers for
c     logicals are generic. There is an offset to get the section
c     numbers for structurals
c
c
c     vr_in_c is partitioned as follows.
c
c     First the logicals...
c
c                            up_dn    up    dn l_pc_p
c     ab_bp bw_bp bw_lb ab_ub  btw at_lb at_ub  te_fx   fx
c         |     |     |     |    |     |     |      |    |
c      ___V_____V_____V_____V____V_____V_____V______V____V
c     |    |     |     |     |    |     |     |      |    |
c     |  1 |   2 |   3 |   4 |  5 |   6 |   7 |    8 |  9 |
c     |____|_____|_____|_____|____|_____|_____|______|____|
c
c
c     where,
c
c     (lg_in_c_)ab_bp_p = vr_in_c(os_lg_in_c_ab_bp_p)
c     (lg_in_c_)bw_bp_p = vr_in_c(os_lg_in_c_bw_bp_p)
c     (lg_in_c_)bw_lb_p = vr_in_c(os_lg_in_c_bw_lb_p)
c     (lg_in_c_)ab_ub_p = vr_in_c(os_lg_in_c_ab_ub_p)
c     (lg_in_c_)btw_p =   vr_in_c(os_lg_in_c_btw_p)
c     (lg_in_c_)up_dn_p = vr_in_c(os_lg_in_c_up_dn_p)
c     (lg_in_c_)at_lb_p = vr_in_c(os_lg_in_c_at_lb_p)
c     (lg_in_c_)up_p =    vr_in_c(os_lg_in_c_up_p)
c     (lg_in_c_)at_ub_p = vr_in_c(os_lg_in_c_at_ub_p)
c     (lg_in_c_)dn_p =    vr_in_c(os_lg_in_c_dn_p)
c     (lg_in_c_)te_fx_p = vr_in_c(os_lg_in_c_te_fx_p)
c     (lg_in_c_)l_pc_p =  vr_in_c(os_lg_in_c_l_pc_p)
c     (lg_in_c_)fx_p =    vr_in_c(os_lg_in_c_fx_p)
c
c     and
c
c     1: Logicals (just) above their break point.
c     2: Logicals (just) below their break point.
c     3: Logicals below their lower bound.
c     4: Logicals above their upper bound.
c     5: Logicals between their bounds (or free).
c     6: Logicals at their lower bound.
c     7: Logicals at their upper bound.
c     8: Logicals temporarily fixed (at their value).
c     9: Logicals permanently fixed (at their value).
c
c     Unless minimizing sum of infeasibilities
c
c     Logicals in sections 1--5 can move up or down.
c     Logicals in section  6 can only move up.
c     Logicals in section  7 can only move down.
c     Logicals in section  8--9 cannot move
c
c     ... then the structurals...
c
c                            up_dn    up    dn l_pc_p
c     ab_bp bw_bp bw_lb ab_ub  btw at_lb at_ub  te_fx   fx
c         |     |     |     |    |     |     |      |    |
c      ___V_____V_____V_____V____V_____V_____V______V____V
c     |    |     |     |     |    |     |     |      |    |
c     | 10 |  11 |  12 |  13 | 14 |  15 |  16 |   17 | 18 |
c     |____|_____|_____|_____|____|_____|_____|______|____|
c
c
c     where,
c     (struc_in_c_)ab_bp_p = vr_in_c(os_struc_in_c_ab_bp_p)
c     (struc_in_c_)bw_bp_p = vr_in_c(os_struc_in_c_bw_bp_p)
c     (struc_in_c_)bw_lb_p = vr_in_c(os_struc_in_c_bw_lb_p)
c     (struc_in_c_)ab_ub_p = vr_in_c(os_struc_in_c_ab_ub_p)
c     (struc_in_c_)btw_p =   vr_in_c(os_struc_in_c_btw_p)
c     (struc_in_c_)up_dn_p = vr_in_c(os_struc_in_c_up_dn_p)
c     (struc_in_c_)at_lb_p = vr_in_c(os_struc_in_c_at_lb_p)
c     (struc_in_c_)up_p =    vr_in_c(os_struc_in_c_up_p)
c     (struc_in_c_)at_ub_p = vr_in_c(os_struc_in_c_at_ub_p)
c     (struc_in_c_)dn_p =    vr_in_c(os_struc_in_c_dn_p)
c     (struc_in_c_)te_fx_p = vr_in_c(os_struc_in_c_te_fx_p)
c     (struc_in_c_)l_pc_p =  vr_in_c(os_struc_in_c_l_pc_p)
c     (struc_in_c_)fx_p =    vr_in_c(os_struc_in_c_fx_p)
c
c     and
c
c     10: Structurals (just) above their break point.
c     11: Structurals (just) below their break point.
c     12: Structurals below their lower bound.
c     13: Structurals above their upper bound.
c     14: Structurals between their bounds (or free).
c     15: Structurals at their lower bound.
c     16: Structurals at their upper bound.
c     17: Structurals temporarily fixed (at their value).
c     18: Structurals permanently fixed (at their value).
c
c     Unless minimizing sum of infeasibilities
c
c     Structurals in sections 10--14 can move up or down.
c     Structurals in section  15 can only move up.
c     Structurals in section  16 can only move down.
c     Structurals in section  17--18 cannot move
c
      integer vr_in_c_sn_ty_ab_bp
      integer vr_in_c_sn_ty_bw_bp
      integer vr_in_c_sn_ty_bw_lb
      integer vr_in_c_sn_ty_ab_ub
      integer vr_in_c_sn_ty_btw
      integer vr_in_c_sn_ty_at_lb
      integer vr_in_c_sn_ty_at_ub
      integer vr_in_c_sn_ty_te_fx
      integer vr_in_c_sn_ty_fx
      integer vr_in_c_n_sn_ty
      integer lg_sn_n_os
      integer struc_sn_n_os
      integer os_vr_in_c_mx_n_c
      integer os_lg_in_c_f_p
      integer os_lg_in_c_ab_bp_p
      integer os_lg_in_c_bw_bp_p
      integer os_lg_in_c_bw_lb_p
      integer os_lg_in_c_ab_ub_p
      integer os_lg_in_c_btw_p
      integer os_lg_in_c_up_dn_p
      integer os_lg_in_c_at_lb_p
      integer os_lg_in_c_up_p
      integer os_lg_in_c_at_ub_p
      integer os_lg_in_c_dn_p
      integer os_lg_in_c_te_fx_p
      integer os_lg_in_c_l_pc_p
      integer os_lg_in_c_fx_p
      integer os_lg_in_c_l_p
      integer os_struc_in_c_f_p
      integer os_struc_in_c_f_p_m1
      integer os_struc_in_c_ab_bp_p
      integer os_struc_in_c_bw_bp_p
      integer os_struc_in_c_bw_lb_p
      integer os_struc_in_c_ab_ub_p
      integer os_struc_in_c_btw_p
      integer os_struc_in_c_up_dn_p
      integer os_struc_in_c_at_lb_p
      integer os_struc_in_c_up_p
      integer os_struc_in_c_at_ub_p
      integer os_struc_in_c_dn_p
      integer os_struc_in_c_te_fx_p
      integer os_struc_in_c_l_pc_p
      integer os_struc_in_c_fx_p
      integer os_struc_in_c_l_p
      integer os_vr_in_c_l_p
      integer vr_in_c_n_sn
      integer vr_in_c_os_ze_en
      parameter (
     &     vr_in_c_sn_ty_ab_bp = 1,
     &     vr_in_c_sn_ty_bw_bp = 2,
     &     vr_in_c_sn_ty_bw_lb = 3,
     &     vr_in_c_sn_ty_ab_ub = 4,
     &     vr_in_c_sn_ty_btw =   5,
     &     vr_in_c_sn_ty_at_lb = 6,
     &     vr_in_c_sn_ty_at_ub = 7,
     &     vr_in_c_sn_ty_te_fx = 8,
     &     vr_in_c_sn_ty_fx =    9,
     &     vr_in_c_n_sn_ty = vr_in_c_sn_ty_fx,
     &     lg_sn_n_os =  0,
     &     struc_sn_n_os = vr_in_c_n_sn_ty)
      parameter (
     &     os_vr_in_c_mx_n_c =       0,
     &     os_lg_in_c_ab_bp_p =     -vr_in_c_sn_ty_ab_bp,
     &     os_lg_in_c_f_p =          os_lg_in_c_ab_bp_p,
     &     os_lg_in_c_bw_bp_p =     -vr_in_c_sn_ty_bw_bp,
     &     os_lg_in_c_bw_lb_p =     -vr_in_c_sn_ty_bw_lb,
     &     os_lg_in_c_ab_ub_p =     -vr_in_c_sn_ty_ab_ub,
     &     os_lg_in_c_btw_p =       -vr_in_c_sn_ty_btw,
     &     os_lg_in_c_up_dn_p =      os_lg_in_c_btw_p,
     &     os_lg_in_c_at_lb_p =     -vr_in_c_sn_ty_at_lb,
     &     os_lg_in_c_up_p =         os_lg_in_c_at_lb_p,
     &     os_lg_in_c_at_ub_p =     -vr_in_c_sn_ty_at_ub,
     &     os_lg_in_c_dn_p =         os_lg_in_c_at_ub_p,
     &     os_lg_in_c_te_fx_p =     -vr_in_c_sn_ty_te_fx,
     &     os_lg_in_c_l_pc_p =       os_lg_in_c_te_fx_p,
     &     os_lg_in_c_fx_p =        -vr_in_c_sn_ty_fx,
     &     os_lg_in_c_l_p =          os_lg_in_c_fx_p)
      parameter (os_struc_in_c_ab_bp_p =
     &     -(struc_sn_n_os+vr_in_c_sn_ty_ab_bp))
      parameter (os_struc_in_c_f_p  =
     &     os_struc_in_c_ab_bp_p)
      parameter (os_struc_in_c_f_p_m1 =
     &     os_struc_in_c_f_p + 1)
      parameter (os_struc_in_c_bw_bp_p =
     &     -(struc_sn_n_os+vr_in_c_sn_ty_bw_bp))
      parameter (os_struc_in_c_bw_lb_p =
     &     -(struc_sn_n_os+vr_in_c_sn_ty_bw_lb))
      parameter (os_struc_in_c_ab_ub_p =
     &     -(struc_sn_n_os+vr_in_c_sn_ty_ab_ub))
      parameter (os_struc_in_c_btw_p =
     &     -(struc_sn_n_os+vr_in_c_sn_ty_btw))
      parameter (os_struc_in_c_up_dn_p =
     &     os_struc_in_c_btw_p)
      parameter (os_struc_in_c_at_lb_p =
     &     -(struc_sn_n_os+vr_in_c_sn_ty_at_lb))
      parameter (os_struc_in_c_up_p =
     &     os_struc_in_c_at_lb_p)
      parameter (os_struc_in_c_at_ub_p =
     &     -(struc_sn_n_os+vr_in_c_sn_ty_at_ub))
      parameter (os_struc_in_c_dn_p =
     &     os_struc_in_c_at_ub_p)
      parameter (os_struc_in_c_te_fx_p =
     &     -(struc_sn_n_os+vr_in_c_sn_ty_te_fx))
      parameter (os_struc_in_c_l_pc_p =
     &     os_struc_in_c_te_fx_p)
      parameter (os_struc_in_c_fx_p =
     &     -(struc_sn_n_os+vr_in_c_sn_ty_fx))
      parameter (os_struc_in_c_l_p = os_struc_in_c_fx_p)
      parameter (os_vr_in_c_l_p = os_struc_in_c_fx_p)
      parameter (vr_in_c_n_sn = struc_sn_n_os + vr_in_c_n_sn_ty)
      parameter (vr_in_c_os_ze_en = vr_in_c_n_sn)
 
      integer rq_inv_no_rq_inv, rq_inv_du_act_er, rq_inv_pv_er
      integer rq_inv_u_growth, rq_inv_mx_n_u, rq_inv_mx_n_eta_grp
      integer rq_inv_reset_bs, rq_inv_nw_bs, rq_inv_nw_r
      integer rq_inv_scl_ml_mtx
      parameter (
     &     rq_inv_no_rq_inv =    0,
     &     rq_inv_du_act_er =    1,
     &     rq_inv_pv_er =        2,
     &     rq_inv_u_growth =     3,
     &     rq_inv_mx_n_u =       4,
     &     rq_inv_mx_n_eta_grp = 5,
     &     rq_inv_reset_bs =     6,
     &     rq_inv_nw_bs =        7,
     &     rq_inv_nw_r =         8,
     &     rq_inv_scl_ml_mtx =   9)
 
      integer rq_reset_no_rq_reset
      integer rq_reset_op
      integer rq_reset_unbd
      integer rq_reset_sing_bs
      integer rq_reset_re_re_pc
      integer rq_reset_ze_pr_wr
      integer rq_reset_cz_r_alg_er
      integer rq_reset_usr_rq_reset
      parameter (
     &     rq_reset_no_rq_reset =  0,
     &     rq_reset_op =           1,
     &     rq_reset_unbd =         2,
     &     rq_reset_sing_bs =      3,
     &     rq_reset_re_re_pc =     4,
     &     rq_reset_ze_pr_wr =     5,
     &     rq_reset_cz_r_alg_er =  6,
     &     rq_reset_usr_rq_reset = 7)
 
      integer rq_re_pc_no_rq_re_pc
      integer rq_re_pc_pr_wt_cg
      integer rq_re_pc_ph_cg
      integer rq_re_pc_inv
      integer rq_re_pc_reset
      integer rq_re_pc_pv_er
      integer rq_re_pc_du_act_er
      integer rq_re_pc_du_act_wg_sgn
      parameter (
     &     rq_re_pc_no_rq_re_pc =   0,
     &     rq_re_pc_pr_wt_cg =      1,
     &     rq_re_pc_ph_cg =         2,
     &     rq_re_pc_inv =           3,
     &     rq_re_pc_reset =         4,
     &     rq_re_pc_pv_er =         5,
     &     rq_re_pc_du_act_er =     6,
     &     rq_re_pc_du_act_wg_sgn = 7)
 
      integer ml_da_no_cg_ck_bt
      integer ml_da_no_cg_non_bc_vr_bd
      integer ml_da_no_cg_bc_vr_bd
      integer ml_da_no_cg_bd
      integer ml_da_no_cg_non_bc_co
      integer ml_da_no_cg_bc_co
      integer ml_da_no_cg_co
      integer ml_da_no_cg_bc_bt
      integer ml_da_no_cg_up_dn_bt
      integer ml_da_no_cg_st
      integer ml_da_no_cg_non_bc_pr_act
      integer ml_da_no_cg_bc_pr_act
      integer ml_da_no_cg_pr_act
      integer ml_da_no_cg_non_bc_du_act
      integer ml_da_no_cg_bc_du_act
      integer ml_da_no_cg_du_act
      integer ml_da_no_cg_al
      parameter (
     &     ml_da_no_cg_ck_bt =         bt1,
     &     ml_da_no_cg_non_bc_vr_bd =  bt2,
     &     ml_da_no_cg_bc_vr_bd =      bt3,
     &     ml_da_no_cg_bd =
     &     ml_da_no_cg_non_bc_vr_bd + ml_da_no_cg_bc_vr_bd,
     &     ml_da_no_cg_non_bc_co =     bt4,
     &     ml_da_no_cg_bc_co =         bt5,
     &     ml_da_no_cg_co =
     &     ml_da_no_cg_non_bc_co + ml_da_no_cg_bc_co,
     &     ml_da_no_cg_bc_bt =         bt6,
     &     ml_da_no_cg_up_dn_bt =      bt7,
     &     ml_da_no_cg_st =
     &     ml_da_no_cg_bc_bt + ml_da_no_cg_up_dn_bt,
     &     ml_da_no_cg_non_bc_pr_act = bt8,
     &     ml_da_no_cg_bc_pr_act =     bt9,
     &     ml_da_no_cg_pr_act =
     &     ml_da_no_cg_non_bc_pr_act+ml_da_no_cg_bc_pr_act,
     &     ml_da_no_cg_non_bc_du_act = bt10,
     &     ml_da_no_cg_bc_du_act =     bt11,
     &     ml_da_no_cg_du_act =
     &     ml_da_no_cg_non_bc_du_act+ml_da_no_cg_bc_du_act,
     &     ml_da_no_cg_al =
     &     ml_da_no_cg_bd + ml_da_no_cg_co + ml_da_no_cg_st +
     &     ml_da_no_cg_pr_act + ml_da_no_cg_du_act)
 
      integer sto_pk_eta_v, sto_full_eta_v
      parameter (
     &     sto_pk_eta_v = 0,
     &     sto_full_eta_v = 1)
 
      integer sto_ix_y, sto_ix_no, sto_ix_poss
      parameter (
     &     sto_ix_y =      1,
     &     sto_ix_no =    -1,
     &     sto_ix_poss =   0)
 
      integer tbu_r_loop_y, tbu_r_loop_no, tbu_r_loop_poss
      parameter (
     &     tbu_r_loop_y =      1,
     &     tbu_r_loop_no =    -1,
     &     tbu_r_loop_poss =   0)
 
      integer no_eta_se_ty
      integer inv_eta_se_ty
      integer pk_c_eta_se_ty
      integer pk_r_eta_se_ty
      integer full_c_eta_se_ty
      integer full_r_eta_se_ty
      integer n_eta_se_ty
      parameter (
     &     no_eta_se_ty =     0,
     &     inv_eta_se_ty =    1,
     &     pk_c_eta_se_ty =   inv_eta_se_ty + 1,
     &     pk_r_eta_se_ty =   pk_c_eta_se_ty + 1,
     &     full_c_eta_se_ty = pk_r_eta_se_ty + 1,
     &     full_r_eta_se_ty = full_c_eta_se_ty + 1,
     &     n_eta_se_ty = full_r_eta_se_ty)
 
      integer mx_n_eta_se
      integer eta_se_rec_os_eta_ty
      integer eta_se_rec_os_n_eta
      integer eta_se_rec_z
      integer eta_se_rec_bwd_os_n_eta
      integer eta_se_rec_bwd_os_eta_ty
      integer eta_se_rec_os_ze_sa_en
      parameter (
     &     mx_n_eta_se = 10,
     &     eta_se_rec_os_eta_ty = 1,
     &     eta_se_rec_os_n_eta = 2,
     &     eta_se_rec_z = 2,
     &     eta_se_rec_bwd_os_n_eta = -1,
     &     eta_se_rec_bwd_os_eta_ty = 0,
     &     eta_se_rec_os_ze_sa_en = eta_se_rec_z)
 
      integer eta_fi_fwd_p_poss_bt
      integer eta_fi_r_eta_poss_bt
      integer eta_fi_bwd_p_poss_bt
      integer eta_fi_fwd_p_y_bt
      integer eta_fi_r_eta_y_bt
      integer eta_fi_bwd_p_y_bt
      parameter (
     &     eta_fi_fwd_p_poss_bt = bt1,
     &     eta_fi_r_eta_poss_bt = bt2,
     &     eta_fi_bwd_p_poss_bt = bt3,
     &     eta_fi_fwd_p_y_bt = bt4,
     &     eta_fi_r_eta_y_bt = bt5,
     &     eta_fi_bwd_p_y_bt = bt6)
 
      integer u_bs_pf, u_bs_pf_r_cp, u_bs_sc
      parameter (
     &     u_bs_pf = 0,
     &     u_bs_pf_r_cp = 1,
     &     u_bs_sc = 2)
 
      integer cz_c_bk_bd_bt
      parameter (
     &     cz_c_bk_bd_bt =      bt1)
 
      integer cz_r_l1_bt
      integer cz_r_refine_bt
      integer cz_r_growth_mode
      integer cz_r_growth_inv
      integer cz_r_growth_cg_tl
      integer cz_r_growth_mv_bd
      integer cz_r_growth_cg_act
      parameter (
     &     cz_r_l1_bt =           bt1,
     &     cz_r_refine_bt =       bt2,
     &     cz_r_growth_mode =     bt3+bt4+bt5,
     &     cz_r_growth_inv =      bt3,
     &     cz_r_growth_cg_tl =    bt4,
     &     cz_r_growth_mv_bd =    bt3+bt4,
     &     cz_r_growth_cg_act =   bt5)
 
      integer df_l1_cz_r_mx_n_cdd
      parameter (df_l1_cz_r_mx_n_cdd = 10)
 
      integer prts_rl_fld_ln
      parameter (prts_rl_fld_ln = 22)
 
      integer ems_vr_ty_r
      integer ems_vr_ty_c
      parameter (
     &     ems_vr_ty_r =   1,
     &     ems_vr_ty_c =   2)
 
c      integer ems_vr_cls_std
c      integer ems_vr_cls_bp
c      integer ems_vr_cls_pwl
c      parameter (
c     &     ems_vr_cls_std = 1,
c     &     ems_vr_cls_bp =  2,
c     &     ems_vr_cls_pwl = 3)
 
 
