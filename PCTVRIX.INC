CM
      integer ix_usr_p_r_lb
      integer ix_usr_p_r_pr_act
      integer ix_usr_p_r_ub
      integer ix_usr_p_r_du_act
      integer ix_usr_p_r_st
      integer ix_usr_p_c_lb
      integer ix_usr_p_c_pr_act
      integer ix_usr_p_c_ub
      integer ix_usr_p_c_du_act
      integer ix_usr_p_c_st
      integer ix_usr_p_c_co
      integer ix_usr_p_r_nm
      integer ix_usr_p_c_nm
      integer ix_usr_p_r_scl
      integer ix_usr_p_c_scl
      integer ix_usr_p_usr_mtx_r_ix
      integer ix_usr_p_usr_mtx_r_v
      integer ix_usr_p_usr_mtx_c_sa
      integer ix_usr_p_usr_mtx_c_ix
      integer ix_usr_p_usr_mtx_c_v
      integer ix_usr_p_usr_mtx_r_sa
      integer ix_usr_p_r_aux_sol
      integer ix_usr_p_c_aux_sol
      integer ix_usr_p_c_co_rg_up_co_v
      integer ix_usr_p_c_co_rg_lo_co_v
      integer ix_usr_p_c_co_rg_up_ob_v
      integer ix_usr_p_c_co_rg_lo_ob_v
      integer ix_usr_p_c_co_rg_up_en_vr
      integer ix_usr_p_c_co_rg_lo_en_vr
      integer ix_usr_p_c_co_rg_up_lv_vr
      integer ix_usr_p_c_co_rg_lo_lv_vr
      integer ix_usr_p_c_bd_rg_up_bd_v
      integer ix_usr_p_c_bd_rg_lo_bd_v
      integer ix_usr_p_c_bd_rg_up_ob_v
      integer ix_usr_p_c_bd_rg_lo_ob_v
      integer ix_usr_p_c_bd_rg_up_en_vr
      integer ix_usr_p_c_bd_rg_lo_en_vr
      integer ix_usr_p_c_bd_rg_up_lv_vr
      integer ix_usr_p_c_bd_rg_lo_lv_vr
      integer ix_usr_p_r_bd_rg_up_bd_v
      integer ix_usr_p_r_bd_rg_lo_bd_v
      integer ix_usr_p_r_bd_rg_up_ob_v
      integer ix_usr_p_r_bd_rg_lo_ob_v
      integer ix_usr_p_r_bd_rg_up_en_vr
      integer ix_usr_p_r_bd_rg_lo_en_vr
      integer ix_usr_p_r_bd_rg_up_lv_vr
      integer ix_usr_p_r_bd_rg_lo_lv_vr
      integer ix_usr_p_pwl_r_usr_cu_sn
      integer ix_usr_p_pwl_c_usr_cu_sn
      integer ix_usr_p_c_co_rg_up_act_v
      integer ix_usr_p_c_co_rg_lo_act_v
      integer n_p_ct_vr
      parameter (ix_usr_p_r_lb =     1)
      parameter (ix_usr_p_r_pr_act = 2)
      parameter (ix_usr_p_r_ub =     3)
      parameter (ix_usr_p_r_du_act = 4)
      parameter (ix_usr_p_r_st =     5)
      parameter (ix_usr_p_c_lb =     6)
      parameter (ix_usr_p_c_pr_act = 7)
      parameter (ix_usr_p_c_ub =     8)
      parameter (ix_usr_p_c_du_act = 9)
      parameter (ix_usr_p_c_st =    10)
      parameter (ix_usr_p_c_co =    11)
      parameter (ix_usr_p_r_nm =    12)
      parameter (ix_usr_p_c_nm =    13)
      parameter (ix_usr_p_r_scl =   14)
      parameter (ix_usr_p_c_scl =   15)
      parameter (ix_usr_p_usr_mtx_r_ix = 17)
      parameter (ix_usr_p_usr_mtx_c_ix = 18)
      parameter (ix_usr_p_usr_mtx_r_v =  19)
      parameter (ix_usr_p_usr_mtx_c_v =  20)
      parameter (ix_usr_p_usr_mtx_r_sa = 21)
      parameter (ix_usr_p_usr_mtx_c_sa = 22)
      parameter (ix_usr_p_r_aux_sol = 29)
      parameter (ix_usr_p_c_aux_sol = 30)
      parameter (ix_usr_p_c_co_rg_up_co_v  = 31)
      parameter (ix_usr_p_c_co_rg_lo_co_v  = 32)
      parameter (ix_usr_p_c_co_rg_up_ob_v  = 33)
      parameter (ix_usr_p_c_co_rg_lo_ob_v  = 34)
      parameter (ix_usr_p_c_co_rg_up_en_vr = 35)
      parameter (ix_usr_p_c_co_rg_lo_en_vr = 36)
      parameter (ix_usr_p_c_co_rg_up_lv_vr = 37)
      parameter (ix_usr_p_c_co_rg_lo_lv_vr = 38)
      parameter (ix_usr_p_c_bd_rg_up_bd_v  = 39)
      parameter (ix_usr_p_c_bd_rg_lo_bd_v  = 40)
      parameter (ix_usr_p_c_bd_rg_up_ob_v  = 41)
      parameter (ix_usr_p_c_bd_rg_lo_ob_v  = 42)
      parameter (ix_usr_p_c_bd_rg_up_en_vr = 43)
      parameter (ix_usr_p_c_bd_rg_lo_en_vr = 44)
      parameter (ix_usr_p_c_bd_rg_up_lv_vr = 45)
      parameter (ix_usr_p_c_bd_rg_lo_lv_vr = 46)
      parameter (ix_usr_p_r_bd_rg_up_bd_v  = 47)
      parameter (ix_usr_p_r_bd_rg_lo_bd_v  = 48)
      parameter (ix_usr_p_r_bd_rg_up_ob_v  = 49)
      parameter (ix_usr_p_r_bd_rg_lo_ob_v  = 50)
      parameter (ix_usr_p_r_bd_rg_up_en_vr = 51)
      parameter (ix_usr_p_r_bd_rg_lo_en_vr = 52)
      parameter (ix_usr_p_r_bd_rg_up_lv_vr = 53)
      parameter (ix_usr_p_r_bd_rg_lo_lv_vr = 54)
      parameter (ix_usr_p_pwl_r_usr_cu_sn = 70)
      parameter (ix_usr_p_pwl_c_usr_cu_sn = 71)
      parameter (ix_usr_p_c_co_rg_up_act_v = 80)
      parameter (ix_usr_p_c_co_rg_lo_act_v = 81)
      parameter (n_p_ct_vr =        81)
