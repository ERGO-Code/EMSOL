CM
c=======================================================================
c     CRASH handles
c
      integer blk_crsh
      integer hdl_crsh_r_ty(0:hdl_z_m1)
      integer hdl_crsh_r_pri_lkb(0:hdl_z_m1)
      integer hdl_crsh_r_pri_lkf(0:hdl_z_m1)
      integer hdl_crsh_c_ty(0:hdl_z_m1)
      integer hdl_crsh_r_n_act_en(0:hdl_z_m1)
      integer hdl_crsh_r_n_act_en_hdr(0:hdl_z_m1)
      integer hdl_crsh_r_n_act_en_lkb(0:hdl_z_m1)
      integer hdl_crsh_r_n_act_en_lkf(0:hdl_z_m1)
      integer hdl_crsh_c_n_act_en(0:hdl_z_m1)
      integer hdl_crsh_mtx_c_mx_abs_v(0:hdl_z_m1)
      integer hdl_crsh_mtx_c_v(0:hdl_z_m1)
      integer hdl_crsh_mtx_c_ix(0:hdl_z_m1)
      integer hdl_crsh_mtx_r_sa(0:hdl_z_m1)
      integer hdl_crsh_r_st(0:hdl_z_m1)
      integer hdl_crsh_c_st(0:hdl_z_m1)
 
      integer ix_hdl_crsh_r_ty
      integer ix_hdl_crsh_r_pri_lkb
      integer ix_hdl_crsh_r_pri_lkf
      integer ix_hdl_crsh_c_ty
      integer ix_hdl_crsh_r_n_act_en
      integer ix_hdl_crsh_r_n_act_en_hdr
      integer ix_hdl_crsh_r_n_act_en_lkb
      integer ix_hdl_crsh_r_n_act_en_lkf
      integer ix_hdl_crsh_c_n_act_en
      integer ix_hdl_crsh_mtx_c_mx_abs_v
      integer ix_hdl_crsh_mtx_c_v
      integer ix_hdl_crsh_mtx_c_ix
      integer ix_hdl_crsh_mtx_r_sa
      integer ix_hdl_crsh_r_st
      integer ix_hdl_crsh_c_st
 
      parameter (ix_hdl_crsh_r_ty =   0)
      parameter (ix_hdl_crsh_r_pri_lkb =
     &     ix_hdl_crsh_r_ty + hdl_z)
      parameter (ix_hdl_crsh_r_pri_lkf =
     &     ix_hdl_crsh_r_pri_lkb + hdl_z)
      parameter (ix_hdl_crsh_c_ty =
     &     ix_hdl_crsh_r_pri_lkf + hdl_z)
      parameter (ix_hdl_crsh_r_n_act_en =
     &     ix_hdl_crsh_c_ty + hdl_z)
      parameter (ix_hdl_crsh_r_n_act_en_hdr =
     &     ix_hdl_crsh_r_n_act_en + hdl_z)
      parameter (ix_hdl_crsh_r_n_act_en_lkb =
     &     ix_hdl_crsh_r_n_act_en_hdr + hdl_z)
      parameter (ix_hdl_crsh_r_n_act_en_lkf =
     &     ix_hdl_crsh_r_n_act_en_lkb + hdl_z)
      parameter (ix_hdl_crsh_c_n_act_en =
     &     ix_hdl_crsh_r_n_act_en_lkf + hdl_z)
      parameter (ix_hdl_crsh_mtx_c_mx_abs_v =
     &     ix_hdl_crsh_c_n_act_en + hdl_z)
      parameter (ix_hdl_crsh_mtx_c_v =
     &     ix_hdl_crsh_mtx_c_mx_abs_v + hdl_z)
      parameter (ix_hdl_crsh_mtx_c_ix =
     &     ix_hdl_crsh_mtx_c_v + hdl_z)
      parameter (ix_hdl_crsh_mtx_r_sa =
     &     ix_hdl_crsh_mtx_c_ix + hdl_z)
      parameter (ix_hdl_crsh_r_st =
     &     ix_hdl_crsh_mtx_r_sa + hdl_z)
      parameter (ix_hdl_crsh_c_st =
     &     ix_hdl_crsh_r_st + hdl_z)
 
      integer crsh_hdl_n_en
      integer n_crsh_hdl
      parameter (
     &     crsh_hdl_n_en = ix_hdl_crsh_c_st + hdl_z,
     &     n_crsh_hdl = crsh_hdl_n_en/hdl_z)
      integer crsh_hdl(0:crsh_hdl_n_en-1)
      common/ems_crsh_hdl_com/crsh_hdl
 
      equivalence (blk_crsh,
     &     crsh_hdl(ix_hdl_crsh_r_ty+hdl_os_blk_n))
      equivalence (hdl_crsh_r_ty(0),
     &     crsh_hdl(ix_hdl_crsh_r_ty))
      equivalence (hdl_crsh_r_pri_lkb(0),
     &     crsh_hdl(ix_hdl_crsh_r_pri_lkb))
      equivalence (hdl_crsh_r_pri_lkf(0),
     &     crsh_hdl(ix_hdl_crsh_r_pri_lkf))
      equivalence (hdl_crsh_c_ty(0),
     &     crsh_hdl(ix_hdl_crsh_c_ty))
      equivalence (hdl_crsh_r_n_act_en(0),
     &     crsh_hdl(ix_hdl_crsh_r_n_act_en))
      equivalence (hdl_crsh_r_n_act_en_hdr(0),
     &     crsh_hdl(ix_hdl_crsh_r_n_act_en_hdr))
      equivalence (hdl_crsh_r_n_act_en_lkb(0),
     &     crsh_hdl(ix_hdl_crsh_r_n_act_en_lkb))
      equivalence (hdl_crsh_r_n_act_en_lkf(0),
     &     crsh_hdl(ix_hdl_crsh_r_n_act_en_lkf))
      equivalence (hdl_crsh_c_n_act_en(0),
     &     crsh_hdl(ix_hdl_crsh_c_n_act_en))
      equivalence (hdl_crsh_mtx_c_mx_abs_v(0),
     &     crsh_hdl(ix_hdl_crsh_mtx_c_mx_abs_v))
      equivalence (hdl_crsh_mtx_c_v(0),
     &     crsh_hdl(ix_hdl_crsh_mtx_c_v))
      equivalence (hdl_crsh_mtx_c_ix(0),
     &     crsh_hdl(ix_hdl_crsh_mtx_c_ix))
      equivalence (hdl_crsh_mtx_r_sa(0),
     &     crsh_hdl(ix_hdl_crsh_mtx_r_sa))
      equivalence (hdl_crsh_r_st(0),
     &     crsh_hdl(ix_hdl_crsh_r_st))
      equivalence (hdl_crsh_c_st(0),
     &     crsh_hdl(ix_hdl_crsh_c_st))
 
c=======================================================================
c     CRASH pointers
c
      integer p_crsh_r_ty
      integer p_crsh_r_pri_lkb
      integer p_crsh_r_pri_lkf
      integer p_crsh_c_ty
      integer p_crsh_r_n_act_en
      integer p_crsh_r_n_act_en_hdr
      integer p_crsh_r_n_act_en_lkb
      integer p_crsh_r_n_act_en_lkf
      integer p_crsh_c_n_act_en
      integer p_crsh_mtx_c_mx_abs_v
      integer p_crsh_mtx_c_v
      integer p_crsh_mtx_c_ix
      integer p_crsh_mtx_r_sa
      integer p_crsh_r_st
      integer p_crsh_c_st
 
      integer ix_p_crsh_r_ty
      integer ix_p_crsh_r_pri_lkb
      integer ix_p_crsh_r_pri_lkf
      integer ix_p_crsh_c_ty
      integer ix_p_crsh_r_n_act_en
      integer ix_p_crsh_r_n_act_en_hdr
      integer ix_p_crsh_r_n_act_en_lkb
      integer ix_p_crsh_r_n_act_en_lkf
      integer ix_p_crsh_c_n_act_en
      integer ix_p_crsh_mtx_c_mx_abs_v
      integer ix_p_crsh_mtx_c_v
      integer ix_p_crsh_mtx_c_ix
      integer ix_p_crsh_mtx_r_sa
      integer ix_p_crsh_r_st
      integer ix_p_crsh_c_st
 
      parameter (ix_p_crsh_r_ty = 1)
      parameter (ix_p_crsh_r_pri_lkb =
     &     ix_p_crsh_r_ty + 1)
      parameter (ix_p_crsh_r_pri_lkf =
     &     ix_p_crsh_r_pri_lkb + 1)
      parameter (ix_p_crsh_c_ty =
     &     ix_p_crsh_r_pri_lkf + 1)
      parameter (ix_p_crsh_r_n_act_en =
     &     ix_p_crsh_c_ty + 1)
      parameter (ix_p_crsh_r_n_act_en_hdr =
     &     ix_p_crsh_r_n_act_en + 1)
      parameter (ix_p_crsh_r_n_act_en_lkb =
     &     ix_p_crsh_r_n_act_en_hdr + 1)
      parameter (ix_p_crsh_r_n_act_en_lkf =
     &     ix_p_crsh_r_n_act_en_lkb + 1)
      parameter (ix_p_crsh_c_n_act_en =
     &     ix_p_crsh_r_n_act_en_lkf + 1)
      parameter (ix_p_crsh_mtx_c_mx_abs_v =
     &     ix_p_crsh_c_n_act_en + 1)
      parameter (ix_p_crsh_mtx_c_v =
     &     ix_p_crsh_mtx_c_mx_abs_v + 1)
      parameter (ix_p_crsh_mtx_c_ix =
     &     ix_p_crsh_mtx_c_v + 1)
      parameter (ix_p_crsh_mtx_r_sa =
     &     ix_p_crsh_mtx_c_ix + 1)
      parameter (ix_p_crsh_r_st =
     &     ix_p_crsh_mtx_r_sa + 1)
      parameter (ix_p_crsh_c_st =
     &     ix_p_crsh_r_st + 1)
 
      integer n_crsh_p
      parameter (n_crsh_p = ix_p_crsh_c_st)
 
      integer crsh_p(n_crsh_p)
      common/ems_crsh_p_com/crsh_p
 
      equivalence (p_crsh_r_ty,
     &     crsh_p(ix_p_crsh_r_ty))
      equivalence (p_crsh_r_pri_lkb,
     &     crsh_p(ix_p_crsh_r_pri_lkb))
      equivalence (p_crsh_r_pri_lkf,
     &     crsh_p(ix_p_crsh_r_pri_lkf))
      equivalence (p_crsh_c_ty,
     &     crsh_p(ix_p_crsh_c_ty))
      equivalence (p_crsh_r_n_act_en,
     &     crsh_p(ix_p_crsh_r_n_act_en))
      equivalence (p_crsh_r_n_act_en_hdr,
     &     crsh_p(ix_p_crsh_r_n_act_en_hdr))
      equivalence (p_crsh_r_n_act_en_lkb,
     &     crsh_p(ix_p_crsh_r_n_act_en_lkb))
      equivalence (p_crsh_r_n_act_en_lkf,
     &     crsh_p(ix_p_crsh_r_n_act_en_lkf))
      equivalence (p_crsh_c_n_act_en,
     &     crsh_p(ix_p_crsh_c_n_act_en))
      equivalence (p_crsh_mtx_c_mx_abs_v,
     &     crsh_p(ix_p_crsh_mtx_c_mx_abs_v))
      equivalence (p_crsh_mtx_c_v,
     &     crsh_p(ix_p_crsh_mtx_c_v))
      equivalence (p_crsh_mtx_c_ix,
     &     crsh_p(ix_p_crsh_mtx_c_ix))
      equivalence (p_crsh_mtx_r_sa,
     &     crsh_p(ix_p_crsh_mtx_r_sa))
      equivalence (p_crsh_r_st,
     &     crsh_p(ix_p_crsh_r_st))
      equivalence (p_crsh_c_st,
     &     crsh_p(ix_p_crsh_c_st))
 
c=======================================================================
c     CRASH parameters
c
      integer crsh_tl_rfn_msk_abs_pv_v_bt
      integer crsh_tl_rfn_msk_rlv_pv_v_bt
      parameter (
     &     crsh_tl_rfn_msk_abs_pv_v_bt = bt1,
     &     crsh_tl_rfn_msk_rlv_pv_v_bt = bt2)
 
      integer crsh_vr_st_no_act
      integer crsh_vr_st_act
      parameter (
     &     crsh_vr_st_no_act = 0,
     &     crsh_vr_st_act =    1)
c
c     Set minimum and maximum priority values
c
      integer crsh_mn_pri_v
      integer crsh_mx_pri_v
      parameter (
     &     crsh_mn_pri_v = 0,
     &     crsh_mx_pri_v = 6)
c
c     Rows and columns with this priority value are never active
c
      integer crsh_no_act_pri_v
      parameter (crsh_no_act_pri_v = crsh_mn_pri_v)
c
c     Variable types for basis-preserving LTSSF crash
c
      integer crsh_vr_ty_bc
      integer crsh_vr_ty_non_bc
      parameter (crsh_vr_ty_bc =     0)
      parameter (crsh_vr_ty_non_bc = 1)
c
c     Variable types for standard LTSSF crash
c
      integer crsh_vr_ty_fx
      integer crsh_vr_ty_fx_ze_fs
      integer crsh_vr_ty_2_sd
      integer crsh_vr_ty_2_sd_ze_fs
      integer crsh_vr_ty_1_sd
      integer crsh_vr_ty_1_sd_ze_fs
      integer crsh_vr_ty_fr
      parameter (crsh_vr_ty_fx =          0)
      parameter (crsh_vr_ty_fx_ze_fs =    1)
      parameter (crsh_vr_ty_2_sd =        2)
      parameter (crsh_vr_ty_2_sd_ze_fs =  3)
      parameter (crsh_vr_ty_1_sd =        4)
      parameter (crsh_vr_ty_1_sd_ze_fs =  5)
      parameter (crsh_vr_ty_fr =          6)
c
c     Limits on numbers of variable types
c
      integer crsh_f_vr_ty
      integer crsh_l_vr_ty
      parameter (crsh_f_vr_ty = crsh_vr_ty_fx)
      parameter (crsh_l_vr_ty = crsh_vr_ty_fr)
 
      character*3 ch3_crsh_vr_ty(crsh_f_vr_ty:crsh_l_vr_ty)
      common/ems_crsh_vr_ty_com/ch3_crsh_vr_ty
 
      integer iz_crsh_com
      integer crsh_ou_cn
      integer crsh_r_ty_pri_v(crsh_f_vr_ty:crsh_l_vr_ty)
      integer crsh_c_ty_pri_v(crsh_f_vr_ty:crsh_l_vr_ty)
      integer crsh_vr_ty_og_n_r(crsh_f_vr_ty:crsh_l_vr_ty)
      integer crsh_vr_ty_rm_n_r(crsh_f_vr_ty:crsh_l_vr_ty)
      integer crsh_vr_ty_og_n_c(crsh_f_vr_ty:crsh_l_vr_ty)
      integer crsh_vr_ty_add_n_c(crsh_f_vr_ty:crsh_l_vr_ty)
      integer crsh_r_pri_v_hdr(crsh_mn_pri_v:crsh_mx_pri_v)
      common/ems_crsh_pri_com/
     &     iz_crsh_com,
     &     crsh_ou_cn,
     &     crsh_r_ty_pri_v,
     &     crsh_c_ty_pri_v,
     &     crsh_vr_ty_og_n_r,
     &     crsh_vr_ty_rm_n_r,
     &     crsh_vr_ty_og_n_c,
     &     crsh_vr_ty_add_n_c,
     &     crsh_r_pri_v_hdr
 
      integer n_crsh_ps
      integer lg_bs_n_fx_vr
      integer crsh_bs_n_fx_vr
      common/ems_crsh_da_com/
     &     n_crsh_ps,
     &     lg_bs_n_fx_vr,
     &     crsh_bs_n_fx_vr
 
      double precision tl_crsh_abs_pv_v
      double precision tl_crsh_rlv_pv_v
      common/ems_crsh_rl_ct_vr/
     &     tl_crsh_abs_pv_v,
     &     tl_crsh_rlv_pv_v
 
      integer crsh_msk_ze_fs_bt
      integer crsh_msk_ze_pr_act_bt
      parameter (crsh_msk_ze_fs_bt = bt1)
      parameter (crsh_msk_ze_pr_act_bt = bt2)
 
      integer crsh_r_pri(crsh_f_vr_ty:crsh_l_vr_ty)
      integer crsh_c_pri(crsh_f_vr_ty:crsh_l_vr_ty)
      common/ems_crsh_pri_com/
     &     crsh_r_pri, crsh_c_pri
