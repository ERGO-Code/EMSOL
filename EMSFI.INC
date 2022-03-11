      integer ems_wr_fi_fmt_vers_n
      integer ems_wr_fi_fmt_rv_n
      parameter (
     &     ems_wr_fi_fmt_vers_n = 1,
     &     ems_wr_fi_fmt_rv_n =   0)
      integer ems_fi_cmt_ky_n_ch
      integer ems_fi_mps_ml_nm_ky_n_ch
      integer ems_fi_fmt_ky_n_ch
      integer ems_fi_mx_ky_n_ch
      integer ems_fi_mn_ky_n_ch
      integer ems_fi_n_r_ky_n_ch
      integer ems_fi_n_c_ky_n_ch
      integer ems_fi_n_mtx_el_ky_n_ch
      integer ems_fi_r_ky_n_ch
      integer ems_fi_c_ky_n_ch
      integer ems_fi_mtx_ky_n_ch
      integer ems_fi_c_bd_ky_n_ch
      integer ems_fi_r_bd_ky_n_ch
      integer ems_fi_c_co_ky_n_ch
      integer ems_fi_nm_ky_n_ch
      integer ems_fi_ob_fn_cs_ky_n_ch
      integer ems_fi_r_co_ky_n_ch
      integer ems_fi_pwl_ky_n_ch
      integer ems_fi_bs_ky_n_ch
      integer ems_fi_bs_in_alt_fmt_ky_n_ch
      integer ems_fi_e_lin_ky_n_ch
      integer ems_fi_ky_mx_n_ch
      parameter (
     &     ems_fi_cmt_ky_n_ch =            1,
     &     ems_fi_mps_ml_nm_ky_n_ch =      4,
     &     ems_fi_fmt_ky_n_ch =           15,
     &     ems_fi_mx_ky_n_ch =             8,
     &     ems_fi_mn_ky_n_ch =             8,
     &     ems_fi_n_r_ky_n_ch =            6,
     &     ems_fi_n_c_ky_n_ch =            9,
     &     ems_fi_n_mtx_el_ky_n_ch =      17,
     &     ems_fi_r_ky_n_ch =              4,
     &     ems_fi_c_ky_n_ch =              7,
     &     ems_fi_mtx_ky_n_ch =            6,
     &     ems_fi_c_bd_ky_n_ch =          13,
     &     ems_fi_r_bd_ky_n_ch =          10,
     &     ems_fi_c_co_ky_n_ch =          12,
     &     ems_fi_nm_ky_n_ch =             5,
     &     ems_fi_ob_fn_cs_ky_n_ch =      18,
     &     ems_fi_r_co_ky_n_ch =           9,
     &     ems_fi_pwl_ky_n_ch =           16,
     &     ems_fi_bs_ky_n_ch =             5,
     &     ems_fi_bs_in_alt_fmt_ky_n_ch = 27,
     &     ems_fi_e_lin_ky_n_ch =         10,
     &     ems_fi_ky_mx_n_ch =            ems_fi_bs_in_alt_fmt_ky_n_ch)
     &
      integer ml_nm_mx_n_ch
      integer ml_nm_mx_n_rl
      parameter (
     &     ml_nm_mx_n_ch = 32,
     &     ml_nm_mx_n_rl = (ml_nm_mx_n_ch+7)/8)
 
      integer ems_fi_iz_com_fg1, ems_fi_iz_com_fg2
      common/ems_ems_fi_iz_com_fg_com/
     &     ems_fi_iz_com_fg1, ems_fi_iz_com_fg2
 
      integer ems_fi_mx_li_ln
      parameter (ems_fi_mx_li_ln = 80)
      character*(ems_fi_mx_li_ln) ems_fi_li
      common/ems_ems_fi_li_com/ems_fi_li
 
      character*(ems_fi_cmt_ky_n_ch) ems_fi_cmt_ky_1
      character*(ems_fi_mps_ml_nm_ky_n_ch) ems_fi_mps_ml_nm_ky
      character*(ems_fi_cmt_ky_n_ch) ems_fi_cmt_ky_2
      character*(ems_fi_cmt_ky_n_ch) ems_fi_cmt_ky_3
      character*(ems_fi_fmt_ky_n_ch) ems_fi_fmt_ky
      character*(ems_fi_mx_ky_n_ch) ems_fi_mx_ky
      character*(ems_fi_mn_ky_n_ch) ems_fi_mn_ky
      character*(ems_fi_n_r_ky_n_ch) ems_fi_n_r_ky
      character*(ems_fi_n_c_ky_n_ch) ems_fi_n_c_ky
      character*(ems_fi_n_mtx_el_ky_n_ch) ems_fi_n_mtx_el_ky
      character*(ems_fi_r_ky_n_ch) ems_fi_r_ky
      character*(ems_fi_c_ky_n_ch) ems_fi_c_ky
      character*(ems_fi_mtx_ky_n_ch) ems_fi_mtx_ky
      character*(ems_fi_c_bd_ky_n_ch) ems_fi_c_bd_ky
      character*(ems_fi_r_bd_ky_n_ch) ems_fi_r_bd_ky
      character*(ems_fi_c_co_ky_n_ch) ems_fi_c_co_ky
      character*(ems_fi_nm_ky_n_ch) ems_fi_nm_ky
      character*(ems_fi_ob_fn_cs_ky_n_ch) ems_fi_ob_fn_cs_ky
      character*(ems_fi_r_co_ky_n_ch) ems_fi_r_co_ky
      character*(ems_fi_pwl_ky_n_ch) ems_fi_pwl_ky
      character*(ems_fi_bs_ky_n_ch) ems_fi_bs_ky
      character*(ems_fi_bs_in_alt_fmt_ky_n_ch) ems_fi_bs_in_alt_fmt_ky
      character*(ems_fi_e_lin_ky_n_ch) ems_fi_e_lin_ky
 
      common/ems_ems_fi_ky_com/
     &     ems_fi_cmt_ky_1, ems_fi_cmt_ky_2, ems_fi_cmt_ky_3,
     &     ems_fi_mps_ml_nm_ky,
     &     ems_fi_fmt_ky, ems_fi_mx_ky, ems_fi_mn_ky,
     &     ems_fi_n_r_ky, ems_fi_n_c_ky, ems_fi_n_mtx_el_ky,
     &     ems_fi_r_ky, ems_fi_c_ky,
     &     ems_fi_mtx_ky, ems_fi_c_bd_ky, ems_fi_r_bd_ky,
     &     ems_fi_c_co_ky, ems_fi_nm_ky,
     &     ems_fi_ob_fn_cs_ky, ems_fi_r_co_ky, ems_fi_pwl_ky,
     &     ems_fi_bs_ky, ems_fi_bs_in_alt_fmt_ky, ems_fi_e_lin_ky
 
      character*8 ems_fi_ml_nm
      common/ems_ems_fi_ml_nm_com/ems_fi_ml_nm
 
      integer rd_ml_fi_rn_mps
      integer rd_ml_fi_rn_basi
      parameter (
     &     rd_ml_fi_rn_mps =  0,
     &     rd_ml_fi_rn_basi = 1)
      integer wr_ml_fi_rn_bcdo
      integer wr_ml_fi_rn_baso
      parameter (
     &     wr_ml_fi_rn_bcdo = 0,
     &     wr_ml_fi_rn_baso = 1)
 
      integer ems_fi_io_mtx_bt
      integer ems_fi_io_c_lb_bt
      integer ems_fi_io_c_ub_bt
      integer ems_fi_io_r_lb_bt
      integer ems_fi_io_r_ub_bt
      integer ems_fi_io_c_co_bt
      integer ems_fi_io_r_co_bt
      integer ems_fi_io_c_nm_bt
      integer ems_fi_io_r_nm_bt
      integer ems_fi_io_pwl_bt
      integer ems_fi_io_ct_vr_bt
      integer ems_fi_io_bs_bt
      integer ems_fi_io_bs_in_alt_fmt_bt
      parameter (
     &     ems_fi_io_mtx_bt =           bt1,
     &     ems_fi_io_c_lb_bt =          bt2,
     &     ems_fi_io_c_ub_bt =          bt3,
     &     ems_fi_io_r_lb_bt =          bt4,
     &     ems_fi_io_r_ub_bt =          bt5,
     &     ems_fi_io_c_co_bt =          bt6,
     &     ems_fi_io_r_co_bt =          bt7,
     &     ems_fi_io_c_nm_bt =          bt8,
     &     ems_fi_io_r_nm_bt =          bt9,
     &     ems_fi_io_pwl_bt =           bt10,
     &     ems_fi_io_ct_vr_bt =         bt11,
     &     ems_fi_io_bs_bt =            bt12,
     &     ems_fi_io_bs_in_alt_fmt_bt = bt13)
 
      integer ems_fi_io_c_bd_msk
      integer ems_fi_io_r_bd_msk
      integer ems_fi_io_bd_msk
      integer ems_fi_io_co_msk
      integer ems_fi_io_nm_msk
      integer ems_fi_io_mn_ml_msk
      parameter (
     &     ems_fi_io_c_bd_msk = ems_fi_io_c_lb_bt +  ems_fi_io_c_ub_bt,
     &     ems_fi_io_r_bd_msk = ems_fi_io_r_lb_bt +  ems_fi_io_r_ub_bt,
     &     ems_fi_io_bd_msk =   ems_fi_io_c_bd_msk + ems_fi_io_r_bd_msk,
     &     ems_fi_io_co_msk =   ems_fi_io_c_co_bt +  ems_fi_io_r_co_bt,
     &     ems_fi_io_nm_msk =   ems_fi_io_c_nm_bt +  ems_fi_io_r_nm_bt,
     &     ems_fi_io_mn_ml_msk =
     &     ems_fi_io_mtx_bt + ems_fi_io_bd_msk + ems_fi_io_c_co_bt)
 
      integer ems_fi_io_msk
      common/ems_ems_fi_i_ct_vr_com/
     &     ems_fi_io_msk
