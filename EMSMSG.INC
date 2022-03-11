CM
c      integer info, warn, er, serious
c      parameter (
c     &     info = 0,
c     &     warn = 100,
c     &     er = 200,
c     &     serious = 300)
c
c     Changed names on 18/05/98
c
      integer ems_msg_lvl_info
      integer ems_msg_lvl_warn
      integer ems_msg_lvl_er
      integer ems_msg_lvl_serious
      parameter (
     &     ems_msg_lvl_info =      0,
     &     ems_msg_lvl_warn =    100,
     &     ems_msg_lvl_er =      200,
     &     ems_msg_lvl_serious = 300)
 
      integer info_msg_n, warn_msg_n, er_msg_n, serious_msg_n
      integer bug_msg_n
      parameter (
     &     info_msg_n =    1000,
     &     warn_msg_n =    4000,
     &     er_msg_n =      6500,
     &     serious_msg_n = 8000,
     &     bug_msg_n =     9000)
 
      integer ems_mx_li_ln
      parameter (ems_mx_li_ln = 500)
      character*(ems_mx_li_ln) ems_li
      common/ems_com_li/ems_li
c
      integer n_ems_msg_ct_vr
      parameter (n_ems_msg_ct_vr=13)
      integer ems_msg_ct_vr(n_ems_msg_ct_vr)
      common/ems_msg_ct_vr_com/ems_msg_ct_vr
 
      integer ems_msg_iz_ct_vr_com_fg1
      integer ems_msg_iz_ct_vr_com_fg2
      integer ems_msg_cod
      integer ems_msg_wr_cn
      integer ems_msg_er_cn
      integer ems_msg_no_prt_fm
      integer ems_msg_no_prt_t
      integer ems_msg_rp_msg_n
      integer ems_msg_wr_li_ln
      integer ems_msg_n_pg_li
      integer ems_msg_my_prcs_n
      integer ems_msg_pg_n
      integer ems_msg_pg_li_n
      equivalence (ems_msg_iz_ct_vr_com_fg1, ems_msg_ct_vr(1))
      equivalence (ems_msg_iz_ct_vr_com_fg2, ems_msg_ct_vr(2))
      equivalence (ems_msg_cod,              ems_msg_ct_vr(3))
      equivalence (ems_msg_wr_cn,            ems_msg_ct_vr(4))
      equivalence (ems_msg_er_cn,            ems_msg_ct_vr(5))
      equivalence (ems_msg_no_prt_fm,        ems_msg_ct_vr(6))
      equivalence (ems_msg_no_prt_t,         ems_msg_ct_vr(7))
      equivalence (ems_msg_rp_msg_n,         ems_msg_ct_vr(8))
      equivalence (ems_msg_wr_li_ln,         ems_msg_ct_vr(9))
      equivalence (ems_msg_n_pg_li,          ems_msg_ct_vr(10))
      equivalence (ems_msg_my_prcs_n,        ems_msg_ct_vr(11))
      equivalence (ems_msg_pg_n,             ems_msg_ct_vr(12))
      equivalence (ems_msg_pg_li_n,          ems_msg_ct_vr(13))
