      integer mx_n_sv_mv_bd
      parameter (mx_n_sv_mv_bd = 100)
 
      double precision sv_mv_bd_v(mx_n_sv_mv_bd)
      integer n_sv_mv_bd
      integer sv_mv_bd_vr_n(mx_n_sv_mv_bd)
      common/ems_sv_mv_bd_com/
     &     sv_mv_bd_v,
     &     n_sv_mv_bd,
     &     sv_mv_bd_vr_n
