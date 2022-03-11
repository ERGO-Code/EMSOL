      program drv_ts_dev
      implicit none
      include 'DS.h'
      include 'EMSV.INC'
c      include 'INVCTVR.INC'
c      include 'TSML.INC'
c      include 'EMSMMGRT.INC'
      integer rt_cod, rd_ml_cn
      integer lc_ds_n_en, lc_is_n_en
      data rd_ml_cn/15/
 
      print*, 'Reached first line'
 
      open(unit = rd_ml_cn, file = 'ml.mps')

      lc_ds_n_en = ds_n_en
c      print*, ' Enter lc_ds_n_en '
c      read*, lc_ds_n_en
      lc_is_n_en = lc_ds_n_en*2

      call ts_ems_sslv(rt_cod, ds, is, ds_n_en, is_n_en, rd_ml_cn)
 
      stop
      end
