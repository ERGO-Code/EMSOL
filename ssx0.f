CM
C->>> --------------------------------------------------> ems_cp_hdl <<<
      subroutine ems_cp_hdl(fm_hdl, t_hdl)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      integer fm_hdl(0:hdl_z_m1), t_hdl(0:hdl_z_m1)
      integer hdl_ix
      do 10, hdl_ix = 0, hdl_z_m1
         t_hdl(hdl_ix) = fm_hdl(hdl_ix)
 10   continue
      return
      end
 
C->>> ----------------------------------------------> ems_hdl_eq_hdl <<<
      logical function ems_hdl_eq_hdl(hdl1, hdl2)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      integer hdl1(0:hdl_z_m1), hdl2(0:hdl_z_m1)
      integer hdl_ix
      logical hdl_eq_hdl
      hdl_eq_hdl = .true.
      do 10, hdl_ix = 0, hdl_z_m1
         if (hdl1(hdl_ix) .ne. hdl2(hdl_ix)) then
            hdl_eq_hdl = .false.
            goto 20
         endif
 10   continue
 20   continue
      ems_hdl_eq_hdl = hdl_eq_hdl
      return
      end
 
C->>> ------------------------------------------------> ems_exch_hdl <<<
      subroutine ems_exch_hdl(hdl1, hdl2)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      integer hdl1(0:hdl_z_m1), hdl2(0:hdl_z_m1)
      integer hdl(0:hdl_z_m1), hdl_ix
      do 10, hdl_ix = 0, hdl_z_m1
         hdl(hdl_ix) = hdl1(hdl_ix)
 10   continue
      do 20, hdl_ix = 0, hdl_z_m1
         hdl1(hdl_ix) = hdl2(hdl_ix)
 20   continue
      do 30, hdl_ix = 0, hdl_z_m1
         hdl2(hdl_ix) = hdl(hdl_ix)
 30   continue
      return
      end
 
