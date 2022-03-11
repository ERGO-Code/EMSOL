CM
C->>> --------------------------------------------------> ems_iz_sos <<<
c     Dummy initialise SOS.
c
      subroutine ems_iz_sos(
     &     ds, is, usr_rt_cod)
      implicit none
      double precision ds
      integer is, usr_rt_cod
      return
      end
 
C->>> ----------------------------------------------------> ems_itru <<<
c     Dummy iteration user exit.
c
      subroutine ems_itru(
     &     ds, is, it_xit_reason, usr_rt_cod)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'ITXITCS.INC'
      include 'RSMICOM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      double precision ds(0:ds_n_en_m1)
      integer is
      integer it_xit_reason, usr_rt_cod
      return
      end
C->>> -----------------------------------------> ems_sos_xit_af_cz_c <<<
c     Dummy SOS exit after CHUZC.
c
      subroutine ems_sos_xit_af_cz_c(
     &     ds, is,
     &     usr_rt_cod, vr_t_en_bs, vr_t_lv_bs, lp_ph)
      implicit none
      double precision ds(0:*)
      integer is(0:*), usr_rt_cod, vr_t_en_bs, vr_t_lv_bs, lp_ph
      return
      end
C->>> ----------------------------------------> ems_sos_xit_af_pr_it <<<
c     Dummy SOS exit after primal activity.
c
      subroutine ems_sos_xit_af_pr_it(
     &     ds, is,
     &     usr_rt_cod, vr_t_en_bs, vr_t_lv_bs, lp_ph)
      implicit none
      double precision ds(0:*)
      integer is(0:*), usr_rt_cod, vr_t_en_bs, vr_t_lv_bs, lp_ph
      return
      end
C->>> ----------------------------------------> ems_sos_xit_af_reset <<<
c     Dummy SOS exit after reset.
c
      subroutine ems_sos_xit_af_reset(
     &     ds, is,
     &     usr_rt_cod, vr_t_en_bs, vr_t_lv_bs, lp_ph)
      implicit none
      double precision ds(0:*)
      integer is(0:*), usr_rt_cod, vr_t_en_bs, vr_t_lv_bs, lp_ph
      return
      end
