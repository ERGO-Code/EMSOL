C->>> -----------------------------------------------> ems_g_emsol_p <<<
c     Get the EMSOL pointers
c
      subroutine ems_g_emsol_p(rt_cod, is)
      implicit none
      integer rt_cod, is(0:*)
c      print*, 'Calling TRUE ems_g_emsol_p'
      call ems_g_ml_p(rt_cod, is)
      if (rt_cod .ne. 0) goto 7000
      call ems_g_rsmi_p(rt_cod, is)
      if (rt_cod .ne. 0) goto 7000
      call ems_g_inv_p(rt_cod, is)
      if (rt_cod .ne. 0) goto 7000
 7000 continue
      return
      end
 
C->>> -----------------------------------------> ems_se_emsol_p_no_p <<<
c     Get the EMSOL pointers
c
      subroutine ems_se_emsol_p_no_p(rt_cod, is)
      implicit none
      integer rt_cod, is(0:*)
c      print*, 'Calling TRUE ems_se_emsol_p_no_p'
      call ems_se_ml_p_no_p(rt_cod, is)
      if (rt_cod .ne. 0) goto 7000
      call ems_se_rsmi_p_no_p(rt_cod)
      if (rt_cod .ne. 0) goto 7000
      call ems_se_inv_p_no_p(rt_cod)
      if (rt_cod .ne. 0) goto 7000
 7000 continue
      return
      end
 
