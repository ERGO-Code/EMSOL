CM
C->>> --------------------------------------------------> ems_rp_mac <<<
c     Reports the macro settings (for this file anyway!)
c
      subroutine ems_rp_mac
      implicit none
      include 'EMSMSG.INC'
      integer su_lib_mac
      integer su_pent_fq_mac
      su_lib_mac = 0
      su_pent_fq_mac = 0
CM      IF (emsol_asm .EQ. 1) THEN
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)'emsol_asm'
C?      call ems_msg_wr_li(info_msg_n)
CM      ENDIF
CM      IF (emsol_da .EQ. 1) THEN
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)'emsol_da'
C?      call ems_msg_wr_li(info_msg_n)
CM      ENDIF
CM      IF (emsol_deb .EQ. 1) THEN
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)'emsol_deb'
C?      call ems_msg_wr_li(info_msg_n)
CM      ENDIF
CM      IF (emsol_dev .EQ. 1) THEN
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)'emsol_dev'
C?      call ems_msg_wr_li(info_msg_n)
CM      ENDIF
CM      IF (emsol_epc .EQ. 1) THEN
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)'emsol_epc'
C?      call ems_msg_wr_li(info_msg_n)
CM      ENDIF
CM      IF (emsol_km .EQ. 1) THEN
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)'emsol_km'
C?      call ems_msg_wr_li(info_msg_n)
CM      ENDIF
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)'emsol_tt'
C?      call ems_msg_wr_li(info_msg_n)
CM      ENDIF
CM      IF (emsol_ttrec .EQ. 1) THEN
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)'emsol_ttrec'
C?      call ems_msg_wr_li(info_msg_n)
CM      ENDIF
CM      IF (emsol_xa .EQ. 1) THEN
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)'emsol_xa'
C?      call ems_msg_wr_li(info_msg_n)
CM      ENDIF
CM      IF (sun_lib .EQ. 1) THEN
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)'sun_lib'
C?      call ems_msg_wr_li(info_msg_n)
C?      su_lib_mac = su_lib_mac + 1
CM      ENDIF
CM      IF (t3d_lib .EQ. 1) THEN
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)'t3d_lib'
C?      call ems_msg_wr_li(info_msg_n)
C?      su_lib_mac = su_lib_mac + 1
CM      ENDIF
CM      IF (ibm_lib .EQ. 1) THEN
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)'ibm_lib'
C?      call ems_msg_wr_li(info_msg_n)
C?      su_lib_mac = su_lib_mac + 1
CM      ENDIF
CM      IF (sgi_lib .EQ. 1) THEN
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)'sgi_lib'
C?      call ems_msg_wr_li(info_msg_n)
C?      su_lib_mac = su_lib_mac + 1
CM      ENDIF
CM      IF (nt_lib .EQ. 1) THEN
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)'nt_lib'
C?      call ems_msg_wr_li(info_msg_n)
C?      su_lib_mac = su_lib_mac + 1
CM      ENDIF
CM      IF (fps_lib .EQ. 1) THEN
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)'fps_lib'
C?      call ems_msg_wr_li(info_msg_n)
C?      su_lib_mac = su_lib_mac + 1
CM      ENDIF
CM      IF (ftn77_lib .EQ. 1) THEN
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)'ftn77_lib'
      call ems_msg_wr_li(info_msg_n)
      su_lib_mac = su_lib_mac + 1
CM      ENDIF
CM      IF (t3d_tt .EQ. 1) THEN
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)'t3d_tt'
C?      call ems_msg_wr_li(info_msg_n)
CM      ENDIF
CM      IF (pent_tt .EQ. 1) THEN
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)'pent_tt'
C?      call ems_msg_wr_li(info_msg_n)
CM      ENDIF
CM      IF (pent_fq_90 .EQ. 1) THEN
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)'pent_fq_90'
C?      call ems_msg_wr_li(info_msg_n)
C?      su_pent_fq_mac = su_pent_fq_mac + 1
CM      ENDIF
CM      IF (pent_fq_135 .EQ. 1) THEN
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)'pent_fq_135'
C?      call ems_msg_wr_li(info_msg_n)
C?      su_pent_fq_mac = su_pent_fq_mac + 1
CM      ENDIF
      if (su_lib_mac .ne. 1) goto 8000
CM      IF (pent_tt .EQ. 1) THEN
C?      if (su_pent_fq_mac .ne. 1) goto 8010
CM      ENDIF
 7000 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      go to 7000
CM      IF (pent_tt .EQ. 1) THEN
C? 8010 continue
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)
C?      call ems_msg_wr_li(bug_msg_n)
C?      go to 7000
CM      ENDIF
 9100 format('Compiled with value 1 for macro ', a)
 9800 format('Not compiled with exactly one *_lib macro set to 1')
CM      IF (pent_tt .EQ. 1) THEN
C? 9801 format('Not compiled with exactly one pent_fq_* macro set to 1')
CM      ENDIF
      end
C->>> ---------------------------------------------------> ems_flush <<<
c     Shell to the architecture-specific flush subroutine.
c
      subroutine ems_flush(cn)
      implicit none
      integer cn
 
CM      IF (ibm_lib .EQ. 1) THEN
C?      call flush_(cn)
CM      ELSE IF (ftn77_lib .EQ. 1) THEN
c
c     No flush with ftn77_lib!
c
CM      ELSE IF (nt_lib .EQ. 1) THEN
C?c
C?c     No flush with NT_lib!
C?c
CM      ELSE
C?c
C?c     On SUN, T3D SGI and FPS the flush subroutine is the same
C?c
C?      call flush(cn)
CM      ENDIF
      return
      end
C->>> -------------------------------------------------> ems_pos_mod <<<
c     Returns the value of m mod n in the range 0..n-1. This is
c     guaranteed on Sun (and IBM?).
c
      integer function ems_pos_mod(m, n)
      implicit none
      integer m, n
      integer pos_mod
      pos_mod = mod(m, n)
CM      IF (t3d_lib .EQ. 1) THEN
C?      if (pos_mod .lt. 0) pos_mod = pos_mod + n
CM      ENDIF
      ems_pos_mod = pos_mod
      return
      end
CM      IF (sun_lib .EQ. 1 .OR. sgi_lib .EQ. 1) THEN
C?c
C?c     On SUN4 and SGI, get and write system information on this Unix
C?c     process.
C?c
C?C->>> -----------------------------------------------> ems_wr_rusage <<<
C?      subroutine ems_wr_rusage(cn)
C?      implicit none
C?      integer cn
C?      double precision usr_tt, sys_tt
C?      integer maxrss, ixrss, idrss, isrss, minflt, majflt, nswap
C?      integer inblock, oublock, msgsnd, msgrcv, nsignals, nvcsw
C?      integer nivcsw
C?
C?      call ems_getrusage(usr_tt, sys_tt,
C?     &     maxrss, ixrss, idrss, isrss, minflt, majflt, nswap,
C?     &     inblock, oublock, msgsnd, msgrcv, nsignals, nvcsw, nivcsw)
C?
C?      write(cn, 9000)
C?      write(cn, 9010)usr_tt
C?      write(cn, 9020)sys_tt
C?      write(cn, 9030)maxrss
C?c      write(cn, 9040)ixrss
C?      write(cn, 9050)idrss
C?c      write(cn, 9060)isrss
C?      write(cn, 9070)minflt
C?      write(cn, 9080)majflt
C?      write(cn, 9090)nswap
C?      write(cn, 9100)inblock
C?      write(cn, 9110)oublock
C?      write(cn, 9120)msgsnd
C?      write(cn, 9130)msgrcv
C?      write(cn, 9140)nsignals
C?      write(cn, 9150)nvcsw
C?      write(cn, 9160)nivcsw
C?      return
C? 9000 format('This process: ')
C? 9010 format('CPU    time = ', f10.2)
C? 9020 format('System time = ', f10.2)
C? 9030 format('Maximum  resident set size = ', i9)
C?c 9040 format('Currently 0 = ', i9)
C? 9050 format('Integral resident set size = ', i9)
C?c 9060 format('Currently 0 = ', i9)
C? 9070 format('Page faults not requiring physical I/O = ', i9)
C? 9080 format('Page faults     requiring physical I/O = ', i9)
C? 9090 format('Swaps = ', i9)
C? 9100 format('Block input  operations = ', i9)
C? 9110 format('Block output operations = ', i9)
C? 9120 format('Messages sent     = ', i9)
C? 9130 format('Messages received = ', i9)
C? 9140 format('Signals received  = ', i9)
C? 9150 format('  Voluntary context switches = ', i9)
C? 9160 format('Involuntary context switches = ', i9)
C?      end
CM      ENDIF
