CM
C->>> -----------------------------------------------> ems_msg_wr_li <<<
      subroutine ems_msg_wr_li(usr_msg_n)
      implicit none
      include 'EMSV.INC'
      include 'EMSVERS.INC'
      include 'EMSMSG.INC'
      include 'EMSMSGN.INC'
      integer usr_msg_n
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_eq_i2
CM      ENDIF
      character*4 ch4_msg_n
      integer ems_ln_t_l_ch
      integer ln_t_l_ch, p0, p1, msg_n, sa
      integer parlp_li_sa_ln, li_sa_ln
      parameter (
     &     parlp_li_sa_ln = 13,
     &     li_sa_ln = 10)
      character*(li_sa_ln) li_sa
      character*(parlp_li_sa_ln) parlp_li_sa
      character*80 msg_6_li
      logical no_prcs_n, wr_cn0
      character*13 ch13_vers
      integer ch13_vers_ln
 
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(ems_msg_iz_ct_vr_com_fg1,
C?     &     ems_msg_iz_ct_vr_com_fg2))
CM      ELSE
      if (ems_msg_iz_ct_vr_com_fg1 .eq. ems_msg_iz_ct_vr_com_fg2)
CM      ENDIF
     &     call ems_msg_iz_ct_vr_com
      msg_n = usr_msg_n
      no_prcs_n = ems_msg_my_prcs_n .lt. 0
      wr_cn0 =
     &     ems_msg_wr_cn .ne. ems_msg_er_cn .and.
     &     ems_msg_wr_cn .ne. 6 .and.
     &     msg_n .ge. bs_warn_msg_n
      if (ems_msg_no_prt_fm .le. 0 .or.
     &     (msg_n .ge. ems_msg_no_prt_fm .and.
     &     msg_n .le. ems_msg_no_prt_t)) then
c
c     If printing has been switched off still maintain the return code.
c
         if (msg_n .ge. bs_serious_msg_n) then
            ems_msg_cod = ems_msg_lvl_serious
         else if (msg_n .ge. bs_er_msg_n) then
            ems_msg_cod = max(ems_msg_lvl_er, ems_msg_cod)
         else if (msg_n .ge. bs_warn_msg_n) then
            ems_msg_cod = max(ems_msg_lvl_warn, ems_msg_cod)
         else if (msg_n .ge. bs_info_msg_n) then
            ems_msg_cod = max(ems_msg_lvl_info, ems_msg_cod)
         endif
         goto 7000
      endif
      if (.not. no_prcs_n) write(parlp_li_sa, 9800)ems_msg_my_prcs_n
 10   continue
      if (ems_msg_pg_n .eq. 0 .and. msg_n .ne. 6) then
         ems_msg_pg_n = 1
         ems_msg_pg_li_n = 1
         if (ems_msg_rp_msg_n .eq. 1) li_sa = '1'//li_prefix//'0006I '
         call ems_msg_g_ch13_vers(ch13_vers, ch13_vers_ln)
         write(msg_6_li, 9100)package, ch13_vers(1:ch13_vers_ln),
     &        ems_msg_pg_n
         ln_t_l_ch = ems_ln_t_l_ch(msg_6_li)
         if (no_prcs_n) then
            p0 = 1
            p1 = min(ln_t_l_ch, ems_msg_wr_li_ln-li_sa_ln)
            if (ems_msg_rp_msg_n .eq. 1) then
               write(ems_msg_wr_cn, 9000)li_sa, msg_6_li(p0:p1)
            else
               write(ems_msg_wr_cn, 9001)msg_6_li(p0:p1)
            endif
         else
            p0 = 1
            p1 = min(ln_t_l_ch,
     &           ems_msg_wr_li_ln-li_sa_ln-parlp_li_sa_ln)
            if (ems_msg_rp_msg_n .eq. 1) then
               write(ems_msg_wr_cn, 9810)parlp_li_sa, li_sa,
     &              msg_6_li(p0:p1)
            else
               write(ems_msg_wr_cn, 9811)parlp_li_sa, msg_6_li(p0:p1)
            endif
         endif
 100     continue
         if (p1 .ge. ln_t_l_ch) goto 200
         ems_msg_pg_li_n = ems_msg_pg_li_n + 1
         p0 = p1 + 1
         p1 = min(ln_t_l_ch, p1+ems_msg_wr_li_ln)
         if (no_prcs_n) then
            write(ems_msg_wr_cn, 9001)msg_6_li(p0:p1)
         else
            write(ems_msg_wr_cn, 9811)parlp_li_sa, msg_6_li(p0:p1)
         endif
         goto 100
      endif
 200  continue
      call ems_wr_i_t_ch(msg_n, ch4_msg_n, sa, '0')
      if (msg_n .eq. 6) then
         ems_msg_pg_n = ems_msg_pg_n + 1
         ems_msg_pg_li_n = 1
         if (ems_msg_rp_msg_n .eq. 1) li_sa = '1'//li_prefix//ch4_msg_n
         if (ems_msg_no_prt_fm .ge. 1) then
            call ems_msg_g_ch13_vers(ch13_vers, ch13_vers_ln)
            write(ems_li, 9100)
     &           package, ch13_vers(1:ch13_vers_ln), ems_msg_pg_n
         endif
      else
         ems_msg_pg_li_n = ems_msg_pg_li_n + 1
         if (ems_msg_rp_msg_n .eq. 1) li_sa = ' '//li_prefix//ch4_msg_n
      endif
      ln_t_l_ch = ems_ln_t_l_ch(ems_li)
      if (msg_n .lt. 0 .or. msg_n .gt. 9999) then
         if (ems_msg_rp_msg_n .eq. 1) then
            write(ems_msg_wr_cn, 9990)li_prefix, msg_n
         else
            write(ems_msg_wr_cn, 9991)msg_n
         endif
         ems_msg_cod = ems_msg_lvl_serious
         goto 2000
      endif
      if (msg_n .ge. bs_serious_msg_n) then
         li_sa(li_sa_ln-1:li_sa_ln) = 'S '
         ems_msg_cod = ems_msg_lvl_serious
      else if (msg_n .ge. bs_er_msg_n) then
         li_sa(li_sa_ln-1:li_sa_ln) = 'E '
         ems_msg_cod = max(ems_msg_lvl_er, ems_msg_cod)
      else if (msg_n .ge. bs_warn_msg_n) then
         li_sa(li_sa_ln-1:li_sa_ln) = 'W '
         ems_msg_cod = max(ems_msg_lvl_warn, ems_msg_cod)
      else
         li_sa(li_sa_ln-1:li_sa_ln) = 'I '
         ems_msg_cod = max(ems_msg_lvl_info, ems_msg_cod)
      endif
      p0 = 1
      if (no_prcs_n) then
         p1 = min(ln_t_l_ch, ems_msg_wr_li_ln-li_sa_ln)
         if (p1 .gt. 0) then
            if (ems_msg_rp_msg_n .eq. 1) then
               write(ems_msg_wr_cn, 9000)li_sa, ems_li(p0:p1)
               if (wr_cn0) write(ems_msg_er_cn, 9000)
     &              li_sa, ems_li(p0:p1)
            else
               write(ems_msg_wr_cn, 9001)ems_li(p0:p1)
               if (wr_cn0) write(ems_msg_er_cn, 9001)ems_li(p0:p1)
            endif
         else
            if (ems_msg_rp_msg_n .eq. 1) then
               write(ems_msg_wr_cn, 9000)li_sa
               if (wr_cn0) write(ems_msg_er_cn, 9000)li_sa
            else
               write(ems_msg_wr_cn, 9001)
               if (wr_cn0) write(ems_msg_er_cn, 9001)
            endif
         endif
      else
         p1 = min(ln_t_l_ch, ems_msg_wr_li_ln-li_sa_ln-parlp_li_sa_ln)
         if (p1 .gt. 0) then
            if (ems_msg_rp_msg_n .eq. 1) then
               write(ems_msg_wr_cn, 9810)parlp_li_sa, li_sa,
     &              ems_li(p0:p1)
               if (wr_cn0) write(ems_msg_er_cn, 9810)parlp_li_sa,
     &              li_sa, ems_li(p0:p1)
            else
               write(ems_msg_wr_cn, 9811)parlp_li_sa, ems_li(p0:p1)
               if (wr_cn0)
     &              write(ems_msg_er_cn, 9811)parlp_li_sa, ems_li(p0:p1)
            endif
         else
            if (ems_msg_rp_msg_n .eq. 1) then
               write(ems_msg_wr_cn, 9810)parlp_li_sa, li_sa
               if (wr_cn0) write(ems_msg_er_cn, 9810)parlp_li_sa, li_sa
            else
               write(ems_msg_wr_cn, 9811)parlp_li_sa
               if (wr_cn0) write(ems_msg_er_cn, 9811)parlp_li_sa
            endif
         endif
      endif
 1000 continue
      if (p1 .ge. ln_t_l_ch) goto 2000
      ems_msg_pg_li_n = ems_msg_pg_li_n + 1
      p0 = p1 + 1
      p1 = min(ln_t_l_ch, p1+ems_msg_wr_li_ln)
      if (no_prcs_n) then
         write(ems_msg_wr_cn, 9001)ems_li(p0:p1)
         if (wr_cn0) write(ems_msg_er_cn, 9001)ems_li(p0:p1)
      else
         write(ems_msg_wr_cn, 9811)parlp_li_sa, ems_li(p0:p1)
         if (wr_cn0) write(ems_msg_er_cn, 9811)
     &        parlp_li_sa, ems_li(p0:p1)
      endif
      goto 1000
 2000 continue
      if (ems_msg_pg_li_n .ge. ems_msg_n_pg_li) then
         msg_n = 6
         go to 10
      endif
 7000 continue
      return
 9000 format(a10, a)
 9001 format(a)
 9100 format(a50, ': Version ', a, ' Page', i5)
 9800 format('Process ', i3, ': ')
 9810 format(a13, a10, a)
 9811 format(a13, a)
 9990 format(1x, a4, '9999S Unidentified message id ', i9)
 9991 format(' Unidentified message id ', i9)
      end
 
      subroutine ems_msg_g_ch13_vers(ch13_vers, ch13_vers_ln)
      implicit none
      include 'EMSVERS.INC'
      character*13 ch13_vers
      integer ch13_vers_ln
      integer p
 
      p = 1
      write(ch13_vers(p:p), 9001)mjor_vers_n
      p = p + 1
      ch13_vers(p:p) = '.'
      p = p + 1
      if (mnor_vers_n .le. 9) then
         write(ch13_vers(p:p), 9001)mnor_vers_n
         p = p + 1
      else
         write(ch13_vers(p:p+1), 9002)mnor_vers_n
         p = p + 2
      endif
      ch13_vers(p:p) = '.'
      p = p + 1
      if (mjor_rv_n .le. 9) then
         write(ch13_vers(p:p), 9001)mjor_rv_n
         p = p + 1
      else
         write(ch13_vers(p:p+1), 9002)mjor_rv_n
         p = p + 2
      endif
      if (mnor_rv_n .gt. 0) then
         ch13_vers(p:p) = '.'
         p = p + 1
         write(ch13_vers(p:p), 9001)mnor_rv_n
         p = p + 1
      endif
      ch13_vers_ln = p-1
      return
 9001 format(i1)
 9002 format(i2)
      end
C->>> ------------------------------------------> ems_msg_wr_li_t_cn <<<
      subroutine ems_msg_wr_li_t_cn(rt_cod, wr_cn)
      implicit none
      include 'EMSMSG.INC'
      integer rt_cod, wr_cn
      integer ems_ln_t_l_ch
      integer ln_t_l_ch
 
      integer rt_cod_serious_f7_wr_er
      parameter (rt_cod_serious_f7_wr_er = ems_msg_lvl_serious)
 
      rt_cod = 0
      if (wr_cn .lt. 0) goto 7100
      ln_t_l_ch = ems_ln_t_l_ch(ems_li)
      if (ln_t_l_ch .lt. 1) goto 7100
 
      write(wr_cn, 9000, err=8990)ems_li(1:ln_t_l_ch)
 7100 continue
      return
 9000 format(a)
 8990 continue
      rt_cod = max(rt_cod_serious_f7_wr_er, rt_cod)
      goto 7100
      end
 
C->>> ----------------------------------------> ems_msg_iz_ct_vr_com <<<
      subroutine ems_msg_iz_ct_vr_com
      implicit none
      include 'EMSMSG.INC'
 
CM      IF (emsol_epc .EQ. 1) THEN
C?      call ems_debug_se_i_a_undn(ems_msg_ct_vr, 1, n_ems_msg_ct_vr)
CM      ENDIF
      ems_msg_iz_ct_vr_com_fg1 = 1
      ems_msg_iz_ct_vr_com_fg2 = 2
      ems_msg_cod = 0
      ems_msg_wr_cn = 6
      ems_msg_er_cn = 0
      ems_msg_no_prt_fm = 10000
      ems_msg_no_prt_t = 0
      ems_msg_rp_msg_n = 1
      ems_msg_wr_li_ln = 80
      ems_msg_n_pg_li = 55
      ems_msg_my_prcs_n = -1
      ems_msg_pg_n = 0
      ems_msg_pg_li_n = 0
 
      return
      end
 
C->>> -----------------------------------------------> ems_wr_i_t_ch <<<
c     Writes an integer to a character variable: right justified and
c     with leading entries filled with ch1_bf_f_dgt. The entry of the
c     first digit (or sign if negative) is also returned. If the integer
c     requires a larger field then only the right-most entries are
c     written.
c
      subroutine ems_wr_i_t_ch(i, ch, sa, ch1_bf_f_dgt)
      implicit none
      character*(*) ch
      integer i, sa
      character*1 ch1_bf_f_dgt
      integer ch_ln, ichar0
      integer te_i, dgt
      integer p
 
      ch_ln = len(ch)
      p = ch_ln
      te_i = iabs(i)
      ichar0 = ichar('0')
 10   continue
      dgt = te_i - 10*(te_i/10)
      ch(p:p) = char(ichar0+dgt)
      te_i = te_i/10
      if (te_i .eq. 0) then
         if (i .lt. 0 .and. p .gt. 1) then
            p = p - 1
            ch(p:p) = '-'
         endif
         sa = p
         do 20, p = 1, sa-1
            ch(p:p) = ch1_bf_f_dgt
 20      continue
      else
         p = p - 1
         if (p .ge. 1) goto 10
      endif
      return
      end
 
