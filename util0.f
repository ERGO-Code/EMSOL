CM
c->>> ---------------------------------------> ems_sv_poss_undn_rl_v <<<
c     Used to save a real value which may be undefined
c
      subroutine ems_sv_poss_undn_rl_v(sv_rl_v, rl_v)
      implicit none
      double precision sv_rl_v, rl_v
      sv_rl_v = rl_v
      return
      end
c->>> ----------------------------------------> ems_sv_poss_undn_i_v <<<
c     Used to save an integer value which may be undefined
c
      subroutine ems_sv_poss_undn_i_v(sv_i_v, i_v)
      implicit none
      integer sv_i_v, i_v
      sv_i_v = i_v
      return
      end
c->>> -------------------------------------> ems_rcov_poss_undn_rl_v <<<
c     Used to recover a real value which may have been undefined
c
      subroutine ems_rcov_poss_undn_rl_v(sv_rl_v, rl_v)
      implicit none
      double precision sv_rl_v, rl_v
      rl_v = sv_rl_v
      return
      end
c->>> --------------------------------------> ems_rcov_poss_undn_i_v <<<
c     Used to recover an integer value which may have been undefined
c
      subroutine ems_rcov_poss_undn_i_v(sv_i_v, i_v)
      implicit none
      integer sv_i_v, i_v
      i_v = sv_i_v
      return
      end
c->>> ------------------------------------------------> ems_i1_ne_i2 <<<
c     Used to test if i1 is not equal to i2: needed when i1 and i2 are
c     unassigned variables.
c
      logical function ems_i1_ne_i2(i1, i2)
      implicit none
      integer i1, i2
      ems_i1_ne_i2 = i1 .ne. i2
      return
      end
 
c->>> ------------------------------------------------> ems_i1_eq_i2 <<<
c     Used to test if i1 is equal to i2: needed when i1 and i2 are
c     unassigned variables.
c
      logical function ems_i1_eq_i2(i1, i2)
      implicit none
      integer i1, i2
      ems_i1_eq_i2 = i1 .eq. i2
      return
      end
 
C->>> -----------------------------------------------> ems_ln_t_f_bl <<<
c     Finds the length of the string up to (but not including) the first
c     non-alpha-numeric/blank character. NB Returns zero for strings of
c     blanks.
c     Used for creating file names.
c
      integer function ems_ln_t_f_bl(li)
      implicit none
      character*(*) li
      integer li_n_ch
      integer p, ln
 
      li_n_ch = len(li)
      ln = 0
      do 10, p = 1, li_n_ch
         if (ichar(li(p:p)) .le. 32 .or. ichar(li(p:p)) .gt. 126) then
c
c     The character is blank or not something which should be printed.
c
            ln = p-1
            go to 1000
         endif
         ln = p
 10   continue
 1000 continue
      ems_ln_t_f_bl = ln
      return
      end
 
C->>> --------------------------------------------------> ems_cp_i_a <<<
      subroutine ems_cp_i_a(ln, fm_i_a, t_i_a, mode)
      implicit none
      integer ln, t_i_a(1:ln), fm_i_a(1:ln), mode, k
      if (mode .eq. 0) then
         do 10, k = 1, ln
            t_i_a(k) = fm_i_a(1)
 10      continue
      else if (mode .lt. 0) then
         do 20, k = 1, ln
            t_i_a(k) = k
 20      continue
      else if (mode .eq. 1) then
         do 30, k = 1, ln
            t_i_a(k) = fm_i_a(k)
 30      continue
      else if (mode .eq. 2) then
         do 40, k = ln, 1, -1
            t_i_a(k) = fm_i_a(k)
 40      continue
      else
CM      IF (emsol_deb .EQ. 1) THEN
C?         write(*, *)' Strange mode ', mode
CM      ENDIF
      end if
      return
      end
 
C->>> -------------------------------------------------> ems_cp_rl_a <<<
      subroutine ems_cp_rl_a(ln, fm_rl_a, t_rl_a, mode)
      implicit none
      integer ln, mode, k
      double precision t_rl_a(1:ln), fm_rl_a(1:ln)
      if (mode .eq. 0) then
         do 10, k = 1, ln
            t_rl_a(k) = fm_rl_a(1)
 10      continue
      else if (mode .eq. 1) then
         do 30, k = 1, ln
            t_rl_a(k) = fm_rl_a(k)
 30      continue
      else if (mode .eq. 2) then
         do 40, k = ln, 1, -1
            t_rl_a(k) = fm_rl_a(k)
 40      continue
      else
CM      IF (emsol_deb .EQ. 1) THEN
C?         write(*, *)' Strange mode ', mode
CM      ENDIF
      end if
      return
      end
 
C->>> ------------------------------------------------> ems_cp_ch8_a <<<
      subroutine ems_cp_ch8_a(ln, fm_ch8_a, t_ch8_a, mode)
      implicit none
      integer ln, mode, k
      character*8 t_ch8_a(1:ln), fm_ch8_a(1:ln)
      if (mode .eq. 0) then
         do 10, k = 1, ln
            t_ch8_a(k) = fm_ch8_a(1)
 10      continue
      else if (mode .eq. 1) then
         do 30, k = 1, ln
            t_ch8_a(k) = fm_ch8_a(k)
 30      continue
      else if (mode .eq. 2) then
         do 40, k = ln, 1, -1
            t_ch8_a(k) = fm_ch8_a(k)
 40      continue
      else
CM      IF (emsol_deb .EQ. 1) THEN
C?         write(*, *)' Strange mode ', mode
CM      ENDIF
      end if
      return
      end
 
C->>> -------------------------------------------------> ems_cp_lg_a <<<
      subroutine ems_cp_lg_a(ln, fm_lg_a, t_lg_a, mode)
      implicit none
      integer ln, mode, k
      logical t_lg_a(1:ln), fm_lg_a(1:ln)
      if (mode .eq. 0) then
         do 10, k = 1, ln
            t_lg_a(k) = fm_lg_a(1)
 10      continue
      else if (mode .eq. 1) then
         do 30, k = 1, ln
            t_lg_a(k) = fm_lg_a(k)
 30      continue
      else if (mode .eq. 2) then
         do 40, k = ln, 1, -1
            t_lg_a(k) = fm_lg_a(k)
 40      continue
      else
CM      IF (emsol_deb .EQ. 1) THEN
C?         write(*, *)' Strange mode ', mode
CM      ENDIF
      end if
      return
      end
 
C->>> -------------------------------------------------> ems_ch8_t_i <<<
      subroutine ems_ch8_t_i(ch8, i)
      implicit none
      include 'EMSV.INC'
      character*8 ch8
      integer i(1:ch_wo_z)
      character*8 lc_ch8
      integer lc_i(1:ch_wo_z)
      equivalence (lc_ch8, lc_i)
      integer i_wo_n
      lc_ch8 = ch8
      do 10, i_wo_n = 1, ch_wo_z
         i(i_wo_n) = lc_i(i_wo_n)
 10   continue
      return
      end
 
C->>> -------------------------------------------------> ems_i_t_ch8 <<<
      subroutine ems_i_t_ch8(i, ch8)
      implicit none
      include 'EMSV.INC'
      integer i(1:ch_wo_z)
      character*8 ch8
      integer lc_i(1:ch_wo_z)
      character*8 lc_ch8
      equivalence (lc_ch8, lc_i)
      integer i_wo_n
      do 10, i_wo_n = 1, ch_wo_z
         lc_i(i_wo_n) = i(i_wo_n)
 10   continue
      ch8 = lc_ch8
      return
      end
 
c->>> ------------------------------------------------> ems_ch8_t_rl <<<
      double precision function ems_ch8_t_rl(ch8)
      implicit none
      character*8 ch8
      character*8 lc_ch8
      double precision rl
      equivalence (rl, lc_ch8)
      lc_ch8 = ch8
      ems_ch8_t_rl = rl
      return
      end
 
c->>> ------------------------------------------------> ems_rl_t_ch8 <<<
      character*8 function ems_rl_t_ch8(rl_nm)
      implicit none
      double precision rl_nm
      double precision lc_rl_nm
      character*8 ch8_nm
      equivalence (lc_rl_nm, ch8_nm)
      lc_rl_nm = rl_nm
      ems_rl_t_ch8 = ch8_nm
      return
      end
 
C->>> ------------------------------------------> ems_rd_fr_fmt_rl_a <<<
      subroutine ems_rd_fr_fmt_rl_a(cn, a, lb, ub, er, e)
      implicit none
      integer cn, lb, ub
      double precision a(lb:ub)
      logical er, e
      er = .false.
      e = .false.
      read(cn, *, err = 7010, end = 7020)a
 7000 continue
      return
 7010 continue
      er = .true.
      go to 7000
 7020 continue
      e = .true.
      go to 7000
      end
 
C->>> -------------------------------------------> ems_rd_fr_fmt_i_a <<<
      subroutine ems_rd_fr_fmt_i_a(cn, a, lb, ub, er, e)
      implicit none
      integer cn, lb, ub, a(lb:ub)
      logical er, e
      er = .false.
      e = .false.
      read(cn, *, err = 7010, end = 7020)a
 7000 continue
      return
 7010 continue
      er = .true.
      go to 7000
 7020 continue
      e = .true.
      go to 7000
      end
 
C->>> ------------------------------------------> ems_rd_un_fmt_rl_a <<<
      subroutine ems_rd_un_fmt_rl_a(cn, a, lb, ub, er, e)
      implicit none
      integer cn, lb, ub
      double precision a(lb:ub)
      logical er, e
      er = .false.
      e = .false.
      read(cn, err = 7010, end = 7020)a
 7000 continue
      return
 7010 continue
      er = .true.
      go to 7000
 7020 continue
      e = .true.
      go to 7000
      end
 
C->>> -------------------------------------------> ems_rd_un_fmt_i_a <<<
      subroutine ems_rd_un_fmt_i_a(cn, a, lb, ub, er, e)
      implicit none
      integer cn, lb, ub, a(lb:ub)
      logical er, e
      er = .false.
      e = .false.
      read(cn, err = 7010, end = 7020)a
 7000 continue
      return
 7010 continue
      er = .true.
      go to 7000
 7020 continue
      e = .true.
      go to 7000
      end
 
C->>> ------------------------------------------> ems_wr_fr_fmt_rl_a <<<
      subroutine ems_wr_fr_fmt_rl_a(cn, a, lb, ub, er)
      implicit none
      integer cn, lb, ub
      logical er
      double precision a(lb:ub)
      er = .false.
      write(cn, *, err = 7010)a
 7000 continue
      return
 7010 continue
      er = .true.
      go to 7000
      end
 
C->>> -------------------------------------------> ems_wr_fr_fmt_i_a <<<
      subroutine ems_wr_fr_fmt_i_a(cn, a, lb, ub, er)
      implicit none
      integer cn, lb, ub, a(lb:ub)
      logical er
      er = .false.
      write(cn, *, err = 7010)a
 7000 continue
      return
 7010 continue
      er = .true.
      go to 7000
      end
 
C->>> ------------------------------------------> ems_wr_un_fmt_rl_a <<<
      subroutine ems_wr_un_fmt_rl_a(cn, a, lb, ub, er)
      implicit none
      integer cn, lb, ub
      double precision a(lb:ub)
      logical er
      er = .false.
      write(cn, err = 7010)a
 7000 continue
      return
 7010 continue
      er = .true.
      goto 7000
      end
 
C->>> -------------------------------------------> ems_wr_un_fmt_i_a <<<
      subroutine ems_wr_un_fmt_i_a(cn, a, lb, ub, er)
      implicit none
      integer cn, lb, ub, a(lb:ub)
      logical er
      er = .false.
      write(cn, err = 7010)a
 7000 continue
      return
 7010 continue
      er = .true.
      goto 7000
      end
 
C->>> ---------------------------------------------------> ems_ICOPY <<<
      subroutine ems_ICOPY(ln, fm_i_a, fm_i_a_stp, t_i_a, t_i_a_stp)
      implicit none
      integer ln, t_i_a(1:ln), fm_i_a_stp, fm_i_a(1:ln), t_i_a_stp, k
      do 10, k = 1, ln
         t_i_a(k) = fm_i_a(k)
 10   continue
      return
      end
 
C->>> ---------------------------------------------------> ems_DCOPY <<<
      subroutine ems_DCOPY(ln, fm_rl_a, fm_rl_a_stp, t_rl_a, t_rl_a_stp)
      implicit none
      integer ln, fm_rl_a_stp, t_rl_a_stp, k
      double precision t_rl_a(1:ln), fm_rl_a(1:ln)
      do 10, k = 1, ln
         t_rl_a(k) = fm_rl_a(k)
 10   continue
      return
      end
 
C->>> ---------------------------------------------------> ems_DCOPY <<<
      subroutine ems_DSCAL(ln, aa, rl_a, rl_a_stp)
      implicit none
      include 'EMSV.INC'
      integer ln, rl_a_stp, k
      double precision aa, rl_a(1:ln)
      if (aa .eq. zero) then
         do 10, k = 1, ln
            rl_a(k) = zero
 10      continue
      else if (aa .eq. one) then
         goto 7000
      else if (aa .eq. -one) then
         do 20, k = 1, ln
            rl_a(k) = -rl_a(k)
 20      continue
      else
         do 30, k = 1, ln
            rl_a(k) = aa*rl_a(k)
 30      continue
      endif
 7000 continue
      return
      end
 
