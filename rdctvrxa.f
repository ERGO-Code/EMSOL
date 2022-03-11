C->>> ------------------------------------------------> ems_rd_ct_vr <<<
      subroutine ems_rd_ct_vr(ems_rt_cod,
     &     usr_n_i_ct_vr, usr_n_rl_ct_vr, usr_i_ct_vr, usr_rl_ct_vr)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMSG.INC'
      include 'EMSRTCOD.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      integer mx_li_ln
      parameter (mx_li_ln = 80)
      integer ct_vr_ty, i_ct_vr_ty, rl_ct_vr_ty
      parameter (i_ct_vr_ty = 1, rl_ct_vr_ty = 2)
      integer num_ln
      parameter (num_ln = 24)
 
      integer ems_rt_cod
      integer usr_n_rl_ct_vr, usr_n_i_ct_vr
      integer usr_i_ct_vr(usr_n_i_ct_vr)
      double precision usr_rl_ct_vr(usr_n_rl_ct_vr)
 
      logical fi_xst
      integer ct_vr_rd_cn, li_n, f_p, l_p
      integer ct_vr
      integer i_ct_vr_v
      double precision rl_ct_vr_v
 
      character*(mx_ct_vr_nm_ln) nm
      character*(num_ln) num
      character*(mx_li_ln) li
      character*(1) ch
      character*(1) tab
 
      ems_rt_cod = 0
      tab = char(9)
 
      call ems_iz_ems_ct_vr_nm
      call ems_iz_osl_ct_vr_nm
 
      inquire(file='ct_vr.dat', EXIST=fi_xst)
      if (.not. fi_xst) go to 7000
 
c     Should find a free channel
      ct_vr_rd_cn = 1
      open(unit=ct_vr_rd_cn, file='ct_vr.dat')
      li_n = 0
      go to 100
 
 90   continue
      write(6, 9800)' ** ct_vr.dat: No assignment in line ', li_n
 
 100  continue
      read(ct_vr_rd_cn, '(a80)', end = 6000) li
c      write(6, *) li
      li_n = li_n+1
      do f_p = 1, mx_li_ln
         ch = li(f_p:f_p)
         if ((ch .ne. ' ') .and.  (ch .ne. tab)) go to 120
      end do
      go to 100
 120  continue
      if (li(f_p:f_p) .eq. '!') go to 100
      do l_p = f_p+1, mx_li_ln
         ch = li(l_p:l_p)
         if ((ch .eq. ' ') .or. (ch .eq. '=') .or.
     &        (ch .eq. tab) .or. (ch .eq. '(')) go to 140
      end do
      go to 90
 140  continue
      nm = li(f_p:l_p-1)
      call ems_str_t_up_case(nm)
      f_p = l_p+1
      if (nm .eq. 'I_CT_VR' .or. nm .eq. 'I_CT') then
         ct_vr_ty = i_ct_vr_ty
         go to 145
      end if
      if (nm .ne. 'RL_CT_VR' .and. nm .ne. 'RL_CT') go to 148
      ct_vr_ty = rl_ct_vr_ty
 145  continue
      if (ch .ne. '(') then
         write(6, *) ' ** ct_vr.dat: ( expected in line', li_n
         go to 100
      end if
      do l_p = f_p, mx_li_ln
         ch = li(l_p:l_p)
         if (ch .eq. ')') go to 147
      end do
      go to 90
 147  continue
      l_p = l_p-1
c      write(6, *) num_ln, f_p, l_p
      num = ' '
      if (l_p - f_p .ge. num_ln) then
         write(6, 9800)
     &        ' ** ct_vr.dat: too many digits specified in line', li_n
         go to 100
      end if
      if (l_p - f_p .ge. 0) then
         num(num_ln - (l_p-f_p):num_ln) = li(f_p:l_p)
      end if
      read(num, '(i24)', err=4000) ct_vr
c      write(6, *) nm, '(', ct_vr, ')'
      f_p = l_p+1
      if (f_p+2 .gt. mx_li_ln) go to 90
      ch = li(f_p:f_p)
      if (ch .ne. ')') then
         write(6, *) ' ** ct_vr.dat: ) expected in line', li_n
         go to 100
      end if
      f_p = f_p + 1
      ch = li(f_p:f_p)
      go to 160
 
 148  continue
      ct_vr_ty = i_ct_vr_ty
      do ct_vr = 1, n_osl_i_ct_vr
         if (nm .eq. osl_i_ct_vr_nm(ct_vr)) go to 160
      end do
      ct_vr_ty = rl_ct_vr_ty
      do ct_vr = 1, n_osl_rl_ct_vr
         if (nm .eq. osl_rl_ct_vr_nm(ct_vr)) go to 160
      end do
c
c     Switch to lower case to check EMSOL control variables.
c
      call ems_str_t_lo_case(nm)
      ct_vr_ty = i_ct_vr_ty
      do ct_vr = 1, n_ems_i_ct_vr
         if (nm .eq. ems_i_ct_vr_nm(ct_vr)) go to 160
      end do
      ct_vr_ty = rl_ct_vr_ty
      do ct_vr = 1, n_ems_rl_ct_vr
         if (nm .eq. ems_rl_ct_vr_nm(ct_vr)) go to 160
      end do
 
      write(6, *) ' ** ct_vr.dat: ', nm, ' is not the name of a control'
      go to 100
 160  continue
      if ((ch .eq. ' ') .or. (ch .eq. tab)) then
         do f_p = f_p, mx_li_ln
            ch = li(f_p:f_p)
            if ((ch .ne. ' ') .and. (ch .ne. tab)) go to 180
         end do
         go to 90
 180     continue
         if (li(f_p:f_p) .ne. '=') go to 90
      end if
      do f_p = f_p+1, mx_li_ln
         ch = li(f_p:f_p)
         if ((ch .ne. ' ') .and. (ch .ne. tab)) go to 190
      end do
      go to 90
 190  continue
      do l_p = f_p, mx_li_ln
         ch = li(l_p:l_p)
         if ((ch .eq. ' ') .or. (ch .eq. tab)
     &        .or. (ch .eq. '!')) go to 200
      end do
      l_p = mx_li_ln+1
 200  continue
      l_p = l_p-1
c      write(6, *) num_ln, f_p, l_p
      num = ' '
      if (l_p - f_p .ge. num_ln) then
         write(6, 9800)
     &        ' ** ct_vr.dat: too many digits specified in line', li_n
         go to 100
      end if
      if (l_p - f_p .ge. 0) then
         num(num_ln - (l_p-f_p):num_ln) = li(f_p:l_p)
      end if
c      write(6, *) nm, '=', num, ' =', li(f_p:l_p)
      if (ct_vr .le. 0) goto 100
      if (ct_vr_ty .eq. i_ct_vr_ty) then
         read(num, '(i24)', err=4000) i_ct_vr_v
c         write(6, 9000)nm, ' i', ct_vr, i_ct_vr_v
         if (ct_vr .le. usr_n_i_ct_vr) then
            usr_i_ct_vr(ct_vr) = i_ct_vr_v
         else
            write(6, 9400)'integer', nm, ct_vr, usr_n_i_ct_vr
         endif
      else
         read(num, '(g24.0)', err=4010) rl_ct_vr_v
c         write(6, 9000)nm, 'rl', ct_vr, rl_ct_vr_v
         if (ct_vr .le. usr_n_i_ct_vr) then
            usr_rl_ct_vr(ct_vr) = rl_ct_vr_v
         else
            write(6, 9400)'real   ', nm, ct_vr, usr_n_rl_ct_vr
         endif
         go to 100
      end if
      go to 100
 
 4000 continue
      write(6, 9800) ' ct_vr.dat: Integer format error in line ', li_n
      go to 100
 
 4010 continue
      write(6, 9800) ' ct_vr_cr.dat: Real format error in line ', li_n
      go to 100
 
 6000 continue
      close(unit=ct_vr_rd_cn)
 
 7000 continue
      return
 
c 9000 format(a, ': ', a2, '_ct_vr(', i3, ') =', i9)
 9400 format('Control variable number for ', a8,
     &     ' control variable ', a, ' is ', i3,
     &     ' which exceeds user dimension of ', i3)
 9800 format(a, i9)
      end
