C->>> -----------------------------------------------> ems_ze_pv_c_v <<<
c     Zeroes the nonzero entries in pv_c_v.
c
      subroutine ems_ze_pv_c_v(pv_c_v, nw_eta_ix)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'RSMICOM.INC'
      integer nw_eta_ix(0:nw_eta_l_ix)
      double precision pv_c_v(0:n_r)
      integer ix_n, r_n
 
      if (nw_eta_l_ix .le. n_r) then
         do 10, ix_n = nw_eta_f_ix, nw_eta_l_ix
            pv_c_v(nw_eta_ix(ix_n)) = zero
 10      continue
      else
         do 20, r_n = 1, n_r
            pv_c_v(r_n) = zero
 20      continue
      end if
      return
      end
 
C->>> -------------------------------------------------> ems_ze_pi_v <<<
c     Zeroes the nonzero entries in pi.
c
      subroutine ems_ze_pi_v(pi_v, pi_ix)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      integer pi_ix(0:n_r)
      double precision pi_v(0:n_r)
      integer ix_n, r_n
 
      if (pi_ix(0) .le. n_r) then
         do 10, ix_n = 1, pi_ix(0)
            pi_v(pi_ix(ix_n)) = zero
 10      continue
      else
         do 20, r_n = 1, n_r
            pi_v(r_n) = zero
 20      continue
      end if
      return
      end
 
C->>> --------------------------------------------> ems_g_vec_ix_bar <<<
c     Sets up pointers into vec_ix of the nonzeros which it indicates.
c     NB Assumes that vec_ix_bar is zeroed on entry.
c
      subroutine ems_g_vec_ix_bar(lc_n_r, vec_ix, vec_ix_bar)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICS.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer lc_n_r, vec_ix(0:lc_n_r), vec_ix_bar(0:lc_n_r)
      integer vec_ix_n
 
      if (vec_ix_bar(0) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9900)
         call ems_msg_wr_li(bug_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?         call ems_dump
CM      ENDIF
      end if
      if (iand(ck_msk, ze_a_ck_bt) .ne. 0)
     &     call ems_ck_ze_i_a(lc_n_r, vec_ix_bar)
      do 10, vec_ix_n = 1, vec_ix(0)
         vec_ix_bar(vec_ix(vec_ix_n)) = vec_ix_n
 10   continue
      vec_ix_bar(0) = 1
      return
 9900 format('vec_ix_bar has not been zeroed')
      end
 
C->>> -------------------------------------------> ems_ze_vec_ix_bar <<<
c     Zeroes the nonzero entries in vec_ix_bar.
c
      subroutine ems_ze_vec_ix_bar(lc_n_r, vec_ix, vec_ix_bar)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      integer lc_n_r, vec_ix(0:lc_n_r), vec_ix_bar(0:lc_n_r)
      integer ix_n, r_n
      if (vec_ix(0) .lt. tl_dse_vec*lc_n_r) then
         do 10, ix_n = 1, vec_ix(0)
            vec_ix_bar(vec_ix(ix_n)) = 0
 10      continue
      else
         do 20, r_n = 1, lc_n_r
            vec_ix_bar(r_n) = 0
 20      continue
      end if
      vec_ix_bar(0) = 0
      return
      end
 
C->>> -------------------------------------------> ems_ck_vec_ix_bar <<<
c     Checks the consistency of vec_ix_bar with vec_v and vec_ix.
c
      subroutine ems_ck_vec_ix_bar(lc_n_r, vec_v, vec_ix, vec_ix_bar)
      implicit none
      include 'EMSV.INC'
      include 'EMSMSG.INC'
      integer lc_n_r, vec_ix(0:lc_n_r), vec_ix_bar(0:lc_n_r)
      double precision vec_v(0:lc_n_r)
      integer r_n, ix_n, n_ix
 
      if (vec_ix_bar(0) .eq. 0) go to 7000
      do 10, ix_n = 1, vec_ix(0)
         r_n = vec_ix(ix_n)
         if (vec_ix_bar(r_n) .ne. ix_n) go to 8001
         if (vec_v(r_n) .eq. zero) go to 8002
 10   continue
      n_ix = 0
      do 20, r_n = 1, lc_n_r
         if (vec_v(r_n) .ne. zero) n_ix = n_ix + 1
 20   continue
      if (n_ix .ne. vec_ix(0)) go to 8003
 7000 continue
      return
 8001 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810)
      call ems_msg_wr_li(bug_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?      call ems_dump
CM      ENDIF
      go to 7000
 8002 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9820)
      call ems_msg_wr_li(bug_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?      call ems_dump
CM      ENDIF
      go to 7000
 8003 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9830)
      call ems_msg_wr_li(bug_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?      call ems_dump
CM      ENDIF
      go to 7000
 9810 format('Inconsistency between vec_ix and vec_ix_bar ')
 9820 format('Inconsistency between vec_ix and vec_v ')
 9830 format('Inconsistency between n_ix and vec_ix(0)')
      end
 
C->>> ----------------------------------------------> ems_cp_nz_v_ix <<<
c     Makes a copy of the nonzero values and indices for an array.
c
      subroutine ems_cp_nz_v_ix(lc_n_r, fm_ix, fm_v, t_ix, t_v)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICS.INC'
      include 'ICTVR.INC'
      integer lc_n_r, fm_ix(0:lc_n_r), t_ix(0:lc_n_r)
      double precision fm_v(0:lc_n_r), t_v(0:lc_n_r)
      integer ix_n
 
      if (iand(ck_msk, ze_a_ck_bt) .ne. 0)
     &     call ems_ck_ze_rl_a(lc_n_r, t_v)
      t_ix(0) = fm_ix(0)
      do 10, ix_n = 1, fm_ix(0)
         t_ix(ix_n) = fm_ix(ix_n)
         t_v(fm_ix(ix_n)) = fm_v(fm_ix(ix_n))
 10   continue
      return
      end
 
C->>> ----------------------------------------------> ems_ck_ze_rl_a <<<
c     Checks that the array supplied is zeroed.
c
      subroutine ems_ck_ze_rl_a(lc_n_r, rl_a)
      implicit none
      include 'EMSV.INC'
      include 'EMSMSG.INC'
      integer lc_n_r
      double precision rl_a(0:lc_n_r)
      integer r_n
      logical er_fd
      integer pr_pass_1
      save pr_pass_1
      data pr_pass_1/0/
 
      er_fd = .false.
      do 10, r_n = 1, lc_n_r
c
c     Keep this .ne. zero just for safety.
c
         if (rl_a(r_n) .ne. zero) then
            er_fd = .true.
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &           r_n, rl_a(r_n)
            call ems_msg_wr_li(bug_msg_n)
c            rl_a(r_n) = zero
         end if
 10   continue
      if (er_fd) then
CM      IF (emsol_deb .EQ. 1) THEN
C?         call ems_dump
CM      ENDIF
      else if (pr_pass_1 .eq. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)lc_n_r
         call ems_msg_wr_li(info_msg_n)
         pr_pass_1 = 1
      end if
      return
 9000 format('Entry ', i9, ' in zeroed array has value ', g11.4)
 9100 format('Real array of dimension ', i9, ' is zeroed ')
      end
 
C->>> -----------------------------------------------> ems_ck_ze_i_a <<<
c     Checks that the integer array supplied is zeroed.
c
      subroutine ems_ck_ze_i_a(lc_n_r, i_a)
      implicit none
      include 'EMSV.INC'
      include 'EMSMSG.INC'
      integer lc_n_r, i_a(0:lc_n_r)
      integer r_n
      logical er_fd
      integer pr_pass_1
      save pr_pass_1
      data pr_pass_1/0/
 
      er_fd = .false.
      do 10, r_n = 1, lc_n_r
         if (i_a(r_n) .ne. 0) then
            er_fd = .true.
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &           r_n, i_a(r_n)
            call ems_msg_wr_li(bug_msg_n)
            i_a(r_n) = 0
         end if
 10   continue
      if (er_fd) then
CM      IF (emsol_deb .EQ. 1) THEN
C?         call ems_dump
CM      ENDIF
      else if (pr_pass_1 .eq. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)lc_n_r
         call ems_msg_wr_li(info_msg_n)
         pr_pass_1 = 1
      end if
      return
 9000 format('Entry ', i9, ' in zeroed array has value ', i9)
 9100 format('Integer array of dimension ', i9, ' is zeroed ')
      end
 
C->>> ------------------------------------------> ems_g_mtx_vec_prod <<<
      subroutine ems_g_mtx_vec_prod(ty, aa, vec, beta, vec_prod, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      integer ty, is(0:is_n_en_m1)
      double precision aa, vec(*), beta, vec_prod(*), ds(0:ds_n_en_m1)
      integer c_n, r_n, el_n
 
      if (ty .eq. 1) then
         if (beta .eq. zero) then
            do 5 r_n=1, n_r
               vec_prod(r_n) = zero
 5          continue
         else if (beta .ne. one) then
            do 10 r_n=1, n_r
               vec_prod(r_n) = vec_prod(r_n)*beta
 10         continue
         end if
         if (aa .ne. zero) then
            do 15 c_n=1, n_c
               if (vec(c_n) .eq. zero) go to 15
               do 14, el_n = is(p_mtx_c_sa+c_n),
     &              is(p_mtx_c_sa+c_n+1)-1
                  vec_prod(is(p_mtx_r_ix+el_n)) =
     &                 vec_prod(is(p_mtx_r_ix+el_n)) +
     &                 aa*vec(c_n)*ds(p_mtx_r_v+el_n)
 
 14            continue
 15         continue
         end if
      else
         if (beta .eq. zero) then
            do 18 c_n=1, n_c
               vec_prod(c_n) = zero
 18         continue
         else if (beta .ne. one) then
            do 20 c_n=1, n_c
               vec_prod(c_n) = vec_prod(c_n)*beta
 20         continue
         end if
         if (aa .ne. zero) then
            do 25 c_n=1, n_c
               do 24, el_n = is(p_mtx_c_sa+c_n),
     &              is(p_mtx_c_sa+c_n+1)-1
                  vec_prod(c_n) = vec_prod(c_n) +
     &                 aa*vec(is(p_mtx_r_ix+el_n))*ds(p_mtx_r_v+el_n)
 24            continue
 25         continue
         end if
      end if
      return
      end
 
