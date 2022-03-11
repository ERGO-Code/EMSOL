CM
C->>> --------------------------------------------------> ems_it_rfn <<<
c     Perform iterative refinement of the solution given by the INVERT
c     of Ax=b or A^Tx=b, that is -A^{-1}b or -A^{-T}b.
c
c     Use     x+dx = -A^{-1}b        or         x+dx = -A^{-T}b
c      => -A(x+dx) = b                    -A^T(x+dx) = b
c      =>     -Adx = b+Ax                     -A^Tdx = b+A^Tx
c      =>       dx = -A^{-1}(b+Ax)                dx = -A^{-T}(b+A^Tx)
c
      subroutine ems_it_rfn(
     &     ems_rt_cod, is, ds, rp_lvl,
     &     trans,
     &     vr_in_r,
     &     mtx_r_v, mtx_r_ix, mtx_c_sa,
     &     sol_v, sol_ix,
     &     rhs_v, rhs_ix,
     &     rsdu_v,
     &     mx_n_rfn_it, tl_it_rfn_norm_rsdu, it_rfn_tran_ze,
     &     msg)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      include 'EMSRTCOD.INC'
      integer ems_rt_cod, rp_lvl
      logical trans
      integer vr_in_r(0:n_r)
      integer mtx_r_ix(0:n_a_el)
      integer mtx_c_sa(0:n_c+1)
      integer sol_ix(0:n_r)
      integer rhs_ix(0:n_r)
      integer is(0:is_n_en_m1)
      double precision mtx_r_v(0:n_a_el)
      double precision sol_v(0:n_r)
      double precision rhs_v(0:n_r)
      double precision rsdu_v(0:n_r)
      double precision ds(0:ds_n_en_m1)
      character*(*) msg
 
      integer mx_n_rfn_it
      double precision tl_it_rfn_norm_rsdu
      double precision it_rfn_tran_ze
 
      double precision tl_it_rfn_norm_dl
 
      integer r_n, vr_n, c_n, el_n, og_r_n, nw_r_n
      double precision sol_r_v, dl
      logical perm_inv
      logical alw_f7_wr
      character*6 ch6_sys
      integer sys_l_ch
      character*12 ch12_norm_rsdu
      integer norm_rsdu_f_ch
      double precision sv_tran_ze
 
      integer rfn_it_n
      double precision it_rfn_iz_norm_rsdu
      double precision it_rfn_norm_rsdu
      double precision it_rfn_norm_dl
 
c      integer wg_r_n
c      print*, ' Enter component of solution to corrupt '
c      read*, wg_r_n
c      if (wg_r_n .ge. 1 .and. wg_r_n .le. n_r) then
c         print*, ' Enter alternative for ', sol_v(wg_r_n)
c         read*, sol_v(wg_r_n)
c      endif
 
      ems_rt_cod = ems_rt_cod_ok
      perm_inv = iand(inv_alg_msk, inv_alg_perm) .ne. 0
      alw_f7_wr = ems_msg_no_prt_fm .ge. 1
      tl_it_rfn_norm_dl = zero
      if (trans) then
c
c     The solution to be refined is that of A^Tx=b
c
         ch6_sys = 'A^Tx=b'
         sys_l_ch = 6
         ch12_norm_rsdu = '||A^Tx-b||_2'
         norm_rsdu_f_ch = 1
      else
         ch6_sys = 'Ax=b  '
         sys_l_ch = 4
         ch12_norm_rsdu = '  ||Ax-b||_2'
         norm_rsdu_f_ch = 3
      endif
CM      IF (emsol_dev .EQ. 1) THEN
C?      if (rp_lvl .ge. 1) then
C?         if (alw_f7_wr) write(ems_li, 9100, err=8990)
C?     &        ch6_sys(1:sys_l_ch), msg
C?         call ems_msg_wr_li(info_msg_n)
C?         if (rp_lvl .ge. 2) then
C?            if (alw_f7_wr) write(ems_li, 9200, err=8990)
C?     &           ch12_norm_rsdu
C?            call ems_msg_wr_li(info_msg_n)
C?         endif
C?      endif
CM      ENDIF
c
c     Make sure that the vector used to store the residual is zeroed
c
      do 1, r_n = 1, n_r
         rsdu_v(r_n) = zero
 1    continue
      rfn_it_n = 0
      it_rfn_norm_dl = inf
 10   continue
c
c     Form y = b
c
      if (rhs_ix(0) .lt. 0) then
c
c     It is assumed that, for vr_n = -rhs_ix(0), the RHS is
c     rhs_v(0)*a_{vr_n}, where a_{vr_n} is the corresponding column of
c     [A:-I]
c
         vr_n = -rhs_ix(0)
         if (vr_n .le. n_c) then
            do 20, el_n = mtx_c_sa(vr_n), mtx_c_sa(vr_n+1)-1
               r_n = mtx_r_ix(el_n)
               rsdu_v(r_n) = rhs_v(0)*mtx_r_v(el_n)
 20         continue
         else
            r_n = vr_n - mx_n_c
            rsdu_v(r_n) = -rhs_v(0)
         endif
      else
         do 110, r_n = 1, n_r
            rsdu_v(r_n) = rhs_v(r_n)
 110     continue
      endif
c      do r_n = 1, n_r
c         write(*, 9300)r_n,
c     &        ' sol_v(r_n) = ', sol_v(r_n),
c     &        ' rhs_v(r_n) = ', rsdu_v(r_n)
c      enddo
      if (trans) then
c
c     Form y := y + A^Tx
c
         do 130, r_n = 1, n_r
            vr_n = vr_in_r(r_n)
            if (vr_n .le. n_c) then
               do 120, el_n = mtx_c_sa(vr_n), mtx_c_sa(vr_n+1)-1
                  c_n = mtx_r_ix(el_n)
                  rsdu_v(r_n) = rsdu_v(r_n) + mtx_r_v(el_n)*sol_v(c_n)
 120           continue
            else
               c_n = vr_n - mx_n_c
               rsdu_v(r_n) = rsdu_v(r_n) - sol_v(c_n)
            endif
 130     continue
      else
c
c     Form y := y + Ax
c
         do 150, c_n = 1, n_r
            sol_r_v = sol_v(c_n)
            if (sol_r_v .eq. zero) goto 150
            vr_n = vr_in_r(c_n)
            if (vr_n .le. n_c) then
               do 140, el_n = mtx_c_sa(vr_n), mtx_c_sa(vr_n+1)-1
                  r_n = mtx_r_ix(el_n)
                  rsdu_v(r_n) = rsdu_v(r_n) + mtx_r_v(el_n)*sol_r_v
 140           continue
            else
               r_n = vr_n - mx_n_c
               rsdu_v(r_n) = rsdu_v(r_n) - sol_r_v
            endif
 150     continue
      endif
c      do r_n = 1, n_r
c         write(*, 9300)r_n,
c     &        ' rsdu_v(r_n) = ', rsdu_v(r_n)
c      enddo
      it_rfn_norm_rsdu = zero
      do 160, r_n = 1, n_r
c     write(*, 9300)r_n, ' rsdu_v(r_n) = ', rsdu_v(r_n)
         it_rfn_norm_rsdu =
     &        it_rfn_norm_rsdu + rsdu_v(r_n)*rsdu_v(r_n)
 160  continue
      it_rfn_norm_rsdu = sqrt(it_rfn_norm_rsdu)
      if (rfn_it_n .eq. 0) it_rfn_iz_norm_rsdu = it_rfn_norm_rsdu
      if (rfn_it_n .ge. mx_n_rfn_it .or.
     &     it_rfn_norm_dl .le. tl_it_rfn_norm_dl .or.
     &     it_rfn_norm_rsdu .le. tl_it_rfn_norm_rsdu) then
c
c     One of the termination criteria is satisfied.
c
         do 170, r_n = 1, n_r
            rsdu_v(r_n) = zero
 170     continue
CM      IF (emsol_dev .EQ. 1) THEN
C?         if (rp_lvl .ge. 2) then
C?            if (alw_f7_wr) write(ems_li, 9202, err=8990)
C?     &           rfn_it_n+1, it_rfn_norm_rsdu
C?            call ems_msg_wr_li(info_msg_n)
C?         else if (rp_lvl .ge. 1) then
C?            if (alw_f7_wr) write(ems_li, 9201, err=8990)
C?     &           rfn_it_n, ch12_norm_rsdu(norm_rsdu_f_ch:12),
C?     &           it_rfn_iz_norm_rsdu, it_rfn_norm_rsdu
C?            call ems_msg_wr_li(info_msg_n)
C?         endif
CM      ENDIF
         goto 7000
      endif
c
c     Perform an iteration of refinement.
c
      rfn_it_n = rfn_it_n + 1
      if (trans) then
c
c     Form y := -A^{-T}y
c
         sv_tran_ze = bwd_tran_ze
         bwd_tran_ze = it_rfn_tran_ze
         call ems_btran(rsdu_v, n_r+1, ds, is)
         bwd_tran_ze = sv_tran_ze
      else
c
c     Form y := -A^{-1}y
c
         if (perm_inv) then
c
c     Permute the RHS in copying from rsdu to perm_rsdu.
c
            do 210, og_r_n = 1, n_r
               r_n = is(p_og_t_nw_perm+og_r_n)
               ds(p_perm_tran_vec+r_n) = rsdu_v(og_r_n)
               rsdu_v(og_r_n) = zero
 210        continue
            sv_tran_ze = fwd_tran_ze
            fwd_tran_ze = it_rfn_tran_ze
            call ems_ftran(ds(p_perm_tran_vec), n_r+1, ds, is)
            fwd_tran_ze = sv_tran_ze
         else
c
c     If not permuting INVERT
c
            sv_tran_ze = fwd_tran_ze
            fwd_tran_ze = it_rfn_tran_ze
            call ems_ftran(rsdu_v, n_r+1, ds, is)
            fwd_tran_ze = sv_tran_ze
         endif
      endif
c
c     Form x: = x + y and zero the vector used to compute y.
c
      it_rfn_norm_dl = zero
      if (perm_inv) then
c
c     If permuting INVERT
c
         if (trans) then
c
c     If refining the solution to A^Tx=b, correction is in rsdu_v and
c     have to apply permutation in updating x.
c
            do 310, nw_r_n = 1, n_r
               dl = rsdu_v(nw_r_n)
               it_rfn_norm_dl = it_rfn_norm_dl + dl*dl
               r_n = is(p_nw_t_og_perm+nw_r_n)
               sol_v(r_n) = sol_v(r_n) + dl
               rsdu_v(nw_r_n) = zero
 310        continue
         else
c
c     If refining the solution to Ax=b, correction is in perm_tran_vec.
c
            do 320, r_n = 1, n_r
               dl = ds(p_perm_tran_vec+r_n)
               it_rfn_norm_dl = it_rfn_norm_dl + dl*dl
               sol_v(r_n) = sol_v(r_n) + dl
               ds(p_perm_tran_vec+r_n) = zero
 320        continue
         endif
      else
c
c     If not permuting INVERT then correction is in rsdu_v
c
         do 330, r_n = 1, n_r
            dl = rsdu_v(r_n)
            it_rfn_norm_dl = it_rfn_norm_dl + dl*dl
            sol_v(r_n) = sol_v(r_n) + dl
            rsdu_v(r_n) = zero
 330     continue
      endif
      it_rfn_norm_dl = sqrt(it_rfn_norm_dl)
CM      IF (emsol_dev .EQ. 1) THEN
C?      if (rp_lvl .ge. 2) then
C?         if (alw_f7_wr) write(ems_li, 9202, err=8990)
C?     &        rfn_it_n, it_rfn_norm_rsdu, it_rfn_norm_dl
C?         call ems_msg_wr_li(info_msg_n)
C?      endif
CM      ENDIF
      goto 10
 7000 continue
CM      IF (emsol_dev .EQ. 1) THEN
C? 7100 continue
CM      ENDIF
      return
CM      IF (emsol_dev .EQ. 1) THEN
C? 8990 continue
C?      ems_rt_cod = max(ems_rt_cod_serious_f7_wr_er,
C?     &     ems_rt_cod)
C?      goto 7100
C? 9100 format('Performing iterative refinement on solution of ', a,
C?     &     ': ', a)
C? 9200 format(' It   ', a12, '     ||dx||_2')
C? 9201 format('After ', i3, ' iterations, ', a,
C?     &     ' has changed from ', g11.4, ' to ', g11.4)
C? 9202 format(i3, 2x, 2(2x, g11.4))
CM      ENDIF
c 9300 format(i5, 2(2x, a, g11.4))
      end
