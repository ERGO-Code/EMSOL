CM      IF (emsol_xa .EQ. 1) THEN
C?C->>> ----------------------------------------------> ems_fo_du_ml <<<
C?c     Forms the dual of a model.
C?c
C?      subroutine ems_fo_du_ml(
C?     &     rl_nm, lbc, cbp, ubc, scl, st,
C?     &     mtx_r_v, mtx_r_ix, mtx_c_sa,
C?     &     mtx_c_v, mtx_c_ix, mtx_r_sa,
C?     &     du_lo_co, du_up_co)
C?      implicit none
C?      include 'EMSV.INC'
C?      include 'EMSPM.INC'
C?      include 'ICTVR.INC'
C?      include 'RLCTVR.INC'
C?      include 'EMSMSG.INC'
C?      double precision rl_nm(0:mx_n_c+n_r)
C?      double precision lbc(0:mx_n_c+n_r)
C?      double precision cbp(0:mx_n_c+n_r)
C?      double precision ubc(0:mx_n_c+n_r)
C?      double precision scl(0:mx_n_c+n_r)
C?      integer st(0:mx_n_c+n_r)
C?      double precision mtx_r_v(0:n_a_el)
C?      integer mtx_r_ix(0:n_a_el)
C?      integer mtx_c_sa(0:n_c+n_r+1)
C?      double precision mtx_c_v(0:n_a_el)
C?      integer mtx_c_ix(0:n_a_el)
C?      integer mtx_r_sa(0:n_r+1)
C?      double precision du_lo_co(0:mx_n_c+n_r)
C?      double precision du_up_co(0:mx_n_c+n_r)
C?      double precision lbc_v, cbp_v, ubc_v
C?      integer r_n, c_n, el_n, vr_n, r_sa
C?
C?      if (n_c .lt. n_r) goto 8000
C?      do 10, r_n = 1, n_r
C?         mtx_r_sa(r_n) = 0
C? 10   continue
C?      do 20, el_n = 1, n_a_el
C?         r_n = mtx_r_ix(el_n)
C?         mtx_r_sa(r_n) = mtx_r_sa(r_n) + 1
C? 20   continue
C?      r_sa = 1
C?      do 30, r_n = 1, n_r
C?         el_n = r_sa + mtx_r_sa(r_n)
C?         mtx_r_sa(r_n) = r_sa
C?         r_sa = el_n
C? 30   continue
C?      do 45, c_n = 1, n_c
C?         do 40, el_n = mtx_c_sa(c_n), mtx_c_sa(c_n+1)-1
C?            r_n = mtx_r_ix(el_n)
C?            mtx_c_ix(mtx_r_sa(r_n)) = c_n
C?            mtx_c_v(mtx_r_sa(r_n)) = mtx_r_v(el_n)
C?            mtx_r_sa(r_n) = mtx_r_sa(r_n) + 1
C? 40      continue
C? 45   continue
C?      do 50, r_n = n_r, 1, -1
C?         mtx_r_sa(r_n+1) = mtx_r_sa(r_n)
C? 50   continue
C?      mtx_r_sa(1) = 1
C?
C?      do 60, r_n = 1, n_r+1
C?         mtx_c_sa(r_n) = mtx_r_sa(r_n)
C? 60   continue
C?      do 70, el_n = 1, n_a_el
C?         mtx_r_ix(el_n) = mtx_c_ix(el_n)
C?         mtx_r_v(el_n) = mtx_c_v(el_n)
C? 70   continue
C?c
C?c     dual logical    lower cost =   primal structural lower bound
C?c     dual logical    upper cost =   primal structural upper bound
C?c
C?      do 110, c_n = 1, n_c
C?         r_n = n_r + c_n
C?         du_lo_co(r_n) = lbc(c_n)
C?         du_up_co(r_n) = ubc(c_n)
C? 110  continue
C?c
C?c     dual structural lower cost = - primal logical    upper bound
C?c     dual structural upper cost = - primal logical    lower bound
C?c
C?      do 112, r_n = 1, n_r
C?         c_n = r_n
C?         du_lo_co(c_n) = -ubc(mx_n_c+r_n)
C?         du_up_co(c_n) = -lbc(mx_n_c+r_n)
C? 112  continue
C?      do 114, vr_n = 1, n_r+n_c
C?         lbc(vr_n) = du_lo_co(vr_n)
C?         ubc(vr_n) = du_up_co(vr_n)
C? 114  continue
C?c
C?c     dual structural breakpoint = primal logical    cost
C?c
C?      do 120, c_n = 1, n_c
C?         r_n = n_r + c_n
C?         du_lo_co(r_n) = cbp(c_n)
C?         du_up_co(r_n) = rl_nm(c_n)
C? 120  continue
C?c
C?c     dual logical    breakpoint = primal structural cost
C?c
C?      do 122, r_n = 1, n_r
C?         c_n = r_n
C?         du_lo_co(c_n) = cbp(mx_n_c+r_n)
C?         du_up_co(c_n) = rl_nm(mx_n_c+r_n)
C? 122  continue
C?      do 124, vr_n = 1, n_r+n_c
C?         cbp(vr_n) = du_lo_co(vr_n)
C?         rl_nm(vr_n) = du_up_co(vr_n)
C? 124  continue
C?      if (iand(ml_da_st_msk, ml_da_st_scl_ml) .ne. 0) then
C?c
C?c     dual structural scale      = 1/(primal logical scale)
C?c
C?         do 130, c_n = 1, n_c
C?            r_n = n_r + c_n
C?            du_lo_co(r_n) = one/scl(c_n)
C? 130     continue
C?c
C?c     dual logical    scale      = 1/(primal structural scale)
C?c
C?         do 132, r_n = 1, n_r
C?            c_n = r_n
C?            du_lo_co(c_n) = one/scl(mx_n_c+r_n)
C? 132     continue
C?         do 134, vr_n = 1, n_r+n_c
C?            scl(vr_n) = du_lo_co(vr_n)
C? 134     continue
C?      endif
C?      n_bp_vr = 0
C?      ob_fn_cs = zero
C?      do 210, vr_n = 1, n_r+n_c
C?         st(vr_n) = 0
C?         lbc_v = lbc(vr_n)
C?         cbp_v = cbp(vr_n)
C?         ubc_v = ubc(vr_n)
C?         if (lbc(vr_n) .le. -inf) then
C?            if (ubc(vr_n) .ge. inf) then
C?c
C?c     Variable has infinite lower and upper costs.
C?c     Make it FX at its breakpoint
C?c
C?               lbc(vr_n) = cbp_v
C?               cbp(vr_n) = zero
C?               ubc(vr_n) = cbp_v
C?            else
C?c
C?c     Variable has infinite lower cost but finite upper cost.
C?c     Make it LB with cost ubc
C?c
C?               lbc(vr_n) = cbp_v
C?               cbp(vr_n) = ubc_v
C?               ubc(vr_n) = inf
C?c
C?c     The contribution to the objective should be zero when the variable
C?c     is at its LB so make a contribution to the ob_fn_cs
C?c
C?               ob_fn_cs = ob_fn_cs - cbp_v*ubc_v
C?            endif
C?         else
C?            if (ubc(vr_n) .ge. inf) then
C?c
C?c     Variable has finite lower cost but infinite upper cost.
C?c     Make it UB with cost ubc
C?c
C?               lbc(vr_n) = -inf
C?               cbp(vr_n) = lbc_v
C?               ubc(vr_n) = cbp_v
C?c
C?c     The contribution to the objective should be zero when the variable
C?c     is at its UB so make a contribution to the ob_fn_cs
C?c
C?               ob_fn_cs = ob_fn_cs + cbp_v*lbc_v
C?            else if (lbc(vr_n) .eq. ubc(vr_n)) then
C?c
C?c     The variable has equal lower and upper costs.
C?c     Make it FR with cost ubc=lbc
C?c
C?               lbc(vr_n) = -inf
C?               cbp(vr_n) = ubc_v
C?               ubc(vr_n) = inf
C?c
C?c     The contribution to the objective should be zero when the variable
C?c     is at its LB so make a contribution to the ob_fn_cs
C?c
C?               ob_fn_cs = ob_fn_cs - cbp_v*ubc_v
C?            else
C?c
C?c     The variable has distinct finite lower and upper costs.
C?c     Make it BP!
C?c
C?               st(vr_n) = alt_bt + bp_bt
C?               n_bp_vr = n_bp_vr + 1
C?            endif
C?         endif
C? 210  continue
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)n_bp_vr
C?      call ems_msg_wr_li(info_msg_n)
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)ob_fn_cs
C?      call ems_msg_wr_li(info_msg_n)
C?      if (n_bp_vr .gt. 0) then
C?         ml_da_st_msk = ior(ml_da_st_msk, ml_da_st_alt_lp)
C?         ml_da_st_msk = ior(ml_da_st_msk, ml_da_st_bp_vr)
C?      else
C?         ml_da_st_msk = ml_da_st_msk -
C?     &        iand(ml_da_st_msk, ml_da_st_alt_lp) -
C?     &        iand(ml_da_st_msk, ml_da_st_bp_vr)
C?      endif
C?      do 220, c_n = 1, n_c
C?         r_n = n_r + c_n
C?         st(r_n) = st(r_n) + bc_bt
C? 220  continue
C?c
C?c     Interchange n_r with n_c and set mx_n_c = n_c and mx_n_r = n_r
C?c
C?      mx_n_r = n_c
C?      mx_n_c = n_r
C?      n_r = mx_n_r
C?      n_c = mx_n_c
C? 7000 continue
C?      return
C? 8000 continue
C?      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
C?      call ems_msg_wr_li(serious_msg_n)
C?      goto 7000
C? 9000 format('Dual problem has ', i7, ' breakpoint variables')
C? 9010 format('Dual problem has objective constant = ', g11.4)
C? 9800 format('Cannot handle n_c < n_r')
C?      end
CM      ENDIF
C->>> ----------------------------------------------> ems_l1_gd_st <<<
      integer function ems_l1_gd_st(l1_dn_gd, l1_up_gd, tl)
      implicit none
      include 'EMSV.INC'
      include 'EMSMSG.INC'
      double precision l1_dn_gd, l1_up_gd, tl
      integer l1_gd_st
c
c     \                      /
c      \                    /
c       \                  /
c        \________________/
c      ^  ^  ^  ^  ^  ^  ^  ^
c      |  |  |  |  |  |  |  |
c      1  2  3  4  5  6  7  8
c
c     Case 1: dn_gd <   0 and up_gd < -tl
c     Case 2: dn_gd <   0 and up_gd <   0   Candidate minimizer
c     Case 3: dn_gd <   0 and up_gd  =  0   Candidate minimizer
c     Case 4: dn_gd <   0 and up_gd <= tl   Candidate minimizer
c     Case 5: dn_gd <   0 and up_gd >  tl   Candidate minimizer
c     Case 6: dn_gd  =  0
c     Case 7: dn_gd <= tl
c     Case 8: dn_gd >  tl
c
c
c     \    |       |       |       |       |       |
c      \up |       |       |       |       |       |
c       \  | << 0  |  < 0  |  = 0  |  > 0  | >> 0  |
c     dn \ |       |       |       |       |       |
c     ____\|_______|_______|_______|_______|_______|
c          |       |       |       |       |       |
c     << 0 |   1   |   2   |   3   |   4   |   5   |
c     _____|_______|_______|_______|_______|_______|
c          |       |       |       |       |       |
c      < 0 |   0   |   2   |   3   |   4   |   5   |
c     _____|_______|_______|_______|_______|_______|
c          |       |       |       |       |       |
c      = 0 |   0   |   0   |   6   |   6   |   6   |
c     _____|_______|_______|_______|_______|_______|
c          |       |       |       |       |       |
c      > 0 |   0   |   0   |   0   |   7   |   7   |
c     _____|_______|_______|_______|_______|_______|
c          |       |       |       |       |       |
c     >> 0 |   0   |   0   |   0   |   0   |   8   |
c     _____|_______|_______|_______|_______|_______|
c
      if (l1_dn_gd .gt. l1_up_gd) goto 8000
      if (l1_dn_gd .lt. zero) then
         if (l1_up_gd .lt. -tl) then
            l1_gd_st = 1
         else if (l1_up_gd .lt. zero) then
            l1_gd_st = 2
         else if (l1_up_gd .eq. zero) then
            l1_gd_st = 3
         else if (l1_up_gd .le. tl) then
            l1_gd_st = 4
         else
            l1_gd_st = 5
         endif
      else if (l1_dn_gd .eq. zero) then
         l1_gd_st = 6
      else if (l1_dn_gd .le. tl) then
         l1_gd_st = 7
      else
         l1_gd_st = 8
      endif
      ems_l1_gd_st = l1_gd_st
 7000 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
     &     l1_dn_gd, l1_up_gd, tl
      call ems_msg_wr_li(bug_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?      call ems_dump
CM      ENDIF
      goto 7000
 9800 format('UNEXPECTED case in ems_l1_gd_st',
     &     'l1_dn_gd, l1_up_gd, tl ', 3(2x, g11.4))
      end
