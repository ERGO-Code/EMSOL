C->>> ------------------------------------------------> ems_struc_pc <<<
c     Price the nonbasic structural columns which are not fixed at their
c     bound. Determine whether row or column pricing is to be used and
c     call the appropriate routine.
c
      subroutine ems_struc_pc(ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMIHDL.INC'
      include 'RSMICS.INC'
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      integer is(0:is_n_en_m1)
      double precision ds(0:ds_n_en_m1)
 
      if (iand(ck_msk, ze_a_ck_bt) .ne. 0)
     &     call ems_ck_ze_rl_a(n_c, ds(p_tbu_r_v))
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_pc_lvl1) call ems_tt_rec(pc_mtx_prod_tt, n_bs)
CM      ENDIF
CM      IF (emsol_da .EQ. 1) THEN
C?      call ems_u_tbu_r_pi_sol_da(is(p_pi_ix))
CM      ENDIF
c
c     Always do row pricing if possible---hard to see how it could be
c     more expensive than column pricing unless only a very small number
c     of columns are being priced.
c
      if (iand(ml_da_st_msk, ml_da_st_r_mtx) .eq. 0) then
         call ems_struc_c_pc(
     &        is(p_vr_in_c),
     &        ds(p_mtx_r_v),
     &        is(p_mtx_r_ix),
     &        is(p_mtx_c_sa),
     &        ds(p_pi_v),
     &        is(p_pi_ix),
     &        ds(p_tbu_r_v),
     &        is(p_tbu_r_ix))
      else
c
c     Use the row-wise representation of the matrix in order to perform
c     the pricing operation.
c
         if (iand(inv_alg_msk, inv_alg_perm) .ne. 0) then
            call ems_perm_r_pc(
     &           ds(p_mtx_c_v),
     &           is(p_mtx_c_ix),
     &           is(p_mtx_r_sa),
     &           ds(p_pi_v),
     &           is(p_pi_ix),
     &           ds(p_tbu_r_v),
     &           is(p_tbu_r_ix),
     &           is(p_nw_t_og_perm))
         else if (iand(asm_msk, asm_r_pc) .eq. 0) then
            call ems_f77_r_pc(
     &           is(p_vr_in_c),
     &           ds(p_mtx_c_v),
     &           is(p_mtx_c_ix),
     &           is(p_mtx_r_sa),
     &           ds(p_pi_v),
     &           is(p_pi_ix),
     &           ds(p_tbu_r_v),
     &           is(p_tbu_r_ix))
         else
            ds(p_pi_v) = one
            is(p_mtx_r_sa) = 0
CM      IF (emsol_asm .EQ. 1) THEN
C?            call ems_r_pc(
C?     &           ds(p_mtx_c_v),
C?     &           is(p_mtx_c_ix),
C?     &           is(p_mtx_r_sa+n_r),
C?     &           ds(p_pi_v+n_r),
C?     &           ds(p_tbu_r_v))
C?            is(p_pi_ix) = 0
CM      ELSE
            call ems_f77_r_pc(
     &           is(p_vr_in_c),
     &           ds(p_mtx_c_v),
     &           is(p_mtx_c_ix),
     &           is(p_mtx_r_sa),
     &           ds(p_pi_v),
     &           is(p_pi_ix),
     &           ds(p_tbu_r_v),
     &           is(p_tbu_r_ix))
CM      ENDIF
         endif
      endif
CM      IF (emsol_da .EQ. 1) THEN
C?      call ems_u_tbu_r_da(ds(p_tbu_r_v))
CM      ENDIF
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl1) call ems_tt_rec(-pc_mtx_prod_tt, n_bs)
CM      ENDIF
      if (iand(ck_msk, ze_a_ck_bt) .ne. 0)
     &     call ems_ck_ze_rl_a(n_r, ds(p_pi_v))
      return
      end
 
C->>> ----------------------------------------------> ems_struc_c_pc <<<
c     Use the copy of the matrix stored by columns to price
c     structural variables vr_in_c(1), ..., vr_in_c(n_c_t_pc)
c     The vector pi is zeroed.
c
      subroutine ems_struc_c_pc(
     &     vr_in_c,
     &     mtx_r_v, mtx_r_ix, mtx_c_sa,
     &     pi_v, pi_ix, tbu_r_v, tbu_r_ix)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'RSMICOM.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      integer vr_in_c(-vr_in_c_n_sn:n_c), pi_ix(0:n_r)
      integer mtx_r_ix(0:n_a_el), mtx_c_sa(0:n_c+1)
      double precision mtx_r_v(0:n_a_el)
      double precision pi_v(0:n_r), tbu_r_v(0:n_c)
      integer tbu_r_ix(0:n_c)
      double precision v, tbu_r_c_v
      integer c_n, vr_n, el_n, r_n, ix_n
      integer pi_n_en
 
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_pc_lvl2) call ems_tt_rec(pc_c_mtx_prod_tt, n_bs)
CM      ENDIF
c
c     Indicate that the indices of nonzeros in the tableau row are not
c     known.
c
      tbu_r_ix(0) = n_c+1
      do 20, c_n = vr_in_c(os_struc_in_c_f_p_m1) + 1,
     &     vr_in_c(os_struc_in_c_l_pc_p)
         vr_n = vr_in_c(c_n)
         tbu_r_c_v = zero
         do 10, el_n = mtx_c_sa(vr_n), mtx_c_sa(vr_n+1)-1
            v = pi_v(mtx_r_ix(el_n))
            tbu_r_c_v = tbu_r_c_v + v*mtx_r_v(el_n)
 10      continue
         tbu_r_v(vr_n) = tbu_r_c_v
 20   continue
c
c     Zero the pi vector, counting the number of nonzeros if the density
c     of the BTRAN solution is required.
c
      if (pi_ix(0) .le. n_r) then
         do 110, ix_n = 1, pi_ix(0)
            pi_v(pi_ix(ix_n)) = zero
 110     continue
         pi_n_en = pi_ix(0)
      else if (btran_sol_dse .ge. zero) then
         do 120, r_n = 1, n_r
            pi_v(r_n) = zero
 120     continue
      else
         pi_n_en = 0
         do 121, r_n = 1, n_r
            if (pi_v(r_n) .ne. zero) pi_n_en = pi_n_en + 1
            pi_v(r_n) = zero
 121     continue
      end if
c
c     Get the density of the BTRAN solution (if required).
c
      if (btran_sol_dse .lt. zero)
     &     btran_sol_dse = float(pi_n_en)/float(n_r)
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_pc_lvl2) call ems_tt_rec(-pc_c_mtx_prod_tt, n_bs)
CM      ENDIF
c
c     Indicate that the pi vector has been zeroed. This has to be done
c     because, in g_bc_fs_cg, any (phase one) basic cost changes due
c     non-pivotal rows becoming feasible are accumulated in the pi
c     vector and g_bc_fs_cg should not assume that pi is zero on entry
c     (because it may be called more than once before the dual
c     activities are updated.)
c
      pi_ix(0) = 0
      return
      end
 
C->>> ------------------------------------------------> ems_f77_r_pc <<<
c     Use the copy of the matrix stored by rows to price the basic
c     variables which are not fixed.
c
c     It is assumed that tbu_r_v is zero on entry.
c     The existence of indices of nonzeros in pi is exploited and pi is
c     zeroed on exit.
c
      subroutine ems_f77_r_pc(
     &     vr_in_c,
     &     mtx_c_v, mtx_c_ix, mtx_r_sa,
     &     pi_v, pi_ix, tbu_r_v, tbu_r_ix)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICS.INC'
      include 'RSMICOM.INC'
      include 'EMSMSG.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
CM      IF (emsol_da .EQ. 1) THEN
C?      include 'EMSDA.INC'
CM      ENDIF
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      integer vr_in_c(-vr_in_c_n_sn:n_c)
      integer mtx_c_ix(0:n_a_el+n_r), mtx_r_sa(0:n_r)
      integer pi_ix(0:n_r)
      double precision mtx_c_v(0:n_a_el+n_r)
      double precision pi_v(0:n_r)
      double precision tbu_r_v(0:mx_n_c+n_r)
      integer tbu_r_ix(0:n_c)
      integer el_n, fm_ix_n, ix_n, r_n, c_n
      integer pi_n_en
      integer tbu_r_n_c
 
      tbu_r_n_c =
     &     vr_in_c(os_struc_in_c_l_pc_p) -
     &     vr_in_c(os_struc_in_c_f_p_m1)
      dse_tbu_r_n_c = tl_dse_tbu_r*tbu_r_n_c
      if (pi_ix(0) .gt. n_r) then
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl2)
C?     &        call ems_tt_rec(pc_dse_r_mtx_prod_tt, n_bs)
CM      ENDIF
         pi_n_en = 0
         do 120, r_n = 1, n_r
CM      IF (emsol_da .EQ. 1) THEN
C?            if (pi_v(r_n) .ne. zero) then
C?               su_n_pc_en = su_n_pc_en + 1
C?               if (abs(pi_v(r_n)) .le. pc_ze)
C?     &              su_n_pc_ze = su_n_pc_ze + 1
C?            endif
CM      ENDIF
            if (pi_v(r_n) .eq. zero) go to 120
            pi_n_en = pi_n_en + 1
            if (abs(pi_v(r_n)) .le. pc_ze) goto 115
            el_n = mtx_r_sa(r_n)
            if (el_n .lt. 0) goto 115
 110        continue
            c_n = mtx_c_ix(el_n)
            if (c_n .gt. 0) then
               tbu_r_v(c_n) = tbu_r_v(c_n) + pi_v(r_n)*mtx_c_v(el_n)
               el_n = el_n + 1
               goto 110
            else
               tbu_r_v(-c_n) = tbu_r_v(-c_n) + pi_v(r_n)*mtx_c_v(el_n)
            endif
 115        continue
            pi_v(r_n) = zero
 120     continue
         tbu_r_ix(0) = n_c+1
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl2)
C?     &        call ems_tt_rec(-pc_dse_r_mtx_prod_tt, n_bs)
CM      ENDIF
      else
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl2)
C?     &        call ems_tt_rec(pc_sps_r_mtx_prod_tt, n_bs)
CM      ENDIF
CM      IF (emsol_da .EQ. 1) THEN
C?         su_n_pc_en = su_n_pc_en + pi_ix(0)
CM      ENDIF
         fm_ix_n = 1
         tbu_r_ix(0) = 0
         pi_n_en = pi_ix(0)
 200     continue
         if (tbu_r_ix(0) .gt. n_c .or. sto_tbu_r_ix .eq. sto_ix_no) then
            do 220, ix_n = fm_ix_n, pi_ix(0)
               r_n = pi_ix(ix_n)
CM      IF (emsol_da .EQ. 1) THEN
C?               if (abs(pi_v(r_n)) .le. pc_ze)
C?     &              su_n_pc_ze = su_n_pc_ze + 1
CM      ENDIF
               if (abs(pi_v(r_n)) .le. pc_ze) goto 215
               el_n = mtx_r_sa(r_n)
               if (el_n .lt. 0) goto 215
 210           continue
               c_n = mtx_c_ix(el_n)
               if (c_n .gt. 0) then
                  tbu_r_v(c_n) =
     &                 tbu_r_v(c_n) + pi_v(r_n)*mtx_c_v(el_n)
                  el_n = el_n + 1
                  goto 210
               else
                  tbu_r_v(-c_n) =
     &                 tbu_r_v(-c_n) + pi_v(r_n)*mtx_c_v(el_n)
               endif
 215           continue
               pi_v(r_n) = zero
 220        continue
            tbu_r_ix(0) = n_c+1
         else
            do 320, ix_n = fm_ix_n, pi_ix(0)
               r_n = pi_ix(ix_n)
CM      IF (emsol_da .EQ. 1) THEN
C?               if (abs(pi_v(r_n)) .le. pc_ze)
C?     &              su_n_pc_ze = su_n_pc_ze + 1
CM      ENDIF
               if (abs(pi_v(r_n)) .le. pc_ze) goto 315
               el_n = mtx_r_sa(r_n)
               if (el_n .lt. 0) goto 315
 310           continue
               c_n = mtx_c_ix(el_n)
               if (c_n .gt. 0) then
                  if (tbu_r_v(c_n) .eq. zero) then
                     tbu_r_v(c_n) = pi_v(r_n)*mtx_c_v(el_n)
                     tbu_r_ix(0) = tbu_r_ix(0) + 1
                     if (tbu_r_ix(0) .lt. dse_tbu_r_n_c)
     &                    tbu_r_ix(tbu_r_ix(0)) = c_n
                  else
                     tbu_r_v(c_n) =
     &                    tbu_r_v(c_n) + pi_v(r_n)*mtx_c_v(el_n)
                  endif
                  el_n = el_n + 1
                  goto 310
               else
                  if (tbu_r_v(-c_n) .eq. zero) then
                     tbu_r_v(-c_n) = pi_v(r_n)*mtx_c_v(el_n)
                     tbu_r_ix(0) = tbu_r_ix(0) + 1
                     if (tbu_r_ix(0) .lt. dse_tbu_r_n_c)
     &                    tbu_r_ix(tbu_r_ix(0)) = -c_n
                  else
                     tbu_r_v(-c_n) =
     &                    tbu_r_v(-c_n) + pi_v(r_n)*mtx_c_v(el_n)
                  endif
               endif
 315           continue
               pi_v(r_n) = zero
               if (tbu_r_ix(0) .ge. dse_tbu_r_n_c) then
                  tbu_r_ix(0) = n_c+1
                  fm_ix_n = ix_n + 1
                  goto 200
               endif
 320        continue
         endif
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl2)
C?     &        call ems_tt_rec(-pc_sps_r_mtx_prod_tt, n_bs)
CM      ENDIF
c         write(*, 9000)tbu_r_ix(0)
      endif
c
c     Get the density of the BTRAN solution (if required).
c
      if (btran_sol_dse .lt. zero)
     &     btran_sol_dse = float(pi_n_en)/float(n_r)
c
c     Indicate that the pi vector has been zeroed.
c
      pi_ix(0) = 0
      return
c 9000 format('Row price yields tableau row with ', i7, ' indices')
      end
 
C->>> -----------------------------------------------> ems_perm_r_pc <<<
c     Use the copy of the matrix stored by rows to price the basic
c     variables which are not fixed. Permutes the pi indices according
c     to nw_t_og_perm.
c
c     It is assumed that tbu_r_v is zero on entry.
c     The existence of indices of nonzeros in pi is exploited and pi is
c     zeroed on exit.
c
      subroutine ems_perm_r_pc(
     &     mtx_c_v, mtx_c_ix, mtx_r_sa,
     &     pi_v, pi_ix, tbu_r_v, tbu_r_ix, nw_t_og_perm)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICS.INC'
      include 'RSMICOM.INC'
      include 'EMSMSG.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
CM      IF (emsol_da .EQ. 1) THEN
C?      include 'EMSDA.INC'
C?      include 'TBURDA.INC'
CM      ENDIF
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      integer mtx_c_ix(0:n_a_el+n_r), mtx_r_sa(0:n_r)
      integer pi_ix(0:n_r), nw_t_og_perm(0:n_r)
      double precision mtx_c_v(0:n_a_el+n_r)
      double precision pi_v(0:n_r), tbu_r_v(0:n_c)
      integer tbu_r_ix(0:n_c)
      integer el_n, ix_n, r_n, c_n, og_r_n
      integer pi_n_en
 
      tbu_r_ix(0) = n_c+1
      if (pi_ix(0) .le. n_r) then
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl2)
C?     &        call ems_tt_rec(pc_sps_r_mtx_prod_tt, n_bs)
CM      ENDIF
CM      IF (emsol_da .EQ. 1) THEN
C?         su_n_pc_en = su_n_pc_en + pi_ix(0)
CM      ENDIF
         pi_n_en = 0
         do 20, ix_n = 1, pi_ix(0)
            r_n = pi_ix(ix_n)
CM      IF (emsol_da .EQ. 1) THEN
C?            if (abs(pi_v(r_n)) .le. pc_ze) su_n_pc_ze = su_n_pc_ze + 1
CM      ENDIF
            pi_n_en = pi_n_en + 1
            if (abs(pi_v(r_n)) .le. pc_ze) goto 15
            og_r_n = nw_t_og_perm(r_n)
            el_n = mtx_r_sa(og_r_n)
            if (el_n .lt. 0) goto 15
 10         continue
            c_n = mtx_c_ix(el_n)
            if (c_n .gt. 0) then
               tbu_r_v(c_n) = tbu_r_v(c_n) + pi_v(r_n)*mtx_c_v(el_n)
               el_n = el_n + 1
               goto 10
            else
               tbu_r_v(-c_n) = tbu_r_v(-c_n) + pi_v(r_n)*mtx_c_v(el_n)
            endif
 15         continue
            pi_v(r_n) = zero
 20      continue
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl2)
C?     &        call ems_tt_rec(-pc_sps_r_mtx_prod_tt, n_bs)
CM      ENDIF
      else
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl2)
C?     &        call ems_tt_rec(pc_dse_r_mtx_prod_tt, n_bs)
CM      ENDIF
         pi_n_en = pi_ix(0)
         do 120, r_n = 1, n_r
CM      IF (emsol_da .EQ. 1) THEN
C?            if (pi_v(r_n) .ne. zero) then
C?               su_n_pc_en = su_n_pc_en + 1
C?               if (abs(pi_v(r_n)) .le. pc_ze)
C?     &              su_n_pc_ze = su_n_pc_ze + 1
C?            endif
CM      ENDIF
            if (pi_v(r_n) .eq. zero) go to 120
            if (abs(pi_v(r_n)) .le. pc_ze) goto 115
            og_r_n = nw_t_og_perm(r_n)
            el_n = mtx_r_sa(og_r_n)
            if (el_n .lt. 0) goto 115
 110        continue
            c_n = mtx_c_ix(el_n)
            if (c_n .gt. 0) then
               tbu_r_v(c_n) = tbu_r_v(c_n) + pi_v(r_n)*mtx_c_v(el_n)
               el_n = el_n + 1
               goto 110
            else
               tbu_r_v(-c_n) = tbu_r_v(-c_n) + pi_v(r_n)*mtx_c_v(el_n)
            endif
 115        continue
            pi_v(r_n) = zero
 120     continue
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl2)
C?     &        call ems_tt_rec(-pc_dse_r_mtx_prod_tt, n_bs)
CM      ENDIF
      endif
c
c     Get the density of the BTRAN solution (if required).
c
      if (btran_sol_dse .lt. zero)
     &     btran_sol_dse = float(pi_n_en)/float(n_r)
c
c     Indicate that the pi vector has been zeroed.
c
      pi_ix(0) = 0
      return
      end
 
C->>> ---------------------------------------> ems_perm_g_rcp_alt_pv <<<
c     Computes the product of the column of the constraint matrix
c     corresponding to vr_n and the given vector. NB applies perm.
c
      subroutine ems_perm_g_rcp_alt_pv(rcp_alt_pv,
     &     vr_n, mtx_r_v, mtx_r_ix, mtx_c_sa, vec, og_t_nw_perm)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      double precision rcp_alt_pv
      double precision mtx_r_v(0:n_a_el), vec(0:n_r)
      integer vr_n, mtx_r_ix(0:n_a_el), mtx_c_sa(0:n_c+1)
      integer og_t_nw_perm(0:n_r)
      double precision v
      integer el_n, nw_r_n
 
      if (vr_n .le. n_c) then
         v = zero
         do 10, el_n = mtx_c_sa(vr_n), mtx_c_sa(vr_n+1)-1
            nw_r_n = og_t_nw_perm(mtx_r_ix(el_n))
            if (vec(nw_r_n) .ne. zero) v = v + vec(nw_r_n)*mtx_r_v(el_n)
 10      continue
      else
         nw_r_n = og_t_nw_perm(vr_n-mx_n_c)
         v = -vec(nw_r_n)
      endif
      rcp_alt_pv = v
      return
      end
 
C->>> --------------------------------------------> ems_g_rcp_alt_pv <<<
c     Computes the product of the column of the constraint matrix
c     corresponding to vr_n and the given vector.
c
      subroutine ems_g_rcp_alt_pv(rcp_alt_pv,
     &     vr_n, mtx_r_v, mtx_r_ix, mtx_c_sa, vec)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      double precision rcp_alt_pv
      double precision mtx_r_v(0:n_a_el), vec(0:n_r)
      integer vr_n, mtx_r_ix(0:n_a_el), mtx_c_sa(0:n_c+1)
      double precision v
      integer el_n, r_n
 
      if (vr_n .le. n_c) then
         v = zero
         do 10, el_n = mtx_c_sa(vr_n), mtx_c_sa(vr_n+1)-1
            r_n = mtx_r_ix(el_n)
            if (vec(r_n) .ne. zero) v = v + vec(r_n)*mtx_r_v(el_n)
 10      continue
      else
         v = -vec(vr_n-mx_n_c)
      endif
      rcp_alt_pv = v
      return
      end
 
C->>> ---------------------------------------------> ems_u_lg_du_act <<<
c     Update the dual activities for the logicals in vr_in_c by adding
c     in the vector pi_v which may be a linear combination of rows of
c     the basis inverse if there have been basic cost changes.
c
      subroutine ems_u_lg_du_act(vr_in_c, du_act, pi_v, pi_ix)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICS.INC'
      include 'ICTVR.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      integer vr_in_c(-vr_in_c_n_sn:n_c), pi_ix(0:n_r)
      double precision du_act(0:mx_n_c+n_r)
      double precision pi_v(0:n_r)
      integer c_n, vr_n, ix_n, r_n
      integer c_loop_ln
 
      c_loop_ln = vr_in_c(os_lg_in_c_l_pc_p)
      if (pi_ix(0) .gt. n_r .or.
     &     tbu_r_loop_mode .eq. tbu_r_loop_no .or.
     &     (tbu_r_loop_mode .eq. tbu_r_loop_poss .and.
     &     2*c_loop_ln .le. pi_ix(0))) then
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl1) call ems_tt_rec(u_lg_du_act_dse_tt, n_bs)
CM      ENDIF
         do 10, c_n = 1, vr_in_c(os_lg_in_c_l_pc_p)
            vr_n = vr_in_c(c_n)
            if (pi_v(vr_n-mx_n_c) .ne. zero)
     &           du_act(vr_n) = du_act(vr_n) - pi_v(vr_n-mx_n_c)
 10      continue
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl1) call ems_tt_rec(-u_lg_du_act_dse_tt, n_bs)
CM      ENDIF
      else
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl1) call ems_tt_rec(u_lg_du_act_sps_tt, n_bs)
CM      ENDIF
         do 20, ix_n = 1, pi_ix(0)
            r_n = pi_ix(ix_n)
            vr_n = mx_n_c + r_n
            du_act(vr_n) = du_act(vr_n) - pi_v(r_n)
 20      continue
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl1) call ems_tt_rec(-u_lg_du_act_sps_tt, n_bs)
CM      ENDIF
      endif
      return
      end
 
C->>> ----------------------------------------> ems_perm_u_lg_du_act <<<
c     Update the dual activities for the logicals in vr_in_c by adding
c     in the vector pi_v which may be a linear combination of rows of
c     the basis inverse if there have been basic cost changes.
c
      subroutine ems_perm_u_lg_du_act(vr_in_c, du_act, pi_v, pi_ix,
     &     og_t_nw_perm, nw_t_og_perm)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICS.INC'
      include 'ICTVR.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      integer vr_in_c(-vr_in_c_n_sn:n_c), pi_ix(0:n_r)
      integer og_t_nw_perm(0:n_r)
      integer nw_t_og_perm(0:n_r)
      double precision du_act(0:mx_n_c+n_r)
      double precision pi_v(0:n_r)
      integer c_n, vr_n, nw_r_n, ix_n, r_n, og_r_n
      integer c_loop_ln
 
      c_loop_ln = vr_in_c(os_lg_in_c_l_pc_p)
      if (pi_ix(0) .gt. n_r .or.
     &     tbu_r_loop_mode .eq. tbu_r_loop_no .or.
     &     (tbu_r_loop_mode .eq. tbu_r_loop_poss .and.
     &     2*c_loop_ln .le. pi_ix(0))) then
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl1) call ems_tt_rec(u_lg_du_act_dse_tt, n_bs)
CM      ENDIF
         do 10, c_n = 1, vr_in_c(os_lg_in_c_l_pc_p)
            vr_n = vr_in_c(c_n)
            nw_r_n = og_t_nw_perm(vr_n-mx_n_c)
            if (pi_v(nw_r_n) .ne. zero)
     &           du_act(vr_n) = du_act(vr_n) - pi_v(nw_r_n)
 10      continue
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl1) call ems_tt_rec(-u_lg_du_act_dse_tt, n_bs)
CM      ENDIF
      else
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl1) call ems_tt_rec(u_lg_du_act_sps_tt, n_bs)
CM      ENDIF
         do 20, ix_n = 1, pi_ix(0)
            r_n = pi_ix(ix_n)
            og_r_n = nw_t_og_perm(r_n)
            vr_n = mx_n_c + og_r_n
            du_act(vr_n) = du_act(vr_n) - pi_v(r_n)
 20      continue
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl1) call ems_tt_rec(-u_lg_du_act_sps_tt, n_bs)
CM      ENDIF
      endif
      return
      end
 
C->>> ------------------------------------------> ems_u_struc_du_act <<<
c     Update the dual activities for the structurals in vr_in_c using
c     the vector tbu_r_v which must be zeroed on exit. Add in tbu_r_v
c     which may be a linear combination of tableau rows if there have
c     been basic cost changes.
c
      subroutine ems_u_struc_du_act(vr_in_c, du_act, tbu_r_v, tbu_r_ix)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICS.INC'
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      integer vr_in_c(-vr_in_c_n_sn:n_c)
      double precision du_act(0:mx_n_c+n_r)
      double precision tbu_r_v(0:n_c)
      integer tbu_r_ix(0:n_c)
      integer c_n, vr_n, c_loop_ln, ix_n
      integer tbu_r_n_nz, tbu_r_n_c
 
      c_loop_ln =
     &     vr_in_c(os_struc_in_c_l_pc_p) -
     &     vr_in_c(os_struc_in_c_f_p_m1)
      tbu_r_n_c = c_loop_ln
      if (tbu_r_ix(0) .gt. n_c .or.
     &     tbu_r_loop_mode .eq. tbu_r_loop_no .or.
     &     (tbu_r_loop_mode .eq. tbu_r_loop_poss .and.
     &     2*c_loop_ln .le. tbu_r_ix(0))) then
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl1)
C?     &        call ems_tt_rec(u_struc_du_act_sps_tt, n_bs)
CM      ENDIF
         tbu_r_n_nz = 0
         do 10, c_n = vr_in_c(os_struc_in_c_f_p_m1) + 1,
     &        vr_in_c(os_struc_in_c_l_pc_p)
            vr_n = vr_in_c(c_n)
            if (tbu_r_v(vr_n) .ne. zero) then
               tbu_r_n_nz = tbu_r_n_nz + 1
               du_act(vr_n) = du_act(vr_n) + tbu_r_v(vr_n)
               tbu_r_v(vr_n) = zero
            endif
 10      continue
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl1)
C?     &        call ems_tt_rec(-u_struc_du_act_sps_tt, n_bs)
CM      ENDIF
      else
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl1)
C?     &        call ems_tt_rec(u_struc_du_act_dse_tt, n_bs)
CM      ENDIF
         tbu_r_n_nz = tbu_r_ix(0)
         do 20, ix_n = 1, tbu_r_ix(0)
            vr_n = tbu_r_ix(ix_n)
            du_act(vr_n) = du_act(vr_n) + tbu_r_v(vr_n)
            tbu_r_v(vr_n) = zero
 20      continue
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl1)
C?     &        call ems_tt_rec(-u_struc_du_act_dse_tt, n_bs)
CM      ENDIF
      endif
c
c     Get the density of the tableau row (if required).
c
      if (tbu_r_dse .lt. zero) then
         if (tbu_r_n_c .le. 0) then
c
c     This situation---no nonbasic structurals---is pretty unlikely.
c     Setting tbu_r_dse to an illegal value (rather than 0 or 1) seems
c     sensible.
c
            tbu_r_dse = two
         else
            tbu_r_dse = float(tbu_r_n_nz)/float(tbu_r_n_c)
         endif
      endif
      if (iand(ck_msk, ze_a_ck_bt) .ne. 0)
     &     call ems_ck_ze_rl_a(n_c, tbu_r_v)
      return
      end
 
C->>> ---------------------------------------------> ems_g_rfn_fu_pi <<<
c     Form the full pi = B^{-T}c_B, using iterative refinement
c
      subroutine ems_g_rfn_fu_pi(ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      include 'EMSMSGN.INC'
      include 'EMSRTCOD.INC'
      double precision ds(0:ds_n_en_m1)
      integer is(0:is_n_en_m1)
      integer ems_rt_cod
      integer ca_ems_rt_cod
c
c     Copy the values and indices of the basic costs
c
      call ems_cp_nz_v_ix(n_r,
     &     is(p_bc_co_ix),
     &     ds(p_bc_co_v),
     &     is(p_pi_ix),
     &     ds(p_pi_v))
c
c     Compute the pi values.
c
CM      IF (emsol_deb .EQ. 1) THEN
C?      if (wr_lp_da .ne. 0) call ems_wr_lp_da(3, ds, is)
CM      ENDIF
      if (sto_btran_ix .eq. sto_ix_no) is(p_pi_ix) = n_r+1
      call ems_btran(ds(p_pi_v), is(p_pi_ix), ds, is)
      call ems_it_rfn(
     &     ca_ems_rt_cod, is, ds, 2,
     &     .true.,
     &     is(p_vr_in_r),
     &     ds(p_mtx_r_v), is(p_mtx_r_ix), is(p_mtx_c_sa),
     &     ds(p_pi_v), n_r+1,
     &     ds(p_bc_co_v), n_r+1,
     &     ds(p_pv_c_v),
     &     mx_n_reset_pi_rfn_it,
     &     tl_reset_pi_it_rfn,
     &     reset_pi_it_rfn_tran_ze,
     &     'Calculating B^Tpi = c_B')
      is(p_pi_ix) = n_r+1
      if (ca_ems_rt_cod .ne. ems_rt_cod_ok) then
         ems_rt_cod = max(ca_ems_rt_cod, ems_rt_cod)
c     if (ems_rt_cod .ge. ems_rt_lvl_serious) goto 7100
         if (ems_rt_cod .ge. ems_rt_lvl_serious) goto 8950
      endif
 7100 continue
      return
 8950 continue
      ems_msg_cod = ems_msg_lvl_serious
      goto 7100
      end
 
C->>> ------------------------------------------> ems_g_du_act_fm_pi <<<
c     Compute the dual activities for all variables in the given list.
c
      subroutine ems_g_du_act_fm_pi(
     &     vr_in_c, rsmi_co,
     &     mtx_r_v, mtx_r_ix, mtx_c_sa,
     &     pi_v, pi_ix, du_act)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'RSMICOM.INC'
      include 'RLCTVR.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      integer vr_in_c(-vr_in_c_n_sn:n_c)
      integer mtx_r_ix(0:n_a_el)
      integer mtx_c_sa(0:n_c+1)
      integer pi_ix(0:n_r)
      double precision rsmi_co(0:mx_n_c+n_r)
      double precision mtx_r_v(0:n_a_el)
      double precision pi_v(0:n_r)
      double precision du_act(0:mx_n_c+n_r)
      integer c_n, vr_n, el_n
      double precision v, du_act_v
 
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_pc_lvl0) call ems_tt_rec(pc_tt, n_bs)
CM      ENDIF
      if (lp_ph .eq. 1) then
         do 10, c_n = 1, vr_in_c(os_lg_in_c_bw_bp_p)
            vr_n = vr_in_c(c_n)
            du_act(vr_n) =        pr_co_mu*rsmi_co(vr_n)
 10      continue
         do 20, c_n = c_n, vr_in_c(os_lg_in_c_bw_lb_p)
            vr_n = vr_in_c(c_n)
            du_act(vr_n) = -one + pr_co_mu*rsmi_co(vr_n)
 20      continue
         do 30, c_n = c_n, vr_in_c(os_lg_in_c_ab_ub_p)
            vr_n = vr_in_c(c_n)
            du_act(vr_n) =  one + pr_co_mu*rsmi_co(vr_n)
 30      continue
         do 40, c_n = c_n, vr_in_c(os_lg_in_c_l_p)
            vr_n = vr_in_c(c_n)
            du_act(vr_n) =        pr_co_mu*rsmi_co(vr_n)
 40      continue
         do 50, c_n = vr_in_c(os_struc_in_c_f_p_m1) + 1,
     &        vr_in_c(os_struc_in_c_bw_bp_p)
            vr_n = vr_in_c(c_n)
            du_act(vr_n) =        pr_co_mu*rsmi_co(vr_n)
 50      continue
         do 60, c_n = c_n, vr_in_c(os_struc_in_c_bw_lb_p)
            vr_n = vr_in_c(c_n)
            du_act(vr_n) = -one + pr_co_mu*rsmi_co(vr_n)
 60      continue
         do 70, c_n = c_n, vr_in_c(os_struc_in_c_ab_ub_p)
            vr_n = vr_in_c(c_n)
            du_act(vr_n) =  one + pr_co_mu*rsmi_co(vr_n)
 70      continue
         do 80, c_n = c_n, vr_in_c(os_struc_in_c_l_p)
            vr_n = vr_in_c(c_n)
            du_act(vr_n) =        pr_co_mu*rsmi_co(vr_n)
 80      continue
      else
         do 110, c_n = 1, vr_in_c(os_vr_in_c_l_p)
            vr_n = vr_in_c(c_n)
            du_act(vr_n) =        pr_co_mu*rsmi_co(vr_n)
 110     continue
      endif
      do 210, c_n = 1, vr_in_c(os_lg_in_c_l_p)
         vr_n = vr_in_c(c_n)
         du_act(vr_n) = du_act(vr_n) - pi_v(vr_n-mx_n_c)
 210  continue
      do 230, c_n = vr_in_c(os_struc_in_c_f_p_m1) + 1,
     &     vr_in_c(os_struc_in_c_l_p)
         vr_n = vr_in_c(c_n)
         du_act_v = du_act(vr_n)
         do 220, el_n = mtx_c_sa(vr_n), mtx_c_sa(vr_n+1)-1
            v = pi_v(mtx_r_ix(el_n))
            if (v .ne. zero) du_act_v = du_act_v + v*mtx_r_v(el_n)
 220     continue
         du_act(vr_n) = du_act_v
 230  continue
      call ems_ze_pi_v(pi_v, pi_ix)
c
c     Indicate that the pi vector has been zeroed. This has to be done
c     because, in g_bc_fs_cg, any (phase one) basic cost changes due
c     non-pivotal rows becoming feasible are accumulated in the pi
c     vector and g_bc_fs_cg should not assume that pi is zero on entry
c     (because it may be called more than once before the dual
c     activities are updated.)
c
      pi_ix(0) = 0
c
c     Indicate that the model nonbasic dual activities are now correct.
c
      ml_da_st_msk = ior(ml_da_st_msk, ml_da_st_non_bc_du_act)
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_pc_lvl0) call ems_tt_rec(-pc_tt, n_bs)
CM      ENDIF
      return
      end
 
C->>> ---------------------------------------------> ems_g_lg_du_act <<<
c     Compute the dual activities for the logicals.
c
      subroutine ems_g_lg_du_act(vr_in_c, rsmi_co, du_act, pi_v)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'RSMICS.INC'
      include 'RSMICOM.INC'
      include 'RLCTVR.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      integer vr_in_c(-vr_in_c_n_sn:n_c)
      double precision rsmi_co(0:mx_n_c+n_r)
      double precision du_act(0:mx_n_c+n_r)
      double precision pi_v(0:n_r)
      integer c_n, vr_n
 
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_pc_lvl1) call ems_tt_rec(g_lg_du_act_dse_tt, n_bs)
CM      ENDIF
      if (lp_ph .eq. 1) then
         do 10, c_n = 1, vr_in_c(os_lg_in_c_bw_bp_p)
            vr_n = vr_in_c(c_n)
            du_act(vr_n) =
     &           -pi_v(vr_n-mx_n_c) +       pr_co_mu*rsmi_co(vr_n)
 10      continue
         do 20 c_n = c_n, vr_in_c(os_lg_in_c_bw_lb_p)
            vr_n = vr_in_c(c_n)
            du_act(vr_n) =
     &           -pi_v(vr_n-mx_n_c) - one + pr_co_mu*rsmi_co(vr_n)
 20      continue
         do 30, c_n = c_n, vr_in_c(os_lg_in_c_ab_ub_p)
            vr_n = vr_in_c(c_n)
            du_act(vr_n) =
     &           -pi_v(vr_n-mx_n_c) + one + pr_co_mu*rsmi_co(vr_n)
 30      continue
         do 40, c_n = c_n, vr_in_c(os_lg_in_c_l_pc_p)
            vr_n = vr_in_c(c_n)
            du_act(vr_n) =
     &           -pi_v(vr_n-mx_n_c) +       pr_co_mu*rsmi_co(vr_n)
 40      continue
      else
         do 110 c_n = 1, vr_in_c(os_lg_in_c_l_pc_p)
            vr_n = vr_in_c(c_n)
            du_act(vr_n) =
     &           -pi_v(vr_n-mx_n_c) +       pr_co_mu*rsmi_co(vr_n)
 110     continue
      endif
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_pc_lvl1) call ems_tt_rec(-g_lg_du_act_dse_tt, n_bs)
CM      ENDIF
      return
      end
 
C->>> ----------------------------------------> ems_perm_g_lg_du_act <<<
c     Compute the dual activities for the logicals.
c
      subroutine ems_perm_g_lg_du_act(vr_in_c, rsmi_co, du_act, pi_v,
     &     og_t_nw_perm)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'RSMICS.INC'
      include 'RSMICOM.INC'
      include 'RLCTVR.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      integer vr_in_c(-vr_in_c_n_sn:n_c)
      integer og_t_nw_perm(0:n_r)
      double precision rsmi_co(0:mx_n_c+n_r)
      double precision du_act(0:mx_n_c+n_r)
      double precision pi_v(0:n_r)
      integer c_n, nw_r_n, vr_n
 
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_pc_lvl1) call ems_tt_rec(g_lg_du_act_dse_tt, n_bs)
CM      ENDIF
      if (lp_ph .eq. 1) then
         do 10 c_n = 1, vr_in_c(os_lg_in_c_bw_bp_p)
            vr_n = vr_in_c(c_n)
            nw_r_n = og_t_nw_perm(vr_n-mx_n_c)
            du_act(vr_n) = -pi_v(nw_r_n)       + pr_co_mu*rsmi_co(vr_n)
 10      continue
         do 20 c_n = c_n, vr_in_c(os_lg_in_c_bw_lb_p)
            vr_n = vr_in_c(c_n)
            nw_r_n = og_t_nw_perm(vr_n-mx_n_c)
            du_act(vr_n) = -pi_v(nw_r_n) - one + pr_co_mu*rsmi_co(vr_n)
 20      continue
         do 30, c_n = c_n, vr_in_c(os_lg_in_c_ab_ub_p)
            vr_n = vr_in_c(c_n)
            nw_r_n = og_t_nw_perm(vr_n-mx_n_c)
            du_act(vr_n) = -pi_v(nw_r_n) + one + pr_co_mu*rsmi_co(vr_n)
 30      continue
         do 40, c_n = c_n, vr_in_c(os_lg_in_c_l_pc_p)
            vr_n = vr_in_c(c_n)
            nw_r_n = og_t_nw_perm(vr_n-mx_n_c)
            du_act(vr_n) = -pi_v(nw_r_n) +       pr_co_mu*rsmi_co(vr_n)
 40      continue
      else
         do 110 c_n = 1, vr_in_c(os_lg_in_c_l_pc_p)
            vr_n = vr_in_c(c_n)
            nw_r_n = og_t_nw_perm(vr_n-mx_n_c)
            du_act(vr_n) = -pi_v(nw_r_n) +       pr_co_mu*rsmi_co(vr_n)
 110     continue
      endif
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_pc_lvl1) call ems_tt_rec(-g_lg_du_act_dse_tt, n_bs)
CM      ENDIF
      return
      end
 
C->>> ------------------------------------------> ems_g_struc_du_act <<<
c     Compute the dual activities for the structurals.
c     tbu_r_v must be zeroed on exit.
c
      subroutine ems_g_struc_du_act(vr_in_c, rsmi_co, du_act,
     &     tbu_r_v, tbu_r_ix)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'RSMICS.INC'
      include 'RSMICOM.INC'
      include 'RLCTVR.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      integer vr_in_c(-vr_in_c_n_sn:n_c)
      double precision rsmi_co(0:mx_n_c+n_r)
      double precision du_act(0:mx_n_c+n_r)
      double precision tbu_r_v(0:mx_n_c+n_r)
      integer tbu_r_ix(0:n_c)
      integer c_n, vr_n
 
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_pc_lvl1) call ems_tt_rec(g_struc_du_act_dse_tt, n_bs)
CM      ENDIF
      if (lp_ph .eq. 1) then
         do 10, c_n = vr_in_c(os_struc_in_c_f_p_m1) + 1,
     &        vr_in_c(os_struc_in_c_bw_bp_p)
            vr_n = vr_in_c(c_n)
            du_act(vr_n) = tbu_r_v(vr_n)       + pr_co_mu*rsmi_co(vr_n)
            tbu_r_v(vr_n) = zero
 10      continue
         do 20, c_n = c_n, vr_in_c(os_struc_in_c_bw_lb_p)
            vr_n = vr_in_c(c_n)
            du_act(vr_n) = tbu_r_v(vr_n) - one + pr_co_mu*rsmi_co(vr_n)
            tbu_r_v(vr_n) = zero
 20      continue
         do 30, c_n = c_n, vr_in_c(os_struc_in_c_ab_ub_p)
            vr_n = vr_in_c(c_n)
            du_act(vr_n) = tbu_r_v(vr_n) + one + pr_co_mu*rsmi_co(vr_n)
            tbu_r_v(vr_n) = zero
 30      continue
         do 40, c_n = c_n, vr_in_c(os_struc_in_c_l_pc_p)
            vr_n = vr_in_c(c_n)
            du_act(vr_n) = tbu_r_v(vr_n) +       pr_co_mu*rsmi_co(vr_n)
            tbu_r_v(vr_n) = zero
 40      continue
      else
         do 110, c_n = vr_in_c(os_struc_in_c_f_p_m1) + 1,
     &        vr_in_c(os_struc_in_c_l_pc_p)
            vr_n = vr_in_c(c_n)
            du_act(vr_n) = tbu_r_v(vr_n) +       pr_co_mu*rsmi_co(vr_n)
            tbu_r_v(vr_n) = zero
 110     continue
      endif
      if (iand(ck_msk, ze_a_ck_bt) .ne. 0)
     &     call ems_ck_ze_rl_a(n_c, tbu_r_v)
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_pc_lvl1) call ems_tt_rec(-g_struc_du_act_dse_tt, n_bs)
CM      ENDIF
      return
      end
 
C->>> ------------------------------------------> ems_perm_btran_sol <<<
c     Permutes the values (and indices if sparse) for the btran solution
c     whose handle is passed. The handle must be passed since it is
c     swapped with that of the destination of the permuted solution---to
c     avoid copying the whole vector. The vector supplied is zeroed.
c
      subroutine ems_perm_btran_sol(
     &     p_btran_sol, hdl_btran_sol, btran_sol_ix, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMIHDL.INC'
      include 'RSMICS.INC'
      include 'ICTVR.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      integer p_btran_sol, hdl_btran_sol(0:hdl_z_m1)
      integer btran_sol_ix(0:n_r)
      integer is(0:is_n_en_m1)
      double precision ds(0:ds_n_en_m1)
      integer n_ix, ix_n, nw_r_n, og_r_n
 
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tt_da .gt. 0) call ems_tt_rec(perm_vec_tt, n_bs)
CM      ENDIF
      if (iand(ck_msk, ze_a_ck_bt) .ne. 0)
     &     call ems_ck_ze_rl_a(n_r, ds(p_perm_tran_vec))
      n_ix = btran_sol_ix(0)
      if (n_ix .le. n_r) then
c
c     Permute the indices in place and permute the values in copying
c     from btran_sol to perm_btran_sol.
c
         do 10, ix_n = 1, n_ix
            nw_r_n = btran_sol_ix(ix_n)
            og_r_n = is(p_nw_t_og_perm+nw_r_n)
            ds(p_perm_tran_vec+og_r_n) = ds(p_btran_sol+nw_r_n)
            btran_sol_ix(ix_n) = og_r_n
            ds(p_btran_sol+nw_r_n) = zero
 10      continue
      else
c
c     Permute the values in copying from btran_sol to perm_btran_sol.
c
         do 20, nw_r_n = 1, n_r
            if (ds(p_btran_sol+nw_r_n) .ne. zero) then
               og_r_n = is(p_nw_t_og_perm+nw_r_n)
               ds(p_perm_tran_vec+og_r_n) = ds(p_btran_sol+nw_r_n)
               ds(p_btran_sol+nw_r_n) = zero
            endif
 20      continue
      endif
c
c     Now the trickery! Exchange handles between btran_sol and
c     perm_btran_sol, making it look as if the original vector has been
c     permuted in place.
c
      call ems_exch_hdl(hdl_btran_sol, hdl_perm_tran_vec)
      if (iand(ck_msk, ze_a_ck_bt) .ne. 0)
     &     call ems_ck_ze_rl_a(n_r, ds(p_btran_sol))
      ix_n = p_btran_sol
      p_btran_sol = p_perm_tran_vec
      p_perm_tran_vec= ix_n
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tt_da .gt. 0) call ems_tt_rec(-perm_vec_tt, n_bs)
CM      ENDIF
      return
      end
 
C->>> --------------------------------------------> ems_reset_pc_alg <<<
c     Resets the pricing algorithm according to nw_pc_alg and the
c     existing value of pc_alg
c
      subroutine ems_reset_pc_alg(nw_pc_alg, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICOM.INC'
      include 'EMSMSG.INC'
      include 'ICTVR.INC'
      integer nw_pc_alg, is(0:is_n_en_m1)
 
      if (nw_pc_alg .eq. pc_alg) goto 7000
c
c     The pricing algorithm has changed. However, pc_alg_approx_dvx and
c     pc_alg_exact_dvx are currently equivalent so rule out this null
c     change
c
      if ((
     &     nw_pc_alg .eq. pc_alg_approx_dvx .or.
     &     nw_pc_alg .eq. pc_alg_exact_dvx) .and. (
     &     pc_alg .eq. pc_alg_approx_dvx .or.
     &     pc_alg .eq. pc_alg_exact_dvx)) then
         pc_alg = nw_pc_alg
         goto 7000
      endif
c
c     The change in pricing algorithm means that any previous edge
c     weights are now incorrect.
c
      ml_da_st_msk =
     &     ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_ed_wt)
c
c     Remove any Devex data structure
c
      if (iand(ml_blk_st_msk, ml_blk_st_dvx) .ne. 0) then
         call ems_rm_blk_dvx(is)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      endif
      if (nw_pc_alg .eq. pc_alg_approx_dvx .or.
     &     nw_pc_alg .eq. pc_alg_exact_dvx) then
c
c     If using Devex, allocate the Devex data structure and initialise
c     the count of Devex frameworks
c
         n_dvx_fwk = 0
         call ems_iz_blk_dvx(is)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      else if (nw_pc_alg .eq. pc_alg_dan) then
c
c     If using Dantzig then the edge weights are (vacuously) correct.
c
         ml_da_st_msk = ior(ml_da_st_msk, ml_da_st_ed_wt)
      endif
c
c     Change the pricing strategy
c
      pc_alg = nw_pc_alg
 7000 continue
      return
      end
 
C->>> ------------------------------------------------> ems_iz_ed_wt <<<
c     Calls the appropriate routine to initialise the edge weights
c
      subroutine ems_iz_ed_wt(ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
c      include 'RSMICS.INC'
      include 'RSMICOM.INC'
      include 'EMSMSG.INC'
c      include 'EMSMSGN.INC'
      include 'ICTVR.INC'
      integer is(0:is_n_en_m1)
      double precision ds(0:ds_n_en_m1)
 
      if (pc_alg .eq. pc_alg_approx_dvx .or.
     &     pc_alg .eq. pc_alg_exact_dvx) then
         call ems_iz_dvx_fwk(
     &        is(p_vr_in_r),
     &        is(p_vr_in_c),
     &        ds(p_ed_wt),
     &        is(p_dvx_ix))
         nw_dvx_fwk = .false.
      else if (pc_alg .eq. pc_alg_sed) then
         call ems_iz_sed_wt(
     &        is(p_vr_in_r),
     &        is(p_vr_in_c),
     &        ds(p_mtx_r_v),
     &        is(p_mtx_c_sa),
     &        ds(p_ed_wt),
     &        ds, is)
      endif
      return
      end
 
