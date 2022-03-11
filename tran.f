C->>> -----------------------------------------> ems_fwd_tran_eta_se <<<
c     Forward application of f_eta_n, ..., l_eta_n from a set of etas.
c
c     If rhs_ix(0) < n_r then it is assumed that rhs_ix(1:n_r) contains
c     the indices of all the nonzeros of rhs_v (it doesn't matter if it
c     contains indices of zeros). These indices are maintained until the
c     density reaches fwd_tran_dse_rhs_n_r, at which point rhs_ix(0) is
c     set to n_r+1 to indicate that the indices are not known.
c     If indices are not to be maintained then n_r+1 may be passed as
c     rhs_ix.
c
c     If the indices of the RHS are known then super-sparse forward
c     operations may be performed, depending on the availability of
c     eta_pv_in_r.
c     If eta_pv_in_r(0) >= 0 then (up to) eta_pv_in_r(0) super-sparse
c     forward operations may be performed.
c     If super-sparse forward operations are not to be performed (or if
c     eta_pv_in_r is unavailable) then -1 may be passed as eta_pv_in_r.
c
      subroutine ems_fwd_tran_eta_se(
     &     alw_f7_wr, wr_cn, er_fd,
     &     f_eta_n, l_eta_n, n_eta_el,
     &     eta_v, eta_ix, eta_sa, eta_pv_in_r,
     &     rhs_v, rhs_ix)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
CM      IF (emsol_da .EQ. 1) THEN
C?      include 'EMSDA.INC'
C?      include 'TRANDA.INC'
C?c      include 'APPETALS.INC'
CM      ENDIF
      include 'SUSFTRAN.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'TRANTT.INC'
CM      ENDIF
      include 'RLCTVR.INC'
      include 'ICTVR.INC'
      logical alw_f7_wr, er_fd
      integer wr_cn
      integer f_eta_n, l_eta_n, n_eta_el
      integer eta_ix(0:n_eta_el), eta_sa(0:l_eta_n+1)
      integer eta_pv_in_r(0:n_r), rhs_ix(0:n_r)
      double precision eta_v(0:n_eta_el), rhs_v(0:n_r)
      integer fm_eta_n, cdd_eta_n
      integer eta_n, el_n, r_n, pv_r, rhs_ix_n
      integer sus_f_eta_n
      logical tran_sps_rhs
CM      IF (emsol_da .EQ. 1) THEN
C?c      integer app_eta_ls_n_en
CM      ENDIF
 
      er_fd = .false.
      fm_eta_n = f_eta_n
      tran_sps_rhs = rhs_ix(0) .lt. fwd_tran_dse_rhs_n_r
      sus_fwd_tran_mx_n_op = eta_pv_in_r(0)
 100  continue
      el_n = eta_sa(fm_eta_n)
      if (.not.tran_sps_rhs) then
c
c     Don't maintain the indices of nonzeros in the RHS
c
         do 130, eta_n = fm_eta_n, l_eta_n
c
c=======================================================================
c     Start performing forward TRAN operation
c
            pv_r = eta_ix(el_n)
CM      IF (emsol_da .EQ. 1) THEN
C?            if (rhs_v(pv_r) .ne. zero) then
C?               su_n_fwd_tran_en = su_n_fwd_tran_en + 1
C?               if (abs(rhs_v(pv_r)) .le. fwd_tran_ze)
C?     &              su_n_fwd_tran_ze = su_n_fwd_tran_ze + 1
C?            endif
CM      ENDIF
            if (rhs_v(pv_r) .eq. zero .or.
     &           abs(rhs_v(pv_r)) .le. fwd_tran_ze) then
               el_n = eta_sa(eta_n+1)
CM      IF (emsol_da .EQ. 1) THEN
C?               tran_op_da(tran_da_rec_n_fwd_std_eta_skp_en) =
C?     &              tran_op_da(tran_da_rec_n_fwd_std_eta_skp_en) + 1
CM      ENDIF
            else
CM      IF (emsol_da .EQ. 1) THEN
C?c               app_eta_ls_n_en = app_eta_ls(0) + 1
C?c               if (app_eta_ls_n_en .le. app_eta_ls_mx_n_en) then
C?c                  app_eta_ls(app_eta_ls_n_en) = eta_n
C?c                  app_eta_ls(0) = app_eta_ls_n_en
C?c               endif
CM      ENDIF
               rhs_v(pv_r) = rhs_v(pv_r)*eta_v(el_n)
               do 120, el_n = el_n+1, eta_sa(eta_n+1)-1
                  rhs_v(eta_ix(el_n)) =
     &                 rhs_v(eta_ix(el_n)) + rhs_v(pv_r)*eta_v(el_n)
 120           continue
            end if
c
c     Finished performing forward TRAN operation
c=======================================================================
c
 130     continue
CM      IF (emsol_da .EQ. 1) THEN
C?         tran_op_da(tran_da_rec_n_fwd_std_eta) =
C?     &        l_eta_n - fm_eta_n + 1
CM      ENDIF
c
c     Indicate that the nonzeros are not known, being careful in case
c     rhs_ix(0) was passed as n_r+1
c
         if (rhs_ix(0) .lt. n_r) rhs_ix(0) = n_r+1
      else
c
c     Maintain the indices of nonzeros in the RHS
c
c     rhs_ix(0:rhs_ix(0))
c     rhs_ix(sus_eta_ls_f_en:sus_eta_ls_l_en)
c     rhs_ix(rpt_eta_ls_f_en:rpt_eta_ls_l_en)
c
         if (sus_fwd_tran_mx_n_op .lt. 0) goto 300
c
c     Perform some supersparse operations
c
         sus_fwd_tran_n_op = 0
         sus_f_eta_n = f_eta_n
         sus_eta_ls_n_en = -1
         if (rhs_ix(0) .gt. sus_eta_srch_al_t_ls_ix_lm) then
            sus_eta_ls_f_en = n_r/3+1
            rpt_eta_ls_f_en = 2*(n_r/3)+1
            if (sus_eta_ls_f_en .le. fwd_tran_dse_rhs_n_r) goto 8000
            sus_eta_ls_l_en = sus_eta_ls_f_en - 1
            do 210, rhs_ix_n = 1, rhs_ix(0)
               r_n = rhs_ix(rhs_ix_n)
               cdd_eta_n = eta_pv_in_r(r_n)
               if (cdd_eta_n .ge. sus_f_eta_n .and.
     &              cdd_eta_n .le. l_eta_n) then
                  sus_eta_ls_l_en = sus_eta_ls_l_en + 1
                  rhs_ix(sus_eta_ls_l_en) = cdd_eta_n
               endif
 210        continue
            sus_eta_ls_n_en = sus_eta_ls_l_en - sus_eta_ls_f_en + 1
         endif
         if (rhs_ix(0) .le. sus_eta_srch_al_t_ls_ix_lm) then
c
c     Use search-all supersparse fwd TRAN
c
CM      IF (emsol_da .EQ. 1) THEN
C?            tran_op_da(tran_da_rec_n_fwd_al_srch_pv) =
C?     &           sus_fwd_tran_n_op
CM      ENDIF
            call ems_srch_al_sus_fwd_tran(
     &           sus_f_eta_n, l_eta_n, n_eta_el,
     &           eta_v, eta_ix, eta_sa, eta_pv_in_r,
     &           rhs_v, rhs_ix,
     &           fm_eta_n)
CM      IF (emsol_da .EQ. 1) THEN
C?            tran_op_da(tran_da_rec_n_fwd_al_srch_pv) =
C?     &           sus_fwd_tran_n_op -
C?     &           tran_op_da(tran_da_rec_n_fwd_al_srch_pv)
CM      ENDIF
         else if (sus_eta_ls_n_en .le. sus_eta_srch_ls_t_buk_ix_lm) then
c
c     Use search-list supersparse fwd TRAN
c
CM      IF (emsol_da .EQ. 1) THEN
C?            tran_op_da(tran_da_rec_n_fwd_ls_srch_pv) =
C?     &           sus_fwd_tran_n_op
CM      ENDIF
            call ems_srch_ls_sus_fwd_tran(
     &           sus_f_eta_n, l_eta_n, n_eta_el,
     &           eta_v, eta_ix, eta_sa, eta_pv_in_r,
     &           rhs_v, rhs_ix,
     &           fm_eta_n)
CM      IF (emsol_da .EQ. 1) THEN
C?            tran_op_da(tran_da_rec_n_fwd_ls_srch_pv) =
C?     &           sus_fwd_tran_n_op -
C?     &           tran_op_da(tran_da_rec_n_fwd_ls_srch_pv)
CM      ENDIF
         else
c
c     Use search-buckets supersparse fwd TRAN
c
CM      IF (emsol_da .EQ. 1) THEN
C?            tran_op_da(tran_da_rec_n_fwd_buk_srch_pv) =
C?     &           sus_fwd_tran_n_op
CM      ENDIF
            call ems_srch_buk_sus_fwd_tran(
     &           sus_f_eta_n, l_eta_n, n_eta_el,
     &           eta_v, eta_ix, eta_sa, eta_pv_in_r,
     &           rhs_v, rhs_ix,
     &           fm_eta_n)
CM      IF (emsol_da .EQ. 1) THEN
C?            tran_op_da(tran_da_rec_n_fwd_buk_srch_pv) =
C?     &           sus_fwd_tran_n_op -
C?     &           tran_op_da(tran_da_rec_n_fwd_buk_srch_pv)
CM      ENDIF
         endif
 
         if (fm_eta_n .gt. l_eta_n) then
CM      IF (emsol_da .EQ. 1) THEN
C?            tran_op_da(tran_da_rec_n_fwd_sus_eta) =
C?     &           l_eta_n - sus_f_eta_n + 1
CM      ENDIF
            goto 400
         endif
         tran_sps_rhs = rhs_ix(0) .lt. fwd_tran_dse_rhs_n_r
         if (.not. tran_sps_rhs) then
CM      IF (emsol_da .EQ. 1) THEN
C?            tran_op_da(tran_da_rec_n_fwd_sus_eta) =
C?     &           fm_eta_n - sus_f_eta_n
CM      ENDIF
            goto 100
         endif
CM      IF (emsol_da .EQ. 1) THEN
C?         tran_op_da(tran_da_rec_n_fwd_sus_eta) =
C?     &        fm_eta_n-1 - sus_f_eta_n + 1
CM      ENDIF
 300     continue
         el_n = eta_sa(fm_eta_n)
         do 330, eta_n = fm_eta_n, l_eta_n
c
c=======================================================================
c     Start performing sparse forward TRAN operation
c
            pv_r = eta_ix(el_n)
CM      IF (emsol_da .EQ. 1) THEN
C?            if (rhs_v(pv_r) .ne. zero) then
C?               su_n_fwd_tran_en = su_n_fwd_tran_en + 1
C?               if (abs(rhs_v(pv_r)) .le. fwd_tran_ze)
C?     &              su_n_fwd_tran_ze = su_n_fwd_tran_ze + 1
C?            endif
CM      ENDIF
            if (rhs_v(pv_r) .eq. zero .or.
     &           abs(rhs_v(pv_r)) .le. fwd_tran_ze) then
               el_n = eta_sa(eta_n+1)
CM      IF (emsol_da .EQ. 1) THEN
C?               tran_op_da(tran_da_rec_n_fwd_sps_eta_skp_en) =
C?     &              tran_op_da(tran_da_rec_n_fwd_sps_eta_skp_en) + 1
CM      ENDIF
            else
CM      IF (emsol_da .EQ. 1) THEN
C?c               app_eta_ls_n_en = app_eta_ls(0) + 1
C?c               if (app_eta_ls_n_en .le. app_eta_ls_mx_n_en) then
C?c                  app_eta_ls(app_eta_ls_n_en) = eta_n
C?c                  app_eta_ls(0) = app_eta_ls_n_en
C?c               endif
CM      ENDIF
               rhs_v(pv_r) = rhs_v(pv_r)*eta_v(el_n)
               do 320, el_n = el_n+1, eta_sa(eta_n+1)-1
                  r_n = eta_ix(el_n)
                  if (rhs_v(r_n) .eq. zero) then
c
c     The RHS entry was zero so a new nonzero must be created.
c
                     rhs_v(r_n) = rhs_v(pv_r)*eta_v(el_n)
                     rhs_ix(0) = rhs_ix(0) + 1
                     if (rhs_ix(0) .le. n_r) rhs_ix(rhs_ix(0)) = r_n
                  else
c
c     The RHS entry is nonzero.
c
                     rhs_v(r_n) = rhs_v(r_n) + rhs_v(pv_r)*eta_v(el_n)
                  end if
 320           continue
            end if
c
c     Finished performing sparse forward TRAN operation
c=======================================================================
c
c     If the RHS becomes dense then stop maintaining the indices of its
c     nonzeros, zero the array used to index into the list of nonzeros
c     and indicate that the indices are to be ignored.
c
            if (rhs_ix(0) .ge. fwd_tran_dse_rhs_n_r) then
               tran_sps_rhs = .false.
               rhs_ix(0) = n_r + 1
CM      IF (emsol_da .EQ. 1) THEN
C?               tran_op_da(tran_da_rec_n_fwd_sps_eta) =
C?     &              eta_n - fm_eta_n + 1
CM      ENDIF
               fm_eta_n = eta_n+1
               go to 100
            end if
 330     continue
CM      IF (emsol_da .EQ. 1) THEN
C?         tran_op_da(tran_da_rec_n_fwd_sps_eta) =
C?     &        l_eta_n - fm_eta_n + 1
CM      ENDIF
 400     continue
      end if
CM      IF (emsol_da .EQ. 1) THEN
C?      tran_op_da(tran_da_rec_n_fwd_eta) = l_eta_n - f_eta_n + 1
CM      ENDIF
 7000 continue
      return
 8000 continue
      if (alw_f7_wr) write(wr_cn, 9800)
     &     sus_eta_ls_f_en, fwd_tran_dse_rhs_n_r, n_r
      er_fd = .true.
      goto 7000
 9800 format('sus_eta_ls_f_en .le. fwd_tran_dse_rhs_n_r ', 3(2x, i7))
      end
 
      subroutine ems_srch_al_sus_fwd_tran(
     &     sus_f_eta_n, l_eta_n, n_eta_el,
     &     eta_v, eta_ix, eta_sa, eta_pv_in_r,
     &     rhs_v, rhs_ix,
     &     fm_eta_n)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
CM      IF (emsol_da .EQ. 1) THEN
C?      include 'EMSDA.INC'
C?      include 'TRANDA.INC'
C?c      include 'APPETALS.INC'
CM      ENDIF
      include 'SUSFTRAN.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'TRANTT.INC'
CM      ENDIF
      include 'RLCTVR.INC'
      include 'ICTVR.INC'
 
      integer sus_f_eta_n, l_eta_n, n_eta_el
      integer eta_ix(0:n_eta_el), eta_sa(0:l_eta_n+1)
      integer eta_pv_in_r(0:n_r), rhs_ix(0:n_r)
      double precision eta_v(0:n_eta_el), rhs_v(0:n_r)
      integer fm_eta_n
      integer dl_eta_n
      integer av_dl_eta_n
      integer mx_nx_dl_eta_n
      integer tru_eta_srch_co
CM      IF (emsol_da .EQ. 1) THEN
C?      integer alt_av_dl_eta_n
CM      ENDIF
      integer eta_n, el_n, r_n, pv_r, rhs_ix_n
CM      IF (emsol_da .EQ. 1) THEN
C?c      integer app_eta_ls_n_en
CM      ENDIF
 
      eta_n = sus_f_eta_n-1
 250  continue
c
c     Get the next eta to apply
c
CM      IF (emsol_tt .EQ. 1) THEN
C?c      call ems_tt_rec(sus_fwd_tran_eta_srch_tt, 0)
CM      ENDIF
      dl_eta_n = eta_n
      fm_eta_n = l_eta_n+1
      do 260, rhs_ix_n = 1, rhs_ix(0)
         r_n = rhs_ix(rhs_ix_n)
         if (eta_pv_in_r(r_n) .gt. eta_n)
     &        fm_eta_n = min(eta_pv_in_r(r_n), fm_eta_n)
 260  continue
CM      IF (emsol_tt .EQ. 1) THEN
C?c      call ems_tt_rec(-sus_fwd_tran_eta_srch_tt, 0)
CM      ENDIF
c
c     If none needs to be applied then return
c
      if (fm_eta_n .gt. l_eta_n) goto 400
c
c     If no more super-sparse operations should be applied then continue
c     with sparse forward TRAN
c
      if (sus_fwd_tran_n_op .ge. sus_fwd_tran_mx_n_op) goto 300
      sus_fwd_tran_n_op = sus_fwd_tran_n_op + 1
      dl_eta_n = fm_eta_n - dl_eta_n + 1
      eta_n = fm_eta_n
c
c=======================================================================
c     Start performing super-sparse forward TRAN operation
c
      el_n = eta_sa(eta_n)
      pv_r = eta_ix(el_n)
CM      IF (emsol_da .EQ. 1) THEN
C?      if (rhs_v(pv_r) .ne. zero) then
C?         su_n_fwd_tran_en = su_n_fwd_tran_en + 1
C?         if (abs(rhs_v(pv_r)) .le. fwd_tran_ze)
C?     &        su_n_fwd_tran_ze = su_n_fwd_tran_ze + 1
C?      endif
CM      ENDIF
      if (rhs_v(pv_r) .eq. zero .or.
     &     abs(rhs_v(pv_r)) .le. fwd_tran_ze) then
         el_n = eta_sa(eta_n+1)
CM      IF (emsol_da .EQ. 1) THEN
C?         tran_op_da(tran_da_rec_n_fwd_sus_eta_skp_en) =
C?     &        tran_op_da(tran_da_rec_n_fwd_sus_eta_skp_en) + 1
CM      ENDIF
      else
CM      IF (emsol_da .EQ. 1) THEN
C?c            app_eta_ls_n_en = app_eta_ls(0) + 1
C?c            if (app_eta_ls_n_en .le. app_eta_ls_mx_n_en) then
C?c               app_eta_ls(app_eta_ls_n_en) = eta_n
C?c               app_eta_ls(0) = app_eta_ls_n_en
C?c            endif
CM      ENDIF
         rhs_v(pv_r) = rhs_v(pv_r)*eta_v(el_n)
         do 295, el_n = el_n+1, eta_sa(eta_n+1)-1
            r_n = eta_ix(el_n)
            if (rhs_v(r_n) .eq. zero) then
               rhs_v(r_n) = rhs_v(pv_r)*eta_v(el_n)
               rhs_ix(0) = rhs_ix(0) + 1
               if (rhs_ix(0) .lt. fwd_tran_dse_rhs_n_r)
     &              rhs_ix(rhs_ix(0)) = r_n
            else
               rhs_v(r_n) = rhs_v(r_n) + rhs_v(pv_r)*eta_v(el_n)
            end if
 295     continue
      end if
c
c     Finished performing super-sparse forward TRAN operation
c=======================================================================
c
      if (rhs_ix(0) .ge. fwd_tran_dse_rhs_n_r) then
         rhs_ix(0) = n_r + 1
         fm_eta_n = eta_n+1
         go to 7000
      end if
c
c     Calculate a weighted average of the past two eta skips and
c     compare it with the cost of the search.
c
      if (sus_fwd_tran_n_op .eq. 1) then
CM      IF (emsol_da .EQ. 1) THEN
C?         if (rp_sus_eta_op .gt. 0) write(*, 9100)
CM      ENDIF
         prev_dl_eta_n = dl_eta_n
      endif
      av_dl_eta_n = (2*dl_eta_n+prev_dl_eta_n)/3
c
c     The cost of the search for the next eta to apply is proportional
c     to the number of indices in the RHS
c
      tru_eta_srch_co = rhs_ix(0)
      eta_srch_co = sus_eta_srch_co_mu*tru_eta_srch_co
      mx_nx_dl_eta_n = l_eta_n-eta_n
CM      IF (emsol_da .EQ. 1) THEN
C?      if (rp_sus_eta_op .gt. 0) then
C?         alt_av_dl_eta_n = n_r/rhs_ix(0)
C?         if (tru_eta_srch_co .gt. av_dl_eta_n) then
C?            write(*, 9110)sus_fwd_tran_n_op,
C?     &           rhs_ix(0), alt_av_dl_eta_n, dl_eta_n,
C?     &           av_dl_eta_n, mx_nx_dl_eta_n, tru_eta_srch_co, 1
C?         else if (tru_eta_srch_co .gt. mx_nx_dl_eta_n) then
C?            write(*, 9110)sus_fwd_tran_n_op,
C?     &           rhs_ix(0), alt_av_dl_eta_n, dl_eta_n,
C?     &           av_dl_eta_n, mx_nx_dl_eta_n, tru_eta_srch_co, 2
C?         else
C?            write(*, 9120)sus_fwd_tran_n_op,
C?     &           rhs_ix(0), alt_av_dl_eta_n, dl_eta_n,
C?     &           av_dl_eta_n, mx_nx_dl_eta_n, tru_eta_srch_co
C?         endif
C?      endif
CM      ENDIF
      if (sus_fwd_tran_n_op .gt. 1 .and.
     &     (eta_srch_co .gt. av_dl_eta_n .or.
     &     eta_srch_co .gt. mx_nx_dl_eta_n)) then
c
c     The cost of super-sparse operations is uncompetitive: stop any
c     further super-sparse operations by setting
c     sus_fwd_tran_mx_n_op = -1
c
         fm_eta_n = eta_n+1
         sus_fwd_tran_mx_n_op = -1
         go to 7000
      endif
      prev_dl_eta_n = dl_eta_n
      goto 250
 300  continue
 400  continue
 7000 continue
      return
c 9100 format('SusOp',
c     &     '   RHS_N_Ix   N_R/N_IX      DlEta    AvDlEta',
c     &     '  MxNxDlEta  EtaSrchCo')
c 9110 format(i5, 6(2x,i9), ': Stop Sus TRAN (', i1, ')')
c 9120 format(i5, 6(2x,i9))
      end
      subroutine ems_srch_ls_sus_fwd_tran(
     &     sus_f_eta_n, l_eta_n, n_eta_el,
     &     eta_v, eta_ix, eta_sa, eta_pv_in_r,
     &     rhs_v, rhs_ix,
     &     fm_eta_n)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
CM      IF (emsol_da .EQ. 1) THEN
C?      include 'EMSDA.INC'
C?      include 'TRANDA.INC'
C?c      include 'APPETALS.INC'
CM      ENDIF
      include 'SUSFTRAN.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'TRANTT.INC'
CM      ENDIF
      include 'RLCTVR.INC'
      include 'ICTVR.INC'
 
      integer sus_f_eta_n, l_eta_n, n_eta_el
      integer eta_ix(0:n_eta_el), eta_sa(0:l_eta_n+1)
      integer eta_pv_in_r(0:n_r), rhs_ix(0:n_r)
      double precision eta_v(0:n_eta_el), rhs_v(0:n_r)
      integer fm_eta_n
      integer dl_eta_n
      integer av_dl_eta_n
      integer mx_nx_dl_eta_n
      integer tru_eta_srch_co
CM      IF (emsol_da .EQ. 1) THEN
C?      integer alt_av_dl_eta_n
CM      ENDIF
      integer dl_n_ix
      integer prev_dl_n_ix
      integer av_dl_n_ix
      integer cdd_eta_n
      integer eta_n, el_n, r_n, pv_r, rhs_ix_n
CM      IF (emsol_da .EQ. 1) THEN
C?c      integer app_eta_ls_n_en
CM      ENDIF
      integer rpt_eta_ls_l_en
      integer rpt_eta_ls_en_n
 
      eta_n = sus_f_eta_n-1
 250  continue
c
c     Get the next eta to apply
c
CM      IF (emsol_tt .EQ. 1) THEN
C?c      call ems_tt_rec(sus_fwd_tran_eta_srch_tt, 0)
CM      ENDIF
      dl_eta_n = eta_n
      fm_eta_n = l_eta_n+1
      rhs_ix_n = sus_eta_ls_f_en
      rpt_eta_ls_l_en = rpt_eta_ls_f_en-1
 265  continue
      if (rhs_ix_n .le. sus_eta_ls_l_en) then
         cdd_eta_n = rhs_ix(rhs_ix_n)
c     if (cdd_eta_n .le. eta_n) then
c     print*, 'Earlier eta on list '
c     print*, 'rhs_ix_n =  ', rhs_ix_n
c     print*, 'cdd_eta_n = ', cdd_eta_n
c     print*, 'sus_f_eta_n =   ', sus_f_eta_n
c     print*, 'eta_n =     ', eta_n
c     print*, 'fm_eta_n =  ', fm_eta_n
c     print*, 'l_eta_n =   ', l_eta_n
c     print*, 'Indices '
c     do ix_n = 1, rhs_ix(0)
c     print*, ix_n, rhs_ix(ix_n)
c     enddo
c     print*, 'Etas '
c     do ix_n = sus_eta_ls_f_en, sus_eta_ls_l_en
c     print*, ix_n, rhs_ix(ix_n)
c     enddo
c     stop
c     endif
         if (cdd_eta_n .lt. fm_eta_n) then
c
c     Update the record of the next eta to be applied and initialise
c     the list of eta list entries which point to this eta.
c
            fm_eta_n = cdd_eta_n
            rpt_eta_ls_l_en = rpt_eta_ls_f_en
            rhs_ix(rpt_eta_ls_l_en) = rhs_ix_n
         else if (cdd_eta_n .eq. fm_eta_n) then
c
c     Update the set of eta list entries which point to the same eta
c     so that all can be removed from the list.
c
            rpt_eta_ls_l_en = rpt_eta_ls_l_en + 1
            rhs_ix(rpt_eta_ls_l_en) = rhs_ix_n
         endif
         rhs_ix_n = rhs_ix_n + 1
         goto 265
      endif
      do 267, rpt_eta_ls_en_n =
     &     rpt_eta_ls_l_en, rpt_eta_ls_f_en, -1
         rhs_ix_n = rhs_ix(rpt_eta_ls_en_n)
         rhs_ix(rhs_ix_n) = rhs_ix(sus_eta_ls_l_en)
         sus_eta_ls_l_en = sus_eta_ls_l_en - 1
 267  continue
CM      IF (emsol_tt .EQ. 1) THEN
C?c      call ems_tt_rec(-sus_fwd_tran_eta_srch_tt, 0)
CM      ENDIF
c
c     If none needs to be applied then return
c
      if (fm_eta_n .gt. l_eta_n) goto 400
c
c     If no more super-sparse operations should be applied then continue
c     with sparse forward TRAN
c
      if (sus_fwd_tran_n_op .ge. sus_fwd_tran_mx_n_op) goto 300
      sus_fwd_tran_n_op = sus_fwd_tran_n_op + 1
      dl_eta_n = fm_eta_n - dl_eta_n + 1
      eta_n = fm_eta_n
c
c=======================================================================
c     Start performing super-sparse forward TRAN operation
c
      el_n = eta_sa(eta_n)
      pv_r = eta_ix(el_n)
      dl_n_ix = rhs_ix(0)
CM      IF (emsol_da .EQ. 1) THEN
C?      if (rhs_v(pv_r) .ne. zero) then
C?         su_n_fwd_tran_en = su_n_fwd_tran_en + 1
C?         if (abs(rhs_v(pv_r)) .le. fwd_tran_ze)
C?     &        su_n_fwd_tran_ze = su_n_fwd_tran_ze + 1
C?      endif
CM      ENDIF
      if (rhs_v(pv_r) .eq. zero .or.
     &     abs(rhs_v(pv_r)) .le. fwd_tran_ze) then
         el_n = eta_sa(eta_n+1)
CM      IF (emsol_da .EQ. 1) THEN
C?         tran_op_da(tran_da_rec_n_fwd_sus_eta_skp_en) =
C?     &        tran_op_da(tran_da_rec_n_fwd_sus_eta_skp_en) + 1
CM      ENDIF
      else
CM      IF (emsol_da .EQ. 1) THEN
C?c            app_eta_ls_n_en = app_eta_ls(0) + 1
C?c            if (app_eta_ls_n_en .le. app_eta_ls_mx_n_en) then
C?c               app_eta_ls(app_eta_ls_n_en) = eta_n
C?c               app_eta_ls(0) = app_eta_ls_n_en
C?c            endif
CM      ENDIF
         rhs_v(pv_r) = rhs_v(pv_r)*eta_v(el_n)
         do 295, el_n = el_n+1, eta_sa(eta_n+1)-1
            r_n = eta_ix(el_n)
            if (rhs_v(r_n) .eq. zero) then
               rhs_v(r_n) = rhs_v(pv_r)*eta_v(el_n)
               rhs_ix(0) = rhs_ix(0) + 1
               if (rhs_ix(0) .lt. fwd_tran_dse_rhs_n_r) then
                  rhs_ix(rhs_ix(0)) = r_n
                  cdd_eta_n = eta_pv_in_r(r_n)
                  if (cdd_eta_n .le. l_eta_n) then
                     sus_eta_ls_l_en = sus_eta_ls_l_en + 1
                     rhs_ix(sus_eta_ls_l_en) = eta_pv_in_r(r_n)
                  endif
               endif
            else
               rhs_v(r_n) = rhs_v(r_n) + rhs_v(pv_r)*eta_v(el_n)
            end if
 295     continue
      end if
c
c     Finished performing super-sparse forward TRAN operation
c=======================================================================
c
      if (rhs_ix(0) .ge. fwd_tran_dse_rhs_n_r) then
         rhs_ix(0) = n_r + 1
         fm_eta_n = eta_n+1
         go to 7000
      end if
      dl_n_ix = rhs_ix(0) - dl_n_ix
c
c     Calculate a weighted average of the past two eta skips and
c     compare it with the cost of the search.
c
      if (sus_fwd_tran_n_op .eq. 1) then
CM      IF (emsol_da .EQ. 1) THEN
C?         if (rp_sus_eta_op .gt. 0) write(*, 9100)
CM      ENDIF
         prev_dl_eta_n = dl_eta_n
         prev_dl_n_ix = dl_n_ix
      endif
      av_dl_eta_n = (2*dl_eta_n+prev_dl_eta_n)/3
      av_dl_n_ix = (2*dl_n_ix+prev_dl_n_ix)/3
c
c     The cost of the search for the next eta to apply is proportional
c     to the number of indices in the list and the average fill-in.
c
      tru_eta_srch_co =
     &     (sus_eta_ls_l_en - sus_eta_ls_f_en + 1) + av_dl_n_ix
      eta_srch_co = sus_eta_srch_co_mu*tru_eta_srch_co
      mx_nx_dl_eta_n = l_eta_n-eta_n
CM      IF (emsol_da .EQ. 1) THEN
C?      if (rp_sus_eta_op .gt. 0) then
C?         alt_av_dl_eta_n = n_r/rhs_ix(0)
C?         if (tru_eta_srch_co .gt. av_dl_eta_n) then
C?            write(*, 9110)sus_fwd_tran_n_op,
C?     &           rhs_ix(0), alt_av_dl_eta_n, dl_eta_n,
C?     &           av_dl_eta_n, mx_nx_dl_eta_n, tru_eta_srch_co, 1
C?         else if (tru_eta_srch_co .gt. mx_nx_dl_eta_n) then
C?            write(*, 9110)sus_fwd_tran_n_op,
C?     &           rhs_ix(0), alt_av_dl_eta_n, dl_eta_n,
C?     &           av_dl_eta_n, mx_nx_dl_eta_n, tru_eta_srch_co, 2
C?         else
C?            write(*, 9120)sus_fwd_tran_n_op,
C?     &           rhs_ix(0), alt_av_dl_eta_n, dl_eta_n,
C?     &           av_dl_eta_n, mx_nx_dl_eta_n, tru_eta_srch_co
C?         endif
C?      endif
CM      ENDIF
      if (sus_fwd_tran_n_op .gt. 1 .and.
     &     (eta_srch_co .gt. av_dl_eta_n .or.
     &     eta_srch_co .gt. mx_nx_dl_eta_n)) then
c
c     The cost of super-sparse operations is uncompetitive: stop any
c     further super-sparse operations by setting
c     sus_fwd_tran_mx_n_op = -1
c
         fm_eta_n = eta_n+1
         sus_fwd_tran_mx_n_op = -1
         go to 7000
      endif
      prev_dl_eta_n = dl_eta_n
      prev_dl_n_ix = dl_n_ix
      goto 250
 300  continue
 400  continue
 7000 continue
      return
c 9100 format('SusOp',
c     &     '   RHS_N_Ix   N_R/N_IX      DlEta    AvDlEta',
c     &     '  MxNxDlEta  EtaSrchCo')
c 9110 format(i5, 6(2x,i9), ': Stop Sus TRAN (', i1, ')')
c 9120 format(i5, 6(2x,i9))
      end
      subroutine ems_srch_buk_sus_fwd_tran(
     &     sus_f_eta_n, l_eta_n, n_eta_el,
     &     eta_v, eta_ix, eta_sa, eta_pv_in_r,
     &     rhs_v, rhs_ix,
     &     fm_eta_n)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
CM      IF (emsol_da .EQ. 1) THEN
C?      include 'EMSDA.INC'
C?      include 'TRANDA.INC'
C?c      include 'APPETALS.INC'
CM      ENDIF
      include 'SUSFTRAN.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'TRANTT.INC'
CM      ENDIF
      include 'RLCTVR.INC'
      include 'ICTVR.INC'
 
      integer sus_eta_n_buk
      integer sus_eta_buk_os_f_en
      integer sus_eta_buk_os_l_en
      parameter (
     &     sus_eta_n_buk = 5,
     &     sus_eta_buk_os_f_en = 0,
     &     sus_eta_buk_os_l_en = 1)
 
      integer sus_f_eta_n, l_eta_n, n_eta_el
      integer eta_ix(0:n_eta_el), eta_sa(0:l_eta_n+1)
      integer eta_pv_in_r(0:n_r), rhs_ix(0:n_r)
      double precision eta_v(0:n_eta_el), rhs_v(0:n_r)
      integer fm_eta_n
      integer dl_eta_n
      integer av_dl_eta_n
      integer mx_nx_dl_eta_n
      integer tru_eta_srch_co
CM      IF (emsol_da .EQ. 1) THEN
C?      integer alt_av_dl_eta_n
CM      ENDIF
      integer dl_n_ix
      integer prev_dl_n_ix
      integer av_dl_n_ix
      integer cdd_eta_n
      integer eta_n, el_n, r_n, pv_r, rhs_ix_n
      logical sus_fwd_tran
CM      IF (emsol_da .EQ. 1) THEN
C?c      integer app_eta_ls_n_en
CM      ENDIF
      integer rpt_eta_ls_l_en
      integer rpt_eta_ls_en_n
      integer fm_sus_eta_buk_n
      integer f_fu_buk_n
      integer sus_eta_p_f_buk
      integer sus_eta_buk_dl_eta_n
      integer sus_eta_buk_n
      integer sus_eta_p_buk
      integer sus_eta_p_nx_buk
      integer sus_eta_buk_rec_z
      integer rp_sus_buk
      rp_sus_buk = 0
 
      f_fu_buk_n = sus_eta_n_buk+1
      sus_eta_p_f_buk = n_r/3+1
      sus_eta_buk_dl_eta_n = max(
     &     (l_eta_n-sus_f_eta_n+sus_eta_n_buk)/sus_eta_n_buk, 1)
      sus_eta_buk_rec_z =
     &     (n_r-sus_eta_p_f_buk+1)/(sus_eta_n_buk+1)
      if (sus_eta_buk_rec_z .le. 3) then
         fm_eta_n = sus_f_eta_n
         sus_fwd_tran_mx_n_op = -1
         go to 7000
      end if
      rpt_eta_ls_f_en =
     &     sus_eta_p_f_buk + sus_eta_n_buk*sus_eta_buk_rec_z
      sus_eta_p_buk = sus_eta_p_f_buk
      do 220, sus_eta_buk_n = 1, sus_eta_n_buk
         rhs_ix(sus_eta_p_buk+sus_eta_buk_os_f_en) =
     &        sus_eta_p_buk + sus_eta_buk_os_l_en + 1
         rhs_ix(sus_eta_p_buk+sus_eta_buk_os_l_en) =
     &        rhs_ix(sus_eta_p_buk+sus_eta_buk_os_f_en) - 1
         sus_eta_p_buk = sus_eta_p_buk + sus_eta_buk_rec_z
 220  continue
      do 222, rhs_ix_n = 1, rhs_ix(0)
         r_n = rhs_ix(rhs_ix_n)
         cdd_eta_n = eta_pv_in_r(r_n)
         if (cdd_eta_n .ge. sus_f_eta_n .and.
     &        cdd_eta_n .le. l_eta_n) then
            sus_eta_buk_n =
     &           (cdd_eta_n-sus_f_eta_n+sus_eta_buk_dl_eta_n)/
     &           sus_eta_buk_dl_eta_n
            sus_eta_p_buk = sus_eta_p_f_buk +
     &           (sus_eta_buk_n-1)*sus_eta_buk_rec_z
            sus_eta_p_nx_buk = sus_eta_p_buk + sus_eta_buk_rec_z
            sus_eta_ls_l_en =
     &           rhs_ix(sus_eta_p_buk+sus_eta_buk_os_l_en) + 1
            if (sus_eta_ls_l_en .ge. sus_eta_p_nx_buk) then
               fm_eta_n = sus_f_eta_n
               sus_fwd_tran_mx_n_op = -1
               go to 7000
            end if
            rhs_ix(sus_eta_ls_l_en) = cdd_eta_n
            rhs_ix(sus_eta_p_buk+sus_eta_buk_os_l_en) =
     &           sus_eta_ls_l_en
         endif
CM      IF (emsol_da .EQ. 1) THEN
C?         if (rp_sus_buk .ne. 0) call ems_rp_sus_eta_buk(
C?     &        n_r, sus_f_eta_n, l_eta_n, rhs_ix)
CM      ENDIF
 222  continue
      fm_sus_eta_buk_n = 1
 
 
      eta_n = sus_f_eta_n-1
 250  continue
c
c     Get the next eta to apply
c
CM      IF (emsol_tt .EQ. 1) THEN
C?c      call ems_tt_rec(sus_fwd_tran_eta_srch_tt, 0)
CM      ENDIF
CM      IF (emsol_da .EQ. 1) THEN
C?      if (rp_sus_buk .ne. 0) call ems_rp_sus_eta_buk(
C?     &     n_r, sus_f_eta_n, l_eta_n, rhs_ix)
CM      ENDIF
      sus_eta_buk_n = fm_sus_eta_buk_n
      sus_eta_p_buk = sus_eta_p_f_buk +
     &     (sus_eta_buk_n-1)*sus_eta_buk_rec_z
      dl_eta_n = eta_n
      fm_eta_n = l_eta_n+1
      fm_sus_eta_buk_n = sus_eta_n_buk+1
      rpt_eta_ls_l_en = rpt_eta_ls_f_en-1
 270  continue
      if (sus_eta_buk_n .eq. f_fu_buk_n) then
c
c     This bucket is full: start standard sparse operations from the
c     first eta in its sequence.
c
c     Have to stop supersparse forward TRAN before the maximum number of
c     supersparse operations has been reached. Achieve this by setting
c     sus_fwd_tran_mx_n_op as if no supersparse operations are allowed
c     at all.
c
         fm_eta_n = sus_f_eta_n + (sus_eta_buk_n-1)*sus_eta_buk_dl_eta_n
         sus_fwd_tran_mx_n_op = -1
CM      IF (emsol_tt .EQ. 1) THEN
C?c         call ems_tt_rec(-sus_fwd_tran_eta_srch_tt, 0)
CM      ENDIF
         goto 7000
      endif
      sus_eta_ls_f_en = rhs_ix(sus_eta_p_buk+sus_eta_buk_os_f_en)
      sus_eta_ls_l_en = rhs_ix(sus_eta_p_buk+sus_eta_buk_os_l_en)
      do 272, rhs_ix_n = sus_eta_ls_f_en, sus_eta_ls_l_en
         cdd_eta_n = rhs_ix(rhs_ix_n)
c     if (cdd_eta_n .le. eta_n) then
c     print*, 'Earlier eta on list '
c     print*, 'rhs_ix_n =  ', rhs_ix_n
c     print*, 'cdd_eta_n = ', cdd_eta_n
c     print*, 'sus_f_eta_n =   ', sus_f_eta_n
c     print*, 'eta_n =     ', eta_n
c     print*, 'fm_eta_n =  ', fm_eta_n
c     print*, 'l_eta_n =   ', l_eta_n
c     call ems_rp_sus_eta_buk(
c     &              n_r, sus_f_eta_n, l_eta_n, rhs_ix)
c     stop
c     endif
         if (cdd_eta_n .lt. fm_eta_n) then
c
c     Update the record of the next eta to be applied and initialise the
c     list of eta list entries which point to this eta.
c
            fm_eta_n = cdd_eta_n
            fm_sus_eta_buk_n = sus_eta_buk_n
            rpt_eta_ls_l_en = rpt_eta_ls_f_en
            rhs_ix(rpt_eta_ls_l_en) = rhs_ix_n
         else if (cdd_eta_n .eq. fm_eta_n) then
c
c     Update the set of eta list entries which point to the same eta so
c     that all can be removed from the list.
c
            rpt_eta_ls_l_en = rpt_eta_ls_l_en + 1
            rhs_ix(rpt_eta_ls_l_en) = rhs_ix_n
         endif
 272  continue
      if (fm_eta_n .gt. l_eta_n) then
         sus_eta_buk_n = sus_eta_buk_n + 1
         sus_eta_p_buk =  sus_eta_p_buk + sus_eta_buk_rec_z
         if (sus_eta_buk_n .le. sus_eta_n_buk) goto 270
      endif
      do 274, rpt_eta_ls_en_n =
     &     rpt_eta_ls_l_en, rpt_eta_ls_f_en, -1
         rhs_ix_n = rhs_ix(rpt_eta_ls_en_n)
         rhs_ix(rhs_ix_n) = rhs_ix(sus_eta_ls_l_en)
         sus_eta_ls_l_en = sus_eta_ls_l_en - 1
 274  continue
      rhs_ix(sus_eta_p_buk+sus_eta_buk_os_l_en) = sus_eta_ls_l_en
CM      IF (emsol_tt .EQ. 1) THEN
C?c      call ems_tt_rec(-sus_fwd_tran_eta_srch_tt, 0)
CM      ENDIF
c
c     If none needs to be applied then return
c
      if (fm_eta_n .gt. l_eta_n) goto 400
c
c     If no more super-sparse operations should be applied then continue
c     with sparse forward TRAN
c
      if (sus_fwd_tran_n_op .ge. sus_fwd_tran_mx_n_op) goto 300
      sus_fwd_tran_n_op = sus_fwd_tran_n_op + 1
      dl_eta_n = fm_eta_n - dl_eta_n + 1
      eta_n = fm_eta_n
c
c=======================================================================
c     Start performing super-sparse forward TRAN operation
c
      sus_fwd_tran = .true.
      el_n = eta_sa(eta_n)
      pv_r = eta_ix(el_n)
      dl_n_ix = rhs_ix(0)
CM      IF (emsol_da .EQ. 1) THEN
C?      if (rhs_v(pv_r) .ne. zero) then
C?         su_n_fwd_tran_en = su_n_fwd_tran_en + 1
C?         if (abs(rhs_v(pv_r)) .le. fwd_tran_ze)
C?     &        su_n_fwd_tran_ze = su_n_fwd_tran_ze + 1
C?      endif
CM      ENDIF
      if (rhs_v(pv_r) .eq. zero .or.
     &     abs(rhs_v(pv_r)) .le. fwd_tran_ze) then
         el_n = eta_sa(eta_n+1)
CM      IF (emsol_da .EQ. 1) THEN
C?         tran_op_da(tran_da_rec_n_fwd_sus_eta_skp_en) =
C?     &        tran_op_da(tran_da_rec_n_fwd_sus_eta_skp_en) + 1
CM      ENDIF
      else
CM      IF (emsol_da .EQ. 1) THEN
C?c            app_eta_ls_n_en = app_eta_ls(0) + 1
C?c            if (app_eta_ls_n_en .le. app_eta_ls_mx_n_en) then
C?c               app_eta_ls(app_eta_ls_n_en) = eta_n
C?c               app_eta_ls(0) = app_eta_ls_n_en
C?c            endif
CM      ENDIF
         rhs_v(pv_r) = rhs_v(pv_r)*eta_v(el_n)
         do 295, el_n = el_n+1, eta_sa(eta_n+1)-1
            r_n = eta_ix(el_n)
            if (rhs_v(r_n) .eq. zero) then
               rhs_v(r_n) = rhs_v(pv_r)*eta_v(el_n)
               rhs_ix(0) = rhs_ix(0) + 1
               if (rhs_ix(0) .lt. fwd_tran_dse_rhs_n_r) then
                  rhs_ix(rhs_ix(0)) = r_n
                  cdd_eta_n = eta_pv_in_r(r_n)
                  if (cdd_eta_n .le. l_eta_n) then
                     sus_eta_buk_n =
     &                    (cdd_eta_n-sus_f_eta_n+
     &                    sus_eta_buk_dl_eta_n)/
     &                    sus_eta_buk_dl_eta_n
                     sus_eta_p_buk = sus_eta_p_f_buk +
     &                    (sus_eta_buk_n-1)*sus_eta_buk_rec_z
                     sus_eta_p_nx_buk =
     &                    sus_eta_p_buk + sus_eta_buk_rec_z
                     sus_eta_ls_l_en =
     &                    rhs_ix(sus_eta_p_buk+
     &                    sus_eta_buk_os_l_en) + 1
                     if (sus_eta_ls_l_en .lt.
     &                    sus_eta_p_nx_buk) then
                        rhs_ix(sus_eta_ls_l_en) = cdd_eta_n
                        rhs_ix(sus_eta_p_buk+sus_eta_buk_os_l_en) =
     &                       sus_eta_ls_l_en
                     else
c
c     This bucket is full. Maintain the number of the first full bucket.
c     If the full bucket is that of the current eta, have to stop doing
c     super-sparse TRAN and start sparse TRAN with the next eta to be
c     applied.
c     Otherwise, once a full bucket is encountered in the supersparse
c     search, have to stop doing super-sparse TRAN and start sparse TRAN
c     with the first eta in the sequence for the full bucket.
c
                        f_fu_buk_n = min(sus_eta_buk_n, f_fu_buk_n)
                        if (sus_eta_buk_n .eq. fm_sus_eta_buk_n) then
                           sus_fwd_tran = .false.
                        endif
                     end if
                  endif
               endif
            else
               rhs_v(r_n) = rhs_v(r_n) + rhs_v(pv_r)*eta_v(el_n)
            end if
 295     continue
      end if
c
c     Finished performing super-sparse forward TRAN operation
c=======================================================================
c
      if (rhs_ix(0) .ge. fwd_tran_dse_rhs_n_r) then
         rhs_ix(0) = n_r + 1
         fm_eta_n = eta_n+1
         go to 7000
      end if
      if (.not.sus_fwd_tran) then
         fm_eta_n = eta_n+1
         sus_fwd_tran_mx_n_op = -1
         go to 7000
      endif
      dl_n_ix = rhs_ix(0) - dl_n_ix
c
c     Calculate a weighted average of the past two eta skips and
c     compare it with the cost of the search.
c
      if (sus_fwd_tran_n_op .eq. 1) then
CM      IF (emsol_da .EQ. 1) THEN
C?         if (rp_sus_eta_op .gt. 0) write(*, 9100)
CM      ENDIF
         prev_dl_eta_n = dl_eta_n
         prev_dl_n_ix = dl_n_ix
      endif
      av_dl_eta_n = (2*dl_eta_n+prev_dl_eta_n)/3
      av_dl_n_ix = (2*dl_n_ix+prev_dl_n_ix)/3
c
c     The cost of the search for the next eta to apply is proportional
c     to the number of indices in the list and the average fill-in.
c
      tru_eta_srch_co =
     &     (sus_eta_ls_l_en - sus_eta_ls_f_en + 1) + av_dl_n_ix
      eta_srch_co = sus_eta_srch_co_mu*tru_eta_srch_co
      mx_nx_dl_eta_n = l_eta_n-eta_n
CM      IF (emsol_da .EQ. 1) THEN
C?      if (rp_sus_eta_op .gt. 0) then
C?         alt_av_dl_eta_n = n_r/rhs_ix(0)
C?         if (tru_eta_srch_co .gt. av_dl_eta_n) then
C?            write(*, 9110)sus_fwd_tran_n_op,
C?     &           rhs_ix(0), alt_av_dl_eta_n, dl_eta_n,
C?     &           av_dl_eta_n, mx_nx_dl_eta_n, tru_eta_srch_co, 1
C?         else if (tru_eta_srch_co .gt. mx_nx_dl_eta_n) then
C?            write(*, 9110)sus_fwd_tran_n_op,
C?     &           rhs_ix(0), alt_av_dl_eta_n, dl_eta_n,
C?     &           av_dl_eta_n, mx_nx_dl_eta_n, tru_eta_srch_co, 2
C?         else
C?            write(*, 9120)sus_fwd_tran_n_op,
C?     &           rhs_ix(0), alt_av_dl_eta_n, dl_eta_n,
C?     &           av_dl_eta_n, mx_nx_dl_eta_n, tru_eta_srch_co
C?         endif
C?      endif
CM      ENDIF
      if (sus_fwd_tran_n_op .gt. 1 .and.
     &     (eta_srch_co .gt. av_dl_eta_n .or.
     &     eta_srch_co .gt. mx_nx_dl_eta_n)) then
c
c     The cost of super-sparse operations is uncompetitive: stop any
c     further super-sparse operations by setting
c     sus_fwd_tran_mx_n_op = -1
c
         fm_eta_n = eta_n+1
         sus_fwd_tran_mx_n_op = -1
         go to 7000
      endif
      prev_dl_eta_n = dl_eta_n
      prev_dl_n_ix = dl_n_ix
      goto 250
 300  continue
 400  continue
 7000 continue
      return
c 9100 format('SusOp',
c     &     '   RHS_N_Ix   N_R/N_IX      DlEta    AvDlEta',
c     &     '  MxNxDlEta  EtaSrchCo')
c 9110 format(i5, 6(2x,i9), ': Stop Sus TRAN (', i1, ')')
c 9120 format(i5, 6(2x,i9))
      end
CM      IF (emsol_da .EQ. 1) THEN
C?      subroutine ems_rp_sus_eta_buk(n_r, f_eta_n, l_eta_n, rhs_ix)
C?      implicit none
C?      integer n_r, f_eta_n, l_eta_n, rhs_ix(0:n_r)
C?      integer fm_eta_n, t_eta_n, rhs_ix_n
C?      integer sus_eta_p_f_buk
C?      integer sus_eta_buk_dl_eta_n
C?      integer sus_eta_buk_n
C?      integer sus_eta_p_buk
C?      integer sus_eta_ls_f_en
C?      integer sus_eta_ls_l_en
C?      integer sus_eta_buk_rec_z
C?
C?      integer sus_eta_n_buk
C?      integer sus_eta_buk_os_f_en
C?      integer sus_eta_buk_os_l_en
C?      parameter (
C?     &     sus_eta_n_buk = 10,
C?     &     sus_eta_buk_os_f_en = 0,
C?     &     sus_eta_buk_os_l_en = 1)
C?
C?      sus_eta_p_f_buk = n_r/3+1
C?      sus_eta_buk_dl_eta_n = max(
C?     &     (l_eta_n-f_eta_n+sus_eta_n_buk)/sus_eta_n_buk, 1)
C?      sus_eta_buk_rec_z =
C?     &     (n_r-sus_eta_p_f_buk+1)/(sus_eta_n_buk+1)
C?      write(*, 9000)f_eta_n, l_eta_n
C?      write(*, 9010)'Pointer to first bucket = ', sus_eta_p_f_buk
C?      write(*, 9010)'Bucket eta interval =     ', sus_eta_buk_dl_eta_n
C?      write(*, 9010)'Bucket record size =      ', sus_eta_buk_rec_z
C?
C?      sus_eta_p_buk = sus_eta_p_f_buk
C?      do 10, sus_eta_buk_n = 1, sus_eta_n_buk
C?         fm_eta_n = f_eta_n + (sus_eta_buk_n-1)*sus_eta_buk_dl_eta_n
C?         t_eta_n = fm_eta_n + sus_eta_buk_dl_eta_n - 1
C?         sus_eta_ls_f_en = rhs_ix(sus_eta_p_buk+sus_eta_buk_os_f_en)
C?         sus_eta_ls_l_en = rhs_ix(sus_eta_p_buk+sus_eta_buk_os_l_en)
C?         if (sus_eta_ls_f_en .gt. sus_eta_ls_l_en) then
C?            write(*, 9101)sus_eta_buk_n, fm_eta_n, t_eta_n
C?         else
C?            write(*, 9102)sus_eta_buk_n, fm_eta_n, t_eta_n,
C?     &           sus_eta_ls_l_en-sus_eta_ls_f_en+1,
C?     &           (rhs_ix(rhs_ix_n),
C?     &           rhs_ix_n = sus_eta_ls_f_en, sus_eta_ls_l_en)
C?         endif
C?         sus_eta_p_buk = sus_eta_p_buk + sus_eta_buk_rec_z
C? 10   continue
C?      return
C? 9000 format(/'Reporting on eta buckets: '/'   Etas ', i7, ' to ', i7)
C? 9010 format(a, 1x, i7)
C? 9100 format('   Bucket ', i2, ': Etas ', i7, ' to ', i7)
C? 9101 format('   Bucket ', i2, ': Etas ', i7, ' to ', i7, ':    EMPTY')
C? 9102 format('   Bucket ', i2, ': Etas ', i7, ' to ', i7, ':',
C?     &     i3, ' Entries: ', (100(1x, i5)))
C?      end
CM      ENDIF
 
C->>> -----------------------------------------> ems_bwd_tran_eta_se <<<
c     Backward application of f_eta_n, ..., l_eta_n from a set of etas.
c     maintaining the indices of nonzeros if rhs_ix(0) is suff small
c
      subroutine ems_bwd_tran_eta_se(
     &     f_eta_n, l_eta_n, n_eta_el,
     &     eta_v, eta_ix, eta_sa,
     &     eta_w_l_en_in_r, eta_w_lm1_en_in_r,
     &     rhs_v, rhs_ix)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
CM      IF (emsol_da .EQ. 1) THEN
C?      include 'EMSDA.INC'
C?      include 'TRANDA.INC'
CM      ENDIF
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      include 'RLCTVR.INC'
      integer f_eta_n, l_eta_n, n_eta_el
      integer eta_ix(0:n_eta_el), eta_sa(0:l_eta_n+1)
      integer rhs_ix(0:n_r)
      double precision eta_v(0:n_eta_el), rhs_v(0:n_r)
      integer eta_w_l_en_in_r(0:n_r)
      integer eta_w_lm1_en_in_r(0:n_r)
      integer fm_eta_n, eta_n, el_n, pv_r
      double precision su
      integer rhs_ix_n, r_n
      logical tran_sps_rhs
      integer sus_bwd_tran_n_op
      integer sus_bwd_tran_op_n
CM      IF (emsol_da .EQ. 1) THEN
C?      logical no_op
CM      ENDIF
 
      fm_eta_n = l_eta_n
      tran_sps_rhs = rhs_ix(0) .lt. bwd_tran_dse_rhs_n_r
      el_n = eta_sa(fm_eta_n+1)
      su = zero
 100  continue
      if (.not.tran_sps_rhs) then
c
c     Don't maintain the indices of nonzeros in the RHS
c
         do 130, eta_n = fm_eta_n, f_eta_n, -1
c
c=======================================================================
c     Start performing backward TRAN operation
c
CM      IF (emsol_da .EQ. 1) THEN
C?            no_op = .true.
CM      ENDIF
            do 120, el_n = el_n-1, eta_sa(eta_n)+1, -1
CM      IF (emsol_da .EQ. 1) THEN
C?               if (rhs_v(eta_ix(el_n)) .ne. zero) no_op = .false.
CM      ENDIF
               su = su + rhs_v(eta_ix(el_n))*eta_v(el_n)
 120        continue
CM      IF (emsol_da .EQ. 1) THEN
C?            if (rhs_v(eta_ix(el_n)) .ne. zero) no_op = .false.
C?            if (no_op) tran_op_da(tran_da_rec_n_bwd_std_eta_no_op) =
C?     &           tran_op_da(tran_da_rec_n_bwd_std_eta_no_op) + 1
CM      ENDIF
            pv_r = eta_ix(el_n)
            rhs_v(pv_r) = (rhs_v(pv_r)+su)*eta_v(el_n)
            su = zero
c
c     Finished performing backward TRAN operation
c=======================================================================
c
 130     continue
CM      IF (emsol_da .EQ. 1) THEN
C?         tran_op_da(tran_da_rec_n_bwd_std_eta) =
C?     &        fm_eta_n - f_eta_n + 1
CM      ENDIF
c
c     Indicate that the nonzeros are not known, being careful in case
c     rhs_ix(0) was passed as n_r+1
c
         if (rhs_ix(0) .lt. n_r) rhs_ix(0) = n_r+1
      else
c
c     Maintain the indices of nonzeros in the RHS
c
         sus_bwd_tran_n_op = eta_w_l_en_in_r(0)
         if (sus_bwd_tran_n_op .lt. 0) goto 300
         sus_bwd_tran_op_n = eta_w_lm1_en_in_r(0)
         if (sus_bwd_tran_op_n .ge. 2) then
            goto 300
         else if (sus_bwd_tran_op_n .ge. 1) then
c
c     Make it look as if the most recent eta to be applied was the first
c     after the current set of eta numbers. In general it will be
c     greater then this but it is known that etas l_eta_n+1...tru_eta_n
c     need not be applied.
c
            eta_n = l_eta_n+1
            goto 250
         endif
c 200     continue
c
c     Get the first eta to apply by finding the last eta with an entry
c     in a row for which the RHS has a nonzero.
c
         fm_eta_n = f_eta_n-1
         do 210, rhs_ix_n = 1, rhs_ix(0)
            r_n = rhs_ix(rhs_ix_n)
            fm_eta_n = max(eta_w_l_en_in_r(r_n), fm_eta_n)
 210     continue
c
c     If none needs to be applied then return
c
         if (fm_eta_n .lt. f_eta_n) then
CM      IF (emsol_da .EQ. 1) THEN
C?            tran_op_da(tran_da_rec_n_bwd_sus_eta) =
C?     &           l_eta_n - f_eta_n + 1
CM      ENDIF
            goto 400
         endif
c
c     If no more super-sparse operations should be applied then continue
c     with sparse backward TRAN
c
         if (sus_bwd_tran_op_n .ge. sus_bwd_tran_n_op) then
CM      IF (emsol_da .EQ. 1) THEN
C?            tran_op_da(tran_da_rec_n_bwd_sus_eta) =
C?     &           l_eta_n - (fm_eta_n+1) + 1
CM      ENDIF
            el_n = eta_sa(fm_eta_n+1)
            goto 290
         endif
         sus_bwd_tran_op_n = sus_bwd_tran_op_n + 1
         eta_n = fm_eta_n
c
c=======================================================================
c     Start performing super-sparse backward TRAN operation
c
CM      IF (emsol_da .EQ. 1) THEN
C?            no_op = .true.
CM      ENDIF
         el_n = eta_sa(eta_n+1)
         do 220, el_n = el_n-1, eta_sa(eta_n)+1, -1
CM      IF (emsol_da .EQ. 1) THEN
C?            if (rhs_v(eta_ix(el_n)) .ne. zero) no_op = .false.
CM      ENDIF
            if (rhs_v(eta_ix(el_n)) .ne. zero)
     &           su = su + rhs_v(eta_ix(el_n))*eta_v(el_n)
 220     continue
CM      IF (emsol_da .EQ. 1) THEN
C?         if (rhs_v(eta_ix(el_n)) .ne. zero) no_op = .false.
C?         if (no_op) tran_op_da(tran_da_rec_n_bwd_sus_eta_no_op) =
C?     &        tran_op_da(tran_da_rec_n_bwd_sus_eta_no_op) + 1
CM      ENDIF
         pv_r = eta_ix(el_n)
         if (su .eq. zero) then
            rhs_v(pv_r) = rhs_v(pv_r)*eta_v(el_n)
         else
            if (rhs_v(pv_r) .eq. zero) then
c
c     Fill-in in the RHS
c
               rhs_v(pv_r) = su*eta_v(el_n)
               rhs_ix(0) = rhs_ix(0) + 1
               rhs_ix(rhs_ix(0)) = pv_r
            else
               rhs_v(pv_r) = (rhs_v(pv_r)+su)*eta_v(el_n)
            end if
            su = zero
         end if
CM      IF (emsol_deb .EQ. 1) THEN
C?c            if (wr_bwd_tran_rhs .gt. 0 .and.
C?c     &           (ol_su .ne. zero .or. ol_rhs_pv_r_v .ne. zero))
C?c     &           call ems_wr_tran_rhs(cn, eta_n, rhs_v, rhs_ix)
CM      ENDIF
c
c     Finished performing super-sparse backward TRAN operation
c=======================================================================
c
         if (rhs_ix(0) .ge. bwd_tran_dse_rhs_n_r) then
CM      IF (emsol_da .EQ. 1) THEN
C?            tran_op_da(tran_da_rec_n_bwd_sus_eta) =
C?     &           l_eta_n - fm_eta_n + 1
CM      ENDIF
            tran_sps_rhs = .false.
            rhs_ix(0) = n_r + 1
            fm_eta_n = eta_n-1
            go to 100
         end if
c
c     Indicate that the first backward TRAN operation has been performed
c
         eta_w_lm1_en_in_r(0) = 1
 250     continue
         fm_eta_n = f_eta_n-1
c
c     Determine the second eta to apply by finding the penultimate eta
c     with an entry in a row for which the RHS has a nonzero.
c
         do 260, rhs_ix_n = 1, rhs_ix(0)
            r_n = rhs_ix(rhs_ix_n)
            if (eta_w_l_en_in_r(r_n) .ge. eta_n) then
               fm_eta_n = max(eta_w_lm1_en_in_r(r_n), fm_eta_n)
            else
               fm_eta_n = max(eta_w_l_en_in_r(r_n), fm_eta_n)
            endif
 260     continue
c
c     If none needs to be applied then return
c
         if (fm_eta_n .lt. f_eta_n) then
CM      IF (emsol_da .EQ. 1) THEN
C?            tran_op_da(tran_da_rec_n_bwd_sus_eta) =
C?     &           l_eta_n - f_eta_n + 1
CM      ENDIF
            goto 400
         endif
c
c     If no further supersparse info can be deduced---because fill-in
c     in the pivotal row has occurred `behind' the two first entries
c     which are indexed in by rows---then switch to doing sparse
c     backward TRAN operation from the eta following the last one
c     applied.
c
         if (fm_eta_n .ge. eta_n) fm_eta_n = eta_n-1
         el_n = eta_sa(fm_eta_n+1)
CM      IF (emsol_da .EQ. 1) THEN
C?         tran_op_da(tran_da_rec_n_bwd_sus_eta) =
C?     &        l_eta_n - (fm_eta_n+1) + 1
CM      ENDIF
 290     continue
c
c     Indicate that the second backward TRAN operation (at least) has
c     been performed
c
         eta_w_lm1_en_in_r(0) = 2
 300     continue
         do 330, eta_n = fm_eta_n, f_eta_n, -1
c
c=======================================================================
c     Start performing sparse backward TRAN operation
c
CM      IF (emsol_da .EQ. 1) THEN
C?            no_op = .true.
CM      ENDIF
            do 320, el_n = el_n-1, eta_sa(eta_n)+1, -1
CM      IF (emsol_da .EQ. 1) THEN
C?               if (rhs_v(eta_ix(el_n)) .ne. zero) no_op = .false.
CM      ENDIF
               if (rhs_v(eta_ix(el_n)) .ne. zero)
     &              su = su + rhs_v(eta_ix(el_n))*eta_v(el_n)
 320        continue
CM      IF (emsol_da .EQ. 1) THEN
C?            if (rhs_v(eta_ix(el_n)) .ne. zero) no_op = .false.
C?            if (no_op) tran_op_da(tran_da_rec_n_bwd_sps_eta_no_op) =
C?     &           tran_op_da(tran_da_rec_n_bwd_sps_eta_no_op) + 1
CM      ENDIF
            pv_r = eta_ix(el_n)
            if (su .eq. zero) then
               rhs_v(pv_r) = rhs_v(pv_r)*eta_v(el_n)
            else
               if (rhs_v(pv_r) .eq. zero) then
c
c     Fill-in in the RHS
c
                  rhs_v(pv_r) = su*eta_v(el_n)
                  rhs_ix(0) = rhs_ix(0) + 1
                  rhs_ix(rhs_ix(0)) = pv_r
               else
                  rhs_v(pv_r) = (rhs_v(pv_r)+su)*eta_v(el_n)
               end if
               su = zero
            end if
CM      IF (emsol_deb .EQ. 1) THEN
C?c            if (wr_bwd_tran_rhs .gt. 0 .and.
C?c     &           (ol_su .ne. zero .or. ol_rhs_pv_r_v .ne. zero))
C?c     &           call ems_wr_tran_rhs(cn, eta_n, rhs_v, rhs_ix)
CM      ENDIF
c
c     Finished performing sparse backward TRAN operation
c=======================================================================
c
c     If the RHS becomes dense then stop maintaining the indices of its
c     nonzeros, zero the array used to index into the list of nonzeros
c     and indicate that the indices are to be ignored.
c
            if (rhs_ix(0) .ge. bwd_tran_dse_rhs_n_r) then
CM      IF (emsol_da .EQ. 1) THEN
C?               tran_op_da(tran_da_rec_n_bwd_sps_eta) =
C?     &              fm_eta_n - eta_n + 1
CM      ENDIF
               tran_sps_rhs = .false.
               rhs_ix(0) = n_r + 1
               fm_eta_n = eta_n-1
               go to 100
            end if
 330     continue
CM      IF (emsol_da .EQ. 1) THEN
C?         tran_op_da(tran_da_rec_n_bwd_sps_eta) =
C?     &        fm_eta_n - f_eta_n + 1
CM      ENDIF
 400     continue
      end if
CM      IF (emsol_da .EQ. 1) THEN
C?      tran_op_da(tran_da_rec_n_bwd_eta) =
C?     &     l_eta_n - f_eta_n + 1
CM      ENDIF
      return
      end
 
CM      IF (emsol_da .EQ. 1) THEN
C?      subroutine ems_ck_sus_bwd_tran_da(
C?     &     n_r, n_eta, n_eta_el,
C?     &     eta_ix, eta_sa,
C?     &     eta_w_l_en_in_r, eta_w_lm1_en_in_r,
C?     &     alt_eta_w_l_en_in_r, alt_eta_w_lm1_en_in_r)
C?      implicit none
C?      integer n_r, n_eta, n_eta_el
C?      integer eta_ix(0:n_eta_el)
C?      integer eta_sa(0:n_eta+1)
C?      integer eta_w_l_en_in_r(0:n_r)
C?      integer eta_w_lm1_en_in_r(0:n_r)
C?      integer alt_eta_w_l_en_in_r(0:n_r)
C?      integer alt_eta_w_lm1_en_in_r(0:n_r)
C?      integer r_n, eta_n, el_n
C?
C?      do 10, r_n = 1, n_r
C?         alt_eta_w_l_en_in_r(r_n) = 0
C?         alt_eta_w_lm1_en_in_r(r_n) = 0
C? 10   continue
C?      do 30, eta_n = 1, n_eta
C?         do 20, el_n = eta_sa(eta_n), eta_sa(eta_n+1)-1
C?            r_n = eta_ix(el_n)
C?            alt_eta_w_lm1_en_in_r(r_n) = alt_eta_w_l_en_in_r(r_n)
C?            alt_eta_w_l_en_in_r(r_n) = eta_n
C? 20      continue
C? 30   continue
C?      do 110, r_n = 1, n_r
C?         if (eta_w_l_en_in_r(r_n) .ne. alt_eta_w_l_en_in_r(r_n))
C?     &        write(*, 9000)r_n,
C?     &        eta_w_l_en_in_r(r_n),
C?     &        alt_eta_w_l_en_in_r(r_n)
C?         if (eta_w_lm1_en_in_r(r_n) .ne. alt_eta_w_lm1_en_in_r(r_n))
C?     &        write(*, 9010)r_n,
C?     &        eta_w_lm1_en_in_r(r_n),
C?     &        alt_eta_w_lm1_en_in_r(r_n)
C? 110   continue
C?      return
C? 9000 format('SusBWD_TRAN***ERROR***For row ', i7,
C?     &     ': eta_w_l_en_in_r is ', i7, ' not ', i7)
C? 9010 format('SusBWD_TRAN***ERROR***For row ', i7,
C?     &     ': eta_w_lm1_en_in_r is ', i7, ' not ', i7)
C?      end
C?
C?      subroutine ems_ck_sus_fwd_tran_da(
C?     &     n_r, n_eta, n_eta_el, n_lo_eta,
C?     &     eta_ix, eta_sa,
C?     &     lo_eta_pv_in_r, up_eta_pv_in_r,
C?     &     alt_lo_eta_pv_in_r, alt_up_eta_pv_in_r)
C?      implicit none
C?      integer n_r, n_eta, n_eta_el, n_lo_eta
C?      integer eta_ix(0:n_eta_el)
C?      integer eta_sa(0:n_eta+1)
C?      integer lo_eta_pv_in_r(0:n_r)
C?      integer up_eta_pv_in_r(0:n_r)
C?      integer alt_lo_eta_pv_in_r(0:n_r)
C?      integer alt_up_eta_pv_in_r(0:n_r)
C?      integer r_n, eta_n, el_n
C?
C?      do 10, r_n = 1, n_r
C?         alt_lo_eta_pv_in_r(r_n) = n_eta+1
C?         alt_up_eta_pv_in_r(r_n) = n_eta+1
C? 10   continue
C?      do 20, eta_n = 1, n_lo_eta
C?         el_n = eta_sa(eta_n)
C?         r_n = eta_ix(el_n)
C?         alt_lo_eta_pv_in_r(r_n) = eta_n
C? 20   continue
C?      do 30, eta_n = n_lo_eta+1, n_eta
C?         el_n = eta_sa(eta_n)
C?         r_n = eta_ix(el_n)
C?         alt_up_eta_pv_in_r(r_n) = eta_n
C? 30   continue
C?      do 110, r_n = 1, n_r
C?         if (lo_eta_pv_in_r(r_n) .le. n_eta .and.
C?     &        lo_eta_pv_in_r(r_n) .ne. alt_lo_eta_pv_in_r(r_n))
C?     &        write(*, 9000)r_n,
C?     &        lo_eta_pv_in_r(r_n),
C?     &        alt_lo_eta_pv_in_r(r_n)
C?         if (up_eta_pv_in_r(r_n) .le. n_eta .and.
C?     &        up_eta_pv_in_r(r_n) .ne. alt_up_eta_pv_in_r(r_n))
C?     &        write(*, 9010)r_n,
C?     &        up_eta_pv_in_r(r_n),
C?     &        alt_up_eta_pv_in_r(r_n)
C? 110   continue
C?      return
C? 9000 format('SusFWD_TRAN***ERROR***For row ', i7,
C?     &     ': lo_eta_pv_in_r is ', i7, ' not ', i7)
C? 9010 format('SusFWD_TRAN***ERROR***For row ', i7,
C?     &     ': up_eta_pv_in_r is ', i7, ' not ', i7)
C?      end
CM      ENDIF
 
      subroutine ems_inv_dse_blk_ftran(
     &     n_r, inv_dse_blk_dim,
     &     rhs_v, rhs_ix, inv_dse_blk_rhs_v,
     &     inv_dse_blk_r_se, inv_dse_blk_r_perm,
     &     inv_dse_blk_v)
      implicit none
      integer n_r, inv_dse_blk_dim
      double precision rhs_v(0:n_r)
      integer rhs_ix(0:n_r)
      integer inv_dse_blk_r_se(0:inv_dse_blk_dim)
      integer inv_dse_blk_r_perm(0:inv_dse_blk_dim)
      double precision inv_dse_blk_v(inv_dse_blk_dim, inv_dse_blk_dim)
      double precision inv_dse_blk_rhs_v(0:inv_dse_blk_dim)
      integer r_n, og_r_n, it_n, c_n
      double precision su
c
c     Indicate that the nonzeros are not known, being careful in case
c     rhs_ix(0) was passed as n_r+1
c
      if (rhs_ix(0) .lt. n_r) rhs_ix(0) = n_r+1
      do 10, r_n = 1, inv_dse_blk_dim
         inv_dse_blk_rhs_v(r_n) = rhs_v(inv_dse_blk_r_se(r_n))
 10   continue
      do 30, it_n = 2, inv_dse_blk_dim
         og_r_n = inv_dse_blk_r_perm(it_n)
         su = 0d0
         do 20, c_n = 1, it_n-1
            su = su + inv_dse_blk_v(c_n, og_r_n)*inv_dse_blk_rhs_v(c_n)
 20      continue
         inv_dse_blk_rhs_v(it_n) = inv_dse_blk_rhs_v(it_n) + su
 30   continue
      do 50, it_n = inv_dse_blk_dim, 1, -1
         og_r_n = inv_dse_blk_r_perm(it_n)
         su = 0d0
         do 40, c_n = inv_dse_blk_dim, it_n+1, -1
            su = su + inv_dse_blk_v(c_n, og_r_n)*inv_dse_blk_rhs_v(c_n)
 40      continue
         inv_dse_blk_rhs_v(it_n) =
     &        (inv_dse_blk_rhs_v(it_n) + su)*inv_dse_blk_v(it_n, og_r_n)
         rhs_v(inv_dse_blk_r_se(it_n)) = inv_dse_blk_rhs_v(it_n)
 50   continue
      return
      end
 
      subroutine ems_inv_dse_blk_btran(
     &     n_r, inv_dse_blk_dim,
     &     rhs_v, rhs_ix, inv_dse_blk_rhs_v,
     &     inv_dse_blk_r_se, inv_dse_blk_r_perm,
     &     inv_dse_blk_v)
      implicit none
      integer n_r, inv_dse_blk_dim
      double precision rhs_v(0:n_r)
      integer rhs_ix(0:n_r)
      integer inv_dse_blk_r_se(0:inv_dse_blk_dim)
      integer inv_dse_blk_r_perm(0:inv_dse_blk_dim)
      double precision inv_dse_blk_v(inv_dse_blk_dim, inv_dse_blk_dim)
      double precision inv_dse_blk_rhs_v(0:inv_dse_blk_dim)
      integer r_n, og_r_n, it_n, c_n
c
c     Indicate that the nonzeros are not known, being careful in case
c     rhs_ix(0) was passed as n_r+1
c
      if (rhs_ix(0) .lt. n_r) rhs_ix(0) = n_r+1
      do 10, r_n = 1, inv_dse_blk_dim
         inv_dse_blk_rhs_v(r_n) = rhs_v(inv_dse_blk_r_se(r_n))
 10   continue
      do 30, it_n = 1, inv_dse_blk_dim
         if (inv_dse_blk_rhs_v(it_n) .eq. 0d0) goto 30
         og_r_n = inv_dse_blk_r_perm(it_n)
         inv_dse_blk_rhs_v(it_n) =
     &        inv_dse_blk_rhs_v(it_n)*inv_dse_blk_v(it_n, og_r_n)
         do 20, c_n = it_n+1, inv_dse_blk_dim
            inv_dse_blk_rhs_v(c_n) = inv_dse_blk_rhs_v(c_n) +
     &           inv_dse_blk_v(c_n, og_r_n)*inv_dse_blk_rhs_v(it_n)
 20      continue
 30   continue
      do 50, it_n = inv_dse_blk_dim, 2, -1
         if (inv_dse_blk_rhs_v(it_n) .eq. 0d0) goto 50
         og_r_n = inv_dse_blk_r_perm(it_n)
         do 40, c_n = it_n-1, 1, -1
            inv_dse_blk_rhs_v(c_n) = inv_dse_blk_rhs_v(c_n) +
     &           inv_dse_blk_v(c_n, og_r_n)*inv_dse_blk_rhs_v(it_n)
 40      continue
 50   continue
      do 60, it_n = 1, inv_dse_blk_dim
         rhs_v(inv_dse_blk_r_se(it_n)) = inv_dse_blk_rhs_v(it_n)
 60   continue
      return
      end
 
C->>> --------------------------------------> ems_g_bwd_tran_c_eta_p <<<
      subroutine ems_g_bwd_tran_c_eta_p(
     &     n_r, n_eta, n_eta_el,
     &     eta_ix, eta_sa,
     &     eta_w_l_en_in_r, eta_w_lm1_en_in_r)
      implicit none
      include 'EMSV.INC'
      integer n_r, n_eta, n_eta_el
      integer eta_ix(0:n_eta_el)
      integer eta_sa(0:n_eta+1)
      integer eta_w_l_en_in_r(0:n_r)
      integer eta_w_lm1_en_in_r(0:n_r)
 
      integer eta_n, el_n, r_n
 
      do 10, r_n = 1, n_r
         eta_w_l_en_in_r(r_n) = 0
         eta_w_lm1_en_in_r(r_n) = 0
 10   continue
      do 30, eta_n = 1, n_eta
         do 20, el_n = eta_sa(eta_n), eta_sa(eta_n+1)-1
            r_n = eta_ix(el_n)
            eta_w_lm1_en_in_r(r_n) = eta_w_l_en_in_r(r_n)
            eta_w_l_en_in_r(r_n) = eta_n
 20      continue
 30   continue
      return
      end
 
C->>> -----------------------------------------> ems_g_r_eta_fi_en_p <<<
      subroutine ems_g_r_eta_fi_en_p(
     &     alw_f7_wr, wr_cn, er_fd,
     &     n_r, n_eta, n_eta_el, n_lo_c_eta,
     &     n_lo_r_eta, n_lo_r_eta_el,
     &     n_up_r_eta, n_up_r_eta_el,
     &     eta_v, eta_ix, eta_sa,
     &     lo_eta_r_sa, up_eta_r_sa,
     &     lo_eta_pv_in_r, up_eta_pv_in_r,
     &     lo_eta_pv_in_c, up_eta_pv_in_c,
     &     lo_eta_r_ln_m1, up_eta_r_ln_m1)
      implicit none
      include 'EMSV.INC'
      logical alw_f7_wr, er_fd
      integer wr_cn
      integer n_r, n_eta, n_eta_el, n_lo_c_eta
      integer n_lo_r_eta, n_lo_r_eta_el
      integer n_up_r_eta, n_up_r_eta_el
      double precision eta_v(0:n_eta_el)
      integer eta_ix(0:n_eta_el)
      integer eta_sa(0:n_eta+1)
      integer lo_eta_r_sa(0:n_r+1)
      integer up_eta_r_sa(0:n_r+1)
      integer lo_eta_pv_in_r(0:n_r)
      integer up_eta_pv_in_r(0:n_r)
      integer lo_eta_pv_in_c(0:n_r)
      integer up_eta_pv_in_c(0:n_r)
      integer lo_eta_r_ln_m1(0:n_r)
      integer up_eta_r_ln_m1(0:n_r)
      integer eta_n, r_n
      integer r_eta_n_en, el_n
      integer mx_eta_n
      integer l_lo_r_eta_n, l_lo_r_el_n
      integer l_up_r_eta_n, l_up_r_el_n
      integer eta_fi_rp_lvl
      integer ck_r_eta_fi_pv_seq
CM      IF (emsol_dev .EQ. 1) THEN
C?      integer inv_pic
CM      ENDIF

      eta_fi_rp_lvl = 0
      if (eta_fi_rp_lvl .gt. 0) then
         if (alw_f7_wr) write(wr_cn, 9000)
         call ems_rp_eta_fi(
     &        alw_f7_wr, wr_cn,
     &        eta_fi_rp_lvl,
     &        'Col L',
     &        1, n_lo_c_eta, n_eta_el,
     &        eta_v, eta_sa, eta_ix)
         call ems_rp_eta_fi(
     &        alw_f7_wr, wr_cn,
     &        eta_fi_rp_lvl,
     &        'Col U',
     &        n_lo_c_eta+1, n_eta, n_eta_el,
     &        eta_v, eta_sa, eta_ix)
      endif
CM      IF (sun_lib .EQ. 1) THEN
C?      inv_pic = 0
C?      if (inv_pic .ne. 0) then
C?         call ems_inv_pic(
C?     &        alw_f7_wr, wr_cn,
C?     &        'ColEtaFi.pic', .true.,
C?     &        n_r, n_r,
C?     &        n_lo_c_eta, n_eta_el,
C?     &        n_lo_c_eta+1, n_eta, n_eta_el,
C?     &        lo_eta_r_ln_m1, up_eta_r_ln_m1,
C?     &        eta_v, eta_ix, eta_sa,
C?     &        eta_v, eta_ix, eta_sa,
C?     &        -1)
C?         call system('/usr/bin/X11/xv ColEtaFi.pic')
C?      endif
CM      ENDIF
 
      ck_r_eta_fi_pv_seq = 0
      if (ck_r_eta_fi_pv_seq .ne. 0) then
         call ems_ck_eta_fi_pv_seq(
     &        alw_f7_wr, wr_cn, er_fd,
     &        n_r, 1, n_lo_c_eta, n_eta_el,
     &        eta_ix, eta_sa,
     &        lo_eta_pv_in_r)
         if (er_fd) goto 8000
         call ems_ck_eta_fi_pv_seq(
     &        alw_f7_wr, wr_cn, er_fd,
     &        n_r, n_lo_c_eta+1, n_eta, n_eta_el,
     &        eta_ix, eta_sa,
     &        up_eta_pv_in_r)
         if (er_fd) goto 8010
      endif
      mx_eta_n = 2*n_r
c
c     Each row has either
c     1.  a pivot in a column-wise L-eta
c     2.  a pivot in a column-wise U-eta
c     3.  a pivot in both a column-wise L-eta and U-eta
c     4.  no pivot in a column-wise eta
c
c     The row-wise L-etas are ordered so that their pivots are in the
c     same order as the pivots in the column-wise L-etas. A row-wise
c     L-eta is avoided if the only entry in the row is a unit pivot.
c
c     The row-wise U-etas are ordered so that their pivots are in the
c     same order as the pivots in the column-wise U-etas. A row-wise
c     U-eta is avoided if the only entry in the row is a unit pivot.
c
c     Each of the subsequent U-etas (whose order is immaterial) has a
c     unit pivot in one the rows with no pivot in a column-wise eta and
c     its non-pivotal entries are the column-wise eta entries in that
c     row.
c
c     Calculate the number of nonpivotal L- and U-eta entries in each
c     row.
c
      do 10, r_n = 1, n_r
         lo_eta_r_ln_m1(r_n) = 0
         up_eta_r_ln_m1(r_n) = 0
         lo_eta_pv_in_c(r_n) = mx_eta_n+1
         up_eta_pv_in_c(r_n) = mx_eta_n+1
 10   continue
      do 30, eta_n = 1, n_lo_c_eta
         do 20, el_n = eta_sa(eta_n)+1, eta_sa(eta_n+1)-1
            r_n = eta_ix(el_n)
            lo_eta_r_ln_m1(r_n) = lo_eta_r_ln_m1(r_n) + 1
 20      continue
 30   continue
      do 50, eta_n = n_lo_c_eta+1, n_eta
         do 40, el_n = eta_sa(eta_n)+1, eta_sa(eta_n+1)-1
            r_n = eta_ix(el_n)
            up_eta_r_ln_m1(r_n) = up_eta_r_ln_m1(r_n) + 1
 40      continue
 50   continue
c
c     Determine the starts of the L-etas.
c
      l_lo_r_eta_n = 0
      l_lo_r_el_n = 0
      do 110, r_n = 1, n_r
         if (lo_eta_pv_in_r(r_n) .le. mx_eta_n) goto 110
c
c     The row has no column-wise L-eta pivot: if it has any entries it
c     needs a row-wise L-eta so account for its pivot in the number of
c     entries.
c
         r_eta_n_en = lo_eta_r_ln_m1(r_n)
         if (r_eta_n_en .le. 0) goto 110
         r_eta_n_en = r_eta_n_en + 1
c
c     Increment the number of L-etas and store the start
c
         l_lo_r_eta_n = l_lo_r_eta_n + 1
         lo_eta_r_sa(l_lo_r_eta_n) = l_lo_r_el_n + 1
c
c     Increment the number of L-eta entries
c
         l_lo_r_el_n = l_lo_r_el_n + r_eta_n_en
c
c     Use lo_eta_pv_in_c to store the eta number for which entries in a
c     particular row are stored
c
         lo_eta_pv_in_c(r_n) = l_lo_r_eta_n
 110  continue
c
c     Go through the column-wise L-etas in reverse order, seeing whether
c     there are any entries in the pivotal rows and, if not, whether the
c     eta has a unit pivot.
c
      do 115, eta_n = n_lo_c_eta, 1, -1
         el_n = eta_sa(eta_n)
         r_n = eta_ix(el_n)
         r_eta_n_en = lo_eta_r_ln_m1(r_n)
         if (r_eta_n_en .le. 0) then
            if (eta_v(el_n) .ne. one) r_eta_n_en = r_eta_n_en + 1
         else
            r_eta_n_en = r_eta_n_en + 1
         endif
         if (r_eta_n_en .gt. 0) then
            l_lo_r_eta_n = l_lo_r_eta_n + 1
            lo_eta_r_sa(l_lo_r_eta_n) = l_lo_r_el_n + 1
            l_lo_r_el_n = l_lo_r_el_n + r_eta_n_en
            lo_eta_pv_in_c(r_n) = l_lo_r_eta_n
         endif
 115  continue
      lo_eta_r_sa(l_lo_r_eta_n+1) = l_lo_r_el_n+1
      n_lo_r_eta = l_lo_r_eta_n
      n_lo_r_eta_el = l_lo_r_el_n
c
c     Determine the starts of the U-etas.
c
c     Nontrivial rows which have no pivot in a column-wise U-eta must be
c     given a row-wise U-eta and only the column-wise U-eta entries can
c     be put into the row-wise U-eta.
c
      l_up_r_eta_n = 0
      l_up_r_el_n = 0
      do 120, r_n = 1, n_r
         if (up_eta_pv_in_r(r_n) .le. mx_eta_n) goto 120
c
c     The row has no column-wise U-eta pivot: if it has any entries it
c     needs a row-wise U-eta so account for its pivot in the number of
c     entries.
c
         r_eta_n_en = up_eta_r_ln_m1(r_n)
         if (r_eta_n_en .le. 0) goto 120
         r_eta_n_en = r_eta_n_en + 1
c
c     Increment the number of U-etas and store the start
c
         l_up_r_eta_n = l_up_r_eta_n + 1
         up_eta_r_sa(l_up_r_eta_n) = l_up_r_el_n + 1
c
c     Increment the number of U-eta entries
c
         l_up_r_el_n = l_up_r_el_n + r_eta_n_en
c
c     Use up_eta_pv_in_c to store the eta number for which entries in a
c     particular row are stored
c
         up_eta_pv_in_c(r_n) = l_up_r_eta_n
 120  continue
      do 125, eta_n = n_eta, n_lo_c_eta + 1, -1
         el_n = eta_sa(eta_n)
         r_n = eta_ix(el_n)
         r_eta_n_en = up_eta_r_ln_m1(r_n)
         if (r_eta_n_en .le. 0) then
            if (eta_v(el_n) .ne. one) r_eta_n_en = r_eta_n_en + 1
         else
            r_eta_n_en = r_eta_n_en + 1
         endif
         if (r_eta_n_en .gt. 0) then
            l_up_r_eta_n = l_up_r_eta_n + 1
            up_eta_r_sa(l_up_r_eta_n) = l_up_r_el_n + 1
            l_up_r_el_n = l_up_r_el_n + r_eta_n_en
            up_eta_pv_in_c(r_n) = l_up_r_eta_n
         endif
 125  continue
      up_eta_r_sa(l_up_r_eta_n+1) = l_up_r_el_n + 1
      n_up_r_eta = l_up_r_eta_n
      n_up_r_eta_el = l_up_r_el_n
 7000 continue
      return
 8000 continue
      if (alw_f7_wr) write(wr_cn, 9800)
      er_fd = .true.
      goto 7000
 8010 continue
      if (alw_f7_wr) write(wr_cn, 9801)
      er_fd = .true.
      goto 7000
 9000 format('Col-wise etas')
 9800 format('ERROR in column-wise L-etas')
 9801 format('ERROR in column-wise U-etas')
      end
 
C->>> -------------------------------------------> ems_g_r_eta_fi_en <<<
      subroutine ems_g_r_eta_fi_en(
     &     alw_f7_wr, wr_cn, er_fd,
     &     n_r, n_eta, n_eta_el, n_lo_c_eta,
     &     n_lo_r_eta, n_lo_r_eta_el,
     &     n_up_r_eta, n_up_r_eta_el,
     &     eta_v, eta_ix, eta_sa,
     &     lo_eta_c_v, lo_eta_c_ix, lo_eta_r_sa,
     &     up_eta_c_v, up_eta_c_ix, up_eta_r_sa,
     &     lo_eta_pv_in_r, up_eta_pv_in_r,
     &     lo_eta_pv_in_c, up_eta_pv_in_c,
     &     lo_eta_r_ln_m1, up_eta_r_ln_m1)
      implicit none
      include 'EMSV.INC'
      logical alw_f7_wr, er_fd
      integer wr_cn
      integer n_r, n_eta, n_eta_el, n_lo_c_eta
      integer n_lo_r_eta, n_lo_r_eta_el
      integer n_up_r_eta, n_up_r_eta_el
      double precision eta_v(0:n_eta_el)
      integer eta_ix(0:n_eta_el)
      integer eta_sa(0:n_eta+1)
      double precision lo_eta_c_v(0:n_lo_r_eta_el)
      integer lo_eta_c_ix(0:n_lo_r_eta_el)
      integer lo_eta_r_sa(0:n_r+1)
      double precision up_eta_c_v(0:n_up_r_eta_el)
      integer up_eta_c_ix(0:n_up_r_eta_el)
      integer up_eta_r_sa(0:n_r+1)
      integer lo_eta_pv_in_r(0:n_r)
      integer up_eta_pv_in_r(0:n_r)
      integer lo_eta_pv_in_c(0:n_r)
      integer up_eta_pv_in_c(0:n_r)
      integer lo_eta_r_ln_m1(0:n_r)
      integer up_eta_r_ln_m1(0:n_r)
 
      integer eta_n, c_n, r_n
 
      integer el_n
      integer r_eta_n, r_eta_el_n
      integer mx_eta_n
      integer lo_r_eta_n
      integer up_r_eta_n
 
      integer eta_fi_ze_v_ix_ck_lvl
      integer ck_r_eta_fi_pv_seq
      integer eta_fi_rp_lvl
 
CM      IF (sun_lib .EQ. 1) THEN
C?      integer inv_pic
C?      integer inv_pic_mx_n_r
C?      parameter (inv_pic_mx_n_r = 1000)
C?      integer lc_i_wk_a(inv_pic_mx_n_r)
CM      ENDIF
 
      er_fd = .false.
      mx_eta_n = 2*n_r
c
c     Insert the unit pivots for rows with no column-wise pivot
c
      do 10, r_n = 1, n_r
         if (lo_eta_pv_in_r(r_n) .gt. mx_eta_n) then
            r_eta_n = lo_eta_pv_in_c(r_n)
            if (r_eta_n .le. mx_eta_n) then
c
c     Add a unit pivot for the additional L-eta.
c
               r_eta_el_n = lo_eta_r_sa(r_eta_n)
               lo_eta_c_ix(r_eta_el_n) = r_n
               lo_eta_c_v(r_eta_el_n) = one
            endif
         endif
 10   continue
      do 20, r_n = 1, n_r
         if (up_eta_pv_in_r(r_n) .gt. mx_eta_n) then
            r_eta_n = up_eta_pv_in_c(r_n)
            if (r_eta_n .le. mx_eta_n) then
c
c     Add a unit pivot for the additional U-eta.
c
               r_eta_el_n = up_eta_r_sa(r_eta_n)
               up_eta_c_ix(r_eta_el_n) = r_n
               up_eta_c_v(r_eta_el_n) = one
            endif
         endif
 20   continue
c
c     Insert the column-wise L-eta entries into the row-wise L-etas and
c     U-etas (for those in rows with no pivot in a column-wise eta).
c
      do 210, lo_r_eta_n = 1, n_lo_r_eta
         lo_eta_r_ln_m1(lo_r_eta_n) = lo_eta_r_sa(lo_r_eta_n)
 210  continue
      do 220, up_r_eta_n = 1, n_up_r_eta
         up_eta_r_ln_m1(up_r_eta_n) = up_eta_r_sa(up_r_eta_n)
 220  continue
      do 240, eta_n = 1, n_lo_c_eta
         el_n = eta_sa(eta_n)
         c_n = eta_ix(el_n)
         r_eta_n = lo_eta_pv_in_c(c_n)
         if (r_eta_n .lt. mx_eta_n) then
            r_eta_el_n = lo_eta_r_sa(r_eta_n)
            lo_eta_c_ix(r_eta_el_n) = c_n
            lo_eta_c_v(r_eta_el_n) = eta_v(el_n)
         endif
         do 230, el_n = eta_sa(eta_n)+1, eta_sa(eta_n+1)-1
            r_n = eta_ix(el_n)
            r_eta_n = lo_eta_pv_in_c(r_n)
            r_eta_el_n = lo_eta_r_ln_m1(r_eta_n) + 1
            lo_eta_c_ix(r_eta_el_n) = c_n
            lo_eta_c_v(r_eta_el_n) = eta_v(el_n)
            lo_eta_r_ln_m1(r_eta_n) = r_eta_el_n
 230     continue
 240  continue
c
c     Insert the column-wise U-eta entries into the row-wise U-etas.
c
      do 260, eta_n = n_lo_c_eta+1, n_eta
         el_n = eta_sa(eta_n)
         c_n = eta_ix(el_n)
         r_eta_n = up_eta_pv_in_c(c_n)
         if (r_eta_n .lt. mx_eta_n) then
            r_eta_el_n = up_eta_r_sa(r_eta_n)
            up_eta_c_ix(r_eta_el_n) = c_n
            up_eta_c_v(r_eta_el_n) = eta_v(el_n)
         endif
         do 250, el_n = eta_sa(eta_n)+1, eta_sa(eta_n+1)-1
            r_n = eta_ix(el_n)
            r_eta_n = up_eta_pv_in_c(r_n)
            r_eta_el_n = up_eta_r_ln_m1(r_eta_n) + 1
            up_eta_c_ix(r_eta_el_n) = c_n
            up_eta_c_v(r_eta_el_n) = eta_v(el_n)
            up_eta_r_ln_m1(r_eta_n) = r_eta_el_n
 250     continue
 260  continue
      eta_fi_ze_v_ix_ck_lvl = 0
      if (eta_fi_ze_v_ix_ck_lvl .gt. 0) then
         call ems_ck_eta_fi_ze_v_ix(
     &        alw_f7_wr, wr_cn, er_fd,
     &        'Row L',
     &        1, n_lo_r_eta, n_lo_r_eta_el,
     &        lo_eta_c_v, lo_eta_c_ix, lo_eta_r_sa)
         if (er_fd) goto 8000
         call ems_ck_eta_fi_ze_v_ix(
     &        alw_f7_wr, wr_cn, er_fd,
     &        'Row U',
     &        1, n_up_r_eta, n_up_r_eta_el,
     &        up_eta_c_v, up_eta_c_ix, up_eta_r_sa)
         if (er_fd) goto 8010
      endif
      eta_fi_rp_lvl = 0
      if (eta_fi_rp_lvl .gt. 0) then
         call ems_rp_eta_fi(
     &        alw_f7_wr, wr_cn,
     &        eta_fi_rp_lvl,
     &        'Row U',
     &        1, n_up_r_eta, n_up_r_eta_el,
     &        up_eta_c_v, up_eta_r_sa, up_eta_c_ix)
         call ems_rp_eta_fi(
     &        alw_f7_wr, wr_cn,
     &        eta_fi_rp_lvl,
     &        'Row L',
     &        1, n_lo_r_eta, n_lo_r_eta_el,
     &        lo_eta_c_v, lo_eta_r_sa, lo_eta_c_ix)
      endif
CM      IF (sun_lib .EQ. 1) THEN
C?      inv_pic = 0
C?      if (inv_pic .ne. 0) then
C?         call ems_inv_pic(
C?     &        alw_f7_wr, wr_cn,
C?     &        'RowEtaFi.pic', .false.,
C?     &        n_r, inv_pic_mx_n_r,
C?     &        n_lo_r_eta, n_lo_r_eta_el,
C?     &        1, n_up_r_eta, n_up_r_eta_el,
C?     &        lo_eta_r_ln_m1, lc_i_wk_a,
C?     &        lo_eta_c_v, lo_eta_c_ix, lo_eta_r_sa,
C?     &        up_eta_c_v, up_eta_c_ix, up_eta_r_sa,
C?     &        -1)
C?         call system('/usr/bin/X11/xv RowEtaFi.pic')
C?      endif
CM      ENDIF
      ck_r_eta_fi_pv_seq = 0
      if (ck_r_eta_fi_pv_seq .gt. 0) then
         call ems_ck_eta_fi_pv_seq(
     &        alw_f7_wr, wr_cn, er_fd,
     &        n_r, 1, n_lo_r_eta, n_lo_r_eta_el,
     &        lo_eta_c_ix, lo_eta_r_sa,
     &        lo_eta_pv_in_c)
         if (er_fd) goto 8000
      endif
      if (ck_r_eta_fi_pv_seq .gt. 0) then
         call ems_ck_eta_fi_pv_seq(
     &        alw_f7_wr, wr_cn, er_fd,
     &        n_r, 1, n_up_r_eta, n_up_r_eta_el,
     &        up_eta_c_ix, up_eta_r_sa,
     &        up_eta_pv_in_c)
         if (er_fd) goto 8010
      endif
 7000 continue
      return
 8000 continue
      if (alw_f7_wr) write(wr_cn, *)'ERROR in row-wise L-etas'
      goto 7000
 8010 continue
      if (alw_f7_wr) write(wr_cn, *)'ERROR in row-wise U-etas'
      goto 7000
      end
 
      subroutine ems_ck_eta_fi_pv_seq(
     &     alw_f7_wr, wr_cn, er_fd,
     &     n_r, f_eta_n, l_eta_n, n_eta_el,
     &     eta_ix, eta_sa,
     &     eta_pv_in_r)
      implicit none
      logical alw_f7_wr, er_fd
      integer wr_cn
      integer n_r, f_eta_n, l_eta_n, n_eta_el
      integer eta_ix(0:n_eta_el)
      integer eta_sa(0:l_eta_n+1)
      integer eta_pv_in_r(0:n_r)
      integer eta_n, el_n, pv_r_n, r_n, pv_eta_n, en_eta_n
 
      er_fd = .false.
      do 20, eta_n = f_eta_n, l_eta_n
         el_n = eta_sa(eta_n)
         pv_r_n = eta_ix(el_n)
         pv_eta_n = eta_pv_in_r(pv_r_n)
         if (pv_eta_n .ne. eta_n) then
CM      IF (emsol_dev .EQ. 1) THEN
C?            write(*, 9000)eta_n, el_n, ' Pivot row ', pv_r_n, pv_eta_n
CM      ENDIF
            er_fd = .true.
         endif
         do 10, el_n = el_n+1, eta_sa(eta_n+1)-1
            r_n = eta_ix(el_n)
            en_eta_n = eta_pv_in_r(r_n)
            if (en_eta_n .le. eta_n) then
CM      IF (emsol_dev .EQ. 1) THEN
C?               write(*, 9000)eta_n, el_n, '       Row ', r_n, en_eta_n
CM      ENDIF
               er_fd = .true.
            endif
 10      continue
 20   continue
      return
CM      IF (emsol_dev .EQ. 1) THEN
C? 9000 format('***ERROR***PvInR: eta ', i7, ' el ', i7,
C?     &     a11, i7, ' EtaPvInR = ', i9)
CM      ENDIF
      end
 
      subroutine ems_ck_eta_fi_ze_v_ix(
     &     alw_f7_wr, wr_cn, er_fd,
     &     ch5_eta_id,
     &     f_eta_n, l_eta_n, n_eta_el,
     &     eta_v, eta_ix, eta_sa)
      implicit none
      include 'EMSV.INC'
      logical alw_f7_wr, er_fd
      integer wr_cn
      character*5 ch5_eta_id
      integer f_eta_n, l_eta_n, n_eta_el
      double precision eta_v(0:n_eta_el)
      integer eta_ix(0:n_eta_el)
      integer eta_sa(0:l_eta_n+1)
      integer eta_n, el_n, os
 
      er_fd = .false.
      do 20, eta_n = f_eta_n, l_eta_n
         el_n = eta_sa(eta_n)
         if (eta_ix(el_n) .eq. 0 .or. eta_v(el_n) .eq. zero) then
            er_fd = .true.
            if (alw_f7_wr) write(wr_cn, 9300)
     &           ch5_eta_id, eta_n, el_n, 0, 'Pivot',
     &           eta_ix(el_n), eta_v(el_n)
         endif
         do 10, el_n = el_n+1, eta_sa(eta_n+1)-1
            if (eta_ix(el_n) .eq. 0 .or. eta_v(el_n) .eq. zero) then
               er_fd = .true.
               os = el_n - eta_sa(eta_n)
               if (alw_f7_wr) write(wr_cn, 9300)
     &              ch5_eta_id, eta_n, el_n, os, 'Entry',
     &              eta_ix(el_n), eta_v(el_n)
            endif
 10      continue
 20   continue
      return
 9300 format(a5, '-eta ', i7, ' el = ', i7, ' os = ', i3,
     &     ': ', a5, ' (r,v) = (', i7, 2x, g11.4, ')')
      end
 
      subroutine ems_rp_eta_fi(
     &     alw_f7_wr, wr_cn,
     &     rp_lvl,
     &     ch5_eta_id,
     &     f_eta_n, l_eta_n, n_eta_el,
     &     eta_v, eta_sa, eta_ix)
      implicit none
      logical alw_f7_wr
      integer wr_cn
      integer rp_lvl
      character*5 ch5_eta_id
      integer f_eta_n, l_eta_n, n_eta_el
      double precision eta_v(0:n_eta_el)
      integer eta_ix(0:n_eta_el)
      integer eta_sa(0:l_eta_n+1)
      integer eta_n, eta_el_n
      integer n_eta, os
      integer eta_n_el
      integer n_unit_eta
 
      if (rp_lvl .le. 0) goto 7000
      n_unit_eta = 0
      n_eta = l_eta_n-f_eta_n+1
      if (n_eta .le. 0) goto 7000
      if (alw_f7_wr) write(*, 9010)ch5_eta_id, n_eta
      do eta_n = f_eta_n, l_eta_n
         os = eta_n - f_eta_n + 1
         eta_el_n = eta_sa(eta_n)
         eta_n_el = eta_sa(eta_n+1)-eta_sa(eta_n)
         if (alw_f7_wr) write(*, 9100)os,
     &        eta_sa(eta_n), eta_sa(eta_n+1)-1,
     &        eta_n_el, eta_ix(eta_el_n), eta_v(eta_el_n)
         if (eta_n_el .eq. 1 .and. eta_v(eta_el_n) .eq. 1d0)
     &        n_unit_eta = n_unit_eta + 1
         if (rp_lvl .ge. 2) then
            do eta_el_n = eta_el_n+1, eta_sa(eta_n+1)-1
               if (alw_f7_wr) write(*, 9110)
     &              eta_ix(eta_el_n), eta_v(eta_el_n)
            enddo
         endif
      enddo
      if (n_unit_eta .gt. 0 .and. alw_f7_wr) write(*, 9200)n_unit_eta
 7000 continue
      return
 9010 format(a5, ' (', i7, '):')
 9100 format('Eta ', i7, ' Els ', i7, ' to ', i7, ' (', i5, '):',
     &     ' Pivot (r, v) = (', i7, 2x, g11.4, ')')
 9110 format(43x, ' Entry (r, v) = (', i7, 2x, g11.4, ')')
 9200 format('Eta file has ', i7, ' unit etas')
      end
 
CM      IF (sun_lib .EQ. 1) THEN
C?      subroutine ems_inv_pic(
C?     &     alw_f7_wr, wr_cn,
C?     &     fi_nm,
C?     &     c_eta_fi,
C?     &     n_r, lc_i_wk_a_n_en,
C?     &     l_lo_eta_n, n_lo_eta_el,
C?     &     f_up_eta_n, l_up_eta_n, n_up_eta_el,
C?     &     r_perm, lc_i_wk_a,
C?     &     lo_eta_v, lo_eta_ix, lo_eta_sa,
C?     &     up_eta_v, up_eta_ix, up_eta_sa,
C?     &     rhs_ix)
C?      implicit none
C?      include 'EMSV.INC'
C?      logical alw_f7_wr
C?      integer wr_cn
C?      character*(*) fi_nm
C?      logical c_eta_fi
C?      integer n_r, lc_i_wk_a_n_en
C?      integer l_lo_eta_n, n_lo_eta_el
C?      integer f_up_eta_n, l_up_eta_n, n_up_eta_el
C?      double precision lo_eta_v(0:n_lo_eta_el)
C?      integer lo_eta_ix(0:n_lo_eta_el)
C?      integer lo_eta_sa(0:l_lo_eta_n+1)
C?      double precision up_eta_v(0:n_up_eta_el)
C?      integer up_eta_ix(0:n_up_eta_el)
C?      integer up_eta_sa(0:l_up_eta_n+1)
C?      integer r_perm(0:n_r)
C?      integer lc_i_wk_a(0:lc_i_wk_a_n_en)
C?      integer rhs_ix(0:n_r)
C?      integer n_eta
C?      integer ix_n, r_n, eta_n, el_n, pv_r_n
C?c      integer c_n, r_eta_n
C?      integer n_pv_r
C?      integer lo_eta_n, up_eta_n
C?      integer lo_tri_pv_r_n, up_tri_pv_r_n
C?      integer ascii_mode, pic_cn
C?      integer inv_pic_mx_n_r
C?      parameter (inv_pic_mx_n_r = 1000)
C?      integer f_lo_eta_n
C?      integer fm_lo_eta_n
C?c      logical mk_eta
C?c      logical rp_lo_eta_fi
C?      integer pv_in_lo_bt
C?      integer pv_in_up_bt
C?      integer pv_in_lo_no_up
C?c      integer pv_in_up_no_lo
C?      integer pv_in_lo_and_up
C?      parameter (
C?     &     pv_in_lo_bt = bt1,
C?     &     pv_in_up_bt = bt2,
C?     &     pv_in_lo_no_up = pv_in_lo_bt,
C?c     &     pv_in_up_no_lo = pv_in_up_bt,
C?     &     pv_in_lo_and_up = pv_in_lo_bt + pv_in_up_bt)
C? 
C?      if (n_r .gt. inv_pic_mx_n_r) goto 7000
C?      pic_cn = 11
C?      f_lo_eta_n = 1
C?      n_eta = l_lo_eta_n + (l_up_eta_n-f_up_eta_n+1)
C?      do 30, r_n = 1, n_r
C?         r_perm(r_n) = 0
C?         lc_i_wk_a(r_n) = 0
C? 30   continue
C?      do 40, eta_n = 1, l_lo_eta_n
C?         el_n = lo_eta_sa(eta_n)
C?         pv_r_n = lo_eta_ix(el_n)
C?         lc_i_wk_a(pv_r_n) = lc_i_wk_a(pv_r_n) + pv_in_lo_bt
C? 40   continue
C?      do 50, eta_n = f_up_eta_n, l_up_eta_n
C?         el_n = up_eta_sa(eta_n)
C?         pv_r_n = up_eta_ix(el_n)
C?         lc_i_wk_a(pv_r_n) = lc_i_wk_a(pv_r_n) + pv_in_up_bt
C? 50   continue
C?      n_pv_r = 0
C?      do 60, r_n = 1, n_r
C?         if (lc_i_wk_a(r_n) .ne. 0) n_pv_r = n_pv_r + 1
C?         if (lc_i_wk_a(r_n) .gt. pv_in_lo_and_up) then
C?            if (alw_f7_wr) write(wr_cn, *)' Eta row ', r_n,
C?     &           ' has more than two pivots: lc_i_wk_a(r_n) = ',
C?     &           lc_i_wk_a(r_n)
C?            goto 7000
C?         endif
C? 60   continue
C? 
C?      r_n = n_r - n_pv_r
C?      if (c_eta_fi) then
C?         up_eta_n = l_up_eta_n
C?         lo_eta_n = 1
C?         if (n_eta .le. 0) goto 100
C?         if (lo_eta_n .gt. l_lo_eta_n) then
C?            lo_tri_pv_r_n = -1
C?            goto 80
C?         endif
C? 70      continue
C?c
C?c     Assign L-only pivots until the next L-and-U pivot is found, in
C?c     which case, assign U-only pivots until the L-and-U pivot found is
C?c     assigned.
C?c
C?         el_n = lo_eta_sa(lo_eta_n)
C?         lo_eta_n = lo_eta_n + 1
C?         lo_tri_pv_r_n = lo_eta_ix(el_n)
C?         if (lc_i_wk_a(lo_tri_pv_r_n) .eq. pv_in_lo_and_up) then
C?c
C?c     An L-and-U pivot is found: assign U-only pivots until this
C?c     L-and-U pivot is assigned.
C?c
C?            goto 80
C?         else
C?c
C?c     An L-only pivot is found
C?c
C?            r_n = r_n + 1
C?            r_perm(lo_tri_pv_r_n) = r_n
C?            if (lo_eta_n .le. l_lo_eta_n) goto 70
C?         endif
C?         if (up_eta_n .ge. f_up_eta_n) goto 80
C?         goto 100
C? 80      continue
C?c
C?c     Assign U-only pivots until the L-and-U pivot found is assigned.
C?c
C?         el_n = up_eta_sa(up_eta_n)
C?         up_eta_n = up_eta_n - 1
C?         up_tri_pv_r_n = up_eta_ix(el_n)
C?         if (up_tri_pv_r_n .eq. lo_tri_pv_r_n) then
C?c
C?c     The matching L-and-U pivot has been found
C?c
C?            r_n = r_n + 1
C?            r_perm(up_tri_pv_r_n) = r_n
C?            if (lo_eta_n .le. l_lo_eta_n) goto 70
C?         else if (lc_i_wk_a(up_tri_pv_r_n) .eq. pv_in_lo_and_up) then
C?c
C?c     This is an L-and-U pivot which has already been assigned
C?c
C?            if (alw_f7_wr) write(wr_cn, *)' So: this CAN happen!'
C?            if (alw_f7_wr) write(wr_cn, *)' lo_eta_n = ', lo_eta_n
C?            if (alw_f7_wr) write(wr_cn, *)' up_eta_n = ', up_eta_n
C?            if (alw_f7_wr) write(wr_cn, *)
C?     &           ' lo_tri_pv_r_n = ', lo_tri_pv_r_n
C?            if (alw_f7_wr) write(wr_cn, *)
C?     &           ' up_tri_pv_r_n = ', up_tri_pv_r_n
C?            if (alw_f7_wr) write(wr_cn, *)
C?     &           ' lc_i_wk_a( ', up_tri_pv_r_n, ') = ', pv_in_lo_and_up
C?            goto 7000
C?         else
C?c
C?c     This is a U-only pivot.
C?c
C?            r_n = r_n + 1
C?            r_perm(up_tri_pv_r_n) = r_n
C?         endif
C?         if (up_eta_n .ge. f_up_eta_n) goto 80
C? 100     continue
C?      else
C?         up_eta_n = 1
C?         fm_lo_eta_n = l_lo_eta_n
C?         if (n_eta .le. 0) goto 200
C?         if (up_eta_n .gt. l_up_eta_n) then
C?            up_tri_pv_r_n = -1
C?            goto 180
C?         endif
C? 170     continue
C?c
C?c     Assign U-only pivots until the next L-and-U pivot is found, in
C?c     which case, assign L-only pivots until the L-and-U pivot found is
C?c     assigned.
C?c
C?         el_n = up_eta_sa(up_eta_n)
C?         up_eta_n = up_eta_n + 1
C?         up_tri_pv_r_n = up_eta_ix(el_n)
C?         if (iand(lc_i_wk_a(up_tri_pv_r_n), pv_in_lo_and_up) .eq.
C?     &        pv_in_lo_and_up) then
C?c
C?c     An L-and-U pivot is found: assign L-only pivots until this
C?c     L-and-U pivot is assigned.
C?c
C?            goto 180
C?         else
C?c
C?c     A U-only pivot is found
C?c
C?            r_n = r_n + 1
C?            r_perm(up_tri_pv_r_n) = r_n
C?            if (up_eta_n .le. l_up_eta_n) goto 170
C?         endif
C?         if (fm_lo_eta_n .ge. f_lo_eta_n) goto 180
C?         goto 200
C? 180     continue
C?c
C?c     Assign L-only pivots until the L-and-U pivot found is assigned.
C?c
C?         do 190, lo_eta_n = l_lo_eta_n, fm_lo_eta_n+1, -1
C?            el_n = lo_eta_sa(lo_eta_n)
C?            lo_tri_pv_r_n = lo_eta_ix(el_n)
C?            if (lo_tri_pv_r_n .eq. up_tri_pv_r_n) then
C?c
C?c     The matching L-and-U pivot has been found
C?c
C?               r_n = r_n + 1
C?               r_perm(lo_tri_pv_r_n) = r_n
C?               if (up_eta_n .le. l_up_eta_n) goto 170
C?            endif
C? 190     continue
C?         do 195, lo_eta_n = fm_lo_eta_n, f_lo_eta_n, -1
C?            el_n = lo_eta_sa(lo_eta_n)
C?            lo_tri_pv_r_n = lo_eta_ix(el_n)
C?            if (lo_tri_pv_r_n .eq. up_tri_pv_r_n) then
C?c
C?c     The matching L-and-U pivot has been found
C?c
C?               r_n = r_n + 1
C?               r_perm(lo_tri_pv_r_n) = r_n
C?               fm_lo_eta_n = lo_eta_n-1
C?               if (up_eta_n .le. l_up_eta_n) goto 170
C?            else if (lc_i_wk_a(lo_tri_pv_r_n) .eq. pv_in_lo_no_up) then
C?c
C?c     This is an L-only pivot which has not been assigned.
C?c
C?               r_n = r_n + 1
C?               r_perm(lo_tri_pv_r_n) = r_n
C?            endif
C? 195     continue
C? 200     continue
C?      endif
C?      if (r_n .ne. n_r) then
C?         if (alw_f7_wr) write(wr_cn, *)' r_n .ne. n_r ', r_n, n_r
C?         goto 7000
C?      endif
C?      pv_r_n = 0
C?      do 210, r_n = 1, n_r
C?         if (r_perm(r_n) .eq. 0) then
C?            pv_r_n = pv_r_n + 1
C?            r_perm(r_n) = pv_r_n
C?         endif
C?         lc_i_wk_a(r_perm(r_n)) = r_n
C? 210  continue
C?c      do 220, r_n = 1, n_r
C?c         print*, 'Picture row ', r_n, ' is ', lc_i_wk_a(r_n)
C?c 220  continue
C?      do 230, r_n = 1, n_r
C?         lc_i_wk_a(r_n) = 0
C? 230  continue
C?      do 240, ix_n = 1, rhs_ix(0)
C?         lc_i_wk_a(r_perm(rhs_ix(ix_n))) = 1
C? 240  continue
C?      lc_i_wk_a(0) = rhs_ix(0)
C?      if (alw_f7_wr) then
C?         ascii_mode = 1
C?c         print*, 'Enter ascii mode 1/0'
C?c         read*, ascii_mode
C?         open(unit = pic_cn, file = fi_nm)
C?         if (ascii_mode .eq. 1) then
C?            write(pic_cn, 9500)n_r, n_eta
C?         else
C?            write(pic_cn, 9501)n_r, n_eta
C?         endif
C?         if (c_eta_fi) then
C?            call eta_se_pic(ascii_mode, pic_cn,
C?     &           n_r, f_lo_eta_n, l_lo_eta_n, n_lo_eta_el,
C?     &           lo_eta_v, lo_eta_ix, lo_eta_sa,
C?     &           r_perm, lc_i_wk_a)
C?            call eta_se_pic(ascii_mode, pic_cn,
C?     &           n_r, f_up_eta_n, l_up_eta_n, n_up_eta_el,
C?     &           up_eta_v, up_eta_ix, up_eta_sa,
C?     &        r_perm, lc_i_wk_a)
C?         else
C?            call eta_se_pic(ascii_mode, pic_cn,
C?     &           n_r, f_up_eta_n, l_up_eta_n, n_up_eta_el,
C?     &           up_eta_v, up_eta_ix, up_eta_sa,
C?     &           r_perm, lc_i_wk_a)
C?            call eta_se_pic(ascii_mode, pic_cn,
C?     &           n_r, f_lo_eta_n, l_lo_eta_n, n_lo_eta_el,
C?     &           lo_eta_v, lo_eta_ix, lo_eta_sa,
C?     &           r_perm, lc_i_wk_a)
C?         endif
C?         close(pic_cn)
C?      endif
C? 7000 continue
C?      return
C? 9500 format('P3'/2i8/'8')
C? 9501 format('P6'/2i8/'255')
C?      end
C?      subroutine eta_se_pic(ascii_mode, pic_cn,
C?     &     n_r, f_eta_n, l_eta_n, n_eta_el,
C?     &     eta_v, eta_ix, eta_sa,
C?     &     r_perm, mk_r)
C?      implicit none
C?      include 'EMSV.INC'
C?      integer ascii_mode, pic_cn
C?      integer n_r, f_eta_n, l_eta_n, n_eta_el
C?      double precision eta_v(0:n_eta_el)
C?      integer eta_ix(0:n_eta_el)
C?      integer eta_sa(0:l_eta_n+1)
C?      integer r_perm(0:n_r)
C?      integer mk_r(0:n_r)
C?      logical mk_eta
C?      integer eta_n, el_n, r_n, pv_r_n
C?      logical no_mk
C? 
C?      integer no_tone
C?c      integer light_tone
C?c      integer hf_tone
C?c      integer dark_tone
C?      integer fu_tone
C?      parameter (
C?     &     no_tone =    8,
C?c     &     light_tone = 6,
C?c     &     hf_tone =    4,
C?c     &     dark_tone =  2,
C?     &     fu_tone =    6)
C?c     &     fu_tone =    0)
C? 
C?      character*1 ch1_no_tone
C?      character*1 ch1_light_tone
C?      character*1 ch1_hf_tone
C?      character*1 ch1_dark_tone
C?      character*1 ch1_fu_tone
C?      character*1 ch1_cz_tone
C? 
C?      integer inv_pic_mx_n_r
C?      parameter (inv_pic_mx_n_r = 1000)
C?      character*6 ch6_eta_c(inv_pic_mx_n_r)
C? 
C?      character*6 ch6_red
C?      character*6 ch6_green
C?      character*6 ch6_blue
C?      character*6 ch6_yellow
C?      character*6 ch6_cyan
C?      character*6 ch6_black
C?      character*6 ch6_white
C? 
C?      character*6 ch6_pv
C?      character*6 ch6_en
C?      character*6 ch6_mk_pv
C?      character*6 ch6_mk_en
C?      character*6 ch6_mk_r
C?      character*6 ch6_mk_eta
C? 
C?      character*3 ch3_eta_c(inv_pic_mx_n_r)
C? 
C?      character*3 ch3_red
C?      character*3 ch3_green
C?      character*3 ch3_blue
C?      character*3 ch3_yellow
C?      character*3 ch3_cyan
C?      character*3 ch3_black
C?      character*3 ch3_white
C? 
C?      character*3 ch3_pv
C?      character*3 ch3_en
C?      character*3 ch3_mk_pv
C?      character*3 ch3_mk_en
C?      character*3 ch3_mk_r
C?      character*3 ch3_mk_eta
C? 
C?      integer cz_tone
C? 
C?      no_mk = mk_r(0) .le. 0
C? 
C?      cz_tone = no_tone
C?c      print*, 'Enter tone for picture (full->-none:0->-8)'
C?c      read*, cz_tone
C? 
C?      if (ascii_mode .eq. 1) then
C?         write(ch6_red, 9000)   cz_tone, fu_tone, fu_tone
C?         write(ch6_green, 9000) fu_tone, cz_tone, fu_tone
C?         write(ch6_blue, 9000)  fu_tone, fu_tone, cz_tone
C?         write(ch6_yellow, 9000)cz_tone, cz_tone, fu_tone
C?         write(ch6_cyan, 9000)  fu_tone, cz_tone, cz_tone
C? 
C?         write(ch6_black, 9000) fu_tone, fu_tone, fu_tone
C?         write(ch6_white, 9000) no_tone, no_tone, no_tone
C?         if (no_mk) then
C?            ch6_pv = ch6_red
C?            ch6_en = ch6_black
C?         else
C?            ch6_pv =     ch6_green
C?            ch6_en =     ch6_blue
C?            ch6_mk_pv =  ch6_red
C?            ch6_mk_en =  ch6_black
C?            ch6_mk_r =   ch6_yellow
C?            ch6_mk_eta = ch6_cyan
C?         endif
C?      else
C?         ch1_no_tone =    char(255)
C?         ch1_light_tone = char(191)
C?         ch1_hf_tone =    char(127)
C?         ch1_dark_tone =  char(63)
C?         ch1_fu_tone =    char(0)
C?         ch1_cz_tone =    ch1_no_tone
C?         ch3_red =    ch1_cz_tone//ch1_fu_tone//ch1_fu_tone
C?         ch3_green =  ch1_fu_tone//ch1_cz_tone//ch1_fu_tone
C?         ch3_blue =   ch1_fu_tone//ch1_fu_tone//ch1_cz_tone
C?         ch3_yellow = ch1_cz_tone//ch1_cz_tone//ch1_fu_tone
C?         ch3_cyan =   ch1_fu_tone//ch1_cz_tone//ch1_cz_tone
C?         ch3_black =  ch1_fu_tone//ch1_fu_tone//ch1_fu_tone
C?         ch3_white =  ch1_no_tone//ch1_no_tone//ch1_no_tone
C?         if (no_mk) then
C?            ch3_pv = ch3_red
C?            ch3_en = ch3_black
C?         else
C?            ch3_pv =     ch3_green
C?            ch3_en =     ch3_blue
C?            ch3_mk_pv =  ch3_red
C?            ch3_mk_en =  ch3_black
C?            ch3_mk_r =   ch3_yellow
C?            ch3_mk_eta = ch3_cyan
C?         endif
C?      endif
C?      if (ascii_mode .eq. 1) then
C?         do 50, eta_n = f_eta_n, l_eta_n
C?            el_n = eta_sa(eta_n)
C?            pv_r_n = r_perm(eta_ix(el_n))
C?            mk_eta = mk_r(pv_r_n) .ne. 0
C?            if (mk_eta) then
C?               do 20, r_n = 1, n_r
C?                  if (mk_r(r_n) .eq. 0) then
C?                     ch6_eta_c(r_n) = ch6_mk_eta
C?                  else
C?                     ch6_eta_c(r_n) = ch6_mk_r
C?                  endif
C? 20            continue
C?               ch6_eta_c(pv_r_n) = ch6_mk_pv
C?            else
C?               do 30, r_n = 1, n_r
C?                  if (mk_r(r_n) .eq. 0) then
C?                     ch6_eta_c(r_n) = ch6_white
C?                  else
C?                     ch6_eta_c(r_n) = ch6_mk_r
C?                  endif
C? 30            continue
C?               ch6_eta_c(pv_r_n) = ch6_pv
C?            endif
C?            do 40, el_n = el_n+1, eta_sa(eta_n+1)-1
C?               r_n = r_perm(eta_ix(el_n))
C?               if (mk_eta) then
C?                  mk_r(r_n) = 1
C?                  ch6_eta_c(r_n) = ch6_mk_en
C?               else
C?                  ch6_eta_c(r_n) = ch6_en
C?               endif
C? 40         continue
C?            write(pic_cn, 9100)(ch6_eta_c(r_n), r_n = 1, n_r)
C? 50      continue
C?      else
C?         do 150, eta_n = f_eta_n, l_eta_n
C?            el_n = eta_sa(eta_n)
C?            pv_r_n = r_perm(eta_ix(el_n))
C?            mk_eta = mk_r(pv_r_n) .ne. 0
C?            if (mk_eta) then
C?               do 120, r_n = 1, n_r
C?                  if (mk_r(r_n) .eq. 0) then
C?                     ch3_eta_c(r_n) = ch3_mk_eta
C?                  else
C?                     ch3_eta_c(r_n) = ch3_mk_r
C?                  endif
C? 120           continue
C?               ch3_eta_c(pv_r_n) = ch3_mk_pv
C?            else
C?               do 130, r_n = 1, n_r
C?                  if (mk_r(r_n) .eq. 0) then
C?                     ch3_eta_c(r_n) = ch3_white
C?                  else
C?                     ch3_eta_c(r_n) = ch3_mk_r
C?                  endif
C? 130            continue
C?               ch3_eta_c(pv_r_n) = ch3_pv
C?            endif
C?            do 140, el_n = el_n+1, eta_sa(eta_n+1)-1
C?               r_n = r_perm(eta_ix(el_n))
C?               if (mk_eta) then
C?                  mk_r(r_n) = 1
C?                  ch3_eta_c(r_n) = ch3_mk_en
C?               else
C?                  ch3_eta_c(r_n) = ch3_en
C?               endif
C? 140        continue
C?c
C?c     !! Need to write out without carriage returns
C?c
C?            write(pic_cn, 9101)(ch3_eta_c(r_n), r_n = 1, n_r)
C? 150     continue
C?      endif
C?      return
C? 9000 format(3(1x, i1))
C? 9100 format(1000a6)
C? 9101 format(1000a3)
C?      end
CM      ENDIF
