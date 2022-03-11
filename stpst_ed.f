C->>> -----------------------------------> ems_ca_u_lg_sed_wt <<<
c     Calls the routines to updates steepest edge weights and possibly
c     update dual activities or do CHUZC.
c
      subroutine ems_ca_u_lg_sed_wt(
     &     pv_c_sgn,
     &     u_du_act,
     &     vr_in_c,
     &     du_act,
     &     pi_v,
     &     pi_ix,
     &     ed_wt,
     &     btran_o_pv_c)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICS.INC'
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      integer pv_c_sgn
      logical u_du_act
      integer vr_in_c(-vr_in_c_n_sn:n_c+1)
      double precision du_act(0:mx_n_c+n_r)
      double precision pi_v(0:n_r)
      integer pi_ix(0:n_r)
      double precision ed_wt(0:mx_n_c+n_r)
      double precision btran_o_pv_c(0:n_r)
      double precision rcp_pv
 
      if (vr_t_en_bs .le. 0 .or. vr_t_lv_bs .eq. 0 .or.
     &     vr_t_en_bs .eq. vr_t_lv_bs) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9900)
     &        vr_t_en_bs, vr_t_lv_bs
         call ems_msg_wr_li(bug_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?         call ems_dump
CM      ENDIF
         go to 7000
      endif
      ed_wt_o_vr_t_en_bs = ed_wt(vr_t_en_bs)
      ed_wt(vr_t_lv_bs) = inf
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_pc_lvl1) call ems_tt_rec(u_lg_sed_wt_tt, n_bs)
CM      ENDIF
      if (u_du_act) then
         call ems_u_lg_sed_wt_du_act(
     &        pv_c_sgn,
     &        vr_t_en_bs,
     &        vr_in_c,
     &        du_act,
     &        pi_v,
     &        pi_ix,
     &        ed_wt,
     &        btran_o_pv_c)
      else
         call ems_u_lg_sed_wt(
     &        pv_c_sgn,
     &        vr_t_en_bs,
     &        vr_in_c,
     &        pi_v,
     &        pi_ix,
     &        ed_wt,
     &        btran_o_pv_c)
      endif
      if (vr_t_lv_bs .ge. mx_n_c) then
c
c     Set the weight for the variable which has left the basis.
c
         rcp_pv = one/pv
         ed_wt(vr_t_lv_bs) = ed_wt_o_vr_t_en_bs*rcp_pv*rcp_pv
      endif
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_pc_lvl1) call ems_tt_rec(-u_lg_sed_wt_tt, n_bs)
CM      ENDIF
 7000 continue
      return
 9900 format(
     &     'Calling u_lg_sed_wt with vr_t_en_bs, vr_t_lv_bs = ',
     &     i9, i9)
      end
 
C->>> ----------------------------------------> ems_u_lg_sed_wt <<<
c     Updates steepest edge weights for logicals.
c
      subroutine ems_u_lg_sed_wt(
     &     pv_c_sgn,
     &     vr_t_en_bs,
     &     vr_in_c,
     &     pi_v,
     &     pi_ix,
     &     ed_wt,
     &     btran_o_pv_c)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
CM      IF (emsol_da .EQ. 1) THEN
C?      include 'EMSDA.INC'
CM      ENDIF
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      integer pv_c_sgn
      integer vr_t_en_bs
      integer vr_in_c(-vr_in_c_n_sn:n_c)
      integer pi_ix(0:n_r)
      double precision pi_v(0:n_r)
      double precision ed_wt(0:mx_n_c+n_r)
      double precision btran_o_pv_c(0:n_r)
      integer vr_n, c_n, ix_n, r_n
      integer c_loop_ln
      double precision ed_wt_o_vr_t_en_bs, su
 
      ed_wt_o_vr_t_en_bs = ed_wt(vr_t_en_bs)
      c_loop_ln = vr_in_c(os_lg_in_c_l_pc_p)
      if (pi_ix(0) .gt. n_r .or.
     &     tbu_r_loop_mode .eq. tbu_r_loop_no .or.
     &     (tbu_r_loop_mode .eq. tbu_r_loop_poss .and.
     &     2*c_loop_ln .le. pi_ix(0))) then
         do 10, c_n = 1, vr_in_c(os_lg_in_c_l_pc_p)
            vr_n = vr_in_c(c_n)
            r_n = vr_n - mx_n_c
CM      IF (emsol_da .EQ. 1) THEN
C?            if (pi_v(r_n) .ne. zero) then
C?               su_n_u_ed_wt_en = su_n_u_ed_wt_en + 1
C?               if (abs(pi_v(r_n)) .le. u_ed_wt_ze)
C?     &              su_n_u_ed_wt_ze = su_n_u_ed_wt_ze + 1
C?            endif
CM      ENDIF
            if (pi_v(r_n) .eq. zero) goto 10
            if (abs(pi_v(r_n)) .le. u_ed_wt_ze) goto 10
            su = pi_v(r_n)*ed_wt_o_vr_t_en_bs +
     &           pv_c_sgn*btran_o_pv_c(r_n)
            ed_wt(vr_n) = ed_wt(vr_n) + pi_v(r_n)*su
 10      continue
      else
         do 20, ix_n = 1, pi_ix(0)
            r_n = pi_ix(ix_n)
CM      IF (emsol_da .EQ. 1) THEN
C?            su_n_u_ed_wt_en = su_n_u_ed_wt_en + 1
C?            if (abs(pi_v(r_n)) .le. u_ed_wt_ze)
C?     &           su_n_u_ed_wt_ze = su_n_u_ed_wt_ze + 1
CM      ENDIF
            if (abs(pi_v(r_n)) .le. u_ed_wt_ze) goto 20
            su = pi_v(r_n)*ed_wt_o_vr_t_en_bs +
     &           pv_c_sgn*btran_o_pv_c(r_n)
            vr_n = mx_n_c + r_n
            ed_wt(vr_n) = ed_wt(vr_n) + pi_v(r_n)*su
 20      continue
      endif
      return
      end
 
C->>> -----------------------------> ems_u_lg_sed_wt_du_act <<<
c     Updates steepest edge weights and dual activities for logicals.
c
      subroutine ems_u_lg_sed_wt_du_act(
     &     pv_c_sgn,
     &     vr_t_en_bs,
     &     vr_in_c,
     &     du_act,
     &     pi_v,
     &     pi_ix,
     &     ed_wt,
     &     btran_o_pv_c)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
CM      IF (emsol_da .EQ. 1) THEN
C?      include 'EMSDA.INC'
CM      ENDIF
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      integer pv_c_sgn
      integer vr_t_en_bs
      integer vr_in_c(-vr_in_c_n_sn:n_c)
      integer pi_ix(0:n_r)
      double precision du_act(0:mx_n_c+n_r)
      double precision pi_v(0:n_r)
      double precision ed_wt(0:mx_n_c+n_r)
      double precision btran_o_pv_c(0:n_r)
      integer vr_n, c_n, ix_n, r_n
      integer c_loop_ln
      double precision pi_v_mu_1, pi_v_mu_2, su
c
c     When btran_o_pv_c is negated, negating pi_v_mu_1 then negates
c     pi_v_mu_2. Hence the non-btran_o_pv_c contribution to su is
c     negated but the product pi_v_mu_1*su has the right sign.
c
      pi_v_mu_1 = pv_c_sgn/du_act(vr_t_en_bs)
      pi_v_mu_2 = ed_wt(vr_t_en_bs)*pi_v_mu_1
c
c     Update the weights (and costs) for the nonbasic variables. This
c     does not give the correct weight for the variable which has just
c     left the basis---this is assigned at the end of the routine---but
c     it does give the correct reduced cost for this variable. This must
c     be done here. If the cost of this variable does not change then
c     its reduced cost is pi_rhs_v_in_pv_r*rcp_pv.
c     However, if the variable becomes feasible as it leaves the basis
c     then its reduced cost is not pi_rhs_v_in_pv_r*rcp_pv because
c     its objective coefficient changes---by an amount delta. In this
c     case the reduced cost is delta + pi_rhs_v_in_pv_r*rcp_pv.
c     This is achieved by the calling routine initialising the reduced
c     cost to zero or delta accordingly so that the correct updated
c     reduced cost will be obtained by adding pi_v(pv_r_n)---which is
c     rcp_pv*pi_rhs_v_in_pv_r.
c
      c_loop_ln = vr_in_c(os_lg_in_c_l_pc_p)
      if (pi_ix(0) .gt. n_r .or.
     &     tbu_r_loop_mode .eq. tbu_r_loop_no .or.
     &     (tbu_r_loop_mode .eq. tbu_r_loop_poss .and.
     &     2*c_loop_ln .le. pi_ix(0))) then
         do 10, c_n = 1, vr_in_c(os_lg_in_c_l_pc_p)
            vr_n = vr_in_c(c_n)
            r_n = vr_n - mx_n_c
CM      IF (emsol_da .EQ. 1) THEN
C?            if (pi_v(r_n) .ne. zero) then
C?               su_n_u_ed_wt_en = su_n_u_ed_wt_en + 1
C?               if (abs(pi_v(r_n)) .le. u_ed_wt_ze)
C?     &              su_n_u_ed_wt_ze = su_n_u_ed_wt_ze + 1
C?            endif
CM      ENDIF
            if (pi_v(r_n) .eq. zero) goto 10
            if (abs(pi_v(r_n)) .le. u_ed_wt_ze) goto 10
            su = pi_v(r_n)*pi_v_mu_2 + btran_o_pv_c(r_n)
            ed_wt(vr_n) = ed_wt(vr_n) + pi_v(r_n)*pi_v_mu_1*su
            du_act(vr_n) = du_act(vr_n) - pi_v(r_n)
 10      continue
      else
         do 20, ix_n = 1, pi_ix(0)
            r_n = pi_ix(ix_n)
CM      IF (emsol_da .EQ. 1) THEN
C?            su_n_u_ed_wt_en = su_n_u_ed_wt_en + 1
C?            if (abs(pi_v(r_n)) .le. u_ed_wt_ze)
C?     &           su_n_u_ed_wt_ze = su_n_u_ed_wt_ze + 1
CM      ENDIF
            if (abs(pi_v(r_n)) .le. u_ed_wt_ze) goto 20
            su = pi_v(r_n)*pi_v_mu_2 + btran_o_pv_c(r_n)
            vr_n = mx_n_c + r_n
            ed_wt(vr_n) = ed_wt(vr_n) + pi_v(r_n)*pi_v_mu_1*su
            du_act(vr_n) = du_act(vr_n) - pi_v(r_n)
 20      continue
      endif
      return
      end
 
C->>> ------------------------------> ems_perm_ca_u_lg_sed_wt <<<
c     Calls the routines to updates steepest edge weights and possibly
c     update dual activities or do CHUZC.
c
      subroutine ems_perm_ca_u_lg_sed_wt(
     &     pv_c_sgn,
     &     u_du_act,
     &     vr_in_c,
     &     du_act,
     &     pi_v,
     &     pi_ix,
     &     ed_wt,
     &     btran_o_pv_c,
     &     og_t_nw_perm,
     &     nw_t_og_perm)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICS.INC'
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      integer pv_c_sgn
      logical u_du_act
      integer vr_in_c(-vr_in_c_n_sn:n_c+1)
      integer pi_ix(0:n_r)
      integer og_t_nw_perm(0:n_r)
      integer nw_t_og_perm(0:n_r)
      double precision du_act(0:mx_n_c+n_r)
      double precision pi_v(0:n_r)
      double precision ed_wt(0:mx_n_c+n_r)
      double precision btran_o_pv_c(0:n_r)
      double precision rcp_pv
 
      if (vr_t_en_bs .le. 0 .or. vr_t_lv_bs .eq. 0 .or.
     &     vr_t_en_bs .eq. vr_t_lv_bs) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9900)
     &        vr_t_en_bs, vr_t_lv_bs
         call ems_msg_wr_li(warn_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?         call ems_dump
CM      ENDIF
         go to 7000
      endif
      ed_wt_o_vr_t_en_bs = ed_wt(vr_t_en_bs)
      ed_wt(vr_t_lv_bs) = inf
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_pc_lvl1) call ems_tt_rec(u_lg_sed_wt_tt, n_bs)
CM      ENDIF
      if (u_du_act) then
         call ems_perm_u_lg_sed_wt_du_act(
     &        pv_c_sgn,
     &        vr_t_en_bs,
     &        vr_in_c,
     &        du_act,
     &        pi_v,
     &        pi_ix,
     &        ed_wt,
     &        btran_o_pv_c,
     &        og_t_nw_perm,
     &        nw_t_og_perm)
      else
         call ems_perm_u_lg_sed_wt(
     &        pv_c_sgn,
     &        vr_t_en_bs,
     &        vr_in_c,
     &        pi_v,
     &        pi_ix,
     &        ed_wt,
     &        btran_o_pv_c,
     &        og_t_nw_perm,
     &        nw_t_og_perm)
      endif
      if (vr_t_lv_bs .ge. mx_n_c) then
c
c     Set the weight for the variable which has left the basis.
c
         rcp_pv = one/pv
         ed_wt(vr_t_lv_bs) = ed_wt_o_vr_t_en_bs*rcp_pv*rcp_pv
      endif
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_pc_lvl1) call ems_tt_rec(-u_lg_sed_wt_tt, n_bs)
CM      ENDIF
 7000 continue
      return
 9900 format(
     &     'Calling u_lg_sed_wt with vr_t_en_bs, vr_t_lv_bs = ',
     &     i9, i9)
      end
 
C->>> -----------------------------------> ems_perm_u_lg_sed_wt <<<
c     Updates steepest edge weights for logicals.
c
      subroutine ems_perm_u_lg_sed_wt(
     &     pv_c_sgn,
     &     vr_t_en_bs,
     &     vr_in_c,
     &     pi_v,
     &     pi_ix,
     &     ed_wt,
     &     btran_o_pv_c,
     &     og_t_nw_perm,
     &     nw_t_og_perm)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
CM      IF (emsol_da .EQ. 1) THEN
C?      include 'EMSDA.INC'
CM      ENDIF
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      integer pv_c_sgn
      integer vr_t_en_bs
      integer vr_in_c(-vr_in_c_n_sn:n_c)
      integer pi_ix(0:n_r)
      integer og_t_nw_perm(0:n_r)
      integer nw_t_og_perm(0:n_r)
      double precision pi_v(0:n_r)
      double precision ed_wt(0:mx_n_c+n_r)
      double precision btran_o_pv_c(0:n_r)
      integer vr_n, nw_r_n, c_n, ix_n, r_n, og_r_n
      integer c_loop_ln
      double precision ed_wt_o_vr_t_en_bs, su
 
      ed_wt_o_vr_t_en_bs = ed_wt(vr_t_en_bs)
      c_loop_ln = vr_in_c(os_lg_in_c_l_pc_p)
      if (pi_ix(0) .gt. n_r .or.
     &     tbu_r_loop_mode .eq. tbu_r_loop_no .or.
     &     (tbu_r_loop_mode .eq. tbu_r_loop_poss .and.
     &     2*c_loop_ln .le. pi_ix(0))) then
         do 10, c_n = 1, vr_in_c(os_lg_in_c_l_pc_p)
            vr_n = vr_in_c(c_n)
            r_n = vr_n - mx_n_c
            nw_r_n = og_t_nw_perm(r_n)
CM      IF (emsol_da .EQ. 1) THEN
C?            if (pi_v(nw_r_n) .ne. zero) then
C?               su_n_u_ed_wt_en = su_n_u_ed_wt_en + 1
C?               if (abs(pi_v(nw_r_n)) .le. u_ed_wt_ze)
C?     &              su_n_u_ed_wt_ze = su_n_u_ed_wt_ze + 1
C?            endif
CM      ENDIF
            if (pi_v(nw_r_n) .eq. zero) goto 10
            if (abs(pi_v(nw_r_n)) .le. u_ed_wt_ze) goto 10
            su = pi_v(nw_r_n)*ed_wt_o_vr_t_en_bs +
     &           pv_c_sgn*btran_o_pv_c(r_n)
            ed_wt(vr_n) = ed_wt(vr_n) + pi_v(nw_r_n)*su
 10      continue
      else
         do 20, ix_n = 1, pi_ix(0)
            r_n = pi_ix(ix_n)
            og_r_n = nw_t_og_perm(r_n)
CM      IF (emsol_da .EQ. 1) THEN
C?            su_n_u_ed_wt_en = su_n_u_ed_wt_en + 1
C?            if (abs(pi_v(nw_r_n)) .le. u_ed_wt_ze)
C?     &           su_n_u_ed_wt_ze = su_n_u_ed_wt_ze + 1
CM      ENDIF
            if (abs(pi_v(r_n)) .le. u_ed_wt_ze) goto 20
            su = pi_v(r_n)*ed_wt_o_vr_t_en_bs +
     &           pv_c_sgn*btran_o_pv_c(og_r_n)
            vr_n = mx_n_c + og_r_n
            ed_wt(vr_n) = ed_wt(vr_n) + pi_v(r_n)*su
 20      continue
      endif
      return
      end
 
C->>> ------------------------> ems_perm_u_lg_sed_wt_du_act <<<
c     Updates steepest edge weights and dual activities for logicals.
c
      subroutine ems_perm_u_lg_sed_wt_du_act(
     &     pv_c_sgn,
     &     vr_t_en_bs,
     &     vr_in_c,
     &     du_act,
     &     pi_v,
     &     pi_ix,
     &     ed_wt,
     &     btran_o_pv_c,
     &     og_t_nw_perm,
     &     nw_t_og_perm)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
CM      IF (emsol_da .EQ. 1) THEN
C?      include 'EMSDA.INC'
CM      ENDIF
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      integer pv_c_sgn
      integer vr_t_en_bs
      integer vr_in_c(-vr_in_c_n_sn:n_c)
      integer pi_ix(0:n_r)
      integer og_t_nw_perm(0:n_r)
      integer nw_t_og_perm(0:n_r)
      double precision du_act(0:mx_n_c+n_r)
      double precision pi_v(0:n_r)
      double precision ed_wt(0:mx_n_c+n_r)
      double precision btran_o_pv_c(0:n_r)
      integer vr_n, nw_r_n, c_n, ix_n, r_n, og_r_n
      integer c_loop_ln
      double precision pi_v_mu_1, pi_v_mu_2, su
 
      pi_v_mu_1 = pv_c_sgn/du_act(vr_t_en_bs)
      pi_v_mu_2 = ed_wt(vr_t_en_bs)*pi_v_mu_1
      c_loop_ln = vr_in_c(os_lg_in_c_l_pc_p)
      if (pi_ix(0) .gt. n_r .or.
     &     tbu_r_loop_mode .eq. tbu_r_loop_no .or.
     &     (tbu_r_loop_mode .eq. tbu_r_loop_poss .and.
     &     2*c_loop_ln .le. pi_ix(0))) then
         do 10 c_n = 1, vr_in_c(os_lg_in_c_l_pc_p)
            vr_n = vr_in_c(c_n)
            r_n = vr_n - mx_n_c
            nw_r_n = og_t_nw_perm(r_n)
CM      IF (emsol_da .EQ. 1) THEN
C?            if (pi_v(nw_r_n) .ne. zero) then
C?               su_n_u_ed_wt_en = su_n_u_ed_wt_en + 1
C?               if (abs(pi_v(nw_r_n)) .le. u_ed_wt_ze)
C?     &              su_n_u_ed_wt_ze = su_n_u_ed_wt_ze + 1
C?            endif
CM      ENDIF
            if (pi_v(nw_r_n) .eq. zero) goto 10
            if (abs(pi_v(nw_r_n)) .le. u_ed_wt_ze) goto 10
            su = pi_v(nw_r_n)*pi_v_mu_2 + btran_o_pv_c(r_n)
            ed_wt(vr_n) = ed_wt(vr_n) + pi_v(nw_r_n)*pi_v_mu_1*su
            du_act(vr_n) = du_act(vr_n) - pi_v(nw_r_n)
 10      continue
      else
         do 20, ix_n = 1, pi_ix(0)
            r_n = pi_ix(ix_n)
            og_r_n = nw_t_og_perm(r_n)
CM      IF (emsol_da .EQ. 1) THEN
C?            su_n_u_ed_wt_en = su_n_u_ed_wt_en + 1
C?            if (abs(pi_v(nw_r_n)) .le. u_ed_wt_ze)
C?     &           su_n_u_ed_wt_ze = su_n_u_ed_wt_ze + 1
CM      ENDIF
            if (abs(pi_v(r_n)) .le. u_ed_wt_ze) goto 20
            su = pi_v(r_n)*pi_v_mu_2 + btran_o_pv_c(og_r_n)
            vr_n = mx_n_c + og_r_n
            ed_wt(vr_n) = ed_wt(vr_n) + pi_v(r_n)*pi_v_mu_1*su
            du_act(vr_n) = du_act(vr_n) - pi_v(r_n)
 20      continue
      endif
      return
      end
 
C->>> ---------------------------------------> ems_ca_u_struc_sed_wt <<<
c     Calls the routines to updates steepest edge weights and possibly
c     update dual activities or do CHUZC.
c
      subroutine ems_ca_u_struc_sed_wt(
     &     pv_c_sgn,
     &     u_du_act,
     &     vr_in_c,
     &     du_act,
     &     tbu_r_v,
     &     tbu_r_ix,
     &     ed_wt,
     &     r_v,
     &     r_ix,
     &     c_sa,
     &     btran_o_pv_c)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICS.INC'
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      integer pv_c_sgn
      logical u_du_act
      integer vr_in_c(-vr_in_c_n_sn:n_c+1)
      integer r_ix(0:n_a_el), c_sa(0:n_c+1)
      double precision du_act(0:mx_n_c+n_r)
      double precision tbu_r_v(0:mx_n_c+n_r)
      integer tbu_r_ix(0:n_c)
      double precision ed_wt(0:mx_n_c+n_r)
      double precision r_v(0:n_a_el)
      double precision btran_o_pv_c(0:n_r)
      double precision rcp_pv
 
      integer struc_in_c_l_pc_p_p1, sv_vr_in_c, r_n
      double precision sv_tbu_r_v
 
      if (vr_t_en_bs .le. 0 .or. vr_t_lv_bs .eq. 0 .or.
     &     vr_t_en_bs .eq. vr_t_lv_bs) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9900)
     &        vr_t_en_bs, vr_t_lv_bs
         call ems_msg_wr_li(warn_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?         call ems_dump
CM      ENDIF
         go to 7000
      endif
      ed_wt_o_vr_t_en_bs = ed_wt(vr_t_en_bs)
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_pc_lvl1) call ems_tt_rec(u_struc_sed_wt_tt, n_bs)
CM      ENDIF
      if (u_du_act) then
         if (iand(asm_msk, asm_u_sed_wt) .ne. 0) then
            struc_in_c_l_pc_p_p1 = vr_in_c(os_struc_in_c_l_pc_p) + 1
            sv_vr_in_c = vr_in_c(struc_in_c_l_pc_p_p1)
            vr_in_c(struc_in_c_l_pc_p_p1) = 0
            sv_tbu_r_v = tbu_r_v(0)
            tbu_r_v(0) = 1.0d0
CM      IF (emsol_asm .EQ. 1) THEN
C?            call ems_core_u_struc_sed_wt(
C?     &           pv_c_sgn,
C?     &           vr_t_en_bs,
C?     &           vr_in_c,
C?     &           du_act,
C?     &           tbu_r_v,
C?     &           ed_wt,
C?     &           r_v,
C?     &           r_ix,
C?     &           c_sa,
C?     &           btran_o_pv_c)
CM      ELSE
            call ems_u_struc_sed_wt_du_act(
     &           pv_c_sgn,
     &           vr_in_c,
     &           du_act,
     &           tbu_r_v,
     &           tbu_r_ix,
     &           ed_wt,
     &           r_v,
     &           r_ix,
     &           c_sa,
     &           btran_o_pv_c)
CM      ENDIF
            vr_in_c(struc_in_c_l_pc_p_p1) = sv_vr_in_c
            tbu_r_v(0) = sv_tbu_r_v
         else
            call ems_u_struc_sed_wt_du_act(
     &           pv_c_sgn,
     &           vr_in_c,
     &           du_act,
     &           tbu_r_v,
     &           tbu_r_ix,
     &           ed_wt,
     &           r_v,
     &           r_ix,
     &           c_sa,
     &           btran_o_pv_c)
         endif
      else
         call ems_u_struc_sed_wt(
     &        pv_c_sgn,
     &        vr_in_c,
     &        tbu_r_v,
     &        tbu_r_ix,
     &        ed_wt,
     &        r_v,
     &        r_ix,
     &        c_sa,
     &        btran_o_pv_c)
      endif
      if (vr_t_lv_bs .le. n_c) then
c
c     Set the weight for the variable which has left the basis.
c
         rcp_pv = one/pv
         ed_wt(vr_t_lv_bs) = ed_wt_o_vr_t_en_bs*rcp_pv*rcp_pv
      endif
c
c     Zero the vector which held the BTRANned pivotal column.
c
      do 10, r_n = 1, n_r
         btran_o_pv_c(r_n) = zero
 10   continue
      if (iand(ck_msk, ze_a_ck_bt) .ne. 0)
     &     call ems_ck_ze_rl_a(n_c, tbu_r_v)
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_pc_lvl1) call ems_tt_rec(-u_struc_sed_wt_tt, n_bs)
CM      ENDIF
 7000 continue
      return
 9900 format(
     &     'Calling u_struc_sed_wt with vr_t_en_bs, vr_t_lv_bs = ',
     &     i9, i9)
      end
 
C->>> -------------------------------------> ems_u_struc_sed_wt <<<
c     Updates steepest edge weights for structurals.
c
      subroutine ems_u_struc_sed_wt(
     &     pv_c_sgn,
     &     vr_in_c,
     &     tbu_r_v,
     &     tbu_r_ix,
     &     ed_wt,
     &     r_v,
     &     r_ix,
     &     c_sa,
     &     btran_o_pv_c)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
CM      IF (emsol_da .EQ. 1) THEN
C?      include 'EMSDA.INC'
CM      ENDIF
      include 'ICTVR.INC'
      include 'RSMICOM.INC'
      include 'RLCTVR.INC'
      integer pv_c_sgn
      integer vr_in_c(-vr_in_c_n_sn:n_c)
      integer r_ix(0:n_a_el), c_sa(0:n_c+1)
      double precision tbu_r_v(0:mx_n_c+n_r)
      integer tbu_r_ix(0:n_c)
      double precision ed_wt(0:mx_n_c+n_r)
      double precision r_v(0:n_a_el)
      double precision btran_o_pv_c(0:n_r)
      integer c_loop_ln, vr_n, c_n, el_n, ix_n
      double precision su
      integer tbu_r_n_nz, tbu_r_n_c
 
      ed_wt_o_vr_t_en_bs = ed_wt(vr_t_en_bs)
      c_loop_ln =
     &     vr_in_c(os_struc_in_c_l_pc_p) -
     &     vr_in_c(os_struc_in_c_f_p_m1)
      tbu_r_n_c = c_loop_ln
      if (tbu_r_ix(0) .gt. n_c .or.
     &     tbu_r_loop_mode .eq. tbu_r_loop_no .or.
     &     (tbu_r_loop_mode .eq. tbu_r_loop_poss .and.
     &     2*c_loop_ln .le. tbu_r_ix(0))) then
         tbu_r_n_nz = 0
         do 20 c_n = vr_in_c(os_struc_in_c_f_p_m1) + 1,
     &        vr_in_c(os_struc_in_c_l_pc_p)
            vr_n = vr_in_c(c_n)
CM      IF (emsol_da .EQ. 1) THEN
C?            if (tbu_r_v(vr_n) .ne. zero) then
C?               su_n_u_ed_wt_en = su_n_u_ed_wt_en + 1
C?               if (abs(tbu_r_v(vr_n)) .le. u_ed_wt_ze)
C?     &              su_n_u_ed_wt_ze = su_n_u_ed_wt_ze + 1
C?            endif
CM      ENDIF
            if (tbu_r_v(vr_n) .eq. zero) goto 20
            tbu_r_n_nz = tbu_r_n_nz + 1
            if (abs(tbu_r_v(vr_n)) .le. u_ed_wt_ze) goto 15
            su = pv_c_sgn*tbu_r_v(vr_n)*ed_wt_o_vr_t_en_bs
            do 10 el_n = c_sa(vr_n), c_sa(vr_n+1)-1
               su = su + r_v(el_n)*btran_o_pv_c(r_ix(el_n))
 10         continue
            ed_wt(vr_n) = ed_wt(vr_n) + tbu_r_v(vr_n)*pv_c_sgn*su
 15         continue
            tbu_r_v(vr_n) = zero
 20      continue
      else
         tbu_r_n_nz = tbu_r_ix(0)
         do 120, ix_n = 1, tbu_r_ix(0)
            vr_n = tbu_r_ix(ix_n)
CM      IF (emsol_da .EQ. 1) THEN
C?            if (tbu_r_v(vr_n) .ne. zero) then
C?               su_n_u_ed_wt_en = su_n_u_ed_wt_en + 1
C?               if (abs(tbu_r_v(vr_n)) .le. u_ed_wt_ze)
C?     &              su_n_u_ed_wt_ze = su_n_u_ed_wt_ze + 1
C?            endif
CM      ENDIF
            if (tbu_r_v(vr_n) .eq. zero) goto 120
            if (abs(tbu_r_v(vr_n)) .le. u_ed_wt_ze) goto 115
            su = pv_c_sgn*tbu_r_v(vr_n)*ed_wt_o_vr_t_en_bs
            do 110 el_n = c_sa(vr_n), c_sa(vr_n+1)-1
               su = su + r_v(el_n)*btran_o_pv_c(r_ix(el_n))
 110        continue
            ed_wt(vr_n) = ed_wt(vr_n) + tbu_r_v(vr_n)*pv_c_sgn*su
 115        continue
            tbu_r_v(vr_n) = zero
 120     continue
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
      return
      end
 
C->>> --------------------------> ems_u_struc_sed_wt_du_act <<<
c     Updates steepest edge weights and dual activities for structurals.
c
      subroutine ems_u_struc_sed_wt_du_act(
     &     pv_c_sgn,
     &     vr_in_c,
     &     du_act,
     &     tbu_r_v,
     &     tbu_r_ix,
     &     ed_wt,
     &     r_v,
     &     r_ix,
     &     c_sa,
     &     btran_o_pv_c)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
CM      IF (emsol_da .EQ. 1) THEN
C?      include 'EMSDA.INC'
CM      ENDIF
      include 'ICTVR.INC'
      include 'RSMICOM.INC'
      include 'RLCTVR.INC'
      integer pv_c_sgn
      integer vr_in_c(-vr_in_c_n_sn:n_c)
      integer r_ix(0:n_a_el), c_sa(0:n_c+1)
      double precision du_act(0:mx_n_c+n_r)
      double precision tbu_r_v(0:mx_n_c+n_r)
      integer tbu_r_ix(0:n_c)
      double precision ed_wt(0:mx_n_c+n_r)
      double precision r_v(0:n_a_el)
      double precision btran_o_pv_c(0:n_r)
      integer c_loop_ln, vr_n, c_n, el_n, ix_n
      double precision tbu_r_v_mu_1, tbu_r_v_mu_2, su
      integer tbu_r_n_nz, tbu_r_n_c
 
      tbu_r_v_mu_1 = pv_c_sgn/du_act(vr_t_en_bs)
      tbu_r_v_mu_2 = ed_wt(vr_t_en_bs)*tbu_r_v_mu_1
      c_loop_ln =
     &     vr_in_c(os_struc_in_c_l_pc_p) -
     &     vr_in_c(os_struc_in_c_f_p_m1)
      tbu_r_n_c = c_loop_ln
      if (tbu_r_ix(0) .gt. n_c .or.
     &     tbu_r_loop_mode .eq. tbu_r_loop_no .or.
     &     (tbu_r_loop_mode .eq. tbu_r_loop_poss .and.
     &     2*c_loop_ln .le. tbu_r_ix(0))) then
         tbu_r_n_nz = 0
         do 20 c_n = vr_in_c(os_struc_in_c_f_p_m1) + 1,
     &        vr_in_c(os_struc_in_c_l_pc_p)
            vr_n = vr_in_c(c_n)
CM      IF (emsol_da .EQ. 1) THEN
C?            if (tbu_r_v(vr_n) .ne. zero) then
C?               su_n_u_ed_wt_en = su_n_u_ed_wt_en + 1
C?               if (abs(tbu_r_v(vr_n)) .le. u_ed_wt_ze)
C?     &              su_n_u_ed_wt_ze = su_n_u_ed_wt_ze + 1
C?            endif
CM      ENDIF
            if (tbu_r_v(vr_n) .eq. zero) goto 20
            tbu_r_n_nz = tbu_r_n_nz + 1
            if (abs(tbu_r_v(vr_n)) .le. u_ed_wt_ze) goto 15
            su = tbu_r_v(vr_n)*tbu_r_v_mu_2
            do 10 el_n = c_sa(vr_n), c_sa(vr_n+1)-1
               su = su + r_v(el_n)*btran_o_pv_c(r_ix(el_n))
 10         continue
            ed_wt(vr_n) = ed_wt(vr_n) + tbu_r_v(vr_n)*tbu_r_v_mu_1*su
            du_act(vr_n) = du_act(vr_n) + tbu_r_v(vr_n)
 15         continue
            tbu_r_v(vr_n) = zero
 20      continue
      else
         tbu_r_n_nz = tbu_r_ix(0)
         do 120, ix_n = 1, tbu_r_ix(0)
            vr_n = tbu_r_ix(ix_n)
CM      IF (emsol_da .EQ. 1) THEN
C?            if (tbu_r_v(vr_n) .ne. zero) then
C?               su_n_u_ed_wt_en = su_n_u_ed_wt_en + 1
C?               if (abs(tbu_r_v(vr_n)) .le. u_ed_wt_ze)
C?     &              su_n_u_ed_wt_ze = su_n_u_ed_wt_ze + 1
C?            endif
CM      ENDIF
            if (tbu_r_v(vr_n) .eq. zero) goto 120
            if (abs(tbu_r_v(vr_n)) .le. u_ed_wt_ze) goto 115
            su = tbu_r_v(vr_n)*tbu_r_v_mu_2
            do 110 el_n = c_sa(vr_n), c_sa(vr_n+1)-1
               su = su + r_v(el_n)*btran_o_pv_c(r_ix(el_n))
 110        continue
            ed_wt(vr_n) = ed_wt(vr_n) + tbu_r_v(vr_n)*tbu_r_v_mu_1*su
            du_act(vr_n) = du_act(vr_n) + tbu_r_v(vr_n)
 115        continue
            tbu_r_v(vr_n) = zero
 120     continue
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
      return
      end
 
C->>> --------------------------------------------> ems_iz_lg_sed_wt <<<
c     Initialises the steepest edge weights (as if) for a logical basis.
c
      subroutine ems_iz_lg_sed_wt(
     &     vr_in_r, vr_in_c,
     &     r_v, c_sa, ed_wt)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer vr_in_r(0:n_r)
      integer vr_in_c(-vr_in_c_n_sn:n_c)
      integer c_sa(0:n_c+1)
      double precision r_v(0:n_a_el), ed_wt(0:mx_n_c+n_r)
      integer r_n, c_n, vr_n, el_n
      logical ed_wt_er
 
      ed_wt_er = .false.
      do 5, r_n = 1, n_r
         vr_n = vr_in_r(r_n)
         ed_wt(vr_n) = zero
         if (vr_n .le. n_c) ed_wt_er = .true.
 5    continue
      if (ed_wt_er) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
         call ems_msg_wr_li(warn_msg_n)
      endif
      do 10, c_n = 1, vr_in_c(os_lg_in_c_l_pc_p)
         vr_n = vr_in_c(c_n)
         ed_wt(vr_n) = one
c         ed_wt(vr_n) = half
c
c     Surely wrong: corrected by JAJH 13/02/97: Error not noticed since
c     logical basis means no logicals in pricing list
c
 10   continue
      do 30, c_n = vr_in_c(os_struc_in_c_f_p_m1) + 1,
     &     vr_in_c(os_struc_in_c_l_pc_p)
         vr_n = vr_in_c(c_n)
         ed_wt(vr_n) = one
         do 20 el_n = c_sa(vr_n), c_sa(vr_n+1)-1
            ed_wt(vr_n) = ed_wt(vr_n) + r_v(el_n)*r_v(el_n)
 20      continue
         ed_wt(vr_n) = ed_wt(vr_n)*half
 30   continue
c
c     Indicate that the edge weight information is correct.
c
      ml_da_st_msk = ior(ml_da_st_msk, ml_da_st_ed_wt)
      return
 9000 format('Initialising steepest edge weights for a',
     &     ' non-logical basis as if it were logical')
      end
 
C->>> ------------------------------------------> ems_iz_sed_wt <<<
c     Initialises the steepest edge weights for a general basis.
c
      subroutine ems_iz_sed_wt(
     &     vr_in_r, vr_in_c,
     &     r_v, c_sa, ed_wt, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer vr_in_r(0:n_r)
      integer vr_in_c(-vr_in_c_n_sn:n_c)
      integer c_sa(0:n_c+1)
      integer is(0:is_n_en_m1)
      double precision r_v(0:n_a_el), ed_wt(0:mx_n_c+n_r)
      double precision ds(0:ds_n_en_m1)
      double precision ems_scpr
      integer r_n, c_n, vr_n
      integer rl_wk_a_ix
      integer i_wk_a_ix
c
c     Check whether the basis is actually logical.
c
      do 5, r_n = 1, n_r
         vr_n = vr_in_r(r_n)
         ed_wt(vr_n) = zero
         if (vr_n .le. n_c) goto 10
 5    continue
c
c     The basis is logical so calculate the weights from the constraint
c     matrix columns.
c
      call ems_iz_lg_sed_wt(vr_in_r, vr_in_c, r_v, c_sa, ed_wt)
      goto 7000
 10   continue
c
c     Initialising steepest edge weights is expensive!!
c
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
      call ems_msg_wr_li(info_msg_n)
      call ems_g_rsmi_rl_wk_a_ix(rl_wk_a_ix)
      if (rl_wk_a_ix .lt. 0) goto 8000
      call ems_g_rsmi_i_wk_a_ix(i_wk_a_ix)
      if (i_wk_a_ix .lt. 0) goto 8000
      do 20, c_n = 1, vr_in_c(os_lg_in_c_l_pc_p)
         vr_n = vr_in_c(c_n)
         do 15, r_n = 1, n_r
            ds(p_rsmi_rl_wk_a(rl_wk_a_ix)+r_n) = zero
 15      continue
         if (sto_ftran_ix .eq. sto_ix_y) then
            is(p_rsmi_i_wk_a(i_wk_a_ix)) = 0
         else
            is(p_rsmi_i_wk_a(i_wk_a_ix)) = n_r+1
         endif
         call ems_g_rhs(1, vr_n,
     &        ds(p_rsmi_rl_wk_a(rl_wk_a_ix)),
     &        is(p_rsmi_i_wk_a(i_wk_a_ix)),
     &        ds, is)
         call ems_ftran(
     &        ds(p_rsmi_rl_wk_a(rl_wk_a_ix)),
     &        is(p_rsmi_i_wk_a(i_wk_a_ix)),
     &        ds, is)
         ed_wt(vr_n) = ems_scpr(one,
     &        ds(p_rsmi_rl_wk_a(rl_wk_a_ix)),
     &        ds(p_rsmi_rl_wk_a(rl_wk_a_ix)), n_r)*half
 20   continue
      do 30, c_n = vr_in_c(os_struc_in_c_f_p_m1) + 1,
     &     vr_in_c(os_struc_in_c_l_pc_p)
         vr_n = vr_in_c(c_n)
         do 25, r_n = 1, n_r
            ds(p_rsmi_rl_wk_a(rl_wk_a_ix)+r_n) = zero
 25      continue
         if (sto_ftran_ix .eq. sto_ix_y) then
            is(p_rsmi_i_wk_a(i_wk_a_ix)) = 0
         else
            is(p_rsmi_i_wk_a(i_wk_a_ix)) = n_r+1
         endif
         call ems_g_rhs(1, vr_n,
     &        ds(p_rsmi_rl_wk_a(rl_wk_a_ix)),
     &        is(p_rsmi_i_wk_a(i_wk_a_ix)),
     &        ds, is)
         call ems_ftran(
     &        ds(p_rsmi_rl_wk_a(rl_wk_a_ix)),
     &        is(p_rsmi_i_wk_a(i_wk_a_ix)),
     &        ds, is)
         ed_wt(vr_n) = ems_scpr(one,
     &        ds(p_rsmi_rl_wk_a(rl_wk_a_ix)),
     &        ds(p_rsmi_rl_wk_a(rl_wk_a_ix)), n_r)*half
 30   continue
      call ems_fr_rsmi_rl_wk_a_ix(rl_wk_a_ix)
      call ems_fr_rsmi_i_wk_a_ix(i_wk_a_ix)
c
c     Indicate that the edge weight information is correct.
c
      ml_da_st_msk = ior(ml_da_st_msk, ml_da_st_ed_wt)
 7000 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9000 format('Initialising steepest edge weights for a non-logical',
     &     ' basis is expensive')
 9800 format('RSMI workspace not available in ems_iz_sed_wt')
      end
