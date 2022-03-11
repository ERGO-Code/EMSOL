C->>> ------------------------------------------> ems_ca_u_lg_dvx_wt <<<
c     Calls the routines to update Devex weights and possibly
c     dual activities.
c
      subroutine ems_ca_u_lg_dvx_wt(
     &     u_du_act,
     &     vr_in_c,
     &     du_act,
     &     pi_v,
     &     pi_ix,
     &     ed_wt)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICS.INC'
      include 'RSMICOM.INC'
CM      IF (emsol_da .EQ. 1) THEN
C?      include 'EMSDA.INC'
CM      ENDIF
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      logical u_du_act
      integer vr_in_c(-vr_in_c_n_sn:n_c)
      integer pi_ix(0:n_r)
      double precision du_act(0:mx_n_c+n_r)
      double precision pi_v(0:n_r)
      double precision ed_wt(0:mx_n_c+n_r)
 
      if (vr_t_en_bs .le. 0 .or. vr_t_lv_bs .eq. 0 .or.
     &     vr_t_en_bs .eq. vr_t_lv_bs) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9900)
     &        vr_t_en_bs, vr_t_lv_bs
         call ems_msg_wr_li(warn_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?         call ems_dump
CM      ENDIF
         go to 7000
      end if
      if (u_du_act) then
         call ems_u_lg_dvx_wt_du_act(
     &        vr_t_en_bs,
     &        vr_in_c,
     &        du_act,
     &        pi_v,
     &        pi_ix,
     &        ed_wt)
      else
         call ems_u_lg_dvx_wt(
     &        vr_t_en_bs,
     &        vr_in_c,
     &        pi_v,
     &        pi_ix,
     &        ed_wt)
      endif
      if (vr_t_lv_bs .gt. mx_n_c) then
c
c     Set the weight for the variable which has just left the basis.
c
         ed_wt(vr_t_lv_bs) = max(one, ed_wt(vr_t_en_bs)/abs(pv))
      endif
 7000 continue
      return
 9900 format('Calling u_lg_dvx_wt with vr_t_en_bs, vr_t_lv_bs = ',
     &     i9, i9)
      end
 
C->>> ---------------------------------------------> ems_u_lg_dvx_wt <<<
c     Updates Devex weights for logicals.
c
      subroutine ems_u_lg_dvx_wt(
     &     vr_t_en_bs,
     &     vr_in_c,
     &     pi_v,
     &     pi_ix,
     &     ed_wt)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
CM      IF (emsol_da .EQ. 1) THEN
C?      include 'EMSDA.INC'
CM      ENDIF
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      include 'ICTVR.INC'
      integer vr_t_en_bs, vr_in_c(-vr_in_c_n_sn:n_c)
      integer pi_ix(0:n_r)
      double precision pi_v(0:n_r)
      double precision ed_wt(0:mx_n_c+n_r)
      integer c_n, vr_n, ix_n, r_n
      integer c_loop_ln
      double precision pi_v_mu
 
      pi_v_mu = ed_wt(vr_t_en_bs)
      c_loop_ln = vr_in_c(os_lg_in_c_l_pc_p)
      if (pi_ix(0) .gt. n_r .or.
     &     tbu_r_loop_mode .eq. tbu_r_loop_no .or.
     &     (tbu_r_loop_mode .eq. tbu_r_loop_poss .and.
     &     2*c_loop_ln .le. pi_ix(0))) then
c
c     Update the weights for the nonbasic logicals. See
c     subroutine ems_u_sed_wt for comments on obtaining the
c     reduced cost of the variable which has just left the basis.
c
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl1) call ems_tt_rec(u_lg_dvx_wt_dse_tt, n_bs)
CM      ENDIF
         do 10 c_n = 1, vr_in_c(os_lg_in_c_l_pc_p)
            vr_n = vr_in_c(c_n)
            if (pi_v(vr_n-mx_n_c) .ne. zero) ed_wt(vr_n) =
     &           max(ed_wt(vr_n), abs(pi_v(vr_n-mx_n_c))*pi_v_mu)
 10      continue
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl1) call ems_tt_rec(-u_lg_dvx_wt_dse_tt, n_bs)
CM      ENDIF
      else
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl1) call ems_tt_rec(u_lg_dvx_wt_sps_tt, n_bs)
CM      ENDIF
         do 20, ix_n = 1, pi_ix(0)
            r_n = pi_ix(ix_n)
            vr_n = mx_n_c + r_n
            ed_wt(vr_n) = max(ed_wt(vr_n), abs(pi_v(r_n))*pi_v_mu)
 20      continue
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl1) call ems_tt_rec(-u_lg_dvx_wt_sps_tt, n_bs)
CM      ENDIF
      endif
      return
      end
 
C->>> --------------------------------------> ems_u_lg_dvx_wt_du_act <<<
c     Updates Devex weights and dual activities for logicals.
c
      subroutine ems_u_lg_dvx_wt_du_act(
     &     vr_t_en_bs,
     &     vr_in_c,
     &     du_act,
     &     pi_v,
     &     pi_ix,
     &     ed_wt)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
CM      IF (emsol_da .EQ. 1) THEN
C?      include 'EMSDA.INC'
CM      ENDIF
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      include 'ICTVR.INC'
      integer vr_t_en_bs, vr_in_c(-vr_in_c_n_sn:n_c)
      integer pi_ix(0:n_r)
      double precision du_act(0:mx_n_c+n_r)
      double precision pi_v(0:n_r)
      double precision ed_wt(0:mx_n_c+n_r)
      integer c_n, vr_n, ix_n, r_n
      integer c_loop_ln
      double precision pi_v_mu
 
      pi_v_mu = ed_wt(vr_t_en_bs)/abs(du_act(vr_t_en_bs))
      c_loop_ln = vr_in_c(os_lg_in_c_l_pc_p)
      if (pi_ix(0) .gt. n_r .or.
     &     tbu_r_loop_mode .eq. tbu_r_loop_no .or.
     &     (tbu_r_loop_mode .eq. tbu_r_loop_poss .and.
     &     2*c_loop_ln .le. pi_ix(0))) then
c
c     Update the weights and dual activities for the nonbasic variables.
c     See subroutine ems_u_sed_wt for comments on obtaining the
c     reduced cost of the variable which has just left the basis.
c
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl1) call ems_tt_rec(u_lg_dvx_wt_dse_tt, n_bs)
CM      ENDIF
         do 10 c_n = 1, vr_in_c(os_lg_in_c_l_pc_p)
            vr_n = vr_in_c(c_n)
            if (pi_v(vr_n-mx_n_c) .ne. zero) then
               ed_wt(vr_n) =
     &              max(ed_wt(vr_n), abs(pi_v(vr_n-mx_n_c))*pi_v_mu)
               du_act(vr_n) = du_act(vr_n) - pi_v(vr_n-mx_n_c)
            end if
 10      continue
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl1) call ems_tt_rec(-u_lg_dvx_wt_dse_tt, n_bs)
CM      ENDIF
      else
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl1) call ems_tt_rec(u_lg_dvx_wt_sps_tt, n_bs)
CM      ENDIF
         do 20, ix_n = 1, pi_ix(0)
            r_n = pi_ix(ix_n)
            vr_n = mx_n_c + r_n
            ed_wt(vr_n) = max(ed_wt(vr_n), abs(pi_v(r_n))*pi_v_mu)
            du_act(vr_n) = du_act(vr_n) - pi_v(r_n)
 20      continue
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl1) call ems_tt_rec(-u_lg_dvx_wt_sps_tt, n_bs)
CM      ENDIF
      endif
      return
      end
 
C->>> -------------------------------------> ems_perm_ca_u_lg_dvx_wt <<<
c     Calls the routines to update Devex weights and possibly
c     dual activities.
c
      subroutine ems_perm_ca_u_lg_dvx_wt(
     &     u_du_act,
     &     vr_in_c,
     &     du_act,
     &     pi_v,
     &     pi_ix,
     &     ed_wt,
     &     og_t_nw_perm,
     &     nw_t_og_perm)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICS.INC'
      include 'RSMICOM.INC'
CM      IF (emsol_da .EQ. 1) THEN
C?      include 'EMSDA.INC'
CM      ENDIF
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      logical u_du_act
      integer vr_in_c(-vr_in_c_n_sn:n_c)
      integer og_t_nw_perm(0:n_r)
      integer nw_t_og_perm(0:n_r)
      integer pi_ix(0:n_r)
      double precision du_act(0:mx_n_c+n_r)
      double precision pi_v(0:n_r)
      double precision ed_wt(0:mx_n_c+n_r)
 
      if (vr_t_en_bs .le. 0 .or. vr_t_lv_bs .eq. 0 .or.
     &     vr_t_en_bs .eq. vr_t_lv_bs) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9900)
     &        vr_t_en_bs, vr_t_lv_bs
         call ems_msg_wr_li(warn_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?         call ems_dump
CM      ENDIF
         go to 7000
      end if
      if (u_du_act) then
         call ems_perm_u_lg_dvx_wt_du_act(
     &        vr_t_en_bs,
     &        vr_in_c,
     &        du_act,
     &        pi_v,
     &        pi_ix,
     &        ed_wt,
     &        og_t_nw_perm,
     &        nw_t_og_perm)
      else
         call ems_perm_u_lg_dvx_wt(
     &        vr_t_en_bs,
     &        vr_in_c,
     &        pi_v,
     &        pi_ix,
     &        ed_wt,
     &        og_t_nw_perm,
     &        nw_t_og_perm)
      endif
      if (vr_t_lv_bs .gt. mx_n_c) then
c
c     Set the weight for the variable which has just left the basis.
c
         ed_wt(vr_t_lv_bs) = max(one, ed_wt(vr_t_en_bs)/abs(pv))
      endif
 7000 continue
      return
 9900 format('Calling u_lg_dvx_wt with vr_t_en_bs, vr_t_lv_bs = ',
     &     i9, i9)
      end
 
C->>> ----------------------------------------> ems_perm_u_lg_dvx_wt <<<
c     Updates Devex weights for logicals.
c
      subroutine ems_perm_u_lg_dvx_wt(
     &     vr_t_en_bs,
     &     vr_in_c,
     &     pi_v,
     &     pi_ix,
     &     ed_wt,
     &     og_t_nw_perm,
     &     nw_t_og_perm)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
CM      IF (emsol_da .EQ. 1) THEN
C?      include 'EMSDA.INC'
CM      ENDIF
      include 'ICTVR.INC'
      integer vr_t_en_bs, vr_in_c(-vr_in_c_n_sn:n_c)
      integer og_t_nw_perm(0:n_r)
      integer nw_t_og_perm(0:n_r)
      integer pi_ix(0:n_r)
      double precision pi_v(0:n_r)
      double precision ed_wt(0:mx_n_c+n_r)
      integer c_n, vr_n, nw_r_n, ix_n, r_n, og_r_n
      integer c_loop_ln
      double precision pi_v_mu
 
      pi_v_mu = ed_wt(vr_t_en_bs)
      c_loop_ln = vr_in_c(os_lg_in_c_l_pc_p)
      if (pi_ix(0) .gt. n_r .or.
     &     tbu_r_loop_mode .eq. tbu_r_loop_no .or.
     &     (tbu_r_loop_mode .eq. tbu_r_loop_poss .and.
     &     2*c_loop_ln .le. pi_ix(0))) then
c
c     Update the weights for the nonbasic logicals. See
c     subroutine ems_u_sed_wt for comments on obtaining the
c     reduced cost of the variable which has just left the basis.
c
         do 10, c_n = 1, vr_in_c(os_lg_in_c_l_pc_p)
            vr_n = vr_in_c(c_n)
            nw_r_n = og_t_nw_perm(vr_n-mx_n_c)
            if (pi_v(nw_r_n) .ne. zero) ed_wt(vr_n) =
     &           max(ed_wt(vr_n), abs(pi_v(nw_r_n))*pi_v_mu)
 10      continue
      else
         do 20, ix_n = 1, pi_ix(0)
            r_n = pi_ix(ix_n)
            og_r_n = nw_t_og_perm(r_n)
            vr_n = mx_n_c + og_r_n
            ed_wt(vr_n) = max(ed_wt(vr_n), abs(pi_v(r_n))*pi_v_mu)
 20      continue
      endif
      return
      end
 
C->>> ---------------------------------> ems_perm_u_lg_dvx_wt_du_act <<<
c     Updates Devex weights and dual activities for logicals.
c
      subroutine ems_perm_u_lg_dvx_wt_du_act(
     &     vr_t_en_bs,
     &     vr_in_c,
     &     du_act,
     &     pi_v,
     &     pi_ix,
     &     ed_wt,
     &     og_t_nw_perm,
     &     nw_t_og_perm)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
CM      IF (emsol_da .EQ. 1) THEN
C?      include 'EMSDA.INC'
CM      ENDIF
      include 'ICTVR.INC'
      integer vr_t_en_bs, vr_in_c(-vr_in_c_n_sn:n_c)
      integer og_t_nw_perm(0:n_r)
      integer nw_t_og_perm(0:n_r)
      integer pi_ix(0:n_r)
      double precision du_act(0:mx_n_c+n_r)
      double precision pi_v(0:n_r)
      double precision ed_wt(0:mx_n_c+n_r)
      integer c_n, vr_n, nw_r_n, ix_n, r_n, og_r_n
      integer c_loop_ln
      double precision pi_v_mu
 
      pi_v_mu = ed_wt(vr_t_en_bs)/abs(du_act(vr_t_en_bs))
      c_loop_ln = vr_in_c(os_lg_in_c_l_pc_p)
      if (pi_ix(0) .gt. n_r .or.
     &     tbu_r_loop_mode .eq. tbu_r_loop_no .or.
     &     (tbu_r_loop_mode .eq. tbu_r_loop_poss .and.
     &     2*c_loop_ln .le. pi_ix(0))) then
c
c     Update the weights and dual activities for the nonbasic logicals.
c     See subroutine ems_u_sed_wt for comments on obtaining the
c     reduced cost of the variable which has just left the basis.
c
         do 10 c_n = 1, vr_in_c(os_lg_in_c_l_pc_p)
            vr_n = vr_in_c(c_n)
            nw_r_n = og_t_nw_perm(vr_n-mx_n_c)
            if (pi_v(nw_r_n) .ne. zero) then
               ed_wt(vr_n) = max(ed_wt(vr_n), abs(pi_v(nw_r_n))*pi_v_mu)
               du_act(vr_n) = du_act(vr_n) - pi_v(nw_r_n)
            end if
 10      continue
      else
         do 20, ix_n = 1, pi_ix(0)
            r_n = pi_ix(ix_n)
            og_r_n = nw_t_og_perm(r_n)
            vr_n = mx_n_c + og_r_n
            ed_wt(vr_n) = max(ed_wt(vr_n), abs(pi_v(r_n))*pi_v_mu)
            du_act(vr_n) = du_act(vr_n) - pi_v(r_n)
 20      continue
      endif
      return
      end
 
C->>> ---------------------------------------> ems_ca_u_struc_dvx_wt <<<
c     Calls the routines to update Devex weights and possibly
c     dual activities.
c
      subroutine ems_ca_u_struc_dvx_wt(
     &     u_du_act,
     &     vr_in_c,
     &     du_act,
     &     tbu_r_v,
     &     tbu_r_ix,
     &     ed_wt)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICS.INC'
      include 'RSMICOM.INC'
CM      IF (emsol_da .EQ. 1) THEN
C?      include 'EMSDA.INC'
CM      ENDIF
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      logical u_du_act
      integer vr_in_c(-vr_in_c_n_sn:n_c)
      double precision du_act(0:mx_n_c+n_r)
      double precision tbu_r_v(0:n_c)
      integer tbu_r_ix(0:n_c)
      double precision ed_wt(0:mx_n_c+n_r)
 
      if (vr_t_en_bs .le. 0 .or. vr_t_lv_bs .eq. 0 .or.
     &     vr_t_en_bs .eq. vr_t_lv_bs) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9900)
     &        vr_t_en_bs, vr_t_lv_bs
         call ems_msg_wr_li(warn_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?         call ems_dump
CM      ENDIF
         go to 7000
      end if
      if (u_du_act) then
         call ems_u_struc_dvx_wt_du_act(
     &        vr_in_c,
     &        du_act,
     &        tbu_r_v,
     &        tbu_r_ix,
     &        ed_wt)
      else
         call ems_u_struc_dvx_wt(
     &        vr_in_c,
     &        tbu_r_v,
     &        tbu_r_ix,
     &        ed_wt)
      endif
      if (vr_t_lv_bs .le. n_c) then
c
c     Set the weight for the variable which has just left the basis.
c
         ed_wt(vr_t_lv_bs) = max(one, ed_wt(vr_t_en_bs)/abs(pv))
      endif
      n_dvx_it = n_dvx_it + 1
      if (iand(ck_msk, ze_a_ck_bt) .ne. 0)
     &     call ems_ck_ze_rl_a(n_c, tbu_r_v)
 7000 continue
      return
 9900 format('Calling u_struc_dvx_wt with vr_t_en_bs, vr_t_lv_bs = ',
     &     i9, i9)
      end
 
C->>> ------------------------------------------> ems_u_struc_dvx_wt <<<
c     Updates Devex weights for structurals.
c
      subroutine ems_u_struc_dvx_wt(
     &     vr_in_c,
     &     tbu_r_v,
     &     tbu_r_ix,
     &     ed_wt)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
CM      IF (emsol_da .EQ. 1) THEN
C?      include 'EMSDA.INC'
CM      ENDIF
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
      integer vr_in_c(-vr_in_c_n_sn:n_c)
      double precision tbu_r_v(0:n_c)
      integer tbu_r_ix(0:n_c)
      double precision ed_wt(0:mx_n_c+n_r)
      integer c_loop_ln, c_n, vr_n, ix_n
      double precision pi_v_mu
      integer tbu_r_n_nz, tbu_r_n_c
 
      pi_v_mu = ed_wt(vr_t_en_bs)
      c_loop_ln =
     &     vr_in_c(os_struc_in_c_l_pc_p) -
     &     vr_in_c(os_struc_in_c_f_p_m1)
      tbu_r_n_c = c_loop_ln
c
c     Update the weights for the nonbasic variables. See
c     subroutine ems_u_sed_wt for comments on obtaining the
c     reduced cost of the variable which has just left the basis.
c
      if (tbu_r_ix(0) .gt. n_c .or.
     &     tbu_r_loop_mode .eq. tbu_r_loop_no .or.
     &     (tbu_r_loop_mode .eq. tbu_r_loop_poss .and.
     &     2*c_loop_ln .le. tbu_r_ix(0))) then
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl1)
C?     &        call ems_tt_rec(u_struc_dvx_wt_dse_tt, n_bs)
CM      ENDIF
         tbu_r_n_nz = 0
         do 10, c_n = vr_in_c(os_struc_in_c_f_p_m1) + 1,
     &        vr_in_c(os_struc_in_c_l_pc_p)
            vr_n = vr_in_c(c_n)
            if (tbu_r_v(vr_n) .ne. zero) then
               tbu_r_n_nz = tbu_r_n_nz + 1
               ed_wt(vr_n) =
     &              max(ed_wt(vr_n), pi_v_mu*abs(tbu_r_v(vr_n)))
               tbu_r_v(vr_n) = zero
            endif
 10      continue
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl1)
C?     &        call ems_tt_rec(-u_struc_dvx_wt_dse_tt, n_bs)
CM      ENDIF
      else
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl1)
C?     &        call ems_tt_rec(u_struc_dvx_wt_sps_tt, n_bs)
CM      ENDIF
         tbu_r_n_nz = tbu_r_ix(0)
         do 20, ix_n = 1, tbu_r_ix(0)
            vr_n = tbu_r_ix(ix_n)
            ed_wt(vr_n) = max(ed_wt(vr_n), pi_v_mu*abs(tbu_r_v(vr_n)))
            tbu_r_v(vr_n) = zero
 20      continue
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl1)
C?     &        call ems_tt_rec(-u_struc_dvx_wt_sps_tt, n_bs)
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
      return
      end
 
C->>> -----------------------------------> ems_u_struc_dvx_wt_du_act <<<
c     Updates Devex weights and dual activities for structurals.
c
      subroutine ems_u_struc_dvx_wt_du_act(
     &     vr_in_c,
     &     du_act,
     &     tbu_r_v,
     &     tbu_r_ix,
     &     ed_wt)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
CM      IF (emsol_da .EQ. 1) THEN
C?      include 'EMSDA.INC'
CM      ENDIF
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
      integer vr_in_c(-vr_in_c_n_sn:n_c)
      double precision du_act(0:mx_n_c+n_r)
      double precision tbu_r_v(0:n_c)
      integer tbu_r_ix(0:n_c)
      double precision ed_wt(0:mx_n_c+n_r)
      integer c_n, vr_n, c_loop_ln, ix_n
      double precision tbu_r_v_mu
      integer tbu_r_n_nz, tbu_r_n_c
 
      tbu_r_v_mu = ed_wt(vr_t_en_bs)/abs(du_act(vr_t_en_bs))
      c_loop_ln =
     &     vr_in_c(os_struc_in_c_l_pc_p) -
     &     vr_in_c(os_struc_in_c_f_p_m1)
      tbu_r_n_c = c_loop_ln
c
c     Update the weights and dual activities for the nonbasic variables.
c     See subroutine ems_u_sed_wt for comments on obtaining the
c     reduced cost of the variable which has just left the basis.
c
      if (tbu_r_ix(0) .gt. n_c .or.
     &     tbu_r_loop_mode .eq. tbu_r_loop_no .or.
     &     (tbu_r_loop_mode .eq. tbu_r_loop_poss .and.
     &     2*c_loop_ln .le. tbu_r_ix(0))) then
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl1)
C?     &        call ems_tt_rec(u_struc_dvx_wt_dse_tt, n_bs)
CM      ENDIF
         tbu_r_n_nz = 0
         do 10, c_n = vr_in_c(os_struc_in_c_f_p_m1) + 1,
     &        vr_in_c(os_struc_in_c_l_pc_p)
            vr_n = vr_in_c(c_n)
            if (tbu_r_v(vr_n) .ne. zero) then
               tbu_r_n_nz = tbu_r_n_nz + 1
               ed_wt(vr_n) = max(
     &              ed_wt(vr_n), abs(tbu_r_v(vr_n))*tbu_r_v_mu)
               du_act(vr_n) = du_act(vr_n) + tbu_r_v(vr_n)
               tbu_r_v(vr_n) = zero
            end if
 10      continue
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl1)
C?     &        call ems_tt_rec(-u_struc_dvx_wt_dse_tt, n_bs)
CM      ENDIF
      else
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl1)
C?     &        call ems_tt_rec(u_struc_dvx_wt_sps_tt, n_bs)
CM      ENDIF
         tbu_r_n_nz = tbu_r_ix(0)
         do 20, ix_n = 1, tbu_r_ix(0)
            vr_n = tbu_r_ix(ix_n)
            if (tbu_r_v(vr_n) .ne. zero) then
               ed_wt(vr_n) = max(
     &              ed_wt(vr_n), abs(tbu_r_v(vr_n))*tbu_r_v_mu)
               du_act(vr_n) = du_act(vr_n) + tbu_r_v(vr_n)
               tbu_r_v(vr_n) = zero
            endif
 20      continue
CM      IF (emsol_tt .EQ. 1) THEN
C?         if (ems_tt_pc_lvl1)
C?     &        call ems_tt_rec(-u_struc_dvx_wt_sps_tt, n_bs)
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
      return
      end
 
C->>> ----------------------------------------------> ems_iz_dvx_fwk <<<
c     Initialises the weights and the index sets for Devex.
c
      subroutine ems_iz_dvx_fwk(
     &     vr_in_r, vr_in_c,
     &     ed_wt,
     &     dvx_ix)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICS.INC'
      include 'ICTVR.INC'
      integer vr_in_r(0:n_r)
      integer vr_in_c(-vr_in_c_n_sn:n_c)
      integer dvx_ix(0:mx_n_c+n_r)
      double precision ed_wt(0:mx_n_c+n_r)
      integer c_n, vr_n, r_n
c
c     Set R is the reference framework and consists of the set of
c     variables for pricing for this simplex iteration.
c     Set H is the set indices which are used to compute the Devex
c     norm. It consists of the basic variables in R so, initially, is
c     empty.
c     The implementation requires the vector
c
c     dvx_ix     If ix = dvx_ix(vr_n) then
c                   ix = 1 => the variable vr_n is in H (in R & basic)
c                   ix = 0 => the variable vr_n is in R but not basic.
c                   ix =-1 => the variable vr_n is not in R.
c
c
c     Zero the logical edge weights in case values of pi are just
c     used to update them without checking whether the variable is basic
c     or nonbasic---thus causing an unassigned variable error with EPC.
c
      call ems_cp_rl_a(n_r, zero, ed_wt(mx_n_c+1), 0)
      do 10, c_n = 1, vr_in_c(os_lg_in_c_l_pc_p)
         vr_n = vr_in_c(c_n)
         ed_wt(vr_n) = one
         dvx_ix(vr_n) = 0
 10   continue
      do 20, c_n = vr_in_c(os_struc_in_c_f_p_m1) + 1,
     &     vr_in_c(os_struc_in_c_l_pc_p)
         vr_n = vr_in_c(c_n)
         ed_wt(vr_n) = one
         dvx_ix(vr_n) = 0
 20   continue
      do 30, r_n = 1, n_r
         vr_n = vr_in_r(r_n)
         ed_wt(vr_n) = zero
         dvx_ix(vr_n) = -1
 30   continue
      n_dvx_it = 1
      n_dvx_fwk = n_dvx_fwk + 1
c
c     Indicate that the edge weight information is correct.
c
      ml_da_st_msk = ior(ml_da_st_msk, ml_da_st_ed_wt)
      return
      end
 
C->>> ----------------------------------------------> ems_iz_blk_dvx <<<
c     Sets up a block for the vector required by Devex.
c
      subroutine ems_iz_blk_dvx(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer blk_n, r_cf, c_cf, a_el_cf, cs, n_wo, p_ml_hdl
      integer mem_mgr_rt_cod
      integer rt_cod
 
      if (iand(ml_blk_st_msk, ml_blk_st_dvx) .ne. 0) goto 8000
      call ems_g_blk_dvx_n_wo(r_cf, c_cf, a_el_cf, cs, n_wo)
      call ems_mem_mgr_ope_blk(mem_mgr_rt_cod, is,
     &     n_wo, ope_blk_anywhere,
     &     cu_ml_n, dvx_blk_id, blk_n)
      if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
         if (mem_mgr_rt_cod .eq. mem_mgr_rt_cod_serious_no_po) then
            ems_msg_cod = ems_msg_lvl_serious
            go to 7000
         else if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
             ems_msg_cod = ems_msg_lvl_serious
            go to 7000
         endif
      endif
      call ems_mem_mgr_rp_ope_blk(
     &     mem_mgr_rt_cod, is, -1,
     &     r_cf, mx_n_r,
     &     c_cf, mx_n_c,
     &     0, 0,
     &     cs, n_wo, dvx_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         go to 7000
      endif
      p_ml_hdl = p_ml_bs_blk + (cu_ml_n-1)*ml_bs_blk_n_wo +
     &     ml_bs_blk_os_hdl
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+mx_n_c+mx_n_r,
     &     i_wo_z, is(p_ml_hdl+ix_hdl_dvx_ix))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_g_ml_p(rt_cod, is)
      if (rt_cod .ne. 0) goto 8020
      ml_blk_st_msk = ml_blk_st_msk + ml_blk_st_dvx
 7000 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9800 format('Model already has space for DEVEX')
 9802 format('Error in ems_g_ml_p')
      end
 
C->>> ------------------------------------------> ems_g_blk_dvx_n_wo <<<
      subroutine ems_g_blk_dvx_n_wo(
     &     r_cf, c_cf, a_el_cf, cs, n_wo)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      integer r_cf, c_cf, a_el_cf, cs, n_wo
      r_cf = i_wo_z
      c_cf = i_wo_z
      a_el_cf =   0
      cs =   i_wo_z
      n_wo = r_cf*mx_n_r + c_cf*mx_n_c + cs
      return
      end
 
C->>> ----------------------------------------------> ems_rm_blk_dvx <<<
c     Removes the block for the vector required by Devex.
c
      subroutine ems_rm_blk_dvx(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer blk_n, p_ml_hdl
      integer mem_mgr_rt_cod
 
      if (iand(ml_blk_st_msk, ml_blk_st_dvx) .eq. 0) goto 8000
      p_ml_hdl = p_ml_bs_blk + (cu_ml_n-1)*ml_bs_blk_n_wo +
     &     ml_bs_blk_os_hdl
      blk_n = is(p_ml_hdl+ix_hdl_dvx_ix)
      call ems_mem_mgr_rm_blk(mem_mgr_rt_cod, is,
     &     blk_n)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         goto 7100
      endif
      ml_blk_st_msk = ml_blk_st_msk - ml_blk_st_dvx
 7000 continue
 7100 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9800 format('Model does not already has space for DEVEX')
      end
 
C->>> ------------------------------------------------> ems_u_dvx_ix <<<
c     Updates the index sets for Devex.
c
      subroutine ems_u_dvx_ix(dvx_ix)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer dvx_ix(0:mx_n_c+n_r)
 
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
c
c     The variable to leave the basis is in R (so will leave H).
c
      if (dvx_ix(vr_t_lv_bs) .gt. 0) dvx_ix(vr_t_lv_bs) = 0
c
c     The variable to enter the basis is in R (and will enter H)
c
      if (dvx_ix(vr_t_en_bs) .eq. 0) dvx_ix(vr_t_en_bs) = 1
 7000 continue
      return
 9900 format('Calling u_dvx_ix with vr_t_en_bs, vr_t_lv_bs = ',
     &     i9, i9)
      end
 
