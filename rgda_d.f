CM
c     Create the RgDa versions from rgda_s.f
c
CM
C->>> ----------------------------------------------> ems_g_vr_rg_da <<<
c     Gets the ranging information for a variable.
c
CM      IF (sps_rgda .EQ. 1) THEN
C?      subroutine ems_g_vr_rg_da_sps(
CM      ELSE
      subroutine ems_g_vr_rg_da_dse(
CM      ENDIF
     &     c_n, en_vr_n, u_bc_rg_da, pv_c_n_ix,
     &     aa_up, aa_lo,
     &     pv_r_n_up, pv_r_n_lo,
     &     vr_in_r, st,
     &     lb, ub,
     &     pr_act, du_act,
     &     pv_c_v, pv_c_ix,
     &     pk_v,
     &     co_rg_up_co_v, co_rg_lo_co_v,
     &     co_rg_up_act_v, co_rg_lo_act_v,
     &     co_rg_up_en_vr, co_rg_lo_en_vr,
     &     co_rg_up_lv_vr, co_rg_lo_lv_vr)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ITXITCS.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      integer c_n, en_vr_n, pv_c_n_ix
      logical u_bc_rg_da
      double precision aa_up, aa_lo
      integer pv_r_n_up, pv_r_n_lo
      integer vr_in_r(0:n_r), st(0:mx_n_c+n_r)
      double precision lb(0:mx_n_c+n_r), ub(0:mx_n_c+n_r)
      double precision pr_act(0:mx_n_c+n_r), du_act(0:mx_n_c+n_r)
      double precision pv_c_v(0:n_r)
      double precision pk_v(0:n_r)
      double precision co_rg_up_co_v(0:mx_n_c+n_r)
      double precision co_rg_lo_co_v(0:mx_n_c+n_r)
      double precision co_rg_up_act_v(0:mx_n_c+n_r)
      double precision co_rg_lo_act_v(0:mx_n_c+n_r)
      integer pv_c_ix(0:n_r)
      integer co_rg_up_en_vr(0:mx_n_c+n_r)
      integer co_rg_lo_en_vr(0:mx_n_c+n_r)
      integer co_rg_up_lv_vr(0:mx_n_c+n_r)
      integer co_rg_lo_lv_vr(0:mx_n_c+n_r)
      double precision pv, rsdu, du_act_v
      double precision co_rg_aa_up, co_rg_aa_lo
      integer r_n, vr_n, en_vr_st, vr_st
      logical ze_du_act, pos_du_act, neg_du_act
      logical en_vr_mv_up, en_vr_mv_dn
      integer ix_n, n_ix
c
c     Analyse the nonbasic variable: its status and activities.
c
      en_vr_st = st(en_vr_n)
      du_act_v = du_act(en_vr_n)
      ze_du_act = abs(du_act_v) .le. tl_du_ifs
      if (ze_du_act) then
         pos_du_act = .false.
         neg_du_act = .false.
      else
         pos_du_act = du_act_v .gt. zero
         neg_du_act = du_act_v .lt. zero
      endif
      en_vr_mv_up = iand(en_vr_st, up_bt) .ne. 0
      en_vr_mv_dn = iand(en_vr_st, dn_bt) .ne. 0
c
c     If the basis is optimal then nonbasic variables which are free to
c     move up or down should not have a non-zero dual activity.
c
      if (en_vr_mv_up .and. en_vr_mv_dn .and. .not.ze_du_act) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9600)en_vr_n,
     &        lb(en_vr_n), pr_act(en_vr_n), ub(en_vr_n), du_act_v
         call ems_msg_wr_li(er_msg_n)
         ze_du_act = .true.
         pos_du_act = .false.
         neg_du_act = .false.
      endif
c
c     If the basis is optimal then nonbasic variables which are free to
c     move up should not have a negative dual activity.
c
      if (en_vr_mv_up .and. neg_du_act) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9601)en_vr_n,
     &        lb(en_vr_n), pr_act(en_vr_n), ub(en_vr_n), du_act_v
         call ems_msg_wr_li(er_msg_n)
         ze_du_act = .true.
         pos_du_act = .false.
         neg_du_act = .false.
      endif
c
c     If the basis is optimal then nonbasic variables which are free to
c     move down should not have a positive dual activity.
c
      if (en_vr_mv_dn .and. pos_du_act) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9602)en_vr_n,
     &        lb(en_vr_n), pr_act(en_vr_n), ub(en_vr_n), du_act_v
         call ems_msg_wr_li(er_msg_n)
         ze_du_act = .true.
         pos_du_act = .false.
         neg_du_act = .false.
      endif
c
c     Initialise the steps and pivotal rows for the primal ratio tests.
c
      aa_up = inf
      aa_lo = inf
      pv_r_n_up = -1
      pv_r_n_lo = -1
      if (en_vr_mv_up) then
c
c     If the entering variable is free to move up then make sure the
c     upper range does not exceed any upper bound.
c
         if (iand(en_vr_st, ub_bt) .ne. 0) then
            aa_up = ub(en_vr_n) - pr_act(en_vr_n)
            pv_r_n_up = 0
         endif
      endif
      if (en_vr_mv_dn) then
c
c     If the entering variable is free to move down then make sure the
c     lower range does not exceed any lower bound.
c
         if (iand(en_vr_st, lb_bt) .ne. 0) then
            aa_lo = pr_act(en_vr_n) - lb(en_vr_n)
            pv_r_n_lo = 0
         endif
      endif
c
c=======================================================================
c     Perform the primal ratio tests for the nonbasic variable.
c
c
c     Initialise the number of (true) nonzero values in the tableau
c     column---if pv_c_ix(0) .le. n_r then zeros and repeated entries
c     indexed by pv_c_ix(1:pv_c_ix(0)) will be removed in the first
c     pass.
c
      n_ix = 0
CM      IF (sps_rgda .EQ. 1) THEN
C?      do 10, ix_n = 1, pv_c_ix(0)
C?         r_n = pv_c_ix(ix_n)
CM      ELSE
      do 10, r_n = 1, n_r
CM      ENDIF
         pv = pv_c_v(r_n)
         if (pv .eq. zero) goto 10
         pv_c_v(r_n) = zero
         if (abs(pv) .le. pk_pv_c_ze) goto 10
c
c     Pack the nonzero value for the second pass.
c
         n_ix = n_ix + 1
         pk_v(n_ix) = pv
         pv_c_ix(n_ix) = r_n
c
c     Determine the basic variable and its status---need the lb/ub bits
c
         vr_n = vr_in_r(r_n)
         vr_st = st(vr_n)
         if (pv .gt. zero) then
            if (iand(vr_st, ub_bt) .ne. 0) then
c
c     Variable moves up to upper bound as entering variable increases
c
               rsdu = ub(vr_n) - pr_act(vr_n)
               if (rsdu .lt. aa_up*pv) then
                  aa_up = rsdu/pv
                  pv_r_n_up = r_n
               endif
            endif
            if (iand(vr_st, lb_bt) .ne. 0) then
c
c     Variable moves down to lower bound as entering variable decreases
c
               rsdu = pr_act(vr_n) - lb(vr_n)
               if (rsdu .lt. aa_lo*pv) then
                  aa_lo = rsdu/pv
                  pv_r_n_lo = r_n
               endif
            endif
         else
            if (iand(vr_st, ub_bt) .ne. 0) then
c
c     Variable moves up to upper bound as entering variable decreases
c
               rsdu = pr_act(vr_n) - ub(vr_n)
               if (rsdu .gt. aa_lo*pv) then
                  aa_lo = rsdu/pv
                  pv_r_n_lo = r_n
               endif
            endif
            if (iand(vr_st, lb_bt) .ne. 0) then
c
c     Variable moves down to lower bound as entering variable increases
c
               rsdu = lb(vr_n) - pr_act(vr_n)
               if (rsdu .gt. aa_up*pv) then
                  aa_up = rsdu/pv
                  pv_r_n_up = r_n
               endif
            endif
         endif
 10   continue
      aa_up = max(aa_up, zero)
      aa_lo = max(aa_lo, zero)
      pv_c_n_ix = n_ix
c
c=======================================================================
c     Update the dual ratio tests for the basic variables.
c
      if (.not. u_bc_rg_da) goto 7000
c
c     Find the steps which respect the bounds
c
      co_rg_aa_up = aa_up
      if (.not. en_vr_mv_up) co_rg_aa_up = zero
      co_rg_aa_lo = aa_lo
      if (.not. en_vr_mv_dn) co_rg_aa_lo = zero
c
c     pos_du_act => en_vr_mv_up and .not.en_vr_mv_dn
c     neg_du_act => en_vr_mv_dn and .not.en_vr_mv_up
c
c     ... since this part of the routine is not called for variables
c     which are not free to move up or down.
 
      do 20, ix_n = 1, n_ix
c
c     Get the packed row number, value and corresponding variable number
c
         r_n = pv_c_ix(ix_n)
         pv = pk_v(ix_n)
         vr_n = vr_in_r(r_n)
         if (pv .gt. zero) then
c
c     Update the cost range data for a positive pivot.
c
            if (ze_du_act) then
               if (en_vr_mv_up .and.
     &              (co_rg_lo_en_vr(vr_n) .ge. 0 .or.
     &              co_rg_aa_up*pv .gt. co_rg_lo_act_v(vr_n))) then
c
c     There is a zero lower cost range for this basic variable and the
c     nonbasic variable is either the first to give such a range or the
c     the change in the primal activity with this nonbasic variable is
c     the greatest so far.
c
c     Record:
c     the lower cost range
c     the change in the primal activity
c     the nonbasic variable number---negated to cheapen the test for
c         whether a minimal (zero) lower cost range has been found.
c     the direction in which the nonbasic variable changes.
c
                  co_rg_lo_co_v(vr_n) =   zero
                  co_rg_lo_act_v(vr_n) =  co_rg_aa_up*pv
                  co_rg_lo_en_vr(vr_n) = -en_vr_n
                  co_rg_lo_lv_vr(vr_n) =  1
               endif
               if (en_vr_mv_dn .and.
     &              (co_rg_up_en_vr(vr_n) .ge. 0 .or.
     &              co_rg_aa_lo*pv .gt. co_rg_up_act_v(vr_n))) then
c
c     There is a zero upper cost range for this basic variable and the
c     nonbasic variable is either the first to give such a range or the
c     the change in the primal activity with this nonbasic variable is
c     the greatest so far.
c
c     Record:
c     the upper cost range
c     the change in the primal activity
c     the nonbasic variable number---negated to cheapen the test for
c         whether a minimal (zero) upper cost range has been found.
c     the direction in which the nonbasic variable changes.
c
                  co_rg_up_co_v(vr_n) =   zero
                  co_rg_up_act_v(vr_n) =  co_rg_aa_lo*pv
                  co_rg_up_en_vr(vr_n) = -en_vr_n
                  co_rg_up_lv_vr(vr_n) = -1
               endif
            else if (pos_du_act) then
c
c     The positive nonbasic dual activity moves down to zero as the
c     basic cost decreases. The resulting basis change then increases
c     the primal activity at zero (further) cost.
c
c     Record:
c     the upper cost range
c     the change in the primal activity
c     the nonbasic variable number
c     the direction in which the nonbasic variable changes.
c
               if (du_act_v .lt. co_rg_lo_co_v(vr_n)*pv) then
                  co_rg_lo_co_v(vr_n) =   du_act_v/pv
                  co_rg_lo_act_v(vr_n) =  co_rg_aa_up*pv
                  co_rg_lo_en_vr(vr_n) =  en_vr_n
                  co_rg_lo_lv_vr(vr_n) =  1
               endif
            else
c
c     The negative nonbasic dual activity moves up to zero as the
c     basic cost increases. The resulting basis change then decreases
c     the primal activity at zero (further) cost.
c
               if (-du_act_v .lt. co_rg_up_co_v(vr_n)*pv) then
                  co_rg_up_co_v(vr_n) =  -du_act_v/pv
                  co_rg_up_act_v(vr_n) =  co_rg_aa_lo*pv
                  co_rg_up_en_vr(vr_n) =  en_vr_n
                  co_rg_up_lv_vr(vr_n) = -1
               endif
            endif
         else
c
c     Update the cost range data for a negative pivot
c
            if (ze_du_act) then
               if (en_vr_mv_up .and.
     &              (co_rg_up_en_vr(vr_n) .ge. 0 .or.
     &              -co_rg_aa_up*pv .gt. co_rg_up_act_v(vr_n))) then
c
c     There is a zero upper cost range for this basic variable and the
c     nonbasic variable is either the first to give such a range or the
c     the change in the primal activity with this nonbasic variable is
c     the greatest so far.
c
c     Record:
c     the upper cost range
c     the change in the primal activity
c     the nonbasic variable number---negated to cheapen the test for
c         whether a minimal (zero) upper cost range has been found.
c     the direction in which the nonbasic variable changes.
c
                  co_rg_up_co_v(vr_n) =   zero
                  co_rg_up_act_v(vr_n) = -co_rg_aa_up*pv
                  co_rg_up_en_vr(vr_n) = -en_vr_n
                  co_rg_up_lv_vr(vr_n) =  1
               endif
               if (en_vr_mv_dn .and.
     &              (co_rg_lo_en_vr(vr_n) .ge. 0 .or.
     &              -co_rg_aa_lo*pv .gt. co_rg_lo_act_v(vr_n))) then
c
c     There is a zero lower cost range for this basic variable and the
c     nonbasic variable is either the first to give such a range or the
c     the change in the primal activity with this nonbasic variable is
c     the greatest so far.
c
c     Record:
c     the lower cost range
c     the change in the primal activity
c     the nonbasic variable number---negated to cheapen the test for
c         whether a minimal (zero) lower cost range has been found.
c     the direction in which the nonbasic variable changes.
c
                  co_rg_lo_co_v(vr_n) =   zero
                  co_rg_lo_act_v(vr_n) = -co_rg_aa_lo*pv
                  co_rg_lo_en_vr(vr_n) = -en_vr_n
                  co_rg_lo_lv_vr(vr_n) = -1
               endif
            else if (pos_du_act) then
c
c     The positive nonbasic dual activity moves down to zero as the
c     basic cost increases. The resulting basis change then increases
c     the primal activity at zero (further) cost.
c
c     Record:
c     the upper cost range
c     the change in the primal activity
c     the nonbasic variable number
c     the direction in which the nonbasic variable changes.
c
               if (du_act_v .lt. -co_rg_up_co_v(vr_n)*pv) then
                  co_rg_up_co_v(vr_n) =  -du_act_v/pv
                  co_rg_up_act_v(vr_n) = -co_rg_aa_up*pv
                  co_rg_up_en_vr(vr_n) =  en_vr_n
                  co_rg_up_lv_vr(vr_n) =  1
               endif
            else
c
c     The negative nonbasic dual activity moves up to zero as the
c     basic cost decreases. The resulting basis change then decreases
c     the primal activity at zero (further) cost.
c
c     Record:
c     the lower cost range
c     the change in the primal activity
c     the nonbasic variable number
c     the direction in which the nonbasic variable changes.
c
               if (-du_act_v .lt. -co_rg_lo_co_v(vr_n)*pv) then
                  co_rg_lo_co_v(vr_n) =   du_act_v/pv
                  co_rg_lo_act_v(vr_n) = -co_rg_aa_lo*pv
                  co_rg_lo_en_vr(vr_n) =  en_vr_n
                  co_rg_lo_lv_vr(vr_n) = -1
               endif
            endif
         endif
 20   continue
 7000 continue
      return
 9600 format('Nonbasic variable ', i7,
     &     ' with Lb:Act:Ub ', g11.4, 2(':', g11.4),
     &     ' can move up and down but has non-zero dual activity ',
     &     g11.4, ': Treating this as zero')
 9601 format('Nonbasic variable ', i7,
     &     ' with Lb:Act:Ub ', g11.4, 2(':', g11.4),
     &     ' can move up but has negative dual activity ',
     &     g11.4, ': Treating this as zero')
 9602 format('Nonbasic variable ', i7,
     &     ' with Lb:Act:Ub ', g11.4, 2(':', g11.4),
     &     ' can move down but has positive dual activity ',
     &     g11.4, ': Treating this as zero')
      end
 
