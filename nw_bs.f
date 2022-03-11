C->>> ---------------------------------------> ems_ca_iz_lg_bs_vr_st <<<
      subroutine ems_ca_iz_lg_bs_vr_st(ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      integer is(0:is_n_en_m1)
      double precision ds(0:ds_n_en_m1)
      call ems_iz_lg_bs_vr_st(
     &     ds(p_lbc), ds(p_cbp), ds(p_ubc),
     &     is(p_st), ds(p_pr_act), ds(p_du_act), ds(p_scl),
     &     is(p_vr_in_r), is(p_vr_in_c))
      return
      end
 
C->>> ------------------------------------------> ems_iz_lg_bs_vr_st <<<
c     Initialise the status vector corresponding to primal logical
c     variables being basic and primal structural variables being
c     nonbasic and set all the primal activities to zero.
c
      subroutine ems_iz_lg_bs_vr_st(
     &     lbc, cbp, ubc,
     &     st, pr_act, du_act, scl,
     &     vr_in_r, vr_in_c)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      double precision lbc(0:mx_n_c+mx_n_r)
      double precision cbp(0:mx_n_c+mx_n_r)
      double precision ubc(0:mx_n_c+mx_n_r)
      integer st(0:mx_n_c+mx_n_r)
      integer vr_in_r(0:n_r)
      integer vr_in_c(-vr_in_c_n_sn:n_c)
      double precision pr_act(0:mx_n_c+mx_n_r)
      double precision du_act(0:mx_n_c+mx_n_r)
      double precision scl(0:mx_n_c+mx_n_r)
      integer r_n, c_n, vr_n, sv_vr_in_c_0
 
      do 10, c_n = 1, n_c
         vr_in_c(c_n) = c_n
         st(c_n) = non_bc_vr_bs_st
         du_act(c_n) = zero
 10   continue
c
c     vr_in_c(0) indicates the dimension for which vr_in_c was defined
c     so preserve this value whilst using vr_in_c(0) to indicate the
c     number of entries currently in vr_in_c.
c
      sv_vr_in_c_0 = vr_in_c(0)
      vr_in_c(0) = n_c
      call ems_iz_st_bd_bt(vr_in_c(0), lbc, ubc, st)
      call ems_iz_vr_st_act(
     &     vr_in_c(0), lbc, cbp, ubc, st, pr_act, scl, tl_mx_iz_pr_act)
      vr_in_c(0) = sv_vr_in_c_0
      do 20, r_n = 1, n_r
         vr_n = mx_n_c+r_n
         vr_in_r(r_n) = vr_n
         st(vr_n) = bc_vr_bs_st + r_n
         du_act(vr_n) = zero
 20   continue
      vr_in_r(0) = n_r
      call ems_iz_st_bd_bt(vr_in_r, lbc, ubc, st)
      call ems_iz_vr_st_act(
     &     vr_in_r, lbc, cbp, ubc, st, pr_act, scl, tl_mx_iz_pr_act)
c
c     Indicate that the following are not correct for the model:
c
c     vr_in_c, INVERT, basic primal activities, edge weights and
c     row-wise representation of matrix columns being priced.
c
      ml_da_st_msk = ml_da_st_msk
     &     - iand(ml_da_st_msk, ml_da_st_vr_in_c)
     &     - iand(ml_da_st_msk, ml_da_st_inv)
     &     - iand(ml_da_st_msk, ml_da_st_bc_pr_act)
     &     - iand(ml_da_st_msk, ml_da_st_ed_wt)
     &     - iand(ml_da_st_msk, ml_da_st_r_mtx)
      return
      end
 
C->>> ---------------------------------------------> ems_iz_st_bd_bt <<<
      subroutine ems_iz_st_bd_bt(vr_ls, lbc, ubc, st)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      integer vr_ls(0:*)
      double precision lbc(0:mx_n_c+mx_n_r)
      double precision ubc(0:mx_n_c+mx_n_r)
      integer st(0:mx_n_c+mx_n_r)
      integer ls_en_n, vr_n, vr_st
 
      do 10, ls_en_n = 1, vr_ls(0)
         vr_n = vr_ls(ls_en_n)
         vr_st = st(vr_n)
         if (iand(vr_st, alt_bt) .eq. 0) then
            vr_st = vr_st - iand(st(vr_n), lb_ub)
            if (lbc(vr_n) .gt. -inf) vr_st = vr_st + lb_bt
            if (ubc(vr_n) .lt.  inf) vr_st = vr_st + ub_bt
            st(vr_n) = vr_st
         endif
 10   continue
      return
      end
 
C->>> --------------------------------------------> ems_iz_vr_st_act <<<
c     Initialises the up, down and ifs bits of the status, and the
c     primal activity for each of a list of (nonbasic) variables.
c     Assumes that the lower and upper bound bits are set correctly.
c     Passes through the primal activities, making sure that they have
c     not been set to excessively large/small values.
c
      subroutine ems_iz_vr_st_act(
     &     vr_ls, lbc, cbp, ubc, st, pr_act, scl, usr_tl_mx_iz_pr_act)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      integer vr_ls(0:*)
      double precision lbc(0:mx_n_c+mx_n_r)
      double precision cbp(0:mx_n_c+mx_n_r)
      double precision ubc(0:mx_n_c+mx_n_r)
      double precision pr_act(0:mx_n_c+mx_n_r)
      double precision scl(0:mx_n_c+mx_n_r)
      double precision usr_tl_mx_iz_pr_act
      integer st(0:mx_n_c+mx_n_r)
      integer ls_en_n, vr_n, vr_st
      double precision scl_v
      double precision pr_act_v
      logical scl_pr_act_v
 
      scl_v = one
      scl_pr_act_v = iand(ml_da_st_msk, ml_da_st_scl_ml_sol) .ne. 0
      do 10, ls_en_n = 1, vr_ls(0)
         vr_n = vr_ls(ls_en_n)
         vr_st = st(vr_n)
         if (scl_pr_act_v) scl_v = scl(vr_n)
c
c     Make sure that the up, dn and ifs bits are not set
c
         vr_st = vr_st - iand(vr_st, ifs_bt)
         vr_st = vr_st - iand(vr_st, up_dn)
         if (iand(vr_st, alt_bt) .eq. 0) then
c
c     Variable is standard
c
            if (iand(vr_st, ub_bt) .eq. 0) then
               if (iand(vr_st, lb_bt) .eq. 0) then
c
c     A FR variable: Set the primal activity to zero
c
c     Set the primal activity to zero and set the up and dn bits.
c
                  pr_act(vr_n) = zero
                  vr_st = vr_st + up_dn
               else
c
c     A LB variable: Set the primal activity to the lower bound
c
                  pr_act_v = lbc(vr_n)*scl_v
                  if (pr_act_v .ge. -usr_tl_mx_iz_pr_act) then
c
c     Set the primal activity to the lower bound and set the up bit...
c
                     pr_act(vr_n) = pr_act_v
                     vr_st = vr_st + up_bt
                  else
c
c     ... unless this is an excessively low value, in which case, set
c     the primal activity to the minimum allowed value and set the up
c     and dn bits.
c
                     pr_act(vr_n) = -usr_tl_mx_iz_pr_act
                     vr_st = vr_st + up_dn
                  endif
               endif
            else
               if (iand(vr_st, lb_bt) .eq. 0) then
c
c     A UB variable: Set the primal activity to the upper bound
c
                  pr_act_v = ubc(vr_n)*scl_v
                  if (pr_act_v .le. usr_tl_mx_iz_pr_act) then
c
c     Set the primal activity to the upper bound and set the dn bit...
c
                     pr_act(vr_n) = pr_act_v
                     vr_st = vr_st + dn_bt
                  else
c
c     ... unless this is an excessively high value, in which case, set
c     the primal activity to the maximum allowed value and set the up
c     and dn bits.
c
                     pr_act(vr_n) = usr_tl_mx_iz_pr_act
                     vr_st = vr_st + up_dn
                  endif
               else if (lbc(vr_n) .ne. ubc(vr_n)) then
c
c     A LB/UB variable: Set the variable to the lower bound
c
c                  vr_st = vr_st + up_bt
c                  pr_act(vr_n) = lbc(vr_n)*scl_v
c
c     Set the variable to the bound of smallest absolute value
c
                  if (abs(lbc(vr_n)) .le. abs(ubc(vr_n))) then
                     pr_act_v = lbc(vr_n)*scl_v
                     if (pr_act_v .ge. -usr_tl_mx_iz_pr_act) then
c
c     Set the primal activity to the lower bound and set the up bit...
c
                        pr_act(vr_n) = pr_act_v
                        vr_st = vr_st + up_bt
                     else
c
c     ... unless this is an excessively low value, in which case, set
c     the primal activity to the minimum allowed value and set the up
c     and dn bits.
c
                        pr_act(vr_n) = -usr_tl_mx_iz_pr_act
                        vr_st = vr_st + up_dn
                     endif
                  else
                     pr_act_v = ubc(vr_n)*scl_v
                     if (pr_act_v .le. usr_tl_mx_iz_pr_act) then
c
c     Set the primal activity to the upper bound and set the dn bit...
c
                        pr_act(vr_n) = pr_act_v
                        vr_st = vr_st + dn_bt
                     else
c
c     ... unless this is an excessively high value, in which case, set
c     the primal activity to the maximum allowed value and set the up
c     and dn bits.
c
                        pr_act(vr_n) = usr_tl_mx_iz_pr_act
                        vr_st = vr_st + up_dn
                     endif
                  endif
               else
c
c     A FX variable: Set the variable to the bound
c
                  pr_act_v = lbc(vr_n)*scl_v
                  if (pr_act_v .ge. -usr_tl_mx_iz_pr_act) then
c
c     Set the primal activity to the lower bound...
c
                     pr_act(vr_n) = pr_act_v
                  else
c
c     ... unless this is an excessively low value, in which case, set
c     the primal activity to the minimum allowed value and set the up
c     and dn bits.
c
                     pr_act(vr_n) = -usr_tl_mx_iz_pr_act
                     vr_st = vr_st + up_dn
                  endif
               endif
            endif
         else if (iand(vr_st, bp_bt) .ne. 0) then
c
c     Variable is BP: Set it at its break point [above/below] according
c     to the smaller absolute value of the [upper/lower] cost.
c
            vr_st = vr_st - iand(vr_st, lb_ub)
            pr_act_v = cbp(vr_n)*scl_v
            if (pr_act_v .ge. -usr_tl_mx_iz_pr_act .and.
     &           pr_act_v .le. usr_tl_mx_iz_pr_act) then
               pr_act(vr_n) = pr_act_v
               if (abs(lbc(vr_n)) .le. abs(ubc(vr_n))) then
c
c     Record the variable as being logically below its breakpoint
c
                  vr_st = vr_st + dn_bt + ub_bt
               else
c
c     Record the variable as being logically above its breakpoint
c
                  vr_st = vr_st + up_bt + lb_bt
               endif
            else if (pr_act_v .lt. -usr_tl_mx_iz_pr_act) then
c
c     ... unless this is an excessively low value... in which case, set
c     the primal activity to the minimum allowed value and indicate that
c     it is distinctly above its breakpoint
c
               pr_act(vr_n) = -usr_tl_mx_iz_pr_act
               vr_st = vr_st + up_dn + lb_bt
            else if (pr_act_v .lt. -usr_tl_mx_iz_pr_act) then
c
c     ... or high value, in which case, set the primal activity to the
c     maximum allowed value and indicate that it is distinctly below its
c     breakpoint.
c
               pr_act(vr_n) = usr_tl_mx_iz_pr_act
               vr_st = vr_st + up_dn + ub_bt
            endif
         else
c
c     Variable is PWL
c
         endif
         st(vr_n) = vr_st
 10   continue
      return
      end
 
C->>> -------------------------------------------------> ems_se_rsmi <<<
c     Sets the RSMI bounds and cost according to the type of variable
c     and the activity. Determines the basic variables, from the status,
c     maintaining any existing basis if possible.
c
      subroutine ems_se_rsmi(
     &     cbp, lbc, ubc, scl,
     &     st, pr_act, vr_in_r,
     &     rsmi_co, rsmi_lb, rsmi_ub)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      include 'EMSMSGN.INC'
      double precision cbp(0:mx_n_c+n_r)
      double precision lbc(0:mx_n_c+n_r)
      double precision ubc(0:mx_n_c+n_r)
      double precision scl(0:mx_n_c+n_r)
      integer st(0:mx_n_c+mx_n_r)
      double precision pr_act(0:mx_n_c+n_r)
      integer vr_in_r(0:n_r)
      double precision rsmi_co(0:mx_n_c+n_r)
      double precision rsmi_lb(0:mx_n_c+n_r)
      double precision rsmi_ub(0:mx_n_c+n_r)
      integer ix_n, vr_n, rsmi_vr_n
c      integer pwl_vr_n, pwl_vr_da_sa, pwl_vr_da_f_en
      integer r_n
      integer n_vr_in_r, n_vr_in_c
      double precision scl_v
 
      if (rq_inv .eq. rq_inv_no_rq_inv) then
c
c     If trying to avoid reinversion, check that the existing vr_in_r
c     consists of just basic variable.
c
         do 10, r_n = 1, n_r
            if (iand(st(vr_in_r(r_n)), bc_bt) .eq. 0) then
               rq_inv = rq_inv_nw_bs
               ml_da_st_msk =
     &              ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_inv)
               go to 100
            end if
 10      continue
      end if
 100  continue
      n_vr_in_r = 0
      n_vr_in_c = 0
      do 110, ix_n = 1, n_r + n_c
         if (ix_n .le. n_c) then
            rsmi_vr_n = ix_n
         else
            rsmi_vr_n = ix_n + (mx_n_c-n_c)
         endif
         if (iand(st(rsmi_vr_n), su_vr_bs_bt) .ne. undn_vr_bs_st) then
            if (iand(st(rsmi_vr_n), bc_bt) .ne. 0) then
               if (n_vr_in_r .eq. n_r) then
c
c     Make the variable nonbasic if there is already a complete basis.
c
                  if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9900)
                  call ems_msg_wr_li(rsmi_msg_n)
                  rq_inv = rq_inv_nw_bs
                  ml_da_st_msk =
     &                 ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_inv)
                  st(rsmi_vr_n) = st(rsmi_vr_n) - bc_bt
               endif
            else
               if (n_vr_in_c .eq. n_c) then
c
c     Make the variable basic if there is already a complete non-basis.
c
                  if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9910)
                  call ems_msg_wr_li(rsmi_msg_n)
                  rq_inv = rq_inv_nw_bs
                  ml_da_st_msk =
     &                 ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_inv)
                  st(rsmi_vr_n) = st(rsmi_vr_n) + bc_bt
               endif
            endif
            if (iand(st(rsmi_vr_n), bc_bt) .ne. 0) then
c
c     The variable is basic.
c
               n_vr_in_r = n_vr_in_r + 1
               if (rq_inv .ne. rq_inv_no_rq_inv)
     &              vr_in_r(n_vr_in_r) = rsmi_vr_n
            else
c
c     The variable is nonbasic.
c
               n_vr_in_c = n_vr_in_c + 1
            end if
         endif
 110  continue
c
c     Second pass: Complete the sets of nonbasic and basic variables
c     (if necessary) using variables given the uncertain basic/nonbasic
c     status. Pass through all such variables now, starting with the
c     structurals, completing the set of nonbasic variables and then the
c     set of basic variables.
c
c     Initialise rsmi_lb/co/ub and corresponding status
c
      scl_v = one
      do 120, ix_n = 1, n_r + n_c
         if (ix_n .le. n_c) then
            vr_n =      ix_n
            rsmi_vr_n = ix_n
         else
            vr_n =      ix_n + (mx_n_c-n_c)
            rsmi_vr_n = ix_n + (mx_n_c-n_c)
         endif
c
c     Scale in forming the RSMI cost, lower bound and upper bound.
c     NB avoid scaling infinite bounds to be less than infinity.
c     If finite bounds are scaled to be infinite (unlikely) then this
c     shouldn't matter.
c
         if (iand(ml_da_st_msk, ml_da_st_scl_ml) .ne. 0)
     &        scl_v = scl(vr_n)
         if (iand(st(rsmi_vr_n), su_vr_bs_bt) .eq. undn_vr_bs_st) then
            st(rsmi_vr_n) = st(rsmi_vr_n) - undn_vr_bs_st
            if (n_vr_in_c .lt. n_c) then
c
c     Make the variable nonbasic
c
               n_vr_in_c = n_vr_in_c + 1
               st(rsmi_vr_n) = st(rsmi_vr_n) + non_bc_vr_bs_st
            else if (n_vr_in_r .lt. n_r) then
c
c     Make the variable basic
c
               n_vr_in_r = n_vr_in_r + 1
               vr_in_r(n_vr_in_r) = rsmi_vr_n
               st(rsmi_vr_n) = st(rsmi_vr_n) + bc_vr_bs_st
            else
               goto 8000
            endif
         endif
         if (iand(st(rsmi_vr_n), alt_bt) .eq. 0) then
c
c     Variable is Standard
c
            call ems_se_rsmi_std_vr(
     &           lbc(vr_n), cbp(vr_n), ubc(vr_n),
     &           rsmi_lb(rsmi_vr_n),
     &           rsmi_co(rsmi_vr_n),
     &           rsmi_ub(rsmi_vr_n),
     &           scl_v,
     &           st(rsmi_vr_n))
         else if (iand(st(rsmi_vr_n), bp_bt) .ne. 0) then
c
c     Variable is BP
c
            call ems_se_rsmi_bp_vr(
     &           lbc(vr_n), cbp(vr_n), ubc(vr_n),
     &           rsmi_lb(rsmi_vr_n),
     &           rsmi_co(rsmi_vr_n),
     &           rsmi_ub(rsmi_vr_n),
     &           pr_act(rsmi_vr_n),
     &           scl_v,
     &           st(rsmi_vr_n))
         else
c
c     Variable is PWL
c
         end if
 120  continue
c
c     Check that there is the right number of basic and nonbasic
c     variables.
c
      if (n_vr_in_c .ne. n_c .or. n_vr_in_r .ne. n_r) goto 8010
c
c     Record where the variable appears in vr_in_r.
c
      do 210, r_n = 1, n_r
         rsmi_vr_n = vr_in_r(r_n)
         st(rsmi_vr_n) = st(rsmi_vr_n) -
     &        iand(st(rsmi_vr_n), mx_mx_ml_a_dim) + r_n
 210  continue
      if (rq_inv .ne. rq_inv_no_rq_inv) then
c
c     Set av_eta_dse to be negative, in which case it is set to an
c     intelligent value after the first INVERT. NB Make sure it is set
c     to an intelligent value if a logical basis is identified and not
c     INVERTed.
c
         av_eta_dse = -one
      endif
c
c     Indicate that the model has vr_in_r correct.
c
      ml_da_st_msk = ior(ml_da_st_msk, ml_da_st_vr_in_r)
 7000 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
     &     n_vr_in_c, n_c,
     &     n_vr_in_r, n_r
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)
     &     n_vr_in_c, n_c,
     &     n_vr_in_r, n_r
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9900 format('Too many basic vars in status list')
 9910 format('Too many nonbasic vars in status list')
 9800 format('n_vr_in_c .ge. n_c .and. n_vr_in_r .ge. n_r',
     &     ' but there are still variables of uncertain status ',
     &     i9, i9, i9, i9)
 9801 format('n_vr_in_c .ne. n_c .or. n_vr_in_r .ne. n_r',
     &     i9, i9, i9, i9)
      end
 
C->>> ----------------------------------------------> ems_iz_vr_in_r <<<
c     Initialises vr_in_r from the status.
c     A variable is
c     Basic    if bc_bt and at least one of up_bt and dn_bt are set
c     Nonbasic if bc_bt is not set
c     Either   if bc_bt and  neither     of up_bt or  dn_bt is  set
c
c     After all the basic variables have been identified, an incomplete
c     basis is extended using variables which are neither Basic nor
c     Nonbasic
c
      subroutine ems_iz_vr_in_r(st, vr_in_r)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      include 'EMSMSGN.INC'
      integer st(0:mx_n_c+mx_n_r)
      integer vr_in_r(0:n_r)
      integer ix_n, vr_n, vr_st
      integer n_vr_in_r, n_vr_in_c
 
      n_vr_in_r = 0
      n_vr_in_c = 0
      do 110, ix_n = 1, n_r + n_c
         if (ix_n .le. n_c) then
            vr_n = ix_n
         else
            vr_n = ix_n + (mx_n_c-n_c)
         endif
         vr_st = st(vr_n)
         if (iand(vr_st, su_vr_bs_bt) .eq. undn_vr_bs_st) goto 110
         if (iand(vr_st, bc_bt) .ne. 0) then
            if (n_vr_in_r .eq. n_r) then
c
c     Make the variable nonbasic if there is already a complete basis.
c
               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9900)
               call ems_msg_wr_li(rsmi_msg_n)
               rq_inv = rq_inv_nw_bs
               ml_da_st_msk =
     &              ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_inv)
               vr_st = vr_st - bc_bt
            endif
         else
            if (n_vr_in_c .eq. n_c) then
c
c     Make the variable basic if there is already a complete non-basis.
c
               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9910)
               call ems_msg_wr_li(rsmi_msg_n)
               rq_inv = rq_inv_nw_bs
               ml_da_st_msk =
     &              ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_inv)
               vr_st = ior(vr_st, bc_vr_bs_st)
            endif
         endif
         if (iand(vr_st, bc_bt) .ne. 0) then
c
c     The variable is basic.
c
            n_vr_in_r = n_vr_in_r + 1
            vr_in_r(n_vr_in_r) = vr_n
            vr_st = vr_st - iand(vr_st, mx_mx_ml_a_dim) + n_vr_in_r
         else
c
c     The variable is nonbasic.
c
            n_vr_in_c = n_vr_in_c + 1
         end if
         st(vr_n) = vr_st
 110  continue
      if (n_vr_in_r+n_vr_in_c .ge. n_r+n_c) goto 1000
c
c     Second pass: Complete the sets of nonbasic and basic variables
c     (if necessary) using variables given the uncertain basic/nonbasic
c     status. Pass through all such variables now, starting with the
c     structurals, completing the set of nonbasic variables and then the
c     set of basic variables.
c
      do 210, ix_n = 1, n_r + n_c
         if (ix_n .le. n_c) then
            vr_n = ix_n
         else
            vr_n = ix_n + (mx_n_c-n_c)
         endif
         vr_st = st(vr_n)
         if (iand(vr_st, su_vr_bs_bt) .ne. undn_vr_bs_st) goto 210
         vr_st = vr_st - iand(vr_st, su_vr_bs_bt)
         if (n_vr_in_c .lt. n_c) then
c
c     Make the variable nonbasic
c
            n_vr_in_c = n_vr_in_c + 1
            vr_st = vr_st + non_bc_vr_bs_st
         else if (n_vr_in_r .lt. n_r) then
c
c     Make the variable basic
c
            n_vr_in_r = n_vr_in_r + 1
            vr_in_r(n_vr_in_r) = vr_n
            vr_st = vr_st + bc_vr_bs_st
         else
            goto 8000
         endif
 210  continue
 1000 continue
c
c     Check that there is the right number of basic and nonbasic
c     variables.
c
      if (n_vr_in_c .ne. n_c .or. n_vr_in_r .ne. n_r) goto 8010
c
c     Indicate that vr_in_r is correct for the model.
c
      ml_da_st_msk = ior(ml_da_st_msk, ml_da_st_vr_in_r)
c
c     Indicate that the following are not correct for the model:
c
c     vr_in_c, INVERT, status/primal activities, basic primal
c     activities, edge weights and row-wise representation of matrix
c     columns being priced.
c
      ml_da_st_msk = ml_da_st_msk
     &     - iand(ml_da_st_msk, ml_da_st_vr_in_c)
     &     - iand(ml_da_st_msk, ml_da_st_inv)
     &     - iand(ml_da_st_msk, ml_da_st_vr_st_fm_act)
     &     - iand(ml_da_st_msk, ml_da_st_bc_pr_act)
     &     - iand(ml_da_st_msk, ml_da_st_ed_wt)
     &     - iand(ml_da_st_msk, ml_da_st_r_mtx)
 7000 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
     &     n_vr_in_c, n_c,
     &     n_vr_in_r, n_r
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)
     &     n_vr_in_c, n_c,
     &     n_vr_in_r, n_r
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9900 format('Too many basic vars in status list')
 9910 format('Too many nonbasic vars in status list')
 9800 format('n_vr_in_c .ge. n_c .and. n_vr_in_r .ge. n_r',
     &     ' but there are still variables of uncertain status ',
     &     i9, i9, i9, i9)
 9801 format('n_vr_in_c .ne. n_c .or. n_vr_in_r .ne. n_r',
     &     i9, i9, i9, i9)
      end
 
C->>> ----------------------------------------------> ems_iz_rsmi_bd <<<
c     Initialise the RSMI bounds and cost according to the type of
c     variable and the activity.
c
      subroutine ems_iz_rsmi_bd(
     &     cbp, lbc, ubc, scl,
     &     st, pr_act, vr_in_r,
     &     rsmi_co, rsmi_lb, rsmi_ub)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      include 'EMSMSGN.INC'
      double precision cbp(0:mx_n_c+n_r)
      double precision lbc(0:mx_n_c+n_r)
      double precision ubc(0:mx_n_c+n_r)
      double precision scl(0:mx_n_c+n_r)
      integer st(0:mx_n_c+mx_n_r)
      double precision pr_act(0:mx_n_c+n_r)
      integer vr_in_r(0:n_r)
      double precision rsmi_co(0:mx_n_c+n_r)
      double precision rsmi_lb(0:mx_n_c+n_r)
      double precision rsmi_ub(0:mx_n_c+n_r)
      integer ix_n, vr_n, vr_st
      double precision scl_v, rcp_scl_v
      double precision pwl_lc, pwl_bp, pwl_uc
c
c     Initialise rsmi_lb/co/ub and corresponding status
c
      scl_v = one
      do 110, ix_n = 1, n_r + n_c
         if (ix_n .le. n_c) then
            vr_n = ix_n
         else
            vr_n = ix_n + (mx_n_c-n_c)
         endif
c
c     Scale in forming the RSMI cost, lower bound and upper bound.
c     NB avoid scaling infinite bounds to be less than infinity.
c     If finite bounds are scaled to be infinite (unlikely) then this
c     shouldn't matter.
c
         if (iand(ml_da_st_msk, ml_da_st_scl_ml) .ne. 0)
     &        scl_v = scl(vr_n)
         vr_st = st(vr_n)
         if (iand(vr_st, alt_bt) .eq. 0) then
c
c     Variable is Standard
c
            if (lbc(vr_n) .gt. -inf) then
               rsmi_lb(vr_n) = min(max(lbc(vr_n)*scl_v, -inf), inf)
            else
               rsmi_lb(vr_n) = -inf
            endif
            rsmi_co(vr_n) =    min(max(cbp(vr_n)/scl_v, -inf), inf)
            if (ubc(vr_n) .lt.  inf) then
               rsmi_ub(vr_n) = min(max(ubc(vr_n)*scl_v, -inf), inf)
            else
               rsmi_ub(vr_n) = inf
            endif
         else if (iand(vr_st, bp_bt) .ne. 0) then
c
c     Variable is BP
c
            rcp_scl_v = one/scl_v
            pwl_lc = min(max(lbc(vr_n)*rcp_scl_v, -inf), inf)
            pwl_bp = min(max(cbp(vr_n)*scl_v,     -inf), inf)
            pwl_uc = min(max(ubc(vr_n)*rcp_scl_v, -inf), inf)
            if (iand(vr_st, ub_bt) .ne. 0) then
               rsmi_lb(vr_n) = pwl_uc - pwl_lc
               rsmi_co(vr_n) = pwl_lc
               rsmi_ub(vr_n) = pwl_bp
            else
               rsmi_lb(vr_n) = pwl_bp
               rsmi_co(vr_n) = pwl_uc
               rsmi_ub(vr_n) = pwl_lc - pwl_uc
            endif
         else
c
c     Variable is PWL
c
         end if
         st(vr_n) = vr_st
 110  continue
      return
      end
 
C->>> ------------------------------------------> ems_se_rsmi_std_vr <<<
c     Set the values of rsmi_vr_lb, rsmi_vr_co, rsmi_vr_ub and the
c     LB/UB bits in the status.
c
      subroutine ems_se_rsmi_std_vr(
     &     ml_vr_lb, ml_vr_co, ml_vr_ub,
     &     rsmi_vr_lb, rsmi_vr_co, rsmi_vr_ub,
     &     scl_v,
     &     vr_st)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      integer vr_st
      double precision ml_vr_lb, ml_vr_co, ml_vr_ub
      double precision rsmi_vr_lb, rsmi_vr_co, rsmi_vr_ub
      double precision scl_v
 
      if (ml_vr_lb .gt. -inf) then
         rsmi_vr_lb = ml_vr_lb*scl_v
         vr_st = ior(vr_st, lb_bt)
      else
         rsmi_vr_lb = -inf
         vr_st = vr_st - iand(vr_st, lb_bt)
      endif
      rsmi_vr_co = ml_vr_co/scl_v
      if (ml_vr_ub .lt.  inf) then
         rsmi_vr_ub = ml_vr_ub*scl_v
         vr_st = ior(vr_st, ub_bt)
      else
         rsmi_vr_ub = inf
         vr_st = vr_st - iand(vr_st, ub_bt)
      endif
      return
      end
 
C->>> -------------------------------------------> ems_se_rsmi_bp_vr <<<
c     Set the values of rsmi_vr_lb, rsmi_vr_co, rsmi_vr_ub and the
c     LB/UB bits in the status.
c
      subroutine ems_se_rsmi_bp_vr(
     &     ml_vr_lb, ml_vr_co, ml_vr_ub,
     &     rsmi_vr_lb, rsmi_vr_co, rsmi_vr_ub,
     &     rsmi_vr_pr_act,
     &     scl_v,
     &     vr_st)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RLCTVR.INC'
      integer vr_st
      double precision ml_vr_lb, ml_vr_co, ml_vr_ub
      double precision rsmi_vr_lb, rsmi_vr_co, rsmi_vr_ub
      double precision rsmi_vr_pr_act
      double precision scl_v
      double precision pwl_lc, pwl_bp, pwl_uc
      double precision rsdu, rcp_scl_v
 
      rcp_scl_v = one/scl_v
      pwl_lc = ml_vr_lb*rcp_scl_v
      pwl_bp = ml_vr_co*scl_v
      pwl_uc = ml_vr_ub*rcp_scl_v
      rsdu = rsmi_vr_pr_act - pwl_bp
      if (rsdu .lt. -tl_pr_ifs .or.
     &     rsdu .le. tl_pr_ifs .and. iand(vr_st, ub_bt) .ne. 0) then
c
c     Variable is below its break point or degenerate and logically
c     below its breakpoint:
c     Set lb:co:ub to uc-lc:lc:bp
c
         rsmi_vr_lb = pwl_uc - pwl_lc
         rsmi_vr_co = pwl_lc
         rsmi_vr_ub = pwl_bp
         vr_st = ior(vr_st, ub_bt) - iand(vr_st, lb_bt)
      else
c
c     Variable is above its break point or degenerate and logically
c     above its breakpoint:
c     Set lb:co:ub to bp:uc:lc-uc
c
         rsmi_vr_lb = pwl_bp
         rsmi_vr_co = pwl_uc
         rsmi_vr_ub = pwl_lc - pwl_uc
         vr_st = ior(vr_st, lb_bt) - iand(vr_st, ub_bt)
      endif
      return
      end
