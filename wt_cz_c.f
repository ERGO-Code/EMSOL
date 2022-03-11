C->>> -----------------------------------------> ems_sq_ed_wt_cz_1_c <<<
c     Returns the best candidate variable from the set of candidates
c     when the edge weights are squared.
c     If bst_vr_n = 0 then none of the candidates is attractive.
c
      subroutine ems_sq_ed_wt_cz_1_c(
     &     bp_swp_sd, en_c_n, en_sn_n,
     &     rsmi_lb, rsmi_ub, st,
     &     vr_in_c, du_act, ed_wt)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICS.INC'
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      include 'RLCTVR.INC'
      integer bp_swp_sd, en_c_n, en_sn_n
      double precision rsmi_lb(0:mx_n_c+n_r)
      double precision rsmi_ub(0:mx_n_c+n_r)
      integer st(0:mx_n_c+n_r)
      integer vr_in_c(-vr_in_c_n_sn:n_c)
      double precision du_act(0:mx_n_c+n_r)
      double precision ed_wt(0:mx_n_c+n_r)
      integer c_n, vr_n
      double precision vr_du_act, abs_vr_du_act, sq_du_act
      double precision mx_rao
 
c      do 1, c_n = 1, vr_in_c(os_lg_in_c_te_fx_p)
c         vr_n = vr_in_c(c_n)
c         write(*, '(i3, 2(2x, g11.4))')
c     &        vr_n, du_act(vr_n), sqrt(ed_wt(vr_n))
c 1    continue
c      do 2, c_n = vr_in_c(os_struc_in_c_f_p_m1) + 1,
c     &     vr_in_c(os_struc_in_c_te_fx_p)
c         vr_n = vr_in_c(c_n)
c         write(*, '(i3, 2(2x, g11.4))')
c     &        vr_n, du_act(vr_n), sqrt(ed_wt(vr_n))
c 2    continue
c
c     Check whether the edge weight information is correct.
c     ...which implies that the strategy is appropriate.
c
      if (iand(ml_da_st_msk, ml_da_st_ed_wt) .eq. 0) goto 8000
      mx_rao = zero
      do 10, c_n = 1, vr_in_c(os_lg_in_c_ab_bp_p)
c
c     Above break point:
c
         vr_n = vr_in_c(c_n)
c
c     lb:co:ub are bp:uco:lco-uco
c
c     Free to move up with given dual activity
c
         vr_du_act = du_act(vr_n)
         if (-vr_du_act .gt. tl_du_ifs) then
            sq_du_act = vr_du_act*vr_du_act
            if (sq_du_act .gt. mx_rao*ed_wt(vr_n)) then
               nx_vr_t_en_bs = vr_n
               mx_rao = sq_du_act/ed_wt(vr_n)
               bp_swp_sd = 0
            end if
         else
c
c     NB: With convex cost functions it cannot be attractive to move
c     both ways.
c
c     Free to move down with modified dual activity
c
            vr_du_act = vr_du_act + pr_co_mu*rsmi_ub(vr_n)
            if (vr_du_act .gt. tl_du_ifs) then
               sq_du_act = vr_du_act*vr_du_act
               if (sq_du_act .gt. mx_rao*ed_wt(vr_n)) then
                  nx_vr_t_en_bs = vr_n
                  mx_rao = sq_du_act/ed_wt(vr_n)
                  bp_swp_sd = -1
c
c     Record the column and section number in order to swap sides if
c     this variable is chosen.
c
                  en_c_n = c_n
                  en_sn_n = lg_sn_n_os + vr_in_c_sn_ty_ab_bp
               end if
            end if
         end if
 10   continue
      do 20, c_n = c_n, vr_in_c(os_lg_in_c_bw_bp_p)
c
c     Below break point:
c
         vr_n = vr_in_c(c_n)
c
c     lb:co:ub are uco-lco:lco:bp
c
c     Free to move down with given dual activity
c
         vr_du_act = du_act(vr_n)
         if (vr_du_act .gt. tl_du_ifs) then
            sq_du_act = vr_du_act*vr_du_act
            if (sq_du_act .gt. mx_rao*ed_wt(vr_n)) then
               nx_vr_t_en_bs = vr_n
               mx_rao = sq_du_act/ed_wt(vr_n)
               bp_swp_sd = 0
            end if
         else
c
c     NB: With convex cost functions it cannot be attractive to move
c     both ways.
c
c     Free to move up with modified dual activity
c
            vr_du_act = vr_du_act + pr_co_mu*rsmi_lb(vr_n)
            if (-vr_du_act .gt. tl_du_ifs) then
               sq_du_act = vr_du_act*vr_du_act
               if (sq_du_act .gt. mx_rao*ed_wt(vr_n)) then
                  nx_vr_t_en_bs = vr_n
                  mx_rao = sq_du_act/ed_wt(vr_n)
                  bp_swp_sd = 1
c
c     Record the column and section number in order to swap sides if
c     this variable is chosen.
c
                  en_c_n = c_n
                  en_sn_n = lg_sn_n_os + vr_in_c_sn_ty_bw_bp
               end if
            end if
         end if
 20   continue
      do 30, c_n = c_n, vr_in_c(os_lg_in_c_up_dn_p)
         vr_n = vr_in_c(c_n)
c
c     Free to move up or down with given dual activity.
c
         abs_vr_du_act = abs(du_act(vr_n))
         if (abs_vr_du_act .gt. tl_du_ifs) then
            sq_du_act = abs_vr_du_act*abs_vr_du_act
            if (sq_du_act .gt. mx_rao*ed_wt(vr_n)) then
               nx_vr_t_en_bs = vr_n
               mx_rao = sq_du_act/ed_wt(vr_n)
c
c     Set bp_swp_sd = 0 in case this is a breakpoint variable.
c
               bp_swp_sd = 0
            end if
         end if
 30   continue
      if (lp_ph .eq. 1 .and.
     &     iand(cz_c_msk, cz_c_bk_bd_bt) .ne. 0) then
         do 40, c_n = c_n, vr_in_c(os_lg_in_c_up_p)
            vr_n = vr_in_c(c_n)
c
c     Free to move up (from lower bound) with given dual activity.
c
            vr_du_act = du_act(vr_n)
            if (-vr_du_act .gt. tl_du_ifs) then
               sq_du_act = vr_du_act*vr_du_act
               if (sq_du_act .gt. mx_rao*ed_wt(vr_n)) then
                  nx_vr_t_en_bs = vr_n
                  mx_rao = sq_du_act/ed_wt(vr_n)
               end if
            else
c
c     NB: It cannot be attractive to move up from a bound and down
c     through it.
c
c     Free to move down (through bound) with modified dual activity
c
               vr_du_act = vr_du_act - one
               if (vr_du_act .gt. tl_du_ifs) then
                  sq_du_act = vr_du_act*vr_du_act
                  if (sq_du_act .gt. mx_rao*ed_wt(vr_n)) then
                     nx_vr_t_en_bs = vr_n
                     mx_rao = sq_du_act/ed_wt(vr_n)
                  end if
               end if
            end if
 40      continue
         do 45, c_n = c_n, vr_in_c(os_lg_in_c_dn_p)
            vr_n = vr_in_c(c_n)
c
c     Free to move down (from upper bound) with given dual activity.
c
            vr_du_act = du_act(vr_n)
            if (vr_du_act .gt. tl_du_ifs) then
               sq_du_act = vr_du_act*vr_du_act
               if (sq_du_act .gt. mx_rao*ed_wt(vr_n)) then
                  nx_vr_t_en_bs = vr_n
                  mx_rao = sq_du_act/ed_wt(vr_n)
               end if
            else
c
c     NB: It cannot be attractive to move down from a bound and up
c     through it.
c
c     Free to move up (through bound) with modified dual activity
c
               vr_du_act = vr_du_act + one
               if (-vr_du_act .gt. tl_du_ifs) then
                  sq_du_act = vr_du_act*vr_du_act
                  if (sq_du_act .gt. mx_rao*ed_wt(vr_n)) then
                     nx_vr_t_en_bs = vr_n
                     mx_rao = sq_du_act/ed_wt(vr_n)
                  end if
               end if
            end if
 45      continue
c
c     Consider moving from a (temporarily) fixed value.
c
         do 47, c_n = c_n, vr_in_c(os_lg_in_c_te_fx_p)
            vr_n = vr_in_c(c_n)
            vr_du_act = du_act(vr_n) + one
            if (-vr_du_act .gt. tl_du_ifs) then
c
c     It is attractive to move up from a fixed value.
c
               sq_du_act = vr_du_act*vr_du_act
               if (sq_du_act .gt. mx_rao*ed_wt(vr_n)) then
                  nx_vr_t_en_bs = vr_n
                  mx_rao = sq_du_act/ed_wt(vr_n)
               end if
            else
c
c     NB: It cannot be attractive to move up from a fixed value and down
c     from it
c
c     It is attractive to move down from a fixed value.
c
               vr_du_act = du_act(vr_n) - one
               if (vr_du_act .gt. tl_du_ifs) then
                  sq_du_act = vr_du_act*vr_du_act
                  if (sq_du_act .gt. mx_rao*ed_wt(vr_n)) then
                     nx_vr_t_en_bs = vr_n
                     mx_rao = sq_du_act/ed_wt(vr_n)
                  end if
               end if
            end if
 47      continue
      else
         do 50, c_n = c_n, vr_in_c(os_lg_in_c_up_p)
            vr_n = vr_in_c(c_n)
c
c     Free to move up with given dual activity.
c
            vr_du_act = du_act(vr_n)
            if (-vr_du_act .gt. tl_du_ifs) then
               sq_du_act = vr_du_act*vr_du_act
               if (sq_du_act .gt. mx_rao*ed_wt(vr_n)) then
                  nx_vr_t_en_bs = vr_n
                  mx_rao = sq_du_act/ed_wt(vr_n)
               end if
            end if
 50      continue
         do 55, c_n = c_n, vr_in_c(os_lg_in_c_dn_p)
            vr_n = vr_in_c(c_n)
c
c     Free to move down with given dual activity.
c
            vr_du_act = du_act(vr_n)
            if (vr_du_act .gt. tl_du_ifs) then
               sq_du_act = vr_du_act*vr_du_act
               if (sq_du_act .gt. mx_rao*ed_wt(vr_n)) then
                  nx_vr_t_en_bs = vr_n
                  mx_rao = sq_du_act/ed_wt(vr_n)
               end if
            end if
 55      continue
      endif
      do 110, c_n = vr_in_c(os_struc_in_c_f_p_m1) + 1,
     &     vr_in_c(os_struc_in_c_ab_bp_p)
c
c     Above break point:
c
         vr_n = vr_in_c(c_n)
c
c     lb:co:ub are bp:uco:lco-uco
c
c     Free to move up with given dual activity
c
         vr_du_act = du_act(vr_n)
         if (-vr_du_act .gt. tl_du_ifs) then
            sq_du_act = vr_du_act*vr_du_act
            if (sq_du_act .gt. mx_rao*ed_wt(vr_n)) then
               nx_vr_t_en_bs = vr_n
               mx_rao = sq_du_act/ed_wt(vr_n)
               bp_swp_sd = 0
            end if
         else
c
c     NB: With convex cost functions it cannot be attractive to move
c     both ways.
c
c     Free to move down with modified dual activity
c
            vr_du_act = vr_du_act + pr_co_mu*rsmi_ub(vr_n)
            if (vr_du_act .gt. tl_du_ifs) then
               sq_du_act = vr_du_act*vr_du_act
               if (sq_du_act .gt. mx_rao*ed_wt(vr_n)) then
                  nx_vr_t_en_bs = vr_n
                  mx_rao = sq_du_act/ed_wt(vr_n)
                  bp_swp_sd = -1
c
c     Record the column and section number in order to swap sides if
c     this variable is chosen.
c
                  en_c_n = c_n
                  en_sn_n = lg_sn_n_os + vr_in_c_sn_ty_ab_bp
               end if
            end if
         end if
 110  continue
      do 120, c_n = c_n, vr_in_c(os_struc_in_c_bw_bp_p)
c
c     Below break point:
c
         vr_n = vr_in_c(c_n)
c
c     lb:co:ub are uco-lco:lco:bp
c
c     Free to move down with given dual activity
c
         vr_du_act = du_act(vr_n)
         if (vr_du_act .gt. tl_du_ifs) then
            sq_du_act = vr_du_act*vr_du_act
            if (sq_du_act .gt. mx_rao*ed_wt(vr_n)) then
               nx_vr_t_en_bs = vr_n
               mx_rao = sq_du_act/ed_wt(vr_n)
               bp_swp_sd = 0
            end if
         else
c
c     NB: With convex cost functions it cannot be attractive to move
c     both ways.
c
c     Free to move up with modified dual activity
c
            vr_du_act = vr_du_act + pr_co_mu*rsmi_lb(vr_n)
            if (-vr_du_act .gt. tl_du_ifs) then
               sq_du_act = vr_du_act*vr_du_act
               if (sq_du_act .gt. mx_rao*ed_wt(vr_n)) then
                  nx_vr_t_en_bs = vr_n
                  mx_rao = sq_du_act/ed_wt(vr_n)
                  bp_swp_sd = 1
c
c     Record the column and section number in order to swap sides if
c     this variable is chosen.
c
                  en_c_n = c_n
                  en_sn_n = lg_sn_n_os + vr_in_c_sn_ty_bw_bp
               end if
            end if
         end if
 120  continue
      do 130, c_n = c_n, vr_in_c(os_struc_in_c_up_dn_p)
         vr_n = vr_in_c(c_n)
c
c     Free to move up or down with given dual activity.
c
         abs_vr_du_act = abs(du_act(vr_n))
         if (abs_vr_du_act .gt. tl_du_ifs) then
            sq_du_act = abs_vr_du_act*abs_vr_du_act
            if (sq_du_act .gt. mx_rao*ed_wt(vr_n)) then
               nx_vr_t_en_bs = vr_n
               mx_rao = sq_du_act/ed_wt(vr_n)
            end if
         end if
 130  continue
      if (lp_ph .eq. 1 .and.
     &     iand(cz_c_msk, cz_c_bk_bd_bt) .ne. 0) then
         do 140, c_n = c_n, vr_in_c(os_struc_in_c_up_p)
            vr_n = vr_in_c(c_n)
c
c     Free to move up (from lower bound) with given dual activity.
c
            vr_du_act = du_act(vr_n)
            if (-vr_du_act .gt. tl_du_ifs) then
               sq_du_act = vr_du_act*vr_du_act
               if (sq_du_act .gt. mx_rao*ed_wt(vr_n)) then
                  nx_vr_t_en_bs = vr_n
                  mx_rao = sq_du_act/ed_wt(vr_n)
               end if
            else
c
c     NB: It cannot be attractive to move up from a bound and down
c     through it.
c
c     Free to move down (through bound) with modified dual activity
c
               vr_du_act = vr_du_act - one
               if (vr_du_act .gt. tl_du_ifs) then
                  sq_du_act = vr_du_act*vr_du_act
                  if (sq_du_act .gt. mx_rao*ed_wt(vr_n)) then
                     nx_vr_t_en_bs = vr_n
                     mx_rao = sq_du_act/ed_wt(vr_n)
                  end if
               end if
            end if
 140     continue
         do 145, c_n = c_n, vr_in_c(os_struc_in_c_dn_p)
            vr_n = vr_in_c(c_n)
c
c     Free to move down (from upper bound) with given dual activity.
c
            vr_du_act = du_act(vr_n)
            if (vr_du_act .gt. tl_du_ifs) then
               sq_du_act = vr_du_act*vr_du_act
               if (sq_du_act .gt. mx_rao*ed_wt(vr_n)) then
                  nx_vr_t_en_bs = vr_n
                  mx_rao = sq_du_act/ed_wt(vr_n)
               end if
            else
c
c     NB: It cannot be attractive to move down from a bound and up
c     through it.
c
c     Free to move up (through bound) with modified dual activity
c
               vr_du_act = vr_du_act + one
               if (-vr_du_act .gt. tl_du_ifs) then
                  sq_du_act = vr_du_act*vr_du_act
                  if (sq_du_act .gt. mx_rao*ed_wt(vr_n)) then
                     nx_vr_t_en_bs = vr_n
                     mx_rao = sq_du_act/ed_wt(vr_n)
                  end if
               end if
            end if
 145     continue
c
c     Consider moving from a (temporarily) fixed value.
c
         do 147, c_n = c_n, vr_in_c(os_struc_in_c_te_fx_p)
            vr_n = vr_in_c(c_n)
            vr_du_act = du_act(vr_n) + one
            if (-vr_du_act .gt. tl_du_ifs) then
c
c     It is attractive to move up from a fixed value.
c
               sq_du_act = vr_du_act*vr_du_act
               if (sq_du_act .gt. mx_rao*ed_wt(vr_n)) then
                  nx_vr_t_en_bs = vr_n
                  mx_rao = sq_du_act/ed_wt(vr_n)
               end if
            else
c
c     NB: It cannot be attractive to move up from a fixed value and down
c     from it
c
c     It is attractive to move down from a fixed value.
c
               vr_du_act = du_act(vr_n) - one
               if (vr_du_act .gt. tl_du_ifs) then
                  sq_du_act = vr_du_act*vr_du_act
                  if (sq_du_act .gt. mx_rao*ed_wt(vr_n)) then
                     nx_vr_t_en_bs = vr_n
                     mx_rao = sq_du_act/ed_wt(vr_n)
                  end if
               end if
            end if
 147     continue
      else
         do 150, c_n = c_n, vr_in_c(os_struc_in_c_up_p)
            vr_n = vr_in_c(c_n)
c
c     Free to move up with given dual activity.
c
            vr_du_act = du_act(vr_n)
            if (-vr_du_act .gt. tl_du_ifs) then
               sq_du_act = vr_du_act*vr_du_act
               if (sq_du_act .gt. mx_rao*ed_wt(vr_n)) then
                  nx_vr_t_en_bs = vr_n
                  mx_rao = sq_du_act/ed_wt(vr_n)
               end if
            end if
 150     continue
         do 155, c_n = c_n, vr_in_c(os_struc_in_c_dn_p)
            vr_n = vr_in_c(c_n)
c
c     Free to move down with given dual activity.
c
            vr_du_act = du_act(vr_n)
            if (vr_du_act .gt. tl_du_ifs) then
               sq_du_act = vr_du_act*vr_du_act
               if (sq_du_act .gt. mx_rao*ed_wt(vr_n)) then
                  nx_vr_t_en_bs = vr_n
                  mx_rao = sq_du_act/ed_wt(vr_n)
               end if
            end if
 155     continue
      endif
 7000 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9800 format('Calling ems_cz_1_c with incorrect edge weights')
      end
C->>> --------------------------------------------> ems_ed_wt_cz_1_c <<<
c     Returns the best candidate variable from the set of candidates.
c     If bst_vr_n = 0 then none of the candidates is attractive.
c
      subroutine ems_ed_wt_cz_1_c(
     &     bp_swp_sd, en_c_n, en_sn_n,
     &     rsmi_lb, rsmi_ub, st,
     &     vr_in_c, du_act, ed_wt)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICS.INC'
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      include 'RLCTVR.INC'
      integer bp_swp_sd, en_c_n, en_sn_n
      double precision rsmi_lb(0:mx_n_c+n_r)
      double precision rsmi_ub(0:mx_n_c+n_r)
      integer st(0:mx_n_c+n_r)
      integer vr_in_c(-vr_in_c_n_sn:n_c)
      double precision du_act(0:mx_n_c+n_r)
      double precision ed_wt(0:mx_n_c+n_r)
      integer c_n, vr_n
      double precision vr_du_act, abs_vr_du_act
      double precision mx_rao
 
c      do 1, c_n = 1, vr_in_c(os_lg_in_c_te_fx_p)
c         vr_n = vr_in_c(c_n)
c         write(*, '(i3, 2(2x, g11.4))')
c     &        vr_n, du_act(vr_n), ed_wt(vr_n)
c 1    continue
c      do 2, c_n = vr_in_c(os_struc_in_c_f_p_m1) + 1,
c     &     vr_in_c(os_struc_in_c_te_fx_p)
c         vr_n = vr_in_c(c_n)
c         write(*, '(i3, 2(2x, g11.4))')
c     &        vr_n, du_act(vr_n), ed_wt(vr_n)
c 2    continue
c
c     Check whether the edge weight information is correct.
c     ...which implies that the strategy is appropriate.
c
      if (iand(ml_da_st_msk, ml_da_st_ed_wt) .eq. 0) goto 8000
      mx_rao = zero
      do 10, c_n = 1, vr_in_c(os_lg_in_c_ab_bp_p)
c
c     Above break point:
c
         vr_n = vr_in_c(c_n)
c
c     lb:co:ub are bp:uco:lco-uco
c
c     Free to move up with given dual activity
c
         vr_du_act = du_act(vr_n)
         if (-vr_du_act .gt. tl_du_ifs) then
            abs_vr_du_act = -vr_du_act
            if (abs_vr_du_act .gt. mx_rao*ed_wt(vr_n)) then
               nx_vr_t_en_bs = vr_n
               mx_rao = abs_vr_du_act/ed_wt(vr_n)
               bp_swp_sd = 0
            end if
         else
c
c     NB: With convex cost functions it cannot be attractive to move
c     both ways.
c
c     Free to move down with modified dual activity
c
            vr_du_act = vr_du_act + pr_co_mu*rsmi_ub(vr_n)
            if (vr_du_act .gt. tl_du_ifs) then
               abs_vr_du_act = vr_du_act
               if (abs_vr_du_act .gt. mx_rao*ed_wt(vr_n)) then
                  nx_vr_t_en_bs = vr_n
                  mx_rao = abs_vr_du_act/ed_wt(vr_n)
                  bp_swp_sd = -1
c
c     Record the column and section number in order to swap sides if
c     this variable is chosen.
c
                  en_c_n = c_n
                  en_sn_n = lg_sn_n_os + vr_in_c_sn_ty_ab_bp
               end if
            end if
         end if
 10   continue
      do 20, c_n = c_n, vr_in_c(os_lg_in_c_bw_bp_p)
c
c     Below break point:
c
         vr_n = vr_in_c(c_n)
c
c     lb:co:ub are uco-lco:lco:bp
c
c     Free to move down with given dual activity
c
         vr_du_act = du_act(vr_n)
         if (vr_du_act .gt. tl_du_ifs) then
            abs_vr_du_act = vr_du_act
            if (abs_vr_du_act .gt. mx_rao*ed_wt(vr_n)) then
               nx_vr_t_en_bs = vr_n
               mx_rao = abs_vr_du_act/ed_wt(vr_n)
               bp_swp_sd = 0
            end if
         else
c
c     NB: With convex cost functions it cannot be attractive to move
c     both ways.
c
c     Free to move up with modified dual activity
c
            vr_du_act = vr_du_act + pr_co_mu*rsmi_lb(vr_n)
            if (-vr_du_act .gt. tl_du_ifs) then
               abs_vr_du_act = -vr_du_act
               if (abs_vr_du_act .gt. mx_rao*ed_wt(vr_n)) then
                  nx_vr_t_en_bs = vr_n
                  mx_rao = abs_vr_du_act/ed_wt(vr_n)
                  bp_swp_sd = 1
c
c     Record the column and section number in order to swap sides if
c     this variable is chosen.
c
                  en_c_n = c_n
                  en_sn_n = lg_sn_n_os + vr_in_c_sn_ty_bw_bp
               end if
            end if
         end if
 20   continue
      do 30, c_n = c_n, vr_in_c(os_lg_in_c_up_dn_p)
         vr_n = vr_in_c(c_n)
c
c     Free to move up or down with given dual activity.
c
         abs_vr_du_act = abs(du_act(vr_n))
         if (abs_vr_du_act .gt. tl_du_ifs) then
            if (abs_vr_du_act .gt. mx_rao*ed_wt(vr_n)) then
               nx_vr_t_en_bs = vr_n
               mx_rao = abs_vr_du_act/ed_wt(vr_n)
c
c     Set bp_swp_sd = 0 in case this is a breakpoint variable.
c
               bp_swp_sd = 0
            end if
         end if
 30   continue
      if (lp_ph .eq. 1 .and.
     &     iand(cz_c_msk, cz_c_bk_bd_bt) .ne. 0) then
         do 40, c_n = c_n, vr_in_c(os_lg_in_c_up_p)
            vr_n = vr_in_c(c_n)
c
c     Free to move up (from lower bound) with given dual activity.
c
            vr_du_act = du_act(vr_n)
            if (-vr_du_act .gt. tl_du_ifs) then
               abs_vr_du_act = -vr_du_act
               if (abs_vr_du_act .gt. mx_rao*ed_wt(vr_n)) then
                  nx_vr_t_en_bs = vr_n
                  mx_rao = abs_vr_du_act/ed_wt(vr_n)
               end if
            else
c
c     NB: It cannot be attractive to move up from a bound and down
c     through it.
c
c     Free to move down (through bound) with modified dual activity
c
               vr_du_act = vr_du_act - one
               if (vr_du_act .gt. tl_du_ifs) then
                  abs_vr_du_act = vr_du_act
                  if (abs_vr_du_act .gt. mx_rao*ed_wt(vr_n)) then
                     nx_vr_t_en_bs = vr_n
                     mx_rao = abs_vr_du_act/ed_wt(vr_n)
                  end if
               end if
            end if
 40      continue
         do 45, c_n = c_n, vr_in_c(os_lg_in_c_dn_p)
            vr_n = vr_in_c(c_n)
c
c     Free to move down (from upper bound) with given dual activity.
c
            vr_du_act = du_act(vr_n)
            if (vr_du_act .gt. tl_du_ifs) then
               abs_vr_du_act = vr_du_act
               if (abs_vr_du_act .gt. mx_rao*ed_wt(vr_n)) then
                  nx_vr_t_en_bs = vr_n
                  mx_rao = abs_vr_du_act/ed_wt(vr_n)
               end if
            else
c
c     NB: It cannot be attractive to move down from a bound and up
c     through it.
c
c     Free to move up (through bound) with modified dual activity
c
               vr_du_act = vr_du_act + one
               if (-vr_du_act .gt. tl_du_ifs) then
                  abs_vr_du_act = -vr_du_act
                  if (abs_vr_du_act .gt. mx_rao*ed_wt(vr_n)) then
                     nx_vr_t_en_bs = vr_n
                     mx_rao = abs_vr_du_act/ed_wt(vr_n)
                  end if
               end if
            end if
 45      continue
c
c     Consider moving from a (temporarily) fixed value.
c
         do 47, c_n = c_n, vr_in_c(os_lg_in_c_te_fx_p)
            vr_n = vr_in_c(c_n)
            vr_du_act = du_act(vr_n) + one
            if (-vr_du_act .gt. tl_du_ifs) then
c
c     It is attractive to move up from a fixed value.
c
               abs_vr_du_act = -vr_du_act
               if (abs_vr_du_act .gt. mx_rao*ed_wt(vr_n)) then
                  nx_vr_t_en_bs = vr_n
                  mx_rao = abs_vr_du_act/ed_wt(vr_n)
               end if
            else
c
c     NB: It cannot be attractive to move up from a fixed value and down
c     from it
c
c     It is attractive to move down from a fixed value.
c
               vr_du_act = du_act(vr_n) - one
               if (vr_du_act .gt. tl_du_ifs) then
                  abs_vr_du_act = vr_du_act
                  if (abs_vr_du_act .gt. mx_rao*ed_wt(vr_n)) then
                     nx_vr_t_en_bs = vr_n
                     mx_rao = abs_vr_du_act/ed_wt(vr_n)
                  end if
               end if
            end if
 47      continue
      else
         do 50, c_n = c_n, vr_in_c(os_lg_in_c_up_p)
            vr_n = vr_in_c(c_n)
c
c     Free to move up with given dual activity.
c
            vr_du_act = du_act(vr_n)
            if (-vr_du_act .gt. tl_du_ifs) then
               abs_vr_du_act = -vr_du_act
               if (abs_vr_du_act .gt. mx_rao*ed_wt(vr_n)) then
                  nx_vr_t_en_bs = vr_n
                  mx_rao = abs_vr_du_act/ed_wt(vr_n)
               end if
            end if
 50      continue
         do 55, c_n = c_n, vr_in_c(os_lg_in_c_dn_p)
            vr_n = vr_in_c(c_n)
c
c     Free to move down with given dual activity.
c
            vr_du_act = du_act(vr_n)
            if (vr_du_act .gt. tl_du_ifs) then
               abs_vr_du_act = vr_du_act
               if (abs_vr_du_act .gt. mx_rao*ed_wt(vr_n)) then
                  nx_vr_t_en_bs = vr_n
                  mx_rao = abs_vr_du_act/ed_wt(vr_n)
               end if
            end if
 55      continue
      endif
      do 110, c_n = vr_in_c(os_struc_in_c_f_p_m1) + 1,
     &     vr_in_c(os_struc_in_c_ab_bp_p)
c
c     Above break point:
c
         vr_n = vr_in_c(c_n)
c
c     lb:co:ub are bp:uco:lco-uco
c
c     Free to move up with given dual activity
c
         vr_du_act = du_act(vr_n)
         if (-vr_du_act .gt. tl_du_ifs) then
            abs_vr_du_act = -vr_du_act
            if (abs_vr_du_act .gt. mx_rao*ed_wt(vr_n)) then
               nx_vr_t_en_bs = vr_n
               mx_rao = abs_vr_du_act/ed_wt(vr_n)
               bp_swp_sd = 0
            end if
         else
c
c     NB: With convex cost functions it cannot be attractive to move
c     both ways.
c
c     Free to move down with modified dual activity
c
            vr_du_act = vr_du_act + pr_co_mu*rsmi_ub(vr_n)
            if (vr_du_act .gt. tl_du_ifs) then
               abs_vr_du_act = vr_du_act
               if (abs_vr_du_act .gt. mx_rao*ed_wt(vr_n)) then
                  nx_vr_t_en_bs = vr_n
                  mx_rao = abs_vr_du_act/ed_wt(vr_n)
                  bp_swp_sd = -1
c
c     Record the column and section number in order to swap sides if
c     this variable is chosen.
c
                  en_c_n = c_n
                  en_sn_n = lg_sn_n_os + vr_in_c_sn_ty_ab_bp
               end if
            end if
         end if
 110  continue
      do 120, c_n = c_n, vr_in_c(os_struc_in_c_bw_bp_p)
c
c     Below break point:
c
         vr_n = vr_in_c(c_n)
c
c     lb:co:ub are uco-lco:lco:bp
c
c     Free to move down with given dual activity
c
         vr_du_act = du_act(vr_n)
         if (vr_du_act .gt. tl_du_ifs) then
            abs_vr_du_act = vr_du_act
            if (abs_vr_du_act .gt. mx_rao*ed_wt(vr_n)) then
               nx_vr_t_en_bs = vr_n
               mx_rao = abs_vr_du_act/ed_wt(vr_n)
               bp_swp_sd = 0
            end if
         else
c
c     NB: With convex cost functions it cannot be attractive to move
c     both ways.
c
c     Free to move up with modified dual activity
c
            vr_du_act = vr_du_act + pr_co_mu*rsmi_lb(vr_n)
            if (-vr_du_act .gt. tl_du_ifs) then
               abs_vr_du_act = -vr_du_act
               if (abs_vr_du_act .gt. mx_rao*ed_wt(vr_n)) then
                  nx_vr_t_en_bs = vr_n
                  mx_rao = abs_vr_du_act/ed_wt(vr_n)
                  bp_swp_sd = 1
c
c     Record the column and section number in order to swap sides if
c     this variable is chosen.
c
                  en_c_n = c_n
                  en_sn_n = lg_sn_n_os + vr_in_c_sn_ty_bw_bp
               end if
            end if
         end if
 120  continue
      do 130, c_n = c_n, vr_in_c(os_struc_in_c_up_dn_p)
         vr_n = vr_in_c(c_n)
c
c     Free to move up or down with given dual activity.
c
         abs_vr_du_act = abs(du_act(vr_n))
         if (abs_vr_du_act .gt. tl_du_ifs) then
            if (abs_vr_du_act .gt. mx_rao*ed_wt(vr_n)) then
               nx_vr_t_en_bs = vr_n
               mx_rao = abs_vr_du_act/ed_wt(vr_n)
            end if
         end if
 130  continue
      if (lp_ph .eq. 1 .and.
     &     iand(cz_c_msk, cz_c_bk_bd_bt) .ne. 0) then
         do 140, c_n = c_n, vr_in_c(os_struc_in_c_up_p)
            vr_n = vr_in_c(c_n)
c
c     Free to move up (from lower bound) with given dual activity.
c
            vr_du_act = du_act(vr_n)
            if (-vr_du_act .gt. tl_du_ifs) then
               abs_vr_du_act = -vr_du_act
               if (abs_vr_du_act .gt. mx_rao*ed_wt(vr_n)) then
                  nx_vr_t_en_bs = vr_n
                  mx_rao = abs_vr_du_act/ed_wt(vr_n)
               end if
            else
c
c     NB: It cannot be attractive to move up from a bound and down
c     through it.
c
c     Free to move down (through bound) with modified dual activity
c
               vr_du_act = vr_du_act - one
               if (vr_du_act .gt. tl_du_ifs) then
                  abs_vr_du_act = vr_du_act
                  if (abs_vr_du_act .gt. mx_rao*ed_wt(vr_n)) then
                     nx_vr_t_en_bs = vr_n
                     mx_rao = abs_vr_du_act/ed_wt(vr_n)
                  end if
               end if
            end if
 140     continue
         do 145, c_n = c_n, vr_in_c(os_struc_in_c_dn_p)
            vr_n = vr_in_c(c_n)
c
c     Free to move down (from upper bound) with given dual activity.
c
            vr_du_act = du_act(vr_n)
            if (vr_du_act .gt. tl_du_ifs) then
               abs_vr_du_act = vr_du_act
               if (abs_vr_du_act .gt. mx_rao*ed_wt(vr_n)) then
                  nx_vr_t_en_bs = vr_n
                  mx_rao = abs_vr_du_act/ed_wt(vr_n)
               end if
            else
c
c     NB: It cannot be attractive to move down from a bound and up
c     through it.
c
c     Free to move up (through bound) with modified dual activity
c
               vr_du_act = vr_du_act + one
               if (-vr_du_act .gt. tl_du_ifs) then
                  abs_vr_du_act = -vr_du_act
                  if (abs_vr_du_act .gt. mx_rao*ed_wt(vr_n)) then
                     nx_vr_t_en_bs = vr_n
                     mx_rao = abs_vr_du_act/ed_wt(vr_n)
                  end if
               end if
            end if
 145     continue
c
c     Consider moving from a (temporarily) fixed value.
c
         do 147, c_n = c_n, vr_in_c(os_struc_in_c_te_fx_p)
            vr_n = vr_in_c(c_n)
            vr_du_act = du_act(vr_n) + one
            if (-vr_du_act .gt. tl_du_ifs) then
c
c     It is attractive to move up from a fixed value.
c
               abs_vr_du_act = -vr_du_act
               if (abs_vr_du_act .gt. mx_rao*ed_wt(vr_n)) then
                  nx_vr_t_en_bs = vr_n
                  mx_rao = abs_vr_du_act/ed_wt(vr_n)
               end if
            else
c
c     NB: It cannot be attractive to move up from a fixed value and down
c     from it
c
c     It is attractive to move down from a fixed value.
c
               vr_du_act = du_act(vr_n) - one
               if (vr_du_act .gt. tl_du_ifs) then
                  abs_vr_du_act = vr_du_act
                  if (abs_vr_du_act .gt. mx_rao*ed_wt(vr_n)) then
                     nx_vr_t_en_bs = vr_n
                     mx_rao = abs_vr_du_act/ed_wt(vr_n)
                  end if
               end if
            end if
 147     continue
      else
         do 150, c_n = c_n, vr_in_c(os_struc_in_c_up_p)
            vr_n = vr_in_c(c_n)
c
c     Free to move up with given dual activity.
c
            vr_du_act = du_act(vr_n)
            if (-vr_du_act .gt. tl_du_ifs) then
               abs_vr_du_act = -vr_du_act
               if (abs_vr_du_act .gt. mx_rao*ed_wt(vr_n)) then
                  nx_vr_t_en_bs = vr_n
                  mx_rao = abs_vr_du_act/ed_wt(vr_n)
               end if
            end if
 150     continue
         do 155, c_n = c_n, vr_in_c(os_struc_in_c_dn_p)
            vr_n = vr_in_c(c_n)
c
c     Free to move down with given dual activity.
c
            vr_du_act = du_act(vr_n)
            if (vr_du_act .gt. tl_du_ifs) then
               abs_vr_du_act = vr_du_act
               if (abs_vr_du_act .gt. mx_rao*ed_wt(vr_n)) then
                  nx_vr_t_en_bs = vr_n
                  mx_rao = abs_vr_du_act/ed_wt(vr_n)
               end if
            end if
 155     continue
      endif
 7000 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9800 format('Calling ems_cz_1_c with incorrect edge weights')
      end
