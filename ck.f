CM
C->>> ----------------------------------------------> ems_ck_rsmi_da <<<
      subroutine ems_ck_rsmi_da(ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMICS.INC'
      include 'ICTVR.INC'
c      include 'RLCTVR.INC'
c      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      double precision ds(0:ds_n_en_m1)
      double precision ftran_er, btran_er
 
      call ems_ck_al_vr_st(
     &     ds(p_rsmi_lb),
     &     ds(p_rsmi_co),
     &     ds(p_rsmi_ub),
     &     is(p_st),
     &     ds(p_pr_act))
      call ems_ck_vr_in_c(is(p_vr_in_c), is(p_st), ds)
      call ems_ck_ix_o_vr(
     &     is(p_vr_in_r),
     &     is(p_vr_in_c),
     &     is(p_st))
      call ems_ca_rand_ck_inv(.true., ftran_er, btran_er, ds, is)
      call ems_ck_bc_co(
     &     is(p_st),
     &     ds(p_rsmi_co),
     &     is(p_vr_in_r),
     &     ds(p_bc_co_v),
     &     is(p_bc_co_ix),
     &     is(p_bc_co_ix_bar))
      call ems_ck_pr_act_rsdu(ds, is)
      call ems_ck_co_rsdu(ds, is)
      return
      end
 
      subroutine ems_ck_pr_act_rsdu(ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMICS.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      include 'EMSMSGN.INC'
      integer is(0:is_n_en_m1)
      double precision ds(0:ds_n_en_m1)
      double precision pr_act_rsdu_norm, rlv_pr_act_rsdu_norm
      integer rl_wk_a_ix
      integer pr_pass_1
      save pr_pass_1
      data pr_pass_1/0/
 
      call ems_g_rsmi_rl_wk_a_ix(rl_wk_a_ix)
      if (rl_wk_a_ix .lt. 0) goto 8000
      call ems_g_pr_act_rsdu_norm(
     &     ds(p_pr_act),
     &     is(p_st),
     &     ds(p_mtx_r_v),
     &     is(p_mtx_r_ix),
     &     is(p_mtx_c_sa),
     &     ds(p_rsmi_rl_wk_a(rl_wk_a_ix)),
     &     pr_act_rsdu_norm, rlv_pr_act_rsdu_norm)
      call ems_fr_rsmi_rl_wk_a_ix(rl_wk_a_ix)
      if (pr_act_rsdu_norm .gt. tl_pr_ifs) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9520)
     &        pr_act_rsdu_norm, rlv_pr_act_rsdu_norm
         call ems_msg_wr_li(warn_msg_n)
      else if (pr_pass_1 .eq. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9200)
         call ems_msg_wr_li(rsmi_msg_n)
         pr_pass_1 = 1
      end if
 7000 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9200 format('No error in primal activity residuals')
 9520 format('Primal activity residual: Absolute = ', g11.4,
     &     ' Relative = ', g11.4)
 9800 format('RSMI workspace not available in ems_ck_pr_act_rsdu')
      end
 
      subroutine ems_ck_co_rsdu(ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMICS.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      include 'EMSMSGN.INC'
      integer is(0:is_n_en_m1)
      double precision ds(0:ds_n_en_m1)
      double precision bc_co_rsdu_norm, rlv_bc_co_rsdu_norm
      double precision non_bc_co_rsdu_norm, rlv_non_bc_co_rsdu_norm
      integer pr_pass_1
      save pr_pass_1
      data pr_pass_1/0/
 
      call ems_g_co_rsdu_norm(
     &     ds(p_du_act),
     &     ds(p_mtx_r_v),
     &     is(p_mtx_r_ix),
     &     is(p_mtx_c_sa),
     &     ds, is,
     &     bc_co_rsdu_norm, rlv_bc_co_rsdu_norm,
     &     non_bc_co_rsdu_norm, rlv_non_bc_co_rsdu_norm)
      if (bc_co_rsdu_norm .gt. tl_pr_ifs) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9520)
     &        bc_co_rsdu_norm, rlv_bc_co_rsdu_norm
         call ems_msg_wr_li(warn_msg_n)
      else if (pr_pass_1 .eq. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9200)
         call ems_msg_wr_li(rsmi_msg_n)
         pr_pass_1 = 1
      end if
      if (non_bc_co_rsdu_norm .gt. tl_pr_ifs) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9521)
     &        non_bc_co_rsdu_norm, rlv_non_bc_co_rsdu_norm
         call ems_msg_wr_li(warn_msg_n)
      else if (pr_pass_1 .eq. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9201)
         call ems_msg_wr_li(rsmi_msg_n)
         pr_pass_1 = 1
      end if
      return
 9200 format('No error in basic cost residuals')
 9201 format('No error in nonbasic cost residuals')
 9520 format('   Basic cost residual: Absolute = ', g11.4,
     &     ' Relative = ', g11.4)
 9521 format('Nonbasic cost residual: Absolute = ', g11.4,
     &     ' Relative = ', g11.4)
      end
 
C->>> ------------------------------------------------> ems_ck_lp_da <<<
      subroutine ems_ck_lp_da(ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMICS.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      double precision ds(0:ds_n_en_m1)
 
      if (iand(ck_msk, st_ck_bt) .ne. 0) then
         call ems_ck_al_vr_st(
     &        ds(p_rsmi_lb),
     &        ds(p_rsmi_co),
     &        ds(p_rsmi_ub),
     &        is(p_st),
     &        ds(p_pr_act))
         call ems_ck_vr_in_c(is(p_vr_in_c), is(p_st), ds)
         call ems_ck_ix_o_vr(
     &        is(p_vr_in_r),
     &        is(p_vr_in_c),
     &        is(p_st))
      end if
      if (iand(ck_msk, bc_co_ck_bt) .ne. 0) then
         call ems_ck_bc_co(
     &        is(p_st),
     &        ds(p_rsmi_co),
     &        is(p_vr_in_r),
     &        ds(p_bc_co_v),
     &        is(p_bc_co_ix),
     &        is(p_bc_co_ix_bar))
      endif
      if (iand(ck_msk, pr_act_ck_bt) .ne. 0) then
         call ems_ck_pr_act_rsdu(ds, is)
      endif
      if ((iand(ck_msk, du_act_ck_bt) .ne. 0) .or.
     &     (iand(ck_msk, ed_wt_ck_bt) .ne. 0 .and.
     &     pc_alg .eq. pc_alg_sed)) then
         call ems_ck_co_rsdu(ds, is)
         call ems_ck_al_du_act_ed_wt(
     &        is(p_vr_in_c),
     &        ds(p_du_act),
     &        ds(p_ed_wt),
     &        ds, is)
      endif
      return
      end
C->>> ---------------------------------------------> ems_ck_vr_bt_se <<<
c     Reports and updates er_fd if the given bit is not set.
c
      subroutine ems_ck_vr_bt_se(bt, ch3_bt_nm, er_fd,
     &     vr_n, vr_st, ch8_vr_bs, ch5_vr_ty, ch5_vr_wh)
      implicit none
      include 'EMSMSG.INC'
      integer bt, vr_n, vr_st
      logical er_fd
      character*3 ch3_bt_nm
      character*5 ch5_vr_ty, ch5_vr_wh
      character*8 ch8_vr_bs
 
      if (iand(vr_st, bt) .eq. 0) goto 8000
 7000 continue
      return
 8000 continue
      er_fd = .true.
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
     &     ch8_vr_bs, ch5_vr_ty, vr_n,
     &     ch5_vr_wh, ch3_bt_nm
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9800 format(a8, 1x, a5, ' variable ', i7, 1x, a5,
     &     ' should     have ', a3, ' bit set')
      end
C->>> ------------------------------------------> ems_ck_vr_bt_no_se <<<
c     Reports and updates er_fd if the given bit is set.
c
      subroutine ems_ck_vr_bt_no_se(bt, ch3_bt_nm, er_fd,
     &     vr_n, vr_st, ch8_vr_bs, ch5_vr_ty, ch5_vr_wh)
      implicit none
      include 'EMSMSG.INC'
      integer bt, vr_n, vr_st
      logical er_fd
      character*3 ch3_bt_nm
      character*5 ch5_vr_ty, ch5_vr_wh
      character*8 ch8_vr_bs
 
      if (iand(vr_st, bt) .ne. 0) goto 8000
 7000 continue
      return
 8000 continue
      er_fd = .true.
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
     &     ch8_vr_bs, ch5_vr_ty, vr_n,
     &     ch5_vr_wh, ch3_bt_nm
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9800 format(a8, 1x, a5, ' variable ', i7, 1x, a5,
     &     ' should not have ', a3, ' bit set')
      end
C->>> ----------------------------------------------> ems_ck_wr_rsmi <<<
c     Checks the indexing, status and primal activities. Writes out the
c     values and status of all variables.
c
      subroutine ems_ck_wr_rsmi(ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMICS.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      double precision ds(0:ds_n_en_m1)
      integer is(0:is_n_en_m1)
 
      ck_msk = st_ck_bt + pr_act_ck_bt
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)n_si_it
      call ems_msg_wr_li(info_msg_n)
      call ems_ck_lp_da(ds, is)
c      call ems_rp_al_vr_st(ds, is)
      return
 9000 format('Checking RSMI on iteration ', i7)
      end
 
C->>> ------------------------------------------------> ems_ck_bc_co <<<
c     Checks the costs of the basic variables.
c
      subroutine ems_ck_bc_co(st, rsmi_co, vr_in_r,
     &     bc_co_v, bc_co_ix, bc_co_ix_bar)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      double precision rsmi_co(0:mx_n_c+n_r)
      double precision bc_co_v(0:n_r)
      integer st(0:mx_n_c+n_r), vr_in_r(0:n_r)
      integer bc_co_ix(0:n_r), bc_co_ix_bar(0:n_r)
      integer r_n, vr_n
      double precision tru_bc_co
      logical er_fd
      integer pr_pass_1
      save pr_pass_1
      data pr_pass_1/0/
 
      er_fd = .false.
      if (lp_ph .eq. 1) then
         do 10, r_n = 1, n_r
            vr_n = vr_in_r(r_n)
            if (iand(st(vr_n), ifs_bt) .eq. 0) then
               tru_bc_co = pr_co_mu*rsmi_co(vr_n)
               if (bc_co_v(r_n) .ne. tru_bc_co) then
                  er_fd = .true.
                  if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &                 vr_n, bc_co_v(r_n), tru_bc_co
                  call ems_msg_wr_li(bug_msg_n)
               end if
            else
               if (iand(st(vr_n), up_bt) .ne. 0) then
                  tru_bc_co = -one + pr_co_mu*rsmi_co(vr_n)
                  if (bc_co_v(r_n) .ne. tru_bc_co) then
                     er_fd = .true.
                     if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &                    vr_n, bc_co_v(r_n), tru_bc_co
                     call ems_msg_wr_li(bug_msg_n)
                  end if
               else
                  tru_bc_co = one + pr_co_mu*rsmi_co(vr_n)
                  if (bc_co_v(r_n) .ne. tru_bc_co) then
                     er_fd = .true.
                     if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &                    vr_n, bc_co_v(r_n), tru_bc_co
                     call ems_msg_wr_li(bug_msg_n)
                  end if
               end if
            end if
 10      continue
      else
         do 20, r_n = 1, n_r
            vr_n = vr_in_r(r_n)
            tru_bc_co = pr_co_mu*rsmi_co(vr_n)
            if (bc_co_v(r_n) .ne. tru_bc_co) then
               er_fd = .true.
               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &              vr_n, bc_co_v(r_n), tru_bc_co
               call ems_msg_wr_li(bug_msg_n)
            end if
 20      continue
      end if
      if (er_fd) then
CM      IF (emsol_deb .EQ. 1) THEN
C?         call ems_dump
CM      ENDIF
         call ems_se_bc_co(st, rsmi_co, vr_in_r,
     &     bc_co_v, bc_co_ix, bc_co_ix_bar)
         fresh_pc = .true.
      else if (pr_pass_1 .eq. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)
         call ems_msg_wr_li(info_msg_n)
         pr_pass_1 = 1
      end if
      return
 9000 format('Variable number ', i7, ' has basic cost of ', g11.4,
     &     ' rather than ', g11.4)
 9010 format('No error in the basic costs')
      end
 
C->>> ---------------------------------------------> ems_ck_al_vr_st <<<
c     Check the status of all the variables.
c
      subroutine ems_ck_al_vr_st(rsmi_lb, rsmi_co, rsmi_ub, st, pr_act)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      include 'EMSMSGN.INC'
      double precision rsmi_lb(0:mx_n_c+n_r)
      double precision rsmi_co(0:mx_n_c+n_r)
      double precision rsmi_ub(0:mx_n_c+n_r)
      double precision pr_act(0:mx_n_c+n_r)
      integer st(0:mx_n_c+n_r)
      logical ems_ck_vr_st
      integer ix, vr_n
      logical er_fd
      integer pr_pass_1
      save pr_pass_1
      data pr_pass_1/0/
 
      er_fd = .false.
      do 10, ix = 1, n_r+n_c
         vr_n = ix
         if (ix .gt. n_c) vr_n = ix + mx_n_c-n_c
         er_fd = ems_ck_vr_st(vr_n,
     &        rsmi_lb(vr_n), rsmi_co(vr_n), rsmi_ub(vr_n),
     &        st(vr_n), pr_act(vr_n)) .or. er_fd
 10   continue
      if (pr_pass_1 .eq. 0 .and. .not. er_fd) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9200)
         call ems_msg_wr_li(rsmi_msg_n)
         pr_pass_1 = 1
      end if
      return
 9200 format('No error in status of variables')
      end
 
C->>> ------------------------------------------------> ems_ck_vr_st <<<
c     Checks variable's bounds and activity against its status.
c
      logical function ems_ck_vr_st(vr_n, rsmi_lb, rsmi_co, rsmi_ub,
     &     st, pr_act)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      integer vr_n, st
      double precision rsmi_lb, rsmi_co, rsmi_ub, pr_act
      double precision rsdu
      logical er_fd
 
      er_fd = .false.
      if (iand(st, alt_bt) .eq. 0) then
c
c     Variable is Standard
c
c     Check the bounds
c
         if (iand(st, lb_bt) .eq. 0) then
c
c     Variable has no lower bound: make sure it it is infinite
c
            if (rsmi_lb .gt. -inf) then
               er_fd = .true.
               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)
     &              'Std', vr_n, 'lb ', 'lb ', rsmi_lb
               call ems_msg_wr_li(bug_msg_n)
            endif
         else
c
c     Variable has a lower bound: make sure it it is finite
c
            if (rsmi_lb .le. -inf) then
               er_fd = .true.
               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9011)
     &              'Std', vr_n, 'lb ', 'lb ', rsmi_lb
               call ems_msg_wr_li(bug_msg_n)
            endif
         endif
         if (iand(st, ub_bt) .eq. 0) then
c
c     Variable has no upper bound: make sure it it is infinite
c
            if (rsmi_ub .lt. inf) then
               er_fd = .true.
               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)
     &              'Std', vr_n, 'ub ', 'ub ', rsmi_ub
               call ems_msg_wr_li(bug_msg_n)
            endif
         else
c
c     Variable has an upper bound: make sure it it is finite
c
            if (rsmi_ub .ge. inf) then
               er_fd = .true.
               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9011)
     &              'Std', vr_n, 'ub ', 'ub ', rsmi_ub
               call ems_msg_wr_li(bug_msg_n)
            endif
         endif
c
c     Check that the bits set are consistent (with the primal activity).
c
         if (iand(st, ub_bt) .eq. 0) then
            if (iand(st, lb_bt) .eq. 0) then
c
c     A FR variable: should have up:dn:fs = 1:1:1
c
               call ems_ck_vr_bt_se(   up_bt, 'up ', er_fd,
     &              vr_n, st, '        ', 'FR   ', 'Btw  ')
               call ems_ck_vr_bt_se(   dn_bt, 'dn ', er_fd,
     &              vr_n, st, '        ', 'FR   ', 'Btw  ')
               call ems_ck_vr_bt_no_se(ifs_bt, 'ifs', er_fd,
     &              vr_n, st, '        ', 'FR   ', 'Btw  ')
            else
c
c     A LB variable:
c
               rsdu = rsmi_lb - pr_act
               if (rsdu .gt. tl_pr_ifs) then
c
c     below its bound should have up:dn:fs = 1:0:0
c
                  call ems_ck_vr_bt_se(   up_bt, 'up ', er_fd,
     &                 vr_n, st, '        ', 'LB   ', 'Bw_Lb')
                  call ems_ck_vr_bt_no_se(dn_bt, 'dn ', er_fd,
     &                 vr_n, st, '        ', 'LB   ', 'Bw_Lb')
                  call ems_ck_vr_bt_se(ifs_bt, 'ifs', er_fd,
     &                 vr_n, st, '        ', 'LB   ', 'Bw_Lb')
               else if (iand(st, bc_bt) .eq. 0 .and.
     &                 rsdu .ge. -tl_pr_ifs) then
c
c     nonbasic at its bound should have up:dn:fs = 1:0:1
c
                  call ems_ck_vr_bt_se(   up_bt, 'up ', er_fd,
     &                 vr_n, st, 'Nonbasic', 'LB   ', 'At_Lb')
                  call ems_ck_vr_bt_no_se(dn_bt, 'dn ', er_fd,
     &                 vr_n, st, 'Nonbasic', 'LB   ', 'At_Lb')
                  call ems_ck_vr_bt_no_se(ifs_bt, 'ifs', er_fd,
     &                 vr_n, st, 'Nonbasic', 'LB   ', 'At_Lb')
               else
c
c     basic at/above or nonbasic above its bound should have
c     up:dn:fs = 1:1:1
c
                  call ems_ck_vr_bt_se(   up_bt, 'up ', er_fd,
     &                 vr_n, st, '        ', 'LB   ', 'Btw  ')
                  call ems_ck_vr_bt_se(   dn_bt, 'dn ', er_fd,
     &                 vr_n, st, '        ', 'LB   ', 'Btw  ')
                  call ems_ck_vr_bt_no_se(ifs_bt, 'ifs', er_fd,
     &                 vr_n, st, '        ', 'LB   ', 'Btw  ')
               endif
            endif
         else
            if (iand(st, lb_bt) .eq. 0) then
c
c     A UB variable:
c
               rsdu = pr_act - rsmi_ub
               if (rsdu .gt. tl_pr_ifs) then
c
c     above its bound should have up:dn:fs = 0:1:0
c
                  call ems_ck_vr_bt_no_se(up_bt, 'up ', er_fd,
     &                 vr_n, st, '        ', 'UB   ', 'Ab_Ub')
                  call ems_ck_vr_bt_se(   dn_bt, 'dn ', er_fd,
     &                 vr_n, st, '        ', 'UB   ', 'Ab_Ub')
                  call ems_ck_vr_bt_se(ifs_bt, 'ifs', er_fd,
     &                 vr_n, st, '        ', 'UB   ', 'Ab_Ub')
               else if (iand(st, bc_bt) .eq. 0 .and.
     &                 rsdu .ge. -tl_pr_ifs) then
c
c     nonbasic at its bound should have up:dn:fs = 0:1:1
c
                  call ems_ck_vr_bt_no_se(up_bt, 'up ', er_fd,
     &                 vr_n, st, 'Nonbasic', 'UB   ', 'At_UB')
                  call ems_ck_vr_bt_se(   dn_bt, 'dn ', er_fd,
     &                 vr_n, st, 'Nonbasic', 'UB   ', 'At_UB')
                  call ems_ck_vr_bt_no_se(ifs_bt, 'ifs', er_fd,
     &                 vr_n, st, 'Nonbasic', 'UB   ', 'At_UB')
               else
c
c     basic at/below or nonbasic below its bound should have
c     up:dn:fs = 1:1:1
c
                  call ems_ck_vr_bt_se(   up_bt, 'up ', er_fd,
     &                 vr_n, st, '        ', 'UB   ', 'Btw  ')
                  call ems_ck_vr_bt_se(   dn_bt, 'dn ', er_fd,
     &                 vr_n, st, '        ', 'UB   ', 'Btw  ')
                  call ems_ck_vr_bt_no_se(ifs_bt, 'ifs', er_fd,
     &                 vr_n, st, '        ', 'UB   ', 'Btw  ')
               endif
            else if (rsmi_lb .ne. rsmi_ub) then
c
c     A LB/UB variable:
c
               rsdu = max(rsmi_lb-pr_act, pr_act-rsmi_ub)
               if (rsdu .gt. tl_pr_ifs) then
                  if (pr_act .lt. rsmi_lb) then
c
c     below its lower bound should have up:dn:fs = 1:0:0
c
                     call ems_ck_vr_bt_se(   up_bt, 'up ', er_fd,
     &                    vr_n, st, '        ', 'LB/UB', 'Bw_Lb')
                     call ems_ck_vr_bt_no_se(dn_bt, 'dn ', er_fd,
     &                    vr_n, st, '        ', 'LB/UB', 'Bw_Lb')
                     call ems_ck_vr_bt_se(ifs_bt, 'ifs', er_fd,
     &                    vr_n, st, '        ', 'LB/UB', 'Bw_Lb')
                  else
c
c     above its upper bound should have up:dn:fs = 0:1:0
c
                     call ems_ck_vr_bt_no_se(up_bt, 'up ', er_fd,
     &                    vr_n, st, '        ', 'LB/UB', 'Ab_Ub')
                     call ems_ck_vr_bt_se(   dn_bt, 'dn ', er_fd,
     &                    vr_n, st, '        ', 'LB/UB', 'Ab_Ub')
                     call ems_ck_vr_bt_se(ifs_bt, 'ifs', er_fd,
     &                    vr_n, st, '        ', 'LB/UB', 'Ab_Ub')
                  endif
               else if (iand(st, bc_bt) .eq. 0 .and.
     &                 rsdu .ge. -tl_pr_ifs) then
                  if (two*pr_act .lt. rsmi_lb+rsmi_ub) then
c
c     nonbasic at its lower bound should have up:dn:fs = 1:0:1
c
                     call ems_ck_vr_bt_se(   up_bt, 'up ', er_fd,
     &                    vr_n, st, 'Nonbasic', 'LB/UB', 'At_Lb')
                     call ems_ck_vr_bt_no_se(dn_bt, 'dn ', er_fd,
     &                    vr_n, st, 'Nonbasic', 'LB/UB', 'At_Lb')
                     call ems_ck_vr_bt_no_se(ifs_bt, 'ifs', er_fd,
     &                    vr_n, st, 'Nonbasic', 'LB/UB', 'At_Lb')
                  else
c
c     nonbasic at its upper bound should have up:dn:fs = 0:1:1
c
                     call ems_ck_vr_bt_no_se(up_bt, 'up ', er_fd,
     &                    vr_n, st, 'Nonbasic', 'LB/UB', 'At_UB')
                     call ems_ck_vr_bt_se(   dn_bt, 'dn ', er_fd,
     &                    vr_n, st, 'Nonbasic', 'LB/UB', 'At_UB')
                     call ems_ck_vr_bt_no_se(ifs_bt, 'ifs', er_fd,
     &                    vr_n, st, 'Nonbasic', 'LB/UB', 'At_UB')
                  endif
               else
c
c     basic at/between or nonbasic between its bounds should have
c     up:dn:fs = 1:1:1
c
                  call ems_ck_vr_bt_se(   up_bt, 'up ', er_fd,
     &                 vr_n, st, '        ', 'LB/UB', 'Btw  ')
                  call ems_ck_vr_bt_se(   dn_bt, 'dn ', er_fd,
     &                 vr_n, st, '        ', 'LB/UB', 'Btw  ')
                  call ems_ck_vr_bt_no_se(ifs_bt, 'ifs', er_fd,
     &                 vr_n, st, '        ', 'LB/UB', 'Btw  ')
               endif
            else
c
c     A FX variable:
c
               rsdu = max(rsmi_lb-pr_act, pr_act-rsmi_ub)
               if (rsdu .gt. tl_pr_ifs) then
                  if (pr_act .lt. rsmi_lb) then
c
c     below its lower bound should have up:dn:fs = 1:0:0
c
                     call ems_ck_vr_bt_se(   up_bt, 'up ', er_fd,
     &                    vr_n, st, '        ', 'FX   ', 'Bw_Lb')
                     call ems_ck_vr_bt_no_se(dn_bt, 'dn ', er_fd,
     &                    vr_n, st, '        ', 'FX   ', 'Bw_Lb')
                     call ems_ck_vr_bt_se(ifs_bt, 'ifs', er_fd,
     &                    vr_n, st, '        ', 'FX   ', 'Bw_Lb')
                  else
c
c     above its upper bound should have up:dn:fs = 0:1:0
c
                     call ems_ck_vr_bt_no_se(up_bt, 'up ', er_fd,
     &                    vr_n, st, '        ', 'FX   ', 'Ab_Ub')
                     call ems_ck_vr_bt_se(   dn_bt, 'dn ', er_fd,
     &                    vr_n, st, '        ', 'FX   ', 'Ab_Ub')
                     call ems_ck_vr_bt_se(ifs_bt, 'ifs', er_fd,
     &                    vr_n, st, '        ', 'FX   ', 'Ab_Ub')
                  endif
               else
                  if (iand(st, bc_bt) .eq. 0) then
c
c     Nonbasic at its fixed value should have up:dn:fs = 0:0:1
c
                     call ems_ck_vr_bt_no_se(up_bt, 'up ', er_fd,
     &                    vr_n, st, 'Nonbasic', 'FX   ', 'Fx   ')
                     call ems_ck_vr_bt_no_se(dn_bt, 'dn ', er_fd,
     &                    vr_n, st, 'Nonbasic', 'FX   ', 'Fx   ')
                     call ems_ck_vr_bt_no_se(ifs_bt, 'ifs', er_fd,
     &                    vr_n, st, 'Nonbasic', 'FX   ', 'Fx   ')
                  else
c
c     Basic at its fixed value should have up:dn:fs = 1:1:1
c
                     call ems_ck_vr_bt_se(   up_bt, 'up ', er_fd,
     &                    vr_n, st, 'Basic   ', 'FX   ', 'Fx   ')
                     call ems_ck_vr_bt_se(   dn_bt, 'dn ', er_fd,
     &                    vr_n, st, 'Basic   ', 'FX   ', 'Fx   ')
                     call ems_ck_vr_bt_no_se(ifs_bt, 'ifs', er_fd,
     &                    vr_n, st, 'Basic   ', 'FX   ', 'Fx   ')
                  endif
               endif
            endif
         endif
      else if (iand(st, bp_bt) .ne. 0) then
c
c     Variable is BP
c
c     Should have the fs bit set:
c
         call ems_ck_vr_bt_no_se(ifs_bt, 'ifs', er_fd,
     &        vr_n, st, '        ', 'BP   ', '     ')
         if (iand(st, lb_bt) .ne. 0) then
c
c     Variable is at/above its break point.
c
c     lb:co:ub are bp:uco:lco-uco
c
c     The ub bit should not be set
c
            call ems_ck_vr_bt_no_se(ub_bt, 'ub ', er_fd,
     &           vr_n, st, '        ', 'BP   ', 'Ab_Bp')
c
c     The up bit should be set
c
            call ems_ck_vr_bt_se(   up_bt, 'up ', er_fd,
     &           vr_n, st, '        ', 'BP   ', 'Ab_Bp')
            rsdu = rsmi_lb - pr_act
            if (rsdu .gt. tl_pr_ifs) then
               er_fd = .true.
               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9020)
     &              vr_n, rsdu
               call ems_msg_wr_li(bug_msg_n)
            endif
            if (iand(st, bc_bt) .eq. 0 .and. rsdu .ge. -tl_pr_ifs) then
c
c     If nonbasic and at the break point then the dn bit should not be
c     set.
c
               call ems_ck_vr_bt_no_se(dn_bt, 'dn ', er_fd,
     &              vr_n, st, 'Nonbasic', 'BP   ', 'Ab_Bp')
            else
c
c     If basic or above the break point then the dn bit should be set.
c
               call ems_ck_vr_bt_se(   dn_bt, 'dn ', er_fd,
     &              vr_n, st, '        ', 'BP   ', 'Btw   ')
            endif
         else
c
c     Variable is at/below its break point.
c
c     lb:co:ub are uco-lco:lco:bp
c
c     The lb bit should not be set
c
            call ems_ck_vr_bt_no_se(lb_bt, 'lb ', er_fd,
     &           vr_n, st, '        ', 'BP   ', 'Bw_Bp')
c
c     The dn bit should be set
c
            call ems_ck_vr_bt_se(   dn_bt, 'dn ', er_fd,
     &           vr_n, st, '        ', 'BP   ', 'Bw_Bp')
            rsdu = pr_act - rsmi_ub
            if (rsdu .gt. tl_pr_ifs) then
               er_fd = .true.
               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9021)
     &              vr_n, rsdu
               call ems_msg_wr_li(bug_msg_n)
            endif
            if (iand(st, bc_bt) .eq. 0 .and. rsdu .ge. -tl_pr_ifs) then
c
c     If nonbasic and at the break point then the up bit should not be
c     set.
c
               call ems_ck_vr_bt_no_se(up_bt, 'up ', er_fd,
     &              vr_n, st, 'Nonbasic', 'BP   ', 'Bw_Bp')
            else
c
c     If basic or above the break point then the up bit should be set.
c
               call ems_ck_vr_bt_se(   up_bt, 'up ', er_fd,
     &              vr_n, st, '        ', 'BP   ', 'Btw  ')
            endif
         endif
      else
c
c     Variable is PWL
c
      endif
      ems_ck_vr_st = er_fd
      return
 9010 format(11x, a3, ' variable ', i7, ' has ', a3,
     &     ' bit not set but ', a3, ' = ', g11.4)
 9011 format(11x, a3, ' variable ', i7, ' has ', a3,
     &     ' bit     set but ', a3, ' = ', g11.4)
 9020 format(11x, 'BP variable ', i7,
     &     ' has lb bit set but act-bp = ', g11.4)
 9021 format(11x, 'BP variable ', i7,
     &     ' has ub bit set but bp-act = ', g11.4)
      end
 
C->>> --------------------------------------> ems_ck_al_du_act_ed_wt <<<
c     Checks the dual activities (and/or edge weights if using steepest
c     edge) of all the variables for pricing,
c     writes a message for eack error or an OK message if there is none.
c
      subroutine ems_ck_al_du_act_ed_wt(vr_in_c, du_act, ed_wt, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMICS.INC'
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      include 'EMSMSGN.INC'
      integer vr_in_c(-vr_in_c_n_sn:n_c), is(0:is_n_en_m1)
      double precision ed_wt(0:mx_n_c+n_r)
      double precision du_act(0:mx_n_c+n_r)
      double precision ds(0:ds_n_en_m1)
      double precision ems_scpr
      integer c_n, vr_n, null
      double precision alt_du_act, du_act_er
      double precision tru_ed_wt, ed_wt_er
      double precision ph_1_du_act
      logical du_act_er_fd
      logical ed_wt_er_fd
      integer pr_pass_1
      save pr_pass_1
      data pr_pass_1/0/
 
      du_act_er_fd = .false.
      ed_wt_er_fd = .false.
      do 10, c_n = 1, vr_in_c(os_lg_in_c_l_pc_p)
         vr_n = vr_in_c(c_n)
         call ems_g_rhs(1, vr_n, ds(p_pv_c_v), n_r+1, ds, is)
         call ems_ftran(ds(p_pv_c_v), n_r+1, ds, is)
         if (iand(ck_msk, du_act_ck_bt) .ne. 0) then
            call ems_g_alt_du_act(
     &           1, vr_n,
     &           is(p_st),
     &           is(p_vr_in_r),
     &           ds(p_rsmi_co),
     &           ds(p_bc_co_v),
     &           is(p_bc_co_ix),
     &           ds(p_pv_c_v),
     &           du_act(vr_n), alt_du_act, ph_1_du_act)
            if (abs(du_act(vr_n)) .gt. tl_du_ifs) then
               du_act_er = abs((du_act(vr_n)-alt_du_act)/du_act(vr_n))
            else
               du_act_er = abs(du_act(vr_n)-alt_du_act)
            end if
            if (du_act_er .gt. tl_du_ifs) then
               du_act_er_fd = .true.
               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9510)
     &              vr_n-mx_n_c, 'Dual activity:',
     &              'BTRAN + PRICE', du_act(vr_n),
     &              '        FTRAN', alt_du_act, du_act_er
               call ems_msg_wr_li(warn_msg_n)
            end if
         endif
         if (iand(ck_msk, ed_wt_ck_bt) .ne. 0) then
            if (pc_alg .eq. pc_alg_sed) then
               tru_ed_wt =
     &              ems_scpr(one, ds(p_pv_c_v), ds(p_pv_c_v), n_r)*half
               ed_wt_er = (tru_ed_wt-ed_wt(vr_n))/ed_wt(vr_n)
               if (abs(ed_wt_er) .gt. tl_du_ifs) then
                  ed_wt_er_fd = .true.
                  if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9510)
     &                 vr_n-mx_n_c, 'Stpst edge wt:',
     &                 ' U_STPST_ED_WT', ed_wt(vr_n),
     &                 '         FTRAN', tru_ed_wt, ed_wt_er
                  call ems_msg_wr_li(warn_msg_n)
               end if
            endif
         endif
         nw_eta_l_ix = n_r+1
         call ems_ze_pv_c_v(ds(p_pv_c_v), null)
 10   continue
      do 20, c_n = vr_in_c(os_struc_in_c_f_p_m1) + 1,
     &     vr_in_c(os_struc_in_c_l_pc_p)
         vr_n = vr_in_c(c_n)
         call ems_g_rhs(1, vr_n, ds(p_pv_c_v), n_r+1, ds, is)
         call ems_ftran(ds(p_pv_c_v), n_r+1, ds, is)
         if (iand(ck_msk, du_act_ck_bt) .ne. 0) then
            call ems_g_alt_du_act(
     &           1, vr_n,
     &           is(p_st),
     &           is(p_vr_in_r),
     &           ds(p_rsmi_co),
     &           ds(p_bc_co_v),
     &           is(p_bc_co_ix),
     &           ds(p_pv_c_v),
     &           du_act(vr_n), alt_du_act, ph_1_du_act)
            if (abs(du_act(vr_n)) .gt. tl_du_ifs) then
               du_act_er = abs((du_act(vr_n)-alt_du_act)/du_act(vr_n))
            else
               du_act_er = abs(du_act(vr_n)-alt_du_act)
            end if
            if (du_act_er .gt. tl_du_ifs) then
               du_act_er_fd = .true.
               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9520)
     &              vr_n, 'Dual activity:',
     &              'BTRAN + PRICE', du_act(vr_n),
     &              '        FTRAN', alt_du_act, du_act_er
               call ems_msg_wr_li(warn_msg_n)
            end if
         endif
         if (iand(ck_msk, ed_wt_ck_bt) .ne. 0) then
            if (pc_alg .eq. pc_alg_sed) then
               tru_ed_wt =
     &              ems_scpr(one, ds(p_pv_c_v), ds(p_pv_c_v), n_r)*half
               ed_wt_er = (tru_ed_wt-ed_wt(vr_n))/ed_wt(vr_n)
               if (abs(ed_wt_er) .gt. tl_du_ifs) then
                  ed_wt_er_fd = .true.
                  if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9520)
     &                 vr_n, 'Stpst edge wt:',
     &                 ' U_STPST_ED_WT', ed_wt(vr_n),
     &                 '         FTRAN', tru_ed_wt, ed_wt_er
                  call ems_msg_wr_li(warn_msg_n)
               end if
            endif
         endif
         nw_eta_l_ix = n_r+1
         call ems_ze_pv_c_v(ds(p_pv_c_v), null)
 20   continue
      if (pr_pass_1 .eq. 0) then
         if (iand(ck_msk, du_act_ck_bt) .ne. 0 .and.
     &        .not.du_act_er_fd) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9210)
            call ems_msg_wr_li(rsmi_msg_n)
         endif
         if (iand(ck_msk, ed_wt_ck_bt) .ne. 0 .and.
     &        .not.ed_wt_er_fd) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9220)
            call ems_msg_wr_li(rsmi_msg_n)
         endif
         pr_pass_1 = 1
      end if
      if (iand(ck_msk, ze_a_ck_bt) .ne. 0)
     &     call ems_ck_ze_rl_a(n_r, ds(p_pv_c_v))
      return
 9210 format('No error in the dual activities')
 9220 format('No error in the edge weights')
 9510 format('Row       ', i7, 1x, a14,
     &     2(' From ', a14, ' = ', g11.4), ' |Rel. error| = ', g11.4)
 9520 format('Column    ', i7, 1x, a14,
     &     2(' From ', a14, ' = ', g11.4), ' |Rel. error| = ', g11.4)
      end
 
C->>> ----------------------------------------------> ems_ck_ix_o_vr <<<
c     Checks that the indices of variables sored in st correspond to the
c     vr_in_r and vr_in_c supplied.
c
      subroutine ems_ck_ix_o_vr(
     &     vr_in_r,
     &     vr_in_c,
     &     st)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
c      include 'RSMICS.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      include 'EMSMSGN.INC'
      integer vr_in_r(0:n_r)
      integer vr_in_c(-vr_in_c_n_sn:n_c)
      integer st(0:mx_n_c+n_r)
      integer r_n, c_n, vr_n, ix_n
      logical er_fd
      integer pr_pass_1
      save pr_pass_1
      data pr_pass_1/0/
 
      er_fd = .false.
      do 10, r_n = 1, n_r
         vr_n = vr_in_r(r_n)
         if (iand(st(vr_n), bc_bt) .ne. bc_bt) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9900)vr_n
            call ems_msg_wr_li(bug_msg_n)
            er_fd = .true.
            st(vr_n) = st(vr_n) + bc_bt
         end if
         ix_n = iand(st(vr_n), mx_mx_ml_a_dim)
         if (ix_n .ne. r_n) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9910)
     &           vr_n, ix_n, r_n
            call ems_msg_wr_li(bug_msg_n)
            er_fd = .true.
            st(vr_n) = st(vr_n) - ix_n + r_n
         end if
 10   continue
      do 20, c_n = 1, vr_in_c(os_vr_in_c_l_p)
         vr_n = vr_in_c(c_n)
         if (iand(st(vr_n), bc_bt) .ne. 0) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9920)vr_n
            call ems_msg_wr_li(bug_msg_n)
            er_fd = .true.
            st(vr_n) = st(vr_n) - bc_bt
         end if
         ix_n = iand(st(vr_n), mx_mx_ml_a_dim)
         if (ix_n .ne. c_n) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9930)
     &           vr_n, ix_n, c_n
            call ems_msg_wr_li(bug_msg_n)
            er_fd = .true.
            st(vr_n) = st(vr_n) - ix_n + c_n
         end if
 20   continue
      if (er_fd) then
CM      IF (emsol_deb .EQ. 1) THEN
C?         call ems_dump
CM      ENDIF
      else if (pr_pass_1 .eq. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9940)
         call ems_msg_wr_li(rsmi_msg_n)
         pr_pass_1 = 1
      end if
      return
 9900 format('Basic variable ', i7, ' does not have basic bit set')
 9910 format('Basic variable ', i7, ' has index ', i7, ' not ', i7)
 9920 format('Nonbasic variable ', i7, ' has basic bit set')
 9930 format('Nonbasic variable ', i7, ' has index ', i7, ' not ', i7)
 9940 format('Indexing of variables is correct')
      end
 
C->>> --------------------------------------------------> ems_ck_inv <<<
c     Checks FTRAN and BTRAN for all columns and rows of the basis
c     matrix respectively.
c
      subroutine ems_ck_inv(ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
c      include 'RSMICS.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      include 'EMSMSGN.INC'
      integer is(0:is_n_en_m1)
      double precision ds(0:ds_n_en_m1)
      integer c_n, r_n, stp
      double precision tran_er, mx_er
      logical er_fd
      integer rl_wk_a_ix
      integer i_wk_a_ix
 
      mx_er = zero
      er_fd = .false.
c
c     Check FTRAN
c
c      stp = max(1, n_r/10)
      stp = 1
      call ems_g_rsmi_rl_wk_a_ix(rl_wk_a_ix)
      if (rl_wk_a_ix .lt. 0) goto 8000
      call ems_g_rsmi_i_wk_a_ix(i_wk_a_ix)
      if (i_wk_a_ix .lt. 0) goto 8000
      call ems_cp_rl_a(n_r, zero, ds(p_rsmi_rl_wk_a(rl_wk_a_ix)+1), 0)
      do 10 c_n = 1, n_r, stp
         if (sto_ftran_ix .eq. sto_ix_y) then
            is(p_rsmi_i_wk_a(i_wk_a_ix)) = 0
         else
            is(p_rsmi_i_wk_a(i_wk_a_ix)) = n_r+1
         endif
         call ems_g_rhs(1, is(p_vr_in_r+c_n),
     &        ds(p_rsmi_rl_wk_a(rl_wk_a_ix)),
     &        is(p_rsmi_i_wk_a(i_wk_a_ix)), ds, is)
         call ems_ftran(
     &        ds(p_rsmi_rl_wk_a(rl_wk_a_ix)),
     &        is(p_rsmi_i_wk_a(i_wk_a_ix)), ds, is)
         tran_er = zero
c
c     ?? This should use indices of nonzeros if possible
c
         do 20, r_n = 1, n_r
            if (r_n .eq. c_n) then
               tran_er = tran_er +
     &              (ds(p_rsmi_rl_wk_a(rl_wk_a_ix)+r_n)+one)**2
            else
               tran_er = tran_er +
     &              (ds(p_rsmi_rl_wk_a(rl_wk_a_ix)+r_n))**2
            end if
            ds(p_rsmi_rl_wk_a(rl_wk_a_ix)+r_n) = zero
 20      continue
         tran_er = sqrt(tran_er)
         mx_er = max(mx_er, tran_er)
         if (tran_er .gt. tl_tran_er) then
            er_fd = .true.
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)
     &           tran_er, c_n, is(p_vr_in_r+c_n)
            call ems_msg_wr_li(warn_msg_n)
         end if
 10   continue
c
c     Check BTRAN
c
      do 110 r_n = 1, n_r, stp
         if (sto_btran_ix .eq. sto_ix_no) then
            is(p_rsmi_i_wk_a(i_wk_a_ix)) = n_r+1
         else
            is(p_rsmi_i_wk_a(i_wk_a_ix)) = 0
         endif
         call ems_g_bs_r(is(p_vr_in_r), r_n,
     &        ds(p_rsmi_rl_wk_a(rl_wk_a_ix)),
     &        is(p_rsmi_i_wk_a(i_wk_a_ix)), ds, is)
         call ems_btran(
     &        ds(p_rsmi_rl_wk_a(rl_wk_a_ix)),
     &        is(p_rsmi_i_wk_a(i_wk_a_ix)), ds, is)
         tran_er = zero
c
c     ?? This should use indices of nonzeros if possible
c
         do 120, c_n = 1, n_r
            if (c_n .eq. r_n) then
               tran_er = tran_er +
     &              (ds(p_rsmi_rl_wk_a(rl_wk_a_ix)+c_n)+one)**2
            else
               tran_er = tran_er +
     &              (ds(p_rsmi_rl_wk_a(rl_wk_a_ix)+c_n))**2
            end if
            ds(p_rsmi_rl_wk_a(rl_wk_a_ix)+c_n) = zero
 120     continue
         tran_er = sqrt(tran_er)
         if (tran_er .gt. tl_tran_er) then
            er_fd = .true.
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9110)
     &           tran_er, r_n
            call ems_msg_wr_li(warn_msg_n)
         end if
 110  continue
      call ems_fr_rsmi_rl_wk_a_ix(rl_wk_a_ix)
      call ems_fr_rsmi_i_wk_a_ix(i_wk_a_ix)
      if (.not. er_fd) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9200)mx_er
         call ems_msg_wr_li(inv_msg_n)
      end if
 7000 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9010 format('Residual ', g11.4, ' for column', i7, ' which is ', i7)
 9110 format('Residual ', g11.4, ' for row   ', i7)
 9200 format('Max error found in INVERT is ', g11.4)
 9800 format('RSMI workspace not available in ems_ck_inv')
      end
 
c->>> ------------------------------------------> ems_ca_rand_ck_inv <<<
c     Interface to ems_rand_ck_inv
c
      subroutine ems_ca_rand_ck_inv(rand, ftran_er, btran_er, ds, is)
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
      logical rand
      double precision ftran_er, btran_er
      double precision ds(0:ds_n_en_m1)
      integer is(0:is_n_en_m1)
      integer rl_wk_a_ix
      integer pr_pass_1
      save pr_pass_1
      data pr_pass_1/0/
 
      call ems_g_rsmi_rl_wk_a_ix(rl_wk_a_ix)
      if (rl_wk_a_ix .lt. 0) goto 8000
      call ems_g_rand_tran_rsdu_norm(rand,
     &     is(p_vr_in_r),
     &     ds(p_mtx_r_v), is(p_mtx_r_ix), is(p_mtx_c_sa),
     &     ds(p_pv_c_v),
     &     ds(p_rsmi_rl_wk_a(rl_wk_a_ix)),
     &     ftran_er, btran_er, is, ds)
      call ems_fr_rsmi_rl_wk_a_ix(rl_wk_a_ix)
      if (max(ftran_er, btran_er) .gt. tl_tran_er) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)ftran_er
         call ems_msg_wr_li(warn_msg_n)
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9020)btran_er
         call ems_msg_wr_li(warn_msg_n)
      else if (pr_pass_1 .eq. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9200)
         call ems_msg_wr_li(rsmi_msg_n)
         pr_pass_1 = 1
      end if
 7000 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9010 format('Error of ', g11.4, ' solving A.x = b for random x')
 9020 format('Error of ', g11.4, ' solving A^x = b for random x')
 9200 format('No error in random tran residuals')
 9800 format('RSMI workspace not available in ems_ca_rand_ck_inv')
      end
 
CM      IF (emsol_dev .EQ. 1) THEN
C?C->>> --------------------------------------------> ems_g_n_pc_vr_el <<<
C?c     Return the number of entries in the matrix being priced.
C?c
C?      subroutine ems_g_n_pc_vr_el(lc_n_pc_vr, lc_n_pc_el,
C?     &     vr_in_c, mtx_c_sa)
C?      implicit none
C?      include 'EMSV.INC'
C?      include 'EMSPM.INC'
C?      include 'RSMICS.INC'
C?      include 'ICTVR.INC'
C?      integer lc_n_pc_vr, lc_n_pc_el
C?      integer vr_in_c(-vr_in_c_n_sn:n_c)
C?      integer mtx_c_sa(0:n_c+1)
C?      integer vr_n, c_n
C?
C?      lc_n_pc_vr = vr_in_c(os_struc_in_c_l_pc_p) -
C?     &     vr_in_c(os_struc_in_c_f_p_m1)
C?      lc_n_pc_el = 0
C?      do 10, c_n =
C?     &     vr_in_c(os_struc_in_c_f_p_m1)+1,
C?     &     vr_in_c(os_struc_in_c_l_pc_p)
C?         vr_n = vr_in_c(c_n)
C?         lc_n_pc_el = lc_n_pc_el + (mtx_c_sa(vr_n+1)-mtx_c_sa(vr_n))
C? 10   continue
C?      return
C?      end
C?
C?      subroutine ems_ck_parsmi_v(ca_n, ds, is)
C?      implicit none
C?      include 'EMSV.INC'
C?      include 'EMSPM.INC'
C?      include 'EMSMMGR.INC'
C?      include 'EMSMEM.INC'
C?      include 'EMSP.INC'
C?      include 'ICTVR.INC'
C?      include 'RLCTVR.INC'
C?      include 'EMSMSG.INC'
C?      include 'RSMICOM.INC'
C?      integer ca_n
C?      double precision ds(0:ds_n_en_m1)
C?      integer is(0:is_n_en_m1)
C?      integer bs_cg_cn
C?      integer vr_ls(10)
C?      double precision ed_wt_ls(10)
C?
C?      integer rd_en_vr_n, rd_lv_vr_n
C?      common/ems_rd_lv_vr_n_com/rd_en_vr_n, rd_lv_vr_n
C?
C?      integer i_null, si_it_n, fwk_n, n_ed_wt, ed_wt_n, vr_n, r_n
C?      double precision ed_wt_er
C?      integer ix_n
C?      integer rd_si_it_n, tr_vr_n, tr_r_n, n_mi_prcs
C?      integer pv_c_cn1, pv_c_cn2
C?      save rd_si_it_n, tr_vr_n, tr_r_n, n_mi_prcs
C?      save pv_c_cn1, pv_c_cn2
C?      data bs_cg_cn/-1/
C?
C?      if (bs_cg_cn .lt. 0) then
C?         print*, 'Enter channel for entering variables'
C?         read*, bs_cg_cn
C?         print*, 'Enter number of MI processes'
C?         read*, n_mi_prcs
C?         pv_c_cn1 = bs_cg_cn+1
C?         open(unit = pv_c_cn1, file = 'pv_c_v.1')
C?         if (n_mi_prcs .gt. 1) then
C?            pv_c_cn2 = bs_cg_cn+2
C?            open(unit = pv_c_cn2, file = 'pv_c_v.2')
C?         endif
C?      endif
C?
C?      if (ts_parsmi .eq. 1) then
C?         if (ca_n .eq. 1) then
C?            read(bs_cg_cn, *)
C?     &           i_null, si_it_n, vr_t_en_bs, rd_lv_vr_n,
C?     &           fwk_n, n_ed_wt,
C?     &           (vr_ls(ed_wt_n), ed_wt_ls(ed_wt_n),
C?     &           ed_wt_n = 1, n_ed_wt)
C?            if (si_it_n .ne. n_si_it+1) then
C?               print*, ' si_it_n .ne. n_si_it+1 ', si_it_n, n_si_it+1
C?               stop
C?            endif
C?            if (vr_t_en_bs .ne. rd_lv_vr_n .and.
C?     &           iand(is(p_st+rd_lv_vr_n), bc_bt) .eq. 0) then
C?               print*, ' rd_lv_vr_n not basic ', rd_lv_vr_n
C?               stop
C?            endif
C?            if (iand(is(p_st+vr_t_en_bs), bc_bt) .ne. 0) then
C?               print*, ' vr_t_en_bs is basic ', vr_t_en_bs
C?               stop
C?            endif
C?         else if (ca_n .eq. 5) then
C?            if (fwk_n .ne. n_dvx_fwk) then
C?               print*, ' fwk_n .ne. n_dvx_fwk ', fwk_n, n_dvx_fwk
C?               stop
C?            endif
C?            do 10, ed_wt_n = 1, n_ed_wt
C?               vr_n = vr_ls(ed_wt_n)
C?               ed_wt_er = abs(ds(p_ed_wt+vr_n)-ed_wt_ls(ed_wt_n))/
C?     &              ds(p_ed_wt+vr_n)
C?               if (ed_wt_er .gt. 1d-2) then
C?                  write(ems_li, 9000)si_it_n, vr_n,
C?     &                 ds(p_ed_wt+vr_n), ed_wt_ls(ed_wt_n), ed_wt_er
C?                  if (ed_wt_er .gt. 1d0) then
C?                     call ems_msg_wr_li(er_msg_n)
C?                  else
C?                     call ems_msg_wr_li(warn_msg_n)
C?                  endif
C?               endif
C? 10         continue
C?         endif
C?      else if (ts_parsmi .eq. 2) then
C?         if (ca_n .eq. 0) then
C?            print*, 'Enter variable to trace'
C?            read*, tr_vr_n
C?            read(bs_cg_cn, *)rd_si_it_n, rd_en_vr_n, rd_lv_vr_n
C?         else if (ca_n .eq. 1) then
C?            vr_t_en_bs = rd_en_vr_n
C?         else if (ca_n .eq. 2) then
C?            read(bs_cg_cn, *)rd_si_it_n, rd_en_vr_n, rd_lv_vr_n
C?            if (rd_en_vr_n .lt. 0) then
C?               rq_inv = 1
C?               read(bs_cg_cn, *)rd_si_it_n, rd_en_vr_n, rd_lv_vr_n
C?            endif
C?         else if (ca_n .eq. 3) then
C?            if (tr_vr_n .gt. mx_n_c) then
C?               r_n = tr_vr_n - mx_n_c
C?               r_n = is(p_og_t_nw_perm+r_n)
C?               write(*, 9200)n_si_it,
C?     &              -ds(p_pi_v+r_n)/ds(p_du_act+vr_t_en_bs)
C?               if (rd_en_vr_n .eq. 0) stop
C?            endif
C?         else if (ca_n .eq. 4) then
C?            if (tr_vr_n .le. n_c) write(*, 9200)n_si_it,
C?     &           ds(p_tbu_r_v+tr_vr_n)/ds(p_du_act+vr_t_en_bs)
C?            if (rd_en_vr_n .eq. 0) stop
C?         else if (ca_n .eq. 6) then
C?
C?c            if (vr_t_en_bs .eq. tr_vr_n) then
C?            write(pv_c_cn1, 9110)n_si_it, vr_t_en_bs
C?            ix_n = 0
C?            write(pv_c_cn1, 9130)
C?            do 1061, r_n = 1, n_r, n_mi_prcs
C?               if (abs(ds(p_pv_c_v+r_n)) .gt. pk_pv_c_ze) then
C?                  ix_n = ix_n + 1
C?                  write(pv_c_cn1, 9135)ix_n, r_n, is(p_vr_in_r+r_n),
C?     &                    ds(p_pv_c_v+r_n)
C?               endif
C? 1061       continue
C?            call ems_flush(pv_c_cn1)
C?            if (n_mi_prcs .gt. 1) then
C?               write(pv_c_cn2, 9110)n_si_it, vr_t_en_bs
C?               ix_n = 0
C?               write(pv_c_cn2, 9130)
C?               do 1062, r_n = 2, n_r, 2
C?                  if (abs(ds(p_pv_c_v+r_n)) .gt. pk_pv_c_ze) then
C?                     ix_n = ix_n + 1
C?                     write(pv_c_cn2, 9135)ix_n, r_n, is(p_vr_in_r+r_n),
C?     &                    ds(p_pv_c_v+r_n)
C?                  endif
C? 1062          continue
C?               call ems_flush(pv_c_cn2)
C?            endif
C?c               stop
C?         endif
C?      else if (ts_parsmi .eq. 3) then
C?         if (ca_n .eq. 0) then
C?            print*, 'Enter row to trace'
C?            read*, tr_r_n
C?            read(bs_cg_cn, *)rd_si_it_n, rd_en_vr_n, rd_lv_vr_n
C?            write(*, 9300)
C?         else if (ca_n .eq. 1) then
C?            vr_t_en_bs = rd_en_vr_n
C?         else if (ca_n .eq. 2) then
C?            read(bs_cg_cn, *)rd_si_it_n, rd_en_vr_n, rd_lv_vr_n
C?            if (rd_en_vr_n .lt. 0) then
C?               rq_inv = 1
C?               read(bs_cg_cn, *)rd_si_it_n, rd_en_vr_n, rd_lv_vr_n
C?            endif
C?         else if (ca_n .eq. 6) then
C?            write(*, 9310)n_si_it, vr_t_en_bs, ds(p_pv_c_v+tr_r_n)
C?         endif
C?      endif
C?      return
C? 9000 format('Iteration:', i5, ' Variable ', i7,
C?     &     ' Ed_Wt ', g11.4, ' Rd_Ed_Wt ', g11.4, ' Error = ', g11.4)
C? 9200 format('Iteration: ', i5, ' Tableau row value = ', g11.4)
C? 9110 format(' it_n = ', i5, ' vr_n = ', i7)
C? 9130 format('ix_n    r_n     vr_n   pv_c_v')
C? 9135 format(i4, 2x, i5, 2x, i7, 2x, g11.4)
C? 9300 format('ix_n     vr_n   pv_r_v')
C? 9310 format(i4, 2x, i7, 2x, g11.4)
C?      end
CM      ENDIF
