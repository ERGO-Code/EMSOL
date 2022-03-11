CM
C->>> ----------------------------------------------------> ems_revn <<<
c     Returns the version number.
c
CM      IF (emsol_epc .EQ. 1) THEN
C?      subroutine ems_tru_revn(
CM      ELSE
      subroutine ems_revn(
CM      ENDIF
     &     rt_cod, ems_rv_n)
      implicit none
      include 'EMSV.INC'
      include 'EMSMSG.INC'
      include 'EMSVERS.INC'
      integer rt_cod, ems_rv_n
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_eq_i2
CM      ENDIF
      integer rn_nm_n_ch
      parameter (rn_nm_n_ch = 8)
      character*(rn_nm_n_ch) rn_nm
      save rn_nm
      data rn_nm/'ems_revn'/
 
      ems_msg_cod = 0
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(ems_msg_iz_ct_vr_com_fg1,
C?     &     ems_msg_iz_ct_vr_com_fg2))
CM      ELSE
      if (ems_msg_iz_ct_vr_com_fg1 .eq. ems_msg_iz_ct_vr_com_fg2)
CM      ENDIF
     &     call ems_msg_iz_ct_vr_com
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(50)
      ems_rv_n = ((mjor_vers_n*256+mnor_vers_n)*256 +
     &     mjor_rv_n)*256 + mnor_rv_n
      rt_cod = ems_msg_cod
      return
 9000 format(31x, 'Entering EMSOL subroutine ', a)
      end
 
C->>> ----------------------------------------------------> ems_mset <<<
c     Change message control settings. Since this routine only changes
c     common message control settings and does not change dspace, it
c     does not call ems_init.
c
CM      IF (emsol_epc .EQ. 1) THEN
C?      subroutine ems_tru_mset(
CM      ELSE
      subroutine ems_mset(
CM      ENDIF
     &     rt_cod, is, sa_n, mx_alw, mx_prt, trace,
     &     usr_xit, e_n, no_n)
      implicit none
      include 'EMSV.INC'
      include 'EMSMSG.INC'
      integer rt_cod, is(0:*), sa_n, mx_alw, mx_prt, trace
      integer usr_xit, e_n, no_n
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_eq_i2
CM      ENDIF
      integer rn_nm_n_ch
      parameter (rn_nm_n_ch = 8)
      character*(rn_nm_n_ch) rn_nm
      save rn_nm
      data rn_nm/'ems_mset'/
 
      ems_msg_cod = 0
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(ems_msg_iz_ct_vr_com_fg1,
C?     &     ems_msg_iz_ct_vr_com_fg2))
CM      ELSE
      if (ems_msg_iz_ct_vr_com_fg1 .eq. ems_msg_iz_ct_vr_com_fg2)
CM      ENDIF
     &     call ems_msg_iz_ct_vr_com
c     if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
c     rn_nm(1:rn_nm_n_ch)
c     call ems_msg_wr_li(50)
      if (sa_n .lt. 0 .or. sa_n .gt. 9999) goto 8101
      if (mx_alw .lt. 0 .or. mx_alw .gt. 0) goto 8102
      if (mx_prt .ge. 0 .and. mx_prt .le. 255) goto 8103
      if (trace .lt. 0 .or. trace .gt. 0) goto 8104
      if (usr_xit .lt. 0 .or. usr_xit .gt. 0) goto 8105
      if (e_n .lt. 1 .or. e_n .gt. 9999) goto 8106
      if (mx_prt .lt. 0) then
         ems_msg_no_prt_fm = sa_n
         ems_msg_no_prt_t = e_n
      else
         ems_msg_no_prt_fm = 10000
         ems_msg_no_prt_t = 0
      endif
      if (no_n .eq. 1) then
         ems_msg_rp_msg_n = 0
      else if (no_n .eq. 2) then
         ems_msg_rp_msg_n = 1
      endif
 7000 continue
      rt_cod = ems_msg_cod
      return
 8101 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810)
     &     rn_nm(1:rn_nm_n_ch),
     &     sa_n, 3, 0, 9999
      call ems_msg_wr_li(7000)
      go to 7000
 8102 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810)
     &     rn_nm(1:rn_nm_n_ch),
     &     mx_alw, 4, 0, 0
      call ems_msg_wr_li(7000)
      go to 7000
 8103 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9811)
     &     rn_nm(1:rn_nm_n_ch),
     &     mx_prt, 5, 0, 255
      call ems_msg_wr_li(serious_msg_n)
      go to 7000
 8104 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810)
     &     rn_nm(1:rn_nm_n_ch),
     &     trace, 6, 0, 0
      call ems_msg_wr_li(7000)
      go to 7000
 8105 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810)
     &     rn_nm(1:rn_nm_n_ch),
     &     usr_xit, 7, 0, 0
      call ems_msg_wr_li(7000)
      go to 7000
 8106 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810)
     &     rn_nm(1:rn_nm_n_ch),
     &     e_n, 8, 1, 9999
      call ems_msg_wr_li(7000)
      go to 7000
c 9000 format(31x, 'Entering EMSOL subroutine ', a)
 9810 format(a, ' has been called with value of ', i9,
     &     ' for parameter ', i1, '. The valid range is ', i9,
     &     ' to ', i9)
 9811 format(a, ' has been called with value of ', i9,
     &     ' for parameter ', i1,
     &     '. This parameter must be less than ', i9,
     &     ' or greater than ', i9)
      end
 
C->>> ----------------------------------------------------> ems_init <<<
c     Initialises EMSOL and sets integer, real and character control
c     variables to their default value. This routine is called by the
c     first call to an EMSOL routine (except for ems_mset). It must be
c     called before a second call to dsca for a particular application.
c     It can be called at any time but all information in ds and the
c     current control variable settings are lost.
c
CM      IF (emsol_epc .EQ. 1) THEN
C?      subroutine ems_tru_init(
CM      ELSE
      subroutine ems_init(
CM      ENDIF
     &     rt_cod, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      integer rt_cod, is(0:*)
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_ne_i2
CM      ENDIF
      integer is_n
      save is_n
      integer rn_nm_n_ch
      parameter (rn_nm_n_ch = 8)
      character*(rn_nm_n_ch) rn_nm
      save rn_nm
      data rn_nm/'ems_init'/
      data is_n/0/
 
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_ne_i2(
C?     &     is(ix_cu_ca_ems_rn_fg1),
C?     &     is(ix_cu_ca_ems_rn_fg2)))
CM      ELSE
      if (is(ix_cu_ca_ems_rn_fg1) .ne. is(ix_cu_ca_ems_rn_fg2))
CM      ENDIF
     &     goto 8990
      is(ix_cu_ca_ems_rn_fg1) = 1
      is(ix_cu_ca_ems_rn_fg2) = 2
      ems_msg_cod = 0
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(50)
c
c     Report the macro settings
c
      call ems_rp_mac
c
c     Augment the index of is declared. This is will have index is_n + 1
c     even if it has been to ems_init before.
c
      is_n = is_n + 1
      is(ix_is_n) = is_n
      is(ix_ca_dsca) = 0
c
c     Initialise the values and ranges of the integer control variables.
c
      call ems_iz_i_ct_vr_df
      call ems_cp_i_a(n_ems_i_ct_vr, i_ct_vr_df, i_ct_vr, 1)
      call ems_se_i_ct_vr_rg
c
c     Initialise the values and ranges of the real control variables.
c
      call ems_iz_rl_ct_vr_df
      call ems_cp_rl_a(n_ems_rl_ct_vr, rl_ct_vr_df, rl_ct_vr, 1)
      call ems_se_rl_ct_vr_rg
c
c     Initialise the character control variables.
c
      call ems_iz_ch_ct_vr
c
c     Initialise the names of control variables
c
      call ems_iz_ems_ct_vr_nm
      call ems_iz_osl_ct_vr_nm
 
      cu_is_n = is(ix_is_n)
      sv_ml_ct_vr = 1
      rt_cod = ems_msg_cod
c
c     Indicate that ems_init has been called with this is(*)
c
      is(ix_ca_init_fg1) = 1
      is(ix_ca_init_fg2) = 2
 7000 continue
      is(ix_cu_ca_ems_rn_fg1) = 0
      is(ix_cu_ca_ems_rn_fg2) = 0
      return
 8990 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9999)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 9000 format(31x, 'Entering EMSOL subroutine ', a)
 9999 format(a, ' cannot be called from a user exit routine')
      end
 
C->>> ---------------------------------------------------> ems_memrq <<<
c     Returns lower bound, estimate and upper bound on the memory
c     requirement in order to solve a single problem with usr_n_r rows,
c     usr_n_c columns and usr_n_a_el constraint matrix entries
c     NB Assumes that operations given by op_msk will be performed.
c
CM      IF (emsol_epc .EQ. 1) THEN
C?      subroutine ems_tru_memrq(
CM      ELSE
      subroutine ems_memrq(
CM      ENDIF
     &     rt_cod, is,
     &     usr_n_ml, usr_n_r, usr_n_c, usr_n_a_el, op_msk,
     &     rq_mem_lb, rq_mem_est, rq_mem_ub)
      implicit none
      include 'EMSV.INC'
      include 'EMSMSG.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      integer rt_cod, is(0:*)
      integer usr_n_ml, usr_n_r, usr_n_c, usr_n_a_el, op_msk
      integer rq_mem_lb, rq_mem_est, rq_mem_ub
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_eq_i2
C?      logical ems_i1_ne_i2
CM      ENDIF
      integer rn_nm_n_ch
      parameter (rn_nm_n_ch = 9)
      character*(rn_nm_n_ch) rn_nm
      save rn_nm
      data rn_nm/'ems_memrq'/
 
      ems_msg_cod = 0
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(ems_msg_iz_ct_vr_com_fg1,
C?     &     ems_msg_iz_ct_vr_com_fg2))
CM      ELSE
      if (ems_msg_iz_ct_vr_com_fg1 .eq. ems_msg_iz_ct_vr_com_fg2)
CM      ENDIF
     &     call ems_msg_iz_ct_vr_com
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(50)
      if (usr_n_ml .le. 0) go to 8101
      if (usr_n_r .le. 0) go to 8102
      if (usr_n_c .le. 0) go to 8103
      if (usr_n_a_el .le. 0) go to 8104
      if (op_msk .lt. 0) go to 8105
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(is(ix_ca_init_fg1), is(ix_ca_init_fg2))) then
CM      ELSE
      if (is(ix_ca_init_fg1) .eq. is(ix_ca_init_fg2)) then
CM      ENDIF
c
c     If ems_init has not been called then, if ems_memrq has been called
c     from within an EMS routine (ie from a user exit
c     routine) return an error. This should not happen anyway!
c
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?         if (ems_i1_ne_i2(
C?     &        is(ix_cu_ca_ems_rn_fg1),
C?     &        is(ix_cu_ca_ems_rn_fg2)))
CM      ELSE
         if (is(ix_cu_ca_ems_rn_fg1) .ne. is(ix_cu_ca_ems_rn_fg2))
CM      ENDIF
     &        goto 8990
c
c     Otherwise, call ems_init now.
c
         call ems_init(ems_msg_cod, is)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      endif
      call ems_g_mem_rq(usr_n_ml, usr_n_r, usr_n_c, usr_n_a_el, op_msk,
     &     rq_mem_lb, rq_mem_est, rq_mem_ub)
 7000 continue
      rt_cod = ems_msg_cod
      return
 8101 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810)
     &     rn_nm(1:rn_nm_n_ch),
     &     usr_n_ml, 3, 1, i_inf
      call ems_msg_wr_li(7029)
      go to 7000
 8102 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810)
     &     rn_nm(1:rn_nm_n_ch),
     &     usr_n_r, 4, 1, i_inf
      call ems_msg_wr_li(7029)
      go to 7000
 8103 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810)
     &     rn_nm(1:rn_nm_n_ch),
     &     usr_n_c, 5, 1, i_inf
      call ems_msg_wr_li(7029)
      go to 7000
 8104 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810)
     &     rn_nm(1:rn_nm_n_ch),
     &     usr_n_a_el, 6, 1, i_inf
      call ems_msg_wr_li(7029)
      go to 7000
 8105 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810)
     &     rn_nm(1:rn_nm_n_ch),
     &     op_msk, 7, 1, i_inf
      call ems_msg_wr_li(7029)
      go to 7000
 8990 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9999)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 9000 format(31x, 'Entering EMSOL subroutine ', a)
 9810 format(a, ' has been called with value of ', i9,
     &     ' for parameter ', i1, '. The valid range is ', i9,
     &     ' to ', i9)
 9999 format(a, ' cannot be called from a user exit routine')
      end
 
C->>> ----------------------------------------------------> ems_dsca <<<
c     Allocates space and initialises the memory manager for a
c     particular application. If information for the current model needs
c     to be saved and the application has changed, nothing
c     can be done and the current model will be corrupted.
c
CM      IF (emsol_epc .EQ. 1) THEN
C?      subroutine ems_tru_dsca(
CM      ELSE
      subroutine ems_dsca(
CM      ENDIF
     &     rt_cod, is, n_rl_wo, n_ml)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMIHDL.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer rt_cod, is(0:*), n_rl_wo, n_ml
      integer mn_blk_p, sos_rt_cod
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_ne_i2, ems_i1_eq_i2
CM      ENDIF
      integer ml_n, fg_n
      integer mem_mgr_rt_cod
      integer rn_nm_n_ch
      parameter (rn_nm_n_ch = 8)
      character*(rn_nm_n_ch) rn_nm
      save rn_nm
      data rn_nm/'ems_dsca'/
 
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_ne_i2(
C?     &     is(ix_cu_ca_ems_rn_fg1),
C?     &     is(ix_cu_ca_ems_rn_fg2)))
CM      ELSE
      if (is(ix_cu_ca_ems_rn_fg1) .ne. is(ix_cu_ca_ems_rn_fg2))
CM      ENDIF
     &     goto 8990
      is(ix_cu_ca_ems_rn_fg1) = 1
      is(ix_cu_ca_ems_rn_fg2) = 2
      ems_msg_cod = 0
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(50)
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(is(ix_ca_init_fg1), is(ix_ca_init_fg2))) then
CM      ELSE
      if (is(ix_ca_init_fg1) .eq. is(ix_ca_init_fg2)) then
CM      ENDIF
c
c     Switch off the `call EMSOL routine' flag during the call to
c     ems_init
c
         is(ix_cu_ca_ems_rn_fg1) = 0
         is(ix_cu_ca_ems_rn_fg2) = 0
         call ems_init(ems_msg_cod, is)
         is(ix_cu_ca_ems_rn_fg1) = 1
         is(ix_cu_ca_ems_rn_fg2) = 2
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      endif
      if (is(ix_ca_dsca) .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)
         call ems_msg_wr_li(3037)
         go to 7000
      endif
      if (n_ml .lt. 1) go to 8101
      if (is(ix_is_n) .ne. cu_is_n) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        is(ix_is_n), cu_is_n
         call ems_msg_wr_li(info_msg_n)
      endif
      if (sv_ml_ct_vr .gt. 0) then
         if (is(ix_is_n) .ne. cu_is_n) then
c
c     Application has changed so current model cannot be saved.
c
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9700)
     &           cu_ml_n, cu_is_n
            call ems_msg_wr_li(er_msg_n)
         endif
      endif
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9200)is(ix_is_n), n_ml
      call ems_msg_wr_li(81)
c
c     Entries is(0:mem_mgr_da_l_is_en) are used by the memory manager.
c     Subsequent entries are used as follows
c
c     is(ix_ca_init_fg1) is set to 1 when init has been called.
c
c     is(ix_ca_init_fg2) is set to 2 when init has been called.
c     .  This mechanism allows equal unassigned patterns to indicate
c     .  that init has not been called.
c
c     is(ix_is_n) is the `is' number.
c     .           This is used to tell which `is' is being passed.
c
c     is(ix_ca_dsca) is set to 1 when dsca has been called.
c
c     is(ix_n_ml) is the number of models in the application.
c
c     is(ix_ca_dscm) is set to 1 when dscm has been called.
c
c     is(ix_cu_ca_ems_rn_fg1) is set to 1 when an EMS routine has been
c     .                       called but has not yet returned.
c
c     is(ix_cu_ca_ems_rn_fg2) is set to 2 when an EMS routine has been
c     .                       called but has not yet returned.
c     .  This mechanism is used to spot illegal calls to EMS routines
c     .  from user exit routines.
c
c     is(p_is_bs_blk+os), os = 0, is_bs_blk_n_wo-1
c     .     is the base block of the application. This contains the
c     .     default control variable settings for the application.
c
c     is(p_ml_bs_blk+os), os = 0, n_ml*ml_bs_blk_n_wo-1
c     .     contains the base blocks of the models. These contain
c
c     .     two flags to indicate whether dscm and ptmi have been called
c     .     for the model
c
c     .     control variable settings for the models
c
c     .     the handles to the model storage.
c
c
c     ?? Update ems_se_com_undn
c
CM      IF (emsol_epc .EQ. 1) THEN
C?      call ems_se_com_undn
CM      ENDIF
      is(ix_n_ml) = n_ml
      cu_is_n = is(ix_is_n)
      ds_n_en_m1 = n_rl_wo-1
      is_n_en_m1 = n_rl_wo*rl_wo_z/i_wo_z-1
      ns_n_en_m1 = n_rl_wo*rl_wo_z/ch_wo_z-1
c
c     Initialise the memory manager so that entries 0...mn_blk_p-1 are
c     not touched by block management.
c
      mn_blk_p = p_ml_bs_blk + n_ml*ml_bs_blk_n_wo
      if (mn_blk_p .gt. n_rl_wo*rl_wo_z) goto 8000
      call ems_mem_mgr_int_iz(mem_mgr_rt_cod, is,
     &     n_rl_wo*rl_wo_z, mn_blk_p)
      if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
         if (mem_mgr_rt_cod .eq. mem_mgr_rt_cod_serious_no_po) then
            ems_msg_cod = ems_msg_lvl_serious
            go to 7000
         else if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
            ems_msg_cod = ems_msg_lvl_serious
            go to 7000
         endif
      endif
      call ems_dn_ems_blk_id(is)
c
c     Indicate that there are no blocks with arrays of solver data and
c     that the data in the solver arrays are not correct.
c
      rsmi_blk_ml_n = 0
      rsmi_blk_st_msk = 0
c
c     Initialise the maximum dimensions for solver arrays.
c
      rsmi_blk_mx_n_r = 0
      rsmi_blk_mx_n_c = 0
c
c     ?? Is ems_iz_sos needed?
c
      call ems_iz_sos(is, is, sos_rt_cod)
      sv_ml_ct_vr = 1
      is(ix_ca_dsca) = 1
      is(ix_ca_dscm) = 0
c
c     Initialise the flags for each model. Currently there are two
c     which indicate whether dscm and ptmi have been called.
c
      do 10, ml_n = 1, n_ml
         do 5, fg_n = 0, ml_bs_blk_n_fg-1
            is(p_ml_bs_blk + (ml_n-1)*ml_bs_blk_n_wo + fg_n) = 0
 5       continue
 10   continue
 
 7000 continue
      is(ix_cu_ca_ems_rn_fg1) = 0
      is(ix_cu_ca_ems_rn_fg2) = 0
      rt_cod = ems_msg_cod
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)n_rl_wo,
     &     mn_blk_p/rl_wo_z+2
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8101 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810)
     &     rn_nm(1:rn_nm_n_ch),
     &     n_ml, 4, 1, i_inf
      call ems_msg_wr_li(7029)
      go to 7000
 8990 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9999)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 9000 format(31x, 'Entering EMSOL subroutine ', a)
 9010 format(' dspace has already been initialized')
 9100 format('Changing from application ', i3, ' to application ', i3)
 9200 format('Application ', i3, ' has ', i7, ' model(s) ')
 9700 format('Before calling ems_dsca, the information for model ', i7,
     &     ' in application ', i3,
     &     ' should have been saved by calling ems_ptmi.',
     &     ' This information cannot be saved by EMSOL since the',
     &     ' application has changed so this model is likely to be',
     &     ' corrupted.')
 9800 format(i9, ' doublewords in dspace is too small.',
     &     ' At least ', i9, ' doublewords are required. ',
     &     ' It is possible that EMSOL has written to addresses',
     &     ' beyond the end of the declared dspace.')
 9810 format(a, ' has been called with value of ', i9,
     &     ' for parameter ', i1, '. The valid range is ', i9,
     &     ' to ', i9)
 9999 format(a, ' cannot be called from a user exit routine')
      end
 
C->>> ----------------------------------------------------> ems_dscm <<<
c     Sets the control variables and initialises the base block for a
c     given model. If this model is not the current model and the
c     information for the current model needs to be saved then this is
c     done with a call to ems_ptmi---unless the application has changed,
c     in which case nothing can be done and the current model will be
c     corrupted.
c
CM      IF (emsol_epc .EQ. 1) THEN
C?      subroutine ems_tru_dscm(
CM      ELSE
      subroutine ems_dscm(
CM      ENDIF
     &     rt_cod, is, ml_n, nblock)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer rt_cod, is(0:*), ml_n, nblock
      integer is_p
      logical t_bs_blk
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_ne_i2, ems_i1_eq_i2
CM      ENDIF
      integer rn_nm_n_ch
      parameter (rn_nm_n_ch = 8)
      character*(rn_nm_n_ch) rn_nm
      save rn_nm
      data rn_nm/'ems_dscm'/
 
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_ne_i2(
C?     &     is(ix_cu_ca_ems_rn_fg1),
C?     &     is(ix_cu_ca_ems_rn_fg2)))
CM      ELSE
      if (is(ix_cu_ca_ems_rn_fg1) .ne. is(ix_cu_ca_ems_rn_fg2))
CM      ENDIF
     &     goto 8990
      is(ix_cu_ca_ems_rn_fg1) = 1
      is(ix_cu_ca_ems_rn_fg2) = 2
      ems_msg_cod = 0
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(50)
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(is(ix_ca_init_fg1), is(ix_ca_init_fg2))) then
CM      ELSE
      if (is(ix_ca_init_fg1) .eq. is(ix_ca_init_fg2)) then
CM      ENDIF
c
c     Switch off the `call EMSOL routine' flag during the call to
c     ems_init
c
         is(ix_cu_ca_ems_rn_fg1) = 0
         is(ix_cu_ca_ems_rn_fg2) = 0
         call ems_init(ems_msg_cod, is)
         is(ix_cu_ca_ems_rn_fg1) = 1
         is(ix_cu_ca_ems_rn_fg2) = 2
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      endif
      if (is(ix_ca_dsca) .eq. 0) go to 8010
c
c     Check that the model number is in range.
c
      if (ml_n .lt. 1 .or. ml_n .gt. is(ix_n_ml)) go to 8101
c
c     Exit if dscm has already been called for this model.
c
      if (is(p_ml_bs_blk+ml_bs_blk_os_ca_dscm_fg) .ne. 0) go to 7000
      if (is(ix_is_n) .ne. cu_is_n) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        is(ix_is_n), cu_is_n
         call ems_msg_wr_li(info_msg_n)
      endif
c
c     Check whether model information has changed for the current model
c     since it was last saved.
c
      if (sv_ml_ct_vr .gt. 0) then
         if (is(ix_is_n) .ne. cu_is_n) then
c
c     Application has changed so current model cannot be saved.
c
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9700)
     &           cu_ml_n, cu_is_n
            call ems_msg_wr_li(er_msg_n)
         else if (ml_n .ne. cu_ml_n) then
c
c     Save the current model.
c
c
c     Switch off the `call EMSOL routine' flag during the call to
c     ems_ptmi
c
            is(ix_cu_ca_ems_rn_fg1) = 0
            is(ix_cu_ca_ems_rn_fg2) = 0
CM      IF (emsol_epc .EQ. 1) THEN
C?            call ems_dum_ptmi(ems_msg_cod, is, cu_ml_n)
CM      ELSE
            call ems_ptmi(ems_msg_cod, is, cu_ml_n)
CM      ENDIF
            is(ix_cu_ca_ems_rn_fg1) = 1
            is(ix_cu_ca_ems_rn_fg2) = 2
            if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
         end if
      end if
c
c     Copy the control variables to/from the base block for the
c     application depending on whether this is the first call to dscm.
c
      is_p = p_is_bs_blk
      t_bs_blk = is(ix_ca_dscm) .eq. 0
      if (t_bs_blk) then
c
c     Reset the values of mx_n_r, mx_n_c and mx_n_a_el to their default
c     values before copying the control variables to the base block.
c     These values can only be changed from their default value between
c     a call to dscm and a call to lmdl/mps.
c
         if (mx_n_r .ne. i_ct_vr_df(ix_mx_n_r)) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9710)
     &           ix_mx_n_r, mx_n_r, i_ct_vr_df(ix_mx_n_r)
            call ems_msg_wr_li(warn_msg_n)
            mx_n_r = i_ct_vr_df(ix_mx_n_r)
         endif
         if (mx_n_c .ne. i_ct_vr_df(ix_mx_n_c)) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9710)
     &           ix_mx_n_c, mx_n_c, i_ct_vr_df(ix_mx_n_c)
            call ems_msg_wr_li(warn_msg_n)
            mx_n_c = i_ct_vr_df(ix_mx_n_c)
         endif
         if (mx_n_a_el .ne. i_ct_vr_df(ix_mx_n_a_el)) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9710)
     &           ix_mx_n_a_el, mx_n_a_el,
     &           i_ct_vr_df(ix_mx_n_a_el)
            call ems_msg_wr_li(warn_msg_n)
            mx_n_a_el = i_ct_vr_df(ix_mx_n_a_el)
         endif
      endif
CM      IF (emsol_epc .EQ. 1) THEN
C?      call ems_dum_cp_ct_vr_t_or_fm_bs_blk(t_bs_blk, is_p, is, is)
CM      ELSE
      call ems_cp_ct_vr_t_or_fm_bs_blk(t_bs_blk, is_p, is, is)
CM      ENDIF
c
c     Initialise the storage handles for the model so that they point
c     to blocks which have not been assigned.
c
      is_p = p_ml_bs_blk + (ml_n-1)*ml_bs_blk_n_wo + ml_bs_blk_os_hdl
      call ems_cp_i_a(ln_ml_hdl, 0, is(is_p), 0)
c
c     Set the current is and model number and indicate that the current
c     model data has not been saved.
c
      cu_is_n = is(ix_is_n)
      cu_ml_n = ml_n
      sv_ml_ct_vr = 1
      is(ix_ca_dscm) = 1
 7000 continue
      is(ix_cu_ca_ems_rn_fg1) = 0
      is(ix_cu_ca_ems_rn_fg2) = 0
      rt_cod = ems_msg_cod
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      go to 7000
 8101 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810)ml_n, is(ix_n_ml)
      call ems_msg_wr_li(7000)
      go to 7000
 8990 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9999)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 9000 format(31x, 'Entering EMSOL subroutine ', a)
 9100 format('Changing from application ', i3, ' to application ', i3)
 9700 format('Before calling ems_dscm, the information for model ', i7,
     &     ' in application ', i3,
     &     ' should have been saved by calling ems_ptmi.',
     &     ' This information cannot be saved by EMSOL since the',
     &     ' application has changed so this model is likely to be',
     &     ' corrupted.')
 9710 format('Resetting integer control variable ', i3, ' from ', i9,
     &     ' to its default value ', i9,
     &     '. To set this control variable for a particular model, ',
     &     'do so after first calling ems_dscm for that model.')
 9801 format('ems_dsca must be called before calling ', a)
 9810 format(' Model number ', i9, ' is invalid:',
     &     i3, ' models have been declared ')
 9999 format(a, ' cannot be called from a user exit routine')
      end
 
C->>> ----------------------------------------------------> ems_gtmi <<<
c     Extracts the model inforamtion for a model. If this model is not
c     the current model and the information for the current model needs
c     to be saved then this is done with a call to ems_ptmi---unless the
c     application has changed, in which case nothing can be done and the
c     current model will be corrupted.
c     This routine is also the way to change from one application to
c     another.
c
CM      IF (emsol_epc .EQ. 1) THEN
C?      subroutine ems_tru_gtmi(
CM      ELSE
      subroutine ems_gtmi(
CM      ENDIF
     &     rt_cod, is, ml_n)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'CHCTVR.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      include 'RSMIHDL.INC'
      integer rt_cod, is(0:*), ml_n
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_ne_i2, ems_i1_eq_i2
CM      ENDIF
      integer is_p
      integer rn_nm_n_ch
      integer ca_rt_cod
      parameter (rn_nm_n_ch = 8)
      character*(rn_nm_n_ch) rn_nm
      save rn_nm
      data rn_nm/'ems_gtmi'/
 
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_ne_i2(
C?     &     is(ix_cu_ca_ems_rn_fg1),
C?     &     is(ix_cu_ca_ems_rn_fg2)))
CM      ELSE
      if (is(ix_cu_ca_ems_rn_fg1) .ne. is(ix_cu_ca_ems_rn_fg2))
CM      ENDIF
     &     goto 8990
      is(ix_cu_ca_ems_rn_fg1) = 1
      is(ix_cu_ca_ems_rn_fg2) = 2
      ems_msg_cod = 0
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(50)
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(is(ix_ca_init_fg1), is(ix_ca_init_fg2))) then
CM      ELSE
      if (is(ix_ca_init_fg1) .eq. is(ix_ca_init_fg2)) then
CM      ENDIF
c
c     Switch off the `call EMSOL routine' flag during the call to
c     ems_init
c
         is(ix_cu_ca_ems_rn_fg1) = 0
         is(ix_cu_ca_ems_rn_fg2) = 0
         call ems_init(ems_msg_cod, is)
         is(ix_cu_ca_ems_rn_fg1) = 1
         is(ix_cu_ca_ems_rn_fg2) = 2
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      endif
      if (is(ix_ca_dsca) .eq. 0) go to 8010
      if (ml_n .lt. 1 .or. ml_n .gt. is(ix_n_ml)) go to 8101
      if (is(p_ml_bs_blk+(ml_n-1)*ml_bs_blk_n_wo+
     &     ml_bs_blk_os_ca_ptmi_fg) .eq. 0) goto 8020
      if (is(ix_is_n) .ne. cu_is_n) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &        is(ix_is_n), cu_is_n
         call ems_msg_wr_li(info_msg_n)
      endif
      if (sv_ml_ct_vr .gt. 0) then
c
c     Model information has changed for the current model since it was
c     last saved.
c
         if (is(ix_is_n) .ne. cu_is_n) then
c
c     Application has changed so current model cannot be saved.
c
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9700)
     &           cu_ml_n, cu_is_n
            call ems_msg_wr_li(er_msg_n)
         else if (ml_n .ne. cu_ml_n) then
c
c     Model has changed within current application so save the model
c     information.
c
c
c     Switch off the `call EMSOL routine' flag during the call to
c     ems_ptmi
c
            is(ix_cu_ca_ems_rn_fg1) = 0
            is(ix_cu_ca_ems_rn_fg2) = 0
CM      IF (emsol_epc .EQ. 1) THEN
C?            call ems_dum_ptmi(ems_msg_cod, is, cu_ml_n)
CM      ELSE
            call ems_ptmi(ems_msg_cod, is, cu_ml_n)
CM      ENDIF
            is(ix_cu_ca_ems_rn_fg1) = 1
            is(ix_cu_ca_ems_rn_fg2) = 2
            if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
         else
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9710)
            call ems_msg_wr_li(warn_msg_n)
         end if
      end if
c
c     Report on the change of model
c
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)ml_n, is(ix_is_n)
      call ems_msg_wr_li(135)
      is_p = p_ml_bs_blk + (ml_n-1)*ml_bs_blk_n_wo + ml_bs_blk_os_ct_vr
c
c     Copy the control variables from the base block.
c
CM      IF (emsol_epc .EQ. 1) THEN
C?      call ems_dum_cp_ct_vr_t_or_fm_bs_blk(.false., is_p, is, is)
CM      ELSE
      call ems_cp_ct_vr_t_or_fm_bs_blk(.false., is_p, is, is)
CM      ENDIF
      if (cu_ml_n .ne. ml_n) goto 8030
      if (cu_is_n .ne. is(ix_is_n)) goto 8040
c
c     Recalculate the model pointers if they are not up-to-date,
c     otherwise just copy them into common.
c
      if (ml_blk_mv_k .lt. is(ix_blk_mv_k)) then
         call ems_g_ml_p(ca_rt_cod, is)
         if (ca_rt_cod .ne. 0) goto 8050
      else
         call ems_cp_ml_p(is)
      endif
      call ems_g_inv_p(ca_rt_cod, is)
      if (ca_rt_cod .ne. 0) goto 8060
      sv_ml_ct_vr = 0
c
c     Re-allocate space for the solver if the data structures are not
c     large enough for the current model
c
      if (mx_n_r .gt. rsmi_blk_mx_n_r .or.
     &     mx_n_c .gt. rsmi_blk_mx_n_c) then
         call ems_iz_al_rsmi_blk(is)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      endif
 7000 continue
      is(ix_cu_ca_ems_rn_fg1) = 0
      is(ix_cu_ca_ems_rn_fg2) = 0
      rt_cod = ems_msg_cod
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      go to 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)
      call ems_msg_wr_li(serious_msg_n)
      go to 7000
 8030 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9803)cu_ml_n, ml_n
      call ems_msg_wr_li(bug_msg_n)
      go to 7000
 8040 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9804)
     &     cu_is_n, is(ix_is_n)
      call ems_msg_wr_li(bug_msg_n)
      go to 7000
 8050 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9805)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 8060 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9806)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 8101 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810)ml_n, is(ix_n_ml)
      call ems_msg_wr_li(7000)
      go to 7000
 8990 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9999)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 9000 format(31x, 'Entering EMSOL subroutine ', a)
 9100 format('Restoring model information as model ', i3,
     &     ' in application ', i3)
 9700 format('Before calling ems_gtmi, the information for model ', i7,
     &     ' in application ', i3,
     &     ' should have been saved by calling ems_ptmi.',
     &     ' This information cannot be saved by EMSOL since the',
     &     ' application has changed so this model is likely to be',
     &     ' corrupted.')
 9710 format('Control variables have been changed for this model but',
     &     ' not saved so will be over-written by last saved values ')
 9801 format('ems_dsca must be called before calling ', a)
 9802 format('ems_ptmi must be called for this model ',
     &     'before attempting to restore it')
 9803 format('Model restored is ', i9, ' not model requested ', i9)
 9804 format('Application restored is ', i9,
     &     ' not application requested ', i9)
 9805 format('Error in ems_g_ml_p')
 9806 format('Error in ems_g_inv_p')
 9810 format(' Model number ', i9, ' is invalid:',
     &     i3, ' models have been declared ')
 9999 format(a, ' cannot be called from a user exit routine')
      end
 
c=======================================================================
c     The routines in this section should only be called with the
c     current application and model and only when the model has been
c     loaded.
 
C->>> ----------------------------------------------------> ems_ptmi <<<
c     Stores the model information for a model.
c
CM      IF (emsol_epc .EQ. 1) THEN
C?      subroutine ems_tru_ptmi(
CM      ELSE
      subroutine ems_ptmi(
CM      ENDIF
     &     rt_cod, is, ml_n)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer rt_cod, is(0:*), ml_n
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_ne_i2, ems_i1_eq_i2
CM      ENDIF
      integer is_p
      integer rn_nm_n_ch
      parameter (rn_nm_n_ch = 8)
      character*(rn_nm_n_ch) rn_nm
      save rn_nm
      data rn_nm/'ems_ptmi'/
 
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_ne_i2(
C?     &     is(ix_cu_ca_ems_rn_fg1),
C?     &     is(ix_cu_ca_ems_rn_fg2)))
CM      ELSE
      if (is(ix_cu_ca_ems_rn_fg1) .ne. is(ix_cu_ca_ems_rn_fg2))
CM      ENDIF
     &     goto 8990
      is(ix_cu_ca_ems_rn_fg1) = 1
      is(ix_cu_ca_ems_rn_fg2) = 2
      ems_msg_cod = 0
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(50)
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(is(ix_ca_init_fg1), is(ix_ca_init_fg2))) then
CM      ELSE
      if (is(ix_ca_init_fg1) .eq. is(ix_ca_init_fg2)) then
CM      ENDIF
c
c     Switch off the `call EMSOL routine' flag during the call to
c     ems_init
c
         is(ix_cu_ca_ems_rn_fg1) = 0
         is(ix_cu_ca_ems_rn_fg2) = 0
         call ems_init(ems_msg_cod, is)
         is(ix_cu_ca_ems_rn_fg1) = 1
         is(ix_cu_ca_ems_rn_fg2) = 2
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      endif
      if (is(ix_ca_dsca) .eq. 0) go to 8010
      if (is(ix_is_n) .ne. cu_is_n) go to 8020
      if (ml_n .lt. 1 .or. ml_n .gt. is(ix_n_ml)) go to 8101
      if (ml_n .ne. cu_ml_n) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9700)cu_ml_n, ml_n
         call ems_msg_wr_li(er_msg_n)
         go to 7000
      end if
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)cu_ml_n, cu_is_n
      call ems_msg_wr_li(134)
      is_p = p_ml_bs_blk + (ml_n-1)*ml_bs_blk_n_wo + ml_bs_blk_os_ct_vr
c
c     Copy the control variables to the base block.
c
CM      IF (emsol_epc .EQ. 1) THEN
C?      call ems_dum_cp_ct_vr_t_or_fm_bs_blk(.true., is_p, is, is)
CM      ELSE
      call ems_cp_ct_vr_t_or_fm_bs_blk(.true., is_p, is, is)
CM      ENDIF
c
c     Indicate that ems_ptmi has been called for this model.
c
      is(p_ml_bs_blk + (ml_n-1)*ml_bs_blk_n_wo +
     &     ml_bs_blk_os_ca_ptmi_fg) = 1
      sv_ml_ct_vr = 0
 7000 continue
      is(ix_cu_ca_ems_rn_fg1) = 0
      is(ix_cu_ca_ems_rn_fg2) = 0
      rt_cod = ems_msg_cod
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      go to 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      go to 7000
 8101 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810)ml_n, is(ix_n_ml)
      call ems_msg_wr_li(7000)
      go to 7000
 8990 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9999)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 9000 format(31x, 'Entering EMSOL subroutine ', a)
 9100 format('Saving model information as model ', i3,
     &     ' in application ', i3)
 9700 format('Current model is ', i3,
     &     ' but ems_ptmi has been called with model ', i3)
 9801 format('ems_dsca must be called before calling ', a)
 9802 format('Not calling ', a, ' with current ds ')
 9810 format(' Model number ', i9, ' is invalid:',
     &     i3, ' models have been declared ')
 9999 format(a, ' cannot be called from a user exit routine')
      end
 
C->>> ----------------------------------------------------> ems_basi <<<
c     Passes ds and ns to the routine which splits them in order to read
c     from a basis file.
c
CM      IF (emsol_epc .EQ. 1) THEN
C?      subroutine ems_tru_basi(
CM      ELSE
      subroutine ems_basi(
CM      ENDIF
     &     rt_cod, is, rd_bs_cn)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSFI.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer rt_cod, is(0:*), rd_bs_cn
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_ne_i2, ems_i1_eq_i2
CM      ENDIF
      integer ml_mtx_fmt, i_da_wr_cn
      integer rn_nm_n_ch
      parameter (rn_nm_n_ch = 8)
      character*(rn_nm_n_ch) rn_nm
      save rn_nm
      data rn_nm/'ems_basi'/
 
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_ne_i2(
C?     &     is(ix_cu_ca_ems_rn_fg1),
C?     &     is(ix_cu_ca_ems_rn_fg2)))
CM      ELSE
      if (is(ix_cu_ca_ems_rn_fg1) .ne. is(ix_cu_ca_ems_rn_fg2))
CM      ENDIF
     &     goto 8990
      is(ix_cu_ca_ems_rn_fg1) = 1
      is(ix_cu_ca_ems_rn_fg2) = 2
      ems_msg_cod = 0
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(50)
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(is(ix_ca_init_fg1), is(ix_ca_init_fg2))) then
CM      ELSE
      if (is(ix_ca_init_fg1) .eq. is(ix_ca_init_fg2)) then
CM      ENDIF
c
c     Switch off the `call EMSOL routine' flag during the call to
c     ems_init
c
         is(ix_cu_ca_ems_rn_fg1) = 0
         is(ix_cu_ca_ems_rn_fg2) = 0
         call ems_init(ems_msg_cod, is)
         is(ix_cu_ca_ems_rn_fg1) = 1
         is(ix_cu_ca_ems_rn_fg2) = 2
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      endif
      if (is(ix_ca_dsca) .eq. 0) go to 8010
      if (is(ix_is_n) .ne. cu_is_n) go to 8020
      if (rd_bs_cn .lt. 0 .or. rd_bs_cn .gt. 99) go to 8101
      if (iand(ml_da_st_msk, ml_da_st_ld) .eq. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
         call ems_msg_wr_li(serious_msg_n)
         goto 7000
      endif
CM      IF (emsol_epc .EQ. 1) THEN
C?      call ems_dum_rd_ml_fi(rd_ml_fi_rn_basi, rd_bs_cn,
C?     &     ml_mtx_fmt, i_da_wr_cn, is, is)
CM      ELSE
      call ems_rd_ml_fi(rd_ml_fi_rn_basi, rd_bs_cn,
     &     ml_mtx_fmt, i_da_wr_cn, is, is)
CM      ENDIF
      if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
 7000 continue
      is(ix_cu_ca_ems_rn_fg1) = 0
      is(ix_cu_ca_ems_rn_fg2) = 0
      rt_cod = ems_msg_cod
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      go to 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      go to 7000
 8101 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810)
     &     rn_nm(1:rn_nm_n_ch),
     &     rd_bs_cn, 3, 0, 99
      call ems_msg_wr_li(7029)
      go to 7000
 8990 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9999)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 9000 format(31x, 'Entering EMSOL subroutine ', a)
 9100 format('No model has been loaded')
 9801 format('ems_dsca must be called before calling ', a)
 9802 format('Not calling ', a, ' with current ds ')
 9810 format(a, ' has been called with value of ', i9,
     &     ' for parameter ', i1, '. The valid range is ', i9,
     &     ' to ', i9)
 9999 format(a, ' cannot be called from a user exit routine')
      end
C->>> ----------------------------------------------------> ems_baso <<<
c     Passes ds, is and ns to the routine which splits them in order to
c     write to a basis file.
c
CM      IF (emsol_epc .EQ. 1) THEN
C?      subroutine ems_tru_baso(
CM      ELSE
      subroutine ems_baso(
CM      ENDIF
     &     rt_cod, is, wr_bs_cn, wr_v)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSFI.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer rt_cod, is(0:*), wr_bs_cn, wr_v
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_eq_i2
CM      ENDIF
      integer ty, n_fld
      integer rn_nm_n_ch
      parameter (rn_nm_n_ch = 8)
      character*(rn_nm_n_ch) rn_nm
      save rn_nm
      data rn_nm/'ems_baso'/
 
      ems_msg_cod = 0
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(50)
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(is(ix_ca_init_fg1), is(ix_ca_init_fg2))) then
CM      ELSE
      if (is(ix_ca_init_fg1) .eq. is(ix_ca_init_fg2)) then
CM      ENDIF
         call ems_init(ems_msg_cod, is)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      endif
      if (is(ix_ca_dsca) .eq. 0) go to 8010
      if (is(ix_is_n) .ne. cu_is_n) go to 8020
      if (wr_bs_cn .lt. 0 .or. wr_bs_cn .gt. 99) go to 8101
      if (wr_v .lt. 0 .or. wr_v .gt. 1) go to 8102
      if (iand(ml_da_st_msk, ml_da_st_ld) .eq. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
         call ems_msg_wr_li(serious_msg_n)
         goto 7000
      endif
CM      IF (emsol_epc .EQ. 1) THEN
C?      call ems_dum_wr_ml_fi(wr_ml_fi_rn_baso, wr_bs_cn,
C?     &     ty, n_fld, wr_v, is, is)
CM      ELSE
      call ems_wr_ml_fi(wr_ml_fi_rn_baso, wr_bs_cn,
     &     ty, n_fld, wr_v, is, is)
CM      ENDIF
 7000 continue
      rt_cod = ems_msg_cod
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      go to 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      go to 7000
 8101 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810)
     &     rn_nm, wr_bs_cn, 3, 0, 99
      call ems_msg_wr_li(7029)
      go to 7000
 8102 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810)
     &     rn_nm(1:rn_nm_n_ch),
     &     wr_v, 4, 0, 1
      call ems_msg_wr_li(7029)
      go to 7000
 9000 format(31x, 'Entering EMSOL subroutine ', a)
 9100 format('No model has been loaded')
 9801 format('ems_dsca must be called before calling ', a)
 9802 format('Not calling ', a, ' with current ds ')
 9810 format(a, ' has been called with value of ', i9,
     &     ' for parameter ', i1, '. The valid range is ', i9,
     &     ' to ', i9)
      end
C->>> ----------------------------------------------------> ems_bcdo <<<
c     Passes ds and is to the routine which splits them in order to
c     write an MPS or index-driven file
c
CM      IF (emsol_epc .EQ. 1) THEN
C?      subroutine ems_tru_bcdo(
CM      ELSE
      subroutine ems_bcdo(
CM      ENDIF
     &     rt_cod, is, wr_ml_cn, ty, n_fld)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSFI.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer rt_cod, is(0:*), wr_ml_cn, ty, n_fld
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_eq_i2
CM      ENDIF
      integer wr_v
      integer rn_nm_n_ch
      parameter (rn_nm_n_ch = 8)
      character*(rn_nm_n_ch) rn_nm
      save rn_nm
      data rn_nm/'ems_bcdo'/
 
      ems_msg_cod = 0
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(50)
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(is(ix_ca_init_fg1), is(ix_ca_init_fg2))) then
CM      ELSE
      if (is(ix_ca_init_fg1) .eq. is(ix_ca_init_fg2)) then
CM      ENDIF
         call ems_init(ems_msg_cod, is)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      endif
      if (is(ix_ca_dsca) .eq. 0) go to 8010
      if (is(ix_is_n) .ne. cu_is_n) go to 8020
      if (wr_ml_cn .lt. 0 .or. wr_ml_cn .gt. 99) go to 8101
      if (ty .lt. 1 .or. ty .gt. 2) go to 8102
      if (n_fld .ne. 2) go to 8103
      if (iand(ml_da_st_msk, ml_da_st_ld) .eq. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
         call ems_msg_wr_li(serious_msg_n)
         goto 7000
      endif
CM      IF (emsol_epc .EQ. 1) THEN
C?      call ems_dum_wr_ml_fi(wr_ml_fi_rn_bcdo, wr_ml_cn,
C?     &     ty, n_fld, wr_v, is, is)
CM      ELSE
      call ems_wr_ml_fi(wr_ml_fi_rn_bcdo, wr_ml_cn,
     &     ty, n_fld, wr_v, is, is)
CM      ENDIF
 7000 continue
      rt_cod = ems_msg_cod
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      go to 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      go to 7000
 8101 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810)
     &     rn_nm(1:rn_nm_n_ch),
     &     wr_ml_cn, 3, 0, 99
      call ems_msg_wr_li(7029)
      go to 7000
 8102 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810)
     &     rn_nm(1:rn_nm_n_ch),
     &     ty, 4, 1, 2
      call ems_msg_wr_li(7029)
      go to 7000
 8103 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810)
     &     rn_nm(1:rn_nm_n_ch),
     &     n_fld, 5, 2, 2
      call ems_msg_wr_li(7029)
      go to 7000
 9000 format(31x, 'Entering EMSOL subroutine ', a)
 9100 format('No model has been loaded')
 9801 format('ems_dsca must be called before calling ', a)
 9802 format('Not calling ', a, ' with current ds ')
 9810 format(a, ' has been called with value of ', i9,
     &     ' for parameter ', i1, '. The valid range is ', i9,
     &     ' to ', i9)
      end
C->>> ----------------------------------------------------> ems_copy <<<
c     Calls the routine to make a column-wise and/or row-wise copy of
c     the current matrix.
c
CM      IF (emsol_epc .EQ. 1) THEN
C?      subroutine ems_tru_copy(
CM      ELSE
      subroutine ems_copy(
CM      ENDIF
     &     rt_cod, is, ty)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer rt_cod, is(0:*), ty
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_ne_i2, ems_i1_eq_i2
CM      ENDIF
      integer rn_nm_n_ch
      parameter (rn_nm_n_ch = 8)
      character*(rn_nm_n_ch) rn_nm
      save rn_nm
      data rn_nm/'ems_copy'/
 
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_ne_i2(
C?     &     is(ix_cu_ca_ems_rn_fg1),
C?     &     is(ix_cu_ca_ems_rn_fg2)))
CM      ELSE
      if (is(ix_cu_ca_ems_rn_fg1) .ne. is(ix_cu_ca_ems_rn_fg2))
CM      ENDIF
     &     goto 8990
      is(ix_cu_ca_ems_rn_fg1) = 1
      is(ix_cu_ca_ems_rn_fg2) = 2
      ems_msg_cod = 0
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(50)
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(is(ix_ca_init_fg1), is(ix_ca_init_fg2))) then
CM      ELSE
      if (is(ix_ca_init_fg1) .eq. is(ix_ca_init_fg2)) then
CM      ENDIF
c
c     Switch off the `call EMSOL routine' flag during the call to
c     ems_init
c
         is(ix_cu_ca_ems_rn_fg1) = 0
         is(ix_cu_ca_ems_rn_fg2) = 0
         call ems_init(ems_msg_cod, is)
         is(ix_cu_ca_ems_rn_fg1) = 1
         is(ix_cu_ca_ems_rn_fg2) = 2
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      endif
      if (is(ix_ca_dsca) .eq. 0) go to 8010
      if (is(ix_is_n) .ne. cu_is_n) go to 8020
      if (ty .lt. 1 .or. ty .gt. 3) go to 8101
      if (iand(ml_da_st_msk, ml_da_st_ld) .eq. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
         call ems_msg_wr_li(serious_msg_n)
         goto 7000
      endif
      if (ty .eq. 1 .or. ty .eq. 2) then
c
c     Remove any existing col copy of the matrix for the user and
c     create a new one.
c
         if (iand(ml_blk_st_msk, ml_blk_st_ml_usr_c_mtx) .ne. 0) then
            call ems_rm_blk_ml_usr_c_mtx(is)
            if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
         endif
         call ems_iz_blk_ml_usr_c_mtx(is)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
CM      IF (emsol_epc .EQ. 1) THEN
C?         call ems_dum_ca_g_ml_usr_c_mtx(is, is)
CM      ELSE
         call ems_ca_g_ml_usr_c_mtx(is, is)
CM      ENDIF
      endif
      if (ty .eq. 1 .or. ty .eq. 3) then
c
c     Remove any existing row copy of the matrix for the user and
c     create a new one.
c
         if (iand(ml_blk_st_msk, ml_blk_st_ml_usr_r_mtx) .ne. 0)
     &        call ems_rm_blk_ml_usr_r_mtx(is)
         call ems_iz_blk_ml_usr_r_mtx(is)
CM      IF (emsol_epc .EQ. 1) THEN
C?         call ems_dum_ca_g_ml_usr_r_mtx(is, is)
CM      ELSE
         call ems_ca_g_ml_usr_r_mtx(is, is)
CM      ENDIF
      endif
 7000 continue
      is(ix_cu_ca_ems_rn_fg1) = 0
      is(ix_cu_ca_ems_rn_fg2) = 0
      rt_cod = ems_msg_cod
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      go to 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      go to 7000
 8101 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810)
     &     rn_nm(1:rn_nm_n_ch),
     &     ty, 3, 1, 3
      call ems_msg_wr_li(7029)
      go to 7000
 8990 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9999)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 9000 format(31x, 'Entering EMSOL subroutine ', a)
 9100 format('No model has been loaded')
 9801 format('ems_dsca must be called before calling ', a)
 9802 format('Not calling ', a, ' with current ds ')
 9810 format(a, ' has been called with value of ', i9,
     &     ' for parameter ', i1, '. The valid range is ', i9,
     &     ' to ', i9)
 9999 format(a, ' cannot be called from a user exit routine')
      end
C->>> ----------------------------------------------------> ems_crsh <<<
c     Determine a crash basis.
c
CM      IF (emsol_epc .EQ. 1) THEN
C?      subroutine ems_tru_crsh(
CM      ELSE
      subroutine ems_crsh(
CM      ENDIF
     &     rt_cod, is, crsh_ty)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer rt_cod, is(0:*), crsh_ty
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_ne_i2, ems_i1_eq_i2
CM      ENDIF
      integer rn_nm_n_ch
      parameter (rn_nm_n_ch = 8)
      character*(rn_nm_n_ch) rn_nm
      save rn_nm
      data rn_nm/'ems_crsh'/
 
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_ne_i2(
C?     &     is(ix_cu_ca_ems_rn_fg1),
C?     &     is(ix_cu_ca_ems_rn_fg2)))
CM      ELSE
      if (is(ix_cu_ca_ems_rn_fg1) .ne. is(ix_cu_ca_ems_rn_fg2))
CM      ENDIF
     &     goto 8990
      is(ix_cu_ca_ems_rn_fg1) = 1
      is(ix_cu_ca_ems_rn_fg2) = 2
      ems_msg_cod = 0
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(50)
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(is(ix_ca_init_fg1), is(ix_ca_init_fg2))) then
CM      ELSE
      if (is(ix_ca_init_fg1) .eq. is(ix_ca_init_fg2)) then
CM      ENDIF
c
c     Switch off the `call EMSOL routine' flag during the call to
c     ems_init
c
         is(ix_cu_ca_ems_rn_fg1) = 0
         is(ix_cu_ca_ems_rn_fg2) = 0
         call ems_init(ems_msg_cod, is)
         is(ix_cu_ca_ems_rn_fg1) = 1
         is(ix_cu_ca_ems_rn_fg2) = 2
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      endif
      if (is(ix_ca_dsca) .eq. 0) go to 8010
      if (is(ix_is_n) .ne. cu_is_n) go to 8020
      if (crsh_ty .lt. 1 .or. crsh_ty .gt. 2) go to 8101
      if (iand(ml_da_st_msk, ml_da_st_ld) .eq. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
         call ems_msg_wr_li(info_msg_n)
         goto 7000
      endif
CM      IF (emsol_epc .EQ. 1) THEN
C?      call ems_dum_ca_crsh_ml(crsh_ty, is, is)
CM      ELSE
      call ems_ca_crsh_ml(crsh_ty, is, is)
CM      ENDIF
      if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      sv_ml_ct_vr = 1
 7000 continue
      is(ix_cu_ca_ems_rn_fg1) = 0
      is(ix_cu_ca_ems_rn_fg2) = 0
      rt_cod = ems_msg_cod
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      go to 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      go to 7000
 8101 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810)
     &     rn_nm(1:rn_nm_n_ch),
     &     crsh_ty, 3, 1, 2
      call ems_msg_wr_li(7029)
      go to 7000
 8990 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9999)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 9000 format(31x, 'Entering EMSOL subroutine ', a)
 9100 format('No model has been loaded')
 9801 format('ems_dsca must be called before calling ', a)
 9802 format('Not calling ', a, ' with current ds ')
 9810 format(a, ' has been called with value of ', i9,
     &     ' for parameter ', i1, '. The valid range is ', i9,
     &     ' to ', i9)
 9999 format(a, ' cannot be called from a user exit routine')
      end
 
C->>> -----------------------------------------------------> ems_mps <<<
CM      IF (emsol_epc .EQ. 1) THEN
C?      subroutine ems_tru_mps(
CM      ELSE
      subroutine ems_mps(
CM      ENDIF
     &     rt_cod, is, rd_ml_cn, ml_mtx_fmt,
     &     i_da_wr_cn)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSFI.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      integer rt_cod, is(0:*), rd_ml_cn, ml_mtx_fmt, i_da_wr_cn
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_ne_i2, ems_i1_eq_i2
CM      ENDIF
      integer rn_nm_n_ch
      parameter (rn_nm_n_ch = 7)
      character*(rn_nm_n_ch) rn_nm
      save rn_nm
      data rn_nm/'ems_mps'/
 
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_ne_i2(
C?     &     is(ix_cu_ca_ems_rn_fg1),
C?     &     is(ix_cu_ca_ems_rn_fg2)))
CM      ELSE
      if (is(ix_cu_ca_ems_rn_fg1) .ne. is(ix_cu_ca_ems_rn_fg2))
CM      ENDIF
     &     goto 8990
      is(ix_cu_ca_ems_rn_fg1) = 1
      is(ix_cu_ca_ems_rn_fg2) = 2
      ems_msg_cod = 0
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(50)
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(is(ix_ca_init_fg1), is(ix_ca_init_fg2))) then
CM      ELSE
      if (is(ix_ca_init_fg1) .eq. is(ix_ca_init_fg2)) then
CM      ENDIF
c
c     Switch off the `call EMSOL routine' flag during the call to
c     ems_init
c
         is(ix_cu_ca_ems_rn_fg1) = 0
         is(ix_cu_ca_ems_rn_fg2) = 0
         call ems_init(ems_msg_cod, is)
         is(ix_cu_ca_ems_rn_fg1) = 1
         is(ix_cu_ca_ems_rn_fg2) = 2
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      endif
      if (is(ix_ca_dsca) .eq. 0) go to 8010
      if (is(ix_is_n) .ne. cu_is_n) go to 8020
      if (is(ix_ca_dscm) .eq. 0) then
         if (cu_ml_n .ne. 1) goto 8030
c
c     Switch off the `call EMSOL routine' flag during the call to
c     ems_dscm
c
         is(ix_cu_ca_ems_rn_fg1) = 0
         is(ix_cu_ca_ems_rn_fg2) = 0
CM      IF (emsol_epc .EQ. 1) THEN
C?         call ems_dum_dscm(ems_msg_cod, is, 1, 1)
CM      ELSE
         call ems_dscm(ems_msg_cod, is, 1, 1)
CM      ENDIF
         is(ix_cu_ca_ems_rn_fg1) = 1
         is(ix_cu_ca_ems_rn_fg2) = 2
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      endif
      if (rd_ml_cn .lt. 0 .or. rd_ml_cn .gt. 99) go to 8101
      if (ml_mtx_fmt .lt. mn_ml_mtx_fmt .or.
     &     ml_mtx_fmt .gt. mx_ml_mtx_fmt) go to 8102
      call ems_iz_ems_fi_com
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tt_da .gt. 0) call ems_tt_rec(mps_tt, -1)
CM      ENDIF
CM      IF (emsol_epc .EQ. 1) THEN
C?      call ems_dum_rd_ml_fi(rd_ml_fi_rn_mps, rd_ml_cn,
C?     &     ml_mtx_fmt, i_da_wr_cn, is, is)
CM      ELSE
      call ems_rd_ml_fi(rd_ml_fi_rn_mps, rd_ml_cn,
     &     ml_mtx_fmt, i_da_wr_cn, is, is)
CM      ENDIF
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 2000
      call ems_wr_ml_z
      sv_ml_ct_vr = 1
 2000 continue
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tt_da .gt. 0) call ems_tt_rec(-mps_tt, -1)
CM      ENDIF
 7000 continue
      is(ix_cu_ca_ems_rn_fg1) = 0
      is(ix_cu_ca_ems_rn_fg2) = 0
      rt_cod = ems_msg_cod
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      go to 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      go to 7000
 8030 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9803)cu_ml_n
      call ems_msg_wr_li(bug_msg_n)
      go to 7000
 8101 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810)
     &     rn_nm(1:rn_nm_n_ch),
     &     rd_ml_cn, 3, 0, 99
      call ems_msg_wr_li(7029)
      go to 7000
 8102 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810)
     &     rn_nm(1:rn_nm_n_ch),
     &     ml_mtx_fmt, 4,
     &     mn_ml_mtx_fmt, mx_ml_mtx_fmt
      call ems_msg_wr_li(7029)
      go to 7000
 8990 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9999)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 9000 format(31x, 'Entering EMSOL subroutine ', a)
 9801 format('ems_dsca must be called before calling ', a)
 9802 format('Not calling ', a, ' with current ds ')
 9803 format('Since ems_dscm has not yet been called for this',
     &     'application, the current model should be 1 not ', i9)
 9810 format(a, ' has been called with value of ', i9,
     &     ' for parameter ', i1, '. The valid range is ', i9,
     &     ' to ', i9)
 9999 format(a, ' cannot be called from a user exit routine')
      end
C->>> --------------------------------------------------> ems_rgda <<<
c     Passes ds and is to the routine which splits them in order to
c     reset the constraint matrix.
c
CM      IF (emsol_epc .EQ. 1) THEN
C?      subroutine ems_tru_rgda(
CM      ELSE
      subroutine ems_rgda(
CM      ENDIF
     &     rt_cod, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      integer rt_cod, is(0:*)
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_ne_i2, ems_i1_eq_i2
CM      ENDIF
      integer rn_nm_n_ch
      parameter (rn_nm_n_ch = 8)
      character*(rn_nm_n_ch) rn_nm
      save rn_nm
      data rn_nm/'ems_rgda'/
 
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_ne_i2(
C?     &     is(ix_cu_ca_ems_rn_fg1),
C?     &     is(ix_cu_ca_ems_rn_fg2)))
CM      ELSE
      if (is(ix_cu_ca_ems_rn_fg1) .ne. is(ix_cu_ca_ems_rn_fg2))
CM      ENDIF
     &     goto 8990
      is(ix_cu_ca_ems_rn_fg1) = 1
      is(ix_cu_ca_ems_rn_fg2) = 2
      ems_msg_cod = 0
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(50)
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(is(ix_ca_init_fg1), is(ix_ca_init_fg2))) then
CM      ELSE
      if (is(ix_ca_init_fg1) .eq. is(ix_ca_init_fg2)) then
CM      ENDIF
c
c     Switch off the `call EMSOL routine' flag during the call to
c     ems_init
c
         is(ix_cu_ca_ems_rn_fg1) = 0
         is(ix_cu_ca_ems_rn_fg2) = 0
         call ems_init(ems_msg_cod, is)
         is(ix_cu_ca_ems_rn_fg1) = 1
         is(ix_cu_ca_ems_rn_fg2) = 2
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      endif
      if (is(ix_ca_dsca) .eq. 0) go to 8010
      if (is(ix_is_n) .ne. cu_is_n) go to 8020
      if (iand(ml_da_st_msk, ml_da_st_ld) .eq. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
         call ems_msg_wr_li(serious_msg_n)
         goto 7000
      endif
CM      IF (emsol_tt .EQ. 1) THEN
C?c      if (g_tt_da .gt. 0) call ems_tt_rec(rgda_tt, -1)
CM      ENDIF
CM      IF (emsol_epc .EQ. 1) THEN
C?      call ems_dum_rg_da(is, is)
CM      ELSE
      call ems_rg_da(is, is)
CM      ENDIF
CM      IF (emsol_tt .EQ. 1) THEN
C?c      if (g_tt_da .gt. 0) call ems_tt_rec(-rgda_tt, -1)
CM      ENDIF
      sv_ml_ct_vr = 1
 7000 continue
      is(ix_cu_ca_ems_rn_fg1) = 0
      is(ix_cu_ca_ems_rn_fg2) = 0
      rt_cod = ems_msg_cod
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      go to 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      go to 7000
 8990 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9999)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 9000 format(31x, 'Entering EMSOL subroutine ', a)
 9100 format('No model has been loaded')
 9801 format('ems_dsca must be called before calling ', a)
 9802 format('Not calling ', a, ' with current ds ')
 9999 format(a, ' cannot be called from a user exit routine')
      end
 
C->>> ----------------------------------------------------> ems_lmdl <<<
c     Loads the linear continuous parts of a model.
c
CM      IF (emsol_epc .EQ. 1) THEN
C?      subroutine ems_tru_lmdl(
CM      ELSE
      subroutine ems_lmdl(
CM      ENDIF
     &     rt_cod, is, usr_mtx_fmt,
     &     usr_n_r, usr_n_c, usr_n_el,
     &     usr_co, usr_r_lb, usr_r_ub, usr_c_lb, usr_c_ub,
     &     usr_mtx_r_i, usr_mtx_c_i, usr_mtx_v)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'ICTVR.INC'
      include 'CHCTVR.INC'
      include 'EMSMSG.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      integer rt_cod, is(0:*), usr_mtx_fmt, usr_n_r, usr_n_c, usr_n_el
      integer usr_mtx_r_i(*), usr_mtx_c_i(*)
      double precision usr_co(*), usr_mtx_v(*)
      double precision usr_r_lb(*), usr_r_ub(*)
      double precision usr_c_lb(*), usr_c_ub(*)
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_ne_i2, ems_i1_eq_i2
CM      ENDIF
      character*8 null_ch8
      integer rn_nm_n_ch
      parameter (rn_nm_n_ch = 8)
      character*(rn_nm_n_ch) rn_nm
      save rn_nm
      data rn_nm/'ems_lmdl'/
 
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_ne_i2(
C?     &     is(ix_cu_ca_ems_rn_fg1),
C?     &     is(ix_cu_ca_ems_rn_fg2)))
CM      ELSE
      if (is(ix_cu_ca_ems_rn_fg1) .ne. is(ix_cu_ca_ems_rn_fg2))
CM      ENDIF
     &     goto 8990
      is(ix_cu_ca_ems_rn_fg1) = 1
      is(ix_cu_ca_ems_rn_fg2) = 2
      ems_msg_cod = 0
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(50)
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(is(ix_ca_init_fg1), is(ix_ca_init_fg2))) then
CM      ELSE
      if (is(ix_ca_init_fg1) .eq. is(ix_ca_init_fg2)) then
CM      ENDIF
c
c     Switch off the `call EMSOL routine' flag during the call to
c     ems_init
c
         is(ix_cu_ca_ems_rn_fg1) = 0
         is(ix_cu_ca_ems_rn_fg2) = 0
         call ems_init(ems_msg_cod, is)
         is(ix_cu_ca_ems_rn_fg1) = 1
         is(ix_cu_ca_ems_rn_fg2) = 2
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      endif
      if (is(ix_ca_dsca) .eq. 0) go to 8010
      if (is(ix_is_n) .ne. cu_is_n) go to 8020
      if (is(ix_ca_dscm) .eq. 0) then
         if (cu_ml_n .ne. 1) goto 8030
c
c     Switch off the `call EMSOL routine' flag during the call to
c     ems_dscm
c
         is(ix_cu_ca_ems_rn_fg1) = 0
         is(ix_cu_ca_ems_rn_fg2) = 0
CM      IF (emsol_epc .EQ. 1) THEN
C?         call ems_dum_dscm(ems_msg_cod, is, 1, 1)
CM      ELSE
         call ems_dscm(ems_msg_cod, is, 1, 1)
CM      ENDIF
         is(ix_cu_ca_ems_rn_fg1) = 1
         is(ix_cu_ca_ems_rn_fg2) = 2
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      endif
      if (usr_mtx_fmt .lt. mn_ml_mtx_fmt .or.
     &     usr_mtx_fmt .gt. mx_ml_mtx_fmt) go to 8101
      if (usr_n_r .lt. 1) go to 8102
      if (usr_n_c .lt. 1) go to 8103
      if (usr_n_el .lt. 1) go to 8104
      call ems_iz_ems_fi_com
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tt_da .gt. 0) call ems_tt_rec(ld_ml_tt, -1)
CM      ENDIF
c
c     If there is any existing model then remove it.
c
      if (ml_blk_st_msk .ne. 0 .or.
     &     ml_da_st_msk .ne. 0) call ems_rm_ml(is)
CM      IF (emsol_epc .EQ. 1) THEN
C?      call ems_dum_ld_ml(usr_mtx_fmt,
CM      ELSE
      call ems_ld_ml(usr_mtx_fmt,
CM      ENDIF
     &     usr_n_r, usr_n_c, usr_n_el,
     &     usr_co, usr_r_lb, usr_r_ub, usr_c_lb, usr_c_ub,
     &     usr_mtx_r_i, usr_mtx_c_i, usr_mtx_v, is, is)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_iz_blk_ml_sol(is)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_iz_blk_ml_vr_ls(is)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_reset_pc_alg(dvx_mode, is)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
CM      IF (emsol_epc .EQ. 1) THEN
C?      call ems_dum_ca_iz_lg_bs(is, is)
CM      ELSE
      call ems_ca_iz_lg_bs(is, is)
CM      ENDIF
c
c     Invent names for the model, its basis and its rows and columns
c
      ch_ml_nm = 'UNNAMED '
      ch_bs_nm = 'UNNAMED '
CM      IF (emsol_epc .EQ. 1) THEN
C?      call ems_dum_se_vr_nm(
C?     &     mx_n_r, null_ch8, 1,
C?     &     mx_n_c, null_ch8, 1,
C?     &     1, is, is)
CM      ELSE
      call ems_se_vr_nm(
     &     mx_n_r, null_ch8, 1,
     &     mx_n_c, null_ch8, 1,
     &     1, is, is)
CM      ENDIF
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
c
c     Indicate that a model has been loaded.
c
      ml_da_st_msk = ml_da_st_msk + ml_da_st_ld
      sv_ml_ct_vr = 1
 7000 continue
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tt_da .gt. 0) call ems_tt_rec(-ld_ml_tt, -1)
CM      ENDIF
      is(ix_cu_ca_ems_rn_fg1) = 0
      is(ix_cu_ca_ems_rn_fg2) = 0
      rt_cod = ems_msg_cod
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      go to 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      go to 7000
 8030 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9803)cu_ml_n
      call ems_msg_wr_li(bug_msg_n)
      go to 7000
 8101 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810)
     &     rn_nm(1:rn_nm_n_ch),
     &     usr_mtx_fmt, 3,
     &     mn_ml_mtx_fmt, mx_ml_mtx_fmt
      call ems_msg_wr_li(7029)
      go to 7000
 8102 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810)
     &      rn_nm, usr_n_r, 4, 1, i_inf
      call ems_msg_wr_li(7029)
      go to 7000
 8103 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810)
     &      rn_nm, usr_n_c, 5, 1, i_inf
      call ems_msg_wr_li(7029)
      go to 7000
 8104 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810)
     &     rn_nm, usr_n_el, 6, 1, i_inf
      call ems_msg_wr_li(7029)
      go to 7000
 8990 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9999)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 9000 format(31x, 'Entering EMSOL subroutine ', a)
 9801 format('ems_dsca must be called before calling ', a)
 9802 format('Not calling ', a, ' with current ds ')
 9803 format('Since ems_dscm has not yet been called for this',
     &     'application, the current model should be 1 not ', i9)
 9810 format(a, ' has been called with value of ', i9,
     &     ' for parameter ', i1, '. The valid range is ', i9,
     &     ' to ', i9)
 9999 format(a, ' cannot be called from a user exit routine')
      end
C->>> ----------------------------------------------------> ems_name <<<
c     Calls ems_se_vr_nm to set variable names for the model.
c
CM      IF (emsol_epc .EQ. 1) THEN
C?      subroutine ems_tru_name(
CM      ELSE
      subroutine ems_name(
CM      ENDIF
     &     rt_cod, is,
     &     usr_n_r_nm, usr_r_nm, usr_sa_r,
     &     usr_n_c_nm, usr_c_nm, usr_sa_c, ems_nm)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'EMSMSG.INC'
      include 'ICTVR.INC'
      integer rt_cod, is(0:*), ems_nm, usr_n_r_nm, usr_sa_r
      integer usr_n_c_nm, usr_sa_c
      character*(*) usr_c_nm(*), usr_r_nm(*)
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_eq_i2
CM      ENDIF
      integer rn_nm_n_ch
      parameter (rn_nm_n_ch = 8)
      character*(rn_nm_n_ch) rn_nm
      save rn_nm
      data rn_nm/'ems_name'/
 
      ems_msg_cod = 0
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(50)
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(is(ix_ca_init_fg1), is(ix_ca_init_fg2))) then
CM      ELSE
      if (is(ix_ca_init_fg1) .eq. is(ix_ca_init_fg2)) then
CM      ENDIF
         call ems_init(ems_msg_cod, is)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      endif
      if (is(ix_ca_dsca) .eq. 0) go to 8010
      if (is(ix_is_n) .ne. cu_is_n) go to 8020
      if (ems_nm .lt. 0 .or. ems_nm .gt. 1) go to 8101
      if (iand(ml_da_st_msk, ml_da_st_ld) .eq. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
         call ems_msg_wr_li(serious_msg_n)
         goto 7000
      endif
CM      IF (emsol_epc .EQ. 1) THEN
C?      call ems_dum_se_vr_nm(
C?     &     usr_n_r_nm, usr_r_nm, usr_sa_r,
C?     &     usr_n_c_nm, usr_c_nm, usr_sa_c, ems_nm, is, is)
CM      ELSE
      call ems_se_vr_nm(
     &     usr_n_r_nm, usr_r_nm, usr_sa_r,
     &     usr_n_c_nm, usr_c_nm, usr_sa_c, ems_nm, is, is)
CM      ENDIF
      sv_ml_ct_vr = 1
 7000 continue
      rt_cod = ems_msg_cod
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      go to 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      go to 7000
 8101 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810)
     &     rn_nm(1:rn_nm_n_ch),
     &     ems_nm, 9, 0, 1
      call ems_msg_wr_li(7029)
      go to 7000
 9000 format(31x, 'Entering EMSOL subroutine ', a)
 9100 format('No model has been loaded')
 9801 format('ems_dsca must be called before calling ', a)
 9802 format('Not calling ', a, ' with current ds ')
 9810 format(a, ' has been called with value of ', i9,
     &     ' for parameter ', i1, '. The valid range is ', i9,
     &     ' to ', i9)
      end
 
C->>> ----------------------------------------------------> ems_prts <<<
c     Calls ems_da to set up the data, then ems_lp_rs or ems_milp_rs to
c     write it out and ems_ca_wr_cu_mtx to write out the current
c     matrix (if required).
c
CM      IF (emsol_epc .EQ. 1) THEN
C?      subroutine ems_tru_prts(
CM      ELSE
      subroutine ems_prts(
CM      ENDIF
     &     rt_cod, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'EMSFI.INC'
      include 'PRTS.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer rt_cod, is(0:*)
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_eq_i2
CM      ENDIF
      integer rn_nm_n_ch
      parameter (rn_nm_n_ch = 8)
      character*(rn_nm_n_ch) rn_nm
      save rn_nm
      data rn_nm/'ems_prts'/
 
      ems_msg_cod = 0
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(50)
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(is(ix_ca_init_fg1), is(ix_ca_init_fg2))) then
CM      ELSE
      if (is(ix_ca_init_fg1) .eq. is(ix_ca_init_fg2)) then
CM      ENDIF
         call ems_init(ems_msg_cod, is)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      endif
      if (is(ix_ca_dsca) .eq. 0) go to 8010
      if (is(ix_is_n) .ne. cu_is_n) go to 8020
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(iz_prts_com_fg1, iz_prts_com_fg2))
CM      ELSE
      if (iz_prts_com_fg1 .eq. iz_prts_com_fg2)
CM      ENDIF
     &     call ems_iz_prts_com
CM      IF (emsol_epc .EQ. 1) THEN
C?      call ems_dum_lp_rs(is, is)
CM      ELSE
      call ems_lp_rs(is, is)
CM      ENDIF
CM      IF (emsol_epc .EQ. 1) THEN
C?      call ems_dum_ca_wr_lp_co(is, is)
CM      ELSE
      call ems_ca_wr_lp_co(is, is)
CM      ENDIF
CM      IF (emsol_epc .EQ. 1) THEN
C?      call ems_dum_ca_wr_cu_mtx(is, is)
CM      ELSE
      call ems_ca_wr_cu_mtx(is, is)
CM      ENDIF
CM      IF (emsol_epc .EQ. 1) THEN
C?      call ems_dum_ca_wr_rg_da(is, is)
CM      ELSE
      call ems_ca_wr_rg_da(is, is)
CM      ENDIF
CM      IF (emsol_deb .EQ. 1) THEN
C?      call ems_flush(ems_wr_cn)
CM      ENDIF
 7000 continue
      rt_cod = ems_msg_cod
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      go to 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      go to 7000
 9000 format(31x, 'Entering EMSOL subroutine ', a)
 9801 format('ems_dsca must be called before calling ', a)
 9802 format('Not calling ', a, ' with current ds ')
      end
C->>> ----------------------------------------------------> ems_scal <<<
c     Calls the scaling routine.
c
CM      IF (emsol_epc .EQ. 1) THEN
C?      subroutine ems_tru_scal(
CM      ELSE
      subroutine ems_scal(
CM      ENDIF
     &     rt_cod, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer rt_cod, is(0:*)
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_ne_i2, ems_i1_eq_i2
CM      ENDIF
      integer rn_nm_n_ch
      parameter (rn_nm_n_ch = 8)
      character*(rn_nm_n_ch) rn_nm
      save rn_nm
      data rn_nm/'ems_scal'/
 
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_ne_i2(
C?     &     is(ix_cu_ca_ems_rn_fg1),
C?     &     is(ix_cu_ca_ems_rn_fg2)))
CM      ELSE
      if (is(ix_cu_ca_ems_rn_fg1) .ne. is(ix_cu_ca_ems_rn_fg2))
CM      ENDIF
     &     goto 8990
      is(ix_cu_ca_ems_rn_fg1) = 1
      is(ix_cu_ca_ems_rn_fg2) = 2
      ems_msg_cod = 0
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(50)
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(is(ix_ca_init_fg1), is(ix_ca_init_fg2))) then
CM      ELSE
      if (is(ix_ca_init_fg1) .eq. is(ix_ca_init_fg2)) then
CM      ENDIF
c
c     Switch off the `call EMSOL routine' flag during the call to
c     ems_init
c
         is(ix_cu_ca_ems_rn_fg1) = 0
         is(ix_cu_ca_ems_rn_fg2) = 0
         call ems_init(ems_msg_cod, is)
         is(ix_cu_ca_ems_rn_fg1) = 1
         is(ix_cu_ca_ems_rn_fg2) = 2
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      endif
      if (is(ix_ca_dsca) .eq. 0) go to 8010
      if (is(ix_is_n) .ne. cu_is_n) go to 8020
      if (iand(ml_da_st_msk, ml_da_st_ld) .eq. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
         call ems_msg_wr_li(serious_msg_n)
         goto 7000
      endif
c
c     Calculate scaling factors.
c
CM      IF (emsol_epc .EQ. 1) THEN
C?      call ems_dum_ca_g_ml_scl_fac(is, is)
CM      ELSE
      call ems_ca_g_ml_scl_fac(is, is)
CM      ENDIF
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 1000
c
c     Scale the model matrix and solution
c
CM      IF (emsol_epc .EQ. 1) THEN
C?      call ems_dum_scl_ml_mtx(is, is)
CM      ELSE
      call ems_scl_ml_mtx(is, is)
CM      ENDIF
CM      IF (emsol_epc .EQ. 1) THEN
C?      call ems_dum_scl_ml_sol(is, is)
CM      ELSE
      call ems_scl_ml_sol(is, is)
CM      ENDIF
 1000 continue
      sv_ml_ct_vr = 1
 7000 continue
      is(ix_cu_ca_ems_rn_fg1) = 0
      is(ix_cu_ca_ems_rn_fg2) = 0
      rt_cod = ems_msg_cod
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      go to 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      go to 7000
 8990 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9999)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 9000 format(31x, 'Entering EMSOL subroutine ', a)
 9100 format('No model has been loaded')
 9801 format('ems_dsca must be called before calling ', a)
 9802 format('Not calling ', a, ' with current ds ')
 9999 format(a, ' cannot be called from a user exit routine')
      end
C->>> ----------------------------------------------------> ems_smap <<<
c     Prints out the memory manager data.
c
CM      IF (emsol_epc .EQ. 1) THEN
C?      subroutine ems_tru_smap(
CM      ELSE
      subroutine ems_smap(
CM      ENDIF
     &     rt_cod, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer rt_cod, is(0:*)
      integer mem_mgr_rt_cod
      integer rn_nm_n_ch
      parameter (rn_nm_n_ch = 8)
      character*(rn_nm_n_ch) rn_nm
      save rn_nm
      data rn_nm/'ems_smap'/
 
      ems_msg_cod = 0
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(50)
      if (is(ix_ca_dsca) .eq. 0) go to 8010
      if (is(ix_is_n) .ne. cu_is_n) go to 8020
CM      IF (emsol_deb .EQ. 1) THEN
C?      call ems_mem_mgr_rp_mem_use(mem_mgr_rt_cod, is,
C?     &     -1, 2, 'From smap')
CM      ELSE
      call ems_mem_mgr_rp_mem_use(mem_mgr_rt_cod, is,
     &     -1, 2, 'From smap')
CM      ENDIF
 7000 continue
      rt_cod = ems_msg_cod
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)
     &     rn_nm(1:rn_nm_n_ch)
 
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      go to 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      go to 7000
 9000 format(31x, 'Entering EMSOL subroutine ', a)
 9801 format('ems_dsca must be called before calling ', a)
 9802 format('Not calling ', a, ' with current ds ')
      end
C->>> ----------------------------------------------------> ems_sslv <<<
c     Converts the EMSOL initial status mode into the internal initial
c     status mode and calls the LP solver.
c
CM      IF (emsol_epc .EQ. 1) THEN
C?      subroutine ems_tru_sslv(
CM      ELSE
      subroutine ems_sslv(
CM      ENDIF
     &     rt_cod, is,
     &     lp_alg_mode, usr_lp_iz_mode)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      include 'MORSMI.INC'
CM      IF (emsol_km .EQ. 1) THEN
C?      include 'EMSKM.INC'
CM      ENDIF
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
 
      integer rt_cod, is(0:*), lp_alg_mode, usr_lp_iz_mode
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_ne_i2, ems_i1_eq_i2
CM      ENDIF
c      integer mem_mgr_rt_cod
      logical reset_loop
      integer lp_iz_mode
      integer rn_nm_n_ch
      parameter (rn_nm_n_ch = 8)
      character*(rn_nm_n_ch) rn_nm
      save rn_nm
      data rn_nm/'ems_sslv'/
 
CM      IF (emsol_km .EQ. 1) THEN
C?      km_tom_inv_sing_msk = 0
CM      ENDIF
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_ne_i2(
C?     &     is(ix_cu_ca_ems_rn_fg1),
C?     &     is(ix_cu_ca_ems_rn_fg2)))
CM      ELSE
      if (is(ix_cu_ca_ems_rn_fg1) .ne. is(ix_cu_ca_ems_rn_fg2))
CM      ENDIF
     &     goto 8990
      is(ix_cu_ca_ems_rn_fg1) = 1
      is(ix_cu_ca_ems_rn_fg2) = 2
      ems_msg_cod = 0
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(50)
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(is(ix_ca_init_fg1), is(ix_ca_init_fg2))) then
CM      ELSE
      if (is(ix_ca_init_fg1) .eq. is(ix_ca_init_fg2)) then
CM      ENDIF
c
c     Switch off the `call EMSOL routine' flag during the call to
c     ems_init
c
         is(ix_cu_ca_ems_rn_fg1) = 0
         is(ix_cu_ca_ems_rn_fg2) = 0
         call ems_init(ems_msg_cod, is)
         is(ix_cu_ca_ems_rn_fg1) = 1
         is(ix_cu_ca_ems_rn_fg2) = 2
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      endif
      if (is(ix_ca_dsca) .eq. 0) go to 8010
      if (is(ix_is_n) .ne. cu_is_n) go to 8020
      if (lp_alg_mode .lt. mn_lp_alg_mode .or.
     &     lp_alg_mode .gt. mx_lp_alg_mode) go to 8101
      if (usr_lp_iz_mode .lt. mn_lp_iz_mode .or.
     &     usr_lp_iz_mode .gt. mx_lp_iz_mode) go to 8102
      if (iand(ml_da_st_msk, ml_da_st_ld) .eq. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
         call ems_msg_wr_li(serious_msg_n)
         goto 7000
      endif
c
c     Apply any scaling factors which exist if the model solution is
c     un-scaled.
c
      if (iand(ml_da_st_msk, ml_da_st_scl_ml) .ne. 0 .and.
     &     iand(ml_da_st_msk, ml_da_st_scl_ml_sol) .eq. 0)
CM      IF (emsol_epc .EQ. 1) THEN
C?     &     call ems_dum_scl_ml_sol(is, is)
CM      ELSE
     &     call ems_scl_ml_sol(is, is)
CM      ENDIF
 
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tt_da .gt. 0) call ems_tt_rec(sslv_tt, -1)
CM      ENDIF
CM      IF (emsol_da .EQ. 1) THEN
C?      call ems_iz_rsmi_stat
CM      ENDIF
      if (n_aux_blk .ne. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9700)
         call ems_msg_wr_li(warn_msg_n)
CM      IF (emsol_xa .EQ. 1) THEN
C?c
C?c     Switch off the `call EMSOL routine' flag during the call to
C?c     ems_nwmt
C?c
C?         is(ix_cu_ca_ems_rn_fg1) = 0
C?         is(ix_cu_ca_ems_rn_fg2) = 0
C?         call ems_nwmt(ems_msg_cod, is, 2)
C?         is(ix_cu_ca_ems_rn_fg1) = 1
C?         is(ix_cu_ca_ems_rn_fg2) = 2
C?         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 2000
CM      ENDIF
      end if
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(ca_iz_mo_rsmi_fg1, ca_iz_mo_rsmi_fg2)) then
CM      ELSE
      if (ca_iz_mo_rsmi_fg1 .eq. ca_iz_mo_rsmi_fg2) then
CM      ENDIF
         ca_iz_mo_rsmi_fg1 = 1
         ca_iz_mo_rsmi_fg2 = 2
         mo_rsmi_rec = .false.
      endif
c      call ems_rp_ml_da_st_msk
      call ems_rp_ml_da_no_cg_msk
c      call ems_rp_ml_blk_st_msk
c      call ems_mem_mgr_rp_mem_use(mem_mgr_rt_cod, is, ems_msg_wr_cn,
c     &     2, 'Before IZ_RSMI')
c      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
CM      IF (emsol_tt .EQ. 1) THEN
C?      call ems_iz_ems_tt_lg(g_tt_da)
CM      ENDIF
      call ems_sv_bs_cg_iz
      lp_iz_mode = 1
CM      IF (emsol_xa .EQ. 1) THEN
C?      if (lp_iz_mode .eq. 1) then
CM      ENDIF
CM      IF (emsol_epc .EQ. 1) THEN
C?         call ems_dum_iz_rsmi(lp_alg_mode, lp_iz_mode, is, is)
CM      ELSE
         call ems_iz_rsmi(lp_alg_mode, lp_iz_mode, is, is)
CM      ENDIF
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 2000
CM      IF (emsol_xa .EQ. 1) THEN
C?      endif
CM      ENDIF
      rq_reset = -1
CM      IF (emsol_xa .EQ. 1) THEN
C?      if (lp_iz_mode .eq. 1) then
CM      ENDIF
CM      IF (emsol_epc .EQ. 1) THEN
C?         call ems_dum_reset_rsmi(reset_loop, is, is)
CM      ELSE
         call ems_reset_rsmi(reset_loop, is, is)
CM      ENDIF
CM      IF (emsol_xa .EQ. 1) THEN
C?      else
CM      IF (emsol_epc .EQ. 1) THEN
C?         call ems_dum_nw_reset_rsmi(lp_iz_mode, reset_loop, is, is)
CM      ELSE
C?         call ems_nw_reset_rsmi(lp_iz_mode, reset_loop, is, is)
CM      ENDIF
C?      endif
CM      ENDIF
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 2000
      if (prob_st .eq. prob_st_mx_n_it) goto 1000
CM      IF (emsol_epc .EQ. 1) THEN
C?      call ems_dum_rsmi(is, is, lp_iz_mode)
CM      ELSE
      call ems_rsmi(is, is, lp_iz_mode)
CM      ENDIF
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 2000
 1000 continue
CM      IF (emsol_epc .EQ. 1) THEN
C?      call ems_dum_xit_rsmi(is, is)
CM      ELSE
      call ems_xit_rsmi(is, is)
CM      ENDIF
 2000 continue
c      call ems_mem_mgr_rp_mem_use(mem_mgr_rt_cod, is, ems_msg_wr_cn,
c     &     2, 'After RSMI')
c      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
c      call ems_rp_ml_da_st_msk
c      call ems_rp_ml_blk_st_msk
      sv_ml_ct_vr = 1
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tt_da .gt. 0) call ems_tt_rec(-sslv_tt, -1)
CM      ENDIF
 7000 continue
      is(ix_cu_ca_ems_rn_fg1) = 0
      is(ix_cu_ca_ems_rn_fg2) = 0
      rt_cod = ems_msg_cod
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)
     &     rn_nm(1:rn_nm_n_ch)
 
      call ems_msg_wr_li(serious_msg_n)
      go to 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      go to 7000
 8101 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810)
     &     rn_nm(1:rn_nm_n_ch),
     &     lp_alg_mode, 3,
     &     mn_lp_alg_mode, mx_lp_alg_mode
      call ems_msg_wr_li(7029)
      go to 7000
 8102 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810)
     &     rn_nm(1:rn_nm_n_ch),
     &     lp_iz_mode, 4,
     &     mn_lp_iz_mode, mx_lp_iz_mode
      call ems_msg_wr_li(7029)
      go to 7000
c 8800 continue
c      ems_msg_cod = max(mem_mgr_rt_cod, ems_msg_cod)
c      goto 7000
 8990 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9999)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 9000 format(31x, 'Entering EMSOL subroutine ', a)
 9100 format('No model has been loaded')
CM      IF (emsol_xa .EQ. 1) THEN
C? 9700 format('Calling ems_sslv with auxiliary block(s) of matrix',
C?     &     ' elements.',
C?     &     ' Results unpredictable.',
C?     &     ' Call ems_nwmt before ems_sslv to include these elements',
C?     &     ' in the model matrix.')
CM      ELSE
 9700 format('Calling ems_sslv with auxiliary block(s) of matrix',
     &     ' elements.',
     &     ' ems_sslv will call ems_nwmt to include these elements',
     &     ' in the model matrix.')
CM      ENDIF
 9801 format('ems_dsca must be called before calling ', a)
 9802 format('Not calling ', a, ' with current ds ')
 9810 format(a, ' has been called with value of ', i9,
     &     ' for parameter ', i1, '. The valid range is ', i9,
     &     ' to ', i9)
 9999 format(a, ' cannot be called from a user exit routine')
      end
 
C->>> ----------------------------------------------------> ems_cget <<<
c     Gets the first n_en_rq of the character control variables.
c
CM      IF (emsol_epc .EQ. 1) THEN
C?      subroutine ems_tru_cget(
CM      ELSE
      subroutine ems_cget(
CM      ENDIF
     &     rt_cod, is, ch_a, n_en_rq)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'CHCTVR.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer rt_cod, is(0:*), n_en_rq, ln
      character*8 ch_a(n_en_rq)
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_eq_i2
CM      ENDIF
      integer rn_nm_n_ch
      parameter (rn_nm_n_ch = 8)
      character*(rn_nm_n_ch) rn_nm
      save rn_nm
      data rn_nm/'ems_cget'/
 
      ems_msg_cod = 0
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(50)
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(is(ix_ca_init_fg1), is(ix_ca_init_fg2))) then
CM      ELSE
      if (is(ix_ca_init_fg1) .eq. is(ix_ca_init_fg2)) then
CM      ENDIF
         call ems_init(ems_msg_cod, is)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      endif
      ln = min(n_en_rq, n_ch_ct_vr)
      call ems_cp_ch8_a(ln, ch_ct_vr, ch_a, 1)
 7000 continue
      rt_cod = ems_msg_cod
      return
 9000 format(31x, 'Entering EMSOL subroutine ', a)
      end
 
C->>> ----------------------------------------------------> ems_cset <<<
c     Sets the first n_en_rq of the character control variables.
c
CM      IF (emsol_epc .EQ. 1) THEN
C?      subroutine ems_tru_cset(
CM      ELSE
      subroutine ems_cset(
CM      ENDIF
     &     rt_cod, is, ch_a, n_en_rq)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'CHCTVR.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer rt_cod, is(0:*), n_en_rq
      character*8 ch_a(n_en_rq)
      integer ix
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_eq_i2
CM      ENDIF
      integer rn_nm_n_ch
      parameter (rn_nm_n_ch = 8)
      character*(rn_nm_n_ch) rn_nm
      save rn_nm
      data rn_nm/'ems_cset'/
 
      ems_msg_cod = 0
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(50)
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(is(ix_ca_init_fg1), is(ix_ca_init_fg2))) then
CM      ELSE
      if (is(ix_ca_init_fg1) .eq. is(ix_ca_init_fg2)) then
CM      ENDIF
         call ems_init(ems_msg_cod, is)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      endif
      do 10, ix = 1, min(n_ch_ct_vr, n_en_rq)
         if (ch_a(ix) .ne. ch_ct_vr(ix)) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9200)
     &           ix, ch_ct_vr(ix), ch_a(ix)
            call ems_msg_wr_li(48)
            ch_ct_vr(ix) = ch_a(ix)
            sv_ml_ct_vr = 1
         end if
 10   continue
 7000 continue
      rt_cod = ems_msg_cod
      return
 9000 format(31x, 'Entering EMSOL subroutine ', a)
 9200 format('Character control variable ', i3,
     &     ' has been changed from ', a, ' to ', a)
      end
 
C->>> ----------------------------------------------------> ems_iget <<<
c     Gets the first n_en_rq of the integer control variables.
c
CM      IF (emsol_epc .EQ. 1) THEN
C?      subroutine ems_tru_iget(
CM      ELSE
      subroutine ems_iget(
CM      ENDIF
     &     rt_cod, is, i_a, n_en_rq)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer rt_cod, is(0:*), n_en_rq, i_a(n_en_rq), ln
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_eq_i2
CM      ENDIF
      integer rn_nm_n_ch
      parameter (rn_nm_n_ch = 8)
      character*(rn_nm_n_ch) rn_nm
      save rn_nm
      data rn_nm/'ems_iget'/
 
      ems_msg_cod = 0
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(50)
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(is(ix_ca_init_fg1), is(ix_ca_init_fg2))) then
CM      ELSE
      if (is(ix_ca_init_fg1) .eq. is(ix_ca_init_fg2)) then
CM      ENDIF
         call ems_init(ems_msg_cod, is)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      endif
      ln = min(n_en_rq, n_ems_i_ct_vr)
      call ems_cp_i_a(ln, i_ct_vr, i_a, 1)
 7000 continue
      rt_cod = ems_msg_cod
      return
 9000 format(31x, 'Entering EMSOL subroutine ', a)
      end
 
C->>> ----------------------------------------------------> ems_iset <<<
c     Sets the first n_en_rq of the integer control variables.
c
CM      IF (emsol_epc .EQ. 1) THEN
C?      subroutine ems_tru_iset(
CM      ELSE
      subroutine ems_iset(
CM      ENDIF
     &     rt_cod, is, i_a, n_en_rq)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer rt_cod, is(0:*), n_en_rq, i_a(n_en_rq)
      integer ix
      integer usr_ems_wr_cn, usr_mx_n_r, usr_mx_n_c, usr_mx_n_a_el
      integer usr_ems_er_cn
      logical cg
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_eq_i2
CM      ENDIF
      integer sv_dvx_mode
      integer sv_ml_nm_n_ch
      integer rn_nm_n_ch
      parameter (rn_nm_n_ch = 8)
      character*(rn_nm_n_ch) rn_nm
      save rn_nm
      save sv_ml_nm_n_ch
      data rn_nm/'ems_iset'/
      data sv_ml_nm_n_ch/0/
 
      ems_msg_cod = 0
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(50)
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(is(ix_ca_init_fg1), is(ix_ca_init_fg2))) then
CM      ELSE
      if (is(ix_ca_init_fg1) .eq. is(ix_ca_init_fg2)) then
CM      ENDIF
         call ems_init(ems_msg_cod, is)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      endif
      if (n_en_rq .ge. ix_ems_wr_cn) then
         usr_ems_wr_cn = i_a(ix_ems_wr_cn)
         if (
     &        usr_ems_wr_cn .eq. 0 .or.
     &        usr_ems_wr_cn .eq. 2 .or.
     &        usr_ems_wr_cn .eq. 5) then
c
c     The control variable ems_wr_cn cannot be set to these values
c
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9002)
            call ems_msg_wr_li(warn_msg_n)
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9120)
     &           i_ct_vr(ix_ems_wr_cn), usr_ems_wr_cn
            call ems_msg_wr_li(3032)
            i_a(ix_ems_wr_cn) = i_ct_vr(ix_ems_wr_cn)
         endif
      endif
      if (n_en_rq .ge. ix_mx_n_r) then
         usr_mx_n_r = i_a(ix_mx_n_r)
         if (usr_mx_n_r .ne. i_ct_vr(ix_mx_n_r) .and.
     &        i_ct_vr(ix_mx_n_r) .ne. i_ct_vr_df(ix_mx_n_r)) then
c
c     The control variable mx_n_r has already been changed once.
c
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &           ix_mx_n_r, ems_i_ct_vr_nm(ix_mx_n_r),
     &           i_ct_vr(ix_mx_n_r), i_ct_vr(ix_mx_n_r)
            call ems_msg_wr_li(3017)
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9120)
     &           i_ct_vr(ix_mx_n_r), usr_mx_n_r
            call ems_msg_wr_li(3032)
            i_a(ix_mx_n_r) = i_ct_vr(ix_mx_n_r)
         endif
      endif
      if (n_en_rq .ge. ix_mx_n_c) then
         usr_mx_n_c = i_a(ix_mx_n_c)
         if (usr_mx_n_c .ne. i_ct_vr(ix_mx_n_c) .and.
     &        i_ct_vr(ix_mx_n_c) .ne. i_ct_vr_df(ix_mx_n_c)) then
c
c     The control variable mx_n_c has already been changed once.
c
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &           ix_mx_n_c, ems_i_ct_vr_nm(ix_mx_n_c),
     &           i_ct_vr(ix_mx_n_c), i_ct_vr(ix_mx_n_c)
            call ems_msg_wr_li(3017)
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9120)
     &           i_ct_vr(ix_mx_n_c), usr_mx_n_c
            call ems_msg_wr_li(3032)
            i_a(ix_mx_n_c) = i_ct_vr(ix_mx_n_c)
         endif
      endif
      if (n_en_rq .ge. ix_mx_n_a_el) then
         usr_mx_n_a_el = i_a(ix_mx_n_a_el)
         if (usr_mx_n_a_el .ne. i_ct_vr(ix_mx_n_a_el) .and.
     &        i_ct_vr(ix_mx_n_a_el) .ne. i_ct_vr_df(ix_mx_n_a_el)) then
c
c     The control variable mx_n_a_el has already been changed once.
c
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &           ix_mx_n_a_el, ems_i_ct_vr_nm(ix_mx_n_a_el),
     &           i_ct_vr(ix_mx_n_a_el), i_ct_vr(ix_mx_n_a_el)
            call ems_msg_wr_li(3017)
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9120)
     &           i_ct_vr(ix_mx_n_a_el), usr_mx_n_a_el
            call ems_msg_wr_li(3032)
            i_a(ix_mx_n_a_el) = i_ct_vr(ix_mx_n_a_el)
         endif
      endif
      if (n_en_rq .ge. ix_ems_er_cn) then
         usr_ems_er_cn = i_a(ix_ems_er_cn)
         if (
     &        usr_ems_er_cn .eq. 2 .or.
     &        usr_ems_er_cn .eq. 5) then
c
c     The control variable ems_er_cn cannot be set to these values
c
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9002)
            call ems_msg_wr_li(warn_msg_n)
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9120)
     &           i_ct_vr(ix_ems_er_cn), usr_ems_er_cn
            call ems_msg_wr_li(3032)
            i_a(ix_ems_er_cn) = i_ct_vr(ix_ems_er_cn)
         endif
      endif
c
c     Take a copy of dvx_mode: If it changes then it will be necessary
c     to deduce whether any edge weights are correct.
c
      sv_dvx_mode = dvx_mode
c
c     Use cg rather than setting sv_ml_ct_vr (yet) because this would
c     make it look as if the user is changing sv_ml_ct_vr and this
c     variable is not settable!
c
      cg = .false.
      do 10, ix = 1, min(n_ems_i_ct_vr, n_en_rq)
         if (i_a(ix) .ne. i_ct_vr(ix)) then
            if (i_ct_vr_lg(ix)) then
c
c     The control variable is settable
c
               if (i_a(ix) .lt. i_ct_vr_lb(ix) .or.
     &              i_a(ix) .gt. i_ct_vr_ub(ix)) then
                  if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &                 ix, ems_i_ct_vr_nm(ix),
     &                 i_ct_vr_lb(ix), i_ct_vr_ub(ix)
                  call ems_msg_wr_li(3017)
                  if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9120)
     &                 i_ct_vr(ix), i_a(ix)
                  call ems_msg_wr_li(3032)
               else
                  if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9200)
     &                 ix, ems_i_ct_vr_nm(ix),
     &                 i_ct_vr(ix), i_a(ix)
                  call ems_msg_wr_li(48)
                  i_ct_vr(ix) = i_a(ix)
                  cg = .true.
               end if
            else
               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9300)ix,
     &              ems_i_ct_vr_nm(ix)
               call ems_msg_wr_li(3034)
            end if
         end if
 10   continue
c
c     If dvx_mode has changed then deduce whether any edge weights are
c     correct.
c
      if (dvx_mode .ne. sv_dvx_mode) then
         call ems_reset_pc_alg(dvx_mode, is)
      endif
      if (ml_nm_n_ch .ne. sv_ml_nm_n_ch) then
CM      IF (emsol_epc .EQ. 1) THEN
C?         call ems_dum_cg_ml_nm_n_ch(sv_ml_nm_n_ch, is, is)
CM      ELSE
         call ems_cg_ml_nm_n_ch(sv_ml_nm_n_ch, is, is)
CM      ENDIF
      endif
c
c     Record whether model control variables have changed.
c
      if (cg) sv_ml_ct_vr = 1
c
c     Update the message control variables from the model control
c     variables
c
      ems_msg_wr_cn = ems_wr_cn
      ems_msg_er_cn = ems_er_cn
      ems_msg_wr_li_ln = wr_li_ln
      ems_msg_n_pg_li = n_pg_li
c
c     Restore the values for mx_n_r/c/a_el which were passed and (maybe)
c     changed.
c
      if (n_en_rq .ge. ix_ems_wr_cn) i_a(ix_ems_wr_cn) = usr_ems_wr_cn
      if (n_en_rq .ge. ix_mx_n_r)    i_a(ix_mx_n_r) = usr_mx_n_r
      if (n_en_rq .ge. ix_mx_n_c)    i_a(ix_mx_n_c) = usr_mx_n_c
      if (n_en_rq .ge. ix_mx_n_a_el) i_a(ix_mx_n_a_el) = usr_mx_n_a_el
      if (n_en_rq .ge. ix_ml_nm_n_ch) ml_nm_n_rl = (ml_nm_n_ch+7)/8
 7000 continue
c      call ems_wr_i_ct_vr
      rt_cod = ems_msg_cod
      return
 9000 format(31x, 'Entering EMSOL subroutine ', a)
 9002 format('Integer control variable 2 cannot take values 0, 2 or 5')
 9100 format('The valid range for integer control variable ', i3,
     &     ' (', a32, ')',
     &     ' is ', i9, ' to ', i9)
 9120 format('A change from ', i9, ' to ', i9, ' will not take place')
 9200 format('Integer control variable ', i3,
     &     ' (', a32, ')',
     &     ' has been changed from ', i9, ' to ', i9)
 9300 format('Control variable ', i3,
     &     ' (', a32, ')',
     &     ' is a read-only variable')
      end
 
C->>> ----------------------------------------------------> ems_nget <<<
c     Gets the first n_en_rq of the index control variables.
c     Un-scale the vectors of the model.
c
CM      IF (emsol_epc .EQ. 1) THEN
C?      subroutine ems_tru_nget(
CM      ELSE
      subroutine ems_nget(
CM      ENDIF
     &     rt_cod, is, p_a, n_en_rq)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      include 'PCTVRIX.INC'
      integer rt_cod, is(0:*), n_en_rq, ln
      integer p_a(n_en_rq)
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_ne_i2, ems_i1_eq_i2
CM      ENDIF
CM      IF (emsol_xa .EQ. 1) THEN
C?      integer usr_n_pwl_c
C?      logical usr_pwl_vr
CM      ENDIF
      integer p_c_nm, p_r_nm
      integer rn_nm_n_ch
      parameter (rn_nm_n_ch = 8)
      character*(rn_nm_n_ch) rn_nm
      save rn_nm
      data rn_nm/'ems_nget'/
 
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_ne_i2(
C?     &     is(ix_cu_ca_ems_rn_fg1),
C?     &     is(ix_cu_ca_ems_rn_fg2)))
CM      ELSE
      if (is(ix_cu_ca_ems_rn_fg1) .ne. is(ix_cu_ca_ems_rn_fg2))
CM      ENDIF
     &     goto 8990
      is(ix_cu_ca_ems_rn_fg1) = 1
      is(ix_cu_ca_ems_rn_fg2) = 2
      ems_msg_cod = 0
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(50)
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(is(ix_ca_init_fg1), is(ix_ca_init_fg2))) then
CM      ELSE
      if (is(ix_ca_init_fg1) .eq. is(ix_ca_init_fg2)) then
CM      ENDIF
c
c     Switch off the `call EMSOL routine' flag during the call to
c     ems_init
c
         is(ix_cu_ca_ems_rn_fg1) = 0
         is(ix_cu_ca_ems_rn_fg2) = 0
         call ems_init(ems_msg_cod, is)
         is(ix_cu_ca_ems_rn_fg1) = 1
         is(ix_cu_ca_ems_rn_fg2) = 2
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      endif
      if (is(ix_ca_dsca) .eq. 0) go to 8010
      if (is(ix_is_n) .ne. cu_is_n) go to 8020
      ln = min(n_en_rq, n_p_ct_vr)
      if (ln .le. 0) goto 7000
      call ems_cp_i_a(ln, 0, p_a, 0)
      if (iand(ml_da_st_msk, ml_da_st_ld) .eq. 0) goto 7000
c
c     If the model solution is scaled then un-scale it
c
      if (iand(ml_da_st_msk, ml_da_st_scl_ml_sol) .ne. 0)
CM      IF (emsol_epc .EQ. 1) THEN
C?     &     call ems_dum_un_scl_ml_sol(is, is)
CM      ELSE
     &     call ems_un_scl_ml_sol(is, is)
CM      ENDIF
c
c     Negate any row duals if it has not already been done.
c
      if (r_du_act_sgn .gt. 0 .and.
     &     iand(ml_da_st_msk, ml_da_st_non_bc_du_act) .ne. 0)
CM      IF (emsol_epc .EQ. 1) THEN
C?     &     call ems_dum_ng_r_du_act(is)
CM      ELSE
     &     call ems_ng_r_du_act(is)
CM      ENDIF
c
c     Get the internal pointers into ds/is/ns and add 2 because ds/is/ns
c     are considered to start at zero internally and the value given
c     by ems_mem_mgr_g_p8/4/1 points to the zero'th entry of the array
c     and OSL points to the 1'st.
c
      if (n_en_rq .ge. ix_usr_p_r_lb)
     &     p_a(ix_usr_p_r_lb) = p_lbc + mx_n_c + 2
      if (n_en_rq .ge. ix_usr_p_r_pr_act)
     &     p_a(ix_usr_p_r_pr_act) = p_pr_act + mx_n_c + 2
      if (n_en_rq .ge. ix_usr_p_r_ub)
     &     p_a(ix_usr_p_r_ub) =  p_ubc + mx_n_c + 2
      if (n_en_rq .ge. ix_usr_p_r_du_act)
     &     p_a(ix_usr_p_r_du_act) =  p_du_act + mx_n_c + 2
      if (n_en_rq .ge. ix_usr_p_r_st)
     &     p_a(ix_usr_p_r_st) =  p_st + mx_n_c + 2
      if (n_en_rq .ge. ix_usr_p_c_lb)
     &     p_a(ix_usr_p_c_lb) =  p_lbc + 2
      if (n_en_rq .ge. ix_usr_p_c_pr_act)
     &     p_a(ix_usr_p_c_pr_act) =  p_pr_act + 2
      if (n_en_rq .ge. ix_usr_p_c_ub)
     &     p_a(ix_usr_p_c_ub) =  p_ubc + 2
      if (n_en_rq .ge. ix_usr_p_c_du_act)
     &     p_a(ix_usr_p_c_du_act) =  p_du_act + 2
      if (n_en_rq .ge. ix_usr_p_c_st)
     &     p_a(ix_usr_p_c_st) =  p_st + 2
      if (n_en_rq .ge. ix_usr_p_c_co)
     &     p_a(ix_usr_p_c_co) =  p_cbp + 2
      if (ml_nm_n_ch .eq. 8) then
         p_c_nm = p_nm
      else
         p_c_nm = p_lng_nm
      endif
      p_r_nm = p_c_nm + ml_nm_n_rl*mx_n_c
      if (n_en_rq .ge. ix_usr_p_r_nm)
     &     p_a(ix_usr_p_r_nm) = p_r_nm + ml_nm_n_rl + 1
      if (n_en_rq .ge. ix_usr_p_c_nm)
     &     p_a(ix_usr_p_c_nm) = p_c_nm + ml_nm_n_rl + 1
      if (n_en_rq .ge. ix_usr_p_r_scl)
     &     p_a(ix_usr_p_r_scl) =  p_scl + mx_n_c + 2
      if (n_en_rq .ge. ix_usr_p_c_scl)
     &     p_a(ix_usr_p_c_scl) =  p_scl + 2
c
c     ?? What pointers should be returned if no user copy has been made?
c
      if (n_en_rq .ge. ix_usr_p_usr_mtx_r_ix)
     &     p_a(ix_usr_p_usr_mtx_r_ix) = p_usr_mtx_r_ix + 2
      if (n_en_rq .ge. ix_usr_p_usr_mtx_c_ix)
     &     p_a(ix_usr_p_usr_mtx_c_ix) = p_usr_mtx_c_ix + 2
      if (n_en_rq .ge. ix_usr_p_usr_mtx_r_v)
     &     p_a(ix_usr_p_usr_mtx_r_v) = p_usr_mtx_r_v + 2
      if (n_en_rq .ge. ix_usr_p_usr_mtx_c_v)
     &     p_a(ix_usr_p_usr_mtx_c_v) = p_usr_mtx_c_v + 2
      if (n_en_rq .ge. ix_usr_p_usr_mtx_r_sa)
     &     p_a(ix_usr_p_usr_mtx_r_sa) = p_usr_mtx_r_sa + 2
      if (n_en_rq .ge. ix_usr_p_usr_mtx_c_sa)
     &     p_a(ix_usr_p_usr_mtx_c_sa) = p_usr_mtx_c_sa + 2
 
      if (prob_st .eq. prob_st_ifs) then
         if (n_en_rq .ge. ix_usr_p_r_aux_sol)
     &        p_a(ix_usr_p_r_aux_sol) = p_du_act + mx_n_c + 2
         if (n_en_rq .ge. ix_usr_p_c_aux_sol)
     &        p_a(ix_usr_p_c_aux_sol) = p_du_act + 2
      else if (prob_st .eq. prob_st_unbd) then
         if (n_en_rq .ge. ix_usr_p_r_aux_sol)
     &        p_a(ix_usr_p_r_aux_sol) = p_r_aux_sol + 2
         if (n_en_rq .ge. ix_usr_p_c_aux_sol)
     &        p_a(ix_usr_p_c_aux_sol) = p_c_aux_sol + 2
      end if
      if (iand(ml_da_st_msk, ml_da_st_rg_da) .ne. 0) then
         if (n_en_rq .ge. ix_usr_p_c_co_rg_up_co_v)
     &        p_a(ix_usr_p_c_co_rg_up_co_v) = p_co_rg_up_co_v + 2
         if (n_en_rq .ge. ix_usr_p_c_co_rg_lo_co_v)
     &        p_a(ix_usr_p_c_co_rg_lo_co_v) = p_co_rg_lo_co_v + 2
         if (n_en_rq .ge. ix_usr_p_c_co_rg_up_ob_v)
     &        p_a(ix_usr_p_c_co_rg_up_ob_v) = p_co_rg_up_ob_v + 2
         if (n_en_rq .ge. ix_usr_p_c_co_rg_lo_ob_v)
     &        p_a(ix_usr_p_c_co_rg_lo_ob_v) = p_co_rg_lo_ob_v + 2
         if (n_en_rq .ge. ix_usr_p_c_co_rg_up_en_vr)
     &        p_a(ix_usr_p_c_co_rg_up_en_vr) = p_co_rg_up_en_vr + 2
         if (n_en_rq .ge. ix_usr_p_c_co_rg_lo_en_vr)
     &        p_a(ix_usr_p_c_co_rg_lo_en_vr) = p_co_rg_lo_en_vr + 2
         if (n_en_rq .ge. ix_usr_p_c_co_rg_up_lv_vr)
     &        p_a(ix_usr_p_c_co_rg_up_lv_vr) = p_co_rg_up_lv_vr + 2
         if (n_en_rq .ge. ix_usr_p_c_co_rg_lo_lv_vr)
     &        p_a(ix_usr_p_c_co_rg_lo_lv_vr) = p_co_rg_lo_lv_vr + 2
         if (n_en_rq .ge. ix_usr_p_c_bd_rg_up_bd_v)
     &        p_a(ix_usr_p_c_bd_rg_up_bd_v) = p_bd_rg_up_bd_v + 2
         if (n_en_rq .ge. ix_usr_p_c_bd_rg_lo_bd_v)
     &        p_a(ix_usr_p_c_bd_rg_lo_bd_v) = p_bd_rg_lo_bd_v + 2
         if (n_en_rq .ge. ix_usr_p_c_bd_rg_up_ob_v)
     &        p_a(ix_usr_p_c_bd_rg_up_ob_v) = p_bd_rg_up_ob_v + 2
         if (n_en_rq .ge. ix_usr_p_c_bd_rg_lo_ob_v)
     &        p_a(ix_usr_p_c_bd_rg_lo_ob_v) = p_bd_rg_lo_ob_v + 2
         if (n_en_rq .ge. ix_usr_p_c_bd_rg_up_en_vr)
     &        p_a(ix_usr_p_c_bd_rg_up_en_vr) = p_bd_rg_up_en_vr + 2
         if (n_en_rq .ge. ix_usr_p_c_bd_rg_lo_en_vr)
     &        p_a(ix_usr_p_c_bd_rg_lo_en_vr) = p_bd_rg_lo_en_vr + 2
         if (n_en_rq .ge. ix_usr_p_c_bd_rg_up_lv_vr)
     &        p_a(ix_usr_p_c_bd_rg_up_lv_vr) = p_bd_rg_up_lv_vr + 2
         if (n_en_rq .ge. ix_usr_p_c_bd_rg_lo_lv_vr)
     &        p_a(ix_usr_p_c_bd_rg_lo_lv_vr) = p_bd_rg_lo_lv_vr + 2
         if (n_en_rq .ge. ix_usr_p_r_bd_rg_up_bd_v)
     &        p_a(ix_usr_p_r_bd_rg_up_bd_v) =
     &        p_bd_rg_up_bd_v + mx_n_c + 2
         if (n_en_rq .ge. ix_usr_p_r_bd_rg_lo_bd_v)
     &        p_a(ix_usr_p_r_bd_rg_lo_bd_v) =
     &        p_bd_rg_lo_bd_v + mx_n_c + 2
         if (n_en_rq .ge. ix_usr_p_r_bd_rg_up_ob_v)
     &        p_a(ix_usr_p_r_bd_rg_up_ob_v) =
     &        p_bd_rg_up_ob_v + mx_n_c + 2
         if (n_en_rq .ge. ix_usr_p_r_bd_rg_lo_ob_v)
     &        p_a(ix_usr_p_r_bd_rg_lo_ob_v) =
     &        p_bd_rg_lo_ob_v + mx_n_c + 2
         if (n_en_rq .ge. ix_usr_p_r_bd_rg_up_en_vr)
     &        p_a(ix_usr_p_r_bd_rg_up_en_vr) =
     &        p_bd_rg_up_en_vr + mx_n_c + 2
         if (n_en_rq .ge. ix_usr_p_r_bd_rg_lo_en_vr)
     &        p_a(ix_usr_p_r_bd_rg_lo_en_vr) =
     &        p_bd_rg_lo_en_vr + mx_n_c + 2
         if (n_en_rq .ge. ix_usr_p_r_bd_rg_up_lv_vr)
     &        p_a(ix_usr_p_r_bd_rg_up_lv_vr) =
     &        p_bd_rg_up_lv_vr + mx_n_c + 2
         if (n_en_rq .ge. ix_usr_p_r_bd_rg_lo_lv_vr)
     &        p_a(ix_usr_p_r_bd_rg_lo_lv_vr) =
     &        p_bd_rg_lo_lv_vr + mx_n_c + 2
         if (n_en_rq .ge. ix_usr_p_c_co_rg_up_act_v)
     &        p_a(ix_usr_p_c_co_rg_up_act_v) = p_co_rg_up_act_v + 2
         if (n_en_rq .ge. ix_usr_p_c_co_rg_lo_act_v)
     &        p_a(ix_usr_p_c_co_rg_lo_act_v) = p_co_rg_lo_act_v + 2
      endif
CM      IF (emsol_xa .EQ. 1) THEN
C?      if (n_en_rq .ge. ix_usr_p_pwl_r_usr_cu_sn) then
C?         usr_pwl_vr = iand(ml_da_st_msk, ml_da_st_usr_pwl_vr) .ne. 0
C?         if (usr_pwl_vr) then
C?            if (is(p_pwl_vr_usr_cu_sn) .eq. 0) then
C?c
C?c     The entries in pwl_vr_usr_cu_sn have not been calculated.
C?c
CM      IF (emsol_epc .EQ. 1) THEN
C?               call ems_dum_ca_g_pwl_vr_usr_cu_sn(
CM      ELSE
C?               call ems_ca_g_pwl_vr_usr_cu_sn(
CM      ENDIF
C?     &              is, is)
C?            endif
C?            usr_n_pwl_c = is(p_pwl_vr_usr_da_sa)
C?            p_a(ix_usr_p_pwl_r_usr_cu_sn) =
C?     &           p_pwl_vr_usr_cu_sn + usr_n_pwl_c + 2
C?         endif
C?         if (n_en_rq .ge. ix_usr_p_pwl_c_usr_cu_sn) then
C?            if (usr_pwl_vr) p_a(ix_usr_p_pwl_c_usr_cu_sn) =
C?     &           p_pwl_vr_usr_cu_sn + 2
C?         endif
C?      endif
CM      ENDIF
c
c     Indicate that the following are not correct for the model:
c
c     vr_in_c, INVERT, condition of the basis, status/primal activities,
c     basic primal activities, edge weights and row-wise representation
c     of matrix columns being priced.
c
      ml_da_st_msk = ml_da_st_msk
     &     - iand(ml_da_st_msk, ml_da_st_vr_in_c)
     &     - iand(ml_da_st_msk, ml_da_st_inv)
     &     - iand(ml_da_st_msk, ml_da_st_bs_cond_ok)
     &     - iand(ml_da_st_msk, ml_da_st_vr_st_fm_act)
     &     - iand(ml_da_st_msk, ml_da_st_bc_pr_act)
     &     - iand(ml_da_st_msk, ml_da_st_ed_wt)
     &     - iand(ml_da_st_msk, ml_da_st_r_mtx)
 7000 continue
      is(ix_cu_ca_ems_rn_fg1) = 0
      is(ix_cu_ca_ems_rn_fg2) = 0
      rt_cod = ems_msg_cod
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)
     &     rn_nm(1:rn_nm_n_ch)
 
      call ems_msg_wr_li(serious_msg_n)
      go to 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      go to 7000
 8990 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9999)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 9000 format(31x, 'Entering EMSOL subroutine ', a)
 9801 format('ems_dsca must be called before calling ', a)
 9802 format('Not calling ', a, ' with current ds ')
 9999 format(a, ' cannot be called from a user exit routine')
      end
 
C->>> ----------------------------------------------------> ems_rget <<<
c     Gets the first n_en_rq of the real control variables.
c
CM      IF (emsol_epc .EQ. 1) THEN
C?      subroutine ems_tru_rget(
CM      ELSE
      subroutine ems_rget(
CM      ENDIF
     &     rt_cod, is, rl_a, n_en_rq)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      integer rt_cod, is(0:*), n_en_rq, ln
      double precision rl_a(n_en_rq)
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_eq_i2
CM      ENDIF
      integer rn_nm_n_ch
      parameter (rn_nm_n_ch = 8)
      character*(rn_nm_n_ch) rn_nm
      save rn_nm
      data rn_nm/'ems_rget'/
 
      ems_msg_cod = 0
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(50)
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(is(ix_ca_init_fg1), is(ix_ca_init_fg2))) then
CM      ELSE
      if (is(ix_ca_init_fg1) .eq. is(ix_ca_init_fg2)) then
CM      ENDIF
         call ems_init(ems_msg_cod, is)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      endif
      ln = min(n_en_rq, n_ems_rl_ct_vr)
      call ems_cp_rl_a(ln, rl_ct_vr, rl_a, 1)
 7000 continue
      rt_cod = ems_msg_cod
      return
 9000 format(31x, 'Entering EMSOL subroutine ', a)
      end
 
C->>> ----------------------------------------------------> ems_rset <<<
c     Sets the first n_en_rq of the real control variables.
c
CM      IF (emsol_epc .EQ. 1) THEN
C?      subroutine ems_tru_rset(
CM      ELSE
      subroutine ems_rset(
CM      ENDIF
     &     rt_cod, is, rl_a, n_en_rq)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      integer rt_cod, is(0:*), n_en_rq
      double precision rl_a(n_en_rq)
      double precision sv_usr_tl_pr_ifs
      double precision sv_usr_tl_du_ifs
      integer ix
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_eq_i2
CM      ENDIF
      integer rn_nm_n_ch
      parameter (rn_nm_n_ch = 8)
      character*(rn_nm_n_ch) rn_nm
      save rn_nm
      data rn_nm/'ems_rset'/
 
      ems_msg_cod = 0
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     rn_nm(1:rn_nm_n_ch)
      call ems_msg_wr_li(50)
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(is(ix_ca_init_fg1), is(ix_ca_init_fg2))) then
CM      ELSE
      if (is(ix_ca_init_fg1) .eq. is(ix_ca_init_fg2)) then
CM      ENDIF
         call ems_init(ems_msg_cod, is)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      endif
c
c     Take copies of usr_tl_pr_ifs and usr_tl_du_ifs. If either changes,
c     then tl_pr_ifs (and hence the feasibility of primal activities)
c     or   tl_du_ifs (and hence the optimality of the solution) may
c     change.
c
      sv_usr_tl_pr_ifs = usr_tl_pr_ifs
      sv_usr_tl_du_ifs = usr_tl_du_ifs
      do 10, ix = 1, min(n_ems_rl_ct_vr, n_en_rq)
         if (rl_a(ix) .ne. rl_ct_vr(ix)) then
            if (rl_ct_vr_lg(ix)) then
c
c     The control variable is settable
c
               if (rl_a(ix) .lt. rl_ct_vr_lb(ix) .or.
     &              rl_a(ix) .gt. rl_ct_vr_ub(ix)) then
                  if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &                 ix, ems_rl_ct_vr_nm(ix),
     &                 rl_ct_vr_lb(ix),
     &                 rl_ct_vr_ub(ix)
                  call ems_msg_wr_li(3018)
                  if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9120)
     &                 rl_ct_vr(ix), rl_a(ix)
                  call ems_msg_wr_li(3033)
               else
                  if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9200)
     &                 ix, ems_rl_ct_vr_nm(ix),
     &                 rl_ct_vr(ix), rl_a(ix)
                  call ems_msg_wr_li(49)
                  rl_ct_vr(ix) = rl_a(ix)
                  sv_ml_ct_vr = 1
               end if
            else
               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9300)ix,
     &              ems_rl_ct_vr_nm(ix)
               call ems_msg_wr_li(3034)
            end if
         end if
 10   continue
      if (usr_tl_pr_ifs .ne. sv_usr_tl_pr_ifs) then
c
c     If the user's primal feasibility tolerance has changed then
c     propagate the change internally.
c
CM      IF (emsol_epc .EQ. 1) THEN
C?         call ems_dum_reset_tl_pr_ifs(is, is)
CM      ELSE
      call ems_reset_tl_pr_ifs(is, is)
CM      ENDIF
      endif
      if (usr_tl_du_ifs .ne. sv_usr_tl_du_ifs) then
c
c     If the user's dual feasibility tolerance has changed then
c     propagate the change internally.
c
CM      IF (emsol_epc .EQ. 1) THEN
C?         call ems_dum_reset_tl_du_ifs(is, is)
CM      ELSE
      call ems_reset_tl_du_ifs(is, is)
CM      ENDIF
      endif
 7000 continue
c      call ems_wr_rl_ct_vr
      rt_cod = ems_msg_cod
      return
 9000 format(31x, 'Entering EMSOL subroutine ', a)
 9100 format('The valid range for real control variable ', i3,
     &     ' (', a32, ')',
     &     ' is ', g17.10, ' to ', g17.10)
 9120 format('A change from ', g17.10, ' to ', g17.10,
     &     ' will not take place')
 9200 format('Real control variable ', i3,
     &     ' (', a32, ')',
     &     ' has been changed from ', g17.10, ' to ', g17.10)
 9300 format('Control variable ', i3,
     &     ' (', a32, ')',
     &     ' is a read-only variable')
      end
