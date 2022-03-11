C->>> ---------------------------------------------> ems_sv_bs_cg_iz <<<
c     Initialise the basis change records.
c
      subroutine ems_sv_bs_cg_iz
      implicit none
      include 'SVBSCG.INC'
 
      ca_sv_bs_cg_iz_fg1 = 1
      ca_sv_bs_cg_iz_fg2 = 2
      bs_cg_rec = .false.
      return
      end
 
C->>> ---------------------------------------------> ems_sv_bs_cg_sa <<<
c     (Re-)start saving basis change records.
c
      subroutine ems_sv_bs_cg_sa(ems_rt_cod)
      implicit none
      include 'EMSRTCOD.INC'
      include 'EMSMSG.INC'
      include 'SVBSCG.INC'
      integer ems_rt_cod
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_eq_i2
CM      ENDIF
c
c     Make sure that ems_sv_bs_cg_iz has been called. This protects the
c     test of bs_cg_rec which may be unassigned.
c
      ems_rt_cod = ems_rt_cod_ok
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(ca_sv_bs_cg_iz_fg1,
C?     &     ca_sv_bs_cg_iz_fg2)) goto 8000
CM      ELSE
      if (ca_sv_bs_cg_iz_fg1 .eq. ca_sv_bs_cg_iz_fg2) goto 8000
CM      ENDIF
      if (.not. bs_cg_rec) then
c
c     Initialise the basis change records if basis change records are
c     not currently being saved.
c
         n_sv_bs_cg = 0
         n_sv_bs_cg_seq = 1
         sv_bs_cg_seq_sa(1) = 1
         bs_cg_rec = .true.
      endif
 7100 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      ems_rt_cod = max(ems_rt_cod_int_er, ems_rt_cod)
      goto 7100
 9800 format('Must call ems_sv_bs_cg_iz before',
     &     ' starting to save basis change records')
      end
 
C->>> -------------------------------------------> ems_sv_bs_cg_stop <<<
c     Stop saving basis change records
c
      subroutine ems_sv_bs_cg_stop
      implicit none
      include 'SVBSCG.INC'
 
      bs_cg_rec = .false.
      return
      end
 
C->>> ------------------------------------------------> ems_sv_bs_cg <<<
c     Save a basis change record---if records are being saved.
c
      subroutine ems_sv_bs_cg(ems_rt_cod,
     &     en_vr, lv_vr, en_vr_prev_st, lv_vr_prev_st)
      implicit none
      include 'EMSRTCOD.INC'
      include 'EMSMSG.INC'
      include 'SVBSCG.INC'
      integer ems_rt_cod
      integer en_vr, lv_vr, en_vr_prev_st, lv_vr_prev_st
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_eq_i2
CM      ENDIF
      integer prev_n_sv_bs_cg_seq
      integer ca_ems_rt_cod
c
c     Make sure that ems_sv_bs_cg_iz has been called. This protects the
c     test of bs_cg_rec which may be unassigned.
c
      ems_rt_cod = ems_rt_cod_ok
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(ca_sv_bs_cg_iz_fg1,
C?     &     ca_sv_bs_cg_iz_fg2)) goto 8000
CM      ELSE
      if (ca_sv_bs_cg_iz_fg1 .eq. ca_sv_bs_cg_iz_fg2) goto 8000
CM      ENDIF
      if (.not. bs_cg_rec) goto 7000
c
c     Save the basis change record---may jump back to here if a
c     sequence of records has been removed to create space.
c
 100   continue
      if (n_sv_bs_cg .lt. mx_n_sv_bs_cg) then
c
c     The basis change record can be saved within the current sequence
c
         n_sv_bs_cg = n_sv_bs_cg + 1
         sv_bs_cg_en_vr(n_sv_bs_cg) = en_vr
         sv_bs_cg_lv_vr(n_sv_bs_cg) = lv_vr
         sv_bs_cg_en_vr_prev_st(n_sv_bs_cg) = en_vr_prev_st
         if (en_vr .ne. lv_vr) then
            sv_bs_cg_lv_vr_prev_st(n_sv_bs_cg) = lv_vr_prev_st
         else
            sv_bs_cg_lv_vr_prev_st(n_sv_bs_cg) = en_vr_prev_st
         endif
         goto 7000
      endif
c
c     Try to create room to save the basis change record within the
c     current sequence by removing the oldest sequence.
c
c     NB:
c     1. This may have to be done several times if there are empty
c     .  sequences
c     2. This may be (immediately) unsuccessful if only one sequence
c     .  uses all the available entries---this will eventually occur
c     .  when the number of basis changes between resets exceeds
c     .  mx_n_sv_bs_cg.
c
 200  continue
      if (n_sv_bs_cg_seq .gt. 1) then
c
c     Keep a copy of the number of sequences before removing one so that
c     the (barely conceivable) infinite loop can avoided visibly in this
c     routine.
c
         prev_n_sv_bs_cg_seq = n_sv_bs_cg_seq
         call ems_sv_bs_cg_rm_ol_seq(ca_ems_rt_cod)
         if (ca_ems_rt_cod .ne. ems_rt_cod_ok) then
            ems_rt_cod = max(ca_ems_rt_cod, ems_rt_cod)
            if (ems_rt_cod .ge. ems_rt_lvl_serious) goto 7100
         endif
c
c     Go back and save the basis change record if space is available.
c
         if (n_sv_bs_cg .lt. mx_n_sv_bs_cg) goto 100
c
c     The sequence just removed must have been null so remove the next
c
c     Check that the number of sequences has been reduced
c
         if (n_sv_bs_cg_seq .lt. prev_n_sv_bs_cg_seq) goto 200
      endif
c
c     Stop recording basis change records
c
      call ems_sv_bs_cg_stop
 7000 continue
 7100 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      ems_rt_cod = max(ems_rt_cod_int_er, ems_rt_cod)
      goto 7100
 9800 format('Must call ems_sv_bs_cg_iz before',
     &     ' saving basis change records')
      end
 
C->>> --------------------------------------> ems_sv_bs_cg_rm_ol_seq <<<
c     Remove the oldest sequence (if possible).
c
      subroutine ems_sv_bs_cg_rm_ol_seq(ems_rt_cod)
      implicit none
      include 'EMSRTCOD.INC'
      include 'EMSMSG.INC'
      include 'SVBSCG.INC'
      integer ems_rt_cod
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_eq_i2
CM      ENDIF
      integer fm_bs_cg_n, t_bs_cg_n, bs_cg_n, seq_n, os
c
c     Make sure that ems_sv_bs_cg_iz has been called. This protects the
c     test of bs_cg_rec which may be unassigned.
c
      ems_rt_cod = ems_rt_cod_ok
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(ca_sv_bs_cg_iz_fg1,
C?     &     ca_sv_bs_cg_iz_fg2)) goto 8000
CM      ELSE
      if (ca_sv_bs_cg_iz_fg1 .eq. ca_sv_bs_cg_iz_fg2) goto 8000
CM      ENDIF
      if (.not. bs_cg_rec) goto 7000
c      print*, '### BEFORE REMOVING BASIS CHANGE RECORD SEQUENCE ###'
c      print*, ' n_sv_bs_cg_seq = ', n_sv_bs_cg_seq
c      print*, ' n_sv_bs_cg = ', n_sv_bs_cg
      if (n_sv_bs_cg_seq .gt. 1) then
c         call ems_sv_bs_cg_rp(ems_rt_cod)
         fm_bs_cg_n = sv_bs_cg_seq_sa(2)
         t_bs_cg_n = n_sv_bs_cg
         os = fm_bs_cg_n - 1
c
c     NB os = 0 is inconceivable---not that it matters!
c
         do 10, bs_cg_n = fm_bs_cg_n, t_bs_cg_n
            sv_bs_cg_en_vr(bs_cg_n-os) = sv_bs_cg_en_vr(bs_cg_n)
            sv_bs_cg_lv_vr(bs_cg_n-os) = sv_bs_cg_lv_vr(bs_cg_n)
            sv_bs_cg_en_vr_prev_st(bs_cg_n-os) =
     &           sv_bs_cg_en_vr_prev_st(bs_cg_n)
            sv_bs_cg_lv_vr_prev_st(bs_cg_n-os) =
     &           sv_bs_cg_lv_vr_prev_st(bs_cg_n)
 10      continue
         n_sv_bs_cg_seq = n_sv_bs_cg_seq - 1
         do 20, seq_n = 1, n_sv_bs_cg_seq
            sv_bs_cg_seq_sa(seq_n) = sv_bs_cg_seq_sa(seq_n+1) - os
 20      continue
         n_sv_bs_cg = n_sv_bs_cg - os
c         print*, '### AFTER REMOVING BASIS CHANGE RECORD SEQUENCE ###'
c         print*, ' n_sv_bs_cg_seq = ', n_sv_bs_cg_seq
c         print*, ' n_sv_bs_cg = ', n_sv_bs_cg
c         call ems_sv_bs_cg_rp(ems_rt_cod)
      endif
c      print*, '### REMOVED BASIS CHANGE RECORD SEQUENCE ###'
c      print*, ' n_sv_bs_cg_seq = ', n_sv_bs_cg_seq
c      print*, ' n_sv_bs_cg = ', n_sv_bs_cg
 7000 continue
 7100 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      ems_rt_cod = max(ems_rt_cod_int_er, ems_rt_cod)
      goto 7100
 9800 format('Must call ems_sv_bs_cg_iz before',
     &     ' removing saved basis change sequence')
      end
C->>> ---------------------------------------------> ems_sv_bs_cg_rp <<<
c     Report the saved basis change records
c
      subroutine ems_sv_bs_cg_rp(ems_rt_cod)
      implicit none
      include 'EMSMSG.INC'
      include 'SVBSCG.INC'
      include 'EMSRTCOD.INC'
      integer ems_rt_cod
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_eq_i2
CM      ENDIF
      integer fm_bs_cg_n, t_bs_cg_n, bs_cg_n, seq_n
      logical alw_f7_wr
c
c     Make sure that ems_sv_bs_cg_iz has been called. This protects the
c     test of bs_cg_rec which may be unassigned.
c
      ems_rt_cod = ems_rt_cod_ok
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(ca_sv_bs_cg_iz_fg1,
C?     &     ca_sv_bs_cg_iz_fg2)) goto 8000
CM      ELSE
      if (ca_sv_bs_cg_iz_fg1 .eq. ca_sv_bs_cg_iz_fg2) goto 8000
CM      ENDIF
      alw_f7_wr = ems_msg_no_prt_fm .ge. 1
      if (.not. bs_cg_rec) then
         if (alw_f7_wr) write(ems_li, 9000, err = 8990)
         call ems_msg_wr_li(info_msg_n)
         goto 7000
      endif
      t_bs_cg_n = 0
      sv_bs_cg_seq_sa(n_sv_bs_cg_seq+1) = n_sv_bs_cg + 1
      do 20, seq_n = 1, n_sv_bs_cg_seq
         if (alw_f7_wr) write(ems_li, 9100, err = 8990)seq_n
         call ems_msg_wr_li(info_msg_n)
         fm_bs_cg_n = t_bs_cg_n + 1
         t_bs_cg_n = sv_bs_cg_seq_sa(seq_n+1)-1
         if (fm_bs_cg_n .le. t_bs_cg_n) then
            if (alw_f7_wr) write(ems_li, 9110, err = 8990)
            call ems_msg_wr_li(info_msg_n)
            do 10, bs_cg_n = fm_bs_cg_n, t_bs_cg_n
               if (alw_f7_wr) write(ems_li, 9120, err = 8990)bs_cg_n,
     &              sv_bs_cg_en_vr(bs_cg_n),
     &              sv_bs_cg_en_vr_prev_st(bs_cg_n),
     &              sv_bs_cg_lv_vr(bs_cg_n),
     &              sv_bs_cg_lv_vr_prev_st(bs_cg_n)
               call ems_msg_wr_li(info_msg_n)
 10         continue
         endif
 20   continue
 7000 continue
 7100 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      ems_rt_cod = max(ems_rt_cod_int_er, ems_rt_cod)
      goto 7100
 8990 continue
      ems_rt_cod = max(ems_rt_cod_serious_f7_wr_er,
     &     ems_rt_cod)
      goto 7100
 9000 format('Not saving basis change records')
 9100 format('Saved basis change record sequence ', i2)
 9110 format('BsCgN ',
     &     '  Entering (     Vr, PrevSt)',
     &     '   Leaving (     Vr, PrevSt)')
 9120 format(i5, 2(13x, i7, 5x, i3))
 9800 format('Must call ems_sv_bs_cg_iz before',
     &     ' reporting saved basis change records')
      end
 
C->>> ----------------------------------------------> ems_g_bs_cg_st <<<
c     Gets the basis change status for a variable
c
      subroutine ems_g_bs_cg_st(ems_rt_cod, ps_rp_lvl, vr_n,
     &     st, vr_in_c, bs_cg_st)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMSG.INC'
      include 'RSMICS.INC'
      include 'RLCTVR.INC'
      include 'EMSRTCOD.INC'
      include 'SVBSCG.INC'
      integer ems_rt_cod, ps_rp_lvl, vr_n, st, bs_cg_st
      integer vr_in_c(-vr_in_c_n_sn:*)
      integer ems_g_vr_in_c_sn_n_o_c
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_eq_i2
CM      ENDIF
      character*30 ems_st_t_ch30
      character*30 ch_st
      integer rp_lvl, c_n, vr_in_c_sn_n, vr_in_c_sn_ty
      integer lc_c_n, vr_in_c_l_p
 
      rp_lvl = ps_rp_lvl
      ems_rt_cod = ems_rt_cod_ok
c
c     Make sure that ems_sv_bs_cg_iz has been called. This protects the
c     test of bs_cg_rec which may be unassigned.
c
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(ca_sv_bs_cg_iz_fg1,
C?     &     ca_sv_bs_cg_iz_fg2)) goto 8000
CM      ELSE
      if (ca_sv_bs_cg_iz_fg1 .eq. ca_sv_bs_cg_iz_fg2) goto 8000
CM      ENDIF
      if (.not. bs_cg_rec) then
c
c     If basis change records are not being saved then don't try to get
c     one. This mechanism is used to avoid addressing vr_in_c which is
c     not set up if basis changes occur due to singularity in the
c     initial reset.
c
         bs_cg_st = bs_cg_st_null
         vr_in_c_sn_ty = 0
         goto 7000
      endif
      if (iand(st, bc_bt) .ne. 0) then
         bs_cg_st = bs_cg_st_bc
         vr_in_c_sn_ty = 0
         goto 7000
      endif
c
c     Variable is nonbasic.
c
      vr_in_c_l_p = vr_in_c(os_vr_in_c_l_p)
c
c     Extract the pointer from the status. Normally this will be the
c     pointer into vr_in_c but, before checking this, have to trim the
c     pointer to the (valid) array bounds of vr_in_c.
c
      c_n = iand(st, mx_mx_ml_a_dim)
      lc_c_n = min(max(c_n, 1), vr_in_c_l_p)
      if (vr_in_c(lc_c_n) .ne. vr_n) then
c
c     c_n is not the pointer into vr_in_c so search for the pointer.
c
         do 10, c_n = 1, vr_in_c_l_p
            if (vr_in_c(c_n) .eq. vr_n) goto 20
 10      continue
         goto 8010
 20      continue
      endif
      vr_in_c_sn_n = ems_g_vr_in_c_sn_n_o_c(c_n, vr_in_c)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 8950
      if (vr_in_c_sn_n .le. vr_in_c_n_sn_ty) then
         vr_in_c_sn_ty = vr_in_c_sn_n
      else
         vr_in_c_sn_ty = vr_in_c_sn_n - vr_in_c_n_sn_ty
      endif
      if (vr_in_c_sn_ty .eq. vr_in_c_sn_ty_ab_bp) then
         bs_cg_st = bs_cg_st_bp_vr_ab_bp
      else if (vr_in_c_sn_ty .eq. vr_in_c_sn_ty_bw_bp) then
         bs_cg_st = bs_cg_st_bp_vr_bw_bp
      else if (vr_in_c_sn_ty .eq. vr_in_c_sn_ty_bw_lb) then
         bs_cg_st = bs_cg_st_vr_bw_lb
      else if (vr_in_c_sn_ty .eq. vr_in_c_sn_ty_ab_ub) then
         bs_cg_st = bs_cg_st_vr_ab_ub
      else if (vr_in_c_sn_ty .eq. vr_in_c_sn_ty_btw) then
         bs_cg_st = bs_cg_st_vr_btw_bd
      else if (vr_in_c_sn_ty .eq. vr_in_c_sn_ty_at_lb) then
         bs_cg_st = bs_cg_st_vr_at_lb
      else if (vr_in_c_sn_ty .eq. vr_in_c_sn_ty_at_ub) then
         bs_cg_st = bs_cg_st_vr_at_ub
      else if (vr_in_c_sn_ty .eq. vr_in_c_sn_ty_te_fx) then
         bs_cg_st = bs_cg_st_fx_vr_at_bd
      else if (vr_in_c_sn_ty .eq. vr_in_c_sn_ty_fx) then
         bs_cg_st = bs_cg_st_fx_vr_at_bd
      else
         goto 8020
      endif
 7000 continue
CM      IF (emsol_dev .EQ. 1) THEN
C?      if (rp_lvl .ne. 0) then
C?         ch_st = ems_st_t_ch30(st)
C?         write(*, 9000, err=8990)vr_n, vr_in_c_sn_ty, ch_st, bs_cg_st
C?      endif
CM      ENDIF
 7100 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      ems_rt_cod = max(ems_rt_cod_int_er, ems_rt_cod)
      goto 7100
 8010 continue
      ch_st = ems_st_t_ch30(st)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801, err=8990)
     &     vr_n, ch_st
      call ems_msg_wr_li(serious_msg_n)
      ems_rt_cod = max(ems_rt_lvl_serious,
     &     ems_rt_cod)
      goto 7100
 8020 continue
      ch_st = ems_st_t_ch30(st)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802, err=8990)
     &     vr_in_c_sn_ty, vr_n, ch_st
      call ems_msg_wr_li(serious_msg_n)
      ems_rt_cod = max(ems_rt_lvl_serious,
     &     ems_rt_cod)
      goto 7100
 8950 continue
      ems_rt_cod = max(ems_rt_lvl_serious,
     &     ems_rt_cod)
      goto 7100
 8990 continue
      ems_rt_cod = max(ems_rt_cod_serious_f7_wr_er,
     &     ems_rt_cod)
      goto 7100
CM      IF (emsol_dev .EQ. 1) THEN
C? 9000 format('vr = ', i7, ' vr_in_c_sn_ty ', i2, ' st: ', a30,
C?     &     ' BsCgSt = ', i3)
CM      ENDIF
 9800 format('Must call ems_sv_bs_cg_iz before',
     &     ' getting a basis change status')
 9801 format('Cannot find variable vr = ', i7,
     &     ' in vr_in_c, st: ', a30)
 9802 format('Cannot match vr_in_c_sn_ty = ', i7,
     &     ' for vr = ', i7, ' st: ', a30)
      end
 
C->>> ---------------------------------------------> ems_sv_bs_cg_ck <<<
c     Check the saved basis change records.
c
      subroutine ems_sv_bs_cg_ck(ems_rt_cod,
     &     prev_reset_bs_cg_st, prev_bs_cg_st)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSRTCOD.INC'
      include 'EMSMSG.INC'
      include 'SVBSCG.INC'
      include 'ICTVR.INC'
      integer ems_rt_cod
      integer prev_reset_bs_cg_st(0:mx_n_c+n_r)
      integer prev_bs_cg_st(0:mx_n_c+n_r)
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_eq_i2
CM      ENDIF
      integer bs_cg_n, bs_cg_st, en_vr, lv_vr
      integer t_bs_cg_n
      integer en_vr_prev_st, lv_vr_prev_st
c
c     Make sure that ems_sv_bs_cg_iz has been called. This protects the
c     test of bs_cg_rec which may be unassigned.
c
      ems_rt_cod = ems_rt_cod_ok
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(ca_sv_bs_cg_iz_fg1,
C?     &     ca_sv_bs_cg_iz_fg2)) goto 8000
CM      ELSE
      if (ca_sv_bs_cg_iz_fg1 .eq. ca_sv_bs_cg_iz_fg2) goto 8000
CM      ENDIF
      if (.not. bs_cg_rec) goto 7000
      t_bs_cg_n = sv_bs_cg_seq_sa(n_sv_bs_cg_seq)
c
c     Pass backwards though this sequence of basis change records,
c     updating the basis change status of all variables before the
c     basis change took place.
c
      do 10, bs_cg_n = n_sv_bs_cg, t_bs_cg_n, -1
         en_vr = sv_bs_cg_en_vr(bs_cg_n)
         lv_vr = sv_bs_cg_lv_vr(bs_cg_n)
         en_vr_prev_st = sv_bs_cg_en_vr_prev_st(bs_cg_n)
         lv_vr_prev_st = sv_bs_cg_lv_vr_prev_st(bs_cg_n)
         prev_bs_cg_st(en_vr) = en_vr_prev_st
         if (en_vr .eq. lv_vr) goto 10
         prev_bs_cg_st(lv_vr) = lv_vr_prev_st
 10   continue
      do 20, bs_cg_n = n_sv_bs_cg, t_bs_cg_n, -1
         en_vr = sv_bs_cg_en_vr(bs_cg_n)
         lv_vr = sv_bs_cg_lv_vr(bs_cg_n)
         bs_cg_st = prev_bs_cg_st(en_vr)
         if (bs_cg_st .ne. prev_reset_bs_cg_st(en_vr)) then
            ems_rt_cod = max(ems_rt_cod_int_er, ems_rt_cod)
CM      IF (emsol_dev .EQ. 1) THEN
C?            write(*, 9999)bs_cg_n, 'Entering', en_vr,
C?     &           bs_cg_st, prev_reset_bs_cg_st(en_vr)
CM      ENDIF
         endif
         bs_cg_st = prev_bs_cg_st(lv_vr)
         if (bs_cg_st .ne. prev_reset_bs_cg_st(lv_vr)) then
            ems_rt_cod = max(ems_rt_cod_int_er, ems_rt_cod)
CM      IF (emsol_dev .EQ. 1) THEN
C?            write(*, 9999)bs_cg_n, 'Leaving ', lv_vr,
C?     &           bs_cg_st, prev_reset_bs_cg_st(lv_vr)
CM      ENDIF
         endif
 20   continue
 7000 continue
 7100 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      ems_rt_cod = max(ems_rt_cod_int_er, ems_rt_cod)
      goto 7100
 9800 format('Must call ems_sv_bs_cg_iz before',
     &     ' checking the saved basis change records')
CM      IF (emsol_dev .EQ. 1) THEN
C? 9999 format('Basis change ', i5, ': ', a, ' variable ', i7,
C?     &     ': prev_bs_cg_st = ', i3,
C?     &     ' ***BUT*** prev_reset_bs_cg_st = ', i3)
CM      ENDIF
      end
 
C->>> -------------------------------------------> ems_ck_reset_loop <<<
c     Check to see whether the saved basis change records indicate that
c     the same basis is being reset.
c
      subroutine ems_ck_reset_loop(ems_rt_cod, ds, is,
     &     prev_reset_bs_cg_st, prev_cu_bs_cg_st, reset_loop)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'SVBSCG.INC'
      include 'RSMICS.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      include 'EMSRTCOD.INC'
      integer ems_rt_cod
      logical reset_loop
      integer prev_reset_bs_cg_st(0:mx_n_c+n_r)
      integer prev_cu_bs_cg_st(0:mx_n_c+n_r)
      integer is(0:*)
      double precision ds(0:*)
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_eq_i2
CM      ENDIF
      integer bs_cg_n, cu_bs_cg_st, prev_bs_cg_st, seq_n, en_vr, lv_vr
      integer fm_bs_cg_n, t_bs_cg_n
      integer en_vr_prev_st, lv_vr_prev_st
      integer ix_n, vr_n, bs_cg_st
      integer ca_ems_rt_cod
      logical er_fd
c
c     Make sure that ems_sv_bs_cg_iz has been called. This protects the
c     test of bs_cg_rec which may be unassigned.
c
      ems_rt_cod = ems_rt_cod_ok
      reset_loop = .false.
      er_fd = .false.
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(ca_sv_bs_cg_iz_fg1,
C?     &     ca_sv_bs_cg_iz_fg2)) goto 8000
CM      ELSE
      if (ca_sv_bs_cg_iz_fg1 .eq. ca_sv_bs_cg_iz_fg2) goto 8000
CM      ENDIF
      if (.not. bs_cg_rec) goto 7000
      if (n_sv_bs_cg .eq. 0) goto 2000
c
c     Get the current basis change status for variables which have been
c     involved in basis changes.
c
      do 5, vr_n = 1, mx_n_c+n_r
         prev_cu_bs_cg_st(vr_n) = 0
 5    continue
      do 10, bs_cg_n = n_sv_bs_cg, 1, -1
         en_vr = sv_bs_cg_en_vr(bs_cg_n)
c
c     Get the current basis change status for the entering variable
c     (unless it is already known)
c
         if (prev_cu_bs_cg_st(en_vr) .eq. 0) then
            if (iand(is(p_st+en_vr), bc_bt) .ne. 0) then
c
c     Likely to be basic...
c
               prev_cu_bs_cg_st(en_vr) = bs_cg_st_bc
            else
c
c     ... but nonbasic if this was a bound swap.
c
c               call ems_og_g_bs_cg_st(ca_ems_rt_cod, 0, en_vr,
c     &              is(p_st+en_vr),
c     &              ds(p_pr_act+en_vr),
c     &              ds(p_rsmi_lb+en_vr),
c     &              ds(p_rsmi_ub+en_vr),
c     &              bs_cg_st)
               call ems_g_bs_cg_st(ca_ems_rt_cod, 0, en_vr,
     &              is(p_st+en_vr),
     &              is(p_vr_in_c),
     &              bs_cg_st)
               if (ca_ems_rt_cod .ne. ems_rt_cod_ok) then
                  ems_rt_cod = max(ca_ems_rt_cod, ems_rt_cod)
                  if (ems_rt_cod .ge. ems_rt_lvl_serious) goto 7100
               endif
               prev_cu_bs_cg_st(en_vr) = bs_cg_st
            endif
         endif
c
c     Get the current basis change status for the leaving variable
c     (unless it is already known)
c
         lv_vr = sv_bs_cg_lv_vr(bs_cg_n)
         if (prev_cu_bs_cg_st(lv_vr) .eq. 0) then
            if (iand(is(p_st+lv_vr), bc_bt) .ne. 0) then
c
c     Shouldn't be basic now since this is the last time this variable
c     left the basis.
c
               goto 8010
            else
c
c     Should be nonbasic
c
c               call ems_og_g_bs_cg_st(ca_ems_rt_cod, 0, lv_vr,
c     &              is(p_st+lv_vr),
c     &              ds(p_pr_act+lv_vr),
c     &              ds(p_rsmi_lb+lv_vr),
c     &              ds(p_rsmi_ub+lv_vr),
c     &              bs_cg_st)
               call ems_g_bs_cg_st(ca_ems_rt_cod, 0, lv_vr,
     &              is(p_st+lv_vr),
     &              is(p_vr_in_c),
     &              bs_cg_st)
               if (ca_ems_rt_cod .ne. ems_rt_cod_ok) then
                  ems_rt_cod = max(ca_ems_rt_cod, ems_rt_cod)
                  if (ems_rt_cod .ge. ems_rt_lvl_serious) goto 7100
               endif
               prev_cu_bs_cg_st(lv_vr) = bs_cg_st
            endif
         endif
 10   continue
c
c     Pass backwards through the sequences of saved basis change records
c     to see whether the current basis has already been reset.
c
      seq_n = n_sv_bs_cg_seq
      fm_bs_cg_n = n_sv_bs_cg
 100  continue
      t_bs_cg_n = sv_bs_cg_seq_sa(seq_n)
c
c     Pass backwards though this sequence of basis change records,
c     updating the basis change status of all variables before the
c     basis change took place.
c
      do 120, bs_cg_n = fm_bs_cg_n, t_bs_cg_n, -1
         en_vr = sv_bs_cg_en_vr(bs_cg_n)
         lv_vr = sv_bs_cg_lv_vr(bs_cg_n)
         en_vr_prev_st = sv_bs_cg_en_vr_prev_st(bs_cg_n)
         lv_vr_prev_st = sv_bs_cg_lv_vr_prev_st(bs_cg_n)
         bs_cg_st = prev_cu_bs_cg_st(en_vr)/prev_bs_cg_st_fac
         prev_cu_bs_cg_st(en_vr) = prev_cu_bs_cg_st(en_vr) +
     &        (en_vr_prev_st - bs_cg_st)*prev_bs_cg_st_fac
         if (en_vr .eq. lv_vr) goto 120
         bs_cg_st = prev_cu_bs_cg_st(lv_vr)/prev_bs_cg_st_fac
         prev_cu_bs_cg_st(lv_vr) = prev_cu_bs_cg_st(lv_vr) +
     &        (lv_vr_prev_st - bs_cg_st)*prev_bs_cg_st_fac
 120  continue
      if (seq_n .eq. n_sv_bs_cg_seq) then
c
c     Check the basis change status deduced by passing backwards though
c     the last sequence of basis change records with the value recorded
c     when the previous reset occurred.
c
         do 130, bs_cg_n = n_sv_bs_cg, t_bs_cg_n, -1
            en_vr = sv_bs_cg_en_vr(bs_cg_n)
            lv_vr = sv_bs_cg_lv_vr(bs_cg_n)
            prev_bs_cg_st = prev_cu_bs_cg_st(en_vr)/prev_bs_cg_st_fac
            cu_bs_cg_st = prev_cu_bs_cg_st(en_vr) -
     &           prev_bs_cg_st*prev_bs_cg_st_fac
            if (prev_bs_cg_st .ne. prev_reset_bs_cg_st(en_vr)) then
               er_fd = .true.
CM      IF (emsol_dev .EQ. 1) THEN
C?               write(*, 9999)bs_cg_n, 'Entering', en_vr,
C?     &              prev_bs_cg_st, prev_reset_bs_cg_st(en_vr)
CM      ENDIF
            endif
            prev_bs_cg_st = prev_cu_bs_cg_st(lv_vr)/prev_bs_cg_st_fac
            cu_bs_cg_st = prev_cu_bs_cg_st(lv_vr) -
     &           prev_bs_cg_st*prev_bs_cg_st_fac
            if (prev_bs_cg_st .ne. prev_reset_bs_cg_st(lv_vr)) then
               er_fd = .true.
CM      IF (emsol_dev .EQ. 1) THEN
C?               write(*, 9999)bs_cg_n, 'Leaving ', lv_vr,
C?     &              prev_bs_cg_st, prev_reset_bs_cg_st(lv_vr)
CM      ENDIF
            endif
 130     continue
      endif
c
c     If all the basis change statuses are the same then a reset loop
c     has occurred.
c
      reset_loop = .false.
      do 140, bs_cg_n = n_sv_bs_cg, t_bs_cg_n, -1
         en_vr = sv_bs_cg_en_vr(bs_cg_n)
         lv_vr = sv_bs_cg_lv_vr(bs_cg_n)
         prev_bs_cg_st = prev_cu_bs_cg_st(en_vr)/prev_bs_cg_st_fac
         cu_bs_cg_st = prev_cu_bs_cg_st(en_vr) -
     &        prev_bs_cg_st*prev_bs_cg_st_fac
         if (prev_bs_cg_st .ne. cu_bs_cg_st) goto 150
         prev_bs_cg_st = prev_cu_bs_cg_st(lv_vr)/prev_bs_cg_st_fac
         cu_bs_cg_st = prev_cu_bs_cg_st(lv_vr) -
     &        prev_bs_cg_st*prev_bs_cg_st_fac
         if (prev_bs_cg_st .ne. cu_bs_cg_st) goto 150
 140  continue
      reset_loop = .true.
      goto 1000
 150  continue
c
c     Set up for the previous sequence (if there is one).
c
      seq_n = seq_n - 1
      if (seq_n .le. 0) goto 1000
      fm_bs_cg_n = t_bs_cg_n - 1
      goto 100
 1000 continue
c
c     Remove the oldest sequence if the sequence limit has been reached,
c     otherwise incerease the sequence counter by 1.
c
      if (n_sv_bs_cg_seq .eq. mx_n_sv_bs_cg_seq) then
         call ems_sv_bs_cg_rm_ol_seq(ca_ems_rt_cod)
         if (ca_ems_rt_cod .ne. ems_rt_cod_ok) then
            ems_rt_cod = max(ca_ems_rt_cod, ems_rt_cod)
            if (ems_rt_cod .ge. ems_rt_lvl_serious) goto 7100
         endif
      endif
      n_sv_bs_cg_seq = n_sv_bs_cg_seq + 1
      sv_bs_cg_seq_sa(n_sv_bs_cg_seq) = n_sv_bs_cg + 1
 2000 continue
c
c     Save the basis change status at this reset for all variables.
c     This is just for checking.
c
      do 2010, ix_n = 1, n_c+n_r
         if (ix_n .le. n_c) then
            vr_n = ix_n
         else
            vr_n = ix_n + (mx_n_c-n_c)
         endif
         if (iand(is(p_st+vr_n), bc_bt) .ne. 0) then
            prev_reset_bs_cg_st(vr_n) = bs_cg_st_bc
         else
c            call ems_og_g_bs_cg_st(ca_ems_rt_cod, 0, vr_n,
c     &           is(p_st+vr_n),
c     &           ds(p_pr_act+vr_n),
c     &           ds(p_rsmi_lb+vr_n),
c     &           ds(p_rsmi_ub+vr_n),
c     &           cu_bs_cg_st)
            call ems_g_bs_cg_st(ca_ems_rt_cod, 0, vr_n,
     &           is(p_st+vr_n),
     &           is(p_vr_in_c),
     &           cu_bs_cg_st)
            if (ca_ems_rt_cod .ne. ems_rt_cod_ok) then
               ems_rt_cod = max(ca_ems_rt_cod, ems_rt_cod)
               if (ems_rt_cod .ge. ems_rt_lvl_serious) goto 7100
            endif
            prev_reset_bs_cg_st(vr_n) = cu_bs_cg_st
         endif
 2010 continue
      if (er_fd) ems_rt_cod = max(ems_rt_lvl_serious, ems_rt_cod)
 7000 continue
 7100 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      ems_rt_cod = max(ems_rt_cod_int_er, ems_rt_cod)
      goto 7100
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 98011)lv_vr
      call ems_msg_wr_li(bug_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 98012)en_vr,
     &     prev_cu_bs_cg_st(en_vr)
      call ems_msg_wr_li(bug_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 98013)bs_cg_n
      call ems_msg_wr_li(bug_msg_n)
      goto 7100
 9800 format('Must call ems_sv_bs_cg_iz before',
     &     ' checking for a reset loop')
CM      IF (emsol_dev .EQ. 1) THEN
C? 9999 format('Basis change ', i5, ': ', a, ' variable ', i7,
C?     &     ': prev_bs_cg_st = ', i3,
C?     &     ' ***BUT*** prev_reset_bs_cg_st = ', i3)
CM      ENDIF
98011 format('Leaving  variable ', i7, ' should not be basic')
98012 format('Entering variable ', i7, ' St = ', i9)
98013 format('bs_cg_n = ', i7)
      end
C=======================================================================
c     Redundant code ---->
C=======================================================================
C->>> -------------------------------------------> ems_og_g_bs_cg_st <<<
c     Gets the basis change status for a variable
c
      subroutine ems_og_g_bs_cg_st(ems_rt_cod, rp_lvl, vr_n,
     &     st, pr_act, lb, ub, bs_cg_st)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICS.INC'
      include 'RLCTVR.INC'
      include 'EMSRTCOD.INC'
      integer ems_rt_cod, rp_lvl, vr_n, st, bs_cg_st
      double precision pr_act, lb, ub
      double precision rsdu, bp
      character*30 ems_st_t_ch30
      character*30 ch_st
 
      ems_rt_cod = ems_rt_cod_ok
      if (iand(st, alt_bt) .eq. 0) then
         if (iand(st, lb_bt) .ne. 0) then
            rsdu = lb - pr_act
            if (rsdu .gt. tl_pr_ifs) then
               bs_cg_st = bs_cg_st_vr_bw_lb
            else if (rsdu .ge. -tl_pr_ifs) then
               bs_cg_st = bs_cg_st_vr_at_lb
            else
c
c     Variable is above its lower bound
c
               if (iand(st, ub_bt) .ne. 0) then
                  rsdu = pr_act - ub
                  if (rsdu .gt. tl_pr_ifs) then
                     bs_cg_st = bs_cg_st_vr_ab_ub
                  else if (rsdu .ge. -tl_pr_ifs) then
                     bs_cg_st = bs_cg_st_vr_at_ub
                  else
                     bs_cg_st = bs_cg_st_vr_btw_bd
                  endif
               else
                  bs_cg_st = bs_cg_st_vr_ab_lb
               endif
            endif
         else if (iand(st, ub_bt) .ne. 0) then
            rsdu = pr_act - ub
            if (rsdu .gt. tl_pr_ifs) then
               bs_cg_st = bs_cg_st_vr_ab_ub
            else if (rsdu .ge. -tl_pr_ifs) then
               bs_cg_st = bs_cg_st_vr_at_ub
            else
               bs_cg_st = bs_cg_st_vr_bw_ub
            endif
         else
            bs_cg_st = bs_cg_st_fr_vr
         endif
      else if (iand(st, bp_bt) .ne. 0) then
         if (iand(st, ub_bt) .ne. 0) then
            bp = ub
         else
            bp = lb
         endif
         rsdu = pr_act - bp
         if (rsdu .gt. tl_pr_ifs) then
            bs_cg_st = bs_cg_st_bp_vr_ab_bp
         else if (rsdu .ge. -tl_pr_ifs) then
            bs_cg_st = bs_cg_st_bp_vr_at_bp
         else
            bs_cg_st = bs_cg_st_bp_vr_bw_bp
         endif
      else
         goto 8000
      endif
      if (rp_lvl .ne. 0) then
         ch_st = ems_st_t_ch30(st)
CM      IF (emsol_dev .EQ. 1) THEN
C?         write(*, 9000, err=8990)vr_n, lb, pr_act, ub, ch_st, bs_cg_st
CM      ENDIF
      endif
c 7000 continue
 7100 continue
      return
 8000 continue
      ems_rt_cod = max(ems_rt_lvl_serious,
     &     ems_rt_cod)
      goto 7100
CM      IF (emsol_dev .EQ. 1) THEN
C? 8990 continue
C?      ems_rt_cod = max(ems_rt_cod_serious_f7_wr_er,
C?     &     ems_rt_cod)
C?      goto 7100
CM      ENDIF
CM      IF (emsol_dev .EQ. 1) THEN
C? 9000 format('vr = ', i7, ' lb:act:ub ', 3(2x, g11.4), ' st: ', a30,
C?     &     ' BsCgSt = ', i3)
CM      ENDIF
      end
 
