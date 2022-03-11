CM
C=======================================================================
c     EMSOL Memory manager:
c
c     Call sequences which may be cause block moves (BlkMv) or the space
c     limit to be reached (NoPo)
c
c     css_mem_mgr -<- g_fr_blk_p  -<-  nw_blk -<- ope_blk -<- ope_a
c     . (BlkMv)         (NoPo)
c     .                   |              |
c     .                   ^              V
c     .                   |              |
c     .                xp_blk    -<- g_fr_blk_n
c     .                (BlkMv)
c
c     Only ope_blk and ope_a are called from external subroutines.
c     ope_blk calls ems_mem_mgr_g_al_p if blocks are moved.
c
C=======================================================================
c     User-called subroutines.
c
C->>> ---------------------------------------> ems_mem_mgr_dn_blk_id <<<
c     Define the text for a new block ID
c
c     Possible return code values set in this routine:
c     -----------------------------------------------
c
c     Routines called:
c     ---------------
c
      subroutine ems_mem_mgr_dn_blk_id(mem_mgr_rt_cod, is,
     &     txt, usr_blk_id)
      implicit none
      include 'EMSMMGR.INC'
      include 'EMSMMGRI.INC'
      include 'EMSMSG.INC'
      integer mem_mgr_rt_cod, is(0:*), usr_blk_id
      character*(*) txt
      logical ems_mem_mgr_no_ca_iz
 
      mem_mgr_rt_cod = mem_mgr_rt_cod_ok
      if (ems_mem_mgr_no_ca_iz(is)) goto 8000
      if (n_blk_id+1 .ge. mx_n_blk_id) then
         mem_mgr_rt_cod = max(mem_mgr_rt_cod_mx_blk_id,
     &        mem_mgr_rt_cod)
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9400, err=8990)
     &        mx_n_blk_id-usr_blk_id_os, blk_id_txt(mx_n_blk_id)
         call ems_msg_wr_li(warn_msg_n)
         usr_blk_id = mx_n_blk_id - usr_blk_id_os
      else
         n_blk_id = n_blk_id + 1
         usr_blk_id = n_blk_id - usr_blk_id_os
         blk_id_txt(n_blk_id) = txt
      endif
c 7000 continue
 7100 continue
      return
 8000 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_no_ca_iz,
     &     mem_mgr_rt_cod)
      goto 7100
 8990 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_serious_f7_wr_er,
     &     mem_mgr_rt_cod)
      goto 7100
 9400 format('Reached the limit on the number of block IDs.',
     &     ' The text associated with block ID ', i3, 'is', a)
      end
 
C->>> -----------------------------------------> ems_mem_mgr_g_hdl_p <<<
c     Calculates the pointers associated with a set of handles
c
c     Possible return code values set in this routine:
c     -----------------------------------------------
c
c     Routines called:
c     ---------------
c
      subroutine ems_mem_mgr_g_hdl_p(mem_mgr_rt_cod, is,
     &     n_hdl, hdl)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSMSG.INC'
      integer mem_mgr_rt_cod, is(0:*)
      integer n_hdl
      integer hdl(hdl_z*n_hdl)
      logical ems_mem_mgr_no_ca_iz
      integer p
      integer ca_mem_mgr_rt_cod
      character*19 rn_nm
      data rn_nm/'ems_mem_mgr_g_hdl_p'/
 
      mem_mgr_rt_cod = mem_mgr_rt_cod_ok
      if (ems_mem_mgr_no_ca_iz(is)) goto 8000
      if (n_hdl .lt. 0) goto 8030
      do 10, p = 1, (n_hdl-1)*hdl_z+1, hdl_z
         if (hdl(p+hdl_os_blk_n) .le. 0) then
            hdl(p+hdl_os_p) = no_p
            goto 10
         endif
         if (hdl(p+hdl_os_wo_z) .eq. ch_wo_z) then
            call ems_mem_mgr_g_p1(ca_mem_mgr_rt_cod, is,
     &           hdl(p+hdl_os_blk_n), hdl(p+hdl_os_p))
         else if (hdl(p+hdl_os_wo_z) .eq. i_wo_z) then
            call ems_mem_mgr_g_p4(ca_mem_mgr_rt_cod, is,
     &           hdl(p+hdl_os_blk_n), hdl(p+hdl_os_p))
         else if (hdl(p+hdl_os_wo_z) .eq. rl_wo_z) then
            call ems_mem_mgr_g_p8(ca_mem_mgr_rt_cod, is,
     &           hdl(p+hdl_os_blk_n), hdl(p+hdl_os_p))
         endif
         if (ca_mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
            mem_mgr_rt_cod = max(ca_mem_mgr_rt_cod, mem_mgr_rt_cod)
            if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 7100
         endif
         hdl(p+hdl_os_blk_mv_k) = is(ix_blk_mv_k)
 10   continue
c 7000 continue
 7100 continue
      return
 8000 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_no_ca_iz,
     &     mem_mgr_rt_cod)
      goto 7100
 8030 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_pm_er,
     &     mem_mgr_rt_cod)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800, err=8990)
     &     rn_nm, n_hdl, 3, 0, i_inf
      call ems_msg_wr_li(serious_msg_n)
      goto 7100
 8990 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_serious_f7_wr_er,
     &     mem_mgr_rt_cod)
      goto 7100
 9800 format(a, ' has been called with value of ', i9,
     &     ' for parameter ', i1, '. The valid range is ', i9,
     &     ' to ', i9)
      end
 
C->>> ------------------------------------> ems_mem_mgr_g_rq_is_n_en <<<
c     Returns the current required number of entries in the integer
c     workspace which would allow a block of n_wo integer words to be
c     allocated, and the maximum number of entries which has been
c     required since the memory manager was initialised.
c
c     Possible return code values set in this routine:
c     -----------------------------------------------
c
c     Routines called:
c     ---------------
c
      subroutine ems_mem_mgr_g_rq_is_n_en(mem_mgr_rt_cod, is,
     &     n_wo, cu_rq_is_n_en, mx_rq_is_n_en)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSMMGRI.INC'
      integer mem_mgr_rt_cod, is(0:*)
      integer n_wo, cu_rq_is_n_en, mx_rq_is_n_en
      logical ems_mem_mgr_no_ca_iz
 
      mem_mgr_rt_cod = mem_mgr_rt_cod_ok
      if (ems_mem_mgr_no_ca_iz(is)) goto 8000
      if (is(ix_n_xa_i_wo_rq) .le. 0) then
         cu_rq_is_n_en = is(ix_l_mgr_p)+1 + n_wo - is(ix_n_fr_wo)
         mx_rq_is_n_en = is(ix_l_mgr_p)+1 + n_wo - is(ix_mn_n_fr_wo)
      else
         cu_rq_is_n_en = is(ix_l_mgr_p)+1 + n_wo + is(ix_n_xa_i_wo_rq)
         mx_rq_is_n_en = cu_rq_is_n_en
      endif
c 7000 continue
 7100 continue
      return
 8000 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_no_ca_iz,
     &     mem_mgr_rt_cod)
      goto 7100
      end
 
C->>> ----------------------------------------------> ems_mem_mgr_iz <<<
c     User interface to the routine which initialises the memory
c     manager. Avoids the user having to set mn_blk_p
c
c     Possible return code values set in this routine:
c     -----------------------------------------------
c
c     Routines called:
c     ---------------
c
      subroutine ems_mem_mgr_iz(mem_mgr_rt_cod, is, is_n_en)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSMMGRI.INC'
      include 'EMSMSG.INC'
      integer mem_mgr_rt_cod, is_n_en
      integer is(0:is_n_en-1)
      integer mn_blk_p
      integer ca_mem_mgr_rt_cod
      character*14 rn_nm
      data rn_nm/'ems_mem_mgr_iz'/
 
      mem_mgr_rt_cod = mem_mgr_rt_cod_ok
      if (is_n_en .lt. 1) goto 8030
      mn_blk_p = mem_mgr_da_l_is_en + 1
      call ems_mem_mgr_int_iz(ca_mem_mgr_rt_cod, is, is_n_en, mn_blk_p)
      if (ca_mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
         mem_mgr_rt_cod = max(ca_mem_mgr_rt_cod, mem_mgr_rt_cod)
         if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 7100
      endif
 7100 continue
      return
 8030 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_pm_er,
     &     mem_mgr_rt_cod)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800, err=8990)
     &     rn_nm, is_n_en, 3, 1, i_inf
      call ems_msg_wr_li(serious_msg_n)
      goto 7100
 8990 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_serious_f7_wr_er,
     &     mem_mgr_rt_cod)
      goto 7100
 9800 format(a, ' has been called with value of ', i9,
     &     ' for parameter ', i1, '. The valid range is ', i9,
     &     ' to ', i9)
      end
 
C->>> ------------------------------------------> ems_mem_mgr_nw_hdl <<<
c     Sets a handle in block blk_n for n_en entries of word length wo_z.
c     Serious error reported if handle asks for more space than is
c     available in block.
c
c     Possible return code values set in this routine:
c     -----------------------------------------------
c
c     Routines called:
c     ---------------
c
      subroutine ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, n_en, wo_z, hdl)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSMMGRI.INC'
      include 'EMSMSG.INC'
      integer mem_mgr_rt_cod, is(0:*), blk_n, n_en, wo_z
      integer hdl(0:hdl_z_m1)
      logical ems_mem_mgr_no_ca_iz
      integer ems_mem_mgr_nx_i_n_en_p
      integer blk_f_fr_p, blk_p, blk_n_wo, sn_p, n_wo
      integer blk_n_fr_wo, blk_n_fr_en
      integer p_blk_da
      integer ca_mem_mgr_rt_cod
      character*18 rn_nm
      data rn_nm/'ems_mem_mgr_nw_hdl'/
 
      mem_mgr_rt_cod = mem_mgr_rt_cod_ok
      if (ems_mem_mgr_no_ca_iz(is)) goto 8000
      if (blk_n .lt. 1 .or. blk_n .gt. is(ix_mx_n_blk)) goto 8030
      if (wo_z .lt. 1) goto 8050
      p_blk_da = is(ix_p_blk_da)+(blk_n-1)*blk_rec_z
      blk_p =      is(p_blk_da+os_blk_is_p)
      if (blk_p .le. 0) goto 8110
      blk_f_fr_p = is(p_blk_da+os_blk_f_fr_p)
      blk_n_wo =   is(p_blk_da+os_blk_n_wo)
      n_wo = n_en*wo_z
      sn_p = blk_p + blk_f_fr_p
c
c     Ensure that the block starts on an integral wo_z boundary.
c
      sn_p = ems_mem_mgr_nx_i_n_en_p(sn_p, wo_z)
      blk_n_fr_wo = blk_p + blk_n_wo - sn_p
      blk_n_fr_en = blk_n_fr_wo/wo_z
      if (n_en .lt. 0) goto 8040
      if (n_en .gt. blk_n_fr_en) goto 8100
      if (sn_p+n_wo .gt. blk_p+blk_n_wo) goto 8120
      is(p_blk_da+os_blk_f_fr_p) = sn_p - blk_p + n_wo
      hdl(hdl_os_blk_n) = blk_n
      hdl(hdl_os_blk_os) = sn_p - blk_p
      hdl(hdl_os_wo_z) = wo_z
CM      IF (emsol_epc .EQ. 1) THEN
C?      if (wo_z .eq. i_wo_z) then
C?         call ems_debug_se_i_a_undn(is(sn_p), 1, n_en)
C?      else
C?         call ems_dum_debug_se_rl_a_undn(is(sn_p), 1, n_en)
C?      endif
CM      ENDIF
c
c     Set the pointer in the handle
c
c 1000 continue
      call ems_mem_mgr_g_hdl_p(ca_mem_mgr_rt_cod, is,
     &     1, hdl)
      if (ca_mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
         mem_mgr_rt_cod = max(ca_mem_mgr_rt_cod, mem_mgr_rt_cod)
         if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 7100
      endif
c 7000 continue
 7100 continue
      return
 8000 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_no_ca_iz,
     &     mem_mgr_rt_cod)
      goto 7100
 8030 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_pm_er,
     &     mem_mgr_rt_cod)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800, err=8990)
     &     rn_nm, blk_n, 3, 1, is(ix_mx_n_blk)
      call ems_msg_wr_li(serious_msg_n)
      goto 7100
 8040 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_pm_er,
     &     mem_mgr_rt_cod)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800, err=8990)
     &     rn_nm, n_en, 4, 0, blk_n_fr_en
      call ems_msg_wr_li(serious_msg_n)
      goto 7100
 8050 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_pm_er,
     &     mem_mgr_rt_cod)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800, err=8990)
     &     rn_nm, wo_z, 5, 1, i_inf
      call ems_msg_wr_li(serious_msg_n)
      goto 7100
 8100 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_blk_no_po,
     &     mem_mgr_rt_cod)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810, err=8990)
     &     n_en, wo_z, blk_n_fr_en
      call ems_msg_wr_li(serious_msg_n)
      goto 7100
 8110 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_no_blk,
     &     mem_mgr_rt_cod)
      goto 7100
 8120 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9812, err=8990)
     &     sn_p, n_wo,  blk_p, blk_n_wo
      call ems_msg_wr_li(serious_msg_n)
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_int_er,
     &     mem_mgr_rt_cod)
      goto 7100
 8990 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_serious_f7_wr_er,
     &     mem_mgr_rt_cod)
      goto 7100
 9800 format(a, ' has been called with value of ', i9,
     &     ' for parameter ', i1, '. The valid range is ', i9,
     &     ' to ', i9)
 9810 format('Attempting to get a handle for an array with ', i9,
     &     ' entries---each of ', i1,
     &     ' fullwords---in a block with room for only ', i9,
     &     ' such entries')
 9812 format('Unexpected internal error in ems_mem_mgr_nw_hdl: ',
     &     4(2x, i9))
      end
 
C->>> -------------------------------------------> ems_mem_mgr_ope_a <<<
c     Allows dynamic allocation of an array as a single block
c
c     Possible return code values set in this routine:
c     -----------------------------------------------
c
c     Routines called:
c     ---------------
c
      subroutine ems_mem_mgr_ope_a(mem_mgr_rt_cod, is, n_en, wo_z, hdl)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSMMGRI.INC'
      include 'EMSMSG.INC'
      integer is(0:*), n_en, wo_z, mem_mgr_rt_cod
      integer hdl(0:hdl_z_m1)
      logical ems_mem_mgr_no_ca_iz
      integer blk_n
      integer ca_mem_mgr_rt_cod
      character*17 rn_nm
      data rn_nm/'ems_mem_mgr_ope_a'/
 
      mem_mgr_rt_cod = mem_mgr_rt_cod_ok
      if (ems_mem_mgr_no_ca_iz(is)) goto 8000
      if (n_en .lt. 0) goto 8030
      if (wo_z .lt. 1) goto 8040
      call ems_mem_mgr_ope_blk(ca_mem_mgr_rt_cod, is,
     &     n_en*wo_z, ope_blk_anywhere,
     &     0, usr_a_blk_id, blk_n)
      if (ca_mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
         if (ca_mem_mgr_rt_cod .eq. mem_mgr_rt_cod_serious_no_po) then
            mem_mgr_rt_cod = mem_mgr_rt_cod_serious_no_po
         else
            mem_mgr_rt_cod = max(ca_mem_mgr_rt_cod, mem_mgr_rt_cod)
         endif
         if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 7100
      endif
      hdl(hdl_os_blk_n) = blk_n
      hdl(hdl_os_blk_os) = 0
      hdl(hdl_os_wo_z) = wo_z
c
c     Set the pointer in the handle
c
      call ems_mem_mgr_g_hdl_p(ca_mem_mgr_rt_cod, is,
     &     1, hdl)
      if (ca_mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
         mem_mgr_rt_cod = max(ca_mem_mgr_rt_cod, mem_mgr_rt_cod)
         if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 7100
      endif
c 7000 continue
 7100 continue
      return
 8000 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_no_ca_iz,
     &     mem_mgr_rt_cod)
      goto 7100
 8030 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_pm_er,
     &     mem_mgr_rt_cod)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800, err=8990)
     &     rn_nm, n_en, 3, 0, i_inf
      call ems_msg_wr_li(serious_msg_n)
      goto 7100
 8040 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_pm_er,
     &     mem_mgr_rt_cod)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800, err=8990)
     &     rn_nm, wo_z, 4, 1, i_inf
      call ems_msg_wr_li(serious_msg_n)
      goto 7100
 8990 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_serious_f7_wr_er,
     &     mem_mgr_rt_cod)
      goto 7100
 9800 format(a, ' has been called with value of ', i9,
     &     ' for parameter ', i1, '. The valid range is ', i9,
     &     ' to ', i9)
      end
 
C->>> -----------------------------------------> ems_mem_mgr_ope_blk <<<
c     Calls ems_mem_mgr_nw_blk to open a new block.
c     Calls ems_mem_mgr_g_al_p if blocks have moved
c
c     Possible return code values set in this routine:
c     -----------------------------------------------
c
c     Routines called:
c     ---------------
c
      subroutine ems_mem_mgr_ope_blk(mem_mgr_rt_cod, is,
     &     n_wo, where, blk_ix, usr_blk_id, blk_n)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSMMGRI.INC'
      include 'EMSMSG.INC'
      integer mem_mgr_rt_cod, is(0:*)
      integer n_wo, where, blk_ix, usr_blk_id, blk_n
      logical ems_mem_mgr_no_ca_iz
      integer blk_mv_k
      integer blk_id
      integer ca_mem_mgr_rt_cod
      character*19 rn_nm
      data rn_nm/'ems_mem_mgr_ope_blk'/
 
      mem_mgr_rt_cod = mem_mgr_rt_cod_ok
      if (ems_mem_mgr_no_ca_iz(is)) goto 8000
      if (n_wo .lt. 0) goto 8030
      if (where .lt. 0 .or. where .gt. 2) goto 8040
      blk_id = usr_blk_id + usr_blk_id_os
      if (blk_id .lt. 0 .or. blk_id .gt. n_blk_id) goto 8060
c
c     Switch off the block move lock and record the block move counter.
c
      is(ix_blk_mv_lck) = 0
      blk_mv_k = is(ix_blk_mv_k)
      call ems_mem_mgr_nw_blk(ca_mem_mgr_rt_cod, is,
     &     n_wo, where, blk_n, blk_ix, blk_id)
      if (ca_mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
         if (ca_mem_mgr_rt_cod .eq. mem_mgr_rt_cod_serious_no_po) then
            mem_mgr_rt_cod = mem_mgr_rt_cod_serious_no_po
         else
            mem_mgr_rt_cod = max(ca_mem_mgr_rt_cod, mem_mgr_rt_cod)
         endif
         if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 7100
      endif
c
c     Recalculate all pointers if (some) blocks have moved.
c
      if (is(ix_blk_mv_k) .gt. blk_mv_k) then
         call ems_mem_mgr_g_al_p(ca_mem_mgr_rt_cod, is)
         if (ca_mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
            mem_mgr_rt_cod = max(ca_mem_mgr_rt_cod, mem_mgr_rt_cod)
            if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 7100
         endif
      endif
c 7000 continue
 7100 continue
c
c     Switch on the block move lock
c
      is(ix_blk_mv_lck) = 1
      return
 8000 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_no_ca_iz,
     &     mem_mgr_rt_cod)
      goto 7100
 8030 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_pm_er,
     &     mem_mgr_rt_cod)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800, err=8990)
     &     rn_nm, n_wo, 3, 1, i_inf
      call ems_msg_wr_li(serious_msg_n)
      goto 7100
 8040 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_pm_er,
     &     mem_mgr_rt_cod)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800, err=8990)
     &     rn_nm, where, 4, 0, 2
      call ems_msg_wr_li(serious_msg_n)
      goto 7100
 8060 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_pm_er,
     &     mem_mgr_rt_cod)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800, err=8990)
     &     rn_nm, usr_blk_id, 6,
     &     -usr_blk_id_os, n_blk_id-usr_blk_id_os
      call ems_msg_wr_li(serious_msg_n)
      goto 7100
 8990 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_serious_f7_wr_er,
     &     mem_mgr_rt_cod)
      goto 7100
 9800 format(a, ' has been called with value of ', i9,
     &     ' for parameter ', i1, '. The valid range is ', i9,
     &     ' to ', i9)
      end
 
C->>> --------------------------------------------> ems_mem_mgr_rm_a <<<
c     Deallocates an array.
c
c     Possible return code values set in this routine:
c     -----------------------------------------------
c
c     Routines called:
c     ---------------
c
      subroutine ems_mem_mgr_rm_a(mem_mgr_rt_cod, is, hdl)
      implicit none
      include 'EMSMMGR.INC'
      integer mem_mgr_rt_cod, is(0:*)
      integer hdl(0:hdl_z_m1)
      logical ems_mem_mgr_no_ca_iz
      integer ca_mem_mgr_rt_cod
 
      mem_mgr_rt_cod = mem_mgr_rt_cod_ok
      if (ems_mem_mgr_no_ca_iz(is)) goto 8000
      call ems_mem_mgr_rm_blk(ca_mem_mgr_rt_cod, is, hdl(hdl_os_blk_n))
      if (ca_mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
         mem_mgr_rt_cod = max(ca_mem_mgr_rt_cod, mem_mgr_rt_cod)
         if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 7100
      endif
c 7000 continue
 7100 continue
      return
 8000 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_no_ca_iz,
     &     mem_mgr_rt_cod)
      goto 7100
      end
 
C->>> ------------------------------------------> ems_mem_mgr_rm_blk <<<
c     Removes block blk_n.
c
c     Possible return code values set in this routine:
c     -----------------------------------------------
c
c     Routines called:
c     ---------------
c
      subroutine ems_mem_mgr_rm_blk(mem_mgr_rt_cod, is, blk_n)
      implicit none
      include 'EMSMMGR.INC'
      include 'EMSMMGRI.INC'
      include 'EMSMSG.INC'
      integer mem_mgr_rt_cod, is(0:*)
      integer blk_n
      logical ems_mem_mgr_no_ca_iz
      integer mgr_p, nx_blk_n, pre_blk_n
      integer p_blk_da, p_pre_blk_da, p_nx_blk_da
      character*18 rn_nm
      data rn_nm/'ems_mem_mgr_rm_blk'/
 
      mem_mgr_rt_cod = mem_mgr_rt_cod_ok
      if (ems_mem_mgr_no_ca_iz(is)) goto 8000
c
c     Check that we are not trying to remove a block which does not
c     exist.
c
      if (blk_n .lt. 1 .or. blk_n .gt. is(ix_mx_n_blk)) goto 8030
      p_blk_da = is(ix_p_blk_da) + (blk_n-1)*blk_rec_z
      if (is(p_blk_da+os_blk_is_p) .le. 0) go to 8100
c
c     Add the block number to the free list (unless it is already full).
c
      if (is(ix_blk_n_fr_ls_n_en) .lt. mx_n_blk_n_fr_ls_en) then
         is(ix_blk_n_fr_ls_n_en) = is(ix_blk_n_fr_ls_n_en) + 1
         is(p_blk_n_fr_ls+is(ix_blk_n_fr_ls_n_en)) = blk_n
      endif
c
c     Update the number of free block numbers
c
      is(ix_n_fr_blk_n) = is(ix_n_fr_blk_n) + 1
c
c     Update the number of free words.
c
      is(ix_n_fr_wo) = is(ix_n_fr_wo) + is(p_blk_da+os_blk_n_wo)
      is(ix_mn_n_fr_wo) = min(is(ix_n_fr_wo), is(ix_mn_n_fr_wo))
c
c     Find the next and previous block in the list.
c
      nx_blk_n =  is(p_blk_da+os_blk_lkf)
      pre_blk_n = is(p_blk_da+os_blk_lkb)
      mgr_p =     is(p_blk_da+os_blk_mgr_p)
      p_nx_blk_da =  is(ix_p_blk_da) + (nx_blk_n-1)*blk_rec_z
      p_pre_blk_da = is(ix_p_blk_da) + (pre_blk_n-1)*blk_rec_z
      if (pre_blk_n .gt. 0) then
c
c     Update the forward link of the previous block.
c
         is(p_pre_blk_da+os_blk_lkf) = nx_blk_n
      else
c
c     The first block in the system is being removed.
c     Assumes that nx_blk_n > 0 (otherwise we are removing the only
c     block in the system which wouldn't happen).
c
         if (is(p_nx_blk_da+os_blk_mgr_p) .eq. blk_at_mgr_bac) then
c
c     There are no blocks in the front half of the manager.
c
            is(ix_f_frt_blk) = -1
            is(ix_f_blk_p) = is(ix_f_mgr_p) - 1
         else
            is(ix_f_frt_blk) = nx_blk_n
            is(ix_f_blk_p) = is(p_nx_blk_da+os_blk_is_p)
         end if
      end if
      if (nx_blk_n .gt. 0) then
c
c     Update the backward link of the next block.
c
         is(p_nx_blk_da+os_blk_lkb) = pre_blk_n
      else
c
c     The last block in the system is being removed.
c     Assumes that pre_blk_n > 0 (otherwise we are removing the only
c     block in the system which wouldn't happen).
c
         if (is(p_pre_blk_da+os_blk_mgr_p) .eq. blk_at_mgr_frt) then
c
c     There are no blocks in the back half of the manager.
c
            is(ix_l_bac_blk) = -1
            is(ix_l_blk_p) = is(ix_l_mgr_p)+1
         else
            is(ix_l_bac_blk) = pre_blk_n
            is(ix_l_blk_p) = is(p_pre_blk_da+os_blk_is_p) +
     &           is(p_pre_blk_da+os_blk_n_wo) - 1
         end if
      end if
      if (blk_n .eq. is(ix_l_frt_blk)) then
c
c     The block is removed from the beginning of the free space so
c     update the first free pointer and the last block at the front of
c     the memory manager.
c
         is(ix_l_frt_blk) = pre_blk_n
         if (pre_blk_n .gt. 0) then
            is(ix_f_fr_p) = is(p_pre_blk_da+os_blk_is_p) +
     &           is(p_pre_blk_da+os_blk_n_wo)
         else
            is(ix_f_fr_p) = is(ix_f_mgr_p)
         end if
      else if (blk_n .eq. is(ix_f_bac_blk)) then
c
c     The block is removed from the end of the free space so
c     update the last free pointer and the first block at the back of
c     the memory manager.
c
         is(ix_f_bac_blk) = nx_blk_n
         if (nx_blk_n .gt. 0) then
            is(ix_l_fr_p) = is(p_nx_blk_da+os_blk_is_p) - 1
         else
            is(ix_l_fr_p) = is(ix_l_mgr_p)
         end if
      end if
c
c     Zero the pointer for the block to be removed.
c
      is(p_blk_da+os_blk_is_p) = 0
      blk_n = 0
c 7000 continue
 7100 continue
      return
 8000 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_no_ca_iz,
     &     mem_mgr_rt_cod)
      goto 7100
 8030 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_pm_er,
     &     mem_mgr_rt_cod)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800, err=8990)
     &     rn_nm, blk_n, 3, 1, is(ix_mx_n_blk)
      call ems_msg_wr_li(serious_msg_n)
      goto 7100
 8100 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_no_blk,
     &     mem_mgr_rt_cod)
      goto 7100
 8990 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_serious_f7_wr_er,
     &     mem_mgr_rt_cod)
      goto 7100
 9800 format(a, ' has been called with value of ', i9,
     &     ' for parameter ', i1, '. The valid range is ', i9,
     &     ' to ', i9)
      end
 
C->>> --------------------------------------> ems_mem_mgr_rp_mem_use <<<
c     Report the memory usage.
c
c     If rp_cn is negative then messages are put out via ems_msg_wr_li,
c     otherwise, they are put out on rp_cn.
c
c     If rp_lvl=1 then a brief report is written
c     If rp_lvl=2 then a verbose report is written
c
c     Possible return code values set in this routine:
c     -----------------------------------------------
c
c     Routines called:
c     ---------------
c
      subroutine ems_mem_mgr_rp_mem_use(mem_mgr_rt_cod, is, rp_cn,
     &     rp_lvl, msg)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSMMGRI.INC'
      include 'EMSMSG.INC'
      integer mem_mgr_rt_cod, is(0:*), rp_cn, rp_lvl
      character*(*) msg
      logical ems_mem_mgr_no_ca_iz
      integer cu_rq_is_n_en, mx_rq_is_n_en
      integer sv_mem_mgr_ck_rp_mode
      integer ca_mem_mgr_rt_cod
      logical alw_f7_wr
      character*22 rn_nm
      data rn_nm/'ems_mem_mgr_rp_mem_use'/
 
      mem_mgr_rt_cod = mem_mgr_rt_cod_ok
      if (ems_mem_mgr_no_ca_iz(is)) goto 8000
      if (rp_lvl .lt. 1 .or. rp_lvl .gt. 2) goto 8050
      alw_f7_wr = ems_msg_no_prt_fm .ge. 1 .or. rp_cn .ge. 0
      if (rp_lvl .eq. 1) then
         if (alw_f7_wr) write(ems_li, 9000, err=8990)msg
         if (rp_cn .lt. 0) then
            call ems_msg_wr_li(info_msg_n)
         else
            if (alw_f7_wr) then
               call ems_msg_wr_li_t_cn(ca_mem_mgr_rt_cod, rp_cn)
               if (ca_mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
                  mem_mgr_rt_cod = max(ca_mem_mgr_rt_cod,
     &                 mem_mgr_rt_cod)
                  if (mem_mgr_rt_cod .ge.
     &                 mem_mgr_rt_lvl_serious) goto 7100
               endif
            endif
         endif
         call ems_mem_mgr_rp_ws_use(ca_mem_mgr_rt_cod, is, rp_cn,
     &     0, i_wo_z, 'integer')
         if (ca_mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
            mem_mgr_rt_cod = max(ca_mem_mgr_rt_cod, mem_mgr_rt_cod)
            if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 7100
         endif
         call ems_mem_mgr_g_rq_is_n_en(ca_mem_mgr_rt_cod, is,
     &        0, cu_rq_is_n_en, mx_rq_is_n_en)
         if (ca_mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
            mem_mgr_rt_cod = max(ca_mem_mgr_rt_cod, mem_mgr_rt_cod)
            if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 7100
         endif
         call ems_mem_mgr_rp_rq_is_n_en(ca_mem_mgr_rt_cod, is,
     &        rp_cn, cu_rq_is_n_en, mx_rq_is_n_en)
         if (ca_mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
            mem_mgr_rt_cod = max(ca_mem_mgr_rt_cod, mem_mgr_rt_cod)
            if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 7100
         endif
      else if (rp_lvl .eq. 2) then
c
c     Save the value of mem_mgr_ck_rp_mode
c
         sv_mem_mgr_ck_rp_mode = is(ix_mem_mgr_ck_rp_mode)
c
c     Force checking and writing of memory manager
c
         is(ix_mem_mgr_ck_rp_mode) = 2
         call ems_mem_mgr_ck_rp(ca_mem_mgr_rt_cod, is,
     &        rp_cn, -1, msg)
c
c     Restore the value of mem_mgr_ck_rp_mode
c
         is(ix_mem_mgr_ck_rp_mode) = sv_mem_mgr_ck_rp_mode
         if (ca_mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
            mem_mgr_rt_cod = max(ca_mem_mgr_rt_cod, mem_mgr_rt_cod)
            if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 7100
         endif
      endif
c 7000 continue
 7100 continue
      return
 8000 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_no_ca_iz,
     &     mem_mgr_rt_cod)
      goto 7100
 8050 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_pm_er,
     &     mem_mgr_rt_cod)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800, err=8990)
     &     rn_nm, rp_lvl, 5, 1, 2
      call ems_msg_wr_li(serious_msg_n)
      goto 7100
 8990 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_serious_f7_wr_er,
     &     mem_mgr_rt_cod)
      goto 7100
 9000 format(a)
 9800 format(a, ' has been called with value of ', i9,
     &     ' for parameter ', i1, '. The valid range is ', i9,
     &     ' to ', i9)
      end
 
C->>> -----------------------------------> ems_mem_mgr_rp_rq_is_n_en <<<
c     Calls the routine which reports the required and current number of
c     workspace entries, with parameters corresponding to the fullword
c     integer workspace.
c
c     If rp_cn is negative then messages are put out via ems_msg_wr_li,
c     otherwise, they are put out on rp_cn.
c
c     Possible return code values set in this routine:
c     -----------------------------------------------
c
c     Routines called:
c     ---------------
c
      subroutine ems_mem_mgr_rp_rq_is_n_en(mem_mgr_rt_cod, is, rp_cn,
     &     cu_rq_is_n_en, mx_rq_is_n_en)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSMSG.INC'
      integer mem_mgr_rt_cod, is(0:*), rp_cn
      integer cu_rq_is_n_en, mx_rq_is_n_en
      logical ems_mem_mgr_no_ca_iz
      integer ca_mem_mgr_rt_cod
 
      mem_mgr_rt_cod = mem_mgr_rt_cod_ok
      if (ems_mem_mgr_no_ca_iz(is)) goto 8000
      call ems_mem_mgr_rp_rq_ws_n_en(ca_mem_mgr_rt_cod, is, rp_cn,
     &     cu_rq_is_n_en, mx_rq_is_n_en, i_wo_z, 'integer')
      if (ca_mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
         mem_mgr_rt_cod = max(ca_mem_mgr_rt_cod, mem_mgr_rt_cod)
         if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 7100
      endif
 7100 continue
      return
 8000 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_no_ca_iz,
     &     mem_mgr_rt_cod)
      goto 7100
      end
 
C->>> ---------------------------------------> ems_mem_mgr_rp_rt_cod <<<
c     Report on the value of rp_rt_cod which is assumed to have been
c     returned by a memory manager routine `rn_nm'.
c
c     If rp_cn is negative then messages are put out via ems_msg_wr_li,
c     otherwise, they are put out on rp_cn.
c
c     Possible return code values set in this routine:
c     -----------------------------------------------
c
c     Routines called:
c     ---------------
c
      subroutine ems_mem_mgr_rp_rt_cod(mem_mgr_rt_cod, is, rp_cn,
     &     rp_rt_cod, rn_nm)
      implicit none
      include 'EMSMMGR.INC'
      include 'EMSMSG.INC'
      integer mem_mgr_rt_cod, is(0:*), rp_cn, rp_rt_cod
      character*(*) rn_nm
      integer rn_nm_ln, rp_rt_lvl, rp_rt_id
      integer part_li_l_ch
      character*11 ch11_rt_lvl
      character*90 ch90_part_li
      character*32 ch32_rn_nm
      integer rt_cod
      logical alw_f7_wr
 
      mem_mgr_rt_cod = mem_mgr_rt_cod_ok
      alw_f7_wr = ems_msg_no_prt_fm .ge. 1 .or. rp_cn .ge. 0
c
c     Copy the routine name into a local variable, giving an error
c     message if the string length is not positive.
c
      rn_nm_ln = min(len(rn_nm), 32)
      if (rn_nm_ln .ge. 1) then
         ch32_rn_nm = rn_nm(1:rn_nm_ln)
      else
         rn_nm_ln = 31
         ch32_rn_nm = 'with non-positive string length'
      endif
c
c     Extract the return code level and set up the first part of the
c     message.
c
      if (rp_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         rp_rt_lvl = mem_mgr_rt_lvl_serious
         ch11_rt_lvl = '* SERIOUS *'
      else if (rp_rt_cod .ge. mem_mgr_rt_lvl_er) then
         rp_rt_lvl = mem_mgr_rt_lvl_er
         ch11_rt_lvl = '** Error **'
      else if (rp_rt_cod .ge. mem_mgr_rt_lvl_warn) then
         rp_rt_lvl = mem_mgr_rt_lvl_warn
         ch11_rt_lvl = '* Warning *'
      else if (rp_rt_cod .ge. mem_mgr_rt_lvl_info) then
         rp_rt_lvl = mem_mgr_rt_lvl_info
         ch11_rt_lvl = 'Information'
      else
         goto 8000
      endif
      ch90_part_li =
     &     ch11_rt_lvl//
     &     ' return code from EMSOL memory manager routine '//
     &     ch32_rn_nm(1:rn_nm_ln)
      part_li_l_ch = 11 + 47 + rn_nm_ln
c
c     Get the return code ID by subtracting the return code level offset
c
      rp_rt_id = rp_rt_cod - rp_rt_lvl
c
c     Write out the message corresponding to the value of rp_rt_id
c
      if (rp_rt_id .eq. mem_mgr_rt_id_ok) then
         if (alw_f7_wr) write(ems_li, 9000, err=8990)
     &        ch90_part_li(1:part_li_l_ch), rp_rt_id
      else if (rp_rt_id .eq. mem_mgr_rt_id_mv_blk) then
         if (alw_f7_wr) write(ems_li, 9001, err=8990)
     &        ch90_part_li(1:part_li_l_ch), rp_rt_id
      else if (rp_rt_id .eq. mem_mgr_rt_id_no_po) then
         if (alw_f7_wr) write(ems_li, 9002, err=8990)
     &        ch90_part_li(1:part_li_l_ch), rp_rt_id
      else if (rp_rt_id .eq. mem_mgr_rt_id_g_p_wg_wo_z) then
         if (alw_f7_wr) write(ems_li, 9003, err=8990)
     &        ch90_part_li(1:part_li_l_ch), rp_rt_id
 
      else if (rp_rt_id .eq. mem_mgr_rt_id_f7_wr_er) then
         if (alw_f7_wr) write(ems_li, 9099, err=8990)
     &        ch90_part_li(1:part_li_l_ch), rp_rt_id
 
      else if (rp_rt_id .eq. mem_mgr_rt_id_no_ca_iz) then
         if (alw_f7_wr) write(ems_li, 9100, err=8990)
     &        ch90_part_li(1:part_li_l_ch), rp_rt_id
      else if (rp_rt_id .eq. mem_mgr_rt_id_pm_er) then
         if (alw_f7_wr) write(ems_li, 9101, err=8990)
     &        ch90_part_li(1:part_li_l_ch), rp_rt_id
      else if (rp_rt_id .eq. mem_mgr_rt_id_no_blk) then
         if (alw_f7_wr) write(ems_li, 9102, err=8990)
     &        ch90_part_li(1:part_li_l_ch), rp_rt_id
      else if (rp_rt_id .eq. mem_mgr_rt_id_blk_no_po) then
         if (alw_f7_wr) write(ems_li, 9103, err=8990)
     &        ch90_part_li(1:part_li_l_ch), rp_rt_id
      else if (rp_rt_id .eq. mem_mgr_rt_id_g_p_no_blk) then
         if (alw_f7_wr) write(ems_li, 9104, err=8990)
     &        ch90_part_li(1:part_li_l_ch), rp_rt_id
 
      else if (rp_rt_id .eq. mem_mgr_rt_id_int_er) then
         if (alw_f7_wr) write(ems_li, 9110, err=8990)
     &        ch90_part_li(1:part_li_l_ch), rp_rt_id
      else if (rp_rt_id .eq. mem_mgr_rt_id_blk_mv_lck_on) then
         if (alw_f7_wr) write(ems_li, 9111, err=8990)
     &        ch90_part_li(1:part_li_l_ch), rp_rt_id
      else if (rp_rt_id .eq. mem_mgr_rt_id_n_fr_wo_er) then
         if (alw_f7_wr) write(ems_li, 9112, err=8990)
     &        ch90_part_li(1:part_li_l_ch), rp_rt_id
      else if (rp_rt_id .eq. mem_mgr_rt_id_blk_n_fr_ls_er) then
         if (alw_f7_wr) write(ems_li, 9113, err=8990)
     &        ch90_part_li(1:part_li_l_ch), rp_rt_id
 
      else if (rp_rt_id .eq. mem_mgr_rt_id_emsol_p_er) then
         if (alw_f7_wr) write(ems_li, 9120, err=8990)
     &        ch90_part_li(1:part_li_l_ch), rp_rt_id
      else if (rp_rt_id .eq. mem_mgr_rt_id_usr_p_er) then
         if (alw_f7_wr) write(ems_li, 9121, err=8990)
     &        ch90_part_li(1:part_li_l_ch), rp_rt_id
 
      else if (rp_rt_id .eq. mem_mgr_rt_id_un_idf_rp_rt_cod) then
         if (alw_f7_wr) write(ems_li, 9999, err=8990)
     &        ch90_part_li(1:part_li_l_ch), rp_rt_id
      else
         goto 8000
      endif
      if (rp_cn .lt. 0) then
         call ems_msg_wr_li(info_msg_n)
      else
         if (alw_f7_wr) then
            call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
            if (rt_cod .ne. 0) goto 8990
         endif
      endif
c 7000 continue
 7100 continue
      return
 8000 continue
      if (alw_f7_wr) write(ems_li, 9800, err=8990)
     &     ch32_rn_nm(1:rn_nm_ln), rp_rt_cod
      if (rp_cn .lt. 0) then
         call ems_msg_wr_li(bug_msg_n)
      else
         if (alw_f7_wr) then
            call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
            if (rt_cod .ne. 0) goto 8990
         endif
      endif
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_un_idf_rp_rt_cod,
     &     mem_mgr_rt_cod)
      goto 7100
 8990 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_serious_f7_wr_er,
     &     mem_mgr_rt_cod)
      goto 7100
 9000 format(a, ': Return ID = ', i3,
     &     ': Routine completed successfully')
 9001 format(a, ': Return ID = ', i3,
     &     ': Block(s) moved')
 9002 format(a, ': Return ID = ', i3,
     &     ': Insufficient entries in workspace')
 9003 format(a, ': Return ID = ', i3,
     &     ': Inconsistency between word size in handle and',
     &     ' ems_mem_mgr_g_p* routine to which it was passed')
 
c 9098 format(a, ': Return ID = ', i3,
c     &     ': FORTRAN 77 read error')
 9099 format(a, ': Return ID = ', i3,
     &     ': FORTRAN 77 write error')
 
 9100 format(a, ': Return ID = ', i3,
     &     ': Must call ems_mem_mgr_iz before this routine')
 9101 format(a, ': Return ID = ', i3,
     &     ': Error in value of parameter to routine')
 9102 format(a, ': Return ID = ', i3,
     &     ': Attempting an operation involving a block which does ',
     &     'not exist')
 9103 format(a, ': Return ID = ', i3,
     &     ': Insufficient space in block for requested handle')
 9104 format(a, ': Return ID = ', i3,
     &     ': Asking for a pointer for a handle whose block ',
     &     'does not exist')
 
 9110 format(a, ': Return ID = ', i3,
     &     ': Internal error')
 9111 format(a, ': Return ID = ', i3,
     &     ': Internal routine called with the block move lock on')
 9112 format(a, ': Return ID = ', i3,
     &     ': Mismatch in number of free integer words')
 9113 format(a, ': Return ID = ', i3,
     &     ': Error in block number free list')
 
 9120 format(a, ': Return ID = ', i3,
     &     ': Error returned by se_emsol_p_no_p or g_emsol_p')
 9121 format(a, ': Return ID = ', i3,
     &     ': Error returned by se_usr_p_no_p or g_usr_p')
 
 9800 format(
     &     'From EMSOL memory manager routine ', a,
     &     ': Return code = ', i9,
     &     ': Code not recognised')
 9999 format(a, ': Return ID = ', i3,
     &     ': Return code to be reported was unidentified')
      end
 
C->>> -----------------------------------> ems_mem_mgr_se_hdl_p_no_p <<<
c     Sets the pointers associated with a set of handles to be `no
c     pointers'
c
c     Possible return code values set in this routine:
c     -----------------------------------------------
c
c     Routines called:
c     ---------------
c
      subroutine ems_mem_mgr_se_hdl_p_no_p(mem_mgr_rt_cod, is,
     &     n_hdl, hdl)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSMSG.INC'
      integer mem_mgr_rt_cod, is(0:*), n_hdl
      integer hdl(hdl_z*n_hdl)
      integer p
      character*25 rn_nm
      data rn_nm/'ems_mem_mgr_se_hdl_p_no_p'/
 
      mem_mgr_rt_cod = mem_mgr_rt_cod_ok
      if (n_hdl .lt. 0) goto 8030
      do 10, p = 1, (n_hdl-1)*hdl_z+1, hdl_z
         hdl(p+hdl_os_p) = no_p
 10   continue
 7100 continue
      return
 8030 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_pm_er,
     &     mem_mgr_rt_cod)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800, err=8990)
     &     rn_nm, n_hdl, 3, 0, i_inf
      call ems_msg_wr_li(serious_msg_n)
      goto 7100
 8990 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_serious_f7_wr_er,
     &     mem_mgr_rt_cod)
      goto 7100
 9800 format(a, ' has been called with value of ', i9,
     &     ' for parameter ', i1, '. The valid range is ', i9,
     &     ' to ', i9)
      end
 
C=======================================================================
c     Internally-called subroutines.
c
C->>> ------------------------------------------> ems_mem_mgr_int_iz <<<
c     Initialise the memory manager.
c
c     Possible return code values set in this routine:
c     -----------------------------------------------
c
c     Routines called:
c     ---------------
c
      subroutine ems_mem_mgr_int_iz(mem_mgr_rt_cod, is,
     &     is_n_en, mn_blk_p)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSMMGRI.INC'
      include 'EMSMSG.INC'
      integer mem_mgr_rt_cod, is_n_en, mn_blk_p
      integer is(0:is_n_en-1)
      integer ems_mem_mgr_nx_i_n_en_p
      integer blk_n, n_wo, tru_mn_blk_p, f_mgr_p
      integer cu_is_n_en, rq_is_n_en
      integer blk_n_fr_ls_n_en, en_n
      integer p_blk_da
      integer ca_mem_mgr_rt_cod
 
      mem_mgr_rt_cod = mem_mgr_rt_cod_ok
CM      IF (emsol_epc .EQ. 1) THEN
C?      call ems_debug_se_i_a_undn(is, 0, mem_mgr_da_l_is_en)
CM      ENDIF
c
c     Check that there are sufficient workspace entries to initialise
c     the memory manager
c
      if (is_n_en .lt. mem_mgr_da_l_is_en+1) goto 8000
c
c     Switch off the block move lock until the memory manager has been
c     initialised.
c
      is(ix_blk_mv_lck) = 0
      is(ix_blk_mv_k) = 0
      is(ix_n_fr_wo) = is_n_en
      is(ix_mn_n_fr_wo) = is(ix_n_fr_wo)
      is(ix_mem_mgr_ck_rp_mode) = 0
c
c     Entry mn_blk_p is the first available to be used for blocks.
c     Entries 0:mn_blk_p-1 should include entries 0:mem_mgr_da_l_is_en
c     which are used by the memory manager to store its scalar
c     information.
c
      tru_mn_blk_p = max(mn_blk_p, mem_mgr_da_l_is_en+1)
      f_mgr_p = ems_mem_mgr_nx_i_n_en_p(tru_mn_blk_p, rl_wo_z)
      is(ix_f_mgr_p) = f_mgr_p
      is(ix_n_fr_wo) = is(ix_n_fr_wo) - f_mgr_p
      is(ix_mn_n_fr_wo) = min(is(ix_n_fr_wo), is(ix_mn_n_fr_wo))
c
c     Entries is_n_en:... are not used by the block manager.
c
      is(ix_l_mgr_p) = is_n_en-1
c
c     Define the IDs and text for null blocks, the block of block data,
c     array-blocks and the last block ID.
c
      blk_id_txt(no_blk_id) = 'Block with no ID'
      blk_id_txt(blk_da_blk_id) = 'Memory manager block data'
      blk_id_txt(a_blk_id) = 'Array'
      blk_id_txt(mx_n_blk_id) = 'Block with last ID'
      n_blk_id = usr_blk_id_os
c
c     For each block there is:
c     0: A pointer into is
c     1: Position in the memory manager 1: Front; 2: Back
c     2: The number of words in the block
c     3: The first free pointer in the block
c     4: A link to the next block
c     5: A link to the previous block
c     6: An indentifying integer
c
c     The first block (at the moment) is the block of block data itself.
c
      is(ix_blk_blk_da) = 1
      is(ix_p_blk_da) = is(ix_f_mgr_p)
      is(ix_mx_n_blk) = og_mx_n_blk
      is(ix_mx_blk_n) = 1
c
c     Record the number of free block numbers and initialise the free
c     list of block numbers
c
      is(ix_n_fr_blk_n) = is(ix_mx_n_blk) - 1
      blk_n_fr_ls_n_en = min(is(ix_n_fr_blk_n), mx_n_blk_n_fr_ls_en)
c
c     Fill so that low-numbered blocks are placed last. Not that it
c     matters!
c
      do 5, en_n = 1, blk_n_fr_ls_n_en
         blk_n = 1 + (blk_n_fr_ls_n_en-en_n+1)
         is(p_blk_n_fr_ls+en_n) = blk_n
 5    continue
      is(ix_blk_n_fr_ls_n_en) = blk_n_fr_ls_n_en
      is(ix_n_xa_i_wo_rq) = 0
c
c     Round up the number of words requested to an integer number of
c     real words
c
      n_wo = ((is(ix_mx_n_blk)*blk_rec_z+rl_wo_z-1)/rl_wo_z)*rl_wo_z
c
c     Check that there is enough room for the block data.
c
      cu_is_n_en = is(ix_l_mgr_p) + 1
      rq_is_n_en = is(ix_f_mgr_p) + n_wo + 1
      if (cu_is_n_en .lt. rq_is_n_en) go to 8010
c
c     Initialise the block data records.
c
      p_blk_da = is(ix_p_blk_da)
      do 10, blk_n = 1, is(ix_mx_n_blk)
         is(p_blk_da+os_blk_is_p) = 0
         p_blk_da = p_blk_da + blk_rec_z
 10   continue
      p_blk_da = is(ix_p_blk_da)
      is(p_blk_da+os_blk_is_p) = p_blk_da
      is(p_blk_da+os_blk_mgr_p) = 1
      is(p_blk_da+os_blk_n_wo) = n_wo
      is(p_blk_da+os_blk_f_fr_p) = n_wo
      is(p_blk_da+os_blk_lkf) = -1
      is(p_blk_da+os_blk_lkb) = -1
      is(p_blk_da+os_blk_id) = blk_da_blk_id
      is(ix_n_fr_wo) = is(ix_n_fr_wo) - n_wo
      is(ix_mn_n_fr_wo) = min(is(ix_n_fr_wo), is(ix_mn_n_fr_wo))
      is(ix_f_frt_blk) = 1
      is(ix_l_frt_blk) = 1
      is(ix_f_bac_blk) = -1
      is(ix_l_bac_blk) = -1
      is(ix_f_blk_p) = is(ix_f_mgr_p)
      is(ix_f_fr_p) =  is(ix_f_mgr_p)+n_wo
      is(ix_l_fr_p) =  is(ix_l_mgr_p)
      is(ix_l_blk_p) = is(ix_l_mgr_p)+1
c
c     Set all pointers to be `no pointers'.
c
      call ems_mem_mgr_se_al_p_no_p(ca_mem_mgr_rt_cod, is)
      if (ca_mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
         mem_mgr_rt_cod = max(ca_mem_mgr_rt_cod, mem_mgr_rt_cod)
         if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 7100
      endif
c
c     Indicate that ems_mem_mgr_iz has been called with this is(*)
c
      is(ix_ca_mem_mgr_iz_fg1) = 1
      is(ix_ca_mem_mgr_iz_fg2) = 2
c 7000 continue
c
c     Switch on the block move lock.
c
      is(ix_blk_mv_lck) = 1
      call ems_mem_mgr_ck_rp(ca_mem_mgr_rt_cod, is,
     &     -1, -1, 'Initialise memory manager')
      if (ca_mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
         mem_mgr_rt_cod = max(ca_mem_mgr_rt_cod, mem_mgr_rt_cod)
         if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 7100
      endif
 7100 continue
      return
 8000 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_serious_no_po,
     &     mem_mgr_rt_cod)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800, err=8990)
      call ems_msg_wr_li(serious_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810, err=8990)
     &     mem_mgr_da_l_is_en+1, is_n_en
      call ems_msg_wr_li(serious_msg_n)
      go to 7100
 8010 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_serious_no_po,
     &     mem_mgr_rt_cod)
      is(ix_n_xa_i_wo_rq) = rq_is_n_en - cu_is_n_en
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801, err=8990)
      call ems_msg_wr_li(serious_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810, err=8990)
     &     rq_is_n_en, is_n_en
      call ems_msg_wr_li(serious_msg_n)
      go to 7100
 8990 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_serious_f7_wr_er,
     &     mem_mgr_rt_cod)
      goto 7100
 9800 format('Insufficient entries in integer workspace to',
     &     ' initialise the memory manager')
 9801 format('Insufficient entries in integer workspace to',
     &     ' open the block of block data')
 9810 format('Number of integer',
     &     ' workspace entries required is (at least) ', i9,
     &     '. Number of entries is currently ', i9)
      end
 
C->>> ---------------------------------------> ems_mem_mgr_rm_blk_wo <<<
c     Remove n_rm_wo from either the right or .not.right of block blk_n.
c
c     Possible return code values set in this routine:
c     -----------------------------------------------
c
c     Routines called:
c     ---------------
c
      subroutine ems_mem_mgr_rm_blk_wo(mem_mgr_rt_cod, is,
     &     blk_n, rm_n_wo, right)
      implicit none
      include 'EMSMMGR.INC'
      include 'EMSMMGRI.INC'
      include 'EMSMSG.INC'
      integer mem_mgr_rt_cod, is(0:*), blk_n, rm_n_wo
      logical right
      logical ems_mem_mgr_no_ca_iz
      integer blk_p, p_blk_da, mgr_p, n_wo, nw_n_wo, f_fr_p
      integer ca_mem_mgr_rt_cod
      character*21 rn_nm
      data rn_nm/'ems_mem_mgr_rm_blk_wo'/
 
      mem_mgr_rt_cod = mem_mgr_rt_cod_ok
      if (ems_mem_mgr_no_ca_iz(is)) goto 8000
      if (blk_n .lt. 1 .or. blk_n .gt. is(ix_mx_n_blk)) goto 8010
      p_blk_da = is(ix_p_blk_da) + (blk_n-1)*blk_rec_z
      blk_p =  is(p_blk_da+os_blk_is_p)
      if (blk_p .le. 0) goto 8020
      mgr_p =  is(p_blk_da+os_blk_mgr_p)
      n_wo =   is(p_blk_da+os_blk_n_wo)
      f_fr_p = is(p_blk_da+os_blk_f_fr_p)
      if (rm_n_wo .lt. 0 .or. rm_n_wo .gt. n_wo) goto 8030
      if (.not. right) goto 8040
      nw_n_wo = n_wo - rm_n_wo
c
      is(p_blk_da+os_blk_n_wo) = nw_n_wo
      is(p_blk_da+os_blk_f_fr_p) = min(nw_n_wo, f_fr_p)
      is(ix_n_fr_wo) = is(ix_n_fr_wo) + rm_n_wo
c
c     Update the first free pointer if this is the last front block.
c
      if (mgr_p .eq. blk_at_mgr_frt .and.
     &     blk_n .eq. is(ix_l_frt_blk)) is(ix_f_fr_p) = blk_p + nw_n_wo
c 7000 continue
 7100 continue
      call ems_mem_mgr_ck_rp(ca_mem_mgr_rt_cod, is,
     &     -1, blk_n, 'Remove words from block')
      if (ca_mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
         mem_mgr_rt_cod = max(ca_mem_mgr_rt_cod, mem_mgr_rt_cod)
         if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 7200
      endif
 7200 continue
      return
 8000 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_no_ca_iz,
     &     mem_mgr_rt_cod)
      goto 7100
 8010 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_pm_er,
     &     mem_mgr_rt_cod)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800, err=8990)
     &     rn_nm, blk_n, 3, 1, is(ix_mx_n_blk)
      call ems_msg_wr_li(serious_msg_n)
      goto 7100
 8020 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_no_blk,
     &     mem_mgr_rt_cod)
      goto 7100
 8030 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_pm_er,
     &     mem_mgr_rt_cod)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800, err=8990)
     &     rn_nm, rm_n_wo, 4, 0, n_wo
      call ems_msg_wr_li(serious_msg_n)
      goto 7100
 8040 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_pm_er,
     &     mem_mgr_rt_cod)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801, err=8990)
     &     rn_nm, right
      goto 7100
 8990 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_serious_f7_wr_er,
     &     mem_mgr_rt_cod)
      goto 7100
 9800 format(a, ' has been called with value of ', i9,
     &     ' for parameter ', i1, '. The valid range is ', i9,
     &     ' to ', i9)
 9801 format(a, ' has been called with value of .false.',
     &     ' for parameter ', i1, '. The valid range setting is .true.')
      end
 
C->>> -----------------------------------------> ems_mem_mgr_clo_blk <<<
c     Closes the block following the space given by handle hdl and n_en
c     entries of word length wo_z.
c
c     Possible return code values set in this routine:
c     -----------------------------------------------
c
c     Routines called:
c     ---------------
c
      subroutine ems_mem_mgr_clo_blk(mem_mgr_rt_cod, is,
     &     n_en, wo_z, hdl)
      implicit none
      include 'EMSMMGR.INC'
      include 'EMSMMGRI.INC'
      integer mem_mgr_rt_cod, is(0:*), n_en, wo_z
      integer hdl(0:hdl_z_m1)
      logical ems_mem_mgr_no_ca_iz
      integer p_blk_da
 
      mem_mgr_rt_cod = mem_mgr_rt_cod_ok
      if (ems_mem_mgr_no_ca_iz(is)) goto 8000
      p_blk_da = is(ix_p_blk_da) + (hdl(hdl_os_blk_n)-1)*blk_rec_z
      is(ix_n_fr_wo) = is(ix_n_fr_wo) +
     &     (is(p_blk_da+os_blk_n_wo)-hdl(hdl_os_blk_os)+n_en*wo_z)
      if (hdl(hdl_os_blk_n) .gt. 0) then
         is(p_blk_da+os_blk_n_wo) = hdl(hdl_os_blk_os)+n_en*wo_z
         is(p_blk_da+os_blk_f_fr_p) = hdl(hdl_os_blk_os)+n_en*wo_z
      endif
c 7000 continue
 7100 continue
      return
 8000 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_no_ca_iz,
     &     mem_mgr_rt_cod)
      goto 7100
      end
 
C->>> ------------------------------------> ems_mem_mgr_se_al_p_no_p <<<
c     Set all pointers to be `no pointers'.
c
c     Possible return code values set in this routine:
c     -----------------------------------------------
c
c     Routines called:
c     ---------------
c
      subroutine ems_mem_mgr_se_al_p_no_p(mem_mgr_rt_cod, is)
      implicit none
      include 'EMSMMGR.INC'
      integer mem_mgr_rt_cod, is(0:*)
      integer rt_cod
 
      mem_mgr_rt_cod = mem_mgr_rt_cod_ok
      call ems_se_emsol_p_no_p(rt_cod, is)
      if (rt_cod .ne. 0) goto 8010
      call ems_se_usr_p_no_p(rt_cod, is)
      if (rt_cod .ne. 0) goto 8020
 7100 continue
      return
 8010 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_emsol_p_er,
     &     mem_mgr_rt_cod)
      goto 7100
 8020 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_usr_p_er,
     &     mem_mgr_rt_cod)
      goto 7100
      end
 
C->>> ------------------------------------------> ems_mem_mgr_g_al_p <<<
c     Get all pointers
c
c     Possible return code values set in this routine:
c     -----------------------------------------------
c
c     Routines called:
c     ---------------
c
      subroutine ems_mem_mgr_g_al_p(mem_mgr_rt_cod, is)
      implicit none
      include 'EMSMMGR.INC'
      integer mem_mgr_rt_cod, is(0:*)
      integer rt_cod
 
      mem_mgr_rt_cod = mem_mgr_rt_cod_ok
      call ems_g_emsol_p(rt_cod, is)
      if (rt_cod .ne. 0) goto 8010
      call ems_g_usr_p(rt_cod, is)
      if (rt_cod .ne. 0) goto 8020
 7100 continue
      return
 8010 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_emsol_p_er,
     &     mem_mgr_rt_cod)
      goto 7100
 8020 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_usr_p_er,
     &     mem_mgr_rt_cod)
      goto 7100
      end
 
C->>> ------------------------------------------> ems_mem_mgr_nw_blk <<<
c     Sets up a new block of n_wo words with position determined by
c     where = 0: anywhere
c     where = 1: last block at start of free space
c     where = 2: first block at end of free space
c
c     Possible return code values set in this routine:
c     -----------------------------------------------
c
c     Routines called:
c     ---------------
c
      subroutine ems_mem_mgr_nw_blk(mem_mgr_rt_cod, is,
     &     ps_n_wo, where, blk_n, blk_ix, blk_id)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSMMGRI.INC'
      integer is(0:*)
      integer ps_n_wo, where, blk_n, blk_ix, blk_id, mem_mgr_rt_cod
      integer ems_mem_mgr_g_fr_blk_n, ems_mem_mgr_g_fr_blk_p
      integer mgr_p, blk_p, n_wo, nx_blk_n, pre_blk_n
      integer lc_blk_ix, lc_blk_id
      integer p_blk_da
      integer ca_mem_mgr_rt_cod
CM      IF (emsol_deb .EQ. 1) THEN
C?      integer ems_mem_mgr_g_n_fr_wo
C?      integer n_fr_wo
CM      ENDIF
 
      mem_mgr_rt_cod = mem_mgr_rt_cod_ok
c
c     Check that the block move lock is not on.
c
      if (is(ix_blk_mv_lck) .ne. 0) goto 8010
c
c     Get a free block number.
c
      blk_n = ems_mem_mgr_g_fr_blk_n(ca_mem_mgr_rt_cod, is)
      if (ca_mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
         if (ca_mem_mgr_rt_cod .eq. mem_mgr_rt_cod_serious_no_po) then
            mem_mgr_rt_cod = mem_mgr_rt_cod_serious_no_po
         else
            mem_mgr_rt_cod = max(ca_mem_mgr_rt_cod, mem_mgr_rt_cod)
         endif
         if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 7100
      endif
c
c     Take a copy of where because once the position of the block is
c     determined mgr_p will be assigned the value 1 or 2.
c
      mgr_p = where
c
c     Round up the number of words requested to an integer number of
c     real words
c
      n_wo = ((ps_n_wo+rl_wo_z-1)/rl_wo_z)*rl_wo_z
c
c     Get a pointer to enough space for the block.
c
      blk_p = ems_mem_mgr_g_fr_blk_p(ca_mem_mgr_rt_cod, is,
     &     n_wo, mgr_p, pre_blk_n, nx_blk_n)
      if (ca_mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
         if (ca_mem_mgr_rt_cod .eq. mem_mgr_rt_cod_serious_no_po) then
            mem_mgr_rt_cod = mem_mgr_rt_cod_serious_no_po
         else
            mem_mgr_rt_cod = max(ca_mem_mgr_rt_cod, mem_mgr_rt_cod)
         endif
         if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 7100
      endif
c
c     Set the data for the block.
c
      p_blk_da = is(ix_p_blk_da)+(blk_n-1)*blk_rec_z
      is(p_blk_da+os_blk_is_p) = blk_p
      is(p_blk_da+os_blk_mgr_p) = mgr_p
      is(p_blk_da+os_blk_n_wo) = n_wo
      is(p_blk_da+os_blk_f_fr_p) = 0
      is(p_blk_da+os_blk_lkf) = nx_blk_n
      is(p_blk_da+os_blk_lkb) = pre_blk_n
      lc_blk_ix = max(blk_ix, 0)
      lc_blk_id = min(max(blk_id, 0), n_blk_id)
      is(p_blk_da+os_blk_id) = lc_blk_id + lc_blk_ix*blk_ix_fac
      is(ix_n_fr_wo) = is(ix_n_fr_wo) - n_wo
      is(ix_mn_n_fr_wo) = min(is(ix_n_fr_wo), is(ix_mn_n_fr_wo))
      if (pre_blk_n .gt. 0) then
c
c     Update the forward link for the previous block.
c
         is(is(ix_p_blk_da)+(pre_blk_n-1)*blk_rec_z+os_blk_lkf) = blk_n
      else if (mgr_p .eq. blk_at_mgr_frt) then
c
c     Update the index of the first block in the system.
c
         is(ix_f_frt_blk) = blk_n
         is(ix_f_blk_p) = blk_p
      end if
      if (nx_blk_n .gt. 0) then
c
c     Update the backward link for the next block.
c
         is(is(ix_p_blk_da)+(nx_blk_n-1)*blk_rec_z+os_blk_lkb) = blk_n
      else if (mgr_p .eq. blk_at_mgr_bac) then
c
c     Update the index of the last block in the system and the pointer
c     to the start of the free space.
c
         is(ix_l_bac_blk) = blk_n
         is(ix_l_blk_p) = blk_p+n_wo - 1
      end if
      if (pre_blk_n .eq. is(ix_l_frt_blk)) then
         if (mgr_p .eq. blk_at_mgr_frt) then
c
c     The new block is placed at the beginning of the free space so
c     update the first free pointer and the last block at the front of
c     the memory manager.
c
            is(ix_l_frt_blk) = blk_n
            is(ix_f_fr_p) = blk_p+n_wo
         else
c
c     The new block is placed at the end of the free space so
c     update the last free pointer and the first block at the back of
c     the memory manager.
c
            is(ix_f_bac_blk) = blk_n
            is(ix_l_fr_p) = blk_p - 1
         end if
      end if
      is(ix_n_xa_i_wo_rq) = 0
CM      IF (emsol_deb .EQ. 1) THEN
C?c
C?c     Get the number of free words---ems_mem_mgr_g_n_fr_wo does a
C?c     check that the calculated number of free words is the same as the
C?c     updated number of free words.
C?c
C?      n_fr_wo = ems_mem_mgr_g_n_fr_wo(ca_mem_mgr_rt_cod, is)
C?      if (ca_mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
C?         mem_mgr_rt_cod = max(ca_mem_mgr_rt_cod, mem_mgr_rt_cod)
C?         if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 7100
C?      endif
CM      ENDIF
c
c     Update the number of free block numbers
c
      is(ix_n_fr_blk_n) = is(ix_n_fr_blk_n) - 1
c 7000 continue
CM      IF (emsol_deb .EQ. 1) THEN
C?      call ems_mem_mgr_ck_rp(ca_mem_mgr_rt_cod, is,
C?     &     -1, blk_n, 'New block')
C?      if (ca_mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
C?         mem_mgr_rt_cod = max(ca_mem_mgr_rt_cod, mem_mgr_rt_cod)
C?         if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 7100
C?      endif
CM      ENDIF
 7100 continue
      return
 8010 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_blk_mv_lck_on,
     &     mem_mgr_rt_cod)
      goto 7100
      end
 
C->>> ------------------------------------------> ems_mem_mgr_xp_blk <<<
c     Expands block blk_n to allow a further n_xa_wo to either the right
c     or .not.right, moving the blocks if necessary.
c
c     Possible return code values set in this routine:
c     -----------------------------------------------
c
c     Routines called:
c     ---------------
c
      subroutine ems_mem_mgr_xp_blk(mem_mgr_rt_cod, is,
     &     blk_n, n_xa_wo, right)
      implicit none
      include 'EMSMMGR.INC'
      include 'EMSMMGRI.INC'
      integer is(0:*), blk_n, n_xa_wo, mem_mgr_rt_cod
      logical right
      integer ems_mem_mgr_g_fr_blk_p
      integer blk_p, blk_id, mgr_p, n_wo, nx_blk_n, pre_blk_n, nx_blk_p
      integer pre_blk_p, ol_blk_p, cp_o_blk_n, pre_blk_n_wo, nw_n_wo
c      integer wo_n
      integer ca_mem_mgr_rt_cod
 
      mem_mgr_rt_cod = mem_mgr_rt_cod_ok
c
c     Check that the block move lock is not on.
c
      if (is(ix_blk_mv_lck) .ne. 0) goto 8010
c
c     Get the pointer to the block
c
      blk_p = is(is(ix_p_blk_da)+(blk_n-1)*blk_rec_z+os_blk_is_p)
c
c     If blk_p <= 0 then the block does not exist.
c
      if (blk_p .le. 0) goto 8020
      mgr_p =     is(is(ix_p_blk_da)+(blk_n-1)*blk_rec_z+os_blk_mgr_p)
      n_wo =      is(is(ix_p_blk_da)+(blk_n-1)*blk_rec_z+os_blk_n_wo)
      nx_blk_n =  is(is(ix_p_blk_da)+(blk_n-1)*blk_rec_z+os_blk_lkf)
      pre_blk_n = is(is(ix_p_blk_da)+(blk_n-1)*blk_rec_z+os_blk_lkb)
      blk_id =    is(is(ix_p_blk_da)+(blk_n-1)*blk_rec_z+os_blk_id)
      if (right) then
c
c     Aiming to expand a block to the right.
c
         if (nx_blk_n .gt. 0) then
c
c     When there is another block to the right, see if there is enough
c     space before it.
c
            nx_blk_p = is(is(ix_p_blk_da)+(nx_blk_n-1)*blk_rec_z+
     &           os_blk_is_p)
            if (blk_p+n_wo+n_xa_wo .ge. nx_blk_p) go to 100
         else
c
c     When expanding the last block, see if there is enough space before
c     the last free pointer.
c
            if (blk_p+n_wo+n_xa_wo .gt. is(ix_l_mgr_p)) go to 100
            if (mgr_p .eq. blk_at_mgr_bac)
     &           is(ix_l_blk_p) = is(ix_l_blk_p)+n_xa_wo
         end if
         if (blk_n .eq. is(ix_l_frt_blk))
     &        is(ix_f_fr_p) = is(ix_f_fr_p) + n_xa_wo
      else
c
c     Aiming to expand a block to the left.
c
         if (pre_blk_n .gt. 0) then
c
c     When there is another block to the left, see if there is enough
c     space after it.
c
            pre_blk_p = is(is(ix_p_blk_da)+(pre_blk_n-1)*blk_rec_z+
     &           os_blk_is_p)
            pre_blk_n_wo = is(is(ix_p_blk_da)+(pre_blk_n-1)*blk_rec_z+
     &           os_blk_n_wo)
            if (blk_p-n_xa_wo .lt. pre_blk_p+pre_blk_n_wo) go to 100
         else
c
c     When expanding the first block, see if there is enough space after
c     the first free pointer.
c
            if (blk_p-n_xa_wo .lt. is(ix_f_mgr_p)) go to 100
            if (mgr_p .eq. blk_at_mgr_frt)
     &           is(ix_f_blk_p) = is(ix_f_blk_p) - n_xa_wo
         end if
         is(is(ix_p_blk_da)+(blk_n-1)*blk_rec_z+os_blk_is_p) =
     &        blk_p - n_xa_wo
         if (blk_n .eq. is(ix_f_bac_blk))
     &        is(ix_l_fr_p) = is(ix_l_fr_p) - n_xa_wo
      end if
c
c     The block could be expanded in place so just update the number of
c     words in the block, the number of free words and return
c
      is(is(ix_p_blk_da)+(blk_n-1)*blk_rec_z+os_blk_n_wo) = n_wo+n_xa_wo
      is(ix_n_fr_wo) = is(ix_n_fr_wo) - n_xa_wo
      is(ix_mn_n_fr_wo) = min(is(ix_n_fr_wo), is(ix_mn_n_fr_wo))
      go to 7000
 100  continue
c
c     The block will have to be moved in order to be expanded.
c     Firstly a new block with sufficient space is allocated, then the
c     block to be expanded is removed from the system and finally its
c     existing data are moved into its new position.
c
      nw_n_wo = n_wo+n_xa_wo
c
c     The block cannot be expanded where it is so look for a pointer
c     which will allow the expanded block to be moved.
c
      blk_p = ems_mem_mgr_g_fr_blk_p(ca_mem_mgr_rt_cod, is,
     &     nw_n_wo, mgr_p, pre_blk_n, nx_blk_n)
      if (ca_mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
         if (ca_mem_mgr_rt_cod .eq. mem_mgr_rt_cod_serious_no_po) then
            mem_mgr_rt_cod = mem_mgr_rt_cod_serious_no_po
         else
            mem_mgr_rt_cod = max(ca_mem_mgr_rt_cod, mem_mgr_rt_cod)
         endif
         if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 7100
      endif
c
c     Store the old pointer of the block. (Don't copy blk_p from above
c     because the block may have been moved by ems_mem_mgr_g_fr_blk_p.)
c
      ol_blk_p = is(is(ix_p_blk_da)+(blk_n-1)*blk_rec_z+os_blk_is_p)
c
c     Remove the block from the linked list of blocks and reset the
c     block number because ems_rm_blk zeroes it.
c
      cp_o_blk_n = blk_n
      call ems_mem_mgr_rm_blk(ca_mem_mgr_rt_cod, is, blk_n)
      if (ca_mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
         mem_mgr_rt_cod = max(ca_mem_mgr_rt_cod, mem_mgr_rt_cod)
         if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 7100
      endif
      blk_n = cp_o_blk_n
c
c     If the block is on the free list then remove it: if it was put on
c     the list as a result of the call to ems_rm_blk then it can only be
c     the last entry.
c
      if (is(p_blk_n_fr_ls+is(ix_blk_n_fr_ls_n_en)) .eq. blk_n)
     &     is(ix_blk_n_fr_ls_n_en) = is(ix_blk_n_fr_ls_n_en) - 1
c
c     Update the number of free block numbers---since ems_mem_mgr_nw_blk
c     is not being called to allocate this block.
c
      is(ix_n_fr_blk_n) = is(ix_n_fr_blk_n) - 1
c
c     Copy the existing block data. THIS MOVES A BLOCK
c
      if (is(ix_blk_mv_lck) .ne. 0) goto 8010
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_mv_blk, mem_mgr_rt_cod)
c
c     Use ems_cp_i_a to avoid unassigned variable checking
c
      call ems_cp_i_a1(n_wo, is(ol_blk_p), is(blk_p))
c      do 110, wo_n = 0, n_wo-1
c         is(blk_p+wo_n) = is(ol_blk_p+wo_n)
c 110  continue
      is(ix_blk_mv_k) = is(ix_blk_mv_k) + 1
c
c     Update is(ix_p_blk_da) in case the block of block data has been
c     moved.
c
      if (blk_n .eq. is(ix_blk_blk_da)) is(ix_p_blk_da) = blk_p
c
c     Set the data for the block. (The first free pointer and the block
c     name are unchanged so do not need to be set)
c
      is(is(ix_p_blk_da)+(blk_n-1)*blk_rec_z+os_blk_is_p) =  blk_p
      is(is(ix_p_blk_da)+(blk_n-1)*blk_rec_z+os_blk_mgr_p) = mgr_p
      is(is(ix_p_blk_da)+(blk_n-1)*blk_rec_z+os_blk_n_wo) =  nw_n_wo
      is(is(ix_p_blk_da)+(blk_n-1)*blk_rec_z+os_blk_lkf) =   nx_blk_n
      is(is(ix_p_blk_da)+(blk_n-1)*blk_rec_z+os_blk_lkb) =   pre_blk_n
      is(is(ix_p_blk_da)+(blk_n-1)*blk_rec_z+os_blk_id) =    blk_id
      is(ix_n_fr_wo) = is(ix_n_fr_wo) - nw_n_wo
      is(ix_mn_n_fr_wo) = min(is(ix_n_fr_wo), is(ix_mn_n_fr_wo))
      if (pre_blk_n .gt. 0) then
c
c     Update the forward link for the previous block.
c
         is(is(ix_p_blk_da)+(pre_blk_n-1)*blk_rec_z+os_blk_lkf) = blk_n
      else if (mgr_p .eq. blk_at_mgr_frt) then
c
c     Update the index of the first block in the system.
c
         is(ix_f_frt_blk) = blk_n
         is(ix_f_blk_p) = blk_p
      end if
      if (nx_blk_n .gt. 0) then
c
c     Update the backward link for the next block.
c
         is(is(ix_p_blk_da)+(nx_blk_n-1)*blk_rec_z+os_blk_lkb) = blk_n
      else if (mgr_p .eq. blk_at_mgr_bac) then
c
c     Update the index of the last block in the system and the pointer
c     to the start of the free space.
c
         is(ix_l_bac_blk) = blk_n
         is(ix_l_blk_p) = blk_p+nw_n_wo - 1
      end if
      if (pre_blk_n .eq. is(ix_l_frt_blk)) then
         if (mgr_p .eq. blk_at_mgr_frt) then
c
c     The new block is placed at the beginning of the free space so
c     update the first free pointer and the last block at the front of
c     the memory manager.
c
            is(ix_l_frt_blk) = blk_n
            is(ix_f_fr_p) = blk_p+nw_n_wo
         else
c
c     The new block is placed at the end of the free space so
c     update the last free pointer and the first block at the back of
c     the memory manager.
c
            is(ix_f_bac_blk) = blk_n
            is(ix_l_fr_p) = blk_p - 1
         end if
      end if
 7000 continue
      call ems_mem_mgr_ck_rp(ca_mem_mgr_rt_cod, is,
     &     -1, blk_n, 'Expand block')
      if (ca_mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
         mem_mgr_rt_cod = max(ca_mem_mgr_rt_cod, mem_mgr_rt_cod)
         if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 7100
      endif
 7100 continue
      return
 8010 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_blk_mv_lck_on,
     &     mem_mgr_rt_cod)
      goto 7100
 8020 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_no_blk,
     &     mem_mgr_rt_cod)
      goto 7100
      end
 
C->>> ---------------------------------------------> ems_mem_mgr_css <<<
c     Compresses the front (back) of the memory manager if mgr_p = 1 (2)
c
c     Possible return code values set in this routine:
c     -----------------------------------------------
c
c     Routines called:
c     ---------------
c
      subroutine ems_mem_mgr_css(mem_mgr_rt_cod, is, mgr_p)
      implicit none
      include 'EMSMMGR.INC'
      include 'EMSMMGRI.INC'
      integer mem_mgr_rt_cod, is(0:*), mgr_p
      integer ems_mem_mgr_nx_i_n_en_p, ems_mem_mgr_pre_i_n_en_p
      integer blk_n, p, blk_p, blk_n_wo
c      integer wo_n
      logical blk_mv
      integer ca_mem_mgr_rt_cod
 
      mem_mgr_rt_cod = mem_mgr_rt_cod_ok
c
c     Check that the block move lock is not on.
c
      if (is(ix_blk_mv_lck) .ne. 0) goto 8010
      blk_mv = .false.
      if (mgr_p .eq. 1) then
         if (is(ix_f_frt_blk) .le. 0) go to 7000
         blk_n =  is(ix_f_frt_blk)
         p =      is(ix_f_mgr_p)
         is(ix_f_fr_p) = is(ix_f_mgr_p)
 100     continue
         if (blk_n .eq. is(ix_f_bac_blk)) then
            is(ix_f_fr_p) = p
            if (blk_mv) is(ix_blk_mv_k) = is(ix_blk_mv_k) + 1
            go to 7000
         end if
         blk_p =    is(is(ix_p_blk_da)+(blk_n-1)*blk_rec_z+os_blk_is_p)
         blk_n_wo = is(is(ix_p_blk_da)+(blk_n-1)*blk_rec_z+os_blk_n_wo)
         p = ems_mem_mgr_nx_i_n_en_p(p, 2)
         if (blk_p .gt. p) then
c
c      THIS MOVES A BLOCK
c
            if (is(ix_blk_mv_lck) .ne. 0) goto 8010
            mem_mgr_rt_cod = max(mem_mgr_rt_cod_mv_blk, mem_mgr_rt_cod)
c
c     Use ems_cp_i_a to avoid unassigned variable checking
c
            call ems_cp_i_a1(blk_n_wo, is(blk_p), is(p))
c            do 110, wo_n = 0, blk_n_wo-1
c               is(p+wo_n) = is(blk_p+wo_n)
c 110        continue
            blk_mv = .true.
c
c     Update is(ix_p_blk_da) in case the block of block data has been
c     moved.
c
            if (blk_n .eq. is(ix_blk_blk_da)) is(ix_p_blk_da) = p
            is(is(ix_p_blk_da)+(blk_n-1)*blk_rec_z+os_blk_is_p) = p
         end if
         p = p+blk_n_wo
         blk_n = is(is(ix_p_blk_da)+(blk_n-1)*blk_rec_z+os_blk_lkf)
         go to 100
      else
         if (is(ix_l_bac_blk) .le. 0) go to 7000
         blk_n =  is(ix_l_bac_blk)
         p =      is(ix_l_mgr_p)+1
         is(ix_l_fr_p) = is(ix_l_mgr_p)
 200     continue
         if (blk_n .eq. is(ix_l_frt_blk)) then
            is(ix_l_fr_p) = p - 1
            if (blk_mv) is(ix_blk_mv_k) = is(ix_blk_mv_k) + 1
            go to 7000
         end if
         blk_p =    is(is(ix_p_blk_da)+(blk_n-1)*blk_rec_z+os_blk_is_p)
         blk_n_wo = is(is(ix_p_blk_da)+(blk_n-1)*blk_rec_z+os_blk_n_wo)
         p = ems_mem_mgr_pre_i_n_en_p(p-blk_n_wo, 2)
         if (blk_p .lt. p) then
c
c      THIS MOVES A BLOCK
c
            if (is(ix_blk_mv_lck) .ne. 0) goto 8010
            mem_mgr_rt_cod = max(mem_mgr_rt_cod_mv_blk, mem_mgr_rt_cod)
c
c     Use ems_cp_i_a to avoid unassigned variable checking
c
            call ems_cp_i_a2(blk_n_wo, is(blk_p), is(p))
c            do 210, wo_n = blk_n_wo-1, 0, -1
c               is(p+wo_n) = is(blk_p+wo_n)
c 210        continue
            blk_mv = .true.
c
c     Update is(ix_p_blk_da) in case the block of block data has been
c     moved.
c
            if (blk_n .eq. is(ix_blk_blk_da)) is(ix_p_blk_da) = p
            is(is(ix_p_blk_da)+(blk_n-1)*blk_rec_z+os_blk_is_p) = p
         end if
         blk_n = is(is(ix_p_blk_da)+(blk_n-1)*blk_rec_z+os_blk_lkb)
         go to 200
      end if
 7000 continue
      call ems_mem_mgr_ck_rp(ca_mem_mgr_rt_cod, is,
     &     -1, blk_n, 'Compressing the memory manager')
      if (ca_mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
         mem_mgr_rt_cod = max(ca_mem_mgr_rt_cod, mem_mgr_rt_cod)
         if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 7100
      endif
 7100 continue
      return
 8010 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_blk_mv_lck_on,
     &     mem_mgr_rt_cod)
      goto 7100
      end
 
C->>> -------------------------------------------> ems_mem_mgr_ck_rp <<<
c     Check the memory management system and report on it if an error is
c     found.
c
c     If rp_cn is negative then messages are put out via ems_msg_wr_li,
c     otherwise, they are put out on rp_cn.
c
c     Possible return code values set in this routine:
c     -----------------------------------------------
c
c     Routines called:
c     ---------------
c
      subroutine ems_mem_mgr_ck_rp(mem_mgr_rt_cod, is, rp_cn, id, msg)
      implicit none
      include 'EMSMMGR.INC'
      include 'EMSMMGRI.INC'
      include 'EMSMSG.INC'
      integer mem_mgr_rt_cod, is(0:*), rp_cn, id
      character*(*) msg
      integer ems_mem_mgr_g_n_fr_wo
      integer n_fr_wo, sv_n_fr_wo
      integer msg_ln
      integer blk_n, blk_ix, blk_id
      integer tru_prev_blk_n, nx_blk_n, prev_blk_n
      integer mgr_p, f_p_af_prev_blk, blk_p
      integer blk_n_wo, blk_f_fr_p
      integer blk_mgr_p
      integer n_fr_blk_n, blk_n_fr_ls_n_en
      integer p_blk_da, en_n, f_en_n, fr_blk_n
      integer ca_mem_mgr_rt_cod
      integer rt_cod
      logical er_fd
      logical alw_f7_wr
c      integer rn_blk_mv_k
c      data rn_blk_mv_k/0/
 
      mem_mgr_rt_cod = mem_mgr_rt_cod_ok
      alw_f7_wr = ems_msg_no_prt_fm .ge. 1 .or. rp_cn .ge. 0
      msg_ln = len(msg)
      if (is(ix_mem_mgr_ck_rp_mode) .eq. 0) go to 7000
      er_fd = .false.
 50   continue
      if (is(ix_mem_mgr_ck_rp_mode) .ge. 2) then
         if (msg_ln .ge. 1) then
            if (id .ge. 0) then
               if (alw_f7_wr) write(ems_li, 9000, err=8990)
     &              msg, id
            else
               if (alw_f7_wr) write(ems_li, 9000, err=8990)
     &              msg
            endif
            if (rp_cn .lt. 0) then
               call ems_msg_wr_li(info_msg_n)
            else
               if (alw_f7_wr) then
                  call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
                  if (rt_cod .ne. 0) goto 8990
               endif
            endif
         endif
         if (alw_f7_wr) write(ems_li, 9001, err=8990)
     &        ' Block move counter     ', is(ix_blk_mv_k)
         if (rp_cn .lt. 0) then
            call ems_msg_wr_li(info_msg_n)
         else
            if (alw_f7_wr) then
               call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
               if (rt_cod .ne. 0) goto 8990
            endif
         endif
         if (alw_f7_wr) write(ems_li, 9001, err=8990)
     &        ' Number of free words   ', is(ix_n_fr_wo)
         if (rp_cn .lt. 0) then
            call ems_msg_wr_li(info_msg_n)
         else
            if (alw_f7_wr) then
               call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
               if (rt_cod .ne. 0) goto 8990
            endif
         endif
         if (alw_f7_wr) write(ems_li, 9001, err=8990)
     &        ' First manager pointer  ', is(ix_f_mgr_p)
         if (rp_cn .lt. 0) then
            call ems_msg_wr_li(info_msg_n)
         else
            if (alw_f7_wr) then
               call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
               if (rt_cod .ne. 0) goto 8990
            endif
         endif
         if (alw_f7_wr) write(ems_li, 9001, err=8990)
     &        ' First block   pointer  ', is(ix_f_blk_p)
         if (rp_cn .lt. 0) then
            call ems_msg_wr_li(info_msg_n)
         else
            if (alw_f7_wr) then
               call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
               if (rt_cod .ne. 0) goto 8990
            endif
         endif
         if (alw_f7_wr) write(ems_li, 9001, err=8990)
     &        ' First free    pointer  ', is(ix_f_fr_p)
         if (rp_cn .lt. 0) then
            call ems_msg_wr_li(info_msg_n)
         else
            if (alw_f7_wr) then
               call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
               if (rt_cod .ne. 0) goto 8990
            endif
         endif
         if (alw_f7_wr) write(ems_li, 9001, err=8990)
     &        ' Last  free    pointer  ', is(ix_l_fr_p)
         if (rp_cn .lt. 0) then
            call ems_msg_wr_li(info_msg_n)
         else
            if (alw_f7_wr) then
               call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
               if (rt_cod .ne. 0) goto 8990
            endif
         endif
         if (alw_f7_wr) write(ems_li, 9001, err=8990)
     &        ' Last  block   pointer  ', is(ix_l_blk_p)
         if (rp_cn .lt. 0) then
            call ems_msg_wr_li(info_msg_n)
         else
            if (alw_f7_wr) then
               call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
               if (rt_cod .ne. 0) goto 8990
            endif
         endif
         if (alw_f7_wr) write(ems_li, 9001, err=8990)
     &        ' Last  manager pointer  ', is(ix_l_mgr_p)
         if (rp_cn .lt. 0) then
            call ems_msg_wr_li(info_msg_n)
         else
            if (alw_f7_wr) then
               call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
               if (rt_cod .ne. 0) goto 8990
            endif
         endif
         if (alw_f7_wr) write(ems_li, 9001, err=8990)
     &        ' First front block      ', is(ix_f_frt_blk)
         if (rp_cn .lt. 0) then
            call ems_msg_wr_li(info_msg_n)
         else
            if (alw_f7_wr) then
               call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
               if (rt_cod .ne. 0) goto 8990
            endif
         endif
         if (alw_f7_wr) write(ems_li, 9001, err=8990)
     &        ' Last  front block      ', is(ix_l_frt_blk)
         if (rp_cn .lt. 0) then
            call ems_msg_wr_li(info_msg_n)
         else
            if (alw_f7_wr) then
               call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
               if (rt_cod .ne. 0) goto 8990
            endif
         endif
         if (alw_f7_wr) write(ems_li, 9001, err=8990)
     &        ' First back  block      ', is(ix_f_bac_blk)
         if (rp_cn .lt. 0) then
            call ems_msg_wr_li(info_msg_n)
         else
            if (alw_f7_wr) then
               call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
               if (rt_cod .ne. 0) goto 8990
            endif
         endif
         if (alw_f7_wr) write(ems_li, 9001, err=8990)
     &        ' Last  back  block      ', is(ix_l_bac_blk)
         if (rp_cn .lt. 0) then
            call ems_msg_wr_li(info_msg_n)
         else
            if (alw_f7_wr) then
               call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
               if (rt_cod .ne. 0) goto 8990
            endif
         endif
         if (alw_f7_wr) write(ems_li, 9100, err=8990)
         if (rp_cn .lt. 0) then
            call ems_msg_wr_li(info_msg_n)
         else
            if (alw_f7_wr) then
               call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
               if (rt_cod .ne. 0) goto 8990
            endif
         endif
      end if
      sv_n_fr_wo = is(ix_n_fr_wo)
      n_fr_wo = ems_mem_mgr_g_n_fr_wo(ca_mem_mgr_rt_cod, is)
      if (ca_mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
         mem_mgr_rt_cod = max(ca_mem_mgr_rt_cod, mem_mgr_rt_cod)
         if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 7100
      endif
      if (sv_n_fr_wo .ne. n_fr_wo) then
         er_fd = .true.
         mem_mgr_rt_cod = max(mem_mgr_rt_cod_int_er,
     &        mem_mgr_rt_cod)
         if (alw_f7_wr) write(ems_li, 9010, err=8990)
     &        n_fr_wo, sv_n_fr_wo
         if (rp_cn .lt. 0) then
            call ems_msg_wr_li(bug_msg_n)
         else
            if (alw_f7_wr) then
               call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
               if (rt_cod .ne. 0) goto 8990
            endif
         endif
      end if
      if (is(ix_f_frt_blk) .gt. 0 .and. is(ix_f_blk_p) .ne.
     &     is(is(ix_p_blk_da)+(is(ix_f_frt_blk)-1)*blk_rec_z+
     &     os_blk_is_p)) then
         er_fd = .true.
         mem_mgr_rt_cod = max(mem_mgr_rt_cod_int_er,
     &        mem_mgr_rt_cod)
         if (alw_f7_wr) write(ems_li, 9015, err=8990)
     &        is(ix_f_blk_p),
     &        is(is(ix_p_blk_da)+(is(ix_f_frt_blk)-1)*blk_rec_z+
     &        os_blk_is_p)
         if (rp_cn .lt. 0) then
            call ems_msg_wr_li(bug_msg_n)
         else
            if (alw_f7_wr) then
               call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
               if (rt_cod .ne. 0) goto 8990
            endif
         endif
      end if
      if (is(ix_l_frt_blk) .gt. 0 .and. is(ix_f_fr_p) .ne.
     &     is(is(ix_p_blk_da)+(is(ix_l_frt_blk)-1)*blk_rec_z+
     &     os_blk_is_p) +
     &     is(is(ix_p_blk_da)+(is(ix_l_frt_blk)-1)*blk_rec_z+
     &     os_blk_n_wo)) then
         er_fd = .true.
         mem_mgr_rt_cod = max(mem_mgr_rt_cod_int_er,
     &        mem_mgr_rt_cod)
         if (alw_f7_wr) write(ems_li, 9020, err=8990)
     &        is(ix_f_fr_p),
     &        is(is(ix_p_blk_da)+(is(ix_l_frt_blk)-1)*blk_rec_z+
     &        os_blk_is_p) +
     &        is(is(ix_p_blk_da)+(is(ix_l_frt_blk)-1)*blk_rec_z+
     &        os_blk_n_wo)
         if (rp_cn .lt. 0) then
            call ems_msg_wr_li(bug_msg_n)
         else
            if (alw_f7_wr) then
               call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
               if (rt_cod .ne. 0) goto 8990
            endif
         endif
      end if
c
c     If there are no free pointers between the front and back block
c     then surely is(ix_f_fr_p) = is(ix_l_fr_p) + 1 so the test
c     if (is(ix_f_fr_p) .gt. is(ix_l_fr_p)) is strictly not an error
c
c      if (is(ix_f_fr_p) .gt. is(ix_l_fr_p)) then
      if (is(ix_f_fr_p) .gt. is(ix_l_fr_p)+1) then
         er_fd = .true.
         mem_mgr_rt_cod = max(mem_mgr_rt_cod_int_er,
     &        mem_mgr_rt_cod)
         if (alw_f7_wr) write(ems_li, 9025, err=8990)
     &        is(ix_f_fr_p), is(ix_l_fr_p)
         if (rp_cn .lt. 0) then
            call ems_msg_wr_li(bug_msg_n)
         else
            if (alw_f7_wr) then
               call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
               if (rt_cod .ne. 0) goto 8990
            endif
         endif
c      else if (is(ix_f_fr_p) .gt. is(ix_l_fr_p)) then
c         if (alw_f7_wr) write(ems_li, 9025, err=8990)
c     &        is(ix_f_fr_p), is(ix_l_fr_p)
c         if (rp_cn .lt. 0) then
c            call ems_msg_wr_li(warn_msg_n)
c         else
c            if (alw_f7_wr) then
c               call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
c               if (rt_cod .ne. 0) goto 8990
c            endif
c         endif
      end if
      if (is(ix_f_bac_blk) .gt. 0 .and. is(ix_l_fr_p) .ne.
     &     is(is(ix_p_blk_da)+(is(ix_f_bac_blk)-1)*blk_rec_z+
     &     os_blk_is_p) - 1) then
         er_fd = .true.
         mem_mgr_rt_cod = max(mem_mgr_rt_cod_int_er,
     &        mem_mgr_rt_cod)
         if (alw_f7_wr) write(ems_li, 9030, err=8990)
     &        is(ix_l_fr_p),
     &        is(is(ix_p_blk_da)+(is(ix_f_bac_blk)-1)*blk_rec_z+
     &        os_blk_is_p) - 1
         if (rp_cn .lt. 0) then
            call ems_msg_wr_li(bug_msg_n)
         else
            if (alw_f7_wr) then
               call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
               if (rt_cod .ne. 0) goto 8990
            endif
         endif
      end if
      if (is(ix_l_bac_blk) .gt. 0 .and. is(ix_l_blk_p) .ne.
     &     is(is(ix_p_blk_da)+(is(ix_l_bac_blk)-1)*blk_rec_z+
     &     os_blk_is_p) +
     &     is(is(ix_p_blk_da)+(is(ix_l_bac_blk)-1)*blk_rec_z+
     &     os_blk_n_wo) - 1) then
         er_fd = .true.
         mem_mgr_rt_cod = max(mem_mgr_rt_cod_int_er,
     &        mem_mgr_rt_cod)
         if (alw_f7_wr) write(ems_li, 9040, err=8990)
     &        is(ix_l_blk_p),
     &        is(is(ix_p_blk_da)+(is(ix_l_bac_blk)-1)*blk_rec_z+
     &        os_blk_is_p) +
     &        is(is(ix_p_blk_da)+(is(ix_l_bac_blk)-1)*blk_rec_z+
     &        os_blk_n_wo) - 1
         if (rp_cn .lt. 0) then
            call ems_msg_wr_li(bug_msg_n)
         else
            if (alw_f7_wr) then
               call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
               if (rt_cod .ne. 0) goto 8990
            endif
         endif
      end if
c
c     Check and write for the front blocks in the system.
c
      blk_n = is(ix_f_frt_blk)
      tru_prev_blk_n = -1
      mgr_p = is(ix_f_mgr_p)
      f_p_af_prev_blk = mgr_p
 100  if (blk_n .eq. is(ix_f_bac_blk)) go to 200
      blk_p =      is(is(ix_p_blk_da)+(blk_n-1)*blk_rec_z+os_blk_is_p)
      blk_mgr_p =  is(is(ix_p_blk_da)+(blk_n-1)*blk_rec_z+os_blk_mgr_p)
      blk_n_wo =   is(is(ix_p_blk_da)+(blk_n-1)*blk_rec_z+os_blk_n_wo)
      blk_f_fr_p = is(is(ix_p_blk_da)+(blk_n-1)*blk_rec_z+os_blk_f_fr_p)
      nx_blk_n =   is(is(ix_p_blk_da)+(blk_n-1)*blk_rec_z+os_blk_lkf)
      prev_blk_n = is(is(ix_p_blk_da)+(blk_n-1)*blk_rec_z+os_blk_lkb)
      blk_id =     is(is(ix_p_blk_da)+(blk_n-1)*blk_rec_z+os_blk_id)
      blk_ix = blk_id/blk_ix_fac
      blk_id = blk_id - blk_ix*blk_ix_fac
      if (blk_ix .lt. 0) then
         er_fd = .true.
         mem_mgr_rt_cod = max(mem_mgr_rt_cod_int_er,
     &        mem_mgr_rt_cod)
         if (alw_f7_wr) write(ems_li, 9120, err=8990)
     &        blk_ix
         if (rp_cn .lt. 0) then
            call ems_msg_wr_li(bug_msg_n)
         else
            if (alw_f7_wr) then
               call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
               if (rt_cod .ne. 0) goto 8990
            endif
         endif
         blk_ix = 0
      endif
      if (blk_id .lt. 0 .or. blk_id .gt. n_blk_id) then
         er_fd = .true.
         mem_mgr_rt_cod = max(mem_mgr_rt_cod_int_er,
     &        mem_mgr_rt_cod)
         if (alw_f7_wr) write(ems_li, 9130, err=8990)
     &        blk_id
         if (rp_cn .lt. 0) then
            call ems_msg_wr_li(bug_msg_n)
         else
            if (alw_f7_wr) then
               call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
               if (rt_cod .ne. 0) goto 8990
            endif
         endif
         blk_id = 0
      endif
      if (is(ix_mem_mgr_ck_rp_mode) .ge. 2) then
        if (alw_f7_wr) write(ems_li, 9110, err=8990)
     &        blk_n, blk_mgr_p, prev_blk_n, blk_p-f_p_af_prev_blk,
     &        blk_p, blk_f_fr_p, blk_n_wo,
     &        blk_p+blk_n_wo-1, nx_blk_n,
     &        blk_ix, blk_id_txt(blk_id)
        if (rp_cn .lt. 0) then
           call ems_msg_wr_li(info_msg_n)
        else
           if (alw_f7_wr) then
              call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
              if (rt_cod .ne. 0) goto 8990
           endif
        endif
      end if
      if (blk_mgr_p .ne. blk_at_mgr_frt) then
         er_fd = .true.
         mem_mgr_rt_cod = max(mem_mgr_rt_cod_int_er,
     &        mem_mgr_rt_cod)
         if (alw_f7_wr) write(ems_li, 9140, err=8990)
     &        blk_mgr_p
         if (rp_cn .lt. 0) then
            call ems_msg_wr_li(bug_msg_n)
         else
            if (alw_f7_wr) then
               call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
               if (rt_cod .ne. 0) goto 8990
            endif
         endif
      end if
      if (mgr_p .gt. blk_p) then
         er_fd = .true.
         mem_mgr_rt_cod = max(mem_mgr_rt_cod_int_er,
     &        mem_mgr_rt_cod)
         if (alw_f7_wr) write(ems_li, 9150, err=8990)
     &        mgr_p, blk_p
         if (rp_cn .lt. 0) then
            call ems_msg_wr_li(bug_msg_n)
         else
            if (alw_f7_wr) then
               call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
               if (rt_cod .ne. 0) goto 8990
            endif
         endif
      end if
      if (blk_f_fr_p .gt. blk_n_wo) then
         er_fd = .true.
         mem_mgr_rt_cod = max(mem_mgr_rt_cod_int_er,
     &        mem_mgr_rt_cod)
         if (alw_f7_wr) write(ems_li, 9155, err=8990)
     &        blk_f_fr_p, blk_n_wo
         if (rp_cn .lt. 0) then
            call ems_msg_wr_li(bug_msg_n)
         else
            if (alw_f7_wr) then
               call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
               if (rt_cod .ne. 0) goto 8990
            endif
         endif
      end if
      if (tru_prev_blk_n .ne. prev_blk_n) then
         er_fd = .true.
         mem_mgr_rt_cod = max(mem_mgr_rt_cod_int_er,
     &        mem_mgr_rt_cod)
         if (alw_f7_wr) write(ems_li, 9160, err=8990)
     &        tru_prev_blk_n, prev_blk_n
         if (rp_cn .lt. 0) then
            call ems_msg_wr_li(bug_msg_n)
         else
            if (alw_f7_wr) then
               call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
               if (rt_cod .ne. 0) goto 8990
            endif
         endif
      end if
      if (blk_n .eq. nx_blk_n) then
         er_fd = .true.
         mem_mgr_rt_cod = max(mem_mgr_rt_cod_int_er,
     &        mem_mgr_rt_cod)
         if (alw_f7_wr) write(ems_li, 9170, err=8990)
     &        blk_n, nx_blk_n
         if (rp_cn .lt. 0) then
            call ems_msg_wr_li(bug_msg_n)
         else
            if (alw_f7_wr) then
               call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
               if (rt_cod .ne. 0) goto 8990
            endif
         endif
         blk_n = is(ix_f_bac_blk)
         go to 200
      end if
      tru_prev_blk_n = blk_n
      mgr_p = blk_p + blk_n_wo
      f_p_af_prev_blk = mgr_p
      blk_n = nx_blk_n
      go to 100
 200  continue
c
c     Check and write for the back blocks in the system.
c
      mgr_p = is(ix_l_fr_p)+1
 210  if (blk_n .eq. -1) then
         if (er_fd .and. is(ix_mem_mgr_ck_rp_mode) .lt. 2) then
            is(ix_mem_mgr_ck_rp_mode) = 2
            go to 50
         end if
         go to 1000
      end if
      blk_p =      is(is(ix_p_blk_da)+(blk_n-1)*blk_rec_z+os_blk_is_p)
      blk_mgr_p =  is(is(ix_p_blk_da)+(blk_n-1)*blk_rec_z+os_blk_mgr_p)
      blk_n_wo =   is(is(ix_p_blk_da)+(blk_n-1)*blk_rec_z+os_blk_n_wo)
      blk_f_fr_p = is(is(ix_p_blk_da)+(blk_n-1)*blk_rec_z+os_blk_f_fr_p)
      nx_blk_n =   is(is(ix_p_blk_da)+(blk_n-1)*blk_rec_z+os_blk_lkf)
      prev_blk_n = is(is(ix_p_blk_da)+(blk_n-1)*blk_rec_z+os_blk_lkb)
      blk_id =     is(is(ix_p_blk_da)+(blk_n-1)*blk_rec_z+os_blk_id)
      blk_ix = blk_id/blk_ix_fac
      blk_id = blk_id - blk_ix*blk_ix_fac
      if (blk_ix .lt. 0) then
         er_fd = .true.
         mem_mgr_rt_cod = max(mem_mgr_rt_cod_int_er,
     &        mem_mgr_rt_cod)
         if (alw_f7_wr) write(ems_li, 9120, err=8990)
     &        blk_ix
         if (rp_cn .lt. 0) then
            call ems_msg_wr_li(bug_msg_n)
         else
            if (alw_f7_wr) then
               call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
               if (rt_cod .ne. 0) goto 8990
            endif
         endif
         blk_ix = 0
      endif
      if (blk_id .lt. 0 .or. blk_id .gt. n_blk_id) then
         er_fd = .true.
         mem_mgr_rt_cod = max(mem_mgr_rt_cod_int_er,
     &        mem_mgr_rt_cod)
         if (alw_f7_wr) write(ems_li, 9130, err=8990)
     &        blk_id
         if (rp_cn .lt. 0) then
            call ems_msg_wr_li(bug_msg_n)
         else
            if (alw_f7_wr) then
               call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
               if (rt_cod .ne. 0) goto 8990
            endif
         endif
         blk_id = 0
      endif
      if (is(ix_mem_mgr_ck_rp_mode) .ge. 2) then
         if (alw_f7_wr) write(ems_li, 9110, err=8990)
     &        blk_n, blk_mgr_p, prev_blk_n, blk_p-f_p_af_prev_blk,
     &        blk_p, blk_f_fr_p, blk_n_wo,
     &        blk_p+blk_n_wo-1, nx_blk_n,
     &        blk_ix, blk_id_txt(blk_id)
         if (rp_cn .lt. 0) then
            call ems_msg_wr_li(info_msg_n)
         else
            if (alw_f7_wr) then
               call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
               if (rt_cod .ne. 0) goto 8990
            endif
         endif
      end if
      if (blk_mgr_p .ne. blk_at_mgr_bac) then
         er_fd = .true.
         mem_mgr_rt_cod = max(mem_mgr_rt_cod_int_er,
     &        mem_mgr_rt_cod)
         if (alw_f7_wr) write(ems_li, 9140, err=8990)
     &        blk_mgr_p
         if (rp_cn .lt. 0) then
            call ems_msg_wr_li(bug_msg_n)
         else
            if (alw_f7_wr) then
               call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
               if (rt_cod .ne. 0) goto 8990
            endif
         endif
      end if
      if (mgr_p .gt. blk_p) then
         er_fd = .true.
         mem_mgr_rt_cod = max(mem_mgr_rt_cod_int_er,
     &        mem_mgr_rt_cod)
         if (alw_f7_wr) write(ems_li, 9150, err=8990)
     &        mgr_p, blk_p
         if (rp_cn .lt. 0) then
            call ems_msg_wr_li(bug_msg_n)
         else
            if (alw_f7_wr) then
               call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
               if (rt_cod .ne. 0) goto 8990
            endif
         endif
      end if
      if (blk_f_fr_p .gt. blk_n_wo) then
         er_fd = .true.
         mem_mgr_rt_cod = max(mem_mgr_rt_cod_int_er,
     &        mem_mgr_rt_cod)
         if (alw_f7_wr) write(ems_li, 9155, err=8990)
     &        blk_f_fr_p, blk_n_wo
         if (rp_cn .lt. 0) then
            call ems_msg_wr_li(bug_msg_n)
         else
            if (alw_f7_wr) then
               call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
               if (rt_cod .ne. 0) goto 8990
            endif
         endif
      end if
      if (tru_prev_blk_n .ne. prev_blk_n) then
         er_fd = .true.
         mem_mgr_rt_cod = max(mem_mgr_rt_cod_int_er,
     &        mem_mgr_rt_cod)
         if (alw_f7_wr) write(ems_li, 9160, err=8990)
     &        tru_prev_blk_n, prev_blk_n
         if (rp_cn .lt. 0) then
            call ems_msg_wr_li(bug_msg_n)
         else
            if (alw_f7_wr) then
               call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
               if (rt_cod .ne. 0) goto 8990
            endif
         endif
      end if
      if (blk_n .eq. nx_blk_n) then
         er_fd = .true.
         mem_mgr_rt_cod = max(mem_mgr_rt_cod_int_er,
     &        mem_mgr_rt_cod)
         if (alw_f7_wr) write(ems_li, 9170, err=8990)
     &        blk_n, nx_blk_n
         if (rp_cn .lt. 0) then
            call ems_msg_wr_li(bug_msg_n)
         else
            if (alw_f7_wr) then
               call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
               if (rt_cod .ne. 0) goto 8990
            endif
         endif
         if (er_fd .and. is(ix_mem_mgr_ck_rp_mode) .lt. 2) then
            is(ix_mem_mgr_ck_rp_mode) = 2
            go to 50
         end if
         go to 1000
      end if
      tru_prev_blk_n = blk_n
      mgr_p = blk_p + blk_n_wo
      f_p_af_prev_blk = mgr_p
      blk_n = nx_blk_n
      go to 210
 1000 continue
c
c     Check the number of free blocks
c
      n_fr_blk_n = 0
      p_blk_da = is(ix_p_blk_da)
      do 1010, blk_n = 1, is(ix_mx_n_blk)
         if (is(p_blk_da) .eq. 0) n_fr_blk_n = n_fr_blk_n + 1
         p_blk_da = p_blk_da + blk_rec_z
 1010 continue
      if (is(ix_n_fr_blk_n) .ne. n_fr_blk_n) then
         er_fd = .true.
         mem_mgr_rt_cod = max(mem_mgr_rt_cod_int_er,
     &        mem_mgr_rt_cod)
         if (alw_f7_wr) write(ems_li, 9200, err=8990)
     &        is(ix_n_fr_blk_n), n_fr_blk_n
         if (rp_cn .lt. 0) then
            call ems_msg_wr_li(bug_msg_n)
         else
            if (alw_f7_wr) then
               call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
               if (rt_cod .ne. 0) goto 8990
            endif
         endif
      endif
c
c     Check and report on the free list
c
      blk_n_fr_ls_n_en = is(ix_blk_n_fr_ls_n_en)
      do 1020, en_n = 1, blk_n_fr_ls_n_en
         fr_blk_n = is(p_blk_n_fr_ls+en_n)
         p_blk_da = is(ix_p_blk_da) + (fr_blk_n-1)*blk_rec_z
         if (is(p_blk_da) .ne. 0) then
            er_fd = .true.
            mem_mgr_rt_cod = max(mem_mgr_rt_cod_int_er,
     &           mem_mgr_rt_cod)
            if (alw_f7_wr) write(ems_li, 9210, err=8990)
     &           fr_blk_n
            if (rp_cn .lt. 0) then
               call ems_msg_wr_li(bug_msg_n)
            else
               if (alw_f7_wr) then
                  call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
                  if (rt_cod .ne. 0) goto 8990
               endif
            endif
         endif
 1020 continue
      if (er_fd .and. is(ix_mem_mgr_ck_rp_mode) .lt. 2) then
         is(ix_mem_mgr_ck_rp_mode) = 2
         go to 50
      end if
      if (alw_f7_wr) write(ems_li, 9220, err=8990)
     &     blk_n_fr_ls_n_en,
     &     (is(p_blk_n_fr_ls+en_n), en_n = 1,
     &     min(blk_n_fr_ls_n_en, blk_n_fr_ls_rp_li_n_en))
      if (rp_cn .lt. 0) then
         call ems_msg_wr_li(info_msg_n)
      else
         if (alw_f7_wr) then
            call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
            if (rt_cod .ne. 0) goto 8990
         endif
      endif
      do 1040, f_en_n = blk_n_fr_ls_rp_li_n_en+1,
     &     blk_n_fr_ls_n_en, blk_n_fr_ls_rp_li_n_en
         if (alw_f7_wr) write(ems_li, 9221, err=8990)
     &        (is(p_blk_n_fr_ls+en_n), en_n = f_en_n,
     &        min(f_en_n+blk_n_fr_ls_rp_li_n_en-1, blk_n_fr_ls_n_en))
         if (rp_cn .lt. 0) then
            call ems_msg_wr_li(info_msg_n)
         else
            if (alw_f7_wr) then
               call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
               if (rt_cod .ne. 0) goto 8990
            endif
         endif
 1040 continue
 7000 continue
 7100 continue
      return
 8990 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_serious_f7_wr_er,
     &     mem_mgr_rt_cod)
      goto 7100
 9000 format(' Reporting memory usage: ', a, 2x, i9)
 9001 format(a, i9)
 9010 format('n_fr_wo and sv_n_fr_wo     ', i9, 2x, i9)
 9015 format('f_blk_p and f_frt_blk_f_p  ', i9, 2x, i9)
 9020 format('f_fr_p  and l_frt_blk_l_p+1', i9, 2x, i9)
 9025 format('f_fr_p  and l_fr_p         ', i9, 2x, i9)
 9030 format('l_fr_p  and f_bac_blk_f_p-1', i9, 2x, i9)
 9040 format('l_blk_p and l_bac_blk_l_p  ', i9, 2x, i9)
 9100 format(
     &     '    Block n    frt/bac   previous        gap',
     &     '      start       f_fr     length        end',
     &     '       next   index  ID')
 9110 format(9(2x, i9), 2x, i6, 2x, a)
 9120 format('Block index out of range               ', i9)
 9130 format('Block ID    out of range               ', i9)
 9140 format('Block manager position blk_mgr_p       ', i9)
 9150 format('Block overlap mgr_p, blk_p             ', i9, 2x, i9)
 9155 format('Block overwrite blk_f_fr_p, blk_n_wo   ', i9, 2x, i9)
 9160 format('Block links tru_prev_blk_n, prev_blk_n ', i9, 2x, i9)
 9170 format('Block links blk_n, nx_blk_n            ', i9, 2x, i9)
 9200 format('Inconsistent number of free block numbers ', i9, 2x, i9)
 9210 format('Block ', i9, ' is on the free list but is not free')
 9220 format('Block number free list has ', i9, ' entries: ',
     &     10(2x, i5))
 9221 format(46x,                                10(2x, i5))
c 9400 format('Compressing memory manager')
c 9410 format('Block move n ', i9)
      end
 
c->>> --------------------------------------> ems_mem_mgr_rp_ope_blk <<<
c     Reports on the composition of a block just opened.
c
c     Possible return code values set in this routine:
c     -----------------------------------------------
c
c     Routines called:
c     ---------------
c
      subroutine ems_mem_mgr_rp_ope_blk(mem_mgr_rt_cod, is, rp_cn,
     &     r_cf, n_r,
     &     c_cf, n_c,
     &     a_el_cf, n_a_el,
     &     cs, n_wo, usr_blk_id)
      implicit none
      include 'EMSMMGR.INC'
      include 'EMSMMGRI.INC'
      include 'EMSMSG.INC'
      integer mem_mgr_rt_cod, rp_cn
      integer r_cf, n_r, c_cf, n_c, a_el_cf, n_a_el, cs, n_wo
      integer usr_blk_id
      integer is(0:*)
      logical ems_mem_mgr_no_ca_iz
      integer ems_mem_mgr_g_n_fr_wo
      integer n_fr_wo
      integer alt_n_wo
      integer lc_blk_id
      integer ca_mem_mgr_rt_cod
      logical alw_f7_wr
 
      mem_mgr_rt_cod = mem_mgr_rt_cod_ok
      if (ems_mem_mgr_no_ca_iz(is)) goto 8000
      alw_f7_wr = ems_msg_no_prt_fm .ge. 1 .or. rp_cn .ge. 0
c
c     Report the constitution of the block just opened and the resulting
c     number of free words.
c
      lc_blk_id = min(max(usr_blk_id+usr_blk_id_os, 0), n_blk_id)
      n_fr_wo = ems_mem_mgr_g_n_fr_wo(ca_mem_mgr_rt_cod, is)
      if (ca_mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
         mem_mgr_rt_cod = max(ca_mem_mgr_rt_cod, mem_mgr_rt_cod)
         if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 7100
      endif
      alt_n_wo = r_cf*n_r + c_cf*n_c + a_el_cf*n_a_el + cs
      if (alt_n_wo .ne. n_wo) then
         if (alw_f7_wr) write(ems_li, 9500, err=8990)
     &        alt_n_wo, n_wo, iabs(alt_n_wo-n_wo)
         call ems_msg_wr_li(warn_msg_n)
      endif
      if (r_cf .eq. 0 .and. c_cf .eq. 0 .and. a_el_cf .eq. 0) then
         if (alw_f7_wr) write(ems_li, 9020, err=8990)
     &        n_wo, blk_id_txt(lc_blk_id), n_fr_wo
      else if (r_cf .gt. 99 .or. cs .gt. 9999) then
         if (alw_f7_wr) write(ems_li, 9010, err=8990)
     &        r_cf, n_r, cs,
     &        n_wo, blk_id_txt(lc_blk_id), n_fr_wo
      else
         if (alw_f7_wr) write(ems_li, 9000, err=8990)
     &        r_cf, n_r, c_cf, n_c, a_el_cf, n_a_el, cs,
     &        n_wo, blk_id_txt(lc_blk_id), n_fr_wo
      endif
      if (rp_cn .lt. 0) then
         call ems_msg_wr_li(info_msg_n)
      else
         if (alw_f7_wr) then
            call ems_msg_wr_li_t_cn(ca_mem_mgr_rt_cod, rp_cn)
            if (ca_mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
               mem_mgr_rt_cod = max(ca_mem_mgr_rt_cod,
     &              mem_mgr_rt_cod)
               if (mem_mgr_rt_cod .ge.
     &              mem_mgr_rt_lvl_serious) goto 7100
            endif
         endif
      endif
c 7000 continue
 7100 continue
      return
 8000 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_no_ca_iz,
     &     mem_mgr_rt_cod)
      goto 7100
 8990 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_serious_f7_wr_er,
     &     mem_mgr_rt_cod)
      goto 7100
 9000 format(
     &     i2, '*', i7, '(rows) +',
     &     i2, '*', i7, '(cols) +',
     &     i2, '*', i7, '(a_el) +',
     &     i4, ' = ', i8, ' integer words for ',
     &     a32, ':', i8, ' free words remain')
 9010 format(30x,
     &     i4, '*', i7, '(rows) +',
     &     i8, ' = ', i8, ' integer words for ',
     &     a32, ':', i8, ' free words remain')
 9020 format(30x, 31x,
     &     i8, ' integer words for ',
     &     a32, ':', i8, ' free words remain')
 9500 format('alt_n_wo .ne. n_wo: ', 3(1x, i9))
      end
 
C->>> ---------------------------------------> ems_mem_mgr_rp_ws_use <<<
c     Report the workspace usage.
c
c     If rp_cn is negative then messages are put out via ems_msg_wr_li,
c     otherwise, they are put out on rp_cn.
c
c     Possible return code values set in this routine:
c     -----------------------------------------------
c
c     Routines called:
c     ---------------
c
      subroutine ems_mem_mgr_rp_ws_use(mem_mgr_rt_cod, is, rp_cn,
     &     ws_f_ix, ws_wo_z, ws_nm)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSMMGRI.INC'
      include 'EMSMSG.INC'
      integer mem_mgr_rt_cod, is(0:*), rp_cn, ws_f_ix, ws_wo_z
      character*(*) ws_nm
      integer f_fr_p, l_fr_p, l_mgr_p, n_aux_fr_wo
      integer rt_cod
      logical alw_f7_wr
 
      mem_mgr_rt_cod = mem_mgr_rt_cod_ok
      alw_f7_wr = ems_msg_no_prt_fm .ge. 1 .or. rp_cn .ge. 0
      f_fr_p = (is(ix_f_fr_p)+(ws_wo_z-1))/ws_wo_z + ws_f_ix
      l_fr_p = (is(ix_l_fr_p)-(ws_wo_z-1))/ws_wo_z + ws_f_ix
      l_mgr_p = (is(ix_l_mgr_p)-(ws_wo_z-1))/ws_wo_z + ws_f_ix
      n_aux_fr_wo = (is(ix_n_fr_wo) - (l_fr_p-f_fr_p+1))/ws_wo_z
      if (l_fr_p .lt. l_mgr_p) then
         if (alw_f7_wr) write(ems_li, 9000, err=8990)
     &        ws_f_ix, f_fr_p-1, l_fr_p+1, l_mgr_p, ws_nm
      else
         if (alw_f7_wr) write(ems_li, 9001, err=8990)
     &        ws_f_ix, f_fr_p-1, ws_nm
      endif
      if (rp_cn .lt. 0) then
         call ems_msg_wr_li(info_msg_n)
      else
         if (alw_f7_wr) then
            call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
            if (rt_cod .ne. 0) goto 8990
         endif
      endif
      if (alw_f7_wr) write(ems_li, 9010, err=8990)
     &     l_fr_p-f_fr_p+1, n_aux_fr_wo
      if (rp_cn .lt. 0) then
         call ems_msg_wr_li(info_msg_n)
      else
         if (alw_f7_wr) then
            call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
            if (rt_cod .ne. 0) goto 8990
         endif
      endif
 7100 continue
      return
c 8000 continue
c      mem_mgr_rt_cod = max(mem_mgr_rt_cod_no_ca_iz,
c     &     mem_mgr_rt_cod)
c      goto 7100
 8990 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_serious_f7_wr_er,
     &     mem_mgr_rt_cod)
      goto 7100
 9000 format('Memory manager is using entries ', i1, ' to', i9,
     &     ' and', i9, ' to', i9, ' of the ', a, ' workspace')
 9001 format('Memory manager is using entries ', i1, ' to', i9,
     &     ' of the ', a, ' workspace')
 9010 format('There are ', i9,
     &     ' contiguous free entries and a further ', i9,
     &     ' between blocks')
      end
 
C->>> -----------------------------------> ems_mem_mgr_rp_rq_ws_n_en <<<
c     Reports the required and current number of workspace entries.
c
c     If rp_cn is negative then messages are put out via ems_msg_wr_li,
c     otherwise, they are put out on rp_cn.
c
c     Possible return code values set in this routine:
c     -----------------------------------------------
c
c     Routines called:
c     ---------------
c
      subroutine ems_mem_mgr_rp_rq_ws_n_en(mem_mgr_rt_cod, is, rp_cn,
     &     cu_rq_n_wo, mx_rq_n_wo, ws_wo_z, ws_nm)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSMMGRI.INC'
      include 'EMSMSG.INC'
      integer mem_mgr_rt_cod, is(0:*), rp_cn
      integer cu_rq_n_wo, mx_rq_n_wo, ws_wo_z
      character*(*) ws_nm
      integer cu_ws_n_en, cu_rq_ws_n_en, mx_rq_ws_n_en
      integer rt_cod
      logical alw_f7_wr
 
      mem_mgr_rt_cod = mem_mgr_rt_cod_ok
      alw_f7_wr = ems_msg_no_prt_fm .ge. 1 .or. rp_cn .ge. 0
      cu_ws_n_en = (is(ix_l_mgr_p)+1)/ws_wo_z
      cu_rq_ws_n_en = (cu_rq_n_wo+ws_wo_z-1)/ws_wo_z
      mx_rq_ws_n_en = (mx_rq_n_wo+ws_wo_z-1)/ws_wo_z
      if (alw_f7_wr) write(ems_li, 9000, err=8990)
     &     ws_nm, cu_rq_ws_n_en, cu_ws_n_en
      if (rp_cn .lt. 0) then
         call ems_msg_wr_li(info_msg_n)
      else
         if (alw_f7_wr) then
            call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
            if (rt_cod .ne. 0) goto 8990
         endif
      endif
      if (alw_f7_wr) write(ems_li, 9010, err=8990)
     &     ws_nm, mx_rq_ws_n_en
      if (rp_cn .lt. 0) then
         call ems_msg_wr_li(info_msg_n)
      else
         if (alw_f7_wr) then
            call ems_msg_wr_li_t_cn(rt_cod, rp_cn)
            if (rt_cod .ne. 0) goto 8990
         endif
      endif
c 7000 continue
 7100 continue
      return
c 8000 continue
c      mem_mgr_rt_cod = max(mem_mgr_rt_cod_no_ca_iz,
c     &     mem_mgr_rt_cod)
c      goto 7100
 8990 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_serious_f7_wr_er,
     &     mem_mgr_rt_cod)
      goto 7100
 9000 format('Number of ', a,
     &     ' workspace entries required is (at least) ', i9,
     &     '. Number of entries is currently ', i9)
 9010 format('Maximum number of ', a,
     &     ' workspace entries required so far is ', i9)
      end
 
C=======================================================================
c     Internally-called integer functions.
c
C->>> ---------------------------------------> ems_mem_mgr_g_n_fr_wo <<<
c     Get the number of free words.
c
c     Possible return code values set in this routine:
c     -----------------------------------------------
c
c     Routines called:
c     ---------------
c
      integer function ems_mem_mgr_g_n_fr_wo(mem_mgr_rt_cod, is)
      implicit none
      include 'EMSMMGR.INC'
      include 'EMSMMGRI.INC'
      include 'EMSMSG.INC'
      integer mem_mgr_rt_cod, is(0:*)
      integer n_fr_wo, p_blk_da, os
      logical ems_mem_mgr_no_ca_iz
 
      mem_mgr_rt_cod = mem_mgr_rt_cod_ok
      n_fr_wo = 0
      if (ems_mem_mgr_no_ca_iz(is)) goto 8000
      n_fr_wo = is(ix_l_mgr_p) - is(ix_f_mgr_p) + 1
      p_blk_da = is(ix_p_blk_da)
      do 10, os = 0, (is(ix_mx_n_blk)-1)*blk_rec_z, blk_rec_z
         if (is(p_blk_da+os+os_blk_is_p) .gt. 0)
     &        n_fr_wo = n_fr_wo - is(p_blk_da+os+os_blk_n_wo)
 10   continue
      if (n_fr_wo .ne. is(ix_n_fr_wo)) goto 8010
c 7000 continue
 7100 continue
      ems_mem_mgr_g_n_fr_wo = n_fr_wo
      return
 8000 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_no_ca_iz,
     &     mem_mgr_rt_cod)
      goto 7100
 8010 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_n_fr_wo_er,
     &     mem_mgr_rt_cod)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801, err=8990)
     &     is(ix_n_fr_wo), n_fr_wo
      call ems_msg_wr_li(bug_msg_n)
      goto 7100
 8990 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_serious_f7_wr_er,
     &     mem_mgr_rt_cod)
      goto 7100
 9801 format('Mismatch between updated number of free words ', i8,
     &     ' and calculated number of free words ', i8)
      end
 
C->>> --------------------------------------> ems_mem_mgr_g_fr_blk_n <<<
c     Return a free block number. If none is free then the block of
c     block data is expanded to allow double the number of records.
c
c     Possible return code values set in this routine:
c     -----------------------------------------------
c
c     Routines called:
c     ---------------
c
      integer function ems_mem_mgr_g_fr_blk_n(mem_mgr_rt_cod, is)
      implicit none
      include 'EMSMMGR.INC'
      include 'EMSMMGRI.INC'
      include 'EMSMSG.INC'
      integer is(0:*), mem_mgr_rt_cod
      integer blk_n_wo, fr_blk_n, blk_n
      integer blk_n_fr_ls_n_en
      integer p_blk_da, en_n
      integer ca_mem_mgr_rt_cod
      integer prev_mx_n_blk, n_xa_blk, nw_mx_n_blk
CM      IF (emsol_deb .EQ. 1) THEN
C?      logical fr_blk_er
CM      ENDIF
 
      mem_mgr_rt_cod = mem_mgr_rt_cod_ok
c
c     Asign a value to fr_blk_n since it is copied into
c     ems_mem_mgr_g_fr_blk_n even if there is an error.
c
      fr_blk_n = 0
c
c     Check that the block move lock is not on.
c
      if (is(ix_blk_mv_lck) .ne. 0) goto 8010
c
c     Try to take the block number from the list of free blocks.
c
      blk_n_fr_ls_n_en = is(ix_blk_n_fr_ls_n_en)
      if (blk_n_fr_ls_n_en .gt. 0) then
         fr_blk_n = is(p_blk_n_fr_ls+blk_n_fr_ls_n_en)
         is(ix_blk_n_fr_ls_n_en) = blk_n_fr_ls_n_en - 1
         go to 7000
      endif
c
c     The free list is empty. Try to create a new free list.
c
      if (is(ix_n_fr_blk_n) .gt. 0) then
         blk_n_fr_ls_n_en =
     &        min(is(ix_n_fr_blk_n)-1, mx_n_blk_n_fr_ls_en)
         en_n = blk_n_fr_ls_n_en
         fr_blk_n = 0
         p_blk_da = is(ix_p_blk_da)
         do 10, blk_n = 1, is(ix_mx_n_blk)
            if (is(p_blk_da) .eq. 0) then
c
c     The block is free
c
               if (fr_blk_n .eq. 0) then
c
c     Take this as the free block number
c
                  fr_blk_n = blk_n
c
c     If there is only one free block number then the new free list will
c     be empty so return.
c
                  if (blk_n_fr_ls_n_en .eq. 0) go to 7000
               else
c
c     Add this block number to the free list.
c
                  is(p_blk_n_fr_ls+en_n) = blk_n
                  en_n = en_n - 1
                  if (en_n .eq. 0) then
c
c     The free list is full so return.
c
                     is(ix_blk_n_fr_ls_n_en) = blk_n_fr_ls_n_en
                     go to 7000
                  endif
               endif
            endif
            p_blk_da = p_blk_da + blk_rec_z
 10      continue
c
c     Should never get here
c
         goto 8020
      endif
c
c     There are no free blocks.
c
CM      IF (emsol_deb .EQ. 1) THEN
C?c
C?c     Check that there are really no free blocks.
C?c
C?      fr_blk_er = .false.
C?      p_blk_da = is(ix_p_blk_da)
C?      do 20, blk_n = 1, is(ix_mx_n_blk)
C?         if (is(p_blk_da) .eq. 0) then
C?            fr_blk_er = .true.
C?            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9900, err=8990)
C?            call ems_msg_wr_li(bug_msg_n)
C?         endif
C?         p_blk_da = p_blk_da + blk_rec_z
C? 20   continue
C?      if (fr_blk_er) then
C?         mem_mgr_rt_cod = max(mem_mgr_rt_cod_blk_n_fr_ls_er,
C?     &        mem_mgr_rt_cod)
C?         goto 7100
C?      endif
C? 9900 format('Memory manager supposed to have no free blocks ',
C?     &     'but block ', i9, ' is free')
CM      ENDIF
c
c     Double the number of blocks available.
c
      prev_mx_n_blk = is(ix_mx_n_blk)
      n_xa_blk = prev_mx_n_blk
      blk_n_wo = n_xa_blk*blk_rec_z
      call ems_mem_mgr_xp_blk(ca_mem_mgr_rt_cod, is,
     &     is(ix_blk_blk_da), blk_n_wo, .true.)
      if (ca_mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
         if (ca_mem_mgr_rt_cod .eq. mem_mgr_rt_cod_serious_no_po) then
            mem_mgr_rt_cod = mem_mgr_rt_cod_serious_no_po
         else
            mem_mgr_rt_cod = max(ca_mem_mgr_rt_cod, mem_mgr_rt_cod)
         endif
         if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 7100
      endif
c
c     Initialise the block data records for the new block.
c
      nw_mx_n_blk = prev_mx_n_blk + n_xa_blk
      p_blk_da = is(ix_p_blk_da) + prev_mx_n_blk*blk_rec_z
      do 30, blk_n = prev_mx_n_blk+1, nw_mx_n_blk
         is(p_blk_da+os_blk_is_p) = 0
         p_blk_da = p_blk_da + blk_rec_z
 30   continue
      is(ix_mx_n_blk) = nw_mx_n_blk
c
c     Update the first free pointer in the block of block data
c
      p_blk_da = is(ix_p_blk_da) + (is(ix_blk_blk_da)-1)*blk_rec_z
      is(p_blk_da+os_blk_f_fr_p) = is(p_blk_da+os_blk_n_wo)
c
c     Use the first new block number as the free block
c
      fr_blk_n = prev_mx_n_blk + 1
c
c     Record the number of free block numbers and initialise the free
c     list of block numbers
c
      is(ix_n_fr_blk_n) = n_xa_blk - 1
      blk_n_fr_ls_n_en = min(is(ix_n_fr_blk_n), mx_n_blk_n_fr_ls_en)
      do 40, en_n = 1, blk_n_fr_ls_n_en
         blk_n = fr_blk_n + (blk_n_fr_ls_n_en-en_n+1)
         is(p_blk_n_fr_ls+en_n) = blk_n
 40   continue
 7000 continue
 7100 continue
      ems_mem_mgr_g_fr_blk_n = fr_blk_n
      return
 8010 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_blk_mv_lck_on,
     &     mem_mgr_rt_cod)
      goto 7100
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802, err=8990)
     &     is(ix_n_fr_blk_n), blk_n_fr_ls_n_en, en_n
      call ems_msg_wr_li(bug_msg_n)
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_blk_n_fr_ls_er,
     &     mem_mgr_rt_cod)
      goto 7100
 8990 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_serious_f7_wr_er,
     &     mem_mgr_rt_cod)
      goto 7100
 9802 format('Memory manager has ', i9,
     &     ' free blocks so the free list should contain ', i9,
     &     ' entries but ', i9, ' free blocks were not found ')
      end
 
C->>> --------------------------------------> ems_mem_mgr_g_fr_blk_p <<<
c     Return a pointer to (at least) n_wo of free space.
c
c     Possible return code values set in this routine:
c     -----------------------------------------------
c
c     Routines called:
c     ---------------
c
      integer function ems_mem_mgr_g_fr_blk_p(mem_mgr_rt_cod, is,
     &     n_wo, mgr_p, pre_blk_n, nx_blk_n)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSMMGRI.INC'
      include 'EMSMSG.INC'
      integer is(0:*), n_wo, pre_blk_n, nx_blk_n, mem_mgr_rt_cod
      integer mgr_p
      integer ems_mem_mgr_nx_i_n_en_p, ems_mem_mgr_pre_i_n_en_p
      integer pre_blk_p, pre_blk_n_wo, nw_mgr_p
      integer nx_blk_p, fr_blk_p
      integer cu_is_n_en, cu_rq_is_n_en, mx_rq_is_n_en
      integer ca_mem_mgr_rt_cod
 
      mem_mgr_rt_cod = mem_mgr_rt_cod_ok
c
c     Asign a value to fr_blk_p since it is copied into
c     ems_mem_mgr_g_fr_blk_p even if there is an error.
c
      fr_blk_p = 0
c
c     Check that the block move lock is not on.
c
      if (is(ix_blk_mv_lck) .ne. 0) goto 8010
c
c     Find a pointer for the block according to the preferred part of
c     the memory manager.
c
      if (mgr_p .eq. ope_blk_anywhere .or.
     &     mgr_p .eq. ope_blk_at_f_fr_p) then
c
c     Try to find space at the beginning of the manager.
c
         pre_blk_n = -1
         nx_blk_n =  is(ix_f_frt_blk)
         nw_mgr_p =  blk_at_mgr_frt
         fr_blk_p =  ems_mem_mgr_nx_i_n_en_p(is(ix_f_mgr_p), 2)
         if (fr_blk_p+n_wo .le. is(ix_f_blk_p)) go to 7000
c
c     Try to find space after each of the front blocks in turn.
c
         pre_blk_n = is(ix_f_frt_blk)
 110     continue
         if (pre_blk_n .eq. is(ix_l_frt_blk)) then
c
c     Try to find space after the last front block.
c
            nx_blk_n = is(ix_f_bac_blk)
            nw_mgr_p = blk_at_mgr_frt
            fr_blk_p = ems_mem_mgr_nx_i_n_en_p(is(ix_f_fr_p), 2)
            if (fr_blk_p+n_wo-1 .le. is(ix_l_fr_p)) go to 7000
            go to 200
         end if
         if (pre_blk_n .le. 0) go to 8020
c
c     Consider the gap between the first pointer not allocated after
c     block pre_blk_n and the last pointer before block nx_blk_n.
c
         pre_blk_p = is(is(ix_p_blk_da)+(pre_blk_n-1)*blk_rec_z+
     &        os_blk_is_p)
         pre_blk_n_wo =
     &        is(is(ix_p_blk_da)+(pre_blk_n-1)*blk_rec_z+os_blk_n_wo)
         nx_blk_n =  is(is(ix_p_blk_da)+(pre_blk_n-1)*blk_rec_z+
     &        os_blk_lkf)
         nx_blk_p =  is(is(ix_p_blk_da)+( nx_blk_n-1)*blk_rec_z+
     &        os_blk_is_p)
         fr_blk_p =  ems_mem_mgr_nx_i_n_en_p(pre_blk_p+pre_blk_n_wo, 2)
c
c     Check that there is enough space for the block.
c     If so then return, otherwise try the gap after the next block ...
c
         if (fr_blk_p+n_wo .le. nx_blk_p) then
            nw_mgr_p = blk_at_mgr_frt
            go to 7000
         else
            pre_blk_n = nx_blk_n
            go to 110
         end if
      end if
 200  continue
      if (mgr_p .eq. ope_blk_anywhere .or.
     &     mgr_p .eq. ope_blk_at_l_fr_p) then
c
c     Try to find space the end of the manager.
c
         pre_blk_n = is(ix_l_bac_blk)
         nx_blk_n =  -1
         nw_mgr_p =  blk_at_mgr_bac
         fr_blk_p =  ems_mem_mgr_pre_i_n_en_p(is(ix_l_mgr_p)-n_wo+1, 2)
         if (fr_blk_p .gt. is(ix_l_blk_p)) go to 7000
c
c     Try to find space before each of the back blocks in turn.
c
         nx_blk_n = is(ix_l_bac_blk)
 210     continue
         if (nx_blk_n .eq. is(ix_f_bac_blk)) then
            if (mgr_p .eq. ope_blk_anywhere) go to 300
c
c     Try to find space before the first back block. (This has already
c     been tried if mgr_p = 0)
c
            pre_blk_n = is(ix_l_frt_blk)
            nw_mgr_p = blk_at_mgr_bac
            fr_blk_p = ems_mem_mgr_pre_i_n_en_p(is(ix_l_fr_p)-n_wo+1, 2)
            if (fr_blk_p .ge. is(ix_f_fr_p)) go to 7000
            go to 300
         end if
         if (nx_blk_n .le. 0) go to 8020
c
c     Consider the gap between the last pointer not allocated before
c     block nx_blk_n and the first pointer after block pre_blk_n.
c
         nx_blk_p =  is(is(ix_p_blk_da)+( nx_blk_n-1)*blk_rec_z+
     &        os_blk_is_p)
         pre_blk_n = is(is(ix_p_blk_da)+( nx_blk_n-1)*blk_rec_z+
     &        os_blk_lkb)
         pre_blk_p = is(is(ix_p_blk_da)+(pre_blk_n-1)*blk_rec_z+
     &        os_blk_is_p)
         pre_blk_n_wo = is(is(ix_p_blk_da)+(pre_blk_n-1)*blk_rec_z+
     &        os_blk_n_wo)
         fr_blk_p = ems_mem_mgr_pre_i_n_en_p(nx_blk_p-n_wo, 2)
c
c     Check that there is enough space for the block.
c     If so then return, otherwise try the gap after the next block ...
c
         if (fr_blk_p .ge. pre_blk_p+pre_blk_n_wo) then
            nw_mgr_p = blk_at_mgr_bac
            go to 7000
         else
            nx_blk_n = pre_blk_n
            go to 210
         end if
      end if
c
c     There are no spaces big enough without moving blocks.
c
 300  continue
      if (mgr_p .eq. ope_blk_anywhere .or.
     &     mgr_p .eq. ope_blk_at_f_fr_p) then
c
c     Try compressing the front of the manager.
c
         call ems_mem_mgr_css(ca_mem_mgr_rt_cod, is, 1)
         if (ca_mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
            mem_mgr_rt_cod = max(ca_mem_mgr_rt_cod, mem_mgr_rt_cod)
            if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 7100
         endif
         fr_blk_p = ems_mem_mgr_nx_i_n_en_p(is(ix_f_fr_p), 2)
         pre_blk_n = is(ix_l_frt_blk)
         nx_blk_n =  is(ix_f_bac_blk)
         nw_mgr_p = blk_at_mgr_frt
         if (fr_blk_p+n_wo-1 .le. is(ix_l_fr_p)) go to 7000
      end if
c
c     Try compressing the back of the manager.
c
      call ems_mem_mgr_css(ca_mem_mgr_rt_cod, is, 2)
      if (ca_mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
         mem_mgr_rt_cod = max(ca_mem_mgr_rt_cod, mem_mgr_rt_cod)
         if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 7100
      endif
      fr_blk_p = ems_mem_mgr_nx_i_n_en_p(is(ix_f_fr_p), 2)
      pre_blk_n = is(ix_l_frt_blk)
      nx_blk_n =  is(ix_f_bac_blk)
      nw_mgr_p = blk_at_mgr_bac
      if (fr_blk_p+n_wo-1 .le. is(ix_l_fr_p)) go to 7000
      if (mgr_p .eq. ope_blk_at_l_fr_p) then
c
c     Try compressing the front of the manager (if it has not already
c     been done).
c
         call ems_mem_mgr_css(ca_mem_mgr_rt_cod, is, 1)
         if (ca_mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
            mem_mgr_rt_cod = max(ca_mem_mgr_rt_cod, mem_mgr_rt_cod)
            if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 7100
         endif
         fr_blk_p = ems_mem_mgr_nx_i_n_en_p(is(ix_f_fr_p), 2)
         pre_blk_n = is(ix_l_frt_blk)
         nx_blk_n =  is(ix_f_bac_blk)
         nw_mgr_p = blk_at_mgr_bac
         if (fr_blk_p+n_wo-1 .le. is(ix_l_fr_p)) go to 7000
      end if
c
c     The memory manager is fully compressed and the gap between the
c     front and back is not wide enough for the space required so we
c     have to give up.
c
      go to 8030
 7000 continue
      mgr_p = nw_mgr_p
 7100 continue
      ems_mem_mgr_g_fr_blk_p = fr_blk_p
      return
 8010 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_blk_mv_lck_on,
     &     mem_mgr_rt_cod)
      goto 7100
 8020 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_int_er,
     &     mem_mgr_rt_cod)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802, err=8990)
      call ems_msg_wr_li(serious_msg_n)
      go to 7100
 8030 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_serious_no_po,
     &     mem_mgr_rt_cod)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9803, err=8990)
      call ems_msg_wr_li(warn_msg_n)
      call ems_mem_mgr_g_rq_is_n_en(ca_mem_mgr_rt_cod, is,
     &     n_wo, cu_rq_is_n_en, mx_rq_is_n_en)
      if (ca_mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
         mem_mgr_rt_cod = max(ca_mem_mgr_rt_cod, mem_mgr_rt_cod)
         if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 7100
      endif
      call ems_mem_mgr_rp_rq_is_n_en(ca_mem_mgr_rt_cod, is,
     &      -1, cu_rq_is_n_en, mx_rq_is_n_en)
      if (ca_mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
         mem_mgr_rt_cod = max(ca_mem_mgr_rt_cod, mem_mgr_rt_cod)
         if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 7100
      endif
      cu_is_n_en = is(ix_l_mgr_p) + 1
      is(ix_n_xa_i_wo_rq) = max(cu_rq_is_n_en-cu_is_n_en, 1)
      go to 7100
 8990 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_serious_f7_wr_er,
     &     mem_mgr_rt_cod)
      goto 7100
 9802 format('Algorithmic error in ems_mem_mgr_g_fr_blk_p')
 9803 format('Insufficient space despite fully compressing the mem',
     &     'ory manager ')
      end
 
C->>> --------------------------------------------> ems_mem_mgr_g_p1 <<<
c     Find the pointer into 'ns' for hdl.
c
c     Possible return code values set in this routine:
c     -----------------------------------------------
c
c     Routines called:
c     ---------------
c
      subroutine ems_mem_mgr_g_p1(mem_mgr_rt_cod, is, hdl, p)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSMMGRI.INC'
      include 'EMSMSG.INC'
      integer mem_mgr_rt_cod, is(0:*), hdl(0:hdl_z_m1), p
      logical ems_mem_mgr_no_ca_iz
 
      mem_mgr_rt_cod = mem_mgr_rt_cod_ok
      p = no_p
      if (ems_mem_mgr_no_ca_iz(is)) goto 8000
      if (hdl(hdl_os_blk_n) .le. 0) goto 8010
      if (hdl(hdl_os_wo_z) .ne. ch_wo_z) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9400, err=8990)
     &        hdl(hdl_os_wo_z)
         call ems_msg_wr_li(warn_msg_n)
         mem_mgr_rt_cod = max(mem_mgr_rt_cod_g_p_wg_wo_z,
     &        mem_mgr_rt_cod)
      endif
      p = (is(is(ix_p_blk_da)+
     &     (hdl(hdl_os_blk_n)-1)*blk_rec_z+os_blk_is_p) +
     &     hdl(hdl_os_blk_os))/ch_wo_z
      hdl(hdl_os_p) = p
      hdl(hdl_os_blk_mv_k) = is(ix_blk_mv_k)
c 7000 continue
 7100 continue
      return
 8000 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_no_ca_iz,
     &     mem_mgr_rt_cod)
      goto 7100
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801, err=8990)
     &     hdl(hdl_os_blk_n)
      call ems_msg_wr_li(serious_msg_n)
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_g_p_no_blk,
     &     mem_mgr_rt_cod)
      goto 7100
 8990 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_serious_f7_wr_er,
     &     mem_mgr_rt_cod)
      goto 7100
 9400 format('Calculating ns pointer for handle with word size ', i1)
 9801 format('Calculating ns pointer for handle with block number ', i9)
      end
 
C->>> --------------------------------------------> ems_mem_mgr_g_p4 <<<
c     Find the pointer into 'is' for hdl.
c
c     Possible return code values set in this routine:
c     -----------------------------------------------
c
c     Routines called:
c     ---------------
c
      subroutine ems_mem_mgr_g_p4(mem_mgr_rt_cod, is, hdl, p)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSMMGRI.INC'
      include 'EMSMSG.INC'
      integer mem_mgr_rt_cod, is(0:*), hdl(0:hdl_z_m1), p
      logical ems_mem_mgr_no_ca_iz
 
      mem_mgr_rt_cod = mem_mgr_rt_cod_ok
      p = no_p
      if (ems_mem_mgr_no_ca_iz(is)) goto 8000
      if (hdl(hdl_os_blk_n) .le. 0) goto 8010
      if (hdl(hdl_os_wo_z) .ne. i_wo_z) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9400, err=8990)
     &        hdl(hdl_os_wo_z)
         call ems_msg_wr_li(warn_msg_n)
         mem_mgr_rt_cod = max(mem_mgr_rt_cod_g_p_wg_wo_z,
     &        mem_mgr_rt_cod)
      endif
      p = (is(is(ix_p_blk_da)+
     &     (hdl(hdl_os_blk_n)-1)*blk_rec_z+os_blk_is_p) +
     &     hdl(hdl_os_blk_os))/i_wo_z
      hdl(hdl_os_p) = p
      hdl(hdl_os_blk_mv_k) = is(ix_blk_mv_k)
c 7000 continue
 7100 continue
      return
 8000 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_no_ca_iz,
     &     mem_mgr_rt_cod)
      goto 7100
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801, err=8990)
     &        hdl(hdl_os_blk_n)
      call ems_msg_wr_li(serious_msg_n)
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_g_p_no_blk,
     &     mem_mgr_rt_cod)
      goto 7100
 8990 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_serious_f7_wr_er,
     &     mem_mgr_rt_cod)
      goto 7100
 9400 format('Calculating is pointer for handle with word size ', i1)
 9801 format('Calculating is pointer for handle with block number ', i9)
      end
 
C->>> --------------------------------------------> ems_mem_mgr_g_p8 <<<
c     Find the pointer into 'ds' for hdl.
c
c     Possible return code values set in this routine:
c     -----------------------------------------------
c
c     Routines called:
c     ---------------
c
      subroutine ems_mem_mgr_g_p8(mem_mgr_rt_cod, is, hdl, p)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSMMGRI.INC'
      include 'EMSMSG.INC'
      integer mem_mgr_rt_cod, is(0:*), hdl(0:hdl_z_m1), p
      logical ems_mem_mgr_no_ca_iz
 
      mem_mgr_rt_cod = mem_mgr_rt_cod_ok
      p = no_p
      if (ems_mem_mgr_no_ca_iz(is)) goto 8000
      if (hdl(hdl_os_blk_n) .le. 0) goto 8010
      if (hdl(hdl_os_wo_z) .ne. rl_wo_z) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9400, err=8990)
     &        hdl(hdl_os_wo_z)
         call ems_msg_wr_li(warn_msg_n)
         mem_mgr_rt_cod = max(mem_mgr_rt_cod_g_p_wg_wo_z,
     &        mem_mgr_rt_cod)
      endif
      p = (is(is(ix_p_blk_da)+
     &     (hdl(hdl_os_blk_n)-1)*blk_rec_z+os_blk_is_p) +
     &     hdl(hdl_os_blk_os))/rl_wo_z
      hdl(hdl_os_p) = p
      hdl(hdl_os_blk_mv_k) = is(ix_blk_mv_k)
c 7000 continue
 7100 continue
      return
 8000 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_no_ca_iz,
     &     mem_mgr_rt_cod)
      goto 7100
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801, err=8990)
     &        hdl(hdl_os_blk_n)
      call ems_msg_wr_li(serious_msg_n)
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_g_p_no_blk,
     &     mem_mgr_rt_cod)
      goto 7100
 8990 continue
      mem_mgr_rt_cod = max(mem_mgr_rt_cod_serious_f7_wr_er,
     &     mem_mgr_rt_cod)
      goto 7100
 9400 format('Calculating ds pointer for handle with word size ', i1)
 9801 format('Calculating ds pointer for handle with block number ', i9)
      end
 
C->>> -------------------------------------> ems_mem_mgr_nx_i_n_en_p <<<
c     Find the pointer to the next wo_z boundary.
c     Returns is_p if wo_z <=0.
c
c     Possible return code values set in this routine:
c     -----------------------------------------------
c
c     Routines called:
c     ---------------
c
      integer function ems_mem_mgr_nx_i_n_en_p(is_p, wo_z)
      implicit none
      integer is_p, wo_z
      if (wo_z .le. 0) then
         ems_mem_mgr_nx_i_n_en_p = is_p
      else
         ems_mem_mgr_nx_i_n_en_p = ((is_p+wo_z-1)/wo_z)*wo_z
      endif
      return
      end
 
C->>> ------------------------------------> ems_mem_mgr_pre_i_n_en_p <<<
c     Find the pointer to the previous wo_z boundary.
c     Returns is_p if wo_z <=0.
c
c     Possible return code values set in this routine:
c     -----------------------------------------------
c
c     Routines called:
c     ---------------
c
      integer function ems_mem_mgr_pre_i_n_en_p(is_p, wo_z)
      implicit none
      integer is_p, wo_z
      if (wo_z .le. 0) then
         ems_mem_mgr_pre_i_n_en_p = is_p
      else
         ems_mem_mgr_pre_i_n_en_p = (is_p/wo_z)*wo_z
      endif
      return
      end
 
C->>> ----------------------------------------> ems_mem_mgr_no_ca_iz <<<
c     Returns true of the memory manager has not been initialised.
c
c     Possible return code values set in this routine:
c     -----------------------------------------------
c
c     Routines called:
c     ---------------
c
      logical function ems_mem_mgr_no_ca_iz(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSMMGRI.INC'
      integer is(0:*)
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_eq_i2
CM      ENDIF
      logical no_ca_iz
 
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      no_ca_iz = ems_i1_eq_i2(
C?     &     is(ix_ca_mem_mgr_iz_fg1),
C?     &     is(ix_ca_mem_mgr_iz_fg2))
CM      ELSE
      no_ca_iz =
     &     is(ix_ca_mem_mgr_iz_fg1) .eq.
     &     is(ix_ca_mem_mgr_iz_fg2)
CM      ENDIF
      ems_mem_mgr_no_ca_iz = no_ca_iz
      return
      end
 
C=======================================================================
c     Dummy routines.
c
C->>> -----------------------------------------> ems_se_emsol_p_no_p <<<
c     Set the EMSOL pointers to be `no pointers'.
c
c      subroutine ems_se_emsol_p_no_p(rt_cod, is)
c      implicit none
c      integer rt_cod, is(0:*)
c      rt_cod = 0
c      return
c      end
c
C->>> -----------------------------------------------> ems_g_emsol_p <<<
c     Get the EMSOL pointers.
c
c      subroutine ems_g_emsol_p(rt_cod, is)
c      implicit none
c      integer rt_cod, is(0:*)
c      rt_cod = 0
c      return
c      end
C->>> -------------------------------------------> ems_se_usr_p_no_p <<<
c     Set the user pointers to be `no pointers'.
c
      subroutine ems_se_usr_p_no_p(rt_cod, is)
      implicit none
      integer rt_cod, is(0:*)
      rt_cod = 0
      return
      end
C->>> -------------------------------------------------> ems_g_usr_p <<<
c     Get the user pointers.
c
      subroutine ems_g_usr_p(rt_cod, is)
      implicit none
      integer rt_cod, is(0:*)
      rt_cod = 0
      return
      end
