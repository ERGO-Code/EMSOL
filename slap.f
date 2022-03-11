C->>> ---------------------------------------------> ems_clo_eta_grp <<<
c     Closes the eta group given by hdl_eta_grp by removing gaps
c     between arrays and reassigning the first free pointer in the
c     corresponding block.
c
      subroutine ems_clo_eta_grp(eta_grp, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'SLAPCS.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer eta_grp(0:mx_eta_grp_rec_z-1), is(0:is_n_en_m1)
      integer p_v, p_ix, p_rec, p
      integer n_v, n_ix, n_rec
      integer mem_mgr_rt_cod
 
      n_v = eta_grp(eta_grp_os_n_v)
      n_ix = eta_grp(eta_grp_os_n_ix)
      n_rec = eta_grp(eta_grp_os_n_rec)
      eta_grp(eta_grp_os_mx_n_v) = n_v
      eta_grp(eta_grp_os_mx_n_ix) = n_ix
      eta_grp(eta_grp_os_mx_n_rec) = n_rec
      call ems_mem_mgr_g_p8(mem_mgr_rt_cod, is,
     &     eta_grp(eta_grp_os_hdl_v), p_v)
      if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) goto 8800
      call ems_mem_mgr_g_p4(mem_mgr_rt_cod, is,
     &     eta_grp(eta_grp_os_hdl_ix), p_ix)
      if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) goto 8800
      call ems_mem_mgr_g_p4(mem_mgr_rt_cod, is,
     &     eta_grp(eta_grp_os_hdl_rec), p_rec)
      if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) goto 8800
c
c     Shift the eta indices to cover the gap after the values.
c     Update the handle for the indices.
c
      p = p_v + rl_wo_z*(1+n_v)
      if (p .lt. p_ix) then
         call ems_cp_i_a1(1+n_ix, is(p_ix), is(p))
         eta_grp(eta_grp_os_hdl_ix + hdl_os_blk_os) =
     &        eta_grp(eta_grp_os_hdl_ix + hdl_os_blk_os) - (p_ix-p)
         eta_grp(eta_grp_os_hdl_ix + hdl_os_p) = p
      end if
      p = p + i_wo_z*(1+n_ix)
c
c     Shift the eta records to cover the gap after the indices.
c     Update the handle for the records.
c
      if (p .lt. p_rec) then
         call ems_cp_i_a1(1+n_rec, is(p_rec), is(p))
         eta_grp(eta_grp_os_hdl_rec + hdl_os_blk_os) =
     &        eta_grp(eta_grp_os_hdl_rec + hdl_os_blk_os) - (p_rec-p)
         eta_grp(eta_grp_os_hdl_rec + hdl_os_p) = p
      end if
      p = p + i_wo_z*(1+n_rec)
c
c     Close the block.
c
      call ems_mem_mgr_clo_blk(mem_mgr_rt_cod, is,
     &     1+n_rec, i_wo_z, eta_grp(eta_grp_os_hdl_rec))
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 8800
c 7000 continue
 7100 continue
      return
 8800 continue
      ems_msg_cod = ems_msg_lvl_serious
      goto 7100
      end
 
C->>> -------------------------------------------> ems_ope_u_eta_grp <<<
c     Opens an eta group with handle hdl_eta_grp for updates.
c
      subroutine ems_ope_u_eta_grp(hdl_eta_grp, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'SLAPCS.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      include 'EMSMMGRI.INC'
      integer hdl_eta_grp(0:hdl_z_m1), is(0:is_n_en_m1)
      double precision ds(0:ds_n_en_m1)
      integer prev_mx_n_u
      integer n_wo, n_fr_wo
      integer cu_is_n_en, cu_rq_is_n_en, mx_rq_is_n_en
      logical no_po
c      integer rt_cod
      integer mem_mgr_rt_cod
 
 100  continue
      call ems_ope_eta_grp(
     &     no_po, n_wo, n_fr_wo,
     &     hdl_eta_grp,
     &     rsmi_eta_grp_ty, sto_pk_eta_v, sto_u_eta_se_ty,
     &     mx_n_u, -1, ds, is)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
 
 
      if (no_po) then
c
c     ?? Should be able to see if any data such as other INVERTS can be
c     deleted and the memory manager re-compressed rather than returning
c     a serious error.
c
         if (lo_sto_mode .eq. 0 .or. mx_n_u .le. 1) go to 8000
         prev_mx_n_u = mx_n_u
         mx_n_u = n_fr_wo*mx_n_u/n_wo
         if (mx_n_u .ge. prev_mx_n_u) goto 8990
         if (mx_n_u .le. 1) go to 8000
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &        n_si_it, mx_n_u
         call ems_msg_wr_li(warn_msg_n)
         usr_mx_n_u = mx_n_u
         goto 100
      endif
 7000 continue
 7100 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)n_si_it
      call ems_msg_wr_li(serious_msg_n)
      call ems_mem_mgr_g_rq_is_n_en(mem_mgr_rt_cod, is,
     &     n_wo, cu_rq_is_n_en, mx_rq_is_n_en)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 7100
      call ems_mem_mgr_rp_rq_ws_n_en(mem_mgr_rt_cod, is, -1,
     &     cu_rq_is_n_en, mx_rq_is_n_en, rl_wo_z, 'double precision')
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) goto 7100
      cu_is_n_en = is(ix_l_mgr_p) + 1
      is(ix_n_xa_i_wo_rq) = max(cu_rq_is_n_en-cu_is_n_en, 1)
      goto 7100
 8990 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9899)n_wo, n_fr_wo,
     &     prev_mx_n_u, mx_n_u
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9000 format('Iteration ', i7,
     &     ': Reducing the maximum number of updates to ', i3,
     &     ' in order to conserve space')
 9800 format('Iteration ', i7,
     &     ': Insufficient space for UPDATE')
 9899 format('Error adjusting mx_n_u: ', 4(2x, i9))
      end
 
C->>> ---------------------------------------------> ems_ope_eta_grp <<<
c     Returns the handle of a new eta group with space for full length
c     or packed values according to sto_eta_v
c
c     Allows space for:
c     usr_mx_n_eta etas (if >=0) or mx_n_u.
c     usr_mx_n_el  entries (if >=0) or mx_n_eta*n_r*f(av_eta_dse).
c
c     If the values are stored in long vectors then mx_n_el refers to
c     the space allocated to the indices.
c
      subroutine ems_ope_eta_grp(
     &     no_po, n_wo, n_fr_wo,
     &     hdl_eta_grp,
     &     eta_grp_ty, sto_eta_v, sto_eta_se_ty,
     &     usr_mx_n_eta, usr_mx_n_el, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'SLAPCS.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      include 'EMSMMGRI.INC'
      logical no_po
      integer n_wo, n_fr_wo
      integer hdl_eta_grp(0:hdl_z_m1)
      integer eta_grp_ty, sto_eta_v, sto_eta_se_ty
      integer usr_mx_n_eta, usr_mx_n_el
      integer is(0:is_n_en_m1)
      double precision ds(0:ds_n_en_m1)
      integer p_eta_v0, p_eta_ix0
      integer mx_n_eta, mx_n_el, blk_n, os
      integer p_eta_grp, p_eta_se_rec
      integer mem_mgr_rt_cod
 
      if (usr_mx_n_eta .lt. 0) then
         mx_n_eta = mx_n_u
      else
         mx_n_eta = usr_mx_n_eta
      end if
      if (usr_mx_n_el .lt. 0) then
         if (av_eta_dse .le. 0d0) goto 8010
         mx_n_el = mx_n_eta*n_r*min(one, two*av_eta_dse) + mx_n_r
      else
         mx_n_el = usr_mx_n_el
      end if
      if (sto_eta_v .eq. sto_pk_eta_v .and. (
     &     sto_eta_se_ty .eq. full_c_eta_se_ty .or.
     &     sto_eta_se_ty .eq. full_r_eta_se_ty)) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9400)
         call ems_msg_wr_li(warn_msg_n)
      endif
      call ems_g_eta_grp_n_wo(
     &     eta_grp_ty, sto_eta_v, mx_n_eta, mx_n_el, n_wo)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
c      if (usr_mx_n_el .ge. 0) then
c         write(*, '(a, 3i9, 2x, f5.2)')
c     &        '####INVERT####: ', mx_n_eta, mx_n_el, n_wo
c      else
c         write(*, '(a, 3i9, 2x, f5.2)')
c     &        '####UPDATE####: ', mx_n_eta, mx_n_el, n_wo
c      endif
      n_fr_wo = is(ix_n_fr_wo)
      no_po = n_wo .gt. n_fr_wo
      if (no_po) goto 7000
c
c     Open a new block to store the eta group
c
      call ems_mem_mgr_ope_blk(mem_mgr_rt_cod, is,
     &     n_wo, ope_blk_at_f_fr_p,
     &     cu_ml_n, eta_grp_blk_id, blk_n)
      if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) then
         if (mem_mgr_rt_cod .eq. mem_mgr_rt_cod_serious_no_po) then
            ems_msg_cod = ems_msg_lvl_serious
            go to 7000
         else if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
             ems_msg_cod = ems_msg_lvl_serious
            go to 7000
         endif
      endif
c
c     Let the handle for the eta group account for the whole block.
c
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, n_wo, i_wo_z, hdl_eta_grp)
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
c
c     Initialise the eta group record.
c
      os = hdl_eta_grp(hdl_os_blk_os)
      call ems_mem_mgr_g_p4(mem_mgr_rt_cod, is,
     &     hdl_eta_grp, p_eta_grp)
      if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) goto 8800
      call ems_iz_eta_grp_rec(
     &     eta_grp_ty,
     &     sto_eta_v,
     &     mx_n_eta, mx_n_el,
     &     blk_n,
     &     os,
     &     is(p_eta_grp))
      call ems_mem_mgr_g_p4(mem_mgr_rt_cod, is,
     &     is(p_eta_grp+eta_grp_os_hdl_rec), p_eta_se_rec)
      if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) goto 8800
      is(p_eta_se_rec) = no_eta_se_ty
      is(p_eta_se_rec+eta_se_rec_os_eta_ty) = no_eta_se_ty
      is(p_eta_grp + eta_grp_os_n_rec) = 1
c
c     Assign a value to the zero'th value and index to avoid an
c     unassigned variable violation in cz_r.
c
      call ems_mem_mgr_g_p8(mem_mgr_rt_cod, is,
     &     is(p_eta_grp + eta_grp_os_hdl_v), p_eta_v0)
      if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) goto 8800
      call ems_mem_mgr_g_p4(mem_mgr_rt_cod, is,
     &     is(p_eta_grp + eta_grp_os_hdl_ix), p_eta_ix0)
      if (mem_mgr_rt_cod .ne. mem_mgr_rt_cod_ok) goto 8800
      ds(p_eta_v0) = zero
      is(p_eta_ix0) = 0
 7000 continue
 7100 continue
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)av_eta_dse
      call ems_msg_wr_li(bug_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?      call ems_dump
CM      ENDIF
      go to 7000
 8800 continue
      ems_msg_cod = ems_msg_lvl_serious
      goto 7100
 9400 format('Allocating space for packed eta values',
     &     ' but eta set type is for storage of full vectors')
 9802 format('Invalid running average eta density of ', g11.4)
      end
 
C->>> --------------------------------------------> ems_iz_inv_bs_bt <<<
      subroutine ems_iz_inv_bs_bt(st, vr_in_r)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      integer st(0:mx_n_c+n_r), vr_in_r(0:n_r)
      integer c_n, r_n, vr_n
 
      do 10, c_n = 1, n_c
         st(c_n) = st(c_n) - iand(st(c_n), inv_bs_bt)
 10   continue
      do 15, r_n = mx_n_c+1, mx_n_c+n_r
         st(r_n) = st(r_n) - iand(st(r_n), inv_bs_bt)
 15   continue
      do 20, r_n = 1, n_r
         vr_n = vr_in_r(r_n)
         st(vr_n) = st(vr_n) + inv_bs_bt
 20   continue
      return
      end
 
C->>> ------------------------------------------> ems_iz_eta_grp_rec <<<
      subroutine ems_iz_eta_grp_rec(
     &     eta_grp_ty,
     &     sto_eta_v,
     &     mx_n_eta,
     &     mx_n_el,
     &     blk_n,
     &     os,
     &     eta_grp)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'SLAPCS.INC'
      include 'ICTVR.INC'
      integer eta_grp_ty, sto_eta_v, mx_n_eta, mx_n_el, blk_n, os
      integer eta_grp(0:mx_eta_grp_rec_z-1)
      integer ems_eta_grp_rec_z
      integer mx_n_v, mx_n_ix, mx_n_rec
 
      call ems_g_eta_grp_a_z(
     &     eta_grp_ty, sto_eta_v, mx_n_eta, mx_n_el,
     &     mx_n_v, mx_n_ix, mx_n_rec)
c
c     Initialise the eta type, the maximum and actual numbers of etas
c     and entries stored.
c
      eta_grp(eta_grp_os_eta_grp_ty) = eta_grp_ty
      eta_grp(eta_grp_os_sto_eta_v) = sto_eta_v
      eta_grp(eta_grp_os_mx_n_eta) = mx_n_eta
      eta_grp(eta_grp_os_mx_n_el) = mx_n_el
      eta_grp(eta_grp_os_f_si_it_n) = 0
      eta_grp(eta_grp_os_n_eta) = 0
      eta_grp(eta_grp_os_n_v) = 0
      eta_grp(eta_grp_os_mx_n_v) = mx_n_v
      eta_grp(eta_grp_os_n_ix) = 0
      eta_grp(eta_grp_os_mx_n_ix) = mx_n_ix
      eta_grp(eta_grp_os_n_rec) = 0
      eta_grp(eta_grp_os_mx_n_rec) = mx_n_rec
      os = os + i_wo_z*ems_eta_grp_rec_z(eta_grp_ty)
c
c     Set the handle for the values.
c
      eta_grp(eta_grp_os_hdl_v + hdl_os_blk_n) = blk_n
      eta_grp(eta_grp_os_hdl_v + hdl_os_blk_os) = os
      eta_grp(eta_grp_os_hdl_v + hdl_os_wo_z) = rl_wo_z
      os = os + rl_wo_z*(1+mx_n_v+1)
 
c      if (eta_grp_ty .eq. parlp_eta_grp_ty) then
cc
cc     Set the handle for the real simplex iteration data.
cc
c         eta_grp(eta_grp_os_hdl_rl_si_da + hdl_os_blk_n) = blk_n
c         eta_grp(eta_grp_os_hdl_rl_si_da + hdl_os_blk_os) = os
c         eta_grp(eta_grp_os_hdl_rl_si_da + hdl_os_wo_z) = rl_wo_z
c         os = os + rl_wo_z*(1+rl_si_da_rec_z*mx_n_eta)
c      endif
c
c     Set the handle for the indices.
c
      eta_grp(eta_grp_os_hdl_ix + hdl_os_blk_n) = blk_n
      eta_grp(eta_grp_os_hdl_ix + hdl_os_blk_os) = os
      eta_grp(eta_grp_os_hdl_ix + hdl_os_wo_z) = i_wo_z
      os = os + i_wo_z*(1+mx_n_ix+1)
c
c     Set the handle for the eta_rec.
c
      eta_grp(eta_grp_os_hdl_rec + hdl_os_blk_n) = blk_n
      eta_grp(eta_grp_os_hdl_rec + hdl_os_blk_os) = os
      eta_grp(eta_grp_os_hdl_rec + hdl_os_wo_z) = i_wo_z
      os = os + i_wo_z*(1+mx_n_rec+1)
c      if (eta_grp_ty .eq. parlp_eta_grp_ty) then
cc
cc     Set the handle for the feasibility candidate row numbers integer
cc     simplex iteration data.
cc
c         eta_grp(eta_grp_os_hdl_cdd_r_ix + hdl_os_blk_n) = blk_n
c         eta_grp(eta_grp_os_hdl_cdd_r_ix + hdl_os_blk_os) = os
c         eta_grp(eta_grp_os_hdl_cdd_r_ix + hdl_os_wo_z) = i_wo_z
c         os = os + i_wo_z*(mx_n_eta*(1+n_r/10))
c         eta_grp(eta_grp_os_hdl_i_si_da + hdl_os_blk_n) = blk_n
c         eta_grp(eta_grp_os_hdl_i_si_da + hdl_os_blk_os) = os
c         eta_grp(eta_grp_os_hdl_i_si_da + hdl_os_wo_z) = i_wo_z
c         os = os + i_wo_z*(1+i_si_da_rec_z*mx_n_eta)
c      endif
      return
      end
 
C->>> -------------------------------------------> ems_iz_eta_se_rec <<<
c     Initialise the record for an eta set.
c
c     |    ty    |   n_eta  | sa_(n_eta+1) |  n_eta  |  ty  |
c           1          2            3      |     4   |   5  |
c     |<---eta_se_rec_z---->|<------1----->|<-eta_se_rec_z->|
c
      subroutine ems_iz_eta_se_rec(
     &     eta_se_ty,
     &     eta_grp_n_ix,
     &     eta_se_rec)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      integer eta_se_ty, eta_grp_n_ix
      integer eta_se_rec(1:eta_se_rec_z+1+eta_se_rec_z+1)
      integer p
c
c     Initialise the type and number of etas at the start of the record.
c
      eta_se_rec(eta_se_rec_os_eta_ty) = eta_se_ty
      eta_se_rec(eta_se_rec_os_n_eta) = 0
c
c     Backward offsets are added to the pointer to the last entry in the
c     record.
c
      p = eta_se_rec_z + 1 + eta_se_rec_z
c
c     Record the start for the 1st eta.
c     Initialise the type and number of etas at the end of the record.
c
      eta_se_rec(p-eta_se_rec_z) = eta_grp_n_ix + 1
      eta_se_rec(p+eta_se_rec_bwd_os_n_eta) = 0
      eta_se_rec(p+eta_se_rec_bwd_os_eta_ty) = eta_se_ty
c
c     Initialise the first record in the next set to correspond to a
c     null set.
c
      eta_se_rec(p+eta_se_rec_os_eta_ty) = no_eta_se_ty
      return
      end
 
C->>> --------------------------------------------> ems_u_eta_se_rec <<<
c     Update the record for an eta set, corresponding to the addition
c     of a single eta in an eta group with eta_grp_n_ix indices.
c
c     |  ty  | n_eta   | sa_1 ... sa_(n_eta+1) |  n_eta  |  ty  | ty' |
c                                       -2     |    -1   |   0  |  1  |
c     |<-eta_se_rec_z->|<---eta_se_n_eta+1---->|<-eta_se_rec_z->|
c
c     |  ty  | n_eta+1 | sa_1 ... sa_(n_eta+2) | n_eta+1 |  ty  | ty' |
c                                       -1     |    0    |   1  |  2  |
c     |<-eta_se_rec_z->|<---eta_se_n_eta+2---->|<-eta_se_rec_z->|
c
      subroutine ems_u_eta_se_rec(
     &     eta_grp_n_ix,
     &     eta_se_rec)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      integer eta_grp_n_ix
      integer eta_se_rec(*)
      integer eta_se_ty, eta_se_n_eta
      integer p
c      integer ix
c
c     Get the type and number of etas from the backward record.
c
      eta_se_ty = eta_se_rec(eta_se_rec_bwd_os_eta_ty)
      eta_se_n_eta = eta_se_rec(eta_se_rec_bwd_os_n_eta)
c
c     Forward offsets are added to the pointer to the last entry in the
c     previous set.
c
      p = -(eta_se_rec_z+eta_se_n_eta+1+eta_se_rec_z)
c      write(*,9000)(ix, ix=p, 1)
c      write(*,9000)(eta_se_rec(ix), ix=p, 1)
c
c     Increase the number of etas for the forward record.
c
      eta_se_rec(p+eta_se_rec_os_n_eta) = eta_se_n_eta + 1
c
c     Initialise the first record in the next set to correspond to a
c     null set.
c
      eta_se_rec(1+eta_se_rec_os_eta_ty) = no_eta_se_ty
c
c     Shift the type and the (updated) number of etas at the end of the
c     record.
c
      eta_se_rec(1+eta_se_rec_bwd_os_eta_ty) = eta_se_ty
      eta_se_rec(1+eta_se_rec_bwd_os_n_eta) = eta_se_n_eta + 1
c
c     Record the start for the (n_eta+1)'st eta.
c
      eta_se_rec(1-eta_se_rec_z) = eta_grp_n_ix + 1
c      write(*,9000)(ix, ix=p, 2)
c      write(*,9000)(eta_se_rec(ix), ix=p, 2)
c 9000 format(20(i4))
 
      return
      end
 
 
C->>> -------------------------------------------> ems_eta_grp_rec_z <<<
c     Returns the number of words of data in an eta group of given type.
c
      integer function ems_eta_grp_rec_z(eta_grp_ty)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'SLAPCS.INC'
      integer eta_grp_ty
c      if (eta_grp_ty .eq. rsmi_eta_grp_ty) then
         ems_eta_grp_rec_z = rsmi_eta_grp_rec_z
c      else if (eta_grp_ty .eq. parlp_eta_grp_ty) then
c         ems_eta_grp_rec_z = parlp_eta_grp_rec_z
c      end if
      return
      end
 
C->>> ------------------------------------------> ems_g_eta_grp_n_wo <<<
c     Gets the number of integer words which make up the eta group
c     record plus the eta group itself.
c
      subroutine ems_g_eta_grp_n_wo(
     &     eta_grp_ty, sto_eta_v, mx_n_eta, mx_n_el, n_wo)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'SLAPCS.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer eta_grp_ty, sto_eta_v, mx_n_eta, mx_n_el, n_wo
      integer ems_eta_grp_rec_z
      integer eta_grp_rec_z
      integer mx_n_v, mx_n_ix, mx_n_rec
 
      call ems_g_eta_grp_a_z(
     &     eta_grp_ty, sto_eta_v, mx_n_eta, mx_n_el,
     &     mx_n_v, mx_n_ix, mx_n_rec)
      eta_grp_rec_z = ems_eta_grp_rec_z(eta_grp_ty)
      if (eta_grp_ty .eq. rsmi_eta_grp_ty) then
         n_wo = i_wo_z*(eta_grp_rec_z) +
     &         rl_wo_z*(1+mx_n_v  +1) +
     &          i_wo_z*(1+mx_n_ix +1) +
     &          i_wo_z*(1+mx_n_rec+1)
c      else if (eta_grp_ty .eq. parlp_eta_grp_ty) then
c         n_wo = i_wo_z*(eta_grp_rec_z) +
c     &         rl_wo_z*(1+mx_n_v  +1) +
c     &         rl_wo_z*(1+rl_si_da_rec_z*mx_n_eta) +
c     &          i_wo_z*(1+mx_n_ix +1) +
c     &          i_wo_z*(1+mx_n_rec+1) +
c     &          i_wo_z*(mx_n_eta*(1+n_r/10)) +
c     &          i_wo_z*(1+i_si_da_rec_z*mx_n_eta)
      else
         go to 8010
      end if
 7000 continue
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)
     &     eta_grp_ty, 1, n_eta_grp_ty
      call ems_msg_wr_li(bug_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?      call ems_dump
CM      ENDIF
      go to 7000
 9801 format('Eta group type is ', i9,
     &     '. Valid range is ', i1, ' to ', i2)
      end
 
C->>> -------------------------------------------> ems_g_eta_grp_a_z <<<
      subroutine ems_g_eta_grp_a_z(
     &     eta_grp_ty, sto_eta_v, mx_n_eta, mx_n_el,
     &     mx_n_v, mx_n_ix, mx_n_rec)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'ICTVR.INC'
      integer eta_grp_ty, sto_eta_v, mx_n_eta, mx_n_el
      integer mx_n_v, mx_n_ix, mx_n_rec
 
      if (sto_eta_v .eq. sto_full_eta_v) then
         mx_n_v = mx_n_eta*(1+n_r)
      else
         mx_n_v = mx_n_el
      endif
      mx_n_ix = mx_n_el
c
c     Make sure that there is enough space for:
c     a start for all the possible etas plus
c     two records and a start for the n+1'st eta in all possible sets
c     plus the null type record for the n+1'st set.
c
      mx_n_rec = mx_n_eta + (2*eta_se_rec_z+1)*mx_n_eta_se + 1
      return
      end
 
C->>> ------------------------------------------------> ems_g_r_o_vr <<<
c     Gets the r_o_vr corresponding to the vr_in_r supplied.
c
      subroutine ems_g_r_o_vr(
     &     vr_in_r,
     &     r_o_vr)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      integer vr_in_r(0:n_r), r_o_vr(0:mx_n_c+n_r)
      integer r_n
 
      do 10, r_n = 1, n_r
         r_o_vr(vr_in_r(r_n)) = r_n
 10   continue
      return
      end
 
C->>> ------------------------------------------> ems_g_vr_in_r_perm <<<
c     Gets the permutation from fm_vr_in_r to the vr_in_r of t_r_o_vr.
c
      subroutine ems_g_vr_in_r_perm(
     &     fm_vr_in_r,
     &     st,
     &     vr_in_r_perm)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer fm_vr_in_r(0:n_r), st(0:mx_n_c+n_r)
      integer vr_in_r_perm(0:n_r)
      integer r_n, vr_n, og_r_n
      double precision v, v1
 
      do 10, r_n = 1, n_r
         vr_n = fm_vr_in_r(r_n)
         og_r_n = iand(st(vr_n), mx_mx_ml_a_dim)
         vr_in_r_perm(r_n) = og_r_n
         if (iand(st(vr_n), bc_bt) .eq. 0) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9900)vr_n
            call ems_msg_wr_li(bug_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?            call ems_dump
CM      ENDIF
         end if
         if (og_r_n .le. 0) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9910)
     &           vr_n, st(vr_n), f_il_ix_n, og_r_n
            call ems_msg_wr_li(bug_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?            call ems_dump
CM      ENDIF
            v = 0d0
            v1 = 1d0/v
         end if
 10   continue
      return
 9900 format('In g_vr_in_r_perm, variable ', i9, ' is not basic')
 9910 format('In g_vr_in_r_perm, og_r_n = ', 4(2x, i9))
      end
 
C->>> ------------------------------------------------> ems_perm_inv <<<
c     Apply the permutation perm to the indices of the entries in the
c     eta file.
c
      subroutine ems_perm_inv(perm, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'SLAPCS.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      include 'EMSMSGN.INC'
      integer perm(0:n_r), is(0:is_n_en_m1)
      integer p_eta_grp, p_ix
      integer n_el, el_n, eta_grp_n
      integer rt_cod
 
      do 20, eta_grp_n = 1, eta_fi_n_grp
         p_eta_grp = eta_fi_p_a((eta_grp_n-1)*eta_fi_p_a_rec_z+
     &        eta_fi_p_a_os_p_eta_grp)
         p_ix = eta_fi_p_a((eta_grp_n-1)*eta_fi_p_a_rec_z+
     &        eta_fi_p_a_os_p_eta_ix)
         if (lo_sto_mode .ne. 0) call ems_clo_eta_grp(is(p_eta_grp), is)
CM      IF (emsol_da .EQ. 1) THEN
C?c         if (n_r .gt. 40000)
C?c     &        call ems_wr_eta_grp_n_wo(is(p_eta_grp), is)
CM      ENDIF
         n_el = is(p_eta_grp+eta_grp_os_n_ix)
         do 10, el_n = 1, n_el
            is(p_ix+el_n) = perm(is(p_ix+el_n))
 10      continue
 20   continue
      if (lo_sto_mode .ne. 0) then
         call ems_g_inv_p(rt_cod, is)
         if (rt_cod .ne. 0) goto 8020
      endif
 7000 continue
      return
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9802 format('Error in ems_g_inv_p')
      end
