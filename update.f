C->>> ------------------------------------------> ems_ca_sto_u_eta <<<
c     Replace the pivot with its negative reciprocal and updates the
c     records so that the new eta becomes part of the eta file. Opens a
c     new update eta group if this will be necessary to store further
c     etas.
c
      subroutine ems_ca_sto_u_eta(nw_eta_sgn, eta_se_ty, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'SLAPCS.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMICS.INC'
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      integer nw_eta_sgn, eta_se_ty
      integer is(0:is_n_en_m1)
      double precision ds(0:ds_n_en_m1)
      logical ems_rq_nw_u_eta_grp
      integer p_hdl_eta_grp, p_eta_grp, cu_eta_se_ty
      integer nw_eta_n_ix
      integer el_n
      integer rt_cod
 
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_u_lvl1) call ems_tt_rec(u_eta_fi_tt, n_bs)
CM      ENDIF
      nw_eta_n_ix = nw_eta_l_ix - nw_eta_f_ix + 1
      p_eta_grp = eta_fi_p_a((eta_fi_n_grp-1)*eta_fi_p_a_rec_z+
     &     eta_fi_p_a_os_p_eta_grp)
c
c     p_nw_eta_rec points to the last eta record: this should be a null
c     set type.
c
      cu_eta_se_ty = is(p_nw_eta_rec-1+eta_se_rec_bwd_os_eta_ty)
      if (cu_eta_se_ty .ne. eta_se_ty) then
c
c     Change of eta set type so initialise a new record.
c
         call ems_iz_eta_se_rec(eta_se_ty,
     &        is(p_eta_grp+eta_grp_os_n_ix), is(p_nw_eta_rec))
         eta_fi_n_se = eta_fi_n_se + 1
         p_nw_eta_rec = p_nw_eta_rec + 2*eta_se_rec_z+1
         is(p_eta_grp+eta_grp_os_n_rec) =
     &        is(p_eta_grp+eta_grp_os_n_rec) + 2*eta_se_rec_z+1
      endif
c
c     Turn the pivotal element into the pivot for an eta: negating the
c     entries if the pivotal column has been formed with opposite sign.
c
      ds(p_nw_eta_v) = -nw_eta_sgn/ds(p_nw_eta_v)
      if (nw_eta_sgn .eq. -1) then
         do 10, el_n = 1, nw_eta_n_ix-1
            ds(p_nw_eta_v+el_n) = -ds(p_nw_eta_v+el_n)
 10      continue
         nw_eta_sgn = 1
      endif
      call ems_u_eta_fi_rec(eta_se_ty, nw_eta_n_ix, pv_r_n,
     &     is(p_eta_grp), ds, is)
CM      IF (emsol_da .EQ. 1) THEN
C?c
C?c     Optionally write out data for the current INVERT and check it.
C?c
C?      if (iand(inv_msg_msk, bt2) .ne. 0)
C?     &     call ems_wr_inv_da(inv_log_msk, ds, is)
CM      ENDIF
c
c     Determine whether a new update eta group is required.
c
      if (ems_rq_nw_u_eta_grp(is(p_eta_grp))) then
         if (eta_fi_n_grp .eq. eta_fi_mx_n_eta_grp) then
            rq_inv = rq_inv_mx_n_eta_grp
            ml_da_st_msk =
     &           ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_inv)
            goto 7000
         endif
         p_hdl_eta_grp = p_ml_bs_blk + (cu_ml_n-1)*ml_bs_blk_n_wo +
     &        ml_bs_blk_os_hdl + ix_eta_fi_hdl +
     &        eta_fi_n_grp*hdl_z
         call ems_ope_u_eta_grp(is(p_hdl_eta_grp), ds, is)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
         eta_fi_n_grp = eta_fi_n_grp + 1
         call ems_g_inv_p(rt_cod, is)
         if (rt_cod .ne. 0) goto 8020
      end if
 7000 continue
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_u_lvl1) call ems_tt_rec(-u_eta_fi_tt, n_bs)
CM      ENDIF
      return
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9802 format('Error in ems_g_inv_p')
      end
 
C->>> --------------------------------------------> ems_u_eta_fi_rec <<<
c     Updates the eta file records corresponding to a new eta with
c     nw_eta_n_ix indices.
c
      subroutine ems_u_eta_fi_rec(eta_se_ty, nw_eta_n_ix, pv_r_n,
     &     eta_grp, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'SLAPCS.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      integer eta_se_ty, nw_eta_n_ix, pv_r_n
      integer eta_grp(0:mx_eta_grp_rec_z)
      double precision ds(0:ds_n_en_m1)
      integer is(0:is_n_en_m1)
c
c     Account for the addition of the eta.
c     Update the number of etas in the eta file and eta group.
c
      eta_grp(eta_grp_os_n_eta) = eta_grp(eta_grp_os_n_eta) + 1
      eta_fi_n_eta = eta_fi_n_eta + 1
c
c     Update the pointer to the first value in the next new eta and
c     the number of valuess in the eta file and eta group.
c
      if (eta_se_ty .eq. pk_c_eta_se_ty) then
         p_nw_eta_v = p_nw_eta_v + nw_eta_n_ix
         eta_grp(eta_grp_os_n_v) = eta_grp(eta_grp_os_n_v) + nw_eta_n_ix
         eta_fi_n_v = eta_fi_n_v + nw_eta_n_ix
      else
         ds(p_nw_eta_v+pv_r_n) = zero
         p_nw_eta_v = p_nw_eta_v + 1+n_r
         eta_grp(eta_grp_os_n_v) = eta_grp(eta_grp_os_n_v) + 1+n_r
         eta_fi_n_v = eta_fi_n_v + 1+n_r
      end if
c
c     Update the pointer to the first index in the next new eta and
c     the number of indices in the eta file and eta group.
c
      p_nw_eta_ix = p_nw_eta_ix + nw_eta_n_ix
      eta_grp(eta_grp_os_n_ix) = eta_grp(eta_grp_os_n_ix) + nw_eta_n_ix
      eta_fi_n_ix = eta_fi_n_ix + nw_eta_n_ix
c
c     Update the eta set record, the pointer to the last record and
c     the number of records in the eta group.
c
      call ems_u_eta_se_rec(eta_grp(eta_grp_os_n_ix), is(p_nw_eta_rec))
      p_nw_eta_rec = p_nw_eta_rec + 1
      eta_grp(eta_grp_os_n_rec) = eta_grp(eta_grp_os_n_rec) + 1
c
c     Assign a value to the first entry in the next new eta to avoid
c     an unassigned variable violation for the assembler ftran.
c
      is(p_nw_eta_ix) = 0
c
c     Update the records of number of updates and average density of
c     etas over (roughly) the past long period.
c
      n_u = n_u + 1
      av_eta_dse = fac_long_prd*float(nw_eta_n_ix)/float(n_r) +
     &     (one-fac_long_prd)*av_eta_dse
      return
      end
 
C->>> -----------------------------------------> ems_rq_nw_u_eta_grp <<<
c     Determines whether a new eta group is required. Makes sure that
c     there is at least enough space to record a fully dense eta in a
c     new set.
c
      logical function ems_rq_nw_u_eta_grp(eta_grp)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'SLAPCS.INC'
      include 'ICTVR.INC'
      integer eta_grp(0:mx_eta_grp_rec_z)
      logical rq_nw_u_eta_grp
 
      rq_nw_u_eta_grp =
     &     eta_grp(eta_grp_os_n_v) +   1+mx_n_r .gt.
     &     eta_grp(eta_grp_os_mx_n_v) .or.
     &     eta_grp(eta_grp_os_n_ix) +  1+mx_n_r .gt.
     &     eta_grp(eta_grp_os_mx_n_ix) .or.
     &     eta_grp(eta_grp_os_n_rec)+  2*eta_se_rec_z+2 .gt.
     &     eta_grp(eta_grp_os_mx_n_rec)
      ems_rq_nw_u_eta_grp = rq_nw_u_eta_grp
      return
      end
 
C->>> --------------------------------------------> ems_u_pf_dse_blk <<<
c     Updates the dense block of eta values in pivotal rows for product
c     form updates.
c
c     The dense block is used to store the following:
c
c                 |    0     |  1 ... dse_blk_n_c  |
c     ------------|----------|---------------------|
c          0      |    -     |  Pivotal values     |
c     ------------|----------|---------------------|
c          1      | Partial  |  Non-pivotal values |
c          :      | BTRAN of |  in rows of update  |
c     dse_blk_n_r | I_pv_r   |  etas with pivots.  |
c
c     u_bs_gthr_pv_r(0)      is the number of rows in the dense block
c     u_bs_gthr_pv_r(1..n_r) are the dense block row numbers for pivotal
c     .                      rows and zero for non-pivotal rows.
c
c     u_bs_dse_blk_pv_r_in_c(0)      is the number of columns in the
c     .                              dense block.
c     u_bs_dse_blk_pv_r_in_c(1..n_u) is the dense block row which is
c     .                              pivotal for each update.
c
c     u_bs_skt_pv_r(0)      is the number of updates for which the dense
c     .                     block is dimensioned.
c     u_bs_skt_pv_r(1..n_u) is the row number for each dense block row
c
c     u_bs_eta_msk(1...) is the bit mask for nonzeros in update etas.
c
      subroutine ems_u_pf_dse_blk(
     &     dse_blk_dim,
     &     u_bs_dse_blk,
     &     u_bs_gthr_pv_r,
     &     u_bs_dse_blk_pv_r_in_c,
     &     u_bs_skt_pv_r,
     &     u_bs_eta_msk,
     &     ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'SLAPCS.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMICS.INC'
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      integer dse_blk_dim
      integer is(0:is_n_en_m1)
      integer u_bs_gthr_pv_r(0:n_r)
      integer u_bs_dse_blk_pv_r_in_c(0:dse_blk_dim)
      integer u_bs_skt_pv_r(0:dse_blk_dim)
      integer u_bs_eta_msk(0:((dse_blk_dim-1)/dse_blk_u_sn_ln+1)*n_r)
      double precision ds(0:ds_n_en_m1)
      double precision u_bs_dse_blk(0:dse_blk_dim, 0:dse_blk_dim)
      integer dse_blk_n_r, dse_blk_n_c
      integer dse_blk_r_n, dse_blk_c_n
      integer pv_r_dse_blk_r_n, r_n, pv_r_eta_msk
c      integer u_n
      integer p_eta_fi_p_a
      integer p_eta_grp, p_eta_v, p_eta_ix, p_eta_rec, p_eta_sa
      integer eta_se_ty, eta_se_n_eta
      integer el_n, tru_f_u_n, use_eta_n
      integer fm_el_n, t_el_n
      integer fm_dse_blk_c_n, t_dse_blk_c_n
      integer dse_blk_sn_n, dse_blk_sn_os, dse_blk_sn_c_n
      logical nw_pv_r
      logical lc_sw
CM      IF (emsol_deb .EQ. 1) THEN
C?      integer wr_eta_ix
C?      integer wr_dse_blk_pf_da
C?      save wr_eta_ix
C?      save wr_dse_blk_pf_da
C?      data wr_eta_ix/0/
C?      data wr_dse_blk_pf_da/0/
CM      ENDIF
 
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_u_lvl1) call ems_tt_rec(u_pf_dse_blk_tt, n_bs)
CM      ENDIF
      dse_blk_sn_n = (n_u-1)/dse_blk_u_sn_ln + 1
      dse_blk_sn_os = (dse_blk_sn_n-1)*n_r
      dse_blk_sn_c_n = mod(n_u-1, dse_blk_u_sn_ln) + 1
      if (n_u .eq. 1) then
c
c     Zero the number of rows/columns in the dense block and the vector
c     of dense block row indices.
c
         u_bs_dse_blk_pv_r_in_c(0) = 0
         u_bs_gthr_pv_r(0) = 0
c         do 10, u_n = 1, dse_blk_dim
c            u_bs_dse_blk_pv_r_in_c(u_n) = 0
c            u_bs_skt_pv_r(u_n) = 0
c 10      continue
         do 20, r_n = 1, n_r
            u_bs_gthr_pv_r(r_n) = 0
 20      continue
      endif
      if (dse_blk_sn_c_n .eq. 1) then
         do 30, r_n = 1, n_r
            u_bs_eta_msk(dse_blk_sn_os+r_n) = 0
 30      continue
      endif
c
c     Get the sure number of rows and columns in the new dense block.
c     There may be a new row, depending upon whether the pivotal row
c     has already been a pivotal row.
c
      dse_blk_n_r = u_bs_gthr_pv_r(0)
      dse_blk_n_c = u_bs_dse_blk_pv_r_in_c(0) + 1
CM      IF (emsol_deb .EQ. 1) THEN
C?      if (wr_dse_blk_pf_da .gt. 0) then
C?         write(*, 9000)'Before update'
C?         call ems_wr_dse_blk_pf_da(
C?     &        dse_blk_dim,
C?     &        u_bs_dse_blk, u_bs_gthr_pv_r,
C?     &        u_bs_dse_blk_pv_r_in_c, u_bs_skt_pv_r, u_bs_eta_msk)
C?      endif
CM      ENDIF
c
c     Record the initial eta mask for this row in the current section so
c     that it is easy to see if there are no entries in earlier etas
c     in this section by just testing for zero.
c
      pv_r_eta_msk = u_bs_eta_msk(dse_blk_sn_os+pv_r_n)
c
c     Determine whether the pivotal row has already been a pivotal row.
c
      pv_r_dse_blk_r_n = u_bs_gthr_pv_r(pv_r_n)
      nw_pv_r = pv_r_dse_blk_r_n .eq. 0
      if (nw_pv_r) then
         dse_blk_n_r = dse_blk_n_r + 1
         pv_r_dse_blk_r_n = dse_blk_n_r
         u_bs_gthr_pv_r(pv_r_n) = pv_r_dse_blk_r_n
         u_bs_skt_pv_r(pv_r_dse_blk_r_n) = pv_r_n
      endif
      u_bs_gthr_pv_r(0) = dse_blk_n_r
      u_bs_dse_blk_pv_r_in_c(0) = dse_blk_n_c
c
c     Zero the new column of the dense block.
c
      do 40, dse_blk_r_n = 1, dse_blk_n_r
         u_bs_dse_blk(dse_blk_r_n, dse_blk_n_c) = zero
 40   continue
c
c     Go through the last eta, recording the values in previous pivotal
c     rows.
c
      p_eta_fi_p_a = (eta_fi_n_grp-1)*eta_fi_p_a_rec_z
      p_eta_grp = eta_fi_p_a(p_eta_fi_p_a+eta_fi_p_a_os_p_eta_grp)
      p_eta_v = eta_fi_p_a(p_eta_fi_p_a+eta_fi_p_a_os_p_eta_v)
      p_eta_ix = eta_fi_p_a(p_eta_fi_p_a+eta_fi_p_a_os_p_eta_ix)
      p_eta_rec = eta_fi_p_a(p_eta_fi_p_a+eta_fi_p_a_os_p_eta_rec)
      p_eta_rec = p_eta_rec + is(p_eta_grp+eta_grp_os_n_rec) - 1
      eta_se_ty = is(p_eta_rec+eta_se_rec_bwd_os_eta_ty)
      if (eta_se_ty .eq. no_eta_se_ty) then
c
c     The eta is in an earlier group
c
         p_eta_fi_p_a = p_eta_fi_p_a - eta_fi_p_a_rec_z
         p_eta_grp = eta_fi_p_a(p_eta_fi_p_a+eta_fi_p_a_os_p_eta_grp)
         p_eta_v = eta_fi_p_a(p_eta_fi_p_a+eta_fi_p_a_os_p_eta_v)
         p_eta_ix = eta_fi_p_a(p_eta_fi_p_a+eta_fi_p_a_os_p_eta_ix)
         p_eta_rec = eta_fi_p_a(p_eta_fi_p_a+eta_fi_p_a_os_p_eta_rec)
         p_eta_rec = p_eta_rec + is(p_eta_grp+eta_grp_os_n_rec) - 1
         eta_se_ty = is(p_eta_rec+eta_se_rec_bwd_os_eta_ty)
         if (eta_se_ty .eq. no_eta_se_ty .or.
     &        eta_se_ty .eq. inv_eta_se_ty) goto 8000
      endif
      eta_se_n_eta = is(p_eta_rec+eta_se_rec_bwd_os_n_eta)
      p_eta_rec = p_eta_rec -
     &     (eta_se_rec_z + eta_se_n_eta + 1 + eta_se_rec_z)
      p_eta_sa = p_eta_rec + eta_se_rec_os_ze_sa_en
CM      IF (emsol_deb .EQ. 1) THEN
C?       if (wr_eta_ix .eq. 1) write(*, 9010)
C?     &     is(p_eta_sa+eta_se_n_eta+1)-is(p_eta_sa+eta_se_n_eta)-1,
C?     &     (is(p_eta_ix+el_n), el_n =
C?     &     is(p_eta_sa+eta_se_n_eta)+1,
C?     &     is(p_eta_sa+eta_se_n_eta+1)-1)
CM      ENDIF
      do 50, el_n = is(p_eta_sa+eta_se_n_eta+1)-1,
     &     is(p_eta_sa+eta_se_n_eta)+1, -1
         r_n = is(p_eta_ix+el_n)
         dse_blk_r_n = u_bs_gthr_pv_r(r_n)
c
c     If this row has been a pivot record its value in the dense block.
c
         if (dse_blk_r_n .gt. 0)
     &        u_bs_dse_blk(dse_blk_r_n, dse_blk_n_c) = ds(p_eta_v+el_n)
c
c     Maintain the record of etas with entries in this row.
c
         u_bs_eta_msk(dse_blk_sn_os+r_n) =
     &        u_bs_eta_msk(dse_blk_sn_os+r_n) + bt_a(dse_blk_sn_c_n)
 50   continue
c
c     Record the pivotal value and the row of the dense block in which
c     it occurs.
c
      if (is(p_eta_ix+el_n) .ne. pv_r_n) goto 8010
      u_bs_dse_blk(0, dse_blk_n_c) = ds(p_eta_v+el_n)
c
c     For long etas the existence of an entry in the pivotal row for
c     this eta has been recorded when the pivotal index was encountered
c     during the pass through the eta indices.
c
      if (nw_eta_l_ix-1 .le. ord_i_ls_stp)
     &     u_bs_eta_msk(dse_blk_sn_os+pv_r_n) =
     &     u_bs_eta_msk(dse_blk_sn_os+pv_r_n) + bt_a(dse_blk_sn_c_n)
      u_bs_dse_blk_pv_r_in_c(dse_blk_n_c) = pv_r_dse_blk_r_n
c
c     If there has already been a pivot in this row then return.
c
      if (.not. nw_pv_r) goto 7000
c
c     Zero the new entry in column zero: Used to accumulate TRANS.
c
      u_bs_dse_blk(pv_r_dse_blk_r_n, 0) = zero
c
c     [fm_dse_blk_c_n, t_dse_blk_c_n] is the current set of columns
c     which must be searched for nonzeros in the new pivotal row.
c
      fm_dse_blk_c_n = dse_blk_n_c - 1
      t_dse_blk_c_n = (dse_blk_sn_n-1)*dse_blk_u_sn_ln + 1
c
c     Initialise tru_f_u_n which is used to determine whether the eta
c     for a particular update is in the current set.
c
      tru_f_u_n = n_u - eta_se_n_eta + 1
 100  continue
c
c     If this row has no entries in earlier etas in this section
c     (pv_r_eta_msk=0) then zero the entries in the rest of the new row
c     for this section.
c
      if (pv_r_eta_msk .eq. 0) then
         do 110, dse_blk_c_n = fm_dse_blk_c_n, t_dse_blk_c_n, -1
            u_bs_dse_blk(pv_r_dse_blk_r_n, dse_blk_c_n) = zero
 110     continue
      else
c
c     Go through the rest of the update etas which have entries in the
c     pivotal row and record their values in the dense block.
c
         dse_blk_sn_c_n = (fm_dse_blk_c_n-t_dse_blk_c_n+1) + 1
         do 160, dse_blk_c_n = fm_dse_blk_c_n, t_dse_blk_c_n, -1
            dse_blk_sn_c_n = dse_blk_sn_c_n - 1
            lc_sw = .false.
            goto 116
 115        continue
            lc_sw = .true.
 116        continue
            if (lc_sw .or.
     &           iand(pv_r_eta_msk, bt_a(dse_blk_sn_c_n)) .ne. 0) then
               if (lc_sw .or.
     &              dse_blk_c_n .lt. tru_f_u_n) then
c
c     The eta is in an earlier set.
c
                  eta_se_ty = is(p_eta_rec+eta_se_rec_bwd_os_eta_ty)
                  if (eta_se_ty .eq. no_eta_se_ty) then
c
c     The eta is in an earlier group
c
                     p_eta_fi_p_a = p_eta_fi_p_a - eta_fi_p_a_rec_z
                     p_eta_grp = eta_fi_p_a(p_eta_fi_p_a+
     &                    eta_fi_p_a_os_p_eta_grp)
                     p_eta_v = eta_fi_p_a(p_eta_fi_p_a+
     &                    eta_fi_p_a_os_p_eta_v)
                     p_eta_ix = eta_fi_p_a(p_eta_fi_p_a+
     &                    eta_fi_p_a_os_p_eta_ix)
                     p_eta_rec = eta_fi_p_a(p_eta_fi_p_a+
     &                    eta_fi_p_a_os_p_eta_rec)
                     p_eta_rec = p_eta_rec + is(p_eta_grp+
     &                    eta_grp_os_n_rec) - 1
                     eta_se_ty = is(p_eta_rec+eta_se_rec_bwd_os_eta_ty)
                     if (eta_se_ty .eq. no_eta_se_ty .or.
     &                    eta_se_ty .eq. inv_eta_se_ty) goto 8020
                  endif
                  eta_se_n_eta = is(p_eta_rec+eta_se_rec_bwd_os_n_eta)
                  p_eta_rec = p_eta_rec -
     &                 (eta_se_rec_z + eta_se_n_eta + 1 + eta_se_rec_z)
                  p_eta_sa = p_eta_rec + eta_se_rec_os_ze_sa_en
                  tru_f_u_n = tru_f_u_n - eta_se_n_eta
               endif
               if (dse_blk_c_n .lt. tru_f_u_n) goto 115
               use_eta_n = dse_blk_c_n - tru_f_u_n + 1
c
c     Search for the pivotal index
c
               el_n = is(p_eta_sa+use_eta_n+1)-1
               if (is(p_eta_ix+el_n) .eq. pv_r_n) goto 150
 120           continue
               t_el_n = el_n-1
               el_n = el_n - ord_i_ls_stp
               if (el_n .le. is(p_eta_sa+use_eta_n)) then
                  fm_el_n = is(p_eta_sa+use_eta_n)+1
                  goto 130
               endif
               if (is(p_eta_ix+el_n) .gt. pv_r_n) goto 120
               if (is(p_eta_ix+el_n) .eq. pv_r_n) goto 150
               fm_el_n = el_n+1
 130           continue
               do 140, el_n = fm_el_n, t_el_n
                  if (is(p_eta_ix+el_n) .eq. pv_r_n) goto 150
 140           continue
               goto 8030
 150           continue
               u_bs_dse_blk(pv_r_dse_blk_r_n, dse_blk_c_n) =
     &              ds(p_eta_v+el_n)
            else
               u_bs_dse_blk(pv_r_dse_blk_r_n, dse_blk_c_n) = zero
            endif
 160     continue
      endif
      if (dse_blk_sn_n .gt. 1) then
c
c     More sections so go back and consider the next one.
c
         dse_blk_sn_n = dse_blk_sn_n - 1
         dse_blk_sn_os = dse_blk_sn_os - n_r
         fm_dse_blk_c_n = t_dse_blk_c_n - 1
         t_dse_blk_c_n = t_dse_blk_c_n - dse_blk_u_sn_ln
         pv_r_eta_msk = u_bs_eta_msk(dse_blk_sn_os+pv_r_n)
         goto 100
      endif
 7000 continue
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_u_lvl1) call ems_tt_rec(-u_pf_dse_blk_tt, n_bs)
CM      ENDIF
CM      IF (emsol_deb .EQ. 1) THEN
C?      if (wr_dse_blk_pf_da .gt. 0) call ems_wr_dse_blk_pf_da(
C?     &     dse_blk_dim,
C?     &     u_bs_dse_blk, u_bs_gthr_pv_r,
C?     &     u_bs_dse_blk_pv_r_in_c, u_bs_skt_pv_r, u_bs_eta_msk)
CM      ENDIF
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?      call ems_dump
CM      ENDIF
      goto 7000
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)
      call ems_msg_wr_li(bug_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?      call ems_dump
CM      ENDIF
      goto 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)dse_blk_c_n
      call ems_msg_wr_li(bug_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?      call ems_dump
CM      ENDIF
      goto 7000
 8030 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9803)
      call ems_msg_wr_li(bug_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?      call ems_dump
CM      ENDIF
      goto 7000
CM      IF (emsol_deb .EQ. 1) THEN
C? 9000 format(//a)
C? 9010 format('Pivot row ', i7, (/10(2x, i7)))
CM      ENDIF
 9800 format('ERROR: Cannot find last update eta')
 9801 format('ERROR: is(p_eta_ix+el_n) .ne. pv_r_n')
 9802 format('ERROR: Cannot find update eta ', i7)
 9803 format('ERROR: new pivot row not in non-pivotal entries')
      end
 
CM      IF (emsol_deb .EQ. 1) THEN
C?C->>> --------------------------------------> ems_wr_dse_blk_pf_da <<<
C?      subroutine ems_wr_dse_blk_pf_da(
C?     &     dse_blk_dim,
C?     &     u_bs_dse_blk,
C?     &     u_bs_gthr_pv_r,
C?     &     u_bs_dse_blk_pv_r_in_c,
C?     &     u_bs_skt_pv_r,
C?     &     u_bs_eta_msk)
C?      implicit none
C?      include 'EMSV.INC'
C?      include 'EMSPM.INC'
C?      include 'ICTVR.INC'
C?      integer dse_blk_dim
C?      integer u_bs_gthr_pv_r(0:n_r)
C?      integer u_bs_dse_blk_pv_r_in_c(0:dse_blk_dim)
C?      integer u_bs_skt_pv_r(0:dse_blk_dim)
C?      integer u_bs_eta_msk(0:((dse_blk_dim-1)/dse_blk_u_sn_ln+1)*n_r)
C?      double precision u_bs_dse_blk(0:dse_blk_dim, 0:dse_blk_dim)
C?      character*12 ems_wr_rl_t_ch12
C?      character*12 eta_r_ch12(10)
C?      character*1 eta_r_ch1(999)
C?      integer fm_dse_blk_c_n, t_dse_blk_c_n
C?      integer dse_blk_r_n, dse_blk_c_n
C?      integer dse_blk_n_r, dse_blk_n_c
C?      integer r_n, r_eta_msk
C?      integer dse_blk_sn_n, dse_blk_sn_os, dse_blk_sn_c_n
C?
C?      dse_blk_n_r = u_bs_gthr_pv_r(0)
C?      dse_blk_n_c = u_bs_dse_blk_pv_r_in_c(0)
C?      if (dse_blk_n_c .eq. 0) goto 7000
C?
C?      dse_blk_sn_n = (n_u-1)/dse_blk_u_sn_ln + 1
C?      dse_blk_sn_os = (dse_blk_sn_n-1)*n_r
C?      dse_blk_sn_c_n = mod(n_u-1, dse_blk_u_sn_ln) + 1
C?
C?      if (dse_blk_n_c .le. 10) then
C?         write(*, 9000)'Dense block values',
C?     &        (dse_blk_c_n, dse_blk_c_n = 1, dse_blk_n_c)
C?         do 10, dse_blk_r_n = 1, dse_blk_n_r
C?            do 5, dse_blk_c_n = 1, dse_blk_n_c
C?               eta_r_ch12(dse_blk_c_n) = ems_wr_rl_t_ch12(
C?     &              u_bs_dse_blk(dse_blk_r_n, dse_blk_c_n))
C? 5          continue
C?            write(*, 9010)dse_blk_r_n, u_bs_skt_pv_r(dse_blk_r_n),
C?     &           (eta_r_ch12(dse_blk_c_n),
C?     &           dse_blk_c_n = 1, dse_blk_n_c)
C?            if (u_bs_gthr_pv_r(u_bs_skt_pv_r(dse_blk_r_n)) .ne.
C?     &            dse_blk_r_n) then
C?               write(*, *)'ERROR: Scatter/Gather inconsistency ',
C?     &              u_bs_skt_pv_r(dse_blk_r_n),
C?     &              u_bs_gthr_pv_r(u_bs_skt_pv_r(dse_blk_r_n)),
C?     &              dse_blk_r_n
C?               goto 7000
C?            endif
C? 10      continue
C?         do 15, dse_blk_c_n = 1, dse_blk_n_c
C?            eta_r_ch12(dse_blk_c_n) = ems_wr_rl_t_ch12(
C?     &           u_bs_dse_blk(0, dse_blk_c_n))
C? 15      continue
C?         write(*, 9000)'Pivotal values and row numbers',
C?     &        (dse_blk_c_n, dse_blk_c_n = 1, dse_blk_n_c)
C?         write(*, 9020)
C?     &        (eta_r_ch12(dse_blk_c_n), dse_blk_c_n = 1, dse_blk_n_c)
C?         write(*, 9021)
C?     &        (u_bs_dse_blk_pv_r_in_c(dse_blk_c_n),
C?     &        dse_blk_c_n = 1, dse_blk_n_c)
C?
C?         write(*, 9000)'Dense block etas',
C?     &        (dse_blk_c_n, dse_blk_c_n = 1, dse_blk_n_c)
C?         do 20, dse_blk_r_n = 1, dse_blk_n_r
C?            r_n = u_bs_skt_pv_r(dse_blk_r_n)
C?            dse_blk_sn_n = 1
C?            dse_blk_sn_os = 0
C?            t_dse_blk_c_n = 0
C? 22         continue
C?            fm_dse_blk_c_n = t_dse_blk_c_n + 1
C?            t_dse_blk_c_n =
C?     &           min(dse_blk_n_c, dse_blk_sn_n*dse_blk_u_sn_ln)
C?            r_eta_msk = u_bs_eta_msk(dse_blk_sn_os+r_n)
C?            dse_blk_sn_c_n = 0
C?            do 25, dse_blk_c_n = fm_dse_blk_c_n, t_dse_blk_c_n
C?               dse_blk_sn_c_n = dse_blk_sn_c_n + 1
C?               if (iand(r_eta_msk, bt_a(dse_blk_sn_c_n)) .ne. 0) then
C?                  if (u_bs_dse_blk_pv_r_in_c(dse_blk_c_n) .eq.
C?     &                 dse_blk_r_n) then
C?                     eta_r_ch1(dse_blk_c_n) = '*'
C?                  else
C?                     eta_r_ch1(dse_blk_c_n) = '1'
C?                  endif
C?               else
C?                  eta_r_ch1(dse_blk_c_n) = '0'
C?               endif
C? 25         continue
C?            if (t_dse_blk_c_n .lt. dse_blk_n_c) then
C?               dse_blk_sn_n = dse_blk_sn_n + 1
C?               dse_blk_sn_os = dse_blk_sn_os + n_r
C?               goto 22
C?            endif
C?            write(*, 9040)dse_blk_r_n, (eta_r_ch1(dse_blk_c_n),
C?     &           dse_blk_c_n = 1, dse_blk_n_c)
C? 20      continue
C?
C?         if (n_r .lt. 100) then
C?            write(*, 9000)'Non-dense block etas',
C?     &           (dse_blk_c_n, dse_blk_c_n = 1, dse_blk_n_c)
C?            do 30, r_n = 1, n_r
C?               dse_blk_r_n = u_bs_gthr_pv_r(r_n)
C?               if (dse_blk_r_n .gt. 0) then
C?                  write(*, 9030)r_n, dse_blk_r_n,
C?     &                 ('-', dse_blk_c_n = 1, dse_blk_n_c)
C?               else
C?                  dse_blk_sn_n = 1
C?                  dse_blk_sn_os = 0
C?                  t_dse_blk_c_n = 0
C? 32               continue
C?                  fm_dse_blk_c_n = t_dse_blk_c_n + 1
C?                  t_dse_blk_c_n =
C?     &                 min(dse_blk_n_c, dse_blk_sn_n*dse_blk_u_sn_ln)
C?                  r_eta_msk = u_bs_eta_msk(dse_blk_sn_os+r_n)
C?                  dse_blk_sn_c_n = fm_dse_blk_c_n -
C?     &                 (dse_blk_sn_n-1)*dse_blk_u_sn_ln - 1
C?                  do 35, dse_blk_c_n = fm_dse_blk_c_n, t_dse_blk_c_n
C?                     dse_blk_sn_c_n = dse_blk_sn_c_n + 1
C?                     if (iand(r_eta_msk, bt_a(dse_blk_sn_c_n))
C?     &                    .ne. 0) then
C?                        eta_r_ch1(dse_blk_c_n) = '*'
C?                     else
C?                        eta_r_ch1(dse_blk_c_n) = '0'
C?                     endif
C? 35               continue
C?                  if (t_dse_blk_c_n .lt. dse_blk_n_c) then
C?                     dse_blk_sn_n = dse_blk_sn_n + 1
C?                     dse_blk_sn_os = dse_blk_sn_os + n_r
C?                     goto 32
C?                  endif
C?                  write(*, 9040)r_n, (eta_r_ch1(dse_blk_c_n),
C?     &                 dse_blk_c_n = 1, dse_blk_n_c)
C?               endif
C? 30         continue
C?         endif
C?      else
C?         write(*, 9100)'Pivotal row numbers',
C?     &        (dse_blk_c_n, dse_blk_c_n = 1, dse_blk_n_c)
C?         write(*, 9121)
C?     &        (u_bs_dse_blk_pv_r_in_c(dse_blk_c_n),
C?     &        dse_blk_c_n = 1, dse_blk_n_c)
C?
C?         write(*, 9100)'Dense block etas',
C?     &        (dse_blk_c_n, dse_blk_c_n = 1, dse_blk_n_c)
C?         do 120, dse_blk_r_n = 1, dse_blk_n_r
C?            r_n = u_bs_skt_pv_r(dse_blk_r_n)
C?            dse_blk_sn_n = 1
C?            dse_blk_sn_os = 0
C?            t_dse_blk_c_n = 0
C? 122        continue
C?            fm_dse_blk_c_n = t_dse_blk_c_n + 1
C?            t_dse_blk_c_n =
C?     &           min(dse_blk_n_c, dse_blk_sn_n*dse_blk_u_sn_ln)
C?            r_eta_msk = u_bs_eta_msk(dse_blk_sn_os+r_n)
C?            dse_blk_sn_c_n =
C?     &           fm_dse_blk_c_n - (dse_blk_sn_n-1)*dse_blk_u_sn_ln - 1
C?            do 125, dse_blk_c_n = fm_dse_blk_c_n, t_dse_blk_c_n
C?               dse_blk_sn_c_n = dse_blk_sn_c_n + 1
C?               if (iand(r_eta_msk, bt_a(dse_blk_sn_c_n)) .ne. 0) then
C?                  if (u_bs_dse_blk_pv_r_in_c(dse_blk_c_n) .eq.
C?     &                 dse_blk_r_n) then
C?                     eta_r_ch1(dse_blk_c_n) = '*'
C?                  else
C?                     eta_r_ch1(dse_blk_c_n) = '1'
C?                  endif
C?               else
C?                  eta_r_ch1(dse_blk_c_n) = '0'
C?               endif
C? 125        continue
C?            if (t_dse_blk_c_n .lt. dse_blk_n_c) then
C?               dse_blk_sn_n = dse_blk_sn_n + 1
C?               dse_blk_sn_os = dse_blk_sn_os + n_r
C?               goto 122
C?            endif
C?            write(*, 9140)dse_blk_r_n, u_bs_skt_pv_r(dse_blk_r_n),
C?     &           (eta_r_ch1(dse_blk_c_n),
C?     &            dse_blk_c_n = 1, dse_blk_n_c)
C? 120     continue
C?
C?         if (n_r .lt. 100) then
C?            write(*, 9100)'Non-dense block etas',
C?     &           (dse_blk_c_n, dse_blk_c_n = 1, dse_blk_n_c)
C?            do 130, r_n = 1, n_r
C?               dse_blk_r_n = u_bs_gthr_pv_r(r_n)
C?               if (dse_blk_r_n .gt. 0) then
C?                  write(*, 9130)r_n, dse_blk_r_n,
C?     &                 ('-', dse_blk_c_n = 1, dse_blk_n_c)
C?               else
C?                  dse_blk_sn_n = 1
C?                  dse_blk_sn_os = 0
C?                  t_dse_blk_c_n = 0
C? 132              continue
C?                  fm_dse_blk_c_n = t_dse_blk_c_n + 1
C?                  t_dse_blk_c_n =
C?     &                 min(dse_blk_n_c, dse_blk_sn_n*dse_blk_u_sn_ln)
C?                  r_eta_msk = u_bs_eta_msk(dse_blk_sn_os+r_n)
C?                  dse_blk_sn_c_n = fm_dse_blk_c_n -
C?     &                 (dse_blk_sn_n-1)*dse_blk_u_sn_ln - 1
C?                  do 135, dse_blk_c_n = fm_dse_blk_c_n, t_dse_blk_c_n
C?                     dse_blk_sn_c_n = dse_blk_sn_c_n + 1
C?                     if (iand(r_eta_msk, bt_a(dse_blk_sn_c_n))
C?     &                    .ne. 0) then
C?                        eta_r_ch1(dse_blk_c_n) = '1'
C?                     else
C?                        eta_r_ch1(dse_blk_c_n) = '0'
C?                     endif
C? 135              continue
C?                  if (t_dse_blk_c_n .lt. dse_blk_n_c) then
C?                     dse_blk_sn_n = dse_blk_sn_n + 1
C?                     dse_blk_sn_os = dse_blk_sn_os + n_r
C?                     goto 132
C?                  endif
C?                  write(*, 9141)r_n, (eta_r_ch1(dse_blk_c_n),
C?     &                 dse_blk_c_n = 1, dse_blk_n_c)
C?               endif
C? 130        continue
C?         endif
C?      endif
C? 7000 continue
C?      return
C? 9000 format(a/,
C?     &     11x,        10(5x, i2, 6x))
C? 9010 format(i2, 2x, i7, 10(1x, a12))
C? 9020 format(11x,        10(1x, a12))
C? 9021 format(11x,        10(5x, i2, 6x))
C? 9030 format(i2, 2x, i7, 10(6x, a1, 6x))
C? 9040 format(i2, 9x,     10(6x, a1, 6x))
C? 9100 format(a/,
C?     &       11x,        999(1x, i2))
C? 9121 format(11x,        999(1x, i2))
C? 9130 format(i2, 2x, i7, 999(2x, a1))
C? 9140 format(i2, 2x, i7, 999(2x, a1))
C? 9141 format(i2, 9x,     999(2x, a1))
C?      end
CM      ENDIF
 
C->>> ------------------------------------------> ems_iz_blk_ml_u_bs <<<
c     Sets up a block and the handles for basis update data.
c
      subroutine ems_iz_blk_ml_u_bs(is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      integer is(0:is_n_en_m1)
      integer dse_blk_u_n_sn
      integer blk_n, r_cf, c_cf, a_el_cf, cs, n_wo, p_ml_hdl
      integer dse_blk_dim
      integer mem_mgr_rt_cod
      integer rt_cod
 
      if (iand(ml_blk_st_msk,  ml_blk_st_ml_u_bs) .ne. 0) goto 8000
      dse_blk_dim = mx_n_u
      dse_blk_u_n_sn = (dse_blk_dim-1)/dse_blk_u_sn_ln + 1
      call ems_g_blk_ml_u_bs_n_wo(r_cf, c_cf, a_el_cf, cs, n_wo)
      call ems_mem_mgr_ope_blk(mem_mgr_rt_cod, is,
     &     n_wo, ope_blk_at_f_fr_p,
     &     cu_ml_n, u_bs_blk_id, blk_n)
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
     &     0, mx_n_c,
     &     0, mx_n_a_el,
     &     cs, n_wo, u_bs_blk_id)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         go to 7000
      endif
c
c     Get handles for the arrays within the block.
c
      p_ml_hdl = p_ml_bs_blk + (cu_ml_n-1)*ml_bs_blk_n_wo +
     &     ml_bs_blk_os_hdl
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, (1+dse_blk_dim)*(1+dse_blk_dim),
     &     rl_wo_z,
     &     is(p_ml_hdl+ix_hdl_u_bs_dse_blk))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n, 1+mx_n_r,  i_wo_z,
     &     is(p_ml_hdl+ix_hdl_u_bs_gthr_pv_r))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n,  1+dse_blk_dim, i_wo_z,
     &     is(p_ml_hdl+ix_hdl_u_bs_dse_blk_pv_r_in_c))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n,  1+dse_blk_dim, i_wo_z,
     &     is(p_ml_hdl+ix_hdl_u_bs_skt_pv_r))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_mem_mgr_nw_hdl(mem_mgr_rt_cod, is,
     &     blk_n,  1+dse_blk_u_n_sn*mx_n_r, i_wo_z,
     &     is(p_ml_hdl+ix_hdl_u_bs_eta_msk))
      if (ems_msg_cod .ge. ems_msg_lvl_serious) go to 7000
      call ems_g_ml_p(rt_cod, is)
      if (rt_cod .ne. 0) goto 8020
c
c     Indicate that the model now has space for update basis data.
c
      ml_blk_st_msk = ml_blk_st_msk + ml_blk_st_ml_u_bs
c
c     Record the number of updates for which the dense block is
c     dimensioned---since mx_n_u may be reduced.
c
      is(p_u_bs_skt_pv_r) = dse_blk_dim
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
 9800 format('Model already has space for update basis data')
 9802 format('Error in ems_g_ml_p')
      end
 
C->>> --------------------------------------> ems_g_blk_ml_u_bs_n_wo <<<
      subroutine ems_g_blk_ml_u_bs_n_wo(r_cf, c_cf, a_el_cf, cs, n_wo)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      integer r_cf, c_cf, a_el_cf, cs, n_wo
      integer dse_blk_dim, dse_blk_u_n_sn
      dse_blk_dim = mx_n_u
      dse_blk_u_n_sn = (dse_blk_dim-1)/dse_blk_u_sn_ln + 1
      r_cf = (1+dse_blk_u_n_sn)*i_wo_z
      c_cf = 0
      a_el_cf = 0
      cs = ((1+dse_blk_dim)*(1+dse_blk_dim))*rl_wo_z +
     &     (1 + 1+dse_blk_dim + 1+dse_blk_dim + 1)*i_wo_z
      n_wo = r_cf*mx_n_r + cs
      return
      end
 
C->>> ------------------------------------------> ems_rm_blk_ml_u_bs <<<
c     Removes the block for the update basis data.
c
      subroutine ems_rm_blk_ml_u_bs(is)
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
 
      if (iand(ml_blk_st_msk,  ml_blk_st_ml_u_bs) .eq. 0) goto 8000
      p_ml_hdl = p_ml_bs_blk + (cu_ml_n-1)*ml_bs_blk_n_wo +
     &     ml_bs_blk_os_hdl
      blk_n = is(p_ml_hdl+ix_blk_ml_u_bs)
      call ems_mem_mgr_rm_blk(mem_mgr_rt_cod, is,
     &     blk_n)
      if (mem_mgr_rt_cod .ge. mem_mgr_rt_lvl_serious) then
         ems_msg_cod = ems_msg_lvl_serious
         goto 7100
      endif
      ml_blk_st_msk = ml_blk_st_msk - ml_blk_st_ml_u_bs
 7000 continue
 7100 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9800 format('Model does not already have space for update basis data')
      end
