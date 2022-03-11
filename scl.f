C->>> -----------------------------------------> ems_ca_g_ml_scl_fac <<<
c     Call the routine to get the scaling factors for the model.
c
      subroutine ems_ca_g_ml_scl_fac(ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      double precision ds(0:ds_n_en_m1)
      integer is(0:is_n_en_m1)
      integer rl_wk_a_ix
      integer i_wk_a_ix1
      integer i_wk_a_ix2
 
      call ems_g_rsmi_rl_wk_a_ix(rl_wk_a_ix)
      if (rl_wk_a_ix .lt. 0) go to 8000
      call ems_g_rsmi_i_wk_a_ix(i_wk_a_ix1)
      if (i_wk_a_ix1 .lt. 0) go to 8000
      call ems_g_rsmi_i_wk_a_ix(i_wk_a_ix2)
      if (i_wk_a_ix2 .lt. 0) go to 8000
      if (scl_mode .eq. 1) then
         call ems_nw_g_ml_scl_fac(
     &        ds(p_mtx_r_v),
     &        is(p_mtx_r_ix),
     &        is(p_mtx_c_sa),
     &        ds(p_scl),
     &        ds(p_rsmi_rl_wk_a(rl_wk_a_ix)),
     &        is(p_rsmi_i_wk_a(i_wk_a_ix1)),
     &        ds, is)
      else
         call ems_g_ml_scl_fac(
     &        ds(p_mtx_r_v),
     &        is(p_mtx_r_ix),
     &        is(p_mtx_c_sa),
     &        ds(p_scl),
     &        ds(p_rsmi_rl_wk_a(rl_wk_a_ix)),
     &        is(p_rsmi_i_wk_a(i_wk_a_ix1)),
     &        is(p_rsmi_i_wk_a(i_wk_a_ix2)),
     &        ds, is)
      endif
      call ems_fr_rsmi_rl_wk_a_ix(rl_wk_a_ix)
      call ems_fr_rsmi_i_wk_a_ix(i_wk_a_ix1)
      call ems_fr_rsmi_i_wk_a_ix(i_wk_a_ix2)
 7100 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7100
 9800 format('RSMI workspace not available in ems_ca_g_ml_scl_fac')
      end
 
C->>> --------------------------------------------> ems_g_ml_scl_fac <<<
c     Geometric mean scaling
c
      subroutine ems_g_ml_scl_fac(
     &     mtx_r_v, mtx_r_ix, mtx_c_sa,
     &     scl, r_v_prod, r_k, n_r_v_prod_div,
     &     ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICS.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      integer mtx_c_sa(0:n_c+1)
      integer mtx_r_ix(0:n_a_el)
      integer r_k(0:n_r)
      integer n_r_v_prod_div(0:n_r)
      integer is(0:*)
      double precision mtx_r_v(0:n_a_el)
      double precision scl(0:mx_n_c+n_r)
      double precision r_v_prod(0:n_r)
c      double precision r_inf_norm(0:n_r)
      double precision ds(0:*)
      double precision mx_el, mn_el
      double precision r_scl, mx_r_scl, mn_r_scl
      double precision c_scl, mx_c_scl, mn_c_scl
      double precision r_v, c_v_prod
      double precision log_2, rcp_log_2, log_mx_scl_prod
      double precision mn_scl, mx_scl
      double precision mtx_mx_v, mtx_mn_nz
      double precision scl_mtx_mx_v, scl_mtx_mn_nz
      double precision r_norm
      double precision big_c_v_prod, sma_c_v_prod, prev_c_scl
      double precision big_r_v_prod, sma_r_v_prod, prev_r_scl
      double precision rcp_c_scl
      integer n_c_v_prod_div, c_k
      integer n_it, r_n, el_n, c_n, i_v, vr_n
      logical cg
 
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tt_da .gt. 0) call ems_tt_rec(scl_ml_tt, -1)
CM      ENDIF
      if (iand(ml_da_st_msk, ml_da_st_scl_ml) .ne. 0) then
c
c     If the model already has scaling factors then (if necessary)
c     un-scale its matrix and solution before calculating new scaling
c     factors.
c
         call ems_un_scl_ml_mtx(ds, is)
         call ems_un_scl_ml_sol(ds, is)
         ml_da_st_msk = ml_da_st_msk - ml_da_st_scl_ml
      else
         do 10, vr_n = 1, mx_n_c+n_r
            scl(vr_n) = one
 10      continue
      endif
      n_it = 0
      log_2 = log(two)
      rcp_log_2 = one/log(two)
      log_mx_scl_prod = log(mx_scl_prod)
      mn_scl = two**(-scl_bd)
      mx_scl = two**scl_bd
c
c     Perform an initial pass to scale rows so that the largest value
c     is one. Use r_v_prod to accumulate infinity norms.
c
      mtx_mn_nz = inf
      mtx_mx_v = zero
      mn_r_scl = inf
      mx_r_scl = zero
      do 110, r_n = 1, n_r
         r_v_prod(r_n) = zero
 110  continue
      do 130, c_n = 1, n_c
         do 120, el_n = mtx_c_sa(c_n), mtx_c_sa(c_n+1)-1
            r_n = mtx_r_ix(el_n)
            r_v = abs(mtx_r_v(el_n))
            if (r_v .gt. zero) mtx_mn_nz = min(r_v, mtx_mn_nz)
            r_v_prod(r_n) = max(r_v, r_v_prod(r_n))
 120     continue
 130  continue
      do 140, r_n = 1, n_r
         r_norm = r_v_prod(r_n)
         if (r_norm .le. scl_v_ze) goto 140
         mtx_mx_v = max(r_norm, mtx_mx_v)
         r_scl = log(r_norm)
         r_scl = r_scl*rcp_log_2
         i_v = -int(r_scl)
         r_scl = two**i_v
         mn_r_scl = min(scl(mx_n_c+r_n), mn_r_scl)
         mx_r_scl = max(scl(mx_n_c+r_n), mx_r_scl)
 140  continue
      if (mtx_mx_v .le. zero .or. mx_r_scl .le. zero) goto 8000
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     mtx_mn_nz, mtx_mx_v, mtx_mx_v/mtx_mn_nz
      call ems_msg_wr_li(info_msg_n)
c
c     Iterate between scaling by columns and scaling by rows.
c
 200  continue
      n_it = n_it + 1
      cg = .false.
      big_c_v_prod = mx_scl_prod/(mtx_mx_v*mx_r_scl)
      sma_c_v_prod = rcp_mx_scl_prod/(mtx_mn_nz*mn_r_scl)
      mx_r_scl = zero
      do 210 r_n = 1, n_r
         r_v_prod(r_n) = one
         r_k(r_n) = 0
         n_r_v_prod_div(r_n) = 0
 210  continue
      do 240, c_n = 1, n_c
         c_v_prod = one
         n_c_v_prod_div = 0
         c_k = 0
         do 220, el_n = mtx_c_sa(c_n), mtx_c_sa(c_n+1)-1
            r_n = mtx_r_ix(el_n)
            r_v = abs(mtx_r_v(el_n))
            if (r_v .le. scl_v_ze) goto 220
            r_v = r_v*scl(mx_n_c+r_n)
            if (c_v_prod .ge. big_c_v_prod) then
               n_c_v_prod_div = n_c_v_prod_div + 1
               c_v_prod = c_v_prod*rcp_mx_scl_prod
            else if (c_v_prod .le. sma_c_v_prod) then
               n_c_v_prod_div = n_c_v_prod_div - 1
               c_v_prod = c_v_prod*mx_scl_prod
            endif
            c_k = c_k + 1
            c_v_prod = c_v_prod*r_v
 220     continue
         if (c_k .eq. 0) goto 225
         c_scl = log(c_v_prod) + n_c_v_prod_div*log_mx_scl_prod
         c_scl = c_scl/float(c_k)
         c_scl = c_scl*rcp_log_2
         i_v = int(c_scl)
         c_scl = two**i_v
         prev_c_scl = scl(c_n)
         scl(c_n) = min(max(c_scl, mn_scl), mx_scl)
         cg = cg .or. abs(scl(c_n)-prev_c_scl) .ge. mn_scl
c
c     Now update the row products using the new column scale factor.
c
 225     continue
         c_scl = scl(c_n)
         rcp_c_scl = one/c_scl
         big_r_v_prod = mx_scl_prod/mtx_mx_v*c_scl
         sma_r_v_prod = rcp_mx_scl_prod/mtx_mn_nz*c_scl
         do 230, el_n = mtx_c_sa(c_n), mtx_c_sa(c_n+1)-1
            r_n = mtx_r_ix(el_n)
            r_v = abs(mtx_r_v(el_n))
            if (r_v .le. scl_v_ze) goto 230
            r_v = r_v*rcp_c_scl
            if (r_v_prod(r_n) .ge. big_r_v_prod) then
               n_r_v_prod_div(r_n) = n_r_v_prod_div(r_n) + 1
               r_v_prod(r_n) = r_v_prod(r_n)*rcp_mx_scl_prod
            else if (r_v_prod(r_n) .le. sma_r_v_prod) then
               n_r_v_prod_div(r_n) = n_r_v_prod_div(r_n) - 1
               r_v_prod(r_n) = r_v_prod(r_n)*mx_scl_prod
            endif
            r_k(r_n) = r_k(r_n) + 1
            r_v_prod(r_n) = r_v_prod(r_n)*r_v
 230     continue
 240  continue
      do 250, r_n = 1, n_r
         if (r_k(r_n) .eq. 0) goto 250
         r_scl = r_v_prod(r_n)
         r_scl = log(r_v_prod(r_n)) +
     &        n_r_v_prod_div(r_n)*log_mx_scl_prod
         r_scl = r_scl/float(r_k(r_n))
         r_scl =  r_scl*rcp_log_2
         i_v = -int(r_scl)
         r_scl = two**i_v
         prev_r_scl = scl(mx_n_c+r_n)
         scl(mx_n_c+r_n) = min(max(r_scl, mn_scl), mx_scl)
         cg = cg .or. abs(scl(mx_n_c+r_n)-prev_r_scl) .ge. mn_scl
         mx_r_scl = max(scl(mx_n_c+r_n), mx_r_scl)
 250  continue
      if (cg .and. n_it .lt. 6) goto 200
c
c     Scale the scaled matrix so that the maximum entry in each column
c     is about one.
c
      mn_r_scl = two*mx_scl
      mx_r_scl = zero
      mn_c_scl = two*mx_scl
      mx_c_scl = zero
      scl_mtx_mx_v = zero
      scl_mtx_mn_nz = inf
      do 320, c_n = 1, n_c
         mx_el = zero
         mn_el = inf
         do 310, el_n = mtx_c_sa(c_n), mtx_c_sa(c_n+1)-1
            r_n = mtx_r_ix(el_n)
            r_v = abs(mtx_r_v(el_n))*scl(mx_n_c+r_n)
            if (r_v .le. zero) goto 310
            mn_el = min(r_v, mn_el)
            mx_el = max(r_v, mx_el)
 310     continue
c
c     Find the greatest power of two less than the maximum entry in the
c     column and modify the scaling factor so that the maximum entry in
c     the scaled column is about one.
c
         if (mx_el .le. zero) goto 320
         c_scl = log(mx_el)*rcp_log_2
         if (c_scl .lt. -half) then
            i_v = int(c_scl-half)
         else
            i_v = int(c_scl+half)
         endif
         c_scl = two**i_v
         scl(c_n) = min(max(c_scl, mn_scl), mx_scl)
         c_scl = scl(c_n)
         rcp_c_scl = one/c_scl
         scl_mtx_mn_nz = min(mn_el*rcp_c_scl, scl_mtx_mn_nz)
         scl_mtx_mx_v = max(mx_el*rcp_c_scl, scl_mtx_mx_v)
         mn_c_scl = min(c_scl, mn_c_scl)
         mx_c_scl = max(c_scl, mx_c_scl)
 320  continue
      do 330, r_n = 1, n_r
         mn_r_scl = min(scl(mx_n_c+r_n), mn_r_scl)
         mx_r_scl = max(scl(mx_n_c+r_n), mx_r_scl)
 330  continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &     n_it, mn_c_scl, mx_c_scl
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9101)
     &     mn_r_scl, mx_r_scl
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9200)
     &     scl_mtx_mn_nz, scl_mtx_mx_v, scl_mtx_mx_v/scl_mtx_mn_nz
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9210)
     &     (mtx_mx_v/mtx_mn_nz)/(scl_mtx_mx_v/scl_mtx_mn_nz)
      call ems_msg_wr_li(info_msg_n)
c
c     Indicate that the model now has scaling factors.
c
      ml_da_st_msk = ml_da_st_msk + ml_da_st_scl_ml
 7000 continue
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tt_da .gt. 0) call ems_tt_rec(-scl_ml_tt, -1)
CM      ENDIF
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
     &     mtx_mx_v, mx_r_scl
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 9000 format('The initial range of the matrix coefficients is [',
     &     g11.4, ', ', g11.4, ']: a relative factor of ', g11.4)
 9100 format('After ', i1, ' scaling iterations, ',
     &     'the ranges of scaling factors',
     &     ' are [', g11.4, ', ', g11.4, '] for columns')
 9101 format(57x,
     &     ' and [', g11.4, ', ', g11.4, '] for rows.')
 9200 format('The scaled  range of the matrix coefficients is [',
     &     g11.4, ', ', g11.4, ']: a relative factor of ', g11.4)
 9210 format('Scaling has improved the relative size of',
     &     ' matrix entries by a factor of ', g11.4)
 9800 format('ERROR scaling model: mtx_mx_v, mx_r_scl = ',
     &     2(1x, g11.4))
      end
 
C->>> --------------------------------------------> ems_g_ml_scl_fac <<<
c     Arithmetic mean scaling
c
      subroutine ems_nw_g_ml_scl_fac(
     &     mtx_r_v, mtx_r_ix, mtx_c_sa,
     &     scl, r_v_sum, r_k,
     &     ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICS.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      integer mtx_c_sa(0:n_c+1)
      integer mtx_r_ix(0:n_a_el)
      integer r_k(0:n_r)
      integer is(0:*)
      double precision mtx_r_v(0:n_a_el)
      double precision scl(0:mx_n_c+n_r)
      double precision r_v_sum(0:n_r)
c      double precision r_inf_norm(0:n_r)
      double precision ds(0:*)
      double precision mx_el
      double precision r_scl, mx_r_scl, mn_r_scl
      double precision c_scl, mx_c_scl, mn_c_scl
      double precision r_v, c_v_sum
      double precision log_2, rcp_log_2
      double precision mn_scl, mx_scl
      double precision mtx_mx_v, mtx_mn_nz
      double precision scl_mtx_mx_v, scl_mtx_mn_nz
      double precision r_norm, prev_c_scl
      double precision rcp_c_scl, prev_r_scl, mn_el
      integer c_k
      integer n_it, r_n, el_n, c_n, i_v, vr_n
      logical cg
 
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tt_da .gt. 0) call ems_tt_rec(scl_ml_tt, -1)
CM      ENDIF
      if (iand(ml_da_st_msk, ml_da_st_scl_ml) .ne. 0) then
c
c     If the model already has scaling factors then (if necessary)
c     un-scale its matrix and solution before calculating new scaling
c     factors.
c
         call ems_un_scl_ml_mtx(ds, is)
         call ems_un_scl_ml_sol(ds, is)
         ml_da_st_msk = ml_da_st_msk - ml_da_st_scl_ml
      else
         do 10, vr_n = 1, mx_n_c+n_r
            scl(vr_n) = one
 10      continue
      endif
      n_it = 0
      log_2 = log(two)
      rcp_log_2 = one/log(two)
      mn_scl = two**(-scl_bd)
      mx_scl = two**scl_bd
c
c     Perform an initial pass to scale rows so that the average value
c     is one. Use r_v_sum to accumulate 1-norms.
c
      mtx_mn_nz = inf
      mtx_mx_v = zero
      mx_r_scl = zero
      do 110, r_n = 1, n_r
         r_v_sum(r_n) = zero
         r_k(r_n) = 0
 110  continue
      do 130, c_n = 1, n_c
         do 120, el_n = mtx_c_sa(c_n), mtx_c_sa(c_n+1)-1
            r_n = mtx_r_ix(el_n)
            r_v = abs(mtx_r_v(el_n))
            if (r_v .gt. zero) mtx_mn_nz = min(r_v, mtx_mn_nz)
            r_k(r_n) = r_k(r_n) + 1
            r_v_sum(r_n) = r_v_sum(r_n) + r_v
            mtx_mx_v = max(r_v, mtx_mx_v)
 120     continue
 130  continue
      do 140, r_n = 1, n_r
         if (r_k(r_n) .eq. 0) goto 140
         r_norm = r_v_sum(r_n)
         if (r_norm .le. scl_v_ze) goto 140
         r_scl = log(r_norm/float(r_k(r_n)))
         r_scl = r_scl*rcp_log_2
         i_v = -int(r_scl)
         r_scl = two**i_v
         mx_r_scl = max(scl(mx_n_c+r_n), mx_r_scl)
 140  continue
      if (mtx_mx_v .le. zero .or. mx_r_scl .le. zero) goto 8000
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     mtx_mn_nz, mtx_mx_v, mtx_mx_v/mtx_mn_nz
      call ems_msg_wr_li(info_msg_n)
c
c     Iterate between scaling by columns and scaling by rows.
c
 200  continue
      n_it = n_it + 1
      cg = .false.
      do 210 r_n = 1, n_r
         r_v_sum(r_n) = one
         r_k(r_n) = 0
 210  continue
      do 240, c_n = 1, n_c
         c_v_sum = one
         c_k = 0
         do 220, el_n = mtx_c_sa(c_n), mtx_c_sa(c_n+1)-1
            r_n = mtx_r_ix(el_n)
            r_v = abs(mtx_r_v(el_n))
            if (r_v .le. scl_v_ze) goto 220
            r_v = r_v*scl(mx_n_c+r_n)
            c_k = c_k + 1
            c_v_sum = c_v_sum + r_v
 220     continue
         if (c_k .eq. 0) goto 225
         c_scl = log(c_v_sum/float(c_k))
         c_scl = c_scl*rcp_log_2
         i_v = int(c_scl)
         c_scl = two**i_v
         prev_c_scl = scl(c_n)
         scl(c_n) = min(max(c_scl, mn_scl), mx_scl)
         cg = cg .or. abs(scl(c_n)-prev_c_scl) .ge. mn_scl
c
c     Now update the row products using the new column scale factor.
c
 225     continue
         c_scl = scl(c_n)
         rcp_c_scl = one/c_scl
         do 230, el_n = mtx_c_sa(c_n), mtx_c_sa(c_n+1)-1
            r_n = mtx_r_ix(el_n)
            r_v = abs(mtx_r_v(el_n))
            if (r_v .le. scl_v_ze) goto 230
            r_v = r_v*rcp_c_scl
            r_k(r_n) = r_k(r_n) + 1
            r_v_sum(r_n) = r_v_sum(r_n) + r_v
 230     continue
 240  continue
      do 250, r_n = 1, n_r
         if (r_k(r_n) .eq. 0) goto 250
         r_scl = r_v_sum(r_n)
         r_scl = log(r_v_sum(r_n)/float(r_k(r_n)))
         r_scl = r_scl*rcp_log_2
         i_v = -int(r_scl)
         r_scl = two**i_v
         prev_r_scl = scl(mx_n_c+r_n)
         scl(mx_n_c+r_n) = min(max(r_scl, mn_scl), mx_scl)
         cg = cg .or. abs(scl(mx_n_c+r_n)-prev_r_scl) .ge. mn_scl
         mx_r_scl = max(scl(mx_n_c+r_n), mx_r_scl)
 250  continue
      if (cg .and. n_it .lt. 6) goto 200
c
c     Scale the scaled matrix so that the maximum entry in each column
c     is about one.
c
      mn_r_scl = two*mx_scl
      mx_r_scl = zero
      mn_c_scl = two*mx_scl
      mx_c_scl = zero
      scl_mtx_mx_v = zero
      scl_mtx_mn_nz = inf
      do 320, c_n = 1, n_c
         mx_el = zero
         mn_el = inf
         do 310, el_n = mtx_c_sa(c_n), mtx_c_sa(c_n+1)-1
            r_n = mtx_r_ix(el_n)
            r_v = abs(mtx_r_v(el_n))*scl(mx_n_c+r_n)
            if (r_v .le. zero) goto 310
            mn_el = min(r_v, mn_el)
            mx_el = max(r_v, mx_el)
 310     continue
c
c     Find the greatest power of two less than the maximum entry in the
c     column and modify the scaling factor so that the maximum entry in
c     the scaled column is about one.
c
         if (mx_el .le. zero) goto 320
         c_scl = log(mx_el)*rcp_log_2
         if (c_scl .lt. -half) then
            i_v = int(c_scl-half)
         else
            i_v = int(c_scl+half)
         endif
         c_scl = two**i_v
         scl(c_n) = min(max(c_scl, mn_scl), mx_scl)
         c_scl = scl(c_n)
         rcp_c_scl = one/c_scl
         scl_mtx_mn_nz = min(mn_el*rcp_c_scl, scl_mtx_mn_nz)
         scl_mtx_mx_v = max(mx_el*rcp_c_scl, scl_mtx_mx_v)
         mn_c_scl = min(c_scl, mn_c_scl)
         mx_c_scl = max(c_scl, mx_c_scl)
 320  continue
      do 330, r_n = 1, n_r
         mn_r_scl = min(scl(mx_n_c+r_n), mn_r_scl)
         mx_r_scl = max(scl(mx_n_c+r_n), mx_r_scl)
 330  continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &     n_it, mn_c_scl, mx_c_scl
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9101)
     &     mn_r_scl, mx_r_scl
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9200)
     &     scl_mtx_mn_nz, scl_mtx_mx_v, scl_mtx_mx_v/scl_mtx_mn_nz
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9210)
     &     (mtx_mx_v/mtx_mn_nz)/(scl_mtx_mx_v/scl_mtx_mn_nz)
      call ems_msg_wr_li(info_msg_n)
c
c     Indicate that the model now has scaling factors.
c
      ml_da_st_msk = ml_da_st_msk + ml_da_st_scl_ml
 7000 continue
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tt_da .gt. 0) call ems_tt_rec(-scl_ml_tt, -1)
CM      ENDIF
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
     &     mtx_mx_v, mx_r_scl
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 9000 format('The initial range of the matrix coefficients is [',
     &     g11.4, ', ', g11.4, ']: a relative factor of ', g11.4)
 9100 format('After ', i1, ' scaling iterations, ',
     &     'the ranges of scaling factors',
     &     ' are [', g11.4, ', ', g11.4, '] for columns')
 9101 format(57x,
     &     ' and [', g11.4, ', ', g11.4, '] for rows.')
 9200 format('The scaled  range of the matrix coefficients is [',
     &     g11.4, ', ', g11.4, ']: a relative factor of ', g11.4)
 9210 format('Scaling has improved the relative size of',
     &     ' matrix entries by a factor of ', g11.4)
 9800 format('ERROR scaling model: mtx_mx_v, mx_r_scl = ',
     &     2(1x, g11.4))
      end
 
C->>> ----------------------------------------------> ems_scl_ml_sol <<<
c     Call the routine to scale the model activities
c
      subroutine ems_scl_ml_sol(ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      double precision ds(0:ds_n_en_m1)
      integer is(0:is_n_en_m1)
c
c     Ensure that the solution is not already scaled.
c
      if (iand(ml_da_st_msk, ml_da_st_scl_ml_sol) .ne. 0) goto 7000
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tt_da .gt. 0) call ems_tt_rec(scl_ml_sol_tt, -1)
CM      ENDIF
      call ems_scl_ml_act(ds(p_pr_act), ds(p_du_act), ds(p_scl))
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tt_da .gt. 0) call ems_tt_rec(-scl_ml_sol_tt, -1)
CM      ENDIF
      ml_da_st_msk = ml_da_st_msk + ml_da_st_scl_ml_sol
 7000 continue
      return
      end
 
C->>> ----------------------------------------------> ems_scl_ml_mtx <<<
c     Call the routine to scale the model matrix.
c
      subroutine ems_scl_ml_mtx(ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      double precision ds(0:ds_n_en_m1)
      integer is(0:is_n_en_m1)
c
c     Ensure that the matrix is not already scaled.
c
      if (iand(ml_da_st_msk, ml_da_st_scl_ml_mtx) .ne. 0) goto 7000
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tt_da .gt. 0) call ems_tt_rec(scl_ml_mtx_tt, -1)
CM      ENDIF
      call ems_scl_ml_c_mtx(ds(p_scl),
     &     ds(p_mtx_r_v), is(p_mtx_r_ix), is(p_mtx_c_sa))
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tt_da .gt. 0) call ems_tt_rec(-scl_ml_mtx_tt, -1)
CM      ENDIF
      ml_da_st_msk = ml_da_st_msk + ml_da_st_scl_ml_mtx
c
c     Indicate that the edge weight information is not correct.
c
      ml_da_st_msk = ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_ed_wt)
c
c     Indicate that the INVERT is not correct.
c
      rq_inv = rq_inv_scl_ml_mtx
      ml_da_st_msk =
     &     ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_inv)
 7000 continue
      return
      end
 
C->>> ----------------------------------------> ems_un_scl_ml_sol <<<
c     Call the routine to un-scale the model activities.
c
      subroutine ems_un_scl_ml_sol(ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      double precision ds(0:ds_n_en_m1)
      integer is(0:is_n_en_m1)
c
c     Ensure that the solution is not already unscaled.
c
      if (iand(ml_da_st_msk, ml_da_st_scl_ml_sol) .eq. 0) goto 7000
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tt_da .gt. 0) call ems_tt_rec(scl_ml_sol_tt, -1)
CM      ENDIF
      call ems_un_scl_ml_act(ds(p_pr_act), ds(p_du_act), ds(p_scl))
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tt_da .gt. 0) call ems_tt_rec(-scl_ml_sol_tt, -1)
CM      ENDIF
      ml_da_st_msk = ml_da_st_msk - ml_da_st_scl_ml_sol
 7000 continue
      return
      end
 
C->>> ----------------------------------------> ems_un_scl_ml_mtx <<<
c     Call the routine to un-scale the model matrix.
c
      subroutine ems_un_scl_ml_mtx(ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      double precision ds(0:ds_n_en_m1)
      integer is(0:is_n_en_m1)
c
c     Ensure that the matrix is not already unscaled.
c
      if (iand(ml_da_st_msk, ml_da_st_scl_ml_mtx) .eq. 0) goto 7000
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tt_da .gt. 0) call ems_tt_rec(scl_ml_mtx_tt, -1)
CM      ENDIF
      call ems_un_scl_ml_c_mtx(ds(p_scl),
     &     ds(p_mtx_r_v), is(p_mtx_r_ix), is(p_mtx_c_sa))
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (g_tt_da .gt. 0) call ems_tt_rec(-scl_ml_mtx_tt, -1)
CM      ENDIF
      ml_da_st_msk = ml_da_st_msk - ml_da_st_scl_ml_mtx
c
c     Indicate that the edge weight information is not correct.
c
      ml_da_st_msk = ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_ed_wt)
c
c     Indicate that the INVERT is not correct.
c
      rq_inv = rq_inv_scl_ml_mtx
      ml_da_st_msk =
     &     ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_inv)
 7000 continue
      return
      end
 
C->>> ----------------------------------------------> ems_scl_ml_act <<<
c     Scale the model activities.
c
      subroutine ems_scl_ml_act(
     &     pr_act, du_act, scl)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      double precision pr_act(0:mx_n_c+n_r), du_act(0:mx_n_c+n_r)
      double precision scl(0:mx_n_c+n_r)
      integer vr_n
 
      do 10, vr_n = 1, n_c
         pr_act(vr_n) = pr_act(vr_n)*scl(vr_n)
         du_act(vr_n) = du_act(vr_n)/scl(vr_n)
 10   continue
      do 20, vr_n = mx_n_c+1, mx_n_c+n_r
         pr_act(vr_n) = pr_act(vr_n)*scl(vr_n)
         du_act(vr_n) = du_act(vr_n)/scl(vr_n)
 20   continue
      return
      end
 
C->>> --------------------------------------------> ems_scl_ml_c_mtx <<<
c     Scale the model matrix.
c
      subroutine ems_scl_ml_c_mtx(scl, mtx_r_v, mtx_r_ix, mtx_c_sa)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      double precision scl(0:mx_n_c+n_r)
      double precision mtx_r_v(0:n_a_el)
      integer mtx_c_sa(0:n_c+1), mtx_r_ix(0:n_a_el)
      integer c_n, el_n
      double precision rcp_c_scl_v
 
      do 20, c_n = 1, n_c
         rcp_c_scl_v = one/scl(c_n)
         do 10, el_n = mtx_c_sa(c_n), mtx_c_sa(c_n+1)-1
            mtx_r_v(el_n) =
     &           mtx_r_v(el_n)*scl(mx_n_c+mtx_r_ix(el_n))*rcp_c_scl_v
 10      continue
 20   continue
      return
      end
 
C->>> -------------------------------------------> ems_un_scl_ml_act <<<
c     Un-scale the model activities.
c
      subroutine ems_un_scl_ml_act(pr_act, du_act, scl)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      double precision pr_act(0:mx_n_c+n_r), du_act(0:mx_n_c+n_r)
      double precision scl(0:mx_n_c+n_r)
      integer vr_n
 
      do 10, vr_n = 1, n_c
         pr_act(vr_n) = pr_act(vr_n)/scl(vr_n)
         du_act(vr_n) = du_act(vr_n)*scl(vr_n)
 10   continue
      do 20, vr_n = mx_n_c+1, mx_n_c+n_r
         pr_act(vr_n) = pr_act(vr_n)/scl(vr_n)
         du_act(vr_n) = du_act(vr_n)*scl(vr_n)
 20   continue
      return
      end
 
C->>> -----------------------------------------> ems_un_scl_ml_c_mtx <<<
c     Un-scale the model matrix.
c
      subroutine ems_un_scl_ml_c_mtx(scl, mtx_r_v, mtx_r_ix, mtx_c_sa)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      double precision scl(0:mx_n_c+n_r)
      double precision mtx_r_v(0:n_a_el)
      integer mtx_c_sa(0:n_c+1), mtx_r_ix(0:n_a_el)
      integer c_n, el_n
      double precision c_scl_v
 
      do 20, c_n = 1, n_c
         c_scl_v = scl(c_n)
         do 10, el_n = mtx_c_sa(c_n), mtx_c_sa(c_n+1)-1
            mtx_r_v(el_n) =
     &           mtx_r_v(el_n)*c_scl_v/scl(mx_n_c+mtx_r_ix(el_n))
 10      continue
 20   continue
      return
      end
