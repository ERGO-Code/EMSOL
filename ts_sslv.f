c     Tests ems_sslv
c
      subroutine ts_ems_sslv(rt_cod, ds, is, ds_n_en, is_n_en, rd_ml_cn)
      implicit none
      include 'EMSV.INC'
      include 'EMSOLC.INC'
      include 'EMSOLI.INC'
      include 'EMSOLN.INC'
      include 'EMSOLR.INC'
      integer rt_cod, ds_n_en, is_n_en, rd_ml_cn
      double precision ds(ds_n_en)
      integer is(is_n_en)
      integer wr_ml, scl_ml, prsl_ml
      integer crsh_ty
      integer rd_bs, wr_bs
      integer cu_rq_ds_n_en, mx_rq_ds_n_en
      integer prsl_msk
      integer sv_IMAXFACTOR
      character*8 ml_nm
      integer ml_nm_ln
CM      IF (emsol_oc .EQ. 1) THEN
      integer oc_rp_msk
CM      ENDIF
      integer n_ca
      integer type
      integer mps_ou_cn
      integer bs_rd_cn, ml_wr_cn, bs_wr_cn, prsl_wr_cn
      integer alg, lp_iz_mode
      integer c_n, n_c, r_n, n_r, n_a_el
      integer rq_mem_lb, rq_mem_est, rq_mem_ub
 
      logical only_rd_mps
c      parameter (only_rd_mps = .true.)
      parameter (only_rd_mps = .false.)
 
      logical only_wr_mps
c      parameter (only_wr_mps = .true.)
      parameter (only_wr_mps = .false.)
 
      logical cf_ed_code
c      parameter (cf_ed_code = .true.)
      parameter (cf_ed_code = .false.)
 
      logical wr_act
      parameter (wr_act = .false.)
      character*60 ch60_sys_cmd_li
      character*60 ch60_bl
      save n_ca
      save type
      save mps_ou_cn
      save bs_rd_cn, ml_wr_cn, bs_wr_cn, prsl_wr_cn
      save alg, lp_iz_mode
      data n_ca/0/
      data type/2/
      data mps_ou_cn/25/
      data bs_rd_cn/16/ml_wr_cn/25/bs_wr_cn/26/prsl_wr_cn/50/
      data alg/1/lp_iz_mode/1/
 
      ch60_bl =
     &     '                              '//
     &     '                              '
      n_ca = n_ca + 1
      call ems_mset(rt_cod, ds, 1, 0, 256, 0, 0, 9999, 1)
      if (n_ca .gt. 1) then
         call ems_init(rt_cod, ds)
         if (rt_cod .gt. 0) call chkrt('ems_init', rt_cod)
      endif
      call ems_dsca(rt_cod, ds, ds_n_en, 1)
      if (rt_cod .gt. 0) call chkrt('ems_dsca', rt_cod)
c
c     Force checking (1) and reporting/checking (2) by setting
c     is(ix_mem_mgr_ck_rp_mode).
c
      is(1+17) = 0
c
c     Initialise the memory manager and set the control variables to
c     theit default values.
c
      call ems_dscm(rt_cod, ds, 1, 0)
      if (rt_cod .gt. 0) call chkrt('ems_dscm', rt_cod)
 
      call ems_iget(rt_cod, ds, emsoli, emsoliln)
      if (rt_cod .gt. 0) call chkrt('ems_iget', rt_cod)
      ILINELEN = 160
      call ems_iset(rt_cod, ds, emsoli, emsoliln)
      if (rt_cod .gt. 0) call chkrt('ems_iset', rt_cod)
 
      call ems_iget(rt_cod, ds, emsoli, emsoliln)
      if (rt_cod .gt. 0) call chkrt('ems_iget', rt_cod)
      call ems_rget(rt_cod, ds, emsolr, emsolrln)
      if (rt_cod .gt. 0) call chkrt('ems_rget', rt_cod)
CM      IF (emsol_xa .EQ. 1) THEN
      call ems_rd_ct_vr(rt_cod, 'ct_vr.dat',
     &     emsoliln, emsolrln, emsoli, emsolr)
      call ems_rd_ct_vr(rt_cod, 'dl_ct_vr.dat',
     &     emsoliln, emsolrln, emsoli, emsolr)
 
      call ems_iset(rt_cod, ds, emsoli, emsoliln)
      if (rt_cod .gt. 0) call chkrt('ems_iset', rt_cod)
      call ems_rset(rt_cod, ds, emsolr, emsolrln)
      if (rt_cod .gt. 0) call chkrt('ems_rset', rt_cod)
c
c     Read in the LP problem from an MPS file.
c
      call ems_mps(rt_cod, ds, rd_ml_cn, type, mps_ou_cn)
      if (rt_cod .gt. 0) call chkrt('ems_mps', rt_cod)
 
      if (only_rd_mps) goto 7000
 
      call ems_cget(rt_cod, ds, emsolc, emsolcln)
      if (rt_cod .gt. 0) call chkrt('ems_cget', rt_cod)
      if (CNAME .eq. 'OPTANYRF') then
         call ems_rget(rt_cod, ds, emsolr, emsolrln)
         if (rt_cod .gt. 0) call chkrt('ems_rget', rt_cod)
         RMAXMIN = -1d0
         call ems_rset(rt_cod, ds, emsolr, emsolrln)
         if (rt_cod .gt. 0) call chkrt('ems_rset', rt_cod)
      endif
 
      if (cf_ed_code) then
         call ems_iget(rt_cod, ds, emsoli, emsoliln)
         if (rt_cod .gt. 0) call chkrt('ems_iget', rt_cod)
         IMAXFACTOR = (50 + INUMROWS/200)+1
         print*, '=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+='
         print*, 'Setting IMAXFACTOR = ', IMAXFACTOR
         print*, '=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+='
         call ems_iset(rt_cod, ds, emsoli, emsoliln)
         if (rt_cod .gt. 0) call chkrt('ems_iset', rt_cod)
      endif
c
c=======================================================================
c     Write model section
c
      wr_ml = 0
      if (only_wr_mps) wr_ml = 2
c      print*,'Write out the model? MPS/EMS/No 2/1/0'
c      read*, wr_ml
      if (wr_ml .gt. 0) then
         print*,'Writing out the model!!!'
         if (wr_ml .eq. 1) then
            open(unit=ml_wr_cn, file = 'wr_ml.ems')
         else
            open(unit=ml_wr_cn, file = 'wr_ml.mps')
            call ems_iget(rt_cod, ds, emsoli, emsoliln)
            if (rt_cod .gt. 0) call chkrt('ems_iget', rt_cod)
            emsoli(80) = -1
            call ems_iset(rt_cod, ds, emsoli, emsoliln)
            if (rt_cod .gt. 0) call chkrt('ems_iset', rt_cod)
         endif
         call ems_bcdo(rt_cod, ds, ml_wr_cn, 2, 2)
         close(ml_wr_cn)
         if (rt_cod .gt. 0) call chkrt('ems_bcdo', rt_cod)
c         stop 'Just writing out model'
      end if
 
      if (only_wr_mps) goto 7000
 
c
c=======================================================================
c     Scale model section
c
      print*,'Entering scaling section'
      scl_ml = 1
      if (cf_ed_code) scl_ml = 0
c      print*,'Scale model? 1/0'
c      read*, scl_ml
      if (scl_ml .le. 0) print*, '!!! NO SCALING!!!'
      if (scl_ml .gt. 0) then
         call ems_scal(rt_cod, ds)
         if (rt_cod .gt. 0) call chkrt('ems_scal', rt_cod)
      endif
c      goto 7000
c
c=======================================================================
c     Initialise basis section
c
c     rd_bs < 0: Crash
c     rd_bs = 0: Logical basis
c     rd_bs > 0: Read basis
c
      rd_bs = -1
      rd_bs = 0
      if (cf_ed_code) rd_bs = 0
      print*, 'Enter rd_bs'
      print*,'  -2: Crash: JH'
      print*,'  -1: Crash: LTSSF'
      print*,'   0: Logical basis'
      print*,'   1: Read CPLEX crash basis'
      print*,'   2: Read existing bs.mps basis'
      print*,'   3: Read Integra basis'
!      rd_bs = 3
c      read*, rd_bs
c
      if (rd_bs .eq. 1) then
c
c     NB: CPLEX/OSL crash requires ml_mps_fi_nm
c
         print*, 'Cannot start from CPLEX/OSL basis: Using LTSSF Crash'
         rd_bs = -1
      endif
 
      if (rd_bs .le. 0) then
c
c     Perform crash or start from logical basis
c
         crsh_ty = -rd_bs
         print*,'Crash model?'
         print*,'   0: No crash'
         print*,'   1: LTSSF'
         print*,'   2: JH'
c         read*, crsh_ty
         print*,'****************************************'
         print*,'Entering crash section', crsh_ty
         print*,'****************************************'
         if (crsh_ty .gt. 0) then
            call ems_crsh(rt_cod, ds, crsh_ty)
            if (rt_cod .gt. 0) call chkrt('ems_crsh', rt_cod)
         endif
      else if (rd_bs .eq. 3) then
         open(unit = bs_rd_cn, file = 'lastas.dat')
         call ems_rd_integra_bs(bs_rd_cn, ds, is)
         close(bs_rd_cn)
c         call ems_rp_vr('After basis ', ds, is, 3)
      end if
c
c=======================================================================
c     Write basis section
c
      wr_bs = 0
c      wr_bs = 2
c      print*,'Write out a basis? Enter val+1 or 0 for no basis'
c      read*, wr_bs
      if (wr_bs .gt. 0) then
         wr_bs = wr_bs - 1
         open(unit = bs_wr_cn, file = 'ml.bs')
         call ems_baso(rt_cod, ds, bs_wr_cn, wr_bs)
         if (rt_cod .gt. 0) call chkrt('ekkbaso ', rt_cod)
         close(bs_wr_cn)
      endif
c
c     Call the LP solver.
c
c     Set lp_iz_mode
c
      lp_iz_mode = 1
      write(*, 9010)'SSLV Lp Iz Mode? ', bt1, ' => Ca RSMI Iz'
      write(*, 9010)'                 ', bt2, ' => Iz Bs Fm St'
c      read*, lp_iz_mode
 
      print*,'Entering solver section'
 
      call ems_iget(rt_cod, ds, emsoli, emsoliln)
      if (rt_cod .gt. 0) call chkrt('ems_iget', rt_cod)
      n_r = INUMROWS
      n_c = INUMCOLS
      n_a_el = emsoli(84)
      call ems_memrq(
     &     rt_cod, is,
     &     1, n_r, n_c, n_a_el, 0,
     &     rq_mem_lb, rq_mem_est, rq_mem_ub)
 
      call ems_sslv(rt_cod, ds, alg, lp_iz_mode)
      if (rt_cod .gt. 0) call chkrt('ems_sslv', rt_cod)
CM      IF (emsol_oc .EQ. 1) THEN
      if (oc_rp_msk .gt. 0) close(51)
CM      ENDIF
c
      call ems_iget(rt_cod, ds, emsoli, emsoliln)
      if (rt_cod .gt. 0) call chkrt('ems_iget', rt_cod)
 
      call ems_rget(rt_cod, ds, emsolr, emsolrln)
      if (rt_cod .gt. 0) call chkrt('ems_rget', rt_cod)
c
c=======================================================================
c     Residual report section
c
c      tl_r_pr_act_rsdu = 1d-6
c      tl_c_pr_act_rsdu = 1d-6
c      tl_rdc_co_rsdu = 1d-6
c      tl_r_rsdu = 1d-6
c 
c      call ems_rsdu(rt_cod, ds,
c     &     tl_r_pr_act_rsdu, tl_c_pr_act_rsdu,
c     &     tl_rdc_co_rsdu, tl_r_rsdu,
c     &     n_ifs_rec, su_ifs_rec, mx_ifs_rec)
c      if (rt_cod .gt. 0) call chkrt('ems_rsdu', rt_cod)
 
c      call ems_rsdu(rt_cod, ds,
c     &     0d0, 0d0, 0d0, 0d0,
c     &     n_ifs_rec, su_ifs_rec, mx_ifs_rec)
c      if (rt_cod .gt. 0) call chkrt('ems_rsdu', rt_cod)
c
c=======================================================================
c     Memory report section
c
      call ems_g_rq_ds_n_en(rt_cod, is, 0, cu_rq_ds_n_en, mx_rq_ds_n_en)
      if (rt_cod .gt. 0) call chkrt('ems_g_rq_ds_n_en', rt_cod)
      write(*, 9100)cu_rq_ds_n_en, mx_rq_ds_n_en, ds_n_en
      write(*, 9101)'Unknown',
     &     n_r, n_c, n_a_el,
     &     cu_rq_ds_n_en, mx_rq_ds_n_en, ds_n_en,
     &     rq_mem_lb, rq_mem_est, rq_mem_ub
 9101 format('MemUse: ,', a, 9(',', i9))
      if (rt_cod.gt. 0) then
         call ems_smap(rt_cod, ds)
         if (rt_cod .gt. 0) call chkrt('ems_smap', rt_cod)
      endif
c
c     Write the results.
c
      call ems_prts(rt_cod, ds)
      if (rt_cod .gt. 0) call chkrt('ems_prts', rt_cod)
c
c     Report on memory usage
c
      call ems_g_rq_ds_n_en(rt_cod, is, 0, cu_rq_ds_n_en, mx_rq_ds_n_en)
      if (rt_cod .gt. 0) call chkrt('ems_g_rq_ds_n_en', rt_cod)

      write(*, 9110)IPROBSTAT, IITERNUM, ROBJVALUE
 
 7000 continue
      return
 9010 format(a, i2, a)
 9100 format(
     &     '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'/
     &     '% Current number of double words required = ', i9, ' %'/
     &     '% Maximum number of double words required = ', i9, ' %'/
     &     '%         number of double words supplied = ', i9, ' %'/
     &     '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
 9110 format('CSV:,', i2, ', ', i9, ', ', g15.8)
      end
 
      subroutine ems_rd_integra_bs(bs_rd_cn, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSPM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      integer bs_rd_cn, is(0:*)
      double precision ds(0:*)
      character*32 ch32_st
      integer ix_n, vr_n, vr_st
      integer n_bc_vr, n_non_bc_vr
      integer bc_bt_ch_n
      parameter (bc_bt_ch_n = 31)
      logical rp
 
      n_bc_vr = 0
      n_non_bc_vr = 0
      read(bs_rd_cn, *)
      do 10, ix_n = 1, n_c+n_r
         rp = ix_n .le. 10
         if (ix_n .le. n_c) then
            vr_n = ix_n
         else
            vr_n = ix_n + mx_n_c-n_c
         endif
         read(bs_rd_cn, '(a32)')ch32_st
         vr_st = is(p_st+vr_n)
         vr_st = vr_st - iand(vr_st, bc_bt)
         if (ch32_st(bc_bt_ch_n:bc_bt_ch_n) .eq. '1') then
            vr_st = vr_st + bc_bt
            n_bc_vr = n_bc_vr + 1
         else
            n_non_bc_vr = n_non_bc_vr + 1
         endif
         if (rp) print*, ix_n, ch32_st, vr_st
         is(p_st+vr_n) = vr_st
 10   continue
      print*, 'Read Integra basis: '
      write(*, 9000)n_bc_vr, n_r, 'basic'
      write(*, 9000)n_non_bc_vr, n_c, 'nonbasic'
      return
 9000 format(i7 , ' of ', i7, 1x, a8, 1x, ' variables')
      end

