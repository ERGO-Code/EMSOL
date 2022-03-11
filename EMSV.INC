CM
      double precision zero, one, two, half, inf, undn
      parameter (
     &     zero = 0.0d0,
     &     one =  1.0d0,
     &     two =  2.0d0,
     &     half = 5.0d-1,
     &     inf =  1.0d31,
     &     undn = 1.1d31)
      integer irand_v1, irand_v2, irand_bs
      parameter (
     &     irand_v1 =   1366,
     &     irand_v2 = 150889,
     &     irand_bs = 714025)
 
      integer rl_wo_z, i_wo_z, ch_wo_z
c
c     When using dense row-wise storage of eta values in pivotal rows,
c     the nonzeros in all etas are stored by setting bits in a small
c     number of full-length integer vectors. Thus there is a limit
c     on the number of etas whose nonzeros can be stored in any one
c     such vector. This limit is given by the (non-sign) bits in an
c     integer.
c
      integer dse_blk_u_sn_ln
CM      IF (t3d_lib .EQ. 1) THEN
C?c     On the T3D double precision and integer words are both 64 bit.
C?c     This allows more updates (when using Dense row-wise storage of
C?c     etas in pivotal rows) and bit32 is no longer the sign bit.
C?
C?      parameter (
C?     &     rl_wo_z = 1,
C?     &     i_wo_z =  1,
C?     &     ch_wo_z = 1,
C?     &     dse_blk_u_sn_ln = 63)
C?      integer iz_bt_a_com_fg1, iz_bt_a_com_fg2
C?      integer bt_a(64)
C?      common/ems_com_bt_a/iz_bt_a_com_fg1, iz_bt_a_com_fg2, bt_a
C?      integer bt33, bt34, bt35, bt36, bt37, bt38, bt39, bt40
C?      integer bt41, bt42, bt43, bt44, bt45, bt46, bt47, bt48, bt49
C?      integer bt50, bt51, bt52, bt53, bt54, bt55, bt56, bt57, bt58
C?      integer bt59, bt60, bt61, bt62, bt63, bt64
C?      parameter (
C?     &     bt33 =     2**32,      bt34 =      2**33,
C?     &     bt35 =     2**34,      bt36 =      2**35,
C?     &     bt37 =     2**36,      bt38 =      2**37,
C?     &     bt39 =     2**38,      bt40 =      2**39,
C?     &     bt41 =     2**40,      bt42 =      2**41,
C?     &     bt43 =     2**42,      bt44 =      2**43,
C?     &     bt45 =     2**44,      bt46 =      2**45,
C?     &     bt47 =     2**46,      bt48 =      2**47,
C?     &     bt49 =     2**48,      bt50 =      2**49,
C?     &     bt51 =     2**50,      bt52 =      2**51,
C?     &     bt53 =     2**52,      bt54 =      2**53,
C?     &     bt55 =     2**54,      bt56 =      2**55,
C?     &     bt57 =     2**56,      bt58 =      2**57,
C?     &     bt59 =     2**58,      bt60 =      2**59,
C?     &     bt61 =     2**60,      bt62 =      2**61,
C?     &     bt63 =     2**62,      bt64 =      2**63)
CM      ELSE
      parameter (
     &     rl_wo_z =  2,
     &     i_wo_z =   1,
     &     ch_wo_z =  2,
     &     dse_blk_u_sn_ln = 31)
      integer iz_bt_a_com_fg1, iz_bt_a_com_fg2
      integer bt_a(32)
      common/ems_com_bt_a/iz_bt_a_com_fg1, iz_bt_a_com_fg2, bt_a
CM      ENDIF
c
c     Assigning a value to the 32nd bit is somewhat problematical. It
c     has to be 2**31 for the T3D since integers are 64-bit. When
c     compiling for machines with 32-bit integers, some compilers allow
c     only 2**31, some only -2147483648 and some both.
c
      integer bt32
CM      IF (sun_lib .EQ. 1) THEN
C?c
C?c     For SUNs, epcf** allows both but f77 requires 2**31
C?c
C?      parameter (bt32 = 2**31)
CM      ELSE IF (t3d_lib .EQ. 1) THEN
C?c
C?c     For T3D, have to use 2**31 since integers are 64-bit.
C?c
C?      parameter (bt32 = 2**31)
CM      ELSE IF (ibm_lib .EQ. 1) THEN
C?c
C?c     For IBM, can use -2147483648? or what?
C?c
C?      parameter (bt32 = -2147483648)
CM      ELSE IF (sgi_lib .EQ. 1) THEN
C?c
C?c     For SGI, can use -2147483648? or what?
C?c
C?      parameter (bt32 = -2147483648)
CM      ELSE IF (nt_lib .EQ. 1) THEN
C?c
C?c     For NT, can only use -2147483648
C?c
C?      parameter (bt32 = -2147483648)
CM      ELSE IF (fps_lib .EQ. 1) THEN
C?c
C?c     For Fortran Power Station, can use -2147483648? or what?
C?c
C?      parameter (bt32 = -2147483648)
CM      ELSE IF (ftn77_lib .EQ. 1) THEN
c
c     For Salford, can only use -2147483648
c
      parameter (bt32 = -2147483648)
CM      ENDIF
      double precision tt_t_ti_fac, ti_t_tt_fac
CM      IF (t3d_tt .EQ. 1) THEN
C?c
C?c     For a 150MHz alpha the conversion factors to/from seconds are:
C?c
C?      parameter (
C?     &     tt_t_ti_fac = 667d-11,
C?     &     ti_t_tt_fac = 150d+6)
CM      ELSE IF (pent_fq_90 .EQ. 1) THEN
C?c
C?c     For a 90MHz Pentium the conversion factors to/from seconds are:
C?c
C?      parameter (
C?     &     tt_t_ti_fac = 111d-11,
C?     &     ti_t_tt_fac =  90d+6)
CM      ELSE IF (pent_fq_135 .EQ. 1) THEN
C?c
C?c     For a 145MHz Pentium the conversion factors to/from seconds are:
C?c
C?      parameter (
C?     &     tt_t_ti_fac = 741d-11,
C?     &     ti_t_tt_fac = 135d+6)
CM      ELSE
c
c     Otherwise, raw times in seconds are stored.
c
      parameter (
     &     tt_t_ti_fac = 0d0,
     &     ti_t_tt_fac = 0d0)
CM      ENDIF
 
      integer sgn_bt
CM      IF (t3d_lib .EQ. 1) THEN
C?      parameter (sgn_bt = bt64)
CM      ELSE
      parameter (sgn_bt = bt32)
CM      ENDIF
      double precision mx_rl
      double precision mx_scl_prod
      double precision rcp_mx_scl_prod
      integer mx_scl_prod_exp
      parameter (
     &     mx_rl = 1d300,
     &     mx_scl_prod = 1d100,
     &     rcp_mx_scl_prod = 1d-100,
     &     mx_scl_prod_exp = 100)
c
c     Values less than this are not written out in prts etc
c
      double precision wr_rl_ze
      parameter (
     &     wr_rl_ze = 1.0d-12)
c
c     Ken's i_inf and i_undn
c
      integer i_inf, i_undn
      parameter (
c     &     i_inf =  88288287,
c     &     i_undn= -88288288)
CM      IF (t3d_lib .EQ. 1) THEN
C?     &     i_inf =  9223372036854775807,
C?     &     i_undn= bt64)
CM      ELSE
     &     i_inf =  2147483647,
     &     i_undn=  bt32)
CM      ENDIF
 
      integer bt1, bt2, bt3, bt4, bt5, bt6, bt7, bt8, bt9, bt10
      integer bt11, bt12, bt13, bt14, bt15, bt16, bt17, bt18, bt19
      integer bt20, bt21, bt22, bt23, bt24, bt25, bt26, bt27, bt28
      integer bt29, bt30, bt31
      parameter (
     &     bt1 =      1,          bt2 =       2,
     &     bt3 =      4,          bt4 =       2**3,
     &     bt5 =      2**4,       bt6 =       2**5,
     &     bt7 =      2**6,       bt8 =       2**7,
     &     bt9 =      2**8,       bt10 =      2**9,
     &     bt11 =     2**10,      bt12 =      2**11,
     &     bt13 =     2**12,      bt14 =      2**13,
     &     bt15 =     2**14,      bt16 =      2**15,
     &     bt17 =     2**16,      bt18 =      2**17,
     &     bt19 =     2**18,      bt20 =      2**19,
     &     bt21 =     2**20,      bt22 =      2**21,
     &     bt23 =     2**22,      bt24 =      2**23,
     &     bt25 =     2**24,      bt26 =      2**25,
     &     bt27 =     2**26,      bt28 =      2**27,
     &     bt29 =     2**28,      bt30 =      2**29,
     &     bt31 =     2**30)
 
