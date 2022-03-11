C->>> ---------------------------------------------------> ems_irand <<<
c     Returns a random integer in the range 0 to irand_bs-1.
c
c     If iflag = 0 then the generator returns the next random number in
c                  the sequence.
c     If iflag = 1 then the generator is restarted and the first random
c                  value is returned.
c
c     Otherwise iflag is used as a new seed for the random number
c     generator, and the first new random value is returned.
c
      integer function ems_irand(iflag)
      implicit none
      include 'EMSV.INC'
      integer iflag
      integer i_v
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_eq_i2
CM      ENDIF
      integer irand
      integer iz_irand_fg1
      integer iz_irand_fg2
      common/ems_irand_com/irand, iz_irand_fg1, iz_irand_fg2
 
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(iz_irand_fg1, iz_irand_fg2)) then
CM      ELSE
      if (iz_irand_fg1 .eq. iz_irand_fg2) then
CM      ENDIF
         irand = 61164
         iz_irand_fg1 = 1
         iz_irand_fg2 = 2
      endif
      if (iflag .eq. 1) irand = 61164
      if (iflag .lt. 0) then
c
c     Make sure that the initial value of irand---taken from a negative
c     seed is in [1-irand_bs, 0]
c
         irand = iflag
         i_v = irand/irand_bs - 1
         irand = irand - i_v*irand_bs
c
c     Form the first random number in the sequence and make sure it is
c     in [0, irand_bs-1]
c
         irand = irand*irand_v1 + irand_v2
         if (irand .gt. 0) then
            i_v = irand/irand_bs
         else
            i_v = irand/irand_bs - 1
         endif
         irand = irand - i_v*irand_bs
      else if (iflag .gt. 1) then
c
c     Make sure that the initial value of irand---taken from a positive
c     seed is in [0, irand_bs-1]
c
         irand = iflag
         i_v = irand/irand_bs
         irand = irand - i_v*irand_bs
      endif
      irand = irand*irand_v1 + irand_v2
      i_v = irand/irand_bs
      irand = irand - i_v*irand_bs
 
      ems_irand = irand
      return
      end
 
C->>> ------------------------------------------> ems_g_cu_rand_seed <<<
c     Passes the current seed(s) back to the user
c
      subroutine ems_ems_g_cu_rand_seed(cu_rand_seed)
      implicit none
      include 'EMSV.INC'
      integer cu_rand_seed
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_eq_i2
CM      ENDIF
      integer irand
      integer iz_irand_fg1
      integer iz_irand_fg2
      common/ems_irand_com/irand, iz_irand_fg1, iz_irand_fg2
 
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(iz_irand_fg1, iz_irand_fg2)) then
CM      ELSE
      if (iz_irand_fg1 .eq. iz_irand_fg2) then
CM      ENDIF
         irand = 61164
         iz_irand_fg1 = 1
         iz_irand_fg2 = 2
      endif
      cu_rand_seed = irand
      return
      end
 
C->>> ---------------------------------------------------> ems_drand <<<
c     Returns a random double precision value in the interval [0, 1]
c
c     If iflag = 0 then the generator returns the next random number in
c                  the sequence.
c     If iflag = 1 then the generator is restarted and the first random
c                  value is returned.
c     Otherwise iflag is used as a new seed for the random number
c     generator, and the first new random value is returned.
c
      double precision function ems_drand(iflag)
      implicit none
      include 'EMSV.INC'
      double precision rcp_irand_bs_m1
      parameter (rcp_irand_bs_m1 = 1d0/(irand_bs-1))
      integer iflag
      integer ems_irand
      integer irand
      double precision drand
 
      irand = ems_irand(iflag)
      drand = float(irand)*rcp_irand_bs_m1
      ems_drand = drand
      return
      end
