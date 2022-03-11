CM
c*********************************************************************
C
c  equivalence statements for the EMSOL real control variables.
c  These may be included in your program to allow referencing
c  EMSOL control variables by mnemonic names.
C
c*********************************************************************
c  Array for real control variables.
      integer emsolrln
      parameter (emsolrln = 150)
      double precision emsolr(emsolrln)
      common/ems_com_emsolr/emsolr
c
c     The allowed amount of primal infeasibility.
c
      double precision RTOLPINF
      equivalence (emsolr( 1), RTOLPINF)
c
c     The allowed amount of dual infeasibility.
c
      double precision RTOLDINF
      equivalence (emsolr( 2), RTOLDINF)
c
c     The weight of the linear objective.
c
      double precision RMAXMIN
      equivalence (emsolr( 3), RMAXMIN)
c
c     The reduction factor for mu in the primal barrier algorithm.
c
      double precision RMUFACTOR
      equivalence (emsolr( 4), RMUFACTOR)
c
c     The lower limit for mu in the primal barrier algorithm.
c
      double precision RMULIMIT
      equivalence (emsolr( 5), RMULIMIT)
c
c     The reduced gradient target reduction factor.
c
      double precision RRGFACTOR
      equivalence (emsolr( 6), RRGFACTOR)
c
c     The reduced gradient limit for the primal barrier algorithm.
c
      double precision RRGLIMIT
      equivalence (emsolr( 7), RRGLIMIT)
c
c     The tolerance for fixing variables in the barrier method when
c     infeasible.
c
      double precision RFIXVAR1
      equivalence (emsolr( 8), RFIXVAR1)
c
c     The tolerance for fixing variables in the barrier method when
c     feasible.
c
      double precision RFIXVAR2
      equivalence (emsolr( 9), RFIXVAR2)
c
c     The absolute pivot tolerance for Cholesky factorization.
c
      double precision RCHOLABSTOL
      equivalence (emsolr(10), RCHOLABSTOL)
c
c     The cut-off tolerance for Cholesky factorization.
c
      double precision RCHOLTINYTOL
      equivalence (emsolr(11), RCHOLTINYTOL)
      double precision RCHOLRELTOL
      equivalence (emsolr(11), RCHOLRELTOL)
c
c     The multiple of mu to add to the linear objective.
c
      double precision RMULINFAC
      equivalence (emsolr(12), RMULINFAC)
c
c     The projection error tolerance.
c
      double precision RPROJTOL
      equivalence (emsolr(13), RPROJTOL)
c
c     The multiplier of the feasible objective that is used when the
c     current solution is primal infeasible.
c
      double precision RPWEIGHT
      equivalence (emsolr(14), RPWEIGHT)
c
c     The rate of change for Rpweight or Rdweight.
c
      double precision RCHANGEWEIGHT
      equivalence (emsolr(15), RCHANGEWEIGHT)
c
c     The cutoff for the branch and bound.
c
      double precision RBBCUTOFF
      equivalence (emsolr(16), RBBCUTOFF)
c
c     The proportion of the feasible objective that is used when the
c     current solution is dual infeasible.
c
      double precision RDWEIGHT
      equivalence (emsolr(17), RDWEIGHT)
c
c     The value of the objective function.
c
      double precision ROBJVALUE
      equivalence (emsolr(18), ROBJVALUE)
c
c     The sum of the primal infeasibilities.
c
      double precision RSUMPINF
      equivalence (emsolr(19), RSUMPINF)
c
c     The sum of the dual infeasibilities.
c
      double precision RSUMDINF
      equivalence (emsolr(20), RSUMDINF)
c
c     The zero tolerance for data in MPS and spreadsheet format.
c
      double precision RTOLMPS
      equivalence (emsolr(21), RTOLMPS)
c
c     The scale factor for all degradation.
c
      double precision RDEGSCALE
      equivalence (emsolr(22), RDEGSCALE)
c
c     The best feasible integer solution found so far.
c
      double precision RBESTSOL
      equivalence (emsolr(23), RBESTSOL)
c
c     The weight for each integer infeasibility.
c
      double precision RIWEIGHT
      equivalence (emsolr(24), RIWEIGHT)
c
c     The amount by which a new solution must be better.
c
      double precision RIMPROVE
      equivalence (emsolr(25), RIMPROVE)
c
c     The value of the target solution.
c
      double precision RTARGET
      equivalence (emsolr(26), RTARGET)
c
c     The integer tolerance.
c
      double precision RTOLINT
      equivalence (emsolr(27), RTOLINT)
c
c     The best possible solution.
c
      double precision RBESTPOSS
      equivalence (emsolr(28), RBESTPOSS)
c
c     The best estimated solution.
c
      double precision RBESTEST
      equivalence (emsolr(29), RBESTEST)
c
c     The step-length multiplier for the primal barrier algorithm.
c
      double precision RSTEPMULT
      equivalence (emsolr(30), RSTEPMULT)
c
c     The initial value of mu for the primal barrier algorithm.
c
      double precision RMUINIT
      equivalence (emsolr(31), RMUINIT)
c
c     The density threshold for Cholesky processing.
c
      double precision RDENSETHR
      equivalence (emsolr(32), RDENSETHR)
c
c     The weight given to true objective in primal composite phase 1.
c
      double precision ROBJWEIGHT
      equivalence (emsolr(33), ROBJWEIGHT)
c
c     The value of the ems_QPAR parametric parameter lambda.
c
      double precision RLAMBDAVAL
      equivalence (emsolr(34), RLAMBDAVAL)
c
c     The value of the ems_QSLV decomposition cutoff.
c
      double precision RDCCUTOFF
      equivalence (emsolr(35), RDCCUTOFF)
c
c     The value of the dual objective for ems_BSLV.
c
      double precision RDOBJVAL
      equivalence (emsolr(36), RDOBJVAL)
c
c     The value of the ems_SPAR parameter lambda.
c
      double precision RSLAMBDA
      equivalence (emsolr(37), RSLAMBDA)
c
c     The limiting value for the ems_SPAR parameter lambda.
c
      double precision RSLAMBDALIM
      equivalence (emsolr(38), RSLAMBDALIM)
c
c     The incrementing value for the ems_SPAR parameter lambda.
c
      double precision RSLAMBDADELTA
      equivalence (emsolr(39), RSLAMBDADELTA)
c
c     The supernode processing threshold.
c
      double precision RTHRESHOLD
      equivalence (emsolr(40), RTHRESHOLD)
c
c     The barrier method primal-dual gap tolerance.
c
      double precision RPDGAPTOL
      equivalence (emsolr(41), RPDGAPTOL)
c
c     The primal-dual barrier method step-length multiplier.
c
      double precision RPDSTEPMULT
      equivalence (emsolr(42), RPDSTEPMULT)
c
c     The diagonal perturbation for Cholesky factorization.
c
      double precision RPERTDIAG
      equivalence (emsolr(43), RPERTDIAG)
c
c     The sample size for the ems_NSLV pricing algorithm.
c
      double precision RNETSAMP
      equivalence (emsolr(44), RNETSAMP)
c
c     Switch to write CPU time used by EMSOL subroutines.
c
      double precision RPRINTCPU
      equivalence (emsolr(45), RPRINTCPU)
