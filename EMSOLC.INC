CM
c*********************************************************************
C
c  equivalence statements for the EMSOL character control variables.
c  These may be included in your program to allow referencing
c  EMSOL control variables by mnemonic names.
C
c*********************************************************************
c
c     Array for character control variables.
c
      integer emsolcln
      parameter (emsolcln = 17)
      character*8 emsolc(emsolcln)
      common/ems_com_emsolc/emsolc
c
c     The problem name in the MPS file.
c
      character*8 CNAME
      equivalence (emsolc( 1), CNAME)
c
c     The objective function row name in the MPS file.
c
      character*8 COBJECTIVE
      equivalence (emsolc( 2), COBJECTIVE)
c
c     The RHS name in the MPS file.
c
      character*8 CRHS
      equivalence (emsolc( 3), CRHS)
c
c     The range name in the MPS file.
c
      character*8 CRANGE
      equivalence (emsolc( 4), CRANGE)
c
c     The bound name in the MPS file.
c
      character*8 CBOUND
      equivalence (emsolc( 5), CBOUND)
c
c     The basis name in the MPS file.
c
      character*8 CBASIS
      equivalence (emsolc( 6), CBASIS)
c
c     The name of cost (objective) change row in the MPS file to be
c
c
c     used by ems_SPAR.
c
      character*8 CCHANGEOBJ
      equivalence (emsolc( 7), CCHANGEOBJ)
c
c     The name of RHS change in the MPS file to be used by ems_SPAR.
c
      character*8 CCHANGERHS
      equivalence (emsolc( 8), CCHANGERHS)
c
c     The name of range change in the MPS file to be used by ems_SPAR.
c
      character*8 CCHANGERANGE
      equivalence (emsolc( 9), CCHANGERANGE)
c
c     The name of bounds change in the MPS file to be used by ems_SPAR.
c
      character*8 CCHANGEBOUNDS
      equivalence (emsolc(10), CCHANGEBOUNDS)
c
c     The name of the spreadsheet range(s) containing the adjustable
c     cells.
c
      character*8 CSSOLUTION
      equivalence (emsolc(11), CSSOLUTION)
c
c     The name of the spreadsheet range(s) containing the constraint
c     cells.
c
      character*8 CSCONSTRTS
      equivalence (emsolc(12), CSCONSTRTS)
c
c     The name of the spreadsheet range containing the objective cell.
c
      character*8 CSOBJECTIVE
      equivalence (emsolc(13), CSOBJECTIVE)
c
c     The name of the spreadsheet range(s) containing SOS sets of type
c     1.
c
      character*8 CSems_SOS1
      equivalence (emsolc(14), CSems_SOS1)
c
c     The name of the spreadsheet range(s) containing SOS sets of type
c     2.
c
      character*8 CSems_SOS2
      equivalence (emsolc(15), CSems_SOS2)
c
c     The name of the spreadsheet range(s) containing SOS sets of type
c     3.
c
      character*8 CSems_SOS3
      equivalence (emsolc(16), CSems_SOS3)
c
c     The name of the spreadsheet range(s) containing SOS sets of type
c     4 (general integer variables).
c
      character*8 CSems_SOS4
      equivalence (emsolc(17), CSems_SOS4)
