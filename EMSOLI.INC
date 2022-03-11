CM
c*********************************************************************
C
c  equivalence statements for the EMSOL integer control variables.
c  These may be included in your program to allow referencing
c  EMSOL control variables by mnemonic names.
C
c*********************************************************************
c
c     Array for integer control variables.
c
      integer emsoliln
      parameter (emsoliln = 250)
      integer emsoli(emsoliln)
      common/ems_com_emsoli/emsoli
c
c     The log frequency.
c
      integer ILOGFREQ
      equivalence (emsoli( 1), ILOGFREQ)
c
c     The unit where output is directed for writing.
c
      integer IPRINTUNIT
      equivalence (emsoli( 2), IPRINTUNIT)
c
c     The maximum number of iterations between a refactorization
c     (invert) of the basis must be performed.
c
      integer IMAXFACTOR
      equivalence (emsoli( 3), IMAXFACTOR)
c
c     The current number of iterations EMSOL has performed in a solve
c     subroutine.
c
      integer IITERNUM
      equivalence (emsoli( 4), IITERNUM)
c
c     The maximum number of iterations EMSOL will perform.
c
      integer IMAXITER
      equivalence (emsoli( 5), IMAXITER)
c
c     The simplex log detail bit mask.
c
      integer ILOGLEVEL
      equivalence (emsoli( 6), ILOGLEVEL)
c
c     One objective function (type N row) flag for MPS files.
c
      integer IONEOBJ
      equivalence (emsoli( 7), IONEOBJ)
c
c     The number of the current ems_QPAR parametric adjustment.
c
      integer IQPARNUMITER
      equivalence (emsoli( 8), IQPARNUMITER)
c
c     The maximum number of rows allowed in the matrix.
c
      integer IMAXROWS
      equivalence (emsoli( 9), IMAXROWS)
c
c     The maximum number of columns allowed in the matrix.
c
      integer IMAXCOLS
      equivalence (emsoli(10), IMAXCOLS)
c
c     The number of characters in the names of the
c
      integer INUMCHAR
      equivalence (emsoli(11), INUMCHAR)
c
c     The stopping condition bit mask.
c
      integer ISTOPMASK
      equivalence (emsoli(12), ISTOPMASK)
c
c     The maximum number of iterations of the interior-point barrier
c     algorithm.
c
      integer IMAXITERB
      equivalence (emsoli(13), IMAXITERB)
c
c     The formation of the adjacency matrix biA <biA sup T>.
c
      integer IADJACTYPE
      equivalence (emsoli(14), IADJACTYPE)
c
c     The formation of the normal matrix.
c
      integer IFORMNTYPE
      equivalence (emsoli(15), IFORMNTYPE)
c
c     The dense column threshold.
c
      integer IDENSECOL
      equivalence (emsoli(16), IDENSECOL)
c
c     The type of Devex pricing to be used.
c
      integer IDEVEXMODE
      equivalence (emsoli(17), IDEVEXMODE)
c
c     The null space checking switch.
c
      integer INULLCHECK
      equivalence (emsoli(18), INULLCHECK)
c
c     The constraint dropping threshold.
c
      integer IDROPROWCT
      equivalence (emsoli(19), IDROPROWCT)
c
c     The frequency which ems_ITRU is called.
c
      integer IITERUFREQ
      equivalence (emsoli(20), IITERUFREQ)
c
c     The potential basis flag.
c
      integer IPOSSBASIS
      equivalence (emsoli(21), IPOSSBASIS)
c
c     The maximum null space projections.
c
      integer IMAXPROJNS
      equivalence (emsoli(22), IMAXPROJNS)
c
c     The number of elements in the last created block.
c
      integer INUMELS
      equivalence (emsoli(23), INUMELS)
c
c     The number of matrix blocks.
c
      integer INUMBLOCKS
      equivalence (emsoli(24), INUMBLOCKS)
c
c     The position of the message number on the output line.
c
      integer IMSGPOS
      equivalence (emsoli(25), IMSGPOS)
c
c     The number of lines on the page.
c
      integer IPAGELINES
      equivalence (emsoli(26), IPAGELINES)
c
c     The number of rows in the matrix.
c
      integer INUMROWS
      equivalence (emsoli(27), INUMROWS)
c
c     The number of structural variables in the matrix.
c
      integer INUMCOLS
      equivalence (emsoli(28), INUMCOLS)
c
c     The current number of primal infeasibilities.
c
      integer INUMPINF
      equivalence (emsoli(29), INUMPINF)
c
c     The current number of dual infeasibilities.
c
      integer INUMDINF
      equivalence (emsoli(30), INUMDINF)
c
c     The bit mask that determines which parts of the matrix are
c     written or read by ems_PTMD or ems_GTMD.
c
      integer IMODELMASK
      equivalence (emsoli(31), IMODELMASK)
c
c     The solution writing bit mask for ems_PRTS.
c
      integer IPRTINFOMASK
      equivalence (emsoli(32), IPRTINFOMASK)
c
c     The write matrix bit mask for ems_PRTS.
c
      integer ISOLMASK
      equivalence (emsoli(33), ISOLMASK)
      integer IPRTMTRXMASK
      equivalence (emsoli(33), IPRTMTRXMASK)
c
c     The number of elements in each spare block.
c
      integer IEXTRABLK
      equivalence (emsoli(34), IEXTRABLK)
c
c     The maximum number of parametric adjustments that will be
c     performed by ems_QPAR.
c
      integer IQPARMAXITER
      equivalence (emsoli(35), IQPARMAXITER)
c
c     The length of the output line.
c
      integer ILINELEN
      equivalence (emsoli(36), ILINELEN)
c
c     The maximum node number created so far.
c
      integer INUMNODES
      equivalence (emsoli(37), INUMNODES)
c
c     The ems_MSLV log detail bit mask.
c
      integer IINTMASK
      equivalence (emsoli(38), IINTMASK)
c
c     The fast iteration switch.
c
      integer IFASTITS
      equivalence (emsoli(39), IFASTITS)
c
c     The maximum number of nodes to evaluate.
c
      integer IMAXNODES
      equivalence (emsoli(40), IMAXNODES)
c
c     The maximum number of feasible integer solutions to find.
c
      integer IMAXSOLS
      equivalence (emsoli(41), IMAXSOLS)
c
c     The number of integer solutions found so far.
c
      integer INUMSOLS
      equivalence (emsoli(42), INUMSOLS)
c
c     The number of individual integer variables.
c
      integer INUMINTS
      equivalence (emsoli(43), INUMINTS)
c
c     The number of sets.
c
      integer INUMSETS
      equivalence (emsoli(44), INUMSETS)
c
c     The number of integer variables at fractional values.
c
      integer INUMUNSAT
      equivalence (emsoli(45), INUMUNSAT)
c
c     IBM Vector facility flag.
c
      integer IVECTOR
      equivalence (emsoli(46), IVECTOR)
c
c     The problem status.
c
      integer IPROBSTAT
      equivalence (emsoli(47), IPROBSTAT)
c
c     The maximum number of decomposition iterations.
c
      integer IMAJORITS
      equivalence (emsoli(48), IMAJORITS)
c
c     The sensitivity information writeing bit mask.
c
      integer IPRINTSENS
      equivalence (emsoli(49), IPRINTSENS)
c
c     The maximum number of integer variables.
c
      integer IMAXINTS
      equivalence (emsoli(50), IMAXINTS)
c
c     The maximum number of sets.
c
      integer IMAXSETS
      equivalence (emsoli(51), IMAXSETS)
c
c     The bit mask that selects various steps of the MIP algorithm.
c
      integer ISTRATEGY
      equivalence (emsoli(52), ISTRATEGY)
c
c     The maximum amount of integer information.
c
      integer IMAXINTINFO
      equivalence (emsoli(53), IMAXINTINFO)
c
c     Number of integer variables that must be fixed for supernode
c     processing to continue.
c
      integer ITHRESHOLD
      equivalence (emsoli(54), ITHRESHOLD)
c
c     The number of heuristic passes to be made by ems_MPRE.
c
      integer IHEURPASS
      equivalence (emsoli(55), IHEURPASS)
c
c     The number of branches allowed inside a supernode before
c     supernode processing ends.
c
      integer ISUPERTOL
      equivalence (emsoli(56), ISUPERTOL)
c
c     The row ordering method indicator.
c
      integer IROWORD
      equivalence (emsoli(57), IROWORD)
c
c     The row ordering read and write logical unit.
c
      integer IORDUNIT
      equivalence (emsoli(58), IORDUNIT)
c
c     The amount of extra information that is saved and restored by
c     the user exit subroutine ems_NODU.
c
      integer IMIPLENGTH
      equivalence (emsoli(59), IMIPLENGTH)
c
c     The current number of iterations ems_BSLV has performed.
c
      integer IITERBNUM
      equivalence (emsoli(60), IITERBNUM)
c
c     The type of pricing for ems_NSLV.
c
      integer IPRICETYPE
      equivalence (emsoli(61), IPRICETYPE)
c
c     A secondary problem status variable.
c
      integer IPROBSTAT2
      equivalence (emsoli(63), IPROBSTAT2)
c
c     The bit mask that indicates the type of model read in by ems_SMDL.
c
      integer ISMDLTYPEMASK
      equivalence (emsoli(64), ISMDLTYPEMASK)
