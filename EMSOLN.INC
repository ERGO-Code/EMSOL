CM
c*********************************************************************
C
c  equivalence statements for the EMSOL index control variables.
c  These may be included in your program to allow referencing
c  EMSOL control variables by mnemonic names.
C
c*********************************************************************
c
c     Array for index control variables.
c
      integer emsolnln
      parameter (emsolnln = 81)
      integer emsoln(emsolnln)
      common/ems_com_emsoln/emsoln
c
c     The index into dspace for the first element of row lower bounds.
c
      integer NROWLOWER
      equivalence (emsoln( 1), NROWLOWER)
c
c     The index into dspace for the first element of row activities.
c
      integer NROWACTS
      equivalence (emsoln( 2), NROWACTS)
c
c     The index into dspace for the first element of row upper bounds.
c
      integer NROWUPPER
      equivalence (emsoln( 3), NROWUPPER)
c
c     The index into dspace for the first element of row slacks
c
      integer NROWDUALS
      equivalence (emsoln( 4), NROWDUALS)
      integer NROWSLACKS
      equivalence (emsoln( 4), NROWSLACKS)
c
c     The index into mspace for the first element of the row status
c     vector.
c
      integer NROWSTAT
      equivalence (emsoln( 5), NROWSTAT)
c
c     The index into dspace for the first element of column lower
c     bounds.
c
      integer NCOLLOWER
      equivalence (emsoln( 6), NCOLLOWER)
c
c     The index into dspace for the first element of the solution
c     (column activities).
c
      integer NCOLSOL
      equivalence (emsoln( 7), NCOLSOL)
c
c     The index into dspace for the first element of column upper
c     bounds.
c
      integer NCOLUPPER
      equivalence (emsoln( 8), NCOLUPPER)
c
c     The index into dspace for the first element of reduced costs.
c
      integer NCOLRCOSTS
      equivalence (emsoln( 9), NCOLRCOSTS)
c
c     The index into mspace for the first element of the column
c     status vector.
c
      integer NCOLSTAT
      equivalence (emsoln(10), NCOLSTAT)
c
c     The index into dspace for the first element of column costs
c     (objective function coefficient).
c
      integer NOBJECTIVE
      equivalence (emsoln(11), NOBJECTIVE)
c
c     The index into dspace for the first element of row names.
c
      integer NROWNAMES
      equivalence (emsoln(12), NROWNAMES)
c
c     The index into dspace for the first element of column names.
c
      integer NCOLNAMES
      equivalence (emsoln(13), NCOLNAMES)
c
c     The index into dspace for the first element of row scale factors.
c
      integer NROWSCALES
      equivalence (emsoln(14), NROWSCALES)
c
c     The index into dspace for the first element of column scale
c     factors.
c
      integer NCOLSCALES
      equivalence (emsoln(15), NCOLSCALES)
c
c     The index into mspace for the first element of the presolve
c     region.
c
      integer NPRESOLVE
      equivalence (emsoln(16), NPRESOLVE)
c
c     The index into mspace for the first element of rows for matrix
c     (column copy).
c
      integer NROWCC
      equivalence (emsoln(17), NROWCC)
c
c     The index into mspace for the first element of columns for
c     matrix (row copy).
c
      integer NCOLRC
      equivalence (emsoln(18), NCOLRC)
c
c     The index into dspace for the first element of elements for
c     matrix (column copy).
c
      integer NELEMCC
      equivalence (emsoln(19), NELEMCC)
c
c     The index into dspace for the first element of columns for
c     matrix (row copy).
c
      integer NELEMRC
      equivalence (emsoln(20), NELEMRC)
c
c     The index into mspace for the first element of row starts (row
c     copy).
c
      integer NROWRC
      equivalence (emsoln(21), NROWRC)
c
c     The index into mspace for the first element of column starts
c     (column copy).
c
      integer NCOLCC
      equivalence (emsoln(22), NCOLCC)
c
c     The index into dspace of the first element of free space.
c
      integer NFIRSTFREE
      equivalence (emsoln(23), NFIRSTFREE)
c
c     The index into dspace of the last element of free space.
c
      integer NLASTFREE
      equivalence (emsoln(24), NLASTFREE)
c
c     The index into mspace for the first element of column entries
c     for latest block.
c
      integer NBLOCKCOL
      equivalence (emsoln(25), NBLOCKCOL)
c
c     The index into mspace for the first element of row entries for
c     latest block.
c
      integer NBLOCKROW
      equivalence (emsoln(26), NBLOCKROW)
c
c     The index into dspace for the first element of elements for
c     latest block.
c
      integer NBLOCKELEM
      equivalence (emsoln(27), NBLOCKELEM)
c
c     The index into mspace for the first element of integer
c     information.
c
      integer NINTINFO
      equivalence (emsoln(28), NINTINFO)
c
c     The index into dspace for the first element of row auxiliary
c     solve information.
c
      integer NROWAUX
      equivalence (emsoln(29), NROWAUX)
c
c     The index into dspace for the first element of column auxiliary
c     solve information.
c
      integer NCOLAUX
      equivalence (emsoln(30), NCOLAUX)
c
c     The index into dspace created by ems_rgda for the first element
c     of the array of cost upper limits.
c
      integer NSOBJUPC
      equivalence (emsoln(31), NSOBJUPC)
c
c     The index into dspace created by ems_rgda for the first element
c     of the array of cost lower limits.
c
      integer NSOBJDNC
      equivalence (emsoln(32), NSOBJDNC)
c
c     The index into dspace created by ems_rgda for the first element
c     of ranges of the objective function values corresponding to the
c     upper limits on cost coefficients indexed by Nsobjupc.
c
      integer NSOBJUPV
      equivalence (emsoln(33), NSOBJUPV)
c
c     The index into dspace created by ems_rgda for the first element
c     of ranges of the objective function values corresponding to the
c     lower limits on cost coefficients indexed by Nsobjdnc.
c
      integer NSOBJDNV
      equivalence (emsoln(34), NSOBJDNV)
c
c     The index into mspace created by ems_rgda for the first element
c     of the array of entering rows or columns corresponding to the
c     increased cost coefficients indexed by Nsobjupc.
c
      integer NSOBJUPE
      equivalence (emsoln(35), NSOBJUPE)
c
c     The index into mspace created by ems_rgda for the first element
c     of the array of entering rows or columns corresponding to the
c     decreased cost coefficients indexed by Nsobjdnc.
c
      integer NSOBJDNE
      equivalence (emsoln(36), NSOBJDNE)
c
c     The index into mspace created by ems_rgda for the first element
c     of the array of leaving rows or columns corresponding to the
c     increased cost coefficients indexed by Nsobjupc.
c
      integer NSOBJUPL
      equivalence (emsoln(37), NSOBJUPL)
c
c     The index into mspace created by ems_rgda for the first element
c     of the array of leaving rows or columns corresponding to the
c     decreased cost coefficients indexed by Nsobjdnc.
c
      integer NSOBJDNL
      equivalence (emsoln(38), NSOBJDNL)
c
c     The index into dspace created by ems_rgda for the first element
c     of the upper limits on column bounds.
c
      integer NSBNDCUPB
      equivalence (emsoln(39), NSBNDCUPB)
c
c     The index into dspace created by ems_rgda for the first element
c     of the lower limits on column activities.
c
      integer NSBNDCDNB
      equivalence (emsoln(40), NSBNDCDNB)
c
c     The index into dspace created by ems_rgda for the first element
c     of the ranges of the objective function values corresponding to
c     the upper limits on column activities indexed by Nsbndcupb.
c
      integer NSBNDCUPV
      equivalence (emsoln(41), NSBNDCUPV)
c
c     The index into dspace created by ems_rgda for the first element
c     of ranges of the objective function values corresponding to the
c     lower limits on column activities indexed by Nsbndcdnb.
c
      integer NSBNDCDNV
      equivalence (emsoln(42), NSBNDCDNV)
c
c     The index into mspace created by ems_rgda for the first element
c     of the array of entering rows or columns corresponding to the
c     upper limits on column activities indexed by Nsbndcupb.
c
      integer NSBNDCUPE
      equivalence (emsoln(43), NSBNDCUPE)
c
c     The index into mspace created by ems_rgda for the first element
c     of the array of entering rows or columns corresponding to the
c     lower limits on column activities indexed by Nsbndcdnb.
c
      integer NSBNDCDNE
      equivalence (emsoln(44), NSBNDCDNE)
c
c     The index into mspace created by ems_rgda for the first element
c     of the array of leaving rows or columns corresponding to the
c     upper limits on column activities indexed by Nsbndcupb.
c
      integer NSBNDCUPL
      equivalence (emsoln(45), NSBNDCUPL)
c
c     The index into mspace created by ems_rgda for the first element
c     of the array of leaving rows or columns corresponding to the
c     lower limits on column activities indexed by Nsbndcdnb.
c
      integer NSBNDCDNL
      equivalence (emsoln(46), NSBNDCDNL)
c
c     The index into dspace created by ems_rgda for the first element
c     of the upper limits on row activities.
c
      integer NSBNDRUPB
      equivalence (emsoln(47), NSBNDRUPB)
c
c     The index into dspace created by ems_rgda for the first element
c     of the lower limits on row activities.
c
      integer NSBNDRDNB
      equivalence (emsoln(48), NSBNDRDNB)
c
c     The index into dspace created by ems_rgda for the first element
c     of the ranges of the objective function values corresponding to
c     the upper limits on row activities indexed by Nsbndrupb.
c
      integer NSBNDRUPV
      equivalence (emsoln(49), NSBNDRUPV)
c
c     The index into dspace created by ems_rgda for the first element
c     of ranges of the objective function values corresponding to the
c     lower limits on row activities indexed by Nsbndrdnb.
c
      integer NSBNDRDNV
      equivalence (emsoln(50), NSBNDRDNV)
c
c     The index into mspace created by ems_rgda for the first element
c     of the array of entering rows or columns corresponding to the
c     upper limits on row activities indexed by Nsbndrupb.
c
      integer NSBNDRUPE
      equivalence (emsoln(51), NSBNDRUPE)
c
c     The index into mspace created by ems_rgda for the first element
c     of the array of entering rows or columns corresponding to the
c     lower limits on row activities indexed by Nsbndrdnb.
c
      integer NSBNDRDNE
      equivalence (emsoln(52), NSBNDRDNE)
c
c     The index into mspace created by ems_rgda for the first element
c     of the array of leaving rows or columns corresponding to the
c     upper limits on row activities indexed by Nsbndrupb.
c
      integer NSBNDRUPL
      equivalence (emsoln(53), NSBNDRUPL)
c
c     The index into mspace created by ems_rgda for the first element
c     of the array of leaving rows or columns corresponding to the
c     lower limits on row activities indexed by Nsbndrdnb.
c
      integer NSBNDRDNL
      equivalence (emsoln(54), NSBNDRDNL)
c
c     The index into mspace for the column selection list.
c
      integer NSELLISTCOL
      equivalence (emsoln(55), NSELLISTCOL)
c
c     The index into mspace for the row selection list.
c
      integer NSELLISTROW
      equivalence (emsoln(56), NSELLISTROW)
c
c     The index into dspace for the parametric cost (objective)
c     change vector created for ems_SPAR.
c
      integer NSPARCOST
      equivalence (emsoln(57), NSPARCOST)
c
c     The index into dspace for the row lower bounds parametric
c     change vector created for ems_SPAR.
c
      integer NSPARRLO
      equivalence (emsoln(58), NSPARRLO)
c
c     The index into dspace for the row upper bounds parametric
c     change vector created for ems_SPAR.
c
      integer NSPARRUP
      equivalence (emsoln(59), NSPARRUP)
c
c     The index into dspace for the column lower bounds parametric
c     change vector created for ems_SPAR.
c
      integer NSPARCLO
      equivalence (emsoln(60), NSPARCLO)
c
c     The index into dspace for the column upper bounds parametric
c     change vector created for ems_SPAR.
c
      integer NSPARCUP
      equivalence (emsoln(61), NSPARCUP)
c
c     The index into mspace for the first element of the indices of
c     the arcs in the basis.
c
      integer NARCID
      equivalence (emsoln(62), NARCID)
c
c     The index into mspace for the first element of the array of
c     node levels.
c
      integer NLEVEL
      equivalence (emsoln(63), NLEVEL)
c
c     The index into mspace for the first element of the array of
c     parent nodes.
c
      integer NPARENT
      equivalence (emsoln(64), NPARENT)
c
c     The index into mspace for the first element of the preorder
c     traversal array.
c
      integer NPREORDER
      equivalence (emsoln(65), NPREORDER)
c
c     The index into mspace for the first element of the reverse
c     preorder traversal array.
c
      integer NREVPREORDER
      equivalence (emsoln(66), NREVPREORDER)
