CM
C->>> -------------------------------------------> ems_iz_ems_fi_com <<<
c     Initialises the common block of EMS file keywords.
c
      subroutine ems_iz_ems_fi_com
      implicit none
      include 'EMSV.INC'
      include 'EMSFI.INC'
 
      ems_fi_cmt_ky_1 =         '!'
      ems_fi_cmt_ky_2 =         '#'
      ems_fi_cmt_ky_3 =         '*'
      ems_fi_mps_ml_nm_ky =     'name'
      ems_fi_fmt_ky =           'ems_file_format'
      ems_fi_mx_ky =            'maximize'
      ems_fi_mn_ky =            'minimize'
      ems_fi_n_r_ky =           'n_rows'
      ems_fi_n_c_ky =           'n_columns'
      ems_fi_n_mtx_el_ky =      'n_matrix_elements'
      ems_fi_r_ky =             'rows'
      ems_fi_c_ky =             'columns'
      ems_fi_mtx_ky =           'matrix'
      ems_fi_c_bd_ky =          'column_bounds'
      ems_fi_r_bd_ky =          'row_bounds'
      ems_fi_c_co_ky =          'column_costs'
      ems_fi_nm_ky =            'names'
      ems_fi_ob_fn_cs_ky =      'objective_constant'
      ems_fi_r_co_ky =          'row_costs'
      ems_fi_pwl_ky =           'piecewise_linear'
      ems_fi_bs_ky =            'basis'
      ems_fi_bs_in_alt_fmt_ky = 'basis_in_alternative_format'
      ems_fi_e_lin_ky =         'end_linear'
c
c     Indicate that the common block of EMS file keywords has been
c     initialised.
c
      ems_fi_iz_com_fg1 = 1
      ems_fi_iz_com_fg2 = 2
      return
      end
C->>> --------------------------------------------> ems_rd_ems_fi_ky <<<
c     Reads the next non-blank, non-comment line in an EMS file as a
c     keyword and puts it into lower case.
c
      subroutine ems_rd_ems_fi_ky(ems_fi_rd_cn, ems_fi_ky)
      implicit none
      include 'EMSV.INC'
      include 'EMSFI.INC'
      include 'EMSMSG.INC'
      integer ems_fi_rd_cn
      character*(*) ems_fi_ky
CM      IF (emsol_epc .EQ. 1) THEN
C?      logical ems_i1_eq_i2
CM      ENDIF
c
c     Make sure that ems_iz_ems_fi_com has been called
c
CM      IF (emsol_epc .EQ. 1) THEN
C?c
C?c     Have to use a function compiled without unassigned variable
C?c     checking in order to test values which may be unassigned.
C?c
C?      if (ems_i1_eq_i2(ems_fi_iz_com_fg1,
C?     &     ems_fi_iz_com_fg2)) goto 8000
CM      ELSE
      if (ems_fi_iz_com_fg1 .eq. ems_fi_iz_com_fg2) goto 8000
CM      ENDIF
 1    continue
      read(ems_fi_rd_cn, '(a)',  err = 8020, end = 8010)ems_fi_ky
c      read(ems_fi_rd_cn, *,  err = 8020, end = 8010)ems_fi_ky
      if (ems_fi_ky(1:ems_fi_cmt_ky_n_ch) .eq. ems_fi_cmt_ky_1) goto 1
      if (ems_fi_ky(1:ems_fi_cmt_ky_n_ch) .eq. ems_fi_cmt_ky_2) goto 1
      if (ems_fi_ky(1:ems_fi_cmt_ky_n_ch) .eq. ems_fi_cmt_ky_3) goto 1
      call ems_str_t_lo_case(ems_fi_ky)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)ems_fi_ky
      call ems_msg_wr_li(info_msg_n)
 7000 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 9100 format('Read EMS file keyword: ', a)
 9800 format('Must call ems_iz_ems_fi_com before EMSOL file ops')
 9801 format('Premature end of file for model ', a8)
 9802 format('Error reading file for model ', a8)
      end
 
 
C->>> ----------------------------------------> ems_se_msg_my_prcs_n <<<
c     Sets the value of ems_msg_my_prcs_n
c
      subroutine ems_se_msg_my_prcs_n(ps_my_prcs_n)
      implicit none
      integer ps_my_prcs_n
      include 'EMSMSG.INC'
      ems_msg_my_prcs_n = ps_my_prcs_n
      return
      end
 
C->>> --------------------------------------------> ems_fi_io_ml_hdr <<<
c     Read/writes the header from/to an EMS file.
c
      subroutine ems_fi_io_ml_hdr(
     &     ems_fi_cn, rd_fi, fi_io,
     &     ch_ml_nm, ch_ob_nm, ch_rhs_nm, ch_rg_nm, ch_bd_nm)
      implicit none
      include 'EMSV.INC'
      include 'EMSFI.INC'
      include 'EMSMSG.INC'
      integer ems_fi_cn, fi_io
      logical rd_fi
      character*8 ch_ml_nm, ch_ob_nm, ch_rhs_nm, ch_rg_nm, ch_bd_nm
 
      if (rd_fi) then
c
c     ?? '(a8)' to be * once ******** becomes UNNAMED in NwMagic
c
         read(ems_fi_cn, '(a8)', err = 8020, end = 8010)ch_ml_nm
         read(ems_fi_cn, '(a8)', err = 8020, end = 8010)ch_ob_nm
         read(ems_fi_cn, '(a8)', err = 8020, end = 8010)ch_rhs_nm
         read(ems_fi_cn, '(a8)', err = 8020, end = 8010)ch_rg_nm
         read(ems_fi_cn, '(a8)', err = 8020, end = 8010)ch_bd_nm
         ems_fi_ml_nm = ch_ml_nm
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)ems_fi_ml_nm
         call ems_msg_wr_li(info_msg_n)
      else
         write(ems_fi_cn, 9100, err = 8030)ch_ml_nm,  ': matrix'
         write(ems_fi_cn, 9100, err = 8030)ch_ob_nm,  ': Objective'
         write(ems_fi_cn, 9100, err = 8030)ch_rhs_nm, ': rhs'
         write(ems_fi_cn, 9100, err = 8030)ch_rg_nm,  ': ranges'
         write(ems_fi_cn, 9100, err = 8030)ch_bd_nm,  ': bounds'
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)ems_fi_ml_nm
         call ems_msg_wr_li(info_msg_n)
      endif
 7000 continue
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8030 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9803)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 9000 format('Reading model ', a8)
 9010 format('Writing model ', a8)
 9100 format(a8, a)
 9801 format('Premature end of file for model ', a8)
 9802 format('Error reading file for model ', a8)
 9803 format('Error writing file for model ', a8)
      end
 
C->>> --------------------------------------------> ems_fi_io_ml_dim <<<
c     Read/writes the model dimesions from/to an EMS file.
c
      subroutine ems_fi_io_ml_dim(
     &     ems_fi_cn, rd_fi, fi_io, fi_fmt_vers_n,
     &     rd_nx_ky, ems_fi_ky,
     &     n_r, n_c, n_a_el)
      implicit none
      include 'EMSV.INC'
      include 'EMSFI.INC'
      include 'EMSMSG.INC'
      integer ems_fi_cn, fi_io, fi_fmt_vers_n
      logical rd_fi
      logical rd_nx_ky
      character*(*) ems_fi_ky
      integer n_r, n_c, n_a_el
 
      if (rd_fi) then
         rd_nx_ky = .false.
         if (fi_fmt_vers_n .eq. 0) then
            read(ems_fi_cn, *, err = 8020, end = 8010)n_r, n_c, n_a_el
         else
 100        continue
            call ems_rd_ems_fi_ky(ems_fi_cn, ems_fi_ky)
            if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
            if (ems_fi_ky(1:ems_fi_n_r_ky_n_ch) .eq.
     &           ems_fi_n_r_ky) then
               read(ems_fi_cn, *, err = 8020, end = 8010)n_r
               goto 100
            else if (ems_fi_ky(1:ems_fi_n_c_ky_n_ch) .eq.
     &              ems_fi_n_c_ky) then
               read(ems_fi_cn, *, err = 8020, end = 8010)n_c
               goto 100
            else if (ems_fi_ky(1:ems_fi_n_mtx_el_ky_n_ch) .eq.
     &              ems_fi_n_mtx_el_ky) then
               read(ems_fi_cn, *, err = 8020, end = 8010)n_a_el
               goto 100
            endif
            rd_nx_ky = .true.
         endif
      else
         write(ems_fi_cn, 9000, err = 8030)ems_fi_n_r_ky
         write(ems_fi_cn, *,    err = 8030)n_r
         write(ems_fi_cn, 9000, err = 8030)ems_fi_n_c_ky
         write(ems_fi_cn, *,    err = 8030)n_c
         write(ems_fi_cn, 9000, err = 8030)ems_fi_n_mtx_el_ky
         write(ems_fi_cn, *,    err = 8030)n_a_el
      endif
 7000 continue
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8030 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9803)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 9000 format(a)
 9801 format('Premature end of file for model ', a8)
 9802 format('Error reading file for model ', a8)
 9803 format('Error writing file for model ', a8)
      end
 
C->>> --------------------------------------------> ems_fi_io_ml_mtx <<<
c     Read/write the matrix from/to an EMS file.
c
      subroutine ems_fi_io_ml_mtx(
     &     ems_fi_cn, rd_fi, fi_io,
     &     n_r, n_c, n_a_el,
     &     mtx_r_v,
     &     mtx_r_ix,
     &     mtx_c_sa)
      implicit none
      include 'EMSV.INC'
      include 'EMSFI.INC'
      include 'EMSMSG.INC'
      integer ems_fi_cn, fi_io
      logical rd_fi
      integer n_r, n_c, n_a_el
      double precision mtx_r_v(n_a_el)
      integer mtx_r_ix(n_a_el)
      integer mtx_c_sa(n_c+1)
      logical a_rd_e, a_rd_er, a_wr_er
 
      if (rd_fi) then
         call ems_rd_fr_fmt_i_a(
     &        ems_fi_cn, mtx_c_sa, 1, n_c+1, a_rd_er, a_rd_e)
         if (a_rd_er) go to 8010
         if (a_rd_e) go to 8020
         call ems_rd_fr_fmt_i_a(
     &        ems_fi_cn, mtx_r_ix, 1, n_a_el, a_rd_er, a_rd_e)
         if (a_rd_er) go to 8010
         if (a_rd_e) go to 8020
         call ems_rd_fr_fmt_rl_a(
     &        ems_fi_cn, mtx_r_v, 1, n_a_el, a_rd_er, a_rd_e)
         if (a_rd_er) go to 8010
         if (a_rd_e) go to 8020
         ems_fi_io_msk = ior(ems_fi_io_mtx_bt, ems_fi_io_msk)
      else
         write(ems_fi_cn, 9000, err = 8030)ems_fi_mtx_ky
         call ems_wr_fr_fmt_i_a(ems_fi_cn, mtx_c_sa, 1, n_c+1,  a_wr_er)
         if (a_wr_er) goto 8030
         call ems_wr_fr_fmt_i_a(ems_fi_cn, mtx_r_ix, 1, n_a_el, a_wr_er)
         if (a_wr_er) goto 8030
         call ems_wr_fr_fmt_rl_a(ems_fi_cn, mtx_r_v, 1, n_a_el, a_wr_er)
         if (a_wr_er) goto 8030
      endif
 7000 continue
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8030 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9803)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 9000 format(a)
 9801 format('Premature end of file for model ', a8)
 9802 format('Error reading file for model ', a8)
 9803 format('Error writing file for model ', a8)
      end
 
C->>> ---------------------------------------------> ems_fi_io_ml_bd <<<
c     Read/write the bounds from/to an EMS file.
c
      subroutine ems_fi_io_ml_bd(
     &     ems_fi_cn, rd_fi, fi_io,
     &     n_r, n_c,
     &     c_lb, r_lb,
     &     c_ub, r_ub)
      implicit none
      include 'EMSV.INC'
      include 'EMSFI.INC'
      include 'EMSMSG.INC'
      integer ems_fi_cn, fi_io
      logical rd_fi
      integer n_r, n_c
      double precision c_lb(n_c)
      double precision r_lb(n_r)
      double precision c_ub(n_c)
      double precision r_ub(n_r)
      logical a_rd_e, a_rd_er, a_wr_er
 
      if (rd_fi) then
         call ems_rd_fr_fmt_rl_a(
     &        ems_fi_cn, c_lb, 1, n_c, a_rd_er, a_rd_e)
         if (a_rd_er) go to 8010
         if (a_rd_e) go to 8020
         call ems_rd_fr_fmt_rl_a(
     &        ems_fi_cn, r_lb, 1, n_r, a_rd_er, a_rd_e)
         if (a_rd_er) go to 8010
         if (a_rd_e) go to 8020
         call ems_rd_fr_fmt_rl_a(
     &        ems_fi_cn, c_ub, 1, n_c, a_rd_er, a_rd_e)
         if (a_rd_er) go to 8010
         if (a_rd_e) go to 8020
         call ems_rd_fr_fmt_rl_a(
     &        ems_fi_cn, r_ub, 1, n_r, a_rd_er, a_rd_e)
         if (a_rd_er) go to 8010
         if (a_rd_e) go to 8020
         ems_fi_io_msk = ior(ems_fi_io_bd_msk, ems_fi_io_msk)
      else
         write(ems_fi_cn, 9000, err = 8030)ems_fi_c_bd_ky
         call ems_wr_fr_fmt_rl_a(ems_fi_cn, c_lb, 1, n_c, a_wr_er)
         if (a_wr_er) goto 8030
         call ems_wr_fr_fmt_rl_a(ems_fi_cn, c_ub, 1, n_c, a_wr_er)
         if (a_wr_er) goto 8030
         write(ems_fi_cn, 9000, err = 8030)ems_fi_r_bd_ky
         call ems_wr_fr_fmt_rl_a(ems_fi_cn, r_lb, 1, n_r, a_wr_er)
         if (a_wr_er) goto 8030
         call ems_wr_fr_fmt_rl_a(ems_fi_cn, r_ub, 1, n_r, a_wr_er)
         if (a_wr_er) goto 8030
      endif
 7000 continue
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8030 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9803)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 9000 format(a)
 9801 format('Premature end of file for model ', a8)
 9802 format('Error reading file for model ', a8)
 9803 format('Error writing file for model ', a8)
      end
 
C->>> -------------------------------------------> ems_fi_io_ml_r_bd <<<
c     Read/write the row bounds from/to an EMS file.
c
      subroutine ems_fi_io_ml_r_bd(
     &     ems_fi_cn, rd_fi, fi_io,
     &     n_r,
     &     r_lb, r_ub)
      implicit none
      include 'EMSV.INC'
      include 'EMSFI.INC'
      include 'EMSMSG.INC'
      integer ems_fi_cn, fi_io
      logical rd_fi
      integer n_r
      double precision r_lb(n_r)
      double precision r_ub(n_r)
      logical a_rd_e, a_rd_er, a_wr_er
 
      if (rd_fi) then
         call ems_rd_fr_fmt_rl_a(
     &        ems_fi_cn, r_lb, 1, n_r, a_rd_er, a_rd_e)
         if (a_rd_er) go to 8010
         if (a_rd_e) go to 8020
         call ems_rd_fr_fmt_rl_a(
     &        ems_fi_cn, r_ub, 1, n_r, a_rd_er, a_rd_e)
         if (a_rd_er) go to 8010
         if (a_rd_e) go to 8020
         ems_fi_io_msk = ior(ems_fi_io_r_bd_msk, ems_fi_io_msk)
      else
         write(ems_fi_cn, 9000, err = 8030)ems_fi_r_bd_ky
         call ems_wr_fr_fmt_rl_a(ems_fi_cn, r_lb, 1, n_r, a_wr_er)
         if (a_wr_er) goto 8030
         call ems_wr_fr_fmt_rl_a(ems_fi_cn, r_ub, 1, n_r, a_wr_er)
         if (a_wr_er) goto 8030
      endif
 7000 continue
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8030 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9803)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 9000 format(a)
 9801 format('Premature end of file for model ', a8)
 9802 format('Error reading file for model ', a8)
 9803 format('Error writing file for model ', a8)
      end
 
C->>> -------------------------------------------> ems_fi_io_ml_c_bd <<<
c     Read/write the column bounds from/to an EMS file.
c
      subroutine ems_fi_io_ml_c_bd(
     &     ems_fi_cn, rd_fi, fi_io,
     &     n_c,
     &     c_lb, c_ub)
      implicit none
      include 'EMSV.INC'
      include 'EMSFI.INC'
      include 'EMSMSG.INC'
      integer ems_fi_cn, fi_io
      logical rd_fi
      integer n_c
      double precision c_lb(n_c)
      double precision c_ub(n_c)
      logical a_rd_e, a_rd_er, a_wr_er
 
      if (rd_fi) then
         call ems_rd_fr_fmt_rl_a(
     &        ems_fi_cn, c_lb, 1, n_c, a_rd_er, a_rd_e)
         if (a_rd_er) go to 8010
         if (a_rd_e) go to 8020
         call ems_rd_fr_fmt_rl_a(
     &        ems_fi_cn, c_ub, 1, n_c, a_rd_er, a_rd_e)
         if (a_rd_er) go to 8010
         if (a_rd_e) go to 8020
         ems_fi_io_msk = ior(ems_fi_io_c_bd_msk, ems_fi_io_msk)
      else
         write(ems_fi_cn, 9000, err = 8030)ems_fi_c_bd_ky
         call ems_wr_fr_fmt_rl_a(ems_fi_cn, c_lb, 1, n_c, a_wr_er)
         if (a_wr_er) goto 8030
         call ems_wr_fr_fmt_rl_a(ems_fi_cn, c_ub, 1, n_c, a_wr_er)
         if (a_wr_er) goto 8030
      endif
 7000 continue
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8030 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9803)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 9000 format(a)
 9801 format('Premature end of file for model ', a8)
 9802 format('Error reading file for model ', a8)
 9803 format('Error writing file for model ', a8)
      end
 
C->>> -------------------------------------------> ems_fi_io_ml_c_co <<<
c     Read/write the column costs from/to an EMS file.
c
      subroutine ems_fi_io_ml_c_co(
     &     ems_fi_cn, rd_fi, fi_io,
     &     n_c,
     &     c_co)
      implicit none
      include 'EMSV.INC'
      include 'EMSFI.INC'
      include 'EMSMSG.INC'
      integer ems_fi_cn, fi_io
      logical rd_fi
      integer n_c
      double precision c_co(n_c)
      logical a_rd_e, a_rd_er, a_wr_er
 
      if (rd_fi) then
         call ems_rd_fr_fmt_rl_a(
     &        ems_fi_cn, c_co, 1, n_c, a_rd_er, a_rd_e)
         if (a_rd_er) go to 8010
         if (a_rd_e) go to 8020
         ems_fi_io_msk = ior(ems_fi_io_c_co_bt, ems_fi_io_msk)
      else
         write(ems_fi_cn, 9000, err = 8030)ems_fi_c_co_ky
         call ems_wr_fr_fmt_rl_a(ems_fi_cn, c_co, 1, n_c, a_wr_er)
         if (a_wr_er) goto 8030
      endif
 7000 continue
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8030 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9803)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 9000 format(a)
 9801 format('Premature end of file for model ', a8)
 9802 format('Error reading file for model ', a8)
 9803 format('Error writing file for model ', a8)
      end
 
C->>> ---------------------------------------------> ems_fi_io_ml_nm <<<
c     Read/write new-style names from/to an EMS file.
c
      subroutine ems_fi_io_ml_nm(
     &     rd_nx_ky, ems_fi_ky,
     &     ems_fi_cn, rd_fi, fi_io,
     &     n_r, n_c,
     &     ml_nm_n_ch, ml_nm_n_rl,
     &     mu_ch, rl_c_nm, rl_r_nm)
      implicit none
      include 'EMSV.INC'
      include 'EMSFI.INC'
      include 'EMSMSG.INC'
      logical rd_nx_ky
      character*(*) ems_fi_ky
      integer ems_fi_cn, fi_io
      logical rd_fi
      character*(*) mu_ch
      integer n_r, n_c, ml_nm_n_ch, ml_nm_n_rl
      double precision rl_c_nm(ml_nm_n_rl, n_c)
      double precision rl_r_nm(ml_nm_n_rl, n_r)
      integer r_n, c_n, null
      character*14 f7_rd_nm_fmt
      character*8 ch8
      double precision rl
      equivalence (rl, ch8)
c
c     Names are of length ml_nm_n_ch with each line being of format
c     i9, 2x, a<ml_nm_n_ch>. The names are in sections: one for rows and
c     one for columns, each headed by a keyword
c
      if (rd_fi) then
c         write(f7_rd_nm_fmt, '(a10, i3, a1)')
c     &        '(i9, 2x, a', ml_nm_n_ch, ')'
         f7_rd_nm_fmt = '(i9, 2x, a)'
c         print*, 'Reading names using format:', f7_rd_nm_fmt
         rd_nx_ky = .false.
 100     continue
         call ems_rd_ems_fi_ky(ems_fi_cn, ems_fi_ky)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
         if (ems_fi_ky(1:ems_fi_r_ky_n_ch) .eq. ems_fi_r_ky) then
            do 160, r_n = 1, n_r
               read(ems_fi_cn, f7_rd_nm_fmt,    err = 8020, end = 8010)
     &              null, mu_ch(1:ml_nm_n_ch)
               call ems_ch_t_rl(ml_nm_n_ch, mu_ch, rl_r_nm(1, r_n))
 160        continue
            ems_fi_io_msk = ior(ems_fi_io_r_nm_bt, ems_fi_io_msk)
            goto 100
         else if (ems_fi_ky(1:ems_fi_c_ky_n_ch) .eq. ems_fi_c_ky) then
            do 170, c_n = 1, n_c
               read(ems_fi_cn, f7_rd_nm_fmt,    err = 8020, end = 8010)
     &              null, mu_ch(1:ml_nm_n_ch)
               call ems_ch_t_rl(ml_nm_n_ch, mu_ch, rl_c_nm(1, c_n))
 170        continue
            ems_fi_io_msk = ior(ems_fi_io_c_nm_bt, ems_fi_io_msk)
            goto 100
         endif
         rd_nx_ky = .true.
      else
         if (ml_nm_n_ch .ne. 8) call ems_iz_ch(mu_ch, ' ')
         write(ems_fi_cn, 9000, err = 8030)ems_fi_nm_ky
         write(ems_fi_cn, *,    err = 8030)ml_nm_n_ch
         write(ems_fi_cn, 9000, err = 8030)ems_fi_c_ky
         do 120, c_n = 1, n_c
            if (ml_nm_n_ch .eq. 8) then
               rl = rl_c_nm(1, c_n)
               write(ems_fi_cn, 9200)c_n, ch8
            else
               call ems_rl_t_ch(ml_nm_n_ch, rl_c_nm(1, c_n), mu_ch)
               write(ems_fi_cn, 9200)c_n, mu_ch(1:ml_nm_n_ch)
            endif
 120     continue
         write(ems_fi_cn, 9000, err = 8030)ems_fi_r_ky
         do 125, r_n = 1, n_r
            if (ml_nm_n_ch .eq. 8) then
               rl = rl_r_nm(1, r_n)
               write(ems_fi_cn, 9200)r_n, ch8
            else
               call ems_rl_t_ch(ml_nm_n_ch, rl_r_nm(1, r_n), mu_ch)
               write(ems_fi_cn, 9200)r_n, mu_ch(1:ml_nm_n_ch)
            endif
 125     continue
      endif
 7000 continue
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8030 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9803)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 9000 format(a)
 9200 format(i9, 2x, a)
 9801 format('Premature end of file for model ', a8)
 9802 format('Error reading file for model ', a8)
 9803 format('Error writing file for model ', a8)
      end
 
C->>> -----------------------------------------> ems_fi_io_ml_nm_ol <<<
c     Read old-style names from/to an EMS file.
c
      subroutine ems_fi_io_ml_nm_ol(
     &     ems_fi_cn, rd_fi, fi_io,
     &     n_r, n_c,
     &     rl_c_nm, rl_r_nm)
      implicit none
      include 'EMSV.INC'
      include 'EMSFI.INC'
      include 'EMSMSG.INC'
      integer ems_fi_cn, fi_io
      logical rd_fi
      integer n_r, n_c
      double precision rl_c_nm(n_c)
      double precision rl_r_nm(n_r)
      integer r_n, c_n
      character*8 ch8
      double precision rl
      equivalence (rl, ch8)
c
c     Names are of length 8 with each line being of format a8
c
      do 10, c_n = 1, n_c
         read(ems_fi_cn, *, err = 8020, end = 8010)ch8
         rl_c_nm(c_n) = rl
 10   continue
      do 20, r_n = 1, n_r
         read(ems_fi_cn, *, err = 8020, end = 8010)ch8
         rl_r_nm(r_n) = rl
 20   continue
      ems_fi_io_msk = ior(ems_fi_io_nm_msk, ems_fi_io_msk)
 7000 continue
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
c 8030 continue
c      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9803)ems_fi_ml_nm
c      call ems_msg_wr_li(serious_msg_n)
c      goto 7000
 9801 format('Premature end of file for model ', a8)
 9802 format('Error reading file for model ', a8)
c 9803 format('Error writing file for model ', a8)
      end
 
C->>> --------------------------------------> ems_fi_io_ml_ob_fn_cs <<<
c     Read/write the objective function constant from/to an EMS file.
c
      subroutine ems_fi_io_ml_ob_fn_cs(
     &     ems_fi_cn, rd_fi, fi_io,
     &     ob_fn_cs)
      implicit none
      include 'EMSV.INC'
      include 'EMSFI.INC'
      include 'EMSMSG.INC'
      integer ems_fi_cn, fi_io
      logical rd_fi
      double precision ob_fn_cs
      double precision rl_v
 
      if (rd_fi) then
         read(ems_fi_cn, *, err = 8020, end = 8010)rl_v
         ob_fn_cs = rl_v
      else
         write(ems_fi_cn, 9000, err = 8030)ems_fi_ob_fn_cs_ky
         write(ems_fi_cn, *, err = 8030)ob_fn_cs
      endif
 7000 continue
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8030 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9803)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 9000 format(a)
 9801 format('Premature end of file for model ', a8)
 9802 format('Error reading file for model ', a8)
 9803 format('Error writing file for model ', a8)
      end
 
C->>> ------------------------------------------> ems_fi_io_ml_r_co <<<
c     Read row costs from/to an EMS file.
c
      subroutine ems_fi_io_ml_r_co(
     &     ems_fi_cn, rd_fi, fi_io,
     &     n_r,
     &     r_co)
      implicit none
      include 'EMSV.INC'
      include 'EMSFI.INC'
      include 'EMSMSG.INC'
      integer ems_fi_cn, fi_io
      logical rd_fi
      integer n_r
      double precision r_co(n_r)
      logical a_rd_e, a_rd_er, a_wr_er
 
      if (rd_fi) then
         call ems_rd_fr_fmt_rl_a(
     &        ems_fi_cn, r_co, 1, n_r, a_rd_er, a_rd_e)
         if (a_rd_er) go to 8010
         if (a_rd_e) go to 8020
         ems_fi_io_msk = ior(ems_fi_io_r_co_bt, ems_fi_io_msk)
      else
         write(ems_fi_cn, 9000, err = 8030)ems_fi_r_co_ky
         call ems_wr_fr_fmt_rl_a(ems_fi_cn, r_co, 1, n_r, a_wr_er)
         if (a_wr_er) goto 8030
      endif
 7000 continue
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8030 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9803)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 9000 format(a)
 9801 format('Premature end of file for model ', a8)
 9802 format('Error reading file for model ', a8)
 9803 format('Error writing file for model ', a8)
      end
 
C->>> --------------------------------------------> ems_fi_io_ml_bs <<<
c     Read a basis from/to an EMS file.
c
      subroutine ems_fi_io_ml_bs(
     &     ems_fi_cn, rd_fi, fi_io,
     &     n_r, n_c,
     &     c_st, r_st,
     &     c_pr_act, r_pr_act,
     &     scl_pr_act, c_scl, r_scl)
      implicit none
      include 'EMSV.INC'
      include 'EMSFI.INC'
      include 'EMSMSG.INC'
      integer ems_fi_cn, fi_io
      logical rd_fi
      integer n_r, n_c
      integer c_st(n_c)
      integer r_st(n_r)
      double precision c_pr_act(n_c)
      double precision r_pr_act(n_r)
      logical scl_pr_act
      double precision c_scl(n_c)
      double precision r_scl(n_r)
      integer r_n, c_n
      double precision pr_act_v
 
      if (rd_fi) then
         if (scl_pr_act) then
            do 10, c_n = 1, n_c
               read(ems_fi_cn, *, err = 8020, end = 8010)
     &              c_st(c_n), pr_act_v
               c_pr_act(c_n) = pr_act_v*c_scl(c_n)
 10         continue
            do 20, r_n = 1, n_r
               read(ems_fi_cn, *, err = 8020, end = 8010)
     &              r_st(r_n), pr_act_v
               r_pr_act(r_n) = pr_act_v*r_scl(r_n)
 20         continue
         else
            do 30, c_n = 1, n_c
               read(ems_fi_cn, *, err = 8020, end = 8010)
     &              c_st(c_n), c_pr_act(c_n)
 30         continue
            do 40, r_n = 1, n_r
               read(ems_fi_cn, *, err = 8020, end = 8010)
     &              r_st(r_n), r_pr_act(r_n)
 40         continue
         endif
         ems_fi_io_msk = ior(ems_fi_io_bs_bt, ems_fi_io_msk)
      else
         write(ems_fi_cn, 9000, err = 8030)ems_fi_bs_ky
         if (scl_pr_act) then
            do 110, c_n = 1, n_c
               pr_act_v = c_pr_act(c_n)/c_scl(c_n)
               write(ems_fi_cn, *, err = 8030)c_st(c_n), pr_act_v
 110        continue
            do 120, r_n = 1, n_r
               pr_act_v = r_pr_act(r_n)/r_scl(r_n)
               write(ems_fi_cn, *, err = 8030)r_st(r_n), pr_act_v
 120        continue
         else
            do 130, c_n = 1, n_c
               write(ems_fi_cn, *, err = 8030)c_st(c_n), c_pr_act(c_n)
 130        continue
            do 140, r_n = 1, n_r
               write(ems_fi_cn, *, err = 8030)r_st(r_n), r_pr_act(r_n)
 140        continue
         endif
      endif
 7000 continue
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8030 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9803)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 9000 format(a)
 9801 format('Premature end of file for model ', a8)
 9802 format('Error reading file for model ', a8)
 9803 format('Error writing file for model ', a8)
      end
 
C->>> ---------------------------------> ems_fi_io_ml_bs_in_alt_fmt <<<
c     Read/write a basis in alternative format from/to an EMS file.
c
      subroutine ems_fi_io_ml_bs_in_alt_fmt(
     &     ems_fi_cn, rd_fi, fi_io,
     &     n_r, n_c,
     &     c_lb, r_lb,
     &     c_ub, r_ub,
     &     c_st, r_st,
     &     c_pr_act, r_pr_act,
     &     su_vr_bs_bt, bc_vr_bs_st, non_bc_vr_bs_st)
      implicit none
      include 'EMSV.INC'
      include 'EMSFI.INC'
      include 'EMSMSG.INC'
      integer ems_fi_cn, fi_io
      logical rd_fi
      integer n_r, n_c
      integer c_st(n_c)
      integer r_st(n_r)
      double precision c_lb(n_c)
      double precision r_lb(n_r)
      double precision c_ub(n_c)
      double precision r_ub(n_r)
      double precision c_pr_act(n_c)
      double precision r_pr_act(n_r)
      integer su_vr_bs_bt, bc_vr_bs_st, non_bc_vr_bs_st
      integer r_n, c_n, rd_st
c      integer n_vr_in_r, n_vr_in_c
 
      if (rd_fi) then
c         n_vr_in_r = 0
c         n_vr_in_c = 0
         do 10, c_n = 1, n_c
            read(ems_fi_cn, *,    err = 8020, end = 8010)rd_st
            if (rd_st .gt. 0) then
c               n_vr_in_r = n_vr_in_r + 1
               c_st(c_n) = c_st(c_n) -
     &              iand(c_st(c_n), su_vr_bs_bt) +
     &              bc_vr_bs_st
            else
c               n_vr_in_c = n_vr_in_c + 1
               c_st(c_n) = c_st(c_n) -
     &              iand(c_st(c_n), su_vr_bs_bt) +
     &              non_bc_vr_bs_st
               if (rd_st .eq. 0) then
                  c_pr_act(c_n) = c_lb(c_n)
               else
                  c_pr_act(c_n) = c_ub(c_n)
               endif
            endif
 10      continue
c         print*, ' After Structurals'
c         print*, ' n_vr_in_r ', n_vr_in_r
c         print*, ' n_vr_in_c ', n_vr_in_c
         do 20, r_n = 1, n_r
            read(ems_fi_cn, *,    err = 8020, end = 8010)rd_st
            if (rd_st .gt. 0) then
c               n_vr_in_r = n_vr_in_r + 1
               r_st(r_n) = r_st(r_n) -
     &              iand(r_st(r_n), su_vr_bs_bt) +
     &              bc_vr_bs_st
            else
c               n_vr_in_c = n_vr_in_c + 1
               r_st(r_n) = r_st(r_n) -
     &              iand(r_st(r_n), su_vr_bs_bt) +
     &              non_bc_vr_bs_st
               if (rd_st .eq. 0) then
                  r_pr_act(r_n) = r_lb(r_n)
               else
                  r_pr_act(r_n) = r_ub(r_n)
               endif
            endif
 20      continue
c         print*, ' Finally'
c         print*, ' n_vr_in_r, n_r ', n_vr_in_r, n_r
c         print*, ' n_vr_in_c, n_c ', n_vr_in_c, n_c
         ems_fi_io_msk = ior(ems_fi_io_bs_in_alt_fmt_bt, ems_fi_io_msk)
      endif
 7000 continue
      return
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)ems_fi_ml_nm
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
c 8030 continue
c      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9803)ems_fi_ml_nm
c      call ems_msg_wr_li(serious_msg_n)
c      goto 7000
 9801 format('Premature end of file for model ', a8)
 9802 format('Error reading file for model ', a8)
c 9803 format('Error writing file for model ', a8)
      end
 
C->>> ------------------------------------------------> ems_ck_ml_bd <<<
c     Checks that that the bounds/costs are consistent and there is a
c     feasible value in [-tl_mx_act, tl_mx_act].
c
      subroutine ems_ck_ml_bd(n_r, n_c,
     &     r_lbc, r_ubc,
     &     c_lbc, c_ubc,
     &     tl_mx_act)
      implicit none
      include 'EMSMSG.INC'
      integer n_r, n_c
      double precision r_lbc(n_r), c_lbc(n_c)
      double precision r_ubc(n_r), c_ubc(n_c)
      double precision tl_mx_act
      integer r_n, c_n
 
      do 10, r_n = 1, n_r
         if (r_lbc(r_n) .gt. r_ubc(r_n)) goto 8100
         if (r_lbc(r_n) .gt.  tl_mx_act) goto 8110
         if (r_ubc(r_n) .lt. -tl_mx_act) goto 8120
 10   continue
      do 20, c_n = 1, n_c
         if (c_lbc(c_n) .gt. c_ubc(c_n)) goto 8200
         if (c_lbc(c_n) .gt.  tl_mx_act) goto 8210
         if (c_ubc(c_n) .lt. -tl_mx_act) goto 8220
 20   continue
 7000 continue
      return
 8100 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810)
     &     r_n, r_lbc(r_n), r_ubc(r_n)
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8110 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9811)
     &     r_n, r_lbc(r_n), tl_mx_act
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8120 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9812)
     &     r_n, r_ubc(r_n), -tl_mx_act
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8200 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9820)
     &     c_n, c_lbc(c_n), c_ubc(c_n)
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8210 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9821)
     &     r_n, r_lbc(r_n), tl_mx_act
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 8220 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9822)
     &     r_n, r_ubc(r_n), -tl_mx_act
      call ems_msg_wr_li(serious_msg_n)
      goto 7000
 9810 format('Invalid bound combination for row ', i7,
     &     ' Lower bound/cost = ', g11.4, ' Upper bound/cost = ', g11.4)
 9811 format('Invalid lower bound/cost for row ', i7,
     &     ' Lower bound/cost = ', g11.4, ' exceeds ', g11.4)
 9812 format('Invalid lower bound/cost for row ', i7,
     &     ' Upper bound/cost = ', g11.4, ' exceeds ', g11.4)
 9820 format('Invalid bound combination for column ', i7,
     &     ' Lower bound/cost = ', g11.4, ' Upper bound/cost = ', g11.4)
 9821 format('Invalid lower bound/cost for row ', i7,
     &     ' Lower bound/cost = ', g11.4, ' exceeds ', g11.4)
 9822 format('Invalid lower bound/cost for row ', i7,
     &     ' Upper bound/cost = ', g11.4, ' exceeds ', g11.4)
      end
 
C->>> ------------------------------------------------> ems_iz_df_nm <<<
c     Give names of the form f_ch//'nnnnnnn' to rl_nm(f_nm_n:l_nm_n)
c
      subroutine ems_iz_df_nm(f_ch,
     &     ml_nm_n_ch, ml_nm_n_rl, f_nm_n, l_nm_n, rl_nm)
      implicit none
      include 'EMSV.INC'
      include 'EMSFI.INC'
      integer ml_nm_n_ch, ml_nm_n_rl, f_nm_n, l_nm_n
      character*1 f_ch
      double precision rl_nm(ml_nm_n_rl, l_nm_n)
      character*7 ems_wr_i_t_ch7
      integer nm_n
      character*(ml_nm_mx_n_ch) mu_ch
      character*8 ch8
      double precision rl
      equivalence (rl, ch8)
 
      if (ml_nm_n_ch .eq. 8) then
         do 10, nm_n = f_nm_n, l_nm_n
            ch8 = f_ch//ems_wr_i_t_ch7(nm_n)
            rl_nm(1, nm_n) = rl
 10      continue
      else
         call ems_iz_ch(mu_ch, ' ')
         do 20, nm_n = f_nm_n, l_nm_n
            mu_ch(1:8) = f_ch//ems_wr_i_t_ch7(nm_n)
            call ems_ch_t_rl(ml_nm_n_ch, mu_ch, rl_nm(1, nm_n))
 20      continue
      endif
      return
      end
C->>> -------------------------------------------> ems_decr_heap_srt <<<
c     Sorts the entries of heap_ix and heap_v by decreasing values of
c     heap_v.
c
      subroutine ems_decr_heap_srt(n, heap_v, heap_ix)
      implicit none
      integer n
      double precision heap_v(0:n)
      integer heap_ix(0:n)
      integer fo_p, srt_p, ix, pa_p, cd_p
      double precision v
 
      if (n .le. 1) goto 7000
      if (heap_ix(0) .ne. 1) then
c
c     The data are assumed to be completely unordered. A heap will be
c     formed and then sorted.
c
         fo_p = n/2 + 1
         srt_p = n
      else
c
c     The data are assumed to form a heap which is to be sorted.
c
         fo_p = 1
         srt_p = n
      endif
 10   continue
      if (fo_p .gt. 1) then
         fo_p = fo_p - 1
         v = heap_v(fo_p)
         ix = heap_ix(fo_p)
      else
         v = heap_v(srt_p)
         ix = heap_ix(srt_p)
         heap_v(srt_p) = heap_v(1)
         heap_ix(srt_p) = heap_ix(1)
         srt_p = srt_p - 1
         if (srt_p .eq. 1) then
            heap_v(1) = v
            heap_ix(1) = ix
            goto 7000
         endif
      endif
      pa_p = fo_p
      cd_p = fo_p + fo_p
 20   continue
      if (cd_p .le. srt_p) then
         if (cd_p .lt. srt_p) then
            if (heap_v(cd_p) .gt. heap_v(cd_p+1)) cd_p = cd_p + 1
         endif
         if (v .gt. heap_v(cd_p)) then
            heap_v(pa_p) = heap_v(cd_p)
            heap_ix(pa_p) = heap_ix(cd_p)
            pa_p = cd_p
            cd_p = cd_p + cd_p
            go to 20
         endif
      endif
      heap_v(pa_p) = v
      heap_ix(pa_p) = ix
      go to 10
 7000 continue
      return
      end
 
C->>> ----------------------------------------> ems_add_to_decr_heap <<<
c     Adds v with associated index ix to the heap if v is greater than
c     the current least entry or the heap is not full, removing the
c     current least entry if the heap is full.
c
      subroutine ems_add_to_decr_heap(n, mx_n, heap_v, heap_ix, v, ix)
      implicit none
      integer n, mx_n
      double precision heap_v(0:mx_n), v
      integer heap_ix(0:mx_n), ix
      integer pa_p, cd_p
 
      if (n .lt. mx_n) then
c
c     The heap is not full so put the new value at the bottom of the
c     heap and let it rise up to its correct level.
c
         n = n + 1
         cd_p = n
         pa_p = cd_p/2
 10      continue
         if (pa_p .gt. 0) then
            if (v .lt. heap_v(pa_p)) then
               heap_v(cd_p) = heap_v(pa_p)
               heap_ix(cd_p) = heap_ix(pa_p)
               cd_p = pa_p
               pa_p = pa_p/2
               goto 10
            endif
         endif
         heap_v(cd_p) = v
         heap_ix(cd_p) = ix
         goto 7000
      else
         if (v .le. heap_v(1)) goto 7000
c
c     The heap is full so replace the least value with the new value
c     and let it sink down to its correct level.
c
         pa_p = 1
         cd_p = pa_p + pa_p
 20      continue
         if (cd_p .le. n) then
            if (cd_p .lt. n) then
               if (heap_v(cd_p) .gt. heap_v(cd_p+1)) cd_p = cd_p + 1
            endif
            if (v .gt. heap_v(cd_p)) then
               heap_v(pa_p) = heap_v(cd_p)
               heap_ix(pa_p) = heap_ix(cd_p)
               pa_p = cd_p
               cd_p = cd_p + cd_p
               go to 20
            endif
         endif
      endif
      heap_v(pa_p) = v
      heap_ix(pa_p) = ix
 7000 continue
c
c     Set heap_ix(0)=1 to indicate that the values form a heap.
c
      heap_ix(0) = 1
      return
      end
 
C->>> -----------------------------------> ems_incr_heap_srt_i_no_ix <<<
c     Sorts the entries of heap_i_v increasing values of heap_i_v.
c
      subroutine ems_incr_heap_srt_i_no_ix(n, heap_i_v)
      implicit none
      integer n
      integer heap_i_v(0:n)
      integer fo_p, srt_p, pa_p, cd_p
      integer i_v
 
      if (n .le. 1) goto 7000
      if (heap_i_v(0) .ne. 1) then
c
c     The data are assumed to be completely unordered. A heap will be
c     formed and then sorted.
c
         fo_p = n/2 + 1
         srt_p = n
      else
c
c     The data are assumed to form a heap which is to be sorted.
c
         fo_p = 1
         srt_p = n
      endif
 10   continue
      if (fo_p .gt. 1) then
         fo_p = fo_p - 1
         i_v = heap_i_v(fo_p)
      else
         i_v = heap_i_v(srt_p)
         heap_i_v(srt_p) = heap_i_v(1)
         srt_p = srt_p - 1
         if (srt_p .eq. 1) then
            heap_i_v(1) = i_v
            goto 7000
         endif
      endif
      pa_p = fo_p
      cd_p = fo_p + fo_p
 20   continue
      if (cd_p .le. srt_p) then
         if (cd_p .lt. srt_p) then
            if (heap_i_v(cd_p) .lt. heap_i_v(cd_p+1)) cd_p = cd_p + 1
         endif
         if (i_v .lt. heap_i_v(cd_p)) then
            heap_i_v(pa_p) = heap_i_v(cd_p)
            pa_p = cd_p
            cd_p = cd_p + cd_p
            go to 20
         endif
      endif
      heap_i_v(pa_p) = i_v
      go to 10
 7000 continue
      return
      end
 
C->>> -------------------------------------------> ems_incr_heap_srt <<<
c     Sorts the entries of heap_ix and heap_v by increasing values of
c     heap_v.
c
      subroutine ems_incr_heap_srt(n, heap_v, heap_ix)
      implicit none
      integer n
      double precision heap_v(0:n)
      integer heap_ix(0:n)
      integer fo_p, srt_p, ix, pa_p, cd_p
      double precision v
 
      if (n .le. 1) goto 7000
      if (heap_ix(0) .ne. 1) then
c
c     The data are assumed to be completely unordered. A heap will be
c     formed and then sorted.
c
         fo_p = n/2 + 1
         srt_p = n
      else
c
c     The data are assumed to form a heap which is to be sorted.
c
         fo_p = 1
         srt_p = n
      endif
 10   continue
      if (fo_p .gt. 1) then
         fo_p = fo_p - 1
         v = heap_v(fo_p)
         ix = heap_ix(fo_p)
      else
         v = heap_v(srt_p)
         ix = heap_ix(srt_p)
         heap_v(srt_p) = heap_v(1)
         heap_ix(srt_p) = heap_ix(1)
         srt_p = srt_p - 1
         if (srt_p .eq. 1) then
            heap_v(1) = v
            heap_ix(1) = ix
            goto 7000
         endif
      endif
      pa_p = fo_p
      cd_p = fo_p + fo_p
 20   continue
      if (cd_p .le. srt_p) then
         if (cd_p .lt. srt_p) then
            if (heap_v(cd_p) .lt. heap_v(cd_p+1)) cd_p = cd_p + 1
         endif
         if (v .lt. heap_v(cd_p)) then
            heap_v(pa_p) = heap_v(cd_p)
            heap_ix(pa_p) = heap_ix(cd_p)
            pa_p = cd_p
            cd_p = cd_p + cd_p
            go to 20
         endif
      endif
      heap_v(pa_p) = v
      heap_ix(pa_p) = ix
      go to 10
 7000 continue
      return
      end
 
C->>> ----------------------------------------> ems_add_to_incr_heap <<<
c     Adds v with associated index ix to the heap if v is less than
c     the current greatest entry or the heap is not full, removing the
c     current greatest entry if the heap is full.
c
      subroutine ems_add_to_incr_heap(n, mx_n, heap_v, heap_ix, v, ix)
      implicit none
      integer n, mx_n
      double precision heap_v(0:mx_n), v
      integer heap_ix(0:mx_n), ix
      integer pa_p, cd_p
 
      if (n .lt. mx_n) then
c
c     The heap is not full so put the new value at the bottom of the
c     heap and let it rise up to its correct level.
c
         n = n + 1
         cd_p = n
         pa_p = cd_p/2
 10      continue
         if (pa_p .gt. 0) then
            if (v .gt. heap_v(pa_p)) then
               heap_v(cd_p) = heap_v(pa_p)
               heap_ix(cd_p) = heap_ix(pa_p)
               cd_p = pa_p
               pa_p = pa_p/2
               goto 10
            endif
         endif
         heap_v(cd_p) = v
         heap_ix(cd_p) = ix
         goto 7000
      else
         if (v .ge. heap_v(1)) goto 7000
c
c     The heap is full so replace the greatest value with the new value
c     and let it sink down to its correct level.
c
         pa_p = 1
         cd_p = pa_p + pa_p
 20      continue
         if (cd_p .le. n) then
            if (cd_p .lt. n) then
               if (heap_v(cd_p) .lt. heap_v(cd_p+1)) cd_p = cd_p + 1
            endif
            if (v .lt. heap_v(cd_p)) then
               heap_v(pa_p) = heap_v(cd_p)
               heap_ix(pa_p) = heap_ix(cd_p)
               pa_p = cd_p
               cd_p = cd_p + cd_p
               go to 20
            endif
         endif
      endif
      heap_v(pa_p) = v
      heap_ix(pa_p) = ix
 7000 continue
c
c     Set heap_ix(0)=1 to indicate that the values form a heap.
c
      heap_ix(0) = 1
      return
      end
 
C->>> --------------------------------> ems_add_to_incr_heap_w_aux_v <<<
c     According to the truth of repl, either
c
c     replace the current greatest (value, index) with (srt_v, ix)
c     ---unless the heap is empty or
c
c     add (srt_v, ix) to the heap if less than the current
c     greatest(value, index)---unless the the heap is full, in which
c     case the current greatest (value, index) is replaced.
c
c     The value of aux_v is stored and moved within heap_aux_v in to
c     correspond to srt_v.
c     NB heap_ix(0) is used to store the current number of entries in
c     the heap and is assumed to be initialised to zero.
c
      subroutine ems_add_to_incr_heap_w_aux_v(repl, mx_n,
     &     heap_srt_v, heap_aux_v, heap_ix,
     &     srt_v, aux_v, ix)
      implicit none
      logical repl
      integer mx_n
      double precision heap_srt_v(0:mx_n), srt_v
      double precision heap_aux_v(0:mx_n), aux_v
      integer heap_ix(0:mx_n), ix
      integer n, pa_p, cd_p
 
      n = heap_ix(0)
CM      IF (emsol_deb .EQ. 1) THEN
C?      if (repl .and. n .eq. 0)
C?     &     print*,' Trying to replace in an empty heap'
C?      if (mx_n .eq. 0) then
C?         print*,' Trying to operate on a heap with mx_n = 0'
C?         goto 7000
C?      endif
CM      ENDIF
      if (n .eq. 0 .or. (.not. repl .and. n .lt. mx_n)) then
c
c     The heap is not full and a new index is to be added so put the new
c     value at the bottom of the heap and let it rise up to its correct
c     level.
c
         n = n + 1
         cd_p = n
         pa_p = cd_p/2
 10      continue
         if (pa_p .gt. 0) then
            if (srt_v .gt. heap_srt_v(pa_p)) then
               heap_srt_v(cd_p) = heap_srt_v(pa_p)
               heap_aux_v(cd_p) = heap_aux_v(pa_p)
               heap_ix(cd_p) = heap_ix(pa_p)
               cd_p = pa_p
               pa_p = pa_p/2
               goto 10
            endif
         endif
         heap_srt_v(cd_p) = srt_v
         heap_aux_v(cd_p) = aux_v
         heap_ix(cd_p) = ix
         goto 7000
      else
         if (n .eq. mx_n .and. srt_v .ge. heap_srt_v(1)) goto 7000
c
c     The new value is to replace the current greatest value so do this
c     and let it sink down to its correct level.
c
         pa_p = 1
         cd_p = pa_p + pa_p
 20      continue
         if (cd_p .le. n) then
            if (cd_p .lt. n) then
               if (heap_srt_v(cd_p) .lt. heap_srt_v(cd_p+1))
     &              cd_p = cd_p + 1
            endif
            if (srt_v .lt. heap_srt_v(cd_p)) then
               heap_srt_v(pa_p) = heap_srt_v(cd_p)
               heap_aux_v(pa_p) = heap_aux_v(cd_p)
               heap_ix(pa_p) = heap_ix(cd_p)
               pa_p = cd_p
               cd_p = cd_p + cd_p
               go to 20
            endif
         endif
      endif
      heap_srt_v(pa_p) = srt_v
      heap_aux_v(pa_p) = aux_v
      heap_ix(pa_p) = ix
 7000 continue
c
c     Store the number of values in heap_ix(0)---which also indicates
c     that the values form a heap.
c
      heap_ix(0) = n
      return
      end
 
C->>> -------------------------------------------> ems_incr_heap_srt <<<
c     Sorts the entries of heap_ix, heap_srt_v and heap_aux_v by
c     increasing values of heap_srt_v.
c
      subroutine ems_incr_heap_srt_w_aux_v(
     &     n, heap_srt_v, heap_aux_v, heap_ix)
      implicit none
      integer n
      double precision heap_srt_v(0:n)
      double precision heap_aux_v(0:n)
      integer heap_ix(0:n)
      integer fo_p, srt_p, ix, pa_p, cd_p
      double precision srt_v, aux_v
 
      if (n .le. 1) goto 7000
      if (heap_ix(0) .le. 0) then
c
c     The data are assumed to be completely unordered. A heap will be
c     formed and then sorted.
c
         fo_p = n/2 + 1
         srt_p = n
      else
c
c     The data are assumed to form a heap which is to be sorted.
c
         fo_p = 1
         srt_p = n
      endif
 10   continue
      if (fo_p .gt. 1) then
         fo_p = fo_p - 1
         srt_v = heap_srt_v(fo_p)
         aux_v = heap_aux_v(fo_p)
         ix = heap_ix(fo_p)
      else
         srt_v = heap_srt_v(srt_p)
         aux_v = heap_aux_v(srt_p)
         ix = heap_ix(srt_p)
         heap_srt_v(srt_p) = heap_srt_v(1)
         heap_aux_v(srt_p) = heap_aux_v(1)
         heap_ix(srt_p) = heap_ix(1)
         srt_p = srt_p - 1
         if (srt_p .eq. 1) then
            heap_srt_v(1) = srt_v
            heap_aux_v(1) = aux_v
            heap_ix(1) = ix
            goto 7000
         endif
      endif
      pa_p = fo_p
      cd_p = fo_p + fo_p
 20   continue
      if (cd_p .le. srt_p) then
         if (cd_p .lt. srt_p) then
            if (heap_srt_v(cd_p) .lt. heap_srt_v(cd_p+1))
     &           cd_p = cd_p + 1
         endif
         if (srt_v .lt. heap_srt_v(cd_p)) then
            heap_srt_v(pa_p) = heap_srt_v(cd_p)
            heap_aux_v(pa_p) = heap_aux_v(cd_p)
            heap_ix(pa_p) = heap_ix(cd_p)
            pa_p = cd_p
            cd_p = cd_p + cd_p
            go to 20
         endif
      endif
      heap_srt_v(pa_p) = srt_v
      heap_aux_v(pa_p) = aux_v
      heap_ix(pa_p) = ix
      go to 10
 7000 continue
      return
      end
 
C->>> --------------------------------------------> ems_g_inv_o_perm <<<
c     Gets the inverse of a given permutation.
c
      subroutine ems_g_inv_o_perm(n_r, perm, inv_o_perm)
      implicit none
      integer n_r
      integer perm(0:n_r), inv_o_perm(0:n_r)
      integer r_n
 
      do 10, r_n = 1, n_r
         inv_o_perm(perm(r_n)) = r_n
 10   continue
      return
      end
 
C->>> ----------------------------------------------------> ems_scpr <<<
      double precision function ems_scpr(a, b, c, n)
      implicit none
      integer n
      double precision a, b(0:n), c(0:n)
      integer i
      ems_scpr = a
      do 1 i = 1, n
 1       ems_scpr = ems_scpr + b(i)*c(i)
      return
      end
 
C->>> ---------------------------------------------------> ems_saxpy <<<
      subroutine ems_saxpy(a, x, y, n)
      implicit none
      include 'EMSV.INC'
      integer n
      double precision a, x(0:n), y(0:n)
      integer i
      if (a .eq. zero) return
      if (a .eq. one) then
         do 1, i = 1, n
           y(i) = y(i) + x(i)
 1      continue
      else if (a .eq. -one) then
         do 2, i = 1, n
            y(i) = y(i) - x(i)
 2       continue
      else
         do 3, i = 1, n
            y(i) = y(i) + a*x(i)
 3       continue
      end if
      return
      end
 
C->>> ----------------------------------------------------> ems_exch <<<
      subroutine ems_exch(i, j)
      implicit none
      integer i, j, k
      k = i
      i = j
      j = k
      return
      end
C->>> -------------------------------------------------> ems_rl_exch <<<
      subroutine ems_rl_exch(i, j)
      implicit none
      double precision i, j, k
      k = i
      i = j
      j = k
      return
      end
 
C->>> --------------------------------------------> ems_wr_rl_t_ch20 <<<
      character*20 function ems_wr_rl_t_ch20(rl)
      implicit none
      include 'EMSV.INC'
      double precision rl
      character*20 ch20
      if (rl .eq. undn) then
         ems_wr_rl_t_ch20 = '                    '
      else if (rl .ge.  inf) then
         ems_wr_rl_t_ch20 = '          + Infinity'
      else if (rl .le. -inf) then
         ems_wr_rl_t_ch20 = '          - Infinity'
      else if (abs(rl) .le. wr_rl_ze) then
         ems_wr_rl_t_ch20 = '           .        '
      else
         if (abs(rl) .ge. 1d-3 .and.
     &        rl .gt. -1d10 .and. rl .lt. 1d11) then
            write(ch20, 9000) rl
         else
            write(ch20, 9100) rl
         end if
         ems_wr_rl_t_ch20 = ch20
      end if
      return
 9000 format(f20.8)
 9100 format(e20.14)
      end
 
C->>> --------------------------------------------> ems_wr_rl_t_ch12 <<<
      character*12 function ems_wr_rl_t_ch12(rl)
      implicit none
      include 'EMSV.INC'
      double precision rl
      character*12 ch12
      if (rl .ge. inf) then
         ems_wr_rl_t_ch12 = ' + Infinity '
      else if (rl .le. -inf) then
         ems_wr_rl_t_ch12 = ' - Infinity '
      else if (abs(rl) .le. wr_rl_ze) then
         ems_wr_rl_t_ch12 = '     .      '
      else
         if (abs(rl) .ge. 0.1 .and.
     &        rl .gt. -10000.0 .and. rl .lt. 100000.0) then
            write(ch12, 9000) rl
         else
            write(ch12, 9100) rl
         end if
         ems_wr_rl_t_ch12 = ch12
      end if
      return
 9000 format(f12.6)
 9100 format(e12.6)
      end
 
C->>> ---------------------------------------------> ems_wr_rl_t_ch4 <<<
      character*4 function ems_wr_rl_t_ch4(rl)
      implicit none
      include 'EMSV.INC'
      double precision rl
      character*4 ch4
      character*5 ch5
      if (rl .ge. inf) then
         ch4 = '+Inf'
      else if (rl .le. -inf) then
         ch4 = '-Inf'
      else if (abs(rl) .le. wr_rl_ze) then
         ch4 = '  . '
      else if (rl .eq. one) then
         ch4 = '  1 '
      else if (rl .eq. -one) then
         ch4 = ' -1 '
      else if (rl .eq. two) then
         ch4 = '  2 '
      else if (rl .eq. -two) then
         ch4 = ' -2 '
      else if (rl .lt. -994.9) then
         ch4 = '----'
      else if (rl .ge. 9995.0) then
         ch4 = '++++'
      else if (rl .le. -99.49 .or. rl .ge. 999.5) then
         write(ch5, 9099)rl
         ch4 = ch5(1:4)
      else if (rl .le. -9.949 .or. rl .ge. 99.95) then
         write(ch4, 9100)rl
      else if (rl .le. -0.9949 .or. rl .ge. 9.995) then
         write(ch4, 9101)rl
      else if (rl .lt. -0.00049 .or. rl .ge. 0.9995) then
         write(ch4, 9102)rl
      else
         write(ch4, 9103)rl
      endif
      ems_wr_rl_t_ch4 = ch4
      return
 9099 format(f5.0)
 9100 format(f4.0)
 9101 format(f4.1)
 9102 format(f4.2)
 9103 format(f4.3)
      end
 
C->>> -------------------------------------------------> ems_i_t_ch3 <<<
c     Determines the character representation of an integer in the range
c     0 .. 999
c     The value is right-justified and sa returns the position of the
c     first digit. Used for constructing file extensions.
c     NB if i<0 then ch3 is char(ichar('0') - mod(i, 10))
c        if i>999 then sa=1 and ch3 contains the last 3 digits.
c
      subroutine ems_i_t_ch3(i, ch3, sa)
      implicit none
      integer i, sa
      character*3 ch3
      integer j, k, r
      sa = 4
      k = i
 1    continue
      sa = sa - 1
      j = k
      k = k/10
      r = j - k*10
      ch3(sa:sa) = char(ichar('0') + r)
      if (k .gt. 0 .and. sa .gt. 1) go to 1
      return
      end
 
C->>> ----------------------------------------------> ems_wr_i_t_ch2 <<<
      character*2 function ems_wr_i_t_ch2(i)
      implicit none
      integer i
      integer ems_pos_mod
      if (i .lt. 0 .or. i .gt. 99) go to 8000
      ems_wr_i_t_ch2 =
     &     char(48+ems_pos_mod(i/10, 10))//
     &     char(48+ems_pos_mod(i, 10))
 7000 continue
      return
 8000 continue
      ems_wr_i_t_ch2 = '  '
      go to 7000
      end
 
C->>> ----------------------------------------------> ems_wr_i_t_ch4 <<<
      character*4 function ems_wr_i_t_ch4(i)
      implicit none
      integer i
      integer ems_pos_mod
      if (i .lt. 0 .or. i .gt. 9999) go to 8001
      ems_wr_i_t_ch4 =
     &     char(48+ems_pos_mod(i/1000, 10))//
     &     char(48+ems_pos_mod(i/100, 10))//
     &     char(48+ems_pos_mod(i/10, 10))//
     &     char(48+ems_pos_mod(i, 10))
 7000 continue
      return
 8001 continue
      ems_wr_i_t_ch4 = '    '
      go to 7000
      end
 
C->>> ----------------------------------------------> ems_wr_i_t_ch7 <<<
      character*7 function ems_wr_i_t_ch7(i)
      implicit none
      integer i
      integer ems_pos_mod
      if (i .lt. 0 .or. i .gt. 9999999) go to 8001
      ems_wr_i_t_ch7 =
     &     char(48+ems_pos_mod(i/1000000, 10))//
     &     char(48+ems_pos_mod(i/100000, 10))//
     &     char(48+ems_pos_mod(i/10000, 10))//
     &     char(48+ems_pos_mod(i/1000, 10))//
     &     char(48+ems_pos_mod(i/100, 10))//
     &     char(48+ems_pos_mod(i/10, 10))//
     &     char(48+ems_pos_mod(i, 10))
 7000 continue
      return
 8001 continue
      ems_wr_i_t_ch7 = '       '
      go to 7000
      end
 
C->>> -----------------------------------------------> ems_i_t_i_pct <<<
c     Return a whole percentage (rounding correctly) for one integer out
c     of another.
c
      integer function ems_i_t_i_pct(i1, i2)
      implicit none
      integer i1, i2
      if (i1 .ge. 0 .and. i2 .gt. 0) then
         ems_i_t_i_pct = (200*i1+i2)/(2*i2)
      else if (i1 .eq. 0 .and. i2 .eq. 0) then
         ems_i_t_i_pct = 0
      else
         ems_i_t_i_pct = -1
      endif
      return
      end
 
C->>> ----------------------------------------------> ems_rl_t_i_pct <<<
c     Return a whole percentage (rounding correctly) for one real out
c     of another.
c
      integer function ems_rl_t_i_pct(rl1, rl2)
      implicit none
      include 'EMSV.INC'
      double precision rl1, rl2
      double precision rl_pct
      integer i_pct
      if (rl1 .ge. zero .and. rl2 .gt. zero) then
         rl_pct = 1d2*rl1/rl2
      else if (rl1 .eq. zero .and. rl2 .eq. zero) then
         rl_pct = zero
      else
         rl_pct = -one
      endif
      i_pct = rl_pct
      ems_rl_t_i_pct = i_pct
      return
      end
 
C->>> -------------------------------------------> ems_str_t_lo_case <<<
      subroutine ems_str_t_lo_case(str)
      implicit none
      character*(*) str
      integer n_ch, ch_n, ix
      integer ichar_lo_case_a
      integer ichar_up_case_a
      integer ichar_up_case_z
      integer dl_ichar
 
      n_ch = len(str)
      ichar_lo_case_a = ichar('a')
      ichar_up_case_a = ichar('A')
      ichar_up_case_z = ichar('Z')
      dl_ichar = ichar_lo_case_a - ichar_up_case_a
      do 10, ch_n = 1, n_ch
         ix = ichar(str(ch_n:ch_n))
         if (ix .ge. ichar_up_case_a .and. ix .le. ichar_up_case_z)
     &        str(ch_n:ch_n) = char(ix+dl_ichar)
 10   continue
      return
      end
 
C->>> -------------------------------------------> ems_str_t_up_case <<<
      subroutine ems_str_t_up_case(str)
      implicit none
      character*(*) str
      integer n_ch, ch_n, ix
      integer ichar_lo_case_a
      integer ichar_lo_case_z
      integer ichar_up_case_a
      integer dl_ichar
 
      n_ch = len(str)
      ichar_lo_case_a = ichar('a')
      ichar_lo_case_z = ichar('z')
      ichar_up_case_a = ichar('A')
      dl_ichar = ichar_up_case_a - ichar_lo_case_a
      do 10, ch_n = 1, n_ch
         ix = ichar(str(ch_n:ch_n))
         if (ix .ge. ichar_lo_case_a .and. ix .le. ichar_lo_case_z)
     &        str(ch_n:ch_n) = char(ix+dl_ichar)
 10   continue
      return
      end
 
c->>> ---------------------------------------------------> ems_iz_ch <<<
      subroutine ems_iz_ch(ch, ch1)
      implicit none
      character*(*) ch
      character*1 ch1
      integer n_ch, p
      n_ch = len(ch)
      do 1, p = 1, n_ch
         ch(p:p) = ch1
 1    continue
      return
      end
 
c->>> -------------------------------------------------> ems_ch_t_rl <<<
      subroutine ems_ch_t_rl(n_ch, ch, rl)
      implicit none
      integer n_ch
      character*(*) ch
      double precision rl(*)
      integer n_rl, n_fu_rl, n_xa_ch, p, rl_n
      character*8 lc_ch
      double precision lc_rl
      equivalence (lc_rl, lc_ch)
 
      n_rl = (n_ch+7)/8
      n_fu_rl = n_ch/8
      p = 0
      do 1, rl_n = 1, n_fu_rl
         lc_ch = ch(p+1:p+8)
         rl(rl_n) = lc_rl
         p = p + 8
 1    continue
      if (n_fu_rl .lt. n_rl) then
         n_xa_ch = n_ch - n_fu_rl*8
         lc_ch = '        '
         lc_ch(1:n_xa_ch) = ch(p+1:n_ch)
         rl(n_rl) = lc_rl
      endif
      return
      end
 
c->>> -------------------------------------------------> ems_rl_t_ch <<<
      subroutine ems_rl_t_ch(n_ch, rl, ch)
      implicit none
      integer n_ch
      double precision rl(*)
      character*(*) ch
      integer n_rl, n_fu_rl, p, rl_n
      double precision lc_rl
      character*8 lc_ch
      equivalence (lc_rl, lc_ch)
 
      n_rl = (n_ch+7)/8
      n_fu_rl = n_ch/8
      p = 0
      do 1, rl_n = 1, n_fu_rl
         lc_rl = rl(rl_n)
         ch(p+1:p+8) = lc_ch
         p = p + 8
 1    continue
      if (n_fu_rl .lt. n_rl) then
         lc_rl = rl(n_rl)
         ch(p+1:p+8) = lc_ch
      endif
      return
      end
 
C->>> -------------------------------------------------------> chkrt <<<
c     Analyses the return code and writes appropriate messages for a
c     routine name of upto 32 characters.
c
      subroutine chkrt(rn_nm, rt_cod)
      implicit none
      include 'EMSMSG.INC'
      character*(*) rn_nm
      integer rt_cod
      integer ems_ln_t_f_bl
      integer rn_nm_ln_t_f_bl
      integer rt_cod_mod_100, rt_cod_div_100
 
      rt_cod_mod_100 = mod(rt_cod, 100)
      rt_cod_div_100 = rt_cod/100
      rn_nm_ln_t_f_bl = ems_ln_t_f_bl(rn_nm)
      if (rt_cod_div_100 .eq. 0) then
         if (rn_nm_ln_t_f_bl .ge. 1) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &           rt_cod, rn_nm(1:rn_nm_ln_t_f_bl)
         else
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)rt_cod, ' '
         endif
         call ems_msg_wr_li(info_msg_n)
      else if (rt_cod_div_100 .eq. 1) then
         if (rn_nm_ln_t_f_bl .ge. 1) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9001)
     &           rt_cod, rn_nm(1:rn_nm_ln_t_f_bl)
         else
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9001)rt_cod, ' '
         endif
         call ems_msg_wr_li(warn_msg_n)
      else if (rt_cod_div_100 .eq. 2) then
         if (rn_nm_ln_t_f_bl .ge. 1) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9002)
     &           rt_cod, rn_nm(1:rn_nm_ln_t_f_bl)
         else
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9002)rt_cod, ' '
         endif
         call ems_msg_wr_li(er_msg_n)
      else if (rt_cod_div_100 .eq. 3) then
         if (rn_nm_ln_t_f_bl .ge. 1) then
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9003)
     &           rt_cod, rn_nm(1:rn_nm_ln_t_f_bl)
         else
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9003)rt_cod, ' '
         endif
         call ems_msg_wr_li(serious_msg_n)
      end if
      return
 9000 format('********** Return code of ', i4,
     &     ': Info message issued during call to ', a, ' **********')
 9001 format('********** Return code of ', i4,
     &     ': Warning issued during call to ', a, ' **********')
 9002 format('********** Return code of ', i4,
     &     ': Error occurred during call to ', a, ' **********')
 9003 format('********** Return code of ', i4,
     &     ': Severe error occurred during call to ', a, ' **********')
      end
 
C->>> ---------------------------------------------> ems_iz_bt_a_com <<<
c     Assign values to the bit array.
c
      subroutine ems_iz_bt_a_com
      implicit none
      include 'EMSV.INC'
      bt_a( 1) = bt 1
      bt_a( 2) = bt 2
      bt_a( 3) = bt 3
      bt_a( 4) = bt 4
      bt_a( 5) = bt 5
      bt_a( 6) = bt 6
      bt_a( 7) = bt 7
      bt_a( 8) = bt 8
      bt_a( 9) = bt 9
      bt_a(10) = bt10
      bt_a(11) = bt11
      bt_a(12) = bt12
      bt_a(13) = bt13
      bt_a(14) = bt14
      bt_a(15) = bt15
      bt_a(16) = bt16
      bt_a(17) = bt17
      bt_a(18) = bt18
      bt_a(19) = bt19
      bt_a(20) = bt20
      bt_a(21) = bt21
      bt_a(22) = bt22
      bt_a(23) = bt23
      bt_a(24) = bt24
      bt_a(25) = bt25
      bt_a(26) = bt26
      bt_a(27) = bt27
      bt_a(28) = bt28
      bt_a(29) = bt29
      bt_a(30) = bt30
      bt_a(31) = bt31
      bt_a(32) = bt32
CM      IF (t3d_lib .EQ. 1) THEN
C?      bt_a(33) = bt33
C?      bt_a(34) = bt34
C?      bt_a(35) = bt35
C?      bt_a(36) = bt36
C?      bt_a(37) = bt37
C?      bt_a(38) = bt38
C?      bt_a(39) = bt39
C?      bt_a(40) = bt40
C?      bt_a(41) = bt41
C?      bt_a(42) = bt42
C?      bt_a(43) = bt43
C?      bt_a(44) = bt44
C?      bt_a(45) = bt45
C?      bt_a(46) = bt46
C?      bt_a(47) = bt47
C?      bt_a(48) = bt48
C?      bt_a(49) = bt49
C?      bt_a(50) = bt50
C?      bt_a(51) = bt51
C?      bt_a(52) = bt52
C?      bt_a(53) = bt53
C?      bt_a(54) = bt54
C?      bt_a(55) = bt55
C?      bt_a(56) = bt56
C?      bt_a(57) = bt57
C?      bt_a(58) = bt58
C?      bt_a(59) = bt59
C?      bt_a(60) = bt60
C?      bt_a(61) = bt61
C?      bt_a(62) = bt62
C?      bt_a(63) = bt63
C?      bt_a(64) = bt64
CM      ENDIF
      iz_bt_a_com_fg1 = 1
      iz_bt_a_com_fg2 = 2
      return
      end
 
C->>> ---------------------------------------------------> ems_rp_bt <<<
c     Report the bist set in an integer
c
      subroutine ems_rp_bt(i)
      implicit none
      include 'EMSV.INC'
      integer i
      integer bt_n
      write(*, 9000)i
CM      IF (t3d_lib .EQ. 1) THEN
C?      do 10, bt_n = 1, 64
CM      ELSE
      do 10, bt_n = 1, 32
CM      ENDIF
         if (iand(i, bt_a(bt_n)) .ne. 0) write(*, 9010)bt_n
 10   continue
      return
 9000 format('Reporting bits set in ', i12)
 9010 format('Bit ', i2, ' is set')
      end
