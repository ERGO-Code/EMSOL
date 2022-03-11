C->>> -----------------------------------------------> ems_bp_swp_sd <<<
c     Swap a variable from one side of its breakpoint to another.
c
      subroutine ems_bp_swp_sd(ab_t_bw, vr_n, c_n, sn_n,
     &     rsmi_lb, rsmi_co, rsmi_ub, du_act,
     &     st, vr_in_c)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      include 'RSMICOM.INC'
      logical ab_t_bw
      integer vr_n, c_n, sn_n
      integer st(0:mx_n_c+n_r)
      integer vr_in_c(-vr_in_c_n_sn:n_c)
      double precision rsmi_lb
      double precision rsmi_co
      double precision rsmi_ub
      double precision du_act
      integer l_c_n, f_c_n
      integer l_vr_n, f_vr_n
      double precision bp, co, dl_co
 
      if (ab_t_bw) then
c
c     Move from above to below
c
         if (iand(st(vr_n), lb_bt) .eq. 0) goto 8000
c
c     lb:co:ub are bp:uco:lco-uco
c
         bp = rsmi_lb
         co = rsmi_co
         dl_co = rsmi_ub
         l_c_n = vr_in_c(-sn_n)
         if (c_n .lt. l_c_n) then
c
c     Have to exchange the variable at the end of the section with
c     the variable which is changing section.
c
            l_vr_n = vr_in_c(l_c_n)
            st(l_vr_n) = st(l_vr_n) - l_c_n + c_n
            vr_in_c(c_n) = l_vr_n
            st(vr_n) = st(vr_n) - c_n + l_c_n
            vr_in_c(l_c_n) = vr_n
         endif
         vr_in_c(-sn_n) = vr_in_c(-sn_n) - 1
c
c     Set lb:co:ub to uco-lco:lco:bp
c
         rsmi_lb = -dl_co
         rsmi_co = co + dl_co
         rsmi_ub = bp
c
c     Change the dual activity
c
         du_act = du_act + pr_co_mu*dl_co
c
c     Ensure that the up and down bits are set, remove the lb bit and
c     add the ub bit
c
         st(vr_n) = ior(st(vr_n), up_dn)
         st(vr_n) = st(vr_n) - lb_bt + ub_bt
      else
c
c     Move from below to above
c
         if (iand(st(vr_n), ub_bt) .eq. 0) goto 8010
c
c     lb:co:ub are uco-lco:lco:bp
c
         bp = rsmi_ub
         co = rsmi_co
         dl_co = rsmi_lb
         f_c_n = vr_in_c(-(sn_n-1)) + 1
         if (c_n .gt. f_c_n) then
c
c     Have to exchange the variable at the start of the section with
c     the variable which is changing section.
c
            f_vr_n = vr_in_c(f_c_n)
            st(f_vr_n) = st(f_vr_n) - f_c_n + c_n
            vr_in_c(c_n) = f_vr_n
            st(vr_n) = st(vr_n) - c_n + f_c_n
            vr_in_c(f_c_n) = vr_n
         endif
         vr_in_c(-(sn_n-1)) = vr_in_c(-(sn_n-1)) + 1
c
c     Set lb:co:ub to bp:uco:lco-uco
c
         rsmi_lb = bp
         rsmi_co = co + dl_co
         rsmi_ub = -dl_co
c
c     Change the dual activity
c
         du_act = du_act + pr_co_mu*dl_co
c
c     Ensure that the up and dn bits are set, remove the ub bit and add
c     the lb bit
c
         st(vr_n) = ior(st(vr_n), up_dn)
         st(vr_n) = st(vr_n) - ub_bt + lb_bt
      endif
 7000 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)vr_n
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)vr_n
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9800 format('Trying to move BP variable ', i7,
     &     ' below BP when it already is')
 9801 format('Trying to move BP variable ', i7,
     &     ' above BP when it already is')
      end
 
C->>> -----------------------------------------------> ems_g_vr_in_c <<<
c     Form vr_in_c from the list of variables unless vr_in_c_n_vr=n_c,
c     in which case vr_in_c is formed from all the variables.
c
      subroutine ems_g_vr_in_c(vr_in_c_n_vr, st, cdd_vr, vr_in_c)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICS.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      integer vr_in_c_n_vr
      integer st(0:mx_n_c+n_r)
      integer cdd_vr(0:vr_in_c_n_vr)
      integer vr_in_c(-vr_in_c_n_sn:vr_in_c_n_vr+1)
      integer ems_g_vr_in_c_sn_ty_o_vr
      integer ix, t_ix, vr_n, sn_ty, sn_n, c_n, p, m_sn_n
 
      if (vr_in_c_n_vr .ge. n_c) then
c
c     Loop over all nonbasic variables (according to the bc_bt in st).
c
         t_ix = n_r+n_c
      else
c
c     Loop over the variables in cdd_vr
c
         t_ix = vr_in_c_n_vr
      end if
      do 10, m_sn_n = -1, -vr_in_c_n_sn, -1
         vr_in_c(m_sn_n) = 0
 10   continue
c
c     Count the number of variables in each section of vr_in_c.
c
      do 20, ix = 1, t_ix
         if (vr_in_c_n_vr .lt. n_c) then
            vr_n = cdd_vr(ix)
            if (iand(st(vr_n), bc_bt) .ne. 0) goto 8000
         else
            if (ix .le. n_c) then
               vr_n = ix
            else
               vr_n = ix-n_c+mx_n_c
            end if
            if (iand(st(vr_n), bc_bt) .ne. 0) go to 20
         end if
         sn_ty = ems_g_vr_in_c_sn_ty_o_vr(st(vr_n))
         if (vr_n .le. n_c) then
            sn_n = struc_sn_n_os + sn_ty
         else
            sn_n = lg_sn_n_os + sn_ty
         endif
         m_sn_n = -sn_n
         vr_in_c(m_sn_n) = vr_in_c(m_sn_n) + 1
c
c     Use (some of) the bits to be used to store the position of the
c     variable in vr_in_c to store its section number temporarily.
c
         st(vr_n) = st(vr_n) - iand(st(vr_n), mx_mx_ml_a_dim) + sn_n
 20   continue
c
c     Count the total number of entries in all sections
c
      p = 0
      do 25, m_sn_n = -1, -vr_in_c_n_sn, -1
         p = p + vr_in_c(m_sn_n)
 25   continue
      if (p .gt. vr_in_c(os_vr_in_c_mx_n_c)) go to 8010
c
c     Reset the pointers to the start-1 of their section.
c
      do 30, sn_n = vr_in_c_n_sn, 1, -1
         m_sn_n = -sn_n
         p = p - vr_in_c(m_sn_n)
         vr_in_c(m_sn_n) = p
 30   continue
      if (vr_in_c(-1) .ne. 0) goto 8020
c
c     Determine the list of variables in each section.
c
      do 40, ix = 1, t_ix
         if (vr_in_c_n_vr .lt. n_c) then
            vr_n = cdd_vr(ix)
         else
            if (ix .le. n_c) then
               vr_n = ix
            else
               vr_n = ix-n_c+mx_n_c
            end if
            if (iand(st(vr_n), bc_bt) .ne. 0) go to 40
         end if
c
c     Extract the section number from the status and reset these bits.
c
         sn_n = iand(st(vr_n), mx_mx_ml_a_dim)
         st(vr_n) = st(vr_n) - sn_n
         m_sn_n = -sn_n
c
c     Increase the pointer to the end of this section and store the
c     variable there.
c
         c_n = vr_in_c(m_sn_n) + 1
         vr_in_c(m_sn_n) = c_n
         vr_in_c(c_n) = vr_n
c
c     Record where the variable appears in vr_in_c
c
         st(vr_n) = st(vr_n) + c_n
 40   continue
c
c     Assign a value to the n_c+1'st entry to avoid an unassigned
c     variable violation for the assembler u_sed_wt.
c
      if (vr_in_c_n_vr .ge. n_c) vr_in_c(n_c+1) = 0
c
c     Indicate that the model has vr_in_c correct.
c
      ml_da_st_msk = ior(ml_da_st_msk, ml_da_st_vr_in_c)
c
c     Indicate that since a new vr_in_c has been formed, any row matrix
c     is not correct.
c
      ml_da_st_msk =
     &     ml_da_st_msk - iand(ml_da_st_msk, ml_da_st_r_mtx)
 7000 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)ix, vr_n
      call ems_msg_wr_li(bug_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?      call ems_dump
CM      ENDIF
      goto 7000
 8010 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9801)
     &     vr_in_c(os_vr_in_c_mx_n_c)
      call ems_msg_wr_li(bug_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?      call ems_dump
CM      ENDIF
      go to 7000
 8020 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9802)vr_in_c(-1)
      call ems_msg_wr_li(bug_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?      call ems_dump
CM      ENDIF
      go to 7000
 9800 format('Variable cdd_vr(', i7, ') = ', i7, ' is basic ')
 9801 format('Insufficient space in vr_in_c: vr_in_c(0) = ', i9)
 9802 format('Pointer to start-1 of first section is ', i9, ' not 0')
      end
 
C->>> --------------------------------------> ems_g_vr_in_c_sn_n_o_c <<<
c     Returns the section within vr_in_c for the variable in column c_n.
c     See EMSPM.INC for the definition of the sections.
c
      integer function ems_g_vr_in_c_sn_n_o_c(c_n, vr_in_c)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICS.INC'
      include 'EMSMSG.INC'
      integer c_n, vr_in_c(-vr_in_c_n_sn:0)
      integer m_sn_n
      integer i_null
      double precision rl_null
 
      if (c_n .le. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9900)
         call ems_msg_wr_li(bug_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?         call ems_dump
CM      ENDIF
      end if
      m_sn_n = -1
      if (c_n .le. vr_in_c(m_sn_n)) go to 10
      m_sn_n = m_sn_n - 1
      if (c_n .le. vr_in_c(m_sn_n)) go to 10
      m_sn_n = m_sn_n - 1
      if (c_n .le. vr_in_c(m_sn_n)) go to 10
      m_sn_n = m_sn_n - 1
      if (c_n .le. vr_in_c(m_sn_n)) go to 10
      m_sn_n = m_sn_n - 1
      if (c_n .le. vr_in_c(m_sn_n)) go to 10
      m_sn_n = m_sn_n - 1
      if (c_n .le. vr_in_c(m_sn_n)) go to 10
      m_sn_n = m_sn_n - 1
      if (c_n .le. vr_in_c(m_sn_n)) go to 10
      m_sn_n = m_sn_n - 1
      if (c_n .le. vr_in_c(m_sn_n)) go to 10
      m_sn_n = m_sn_n - 1
      if (c_n .le. vr_in_c(m_sn_n)) go to 10
      m_sn_n = m_sn_n - 1
      if (c_n .le. vr_in_c(m_sn_n)) go to 10
      m_sn_n = m_sn_n - 1
      if (c_n .le. vr_in_c(m_sn_n)) go to 10
      m_sn_n = m_sn_n - 1
      if (c_n .le. vr_in_c(m_sn_n)) go to 10
      m_sn_n = m_sn_n - 1
      if (c_n .le. vr_in_c(m_sn_n)) go to 10
      m_sn_n = m_sn_n - 1
      if (c_n .le. vr_in_c(m_sn_n)) go to 10
      m_sn_n = m_sn_n - 1
      if (c_n .le. vr_in_c(m_sn_n)) go to 10
      m_sn_n = m_sn_n - 1
      if (c_n .le. vr_in_c(m_sn_n)) go to 10
      m_sn_n = m_sn_n - 1
      if (c_n .le. vr_in_c(m_sn_n)) go to 10
      m_sn_n = m_sn_n - 1
      if (c_n .le. vr_in_c(m_sn_n)) go to 10
      if (-m_sn_n .le. vr_in_c_n_sn) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9905)
     &        -m_sn_n, vr_in_c_n_sn
         call ems_msg_wr_li(bug_msg_n)
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9910)
     &        c_n, vr_in_c(0), m_sn_n
         call ems_msg_wr_li(bug_msg_n)
         call ems_rp_vr_in_c(1, vr_in_c, i_null, rl_null)
      endif
CM      IF (emsol_deb .EQ. 1) THEN
C?      call ems_dump
CM      ENDIF
 10   continue
      ems_g_vr_in_c_sn_n_o_c = -m_sn_n
      return
 9900 format('c_n .le. 0 in ems_g_vr_in_c_sn_n_o_c')
 9905 format('-m_sn_n = ', i2, ' .le. vr_in_c_n_sn = ', i2)
 9910 format(i7, ' = c_n .gt. n_c = ', i7, ': m_sn_n = ', i3,
     &     ' in ems_g_vr_in_c_sn_n_o_c')
      end
 
C->>> ------------------------------------> ems_g_vr_in_c_sn_ty_o_vr <<<
c     Returns the section type within (any) vr_in_c for the variable
c     vr_n.
c
      integer function ems_g_vr_in_c_sn_ty_o_vr(st)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      include 'RSMICOM.INC'
      integer st
      integer sn_ty
 
      if (iand(st, bc_bt) .ne. 0) goto 8000
c
c     HOLDS FOR ALL TYPES OF VARIABLE
c
      if (iand(st, ub_bt) .eq. 0) then
         if (iand(st, lb_bt) .eq. 0) then
            sn_ty = vr_in_c_sn_ty_btw
         else
            if (iand(st, ifs_bt) .ne. 0) then
               sn_ty = vr_in_c_sn_ty_bw_lb
            else if (iand(st, dn_bt) .eq. 0) then
               sn_ty = vr_in_c_sn_ty_at_lb
            else
               sn_ty = vr_in_c_sn_ty_btw
            endif
         endif
      else
         if (iand(st, lb_bt) .eq. 0) then
            if (iand(st, ifs_bt) .ne. 0) then
               sn_ty = vr_in_c_sn_ty_ab_ub
            else if (iand(st, up_bt) .eq. 0) then
               sn_ty = vr_in_c_sn_ty_at_ub
            else
               sn_ty = vr_in_c_sn_ty_btw
            endif
         else
            if (iand(st, ifs_bt) .ne. 0) then
               if (iand(st, up_bt) .ne. 0) then
                  sn_ty = vr_in_c_sn_ty_bw_lb
               else
                  sn_ty = vr_in_c_sn_ty_ab_ub
               endif
            else
               if (iand(st, up_bt) .eq. 0) then
                  if (iand(st, dn_bt) .eq. 0) then
                     if (iand(st, sos_bt) .ne. 0 .or.
     &                    (lp_ph .eq. 1 .and.
     &                    iand(cz_c_msk, cz_c_bk_bd_bt) .ne. 0)) then
c
c     Fix temporarily if the variable is SOS or bounds can be broken in
c     Phase I
c
                        sn_ty = vr_in_c_sn_ty_te_fx
                     else
                        sn_ty = vr_in_c_sn_ty_fx
                     endif
                  else
                     sn_ty = vr_in_c_sn_ty_at_ub
                  endif
               else
                  if (iand(st, dn_bt) .eq. 0) then
                     sn_ty = vr_in_c_sn_ty_at_lb
                  else
                     sn_ty = vr_in_c_sn_ty_btw
                  endif
               endif
            endif
         endif
      end if
      if (iand(st, alt_bt) .ne. 0) then
         if (iand(st, bp_bt) .ne. 0) then
c
c     Only difference with BP variables is that
c     at_lb should be ab_bp
c     at_ub should be bw_bp
c
            if (sn_ty .eq. vr_in_c_sn_ty_at_lb) then
               sn_ty = vr_in_c_sn_ty_ab_bp
            else if (sn_ty .eq. vr_in_c_sn_ty_at_ub) then
               sn_ty = vr_in_c_sn_ty_bw_bp
            endif
         else
c
c     Similar with PWL variables except that they can be At_Lb or At_Ub
c     if they are at entries 2 or n-1
c
         endif
      endif
      ems_g_vr_in_c_sn_ty_o_vr = sn_ty
 7000 continue
      return
 8000 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9800)
      call ems_msg_wr_li(bug_msg_n)
      goto 7000
 9800 format('ems_g_vr_in_c_sn_ty_o_vr called with basic variable')
      end
 
C->>> -----------------------------------------------> ems_u_vr_in_c <<<
c     Update vr_in_c.
c     The column of the variable to leave vr_in_c is lv_c_n (.ne. 0).
c     The variable to enter vr_in_c is en_vr_n (.ne. 0).
c
c     NB Entering variables and leaving columns refer to vr_in_c not the
c     columns entering and variables leaving the basis. Apparent changes
c     to the entering variable and leaving column in the code below do
c     not affect the values in the parameter list and simply allow the
c     movements of variables between sections of vr_in_c to be
c     programmed conveniently.
c
c     lv_c_n returns the column where variable en_vr_n actually entered.
c
c     Notes.
c     1: A bound swap corresponds to vr_in_c(lv_c_n) = en_vr_n and is
c     handled automatically.
c     2: If there is no leaving column (indicated by lv_c_n=0) then
c     the entering variable is placed in the right section and vr_in_c
c     is extended (if there is room). This may occur when building up a
c     vr_in_c.
c     3: If there is no entering variable (indicated by en_vr_n=0) then
c     the leaving variable is removed and vr_in_c is reduced. This may
c     occur when a variable becomes basic in minor iterations. In this
c     case rsmi_lb, rsmi_ub, st, pr_act, and tl_ifs are not used.
c
      subroutine ems_u_vr_in_c(usr_lv_c_n, usr_en_vr_n,
     &     rsmi_lb, rsmi_co, rsmi_ub,
     &     st, pr_act, du_act, tl_ifs, vr_in_c, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'RSMICS.INC'
      include 'RSMICOM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
CM      IF (emsol_tt .EQ. 1) THEN
C?      include 'EMSTT.INC'
CM      ENDIF
      integer usr_lv_c_n, usr_en_vr_n
      integer st(0:mx_n_c+n_r)
      integer vr_in_c(-vr_in_c_n_sn:n_c)
      integer is(0:is_n_en_m1)
      double precision rsmi_lb(0:mx_n_c+n_r)
      double precision rsmi_co(0:mx_n_c+n_r)
      double precision rsmi_ub(0:mx_n_c+n_r)
      double precision pr_act(0:mx_n_c+n_r)
      double precision du_act(0:mx_n_c+n_r)
      double precision tl_ifs
      double precision ds(0:ds_n_en_m1)
      integer ems_g_vr_in_c_sn_n_o_c
      integer lv_vr_n, lv_c_n, en_vr_n
      integer lv_sn_n, en_sn_ty, en_sn_n, sn_n, vr_n, en_c_n, m_sn_n
      integer rp_mode
 
      if (usr_lv_c_n .le. 0 .and. usr_en_vr_n .le. 0) go to 7000
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_u_lvl1) call ems_tt_rec(u_vr_in_c_tt, n_bs)
CM      ENDIF
      if (usr_lv_c_n .gt. vr_in_c(os_vr_in_c_l_p)) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9900)
         call ems_msg_wr_li(bug_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?         call ems_dump
CM      ENDIF
      end if
      lv_c_n = usr_lv_c_n
      en_vr_n = usr_en_vr_n
      if (lv_c_n .gt. 0) then
         lv_vr_n = vr_in_c(lv_c_n)
         if (lv_vr_n .le. n_c .and.
     &        lv_c_n .le. vr_in_c(os_struc_in_c_l_pc_p)) then
c
c     If the variable to leave vr_in_c was structural and being priced
c     then update the row representation of the matrix.
c
            n_pc_vr = n_pc_vr - 1
            n_pc_el = n_pc_el -
     &           (is(p_mtx_c_sa+lv_vr_n+1) - is(p_mtx_c_sa+lv_vr_n))
            if (iand(ml_da_st_msk, ml_da_st_r_mtx) .ne. 0) then
               call ems_u_ml_r_mtx(lv_vr_n, 0,
     &              is(p_mtx_r_ix), is(p_mtx_c_sa), ds(p_mtx_c_v),
     &              is(p_mtx_c_ix), is(p_mtx_r_sa))
               if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
            endif
         endif
         lv_sn_n = ems_g_vr_in_c_sn_n_o_c(lv_c_n, vr_in_c)
      else
c
c     There is no leaving column so vr_in_c will lengthen by 1. First
c     add a column to the last section (if possible). Then treat this
c     extra column as a leaving column.
c
c     Set lv_vr_n = en_vr_n so that to prevent du_act(en_vr_n) = zero
c
         lv_vr_n = en_vr_n
         if (vr_in_c(os_vr_in_c_l_p) .lt.
     &        vr_in_c(os_vr_in_c_mx_n_c)) then
            lv_c_n = vr_in_c(os_vr_in_c_l_p) + 1
            vr_in_c(os_vr_in_c_l_p) = lv_c_n
            vr_in_c(lv_c_n) = 0
            lv_sn_n = vr_in_c_n_sn
         else
            go to 8001
         end if
      end if
c      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
c     &        n_si_it+1, lv_vr_n, en_vr_n
c      call ems_msg_wr_li(info_msg_n)
c 9000 format(' Iteration', i7, ' u_vr_in_c ', 2i7)
      if (en_vr_n .gt. 0) then
c
c     Reset the status of the variable entering vr_in_c
c
c     If lv_vr_n = en_vr_n then en_vr_n was nonbasic before and
c     u_vr_in_c is just (possibly) moving this variable between sections
c     Its dual activity is updated.
c
c     Otherwise en_vr_n was basic before and its dual activity is
c     zeroed---it was theoretically zero anyway---so that any change in
c     the cost can be accumulated.
c
         if (en_vr_n .ne. lv_vr_n) du_act(en_vr_n) = zero
         if (iand(ml_da_st_msk, ml_da_st_alt_lp) .eq. 0 .or.
     &        iand(st(en_vr_n), alt_bt) .eq. 0) then
            call ems_reset_1_non_bc_vr_st(
     &           en_vr_n,
     &           en_sn_ty,
     &           st(en_vr_n),
     &           rsmi_lb(en_vr_n),
     &           rsmi_co(en_vr_n),
     &           rsmi_ub(en_vr_n),
     &           pr_act(en_vr_n),
     &           du_act(en_vr_n),
     &           tl_ifs)
         else if (iand(st(en_vr_n), bp_bt) .ne. 0) then
            call ems_reset_1_non_bc_bp_vr_st(
     &           en_vr_n,
     &           en_sn_ty,
     &           st(en_vr_n),
     &           rsmi_lb(en_vr_n),
     &           rsmi_co(en_vr_n),
     &           rsmi_ub(en_vr_n),
     &           pr_act(en_vr_n),
     &           du_act(en_vr_n),
     &           tl_ifs)
         else
            call ems_reset_1_non_bc_pwl_vr_st(
     &           en_vr_n,
     &           en_sn_ty,
     &           st(en_vr_n),
     &           rsmi_lb(en_vr_n),
     &           rsmi_co(en_vr_n),
     &           rsmi_ub(en_vr_n),
     &           ds(p_pwl_vr_da_v),
     &           is(p_pwl_vr_ls),
     &           is(p_pwl_vr_da_sa),
     &           is(p_pwl_vr_cu_sn),
     &           pr_act(en_vr_n),
     &           du_act(en_vr_n),
     &           tl_ifs)
         endif
         if (en_vr_n .le. n_c) then
            en_sn_n = struc_sn_n_os + en_sn_ty
         else
            en_sn_n = lg_sn_n_os + en_sn_ty
         endif
      else
c
c     There is no entering variable so vr_in_c will shorten by 1. First
c     remove a column from the last section (with entries) and treat the
c     corresponding variable as an entering variable.
c
         en_c_n = vr_in_c(os_vr_in_c_l_p)
         en_vr_n = vr_in_c(en_c_n)
         en_sn_n = ems_g_vr_in_c_sn_n_o_c(en_c_n, vr_in_c)
         en_c_n = en_c_n - 1
         do 5, sn_n = en_sn_n, vr_in_c_n_sn
            m_sn_n = -sn_n
            vr_in_c(m_sn_n) = en_c_n
 5       continue
         vr_in_c(os_vr_in_c_l_p) = en_c_n
      end if
      if (lv_sn_n .gt. en_sn_n) then
c
c     The leaving column is in a later section than the that of the
c     entering variable.
c
         do 10, sn_n = lv_sn_n-1, en_sn_n, -1
c              ___
c             |   |
c     |      |    V       |
c     |      |            |
c     | sn_n |->  sn_n+1  |
c     |      |            |
c     |      |    |       |
c                 V
c                 lv_c_n
c
c     The pointer to the end of this section is increased by one.
c
            m_sn_n = -sn_n
            en_c_n = vr_in_c(m_sn_n) + 1
            vr_in_c(m_sn_n) = en_c_n
c
c     If the current leaving column is at the end of this section then
c     no indices need to be moved. This happens if the leaving column
c     was the first in section sn_n+1 and is most likely to occur if
c     section sn_n+1 is empty.
c
            if (lv_c_n .eq. en_c_n) go to 10
c
c     The variable which has entered this section as a result is moved
c     to the current leaving column position which is then updated to be
c     the last column in this section.
c
            vr_n = vr_in_c(en_c_n)
            vr_in_c(lv_c_n) = vr_n
            st(vr_n) = st(vr_n) - en_c_n + lv_c_n
            lv_c_n = en_c_n
 10      continue
      else if (lv_sn_n .lt. en_sn_n) then
c
c     The leaving column is in an earlier section than that of the
c     entering variable.
c
         do 20, sn_n = lv_sn_n, en_sn_n-1
c
c              ___
c             |   |
c     |       V    |
c     |            |
c     |  sn_n    <-|
c     |            |
c     |       |    |
c             V
c        lv_c_n
c
c
c     The pointer to the end of this section is decreased by one.
c
            m_sn_n = -sn_n
            en_c_n = vr_in_c(m_sn_n)
            vr_in_c(m_sn_n) = en_c_n - 1
c
c     If the current leaving column was at the end of this section then
c     no indices need to be moved. This is most likely to occur if
c     section sn_n is empty.
c
            if (lv_c_n .eq. en_c_n) go to 20
c
c     Move the column at the end of this section to the position of the
c     column leaving this section.
c
            vr_n = vr_in_c(en_c_n)
            vr_in_c(lv_c_n) = vr_n
            st(vr_n) = st(vr_n) - en_c_n + lv_c_n
c
c     Set lv_c_n to the current pointer to the end of this section and
c     then reduce this pointer to make it look as if a variable
c     has been removed from (the start of) the next section.
c
            lv_c_n = en_c_n
 20      continue
      end if
c
c     lv_c_n is the first column of the section of the entering variable
c     unless the entering/leaving sections are the same, in which case
c     it is just any column in the section (not that this matters).
c
      vr_in_c(lv_c_n) = en_vr_n
      st(en_vr_n) =
     &    st(en_vr_n) - iand(st(en_vr_n), mx_mx_ml_a_dim) + lv_c_n
      if (usr_en_vr_n .gt. 0) then
         if (usr_lv_c_n .gt. 0) usr_lv_c_n = lv_c_n
c
c     If the variable to leave the basis is structural and being priced
c     then update the row representation of the matrix.
c
         if (usr_en_vr_n .le. n_c .and.
     &        lv_c_n .le. vr_in_c(os_struc_in_c_l_pc_p)) then
            n_pc_vr = n_pc_vr + 1
            n_pc_el = n_pc_el +
     &           (is(p_mtx_c_sa+en_vr_n+1) - is(p_mtx_c_sa+en_vr_n))
            if (iand(ml_da_st_msk, ml_da_st_r_mtx) .ne. 0) then
               call ems_u_ml_r_mtx(0, en_vr_n,
     &              is(p_mtx_r_ix), is(p_mtx_c_sa), ds(p_mtx_c_v),
     &              is(p_mtx_c_ix), is(p_mtx_r_sa))
               if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
            endif
         endif
      else if (usr_lv_c_n .gt. 0) then
         usr_lv_c_n = 0
      end if
CM      IF (emsol_tt .EQ. 1) THEN
C?      if (ems_tt_u_lvl1) call ems_tt_rec(-u_vr_in_c_tt, n_bs)
CM      ENDIF
      if (iand(ck_msk, st_ck_bt) .ne. 0) then
         call ems_ck_vr_in_c(is(p_vr_in_c), is(p_st), ds)
         if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
      endif
      if (iand(wr_lp_da, wr_vr_in_c_bt) .ne. 0) then
         rp_mode = 0
         if (n_c .le. 100) rp_mode = 1
         call ems_rp_vr_in_c(rp_mode, vr_in_c, st, ds(p_du_act))
      endif
 7000 continue
      return
 8001 continue
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9810)
      call ems_msg_wr_li(bug_msg_n)
CM      IF (emsol_deb .EQ. 1) THEN
C?      call ems_dump
CM      ENDIF
      go to 7000
 9810 format('No room to add variable to vr_in_c')
 9900 format('usr_lv_c_n .gt. vr_in_c(os_vr_in_c_l_p)')
      end
 
C->>> ---------------------------------------------> ems_fx_te_fx_vr <<<
c     Fix permanently any variable fixed temporarily which is not SOS.
c
      subroutine ems_fx_te_fx_vr(vr_in_c, st, ds, is)
      implicit none
      include 'EMSV.INC'
      include 'EMSMMGR.INC'
      include 'EMSPM.INC'
      include 'EMSMEM.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      include 'RSMICOM.INC'
      include 'EMSP.INC'
      integer vr_in_c(-vr_in_c_n_sn:n_c)
      integer st(0:mx_n_c+n_r)
      double precision ds(0:ds_n_en_m1)
      integer is(0:is_n_en_m1)
      integer c_n, fm_c_n, t_c_n, l_c_n, vr_n, l_vr_n
c      integer sv_lp_ph
 
c      call ems_rp_vr_in_c(1, vr_in_c, st, ds(p_du_act))
      fm_c_n = vr_in_c(os_lg_in_c_dn_p) + 1
      t_c_n = vr_in_c(os_lg_in_c_te_fx_p)
      l_c_n = t_c_n
      do 20, c_n = fm_c_n, t_c_n
 10      continue
         if (c_n .gt. l_c_n) goto 30
         vr_n = vr_in_c(c_n)
         if (iand(st(vr_n), sos_bt) .eq. 0) then
            l_vr_n = vr_in_c(l_c_n)
            vr_in_c(c_n) = l_vr_n
            vr_in_c(l_c_n) = vr_n
            st(vr_n) = st(vr_n) - c_n + l_c_n
            st(l_vr_n) = st(l_vr_n) + c_n - l_c_n
            l_c_n = l_c_n - 1
            goto 10
         endif
 20   continue
 30   continue
      vr_in_c(os_lg_in_c_te_fx_p) = l_c_n
      fm_c_n = vr_in_c(os_struc_in_c_dn_p) + 1
      t_c_n = vr_in_c(os_struc_in_c_te_fx_p)
      l_c_n = t_c_n
      do 120, c_n = fm_c_n, t_c_n
 110     continue
         if (c_n .gt. l_c_n) goto 130
         vr_n = vr_in_c(c_n)
         if (iand(st(vr_n), sos_bt) .eq. 0) then
            l_vr_n = vr_in_c(l_c_n)
            vr_in_c(c_n) = l_vr_n
            vr_in_c(l_c_n) = vr_n
            st(vr_n) = st(vr_n) - c_n + l_c_n
            st(l_vr_n) = st(l_vr_n) + c_n - l_c_n
            n_pc_vr = n_pc_vr - 1
            n_pc_el = n_pc_el -
     &           (is(p_mtx_c_sa+vr_n+1) - is(p_mtx_c_sa+vr_n))
            if (iand(ml_da_st_msk, ml_da_st_r_mtx) .ne. 0) then
c
c     Remove the variable from the row representation of the matrix.
c
               call ems_u_ml_r_mtx(vr_n, 0,
     &              is(p_mtx_r_ix), is(p_mtx_c_sa), ds(p_mtx_c_v),
     &              is(p_mtx_c_ix), is(p_mtx_r_sa))
               if (ems_msg_cod .ge. ems_msg_lvl_serious) goto 7000
            endif
            l_c_n = l_c_n - 1
            goto 110
         endif
 120  continue
 130  continue
      vr_in_c(os_struc_in_c_te_fx_p) = l_c_n
c      call ems_rp_vr_in_c(1, vr_in_c, st, ds(p_du_act))
c      sv_lp_ph = lp_ph
c      lp_ph = 2
c      call ems_ck_vr_in_c(is(p_vr_in_c), is(p_st), ds)
c      lp_ph = sv_lp_ph
 7000 continue
      return
      end
 
C->>> ----------------------------------------------> ems_ck_vr_in_c <<<
      subroutine ems_ck_vr_in_c(vr_in_c, st, ds)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMMGR.INC'
      include 'EMSMEM.INC'
      include 'EMSP.INC'
      include 'ICTVR.INC'
      include 'EMSMSG.INC'
      include 'EMSMSGN.INC'
      integer vr_in_c(-vr_in_c_n_sn:n_c)
      integer st(0:mx_n_c+n_r)
      double precision ds(0:ds_n_en_m1)
      integer ems_g_vr_in_c_sn_ty_o_vr
      integer c_n, sn_n, vr_n, tru_sn_ty, tru_sn_n, m_sn_n
      logical er_fd
      integer pr_pass_1
      save pr_pass_1
      data pr_pass_1/0/
 
      er_fd = .false.
      c_n = 1
      do 20, sn_n = 1, vr_in_c_n_sn
         m_sn_n = -sn_n
         do 10, c_n = c_n, vr_in_c(m_sn_n)
            vr_n = vr_in_c(c_n)
            if (sn_n .eq. lg_sn_n_os + vr_in_c_sn_ty_te_fx .or.
     &           sn_n .eq. struc_sn_n_os + vr_in_c_sn_ty_te_fx) then
               if (sn_n .eq. lg_sn_n_os + vr_in_c_sn_ty_te_fx .and.
     &              vr_n .le. mx_n_c) then
                  er_fd = .true.
                  if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)
     &                 vr_n,
     &                 ds(p_rsmi_lb+vr_n),
     &                 ds(p_pr_act+vr_n),
     &                 ds(p_rsmi_ub+vr_n)
                  call ems_msg_wr_li(bug_msg_n)
               else if (sn_n .eq. struc_sn_n_os + vr_in_c_sn_ty_te_fx
     &                 .and. vr_n .gt. mx_n_c) then
                  er_fd = .true.
                  if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9020)
     &                 vr_n,
     &                 ds(p_rsmi_lb+vr_n),
     &                 ds(p_pr_act+vr_n),
     &                 ds(p_rsmi_ub+vr_n)
                  call ems_msg_wr_li(bug_msg_n)
               endif
               goto 10
            endif
            tru_sn_ty = ems_g_vr_in_c_sn_ty_o_vr(st(vr_n))
            if (vr_n .le. n_c) then
               tru_sn_n = struc_sn_n_os + tru_sn_ty
            else
               tru_sn_n = lg_sn_n_os + tru_sn_ty
            endif
            if (sn_n .ne. tru_sn_n) then
               er_fd = .true.
               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
     &              vr_n,
     &              ds(p_rsmi_lb+vr_n),
     &              ds(p_pr_act+vr_n),
     &              ds(p_rsmi_ub+vr_n),
     &              sn_n, tru_sn_n
               call ems_msg_wr_li(bug_msg_n)
            end if
 10      continue
 20   continue
      if (er_fd) then
CM      IF (emsol_deb .EQ. 1) THEN
C?         call ems_dump
CM      ENDIF
      else if (pr_pass_1 .eq. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9900)
         call ems_msg_wr_li(rsmi_msg_n)
         pr_pass_1 = 1
      end if
      return
 9010 format('vr_n = ', i7, 3(1x, g11.4),
     &     ' is a structural fixed temporarily as a logical')
 9020 format('vr_n = ', i7, 3(1x, g11.4),
     &     ' is a logical fixed temporarily as a structural')
 9100 format('vr_n = ', i7, 3(1x, g11.4), ' in section ', i2,
     &     ' should be in section ', i2)
 9900 format('vr_in_c is correct')
      end
 
C->>> ----------------------------------------------> ems_rp_vr_in_c <<<
c     If mode = 0 then report the number of variables in each section.
c     If mode = 1 then report the variable numbers as well.
c     If mode > 1 then report attractiveness of each variable as well.
c
      subroutine ems_rp_vr_in_c(mode, vr_in_c, st, du_act)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'ICTVR.INC'
      include 'RLCTVR.INC'
      include 'EMSMSG.INC'
      include 'EMSMSGN.INC'
      include 'RSMICS.INC'
      integer mode, vr_in_c(-vr_in_c_n_sn:*)
      integer st(0:mx_n_c+n_r)
      double precision du_act(0:mx_n_c+n_r)
      logical ems_du_act_atr
      character*30 ems_st_t_ch30
      logical du_act_atr
      character*30 sn_nm(vr_in_c_n_sn)
      character*30 ch_st
      integer sn_n, r_n, c_n, t_c_n, m_sn_n, vr_n
      integer sn_n_c
      data sn_nm/
     &     'Logical above break point     ',
     &     'Logical below break point     ',
     &     'Logical below lower bound     ',
     &     'Logical above upper bound     ',
     &     'Logical between bounds/free   ',
     &     'Logical at lower bound        ',
     &     'Logical at upper bound        ',
     &     'Logical temporarily fixed     ',
     &     'Logical permanently fixed     ',
     &     'Structural above break point  ',
     &     'Structural below break point  ',
     &     'Structural below lower bound  ',
     &     'Structural above upper bound  ',
     &     'Structural between bounds/free',
     &     'Structural at lower bound     ',
     &     'Structural at upper bound     ',
     &     'Structural temporarily fixed  ',
     &     'Structural permanently fixed  '/
 
      if (mode .eq. 0) then
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
         call ems_msg_wr_li(rsmi_msg_n)
         c_n = 1
         do 10, sn_n = 1, vr_in_c_n_sn
            m_sn_n = -sn_n
            sn_n_c = vr_in_c(m_sn_n) - c_n + 1
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9010)
     &           sn_n, sn_nm(sn_n), sn_n_c
            call ems_msg_wr_li(rsmi_msg_n)
            c_n = vr_in_c(m_sn_n) + 1
 10      continue
      else if (mode .eq. 1) then
c         call ems_rp_vr_in_c_pm
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9100)
         call ems_msg_wr_li(rsmi_msg_n)
         c_n = 1
         do 120, sn_n = 1, vr_in_c_n_sn
            m_sn_n = -sn_n
            sn_n_c = vr_in_c(m_sn_n) - c_n + 1
            t_c_n = min(c_n+9, vr_in_c(m_sn_n))
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9110)
     &           sn_n, sn_nm(sn_n), sn_n_c,
     &           (vr_in_c(c_n), c_n = c_n, t_c_n)
            call ems_msg_wr_li(rsmi_msg_n)
            do 110, r_n = c_n, vr_in_c(m_sn_n), 10
               t_c_n = min(c_n+9, vr_in_c(m_sn_n))
               if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9120)
     &              (vr_in_c(c_n), c_n = c_n, t_c_n)
               call ems_msg_wr_li(rsmi_msg_n)
 110        continue
            c_n = vr_in_c(m_sn_n) + 1
 120     continue
      else
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9200)
         call ems_msg_wr_li(rsmi_msg_n)
         if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9201)
         do 205, sn_n = 1, vr_in_c_n_sn
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9202)
     &           sn_n, vr_in_c(-sn_n)
            call ems_msg_wr_li(rsmi_msg_n)
 205     continue
         c_n = 1
         do 220, sn_n = 1, vr_in_c_n_sn
            m_sn_n = -sn_n
            sn_n_c = vr_in_c(m_sn_n) - c_n + 1
            if (sn_n_c .le. 0) then
               if (sn_n_c .lt. 0) then
                  if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9299)
     &                 sn_n, sn_n_c
                  call ems_msg_wr_li(rsmi_msg_n)
               endif
               goto 220
            endif
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9210)sn_nm(sn_n)
            call ems_msg_wr_li(rsmi_msg_n)
            if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9211)
            call ems_msg_wr_li(rsmi_msg_n)
            do 210, c_n = c_n, vr_in_c(m_sn_n)
               vr_n = vr_in_c(c_n)
               ch_st = ems_st_t_ch30(st(vr_n))
               if (m_sn_n .eq. os_lg_in_c_fx_p .or.
     &              m_sn_n .eq. os_struc_in_c_fx_p) then
                  if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9220)
     &                 c_n, vr_n, ch_st
                  call ems_msg_wr_li(rsmi_msg_n)
               else
                  du_act_atr = ems_du_act_atr(st(vr_n),
     &                 du_act(vr_n), tl_du_ifs)
                  if (du_act_atr) then
                     if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9221)
     &                    c_n, vr_n, ch_st, du_act(vr_n)
                     call ems_msg_wr_li(rsmi_msg_n)
                  else
                     if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9222)
     &                    c_n, vr_n, ch_st, du_act(vr_n)
                     call ems_msg_wr_li(rsmi_msg_n)
                  endif
               endif
 210        continue
 220     continue
      end if
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9500)
     &     vr_in_c(-vr_in_c_n_sn), vr_in_c(0)
      call ems_msg_wr_li(rsmi_msg_n)
      return
 9000 format(' Section of vr_in_c       Length')
 9010 format(i2, ': ', a30, 2x, i7)
 9100 format(' Section of vr_in_c       Length  Variables in section')
 9110 format(i2, ': ', a30, 2x, i7, 1x, 10(1x, i7))
 9120 format(44x, 10(1x, i7))
 9200 format(' Section of vr_in_c')
 9201 format('Section   Pointer')
 9202 format(5x, i2, 2x, i7)
 9210 format(a30)
 9211 format(' Ix  Variable  Status                     Dual Act')
 9220 format(i3, 3x, i7, 2x, a30)
 9221 format(i3, 3x, i7, 2x, a30, 4x, g11.4, 2x, '  Attractive')
 9222 format(i3, 3x, i7, 2x, a30, 4x, g11.4, 2x, 'Unattractive')
 9299 format('STRANGE: Section ', i2, ' has ', i9, ' entries')
 9500 format(' vr_in_c uses ', i7, ' of the ',
     &     i7, ' entries for which it was dimensioned')
      end
 
      subroutine ems_rp_vr_in_c_pm
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'EMSMSG.INC'
 
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'vr_in_c_sn_ty_ab_bp  ', vr_in_c_sn_ty_ab_bp
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'vr_in_c_sn_ty_bw_bp  ', vr_in_c_sn_ty_bw_bp
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'vr_in_c_sn_ty_bw_lb  ', vr_in_c_sn_ty_bw_lb
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'vr_in_c_sn_ty_ab_ub  ', vr_in_c_sn_ty_ab_ub
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'vr_in_c_sn_ty_btw    ', vr_in_c_sn_ty_btw
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'vr_in_c_sn_ty_at_lb  ', vr_in_c_sn_ty_at_lb
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'vr_in_c_sn_ty_at_ub  ', vr_in_c_sn_ty_at_ub
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'vr_in_c_sn_ty_te_fx  ', vr_in_c_sn_ty_te_fx
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'vr_in_c_sn_ty_fx     ', vr_in_c_sn_ty_fx
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'vr_in_c_n_sn_ty      ', vr_in_c_n_sn_ty
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'lg_sn_n_os           ', lg_sn_n_os
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'struc_sn_n_os        ', struc_sn_n_os
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'os_vr_in_c_mx_n_c    ', os_vr_in_c_mx_n_c
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'os_lg_in_c_f_p       ', os_lg_in_c_f_p
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'os_lg_in_c_ab_bp_p   ', os_lg_in_c_ab_bp_p
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'os_lg_in_c_bw_bp_p   ', os_lg_in_c_bw_bp_p
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'os_lg_in_c_bw_lb_p   ', os_lg_in_c_bw_lb_p
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'os_lg_in_c_ab_ub_p   ', os_lg_in_c_ab_ub_p
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'os_lg_in_c_btw_p     ', os_lg_in_c_btw_p
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'os_lg_in_c_up_dn_p   ', os_lg_in_c_up_dn_p
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'os_lg_in_c_at_lb_p   ', os_lg_in_c_at_lb_p
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'os_lg_in_c_up_p      ', os_lg_in_c_up_p
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'os_lg_in_c_at_ub_p   ', os_lg_in_c_at_ub_p
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'os_lg_in_c_dn_p      ', os_lg_in_c_dn_p
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'os_lg_in_c_te_fx_p   ', os_lg_in_c_te_fx_p
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'os_lg_in_c_l_pc_p    ', os_lg_in_c_l_pc_p
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'os_lg_in_c_fx_p      ', os_lg_in_c_fx_p
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'os_lg_in_c_l_p       ', os_lg_in_c_l_p
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'os_struc_in_c_f_p    ', os_struc_in_c_f_p
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'os_struc_in_c_f_p_m1 ', os_struc_in_c_f_p_m1
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'os_struc_in_c_bw_bp_p', os_struc_in_c_bw_bp_p
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'os_struc_in_c_ab_bp_p', os_struc_in_c_ab_bp_p
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'os_struc_in_c_bw_lb_p', os_struc_in_c_bw_lb_p
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'os_struc_in_c_ab_ub_p', os_struc_in_c_ab_ub_p
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'os_struc_in_c_btw_p  ', os_struc_in_c_btw_p
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'os_struc_in_c_up_dn_p', os_struc_in_c_up_dn_p
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'os_struc_in_c_at_lb_p', os_struc_in_c_at_lb_p
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'os_struc_in_c_up_p   ', os_struc_in_c_up_p
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'os_struc_in_c_at_ub_p', os_struc_in_c_at_ub_p
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'os_struc_in_c_dn_p   ', os_struc_in_c_dn_p
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'os_struc_in_c_te_fx_p', os_struc_in_c_te_fx_p
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'os_struc_in_c_l_pc_p ', os_struc_in_c_l_pc_p
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'os_struc_in_c_fx_p   ', os_struc_in_c_fx_p
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'os_struc_in_c_l_p    ', os_struc_in_c_l_p
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'os_vr_in_c_l_p       ', os_vr_in_c_l_p
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'vr_in_c_n_sn         ', vr_in_c_n_sn
      call ems_msg_wr_li(info_msg_n)
      if (ems_msg_no_prt_fm .ge. 1) write(ems_li, 9000)
     &     'vr_in_c_os_ze_en     ', vr_in_c_os_ze_en
      call ems_msg_wr_li(info_msg_n)
      return
 9000 format(a21, ' = ', i3)
      end
 
C->>> -----------------------------------------------> ems_st_t_ch30 <<<
c     Decode the status into a character string.
c
      character*30 function ems_st_t_ch30(vr_st)
      implicit none
      include 'EMSV.INC'
      include 'EMSPM.INC'
      include 'RSMICS.INC'
      integer vr_st
      integer ch_p
      character*30 ch30
 
      ch_p = 1
      ch30 = '                              '
      if (iand(vr_st, bc_bt) .ne. 0)  ch30(ch_p:ch_p+2) = 'Bc '
      ch_p = ch_p + 3
      if (iand(vr_st, dn_bt) .ne. 0)  ch30(ch_p:ch_p+2) = 'Dn '
      ch_p = ch_p + 3
      if (iand(vr_st, up_bt) .ne. 0)  ch30(ch_p:ch_p+2) = 'Up '
      ch_p = ch_p + 3
      if (iand(vr_st, ifs_bt) .ne. 0) ch30(ch_p:ch_p+2) = 'Ifs'
      ch_p = ch_p + 3
      if (iand(vr_st, lb_bt) .ne. 0)  ch30(ch_p:ch_p+2) = 'Lb '
      ch_p = ch_p + 3
      if (iand(vr_st, ub_bt) .ne. 0)  ch30(ch_p:ch_p+2) = 'Ub '
      ch_p = ch_p + 3
      if (iand(vr_st, alt_bt) .ne. 0) then
         ch30(ch_p:ch_p+2) = 'Alt'
         ch_p = ch_p + 3
         if (iand(vr_st, bp_bt) .ne. 0) then
            ch30(ch_p:ch_p+2) = 'Bp '
         else
            ch30(ch_p:ch_p+2) = 'Pwl'
         endif
         ch_p = ch_p + 3
      else
         ch30(ch_p:ch_p+2) = 'Std'
         ch_p = ch_p + 3
         ch_p = ch_p + 3
      endif
      if (iand(vr_st, sos_bt) .ne. 0) ch30(ch_p:ch_p+2) = 'Sos'
      ems_st_t_ch30 = ch30
      return
      end
