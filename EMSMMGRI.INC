CM
      integer blk_at_mgr_frt
      integer blk_at_mgr_bac
      parameter (
     &     blk_at_mgr_frt = ope_blk_at_f_fr_p,
     &     blk_at_mgr_bac = ope_blk_at_l_fr_p)
      integer blk_da_blk_id
      integer a_blk_id
      integer no_blk_id
      integer usr_blk_da_blk_id
      integer usr_blk_id_os
      integer usr_a_blk_id
      integer usr_no_blk_id
      integer mx_n_blk_id
      integer blk_ix_fac
      parameter (
     &     blk_da_blk_id =     0,
     &     a_blk_id =          1,
     &     no_blk_id =         2,
     &     usr_blk_id_os =     2,
     &     usr_blk_da_blk_id = blk_da_blk_id - usr_blk_id_os,
     &     usr_a_blk_id =      a_blk_id - usr_blk_id_os,
     &     usr_no_blk_id =     no_blk_id - usr_blk_id_os,
     &     mx_n_blk_id =       99,
     &     blk_ix_fac = mx_n_blk_id+1)
 
      integer n_blk_id
      common/ems_mem_mgr_n_blk_id_com/n_blk_id
 
      integer blk_id_txt_n_ch
      parameter (blk_id_txt_n_ch = 32)
      character*(blk_id_txt_n_ch) blk_id_txt(0:mx_n_blk_id)
      common/ems_mem_mgr_blk_id_txt_com/blk_id_txt
 
      integer os_blk_is_p, os_blk_mgr_p, os_blk_n_wo, os_blk_f_fr_p
      integer os_blk_lkf, os_blk_lkb, os_blk_id
      parameter (
     &     os_blk_is_p = 0, os_blk_mgr_p =    1,
     &     os_blk_n_wo = 2, os_blk_f_fr_p =   3,
     &     os_blk_lkf =  4, os_blk_lkb =      5,
     &     os_blk_id =   6)
      integer blk_nm_n_i_wo
      integer og_mx_n_blk, blk_rec_z
      integer mx_n_blk_n_fr_ls_en
      parameter (
     &     blk_nm_n_i_wo = 4,
     &     og_mx_n_blk = 20,
     &     blk_rec_z = 7,
     &     mx_n_blk_n_fr_ls_en = 10)
 
      integer blk_n_fr_ls_rp_li_n_en
      parameter (blk_n_fr_ls_rp_li_n_en = 10)
 
      integer ix_ca_mem_mgr_iz_fg1
      integer ix_ca_mem_mgr_iz_fg2
      integer ix_blk_mv_lck
      integer ix_n_fr_wo
      integer ix_p_blk_da
      integer ix_f_mgr_p
      integer ix_f_blk_p
      integer ix_f_fr_p
      integer ix_l_fr_p
      integer ix_l_blk_p
      integer ix_l_mgr_p
      integer ix_f_frt_blk
      integer ix_l_frt_blk
      integer ix_f_bac_blk
      integer ix_l_bac_blk
      integer ix_blk_blk_da
      integer ix_mem_mgr_ck_rp_mode
      integer ix_mx_n_blk
      integer ix_mx_blk_n
      integer ix_n_fr_blk_n
      integer ix_blk_n_fr_ls_n_en
      integer p_blk_n_fr_ls
      integer ix_mn_n_fr_wo
      integer ix_n_xa_i_wo_rq
      integer mem_mgr_da_l_is_en
c
c     parameter (ix_blk_mv_k =            0) ! Set in EMSMMGR
      parameter (ix_ca_mem_mgr_iz_fg1 =   ix_blk_mv_k + 1)
      parameter (ix_ca_mem_mgr_iz_fg2 =   ix_ca_mem_mgr_iz_fg1 + 1)
      parameter (ix_blk_mv_lck =          ix_ca_mem_mgr_iz_fg2 + 1)
      parameter (ix_n_fr_wo =             ix_blk_mv_lck + 1)
      parameter (ix_p_blk_da =            ix_n_fr_wo + 1)
      parameter (ix_f_mgr_p =             ix_p_blk_da + 1)
      parameter (ix_f_blk_p =             ix_f_mgr_p + 1)
      parameter (ix_f_fr_p =              ix_f_blk_p + 1)
      parameter (ix_l_fr_p =              ix_f_fr_p + 1)
      parameter (ix_l_blk_p =             ix_l_fr_p + 1)
      parameter (ix_l_mgr_p =             ix_l_blk_p + 1)
      parameter (ix_f_frt_blk =           ix_l_mgr_p + 1)
      parameter (ix_l_frt_blk =           ix_f_frt_blk + 1)
      parameter (ix_f_bac_blk =           ix_l_frt_blk + 1)
      parameter (ix_l_bac_blk =           ix_f_bac_blk + 1)
      parameter (ix_blk_blk_da =          ix_l_bac_blk + 1)
      parameter (ix_mem_mgr_ck_rp_mode =  ix_blk_blk_da + 1)
      parameter (ix_mx_n_blk =            ix_mem_mgr_ck_rp_mode + 1)
      parameter (ix_mx_blk_n =            ix_mx_n_blk + 1)
      parameter (ix_n_fr_blk_n =          ix_mx_blk_n + 1)
      parameter (ix_blk_n_fr_ls_n_en =    ix_n_fr_blk_n + 1)
      parameter (p_blk_n_fr_ls =          ix_blk_n_fr_ls_n_en)
      parameter (ix_mn_n_fr_wo =          p_blk_n_fr_ls +
     &     1 + mx_n_blk_n_fr_ls_en)
      parameter (ix_n_xa_i_wo_rq =        ix_mn_n_fr_wo + 1)
      parameter (mem_mgr_da_l_is_en =     ix_n_xa_i_wo_rq)
c
c     16/11/95: n_mem_mgr_da = 32
c     21/05/97: n_mem_mgr_da = 31
c     07/07/97: n_mem_mgr_da = 32
c     06/06/98: n_mem_mgr_da = 33
c     15/06/98: mem_mgr_da_l_is_en = 33 (Renaming of n_mem_mgr_da)
c
c     NB:
c     cp_o_mem_mgr_da_l_is_en in EMSMEM.INC should be identical to
c     mem_mgr_da_l_is_en and is made to avoid having to include
c     EMSMMGRI.INC in EMSMEM.INC just to get this value.
c
