      integer mx_n_sv_bs_cg
      parameter (mx_n_sv_bs_cg = 100)
 
      integer mx_n_sv_bs_cg_seq
      parameter (mx_n_sv_bs_cg_seq = 10)
 
      integer ca_sv_bs_cg_iz_fg1, ca_sv_bs_cg_iz_fg2
      integer n_sv_bs_cg, n_sv_bs_cg_seq
      integer sv_bs_cg_seq_sa(mx_n_sv_bs_cg_seq+1)
      integer sv_bs_cg_en_vr(mx_n_sv_bs_cg)
      integer sv_bs_cg_en_vr_prev_st(mx_n_sv_bs_cg)
      integer sv_bs_cg_lv_vr(mx_n_sv_bs_cg)
      integer sv_bs_cg_lv_vr_prev_st(mx_n_sv_bs_cg)
      logical bs_cg_rec
      common/ems_sv_bs_cg_com/
     &     ca_sv_bs_cg_iz_fg1, ca_sv_bs_cg_iz_fg2,
     &     n_sv_bs_cg, n_sv_bs_cg_seq,
     &     sv_bs_cg_seq_sa,
     &     sv_bs_cg_en_vr,
     &     sv_bs_cg_en_vr_prev_st,
     &     sv_bs_cg_lv_vr,
     &     sv_bs_cg_lv_vr_prev_st,
     &     bs_cg_rec
