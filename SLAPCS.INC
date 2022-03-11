CM
      integer eta_fi_p_a_os_p_eta_grp
      integer eta_fi_p_a_os_p_eta_v
      integer eta_fi_p_a_os_p_eta_ix
      integer eta_fi_p_a_os_p_eta_rec
      integer eta_fi_p_a_rec_z
      parameter (
     &     eta_fi_p_a_os_p_eta_grp = 0,
     &     eta_fi_p_a_os_p_eta_v = 1,
     &     eta_fi_p_a_os_p_eta_ix = 2,
     &     eta_fi_p_a_os_p_eta_rec = 3,
     &     eta_fi_p_a_rec_z = eta_fi_p_a_os_p_eta_rec + 1)
c     cp_o_eta_fi_p_a_rec_z in EMSP.INC is identical to
c     eta_fi_p_a_rec_z and is  made to avoid having to include
c     SLAPCS.INC in EMSP.INC just to get this value.
 
      integer rl_si_da_os_du_act
      integer rl_si_da_os_aa
      integer rl_si_da_os_tl_pr_ifs
      integer rl_si_da_rec_z
      parameter (
     &   rl_si_da_os_du_act = 0,
     &   rl_si_da_os_aa = 1,
     &   rl_si_da_os_tl_pr_ifs = 2,
     &   rl_si_da_rec_z = rl_si_da_os_tl_pr_ifs + 1)
 
      integer bs_cg_os_vr_t_en_bs
      integer bs_cg_os_vr_t_lv_bs
      integer bs_cg_rec_z
      parameter (
     &   bs_cg_os_vr_t_en_bs = 0,
     &   bs_cg_os_vr_t_lv_bs = 1,
     &   bs_cg_rec_z = bs_cg_os_vr_t_lv_bs + 1)
 
c      integer i_si_da_os_si_it_n
c      integer i_si_da_os_fm_prcs_n
c      integer i_si_da_os_vr_t_en_bs
c      integer i_si_da_os_vr_t_lv_bs
c      integer i_si_da_os_n_el
c      integer i_si_da_os_n_cdd_r
c      integer i_si_da_rec_z
c      parameter (
c     &   i_si_da_os_si_it_n    = 0,
c     &   i_si_da_os_fm_prcs_n  = i_si_da_os_si_it_n + 1,
c     &   i_si_da_os_vr_t_en_bs = i_si_da_os_fm_prcs_n + 1,
c     &   i_si_da_os_vr_t_lv_bs = i_si_da_os_vr_t_en_bs + 1,
c     &   i_si_da_os_n_el       = i_si_da_os_vr_t_lv_bs + 1,
c     &   i_si_da_os_n_cdd_r    = i_si_da_os_n_el + 1,
c     &   i_si_da_rec_z         = i_si_da_os_n_cdd_r + 1)
 
      integer eta_grp_os_eta_grp_ty
      integer eta_grp_os_sto_eta_v
      integer eta_grp_os_mx_n_el
      integer eta_grp_os_mx_n_eta
      integer eta_grp_os_n_eta
      integer eta_grp_os_f_si_it_n
      integer eta_grp_os_n_v
      integer eta_grp_os_mx_n_v
      integer eta_grp_os_n_ix
      integer eta_grp_os_mx_n_ix
      integer eta_grp_os_n_rec
      integer eta_grp_os_mx_n_rec
      integer eta_grp_os_hdl_v
      integer eta_grp_os_hdl_ix
      integer eta_grp_os_hdl_rec
c      integer eta_grp_os_hdl_bs_da
c      integer eta_grp_os_hdl_cdd_r_ix
c      integer eta_grp_os_hdl_rl_si_da
c      integer eta_grp_os_hdl_i_si_da
      parameter (
     &     eta_grp_os_eta_grp_ty =   0,
     &     eta_grp_os_sto_eta_v =    eta_grp_os_eta_grp_ty +        1,
     &     eta_grp_os_mx_n_eta =     eta_grp_os_sto_eta_v +         1,
     &     eta_grp_os_mx_n_el =      eta_grp_os_mx_n_eta +          1,
     &     eta_grp_os_f_si_it_n =    eta_grp_os_mx_n_el +           1,
     &     eta_grp_os_n_eta =        eta_grp_os_f_si_it_n +         1,
     &     eta_grp_os_n_v =          eta_grp_os_n_eta +             1,
     &     eta_grp_os_mx_n_v =       eta_grp_os_n_v +               1,
     &     eta_grp_os_n_ix =         eta_grp_os_mx_n_v +            1,
     &     eta_grp_os_mx_n_ix =      eta_grp_os_n_ix +              1,
     &     eta_grp_os_n_rec =        eta_grp_os_mx_n_ix +           1,
     &     eta_grp_os_mx_n_rec =     eta_grp_os_n_rec +             1,
     &     eta_grp_os_hdl_v =        eta_grp_os_mx_n_rec +          1,
     &     eta_grp_os_hdl_ix =       eta_grp_os_hdl_v +         hdl_z,
     &     eta_grp_os_hdl_rec =      eta_grp_os_hdl_ix +        hdl_z)
c     &     eta_grp_os_hdl_bs_da =    eta_grp_os_hdl_rec +       hdl_z,
c     &     eta_grp_os_hdl_cdd_r_ix = eta_grp_os_hdl_bs_da +     hdl_z,
c     &     eta_grp_os_hdl_rl_si_da = eta_grp_os_hdl_cdd_r_ix +  hdl_z,
c     &     eta_grp_os_hdl_i_si_da =  eta_grp_os_hdl_rl_si_da +  hdl_z)
 
      integer rsmi_eta_grp_ty
c      integer parlp_eta_grp_ty
      integer n_eta_grp_ty
      parameter (
     &     rsmi_eta_grp_ty =  1,
c     &     parlp_eta_grp_ty = rsmi_eta_grp_ty + 1,
     &     n_eta_grp_ty =     rsmi_eta_grp_ty)
 
      integer rsmi_eta_grp_rec_z
      integer parlp_eta_grp_rec_z
      integer mx_eta_grp_rec_z
      parameter (
     &     rsmi_eta_grp_rec_z =  ((eta_grp_os_hdl_v + 3*hdl_z+1)/2)*2,
     &     parlp_eta_grp_rec_z = ((eta_grp_os_hdl_v + 7*hdl_z+1)/2)*2,
     &     mx_eta_grp_rec_z =    parlp_eta_grp_rec_z)
 
