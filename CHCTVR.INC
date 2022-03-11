CM
      include 'CHCTVRIX.INC'
      character*8 ch_ml_nm, ch_ob_nm, ch_rhs_nm
      character*8 ch_rg_nm, ch_bd_nm, ch_bs_nm
      character*8 ch_ct_vr(n_ch_ct_vr)
      common/ems_com_ch_ct_vr/ch_ct_vr
      equivalence (ch_ml_nm,            ch_ct_vr(ix_ch_ml_nm))
      equivalence (ch_ob_nm,            ch_ct_vr(ix_ch_ob_nm))
      equivalence (ch_rhs_nm,           ch_ct_vr(ix_ch_rhs_nm))
      equivalence (ch_rg_nm,            ch_ct_vr(ix_ch_rg_nm))
      equivalence (ch_bd_nm,            ch_ct_vr(ix_ch_bd_nm))
      equivalence (ch_bs_nm,            ch_ct_vr(ix_ch_bs_nm))
