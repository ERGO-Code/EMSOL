      integer tom_inv_sing_tl_ck
      integer tom_inv_sing_al_ck
      integer tom_inv_sing_tl_alw
      integer tom_inv_sing_al_alw
      parameter (
     &     tom_inv_sing_tl_ck = bt1,
     &     tom_inv_sing_al_ck = bt1+bt2,
     &     tom_inv_sing_tl_alw = bt3,
     &     tom_inv_sing_al_alw = bt3+bt4)
 
      integer tom_inv_sing_msk
      common/ems_tom_inv_sing_msk_com/tom_inv_sing_msk
 
      integer tom_inv_rpt_n
      common/ems_tom_inv_rpt_n_com/tom_inv_rpt_n
 
      integer tom_inv_no_po_bt
      integer tom_inv_rpt_bt
      integer tom_inv_er_bt
      parameter (
     &     tom_inv_no_po_bt = 1,
     &     tom_inv_rpt_bt = 2,
     &     tom_inv_er_bt = 4)
