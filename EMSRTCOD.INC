      integer ems_rt_lvl_bs
      integer ems_rt_lvl_info
      integer ems_rt_lvl_warn
      integer ems_rt_lvl_er
      integer ems_rt_lvl_serious
      parameter (
     &     ems_rt_lvl_bs =        100,
     &     ems_rt_lvl_info =    0*ems_rt_lvl_bs,
     &     ems_rt_lvl_warn =    1*ems_rt_lvl_bs,
     &     ems_rt_lvl_er =      2*ems_rt_lvl_bs,
     &     ems_rt_lvl_serious = 3*ems_rt_lvl_bs)
 
      integer ems_rt_id_ok
 
      integer ems_rt_id_prob_st_op
      integer ems_rt_id_prob_st_ifs
      integer ems_rt_id_prob_st_unbd
      integer ems_rt_id_prob_st_mx_n_it
      integer ems_rt_id_prob_st_no_sol
      integer ems_rt_id_prob_st_mx_n_sol
 
      integer ems_rt_id_no_po
      integer ems_rt_id_f7_rd_er
      integer ems_rt_id_f7_wr_er
 
      integer ems_rt_id_usr_pm_er
      integer ems_rt_id_usr_da_er
 
      integer ems_rt_id_int_er
 
      parameter (
     &     ems_rt_id_ok =               0)
 
      parameter (
     &     ems_rt_id_prob_st_op =       0,
     &     ems_rt_id_prob_st_ifs =      1,
     &     ems_rt_id_prob_st_unbd =     2,
     &     ems_rt_id_prob_st_mx_n_it =  3,
     &     ems_rt_id_prob_st_no_sol =   4,
     &     ems_rt_id_prob_st_mx_n_sol = 5)
 
      parameter (
     &     ems_rt_id_no_po =            10)
 
      parameter (
     &     ems_rt_id_f7_rd_er =         98,
     &     ems_rt_id_f7_wr_er =         99)
 
      parameter (
     &     ems_rt_id_usr_pm_er =        100,
     &     ems_rt_id_usr_da_er =        101)
 
      parameter (
     &     ems_rt_id_int_er =           110)
 
      integer ems_rt_cod_ok
      parameter (ems_rt_cod_ok = ems_rt_id_ok +
     &     ems_rt_lvl_info)
 
      integer ems_rt_cod_serious_f7_wr_er
      integer ems_rt_cod_serious_f7_rd_er
      integer ems_rt_cod_usr_pm_er
      integer ems_rt_cod_usr_da_er
      integer ems_rt_cod_int_er
      parameter (ems_rt_cod_serious_f7_wr_er = ems_rt_id_f7_wr_er +
     &     ems_rt_lvl_serious)
      parameter (ems_rt_cod_serious_f7_rd_er = ems_rt_id_f7_rd_er +
     &     ems_rt_lvl_serious)
      parameter (ems_rt_cod_usr_pm_er = ems_rt_id_usr_pm_er +
     &     ems_rt_lvl_serious)
      parameter (ems_rt_cod_usr_da_er = ems_rt_id_usr_da_er +
     &     ems_rt_lvl_serious)
      parameter (ems_rt_cod_int_er = ems_rt_id_int_er +
     &     ems_rt_lvl_serious)
