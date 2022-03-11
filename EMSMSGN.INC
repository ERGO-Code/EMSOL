CM
      integer bs_info_msg_n
      integer bs_warn_msg_n
      integer bs_er_msg_n
      integer bs_serious_msg_n
      integer bs_pvm_er_msg_n
      parameter (
     &     bs_info_msg_n =    0,
     &     bs_warn_msg_n =    3000,
     &     bs_er_msg_n =      6000,
     &     bs_serious_msg_n = 7000,
     &     bs_pvm_er_msg_n = 8500)
 
      integer mem_mgr_msg_n
      integer inv_msg_n
      integer rsmi_msg_n
      integer pvm_msg_n
      parameter (
     &     mem_mgr_msg_n = bs_info_msg_n + 1,
     &     inv_msg_n = mem_mgr_msg_n + 1,
     &     rsmi_msg_n = inv_msg_n + 1,
     &     pvm_msg_n = bs_info_msg_n + 10)
 
 
