      integer ds_n_en, is_n_en, ns_n_en
c      parameter (ds_n_en = 8 0 000,
c      parameter (ds_n_en = 250 000,
c      parameter (ds_n_en = 200 000 000,  ! 
c      parameter (ds_n_en = 225 000 000,  !
!      parameter (ds_n_en = 250 000 000,  ! OK with ifort -fast  
      parameter (ds_n_en = 260 000 000,  ! 
c      parameter (ds_n_en = 268 100 000,  !OK with ifort -fast (no sslv)
c      parameter (ds_n_en = 268 400 000, !SegV with ifort -fast
c      parameter (ds_n_en = 268 435 455,  !OK with ifort
c      parameter (ds_n_en = 268 435 456)  !Fails
     &   is_n_en = ds_n_en*2,
     &   ns_n_en = ds_n_en)
      double precision ds(ds_n_en)
      integer is(is_n_en)
c      character*8 ns(ns_n_en)
      equivalence (ds(1),      is(1))
c      equivalence (ds(1),      ns(1))

