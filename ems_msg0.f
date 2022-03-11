CM
C->>> -----------------------------------------------> ems_ln_t_l_ch <<<
c     Finds the length of the string up to the last alpha-numeric
c     non-blank character. NB Returns zero for strings of blanks.
c     Used for printing strings.
c
      integer function ems_ln_t_l_ch(li)
      implicit none
      character*(*) li
      integer li_n_ch
      integer p, ln
 
      li_n_ch = len(li)
      ln = 0
      do 10, p = 1, li_n_ch
         if (ichar(li(p:p)) .lt. 32 .or. ichar(li(p:p)) .gt. 126) then
c            write(*, 9999)li, p, li(p:p), ichar(li(p:p))
c 9999       format('|', a, '|', i2, 2z, '|', a1, '|', i3)
c
c     The character is not something which should be printed.
c
            ln = p-1
            go to 1000
         endif
         if (ichar(li(p:p)) .ne. 32) ln = p
 10   continue
 1000 continue
      ems_ln_t_l_ch = ln
      return
      end
 
