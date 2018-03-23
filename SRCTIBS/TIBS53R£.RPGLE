000100971103     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200971110     F
000300971110     FINPUT     UF   F 9999        disk
000400971110     iinput     aa
000500971110     i                                  1 9999  inprec
000600971110     c**
000700971110     c                   read      input                                  31
000800971110     c     *in31         doweq     *off
000900971110     c                   delete    input
001000971110     c                   read      input                                  31
001100971110     c                   enddo
001200971110     c*
001300971110     c*
001400971110     c                   seton                                        lr
001500971104     c**
