000100011003     H DECEDIT('0,') DATEDIT(*DMY.)
000200130606     FTITAS02l  uf   E           k DISK
000300130606     ffnblp01l  if   e           k disk
000400011003     C*------------------------------------------------------------------------*
000500091117     C     KARB          KLIST
000600091117     C                   KFLD                    tasaas
000700091117     C                   KFLD                    taslnp
000800091117     C                   KFLD                    tasnrs
000900091117     C                   KFLD                    tasnsp
001000011003     C*------------------------------------------------------------------------*
001100011003     C*
001200130606     c                   z-add     0435730       ksc               7 0
001300130606     C     ksc           SETLL     TITAS02l
001400011003     C                   DO        *HIVAL
001500091117     c                   setoff                                       0102
001600130606     C     ksc           READe     TITAS02l
001700130606     c                   if        %eof(titas02l)
001800091117     C                   LEAVE
001900091117     c                   end
002000011003     C*
002100130606     c                   if        tasrmn>500000
002200130606     c     karb          chain     fnblp01l
002300130606     c                   if        %found(fnblp01l)
002400130606     c                   eval      tasmgs=blpmgs
002500130606     c                   update    titas000
002600130606     c                   endif
002700130606     c                   endif
002800011003     C*
002900011003     C                   ENDDO
003000011003     C                   SETON                                        LR
