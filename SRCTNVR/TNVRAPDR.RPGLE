000100980323     H DECEDIT('0,') DATEDIT(*DMY.)
000200000809      *------------------------------------------------------------------------*
000300021202      * trascodifica fiapd00f da fnapd00f                                      *
000400000809      *------------------------------------------------------------------------*
000500021202     Ffnapd01l  iF   E           K DISK
000501021202     Ffiapd00f  o    E           K DISK
000502021202     ddznp           e ds                  prefix(£)
000503021202     dapdznpds       e ds
000600000809      *
000612020829     c*
000700021202     C     *loval        setll     fnapd01l
000701021202     c                   do        *hival
000702021202     C                   read      fnapd01l
000703021202     c                   if        %eof(fnapd01l)
000704021202     c                   leave
000705021202     c                   end
000706021202     c                   clear                   apdhar
000707021202     c                   clear                   apdhpa
000708021202     c                   eval      apdtip = 'A'
000709021202     c                   eval      apdcam = 'S'
000710021202     c                   eval      apdcap = 'S'
000711021202     c                   eval      apdscm = 'S'
000712021202     c                   eval      apdscp = 'S'
000713021202     c                   eval      apdrsf = apdrsc
000714021202     c                   movel(p)  apdznp        dznp
000715021202     c                   eval      §znpz1 = £§znpz1
000716021202     c                   eval      §znpz2 = £§znpz2
000717021202     c                   eval      §znpz3 = £§znpz3
000718021202     c                   eval      §znpz4 = £§znpz4
000719021202     c                   eval      §znpz5 = £§znpz5
000720021202     c                   movel(p)  apdznpds      apdznp
000721021202     c                   movel     £§znptp       apdptp
000722021202     c                   move      £§znpdt       apddtr
000723021202     c                   eval      apdftr = 'T'
001200021202     C                   write     fiapd000
001300021202     C                   ENDdo
002200020829      *
002300990108     C                   SETON                                        LR
