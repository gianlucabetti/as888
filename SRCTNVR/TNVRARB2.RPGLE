000100050118     H DECEDIT('0,') DATEDIT(*YMD.)
000200021214      *
000300100111     Ftabel00f  iF   E           K DISK
000400141106     FFNarb25L  iF   E           K DISK
000500091125     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000600141106     FQSYSPRT2  O    F  132        PRINTER OFLIND(*INOe)
000700100111
000800100111     dkpjba          e ds
000900100111     dtnsd99ds       e ds
001000021214      *--------------------------------------------------
001100100111     C     *ENTRY        PLIST
001200100111     C                   PARM                    kpjba
001300100111     c
001400141106     C                   movel     kpjbu         fil               3
001500141106     c                   if        fil>*zeros
001600141106     c                   movel     fil           kfil              3 0
001700141106     c                   else
001800141106     c                   z-add     001           kfil
001900100111     c                   endif
002000141106     c
002100100111     c     karb          klist
002200141106     c                   kfld                    kfil              3 0
002300141106     c                   kfld                    kdcm              8 0
002400141107     c                   seton                                        of
002500100111     C
002600141111     c                   eval      kdcm=20141101
002700100111     c
002800141106    0c                   dow       kfil>0
002900141106     c
003000141107     c                   if        *inof
003100100111     c                   except    testata
003200141107     c                   setoff                                       of
003300141107     c                   endif
003400100111
003500141106     c     karb          setll     fnarb25l
003600141106     c     kfil          reade     fnarb25l
003700051110
003800141106    1c                   DOW       not %EOF(FNarb25l)
003900141106    2c                   if        arbdcm>0 and arbnci>0 and
004000141106     c                             (arbgc1<>'  'or arbgc2<>'  ')
004100141106     c                   add       1             totale            9 0
004200100111     c* elaboora bolla
004300100111     c                   exsr      Elabolla
004400141106    2c                   endif
004500091125     c
004600141106     c     kfil          reade     fnarb25l
004700141106    1c                   enddo
004800021216      *
004900100111     c                   except    fine
005000141106     c                   clear                   totale
005100141106     c                   clear                   totagg
005200141106     c     kfil          setgt     fnarb25l
005300141106     c                   read      fnarb25l
005400141106    1c                   if        not %eof(fnarb25l)
005500141106     c                   eval      kfil=arblna
005600141107     c                   seton                                        of
005700141106     c                   else
005800141106     c                   clear                   kfil
005900141106    1c                   endif
006000141106    0c                   enddo
006100100111      *
006200021214     C                   eval       *inlr = *on
006300100111     c*----------------------------------------------------------
006400100111     c     Elabolla      BEGSR
006500100111     c*  Col  tnsd99 verifico che data cons richeista mettere
006600100111     c                   clear                   tnsd99ds
006700100111     c                   eval      d98tbo='A'
006800100111     c                   eval      d98aas=arbaas
006900100111     c                   eval      d98lnp=arblnp
007000100111     c                   eval      d98nrs=arbnrs
007100100111     c                   eval      d98nsp=arbnsp
007200100111     C                   CALL      'TNSD99R'
007300100111     C                   PARM                    tnsd99ds
007400141107     c* se  è ancora in ritardo stampo
007500141107     c                   if        d98nci> 0
007600141106     c                   add       1             totagg            9 0
007700141106     c                   except    bolla
007800141106     c                   endif
007900100111     c
008000100111     c                   ENDSR
008100100111     c
008200100111     OQSYSPRT   E            TESTATA     1  1
008300141107     O                                              'bolle ancora in ritardo '
008400141107     O                                        +   0 ' per GC1 GC2'
008500100111     OQSYSPRT   E            bolla       1
008600100111     O                                              'Bolla:'
008700100111     O                       arbaas           +   1
008800100111     O                       arblnp           +   1
008900100111     O                       arbnrs           +   1
009000100111     O                       arbnsp        2  +   1
009100100111     O                       arblna           +   1
009200141106     O                       arbgc1           +   1
009300141106     O                       arbgc2           +   1
009400141110     O                       arbdti           +   1 '    /  /  '
009500141110     O                       arbhti           +   1
009600141110     O                       arbdcm           +   1 '    /  /  '
009700141110     O                       arbhmc           +   1
009800141110     O                       arbcad           +   1
009900141110     O                       arblod           +   1
010000141106     OQSYSPRT2  E            FINE        2
010100141106     O                                              'Fil:'
010200141106     O                       kfil          2  +   2
010300141106     O                                              'Totale bolle:'
010400141106     O                       totale        2  +   2
010500141107     O                                              'Totale bolle in ritardo '
010600141106     O                       totagg        2  +   2
