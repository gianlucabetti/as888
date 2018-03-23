000100050118     H DECEDIT('0,') DATEDIT(*YMD.)
000200021214      *
000300100111     Ftabel00f  iF   E           K DISK
000400141108     FFNarb00F  UF   E             DISK
000500141106     FFNarb25L  iF   E           K DISK
000600141108     F                                     INFDS(FNarb1)
000700141108     F                                     RENAME(FNarb000:FNarb025)
000800091125     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000900141106     FQSYSPRT2  O    F  132        PRINTER OFLIND(*INOe)
001000100111
001100100111     dkpjba          e ds
001200100111     dtnsd99ds       e ds
001300141108     D fnarb1          DS
001400141108     D  arbNRR               397    400B 0
001500021214      *--------------------------------------------------
001600100111     C     *ENTRY        PLIST
001700100111     C                   PARM                    kpjba
001800100111     c
001900141108     C                   movel     kpjbu         aggio             1
002000141108     c                   z-add     001           kfil              3 0
002100141106     c
002200100111     c     karb          klist
002300141106     c                   kfld                    kfil              3 0
002400141106     c                   kfld                    kdcm              8 0
002500141107     c                   seton                                        of
002600100111     C
002700141111     c                   eval      kdcm=20141101
002800100111     c
002900141106    0c                   dow       kfil>0
003000141106     c
003100141107     c                   if        *inof
003200100111     c                   except    testata
003300141107     c                   setoff                                       of
003400141107     c                   endif
003500100111
003600141106     c     karb          setll     fnarb25l
003700141106     c     kfil          reade     fnarb25l
003800051110
003900141106    1c                   DOW       not %EOF(FNarb25l)
004000141106    2c                   if        arbdcm>0 and arbnci>0 and
004100141106     c                             (arbgc1<>'  'or arbgc2<>'  ')
004200141106     c                   add       1             totale            9 0
004300100111     c* elaboora bolla
004400100111     c                   exsr      Elabolla
004500141106    2c                   endif
004600091125     c
004700141106     c     kfil          reade     fnarb25l
004800141106    1c                   enddo
004900021216      *
005000100111     c                   except    fine
005100141106     c                   clear                   totale
005200141106     c                   clear                   totagg
005300141106     c     kfil          setgt     fnarb25l
005400141106     c                   read      fnarb25l
005500141106    1c                   if        not %eof(fnarb25l)
005600141106     c                   eval      kfil=arblna
005700141107     c                   seton                                        of
005800141106     c                   else
005900141106     c                   clear                   kfil
006000141106    1c                   endif
006100141106    0c                   enddo
006200100111      *
006300021214     C                   eval       *inlr = *on
006400100111     c*----------------------------------------------------------
006500100111     c     Elabolla      BEGSR
006600100111     c*  Col  tnsd99 verifico che data cons richeista mettere
006700100111     c                   clear                   tnsd99ds
006800100111     c                   eval      d98tbo='A'
006900100111     c                   eval      d98aas=arbaas
007000100111     c                   eval      d98lnp=arblnp
007100100111     c                   eval      d98nrs=arbnrs
007200100111     c                   eval      d98nsp=arbnsp
007300100111     C                   CALL      'TNSD99R'
007400100111     C                   PARM                    tnsd99ds
007500141106     c* se ora non è più in ritardo stampo
007600141106     c                   if        d98nci<=0
007700141106     c                   add       1             totagg            9 0
007800141106     c                   except    bolla
007900141108     c                   if        aggio='S'
008000141108     c     arbnrr        chain(E)  fnarb00f
008100141108     c                   if        %error
008200141108     c                   else
008300141108     c                   if        %found
008400141108     c                   eval      arbfbs='S'
008500141108     c                   eval      arbdti=0
008600141108     c                   eval      arbhti=0
008700141108     c                   except    abolla
008800141106     c                   endif
008900141108     c                   endif
009000141108     c                   endif
009100141108     c                   endif
009200100111     c
009300100111     c                   ENDSR
009400100111     c
009500141108     Ofnarb000  E            abolla
009600141108     O                       ARBFBS
009700141108     O                       ARBDTI
009800141108     O                       ARBHTI
009900100111     OQSYSPRT   E            TESTATA     1  1
010000141106     O                                              'AGGIORNAMENTO bolle in r'
010100141106     O                                        +   0 'itardo per GC1 GC2'
010200100111     OQSYSPRT   E            bolla       1
010300141108     O
010400100111     O                       arbaas           +   1
010500100111     O                       arblnp           +   1
010600100111     O                       arbnrs           +   1
010700100111     O                       arbnsp        2  +   1
010800100111     O                       arblna           +   1
010900141106     O                       arbgc1           +   1
011000141106     O                       arbgc2           +   1
011100141106     O                       arbdcm           +   1 '    /  /  '
011200141106     OQSYSPRT2  E            FINE        2
011300141106     O                                              'Fil:'
011400141106     O                       kfil          2  +   2
011500141108     O                                        +   2 'Totale bolle:'
011600141106     O                       totale        2  +   2
011700141108     O                                        +   2 'Totale bolle aggiornate:'
011800141106     O                       totagg        2  +   2
