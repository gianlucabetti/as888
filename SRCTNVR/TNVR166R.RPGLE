000100951009     H DECEDIT('0,') DATEDIT(*DMY.)
000200980625      * ---------------------------------------------------------
000300140314     fFNARB27L  iF   E           K DISK
000400140314     fFNARb01L  uF   E           K DISK    rename(fnarb000:fnarb001) prefix(A_)
000500140314     fFNblp01L  uF   E           K DISK
000600140314     fTITAS30c  uF   E           K DISK    usropn extfile(WTITAS)
000700140314     fFNART01L  IF   E           K DISK
000800140313     fFiars01L  IF   E           K DISK
000900140313     fFiars02L  IF   E           K DISK    rename(fiars000:fiars002)
001000140313     fQSYSPRT   O    F  132        PRINTER  oflind(*INOF)
001100140313     d knot            s                   like(arsnot)
001200140313     d knsc            s                   like(arsnsc)
001300140313     d klnp            s                   like(arsfls)
001400140314     d WTITAS          s             21    inz('GAITRAGRU /TITAS30C')
001500980611      * ---------------------------------------------------------
001600041222     C     KARB          KLIST
001700041222     C                   KFLD                    KLNP
001800041222     C                   KFLD                    KAAS              4 0
001900041222     C                   KFLD                    KMGS              4 0
002000140313     C     KARs1         KLIST
002100140313     C                   KFLD                    artfls
002200140313     C                   KFLD                    artlna
002300140313     C                   KFLD                    artnrs
002400140313     C                   KFLD                    artnsc
002500140313     C                   KFLD                    trc               1
002600140313     C     KARs2         KLIST
002700140313     C                   KFLD                    knot
002800140313     C                   KFLD                    trc               1
002900140313
003000140313     c                   eval      trc='C'
003100041222     C     KART          KLIST
003200041222     C                   KFLD                    ARBAAS
003300041222     C                   KFLD                    ARBLNP
003400041222     C                   KFLD                    ARBNRS
003500041222     C                   KFLD                    ARBNSP
003600041222     C
003700140314     C                   OPEN(e)   titas30c
003800140314     c
003900140314     c                   if        not %open(titas30c)
004000140314     c                   eval      %subst(WTITAS:7:4)='GRPS'
004100140314     C                   OPEN      titas30c
004200140314     c                   endif
004300140314
004400140313     C                   EVAL      KAAS=2014
004500140313     C                   EVAL      KMGS=0312
004600140313     C                   EVAL      Klnp=166
004700041222     C
004800041222     C     KARB          SETLL     FNARB27L
004900140313     C     KARB          READE     FNARB27L
005000041222     C
005100140313    1C                   DOW       NOT %EOF(FNARB27L)
005200140313     c
005300140313    2c                   if        arbksc=1662139
005400041222     C* LETTO FNART
005500041222     C     KART          SETLL     FNART01L
005600041222     C     KART          READE     FNART01L
005700041222     C
005800140314    3C                   if        NOT %EOF(FNART01L)
005900140314    4c****                         artnpp=88
006000140314     c****               add       1             tot88             7 0
006100140313     c
006200140313     c* verifico se il chi sono è doppio
006300140313     c     kars1         chain     fiars01l
006400140313    5c                   if        %found(fiars01l)
006500140313     c                   eval      knot=arsnot
006600140313     c                   eval      knsc=arsnsc
006700140313
006800140313     c     kars2         setll     fiars02l
006900140313     c     kars2         reade     fiars02l
007000140313    6c                   dow       not %eof(fiars02l)
007100140313    7c                   if        arsnsc<>knsc
007200140313     c                   leave
007300140313    7c                   endif
007400140313     c     kars2         reade     fiars02l
007500140313    6c                   enddo
007600140313     c*
007700140313    6c                   if        not %eof(fiars02l)
007800140313     c* DOPPIA!!!
007900140313     c                   except    bolla
008000140314     c
008100140313     c                   add       1             tot               7 0
008200140313     c                   if        arbdcm>0
008300140313     c                   add       1             cons              7 0
008400140314     c                   else
008500140313     c                   if        arbndc>0
008600140313     c                   add       1             dist              7 0
008700140313     c                   endif
008800140314     c                   endif
008900140313     c                   if        arbcbo='A '
009000140313     c                   add       1             CS                7 0
009100140314     c                   else
009200140314     c* Aggiorno il cod bolla
009300140314     c     kart          chain     fnarb01l
009400140314     c                   if        %found(fnarb01l)
009500140314     c                   eval      a_arbcbo='A '
009600140314     c                   update    fnarb001
009700140313     c                   endif
009800140314     c     kart          chain     fnblp01l
009900140314     c                   if        %found(fnblp01l)
010000140314     c                   eval      blpcbo  ='A '
010100140314     c                   update    fnblp000
010200140314     c                   endif
010300140314     c     kart          chain     titas30c
010400140314     c                   if        %found(titas30c)
010500140314     c                   eval      tascbo  ='A '
010600140314     c                   eval      tastbl  ='F7'
010700140314     c                   update    titas000                             99
010800140314     c                   endif
010900140314     c                   endif
011000140313     c
011100140314     c                   else
011200140313     c                   seton                                        02
011300140314     c***                except    bolla
011400140313     c                   setoff                                       02
011500140313    6c                   endif
011600140313     c                   else
011700140313     c                   seton                                        01
011800140314     c***                except    bolla
011900140313     c                   setoff                                       01
012000140313    5c                   endif
012100140313     c
012200140313    3c                   endif
012300140313    2c                   endif
012400041222     c
012500140313     C     KARB          READE     FNARB27L
012600140313    1c                   enddo
012700041222     c
012800140313     c                   except    totale
012900041222     c                   seton                                        lr
013000041222     c
013100980611      *------------------------------------------------------------
013200140313     OQSYSPRT   E            bolla       1
013300140313     O                                        +   0 'Bolle Amazon doppie'
013400140313     O                       arbaas           +   2
013500041222     O                       arblnp           +   2
013600041222     O                       arbnrs        z  +   2
013700041222     O                       arbnsp        z  +   2
013800140313     O                       arbddc           +   2 '    /  /  '
013900140313     O                       arbndc        z  +   2
014000140313     O                       arbdcm           +   2 '    /  /  '
014100140313     O                       arbcca           +   2
014200140313     O                       arbcbo           +   2
014300140314     O                       artnpp           +   2
014400140313     o               01                         132 'non trovato CHISONO       '
014500140313     o               02                         132 'non trovato CHISONO DOPPIO'
014600140313     OQSYSPRT   E            totale      1
014700140313     O                                        +   0 'Bolle Amazon doppie:TOTALE'
014800140313     O                       tot           2  +   2
014900140313     O                                        +   2 'consegnate'
015000140313     O                       cons          2  +   2
015100140313     O                                        +   2 'in distinta'
015200140313     O                       dist          2  +   2
015300140313     O                                        +   2 'C/S'
015400140313     O                       CS            2  +   2
015500140314     O**                                      +   2 'in 88'
015600140314     O**                     tot88         2  +   2
