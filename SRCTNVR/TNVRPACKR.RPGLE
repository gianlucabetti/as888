000100951009     H DECEDIT('0,') DATEDIT(*DMY.)
000200980625      * ---------------------------------------------------------
000300140326     fFNblp37L  iF   E           K DISK
000400140326     ffiar801L  IF   E           K DISK
000500140326     ffiar501L  uF   E           K DISK
000600140326     F                                     RENAME(Fiar5000:fiar5f)
000700140326     ffiar531c  uF   E           K DISK    extfile(war5F) usropn
000800140326     ftabel00f  IF   E           K DISK
000900140326     fqsysprt   O    F  132        PRINTER  oflind(*INOF)
001000140326
001100140326     d War5f           s             21    inz('GAITRAGRU /FIAR531C')
001200140326     d dAr5gen       e ds                  Inz
001300140326     d dAr8§pdf      e ds                  Inz
001400140326
001500140326     iFIAR5000      50
001600140326     iFIAR5P00      51
001700980611      * ---------------------------------------------------------
001800041222     C     KART          KLIST
001900140326     C                   KFLD                    blpAAS
002000140326     C                   KFLD                    blpLNP
002100140326     C                   KFLD                    blpNRS
002200140326     C                   KFLD                    blpNSP
002300140326     C     KAR5          KLIST
002400140326     C                   KFLD                    blpAAS
002500140326     C                   KFLD                    blpLNP
002600140326     C                   KFLD                    blpNRS
002700140326     C                   KFLD                    blpNSP
002800140326     C                   KFLD                    TRD               3
002900140326     C                   EVAL      TRD='GEN'
003000140326     C     KArb          KLIST
003100140326     C                   KFLD                    klnp              3 0
003200140326     C                   KFLD                    kdcm              8 0
003300140326
003400140326     c                   clear                   kcut              1 0
003500140326     c                   clear                   kcod              2
003600140326     c                   clear                   kkey              8
003700140326     c
003800140326     c                   Open(e)   fiar531c
003900140326    1c                   if        not %open(fiar531C)
004000140326     c                   eval      %subst(War5f:7:5)='GRPS'
004100140326     c                   Open      fiar531C
004200140326    1c                   endif
004300140326     c                   clear                   conta             9 0
004400140326     c                   clear                   contan            9 0
004500041222     C
004600140326     c     kLNp          SETgt     fnblp000                           30
004700140326     c                   READ      fnblp000
004800041222     C
004900140326    1C                   DOW       NOT %EOF(FNblp37L)
005000140326     C                   EVAL      KLNp=blplnp
005100140326     c                   clear                   kdcm
005200140326     C     KARB          SETLL     FNblp37l
005300140326     C     KARB          READE     FNblp37L
005400041222     C
005500140326    2C                   DOW       NOT %EOF(FNblp37L)
005600140326     c                   clear                   wpdf              1
005700140326     c                   clear                   wpdfsta           1
005800140326     c                   clear                   wpdfmai           1
005900140326     C*
006000140326     C     KART          SETLL     fiar801l
006100140326     C     KART          reade     fiar801l
006200041222     C
006300140326    3C                   DOW       NOT %EOF(fiar801l)
006400140326     c                   eval      wpdf='S'
006500140326     c
006600140326    4c                   if        ar8tmd='§PDF'
006700140326     c                   eval      wpdf='P'
006800140326     c                   eval      dar8§pdf=ar8uni
006900140326    5c                   if        %subst(§ar8pidz:1:1)='1'
007000140326     c                             and (§AR8PTRT='S' or §AR8PTRT='X')
007100140326     c                   eval      wpdfsta='S'
007200140326    5c                   endif
007300140326    5c                   if        %subst(§ar8pidz:2:1)='1'
007400140326     c                   eval      wpdfmai='S'
007500140326    5c                   endif
007600140326     c
007700140326    3c                   endif
007800140326     c
007900140326     C     KART          reade     fiar801l
008000140326    3c                   enddo
008100041222     c
008200041222     c
008300140326    3c                   if        wpdf<>' '
008400140326     c
008500140326   3ac     kar5          chain     fiar501l
008600140326     c                   if        %found(fiar501l)
008700140326     c                   eval      dar5gen=ar5uni
008800140326    4c                   if        §ar5als=' ' and §ar5alx=' '  and §ar5alm=' '
008900140326     c
009000140326    5c                   if        wpdfsta='S'
009100140326     c                   eval      §ar5als='S'
009200140326    5c                   endif
009300140326     c
009400140326    5c                   if        wpdfmai='S'
009500140326     c                   eval      §ar5alm='S'
009600140326    5c                   endif
009700140326
009800140326    5c                   if        wpdf='S'
009900140326     c                   eval      §ar5alx='S'
010000140326    5c                   endif
010100140326     c
010200140326     c                   eval      ar5uni=dar5gen
010300140326    5c                   if        §ar5als=' ' and §ar5alx=' '  and §ar5alm=' '
010400140326     c                   unlock    fiar501l
010500140326     c                   add       1             contaN            9 0
010600140326   x5c                   else
010700140326     c                   update    fiar5f
010800140326     c                   add       1             conta
010900140326    5c                   endif
011000140326     c
011100140326     c                   except    bolla
011200140326     c
011300140326     c                   setoff                                       5051
011400140326
011500140326   5ac                   if        §ar5als<>' ' or §ar5alx<>' '  or §ar5alm<>' '
011600140326     c     kar5          chain     fiar531c
011700140326     c                   eval      dar5gen=ar5uni
011800140326
011900140326    5c                   if        wpdfsta='S'
012000140326     c                   eval      §ar5als='S'
012100140326    5c                   endif
012200140326     c
012300140326    5c                   if        wpdfmai='S'
012400140326     c                   eval      §ar5alm='S'
012500140326    5c                   endif
012600140326
012700140326    5c                   if        wpdf='S'
012800140326     c                   eval      §ar5alx='S'
012900140326    5c                   endif
013000140326     c
013100140326     c                   eval      ar5uni=dar5gen
013200140326     c   50              update    fiar5000
013300140326     c   51              update    fiar5P00
013400140326     c
013500140326   5ac                   endif
013600140326    4c                   endif
013700140326   3ac                   endif
013800140326    3c                   endif
013900041222     C
014000041222     c
014100140326     C     KARB          READE     FNblp37L
014200041222    2c                   enddo
014300041222     c
014400140326     c     klnp          setgt     fnblp37l
014500140326     c                   read      fnblp37l
014600041222    1c                   enddo
014700041222     c
014800140326     c                   except    totale
014900041222     c                   seton                                        lr
015000041222     c
015100980611      *------------------------------------------------------------
015200140326     Oqsysprt   E            bolla       1
015300140326     O                                        +   0 'bolla '
015400140326     O                                        +   0 'per packing list'
015500140326     O                       blpaas           +   2
015600140326     O                       blplnp           +   2
015700140326     O                       blpnrs        z  +   2
015800140326     O                       blpnsp        z  +   2
015900140326     O                       blpctm           +   2
016000140326     o                                        +   2 'WPDF'
016100140326     O                       wpdf             +   2
016200140326     o                                        +   2 'stampa'
016300140326     O                       wpdfsta          +   2
016400140326     o                                        +   2 'mail'
016500140326     O                       wpdfmai          +   2
016600140326     Oqsysprt   E            totale      1
016700140326     O                                        +   0 'totale '
016800140326     O                       conta         2  +   2
016900140326     O                                        +   0 'totale non aggiornatiPDF'
017000140326     O                       contan        2  +   2
