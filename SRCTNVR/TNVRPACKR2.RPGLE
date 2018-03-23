000100951009     H DECEDIT('0,') DATEDIT(*DMY.)
000200980625      * ---------------------------------------------------------
000300140326     fFNblp37L  iF   E           K DISK
000400140326     ffiar801L  IF   E           K DISK
000500140326     ffiar501L  uF   E           K DISK
000600140326     F                                     RENAME(Fiar5000:fiar5f)
000700140326     ffiar531c  uF   E           K DISK    extfile(war5F) usropn
000800140326     ftabel00f  IF   E           K DISK
000900140326     fqsysprt   O    F  132        PRINTER  oflind(*INOF)
001000140326     fprtf198   O    F  198        PRINTER  oflind(*INOE)
001100140326
001200140326     d War5f           s             21    inz('GAITRAGRU /FIAR531C')
001300140326     d dAr5gen       e ds                  Inz
001400140326     d dAr8§pdf      e ds                  Inz
001500140326
001600140326     iFIAR5000      50
001700140326     iFIAR5P00      51
001800980611      * ---------------------------------------------------------
001900041222     C     KART          KLIST
002000140326     C                   KFLD                    blpAAS
002100140326     C                   KFLD                    blpLNP
002200140326     C                   KFLD                    blpNRS
002300140326     C                   KFLD                    blpNSP
002400140326     C     KAR5          KLIST
002500140326     C                   KFLD                    blpAAS
002600140326     C                   KFLD                    blpLNP
002700140326     C                   KFLD                    blpNRS
002800140326     C                   KFLD                    blpNSP
002900140326     C                   KFLD                    TRD               3
003000140326     c
003100140326     C                   EVAL      TRD='GEN'
003200140326     c
003300140326     C     KArb          KLIST
003400140326     C                   KFLD                    klnp              3 0
003500140326     C                   KFLD                    kdcm              8 0
003600140326
003700140326     c                   clear                   kcut              1 0
003800140326     c                   clear                   kcod              2
003900140326     c                   clear                   kkey              8
004000140326     c
004100140326     c                   Open(e)   fiar531c
004200140326    1c                   if        not %open(fiar531C)
004300140326     c                   eval      %subst(War5f:7:5)='GRPS'
004400140326     c                   Open      fiar531C
004500140326    1c                   endif
004600140326
004700140326     c                   clear                   conta             9 0
004800140326     c                   clear                   contan            9 0
004900140326     c                   eval      kdcm=20140313
005000140326     c
005100041222     C
005200140326     c     kLNp          SETgt     fnblp000                           30
005300140326     c                   READ      fnblp000
005400041222     C
005500140326    1C                   DOW       NOT %EOF(FNblp37L)
005600140326     C                   EVAL      KLNp=blplnp
005700140326     C     KARB          SETLL     FNblp37l
005800140326     C     Klnp          READE     FNblp37L
005900041222     C
006000140326    2C                   DOW       NOT %EOF(FNblp37L)
006100140326     c                   clear                   wpdf              1
006200140326     c                   clear                   wpdfsta           1
006300140326     c                   clear                   wpdfmai           1
006400140326     C*
006500140326     C     KART          SETLL     fiar801l
006600140326     C     KART          reade     fiar801l
006700041222     C
006800140326    3C                   DOW       NOT %EOF(fiar801l)
006900140326     c                   eval      wpdf='S'
007000140326     c
007100140326    4c                   if        ar8tmd='§PDF'
007200140326     c                   eval      wpdf='P'
007300140326     c                   eval      dar8§pdf=ar8uni
007400140326    5c                   if        %subst(§ar8pidz:1:1)='1'
007500140326     c                             and (§AR8PTRT='S' or §AR8PTRT='X')
007600140326     c                   eval      wpdfsta='S'
007700140326    5c                   endif
007800140326    5c                   if        %subst(§ar8pidz:2:1)='1'
007900140326     c                   eval      wpdfmai='S'
008000140326    5c                   endif
008100140326     c
008200140326    3c                   endif
008300140326     c
008400140326     C     KART          reade     fiar801l
008500140326    3c                   enddo
008600041222     c
008700140326    3c                   if        wpdf<>' '
008800140326     c
008900140326     c     kar5          chain     fiar501l
009000140326   3ac                   if        %found(fiar501l)
009100140326     c                   eval      dar5gen=ar5uni
009200140326    4c                   if        §ar5als=' ' and §ar5alx=' '  and §ar5alm=' '
009300140326     c
009400140326    5c                   if        wpdfsta='S'
009500140326     c                   eval      §ar5als='S'
009600140326    5c                   endif
009700140326     c
009800140326    5c                   if        wpdfmai='S'
009900140326     c                   eval      §ar5alm='S'
010000140326    5c                   endif
010100140326
010200140326    5c                   if        wpdf='S'
010300140326     c                   eval      §ar5alx='S'
010400140326    5c                   endif
010500140326     c
010600140326     c                   eval      ar5uni=dar5gen
010700140326    5c                   if        §ar5als=' ' and §ar5alx=' '  and §ar5alm=' '
010800140326     c                   unlock    fiar501l
010900140326     c                   add       1             contaN            9 0
011000140326     c                   except    bolla1
011100140326   x5c                   else
011200140326     c                   update    fiar5f
011300140326     c                   except    bolla
011400140326     c                   add       1             conta
011500140326    5c                   endif
011600140326
011700140326     c
011800140326     c                   setoff                                       5051
011900140326   5ac                   if        §ar5als<>' ' or §ar5alx<>' '  or §ar5alm<>' '
012000140326     c     kar5          chain     fiar531c
012100140326     c
012200140326   5bc                   if        %found(fiar531c)
012300140326     c                   eval      dar5gen=ar5uni
012400140326
012500140326    5c                   if        wpdfsta='S'
012600140326     c                   eval      §ar5als='S'
012700140326    5c                   endif
012800140326     c
012900140326    5c                   if        wpdfmai='S'
013000140326     c                   eval      §ar5alm='S'
013100140326    5c                   endif
013200140326
013300140326    5c                   if        wpdf='S'
013400140326     c                   eval      §ar5alx='S'
013500140326    5c                   endif
013600140326     c
013700140326     c                   eval      ar5uni=dar5gen
013800140326     c   50              update    fiar5000
013900140326     c   51              update    fiar5P00
014000140326     c
014100140326 x5b c                   else
014200140326     c                   add       1             c_noar5s          9 0
014300140326     c     kar5          chain     fiar501l
014400140326   3ac                   if        %found(fiar501l)
014500140326     c                   clear                   ar5ft2
014600140326     c                   clear                   ar5ft3
014700140326     c                   update    fiar5f
014800140326     c                   endif
014900140326     c
015000140326   5bc                   endif
015100140326   5ac                   endif
015200140326    4c                   endif
015300140326     c
015400140326  x3ac                   else
015500140326     c                   add       1             c_noar5f          9 0
015600140326     c                   except    bollaNO5
015700140326   3ac                   endif
015800140326    3c                   endif
015900041222     C
016000041222     c
016100140326     C     Klnp          READE     FNblp37L
016200041222    2c                   enddo
016300041222     c
016400140326     c     klnp          setgt     fnblp37l
016500140326     c                   read      fnblp37l
016600041222    1c                   enddo
016700041222     c
016800140326     c                   except    totale
016900041222     c                   seton                                        lr
017000041222     c
017100980611      *------------------------------------------------------------
017200140326     Oqsysprt   E            bolla       1
017300140326     O                                        +   0 'bolla '
017400140326     O                                        +   0 'per packing list'
017500140326     O                       blpaas           +   2
017600140326     O                       blplnp           +   2
017700140326     O                       blpnrs        z  +   2
017800140326     O                       blpnsp        z  +   2
017900140326     O                       blpctm           +   2
018000140326     O                       blpdcm           +   2 '    /  /  '
018100140326     o                                        +   2 'WPDF'
018200140326     O                       wpdf             +   2
018300140326     o                                        +   2 'stampa'
018400140326     O                       wpdfsta          +   2
018500140326     o                                        +   2 'mail'
018600140326     O                       wpdfmai          +   2
018700140326     O                       blprsm           +   2
018800140326     Oqsysprt   E            bollaNO5    1
018900140326     O                                        +   0 'bolla '
019000140326     O                                        +   0 'no fiar5 FIL'
019100140326     O                       blpaas           +   2
019200140326     O                       blplnp           +   2
019300140326     O                       blpnrs        z  +   2
019400140326     O                       blpnsp        z  +   2
019500140326     O                       blpnsp        z  +   2
019600140326     O                       blpctm           +   2
019700140326     O                       blpdcm           +   2 '    /  /  '
019800140326     o                                        +   2 'WPDF'
019900140326     O                       wpdf             +   2
020000140326     o                                        +   2 'stampa'
020100140326     O                       wpdfsta          +   2
020200140326     o                                        +   2 'mail'
020300140326     O                       wpdfmai          +   2
020400140326     Oqsysprt   E            totale      1
020500140326     O                                        +   0 'totale '
020600140326     O                       conta         2  +   2
020700140326     O                                        +   2 'totale non aggiornatiPDF'
020800140326     O                       contan        2  +   2
020900140326     O                                        +   2 'no FIAR5 in fil'
021000140326     O                       c_noar5f      2  +   2
021100140326     O                                        +   2 'no FIAR5 in sed'
021200140326     O                       c_noar5s      2  +   2
021300140326     Oprtf198   E            bolla1      1
021400140326     O                                        +   0 'Bolla con PDF '
021500140326     O                                        +   0 'senza trattamento '
021600140326     O                                        +   0 'X/S'
021700140326     O                       blpaas           +   2
021800140326     O                       blplnp           +   2
021900140326     O                       blpnrs        z  +   2
022000140326     O                       blpnsp        z  +   2
022100140326     O                       blpksc           +   2
022200140326     O                       blprsm           +   2
022300140326     O                       blpctm           +   2
022400140326     o                                        +   2 'WPDF'
022500140326     O                       wpdf             +   2
022600140326     o                                        +   2 'stampa'
022700140326     O                       wpdfsta          +   2
022800140326     o                                        +   2 'mail'
022900140326     O                       wpdfmai          +   2
023000140326     o
023100140326     O                       blpdcm           +   2 '    /  /  '
