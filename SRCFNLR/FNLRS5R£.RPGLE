000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200040521      * FNLRS5R *----------------------------------------------------*
000300040521      *            Stampa segnacolli export DPD in arrivo -BATCH-    *
000400911205      *--------------------------------------------------------------*
000500040521     FFNARB25l  IF   E           K DISK
000600040521     FFNARt01l  IF   E           K DISK
000700040521     Ffnwlr73f  o  A E           K DISK    usropn
000800031111     fAzorg01l  if   e           k Disk
000900970219     D*
001000040521     D kdcm            S                   LIKE(ARBDCM)
001100000427     D L1              S              3  0 DIM(30)                              P.O.GESTITI
001200051007     d skey            s              8  0 dim(900)
001300051007     d sl53            s              9    dim(900)
001400000419     D*
001500970219     D* PARAMETRI RICEVUTI DAL PGM CHIAMANTE
001600040521     D fnlrs4ds      e DS
001700000427     D* DS ESTERNA PER trul06r
001800000427     D trul06ds      E DS
001900000427     D  LIN                    1     90  0 DIM(30)                              P.O. COMODO
002000040521      * Parametri per richiamo a TRUL09R
002100040521     d Trul09ds      e ds
002200040521     d  T09                   19    108  0 dim(30)
002300040521
002400040521     D slna            S              3  0 DIM(30)                              L6 NO GESTITA
002500970219     D*
002600940216     D KPJBA         E DS
002700970219     D CNCR80        E DS
002800970219     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002900000418     D fnlv53ds      E DS
003000040521     D OG150         E DS
003100040521     d                 ds
003200040521     d  fdsfle                 1      3  0
003300040521     d  fdsnfv                 4      8  0
003400040521     d  dskey                  1      8  0
003500040521     d                 ds
003600040521     d  fdserr                 1      1
003700040521     d  fdsnfa                 2      6  0
003800040521     d  fdsfgs                 7      9  0
003900040521     d  dsl53                  1      9
004000040521     d* parametri per pgm fnlr73r: di tutti i seguenti parametri imposto sol
004100040521     d* TIRIS="F"
004200040521     D PARAM           DS
004300040521     D  VIDLNP                11     13  0 inz
004400040521     D  DSD                   14     21  0 inz
004500040521     D  DAAS                  14     17  0 inz
004600040521     D  DMGS                  18     21  0 inz
004700040521     D  DSA                   22     29  0 inz
004800040521     D  VIDSED                30     31  0 inz
004900040521     D  VIDNSD                32     38  0 inz
005000040521     D  NUMD                  30     38  0 inz
005100040521     D  VIDAAS                39     42  0 inz
005200040521     D  VIDSEA                43     44  0 inz
005300040521     D  VIDNSA                45     51  0 inz
005400040521     D  NUMA                  43     51  0 inz
005500040521     D  VIDLNA                52     54  0 inz
005600040521     D  TIRIS                 55     55    inz('F')
005700040521     D  VIDSDI                56     56    inz
005800040521     D  VIDDDT                57     57    inz
005900040521     d  VidFls                58     60  0 inz
006000040521     d  VidNrs                61     62  0 inz
006100040521     d  VidNsc                63     69  0 inz
006200040521     d  VidLnas               70     72  0 inz
006300040521     d* - parametri da TRUL90R:
006400040521     d* · pgm.   per stampa LDV
006500040521     d  wD90psl              157    166    inz
006600040521     d* · pgm di stampa etichette segnacolli
006700040521     D  wd90pss              207    216
006800970219     C****************************************************************
006900970219     C*  RIEPILOGO INDICATORI
007000970219     C****************************************************************
007100970219     C* 01    - OFF ---> S T A M P A        ON ---> R I S T A M P A
007200970402     C* 30    - LETTURA
007300980102     C* 31/35 - COMODO
007400970219     C*****************************************************************
007500920422     C*
007600970219     C*****
007700970219     C* E L A B O R O
007800970219     C*****
007900970402     C*
008000040521     c                   open      fnwlr73f
008100040521     c                   z-add     1             ii
008200040526    1c     slna(ii)      dowgt     *zeros
008300040521     c     ii            andle     30
008400040521     c* a cambio di lna pulisco schiere di comodo per richiamo lv53
008500040521     c                   clear                   skey
008600040521     c                   clear                   sl53
008700040521     c                   clear                   i5                3 0
008800040521     c     karb          setll     fnarb25l
008900040526    2c                   do        *hival
009000040521     c     karb          reade     fnarb25l
009100040526    3c                   if        %eof(fnarb25l)
009200040521     c                   leave
009300040526    3c                   endif
009400040521     c* guardo se bolla locale
009500040521     c     arblnp        lookup    l1                                     35
009600040521     c     kart          setll     fnart01l
009700040526    3c                   do        *hival
009800040521     c     kart          reade     fnart01l
009900040526    4c                   if        %eof(fnart01l)
010000040521     c                   leave
010100040526    4c                   endif
010200040526     c* se data arrivo=data foglio oppure se collo locale senza data
010300040526     c* di arrivo --> sempre abbinato
010400040526    4c     artdam        ifeq      ds1dfa
010500040526     c     *in35         oreq      *on
010600040526     c     artdam        andeq     *zeros
010700040526     c                   movel     'S'           wabb              1
010800040526     c                   else
010900040526     c                   clear                   wabb
011000040526    4c                   endif
011100040521     c* se non locale richiamo fnlv53r per sapere se abbinato a foglio arriv
011200040521     c* richiesto
011300040521     c* richiamo fnlv53r solo se non già richiamato per lo stesso fle/Nfv
011400040521     c* lna non la considero in quanto sto leggendo con reade su arb per lna
011500040526    4c     wabb          ifeq      *blanks
011600040521     c                   eval      fdsfle = arbfle
011700040521     c                   eval      fdsnfv = artnfv
011800040521     c                   z-add     1             i5a               3 0
011900040521     c     dskey         lookup    skey(i5a)                              30
012000040526    5c     *in30         ifeq      *off
012100040521     c                   clear                   fnlv53ds
012200040521     c                   movel     'A'           d53tfo
012300040521     c                   z-add     simfel        d53fel
012400040521     c                   z-add     artnfv        d53nfv
012500040521     c                   move      arbfle        d53fgs
012600040521     c                   movel     'B'           d53abb
012700040521     c                   z-add     arblna        d53lna
012800040521     c                   z-add     arbtfa        d53tfa
012900040521     c                   call      'FNLV53R'
013000040521     c                   parm                    fnlv53ds
013100040521     c                   eval      fdserr = d53err
013200040521     c                   eval      fdsnfa = d53nfa
013300040521     c                   eval      fdsfgs = d53lai
013400040521     c* memorizzo dati in schiere
013500040521     c                   add       1             i5
013600040521     c                   movel     dskey         skey(i5)
013700040521     c                   movel     dsl53         sl53(i5)
013800040526    5c                   endif
013900040521     c   30              movel     sl53(i5a)     dsl53
014000040526    5c     fdserr        ifeq      *blanks
014100040526     c     fdsnfa        andeq     ds1nfa
014200040526     c     fdsfgs        andeq     ds1fgs
014300040521     c                   movel     'S'           wabb
014400040526    5c                   endif
014500040526    4c                   endif
014600040521     c*se abbinato al foglio richiesto memorizzo in file di work
014700040526    4c     wabb          ifeq      'S'
014800040521     c                   eval      wlrlnp = artlnp
014900040521     c                   eval      wlrlna = artlna
015000040521     c                   eval      wlrnrs = artnrs
015100040521     c                   eval      wlrnsc = artnsc
015200040521     c                   eval      wlrrmn = arbrmn
015300040521     c                   write     fnwlrs73
015400040526    4c                   endif
015500040526    3c                   enddo
015600040526    2c                   enddo
015700040521     c                   add       1             ii
015800040526    1c                   enddo
015900000418     c
016000000427     c* Richiamo pgm che elabora file di work e richiama la stampa l.v.
016100040521     c                   close     fnwlr73f
016200040521     c                   clear                   kpjbu
016300040521     c                   eval      wd90pss = ds1d90pss
016400040521     c                   movel     param         kpjbu
016500040521     c                   call      'FNLR73R'
016600000427     c                   parm                    kpjba
016700000427     c*
016800000427     C                   movel     'C'           d53tla
016900000427     C                   call      'FNLV53R'
017000000427     C                   parm                    fnlv53ds
017100911206      *
017200911206     C                   SETON                                        LR
017300970219     C*
017400970219     C*--- SR INIZIALE -----------------------------------------------*
017500970219     C     *INZSR        BEGSR
017600000426     C****  DATI PASSATI  ****
017700000426     C     *ENTRY        PLIST
017800000426     C                   PARM                    KPJBA
017900040521     C                   MOVEL     KPJBU         fnlrs4ds
018000040521     c* carico schiera delle linee di arrivo da elaborare
018100040521     c                   clear                   slna
018200040521     c     ds1lna        ifgt      *zeros
018300040521     c                   z-add     ds1lna        slna(1)
018400040521     c                   else
018500040521     c* se non passata lna specifica carico le lna con §ogsea='S'
018600040521     c* dell'area di arrivo del p.o. gestione
018700040521     c                   clear                   trul09ds
018800040521     c                   eval      i09tla = 'L'
018900040521     c                   eval      i09mod = 'TFA'
019000040521     c                   eval      i09tfa = ds1fgs
019100040521     c                   eval      i09dta = ds1dfa
019200040521     c                   call      'TRUL09R'
019300040521     c                   parm                    trul09ds
019400040521     c                   z-add     1             ii                2 0
019500040521     c                   z-add     0             xx                2 0
019600040521     c     t09(ii)       dowgt     *zeros
019700040521     c     ii            andle     30
019800040521     c     t09(ii)       chain     azorg01l
019900040521     c                   if        %found(azorg01l)
020000040521     c                   movel     orgdf0        og150
020100040521     c                   else
020200040521     c                   clear                   og150
020300040521     c                   endif
020400040521     c     §ogsea        ifeq      'S'
020500040521     c                   add       1             xx
020600040521     c                   eval      slna(xx) = t09(ii)
020700040521     c                   endif
020800040521     c                   add       1             ii
020900040521     c                   enddo
021000040521     c                   endif
021100040521
021200000426     C****  REPERISCO SIMFEL  ****
021300000426     C                   Z-ADD     1             CODUT
021400000426     C                   CALL      'X§PARUT'
021500000426     C                   PARM                    UT§DSE
021600000426     C                   MOVEL     REC80         CNCR80
021700000427     C****  CARICO TABELLA FILIALI GESTITE £1  ****
021800000427     C                   clear                   trul06ds
021900040521     c                   movel     'L'           d06tla
022000000427     C                   move      '£1'          d06cod
022100040521     C                   movel     simfel        d06key
022200000427     C                   movel     trul06ds      kpjbu
022300000427     C                   call      'TRUL06R'
022400000427     C                   parm                    kpjba
022500000427     C                   movel     kpjbu         trul06ds
022600000427     C                   movea     lin           l1
022700040521     c*
022800970219     C****
022900970219     C** KLIST
023000970219     C****
023100040521     c     karb          klist
023200040521     c                   kfld                    slna(ii)
023300040521     c                   kfld                    kdcm
023400040521     c                   move      *zeros        kdcm
023500040521     C     Kart          KLIST
023600000419     C                   KFLD                    ARBAAS
023700000419     C                   KFLD                    ARBLNP
023800000419     C                   KFLD                    ARBNRS
023900000419     C                   KFLD                    ARBNSP
024000970402     C*
024100970402     C                   ENDSR
