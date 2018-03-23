000100040305     H*     controllo spedizioni sparate ad imp ma arrivate in arrivo *
000200930113     H*---------------------------------------------------------------*
000300930113     H DECEDIT('0,') DATEDIT(*DMY.)
000400040305     FFNFVV02L  IF   E           K DISK
000500040305     FFNFGV02L  IF   E           K DISK
000600000204     FFIBRV01L  IF   E           K DISK
000700930115     FAZORG01L  IF   E           K DISK
000800040305     FFNarb01L  IF   E           K DISK
000900040305     FFNart27L  IF   E           K DISK
001000930119     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001100040308     FQSYSPRT1  O    F  132        PRINTER OFLIND(*INOE)
001200930114     D*
001300930114     D* SCHIERA STAMPA TESTATA
001400040305     D STA             S             66    DIM(5) CTDATA PERRCD(1)
001500930113     D*
001600040305     D* SCHIERA CONTENENTE LE lnp di cui colli imp arrivati
001700040305     D lnp             S              3  0 DIM(150)
001800040305     D ncl             S              5  0 DIM(150)
001900040305     D nsp             S              5  0 DIM(150)
002000950320     D*
002100040305     D fnlv53ds      E DS
002200040305     D WLBDAT          DS
002300950131     D  G02DAT                 1      8  0
002400950131     D  G02INV                 9     16  0
002500950131     D  G02ERR                17     17
002600950131     D  G02TGI                18     22  0
002700040305     D arbspe          DS
002800040305     D  arbaas                 1      4  0
002900040305     D  arblnp                 5      7  0
003000040305     D  arbnrs                 8      9  0
003100040305     D  arbnsp                10     16  0
003200930113     C*---------------------------------------------------------------*
003300040305     C     *ENTRY        PLIST
003400040305     c                   parm                    kdatada           8
003500040305     c                   parm                    kdataa            8
003600040305     c                   clear                   datada
003700040305     c                   clear                   dataa
003800040305     c                   if        kdatada>*zeros
003900040305     c                   movel     kdatada       datada            8 0
004000040305     c                   endif
004100040305     c                   if        kdataa>*zeros
004200040305     c                   movel     kdataa        dataa             8 0
004300040305     c                   endif
004400040305     c* lettura tutti fogli imp di una data o data dal al
004500040305     c     kfvv2         klist
004600040305     c                   kfld                    knpg              1 0
004700040305     c                   kfld                    datada
004800040305     c     kbrv1         klist
004900040305     c                   kfld                    fvvnpg
005000040305     c                   kfld                    knfv              6 0
005100040305     c                   kfld                    fvvfgs
005200040305     c     kart          klist
005300040305     c                   kfld                    brvlnp
005400040305     c                   kfld                    brvlna
005500040305     c                   kfld                    brvnrs
005600040305     c                   kfld                    brvnsc
005700040305     c     karb          klist
005800040305     c                   kfld                    artaas
005900040305     c                   kfld                    artlnp
006000040305     c                   kfld                    artnrs
006100040305     c                   kfld                    artnsp
006200040305     c     kfgv          klist
006300040305     c                   kfld                    fvvfle
006400040305     c                   kfld                    fvvdfv
006500040305     c*
006600040305     c                   if        dataa=0
006700040305     c                   eval      dataa=datada
006800040305     c                   endif
006900040305     c                   z-add     3             knpg
007000040305     c                   clear                   savspe           16
007100040308     c                   seton                                        of
007200040305     c
007300040305     c     kfvv2         setll     fnfvv02l
007400040305     c     knpg          reade     fnfvv02l                               30
007500040308    1c                   dow       not *in30  AND
007600040308     c                             fvvdfv<=dataa
007700040305     c* solo foglio imp
007800040305    2c                   if        fvvspg='P'
007900040305     c                   z-add     fvvnfv        knfv
008000040305     c* Leggo le spunte
008100040305     c     kbrv1         setll     fibrv01l
008200040305     c     kbrv1         reade     fibrv01l                               31
008300040305    3c                   dow       not *in31
008400040305     c* Della spunta letta prendo la data di arrivo e cerco il relativo
008500040305     c*  foglio viaggio partenza su cui doveva partire il collo
008600040305     c* solo se c'e' la bolla arrivi anche sul sistema
008700040305     c     kart          chain     fnart27l
008800040305    4c                   if        %found(fnart27l) and artdam>0
008900040305     c     karb          chain     fnarb01l
009000040305    5c                   if        %found(fnarb01l)
009100040305     c*
009200040305     c                   clear                   wuscita
009300040305     c     kfgv          setll     fnfgv02l
009400040305     c     kfgv          reade     fnfgv02l                               33
009500040305    6c                   dow       not *in33  and wuscita=*blanks
009600040305     C                   CLEAR                   fnlv53ds
009700040305     C                   MOVEL     'A'           D53TFO
009800040305     C                   MOVE      FgvNFV        D53NFV
009900040305     C                   MOVE      FgvLNP        D53FGS
010000040305     C                   MOVEL     'B'           D53ABB
010100040305     C                   MOVE      ARBLNA        D53LNA
010200040305     C                   MOVE      ARBTFA        D53TFA
010300040305     C                   CALL      'FNLV53R'
010400040305     c                   parm                    fnlv53ds
010500040305     c*
010600040305     c* se non ci sono errori, verifico se foglio abbinato
010700040305    7c                   if        d53err=*blanks and
010800040305     c                             d53nfa>0
010900040305     c                   movel     'S'           wuscita           1
011000040305    8c                   if        d53dfa=artdam
011100040305     c                   exsr      conta
011200040308     c                   else
011300040308     C                   EXCEPT    SPEDno
011400040305    8c                   endif
011500040305    7c                   endif
011600040305     c     kfgv          reade     fnfgv02l                               33
011700040305    6c                   enddo
011800040305     c
011900040305    5c                   endif
012000040305    4c                   endif
012100040305     c     kbrv1         reade     fibrv01l                               31
012200040305    3c                   enddo
012300040305    2c                   endif
012400040305     c
012500040305     c     knpg          reade     fnfvv02l                               30
012600040305    1c                   enddo
012700040305     C*
012800930115     C                   EXSR      DECTES
012900930113     C*
013000930113     C                   SETON                                            LR
013100040305     C*---------------------------------------------------------------*
013200040305     C*                    conta                                      *
013300040305     C*---------------------------------------------------------------*
013400040305     C     conta         BEGSR
013500040305     c                   z-add     1             A                 3 0
013600040305     c     arblnp        lookup    lnp(a)                                 50
013700040305     c                   if        not           *in50
013800040305     c     000           lookup    lnp(a)                                 50
013900040305     c                   eval      lnp(a)=arblnp
014000040305     c                   endif
014100040305     c* colli
014200040305     c                   add       1             ncl(a)
014300040305     c* spedizioni se non memorizzata
014400040305     c                   if        arbspe<>savspe
014500040305     c                   add       1             nsp(a)
014600040305     c                   eval      savspe=arbspe
014700040305     c                   endif
014800040305     C*
014900040305     C* STAMPO IL COLLO PE CONTROLLO
015000040308     C                   EXCEPT    SPEDSI
015100040305     c                   ENDSR
015200930115     C*---------------------------------------------------------------*
015300930115     C*                    DECTES                                     *
015400930115     C* ROUTINE DI DECODIFICA VARIABILI IN STAMPA  - TESTATA -        *
015500930115     C*---------------------------------------------------------------*
015600930115     C     DECTES        BEGSR
015700930115     C*
015800950131     C                   TIME                    WTIME            14 0
015900950131     C                   MOVE      WTIME         WDATE             8 0
016000950131     C                   MOVEL     WTIME         UTIME             6 0
016100040305     C*
016200040305     c                   clear                   wlbdat
016300040305     C                   Z-ADD     datada        G02inv
016400040305     C                   MOVEL     '3'           G02ERR
016500040305     C                   CALL      'XSRDA8'
016600040305     C                   PARM                    WLBDAT
016700040305     C                   Z-ADD     G02dat        stada             8 0
016800040305     C*
016900040305     c                   if        dataa>0
017000040305     c                   clear                   wlbdat
017100040305     C                   Z-ADD     dataa         G02inv
017200040305     C                   MOVEL     '3'           G02ERR
017300040305     C                   CALL      'XSRDA8'
017400040305     C                   PARM                    WLBDAT
017500040305     C                   Z-ADD     G02dat        staa
017600040305     c                   else
017700040305     c                   clear                   staa              8 0
017800040305     C                   ENDIF
017900040305     C
018000040305     C                   Z-ADD     1             A
018100040305     C                   DOW       LNP(A)>0
018200930115     C* DECODIFICA FILIALE IN GESTIONE
018300040305     C     LNP(A)        CHAIN     AZORG01L                           36
018400040305     C  N36              MOVEL     ORGDES        DECLNP           20
018500040305     C   36              MOVEL     *BLANKS       declnp
018600040305IF   C     *INOF         IFEQ      '1'
018700040305     C                   EXCEPT    TES
018800040305     C                   SETOFF                                           OF
018900040305     C                   ENDIF
019000040305     C                   EXCEPT    DETTA
019100040305     C
019200040305     C                   ADD       1             A
019300040305     C                   ENDDO
019400930115     C*
019500930115     C                   ENDSR
019600930114     C*---------------------------------------------------------------*
019700930114     OQSYSPRT   E            TES              02
019800930114     O                       STA(1)              66
019900930114     O                       STA(2)             132
020000950131     O                       WDATE              120 '  /  /    '
020100930115     O*---------------------------------------------------------------*
020200930114     O          E            TES         1
020300950320     O                       UTIME              119 ' 0:  :  '
020400930115     O*---------------------------------------------------------------*
020500930114     O          E            TES         1
020600930115     O                       STA(3)              66
020700040308     O                       stada               19 '  /  /    '
020800040308     O                       staa                35 '  /  /    '
020900930115     O*---------------------------------------------------------------*
021000040308     O          E            TES         2  1
021100930115     O                       STA(4)              66
021200930119     O*---------------------------------------------------------------*
021300040305     O          E            DETTA          1
021400040305     O                       lnp(a)               3
021500040305     O                       declnp              24
021600040305     O                       ncl(a)        z     31
021700040305     O                       nsp(a)        z     44
021800040305     O*---------------------------------------------------------------*
021900040308     Oqsysprt1  E            SPEDSI         1
022000040308     O                                              'Spedizione SI:'
022100040305     O                       ARBAAS           +   1
022200040305     O                       ARBLNP           +   1
022300040305     O                       ARBNRS        z  +   1
022400040305     O                       ARBNSP        z  +   1
022500040305     O                                        +   1 'Collo'
022600040305     O                       artlna           +   1
022700040305     O                       artnsc        z  +   1
022800040305     O                       artdam           +   1 '    /  /  '
022900040305     O                                        +   1 'foglio'
023000040305     o                       d53fgs        z  +   1
023100040308     o                       d53nfv        z  +   1
023200040308     o                       d53lai           +   1
023300040308     o                       d53nfa        z  +   1
023400040305     O                       d53dfa           +   1 '    /  /  '
023500040308     oqsysprt1  E            SPEDNO         1
023600040308     O                                              'Spedizione NO:'
023700040308     O                       ARBAAS           +   1
023800040308     O                       ARBLNP           +   1
023900040308     O                       ARBNRS        z  +   1
024000040308     O                       ARBNSP        z  +   1
024100040308     O                                        +   1 'Collo'
024200040308     O                       artlna           +   1
024300040308     O                       artnsc        z  +   1
024400040308     O                       artdam           +   1 '    /  /  '
024500040308     O                                        +   1 'foglio'
024600040308     o                       d53fgs        z  +   1
024700040308     o                       d53nfv        z  +   1
024800040308     o                       d53lai           +   1
024900040308     o                       d53nfa        z  +   1
025000040308     O                       d53dfa           +   1 '    /  /  '
025100930114     O*---------------------------------------------------------------*
025200040305**
025300040305                                 ** Colli spuntati in IMP ma arriv    1
025400040305ati **                                      XX/XX/XXXX                2
025500040305Data dal xx/xx/xxxx   al xx/xx/xxxx
025600040305Linea partenza            Colli     Spedizioni
025700040305XXX-XXXXXXXXXXXXXXXXXXXX  XXXXX        XXXXX
