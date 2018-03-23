000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941228     H* FNLS83R *----------------------------------------------------*
000300970630     H*                  STAMPA  BOLLE NO DDT
000400000000     H*--------------------------------------------------------------*
000500051109     FFNBLP45L  iF   E           K DISK
000600051109     F                                     INFDS(blp1)
000700051109     FFNBLP00f  UF   E             DISK    RENAME(FNblp000:FNblpfis)
000800000511     FAZORG01L  IF   E           K DISK
000900991116      *
001000991118      *  SCHIERE PER COMPORRE BARCODE
001100991118      *
001200051109     D blp1            DS
001300051109     D  blpNRR               397    400B 0
001400941228     D PARAM           DS
001500941228     D  V1CLNA                 1      3  0
001600941228     D  V1CKSC                 4     10  0
001700941228     D  V1CPDR                11     17  0
001800941228     D  V1CSOP                18     18
001900970218     D  V1CLNP                19     21  0
002000970326     D  PARDSD                22     29  0
002100970326     D  PARDSA                30     37  0
002200970630     D  V1CNRD                38     39  0
002300970630     D  V1CNSD                40     46  0
002400970630     D  V1CNRA                47     48  0
002500970630     D  V1CNSA                49     55  0
002600970630     D  V1CRIS                56     56
002700000511     D  V1CDPD                57     57
002800030623     d* - parametri da TRUL90R:
002900030623     d* · pgm.   per stampa LDV
003000030623     d  wD90psl              187    196
003100970326     D                 DS
003200970326     D  BLPAAS                 1      4  0
003300970326     D  BLPMGS                 5      8  0
003400970326     D  BLPDSP                 1      8  0
003500991118      * CALCOLO DEL CHCK DIGIT PER BARCODE DPD
003600991118     D                 DS
003700991118     D  DPDLNP                 1      3  0
003800991118     D  DPDGMS                 4      7  0
003900991118     D  DPDNDS                 8     11  0
004000991118     D  §11                    1     11  0
004100991118     D                                     DIM(11)
004200991118     D  DPDCHD                12     12  0
004300991118     D  DPDBRC                 1     12  0
004400030623      * DS per pgm di STAMPA BOLLE
004500030623     d FNLSB5DS      e ds                  inz
004600910205     D KPJBA         E DS
004700000511     D OG143         E DS
004800020610     d ut§dse0f      e ds
004900020610     d cncr80        e ds
005000020610     d trul06ds      e ds
005100020610     d  lin                    1     90  0 dim(30)
005200030623
005300030623     d wPagA4          s              5  0 inz
005400030623     d wPagA5          s              5  0 inz
005500030623     d wPagB0          s              5  0 inz
005600030623
005700910412     C****************************************************************
005800910412     C*  RIEPILOGO INDICATORI
005900910412     C***************************************************************
006000000000     C     *ENTRY        PLIST
006100000000     C                   PARM                    KPJBA
006200020610     C                   MOVEL     KPJBU         PARAM
006300020610
006400020610     c                   eval      codut = 1
006500020610     c                   call      'X§PARUT'
006600020610     c                   parm                    ut§dse0f
006700020610     c                   movel     rec80         cncr80
006800020610
006900020610     c                   if        v1clnp = *zeros
007000020610     c                   clear                   trul06ds
007100020610     c                   eval      d06tla = 'L'
007200020610     c                   eval      d06cod = '£1'
007300020610     c                   movel     simfel        d06key
007400020610     c                   movel     trul06ds      kpjbu
007500020610     c                   call      'TRUL06R'
007600020610     c                   parm                    kpjba
007700020610     c                   movel     kpjbu         trul06ds
007800020610     c                   endif
007900020610
008000970630     C     KBLP          KLIST
008100970630     C                   KFLD                    WFST              1
008200970630     C                   KFLD                    V1CLNA
008300000511     C*
008400000511     C     *LIKE         DEFINE    BLPLNP        SAVLNP
008500900511     C*---------------------------------------------------------------*
008600900517     C*
008700020610     C**!!!              MOVEL     KPJBU         PARAM
008800971014     C* AZZERO IL CONTATORE DELLE BOLLE
008900030623     ***C                   CLEAR                   WCONTB
009000030623     c                   clear                   wPagA4
009100030623     c                   clear                   wPagA5
009200030623     c                   clear                   wPagB0
009300970630     C* SE RISTAMPA IMPOSTO Y
009400970630     C     V1CRIS        IFEQ      'S'
009500970630     C                   MOVEL     'Y'           WFST
009600970630     C**
009700970630     C* SOLO IN RISTAMPA C'E' IL NUMERO DI SPEDIZIONE
009800970630     C     V1CNSD        IFGT      0
009900970630     C     V1CNSA        ANDEQ     0
010000970630     C                   Z-ADD     V1CNSD        V1CNSA
010100970630     C                   ENDIF
010200970630     C                   ELSE
010300970630     C                   MOVEL     'N'           WFST
010400970630     C                   ENDIF
010500970630     C*
010600941228     C**
010700941228     C* VEDO SELEZIONI FATTE
010800941228     C**
010900941228     C     V1CLNA        IFGT      *ZEROS
011000910205     C                   SETON                                        02
011100910205     C                   END
011200910205     C*
011300970630     C   02KBLP          SETLL     FNBLP45L
011400970630     C  N02WFST          SETLL     FNBLP45L
011500910205     C*
011600970630     C   02KBLP          READE     FNBLP45L                               31
011700970630     C  N02WFST          READE     FNBLP45L                               31
011800910205     C*
011900030623do  1C     *IN31         DOWEQ     *OFF
012000941228     C*
012100941228     C                   EXSR      CONFRO
012200910412     C* STAMPA
012300030623if  2C     WSTAMP        IFEQ      'S'
012400030623     c                   clear                   FNLSB5DS
012500941228     C                   MOVEL     BLPAAS        DB0AAS
012600941228     C                   MOVEL     BLPLNP        DB0LNP
012700941228     C                   MOVEL     BLPNRS        DB0NRS
012800941228     C                   MOVEL     BLPNSP        DB0NSP
012900941228     C                   MOVEL     'P'           DB0TBO
013000991118      *
013100970630     C     V1CRIS        IFEQ      'S'
013200970630     C                   MOVEL     'R'           DB0RIS
013300970630     C                   ENDIF
013400030623     C*
013500030623     ***C                   ADD       1             WCONTB
013600030623     ***C**
013700030623     ***C     WCONTB        IFGT      490
013800030715      * Se ho già stampato 490 bolle:
013900030715if  3c                   IF           wPagA4 >= 490
014000030623     c                             or wPagA5 >= 490
014100030623     c                             or wPagB0 >= 490
014200030623if  4C     SAVLNA        IFNE      BLPLNA
014300971014     C     V1CLNA        ANDEQ     0
014400971014     C     V1CLNA        ORGT      0
014500030715      *    1) chiedo la chiusura e riapertura dei prtf
014600030715     c                   eval      DB0fl3 =  *on
014700030715     c                   eval      DB0fl4 =  *on
014800030715     c                   eval      DB0fl6 =  *on
014900030715     c                   eval      DB0fl7 =  *on
015000030715     c                   eval      DB0fl8 =  *on
015100030715      *    2) e riparto con i conteggi
015200030623     ***C                   Z-ADD     1             WCONTB            5 0
015300030623     c                   clear                   wPagA4
015400030623     c                   clear                   wPagA5
015500030623     c                   clear                   wPagB0
015600030623e   4C                   ENDIF
015700030623e   3C                   ENDIF
015800971014     C**
015900030623     c                   call      wD90psl
016000030623     c                   parm                    FNLSB5DS
016100030623      *
016200030623      * Aggiorno il conteggio delle pagine stampate (x tipo)
016300030623sel 3c                   select
016400030623      * - formato A4 su stampante laser
016500030623w   3c                   when      DB0tms = '4'
016600030623     c                   add       1             wPagA4
016700030623      * - formato A5 su stampante laser
016800030623w   3c                   when      DB0tms = '5'
016900030623     c                   add       1             wPagA5
017000030623      * - altri..... su stampante ad aghi
017100030623x   1c                   other
017200030623     c                   add       1             wPagB0
017300030623e   3c                   endsl
017400971014     C**
017500971014     C                   MOVEL     BLPLNA        SAVLNA            3 0
017600941228     C* AGGIORNO BOLLA
017700970630     C     V1CRIS        IFEQ      'N'
017800051109     c     blpnrr        chain     fnblp00f
017900970213     C                   MOVEL     'Y'           BLPFST
018000051109     C                   UPDATE    FNBLPfis
018100970630     C                   ENDIF
018200941228     C                   ENDIF
018300941228     C**
018400970630     C   02KBLP          READE     FNBLP45L                               31
018500970630     C  N02WFST          READE     FNBLP45L                               31
018600941228     C                   ENDDO
018700941228     C*
018800000000     C                   SETON                                        LR
018900910205     C*
019000941228     C* CONFRONTI----------------------------------------------------*
019100941228     C     CONFRO        BEGSR
019200941228     C                   CLEAR                   WSTAMP
019300991012     C* ESCLUDO SEMPRE I PREPAGATI NON CONFERMATI
019400991012     C     BLPPDR        IFEQ      0
019500991012     C                   GOTO      ENDCON
019600991012     C                   ENDIF
019700970218     C* LINEA DI PARTENZA
019800970218     C     V1CLNP        IFGT      0
019900970218     C     V1CLNP        ANDNE     BLPLNP
020000970218     C                   GOTO      ENDCON
020100970218     C                   ENDIF
020200000511     C* CONTROLLO SE RICHIESTE SOLO LE LNP DPD
020300000511    1C     V1CDPD        IFEQ      'D'
020400000511    2C     BLPLNP        IFNE      SAVLNP
020500000511     C                   Z-ADD     BLPLNP        SAVLNP
020600000511     C     BLPLNP        CHAIN     AZORG01L                           30
020700000511     C  N30              MOVEL     ORGDE3        OG143
020800000511     C   30              CLEAR                   OG143
020900000511    2C                   ENDIF
021000020206      **         §OGDPD    IFNE 'S'
021100020206    2C     §OGNTW        IFNE      'DPD'
021200000511     C                   GOTO      ENDCON
021300000511    2C                   ENDIF
021400000511    1C                   ENDIF
021500020610      * controllo che la linea di partenza della bolla faccia parte della £1
021600020610      * se non immesso il p.o. gestione
021700020610     c                   if        v1clnp = *zeros
021800020610     c     blplnp        lookup    lin                                    30
021900020610     c                   if        not *in30
022000020610     c                   goto      endcon
022100020610     c                   endif
022200020610     c                   endif
022300970218     C*
022400941228     C* SIGLA OPERATORE
022500941228     C     V1CSOP        IFNE      *BLANKS
022600941228     C     V1CSOP        ANDNE     BLPSOP
022700941228     C                   GOTO      ENDCON
022800941228     C                   ENDIF
022900941228     C*
023000941228     C* CODICE CLIENTE
023100941228     C     V1CKSC        IFNE      0
023200941228     C     BLPCCM        IFGT      *ZEROS
023300941228     C     BLPCCM        IFNE      V1CKSC
023400941228     C                   GOTO      ENDCON
023500941228     C                   ENDIF
023600941228     C*
023700941228     C                   ELSE
023800941228     C*
023900941228     C     BLPKSC        IFNE      V1CKSC
024000941228     C                   GOTO      ENDCON
024100941228     C                   ENDIF
024200941228     C                   ENDIF
024300941228     C                   ENDIF
024400941228     C* CODICE PADRONCINO
024500941228     C     V1CPDR        IFGT      *ZEROS
024600941228     C     V1CPDR        ANDNE     BLPPDR
024700941228     C                   GOTO      ENDCON
024800941228     C                   ENDIF
024900970326     C* DATA SPEDIZIONE
025000970326     C     PARDSD        IFGT      0
025100970326     C     PARDSA        ANDGT     0
025200970326     C     BLPDSP        IFLT      PARDSD
025300971014     C     BLPDSP        ORGT      PARDSA
025400970326     C                   GOTO      ENDCON
025500970326     C                   ENDIF
025600970326     C                   ENDIF
025700970326     C     PARDSD        IFGT      0
025800970326     C     PARDSA        ANDEQ     0
025900970326     C     BLPDSP        ANDNE     PARDSD
026000970326     C                   GOTO      ENDCON
026100970326     C                   ENDIF
026200970326     C     PARDSD        IFEQ      0
026300970326     C     PARDSA        ANDGT     0
026400970326     C     BLPDSP        ANDGT     PARDSA
026500970326     C                   GOTO      ENDCON
026600970326     C                   ENDIF
026700970630     C* NUMERO SPEDIZIONE
026800970630     C     V1CNSD        IFGT      0
026900970630     C     V1CNSA        ORGT      0
027000970630     C     BLPNSP        IFLT      V1CNSD
027100970630     C     BLPNSP        ORGT      V1CNSA
027200970630     C                   GOTO      ENDCON
027300970630     C                   ENDIF
027400970630     C                   ENDIF
027500970630     C* NUMERO SERIE SPEDIZIONE
027600970630     C     V1CNRD        IFGT      0
027700970630     C     V1CNRA        ORGT      0
027800970630     C     BLPNRS        IFLT      V1CNRD
027900970630     C     BLPNRS        ORGT      V1CNRA
028000970630     C                   GOTO      ENDCON
028100970630     C                   ENDIF
028200970630     C                   ENDIF
028300910205     C*
028400941228     C                   MOVEL     'S'           WSTAMP            1
028500941228     C*
028600941228     C     ENDCON        ENDSR
