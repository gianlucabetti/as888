000100130314     /*PRM dbgview(*source)
000200130314     /*END
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400941228     H* FNLS83R *----------------------------------------------------*
000500970630     H*                  STAMPA  BOLLE NO DDT
000600000000     H*--------------------------------------------------------------*
000700051109     FFNBLP45L  iF   E           K DISK
000800051109     F                                     INFDS(blp1)
000900051109     FFNBLP00f  UF   E             DISK    RENAME(FNblp000:FNblpfis)
001000000511     FAZORG01L  IF   E           K DISK
001100991116      *
001200991118      *  SCHIERE PER COMPORRE BARCODE
001300991118      *
001400051109     D blp1            DS
001500051109     D  blpNRR               397    400B 0
001600941228     D PARAM           DS
001700941228     D  V1CLNA                 1      3  0
001800941228     D  V1CKSC                 4     10  0
001900941228     D  V1CPDR                11     17  0
002000941228     D  V1CSOP                18     18
002100970218     D  V1CLNP                19     21  0
002200970326     D  PARDSD                22     29  0
002300970326     D  PARDSA                30     37  0
002400970630     D  V1CNRD                38     39  0
002500970630     D  V1CNSD                40     46  0
002600970630     D  V1CNRA                47     48  0
002700970630     D  V1CNSA                49     55  0
002800970630     D  V1CRIS                56     56
002900000511     D  V1CDPD                57     57
003000030623     d* - parametri da TRUL90R:
003100030623     d* · pgm.   per stampa LDV
003200030623     d  wD90psl              187    196
003300970326     D                 DS
003400970326     D  BLPAAS                 1      4  0
003500970326     D  BLPMGS                 5      8  0
003600970326     D  BLPDSP                 1      8  0
003700991118      * CALCOLO DEL CHCK DIGIT PER BARCODE DPD
003800991118     D                 DS
003900991118     D  DPDLNP                 1      3  0
004000991118     D  DPDGMS                 4      7  0
004100991118     D  DPDNDS                 8     11  0
004200991118     D  §11                    1     11  0
004300991118     D                                     DIM(11)
004400991118     D  DPDCHD                12     12  0
004500991118     D  DPDBRC                 1     12  0
004600030623      * DS per pgm di STAMPA BOLLE
004700030623     d FNLSB5DS      e ds                  inz
004800130314     d FNLSB6ds1     e ds                  inz
004900130314     d  DB6pdf       e                     inz('F')
005000910205     D KPJBA         E DS
005100000511     D OG143         E DS
005200020610     d ut§dse0f      e ds
005300020610     d cncr80        e ds
005400020610     d trul06ds      e ds
005500020610     d  lin                    1     90  0 dim(30)
005600030623
005700030623     d wPagA4          s              5  0 inz
005800130325     d wPagA5          s              5  0 inz
005900030623
006000910412     C****************************************************************
006100910412     C*  RIEPILOGO INDICATORI
006200910412     C***************************************************************
006300000000     C     *ENTRY        PLIST
006400000000     C                   PARM                    KPJBA
006500020610     C                   MOVEL     KPJBU         PARAM
006600020610
006700020610     c                   eval      codut = 1
006800020610     c                   call      'X§PARUT'
006900020610     c                   parm                    ut§dse0f
007000020610     c                   movel     rec80         cncr80
007100020610
007200020610     c                   if        v1clnp = *zeros
007300020610     c                   clear                   trul06ds
007400020610     c                   eval      d06tla = 'L'
007500020610     c                   eval      d06cod = '£1'
007600020610     c                   movel     simfel        d06key
007700020610     c                   movel     trul06ds      kpjbu
007800020610     c                   call      'TRUL06R'
007900020610     c                   parm                    kpjba
008000020610     c                   movel     kpjbu         trul06ds
008100020610     c                   endif
008200020610
008300970630     C     KBLP          KLIST
008400970630     C                   KFLD                    WFST              1
008500970630     C                   KFLD                    V1CLNA
008600000511     C*
008700000511     C     *LIKE         DEFINE    BLPLNP        SAVLNP
008800900511     C*---------------------------------------------------------------*
008900900517     C*
009000971014     C* AZZERO IL CONTATORE DELLE BOLLE
009100030623     c                   clear                   wPagA4
009200130325     c                   clear                   wPagA5
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
013500030715      * Se ho già stampato 490 bolle:
013600030715if  3c                   IF           wPagA4 >= 490
013700130325     c                             or wPagA5 >= 490
013800030623if  4C     SAVLNA        IFNE      BLPLNA
013900971014     C     V1CLNA        ANDEQ     0
014000971014     C     V1CLNA        ORGT      0
014100030715      *    1) chiedo la chiusura e riapertura dei prtf
014200130314     ***c                   eval      DB0fl3 =  *on
014300130325     c                   eval      DB0fl4 =  *on
014400030715     c                   eval      DB0fl6 =  *on
014500030715     c                   eval      DB0fl7 =  *on
014600130314     ***c                   eval      DB0fl8 =  *on
014700030715      *    2) e riparto con i conteggi
014800030623     c                   clear                   wPagA4
014900130325     c                   clear                   wPagA5
015000030623e   4C                   ENDIF
015100030623e   3C                   ENDIF
015200971014     C**
015300030623     c                   call      wD90psl
015400030623     c                   parm                    FNLSB5DS
015500130314     c                   parm                    FNLSB6ds1
015600030623      *
015700130314      * Aggiorno il conteggio delle pagine stampate (x tipo)
015800130325      * (in realtà ora il modello è unico - A4)
015900130325sel 3c                   select
016000130325      * - formato A5 su stampante laser
016100130325w   3c                   when      DB0tms = '5'  or  DB0tms = 'I'
016200130325     ***c                   add       1             wPagA5
016300130325     c                   add       1             wPagA4
016400130325      * - formato A4 su stampante laser
016500130325w   3c                   other
016600130325     c                   add       1             wPagA4
016700130325e   3c                   endsl
016800971014     C**
016900971014     C                   MOVEL     BLPLNA        SAVLNA            3 0
017000941228     C* AGGIORNO BOLLA
017100970630     C     V1CRIS        IFEQ      'N'
017200051109     c     blpnrr        chain     fnblp00f
017300970213     C                   MOVEL     'Y'           BLPFST
017400051109     C                   UPDATE    FNBLPfis
017500970630     C                   ENDIF
017600941228     C                   ENDIF
017700941228     C**
017800970630     C   02KBLP          READE     FNBLP45L                               31
017900970630     C  N02WFST          READE     FNBLP45L                               31
018000941228     C                   ENDDO
018100941228     C*
018200130314      * Fine
018300130320if  1c                   if        DB6num > *zero
018400130314     c                   clear                   FNLSB5ds
018500130314     c                   eval      DB0tla = 'L'
018600130314     c                   eval      DB0fa4 = 'I'
018700130314     c                   eval      DB6pdf = 'S'
018800130314     c                   call      wD90psl
018900130314     C                   parm                    FNLSB5ds
019000130314     c                   parm                    FNLSB6ds1
019100130314     c                   else
019200130314     c                   clear                   FNLSB5ds
019300130314     c                   eval      DB0tla = 'C'
019400130314     c                   call      wD90psl
019500130314     C                   parm                    FNLSB5ds
019600130314e   1c                   endif
019700130314      *
019800000000     C                   SETON                                        LR
019900910205     C*
020000941228     C* CONFRONTI----------------------------------------------------*
020100941228     C     CONFRO        BEGSR
020200941228     C                   CLEAR                   WSTAMP
020300991012     C* ESCLUDO SEMPRE I PREPAGATI NON CONFERMATI
020400991012     C     BLPPDR        IFEQ      0
020500991012     C                   GOTO      ENDCON
020600991012     C                   ENDIF
020700970218     C* LINEA DI PARTENZA
020800970218     C     V1CLNP        IFGT      0
020900970218     C     V1CLNP        ANDNE     BLPLNP
021000970218     C                   GOTO      ENDCON
021100970218     C                   ENDIF
021200000511     C* CONTROLLO SE RICHIESTE SOLO LE LNP DPD
021300000511    1C     V1CDPD        IFEQ      'D'
021400000511    2C     BLPLNP        IFNE      SAVLNP
021500000511     C                   Z-ADD     BLPLNP        SAVLNP
021600000511     C     BLPLNP        CHAIN     AZORG01L                           30
021700000511     C  N30              MOVEL     ORGDE3        OG143
021800000511     C   30              CLEAR                   OG143
021900000511    2C                   ENDIF
022000020206      **         §OGDPD    IFNE 'S'
022100020206    2C     §OGNTW        IFNE      'DPD'
022200000511     C                   GOTO      ENDCON
022300000511    2C                   ENDIF
022400000511    1C                   ENDIF
022500020610      * controllo che la linea di partenza della bolla faccia parte della £1
022600020610      * se non immesso il p.o. gestione
022700020610     c                   if        v1clnp = *zeros
022800020610     c     blplnp        lookup    lin                                    30
022900020610     c                   if        not *in30
023000020610     c                   goto      endcon
023100020610     c                   endif
023200020610     c                   endif
023300970218     C*
023400941228     C* SIGLA OPERATORE
023500941228     C     V1CSOP        IFNE      *BLANKS
023600941228     C     V1CSOP        ANDNE     BLPSOP
023700941228     C                   GOTO      ENDCON
023800941228     C                   ENDIF
023900941228     C*
024000941228     C* CODICE CLIENTE
024100941228     C     V1CKSC        IFNE      0
024200941228     C     BLPCCM        IFGT      *ZEROS
024300941228     C     BLPCCM        IFNE      V1CKSC
024400941228     C                   GOTO      ENDCON
024500941228     C                   ENDIF
024600941228     C*
024700941228     C                   ELSE
024800941228     C*
024900941228     C     BLPKSC        IFNE      V1CKSC
025000941228     C                   GOTO      ENDCON
025100941228     C                   ENDIF
025200941228     C                   ENDIF
025300941228     C                   ENDIF
025400941228     C* CODICE PADRONCINO
025500941228     C     V1CPDR        IFGT      *ZEROS
025600941228     C     V1CPDR        ANDNE     BLPPDR
025700941228     C                   GOTO      ENDCON
025800941228     C                   ENDIF
025900970326     C* DATA SPEDIZIONE
026000970326     C     PARDSD        IFGT      0
026100970326     C     PARDSA        ANDGT     0
026200970326     C     BLPDSP        IFLT      PARDSD
026300971014     C     BLPDSP        ORGT      PARDSA
026400970326     C                   GOTO      ENDCON
026500970326     C                   ENDIF
026600970326     C                   ENDIF
026700970326     C     PARDSD        IFGT      0
026800970326     C     PARDSA        ANDEQ     0
026900970326     C     BLPDSP        ANDNE     PARDSD
027000970326     C                   GOTO      ENDCON
027100970326     C                   ENDIF
027200970326     C     PARDSD        IFEQ      0
027300970326     C     PARDSA        ANDGT     0
027400970326     C     BLPDSP        ANDGT     PARDSA
027500970326     C                   GOTO      ENDCON
027600970326     C                   ENDIF
027700970630     C* NUMERO SPEDIZIONE
027800970630     C     V1CNSD        IFGT      0
027900970630     C     V1CNSA        ORGT      0
028000970630     C     BLPNSP        IFLT      V1CNSD
028100970630     C     BLPNSP        ORGT      V1CNSA
028200970630     C                   GOTO      ENDCON
028300970630     C                   ENDIF
028400970630     C                   ENDIF
028500970630     C* NUMERO SERIE SPEDIZIONE
028600970630     C     V1CNRD        IFGT      0
028700970630     C     V1CNRA        ORGT      0
028800970630     C     BLPNRS        IFLT      V1CNRD
028900970630     C     BLPNRS        ORGT      V1CNRA
029000970630     C                   GOTO      ENDCON
029100970630     C                   ENDIF
029200970630     C                   ENDIF
029300910205     C*
029400941228     C                   MOVEL     'S'           WSTAMP            1
029500941228     C*
029600941228     C     ENDCON        ENDSR
