000100030116     hDECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300040819      *       prompt di Controllo fatture
000400000000      *****************************************************************
000500070529     FTABEL00F  IF   E           K DISK
000600070530     FAZORG01L  IF   E           K DISK
000700070529     FTise9aD   CF   E             WORKSTN
000800030116      *****************************************************************
000900030116     d KPJBA         E DS
001000030120     D Tibs36ds      E DS
001100000000     D CNCR80        E DS
001200110511     D CNCR42        E DS
001300000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001400070530     D tise9ads      e DS                  inz
001500070529     ddtaiso           s               d
001600070529     ddtaeur           s               d   datfmt(*eur)
001700940926     D*
001800000000     D WLBDAT          DS
001900070530     D  G08DAT                 1      8  0
002000070530     D  G08INV                 9     16  0
002100070530     D  G08ERR                17     17
002200070530     D  G08TGI                18     22  0
002300070605     D*------------------
002400070605     D* DS DATI PER STATISTICHE -DI GRUPPO-
002500070605     D*------------------
002600070605     D DS5S          E DS
002700070605     D*------------------
002800070605     D* DS DATA ULTIMO AGGIORNAMENTO SALDI
002900070605     D*------------------
003000070605     D                 DS
003100070605     D  AGGAA                  1      4  0
003200070605     D  AGGMM                  5      6  0
003300070605     D  AGGAM                  1      6  0
003400070605     D  AGGGG                  7      8  0
003500070605     D  WDAC                   1      8  0
003600030205      *
003700030116     D digits          C                   CONST('0123456789')
003800030116      *****************************************************************
003900000000     C     *ENTRY        PLIST
004000000000     C                   PARM                    KPJBA
004100070529     C                   movel     kpjbu         datora14         14
004200070530     C                   MOVEL     datora14      G08INV
004300070530     C                   Z-ADD     *ZEROS        G08DAT
004400070530     C                   MOVEL     '3'           G08ERR
004500070530     C                   CALL      'XSRDA8'
004600070530     C                   PARM                    WLBDAT
004700070530     c                   move      g08dat        v1edel
004800070529     c                   move      datora14      v1eora
004900070529     c
005000070530     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
005100070529     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
005200070529     C     *LIKE         DEFINE    TBLCOD        KTBCOD
005300070529     C     *LIKE         DEFINE    TBLKEY        KTBKEY
005400070530     C*
005500070530     C* CHIAVE LETTURA AZORG01L - COMPLETA
005600070530     C     KEYORG        KLIST
005700070530     C                   KFLD                    KORFIL
005800070529     C*
005900070529     C* CHIAVE LETTURA TABEL00F - COMPLETA
006000070529     C     KEYTAB        KLIST
006100070529     C                   KFLD                    KTBKUT
006200070529     C                   KFLD                    KTBCOD
006300070529     C                   KFLD                    KTBKEY
006400070529     C*
006500070529     C* CHIAVE LETTURA TABEL00F - PARZIALE
006600070529     C     KEYTA2        KLIST
006700070529     C                   KFLD                    KTBKUT
006800070529     C                   KFLD                    KTBCOD
006900070529     C*
007000030120      *
007100030825     C* Imposta la UDATE  da TIME
007200030825     C                   TIME                    W0140            14 0
007300030825     C                   MOVE      W0140         UDATE8            8 0
007400030825     C*
007500030120      *  In base all'utente prende il P.O.
007600030120     C                   CLEAR                   Tibs36Ds
007700030120     c                   EVAL      I36ute = Knmus
007800030120     c                   EVAL      I36Tla = 'L'
007900030120     C                   CALL      'TIBS36R'
008000030120     C                   PARM                    Tibs36Ds
008100030827     c                   movel     KNMUS         ALFA3             3
008200030829     c                   setoff                                       0102
008300030829      * solo per EDP....:
008400030829     c                   if        ALFA3 ='EDP'
008500030829     c                   seton                                        01
008600030829     c                   end
008700030829      * per utenti di filiale:
008800030829     c                   if        ALFA3<>'EDP' and O36Pou <>46
008900030829     c                   seton                                        02
009000030829     c                   end
009100000000      *--------------------------------------------------------------*
009200000000      *             M A I N      L I N E
009300000000      *--------------------------------------------------------------*
009400000000     C                   Z-ADD     1             CODUT
009500000000     C                   CALL      'X§PARUT'
009600000000     C                   PARM                    UT§DSE
009700110511     C********           MOVEL     RAGUT         DESDIT
009800000000     C                   MOVEL     REC80         CNCR80
009900110511     C                   MOVEL     REC42         CNCR42
010000110511     C                   MOVEL     simRGS        DESDIT
010100040819      *
010200040819     C     INIZIO        tag
010300070529      *   Inizializza video parametri
010400070529     c                   move      'C'           v1CU
010500070529     c                   clear                   v1qua1
010600070529     c                   clear                   v1qua2
010700070529     c                   clear                   v1qua3
010800070529     c                   clear                   v1qua4
010900070529     c                   clear                   v1qua5
011000070529     c                   clear                   v1ioe
011100070529     c                   clear                   v1ntw
011200070601     c                   move      'N'           v1sinN
011300070529     c                   clear                   v1dis
011400070529     c                   clear                   v1area
011500070529     c                   clear                   v1fil
011600070530     c                   clear                   v1ddis
011700070530     c                   clear                   v1dare
011800070530     c                   clear                   v1dfil
011900070605     C*--------------------
012000070605     C                   MOVEL     1             KTBKUT
012100070605     C                   MOVEL     '5S'          KTBCOD
012200070605     C     KEYTA2        CHAIN     TABEL00F                           99
012300070605IF  1C     *IN99         IFEQ      *Off                                         *TABELLA NON TROVATA
012400070605     C                   MOVEL     TBLUNI        DS5S
012500070605     C                   Z-ADD     §5SDAC        WDAC                           *ULTIMO AGGIORNAMENT
012600070605      *
012700070605     C* DATA ULTIMO AGGIORNAMENTO - 1 MESE
012800070605     C                   SUB       1             AGGMM
012900070605     C     AGGMM         IFEQ      *ZEROS                                       *SE GENNAIO
013000070605     C                   Z-ADD     12            AGGMM                          *IMPOSTO DICEMBRE
013100070605     C                   SUB       1             AGGAA                          *DELL'ANNO PRECEDENT
013200070605     C                   ENDIF
013300070605     C*
013400070605E   1C                   Else
013500070605     c                   move      *month        v1ameC
013600070605     c                   move      *year         v1annC
013700070605E   1C                   ENDIF
013800070605     C*
013900070605     c                   move      aggaa         v1annC
014000070529     c                   move      'N'           v1sinC
014100070529     c                   move      1             v1dmeC
014200070605     c                   move      AGGMM         v1ameC
014300030915      *
014400070605     c     v1annC        sub       1             v1annP
014500070529     c                   move      'N'           v1sinP
014600070607     c                   move      'S'           v1anno
014700070605     c                   move      'S'           v1snaP
014800070529     c                   move      1             v1dmeP
014900070605     c                   move      12            v1ameP
015000070529      *
015100000000      * Emetto il formato video
015200030116     C     Video1        TAG
015300070605     C                   SETOFF                                       99
015400070529     C                   EXFMT     SE9AD01
015500000000      *
015600000000     C   KC              GOTO      FINE                                         F3=USCITA
015700000000      *
015800000000      * Eseguo i  controlli sui campi immessi a video
015900000000     C                   EXSR      CONTR
016000040830      *
016100040830      * Se non vi sono errori ma non e' premuto F6 emetto video
016200070605     C   99
016300040830     CorNkf              GOTO      Video1
016400030905      *
016500070529     c                   clear                   tise9ads
016600030905      * compone il codice numerico
016700070529     c                   move      V1CU          D9ACU
016800070529     c                   move      V1QUA1        D9AQUA1
016900070529     c                   move      V1QUA2        D9AQUA2
017000070529     c                   move      V1QUA3        D9AQUA3
017100070529     c                   move      V1QUA4        D9AQUA4
017200070529     c                   move      V1QUA5        D9AQUA5
017300070601     C                   move      V1IOE         D9AIOE
017400070601     C                   move      V1NTW         D9ANTW
017500070601     C                   move      V1SINN        D9AntwSIN
017600070601     C                   move      V1DIS         D9ADIS
017700070529     C                   move      V1AREA        D9AAREA
017800070529     C                   move      V1FIL         D9AFIL
017900070529     C                   move      V1ANNC        D9AANNOC
018000070529     C                   move      V1DMEC        D9ADMESC
018100070529     C                   move      V1AMEC        D9AAMESC
018200070529     C                   move      V1SINC        D9ASINAC
018300070529     C                   move      V1ANNP        D9AANNOP
018400070529     C                   move      V1DMEP        D9ADMESP
018500070529     C                   move      V1AMEP        D9AAMESP
018600070529     C                   move      V1SINP        D9ASINAP
018700070531     C                   move      V1edel        D9Adel
018800070531     C                   move      V1eora        D9Aora
018900040819      *
019000040819      *  richiama il pgm che esegue il consolidamento
019100070529     c                   movel     tise9ads      kpjbu
019200070530     c                   call      'TISE9AC1'
019300040826     c                   parm                    kpjba
019400000000      *
019500030116     C                   GOTO      INIZIO
019600030116      *
019700000000     C     FINE          TAG
019800000000     C                   SETON                                        LR
019900000000      *--------------------------------------------------------------*
020000000000      * Routine per i controlli sui campi del formato video          *
020100000000      *--------------------------------------------------------------*
020200000000     C     CONTR         BEGSR
020300000000      *
020400030116     c                   movea     '0000000000'  *in(41)
020500040819     c                   movea     '0000000000'  *in(51)
020600070530     c                   clear                   v1ddis
020700070530     c                   clear                   v1dare
020800070530     c                   clear                   v1dfil
020900070529      *
021000070605     C**** v1cu          ifeq      'C'
021100070605     C****               clear                   v1qua1
021200070605     C*****              clear                   v1qua2
021300070605     C*****              clear                   v1qua3
021400070605     C*****              clear                   v1qua4
021500070605     C*****              clear                   v1qua5
021600070605     C*****              End
021700070605     C*
021800070605     C*  solo x unificante può essere richiesta la qualità
021900070605     C                   if        v1cu  = 'C' and
022000070605     C                             (v1qua1 <> ' ' or v1qua2 <> ' '  or
022100070605     C                              v1qua3 <> ' ' or v1qua4 <> ' '  or
022200070605     C                              v1qua5 <> ' ')
022300070605     C                   seton                                        4799
022400070605     C                   End
022500070605     C*****
022600070530     C* CONTROLLO SE E' STATO INSERITO '?'
022700070530     C     '?'           SCAN      v1qua1                                 21
022800070530     C*
022900070530     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROGAZIONE TABELLE
023000070530IF  1C     *IN21         IFEQ      '1'
023100070605     c                   seton                                        99
023200070530     C                   Z-ADD     1             §KUT
023300070530     C                   MOVEL     'IC'          §COD
023400070530     C                   MOVEL     *BLANKS       §KEY
023500070530     C                   MOVEL     *BLANKS       §DES
023600070530     C                   CALL      'X§TABER'
023700070530     C                   PARM                    §KUT              1 0
023800070530     C                   PARM                    §COD              2
023900070530     C                   PARM                    §KEY              8
024000070530     C                   PARM                    §DES             30
024100070530     C                   MOVEL     §KEY          v1qua1
024200070530X   1C                   ELSE
024300070530     C*
024400070530     C* CONTROLLO CHE L'AREA ESISTA IN TABELLA
024500070530     C                   Z-ADD     1             KTBKUT
024600070530     C                   MOVEL     'IC'          KTBCOD
024700070530     C                   MOVEL     *BLANKS       KTBKEY
024800070530     C                   MOVEL     v1qua1        KTBKEY
024900070530     C     KEYTAB        CHAIN     TABEL00F
025000070530IF  2C                   IF        not %Found(TABEL00F) or TBLFLG = '*'
025100070530E   2C                   ENDIF
025200070530E   1C                   ENDIF
025300070529      *
025400070530     C* CONTROLLO SE E' STATO INSERITO '?'
025500070530     C     '?'           SCAN      v1qua2                                 21
025600070530     C*
025700070530     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROGAZIONE TABELLE
025800070530IF  1C     *IN21         IFEQ      '1'
025900070605     c                   seton                                        99
026000070530     C                   Z-ADD     1             §KUT
026100070530     C                   MOVEL     'IC'          §COD
026200070530     C                   MOVEL     *BLANKS       §KEY
026300070530     C                   MOVEL     *BLANKS       §DES
026400070530     C                   CALL      'X§TABER'
026500070530     C                   PARM                    §KUT              1 0
026600070530     C                   PARM                    §COD              2
026700070530     C                   PARM                    §KEY              8
026800070530     C                   PARM                    §DES             30
026900070530     C                   MOVEL     §KEY          v1qua2
027000070530X   1C                   ELSE
027100070530     C*
027200070530     C* CONTROLLO CHE L'AREA ESISTA IN TABELLA
027300070530     C                   Z-ADD     1             KTBKUT
027400070530     C                   MOVEL     'IC'          KTBCOD
027500070530     C                   MOVEL     *BLANKS       KTBKEY
027600070530     C                   MOVEL     v1qua2        KTBKEY
027700070530     C     KEYTAB        CHAIN     TABEL00F
027800070530IF  2C                   IF        not %Found(TABEL00F) or TBLFLG = '*'
027900070530E   2C                   ENDIF
028000070530E   1C                   ENDIF
028100070530      *
028200070530     C* CONTROLLO SE E' STATO INSERITO '?'
028300070530     C     '?'           SCAN      v1qua3                                 21
028400070530     C*
028500070530     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROGAZIONE TABELLE
028600070530IF  1C     *IN21         IFEQ      '1'
028700070605     c                   seton                                        99
028800070530     C                   Z-ADD     1             §KUT
028900070530     C                   MOVEL     'IC'          §COD
029000070530     C                   MOVEL     *BLANKS       §KEY
029100070530     C                   MOVEL     *BLANKS       §DES
029200070530     C                   CALL      'X§TABER'
029300070530     C                   PARM                    §KUT              1 0
029400070530     C                   PARM                    §COD              2
029500070530     C                   PARM                    §KEY              8
029600070530     C                   PARM                    §DES             30
029700070530     C                   MOVEL     §KEY          v1qua3
029800070530X   1C                   ELSE
029900070530     C*
030000070530     C* CONTROLLO CHE L'AREA ESISTA IN TABELLA
030100070530     C                   Z-ADD     1             KTBKUT
030200070530     C                   MOVEL     'IC'          KTBCOD
030300070530     C                   MOVEL     *BLANKS       KTBKEY
030400070530     C                   MOVEL     v1qua3        KTBKEY
030500070530     C     KEYTAB        CHAIN     TABEL00F
030600070530IF  2C                   IF        not %Found(TABEL00F) or TBLFLG = '*'
030700070530E   2C                   ENDIF
030800070530E   1C                   ENDIF
030900070530      *
031000070530     C* CONTROLLO SE E' STATO INSERITO '?'
031100070530     C     '?'           SCAN      v1qua4                                 21
031200070530     C*
031300070530     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROGAZIONE TABELLE
031400070530IF  1C     *IN21         IFEQ      '1'
031500070605     c                   seton                                        99
031600070530     C                   Z-ADD     1             §KUT
031700070530     C                   MOVEL     'IC'          §COD
031800070530     C                   MOVEL     *BLANKS       §KEY
031900070530     C                   MOVEL     *BLANKS       §DES
032000070530     C                   CALL      'X§TABER'
032100070530     C                   PARM                    §KUT              1 0
032200070530     C                   PARM                    §COD              2
032300070530     C                   PARM                    §KEY              8
032400070530     C                   PARM                    §DES             30
032500070530     C                   MOVEL     §KEY          v1qua4
032600070530X   1C                   ELSE
032700070530     C*
032800070530     C* CONTROLLO CHE L'AREA ESISTA IN TABELLA
032900070530     C                   Z-ADD     1             KTBKUT
033000070530     C                   MOVEL     'IC'          KTBCOD
033100070530     C                   MOVEL     *BLANKS       KTBKEY
033200070530     C                   MOVEL     v1qua4        KTBKEY
033300070530     C     KEYTAB        CHAIN     TABEL00F
033400070530IF  2C                   IF        not %Found(TABEL00F) or TBLFLG = '*'
033500070530E   2C                   ENDIF
033600070530E   1C                   ENDIF
033700070530      *
033800070530     C* CONTROLLO SE E' STATO INSERITO '?'
033900070530     C     '?'           SCAN      v1qua5                                 21
034000070530     C*
034100070530     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROGAZIONE TABELLE
034200070530IF  1C     *IN21         IFEQ      '1'
034300070605     c                   seton                                        99
034400070530     C                   Z-ADD     1             §KUT
034500070530     C                   MOVEL     'IC'          §COD
034600070530     C                   MOVEL     *BLANKS       §KEY
034700070530     C                   MOVEL     *BLANKS       §DES
034800070530     C                   CALL      'X§TABER'
034900070530     C                   PARM                    §KUT              1 0
035000070530     C                   PARM                    §COD              2
035100070530     C                   PARM                    §KEY              8
035200070530     C                   PARM                    §DES             30
035300070530     C                   MOVEL     §KEY          v1qua5
035400070530X   1C                   ELSE
035500070530     C*
035600070530     C* CONTROLLO CHE L'AREA ESISTA IN TABELLA
035700070530     C                   Z-ADD     1             KTBKUT
035800070530     C                   MOVEL     'IC'          KTBCOD
035900070530     C                   MOVEL     *BLANKS       KTBKEY
036000070530     C                   MOVEL     v1qua5        KTBKEY
036100070530     C     KEYTAB        CHAIN     TABEL00F
036200070530IF  2C                   IF        not %Found(TABEL00F) or TBLFLG = '*'
036300070530E   2C                   ENDIF
036400070530E   1C                   ENDIF
036500070530      *
036600070605     C***  v1ioe         ifeq      'I'
036700070605     C***                clear                   v1ntw
036800070605     C***                End
036900070529      *
037000070605     C**** v1ntw         ifne      *blank
037100070605     C****               move      'E'           v1ioe
037200070605     C****               End
037300070605      *
037400070605     C     v1ioe         ifne      'E'
037500070605     C     v1ntw         andne     *blank
037600070605     C                   seton                                        4999
037700070605     C                   End
037800070530     C*--------------------
037900070530     C* Solo una scelta fra Distretto Area e Filiale
038000070530     C*--------------------
038100070530     c                   if        v1DIS <> ' ' and v1AREA <> ' ' or
038200070530     c                             v1DIS <> ' ' and v1FIL  <> ' ' or
038300070530     c                             v1FIL <> ' ' and v1AREA <> ' '
038400070530     C                   seton                                        4899
038500070530     c                   end
038600070530     C*--------------------
038700070530     C* Distretto
038800070530     C*--------------------
038900070530IF  2C     V1dis         IFNE      *BLANKS                                      *SELEZIONATA
039000070530     C*
039100070530     C* CONTROLLO SE E' STATO INSERITO '?'
039200070530     C     '?'           SCAN      v1dis                                  21
039300070530     C*
039400070530     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROGAZIONE DIVISIONI
039500070530IF  1C     *IN21         IFEQ      *ON
039600070605     C                   seton                                          99
039700070530     C                   Z-ADD     1             §KUT
039800070530     C                   MOVEL     '17'          §COD
039900070530     C                   MOVEL     *BLANKS       §KEY
040000070530     C                   MOVEL     *BLANKS       §DES
040100070530     C                   CALL      'X§TABER'
040200070530     C                   PARM                    §KUT
040300070530     C                   PARM                    §COD
040400070530     C                   PARM                    §KEY
040500070530     C                   PARM                    §DES
040600070530     C*
040700070530     C                   MOVEL     §KEY          v1dis
040800070530     C                   MOVEL     §DES          v1ddis
040900070530     c                   seton                                        99
041000070530X   1C                   ELSE
041100070530     C*
041200070530     C* CONTROLLO CHE LA DIVISIONE ESISTA IN TABELLA
041300070530     C                   Z-ADD     1             KTBKUT
041400070530     C                   MOVEL     '17'          KTBCOD
041500070530     C                   MOVEL     *BLANKS       KTBKEY
041600070530     C                   MOVEL     v1dis         KTBKEY
041700070530     C     KeyTAB        CHAIN     TABEL00F
041800070530IF  2C                   IF        not %Found(TABEL00F) or TBLFLG = '*'
041900070530     C                   seton                                        4199
042000070530     c                   else
042100070530     c                   eval      v1ddis = %subst(tblUNI:1:25)
042200070530E   2C                   ENDIF
042300070530E   1C                   ENDIF
042400070530     C*
042500070530E   2C                   ENDIF
042600070530     C*--------------------
042700070530     C* AREA
042800070530     C*--------------------
042900070530IF  2C     v1area        IFNE      *BLANKS                                      *SELEZIONATA
043000070530     C*
043100070530     C* CONTROLLO SE E' STATO INSERITO '?'
043200070530     C     '?'           SCAN      v1area                                 21
043300070530     C*
043400070530     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROGAZIONE AREE
043500070530IF  1C     *IN21         IFEQ      *on
043600070605     C                   seton                                          99
043700070530     C                   Z-ADD     1             §KUT
043800070530     C                   MOVEL     '05'          §COD
043900070530     C                   MOVEL     *BLANKS       §KEY
044000070530     C                   MOVEL     *BLANKS       §DES
044100070530     C                   CALL      'X§TABER'
044200070530     C                   PARM                    §KUT              1 0
044300070530     C                   PARM                    §COD              2
044400070530     C                   PARM                    §KEY              8
044500070530     C                   PARM                    §DES             30
044600070530     C*
044700070530     C                   MOVEL     §KEY          v1area
044800070530     C                   MOVEL     §DES          v1dare
044900070530     c                   seton                                        99
045000070530X   1C                   ELSE
045100070530     C*
045200070530     C* CARATTERI ALFABETICI NON VALIDI
045300070530IF  2C     v1area        IFLT      *ZEROS
045400070530     C                   seton                                        4299
045500070530X   2C                   ELSE
045600070530     C*
045700070530     C* CONTROLLO CHE L'AREA ESISTA IN TABELLA
045800070530     C                   Z-ADD     1             KTBKUT
045900070530     C                   MOVEL     '05'          KTBCOD
046000070530     C                   MOVEL     *BLANKS       KTBKEY
046100070530     C                   MOVEL     v1area        KTBKEY
046200070530     C     KeyTAB        CHAIN     TABEL00F
046300070530IF  2C                   IF        not %Found(TABEL00F) or TBLFLG = '*'
046400070530     C                   seton                                        4299
046500070530     c                   else
046600070530     c                   eval      v1dare = %subst(tblUNI:1:25)
046700070530E   2C                   ENDIF
046800070530E   2C                   ENDIF
046900070530     C*
047000070530E   1C                   ENDIF
047100070530     C*
047200070530E   2C                   ENDIF
047300070530     C*--------------------
047400070530     C* FILIALE
047500070530     C*--------------------
047600070530IF  2C     v1fil         IFNE      *BLANKS                                      *SELEZIONATA
047700070530     C*
047800070530     C* CONTROLLO SE E' STATO INSERITO '?'
047900070530     C     '?'           SCAN      v1fil                                  21
048000070530     C*
048100070530     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROGAZIONE FILIALI
048200070530IF  1C     *IN21         IFEQ      '1'
048300070605     C                   seton                                          99
048400070530     C                   MOVEL     *BLANKS       §COD1
048500070530     C                   MOVE      *BLANKS       §DES1
048600070530     C                   MOVE      *BLANKS       §PRA
048700070530     C                   CALL      'TNSD24R'
048800070530     C                   PARM                    §COD1             3
048900070530     C                   PARM                    §TIP              1
049000070530     C                   PARM                    §DES1            25
049100070530     C                   PARM                    §PRA              3
049200070530     C*
049300070530     C                   MOVEL     §COD1         v1fil
049400070530     C                   MOVEL     §DES1         v1dfil
049500070530     c                   seton                                        99
049600070530X   1C                   ELSE
049700070530     C*
049800070530     C* CARATTERI ALFABETICI NON VALIDI
049900070530IF  2C     v1fil         IFLT      *ZEROS
050000070530     C                   seton                                        4399
050100070530X   2C                   ELSE
050200070530     C*
050300070530     C* CONTROLLO CHE LA FILIALE ESISTA IN ARCHIVIO
050400070530     C                   MOVEL     v1fil         KORFIL
050500070530     C     KEYORG        CHAIN     AZORG01L                           99
050600070530IF  3C     *IN99         IFEQ      '1'
050700070530     C     ORGFAG        OREQ      'V'
050800070530     C                   seton                                        4399
050900070530     c                   else
051000070530     c                   movel     orgdes        v1dfil
051100070530E   3C                   ENDIF
051200070530E   2C                   ENDIF
051300070530E   1C                   ENDIF
051400070530     C*
051500070530E   2C                   ENDIF
051600070530     C*
051700070529     C*--------------------
051800070530     C* Ordinamento date
051900070530     C*--------------------
052000070530     c                   if        v1dmec > v1amec
052100070530     c                   seton                                        4499
052200070530     c                   end
052300040819      *
052400070530     c                   if        v1dmep > v1amep
052500070530     c                   seton                                        4599
052600070530     c                   end
052700070530      *
052800070530     c                   if        v1dmec =0 and v1amec = 0 and
052900070530     c                             v1dmep =0 and v1amep = 0
053000070530     c                   seton                                        4699
053100070530     c                   end
053200070605      *
053300070605      *  Anno in corso Sintetica e precedente in dettaglio : NO
053400070605     c                   if        v1sinp = 'N' and v1sinc = 'S' and
053500070605     c                             v1snap = 'S'
053600070605     c                   seton                                        5099
053700070605     c                   end
053800070605      *
053900070607     c                   if        v1anno = 'N' and v1amep = 12
054000070607     c                   eval      v1dmep = v1dmec
054100070607     c                   eval      v1amep = v1amec
054200070607     c                   seton                                          99
054300070607     c                   end
054400070607      *
054500070608     c                   if        v1anno = 'S' and v1amep <> 12  and
054600070608     c                             v1snaP = 'S'
054700070607     c                   eval      v1dmep = 1
054800070607     c                   eval      v1amep = 12
054900070607     c                   seton                                          99
055000070607     c                   end
055100070608      *
055200070608      *   NOn voglio elaborare l'anno precedente
055300070608     c                   if        v1snaP = 'N'
055400070608     c                   eval      v1dmeP = 0
055500070608     c                   eval      v1ameP = 0
055600070608     c                   end
055700070607      *
055800070530     C*--------------------
055900030116     C     endct1        ENDSR
056000070529     C*------------------------------------------------------------------------*
