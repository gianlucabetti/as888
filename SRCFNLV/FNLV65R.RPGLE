000100970225     H DECEDIT('0,') DATEDIT(*DMY.)
000200970225     H* FNLV65R *-----------------------------------------------------*
000300970225     H*         - REPORT CONTROLLO QUALITA' - FILTRO                  *
000400970225     H*---------------------------------------------------------------*
000500970224     FAZORG02L  IF   E           K DISK
000600970225     FAZORG01L  IF   E           K DISK
000700970225     F                                     RENAME(AZORG:AZORG1)
000800970225     FTABEL00F  IF   E           K DISK
000900970224     FFNLV65D   CF   E             WORKSTN
001000990427     D MSG             S             78    DIM(17) CTDATA PERRCD(1)
001100110125     D CHKO            s             40    dim(1) ctdata  perrcd(1)
001200110125     D*
001300970224     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001400970224     D KPJBA         E DS
001500970224     D CNCR80        E DS
001600000911     D DS5AS1        E DS
001700970224     D WLBDA8          DS
001800970224     D  G08DAT                 1      8  0
001900970224     D  G08INV                 9     16  0
002000970224     D  G08ERR                17     17
002100970224     D  G08TGI                18     22  0
002200970409     D WGIDAT          DS
002300970409     D  GIODAT                 1      8  0
002400970409     D  GIOINV                 9     16  0
002500970409     D  GIOTGI                17     21  0
002600970225     D PARAM           DS
002700970225     D  V1CDST                 1      1
002800970225     D  PARCAR                 2      4  0
002900970225     D  V1CLIN                 5      7  0
003000970225     D  PARDEL                 8     15  0
003100970225     D  V1CDEL                16     23  0
003200990426     D  V1CIND                24     24
003300970407     D  V1CTSP                25     25
003400970526     D  V1CDAL                26     33  0
003500970526     D  PARDAL                34     41  0
003600970527     D  PARTGI                42     46  0
003700990426     D  V1CINA                47     47
003800990426     D  V1CINN                48     48
003900110505     D  wntw                  49     49
004000990427     D  V1CCRP                50     50
004100110505
004200970227      *---------------------------------------------------------------*
004300970227      * INDICATORI USATI
004400970227      *---------------------------------------------------------------*
004500970227      * 05    = OFF - PROGRAMMA RICHIAMATO DA SEDE (PER ORA NON E' GES-
004600970227      *               TITO IL RICHIAMO DA FILIALE)
004700970227      * 28    = ERRORE GENERICO VIDEO
004800970227      * 29    = RIEMETTE FORMATO VIDEO
004900970227      * 30    = DI COMODO
005000970227      * 40    = ERRORE CODICE DISTRETTO
005100970227      * 41    = ERRORE CODICE AREA
005200970227      * 42    = ERRORE PUNTO OPERATIVO
005300970526      * 43    = ERRORE DATA ELABORAZIONE
005400970526      * 45    = ERRORE DATA ELABORAZIONE AL
005500990427      * 46    = ERRORE INVIO NETWORK
005600990427      * 47    = ERRORE CREAZIONE DATI REPORT
005700970227      *---------------------------------------------------------------*
005800970225     C     INIZIO        TAG
005900970224     C* IMPOSTO CAMPI VIDEO
006000970224     C                   MOVE      *BLANKS       V1CDST
006100970224     C                   MOVE      *BLANKS       V1DDST
006200970225     C                   MOVE      *BLANKS       V1CCAR
006300970224     C                   MOVE      *BLANKS       V1DCAR
006400970224     C                   MOVE      *ZEROS        V1CLIN
006500970224     C                   MOVE      *BLANKS       V1DLIN
006600970224     C                   MOVE      *ZEROS        V1CDEL
006700970526     C                   MOVE      *ZEROS        V1CDAL
006800990427     C                   MOVE      'N'           V1CIND
006900990427     C                   MOVE      'N'           V1CINA
007000990427     C                   MOVE      'N'           V1CINN
007100990427     C                   CLEAR                   V1CNTW
007200990427     C                   MOVE      'N'           V1CCRP
007300970407     C                   MOVE      'N'           V1CTSP
007400970224     C     EMD01         TAG
007500970224     C                   WRITE     LV65T01
007600970224     C                   EXFMT     LV65D01
007700970224     C                   SETOFF                                       2829
007800970224     C                   MOVE      *BLANKS       V1ZMSG
007900970224     C     *INKC         CABEQ     '1'           FINE
008000970224     C                   EXSR      CTRD01
008100970224     C   28
008200970224     COR 29
008300970225     CORNKF              GOTO      EMD01
008400990507     C* SOTTOMISSIONE BATCH AGGIORNAMENTO WFILE REPORT QUALITA
008500110125
008600110125     c                   eval      comman=chko(1)
008700110125     c                   call(e)   'QCMDEXC'
008800110125     c                   parm                    comman           80
008900110125     c                   parm      40            lung             15 5
009000110125     c                   if        not %error
009100110125     c                   eval      kqieb='K018SDINO'
009200110125     c                   endif
009300110125
009400990507     C     V1CCRP        IFEQ      'S'
009500990507     C                   MOVEL     'LV64'        KCOAZ
009600990507     C                   CALL      'BCH10'
009700990507     C                   PARM                    KPJBA
009800990507     C                   END
009900110125     c
010000990507     C* SOTTOMISSIONE BATCH CONTROLLO QUALITA'
010100110125     c
010200970225     C                   MOVEL     'LV66'        KCOAZ
010300970225     C                   MOVEL     PARAM         KPJBU
010400970225     C                   CALL      'BCH10'
010500970225     C                   PARM                    KPJBA
010600970409     C*
010700970225     C                   GOTO      INIZIO
010800970224     C*
010900970224     C     FINE          TAG
011000970224     C                   SETON                                        LR
011100970224     C*****************************************************************
011200970224     C* CONTROLLI VIDEO
011300970224     C*****************************************************************
011400970224     C     CTRD01        BEGSR
011500970225     C                   SETOFF                                       404142
011600970526     C                   SETOFF                                       434445
011700990427     C                   SETOFF                                       4647
011800110505     c                   clear                   wntw
011900970224     C* CODICE DISTRETTO
012000970224     C                   CLEAR                   V1DDST
012100970224     C* Gestione ?
012200970224     C     '?'           SCAN      V1CDST                                 30
012300970224    1C     *IN30         IFEQ      *ON
012400970224     C                   MOVEL     CODUT         §KUT
012500970224     C                   MOVEL     '17'          §COD
012600970224     C                   MOVEL     *BLANKS       §KEY
012700970224     C                   CALL      'X§TABER'
012800970224     C                   PARM                    §KUT              1 0
012900970224     C                   PARM                    §COD              2
013000970224     C                   PARM                    §KEY              8
013100970401     C                   PARM                    §DES             25
013200970224     C                   MOVEL     §KEY          V1CDST
013300970224    2C     V1CDST        IFNE      *BLANKS
013400970224     C                   MOVEL     §DES          V1DDST
013500970224    2C                   END
013600970224     C                   SETON                                        29
013700970224     C                   GOTO      ETRD01
013800970224    1C                   END
013900970225     C*
014000970224    1C     V1CDST        IFNE      *BLANKS
014100970225     C* Decodifico
014200970225     C                   EXSR      DECDST
014300970227     C* Non è possibile richiedere il distretto '9' - SEDE
014400970227     C     V1CDST        IFEQ      '9'
014500970227     C                   SETON                                        2840
014600970227     C                   MOVEL     MSG(10)       V1ZMSG
014700970227     C                   GOTO      ETRD01
014800970227     C                   END
014900970225     C* Se immesso deve esistere in organigramma
015000970225     C     V1CDST        CHAIN     AZORG02L                           30
015100970225    2C     *IN30         DOWEQ     *OFF
015200970225     C     ORGFVA        ANDNE     *BLANKS
015300970721     C     V1CDST        READE     AZORG02L                               30
015400970225    2C                   ENDDO
015500970225    2C     *IN30         IFEQ      *ON
015600970224     C                   SETON                                        2840
015700970224     C                   MOVEL     MSG(1)        V1ZMSG
015800970224     C                   GOTO      ETRD01
015900970224    2C                   END
016000970224    1C                   END
016100970225     C* CODICE AREA
016200970224     C                   CLEAR                   V1DCAR
016300970224     C* Gestione ?
016400970224     C     '?'           SCAN      V1CCAR                                 30
016500970224    1C     *IN30         IFEQ      *ON
016600970224     C                   MOVEL     CODUT         §KUT
016700970224     C                   MOVEL     '05'          §COD
016800970224     C                   MOVEL     *BLANKS       §KEY
016900970224     C                   CALL      'X§TABER'
017000970224     C                   PARM                    §KUT              1 0
017100970224     C                   PARM                    §COD              2
017200970224     C                   PARM                    §KEY              8
017300970401     C                   PARM                    §DES             25
017400970224     C                   MOVEL     §KEY          V1CCAR
017500970224    2C     V1CCAR        IFNE      *BLANKS
017600970224     C                   MOVEL     §DES          V1DCAR
017700970224    2C                   END
017800970224     C                   SETON                                        29
017900970224     C                   GOTO      ETRD01
018000970224    1C                   END
018100970225     C*
018200970225     C* Se = blanks come se fosse = 0
018300970225    1C     V1CCAR        IFEQ      *BLANKS
018400970225     C                   MOVE      *ZEROS        PARCAR
018500970225     C                   ELSE
018600970225     C                   MOVE      V1CCAR        PARCAR
018700970225    1C                   END
018800970225     C*
018900970225     C*
019000970225    1C     PARCAR        IFNE      *ZEROS
019100970225     C* Decodifico
019200970225     C                   EXSR      DECCAR
019300970225     C* Se immesso anche il codice distretto controllo che l'area ne
019400970225     C* faccia parte
019500970225    2C     V1CDST        IFNE      *BLANKS
019600970225     C     KORG          CHAIN     AZORG02L                           30
019700970225    3C     *IN30         DOWEQ     *OFF
019800970225     C     ORGFVA        ANDNE     *BLANKS
019900970225     C     KORG          READE     AZORG02L                               30
020000970225    3C                   ENDDO
020100970225    3C     *IN30         IFEQ      *ON
020200970225     C                   SETON                                        4128
020300970225     C                   MOVEL     MSG(3)        V1ZMSG
020400970225     C                   GOTO      ETRD01
020500970225    3C                   ENDIF
020600970225   X2C                   ELSE
020700970225     C* Altrimenti reperisco il codice distretto relativo all'area
020800970225     C     *LOVAL        SETLL     AZORG02L
020900970225     C                   READ      AZORG02L                               30
021000970225    3C     *IN30         DOWEQ     *OFF
021100970225     C     V1CDST        ANDEQ     *BLANKS
021200970225    4C     ORGCAR        IFNE      PARCAR
021300970225     C     KORG1         SETGT     AZORG02L
021400970225   X4C                   ELSE
021500970225    5C     ORGFVA        IFEQ      *BLANKS
021600970227     C* se codice distretto trovato = 9 - SEDE --> errore
021700970227    6C     ORGFL3        IFEQ      '9'
021800970227     C                   SETON                                        2841
021900970227     C                   MOVEL     MSG(11)       V1ZMSG
022000970227     C                   GOTO      ETRD01
022100970227    6C                   END
022200970225     C                   MOVEL     ORGFL3        V1CDST
022300970225     C* Decodifico
022400970225     C                   EXSR      DECDST
022500970225    5C                   END
022600970225    4C                   END
022700970225     C                   READ      AZORG02L                               30
022800970225    3C                   ENDDO
022900970225     C* A fine ciclo, se non ho trovato il distretto, emetto msg errore
023000970225     C* di area inesistente in organigramma
023100970225    3C     V1CDST        IFEQ      *BLANKS
023200970225     C                   SETON                                        2841
023300970225     C                   MOVEL     MSG(2)        V1ZMSG
023400970225     C                   GOTO      ETRD01
023500970225    3C                   END
023600970225    2C                   ENDIF
023700970224     C*
023800970224    1C                   END
023900970225     C* PUNTO OPERATIVO
024000970225     C                   CLEAR                   V1DLIN
024100970225    1C     V1CLIN        IFGT      *ZEROS
024200970227     C* Deve esistere in organigramma e orgfag deve essere <> 'V'
024300970225     C     V1CLIN        CHAIN     AZORG01L                           30
024400970225    2C     *IN30         IFEQ      *OFF
024500970225     C     ORGFVA        ANDEQ     *BLANKS
024600970227     C                   MOVEL     ORGDES        V1DLIN
024700970227     C     ORGFAG        IFEQ      'V'
024800970227     C                   SETON                                        4228
024900970227     C                   MOVEL     MSG(9)        V1ZMSG
025000970227     C                   GOTO      ETRD01
025100970227     C                   END
025200970225   X2C                   ELSE
025300970225     C                   SETON                                        4228
025400970225     C                   MOVEL     MSG(4)        V1ZMSG
025500970225     C                   GOTO      ETRD01
025600970225    2C                   END
025700970225     C* Deve essere congruente con area o distr. eventualmente immessi
025800970225     C     PARCAR        IFNE      *ZEROS
025900970225     C     ORGCAR        ANDNE     PARCAR
026000970225     C                   SETON                                        4228
026100970225     C                   MOVEL     MSG(5)        V1ZMSG
026200970225     C                   GOTO      ETRD01
026300970225     C                   END
026400970225     C     V1CDST        IFNE      *BLANKS
026500970225     C     ORGFL3        ANDNE     V1CDST
026600970225     C                   SETON                                        4228
026700970225     C                   MOVEL     MSG(6)        V1ZMSG
026800970225     C                   GOTO      ETRD01
026900970225     C                   END
027000970227     C* errore se punto operativo fa parte del distretto '9' - SEDE
027100970227     C     ORGFL3        IFEQ      '9'
027200970227     C                   SETON                                        4228
027300970227     C                   MOVEL     MSG(12)       V1ZMSG
027400970227     C                   GOTO      ETRD01
027500970227     C                   END
027600990427     C* Memorizzo se p.o. 'BAR' O 'SDI'
027700990427     C                   MOVEL     ORGDIT        COMDIT
027800970225     C* Imposto il cod.distretto e area se non immessi
027900970225     C     V1CDST        IFEQ      *BLANKS
028000970225     C                   MOVEL     ORGFL3        V1CDST
028100970225     C                   EXSR      DECDST
028200970225     C                   END
028300970225     C     PARCAR        IFEQ      *ZEROS
028400970225     C                   MOVE      ORGCAR        V1CCAR
028500970225     C                   MOVE      ORGCAR        PARCAR
028600970225     C                   EXSR      DECCAR
028700970225     C                   END
028800970225    1C                   END
028900970225     C* DATA ELABORAZIONE
029000970225     C                   CLEAR                   PARDEL
029100970527     C                   CLEAR                   PARTGI
029200970225     C*
029300970225    1C     V1CDEL        IFEQ      *ZEROS
029400970225     C                   SETON                                        4328
029500970225     C                   MOVEL     MSG(7)        V1ZMSG
029600970225     C                   GOTO      ETRD01
029700970225    1C                   END
029800970225     C                   CLEAR                   WLBDA8
029900970225     C                   Z-ADD     V1CDEL        G08DAT
030000970225     C                   CALL      'XSRDA8'
030100970225     C                   PARM                    WLBDA8
030200970227    1C     G08ERR        IFEQ      '1'
030300970225     C                   MOVEL     MSG(8)        V1ZMSG
030400970225     C                   SETON                                        4328
030500970225     C                   GOTO      ETRD01
030600970227    1C                   END
030700970225     C                   MOVE      G08DAT        V1CDEL
030800970225     C                   MOVE      G08INV        PARDEL
030900970527     C                   MOVE      G08TGI        PARTGI
031000970526     C* DATA ELABORAZIONE AL
031100970526     C                   CLEAR                   PARDAL
031200970526     C*
031300970526    1C     V1CDAL        IFGT      *ZEROS
031400970526     C                   CLEAR                   WLBDA8
031500970526     C                   Z-ADD     V1CDAL        G08DAT
031600970526     C                   CALL      'XSRDA8'
031700970526     C                   PARM                    WLBDA8
031800970526    2C     G08ERR        IFEQ      '1'
031900970526     C                   MOVEL     MSG(8)        V1ZMSG
032000970526     C                   SETON                                        4528
032100970526     C                   GOTO      ETRD01
032200970526    2C                   END
032300970526     C                   MOVE      G08DAT        V1CDAL
032400970526     C                   MOVE      G08INV        PARDAL
032500970526     C* ERRORE SE DATA AL < DI DATA DAL
032600970526     C     PARDAL        IFLT      PARDEL
032700970526     C                   MOVEL     MSG(14)       V1ZMSG
032800970526     C                   SETON                                        4528
032900970526     C                   GOTO      ETRD01
033000970526     C                   END
033100970526    1C                   END
033200990427     c* Se immesso invio network, devo impostare B o S
033300990427     C* e quest'ultimo deve essere congruente col punto operativo
033400990427     C* eventualmente richiesto
033500990427    1C     V1CINN        IFEQ      'S'
033600990427    2C     V1CNTW        IFEQ      ' '
033700990427     C                   MOVEL     MSG(15)       V1ZMSG
033800990427     C                   SETON                                        4628
033900990427     C                   GOTO      ETRD01
034000990427    2C                   ENDIF
034100110505      * ricodifico il network così non cambio i controlli che fa dopo
034200110505      * perchè come BAR ci sono anche le filiali estere che come ntw hanno
034300110505      * EEX.
034400110505     c                   IF        V1Cntw = 'C'
034500110505     c                   eval      wntw = 'B'
034600110505     c                   ENDIF
034700110505     c                   IF        V1Cntw = 'M'
034800110505     c                   eval      wntw = 'S'
034900110505     c                   ENDIF
035000990427    2C     V1CLIN        IFGT      *ZEROS
035100110505     C**   COMDIT        ANDNE     V1CNTW
035200110505     c     COMDIT        ANDNE     wntw
035300990427     C                   MOVEL     MSG(17)       V1ZMSG
035400990427     C                   SETON                                        4628
035500990427     C                   GOTO      ETRD01
035600990427    2C                   END
035700990427    1C                   ENDIF
035800990427     c* Se richiesta creazione dati report è necessaria l'esistenza
035900990427     C* dei dati in linea
036000990430     C     V1CCRP        IFEQ      'S'
036100990430     C     PARDEL        ANDLE     PARPUL
036200990427     C                   MOVEL     MSG(16)       V1ZMSG
036300990427     C                   SETON                                        4728
036400990427     C                   GOTO      ETRD01
036500990427     C                   ENDIF
036600970227     C*
036700970224     C     ETRD01        ENDSR
036800970225     C*****************************************************************
036900970225     C* DECODIFICA CODICE DISTRETTO
037000970225     C*****************************************************************
037100970225     C     DECDST        BEGSR
037200970225     C* Chaino tabella 17 per decodifica
037300970225     C                   MOVEL     '17'          KCOD
037400970225     C                   MOVEL(P)  V1CDST        KKEY
037500970225     C     KTAB          CHAIN     TABEL00F                           30
037600970225    2C     *IN30         IFEQ      *OFF
037700970225     C     TBLFLG        ANDEQ     *BLANKS
037800970225     C                   MOVEL     TBLUNI        V1DDST
037900970225    2C                   END
038000970225     C                   ENDSR
038100970225     C*****************************************************************
038200970225     C* DECODIFICA CODICE AREA
038300970225     C*****************************************************************
038400970225     C     DECCAR        BEGSR
038500970225     C* Chaino tabella 05 per decodifica
038600970225     C                   MOVEL     '05'          KCOD
038700970225     C                   MOVEL(P)  PARCAR        KKEY
038800970225     C     KTAB          CHAIN     TABEL00F                           30
038900970225    2C     *IN30         IFEQ      *OFF
039000970225     C     TBLFLG        ANDEQ     *BLANKS
039100970225     C                   MOVEL     TBLUNI        V1DCAR
039200970225    2C                   END
039300970225     C                   ENDSR
039400970409     C*****************************************************************
039500970409     C* REPERIMENTO DATA ULTIMO UTILIZZO PGM PULIZIE
039600970409     C*****************************************************************
039700040924     C     CHKUTI        BEGSR
039800040924     C                   CLEAR                   WDTUTI
039900970409     C*
040000040924     C                   MOVE      *BLANKS       $DAT
040100040924     C                   MOVE      *BLANKS       $ERR
040200040924     C                   CALL      'TRUL49C'                            99
040300040924     C                   PARM                    $PGM
040400040924     C                   PARM                    $DAT              8
040500040924     C                   PARM                    $ERR              1
040600970409      * PROGRAMMA IN USO DATA ULTIMO UTILIZZO = DATA DEL GIORNO
040700040924    1C     $ERR          IFEQ      '1'
040800040924     C     *IN99         OREQ      '1'
040900040924     C     $DAT          OREQ      '00000000'
041000040924     C                   Z-ADD     WDATEU        WDTUTI
041100040924   X1C                   ELSE
041200040924    2C     $DAT          IFNE      *BLANKS
041300040924     C                   MOVE      $DAT          WDTUTI
041400040924    2C                   ENDIF
041500040924    1C                   ENDIF
041600040924     C                   ENDSR
041700970225     C*****************************************************************
041800970224     C* OPERAZIONI INIZIALI
041900970224     C*****************************************************************
042000970224     C     *INZSR        BEGSR
042100970224     C*
042200970224     C     *ENTRY        PLIST
042300970224     C                   PARM                    KPJBA
042400970224     C*
042500970224     C                   Z-ADD     1             CODUT
042600970224     C                   CALL      'X§PARUT'
042700970224     C                   PARM                    UT§DSE
042800970224     C                   MOVEL     REC80         CNCR80
042900970224     C                   MOVEL     RAGUT         RSUT
043000970224     C*
043100970224     C                   TIME                    WTIME            14 0
043200970224     C                   MOVE      WTIME         WDATE             8 0
043300970224     C                   CLEAR                   WLBDA8
043400970224     C                   MOVE      WDATE         G08DAT
043500970224     C                   CALL      'XSRDA8'
043600970224     C                   PARM                    WLBDA8
043700970224     C                   MOVE      G08INV        WDATEU            8 0
043800970409     C*
043900970718     C* CHAINO TABELLA 5A PER REPERIRE GIORNI DI PULIZIA STATISTICHE
044000970718     C                   MOVEL     '5A'          KCOD
044100000911     C                   MOVEL(P)  'SEDE1'       KKEY
044200970409     C     KTAB          CHAIN     TABEL00F                           30
044300000911     C  N30              MOVEL     TBLUNI        DS5AS1
044400000911     C   30              MOVEL     *ZEROS        DS5AS1
044500040924     C* I pgm di pulizia statistiche sono 2 ma siccome entrambe le azio
044600040924     C* ni vengono schedulate per il week-end reperisco solo 1 data ul-
044700040924     C* timo utilizzo
044800040924     C                   MOVEL(P)  'FNLSC7R'     $PGM             10
044900040924     C                   EXSR      CHKUTI
045000040924     C                   CLEAR                   WLBDA8
045100040924     C                   Z-ADD     WDTUTI        G08INV
045200040924     C                   MOVEL     '3'           G08ERR
045300040924     C                   CALL      'XSRDA8'
045400040924     C                   PARM                    WLBDA8
045500970409     C* DETERMINO DATA PER PULIZIA ARCHIVI STATISTICHE
045600040924     C     G08TGI        SUB       §5ASTS        GIOTGI
045700970409     C                   CALL      'XSRGI8'
045800970409     C                   PARM                    WGIDAT
045900970409     C                   Z-ADD     GIOINV        PARPUL
046000040924     C***                  Z-ADDGIOINV    PARPU1
046100970409     C*
046200970224     C* SE NON SONO IN SEDE PROTEGGO IL DISTRETTO
046300970224     C     SIMFEL        COMP      0                                  0505
046400970224     C*
046500970224     C     KTAB          KLIST
046600970224     C                   KFLD                    CODUT
046700970224     C                   KFLD                    KCOD
046800970224     C                   KFLD                    KKEY
046900970225     C*
047000970225     C     KORG          KLIST
047100970225     C                   KFLD                    V1CDST
047200970225     C                   KFLD                    PARCAR
047300970225     C*
047400970225     C     KORG1         KLIST
047500970225     C                   KFLD                    ORGFL3
047600970225     C                   KFLD                    ORGCAR
047700970224     C*
047800970224     C     *LIKE         DEFINE    TBLCOD        KCOD
047900970224     C     *LIKE         DEFINE    TBLKEY        KKEY
048000040924     C     *like         DEFine    WDATEU        WDTUTI
048100990427     C     *LIKE         DEFINE    V1CNTW        COMDIT
048200040924     C     *LIKE         DEFINE    PARDEL        PARPUL
048300970225     C*
048400970224     C                   ENDSR
048500990427**
048600970225Cod.Distretto inesistente in organigramma o annullato                          1
048700970225Cod.Area inesistente in organigramma o annullato                               2
048800970225Il codice area non è congruente col distretto richiesto                        3
048900980407Punto operativo inesistente o annullato                                        4
049000970225Il punto operativo non è congruente con il codice area richiesto               5
049100970225Il punto operativo non è congruente con il codice distretto richiesto          6
049200970526Data elaborazione "dal" obbligatoria                                           7
049300970225Data elaborazione errata                                                       8
049400970227Immettere un punto operativo valido                                            9
049500970227Immettere un codice distretto valido                                          10
049600970227Codice area immesso facente parte di un distretto non valido                  11
049700970227Punto operativo immesso facente parte di un distretto non valido              12
049800970228I suddetti p.o. non hanno eseguito l'IDD nella data richiesta. ENTER PER FORZ.13
049900970526Data iniziale maggiore di quella finale                                       14
050000110505Immettere network: Corriere o Messaggerie                                     15
050100990427Creazione dati REPORT non più effettuabile per il periodo richiesto           16
050200990427Network incongruente con il p.o. scelto                                       17
050300110125**
050400110125CHKOBJ OBJ(K018SDINO) OBJTYPE(*JOBQ)
