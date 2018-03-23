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
003900990427     D  V1CNTW                49     49
004000990427     D  V1CCRP                50     50
004100970227      *---------------------------------------------------------------*
004200970227      * INDICATORI USATI
004300970227      *---------------------------------------------------------------*
004400970227      * 05    = OFF - PROGRAMMA RICHIAMATO DA SEDE (PER ORA NON E' GES-
004500970227      *               TITO IL RICHIAMO DA FILIALE)
004600970227      * 28    = ERRORE GENERICO VIDEO
004700970227      * 29    = RIEMETTE FORMATO VIDEO
004800970227      * 30    = DI COMODO
004900970227      * 40    = ERRORE CODICE DISTRETTO
005000970227      * 41    = ERRORE CODICE AREA
005100970227      * 42    = ERRORE PUNTO OPERATIVO
005200970526      * 43    = ERRORE DATA ELABORAZIONE
005300970526      * 45    = ERRORE DATA ELABORAZIONE AL
005400990427      * 46    = ERRORE INVIO NETWORK
005500990427      * 47    = ERRORE CREAZIONE DATI REPORT
005600970227      *---------------------------------------------------------------*
005700970225     C     INIZIO        TAG
005800970224     C* IMPOSTO CAMPI VIDEO
005900970224     C                   MOVE      *BLANKS       V1CDST
006000970224     C                   MOVE      *BLANKS       V1DDST
006100970225     C                   MOVE      *BLANKS       V1CCAR
006200970224     C                   MOVE      *BLANKS       V1DCAR
006300970224     C                   MOVE      *ZEROS        V1CLIN
006400970224     C                   MOVE      *BLANKS       V1DLIN
006500970224     C                   MOVE      *ZEROS        V1CDEL
006600970526     C                   MOVE      *ZEROS        V1CDAL
006700990427     C                   MOVE      'N'           V1CIND
006800990427     C                   MOVE      'N'           V1CINA
006900990427     C                   MOVE      'N'           V1CINN
007000990427     C                   CLEAR                   V1CNTW
007100990427     C                   MOVE      'N'           V1CCRP
007200970407     C                   MOVE      'N'           V1CTSP
007300970224     C     EMD01         TAG
007400970224     C                   WRITE     LV65T01
007500970224     C                   EXFMT     LV65D01
007600970224     C                   SETOFF                                       2829
007700970224     C                   MOVE      *BLANKS       V1ZMSG
007800970224     C     *INKC         CABEQ     '1'           FINE
007900970224     C                   EXSR      CTRD01
008000970224     C   28
008100970224     COR 29
008200970225     CORNKF              GOTO      EMD01
008300990507     C* SOTTOMISSIONE BATCH AGGIORNAMENTO WFILE REPORT QUALITA
008400110125
008500110125     c                   eval      comman=chko(1)
008600110125     c                   call(e)   'QCMDEXC'
008700110125     c                   parm                    comman           80
008800110125     c                   parm      40            lung             15 5
008900110125     c                   if        not %error
009000110125     c                   eval      kqieb='K018SDINO'
009100110125     c                   endif
009200110125
009300990507     C     V1CCRP        IFEQ      'S'
009400990507     C                   MOVEL     'LV64'        KCOAZ
009500990507     C                   CALL      'BCH10'
009600990507     C                   PARM                    KPJBA
009700990507     C                   END
009800110125     c
009900990507     C* SOTTOMISSIONE BATCH CONTROLLO QUALITA'
010000110125     c
010100970225     C                   MOVEL     'LV66'        KCOAZ
010200970225     C                   MOVEL     PARAM         KPJBU
010300970225     C                   CALL      'BCH10'
010400970225     C                   PARM                    KPJBA
010500970409     C*
010600970225     C                   GOTO      INIZIO
010700970224     C*
010800970224     C     FINE          TAG
010900970224     C                   SETON                                        LR
011000970224     C*****************************************************************
011100970224     C* CONTROLLI VIDEO
011200970224     C*****************************************************************
011300970224     C     CTRD01        BEGSR
011400970225     C                   SETOFF                                       404142
011500970526     C                   SETOFF                                       434445
011600990427     C                   SETOFF                                       4647
011700970224     C* CODICE DISTRETTO
011800970224     C                   CLEAR                   V1DDST
011900970224     C* Gestione ?
012000970224     C     '?'           SCAN      V1CDST                                 30
012100970224    1C     *IN30         IFEQ      *ON
012200970224     C                   MOVEL     CODUT         §KUT
012300970224     C                   MOVEL     '17'          §COD
012400970224     C                   MOVEL     *BLANKS       §KEY
012500970224     C                   CALL      'X§TABER'
012600970224     C                   PARM                    §KUT              1 0
012700970224     C                   PARM                    §COD              2
012800970224     C                   PARM                    §KEY              8
012900970401     C                   PARM                    §DES             25
013000970224     C                   MOVEL     §KEY          V1CDST
013100970224    2C     V1CDST        IFNE      *BLANKS
013200970224     C                   MOVEL     §DES          V1DDST
013300970224    2C                   END
013400970224     C                   SETON                                        29
013500970224     C                   GOTO      ETRD01
013600970224    1C                   END
013700970225     C*
013800970224    1C     V1CDST        IFNE      *BLANKS
013900970225     C* Decodifico
014000970225     C                   EXSR      DECDST
014100970227     C* Non è possibile richiedere il distretto '9' - SEDE
014200970227     C     V1CDST        IFEQ      '9'
014300970227     C                   SETON                                        2840
014400970227     C                   MOVEL     MSG(10)       V1ZMSG
014500970227     C                   GOTO      ETRD01
014600970227     C                   END
014700970225     C* Se immesso deve esistere in organigramma
014800970225     C     V1CDST        CHAIN     AZORG02L                           30
014900970225    2C     *IN30         DOWEQ     *OFF
015000970225     C     ORGFVA        ANDNE     *BLANKS
015100970721     C     V1CDST        READE     AZORG02L                               30
015200970225    2C                   ENDDO
015300970225    2C     *IN30         IFEQ      *ON
015400970224     C                   SETON                                        2840
015500970224     C                   MOVEL     MSG(1)        V1ZMSG
015600970224     C                   GOTO      ETRD01
015700970224    2C                   END
015800970224    1C                   END
015900970225     C* CODICE AREA
016000970224     C                   CLEAR                   V1DCAR
016100970224     C* Gestione ?
016200970224     C     '?'           SCAN      V1CCAR                                 30
016300970224    1C     *IN30         IFEQ      *ON
016400970224     C                   MOVEL     CODUT         §KUT
016500970224     C                   MOVEL     '05'          §COD
016600970224     C                   MOVEL     *BLANKS       §KEY
016700970224     C                   CALL      'X§TABER'
016800970224     C                   PARM                    §KUT              1 0
016900970224     C                   PARM                    §COD              2
017000970224     C                   PARM                    §KEY              8
017100970401     C                   PARM                    §DES             25
017200970224     C                   MOVEL     §KEY          V1CCAR
017300970224    2C     V1CCAR        IFNE      *BLANKS
017400970224     C                   MOVEL     §DES          V1DCAR
017500970224    2C                   END
017600970224     C                   SETON                                        29
017700970224     C                   GOTO      ETRD01
017800970224    1C                   END
017900970225     C*
018000970225     C* Se = blanks come se fosse = 0
018100970225    1C     V1CCAR        IFEQ      *BLANKS
018200970225     C                   MOVE      *ZEROS        PARCAR
018300970225     C                   ELSE
018400970225     C                   MOVE      V1CCAR        PARCAR
018500970225    1C                   END
018600970225     C*
018700970225     C*
018800970225    1C     PARCAR        IFNE      *ZEROS
018900970225     C* Decodifico
019000970225     C                   EXSR      DECCAR
019100970225     C* Se immesso anche il codice distretto controllo che l'area ne
019200970225     C* faccia parte
019300970225    2C     V1CDST        IFNE      *BLANKS
019400970225     C     KORG          CHAIN     AZORG02L                           30
019500970225    3C     *IN30         DOWEQ     *OFF
019600970225     C     ORGFVA        ANDNE     *BLANKS
019700970225     C     KORG          READE     AZORG02L                               30
019800970225    3C                   ENDDO
019900970225    3C     *IN30         IFEQ      *ON
020000970225     C                   SETON                                        4128
020100970225     C                   MOVEL     MSG(3)        V1ZMSG
020200970225     C                   GOTO      ETRD01
020300970225    3C                   ENDIF
020400970225   X2C                   ELSE
020500970225     C* Altrimenti reperisco il codice distretto relativo all'area
020600970225     C     *LOVAL        SETLL     AZORG02L
020700970225     C                   READ      AZORG02L                               30
020800970225    3C     *IN30         DOWEQ     *OFF
020900970225     C     V1CDST        ANDEQ     *BLANKS
021000970225    4C     ORGCAR        IFNE      PARCAR
021100970225     C     KORG1         SETGT     AZORG02L
021200970225   X4C                   ELSE
021300970225    5C     ORGFVA        IFEQ      *BLANKS
021400970227     C* se codice distretto trovato = 9 - SEDE --> errore
021500970227    6C     ORGFL3        IFEQ      '9'
021600970227     C                   SETON                                        2841
021700970227     C                   MOVEL     MSG(11)       V1ZMSG
021800970227     C                   GOTO      ETRD01
021900970227    6C                   END
022000970225     C                   MOVEL     ORGFL3        V1CDST
022100970225     C* Decodifico
022200970225     C                   EXSR      DECDST
022300970225    5C                   END
022400970225    4C                   END
022500970225     C                   READ      AZORG02L                               30
022600970225    3C                   ENDDO
022700970225     C* A fine ciclo, se non ho trovato il distretto, emetto msg errore
022800970225     C* di area inesistente in organigramma
022900970225    3C     V1CDST        IFEQ      *BLANKS
023000970225     C                   SETON                                        2841
023100970225     C                   MOVEL     MSG(2)        V1ZMSG
023200970225     C                   GOTO      ETRD01
023300970225    3C                   END
023400970225    2C                   ENDIF
023500970224     C*
023600970224    1C                   END
023700970225     C* PUNTO OPERATIVO
023800970225     C                   CLEAR                   V1DLIN
023900970225    1C     V1CLIN        IFGT      *ZEROS
024000970227     C* Deve esistere in organigramma e orgfag deve essere <> 'V'
024100970225     C     V1CLIN        CHAIN     AZORG01L                           30
024200970225    2C     *IN30         IFEQ      *OFF
024300970225     C     ORGFVA        ANDEQ     *BLANKS
024400970227     C                   MOVEL     ORGDES        V1DLIN
024500970227     C     ORGFAG        IFEQ      'V'
024600970227     C                   SETON                                        4228
024700970227     C                   MOVEL     MSG(9)        V1ZMSG
024800970227     C                   GOTO      ETRD01
024900970227     C                   END
025000970225   X2C                   ELSE
025100970225     C                   SETON                                        4228
025200970225     C                   MOVEL     MSG(4)        V1ZMSG
025300970225     C                   GOTO      ETRD01
025400970225    2C                   END
025500970225     C* Deve essere congruente con area o distr. eventualmente immessi
025600970225     C     PARCAR        IFNE      *ZEROS
025700970225     C     ORGCAR        ANDNE     PARCAR
025800970225     C                   SETON                                        4228
025900970225     C                   MOVEL     MSG(5)        V1ZMSG
026000970225     C                   GOTO      ETRD01
026100970225     C                   END
026200970225     C     V1CDST        IFNE      *BLANKS
026300970225     C     ORGFL3        ANDNE     V1CDST
026400970225     C                   SETON                                        4228
026500970225     C                   MOVEL     MSG(6)        V1ZMSG
026600970225     C                   GOTO      ETRD01
026700970225     C                   END
026800970227     C* errore se punto operativo fa parte del distretto '9' - SEDE
026900970227     C     ORGFL3        IFEQ      '9'
027000970227     C                   SETON                                        4228
027100970227     C                   MOVEL     MSG(12)       V1ZMSG
027200970227     C                   GOTO      ETRD01
027300970227     C                   END
027400990427     C* Memorizzo se p.o. 'BAR' O 'SDI'
027500990427     C                   MOVEL     ORGDIT        COMDIT
027600970225     C* Imposto il cod.distretto e area se non immessi
027700970225     C     V1CDST        IFEQ      *BLANKS
027800970225     C                   MOVEL     ORGFL3        V1CDST
027900970225     C                   EXSR      DECDST
028000970225     C                   END
028100970225     C     PARCAR        IFEQ      *ZEROS
028200970225     C                   MOVE      ORGCAR        V1CCAR
028300970225     C                   MOVE      ORGCAR        PARCAR
028400970225     C                   EXSR      DECCAR
028500970225     C                   END
028600970225    1C                   END
028700970225     C* DATA ELABORAZIONE
028800970225     C                   CLEAR                   PARDEL
028900970527     C                   CLEAR                   PARTGI
029000970225     C*
029100970225    1C     V1CDEL        IFEQ      *ZEROS
029200970225     C                   SETON                                        4328
029300970225     C                   MOVEL     MSG(7)        V1ZMSG
029400970225     C                   GOTO      ETRD01
029500970225    1C                   END
029600970225     C                   CLEAR                   WLBDA8
029700970225     C                   Z-ADD     V1CDEL        G08DAT
029800970225     C                   CALL      'XSRDA8'
029900970225     C                   PARM                    WLBDA8
030000970227    1C     G08ERR        IFEQ      '1'
030100970225     C                   MOVEL     MSG(8)        V1ZMSG
030200970225     C                   SETON                                        4328
030300970225     C                   GOTO      ETRD01
030400970227    1C                   END
030500970225     C                   MOVE      G08DAT        V1CDEL
030600970225     C                   MOVE      G08INV        PARDEL
030700970527     C                   MOVE      G08TGI        PARTGI
030800970526     C* DATA ELABORAZIONE AL
030900970526     C                   CLEAR                   PARDAL
031000970526     C*
031100970526    1C     V1CDAL        IFGT      *ZEROS
031200970526     C                   CLEAR                   WLBDA8
031300970526     C                   Z-ADD     V1CDAL        G08DAT
031400970526     C                   CALL      'XSRDA8'
031500970526     C                   PARM                    WLBDA8
031600970526    2C     G08ERR        IFEQ      '1'
031700970526     C                   MOVEL     MSG(8)        V1ZMSG
031800970526     C                   SETON                                        4528
031900970526     C                   GOTO      ETRD01
032000970526    2C                   END
032100970526     C                   MOVE      G08DAT        V1CDAL
032200970526     C                   MOVE      G08INV        PARDAL
032300970526     C* ERRORE SE DATA AL < DI DATA DAL
032400970526     C     PARDAL        IFLT      PARDEL
032500970526     C                   MOVEL     MSG(14)       V1ZMSG
032600970526     C                   SETON                                        4528
032700970526     C                   GOTO      ETRD01
032800970526     C                   END
032900970526    1C                   END
033000990427     c* Se immesso invio network, devo impostare B o S
033100990427     C* e quest'ultimo deve essere congruente col punto operativo
033200990427     C* eventualmente richiesto
033300990427    1C     V1CINN        IFEQ      'S'
033400990427    2C     V1CNTW        IFEQ      ' '
033500990427     C                   MOVEL     MSG(15)       V1ZMSG
033600990427     C                   SETON                                        4628
033700990427     C                   GOTO      ETRD01
033800990427    2C                   ENDIF
033900990427    2C     V1CLIN        IFGT      *ZEROS
034000990427     C     COMDIT        ANDNE     V1CNTW
034100990427     C                   MOVEL     MSG(17)       V1ZMSG
034200990427     C                   SETON                                        4628
034300990427     C                   GOTO      ETRD01
034400990427    2C                   END
034500990427    1C                   ENDIF
034600990427     c* Se richiesta creazione dati report è necessaria l'esistenza
034700990427     C* dei dati in linea
034800990430     C     V1CCRP        IFEQ      'S'
034900990430     C     PARDEL        ANDLE     PARPUL
035000990427     C                   MOVEL     MSG(16)       V1ZMSG
035100990427     C                   SETON                                        4728
035200990427     C                   GOTO      ETRD01
035300990427     C                   ENDIF
035400970227     C*
035500970224     C     ETRD01        ENDSR
035600970225     C*****************************************************************
035700970225     C* DECODIFICA CODICE DISTRETTO
035800970225     C*****************************************************************
035900970225     C     DECDST        BEGSR
036000970225     C* Chaino tabella 17 per decodifica
036100970225     C                   MOVEL     '17'          KCOD
036200970225     C                   MOVEL(P)  V1CDST        KKEY
036300970225     C     KTAB          CHAIN     TABEL00F                           30
036400970225    2C     *IN30         IFEQ      *OFF
036500970225     C     TBLFLG        ANDEQ     *BLANKS
036600970225     C                   MOVEL     TBLUNI        V1DDST
036700970225    2C                   END
036800970225     C                   ENDSR
036900970225     C*****************************************************************
037000970225     C* DECODIFICA CODICE AREA
037100970225     C*****************************************************************
037200970225     C     DECCAR        BEGSR
037300970225     C* Chaino tabella 05 per decodifica
037400970225     C                   MOVEL     '05'          KCOD
037500970225     C                   MOVEL(P)  PARCAR        KKEY
037600970225     C     KTAB          CHAIN     TABEL00F                           30
037700970225    2C     *IN30         IFEQ      *OFF
037800970225     C     TBLFLG        ANDEQ     *BLANKS
037900970225     C                   MOVEL     TBLUNI        V1DCAR
038000970225    2C                   END
038100970225     C                   ENDSR
038200970409     C*****************************************************************
038300970409     C* REPERIMENTO DATA ULTIMO UTILIZZO PGM PULIZIE
038400970409     C*****************************************************************
038500040924     C     CHKUTI        BEGSR
038600040924     C                   CLEAR                   WDTUTI
038700970409     C*
038800040924     C                   MOVE      *BLANKS       $DAT
038900040924     C                   MOVE      *BLANKS       $ERR
039000040924     C                   CALL      'TRUL49C'                            99
039100040924     C                   PARM                    $PGM
039200040924     C                   PARM                    $DAT              8
039300040924     C                   PARM                    $ERR              1
039400970409      * PROGRAMMA IN USO DATA ULTIMO UTILIZZO = DATA DEL GIORNO
039500040924    1C     $ERR          IFEQ      '1'
039600040924     C     *IN99         OREQ      '1'
039700040924     C     $DAT          OREQ      '00000000'
039800040924     C                   Z-ADD     WDATEU        WDTUTI
039900040924   X1C                   ELSE
040000040924    2C     $DAT          IFNE      *BLANKS
040100040924     C                   MOVE      $DAT          WDTUTI
040200040924    2C                   ENDIF
040300040924    1C                   ENDIF
040400040924     C                   ENDSR
040500970225     C*****************************************************************
040600970224     C* OPERAZIONI INIZIALI
040700970224     C*****************************************************************
040800970224     C     *INZSR        BEGSR
040900970224     C*
041000970224     C     *ENTRY        PLIST
041100970224     C                   PARM                    KPJBA
041200970224     C*
041300970224     C                   Z-ADD     1             CODUT
041400970224     C                   CALL      'X§PARUT'
041500970224     C                   PARM                    UT§DSE
041600970224     C                   MOVEL     REC80         CNCR80
041700970224     C                   MOVEL     RAGUT         RSUT
041800970224     C*
041900970224     C                   TIME                    WTIME            14 0
042000970224     C                   MOVE      WTIME         WDATE             8 0
042100970224     C                   CLEAR                   WLBDA8
042200970224     C                   MOVE      WDATE         G08DAT
042300970224     C                   CALL      'XSRDA8'
042400970224     C                   PARM                    WLBDA8
042500970224     C                   MOVE      G08INV        WDATEU            8 0
042600970409     C*
042700970718     C* CHAINO TABELLA 5A PER REPERIRE GIORNI DI PULIZIA STATISTICHE
042800970718     C                   MOVEL     '5A'          KCOD
042900000911     C                   MOVEL(P)  'SEDE1'       KKEY
043000970409     C     KTAB          CHAIN     TABEL00F                           30
043100000911     C  N30              MOVEL     TBLUNI        DS5AS1
043200000911     C   30              MOVEL     *ZEROS        DS5AS1
043300040924     C* I pgm di pulizia statistiche sono 2 ma siccome entrambe le azio
043400040924     C* ni vengono schedulate per il week-end reperisco solo 1 data ul-
043500040924     C* timo utilizzo
043600040924     C                   MOVEL(P)  'FNLSC7R'     $PGM             10
043700040924     C                   EXSR      CHKUTI
043800040924     C                   CLEAR                   WLBDA8
043900040924     C                   Z-ADD     WDTUTI        G08INV
044000040924     C                   MOVEL     '3'           G08ERR
044100040924     C                   CALL      'XSRDA8'
044200040924     C                   PARM                    WLBDA8
044300970409     C* DETERMINO DATA PER PULIZIA ARCHIVI STATISTICHE
044400040924     C     G08TGI        SUB       §5ASTS        GIOTGI
044500970409     C                   CALL      'XSRGI8'
044600970409     C                   PARM                    WGIDAT
044700970409     C                   Z-ADD     GIOINV        PARPUL
044800040924     C***                  Z-ADDGIOINV    PARPU1
044900970409     C*
045000970224     C* SE NON SONO IN SEDE PROTEGGO IL DISTRETTO
045100970224     C     SIMFEL        COMP      0                                  0505
045200970224     C*
045300970224     C     KTAB          KLIST
045400970224     C                   KFLD                    CODUT
045500970224     C                   KFLD                    KCOD
045600970224     C                   KFLD                    KKEY
045700970225     C*
045800970225     C     KORG          KLIST
045900970225     C                   KFLD                    V1CDST
046000970225     C                   KFLD                    PARCAR
046100970225     C*
046200970225     C     KORG1         KLIST
046300970225     C                   KFLD                    ORGFL3
046400970225     C                   KFLD                    ORGCAR
046500970224     C*
046600970224     C     *LIKE         DEFINE    TBLCOD        KCOD
046700970224     C     *LIKE         DEFINE    TBLKEY        KKEY
046800040924     C     *like         DEFine    WDATEU        WDTUTI
046900990427     C     *LIKE         DEFINE    V1CNTW        COMDIT
047000040924     C     *LIKE         DEFINE    PARDEL        PARPUL
047100970225     C*
047200970224     C                   ENDSR
047300990427**
047400970225Cod.Distretto inesistente in organigramma o annullato                          1
047500970225Cod.Area inesistente in organigramma o annullato                               2
047600970225Il codice area non è congruente col distretto richiesto                        3
047700980407Punto operativo inesistente o annullato                                        4
047800970225Il punto operativo non è congruente con il codice area richiesto               5
047900970225Il punto operativo non è congruente con il codice distretto richiesto          6
048000970526Data elaborazione "dal" obbligatoria                                           7
048100970225Data elaborazione errata                                                       8
048200970227Immettere un punto operativo valido                                            9
048300970227Immettere un codice distretto valido                                          10
048400970227Codice area immesso facente parte di un distretto non valido                  11
048500970227Punto operativo immesso facente parte di un distretto non valido              12
048600970228I suddetti p.o. non hanno eseguito l'IDD nella data richiesta. ENTER PER FORZ.13
048700970526Data iniziale maggiore di quella finale                                       14
048800990427Immettere network: Bartolini o S.D.I.                                         15
048900990427Creazione dati REPORT non più effettuabile per il periodo richiesto           16
049000990427Network incongruente con il p.o. scelto                                       17
049100110125**
049200110125CHKOBJ OBJ(K018SDINO) OBJTYPE(*JOBQ)
