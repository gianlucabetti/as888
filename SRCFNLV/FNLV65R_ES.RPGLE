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
003200970407     D  V1CTSP                25     25
003300970526     D  V1CDAL                26     33  0
003400970526     D  PARDAL                34     41  0
003500970527     D  PARTGI                42     46  0
003600990427     D  V1CCRP                50     50
003700970227      *---------------------------------------------------------------*
003800970227      * INDICATORI USATI
003900970227      *---------------------------------------------------------------*
004000970227      * 05    = OFF - PROGRAMMA RICHIAMATO DA SEDE (PER ORA NON E' GES-
004100970227      *               TITO IL RICHIAMO DA FILIALE)
004200970227      * 28    = ERRORE GENERICO VIDEO
004300970227      * 29    = RIEMETTE FORMATO VIDEO
004400970227      * 30    = DI COMODO
004500970227      * 40    = ERRORE CODICE DISTRETTO
004600970227      * 41    = ERRORE CODICE AREA
004700970227      * 42    = ERRORE PUNTO OPERATIVO
004800970526      * 43    = ERRORE DATA ELABORAZIONE
004900970526      * 45    = ERRORE DATA ELABORAZIONE AL
005000990427      * 46    = ERRORE INVIO NETWORK
005100990427      * 47    = ERRORE CREAZIONE DATI REPORT
005200970227      *---------------------------------------------------------------*
005300970225     C     INIZIO        TAG
005400970224     C* IMPOSTO CAMPI VIDEO
005500970224     C                   MOVE      *BLANKS       V1CDST
005600970224     C                   MOVE      *BLANKS       V1DDST
005700970225     C                   MOVE      *BLANKS       V1CCAR
005800970224     C                   MOVE      *BLANKS       V1DCAR
005900970224     C                   MOVE      *ZEROS        V1CLIN
006000970224     C                   MOVE      *BLANKS       V1DLIN
006100970224     C                   MOVE      *ZEROS        V1CDEL
006200970526     C                   MOVE      *ZEROS        V1CDAL
006300990427     C                   MOVE      'N'           V1CCRP
006400970407     C                   MOVE      'N'           V1CTSP
006500970224     C     EMD01         TAG
006600970224     C                   WRITE     LV65T01
006700970224     C                   EXFMT     LV65D01
006800970224     C                   SETOFF                                       2829
006900970224     C                   MOVE      *BLANKS       V1ZMSG
007000970224     C     *INKC         CABEQ     '1'           FINE
007100970224     C                   EXSR      CTRD01
007200970224     C   28
007300970224     COR 29
007400970225     CORNKF              GOTO      EMD01
007500990507     C* SOTTOMISSIONE BATCH AGGIORNAMENTO WFILE REPORT QUALITA
007600110125
007700110125     c                   eval      comman=chko(1)
007800110125     c                   call(e)   'QCMDEXC'
007900110125     c                   parm                    comman           80
008000110125     c                   parm      40            lung             15 5
008100110125     c                   if        not %error
008200110125     c                   eval      kqieb='K018SDINO'
008300110125     c                   endif
008400110125
008500990507     C     V1CCRP        IFEQ      'S'
008600990507     C                   MOVEL     'LV64'        KCOAZ
008700990507     C                   CALL      'BCH10'
008800990507     C                   PARM                    KPJBA
008900990507     C                   END
009000110125     c
009100990507     C* SOTTOMISSIONE BATCH CONTROLLO QUALITA'
009200110125     c
009300970225     C                   MOVEL     'LV66'        KCOAZ
009400970225     C                   MOVEL     PARAM         KPJBU
009500970225     C                   CALL      'BCH10'
009600970225     C                   PARM                    KPJBA
009700970409     C*
009800970225     C                   GOTO      INIZIO
009900970224     C*
010000970224     C     FINE          TAG
010100970224     C                   SETON                                        LR
010200970224     C*****************************************************************
010300970224     C* CONTROLLI VIDEO
010400970224     C*****************************************************************
010500970224     C     CTRD01        BEGSR
010600970225     C                   SETOFF                                       404142
010700970526     C                   SETOFF                                       434445
010800990427     C                   SETOFF                                       4647
010900970224     C* CODICE DISTRETTO
011000970224     C                   CLEAR                   V1DDST
011100970224     C* Gestione ?
011200970224     C     '?'           SCAN      V1CDST                                 30
011300970224    1C     *IN30         IFEQ      *ON
011400970224     C                   MOVEL     CODUT         §KUT
011500970224     C                   MOVEL     '17'          §COD
011600970224     C                   MOVEL     *BLANKS       §KEY
011700970224     C                   CALL      'X§TABER'
011800970224     C                   PARM                    §KUT              1 0
011900970224     C                   PARM                    §COD              2
012000970224     C                   PARM                    §KEY              8
012100970401     C                   PARM                    §DES             25
012200970224     C                   MOVEL     §KEY          V1CDST
012300970224    2C     V1CDST        IFNE      *BLANKS
012400970224     C                   MOVEL     §DES          V1DDST
012500970224    2C                   END
012600970224     C                   SETON                                        29
012700970224     C                   GOTO      ETRD01
012800970224    1C                   END
012900970225     C*
013000970224    1C     V1CDST        IFNE      *BLANKS
013100970225     C* Decodifico
013200970225     C                   EXSR      DECDST
013300970227     C* Non è possibile richiedere il distretto '9' - SEDE
013400970227     C     V1CDST        IFEQ      '9'
013500970227     C                   SETON                                        2840
013600970227     C                   MOVEL     MSG(10)       V1ZMSG
013700970227     C                   GOTO      ETRD01
013800970227     C                   END
013900970225     C* Se immesso deve esistere in organigramma
014000970225     C     V1CDST        CHAIN     AZORG02L                           30
014100970225    2C     *IN30         DOWEQ     *OFF
014200970225     C     ORGFVA        ANDNE     *BLANKS
014300970721     C     V1CDST        READE     AZORG02L                               30
014400970225    2C                   ENDDO
014500970225    2C     *IN30         IFEQ      *ON
014600970224     C                   SETON                                        2840
014700970224     C                   MOVEL     MSG(1)        V1ZMSG
014800970224     C                   GOTO      ETRD01
014900970224    2C                   END
015000970224    1C                   END
015100970225     C* CODICE AREA
015200970224     C                   CLEAR                   V1DCAR
015300970224     C* Gestione ?
015400970224     C     '?'           SCAN      V1CCAR                                 30
015500970224    1C     *IN30         IFEQ      *ON
015600970224     C                   MOVEL     CODUT         §KUT
015700970224     C                   MOVEL     '05'          §COD
015800970224     C                   MOVEL     *BLANKS       §KEY
015900970224     C                   CALL      'X§TABER'
016000970224     C                   PARM                    §KUT              1 0
016100970224     C                   PARM                    §COD              2
016200970224     C                   PARM                    §KEY              8
016300970401     C                   PARM                    §DES             25
016400970224     C                   MOVEL     §KEY          V1CCAR
016500970224    2C     V1CCAR        IFNE      *BLANKS
016600970224     C                   MOVEL     §DES          V1DCAR
016700970224    2C                   END
016800970224     C                   SETON                                        29
016900970224     C                   GOTO      ETRD01
017000970224    1C                   END
017100970225     C*
017200970225     C* Se = blanks come se fosse = 0
017300970225    1C     V1CCAR        IFEQ      *BLANKS
017400970225     C                   MOVE      *ZEROS        PARCAR
017500970225     C                   ELSE
017600970225     C                   MOVE      V1CCAR        PARCAR
017700970225    1C                   END
017800970225     C*
017900970225     C*
018000970225    1C     PARCAR        IFNE      *ZEROS
018100970225     C* Decodifico
018200970225     C                   EXSR      DECCAR
018300970225     C* Se immesso anche il codice distretto controllo che l'area ne
018400970225     C* faccia parte
018500970225    2C     V1CDST        IFNE      *BLANKS
018600970225     C     KORG          CHAIN     AZORG02L                           30
018700970225    3C     *IN30         DOWEQ     *OFF
018800970225     C     ORGFVA        ANDNE     *BLANKS
018900970225     C     KORG          READE     AZORG02L                               30
019000970225    3C                   ENDDO
019100970225    3C     *IN30         IFEQ      *ON
019200970225     C                   SETON                                        4128
019300970225     C                   MOVEL     MSG(3)        V1ZMSG
019400970225     C                   GOTO      ETRD01
019500970225    3C                   ENDIF
019600970225   X2C                   ELSE
019700970225     C* Altrimenti reperisco il codice distretto relativo all'area
019800970225     C     *LOVAL        SETLL     AZORG02L
019900970225     C                   READ      AZORG02L                               30
020000970225    3C     *IN30         DOWEQ     *OFF
020100970225     C     V1CDST        ANDEQ     *BLANKS
020200970225    4C     ORGCAR        IFNE      PARCAR
020300970225     C     KORG1         SETGT     AZORG02L
020400970225   X4C                   ELSE
020500970225    5C     ORGFVA        IFEQ      *BLANKS
020600970227     C* se codice distretto trovato = 9 - SEDE --> errore
020700970227    6C     ORGFL3        IFEQ      '9'
020800970227     C                   SETON                                        2841
020900970227     C                   MOVEL     MSG(11)       V1ZMSG
021000970227     C                   GOTO      ETRD01
021100970227    6C                   END
021200970225     C                   MOVEL     ORGFL3        V1CDST
021300970225     C* Decodifico
021400970225     C                   EXSR      DECDST
021500970225    5C                   END
021600970225    4C                   END
021700970225     C                   READ      AZORG02L                               30
021800970225    3C                   ENDDO
021900970225     C* A fine ciclo, se non ho trovato il distretto, emetto msg errore
022000970225     C* di area inesistente in organigramma
022100970225    3C     V1CDST        IFEQ      *BLANKS
022200970225     C                   SETON                                        2841
022300970225     C                   MOVEL     MSG(2)        V1ZMSG
022400970225     C                   GOTO      ETRD01
022500970225    3C                   END
022600970225    2C                   ENDIF
022700970224     C*
022800970224    1C                   END
022900970225     C* PUNTO OPERATIVO
023000970225     C                   CLEAR                   V1DLIN
023100970225    1C     V1CLIN        IFGT      *ZEROS
023200970227     C* Deve esistere in organigramma e orgfag deve essere <> 'V'
023300970225     C     V1CLIN        CHAIN     AZORG01L                           30
023400970225    2C     *IN30         IFEQ      *OFF
023500970225     C     ORGFVA        ANDEQ     *BLANKS
023600970227     C                   MOVEL     ORGDES        V1DLIN
023700970227     C     ORGFAG        IFEQ      'V'
023800970227     C                   SETON                                        4228
023900970227     C                   MOVEL     MSG(9)        V1ZMSG
024000970227     C                   GOTO      ETRD01
024100970227     C                   END
024200970225   X2C                   ELSE
024300970225     C                   SETON                                        4228
024400970225     C                   MOVEL     MSG(4)        V1ZMSG
024500970225     C                   GOTO      ETRD01
024600970225    2C                   END
024700970225     C* Deve essere congruente con area o distr. eventualmente immessi
024800970225     C     PARCAR        IFNE      *ZEROS
024900970225     C     ORGCAR        ANDNE     PARCAR
025000970225     C                   SETON                                        4228
025100970225     C                   MOVEL     MSG(5)        V1ZMSG
025200970225     C                   GOTO      ETRD01
025300970225     C                   END
025400970225     C     V1CDST        IFNE      *BLANKS
025500970225     C     ORGFL3        ANDNE     V1CDST
025600970225     C                   SETON                                        4228
025700970225     C                   MOVEL     MSG(6)        V1ZMSG
025800970225     C                   GOTO      ETRD01
025900970225     C                   END
026000970227     C* errore se punto operativo fa parte del distretto '9' - SEDE
026100970227     C     ORGFL3        IFEQ      '9'
026200970227     C                   SETON                                        4228
026300970227     C                   MOVEL     MSG(12)       V1ZMSG
026400970227     C                   GOTO      ETRD01
026500970227     C                   END
026600970225     C* Imposto il cod.distretto e area se non immessi
026700970225     C     V1CDST        IFEQ      *BLANKS
026800970225     C                   MOVEL     ORGFL3        V1CDST
026900970225     C                   EXSR      DECDST
027000970225     C                   END
027100970225     C     PARCAR        IFEQ      *ZEROS
027200970225     C                   MOVE      ORGCAR        V1CCAR
027300970225     C                   MOVE      ORGCAR        PARCAR
027400970225     C                   EXSR      DECCAR
027500970225     C                   END
027600970225    1C                   END
027700970225     C* DATA ELABORAZIONE
027800970225     C                   CLEAR                   PARDEL
027900970527     C                   CLEAR                   PARTGI
028000970225     C*
028100970225    1C     V1CDEL        IFEQ      *ZEROS
028200970225     C                   SETON                                        4328
028300970225     C                   MOVEL     MSG(7)        V1ZMSG
028400970225     C                   GOTO      ETRD01
028500970225    1C                   END
028600970225     C                   CLEAR                   WLBDA8
028700970225     C                   Z-ADD     V1CDEL        G08DAT
028800970225     C                   CALL      'XSRDA8'
028900970225     C                   PARM                    WLBDA8
029000970227    1C     G08ERR        IFEQ      '1'
029100970225     C                   MOVEL     MSG(8)        V1ZMSG
029200970225     C                   SETON                                        4328
029300970225     C                   GOTO      ETRD01
029400970227    1C                   END
029500970225     C                   MOVE      G08DAT        V1CDEL
029600970225     C                   MOVE      G08INV        PARDEL
029700970527     C                   MOVE      G08TGI        PARTGI
029800970526     C* DATA ELABORAZIONE AL
029900970526     C                   CLEAR                   PARDAL
030000970526     C*
030100970526    1C     V1CDAL        IFGT      *ZEROS
030200970526     C                   CLEAR                   WLBDA8
030300970526     C                   Z-ADD     V1CDAL        G08DAT
030400970526     C                   CALL      'XSRDA8'
030500970526     C                   PARM                    WLBDA8
030600970526    2C     G08ERR        IFEQ      '1'
030700970526     C                   MOVEL     MSG(8)        V1ZMSG
030800970526     C                   SETON                                        4528
030900970526     C                   GOTO      ETRD01
031000970526    2C                   END
031100970526     C                   MOVE      G08DAT        V1CDAL
031200970526     C                   MOVE      G08INV        PARDAL
031300970526     C* ERRORE SE DATA AL < DI DATA DAL
031400970526     C     PARDAL        IFLT      PARDEL
031500970526     C                   MOVEL     MSG(14)       V1ZMSG
031600970526     C                   SETON                                        4528
031700970526     C                   GOTO      ETRD01
031800970526     C                   END
031900970526    1C                   END
032000990427     c* Se richiesta creazione dati report è necessaria l'esistenza
032100990427     C* dei dati in linea
032200990430     C     V1CCRP        IFEQ      'S'
032300990430     C     PARDEL        ANDLE     PARPUL
032400990427     C                   MOVEL     MSG(16)       V1ZMSG
032500990427     C                   SETON                                        4728
032600990427     C                   GOTO      ETRD01
032700990427     C                   ENDIF
032800970227     C*
032900970224     C     ETRD01        ENDSR
033000970225     C*****************************************************************
033100970225     C* DECODIFICA CODICE DISTRETTO
033200970225     C*****************************************************************
033300970225     C     DECDST        BEGSR
033400970225     C* Chaino tabella 17 per decodifica
033500970225     C                   MOVEL     '17'          KCOD
033600970225     C                   MOVEL(P)  V1CDST        KKEY
033700970225     C     KTAB          CHAIN     TABEL00F                           30
033800970225    2C     *IN30         IFEQ      *OFF
033900970225     C     TBLFLG        ANDEQ     *BLANKS
034000970225     C                   MOVEL     TBLUNI        V1DDST
034100970225    2C                   END
034200970225     C                   ENDSR
034300970225     C*****************************************************************
034400970225     C* DECODIFICA CODICE AREA
034500970225     C*****************************************************************
034600970225     C     DECCAR        BEGSR
034700970225     C* Chaino tabella 05 per decodifica
034800970225     C                   MOVEL     '05'          KCOD
034900970225     C                   MOVEL(P)  PARCAR        KKEY
035000970225     C     KTAB          CHAIN     TABEL00F                           30
035100970225    2C     *IN30         IFEQ      *OFF
035200970225     C     TBLFLG        ANDEQ     *BLANKS
035300970225     C                   MOVEL     TBLUNI        V1DCAR
035400970225    2C                   END
035500970225     C                   ENDSR
035600970409     C*****************************************************************
035700970409     C* REPERIMENTO DATA ULTIMO UTILIZZO PGM PULIZIE
035800970409     C*****************************************************************
035900040924     C     CHKUTI        BEGSR
036000040924     C                   CLEAR                   WDTUTI
036100970409     C*
036200040924     C                   MOVE      *BLANKS       $DAT
036300040924     C                   MOVE      *BLANKS       $ERR
036400040924     C                   CALL      'TRUL49C'                            99
036500040924     C                   PARM                    $PGM
036600040924     C                   PARM                    $DAT              8
036700040924     C                   PARM                    $ERR              1
036800970409      * PROGRAMMA IN USO DATA ULTIMO UTILIZZO = DATA DEL GIORNO
036900040924    1C     $ERR          IFEQ      '1'
037000040924     C     *IN99         OREQ      '1'
037100040924     C     $DAT          OREQ      '00000000'
037200040924     C                   Z-ADD     WDATEU        WDTUTI
037300040924   X1C                   ELSE
037400040924    2C     $DAT          IFNE      *BLANKS
037500040924     C                   MOVE      $DAT          WDTUTI
037600040924    2C                   ENDIF
037700040924    1C                   ENDIF
037800040924     C                   ENDSR
037900970225     C*****************************************************************
038000970224     C* OPERAZIONI INIZIALI
038100970224     C*****************************************************************
038200970224     C     *INZSR        BEGSR
038300970224     C*
038400970224     C     *ENTRY        PLIST
038500970224     C                   PARM                    KPJBA
038600970224     C*
038700970224     C                   Z-ADD     1             CODUT
038800970224     C                   CALL      'X§PARUT'
038900970224     C                   PARM                    UT§DSE
039000970224     C                   MOVEL     REC80         CNCR80
039100970224     C                   MOVEL     RAGUT         RSUT
039200970224     C*
039300970224     C                   TIME                    WTIME            14 0
039400970224     C                   MOVE      WTIME         WDATE             8 0
039500970224     C                   CLEAR                   WLBDA8
039600970224     C                   MOVE      WDATE         G08DAT
039700970224     C                   CALL      'XSRDA8'
039800970224     C                   PARM                    WLBDA8
039900970224     C                   MOVE      G08INV        WDATEU            8 0
040000970409     C*
040100970718     C* CHAINO TABELLA 5A PER REPERIRE GIORNI DI PULIZIA STATISTICHE
040200970718     C                   MOVEL     '5A'          KCOD
040300000911     C                   MOVEL(P)  'SEDE1'       KKEY
040400970409     C     KTAB          CHAIN     TABEL00F                           30
040500000911     C  N30              MOVEL     TBLUNI        DS5AS1
040600000911     C   30              MOVEL     *ZEROS        DS5AS1
040700040924     C* I pgm di pulizia statistiche sono 2 ma siccome entrambe le azio
040800040924     C* ni vengono schedulate per il week-end reperisco solo 1 data ul-
040900040924     C* timo utilizzo
041000040924     C                   MOVEL(P)  'FNLSC7R'     $PGM             10
041100040924     C                   EXSR      CHKUTI
041200040924     C                   CLEAR                   WLBDA8
041300040924     C                   Z-ADD     WDTUTI        G08INV
041400040924     C                   MOVEL     '3'           G08ERR
041500040924     C                   CALL      'XSRDA8'
041600040924     C                   PARM                    WLBDA8
041700970409     C* DETERMINO DATA PER PULIZIA ARCHIVI STATISTICHE
041800040924     C     G08TGI        SUB       §5ASTS        GIOTGI
041900970409     C                   CALL      'XSRGI8'
042000970409     C                   PARM                    WGIDAT
042100970409     C                   Z-ADD     GIOINV        PARPUL
042200040924     C***                  Z-ADDGIOINV    PARPU1
042300970409     C*
042400970224     C* SE NON SONO IN SEDE PROTEGGO IL DISTRETTO
042500970224     C     SIMFEL        COMP      0                                  0505
042600970224     C*
042700970224     C     KTAB          KLIST
042800970224     C                   KFLD                    CODUT
042900970224     C                   KFLD                    KCOD
043000970224     C                   KFLD                    KKEY
043100970225     C*
043200970225     C     KORG          KLIST
043300970225     C                   KFLD                    V1CDST
043400970225     C                   KFLD                    PARCAR
043500970225     C*
043600970225     C     KORG1         KLIST
043700970225     C                   KFLD                    ORGFL3
043800970225     C                   KFLD                    ORGCAR
043900970224     C*
044000970224     C     *LIKE         DEFINE    TBLCOD        KCOD
044100970224     C     *LIKE         DEFINE    TBLKEY        KKEY
044200040924     C     *like         DEFine    WDATEU        WDTUTI
044300040924     C     *LIKE         DEFINE    PARDEL        PARPUL
044400970225     C*
044500970224     C                   ENDSR
044600990427**
044700970225Cod.Distretto inesistente in organigramma o annullato                          1
044800970225Cod.Area inesistente in organigramma o annullato                               2
044900970225Il codice area non è congruente col distretto richiesto                        3
045000980407Punto operativo inesistente o annullato                                        4
045100970225Il punto operativo non è congruente con il codice area richiesto               5
045200970225Il punto operativo non è congruente con il codice distretto richiesto          6
045300970526Data elaborazione "dal" obbligatoria                                           7
045400970225Data elaborazione errata                                                       8
045500970227Immettere un punto operativo valido                                            9
045600970227Immettere un codice distretto valido                                          10
045700970227Codice area immesso facente parte di un distretto non valido                  11
045800970227Punto operativo immesso facente parte di un distretto non valido              12
045900970228I suddetti p.o. non hanno eseguito l'IDD nella data richiesta. ENTER PER FORZ.13
046000970526Data iniziale maggiore di quella finale                                       14
046100110323
046200990427Creazione dati REPORT non più effettuabile per il periodo richiesto           16
046300110323
046400110125**
046500110125CHKOBJ OBJ(K018SDINO) OBJTYPE(*JOBQ)
