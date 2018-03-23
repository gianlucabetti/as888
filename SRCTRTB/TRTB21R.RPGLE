000100930708     H DATEDIT(*YMD)
000200930708     F*--------------------------------------------------------------*
000300930708     F* GESTIONE TABELLA 2A
000400930708     F*----------------------* USO DEGLI INDICATORI *----------------*
000500930708     F*  01        INSERIMENTO                                       *
000600930708     F*  02        VARIAZIONE                                        *
000700940930     F*  04        ANNULLAMENTO                                      *
000800930708     F*  79        COMANDO DI ANNULLAMENTO CONSENTITO SOLO DA EDP    *
000900930708     F*--------------------------------------------------------------*
001000930708     FTRTB21D   CF   E             WORKSTN
001100930708     FTABEL00F  UF A E           K DISK
001200930708     D*--------------------------------------------------------------*
001300930708     D*                        I N P U T                             *
001400930708     D*--------------------------------------------------------------*
001500930708     D DS2A          E DS
001600940930     D DS2Z          E DS
001700060315     D  alfanep               24     25
001800930708      *
001900930708     D KPJBA         E DS
002000930708     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
002100971210     D TRUL12        E DS                  EXTNAME(TRUL12DS)
002200090114     D tntbices      E DS
002300930708     C******************************************************
002400940401     C*  Inizializzazione prima videata
002500940401     C                   EXSR      INZVID
002600940401     C*
002700940401     C*  Loop videate
002800940401     C     *INKC         DOWEQ     *OFF
002900940401     C     TIPVID        CASEQ     '1'           GESVD1
003000940401     C     TIPVID        CASEQ     '2'           GESVD2
003100940930     C     TIPVID        CASEQ     '3'           GESVD3
003200060315     C     TIPVID        CASEQ     '4'           GESVD4
003300940401     C                   END
003400940401     C                   END
003500891030     C     FINE          TAG
003600891030     C                   SETON                                        LR
003700891030     C**------------------------
003800891030     C** PULISCI CAMPI VIDEO
003900891030     C**------------------------
004000891030     C     PULVID        BEGSR
004100900608      *
004200930708     C                   CLEAR                   DS2A
004300940930     C                   CLEAR                   DS2Z
004400900608      *
004500891030     C                   ENDSR
004600891030     C**----------------------------
004700891030     C** AGGIORNAMENTO / INSERIMENTO
004800891030     C**----------------------------
004900891030     C     SCRIVI        BEGSR
005000940930     C                   MOVEL     CODUT         §KUT
005100940930     C                   MOVEL     '2A'          §COD
005200940930     C                   MOVEL     VCOD          §KEY
005300891107     C     KTAB          CHAIN     TABEL                              80
005400891030     C                   MOVEL     §KUT          TBLKUT
005500891030     C                   MOVEL     §COD          TBLCOD
005600891030     C                   MOVEL     §KEY          TBLKEY
005700900612     C                   MOVEL     DS2A          TBLUNI
005800900420     C*§
005900900420     C                   MOVE      '1'           TBLFTT
006000900420     C                   MOVE      *ZERO         TBLFLT
006100900420     C                   MOVE      *BLANK        TBLFTR
006200900420     C                   MOVE      *ZERO         TBLDTR
006300900420     C*§
006400891102     C   80              WRITE     TABEL                                98
006500891102     C  N80              UPDATE    TABEL                                98
006600940930     C*
006700940930     C                   MOVEL     CODUT         §KUT
006800940930     C                   MOVEL     '2Z'          §COD
006900940930     C                   MOVEL     VCOD          §KEY
007000940930     C     KTAB          CHAIN     TABEL                              80
007100940930     C                   MOVEL     §KUT          TBLKUT
007200940930     C                   MOVEL     §COD          TBLCOD
007300940930     C                   MOVEL     §KEY          TBLKEY
007400940930     C                   MOVEL     *BLANKS       TBLUNI
007500940930     C                   MOVEL     DS2Z          TBLUNI
007600940930     C*§
007700940930     C                   MOVE      '1'           TBLFTT
007800940930     C                   MOVE      *ZERO         TBLFLT
007900940930     C                   MOVE      *BLANK        TBLFTR
008000940930     C                   MOVE      *ZERO         TBLDTR
008100940930     C*§
008200940930     C   80              WRITE     TABEL                                98
008300940930     C  N80              UPDATE    TABEL                                98
008400891030     C                   ENDSR
008500940401     C**----------------------------
008600940401     C** GESTIONE PRIMA VIDEATA
008700940401     C**----------------------------
008800940401     C     GESVD1        BEGSR
008900940401     C*
009000940401     C                   EXFMT     TB21D1
009100940401     C*
009200940401     C     *INKC         IFEQ      '0'
009300940401     C     VCOD          ANDNE     *BLANKS
009400940401     C*§  RICERCA IN TABEL00F
009500940401     C     '?'           SCAN      VCOD:1                                 80
009600940401     C     *IN80         IFEQ      *ON
009700940401     C                   MOVEL     CODUT         §KUT
009800940401     C                   MOVEL     '2A'          §COD
009900940401     C                   MOVE      *ZEROS        VCOD
010000940401     C                   CALL      'X§TABER'
010100940401     C                   PARM                    §KUT
010200940401     C                   PARM                    §COD
010300940401     C                   PARM                    §KEY
010400940401     C                   PARM                    §DES
010500940401     C                   MOVEL     §KEY          VCOD
010600940401     C                   ELSE
010700940401     C*----------------------------------------------
010800940401     C*  POSIZION. SU TABELLA
010900940401     C                   MOVEL     CODUT         §KUT
011000940401     C                   MOVEL     '2A'          §COD
011100940401     C                   MOVEL     VCOD          §KEY
011200940930     C                   SETOFF                                       010204
011300940401     C                   EXSR      PULVID
011400940401     C                   MOVEL     '2'           TIPVID
011500940930     C     KTAB          CHAIN     TABEL                              80
011600940401     C     *IN80         IFEQ      *ON
011700940401     C                   SETON                                        01
011800940401     C                   ELSE
011900940930     C                   MOVEL     TBLUNI        DS2A
012000940930     C     TBLFLG        IFEQ      ' '
012100940401     C                   SETON                                        02
012200940930     C                   ELSE
012300940930     C                   SETON                                        04
012400940401     C                   END
012500940930     C                   END
012600940401     C*
012700940930     C                   MOVEL     CODUT         §KUT
012800940930     C                   MOVEL     '2Z'          §COD
012900940930     C                   MOVEL     VCOD          §KEY
013000940930     C                   MOVEL     '2'           TIPVID
013100940930     C     KTAB          CHAIN     TABEL                              81
013200940930     C  N81              MOVEL     TBLUNI        DS2Z
013300940401     C                   END
013400940401     C*
013500940930     C                   END
013600940930     C*
013700940401     C                   ENDSR
013800940401     C**----------------------------
013900940401     C** GESTIONE SECONDA VIDEATA
014000940401     C**----------------------------
014100940401     C     GESVD2        BEGSR
014200940401     C*
014300940401     C                   SETOFF                                       99
014400940401     C                   EXFMT     TB21D2
014500940401     C   KC              GOTO      FINE
014600940401     C*
014700940401     C     *INKL         IFEQ      '1'
014800940401     C                   MOVEL     '1'           TIPVID
014900940401     C                   EXSR      INZVID
015000940401     C                   ELSE
015100940401     C**--- ANNULLAMENTO
015200940930     C     *INKQ         IFEQ      '1'
015300940930     C                   MOVEL     CODUT         §KUT
015400940930     C                   MOVEL     '2A'          §COD
015500940930     C                   MOVEL     VCOD          §KEY
015600940930     C     KTAB          CHAIN     TABEL                              80
015700940930     C                   MOVEL     '*'           TBLFLG
015800940930     C  N80              UPDATE    TABEL
015900940930     C                   MOVEL     CODUT         §KUT
016000940930     C                   MOVEL     '2Z'          §COD
016100940930     C                   MOVEL     VCOD          §KEY
016200940930     C     KTAB          CHAIN     TABEL                              80
016300940930     C                   MOVEL     '*'           TBLFLG
016400940930     C  N80              UPDATE    TABEL
016500940401     C                   MOVEL     '1'           TIPVID
016600940401     C                   EXSR      INZVID
016700940930     C                   ELSE
016800940930     C**--- RIPRISTINO
016900940930     C     *INKE         IFEQ      '1'
017000940930     C                   MOVEL     CODUT         §KUT
017100940930     C                   MOVEL     '2A'          §COD
017200940930     C                   MOVEL     VCOD          §KEY
017300940930     C     KTAB          CHAIN     TABEL                              80
017400940930     C                   MOVEL     ' '           TBLFLG
017500940930     C  N80              UPDATE    TABEL
017600940930     C                   MOVEL     CODUT         §KUT
017700940930     C                   MOVEL     '2Z'          §COD
017800940930     C                   MOVEL     VCOD          §KEY
017900940930     C     KTAB          CHAIN     TABEL                              80
018000940930     C                   MOVEL     ' '           TBLFLG
018100940930     C  N80              UPDATE    TABEL
018200940930     C                   MOVEL     '1'           TIPVID
018300940930     C                   EXSR      INZVID
018400940930     C                   ELSE
018500940930     C                   MOVEL     '3'           TIPVID
018600940930     C                   END
018700940930     C                   END
018800940401      *
018900940401     C                   END
019000940401     C*
019100940401     C                   ENDSR
019200940930     C**----------------------------
019300940930     C** GESTIONE TERZA   VIDEATA
019400940930     C**----------------------------
019500940930     C     GESVD3        BEGSR
019600940930     C*
019700080228     c     ErrVD3        tag
019800940930     C                   SETOFF                                       99
019900940930     C                   EXFMT     TB21D3
020000940930     C   KC              GOTO      FINE
020100940930     C*
020200940930     C     *INKL         IFEQ      '1'
020300940930     C                   MOVEL     '2'           TIPVID
020400940930     C                   ELSE
020500940930     C**--- ANNULLAMENTO
020600940930     C     *INKQ         IFEQ      '1'
020700940930     C                   MOVEL     CODUT         §KUT
020800940930     C                   MOVEL     '2A'          §COD
020900940930     C                   MOVEL     VCOD          §KEY
021000940930     C     KTAB          CHAIN     TABEL                              80
021100940930     C                   MOVEL     '*'           TBLFLG
021200940930     C                   UPDATE    TABEL
021300940930     C                   MOVEL     CODUT         §KUT
021400940930     C                   MOVEL     '2Z'          §COD
021500940930     C                   MOVEL     VCOD          §KEY
021600940930     C     KTAB          CHAIN     TABEL                              80
021700940930     C                   MOVEL     '*'           TBLFLG
021800940930     C                   UPDATE    TABEL
021900940930     C                   MOVEL     '1'           TIPVID
022000940930     C                   EXSR      INZVID
022100940930     C                   ELSE
022200940930     C**--- RIPRISTINO
022300940930     C     *INKE         IFEQ      '1'
022400940930     C                   MOVEL     CODUT         §KUT
022500940930     C                   MOVEL     '2A'          §COD
022600940930     C                   MOVEL     VCOD          §KEY
022700940930     C     KTAB          CHAIN     TABEL                              80
022800940930     C                   MOVEL     ' '           TBLFLG
022900940930     C                   UPDATE    TABEL
023000940930     C                   MOVEL     CODUT         §KUT
023100940930     C                   MOVEL     '2Z'          §COD
023200940930     C                   MOVEL     VCOD          §KEY
023300940930     C     KTAB          CHAIN     TABEL                              80
023400940930     C                   MOVEL     ' '           TBLFLG
023500940930     C                   UPDATE    TABEL
023600940930     C                   MOVEL     '1'           TIPVID
023700940930     C                   EXSR      INZVID
023800940930     C                   ELSE
023900060315     C                   MOVEL     '4'           TIPVID
024000940930     C                   END
024100940930     C                   END
024200080228     C* controllo x PDA
024300080228     c                   if        §2ZPDA = 'N' and §2ZINFPDA = 'S'
024400080228     c                   seton                                        2999
024500080228     c                   goto      errvd3
024600080228     c                   end
024700131127     C* controlla obbligatorietà dell'immissione Note che può essere messa
024800131127     C* solo per eventi di tipo Riserva e con Gestione da PDA.
024900131127     c                   if        §2ATIP<>'R' or §2ZPDA <>*blank
025000131127     c                   if          §2zOBBnote ='S'
025100131127     c                   seton                                        2899
025200131127     c                   goto      errvd3
025300131127     c                   end
025400131127     c                   end
025500080228     C*
025600940930     C                   END
025700940930     C*
025800940930     C                   ENDSR
025900060315     C**----------------------------
026000060315     C** GESTIONE QUARTA  VIDEATA
026100060315     C**----------------------------
026200060315     C     GESVD4        BEGSR
026300060315     c* controllo se il campo alfanep è numerico
026400060315     c                   if        alfanep<>'00'
026500060315     c                   testn                   alfanep              40
026600060315     c                   move      alfanep       w001a             1
026700060315     c                   if        not *in40 or w001a<'0'
026800060315     c                   clear                   §2znep
026900060315     c                   movel     '00'          alfanep
027000060315     c                   endif
027100060315     c                   endif
027200060315     C*
027300060315     C                   SETOFF                                       99
027400060315     C                   EXFMT     TB21D4
027500060315     C   KC              GOTO      FINE
027600060315     C*
027700060315     C     *INKL         IFEQ      '1'
027800060315     C                   MOVEL     '3'           TIPVID
027900060315     C                   ELSE
028000060315     C**--- ANNULLAMENTO
028100060315     C     *INKQ         IFEQ      '1'
028200060315     C                   MOVEL     CODUT         §KUT
028300060315     C                   MOVEL     '2A'          §COD
028400060315     C                   MOVEL     VCOD          §KEY
028500060315     C     KTAB          CHAIN     TABEL                              80
028600060315     C                   MOVEL     '*'           TBLFLG
028700060315     C                   UPDATE    TABEL
028800060315     C                   MOVEL     CODUT         §KUT
028900060315     C                   MOVEL     '2Z'          §COD
029000060315     C                   MOVEL     VCOD          §KEY
029100060315     C     KTAB          CHAIN     TABEL                              80
029200060315     C                   MOVEL     '*'           TBLFLG
029300060315     C                   UPDATE    TABEL
029400060315     C                   MOVEL     '1'           TIPVID
029500060315     C                   EXSR      INZVID
029600060315     C                   ELSE
029700060315     C**--- RIPRISTINO
029800060315     C     *INKE         IFEQ      '1'
029900060315     C                   MOVEL     CODUT         §KUT
030000060315     C                   MOVEL     '2A'          §COD
030100060315     C                   MOVEL     VCOD          §KEY
030200060315     C     KTAB          CHAIN     TABEL                              80
030300060315     C                   MOVEL     ' '           TBLFLG
030400060315     C                   UPDATE    TABEL
030500060315     C                   MOVEL     CODUT         §KUT
030600060315     C                   MOVEL     '2Z'          §COD
030700060315     C                   MOVEL     VCOD          §KEY
030800060315     C     KTAB          CHAIN     TABEL                              80
030900060315     C                   MOVEL     ' '           TBLFLG
031000060315     C                   UPDATE    TABEL
031100060315     C                   MOVEL     '1'           TIPVID
031200060315     C                   EXSR      INZVID
031300060315     C                   ELSE
031400060315     C                   MOVEL     '4'           TIPVID
031500060315     C                   END
031600060315     C                   END
031700060331     c* SE CEP <>*blanks allora FEP obbligatorio
031800060331     c                   if        §2zcep<>' ' and §2zfep=' '
031900090115     c                   seton                                        5099
032000060331     C                   MOVEL     '4'           TIPVID
032100060331     c                   endif
032200060404     c* SE CEP <>*blanks allora DEP obbligatorio
032300060404     c                   if        §2zcep<>' ' and §2zdep=' '
032400090115     c                   seton                                        5199
032500060404     C                   MOVEL     '4'           TIPVID
032600060404     c                   endif
032700090115     c
032800170209     c* SE FLG <>*blanks allora non è possibile
032900170209     c                   if        §2aflg<>' ' and §2zinvape = 'S'
033000170209     c                   seton                                        5299
033100170209     C                   MOVEL     '4'           TIPVID
033200170209     c                   endif
033300170209     c
033400170209     c* SE FLG <>*blanks allora non è possibile
033500170209     c                   if        §2aflg<>' ' and §2zwebdis = 'S'
033600170209     c                   seton                                        5399
033700170209     C                   MOVEL     '4'           TIPVID
033800170209     c                   endif
033900170209     c
034000090115     c* Se premuto F6 chiedo se si è sicuri di non dover inserire ICE
034100090115     c                   if        *in01 and *in06 and wforza=' '
034200090115     c                   seton                                        3099
034300090115     c                   eval      wforza='S'
034400090115     C                   MOVEL     '4'           TIPVID
034500090115     c                   endif
034600060315      *
034700090115     C  n99*IN06         IFEQ      '1'
034800090114     C     *IN08         oreq      '1'
034900060315     C                   EXSR      SCRIVI
035000090114     c* se F08 richiamo pgm di inserimento ICE
035100090114     c                   if        *in08
035200090114     c                   clear                   tntbices
035300090114     c                   eval      iceke1=vcod
035400090114     c                   eval      icedes=§2ades
035500090114     c                   movel     tntbices      kpjbu
035600090114     c                   call      'TNTBICER'
035700090114     c                   parm                    kpjba
035800090114     c                   endif
035900090114     c
036000060315     C                   MOVEL     '1'           TIPVID
036100060315     C                   EXSR      INZVID
036200060315     C                   END
036300060315     C*
036400060315     C                   END
036500060315     C*
036600060315     C                   ENDSR
036700940401     C**----------------------------
036800940401     C** GESTIONE INZ.VIDEATA
036900940401     C**----------------------------
037000940401     C     INZVID        BEGSR
037100940401     C*
037200940401     C                   MOVEL     RAGUT         RSUT
037300940401     C                   MOVEL     *BLANKS       §DES             20
037400940401     C                   MOVEL     *BLANKS       VCOD
037500940401     C                   MOVEL     '?'           VCOD
037600090115     c                   clear                   wforza            1
037700940401     C                   ENDSR
037800940401     C**----------------------------
037900940401     C** GESTIONE OPERAZ.INIZIALI
038000940401     C**----------------------------
038100940401     C     *INZSR        BEGSR
038200940401     C*
038300940401     C     *ENTRY        PLIST
038400940401     C                   PARM                    KPJBA
038500940401     C                   Z-ADD     1             CODUT
038600060112     C                   CALL      'XPARUT'
038700940401     C                   PARM                    UTEDSE
038800940401     C                   MOVEL     RAGUT         RSUT
038900940401     C******************************************************
039000940401     C* POSIZIONAMENTO TABELLE                             *
039100940401     C     KTAB          KLIST
039200940401     C                   KFLD                    §KUT
039300940401     C                   KFLD                    §COD
039400940401     C                   KFLD                    §KEY
039500940401     C******************************************************
039600940401     C     *LIKE         DEFINE    TBLKUT        §KUT
039700940401     C     *LIKE         DEFINE    TBLCOD        §COD
039800940401     C     *LIKE         DEFINE    TBLKEY        §KEY
039900971210     C******************************************************
040000940401     C                   MOVEL     KNMUS         PROF              3
040100940401     C                   MOVEL     '1'           TIPVID            1
040200940401     C     PROF          IFNE      'EDP'
040300940401     C                   SETON                                        79
040400940401     C                   END
040500000525     C******************************************************
040600000525     C* Richiamo pgm per controllo modificabilità tabella
040700000525     C                   MOVEL     KNSIF         SIFPR7            7
040800000525     C                   MOVE      SIFPR7        SIFPR1            1
040900000525     C*  SE SIF DI PROVA NON CONTROLLO
041000000525     C     SIFPR1        IFNE      'P'
041100000525     C                   CLEAR                   TRUL12
041200000525     C                   MOVEL     'L'           I12TLA
041300000525     C                   MOVEL     KNSIF         I12NSI
041400000525     C                   MOVEL     '2A'          I12COD
041500000525     C                   CALL      'TRUL12R'
041600000525     C                   PARM                    TRUL12
041700000525     C     O12ERR        IFEQ      '1'
041800000525     C                   GOTO      FINE
041900000525     C                   END
042000000525     C                   END
042100940401     C*
042200940401     C                   ENDSR
