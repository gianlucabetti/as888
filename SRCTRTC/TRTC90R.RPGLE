000100960605      ***********************************************************************
000200960605      *   IMPORTA BOLLE DA PARTNER ESTERI            - E.D.I.   ESTERO -    *
000300960605      ***********************************************************************
000400960605     H DECEDIT('0,') DATEDIT(*DMY.)
000500000306      *---------------------------------------------------------------------*
000600000306      *  DATA BASE                                                          *
000700000306      *---------------------------------------------------------------------*
000800960605     FEDTAB01L  IF   E           K DISK
000900000306     FTRTC90D   CF   E             WORKSTN SFILE(TC90S02:NRR2)
001000000306      *---------------------------------------------------------------*
001100000306      *  DS
001200000306      *---------------------------------------------------------------*
001300960605     D KPJBA         E DS
001400960605     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001500960606     D CNCR80        E DS
001600040216     D DSCE          E DS                  EXTNAME(EDIDSCE)
001700040216     D DSEA          E DS                  EXTNAME(EDIDSEA)
001800040216     D DSEM          E DS                  EXTNAME(EDIDSEM)
001900040216     D DSFF          E DS                  EXTNAME(EDIDSFF)
002000101018     D DSPF          E DS                  EXTNAME(EDIDSPF)
002100101018     D DSPI          E DS                  EXTNAME(EDIDSPI)
002200101018     D DSPR          E DS                  EXTNAME(EDIDSPR)
002300040216     D DSSS          E DS                  EXTNAME(EDIDSSS)
002400040216     D DS99          E DS                  EXTNAME(EDIDS99)
002500960612     D TRTC90        E DS                  EXTNAME(TRTC90DS)
002600960612     D TRTC91        E DS                  EXTNAME(TRTC91DS)
002700960607     D TRTC92        E DS                  EXTNAME(TRTC92DS)
002800960605     D                SDS
002900960605     D  NOMPGM                 1     10
003000960605     C*---------------------------------------------------------------*
003100960605     C*  CICLO PRINCIPALE
003200960605     C*---------------------------------------------------------------*
003300960605      * Inizializzo prima videata
003400960605     C                   EXSR      INZ01
003500960605      * Loop gestione videate
003600960605     C     WFINE         DOWEQ     'N'
003700960605     C     WTPVID        CASEQ     '1'           GESD01                         Sce.tabella
003800960605     C     WTPVID        CASEQ     '2'           GESD02                         gestione modif.
003900960605     C                   END
004000960605     C                   END
004100960605      * Fine Lavoro
004200960605     C     FINE          TAG
004300960605     C                   SETON                                        LR
004400960605     C*---------------------------------------------------------------*
004500960605     C*  INZ01: Inizializzo prima videata
004600960605     C*---------------------------------------------------------------*
004700960606     C     INZ01         BEGSR
004800960606     C*
004900960606     C                   MOVEL     ' ?'          V1CCOD
005000960606     C                   MOVEL     *BLANKS       V1DCOD
005100960606     C                   MOVEL     NOMPGM        V1CPGM
005200960606     C                   MOVEL     '1'           WTPVID            1
005300960606     C*
005400960606     C                   ENDSR
005500960606     C*---------------------------------------------------------------*
005600960606     C*  INZ02: Caricamento sufile
005700960606     C*---------------------------------------------------------------*
005800960606     C     INZ02         BEGSR
005900960606     C*
006000960607     C                   Z-ADD     0             NRR2
006100040225     C                   Z-ADD     1             srn1
006200960606     C                   MOVEL     V1CCOD        V2CCOD
006300960606     C                   MOVEL     V1DCOD        V2DCOD
006400960606     C                   MOVEL     NOMPGM        V2CPGM
006500960606     C*  Pulisco subfile
006600960606     C                   SETOFF                                       2021
006700960606     C                   WRITE     TC90C02
006800960606     C                   SETON                                        2021
006900960606      *  Eseguo caricamento prima pagina
007000960606     C                   MOVEL     V2CCOD        KCOD
007100960606     C                   MOVEL     *LOVAL        WINKEY
007200960606     C                   EXSR      ROLL02
007300960606      *  Se non ho dati da modificare vado in immissione
007400960607     C     NRR2          IFEQ      0
007500960607     C                   EXSR      CALLR1
007600960606     C                   ELSE
007700960606     C                   MOVEL     '2'           WTPVID            1
007800960606     C                   END
007900960606     C*
008000960606     C                   ENDSR
008100960606     C*---------------------------------------------------------------*
008200960606     C*  GESD01: Gestione prima videata
008300960606     C*---------------------------------------------------------------*
008400960606     C     GESD01        BEGSR
008500960606     C*
008600960606     C                   EXFMT     TC90D01
008700960606     C*  Fine Lavoro
008800960606     C     *INKC         IFEQ      '1'
008900960606     C                   MOVEL     'S'           WFINE
009000960606     C                   GOTO      FINVD1
009100960606     C                   END
009200960606     C*  Controllo se devo richiamare interrogazione
009300960606     C     '?'           SCAN      V1CCOD                                 32
009400960606     C     *IN32         IFEQ      '1'
009500960606     C                   CLEAR                   TRTC91
009600960606     C                   MOVEL     TRTC91        KPJBU
009700960606     C                   CALL      'TRTC91R'
009800960606     C                   PARM                    KPJBA
009900960607     C                   MOVEL     KPJBU         TRTC91
010000960607     C                   MOVEL     D91COD        V1CCOD
010100960606     C                   MOVEL     D91UNI        V1DCOD
010200960606     C                   GOTO      FINVD1
010300960606     C                   END
010400960606     C*  Controllo se immissione o manutenzione
010500960606     C                   MOVEL     '99'          KCOD
010600960606     C                   MOVEL     *BLANKS       KKEY
010700960606     C                   MOVEL     V1CCOD        KKEY
010800960607     C     KTAB          CHAIN     EDTAB01L                           30
010900960606     C     *IN30         IFEQ      '0'
011000960607     C                   MOVEL     TABFTT        WFTT
011100960607     C                   MOVEL     TABFLT        WFLT
011200040216     C                   MOVEL     TABUNI        ds99
011300040216     c                   if        §99PGM<> *blank
011400040216     C*  Call
011500040216     C                   CALL      §99PGM
011600040216     C                   PARM                    KPJBA
011700040216     c                   else
011800960607     C                   MOVEL     TABUNI        V1DCOD
011900960606     C                   EXSR      INZ02
012000040216     c                   end
012100960606     C                   ELSE
012200960607     C                   EXSR      CALL92
012300960606     C                   END
012400960606     C*
012500960606     C     FINVD1        ENDSR
012600960606     C*---------------------------------------------------------------*
012700960606     C*  GESD02: Gestione seconda videata
012800960606     C*---------------------------------------------------------------*
012900960606     C     GESD02        BEGSR
013000960606     C*
013100960606     C                   WRITE     TC90T02
013200960606     C                   EXFMT     TC90C02
013300960606     C*  Fine Lavoro
013400960606     C     *INKC         IFEQ      '1'
013500960606     C                   MOVEL     'S'           WFINE
013600960606     C                   GOTO      FINVD2
013700960606     C                   END
013800960606     C*  Ritorno
013900960606     C     *INKL         IFEQ      '1'
014000960606     C                   EXSR      INZ01
014100960606     C                   GOTO      FINVD2
014200960606     C                   END
014300960606     C*  Rollup
014400960606     C     *IN25         IFEQ      '1'
014500960606     C                   EXSR      ROLL02
014600960606     C                   GOTO      FINVD2
014700960606     C                   END
014800960606     C*  Inserimento
014900960606     C     *INKJ         IFEQ      '1'
015000960607     C                   EXSR      CALLR1
015100960606     C                   GOTO      FINVD2
015200960606     C                   END
015300960606     C*  Controlli
015400960606     C                   EXSR      CTR02
015500960606     C   28              GOTO      FINVD2
015600960606     C*
015700960606     C     FINVD2        ENDSR
015800960606     C*---------------------------------------------------------------*
015900960606     C*  ROLL02: Caricamento una pagina sufile chiavi singola tabella
016000960606     C*---------------------------------------------------------------*
016100960606     C     ROLL02        BEGSR
016200960606     C*
016300960606     C                   Z-ADD     0             WNRIG2            2 0
016400960606     C                   MOVEL     WINKEY        KKEY
016500960606     C     KTAB          SETGT     EDTAB01L                           30
016600960606     C     KCOD          READE     EDTAB01L                               30
016700000306      *
016800000306     C                   DOW        *IN30 = *off  and  Wnrig2 < 16
016900960606     C                   ADD       1             NRR2              4 0
017000960606     C                   ADD       1             WNRIG2
017100960606     C                   MOVEL     TABKEY        V2CKEY
017200000306     C*?*                MOVEL     TABUNI        V2DCOD
017300000306     C                   If         TABFLG = *blanks
017400000306     C                   clear                   V2CANN
017500000306     C                   Else
017600000306     c                   eval      V2Cann = 'ANNULLATO'
017700000306     C                   Endif
017800040216      *
017900040216     C                   clear                   V2DDES
018000040216      * solo x la CE decodifica
018100040216     c                   if        Kcod   = 'CE'
018200040216     C                   MOVEL     TABUNI        dsce
018300040216     C                   MOVEL     §ceDEC        V2DDES
018400040216     c                   end
018500040216      *
018600040216      * solo x la EA decodifica
018700040216     c                   if        Kcod   = 'EA'
018800040216     C                   MOVEL     TABUNI        dsea
018900040216     C                   MOVEL     §eaDES        V2DDES
019000040216     c                   end
019100040216      *
019200040216      * solo x la EM decodifica
019300040216     c                   if        Kcod   = 'EM'
019400040216     C                   MOVEL     TABUNI        dsem
019500040216     C                   MOVEL     §emDES        V2DDES
019600040216     c                   end
019700040216      *
019800040216      * solo x la FF decodifica
019900040216     c                   if        Kcod   = 'FF'
020000040216     C                   MOVEL     TABUNI        dsff
020100040216     C                   MOVEL     §ffDES        V2DDES
020200040216     c                   end
020300040216      *
020400101018      * solo x la PI decodifica
020500101018     c                   if        Kcod   = 'PI'
020600101018     C                   MOVEL     TABUNI        dsPI
020700101018     C                   MOVEL     §PIDESCR      V2DDES
020800101018     c                   end
020900101018      *
021000101018      * solo x la PF decodifica
021100101018     c                   if        Kcod   = 'PF'
021200101018     C                   MOVEL     TABUNI        dsPF
021300101018     C                   MOVEL     §PFDEFIN      V2DDES
021400101018     c                   end
021500101018      *
021600101018      * solo x la PR decodifica
021700101018     c                   if        Kcod   = 'PR'
021800101018     C                   MOVEL     TABUNI        dsPR
021900101018     C                   MOVEL     §PRTRADUT     V2DDES
022000101018     c                   end
022100101018      *
022200040216      * solo x la SS decodifica
022300040216     c                   if        Kcod   = 'SS'
022400040216     C                   MOVEL     TABUNI        dsss
022500040216     C                   MOVEL     §ssDES        V2DDES
022600040216     c                   end
022700000306      *
022800040216      * solo x la TB TC decodifica
022900040216     c                   if        Kcod   = 'TB' or Kcod   = 'TC'  or
023000040216     c                             Kcod   = 'MS' or Kcod   = '99'  or
023100040220     c                             Kcod   = '1A' or Kcod   = 'TS'
023200040216     C                   MOVEL     TABUNI        V2DDES
023300040216     c                   end
023400040220     c                   if        Kcod   = 'CL'
023500040220     C                   eval      V2DDES = %Subst(TABUNI: 1: 7) + ' ' +
023600040220     C                             %Subst(TABUNI: 23: 30)
023700040220     c                   end
023800040216      *
023900960606     C                   WRITE     TC90S02
024000000306      *
024100000306     C                   If        Wnrig2 < 16
024200000306     C     KCOD          READE     EDTAB01L                               30
024300000306     c                   Endif
024400000306      *
024500000306     C                   ENDDO
024600000306      *
024700040225     c                   z-add     nrr2          srn1
024800960606     C                   MOVEL     TABKEY        WINKEY
024900960606     C*  Se sono a fine file accendo 23
025000960606     C     *IN30         COMP      '1'                                    23
025100960606     C*
025200960606     C                   ENDSR
025300960606     C*---------------------------------------------------------------*
025400960607     C*  CALL92: Richiamo pgm TRTC92R
025500960606     C*---------------------------------------------------------------*
025600960607     C     CALL92        BEGSR
025700960606     C*
025800960607     C                   CLEAR                   TRTC92
025900960607     C                   MOVEL     V1CCOD        D92COD
026000960607     C                   MOVEL     'I'           D92MOD
026100960607     C                   MOVEL     TRTC92        KPJBU
026200960607     C*  Call
026300960607     C                   CALL      'TRTC92R'
026400960607     C                   PARM                    KPJBA
026500960607     C                   MOVEL     KPJBU         TRTC92
026600960607     C*  Fine Lavoro
026700960607     C     D92CMD        IFEQ      '03'
026800960607     C                   MOVEL     'S'           WFINE
026900960607     C                   GOTO      FICAL1
027000960607     C                   END
027100960607     C*  Ritorno
027200960607     C     D92CMD        IFEQ      '12'
027300960607     C* azzero i campi e riemetto prima videata
027400960607     C                   EXSR      INZ01
027500960607     C                   GOTO      FICAL1
027600960607     C                   END
027700960607     C* richiamo pgm per immissione key
027800960607     C                   MOVEL     D92FTT        WFTT
027900960607     C                   MOVEL     D92FLT        WFLT
028000960607     C                   MOVEL     D92DEC        V1DCOD
028100960607     C                   EXSR      CALLR1
028200960607     C*
028300960607     C     FICAL1        ENDSR
028400960607     C*---------------------------------------------------------------*
028500960607     C*  CALLR1: Richiamo pgm TRTC90R1
028600960607     C*---------------------------------------------------------------*
028700960607     C     CALLR1        BEGSR
028800960607     C*
028900960607     C                   CLEAR                   TRTC90
029000960607     C*  Se vengo da subfile e non h richiesto l'immissione passo la
029100960607     C*  key da manutenzionare
029200960607     C                   MOVEL     'I'           D90MOD
029300960607     C     WTPVID        IFEQ      '2'
029400960607     C     *INKJ         ANDEQ     '0'
029500960607     C                   MOVEL     V2CKEY        D90KEY
029600960607     C                   MOVEL     'M'           D90MOD
029700960607     C                   END
029800960606     C*  Passo altri parametri
029900960607     C                   MOVEL     WFTT          D90FTT
030000960607     C                   MOVEL     WFLT          D90FLT
030100960606     C                   MOVEL     V1CCOD        D90COD
030200960606     C                   MOVEL     V1DCOD        D90DEC
030300960606     C                   MOVEL     TRTC90        KPJBU
030400960606     C*  Call
030500960607     C                   CALL      'TRTC90R1'
030600960606     C                   PARM                    KPJBA
030700960606     C                   MOVEL     KPJBU         TRTC90
030800960606     C*  Fine Lavoro
030900960606     C     D90CMD        IFEQ      '03'
031000960606     C                   MOVEL     'S'           WFINE
031100960606     C                   GOTO      FINCAL
031200960606     C                   END
031300960606     C*  Modifica
031400960606     C     D90CMD        IFEQ      '11'
031500960606     C                   EXSR      INZ02
031600960606     C                   GOTO      FINCAL
031700960606     C                   END
031800960606     C*  Ritorno
031900960606     C     D90CMD        IFEQ      '12'
032000960606     C*  Se vengo da prima videata azzero i campi e la riemetto
032100960606     C     WTPVID        IFEQ      '1'
032200960606     C                   EXSR      INZ01
032300960606     C                   ELSE
032400960606     C*  Altrimenti se sono in immissione rieseguo caricamento sbf
032500960607     C     *INKJ         IFEQ      '1'
032600960606     C                   EXSR      INZ02
032700960606     C                   END
032800960606     C                   END
032900960606     C*
033000960606     C                   END
033100960606     C*
033200960606     C     FINCAL        ENDSR
033300960606     C*---------------------------------------------------------------*
033400960606     C*  CTR02: Controlli seconda videata
033500960606     C*---------------------------------------------------------------*
033600960606     C     CTR02         BEGSR
033700960606     C*
033800960606     C*  Loop letture subfile
033900960607     C                   MOVEL     'N'           WSCE
034000960606     C                   READC     TC90S02                                31
034100960606     C     *IN31         DOWEQ     '0'
034200960606     C     V2CSCE        IFEQ      '1'
034300960607     C                   MOVEL     'S'           WSCE              1
034400960607     C                   EXSR      CALLR1
034500960607     C     NRR2          CHAIN     TC90S02                            31
034600960606     C                   SETON                                        22
034700960606     C                   MOVEL     ' '           V2CSCE
034800960606     C                   UPDATE    TC90S02
034900960606     C                   SETOFF                                       22
035000960606     C                   END
035100960606     C                   READC     TC90S02                                31
035200960606     C                   END
035300960606     C*
035400960607     C     WSCE          IFEQ      'S'
035500960607     C                   EXSR      INZ02
035600960607     C                   END
035700960607     C*
035800960606     C                   ENDSR
035900960606     C*---------------------------------------------------------------*
036000960606     C*  *INZSR: Operazioni iniziali                                  *
036100960606     C*---------------------------------------------------------------*
036200960606     C     *INZSR        BEGSR
036300960606     C*
036400960606     C     *ENTRY        PLIST
036500960606     C                   PARM                    KPJBA
036600960606      *  Call a XPARUT
036700960606     C                   Z-ADD     1             CODUT
036800960606     C                   CALL      'X§PARUT'
036900960606     C                   PARM                    UT§DSE
037000960606     C                   MOVEL     REC80         CNCR80
037100960606     C                   MOVEL     RAGUT         RSUT
037200960606      * Definizione chiavi
037300960606     C     KTAB          KLIST
037400960606     C                   KFLD                    KCOD
037500960606     C                   KFLD                    KKEY
037600960606      * Definizione varibili
037700960606     C     *LIKE         DEFINE    TABCOD        KCOD
037800960606     C     *LIKE         DEFINE    TABKEY        KKEY
037900960607     C     *LIKE         DEFINE    TABFTT        WFTT
038000960607     C     *LIKE         DEFINE    TABFLT        WFLT
038100960606     C     *LIKE         DEFINE    TABKEY        WINKEY
038200960606     C* Inizializzo variabili
038300960606     C                   MOVEL     'N'           WFINE             1
038400960606     C*
038500960606     C                   ENDSR
