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
002000040216     D DSSS          E DS                  EXTNAME(EDIDSSS)
002100040216     D DS99          E DS                  EXTNAME(EDIDS99)
002200960612     D TRTC90        E DS                  EXTNAME(TRTC90DS)
002300960612     D TRTC91        E DS                  EXTNAME(TRTC91DS)
002400960607     D TRTC92        E DS                  EXTNAME(TRTC92DS)
002500960605     D                SDS
002600960605     D  NOMPGM                 1     10
002700960605     C*---------------------------------------------------------------*
002800960605     C*  CICLO PRINCIPALE
002900960605     C*---------------------------------------------------------------*
003000960605      * Inizializzo prima videata
003100960605     C                   EXSR      INZ01
003200960605      * Loop gestione videate
003300960605     C     WFINE         DOWEQ     'N'
003400960605     C     WTPVID        CASEQ     '1'           GESD01                         Sce.tabella
003500960605     C     WTPVID        CASEQ     '2'           GESD02                         gestione modif.
003600960605     C                   END
003700960605     C                   END
003800960605      * Fine Lavoro
003900960605     C     FINE          TAG
004000960605     C                   SETON                                        LR
004100960605     C*---------------------------------------------------------------*
004200960605     C*  INZ01: Inizializzo prima videata
004300960605     C*---------------------------------------------------------------*
004400960606     C     INZ01         BEGSR
004500960606     C*
004600960606     C                   MOVEL     ' ?'          V1CCOD
004700960606     C                   MOVEL     *BLANKS       V1DCOD
004800960606     C                   MOVEL     NOMPGM        V1CPGM
004900960606     C                   MOVEL     '1'           WTPVID            1
005000960606     C*
005100960606     C                   ENDSR
005200960606     C*---------------------------------------------------------------*
005300960606     C*  INZ02: Caricamento sufile
005400960606     C*---------------------------------------------------------------*
005500960606     C     INZ02         BEGSR
005600960606     C*
005700960607     C                   Z-ADD     0             NRR2
005800040225     C                   Z-ADD     1             srn1
005900960606     C                   MOVEL     V1CCOD        V2CCOD
006000960606     C                   MOVEL     V1DCOD        V2DCOD
006100960606     C                   MOVEL     NOMPGM        V2CPGM
006200960606     C*  Pulisco subfile
006300960606     C                   SETOFF                                       2021
006400960606     C                   WRITE     TC90C02
006500960606     C                   SETON                                        2021
006600960606      *  Eseguo caricamento prima pagina
006700960606     C                   MOVEL     V2CCOD        KCOD
006800960606     C                   MOVEL     *LOVAL        WINKEY
006900960606     C                   EXSR      ROLL02
007000960606      *  Se non ho dati da modificare vado in immissione
007100960607     C     NRR2          IFEQ      0
007200960607     C                   EXSR      CALLR1
007300960606     C                   ELSE
007400960606     C                   MOVEL     '2'           WTPVID            1
007500960606     C                   END
007600960606     C*
007700960606     C                   ENDSR
007800960606     C*---------------------------------------------------------------*
007900960606     C*  GESD01: Gestione prima videata
008000960606     C*---------------------------------------------------------------*
008100960606     C     GESD01        BEGSR
008200960606     C*
008300960606     C                   EXFMT     TC90D01
008400960606     C*  Fine Lavoro
008500960606     C     *INKC         IFEQ      '1'
008600960606     C                   MOVEL     'S'           WFINE
008700960606     C                   GOTO      FINVD1
008800960606     C                   END
008900960606     C*  Controllo se devo richiamare interrogazione
009000960606     C     '?'           SCAN      V1CCOD                                 32
009100960606     C     *IN32         IFEQ      '1'
009200960606     C                   CLEAR                   TRTC91
009300960606     C                   MOVEL     TRTC91        KPJBU
009400960606     C                   CALL      'TRTC91R'
009500960606     C                   PARM                    KPJBA
009600960607     C                   MOVEL     KPJBU         TRTC91
009700960607     C                   MOVEL     D91COD        V1CCOD
009800960606     C                   MOVEL     D91UNI        V1DCOD
009900960606     C                   GOTO      FINVD1
010000960606     C                   END
010100960606     C*  Controllo se immissione o manutenzione
010200960606     C                   MOVEL     '99'          KCOD
010300960606     C                   MOVEL     *BLANKS       KKEY
010400960606     C                   MOVEL     V1CCOD        KKEY
010500960607     C     KTAB          CHAIN     EDTAB01L                           30
010600960606     C     *IN30         IFEQ      '0'
010700960607     C                   MOVEL     TABFTT        WFTT
010800960607     C                   MOVEL     TABFLT        WFLT
010900040216     C                   MOVEL     TABUNI        ds99
011000040216     c                   if        §99PGM<> *blank
011100040216     C*  Call
011200040216     C                   CALL      §99PGM
011300040216     C                   PARM                    KPJBA
011400040216     c                   else
011500960607     C                   MOVEL     TABUNI        V1DCOD
011600960606     C                   EXSR      INZ02
011700040216     c                   end
011800960606     C                   ELSE
011900960607     C                   EXSR      CALL92
012000960606     C                   END
012100960606     C*
012200960606     C     FINVD1        ENDSR
012300960606     C*---------------------------------------------------------------*
012400960606     C*  GESD02: Gestione seconda videata
012500960606     C*---------------------------------------------------------------*
012600960606     C     GESD02        BEGSR
012700960606     C*
012800960606     C                   WRITE     TC90T02
012900960606     C                   EXFMT     TC90C02
013000960606     C*  Fine Lavoro
013100960606     C     *INKC         IFEQ      '1'
013200960606     C                   MOVEL     'S'           WFINE
013300960606     C                   GOTO      FINVD2
013400960606     C                   END
013500960606     C*  Ritorno
013600960606     C     *INKL         IFEQ      '1'
013700960606     C                   EXSR      INZ01
013800960606     C                   GOTO      FINVD2
013900960606     C                   END
014000960606     C*  Rollup
014100960606     C     *IN25         IFEQ      '1'
014200960606     C                   EXSR      ROLL02
014300960606     C                   GOTO      FINVD2
014400960606     C                   END
014500960606     C*  Inserimento
014600960606     C     *INKJ         IFEQ      '1'
014700960607     C                   EXSR      CALLR1
014800960606     C                   GOTO      FINVD2
014900960606     C                   END
015000960606     C*  Controlli
015100960606     C                   EXSR      CTR02
015200960606     C   28              GOTO      FINVD2
015300960606     C*
015400960606     C     FINVD2        ENDSR
015500960606     C*---------------------------------------------------------------*
015600960606     C*  ROLL02: Caricamento una pagina sufile chiavi singola tabella
015700960606     C*---------------------------------------------------------------*
015800960606     C     ROLL02        BEGSR
015900960606     C*
016000960606     C                   Z-ADD     0             WNRIG2            2 0
016100960606     C                   MOVEL     WINKEY        KKEY
016200960606     C     KTAB          SETGT     EDTAB01L                           30
016300960606     C     KCOD          READE     EDTAB01L                               30
016400000306      *
016500000306     C                   DOW        *IN30 = *off  and  Wnrig2 < 16
016600960606     C                   ADD       1             NRR2              4 0
016700960606     C                   ADD       1             WNRIG2
016800960606     C                   MOVEL     TABKEY        V2CKEY
016900000306     C*?*                MOVEL     TABUNI        V2DCOD
017000000306     C                   If         TABFLG = *blanks
017100000306     C                   clear                   V2CANN
017200000306     C                   Else
017300000306     c                   eval      V2Cann = 'ANNULLATO'
017400000306     C                   Endif
017500040216      *
017600040216     C                   clear                   V2DDES
017700040216      * solo x la CE decodifica
017800040216     c                   if        Kcod   = 'CE'
017900040216     C                   MOVEL     TABUNI        dsce
018000040216     C                   MOVEL     §ceDEC        V2DDES
018100040216     c                   end
018200040216      *
018300040216      * solo x la EA decodifica
018400040216     c                   if        Kcod   = 'EA'
018500040216     C                   MOVEL     TABUNI        dsea
018600040216     C                   MOVEL     §eaDES        V2DDES
018700040216     c                   end
018800040216      *
018900040216      * solo x la EM decodifica
019000040216     c                   if        Kcod   = 'EM'
019100040216     C                   MOVEL     TABUNI        dsem
019200040216     C                   MOVEL     §emDES        V2DDES
019300040216     c                   end
019400040216      *
019500040216      * solo x la FF decodifica
019600040216     c                   if        Kcod   = 'FF'
019700040216     C                   MOVEL     TABUNI        dsff
019800040216     C                   MOVEL     §ffDES        V2DDES
019900040216     c                   end
020000040216      *
020100040216      * solo x la SS decodifica
020200040216     c                   if        Kcod   = 'SS'
020300040216     C                   MOVEL     TABUNI        dsss
020400040216     C                   MOVEL     §ssDES        V2DDES
020500040216     c                   end
020600000306      *
020700040216      * solo x la TB TC decodifica
020800040216     c                   if        Kcod   = 'TB' or Kcod   = 'TC'  or
020900040216     c                             Kcod   = 'MS' or Kcod   = '99'  or
021000040220     c                             Kcod   = '1A' or Kcod   = 'TS'
021100040216     C                   MOVEL     TABUNI        V2DDES
021200040216     c                   end
021300040220     c                   if        Kcod   = 'CL'
021400040220     C                   eval      V2DDES = %Subst(TABUNI: 1: 7) + ' ' +
021500040220     C                             %Subst(TABUNI: 23: 30)
021600040220     c                   end
021700040216      *
021800960606     C                   WRITE     TC90S02
021900000306      *
022000000306     C                   If        Wnrig2 < 16
022100000306     C     KCOD          READE     EDTAB01L                               30
022200000306     c                   Endif
022300000306      *
022400000306     C                   ENDDO
022500000306      *
022600040225     c                   z-add     nrr2          srn1
022700960606     C                   MOVEL     TABKEY        WINKEY
022800960606     C*  Se sono a fine file accendo 23
022900960606     C     *IN30         COMP      '1'                                    23
023000960606     C*
023100960606     C                   ENDSR
023200960606     C*---------------------------------------------------------------*
023300960607     C*  CALL92: Richiamo pgm TRTC92R
023400960606     C*---------------------------------------------------------------*
023500960607     C     CALL92        BEGSR
023600960606     C*
023700960607     C                   CLEAR                   TRTC92
023800960607     C                   MOVEL     V1CCOD        D92COD
023900960607     C                   MOVEL     'I'           D92MOD
024000960607     C                   MOVEL     TRTC92        KPJBU
024100960607     C*  Call
024200960607     C                   CALL      'TRTC92R'
024300960607     C                   PARM                    KPJBA
024400960607     C                   MOVEL     KPJBU         TRTC92
024500960607     C*  Fine Lavoro
024600960607     C     D92CMD        IFEQ      '03'
024700960607     C                   MOVEL     'S'           WFINE
024800960607     C                   GOTO      FICAL1
024900960607     C                   END
025000960607     C*  Ritorno
025100960607     C     D92CMD        IFEQ      '12'
025200960607     C* azzero i campi e riemetto prima videata
025300960607     C                   EXSR      INZ01
025400960607     C                   GOTO      FICAL1
025500960607     C                   END
025600960607     C* richiamo pgm per immissione key
025700960607     C                   MOVEL     D92FTT        WFTT
025800960607     C                   MOVEL     D92FLT        WFLT
025900960607     C                   MOVEL     D92DEC        V1DCOD
026000960607     C                   EXSR      CALLR1
026100960607     C*
026200960607     C     FICAL1        ENDSR
026300960607     C*---------------------------------------------------------------*
026400960607     C*  CALLR1: Richiamo pgm TRTC90R1
026500960607     C*---------------------------------------------------------------*
026600960607     C     CALLR1        BEGSR
026700960607     C*
026800960607     C                   CLEAR                   TRTC90
026900960607     C*  Se vengo da subfile e non h richiesto l'immissione passo la
027000960607     C*  key da manutenzionare
027100960607     C                   MOVEL     'I'           D90MOD
027200960607     C     WTPVID        IFEQ      '2'
027300960607     C     *INKJ         ANDEQ     '0'
027400960607     C                   MOVEL     V2CKEY        D90KEY
027500960607     C                   MOVEL     'M'           D90MOD
027600960607     C                   END
027700960606     C*  Passo altri parametri
027800960607     C                   MOVEL     WFTT          D90FTT
027900960607     C                   MOVEL     WFLT          D90FLT
028000960606     C                   MOVEL     V1CCOD        D90COD
028100960606     C                   MOVEL     V1DCOD        D90DEC
028200960606     C                   MOVEL     TRTC90        KPJBU
028300960606     C*  Call
028400960607     C                   CALL      'TRTC90R1'
028500960606     C                   PARM                    KPJBA
028600960606     C                   MOVEL     KPJBU         TRTC90
028700960606     C*  Fine Lavoro
028800960606     C     D90CMD        IFEQ      '03'
028900960606     C                   MOVEL     'S'           WFINE
029000960606     C                   GOTO      FINCAL
029100960606     C                   END
029200960606     C*  Modifica
029300960606     C     D90CMD        IFEQ      '11'
029400960606     C                   EXSR      INZ02
029500960606     C                   GOTO      FINCAL
029600960606     C                   END
029700960606     C*  Ritorno
029800960606     C     D90CMD        IFEQ      '12'
029900960606     C*  Se vengo da prima videata azzero i campi e la riemetto
030000960606     C     WTPVID        IFEQ      '1'
030100960606     C                   EXSR      INZ01
030200960606     C                   ELSE
030300960606     C*  Altrimenti se sono in immissione rieseguo caricamento sbf
030400960607     C     *INKJ         IFEQ      '1'
030500960606     C                   EXSR      INZ02
030600960606     C                   END
030700960606     C                   END
030800960606     C*
030900960606     C                   END
031000960606     C*
031100960606     C     FINCAL        ENDSR
031200960606     C*---------------------------------------------------------------*
031300960606     C*  CTR02: Controlli seconda videata
031400960606     C*---------------------------------------------------------------*
031500960606     C     CTR02         BEGSR
031600960606     C*
031700960606     C*  Loop letture subfile
031800960607     C                   MOVEL     'N'           WSCE
031900960606     C                   READC     TC90S02                                31
032000960606     C     *IN31         DOWEQ     '0'
032100960606     C     V2CSCE        IFEQ      '1'
032200960607     C                   MOVEL     'S'           WSCE              1
032300960607     C                   EXSR      CALLR1
032400960607     C     NRR2          CHAIN     TC90S02                            31
032500960606     C                   SETON                                        22
032600960606     C                   MOVEL     ' '           V2CSCE
032700960606     C                   UPDATE    TC90S02
032800960606     C                   SETOFF                                       22
032900960606     C                   END
033000960606     C                   READC     TC90S02                                31
033100960606     C                   END
033200960606     C*
033300960607     C     WSCE          IFEQ      'S'
033400960607     C                   EXSR      INZ02
033500960607     C                   END
033600960607     C*
033700960606     C                   ENDSR
033800960606     C*---------------------------------------------------------------*
033900960606     C*  *INZSR: Operazioni iniziali                                  *
034000960606     C*---------------------------------------------------------------*
034100960606     C     *INZSR        BEGSR
034200960606     C*
034300960606     C     *ENTRY        PLIST
034400960606     C                   PARM                    KPJBA
034500960606      *  Call a XPARUT
034600960606     C                   Z-ADD     1             CODUT
034700960606     C                   CALL      'X§PARUT'
034800960606     C                   PARM                    UT§DSE
034900960606     C                   MOVEL     REC80         CNCR80
035000960606     C                   MOVEL     RAGUT         RSUT
035100960606      * Definizione chiavi
035200960606     C     KTAB          KLIST
035300960606     C                   KFLD                    KCOD
035400960606     C                   KFLD                    KKEY
035500960606      * Definizione varibili
035600960606     C     *LIKE         DEFINE    TABCOD        KCOD
035700960606     C     *LIKE         DEFINE    TABKEY        KKEY
035800960607     C     *LIKE         DEFINE    TABFTT        WFTT
035900960607     C     *LIKE         DEFINE    TABFLT        WFLT
036000960606     C     *LIKE         DEFINE    TABKEY        WINKEY
036100960606     C* Inizializzo variabili
036200960606     C                   MOVEL     'N'           WFINE             1
036300960606     C*
036400960606     C                   ENDSR
