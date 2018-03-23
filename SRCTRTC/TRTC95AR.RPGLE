000100960605      ***********************************************************************
000200960605      *   IMPORTA BOLLE DA PARTNER ESTERI            - E.D.I.   ESTERO -    *
000300960605      ***********************************************************************
000400960605     H DECEDIT('0,') DATEDIT(*DMY.)
000500960605     F*---------------------------------------------------------------------*
000600050823     F*  DATA BASE                                                          *
000700960605     F*---------------------------------------------------------------------*
000800960607     FEDTAB01L  UF A E           K DISK
000900960612     F*---------
001000960612     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
001100960605     F*---------
001200050823     FTRTC95AD  CF   E             WORKSTN
001300960607     D*---------------------------------------------------------------*
001400960607     D*  SCHIERE
001500960607     D*---------------------------------------------------------------*
001600980420     D ERR             S             70    DIM(8) CTDATA PERRCD(1)              Errori
001700960605     D*---------------------------------------------------------------*
001800960605     D*  DS
001900960605     D*---------------------------------------------------------------*
002000960605     D KPJBA         E DS
002100960605     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002200960612     D CNCR80        E DS
002300050823     D DSER          E DS                  EXTNAME(EDIDSER)
002400960611     D TRTC91        E DS                  EXTNAME(TRTC91DS)
002500960605     D                SDS
002600960605     D  NOMPGM                 1     10
002700960612     I*----------------------------------------------------------------
002800960612     I*  FILE
002900960612     I*---------------------------------------------------------------*
003000960612     IAZORG01L  AA
003100960612     I                             P    4    5 0ORGFIL
003200960612     I                                  6    6  ORGFVA
003300960612     I                                 14   33  ORGDES
003400960605     C*---------------------------------------------------------------*
003500960605     C*  CICLO PRINCIPALE
003600960605     C*---------------------------------------------------------------*
003700960611      * Inizializzo prima videata
003800960605     C                   EXSR      INZ01
003900960605      * Loop gestione videate
004000960605     C     WFINE         DOWEQ     'N'
004100960605     C     WTPVID        CASEQ     '1'           GESD01                         Sce.tabella
004200960607     C     WTPVID        CASEQ     '2'           GESD02                         cod.tabella
004300960605     C                   END
004400960605     C                   END
004500960605      * Fine Lavoro
004600960605     C     FINE          TAG
004700960605     C                   SETON                                        LR
004800960605     C*---------------------------------------------------------------*
004900960605     C*  INZ01: Inizializzo prima videata
005000960605     C*---------------------------------------------------------------*
005100960606     C     INZ01         BEGSR
005200960606     C*
005300960612     C                   SETOFF                                       404128
005400050823     C                   MOVEL     'ERRMSG'      V1Ccod
005500960606     C                   MOVEL     NOMPGM        V1CPGM
005600960606     C                   MOVEL     '1'           WTPVID            1
005700960606     C*
005800960606     C                   ENDSR
005900960607     C*---------------------------------------------------------------*
006000960607     C*  INZ02: Inizializzo seconda videata
006100960607     C*---------------------------------------------------------------*
006200960607     C     INZ02         BEGSR
006300960607     C*
006400980420     C                   SETOFF                                       010228
006500961115     C                   SETOFF                                       404142
006600980420     C                   SETOFF                                       43
006700960607     C                   MOVEL     *BLANKS       $MSG
006800960607     C                   MOVEL     NOMPGM        V2CPGM
006900960607     C                   MOVEL     '2'           WTPVID            1
007000960611     C*  Eseguo posizionamento
007100050823     C                   MOVEL     'ER'          KCOD
007200050823     C                   MOVEL     V1Ccod        V2Ccod
007300050823     C                   MOVEL     V1Ccod        WSEI
007400960612     C                   MOVEL     WSEI          KKEY
007500960607     C     KTAB          CHAIN     EDTAB01L                           30
007600960607     C     *IN30         IFEQ      '0'
007700960607     C                   SETON                                        02
007800050823     C                   MOVEL     TABUNI        DSER
007900050823     C                   MOVEL     §ERACT        V2CACT
008000050823     C                   MOVEL     §ERT80        V2CT80
008100050823     C                   MOVEL     §ERD01        V2CD01
008200050823     C                   MOVEL     §ERUSR        V2CUSR
008300050823     C                   MOVEL     §ERORA        V2CORA
008400050823     C                   MOVEL     §ERCER        V2CERR
008500960607     C                   MOVEL     TABFTT        V2CFTT
008600960607     C                   MOVEL     TABFLT        V2CFLT
008700960607     C     TABFLG        COMP      ' '                                0404
008800960607     C                   ELSE
008900960607     C                   SETON                                        01
009000970221     C                   SETOFF                                       04
009100050823     C                   MOVEL     *BLANKS       V2CACT
009200050823     C                   MOVEL     *BLANKS       V2CT80
009300050823     C                   MOVEL     *BLANKS       V2CD01
009400050823     C                   MOVEL     *BLANKS       V2CUSR
009500050823     C                   MOVEL     *BLANKS       V2Cerr
009600050823     C                   MOVEL     *BLANKS       V2Cora
009700960607     C                   MOVEL     *BLANKS       V2CFTT
009800960607     C                   MOVEL     *BLANKS       V2CFLT
009900960607     C                   END
010000960607     C*
010100960607     C                   ENDSR
010200960606     C*---------------------------------------------------------------*
010300960606     C*  GESD01: Gestione prima videata
010400960606     C*---------------------------------------------------------------*
010500960606     C     GESD01        BEGSR
010600960606     C*
010700050823     C                   EXFMT     TC95AD01
010800960612     C                   SETOFF                                       404128
010900960606     C*  Fine Lavoro
011000960606     C     *INKC         IFEQ      '1'
011100960606     C                   MOVEL     'S'           WFINE
011200960606     C                   GOTO      FINVD1
011300960606     C                   END
011400960612     C*  Controlli prima videata
011500960612     C                   EXSR      CTR01
011600960607     C*
011700960612     C     *IN28         IFEQ      '1'
011800960612     C     *IN32         OREQ      '1'
011900960612     C                   GOTO      FINVD1
012000960612     C                   END
012100960612     C*
012200960606     C                   EXSR      INZ02
012300960606     C*
012400960606     C     FINVD1        ENDSR
012500960607     C*---------------------------------------------------------------*
012600960607     C*  GESD02: Gestione seconda videata
012700960607     C*---------------------------------------------------------------*
012800960607     C     GESD02        BEGSR
012900960607     C*
013000050823     C                   EXFMT     TC95AD02
013100961115     C                   SETOFF                                       404142
013200980420     C                   SETOFF                                       4328
013300960607     C*  Fine Lavoro
013400960607     C     *INKC         IFEQ      '1'
013500960607     C                   MOVEL     'S'           WFINE
013600960611     C                   GOTO      FINVD2
013700960607     C                   END
013800960607     C*  Ritorno
013900960607     C     *INKL         IFEQ      '1'
014000960607     C                   MOVEL     '1'           WTPVID
014100960607     C                   GOTO      FINVD2
014200960607     C                   END
014300960607     C*  Annullamento
014400960607     C     *INKQ         IFEQ      '1'
014500960607     C                   EXSR      ANNUL
014600960607     C                   GOTO      FINVD2
014700960607     C                   END
014800960607     C*  Controlli
014900960607     C                   EXSR      CTR02
015000960607     C   28              GOTO      FINVD2
015100960607     C*  Ripristino
015200960607     C     *INKE         IFEQ      '1'
015300960607     C                   EXSR      RIPRIS
015400960607     C                   GOTO      FINVD2
015500960607     C                   END
015600960607     C*  Conferma
015700960607     C     *INKF         IFEQ      '1'
015800960607     C                   EXSR      CONFER
015900960612     C                   EXSR      INZ01
016000960607     C                   END
016100960607     C*
016200960607     C     FINVD2        ENDSR
016300960612     C*---------------------------------------------------------------*
016400960612     C*  CTR01: Controlli prima videata
016500960612     C*---------------------------------------------------------------*
016600960612     C     CTR01         BEGSR
016700960612     C*
016800960612     C*  Controllo se devo richiamare interrogazione
016900050823     C     '?'           SCAN      V1Ccod                                 32
017000960612     C     *IN32         IFEQ      '1'
017100960612     C                   CLEAR                   TRTC91
017200050823     C                   MOVEL     'ER'          D91COD
017300960612     C                   MOVEL     TRTC91        KPJBU
017400960612     C                   CALL      'TRTC91R1'
017500960612     C                   PARM                    KPJBA
017600960612     C                   MOVEL     KPJBU         TRTC91
017700960612     C                   MOVEL     D91KEY        WSEI              6
017800050823     C                   MOVEL     WSEI          V1Ccod
017900960612     C                   END
018000960612     C*  Codice Stato evento
018100050823     C     V1Ccod        IFEQ      *BLANKS
018200960612     C                   SETON                                        4028
018300960612     C                   MOVEL     ERR(1)        $MSG
018400960612     C                   GOTO      FINCT1
018500960612     C                   END
018600960612     C*
018700960612     C     FINCT1        ENDSR
018800960607     C*---------------------------------------------------------------*
018900960607     C*  CTR02: Controlli seconda videata
019000960607     C*---------------------------------------------------------------*
019100960607     C     CTR02         BEGSR
019200960607     C*
019300960612     C*  Codice evento obbligatorio
019400050823     C     V2CUSR        IFEQ      *BLANKS
019500960612     C                   SETON                                        4028
019600050823     C                   MOVEL     ERR(2)        $MSG
019700960611     C                   GOTO      FINCT2
019800960611     C                   END
019900050823      **
020000960607     C*  Controllo se filale da indicare
020100960607     C     V2CFTT        IFEQ      '2'
020200960607     C*  Trasmissione solo a  una filiale
020300960607     C     V2CFLT        IFEQ      0
020400960612     C                   SETON                                        4128
020500960612     C                   MOVEL     ERR(5)        $MSG
020600960607     C                   GOTO      FINCT2
020700960607     C                   ELSE
020800960607     C                   Z-ADD     V2CFLT        ORGFIL
020900960607     C     ORGFIL        CHAIN     AZORG01L                           30
021000960607     C     *IN30         IFEQ      '1'
021100960612     C                   SETON                                        4128
021200960612     C                   MOVEL     ERR(6)        $MSG
021300960607     C                   GOTO      FINCT2
021400960607     C                   END
021500960607     C                   END
021600960607     C*  Trasmissione a tutti o a nessuno
021700960607     C                   ELSE
021800960607     C     V2CFLT        IFNE      0
021900960612     C                   SETON                                        4128
022000960612     C                   MOVEL     ERR(7)        $MSG
022100960607     C                   GOTO      FINCT2
022200960607     C                   END
022300960607     C                   END
022400960607     C*
022500960607     C     FINCT2        ENDSR
022600960607     C*---------------------------------------------------------------*
022700960607     C*  ANNUL: Eseguo annullamento tabella                           *
022800960607     C*---------------------------------------------------------------*
022900960607     C     ANNUL         BEGSR
023000960607     C*
023100960607     C                   MOVEL     *BLANKS       KKEY
023200050823     C                   MOVEL     V2Ccod        WSEI
023300960612     C                   MOVEL     WSEI          KKEY
023400050823     C                   MOVEL     'ER'          KCOD
023500960607     C     KTAB          CHAIN     EDTAB01L                           30
023600960607     C     *IN30         IFEQ      '0'
023700960607     C                   MOVEL     '*'           TABFLG
023800960607     C     V2CFTT        IFNE      ' '
023900960607     C                   MOVEL     ' '           TABFTR
024000960607     C                   Z-ADD     0             TABDTR
024100960607     C                   END
024200960607     C                   EXCEPT    TABANN
024300960607     C                   END
024400960611     C*
024500960607     C                   MOVEL     '1'           WTPVID
024600960607     C*
024700960607     C                   ENDSR
024800960607     C*---------------------------------------------------------------*
024900960607     C*  RIPRIS: Ripristino tabella annullata                         *
025000960607     C*---------------------------------------------------------------*
025100960607     C     RIPRIS        BEGSR
025200960607     C*
025300960607     C                   MOVEL     *BLANKS       KKEY
025400050823     C                   MOVEL     V2Ccod        WSEI
025500960612     C                   MOVEL     WSEI          KKEY
025600050823     C                   MOVEL     'ER'          KCOD
025700960607     C     KTAB          CHAIN     EDTAB01L                           30
025800960607     C     *IN30         IFEQ      '0'
025900960607     C                   MOVEL     ' '           TABFLG
026000960607     C     V2CFTT        IFNE      ' '
026100960607     C                   MOVEL     ' '           TABFTR
026200960607     C                   Z-ADD     0             TABDTR
026300960607     C                   END
026400960607     C                   EXCEPT    TABANN
026500960607     C                   END
026600960607     C                   MOVEL     '1'           WTPVID
026700960607     C*
026800960607     C                   ENDSR
026900960607     C*---------------------------------------------------------------*
027000960607     C*  CONFER: Confermo inserimento/variazione                      *
027100960607     C*---------------------------------------------------------------*
027200960607     C     CONFER        BEGSR
027300960607     C*
027400960607     C*  Gestisco scrittura/aggiornamento key
027500960607     C                   MOVEL     *BLANKS       KKEY
027600050823     C                   MOVEL     V2Ccod        WSEI
027700960612     C                   MOVEL     WSEI          KKEY
027800050823     C                   MOVEL     'ER'          KCOD
027900960607     C     KTAB          CHAIN     EDTAB01L                           30
028000050823     C                   MOVEL     V2CACT        §ERACT
028100050823     C                   MOVEL     V2CT80        §ERT80
028200050823     C                   MOVEL     V2CD01        §ERD01
028300050823     C                   MOVEL     V2CUSR        §ERUSR
028400050823     C                   MOVEL     DSer          TABUNI
028500960607     C                   MOVEL     V2CFTT        TABFTT
028600960607     C                   Z-ADD     V2CFLT        TABFLT
028700960607     C     V2CFTT        IFNE      ' '
028800960607     C                   MOVEL     ' '           TABFTR
028900960607     C                   Z-ADD     0             TABDTR
029000960607     C                   END
029100960607     C     *IN30         IFEQ      '0'
029200960607     C                   UPDATE    EDTAB000
029300960607     C                   ELSE
029400960612     C                   MOVEL     WSEI          TABKEY
029500050823     C                   MOVEL     'ER'          TABCOD
029600960607     C                   WRITE     EDTAB000
029700960607     C                   END
029800960607     C                   MOVEL     '1'           WTPVID
029900960607     C*
030000960607     C                   ENDSR
030100960606     C*---------------------------------------------------------------*
030200960606     C*  *INZSR: Operazioni iniziali                                  *
030300960606     C*---------------------------------------------------------------*
030400960606     C     *INZSR        BEGSR
030500960606     C*
030600960606     C     *ENTRY        PLIST
030700960606     C                   PARM                    KPJBA
030800960606      *  Call a XPARUT
030900960606     C                   Z-ADD     1             CODUT
031000960606     C                   CALL      'X§PARUT'
031100960606     C                   PARM                    UT§DSE
031200960606     C                   MOVEL     REC80         CNCR80
031300960606     C                   MOVEL     RAGUT         RSUT
031400960606      * Definizione chiavi
031500960606     C     KTAB          KLIST
031600960606     C                   KFLD                    KCOD
031700960606     C                   KFLD                    KKEY
031800960606      * Definizione varibili
031900960612     C     *LIKE         DEFINE    TABCOD        KCOD
032000960606     C     *LIKE         DEFINE    TABKEY        KKEY
032100960606     C* Inizializzo variabili
032200960606     C                   MOVEL     'N'           WFINE             1
032300960606     C*
032400960606     C                   ENDSR
032500960607     O*-----------------------------------------------------*
032600960607     OEDTAB000  E            TABANN
032700960607     O                       TABFLG
032800960607     O                       TABFLT
032900960607     O                       TABFTT
033000080709     O                       TABFTR
033100080709     O                       TABDTR
033200980420**         ERR
033300050823Codice Tabella       obbligatorio                                      01
033400050823Immettere UTENTE  a cui inviare il messaggio                           02
033500050823                                                                       03
033600050823                                                                       04
033700050823Immettere Filiale a cui trasmettere                                    05
033800960612Se trasmissione a tutti o a nessuno impossibile immettere filiale      06
033900960612Filale a cui tramettere inesistente                                    07
034000980420Scelta tipologie evento non congruenti                                 08
