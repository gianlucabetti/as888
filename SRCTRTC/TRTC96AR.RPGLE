000100960605      ***********************************************************************
000200960605      *   IMPORTA BOLLE DA PARTNER ESTERI            - E.D.I.   ESTERO -    *
000300960605      ***********************************************************************
000400960605     H DECEDIT('0,') DATEDIT(*DMY.)
000500960605     F*---------------------------------------------------------------------*
000600960605     F*  DATA BASE                                                          *
000700960605     F*---------------------------------------------------------------------*
000800960607     FEDTAB01L  UF A E           K DISK
000900960612     F*---------
001000960612     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
001100960605     F*---------
001200050823     FTRTC96AD  CF   E             WORKSTN
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
002300050823     D DSGG          E DS                  EXTNAME(EDIDSGG)
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
005400050823     C                   eval      V1Ccod = 'PULISNDRCV'
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
007100050823     C                   MOVEL     'GG'          KCOD
007200050823     C                   MOVEL     V1Ccod        V2Ccod
007300050823     C                   MOVEL(p)  V1Ccod        KKEY
007400960607     C     KTAB          CHAIN     EDTAB01L                           30
007500960607     C     *IN30         IFEQ      '0'
007600960607     C                   SETON                                        02
007700050823     C                   MOVEL     TABUNI        DSgg
007800050823     C                   MOVEL     §ggdtr        V2Cdtr
007900050823     C                   MOVEL     §ggggr        V2Cggr
008000050823     C                   MOVEL     §ggdts        V2Cdts
008100050823     C                   MOVEL     §ggggs        V2Cggs
008200960607     C                   MOVEL     TABFTT        V2CFTT
008300960607     C                   MOVEL     TABFLT        V2CFLT
008400960607     C     TABFLG        COMP      ' '                                0404
008500960607     C                   ELSE
008600960607     C                   SETON                                        01
008700970221     C                   SETOFF                                       04
008800050823     C                   MOVEL     *zeros        V2Cdtr
008900050823     C                   MOVEL     *zeros        V2Cggr
009000050823     C                   MOVEL     *zeros        V2Cdts
009100050823     C                   MOVEL     *zeros        V2Cggs
009200960607     C                   MOVEL     *BLANKS       V2CFTT
009300960607     C                   MOVEL     *BLANKS       V2CFLT
009400960607     C                   END
009500960607     C*
009600960607     C                   ENDSR
009700960606     C*---------------------------------------------------------------*
009800960606     C*  GESD01: Gestione prima videata
009900960606     C*---------------------------------------------------------------*
010000960606     C     GESD01        BEGSR
010100960606     C*
010200050823     C                   EXFMT     TC96AD01
010300960612     C                   SETOFF                                       404128
010400960606     C*  Fine Lavoro
010500960606     C     *INKC         IFEQ      '1'
010600960606     C                   MOVEL     'S'           WFINE
010700960606     C                   GOTO      FINVD1
010800960606     C                   END
010900960612     C*  Controlli prima videata
011000960612     C                   EXSR      CTR01
011100960607     C*
011200960612     C     *IN28         IFEQ      '1'
011300960612     C     *IN32         OREQ      '1'
011400960612     C                   GOTO      FINVD1
011500960612     C                   END
011600960612     C*
011700960606     C                   EXSR      INZ02
011800960606     C*
011900960606     C     FINVD1        ENDSR
012000960607     C*---------------------------------------------------------------*
012100960607     C*  GESD02: Gestione seconda videata
012200960607     C*---------------------------------------------------------------*
012300960607     C     GESD02        BEGSR
012400960607     C*
012500050823     C                   EXFMT     TC96AD02
012600961115     C                   SETOFF                                       404142
012700980420     C                   SETOFF                                       4328
012800960607     C*  Fine Lavoro
012900960607     C     *INKC         IFEQ      '1'
013000960607     C                   MOVEL     'S'           WFINE
013100960611     C                   GOTO      FINVD2
013200960607     C                   END
013300960607     C*  Ritorno
013400960607     C     *INKL         IFEQ      '1'
013500960607     C                   MOVEL     '1'           WTPVID
013600960607     C                   GOTO      FINVD2
013700960607     C                   END
013800960607     C*  Annullamento
013900960607     C     *INKQ         IFEQ      '1'
014000960607     C                   EXSR      ANNUL
014100960607     C                   GOTO      FINVD2
014200960607     C                   END
014300960607     C*  Controlli
014400960607     C                   EXSR      CTR02
014500960607     C   28              GOTO      FINVD2
014600960607     C*  Ripristino
014700960607     C     *INKE         IFEQ      '1'
014800960607     C                   EXSR      RIPRIS
014900960607     C                   GOTO      FINVD2
015000960607     C                   END
015100960607     C*  Conferma
015200960607     C     *INKF         IFEQ      '1'
015300960607     C                   EXSR      CONFER
015400960612     C                   EXSR      INZ01
015500960607     C                   END
015600960607     C*
015700960607     C     FINVD2        ENDSR
015800960612     C*---------------------------------------------------------------*
015900960612     C*  CTR01: Controlli prima videata
016000960612     C*---------------------------------------------------------------*
016100960612     C     CTR01         BEGSR
016200960612     C*
016300960612     C*  Controllo se devo richiamare interrogazione
016400050823     C     '?'           SCAN      V1Ccod                                 32
016500960612     C     *IN32         IFEQ      '1'
016600960612     C                   CLEAR                   TRTC91
016700050823     C                   MOVEL     'GG'          D91COD
016800960612     C                   MOVEL     TRTC91        KPJBU
016900960612     C                   CALL      'TRTC91R1'
017000960612     C                   PARM                    KPJBA
017100960612     C                   MOVEL     KPJBU         TRTC91
017200960612     C                   MOVEL     D91KEY        WSEI              6
017300050823     C                   MOVEL     WSEI          V1Ccod
017400960612     C                   END
017500960612     C*  Codice Stato evento
017600050823     C     V1Ccod        IFEQ      *BLANKS
017700960612     C                   SETON                                        4028
017800960612     C                   MOVEL     ERR(1)        $MSG
017900960612     C                   GOTO      FINCT1
018000960612     C                   END
018100960612     C*
018200960612     C     FINCT1        ENDSR
018300960607     C*---------------------------------------------------------------*
018400960607     C*  CTR02: Controlli seconda videata
018500960607     C*---------------------------------------------------------------*
018600960607     C     CTR02         BEGSR
018700960607     C*
018800960607     C*  Controllo se filale da indicare
018900960607     C     V2CFTT        IFEQ      '2'
019000960607     C*  Trasmissione solo a  una filiale
019100960607     C     V2CFLT        IFEQ      0
019200960612     C                   SETON                                        4128
019300960612     C                   MOVEL     ERR(5)        $MSG
019400960607     C                   GOTO      FINCT2
019500960607     C                   ELSE
019600960607     C                   Z-ADD     V2CFLT        ORGFIL
019700960607     C     ORGFIL        CHAIN     AZORG01L                           30
019800960607     C     *IN30         IFEQ      '1'
019900960612     C                   SETON                                        4128
020000960612     C                   MOVEL     ERR(6)        $MSG
020100960607     C                   GOTO      FINCT2
020200960607     C                   END
020300960607     C                   END
020400960607     C*  Trasmissione a tutti o a nessuno
020500960607     C                   ELSE
020600960607     C     V2CFLT        IFNE      0
020700960612     C                   SETON                                        4128
020800960612     C                   MOVEL     ERR(7)        $MSG
020900960607     C                   GOTO      FINCT2
021000960607     C                   END
021100960607     C                   END
021200960607     C*
021300960607     C     FINCT2        ENDSR
021400960607     C*---------------------------------------------------------------*
021500960607     C*  ANNUL: Eseguo annullamento tabella                           *
021600960607     C*---------------------------------------------------------------*
021700960607     C     ANNUL         BEGSR
021800960607     C*
021900050823     C                   MOVEL(p)  V2Ccod        KKEY
022000050823     C                   MOVEL     'GG'          KCOD
022100960607     C     KTAB          CHAIN     EDTAB01L                           30
022200960607     C     *IN30         IFEQ      '0'
022300960607     C                   MOVEL     '*'           TABFLG
022400960607     C     V2CFTT        IFNE      ' '
022500960607     C                   MOVEL     ' '           TABFTR
022600960607     C                   Z-ADD     0             TABDTR
022700960607     C                   END
022800960607     C                   EXCEPT    TABANN
022900960607     C                   END
023000960611     C*
023100960607     C                   MOVEL     '1'           WTPVID
023200960607     C*
023300960607     C                   ENDSR
023400960607     C*---------------------------------------------------------------*
023500960607     C*  RIPRIS: Ripristino tabella annullata                         *
023600960607     C*---------------------------------------------------------------*
023700960607     C     RIPRIS        BEGSR
023800960607     C*
023900050823     C                   MOVEL(p)  V2Ccod        KKEY
024000050823     C                   MOVEL     'GG'          KCOD
024100960607     C     KTAB          CHAIN     EDTAB01L                           30
024200960607     C     *IN30         IFEQ      '0'
024300960607     C                   MOVEL     ' '           TABFLG
024400960607     C     V2CFTT        IFNE      ' '
024500960607     C                   MOVEL     ' '           TABFTR
024600960607     C                   Z-ADD     0             TABDTR
024700960607     C                   END
024800960607     C                   EXCEPT    TABANN
024900960607     C                   END
025000960607     C                   MOVEL     '1'           WTPVID
025100960607     C*
025200960607     C                   ENDSR
025300960607     C*---------------------------------------------------------------*
025400960607     C*  CONFER: Confermo inserimento/variazione                      *
025500960607     C*---------------------------------------------------------------*
025600960607     C     CONFER        BEGSR
025700960607     C*
025800960607     C*  Gestisco scrittura/aggiornamento key
025900050823     C                   MOVEL(p)  V2Ccod        KKEY
026000050823     C                   MOVEL     'GG'          KCOD
026100960607     C     KTAB          CHAIN     EDTAB01L                           30
026200050823     C                   MOVEL     V2Cdtr        §ggdtr
026300050823     C                   MOVEL     V2Cggr        §ggggr
026400050823     C                   MOVEL     V2Cdts        §ggdts
026500050823     C                   MOVEL     V2Cggs        §ggggs
026600050823     C                   MOVEL     DSgg          TABUNI
026700960607     C                   MOVEL     V2CFTT        TABFTT
026800960607     C                   Z-ADD     V2CFLT        TABFLT
026900960607     C     V2CFTT        IFNE      ' '
027000960607     C                   MOVEL     ' '           TABFTR
027100960607     C                   Z-ADD     0             TABDTR
027200960607     C                   END
027300960607     C     *IN30         IFEQ      '0'
027400960607     C                   UPDATE    EDTAB000
027500960607     C                   ELSE
027600050823     C                   MOVEL     kkey          TABKEY
027700050823     C                   MOVEL     'GG'          TABCOD
027800960607     C                   WRITE     EDTAB000
027900960607     C                   END
028000960607     C                   MOVEL     '1'           WTPVID
028100960607     C*
028200960607     C                   ENDSR
028300960606     C*---------------------------------------------------------------*
028400960606     C*  *INZSR: Operazioni iniziali                                  *
028500960606     C*---------------------------------------------------------------*
028600960606     C     *INZSR        BEGSR
028700960606     C*
028800960606     C     *ENTRY        PLIST
028900960606     C                   PARM                    KPJBA
029000960606      *  Call a XPARUT
029100960606     C                   Z-ADD     1             CODUT
029200960606     C                   CALL      'X§PARUT'
029300960606     C                   PARM                    UT§DSE
029400960606     C                   MOVEL     REC80         CNCR80
029500960606     C                   MOVEL     RAGUT         RSUT
029600960606      * Definizione chiavi
029700960606     C     KTAB          KLIST
029800960606     C                   KFLD                    KCOD
029900960606     C                   KFLD                    KKEY
030000960606      * Definizione varibili
030100960612     C     *LIKE         DEFINE    TABCOD        KCOD
030200960606     C     *LIKE         DEFINE    TABKEY        KKEY
030300960606     C* Inizializzo variabili
030400960606     C                   MOVEL     'N'           WFINE             1
030500960606     C*
030600960606     C                   ENDSR
030700960607     O*-----------------------------------------------------*
030800960607     OEDTAB000  E            TABANN
030900960607     O                       TABFLG
031000960607     O                       TABFLT
031100960607     O                       TABFTT
031200080709     O                       TABFTR
031300080709     O                       TABDTR
031400980420**         ERR
031500050823Codice Tabella       obbligatorio                                      01
031600050823                                                                       02
031700050823                                                                       03
031800050823                                                                       04
031900050823Immettere Filiale a cui trasmettere                                    05
032000960612Se trasmissione a tutti o a nessuno impossibile immettere filiale      06
032100960612Filale a cui tramettere inesistente                                    07
032200980420Scelta tipologie evento non congruenti                                 08
