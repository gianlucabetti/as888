000100960605      ***********************************************************************
000200960605      *   IMPORTA BOLLE DA PARTNER ESTERI            - E.D.I.   ESTERO -    *
000300960605      ***********************************************************************
000400960605     H DECEDIT('0,') DATEDIT(*DMY.)
000500960605     F*---------------------------------------------------------------------*
000600960605     F*  DATA BASE                                                          *
000700960605     F*---------------------------------------------------------------------*
000800960607     FEDTAB01L  UF A E           K DISK
000900960607     F*---------
001000960607     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
001100960605     F*---------
001200960606     FTRTC90D   CF   E             WORKSTN
001300960607     D*---------------------------------------------------------------*
001400960607     D*  SCHIERE
001500960607     D*---------------------------------------------------------------*
001600960607     D ERR             S             70    DIM(5) CTDATA PERRCD(1)              Errori
001700960605     D*---------------------------------------------------------------*
001800960605     D*  DS
001900960605     D*---------------------------------------------------------------*
002000960605     D KPJBA         E DS
002100960605     D UT�DSE        E DS                  EXTNAME(UT�DSE0F)
002200960606     D CNCR80        E DS
002300960606     D TRTC90        E DS                  EXTNAME(TRTC90DS)
002400960605     D                SDS
002500960605     D  NOMPGM                 1     10
002600960607     I*----------------------------------------------------------------
002700960607     I*  FILE
002800960607     I*---------------------------------------------------------------*
002900960607     IAZORG01L  AA
003000960607     I                             P    4    5 0ORGFIL
003100960605     C*---------------------------------------------------------------*
003200960605     C*  CICLO PRINCIPALE
003300960605     C*---------------------------------------------------------------*
003400960607      * Inizializzo terza videata
003500960607     C                   EXSR      INZ03
003600960605      * Loop gestione videate
003700960605     C     WFINE         DOWEQ     'N'
003800960607     C     WTPVID        CASEQ     '3'           GESD03                         cod.tabella
003900960605     C                   END
004000960605     C                   END
004100960605      * Fine Lavoro
004200960605     C     FINE          TAG
004300960606     C                   MOVEL     TRTC90        KPJBU
004400960605     C                   SETON                                        LR
004500960607     C*---------------------------------------------------------------*
004600960607     C*  INZ03: Inizializzo terza videata
004700960607     C*---------------------------------------------------------------*
004800960607     C     INZ03         BEGSR
004900960607     C*
005000960606     C                   SETOFF                                       404142
005100960606     C                   SETOFF                                       28
005200960606     C                   MOVEL     *BLANKS       $MSG
005300960607     C                   MOVEL     D90COD        V3CCOD
005400960607     C                   MOVEL     D90DEC        V3DCOD
005500960607     C                   MOVEL     NOMPGM        V3CPGM
005600960607     C                   MOVEL     '3'           WTPVID            1
005700960606     C*  Se sono in manutenzione imposto chiave e valore
005800960606     C     WMODAL        IFEQ      'M'
005900960606     C                   SETOFF                                       01
006000960607     C                   MOVEL     *BLANKS       KKEY
006100960607     C                   MOVEL     D90KEY        KKEY
006200960607     C                   MOVEL     D90COD        KCOD
006300960607     C                   MOVEL     D90KEY        V3CKEY
006400960606     C     KTAB          CHAIN     EDTAB01L                           30
006500960607     C                   MOVEL     TABUNI        V3CDE1
006600960607     C                   MOVE      TABUNI        V3CDE2
006700960607     C                   MOVEL     TABFTT        V3CFTT
006800960607     C                   Z-ADD     TABFLT        V3CFLT
006900960606     C     TABFLG        COMP      ' '                                0404
007000960606     C                   ELSE
007100960606     C                   SETON                                        01
007200960607     C                   MOVEL     *BLANKS       V3CDE1
007300960607     C                   MOVEL     *BLANKS       V3CDE2
007400960607     C                   MOVEL     *BLANKS       V3CKEY
007500960607     C                   MOVEL     D90FTT        V3CFTT
007600960607     C                   Z-ADD     D90FLT        V3CFLT
007700960606     C                   END
007800960606     C*
007900960606     C                   ENDSR
008000960607     C*---------------------------------------------------------------*
008100960607     C*  GESD03: Gestione terza videata
008200960607     C*---------------------------------------------------------------*
008300960607     C     GESD03        BEGSR
008400960607     C*
008500960607     C                   EXFMT     TC90D03
008600960607     C                   SETOFF                                       284041
008700960607     C                   SETOFF                                       4243
008800960606     C*  Fine Lavoro
008900960606     C     *INKC         IFEQ      '1'
009000960606     C                   MOVEL     'S'           WFINE
009100960607     C                   MOVEL     '03'          D90CMD
009200960607     C                   GOTO      FINVD3
009300960606     C                   END
009400960606     C*  Ritorno
009500960606     C     *INKL         IFEQ      '1'
009600960606     C                   MOVEL     'S'           WFINE
009700960606     C                   MOVEL     '12'          D90CMD
009800960607     C                   GOTO      FINVD3
009900960607     C                   END
010000960606     C*  Inserimento
010100960606     C     *INKJ         IFEQ      '1'
010200960606     C                   MOVEL     'I'           WMODAL
010300960607     C                   EXSR      INZ03
010400960606     C                   GOTO      FINVD3
010500960606     C                   END
010600960606     C*  Modifica
010700960606     C     *INKK         IFEQ      '1'
010800960607     C                   MOVEL     '11'          D90CMD
010900960607     C                   MOVEL     'S'           WFINE
011000960607     C                   GOTO      FINVD3
011100960606     C                   END
011200960606     C*  Annullamento
011300960606     C     *INKQ         IFEQ      '1'
011400960606     C                   EXSR      ANNUL
011500960607     C                   GOTO      FINVD3
011600960606     C                   END
011700960606     C*  Controlli
011800960607     C                   EXSR      CTR03
011900960607     C   28              GOTO      FINVD3
012000960606     C*  Ripristino
012100960606     C     *INKE         IFEQ      '1'
012200960606     C                   EXSR      RIPRIS
012300960607     C                   GOTO      FINVD3
012400960606     C                   END
012500960606     C*  Conferma
012600960606     C     *INKF         IFEQ      '1'
012700960606     C                   EXSR      CONFER
012800960606     C                   END
012900960606     C*
013000960606     C     FINVD3        ENDSR
013100960607     C*---------------------------------------------------------------*
013200960607     C*  CTR03: Controlli terza videata
013300960607     C*---------------------------------------------------------------*
013400960607     C     CTR03         BEGSR
013500960607     C*
013600960606     C*  Descrizione obbligatoria
013700960607     C     V3CDE1        IFEQ      *BLANKS
013800960607     C     V3CDE2        ANDEQ     *BLANKS
013900960606     C                   SETON                                        414228
014000960607     C                   MOVEL     ERR(1)        $MSG
014100960607     C                   GOTO      FINCT3
014200960606     C                   END
014300960606     C*  Se sono in inserimento controllo che non esista gi�
014400960606     C     WMODAL        IFEQ      'I'
014500960607     C                   MOVEL     *BLANKS       KKEY
014600960607     C                   MOVEL     V3CCOD        KCOD
014700960607     C                   MOVEL     V3CKEY        KKEY
014800960606     C     KTAB          CHAIN     EDTAB01L                           30
014900960607     C     *IN30         IFEQ      '0'
015000960606     C                   SETON                                        4028
015100960607     C                   MOVEL     ERR(2)        $MSG
015200960607     C                   GOTO      FINCT3
015300960606     C                   END
015400960606     C                   END
015500960607     C*  Controllo se filale da indicare
015600960607     C     V3CFTT        IFEQ      '2'
015700960607     C*  Trasmissione solo a  una filiale
015800960607     C     V3CFLT        IFEQ      0
015900960607     C                   SETON                                        4328
016000960607     C                   MOVEL     ERR(3)        $MSG
016100960607     C                   GOTO      FINCT3
016200960607     C                   ELSE
016300960607     C                   Z-ADD     V3CFLT        ORGFIL
016400960607     C     ORGFIL        CHAIN     AZORG01L                           30
016500960607     C     *IN30         IFEQ      '1'
016600960607     C                   SETON                                        4328
016700960607     C                   MOVEL     ERR(5)        $MSG
016800960607     C                   GOTO      FINCT3
016900960607     C                   END
017000960607     C                   END
017100960607     C*  Trasmissione a tutti o a nessuno
017200960607     C                   ELSE
017300960607     C     V3CFLT        IFNE      0
017400960607     C                   SETON                                        4328
017500960607     C                   MOVEL     ERR(4)        $MSG
017600960607     C                   GOTO      FINCT3
017700960607     C                   END
017800960607     C                   END
017900960606     C*
018000960607     C     FINCT3        ENDSR
018100960607     C*---------------------------------------------------------------*
018200960607     C*  ANNUL: Eseguo annullamento tabella                           *
018300960607     C*---------------------------------------------------------------*
018400960607     C     ANNUL         BEGSR
018500960607     C*
018600960607     C                   MOVEL     *BLANKS       KKEY
018700960607     C                   MOVEL     V3CCOD        KCOD
018800960607     C                   MOVEL     V3CKEY        KKEY
018900960607     C     KTAB          CHAIN     EDTAB01L                           30
019000960607     C     *IN30         IFEQ      '0'
019100960607     C                   MOVEL     '*'           TABFLG
019200960607     C     V3CFTT        IFNE      ' '
019300960607     C                   MOVEL     ' '           TABFTR
019400960607     C                   Z-ADD     0             TABDTR
019500960607     C                   END
019600960607     C                   EXCEPT    TABANN
019700960607     C                   END
019800960607     C*  Vado a fine
019900960607     C                   MOVEL     'S'           WFINE
020000960607     C                   MOVEL     '16'          D90CMD
020100960607     C*
020200960607     C                   ENDSR
020300960607     C*---------------------------------------------------------------*
020400960607     C*  RIPRIS: Ripristino tabella annullata                         *
020500960607     C*---------------------------------------------------------------*
020600960607     C     RIPRIS        BEGSR
020700960607     C*
020800960607     C                   MOVEL     *BLANKS       KKEY
020900960607     C                   MOVEL     V3CCOD        KCOD
021000960607     C                   MOVEL     V3CKEY        KKEY
021100960607     C     KTAB          CHAIN     EDTAB01L                           30
021200960607     C     *IN30         IFEQ      '0'
021300960607     C                   MOVEL     ' '           TABFLG
021400960607     C     V3CFTT        IFNE      ' '
021500960607     C                   MOVEL     ' '           TABFTR
021600960607     C                   Z-ADD     0             TABDTR
021700960607     C                   END
021800960607     C                   EXCEPT    TABANN
021900960607     C                   END
022000960607     C*  Vado a fine
022100960607     C                   MOVEL     'S'           WFINE
022200960607     C                   MOVEL     '16'          D90CMD
022300960607     C*
022400960607     C                   ENDSR
022500960607     C*---------------------------------------------------------------*
022600960607     C*  CONFER: Confermo inserimento/variazione                      *
022700960607     C*---------------------------------------------------------------*
022800960607     C     CONFER        BEGSR
022900960607     C*
023000960607     C*  Gestisco scrittura/aggiornamento key
023100960607     C                   MOVEL     *BLANKS       KKEY
023200960607     C                   MOVEL     V3CCOD        KCOD
023300960607     C                   MOVEL     V3CKEY        KKEY
023400960607     C     KTAB          CHAIN     EDTAB01L                           30
023500960607     C                   MOVEL     V3CDE1        TABUNI
023600960607     C                   MOVE      V3CDE2        TABUNI
023700960607     C                   MOVEL     V3CFTT        TABFTT
023800960607     C                   Z-ADD     V3CFLT        TABFLT
023900960607     C     V3CFTT        IFNE      ' '
024000960607     C                   MOVEL     ' '           TABFTR
024100960607     C                   Z-ADD     0             TABDTR
024200960607     C                   END
024300960607     C     *IN30         IFEQ      '0'
024400960607     C                   UPDATE    EDTAB000
024500960607     C                   ELSE
024600960607     C                   MOVEL     V3CKEY        TABKEY
024700960607     C                   MOVEL     V3CCOD        TABCOD
024800960607     C                   WRITE     EDTAB000
024900960607     C                   END
025000960607     C*  Se sono in inserimento --> loop videata 4
025100960607     C     WMODAL        IFEQ      'I'
025200960607     C                   EXSR      INZ03
025300960607     C                   ELSE
025400960607     C*  Vado a fine
025500960607     C                   MOVEL     'S'           WFINE
025600960607     C                   MOVEL     '16'          D90CMD
025700960607     C                   END
025800960607     C*
025900960607     C                   ENDSR
026000960606     C*---------------------------------------------------------------*
026100960606     C*  *INZSR: Operazioni iniziali                                  *
026200960606     C*---------------------------------------------------------------*
026300960606     C     *INZSR        BEGSR
026400960606     C*
026500960606     C     *ENTRY        PLIST
026600960606     C                   PARM                    KPJBA
026700960607     C                   MOVEL     KPJBU         TRTC90
026800960606      *  Call a XPARUT
026900960606     C                   Z-ADD     1             CODUT
027000960606     C                   CALL      'X�PARUT'
027100960606     C                   PARM                    UT�DSE
027200960606     C                   MOVEL     REC80         CNCR80
027300960606     C                   MOVEL     RAGUT         RSUT
027400960606      * Definizione chiavi
027500960606     C     KTAB          KLIST
027600960606     C                   KFLD                    KCOD
027700960606     C                   KFLD                    KKEY
027800960606      * Definizione varibili
027900960606     C     *LIKE         DEFINE    TABCOD        KCOD
028000960606     C     *LIKE         DEFINE    TABKEY        KKEY
028100960606     C* Inizializzo variabili
028200960606     C                   MOVEL     'N'           WFINE             1
028300960607     C                   MOVEL     D90MOD        WMODAL            1
028400960606     C*
028500960606     C                   ENDSR
028600960607     O*-----------------------------------------------------*
028700960607     OEDTAB000  E            TABANN
028800960607     O                       TABFLG
028900960607     O                       TABFLT
029000960607     O                       TABFTT
029100000306     O                       TABFTR
029200000306     O                       TABDTR
029300960607**         ERR
029400960607Descrizione codice obbligatoria
029500960607Tabella gi� esistente
029600960607Immettere filiale a cui trasmettere
029700960607Se trasmissione a tutti o a nessuno impossibile immettere filiale
029800960607Filale a cui tramettere inesistente
