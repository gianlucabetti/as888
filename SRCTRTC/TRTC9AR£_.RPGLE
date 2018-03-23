000100960605      ***********************************************************************
000200050223      *   gestione tabella CL                        - E.D.I.   ESTERO -    *
000300960605      ***********************************************************************
000400960605     H DECEDIT('0,') DATEDIT(*DMY.)
000500960605     F*---------------------------------------------------------------------*
000600960605     F*  DATA BASE                                                          *
000700960605     F*---------------------------------------------------------------------*
000800960607     FEDTAB01L  UF A E           K DISK
000900960611     F*---------
001000960612     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
001100960605     F*---------
001200050223     FTRTC9AD   CF   E             WORKSTN
001300960607     D*---------------------------------------------------------------*
001400960607     D*  SCHIERE
001500960607     D*---------------------------------------------------------------*
001600980203     D ERR             S             70    DIM(5) CTDATA PERRCD(1)              Errori
001700960605     D*---------------------------------------------------------------*
001800960605     D*  DS
001900960605     D*---------------------------------------------------------------*
002000960605     D KPJBA         E DS
002100960605     D UT伶SE        E DS                  EXTNAME(UT伶SE0F)
002200960612     D CNCR80        E DS
002300050223     D DSCL          E DS                  EXTNAME(EDIDSCL)
002400050223     D TRTC91        E DS                  EXTNAME(TRTC91DS)
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
005300960612     C                   SETOFF                                       4028
005400050223     C                   MOVEL     *BLANKS       V1Ccod
005500050223     C                   MOVEl     '?'           V1Ccod
005600960606     C                   MOVEL     NOMPGM        V1CPGM
005700960606     C                   MOVEL     '1'           WTPVID            1
005800960606     C*
005900960606     C                   ENDSR
006000960607     C*---------------------------------------------------------------*
006100960607     C*  INZ02: Inizializzo seconda videata
006200960607     C*---------------------------------------------------------------*
006300960607     C     INZ02         BEGSR
006400960607     C*
006500960612     C                   SETOFF                                       404142
006600961021     C                   SETOFF                                       4344
006700960607     C                   SETOFF                                       280102
006800960607     C                   MOVEL     *BLANKS       $MSG
006900960607     C                   MOVEL     NOMPGM        V2CPGM
007000960607     C                   MOVEL     '2'           WTPVID            1
007100960611     C*  Eseguo posizionamento
007200050223     C                   MOVEL     'CL'          KCOD
007300960612     C                   MOVEL     *BLANKS       KKEY
007400050223     C                   MOVEL     V1Ccod        KKEY
007500050223     C                   MOVEL     V1Ccod        v2Ccod
007600960607     C     KTAB          CHAIN     EDTAB01L                           30
007700960607     C     *IN30         IFEQ      '0'
007800960607     C                   SETON                                        02
007900050228     c                   eval      v2cseg = 'RFF+FF:' + %subst(v1ccod:8:15)
008000050223     C                   MOVEL     TABUNI        DSCL
008100050223     C                   MOVEL     低LKSC        V2cKSC
008200050223     C                   MOVEL     低LTAR        V2cTAR
008300050223     C                   MOVEL     低LFTX        V2cFTX
008400050223     C                   MOVEL     低LROB        V2cROB
008500050223     C                   MOVEL     低LLNP        V2cLNP
008600050223     C                   MOVEL     低LNRS        V2cNRS
008700050223     C                   MOVEL     低LCTM        V2cCTM
008800050223     C                   MOVEL     低LBS1        V2cBS1
008900050223     C                   MOVEL     低LDES        V2cDES
009000050223     C                   MOVEL     低LNSP        V2cNSP
009100960607     C                   MOVEL     TABFTT        V2CFTT
009200960607     C                   MOVEL     TABFLT        V2CFLT
009300960607     C     TABFLG        COMP      ' '                                0404
009400960607     C                   ELSE
009500960607     C                   SETON                                        01
009600050223     C                   clear                   V2cKSC
009700050223     C                   clear                   V2cTAR
009800050223     C                   clear                   V2cFTX
009900050223     C                   clear                   V2cROB
010000050223     C                   clear                   V2cLNP
010100050223     C                   clear                   V2cNRS
010200050223     C                   clear                   V2cCTM
010300050223     C                   clear                   V2cBS1
010400050223     C                   clear                   V2cDES
010500050223     C                   clear                   V2cNSP
010600960607     C                   END
010700960607     C*
010800960607     C                   ENDSR
010900960606     C*---------------------------------------------------------------*
011000960606     C*  GESD01: Gestione prima videata
011100960606     C*---------------------------------------------------------------*
011200960606     C     GESD01        BEGSR
011300960606     C*
011400050223     C                   EXFMT     TC9AD01
011500980203     C                   SETOFF                                       4028
011600960606     C*  Fine Lavoro
011700960606     C     *INKC         IFEQ      '1'
011800960606     C                   MOVEL     'S'           WFINE
011900960606     C                   GOTO      FINVD1
012000960606     C                   END
012100960612     C*  Controlli prima videata
012200960612     C                   EXSR      CTR01
012300960607     C*
012400960612     C     *IN28         IFEQ      '1'
012500960612     C     *IN32         OREQ      '1'
012600960612     C                   GOTO      FINVD1
012700960612     C                   END
012800960612     C*
012900960606     C                   EXSR      INZ02
013000960606     C*
013100960606     C     FINVD1        ENDSR
013200960607     C*---------------------------------------------------------------*
013300960607     C*  GESD02: Gestione seconda videata
013400960607     C*---------------------------------------------------------------*
013500960607     C     GESD02        BEGSR
013600960607     C*
013700050223     C                   EXFMT     TC9AD02
013800980203     C                   SETOFF                                       404142
013900080709     C                   SETOFF                                       4344
014000980203     C                   SETOFF                                       28
014100960607     C*  Fine Lavoro
014200960607     C     *INKC         IFEQ      '1'
014300960607     C                   MOVEL     'S'           WFINE
014400960611     C                   GOTO      FINVD2
014500960607     C                   END
014600960607     C*  Ritorno
014700960607     C     *INKL         IFEQ      '1'
014800960607     C                   MOVEL     '1'           WTPVID
014900960607     C                   GOTO      FINVD2
015000960607     C                   END
015100960607     C*  Annullamento
015200960607     C     *INKQ         IFEQ      '1'
015300960607     C                   EXSR      ANNUL
015400960607     C                   GOTO      FINVD2
015500960607     C                   END
015600960607     C*  Controlli
015700960607     C                   EXSR      CTR02
015800960607     C   28              GOTO      FINVD2
015900960607     C*  Ripristino
016000960607     C     *INKE         IFEQ      '1'
016100960607     C                   EXSR      RIPRIS
016200960607     C                   GOTO      FINVD2
016300960607     C                   END
016400960607     C*  Conferma
016500960607     C     *INKF         IFEQ      '1'
016600960607     C                   EXSR      CONFER
016700960612     C                   EXSR      INZ01
016800960607     C                   END
016900960607     C*
017000960607     C     FINVD2        ENDSR
017100960612     C*---------------------------------------------------------------*
017200960612     C*  CTR01: Controlli prima videata
017300960612     C*---------------------------------------------------------------*
017400960612     C     CTR01         BEGSR
017500960612     C*
017600050223     C     '?'           SCAN      V1Ccod                                 32
017700980203     C     *IN32         IFEQ      '1'
017800050223     C                   CLEAR                   TRTC91
017900050223     C                   MOVEL     'CL'          D91COD
018000050223     C                   MOVEL     TRTC91        KPJBU
018100050223     C                   CALL      'TRTC91R1'
018200980203     C                   PARM                    KPJBA
018300050223     C                   MOVEL     KPJBU         TRTC91
018400050223     C                   MOVEL     D91KEY        V1Ccod
018500980203     C                   END
018600050223     C*  Codice Cliente PArticolare
018700050223     C     V1Ccod        IFEQ      *BLANKS
018800960612     C                   SETON                                        4028
018900980203     C                   MOVEL     ERR(1)        $MSG
019000960612     C                   GOTO      FINCT1
019100960612     C                   END
019200960612     C*
019300960612     C     FINCT1        ENDSR
019400960607     C*---------------------------------------------------------------*
019500960607     C*  CTR02: Controlli seconda videata
019600960607     C*---------------------------------------------------------------*
019700960607     C     CTR02         BEGSR
019800050223     C*
019900050223     C*  Controllo se devo immettere il Codice Cliente
020000050223     C     V2cksc        IFEQ      *zeros
020100050223     C                   SETON                                        4028
020200050223     C                   MOVEL     ERR(1)        $MSG
020300050223     C                   GOTO      FINCT2
020400050223     C                   END
020500960607     C*
020600050223     C*  Controllo se devo immettere la Tariffa Cliente
020700050223     C     V2ctar        IFEQ      *blanks
020800050223     C                   SETON                                        4128
020900050223     C                   MOVEL     ERR(1)        $MSG
021000050223     C                   GOTO      FINCT2
021100050223     C                   END
021200050223     C*
021300050223     C*  Controllo se devo immettere la Linea di Partenza
021400050223     C     V2clnp        IFEQ      *zeros
021500050223     C                   SETON                                        4228
021600050223     C                   MOVEL     ERR(1)        $MSG
021700050223     C                   GOTO      FINCT2
021800050223     C                   END
021900050223     C*
022000050223     C*  Controllo se devo immettere il Codice Tratt.Merce
022100050223     C     V2cctm        IFEQ      *blanks
022200050223     C                   SETON                                        4328
022300050223     C                   MOVEL     ERR(1)        $MSG
022400050223     C                   GOTO      FINCT2
022500050223     C                   END
022600050223     C*
022700980203     C*  Controllo se devo immettere la descrizione
022800050223     C     V2cdes        IFEQ      *BLANKS
022900050223     C                   SETON                                        4428
023000050223     C                   MOVEL     ERR(1)        $MSG
023100960612     C                   GOTO      FINCT2
023200960612     C                   END
023300960612     C*  Controllo se filale da indicare
023400960612     C     V2CFTT        IFEQ      '2'
023500960612     C*  Trasmissione solo a  una filiale
023600960612     C     V2CFLT        IFEQ      0
023700980203     C                   SETON                                        4228
023800980203     C                   MOVEL     ERR(3)        $MSG
023900960612     C                   GOTO      FINCT2
024000960612     C                   ELSE
024100960612     C                   Z-ADD     V2CFLT        ORGFIL
024200960612     C     ORGFIL        CHAIN     AZORG01L                           30
024300960612     C     *IN30         IFEQ      '1'
024400960612     C                   SETON                                        4128
024500980203     C                   MOVEL     ERR(4)        $MSG
024600960612     C                   GOTO      FINCT2
024700960612     C                   END
024800960612     C                   END
024900960612     C*  Trasmissione a tutti o a nessuno
025000960612     C                   ELSE
025100960612     C     V2CFLT        IFNE      0
025200960612     C                   SETON                                        4128
025300980203     C                   MOVEL     ERR(5)        $MSG
025400960612     C                   GOTO      FINCT2
025500960612     C                   END
025600960612     C                   END
025700960607     C*
025800960607     C     FINCT2        ENDSR
025900960607     C*---------------------------------------------------------------*
026000960607     C*  ANNUL: Eseguo annullamento tabella                           *
026100960607     C*---------------------------------------------------------------*
026200960607     C     ANNUL         BEGSR
026300960607     C*
026400960607     C                   MOVEL     *BLANKS       KKEY
026500050223     C                   MOVEL     V2Ccod        KKEY
026600050223     C                   MOVEL     'CL'          KCOD
026700960607     C     KTAB          CHAIN     EDTAB01L                           30
026800960607     C     *IN30         IFEQ      '0'
026900960607     C                   MOVEL     '*'           TABFLG
027000960607     C     V2CFTT        IFNE      ' '
027100960607     C                   MOVEL     ' '           TABFTR
027200960607     C                   Z-ADD     0             TABDTR
027300960607     C                   END
027400960607     C                   EXCEPT    TABANN
027500960607     C                   END
027600960611     C*
027700960607     C                   MOVEL     '1'           WTPVID
027800960607     C*
027900960607     C                   ENDSR
028000960607     C*---------------------------------------------------------------*
028100960607     C*  RIPRIS: Ripristino tabella annullata                         *
028200960607     C*---------------------------------------------------------------*
028300960607     C     RIPRIS        BEGSR
028400960607     C*
028500960607     C                   MOVEL     *BLANKS       KKEY
028600050223     C                   MOVEL     V2Ccod        KKEY
028700050223     C                   MOVEL     'CL'          KCOD
028800960607     C     KTAB          CHAIN     EDTAB01L                           30
028900960607     C     *IN30         IFEQ      '0'
029000960607     C                   MOVEL     ' '           TABFLG
029100960607     C     V2CFTT        IFNE      ' '
029200960607     C                   MOVEL     ' '           TABFTR
029300960607     C                   Z-ADD     0             TABDTR
029400960607     C                   END
029500960607     C                   EXCEPT    TABANN
029600960607     C                   END
029700960607     C                   MOVEL     '1'           WTPVID
029800960607     C*
029900960607     C                   ENDSR
030000960607     C*---------------------------------------------------------------*
030100960607     C*  CONFER: Confermo inserimento/variazione                      *
030200960607     C*---------------------------------------------------------------*
030300960607     C     CONFER        BEGSR
030400960607     C*
030500960607     C*  Gestisco scrittura/aggiornamento key
030600960607     C                   MOVEL     *BLANKS       KKEY
030700050223     C                   MOVEL     V2Ccod        KKEY
030800050223     C                   MOVEL     'CL'          KCOD
030900960607     C     KTAB          CHAIN     EDTAB01L                           30
031000050223     C                   MOVEL     V2cKSC        低LKSC
031100050223     C                   MOVEL     V2cTAR        低LTAR
031200050223     C                   MOVEL     V2cFTX        低LFTX
031300050223     C                   MOVEL     V2cROB        低LROB
031400050223     C                   MOVEL     V2cLNP        低LLNP
031500050223     C                   MOVEL     V2cNRS        低LNRS
031600050223     C                   MOVEL     V2cCTM        低LCTM
031700050223     C                   MOVEL     V2cBS1        低LBS1
031800050223     C                   MOVEL     V2cDES        低LDES
031900050223     C                   MOVEL     V2cNSP        低LNSP
032000050223     C                   MOVEL     DSCL          TABUNI
032100960607     C                   MOVEL     V2CFTT        TABFTT
032200960607     C                   Z-ADD     V2CFLT        TABFLT
032300960607     C     V2CFTT        IFNE      ' '
032400960607     C                   MOVEL     ' '           TABFTR
032500960607     C                   Z-ADD     0             TABDTR
032600960607     C                   END
032700960607     C     *IN30         IFEQ      '0'
032800960607     C                   UPDATE    EDTAB000
032900960607     C                   ELSE
033000960612     C                   MOVEL     *BLANKS       TABKEY
033100050223     C                   MOVEL     V2Ccod        TABKEY
033200050223     C                   MOVEL     'CL'          TABCOD
033300960607     C                   WRITE     EDTAB000
033400960607     C                   END
033500960607     C                   MOVEL     '1'           WTPVID
033600960607     C*
033700960607     C                   ENDSR
033800960606     C*---------------------------------------------------------------*
033900960606     C*  *INZSR: Operazioni iniziali                                  *
034000960606     C*---------------------------------------------------------------*
034100960606     C     *INZSR        BEGSR
034200960606     C*
034300960606     C     *ENTRY        PLIST
034400960606     C                   PARM                    KPJBA
034500960606      *  Call a XPARUT
034600960606     C                   Z-ADD     1             CODUT
034700960606     C                   CALL      'X判ARUT'
034800960606     C                   PARM                    UT伶SE
034900960606     C                   MOVEL     REC80         CNCR80
035000960606     C                   MOVEL     RAGUT         RSUT
035100960606      * Definizione chiavi
035200960606     C     KTAB          KLIST
035300960606     C                   KFLD                    KCOD
035400960606     C                   KFLD                    KKEY
035500960606      * Definizione varibili
035600960612     C     *LIKE         DEFINE    TABCOD        KCOD
035700960606     C     *LIKE         DEFINE    TABKEY        KKEY
035800960606     C* Inizializzo variabili
035900960606     C                   MOVEL     'N'           WFINE             1
036000960606     C*
036100960606     C                   ENDSR
036200960607     O*-----------------------------------------------------*
036300960607     OEDTAB000  E            TABANN
036400960607     O                       TABFLG
036500960607     O                       TABFLT
036600960607     O                       TABFTT
036700080709     O                       TABFTR
036800080709     O                       TABDTR
036900980203**         ERR
037000050223Campo Obbligatorio                                                     01
037100050223                                                                       02
037200980203Immettere filiale a cui trasmettere                                    03
037300980203Se trasmissione a tutti o a nessuno impossibile immettere filiale      04
037400980203Filale a cui tramettere inesistente                                    05
