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
002100960605     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002200960612     D CNCR80        E DS
002300050223     D DSCL          E DS                  EXTNAME(EDIDSCL)
002400050223     D TRTC91        E DS                  EXTNAME(TRTC91DS)
002500960605     D                SDS
002600960605     D  NOMPGM                 1     10
002601160209     d digits          c                   '0123456789'
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
006600160209     C                   SETOFF                                       434445
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
008100050223     C                   MOVEL     §CLKSC        V2cKSC
008200050223     C                   MOVEL     §CLTAR        V2cTAR
008300160209     C                   MOVEL     §CLFTX        V2cFGS
008400050223     C                   MOVEL     §CLROB        V2cROB
008500050223     C                   MOVEL     §CLLNP        V2cLNP
008600050223     C                   MOVEL     §CLNRS        V2cNRS
008700050223     C                   MOVEL     §CLCTM        V2cCTM
008800050223     C                   MOVEL     §CLBS1        V2cBS1
008900050223     C                   MOVEL     §CLDES        V2cDES
009000050223     C                   MOVEL     §CLNSP        V2cNSP
009100110406     C                   MOVEL     §CLTIC        V2cTIC
009200960607     C                   MOVEL     TABFTT        V2CFTT
009300960607     C                   MOVEL     TABFLT        V2CFLT
009400960607     C     TABFLG        COMP      ' '                                0404
009500960607     C                   ELSE
009600960607     C                   SETON                                        01
009700050223     C                   clear                   V2cKSC
009800050223     C                   clear                   V2cTAR
010000050223     C                   clear                   V2cROB
010100050223     C                   clear                   V2cLNP
010101160209     C                   clear                   V2cFGS
010200050223     C                   clear                   V2cNRS
010300050223     C                   clear                   V2cCTM
010400050223     C                   clear                   V2cBS1
010500050223     C                   clear                   V2cDES
010600050223     C                   clear                   V2cNSP
010700110406     C                   clear                   V2cTIC
010800960607     C                   END
010900960607     C*
011000960607     C                   ENDSR
011100960606     C*---------------------------------------------------------------*
011200960606     C*  GESD01: Gestione prima videata
011300960606     C*---------------------------------------------------------------*
011400960606     C     GESD01        BEGSR
011500960606     C*
011600050223     C                   EXFMT     TC9AD01
011700980203     C                   SETOFF                                       4028
011800960606     C*  Fine Lavoro
011900960606     C     *INKC         IFEQ      '1'
012000960606     C                   MOVEL     'S'           WFINE
012100960606     C                   GOTO      FINVD1
012200960606     C                   END
012300960612     C*  Controlli prima videata
012400960612     C                   EXSR      CTR01
012500960607     C*
012600960612     C     *IN28         IFEQ      '1'
012700960612     C     *IN32         OREQ      '1'
012800960612     C                   GOTO      FINVD1
012900960612     C                   END
013000960612     C*
013100960606     C                   EXSR      INZ02
013200960606     C*
013300960606     C     FINVD1        ENDSR
013400960607     C*---------------------------------------------------------------*
013500960607     C*  GESD02: Gestione seconda videata
013600960607     C*---------------------------------------------------------------*
013700960607     C     GESD02        BEGSR
013800960607     C*
013900050223     C                   EXFMT     TC9AD02
014000980203     C                   SETOFF                                       404142
014100160209     C                   SETOFF                                       434445
014200980203     C                   SETOFF                                       28
014300960607     C*  Fine Lavoro
014400960607     C     *INKC         IFEQ      '1'
014500960607     C                   MOVEL     'S'           WFINE
014600960611     C                   GOTO      FINVD2
014700960607     C                   END
014800960607     C*  Ritorno
014900960607     C     *INKL         IFEQ      '1'
015000960607     C                   MOVEL     '1'           WTPVID
015100960607     C                   GOTO      FINVD2
015200960607     C                   END
015300960607     C*  Annullamento
015400960607     C     *INKQ         IFEQ      '1'
015500960607     C                   EXSR      ANNUL
015600960607     C                   GOTO      FINVD2
015700960607     C                   END
015800960607     C*  Controlli
015900960607     C                   EXSR      CTR02
016000960607     C   28              GOTO      FINVD2
016100960607     C*  Ripristino
016200960607     C     *INKE         IFEQ      '1'
016300960607     C                   EXSR      RIPRIS
016400960607     C                   GOTO      FINVD2
016500960607     C                   END
016600960607     C*  Conferma
016700960607     C     *INKF         IFEQ      '1'
016800960607     C                   EXSR      CONFER
016900960612     C                   EXSR      INZ01
017000960607     C                   END
017100960607     C*
017200960607     C     FINVD2        ENDSR
017300960612     C*---------------------------------------------------------------*
017400960612     C*  CTR01: Controlli prima videata
017500960612     C*---------------------------------------------------------------*
017600960612     C     CTR01         BEGSR
017700960612     C*
017800050223     C     '?'           SCAN      V1Ccod                                 32
017900980203     C     *IN32         IFEQ      '1'
018000050223     C                   CLEAR                   TRTC91
018100050223     C                   MOVEL     'CL'          D91COD
018200050223     C                   MOVEL     TRTC91        KPJBU
018300050223     C                   CALL      'TRTC91R1'
018400980203     C                   PARM                    KPJBA
018500050223     C                   MOVEL     KPJBU         TRTC91
018600050223     C                   MOVEL     D91KEY        V1Ccod
018700980203     C                   END
018800050223     C*  Codice Cliente PArticolare
018900050223     C     V1Ccod        IFEQ      *BLANKS
019000960612     C                   SETON                                        4028
019100980203     C                   MOVEL     ERR(1)        $MSG
019200960612     C                   GOTO      FINCT1
019300960612     C                   END
019400960612     C*
019500960612     C     FINCT1        ENDSR
019600960607     C*---------------------------------------------------------------*
019700960607     C*  CTR02: Controlli seconda videata
019800960607     C*---------------------------------------------------------------*
019900960607     C     CTR02         BEGSR
020000050223     C*
020100050223     C*  Controllo se devo immettere il Codice Cliente
020200050223     C     V2cksc        IFEQ      *zeros
020300050223     C                   SETON                                        4028
020400050223     C                   MOVEL     ERR(1)        $MSG
020500050223     C                   GOTO      FINCT2
020600050223     C                   END
020700960607     C*
020800050223     C*  Controllo se devo immettere la Tariffa Cliente
020900050223     C     V2ctar        IFEQ      *blanks
021000050223     C                   SETON                                        4128
021100050223     C                   MOVEL     ERR(1)        $MSG
021200050223     C                   GOTO      FINCT2
021300050223     C                   END
021400050223     C*
021500050223     C*  Controllo se devo immettere la Linea di Partenza
021600050223     C     V2clnp        IFEQ      *zeros
021700160209     C                   CLEAR                   V2CLNP
022000050223     C                   END
022100050223     C*
022101160209     C*  Controllo sulla Filiale di Gestione
022102160209     C                   IF        V2cfgs <> *blank and V2cfgs <> *ZEROS AND
022103160209     C                             V2cfgs = %editc(V2clnp:'X')
022104160209     C                   SETON                                        4528
022105160209     C                   MOVEL     ERR(2)        $MSG
022106160209     C                   GOTO      FINCT2
022107160209     C                   END
022108160209      *
022109160209      *  controllo validità campo Filiale
022110160209     C                   IF        V2cfgs <> *blank and V2cfgs <> *ZEROS
022111160209     c     digits        check     V2cfgs
022112160209     c                   if        %Found
022115160209     C                   SETON                                        4528
022116160209     C                   MOVEL     ERR(2)        $MSG
022117160209     C                   GOTO      FINCT2
022118160209     c                   end
022119160209      *
022122160209     C                   movel     V2CFGS        ORGFIL
022123160209     C     ORGFIL        CHAIN     AZORG01L
022124160209     C                   IF        not %found(AZORG01L)
022125160209     C                   SETON                                        4528
022126160209     C                   MOVEL     ERR(2)        $MSG
022127160209     C                   GOTO      FINCT2
022128160209     C                   END
022129160209     C                   END
022130160209     C*
022200050223     C*  Controllo se devo immettere il Codice Tratt.Merce
022300050223     C     V2cctm        IFEQ      *blanks
022400050223     C                   SETON                                        4328
022500050223     C                   MOVEL     ERR(1)        $MSG
022600050223     C                   GOTO      FINCT2
022700050223     C                   END
022800050223     C*
022900980203     C*  Controllo se devo immettere la descrizione
023000050223     C     V2cdes        IFEQ      *BLANKS
023100050223     C                   SETON                                        4428
023200050223     C                   MOVEL     ERR(1)        $MSG
023300960612     C                   GOTO      FINCT2
023400960612     C                   END
023500960612     C*  Controllo se filale da indicare
023600960612     C     V2CFTT        IFEQ      '2'
023700960612     C*  Trasmissione solo a  una filiale
023800960612     C     V2CFLT        IFEQ      0
023900980203     C                   SETON                                        4228
024000980203     C                   MOVEL     ERR(3)        $MSG
024100960612     C                   GOTO      FINCT2
024200960612     C                   ELSE
024300960612     C                   Z-ADD     V2CFLT        ORGFIL
024400960612     C     ORGFIL        CHAIN     AZORG01L                           30
024500960612     C     *IN30         IFEQ      '1'
024600960612     C                   SETON                                        4128
024700980203     C                   MOVEL     ERR(4)        $MSG
024800960612     C                   GOTO      FINCT2
024900960612     C                   END
025000960612     C                   END
025100960612     C*  Trasmissione a tutti o a nessuno
025200960612     C                   ELSE
025300960612     C     V2CFLT        IFNE      0
025400960612     C                   SETON                                        4128
025500980203     C                   MOVEL     ERR(5)        $MSG
025600960612     C                   GOTO      FINCT2
025700960612     C                   END
025800960612     C                   END
025900960607     C*
026000960607     C     FINCT2        ENDSR
026100960607     C*---------------------------------------------------------------*
026200960607     C*  ANNUL: Eseguo annullamento tabella                           *
026300960607     C*---------------------------------------------------------------*
026400960607     C     ANNUL         BEGSR
026500960607     C*
026600960607     C                   MOVEL     *BLANKS       KKEY
026700050223     C                   MOVEL     V2Ccod        KKEY
026800050223     C                   MOVEL     'CL'          KCOD
026900960607     C     KTAB          CHAIN     EDTAB01L                           30
027000960607     C     *IN30         IFEQ      '0'
027100960607     C                   MOVEL     '*'           TABFLG
027200960607     C     V2CFTT        IFNE      ' '
027300960607     C                   MOVEL     ' '           TABFTR
027400960607     C                   Z-ADD     0             TABDTR
027500960607     C                   END
027600960607     C                   EXCEPT    TABANN
027700960607     C                   END
027800960611     C*
027900960607     C                   MOVEL     '1'           WTPVID
028000960607     C*
028100960607     C                   ENDSR
028200960607     C*---------------------------------------------------------------*
028300960607     C*  RIPRIS: Ripristino tabella annullata                         *
028400960607     C*---------------------------------------------------------------*
028500960607     C     RIPRIS        BEGSR
028600960607     C*
028700960607     C                   MOVEL     *BLANKS       KKEY
028800050223     C                   MOVEL     V2Ccod        KKEY
028900050223     C                   MOVEL     'CL'          KCOD
029000960607     C     KTAB          CHAIN     EDTAB01L                           30
029100960607     C     *IN30         IFEQ      '0'
029200960607     C                   MOVEL     ' '           TABFLG
029300960607     C     V2CFTT        IFNE      ' '
029400960607     C                   MOVEL     ' '           TABFTR
029500960607     C                   Z-ADD     0             TABDTR
029600960607     C                   END
029700960607     C                   EXCEPT    TABANN
029800960607     C                   END
029900960607     C                   MOVEL     '1'           WTPVID
030000960607     C*
030100960607     C                   ENDSR
030200960607     C*---------------------------------------------------------------*
030300960607     C*  CONFER: Confermo inserimento/variazione                      *
030400960607     C*---------------------------------------------------------------*
030500960607     C     CONFER        BEGSR
030600960607     C*
030700960607     C*  Gestisco scrittura/aggiornamento key
030800960607     C                   MOVEL     *BLANKS       KKEY
030900050223     C                   MOVEL     V2Ccod        KKEY
031000050223     C                   MOVEL     'CL'          KCOD
031100960607     C     KTAB          CHAIN     EDTAB01L                           30
031200050223     C                   MOVEL     V2cKSC        §CLKSC
031300050223     C                   MOVEL     V2cTAR        §CLTAR
031400160209     C                   IF        V2cFGS ='000'
031401160209     C                   CLEAR                   V2CFGS
031402160209     C                   END
031403160209     C                   MOVEL     V2cFGS        §CLFTX
031500050223     C                   MOVEL     V2cROB        §CLROB
031600050223     C                   MOVEL     V2cLNP        §CLLNP
031700050223     C                   MOVEL     V2cNRS        §CLNRS
031800050223     C                   MOVEL     V2cCTM        §CLCTM
031900110406     C                   MOVEL     V2cTIC        §CLTIC
032000050223     C                   MOVEL     V2cBS1        §CLBS1
032100050223     C                   MOVEL     V2cDES        §CLDES
032200050223     C                   MOVEL     V2cNSP        §CLNSP
032300050223     C                   MOVEL     DSCL          TABUNI
032400960607     C                   MOVEL     V2CFTT        TABFTT
032500960607     C                   Z-ADD     V2CFLT        TABFLT
032600960607     C     V2CFTT        IFNE      ' '
032700960607     C                   MOVEL     ' '           TABFTR
032800960607     C                   Z-ADD     0             TABDTR
032900960607     C                   END
033000960607     C     *IN30         IFEQ      '0'
033100960607     C                   UPDATE    EDTAB000
033200960607     C                   ELSE
033300960612     C                   MOVEL     *BLANKS       TABKEY
033400050223     C                   MOVEL     V2Ccod        TABKEY
033500050223     C                   MOVEL     'CL'          TABCOD
033600960607     C                   WRITE     EDTAB000
033700960607     C                   END
033800960607     C                   MOVEL     '1'           WTPVID
033900960607     C*
034000960607     C                   ENDSR
034100960606     C*---------------------------------------------------------------*
034200960606     C*  *INZSR: Operazioni iniziali                                  *
034300960606     C*---------------------------------------------------------------*
034400960606     C     *INZSR        BEGSR
034500960606     C*
034600960606     C     *ENTRY        PLIST
034700960606     C                   PARM                    KPJBA
034800960606      *  Call a XPARUT
034900960606     C                   Z-ADD     1             CODUT
035000960606     C                   CALL      'X§PARUT'
035100960606     C                   PARM                    UT§DSE
035200960606     C                   MOVEL     REC80         CNCR80
035300960606     C                   MOVEL     RAGUT         RSUT
035400960606      * Definizione chiavi
035500960606     C     KTAB          KLIST
035600960606     C                   KFLD                    KCOD
035700960606     C                   KFLD                    KKEY
035800960606      * Definizione varibili
035900960612     C     *LIKE         DEFINE    TABCOD        KCOD
036000960606     C     *LIKE         DEFINE    TABKEY        KKEY
036100960606     C* Inizializzo variabili
036200960606     C                   MOVEL     'N'           WFINE             1
036300960606     C*
036400960606     C                   ENDSR
036500960607     O*-----------------------------------------------------*
036600960607     OEDTAB000  E            TABANN
036700960607     O                       TABFLG
036800960607     O                       TABFLT
036900960607     O                       TABFTT
037000080709     O                       TABFTR
037100080709     O                       TABDTR
037200980203**         ERR
037300050223Campo Obbligatorio                                                     01
037400160209Filale non corretta o Uguale alla LNP!                                 02
037500980203Immettere filiale a cui trasmettere                                    03
037600980203Se trasmissione a tutti o a nessuno impossibile immettere filiale      04
037700980203Filale a cui tramettere inesistente                                    05
