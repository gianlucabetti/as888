000100960605      ***********************************************************************
000200960605      *   IMPORTA BOLLE DA PARTNER ESTERI            - E.D.I.   ESTERO -    *
000300960605      ***********************************************************************
000400960605     H DECEDIT('0,') DATEDIT(*DMY.)
000500960605     F*---------------------------------------------------------------------*
000600960605     F*  DATA BASE                                                          *
000700960605     F*---------------------------------------------------------------------*
000800960607     FEDTAB01L  UF A E           K DISK
000900960611     F*---------
001000960611     FTABEL00F  IF   E           K DISK
001100960612     F*---------
001200960612     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
001300960605     F*---------
001400980203     FTRTC96D   CF   E             WORKSTN
001500960607     D*---------------------------------------------------------------*
001600960607     D*  SCHIERE
001700960607     D*---------------------------------------------------------------*
001800960612     D C2A             S              3    DIM(200)                             Cod.EVENTO BART
001900150728     D ERR             S             70    DIM(7) CTDATA PERRCD(1)              Errori
002000960605     D*---------------------------------------------------------------*
002100960605     D*  DS
002200960605     D*---------------------------------------------------------------*
002300960605     D KPJBA         E DS
002400960605     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002500960612     D CNCR80        E DS
002600980203     D DSEM          E DS                  EXTNAME(EDIDSEM)
002700960611     D TRTC91        E DS                  EXTNAME(TRTC91DS)
002800960605     D                SDS
002900960605     D  NOMPGM                 1     10
003000960612     I*----------------------------------------------------------------
003100960612     I*  FILE
003200960612     I*---------------------------------------------------------------*
003300960612     IAZORG01L  AA
003400960612     I                             P    4    5 0ORGFIL
003500960612     I                                  6    6  ORGFVA
003600960612     I                                 14   33  ORGDES
003700960605     C*---------------------------------------------------------------*
003800960605     C*  CICLO PRINCIPALE
003900960605     C*---------------------------------------------------------------*
004000960611      * Inizializzo prima videata
004100960605     C                   EXSR      INZ01
004200960605      * Loop gestione videate
004300960605     C     WFINE         DOWEQ     'N'
004400960605     C     WTPVID        CASEQ     '1'           GESD01                         Sce.tabella
004500960607     C     WTPVID        CASEQ     '2'           GESD02                         cod.tabella
004600960605     C                   END
004700960605     C                   END
004800960605      * Fine Lavoro
004900960605     C     FINE          TAG
005000960605     C                   SETON                                        LR
005100960605     C*---------------------------------------------------------------*
005200960605     C*  INZ01: Inizializzo prima videata
005300960605     C*---------------------------------------------------------------*
005400960606     C     INZ01         BEGSR
005500960606     C*
005600960612     C                   SETOFF                                       4028
005700980203     C                   MOVEL     *BLANKS       V1CSTS
005800980203     C                   MOVE      ' ?'          V1CSTS
005900980203     C                   MOVEL     *BLANKS       V1CCEV
006000960606     C                   MOVEL     NOMPGM        V1CPGM
006100960606     C                   MOVEL     '1'           WTPVID            1
006200960606     C*
006300960606     C                   ENDSR
006400960607     C*---------------------------------------------------------------*
006500960607     C*  INZ02: Inizializzo seconda videata
006600960607     C*---------------------------------------------------------------*
006700960607     C     INZ02         BEGSR
006800960607     C*
006900960612     C                   SETOFF                                       404142
007000150728     C                   SETOFF                                       434445
007100960607     C                   SETOFF                                       280102
007200960607     C                   MOVEL     *BLANKS       $MSG
007300960607     C                   MOVEL     NOMPGM        V2CPGM
007400960607     C                   MOVEL     '2'           WTPVID            1
007500960611     C*  Eseguo posizionamento
007600980203     C                   MOVEL     'EM'          KCOD
007700960612     C                   MOVEL     *BLANKS       KKEY
007800980203     C                   MOVEL     V1CSTS        WCOMO6            6
007900980203     C                   MOVE      V1CCEV        WCOMO6            6
008000980203     C                   MOVEL     WCOMO6        KKEY
008100980203     C                   MOVEL     V1CSTS        V2CSTS
008200980203     C                   MOVEL     V1CCEV        V2CCEV
008300960607     C     KTAB          CHAIN     EDTAB01L                           30
008400960607     C     *IN30         IFEQ      '0'
008500960607     C                   SETON                                        02
008600980203     C                   MOVEL     TABUNI        DSEM
008700980203     C                   MOVEL     §EMDES        V2DCEV
008800980203     C                   MOVEL     §EMLAB        V2CLAB
008900980203     C                   MOVEL     §EMVGL        V2CVGL
009000980203     C                   MOVEL     §EMTOT        V2CTOT
009100980203     C                   MOVEL     §EMTRA        V2CTRA
009200980203     C                   MOVEL     §EMASS        V2CBPA
009300980203     C                   MOVEL     §EMBLP        V2CBLP
009400980203     C                   MOVEL     §EMBLT        V2CBLT
009500980203     C                   MOVEL     §EMCCA        V2CCCA
009600990610     C                   MOVEL     §EMCAC        V2CCAC
009700980203     C                   MOVEL     §EMFOR        V2CFOR
009800980203     C                   MOVEL     §EMCOD        V2CC2A
009900150728     C                   MOVEL     §EMMAI        V2CMAI
010000960607     C                   MOVEL     TABFTT        V2CFTT
010100960607     C                   MOVEL     TABFLT        V2CFLT
010200960607     C     TABFLG        COMP      ' '                                0404
010300960607     C                   ELSE
010400960607     C                   SETON                                        01
010500960607     C                   MOVEL     *BLANKS       V2CFTT
010600960607     C                   MOVEL     *BLANKS       V2CFLT
010700980203     C                   MOVEL     *BLANKS       V2DCEV
010800980203     C                   MOVEL     *BLANKS       V2CLAB
010900980203     C                   MOVEL     *BLANKS       V2CVGL
011000980203     C                   MOVEL     *BLANKS       V2CTOT
011100980203     C                   MOVEL     *BLANKS       V2CTRA
011200980203     C                   MOVEL     *BLANKS       V2CBPA
011300980203     C                   MOVEL     *BLANKS       V2CBLP
011400980203     C                   MOVEL     *BLANKS       V2CBLT
011500980203     C                   MOVEL     *BLANKS       V2CCCA
011600990610     C                   MOVEL     *BLANKS       V2CCAC
011700980203     C                   MOVEL     *BLANKS       V2CFOR
011800980203     C                   MOVEL     *BLANKS       V2CC2A
011900960607     C                   END
012000960607     C*
012100960607     C                   ENDSR
012200960606     C*---------------------------------------------------------------*
012300960606     C*  GESD01: Gestione prima videata
012400960606     C*---------------------------------------------------------------*
012500960606     C     GESD01        BEGSR
012600960606     C*
012700980203     C                   EXFMT     TC96D01
012800980203     C                   SETOFF                                       4028
012900960606     C*  Fine Lavoro
013000960606     C     *INKC         IFEQ      '1'
013100960606     C                   MOVEL     'S'           WFINE
013200960606     C                   GOTO      FINVD1
013300960606     C                   END
013400960612     C*  Controlli prima videata
013500960612     C                   EXSR      CTR01
013600960607     C*
013700960612     C     *IN28         IFEQ      '1'
013800960612     C     *IN32         OREQ      '1'
013900960612     C                   GOTO      FINVD1
014000960612     C                   END
014100960612     C*
014200960606     C                   EXSR      INZ02
014300960606     C*
014400960606     C     FINVD1        ENDSR
014500960607     C*---------------------------------------------------------------*
014600960607     C*  GESD02: Gestione seconda videata
014700960607     C*---------------------------------------------------------------*
014800960607     C     GESD02        BEGSR
014900960607     C*
015000980203     C                   EXFMT     TC96D02
015100980203     C                   SETOFF                                       404142
015101150728     C                   SETOFF                                       434445
015200980203     C                   SETOFF                                       28
015300960607     C*  Fine Lavoro
015400960607     C     *INKC         IFEQ      '1'
015500960607     C                   MOVEL     'S'           WFINE
015600960611     C                   GOTO      FINVD2
015700960607     C                   END
015800960607     C*  Ritorno
015900960607     C     *INKL         IFEQ      '1'
016000960607     C                   MOVEL     '1'           WTPVID
016100960607     C                   GOTO      FINVD2
016200960607     C                   END
016300960607     C*  Annullamento
016400960607     C     *INKQ         IFEQ      '1'
016500960607     C                   EXSR      ANNUL
016600960607     C                   GOTO      FINVD2
016700960607     C                   END
016800960607     C*  Controlli
016900960607     C                   EXSR      CTR02
017000960607     C   28              GOTO      FINVD2
017100960607     C*  Ripristino
017200960607     C     *INKE         IFEQ      '1'
017300960607     C                   EXSR      RIPRIS
017400960607     C                   GOTO      FINVD2
017500960607     C                   END
017600960607     C*  Conferma
017700960607     C     *INKF         IFEQ      '1'
017800960607     C                   EXSR      CONFER
017900960612     C                   EXSR      INZ01
018000960607     C                   END
018100960607     C*
018200960607     C     FINVD2        ENDSR
018300960612     C*---------------------------------------------------------------*
018400960612     C*  CTR01: Controlli prima videata
018500960612     C*---------------------------------------------------------------*
018600960612     C     CTR01         BEGSR
018700960612     C*
018800980203     C     '?'           SCAN      V1CSTS                                 32
018900980203     C     *IN32         IFEQ      '1'
019000980203     C                   CLEAR                   TRTC91
019100980203     C                   MOVEL     'EM'          D91COD
019200980203     C                   MOVEL     TRTC91        KPJBU
019300980203     C                   CALL      'TRTC91R1'
019400980203     C                   PARM                    KPJBA
019500980203     C                   MOVEL     KPJBU         TRTC91
019600980203     C                   MOVEL     D91KEY        WSEI              6
019700980203     C                   MOVEL     WSEI          V1CSTS
019800980203     C                   MOVE      WSEI          V1CCEV
019900980203     C                   END
020000960612     C*  Codice evento obbligatorio
020100980203     C     V1CSTS        IFEQ      *BLANKS
020200980203     C     V1CCEV        ANDEQ     *BLANKS
020300960612     C                   SETON                                        4028
020400980203     C                   MOVEL     ERR(1)        $MSG
020500960612     C                   GOTO      FINCT1
020600960612     C                   END
020700960612     C*
020800960612     C     FINCT1        ENDSR
020900960607     C*---------------------------------------------------------------*
021000960607     C*  CTR02: Controlli seconda videata
021100960607     C*---------------------------------------------------------------*
021200960607     C     CTR02         BEGSR
021300960607     C*
021400980203     C*  Controllo se devo immettere la descrizione
021500980203     C     V2DCEV        IFEQ      *BLANKS
021600960612     C                   SETON                                        4028
021700980203     C                   MOVEL     ERR(2)        $MSG
021800960612     C                   GOTO      FINCT2
021900960612     C                   END
022000980203     C*  Ricerca Codice Evento 2A
022100980203     C     '?'           SCAN      V2CC2A                                 32
022200980203     C     *IN32         IFEQ      '1'
022300980203     C                   Z-ADD     1             §KUT
022400980203     C                   MOVEL     '2A'          §COD
022500980203     C                   CALL      'X§TABER'
022600980203     C                   PARM                    §KUT
022700980203     C                   PARM                    §COD
022800980203     C                   PARM                    §KEY
022900980203     C                   PARM                    §DES             25
023000980203     C                   MOVEL     §KEY          V2CC2A
023100980203     C                   END
023200980203     C*  Codice evento Bartolini
023300980203     C     V2CC2A        IFNE      *BLANKS
023400980203     C     V2CC2A        LOOKUP    C2A                                    32
023500980204     C     *IN32         IFEQ      '0'
023600960612     C                   SETON                                        4128
023700960612     C                   MOVEL     ERR(2)        $MSG
023800960612     C                   GOTO      FINCT2
023900960612     C                   END
024000980203     C                   END
024100150728     C* Controlli per "Chiusura bolla per merce mai affidata"= 'S'
024201150728     c                   if        v2cmai='S'
024202150729     C                   MOVEL     V2CSTS        WCOMO6
024203150729     C                   MOVE      V2CCEV        WCOMO6
024204150729     c* deve essere l'unico evento ad averlo
024205150729     c     'EM'          setll     edtab01l
024206150729     c     'EM'          reade(N)  edtab01l
024207150729     c                   dow       not %eof(edtab01l)
024208150729     c                   if        %subst(tabkey:1:6)<>wcomo6
024209150729     c                   eval      dsem=tabuni
024210150729     c                   if        §emmai='S'
024211150729     c                   seton                                        4528
024212150729     C                   MOVEL     ERR(7)        $MSG
024213150729     c                   eval      $msg=%trim($msg)+' '+%subst(tabkey:1:3) +
024214150729     c                                              ' '+%subst(tabkey:4:3) +
024215150729     c                                              '-'+§emdes
024216150729     C                   GOTO      FINCT2
024217150729     c                   endif
024218150729     c                   endif
024219150729     c     'EM'          reade(N)  edtab01l
024220150729     c                   enddo
024221150729     c* ri-chaino il record in manutenzione
024222150729     c                   if        *in02
024223150729     C                   MOVEL     'EM'          KCOD
024224150729     C                   MOVEL     WCOMO6        KKEY
024225150729     C     KTAB          CHAIN     EDTAB01L                           30
024226150729     c                   endif
024227150728     c
024228150728     c* flag "agg bolla come consegnata" e "agg il codice consegna anomala"
024229150728     c* devono essere = 'S'
024230150728     c                   if        v2cblp=' '
024231150728     c                   seton                                        4328
024232150728     C                   MOVEL     ERR(6)        $MSG
024233150728     C                   GOTO      FINCT2
024234150728     c                   endif
024235150728     c                   if        v2ccca=' '
024236150728     c                   seton                                        4428
024237150728     C                   MOVEL     ERR(6)        $MSG
024238150728     C                   GOTO      FINCT2
024239150728     c                   endif
024240150728     c                   endif
024300960612     C*  Controllo se filale da indicare
024400960612     C     V2CFTT        IFEQ      '2'
024500960612     C*  Trasmissione solo a  una filiale
024600960612     C     V2CFLT        IFEQ      0
024700980203     C                   SETON                                        4228
024800980203     C                   MOVEL     ERR(3)        $MSG
024900960612     C                   GOTO      FINCT2
025000960612     C                   ELSE
025100960612     C                   Z-ADD     V2CFLT        ORGFIL
025200960612     C     ORGFIL        CHAIN     AZORG01L                           30
025300960612     C     *IN30         IFEQ      '1'
025400960612     C                   SETON                                        4128
025500980203     C                   MOVEL     ERR(4)        $MSG
025600960612     C                   GOTO      FINCT2
025700960612     C                   END
025800960612     C                   END
025900960612     C*  Trasmissione a tutti o a nessuno
026000960612     C                   ELSE
026100960612     C     V2CFLT        IFNE      0
026200960612     C                   SETON                                        4128
026300980203     C                   MOVEL     ERR(5)        $MSG
026400960612     C                   GOTO      FINCT2
026500960612     C                   END
026600960612     C                   END
026700960607     C*
026800960607     C     FINCT2        ENDSR
026900960607     C*---------------------------------------------------------------*
027000960607     C*  ANNUL: Eseguo annullamento tabella                           *
027100960607     C*---------------------------------------------------------------*
027200960607     C     ANNUL         BEGSR
027300960607     C*
027400960607     C                   MOVEL     *BLANKS       KKEY
027500960612     C                   MOVEL     V2CC2A        KKEY
027600980203     C                   MOVEL     'EM'          KCOD
027700960607     C     KTAB          CHAIN     EDTAB01L                           30
027800960607     C     *IN30         IFEQ      '0'
027900960607     C                   MOVEL     '*'           TABFLG
028000960607     C     V2CFTT        IFNE      ' '
028100960607     C                   MOVEL     ' '           TABFTR
028200960607     C                   Z-ADD     0             TABDTR
028300960607     C                   END
028400960607     C                   EXCEPT    TABANN
028500960607     C                   END
028600960611     C*
028700960607     C                   MOVEL     '1'           WTPVID
028800960607     C*
028900960607     C                   ENDSR
029000960607     C*---------------------------------------------------------------*
029100960607     C*  RIPRIS: Ripristino tabella annullata                         *
029200960607     C*---------------------------------------------------------------*
029300960607     C     RIPRIS        BEGSR
029400960607     C*
029500960607     C                   MOVEL     *BLANKS       KKEY
029600960612     C                   MOVEL     V2CC2A        KKEY
029700980203     C                   MOVEL     'EM'          KCOD
029800960607     C     KTAB          CHAIN     EDTAB01L                           30
029900960607     C     *IN30         IFEQ      '0'
030000960607     C                   MOVEL     ' '           TABFLG
030100960607     C     V2CFTT        IFNE      ' '
030200960607     C                   MOVEL     ' '           TABFTR
030300960607     C                   Z-ADD     0             TABDTR
030400960607     C                   END
030500960607     C                   EXCEPT    TABANN
030600960607     C                   END
030700960607     C                   MOVEL     '1'           WTPVID
030800960607     C*
030900960607     C                   ENDSR
031000960607     C*---------------------------------------------------------------*
031100960607     C*  CONFER: Confermo inserimento/variazione                      *
031200960607     C*---------------------------------------------------------------*
031300960607     C     CONFER        BEGSR
031400960607     C*
031500960607     C*  Gestisco scrittura/aggiornamento key
031600960607     C                   MOVEL     *BLANKS       KKEY
031700980204     C                   MOVEL     V2CSTS        WCOMO6
031800980204     C                   MOVE      V2CCEV        WCOMO6
031900980204     C                   MOVEL     WCOMO6        KKEY
032000980203     C                   MOVEL     'EM'          KCOD
032100960607     C     KTAB          CHAIN     EDTAB01L                           30
032200980203     C                   MOVEL     V2DCEV        §EMDES
032300980203     C                   MOVEL     V2CLAB        §EMLAB
032400980203     C                   MOVEL     V2CVGL        §EMVGL
032500980203     C                   MOVEL     V2CTOT        §EMTOT
032600980203     C                   MOVEL     V2CTRA        §EMTRA
032700980203     C                   MOVEL     V2CBPA        §EMASS
032800980203     C                   MOVEL     V2CBLP        §EMBLP
032900980203     C                   MOVEL     V2CBLT        §EMBLT
033000980203     C                   MOVEL     V2CCCA        §EMCCA
033100990610     C                   MOVEL     V2CCAC        §EMCAC
033200980203     C                   MOVEL     V2CFOR        §EMFOR
033300980203     C                   MOVEL     V2CC2A        §EMCOD
033400150728     C                   MOVEL     V2CMAI        §EMMAI
033500980203     C                   MOVEL     DSEM          TABUNI
033600960607     C                   MOVEL     V2CFTT        TABFTT
033700960607     C                   Z-ADD     V2CFLT        TABFLT
033800960607     C     V2CFTT        IFNE      ' '
033900960607     C                   MOVEL     ' '           TABFTR
034000960607     C                   Z-ADD     0             TABDTR
034100960607     C                   END
034200960607     C     *IN30         IFEQ      '0'
034300960607     C                   UPDATE    EDTAB000
034400960607     C                   ELSE
034500960612     C                   MOVEL     *BLANKS       TABKEY
034600980203     C                   MOVEL     V2CSTS        WCOMO6
034700980203     C                   MOVE      V2CCEV        WCOMO6
034800980203     C                   MOVEL     WCOMO6        TABKEY
034900980203     C                   MOVEL     'EM'          TABCOD
035000960607     C                   WRITE     EDTAB000
035100960607     C                   END
035200960607     C                   MOVEL     '1'           WTPVID
035300960607     C*
035400960607     C                   ENDSR
035500960606     C*---------------------------------------------------------------*
035600960606     C*  *INZSR: Operazioni iniziali                                  *
035700960606     C*---------------------------------------------------------------*
035800960606     C     *INZSR        BEGSR
035900960606     C*
036000960606     C     *ENTRY        PLIST
036100960606     C                   PARM                    KPJBA
036200960606      *  Call a XPARUT
036300960606     C                   Z-ADD     1             CODUT
036400960606     C                   CALL      'X§PARUT'
036500960606     C                   PARM                    UT§DSE
036600960606     C                   MOVEL     REC80         CNCR80
036700960606     C                   MOVEL     RAGUT         RSUT
036800960606      * Definizione chiavi
036900960606     C     KTAB          KLIST
037000960606     C                   KFLD                    KCOD
037100960606     C                   KFLD                    KKEY
037200960611     C     KTAB1         KLIST
037300960611     C                   KFLD                    KKUT
037400960611     C                   KFLD                    KCOD1
037500960606      * Definizione varibili
037600960612     C     *LIKE         DEFINE    TBLKUT        KKUT
037700960612     C     *LIKE         DEFINE    TABCOD        KCOD
037800960606     C     *LIKE         DEFINE    TABKEY        KKEY
037900960606     C     *LIKE         DEFINE    TABKEY        WINKEY
038000960611     C     *LIKE         DEFINE    TBLCOD        KCOD1
038100960611     C     *LIKE         DEFINE    TBLKUT        §KUT
038200960611     C     *LIKE         DEFINE    TBLCOD        §COD
038300960611     C     *LIKE         DEFINE    TBLKEY        §KEY
038400960606     C* Inizializzo variabili
038500960606     C                   MOVEL     'N'           WFINE             1
038600960612     C* Caricamento dati tabella 2A
038700960611     C                   Z-ADD     1             X                 3 0
038800960611     C                   Z-ADD     1             KKUT
038900960612     C                   MOVEL     '2A'          KCOD1
039000960611     C     KTAB1         CHAIN     TABEL00F                           30
039100960611     C     *IN30         DOWEQ     '0'
039200960611     C     TBLFLG        IFEQ      *BLANKS
039300960611     C                   ADD       1             X
039400960612     C                   MOVEL     TBLKEY        C2A(X)
039500960611     C                   END
039600960611     C     KTAB1         READE     TABEL00F                               30
039700960611     C                   END
039800960606     C*
039900960606     C                   ENDSR
040000960607     O*-----------------------------------------------------*
040100960607     OEDTAB000  E            TABANN
040200960607     O                       TABFLG
040300960607     O                       TABFLT
040400960607     O                       TABFTT
040500080709     O                       TABFTR
040600080709     O                       TABDTR
040700980203**         ERR
040800980203Codice Evento manuale obbligatorio                                     01
040900120305Codice Evento BRT inesistente                                          02
041000980203Immettere filiale a cui trasmettere                                    03
041100980203Se trasmissione a tutti o a nessuno impossibile immettere filiale      04
041200980203Filale a cui tramettere inesistente                                    05
041401150728Immettere "S": presente "Chiusura bolla per Merce mai affidata" ='S'   06
041500150728Opzione già presente per il Codice Evento                              07
