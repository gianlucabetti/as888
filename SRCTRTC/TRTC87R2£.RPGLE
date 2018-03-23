000100961003     H DECEDIT('0,') DATEDIT(*DMY.)
000200961003     H*---------------------------------------------------------------------*
000300961003     H*  Ritorno date arrivo ai partner  - E.D.I. - Estero -                *
000400961003     F*---------------------------------------------------------------------*
000500961003     F*  DATA BASE                                                          *
000600961003     F*---------------------------------------------------------------------*
000700961003     FEDBRV01L  IF   E           K DISK
000800961008     F*---------
000900961008     FEDSUM01L  IF   E           K DISK
001000980326     F*---------
001100980326     FEDBRV04L  IF   E           K DISK
001200980326     F                                     RENAME(EDBRV000:EDBRV004)
001300980326     F*---------
001400980326     FEDSUM04L  IF   E           K DISK
001500980326     F                                     RENAME(EDSUM000:EDSUM004)
001600961009     F*---------
001700961009     FEDTAB01L  IF   E           K DISK
001800980326     F*---------
001900980326     FTABEL00F  IF   E           K DISK
002000961003     F*---------
002100961003     FTRTC87D   CF   E             WORKSTN
002200961008     F                                     SFILE(TC87S03:NRR3)
002300961003     D*---------------------------------------------------------------------*
002400961003     D*  SCHIERE
002500961003     D*---------------------------------------------------------------------*
002600961009     D* Tabella errori date arrivo
002700961009     D CEA             S              3    DIM(100)                             codici errori
002800961009     D DEA             S             90    DIM(100)                             dati tab.EA
002900980326     D* Tabella descrizioni anomalie di spunta
003000980326     D C3E             S              1    DIM(100)                             codici anom.
003100980326     D D3E             S             89    DIM(100)                             decod. anom.
003200961003     D* Schiera errori
003300961008     D ERR             S             70    DIM(6) CTDATA PERRCD(1)
003400961003     D*---------------------------------------------------------------------*
003500961003     D* DS
003600961003     D*---------------------------------------------------------------------*
003700961003     D KPJBA         E DS
003800961003     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003900961003     D CNCR80        E DS
004000961003     D* Ds scomposzione tipo capoconti
004100961003     D TCUDS           DS
004200961003     D  F34                    3      4
004300961003     D  F56                    5      6
004400961009     D DSEA          E DS                  EXTNAME(EDIDSEA)
004500961009     D DSPT          E DS                  EXTNAME(EDIDSPT)
004600961007     D DSPU          E DS                  EXTNAME(EDIDSPU)
004700961007     D TRTC87        E DS                  EXTNAME(TRTC87DS)
004800961003     D*  Ds per inversione data
004900961003     D WLBDA8          DS
005000961003     D  G02DAT                 1      8  0
005100961003     D  G02INV                 9     16  0
005200961003     D  G02ERR                17     17
005300961003     D  G02TGI                18     22  0
005400961009     D*  Ds per definizione nr.spedizione - segnacollo da EDBRV
005500961009     D SPEBRV          DS
005600961029     D  BRVFLS                 1      3  0
005700961015     D  BRVNRS                 4      5  0
005800961015     D  BRVNSC                 6     12  0
005900961015     D  BRVAAS                13     16  0
006000961029     D  BRVLNP                17     19  0
006100961029     D  BRVNSP                20     26  0
006200961009     D*  Ds per definizione nr.spedizione - segnacollo da EDSUM
006300961009     D SPESUM          DS
006400961029     D  SUMFLS                 1      3  0
006500961015     D  SUMNRS                 4      5  0
006600961015     D  SUMNSC                 6     12  0
006700961015     D  SUMAAS                13     16  0
006800961029     D  SUMLNP                17     19  0
006900961029     D  SUMNSP                20     26  0
007000961003     D                SDS
007100961003     D  NOMPGM                 1     10
007200961003     C*---------------------------------------------------------------------*
007300961003     C* Ciclo principale
007400961003     C*---------------------------------------------------------------------*
007500961008     C* Inizializzo terza videata
007600961008     C                   EXSR      INZ03
007700961003      * Loop gestione videate
007800961003     C     WFINE         DOWEQ     'N'
007900961008     C     WTPVID        CASEQ     '3'           GESS03                         dati spunte
008000961003     C                   END
008100961003     C                   END
008200961003      * Fine Lavoro
008300961003     C     FINE          TAG
008400961009     C                   MOVEL     TRTC87        KPJBU
008500961003     C                   SETON                                        LR
008600961003     C*---------------------------------------------------------------*
008700961008     C*  INZ03: Inizializzo Terza videata
008800961003     C*---------------------------------------------------------------*
008900961008     C     INZ03         BEGSR
009000961003     C*
009100961003     C                   SETOFF                                       28
009200961009     C                   MOVEL     *BLANKS       $MSG
009300980330     C                   MOVEL     *BLANKS       WAGGEC            1
009400961009     C                   Z-ADD     1             NR3KEY
009500961008     C                   MOVEL     NOMPGM        V3CPGM
009600961008     C                   Z-ADD     D87CCM        V3CCCM
009700961008     C                   MOVEL     D87DEC        V3DCCM
009800980325     C                   Z-ADD     D87LNP        V3CLNP
009900980326     C                   MOVEL     D87DLP        V3DLNP
010000961011     C                   MOVE      D87NFE        V3CNFE
010100961009     C                   MOVEL     '3'           WTPVID            1
010200010405      *
010300980326     C*  Controllo se lavoro x lnp o x cliente
010400980326     C     D87LNP        IFEQ      0
010500980326     C                   SETON                                        05
010600980326     C                   SETOFF                                       06
010700980326     C                   ELSE
010800980326     C                   SETOFF                                       05
010900980326     C                   SETON                                        06
011000980326     C                   END
011100961003     C*  Pulisco subfile
011200961009     C                   Z-ADD     0             NRR3              4 0
011300961003     C                   SETOFF                                       2021
011400961008     C                   WRITE     TC87C03
011500961003     C                   SETON                                        2021
011600961008      *  Eseguo caricamento
011700961008     C                   EXSR      ROLLUP
011800970626     C                   MOVEL     'N'           WVUOTO            1
011900970626     C     NRR3          IFEQ      0
012000970626     C                   MOVEL     'S'           WVUOTO
012100970626     C                   SETOFF                                       20
012200970626     C                   END
012300961003     C*
012400961003     C                   ENDSR
012500961003     C*---------------------------------------------------------------*
012600961008     C*  GESS03: Gestione subfile dati BRV
012700961003     C*---------------------------------------------------------------*
012800961008     C     GESS03        BEGSR
012900961003     C*
013000961009     C  N28              Z-ADD     1             NR3KEY
013100970218     C  N28              Z-ADD     1             NRR3
013200970626     C     WVUOTO        COMP      'S'                                2020
013300980401     C     D87BS1        COMP      'E'                                    10
013400980409     C   10D87SEC        COMP      'S'                                    10
013500961009     C                   WRITE     TC87T03
013600961009     C                   EXFMT     TC87C03
013700961003     C*  Fine Lavoro
013800961003     C     *INKC         IFEQ      '1'
013900961003     C                   MOVEL     'S'           WFINE
014000961009     C                   MOVEL     '03'          D87CMD
014100961008     C                   GOTO      FINVS3
014200961003     C                   END
014300961003     C*  Ritorno
014400961003     C     *INKL         IFEQ      '1'
014500961009     C                   MOVEL     'S'           WFINE
014600961009     C                   MOVEL     '12'          D87CMD
014700961009     C                   GOTO      FINVS3
014800961003     C                   END
014900980330     C*  Aggiunta Eccedenze
015000980330     C     *INKJ         IFEQ      '1'
015100980330     C                   MOVEL     TRTC87        KPJBU
015200980330     C                   CALL      'TRTC87R6'
015300980330     C                   PARM                    KPJBA
015400980331     C                   MOVEL     KPJBU         TRTC87
015500980330     C                   GOTO      FINVS3
015600980330     C                   END
015700961003     C*  Controlli
015800970626     C     WVUOTO        IFNE      'S'
015900961009     C                   EXSR      CTR03
016000970626     C                   END
016100961009     C   28              GOTO      FINVS3
016200961009     C*  Conferma
016300961009     C     *INKF         IFEQ      '1'
016400961009     C                   MOVEL     'S'           WFINE
016500961009     C                   MOVEL     '06'          D87CMD
016600961010     C*  Richiamo pgm x aggiornamento BRV/SUM
016700970626     C     WVUOTO        IFNE      'S'
016800961009     C                   MOVEL     TRTC87        KPJBU
016900961009     C                   CALL      'TRTC87R3'
017000961010     C                   PARM                    KPJBA
017100970626     C                   END
017200961010     C*  Richiamo pgm x scrittura errori WF.
017300961010     C                   CALL      'TRTC87R4'
017400961010     C                   PARM                    KPJBA
017500010405      *
017600961010     C*  Sottometto scrittura IFTSTA
017700961010     C                   MOVEL     'TC88'        KCOAZ
017800961010     C                   CALL      'BCH10'
017900961010     C                   PARM                    KPJBA
018000010405      *
018100961009     C                   END
018200961003     C*
018300961009     C     FINVS3        ENDSR
018400961003     C*---------------------------------------------------------------*
018500961009     C*  CTR03: Controlli Terza videata
018600961003     C*---------------------------------------------------------------*
018700961009     C     CTR03         BEGSR
018800961003     C*
018900961003     C*  Controllo selezioni effettuate
019000961009     C                   READC     TC87S03                                29
019100961003     C     *IN29         DOWEQ     '0'
019200961011     C                   SETOFF                                       40
019300961025     C     V3CFGS        IFEQ      'A'
019400961009     C     V3CMER        ANDEQ     'N'
019500961009     C  N28              Z-ADD     NRR3          NR3KEY
019600961009     C*  Se ho chiesto annullamento di edsum e non è prevista la
019700961009     C*  manutenzione dell'errore do segnalazione
019800961009     C                   SETON                                        2840
019900961009     C                   MOVEL     ERR(1)        $MSG
020000961008     C                   END
020100961009     C*  Aggiorno subfile
020200961009     C                   SETON                                        22
020300961009     C                   MOVEL     V3CIN3        *IN03
020400961029     C     V3DERS        COMP      *BLANKS                                04
020500961009     C                   UPDATE    TC87S03
020600961009     C                   SETOFF                                       22
020700961008     C*  Lettura successiva subfile
020800961009     C                   READC     TC87S03                                29
020900961003     C                   END
021000961003     C*
021100961003     C                   ENDSR
021200961003     C*----------------------------------------------------*
021300961003     C*   ROLLUP: Caricamento dati da EDVAB                *
021400961003     C*----------------------------------------------------*
021500961003     C     ROLLUP        BEGSR
021600961008      *----------------------------------------------------*
021700961008      *  Leggo contemporaneamente EDBRV e EDSUM.
021800961008      *  EDBRV x codice cliente nr.foglio entrata
021900961008      *  EDSUM x codice cliente
022000961008      *----------------------------------------------------*
022100961008     C                   Z-ADD     0             NRR3
022200961008     C                   Z-ADD     D87CCM        KCCM
022300980401     C                   Z-ADD     D87LNP        KLNP
022400961009     C                   Z-ADD     0             KNFE1
022500961011     C                   MOVEL     *BLANKS       KFLG
022600961008     C                   Z-ADD     D87NFE        KNFE
022700961008     C                   MOVE      *HIVAL        SPESUM
022800961008     C                   MOVE      *HIVAL        SPEBRV
022900961008      *  Prima lettura:
023000980326     C     D87LNP        IFEQ      0
023100961009     C     KBRV          CHAIN     EDBRV01L                           31
023200961009     C     KSUM          CHAIN     EDSUM01L                           32
023300980326     C                   ELSE
023400980326     C     KBRV1         CHAIN     EDBRV04L                           31
023500980326     C     KSUM1         CHAIN     EDSUM04L                           32
023600980326     C                   END
023700961008      *----------------------------------------------------*
023800961008      *  L'elaborazione continua fino a che non ho più
023900961008      *  nessun dato da caricare ne da BRV ne da EDSUM
024000961008      *----------------------------------------------------*
024100961008     C     *IN31         DOWEQ     '0'                                          Eof EDBRV or
024200961008     C     *IN32         OREQ      '0'                                          Eof EDSUM or
024300961008      *----------------------------------------------------*
024400961008      *  ... fino a che il numero spedizione - segnacollo
024500961008      *      che leggo su EDBRV è inferiore a quello letto
024600961008      *      continuo a leggere solo quest'ultimo e verifico
024700961008      *      se devo caricare i dati a subfile.
024800961008      *----------------------------------------------------*
024900961008     C     SPEBRV        DOWLT     SPESUM
025000961008     C     *IN31         ANDEQ     '0'
025100961008     C                   EXSR      CARBRV
025200961008     C                   END
025300961008      *----------------------------------------------------*
025400961008      *  ... fino a che il numero spedizione - segnacollo
025500961008      *      che leggo su EDSUM è inferiore a quello letto
025600961008      *      carico i dati del subfile e continuo a leggere
025700961008      *      solo quest'ultimo
025800961008      *----------------------------------------------------*
025900961008     C     SPESUM        DOWLT     SPEBRV
026000961008     C     *IN32         ANDEQ     '0'
026100961008     C                   EXSR      CARSUM
026200961008     C                   END
026300961008      *----------------------------------------------------*
026400961008      *  ... fino a che il numero spedizione - segnacollo
026500961008      *      che leggo su EDSUM è ugulale a quello letto
026600961008      *      carico i dati del subfile e continuo a leggere
026700961008      *      entrambi
026800961008      *----------------------------------------------------*
026900961008     C     SPESUM        DOWEQ     SPEBRV
027000961008     C     *IN31         ANDEQ     '0'
027100961008     C     *IN32         ANDEQ     '0'
027200961008     C                   EXSR      CARTUT
027300961008     C                   END
027400961003     C*
027500961009     C                   END
027600961009     C*
027700961003     C                   ENDSR
027800961008     C*----------------------------------------------------*
027900961008     C*   CARBRV: Caricamento dati da EDBRV                *
028000961008     C*----------------------------------------------------*
028100961008     C     CARBRV        BEGSR
028200980326     C*
028300980326     C                   CLEAR                   DSEA
028400980326     C* Se ho degli errori aggancio tabella EA
028500980326     C     BRVSTS        IFNE      *BLANKS                                      letto errore
028600980326     C     BRVFLG        ANDEQ     *BLANKS                                      errore
028700980326     C                   Z-ADD     1             X
028800980326     C     BRVSTS        LOOKUP    CEA(X)                                 33
028900980326     C     *IN33         IFEQ      '1'                                          trovato err.
029000980326     C                   MOVEL     DEA(X)        DSEA
029100980326     C                   END
029200980326     C* Se c'è anomalia e il tipo errore non prevale su di
029300980326     C* essa imposto flag stato evento da ignorare a 'S'
029400980326     C     BRVCAN        IFNE      *BLANKS
029500980326     C     §EACAN        ANDNE     'S'
029600980326     C                   MOVEL     'S'           §EASTS
029700980326     C                   END
029800980326     C                   END
029900980326      *----------------------------------------------------*
030000980326      *  ... carico i dati solo se ho richiesto di vedere
030100980326      *      sia gli errori che le bolle positive o se ho
030200980326      *      richiesto di vedere gli errori e ho letto una
030300980326      *      bolla con un codice di errore non da ignorare
030400980326      *      oppure o letto una bolla con codice anomalia
030500980326      *      impostato
030600980326      *----------------------------------------------------*
030700980331     C     D87ERR        IFEQ      'N'                                          carico tutto
030800980326     C     BRVSTS        ORNE      *BLANKS                                      o errore
030900980326     C     BRVFLG        ANDEQ     *BLANKS                                      valido
031000980326     C     §EASTS        ANDNE     'S'                                          no ignorare
031100980326     C     BRVCAN        ORNE      *BLANKS                                      o anomalia
031200961009     C                   CLEAR                   TC87S03
031300961030     C                   SETON                                        03
031400961030     C                   SETON                                        04
031500961011     C                   Z-ADD     BRVAAS        V3CAAS
031600961011     C                   Z-ADD     BRVLNP        V3CLNP
031700961008     C                   Z-ADD     BRVNRS        V3CNRS
031800961008     C                   Z-ADD     BRVNSP        V3CNSP
031900961025     C                   Z-ADD     BRVNSC        V3CNSC
032000961025     C                   Z-ADD     BRVFLS        V3CFLS
032100961008     C                   Z-ADD     BRVNPS        V3CNPS
032200961008     C                   Z-ADD     BRVLNA        V3CLAB
032300961025     C                   MOVEL     *BLANKS       V3CFGB
032400961008      *  Decodifico errore se c'è
032500961008     C     BRVSTS        IFNE      *BLANKS                                      letto errore
032600961030     C     BRVFLG        ANDEQ     *BLANKS                                      errore
032700980326     C     *IN33         ANDEQ     '1'                                          trovato err.
032800980326     C     §EASTS        ANDNE     'S'                                          trovato err.
032900961008     C                   MOVEL     DEA(X)        DSEA
033000961009     C                   MOVEL     BRVSTS        V3CERB
033100961009     C                   MOVEL     §EADES        V3DERB
033200961008     C     §EAMOD        COMP      'S'                                0303
033300980326     C* Se non c'è errore controllo se c'è anomalia
033400980326     C                   ELSE
033500980326     C     BRVCAN        IFNE      *BLANKS                                      anomalia
033600980402     C     BRVSTS        IFNE      *BLANKS
033700980402     C     §EAMOD        COMP      'S'                                0303
033800980402     C                   ELSE
033900980402     C                   SETOFF                                       03
034000980402     C                   END
034100980326     C* .... reperisco eventuale decodifica
034200980326     C                   Z-ADD     1             X
034300980326     C     BRVCAN        LOOKUP    C3E(X)                                 33
034400980326     C     *IN33         IFEQ      '1'                                          anomalia trv
034500980326     C                   MOVEL     BRVCAN        V3CERB
034600980326     C                   MOVEL     D3E(X)        V3DERB
034700980326     C                   END                                                    anom OK
034800980326     C                   END                                                    trovato anom
034900961008     C                   END                                                    letto errore
035000961008     C                   MOVEL     *IN03         V3CIN3
035100961008     C                   ADD       1             NRR3
035200961031     C                   SETON                                        22
035300961008     C                   WRITE     TC87S03
035400961031     C                   SETOFF                                       22
035500961008     C                   END                                                    carico sfl
035600961008      *  lettura successiva
035700980326     C     D87LNP        IFEQ      0
035800980326     C     KBRV          READE     EDBRV01L                               31
035900980326     C                   ELSE
036000980326     C     KBRV1         READE     EDBRV04L                               31
036100980326     C                   END
036200961008      *----------------------------------------------------*
036300961008      *  ... se sono a fine file EDBRV imposto a *HIVAL
036400961008      *      numero spedizione segnacollo in modo che risulti
036500961008      *      sempre superiore a quello di EDSUM
036600961008      *----------------------------------------------------*
036700961008     C   31              MOVE      *HIVAL        SPEBRV
036800961008     C*
036900961008     C                   ENDSR
037000961008     C*----------------------------------------------------*
037100961008     C*   CARSUM: Caricamento dati da EDSUM                *
037200961008     C*----------------------------------------------------*
037300961008     C     CARSUM        BEGSR
037400961008     C*
037500961009     C                   CLEAR                   TC87S03
037600961030     C                   SETON                                        03
037700961011     C                   Z-ADD     SUMAAS        V3CAAS
037800961008     C                   Z-ADD     SUMLNP        V3CLNP
037900961008     C                   Z-ADD     SUMNRS        V3CNRS
038000961008     C                   Z-ADD     SUMNSP        V3CNSP
038100961008     C                   Z-ADD     SUMNSC        V3CNSC
038200961025     C                   Z-ADD     SUMFLS        V3CFLS
038300961008     C                   Z-ADD     SUMLNA        V3CLAS
038400970620     C                   MOVEL     SUMCMR        V3CCMR
038500961025     C                   MOVEL     *BLANKS       V3CFGS
038600961008      *  Decodifico errore
038700961008     C                   Z-ADD     1             X
038800961008     C     SUMSTS        LOOKUP    CEA(X)                                 33
038900961008     C     *IN33         IFEQ      '1'                                          trovato err.
039000961008     C                   MOVEL     DEA(X)        DSEA
039100961009     C                   MOVEL     SUMSTS        V3CERS
039200961009     C                   MOVEL     §EADES        V3DERS
039300961008     C                   MOVEL     §EAMOD        V3CMER
039400961008     C                   END                                                    trovato err.
039500961008     C                   ADD       1             NRR3
039600961031     C                   SETON                                        22
039700961008     C                   WRITE     TC87S03
039800961031     C                   SETOFF                                       22
039900961008      *  lettura successiva
040000980326     C     D87LNP        IFEQ      0
040100961009     C     KSUM          READE     EDSUM01L                               32
040200980326     C                   ELSE
040300980326     C     KSUM1         READE     EDSUM04L                               32
040400980326     C                   END
040500961008      *----------------------------------------------------*
040600961008      *  ... se sono a fine file EDSUM imposto a *HIVAL
040700961008      *      numero spedizione segnacollo in modo che risulti
040800961008      *      sempre superiore a quello di EDBRV
040900961008      *----------------------------------------------------*
041000961008     C   32              MOVE      *HIVAL        SPESUM
041100961008     C*
041200961008     C                   ENDSR
041300961008     C*----------------------------------------------------*
041400961008     C*   CARTUT: Caricamento sia EDBRV che EDSUM          *
041500961008     C*----------------------------------------------------*
041600961008     C     CARTUT        BEGSR
041700961008     C*
041800961008     C                   SETON                                        03
041900961009     C                   CLEAR                   TC87S03
042000961011     C                   Z-ADD     SUMAAS        V3CAAS
042100961008     C                   Z-ADD     SUMLNP        V3CLNP
042200961008     C                   Z-ADD     SUMNRS        V3CNRS
042300961008     C                   Z-ADD     SUMNSP        V3CNSP
042400961008     C                   Z-ADD     SUMNSC        V3CNSC
042500961025     C                   Z-ADD     SUMFLS        V3CFLS
042600961008     C                   Z-ADD     SUMLNA        V3CLAS
042700970620     C                   MOVEL     SUMCMR        V3CCMR
042800961025     C                   MOVEL     *BLANKS       V3CFGS
042900980326      *  Reperisco dati errore
043000961008     C                   Z-ADD     1             X
043100961008     C     SUMSTS        LOOKUP    CEA(X)                                 33
043200961008     C     *IN33         IFEQ      '1'                                          trovato err.
043300961008     C                   MOVEL     DEA(X)        DSEA
043400961009     C                   MOVEL     SUMSTS        V3CERS
043500961009     C                   MOVEL     §EADES        V3DERS
043600961008     C                   MOVEL     §EAMOD        V3CMER
043700961008     C                   END                                                    trovato err.
043800980326      *----------------------------------------------------*
043900980326      * DATI DA EDBRV
044000980326     C                   CLEAR                   DSEA
044100980326     C* Se ho degli errori aggancio tabella EA
044200980326     C     BRVSTS        IFNE      *BLANKS                                      letto errore
044300980326     C     BRVFLG        ANDEQ     *BLANKS                                      errore
044400980326     C                   Z-ADD     1             X
044500980326     C     BRVSTS        LOOKUP    CEA(X)                                 33
044600980326     C     *IN33         IFEQ      '1'                                          trovato err.
044700980326     C                   MOVEL     DEA(X)        DSEA
044800980326     C                   END
044900980326     C* Se c'è anomalia e il tipo errore non prevale su di
045000980326     C* essa imposto flag stato evento da ignorare a 'S'
045100980326     C     BRVCAN        IFNE      *BLANKS
045200980326     C     §EACAN        ANDNE     'S'
045300980326     C                   MOVEL     'S'           §EASTS
045400980326     C                   END
045500980326     C                   END
045600961008      *----------------------------------------------------*
045700961008      *  ... carico i dati da BRV solo se ho richiesto di
045800961008      *      vedere sia gli errori che le bolle positive o
045900961008      *      se ho richiesto di vedere gli errori e ho letto
046000961008      *      una bolla con il codice di errore impostato
046100961008      *----------------------------------------------------*
046200961008     C     D87ERR        IFEQ      'N'                                          carico tutto
046300961011     C     BRVSTS        ORNE      *BLANKS                                      errore
046400961030     C     BRVFLG        ANDEQ     *BLANKS                                      errore
046500980326     C     §EASTS        ANDNE     'S'                                          no ignorare
046600980326     C     BRVCAN        ORNE      *BLANKS                                      o anomalia
046700961008     C                   Z-ADD     BRVNPS        V3CNPS
046800961008     C                   Z-ADD     BRVLNA        V3CLAB
046900961025     C                   MOVEL     *BLANKS       V3CFGB
047000961008      *  Decodifico errore se c'è
047100961008     C     BRVSTS        IFNE      *BLANKS                                      letto errore
047200961030     C     BRVFLG        ANDEQ     *BLANKS                                      errore
047300980326     C     *IN33         ANDEQ     '1'                                          trovato err.
047400980326     C     §EASTS        ANDNE     'S'                                          trovato err.
047500961008     C                   MOVEL     DEA(X)        DSEA
047600961009     C                   MOVEL     BRVSTS        V3CERB
047700961009     C                   MOVEL     §EADES        V3DERB
047800961008     C     §EAMOD        COMP      'S'                                0303
047900961008     C                   END                                                    trovato err.
048000980326     C* Se non c'è errore controllo se c'è anomalia
048100980326     C                   ELSE
048200980326     C     BRVCAN        IFNE      *BLANKS                                      anomalia
048300980326     C* .... reperisco eventuale decodifica
048400980326     C                   Z-ADD     1             X
048500980326     C     BRVCAN        LOOKUP    C3E(X)                                 33
048600980326     C     *IN33         IFEQ      '1'                                          anomalia trv
048700980326     C                   MOVEL     BRVCAN        V3CERB
048800980326     C                   MOVEL     D3E(X)        V3DERB
048900980326     C                   END                                                    trovato anom
049000980326     C                   END                                                    trovato anom
049100961008     C                   END                                                    carico BRV
049200980326     C*
049300961008     C                   MOVEL     *IN03         V3CIN3
049400961008     C                   ADD       1             NRR3
049500961031     C                   SETON                                        22
049600961008     C                   WRITE     TC87S03
049700961031     C                   SETOFF                                       22
049800961008      *  leggo entrambi
049900980326     C     D87LNP        IFEQ      0
050000961008     C     KBRV          READE     EDBRV01L                               31
050100961009     C     KSUM          READE     EDSUM01L                               32
050200980326     C                   ELSE
050300980326     C     KBRV1         READE     EDBRV04L                               31
050400980326     C     KSUM1         READE     EDSUM04L                               32
050500980326     C                   END
050600961008      *----------------------------------------------------*
050700961008      *  ... se sono a fine file EDBRV imposto a *HIVAL
050800961008      *      numero spedizione segnacollo in modo che risulti
050900961008      *      sempre superiore a quello di EDSUM
051000961008      *----------------------------------------------------*
051100961008     C   31              MOVE      *HIVAL        SPEBRV
051200961008      *----------------------------------------------------*
051300961008      *  ... se sono a fine file EDSUM imposto a *HIVAL
051400961008      *      numero spedizione segnacollo in modo che risulti
051500961008      *      sempre superiore a quello di EDBRV
051600961008      *----------------------------------------------------*
051700961008     C   32              MOVE      *HIVAL        SPESUM
051800961008      *
051900961008     C                   ENDSR
052000961003     C*----------------------------------------------------*
052100961003     C*   *INZSR: Operazioni iniziali                      *
052200961003     C*----------------------------------------------------*
052300961003     C     *INZSR        BEGSR
052400961003     C*
052500961003     C     *ENTRY        PLIST
052600961003     C                   PARM                    KPJBA
052700961011     C                   MOVEL     KPJBU         TRTC87
052800961003     C*  Richiamo XPARUT
052900961003     C                   Z-ADD     1             CODUT
053000961003     C                   CALL      'X§PARUT'
053100961003     C                   PARM                    UT§DSE
053200961003     C                   MOVEL     REC80         CNCR80
053300961003     C                   MOVEL     RAGUT         RSUT
053400961003     C*  Chiavi di accesso
053500961008     C     KBRV          KLIST
053600961008     C                   KFLD                    KCCM
053700961009     C                   KFLD                    KNFE
053800961009     C     KSUM          KLIST
053900961009     C                   KFLD                    KCCM
054000961009     C                   KFLD                    KNFE1
054100961011     C                   KFLD                    KFLG
054200980326     C     KBRV1         KLIST
054300980326     C                   KFLD                    KLNP
054400980326     C                   KFLD                    KNFE
054500980326     C     KSUM1         KLIST
054600980326     C                   KFLD                    KLNP
054700980326     C                   KFLD                    KNFE1
054800980326     C                   KFLD                    KFLG
054900980326     C     KTAB          KLIST
055000980326     C                   KFLD                    KKUT
055100980326     C                   KFLD                    KCOD1
055200961003     C*  Definizione variabili
055300961003     C     *LIKE         DEFINE    TABCOD        KCOD
055400980326     C     *LIKE         DEFINE    BRVLNP        KLNP
055500980326     C     *LIKE         DEFINE    BRVCCM        KCCM
055600961008     C     *LIKE         DEFINE    BRVNFE        KNFE
055700961009     C     *LIKE         DEFINE    SUMNFE        KNFE1
055800961011     C     *LIKE         DEFINE    SUMFLG        KFLG
055900980326     C     *LIKE         DEFINE    TBLKUT        KKUT
056000980326     C     *LIKE         DEFINE    TBLCOD        KCOD1
056100961009     C*  Carico in schiera codici tabella - EA -
056200961009     C                   Z-ADD     0             X                 4 0
056300010405     C                   CLEAR                   DEA
056400010405     C                   CLEAR                   CEA
056500961009     C                   MOVEL     'EA'          KCOD
056600961009     C     KCOD          CHAIN     EDTAB01L                           31
056700961009     C     *IN31         DOWEQ     '0'
056800961009     C     TABFLG        IFEQ      ' '
056900961009     C                   ADD       1             X
057000961009     C                   MOVEL     TABUNI        DEA(X)
057100961009     C                   MOVEL     TABKEY        CEA(X)
057200961009     C                   END
057300961009     C     KCOD          READE     EDTAB01L                               31
057400961009     C                   END
057500980326     C*  Carico in schiera codici tabella - 3E -
057600980326     C                   Z-ADD     0             X                 4 0
057700010405     C                   CLEAR                   D3E
057800010405     C                   CLEAR                   C3E
057900980326     C                   Z-ADD     1             KKUT
058000980326     C                   MOVEL     '3E'          KCOD1
058100980326     C     KTAB          CHAIN     TABEL00F                           31
058200980326     C     *IN31         DOWEQ     '0'
058300980326     C     TBLFLG        IFEQ      ' '
058400980326     C                   ADD       1             X
058500980326     C                   MOVEL     TBLUNI        D3E(X)
058600980326     C                   MOVEL     TBLKEY        C3E(X)
058700980326     C                   END
058800980326     C     KTAB          READE     TABEL00F                               31
058900980326     C                   END
059000961003     C*  Inizializzo variabili
059100961003     C                   MOVEL     'N'           WFINE             1
059200961003     C*
059300961003     C                   ENDSR
059400961008**
059500961008Punto operativo inesistente o annullato                                01
059600961008Punto operativo non gestito su questo AS                               02
059700961008Il punto operativo non si gestisce in modo autonomo                    03
059800961008Codice cliente inesistente o annullato                                 04
059900961008Codice cliente non relativo ad un partner                              05
060000961008Per il cliente selezionato non ci sono date di arrivo da trasmettere   06
