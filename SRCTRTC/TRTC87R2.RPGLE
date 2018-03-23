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
017700040518      *---
017800040518      * se non aveva il codice partner lo imposta per gestire un
017900040518      * membro ben preciso
018000040518      *---
018100040524     c                   if        %subst(Kpjbu: 1: 7) = '0000000'
018200040524     c                   eval      PTNCOD = 'PT'
018300040524     c                   eval      PTNKEY = %subst(Kpjbu: 74: 35)
018400040518     C     KPTN          chain     EDTAB01L
018500040524     c                   if        %found(EDTAB01L)
018600040524     c                   movel     TABUNI        DSPT
018700040524     c                   move      §PTKSC        KPJBU
018800040524     c                   else
018900040524     c                   move      '0000000'     KPJBU
019000040524     c                   end
019100040518     c                   else
019200040524     c                   clear                   cod7              7
019300040524     c                   eval      cod7 = %subst(Kpjbu: 1: 7)
019400040524     c                   move      cod7          KPJBU
019500040518     c                   end
019600040518      *---
019700961010     C                   MOVEL     'TC88'        KCOAZ
019800961010     C                   CALL      'BCH10'
019900961010     C                   PARM                    KPJBA
020000010405      *
020100961009     C                   END
020200961003     C*
020300961009     C     FINVS3        ENDSR
020400961003     C*---------------------------------------------------------------*
020500961009     C*  CTR03: Controlli Terza videata
020600961003     C*---------------------------------------------------------------*
020700961009     C     CTR03         BEGSR
020800961003     C*
020900961003     C*  Controllo selezioni effettuate
021000961009     C                   READC     TC87S03                                29
021100961003     C     *IN29         DOWEQ     '0'
021200961011     C                   SETOFF                                       40
021300961025     C     V3CFGS        IFEQ      'A'
021400961009     C     V3CMER        ANDEQ     'N'
021500961009     C  N28              Z-ADD     NRR3          NR3KEY
021600961009     C*  Se ho chiesto annullamento di edsum e non è prevista la
021700961009     C*  manutenzione dell'errore do segnalazione
021800961009     C                   SETON                                        2840
021900961009     C                   MOVEL     ERR(1)        $MSG
022000961008     C                   END
022100961009     C*  Aggiorno subfile
022200961009     C                   SETON                                        22
022300961009     C                   MOVEL     V3CIN3        *IN03
022400961029     C     V3DERS        COMP      *BLANKS                                04
022500961009     C                   UPDATE    TC87S03
022600961009     C                   SETOFF                                       22
022700961008     C*  Lettura successiva subfile
022800961009     C                   READC     TC87S03                                29
022900961003     C                   END
023000961003     C*
023100961003     C                   ENDSR
023200961003     C*----------------------------------------------------*
023300961003     C*   ROLLUP: Caricamento dati da EDVAB                *
023400961003     C*----------------------------------------------------*
023500961003     C     ROLLUP        BEGSR
023600961008      *----------------------------------------------------*
023700961008      *  Leggo contemporaneamente EDBRV e EDSUM.
023800961008      *  EDBRV x codice cliente nr.foglio entrata
023900961008      *  EDSUM x codice cliente
024000961008      *----------------------------------------------------*
024100961008     C                   Z-ADD     0             NRR3
024200961008     C                   Z-ADD     D87CCM        KCCM
024300980401     C                   Z-ADD     D87LNP        KLNP
024400961009     C                   Z-ADD     0             KNFE1
024500961011     C                   MOVEL     *BLANKS       KFLG
024600961008     C                   Z-ADD     D87NFE        KNFE
024700961008     C                   MOVE      *HIVAL        SPESUM
024800961008     C                   MOVE      *HIVAL        SPEBRV
024900961008      *  Prima lettura:
025000980326     C     D87LNP        IFEQ      0
025100961009     C     KBRV          CHAIN     EDBRV01L                           31
025200961009     C     KSUM          CHAIN     EDSUM01L                           32
025300980326     C                   ELSE
025400980326     C     KBRV1         CHAIN     EDBRV04L                           31
025500980326     C     KSUM1         CHAIN     EDSUM04L                           32
025600980326     C                   END
025700961008      *----------------------------------------------------*
025800961008      *  L'elaborazione continua fino a che non ho più
025900961008      *  nessun dato da caricare ne da BRV ne da EDSUM
026000961008      *----------------------------------------------------*
026100961008     C     *IN31         DOWEQ     '0'                                          Eof EDBRV or
026200961008     C     *IN32         OREQ      '0'                                          Eof EDSUM or
026300961008      *----------------------------------------------------*
026400961008      *  ... fino a che il numero spedizione - segnacollo
026500961008      *      che leggo su EDBRV è inferiore a quello letto
026600961008      *      continuo a leggere solo quest'ultimo e verifico
026700961008      *      se devo caricare i dati a subfile.
026800961008      *----------------------------------------------------*
026900961008     C     SPEBRV        DOWLT     SPESUM
027000961008     C     *IN31         ANDEQ     '0'
027100961008     C                   EXSR      CARBRV
027200961008     C                   END
027300961008      *----------------------------------------------------*
027400961008      *  ... fino a che il numero spedizione - segnacollo
027500961008      *      che leggo su EDSUM è inferiore a quello letto
027600961008      *      carico i dati del subfile e continuo a leggere
027700961008      *      solo quest'ultimo
027800961008      *----------------------------------------------------*
027900961008     C     SPESUM        DOWLT     SPEBRV
028000961008     C     *IN32         ANDEQ     '0'
028100961008     C                   EXSR      CARSUM
028200961008     C                   END
028300961008      *----------------------------------------------------*
028400961008      *  ... fino a che il numero spedizione - segnacollo
028500961008      *      che leggo su EDSUM è ugulale a quello letto
028600961008      *      carico i dati del subfile e continuo a leggere
028700961008      *      entrambi
028800961008      *----------------------------------------------------*
028900961008     C     SPESUM        DOWEQ     SPEBRV
029000961008     C     *IN31         ANDEQ     '0'
029100961008     C     *IN32         ANDEQ     '0'
029200961008     C                   EXSR      CARTUT
029300961008     C                   END
029400961003     C*
029500961009     C                   END
029600961009     C*
029700961003     C                   ENDSR
029800961008     C*----------------------------------------------------*
029900961008     C*   CARBRV: Caricamento dati da EDBRV                *
030000961008     C*----------------------------------------------------*
030100961008     C     CARBRV        BEGSR
030200980326     C*
030300980326     C                   CLEAR                   DSEA
030400980326     C* Se ho degli errori aggancio tabella EA
030500980326     C     BRVSTS        IFNE      *BLANKS                                      letto errore
030600980326     C     BRVFLG        ANDEQ     *BLANKS                                      errore
030700980326     C                   Z-ADD     1             X
030800980326     C     BRVSTS        LOOKUP    CEA(X)                                 33
030900980326     C     *IN33         IFEQ      '1'                                          trovato err.
031000980326     C                   MOVEL     DEA(X)        DSEA
031100980326     C                   END
031200980326     C* Se c'è anomalia e il tipo errore non prevale su di
031300980326     C* essa imposto flag stato evento da ignorare a 'S'
031400980326     C     BRVCAN        IFNE      *BLANKS
031500980326     C     §EACAN        ANDNE     'S'
031600980326     C                   MOVEL     'S'           §EASTS
031700980326     C                   END
031800980326     C                   END
031900980326      *----------------------------------------------------*
032000980326      *  ... carico i dati solo se ho richiesto di vedere
032100980326      *      sia gli errori che le bolle positive o se ho
032200980326      *      richiesto di vedere gli errori e ho letto una
032300980326      *      bolla con un codice di errore non da ignorare
032400980326      *      oppure o letto una bolla con codice anomalia
032500980326      *      impostato
032600980326      *----------------------------------------------------*
032700980331     C     D87ERR        IFEQ      'N'                                          carico tutto
032800980326     C     BRVSTS        ORNE      *BLANKS                                      o errore
032900980326     C     BRVFLG        ANDEQ     *BLANKS                                      valido
033000980326     C     §EASTS        ANDNE     'S'                                          no ignorare
033100980326     C     BRVCAN        ORNE      *BLANKS                                      o anomalia
033200961009     C                   CLEAR                   TC87S03
033300961030     C                   SETON                                        03
033400961030     C                   SETON                                        04
033500961011     C                   Z-ADD     BRVAAS        V3CAAS
033600961011     C                   Z-ADD     BRVLNP        V3CLNP
033700961008     C                   Z-ADD     BRVNRS        V3CNRS
033800961008     C                   Z-ADD     BRVNSP        V3CNSP
033900961025     C                   Z-ADD     BRVNSC        V3CNSC
034000961025     C                   Z-ADD     BRVFLS        V3CFLS
034100961008     C                   Z-ADD     BRVNPS        V3CNPS
034200961008     C                   Z-ADD     BRVLNA        V3CLAB
034300961025     C                   MOVEL     *BLANKS       V3CFGB
034400961008      *  Decodifico errore se c'è
034500961008     C     BRVSTS        IFNE      *BLANKS                                      letto errore
034600961030     C     BRVFLG        ANDEQ     *BLANKS                                      errore
034700980326     C     *IN33         ANDEQ     '1'                                          trovato err.
034800980326     C     §EASTS        ANDNE     'S'                                          trovato err.
034900961008     C                   MOVEL     DEA(X)        DSEA
035000961009     C                   MOVEL     BRVSTS        V3CERB
035100961009     C                   MOVEL     §EADES        V3DERB
035200961008     C     §EAMOD        COMP      'S'                                0303
035300980326     C* Se non c'è errore controllo se c'è anomalia
035400980326     C                   ELSE
035500980326     C     BRVCAN        IFNE      *BLANKS                                      anomalia
035600980402     C     BRVSTS        IFNE      *BLANKS
035700980402     C     §EAMOD        COMP      'S'                                0303
035800980402     C                   ELSE
035900980402     C                   SETOFF                                       03
036000980402     C                   END
036100980326     C* .... reperisco eventuale decodifica
036200980326     C                   Z-ADD     1             X
036300980326     C     BRVCAN        LOOKUP    C3E(X)                                 33
036400980326     C     *IN33         IFEQ      '1'                                          anomalia trv
036500980326     C                   MOVEL     BRVCAN        V3CERB
036600980326     C                   MOVEL     D3E(X)        V3DERB
036700980326     C                   END                                                    anom OK
036800980326     C                   END                                                    trovato anom
036900961008     C                   END                                                    letto errore
037000961008     C                   MOVEL     *IN03         V3CIN3
037100961008     C                   ADD       1             NRR3
037200961031     C                   SETON                                        22
037300961008     C                   WRITE     TC87S03
037400961031     C                   SETOFF                                       22
037500961008     C                   END                                                    carico sfl
037600961008      *  lettura successiva
037700980326     C     D87LNP        IFEQ      0
037800980326     C     KBRV          READE     EDBRV01L                               31
037900980326     C                   ELSE
038000980326     C     KBRV1         READE     EDBRV04L                               31
038100980326     C                   END
038200961008      *----------------------------------------------------*
038300961008      *  ... se sono a fine file EDBRV imposto a *HIVAL
038400961008      *      numero spedizione segnacollo in modo che risulti
038500961008      *      sempre superiore a quello di EDSUM
038600961008      *----------------------------------------------------*
038700961008     C   31              MOVE      *HIVAL        SPEBRV
038800961008     C*
038900961008     C                   ENDSR
039000961008     C*----------------------------------------------------*
039100961008     C*   CARSUM: Caricamento dati da EDSUM                *
039200961008     C*----------------------------------------------------*
039300961008     C     CARSUM        BEGSR
039400961008     C*
039500961009     C                   CLEAR                   TC87S03
039600961030     C                   SETON                                        03
039700961011     C                   Z-ADD     SUMAAS        V3CAAS
039800961008     C                   Z-ADD     SUMLNP        V3CLNP
039900961008     C                   Z-ADD     SUMNRS        V3CNRS
040000961008     C                   Z-ADD     SUMNSP        V3CNSP
040100961008     C                   Z-ADD     SUMNSC        V3CNSC
040200961025     C                   Z-ADD     SUMFLS        V3CFLS
040300961008     C                   Z-ADD     SUMLNA        V3CLAS
040400970620     C                   MOVEL     SUMCMR        V3CCMR
040500961025     C                   MOVEL     *BLANKS       V3CFGS
040600961008      *  Decodifico errore
040700961008     C                   Z-ADD     1             X
040800961008     C     SUMSTS        LOOKUP    CEA(X)                                 33
040900961008     C     *IN33         IFEQ      '1'                                          trovato err.
041000961008     C                   MOVEL     DEA(X)        DSEA
041100961009     C                   MOVEL     SUMSTS        V3CERS
041200961009     C                   MOVEL     §EADES        V3DERS
041300961008     C                   MOVEL     §EAMOD        V3CMER
041400961008     C                   END                                                    trovato err.
041500961008     C                   ADD       1             NRR3
041600961031     C                   SETON                                        22
041700961008     C                   WRITE     TC87S03
041800961031     C                   SETOFF                                       22
041900961008      *  lettura successiva
042000980326     C     D87LNP        IFEQ      0
042100961009     C     KSUM          READE     EDSUM01L                               32
042200980326     C                   ELSE
042300980326     C     KSUM1         READE     EDSUM04L                               32
042400980326     C                   END
042500961008      *----------------------------------------------------*
042600961008      *  ... se sono a fine file EDSUM imposto a *HIVAL
042700961008      *      numero spedizione segnacollo in modo che risulti
042800961008      *      sempre superiore a quello di EDBRV
042900961008      *----------------------------------------------------*
043000961008     C   32              MOVE      *HIVAL        SPESUM
043100961008     C*
043200961008     C                   ENDSR
043300961008     C*----------------------------------------------------*
043400961008     C*   CARTUT: Caricamento sia EDBRV che EDSUM          *
043500961008     C*----------------------------------------------------*
043600961008     C     CARTUT        BEGSR
043700961008     C*
043800961008     C                   SETON                                        03
043900961009     C                   CLEAR                   TC87S03
044000961011     C                   Z-ADD     SUMAAS        V3CAAS
044100961008     C                   Z-ADD     SUMLNP        V3CLNP
044200961008     C                   Z-ADD     SUMNRS        V3CNRS
044300961008     C                   Z-ADD     SUMNSP        V3CNSP
044400961008     C                   Z-ADD     SUMNSC        V3CNSC
044500961025     C                   Z-ADD     SUMFLS        V3CFLS
044600961008     C                   Z-ADD     SUMLNA        V3CLAS
044700970620     C                   MOVEL     SUMCMR        V3CCMR
044800961025     C                   MOVEL     *BLANKS       V3CFGS
044900980326      *  Reperisco dati errore
045000961008     C                   Z-ADD     1             X
045100961008     C     SUMSTS        LOOKUP    CEA(X)                                 33
045200961008     C     *IN33         IFEQ      '1'                                          trovato err.
045300961008     C                   MOVEL     DEA(X)        DSEA
045400961009     C                   MOVEL     SUMSTS        V3CERS
045500961009     C                   MOVEL     §EADES        V3DERS
045600961008     C                   MOVEL     §EAMOD        V3CMER
045700961008     C                   END                                                    trovato err.
045800980326      *----------------------------------------------------*
045900980326      * DATI DA EDBRV
046000980326     C                   CLEAR                   DSEA
046100980326     C* Se ho degli errori aggancio tabella EA
046200980326     C     BRVSTS        IFNE      *BLANKS                                      letto errore
046300980326     C     BRVFLG        ANDEQ     *BLANKS                                      errore
046400980326     C                   Z-ADD     1             X
046500980326     C     BRVSTS        LOOKUP    CEA(X)                                 33
046600980326     C     *IN33         IFEQ      '1'                                          trovato err.
046700980326     C                   MOVEL     DEA(X)        DSEA
046800980326     C                   END
046900980326     C* Se c'è anomalia e il tipo errore non prevale su di
047000980326     C* essa imposto flag stato evento da ignorare a 'S'
047100980326     C     BRVCAN        IFNE      *BLANKS
047200980326     C     §EACAN        ANDNE     'S'
047300980326     C                   MOVEL     'S'           §EASTS
047400980326     C                   END
047500980326     C                   END
047600961008      *----------------------------------------------------*
047700961008      *  ... carico i dati da BRV solo se ho richiesto di
047800961008      *      vedere sia gli errori che le bolle positive o
047900961008      *      se ho richiesto di vedere gli errori e ho letto
048000961008      *      una bolla con il codice di errore impostato
048100961008      *----------------------------------------------------*
048200961008     C     D87ERR        IFEQ      'N'                                          carico tutto
048300961011     C     BRVSTS        ORNE      *BLANKS                                      errore
048400961030     C     BRVFLG        ANDEQ     *BLANKS                                      errore
048500980326     C     §EASTS        ANDNE     'S'                                          no ignorare
048600980326     C     BRVCAN        ORNE      *BLANKS                                      o anomalia
048700961008     C                   Z-ADD     BRVNPS        V3CNPS
048800961008     C                   Z-ADD     BRVLNA        V3CLAB
048900961025     C                   MOVEL     *BLANKS       V3CFGB
049000961008      *  Decodifico errore se c'è
049100961008     C     BRVSTS        IFNE      *BLANKS                                      letto errore
049200961030     C     BRVFLG        ANDEQ     *BLANKS                                      errore
049300980326     C     *IN33         ANDEQ     '1'                                          trovato err.
049400980326     C     §EASTS        ANDNE     'S'                                          trovato err.
049500961008     C                   MOVEL     DEA(X)        DSEA
049600961009     C                   MOVEL     BRVSTS        V3CERB
049700961009     C                   MOVEL     §EADES        V3DERB
049800961008     C     §EAMOD        COMP      'S'                                0303
049900961008     C                   END                                                    trovato err.
050000980326     C* Se non c'è errore controllo se c'è anomalia
050100980326     C                   ELSE
050200980326     C     BRVCAN        IFNE      *BLANKS                                      anomalia
050300980326     C* .... reperisco eventuale decodifica
050400980326     C                   Z-ADD     1             X
050500980326     C     BRVCAN        LOOKUP    C3E(X)                                 33
050600980326     C     *IN33         IFEQ      '1'                                          anomalia trv
050700980326     C                   MOVEL     BRVCAN        V3CERB
050800980326     C                   MOVEL     D3E(X)        V3DERB
050900980326     C                   END                                                    trovato anom
051000980326     C                   END                                                    trovato anom
051100961008     C                   END                                                    carico BRV
051200980326     C*
051300961008     C                   MOVEL     *IN03         V3CIN3
051400961008     C                   ADD       1             NRR3
051500961031     C                   SETON                                        22
051600961008     C                   WRITE     TC87S03
051700961031     C                   SETOFF                                       22
051800961008      *  leggo entrambi
051900980326     C     D87LNP        IFEQ      0
052000961008     C     KBRV          READE     EDBRV01L                               31
052100961009     C     KSUM          READE     EDSUM01L                               32
052200980326     C                   ELSE
052300980326     C     KBRV1         READE     EDBRV04L                               31
052400980326     C     KSUM1         READE     EDSUM04L                               32
052500980326     C                   END
052600961008      *----------------------------------------------------*
052700961008      *  ... se sono a fine file EDBRV imposto a *HIVAL
052800961008      *      numero spedizione segnacollo in modo che risulti
052900961008      *      sempre superiore a quello di EDSUM
053000961008      *----------------------------------------------------*
053100961008     C   31              MOVE      *HIVAL        SPEBRV
053200961008      *----------------------------------------------------*
053300961008      *  ... se sono a fine file EDSUM imposto a *HIVAL
053400961008      *      numero spedizione segnacollo in modo che risulti
053500961008      *      sempre superiore a quello di EDBRV
053600961008      *----------------------------------------------------*
053700961008     C   32              MOVE      *HIVAL        SPESUM
053800961008      *
053900961008     C                   ENDSR
054000961003     C*----------------------------------------------------*
054100961003     C*   *INZSR: Operazioni iniziali                      *
054200961003     C*----------------------------------------------------*
054300961003     C     *INZSR        BEGSR
054400961003     C*
054500961003     C     *ENTRY        PLIST
054600961003     C                   PARM                    KPJBA
054700961011     C                   MOVEL     KPJBU         TRTC87
054800961003     C*  Richiamo XPARUT
054900961003     C                   Z-ADD     1             CODUT
055000961003     C                   CALL      'X§PARUT'
055100961003     C                   PARM                    UT§DSE
055200961003     C                   MOVEL     REC80         CNCR80
055300961003     C                   MOVEL     RAGUT         RSUT
055400961003     C*  Chiavi di accesso
055500961008     C     KBRV          KLIST
055600961008     C                   KFLD                    KCCM
055700961009     C                   KFLD                    KNFE
055800961009     C     KSUM          KLIST
055900961009     C                   KFLD                    KCCM
056000961009     C                   KFLD                    KNFE1
056100961011     C                   KFLD                    KFLG
056200980326     C     KBRV1         KLIST
056300980326     C                   KFLD                    KLNP
056400980326     C                   KFLD                    KNFE
056500980326     C     KSUM1         KLIST
056600980326     C                   KFLD                    KLNP
056700980326     C                   KFLD                    KNFE1
056800980326     C                   KFLD                    KFLG
056900980326     C     KTAB          KLIST
057000980326     C                   KFLD                    KKUT
057100980326     C                   KFLD                    KCOD1
057200040518      *
057300040518     C     KPTN          KLIST
057400040518     C                   KFLD                    PTNCOD
057500040518     C                   KFLD                    PTNKEY
057600040518      *
057700961003     C*  Definizione variabili
057800040518     C     *LIKE         DEFINE    TABCOD        PTNCOD
057900040518     C     *LIKE         DEFINE    TABKEY        PTNKEY
058000961003     C     *LIKE         DEFINE    TABCOD        KCOD
058100980326     C     *LIKE         DEFINE    BRVLNP        KLNP
058200980326     C     *LIKE         DEFINE    BRVCCM        KCCM
058300961008     C     *LIKE         DEFINE    BRVNFE        KNFE
058400961009     C     *LIKE         DEFINE    SUMNFE        KNFE1
058500961011     C     *LIKE         DEFINE    SUMFLG        KFLG
058600980326     C     *LIKE         DEFINE    TBLKUT        KKUT
058700980326     C     *LIKE         DEFINE    TBLCOD        KCOD1
058800961009     C*  Carico in schiera codici tabella - EA -
058900961009     C                   Z-ADD     0             X                 4 0
059000010405     C                   CLEAR                   DEA
059100010405     C                   CLEAR                   CEA
059200961009     C                   MOVEL     'EA'          KCOD
059300961009     C     KCOD          CHAIN     EDTAB01L                           31
059400961009     C     *IN31         DOWEQ     '0'
059500961009     C     TABFLG        IFEQ      ' '
059600961009     C                   ADD       1             X
059700961009     C                   MOVEL     TABUNI        DEA(X)
059800961009     C                   MOVEL     TABKEY        CEA(X)
059900961009     C                   END
060000961009     C     KCOD          READE     EDTAB01L                               31
060100961009     C                   END
060200980326     C*  Carico in schiera codici tabella - 3E -
060300980326     C                   Z-ADD     0             X                 4 0
060400010405     C                   CLEAR                   D3E
060500010405     C                   CLEAR                   C3E
060600980326     C                   Z-ADD     1             KKUT
060700980326     C                   MOVEL     '3E'          KCOD1
060800980326     C     KTAB          CHAIN     TABEL00F                           31
060900980326     C     *IN31         DOWEQ     '0'
061000980326     C     TBLFLG        IFEQ      ' '
061100980326     C                   ADD       1             X
061200980326     C                   MOVEL     TBLUNI        D3E(X)
061300980326     C                   MOVEL     TBLKEY        C3E(X)
061400980326     C                   END
061500980326     C     KTAB          READE     TABEL00F                               31
061600980326     C                   END
061700961003     C*  Inizializzo variabili
061800961003     C                   MOVEL     'N'           WFINE             1
061900961003     C*
062000961003     C                   ENDSR
062100961008**
062200961008Punto operativo inesistente o annullato                                01
062300961008Punto operativo non gestito su questo AS                               02
062400961008Il punto operativo non si gestisce in modo autonomo                    03
062500961008Codice cliente inesistente o annullato                                 04
062600961008Codice cliente non relativo ad un partner                              05
062700961008Per il cliente selezionato non ci sono date di arrivo da trasmettere   06
