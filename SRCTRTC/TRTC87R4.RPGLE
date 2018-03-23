000100050927     H DECEDIT('0,') DATEDIT(*YMD.)
000200961009     H*---------------------------------------------------------------------*
000300961009     H*  Ritorno date arrivo ai partner  - E.D.I. - Estero -                *
000400961009     F*---------------------------------------------------------------------*
000500961009     F*  DATA BASE                                                          *
000600961009     F*---------------------------------------------------------------------*
000700961009     FEDRDE01L  IF   E           K DISK
000800961009     F*---------
000900961010     FEDTAB01L  IF   E           K DISK
001000961010     F*---------
001100961010     FFNBLP01L  IF   E           K DISK
001200961010     F*---------
001300050926     Ffnblt01l  IF   E           K DISK
001400050926     F*---------
001500060213     Ffiar401L  IF   E           K DISK
001600980108     F*---------
001700980108     FEDSUM04L  UF   E           K DISK
001800980108     F                                     RENAME(EDSUM000:EDSUMLNP)
001900961009     F*---------
002000961009     FEDSUM01L  UF   E           K DISK
002100961009     F*---------
002200961025     FEDBRV02L  UF   E           K DISK
002300961009     F*---------
002400980108     FEDBRV03L  UF   E           K DISK
002500980108     F                                     RENAME(EDBRV000:EDBRVLNP)
002600980108     F*---------
002700961010     FEDSTAWRK  UF A E           K DISK
002800980331     F*---------
002900980401     FTRTC87D6  IF   E             WORKSTN USROPN
003000980331     F                                     SFILE(TC87S01:NRR1)
003100961010     D*---------------------------------------------------------------------*
003200961010     D*  SCHIERE
003300961010     D*---------------------------------------------------------------------*
003400961010     D* Tabella errori date arrivo
003500961010     D CEA             S              3    DIM(100)                             codici errori
003600961010     D DEA             S             90    DIM(100)                             dati tab.EA
003700961009     D*---------------------------------------------------------------------*
003800961009     D* DS
003900961009     D*---------------------------------------------------------------------*
004000961009     D KPJBA         E DS
004100961009     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004200961009     D CNCR80        E DS
004300961009     D TRTC87        E DS                  EXTNAME(TRTC87DS)
004400020726     D DSCL          E DS                  EXTNAME(EDIDSCL)
004500961010     D DSPT          E DS                  EXTNAME(EDIDSPT)
004600961010     D DSPU          E DS                  EXTNAME(EDIDSPU)
004700961010     D DSEA          E DS                  EXTNAME(EDIDSEA)
004800961009     C*---------------------------------------------------------------------*
004900961009     C* Ciclo principale
005000961009     C*---------------------------------------------------------------------*
005100961009      *  Carico da EDSUM tutte le bolle a cui ho attribuito il
005200961009      *  foglio entrata e cancello record
005300980108     C     D87LNP        IFEQ      0
005400961009     C                   Z-ADD     D87CCM        KCCM
005500961009     C                   Z-ADD     D87NFE        KNFE
005600961009     C     KSUM          CHAIN     EDSUM01L                           31
005700980108     C                   ELSE
005800980108     C                   Z-ADD     D87LNP        KLNP
005900980108     C                   Z-ADD     D87NFE        KNFE
006000980108     C     KSUM4         CHAIN     EDSUM04L                           31
006100980108     C                   END
006200961009     C     *IN31         DOWEQ     '0'
006300961010     C                   EXSR      WRTSUM
006400961009     C                   END
006500980327      *---------------------------------------------------*
006600980327      * Gestione scrittura errori da EDBRV
006700980327     C                   EXSR      WRTERR
006800980327      *---------------------------------------------------*
006900980327      * Gestione scrittura dati corretti
007000980327     C                   EXSR      WRTDAT
007100980327      *---------------------------------------------------*
007200980331      * Aggiorno dati eccedenze
007300980331     C     D87ECC        IFEQ      'S'
007400980331     C     D87WRT        ANDEQ     'S'
007500980331     C                   OPEN      TRTC87D6
007600980331     C                   EXSR      WRTECC
007700980331     C                   END
007800980331      *---------------------------------------------------*
007900961009      * Fine
008000961010     C                   MOVEL     KKEY          D87CPT
008100961010     C                   MOVEL     TRTC87        KPJBU
008200140911      *
008300961009     C                   SETON                                        LR
008400980327      *------------------------------------------------------------*
008500980327      * Carico errori in file di Work
008600980327      *------------------------------------------------------------*
008700980327     C     WRTERR        BEGSR
008800980327      *---------------------------------------------------*
008900980327      *  Carico dati da bolle errori non annullate da BRV
009000980327      *  mi posiziono con flag annullamento abblencato
009100980327      *  e errori a si x scrivere solo errori rimasti
009200980327     C                   MOVEL     *BLANKS       KFLG
009300980327     C                   MOVEL     *BLANKS       KSTS
009400140911      *
009500980327     C     D87LNP        IFEQ      0
009600980401     C                   Z-ADD     KCCM          KKEY1
009700980330     C     KBRV3         CHAIN     EDBRV02L                           31
009800980327     C                   ELSE
009900980401     C                   Z-ADD     KLNP          KKEY1
010000980330     C     KBRV6         CHAIN     EDBRV03L                           31
010100980327     C                   END
010200140911      *
010300140911      *------------------>
010400980327      * Loop letture
010500980327     C     *IN31         DOWEQ     '0'
010600980402     C     BRVFLG        ANDEQ     ' '
010700140911      *
010800980331     C                   MOVEL     BRVSTS        KSTS
010900980331     C                   MOVEL     BRVCAN        KCAN
011000140911      *
011100980327      * Aggancio dati stato della spunta
011200980327     C                   Z-ADD     1             X
011300980327     C                   CLEAR                   DSEA
011400140911      *
011500980328     C     BRVFLG        IFEQ      *BLANKS
011600980327     C     BRVSTS        LOOKUP    CEA(X)                                 32
011700980328     C                   ELSE
011800980328     C                   MOVEL     *BLANKS       WSTS              3
011900980402     C                   MOVEL     *BLANKS       BRVCAN
012000980328     C     WSTS          LOOKUP    CEA(X)                                 32
012100980328     C                   END
012200140911      *
012300980328     C   32              MOVEL     DEA(X)        DSEA
012400140911      *
012500980327      * Se lo stato è di errore ed è un errore da non ignorare
012600980327      * e non c'è nessuna anomalia di spunta .....
012700980327     C     §EATPE        IFNE      *BLANKS
012800980327     C     §EASTS        ANDEQ     *BLANKS
012900980327     C     BRVCAN        ANDEQ     *BLANKS
013000980327      * ... oppure se lo stato è di errore ed è un errore da
013100980327      * non ignorare, c'è una anomalia di spunta ma l'errore
013200980327      * deve prevalere scrivo l'evento errato
013300980327     C     §EATPE        ORNE      *BLANKS
013400980327     C     §EASTS        ANDEQ     *BLANKS
013500980402     C     §EACAN        ANDEQ     'S'
013600980327     C     BRVCAN        ANDNE     *BLANKS
013700140911      *
013800980330     C                   EXSR      WRTBRE
013900140911      *
014000980327      * ... altrimenti se c'è anomalia di spunta scrivo quella
014100980327     C                   ELSE
014200140911      *
014300980327     C     BRVCAN        IFNE      *BLANKS
014400980327     C                   EXSR      WRTANO
014500980327     C                   ELSE
014600140911      *
014700980327      * ... se non c'è anomalia di spunta mi posiziono su
014800980327      * codice stato/anomalia successivi
014900980327     C     D87LNP        IFEQ      0
015000980330     C     KBRV7         SETGT     EDBRV02L
015100980330     C     KBRV3         READE     EDBRV02L                               31
015200980327     C                   ELSE
015300980330     C     KBRV4         SETGT     EDBRV03L
015400980330     C     KBRV6         READE     EDBRV03L                               31
015500980327     C                   END
015600980327      *
015700980327     C                   END                                                    Scrivo anom.spunta
015800980327     C                   END                                                    Scrivo errore
015900140911      *
016000980327     C                   END                                                    Loop EDBRV
016100140911      *
016200980327     C                   ENDSR
016300980327      *------------------------------------------------------------*
016400980327      * Carico dati corretti
016500980327      *------------------------------------------------------------*
016600980327     C     WRTDAT        BEGSR
016700980327      *
016800980327      *  Carico dati Bolle senza errori
016900980327     C     D87LNP        IFEQ      0
017000980327     C     KBRV3         CHAIN     EDBRV02L                           31
017100980327     C                   ELSE
017200980327     C     KBRV6         CHAIN     EDBRV03L                           31
017300980327     C                   END
017400980331      *
017500980331     C                   Z-ADD     1             X
017600980331     C                   CLEAR                   DSEA
017700980331     C     D87RIT        IFEQ      *BLANKS
017800980331     C     '   '         LOOKUP    CEA(X)                                 32
017900980331     C                   ELSE
018000980331     C     'RIT'         LOOKUP    CEA(X)                                 32
018100980331     C                   END
018200980331     C   32              MOVEL     DEA(X)        DSEA
018300980331      *
018400980327     C     *IN31         DOWEQ     '0'
018500980327      *  Ritorno i dati della bolla solo se devo tornare le
018600980327      *  bolle positive: Scrivo la bolla solo se non esiste
018700980327      * -> se segnacolli sempre scrivo la bolla solo se non
018800980327      *     esiste già segnacollo con errore
018900980327      * -> se segnacolli solo negativi o mai scrivo la bolla
019000980327      *    solo non esiste nessun segnacollo ERRATO x essa
019100140911      *
019200980327     C     D87WRT        IFEQ      'S'
019300980327     C     §PUDTB        IFEQ      'E'
019400140911      *
019500980327      * carico da EDRDE nr.cmr/afb
019600980401     C     §PTCMR        IFEQ      'S'
019700980327     C                   Z-ADD     BRVAAS        KAAS
019800980327     C                   Z-ADD     BRVLNP        KLNP
019900980327     C                   Z-ADD     BRVNRS        KNRS
020000980327     C                   Z-ADD     BRVNSP        KNSP
020100980327     C                   MOVEL     'TD1154'      KNMC
020200980327     C                   Z-ADD     1             KNRP
020300980327     C                   MOVEL     *BLANKS       RDEVAL
020400980327     C     KRDE          CHAIN     EDRDE01L                           32
020500140911      *
020600980401     C                   MOVEL     RDEVAL        KCMR
020700980401     C                   ELSE
020800980401     C                   MOVEL     *BLANKS       KCMR
020900980401     C                   MOVEL     '0'           KCMR
021000980401     C                   END
021100140911      *
021200980327     C                   Z-ADD     BRVAAS        KAAS
021300980327     C                   Z-ADD     BRVLNP        KLNP
021400980327     C                   Z-ADD     BRVNRS        KNRS
021500980327     C                   Z-ADD     BRVNSP        KNSP
021600980327     C                   MOVE      *HIVAL        STANSC
021700980327     C     KSTA          CHAIN     EDSTAWRK                           32
021800140911      *
021900980327     C     §PUSGD        IFNE      'S'
022000980327     C     *IN32         IFEQ      '1'
022100980327     C     STAERR        OREQ      '309'
022200980327     C     STAERR        OREQ      '   '
022300980327     C                   EXSR      WRTBOK
022400980327     C                   END
022500980327     C                   ELSE
022600980327     C     *IN32         DOWEQ     '0'
022700980327     C     STANSC        ANDNE     BRVNSC
022800980327     C     KSTA          READE     EDSTAWRK                               32
022900980327     C                   END
023000980327     C     STANSC        IFNE      BRVNSC
023100980327     C                   EXSR      WRTBOK
023200980327     C                   END
023300980327     C                   END
023400140911      *
023500980327     C*  Se devo tornare solo le bolle negative con tutti i
023600980327     C*  scrivo la bolla solo se esiste già il record in
023700980327     C*  EDSTA --> errori  - con quel nr.CMR
023800980327     C                   ELSE
023900140911      *
024000980327      * carico da EDRDE nr.cmr/afb
024100980327     C                   Z-ADD     BRVAAS        KAAS
024200980327     C                   Z-ADD     BRVLNP        KLNP
024300980327     C                   Z-ADD     BRVNRS        KNRS
024400980327     C                   Z-ADD     BRVNSP        KNSP
024500980327     C                   MOVEL     'TD1154'      KNMC
024600980327     C                   Z-ADD     1             KNRP
024700980327     C                   MOVEL     *BLANKS       RDEVAL
024800980327     C     KRDE          CHAIN     EDRDE01L                           32
024900980327     C*
025000980327     C                   Z-ADD     BRVAAS        KAAS
025100980327     C                   Z-ADD     BRVLNP        KLNP
025200980327     C                   Z-ADD     BRVNRS        KNRS
025300980327     C                   Z-ADD     BRVNSP        KNSP
025400980327     C                   MOVEL     RDEVAL        KCMR
025500980327     C     KSTA          CHAIN     EDSTAWRK                           32
025600140911      *
025700980327     C     *IN32         IFEQ      '0'
025800980327     C     §PUSGD        ANDEQ     'S'
025900980327     C     *IN32         DOWEQ     '0'
026000980327     C     STANSC        ANDNE     BRVNSC
026100980327     C     KSTA          READE     EDSTAWRK                               32
026200980327     C                   END
026300980327     C     STANSC        IFNE      BRVNSC
026400980327     C                   EXSR      WRTBOK
026500980327     C                   END
026600980327     C                   END
026700980327     C*
026800980327     C                   END
026900980327     C                   END
027000980327      * cancello record
027100980327      * prossima lettura
027200980327     C     D87LNP        IFEQ      0
027300980327     C                   DELETE    EDBRV000
027400980327     C     KBRV3         READE     EDBRV02L                               31
027500980327     C                   ELSE
027600980327     C                   DELETE    EDBRVLNP
027700980327     C     KBRV6         READE     EDBRV03L                               31
027800980327     C                   END
027900980327     C                   END
028000980330     C*
028100980330     C                   ENDSR
028200961010     C*------------------------------------------------------------*
028300961010     C*  WRTSUM: Scrivo dati da EDSUM
028400961010     C*------------------------------------------------------------*
028500961010     C     WRTSUM        BEGSR
028600961010     C*
028700961011     C     SUMFLG        IFEQ      *BLANKS
028800961010     C                   CLEAR                   EDSTA000
028900961010     C                   Z-ADD     SUMAAS        STAAAS
029000961010     C                   Z-ADD     SUMLNP        STALNP
029100970626     C                   Z-ADD     SUMFLS        STAFLS
029200961010     C                   Z-ADD     SUMLNA        STALNA
029300961010     C                   Z-ADD     SUMZNC        STAZNC
029400961010     C                   Z-ADD     SUMNRS        STANRS
029500961010     C                   Z-ADD     SUMNSP        STANSP
029600961015     C     STANSP        IFEQ      0
029700961015     C                   Z-ADD     SUMNSC        STANSP
029800961015     C                   END
029900961015     C                   Z-ADD     SUMNSC        STANSC
030000961010     C                   Z-ADD     SUMCCM        STACCM
030100961010     C                   MOVEL     WTPDOC        STATPD
030200961010     C                   MOVEL     SUMCMR        STACMR
030300961010     C                   MOVEL     D87NFE        STANFE
030400961010     C                   MOVEL     D87DFE        STADFE
030500961010     C                   MOVEL     SUMCNI        STACNI
030600961010      * Mi posiziono sulla bolla x reperire la data ritiro
030700961010     C                   Z-ADD     SUMAAS        KAAS
030800961010     C                   Z-ADD     SUMLNP        KLNP
030900961010     C                   Z-ADD     SUMNRS        KNRS
031000961010     C                   Z-ADD     SUMNSP        KNSP
031100050926      **
031200961010      * Se non la trovo imposto la data del foglio entrata
031300050928      *      Adesso imposta sempre la data foglio : dal 28/9/2005
031400961015     C                   Z-ADD     D87DFE        STADTR
031500050926      **
031600050929      **  Ma se c'è la data di spunta entrata su dettaglio Segnacollo
031700050929      **   prende questa.
031800050929     C                   z-add     sumnsc        knsc
031900050929     C     kblt          CHAIN     fnblt01l
032000050929     c                   if        %Found(fnblt01l) and bltDSE >0
032100050929     C                   z-add     bltDSE        STADTR
032200050926     c                   endIf
032300050926      **
032400961010      * CERCO ERRORE
032500961015     C                   Z-ADD     1             X
032600961010     C     SUMSTS        LOOKUP    CEA(X)                                 32
032700961010     C     *IN32         IFEQ      '1'
032800961010     C                   MOVEL     DEA(X)        DSEA
032900140911     C*
033000140911      **  imposta lo STS stato
033100140911     C                   Exsr      Imposta_STS
033200140911     C*
033300980330     C                   END
033400961010      * scrivo EDSTA
033500980401     C                   Z-ADD     KKEY1         STAKEY
033600980402     C                   MOVEL     'S'           STAFLE
033700961010     C                   WRITE     EDSTA000
033800961011     C                   END
033900140911      *
034000980108     C     D87LNP        IFEQ      0
034100980108      * cancello record
034200980108     C                   DELETE    EDSUM000
034300980108      * prossima lettura
034400980108     C     KSUM          READE     EDSUM01L                               31
034500980108     C                   ELSE
034600980108      * cancello record
034700980108     C                   DELETE    EDSUMLNP
034800980108      * prossima lettura
034900980108     C     KSUM4         READE     EDSUM04L                               31
035000980108     C                   END
035100961010     C*
035200961010     C                   ENDSR
035300961010     C*------------------------------------------------------------*
035400980330     C*  WRTBRE: Scrivo ERRORI DA EDBRV
035500961010     C*------------------------------------------------------------*
035600980330     C     WRTBRE        BEGSR
035700980331     C*
035800980331     C     *IN31         DOWEQ     '0'
035900980402     C     BRVSTS        ANDEQ     KSTS
036000980402     C     BRVCAN        ANDEQ     KCAN
036100980402     C     BRVFLG        ANDEQ     *BLANKS
036200961024     C     D87WRT        IFEQ      'S'
036300961010     C                   CLEAR                   EDSTA000
036400980401     C                   Z-ADD     KKEY1         STAKEY
036500961010     C                   MOVEL     D87NFE        STANFE
036600961010     C                   MOVEL     D87DFE        STADFE
036700961010     C                   Z-ADD     BRVAAS        STAAAS
036800961010     C                   Z-ADD     BRVLNP        STALNP
036900970626     C                   Z-ADD     BRVFLS        STAFLS
037000961010     C                   Z-ADD     BRVLNA        STALNA
037100961010     C                   Z-ADD     BRVZNC        STAZNC
037200961010     C                   Z-ADD     BRVNRS        STANRS
037300961010     C                   Z-ADD     BRVNSP        STANSP
037400961015     C     STANSP        IFEQ      0
037500961015     C                   Z-ADD     BRVNSC        STANSP
037600961015     C                   END
037700961010     C                   Z-ADD     BRVNSC        STANSC
037800961010     C                   Z-ADD     BRVCCM        STACCM
037900961010     C                   MOVEL     WTPDOC        STATPD
038000050926      **
038100140911      **   aggancia la Bolla in Partenza per reperire i riferimenti
038200140911     c                   exsr      chk_BollaPart
038300050927      **
038400961010      * scrivo EDSTA
038500970218     C     §EATPE        IFEQ      'E'
038600970218     C     *IN34         OREQ      '0'
038700980402     C                   MOVEL     'S'           STAFLE
038800970218     C                   WRITE     EDSTA000
038900961024     C                   END
039000140911      **
039100970218     C                   END
039200140911      **
039300980108     C     D87LNP        IFEQ      0
039400980108      * cancello record
039500980108     C                   DELETE    EDBRV000
039600980108      * prossima lettura
039700980331     C     KBRV3         READE     EDBRV02L                               31
039800980108     C                   ELSE
039900980108      * cancello record
040000980108     C                   DELETE    EDBRVLNP
040100980108      * prossima lettura
040200980331     C     KBRV6         READE     EDBRV03L                               31
040300980108     C                   END
040400980331     C                   END
040500961010     C*
040600961010     C                   ENDSR
040700980330     C*------------------------------------------------------------*
040800980330     C*  WRTANO: Scrivo ERRORI DA EDBRV
040900980330     C*------------------------------------------------------------*
041000980330     C     WRTANO        BEGSR
041100980330     C*
041200980331     C     *IN31         DOWEQ     '0'
041300980402     C     BRVSTS        ANDEQ     KSTS
041400980402     C     BRVCAN        ANDEQ     KCAN
041500980402     C     BRVFLG        ANDEQ     *BLANKS
041600980330     C     D87WRT        IFEQ      'S'
041700980330     C                   CLEAR                   EDSTA000
041800980401     C                   Z-ADD     KKEY1         STAKEY
041900980330     C                   MOVEL     D87NFE        STANFE
042000980330     C                   MOVEL     D87DFE        STADFE
042100980330     C                   Z-ADD     BRVAAS        STAAAS
042200980330     C                   Z-ADD     BRVLNP        STALNP
042300980330     C                   Z-ADD     BRVFLS        STAFLS
042400980330     C                   Z-ADD     BRVLNA        STALNA
042500980330     C                   Z-ADD     BRVZNC        STAZNC
042600980330     C                   Z-ADD     BRVNRS        STANRS
042700980330     C                   Z-ADD     BRVNSP        STANSP
042800980330     C     STANSP        IFEQ      0
042900980330     C                   Z-ADD     BRVNSC        STANSP
043000980330     C                   END
043100980330     C                   Z-ADD     BRVNSC        STANSC
043200980330     C                   Z-ADD     BRVCCM        STACCM
043300980330     C                   MOVEL     WTPDOC        STATPD
043400140911      **
043500980330     C                   Z-ADD     1             X
043600980330     C     'A  '         LOOKUP    CEA(X)                                 32
043700980330     C   32              MOVEL     DEA(X)        DSEA
043800140911      **
043900140911      **   aggancia la Bolla in Partenza per reperire i riferimenti
044000140911     c                   exsr      chk_BollaPart
044100050926      **
044200980330      * scrivo EDSTA
044300980330     C     §EATPE        IFEQ      'E'
044400980330     C     *IN34         OREQ      '0'
044500980402     C                   MOVEL     'S'           STAFLE
044600980330     C                   WRITE     EDSTA000
044700980330     C                   END
044800140911      **
044900980330     C                   END
045000140911      **
045100980330     C     D87LNP        IFEQ      0
045200980330      * cancello record
045300980330     C                   DELETE    EDBRV000
045400980330      * prossima lettura
045500980331     C     KBRV3         READE     EDBRV02L                               31
045600980330     C                   ELSE
045700980330      * cancello record
045800980330     C                   DELETE    EDBRVLNP
045900980330      * prossima lettura
046000980331     C     KBRV6         READE     EDBRV03L                               31
046100980330     C                   END
046200980331     C                   END
046300980330     C*
046400980330     C                   ENDSR
046500961010     C*------------------------------------------------------------*
046600961010     C*  WRTBOK: Scrivo dati da EDBRV x no errori
046700961010     C*------------------------------------------------------------*
046800961010     C     WRTBOK        BEGSR
046900961010     C*
047000961010     C                   CLEAR                   EDSTA000
047100980401     C                   Z-ADD     KKEY1         STAKEY
047200961010     C                   Z-ADD     BRVAAS        STAAAS
047300961010     C                   Z-ADD     BRVLNP        STALNP
047400970626     C                   Z-ADD     BRVFLS        STAFLS
047500961010     C                   Z-ADD     BRVLNA        STALNA
047600961010     C                   Z-ADD     BRVZNC        STAZNC
047700961010     C                   Z-ADD     BRVNRS        STANRS
047800961010     C                   Z-ADD     BRVNSP        STANSP
047900961015     C     STANSP        IFEQ      0
048000961015     C                   Z-ADD     BRVNSC        STANSP
048100961015     C                   END
048200961010     C                   Z-ADD     BRVNSC        STANSC
048300961010     C                   Z-ADD     BRVCCM        STACCM
048400961010     C                   MOVEL     D87NFE        STANFE
048500961010     C                   MOVEL     D87DFE        STADFE
048600961030     C                   MOVEL     WTPDOC        STATPD
048700140911      **
048800140911      **   aggancia la Bolla in Partenza per reperire i riferimenti
048900140911     c                   exsr      chk_BollaPart
049000050926      **
049100961010      * scrivo EDSTA
049200980402     C                   MOVEL     *BLANKS       STAFLE
049300961010     C                   WRITE     EDSTA000
049400961010     C*
049500961010     C                   ENDSR
049600980331     C*------------------------------------------------------------*
049700980331     C*  WRTECC: Scrivo eccedenze Euroexpress
049800980331     C*------------------------------------------------------------*
049900980331     C     WRTECC        BEGSR
050000980331     C*
050100980331     C                   SETON                                        2120
050200980331     C                   Z-ADD     1             NRR1              4 0
050300980331     C                   Z-ADD     1             NR1KEY            4 0
050400980401     C***                  WRITETC87Z01
050500980401     C     NRR1          CHAIN     TC87S01                            29
050600980331     C     *IN29         DOWEQ     '0'
050700980331     C     V1CNOT        IFNE      *BLANKS
050800980331     C                   CLEAR                   EDSTA000
050900980401     C                   Z-ADD     KKEY1         STAKEY
051000980331     C                   MOVEL     WTPDOC        STATPD
051100980331     C                   MOVEL     D87NFE        STANFE
051200980331     C                   MOVEL     D87DFE        STADFE
051300980331      * CERCO ERRORE
051400980331     C                   Z-ADD     1             X
051500980331     C     '002'         LOOKUP    CEA(X)                                 32
051600980331     C     *IN32         IFEQ      '1'
051700980331     C                   MOVEL     DEA(X)        DSEA
051800140911     C*
051900140911      **  imposta lo STS stato
052000140911     C                   Exsr      Imposta_STS
052100140911     C*
052200980331     C                   END
052300980401     C                   MOVEL     '0'           STACMR
052400980331     C                   MOVEL     'JUT'         STACNI
052500980331     C                   MOVE      V1CNOT        STACNI
052600980331      * scrivo EDSTA
052700980402     C                   MOVEL     'S'           STAFLE
052800980331     C                   WRITE     EDSTA000
052900980331     C                   END
053000980331      *
053100980331     C                   ADD       1             NRR1
053200980331     C     NRR1          CHAIN     TC87S01                            29
053300980331     C                   END
053400980331      *
053500980331     C                   ENDSR
053600961009     C*------------------------------------------------------------*
053700961009     C* OPERAZIONI INIZIALI
053800961009     C*------------------------------------------------------------*
053900961009     C     *INZSR        BEGSR
054000961009     C*
054100961009     C     *ENTRY        PLIST
054200961009     C                   PARM                    KPJBA
054300961009     C                   MOVEL     KPJBU         TRTC87
054400961009     C*  Definizione chiavi
054500961010     C     KBRV3         KLIST
054600961010     C                   KFLD                    KCCM
054700961010     C                   KFLD                    KNFE
054800980108     C     KBRV4         KLIST
054900980108     C                   KFLD                    KLNP
055000980108     C                   KFLD                    KNFE
055100980108     C                   KFLD                    KFLG
055200980108     C                   KFLD                    KSTS
055300980330     C                   KFLD                    KCAN
055400980108     C     KBRV5         KLIST
055500980108     C                   KFLD                    KLNP
055600980108     C                   KFLD                    KNFE
055700980108     C                   KFLD                    KFLG
055800980330     C                   KFLD                    KSTS
055900980108     C     KBRV6         KLIST
056000980108     C                   KFLD                    KLNP
056100980108     C                   KFLD                    KNFE
056200980330     C     KBRV7         KLIST
056300980330     C                   KFLD                    KCCM
056400980330     C                   KFLD                    KNFE
056500980330     C                   KFLD                    KFLG
056600980330     C                   KFLD                    KSTS
056700980330     C                   KFLD                    KCAN
056800961009     C     KSUM          KLIST
056900961009     C                   KFLD                    KCCM
057000961009     C                   KFLD                    KNFE
057100980108     C     KSUM4         KLIST
057200980108     C                   KFLD                    KLNP
057300980108     C                   KFLD                    KNFE
057400961009     C     KRDE          KLIST
057500961009     C                   KFLD                    KAAS
057600961009     C                   KFLD                    KLNP
057700961009     C                   KFLD                    KNRS
057800961009     C                   KFLD                    KNSP
057900961009     C                   KFLD                    KNMC
058000961009     C                   KFLD                    KNRP
058100060213     C     Kar4          KLIST
058200961009     C                   KFLD                    KAAS
058300961009     C                   KFLD                    KLNP
058400961009     C                   KFLD                    KNRS
058500961009     C                   KFLD                    KNSP
058600961015     C                   KFLD                    KTRC
058700961010     C     KBLP          KLIST
058800961010     C                   KFLD                    KAAS
058900961010     C                   KFLD                    KLNP
059000961010     C                   KFLD                    KNRS
059100961010     C                   KFLD                    KNSP
059200050926     C     KBLT          KLIST
059300050926     C                   KFLD                    KAAS
059400050926     C                   KFLD                    KLNP
059500050926     C                   KFLD                    KNRS
059600050926     C                   KFLD                    KNSP
059700050926     C                   KFLD                    KNSC
059800961010     C     KTAB          KLIST
059900961029     C                   KFLD                    KCOD
060000961010     C                   KFLD                    KKEY
060100961010     C     KSTA          KLIST
060200980401     C                   KFLD                    KKEY1
060300961010     C                   KFLD                    KNFE
060400961010     C                   KFLD                    KCMR
060500961010     C                   KFLD                    KAAS
060600961010     C                   KFLD                    KLNP
060700961010     C                   KFLD                    KNRS
060800961010     C                   KFLD                    KNSP
060900961009     C*  Definizione variabili
061000961009     C     *LIKE         DEFINE    BRVCCM        KCCM
061100961009     C     *LIKE         DEFINE    BRVNFE        KNFE
061200961009     C     *LIKE         DEFINE    BRVFLG        KFLG
061300961009     C     *LIKE         DEFINE    BRVSTS        KSTS
061400980330     C     *LIKE         DEFINE    BRVCAN        KCAN
061500961009     C     *LIKE         DEFINE    RDEAAS        KAAS
061600961009     C     *LIKE         DEFINE    RDELNP        KLNP
061700961009     C     *LIKE         DEFINE    RDENRS        KNRS
061800961009     C     *LIKE         DEFINE    RDENSP        KNSP
061900050926     C     *like         define    bltnsc        knsc
062000060213     C     *LIKE         DEFINE    ar4TRC        KTRC
062100961009     C     *LIKE         DEFINE    RDENMC        KNMC
062200961009     C     *LIKE         DEFINE    RDENRP        KNRP
062300961010     C     *LIKE         DEFINE    TABCOD        KCOD
062400961010     C     *LIKE         DEFINE    TABKEY        KKEY
062500961010     C     *LIKE         DEFINE    STACMR        KCMR
062600980401     C     *LIKE         DEFINE    STAKEY        KKEY1
062700961009     C*
062800961029     C                   MOVEL     D87CCM        WCCM              7
062900020221     C                   MOVE      D87CCM        WKSC              7 0
063000961010     C*  Carico in schiera codici tabella - EA -
063100961010     C                   Z-ADD     0             X                 4 0
063200961010     C                   MOVEL     'EA'          KCOD
063300961010     C                   MOVEA     *BLANKS       DEA
063400961010     C                   MOVEA     *BLANKS       CEA
063500961010     C     KCOD          CHAIN     EDTAB01L                           31
063600961010     C     *IN31         DOWEQ     '0'
063700961010     C     TABFLG        IFEQ      ' '
063800961010     C                   ADD       1             X
063900961010     C                   MOVEL     TABUNI        DEA(X)
064000961010     C                   MOVEL     TABKEY        CEA(X)
064100961010     C                   END
064200961010     C     KCOD          READE     EDTAB01L                               31
064300961010     C                   END
064400020726      *
064500020726     C*  Carico DATI tabella - CL -  se il pgm deve elaborare x cliente
064600020726      *   ed è un codice inviato tramite RFF+FF quindi decodificato sulla
064700020726      *    tabella CL. Se trovato deve memorizzarselo e provare a cercarlo
064800020726      *     con il codice del Partner relativo (sempre dalla CL) per prendere
064900020726      *      poi l'UNB.
065000020726     C                   Z-ADD     0             X
065100020726     C                   z-add     0             CodPt7            7 0
065200020726      * se elaborazione x cliente
065300020726     c                   if        wksc <> 0
065400020726     C                   MOVEL     'CL'          KCOD
065500020726     C     KCOD          CHAIN     EDTAB01L                           31
065600020726     C     *IN31         DOWEQ     '0'                                          Read EDTAB
065700020726     C     TABFLG        IFEQ      ' '                                          flg annull
065800020726     C                   MOVEL     TABUNI        DSCL
065900020726     C     §CLKSC        IFEQ      WKSC                                         cod.cli.
066000020726     C                   MOVEL     TABKEY        CodPt7
066100020726     c                   leave
066200020726     C                   END                                                    cod.cli
066300020726     C                   END                                                    flg.annul
066400020726     C     KCOD          READE     EDTAB01L                               31
066500020726     C                   END                                                    READ EDTAB
066600020726     C                   Endif                                                  READ EDTAB
066700020726      *
066800961010     C*  Carico DATI tabella - PT -
066900961010     C                   Z-ADD     0             X                 4 0
067000961010     C                   MOVEL     'PT'          KCOD
067100961010     C     KCOD          CHAIN     EDTAB01L                           31
067200961010     C     *IN31         DOWEQ     '0'                                          Read EDTAB
067300961010     C     TABFLG        IFEQ      ' '                                          flg annull
067400961010     C                   MOVEL     TABUNI        DSPT
067500020726      * deve reperire l'UNB per la trasmissione
067600961029     C     §PTKSC        IFEQ      WKSC                                         cod.cli.
067700980401     C     D87LNP        ANDEQ     0
067800020726     C     §PTKSC        oreq      CodPt7                                       cod.cli.
067900020726     C     D87LNP        andeq     0
068000980401     C     §PTLNP        OREQ      D87LNP                                       LNP
068100980401     C     §PTLNP        ANDNE     0
068200961010     C                   MOVEL     TABKEY        KKEY
068300961010     C     §PTNF1        IFEQ      'S'                                          n.f.v.
068400961010     C                   MOVEL     'AFB'         WTPDOC            3            da
068500961010     C                   END                                                    tornare
068600961010     C     §PTCM1        IFEQ      'S'                                          nr.cmr
068700961010     C                   MOVEL     'CMR'         WTPDOC            3            da
068800961010     C                   END                                                    tornare
068900961010     C                   END                                                    cod.cli
069000961010     C*
069100961010     C                   END                                                    flg.annul
069200961010     C     KCOD          READE     EDTAB01L                               31
069300961010     C                   END                                                    READ EDTAB
069400961010     C*  Carico DATI tabella - PU -
069500961010     C                   Z-ADD     0             X                 4 0
069600961010     C                   MOVEL     'PU'          KCOD
069700961010     C     KTAB          CHAIN     EDTAB01L                           31
069800961010     C     *IN31         IFEQ      '0'                                          EDTAB OK
069900961010     C     TABFLG        ANDEQ     ' '
070000961010     C                   MOVEL     TABUNI        DSPU
070100961010     C                   END                                                    EDTAB OK
070200961010     C*
070300961009     C                   ENDSR
070400140911      *==============================================================
070500140911      * Le routine seguenti risalgono al 1996 1998
070600140911      *    si è deciso di accorparle in routine in quanto tutte uguali
070700140911      *==============================================================
070800140911     C*--------------------------------------------------------------
070900140911     C*  aggancia la BOLLA sui BLP
071000140911     C*--------------------------------------------------------------
071100140911     C     chk_BollaPart BEGSR
071200140911     C*
071300140911      **  imposta lo STS stato
071400140911     C                   Exsr      Imposta_STS
071500140911     C*
071600140911      * carico da EDRDE nr.cmr/afb
071700140911     C                   Z-ADD     BRVAAS        KAAS
071800140911     C                   Z-ADD     BRVLNP        KLNP
071900140911     C                   Z-ADD     BRVNRS        KNRS
072000140911     C                   Z-ADD     BRVNSP        KNSP
072100140911     C                   MOVEL     'TD1154'      KNMC
072200140911     C                   Z-ADD     1             KNRP
072300140911     C                   MOVEL     *BLANKS       RDEVAL
072400140911     C     KRDE          CHAIN     EDRDE01L                           32
072500140911     C*
072600140911     C                   MOVEL     RDEVAL        STACMR
072700140911     C     STACMR        IFEQ      *BLANKS
072800140911     C                   MOVEL     '0'           STACMR
072900140911     C                   END
073000140911      **
073100140911      **  imposta il CNI
073200140911     C                   MOVEL     *BLANKS       ar4NOT
073300140911     C                   MOVEL     '0'           ar4NOT
073400140911     C                   MOVEL     'E'           KTRC
073500140911     C     Kar4          CHAIN     fiar401L                           34
073600140911     C                   MOVEL     ar4NOT        STACNI
073700140911     C*
073800140911      ** ---------------
073900140911     C*
074000140911      * Mi posiziono sulla bolla x reperire la data ritiro
074100140911     C     KBLP          CHAIN     FNBLP01L                           32
074200140911      **
074300140911     C     *IN32         IFEQ      '0'
074400140911      **
074500140911      * Se non è stato impostato il ar4 scrivo rife.da BLPRMA/RMN
074600140911     C     STACNI        IFEQ      '0'
074700140911     C     BLPRMA        IFNE      *BLANKS
074800140911     C                   MOVEL     BLPRMA        STACNI
074900140911     C                   ELSE
075000140911     C                   MOVEL     BLPRMN        STACNI
075100140911     C                   ENDIF
075200140911     C                   ENDIF
075300140911     C*
075400140911     C* Non viene + inviata la data di ritiro ma la data del foglio
075500140911     C*  o la data spunta entrata
075600140911     C*******            Z-ADD     BLPDRT        STADTR
075700140911     C*
075800140911     C                   ELSE
075900140911      * Se non la trovo imposto la data - ora spunta
076000140911     C                   Z-ADD     BRVDCS        STADTR
076100140911     C                   Z-ADD     BRVHCS        STAHMS
076200140911     C                   END
076300140911      **
076400140911      **   Portato fuori dall'IF perchè l'orario NON veniva mai attribuito
076500140911      **    sui segmenti di data. Così l'orario della spunta viene impostato
076600140911      **   Nel programma di costruzione dell'EDI TRTCT88R deve essere testato
076700140911      **    che la DATA+ORA (spunta) non siano posteriori alla DATA ORA della
076800140911      **   chiusura del Foglio, in tal caso si dovrà impostare DATA ORA del
076900140911      **   Foglio stesso.
077000140911      **     Al momento solo per quelli che vogliono la data completa con
077100140911      **   l'orario su tab.PT
077200140911      *
077300140911     c                   if        §pudate203 ='S' and STAhms = 0
077400140911     C                   Z-ADD     BRVHCS        STAHMS
077500140911     c                   end
077600140911      ** ---------------
077700140911      **
077800140911      **  Imposta la Data del foglio Entrata :
077900140911     C                   Z-ADD     D87DFE        STADTR
078000140911      **  oppure
078100140911      **  se c'è la data di spunta entrata su dettaglio Segnacollo
078200140911      **   prende questa.
078300140911     C                   z-add     brvnsc        knsc
078400140911     C     kblt          CHAIN     fnblt01l
078500140911     c                   if        %Found(fnblt01l) and bltDSE >0
078600140911     C                   z-add     bltDSE        STADTR
078700140911     c                   end
078800140911      *-
078900140911     C                   ENDSR
079000140911     C*--------------------------------------------------------------
079100140911      *-  carica i campi dello STATUS STS
079200140911     C*--------------------------------------------------------------
079300140911     C     Imposta_STS   BEGSR
079400140911      *-
079500140911     C     §PULAB        IFEQ      'S'
079600140911     C                   MOVEL     §EASTA        STASTS
079700140911     C                   MOVEL     §EACOD        STAERR
079800140911     C                   ELSE
079900140911     C                   MOVEL     §EASTV        STASTS
080000140911     C                   MOVEL     §EACDV        STAERR
080100140911     C                   END
080200140911      *-
080300140911     C                   ENDSR
080400140911     C*--------------------------------------------------------------
