000100050927     H DECEDIT('0,') DATEDIT(*YMD.)
000200961009     H*---------------------------------------------------------------------*
000300961009     H*  Ritorno date arrivo ai partner  - E.D.I. - Estero -                *
000400961009     F*---------------------------------------------------------------------*
000500961009     F*  DATA BASE                                                          *
000600961009     F*---------------------------------------------------------------------*
000700961009     FEDRDE01L  IF   E           K DISK
000800961029     F*---------
000900020221     F****TABEL00FIF  E           K        DISK
001000961009     F*---------
001100961010     FEDTAB01L  IF   E           K DISK
001200961010     F*---------
001300961010     FFNBLP01L  IF   E           K DISK
001400961010     F*---------
001500050926     Ffnblt01l  IF   E           K DISK
001600050926     F*---------
001700060213     Ffiar401L  IF   E           K DISK
001800980108     F*---------
001900980108     FEDSUM04L  UF   E           K DISK
002000980108     F                                     RENAME(EDSUM000:EDSUMLNP)
002100961009     F*---------
002200961009     FEDSUM01L  UF   E           K DISK
002300961009     F*---------
002400961025     FEDBRV02L  UF   E           K DISK
002500961009     F*---------
002600980108     FEDBRV03L  UF   E           K DISK
002700980108     F                                     RENAME(EDBRV000:EDBRVLNP)
002800980108     F*---------
002900961010     FEDSTAWRK  UF A E           K DISK
003000980331     F*---------
003100980401     FTRTC87D6  IF   E             WORKSTN USROPN
003200980331     F                                     SFILE(TC87S01:NRR1)
003300961010     D*---------------------------------------------------------------------*
003400961010     D*  SCHIERE
003500961010     D*---------------------------------------------------------------------*
003600961010     D* Tabella errori date arrivo
003700961010     D CEA             S              3    DIM(100)                             codici errori
003800961010     D DEA             S             90    DIM(100)                             dati tab.EA
003900961009     D*---------------------------------------------------------------------*
004000961009     D* DS
004100961009     D*---------------------------------------------------------------------*
004200961009     D KPJBA         E DS
004300961009     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004400961009     D CNCR80        E DS
004500961009     D TRTC87        E DS                  EXTNAME(TRTC87DS)
004600020726     D DSCL          E DS                  EXTNAME(EDIDSCL)
004700961010     D DSPT          E DS                  EXTNAME(EDIDSPT)
004800961010     D DSPU          E DS                  EXTNAME(EDIDSPU)
004900961010     D DSEA          E DS                  EXTNAME(EDIDSEA)
005000961009     C*---------------------------------------------------------------------*
005100961009     C* Ciclo principale
005200961009     C*---------------------------------------------------------------------*
005300961009      *  Carico da EDSUM tutte le bolle a cui ho attribuito il
005400961009      *  foglio entrata e cancello record
005500980108     C     D87LNP        IFEQ      0
005600961009     C                   Z-ADD     D87CCM        KCCM
005700961009     C                   Z-ADD     D87NFE        KNFE
005800961009     C     KSUM          CHAIN     EDSUM01L                           31
005900980108     C                   ELSE
006000980108     C                   Z-ADD     D87LNP        KLNP
006100980108     C                   Z-ADD     D87NFE        KNFE
006200980108     C     KSUM4         CHAIN     EDSUM04L                           31
006300980108     C                   END
006400961009     C     *IN31         DOWEQ     '0'
006500961010     C                   EXSR      WRTSUM
006600961009     C                   END
006700980327      *---------------------------------------------------*
006800980327      * Gestione scrittura errori da EDBRV
006900980327     C                   EXSR      WRTERR
007000980327      *---------------------------------------------------*
007100980327      * Gestione scrittura dati corretti
007200980327     C                   EXSR      WRTDAT
007300980327      *---------------------------------------------------*
007400980331      * Aggiorno dati eccedenze
007500980331     C     D87ECC        IFEQ      'S'
007600980331     C     D87WRT        ANDEQ     'S'
007700980331     C                   OPEN      TRTC87D6
007800980331     C                   EXSR      WRTECC
007900980331     C                   END
008000980331      *---------------------------------------------------*
008100961009      * Fine
008200961010     C                   MOVEL     KKEY          D87CPT
008300961010     C                   MOVEL     TRTC87        KPJBU
008400961009     C                   SETON                                        LR
008500980327      *------------------------------------------------------------*
008600980327      * Carico errori in file di Work
008700980327      *------------------------------------------------------------*
008800980327     C     WRTERR        BEGSR
008900980327      *---------------------------------------------------*
009000980327      *  Carico dati da bolle errori non annullate da BRV
009100980327      *  mi posiziono con flag annullamento abblencato
009200980327      *  e errori a si x scrivere solo errori rimasti
009300980327     C                   MOVEL     *BLANKS       KFLG
009400980327     C                   MOVEL     *BLANKS       KSTS
009500980327     C     D87LNP        IFEQ      0
009600980401     C                   Z-ADD     KCCM          KKEY1
009700980330     C     KBRV3         CHAIN     EDBRV02L                           31
009800980327     C                   ELSE
009900980401     C                   Z-ADD     KLNP          KKEY1
010000980330     C     KBRV6         CHAIN     EDBRV03L                           31
010100980327     C                   END
010200980327      *---------------------------------------------------*
010300980327      * Loop letture
010400980327     C     *IN31         DOWEQ     '0'
010500980402     C     BRVFLG        ANDEQ     ' '
010600980331     C                   MOVEL     BRVSTS        KSTS
010700980331     C                   MOVEL     BRVCAN        KCAN
010800980327      * Aggancio dati stato della spunta
010900980327     C                   Z-ADD     1             X
011000980327     C                   CLEAR                   DSEA
011100980328     C     BRVFLG        IFEQ      *BLANKS
011200980327     C     BRVSTS        LOOKUP    CEA(X)                                 32
011300980328     C                   ELSE
011400980328     C                   MOVEL     *BLANKS       WSTS              3
011500980402     C                   MOVEL     *BLANKS       BRVCAN
011600980328     C     WSTS          LOOKUP    CEA(X)                                 32
011700980328     C                   END
011800980328     C   32              MOVEL     DEA(X)        DSEA
011900980327      * Se lo stato è di errore ed è un errore da non ignorare
012000980327      * e non c'è nessuna anomalia di spunta .....
012100980327     C     §EATPE        IFNE      *BLANKS
012200980327     C     §EASTS        ANDEQ     *BLANKS
012300980327     C     BRVCAN        ANDEQ     *BLANKS
012400980327      * ... oppure se lo stato è di errore ed è un errore da
012500980327      * non ignorare, c'è una anomalia di spunta ma l'errore
012600980327      * deve prevalere scrivo l'evento errato
012700980327     C     §EATPE        ORNE      *BLANKS
012800980327     C     §EASTS        ANDEQ     *BLANKS
012900980402     C     §EACAN        ANDEQ     'S'
013000980327     C     BRVCAN        ANDNE     *BLANKS
013100980330     C                   EXSR      WRTBRE
013200980327      * ... altrimenti se c'è anomalia di spunta scrivo quella
013300980327     C                   ELSE
013400980327     C     BRVCAN        IFNE      *BLANKS
013500980327     C                   EXSR      WRTANO
013600980327     C                   ELSE
013700980327      * ... se non c'è anomalia di spunta mi posiziono su
013800980327      * codice stato/anomalia successivi
013900980327     C     D87LNP        IFEQ      0
014000980330     C     KBRV7         SETGT     EDBRV02L
014100980330     C     KBRV3         READE     EDBRV02L                               31
014200980327     C                   ELSE
014300980330     C     KBRV4         SETGT     EDBRV03L
014400980330     C     KBRV6         READE     EDBRV03L                               31
014500980327     C                   END
014600980327      *
014700980327     C                   END                                                    Scrivo anom.spunta
014800980327      *
014900980327     C                   END                                                    Scrivo errore
015000980327      *
015100980327     C                   END                                                    Loop EDBRV
015200980327      *---------------------------------------------------*
015300980327     C                   ENDSR
015400980327      *------------------------------------------------------------*
015500980327      * Carico dati corretti
015600980327      *------------------------------------------------------------*
015700980327     C     WRTDAT        BEGSR
015800980327      *
015900980327      *  Carico dati Bolle senza errori
016000980327     C     D87LNP        IFEQ      0
016100980327     C     KBRV3         CHAIN     EDBRV02L                           31
016200980327     C                   ELSE
016300980327     C     KBRV6         CHAIN     EDBRV03L                           31
016400980327     C                   END
016500980331      *
016600980331     C                   Z-ADD     1             X
016700980331     C                   CLEAR                   DSEA
016800980331     C     D87RIT        IFEQ      *BLANKS
016900980331     C     '   '         LOOKUP    CEA(X)                                 32
017000980331     C                   ELSE
017100980331     C     'RIT'         LOOKUP    CEA(X)                                 32
017200980331     C                   END
017300980331     C   32              MOVEL     DEA(X)        DSEA
017400980331      *
017500980327     C     *IN31         DOWEQ     '0'
017600980327      *  Ritorno i dati della bolla solo se devo tornare le
017700980327      *  bolle positive: Scrivo la bolla solo se non esiste
017800980327      * -> se segnacolli sempre scrivo la bolla solo se non
017900980327      *     esiste già segnacollo con errore
018000980327      * -> se segnacolli solo negativi o mai scrivo la bolla
018100980327      *    solo non esiste nessun segnacollo ERRATO x essa
018200980327     C     D87WRT        IFEQ      'S'
018300980327     C     §PUDTB        IFEQ      'E'
018400980327      * carico da EDRDE nr.cmr/afb
018500980401     C     §PTCMR        IFEQ      'S'
018600980327     C                   Z-ADD     BRVAAS        KAAS
018700980327     C                   Z-ADD     BRVLNP        KLNP
018800980327     C                   Z-ADD     BRVNRS        KNRS
018900980327     C                   Z-ADD     BRVNSP        KNSP
019000980327     C                   MOVEL     'TD1154'      KNMC
019100980327     C                   Z-ADD     1             KNRP
019200980327     C                   MOVEL     *BLANKS       RDEVAL
019300980327     C     KRDE          CHAIN     EDRDE01L                           32
019400980401     C                   MOVEL     RDEVAL        KCMR
019500980401     C                   ELSE
019600980401     C                   MOVEL     *BLANKS       KCMR
019700980401     C                   MOVEL     '0'           KCMR
019800980401     C                   END
019900980327     C                   Z-ADD     BRVAAS        KAAS
020000980327     C                   Z-ADD     BRVLNP        KLNP
020100980327     C                   Z-ADD     BRVNRS        KNRS
020200980327     C                   Z-ADD     BRVNSP        KNSP
020300980327     C                   MOVE      *HIVAL        STANSC
020400980327     C     KSTA          CHAIN     EDSTAWRK                           32
020500980327     C     §PUSGD        IFNE      'S'
020600980327     C     *IN32         IFEQ      '1'
020700980327     C     STAERR        OREQ      '309'
020800980327     C     STAERR        OREQ      '   '
020900980327     C                   EXSR      WRTBOK
021000980327     C                   END
021100980327     C                   ELSE
021200980327     C     *IN32         DOWEQ     '0'
021300980327     C     STANSC        ANDNE     BRVNSC
021400980327     C     KSTA          READE     EDSTAWRK                               32
021500980327     C                   END
021600980327     C     STANSC        IFNE      BRVNSC
021700980327     C                   EXSR      WRTBOK
021800980327     C                   END
021900980327     C                   END
022000980327     C*  Se devo tornare solo le bolle negative con tutti i
022100980327     C*  scrivo la bolla solo se esiste già il record in
022200980327     C*  EDSTA --> errori  - con quel nr.CMR
022300980327     C                   ELSE
022400980327      * carico da EDRDE nr.cmr/afb
022500980327     C                   Z-ADD     BRVAAS        KAAS
022600980327     C                   Z-ADD     BRVLNP        KLNP
022700980327     C                   Z-ADD     BRVNRS        KNRS
022800980327     C                   Z-ADD     BRVNSP        KNSP
022900980327     C                   MOVEL     'TD1154'      KNMC
023000980327     C                   Z-ADD     1             KNRP
023100980327     C                   MOVEL     *BLANKS       RDEVAL
023200980327     C     KRDE          CHAIN     EDRDE01L                           32
023300980327     C*
023400980327     C                   Z-ADD     BRVAAS        KAAS
023500980327     C                   Z-ADD     BRVLNP        KLNP
023600980327     C                   Z-ADD     BRVNRS        KNRS
023700980327     C                   Z-ADD     BRVNSP        KNSP
023800980327     C                   MOVEL     RDEVAL        KCMR
023900980327     C     KSTA          CHAIN     EDSTAWRK                           32
024000980327     C     *IN32         IFEQ      '0'
024100980327     C     §PUSGD        ANDEQ     'S'
024200980327     C     *IN32         DOWEQ     '0'
024300980327     C     STANSC        ANDNE     BRVNSC
024400980327     C     KSTA          READE     EDSTAWRK                               32
024500980327     C                   END
024600980327     C     STANSC        IFNE      BRVNSC
024700980327     C                   EXSR      WRTBOK
024800980327     C                   END
024900980327     C                   END
025000980327     C*
025100980327     C                   END
025200980327     C                   END
025300980327      * cancello record
025400980327      * prossima lettura
025500980327     C     D87LNP        IFEQ      0
025600980327     C                   DELETE    EDBRV000
025700980327     C     KBRV3         READE     EDBRV02L                               31
025800980327     C                   ELSE
025900980327     C                   DELETE    EDBRVLNP
026000980327     C     KBRV6         READE     EDBRV03L                               31
026100980327     C                   END
026200980327     C                   END
026300980330     C*
026400980330     C                   ENDSR
026500961010     C*------------------------------------------------------------*
026600961010     C*  WRTSUM: Scrivo dati da EDSUM
026700961010     C*------------------------------------------------------------*
026800961010     C     WRTSUM        BEGSR
026900961010     C*
027000961011     C     SUMFLG        IFEQ      *BLANKS
027100961010     C                   CLEAR                   EDSTA000
027200961010     C                   Z-ADD     SUMAAS        STAAAS
027300961010     C                   Z-ADD     SUMLNP        STALNP
027400970626     C                   Z-ADD     SUMFLS        STAFLS
027500961010     C                   Z-ADD     SUMLNA        STALNA
027600961010     C                   Z-ADD     SUMZNC        STAZNC
027700961010     C                   Z-ADD     SUMNRS        STANRS
027800961010     C                   Z-ADD     SUMNSP        STANSP
027900961015     C     STANSP        IFEQ      0
028000961015     C                   Z-ADD     SUMNSC        STANSP
028100961015     C                   END
028200961015     C                   Z-ADD     SUMNSC        STANSC
028300961010     C                   Z-ADD     SUMCCM        STACCM
028400961010     C                   MOVEL     WTPDOC        STATPD
028500961010     C                   MOVEL     SUMCMR        STACMR
028600961010     C                   MOVEL     D87NFE        STANFE
028700961010     C                   MOVEL     D87DFE        STADFE
028800961010     C                   MOVEL     SUMCNI        STACNI
028900961010      * Mi posiziono sulla bolla x reperire la data ritiro
029000961010     C                   Z-ADD     SUMAAS        KAAS
029100961010     C                   Z-ADD     SUMLNP        KLNP
029200961010     C                   Z-ADD     SUMNRS        KNRS
029300961010     C                   Z-ADD     SUMNSP        KNSP
029400050926      **
029500050928      **   Se non trova spunta entrata imposta la data del foglio
029600050928      **    e non più la data ritiro
029700050928     C**** KBLP          CHAIN     FNBLP01L                           32
029800050928     C**** *IN32         IFEQ      '0'
029900050928     C*****              Z-ADD     BLPDRT        STADTR
030000050928     C*****              ELSE
030100961010      * Se non la trovo imposto la data del foglio entrata
030200050928      *      Adesso imposta sempre la data foglio : dal 28/9/2005
030300961015     C                   Z-ADD     D87DFE        STADTR
030400050928     C********           END
030500050926      **
030600050929      **  Ma se c'è la data di spunta entrata su dettaglio Segnacollo
030700050929      **   prende questa.
030800050929     C                   z-add     sumnsc        knsc
030900050929     C     kblt          CHAIN     fnblt01l
031000050929     c                   if        %Found(fnblt01l) and bltDSE >0
031100050929     C                   z-add     bltDSE        STADTR
031200050926     c                   endIf
031300050926      **
031400961010      * CERCO ERRORE
031500961015     C                   Z-ADD     1             X
031600961010     C     SUMSTS        LOOKUP    CEA(X)                                 32
031700961010     C     *IN32         IFEQ      '1'
031800961010     C                   MOVEL     DEA(X)        DSEA
031900980330     C     §PULAB        IFEQ      'S'
032000961010     C                   MOVEL     §EASTA        STASTS
032100961010     C                   MOVEL     §EACOD        STAERR
032200980330     C                   ELSE
032300980330     C                   MOVEL     §EASTV        STASTS
032400980330     C                   MOVEL     §EACDV        STAERR
032500961015     C                   END
032600980330     C                   END
032700961010      * scrivo EDSTA
032800980401     C                   Z-ADD     KKEY1         STAKEY
032900980402     C                   MOVEL     'S'           STAFLE
033000961010     C                   WRITE     EDSTA000
033100961011     C                   END
033200980108     C     D87LNP        IFEQ      0
033300980108      * cancello record
033400980108     C                   DELETE    EDSUM000
033500980108      * prossima lettura
033600980108     C     KSUM          READE     EDSUM01L                               31
033700980108     C                   ELSE
033800980108      * cancello record
033900980108     C                   DELETE    EDSUMLNP
034000980108      * prossima lettura
034100980108     C     KSUM4         READE     EDSUM04L                               31
034200980108     C                   END
034300961010     C*
034400961010     C                   ENDSR
034500961010     C*------------------------------------------------------------*
034600980330     C*  WRTBRE: Scrivo ERRORI DA EDBRV
034700961010     C*------------------------------------------------------------*
034800980330     C     WRTBRE        BEGSR
034900980331     C*
035000980331     C     *IN31         DOWEQ     '0'
035100980402     C     BRVSTS        ANDEQ     KSTS
035200980402     C     BRVCAN        ANDEQ     KCAN
035300980402     C     BRVFLG        ANDEQ     *BLANKS
035400961024     C     D87WRT        IFEQ      'S'
035500961010     C                   CLEAR                   EDSTA000
035600980401     C                   Z-ADD     KKEY1         STAKEY
035700961010     C                   MOVEL     D87NFE        STANFE
035800961010     C                   MOVEL     D87DFE        STADFE
035900961010     C                   Z-ADD     BRVAAS        STAAAS
036000961010     C                   Z-ADD     BRVLNP        STALNP
036100970626     C                   Z-ADD     BRVFLS        STAFLS
036200961010     C                   Z-ADD     BRVLNA        STALNA
036300961010     C                   Z-ADD     BRVZNC        STAZNC
036400961010     C                   Z-ADD     BRVNRS        STANRS
036500961010     C                   Z-ADD     BRVNSP        STANSP
036600961015     C     STANSP        IFEQ      0
036700961015     C                   Z-ADD     BRVNSC        STANSP
036800961015     C                   END
036900961010     C                   Z-ADD     BRVNSC        STANSC
037000961010     C                   Z-ADD     BRVCCM        STACCM
037100961010     C                   MOVEL     WTPDOC        STATPD
037200980330     C     §PULAB        IFEQ      'S'
037300961010     C                   MOVEL     §EASTA        STASTS
037400961010     C                   MOVEL     §EACOD        STAERR
037500980330     C                   ELSE
037600980330     C                   MOVEL     §EASTV        STASTS
037700980330     C                   MOVEL     §EACDV        STAERR
037800980330     C                   END
037900961010      * carico da EDRDE nr.cmr/afb
038000961010     C                   Z-ADD     BRVAAS        KAAS
038100961010     C                   Z-ADD     BRVLNP        KLNP
038200961010     C                   Z-ADD     BRVNRS        KNRS
038300961010     C                   Z-ADD     BRVNSP        KNSP
038400961010     C                   MOVEL     'TD1154'      KNMC
038500961010     C                   Z-ADD     1             KNRP
038600961010     C                   MOVEL     *BLANKS       RDEVAL
038700961010     C     KRDE          CHAIN     EDRDE01L                           32
038800961010     C                   MOVEL     RDEVAL        STACMR
038900961016     C     STACMR        IFEQ      *BLANKS
039000961016     C                   MOVEL     '0'           STACMR
039100961016     C                   END
039200060213     C                   MOVEL     *BLANKS       ar4NOT
039300060213     C                   MOVEL     '0'           ar4NOT
039400961015     C                   MOVEL     'E'           KTRC
039500060213     C     Kar4          CHAIN     Fiar401L                           34
039600060213     C                   MOVEL     ar4NOT        STACNI
039700050926      **
039800961010      * Mi posiziono sulla bolla x reperire la data ritiro
039900961010     C     KBLP          CHAIN     FNBLP01L                           32
040000980424     C     *IN32         IFEQ      '0'
040100060213      * Se non è stato impostato il ar4 scrivo rife.da BLPRMA/RMN
040200980424     C     STACNI        IFEQ      '0'
040300980424     C     BLPRMA        IFNE      *BLANKS
040400980424     C                   MOVEL     BLPRMA        STACNI
040500980424     C                   ELSE
040600980424     C                   MOVEL     BLPRMN        STACNI
040700980424     C                   ENDIF
040800980424     C                   ENDIF
040900980424     C*
041000050929     C* Non viene + inviata la data di ritiro ma la data del foglio
041100050930     C*  o la data spunta entrata
041200050929     C*******            Z-ADD     BLPDRT        STADTR
041300050929     C*
041400961010     C                   ELSE
041500961010      * Se non la trovo imposto la data - ora spunta
041600961010     C                   Z-ADD     BRVDCS        STADTR
041700961010     C                   Z-ADD     BRVHCS        STAHMS
041800961010     C                   END
041900050926      **
042000050929      **  Imposta la Data del foglio Entrata :
042100050929     C                   Z-ADD     D87DFE        STADTR
042200050930      **  oppure
042300050929      **  se c'è la data di spunta entrata su dettaglio Segnacollo
042400050929      **   prende questa.
042500050929     C                   z-add     brvnsc        knsc
042600050929     C     kblt          CHAIN     fnblt01l
042700050929     c                   if        %Found(fnblt01l) and bltDSE >0
042800050929     C                   z-add     bltDSE        STADTR
042900050929     c                   end
043000050927      **
043100961010      * scrivo EDSTA
043200970218     C     §EATPE        IFEQ      'E'
043300970218     C     *IN34         OREQ      '0'
043400980402     C                   MOVEL     'S'           STAFLE
043500970218     C                   WRITE     EDSTA000
043600961024     C                   END
043700970218     C                   END
043800980108     C     D87LNP        IFEQ      0
043900980108      * cancello record
044000980108     C                   DELETE    EDBRV000
044100980108      * prossima lettura
044200980331     C     KBRV3         READE     EDBRV02L                               31
044300980108     C                   ELSE
044400980108      * cancello record
044500980108     C                   DELETE    EDBRVLNP
044600980108      * prossima lettura
044700980331     C     KBRV6         READE     EDBRV03L                               31
044800980108     C                   END
044900980331     C                   END
045000961010     C*
045100961010     C                   ENDSR
045200980330     C*------------------------------------------------------------*
045300980330     C*  WRTANO: Scrivo ERRORI DA EDBRV
045400980330     C*------------------------------------------------------------*
045500980330     C     WRTANO        BEGSR
045600980330     C*
045700980331     C     *IN31         DOWEQ     '0'
045800980402     C     BRVSTS        ANDEQ     KSTS
045900980402     C     BRVCAN        ANDEQ     KCAN
046000980402     C     BRVFLG        ANDEQ     *BLANKS
046100980330     C     D87WRT        IFEQ      'S'
046200980330     C                   CLEAR                   EDSTA000
046300980401     C                   Z-ADD     KKEY1         STAKEY
046400980330     C                   MOVEL     D87NFE        STANFE
046500980330     C                   MOVEL     D87DFE        STADFE
046600980330     C                   Z-ADD     BRVAAS        STAAAS
046700980330     C                   Z-ADD     BRVLNP        STALNP
046800980330     C                   Z-ADD     BRVFLS        STAFLS
046900980330     C                   Z-ADD     BRVLNA        STALNA
047000980330     C                   Z-ADD     BRVZNC        STAZNC
047100980330     C                   Z-ADD     BRVNRS        STANRS
047200980330     C                   Z-ADD     BRVNSP        STANSP
047300980330     C     STANSP        IFEQ      0
047400980330     C                   Z-ADD     BRVNSC        STANSP
047500980330     C                   END
047600980330     C                   Z-ADD     BRVNSC        STANSC
047700980330     C                   Z-ADD     BRVCCM        STACCM
047800980330     C                   MOVEL     WTPDOC        STATPD
047900980330     C                   Z-ADD     1             X
048000980330     C     'A  '         LOOKUP    CEA(X)                                 32
048100980330     C   32              MOVEL     DEA(X)        DSEA
048200980330     C     §PULAB        IFEQ      'S'
048300980330     C                   MOVEL     §EASTA        STASTS
048400980330     C                   MOVEL     §EACOD        STAERR
048500980330     C                   ELSE
048600980330     C                   MOVEL     §EASTV        STASTS
048700980330     C                   MOVEL     §EACDV        STAERR
048800980330     C                   END
048900980330      * carico da EDRDE nr.cmr/afb
049000980330     C                   Z-ADD     BRVAAS        KAAS
049100980330     C                   Z-ADD     BRVLNP        KLNP
049200980330     C                   Z-ADD     BRVNRS        KNRS
049300980330     C                   Z-ADD     BRVNSP        KNSP
049400980330     C                   MOVEL     'TD1154'      KNMC
049500980330     C                   Z-ADD     1             KNRP
049600980330     C                   MOVEL     *BLANKS       RDEVAL
049700980330     C     KRDE          CHAIN     EDRDE01L                           32
049800980330     C                   MOVEL     RDEVAL        STACMR
049900980330     C     STACMR        IFEQ      *BLANKS
050000980330     C                   MOVEL     '0'           STACMR
050100980330     C                   END
050200060213     C                   MOVEL     *BLANKS       ar4NOT
050300060213     C                   MOVEL     '0'           ar4NOT
050400980330     C                   MOVEL     'E'           KTRC
050500060213     C     Kar4          CHAIN     Fiar401L                           34
050600060213     C                   MOVEL     ar4NOT        STACNI
050700050926      **
050800980330      * Mi posiziono sulla bolla x reperire la data ritiro
050900980330     C     KBLP          CHAIN     FNBLP01L                           32
051000980330     C     *IN32         IFEQ      '0'
051100050928     C**** La data Ritiro è stata sostituita con la data Spunta Entrata o quella
051200050928     C**** del foglio di quadratura.
051300050928     C*******            Z-ADD     BLPDRT        STADTR
051400060213      * Se non è stato impostato il ar4 scrivo rife.da BLPRMA/RMN
051500980424     C     STACNI        IFEQ      '0'
051600980424     C     BLPRMA        IFNE      *BLANKS
051700980424     C                   MOVEL     BLPRMA        STACNI
051800980424     C                   ELSE
051900980424     C                   MOVEL     BLPRMN        STACNI
052000980424     C                   ENDIF
052100980424     C                   ENDIF
052200980330     C                   ELSE
052300980330      * Se non la trovo imposto la data - ora spunta
052400980330     C                   Z-ADD     BRVDCS        STADTR
052500980330     C                   Z-ADD     BRVHCS        STAHMS
052600980330     C                   END
052700050926      **
052800050929      **   Imposta  la data del foglio .....
052900050929      **   ma se c'è la data di spunta entrata su dettaglio Segnacollo
053000050926      **   prende questa.
053100050929     C                   z-add     D87DFE        STADTR
053200050926     C                   z-add     brvnsc        knsc
053300050926     C     kblt          CHAIN     fnblt01l
053400050926     c                   if        %Found(fnblt01l) and bltDSE >0
053500050926     C                   z-add     bltDSE        STADTR
053600050928     c                   endIF
053700050926      **
053800980330      * scrivo EDSTA
053900980330     C     §EATPE        IFEQ      'E'
054000980330     C     *IN34         OREQ      '0'
054100980402     C                   MOVEL     'S'           STAFLE
054200980330     C                   WRITE     EDSTA000
054300980330     C                   END
054400980330     C                   END
054500980330     C     D87LNP        IFEQ      0
054600980330      * cancello record
054700980330     C                   DELETE    EDBRV000
054800980330      * prossima lettura
054900980331     C     KBRV3         READE     EDBRV02L                               31
055000980330     C                   ELSE
055100980330      * cancello record
055200980330     C                   DELETE    EDBRVLNP
055300980330      * prossima lettura
055400980331     C     KBRV6         READE     EDBRV03L                               31
055500980330     C                   END
055600980331     C                   END
055700980330     C*
055800980330     C                   ENDSR
055900961010     C*------------------------------------------------------------*
056000961010     C*  WRTBOK: Scrivo dati da EDBRV x no errori
056100961010     C*------------------------------------------------------------*
056200961010     C     WRTBOK        BEGSR
056300961010     C*
056400961010     C                   CLEAR                   EDSTA000
056500980401     C                   Z-ADD     KKEY1         STAKEY
056600961010     C                   Z-ADD     BRVAAS        STAAAS
056700961010     C                   Z-ADD     BRVLNP        STALNP
056800970626     C                   Z-ADD     BRVFLS        STAFLS
056900961010     C                   Z-ADD     BRVLNA        STALNA
057000961010     C                   Z-ADD     BRVZNC        STAZNC
057100961010     C                   Z-ADD     BRVNRS        STANRS
057200961010     C                   Z-ADD     BRVNSP        STANSP
057300961015     C     STANSP        IFEQ      0
057400961015     C                   Z-ADD     BRVNSC        STANSP
057500961015     C                   END
057600961010     C                   Z-ADD     BRVNSC        STANSC
057700961010     C                   Z-ADD     BRVCCM        STACCM
057800961010     C                   MOVEL     D87NFE        STANFE
057900961010     C                   MOVEL     D87DFE        STADFE
058000961030     C                   MOVEL     WTPDOC        STATPD
058100980330     C     §PULAB        IFEQ      'S'
058200961015     C                   MOVEL     §EASTA        STASTS
058300961015     C                   MOVEL     §EACOD        STAERR
058400980330     C                   ELSE
058500980330     C                   MOVEL     §EASTV        STASTS
058600980330     C                   MOVEL     §EACDV        STAERR
058700980330     C                   END
058800980424      * carico da EDRDE nr.cmr/afb
058900980424     C                   Z-ADD     BRVAAS        KAAS
059000980424     C                   Z-ADD     BRVLNP        KLNP
059100980424     C                   Z-ADD     BRVNRS        KNRS
059200980424     C                   Z-ADD     BRVNSP        KNSP
059300980424     C                   MOVEL     'TD1154'      KNMC
059400980424     C                   Z-ADD     1             KNRP
059500980424     C                   MOVEL     *BLANKS       RDEVAL
059600980424     C     KRDE          CHAIN     EDRDE01L                           32
059700961010     C                   MOVEL     RDEVAL        STACMR
059800961016     C     STACMR        IFEQ      *BLANKS
059900961016     C                   MOVEL     '0'           STACMR
060000961016     C                   END
060100060213     C                   MOVEL     *BLANKS       ar4NOT
060200060213     C                   MOVEL     '0'           ar4NOT
060300961015     C                   MOVEL     'E'           KTRC
060400060213     C     Kar4          CHAIN     fiar401L                           32
060500060213     C                   MOVEL     ar4NOT        STACNI
060600050928
060700961010      * Mi posiziono sulla bolla x reperire la data ritiro
060800961010     C     KBLP          CHAIN     FNBLP01L                           32
060900961010     C     *IN32         IFEQ      '0'
061000060213      * Se non è stato impostato il ar4 scrivo rife.da BLPRMA/RMN
061100980424     C     STACNI        IFEQ      '0'
061200980424     C     BLPRMA        IFNE      *BLANKS
061300980424     C                   MOVEL     BLPRMA        STACNI
061400980424     C                   ELSE
061500980424     C                   MOVEL     BLPRMN        STACNI
061600980424     C                   ENDIF
061700980424     C                   ENDIF
061800050928      * la data ritiro verrà sostituita dalla data spunta entrata
061900050930      * e se non c'è dalla data del foglio 30/9/05
062000050928     C******             Z-ADD     BLPDRT        STADTR
062100961010     C                   ELSE
062200961010      * Se non la trovo imposto la data - ora spunta
062300961010     C                   Z-ADD     BRVDCS        STADTR
062400961010     C                   Z-ADD     BRVHCS        STAHMS
062500961010     C                   END
062600050926      **
062700050929      **  imposta la data del foglio quadratura.....
062800050929      **  ma se c'è la data di spunta entrata su dettaglio Segnacollo
062900050926      **   prende questa.
063000050929     C                   z-add     D87DFE        STADTR
063100050926     C                   z-add     brvnsc        knsc
063200050926     C     kblt          CHAIN     fnblt01l
063300050926     c                   if        %Found(fnblt01l) and bltDSE >0
063400050926     C                   z-add     bltDSE        STADTR
063500050928     c                   end
063600050926      **
063700961010      * scrivo EDSTA
063800980402     C                   MOVEL     'S'           STAFLE
063900980402     C                   MOVEL     *BLANKS       STAFLE
064000961010     C                   WRITE     EDSTA000
064100961010     C*
064200961010     C                   ENDSR
064300980331     C*------------------------------------------------------------*
064400980331     C*  WRTECC: Scrivo eccedenze Euroexpress
064500980331     C*------------------------------------------------------------*
064600980331     C     WRTECC        BEGSR
064700980331     C*
064800980331     C                   SETON                                        2120
064900980331     C                   Z-ADD     1             NRR1              4 0
065000980331     C                   Z-ADD     1             NR1KEY            4 0
065100980401     C***                  WRITETC87Z01
065200980401     C     NRR1          CHAIN     TC87S01                            29
065300980331     C     *IN29         DOWEQ     '0'
065400980331     C     V1CNOT        IFNE      *BLANKS
065500980331     C                   CLEAR                   EDSTA000
065600980401     C                   Z-ADD     KKEY1         STAKEY
065700980331     C                   MOVEL     WTPDOC        STATPD
065800980331     C                   MOVEL     D87NFE        STANFE
065900980331     C                   MOVEL     D87DFE        STADFE
066000980331      * CERCO ERRORE
066100980331     C                   Z-ADD     1             X
066200980331     C     '002'         LOOKUP    CEA(X)                                 32
066300980331     C     *IN32         IFEQ      '1'
066400980331     C                   MOVEL     DEA(X)        DSEA
066500980331     C     §PULAB        IFEQ      'S'
066600980331     C                   MOVEL     §EASTA        STASTS
066700980331     C                   MOVEL     §EACOD        STAERR
066800980331     C                   ELSE
066900980331     C                   MOVEL     §EASTV        STASTS
067000980331     C                   MOVEL     §EACDV        STAERR
067100980331     C                   END
067200980331     C                   END
067300980401     C                   MOVEL     '0'           STACMR
067400980331     C                   MOVEL     'JUT'         STACNI
067500980331     C                   MOVE      V1CNOT        STACNI
067600980331      * scrivo EDSTA
067700980402     C                   MOVEL     'S'           STAFLE
067800980331     C                   WRITE     EDSTA000
067900980331     C                   END
068000980331      *
068100980331     C                   ADD       1             NRR1
068200980331     C     NRR1          CHAIN     TC87S01                            29
068300980331     C                   END
068400980331      *
068500980331     C                   ENDSR
068600961009     C*------------------------------------------------------------*
068700961009     C* OPERAZIONI INIZIALI
068800961009     C*------------------------------------------------------------*
068900961009     C     *INZSR        BEGSR
069000961009     C*
069100961009     C     *ENTRY        PLIST
069200961009     C                   PARM                    KPJBA
069300961009     C                   MOVEL     KPJBU         TRTC87
069400961009     C*  Definizione chiavi
069500961010     C     KBRV1         KLIST
069600961009     C                   KFLD                    KCCM
069700961009     C                   KFLD                    KNFE
069800961009     C                   KFLD                    KFLG
069900961009     C                   KFLD                    KSTS
070000961010     C     KBRV2         KLIST
070100961010     C                   KFLD                    KCCM
070200961010     C                   KFLD                    KNFE
070300961010     C                   KFLD                    KFLG
070400980330     C                   KFLD                    KSTS
070500961010     C     KBRV3         KLIST
070600961010     C                   KFLD                    KCCM
070700961010     C                   KFLD                    KNFE
070800980108     C     KBRV4         KLIST
070900980108     C                   KFLD                    KLNP
071000980108     C                   KFLD                    KNFE
071100980108     C                   KFLD                    KFLG
071200980108     C                   KFLD                    KSTS
071300980330     C                   KFLD                    KCAN
071400980108     C     KBRV5         KLIST
071500980108     C                   KFLD                    KLNP
071600980108     C                   KFLD                    KNFE
071700980108     C                   KFLD                    KFLG
071800980330     C                   KFLD                    KSTS
071900980108     C     KBRV6         KLIST
072000980108     C                   KFLD                    KLNP
072100980108     C                   KFLD                    KNFE
072200980330     C     KBRV7         KLIST
072300980330     C                   KFLD                    KCCM
072400980330     C                   KFLD                    KNFE
072500980330     C                   KFLD                    KFLG
072600980330     C                   KFLD                    KSTS
072700980330     C                   KFLD                    KCAN
072800961009     C     KSUM          KLIST
072900961009     C                   KFLD                    KCCM
073000961009     C                   KFLD                    KNFE
073100980108     C     KSUM4         KLIST
073200980108     C                   KFLD                    KLNP
073300980108     C                   KFLD                    KNFE
073400961009     C     KRDE          KLIST
073500961009     C                   KFLD                    KAAS
073600961009     C                   KFLD                    KLNP
073700961009     C                   KFLD                    KNRS
073800961009     C                   KFLD                    KNSP
073900961009     C                   KFLD                    KNMC
074000961009     C                   KFLD                    KNRP
074100060213     C     Kar4          KLIST
074200961009     C                   KFLD                    KAAS
074300961009     C                   KFLD                    KLNP
074400961009     C                   KFLD                    KNRS
074500961009     C                   KFLD                    KNSP
074600961015     C                   KFLD                    KTRC
074700961010     C     KBLP          KLIST
074800961010     C                   KFLD                    KAAS
074900961010     C                   KFLD                    KLNP
075000961010     C                   KFLD                    KNRS
075100961010     C                   KFLD                    KNSP
075200050926     C     KBLT          KLIST
075300050926     C                   KFLD                    KAAS
075400050926     C                   KFLD                    KLNP
075500050926     C                   KFLD                    KNRS
075600050926     C                   KFLD                    KNSP
075700050926     C                   KFLD                    KNSC
075800961010     C     KTAB          KLIST
075900961029     C                   KFLD                    KCOD
076000961010     C                   KFLD                    KKEY
076100020221     C******     KTAB1     KLIST
076200020221     C******               KFLD           KKUT
076300020221     C******               KFLD           KCOD1
076400961010     C     KSTA          KLIST
076500980401     C                   KFLD                    KKEY1
076600961010     C                   KFLD                    KNFE
076700961010     C                   KFLD                    KCMR
076800961010     C                   KFLD                    KAAS
076900961010     C                   KFLD                    KLNP
077000961010     C                   KFLD                    KNRS
077100961010     C                   KFLD                    KNSP
077200961009     C*  Definizione variabili
077300961009     C     *LIKE         DEFINE    BRVCCM        KCCM
077400961009     C     *LIKE         DEFINE    BRVNFE        KNFE
077500961009     C     *LIKE         DEFINE    BRVFLG        KFLG
077600961009     C     *LIKE         DEFINE    BRVSTS        KSTS
077700980330     C     *LIKE         DEFINE    BRVCAN        KCAN
077800961009     C     *LIKE         DEFINE    RDEAAS        KAAS
077900961009     C     *LIKE         DEFINE    RDELNP        KLNP
078000961009     C     *LIKE         DEFINE    RDENRS        KNRS
078100961009     C     *LIKE         DEFINE    RDENSP        KNSP
078200050926     C     *like         define    bltnsc        knsc
078300060213     C     *LIKE         DEFINE    ar4TRC        KTRC
078400961009     C     *LIKE         DEFINE    RDENMC        KNMC
078500961009     C     *LIKE         DEFINE    RDENRP        KNRP
078600961010     C     *LIKE         DEFINE    TABCOD        KCOD
078700961010     C     *LIKE         DEFINE    TABKEY        KKEY
078800020221     C*****      *LIKE     DEFN TBLKUT    KKUT
078900020221     C*****      *LIKE     DEFN TBLCOD    KCOD1
079000961010     C     *LIKE         DEFINE    STACMR        KCMR
079100980401     C     *LIKE         DEFINE    STAKEY        KKEY1
079200961009     C*
079300961029     C                   MOVEL     D87CCM        WCCM              7
079400020221     C                   MOVE      D87CCM        WKSC              7 0
079500020221     C* Caricamento Tabella 3N
079600020221     C*******              Z-ADD0         X
079700020221     C*******              Z-ADD1         KKUT
079800020221     C*******              MOVEL'3N'      KCOD1
079900020221     C*******    KTAB1     CHAINTABEL00F             30
080000020221     C*******    *IN30     DOWEQ'0'
080100020221     C*******    TBLFLG    IFEQ *BLANKS
080200020221     C*******              ADD  1         X
080300020221     C*******              MOVELTBLUNI    WCOMO7  7
080400020221     C*******    WCCM      IFEQ WCOMO7
080500020221     C*******              MOVELTBLKEY    WKSC    70
080600020221     C*******              END
080700020221     C*******              END
080800020221     C*******    KTAB1     READETABEL00F                 30
080900020221     C*******              END
081000961010     C*  Carico in schiera codici tabella - EA -
081100961010     C                   Z-ADD     0             X                 4 0
081200961010     C                   MOVEL     'EA'          KCOD
081300961010     C                   MOVEA     *BLANKS       DEA
081400961010     C                   MOVEA     *BLANKS       CEA
081500961010     C     KCOD          CHAIN     EDTAB01L                           31
081600961010     C     *IN31         DOWEQ     '0'
081700961010     C     TABFLG        IFEQ      ' '
081800961010     C                   ADD       1             X
081900961010     C                   MOVEL     TABUNI        DEA(X)
082000961010     C                   MOVEL     TABKEY        CEA(X)
082100961010     C                   END
082200961010     C     KCOD          READE     EDTAB01L                               31
082300961010     C                   END
082400020726      *
082500020726     C*  Carico DATI tabella - CL -  se il pgm deve elaborare x cliente
082600020726      *   ed è un codice inviato tramite RFF+FF quindi decodificato sulla
082700020726      *    tabella CL. Se trovato deve memorizzarselo e provare a cercarlo
082800020726      *     con il codice del Partner relativo (sempre dalla CL) per prendere
082900020726      *      poi l'UNB.
083000020726     C                   Z-ADD     0             X
083100020726     C                   z-add     0             CodPt7            7 0
083200020726      * se elaborazione x cliente
083300020726     c                   if        wksc <> 0
083400020726     C                   MOVEL     'CL'          KCOD
083500020726     C     KCOD          CHAIN     EDTAB01L                           31
083600020726     C     *IN31         DOWEQ     '0'                                          Read EDTAB
083700020726     C     TABFLG        IFEQ      ' '                                          flg annull
083800020726     C                   MOVEL     TABUNI        DSCL
083900020726     C     §CLKSC        IFEQ      WKSC                                         cod.cli.
084000020726     C                   MOVEL     TABKEY        CodPt7
084100020726     c                   leave
084200020726     C                   END                                                    cod.cli
084300020726     C                   END                                                    flg.annul
084400020726     C     KCOD          READE     EDTAB01L                               31
084500020726     C                   END                                                    READ EDTAB
084600020726     C                   Endif                                                  READ EDTAB
084700020726      *
084800961010     C*  Carico DATI tabella - PT -
084900961010     C                   Z-ADD     0             X                 4 0
085000961010     C                   MOVEL     'PT'          KCOD
085100961010     C     KCOD          CHAIN     EDTAB01L                           31
085200961010     C     *IN31         DOWEQ     '0'                                          Read EDTAB
085300961010     C     TABFLG        IFEQ      ' '                                          flg annull
085400961010     C                   MOVEL     TABUNI        DSPT
085500020726      * deve reperire l'UNB per la trasmissione
085600961029     C     §PTKSC        IFEQ      WKSC                                         cod.cli.
085700980401     C     D87LNP        ANDEQ     0
085800020726     C     §PTKSC        oreq      CodPt7                                       cod.cli.
085900020726     C     D87LNP        andeq     0
086000980401     C     §PTLNP        OREQ      D87LNP                                       LNP
086100980401     C     §PTLNP        ANDNE     0
086200961010     C                   MOVEL     TABKEY        KKEY
086300961010     C     §PTNF1        IFEQ      'S'                                          n.f.v.
086400961010     C                   MOVEL     'AFB'         WTPDOC            3            da
086500961010     C                   END                                                    tornare
086600961010     C     §PTCM1        IFEQ      'S'                                          nr.cmr
086700961010     C                   MOVEL     'CMR'         WTPDOC            3            da
086800961010     C                   END                                                    tornare
086900961010     C                   END                                                    cod.cli
087000961010     C*
087100961010     C                   END                                                    flg.annul
087200961010     C     KCOD          READE     EDTAB01L                               31
087300961010     C                   END                                                    READ EDTAB
087400961010     C*  Carico DATI tabella - PU -
087500961010     C                   Z-ADD     0             X                 4 0
087600961010     C                   MOVEL     'PU'          KCOD
087700961010     C     KTAB          CHAIN     EDTAB01L                           31
087800961010     C     *IN31         IFEQ      '0'                                          EDTAB OK
087900961010     C     TABFLG        ANDEQ     ' '
088000961010     C                   MOVEL     TABUNI        DSPU
088100961010     C                   END                                                    EDTAB OK
088200961010     C*
088300961009     C                   ENDSR
