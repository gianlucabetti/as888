000100110718     /*END
000200110715     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000300110715     h alwnull(*inputonly)
000400110726      * FNLSA7R *----------------------------------------------------*
000500020909      *             QUADRATURA ENTRATA A MAGAZZINO       BATCH       *
000600020909      *--------------------------------------------------------------*
000700010717     FFNBLP31L  IF   E           K DISK
000800991022     FFNBLP01L  IF   E           K DISK
000900941228     F                                     RENAME(FNBLP000:FNBLP001)
001000941228     FFNBLT01L  IF   E           K DISK
001100941228     FFNBLT27L  IF   E           K DISK
001200941228     F                                     RENAME(FNBLT000:FNBLT027)
001300050930     FFNarT01L  IF   E           K DISK
001400050930     FFNarb01L  IF   E           K DISK
001500150623     FFibsp01l  IF   E           K DISK
001600070123     FFNBRV01L  IF   E           K DISK
001700070123     FFNBRV09L  IF   E           K DISK
001800070123     F                                     RENAME(FNBRV000:FNBRV009)
001900950601     F*
002000950601     FFNFVV01L  UF   E           K DISK
002100020918     FFNFVV02L  IF   E           K DISK
002200020918     F                                     RENAME(FNFVV000:FNFVV002)
002300950601     FFNFGV02L  IF   E           K DISK
002400950601     F                                     RENAME(FNFGV000:FNFGV002)
002500930318     FAZORG01L  IF   E           K DISK
002600930319     FCNACO00F  IF   E           K DISK
002700021203     FFIAPD01L  IF   E           K DISK
002800991022     FFNANM02L  IF   E           K DISK
002900110720     fFIARS01L  if   e           k disk
003000950531     FTABEL00F  IF   E           K DISK
003100130225     Ftntbe01l  IF   E           K DISK
003200961025     FEDBRV00F  IF A E             DISK    USROPN
003300020909     F                                     prefix(EDI:3)
003400121102     F**EDTAB01L  IF   E           K DISK    USROPN
003500110715     fWFQEM00F  o    e             disk    usropn
003600140717     ffnque00F  o    e             disk    usropn
003700940329     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
003800020909
003900110714      *--------------------------------------------------------------*
004000110714      *   C O S T A N T I
004100110714      *--------------------------------------------------------------*
004200080109     d $MaxElem        c                   const(998)
004300020909      *--------------------------------------------------------------*
004400020909      *   S C H I E R E
004500020909      *--------------------------------------------------------------*
004600920804     D L1              S              3  0 DIM(30)
004700961028     D STA             S             66    DIM(18) CTDATA PERRCD(1)
004800980324     D* CARICO I COD PISTOLA CREATI IN AUTOMATICO
004900980402     D NPS             S              2    DIM(99)
005000980402     D NPF             S              2    DIM(99)
005100091013     D* Ccodici trattamento merce autospunta
005200091013     D ctma            S              2    DIM(59)
005300980108     D* Nuove schiere per gestione dati parametri EDI
005400121102     D** ECC             S              1    DIM(12)
005500121102     D** DTB             S              1    DIM(12)
005600121102     D** SGD             S              1    DIM(12)
005700950605     D* METTO LE LNP SELEZIONATE SE CI SONO O TUTTA LA L1
005800950605     D SKI             S              3  0 DIM(30)
005900950605     D* METTO LE LNP SELEZIONATE MA SOLO UNA VOLTA, SENZA RIPETERLE
006000950605     D* O LA L1 SE LNP E' VUOTA
006100950605     D LP2             S              3  0 DIM(30)
006200950605     D*
006300070109     D* LNP + tfp richieste
006400070109     D LP3             S              6  0 DIM(99)
006500070109     D* LNP + 5F  CHE LEGGO DELLE SPUNTE
006600160713     D LP4             S              3  0 DIM(199)
006700950605     D*
006800950629     D*
006900980108     D* CARICO IN SCHIERA CODICI E LNP DELLA TABELLA PT-PU
007000121102     D** EDK             S             35    DIM(99)
007100121102     D** EDU             S             90    DIM(99)
007200130225     d
007300130225     D FAS             S              5  0 DIM($MaxElem)
007400130225     d* fls/nrs che possono partire da più tfp
007500130225     D flsnrs          S              5  0 DIM($MaxElem)
007600130225     d
007700130225     D xcotfp          S              3    DIM(85)
007800150623     d
007900150623     d                 ds
008000150623     d bsplin
008100150623     d sbsp                           3  0 dim(30) overlay(bsplin)
008200020909      *--------------------------------------------------------------*
008300020909      *   S T R U T T U R E   D A T I  MULTIPLE
008400020909      *--------------------------------------------------------------*
008500080109     D DSLAS           DS            87    OCCURS($MaxElem)
008600020909     D  LAS                    1     87
008700020909     D                                     DIM(29)
008800020909      *--------------------------------------------------------------*
008900020909      *   S T R U T T U R E   D A T I  DEFINITE ESTERNAMENTE
009000020909      *--------------------------------------------------------------*
009100020909     D KPJBA         E DS
009200101202      *
009300101202      * - Parametri per richiamo al pgm di controllo profilo utenti
009400101202     d TIBS34ds      e ds
009500101202      * - Ds di riferimento al file esterno AzUte00F
009600101202     d AZUTEds       e ds                  ExtName(AzUte00F)
009700101202      * - Ds per dati organigramma
009800101202     d dDatiUte      e ds
009900101202      *
010000121102     D** EDIDSPU       E DS
010100121102     D** EDIDSPT       E DS
010200020909     D TRTC87DS      E DS
010300091013     D DS1b          E DS
010400020909     D DS7J          E DS
010500070419     D DS7O          E DS
010600180126     D DS7B          E DS
010700111117     D trulnrsds     E DS
010800020909     D* DS PER TRUL06R - CARICAMENTO £X
010900020909     D TRUL06DS      E DS
011000020909     D  LIN                    1     90  0
011100020909     D                                     DIM(30)
011200020909     D* DS PER FNLV53R - CARICAMENTO DATI DI UN FOGLIO
011300020909     D FNLV53DS      E DS
011400070109     D* DS PER FNLV55R - reperimento terminal partenza/arrivo
011500070109     D fnlv55ds      E DS
011600130225     D ktfp            S                   like(blptfp)
011700130225     D kcod            S                   like(tbecod)
011800130225     D kke1            S                   like(tbeke1)
011900130225     d Indx            s              4  0
012000130225     d Wdefl           s              1
012100020909      *--------------------------------------------------------------*
012200020909      *   S T R U T T U R E   D A T I  DEFINITE INTERNAMENTE
012300020909      *--------------------------------------------------------------*
012400941228     D PARAM           DS
012500110714     D  VIDNFV                 1      3P 0
012600110714     d  PrmORD                 4      4
012700110714      * ...libero              5      5
012800941228     D  COMANS                 6      9  0
012900941228     D  COMMGS                10     13  0
013000941228     D  COMDSP                 6     13  0
013100950323     D  LNP                   14     37P 0
013200950323     D                                     DIM(12)
013300950323     D  CPC                   38     85P 0
013400950323     D                                     DIM(12)
013500950323     D  CLI                   86    133P 0
013600950323     D                                     DIM(12)
013700950323     D  DRT                  134    193P 0
013800950605     D                                     DIM(12)
013900060310     D  NRS                  194    217
014000950323     D                                     DIM(12)
014100991022     D  MOP                  218    229
014200950605     D                                     DIM(12)
014300950323     D  NRT                  230    241
014400950605     D                                     DIM(12)
014500961022     D* SOLO PER EDI: FLAG DI CREAZIONE DEL FIL EDBRV00F
014600961022     D  EDSEL                242    242
014700080626     D  VID9000              243    243
014800130226     D  VIDmanca             244    244
014900980415     D  COMDSA               246    250P 0
015000980108     D  PARTQU               251    251
015100980108     D  VIDSN                252    252
015200980108     D  VIDSTA               253    253
015300980108     D  VIDFIL               254    256  0
015400980416     D                 DS
015500980416     D  WANSA                  1      4  0
015600980416     D  WMGSA                  5      8  0
015700980416     D  WDSA                   1      8  0
015800911018     D                 DS
015900941228     D  BLPAAS                 1      4  0
016000950113     D  BLPMGS                 5      8  0
016100941228     D  BLPDSP                 1      8  0
016200911018     D                 DS
016300941228     D  COMAAS                 1      4  0
016400941228     D  BLPLNP                 5      7  0
016500941228     D  BLPNRS                 8      9  0
016600941228     D  BLPNSP                10     16  0
016700950113     D  BLPSPE                 1     16  0
016800930318     D                 DS
016900950113     D  AA                     1      4  0
017000950113     D  MM                     5      6  0
017100950113     D  GG                     7      8  0
017200950113     D  DATA                   1      8  0
017300020909      *--------------------------------------------------------------*
017400020909      *   V A R I A B I L I
017500020909      *--------------------------------------------------------------*
017600020909     D COMLNP          S                   LIKE(BLPLNP) inz
017700020909     D SAVSPE          S                   LIKE(BLPSPE) inz
017800020909     D WNPG            S                   LIKE(BRVNPG) inz
017900020909     D WSNPG           S                   LIKE(BRVNPG) inz
018000110718     ***D PRTNPG          S                   LIKE(BRVNPG) inz
018100020909     D PARNPG          S                   LIKE(BRVNPG) inz
018200020909     D WDTNPG          S                   LIKE(BRVNPG) inz
018300020909     D WDTNFV          S                   LIKE(BRVNFV) inz
018400020909     D WDTNPS          S                   LIKE(BRVNPS) inz
018500030624     d wdtLnp          s                   Like(BrvLnp) Inz
018600030624     d wdtNrs          s                   Like(BrvNrs) Inz
018700120127     d prtLnp          s                   Like(BrvLnp) Inz
018800120127     d prtNrs          s                   Like(BrvNrs) Inz
018900110718     ***D PRTNFV          S                   LIKE(BRVNFV) inz
019000020909     D WSNFV           S                   LIKE(BRVNFV) inz
019100020909     D COMNFV          S                   LIKE(BRVNFV) inz
019200020909     D WNFV            S                   LIKE(BRVNFV) inz
019300110719     ***D W88NFV          S                   LIKE(BRVNFV) inz
019400110719     ***D W88NPS          S                   LIKE(BRVNPS) inz
019500110718     ***D PRTDFV          S                   LIKE(FVVDFV) inz
019600020909     D WDFV            S                   LIKE(FVVDFV) inz
019700020909     D STADFV          S                   LIKE(FVVDFV) inz
019800020909     D ENTDFV          S                   LIKE(STADSP) inz
019900020909     D DAMDFV          S                   LIKE(STADSP) inz
020000020909     D WFGS            S                   LIKE(BRVFGS) inz
020100020909     D PRTNPS          S                   LIKE(BRVNPS) inz
020200020909     D ENTNPS          S                   LIKE(BRVNPS) inz
020300020909     D SAVNPS          S                   LIKE(BRVNPS) inz
020400020909     D PRTZNC          S                   LIKE(BLPZNC) inz
020500110718     ***D PRTSPG          S                   LIKE(FVVSPG) inz
020600020909     D WSSPG           S                   LIKE(FVVSPG) inz
020700020909     D WSFGS           S                   LIKE(FVVFGS) inz
020800110718     ***D PRTFGS          S                   LIKE(FVVFGS) inz
020900020909     D DESNFV          S                   LIKE(FVVDSF) inz
021000020909     D VIDDSP          S                   LIKE(COMDSP) inz
021100020909     D VIDDSA          S                   LIKE(COMDSP) inz
021200020909     D COMNRT          S                   LIKE(BLPNRT) inz
021300020909     D COD             S                   LIKE(TBLCOD) inz
021400020909     D KEY             S                   LIKE(TBLKEY) inz
021500020909     D WMAX            S                   LIKE(TBLKEY) inz
021600020909     D WFEDFV          S                   LIKE(FVVDFV) inz
021700050920     D Wdatafv         S                   LIKE(FVVDFV) inz
021800020909     D WSTS            S                   LIKE(EDISTS) inz
021900121102     D** EDCOD           S                   LIKE(TABCOD) inz
022000121102     D** EDKEY           S                   LIKE(TABKEY) inz
022100020909     D WDEF            S                   LIKE(D53DEF) inz
022200020909     D WSPG            S                   LIKE(D53SPG) inz
022300020909     D COMCAA          S                   LIKE(ANMCAA) INZ(55)
022400110720     d S_ChiSono       s                   like(ARSnot) inz
022500020918     d kfgs            s                   Like(fvvfgs)
022600021203     D kpdpdr          S                   LIKE(APDpdr)
022700021203     D kpdtip          S                   LIKE(APDtip)
022800020909     D*
022900020909     D* Indici di schiere
023000020909     D X               S              3  0 inz
023100020909     D Y               S              3  0 inz
023200020909     D Z               S              3  0 inz
023300020909     D XX              S              4  0 inz
023400020909     D YY              S              3  0 inz
023500070109     D EE              S              4  0 inz
023600070109     D ZZ              S              4  0 inz
023700150623     D BB              S              4  0 inz
023800020909     D W               S              4  0 inz
023900020909     D WW              S              4  0 inz
024000020909     D XW              S              4  0 inz
024100020918     d xy              s              2  0 inz
024200140717     d i               s              3  0 inz
024300020909     D* Flag booleani
024400020909     D WTROV           S              1    inz
024500020909     D RIPROW          S              1  0 inz
024600020909     D $ok£1           S              1    inz(*off)
024700020909     D* Campi di comodo
024800020909     D W002A           S              2    inz
024900020909     D W0030           S              3  0 inz
025000020909     D W0140           S             14  0 inz
025100020909     D WLPSR           S              5  0 inz
025200140717     d dataiso         s               d   datfmt(*iso)
025300140717     d wque            s              1    inz
025400020909     D* Campi di stampa
025500020909     D UDATE8          S              8  0 inz
025600020909     D UTIME           S              6  0 inz
025700020909     D COMSTA          S              2    inz
025800080626     D COM9000         S              2    inz
025900020909     D COMSN           S              2    inz
026000130226     D COMMANCA        S              2    inz
026100020909     D GGMM            S              4  0 inz
026200020909     D EDICON          S              1    inz
026300020909     D* - Decodifica P.O. Gestione
026400020909     D DESFIL          S             20    inz
026500020909     D* - Data Spedizione
026600020909     D STADSP          S              4  0 inz
026700020909     D* - Ragione Sociale Mittente
026800110715     D STARSM          S             25    inz
026900110721      * - Ragione Sociale Destinatario
027000110721     d S_StaRSD        s             25    inz
027100020909     D* - Data Ritiro Merce
027200020909     D STADRT          S              4  0 inz
027300020909     D* - Decodifica Padroncino
027400020909     D DESCPR          S             15    inz
027500020909     D* - Data Foglio
027600020909     D PR4DFV          S              4  0 inz
027700020909     D PRTDPG          S              8    inz
027800110718     D* - P.O. Gestione
027900110718     D PRDFGS          S              8    inz
028000020909     D* - Spunta Mancante
028100110718     ***D DAMNPS          S              2    inz
028200110718     ***D DESANP          S             11    inz
028300020909     D* - P.O. Linea di Partenza
028400020909     D DESLNP          S             15    inz
028500020909     D* - Decodifica Padroncino
028600020909     D DESCPC          S             15    inz
028700020909     D* - Decodifica Cliente
028800020909     D DESCLI          S             20    inz
028900020909     D* - Data Ritiro Merce
029000020909     D VIDDRT          S              8  0 inz
029100020909     D* - Numero Serie
029200020909     D VIDNRS          S              2    inz
029300110715
029400110715       // -?Stringa SQL da eseguire?
029500110715     d wSQL            s           2048    Varying        inz
029600110715
029700110715       // -?Ds di comodo per la definizione dei campi estratti via SQL?
029800110715     d WFQEMds       e ds                  extname(WFQEM00F)
029900110718     d                                     inz
030000110715
030100110715       //--------------------------------------------------------------
030200110715       //?Definizione prototipi procedure.                             ?
030300110715       //--------------------------------------------------------------
030400110715
030500110715       // -?Parametri gestione errori API.?
030600110715      /copy qsysinc/qRpgleSrc,QUSEC
030700110715       // -?Parametri API QCAPCMD (Process Commands)?
030800110715     d Qcmd            s           2048    inz  varying
030900110715      /copy qSysInc/qRpgleSrc,QCAPCMD
031000110715       // -?API QCAPCMD (Process Commands)?
031100110715      /copy gaitrasrc/srcProtoPR,QCAPCMD
031200110720
031300110720       //--------------------------------------------------------------
031400110720       //?Definizione key-list.                                        ?
031500110720       //--------------------------------------------------------------
031600110720
031700110720       // -?File FIARS01L?
031800110720     d k05fiars01    e ds                  extname(FIARS01L : *key)
031900110720     d                                     prefix(k_)   inz
032000020909
032100020909      ****************************************************************
032200020909      *  RIEPILOGO INDICATORI
032300020909      ****************************************************************
032400020909      * 01    - STAMPA SCRITTA - MANCANTE
032500020909      * 02    - STAMPA SCRITTA - PRESENTE SU FOGLIO ENTRATA XXXXX
032600020909      * 03    - STAMPA SCRITTA - ZONA SEGNACOLLO DIVERSA DA ZONA BOLLA
032700020909      * 04    - STAMPA SCRITTA - ECCEDENTE DI BOLLA NON SELEZIONATA
032800020909      * 05    - STAMPA SCRITTA - MANCA BOLLA PARTENZA
032900070419      * 06 96 - stampa decodifica consegna anomala se c'è
033000070419      * 07    - ON  CAMBIATA SPEDIZIONE
033100020909      * 08    - ON  ESISTE SERIE
033200020909      * 09    - SEGNACOLLI NON SEQUENZIALI
033300020909      * 10    - ON LOKUP RIUSCITA
033400020909      * 11    - STAMPA SCRITTA - COLLO MANCANTE SPARATO SU F.ENTRATA
033500020909      * 12    - STAMPA SCRITTA - COLLO MANCANTE SPARATO IN ARRIVO
033600020909      * 13    - DI COMODO
033700110718     *** * 14    - SE SEGNACOLLO MANCANTE STAMPO RIFERIM. ALLEGATI MITT.
033800020909      * 15    - ON  NON C'E' COD PADRONCINO CONSEGNA
033900020909      * 16    - ON  LOKUP RIUSCITA
034000061215      * 17    - acceso se collo arrivato al terminal di arrivo previsto
034100061215      *       - e non disguidato
034200020909      * 18    - ON  E' RICHIESTA LA STAMPA ANOMALIE DI MANCA BOLLA E
034300020909      *         COLLI ECCEDENTI
034400020909      * 19    - ELABORO ALMENO UNA BOLLA
034500020909      * 20    - STAMPO TESTATA
034600020909      * 21    - STAMPA SCRITTA - COLLO MANCANTE SPUNTA 88
034700020909      * 22    - STAMPA SCRITTA - COLLO ECCEDENTE DI BOLLA NON BORDERIZZ
034800020909      * 23/24 - USATI IN STAMPA PER EMETTERE IL TRATTINO SEPARATORE
034900020909      * 25    - TIPO QUADRATURA CON SPUNTE PARTENZA
035000070419      * 26    - per EDIBRV
035100020909      * 27    - BOLLA AGGIORN.CON SPUNTA FASULLA MA PRESENTE SPU.REALE
035200020909      * 28    - STAMPA SCRITTA - COLLO MANCANTE SPARATO IN ARRIVO
035300020909      *         SU ALTRA RIGA *** ATTENZIONE *** NON PIU' GESTITO E'
035400020909      *         SEMPRE OFF (SONO STATE LASCIATE COMUNQUE LE SPECIFICHE
035500020909      *         DI OUTPUT)
035600020909      * 29    - STAMPO PISTOLA DATA DI ARRIVO
035700020909      * 30/36 - COMODO
035800120127      * 40    - STAMPO DATA per pistola fasulla non in data del controllo
035900091013      * 85    - ctm di autospunta per quadratura entrata
036000070419      * 96 06 - stampa decodifica consegna anomala se c'è
036100020909      *****************************************************************
036200020909
036300900622     C     *ENTRY        PLIST
036400900622     C                   PARM                    KPJBA
036500900622     C                   MOVEL     KPJBU         PARAM
036600950530     C*
036700950531     C* VEDO CHE TIPO DI QUADRATURA E'.
036800950530     C* 25 ON - QUADRATURA CON SPUNTE PARTENZA
036900950530     C     PARTQU        IFEQ      'P'
037000950530     C                   SETON                                        25
037100991022     C                   ENDIF
037200930318     C*---------------------------------------------------------------*
037300920804     C* CARICO TABELLA FILIALI GESTITE £1
037400920804     C                   EXSR      CARTAB
037500930318     C*
037600930318     C* IMPOSTO CAMPI FISSI
037700930318     C                   EXSR      IMPO
037800920804     C*
037900930318     C* STAMPA DELLE RICHIESTE EFFETTUATE A VIDEO
038000930318     C                   EXSR      VIDEO
038100930318     C*
038200950605    1C     LNP(1)        IFEQ      0
038300000619     C                   Z-ADD     0             Y
038400000619     C     1             DO        30            X
038500000619     C     L1(X)         IFNE      0
038600000619     C                   ADD       1             Y
038700000619     C                   MOVE      L1(X)         SKI(Y)
038800000619     C                   MOVE      L1(X)         LP2(Y)
038900000619     C                   ENDIF
039000000619     C                   ENDDO
039100000619      *
039200060310     C                   MOVEL     '99'          NRS
039300950605     C*
039400950605   X1C                   ELSE
039500000619     C* . . . ALTRIMENTI
039600950605     C                   MOVEA     LNP           SKI
039700950605     C* MEMORIZZO LE LNP SELEZIONATE, MA SOLO UNA VOLTA SE FOSSERO
039800950605     C*  RIPETUTE
039900020909     C                   Z-ADD     1             X
040000020909     C                   Z-ADD     1             Y
040100950928    2C     X             DOWLE     12
040200950928     C     LNP(X)        ANDNE     0
040300950605     C     LNP(X)        LOOKUP    LP2                                    30
040400950712     C  N30              MOVEL     LNP(X)        LP2(Y)
040500950605     C  N30              ADD       1             Y
040600950605     C                   ADD       1             X
040700950605    2C                   ENDDO
040800950605    1C                   ENDIF
040900920907     C*
041000950920     C                   Z-ADD     1             Y
041100950920    1C     LP2(Y)        DOWNE     0
041200920902     C*
041300950920     C                   MOVEL     LP2(Y)        COMLNP
041400920804     C* LEGGO LE BOLLE
041500010717     C     KBLP          SETLL     FNBLP31L
041600010717     C     COMLNP        READE     FNBLP31L                               30
041700980417     C* ESCO DAL CICLO SE SUPERATA DATA AL
041800980417     C  N30BLPAAS        IFGT      WANSA
041900980417     C     BLPAAS        ORLE      WANSA
042000980417     C     BLPMGS        ANDGT     WMGSA
042100980417     C                   SETON                                        30
042200980417     C                   END
042300920805     C*
042400950920    2C     *IN30         DOWEQ     *OFF
042500940329     C                   SETOFF                                       010203
042600950601     C                   SETOFF                                       352123
042700000314     C                   SETOFF                                       112712
042800070108     C                   SETOFF                                       282917
042900120127     C                   SETOFF                                       069640
043000950920     C                   SETON                                        10
043100950920     C*
043200950920     C* CONTROLLO TUTTE LE SELEZIONI EFFETTUATE PER QUELLA LINEA
043300950920     C                   Z-ADD     1             X
043400950920    3C     *IN10         DOWEQ     *ON
043500950920     C     COMLNP        LOOKUP    SKI(X)                                 10
043600950920     C*
043700950920     C  N10              SETON                                        35
043800950920     C   10              EXSR      SELEZ
043900950920     C*
044000950920     C  N35              SETOFF                                       10
044100020909     C   35              Z-ADD     X             XX
044200950920     C   35
044300950920     CAN 10              SETOFF                                       35
044400980108     C                   ADD       1             X
044500950920     C                   ENDDO
044600920807     C*
044700950531    3C     *IN35         IFEQ      *OFF
044800990720     C* ESCLUDO I PREPAGATI NON CONFERMATI
044900990720     C     BLPPDR        ANDGT     0
045000091013     c
045100091013     c* controllo il cod trattamento merce
045200091013     c                   setoff                                       85
045300091013     c                   if        not *in25
045400091013     c     blpctm        lookup    ctma                                   85
045500091013     c                   endif
045600091013     c*
045700091013     c* Per quadratura entrata elaboto solo i CTM non autosopunta
045800091013   3ac                   if        not *in85 or *in25
045900950605     C*
046000080626     C* Se non è locale elaboro sempre
046100080626    4C     BLPTFP        IFNE      BLPTFA
046200091013     C     BLPLNP        ANDNE     BLPLNA
046300080626     C* ELABORO LE LOCALI DEI CLIENTI CON serie
046400080626     C     BLPNRS        ORGT      0
046500080626     c
046600091013     c* Quadratura con spunte entrata -->
046700080626     C     *IN25         OREQ      *OFF
046800080626     c
046900080626     c* Quadratura con spunte partenza: le locali serie 00 elaborate
047000080626     c*                    a richiesta VID9000
047100080626     c     *in25         oreq      *on
047200080626     c     blpnrs        andeq     0
047300080626     c     vid9000       andeq     'S'
047400080626     c
047500961025     C* SE ELABORAIZONE CON EDI LEGGO SEMPRE ANCHE LE BOLLE LOCALI
047600080626     C     EDSEL         OREQ      'S'
047700080626     c
047800080617     c***
047900940426     C* 19 ON  - ELABORO ALMENO UNA BOLLA
048000940426     C                   SETON                                        19
048100920804     C* CHAINO DETTAGLIO BOLLA
048200941228     C     KBLT          SETLL     FNBLT01L
048300941228     C     KBLT          READE     FNBLT01L                               32
048400950605    5C     *IN32         DOWEQ     '0'
048500950531     C*
048600950531     C  N25              EXSR      SPUENT
048700980402     C* ESCLUDO NELLA QUADRATURA CON SPUNTE ENTRATA I COLLI
048800980402     C*  CON PISTOLA 83 IDD IN PARTENZA, COLLI NON PERVENUTI DAL PARTNE
048900980402     C   25BLTNPP        IFNE      83
049000980402     C                   EXSR      SPUPAR
049100980402     C                   ENDIF
049200920804     C*
049300940329     C                   SETOFF                                       010203
049400950606     C                   SETOFF                                       212311
049500000320     C                   SETOFF                                       271228
049600120127     C                   SETOFF                                       291740
049700980416     C     KBLT          READE     FNBLT01L                               32
049800950605    5C                   ENDDO
049900080626    4C                   ENDIF
050000091013   3aC                   ENDIF
050100950605    3C                   ENDIF
050200920804     C*
050300010717     C     COMLNP        READE     FNBLP31L                               30
050400980417     C* ESCO DAL CICLO SE SUPERATA DATA AL
050500980417     C  N30BLPAAS        IFGT      WANSA
050600980417     C     BLPAAS        ORLE      WANSA
050700980417     C     BLPMGS        ANDGT     WMGSA
050800980417     C                   SETON                                        30
050900980417     C                   END
051000950113    2C                   END
051100920804     C*
051200950920     C                   ADD       1             Y
051300950113    1C                   END
051400950703     C**
051500070109     C**  PREPARA  LA SKIERA DI LETTURA DELLE LNP+tfp
051600070109     C                   EXSR      PREtfp
051700070109     c* ordino la schiera
051800070109     c                   sorta     lp3
051900070109     c                   z-add     1             EE
052000070109     c                   clear                   savtfp            3 0
052100070109     c
052200070109    1c                   do        99            ee
052300070109    2c                   if        lp3(ee)>0
052400070109     c* A cambio terminal di partenza eseguo lettura
052500070109     c                   movel     lp3(ee)       ktfp
052600070109     c
052700070109    3c                   if        ktfp<>savtfp
052800070109     C**
052900070109     C**  PREPARA  LA SKIERA DI LETTURA DELLE LNP aggiuntive da tabella
053000070109     c**  5F + £1 di ktfp
053100070109     C                   EXSR      Carfas
053200070109     C                   EXSR      PRELP3
053300930528     C*
053400950601     C* QUADRATURA CON SPUNTE ENTRATA
053500070109    4C     *IN25         IFEQ      *OFF
053600950601     C                   Z-ADD     5             WNPG
053700950601     C                   Z-ADD     VIDNFV        WNFV
053800950601     C                   Z-ADD     VIDFIL        WFGS
053900050920     c                   z-add     wfedfv        wdatafv
054000950601     C*
054100950601     C                   EXSR      LEGBRV
054200070109   x4C                   ELSE
054300950605     C*
054400950601     C* QUADRATURA CON SPUNTE PARTENZA
054500950601     C                   Z-ADD     1             WNPG
054600950601     C     KFGV          SETLL     FNFGV002
054700950601     C     KFGV          READE     FNFGV002                               30
054800070109    5C     *IN30         DOWEQ     *OFF
054900950601     C                   Z-ADD     FGVNFV        WNFV
055000950601     C                   Z-ADD     FGVLNP        WFGS
055100050920     c                   z-add     fgvdfv        wdatafv
055200950601     C*
055300950601     C                   EXSR      LEGBRV
055400950601     C     KFGV          READE     FNFGV002                               30
055500070109    5C                   ENDDO
055600950601     C* LEGGO FOGLIO IMP
055700950601     C                   Z-ADD     3             WNPG
055800020918      * devo leggere per tutti i p.o. del terminal di partenza
055900070109    5c                   do        30            xy
056000070109    6c                   If        lin(xy) = *Zeros
056100020918     c                   Leave
056200070109    6c                   EndIf
056300070109     c                   Movel     Lin(xy)       Kfgs
056400020918     C     KFVV2         SETLL     FNFVV002
056500020918     C     KFVV2         READE     FNFVV002                               30
056600070109    6C     *IN30         DOWEQ     *OFF
056700950601     C*
056800070109    7C     FVVSPG        IFEQ      'P'
056900950601     C                   Z-ADD     FVVNFV        WNFV
057000950601     C                   Z-ADD     FVVFGS        WFGS
057100050920     c                   z-add     fgvdfv        wdatafv
057200950601     C*
057300950601     C                   EXSR      LEGBRV
057400070109    7C                   ENDIF
057500950601     C*
057600020918     C     KFVV2         READE     FNFVV002                               30
057700070109    6C                   ENDDO
057800101202      *
057900070109    5c                   EndDo
058000950601     C*
058100070109    4C                   ENDIF
058200070109     c                   eval      savtfp=ktfp
058300070109    3C                   ENDIF
058400070109    2C                   ENDIF
058500070109    1C                   ENDdo
058600110715      *
058700110715      * Stampa dati registrati nel work-file WFQEM00F
058800110715     c                   exsr      sr_StampaWF
058900911204     C*
059000940421     C* 19 ON  - HO ELABORATO ALMENO UNA BOLLA
059100950113    1C     *IN19         IFEQ      *ON
059200940421     C* SE NON CI SONO ANOMALIE STAMPO MESSAGGIO DI O.K.
059300940426     C  N20              EXCEPT    OK
059400950113   X1C                   ELSE
059500940421     C* SE NON HO ELABORATO NESSUN DATO STAMPO MESSAGGIO
059600940421     C                   EXCEPT    NOELAB
059700950113    1C                   ENDIF
059800950601     C*
059900950601     C* STAMPO DICITURA DI FINE STAMPA
060000110715     C                   EXCEPT    PIEDI
060100920916     C*
060200950531    1C  N25VIDSN         IFEQ      'S'
060300140717     c* se quadratura definitiva per edi non chiudo il foglio ma scrivo file
060400140717     c* FNQUE per memorizzare i parametri di elaborazione che servono al filtro
060500140717     c* per impedire lanci ripetuti con gli stessi parametri
060600140717     c                   if        edsel<>'S'
060700950113     C     KFVV          CHAIN     FNFVV000                           32
060800920916     C*
060900920916     C  N32              MOVEL     'S'           FVVFCF
061000950113     C  N32              UPDATE    FNFVV000
061100140717     c                   else
061200140717     c                   if        wque='1'
061300140717     c                   exsr      sr_FNQUE
061400140717     c                   endif
061500140717     c                   endif
061600950113    1C                   END
061700020909     C                   CLEAR                   FNLV53DS
061800980324     C                   MOVEL     'C'           D53TLA
061900980324     C                   CALL      'FNLV53R'
062000020909     C                   PARM                    FNLV53DS
062100070109     C                   CLEAR                   FNLV55DS
062200070109     C                   MOVEL     'C'           D55TLA
062300070109     C                   CALL      'FNLV55R'
062400070109     C                   PARM                    FNLV55DS
062500920424     C*
062600020909     C                   MOVEL     *ON           *INLR
062700911018     C*
062800140717     C*--- SCRITTURA FILE FNQUE: parametri di elaboraz. def. x EDI---*
062900140717     C     SR_FNQUE      BEGSR
063000140717     c                   clear                   fnque000
063100140717     c                   z-add     vidnfv        quenfv
063200140717     c                   z-add     5             quenpg
063300140717     c                   eval      QUEFGS=vidfil
063400140717     c                   eval      dataiso=%date()
063500140717     c                   eval      QUEDEL=%dec(dataiso)
063600140717     c                   eval      QUEUSR=knmus
063700140717     c* ciclo di write per ogni elemento della schiera dei parametri di lancio
063800140717     c                   for       I=1 to %elem(lnp)
063900140717     c                   if        lnp(i)>0 or
064000140717     c                             cpc(i)>0 or
064100140717     c                             cli(i)>0 or
064200140717     c                             drt(i)>0 or
064300140717     c                             nrt(i)<> *blanks or
064400140717     c                             mop(i)<> *blanks
064500140717     c                   eval      QUELNP=lnp(i)
064600140717     c                   eval      QUEPDR=cpc(i)
064700140717     c                   eval      quenrs=nrs(i)
064800140717     c                   eval      QUECCM=cli(i)
064900140717     c                   eval      QUEDRT=drt(i)
065000140717     c                   eval      quenrt=nrt(i)
065100140717     c                   eval      QUEFPP=mop(i)
065200140717     c                   write     fnque000
065300140717     c                   endif
065400140717     c                   endfor
065500140717     c                   endsr
065600930318     C*--- IMPOSTO CAMPI FISSI --------------------------------------*
065700911018     C     IMPO          BEGSR
065800950113     C                   Z-ADD     0             SAVSPE
065900950113     C* PRENDO LA DATA DEL GIORNO DA TIME
066000020909     C                   TIME                    W0140
066100020909     C                   MOVE      W0140         UDATE8
066200020909     C                   MOVEL     W0140         UTIME
066300930125     C*
066400020909     C                   MOVEL     *ON           *INOF
066500930319     C*
066600930527     C* CONTROLLO SE RICHIESTA STAMPA ANOMALIE DI MANCA BOLLA E COLLI
066700930527     C*   ECCEDENTI
066800930319     C     VIDSTA        IFEQ      'S'
066900930319     C                   SETON                                        18
067000020909     C                   MOVEL     'SI'          COMSTA
067100930319     C                   ELSE
067200930319     C                   MOVEL     'NO'          COMSTA
067300930319     C                   END
067400080626     C* CONTROLLO SE RICHIESTA STAMPA bolle locali
067500080626     C     VID9000       IFEQ      'S'
067600080626     C                   MOVEL     'SI'          COM9000
067700080626     C                   ELSE
067800080626     C                   MOVEL     'NO'          COM9000
067900080626     C                   END
068000070108     C
068100070108     C* LA RICHIESTA VALE SIA PER QUADRATURA CON SPU PARTENZE CHE
068200070108     C*  ENTRATA MA CON SIGNIFICATI DIVERSI
068300070108     C     VIDSN         IFEQ      'S'
068400070108     C                   MOVEL     'SI'          COMSN
068500070108     C                   ELSE
068600070108     C                   MOVEL     'NO'          COMSN
068700070108     C                   END
068800130226     C* Quadrautra entrata: esclusione i mancanti spuntati in latro foglio
068900130226     C     VIDmanca      IFEQ      'S'
069000130226     C                   MOVEL     'SI'          COMmanca
069100130226     C                   ELSE
069200130226     C                   MOVEL     'NO'          COMmanca
069300130226     C                   END
069400980416     C* IMPOSTO DATA SPED.AL PER LETTURE:SE COMDSA=0 -> WDSA=COMDSP;
069500980416     C* ALTRIMENTI WDSA=COMDSA
069600980416     C     COMDSA        IFEQ      *ZEROS
069700980416     C                   Z-ADD     COMDSP        WDSA
069800980416     C                   ELSE
069900980416     C                   Z-ADD     COMDSA        WDSA
070000980416     C                   END
070100950601     C**
070200950601     C* GIRO DATA SPEDIZIONE IN GG/MM/AA
070300950601     C                   Z-ADD     COMDSP        DATA
070400950601     C                   Z-ADD     MM            GGMM
070500950601     C                   MOVEL     GG            GGMM
070600950601     C                   Z-ADD     AA            VIDDSP
070700950601     C                   MOVEL     GGMM          VIDDSP
070800980416     C*
070900980416     C                   Z-ADD     WDSA          DATA
071000980416     C                   Z-ADD     MM            GGMM
071100980416     C                   MOVEL     GG            GGMM
071200980416     C                   Z-ADD     AA            VIDDSA
071300980416     C                   MOVEL     GGMM          VIDDSA
071400110718      /free
071500110718         // -?Pulizia work-file?
071600110718         Qcmd = 'DLTF file(QTEMP/WFQEM00F)';
071700110718         exsr  sr_ExecCmd;
071800110718         Qcmd = 'CRTDUPOBJ obj(WFQEM00F) +
071900110718                           fromlib(*libl) +
072000110718                           objtype(*file) +
072100110718                           tolib(QTEMP) +
072200110718                           data(*no)';
072300110718         exsr  sr_ExecCmd;
072400110718         // -?Apertura dello stesso?
072500110718         open WFQEM00F;
072600110718      /end-free
072700950530     C**
072800950530     C* SOLO SE RICHIESTO FOGLIO ENTRATA
072900950530     C     *IN25         IFEQ      *OFF
073000950601     C                   Z-ADD     VIDNFV        COMNFV
073100950530     C*
073200920409     C*
073300930318     C* PRENDO DATA E DESCRIZIONE DEL FOGLIO ENTRATA
073400950531     C                   Z-ADD     5             WNPG
073500950113     C     KFVV          CHAIN(N)  FNFVV000                           32
073600920409     C*
073700920421     C     *IN32         IFEQ      '0'
073800950113     C                   Z-ADD     FVVDFV        DATA
073900020909     C                   Z-ADD     MM            GGMM
074000950113     C                   MOVEL     GG            GGMM
074100950113     C                   Z-ADD     AA            STADFV
074200950113     C                   MOVEL     GGMM          STADFV
074300961022     C                   MOVEL     FVVDFV        WFEDFV
074400930318     C*
074500950113     C                   MOVEL     FVVDSF        DESNFV
074600920421     C                   END
074700930318     C*
074800930318     C* DECODIFICA DELLA FILIALE IN GESTIONE
074900930318     C     VIDFIL        CHAIN     AZORG                              32
075000020909     C  N32              MOVEL     ORGDES        DESFIL
075100930318     C   32              MOVEL     *BLANKS       DESFIL
075200950601     C                   ENDIF
075300961022     C*
075400961022     C* SE ELABORAZIONE DEFINITIVA E RICHIESTA EDI APRO IL FILE
075500961025     C     EDSEL         IFEQ      'S'
075600961025     C     VIDSN         ANDEQ     'S'
075700980108     C* ESEGUO CARICAMENTO DATI TABELLA PT E PU DA EDTAB
075800121102     C******             EXSR      CARPU
075900980108     C*
076000020909     C                   MOVEL     'S'           EDICON
076100961028     C                   SETON                                        26
076200980108     C                   OPEN      EDBRV00F
076300140717     C                   OPEN      FNQUE00F
076400961025     C*
076500961025     C* CALL AL PGM CHE CANCELLA EVENTUALI RECORD GIA' PRESENTI
076600020909     C                   DO        12            Y
076700980108     C     CLI(Y)        IFNE      0
076800020909     C                   CLEAR                   TRTC87DS
076900980108     C                   MOVEL     CLI(Y)        D87CCM
077000961022     C                   MOVEL     VIDNFV        D87NFE
077100961022     C                   MOVEL     WFEDFV        D87DFE
077200020909     C                   MOVEL     TRTC87DS      KPJBU
077300961022     C                   CALL      'TRTC87R4'
077400961022     C                   PARM                    KPJBA
077500980108     C                   ELSE
077600980108     C     LNP(Y)        IFNE      0
077700020909     C                   CLEAR                   TRTC87DS
077800980108     C                   MOVEL     LNP(Y)        D87LNP
077900980108     C                   MOVEL     VIDNFV        D87NFE
078000980108     C                   MOVEL     WFEDFV        D87DFE
078100020909     C                   MOVEL     TRTC87DS      KPJBU
078200980108     C                   CALL      'TRTC87R4'
078300980108     C                   PARM                    KPJBA
078400980108     C                   END
078500980108     C                   END
078600980108     C                   END
078700980108     C*
078800961025     C* LETTURA FITTIZIA DEL FILE PER GIUSTIFICARE L'APERTURA IN INPUT
078900961025     C*  PER CHIAREZZA HO PREFERITO RIDENOMINARE I CAMPI
079000961025     C  N30
079100961025     CAN 30              READ      EDBRV000                               30
079200961022     C                   ENDIF
079300980108     C*
079400911018     C                   ENDSR
079500911015     C*
079600930318     C*--- STAMPA PARAMETRI RICHIESTI --------------------------------*
079700930318     C     VIDEO         BEGSR
079800930318     C*
079900930318     C                   EXCEPT    RICHI1
080000930318     C*
080100930318     C                   Z-ADD     1             X
080200950323     C     X             DOWLE     12
080300930615     C     LNP(X)        ANDNE     0
080400930318     C*
080500930319     C                   SETOFF                                       2324
080600020909     C                   MOVEL     *BLANKS       DESLNP
080700020909     C                   MOVEL     *BLANKS       DESCPC
080800020909     C                   MOVEL     *BLANKS       DESCLI
080900020909     C                   Z-ADD     0             VIDDRT
081000930319     C*
081100930318     C* DECODIFICO LINEA DI PARTENZA
081200930318     C     LNP(X)        CHAIN     AZORG                              32
081300930319     C  N32              MOVEL     ORGDES        DESLNP
081400930318     C   32              MOVEL     *BLANKS       DESLNP
081500930318     C*
081600930318     C* DECODIFICO CODICE PADRONCINO
081700930318     C     CPC(X)        IFNE      0
081800930319     C                   SETON                                        23
081900021203     C*    CPC(X)        CHAIN     FNAPD01L                           32
082000021205     C                   move      CPC(X)        kpdpdr
082100021203     C                   move      'A'           kpdtip
082200021203     C     kfapd         chain     fiapd01l                           32
082300950113     C  N32              MOVEL     APDRSC        DESCPC
082400950113     C   32              CLEAR                   DESCPC
082500930318     C                   END
082600930318     C*
082700930524     C* NUMERO SERIE
082800060310     C     NRS(X)        IFEQ      '99'
082900020909     C                   MOVEL     *BLANKS       VIDNRS
083000930524     C                   ELSE
083100930524     C                   MOVEL     NRS(X)        VIDNRS
083200930524     C                   END
083300930524     C*
083400930318     C* DECODIFICO CODICE CLIENTE
083500930318     C     CLI(X)        IFNE      0
083600930319     C                   SETON                                        24
083700930318     C     KACO          CHAIN     CNACO000                           32
083800930319     C  N32              MOVEL     ACORAG        DESCLI
083900930318     C   32              MOVEL     *BLANKS       DESCLI
084000930318     C                   END
084100930318     C*
084200930318     C* GIRO DATA RITIRO MERCE
084300930318     C     DRT(X)        IFNE      0
084400930318     C                   Z-ADD     DRT(X)        DATA
084500950113     C                   Z-ADD     MM            GGMM
084600950113     C                   MOVEL     GG            GGMM
084700950113     C                   Z-ADD     AA            VIDDRT
084800950113     C                   MOVEL     GGMM          VIDDRT
084900930318     C                   END
085000930318     C*
085100930318     C                   EXCEPT    RICHI2
085200930318     C                   ADD       1             X
085300930318     C                   END
085400930318     C                   ENDSR
085500930318     C*
085600920804     C*--- CARICO TABELLA FILIALI GESTITE £1 -------------------------*
085700920804     C     CARTAB        BEGSR
085800941228     C***
085900941228     C* CARICO TABELLA FILIALI GESTITE £1
086000941228     C***
086100020909     C                   CLEAR                   TRUL06DS
086200941228     C                   MOVE      '£1'          D06COD
086300941228     C                   MOVEL     SIMFEL        D06KEY
086400061215     c                   move      'S'           d06key
086500061215     c                   movel     comdsp        d06drf
086600020909     C                   MOVEL     TRUL06DS      KPJBU
086700941228     C*
086800941228     C                   CALL      'TRUL06R'
086900941228     C                   PARM                    KPJBA
087000020909     C                   MOVEL     KPJBU         TRUL06DS
087100941228     C                   MOVEA     LIN           L1
087200980324     C***
087300980324     C*  CODICI PISTOLA CON PISTOLA AUTOMATICA
087400980324     C***
087500980324     C                   Z-ADD     1             X
087600020909     C                   Z-ADD     1             Y
087700980324     C                   MOVEL     '7J'          COD
087800980324     C     KTAB2         CHAIN     TABEL00F                           31
087900980324     C*
088000980324     C     *IN31         DOWEQ     *OFF
088100980324     C                   MOVEL     TBLUNI        DS7J
088200980324     C     §7JRPS        IFEQ      'A'
088300980324     C                   MOVEL     TBLKEY        NPS(X)
088400980324     C                   ADD       1             X
088500980324     C                   ENDIF
088600980324     C     §7JFSL        IFEQ      'F'
088700980402     C                   MOVEL     TBLKEY        NPF(Y)
088800980324     C                   ADD       1             Y
088900980324     C                   ENDIF
089000980324     C     KTAB2         READE     TABEL00F                               31
089100980324     C                   ENDDO
089200091013     C***
089300091013     C*  carico i codici trattamento merce autospunta
089400091013     C***
089500091013     C                   Z-ADD     1             X
089600091013     C                   MOVEL     '1B'          COD
089700091013     C     KTAB2         CHAIN     TABEL00F                           31
089800091013     C*
089900091013     C     *IN31         DOWEQ     *OFF
090000091013     C                   MOVEL     TBLUNI        DS1B
090100091013     C     §1bfg5        IFEQ      'N'
090200091013     C                   MOVEL     TBLKEY        ctma(X)
090300091013     C                   ADD       1             X
090400091013     C                   ENDIF
090500091013     C     KTAB2         READE     TABEL00F                               31
090600091013     C                   ENDDO
090700950703     C*
090800130225     c* carico  eventuali colli disk B che partono da TFP diversi
090900130225     c                   clear                   flsnrs
091000130225     c                   clear                   xcotfp
091100130225     C                   Z-ADD     1             X
091200130225     c*
091300130225     C                   MOVEL     'XCO'         kcod
091400130225     C                   eval      kke1='ALTRITFP'
091500130225     C     KTbe          setll     Tntbe01l
091600130225     C     KTbe          reade     tntbe01l
091700130225     c
091800130225    1c                   dow       not %eof(tntbe01l)
091900130225    2c                   if        tbeatb=' '
092000130225     C                   MOVEA     TBeUNI        xcotfp
092100130225     c                   movel     simfel        simalfa           3
092200130225     c                   eval      Indx=%lookup(simalfa:xcotfp)
092300130225    3c                   if        Indx>0
092400130225     C                   MOVE      TBeke2        flsnrs(X)
092500130225     C                   ADD       1             X
092600130225    4C     X             IFGE      $MaxElem
092700130225     C                   SETON                                        31
092800130225    4C                   ENDIF
092900130225    3C                   END
093000130225    2C                   END
093100130225     c
093200130225     C     KTbe          reade     tntbe01l
093300130225    1C                   ENDDO
093400130225
093500920804     C                   ENDSR
093600070109     C**********************************************************************
093700070109     C*  FILIALE DI ALTRI terminal confermabili
093800070109     C**********************************************************************
093900070109     c     CARFAS        BEGSR
094000070109     C***
094100070109     C* CARICO TABELLA FILIALI GESTITE da tfp
094200070109     C***
094300070109     C                   CLEAR                   TRUL06DS
094400070109     C                   MOVE      '£1'          D06COD
094500070109     C                   MOVEL     ktfp          D06KEY
094600070109     c                   move      'S'           d06key
094700070109     c                   movel     comdsp        d06drf
094800070109     C                   MOVEL     TRUL06DS      KPJBU
094900070109     C*
095000070109     C                   CALL      'TRUL06R'
095100070109     C                   PARM                    KPJBA
095200070109     C                   MOVEL     KPJBU         TRUL06DS
095300101202     c*
095400070109     c                   clear                   FAS
095500070109     c                   clear                   LAS
095600070109     C                   Z-ADD     1             X
095700101202     c*
095800070109     C                   MOVEL     '5F'          COD
095900070109     C                   MOVEL(P)  ktfp          KEY
096000070109     C                   MOVE      *ALL'9'       WMAX
096100070109     C                   MOVEL     ktfp          WMAX
096200070109     C     KTAB          SETLL     TABEL00F
096300070109     C     KTAB2         READE     TABEL00F                               31
096400070109     C     *IN31         DOWEQ     *OFF
096500070109     C     TBLKEY        ANDLE     WMAX
096600070109     C     TBLFLG        IFEQ      ' '
096700070109     C                   MOVE      TBLKEY        FAS(X)
096800070109     C     X             OCCUR     DSLAS
096900070109     C                   MOVEA     TBLUNI        LAS
097000070109     C                   ADD       1             X
097100080109    4C     X             IFGE      $MaxElem
097200070109     C                   SETON                                        31
097300070109    4C                   ENDIF
097400070109     C                   END
097500070109     C  N31KTAB2         READE     TABEL00F                               31
097600070109     C                   ENDDO
097700150623     c
097800150623     c* Accodi alla FAS anche le lnp serie 00 confermabili qui
097900150623     c*  da file FIBSP
098000150623    0c                   if        x<$MaxElem
098100150623     c
098200150623     c     000           setgt     fibsp01l
098300150623     c                   read      fibsp01l
098400150623    1c                   dow       not %eof(fibsp01l)
098500150623    2c                   if        bspfil>0 and bspfpa='S'
098600150623     c                   z-add     1             BB
098700150623    3c                   dow       lin(bb)>0
098800150623    4c                   if        %lookup(lin(BB):sbsp)>0
098900150623     c                   eval      fas(X)=bspfil*100
099000150623     C     X             OCCUR     DSLAS
099100150623     C                   MOVEA     bsplin        LAS
099200150623     C                   ADD       1             X
099300150623     c                   leave
099400150623    4c                   endif
099500150623     c                   add       1             BB
099600150623    3c                   enddo
099700150623     c
099800150623    2c                   endif
099900150623    2C     X             IFGE      $MaxElem
100000150623     c                   leave
100100150623    2c                   endif
100200150623
100300150623     c                   read      fibsp01l
100400150623    1c                   enddo
100500150623    0c                   endif
100600150623     c
100700070109     c                   ENDSR
100800930527     C*
100900110715      * REGISTRO ERRORE NEL WORK-FILE -------------------------------*
101000110715     ***C     STAMPA        BEGSR
101100110715     c     sr_WriteWF    BEGSR
101200110715     c*
101300980623     C* SE CONTROLLO IN EDI NON STMAPO SE COLLO CONSEGNATO ALTRIMENTI
101400980623     C** SEMPRE
101500110715if  1C     EDSEL         IFEQ      'S'
101600980623     C     BLPDCM        ANDEQ     0
101700980623     C     EDICON        ORNE      'S'
101800980623     C     *IN05         OREQ      '1'
101900980623     C**
102000110719     ***C                   SETON                                        20
102100110719     ***C*
102200110719     ***C* 05 OFF - STAMPO DATI SPEDIZIONE
102300110719do  2***C  N05              DO
102400110719     ***C*
102500110719     ***C                   Z-ADD     BLPAAS        COMAAS
102600110719if  3***C     SAVSPE        IFNE      BLPSPE
102700110719     ***C                   SETON                                        07
102800110719     ***C                   MOVEL     BLPSPE        SAVSPE
102900110719e   3***C                   END
103000110719     ***C* DATA SPEDIZIONE
103100110719do  3***C   07              DO
103200110719     ***C*
103300110719     ***C* SEGNACOLLI NON CONSECUTIVI
103400110719     ***C     BLPFNS        COMP      'S'                                    09
103500110719     ***C*
103600110719     ***C* SERIE >< 0
103700110719     ***C     BLPNRS        COMP      0                                  0808
103800110719     ***C****
103900110719     ***C**** RIFERIM. ALLEGATI MITTENTE
104000110719     ***C***     BLPRMA        COMP      *BLANKS                            1414
104100110719     ***C*
104200110719e   3***C                   END
104300110719e   2***C                   END
104400110719     ***c*
104500110719     ***c* Per consegna anomala, vedo che indicatore accendere
104600110719     ***c                   setoff                                       9606
104700110719if  2***c                   if        blpcca<>' '
104800110719if  3***c                   if        *in01 and not *in11 and not *in12
104900110719     ***c                   seton                                        06
105000110719x   3***c                   else
105100110719     ***c                   seton                                        96
105200110719e   3***c                   endif
105300110719e   2***c                   endif
105400110715      *
105500110715      * REGISTRO RECORD DI DETTAGLIO
105600110715      *
105700110719if  2c                   if        Not *in05
105800110718     c                   eval      WQEaas = BLPaas
105900110718     c                   eval      WQElnp = BLPlnp
106000110718     c                   eval      WQEnrs = BLPnrs
106100110718     c                   eval      WQEnsp = BLPnsp
106200110718     c                   eval      WQEmgs = BLPmgs
106300110718     c                   eval      WQElna = BLPlna
106400110718     c                   eval      WQErsm = BLPrsm
106500110720     c                   eval      WQEfls = BLTfls
106600110718     c                   eval      WQEnsc = BLTnsc
106700110719x   2c                   else
106800110718     c                   eval      WQEaas = *hival
106900110718     c                   eval      WQElnp = BRVlnp
107000110720     c                   eval      WQEfls = BRVlnp
107100110718     c                   eval      WQEnrs = BRVnrs
107200110718     c                   clear                   WQEnsp
107300110718     c                   eval      WQEmgs = *hival
107400110718     c                   eval      WQElna = BRVlna
107500110718     c                   eval      WQErsm = *hival
107600110718     c                   eval      WQEnsc = BRVnsc
107700110719e   2c                   endif
107800110718      *
107900110718     c                   movea     *in           WQEind
108000110715      *
108100110715     c                   write     WFQEM000
108200140717     c                   eval      wque='1'
108300110715      *
108400110719     ***C                   SETOFF                                       07
108500110715     C**
108600110719e   1C                   ENDIF
108700900622     C*
108800900622     C                   ENDSR
108900950531     C*
109000950531     C*  SELEZIONI FATTE NEL FILTRO ----------------------------------*
109100950531     C     SELEZ         BEGSR
109200970529     C* SE X E' > DI 12 (MASSIME SELEZIONI) --> FACCIO FINA CHE VADA
109300970529     C*  TUTTO BENE PERCHE' TANTO E' COSI' PERCHE' HANNO CHIESTO
109400970529     C*  TUTTE LA L1
109500970529    0C     X             IFLE      12
109600970529     C**
109700950531     C* SE IMMESSO CONTROLLO CODICE PADRONCINO
109800980409    1C  N35CPC(X)        IFNE      0
109900950531     C     CPC(X)        ANDNE     BLPPDR
110000950531     C                   SETON                                        35
110100950531    1C                   END
110200950531     C*
110300950531     C* SE SERIE >< 99 CONTROLLO
110400060310     c                   select
110500101202     c*
110600060310    1C                   when      nrs(x)='99'
110700101202     c*
110800060310     c                   when      nrs(x)='SI'
110900060310     c                   if        blpnrs=0
111000060310     C                   SETON                                        35
111100060310     c                   endif
111200101202     c*
111300060310    1C                   other
111400060310     c                   movel     blpnrs        alfanrs           2
111500060310     C     NRS(X)        ifne      alfanrs
111600950531     C                   SETON                                        35
111700950531    1C                   END
111800060310    1C                   ENDsl
111900950531     C*
112000950531     C* SE IMMESSO CONTROLLO CODICE CLIENTE
112100950531    1C  N35CLI(X)        IFNE      0
112200950531    2C     BLPCCM        IFGT      0
112300950531    3C     CLI(X)        IFNE      BLPCCM
112400950531     C                   SETON                                        35
112500950531    3C                   END
112600950531     C*
112700950531   X2C                   ELSE
112800950531    3C     CLI(X)        IFNE      BLPKSC
112900950531     C                   SETON                                        35
113000950531    3C                   END
113100950531    2C                   END
113200950531    1C                   END
113300950531     C*
113400950531     C* SE IMMESSA CONTROLLO DATA RITIRO
113500950531    1C  N35DRT(X)        IFNE      0
113600950531     C     DRT(X)        ANDNE     BLPDRT
113700950531     C                   SETON                                        35
113800950531    1C                   END
113900950531     C* SE IMMESSO CONTROLLO SE MATTINO O POMERIGGIO
114000950531    1C  N35MOP(X)        IFNE      *BLANKS
114100950531     C     MOP(X)        ANDNE     BLPFPP
114200950531     C                   SETON                                        35
114300950531    1C                   END
114400950531     C* SE IMMESSO CONTROLLO NUMERO RITIRO
114500950531    1C  N35NRT(X)        IFNE      *BLANKS
114600950531     C                   MOVE      NRT(X)        COMNRT
114700950531     C     COMNRT        IFNE      BLPNRT
114800950531     C                   SETON                                        35
114900950531     C                   END
115000950531    1C                   END
115100970529    0C                   END
115200950531     C                   ENDSR
115300950531     C*
115400950531     C* CHAINO SPUNTE ENTRATA ----------------------------------------*
115500950531     C     SPUENT        BEGSR
115600950531     C* CAMPI PER LA STAMPA
115700950531     C                   CLEAR                   PRTNPG
115800980402     C                   CLEAR                   PRTSPG
115900950531     C                   CLEAR                   PRTNFV
116000950601     C                   CLEAR                   PRTDFV
116100980324     C                   CLEAR                   PRTZNC
116200980402     C                   CLEAR                   PRTFGS
116300950531     C*
116400950531     C                   Z-ADD     5             WNPG
116500980324     C                   EXSR      CHABRV
116600980324     C* SE NON HO TROVATO LA SPUNTA ENTRATA CHE VOLEVO VADO
116700130225     C*  AVANTI NELLA RICERCA --> cat 1
116800980324    1C     WTROV         IFEQ      ' '
116900980324     C     PRTDFV        ANDLT     WFEDFV
117000980324     C**
117100980324     C                   Z-ADD     1             WNPG
117200130225     c                   eval      wdefl=' '
117300980324     C                   EXSR      CHABRV
117400130225    1c                   endif
117500130225     c
117600130225     c* Se segnacollo mancante --> controllo se spuntato da altro terminal
117700130225    1C     WTROV         IFEQ      ' '
117800130225     C     PRTDFV        ANDeq     0
117900130225     C                   Z-ADD     5             WNPG
118000130225     C                   EXSR      CHABRV_altr
118100130225    2C     PRTDFV        IFLT      WFEDFV
118200130225     C                   Z-ADD     1             WNPG
118300130225     C                   EXSR      CHABRV_altr
118400130225    2c                   endif
118500130225    1c                   endif
118600130225     c
118700130225     c* Se non trovato in altro TFP controllo le altre categorie
118800130225    1C     WTROV         IFEQ      ' '
118900130225     C     PRTDFV        ANDeq     0
119000130225     c
119100980324     C                   Z-ADD     3             WNPG
119200980324     C                   EXSR      CHABRV
119300130225     c
119400130225    2C     PRTDFV        IFLT      WFEDFV
119500980324     C                   Z-ADD     2             WNPG
119600980324     C                   EXSR      CHABRV
119700130225     c
119800130225    3C     PRTDFV        IFLT      WFEDFV
119900130225     C                   Z-ADD     1             WNPG
120000130225     c                   eval      wdefl='S'
120100130225     C                   EXSR      CHABRV
120200130225     c                   eval      wdefl=' '
120300130225     c
120400130225    4C     PRTDFV        IFLT      WFEDFV
120500130225     C                   Z-ADD     4             WNPG
120600980324     C                   EXSR      CHABRV
120700980324    4C                   ENDIF
120800980324    3C                   ENDIF
120900130225    2C                   ENDIF
121000980324    1C                   ENDIF
121100980324     C**
121200950531     C* 01 ON  - SEGNACOLLO MANCANTE
121300980324    1C     WTROV         IFEQ      ' '
121400980324    2C     PRTDFV        IFEQ      0
121500950531     C                   SETON                                        01
121600961022     C*
121700961022     C* SCRIVO FILE EDBRV  CODICE 001 --> MANCA COLLO
121800980324    3C     EDICON        IFEQ      'S'
121900980415    3C     BLPDCM        ANDEQ     0
122000961025     C                   MOVEL     '001'         WSTS
122100961022     C                   EXSR      WRTEDI
122200980324    3C                   ENDIF
122300950531     C*
122400980324   X2C                   ELSE
122500950531     C*
122600980324     C* 02 ON  - SEGNACOLLO SPARATO SU ALTRO FOGLIO
122700980205     C**
122800950531     C                   SETON                                        02
122900961022     C*
123000980324     C* SCRIVO FILE EDBRV  CODICE 004 --> SE DATA =
123100980324     C*  ALTRO FOGLIO STESSA DATA
123200980324    3C     EDICON        IFEQ      'S'
123300980324     C     PRTDFV        ANDEQ     WFEDFV
123400980324     C                   MOVEL     '004'         WSTS
123500961022     C                   EXSR      WRTEDI
123600980324    3C                   ENDIF
123700980324     C* SCRIVO FILE EDBRV  CODICE 005 --> SE DATA <>
123800980324     C*  ALTRO FOGLIO ALTRA  DATA
123900980324    3C     EDICON        IFEQ      'S'
124000980324     C     PRTDFV        ANDNE     WFEDFV
124100980324     C                   MOVEL     '005'         WSTS
124200980324     C                   EXSR      WRTEDI
124300980324    3C                   ENDIF
124400130226     c
124500130226     c* Spuntato su altro foglio lo memorizzo solo se non è da scludere
124600130226     c*  (VIDMANCA ='S')
124700130226    3c     vidmanca      ifeq      'S'
124800130226     c                   setoff                                       02
124900130226    3C                   ENDIF
125000130226    2C                   ENDIF
125100961022     C* SCRIVO EDI
125200980324   X1C                   ELSE
125300980402     C* SPUNTA TRAOVATA SUL FOGLIO ENTRATA IN ESAME
125400961025    2C     EDICON        IFEQ      'S'
125500961022     C* SE RICHIESTI ANCHE I POSITIVI
125600980324     C* SE NON HO AVUTO ANOMALIA COLLO BOLLA E SPUNTA ENTRATA
125700961022     C                   CLEAR                   WSTS
125800961022     C                   EXSR      WRTEDI
125900961022    2C                   ENDIF
126000980402    1C                   ENDIF
126100980402     C*
126200980402     C* 03 ON  - ZONA SEGNACOLLO DIVERSA DA ZONA BOLLA
126300170407     c* dal momento che dal 04/16 aggiorniamo la bolla con la zona del
126400170407     c*  segnacollo, questa segnalazionew non serve più
126500170407    3C**N01BLPZNC        IFNE      PRTZNC
126600170407     C***                SETON                                        03
126700170407    3C***                ENDIF
126800950531     C*
126900980402    1C     *IN01         IFEQ      *ON
127000980402     C     *IN02         OREQ      *ON
127100950601     C     *IN03         OREQ      *ON
127200110715     ***C                   EXSR      STAMPA
127300110715     c                   exsr      sr_WriteWF
127400980402    1C                   ENDIF
127500950531     C                   ENDSR
127600980324     C*
127700980324     C* CHAINO PER VEDER SE ESISTE ALTRA SPUNTA ----------------------*
127800980324     C     CHABRV        BEGSR
127900061215     c                   z-add     simfel        ktfp
128000980324     C* SE HO TROVATO LA SPUNTA ENTRATA RELATIVA AL FOGLIO CHE STO
128100980324     C*  CONTROLLANDO IMPOSTO IL CAMPO WTROV=S
128200980324     C                   CLEAR                   WTROV
128300070123     C     KBRV          CHAIN     FNBRV009                           33
128400980408    1C     *IN33         DOWEQ     *OFF
128500980324     C* SE STO LEGGENDO LE SPUNTE ENTRATA VERIFICO SE SPARATO
128600980324     C*  SUL FOGLIO IN QUADRATURA ALTRIMENTI MEMORIZZO LA DATA E
128700980324     C*  NUMERO FOGLIO PIU' ALTA CHE C'E'
128800980324     C**
128900980324    2C     WNPG          IFEQ      5
129000980324     C     BRVNFV        ANDNE     COMNFV
129100980324     C     WNPG          ORNE      5
129200980402     C* ESCLUDO LE PISTOLE AUTOGENERATE
129300020909     C                   MOVEL     BRVNPS        W002A
129400980402     C     W002A         LOOKUP    NPS                                    34
129500980324    3C     *IN34         IFEQ      *OFF
129600980324     C* PRENDO LA DATA DEL FOGLIO
129700020909     C                   CLEAR                   FNLV53DS
129800000201     C                   Z-ADD     BRVNFV        D53NFV
129900980324     C                   MOVEL     BRVNPG        D53NPG
130000980326     C                   MOVEL     BRVFGS        D53FGS
130100980324     C                   CALL      'FNLV53R'
130200020909     C                   PARM                    FNLV53DS
130300980324     C* SE NON C'E' ERRORE MEMORIZZO LA DATA PIU' ALTA
130400980324    4C     D53ERR        IFEQ      ' '
130500980324     C     D53DFV        ANDGT     PRTDFV
130600130225     c* se cat 1 elaboto defluenza o non in base alla richiesta
130700130225    5c                   if        brvnpg<>1   or d53def=wdefl
130800130225     c
130900980324     C                   MOVEL     D53DFV        PRTDFV
131000000201     C                   Z-ADD     D53NFV        PRTNFV
131100980324     C                   MOVEL     D53NPG        PRTNPG
131200980402     C                   MOVEL     D53SPG        PRTSPG
131300980324     C                   MOVEL     BRVZNC        PRTZNC
131400980402     C                   MOVEL     BRVFGS        PRTFGS
131500130225    5C                   ENDIF
131600130225    4C                   ENDIF
131700980324    3C                   ENDIF
131800980324     C**
131900070123     C     KBRV          READE     FNBRV009                               33
132000980324   X2C                   ELSE
132100980324     C                   SETON                                        33
132200020909     C                   MOVEL     'S'           WTROV
132300980402     C                   MOVEL     WFEDFV        PRTDFV
132400000201     C                   Z-ADD     BRVNFV        PRTNFV
132500980402     C                   MOVEL     BRVNPG        PRTNPG
132600980402     C                   MOVEL     BRVZNC        PRTZNC
132700980402     C                   MOVEL     BRVFGS        PRTFGS
132800980324    2C                   ENDIF
132900980408    1C                   ENDDO
133000980324     C                   ENDSR
133100950531     C*
133200130222     C* CHAINO PER VEDER SE ESISTE ALTRA SPUNTA su altro terminal ----*
133300130222     C     CHABRV_altr   BEGSR
133400130222     C* SE HO TROVATO LA SPUNTA ENTRATA RELATIVA AL FOGLIO CHE STO
133500130222     C*  CONTROLLANDO IMPOSTO IL CAMPO WTROV=S
133600130222     C                   CLEAR                   WTROV
133700130222     C     KBRV_altr     CHAIN     FNBRV009                           33
133800130222    1C     *IN33         DOWEQ     *OFF
133900130222     C* SE STO LEGGENDO LE SPUNTE ENTRATA VERIFICO SE SPARATO
134000130222     C*  SUL FOGLIO IN QUADRATURA ALTRIMENTI MEMORIZZO LA DATA E
134100130222     C*  NUMERO FOGLIO PIU' ALTA CHE C'E'
134200130222    2c                   if        brvfle<>simfel
134300130222     C**
134400130222     C* ESCLUDO LE PISTOLE AUTOGENERATE
134500130222     C                   MOVEL     BRVNPS        W002A
134600130222     C     W002A         LOOKUP    NPS                                    34
134700130222    3C     *IN34         IFEQ      *OFF
134800130222     C* PRENDO LA DATA DEL FOGLIO
134900130222     C                   CLEAR                   FNLV53DS
135000130222     C                   Z-ADD     BRVNFV        D53NFV
135100130222     C                   MOVEL     BRVNPG        D53NPG
135200130222     C                   MOVEL     BRVFGS        D53FGS
135300130222     C                   CALL      'FNLV53R'
135400130222     C                   PARM                    FNLV53DS
135500130222     C* SE NON C'E' ERRORE MEMORIZZO LA DATA PIU' ALTA
135600130222    4C     D53ERR        IFEQ      ' '
135700130222     C     D53DFV        ANDGT     PRTDFV
135800130222     C                   MOVEL     D53DFV        PRTDFV
135900130222     C                   Z-ADD     D53NFV        PRTNFV
136000130222     C                   MOVEL     D53NPG        PRTNPG
136100130222     C                   MOVEL     D53SPG        PRTSPG
136200130222     C                   MOVEL     BRVZNC        PRTZNC
136300130222     C                   MOVEL     BRVFGS        PRTFGS
136400130222    4C                   ENDIF
136500130222    3C                   ENDIF
136600130222    2C                   ENDIF
136700130222     C**
136800130222     C     KBRV_altr     READE     FNBRV009                               33
136900130222    1C                   ENDDO
137000130222     C                   ENDSR
137100130222     c*
137200950531     C* CHAINO SPUNTE PARTENZA E IMP ---------------------------------*
137300950531     C     SPUPAR        BEGSR
137400980402     C* MEMORIZZO SPUNTA DA STAMPARE
137500980402     C                   CLEAR                   PRTNPS
137600980402     C                   CLEAR                   PRTNPG
137700980402     C                   CLEAR                   PRTSPG
137800950531     C                   CLEAR                   PRTNFV
137900950531     C                   CLEAR                   PRTDFV
138000980402     C                   CLEAR                   PRTZNC
138100980402     C                   CLEAR                   PRTFGS
138200120127     C                   CLEAR                   PRTlnp
138300120127     C                   CLEAR                   PRTnrs
138400980402     C* MEMORIZZO IL COLLO QUANDO E' UN FASULLO
138500950601     C                   CLEAR                   W88NFV
138600980402     C                   CLEAR                   W88NPS
138700980402     C**MEMORIZZO SPUNTA CON DATA = ALLA DATA DI ELABORAZIONE
138800980402     C                   CLEAR                   WDTNFV
138900980402     C                   CLEAR                   WDTNPS
139000980402     C                   CLEAR                   WDTNPG
139100030624     c                   Clear                   wdtLnp
139200030624     c                   Clear                   wdtNrs
139300950531     C*
139400980402     C                   CLEAR                   WDFV
139500950531     C                   Z-ADD     1             WNPG
139600101202     c*
139700061215     c* se la lnp che sto elaborando non è in £1, carico la sua
139800061215     c*  £1 e il suo terminal di partenza
139900061215     c     comlnp        lookup    l1                                     39
140000061215     C***
140100061215     c                   if        *in39
140200061215     c                   eval      ktfp=simfel
140300061215     c                   else
140400061215     C                   CLEAR                   TRUL06DS
140500061215     C                   MOVE      'PP'          D06COD
140600061215     C                   MOVEL     comlnp        D06KEY
140700061215     c                   move      'S'           d06key
140800061215     c                   movel     comdsp        d06drf
140900061215     C                   MOVEL     TRUL06DS      KPJBU
141000061215     C*
141100061215     C                   CALL      'TRUL06R'
141200061215     C                   PARM                    KPJBA
141300061215     C                   MOVEL     KPJBU         TRUL06DS
141400061215     c                   movel     lin(1)        ktfp
141500061215     c                   endif
141600101202     c*
141700980402    0C                   DO        2
141800101202      *
141900980402     C**
142000980402     C                   CLEAR                   WSNFV
142100070123     C     KBRV          CHAIN     FNBRV009                           33
142200950531     C*
142300950531    1C     *IN33         DOWEQ     *OFF
142400980402     C* PRENDO LA DATA
142500980402    2C     BRVNFV        IFNE      WSNFV
142600020909     C                   CLEAR                   FNLV53DS
142700000201     C                   Z-ADD     BRVNFV        D53NFV
142800980402     C                   MOVEL     BRVNPG        D53NPG
142900980402     C                   MOVEL     BRVFGS        D53FGS
143000980402     C                   CALL      'FNLV53R'
143100020909     C                   PARM                    FNLV53DS
143200980402    3C     D53ERR        IFEQ      ' '
143300980402     C                   MOVEL     D53DFV        WDFV
143400980423     C                   MOVEL     D53DEF        WDEF
143500980423     C                   MOVEL     D53SPG        WSPG
143600980402     C                   ELSE
143700980402     C                   CLEAR                   WDFV
143800980423     C                   CLEAR                   WDEF
143900980423     C                   CLEAR                   WSPG
144000980402    3C                   ENDIF
144100000201     C                   Z-ADD     BRVNFV        WSNFV
144200980402    2C                   ENDIF
144300980402     C**
144400980402     C** ESCLUDO SE NON E' SPUNTA PARTENZA O SPUNTA IMP
144500980423     C**  ESCLUDO ANCHE LE SPUNTE DI DEFLUENZA
144600980423    2C     WNPG          IFEQ      1
144700980423     C     WDEF          ANDEQ     ' '
144800980423    2C     WNPG          OREQ      3
144900980423     C     WSPG          ANDEQ     'P'
145000980402     C**
145100980402     C* SPUNTA CON DATA = ALLA DATA DI ELABORAZIONE
145200980402    3C     WDFV          IFEQ      COMDSP
145300980402     C     WDTNFV        ANDEQ     0
145400980402     C                   MOVEL     BRVNFV        WDTNFV
145500980402     C                   MOVEL     BRVNPS        WDTNPS
145600980402     C                   MOVEL     BRVNPG        WDTNPG
145700030624     c                   Eval      wdtLnp = BrvLnp
145800030624     c                   Eval      wdtNrs = BrvNrs
145900980402    3C                   ENDIF
146000950601     C*
146100980402     C* MEMORIZZO SPUNTA CON DATA PIU' ALTA
146200980402    3C     WDFV          IFGT      PRTDFV
146300980403     C                   MOVEL     D53SPG        PRTSPG
146400980402     C                   Z-ADD     BRVNPG        PRTNPG
146500980402     C                   Z-ADD     BRVFGS        PRTFGS
146600980402     C                   Z-ADD     BRVNFV        PRTNFV
146700980402     C                   Z-ADD     WDFV          PRTDFV
146800950606     C                   MOVEL     BRVZNC        PRTZNC
146900980402     C                   MOVEL     BRVNPS        PRTNPS
147000120127     c                   Eval      prtLnp = BrvLnp
147100120127     c                   Eval      prtNrs = BrvNrs
147200980402    3C                   ENDIF
147300980402    2C                   ENDIF
147400950531     C*
147500070123     C     KBRV          READE     FNBRV009                               33
147600950531    1C                   ENDDO
147700980402     C** SE NON TROVATA NELLE SPUNTE PARTENZA CERCO NELLE SPUNTE IMP
147800980402     C                   Z-ADD     3             WNPG
147900980402    AC                   ENDDO
148000950531     C*
148100980402     C* VEDO SE STAMPARE QUALCHE COSA
148200980402     C* 1) COLLO MANCANTE IN DATA RICHIESTA
148300980402    1C     WDTNFV        IFEQ      0
148400980402     C                   SETON                                        01
148500980402   X1C                   ELSE
148600980402     C* 2) SE LA SPUNTA TROVATA IN QUELLA DATA E' FASULLA
148700990609     C*    ANCHE SE NELLA TABELLA NON E' CONSIDERATA FASULLA CI METTIAM
148800990609     C*    ANCHE LE 90 CHE NON SONO MAI STATE SPARATE NEMMENO IN ENTRAT
148900030624      *    ma solo se non hanno lnp 102 e serie 02
149000030624      *    (bolle recapito contrassegni)
149100980402     C                   MOVEL     WDTNPS        W002A
149200980402     C     W002A         LOOKUP    NPF                                    21
149300980402    2C     *IN21         IFEQ      *ON
149400990609     C     W002A         OREQ      '90'
149500030624     c     wdtLnp        andne     102
149600060321     C     W002A         OREQ      '90'
149700030624     c     wdtNrs        andne     02
149800990609     C                   SETON                                        21
149900980402     C                   Z-ADD     WDTNFV        W88NFV
150000020909     C                   Z-ADD     WDTNPS        W88NPS
150100980402    2C                   ENDIF
150200980402    1C                   ENDIF
150300980402     C* 3) SE E' STATA TROVATA UNA SPUNTA PARTENZA O IMP CON DATA >
150400980402     C*    SOLO SE LA SPUNTA DEL GIORNO NON ERA UNA SPUNTA IMP
150500980402     C*    (NEL CASO E' TUTTO CORRETTO)
150600980402    1C     PRTNFV        IFGT      0
150700980403    2C     PRTDFV        IFNE      COMDSP
150800980403     C     WDTNPG        ANDNE     3
150900980402     C                   SETON                                            02
151000120127     c* controllo se per caso è spunta fasulla lo stesso
151100120127     C                   MOVEL     prtnPS        W002A
151200120127     C     W002A         LOOKUP    NPF                                    21
151300120127    2C     *IN21         IFEQ      *ON
151400120127     C     W002A         OREQ      '90'
151500120127     c     prtLnp        andne     102
151600120127     C     W002A         OREQ      '90'
151700120127     c     prtNrs        andne     02
151800120127     C                   SETON                                        2140
151900120127     C                   Z-ADD     prtNFV        W88NFV
152000120127     C                   Z-ADD     prtNPS        W88NPS
152100120127    2C                   ENDIF
152200980402    2C                   ENDIF
152300980402     C* 4) SE ZONA BOLLA <> DA ZONA SEGNACOLLO
152400170407    2C***  PRTZNC        IFNE      BLPZNC
152500170407     C***                SETON                                            03
152600170407    2C***                ENDIF
152700980402    1C                   ENDIF
152800980402     C**
152900980402     C* 5) SE IL COLLO E' STATO AGGIORNATO CON PISTOLA FASULLA
153000980402     C*    MA LA SPUNTA TROVATA NELLA STESSA DATA E' REALE
153100980402    1C     BLTDFV        IFGT      0
153200980402     C                   MOVEL     BLTNPP        W002A
153300980402     C     W002A         LOOKUP    NPF                                    33
153400980402    2C     *IN33         IFEQ      *ON
153500990609     C     W002A         OREQ      '90'
153600980402    3C     PRTDFV        IFGT      0
153700980402     C     PRTNPS        ANDNE     BLTNPP
153800980403     C     PRTNPG        ANDEQ     1
153900980402     C                   SETON                                        27
154000980402    3C                   ENDIF
154100980402    2C                   ENDIF
154200980402    1C                   ENDIF
154300980402     C**
154400950601     C* SE COLLO MANCANTE O SPUNTA 88 DICO SE E' STATO SPARATO IN ENTR
154500980402     C   01
154600980402    1COR 21BLTDSE        IFGT      0
154700950601     C                   SETON                                        11
154800980402    1C                   ENDIF
154900000621      *
155000000621      * DICO SE E'STATO SPARATO IN ARRIVO - - - - - - - - - - - - - -
155100000621      *
155200160517    1c                   if        *in01 or *in21
155300000621     C* CERCO L'ANOMALIA 60 O 61
155400160517     C**                 Z-ADD     0             RIPROW
155500160517     C**   RIPROV        TAG
155600020909      *    =================
155700160517    2C**   RIPROW        IFEQ      0
155800160517     C**                 Z-ADD     60            COMCAA
155900160517    2C**                 ENDIF
156000160517    2C**   RIPROW        IFEQ      1
156100160517     C**                 Z-ADD     61            COMCAA
156200160517    2C**                 ENDIF
156300000621      *
156400160517IF  2C**   RIPROW        IFLE      1
156500160517      **
156600160517     C                   CLEAR                   DAMNPS
156700160517     C                   CLEAR                   ANMDAO
156800160517     C                   CLEAR                   DESANP
156900160517     C**   KANM2         CHAIN     FNANM02L                           31
157000160517    3C**   *IN31         IFEQ      *OFF
157100160517     C**                 MOVEL     ANMNPS        DAMNPS
157200160517    4C**   RIPROW        IFEQ      0
157300160517     C**   'ARRIVATO'    CAT       ' il':0       DESANP
157400160517     c**                 seton                                        17
157500160517     C**                 ELSE
157600160517     C**   'DISGUIDA'    CAT       'TO ':0       DESANP
157700160517    4C**                 ENDIF
157800160517     C**                 ELSE
157900160517     C**                 ADD       1             RIPROW
158000160517     C**                 GOTO      RIPROV
158100160517      **                 ----------------
158200160517    3C**                 ENDIF
158300160517     C*
158400160517    2C**                 ENDIF
158500050930     c
158600050930    2c                   if        anmdao=0
158700050930     c     kart          chain     fnart01l
158800050930    3c                   if        %found(fnart01l)
158900050930    4c                   if        artdam>0
159000050930     C                   MOVEL     artnap        DAMNPS
159100110718     C     'ARRIVATO'    CAT       ' il':0       DESANP
159200061215     c                   seton                                        17
159300050930     c                   movel     artdam        anmdao
159400050930   x4c                   else
159500050930    5c                   if        artdet>0
159600050930     c                   movel     artpet        w002a
159700050930     c     w002a         lookup    nps                                    31
159800050930    6c                   if        not *in31
159900050930     c     kblp2         chain     fnarb01l
160000050930    7c                   if        %found(fnarb01l) and arbflp=artflp
160100110718     C     'ARRIVATO'    CAT       ' il':0       DESANP
160200061215     c                   seton                                        17
160300050930     c                   else
160400110718     C     'DISGUIDA'    CAT       'TO ':0       DESANP
160500050930    7c                   endif
160600061215     c                   movel     artpet        damnps
160700050930     c                   movel     artdet        anmdao
160800050930    6c                   endif
160900050930     c                   endif
161000101202    5c*
161100050930    4c                   endif
161200050930    3c                   endif
161300050930    3c                   endif
161400101202     c*
161500000621     C*
161600000621     C     ANMDAO        IFGT      0
161700000621     C                   MOVEL     ANMDAO        DATA
161800000314     C                   Z-ADD     MM            DAMDFV
161900000314     C                   MOVEL     GG            DAMDFV
162000000320     C     DAMNPS        IFNE      *BLANKS
162100000320     C                   SETON                                        29
162200000320     C                   ENDIF
162300000314     C                   SETON                                        12
162400000621     C                   ENDIF
162500000621     C*
162600070110     C                   ENDif
162700070110     c*  se 11 acceso non devo dare segnalazione di collo arrivato
162800070110     c                   if        *in11
162900070110     c                   setoff                                       2912
163000070110     c                   endif
163100000621      *
163200061215      * Solo per errori 01 o 21, se trovato collo in arrivo
163300061215      *  e non è da stampare, non eseguo la stampa
163400101202     c*
163500070110     c   25              if        vidsn='S' and *in17  and
163600061215     c                             (*in21 or *in01)
163700070110     c                   setoff                                       012111
163800061215     c                   endif
163900061215      *
164000000621      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
164100000621      *
164200980402    1C     *IN01         IFEQ      *ON
164300980402     C     *IN02         OREQ      *ON
164400980402     C     *IN03         OREQ      *ON
164500980402     C     *IN21         OREQ      *ON
164600980402     C     *IN27         OREQ      *ON
164700110715     ***C                   EXSR      STAMPA
164800110715     c                   exsr      sr_WriteWF
164900950531    1C                   ENDIF
165000950531     C                   ENDSR
165100950601     C*
165200950605     C* CONTROLLO LE SPUNTE DEL FOGLIO DA ESAMINARE -----------------*
165300950601     C     LEGBRV        BEGSR
165400950601     C                   SETOFF                                       010203
165500950601     C                   SETOFF                                       040522
165600950605     C                   Z-ADD     1             Y
165700950605     C*
165800070109    0C     LP4(Y)        DOWGT     0
165900070123     C     KFVV3         SETLL     FNBRV000
166000070123     C     KFVV3         READE     FNBRV000                               33
166100950601     C*
166200950605    1C     *IN33         DOWEQ     *OFF
166300950601     C* ESCLUDO LE SPUNTE 88 E 90
166400950601   1AC     BRVNPS        IFNE      88
166500950601     C     BRVNPS        ANDNE     90
166600970506     C     BRVNPS        ANDNE     89
166700050919     c* elaboro solo le spunte con brvatr = blanks (esclusione spunte di
166800050919     c* colli non miei)
166900130225     c* elaboro però se si tratta di fls/nrs non di simfel ma che puiò
167000130225     c*     partire da altri TFP
167100130225     c***                clear                   altrotfp          1
167200130225    2C***  BRVNRS        ifgt      0
167300130225     C***                MOVE      BRVNRS        WLPSR
167400130225     C***                MOVEL     BRVLNP        WLPSR
167500130225     C*
167600130225     C***                Z-ADD     1             Z
167700130225     C***  WLPSR         LOOKUP    FLSNRS(Z)                              36
167800130225    3c***                if        *in36
167900130225     c***                eval      altrotfp='S'
168000130225    3c***                endif
168100130225    2c***                endif
168200130225     c
168300130225
168400130225   1Bc     Brvatr        ifeq      *blanks
168500950601     C*
168600950601     C     KBLT2         CHAIN     FNBLT27L                           34
168700950601     C  N34KBLP2         CHAIN     FNBLP001                           34
168800020909      *
168900020909      * Se trovo la bolla, controllo il P.O. di partenza nella £1:
169000130225      * se non è in £1 passo ad altra spunta
169100130225      *  tranne si se  tratta di bolla autorizzata a partire da + TFP
169200020909     C                   eval      $ok£1 = *off
169300020909    2C                   if        not *in34
169400070109     C     BLPlnp        lookup    LIN                                    10
169500020909     C   10              eval      $ok£1 = *on
169600020909    2C                   endif
169700020909     C*
169800950601     C* M A N C A   B O L L A
169900950601    2C     *IN34         IFEQ      *ON
170000950601     C*
170100950601     C* 18 OFF - NON E' RICHIESTA LA STAMPA ANOMALIE MANCA BOLLA
170200950703    3C     *IN18         IFEQ      *OFF
170300950601     C                   SETOFF                                       35
170400950601     C                   SETON                                        10
170500950601     C*
170600950601     C                   Z-ADD     1             X
170700950601     C* NEL CASO IN CUI SIANO STATE IMMESSE PIU' SELEZIONI AVENTI
170800950601     C* LA STESSA LINEA DI PARTENZA, RIESEGUO LA LOKUP
170900950605    4C     *IN10         DOWEQ     *ON
171000950601     C*
171100950601     C     BRVLNP        LOOKUP    SKI(X)                                 10
171200950703     C   10              EXSR      SERIE
171300950601     C                   ADD       1             X
171400950703    4C                   ENDDO
171500950703     C*
171600950703     C* SE NON TROVATO, LEGGO ANCHE LA SKIERA FAS
171700961025     C* SOLO SE LA SERIE E' > 0
171800961025     C* VERIFICO SE SI TRATTA DI LNP NON IN L1 MA CONFERMABILE SUL MIO
171900961025     C*  AS --> IN QUESTO CASO USO LNP CHE CONFERMA E NON LNP COLLO
172000950703    4C     *IN35         IFEQ      *OFF
172100950703     C     BRVNRS        ANDGT     0
172200020909     C                   MOVE      BRVNRS        WLPSR
172300960610     C                   MOVEL     BRVLNP        WLPSR
172400950703     C*
172500020909     C                   Z-ADD     1             Z
172600960610     C     WLPSR         LOOKUP    FAS(Z)                                 36
172700960610     C   36Z             OCCUR     DSLAS
172800950703     C*
172900020909     C                   Z-ADD     1             YY
173000960610     C     *IN36         DOWEQ     *ON
173100960619     C     LAS(YY)       ANDGT     *ZEROS
173200950703     C                   Z-ADD     1             X
173300960619     C                   SETON                                        10
173400960610    6C     *IN10         DOWEQ     *ON
173500950703     C*
173600020909     C                   MOVE      LAS(YY)       W0030
173700960619     C     W0030         LOOKUP    SKI(X)                                 10
173800950703     C   10              EXSR      SERIE
173900950703     C                   ADD       1             X
174000950703    6C                   ENDDO
174100950703     C*
174200950703     C   35              SETOFF                                       36
174300960610     C                   ADD       1             YY
174400960610     C                   ENDDO
174500950703    4C                   ENDIF
174600950703     C*
174700950703    3C                   ENDIF
174800950601     C*
174900950601     C* MANCA BOLLA
175000950703    3C     *IN18         IFEQ      *ON
175100961025     C* SE MANCA BOLLA DI LNP NON IN L1 CONTROLLO CHE NON ESISTA
175200950703     C*  UNA SPUNTA PARTENZA DI ALTRO AS: SE ESISTE NON DO LA
175300950703     C*  SEGNALAZIONE
175400961025     C     *IN35         OREQ      *ON
175500050920     c                   clear                   wmio              1
175600150623     c                   clear                   wbsptp            1
175700150623    2c                   if        *in18 = *on  and brvnrs = 0
175800050920     c     brvlnp        lookup    l1                                     31
175900150623    3c                   if        *in31 = *off
176000150623     c* verifico se lnp che può partire da me
176100150623     c     brvlnp        chain     fibsp01l
176200150623    4c                   if        %found(fibsp01l) and bspfpa='S'
176300150623     c                             and %lookup(brvpes:sbsp)>0
176400150623     c                   eval      wbsptp='S'
176500150623    4c                   endif
176600150623     c
176700150623    4c                   if        not %found(fibsp01l) or
176800150623     c                             wbsptp<>'S'
176900050920     c                   eval      wmio = 'N'
177000150623    4c                   endif
177100150623    3c                   endif
177200150623    2c                   endif
177300050919     c*
177400050920     c* Considerazioni per colli con serie:
177500050919     c* se a fronte della spunta è presente un disguido non stampo
177600050920     c* (collo non mio)
177700050920    4c                   if        brvnrs > 0
177800050920     c                   z-add     55            comcaa
177900050920     c     leganm        tag
178000050919     C     KANM          SETLL     FNANM02L                               31
178100050920    5c                   do        *hival
178200050919     C     KANM          reade     FNANM02L                               31
178300050920    6c                   if        %eof(fnanm02l)  or wmio = 'N'
178400050919     c                   leave
178500050920    6c                   endif
178600050920    6c                   if        anmfle = brvfgs
178700050920     c                   eval      wmio  = 'N'
178800050920    6c                   endif
178900050920    5c                   enddo
179000050920    5c                   if        comcaa = 55 and wmio = ' '
179100050920     c                   z-add     56            comcaa
179200050920     c                   goto      leganm
179300050920    5c                   endif
179400050920     C* Se non trovata anomalia di disguido cerco se esiste spunta partenza
179500050920     c* di altro p.o. in data <= della mia spunta. La spunta partenza NON
179600050920     c* deve essere di defluenza, transito, disguido e deve risultare trasme
179700050920     c* ssa. Se trovo tale spunta significa che il collo non è mio e quindi
179800050920     c* non stampo il manca bolla
179900050920    5c     wmio          ifeq      ' '
180000950703     C                   Z-ADD     BRVNPS        SAVNPS
180100950707     C                   Z-ADD     1             PARNPG
180200070123     C     KBRV2         setll     FNBRV009
180300050920    6C                   DO        *hival
180400070123     C     KBRV2         reade     FNBRV009
180500070123    7c                   if        %eof(fnbrv09l)
180600050920     c                   leave
180700050920    7c                   endif
180800070109    7c                   if        brvfle = ktfp  or brvatr <> *blanks
180900050920     c                   iter
181000050920    7c                   endif
181100050920     c* verifico se spunta di defluenza
181200050920     C                   CLEAR                   FNLV53DS
181300050920     C                   Z-ADD     BRVNFV        D53NFV
181400050920     C                   MOVEL     BRVNPG        D53NPG
181500050920     C                   MOVEL     BRVFGS        D53FGS
181600050920     C                   CALL      'FNLV53R'
181700050920     C                   PARM                    FNLV53DS
181800050920    7C     D53ERR        IFEQ      ' '
181900050920     c     d53def        andne     'S'
182000050920    8c                   if        d53dfv < wdatafv or (d53dfv=wdatafv
182100050920     c                             and brvftr <> *blanks)
182200050920     c                   eval      wmio = 'N'
182300050920    8C                   endif
182400050920    7c                   endif
182500050920    6c                   enddo
182600050920     C                   Z-ADD     SAVNPS        BRVNPS
182700050920    5c                   endif
182800050920    4c                   endif
182900950703     C*
183000050920     C* Stampa manca bolla
183100050920    4C     wmio          IFEQ      ' '
183200950703     C                   SETON                                        05
183300110715     ***C                   EXSR      STAMPA
183400110715     c                   exsr      sr_WriteWF
183500961025     C* SCRIVO FILE EDBRV  CODICE 001 --> MANCA COLLO
183600961025     C     EDICON        IFEQ      'S'
183700961025     C                   MOVEL     '002'         WSTS
183800961025     C                   EXSR      WRTEDI
183900961025     C                   ENDIF
184000950703    4C                   ENDIF
184100950703     C*
184200950703    3C                   ENDIF
184300950601     C***
184400950601   X2C                   ELSE
184500991022     C***
184600020909     C* se il P.O. di partenza (BLPLNP) non è in £1 :
184700020909     C*  NON STAMPO NULLA E NON FACCIO NESSUN CONTROLLO
184800101202   2BC     $ok£1         ifeq      *on
184900950605     C*
185000950605     C* 35 ON - MI INDICHERA' SE DEVO STAMPARE L'ANOMALIA
185100950605     C                   SETOFF                                       35
185200950601     C*
185300950601     C* SE QUADRATURA SPUNTA PARTENZA CONTROLLO SE BOLLA NON BORDERIZZ
185400950605    3C     *IN25         IFEQ      *ON
185500950601     C     BLPDBR        ANDEQ     0
185600950601     C                   SETON                                        22
185700950605     C   18              SETON                                        35
185800950605     C  N18              SETON                                        10
185900950605    3C                   ENDIF
186000950605     C*
186100950605     C* 25 OFF - CONTROLLO SE ECCEDENTE DI BOLLA NON SELEZIONATA
186200950605     C* 25 ON  - 22 ON - BOLLA NON BORDERIZZATA: VERIFICO LE RICHIESTE
186300950605     C*          PER DECIDERE SE STAMPARE O MENO.
186400950605     C*          SALTO IL CONTROLLO DELLA DATA
186500950601     C*
186600950605   2AC     *IN25         IFEQ      *OFF
186700950605     C     *IN22         OREQ      *ON
186800950605     C     *IN18         ANDEQ     *OFF
186900950605     C**
187000950601     C* CONTROLLO DATA SPEDIZIONE
187100950605     C**
187200980416    3C  N25BLPDSP        IFLT      COMDSP
187300980416     C     BLPDSP        ORGT      WDSA
187400961025     C* STAMPA COLLI ECCEDENTI DI SELEZIONI NON RICHIESTE --> SI 18ON
187500961025     C*  STAMPO SEMPRE LA ECCEDENZA PER DATA
187600950601     C   18              SETON                                        35
187700961025     C*
187800961025     C* STAMPA COLLI ECCEDENTI DI SELEZIONI NON RICHIESTE --> NO 18OFF
187900961025     C*  STAMPO L'ECCEDENZA PER DATA SOLO SE RIENTRA NELLE SELEZIONI
188000961025     C  N18              SETON                                        10
188100961025     C**
188200950601   X3C                   ELSE
188300950601     C  N18              SETOFF                                       10
188400950601    3C                   END
188500950601     C*
188600950601     C                   Z-ADD     1             X
188700950605     C   18              SETON                                        10
188800950601     C*
188900961025     C* IL LOOP 3 -->   N35 *IN10 DOWEQ*ON
189000961025     C*  LO ESEGUE SOLTANTO NEI SEGUENTI 2 CASI:
189100961025     C*  18 OFF - STAMPA LA ECCEDENZA DI DATA SPEDIZIONE <> SE LA BOLLA
189200961025     C*           RIENTRA NELLE SELEZIONI SOTTO EFFETTUATE
189300961025     C*  18 ON  - STAMPA LA ECCEDENZA SE LA DATA SPEDIZONE VA BENE
189400961025     C*           MA COMUNQUE NON E' STATA SELEZIONATA PER LE ALTRE RIC
189500961025     C*
189600950601     C* NEL CASO IN CUI SIANO STATE IMMESSE PIU' SELEZIONI AVENTI
189700950601     C* LA STESSA LINEA DI PARTENZA, RIESEGUO LA LOKUP
189800950605    3C  N35*IN10         DOWEQ     *ON
189900950601     C*
190000950601     C* CONTROLLO LINEA DI PARTENZA
190100950601     C     BLPLNP        LOOKUP    SKI(X)                                 10
190200950601     C*
190300950601     C* 18 ON - E' RICHIESTA LA STAMPA ANOMALIE DEI COLLI ECCEDENTI
190400950605    4C     *IN18         IFEQ      *ON
190500950601     C*
190600980115     C  N10              SETON                                        35
190700950605     C   10              EXSR      SELEZ
190800950601     C*
190900950601     C  N35              SETOFF                                       10
191000020909     C   35              Z-ADD     X             XX
191100950601     C   35
191200950601     CAN 10              SETOFF                                       35
191300950601     C*
191400950601   X4C                   ELSE
191500950601     C* 18 OFF - NON E' RICHIESTA LA STAMPA ANOMALIE DEI COLLI ECCEDENTI
191600950601     C*
191700950605     C* SE NON IMMESSA NESSUNA SELEZIONE --> STAMPA IL COLLO ECCEDENTE
191800950605    5C     *IN10         IFEQ      *ON
191900950605     C*
192000950601     C                   SETOFF                                       13
192100950601     C* SE IMMESSO CONTROLLO CODICE CLIENTE
192200970529    6C     X             IFLE      12
192300970529     C     CLI(X)        ANDNE     0
192400950601     C                   SETON                                        13
192500970529    7C     BLPCCM        IFGT      0
192600970529    8C     CLI(X)        IFEQ      BLPCCM
192700950601     C                   SETON                                        35
192800970529    8C                   END
192900950601     C*
193000970529   X7C                   ELSE
193100970529    8C     CLI(X)        IFEQ      BLPKSC
193200950601     C                   SETON                                        35
193300970529    8C                   END
193400970529    7C                   END
193500020909     C   35              Z-ADD     X             XX
193600950601    6C                   END
193700950601     C*
193800950601     C* SE SERIE >< 99 CONTROLLO
193900950601     C   13
194000950601     CAN 35
194100970529    6CORN13X             IFLE      12
194200060310     C     NRS(X)        ANDNE     '99'
194300950601     C                   SETON                                        13
194400060310     c                   if        nrs(x)='SI'
194500060310     c
194600060310     C                   IF        BLPNRS>0
194700060310     C                   SETON                                        35
194800060310     C                   ELSE
194900060310     C                   SETOFF                                       35
195000060310     C                   END
195100060310     c
195200060310     c                   else
195300060310     c                   movel     blpnrs        alfanrs           2
195400060310     C     NRS(X)        IFEQ      alfanrs
195500950601     C                   SETON                                        35
195600950601     C                   ELSE
195700950601     C                   SETOFF                                       35
195800950601     C                   END
195900950601    6C                   END
196000060310    6C                   END
196100950601     C*
196200950601     C* SE IMMESSO CONTROLLO CODICE PADRONCINO
196300950601     C   13
196400950601     CAN 35
196500970529    6CORN13X             IFLE      12
196600970529     C     CPC(X)        ANDNE     0
196700950601     C                   SETON                                        13
196800950601     C     CPC(X)        IFEQ      BLPPDR
196900950601     C                   SETON                                        35
197000950601     C                   ELSE
197100950601     C                   SETOFF                                       35
197200950601     C                   END
197300950601    6C                   END
197400950601     C*
197500950601     C* SE IMMESSA CONTROLLO DATA RITIRO
197600950601     C   13
197700950601     CAN 35
197800970529    6CORN13X             IFLE      12
197900970529     C     DRT(X)        ANDNE     0
198000950601     C                   SETON                                        13
198100950601     C     DRT(X)        IFEQ      BLPDRT
198200950601     C                   SETON                                        35
198300950601     C                   ELSE
198400950601     C                   SETOFF                                       35
198500950601     C                   END
198600950601    6C                   END
198700950601     C* SE IMMESSO CONTROLLO SE MATTINO O POMERIGGIO
198800950601     C   13
198900950601     CAN 35
199000970529    6CORN13X             IFLE      12
199100970529    6C     MOP(X)        ANDNE     *BLANKS
199200950601     C                   SETON                                        13
199300950601     C     MOP(X)        IFEQ      BLPFPP
199400950601     C                   SETON                                        35
199500950601     C                   ELSE
199600950601     C                   SETOFF                                       35
199700950601     C                   END
199800950601    6C                   END
199900950601     C* SE IMMESSO CONTROLLO NUMERO RITIRO
200000950601     C   13
200100950601     CAN 35
200200970529    6CORN13X             IFLE      12
200300970529    6C     NRT(X)        ANDNE     *BLANKS
200400950601     C                   SETON                                        13
200500950601     C                   MOVE      NRT(X)        COMNRT
200600950601     C     COMNRT        IFEQ      BLPNRT
200700950601     C                   SETON                                        35
200800950601     C                   ELSE
200900950601     C                   SETOFF                                       35
201000950601     C                   END
201100950601    6C                   END
201200950601     C*
201300950601     C  N13              SETON                                        35
201400950601    5C                   END
201500950601     C*
201600950601     C   35              SETOFF                                       10
201700950601    4C                   END
201800950601     C*
201900950601     C                   ADD       1             X
202000950605    3C                   ENDDO
202100950605   2AC                   ENDIF
202200950601     C*
202300950605    3C     *IN35         IFEQ      *ON
202400950605     C* 25 OFF -SEGNACOLLO ECCEDENTE DI BOLLA NON SELEZIONATA
202500950605     C*         ACCENDO     LO 04
202600950605     C* 25 ON  -SEGNACOLLO ECCEDENTE DI BOLLA NON BORDERIZZATA
202700950605     C*         GIA' ACCESO IL 22
202800961025     C     *IN25         IFEQ      *OFF
202900961025     C                   SETON                                        04
203000961028     C     EDICON        IFEQ      'S'
203100961025     C                   MOVEL     '003'         WSTS
203200961025     C                   EXSR      WRTEDI
203300961025     C                   ENDIF
203400961025     C                   ENDIF
203500950601     C*
203600950601     C* 03 ON  - ZONA SEGNACOLLO DIVERSA DA ZONA BOLLA
203700170407     C***  BLPZNC        IFNE      BRVZNC
203800170407     C***                SETON                                        03
203900170407     C***                ENDIF
204000950601     C*
204100110715     ***C                   EXSR      STAMPA
204200110715     c                   exsr      sr_WriteWF
204300950601    3C                   ENDIF
204400950601     C*
204500991022   2BC                   ENDIF
204600991022    2C                   ENDIF
204700130225   1BC                   ENDIF
204800130225   1AC                   ENDIF
204900950601     C*
205000950601     C                   SETOFF                                       040503
205100950601     C                   SETOFF                                       103522
205200070123     C     KFVV3         READE     FNBRV000                               33
205300950601    1C                   ENDDO
205400950605     C*
205500950605     C                   ADD       1             Y
205600950605    0C                   ENDDO
205700950601     C                   ENDSR
205800950703     C*
205900950703     C* PREPARA LA SKIERA DI LETTURA DELLE LNP SU FNBRV---------------*
206000950703     C     PRELP3        BEGSR
206100070109     C                   CLEAR                   LP4
206200950703     C**
206300070109     C* SE RICHIESTA tutta la stampa dei colli eccedenti
206400070109     C*  accodo tutta la FAS caricata (di fatto di simfel come prima)
206500950703    1C     *IN18         IFEQ      *ON
206600070109     c                   movea     l1            lp4
206700950703     C*
206800950703     C                   Z-ADD     1             X
206900070109     C     000           LOOKUP    LP4(X)                                 32
207000950703     C* ACCODO LA SKIERA FAS DELLE ALTRE FILIALI GESTITE
207100950703     C*  COME LNP SEGNACOLLO
207200950703    3C     *IN32         IFEQ      *ON
207300950703     C                   Z-ADD     1             Y
207400950703     C*
207500950703    4C     FAS(Y)        DOWGT     0
207600020909     C                   MOVEL     FAS(Y)        W0030
207700070109     C     W0030         LOOKUP    LP4                                    32
207800950703    5C     *IN32         IFEQ      *OFF
207900070109     C                   z-add     W0030         LP4(X)
208000950703     C                   ADD       1             X
208100950703    5C                   ENDIF
208200950703     C*
208300950703     C                   ADD       1             Y
208400950703    4C                   ENDDO
208500950703    3C                   ENDIF
208600950703     C*
208700950703   X1C                   ELSE
208800950703     C**
208900070109     C* SE richiesti solo colli eccedenti delle lnp immesse
209000070109     C* DELLA SKIERA FAS CARICO SOLO QUELLE RELATIVE ALLE LINEE
209100070109     C*  ELABORATE
209200070109     c                   move      lp3(ee)       lp4(1)
209300070109     C                   Z-ADD     2             Y
209400070109     c                   eval      zz=ee+1
209500070109     c* Se non è l'ulitmo record letto, cerco altre lnp selezionate
209600070109     c*  per lo stesso terminal di partenza
209700070109     c                   if        zz<=99
209800070109     c                   movel     lp3(zz)       elatfp            3 0
209900070109     c
210000070109     c                   dow       zz<=99 and elatfp=ktfp
210100070109     c                   move      lp3(zz)       lp4(Y)
210200070109     c                   add       1             y
210300070109     c
210400070109     c                   add       1             zz
210500070109     c
210600070109     c                   if        zz<=99
210700070109     c                   movel     lp3(zz)       elatfp
210800070109     c                   endif
210900070109     c                   enddo
211000070109     c
211100070109     c                   endif
211200070109     c
211300070109     c
211400950703     C* DELLA SKIERA FAS CARICO SOLO QUELLE RELATIVE ALLE LINEE
211500950703     C*  ELABORATE
211600950703     C                   Z-ADD     1             X
211700070109     C     000           LOOKUP    LP4(X)                                 32
211800950703     C* ACCODO LA SKIERA FAS DELLE ALTRE FILIALI GESTITE
211900950703     C*  COME LNP SEGNACOLLO
212000950703    3C     *IN32         IFEQ      *ON
212100950703     C                   Z-ADD     1             Y
212200950703     C*
212300960610    4C     FAS(Y)        DOWGT     *ZEROS
212400080109     C     Y             ANDLE     $MaxElem
212500960610     C     Y             OCCUR     DSLAS
212600020909     C                   Z-ADD     1             YY
212700960610    5C     YY            DOWLE     87
212800960610     C     LAS(YY)       ANDGT     *ZEROS
212900960619     C                   MOVE      LAS(YY)       W0030
213000070109     C     W0030         LOOKUP    LP4                                    32
213100960610    6C     *IN32         IFEQ      *ON
213200020909     C                   MOVEL     FAS(Y)        W0030
213300070109     C     W0030         LOOKUP    LP4                                    32
213400070109     C  N32              z-add     W0030         LP4(X)
213500960610     C  N32              ADD       1             X
213600960610    6C                   END
213700960610     C                   ADD       1             YY
213800960610    5C                   ENDDO
213900960610     C                   ADD       1             Y
214000960610    4C                   ENDDO
214100960610     C*
214200950703    3C                   ENDIF
214300950703     C*
214400950703    1C                   ENDIF
214500950703     C*
214600950703     C                   ENDSR
214700950703     C*
214800070109     C* PREPARA LA SKIERA DI LETTURA DELLE LNP + tfp -----------------*
214900070109     C     PRETFP        BEGSR
215000070109     C                   CLEAR                   LP3
215100070109     C**
215200070109     C* SE RICHIESTA LA STAMPA DEI COLLI ECCEDENTI--> LEGGO TUTTE LE
215300070109     C*  SPUNTE
215400070109     c* carico la £1 perchè nel cotrollo entrata non posso
215500070109     c*  richidere lnp non dell'area (18 on solo se lnp dell'area)
215600070109    1C     *IN18         IFEQ      *ON
215700101202     c*
215800070109     C                   Z-ADD     1             Y
215900070109    2C                   DOW       L1(Y)>0
216000070109     C                   MOVE      L1(Y)         LP3(Y)
216100070109     C                   MOVEL     SIMFEL        LP3(Y)
216200070109     c                   add       1             y
216300070109    2c                   enddo
216400070109     C*
216500070109   X1C                   ELSE
216600070109     C**
216700070109     C* SE NON RICHIESTA --> LEGGO SOLO LE SPUNTE DELLE LNP RICHIESTE
216800070109     C*
216900070109     C                   Z-ADD     1             Y
217000070109    2C                   DOW       Lp2(Y)>0
217100070109     c* Cerco terminal di partenza
217200070109     c                   clear                   fnlv55ds
217300070109     c                   eval      d55tpt='P'
217400070109     c                   movel     lp2(y)        d55lin
217500070109     c                   movel     comdsp        d55drf
217600070109     c                   call      'FNLV55R'
217700070109     c                   parm                    fnlv55ds
217800070109     c                   movel     d55tfp        lp3(Y)
217900070109     C                   MOVE      LP2(Y)        LP3(Y)
218000070109     c                   add       1             y
218100070109     c                   enddo
218200101202     c*
218300070109     c                   endif
218400101202     c*
218500070109     C                   ENDSR
218600950703     C* CONTROLLO LA SERIE SE E' DELLE SELEZIONI EFFETTUATE-----------*
218700950703     C     SERIE         BEGSR
218800950703     C*
218900950703     C* SE SERIE >< 99 CONTROLLO
219000060310    1C     X             IFLE      12
219100060310     C     NRS(X)        ANDNE     '99'
219200060310    2c                   if        nrs(x)='SI'
219300060310    3c                   if        brvnrs>0
219400060310     C                   SETON                                        35
219500060310    3C                   END
219600060310   x2c                   else
219700101202     c*
219800060310     c                   movel     brvnrs        alfanrs           2
219900060310    3C     NRS(X)        IFEQ      alfanrs
220000950703     C                   SETON                                        35
220100060310    3C                   END
220200060310    2C                   END
220300060310   X1C                   ELSE
220400950703     C                   SETON                                        35
220500060310    1C                   END
220600950703     C*
220700111117     c* Se la serie va bene --> se c'e' cliente controllo anche range di
220800111117     c*  segnacolli
220900111117     c                   if        *in35 and brvnrs>0 and cli(x)>0
221000111117     c                   clear                   trulnrsds
221100111117     c                   eval      inrsccm=cli(X)
221200111117     c                   eval      inrsnrs=brvnrs
221300111117     c                   eval      inrsfls=brvlnp
221400111117     c                   eval      inrsncd=brvnsc
221500111117     c                   call      'TRULNRSR'
221600111117     c                   parm                    trulnrsds
221700111117     c                   if        onrserr<>' '
221800111117     c                   setoff                                       35
221900111117     c                   endif
222000111117     c
222100111117     c                   endif
222200950703     C*
222300950703     C   35              SETOFF                                       10
222400950703     C                   ENDSR
222500961022     C*
222600961022     C* SCRIVO FILE PER EDI ------------------------------------------*
222700961022     C     WRTEDI        BEGSR
222800961025     C* WSTS = '   ' C'E' COLLO E BOLLA; SITUAZIONE POSITIVA
222900961025     C*      = '001' COLLO MANCANTE; NON ESISTE NESSUNA SPUNTA ENTRATA
223000961025     C*        '002' COLLO ECCEDENTE;NON HO I DATI DELLA BOLLA
223100961025     C*        '003' COLLO ECCEDENTE;HO I DATI DELLA BOLLA IN ALTRA
223200961025     C*                              DATA SPEDIZIONE
223300980324     C*        '004' COLLO MANCANTE; PRESENTE SU ALTRO FOGLIO STESSA DT
223400980324     C*        '005' COLLO MANCANTE; PRESENTE SU ALTRO FOGLIO ALTRA  DT
223500961025     C                   CLEAR                   EDBRV000
223600980324     C     WSTS          IFNE      '002'
223700961022     C                   MOVEL     BLPAAS        EDIAAS
223800961022     C                   MOVEL     BLPLNP        EDILNP
223900961022     C                   MOVEL     BLPNRS        EDINRS
224000961022     C                   MOVEL     BLPNSP        EDINSP
224100961022     C                   MOVEL     BLPLNA        EDILNA
224200961022     C                   MOVEL     BLPFLS        EDIFLS
224300961022     C                   MOVEL     BLTNSC        EDINSC
224400980326     C                   MOVEL     BLTCAN        EDICAN
224500961028     C                   Z-ADD     BLPZNC        EDIZNC
224600961022     C                   MOVEL     WSTS          EDISTS
224700961022     C                   ENDIF
224800961025     C     WSTS          IFEQ      '002'
224900961025     C     WSTS          OREQ      '003'
225000961025     C     WSTS          OREQ      *BLANKS
225100961022     C                   MOVEL     BRVNPS        EDINPS
225200000209     C                   Z-ADD     BRVDFS        EDIDCS
225300070123     C                   movel     BRVHMS        EDIHCS
225400961022     C                   ENDIF
225500961025     C     WSTS          IFEQ      '002'
225600980326     C                   MOVEL     BRVCAN        EDICAN
225700961025     C                   MOVEL     BRVLNA        EDILNA
225800961025     C                   MOVEL     BRVLNP        EDIFLS
225900961025     C                   MOVEL     BRVNSC        EDINSC
226000961025     C                   MOVEL     BRVNRS        EDINRS
226100961025     C                   MOVEL     BRVZNC        EDIZNC
226200961025     C                   MOVEL     WSTS          EDISTS
226300961025     C                   ENDIF
226400961025     C*
226500961025     C     WSTS          IFEQ      '003'
226600961025     C                   ENDIF
226700961022     C                   MOVEL     VIDNFV        EDINFE
226800961022     C                   MOVEL     WFEDFV        EDIDFE
226900980108     C     BLPCCM        IFEQ      0
227000980108     C                   MOVEL     BLPKSC        EDICCM
227100980108     C                   ELSE
227200980108     C                   MOVEL     BLPCCM        EDICCM
227300980108     C                   END
227400961022     C*
227500961022     C                   WRITE     EDBRV000
227600961022     C                   ENDSR
227700980108     C*
227800980108     C* CARICO DATI IN SCHIERA ---------------------------------------*
227900121102     C**   CARPU         BEGSR
228000980108     C* CARICO I DATI DELLA TABELLA PU DEI CODICI CLIENTI/PARTNER O
228100980108     C* DELLE LNP SELEZIONATE IN SCHIERA
228200121102     C**                 OPEN      EDTAB01L
228300980108     C* --------------------------------------------------------------*
228400980108     C* ESEGUO LOOP SU TABELLA PU DEI CODICI CHE VOGLIONO LE DATE
228500980108     C* ARRIVO
228600121102     C**                 Z-ADD     0             WW
228700121102     C**                 MOVEL     'PU'          EDCOD
228800121102     C**   EDCOD         SETLL     EDTAB01L
228900121102     C**   EDCOD         READE     EDTAB01L                               30
229000121102     C**   *IN30         DOWEQ     *OFF
229100121102     C**   TABFLG        IFEQ      ' '
229200121102     C**                 MOVEL     TABUNI        EDIDSPU
229300121102     C**   §PUDTA        IFEQ      'S'
229400121102     C**                 ADD       1             WW
229500121102     C**                 MOVEL     TABKEY        EDK(WW)
229600121102     C**                 MOVEL     TABUNI        EDU(WW)
229700121102     C**                 END
229800121102     C**                 END
229900121102     C**   EDCOD         READE     EDTAB01L                               30
230000121102     C*+                 END
230100980108     C*---------------------------------------------------------------*
230200121102     C**                 DO        WW            W
230300121102     C**                 MOVEL     'PT'          EDCOD
230400121102     C**                 MOVEL     EDK(W)        EDKEY
230500121102     C**   KEDTAB        CHAIN     EDTAB01L                           30
230600121102    3C*+   *IN30         IFEQ      *OFF
230700121102     C**   TABFLG        ORNE      ' '
230800121102     C**                 MOVEL     TABUNI        EDIDSPT
230900121102     C**                 Z-ADD     1             XW
231000121102     C**   §PTKSC        LOOKUP    CLI(XW)                                32
231100121102     C**   *IN32         IFEQ      '0'
231200121102     C*+                 Z-ADD     1             XW
231300121102     C**   §PTLNP        LOOKUP    LNP(XW)                                32
231400121102     C**   CLI(XW)       IFNE      *ZEROS
231500121102     C*+                 SETOFF                                       32
231600121102     C**                 END
231700121102     C*+                 END
231800121102     C**   *IN32         IFEQ      '1'
231900121102     C**                 MOVEL     EDU(W)        EDIDSPU
232000121102     C**                 MOVE      §PUDTB        DTB(XW)
232100121102     C**                 MOVE      §PUSGD        SGD(XW)
232200121102     C**                 MOVE      §PUECC        ECC(XW)
232300121102     C**                 ENDIF
232400121102     C**                 ENDIF
232500121102     C**
232600121102     C**                 ENDDO
232700121102     C**
232800121102     C**                 CLOSE     EDTAB01L
232900980108     C*
233000121102     C**                 ENDSR
233100121102     C**
233200950531     C* OPERAZIONI INIZIALI E DEFINIZIONE CAMPI E KLIST --------------*
233300950531     C     *INZSR        BEGSR
233400010717     C* ACCESSO FNBLP31L
233500950531     C     KBLP          KLIST
233600950531     C                   KFLD                    COMLNP
233700950531     C                   KFLD                    COMANS
233800950531     C                   KFLD                    COMMGS
233900950531     C* ACCESSO FNBLP01L
234000950531     C     KBLP2         KLIST
234100950531     C                   KFLD                    BLTAAS
234200950531     C                   KFLD                    BLTLNP
234300950531     C                   KFLD                    BLTNRS
234400950531     C                   KFLD                    BLTNSP
234500050930     C     Kart          KLIST
234600050930     C                   KFLD                    BLTAAS
234700050930     C                   KFLD                    BLTLNP
234800050930     C                   KFLD                    BLTNRS
234900050930     C                   KFLD                    BLTNSP
235000050930     C                   KFLD                    BLTnsc
235100950531     C* ACCESSO FNBLT01L
235200950531     C     KBLT          KLIST
235300950531     C                   KFLD                    BLPAAS
235400950531     C                   KFLD                    BLPLNP
235500950531     C                   KFLD                    BLPNRS
235600950531     C                   KFLD                    BLPNSP
235700950531     C* ACCESSO FNBLT27L
235800950531     C     KBLT2         KLIST
235900950531     C                   KFLD                    BRVLNP
236000950531     C                   KFLD                    BRVLNA
236100950531     C                   KFLD                    BRVNRS
236200950531     C                   KFLD                    BRVNSC
236300991022     C     KANM          KLIST
236400991022     C                   KFLD                    BRVLNP
236500991022     C                   KFLD                    BRVLNA
236600991022     C                   KFLD                    BRVNRS
236700991022     C                   KFLD                    BRVNSC
236800991022     C                   KFLD                    COMCAA
236900000320     C     KANM2         KLIST
237000000320     C                   KFLD                    BLTFLS
237100000320     C                   KFLD                    BLTLNA
237200000320     C                   KFLD                    BLTNRS
237300000320     C                   KFLD                    BLTNSC
237400000320     C                   KFLD                    COMCAA
237500070123     C* ACCESSO FNBRV09L
237600950531     C     KBRV          KLIST
237700950531     C                   KFLD                    WNPG
237800950531     C                   KFLD                    BLTFLS
237900950531     C                   KFLD                    BLTLNA
238000950531     C                   KFLD                    BLTNRS
238100950531     C                   KFLD                    BLTNSC
238200061215     C                   KFLD                    ktfp
238300130222     C     KBRV_altr     KLIST
238400130222     C                   KFLD                    WNPG
238500130222     C                   KFLD                    BLTFLS
238600130222     C                   KFLD                    BLTLNA
238700130222     C                   KFLD                    BLTNRS
238800130222     C                   KFLD                    BLTNSC
238900070123     C* ACCESSO FNBRV09L
239000950703     C     KBRV2         KLIST
239100950707     C                   KFLD                    PARNPG
239200950703     C                   KFLD                    BRVLNP
239300950703     C                   KFLD                    BRVLNA
239400950703     C                   KFLD                    BRVNRS
239500950703     C                   KFLD                    BRVNSC
239600000201     C* ACCESSO FNFVV01L
239700950531     C     KFVV          KLIST
239800950531     C                   KFLD                    WNPG
239900950531     C                   KFLD                    VIDNFV
240000950531     C                   KFLD                    VIDFIL
240100950601     C     KFVV3         KLIST
240200950601     C                   KFLD                    WNPG
240300950601     C                   KFLD                    WNFV
240400950601     C                   KFLD                    WFGS
240500070109     C                   KFLD                    LP4(Y)
240600020918     C     KFVV2         KLIST
240700950601     C                   KFLD                    WNPG
240800950601     C                   KFLD                    COMDSP
240900020918     C                   KFLD                    Kfgs
241000950601     C     KFGV          KLIST
241100061215     C                   KFLD                    ktfp
241200950601     C                   KFLD                    COMDSP
241300950531     C* ACCESSO CNACO00F
241400950531     C     KACO          KLIST
241500950531     C                   KFLD                    CODUT
241600101202     C                   KFLD                    dutKCI
241700950531     C                   KFLD                    CLI(X)
241800950531     C     KTAB          KLIST
241900950531     C                   KFLD                    CODUT
242000950531     C                   KFLD                    COD
242100950531     C                   KFLD                    KEY
242200950629     C     KTAB2         KLIST
242300950629     C                   KFLD                    CODUT
242400950629     C                   KFLD                    COD
242500130225     C     Ktbe          KLIST
242600130225     C                   KFLD                    kcod
242700130225     C                   KFLD                    kke1
242800121102     C**     KEDTAB        KLIST
242900121102     C**                   KFLD                    EDCOD
243000121102     C**                   KFLD                    EDKEY
243100021203     C*
243200021203     C     Kfapd         klist
243300021203     C                   kfld                    kpdtip
243400021203     C                   kfld                    kpdpdr
243500950531     C*
243600101202     C                   Z-ADD     1             CODUT             1 0
243700101202      *
243800101202      * Reperimento dati utente/aziendali
243900101202     c                   exsr      sr_DatiJob
244000980402     C*
244100180126     C****               MOVEL     'I.M.X'       CINVEN
244200950531     C*
244300950531     C                   ENDSR
244400110715      /free
244500110715
244600110715       //-------------------------------------------------------------
244700110715       //?Lettura WFQEM00F per stampa errori                          ?
244800110715       //-------------------------------------------------------------
244900110715       BEGSR  sr_StampaWF;
245000110715
245100110718         // -?Chiusura del file WFQEM00F?
245200110718         close WFQEM00F;
245300110718
245400110715         // -?Preparazione SQL di lettura WFQEM00F?
245500110715         wSQL = 'select * from WFQEM00F';
245600110715
245700110715         if  PrmORD = '2';
245800110718           wSQL += ' order by WQErsm, +
245900110718                              WQElnp, WQEaas, WQEmgs, WQEnrs, WQEnsc';
246000120208         //else;
246100120208         //  wSQL += ' order by WQElnp, WQEaas, WQEmgs, WQEnrs, WQEnsc';
246200110715         endif;
246300110715
246400110715         // -?Dichiarazione cursore?
246500110715         exec sql   prepare S1   from :wSQL;
246600110715         exec sql   declare C1   cursor for S1;
246700110715
246800110715         // -?Apertura del cursore?
246900110715         exec sql   open C1;
247000110715
247100110715         if  SQLcode < *zero;
247200110715           exsr  sr_PrintErrSQL;
247300110718           leavesr;
247400110715         endif;
247500110715
247600110715         // -?Ciclo di lettura wf?
247700110715         exec sql   fetch next   from C1   into :WFQEMds;
247800110715
247900110715         DoW  SQLcode <> 100;
248000110718
248100110718           if  SQLcode < *zero;
248200110718             exsr  sr_PrintErrSQL;
248300110718             leavesr;
248400110718           endif;
248500110715
248600110715           exsr  sr_Stampa;
248700110718
248800110718           exec sql   fetch next   from C1   into :WFQEMds;
248900110715
249000110718         EndDo;
249100110715
249200110715         // -?Chiusura del cursore?
249300110715         exec sql   close C1;
249400110715
249500110715       ENDSR;
249600110715
249700110715       //--------------------------------------------------------------
249800110715       //?Stampa segnalazione dell'errore rilevato via SQL             ?
249900110715       //--------------------------------------------------------------
250000110715       BEGSR  sr_PrintErrSQL;
250100110715
250200110715         // -?Stampa del Dump?
250300110715         Dump(A);
250400110715
250500110715         // -?Stampa del Job-Log?
250600110715         Qcmd = 'DSPJOBLOG job(*) output(*print)';
250700110715         exsr  sr_ExecCmd;
250800110715
250900110715       ENDSR;
251000110715
251100110715       //--------------------------------------------------------------
251200110715       //?Esecuzione del comando (già impostato).                      ?
251300110715       //--------------------------------------------------------------
251400110715       BEGSR  sr_ExecCmd;
251500110715
251600110715         clear Qcap0100;
251700110715         Qcabcsdh = *off;
251800110715         Qcapa    = *off;
251900110715         Qcacmdss = *off;
252000110715         Qcaerved = *allX'00';
252100110715
252200110715         clear Qusec;
252300110715         Qusbprv  = %size(Qusec);
252400110715
252500110715         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
252600110715                           %size(Qcap0100) : 'CPOP0100' : *omit :
252700110715                           0 : 0 : Qusec);
252800110715
252900110715         //if  Qusei <> *blank;
253000110715         //  ...;
253100110715         //endif;
253200110715
253300110715       ENDSR;
253400110719
253500110719      /end-free
253600110715
253700110715       //-------------------------------------------------------------
253800110715       //?Stampa singolo errore                                       ?
253900110715       //-------------------------------------------------------------
254000110715     c     sr_Stampa     BEGSR
254100110715      *
254200110718     c                   movea     WQEind        *in
254300110718      *
254400110715     C                   SETON                                        20
254500110715     C*
254600110715     C* STAMPA TESTATA
254700110719do  1C   OF              DO
254800110715     C                   SETON                                        07
254900110715     C                   EXCEPT    TES
255000110715     C                   SETOFF                                       OF
255100110719e   1C                   ENDDO
255200110720 *
255300110721     c                   clear                   S_StaRSD
255400110720     c                   clear                   S_ChiSOno
255500110715     C*
255600110715     C* 05 OFF - STAMPO DATI SPEDIZIONE
255700110719if  1c                   If        NOT *in05
255800110719      *
255900110718     c                   eval      BLPaas = WQEaas
256000110718     c                   eval      BLPlnp = WQElnp
256100110718     c                   eval      BLPnrs = WQEnrs
256200110718     c                   eval      BLPnsp = WQEnsp
256300110718     c                   eval      BLPmgs = WQEmgs
256400110718     c                   eval      BLPlna = WQElna
256500110718     c                   eval      BLPrsm = WQErsm
256600110721     c                   eval      BLTfls = WQEfls
256700110718     c                   eval      BLTnsc = WQEnsc
256800110718      *
256900110715     C                   Z-ADD     BLPAAS        COMAAS
257000110719if  2C     SAVSPE        IFNE      BLPSPE
257100110715     C                   SETON                                        07
257200110715     C                   MOVEL     BLPSPE        SAVSPE
257300110719e   2C                   ENDIF
257400110715     C* DATA SPEDIZIONE
257500110719do  2C   07              DO
257600110715     C                   Z-ADD     BLPDSP        DATA
257700110715     C                   Z-ADD     MM            STADSP
257800110715     C                   MOVEL     GG            STADSP
257900110715     C*
258000110715     C* SEGNACOLLI NON CONSECUTIVI
258100110715     C     BLPFNS        COMP      'S'                                    09
258200110715     C*
258300110715     C* SERIE >< 0
258400110715     C     BLPNRS        COMP      0                                  0808
258500110715     C*
258600110718     ***C* RIFERIM. ALLEGATI MITTENTE
258700110718     ***C     BLPRMA        COMP      *BLANKS                            1414
258800110715     C*
258900110715     C* ACCORCIO LA RAGIONE SOCIALE MITTENTE
259000110715     C                   MOVEL     BLPRSM        STARSM
259100110721      *
259200110721      * - Accorcio la ragione sociale Destinatario
259300110721     c                   movel     BLPrsd        S_StaRSD
259400110715     C*
259500110715     C* GIRO DATA RITIRO MERCE
259600110715     C                   Z-ADD     BLPDRT        DATA
259700110715     C                   Z-ADD     MM            STADRT
259800110715     C                   MOVEL     GG            STADRT
259900110715     c*
260000110715     c* se presente consegna anomala, la decodifico
260100110715     c                   clear                   descca
260200110715     c                   clear                   dcmdfv            4 0
260300110719if  3c                   if        blpcca<>' '
260400110715     C                   MOVEL     '7O'          COD
260500110715     C                   MOVEL(P)  blpcca        KEY
260600110715     C     KTAB          CHAIN     TABEL                              30
260700110715     C  N30              MOVEL     TBLUNI        ds7o
260800110715     C   30              clear                   ds7o
260900110715     c                   movel     §7odei        descca           13
261000110715     c* giro la data consegnamerce se c'e'
261100110719if  4c                   if        blpdcm>0
261200110715     C                   Z-ADD     blpdcm        DATA
261300110715     C                   Z-ADD     MM            GGMM
261400110715     C                   MOVEL     GG            GGMM
261500110715     C                   MOVEL     GGMM          dcmDFV
261600110719e   4c                   endif
261700110719e   3c                   endif
261800110715     C*
261900110715     C* DECODIFICO CODICE PADRONCINO
262000110715     C*    BLPPDR        CHAIN     FNAPD01L                           15
262100110715     C                   move      BlpPdr        kpdpdr
262200110715     C                   move      'A'           kpdtip
262300110715     C     kfapd         chain     fiapd01l                           15
262400110715     C  N15              MOVEL     APDRSC        DESCPR
262500110715     C   15              CLEAR                   DESCPR
262600110719e   2C                   ENDDO
262700110720      /free
262800110720           // -?Reperimento "CHI SONO"?
262900110721           if  WQEnrs = *zero;
263000110720             k_ARSfls = WQEfls;
263100110720             k_ARSlna = WQElna;
263200110720             k_ARSnrs = WQEnrs;
263300110720             k_ARSnsc = WQEnsc;
263400110720             k_ARStrc = 'C';
263500110720             chain  %kds(k05fiars01)  FIARS000;
263600110720             if  %found(FIARS01L);
263700110720               S_ChiSono = ARSnot;
263800110720             endif;
263900110720           endif;
264000110720      /end-free
264100110718      *
264200110719x   1c                   Else
264300110718      *
264400110718     c                   eval      BRVlnp = WQElnp
264500110718     c                   eval      BRVnrs = WQEnrs
264600110718     c                   eval      BRVlna = WQElna
264700110718     c                   eval      BRVnsc = WQEnsc
264800110718      *
264900110719e   1c                   EndIf
265000110719      *
265100110719     C* SE COLLO MANCANTE O SPUNTA 88 DICO SE E' STATO SPARATO IN ENTR
265200110719     C   01
265300110719if  1COR 21BLTDSE        IFGT      0
265400110719     C                   SETON                                        11
265500110719     C                   Z-ADD     BLTDSE        DATA
265600110719     C                   Z-ADD     MM            ENTDFV
265700110719     C                   MOVEL     GG            ENTDFV
265800110719     C                   Z-ADD     BLTNPE        ENTNPS
265900110719e   1C                   ENDIF
266000110719     C*
266100110719if  1C     ANMDAO        IFGT      0
266200110719     C                   MOVEL     ANMDAO        DATA
266300110719     C                   Z-ADD     MM            DAMDFV
266400110719     C                   MOVEL     GG            DAMDFV
266500110719if  2C     DAMNPS        IFNE      *BLANKS
266600110719     C                   SETON                                        29
266700110719e   2C                   ENDIF
266800110719     C                   SETON                                        12
266900110719e   1C                   ENDIF
267000110715     C*
267100110715     C* SE 02 ON - SU ALTRO FOGLIO, GIRO LA DATA DEL FOGLIO
267200110720if  1C     *IN02         IFEQ      *ON
267300110715     C                   Z-ADD     PRTDFV        DATA
267400110715     C                   Z-ADD     MM            GGMM
267500110715     C                   MOVEL     GG            GGMM
267600110715     C                   MOVEL     GGMM          PR4DFV
267700110715     C*
267800110720if  2C     PRTNPG        IFNE      WSNPG
267900110715     C     PRTSPG        ORNE      WSSPG
268000110720if  3C     PRTSPG        IFNE      ' '
268100110715     C                   CLEAR                   PRTDPG
268200180126     C                   MOVEL     '7B'          COD
268300180126     C                   MOVEL(P)  PRTsPG        KEY
268400180126     C     KTAB          CHAIN     TABEL                              30
268500180126     C  N30              MOVEL     TBLUNI        ds7b
268600180126     C   30              CLEAR                   ds7b
268700180126     C                   MOVEl(p)  §7BDESB6      prtdpg
268800110720x   3C                   ELSE
268900110715     C                   MOVEL     '7N'          COD
269000110715     C                   MOVEL(P)  PRTNPG        KEY
269100110715     C     KTAB          CHAIN     TABEL                              30
269200110715     C  N30              MOVEL     TBLUNI        PRTDPG
269300110715     C   30              CLEAR                   PRTDPG
269400110720e   3C                   ENDIF
269500110715     C*
269600110715     C                   MOVEL     PRTNPG        WSNPG
269700110715     C                   MOVEL     PRTSPG        WSSPG
269800110720e   2C                   ENDIF
269900110715     C* DECODIFICO LA FIL GESTIONE
270000110720if  2C     PRTFGS        IFNE      WSFGS
270100110715     C     PRTFGS        CHAIN     AZORG01L                           30
270200110720if  3C  N30ORGDE5        IFNE      *BLANKS
270300110715     C                   MOVEL     ORGDE5        PRDFGS
270400110720x   3C                   ELSE
270500110715     C                   MOVEL     ORGDES        PRDFGS
270600110720e   3C                   ENDIF
270700110715     C   30              CLEAR                   PRDFGS
270800110715     C                   MOVEL     PRTFGS        WSFGS
270900110720e   2C                   ENDIF
271000110720e   1C                   ENDIF
271100120127     c
271200120127     c* Per situazione particolare in cui ho spunta fasulla in altra data
271300120127     c*  stampo la data e non stampo la riga di spunta in altra data
271400120127     c                   if        *in40
271500120127     c                   setoff                                       0102
271600120127     c                   endif
271700110715     c*
271800110715     c* Per consegna anomala, vedo che indicatore accendere
271900110715     c                   setoff                                       9606
272000110720if  1c                   if        blpcca<>' '
272100110720if  2c                   if        *in01 and not *in11 and not *in12
272200110715     c                   seton                                        06
272300110720x   2c                   else
272400110715     c                   seton                                        96
272500110720e   2c                   endif
272600110720e   1c                   endif
272700110715     C*
272800110715     C* STAMPO DETTAGLIO
272900120208     c* sempre per quadratura con spu partenza- 25 on
273000120208     c* sempre se richeiste tutte le anomalie - 25 off  e vid9000=N
273100120208     c* solo i manca record                   - 25 off e vid9000=S
273200120208    1c                   if        *in25   or
273300120208     c                             vid9000='N' or *in05
273400120208     c
273500110720     C                   EXCEPT    DET
273600110720      /free
273700110720         // -?Collo eccedente di bolla NON borderizzata?
273800110720         if  *in22;
273900110720           except  Det_22;
274000110720         endif;
274100110720         // -?Collo eccedente di bolla NON selezionata?
274200110720         if  *in04;
274300110721           if  (S_ChiSono <> *blank  or  S_StaRSD <> *blank)  and
274400110721               *in22;
274500110720             except  DetChiSono;
274600110720           endif;
274700110720           except  Det_04;
274800110720         endif;
274900110720         // -?Collo mancante?
275000110720         if  *in05;
275100120208          // trattandosi di manca record bolla non si può essere chi sono e RSD
275200120208          //  if  (S_ChiSono <> *blank  or  S_StaRSD <> *blank)  and
275300120208          //      (*in22  or  *in04);
275400120208          //    except  DetChiSono;
275500120208          //  endif;
275600120208
275700110720           except  Det_05;
275800110720         endif;
275900110720         // -?Spunta mancante?
276000110720         if  *in01;
276100110721           if  (S_ChiSono <> *blank  or  S_StaRSD <> *blank)  and
276200110720               (*in22  or  *in04  or  *in05);
276300110720             except  DetChiSono;
276400110720           endif;
276500110720           except  Det_01;
276600110720         endif;
276700110720         // -?Spunta mancante: presente spunta 88?
276800110720         if  *in21;
276900110721           if  (S_ChiSono <> *blank  or  S_StaRSD <> *blank)  and
277000110720               (*in22  or  *in04  or  *in05  or  *in01);
277100110720             except  DetChiSono;
277200110720           endif;
277300110720           except  Det_21;
277400110720         endif;
277500110720         // -?Spunta su altro foglio?
277600110720         if  *in02;
277700110721           if  (S_ChiSono <> *blank  or  S_StaRSD <> *blank)  and
277800110720               (*in22  or  *in04  or  *in05  or  *in01  or  *in21);
277900110720             except  DetChiSono;
278000110720           endif;
278100110720           except  Det_02;
278200110720         endif;
278300110720         // -?Dettaglio colli aggiornato con pistola fasulla?
278400110720         if  *in27;
278500110721           if  (S_ChiSono <> *blank  or  S_StaRSD <> *blank)  and
278600110720               (*in22  or  *in04  or  *in05  or  *in01  or  *in21  or
278700110720                *in02);
278800110720             except  DetChiSono;
278900110720           endif;
279000110720           except  Det_27;
279100110720         endif;
279200110720         // -?Bolla con consegna anomala?
279300110720         if  *in96;
279400110721           if  (S_ChiSono <> *blank  or  S_StaRSD <> *blank)  and
279500110720               (*in22  or  *in04  or  *in05  or  *in01  or  *in21  or
279600110720                *in02  or  *in27);
279700110720             except  DetChiSono;
279800110720           endif;
279900110720           except  Det_96;
280000110720         endif;
280100110720         // -?Zona segnacollo diversa da zona bolla?
280200110720         if  *in03;
280300110721           if  (S_ChiSono <> *blank  or  S_StaRSD <> *blank)  and
280400110720               (*in22  or  *in04  or  *in05  or  *in01  or  *in21  or
280500110720                *in02  or  *in27  or  *in96);
280600110720             except  DetChiSono;
280700110720           endif;
280800110720           except  Det_03;
280900110720         endif;
281000110721         // -?...solo Chi-Sono & Rag.Soc.Destinatario?
281100110721         if  S_ChiSono <> *blank  or  S_StaRSD <> *blank;
281200110720           if  Not *in01  and  Not *in02  and  Not *in03  and
281300110720               Not *in04  and  Not *in05  and
281400110720               Not *in21  and  Not *in22  and  Not *in27  and
281500110720               Not *in96;
281600110729             except  Det_SkipBe;
281700110720           endif;
281800110720           except  DetChiSono;
281900110729           except  Det_SkipAf;
282000110720         endif;
282100110720      /end-free
282200120208     c                   endif
282300110715     C                   SETOFF                                       07
282400110715      *
282500110715     c                   ENDSR
282600101202
282700101202      *---------------------------------------------------------------*
282800101202      *?Reperimento dati del job (Utente/Operativi)                  ?*
282900101202      *---------------------------------------------------------------*
283000101202     c     sr_DatiJob    BEGSR
283100101202      *
283200101202     c     *dtaara       define    §azute        AZUTEds
283300101202     c     *dtaara       define    §datiute      dDatiUte
283400101202      *
283500101202     c                   in(E)     *dtaara
283600101202     c                   IF        %Error or RSUT = *blanks
283700101202     c                   clear                   TIBS34ds
283800101202     c                   call      'TIBS34R'
283900101202     c                   parm                    TIBS34ds
284000101202     c                   in        *dtaara
284100101202     c                   ENDIF
284200101202      *
284300101202     c                   ENDSR
284400950113     C*---------------------------------------------------------------*
284500020909
284600940329     OPRTF198   E            RICHI1           02
284700940329     O                       STA(1)             132
284800900626     O                       RSUT                20
284900110726     O                                          148 'FNLSA7R'
285000950605     O                       UDATE8             170 '  /  /    '
285100940329     O                                          193 'PAG.'
285200940329     O                       PAGE          Z    198
285300950605     O          E            RICHI1           03
285400950605     O               25      STA(16)            132
285500961028     O              N25N26   STA(17)            132
285600961028     O              N25 26   STA(18)            132
285700950605     O                       KNSIF               10
285800950605     O                       KNMUS               22
285900950605     O               25      VIDDSP             124 '  /  /    '
286000950605     O                       UTIME              170 '  :  :  '
286100930318     O          E            RICHI1      3
286200930318     O                                              'P A R A M E T R I'
286300930319     O                                         +  4 'R I C H I E S T I'
286400950530     O          E   N25      RICHI1      2
286500930319     O                                              'FOGLIO ENTRATA DI'
286600930318     O                       VIDFIL            +  2
286700930319     O                                         +  1 '-'
286800930319     O                       DESFIL            +  1
286900930319     O                                         +  5 'N.RO'
287000930318     O                       VIDNFV            +  2
287100930319     O                       DESNFV            +  3
287200930318     O          E            RICHI1      2
287300980416     O              N25                          24 'DATA SPEDIZIONE ........'
287400070109     O              N25                        +  0 '..........: DAL'
287500950601     O               25                          24 'DATA ELABORAZIONE ......'
287600070109     O               25                        +  0 '..............:'
287700950113     O                       VIDDSP            +  2 '  /  /    '
287800980416     O              N25                        +  2 'AL'
287900980416     O              N25      VIDDSA            +  2 '  /  /    '
288000930318     O          E            RICHI1      2
288100930318     O                                              'ANOMALIE COLLI ECCEDENTI'
288200070109     O                                         +  0 ' .............:'
288300930319     O                       COMSTA            +  2
288400080626     O          E    25      RICHI1      2
288500080626     O                                              'ANOMALIE BOLLE LOCALI SE'
288600080626     O                                         +  0 'RIE  00  .....:'
288700080626     O                       COM9000           +  2
288800070108     O          E    25      RICHI1      2
288900070109     O                                              'ESCLUDI SPUNTE 88/90 DI '
289000070109     O                                         +  0 'COLLI ARRIVATI:'
289100070108     O                       COMSN             +  2
289200110715     o          e            RICHI1      2
289300110715     o                                              'ORDINAMENTO STAMPA .....-
289400110715     o                                              ..............:'
289500110715     o                       PrmORD            +  2
289600110721     o                                         +  3 '(1 = Linea di Partenza /-
289700110721     o                                               Serie / Segnacollo;'
289800110719     o          e            RICHI1      2
289900110715     o                                              '                        -
290000110715     o                                                             '
290100110721     o                                         +  6 ' 2 = Ragione Sociale Mit-
290200110719     o                                              tente / Linea di Partenza-
290300110721     o                                               / Serie / Segnacollo)'
290400950530     O          E   N25      RICHI1      2
290500130226     O                                              'Escludi i"MANCANTI" di A'
290600130226     O                                         +  0 'LTRO FOGLIO ..:'
290700130226     O                       COMmanca          +  2
290800130226     O          E   N25      RICHI1      2
290900130226     O                                              'ELABORAZIONE DEFINITIVA '
291000130226     O                                         +  0 '..............:'
291100130226     O                       COMSN             +  2
291200930318     O          E            RICHI1      3
291300930319     O                                         +  6 'LINEA PARTENZA'
291400961017     O                                         + 12 'COD. AUTOTRASPORTATORE'
291500961017     O                                         + 10 'SERIE'
291600930521     O                                         +  7 'C O D.    C L I E N T E'
291700930521     O                                         + 15 'DATA RITIRO'
291800950323     O                                         +  3 'N.RITIRO'
291900950324     O                                         +  3 'MAT/POM'
292000930318     O          E            RICHI2      2
292100930319     O                       LNP(X)        Z   +  5
292200930319     O                                         +  1 '-'
292300930319     O                       DESLNP            +  1
292400930319     O                       CPC(X)        Z   +  6
292500930319     O                     23                  +  1 '-'
292600930319     O                       DESCPC            +  1
292700930524     O                       VIDNRS            +  9
292800930521     O                       CLI(X)        Z   +  7
292900930319     O                     24                  +  1 '-'
293000930319     O                       DESCLI            +  1
293100950113     O                       VIDDRT            +  9 '  /  /    '
293200950324     O                       NRT(X)            +  8
293300950324     O                       MOP(X)            +  9
293400930317     O*
293500930317     O          E            TES              02
293600940329     O                       STA(1)             132
293700930317     O                       RSUT                20
293800110726     O                                          148 'FNLSA7R'
293900950605     O                       UDATE8             170 '  /  /    '
294000940329     O                                          193 'PAG.'
294100940329     O                       PAGE          Z    198
294200950605     O          E            TES              03
294300950605     O               25      STA(16)            132
294400950605     O              N25      STA(17)            132
294500950605     O               25      VIDDSP             124 '  /  /    '
294600950605     O                       KNSIF               10
294700950605     O                       KNMUS               22
294800950605     O                       UTIME              170 '  :  :  '
294900950530     O          E   N25      TES         2
295000940329     O                       STA(2)              66
295100930319     O                       VIDFIL              39
295200930319     O                                           40 '-'
295300930319     O                       DESFIL              60
295400930319     O                                           69 'N.RO'
295500930319     O                       VIDNFV        Z     76
295600930319     O                                           84 'DEL'
295700950113     O                       STADFV              96 '  /  /    '
295800911018     O          E            TES         2
295900940329     O                       STA(3)              66
296000940329     O                       STA(6)             132
296100950324     O                       STA(12)            198
296200980402     O          E            TES         1  1
296300940329     O                       STA(4)              66
296400940329     O                       STA(7)             132
296500950324     O                       STA(13)            198
296600911015     O*
296700980402     O          E    05      DET            1
296800920423     O                                          132 ' '
296900980402     O          E            DET         0  0
297000110715     O              N05 07   BLPAAS        z
297100110715     O              N05 07   BLPLNP        z  +   1
297200110715     O              N05 07 08BLPNRS        z  +   1
297300110715     O              N05 07 08                 +   0 '-'
297400110715     O              N05 07   BLPNSP        z  +   0
297500110715     O              N05 07   STADSP           +   1 '  /  '
297600110715     O              N05 07   BLPLNA        z  +   1
297700110715     O              N05 07   STARSM           +   2
297800110715     o              N05 07   BLPRMN        z  +   2
297900110715     o              N05 07   BLPRMA           +   1
298000110715     o              N05 07   BLPNCL        z  +   1
298100110715     O              N05 07N15BLPPDR        z  +   1
298200110715     O              N05 07N15                 +   0 '-'
298300110715     O              N05 07N15DESCPR           +   0
298400110715     O              N05 07   STADRT           +   2 '  /  '
298500110715     O              N05      BLTNSC        z  +   2
298600110715     O               05      BRVLNP        z      8
298700110715     O               05      BRVNRS        z     11
298800110715     O               05      BRVLNA        z     29
298900110718     O               05      BRVNSC        z    135
299000110715     ***O              N05 07N09BLPNCD        Z     75
299100110715     ***O              N05 07N09                    76 '/'
299200110715     ***O              N05 07N09BLPNCA        Z     83
299300110715     ***O              N05 07 09                    83 'NON SEQUENZIALI'
299400110715     ***O              N05 07   BLPZNC              88
299500110715     ***O               05      BRVZNC              88
299600110715     ***O              N05 07   BLPNRT             125
299700110715     ***O              N05 07   BLPFPP             128
299800980402     O* COLLO ECCEDENTE DI BOLLA NON BORDERIZZATA
299900110720     O          E    22      DET_22      0  1
300000980402     O                                          158 'ECCEDENTE DI BOLLA'
300100980402     O                                          175 'NON BORDERIZZATA'
300200980402     O* COLLO ECCEDENTE DI BOLLA NON SELEZIONATA
300300110720     O          E    04      DET_04      0  1
300400980402     O                                          158 'ECCEDENTE DI BOLLA'
300500980402     O                                          174 'NON SELEZIONATA'
300600110720     O          E    05      DET_05      0  1
300700980402     O* COLLO MANCANTE
300800980402     O                                          151 'MANCA BOLLA'
300900980402     O                                          174 'LETTORE BARCODE N.RO:'
301000980402     O                       BRVNPS             177
301100110715     o                                        +   1 'ZONA'
301200110715     o                       BRVZNC           +   1
301300950531     O* SPUNTA MANCANTE
301400110720     O          E    01      DET_01      0  1
301500980402     O                                          148 'MANCANTE'
301600110718     ***O                  14                      160 ':RIF.ALFAB.'
301700110715     ***O                  14   BLPRMA             175
301800070419     O                  11                      187 'ENTRATO il'
301900980402     O                  11   ENTDFV             193 '  /  '
302000980402     O                  11                      196 'P.'
302100980402     O                  11   ENTNPS             198
302200000621     O                  12   DESANP             187
302300000314     O                  12   DAMDFV             193 '  /  '
302400000320     O               29 12                      196 'P.'
302500000320     O               29 12   DAMNPS             198
302600070419     O                  06   descca             188
302700070419     O                  06                      191 'il'
302800070419     O                  06   DCMDFV             198 '  /  '
302900000314     O* SPUNTA MANCANTE
303000110720     O          E    01 28   DET_01      0  1
303100070419     O                                          187 'ARRIVATO il'
303200000314     O                       DAMDFV             193 '  /  '
303300000320     O               29                         196 'P.'
303400000320     O               29      DAMNPS             198
303500980402     O* SPUNTA MANCANTE: PRESENTE SPUNTA 88
303600110720     O          E    21      DET_21      0  1
303700980402     O                                          155 'MANCANTE:SPUNTA'
303800980402     O                       W88NPS             158
303900000201     O                                          161 'FV'
304000120127     O                       W88NFV        Z    167
304100120127     o                  40                      170 'dt'
304200120127     O                  40   pr4DFV             176 '  /  '
304300070419     O                  11                      187 'ENTRATO il'
304400980402     O                  11   ENTDFV             193 '  /  '
304500980402     O                  11                      196 'P.'
304600950606     O                  11 21ENTNPS             198
304700000621     O                  12   DESANP             187
304800000314     O                  12   DAMDFV             193 '  /  '
304900000320     O               29 12                      196 'P.'
305000000320     O               29 12   DAMNPS             198
305100000314     O* SPUNTA MANCANTE: PRESENTE SPUNTA 88
305200110720     O          E    21 28   DET_21      0  1
305300070419     O                                          187 'ARRIVATO il'
305400000314     O                       DAMDFV             193 '  /  '
305500000320     O               29                         196 'P.'
305600000320     O               29      DAMNPS             198
305700950531     O* SPUNTA SU ALTRO FOGLIO
305800110720     O          E    02      DET_02      0  1
305900980402     O                    N25                   161 'MANCANTE, SPUNTATO DA'
306000980402     O                     25                   161 '          SPUNTATO DA'
306100980402     O                       PRTFGS             166
306200070419     O                                          170 'in'
306300980402     O                       PRTDPG             180
306400980402     O                       PRTNFV        Z    187
306500070419     O                                          191 'il'
306600980402     O                       PR4DFV             198 '  /  '
306700980402     O* DETTAGLIO COLLI AGGIORNATNO CON PISTOLA FASULLA
306800110720     O          E    27      DET_27      0  1
306900980402     O                                          164 'BOLLA AGGIORNATA CON SPU'
307000980402     O                                          167 'NTA'
307100980402     O                       BLTNPP             171
307200980402     O                                          198 'MA PRESENTE SPUNTA REALE'
307300070419     O* bolla con consegna anomala
307400110720     O          E    96      DET_96      0  1
307500070419     O                       descca             188
307600070419     O                                          191 'il'
307700070419     O                       DCMDFV             198 '  /  '
307800980402     O* ZONA SEGNACOLLO DIVERSA DA ZONA BOLLA
307900110720     O          E    03      DET_03      0  1
308000950601     O                                          155 'ZONA SEGNACOLLO'
308100950601     O                       BRVZNC             158
308200980403     O                                          181 'DIVERSA DA ZONA BOLLA'
308300110721      *
308400110729     o          e            Det_SkipBe  1
308500110720     o                                          132 ' '
308600110721      *
308700110720     o          e            DETchiSono  0  0
308800110729     o                       S_StaRSD         +  45
308900110729     o                       S_ChiSono        +  29
309000110721      *
309100110729     o          e            Det_SkipAf  1
309200110720     o                                          132 ' '
309300110721      *
309400940426     O          E            OK          3
309500940426     O                                              ' '
309600940426     O          E            OK          3
309700940329     O                       STA(9)             132
309800940426     O          E   N20      NOELAB      3
309900940426     O                                              ' '
310000940426     O          E            NOELAB      3
310100940421     O                       STA(11)            132
310200930319     O          E            PIEDI            64
310300940329     O                       STA(10)            132
310400110715** STA ..1....+....2....+....3....+....4....+....5....+....6....+.
310500110715      ****       QUADRATURA   ENTRATA   A   MAGAZZINO      ****      1
310600940329F O G L I O   E N T R A T A :   DI  XXX-XXXXXXXXXXXXXXXXXXXX         2
310700110715*------- SPEDIZIONE --------*  M I T T E N T E            *--- RIF   3
310800110929ANNO LNP SER-NUMERO  DATA LNA                Destinatario        N   4
310900110715XXXX XXX XX-XXXXXXX XX/XX XXX  *...+....1....+....2....+  12345678   5
311000110715ERIMENTO  MITTENTE ---*  NUM. AUTOTRASPORTATORE        DATA    SEG   6
311100110721UMERICO ALFANUMERICO    COLLI    chi sono              RITIRO  ERR   7
3112001107159012345 *...+....1....+ 12345 XXXXXXX-XXXXXXXXXXXXXXX  XX/XX  1234   8
311300940329        TUTTO  O.K.  :  NESSUNA  ANOMALIA  RISCONTRATA  !!           9
311400940329        ***          F I N E      S T A M P A          ***          10
311500940426         N E S S U N A    B O L L A     E L A B O R A T A  !!       11
311600110715NAC                                                                 12
311700110715ATO                                                                 13
311800110715567                                                                 14
311900950530                           DEL  XX/XX/XXXX                          15
312000950606      **** CON SPUNTE PARTENZA E SPUNTE IMP DEL XX/XX/XXXX ****     16
312100950605      ****                CON SPUNTE ENTRATA               ****     17
312200961028      ****                 E .  D .  I .                   ****     18
