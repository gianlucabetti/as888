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
010700171023     D trulnrsds     E DS
010800171023     D xgiolavds     E DS
010900020909     D* DS PER TRUL06R - CARICAMENTO £X
011000020909     D TRUL06DS      E DS
011100020909     D  LIN                    1     90  0
011200020909     D                                     DIM(30)
011300020909     D* DS PER FNLV53R - CARICAMENTO DATI DI UN FOGLIO
011400020909     D FNLV53DS      E DS
011500070109     D* DS PER FNLV55R - reperimento terminal partenza/arrivo
011600070109     D fnlv55ds      E DS
011700130225     D ktfp            S                   like(blptfp)
011800130225     D kcod            S                   like(tbecod)
011900130225     D kke1            S                   like(tbeke1)
012000130225     d Indx            s              4  0
012100130225     d Wdefl           s              1
012200020909      *--------------------------------------------------------------*
012300020909      *   S T R U T T U R E   D A T I  DEFINITE INTERNAMENTE
012400020909      *--------------------------------------------------------------*
012500941228     D PARAM           DS
012600110714     D  VIDNFV                 1      3P 0
012700110714     d  PrmORD                 4      4
012800110714      * ...libero              5      5
012900941228     D  COMANS                 6      9  0
013000941228     D  COMMGS                10     13  0
013100941228     D  COMDSP                 6     13  0
013200950323     D  LNP                   14     37P 0
013300950323     D                                     DIM(12)
013400950323     D  CPC                   38     85P 0
013500950323     D                                     DIM(12)
013600950323     D  CLI                   86    133P 0
013700950323     D                                     DIM(12)
013800950323     D  DRT                  134    193P 0
013900950605     D                                     DIM(12)
014000060310     D  NRS                  194    217
014100950323     D                                     DIM(12)
014200991022     D  MOP                  218    229
014300950605     D                                     DIM(12)
014400950323     D  NRT                  230    241
014500950605     D                                     DIM(12)
014600961022     D* SOLO PER EDI: FLAG DI CREAZIONE DEL FIL EDBRV00F
014700961022     D  EDSEL                242    242
014800080626     D  VID9000              243    243
014900130226     D  VIDmanca             244    244
015000980415     D  COMDSA               246    250P 0
015100980108     D  PARTQU               251    251
015200980108     D  VIDSN                252    252
015300980108     D  VIDSTA               253    253
015400980108     D  VIDFIL               254    256  0
015500980416     D                 DS
015600980416     D  WANSA                  1      4  0
015700980416     D  WMGSA                  5      8  0
015800980416     D  WDSA                   1      8  0
015900911018     D                 DS
016000941228     D  BLPAAS                 1      4  0
016100950113     D  BLPMGS                 5      8  0
016200941228     D  BLPDSP                 1      8  0
016300911018     D                 DS
016400941228     D  COMAAS                 1      4  0
016500941228     D  BLPLNP                 5      7  0
016600941228     D  BLPNRS                 8      9  0
016700941228     D  BLPNSP                10     16  0
016800950113     D  BLPSPE                 1     16  0
016900930318     D                 DS
017000950113     D  AA                     1      4  0
017100950113     D  MM                     5      6  0
017200950113     D  GG                     7      8  0
017300950113     D  DATA                   1      8  0
017400020909      *--------------------------------------------------------------*
017500020909      *   V A R I A B I L I
017600020909      *--------------------------------------------------------------*
017700020909     D COMLNP          S                   LIKE(BLPLNP) inz
017800020909     D SAVSPE          S                   LIKE(BLPSPE) inz
017900020909     D WNPG            S                   LIKE(BRVNPG) inz
018000020909     D WSNPG           S                   LIKE(BRVNPG) inz
018100110718     ***D PRTNPG          S                   LIKE(BRVNPG) inz
018200020909     D PARNPG          S                   LIKE(BRVNPG) inz
018300020909     D WDTNPG          S                   LIKE(BRVNPG) inz
018400020909     D WDTNFV          S                   LIKE(BRVNFV) inz
018500020909     D WDTNPS          S                   LIKE(BRVNPS) inz
018600030624     d wdtLnp          s                   Like(BrvLnp) Inz
018700030624     d wdtNrs          s                   Like(BrvNrs) Inz
018800120127     d prtLnp          s                   Like(BrvLnp) Inz
018900120127     d prtNrs          s                   Like(BrvNrs) Inz
019000110718     ***D PRTNFV          S                   LIKE(BRVNFV) inz
019100020909     D WSNFV           S                   LIKE(BRVNFV) inz
019200020909     D COMNFV          S                   LIKE(BRVNFV) inz
019300020909     D WNFV            S                   LIKE(BRVNFV) inz
019400110719     ***D W88NFV          S                   LIKE(BRVNFV) inz
019500110719     ***D W88NPS          S                   LIKE(BRVNPS) inz
019600110718     ***D PRTDFV          S                   LIKE(FVVDFV) inz
019700020909     D WDFV            S                   LIKE(FVVDFV) inz
019800020909     D STADFV          S                   LIKE(FVVDFV) inz
019900020909     D ENTDFV          S                   LIKE(STADSP) inz
020000020909     D DAMDFV          S                   LIKE(STADSP) inz
020100020909     D WFGS            S                   LIKE(BRVFGS) inz
020200020909     D PRTNPS          S                   LIKE(BRVNPS) inz
020300020909     D ENTNPS          S                   LIKE(BRVNPS) inz
020400020909     D SAVNPS          S                   LIKE(BRVNPS) inz
020500020909     D PRTZNC          S                   LIKE(BLPZNC) inz
020600110718     ***D PRTSPG          S                   LIKE(FVVSPG) inz
020700020909     D WSSPG           S                   LIKE(FVVSPG) inz
020800020909     D WSFGS           S                   LIKE(FVVFGS) inz
020900110718     ***D PRTFGS          S                   LIKE(FVVFGS) inz
021000020909     D DESNFV          S                   LIKE(FVVDSF) inz
021100020909     D VIDDSP          S                   LIKE(COMDSP) inz
021200020909     D VIDDSA          S                   LIKE(COMDSP) inz
021300020909     D COMNRT          S                   LIKE(BLPNRT) inz
021400020909     D COD             S                   LIKE(TBLCOD) inz
021500020909     D KEY             S                   LIKE(TBLKEY) inz
021600020909     D WMAX            S                   LIKE(TBLKEY) inz
021700020909     D WFEDFV          S                   LIKE(FVVDFV) inz
021800050920     D Wdatafv         S                   LIKE(FVVDFV) inz
021900020909     D WSTS            S                   LIKE(EDISTS) inz
022000121102     D** EDCOD           S                   LIKE(TABCOD) inz
022100121102     D** EDKEY           S                   LIKE(TABKEY) inz
022200020909     D WDEF            S                   LIKE(D53DEF) inz
022300020909     D WSPG            S                   LIKE(D53SPG) inz
022400020909     D COMCAA          S                   LIKE(ANMCAA) INZ(55)
022500110720     d S_ChiSono       s                   like(ARSnot) inz
022600020918     d kfgs            s                   Like(fvvfgs)
022700021203     D kpdpdr          S                   LIKE(APDpdr)
022800021203     D kpdtip          S                   LIKE(APDtip)
022900020909     D*
023000020909     D* Indici di schiere
023100020909     D X               S              3  0 inz
023200020909     D Y               S              3  0 inz
023300020909     D Z               S              3  0 inz
023400020909     D XX              S              4  0 inz
023500020909     D YY              S              3  0 inz
023600070109     D EE              S              4  0 inz
023700070109     D ZZ              S              4  0 inz
023800150623     D BB              S              4  0 inz
023900020909     D W               S              4  0 inz
024000020909     D WW              S              4  0 inz
024100020909     D XW              S              4  0 inz
024200020918     d xy              s              2  0 inz
024300140717     d i               s              3  0 inz
024400020909     D* Flag booleani
024500020909     D WTROV           S              1    inz
024600020909     D RIPROW          S              1  0 inz
024700020909     D $ok£1           S              1    inz(*off)
024800020909     D* Campi di comodo
024900020909     D W002A           S              2    inz
025000020909     D W0030           S              3  0 inz
025100020909     D W0140           S             14  0 inz
025200020909     D WLPSR           S              5  0 inz
025300140717     d dataiso         s               d   datfmt(*iso)
025400140717     d wque            s              1    inz
025500020909     D* Campi di stampa
025600020909     D UDATE8          S              8  0 inz
025700020909     D UTIME           S              6  0 inz
025800020909     D COMSTA          S              2    inz
025900080626     D COM9000         S              2    inz
026000020909     D COMSN           S              2    inz
026100130226     D COMMANCA        S              2    inz
026200020909     D GGMM            S              4  0 inz
026300020909     D EDICON          S              1    inz
026400020909     D* - Decodifica P.O. Gestione
026500020909     D DESFIL          S             20    inz
026600020909     D* - Data Spedizione
026700020909     D STADSP          S              4  0 inz
026800020909     D* - Ragione Sociale Mittente
026900110715     D STARSM          S             25    inz
027000110721      * - Ragione Sociale Destinatario
027100110721     d S_StaRSD        s             25    inz
027200020909     D* - Data Ritiro Merce
027300020909     D STADRT          S              4  0 inz
027400020909     D* - Decodifica Padroncino
027500020909     D DESCPR          S             15    inz
027600020909     D* - Data Foglio
027700020909     D PR4DFV          S              4  0 inz
027800020909     D PRTDPG          S              8    inz
027900110718     D* - P.O. Gestione
028000110718     D PRDFGS          S              8    inz
028100020909     D* - Spunta Mancante
028200110718     ***D DAMNPS          S              2    inz
028300110718     ***D DESANP          S             11    inz
028400020909     D* - P.O. Linea di Partenza
028500020909     D DESLNP          S             15    inz
028600020909     D* - Decodifica Padroncino
028700020909     D DESCPC          S             15    inz
028800020909     D* - Decodifica Cliente
028900020909     D DESCLI          S             20    inz
029000020909     D* - Data Ritiro Merce
029100020909     D VIDDRT          S              8  0 inz
029200020909     D* - Numero Serie
029300020909     D VIDNRS          S              2    inz
029400110715
029500110715       // -?Stringa SQL da eseguire?
029600110715     d wSQL            s           2048    Varying        inz
029700110715
029800110715       // -?Ds di comodo per la definizione dei campi estratti via SQL?
029900110715     d WFQEMds       e ds                  extname(WFQEM00F)
030000110718     d                                     inz
030100110715
030200110715       //--------------------------------------------------------------
030300110715       //?Definizione prototipi procedure.                             ?
030400110715       //--------------------------------------------------------------
030500110715
030600110715       // -?Parametri gestione errori API.?
030700110715      /copy qsysinc/qRpgleSrc,QUSEC
030800110715       // -?Parametri API QCAPCMD (Process Commands)?
030900110715     d Qcmd            s           2048    inz  varying
031000110715      /copy qSysInc/qRpgleSrc,QCAPCMD
031100110715       // -?API QCAPCMD (Process Commands)?
031200110715      /copy gaitrasrc/srcProtoPR,QCAPCMD
031300110720
031400110720       //--------------------------------------------------------------
031500110720       //?Definizione key-list.                                        ?
031600110720       //--------------------------------------------------------------
031700110720
031800110720       // -?File FIARS01L?
031900110720     d k05fiars01    e ds                  extname(FIARS01L : *key)
032000110720     d                                     prefix(k_)   inz
032100020909
032200020909      ****************************************************************
032300020909      *  RIEPILOGO INDICATORI
032400020909      ****************************************************************
032500020909      * 01    - STAMPA SCRITTA - MANCANTE
032600020909      * 02    - STAMPA SCRITTA - PRESENTE SU FOGLIO ENTRATA XXXXX
032700020909      * 03    - STAMPA SCRITTA - ZONA SEGNACOLLO DIVERSA DA ZONA BOLLA
032800020909      * 04    - STAMPA SCRITTA - ECCEDENTE DI BOLLA NON SELEZIONATA
032900020909      * 05    - STAMPA SCRITTA - MANCA BOLLA PARTENZA
033000070419      * 06 96 - stampa decodifica consegna anomala se c'è
033100070419      * 07    - ON  CAMBIATA SPEDIZIONE
033200020909      * 08    - ON  ESISTE SERIE
033300020909      * 09    - SEGNACOLLI NON SEQUENZIALI
033400020909      * 10    - ON LOKUP RIUSCITA
033500020909      * 11    - STAMPA SCRITTA - COLLO MANCANTE SPARATO SU F.ENTRATA
033600020909      * 12    - STAMPA SCRITTA - COLLO MANCANTE SPARATO IN ARRIVO
033700020909      * 13    - DI COMODO
033800110718     *** * 14    - SE SEGNACOLLO MANCANTE STAMPO RIFERIM. ALLEGATI MITT.
033900020909      * 15    - ON  NON C'E' COD PADRONCINO CONSEGNA
034000020909      * 16    - ON  LOKUP RIUSCITA
034100061215      * 17    - acceso se collo arrivato al terminal di arrivo previsto
034200061215      *       - e non disguidato
034300020909      * 18    - ON  E' RICHIESTA LA STAMPA ANOMALIE DI MANCA BOLLA E
034400020909      *         COLLI ECCEDENTI
034500020909      * 19    - ELABORO ALMENO UNA BOLLA
034600020909      * 20    - STAMPO TESTATA
034700020909      * 21    - STAMPA SCRITTA - COLLO MANCANTE SPUNTA 88
034800020909      * 22    - STAMPA SCRITTA - COLLO ECCEDENTE DI BOLLA NON BORDERIZZ
034900020909      * 23/24 - USATI IN STAMPA PER EMETTERE IL TRATTINO SEPARATORE
035000020909      * 25    - TIPO QUADRATURA CON SPUNTE PARTENZA
035100070419      * 26    - per EDIBRV
035200020909      * 27    - BOLLA AGGIORN.CON SPUNTA FASULLA MA PRESENTE SPU.REALE
035300020909      * 28    - STAMPA SCRITTA - COLLO MANCANTE SPARATO IN ARRIVO
035400020909      *         SU ALTRA RIGA *** ATTENZIONE *** NON PIU' GESTITO E'
035500020909      *         SEMPRE OFF (SONO STATE LASCIATE COMUNQUE LE SPECIFICHE
035600020909      *         DI OUTPUT)
035700020909      * 29    - STAMPO PISTOLA DATA DI ARRIVO
035800020909      * 30/36 - COMODO
035900120127      * 40    - STAMPO DATA per pistola fasulla non in data del controllo
036000091013      * 85    - ctm di autospunta per quadratura entrata
036100070419      * 96 06 - stampa decodifica consegna anomala se c'è
036200020909      *****************************************************************
036300020909
036400900622     C     *ENTRY        PLIST
036500900622     C                   PARM                    KPJBA
036600900622     C                   MOVEL     KPJBU         PARAM
036700950530     C*
036800950531     C* VEDO CHE TIPO DI QUADRATURA E'.
036900950530     C* 25 ON - QUADRATURA CON SPUNTE PARTENZA
037000950530     C     PARTQU        IFEQ      'P'
037100950530     C                   SETON                                        25
037200991022     C                   ENDIF
037300930318     C*---------------------------------------------------------------*
037400920804     C* CARICO TABELLA FILIALI GESTITE £1
037500920804     C                   EXSR      CARTAB
037600930318     C*
037700930318     C* IMPOSTO CAMPI FISSI
037800930318     C                   EXSR      IMPO
037900920804     C*
038000930318     C* STAMPA DELLE RICHIESTE EFFETTUATE A VIDEO
038100930318     C                   EXSR      VIDEO
038200930318     C*
038300950605    1C     LNP(1)        IFEQ      0
038400000619     C                   Z-ADD     0             Y
038500000619     C     1             DO        30            X
038600000619     C     L1(X)         IFNE      0
038700000619     C                   ADD       1             Y
038800000619     C                   MOVE      L1(X)         SKI(Y)
038900000619     C                   MOVE      L1(X)         LP2(Y)
039000000619     C                   ENDIF
039100000619     C                   ENDDO
039200000619      *
039300060310     C                   MOVEL     '99'          NRS
039400950605     C*
039500950605   X1C                   ELSE
039600000619     C* . . . ALTRIMENTI
039700950605     C                   MOVEA     LNP           SKI
039800950605     C* MEMORIZZO LE LNP SELEZIONATE, MA SOLO UNA VOLTA SE FOSSERO
039900950605     C*  RIPETUTE
040000020909     C                   Z-ADD     1             X
040100020909     C                   Z-ADD     1             Y
040200950928    2C     X             DOWLE     12
040300950928     C     LNP(X)        ANDNE     0
040400950605     C     LNP(X)        LOOKUP    LP2                                    30
040500950712     C  N30              MOVEL     LNP(X)        LP2(Y)
040600950605     C  N30              ADD       1             Y
040700950605     C                   ADD       1             X
040800950605    2C                   ENDDO
040900950605    1C                   ENDIF
041000920907     C*
041100950920     C                   Z-ADD     1             Y
041200950920    1C     LP2(Y)        DOWNE     0
041300920902     C*
041400950920     C                   MOVEL     LP2(Y)        COMLNP
041500920804     C* LEGGO LE BOLLE
041600010717     C     KBLP          SETLL     FNBLP31L
041700010717     C     COMLNP        READE     FNBLP31L                               30
041800980417     C* ESCO DAL CICLO SE SUPERATA DATA AL
041900980417     C  N30BLPAAS        IFGT      WANSA
042000980417     C     BLPAAS        ORLE      WANSA
042100980417     C     BLPMGS        ANDGT     WMGSA
042200980417     C                   SETON                                        30
042300980417     C                   END
042400920805     C*
042500950920    2C     *IN30         DOWEQ     *OFF
042600940329     C                   SETOFF                                       010203
042700950601     C                   SETOFF                                       352123
042800000314     C                   SETOFF                                       112712
042900070108     C                   SETOFF                                       282917
043000120127     C                   SETOFF                                       069640
043100950920     C                   SETON                                        10
043200950920     C*
043300950920     C* CONTROLLO TUTTE LE SELEZIONI EFFETTUATE PER QUELLA LINEA
043400950920     C                   Z-ADD     1             X
043500950920    3C     *IN10         DOWEQ     *ON
043600950920     C     COMLNP        LOOKUP    SKI(X)                                 10
043700950920     C*
043800950920     C  N10              SETON                                        35
043900950920     C   10              EXSR      SELEZ
044000950920     C*
044100950920     C  N35              SETOFF                                       10
044200020909     C   35              Z-ADD     X             XX
044300950920     C   35
044400950920     CAN 10              SETOFF                                       35
044500980108     C                   ADD       1             X
044600950920     C                   ENDDO
044700920807     C*
044800950531    3C     *IN35         IFEQ      *OFF
044900990720     C* ESCLUDO I PREPAGATI NON CONFERMATI
045000990720     C     BLPPDR        ANDGT     0
045100091013     c
045200091013     c* controllo il cod trattamento merce
045300091013     c                   setoff                                       85
045400091013     c                   if        not *in25
045500091013     c     blpctm        lookup    ctma                                   85
045600091013     c                   endif
045700091013     c*
045800091013     c* Per quadratura entrata elaboto solo i CTM non autosopunta
045900091013   3ac                   if        not *in85 or *in25
046000950605     C*
046100080626     C* Se non è locale elaboro sempre
046200080626    4C     BLPTFP        IFNE      BLPTFA
046300091013     C     BLPLNP        ANDNE     BLPLNA
046400080626     C* ELABORO LE LOCALI DEI CLIENTI CON serie
046500080626     C     BLPNRS        ORGT      0
046600080626     c
046700091013     c* Quadratura con spunte entrata -->
046800080626     C     *IN25         OREQ      *OFF
046900080626     c
047000080626     c* Quadratura con spunte partenza: le locali serie 00 elaborate
047100080626     c*                    a richiesta VID9000
047200080626     c     *in25         oreq      *on
047300080626     c     blpnrs        andeq     0
047400080626     c     vid9000       andeq     'S'
047500080626     c
047600961025     C* SE ELABORAIZONE CON EDI LEGGO SEMPRE ANCHE LE BOLLE LOCALI
047700080626     C     EDSEL         OREQ      'S'
047800080626     c
047900080617     c***
048000940426     C* 19 ON  - ELABORO ALMENO UNA BOLLA
048100940426     C                   SETON                                        19
048200920804     C* CHAINO DETTAGLIO BOLLA
048300941228     C     KBLT          SETLL     FNBLT01L
048400941228     C     KBLT          READE     FNBLT01L                               32
048500950605    5C     *IN32         DOWEQ     '0'
048600950531     C*
048700950531     C  N25              EXSR      SPUENT
048800980402     C* ESCLUDO NELLA QUADRATURA CON SPUNTE ENTRATA I COLLI
048900980402     C*  CON PISTOLA 83 IDD IN PARTENZA, COLLI NON PERVENUTI DAL PARTNE
049000980402     C   25BLTNPP        IFNE      83
049100171019     c* Escludo anche le spunte pistola 98 se richiesto
049200171019     c                   if        vidmanca='N' or bltnpp<>98
049300980402     C                   EXSR      SPUPAR
049400980402     C                   ENDIF
049500171019     C                   ENDIF
049600920804     C*
049700940329     C                   SETOFF                                       010203
049800950606     C                   SETOFF                                       212311
049900000320     C                   SETOFF                                       271228
050000120127     C                   SETOFF                                       291740
050100980416     C     KBLT          READE     FNBLT01L                               32
050200950605    5C                   ENDDO
050300080626    4C                   ENDIF
050400091013   3aC                   ENDIF
050500950605    3C                   ENDIF
050600920804     C*
050700010717     C     COMLNP        READE     FNBLP31L                               30
050800980417     C* ESCO DAL CICLO SE SUPERATA DATA AL
050900980417     C  N30BLPAAS        IFGT      WANSA
051000980417     C     BLPAAS        ORLE      WANSA
051100980417     C     BLPMGS        ANDGT     WMGSA
051200980417     C                   SETON                                        30
051300980417     C                   END
051400950113    2C                   END
051500920804     C*
051600950920     C                   ADD       1             Y
051700950113    1C                   END
051800950703     C**
051900070109     C**  PREPARA  LA SKIERA DI LETTURA DELLE LNP+tfp
052000070109     C                   EXSR      PREtfp
052100070109     c* ordino la schiera
052200070109     c                   sorta     lp3
052300070109     c                   z-add     1             EE
052400070109     c                   clear                   savtfp            3 0
052500070109     c
052600070109    1c                   do        99            ee
052700070109    2c                   if        lp3(ee)>0
052800070109     c* A cambio terminal di partenza eseguo lettura
052900070109     c                   movel     lp3(ee)       ktfp
053000070109     c
053100070109    3c                   if        ktfp<>savtfp
053200070109     C**
053300070109     C**  PREPARA  LA SKIERA DI LETTURA DELLE LNP aggiuntive da tabella
053400070109     c**  5F + £1 di ktfp
053500070109     C                   EXSR      Carfas
053600070109     C                   EXSR      PRELP3
053700930528     C*
053800950601     C* QUADRATURA CON SPUNTE ENTRATA
053900070109    4C     *IN25         IFEQ      *OFF
054000950601     C                   Z-ADD     5             WNPG
054100950601     C                   Z-ADD     VIDNFV        WNFV
054200950601     C                   Z-ADD     VIDFIL        WFGS
054300050920     c                   z-add     wfedfv        wdatafv
054400950601     C*
054500950601     C                   EXSR      LEGBRV
054600070109   x4C                   ELSE
054700950605     C*
054800950601     C* QUADRATURA CON SPUNTE PARTENZA
054900950601     C                   Z-ADD     1             WNPG
055000950601     C     KFGV          SETLL     FNFGV002
055100950601     C     KFGV          READE     FNFGV002                               30
055200070109    5C     *IN30         DOWEQ     *OFF
055300950601     C                   Z-ADD     FGVNFV        WNFV
055400950601     C                   Z-ADD     FGVLNP        WFGS
055500050920     c                   z-add     fgvdfv        wdatafv
055600950601     C*
055700950601     C                   EXSR      LEGBRV
055800950601     C     KFGV          READE     FNFGV002                               30
055900070109    5C                   ENDDO
056000950601     C* LEGGO FOGLIO IMP
056100950601     C                   Z-ADD     3             WNPG
056200020918      * devo leggere per tutti i p.o. del terminal di partenza
056300070109    5c                   do        30            xy
056400070109    6c                   If        lin(xy) = *Zeros
056500020918     c                   Leave
056600070109    6c                   EndIf
056700070109     c                   Movel     Lin(xy)       Kfgs
056800020918     C     KFVV2         SETLL     FNFVV002
056900020918     C     KFVV2         READE     FNFVV002                               30
057000070109    6C     *IN30         DOWEQ     *OFF
057100950601     C*
057200070109    7C     FVVSPG        IFEQ      'P'
057300950601     C                   Z-ADD     FVVNFV        WNFV
057400950601     C                   Z-ADD     FVVFGS        WFGS
057500050920     c                   z-add     fgvdfv        wdatafv
057600950601     C*
057700950601     C                   EXSR      LEGBRV
057800070109    7C                   ENDIF
057900950601     C*
058000020918     C     KFVV2         READE     FNFVV002                               30
058100070109    6C                   ENDDO
058200101202      *
058300070109    5c                   EndDo
058400950601     C*
058500070109    4C                   ENDIF
058600070109     c                   eval      savtfp=ktfp
058700070109    3C                   ENDIF
058800070109    2C                   ENDIF
058900070109    1C                   ENDdo
059000110715      *
059100110715      * Stampa dati registrati nel work-file WFQEM00F
059200110715     c                   exsr      sr_StampaWF
059300911204     C*
059400940421     C* 19 ON  - HO ELABORATO ALMENO UNA BOLLA
059500950113    1C     *IN19         IFEQ      *ON
059600940421     C* SE NON CI SONO ANOMALIE STAMPO MESSAGGIO DI O.K.
059700940426     C  N20              EXCEPT    OK
059800950113   X1C                   ELSE
059900940421     C* SE NON HO ELABORATO NESSUN DATO STAMPO MESSAGGIO
060000940421     C                   EXCEPT    NOELAB
060100950113    1C                   ENDIF
060200950601     C*
060300950601     C* STAMPO DICITURA DI FINE STAMPA
060400110715     C                   EXCEPT    PIEDI
060500920916     C*
060600950531    1C  N25VIDSN         IFEQ      'S'
060700140717     c* se quadratura definitiva per edi non chiudo il foglio ma scrivo file
060800140717     c* FNQUE per memorizzare i parametri di elaborazione che servono al filtro
060900140717     c* per impedire lanci ripetuti con gli stessi parametri
061000140717     c                   if        edsel<>'S'
061100950113     C     KFVV          CHAIN     FNFVV000                           32
061200920916     C*
061300920916     C  N32              MOVEL     'S'           FVVFCF
061400950113     C  N32              UPDATE    FNFVV000
061500140717     c                   else
061600140717     c                   if        wque='1'
061700140717     c                   exsr      sr_FNQUE
061800140717     c                   endif
061900140717     c                   endif
062000950113    1C                   END
062100020909     C                   CLEAR                   FNLV53DS
062200980324     C                   MOVEL     'C'           D53TLA
062300980324     C                   CALL      'FNLV53R'
062400020909     C                   PARM                    FNLV53DS
062500070109     C                   CLEAR                   FNLV55DS
062600070109     C                   MOVEL     'C'           D55TLA
062700070109     C                   CALL      'FNLV55R'
062800070109     C                   PARM                    FNLV55DS
062900920424     C*
063000020909     C                   MOVEL     *ON           *INLR
063100911018     C*
063200140717     C*--- SCRITTURA FILE FNQUE: parametri di elaboraz. def. x EDI---*
063300140717     C     SR_FNQUE      BEGSR
063400140717     c                   clear                   fnque000
063500140717     c                   z-add     vidnfv        quenfv
063600140717     c                   z-add     5             quenpg
063700140717     c                   eval      QUEFGS=vidfil
063800140717     c                   eval      dataiso=%date()
063900140717     c                   eval      QUEDEL=%dec(dataiso)
064000140717     c                   eval      QUEUSR=knmus
064100140717     c* ciclo di write per ogni elemento della schiera dei parametri di lancio
064200140717     c                   for       I=1 to %elem(lnp)
064300140717     c                   if        lnp(i)>0 or
064400140717     c                             cpc(i)>0 or
064500140717     c                             cli(i)>0 or
064600140717     c                             drt(i)>0 or
064700140717     c                             nrt(i)<> *blanks or
064800140717     c                             mop(i)<> *blanks
064900140717     c                   eval      QUELNP=lnp(i)
065000140717     c                   eval      QUEPDR=cpc(i)
065100140717     c                   eval      quenrs=nrs(i)
065200140717     c                   eval      QUECCM=cli(i)
065300140717     c                   eval      QUEDRT=drt(i)
065400140717     c                   eval      quenrt=nrt(i)
065500140717     c                   eval      QUEFPP=mop(i)
065600140717     c                   write     fnque000
065700140717     c                   endif
065800140717     c                   endfor
065900140717     c                   endsr
066000930318     C*--- IMPOSTO CAMPI FISSI --------------------------------------*
066100911018     C     IMPO          BEGSR
066200950113     C                   Z-ADD     0             SAVSPE
066300950113     C* PRENDO LA DATA DEL GIORNO DA TIME
066400020909     C                   TIME                    W0140
066500020909     C                   MOVE      W0140         UDATE8
066600020909     C                   MOVEL     W0140         UTIME
066700930125     C*
066800020909     C                   MOVEL     *ON           *INOF
066900930319     C*
067000930527     C* CONTROLLO SE RICHIESTA STAMPA ANOMALIE DI MANCA BOLLA E COLLI
067100930527     C*   ECCEDENTI
067200930319     C     VIDSTA        IFEQ      'S'
067300930319     C                   SETON                                        18
067400020909     C                   MOVEL     'SI'          COMSTA
067500930319     C                   ELSE
067600930319     C                   MOVEL     'NO'          COMSTA
067700930319     C                   END
067800080626     C* CONTROLLO SE RICHIESTA STAMPA bolle locali
067900080626     C     VID9000       IFEQ      'S'
068000080626     C                   MOVEL     'SI'          COM9000
068100080626     C                   ELSE
068200080626     C                   MOVEL     'NO'          COM9000
068300080626     C                   END
068400070108     C
068500070108     C* LA RICHIESTA VALE SIA PER QUADRATURA CON SPU PARTENZE CHE
068600070108     C*  ENTRATA MA CON SIGNIFICATI DIVERSI
068700070108     C     VIDSN         IFEQ      'S'
068800070108     C                   MOVEL     'SI'          COMSN
068900070108     C                   ELSE
069000070108     C                   MOVEL     'NO'          COMSN
069100070108     C                   END
069200171019
069300171019     C* LA RICHIESTA VALE SIA PER QUADRATURA CON SPU PARTENZE CHE
069400171019     C*  ENTRATA MA CON SIGNIFICATI DIVERSI
069500171019     C* Quadratura entrata: esclusione i mancanti spuntati in latro foglio
069600171019     C* Quadratura partenza:esclusione pistole 98
069700130226     C     VIDmanca      IFEQ      'S'
069800130226     C                   MOVEL     'SI'          COMmanca
069900130226     C                   ELSE
070000130226     C                   MOVEL     'NO'          COMmanca
070100130226     C                   END
070200980416     C* IMPOSTO DATA SPED.AL PER LETTURE:SE COMDSA=0 -> WDSA=COMDSP;
070300980416     C* ALTRIMENTI WDSA=COMDSA
070400980416     C     COMDSA        IFEQ      *ZEROS
070500980416     C                   Z-ADD     COMDSP        WDSA
070600980416     C                   ELSE
070700980416     C                   Z-ADD     COMDSA        WDSA
070800980416     C                   END
070900171023     c
071000171023     c** Per il caricamento delle fls/nrs confermabili
071100171023     c* sottraggo alla data del giorno 2 giorni lavorativi
071200171023     c                   clear                   xgiolavds
071300171023     c                   eval      IXGLDATA=comdsp
071400171023     c                   eval      IXGLPA  ='P'
071500171023     c                   eval      IXGLSUB ='S'
071600171023     c                   eval      IXGLGG  =1
071700171023     c                   eval      IXGLLAV ='S'
071800171023     c                   eval      IXGLFIL =simfel
071900171023     c                   call      'XGIOLAV'
072000171023     c                   parm                    xgiolavds
072100171023     c                   if        OXGLDATA=0
072200171023     c                   eval      oxgldata=comdsp
072300171023     c                   endif
072400171023     c*  evo leggere le bolle a DSD - 1 lavorativo (potrebbe avanzare
072500171023     c*  un collo dal giorno prima)
072600950601     C**
072700950601     C* GIRO DATA SPEDIZIONE IN GG/MM/AA
072800950601     C                   Z-ADD     COMDSP        DATA
072900950601     C                   Z-ADD     MM            GGMM
073000950601     C                   MOVEL     GG            GGMM
073100950601     C                   Z-ADD     AA            VIDDSP
073200950601     C                   MOVEL     GGMM          VIDDSP
073300980416     C*
073400980416     C                   Z-ADD     WDSA          DATA
073500980416     C                   Z-ADD     MM            GGMM
073600980416     C                   MOVEL     GG            GGMM
073700980416     C                   Z-ADD     AA            VIDDSA
073800980416     C                   MOVEL     GGMM          VIDDSA
073900110718      /free
074000110718         // -?Pulizia work-file?
074100110718         Qcmd = 'DLTF file(QTEMP/WFQEM00F)';
074200110718         exsr  sr_ExecCmd;
074300110718         Qcmd = 'CRTDUPOBJ obj(WFQEM00F) +
074400110718                           fromlib(*libl) +
074500110718                           objtype(*file) +
074600110718                           tolib(QTEMP) +
074700110718                           data(*no)';
074800110718         exsr  sr_ExecCmd;
074900110718         // -?Apertura dello stesso?
075000110718         open WFQEM00F;
075100110718      /end-free
075200950530     C**
075300950530     C* SOLO SE RICHIESTO FOGLIO ENTRATA
075400950530     C     *IN25         IFEQ      *OFF
075500950601     C                   Z-ADD     VIDNFV        COMNFV
075600950530     C*
075700920409     C*
075800930318     C* PRENDO DATA E DESCRIZIONE DEL FOGLIO ENTRATA
075900950531     C                   Z-ADD     5             WNPG
076000950113     C     KFVV          CHAIN(N)  FNFVV000                           32
076100920409     C*
076200920421     C     *IN32         IFEQ      '0'
076300950113     C                   Z-ADD     FVVDFV        DATA
076400020909     C                   Z-ADD     MM            GGMM
076500950113     C                   MOVEL     GG            GGMM
076600950113     C                   Z-ADD     AA            STADFV
076700950113     C                   MOVEL     GGMM          STADFV
076800961022     C                   MOVEL     FVVDFV        WFEDFV
076900930318     C*
077000950113     C                   MOVEL     FVVDSF        DESNFV
077100920421     C                   END
077200930318     C*
077300930318     C* DECODIFICA DELLA FILIALE IN GESTIONE
077400930318     C     VIDFIL        CHAIN     AZORG                              32
077500020909     C  N32              MOVEL     ORGDES        DESFIL
077600930318     C   32              MOVEL     *BLANKS       DESFIL
077700950601     C                   ENDIF
077800961022     C*
077900961022     C* SE ELABORAZIONE DEFINITIVA E RICHIESTA EDI APRO IL FILE
078000961025     C     EDSEL         IFEQ      'S'
078100961025     C     VIDSN         ANDEQ     'S'
078200980108     C* ESEGUO CARICAMENTO DATI TABELLA PT E PU DA EDTAB
078300121102     C******             EXSR      CARPU
078400980108     C*
078500020909     C                   MOVEL     'S'           EDICON
078600961028     C                   SETON                                        26
078700980108     C                   OPEN      EDBRV00F
078800140717     C                   OPEN      FNQUE00F
078900961025     C*
079000961025     C* CALL AL PGM CHE CANCELLA EVENTUALI RECORD GIA' PRESENTI
079100020909     C                   DO        12            Y
079200980108     C     CLI(Y)        IFNE      0
079300020909     C                   CLEAR                   TRTC87DS
079400980108     C                   MOVEL     CLI(Y)        D87CCM
079500961022     C                   MOVEL     VIDNFV        D87NFE
079600961022     C                   MOVEL     WFEDFV        D87DFE
079700020909     C                   MOVEL     TRTC87DS      KPJBU
079800961022     C                   CALL      'TRTC87R4'
079900961022     C                   PARM                    KPJBA
080000980108     C                   ELSE
080100980108     C     LNP(Y)        IFNE      0
080200020909     C                   CLEAR                   TRTC87DS
080300980108     C                   MOVEL     LNP(Y)        D87LNP
080400980108     C                   MOVEL     VIDNFV        D87NFE
080500980108     C                   MOVEL     WFEDFV        D87DFE
080600020909     C                   MOVEL     TRTC87DS      KPJBU
080700980108     C                   CALL      'TRTC87R4'
080800980108     C                   PARM                    KPJBA
080900980108     C                   END
081000980108     C                   END
081100980108     C                   END
081200980108     C*
081300961025     C* LETTURA FITTIZIA DEL FILE PER GIUSTIFICARE L'APERTURA IN INPUT
081400961025     C*  PER CHIAREZZA HO PREFERITO RIDENOMINARE I CAMPI
081500961025     C  N30
081600961025     CAN 30              READ      EDBRV000                               30
081700961022     C                   ENDIF
081800980108     C*
081900911018     C                   ENDSR
082000911015     C*
082100930318     C*--- STAMPA PARAMETRI RICHIESTI --------------------------------*
082200930318     C     VIDEO         BEGSR
082300930318     C*
082400930318     C                   EXCEPT    RICHI1
082500930318     C*
082600930318     C                   Z-ADD     1             X
082700950323     C     X             DOWLE     12
082800930615     C     LNP(X)        ANDNE     0
082900930318     C*
083000930319     C                   SETOFF                                       2324
083100020909     C                   MOVEL     *BLANKS       DESLNP
083200020909     C                   MOVEL     *BLANKS       DESCPC
083300020909     C                   MOVEL     *BLANKS       DESCLI
083400020909     C                   Z-ADD     0             VIDDRT
083500930319     C*
083600930318     C* DECODIFICO LINEA DI PARTENZA
083700930318     C     LNP(X)        CHAIN     AZORG                              32
083800930319     C  N32              MOVEL     ORGDES        DESLNP
083900930318     C   32              MOVEL     *BLANKS       DESLNP
084000930318     C*
084100930318     C* DECODIFICO CODICE PADRONCINO
084200930318     C     CPC(X)        IFNE      0
084300930319     C                   SETON                                        23
084400021203     C*    CPC(X)        CHAIN     FNAPD01L                           32
084500021205     C                   move      CPC(X)        kpdpdr
084600021203     C                   move      'A'           kpdtip
084700021203     C     kfapd         chain     fiapd01l                           32
084800950113     C  N32              MOVEL     APDRSC        DESCPC
084900950113     C   32              CLEAR                   DESCPC
085000930318     C                   END
085100930318     C*
085200930524     C* NUMERO SERIE
085300060310     C     NRS(X)        IFEQ      '99'
085400020909     C                   MOVEL     *BLANKS       VIDNRS
085500930524     C                   ELSE
085600930524     C                   MOVEL     NRS(X)        VIDNRS
085700930524     C                   END
085800930524     C*
085900930318     C* DECODIFICO CODICE CLIENTE
086000930318     C     CLI(X)        IFNE      0
086100930319     C                   SETON                                        24
086200930318     C     KACO          CHAIN     CNACO000                           32
086300930319     C  N32              MOVEL     ACORAG        DESCLI
086400930318     C   32              MOVEL     *BLANKS       DESCLI
086500930318     C                   END
086600930318     C*
086700930318     C* GIRO DATA RITIRO MERCE
086800930318     C     DRT(X)        IFNE      0
086900930318     C                   Z-ADD     DRT(X)        DATA
087000950113     C                   Z-ADD     MM            GGMM
087100950113     C                   MOVEL     GG            GGMM
087200950113     C                   Z-ADD     AA            VIDDRT
087300950113     C                   MOVEL     GGMM          VIDDRT
087400930318     C                   END
087500930318     C*
087600930318     C                   EXCEPT    RICHI2
087700930318     C                   ADD       1             X
087800930318     C                   END
087900930318     C                   ENDSR
088000930318     C*
088100920804     C*--- CARICO TABELLA FILIALI GESTITE £1 -------------------------*
088200920804     C     CARTAB        BEGSR
088300941228     C***
088400941228     C* CARICO TABELLA FILIALI GESTITE £1
088500941228     C***
088600020909     C                   CLEAR                   TRUL06DS
088700941228     C                   MOVE      '£1'          D06COD
088800941228     C                   MOVEL     SIMFEL        D06KEY
088900061215     c                   move      'S'           d06key
089000061215     c                   movel     comdsp        d06drf
089100020909     C                   MOVEL     TRUL06DS      KPJBU
089200941228     C*
089300941228     C                   CALL      'TRUL06R'
089400941228     C                   PARM                    KPJBA
089500020909     C                   MOVEL     KPJBU         TRUL06DS
089600941228     C                   MOVEA     LIN           L1
089700980324     C***
089800980324     C*  CODICI PISTOLA CON PISTOLA AUTOMATICA
089900980324     C***
090000980324     C                   Z-ADD     1             X
090100020909     C                   Z-ADD     1             Y
090200980324     C                   MOVEL     '7J'          COD
090300980324     C     KTAB2         CHAIN     TABEL00F                           31
090400980324     C*
090500980324     C     *IN31         DOWEQ     *OFF
090600980324     C                   MOVEL     TBLUNI        DS7J
090700980324     C     §7JRPS        IFEQ      'A'
090800980324     C                   MOVEL     TBLKEY        NPS(X)
090900980324     C                   ADD       1             X
091000980324     C                   ENDIF
091100980324     C     §7JFSL        IFEQ      'F'
091200980402     C                   MOVEL     TBLKEY        NPF(Y)
091300980324     C                   ADD       1             Y
091400980324     C                   ENDIF
091500980324     C     KTAB2         READE     TABEL00F                               31
091600980324     C                   ENDDO
091700091013     C***
091800091013     C*  carico i codici trattamento merce autospunta
091900091013     C***
092000091013     C                   Z-ADD     1             X
092100091013     C                   MOVEL     '1B'          COD
092200091013     C     KTAB2         CHAIN     TABEL00F                           31
092300091013     C*
092400091013     C     *IN31         DOWEQ     *OFF
092500091013     C                   MOVEL     TBLUNI        DS1B
092600091013     C     §1bfg5        IFEQ      'N'
092700091013     C                   MOVEL     TBLKEY        ctma(X)
092800091013     C                   ADD       1             X
092900091013     C                   ENDIF
093000091013     C     KTAB2         READE     TABEL00F                               31
093100091013     C                   ENDDO
093200950703     C*
093300130225     c* carico  eventuali colli disk B che partono da TFP diversi
093400130225     c                   clear                   flsnrs
093500130225     c                   clear                   xcotfp
093600130225     C                   Z-ADD     1             X
093700130225     c*
093800130225     C                   MOVEL     'XCO'         kcod
093900130225     C                   eval      kke1='ALTRITFP'
094000130225     C     KTbe          setll     Tntbe01l
094100130225     C     KTbe          reade     tntbe01l
094200130225     c
094300130225    1c                   dow       not %eof(tntbe01l)
094400130225    2c                   if        tbeatb=' '
094500130225     C                   MOVEA     TBeUNI        xcotfp
094600130225     c                   movel     simfel        simalfa           3
094700130225     c                   eval      Indx=%lookup(simalfa:xcotfp)
094800130225    3c                   if        Indx>0
094900130225     C                   MOVE      TBeke2        flsnrs(X)
095000130225     C                   ADD       1             X
095100130225    4C     X             IFGE      $MaxElem
095200130225     C                   SETON                                        31
095300130225    4C                   ENDIF
095400130225    3C                   END
095500130225    2C                   END
095600130225     c
095700130225     C     KTbe          reade     tntbe01l
095800130225    1C                   ENDDO
095900130225
096000920804     C                   ENDSR
096100070109     C**********************************************************************
096200070109     C*  FILIALE DI ALTRI terminal confermabili
096300070109     C**********************************************************************
096400070109     c     CARFAS        BEGSR
096500070109     C***
096600070109     C* CARICO TABELLA FILIALI GESTITE da tfp
096700070109     C***
096800070109     C                   CLEAR                   TRUL06DS
096900070109     C                   MOVE      '£1'          D06COD
097000070109     C                   MOVEL     ktfp          D06KEY
097100070109     c                   move      'S'           d06key
097200070109     c                   movel     comdsp        d06drf
097300070109     C                   MOVEL     TRUL06DS      KPJBU
097400070109     C*
097500070109     C                   CALL      'TRUL06R'
097600070109     C                   PARM                    KPJBA
097700070109     C                   MOVEL     KPJBU         TRUL06DS
097800101202     c*
097900070109     c                   clear                   FAS
098000070109     c                   clear                   LAS
098100070109     C                   Z-ADD     1             X
098200171019     C                   Z-ADD     1             Z
098300101202     c*
098400170906     C**                 MOVEL     '5F'          COD
098500170906     C**                 MOVEL(P)  ktfp          KEY
098600170906     C**                 MOVE      *ALL'9'       WMAX
098700170906     C**                 MOVEL     ktfp          WMAX
098800170906     C**   KTAB          SETLL     TABEL00F
098900170906     C**   KTAB2         READE     TABEL00F                               31
099000170906     C**   *IN31         DOWEQ     *OFF
099100170906     C**   TBLKEY        ANDLE     WMAX
099200170906     C**   TBLFLG        IFEQ      ' '
099300170906     C**                 MOVE      TBLKEY        FAS(X)
099400170906     C**   X             OCCUR     DSLAS
099500170906     C**                 MOVEA     TBLUNI        LAS
099600170906     C**                 ADD       1             X
099700170906    4C**   X             IFGE      $MaxElem
099800170906     C**                 SETON                                        31
099900170906    4C**                 ENDIF
100000170906     C**                 END
100100170906     C**N31KTAB2         READE     TABEL00F                               31
100200170906     C**                 ENDDO
100300170906
100400170906     c* lettura con sql di fls/nrs confermabili
100500170906
100600170906      /free
100700170907    w  wsql='select distinct blptfp, blplnp, blpfls, blpnrs from fnblp00f  +
100800171023    '  where blpnrs>0 and (blpaas*10000+blpmgs) between ' +
100900171023    '  %editc(oxgldata :'X')+ ' and '+ %editc(wdsa:'X') + ' AND BLPFLS<>BLPLNP';
101000170906
101100170906      // -?Dichiarazione cursore?
101200170906         exec sql   prepare S2   from :wSQL;
101300170906         exec sql   declare C2   cursor for S2;
101400170906
101500170906         // -?Apertura del cursore?
101600170906         exec sql   open C2;
101700170906
101800170906         if  SQLcode < *zero;
101900170906           exsr  sr_PrintErrSQL;
102000170906           leavesr;
102100170906         endif;
102200170906
102300170906         // -?Ciclo di lettura wf?
102400171019         exec sql   fetch next   from C2   into :blptfp, :blplnp,
102500171019                                                :blpfls, :blpnrs;
102600170906
102700170906         DoW  SQLcode <> 100;
102800170906
102900170906           if  SQLcode < *zero;
103000170906             exsr  sr_PrintErrSQL;
103100170906             leavesr;
103200170906           endif;
103300170906
103400170906           exsr  sr_caricafls;
103500171019
103600171019         exec sql   fetch next   from C2   into :blptfp, :blplnp,
103700171019                                                :blpfls, :blpnrs;
103800170906
103900170906         EndDo;
104000170906
104100170906         // -?Chiusura del cursore?
104200170906         exec sql   close C2;
104300170906      /end-free
104400170906
104500150623     c* Accodi alla FAS anche le lnp serie 00 confermabili qui
104600150623     c*  da file FIBSP
104700150623    0c                   if        x<$MaxElem
104800150623     c
104900150623     c     000           setgt     fibsp01l
105000150623     c                   read      fibsp01l
105100150623    1c                   dow       not %eof(fibsp01l)
105200150623    2c                   if        bspfil>0 and bspfpa='S'
105300150623     c                   z-add     1             BB
105400150623    3c                   dow       lin(bb)>0
105500150623    4c                   if        %lookup(lin(BB):sbsp)>0
105600150623     c                   eval      fas(X)=bspfil*100
105700150623     C     X             OCCUR     DSLAS
105800150623     C                   MOVEA     bsplin        LAS
105900150623     C                   ADD       1             X
106000150623     c                   leave
106100150623    4c                   endif
106200150623     c                   add       1             BB
106300150623    3c                   enddo
106400150623     c
106500150623    2c                   endif
106600150623    2C     X             IFGE      $MaxElem
106700150623     c                   leave
106800150623    2c                   endif
106900150623
107000150623     c                   read      fibsp01l
107100150623    1c                   enddo
107200150623    0c                   endif
107300150623     c
107400070109     c                   ENDSR
107500170906
107600170906      * Carico tutte le filiali segnacollo confermabili dal terminal.SIMFEL --*
107700170906     C     sr_caricafls  BEGSR
107800170907    1C                   if        blptfp=ktfp and blpfls<>blplnp
107900170907     c* non deve essere nemmeno
108000170907     C                   z-add     blpnrs        w0050
108100170907     C                   movel     blpfls        w0050             5 0
108200170907     C                   eval      Indx=%lookup(w0050:fas)
108300170906    2C                   if        indx=0
108400171019    3c                   if        Z <=  $MaxElem
108500171019     C                   move      w0050         FAS(Z)
108600171019     c                   eval      x=Z
108700171019     c                   add       1             Z
108800170906    3c                   endif
108900170906     c
109000170906   x2C                   else
109100170906     C                   eval      x=indx
109200170906    2C                   endif
109300170906
109400170906     c     X             OCCUR     DSLAS
109500171019     c                   eval      indx=%lookup('   ':las)
109600170907    2c                   if        indx>0
109700170907     c                   eval      las(indx)=%editc(blplnp:'X')
109800170906    2c                   endif
109900171019
110000171019    1C                   endif
110100170906
110200170906     c                   ENDSR
110300930527     C*
110400110715      * REGISTRO ERRORE NEL WORK-FILE -------------------------------*
110500110715     ***C     STAMPA        BEGSR
110600110715     c     sr_WriteWF    BEGSR
110700110715     c*
110800171019     C* SE CONTROLLO IN EDI NON STampo SE COLLO CONSEGNATO ALTRIMENTI
110900980623     C** SEMPRE
111000110715if  1C     EDSEL         IFEQ      'S'
111100980623     C     BLPDCM        ANDEQ     0
111200980623     C     EDICON        ORNE      'S'
111300980623     C     *IN05         OREQ      '1'
111400980623     C**
111500110715      *
111600110715      * REGISTRO RECORD DI DETTAGLIO
111700110715      *
111800110719if  2c                   if        Not *in05
111900110718     c                   eval      WQEaas = BLPaas
112000110718     c                   eval      WQElnp = BLPlnp
112100110718     c                   eval      WQEnrs = BLPnrs
112200110718     c                   eval      WQEnsp = BLPnsp
112300110718     c                   eval      WQEmgs = BLPmgs
112400110718     c                   eval      WQElna = BLPlna
112500110718     c                   eval      WQErsm = BLPrsm
112600110720     c                   eval      WQEfls = BLTfls
112700110718     c                   eval      WQEnsc = BLTnsc
112800110719x   2c                   else
112900110718     c                   eval      WQEaas = *hival
113000110718     c                   eval      WQElnp = BRVlnp
113100110720     c                   eval      WQEfls = BRVlnp
113200110718     c                   eval      WQEnrs = BRVnrs
113300110718     c                   clear                   WQEnsp
113400110718     c                   eval      WQEmgs = *hival
113500110718     c                   eval      WQElna = BRVlna
113600110718     c                   eval      WQErsm = *hival
113700110718     c                   eval      WQEnsc = BRVnsc
113800110719e   2c                   endif
113900110718      *
114000110718     c                   movea     *in           WQEind
114100110715      *
114200110715     c                   write     WFQEM000
114300140717     c                   eval      wque='1'
114400110715      *
114500110715     C**
114600110719e   1C                   ENDIF
114700900622     C*
114800900622     C                   ENDSR
114900950531     C*
115000950531     C*  SELEZIONI FATTE NEL FILTRO ----------------------------------*
115100950531     C     SELEZ         BEGSR
115200970529     C* SE X E' > DI 12 (MASSIME SELEZIONI) --> FACCIO FINA CHE VADA
115300970529     C*  TUTTO BENE PERCHE' TANTO E' COSI' PERCHE' HANNO CHIESTO
115400970529     C*  TUTTE LA L1
115500970529    0C     X             IFLE      12
115600970529     C**
115700950531     C* SE IMMESSO CONTROLLO CODICE PADRONCINO
115800980409    1C  N35CPC(X)        IFNE      0
115900950531     C     CPC(X)        ANDNE     BLPPDR
116000950531     C                   SETON                                        35
116100950531    1C                   END
116200950531     C*
116300950531     C* SE SERIE >< 99 CONTROLLO
116400060310     c                   select
116500101202     c*
116600060310    1C                   when      nrs(x)='99'
116700101202     c*
116800060310     c                   when      nrs(x)='SI'
116900060310     c                   if        blpnrs=0
117000060310     C                   SETON                                        35
117100060310     c                   endif
117200101202     c*
117300060310    1C                   other
117400060310     c                   movel     blpnrs        alfanrs           2
117500060310     C     NRS(X)        ifne      alfanrs
117600950531     C                   SETON                                        35
117700950531    1C                   END
117800060310    1C                   ENDsl
117900950531     C*
118000950531     C* SE IMMESSO CONTROLLO CODICE CLIENTE
118100950531    1C  N35CLI(X)        IFNE      0
118200950531    2C     BLPCCM        IFGT      0
118300950531    3C     CLI(X)        IFNE      BLPCCM
118400950531     C                   SETON                                        35
118500950531    3C                   END
118600950531     C*
118700950531   X2C                   ELSE
118800950531    3C     CLI(X)        IFNE      BLPKSC
118900950531     C                   SETON                                        35
119000950531    3C                   END
119100950531    2C                   END
119200950531    1C                   END
119300950531     C*
119400950531     C* SE IMMESSA CONTROLLO DATA RITIRO
119500950531    1C  N35DRT(X)        IFNE      0
119600950531     C     DRT(X)        ANDNE     BLPDRT
119700950531     C                   SETON                                        35
119800950531    1C                   END
119900950531     C* SE IMMESSO CONTROLLO SE MATTINO O POMERIGGIO
120000950531    1C  N35MOP(X)        IFNE      *BLANKS
120100950531     C     MOP(X)        ANDNE     BLPFPP
120200950531     C                   SETON                                        35
120300950531    1C                   END
120400950531     C* SE IMMESSO CONTROLLO NUMERO RITIRO
120500950531    1C  N35NRT(X)        IFNE      *BLANKS
120600950531     C                   MOVE      NRT(X)        COMNRT
120700950531     C     COMNRT        IFNE      BLPNRT
120800950531     C                   SETON                                        35
120900950531     C                   END
121000950531    1C                   END
121100970529    0C                   END
121200950531     C                   ENDSR
121300950531     C*
121400950531     C* CHAINO SPUNTE ENTRATA ----------------------------------------*
121500950531     C     SPUENT        BEGSR
121600950531     C* CAMPI PER LA STAMPA
121700950531     C                   CLEAR                   PRTNPG
121800980402     C                   CLEAR                   PRTSPG
121900950531     C                   CLEAR                   PRTNFV
122000950601     C                   CLEAR                   PRTDFV
122100980324     C                   CLEAR                   PRTZNC
122200980402     C                   CLEAR                   PRTFGS
122300950531     C*
122400950531     C                   Z-ADD     5             WNPG
122500980324     C                   EXSR      CHABRV
122600980324     C* SE NON HO TROVATO LA SPUNTA ENTRATA CHE VOLEVO VADO
122700130225     C*  AVANTI NELLA RICERCA --> cat 1
122800980324    1C     WTROV         IFEQ      ' '
122900980324     C     PRTDFV        ANDLT     WFEDFV
123000980324     C**
123100980324     C                   Z-ADD     1             WNPG
123200130225     c                   eval      wdefl=' '
123300980324     C                   EXSR      CHABRV
123400130225    1c                   endif
123500130225     c
123600130225     c* Se segnacollo mancante --> controllo se spuntato da altro terminal
123700130225    1C     WTROV         IFEQ      ' '
123800130225     C     PRTDFV        ANDeq     0
123900130225     C                   Z-ADD     5             WNPG
124000130225     C                   EXSR      CHABRV_altr
124100130225    2C     PRTDFV        IFLT      WFEDFV
124200130225     C                   Z-ADD     1             WNPG
124300130225     C                   EXSR      CHABRV_altr
124400130225    2c                   endif
124500130225    1c                   endif
124600130225     c
124700130225     c* Se non trovato in altro TFP controllo le altre categorie
124800130225    1C     WTROV         IFEQ      ' '
124900130225     C     PRTDFV        ANDeq     0
125000130225     c
125100980324     C                   Z-ADD     3             WNPG
125200980324     C                   EXSR      CHABRV
125300130225     c
125400130225    2C     PRTDFV        IFLT      WFEDFV
125500980324     C                   Z-ADD     2             WNPG
125600980324     C                   EXSR      CHABRV
125700130225     c
125800130225    3C     PRTDFV        IFLT      WFEDFV
125900130225     C                   Z-ADD     1             WNPG
126000130225     c                   eval      wdefl='S'
126100130225     C                   EXSR      CHABRV
126200130225     c                   eval      wdefl=' '
126300130225     c
126400130225    4C     PRTDFV        IFLT      WFEDFV
126500130225     C                   Z-ADD     4             WNPG
126600980324     C                   EXSR      CHABRV
126700980324    4C                   ENDIF
126800980324    3C                   ENDIF
126900130225    2C                   ENDIF
127000980324    1C                   ENDIF
127100980324     C**
127200950531     C* 01 ON  - SEGNACOLLO MANCANTE
127300980324    1C     WTROV         IFEQ      ' '
127400980324    2C     PRTDFV        IFEQ      0
127500950531     C                   SETON                                        01
127600961022     C*
127700961022     C* SCRIVO FILE EDBRV  CODICE 001 --> MANCA COLLO
127800980324    3C     EDICON        IFEQ      'S'
127900980415    3C     BLPDCM        ANDEQ     0
128000961025     C                   MOVEL     '001'         WSTS
128100961022     C                   EXSR      WRTEDI
128200980324    3C                   ENDIF
128300950531     C*
128400980324   X2C                   ELSE
128500950531     C*
128600980324     C* 02 ON  - SEGNACOLLO SPARATO SU ALTRO FOGLIO
128700980205     C**
128800950531     C                   SETON                                        02
128900961022     C*
129000980324     C* SCRIVO FILE EDBRV  CODICE 004 --> SE DATA =
129100980324     C*  ALTRO FOGLIO STESSA DATA
129200980324    3C     EDICON        IFEQ      'S'
129300980324     C     PRTDFV        ANDEQ     WFEDFV
129400980324     C                   MOVEL     '004'         WSTS
129500961022     C                   EXSR      WRTEDI
129600980324    3C                   ENDIF
129700980324     C* SCRIVO FILE EDBRV  CODICE 005 --> SE DATA <>
129800980324     C*  ALTRO FOGLIO ALTRA  DATA
129900980324    3C     EDICON        IFEQ      'S'
130000980324     C     PRTDFV        ANDNE     WFEDFV
130100980324     C                   MOVEL     '005'         WSTS
130200980324     C                   EXSR      WRTEDI
130300980324    3C                   ENDIF
130400130226     c
130500130226     c* Spuntato su altro foglio lo memorizzo solo se non è da scludere
130600130226     c*  (VIDMANCA ='S')
130700130226    3c     vidmanca      ifeq      'S'
130800130226     c                   setoff                                       02
130900130226    3C                   ENDIF
131000130226    2C                   ENDIF
131100961022     C* SCRIVO EDI
131200980324   X1C                   ELSE
131300980402     C* SPUNTA TRAOVATA SUL FOGLIO ENTRATA IN ESAME
131400961025    2C     EDICON        IFEQ      'S'
131500961022     C* SE RICHIESTI ANCHE I POSITIVI
131600980324     C* SE NON HO AVUTO ANOMALIA COLLO BOLLA E SPUNTA ENTRATA
131700961022     C                   CLEAR                   WSTS
131800961022     C                   EXSR      WRTEDI
131900961022    2C                   ENDIF
132000980402    1C                   ENDIF
132100980402     C*
132200980402     C* 03 ON  - ZONA SEGNACOLLO DIVERSA DA ZONA BOLLA
132300170407     c* dal momento che dal 04/16 aggiorniamo la bolla con la zona del
132400170407     c*  segnacollo, questa segnalazionew non serve più
132500170407    3C**N01BLPZNC        IFNE      PRTZNC
132600170407     C***                SETON                                        03
132700170407    3C***                ENDIF
132800950531     C*
132900980402    1C     *IN01         IFEQ      *ON
133000980402     C     *IN02         OREQ      *ON
133100950601     C     *IN03         OREQ      *ON
133200110715     ***C                   EXSR      STAMPA
133300110715     c                   exsr      sr_WriteWF
133400980402    1C                   ENDIF
133500950531     C                   ENDSR
133600980324     C*
133700980324     C* CHAINO PER VEDER SE ESISTE ALTRA SPUNTA ----------------------*
133800980324     C     CHABRV        BEGSR
133900061215     c                   z-add     simfel        ktfp
134000980324     C* SE HO TROVATO LA SPUNTA ENTRATA RELATIVA AL FOGLIO CHE STO
134100980324     C*  CONTROLLANDO IMPOSTO IL CAMPO WTROV=S
134200980324     C                   CLEAR                   WTROV
134300070123     C     KBRV          CHAIN     FNBRV009                           33
134400980408    1C     *IN33         DOWEQ     *OFF
134500980324     C* SE STO LEGGENDO LE SPUNTE ENTRATA VERIFICO SE SPARATO
134600980324     C*  SUL FOGLIO IN QUADRATURA ALTRIMENTI MEMORIZZO LA DATA E
134700980324     C*  NUMERO FOGLIO PIU' ALTA CHE C'E'
134800980324     C**
134900980324    2C     WNPG          IFEQ      5
135000980324     C     BRVNFV        ANDNE     COMNFV
135100980324     C     WNPG          ORNE      5
135200980402     C* ESCLUDO LE PISTOLE AUTOGENERATE
135300020909     C                   MOVEL     BRVNPS        W002A
135400980402     C     W002A         LOOKUP    NPS                                    34
135500980324    3C     *IN34         IFEQ      *OFF
135600980324     C* PRENDO LA DATA DEL FOGLIO
135700020909     C                   CLEAR                   FNLV53DS
135800000201     C                   Z-ADD     BRVNFV        D53NFV
135900980324     C                   MOVEL     BRVNPG        D53NPG
136000980326     C                   MOVEL     BRVFGS        D53FGS
136100980324     C                   CALL      'FNLV53R'
136200020909     C                   PARM                    FNLV53DS
136300980324     C* SE NON C'E' ERRORE MEMORIZZO LA DATA PIU' ALTA
136400980324    4C     D53ERR        IFEQ      ' '
136500980324     C     D53DFV        ANDGT     PRTDFV
136600130225     c* se cat 1 elaboto defluenza o non in base alla richiesta
136700130225    5c                   if        brvnpg<>1   or d53def=wdefl
136800130225     c
136900980324     C                   MOVEL     D53DFV        PRTDFV
137000000201     C                   Z-ADD     D53NFV        PRTNFV
137100980324     C                   MOVEL     D53NPG        PRTNPG
137200980402     C                   MOVEL     D53SPG        PRTSPG
137300980324     C                   MOVEL     BRVZNC        PRTZNC
137400980402     C                   MOVEL     BRVFGS        PRTFGS
137500130225    5C                   ENDIF
137600130225    4C                   ENDIF
137700980324    3C                   ENDIF
137800980324     C**
137900070123     C     KBRV          READE     FNBRV009                               33
138000980324   X2C                   ELSE
138100980324     C                   SETON                                        33
138200020909     C                   MOVEL     'S'           WTROV
138300980402     C                   MOVEL     WFEDFV        PRTDFV
138400000201     C                   Z-ADD     BRVNFV        PRTNFV
138500980402     C                   MOVEL     BRVNPG        PRTNPG
138600980402     C                   MOVEL     BRVZNC        PRTZNC
138700980402     C                   MOVEL     BRVFGS        PRTFGS
138800980324    2C                   ENDIF
138900980408    1C                   ENDDO
139000980324     C                   ENDSR
139100950531     C*
139200130222     C* CHAINO PER VEDER SE ESISTE ALTRA SPUNTA su altro terminal ----*
139300130222     C     CHABRV_altr   BEGSR
139400130222     C* SE HO TROVATO LA SPUNTA ENTRATA RELATIVA AL FOGLIO CHE STO
139500130222     C*  CONTROLLANDO IMPOSTO IL CAMPO WTROV=S
139600130222     C                   CLEAR                   WTROV
139700130222     C     KBRV_altr     CHAIN     FNBRV009                           33
139800130222    1C     *IN33         DOWEQ     *OFF
139900130222     C* SE STO LEGGENDO LE SPUNTE ENTRATA VERIFICO SE SPARATO
140000130222     C*  SUL FOGLIO IN QUADRATURA ALTRIMENTI MEMORIZZO LA DATA E
140100130222     C*  NUMERO FOGLIO PIU' ALTA CHE C'E'
140200130222    2c                   if        brvfle<>simfel
140300130222     C**
140400130222     C* ESCLUDO LE PISTOLE AUTOGENERATE
140500130222     C                   MOVEL     BRVNPS        W002A
140600130222     C     W002A         LOOKUP    NPS                                    34
140700130222    3C     *IN34         IFEQ      *OFF
140800130222     C* PRENDO LA DATA DEL FOGLIO
140900130222     C                   CLEAR                   FNLV53DS
141000130222     C                   Z-ADD     BRVNFV        D53NFV
141100130222     C                   MOVEL     BRVNPG        D53NPG
141200130222     C                   MOVEL     BRVFGS        D53FGS
141300130222     C                   CALL      'FNLV53R'
141400130222     C                   PARM                    FNLV53DS
141500130222     C* SE NON C'E' ERRORE MEMORIZZO LA DATA PIU' ALTA
141600130222    4C     D53ERR        IFEQ      ' '
141700130222     C     D53DFV        ANDGT     PRTDFV
141800130222     C                   MOVEL     D53DFV        PRTDFV
141900130222     C                   Z-ADD     D53NFV        PRTNFV
142000130222     C                   MOVEL     D53NPG        PRTNPG
142100130222     C                   MOVEL     D53SPG        PRTSPG
142200130222     C                   MOVEL     BRVZNC        PRTZNC
142300130222     C                   MOVEL     BRVFGS        PRTFGS
142400130222    4C                   ENDIF
142500130222    3C                   ENDIF
142600130222    2C                   ENDIF
142700130222     C**
142800130222     C     KBRV_altr     READE     FNBRV009                               33
142900130222    1C                   ENDDO
143000130222     C                   ENDSR
143100130222     c*
143200950531     C* CHAINO SPUNTE PARTENZA E IMP ---------------------------------*
143300950531     C     SPUPAR        BEGSR
143400980402     C* MEMORIZZO SPUNTA DA STAMPARE
143500980402     C                   CLEAR                   PRTNPS
143600980402     C                   CLEAR                   PRTNPG
143700980402     C                   CLEAR                   PRTSPG
143800950531     C                   CLEAR                   PRTNFV
143900950531     C                   CLEAR                   PRTDFV
144000980402     C                   CLEAR                   PRTZNC
144100980402     C                   CLEAR                   PRTFGS
144200120127     C                   CLEAR                   PRTlnp
144300120127     C                   CLEAR                   PRTnrs
144400980402     C* MEMORIZZO IL COLLO QUANDO E' UN FASULLO
144500950601     C                   CLEAR                   W88NFV
144600980402     C                   CLEAR                   W88NPS
144700980402     C**MEMORIZZO SPUNTA CON DATA = ALLA DATA DI ELABORAZIONE
144800980402     C                   CLEAR                   WDTNFV
144900980402     C                   CLEAR                   WDTNPS
145000980402     C                   CLEAR                   WDTNPG
145100030624     c                   Clear                   wdtLnp
145200030624     c                   Clear                   wdtNrs
145300950531     C*
145400980402     C                   CLEAR                   WDFV
145500950531     C                   Z-ADD     1             WNPG
145600101202     c*
145700061215     c* se la lnp che sto elaborando non è in £1, carico la sua
145800061215     c*  £1 e il suo terminal di partenza
145900061215     c     comlnp        lookup    l1                                     39
146000061215     C***
146100061215     c                   if        *in39
146200061215     c                   eval      ktfp=simfel
146300061215     c                   else
146400061215     C                   CLEAR                   TRUL06DS
146500061215     C                   MOVE      'PP'          D06COD
146600061215     C                   MOVEL     comlnp        D06KEY
146700061215     c                   move      'S'           d06key
146800061215     c                   movel     comdsp        d06drf
146900061215     C                   MOVEL     TRUL06DS      KPJBU
147000061215     C*
147100061215     C                   CALL      'TRUL06R'
147200061215     C                   PARM                    KPJBA
147300061215     C                   MOVEL     KPJBU         TRUL06DS
147400061215     c                   movel     lin(1)        ktfp
147500061215     c                   endif
147600101202     c*
147700980402    0C                   DO        2
147800101202      *
147900980402     C**
148000980402     C                   CLEAR                   WSNFV
148100070123     C     KBRV          CHAIN     FNBRV009                           33
148200950531     C*
148300950531    1C     *IN33         DOWEQ     *OFF
148400980402     C* PRENDO LA DATA
148500980402    2C     BRVNFV        IFNE      WSNFV
148600020909     C                   CLEAR                   FNLV53DS
148700000201     C                   Z-ADD     BRVNFV        D53NFV
148800980402     C                   MOVEL     BRVNPG        D53NPG
148900980402     C                   MOVEL     BRVFGS        D53FGS
149000980402     C                   CALL      'FNLV53R'
149100020909     C                   PARM                    FNLV53DS
149200980402    3C     D53ERR        IFEQ      ' '
149300980402     C                   MOVEL     D53DFV        WDFV
149400980423     C                   MOVEL     D53DEF        WDEF
149500980423     C                   MOVEL     D53SPG        WSPG
149600980402     C                   ELSE
149700980402     C                   CLEAR                   WDFV
149800980423     C                   CLEAR                   WDEF
149900980423     C                   CLEAR                   WSPG
150000980402    3C                   ENDIF
150100000201     C                   Z-ADD     BRVNFV        WSNFV
150200980402    2C                   ENDIF
150300980402     C**
150400980402     C** ESCLUDO SE NON E' SPUNTA PARTENZA O SPUNTA IMP
150500980423     C**  ESCLUDO ANCHE LE SPUNTE DI DEFLUENZA
150600980423    2C     WNPG          IFEQ      1
150700980423     C     WDEF          ANDEQ     ' '
150800980423    2C     WNPG          OREQ      3
150900980423     C     WSPG          ANDEQ     'P'
151000980402     C**
151100980402     C* SPUNTA CON DATA = ALLA DATA DI ELABORAZIONE
151200980402    3C     WDFV          IFEQ      COMDSP
151300980402     C     WDTNFV        ANDEQ     0
151400980402     C                   MOVEL     BRVNFV        WDTNFV
151500980402     C                   MOVEL     BRVNPS        WDTNPS
151600980402     C                   MOVEL     BRVNPG        WDTNPG
151700030624     c                   Eval      wdtLnp = BrvLnp
151800030624     c                   Eval      wdtNrs = BrvNrs
151900980402    3C                   ENDIF
152000950601     C*
152100980402     C* MEMORIZZO SPUNTA CON DATA PIU' ALTA
152200980402    3C     WDFV          IFGT      PRTDFV
152300980403     C                   MOVEL     D53SPG        PRTSPG
152400980402     C                   Z-ADD     BRVNPG        PRTNPG
152500980402     C                   Z-ADD     BRVFGS        PRTFGS
152600980402     C                   Z-ADD     BRVNFV        PRTNFV
152700980402     C                   Z-ADD     WDFV          PRTDFV
152800950606     C                   MOVEL     BRVZNC        PRTZNC
152900980402     C                   MOVEL     BRVNPS        PRTNPS
153000120127     c                   Eval      prtLnp = BrvLnp
153100120127     c                   Eval      prtNrs = BrvNrs
153200980402    3C                   ENDIF
153300980402    2C                   ENDIF
153400950531     C*
153500070123     C     KBRV          READE     FNBRV009                               33
153600950531    1C                   ENDDO
153700980402     C** SE NON TROVATA NELLE SPUNTE PARTENZA CERCO NELLE SPUNTE IMP
153800980402     C                   Z-ADD     3             WNPG
153900980402    AC                   ENDDO
154000950531     C*
154100980402     C* VEDO SE STAMPARE QUALCHE COSA
154200980402     C* 1) COLLO MANCANTE IN DATA RICHIESTA
154300980402    1C     WDTNFV        IFEQ      0
154400980402     C                   SETON                                        01
154500980402   X1C                   ELSE
154600980402     C* 2) SE LA SPUNTA TROVATA IN QUELLA DATA E' FASULLA
154700990609     C*    ANCHE SE NELLA TABELLA NON E' CONSIDERATA FASULLA CI METTIAM
154800990609     C*    ANCHE LE 90 CHE NON SONO MAI STATE SPARATE NEMMENO IN ENTRAT
154900030624      *    ma solo se non hanno lnp 102 e serie 02
155000030624      *    (bolle recapito contrassegni)
155100980402     C                   MOVEL     WDTNPS        W002A
155200980402     C     W002A         LOOKUP    NPF                                    21
155300980402    2C     *IN21         IFEQ      *ON
155400990609     C     W002A         OREQ      '90'
155500030624     c     wdtLnp        andne     102
155600060321     C     W002A         OREQ      '90'
155700030624     c     wdtNrs        andne     02
155800990609     C                   SETON                                        21
155900980402     C                   Z-ADD     WDTNFV        W88NFV
156000020909     C                   Z-ADD     WDTNPS        W88NPS
156100980402    2C                   ENDIF
156200980402    1C                   ENDIF
156300980402     C* 3) SE E' STATA TROVATA UNA SPUNTA PARTENZA O IMP CON DATA >
156400980402     C*    SOLO SE LA SPUNTA DEL GIORNO NON ERA UNA SPUNTA IMP
156500980402     C*    (NEL CASO E' TUTTO CORRETTO)
156600980402    1C     PRTNFV        IFGT      0
156700980403    2C     PRTDFV        IFNE      COMDSP
156800980403     C     WDTNPG        ANDNE     3
156900980402     C                   SETON                                            02
157000120127     c* controllo se per caso è spunta fasulla lo stesso
157100120127     C                   MOVEL     prtnPS        W002A
157200120127     C     W002A         LOOKUP    NPF                                    21
157300120127    2C     *IN21         IFEQ      *ON
157400120127     C     W002A         OREQ      '90'
157500120127     c     prtLnp        andne     102
157600120127     C     W002A         OREQ      '90'
157700120127     c     prtNrs        andne     02
157800120127     C                   SETON                                        2140
157900120127     C                   Z-ADD     prtNFV        W88NFV
158000120127     C                   Z-ADD     prtNPS        W88NPS
158100120127    2C                   ENDIF
158200980402    2C                   ENDIF
158300980402     C* 4) SE ZONA BOLLA <> DA ZONA SEGNACOLLO
158400170407    2C***  PRTZNC        IFNE      BLPZNC
158500170407     C***                SETON                                            03
158600170407    2C***                ENDIF
158700980402    1C                   ENDIF
158800980402     C**
158900980402     C* 5) SE IL COLLO E' STATO AGGIORNATO CON PISTOLA FASULLA
159000980402     C*    MA LA SPUNTA TROVATA NELLA STESSA DATA E' REALE
159100980402    1C     BLTDFV        IFGT      0
159200980402     C                   MOVEL     BLTNPP        W002A
159300980402     C     W002A         LOOKUP    NPF                                    33
159400980402    2C     *IN33         IFEQ      *ON
159500990609     C     W002A         OREQ      '90'
159600980402    3C     PRTDFV        IFGT      0
159700980402     C     PRTNPS        ANDNE     BLTNPP
159800980403     C     PRTNPG        ANDEQ     1
159900980402     C                   SETON                                        27
160000980402    3C                   ENDIF
160100980402    2C                   ENDIF
160200980402    1C                   ENDIF
160300980402     C**
160400950601     C* SE COLLO MANCANTE O SPUNTA 88 DICO SE E' STATO SPARATO IN ENTR
160500980402     C   01
160600980402    1COR 21BLTDSE        IFGT      0
160700950601     C                   SETON                                        11
160800980402    1C                   ENDIF
160900000621      *
161000000621      * DICO SE E'STATO SPARATO IN ARRIVO - - - - - - - - - - - - - -
161100000621      *
161200160517    1c                   if        *in01 or *in21
161300000621     C* CERCO L'ANOMALIA 60 O 61
161400160517     C**                 Z-ADD     0             RIPROW
161500160517     C**   RIPROV        TAG
161600020909      *    =================
161700160517    2C**   RIPROW        IFEQ      0
161800160517     C**                 Z-ADD     60            COMCAA
161900160517    2C**                 ENDIF
162000160517    2C**   RIPROW        IFEQ      1
162100160517     C**                 Z-ADD     61            COMCAA
162200160517    2C**                 ENDIF
162300000621      *
162400160517IF  2C**   RIPROW        IFLE      1
162500160517      **
162600160517     C                   CLEAR                   DAMNPS
162700160517     C                   CLEAR                   ANMDAO
162800160517     C                   CLEAR                   DESANP
162900160517     C**   KANM2         CHAIN     FNANM02L                           31
163000160517    3C**   *IN31         IFEQ      *OFF
163100160517     C**                 MOVEL     ANMNPS        DAMNPS
163200160517    4C**   RIPROW        IFEQ      0
163300160517     C**   'ARRIVATO'    CAT       ' il':0       DESANP
163400160517     c**                 seton                                        17
163500160517     C**                 ELSE
163600160517     C**   'DISGUIDA'    CAT       'TO ':0       DESANP
163700160517    4C**                 ENDIF
163800160517     C**                 ELSE
163900160517     C**                 ADD       1             RIPROW
164000160517     C**                 GOTO      RIPROV
164100160517      **                 ----------------
164200160517    3C**                 ENDIF
164300160517     C*
164400160517    2C**                 ENDIF
164500050930     c
164600050930    2c                   if        anmdao=0
164700050930     c     kart          chain     fnart01l
164800050930    3c                   if        %found(fnart01l)
164900050930    4c                   if        artdam>0
165000050930     C                   MOVEL     artnap        DAMNPS
165100110718     C     'ARRIVATO'    CAT       ' il':0       DESANP
165200061215     c                   seton                                        17
165300050930     c                   movel     artdam        anmdao
165400050930   x4c                   else
165500050930    5c                   if        artdet>0
165600050930     c                   movel     artpet        w002a
165700050930     c     w002a         lookup    nps                                    31
165800050930    6c                   if        not *in31
165900050930     c     kblp2         chain     fnarb01l
166000050930    7c                   if        %found(fnarb01l) and arbflp=artflp
166100110718     C     'ARRIVATO'    CAT       ' il':0       DESANP
166200061215     c                   seton                                        17
166300050930     c                   else
166400110718     C     'DISGUIDA'    CAT       'TO ':0       DESANP
166500050930    7c                   endif
166600061215     c                   movel     artpet        damnps
166700050930     c                   movel     artdet        anmdao
166800050930    6c                   endif
166900050930     c                   endif
167000101202    5c*
167100050930    4c                   endif
167200050930    3c                   endif
167300050930    3c                   endif
167400101202     c*
167500000621     C*
167600000621     C     ANMDAO        IFGT      0
167700000621     C                   MOVEL     ANMDAO        DATA
167800000314     C                   Z-ADD     MM            DAMDFV
167900000314     C                   MOVEL     GG            DAMDFV
168000000320     C     DAMNPS        IFNE      *BLANKS
168100000320     C                   SETON                                        29
168200000320     C                   ENDIF
168300000314     C                   SETON                                        12
168400000621     C                   ENDIF
168500000621     C*
168600070110     C                   ENDif
168700070110     c*  se 11 acceso non devo dare segnalazione di collo arrivato
168800070110     c                   if        *in11
168900070110     c                   setoff                                       2912
169000070110     c                   endif
169100000621      *
169200061215      * Solo per errori 01 o 21, se trovato collo in arrivo
169300061215      *  e non è da stampare, non eseguo la stampa
169400101202     c*
169500070110     c   25              if        vidsn='S' and *in17  and
169600061215     c                             (*in21 or *in01)
169700070110     c                   setoff                                       012111
169800061215     c                   endif
169900061215      *
170000000621      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
170100000621      *
170200980402    1C     *IN01         IFEQ      *ON
170300980402     C     *IN02         OREQ      *ON
170400980402     C     *IN03         OREQ      *ON
170500980402     C     *IN21         OREQ      *ON
170600980402     C     *IN27         OREQ      *ON
170700110715     ***C                   EXSR      STAMPA
170800110715     c                   exsr      sr_WriteWF
170900950531    1C                   ENDIF
171000950531     C                   ENDSR
171100950601     C*
171200950605     C* CONTROLLO LE SPUNTE DEL FOGLIO DA ESAMINARE -----------------*
171300950601     C     LEGBRV        BEGSR
171400950601     C                   SETOFF                                       010203
171500950601     C                   SETOFF                                       040522
171600950605     C                   Z-ADD     1             Y
171700950605     C*
171800070109    0C     LP4(Y)        DOWGT     0
171900070123     C     KFVV3         SETLL     FNBRV000
172000070123     C     KFVV3         READE     FNBRV000                               33
172100950601     C*
172200950605    1C     *IN33         DOWEQ     *OFF
172300950601     C* ESCLUDO LE SPUNTE 88 E 90
172400950601   1AC     BRVNPS        IFNE      88
172500950601     C     BRVNPS        ANDNE     90
172600970506     C     BRVNPS        ANDNE     89
172700050919     c* elaboro solo le spunte con brvatr = blanks (esclusione spunte di
172800050919     c* colli non miei)
172900130225     c* elaboro però se si tratta di fls/nrs non di simfel ma che puiò
173000130225     c*     partire da altri TFP
173100130225     c***                clear                   altrotfp          1
173200130225    2C***  BRVNRS        ifgt      0
173300130225     C***                MOVE      BRVNRS        WLPSR
173400130225     C***                MOVEL     BRVLNP        WLPSR
173500130225     C*
173600130225     C***                Z-ADD     1             Z
173700130225     C***  WLPSR         LOOKUP    FLSNRS(Z)                              36
173800130225    3c***                if        *in36
173900130225     c***                eval      altrotfp='S'
174000130225    3c***                endif
174100130225    2c***                endif
174200130225     c
174300130225
174400130225   1Bc     Brvatr        ifeq      *blanks
174500950601     C*
174600950601     C     KBLT2         CHAIN     FNBLT27L                           34
174700950601     C  N34KBLP2         CHAIN     FNBLP001                           34
174800020909      *
174900020909      * Se trovo la bolla, controllo il P.O. di partenza nella £1:
175000130225      * se non è in £1 passo ad altra spunta
175100130225      *  tranne si se  tratta di bolla autorizzata a partire da + TFP
175200020909     C                   eval      $ok£1 = *off
175300020909    2C                   if        not *in34
175400070109     C     BLPlnp        lookup    LIN                                    10
175500020909     C   10              eval      $ok£1 = *on
175600020909    2C                   endif
175700020909     C*
175800950601     C* M A N C A   B O L L A
175900950601    2C     *IN34         IFEQ      *ON
176000950601     C*
176100950601     C* 18 OFF - NON E' RICHIESTA LA STAMPA ANOMALIE MANCA BOLLA
176200950703    3C     *IN18         IFEQ      *OFF
176300950601     C                   SETOFF                                       35
176400950601     C                   SETON                                        10
176500950601     C*
176600950601     C                   Z-ADD     1             X
176700950601     C* NEL CASO IN CUI SIANO STATE IMMESSE PIU' SELEZIONI AVENTI
176800950601     C* LA STESSA LINEA DI PARTENZA, RIESEGUO LA LOKUP
176900950605    4C     *IN10         DOWEQ     *ON
177000950601     C*
177100950601     C     BRVLNP        LOOKUP    SKI(X)                                 10
177200950703     C   10              EXSR      SERIE
177300950601     C                   ADD       1             X
177400950703    4C                   ENDDO
177500950703     C*
177600950703     C* SE NON TROVATO, LEGGO ANCHE LA SKIERA FAS
177700961025     C* SOLO SE LA SERIE E' > 0
177800961025     C* VERIFICO SE SI TRATTA DI LNP NON IN L1 MA CONFERMABILE SUL MIO
177900961025     C*  AS --> IN QUESTO CASO USO LNP CHE CONFERMA E NON LNP COLLO
178000950703    4C     *IN35         IFEQ      *OFF
178100950703     C     BRVNRS        ANDGT     0
178200020909     C                   MOVE      BRVNRS        WLPSR
178300960610     C                   MOVEL     BRVLNP        WLPSR
178400950703     C*
178500020909     C                   Z-ADD     1             Z
178600960610     C     WLPSR         LOOKUP    FAS(Z)                                 36
178700960610     C   36Z             OCCUR     DSLAS
178800950703     C*
178900020909     C                   Z-ADD     1             YY
179000960610     C     *IN36         DOWEQ     *ON
179100960619     C     LAS(YY)       ANDGT     *ZEROS
179200950703     C                   Z-ADD     1             X
179300960619     C                   SETON                                        10
179400960610    6C     *IN10         DOWEQ     *ON
179500950703     C*
179600020909     C                   MOVE      LAS(YY)       W0030
179700960619     C     W0030         LOOKUP    SKI(X)                                 10
179800950703     C   10              EXSR      SERIE
179900950703     C                   ADD       1             X
180000950703    6C                   ENDDO
180100950703     C*
180200950703     C   35              SETOFF                                       36
180300960610     C                   ADD       1             YY
180400960610     C                   ENDDO
180500950703    4C                   ENDIF
180600950703     C*
180700950703    3C                   ENDIF
180800950601     C*
180900950601     C* MANCA BOLLA
181000950703    3C     *IN18         IFEQ      *ON
181100961025     C* SE MANCA BOLLA DI LNP NON IN L1 CONTROLLO CHE NON ESISTA
181200950703     C*  UNA SPUNTA PARTENZA DI ALTRO AS: SE ESISTE NON DO LA
181300950703     C*  SEGNALAZIONE
181400961025     C     *IN35         OREQ      *ON
181500050920     c                   clear                   wmio              1
181600150623     c                   clear                   wbsptp            1
181700150623    2c                   if        *in18 = *on  and brvnrs = 0
181800050920     c     brvlnp        lookup    l1                                     31
181900150623    3c                   if        *in31 = *off
182000150623     c* verifico se lnp che può partire da me
182100150623     c     brvlnp        chain     fibsp01l
182200150623    4c                   if        %found(fibsp01l) and bspfpa='S'
182300150623     c                             and %lookup(brvpes:sbsp)>0
182400150623     c                   eval      wbsptp='S'
182500150623    4c                   endif
182600150623     c
182700150623    4c                   if        not %found(fibsp01l) or
182800150623     c                             wbsptp<>'S'
182900050920     c                   eval      wmio = 'N'
183000150623    4c                   endif
183100150623    3c                   endif
183200150623    2c                   endif
183300050919     c*
183400050920     c* Considerazioni per colli con serie:
183500050919     c* se a fronte della spunta è presente un disguido non stampo
183600050920     c* (collo non mio)
183700050920    4c                   if        brvnrs > 0
183800050920     c                   z-add     55            comcaa
183900050920     c     leganm        tag
184000050919     C     KANM          SETLL     FNANM02L                               31
184100050920    5c                   do        *hival
184200050919     C     KANM          reade     FNANM02L                               31
184300050920    6c                   if        %eof(fnanm02l)  or wmio = 'N'
184400050919     c                   leave
184500050920    6c                   endif
184600050920    6c                   if        anmfle = brvfgs
184700050920     c                   eval      wmio  = 'N'
184800050920    6c                   endif
184900050920    5c                   enddo
185000050920    5c                   if        comcaa = 55 and wmio = ' '
185100050920     c                   z-add     56            comcaa
185200050920     c                   goto      leganm
185300050920    5c                   endif
185400050920     C* Se non trovata anomalia di disguido cerco se esiste spunta partenza
185500050920     c* di altro p.o. in data <= della mia spunta. La spunta partenza NON
185600050920     c* deve essere di defluenza, transito, disguido e deve risultare trasme
185700050920     c* ssa. Se trovo tale spunta significa che il collo non è mio e quindi
185800050920     c* non stampo il manca bolla
185900050920    5c     wmio          ifeq      ' '
186000950703     C                   Z-ADD     BRVNPS        SAVNPS
186100950707     C                   Z-ADD     1             PARNPG
186200070123     C     KBRV2         setll     FNBRV009
186300050920    6C                   DO        *hival
186400070123     C     KBRV2         reade     FNBRV009
186500070123    7c                   if        %eof(fnbrv09l)
186600050920     c                   leave
186700050920    7c                   endif
186800070109    7c                   if        brvfle = ktfp  or brvatr <> *blanks
186900050920     c                   iter
187000050920    7c                   endif
187100050920     c* verifico se spunta di defluenza
187200050920     C                   CLEAR                   FNLV53DS
187300050920     C                   Z-ADD     BRVNFV        D53NFV
187400050920     C                   MOVEL     BRVNPG        D53NPG
187500050920     C                   MOVEL     BRVFGS        D53FGS
187600050920     C                   CALL      'FNLV53R'
187700050920     C                   PARM                    FNLV53DS
187800050920    7C     D53ERR        IFEQ      ' '
187900050920     c     d53def        andne     'S'
188000050920    8c                   if        d53dfv < wdatafv or (d53dfv=wdatafv
188100050920     c                             and brvftr <> *blanks)
188200050920     c                   eval      wmio = 'N'
188300050920    8C                   endif
188400050920    7c                   endif
188500050920    6c                   enddo
188600050920     C                   Z-ADD     SAVNPS        BRVNPS
188700050920    5c                   endif
188800050920    4c                   endif
188900950703     C*
189000050920     C* Stampa manca bolla
189100050920    4C     wmio          IFEQ      ' '
189200950703     C                   SETON                                        05
189300110715     ***C                   EXSR      STAMPA
189400110715     c                   exsr      sr_WriteWF
189500961025     C* SCRIVO FILE EDBRV  CODICE 001 --> MANCA COLLO
189600961025     C     EDICON        IFEQ      'S'
189700961025     C                   MOVEL     '002'         WSTS
189800961025     C                   EXSR      WRTEDI
189900961025     C                   ENDIF
190000950703    4C                   ENDIF
190100950703     C*
190200950703    3C                   ENDIF
190300950601     C***
190400950601   X2C                   ELSE
190500991022     C***
190600020909     C* se il P.O. di partenza (BLPLNP) non è in £1 :
190700020909     C*  NON STAMPO NULLA E NON FACCIO NESSUN CONTROLLO
190800101202   2BC     $ok£1         ifeq      *on
190900950605     C*
191000950605     C* 35 ON - MI INDICHERA' SE DEVO STAMPARE L'ANOMALIA
191100950605     C                   SETOFF                                       35
191200950601     C*
191300950601     C* SE QUADRATURA SPUNTA PARTENZA CONTROLLO SE BOLLA NON BORDERIZZ
191400950605    3C     *IN25         IFEQ      *ON
191500950601     C     BLPDBR        ANDEQ     0
191600950601     C                   SETON                                        22
191700950605     C   18              SETON                                        35
191800950605     C  N18              SETON                                        10
191900950605    3C                   ENDIF
192000950605     C*
192100950605     C* 25 OFF - CONTROLLO SE ECCEDENTE DI BOLLA NON SELEZIONATA
192200950605     C* 25 ON  - 22 ON - BOLLA NON BORDERIZZATA: VERIFICO LE RICHIESTE
192300950605     C*          PER DECIDERE SE STAMPARE O MENO.
192400950605     C*          SALTO IL CONTROLLO DELLA DATA
192500950601     C*
192600950605   2AC     *IN25         IFEQ      *OFF
192700950605     C     *IN22         OREQ      *ON
192800950605     C     *IN18         ANDEQ     *OFF
192900950605     C**
193000950601     C* CONTROLLO DATA SPEDIZIONE
193100950605     C**
193200980416    3C  N25BLPDSP        IFLT      COMDSP
193300980416     C     BLPDSP        ORGT      WDSA
193400961025     C* STAMPA COLLI ECCEDENTI DI SELEZIONI NON RICHIESTE --> SI 18ON
193500961025     C*  STAMPO SEMPRE LA ECCEDENZA PER DATA
193600950601     C   18              SETON                                        35
193700961025     C*
193800961025     C* STAMPA COLLI ECCEDENTI DI SELEZIONI NON RICHIESTE --> NO 18OFF
193900961025     C*  STAMPO L'ECCEDENZA PER DATA SOLO SE RIENTRA NELLE SELEZIONI
194000961025     C  N18              SETON                                        10
194100961025     C**
194200950601   X3C                   ELSE
194300950601     C  N18              SETOFF                                       10
194400950601    3C                   END
194500950601     C*
194600950601     C                   Z-ADD     1             X
194700950605     C   18              SETON                                        10
194800950601     C*
194900961025     C* IL LOOP 3 -->   N35 *IN10 DOWEQ*ON
195000961025     C*  LO ESEGUE SOLTANTO NEI SEGUENTI 2 CASI:
195100961025     C*  18 OFF - STAMPA LA ECCEDENZA DI DATA SPEDIZIONE <> SE LA BOLLA
195200961025     C*           RIENTRA NELLE SELEZIONI SOTTO EFFETTUATE
195300961025     C*  18 ON  - STAMPA LA ECCEDENZA SE LA DATA SPEDIZONE VA BENE
195400961025     C*           MA COMUNQUE NON E' STATA SELEZIONATA PER LE ALTRE RIC
195500961025     C*
195600950601     C* NEL CASO IN CUI SIANO STATE IMMESSE PIU' SELEZIONI AVENTI
195700950601     C* LA STESSA LINEA DI PARTENZA, RIESEGUO LA LOKUP
195800950605    3C  N35*IN10         DOWEQ     *ON
195900950601     C*
196000950601     C* CONTROLLO LINEA DI PARTENZA
196100950601     C     BLPLNP        LOOKUP    SKI(X)                                 10
196200950601     C*
196300950601     C* 18 ON - E' RICHIESTA LA STAMPA ANOMALIE DEI COLLI ECCEDENTI
196400950605    4C     *IN18         IFEQ      *ON
196500950601     C*
196600980115     C  N10              SETON                                        35
196700950605     C   10              EXSR      SELEZ
196800950601     C*
196900950601     C  N35              SETOFF                                       10
197000020909     C   35              Z-ADD     X             XX
197100950601     C   35
197200950601     CAN 10              SETOFF                                       35
197300950601     C*
197400950601   X4C                   ELSE
197500950601     C* 18 OFF - NON E' RICHIESTA LA STAMPA ANOMALIE DEI COLLI ECCEDENTI
197600950601     C*
197700950605     C* SE NON IMMESSA NESSUNA SELEZIONE --> STAMPA IL COLLO ECCEDENTE
197800950605    5C     *IN10         IFEQ      *ON
197900950605     C*
198000950601     C                   SETOFF                                       13
198100950601     C* SE IMMESSO CONTROLLO CODICE CLIENTE
198200970529    6C     X             IFLE      12
198300970529     C     CLI(X)        ANDNE     0
198400950601     C                   SETON                                        13
198500970529    7C     BLPCCM        IFGT      0
198600970529    8C     CLI(X)        IFEQ      BLPCCM
198700950601     C                   SETON                                        35
198800970529    8C                   END
198900950601     C*
199000970529   X7C                   ELSE
199100970529    8C     CLI(X)        IFEQ      BLPKSC
199200950601     C                   SETON                                        35
199300970529    8C                   END
199400970529    7C                   END
199500020909     C   35              Z-ADD     X             XX
199600950601    6C                   END
199700950601     C*
199800950601     C* SE SERIE >< 99 CONTROLLO
199900950601     C   13
200000950601     CAN 35
200100970529    6CORN13X             IFLE      12
200200060310     C     NRS(X)        ANDNE     '99'
200300950601     C                   SETON                                        13
200400060310     c                   if        nrs(x)='SI'
200500060310     c
200600060310     C                   IF        BLPNRS>0
200700060310     C                   SETON                                        35
200800060310     C                   ELSE
200900060310     C                   SETOFF                                       35
201000060310     C                   END
201100060310     c
201200060310     c                   else
201300060310     c                   movel     blpnrs        alfanrs           2
201400060310     C     NRS(X)        IFEQ      alfanrs
201500950601     C                   SETON                                        35
201600950601     C                   ELSE
201700950601     C                   SETOFF                                       35
201800950601     C                   END
201900950601    6C                   END
202000060310    6C                   END
202100950601     C*
202200950601     C* SE IMMESSO CONTROLLO CODICE PADRONCINO
202300950601     C   13
202400950601     CAN 35
202500970529    6CORN13X             IFLE      12
202600970529     C     CPC(X)        ANDNE     0
202700950601     C                   SETON                                        13
202800950601     C     CPC(X)        IFEQ      BLPPDR
202900950601     C                   SETON                                        35
203000950601     C                   ELSE
203100950601     C                   SETOFF                                       35
203200950601     C                   END
203300950601    6C                   END
203400950601     C*
203500950601     C* SE IMMESSA CONTROLLO DATA RITIRO
203600950601     C   13
203700950601     CAN 35
203800970529    6CORN13X             IFLE      12
203900970529     C     DRT(X)        ANDNE     0
204000950601     C                   SETON                                        13
204100950601     C     DRT(X)        IFEQ      BLPDRT
204200950601     C                   SETON                                        35
204300950601     C                   ELSE
204400950601     C                   SETOFF                                       35
204500950601     C                   END
204600950601    6C                   END
204700950601     C* SE IMMESSO CONTROLLO SE MATTINO O POMERIGGIO
204800950601     C   13
204900950601     CAN 35
205000970529    6CORN13X             IFLE      12
205100970529    6C     MOP(X)        ANDNE     *BLANKS
205200950601     C                   SETON                                        13
205300950601     C     MOP(X)        IFEQ      BLPFPP
205400950601     C                   SETON                                        35
205500950601     C                   ELSE
205600950601     C                   SETOFF                                       35
205700950601     C                   END
205800950601    6C                   END
205900950601     C* SE IMMESSO CONTROLLO NUMERO RITIRO
206000950601     C   13
206100950601     CAN 35
206200970529    6CORN13X             IFLE      12
206300970529    6C     NRT(X)        ANDNE     *BLANKS
206400950601     C                   SETON                                        13
206500950601     C                   MOVE      NRT(X)        COMNRT
206600950601     C     COMNRT        IFEQ      BLPNRT
206700950601     C                   SETON                                        35
206800950601     C                   ELSE
206900950601     C                   SETOFF                                       35
207000950601     C                   END
207100950601    6C                   END
207200950601     C*
207300950601     C  N13              SETON                                        35
207400950601    5C                   END
207500950601     C*
207600950601     C   35              SETOFF                                       10
207700950601    4C                   END
207800950601     C*
207900950601     C                   ADD       1             X
208000950605    3C                   ENDDO
208100950605   2AC                   ENDIF
208200950601     C*
208300950605    3C     *IN35         IFEQ      *ON
208400950605     C* 25 OFF -SEGNACOLLO ECCEDENTE DI BOLLA NON SELEZIONATA
208500950605     C*         ACCENDO     LO 04
208600950605     C* 25 ON  -SEGNACOLLO ECCEDENTE DI BOLLA NON BORDERIZZATA
208700950605     C*         GIA' ACCESO IL 22
208800961025     C     *IN25         IFEQ      *OFF
208900961025     C                   SETON                                        04
209000961028     C     EDICON        IFEQ      'S'
209100961025     C                   MOVEL     '003'         WSTS
209200961025     C                   EXSR      WRTEDI
209300961025     C                   ENDIF
209400961025     C                   ENDIF
209500950601     C*
209600950601     C* 03 ON  - ZONA SEGNACOLLO DIVERSA DA ZONA BOLLA
209700170407     C***  BLPZNC        IFNE      BRVZNC
209800170407     C***                SETON                                        03
209900170407     C***                ENDIF
210000950601     C*
210100110715     ***C                   EXSR      STAMPA
210200110715     c                   exsr      sr_WriteWF
210300950601    3C                   ENDIF
210400950601     C*
210500991022   2BC                   ENDIF
210600991022    2C                   ENDIF
210700130225   1BC                   ENDIF
210800130225   1AC                   ENDIF
210900950601     C*
211000950601     C                   SETOFF                                       040503
211100950601     C                   SETOFF                                       103522
211200070123     C     KFVV3         READE     FNBRV000                               33
211300950601    1C                   ENDDO
211400950605     C*
211500950605     C                   ADD       1             Y
211600950605    0C                   ENDDO
211700950601     C                   ENDSR
211800950703     C*
211900950703     C* PREPARA LA SKIERA DI LETTURA DELLE LNP SU FNBRV---------------*
212000950703     C     PRELP3        BEGSR
212100070109     C                   CLEAR                   LP4
212200950703     C**
212300070109     C* SE RICHIESTA tutta la stampa dei colli eccedenti
212400070109     C*  accodo tutta la FAS caricata (di fatto di simfel come prima)
212500950703    1C     *IN18         IFEQ      *ON
212600070109     c                   movea     l1            lp4
212700950703     C*
212800950703     C                   Z-ADD     1             X
212900070109     C     000           LOOKUP    LP4(X)                                 32
213000950703     C* ACCODO LA SKIERA FAS DELLE ALTRE FILIALI GESTITE
213100950703     C*  COME LNP SEGNACOLLO
213200950703    3C     *IN32         IFEQ      *ON
213300950703     C                   Z-ADD     1             Y
213400950703     C*
213500950703    4C     FAS(Y)        DOWGT     0
213600020909     C                   MOVEL     FAS(Y)        W0030
213700070109     C     W0030         LOOKUP    LP4                                    32
213800950703    5C     *IN32         IFEQ      *OFF
213900070109     C                   z-add     W0030         LP4(X)
214000950703     C                   ADD       1             X
214100950703    5C                   ENDIF
214200950703     C*
214300950703     C                   ADD       1             Y
214400950703    4C                   ENDDO
214500950703    3C                   ENDIF
214600950703     C*
214700950703   X1C                   ELSE
214800950703     C**
214900070109     C* SE richiesti solo colli eccedenti delle lnp immesse
215000070109     C* DELLA SKIERA FAS CARICO SOLO QUELLE RELATIVE ALLE LINEE
215100070109     C*  ELABORATE
215200070109     c                   move      lp3(ee)       lp4(1)
215300070109     C                   Z-ADD     2             Y
215400070109     c                   eval      zz=ee+1
215500070109     c* Se non è l'ulitmo record letto, cerco altre lnp selezionate
215600070109     c*  per lo stesso terminal di partenza
215700070109     c                   if        zz<=99
215800070109     c                   movel     lp3(zz)       elatfp            3 0
215900070109     c
216000070109     c                   dow       zz<=99 and elatfp=ktfp
216100070109     c                   move      lp3(zz)       lp4(Y)
216200070109     c                   add       1             y
216300070109     c
216400070109     c                   add       1             zz
216500070109     c
216600070109     c                   if        zz<=99
216700070109     c                   movel     lp3(zz)       elatfp
216800070109     c                   endif
216900070109     c                   enddo
217000070109     c
217100070109     c                   endif
217200070109     c
217300070109     c
217400950703     C* DELLA SKIERA FAS CARICO SOLO QUELLE RELATIVE ALLE LINEE
217500950703     C*  ELABORATE
217600950703     C                   Z-ADD     1             X
217700070109     C     000           LOOKUP    LP4(X)                                 32
217800950703     C* ACCODO LA SKIERA FAS DELLE ALTRE FILIALI GESTITE
217900950703     C*  COME LNP SEGNACOLLO
218000950703    3C     *IN32         IFEQ      *ON
218100950703     C                   Z-ADD     1             Y
218200950703     C*
218300960610    4C     FAS(Y)        DOWGT     *ZEROS
218400080109     C     Y             ANDLE     $MaxElem
218500960610     C     Y             OCCUR     DSLAS
218600020909     C                   Z-ADD     1             YY
218700960610    5C     YY            DOWLE     87
218800960610     C     LAS(YY)       ANDGT     *ZEROS
218900960619     C                   MOVE      LAS(YY)       W0030
219000070109     C     W0030         LOOKUP    LP4                                    32
219100960610    6C     *IN32         IFEQ      *ON
219200020909     C                   MOVEL     FAS(Y)        W0030
219300070109     C     W0030         LOOKUP    LP4                                    32
219400070109     C  N32              z-add     W0030         LP4(X)
219500960610     C  N32              ADD       1             X
219600960610    6C                   END
219700960610     C                   ADD       1             YY
219800960610    5C                   ENDDO
219900960610     C                   ADD       1             Y
220000960610    4C                   ENDDO
220100960610     C*
220200950703    3C                   ENDIF
220300950703     C*
220400950703    1C                   ENDIF
220500950703     C*
220600950703     C                   ENDSR
220700950703     C*
220800070109     C* PREPARA LA SKIERA DI LETTURA DELLE LNP + tfp -----------------*
220900070109     C     PRETFP        BEGSR
221000070109     C                   CLEAR                   LP3
221100070109     C**
221200070109     C* SE RICHIESTA LA STAMPA DEI COLLI ECCEDENTI--> LEGGO TUTTE LE
221300070109     C*  SPUNTE
221400070109     c* carico la £1 perchè nel cotrollo entrata non posso
221500070109     c*  richidere lnp non dell'area (18 on solo se lnp dell'area)
221600070109    1C     *IN18         IFEQ      *ON
221700101202     c*
221800070109     C                   Z-ADD     1             Y
221900070109    2C                   DOW       L1(Y)>0
222000070109     C                   MOVE      L1(Y)         LP3(Y)
222100070109     C                   MOVEL     SIMFEL        LP3(Y)
222200070109     c                   add       1             y
222300070109    2c                   enddo
222400070109     C*
222500070109   X1C                   ELSE
222600070109     C**
222700070109     C* SE NON RICHIESTA --> LEGGO SOLO LE SPUNTE DELLE LNP RICHIESTE
222800070109     C*
222900070109     C                   Z-ADD     1             Y
223000070109    2C                   DOW       Lp2(Y)>0
223100070109     c* Cerco terminal di partenza
223200070109     c                   clear                   fnlv55ds
223300070109     c                   eval      d55tpt='P'
223400070109     c                   movel     lp2(y)        d55lin
223500070109     c                   movel     comdsp        d55drf
223600070109     c                   call      'FNLV55R'
223700070109     c                   parm                    fnlv55ds
223800070109     c                   movel     d55tfp        lp3(Y)
223900070109     C                   MOVE      LP2(Y)        LP3(Y)
224000070109     c                   add       1             y
224100070109     c                   enddo
224200101202     c*
224300070109     c                   endif
224400101202     c*
224500070109     C                   ENDSR
224600950703     C* CONTROLLO LA SERIE SE E' DELLE SELEZIONI EFFETTUATE-----------*
224700950703     C     SERIE         BEGSR
224800950703     C*
224900950703     C* SE SERIE >< 99 CONTROLLO
225000060310    1C     X             IFLE      12
225100060310     C     NRS(X)        ANDNE     '99'
225200060310    2c                   if        nrs(x)='SI'
225300060310    3c                   if        brvnrs>0
225400060310     C                   SETON                                        35
225500060310    3C                   END
225600060310   x2c                   else
225700101202     c*
225800060310     c                   movel     brvnrs        alfanrs           2
225900060310    3C     NRS(X)        IFEQ      alfanrs
226000950703     C                   SETON                                        35
226100060310    3C                   END
226200060310    2C                   END
226300060310   X1C                   ELSE
226400950703     C                   SETON                                        35
226500060310    1C                   END
226600950703     C*
226700111117     c* Se la serie va bene --> se c'e' cliente controllo anche range di
226800111117     c*  segnacolli
226900111117     c                   if        *in35 and brvnrs>0 and cli(x)>0
227000111117     c                   clear                   trulnrsds
227100111117     c                   eval      inrsccm=cli(X)
227200111117     c                   eval      inrsnrs=brvnrs
227300111117     c                   eval      inrsfls=brvlnp
227400111117     c                   eval      inrsncd=brvnsc
227500111117     c                   call      'TRULNRSR'
227600111117     c                   parm                    trulnrsds
227700111117     c                   if        onrserr<>' '
227800111117     c                   setoff                                       35
227900111117     c                   endif
228000111117     c
228100111117     c                   endif
228200950703     C*
228300950703     C   35              SETOFF                                       10
228400950703     C                   ENDSR
228500961022     C*
228600961022     C* SCRIVO FILE PER EDI ------------------------------------------*
228700961022     C     WRTEDI        BEGSR
228800961025     C* WSTS = '   ' C'E' COLLO E BOLLA; SITUAZIONE POSITIVA
228900961025     C*      = '001' COLLO MANCANTE; NON ESISTE NESSUNA SPUNTA ENTRATA
229000961025     C*        '002' COLLO ECCEDENTE;NON HO I DATI DELLA BOLLA
229100961025     C*        '003' COLLO ECCEDENTE;HO I DATI DELLA BOLLA IN ALTRA
229200961025     C*                              DATA SPEDIZIONE
229300980324     C*        '004' COLLO MANCANTE; PRESENTE SU ALTRO FOGLIO STESSA DT
229400980324     C*        '005' COLLO MANCANTE; PRESENTE SU ALTRO FOGLIO ALTRA  DT
229500961025     C                   CLEAR                   EDBRV000
229600980324     C     WSTS          IFNE      '002'
229700961022     C                   MOVEL     BLPAAS        EDIAAS
229800961022     C                   MOVEL     BLPLNP        EDILNP
229900961022     C                   MOVEL     BLPNRS        EDINRS
230000961022     C                   MOVEL     BLPNSP        EDINSP
230100961022     C                   MOVEL     BLPLNA        EDILNA
230200961022     C                   MOVEL     BLPFLS        EDIFLS
230300961022     C                   MOVEL     BLTNSC        EDINSC
230400980326     C                   MOVEL     BLTCAN        EDICAN
230500961028     C                   Z-ADD     BLPZNC        EDIZNC
230600961022     C                   MOVEL     WSTS          EDISTS
230700961022     C                   ENDIF
230800961025     C     WSTS          IFEQ      '002'
230900961025     C     WSTS          OREQ      '003'
231000961025     C     WSTS          OREQ      *BLANKS
231100961022     C                   MOVEL     BRVNPS        EDINPS
231200000209     C                   Z-ADD     BRVDFS        EDIDCS
231300070123     C                   movel     BRVHMS        EDIHCS
231400961022     C                   ENDIF
231500961025     C     WSTS          IFEQ      '002'
231600980326     C                   MOVEL     BRVCAN        EDICAN
231700961025     C                   MOVEL     BRVLNA        EDILNA
231800961025     C                   MOVEL     BRVLNP        EDIFLS
231900961025     C                   MOVEL     BRVNSC        EDINSC
232000961025     C                   MOVEL     BRVNRS        EDINRS
232100961025     C                   MOVEL     BRVZNC        EDIZNC
232200961025     C                   MOVEL     WSTS          EDISTS
232300961025     C                   ENDIF
232400961025     C*
232500961025     C     WSTS          IFEQ      '003'
232600961025     C                   ENDIF
232700961022     C                   MOVEL     VIDNFV        EDINFE
232800961022     C                   MOVEL     WFEDFV        EDIDFE
232900980108     C     BLPCCM        IFEQ      0
233000980108     C                   MOVEL     BLPKSC        EDICCM
233100980108     C                   ELSE
233200980108     C                   MOVEL     BLPCCM        EDICCM
233300980108     C                   END
233400961022     C*
233500961022     C                   WRITE     EDBRV000
233600961022     C                   ENDSR
233700980108     C*
233800980108     C* CARICO DATI IN SCHIERA ---------------------------------------*
233900121102     C**   CARPU         BEGSR
234000980108     C* CARICO I DATI DELLA TABELLA PU DEI CODICI CLIENTI/PARTNER O
234100980108     C* DELLE LNP SELEZIONATE IN SCHIERA
234200121102     C**                 OPEN      EDTAB01L
234300980108     C* --------------------------------------------------------------*
234400980108     C* ESEGUO LOOP SU TABELLA PU DEI CODICI CHE VOGLIONO LE DATE
234500980108     C* ARRIVO
234600121102     C**                 Z-ADD     0             WW
234700121102     C**                 MOVEL     'PU'          EDCOD
234800121102     C**   EDCOD         SETLL     EDTAB01L
234900121102     C**   EDCOD         READE     EDTAB01L                               30
235000121102     C**   *IN30         DOWEQ     *OFF
235100121102     C**   TABFLG        IFEQ      ' '
235200121102     C**                 MOVEL     TABUNI        EDIDSPU
235300121102     C**   §PUDTA        IFEQ      'S'
235400121102     C**                 ADD       1             WW
235500121102     C**                 MOVEL     TABKEY        EDK(WW)
235600121102     C**                 MOVEL     TABUNI        EDU(WW)
235700121102     C**                 END
235800121102     C**                 END
235900121102     C**   EDCOD         READE     EDTAB01L                               30
236000121102     C*+                 END
236100980108     C*---------------------------------------------------------------*
236200121102     C**                 DO        WW            W
236300121102     C**                 MOVEL     'PT'          EDCOD
236400121102     C**                 MOVEL     EDK(W)        EDKEY
236500121102     C**   KEDTAB        CHAIN     EDTAB01L                           30
236600121102    3C*+   *IN30         IFEQ      *OFF
236700121102     C**   TABFLG        ORNE      ' '
236800121102     C**                 MOVEL     TABUNI        EDIDSPT
236900121102     C**                 Z-ADD     1             XW
237000121102     C**   §PTKSC        LOOKUP    CLI(XW)                                32
237100121102     C**   *IN32         IFEQ      '0'
237200121102     C*+                 Z-ADD     1             XW
237300121102     C**   §PTLNP        LOOKUP    LNP(XW)                                32
237400121102     C**   CLI(XW)       IFNE      *ZEROS
237500121102     C*+                 SETOFF                                       32
237600121102     C**                 END
237700121102     C*+                 END
237800121102     C**   *IN32         IFEQ      '1'
237900121102     C**                 MOVEL     EDU(W)        EDIDSPU
238000121102     C**                 MOVE      §PUDTB        DTB(XW)
238100121102     C**                 MOVE      §PUSGD        SGD(XW)
238200121102     C**                 MOVE      §PUECC        ECC(XW)
238300121102     C**                 ENDIF
238400121102     C**                 ENDIF
238500121102     C**
238600121102     C**                 ENDDO
238700121102     C**
238800121102     C**                 CLOSE     EDTAB01L
238900980108     C*
239000121102     C**                 ENDSR
239100121102     C**
239200950531     C* OPERAZIONI INIZIALI E DEFINIZIONE CAMPI E KLIST --------------*
239300950531     C     *INZSR        BEGSR
239400010717     C* ACCESSO FNBLP31L
239500950531     C     KBLP          KLIST
239600950531     C                   KFLD                    COMLNP
239700950531     C                   KFLD                    COMANS
239800950531     C                   KFLD                    COMMGS
239900950531     C* ACCESSO FNBLP01L
240000950531     C     KBLP2         KLIST
240100950531     C                   KFLD                    BLTAAS
240200950531     C                   KFLD                    BLTLNP
240300950531     C                   KFLD                    BLTNRS
240400950531     C                   KFLD                    BLTNSP
240500050930     C     Kart          KLIST
240600050930     C                   KFLD                    BLTAAS
240700050930     C                   KFLD                    BLTLNP
240800050930     C                   KFLD                    BLTNRS
240900050930     C                   KFLD                    BLTNSP
241000050930     C                   KFLD                    BLTnsc
241100950531     C* ACCESSO FNBLT01L
241200950531     C     KBLT          KLIST
241300950531     C                   KFLD                    BLPAAS
241400950531     C                   KFLD                    BLPLNP
241500950531     C                   KFLD                    BLPNRS
241600950531     C                   KFLD                    BLPNSP
241700950531     C* ACCESSO FNBLT27L
241800950531     C     KBLT2         KLIST
241900950531     C                   KFLD                    BRVLNP
242000950531     C                   KFLD                    BRVLNA
242100950531     C                   KFLD                    BRVNRS
242200950531     C                   KFLD                    BRVNSC
242300991022     C     KANM          KLIST
242400991022     C                   KFLD                    BRVLNP
242500991022     C                   KFLD                    BRVLNA
242600991022     C                   KFLD                    BRVNRS
242700991022     C                   KFLD                    BRVNSC
242800991022     C                   KFLD                    COMCAA
242900000320     C     KANM2         KLIST
243000000320     C                   KFLD                    BLTFLS
243100000320     C                   KFLD                    BLTLNA
243200000320     C                   KFLD                    BLTNRS
243300000320     C                   KFLD                    BLTNSC
243400000320     C                   KFLD                    COMCAA
243500070123     C* ACCESSO FNBRV09L
243600950531     C     KBRV          KLIST
243700950531     C                   KFLD                    WNPG
243800950531     C                   KFLD                    BLTFLS
243900950531     C                   KFLD                    BLTLNA
244000950531     C                   KFLD                    BLTNRS
244100950531     C                   KFLD                    BLTNSC
244200061215     C                   KFLD                    ktfp
244300130222     C     KBRV_altr     KLIST
244400130222     C                   KFLD                    WNPG
244500130222     C                   KFLD                    BLTFLS
244600130222     C                   KFLD                    BLTLNA
244700130222     C                   KFLD                    BLTNRS
244800130222     C                   KFLD                    BLTNSC
244900070123     C* ACCESSO FNBRV09L
245000950703     C     KBRV2         KLIST
245100950707     C                   KFLD                    PARNPG
245200950703     C                   KFLD                    BRVLNP
245300950703     C                   KFLD                    BRVLNA
245400950703     C                   KFLD                    BRVNRS
245500950703     C                   KFLD                    BRVNSC
245600000201     C* ACCESSO FNFVV01L
245700950531     C     KFVV          KLIST
245800950531     C                   KFLD                    WNPG
245900950531     C                   KFLD                    VIDNFV
246000950531     C                   KFLD                    VIDFIL
246100950601     C     KFVV3         KLIST
246200950601     C                   KFLD                    WNPG
246300950601     C                   KFLD                    WNFV
246400950601     C                   KFLD                    WFGS
246500070109     C                   KFLD                    LP4(Y)
246600020918     C     KFVV2         KLIST
246700950601     C                   KFLD                    WNPG
246800950601     C                   KFLD                    COMDSP
246900020918     C                   KFLD                    Kfgs
247000950601     C     KFGV          KLIST
247100061215     C                   KFLD                    ktfp
247200950601     C                   KFLD                    COMDSP
247300950531     C* ACCESSO CNACO00F
247400950531     C     KACO          KLIST
247500950531     C                   KFLD                    CODUT
247600101202     C                   KFLD                    dutKCI
247700950531     C                   KFLD                    CLI(X)
247800950531     C     KTAB          KLIST
247900950531     C                   KFLD                    CODUT
248000950531     C                   KFLD                    COD
248100950531     C                   KFLD                    KEY
248200950629     C     KTAB2         KLIST
248300950629     C                   KFLD                    CODUT
248400950629     C                   KFLD                    COD
248500130225     C     Ktbe          KLIST
248600130225     C                   KFLD                    kcod
248700130225     C                   KFLD                    kke1
248800121102     C**     KEDTAB        KLIST
248900121102     C**                   KFLD                    EDCOD
249000121102     C**                   KFLD                    EDKEY
249100021203     C*
249200021203     C     Kfapd         klist
249300021203     C                   kfld                    kpdtip
249400021203     C                   kfld                    kpdpdr
249500950531     C*
249600101202     C                   Z-ADD     1             CODUT             1 0
249700101202      *
249800101202      * Reperimento dati utente/aziendali
249900101202     c                   exsr      sr_DatiJob
250000980402     C*
250100180126     C****               MOVEL     'I.M.X'       CINVEN
250200950531     C*
250300950531     C                   ENDSR
250400110715      /free
250500110715
250600110715       //-------------------------------------------------------------
250700110715       //?Lettura WFQEM00F per stampa errori                          ?
250800110715       //-------------------------------------------------------------
250900110715       BEGSR  sr_StampaWF;
251000110715
251100110718         // -?Chiusura del file WFQEM00F?
251200110718         close WFQEM00F;
251300110718
251400110715         // -?Preparazione SQL di lettura WFQEM00F?
251500110715         wSQL = 'select * from WFQEM00F';
251600110715
251700110715         if  PrmORD = '2';
251800110718           wSQL += ' order by WQErsm, +
251900110718                              WQElnp, WQEaas, WQEmgs, WQEnrs, WQEnsc';
252000120208         //else;
252100120208         //  wSQL += ' order by WQElnp, WQEaas, WQEmgs, WQEnrs, WQEnsc';
252200110715         endif;
252300110715
252400110715         // -?Dichiarazione cursore?
252500110715         exec sql   prepare S1   from :wSQL;
252600110715         exec sql   declare C1   cursor for S1;
252700110715
252800110715         // -?Apertura del cursore?
252900110715         exec sql   open C1;
253000110715
253100110715         if  SQLcode < *zero;
253200110715           exsr  sr_PrintErrSQL;
253300110718           leavesr;
253400110715         endif;
253500110715
253600110715         // -?Ciclo di lettura wf?
253700110715         exec sql   fetch next   from C1   into :WFQEMds;
253800110715
253900110715         DoW  SQLcode <> 100;
254000110718
254100110718           if  SQLcode < *zero;
254200110718             exsr  sr_PrintErrSQL;
254300110718             leavesr;
254400110718           endif;
254500110715
254600110715           exsr  sr_Stampa;
254700110718
254800110718           exec sql   fetch next   from C1   into :WFQEMds;
254900110715
255000110718         EndDo;
255100110715
255200110715         // -?Chiusura del cursore?
255300110715         exec sql   close C1;
255400110715
255500110715       ENDSR;
255600110715
255700110715       //--------------------------------------------------------------
255800110715       //?Stampa segnalazione dell'errore rilevato via SQL             ?
255900110715       //--------------------------------------------------------------
256000110715       BEGSR  sr_PrintErrSQL;
256100110715
256200110715         // -?Stampa del Dump?
256300110715         Dump(A);
256400110715
256500110715         // -?Stampa del Job-Log?
256600110715         Qcmd = 'DSPJOBLOG job(*) output(*print)';
256700110715         exsr  sr_ExecCmd;
256800110715
256900110715       ENDSR;
257000110715
257100110715       //--------------------------------------------------------------
257200110715       //?Esecuzione del comando (già impostato).                      ?
257300110715       //--------------------------------------------------------------
257400110715       BEGSR  sr_ExecCmd;
257500110715
257600110715         clear Qcap0100;
257700110715         Qcabcsdh = *off;
257800110715         Qcapa    = *off;
257900110715         Qcacmdss = *off;
258000110715         Qcaerved = *allX'00';
258100110715
258200110715         clear Qusec;
258300110715         Qusbprv  = %size(Qusec);
258400110715
258500110715         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
258600110715                           %size(Qcap0100) : 'CPOP0100' : *omit :
258700110715                           0 : 0 : Qusec);
258800110715
258900110715         //if  Qusei <> *blank;
259000110715         //  ...;
259100110715         //endif;
259200110715
259300110715       ENDSR;
259400110719
259500110719      /end-free
259600110715
259700110715       //-------------------------------------------------------------
259800110715       //?Stampa singolo errore                                       ?
259900110715       //-------------------------------------------------------------
260000110715     c     sr_Stampa     BEGSR
260100110715      *
260200110718     c                   movea     WQEind        *in
260300110718      *
260400110715     C                   SETON                                        20
260500110715     C*
260600110715     C* STAMPA TESTATA
260700110719do  1C   OF              DO
260800110715     C                   SETON                                        07
260900110715     C                   EXCEPT    TES
261000110715     C                   SETOFF                                       OF
261100110719e   1C                   ENDDO
261200110720 *
261300110721     c                   clear                   S_StaRSD
261400110720     c                   clear                   S_ChiSOno
261500110715     C*
261600110715     C* 05 OFF - STAMPO DATI SPEDIZIONE
261700110719if  1c                   If        NOT *in05
261800110719      *
261900110718     c                   eval      BLPaas = WQEaas
262000110718     c                   eval      BLPlnp = WQElnp
262100110718     c                   eval      BLPnrs = WQEnrs
262200110718     c                   eval      BLPnsp = WQEnsp
262300110718     c                   eval      BLPmgs = WQEmgs
262400110718     c                   eval      BLPlna = WQElna
262500110718     c                   eval      BLPrsm = WQErsm
262600110721     c                   eval      BLTfls = WQEfls
262700110718     c                   eval      BLTnsc = WQEnsc
262800110718      *
262900110715     C                   Z-ADD     BLPAAS        COMAAS
263000110719if  2C     SAVSPE        IFNE      BLPSPE
263100110715     C                   SETON                                        07
263200110715     C                   MOVEL     BLPSPE        SAVSPE
263300110719e   2C                   ENDIF
263400110715     C* DATA SPEDIZIONE
263500110719do  2C   07              DO
263600110715     C                   Z-ADD     BLPDSP        DATA
263700110715     C                   Z-ADD     MM            STADSP
263800110715     C                   MOVEL     GG            STADSP
263900110715     C*
264000110715     C* SEGNACOLLI NON CONSECUTIVI
264100110715     C     BLPFNS        COMP      'S'                                    09
264200110715     C*
264300110715     C* SERIE >< 0
264400110715     C     BLPNRS        COMP      0                                  0808
264500110715     C*
264600110718     ***C* RIFERIM. ALLEGATI MITTENTE
264700110718     ***C     BLPRMA        COMP      *BLANKS                            1414
264800110715     C*
264900110715     C* ACCORCIO LA RAGIONE SOCIALE MITTENTE
265000110715     C                   MOVEL     BLPRSM        STARSM
265100110721      *
265200110721      * - Accorcio la ragione sociale Destinatario
265300110721     c                   movel     BLPrsd        S_StaRSD
265400110715     C*
265500110715     C* GIRO DATA RITIRO MERCE
265600110715     C                   Z-ADD     BLPDRT        DATA
265700110715     C                   Z-ADD     MM            STADRT
265800110715     C                   MOVEL     GG            STADRT
265900110715     c*
266000110715     c* se presente consegna anomala, la decodifico
266100110715     c                   clear                   descca
266200110715     c                   clear                   dcmdfv            4 0
266300110719if  3c                   if        blpcca<>' '
266400110715     C                   MOVEL     '7O'          COD
266500110715     C                   MOVEL(P)  blpcca        KEY
266600110715     C     KTAB          CHAIN     TABEL                              30
266700110715     C  N30              MOVEL     TBLUNI        ds7o
266800110715     C   30              clear                   ds7o
266900110715     c                   movel     §7odei        descca           13
267000110715     c* giro la data consegnamerce se c'e'
267100110719if  4c                   if        blpdcm>0
267200110715     C                   Z-ADD     blpdcm        DATA
267300110715     C                   Z-ADD     MM            GGMM
267400110715     C                   MOVEL     GG            GGMM
267500110715     C                   MOVEL     GGMM          dcmDFV
267600110719e   4c                   endif
267700110719e   3c                   endif
267800110715     C*
267900110715     C* DECODIFICO CODICE PADRONCINO
268000110715     C*    BLPPDR        CHAIN     FNAPD01L                           15
268100110715     C                   move      BlpPdr        kpdpdr
268200110715     C                   move      'A'           kpdtip
268300110715     C     kfapd         chain     fiapd01l                           15
268400110715     C  N15              MOVEL     APDRSC        DESCPR
268500110715     C   15              CLEAR                   DESCPR
268600110719e   2C                   ENDDO
268700110720      /free
268800110720           // -?Reperimento "CHI SONO"?
268900110721           if  WQEnrs = *zero;
269000110720             k_ARSfls = WQEfls;
269100110720             k_ARSlna = WQElna;
269200110720             k_ARSnrs = WQEnrs;
269300110720             k_ARSnsc = WQEnsc;
269400110720             k_ARStrc = 'C';
269500110720             chain  %kds(k05fiars01)  FIARS000;
269600110720             if  %found(FIARS01L);
269700110720               S_ChiSono = ARSnot;
269800110720             endif;
269900110720           endif;
270000110720      /end-free
270100110718      *
270200110719x   1c                   Else
270300110718      *
270400110718     c                   eval      BRVlnp = WQElnp
270500110718     c                   eval      BRVnrs = WQEnrs
270600110718     c                   eval      BRVlna = WQElna
270700110718     c                   eval      BRVnsc = WQEnsc
270800110718      *
270900110719e   1c                   EndIf
271000110719      *
271100110719     C* SE COLLO MANCANTE O SPUNTA 88 DICO SE E' STATO SPARATO IN ENTR
271200110719     C   01
271300110719if  1COR 21BLTDSE        IFGT      0
271400110719     C                   SETON                                        11
271500110719     C                   Z-ADD     BLTDSE        DATA
271600110719     C                   Z-ADD     MM            ENTDFV
271700110719     C                   MOVEL     GG            ENTDFV
271800110719     C                   Z-ADD     BLTNPE        ENTNPS
271900110719e   1C                   ENDIF
272000110719     C*
272100110719if  1C     ANMDAO        IFGT      0
272200110719     C                   MOVEL     ANMDAO        DATA
272300110719     C                   Z-ADD     MM            DAMDFV
272400110719     C                   MOVEL     GG            DAMDFV
272500110719if  2C     DAMNPS        IFNE      *BLANKS
272600110719     C                   SETON                                        29
272700110719e   2C                   ENDIF
272800110719     C                   SETON                                        12
272900110719e   1C                   ENDIF
273000110715     C*
273100110715     C* SE 02 ON - SU ALTRO FOGLIO, GIRO LA DATA DEL FOGLIO
273200110720if  1C     *IN02         IFEQ      *ON
273300110715     C                   Z-ADD     PRTDFV        DATA
273400110715     C                   Z-ADD     MM            GGMM
273500110715     C                   MOVEL     GG            GGMM
273600110715     C                   MOVEL     GGMM          PR4DFV
273700110715     C*
273800110720if  2C     PRTNPG        IFNE      WSNPG
273900110715     C     PRTSPG        ORNE      WSSPG
274000110720if  3C     PRTSPG        IFNE      ' '
274100110715     C                   CLEAR                   PRTDPG
274200180126     C                   MOVEL     '7B'          COD
274300180126     C                   MOVEL(P)  PRTsPG        KEY
274400180126     C     KTAB          CHAIN     TABEL                              30
274500180126     C  N30              MOVEL     TBLUNI        ds7b
274600180126     C   30              CLEAR                   ds7b
274700180126     C                   MOVEl(p)  §7BDESB6      prtdpg
274800110720x   3C                   ELSE
274900110715     C                   MOVEL     '7N'          COD
275000110715     C                   MOVEL(P)  PRTNPG        KEY
275100110715     C     KTAB          CHAIN     TABEL                              30
275200110715     C  N30              MOVEL     TBLUNI        PRTDPG
275300110715     C   30              CLEAR                   PRTDPG
275400110720e   3C                   ENDIF
275500110715     C*
275600110715     C                   MOVEL     PRTNPG        WSNPG
275700110715     C                   MOVEL     PRTSPG        WSSPG
275800110720e   2C                   ENDIF
275900110715     C* DECODIFICO LA FIL GESTIONE
276000110720if  2C     PRTFGS        IFNE      WSFGS
276100110715     C     PRTFGS        CHAIN     AZORG01L                           30
276200110720if  3C  N30ORGDE5        IFNE      *BLANKS
276300110715     C                   MOVEL     ORGDE5        PRDFGS
276400110720x   3C                   ELSE
276500110715     C                   MOVEL     ORGDES        PRDFGS
276600110720e   3C                   ENDIF
276700110715     C   30              CLEAR                   PRDFGS
276800110715     C                   MOVEL     PRTFGS        WSFGS
276900110720e   2C                   ENDIF
277000110720e   1C                   ENDIF
277100120127     c
277200120127     c* Per situazione particolare in cui ho spunta fasulla in altra data
277300120127     c*  stampo la data e non stampo la riga di spunta in altra data
277400120127     c                   if        *in40
277500120127     c                   setoff                                       0102
277600120127     c                   endif
277700110715     c*
277800110715     c* Per consegna anomala, vedo che indicatore accendere
277900110715     c                   setoff                                       9606
278000110720if  1c                   if        blpcca<>' '
278100110720if  2c                   if        *in01 and not *in11 and not *in12
278200110715     c                   seton                                        06
278300110720x   2c                   else
278400110715     c                   seton                                        96
278500110720e   2c                   endif
278600110720e   1c                   endif
278700110715     C*
278800110715     C* STAMPO DETTAGLIO
278900120208     c* sempre per quadratura con spu partenza- 25 on
279000120208     c* sempre se richeiste tutte le anomalie - 25 off  e vid9000=N
279100120208     c* solo i manca record                   - 25 off e vid9000=S
279200120208    1c                   if        *in25   or
279300120208     c                             vid9000='N' or *in05
279400120208     c
279500110720     C                   EXCEPT    DET
279600110720      /free
279700110720         // -?Collo eccedente di bolla NON borderizzata?
279800110720         if  *in22;
279900110720           except  Det_22;
280000110720         endif;
280100110720         // -?Collo eccedente di bolla NON selezionata?
280200110720         if  *in04;
280300110721           if  (S_ChiSono <> *blank  or  S_StaRSD <> *blank)  and
280400110721               *in22;
280500110720             except  DetChiSono;
280600110720           endif;
280700110720           except  Det_04;
280800110720         endif;
280900110720         // -?Collo mancante?
281000110720         if  *in05;
281100120208          // trattandosi di manca record bolla non si può essere chi sono e RSD
281200120208          //  if  (S_ChiSono <> *blank  or  S_StaRSD <> *blank)  and
281300120208          //      (*in22  or  *in04);
281400120208          //    except  DetChiSono;
281500120208          //  endif;
281600120208
281700110720           except  Det_05;
281800110720         endif;
281900110720         // -?Spunta mancante?
282000110720         if  *in01;
282100110721           if  (S_ChiSono <> *blank  or  S_StaRSD <> *blank)  and
282200110720               (*in22  or  *in04  or  *in05);
282300110720             except  DetChiSono;
282400110720           endif;
282500110720           except  Det_01;
282600110720         endif;
282700110720         // -?Spunta mancante: presente spunta 88?
282800110720         if  *in21;
282900110721           if  (S_ChiSono <> *blank  or  S_StaRSD <> *blank)  and
283000110720               (*in22  or  *in04  or  *in05  or  *in01);
283100110720             except  DetChiSono;
283200110720           endif;
283300110720           except  Det_21;
283400110720         endif;
283500110720         // -?Spunta su altro foglio?
283600110720         if  *in02;
283700110721           if  (S_ChiSono <> *blank  or  S_StaRSD <> *blank)  and
283800110720               (*in22  or  *in04  or  *in05  or  *in01  or  *in21);
283900110720             except  DetChiSono;
284000110720           endif;
284100110720           except  Det_02;
284200110720         endif;
284300110720         // -?Dettaglio colli aggiornato con pistola fasulla?
284400110720         if  *in27;
284500110721           if  (S_ChiSono <> *blank  or  S_StaRSD <> *blank)  and
284600110720               (*in22  or  *in04  or  *in05  or  *in01  or  *in21  or
284700110720                *in02);
284800110720             except  DetChiSono;
284900110720           endif;
285000110720           except  Det_27;
285100110720         endif;
285200110720         // -?Bolla con consegna anomala?
285300110720         if  *in96;
285400110721           if  (S_ChiSono <> *blank  or  S_StaRSD <> *blank)  and
285500110720               (*in22  or  *in04  or  *in05  or  *in01  or  *in21  or
285600110720                *in02  or  *in27);
285700110720             except  DetChiSono;
285800110720           endif;
285900110720           except  Det_96;
286000110720         endif;
286100110720         // -?Zona segnacollo diversa da zona bolla?
286200110720         if  *in03;
286300110721           if  (S_ChiSono <> *blank  or  S_StaRSD <> *blank)  and
286400110720               (*in22  or  *in04  or  *in05  or  *in01  or  *in21  or
286500110720                *in02  or  *in27  or  *in96);
286600110720             except  DetChiSono;
286700110720           endif;
286800110720           except  Det_03;
286900110720         endif;
287000110721         // -?...solo Chi-Sono & Rag.Soc.Destinatario?
287100110721         if  S_ChiSono <> *blank  or  S_StaRSD <> *blank;
287200110720           if  Not *in01  and  Not *in02  and  Not *in03  and
287300110720               Not *in04  and  Not *in05  and
287400110720               Not *in21  and  Not *in22  and  Not *in27  and
287500110720               Not *in96;
287600110729             except  Det_SkipBe;
287700110720           endif;
287800110720           except  DetChiSono;
287900110729           except  Det_SkipAf;
288000110720         endif;
288100110720      /end-free
288200120208     c                   endif
288300110715     C                   SETOFF                                       07
288400110715      *
288500110715     c                   ENDSR
288600101202
288700101202      *---------------------------------------------------------------*
288800101202      *?Reperimento dati del job (Utente/Operativi)                  ?*
288900101202      *---------------------------------------------------------------*
289000101202     c     sr_DatiJob    BEGSR
289100101202      *
289200101202     c     *dtaara       define    §azute        AZUTEds
289300101202     c     *dtaara       define    §datiute      dDatiUte
289400101202      *
289500101202     c                   in(E)     *dtaara
289600101202     c                   IF        %Error or RSUT = *blanks
289700101202     c                   clear                   TIBS34ds
289800101202     c                   call      'TIBS34R'
289900101202     c                   parm                    TIBS34ds
290000101202     c                   in        *dtaara
290100101202     c                   ENDIF
290200101202      *
290300101202     c                   ENDSR
290400950113     C*---------------------------------------------------------------*
290500020909
290600940329     OPRTF198   E            RICHI1           02
290700940329     O                       STA(1)             132
290800900626     O                       RSUT                20
290900110726     O                                          148 'FNLSA7R'
291000950605     O                       UDATE8             170 '  /  /    '
291100940329     O                                          193 'PAG.'
291200940329     O                       PAGE          Z    198
291300950605     O          E            RICHI1           03
291400950605     O               25      STA(16)            132
291500961028     O              N25N26   STA(17)            132
291600961028     O              N25 26   STA(18)            132
291700950605     O                       KNSIF               10
291800950605     O                       KNMUS               22
291900950605     O               25      VIDDSP             124 '  /  /    '
292000950605     O                       UTIME              170 '  :  :  '
292100930318     O          E            RICHI1      3
292200930318     O                                              'P A R A M E T R I'
292300930319     O                                         +  4 'R I C H I E S T I'
292400950530     O          E   N25      RICHI1      2
292500930319     O                                              'FOGLIO ENTRATA DI'
292600930318     O                       VIDFIL            +  2
292700930319     O                                         +  1 '-'
292800930319     O                       DESFIL            +  1
292900930319     O                                         +  5 'N.RO'
293000930318     O                       VIDNFV            +  2
293100930319     O                       DESNFV            +  3
293200930318     O          E            RICHI1      2
293300980416     O              N25                          24 'DATA SPEDIZIONE ........'
293400070109     O              N25                        +  0 '..........: DAL'
293500950601     O               25                          24 'DATA ELABORAZIONE ......'
293600070109     O               25                        +  0 '..............:'
293700950113     O                       VIDDSP            +  2 '  /  /    '
293800980416     O              N25                        +  2 'AL'
293900980416     O              N25      VIDDSA            +  2 '  /  /    '
294000930318     O          E            RICHI1      2
294100930318     O                                              'ANOMALIE COLLI ECCEDENTI'
294200070109     O                                         +  0 ' .............:'
294300930319     O                       COMSTA            +  2
294400080626     O          E    25      RICHI1      2
294500080626     O                                              'ANOMALIE BOLLE LOCALI SE'
294600080626     O                                         +  0 'RIE  00  .....:'
294700080626     O                       COM9000           +  2
294800171023     O          E    25      RICHI1      2
294900171023     O                                              'ESCLUDI SPUNTE 98 ......'
295000171023     O                                         +  0 '..............:'
295100171023     O                       COMMANCA          +  2
295200070108     O          E    25      RICHI1      2
295300070109     O                                              'ESCLUDI SPUNTE 88/90 DI '
295400070109     O                                         +  0 'COLLI ARRIVATI:'
295500070108     O                       COMSN             +  2
295600110715     o          e            RICHI1      2
295700110715     o                                              'ORDINAMENTO STAMPA .....-
295800110715     o                                              ..............:'
295900110715     o                       PrmORD            +  2
296000110721     o                                         +  3 '(1 = Linea di Partenza /-
296100110721     o                                               Serie / Segnacollo;'
296200110719     o          e            RICHI1      2
296300110715     o                                              '                        -
296400110715     o                                                             '
296500110721     o                                         +  6 ' 2 = Ragione Sociale Mit-
296600110719     o                                              tente / Linea di Partenza-
296700110721     o                                               / Serie / Segnacollo)'
296800950530     O          E   N25      RICHI1      2
296900130226     O                                              'Escludi i"MANCANTI" di A'
297000130226     O                                         +  0 'LTRO FOGLIO ..:'
297100130226     O                       COMmanca          +  2
297200130226     O          E   N25      RICHI1      2
297300130226     O                                              'ELABORAZIONE DEFINITIVA '
297400130226     O                                         +  0 '..............:'
297500130226     O                       COMSN             +  2
297600930318     O          E            RICHI1      3
297700930319     O                                         +  6 'LINEA PARTENZA'
297800961017     O                                         + 12 'COD. AUTOTRASPORTATORE'
297900961017     O                                         + 10 'SERIE'
298000930521     O                                         +  7 'C O D.    C L I E N T E'
298100930521     O                                         + 15 'DATA RITIRO'
298200950323     O                                         +  3 'N.RITIRO'
298300950324     O                                         +  3 'MAT/POM'
298400930318     O          E            RICHI2      2
298500930319     O                       LNP(X)        Z   +  5
298600930319     O                                         +  1 '-'
298700930319     O                       DESLNP            +  1
298800930319     O                       CPC(X)        Z   +  6
298900930319     O                     23                  +  1 '-'
299000930319     O                       DESCPC            +  1
299100930524     O                       VIDNRS            +  9
299200930521     O                       CLI(X)        Z   +  7
299300930319     O                     24                  +  1 '-'
299400930319     O                       DESCLI            +  1
299500950113     O                       VIDDRT            +  9 '  /  /    '
299600950324     O                       NRT(X)            +  8
299700950324     O                       MOP(X)            +  9
299800930317     O*
299900930317     O          E            TES              02
300000940329     O                       STA(1)             132
300100930317     O                       RSUT                20
300200110726     O                                          148 'FNLSA7R'
300300950605     O                       UDATE8             170 '  /  /    '
300400940329     O                                          193 'PAG.'
300500940329     O                       PAGE          Z    198
300600950605     O          E            TES              03
300700950605     O               25      STA(16)            132
300800950605     O              N25      STA(17)            132
300900950605     O               25      VIDDSP             124 '  /  /    '
301000950605     O                       KNSIF               10
301100950605     O                       KNMUS               22
301200950605     O                       UTIME              170 '  :  :  '
301300950530     O          E   N25      TES         2
301400940329     O                       STA(2)              66
301500930319     O                       VIDFIL              39
301600930319     O                                           40 '-'
301700930319     O                       DESFIL              60
301800930319     O                                           69 'N.RO'
301900930319     O                       VIDNFV        Z     76
302000930319     O                                           84 'DEL'
302100950113     O                       STADFV              96 '  /  /    '
302200911018     O          E            TES         2
302300940329     O                       STA(3)              66
302400940329     O                       STA(6)             132
302500950324     O                       STA(12)            198
302600980402     O          E            TES         1  1
302700940329     O                       STA(4)              66
302800940329     O                       STA(7)             132
302900950324     O                       STA(13)            198
303000911015     O*
303100980402     O          E    05      DET            1
303200920423     O                                          132 ' '
303300980402     O          E            DET         0  0
303400110715     O              N05 07   BLPAAS        z
303500110715     O              N05 07   BLPLNP        z  +   1
303600110715     O              N05 07 08BLPNRS        z  +   1
303700110715     O              N05 07 08                 +   0 '-'
303800110715     O              N05 07   BLPNSP        z  +   0
303900110715     O              N05 07   STADSP           +   1 '  /  '
304000110715     O              N05 07   BLPLNA        z  +   1
304100110715     O              N05 07   STARSM           +   2
304200110715     o              N05 07   BLPRMN        z  +   2
304300110715     o              N05 07   BLPRMA           +   1
304400110715     o              N05 07   BLPNCL        z  +   1
304500110715     O              N05 07N15BLPPDR        z  +   1
304600110715     O              N05 07N15                 +   0 '-'
304700110715     O              N05 07N15DESCPR           +   0
304800110715     O              N05 07   STADRT           +   2 '  /  '
304900110715     O              N05      BLTNSC        z  +   2
305000110715     O               05      BRVLNP        z      8
305100110715     O               05      BRVNRS        z     11
305200110715     O               05      BRVLNA        z     29
305300110718     O               05      BRVNSC        z    135
305400110715     ***O              N05 07N09BLPNCD        Z     75
305500110715     ***O              N05 07N09                    76 '/'
305600110715     ***O              N05 07N09BLPNCA        Z     83
305700110715     ***O              N05 07 09                    83 'NON SEQUENZIALI'
305800110715     ***O              N05 07   BLPZNC              88
305900110715     ***O               05      BRVZNC              88
306000110715     ***O              N05 07   BLPNRT             125
306100110715     ***O              N05 07   BLPFPP             128
306200980402     O* COLLO ECCEDENTE DI BOLLA NON BORDERIZZATA
306300110720     O          E    22      DET_22      0  1
306400980402     O                                          158 'ECCEDENTE DI BOLLA'
306500980402     O                                          175 'NON BORDERIZZATA'
306600980402     O* COLLO ECCEDENTE DI BOLLA NON SELEZIONATA
306700110720     O          E    04      DET_04      0  1
306800980402     O                                          158 'ECCEDENTE DI BOLLA'
306900980402     O                                          174 'NON SELEZIONATA'
307000110720     O          E    05      DET_05      0  1
307100980402     O* COLLO MANCANTE
307200980402     O                                          151 'MANCA BOLLA'
307300980402     O                                          174 'LETTORE BARCODE N.RO:'
307400980402     O                       BRVNPS             177
307500110715     o                                        +   1 'ZONA'
307600110715     o                       BRVZNC           +   1
307700950531     O* SPUNTA MANCANTE
307800110720     O          E    01      DET_01      0  1
307900980402     O                                          148 'MANCANTE'
308000110718     ***O                  14                      160 ':RIF.ALFAB.'
308100110715     ***O                  14   BLPRMA             175
308200070419     O                  11                      187 'ENTRATO il'
308300980402     O                  11   ENTDFV             193 '  /  '
308400980402     O                  11                      196 'P.'
308500980402     O                  11   ENTNPS             198
308600000621     O                  12   DESANP             187
308700000314     O                  12   DAMDFV             193 '  /  '
308800000320     O               29 12                      196 'P.'
308900000320     O               29 12   DAMNPS             198
309000070419     O                  06   descca             188
309100070419     O                  06                      191 'il'
309200070419     O                  06   DCMDFV             198 '  /  '
309300000314     O* SPUNTA MANCANTE
309400110720     O          E    01 28   DET_01      0  1
309500070419     O                                          187 'ARRIVATO il'
309600000314     O                       DAMDFV             193 '  /  '
309700000320     O               29                         196 'P.'
309800000320     O               29      DAMNPS             198
309900980402     O* SPUNTA MANCANTE: PRESENTE SPUNTA 88
310000110720     O          E    21      DET_21      0  1
310100980402     O                                          155 'MANCANTE:SPUNTA'
310200980402     O                       W88NPS             158
310300000201     O                                          161 'FV'
310400120127     O                       W88NFV        Z    167
310500120127     o                  40                      170 'dt'
310600120127     O                  40   pr4DFV             176 '  /  '
310700070419     O                  11                      187 'ENTRATO il'
310800980402     O                  11   ENTDFV             193 '  /  '
310900980402     O                  11                      196 'P.'
311000950606     O                  11 21ENTNPS             198
311100000621     O                  12   DESANP             187
311200000314     O                  12   DAMDFV             193 '  /  '
311300000320     O               29 12                      196 'P.'
311400000320     O               29 12   DAMNPS             198
311500000314     O* SPUNTA MANCANTE: PRESENTE SPUNTA 88
311600110720     O          E    21 28   DET_21      0  1
311700070419     O                                          187 'ARRIVATO il'
311800000314     O                       DAMDFV             193 '  /  '
311900000320     O               29                         196 'P.'
312000000320     O               29      DAMNPS             198
312100950531     O* SPUNTA SU ALTRO FOGLIO
312200110720     O          E    02      DET_02      0  1
312300980402     O                    N25                   161 'MANCANTE, SPUNTATO DA'
312400980402     O                     25                   161 '          SPUNTATO DA'
312500980402     O                       PRTFGS             166
312600070419     O                                          170 'in'
312700980402     O                       PRTDPG             180
312800980402     O                       PRTNFV        Z    187
312900070419     O                                          191 'il'
313000980402     O                       PR4DFV             198 '  /  '
313100980402     O* DETTAGLIO COLLI AGGIORNATNO CON PISTOLA FASULLA
313200110720     O          E    27      DET_27      0  1
313300980402     O                                          164 'BOLLA AGGIORNATA CON SPU'
313400980402     O                                          167 'NTA'
313500980402     O                       BLTNPP             171
313600980402     O                                          198 'MA PRESENTE SPUNTA REALE'
313700070419     O* bolla con consegna anomala
313800110720     O          E    96      DET_96      0  1
313900070419     O                       descca             188
314000070419     O                                          191 'il'
314100070419     O                       DCMDFV             198 '  /  '
314200980402     O* ZONA SEGNACOLLO DIVERSA DA ZONA BOLLA
314300110720     O          E    03      DET_03      0  1
314400950601     O                                          155 'ZONA SEGNACOLLO'
314500950601     O                       BRVZNC             158
314600980403     O                                          181 'DIVERSA DA ZONA BOLLA'
314700110721      *
314800110729     o          e            Det_SkipBe  1
314900110720     o                                          132 ' '
315000110721      *
315100110720     o          e            DETchiSono  0  0
315200110729     o                       S_StaRSD         +  45
315300110729     o                       S_ChiSono        +  29
315400110721      *
315500110729     o          e            Det_SkipAf  1
315600110720     o                                          132 ' '
315700110721      *
315800940426     O          E            OK          3
315900940426     O                                              ' '
316000940426     O          E            OK          3
316100940329     O                       STA(9)             132
316200940426     O          E   N20      NOELAB      3
316300940426     O                                              ' '
316400940426     O          E            NOELAB      3
316500940421     O                       STA(11)            132
316600930319     O          E            PIEDI            64
316700940329     O                       STA(10)            132
316800110715** STA ..1....+....2....+....3....+....4....+....5....+....6....+.
316900110715      ****       QUADRATURA   ENTRATA   A   MAGAZZINO      ****      1
317000940329F O G L I O   E N T R A T A :   DI  XXX-XXXXXXXXXXXXXXXXXXXX         2
317100110715*------- SPEDIZIONE --------*  M I T T E N T E            *--- RIF   3
317200110929ANNO LNP SER-NUMERO  DATA LNA                Destinatario        N   4
317300110715XXXX XXX XX-XXXXXXX XX/XX XXX  *...+....1....+....2....+  12345678   5
317400110715ERIMENTO  MITTENTE ---*  NUM. AUTOTRASPORTATORE        DATA    SEG   6
317500110721UMERICO ALFANUMERICO    COLLI    chi sono              RITIRO  ERR   7
3176001107159012345 *...+....1....+ 12345 XXXXXXX-XXXXXXXXXXXXXXX  XX/XX  1234   8
317700940329        TUTTO  O.K.  :  NESSUNA  ANOMALIA  RISCONTRATA  !!           9
317800940329        ***          F I N E      S T A M P A          ***          10
317900940426         N E S S U N A    B O L L A     E L A B O R A T A  !!       11
318000110715NAC                                                                 12
318100110715ATO                                                                 13
318200110715567                                                                 14
318300950530                           DEL  XX/XX/XXXX                          15
318400950606      **** CON SPUNTE PARTENZA E SPUNTE IMP DEL XX/XX/XXXX ****     16
318500950605      ****                CON SPUNTE ENTRATA               ****     17
318600961028      ****                 E .  D .  I .                   ****     18
