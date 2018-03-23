000100151125>>>>>H DFTACTGRP(*NO) BNDDIR('PJXBND')
000300940211     H DECEDIT('0,') DATEDIT(*DMY.)
000400940224      *----------*
000500941027      * ANA007R  *
000600940224      *----------*
000700980528      *               ANAGRAFICA SOCIETÀ
000800940224      *          ------------------------------
000900940224      *
001000940224      *
001100940224      * DOCUMENTI MICROANALISI :
001200940224      *
001300940224      * NOTE TECNICHE :
001400940224      *
001500940224      *
001600940224      * PERSONALIZZAZIONI
001700940224      *
001800940224      *
001900940224      *
002000940314      *               MAPPA INDICATORI
002100940224      *
002200940307      *  21           GENERICO OPERAZIONI I/O
002300940224      *  22           GENERICO ERRORE OPERAZIONI I/O
002400940224      *  30           SFLDSP
002500940224      * N31           SFLCLR
002600940224      *  31           SFLDSPCTL
002700940224      *  32           SFLNXTCHG
002800940224      *  33           SFLEND
002900940224      *  39           OF PRTF
003000940224      *  40 <---> 49  DSPATR ERRORI SU SFL
003100941027      *  Specificare l'uso dei singoli indicatori
003200940224      *  50 <---> 98  ERRORI SU VIDEO
003300941027      *  Specificare l'uso dei singoli indicatori
003400940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
003500940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
003600940224      *  99           INDIC. GENERALE DI ERRORE
003700940128     F*----------------------------------------------------*
003800941027     F*----------------------------------------------------------------
003900941027     F* Viste logiche utilizzate per i diversi ordinamenti
004000941027     F*----------------------------------------------------------------
004100940909     F*------------
004200941011     FANSIF01L  IF   E           K DISK
004300941027     F                                     RENAME(ANSIF000:ANSIF01)
004400941212     FANSIF02L  IF   E           K DISK
004500941212     F                                     RENAME(ANSIF000:ANSIF02)
004600940211     F*------------
004700940201     FANMPE02L  UF A E           K DISK
004800940211     F*------------
004900941011    >FANA007D   CF   E             WORKSTN
005000941011     F                                     RENAME(A007D1:FMTD1)
005100941011     F                                     RENAME(A007D2:FMTD2)
005200941011     F                                     RENAME(A007D3:FMTD3)
005300941011     F                                     RENAME(A007S1:FMTS1)
005400941011     F                                     RENAME(A007C1:FMTC1)
005500941011     F                                     RENAME(A007T1:FMTT1)
005600941011     F                                     RENAME(A007Z1:FMTZ1)
005700941011     F                                     RENAME(A007Z2:FMTZ2)
005800941011     F                                     RENAME(A007Z3:FMTZ3)
005900940131     F                                     SFILE(FMTS1:S1NRR)
006000940201     F                                     INFDS(DSFMT)
006100940128     D*----------------------------------------------------*
006200980528     D* dimensione della schiera $MS1
006300980528     D $DIMOP          C                   CONST(30)
006400980528     D*
006500940302     D* Schiere per la gestione delle opzioni
006600980528    >D $MS1            S             27    DIM($DIMOP) CTDATA PERRCD(1)
006700980528    >D $TX1            S             50    DIM($DIMOP) ALT($MS1)
006800980528    >D $OP1            S              2  0 DIM($DIMOP) CTDATA PERRCD(1)
006900980528    >D $FL1            S              1    DIM($DIMOP) ALT($OP1)
007000980608     D* Schiera contenente i nomi dei programmi richiamati attraverso
007100980608     D* le opzioni personalizzate
007200980608     D $PGO1           S             10    DIM($dimop)
007300950403     D $TX             S             50    DIM(50)
007400950403     D $FL             S              1    DIM(50)
007500940909     D* Schiere generali per la gestione dei tasti di funzione
007600940506     D $FM             S             27    DIM(24)
007700940506     D $FX             S             50    DIM(24)
007800940506     D $FC             S              1    DIM(24)
007900940506     D $FR             S              1    DIM(24)
008000940909     D* Schiere per la gestione dei tasti di funzione
008100940506     D* Video D1
008200940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
008300940506     D $FX1            S             50    DIM(24) ALT($FM1)
008400940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
008500940506     D $FR1            S              1    DIM(24) ALT($FC1)
008600980608     D* Schiera contente i nomi dei programmi da richiamare con i tasti
008700980608     D* funzione personalizzata S1
008800980608     D $PGF2           S             10    DIM(25)
008900940506     D* Schiere per la gestione dei tasti di funzione
009000940506     D* Video S1
009100940506     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
009200940506     D $FX2            S             50    DIM(24) ALT($FM2)
009300940506     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
009400940506     D $FR2            S              1    DIM(24) ALT($FC2)
009500940916     D* Schiere per la gestione dei tasti di funzione
009600940916     D* Video D2
009700940916     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
009800940916     D $FX3            S             50    DIM(24) ALT($FM3)
009900940916     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
010000940916     D $FR3            S              1    DIM(24) ALT($FC3)
010100980529     D*
010200980529     D $D1L01          S              3  0
010300980529     D $D1L02          S              3  0
010400980529     D $S1L01          S              3  0
010500980529     D $S1L02          S              3  0
010600980529     D $C1Op1          S              3  0
010700980529     D $C1Op2          S              3  0
010800980529     D $D2L01          S              3  0
010900980529     D $D2L02          S              3  0
011000980608     D* Indice della schiera tasti funzione
011100980608     D PosFun          S              2  0
011200980608     D*-------------
011300940207     D* Valori assunti
011400980601     D XMPEDS        E DS                  INZ
011500980601     D XMPEDS1       E DS                  INZ
011600980601     D DSMPE           DS                  INZ
011700940207     D  $LOP                   1      1  0
011800940831     D  $ORD                   2      3  0
011900980529     D* Indice della schiera per le opzioni personalizzate
012000980528     D PosOpz          S              2  0
012100980528     D* numero max opzioni
012200980528     D NumOpz          S              2  0 Inz($DIMOP)
012300980528     D* numero effettive opzioni
012400980601     D OpzLen          S              2  0
012500980529     D*-------------
012600980529     D*-------------
012700940211     D* Passaggio Parametri
012800940211     D KPJBA         E DS
012900940225     D*-------------
013000940225     D* Reperimento Autorizzazioni
013100980601     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
013200940325     D*-------------
013300940325     D* Parametri in ricezione
013400980601    >D ANA007DS      E DS                  INZ
013500940325     D*-------------
013600980528     D* Parametri del PGM di manutenzione ANA008R
013700980601    >D ana008ds      E DS                  EXTNAME(ANA008DS) INZ
013800941011    >D  RET008       E                     INZ('0')
013900941011    >D  OPR008       E                     INZ('0')
014000941011    >D  ERR008       E                     INZ('0')
014100940211     D*-------------
014200980528     D* campo di salvataggio per la DS d'ingresso durante la chiamata
014300980528     D* alla funzione personalizzata
014400980601    >D $SaveDS         S                   like(ANA007DS)
014500980528     D*-------------
014600940211     D DSFMT           DS           512
014700940506     D  $TASTO               369    369
014800940211     D  NRG                  370    370
014900940211     D  NCL                  371    371
015000940211     D  SFLNRR               378    379B 0
015100941212     D                 DS           512
015200941212     D  GIORNO                 1      2  0
015300941212     D  MESE                   3      4  0
015400941212     D  ANNO                   5      6  0
015500941212     D  DGMA06                 1      6  0
015600940211     D*-------------
015700940211     D* posizione cursore
015800940211     D CURSOR          DS
015900940211     D  RIRI                   1      2B 0 INZ
016000940211     D  R$$                    2      2
016100940211     D  COCO                   3      4B 0 INZ
016200940211     D  C$$                    4      4
016300940127     D*-------------
016400940127     D* Reperimento nome PGM
016500940127     D STATUS         SDS           333
016600940127     D  DSPGM            *PROC
016700980528     D  PARMS            *PARMS
016800940207     D*-------------
016900940616     D* Costanti
017000940616     D* nr.di righe del sfl (SFLPAG)
017100940314     D SFLPAG          C                   CONST(10)
017101080317     D*-------------
017102080317G1553D* Chiamata pgm ricodifica
017103080317G1553D Ana770DS      E DS                  Inz
017200940616     D*-------------
017300940506     D* Tasti di funzione
017400940506     D F01             C                   CONST(X'31')
017500940506     D F02             C                   CONST(X'32')
017600940506     D F03             C                   CONST(X'33')
017700940506     D F04             C                   CONST(X'34')
017800940506     D F05             C                   CONST(X'35')
017900940506     D F06             C                   CONST(X'36')
018000940506     D F07             C                   CONST(X'37')
018100940506     D F08             C                   CONST(X'38')
018200940506     D F09             C                   CONST(X'39')
018300940506     D F10             C                   CONST(X'3A')
018400940506     D F11             C                   CONST(X'3B')
018500940506     D F12             C                   CONST(X'3C')
018600940506     D F13             C                   CONST(X'B1')
018700940506     D F14             C                   CONST(X'B2')
018800940506     D F15             C                   CONST(X'B3')
018900940506     D F16             C                   CONST(X'B4')
019000940506     D F17             C                   CONST(X'B5')
019100940506     D F18             C                   CONST(X'B6')
019200940506     D F19             C                   CONST(X'B7')
019300940506     D F20             C                   CONST(X'B8')
019400940506     D F21             C                   CONST(X'B9')
019500940506     D F22             C                   CONST(X'BA')
019600940506     D F23             C                   CONST(X'BB')
019700940506     D F24             C                   CONST(X'BC')
019800940506     D ENTER           C                   CONST(X'F1')
019900940506     D ROLDWN          C                   CONST(X'F4')
020000940506     D ROLLUP          C                   CONST(X'F5')
020100941103     D*-------------
020200941103     D* Dati di ambiente ottenuti da XSOC
020300941103     D SOC001        E DS                  EXTNAME(XSOC001DS)
020400960102     D  XSCSLC       E                     EXTFLD(XSCSALDOC)
020500960102     D  XSCSLA       E                     EXTFLD(XSCSALDOA)
020600941103      * DS Interna per dati di output di XSOC
020700941103     D XSOCDS          DS          1000
020800980608      * Variabili per gestione videate
020900980608      *
021000980608     D $Gest           S              2    inz
021100980608     D $Fine           S              1    inz                                  fine pgm
021200980608     D $InzD1          S              1    inz                                  inizializzione D1
021300980608     D $InzS1          S              1    inz                                  inizializzione S1
021400980608     D $InzD2          S              1    inz                                  inizializzione D2
021500980608     D $LastG          S              2    inz                                  fmt di provenienza
021600980608     D $LastV          S              2    inz                                  ultima emissione
021700980608     D $EFile          S              1    inz                                  memo fine file
021800980608     D $Esci           S              1    inz                                  rich.uscita ciclo
021900980608     D $RcdOk          S              1    inz                                  rcd letto valido
022000980608     D $Pied1          S              3  0 inz                                  piede da emett. D1
022100980608     D $Pies1          S              3  0 inz                                  piede da emett. S1
022200980608      * Variabili appoggio sempre presenti in tutte le anagrafiche
022300980608     D S1Nrr           S                   like(C1RCD)
022400980608     D WSfl            S                   like(C1Nrr)
022500980608     D WMax            S                   like(C1Rcd)
022600980608     D WPag            S                   like(C1Rcd)
022700980608     D WInzS1          S                   like($InzS1)                                     ne S1
022800980608    >D WOpr            S                   like(Opr007)
022900980608     D WInKR           S                   like(*InKR)                                      ne S1
023000980608     D WWGrp           S              3    inz
023100980608     D CurR            S              2  0 inz
023200980608     D CurC            S              2  0 inz
023300980608     D Win             S             99    inz
023400980608     D DAMG10          S             10    inz
023500980608     D W1Pos           S                   like(C1Pos)
023600980608      *
023700980608      * Variabili appoggio specifiche della singola anagrafica
023800980608      *
023900980608      * campi per gestione F4=Ricerca
024000980608    >D WWSoc           S                   like(SifSoc)
024100980608      * campi per parzializzazioni su rcd (solo alfanumerici)
024200980608    >D Soc999          S                   like(SifSoc)
024300980608    >D Cpa999          S                   like(SifCpa)
024400980608      * Indici
024500980608      *
024600980608     D X               S              3S 0 inz
024700980608     D Y               S              3S 0 inz
024800980608      *
024900000120A0740D ZDateISO        S               D
025000000120      *
025100940128     I*----------------------------------------------------*
025200941011     I*
025300941011     I* RIDENOMINAZIONE CAMPI
025400941115     IANSIF01
025500941205     I              SIFSOCIETA                  SIFSOC
025600941205     I              SIFCDSIA                    SIFSIA
025700941205     I              SIFCDSIAGR                  SIFSI2
025800941205     I              SIFUBICAZ                   SIFMCH
025900941205     I              SIFSYSINF                   SIFNSI
026000941205     I              SIFLINGUA                   SIFLIN
026100941205     I              SIFFLGOPE                   SIFFLG
026200941205     I              SIFDESBREV                  SIFDSI
026300941205     I              SIFDIVISA                   SIFDIV
026400941205     I              SIFINPUT                    SIFINP
026500941205     I              SIFLOCALIT                  SIFCIT
026600941205     I              SIFCDFISC                   SIFCDF
026700941205     I              SIFPARTIVA                  SIFIVA
026800941205     I              SIFPROV                     SIFPRV
026900941205     I              SIFSTATO                    SIFSTA
027000941205     I              SIFTELEFON                  SIFTEL
027100941205     I              SIFTELEX                    SIFTLX
027200941205     I              SIFTELEFAX                  SIFFAX
027300941205     I              SIFDTISCC                   SIFDTI
027400941205     I              SIFNATGIUR                  SIFNCR
027500941205     I              SIFDTAPBIL                  SIFDAB
027600941205     I              SIFDTSCBIL                  SIFDSA
027700941205     I              SIFISCCIAA                  SIFCCT
027800941205     I              SIFATCCIAA                  SIFATT
027900941205     I              SIFCAPOFCT                  SIFCPC
028000941205     I              SIFCAPOFAN                  SIFCPA
028100941205     I              SIFSOCFITT                  SIFSCF
028200941205     I              SIFCAPOFSN                  SIFCPF
028300941205     I              SIFDTFIVL                   SIFDFV
028400941205     I              SIFCDCTL20                  SIFCDC
028500960102     I              SIFSALDOC                   SIFSLC
028600960102     I              SIFSALDOA                   SIFSLA
028700941212     IANSIF02
028800941212     I              SIFSOCIETA                  SIFSOC
028900941212     I              SIFCDSIA                    SIFSIA
029000941212     I              SIFCDSIAGR                  SIFSI2
029100941212     I              SIFUBICAZ                   SIFMCH
029200941212     I              SIFSYSINF                   SIFNSI
029300941212     I              SIFLINGUA                   SIFLIN
029400941212     I              SIFFLGOPE                   SIFFLG
029500941212     I              SIFDESBREV                  SIFDSI
029600941212     I              SIFDIVISA                   SIFDIV
029700941212     I              SIFINPUT                    SIFINP
029800941212     I              SIFLOCALIT                  SIFCIT
029900941212     I              SIFCDFISC                   SIFCDF
030000941212     I              SIFPARTIVA                  SIFIVA
030100941212     I              SIFPROV                     SIFPRV
030200941212     I              SIFSTATO                    SIFSTA
030300941212     I              SIFTELEFON                  SIFTEL
030400941212     I              SIFTELEX                    SIFTLX
030500941212     I              SIFTELEFAX                  SIFFAX
030600941212     I              SIFDTISCC                   SIFDTI
030700941212     I              SIFNATGIUR                  SIFNCR
030800941212     I              SIFDTAPBIL                  SIFDAB
030900941212     I              SIFDTSCBIL                  SIFDSA
031000941212     I              SIFISCCIAA                  SIFCCT
031100941212     I              SIFATCCIAA                  SIFATT
031200941212     I              SIFCAPOFCT                  SIFCPC
031300941212     I              SIFCAPOFAN                  SIFCPA
031400941212     I              SIFSOCFITT                  SIFSCF
031500941212     I              SIFCAPOFSN                  SIFCPF
031600941212     I              SIFDTFIVL                   SIFDFV
031700941212     I              SIFCDCTL20                  SIFCDC
031800960102     I              SIFSALDOC                   SIFSLC
031900960102     I              SIFSALDOA                   SIFSLA
032000941103      *----------------------------------------------------*
032100941103      *                MAIN LINE PROGRAM
032200941103      *----------------------------------------------------*
032300941103      *
032400941103      * inizializzazione variabili
032500940223     C                   EXSR      INZVAR
032600941103      *
032700940223     C     $FINE         DOWEQ     *OFF
032800940131     C     $GEST         CASEQ     'D1'          GESD1                          Parzializz.
032900940131     C     $GEST         CASEQ     'S1'          GESS1                          Lista
033000940131     C     $GEST         CASEQ     'D2'          GESD2                          Valori ass.
033100941027     C                   ENDCS
033200941027     C                   ENDDO
033300941103      * fine programma
033400940325     C                   EXSR      ENDPGM
033500941103      *
033600940325     C/EJECT
033700941103      ************************************************************
033800941103      * FINE PROGRAMMA
033900941103      ************************************************************
034000940325     C     ENDPGM        BEGSR
034100941103      *
034200941103      * aggiorno valori assunti
034300940722     C                   EXSR      AGGMPE
034400941011     C   KC              MOVE      '1'           RET007
034500941011     C   KL              MOVE      '2'           RET007
034600941103      *
034700941103      * passaggio dati al pgm chiamante
034800980601    >C                   MOVEL     ANA007DS      KPJBU
034900941103      *
035000941103      * Se pgm Guida non è stato richiamato posso chiudere in LR
035100941011    >C     OPZ007        IFEQ      *BLANK
035200940325     C                   SETON                                            LR
035300941103      * altrimenti chiudo in RT
035400940325     C                   ELSE
035500940325     C                   SETON                                            RT
035600940325     C                   ENDIF
035700940616     C                   RETURN
035800941103      *
035900940325     C                   ENDSR
036000940224     C/EJECT
036100941103      ************************************************************
036200941103      * RICERCHE
036300941103      ************************************************************
036400940127     C     SEARCH        BEGSR
036500941103      *
036600941103      * determino Riga/Colonna del cursore
036700940127     C                   MOVE      NRG           R$$
036800940127     C                   MOVE      NCL           C$$
036900940127     C                   Z-ADD     RIRI          CURR
037000940127     C                   Z-ADD     COCO          CURC
037100941103      *
0372009401271    C                   SELECT
037300941103      *
037400941103    > *--------------------------------------------------------------
037500941103    > * inserire le routine di ricerca
037600941103    > *--------------------------------------------------------------
037700941103      * FMTD1  - MODULO
037800941212      * Società
037900980529    >C     H1NMFL        WHENEQ    '<>-+ùà'
038000941103      *
0381009402231O   C                   OTHER
038200941103      * imposto pos. del cursore
038300940929     C                   SETON                                        9899
038400980601     C*
0385009401271-   C                   ENDSL
038600980601     C*
038700980601     C                   Z-ADD     CURR          H1RIGA
038800980601     C                   Z-ADD     CURC          H1COLO
038900980601     C*
039000940506     C                   MOVE      *HIVAL        $LASTV
039100940127     C                   ENDSR
039200940131     C/EJECT
039300941103      ************************************************************
039400941103      * RIEMPIMENTO OPZIONI
039500941103      ************************************************************
039600940131     C     RIEOPZ        BEGSR
039700941103      *
039800941103      * Input :
039900941103      * - $TX = schiera con tutti i testi disponibili
040000941103      * - $FL = schiere con i flag di validità associati ai testi
040100941103      * - WCOZ = variabile contenente l'indice dell'ultimo testo
040200941103      *          opzione usato
040300941103      * - WMXO = variabile contenente il numero massimo di testi
040400941103      *          opzione usabili
040500941103      * - $L01 = Lunghezza prima riga
040600941103      * - $L02 = Lunghezza seconda riga
040700941103      * Output :
040800941103      * - WOPZ1 = variabile contenente la prima riga opzioni
040900941103      * - WOPZ2 = variabile contenente la seconda riga opzioni
041000950403     C* - FF23  = flag che indica se c'è bisogno del tasto F23
041100941103      *
041200961209     C                   CALLB     'X04OPMSG'
041300941103      * input
041400940223     C                   PARM                    $TX
041500940223     C                   PARM                    $FL
041600940225     C                   PARM                    WCOZ              3 0
041700940223     C                   PARM                    WMXO              3 0
041800941103      * output
041900940223     C                   PARM                    WOPZ1            76
042000940223     C                   PARM                    WOPZ2            76
042100950403     C                   PARM                    FF23              1
042200941103    > * input
042300950403    >C                   PARM                    $L01              3 0
042400950403    >C                   PARM                    $L02              3 0
042500941103      *
042600060224A1148C                   PARM                    $ALLOPT         960
042700060224A1148C*
042800940131     C                   ENDSR
042900940506     C/EJECT
043000941103      ************************************************************
043100941103      * RIEMPIMENTO TASTI FUNZIONE
043200941103      ************************************************************
043300940506     C     RIEKEY        BEGSR
043400941103      *
043500941103      * Input :
043600941103      * - $FX = schiera con tutti i testi disponibili
043700941103      * - $FC = schiere con i flag di validità associati ai testi
043800941103      * - $FR = schiere con i flag di REVERSE IMAGE
043900941103      * - $ULK = ultimo testo caricato
044000941103      * - $L01 = Lunghezza prima riga
044100941103      * - $L02 = Lunghezza seconda riga
044200941103      * Output :
044300941103      * - $ULK
044400941103      * - $KEY1= 1° riga di tasti funzione
044500941103      * - $KEY2= 2° riga di tasti funzione
044600941103      *
044700961209     C                   CALLB     'XKEYMSG'
044800941103      * input
044900940506     C                   PARM                    $FX
045000940506     C                   PARM                    $FC
045100940506     C                   PARM                    $FR
045200940506     C                   PARM                    $ULK              3 0
045300941103      * output
045400940506     C                   PARM                    $KEY1            79
045500940506     C                   PARM                    $KEY2            79
045600941103    > * input
045700980601     C                   PARM                    $L01              3 0
045800980601     C                   PARM                    $L02              3 0
045900941103      *
046000060224A1148C                   PARM                    $ALLFUNCT       480
046100060224A1148C*
046200940506     C                   ENDSR
046300940224     C/EJECT
046400941103      ************************************************************
046500941103      * GESTIONE EMISSIONE TASTI FUNZIONE
046600941103      ************************************************************
046700940224     C     GESPIE        BEGSR
046800941103      *
046900940224     C                   SELECT
047000941103      * Parzializzazioni
047100940224     C     $GEST         WHENEQ    'D1'
047200940506     C     $ULKD1        IFEQ      0
047300941103      * riempimento opzioni
047400940506     C                   EXSR      F24D1
047500940506     C                   ELSE
047600940506     C                   WRITE     FMTZ1
047700940506     C                   ENDIF
047800941103      * Lista
047900940506     C     $GEST         WHENEQ    'S1'
048000940506     C     $ULKS1        IFEQ      0
048100940506     C                   EXSR      F24S1
048200940506     C                   ELSE
048300940506     C                   WRITE     FMTZ2
048400940506     C                   ENDIF
048500941103      * Modifica valori assunti
048600940916     C     $GEST         WHENEQ    'D2'
048700940916     C     $ULKD2        IFEQ      0
048800940916     C                   EXSR      F24D2
048900940916     C                   ELSE
049000940916     C                   WRITE     FMTZ3
049100940916     C                   ENDIF
049200940506     C                   ENDSL
049300941103      *
049400940224     C                   ENDSR
049500940224     C/EJECT
049600941103      ************************************************************
049700941103      * GESTIONE PARZIALIZZAZIONI
049800941103      ************************************************************
049900940224     C     GESD1         BEGSR
050000941103      * inizializzazione videata
050100940223     C     $INZD1        IFEQ      *ON
050200940127     C                   EXSR      INZD1
050300940223     C                   MOVE      *OFF          $INZD1
050400940117     C                   END
050500941103      * emissione piede videata se proveniente da altra
050600941103      * salvataggio valore formato di provenienza
050700940202     C     $LASTV        IFNE      'D1'
050800941103      * salvo stato F17 per sapere cosa fare in caso di F12
050900961209     C                   MOVE      *INKR         WINKR             1
051000940202     C                   WRITE     FMTT1
051100940224     C                   EXSR      GESPIE
051200940202     C                   MOVE      $LASTV        $LASTG
051300940202     C                   MOVE      'D1'          $LASTV
051400940707     C                   ENDIF
051500941103      *
051600941103      * se esistono errori sulla videata
051700941103      * emetto la write del formato a indicatori spenti per vedere
051800941103      * le eventuali decodifiche
051900940707     C     *IN99         IFEQ      *ON
052000940707     C                   MOVEA     *IN           WIN
052100940707     C                   MOVE      *ALL'0'       IN5098           49
052200940707     C                   MOVEA     IN5098        *IN(50)
052300940707     C                   WRITE     FMTD1
052400940707     C                   MOVEA     WIN           *IN
052500940707     C                   ENDIF
052600941103      *
052700980601     C                   EXFMT     FMTD1
052800980601     C* azzero la posizione del cursore
052900980601     C                   Z-ADD     0             H1RIGA
053000980601     C                   Z-ADD     0             H1COLO
053100941103      * controllo abilitazione tasti funzione
053200941103      *
053300940506     C                   EXSR      CTRKD1
053400941103      *
053500940506     C     *IN99         IFEQ      *OFF
053600941103      *
053700940117     C                   SELECT
053800941103      * F2=Decodifica
053900940916     C     $TASTO        WHENEQ    F02
054000940916     C                   EXSR      F02D1
054100941103      * F3=Fine
054200940506     C     $TASTO        WHENEQ    F03
054300940307     C                   EXSR      F03D1
054400941103      * F4=Ricerca
054500940506     C     $TASTO        WHENEQ    F04
054600940127     C                   EXSR      SEARCH
054700941103      * F5=Refresh
054800940916     C     $TASTO        WHENEQ    F05
054900940916     C                   EXSR      F05D1
055000961213      * F10=Immissione
055100961213     C     $TASTO        WHENEQ    F10
055200961213     C                   EXSR      F10D1
055300941103      * F12=Ritorno
055400940506     C     $TASTO        WHENEQ    F12
055500940307     C                   EXSR      F12D1
055600941103      * F18=Modifica valori assunti
055700940506     C     $TASTO        WHENEQ    F18
055800940307     C                   EXSR      F18D1
055900941103      * F24=Altri tasti
056000940506     C     $TASTO        WHENEQ    F24
056100940309     C                   EXSR      F24D1
056200941103      *
056300940117     C                   OTHER
056400940127     C                   EXSR      CTRD1
056500940127     C     *IN99         IFEQ      *OFF
056600941103      * definisco l'ordinamento
056700941212     C                   SELECT
056800941103    > * << nel caso di più viste logiche e quindi più campi di
056900941103    > *    parzializzazione, qui vanno definiti i criteri per
057000941103    > *    determinare quale VL va letta, a seconda di quali campi
057100941103    > *    sono stati valorizzati >>
057200941212    >C     D1SOC         WHENNE    *BLANK
057300940131     C                   Z-ADD     1             $ORD
057400941212    >C     D1CPA         WHENNE    *BLANK
057500941212     C                   Z-ADD     2             $ORD
057600941103      * nel caso non sia stato digitato nulla uso l'ordinamento
057700941103      * dei valori assunti
057800941212     C                   ENDSL
057900940127     C                   MOVE      'S1'          $GEST
058000940223     C                   MOVE      *ON           $INZS1
058100940302     C                   MOVE      *BLANK        W1POS
058200940211     C                   ENDIF
058300941103      *
058400940117     C                   ENDSL
058500941103      *
058600940506     C                   ENDIF
058700940117     C                   ENDSR
058800940224     C/EJECT
058900941103      ************************************************************
059000941103      * INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
059100941103      ************************************************************
059200940127     C     INZD1         BEGSR
059300941103      *
059400940318     C                   CLEAR                   FMTD1
059401151125>>>>>C                   EVAL      d1DtFiVl = %DATE()
059500941103      * Se pgm non richiamato
059600941011     C     OPZ007        IFEQ      *BLANK
059700941103    > **********           Z-ADDUDATE     D1DRG
059800940831     C                   ELSE
059900941103      * Se pgm richiamato
060000940831     C                   SELECT
060100941011    >C     SOC007        WHENNE    *BLANK
060200941011    >C                   MOVEL     SOC007        D1SOC
060300941212    >C     CPA007        WHENNE    *BLANK
060400941212    >C                   MOVEL     CPA007        D1CPA
060500940831    >C                   OTHER
060600941103    > *************        Z-ADDUDATE     D1DRG
060700940831     C                   ENDSL
060800940831     C                   END
060900941103      *
061000940117     C                   ENDSR
061100940506     C/EJECT
061200941103      ************************************************************
061300941103      * CONTROLLO ABILITAZIONE TASTI VIDEATA D1
061400941103      * INPUT
061500941103      * $FC1 = Abilitazioni videata D!
061600941103      * OUTPUT
061700941103      * *IN97 *IN99 Errori
061800941103      ************************************************************
061900940506     C     CTRKD1        BEGSR
062000940506     C                   SETOFF                                       99
062100940506     C                   MOVEA     $FC1          $FC
062200940506     C                   EXSR      CTRKEY
062300940506     C     *IN99         IFEQ      *ON
062400940506     C                   SETON                                        97
062500940506     C                   ENDIF
062600940506     C                   ENDSR
062700940506     C/EJECT
062800941103      ************************************************************
062900941103      * CONTROLLO ABILITAZIONE TASTI
063000941103      * INPUT
063100941103      * $TASTO =Tasto funzione premuto
063200941103      * $FC = Tasti abilitati
063300941103      * OUTPUT
063400941103      * IN99 = ERrore
063500941103      ************************************************************
063600940506     C     CTRKEY        BEGSR
063700980608     C*
063800980608     C                   SETOFF                                       99
063900980608     C*
064000940506     C                   SELECT
064100940506     C     $TASTO        WHENEQ    F01
064200980529     C                   z-add     01            posfun
064300940720     C     $FC(1)        IFEQ      '0'
064400940506     C                   SETON                                        99
064500940506     C                   ENDIF
064600940506     C     $TASTO        WHENEQ    F02
064700980529     C                   z-add     02            posfun
064800940720     C     $FC(2)        IFEQ      '0'
064900940506     C                   SETON                                        99
065000940506     C                   ENDIF
065100940506     C     $TASTO        WHENEQ    F03
065200980529     C                   z-add     03            posfun
065300940720     C     $FC(3)        IFEQ      '0'
065400940506     C                   SETON                                        99
065500940506     C                   ENDIF
065600940506     C     $TASTO        WHENEQ    F04
065700980529     C                   z-add     04            posfun
065800940720     C     $FC(4)        IFEQ      '0'
065900940506     C                   SETON                                        99
066000940506     C                   ENDIF
066100940506     C     $TASTO        WHENEQ    F05
066200980529     C                   z-add     05            posfun
066300940720     C     $FC(5)        IFEQ      '0'
066400940506     C                   SETON                                        99
066500940506     C                   ENDIF
066600940506     C     $TASTO        WHENEQ    F06
066700980529     C                   z-add     06            posfun
066800940720     C     $FC(6)        IFEQ      '0'
066900940506     C                   SETON                                        99
067000940506     C                   ENDIF
067100940506     C     $TASTO        WHENEQ    F07
067200980529     C                   z-add     07            posfun
067300940720     C     $FC(7)        IFEQ      '0'
067400940506     C                   SETON                                        99
067500940506     C                   ENDIF
067600940506     C     $TASTO        WHENEQ    F08
067700980529     C                   z-add     08            posfun
067800940720     C     $FC(8)        IFEQ      '0'
067900940506     C                   SETON                                        99
068000940506     C                   ENDIF
068100940615     C     $TASTO        WHENEQ    F09
068200980529     C                   z-add     09            posfun
068300940720     C     $FC(09)       IFEQ      '0'
068400940506     C                   SETON                                        99
068500940506     C                   ENDIF
068600940506     C     $TASTO        WHENEQ    F10
068700980529     C                   z-add     10            posfun
068800940720     C     $FC(10)       IFEQ      '0'
068900940506     C                   SETON                                        99
069000940506     C                   ENDIF
069100940506     C     $TASTO        WHENEQ    F11
069200980529     C                   z-add     11            posfun
069300940720     C     $FC(11)       IFEQ      '0'
069400940506     C                   SETON                                        99
069500940506     C                   ENDIF
069600940506     C     $TASTO        WHENEQ    F12
069700980529     C                   z-add     12            posfun
069800940720     C     $FC(12)       IFEQ      '0'
069900940506     C                   SETON                                        99
070000940506     C                   ENDIF
070100940506     C     $TASTO        WHENEQ    F13
070200980529     C                   z-add     13            posfun
070300940720     C     $FC(13)       IFEQ      '0'
070400940506     C                   SETON                                        99
070500940506     C                   ENDIF
070600940506     C     $TASTO        WHENEQ    F14
070700980529     C                   z-add     14            posfun
070800940720     C     $FC(14)       IFEQ      '0'
070900940506     C                   SETON                                        99
071000940506     C                   ENDIF
071100940506     C     $TASTO        WHENEQ    F15
071200980529     C                   z-add     15            posfun
071300940720     C     $FC(15)       IFEQ      '0'
071400940506     C                   SETON                                        99
071500940506     C                   ENDIF
071600940506     C     $TASTO        WHENEQ    F16
071700980529     C                   z-add     16            posfun
071800940720     C     $FC(16)       IFEQ      '0'
071900940506     C                   SETON                                        99
072000940506     C                   ENDIF
072100940506     C     $TASTO        WHENEQ    F17
072200980529     C                   z-add     17            posfun
072300940720     C     $FC(17)       IFEQ      '0'
072400940506     C                   SETON                                        99
072500940506     C                   ENDIF
072600940506     C     $TASTO        WHENEQ    F18
072700980529     C                   z-add     18            posfun
072800940720     C     $FC(18)       IFEQ      '0'
072900940506     C                   SETON                                        99
073000940506     C                   ENDIF
073100940506     C     $TASTO        WHENEQ    F19
073200980529     C                   z-add     19            posfun
073300940720     C     $FC(19)       IFEQ      '0'
073400940506     C                   SETON                                        99
073500940506     C                   ENDIF
073600940506     C     $TASTO        WHENEQ    F20
073700980529     C                   z-add     20            posfun
073800940720     C     $FC(20)       IFEQ      '0'
073900940506     C                   SETON                                        99
074000940506     C                   ENDIF
074100940506     C     $TASTO        WHENEQ    F21
074200980529     C                   z-add     21            posfun
074300940720     C     $FC(21)       IFEQ      '0'
074400940506     C                   SETON                                        99
074500940506     C                   ENDIF
074600940506     C     $TASTO        WHENEQ    F22
074700980529     C                   z-add     22            posfun
074800940720     C     $FC(22)       IFEQ      '0'
074900940506     C                   SETON                                        99
075000940506     C                   ENDIF
075100940506     C     $TASTO        WHENEQ    F23
075200980529     C                   z-add     23            posfun
075300940720     C     $FC(23)       IFEQ      '0'
075400940506     C                   SETON                                        99
075500940506     C                   ENDIF
075600940506     C     $TASTO        WHENEQ    F24
075700980529     C                   z-add     24            posfun
075800940720     C     $FC(24)       IFEQ      '0'
075900940506     C                   SETON                                        99
076000940506     C                   ENDIF
076100940506     C                   ENDSL
076200980601
076300940506     C                   ENDSR
076400940916     C/EJECT
076500941103      ************************************************************
076600941103      * GESTIONE F02 VIDEO D1
076700941103      ************************************************************
076800940916     C     F02D1         BEGSR
076900941103      *
077000941103      * per decodificare la videata richiamo la routine dei ctrl
077100941103      * e poi disattivo gli eventuali messaggi di errore
077200940916     C                   EXSR      CTRD1
077300941103      *
077400940916     C                   MOVE      *ALL'0'       IN5098
077500940916     C                   MOVEA     IN5098        *IN(50)
077600941103      *
077700940916     C                   ENDSR
077800940224     C/EJECT
077900941103      ************************************************************
078000941103      * GESTIONE F03 VIDEO D1
078100941103      ************************************************************
078200940307     C     F03D1         BEGSR
078300941103      *
078400940307     C                   MOVE      *ON           $FINE
078500980529    >C                   MOVE      '1'           RET007
078600980529     C* fine programma
078700940325     C                   EXSR      ENDPGM
078800980529     C*
078900940307     C                   ENDSR
079000940916     C/EJECT
079100941103      ************************************************************
079200941103      * GESTIONE F05 VIDEO D1
079300941103      ************************************************************
079400940916     C     F05D1         BEGSR
079500941103      *
079600940916     C                   MOVE      *ON           $INZD1
079700941103      *
079800940916     C                   ENDSR
079900961213     C/EJECT
080000961213      ************************************************************
080100961213      * GESTIONE F10 VIDEO D1
080200961213      * AGGIUNTA RECORD
080300961213      ************************************************************
080400961213     C     F10D1         BEGSR
080500961213      *
080600980601    >C                   RESET                   ana008ds
080700961213    >C                   MOVEL     '01'          OPZ008
080800980601    >C                   MOVEL     ana008ds      KPJBU
080900961213    >C                   CALL      'ANA008R'
081000961213     C                   PARM                    KPJBA
081100980601    >C                   MOVEL     KPJBU         ana008ds
081200961213      * ritorno da PGM gestione
081300961213    >C                   EXSR      RTN008
081400961213      *
081500961213     C                   MOVE      *HIVAL        $LASTV
081600961213      *
081700961213     C                   ENDSR
081800940307     C/EJECT
081900941103      ************************************************************
082000941103      * GESTIONE F12 VIDEO D1
082100941103      ************************************************************
082200940307     C     F12D1         BEGSR
082300941103      *
082400941103      *  se la 1a videata è la lista oppure sono arrivato qui con F17,
082500941103      *  per F12 torno alla lista
082600940307     C     $LOP          IFEQ      2
082700940307     C     WINKR         OREQ      *ON
082800940307     C                   MOVE      'S1'          $GEST
082900941103      *  altrimenti, per F12 torno a menu
083000940307     C                   ELSE
083100940307     C                   MOVE      *ON           $FINE
083200980529    >C                   MOVE      '2'           RET007
083300980529     C                   EndIf
083400941103      *
083500980529     C                   EndSr
083600940307     C/EJECT
083700941103      ************************************************************
083800941103      * GESTIONE F18 VIDEO D1
083900941103      ************************************************************
084000940307     C     F18D1         BEGSR
084100941103      *
084200940307     C                   MOVE      'D2'          $GEST
084300940307     C                   MOVEL     *ON           $INZD2
084400941103      *
084500940307     C                   ENDSR
084600940307     C/EJECT
084700941103      ************************************************************
084800941103      * GESTIONE F24 VIDEO D1
084900941103      * input
085000941103      * $ULKD1 = Ultimo tasto funzione emesso per video D1
085100941103      * $FX1 =Testi tasti funzione
085200941103      * $FC1 = Abilitazioni
085300941103      * $FR1 = Inversioni di fondo
085400941103      ************************************************************
085500940307     C     F24D1         BEGSR
085600941103      *
085700940506     C                   MOVEA     $FX1          $FX
085800940506     C                   MOVEA     $FC1          $FC
085900940506     C                   MOVEA     $FR1          $FR
086000940506     C                   Z-ADD     $ULKD1        $ULK
086100941011     C                   Z-ADD     79            $L01              3 0
086200941011     C                   Z-ADD     79            $L02              3 0
086300940506     C                   EXSR      RIEKEY
086400940506     C                   Z-ADD     $ULK          $ULKD1            3 0
086500940720     C                   MOVEA     $FC           $FC1
086600940506     C                   MOVEL     $KEY1         Z1KE1
086700060224A1148C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
086800940506     C                   MOVEL     $KEY2         Z1KE2
086900940506     C                   WRITE     FMTZ1
087000941103      *
087100941103      *
087200940307     C                   ENDSR
087300940307     C/EJECT
087400941103      ************************************************************
087500941103      * CONTROLLO VIDEATA PARZIALIZZAZIONI
087600941103      ************************************************************
087700940307     C     CTRD1         BEGSR
087800941103      *
087900940127     C                   SETOFF                                       99
088000941103      * non imposto pos. del cursore
088100940127     C                   Z-ADD     0             H1RIGA
088200940127     C                   Z-ADD     0             H1COLO
088300941103      *
088400941103    > * << elencare gli eventuali controlli sui campi di
088500941103    > *    parzializzazione, ricordando che quasi sempre essi
088600941103    > *    non ne avranno perchè determinano solo il range di
088700941103    > *    scrittura della lista e quindi (nel caso di campi
088800941103    > *    alfanumerici) saranno trattati come chiavi parziali >>
088900941103      *
089000940117     C                   ENDSR
089100940224     C/EJECT
089200941103      ************************************************************
089300941103      * GESTIONE LISTA
089400941103      ************************************************************
089500940127     C     GESS1         BEGSR
089600941103      * inizializzazione videata
089700940223     C     $INZS1        IFEQ      *ON
089800940127     C                   EXSR      INZS1
089900940223     C                   MOVE      *OFF          $INZS1
090000940127     C                   ENDIF
090100941103      * emissione piede videata se proveniente da altra
090200941103      * salvataggio valore formato di provenienza
090300940202     C     $LASTV        IFNE      'S1'
090400940202     C                   WRITE     FMTT1
090500940224     C                   EXSR      GESPIE
090600940202     C                   MOVE      $LASTV        $LASTG
090700940202     C                   MOVE      'S1'          $LASTV
090800980608     C                   ENDIF
090900941103      *
091000940223     C     WMAX          IFEQ      0
091100940211     C                   WRITE     FMTD3                                        NO RCD
091200940211     C                   ENDIF
091300980529     C* Inizializzazione indice tasti funzione personalizzati
091400980529     C                   Eval      PosFun = 25
091500980601      *
091600980601     C                   EXFMT     FMTC1
091700980601     C* azzero la posizione del cursore
091800980601     C                   Z-ADD     0             H1RIGA
091900980601     C                   Z-ADD     0             H1COLO
092000980601     C*
092100940204     C     C1NRR         IFNE      0
092200940204     C                   Z-ADD     C1NRR         WSFL
092300940204     C                   ENDIF
092400980601     C                   IF        SFLNRR > 0
092500940127     C                   Z-ADD     SFLNRR        C1RCD
092600980601     C                   ENDIF
092700980601     C* inizializzazione indice opzioni personalizzate
092800980601     C                   Eval      PosOpz = 0
092900980608     C* CONTROLLO ABILITAZIONE TASTI
093000980608     C* INPUT
093100980608     C* $FC2 = Abilitazioni videata S1
093200980608     C* OUTPUT
093300980608     C* *IN97 + *IN99 Errori
093400980608     C                   MOVEA     $FC2          $FC
093500980608     C                   EXSR      CTRKEY
093600941103      *
093700940506     C     *IN99         IFEQ      *OFF
093800941103      *
093900941103      *
0940009401271    C                   SELECT
094100941103      * F3=Fine
094200940506     C     $TASTO        WHENEQ    F03
094300940309     C                   EXSR      F03S1
094400941103      * F10=Immissione
094500940506     C     $TASTO        WHENEQ    F10
094600940309     C                   EXSR      F10S1
094700941103      * F12=Ritorno
094800940506     C     $TASTO        WHENEQ    F12
094900940309     C                   EXSR      F12S1
095000941103      * F17=Parzializzazioni
095100940506     C     $TASTO        WHENEQ    F17
095200940309     C                   EXSR      F17S1
095300941103      * F18=Modifica valori assunti
095400940506     C     $TASTO        WHENEQ    F18
095500940309     C                   EXSR      F18S1
095600941103      * F23=Altre opzioni
095700940506     C     $TASTO        WHENEQ    F23
095800940309     C                   EXSR      F23S1
095900941103      * F24=Altri tasti
096000940506     C     $TASTO        WHENEQ    F24
096100940309     C                   EXSR      F24S1
096200941103      * ROLL-UP
096300940506     C     $TASTO        WHENEQ    ROLLUP
096400940127     C                   EXSR      ROLS1
096500980529     C* Personalizzata
096600980529     C     $PGF2(PosFun) Whenne    *BLANK
096700980529     C                   EXSR      FperS1
096800941103      *
0969009401271O   C                   OTHER
097000941103      * CONTROLLO DATI
097100980529     C                   ExSr      CTRC1
097200980529     C     *IN99         IfEq      *OFF
097300980529     C                   ExSr      CTRS1
097400980529     C                   EndIf
097500980601     C*
0976009805291-   C                   EndSl
097700980608     C                   Else
097800980608     C                   SetOn                                        97
097900980601     C*
098000980529     C                   EndIf
098100980601     C*
098200940127     C                   ENDSR
098300940224     C/EJECT
098400941103      ************************************************************
098500941103      * INIZIALIZZAZIONE LISTA
098600941103      ************************************************************
098700940127     C     INZS1         BEGSR
098800941103      * pulizia SFL
098900940128     C                   SETOFF                                         3031
099000940131     C                   WRITE     FMTC1
099100940128     C                   SETON                                          31
099200941103      *
099300941103      * riempimento opzioni
099400940223     C                   MOVEA     $TX1          $TX
099500940223     C                   MOVEA     $FL1          $FL
099600940309     C                   Z-ADD     0             WCOZ
099700941103      *  equivale al num.di elementi della schiera $TX1
099800980601     C                   Z-ADD     OpzLen        WMXO
099900980601     C                   Z-ADD     $C1Op1        $L01
100000980601     C                   Z-ADD     $C1Op2        $L02
100100940128     C                   EXSR      RIEOPZ
100200950403     C                   Z-ADD     23            X
100300950403     C                   MOVE      FF23          $FC2(X)
100400940608     C                   MOVEL     WOPZ1         C1OPZ1
100500060224A1148C                   MOVEL     $ALLOPT       H1ALLOPT
100600940608     C                   MOVEL     WOPZ2         C1OPZ2
100700940225     C                   Z-ADD     WCOZ          C1COZ
100800941103      *
100900941103      * CARICAMENTO SFL A PAGINE
101000941103      *
101100940201     C                   Z-ADD     0             S1NRR
101200940128     C                   Z-ADD     0             WMAX
101300941103      *
101400941103      * Posizionamento su file pilota
101500940302     C                   CLEAR                   C1POS
101600941011    >C                   EXSR      SETSIF
101700941103      * ripristino i valori del posizionamento
101800940302     C                   MOVE      W1POS         C1POS
101900941103      *
102000941103      * Lettura file pilota
102100941011    >C                   EXSR      REDSIF
102200941103      *
102300980601     C* se arrivo dalla parzializzazione devo caricare solo la prima pagina
102400980601     C                   IF        $LastV = 'D1'
102500980601     C                   Z-ADD     SFLPAG        WPAG
102600980601     C                   ENDIF
102700940127     C                   EXSR      ROLS1
102800980601     C* imposto il caricamento sfl del numero massimo di righe per pagina
102900980601     C                   Z-ADD     SFLPAG        WPAG
103000941103      *
103100940127     C                   ENDSR
103200940127     C/EJECT
103300941103      ************************************************************
103400941103      * CARICAMENTO PAGINA LISTA
103500941103      ************************************************************
103600940127     C     ROLS1         BEGSR
103700941103      *
103800940128     C                   SETOFF                                       32
103900940223     C                   Z-ADD     0             Y
104000940127     C                   Z-ADD     WMAX          S1NRR
104100941103      *
104200941103      * Leggo dal file anagrafico per caricare la lista
104300941103      *
1044009401311    C     $EFILE        DOWEQ     *OFF
104500940201     C     Y             ANDLT     WPAG
104600941103      *
104700940131     C                   EXSR      RIES1
104800941103      *
104900940127     C                   ADD       1             S1NRR
105000940127     C                   ADD       1             Y
105100941103      *
105200940131     C                   WRITE     FMTS1
105300941103      *
105400941011    >C                   EXSR      REDSIF
105500941103      *
1056009401271-   C                   ENDDO
105700941103      *
105800940223     C                   Z-ADD     S1NRR         WMAX                 30
105900941103      *
106000941103      * POSIZIONAMENTO AL 1° RCD DELLA PAGINA
106100941103      *
106200940201     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
106300940127     C                   MVR                     RESTO             3 0
106400940131     C     PAGINE        MULT      SFLPAG        C1RCD
1065009401271    C     RESTO         IFGT      0
106600940127     C                   ADD       1             C1RCD
1067009401271E   C                   ELSE
106800940407     C                   SUB       SFLPAG        C1RCD
106900940407     C                   ADD       1             C1RCD
1070009401271-   C                   ENDIF
107100941103      *
107200940127     C                   ENDSR
107300940224     C/EJECT
107400941103      ************************************************************
107500941103      * POSIZIONAMENTO RCD ARCHIVIO PILOTA
107600941103      ************************************************************
107700941011    >C     SETSIF        BEGSR
107800941103      *
107900940224     C                   SELECT
108000941103    > * Ord.1 = chiave ordinamento 1
108100940224     C     $ORD          WHENEQ    1
108200941103    > *-----------------------------------------------------------
108300941103    > * Gestione posizionamento
108400941103    > * Se il campo W1POS è > *blank imposto il posizionamento con
108500941103    > * W1POS altrimenti lo imposto con il valore di parametrizzazione
108600941103    > *-----------------------------------------------------------
108700940302     C     W1POS         IFNE      *BLANK
108800941011    >C                   MOVEL     W1POS         SIFSOC
108900940302     C                   ELSE
109000941011    >C                   MOVEL     D1SOC         SIFSOC
109100940302     C                   END
109200941012    >C     K1SIF1        SETLL     ANSIF01
109300941212     C     $ORD          WHENEQ    2
109400941212      * Ord.2 = capofila
109500941212     C     W1POS         IFNE      *BLANK
109600941212    >C                   MOVEL     W1POS         SIFCPA
109700941212     C                   ELSE
109800941212    >C                   MOVEL     D1CPA         SIFCPA
109900941212     C                   END
110000941212    >C     K1SIF2        SETLL     ANSIF02
110100940224     C                   ENDSL
110200941103      *
110300940224     C                   ENDSR
110400940128     C/EJECT
110500941103      ************************************************************
110600941103      * LETTURA RCD ARCHIVIO PILOTA
110700941103      ************************************************************
110800941011    >C     REDSIF        BEGSR
110900941103      *
111000940131     C                   MOVEL     *OFF          $EFILE
111100940131     C                   MOVEL     *OFF          $RCDOK
111200941103      *
1113009401311    C     $EFILE        DOUEQ     *ON
111400940131     C     $RCDOK        OREQ      *ON
111500941103      *
1116009401312    C                   SELECT
111700940131     C     $ORD          WHENEQ    1
111800941011    >C                   READ      ANSIF01                                21
111900941212     C     $ORD          WHENEQ    2
112000941212    >C                   READ      ANSIF02                                21
1121009401312-   C                   ENDSL
112200941103      *
112300940131     C                   MOVE      *IN21         $EFILE
112400941103      *
1125009401312    C     $EFILE        IFEQ      *OFF
112600941011    >C                   EXSR      SELSIF
1127009401312-   C                   ENDIF
112800941103      *
1129009401311-   C                   ENDDO
113000941103      *
113100940131     C                   MOVE      *IN21         *IN33                          SflEnd
113200941103      *
113300940131     C                   ENDSR
113400940224     C/EJECT
113500941103      ************************************************************
113600941103      * CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
113700941103      ************************************************************
113800940224     C     CLCPAG        BEGSR
113900941103      * Input :
114000941103      * - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
114100941103      *          cursore
114200941103      * - SFLPAG = numero rcd per pagina sfl
114300941103      * Output :
114400941103      * - WPAG = pagina fino a cui deve essere ricaricato il sfl
114500941103      *
114600940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
114700940224     C                   MVR                     RESTO             3 0
114800940224     C     RESTO         IFGT      0
114900940224     C                   ADD       1             PAGINE
115000940224     C                   ENDIF
115100940224     C     PAGINE        MULT      SFLPAG        WPAG
115200941103      *
115300940224     C                   ENDSR
115400940131     C/EJECT
115500941103      ************************************************************
115600941103      * SELEZIONI SULLA RIGA LETTA
115700941103      * impostare a *OFF il campo $RCDOK per scartare un record che
115800941103      * non rientra nei parametri di parzializzazione
115900941103      * Eseguire una *HIVAL SETLL al raggiungimento di un limite
116000941103      * superiore stabilito nei parametri
116100941103      *
116200941103      ************************************************************
116300941011    >C     SELSIF        BEGSR
116400941103      *
116500940202     C                   MOVEL     *ON           $RCDOK
116600940131A1211X*                  MOVEL     *ALL'9'       ALL9             15
116700940131A1211C                   MOVEL     *hival        ALL9             15
116800941103      * creo limite massimo parzializzazioni
116900941103    > * << per ogni valore alfanumerico creo il limite superiore
117000941103    > *    per cui caricare, creando una variabile che contiene
117100941103    > *    il valore digitato nel campo a video riempito di "9" >>
117200941012    >C     D1SOC         CAT       ALL9:0        SOC999
117300941212    >C     D1CPA         CAT       ALL9:0        CPA999
117400941103      *
117500940314     C                   SELECT
117600941103      *
117700941103      * SELEZIONI RISPETTO AI VALORI RCD
117800941103      *
117900941103    > * << elaborare eventuali selezioni che non riguardano le
118000941103    > *    parzializzazioni ma più propriamente i dati del rcd ;
118100941103    > *    esempio : esclusione degli annullati. >>
118200941103      *
118300941103      * SELEZIONI RISPETTO ALLE PARZIALIZZAZIONI
118400941103      *
118500941103    > * oggetto non compreso nella scelta
118600941011    >C     SIFSOC        WHENLT    D1SOC
118700941012    >C     SIFSOC        ORGT      SOC999
118800941011    >C                   MOVEL     *OFF          $RCDOK
118900941012    >C     SIFSOC        IFGT      SOC999
119000941011    >C     $ORD          ANDEQ     1
119100941011    >C     *HIVAL        SETLL     ANSIF01
119200941011    >C                   ENDIF
119300941212    > * Capofila non compreso nella scelta
119400941212    >C     SIFCPA        WHENLT    D1CPA
119500941212    >C     SIFCPA        ORGT      CPA999
119600941212    >C                   MOVEL     *OFF          $RCDOK
119700941212    >C     SIFCPA        IFGT      CPA999
119800941212    >C     $ORD          ANDEQ     2
119900941212    >C     *HIVAL        SETLL     ANSIF02
120000941212    >C                   ENDIF
120001151125>>>>>C                   WHEN      sifDfv > *LOVAL AND sifDfv < d1dtFiVl
120002151125>>>>>C                   EVAL      $rcdOk = *OFF
120100941212      * Data oltre la fine validita' del record
120200941212    >C****       SIFDFV    WHGE DAMG10
120300941212    >C**++                 MOVEL*OFF      $RCDOK
120400941103      *
120500940202     C                   ENDSL
120600941103      *
120700940202     C                   ENDSR
120800940131     C/EJECT
120900941103      ************************************************************
121000941103      * RIEMPIMENTO RIGA LISTA
121100941103      ************************************************************
121200940131     C     RIES1         BEGSR
121300941103      *
121400940131     C                   CLEAR                   FMTS1
121500941103    > * << riempio i campi del sfl, compresi gli hidden con i valori
121600941103    > *    del rcd letto >>
121700941103      *
121800941011    >C                   MOVEL     SIFSOC        S1SOC
121900941212    >C                   MOVEL     SIFRGS        S1RGS
122000941212    >C                   MOVEL     SIFCPA        S1CPA
122100950110     C                   MOVEL     SIFDFV        XDTAMG
122200961209     C                   CALLB     'XDT4000'
122300950110     C                   PARM                    XDTAMG           10
122400950110     C                   PARM                    XDTGMA            6 0
122500950110     C                   PARM      2             XDTSTA            1 0
122600950110 4   C     XDTSTA        IFEQ      0                                            *
122700950110     C                   MOVE      XDTGMA        S1DFV
122800950110 4-  C                   ENDIF                                                  *
122900941103      *
123000940131     C                   ENDSR
123100940309     C/EJECT
123200941103      ************************************************************
123300941103      * GESTIONE F03 VIDEO S1
123400941103      ************************************************************
123500940309     C     F03S1         BEGSR
123600941103      *
123700940309     C                   MOVE      *ON           $FINE
123800941103      * fine programma
123900940325     C                   EXSR      ENDPGM
124000941103      *
124100940309     C                   ENDSR
124200940309     C/EJECT
124300941103      ************************************************************
124400941103      * GESTIONE F10 VIDEO S1
124500941103      * AGGIUNTA RECORD
124600941103      ************************************************************
124700940309     C     F10S1         BEGSR
124800941103      *
124900980601    >C                   RESET                   ana008ds
125000941011    >C                   MOVEL     '01'          OPZ008
125100980601    >C                   MOVEL     ana008ds      KPJBU
125200941011    >C                   CALL      'ANA008R'
125300940309     C                   PARM                    KPJBA
125400980601    >C                   MOVEL     KPJBU         ana008ds
125500941103      * ritorno da PGM gestione
125600941011    >C                   EXSR      RTN008
125700940309     C     WINZS1        IFEQ      *ON
125800940309     C                   MOVE      *ON           $INZS1
125900941103      * se esistevano già righe sul sfl
126000941103      * calcolo pagina a cui deve posizionarsi
126100940309     C     WSFL          IFGT      0
126200940309     C                   EXSR      CLCPAG
126300941103      * altrimenti carico solo la 1a pagina
126400940309     C                   ELSE
126500940309     C                   Z-ADD     SFLPAG        WPAG
126600940309     C                   ENDIF
126700940309     C                   ENDIF
126800941103      *
126900940627     C                   MOVE      *HIVAL        $LASTV
127000941103      *
127100940309     C                   ENDSR
127200940309     C/EJECT
127300941103      ************************************************************
127400941103      * GESTIONE F12 VIDEO S1
127500941103      ************************************************************
127600940309     C     F12S1         BEGSR
127700941103      *
127800941103      *  se la 1a videata sono le parzializzazioni, per F12 torno a queste
127900940309     C     $LOP          IFEQ      1
128000940309     C                   MOVE      'D1'          $GEST
128100941103      *  se la 1a videata è questa, per F12 torno a menu
128200940309     C                   ELSE
128300940309     C                   MOVE      *ON           $FINE
128400940309     C                   END
128500941103      *
128600940309     C                   ENDSR
128700940309     C/EJECT
128800941103      ************************************************************
128900941103      * GESTIONE F17 VIDEO S1
129000941103      ************************************************************
129100940309     C     F17S1         BEGSR
129200941103      *
129300940309     C                   MOVE      'D1'          $GEST
129400941103      * forza certi valori nel caso il pgm sia stato richiamato con
129500941103      * chiave specifica
129600980601    >C     SOC007        IfNe      *BLANK
129700980601<<< >C                   Movel     *BLANK        SOC007
129800980601     C                   EndIf
129900980601    >C     CPA007        IfNe      *BLANK
130000980601<<< >C                   Movel     *BLANK        SOC007
130100980601     C                   EndIf
130200941103      *
130300940309     C                   ENDSR
130400940309     C/EJECT
130500941103      ************************************************************
130600941103      * GESTIONE F18 VIDEO S1
130700941103      ************************************************************
130800940309     C     F18S1         BEGSR
130900941103      *
131000940309     C                   MOVE      'D2'          $GEST
131100940309     C                   MOVEL     *ON           $INZD2
131200940831     C     $ORD          IFEQ      99
131300940831     C                   Z-ADD     1             $ORD
131400980601     C                   ENDIf
131500941103      *
131600940309     C                   ENDSR
131700940309     C/EJECT
131800941103      ************************************************************
131900941103      * GESTIONE F23 VIDEO S1
132000941103      ************************************************************
132100940309     C     F23S1         BEGSR
132200941103      *
132300980601     C                   Z-ADD     $C1Op1        $L01
132400980601     C                   Z-ADD     $C1Op2        $L02
132500940309     C                   EXSR      RIEOPZ
132600950403     C                   Z-ADD     23            X
132700950403     C                   MOVE      FF23          $FC2(X)
132800940608     C                   MOVEL     WOPZ1         C1OPZ1
132900060224A1148C                   MOVEL     $ALLOPT       H1ALLOPT
133000940608     C                   MOVEL     WOPZ2         C1OPZ2
133100940309     C                   Z-ADD     WCOZ          C1COZ
133200941103      *
133300940309     C                   ENDSR
133400940309     C/EJECT
133500941103      ************************************************************
133600941103      * GESTIONE F24 VIDEO S1
133700941103      ************************************************************
133800940309     C     F24S1         BEGSR
133900941103      *
134000941103      *
134100940506     C                   MOVEA     $FX2          $FX
134200940506     C                   MOVEA     $FC2          $FC
134300940506     C                   MOVEA     $FR2          $FR
134400940506     C                   Z-ADD     $ULKS1        $ULK
134500980601     C                   Z-ADD     $S1L01        $L01
134600980601     C                   Z-ADD     $S1L02        $L02
134700940506     C                   EXSR      RIEKEY
134800940506     C                   Z-ADD     $ULK          $ULKS1            3 0
134900940720     C                   MOVEA     $FC           $FC2
135000940506     C                   MOVEL     $KEY1         Z2KE1
135100060224A1148C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
135200940506     C                   MOVEL     $KEY2         Z2KE2
135300940506     C                   WRITE     FMTZ2
135400941103      *
135500940309     C                   ENDSR
135600940128     C/EJECT
135700941103      ************************************************************
135800941103      * CONTROLLO TESTATA LISTA
135900941103      ************************************************************
136000940131     C     CTRC1         BEGSR
136100941103      *
136200940201     C                   MOVE      *OFF          *IN99
136300941103      *
136400941103      * SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
136500941103      * L'INIZIALIZZAZIONE LISTA
136600941103      *
136700940721    >C     C1POS         IFNE      W1POS
136800940202     C                   EXSR      POSS1
136900980601     C                   EndIf
137000941103      *
137100940202     C                   ENDSR
137200940202     C/EJECT
137300941103      ************************************************************
137400941103      * CONTROLLI PER POSIZIONAMENTO SU LISTA
137500941103      ************************************************************
137600940202     C     POSS1         BEGSR
137700941103      *
137800941103      * CONTROLLO SE ESISTONO OPZIONI ATTIVE
137900941103      *
1380009402021    C     WMAX          IFGT      0
138100940202     C                   READC     FMTS1                                  21
1382009402022    C     *IN21         DOWEQ     *OFF
138300940202     C     *IN99         ANDEQ     *OFF
138400941103      *
1385009402023    C     S1OPZ         IFNE      0
138600940202     C                   SETON                                        51  99
1387009402023-   C                   ENDIF
138800941103      * se rilevata opzione attiva, attivo sflnxtchg
1389009402023    C     *IN99         IFEQ      *ON
139000940202     C                   MOVE      *ON           *IN32
139100940202     C                   UPDATE    FMTS1
1392009402023E   C                   ELSE
139300940202     C                   READC     FMTS1                                  21
1394009402023-   C                   ENDIF
139500941103      *
1396009402022-   C                   ENDDO
139700941103      *
1398009402021-   C                   ENDIF
139900941103      *
140000941103      * SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
140100941103      * AL VALORE DI POSIZIONAMENTO :
140200941103      * se questo è maggiore del valore del rispettivo campo di
140300941103      * parzializzazione lo uso, altrimenti eseguo un ricaricamento
140400941103      * con i valori di parzializzazione.
140500941103      *
1406009402021    C     *IN99         IFEQ      *OFF
140700940721    >C                   MOVE      *BLANK        W1POS
140800941103      *
1409009402022    C                   SELECT
141000941103      *
141100940202     C     $ORD          WHENEQ    1
141200941011    >C     C1POS         IFGT      D1SOC
141300941012    >C                   MOVEL     C1POS         W1POS
141400980601     C                   EndIf
141500941103      *
141600940202     C                   MOVE      *ON           *IN99
141700940223     C                   MOVE      *ON           $INZS1
141800980601      *
141900941212     C     $ORD          WHENEQ    2
142000941212    >C     C1POS         IFGT      D1CPA
142100941212    >C                   MOVEL     C1POS         W1POS
142200980601     C                   ENDIf
142300941212      *
142400941212     C                   MOVE      *ON           *IN99
142500941212     C                   MOVE      *ON           $INZS1
142600941103      *
142700941103      *
1428009402022-   C                   ENDSL
142900941103      *
1430009402021-   C                   ENDIF
143100941103      *
143200940131     C                   ENDSR
143300940131     C/EJECT
143400941103      ************************************************************
143500941103      * CONTROLLO OPZIONI LISTA
143600941103      ************************************************************
143700940131     C     CTRS1         BEGSR
143800941103      *
143900940202     C                   MOVEL     *OFF          $ESCI
144000940201     C                   SETOFF                                       99
144100940131     C                   Z-ADD     0             S1OPZ
144200941103      *
144300941103      * Leggo il sfl solo se ci sono rcd
1444009401311    C     WMAX          IFGT      0
144500940131     C                   READC     FMTS1                                  21
144600941103      *
144700941103      * esce se fine sfl o errore che richiede l'uscita
1448009401312    C     *IN21         DOWEQ     *OFF
144900940131     C     $ESCI         ANDEQ     *OFF
145000940201     C                   Z-ADD     S1NRR         C1RCD
145100941103      * ctrl su riga
145200940131     C                   EXSR      RECS1
145300941103      * gestione opzioni
1454009401313    C     S1OPZ         IFNE      0
145500940201     C     *IN99         ANDEQ     *OFF
145600940131     C                   EXSR      OPZS1
1457009401273-   C                   ENDIF
145800941125      * se rilevato errore, attivo sflnxtchg
1459009402013    C     *IN99         IFEQ      *ON
146000940131     C                   MOVE      *ON           *IN32
1461009402233-   C                   ENDIF
146200941103      *
146300940131     C                   Z-ADD     0             S1OPZ
146400941103      *
146500940127     C                   UPDATE    FMTS1
146600941125     C                   MOVE      *OFF          *IN32
146700941103      *
146800941103      * leggo prossimo rcd dal sfl se no richiesta uscita ciclo
1469009806013    C     $ESCI         IFEQ      *OFF
147000940131     C                   READC     FMTS1                                  21
147100941103      * a fine ciclo ripristino il flag richiesta iniz.sfl
1472009402014    C     *IN21         IFEQ      *ON
147300940201     C                   MOVE      WINZS1        $INZS1
147400941103      * calcolo pagina a cui deve posizionarsi
147500940728     C     WSFL          IFGT      0
147600940728     C                   EXSR      CLCPAG
147700940728     C                   ELSE
147800940728     C                   Z-ADD     SFLPAG        WPAG
147900940728     C                   ENDIF
1480009402014-   C                   ENDIF
1481009402013-   C                   ENDIF
148200941103      *
1483009401272-   C                   ENDDO
148400941103      *
1485009401311-   C                   ENDIF
148600941103      *
148700940127     C                   ENDSR
148800940127     C/EJECT
148900941103      ************************************************************
149000941103      * CONTROLLO CAMPI I/O RIGA LISTA
149100941103      ************************************************************
149200940131     C     RECS1         BEGSR
149300941103      *
149400941103      * reset indicatori DSPATR
149500940201     C                   MOVE      *ALL'0'       IN4049           10
149600940201     C                   MOVEA     IN4049        *IN(40)
149700941103      *
149800941103      * Controllo se Utente Autorizzato alla scelta
149900940202     C     S1OPZ         IFNE      0
150000940131     C                   Z-ADD     1             X
150100940223     C     S1OPZ         LOOKUP    $OP1(X)                                21
150200980608     C* mi salvo l'indice dell' opzione
150300980608     C   21              Z-add     X             PosOpz            2 0
150400940211     C     *IN21         IFEQ      *OFF
150500940223     C     $FL1(X)       OREQ      'N'
150600940131     C                   MOVE      *ON           $ESCI
150700940201     C                   SETON                                        504099
150800940131     C                   ENDIF
150900940202     C                   ENDIF
151000941103      *
151100940131     C                   ENDSR
151200940224     C/EJECT
151300941103      ************************************************************
151400941103      * GESTIONE OPZIONI LISTA
151500941103      ************************************************************
151600940131     C     OPZS1         BEGSR
151700980528     C*
151800980528     C                   Z-ADD     RIRI          CURR
151900980528     C                   Z-ADD     COCO          CURC
152000980528     C*
1521009402251    C                   SELECT
152200941103      *
152300941103      * se opz=Selezione passo la chiave al pgm chiamante
152400940225     C     S1OPZ         WHENEQ    1
152500980601    > * << passare nei campi appositi della DS ANA007DS il valore
152600941103    > *    della chiave della VL univoca (se c'è) >>
152700941011    >C                   MOVE      S1OPZ         OPZ007
152800941011    >C                   MOVE      S1SOC         SOC007
152900941212    >C                   MOVE      S1CPA         CPA007
153000940225     C                   MOVE      *ON           $ESCI
153100940225     C                   MOVE      *ON           $FINE
153300940909     C                   MOVE      *HIVAL        $LASTV
153301080317      *
153302080317G1553 * Chiamata programma ricodifiche
153303080317 "   C     S1OPZ         WHENEQ    77
153304080317 "   C                   Reset                   Ana770ds
153305080317 "   C                   MOVEL     '77'          opz770
153306080317 "   c                   Movel     *blank        TPRIC770
153307080317 "   C                   Movel     *blank        TPCAMP770
153308080317 "   C                   Movel     'SOCIETA'     TPDATO770
153309080317 "   c                   movel     S1SOC         CODICEO770
153310080317 "    * Selezione Righe tutte /Caricamento da struttura
153311080317 "   C                   Move      *on           CARSTR770
153312080317 "   C                   movel     'ANA007R'     PgmCall770
153313080317 "    *
153314080317 "   C                   MOVEL(P)  Ana770DS      KPJBU
153315080317 "   C                   CALL      'ANA770R'
153316080317 "   C                   PARM                    KPJBA
153317080317G1553C                   MOVE      *HIVAL        $LASTV
153318080317      *
153400941103      * se altre opzioni (o nessuna opzione) richiamo pgm gestione
153500980601     C* se selezionata opzione personalizzata, richiamo il programma
153600980601     C* corrispondente
153700980601     C     $PGO1(PosOpz) Whenne    *Blank
153800980601    >C                   Reset                   ana008ds
153900980601    >C                   Move      XscSoc        Soc008
154000980601    >C                   Movel     S1Opz         Opz008
154100980601     C* Passare nei campi appositi della DS Ana008Ds il valore
154200980601     C* della chiave della VL univoca
154300980601    >C                   MOVE      S1SOC         SOC008
154400980601     C*
154500980601    >C                   Movel     Ana008Ds      KPJBU
154600980601     C                   Call      $PGO1(PosOpz)                        22
154700980601     C                   PARM                    KPJBA
154800980601     C                   PARM                    NOMPGM
154900980601     C*
155000980601    >C                   MOVEL     KPJBU         Ana008Ds
155100980601     C* ritorno da PGM
155200980601     C                   EXSR      RTN008
1553009402251O   C                   OTHER
155400941103      *
155500980601    >C                   RESET                   ana008ds
155600980601    > * << passare nei campi appositi della DS Ana008DS il valore
155700941103    > *    della chiave della VL univoca (se c'è) >>
155800941011    >C                   MOVEL     S1OPZ         OPZ008
155900941011    >C                   MOVEL     S1SOC         SOC008
156000941103      *
156100980601    >C                   MOVEL     ana008ds      KPJBU
156200941011    >C                   CALL      'ANA008R'
156300940314     C                   PARM                    KPJBA
156400980601    >C                   MOVEL     KPJBU         ana008ds
156500941103      *
156600941103      * ritorno da PGM gestione
156700980601    >C                   ExSr      Rtn008
156800941103      * se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
1569009806012    C     *IN99         IfEq      *On
157000980601     C                   Seton                                        40
1571009806012-   C                   EndIf
157200941103      *
157300980601     C                   Move      *Hival        $LastV
1574009806011-   C                   EndSl
157500941103      *
157600940131     C                   ENDSR
157700940223     C/EJECT
157800941103      ************************************************************
157900941103      * GESTIONE RITORNO DA PGM DI GESTIONE
158000941103      ************************************************************
158100941011    >C     RTN008        BEGSR
158200941103      *
158300941103      * modo di ritorno
158400941103      *
1585009402231    C                   SELECT
158600941103    > * << questi modi di utilizzo dei valori di ritorno dal
158700941103    > *    pgm di manutenzione rcd di anagrafica sono delle
158800941103    > *    proposte, normalmente sempre valide, ma modificabili
158900941103    > *    per situazioni particolari >>
159000941103      * 1 = F3
159100941011    >C     RET008        WHENEQ    '1'
159200940224     C                   MOVE      *ON           $ESCI                          sospese READC
159300940223     C                   MOVE      *ON           $FINE                          fine pgm
159400941103      * 2 = F12
159500941011    >C     RET008        WHENEQ    '2'
159600940223     C                   MOVE      *ON           $ESCI                          sospese READC
159700941103      *
1598009402231-   C                   ENDSL
159900941103      *
160000941103      * operazione eseguite dal pgm chiamato
160100941103      *
1602009402231    C                   SELECT
160300941103      * 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
160400941011    >C     OPR008        WHENEQ    '1'
160500980608     C                   MOVE      *ON           WInzS1                         iniz.sfl fine ciclo
160600941103      *
1607009402231-   C                   ENDSL
160800941103      *
160900941103      * funzione non eseguibile per errore :
161000941103      *
1611009402231    C                   SELECT
161200941103      * 1 = funzione richiamata chiusa in errore
161300941103    > *  eventualmente gestire altri codici di errore
161400941011    >C     ERR008        WHENEQ    '1'
161500940223     C                   MOVE      *ON           $ESCI                          sospese READC
161600940223     C                   SETON                                        5299      emiss.SFLMSGID
161700941103      *
1618009402231-   C                   ENDSL
161900941103      *
162000940223     C                   ENDSR
162100940223     C/EJECT
162200941103      ************************************************************
162300941103      * GESTIONE VALORI ASSUNTI
162400941103      ************************************************************
162500940131     C     GESD2         BEGSR
162600941103      *
162700980601     C     $INZD2        IfEq      *ON
162800980601     C                   ExSr      INZD2
162900980601     C                   Move      *OFF          $INZD2
163000980601     C                   EndIf
163100980601     C*
163200980601     C     $LASTV        IfNe      'D2'
163300980601     C                   ExSr      GESPIE
163400980601   B1C     $LASTV        IFNE      *HIVAL
163500940202     C                   MOVE      $LASTV        $LASTG
163600980601     C                   ENDIF
163700940202     C                   MOVE      'D2'          $LASTV
163800940707     C                   ENDIF
163900941103      *
164000941103      * se esistono errori sulla videata
164100941103      * emetto la write del formato a indicatori spenti per vedere
164200941103      * le eventuali decodifiche
164300980601     C     *IN99         IfEq      *ON
164400980601     C                   MoveA     *IN           WIN
164500980601     C                   Move      *ALL'0'       IN5098           49
164600980601     C                   MoveA     IN5098        *IN(50)
164700980601     C                   Write     FMTD2
164800980601     C                   MoveA     WIN           *IN
164900980601     C                   EndIf
165000941103      *
165100940131     C                   EXFMT     FMTD2
165200941103      *
165300980601     C* azzero la posizione del cursore
165400980601     C                   Z-ADD     0             H1RIGA
165500980601     C                   Z-ADD     0             H1COLO
165600980601     C*
165700980601     C* CONTROLLO ABILITAZIONE TASTI
165800980601     C* INPUT
165900980601     C* $FC3 = Abilitazioni videata D2
166000980601     C* OUTPUT
166100980601     C* *IN97 + *IN99 Errori
166200980601     C                   MoveA     $FC3          $FC
166300980601     C                   ExSr      CTRKEY
166400980601     C*
166500980601   B1C     *IN99         IfEq      *Off
166600980601     C*
166700980601     C                   Select
166800941103      * F3=Fine
166900940506     C     $TASTO        WHENEQ    F03
167000940309     C                   EXSR      F03D2
167100941103      * F12=Ritorno
167200940506     C     $TASTO        WHENEQ    F12
167300940309     C                   EXSR      F12D2
167400941103      * F24=Altri tasti
167500940916     C     $TASTO        WHENEQ    F24
167600940916     C                   EXSR      F24D2
167700941103      *
167800940131     C                   OTHER
167900940131     C                   EXSR      CTRD2
1680009401312    C     *IN99         IFEQ      *OFF
168100940211     C                   MOVE      $LASTG        $GEST
1682009401313    C                   SELECT
168300940202     C     $LASTG        WHENEQ    'S1'
168400941103      * se è stato modificato l'ordinamento di default, ricarico la lista
168500980601     C     $ORD          IfNe      D2ORD
168600980601     C                   Move      *On           $INZS1
168700980601     C                   Move      *Blank        W1POS
168800980601     C                   EndIf
168900980601     C     $LASTG        WhenEq    'D1'
169000941103      * per ora niente
1691009401313-   C                   ENDSL
169200941103      * definisco i nuovi valori di default
169300940201     C                   Z-ADD     D2LOP         $LOP
169400940131     C                   Z-ADD     D2ORD         $ORD
169500941103    > * << elenco altri dati default >>
1696009806012-   C                   EndIf
169700941103      *
169800980601     C                   EndSl
169900941103      *
1700009806082-   C                   EndIf
170100980608      *
170200980601     C                   EndSr
170300940223     C/EJECT
170400941103      ************************************************************
170500941103      * INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
170600941103      ************************************************************
170700940201     C     INZD2         BEGSR
170800941103      *
170900940131     C                   CLEAR                   FMTD2
171000940201     C                   MOVEL     $LOP          D2LOP
171100940831     C                   MOVE      $ORD          D2ORD
171200941103    > * << elenco altri dati default >>
171300941103      *
171400940131     C                   ENDSR
171500940309     C/EJECT
171600941103      ************************************************************
171700941103      * GESTIONE F03 VIDEO D2
171800941103      ************************************************************
171900940309     C     F03D2         BEGSR
172000941103      *
172100940309     C                   MOVE      *ON           $FINE
172200941103      * fine programma
172300940325     C                   EXSR      ENDPGM
172400941103      *
172500940309     C                   ENDSR
172600940309     C/EJECT
172700941103      ************************************************************
172800941103      * GESTIONE F12 VIDEO D2
172900941103      ************************************************************
173000940309     C     F12D2         BEGSR
173100941103      *
173200940309     C                   MOVE      $LASTG        $GEST
173300941103      *
173400940309     C                   ENDSR
173500940916     C/EJECT
173600941103      ************************************************************
173700941103      * GESTIONE F24 VIDEO D2
173800941103      ************************************************************
173900940916     C     F24D2         BEGSR
174000941103      *
174100940916     C                   MOVEA     $FX3          $FX
174200940916     C                   MOVEA     $FC3          $FC
174300940916     C                   MOVEA     $FR3          $FR
174400940916     C                   Z-ADD     $ULKD2        $ULK
174500980601     C                   Z-ADD     $D2L01        $L01
174600980601     C                   Z-ADD     $D2L02        $L02
174700940916     C                   EXSR      RIEKEY
174800940916     C                   Z-ADD     $ULK          $ULKD2            3 0
174900940916     C                   MOVEA     $FC           $FC3
175000940916     C                   MOVEL     $KEY1         Z3KE1
175100060224A1148C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
175200940916     C                   MOVEL     $KEY2         Z3KE2
175300940916     C                   WRITE     FMTZ3
175400941103      *
175500940916     C                   ENDSR
175600940223     C/EJECT
175700941103      ************************************************************
175800941103      * CONTROLLO VIDEATA VALORI ASSUNTI
175900941103      ************************************************************
176000940131     C     CTRD2         BEGSR
176100941103      *
176200940131     C                   SETOFF                                       99
176300941103      * non imposto pos. del cursore
176400940131     C                   Z-ADD     0             H1RIGA
176500940131     C                   Z-ADD     0             H1COLO
176600941103      *
176700941103      * OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
176800940201     C     D2LOP         IFEQ      0
176900940131     C                   SETON                                          5099
177000940131     C                   END
177100941103      *
177200941103      * OBBLIGATORIO VALORE PER ORDINAMENTO
177300980529     C     D2ORD         IfEq      0
177400980529     C                   SetOn                                          5199
177500980529     C                   EndIf
177600941103    > * << elenco controlli altri dati default >>
177700941103      *
177800940131     C                   ENDSR
177900940223     C/EJECT
178000980529     C************************************************************
178100980529     C* Chiamata programma con tasto personalizzato
178200980529     C************************************************************
178300980529     C     FPerS1        BEGSR
178400980529     C*
178500980529     C* salvo la DS di ingresso
178600980601    >C                   MOVEL     ANA007DS      $SaveDS
178700980529     C*
178800980601    >C                   MOVEL     ANA007DS      KPJBU
178900980529     C                   CALL      $PGF2(PosFun)                        22
179000980529     C                   PARM                    KPJBA
179100980529     C                   PARM                    NOMPGM
179200980529     C*
179300980529     C* ripristino la DS di ingresso
179400980601    >C                   MOVEL     $SaveDS       ANA007DS
179500980529     C*
179600980529     C                   ENDSR
179700980529     C/EJECT
179800941103      ************************************************************
179900941103      * AGGIORNAMENTO VALORI ASSUNTI
180000941103      ************************************************************
180100940201     C     AGGMPE        BEGSR
180200941103      *
180300940201     C                   CLEAR                   ANMPE000
180400940201     C                   MOVEL     XSCSOC        MPESOC
180500940201     C                   MOVEL     DSPGM         MPEPGM
180600940201     C                   MOVEL     KNMUS         MPEPRF
180700941013     C     OPZ007        IFEQ      '01'
180800941013     C                   MOVEL     'PX'          MPETIP
180900941013     C                   ELSE
181000941013     C                   MOVEL     'PV'          MPETIP
181100941013     C                   ENDIF
181200980529     C     K4MPE2        Chain     ANMPE02L                           21
181300980529     C                   Movel     DSMPE         MPEPAR
181400980529     C     *IN21         IfEq      *ON
181500980529     C                   Write     ANMPE000
181600980529     C                   Else
181700980529     C                   Update    ANMPE000
181800980529     C                   EndIf
181900941103      *
182000980529     C                   EndSr
182100940223     C/EJECT
182200941103      ************************************************************
182300941103      * OPERAZIONI INIZIALI
182400941103      ************************************************************
182500940131     C     *INZSR        BEGSR
182600941103      *
182700000120A0740C     *DMY          Move      Udate         ZDateISO
182800000120A0740C     *JobRun       Move      ZDateISO      ZDate             6 0
182900000120      *
183000941103      * Reperimento parametri
183100941103      *
183200940117     C     *ENTRY        PLIST
183300940117     C                   PARM                    KPJBA
183400941103      *
183500941103      * Reperimento dati società
183600941103      *
183700941103     C                   MOVEL     'SOC001'      TIPXSC
183800941103     C                   MOVEL     *BLANK        SOCXSC
183900941103     C                   EXSR      REPSOC
184000941103     C     RTNXSC        IFNE      '1'
184100941103     C                   MOVEL     XSOCDS        SOC001
184200941103     C                   ELSE
184300941103     C                   MOVE      *ON           $FINE
184400980528    >C                   MOVE      '1'           Err007
184500941103     C                   EXSR      ENDPGM
184600941103     C                   ENDIF
184700980601     C*
184800980601     C* reperisco le dimensioni delle variabili che conterranno i tasti
184900980601     C                   EVAL      $D1L01 = %Size(Z1Ke1)
185000980601     C                   EVAL      $D1L02 = %Size(Z1Ke2)
185100980601     C                   EVAL      $S1L01 = %Size(Z2Ke1)
185200980601     C                   EVAL      $S1L02 = %Size(Z2Ke2)
185300980601     C                   EVAL      $C1Op1 = %Size(C1Opz1)
185400980601     C                   EVAL      $C1Op2 = %Size(C1Opz2)
185500980601     C                   EVAL      $D2L01 = %Size(Z3Ke1)
185600980601     C                   EVAL      $D2L02 = 0
185700941103      * Reperimento autorizzazioni
185800941103      *
185900941103    > * << nel caso l'anagrafica non gestisse le autorizzazioni
186000941103    > *    asteriscare la chiamata alla subroutine REPAUT >>
186100941103    > *************        EXSR REPAUT
186106080317     C*
186200941103      *
186300941103      * Reperimento opzioni
186400941103      *
186500940128     C                   EXSR      REPOPZ
186600941103      *
186700941103      * Reperimento tasti di funzione
186800941103      *
186900940506     C                   EXSR      REPKEY
187000941103      *
187100980528     C                   CAllb     'X46USREXT'
187200980528     C                   PARM                    $OP1
187300980528     C                   PARM                    $TX1
187400980528     C                   PARM                    $FL1
187500980528     C                   PARM                    $PGO1
187600980528     C                   PARM      $dimOp        NumOpz
187700980528     C                   PARM                    $FX2
187800980528     C                   PARM                    $FC2
187900980528     C                   PARM                    $PGF2
188000980528     C                   PARM                    DsPgm
188100980528     C                   PARM                    xsccut
188200980528     C                   PARM                    xscgrp
188300980528     C*
188400980528     C                   Z-add     1             OPZLEN
188500980528     C     *blank        Lookup    $TX1(opzlen)                           21
188600980528     C                   if        *in21 = *on
188700980528     C                   sub       1             OPZLEN
188800980528     C                   else
188900980528     C                   Z-add     $DimOp        OPZLEN
189000980528     C                   endif
189100941103      *
189200940117     C                   ENDSR
189300940225     C/EJECT
189400941103      ************************************************************
189500941103      * REPERIMENTO AUTORIZZAZIONI UTENTE
189600941103      ************************************************************
189700941103      *          REPAUT    BEGSR
189800941103      *
189900980601      *                    CLEARAUTDS
190000941103      *
190100941103      *                    Z-ADD0         XCAGRP           GRUPPO
190200941103      *                    MOVELKNMUS     XCAPRF           PROFILO
190300941103    > *                    MOVEL'OBJALL'  XCAFNC           FUNZIONE
190400941103      *                    MOVEL*ZERO     XCAVFU           VARIABILE
190500941103      *                    MOVE 'CK'      XCATCK           TIPO CTRL
190600941103      *
190700961209      *                    CALLB'XCHKAUT'
190800980601      *                    PARM           AUTDS
190900941103      * utente non abilitato
191000941103      *          XCARTN    IFEQ 0
191100941103    > *                    MOVELXCADDS    AUTDS
191200941103      *                    ELSE
191300941103    > *                    CLEARAUTDS
191400941103      *                    END
191500941103      *
191600941103      *                    ENDSR
191700940223     C/EJECT
191800941103      ************************************************************
191900941103      * INIZIALIZZAZIONE VARIABILI
192000941103      ************************************************************
192100940223     C     INZVAR        BEGSR
192200980601     C*
192300980601     C* Dati da PGM chiamante
192400980601     C*
192500980601    >C                   MOVEL     KPJBU         ANA007DS
192600980601     C* Reperimento valori default utente
192700941026     C                   EXSR      REPDFT
192701080317     C*
192900980601     C* Definizione delle opzioni utilizzabili a pgm
193100940506     C                   EXSR      DEFOPZ
193102080317      *
193200941103      * Variabili per gestione videate
193400940223     C                   MOVE      *OFF          $FINE                          fine pgm
193500940223     C                   MOVE      *OFF          $INZD1                         inizializzione D1
193600940223     C                   MOVE      *OFF          $INZS1                         inizializzione S1
193700940223     C                   MOVE      *OFF          $INZD2                         inizializzione D2
193800940223     C                   MOVE      *OFF          $EFILE                         memo fine file
193900940223     C                   MOVE      *OFF          $ESCI                          rich.uscita ciclo
194000940223     C                   MOVE      *OFF          $RCDOK                         rcd letto valido
194100940509     C                   MOVE      *HIVAL        $LASTV
194200940506     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
194300940506     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
194400940916     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
194500980601     C* azzero la posizione del cursore
194600980601     C                   Z-ADD     0             H1RIGA
194700980601     C                   Z-ADD     0             H1COLO
194800980601      *
194900940318     C                   MOVE      *ON           $INZD1
195000940318     C                   MOVE      *ON           $INZS1
195100941103      *
195200980601     C* Variabili appoggio
195300980601     C                   Z-ADD     SFLPAG        WPAG
195400980601     C                   MOVEL     *OFF          WOPR
195500980601     C                   CLEAR                   W1POS
195600980601     C*
195700940223     C                   SELECT
195800940223     C     $LOP          WHENEQ    1
195900940223     C                   MOVE      'D1'          $GEST
196000940223     C     $LOP          WHENEQ    2
196100940223     C                   MOVE      'S1'          $GEST
196200941104     C                   EXSR      INZD1
196300941104     C                   MOVE      *OFF          $INZD1
196400940223     C                   ENDSL
196500980601     C*
196600980601     C* Valorizzazione campi univoci testate
196700940223     C                   CLEAR                   FMTT1
196800940223     C                   MOVEL     KNSIF         NOMSIF
196900941111     C                   MOVEL     XSCDSI        NOMDIT
197000941027     C                   MOVEL     DSPGM         NOMPGM
197100941212      *Inserisco in DAMG10 la data odierna in formato ISO
197200000120A0740C*****              MOVEL     UYEAR         ANNO
197300000120  "  C*****              MOVEL     UMONTH        MESE
197400000120  "  C*****              MOVEL     UDAY          GIORNO
197500000120  "  C*****              MOVEL     DGMA06        XDTGMA
197600000120A0740C                   MOVEL     ZDate         XDTGMA
197700961209     C                   CALLB     'XDT4000'
197800941212     C                   PARM                    XDTAMG           10
197900941212     C                   PARM                    XDTGMA            6 0
198000941212     C                   PARM      4             XDTSTA            1 0
198100941212 4   C     XDTSTA        IFEQ      0                                            *
198200941212     C                   MOVE      XDTAMG        DAMG10
198300941212 4-  C                   ENDIF                                                  *
198400940223     C                   ENDSR
198500940223     C/EJECT
198600941103      ************************************************************
198700941103      * REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
198800941103      ************************************************************
198900940131     C     REPDFT        BEGSR
199000941103      *
199100940201     C                   MOVEL     XSCSOC        MPESOC
199200940201     C                   MOVEL     DSPGM         MPEPGM
199300940131     C                   MOVEL     KNMUS         MPEPRF
199400941013     C     OPZ007        IFEQ      '01'
199500941013     C                   MOVEL     'PX'          MPETIP
199600941013     C                   ELSE
199700941013     C                   MOVEL     'PV'          MPETIP
199800941013     C                   ENDIF
199900940202     C     K4MPE2        CHAIN(N)  ANMPE02L                           21
200000940131     C     *IN21         IFEQ      *OFF
200100940131     C                   MOVEL     MPEPAR        DSMPE
200200941103      * nel caso sia la prima volta che l'utente apre questa anagrafica
200300941103      * i suoi valori di default vengono decisi a pgm
200400940131     C                   ELSE
200500940201     C                   Z-ADD     1             $LOP
200600940131     C                   Z-ADD     1             $ORD
200700941103    > * << elenco altri dati default >>
200800940131     C                   END
200900941103      *
201000940131     C                   ENDSR
201100940223     C/EJECT
201200941103      ************************************************************
201300941103      * DEFINIZIONE OPZIONI UTILIZZABILI A PGM
201400941103      ************************************************************
201500940225     C     DEFOPZ        BEGSR
201600941103      *
201700941103      * Questa routine deve valorizzare la schiera $FL1
201800941103      * con S o N a seconda della utilizzabilità o meno
201900941103      * dell'opzione correlata
202000941103      *
202100941103      * Inizialmente si considerano tutte le opzioni come valide
202200980601     C     1             DO        $DIMOP        X
202300940225     C                   MOVE      'S'           $FL1(X)
202400940225     C                   END
202401080317      *
202402080317G1553C* Verifico se gestite le ricodifiche
202403080317 "   C                   Call      'ANA773R'                            33
202404080317 "   C                   Parm                    Kpjba
202405080317 "   C                   Parm                    Soc001
202406080317 "   C                   Parm                    GestRic           1
202407080317 "   C                   If        GestRic = *off
202408080317 "    * Se non gestite disattivo l'opzione 77=Ricodifiche
202409080317 "   C                   Z-ADD     1             X
202410080317 "   C     77            LOOKUP    $OP1(X)                                21
202411080317 "   C     *IN21         IFEQ      *ON
202412080317 "   C                   MOVE      'N'           $FL1(X)
202413080317 "   C                   ENDIF
202414080317G1553C                   Endif
202500941103      *
202600941103      * Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
202700941103      * con S o N a seconda della utilizzabilità o meno
202800941103      * del tasto funzione correlato
202900941103      *
203000941103      * Inizialmente ripristino le abilitazioni come da schiera a tempo
203100941103      * di compilazione
203200940715     C                   RESET                   $FC1
203300940715     C                   RESET                   $FR1
203400940715     C                   RESET                   $FC2
203500940715     C                   RESET                   $FR2
203600940919     C                   RESET                   $FC3
203700940919     C                   RESET                   $FR3
203800941103      *
203900941103      * Dopodichè, a seconda del richiamo e delle autorizzazioni
204000941103      * invalido alcune opzioni
204100940225     C                   SELECT
204200941103      * Se pgm richiamato in ricerca disattivo :
204300941011    >C     OPZ007        WHENEQ    '01'
204400941103      * F10=Immissione
204500940506     C                   Z-ADD     10            X
204600940720     C                   MOVE      '0'           $FC2(X)
204700941103      * F3=Fine
204800940603     C                   Z-ADD     3             X
204900940720     C                   MOVE      '0'           $FC2(X)
205000941103      * 2=Modifica
205100940307     C                   Z-ADD     1             X
205200940225     C     02            LOOKUP    $OP1(X)                                21
205300940225     C     *IN21         IFEQ      *ON
205400940225     C                   MOVE      'N'           $FL1(X)
205500940225     C                   ENDIF
205600941103      * 3=Copia
205700940225     C                   Z-ADD     1             X
205800940225     C     03            LOOKUP    $OP1(X)                                21
205900940225     C     *IN21         IFEQ      *ON
206000940225     C                   MOVE      'N'           $FL1(X)
206100940225     C                   ENDIF
206200941103      * 4=Annullamento
206300940225     C                   Z-ADD     1             X
206400940225     C     04            LOOKUP    $OP1(X)                                21
206500940225     C     *IN21         IFEQ      *ON
206600940225     C                   MOVE      'N'           $FL1(X)
206700940225     C                   ENDIF
206701080317G1553C* 77=Ricodifiche
206702080317 "   C                   Z-ADD     1             X
206703080317 "   C     77            LOOKUP    $OP1(X)                                21
206704080317 "   C     *IN21         IFEQ      *ON
206705080317 "   C                   MOVE      'N'           $FL1(X)
206706080317G1553C                   ENDIF
207000941103      * Se pgm richiamato in interrogazione disattivo :
207100941011    >C     OPZ007        WHENEQ    '05'
207200941103      * F10=Immissione
207300940615     C                   Z-ADD     10            X
207400940720     C                   MOVE      '0'           $FC2(X)
207500941103      * 1=Selezione
207600940225     C                   Z-ADD     1             X
207700940225     C     01            LOOKUP    $OP1(X)                                21
207800940225     C     *IN21         IFEQ      *ON
207900940225     C                   MOVE      'N'           $FL1(X)
208000940225     C                   ENDIF
208100941103      * 2=Modifica
208200940225     C                   Z-ADD     1             X
208300940225     C     02            LOOKUP    $OP1(X)                                21
208400940225     C     *IN21         IFEQ      *ON
208500940225     C                   MOVE      'N'           $FL1(X)
208600940225     C                   ENDIF
208700941103      * 3=Copia
208800940225     C                   Z-ADD     1             X
208900940225     C     03            LOOKUP    $OP1(X)                                21
209000940225     C     *IN21         IFEQ      *ON
209100940225     C                   MOVE      'N'           $FL1(X)
209200940225     C                   ENDIF
209300941103      * 4=Annullamento
209400940225     C                   Z-ADD     1             X
209500940225     C     04            LOOKUP    $OP1(X)                                21
209600940225     C     *IN21         IFEQ      *ON
209700940225     C                   MOVE      'N'           $FL1(X)
209800940225     C                   ENDIF
209900941103      *
210000940225     C                   OTHER
210100941103      * Se pgm non richiamato, disattivo subito :
210200941103      * 1=Selezione
210300940225     C                   Z-ADD     1             X
210400940225     C     01            LOOKUP    $OP1(X)                                21
210500940225     C     *IN21         IFEQ      *ON
210600940225     C                   MOVE      'N'           $FL1(X)
210700940225     C                   ENDIF
211600941103      *
211700940225     C                   ENDSL
211800941103      *
211900940225     C                   ENDSR
212000940225     C/EJECT
212100941103      ************************************************************
212200941103      * REPERIMENTO OPZIONI
212300941103      ************************************************************
212400940225     C     REPOPZ        BEGSR
212500941103      *
212600940314     C     1             DO        $DIMOP        X
212700941103      *
212800940223     C     $MS1(X)       IFNE      *BLANK
212900941103      *
213000940223     C                   MOVEL     $MS1(X)       IDMSG
213100961209     C                   CALLB     'XRTVM'
213200940201     C                   PARM                    IDMSG            27
213300940201     C                   PARM                    TXTMSG          100
213400940201     C                   PARM                    ERRMSG            1
213500941103      *
213600940128     C     ERRMSG        IFNE      *ON
213700940223     C                   MOVEL     TXTMSG        $TX1(X)
213800940128     C                   ELSE
213900940223     C                   MOVEL     *ALL'?'       $TX1(X)
214000980528     C                   ENDIF
214100941103      *
214200980528     C                   ENDIF
214300941103      *
214400980528     C                   ENDDO
214500941103      *
214600940128     C                   ENDSR
214700940506     C/EJECT
214800941103      ************************************************************
214900941103      * REPERIMENTO TASTI DI FUNZIONE
215000941103      ************************************************************
215100940506     C     REPKEY        BEGSR
215200940506     C                   MOVEA     $FM1          $FM
215300950906     C                   MOVEA     $FC1          $FC
215400940506     C                   EXSR      REPKKK
215500940506     C                   MOVEA     $FX           $FX1
215600941103      *
215700940506     C                   MOVEA     $FM2          $FM
215800950906     C                   MOVEA     $FC2          $FC
215900940506     C                   EXSR      REPKKK
216000940506     C                   MOVEA     $FX           $FX2
216100941103      *
216200940919     C                   MOVEA     $FM3          $FM
216300950906     C                   MOVEA     $FC3          $FC
216400940919     C                   EXSR      REPKKK
216500940919     C                   MOVEA     $FX           $FX3
216600940919     C                   ENDSR
216700940506     C/EJECT
216800941103      ************************************************************
216900941103      * REPERIMENTO TASTI DI FUNZIONE
217000941103      ************************************************************
217100940506     C     REPKKK        BEGSR
217200941103      *
217300940506     C                   CLEAR                   $FX
217400940506     C     1             DO        24            X
217500941103      *
217600940506     C     $FM(X)        IFNE      *BLANK
217700950906     C     $FC(X)        ANDNE     '0'
217800941103      *
217900940506     C                   MOVEL     $FM(X)        IDMSG
218000961209     C                   CALLB     'XRTVM'
218100940506     C                   PARM                    IDMSG            27
218200940506     C                   PARM                    TXTMSG          100
218300940506     C                   PARM                    ERRMSG            1
218400941103      *
218500940506     C     ERRMSG        IFNE      *ON
218600940506     C                   MOVEL     TXTMSG        $FX(X)
218700940506     C                   ELSE
218800940506     C                   MOVEL     *ALL' '       $FX(X)
218900980528     C                   ENDIF
219000941103      *
219100980528     C                   ENDIF
219200941103      *
219300980528     C                   ENDDO
219400941103      *
219500940506     C                   ENDSR
219600940223     C/EJECT
219700941103      ************************************************************
219800941103      * DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
219900941103      ************************************************************
220000940223     C     DEFCAM        BEGSR
220100941103      *
220200941103      * klist
220300941103      *
220400941103    > * ANSIF01L
220500941012    >C     K1SIF1        KLIST
220600941011    >C                   KFLD                    SIFSOC
220700941212    > * ANSIF02L
220800941212    >C     K1SIF2        KLIST
220900941212    >C                   KFLD                    SIFCPA
221000941103      *
221100941103      * ANMPE02L
221200940128     C     K4MPE2        KLIST
221300940128     C                   KFLD                    MPESOC
221400940201     C                   KFLD                    MPEPGM
221500940128     C                   KFLD                    MPETIP
221600941103      *
221700940117     C                   ENDSR
221800940223     C/EJECT
221900941103      ************************************************************
222000941103      * Reperimento dati società
222100941103      ************************************************************
222200941103     C     REPSOC        BEGSR
222300941103      *
222400961209     C                   CALLB     'XSOC'
222500941103     C                   PARM                    TIPXSC            6
222600941103     C                   PARM                    SOCXSC            3
222700941103     C                   PARM                    CDSXSC            9 0
222800941103     C                   PARM                    MODXSC            3
222900941103     C                   PARM      *BLANKS       RTNXSC            1
223000941103     C                   PARM                    XSOCDS
223100941103     C                   PARM                    KPJBA
223200941103      *
223300941103     C                   ENDSR
223400980608** $MS1 / $TX1
223500941011PROMSG    *LIBL     OPZ0001
223600941011PROMSG    *LIBL     OPZ0002
223700940131PROMSG    *LIBL     OPZ0003
223800961209PROMSG    *LIBL     OPZ0124
223900940131PROMSG    *LIBL     OPZ0005
223901080317PROMSG    *LIBL     OPZ0604       <Edi  #G1553#
224000980608** $OP1 / $FL1
22410094022501S      <Selezione
22420094022502S      <Modifica
22430094022503S      <Copia
22440094022504S      <Annullamento
22450094022505S      <Visualizzazione
22450108031777S      <Ricodifiche EURITMO           #G1553#
224600980608** TASTI DI FUNZIONE  D1 : $FM1 / $FX1
224700961213PROMSG    *LIBL     KEY0001  01
224800961213PROMSG    *LIBL     KEY0002  02
224900961213PROMSG    *LIBL     KEY0003  03
225000961213PROMSG    *LIBL     KEY0004  04
225100961213PROMSG    *LIBL     KEY0005  05
225200961213PROMSG    *LIBL     KEY0006  06
225300961213PROMSG    *LIBL     KEY0007  07
225400961213PROMSG    *LIBL     KEY0008  08
225500961213PROMSG    *LIBL     KEY0009  09
225600961213PROMSG    *LIBL     KEY0010  10
225700961213PROMSG    *LIBL     KEY0011  11
225800961213PROMSG    *LIBL     KEY0012  12
225900961213PROMSG    *LIBL     KEY0013  13
226000961213PROMSG    *LIBL     KEY0014  14
226100961213PROMSG    *LIBL     KEY0015  15
226200961213PROMSG    *LIBL     KEY0016  16
226300961213PROMSG    *LIBL     KEY0017  17
226400961213PROMSG    *LIBL     KEY0018  18
226500961213PROMSG    *LIBL     KEY0019  19
226600961213PROMSG    *LIBL     KEY0020  20
226700961213PROMSG    *LIBL     KEY0021  21
226800961213PROMSG    *LIBL     KEY0022  22
226900961213PROMSG    *LIBL     KEY0023  23
227000961213PROMSG    *LIBL     KEY0024  24
227100980608**  ABILITAZIONE D1 : $FC1 / $FR1
2272009407200N  01
2273009407200N  02
2274009407201N  03
2275009407201N  04
2276009407200N  05
2277009407200N  06
2278009407200N  07
2279009407200N  08
2280009407200N  09
2281009612131N  10
2282009407200N  11
2283009407201N  12
2284009407200N  13
2285009407200N  14
2286009407200N  15
2287009407200N  16
2288009407200N  17
2289009407201N  18
2290009407200N  19
2291009407200N  20
2292009407200N  21
2293009407200N  22
2294009509060N  23   Impostare a '1' se si utilizzano opzioni
2295009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
229600980608** TASTI DI FUNZIONE  S1 : $FM2 / $FX2
229700940916PROMSG    *LIBL     KEY0001  01
229800940916PROMSG    *LIBL     KEY0002  02
229900940916PROMSG    *LIBL     KEY0003  03
230000940916PROMSG    *LIBL     KEY0004  04
230100940916PROMSG    *LIBL     KEY0005  05
230200940916PROMSG    *LIBL     KEY0006  06
230300940916PROMSG    *LIBL     KEY0007  07
230400940916PROMSG    *LIBL     KEY0008  08
230500940916PROMSG    *LIBL     KEY0009  09
230600940916PROMSG    *LIBL     KEY0010  10
230700940916PROMSG    *LIBL     KEY0011  11
230800940916PROMSG    *LIBL     KEY0012  12
230900940916PROMSG    *LIBL     KEY0013  13
231000940916PROMSG    *LIBL     KEY0014  14
231100940916PROMSG    *LIBL     KEY0015  15
231200940916PROMSG    *LIBL     KEY0016  16
231300940916PROMSG    *LIBL     KEY0017  17
231400940916PROMSG    *LIBL     KEY0018  18
231500940916PROMSG    *LIBL     KEY0019  19
231600940916PROMSG    *LIBL     KEY0020  20
231700940916PROMSG    *LIBL     KEY0021  21
231800940916PROMSG    *LIBL     KEY0022  22
231900940916PROMSG    *LIBL     KEY0023  23
232000940916PROMSG    *LIBL     KEY0024  24
232100980608**  ABILITAZIONE S1 : $FC2 / $FR2
2322009409160N  01
2323009409160N  02
2324009409161N  03
2325009612090N  04
2326009409160N  05
2327009409160N  06
2328009409160N  07
2329009409160N  08
2330009409160N  09
2331009409161N  10
2332009409160N  11
2333009409161N  12
2334009409160N  13
2335009409160N  14
2336009409160N  15
2337009409160N  16
2338009409161N  17
2339009409161N  18
2340009409160N  19
2341009409160N  20
2342009409160N  21
2343009409160N  22
2344009509061N  23   Impostare a '1' se si utilizzano opzioni
2345009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
234600980608** TASTI DI FUNZIONE  D3 : $FM3 / $FX3
234700940506PROMSG    *LIBL     KEY0001  01
234800940506PROMSG    *LIBL     KEY0002  02
234900940506PROMSG    *LIBL     KEY0003  03
235000940506PROMSG    *LIBL     KEY0004  04
235100940506PROMSG    *LIBL     KEY0005  05
235200940506PROMSG    *LIBL     KEY0006  06
235300940506PROMSG    *LIBL     KEY0007  07
235400940506PROMSG    *LIBL     KEY0008  08
235500940506PROMSG    *LIBL     KEY0009  09
235600940506PROMSG    *LIBL     KEY0010  10
235700940506PROMSG    *LIBL     KEY0011  11
235800940506PROMSG    *LIBL     KEY0012  12
235900940506PROMSG    *LIBL     KEY0013  13
236000940506PROMSG    *LIBL     KEY0014  14
236100940506PROMSG    *LIBL     KEY0015  15
236200940506PROMSG    *LIBL     KEY0016  16
236300940506PROMSG    *LIBL     KEY0017  17
236400940506PROMSG    *LIBL     KEY0018  18
236500940506PROMSG    *LIBL     KEY0019  19
236600940506PROMSG    *LIBL     KEY0020  20
236700940506PROMSG    *LIBL     KEY0021  21
236800940506PROMSG    *LIBL     KEY0022  22
236900940506PROMSG    *LIBL     KEY0023  23
237000940506PROMSG    *LIBL     KEY0024  24
237100980608**  ABILITAZIONE D3 : $FC3 / $FR3
2372009407200N  01
2373009407200N  02
2374009407201N  03
2375009407200N  04
2376009407200N  05
2377009407200N  06
2378009407200N  07
2379009407200N  08
2380009407200N  09
2381009409160N  10
2382009407200N  11
2383009407201N  12
2384009407200N  13
2385009407200N  14
2386009407200N  15
2387009407200N  16
2388009409160N  17
2389009409160N  18
2390009407200N  19
2391009407200N  20
2392009407200N  21
2393009407200N  22
2394009509060N  23   Impostare a '1' se si utilizzano opzioni
2395009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
