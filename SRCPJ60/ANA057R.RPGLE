000100990524     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200990524     H*PARMS CVTOPT(*DATETIME)
000300940211     H DECEDIT('0,') DATEDIT(*DMY.)
000400941115      *
000500950410      *            GUIDA VARIAZIONE ESTREMI FISCALI
000600950410      *           ----------------------------------
000700940224      *
000800940224      *
000900940224      * DOCUMENTI MICROANALISI :
001000940224      *
001100940224      * NOTE TECNICHE :
001200940224      *
001300940224      *
001400940224      * PERSONALIZZAZIONI
001500940224      *
001600940224      *
001700940224      *
001800940314      *               MAPPA INDICATORI
001900940224      *
002000940307      *  21           GENERICO OPERAZIONI I/O
002100940224      *  22           GENERICO ERRORE OPERAZIONI I/O
002200940224      *  30           SFLDSP
002300940224      * N31           SFLCLR
002400940224      *  31           SFLDSPCTL
002500940224      *  32           SFLNXTCHG
002600940224      *  33           SFLEND
002700940224      *  39           OF PRTF
002800940224      *  40 <---> 49  DSPATR ERRORI SU SFL
002900940317    > *  Specificare l'uso dei singoli indicatori
003000940224      *  50 <---> 98  ERRORI SU VIDEO
003100940909    > *  Specificare l'uso dei singoli indicatori
003200940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
003300940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
003400940224      *  99           INDIC. GENERALE DI ERRORE
003500950407     F*----------------------------------------------------------------
003600000105A0732F***ANCPI01L  IF   E           K DISK
003700000105A0732F***                                  RENAME(ANCPI000:ANCPI01)
003800950410     F*------------
003900000105A0732F***ANCPI02L  IF   E           K DISK
004000000105A0732F***                                  RENAME(ANCPI000:ANCPI02)
004100950410     F*------------
004200950407     FANSOG01L  IF   E           K DISK
004300950407     F                                     RENAME(ANSOG000:ANSOG01)
004400950410     F*------------
004500950410     FANIND01L  IF   E           K DISK
004600950410     F                                     RENAME(ANIND000:ANIND01)
004700940211     F*------------
004800990525b0025F***ANMPE02L  UF A E           K DISK
004900940211     F*------------
005000950407     FANA057D   CF   E             WORKSTN
005100950410     F                                     RENAME(A057D1:FMTD1)
005200950410     F                                     RENAME(A057D2:FMTD2)
005300950410     F                                     RENAME(A057D3:FMTD3)
005400950410     F                                     RENAME(A057S1:FMTS1)
005500950410     F                                     RENAME(A057C1:FMTC1)
005600950410     F                                     RENAME(A057T1:FMTT1)
005700950410     F                                     RENAME(A057Z1:FMTZ1)
005800950410     F                                     RENAME(A057Z2:FMTZ2)
005900950410     F                                     RENAME(A057Z3:FMTZ3)
006000940131     F                                     SFILE(FMTS1:S1NRR)
006100940201     F                                     INFDS(DSFMT)
006200940128     D*----------------------------------------------------*
006300940302     D* Schiere per la gestione delle opzioni
006400940302     D*
006500100830R027 X**** $MS1            S             27    DIM(5) CTDATA PERRCD(1)
006501100831 "   X**** $TX1            S             50    DIM(5) ALT($MS1)
006502100831 "   X**** $OP1            S              2  0 DIM(5) CTDATA PERRCD(1)
006800100830R027 X**** $FL1            S              1    DIM(5) ALT($OP1)
006801100830R027 D $MS1            S             27    DIM(6) CTDATA PERRCD(1)
006802100831 "   D $TX1            S             50    DIM(6) ALT($MS1)
006803100831 "   D $OP1            S              2  0 DIM(6) CTDATA PERRCD(1)
006804100830R027 D $FL1            S              1    DIM(6) ALT($OP1)
006900941214     D $TX             S             50    DIM(50)
007000941214     D $FL             S              1    DIM(50)
007100940909     D* Schiere generali per la gestione dei tasti di funzione
007200940506     D $FM             S             27    DIM(24)
007300940506     D $FX             S             50    DIM(24)
007400940506     D $FC             S              1    DIM(24)
007500940506     D $FR             S              1    DIM(24)
007600940909     D* Schiere per la gestione dei tasti di funzione
007700940506     D* Video D1
007800940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
007900940506     D $FX1            S             50    DIM(24) ALT($FM1)
008000940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
008100940506     D $FR1            S              1    DIM(24) ALT($FC1)
008200940506     D* Schiere per la gestione dei tasti di funzione
008300940506     D* Video S1
008400940506     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
008500940506     D $FX2            S             50    DIM(24) ALT($FM2)
008600940506     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
008700940506     D $FR2            S              1    DIM(24) ALT($FC2)
008800940916     D* Schiere per la gestione dei tasti di funzione
008900940916     D* Video D2
009000940916     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
009100940916     D $FX3            S             50    DIM(24) ALT($FM3)
009200940916     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
009300940916     D $FR3            S              1    DIM(24) ALT($FC3)
009400940207     D* Valori assunti
009500990524B0025D XMPEDS        E DS                  INZ
009600990524B0025D XMPEDS1       E DS                  INZ
009700941111     D DSMPE           DS                  INZ
009800940207     D  $LOP                   1      1  0
009900940831     D  $ORD                   2      3  0
010000941026     D*-------------
010100941026     D* Parametri del PGM di gestione commenti ANA725R
010200950407    >D*  se non sono gestiti i commenti bisogna asteriscare
010300950407     D*725DS    EIDSANA725DS
010400950407     D*I            '0'                             RET725
010500950407     D*I            '0'                             OPR725
010600950407     D*I            '0'                             ERR725
010700950407     D* Ricerca soggetto
010800950407     D A040DS        E DS                  EXTNAME(ANA040DS) INZ
010900950407     D  CMT040       E                     INZ('0')
011000950407     D  RET040       E                     INZ('0')
011100950407     D  OPR040       E                     INZ('0')
011200950407     D  ERR040       E                     INZ('0')
011300950407     D*-------------
011400950301    >D* Parametri del PGM invio posta ANA727R
011500950407    >D*  se non è gestita la posta bisogna asteriscare
011600950414     D*727DS    EIDSANA727DS
011700950414     D*I            '0'                             RET727
011800950414     D*I            '0'                             OPR727
011900950414     D*I            '0'                             ERR727
012000940207     D*-------------
012100940211     D* Passaggio Parametri
012200940211     D KPJBA         E DS
012300940211     D*-------------
012400940225     D* Reperimento Autorizzazioni
012500941124     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
012600950411     D*-------------
012700950411     D* Passaggio Autorizzazioni
012800950411     D DSAUT           DS           512
012900941124     D*-------------
013000941124     D* Maschera dati autorizzazioni
013100950410     D GSOGDS        E DS                  EXTNAME(ANGSOGDS)
013200941209     D*-------------
013300941209     D* Dati di ambiente ottenuti da XSOC
013400941209    >D SOC001        E DS                  EXTNAME(XSOC001DS)
013500941209     D*-------------
013600941209     D* DS Interna per dati di output di XSOC
013700941209     D XSOCDS          DS          1000
013800940325     D*-------------
013900940325     D* Parametri in ricezione
014000950407     D A057DS        E DS                  EXTNAME(ANA057DS)
014100940325     D*-------------
014200000105A0732 * Ds contenente il record letto da ANA057R1
014300000105  "  D ancpi00f      E DS
014400000105  "  D*-------------
014500000105  "  D* Parametri per il pgm di lettura ANA057R1
014600000105  "  D Ana057ds1     E DS
014700000105A0732D*-------------
014800950407     D* Parametri del PGM di manutenzione ANA058R
014900950407     D A058DS        E DS                  EXTNAME(ANA058DS) INZ
015000950410     D  RET058       E                     INZ('0')
015100950410     D  OPR058       E                     INZ('0')
015200950410     D  ERR058       E                     INZ('0')
015201160503R224 X*ex R027 -----
015202160503 "   X*ex R027 Parametri del PGM di manutenzione  dati aggiuntivi
015203160503 "   X*ex R027 DGSD16DS     E DS                  inz
015204160503 "   X*ex R027 RETD16       E                     INZ('0')
015205160503 "   X*ex R027 OPRD16       E                     INZ('0')
015206160503 "   X*ex R027 ERRD16       E                     INZ('0')
015207160503 "   X*ex R027 CMTD16       E                     INZ('0')
015208160503R027 X*ex R027 DtFiVlD16    E                     INZ('0001-01-01')
015209160503R224 D*-------------
015210160503 "   D* Parametri del PGM guida dati aggiuntivi anagrafici
015211160503 "   D SDGSD15DS     E DS                  inz
015212160503 "   D  RETD15       E                     INZ('0')
015213160503 "   D  OPRD15       E                     INZ('0')
015214160503 "   D  ERRD15       E                     INZ('0')
015215160503 "   D  CMTD15       E                     INZ('0')
015216160509 "   D  DtFiVlD15    E                     INZ('0001-01-01')
015217160509R224 D  DtParD15     E                     INZ('0001-01-01')
015300940211     D*-------------
015400940211     D DSFMT           DS           512
015500940506     D  $TASTO               369    369
015600940211     D  NRG                  370    370
015700940211     D  NCL                  371    371
015800940211     D  SFLNRR               378    379B 0
015900940211     D*-------------
016000940211     D* posizione cursore
016100940211     D CURSOR          DS
016200940211     D  RIRI                   1      2B 0 INZ
016300940211     D  R$$                    2      2
016400940211     D  COCO                   3      4B 0 INZ
016500940211     D  C$$                    4      4
016600940127     D*-------------
016700940127     D* Reperimento nome PGM
016800940127     D STATUS         SDS           333
016900940127     D  DSPGM            *PROC
017000950411     D  PARMS            *PARMS
017100000110A0730D*-------------
017200000110  "  D* Parametri per ricerca codice ANATB00F
017300000110  "  D XTABDS        E DS                  INZ
017400000110  "  D*-------------
017500000110  "  D* DS esterna provincia
017600000110  "  D TABC17        E DS                  EXTNAME(ANGC17DS) INZ
017700000110  "  D*-------------
017800000110  "  D*DS esterna stato
017900000110A0730D TABG12        E DS                  EXTNAME(ANGG12DS) INZ
018000000103     D*-------------
018100000103     D* Costanti
018200000103     D* nr.di righe del sfl (SFLPAG)
018300000103     D SFLPAG          C                   CONST(8)
018400000103     D* dimensione della schiera $MS1
018500100830R027 X**** $DIMOP          C                   CONST(5)
018501100830R027 D $DIMOP          C                   CONST(6)
018600941117     D* data a 0
018700941117     D $DT0            C                   CONST('0001-01-01')
018800950410     D* data massima
018900950410     D $DT9            C                   CONST('9999-12-31')
019000000119A0735D***** Data massima nel formato passato da ANA040 ('2039-12-31')
019100000119A0735D***** ex-A0732  MaxRic          C                   CONST(311239)
019200000105     D*-------------
019300940506     D* Tasti di funzione
019400940506     D F01             C                   CONST(X'31')
019500940506     D F02             C                   CONST(X'32')
019600940506     D F03             C                   CONST(X'33')
019700940506     D F04             C                   CONST(X'34')
019800940506     D F05             C                   CONST(X'35')
019900940506     D F06             C                   CONST(X'36')
020000940506     D F07             C                   CONST(X'37')
020100940506     D F08             C                   CONST(X'38')
020200940506     D F09             C                   CONST(X'39')
020300940506     D F10             C                   CONST(X'3A')
020400940506     D F11             C                   CONST(X'3B')
020500940506     D F12             C                   CONST(X'3C')
020600940506     D F13             C                   CONST(X'B1')
020700940506     D F14             C                   CONST(X'B2')
020800940506     D F15             C                   CONST(X'B3')
020900940506     D F16             C                   CONST(X'B4')
021000940506     D F17             C                   CONST(X'B5')
021100940506     D F18             C                   CONST(X'B6')
021200940506     D F19             C                   CONST(X'B7')
021300940506     D F20             C                   CONST(X'B8')
021400940506     D F21             C                   CONST(X'B9')
021500940506     D F22             C                   CONST(X'BA')
021600940506     D F23             C                   CONST(X'BB')
021700940506     D F24             C                   CONST(X'BC')
021800940506     D ENTER           C                   CONST(X'F1')
021900940506     D ROLDWN          C                   CONST(X'F4')
022000940506     D ROLLUP          C                   CONST(X'F5')
022100950331     D*-------------
022200950331     D* Sottotitolo di testata
022300950410     D SOTTIT          C                   CONST('PROMSG    *LIBL-
022400000105     D                                          COS0095')
022500000105     D*----------
022600000119A0735D***** ex-A0732 Data di appoggio
022700000119  "  D***** ex-A0732 DtTemp          s              6  0
022800000114A0735D ZDateISO        S               D
022801100901R027 D $Sogg           S                   Like(S1Sog)
022802100901R027 D $DtFiVl         S               D
022900021014A0972D* Attributi video
023000021014A0972D Normale         C                   Const(X'32')
023100021014A0972D Protetto        C                   Const(X'A0')
023101160505R224 D*----------------------------------------------------*
023102160505  "  D* Controllo chiamata ricorsiva
023103160505  "  D X48ChPgmDS    E DS                  INZ
023104160505R224  *----------------------------------------------------*
023200940302     I*
023300940211     I/EJECT
023400940128     I*----------------------------------------------------*
023500000105A0732I* Ridenominazione Campi file ANCPI00F
023600000105  "  I***ANCPI01
023700000105  "  I***           CPISOGG                     CPICOD
023800000105  "  I***           CPIDTFIVL                   CPIDTF
023900000105  "  I***           CPIPARTIVA                  CPIIVA
024000000105  "  I***           CPIFRCPIVA                  CPIFPI
024100000105  "  I***           CPICDFISC                   CPICDF
024200000105  "  I***           CPIDESSOG1                  CPIDE1
024300000105  "  I***           CPIDESSOG2                  CPIDE2
024400000105  "  I***           CPIISCCIAA                  CPICIA
024500000105  "  I***           CPINATGIUR                  CPINAT
024600000105  "  I***           CPIINDRIZ                   CPIVIA
024700000105  "  I***           CPILOCALIT                  CPICIT
024800000105  "  I***           CPICAP                      CPICAP
024900000105  "  I***           CPIPROV                     CPIPRV
025000000105  "  I***           CPISTATO                    CPISTA
025100000105  "  I*-------------
025200000105  "  I* Ridenominazione Campi file ANCPI00F
025300000105  "  I***ANCPI02
025400000105  "  I***           CPISOGG                     CPICOD
025500000105  "  I***           CPIDTFIVL                   CPIDTF
025600000105  "  I***           CPIPARTIVA                  CPIIVA
025700000105  "  I***           CPIFRCPIVA                  CPIFPI
025800000105  "  I***           CPICDFISC                   CPICDF
025900000105  "  I***           CPIDESSOG1                  CPIDE1
026000000105  "  I***           CPIDESSOG2                  CPIDE2
026100000105  "  I***           CPIISCCIAA                  CPICIA
026200000105  "  I***           CPINATGIUR                  CPINAT
026300000105  "  I***           CPIINDRIZ                   CPIVIA
026400000105  "  I***           CPILOCALIT                  CPICIT
026500000105  "  I***           CPICAP                      CPICAP
026600000105  "  I***           CPIPROV                     CPIPRV
026700000105A0732I***           CPISTATO                    CPISTA
026800940211     I*-------------
026900950407     I* Anagrafica soggetti
027000950407     IANSOG01
027100950407     I              SOGSOGG                     SOGCOD
027200950407     I              SOGDTENTR                   SOGDAT
027300950407     I              SOGDTULMO                   SOGDAU
027400950407     I              SOGDTINVL                   SOGDAI
027500950407     I              SOGDTFIVL                   SOGDAF
027600950407     I              SOGDTNASC                   SOGDAN
027700950407     I              SOGDES                      SOGDES
027800950407     I              SOGDESAGG                   SOGDE1
027900950407     I              SOGCDFISC                   SOGCDF
028000950407     I              SOGPARTIVA                  SOGIVA
028100950407     I              SOGFRCPIVA                  SOGFPI
028200950407     I              SOGISCCIAA                  SOGCIA
028300950407     I              SOGSEX                      SOGSES
028400950407     I              SOGNATGIUR                  SOGNAT
028500950407     I              SOGCODPREC                  SOGCOP
028600950407     I              SOGABI                      SOGABI
028700950407     I              SOGCLFSOG                   SOGCLG
028800950410     I*-------------
028900950410     I* Anagrafica indirizzi
029000950410     IANIND01
029100950410     I              INDSOGG                     INDCOD
029200950410     I              INDTPIND                    INDTII
029300950410     I              INDCDIND                    INDCOI
029400950410     I              INDDTULMO                   INDDAU
029500950410     I              INDDTINVL                   INDDAI
029600950410     I              INDDTFIVL                   INDDAF
029700950410     I              INDDESFIL                   INDDES
029800950410     I              INDDESAGG                   INDDE1
029900950410     I              INDINDRIZ                   INDVIA
030000950410     I              INDLOCALIT                  INDCIT
030100950410     I              INDCAP                      INDCAP
030200950410     I              INDPROV                     INDPRV
030300950410     I              INDSTATO                    INDSTA
030400950410     I              INDCAB                      INDCAB
030500950410     I              INDLINGUA                   INDLIN
030600950410     I              INDTELEX                    INDTLX
030700950410     I              INDTELEFON                  INDTEL
030800950410     I              INDTELEFAX                  INDTLF
030900940207     I*-------------
031000940127     C*----------------------------------------------------*
031100940127     C*                MAIN LINE PROGRAM
031200940127     C*----------------------------------------------------*
031300940127     C*
031400940223     C* inizializzazione variabili
031500940223     C                   EXSR      INZVAR
031600940223     C*
031700940223     C     $FINE         DOWEQ     *OFF
031800940131     C     $GEST         CASEQ     'D1'          GESD1                          Parzializz.
031900940131     C     $GEST         CASEQ     'S1'          GESS1                          Lista
032000940131     C     $GEST         CASEQ     'D2'          GESD2                          Valori ass.
032100941027     C                   ENDCS
032200941027     C                   ENDDO
032300940325     C* fine programma
032400940325     C                   EXSR      ENDPGM
032500940225     C*
032600940325     C/EJECT
032700940325     C************************************************************
032800940325     C* FINE PROGRAMMA
032900940325     C************************************************************
033000940325     C     ENDPGM        BEGSR
033100940325     C*
033200940325     C* passaggio dati al pgm chiamante
033300950407     C                   MOVEL     WOPR          OPR057                                     ne S1
033400950407     C                   MOVEL     A057DS        KPJBU
033500940325     C*
033600940325     C* Se pgm Guida non è stato richiamato posso chiudere in LR
033700950407     C     OPZ057        IFEQ      *BLANK
033800940325     C                   SETON                                            LR
033900940325     C* altrimenti chiudo in RT
034000940325     C                   ELSE
034100940325     C                   SETON                                            RT
034200940325     C                   ENDIF
034300940616     C                   RETURN
034400940325     C*
034500940325     C                   ENDSR
034600940224     C/EJECT
034700940201     C************************************************************
034800940201     C* RICERCHE
034900940201     C************************************************************
035000940127     C     SEARCH        BEGSR
035100940127     C*
035200940127     C* determino Riga/Colonna del cursore
035300940127     C                   MOVE      NRG           R$$
035400940127     C                   MOVE      NCL           C$$
035500940127     C                   Z-ADD     RIRI          CURR
035600940127     C                   Z-ADD     COCO          CURC
035700940127     C*
035800941118   B1C                   SELECT
035900940127     C*
036000950407    >C*--------------------------------------------------------------
036100950407    >C* inserire le routine di ricerca
036200950407    >C*--------------------------------------------------------------
036300950407     C* FMTD1  - SOGGETTO
036400950407     C     H1NMFL        WHENEQ    'D1SOG'
036500950407     C                   RESET                   A040DS
036600950407     C                   MOVE      '01'          OPZ040
036700950407     C                   CLEAR                   KPJBU
036800950407     C                   MOVEL     A040DS        KPJBU
036900950407     C                   CALL      'ANA040R'
037000950407     C                   PARM                    KPJBA
037100950407     C                   MOVEL     KPJBU         A040DS
037200950407     C     ERR040        IFEQ      '0'
037300950407     C                   MOVE      COD040        WWSOG
037400950407     C                   ELSE
037500950407     C                   MOVE      *BLANK        WWSOG
037600950407     C                   ENDIF
037700950407     C*
037800950407     C     WWSOG         IFNE      *BLANK
037900950407     C                   MOVEL     WWSOG         D1SOG
038000950407     C                   ENDIF
038100950407     C* imposto pos. del cursore
038200950407     C                   Z-ADD     CURR          H1RIGA
038300950407     C                   Z-ADD     CURC          H1COLO
038400950407     C*
038500000103A0730C* FMTD1  - STATO
038600000103  "  C     H1NMFL        WHENEQ    'D1STATO'
038700000103  "  C                   RESET                   XTABDS
038800000103  "  C                   MOVE      '01'          XTAOPZ
038900000103  "  C                   MOVE      XSCSOC        XTAAZI
039000000103  "  C                   MOVE      XSCLIN        XTALIN
039100000103  "  C                   Z-ADD     CURR          XTARIG
039200000103  "  C                   Z-ADD     CURC          XTACOL
039300000103  "  C                   CLEAR                   KPJBU
039400000103  "  C                   MOVEL     XTABDS        KPJBU
039500000103  "  C                   CALL      'ANGG12R'
039600000103  "  C                   PARM                    KPJBA
039700000103  "  C                   MOVEL     KPJBU         XTABDS
039800000103  "  C     XTAERR        IFNE      '1'
039900000103  "  C                   MOVE      XTAKEY        WWSTA
040000000103  "  C                   ELSE
040100000103  "  C                   MOVE      *BLANK        WWSTA
040200000103  "  C                   ENDIF
040300000103  "  C*
040400000103  "  C     WWSTA         IFNE      *BLANK
040500000103  "  C                   MOVEL     WWSTA         D1Stato
040600000103  "  C                   ENDIF
040700000103  "  C* imposto pos. del cursore
040800000103  "  C                   Z-ADD     CURR          H1RIGA
040900000103  "  C                   Z-ADD     CURC          H1COLO
041000000103  "  C*
041100000103  "  C* FMTD1  - PROVINCIA
041200000103  "  C     H1NMFL        WHENEQ    'D1PROV'
041300000103  "  C                   RESET                   XTABDS
041400000103  "  C                   MOVE      '01'          XTAOPZ
041500000103  "  C                   MOVE      XSCSOC        XTAAZI
041600000103  "  C                   MOVE      XSCLIN        XTALIN
041700000103  "  C                   Z-ADD     CURR          XTARIG
041800000103  "  C                   Z-ADD     CURC          XTACOL
041900000103  "  C                   CLEAR                   KPJBU
042000000103  "  C                   MOVEL     XTABDS        KPJBU
042100000103  "  C                   CALL      'ANGC17R'
042200000103  "  C                   PARM                    KPJBA
042300000103  "  C                   MOVEL     KPJBU         XTABDS
042400000103  "  C     XTAERR        IFNE      '1'
042500000103  "  C                   MOVE      XTAKEY        WWPRV
042600000103  "  C                   ELSE
042700000103  "  C                   MOVE      *BLANK        WWPRV
042800000103  "  C                   ENDIF
042900000103  "  C*
043000000103  "  C     WWPRV         IFNE      *BLANK
043100000103  "  C                   MOVEL     WWPRV         D1Prov
043200000103  "  C                   ENDIF
043300000103  "  C* imposto pos. del cursore
043400000103  "  C                   Z-ADD     CURR          H1RIGA
043500000103A0730C                   Z-ADD     CURC          H1COLO
043600940223     C*
043700940223     C* ricerca non abilitata in questa posizione
043800941118   X1C                   OTHER
043900940929     C* imposto pos. del cursore
044000940929     C                   Z-ADD     CURR          H1RIGA
044100940929     C                   Z-ADD     CURC          H1COLO
044200940929     C                   SETON                                        9899
044300940127     C*
044400941118   E1C                   ENDSL
044500940127     C*
044600940506     C                   MOVE      *HIVAL        $LASTV
044700940127     C                   ENDSR
044800940131     C/EJECT
044900940224     C************************************************************
045000940131     C* RIEMPIMENTO OPZIONI
045100940224     C************************************************************
045200940131     C     RIEOPZ        BEGSR
045300940223     C*
045400940223     C* Input :
045500940223     C* - $TX = schiera con tutti i testi disponibili
045600940223     C* - $FL = schiere con i flag di validità associati ai testi
045700940225     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
045800940223     C*          opzione usato
045900940223     C* - WMXO = variabile contenente il numero massimo di testi
046000940223     C*          opzione usabili
046100940721     C* - $L01 = Lunghezza prima riga
046200940721     C* - $L02 = Lunghezza seconda riga
046300940223     C* Output :
046400940223     C* - WOPZ1 = variabile contenente la prima riga opzioni
046500940223     C* - WOPZ2 = variabile contenente la seconda riga opzioni
046600941214     C* - FF23  = flag che indica se c'è bisogno del tasto F23
046700940223     C*
046800941214     C                   CALL      'X04OPMSG'
046900940223     C* input
047000940223     C                   PARM                    $TX
047100940223     C                   PARM                    $FL
047200940225     C                   PARM                    WCOZ              3 0
047300940223     C                   PARM                    WMXO              3 0
047400940223     C* output
047500940223     C                   PARM                    WOPZ1            76
047600940223     C                   PARM                    WOPZ2            76
047700941214     C                   PARM                    FF23              1
047800950301     C* input
047900940721    >C                   PARM                    $L01              3 0
048000940721    >C                   PARM                    $L02              3 0
048100940223     C*
048200060224A1148C                   PARM                    $ALLOPT         960
048300060224A1148C*
048400940131     C                   ENDSR
048500940506     C/EJECT
048600940506     C************************************************************
048700940506     C* RIEMPIMENTO TASTI FUNZIONE
048800940506     C************************************************************
048900940506     C     RIEKEY        BEGSR
049000940506     C*
049100940506     C* Input :
049200940506     C* - $FX = schiera con tutti i testi disponibili
049300940506     C* - $FC = schiere con i flag di validità associati ai testi
049400940506     C* - $FR = schiere con i flag di REVERSE IMAGE
049500940506     C* - $ULK = ultimo testo caricato
049600940721     C* - $L01 = Lunghezza prima riga
049700940721     C* - $L02 = Lunghezza seconda riga
049800940506     C* Output :
049900940506     C* - $ULK
050000940609     C* - $KEY1= 1° riga di tasti funzione
050100940609     C* - $KEY2= 2° riga di tasti funzione
050200940506     C*
050300940506     C                   CALL      'XKEYMSG'
050400940506     C* input
050500940506     C                   PARM                    $FX
050600940506     C                   PARM                    $FC
050700940506     C                   PARM                    $FR
050800940506     C                   PARM                    $ULK              3 0
050900940506     C* output
051000940506     C                   PARM                    $KEY1            79
051100940506     C                   PARM                    $KEY2            79
051200950301     C* input
051300940721    >C                   PARM                    $L01              3 0
051400940721    >C                   PARM                    $L02              3 0
051500940506     C*
051600060224A1148C                   PARM                    $ALLFUNCT       480
051700060224A1148C*
051800940506     C                   ENDSR
051900940224     C/EJECT
052000940224     C************************************************************
052100940224     C* GESTIONE EMISSIONE TASTI FUNZIONE
052200940224     C************************************************************
052300940224     C     GESPIE        BEGSR
052400940224     C*
052500940224     C                   SELECT
052600940224     C* Parzializzazioni
052700940224     C     $GEST         WHENEQ    'D1'
052800940506     C     $ULKD1        IFEQ      0
052900940506     C* riempimento opzioni
053000940506     C                   EXSR      F24D1
053100940506     C                   ELSE
053200940506     C                   WRITE     FMTZ1
053300940506     C                   ENDIF
053400940506     C* Lista
053500940506     C     $GEST         WHENEQ    'S1'
053600940506     C     $ULKS1        IFEQ      0
053700940506     C                   EXSR      F24S1
053800940506     C                   ELSE
053900940506     C                   WRITE     FMTZ2
054000940506     C                   ENDIF
054100940916     C* Modifica valori assunti
054200940916     C     $GEST         WHENEQ    'D2'
054300940916     C     $ULKD2        IFEQ      0
054400940916     C                   EXSR      F24D2
054500940916     C                   ELSE
054600940916     C                   WRITE     FMTZ3
054700940916     C                   ENDIF
054800940506     C                   ENDSL
054900940506     C*
055000940224     C                   ENDSR
055100940224     C/EJECT
055200940224     C************************************************************
055300940224     C* GESTIONE PARZIALIZZAZIONI
055400940224     C************************************************************
055500940224     C     GESD1         BEGSR
055600940224     C* inizializzazione videata
055700940223     C     $INZD1        IFEQ      *ON
055800940127     C                   EXSR      INZD1
055900940223     C                   MOVE      *OFF          $INZD1
056000941027     C                   ENDIF
056100940223     C* emissione piede videata se proveniente da altra
056200940223     C* salvataggio valore formato di provenienza
056300940202     C     $LASTV        IFNE      'D1'
056400940223     C* salvo stato F17 per sapere cosa fare in caso di F12
056500940223     C                   MOVE      *INKR         WINKR
056600060308A1150C                   IF        opz057=*blanks
056700060308  "  c                   seton                                        17
056800060308A1150c                   endif
056900940202     C                   WRITE     FMTT1
057000940224     C                   EXSR      GESPIE
057100940202     C                   MOVE      $LASTV        $LASTG
057200940202     C                   MOVE      'D1'          $LASTV
057300940707     C                   ENDIF
057400940707     C*
057500940707     C* se esistono errori sulla videata
057600940707     C* emetto la write del formato a indicatori spenti per vedere
057700940707     C* le eventuali decodifiche
057800940707     C     *IN99         IFEQ      *ON
057900940707     C                   MOVEA     *IN           WIN
058000940707     C                   MOVE      *ALL'0'       IN5098           49
058100940707     C                   MOVEA     IN5098        *IN(50)
058200940707     C                   WRITE     FMTD1
058300940707     C                   MOVEA     WIN           *IN
058400940707     C                   ENDIF
058500940131     C*
058600940131     C                   EXFMT     FMTD1
058700940506     C*
058800941205     C* controllo abilitazione tasti funzione
058900940506     C                   EXSR      CTRKD1
059000940506     C*
059100940506     C     *IN99         IFEQ      *OFF
059200940131     C*
059300940117     C                   SELECT
059400940916     C* F2=Decodifica
059500940916     C     $TASTO        WHENEQ    F02
059600940916     C                   EXSR      F02D1
059700940131     C* F3=Fine
059800940506     C     $TASTO        WHENEQ    F03
059900940307     C                   EXSR      F03D1
060000940131     C* F4=Ricerca
060100940506     C     $TASTO        WHENEQ    F04
060200940127     C                   EXSR      SEARCH
060300940916     C* F5=Refresh
060400940916     C     $TASTO        WHENEQ    F05
060500940916     C                   EXSR      F05D1
060600941206     C* F8=Successivo
060700941206     C     $TASTO        WHENEQ    F08
060800941206     C                   EXSR      F08D1
060900950301     C* F10=Immissione
061000950301     C     $TASTO        WHENEQ    F10
061100950301     C                   EXSR      F10D1
061200940131     C* F12=Ritorno
061300940506     C     $TASTO        WHENEQ    F12
061400940307     C                   EXSR      F12D1
061500940131     C* F18=Modifica valori assunti
061600940506     C     $TASTO        WHENEQ    F18
061700940307     C                   EXSR      F18D1
061800940224     C* F24=Altri tasti
061900940506     C     $TASTO        WHENEQ    F24
062000940309     C                   EXSR      F24D1
062100940131     C*
062200940117     C                   OTHER
062300940127     C                   EXSR      CTRD1
062400940127     C     *IN99         IFEQ      *OFF
062500940131     C* definisco l'ordinamento
062600940131     C                   SELECT
062700950407     C*  nel caso di più viste logiche e quindi più campi di
062800950407     C*  parzializzazione, qui vanno definiti i criteri per
062900950407     C*  determinare quale VL va letta, a seconda di quali campi
063000950407     C*  sono stati valorizzati
063100950407     C     D1SOG         WHENNE    *BLANK
063200940131     C                   Z-ADD     1             $ORD
063300950410     C     D1DTF         WHENNE    *ZERO
063400940131     C                   Z-ADD     2             $ORD
063500941104     C* imposto l'ordinamento 99 per ottenere in selezione solo il
063600941104     C* rcd con la chiave passata
063700941214     C     $UNIQ         WHENEQ    *ON
063800940831     C                   Z-ADD     99            $ORD
063900950314     C* nel caso non sia stato digitato imposta ordinamento di default
064000950314     C                   OTHER
064100940131     C                   ENDSL
064200940127     C                   MOVE      'S1'          $GEST
064300940223     C                   MOVE      *ON           $INZS1
064400000103A0730C***                MOVE      *BLANK        W1POS
064500000103A0730C                   MOVE      *ZEROS        W1POS
064600940211     C                   ENDIF
064700940224     C*
064800940117     C                   ENDSL
064900940224     C*
065000940506     C                   ENDIF
065100940117     C                   ENDSR
065200940224     C/EJECT
065300940224     C************************************************************
065400940224     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
065500940224     C************************************************************
065600940127     C     INZD1         BEGSR
065700940318     C*
065800940318     C                   CLEAR                   FMTD1
065900941013     C* Se pgm  richiamato con parametri forniti , li imposto nelle
066000941013     C*    parzializzazioni
066100941104     C                   SELECT
066200950414     C*
066300941213     C* il pgm è richiamato con chiave univoca per gestione POSTA
066400950414     C*          $UNIQ     WHEQ *ON
066500950414     C*                    MOVE UQ1057    D1SOG
066600950414     C*-- UQ2057 = DATA LUNGA 10
066700950414     C***-- CONVERSIONE DATA DA 10 A 6 :
066800950414     C*          UQ2057    IFGT *ZEROS
066900950414     C*                    CALL 'XDT4000'
067000950414     C*                    PARM UQ2057    XDTAMG 10
067100950414     C*                    PARM           XDTGMA  60
067200950414     C*                    PARM 2         XDTSTA  10
067300950414     C*                    MOVELXDTGMA    D1DTF
067400950414     C*                    ELSE
067500950414     C*                    Z-ADD0         D1DTF
067600950414     C*                    END
067700941104     C* il pgm è richiamato e
067800941104     C* sono stati passati dei valori di parzializzazione ma non
067900941104     C* la chiave univoca
068000950407     C     OPZ057        WHENNE    *BLANK
068100950403     C* imposto i valori passati sui campi a video
068200950407     C                   MOVE      SOG057        D1SOG
068300950410     C*-- DTF057 = DATA LUNGA 10
068400950410     C***-- CONVERSIONE DATA DA 10 A 6 :
068500950410     C     DTF057        IFGT      *ZEROS
068600000110A0732C     SavDtf        andne     $Dt9
068700950410     C                   CALL      'XDT4000'
068800950410     C                   PARM      DTF057        XDTAMG           10
068900950410     C                   PARM                    XDTGMA            6 0
069000950410     C                   PARM      2             XDTSTA            1 0
069100950410     C                   MOVEL     XDTGMA        D1DTF
069200950410     C                   ELSE
069300950410     C                   Z-ADD     0             D1DTF
069400950410     C                   END
069500941104     C* il pgm non è richiamato
069600950407     C     OPZ057        WHENEQ    *BLANK
069700950403     C* imposto dei valori di default
069800950407     C                   MOVE      *BLANK        D1SOG
069900000114A0735C*****              Z-ADD     UDATE         D1DTF
070000000114A0735C                   Z-ADD     ZDate         D1DTF
070100941115     C                   ENDSL
070200940224     C*
070300940117     C                   ENDSR
070400940506     C/EJECT
070500940506     C************************************************************
070600940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
070700940609     C* INPUT
070800941205     C* $FC1 = Abilitazioni videata D1
070900940509     C* OUTPUT
071000940509     C* *IN97 *IN99 Errori
071100940506     C************************************************************
071200940506     C     CTRKD1        BEGSR
071300941205     C*
071400940506     C                   SETOFF                                       99
071500940506     C                   MOVEA     $FC1          $FC
071600940506     C                   EXSR      CTRKEY
071700940506     C     *IN99         IFEQ      *ON
071800940506     C                   SETON                                        97
071900940506     C                   ENDIF
072000941205     C*
072100940506     C                   ENDSR
072200940916     C/EJECT
072300940916     C************************************************************
072400940916     C* GESTIONE F02 VIDEO D1
072500940916     C************************************************************
072600940916     C     F02D1         BEGSR
072700940916     C*
072800940916     C* per decodificare la videata richiamo la routine dei ctrl
072900940916     C* e poi disattivo gli eventuali messaggi di errore
073000940916     C                   EXSR      CTRD1
073100940916     C*
073200940916     C                   MOVE      *ALL'0'       IN5098
073300940916     C                   MOVEA     IN5098        *IN(50)
073400940916     C*
073500940916     C                   ENDSR
073600940224     C/EJECT
073700940131     C************************************************************
073800940307     C* GESTIONE F03 VIDEO D1
073900940131     C************************************************************
074000940307     C     F03D1         BEGSR
074100940131     C*
074200940307     C                   MOVE      *ON           $FINE
074300950407    >C                   MOVE      '1'           RET057
074400940325     C* fine programma
074500940325     C                   EXSR      ENDPGM
074600940307     C*
074700940307     C                   ENDSR
074800940916     C/EJECT
074900940916     C************************************************************
075000940916     C* GESTIONE F05 VIDEO D1
075100940916     C************************************************************
075200940916     C     F05D1         BEGSR
075300940916     C*
075400940916     C                   MOVE      *ON           $INZD1
075500940916     C*
075600940916     C                   ENDSR
075700941206     C/EJECT
075800941206     C************************************************************
075900941206     C* GESTIONE F08 VIDEO D1
076000941206     C************************************************************
076100941206     C     F08D1         BEGSR
076200941206     C*
076300941206     C                   MOVE      *ON           $FINE
076400950407    >C                   MOVE      '0'           RET057
076500941206     C*
076600941206     C                   ENDSR
076700950301     C/EJECT
076800950301     C************************************************************
076900950301     C* GESTIONE F10 VIDEO D1
077000950301     C************************************************************
077100950301     C     F10D1         BEGSR
077200950301     C*
077300950407    >C                   RESET                   A058DS
077400950407    >C                   MOVEL     '01'          OPZ058
077500950410    >C*          CMT057    IFEQ '1'
077600950410    >C*                    MOVEL'1'       CMT058
077700950410     C*                    ELSE
077800950410    >C*                    MOVEL'0'       CMT058
077900950410     C*                    ENDIF
078000950301     C* passare anche le parzializzazioni ricevute relative alla chiave
078100950301     C* univoca : se ce ne sono saranno usate come default d'immissione
078200950410     C                   MOVEL     SOG057        SOG058
078300950410     C                   MOVEL     DTF057        DTF058
078400950403     C* codice di riferimento per le valorizzazioni di default
078500950410    >C***                  MOVE $DEF      DF1058
078600950407    >C                   MOVEL     A058DS        KPJBU
078700950407    >C                   CALL      'ANA058R'
078800950301     C                   PARM                    KPJBA
078900950407    >C                   MOVEL     KPJBU         A058DS
079000950301     C* ritorno da PGM gestione
079100950407    >C                   EXSR      RTN058
079200950301     C*
079300950301     C                   MOVE      *HIVAL        $LASTV
079400950301     C*
079500950301     C                   ENDSR
079600940307     C/EJECT
079700940307     C************************************************************
079800940307     C* GESTIONE F12 VIDEO D1
079900940307     C************************************************************
080000940307     C     F12D1         BEGSR
080100940307     C*
080200940307     C*  se la 1a videata è la lista oppure sono arrivato qui con F17,
080300940307     C*  per F12 torno alla lista
080400940307     C     $LOP          IFEQ      2
080500940307     C     WINKR         OREQ      *ON
080600940307     C                   MOVE      'S1'          $GEST
080700940307     C*  altrimenti, per F12 torno a menu
080800940307     C                   ELSE
080900940307     C                   MOVE      *ON           $FINE
081000950407    >C                   MOVE      '2'           RET057
081100941027     C                   ENDIF
081200940307     C*
081300940307     C                   ENDSR
081400940307     C/EJECT
081500940307     C************************************************************
081600940307     C* GESTIONE F18 VIDEO D1
081700940307     C************************************************************
081800940307     C     F18D1         BEGSR
081900940307     C*
082000940307     C                   MOVE      'D2'          $GEST
082100940307     C                   MOVEL     *ON           $INZD2
082200940307     C*
082300940307     C                   ENDSR
082400940307     C/EJECT
082500940307     C************************************************************
082600940307     C* GESTIONE F24 VIDEO D1
082700940609     C* input
082800940509     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
082900940509     C* $FX1 =Testi tasti funzione
083000940509     C* $FC1 = Abilitazioni
083100940509     C* $FR1 = Inversioni di fondo
083200940307     C************************************************************
083300940307     C     F24D1         BEGSR
083400940506     C*
083500940506     C                   MOVEA     $FX1          $FX
083600940506     C                   MOVEA     $FC1          $FC
083700940506     C                   MOVEA     $FR1          $FR
083800940506     C                   Z-ADD     $ULKD1        $ULK
083900941220    >C                   Z-ADD     79            $L01
084000941220    >C                   Z-ADD     79            $L02
084100940506     C                   EXSR      RIEKEY
084200940506     C                   Z-ADD     $ULK          $ULKD1            3 0
084300940720     C                   MOVEA     $FC           $FC1
084400940506     C                   MOVEL     $KEY1         Z1KE1
084500060224A1148C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
084600940506     C                   MOVEL     $KEY2         Z1KE2
084700940506     C                   WRITE     FMTZ1
084800940506     C*
084900940307     C*
085000940307     C                   ENDSR
085100940307     C/EJECT
085200940307     C************************************************************
085300940307     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
085400940307     C************************************************************
085500940307     C     CTRD1         BEGSR
085600940307     C*
085700940127     C                   SETOFF                                       99
085800940127     C* non imposto pos. del cursore
085900940127     C                   Z-ADD     0             H1RIGA
086000940127     C                   Z-ADD     0             H1COLO
086100940131     C*
086200950407    >C*  elencare gli eventuali controlli sui campi di
086300950407    >C*  parzializzazione, ricordando che quasi sempre essi
086400950407    >C*  non ne avranno perchè determinano solo il range di
086500950407    >C*  scrittura della lista e quindi (nel caso di campi
086600950407    >C*  alfanumerici) saranno trattati come chiavi parziali
086700950407     C*
086800950407     C* Allineamento dei codice soggetto
086900950407     C*
087000950407     C     D1SOG         IFNE      *BLANK
087100950407     C                   CALL      'XALLINEA'
087200950407     C                   PARM                    D1SOG
087300950407     C                   PARM      8             LEN               5 0
087400950407     C                   PARM                    XSCINP
087500950407     C                   ENDIF
087600000110A0732 * Spengo l'indicatore perchè, se non vengono trovati errori,
087700000110  "   * il formato non viene riemesso, ma si va direttamente nel
087800000110  "   * pgm di gestione, quindi gli indicatori non si spengono
087900000110  "   * automaticamente
088000000119A0732C                   setoff                                       55
088100000119     C* giro Data :
088200000110A0732C                   if        D1Dtf <> *zeros
088300950411     C                   CALL      'XDT4000'
088400950411     C                   PARM                    XDTAMG           10
088500950411     C                   PARM      D1DTF         XDTGMA            6 0
088600950412     C                   PARM      3             XDTSTA            1 0
088700950411     C                   MOVEL     XDTAMG        SAVDTF
088800950412     C     XDTSTA        IFNE      0
088900000110A0732C***                SETON                                        4199
089000000110A0732C                   SETON                                        5599
089100950412     C                   END
089200000105A0732C                   else
089300000105  "  C                   movel     $dt9          SavDtf
089400000105A0732C                   endif
089500950411     C*
089600950410     C*
089700940117     C                   ENDSR
089800940224     C/EJECT
089900940127     C************************************************************
090000940131     C* GESTIONE LISTA
090100940127     C************************************************************
090200940127     C     GESS1         BEGSR
090300940223     C* inizializzazione videata
090400940223     C     $INZS1        IFEQ      *ON
090500940127     C                   EXSR      INZS1
090600940223     C                   MOVE      *OFF          $INZS1
090700940127     C                   ENDIF
090800940223     C* emissione piede videata se proveniente da altra
090900940223     C* salvataggio valore formato di provenienza
091000940202     C     $LASTV        IFNE      'S1'
091100060308A1150C                   IF        opz057=*blanks
091200060308  "  c                   seton                                        17
091300060308A1150c                   endif
091400940202     C                   WRITE     FMTT1
091500940224     C                   EXSR      GESPIE
091600940202     C                   MOVE      $LASTV        $LASTG
091700940202     C                   MOVE      'S1'          $LASTV
091800941027     C                   ENDIF
091900940223     C*
092000940223     C     WMAX          IFEQ      0
092100940211     C                   WRITE     FMTD3                                        NO RCD
092200940211     C                   ENDIF
092300941026     C*Serve per emissione finestra pgm invio anA727r (per open/close
092400940831     C*window)
092500950414     C*                    MOVE '1'       WIN727  1
092600940127     C*
092700940131     C                   EXFMT     FMTC1
092800940204     C     C1NRR         IFNE      0
092900940204     C                   Z-ADD     C1NRR         WSFL
093000940204     C                   ENDIF
093100940127     C                   Z-ADD     SFLNRR        C1RCD
093200940506     C*
093300940506     C* controllo abilitazione tasti funzione
093400940506     C                   EXSR      CTRKS1
093500941205     C*
093600940506     C     *IN99         IFEQ      *OFF
093700940127     C*
093800941027     C                   SELECT
093900940127     C* F3=Fine
094000940506     C     $TASTO        WHENEQ    F03
094100940309     C                   EXSR      F03S1
094200941206     C* F8=Successivo
094300941206     C     $TASTO        WHENEQ    F08
094400941206     C                   EXSR      F08S1
094500940131     C* F10=Immissione
094600940506     C     $TASTO        WHENEQ    F10
094700940309     C                   EXSR      F10S1
094800940131     C* F12=Ritorno
094900940506     C     $TASTO        WHENEQ    F12
095000940309     C                   EXSR      F12S1
095100940131     C* F17=Parzializzazioni
095200940506     C     $TASTO        WHENEQ    F17
095300940309     C                   EXSR      F17S1
095400940131     C* F18=Modifica valori assunti
095500940506     C     $TASTO        WHENEQ    F18
095600940309     C                   EXSR      F18S1
095700940224     C* F23=Altre opzioni
095800940506     C     $TASTO        WHENEQ    F23
095900940309     C                   EXSR      F23S1
096000940224     C* F24=Altri tasti
096100940506     C     $TASTO        WHENEQ    F24
096200940309     C                   EXSR      F24S1
096300940127     C* ROLL-UP
096400940506     C     $TASTO        WHENEQ    ROLLUP
096500940127     C                   EXSR      ROLS1
096600940127     C*
096700941027     C                   OTHER
096800940127     C* CONTROLLO DATI
096900940131     C                   EXSR      CTRC1
097000940201     C     *IN99         IFEQ      *OFF
097100940131     C                   EXSR      CTRS1
097200941027     C                   ENDIF
097300941205     C*
097400941027     C                   ENDSL
097500940127     C*
097600940506     C                   ENDIF
097700941205     C*
097800940127     C                   ENDSR
097900940224     C/EJECT
098000940127     C************************************************************
098100940131     C* INIZIALIZZAZIONE LISTA
098200940127     C************************************************************
098300940127     C     INZS1         BEGSR
098400940302     C* pulizia SFL
098500940128     C                   SETOFF                                         3031
098600940131     C                   WRITE     FMTC1
098700940128     C                   SETON                                          31
098800950411     C* Reperisco dati anagrafici Soggetto :
098900950411     C                   MOVEL     D1SOG         SOGCOD
099000950411     C     SOGCOD        CHAIN     ANSOG01L                           21
099100950411     C     *IN21         IFEQ      '0'
099200950411     C                   MOVEL     SOGCOD        C1SOG
099300950411     C                   MOVEL     SOGDES        C1SOGD
099400950411     C                   END
099500950411     C                   MOVEL     D1SOG         INDCOD
099600950411     C     INDCOD        CHAIN     ANIND01L                           21
099700950411     C     *IN21         IFEQ      '0'
099800950411     C                   MOVEL     INDVIA        C1VIA
099900950411     C                   MOVEL     INDCIT        C1CIT
100000950411     C                   MOVEL     INDCAP        C1CAP
100100950411     C                   MOVEL     INDPRV        C1PRV
100200950411     C                   END
100300940127     C*
100400940128     C* riempimento opzioni
100500940223     C                   MOVEA     $TX1          $TX
100600940223     C                   MOVEA     $FL1          $FL
100700940309     C                   Z-ADD     0             WCOZ
100800940225     C*  equivale al num.di elementi della schiera $TX1
100900940509     C                   Z-ADD     $DIMOP        WMXO
101000941220    >C                   Z-ADD     76            $L01
101100941220    >C                   Z-ADD     76            $L02
101200940128     C                   EXSR      RIEOPZ
101300941214     C                   Z-ADD     23            X
101400941214     C                   MOVE      FF23          $FC2(X)
101500940608     C                   MOVEL     WOPZ1         C1OPZ1
101600060224A1148C                   MOVEL     $ALLOPT       H1ALLOPT
101700940608     C                   MOVEL     WOPZ2         C1OPZ2
101800940225     C                   Z-ADD     WCOZ          C1COZ
101900940128     C*
102000940128     C* CARICAMENTO SFL A PAGINE
102100940128     C*
102200940201     C                   Z-ADD     0             S1NRR
102300940128     C                   Z-ADD     0             WMAX
102400940224     C*
102500940224     C* Posizionamento su file pilota
102600940302     C                   CLEAR                   C1POS
102700000105A0732C***                EXSR      SETANA
102800940302     C* ripristino i valori del posizionamento
102900940302     C                   MOVE      W1POS         C1POS
103000950419     C**********           MOVE W1DTF     C1DTF
103100940131     C*
103200940201     C* Lettura file pilota
103300000105A0732 * Richiedo primo record
103400000104  "  C                   clear                   Ana057ds1
103500000105A0732C                   eval      Prima0571 = *On
103600941214     C                   EXSR      REDANA
103700940131     C*
103800940131     C* Carico 1a pagina
103900940127     C                   EXSR      ROLS1
104000940201     C                   Z-ADD     SFLPAG        WPAG
104100940127     C*
104200940127     C                   ENDSR
104300940127     C/EJECT
104400940127     C************************************************************
104500940131     C* CARICAMENTO PAGINA LISTA
104600940127     C************************************************************
104700940127     C     ROLS1         BEGSR
104800940127     C*
104900940128     C                   SETOFF                                       32
105000940223     C                   Z-ADD     0             Y
105100940127     C                   Z-ADD     WMAX          S1NRR
105200940127     C*
105300940127     C* Leggo dal file anagrafico per caricare la lista
105400940127     C*
105500941027     C     $EFILE        DOWEQ     *OFF
105600940201     C     Y             ANDLT     WPAG
105700940127     C*
105800940131     C                   EXSR      RIES1
105900950410     C*
106000950410     C                   CALL      'XDT4000'
106100000105A0732C***                PARM      CPIDTF        XDTAMG           10
106200000105A0732C                   PARM      CPIDTFIVL     XDTAMG           10
106300950410     C                   PARM                    XDTGMA            6 0
106400950410     C                   PARM      2             XDTSTA            1 0
106500950410     C                   MOVEL     XDTGMA        S1DTF
106600000105A0732C***                MOVEL     CPIDTF        H1DTF
106700000105A0732C                   MOVEL     CPIDTFIVL     H1DTF
106800950410     C*
106900000105A0732C***                MOVEL     CPIIVA        S1IVA
107000000105  "  C***                MOVEL     CPICDF        S1CDF
107100000105  "  C***                MOVEL     CPIIVA        S1IVA
107200000105  "  C                   MOVEL     CPIPARTIVA    S1IVA
107300000105A0732C                   MOVEL     CPICDFISC     S1CDF
107400950410     C*
107500940127     C                   ADD       1             S1NRR
107600940127     C                   ADD       1             Y
107700940127     C*
107800940131     C                   WRITE     FMTS1
107900940131     C*
108000000105A0732 * Richiedo i record successivi al primo
108100000104  "  C                   clear                   Ana057ds1
108200000105A0732C                   eval      Prima0571 = *Off
108300941214     C                   EXSR      REDANA
108400940128     C*
108500941027     C                   ENDDO
108600940127     C*
108700940223     C                   Z-ADD     S1NRR         WMAX                 30
108800940127     C*
108900940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
109000940127     C*
109100940201     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
109200940127     C                   MVR                     RESTO             3 0
109300940131     C     PAGINE        MULT      SFLPAG        C1RCD
109400941027     C     RESTO         IFGT      0
109500940127     C                   ADD       1             C1RCD
109600941027     C                   ELSE
109700940407     C                   SUB       SFLPAG        C1RCD
109800940407     C                   ADD       1             C1RCD
109900941027     C                   ENDIF
110000940128     C*
110100940127     C                   ENDSR
110200940224     C/EJECT
110300000105A0732C************************************************************
110400000104  "  C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
110500000104  "  C************************************************************
110600000104  "  C***  SETANA        BEGSR
110700000104  "  C*
110800000104  "  C***                SELECT
110900000104  "  C* Gestione posizionamento
111000000104  "  C* Se il campo W1POS è > *blank imposto il posizionamento con
111100000104  "  C* W1POS altrimenti lo imposto con il valore di parametrizzazione
111200000104  "  C*
111300000104  "  C* Ord.1 = SOG/DTF (descend)
111400000110A0732C***  $ORD          WHENEQ    1
111500000110A0730C***  W1POS         IFNE      *BLANK
111600000110A0730C***                MOVEL     W1POS         CPICOD
111700000110A0732C***EX-A0730  W1POS         IFGT      *ZEROS
111800000110  "  C***    "           MOVEL     W1POS         W1DTF
111900000110  "  C***    "           CALL      'XDT4000'
112000000110  "  C***    "           PARM                    XDTAMG           10
112100000110  "  C***    "           PARM      W1DTF         XDTGMA            6 0
112200000110  "  C***    "           PARM      4             XDTSTA            1 0
112300000110  " >C***    "           MOVEL     D1SOG         CPICOD
112400000110  "  C*** EX-A0730       MOVEL     XDTAMG        CPIDTF
112500000110  "  C***                ELSE
112600000104  " >C***                MOVEL     D1SOG         CPICOD
112700000104  " >C***                MOVEL     SAVDTF        CPIDTF
112800000104  "  C***                ENDIF
112900000104  " >C***  K2CPI1        SETLL     ANCPI01
113000000104  "  C*
113100000104  " >C* Ord.2 = DTF (descend)/SOG
113200000104  "  C***  $ORD          WHENEQ    2
113300000104  "  C***  W1POS         IFGT      *ZEROS
113400000104  "  C***                MOVEL     W1POS         W1DTF
113500000104  "  C***                CALL      'XDT4000'
113600000104  "  C***                PARM                    XDTAMG           10
113700000104  "  C***                PARM      W1DTF         XDTGMA            6 0
113800000104  "  C***                PARM      4             XDTSTA            1 0
113900000104  "  C***                MOVEL     XDTAMG        CPIDTF
114000000104  "  C***                ELSE
114100000104  " >C***                MOVEL     SAVDTF        CPIDTF
114200000104  " >C***                MOVEL     D1SOG         CPICOD
114300000104  "  C***                ENDIF
114400000104  " >C***  K2CPI2        SETLL     ANCPI02
114500000104  "  C* Ord.99= Chiave univoca
114600000104  "  C* (quando viene gestita la chiave univoca NON deve essere gestito
114700000104  "  C*  il posizionamento C1POS/W1POS)
114800000104  "  C***  $ORD          WHENEQ    99
114900000104  " >C***                MOVEL     D1SOG         CPICOD
115000000104  " >C***                MOVEL     SAVDTF        CPIDTF
115100000104  " >C***  K2CPI1        SETLL     ANCPI01
115200000104  "  C***                ENDSL
115300000104  "  C*
115400000104  "  C***                ENDSR
115500000105A0732C/EJECT
115600000104     C************************************************************
115700000104     C* LETTURA RCD ARCHIVIO PILOTA
115800000104     C************************************************************
115900000104     C     REDANA        BEGSR
116000000104     C*
116100000105A0732 * La lettura "classica" della template (setana,redana,selana)
116200000105A0732 * è sostituita dal richiamo al pgm sqlrpgle ANA057R1
116300000104     C                   MOVEL     *OFF          $EFILE
116400000105A0732C***                MOVEL     *OFF          $RCDOK
116500000104     C*
116600000105A0732C                   eval      Sog0571 = D1Sog
116700000105  "  C                   eval      Stato0571 = D1Stato
116800000105  "  C                   eval      Prov0571 = D1Prov
116900000105  "  C                   eval      PIva0571 = D1PartIVA
117000000105  "  C                   eval      CdFisc0571 = D1CdFisc
117100000105  "   * valorizzo la data gestendo il posizionamento
117200000105  "  C                   if        W1Pos <> *zeros
117300000105  "  C                   CALL      'XDT4000'
117400000105  "  C     DtFivl0571    PARM                    XDTAMG           10
117500000105  "  C                   PARM      W1Pos         XDTGMA            6 0
117600000105  "  C                   PARM      2             XDTSTA            1 0
117700000105  "  C                   else
117800000105  "  C                   movel     SavDtf        DtFivl0571
117900060308A0732C                   endif
118000060308A1150C                   eval      DesSog1571 = D1DesSog1
118100060308A0732 *
118200000104  "  C                   movel(p)  ana057ds1     kpjbu
118300000105  "  C                   clear                   ancpi00f
118400000104  "   *
118500000104  "  C                   call      'ANA057R1'
118600000104  "  C                   parm                    kpjba
118700000105  "  C                   parm                    ancpi00f
118800000104  "   *
118900000104  "  C                   movel     kpjbu         ana057ds1
119000000110  "  C                   eval      *in21 = Rec0571
119100000104  "  C                   MOVE      *IN21         $EFILE
119200000104  "  C*
119300000104  "  C                   MOVE      *IN21         *IN33                          SflEnd
119400000104  "  C*
119500000104  "  C***                SELECT
119600000104  "  C***  $ORD          WHENEQ    1
119700000104  " >C***                READ      ANCPI01                                21
119800000104  "  C***  $ORD          WHENEQ    2
119900000104  " >C***                READ      ANCPI02                                21
120000000104  "  C***  $ORD          WHENEQ    99
120100000104  " >C***                READ      ANCPI01                                21
120200000104  "  C***                ENDSL
120300000104  "  C*
120400000104  "  C***                MOVE      *IN21         $EFILE
120500000104  "  C*
120600000104  "  C***  $EFILE        IFEQ      *OFF
120700000104  "  C***                EXSR      SELANA
120800000104  "  C***                ENDIF
120900000104  "  C*
121000000104  "  C***                ENDDO
121100000104  "  C*
121200000105A0732C***                MOVE      *IN21         *IN33                          SflEnd
121300000104     C*
121400000104     C                   ENDSR
121500000104     C/EJECT
121600940224     C************************************************************
121700940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
121800940224     C************************************************************
121900940224     C     CLCPAG        BEGSR
122000940224     C* Input :
122100940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
122200940224     C*          cursore
122300940224     C* - SFLPAG = numero rcd per pagina sfl
122400940224     C* Output :
122500940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
122600940224     C*
122700940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
122800940224     C                   MVR                     RESTO             3 0
122900940224     C     RESTO         IFGT      0
123000940224     C                   ADD       1             PAGINE
123100940224     C                   ENDIF
123200940224     C     PAGINE        MULT      SFLPAG        WPAG
123300940224     C*
123400940224     C                   ENDSR
123500940131     C/EJECT
123600000105A0732C************************************************************
123700000104  "  C* SELEZIONI SULLA RIGA LETTA
123800000104  "  C* impostare a *OFF il campo $RCDOK per scartare un record che
123900000104  "  C* non rientra nei parametri di parzializzazione
124000000104  "  C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
124100000104  "  C* superiore stabilito nei parametri
124200000104  "  C*
124300000104  "  C************************************************************
124400000104  "  C***  SELANA        BEGSR
124500000104  "  C*
124600000104  "  C***                MOVEL     *ON           $RCDOK
124700000104  "  C***                MOVEL     *ALL'9'       ALL9             15
124800000104  "  C* creo limite massimo parzializzazioni :
124900000104  "  C* per ogni valore alfanumerico creo il limite superiore
125000000104  "  C* per cui caricare, creando una variabile che contiene
125100000104  "  C* il valore digitato nel campo a video riempito di "9"
125200000104  " >C***  D1SOG         CAT       ALL9:0        SOG999
125300000110  "  C***EX-A0730  D1Stato       CAT       ALL9:0        Sta999
125400000110  " >C***  "  D1Prov        CAT       ALL9:0        Prv999
125500000110  " >C***  "  D1PartIVA     CAT       ALL9:0        Iva999
125600000110  "  C***EX-A0730  D1CdFisc      CAT       ALL9:0        Cdf999
125700000104  "  C*
125800000104  "  C*
125900000104  "  C***                SELECT
126000000104  "  C*
126100000104  "  C* SELEZIONI RISPETTO AI VALORI RCD
126200000104  "  C*
126300000104  "  C* Scartare i record che non rispondono alle selezioni effettuate
126400000104  "  C* esempio : esclusione degli annullati.
126500000104  "  C*
126600000104  " >C****       ANAANN    WHEQ 'A'
126700000104  " >C****                 MOVEL*OFF      $RCDOK
126800000110  " 0 * EX- A0730 Stato
126900000110  "  C***   "            when      D1Stato <> *blanks and
127000000110  "  C***   "                      (CpiSta < D1Stato or
127100000110  "  C***   "                      CpiSta > Sta999)
127200000110  "  C***   "           eval      $RcdOk = *Off
127300000110  "   *     " Provincia
127400000110  "  C***   "            when      D1Prov <> *blanks and
127500000110  "  C***   "                      (CpiPrv < D1Prov or
127600000110  "  C***   "                       CpiPrv > Prv999)
127700000110  "  C***   "            eval      $Rcdok = *Off
127800000110  "   *     " Partita IVA
127900000110  "  C***   "            when      D1PartIVA <> *blanks and
128000000110  "  C***   "                      (CpiIva < D1PartIVA or
128100000110  "  C***   "                       CpiIva > Iva999)
128200000110  "  C***   "            eval      $RcdOk = *Off
128300000110  "   *     " Codice fiscale
128400000110  "  C***   "            when      D1CdFisc <> *blanks and
128500000110  "  C***   "                      (CpiCdf < D1CdFisc or
128600000110  "  C***   "                       CpiCdf > Cdf999)
128700000110  "  C*** EX-A0730       eval      $RcdOk = *Off
128800000104  "  C*
128900000104  "  C* SELEZIONI RISPETTO ALLE PARZIALIZZAZIONI
129000000104  "  C*
129100000104  "  C** soggetto non compreso nella scelta
129200000104  "  C***  CPICOD        WHENLT    D1SOG
129300000104  "  C***  CPICOD        ORGT      SOG999
129400000104  "  C***                MOVEL     *OFF          $RCDOK
129500000104  "  C*
129600000104  "  C** data (descend) deve essere < = a quella digitata
129700000104  "  C***  CPIDTF        WHENGT    SAVDTF
129800000104  "  C***                MOVEL     *OFF          $RCDOK
129900000104  "  C*
130000000104  "  C***                ENDSL
130100000104  "  C* FINE SELEZIONE
130200000104  "  C*  nel caso che il dato letto sia > del limite superiore e
130300000104  "  C*  questo dato è la chiave della VL in lettura, allora per
130400000104  "  C*  evitare altri accessi inutili salto a fine file
130500000104  "  C***                SELECT
130600000104  "  C* ordinamento per soggetto
130700000104  " >C***  CPICOD        WHENGT    SOG999
130800000104  " >C***  $ORD          ANDEQ     1
130900000104  "  C***                MOVE      *HIVAL        CPICOD
131000000104  " >C***  CPICOD        SETLL     ANCPI01
131100000104  "  C* ordinamento per soggetto
131200000104  " >C***  CPIDTF        WHENGT    SAVDTF
131300000104  " >C***  $ORD          ANDEQ     1
131400000104  "  C***                MOVE      SAVDTF        CPIDTF
131500000104  " >C***  K2CPI1        SETLL     ANCPI01
131600000104  "  C***                ENDSL
131700000104  "  C***
131800000104  "  C***                ENDSR
131900000105A0732C/EJECT
132000940131     C************************************************************
132100940131     C* RIEMPIMENTO RIGA LISTA
132200940131     C************************************************************
132300940131     C     RIES1         BEGSR
132400940131     C*
132500940131     C                   CLEAR                   FMTS1
132600950301     C*
132700950301     C* riempio i campi del sfl, compresi gli hidden con i valori
132800950301     C* del rcd letto
132900950407    >C*-- Converto data :
133000950407     C                   CALL      'XDT4000'
133100000105A0732C***                PARM      CPIDTF        XDTAMG           10
133200000105A0732C                   PARM      CPIDTFIVL     XDTAMG           10
133300950407     C                   PARM                    XDTGMA            6 0
133400950407     C                   PARM      2             XDTSTA            1 0
133500950407     C                   MOVEL     XDTGMA        S1DTF
133600000105A0732C***                MOVEL     CPIDTF        H1DTF
133700000105A0732C                   MOVEL     CPIDTFIVL     H1DTF
133800950407     C*
133900000105A0732C***                MOVEL     CPIIVA        S1IVA
134000000105  "  C***                MOVEL     CPICDF        S1CDF
134100000105  "  C***                MOVEL     CPICOD        S1SOG
134200000105  "  C                   MOVEL     CPIPARTIVA    S1IVA
134300000105  "  C                   MOVEL     CPICDFISC     S1CDF
134400000105A0732C                   MOVEL     CPISOGG       S1SOG
134500950301     C*
134600950301     C* imposto quegli attributi del sfl che sono modificabili
134700950301     C* al richiamo del determinato pgm
134800950301     C                   EXSR      ATRS1
134900940131     C*
135000940131     C                   ENDSR
135100950301     C/EJECT
135200950301     C************************************************************
135300950301     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
135400950301     C************************************************************
135500950301     C     ATRS1         BEGSR
135600950411     C* ND di cliente se ricevuti e non cambiati
135700950411     C                   SETOFF                                       49
135800950411     C     SOG057        IFNE      *BLANK
135900950411     C     SOG057        ANDEQ     D1SOG
136000950411     C                   SETON                                        49
136100950411     C                   ENDIF
136200950411     C*
136300950301     C*
136400950301     C                   ENDSR
136500941205     C/EJECT
136600941205     C************************************************************
136700941205     C* CONTROLLO ABILITAZIONE TASTI VIDEATA S1
136800941205     C* INPUT
136900941205     C* $FC2 = Abilitazioni videata S1
137000941205     C* OUTPUT
137100941205     C* *IN97 *IN99 Errori
137200941205     C************************************************************
137300941205     C     CTRKS1        BEGSR
137400941205     C*
137500941205     C                   SETOFF                                       99
137600941205     C                   MOVEA     $FC2          $FC
137700941205     C                   EXSR      CTRKEY
137800941205     C     *IN99         IFEQ      *ON
137900941205     C                   SETON                                        97
138000941205     C                   ENDIF
138100941205     C*
138200941205     C                   ENDSR
138300941206     C/EJECT
138400941206     C************************************************************
138500941206     C* GESTIONE F08 VIDEO S1
138600941206     C************************************************************
138700941206     C     F08S1         BEGSR
138800941206     C*
138900941206     C                   MOVE      *ON           $FINE
139000950407    >C                   MOVE      '0'           RET057
139100941206     C*
139200941206     C                   ENDSR
139300940309     C/EJECT
139400940309     C************************************************************
139500940309     C* GESTIONE F03 VIDEO S1
139600940309     C************************************************************
139700940309     C     F03S1         BEGSR
139800940309     C*
139900940309     C                   MOVE      *ON           $FINE
140000950407    >C                   MOVE      '1'           RET057
140100940325     C* fine programma
140200940325     C                   EXSR      ENDPGM
140300940309     C*
140400940309     C                   ENDSR
140500940309     C/EJECT
140600940309     C************************************************************
140700940309     C* GESTIONE F10 VIDEO S1
140800940609     C* AGGIUNTA RECORD
140900940309     C************************************************************
141000940309     C     F10S1         BEGSR
141100940309     C*
141200950410    >C                   RESET                   A058DS
141300950407    >C                   MOVEL     '01'          OPZ058
141400950410    >C****       CMT057    IFEQ '1'
141500950410    >C*                    MOVEL'1'       CMT058
141600950410     C*                    ELSE
141700950410    >C*                    MOVEL'0'       CMT058
141800950410     C*                    ENDIF
141900950102     C* passare anche le parzializzazioni ricevute relative alla chiave
142000950102     C* univoca : se ce ne sono saranno usate come default d'immissione
142100950410    >C                   MOVEL     SOG057        SOG058
142200950410    >C                   MOVEL     DTF057        DTF058
142300950403     C* codice di riferimento per le valorizzazioni di default
142400950410    >C******               MOVE $DEF      DF1058
142500950412    >C                   MOVEL     A058DS        KPJBU
142600950407    >C                   CALL      'ANA058R'
142700940309     C                   PARM                    KPJBA
142800950412    >C                   MOVEL     KPJBU         A058DS
142900940309     C* ritorno da PGM gestione
143000950407    >C                   EXSR      RTN058
143100940309     C     WINZS1        IFEQ      *ON
143200940309     C                   MOVE      *ON           $INZS1
143300940309     C* se esistevano già righe sul sfl
143400940309     C* calcolo pagina a cui deve posizionarsi
143500940309     C     WSFL          IFGT      0
143600940309     C                   EXSR      CLCPAG
143700940309     C* altrimenti carico solo la 1a pagina
143800940309     C                   ELSE
143900940309     C                   Z-ADD     SFLPAG        WPAG
144000940309     C                   ENDIF
144100940309     C                   ENDIF
144200940309     C*
144300940627     C                   MOVE      *HIVAL        $LASTV
144400940627     C*
144500940309     C                   ENDSR
144600940309     C/EJECT
144700940309     C************************************************************
144800940309     C* GESTIONE F12 VIDEO S1
144900940309     C************************************************************
145000940309     C     F12S1         BEGSR
145100940309     C*
145200940309     C*  se la 1a videata sono le parzializzazioni, per F12 torno a queste
145300940309     C     $LOP          IFEQ      1
145400940309     C                   MOVE      'D1'          $GEST
145500940309     C*  se la 1a videata è questa, per F12 torno a menu
145600940309     C                   ELSE
145700940309     C                   MOVE      *ON           $FINE
145800950410    >C                   MOVE      '2'           RET057
145900941027     C                   ENDIF
146000940309     C*
146100940309     C                   ENDSR
146200940309     C/EJECT
146300940309     C************************************************************
146400940309     C* GESTIONE F17 VIDEO S1
146500940309     C************************************************************
146600940309     C     F17S1         BEGSR
146700940309     C*
146800940309     C                   MOVE      'D1'          $GEST
146900941104     C* se il pgm è stato richiamato con chiave univoca
147000941104     C* e dopo è stata richiesta una parzializzazione
147100941104     C* tolgo la gestione della chiave univoca (azzere/abblenco il
147200941104     C* valore; propongo come ordinamento il 1°)
147300941213     C     $UNIQ         IFEQ      *ON
147400950331     C                   MOVE      *OFF          $UNIQ
147500950414    >C*                    MOVE *BLANK    UQ1057
147600950414    >C*                    MOVE *BLANK    UQ2057
147700941027     C                   ENDIF
147800940831     C     $ORD          IFEQ      99
147900940831     C                   Z-ADD     1             $ORD
148000941104     C                   MOVE      *ON           $INZD1
148100941027     C                   ENDIF
148200940309     C*
148300940309     C                   ENDSR
148400940309     C/EJECT
148500940309     C************************************************************
148600940309     C* GESTIONE F18 VIDEO S1
148700940309     C************************************************************
148800940309     C     F18S1         BEGSR
148900940309     C*
149000940309     C                   MOVE      'D2'          $GEST
149100940309     C                   MOVEL     *ON           $INZD2
149200940831     C     $ORD          IFEQ      99
149300940831     C                   Z-ADD     1             $ORD
149400941027     C                   ENDIF
149500940309     C*
149600940309     C                   ENDSR
149700940309     C/EJECT
149800940309     C************************************************************
149900940309     C* GESTIONE F23 VIDEO S1
150000940309     C************************************************************
150100940309     C     F23S1         BEGSR
150200940309     C*
150300941220    >C                   Z-ADD     76            $L01
150400941220    >C                   Z-ADD     76            $L02
150500940309     C                   EXSR      RIEOPZ
150600941214     C                   Z-ADD     23            X
150700941214     C                   MOVE      FF23          $FC2(X)
150800940608     C                   MOVEL     WOPZ1         C1OPZ1
150900060224A1148C                   MOVEL     $ALLOPT       H1ALLOPT
151000940608     C                   MOVEL     WOPZ2         C1OPZ2
151100940309     C                   Z-ADD     WCOZ          C1COZ
151200940309     C*
151300940309     C                   ENDSR
151400940309     C/EJECT
151500940309     C************************************************************
151600940309     C* GESTIONE F24 VIDEO S1
151700940309     C************************************************************
151800940309     C     F24S1         BEGSR
151900940506     C*
152000940506     C                   MOVEA     $FX2          $FX
152100940506     C                   MOVEA     $FC2          $FC
152200940506     C                   MOVEA     $FR2          $FR
152300940506     C                   Z-ADD     $ULKS1        $ULK
152400941220    >C                   Z-ADD     79            $L01
152500941220    >C                   Z-ADD     79            $L02
152600940506     C                   EXSR      RIEKEY
152700940506     C                   Z-ADD     $ULK          $ULKS1            3 0
152800940720     C                   MOVEA     $FC           $FC2
152900940506     C                   MOVEL     $KEY1         Z2KE1
153000060224A1148C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
153100940506     C                   MOVEL     $KEY2         Z2KE2
153200940506     C                   WRITE     FMTZ2
153300940506     C*
153400940309     C                   ENDSR
153500940128     C/EJECT
153600940128     C************************************************************
153700940131     C* CONTROLLO TESTATA LISTA
153800940128     C************************************************************
153900940131     C     CTRC1         BEGSR
154000940128     C*
154100940201     C                   MOVE      *OFF          *IN99
154200000103A0730C* Controllo data valida in C1POS
154300000114A0730C     C1POS         IFNE      0
154400000114A0735C***** ex-A0730 *DMY          TEST(D)                 C1POS        90
154500000114A0735C     *JobRun       TEST(D)                 C1POS                  90
154600000114A0730C   90              SETON                                        99
154700000103A0730C                   ENDIF
154800000103     C*
154900940131     C*
155000940202     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
155100940202     C* L'INIZIALIZZAZIONE LISTA
155200940202     C*
155300941214     C     C1POS         IFNE      W1POS
155400950414     C******     C1DTF     ORNE W1DTF
155500940202     C                   EXSR      POSS1
155600941027     C                   ENDIF
155700940202     C*
155800940202     C                   ENDSR
155900940202     C/EJECT
156000940202     C************************************************************
156100940202     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
156200940202     C************************************************************
156300940202     C     POSS1         BEGSR
156400940202     C*
156500940202     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
156600940202     C*
156700941118   B1C     WMAX          IFGT      0
156800940202     C                   READC     FMTS1                                  21
156900941118   B2C     *IN21         DOWEQ     *OFF
157000940202     C     *IN99         ANDEQ     *OFF
157100940202     C*
157200941118   B3C     S1OPZ         IFNE      0
157300940202     C                   SETON                                        51  99
157400941118   E3C                   ENDIF
157500940202     C* se rilevata opzione attiva, attivo sflnxtchg
157600941118   B3C     *IN99         IFEQ      *ON
157700940202     C                   MOVE      *ON           *IN32
157800950301     C*
157900950301     C* imposto quegli attributi del sfl che sono modificabili
158000950301     C* al richiamo del determinato pgm
158100941118     C                   EXSR      ATRS1
158200941118     C*
158300940202     C                   UPDATE    FMTS1
158400941118   X3C                   ELSE
158500940202     C                   READC     FMTS1                                  21
158600941118   E3C                   ENDIF
158700940202     C*
158800941118   E2C                   ENDDO
158900940202     C*
159000941118   E1C                   ENDIF
159100940202     C*
159200940221     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
159300940221     C* AL VALORE DI POSIZIONAMENTO :
159400940221     C* se questo è maggiore del valore del rispettivo campo di
159500940221     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
159600940221     C* con i valori di parzializzazione.
159700940221     C*
159800941118   B1C     *IN99         IFEQ      *OFF
159900940721    >C                   MOVE      *BLANK        W1POS
160000940202     C*
160100000103     C                   SELECT
160200000103     C*
160300000105A0730 * Per tutti gli ordinamenti il campo di posizionamento è la data
160400000105  "  C***  $ORD          WHENEQ    1
160500000103  "  C*
160600000103  "  C***  C1POS         IFGT      D1SOG
160700000103  "  C***                MOVEL     C1POS         W1POS
160800000103  "  C***                ENDIF
160900000103  "  C*
161000000103  "  C***                MOVE      *ON           *IN99
161100000103A0730C***                MOVE      *ON           $INZS1
161200000103     C*
161300000103     C     $ORD          WHENEQ    2
161400000103A0730C     $ORD          OREQ      1
161500950410     C*
161600000103A0730C***                MOVEL     W1POS         W1DTF
161700000103A0730C                   MOVEL     C1POS         W1DTF
161800950410     C                   CALL      'XDT4000'
161900950410     C                   PARM                    XDTAMG           10
162000950419     C                   PARM      W1DTF         XDTGMA            6 0
162100950410     C                   PARM      4             XDTSTA            1 0
162200950419    >C     XDTAMG        IFLT      SAVDTF
162300950419     C                   MOVEL     C1POS         W1POS
162400950419     C                   ENDIF
162500950419     C*
162600950410     C                   MOVE      *ON           *IN99
162700950410     C                   MOVE      *ON           $INZS1
162800940831     C*
162900940831     C     $ORD          WHENEQ    99
163000940831     C*
163100940831     C                   MOVE      *ON           *IN99
163200940831     C                   MOVE      *ON           $INZS1
163300940831     C*
163400941118   E2C                   ENDSL
163500940202     C*
163600941118   E1C                   ENDIF
163700940202     C*
163800940131     C                   ENDSR
163900940131     C/EJECT
164000940131     C************************************************************
164100940131     C* CONTROLLO OPZIONI LISTA
164200940131     C************************************************************
164300940131     C     CTRS1         BEGSR
164400940131     C*
164500940202     C                   MOVEL     *OFF          $ESCI
164600940201     C                   SETOFF                                       99
164700940131     C                   Z-ADD     0             S1OPZ
164800940131     C*
164900940127     C* Leggo il sfl solo se ci sono rcd
165000941118   B1C     WMAX          IFGT      0
165100940131     C                   READC     FMTS1                                  21
165200940127     C*
165300940131     C* esce se fine sfl o errore che richiede l'uscita
165400941118   B2C     *IN21         DOWEQ     *OFF
165500940131     C     $ESCI         ANDEQ     *OFF
165600940201     C                   Z-ADD     S1NRR         C1RCD
165700940131     C* ctrl su riga
165800940131     C                   EXSR      RECS1
165900940131     C* gestione opzioni
166000941118   B3C     S1OPZ         IFNE      0
166100940201     C     *IN99         ANDEQ     *OFF
166200940131     C                   EXSR      OPZS1
166300941118   E3C                   ENDIF
166400940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
166500941118   B3C     *IN99         IFEQ      *ON
166600940131     C                   MOVE      *ON           *IN32
166700941118   E3C                   ENDIF
166800950301     C*
166900950301     C* imposto quegli attributi del sfl che sono modificabili
167000950301     C* al richiamo del determinato pgm
167100941118     C                   EXSR      ATRS1
167200941118     C*
167300940131     C                   Z-ADD     0             S1OPZ
167400940223     C*
167500940127     C                   UPDATE    FMTS1
167600941125     C                   MOVE      *OFF          *IN32
167700940223     C*
167800940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
167900941118   B3C     $ESCI         IFEQ      *OFF
168000940131     C                   READC     FMTS1                                  21
168100940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
168200941118   B4C     *IN21         IFEQ      *ON
168300940201     C                   MOVE      WINZS1        $INZS1
168400940204     C* calcolo pagina a cui deve posizionarsi
168500940728     C     WSFL          IFGT      0
168600940728     C                   EXSR      CLCPAG
168700940728     C                   ELSE
168800940728     C                   Z-ADD     SFLPAG        WPAG
168900940728     C                   ENDIF
169000941118   E4C                   ENDIF
169100941118   E4C                   ENDIF
169200940131     C*
169300941118   E2C                   ENDDO
169400940127     C*
169500941118   E1C                   ENDIF
169600940131     C*
169700940127     C                   ENDSR
169800940127     C/EJECT
169900940127     C************************************************************
170000940131     C* CONTROLLO CAMPI I/O RIGA LISTA
170100940127     C************************************************************
170200940131     C     RECS1         BEGSR
170300940131     C*
170400940201     C* reset indicatori DSPATR
170500940201     C                   MOVE      *ALL'0'       IN4049           10
170600940201     C                   MOVEA     IN4049        *IN(40)
170700940201     C*
170800940131     C* Controllo se Utente Autorizzato alla scelta
170900940202     C     S1OPZ         IFNE      0
171000940131     C                   Z-ADD     1             X
171100940223     C     S1OPZ         LOOKUP    $OP1(X)                                21
171200940211     C     *IN21         IFEQ      *OFF
171300940223     C     $FL1(X)       OREQ      'N'
171400940131     C                   MOVE      *ON           $ESCI
171500940201     C                   SETON                                        504099
171600940131     C                   ENDIF
171700940202     C                   ENDIF
171800940131     C*
171900940131     C                   ENDSR
172000941118     C/EJECT
172100941118     C************************************************************
172200941118     C* GESTIONE OPZIONI LISTA
172300941118     C************************************************************
172400941118     C     OPZS1         BEGSR
172500950301     C*
172600950301     C                   Z-ADD     RIRI          CURR
172700950301     C                   Z-ADD     COCO          CURC
172800941118     C*
172900941027     C                   SELECT
173000940225     C*
173100940225     C* se opz=Selezione passo la chiave al pgm chiamante
173200940225     C     S1OPZ         WHENEQ    1
173300950407    >C                   MOVE      S1OPZ         OPZ057
173400950407     C* passare nei campi appositi della DS A057DS il valore
173500950301     C* della chiave della VL univoca (se c'è)
173600950411    >C                   MOVE      S1SOG         SOG057
173700070228A1223C                   MOVE      S1IVA         IVA057
173800950411    >C                   MOVE      H1DTF         DTF057
173900940225     C                   MOVE      *ON           $ESCI
174000940225     C                   MOVE      *ON           $FINE
174100940909     C*
174200940909     C                   MOVE      *HIVAL        $LASTV
174300940831     C* se opz=Invia scrive nella posta
174400950414     C*
174500950414     C*          S1OPZ     WHEQ 10
174600950414     C*                    RESETA727DS
174700950414     C*                    MOVE '01'      OPZ727
174800950414    >C*                    MOVELH1SOGD    TXT727           Testo
174900950414    >C*SB                  MOVEL'700'     TPA727
175000950414    >C*                    MOVELS1SOG     KY1727
175100950414    >C*                    MOVEL*BLANK    KY2727
175200950414    >C*                    MOVEL*BLANK    KY3727
175300950414    >C*                    MOVEL*BLANK    KY4727
175400950414     C*                    MOVE 'E'       PGM727
175500950414     C*                    MOVE XSCSOC    SOC727
175600950414     C*                    MOVE WIN727    INZ727
175700950414     C*                    CLEARKPJBU
175800950414     C*                    MOVELA727DS    KPJBU
175900950414     C*                    CALL 'ANA727R'
176000950414     C*                    PARM           KPJBA
176100950414     C*                    MOVELKPJBU     A727DS
176200950414     C*
176300950414     C*                    EXSR RTN727
176400950414     C*
176500950414     C*                    MOVE *HIVAL    $LASTV
176600940831     C*
176700940831     C* se opz=Commenti richiamo pgm gestione commenti
176800950410      *
176900950410     C*          S1OPZ     WHEQ 14
177000950410     C*                    RESETA725DS
177100950410    >C*          OPZ057    IFEQ '04'
177200950410    >C*          OPZ057    OREQ '05'
177300950410    >C* oppure se l'utente è abilitato per la funzione specifica di
177400950410    >C* questa anagrafica solo alla visualizzazione
177500950410     C*                    MOVE '05'      OPZ725
177600950410     C*                    ELSE
177700950410     C*                    MOVE '02'      OPZ725
177800950410     C*                    ENDIF
177900950410    >C*                    MOVE '___'     TPA725
178000950410     C* inserire chiavi commenti
178100950410    >C*                    MOVEL______    KY1725
178200950410    >C*                    MOVEL______    KY2725
178300950410    >C*                    MOVEL______    KY3725
178400950410    >C*                    MOVEL______    KY4725
178500950410     C*                    MOVE *BLANK    KPJBU
178600950410     C*                    MOVELA725DS    KPJBU
178700950410     C*                    CALL 'ANA725R'
178800950410     C*                    PARM           KPJBA
178900950410     C*                    MOVELKPJBU     A725DS
179000950410     C*
179100950410     C* ritorno da PGM commenti
179200950410     C*                    EXSR RTN725
179300950410     C*
179400950410     C*                    MOVE *HIVAL    $LASTV
179401100830R027 C*
179402100830  "  C* se opz 22 = Dati aggiuntivi
179403160505R027 C     S1OPZ         WHENEQ    22
179404160505R224 C*
179405160505  "  C* ctr chiamata ricorsiva
179406160505  "  C                   reset                   X48ChPgmds
179407160505  "  C                   eval      X48Pgm      = 'SDGSD15R'
179408160505  "  C                   eval      Kpjbu       = X48ChPgmDS
179409160505  "  C                   call      'X48CHPGM'
179410160505  "  C                   parm                    Kpjba
179411160505  "  C                   movel     Kpjbu         x48ChPgmDS
179412160505  "  C                   if        x48errore   = *Off
179413160505R224 C*
179414160505R027 C                   Eval      $Sogg       = S1Sog
179415100831  "  C                   if        S1DTF      <> 0
179416100831  "  C                   CALL      'XDT4000'
179417100831  "  C                   PARM                    XDTAMG           10
179418100831  "  C                   PARM      S1DTF         XDTGMA            6 0
179419100831  "  C                   PARM      4             XDTSTA            1 0
179420100831  "  C                   MOVEL     XDTAMG        $DtFiVl
179421100831  "  C                   endif
179422100830  "  C                   EXSR      CallDatiAgg
179423100830  "  C*
179424100830R027 C                   MOVE      *HIVAL        $LASTV
179425160505R224 C*
179426160505R224 C                   endif
179500940225     C*
179600940225     C* se altre opzioni (o nessuna opzione) richiamo pgm gestione
179700941027     C                   OTHER
179800940225     C*
179900950410    >C                   RESET                   A058DS
180000950410    >C                   MOVEL     S1OPZ         OPZ058
180100950410    >C***        CMT057    IFEQ '1'
180200950410    >C***                  MOVEL'1'       CMT002
180300950410     C**                   ELSE
180400950410    >C*                    MOVEL'0'       CMT002
180500950410     C*                    ENDIF
180600950410     C* passare nei campi appositi della DS A058DS il valore
180700950310     C* della chiave della VL univoca
180800950412     C                   MOVEL     S1SOG         SOG058
180900950411     C                   MOVEL     H1DTF         DTF058
181000940314     C*
181100950410    >C                   MOVEL     A058DS        KPJBU
181200950411    >C                   CALL      'ANA058R'
181300940314     C                   PARM                    KPJBA
181400950410    >C*  se vengono gestite le autorizzazioni
181500950410    >C*  le passo al pgm di manutenzione per evitargli di doversele
181600950410    >C*  riprendere
181700950410    >C                   PARM                    AUTDS
181800950301     C*
181900950410    >C                   MOVEL     KPJBU         A058DS
182000940201     C*
182100940223     C* ritorno da PGM gestione
182200950410    >C                   EXSR      RTN058
182300940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
182400941027     C     *IN99         IFEQ      *ON
182500940223     C                   SETON                                        40
182600941027     C                   ENDIF
182700940131     C*
182800940506     C                   MOVE      *HIVAL        $LASTV
182900941027     C                   ENDSL
183000940225     C*
183100940131     C                   ENDSR
183200940223     C/EJECT
183201160503R224 X*ex R027 ***************************************************
183202160503  "  X*ex R027 Richiamo pgm per dati anagrafici aggiuntivi
183203160503  "  X*ex R027 ***************************************************
183204160503  "  X*ex R027    CallDatiAgg   BEGSR
183205160503  "  X*ex R027
183206160503  "  X*ex R027           RESET                   SDGSD16DS
183207160503  "  X*ex R027
183208160503  "  X*ex R027           MOVEL     '02'          OPZD16
183209160503  "  X*ex R027 CMT057        IFEQ      '1'
183210160503  "  X*ex R027           MOVEL     '1'           CMTD16
183211160503  "  X*ex R027           ELSE
183212160503  "  X*ex R027           MOVEL     '0'           CMTD16
183213160503  "  X*ex R027           ENDIF
183214160503  "  X*ex R027
183223160503  "  X*ex R027           Eval      SoggD16     = $Sogg
183224160503  "  X*ex R027           Move      $DtFiVl       DtFiVlD16
183237160503  "  X*ex R027
183238160503  "  X*ex R027           MOVEL     SDGSD16DS     KPJBU
183239160503  "  X*ex R027
183240160503  "  X*ex R027           CALL      'SDGSD16R'
183241160503  "  X*ex R027           PARM                    KPJBA
183242160503  "  X*ex R027
183243160503  "  X*ex R027           MOVEL     KPJBU         SDGSD16DS
183244160503  "  X*ex R027
183245160503  "  X*ex R027 ritorno da PGM gestione
183246160503  "  X*ex R027           EXSR      RTND16
183247160503  "  X*ex R027
183248160503  "  X*ex R027 se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
183249160503  "  X*ex R027 *IN99         IFEQ      *ON
183250160503  "  X*ex R027           SETON                                        40
183251160503  "  X*ex R027           ENDIF
183252160503  "  X*ex R027
183253160503  "  X*ex R027           MOVE      *HIVAL        $LASTV
183254160503  "  X*ex R027
183255160503  "  X*ex R027           ENDSR
183256160503  "  X*ex R027 /EJECT
183257160503  "  X*ex R027******************************************************
183258160503  "  X*ex R027 GESTIONE RITORNO DA PGM DI GESTIONE
183259160503  "  X*ex R027****************************************************
183260160503  "  X*ex R027 RTND16    BEGSR
183261160503  "  X*ex R027
183262160503  "  X*ex R027 modo di ritorno
183263160503  "  X*ex R027
183264160503  "  X*ex R027           SELECT
183265160503  "  X*ex R027 questi modi di utilizzo dei valori di ritorno dal
183266160503  "  X*ex R027 pgm di manutenzione rcd di anagrafica sono delle
183267160503  "  X*ex R027 proposte, normalmente sempre valide, ma modificabili
183268160503  "  X*ex R027 per situazioni particolari
183269160503  "  X*ex R027 1 = F3
183270160503  "  X*ex R027  RETD16        WHENEQ    '1'
183271160503  "  X*ex R027           MOVE      *ON           $ESCI                          sospese READC
183272160503  "  X*ex R027           MOVE      *ON           $FINE                          fine pgm
183273160503  "  X*ex R027           MOVE      '1'           RET040
183274160503  "  X*ex R027 2 = F12
183275160503  "  X*ex R027 RETD16        WHENEQ    '2'
183276160503  "  X*ex R027 se il pgm di manutenzione ha prima una guida, è necessario anche
183277160503  "  X*ex R027 questo test, altrimenti prevale il retD04 e il sfl non viene
183278160503  "  X*ex R027 ricaricato
183279160503  "  X*ex R027 OPRD16        ANDNE     '1'
183280160503  "  X*ex R027
183281160503  "  X*ex R027 se l'opzione con cui è stato chiamato non è la modifica
183282160503  "  X*ex R027 memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
183283160503  "  X*ex R027 OPZD16        IFNE      '02'
183296160503  "  X*ex R027           MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
183297160503  "  X*ex R027           else
183298160503  "  X*ex R027           MOVE      *ON           $ESCI                          sospese READC
183299160503  "  X*ex R027           endif
183300160503  "  X*ex R027
183301160503  "  X*ex R027           ENDSL
183302160503  "  X*ex R027
183303160503  "  X*ex R027 operazione eseguite dal pgm chiamato
183304160503  "  X*ex R027
183305160503  "  X*ex R027           SELECT
183306160503  "  X*ex R027 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
183307160503  "  X*ex R027 OPRD16        WHENEQ    '1'
183308160503  "  X*ex R027           MOVE      OPRD16        WOPR                           avverto pgm chiamant
183309160503  "  X*ex R027
183310160503  "  X*ex R027 se l'opzione con cui è stato chiamato non è la modifica
183311160503  "  X*ex R027 memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
183312160503  "  X*ex R027 OPZD16        IFNE      '02'
183320160503  "  X*ex R027           MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
183321160503  "  X*ex R027 se l'opzione con cui è stato chiamato è la modifica
183322160503  "  X*ex R027 aggiorno subito la riga di sfl e non richiedo il ripristino sfl
183323160503  "  X*ex R027           ELSE
183324160503  "  X*ex R027*********** EXSR      AGGS1
183325160503  "  X*ex R027           ENDIF
183326160503  "  X*ex R027
183327160503  "  X*ex R027           ENDSL
183328160503  "  X*ex R027
183329160503  "  X*ex R027 funzione non eseguibile per errore :
183330160503  "  X*ex R027
183331160503  "  X*ex R027           SELECT
183332160503  "  X*ex R027 1 = funzione richiamata chiusa in errore
183333160503  "  X*ex R027 ERRD16        WHENEQ    '1'
183334160503  "  X*ex R027           MOVE      *ON           $ESCI                          sospese READC
183335160503  "  X*ex R027           SETON                                        9399      emiss.SFLMSGID
183336160503  "  X*ex R027 2 = funzione richiamata senza la dovuta autorizzazione
183337160503  "  X*ex R027 ERRD16        WHENEQ    '2'
183338160503  "  X*ex R027           MOVE      *ON           $ESCI                          sospese READC
183339160503  "  X*ex R027           SETON                                        9499      emiss.SFLMSGID
183340160503  "  X*ex R027
183341160503  "  X*ex R027           ENDSL
183342160503  "  X*ex R027
183354160503  "  X*ex R027           ENDSR
183355160503R224 X*ex R027 /EJECT
183356160503R224 C************************************************************
183357160503  "  C* Richiamo pgm per dati anagrafici aggiuntivi
183358160503  "  C************************************************************
183359160503R224 C     CallDatiAgg   BEGSR
183360160503     C*
183361160503     C                   RESET                   SDGSD15DS
183362160503     C*
183363160509     C                   MOVEL     '22'          OPZD15
183364160503     C     CMT040        IFEQ      '1'
183365160503     C                   MOVEL     '1'           CMTD15
183366160503     C                   ELSE
183367160503     C                   MOVEL     '0'           CMTD15
183368160503     C                   ENDIF
183369160503     C*
183371160503     C                   Eval      SoggD15     = $Sogg
183372160503     C                   Move      $DtFiVl       DtFiVlD15
183373160503     C                   Eval      TpCallD15   = '2'
183374160503     C                   Eval      TpDatiD15   = '1'
183375160610     C* Tutte le destinazioni
183376160610     C                   Eval      DestD15     = '**'
183377160503     C*
183378160503     C                   MOVEL     SDGSD15DS     KPJBU
183379160503     C*
183380160503     C                   CALL      'SDGSD15R'
183381160503     C                   PARM                    KPJBA
183382160503     C*
183383160503     C                   MOVEL     KPJBU         SDGSD15DS
183384160503     C*
183385160503     C* ritorno da PGM gestione
183386160503     C                   EXSR      RTND15
183387160503     C*
183388160503     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
183389160503     C     *IN99         IFEQ      *ON
183390160503     C                   SETON                                        40
183391160503     C                   ENDIF
183392160503     C*
183393160503     C                   MOVE      *HIVAL        $LASTV
183394160503     C*
183395160503R224 C                   ENDSR
183396160503R224 C/EJECT
183397160503R224 C************************************************************
183398160503 "   C* GESTIONE RITORNO DA PGM DI GESTIONE
183399160503 "   C************************************************************
183400160503R224 C     RTND15        BEGSR
183401160503     C*
183402160503     C* modo di ritorno
183403160503     C*
183404160503     C                   SELECT
183405160503     C* questi modi di utilizzo dei valori di ritorno dal
183406160503     C* pgm di manutenzione rcd di anagrafica sono delle
183407160503     C* proposte, normalmente sempre valide, ma modificabili
183408160503     C* per situazioni particolari
183409160503     C* 1 = F3
183410160503    >C     RETD15        WHENEQ    '1'
183411160503     C                   MOVE      *ON           $ESCI                          sospese READC
183412160503     C                   MOVE      *ON           $FINE                          fine pgm
183413160503    >C                   MOVE      '1'           RET057
183414160503     C* 2 = F12
183415160503    >C     RETD15        WHENEQ    '2'
183416160503     C* se il pgm di manutenzione ha prima una guida, è necessario anche
183417160503     c* questo test, altrimenti prevale il retD04 e il sfl non viene
183418160503     C* ricaricato
183419160503    >C     OPRD15        ANDNE     '1'
183420160503     C*
183421160503     C* se l'opzione con cui è stato chiamato non è la modifica
183422160503     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
183423160503    >C     OPZD15        IFNE      '02'
183424160503     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
183425160503     C                   else
183426160503     C                   MOVE      *ON           $ESCI                          sospese READC
183427160503     C                   endif
183428160503     C*
183429160503     C                   ENDSL
183430160503     C*
183431160503     C* operazione eseguite dal pgm chiamato
183432160503     C*
183433160503     C                   SELECT
183434160503     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
183435160503    >C     OPRD15        WHENEQ    '1'
183436160503    >C                   MOVE      OPRD15        WOPR                           avverto pgm chiamant
183437160503     C*
183438160503     C* se l'opzione con cui è stato chiamato non è la modifica
183439160503     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
183440160503    >C     OPZD15        IFNE      '02'
183441160503     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
183442160503     C* se l'opzione con cui è stato chiamato è la modifica
183443160503     C* aggiorno subito la riga di sfl e non richiedo il ripristino sfl
183444160503     C                   ELSE
183445160503     C                   EXSR      AGGS1
183446160503     C                   ENDIF
183447160503     C*
183448160503     C                   ENDSL
183449160503     C*
183450160503     C* funzione non eseguibile per errore :
183451160503     C*
183452160503     C                   SELECT
183453160503     C* 1 = funzione richiamata chiusa in errore
183454160503    >C     ERRD15        WHENEQ    '1'
183455160503     C                   MOVE      *ON           $ESCI                          sospese READC
183456160503     C                   SETON                                        9399      emiss.SFLMSGID
183457160503     C* 2 = funzione richiamata senza la dovuta autorizzazione
183458160503    >C     ERRD15        WHENEQ    '2'
183459160503     C                   MOVE      *ON           $ESCI                          sospese READC
183460160503     C                   SETON                                        9499      emiss.SFLMSGID
183461160503     C*
183462160503     C                   ENDSL
183463160503     C*
183464160503R224 C                   ENDSR
183465160503R224 C/EJECT
183466940223     C************************************************************
183467940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
183500940223     C************************************************************
183600950407    >C     RTN058        BEGSR
183700940223     C*
183800940223     C* modo di ritorno
183900940223     C*
184000941027     C                   SELECT
184100950301     C* questi modi di utilizzo dei valori di ritorno dal
184200950301     C* pgm di manutenzione rcd di anagrafica sono delle
184300950301     C* proposte, normalmente sempre valide, ma modificabili
184400950301     C* per situazioni particolari
184500940223     C* 1 = F3
184600950407    >C     RET058        WHENEQ    '1'
184700940224     C                   MOVE      *ON           $ESCI                          sospese READC
184800940223     C                   MOVE      *ON           $FINE                          fine pgm
184900950410     C                   MOVE      '1'           RET057
185000940223     C* 2 = F12
185100950407    >C     RET058        WHENEQ    '2'
185200940223     C                   MOVE      *ON           $ESCI                          sospese READC
185300940223     C*
185400941027     C                   ENDSL
185500940223     C*
185600940223     C* operazione eseguite dal pgm chiamato
185700940223     C*
185800941027     C                   SELECT
185900940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
186000950407    >C     OPR058        WHENEQ    '1'
186100950301     C*
186200950301     C* se l'opzione con cui è stato chiamato non è la modifica
186300950301     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
186400950407    >C     OPZ058        IFNE      '02'
186500940223     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
186600950407    >C                   MOVE      OPR058        WOPR                           avverto pgm chiamant
186601160503R224 X*ex R027
186602160503  "  X*ex R027           Eval      $Sogg       = Sog058
186603160503  "  X*ex R027           if        Opz058      = '01'
186605160503  "  X*ex R027           if        DtF058      = $Dt9
186606160503  "  X*ex R027           Move      $Dt0          $DtFiVl
186607160503  "  X*ex R027           else
186608160503  "  X*ex R027           Move      DtF058        $DtFiVl
186609160503  "  X*ex R027           endif
186611160503  "  X*ex R027           EXSR      CallDatiAgg
186612160503  "  X*ex R027           endif
186613160503R224 X*ex R027
186700950301     C* se l'opzione con cui è stato chiamato è la modifica
186800950301     C* aggiorno subito la riga di sfl e non richiedo il ripristino sfl
186900950301     C                   ELSE
187000950301     C                   EXSR      AGGS1
187100950301     C                   ENDIF
187200940223     C*
187300941027     C                   ENDSL
187400940223     C*
187500940223     C* funzione non eseguibile per errore :
187600940223     C*
187700941027     C                   SELECT
187800940223     C* 1 = funzione richiamata chiusa in errore
187900950407    >C     ERR058        WHENEQ    '1'
188000940223     C                   MOVE      *ON           $ESCI                          sospese READC
188100940223     C                   SETON                                        5299      emiss.SFLMSGID
188200950301     C* 2 = funzione richiamata senza la dovuta autorizzazione
188300950407    >C     ERR058        WHENEQ    '2'
188400950301     C                   MOVE      *ON           $ESCI                          sospese READC
188500950301     C                   SETON                                        5499      emiss.SFLMSGID
188600941027     C*
188700941027     C                   ENDSL
188800940223     C*
188900940223     C                   ENDSR
189000950301     C/EJECT
189100950301     C************************************************************
189200950301     C* AGGIORNAMENTO RIGA LISTA
189300950301     C************************************************************
189400950301     C     AGGS1         BEGSR
189500950301     C*
189600950301     C* riempio i campi del sfl che sono stati ripassati dal pgm
189700950301     C* di manutenzione dopo una opzione 02
189800950301     C*
189900950410     C                   MOVEL     IVA058        S1IVA
190000950410     C                   MOVEL     CDF058        S1CDF
190100950301     C*
190200950301     C                   ENDSR
190300941124     C/EJECT
190400941124     C************************************************************
190500941124     C* GESTIONE RITORNO DA PGM DEI COMMENTI
190600941124     C************************************************************
190700950410      *
190800950410     C*          RTN725    BEGSR
190900950410     C*
191000950410     C* modo di ritorno
191100950410     C*
191200950410     C*                    SELEC
191300950410     C* questi modi di utilizzo dei valori di ritorno dal
191400950410     C* pgm di manutenzione rcd di anagrafica sono delle
191500950410     C* proposte, normalmente sempre valide, ma modificabili
191600950410     C* per situazioni particolari
191700950410     C* 1 = F3
191800950410     C*          RET725    WHEQ '1'
191900950410     C*                    MOVE *ON       $ESCI            sospese READC
192000950410     C*                    MOVE *ON       $FINE            fine pgm
192100950410     C* 2 = F12
192200950410     C*          RET725    WHEQ '2'
192300950410     C*                    MOVE *ON       $ESCI            sospese READC
192400950410     C*
192500950410     C*                    ENDSL
192600950410     C*
192700950410     C* operazione eseguite dal pgm chiamato
192800950410     C*
192900950410     C*                    SELEC
193000950410     C* 1 = eseguita scrittura o modifica commento
193100950410     C*          OPR725    WHEQ '1'
193200950410     C*
193300950410     C*                    ENDSL
193400950410     C*
193500950410     C* funzione non eseguibile per errore :
193600950410     C*
193700950410     C*                    SELEC
193800950410     C* 1 = funzione richiamata chiusa in errore
193900950410     C*          ERR725    WHEQ '1'
194000950410     C*                    MOVE *ON       $ESCI            sospese READC
194100950410     C*                    SETON                     5299  emiss.SFLMSGID
194200950410     C* 2 = funzione richiamata senza la dovuta autorizzazione
194300950410     C*          ERR725    WHEQ '2'
194400950410     C*                    MOVE *ON       $ESCI            sospese READC
194500950410     C*                    SETON                     5499  emiss.SFLMSGID
194600950410     C*
194700950410     C*                    ENDSL
194800950410     C*
194900950410     C*                    ENDSR
195000950331     C/EJECT
195100950331     C************************************************************
195200950331     C* GESTIONE RITORNO DA PGM INVIO
195300950331     C************************************************************
195400950414     C*          RTN727    BEGSR
195500950331     C*
195600950331     C* modo di ritorno
195700950331     C*
195800950414     C*                    SELEC
195900950414     C* questi modi di utilizzo dei valori di ritorno dal
196000950414     C* pgm di manutenzione rcd di anagrafica sono delle
196100950414     C* proposte, normalmente sempre valide, ma modificabili
196200950414     C* per situazioni particolari
196300950414     C* 1 = F3
196400950414     C*          RET727    WHEQ '1'
196500950414     C*                    MOVE *ON       $ESCI            sospese READC
196600950414     C*                    MOVE *ON       $FINE            fine pgm
196700950414     C* 2 = F12
196800950414     C*          RET727    WHEQ '2'
196900950414     C*                    MOVE *ON       $ESCI            sospese READC
197000950414     C*
197100950414     C*                    ENDSL
197200950414     C*
197300950414     C* operazione eseguite dal pgm chiamato
197400950414     C*
197500950414     C*                    SELEC
197600950414     C* 1 = eseguito aggiornamento
197700950414     C*          OPR727    WHEQ '1'
197800950414     C*
197900950414     C*                    ENDSL
198000950414     C*
198100950414     C* funzione non eseguibile per errore :
198200950414     C*
198300950414     C*                    SELEC
198400950414     C* 1 = funzione richiamata chiusa in errore
198500950414     C*          ERR727    WHEQ '1'
198600950414     C*                    MOVE *ON       $ESCI            sospese READC
198700950414     C*                    SETON                     5299  emiss.SFLMSGID
198800950414     C* 2 = funzione richiamata senza la dovuta autorizzazione
198900950414     C*          ERR727    WHEQ '2'
199000950414     C*                    MOVE *ON       $ESCI            sospese READC
199100950414     C*                    SETON                     5499  emiss.SFLMSGID
199200950414     C*
199300950414     C*                    ENDSL
199400950414     C*
199500950414     C* disattivo il flag di avvenuta EXFMT dopo il richiamo a pgm
199600950414     C* di sola finestra
199700950414     C*                    MOVEL'0'       WIN727
199800950414     C*
199900950414     C*                    ENDSR
200000940223     C/EJECT
200100940131     C************************************************************
200200940131     C* GESTIONE VALORI ASSUNTI
200300940131     C************************************************************
200400940131     C     GESD2         BEGSR
200500940131     C*
200600941118   B1C     $INZD2        IFEQ      *ON
200700940131     C                   EXSR      INZD2
200800940223     C                   MOVE      *OFF          $INZD2
200900941118   E1C                   ENDIF
201000940202     C*
201100941118   B1C     $LASTV        IFNE      'D2'
201200940923     C                   EXSR      GESPIE
201300940202     C                   MOVE      $LASTV        $LASTG
201400940202     C                   MOVE      'D2'          $LASTV
201500941118   E1C                   ENDIF
201600940131     C*
201700940707     C* se esistono errori sulla videata
201800940707     C* emetto la write del formato a indicatori spenti per vedere
201900940707     C* le eventuali decodifiche
202000941118   B1C     *IN99         IFEQ      *ON
202100940707     C                   MOVEA     *IN           WIN
202200940707     C                   MOVE      *ALL'0'       IN5098           49
202300940707     C                   MOVEA     IN5098        *IN(50)
202400940707     C                   WRITE     FMTD2
202500940707     C                   MOVEA     WIN           *IN
202600941118   E1C                   ENDIF
202700940707     C*
202800940131     C                   EXFMT     FMTD2
202900941205     C*
203000941205     C* controllo abilitazione tasti funzione
203100941205     C                   EXSR      CTRKD2
203200941205     C*
203300941205   B1C     *IN99         IFEQ      *OFF
203400941205     C*
203500941205   B2C                   SELECT
203600940131     C* F3=Fine
203700940506     C     $TASTO        WHENEQ    F03
203800940309     C                   EXSR      F03D2
203900940131     C* F12=Ritorno
204000940506     C     $TASTO        WHENEQ    F12
204100940309     C                   EXSR      F12D2
204200940916     C* F24=Altri tasti
204300940916     C     $TASTO        WHENEQ    F24
204400940916     C                   EXSR      F24D2
204500940131     C*
204600940131     C                   OTHER
204700940131     C                   EXSR      CTRD2
204800941205   B3C     *IN99         IFEQ      *OFF
204900940211     C                   MOVE      $LASTG        $GEST
205000941205   B4C                   SELECT
205100940202     C     $LASTG        WHENEQ    'S1'
205200940131     C* se è stato modificato l'ordinamento di default, ricarico la lista
205300941205   B5C     $ORD          IFNE      D2ORD
205400940223     C                   MOVE      *ON           $INZS1
205500940302     C                   MOVE      *BLANK        W1POS
205600941205   E5C                   ENDIF
205700940202     C     $LASTG        WHENEQ    'D1'
205800940318     C* per ora niente
205900941205   E4C                   ENDSL
206000940131     C* definisco i nuovi valori di default
206100940201     C                   Z-ADD     D2LOP         $LOP
206200940131     C                   Z-ADD     D2ORD         $ORD
206300950410    >C***                  MOVE D2DEF     $DEF
206400950322     C*
206500950322     C* aggiorno valori assunti
206600950328     C* se richiesto il salvataggio
206700950328   B4C     D2BSV         IFEQ      1
206800950322     C                   EXSR      AGGMPE
206900950322   E4C                   ENDIF
207000950322     C*
207100941205   E3C                   ENDIF
207200940131     C*
207300941205   E2C                   ENDSL
207400940131     C*
207500941205   E1C                   ENDIF
207600941205     C*
207700940131     C                   ENDSR
207800940223     C/EJECT
207900940131     C************************************************************
208000940211     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
208100940131     C************************************************************
208200940201     C     INZD2         BEGSR
208300940131     C*
208400940131     C                   CLEAR                   FMTD2
208500021014A0972C                   EVAL      D2BSVA = Normale
208600950328     C* se sono entrato con chiave univoca
208700950328     C* impedisco il salvataggio valori assunti
208800950328     C     $UNIQ         IFEQ      *ON
208900021014A0972X***                Z-ADD     2             D2BSV
209000021014A0972C                   EVAL      D2BSVA = Protetto
209100950328     C                   ENDIF
209200940201     C                   MOVEL     $LOP          D2LOP
209300940831     C                   MOVE      $ORD          D2ORD
209400950410     C****                 MOVE $DEF      D2DEF
209500940131     C*
209600940131     C                   ENDSR
209700941205     C/EJECT
209800941205     C************************************************************
209900941205     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D2
210000941205     C* INPUT
210100941205     C* $FC3 = Abilitazioni videata D2
210200941205     C* OUTPUT
210300941205     C* *IN97 *IN99 Errori
210400941205     C************************************************************
210500941205     C     CTRKD2        BEGSR
210600941205     C*
210700941205     C                   SETOFF                                       99
210800941205     C                   MOVEA     $FC3          $FC
210900941205     C                   EXSR      CTRKEY
211000941205     C     *IN99         IFEQ      *ON
211100941205     C                   SETON                                        97
211200941205     C                   ENDIF
211300941205     C*
211400941205     C                   ENDSR
211500940309     C/EJECT
211600940309     C************************************************************
211700940309     C* GESTIONE F03 VIDEO D2
211800940309     C************************************************************
211900940309     C     F03D2         BEGSR
212000940309     C*
212100940309     C                   MOVE      *ON           $FINE
212200940325     C* fine programma
212300940325     C                   EXSR      ENDPGM
212400940309     C*
212500940309     C                   ENDSR
212600940309     C/EJECT
212700940309     C************************************************************
212800940309     C* GESTIONE F12 VIDEO D2
212900940309     C************************************************************
213000940309     C     F12D2         BEGSR
213100940309     C*
213200940309     C                   MOVE      $LASTG        $GEST
213300940309     C*
213400940309     C                   ENDSR
213500940916     C/EJECT
213600940916     C************************************************************
213700940916     C* GESTIONE F24 VIDEO D2
213800940916     C************************************************************
213900940916     C     F24D2         BEGSR
214000940916     C*
214100940916     C                   MOVEA     $FX3          $FX
214200940916     C                   MOVEA     $FC3          $FC
214300940916     C                   MOVEA     $FR3          $FR
214400940916     C                   Z-ADD     $ULKD2        $ULK
214500950403    >C                   Z-ADD     44            $L01
214600950328    >C                   Z-ADD     0             $L02
214700940916     C                   EXSR      RIEKEY
214800940916     C                   Z-ADD     $ULK          $ULKD2            3 0
214900940916     C                   MOVEA     $FC           $FC3
215000940916     C                   MOVEL     $KEY1         Z3KE1
215100060224A1148C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
215200940916     C                   WRITE     FMTZ3
215300940916     C*
215400940916     C                   ENDSR
215500940223     C/EJECT
215600940131     C************************************************************
215700940131     C* CONTROLLO VIDEATA VALORI ASSUNTI
215800940131     C************************************************************
215900940131     C     CTRD2         BEGSR
216000940131     C*
216100940131     C                   SETOFF                                       99
216200940131     C* non imposto pos. del cursore
216300940131     C                   Z-ADD     0             H1RIGA
216400940131     C                   Z-ADD     0             H1COLO
216500940131     C*
216600940131     C* OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
216700940201     C     D2LOP         IFEQ      0
216800940131     C                   SETON                                          5099
216900941027     C                   ENDIF
217000940131     C*
217100940131     C* OBBLIGATORIO VALORE PER ORDINAMENTO
217200940131     C     D2ORD         IFEQ      0
217300940131     C                   SETON                                          5199
217400941027     C                   ENDIF
217500950403     C*
217600950403     C* CONTROLLO ESISTENZA CODICE PER I DEFAULT
217700950410      *
217800950410     C*          D2DEF     IFNE *BLANK
217900950410    >C*          D2DEF     SETLLAN___01                  21
218000950410     C*          *IN21     IFEQ *OFF
218100950410     C*                    SETON                     5699
218200950410     C*                    ENDIF
218300950410     C*                    ENDIF
218400940131     C*
218500940131     C                   ENDSR
218600941205     C/EJECT
218700941205     C************************************************************
218800941205     C* CONTROLLO ABILITAZIONE TASTI
218900941205     C* INPUT
219000941205     C* $TASTO =Tasto funzione premuto
219100941205     C* $FC = Tasti abilitati
219200941205     C* OUTPUT
219300941205     C* IN99 = ERrore
219400941205     C************************************************************
219500941205     C     CTRKEY        BEGSR
219600941205     C*
219700941205     C                   SELECT
219800941205     C     $TASTO        WHENEQ    F01
219900941205     C     $FC(1)        IFEQ      '0'
220000941205     C                   SETON                                        99
220100941205     C                   ENDIF
220200941205     C     $TASTO        WHENEQ    F02
220300941205     C     $FC(2)        IFEQ      '0'
220400941205     C                   SETON                                        99
220500941205     C                   ENDIF
220600941205     C     $TASTO        WHENEQ    F03
220700941205     C     $FC(3)        IFEQ      '0'
220800941205     C                   SETON                                        99
220900941205     C                   ENDIF
221000941205     C     $TASTO        WHENEQ    F04
221100941205     C     $FC(4)        IFEQ      '0'
221200941205     C                   SETON                                        99
221300941205     C                   ENDIF
221400941205     C     $TASTO        WHENEQ    F05
221500941205     C     $FC(5)        IFEQ      '0'
221600941205     C                   SETON                                        99
221700941205     C                   ENDIF
221800941205     C     $TASTO        WHENEQ    F06
221900941205     C     $FC(6)        IFEQ      '0'
222000941205     C                   SETON                                        99
222100941205     C                   ENDIF
222200941205     C     $TASTO        WHENEQ    F07
222300941205     C     $FC(7)        IFEQ      '0'
222400941205     C                   SETON                                        99
222500941205     C                   ENDIF
222600941205     C     $TASTO        WHENEQ    F08
222700941205     C     $FC(8)        IFEQ      '0'
222800941205     C                   SETON                                        99
222900941205     C                   ENDIF
223000941205     C     $TASTO        WHENEQ    F09
223100941205     C     $FC(09)       IFEQ      '0'
223200941205     C                   SETON                                        99
223300941205     C                   ENDIF
223400941205     C     $TASTO        WHENEQ    F10
223500941205     C     $FC(10)       IFEQ      '0'
223600941205     C                   SETON                                        99
223700941205     C                   ENDIF
223800941205     C     $TASTO        WHENEQ    F11
223900941205     C     $FC(11)       IFEQ      '0'
224000941205     C                   SETON                                        99
224100941205     C                   ENDIF
224200941205     C     $TASTO        WHENEQ    F12
224300941205     C     $FC(12)       IFEQ      '0'
224400941205     C                   SETON                                        99
224500941205     C                   ENDIF
224600941205     C     $TASTO        WHENEQ    F13
224700941205     C     $FC(13)       IFEQ      '0'
224800941205     C                   SETON                                        99
224900941205     C                   ENDIF
225000941205     C     $TASTO        WHENEQ    F14
225100941205     C     $FC(14)       IFEQ      '0'
225200941205     C                   SETON                                        99
225300941205     C                   ENDIF
225400941205     C     $TASTO        WHENEQ    F15
225500941205     C     $FC(15)       IFEQ      '0'
225600941205     C                   SETON                                        99
225700941205     C                   ENDIF
225800941205     C     $TASTO        WHENEQ    F16
225900941205     C     $FC(16)       IFEQ      '0'
226000941205     C                   SETON                                        99
226100941205     C                   ENDIF
226200941205     C     $TASTO        WHENEQ    F17
226300941205     C     $FC(17)       IFEQ      '0'
226400941205     C                   SETON                                        99
226500941205     C                   ENDIF
226600941205     C     $TASTO        WHENEQ    F18
226700941205     C     $FC(18)       IFEQ      '0'
226800941205     C                   SETON                                        99
226900941205     C                   ENDIF
227000941205     C     $TASTO        WHENEQ    F19
227100941205     C     $FC(19)       IFEQ      '0'
227200941205     C                   SETON                                        99
227300941205     C                   ENDIF
227400941205     C     $TASTO        WHENEQ    F20
227500941205     C     $FC(20)       IFEQ      '0'
227600941205     C                   SETON                                        99
227700941205     C                   ENDIF
227800941205     C     $TASTO        WHENEQ    F21
227900941205     C     $FC(21)       IFEQ      '0'
228000941205     C                   SETON                                        99
228100941205     C                   ENDIF
228200941205     C     $TASTO        WHENEQ    F22
228300941205     C     $FC(22)       IFEQ      '0'
228400941205     C                   SETON                                        99
228500941205     C                   ENDIF
228600941205     C     $TASTO        WHENEQ    F23
228700941205     C     $FC(23)       IFEQ      '0'
228800941205     C                   SETON                                        99
228900941205     C                   ENDIF
229000941205     C     $TASTO        WHENEQ    F24
229100941205     C     $FC(24)       IFEQ      '0'
229200941205     C                   SETON                                        99
229300941205     C                   ENDIF
229400941205     C                   ENDSL
229500941205     C*
229600941205     C                   ENDSR
229700940223     C/EJECT
229800940201     C************************************************************
229900940201     C* AGGIORNAMENTO VALORI ASSUNTI
230000940201     C************************************************************
230100940201     C     AGGMPE        BEGSR
230200940201     C*
230300990524B0025C****               CLEAR                   ANMPE000
230400990524B0025C**                 MOVEL     XSCSOC        MPESOC
230500990524B0025C***                MOVEL     DSPGM         MPEPGM
230600990524B0025C***                MOVEL     KNMUS         MPEPRF
230700990525  "  C* preparo i parametri di chiamata al driver
230800990525  "  C                   CLEAR                   XMPEDS
230900990525  "  C                   EVAL      TpChiamata = '03'
231000990525  "  C                   EVAL      NChiavi = 5
231100990525  "  C                   EVAL      VLogica = 2
231200990525  "  C*
231300990525  "  C* preparo le chiavi per la chiamata al driver
231400990525  "  C                   CLEAR                   XMPEDS1
231500990525  "  C                   MOVEL     XSCSOC        DSMPESOC
231600990525  "  C                   MOVEL     DSPGM         DSMPEPGM
231700040302B0025C                   MOVEL     KNMUS         DSMPEPRF
231800941013     C*
231900950322     C* Si memorizzano i dati di default a seconda delle modalità con
232000950322     C* cui il pgm è usato
232100950322     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
232200950322     C* MPETIP = 'PV'  pgm richiamato da menù
232300950322     C* MPETIP = 'PB'  x lancio Batch
232400950322     C*
232500941013     C                   SELECT
232600950410     C     OPZ057        WHENEQ    '01'
232700990525b0025C***                MOVEL     'PX'          MPETIP
232800990525b0025C                   MOVEL     'PX'          dsMPETIP
232900941013     C                   OTHER
233000990525b0025C******             MOVEL     'PV'          MPETIP
233100990525b0025C                   MOVEL     'PV'          dsMPETIP
233200990525     C                   ENDSL
233300990525B0025C***  K4MPE2        CHAIN     ANMPE02L                           21
233400990525B0025C******             MOVEL     DSMPE         MPEPAR
233500990524B0025C***  *IN21         IFEQ      *ON
233600990524B0025C**                 WRITE     ANMPE000
233700990524B0025C**                 ELSE
233800990524B0025C**                 UPDATE    ANMPE000
233900990524B0025C***                ENDIF
234000990524 "   C*
234100990525 "   C                   MOVEL     DSMPE         dsMPEPAR
234200990524 "   C                   CALLB     'XMPE'
234300990524 "   C                   PARM                    XMPEDS
234400040302B0025C                   PARM                    XMPEDS1
234500940201     C*
234600940201     C                   ENDSR
234700940223     C/EJECT
234800940131     C************************************************************
234900940131     C* OPERAZIONI INIZIALI
235000940131     C************************************************************
235100940131     C     *INZSR        BEGSR
235200000114A0735C     *DMY          Move      Udate         ZDateISO
235300000114A0735C     *JobRun       Move      ZDateISO      ZDate             6 0
235400940131     C*
235500940127     C* Reperimento parametri
235600940127     C*
235700940117     C     *ENTRY        PLIST
235800940117     C                   PARM                    KPJBA
235900950411     C                   PARM                    DSAUT
236000940225     C*
236100950411     C*
236200950411     C     PARMS         IFLT      2
236300950411     C                   MOVE      *BLANK        AUTDS
236400950411     C                   ELSE
236500950411     C                   MOVEL     DSAUT         AUTDS
236600950411     C                   ENDIF
236700950411     C*
236800940223     C* Reperimento opzioni
236900940127     C*
237000940128     C                   EXSR      REPOPZ
237100950406     C*
237200940506     C* Reperimento tasti di funzione
237300940506     C*
237400940506     C                   EXSR      REPKEY
237500940506     C*
237600940127     C*
237700940117     C                   ENDSR
237800940223     C/EJECT
237900940223     C************************************************************
238000940223     C* INIZIALIZZAZIONE VARIABILI
238100940223     C************************************************************
238200940223     C     INZVAR        BEGSR
238300941215     C*
238400941215     C* Dati da PGM chiamante
238500941215     C*
238600950410    >C                   MOVEL     KPJBU         A057DS
238700941201     C*
238800941201     C* Reperimento dati società
238900941201     C*
239000941115    >C                   MOVEL     'SOC001'      TIPXSC
239100941115     C                   MOVEL     *BLANK        SOCXSC
239200941115     C                   EXSR      REPSOC
239300941115     C     RTNXSC        IFNE      '1'
239400950330    >C                   MOVEL     XSOCDS        SOC001
239500941115     C                   ELSE
239600950116     C                   MOVE      *ON           $FINE
239700950116     C                   EXSR      ENDPGM
239800941115     C                   ENDIF
239900941201     C*
240000941124     C* Reperimento autorizzazioni
240100950410     C*
240200950410     C* le autorizzazioni potrebbero essere già state passate,
240300950410     C* nel qual caso non vanno reperite
240400950410     C     AUTDS         IFEQ      *BLANK
240500941124    >C                   EXSR      REPAUT
240600950410     C                   ELSE
240700950410     C                   MOVEL     XCADDS        GSOGDS
240800950410     C                   ENDIF
240900941026     C*
241000941026     C* Reperimento valori default utente
241100941026     C*
241200941026     C                   EXSR      REPDFT
241300940506     C*
241400940506     C* Definizione delle opzioni utilizzabili a pgm
241500940506     C*
241600940506     C                   EXSR      DEFOPZ
241700950330     C*
241800940223     C* Variabili per gestione videate
241900940223     C*
242000940223     C                   MOVE      *OFF          $FINE                          fine pgm
242100940223     C                   MOVE      *OFF          $INZD1                         inizializzione D1
242200940223     C                   MOVE      *OFF          $INZS1                         inizializzione S1
242300940223     C                   MOVE      *OFF          $INZD2                         inizializzione D2
242400940223     C                   MOVE      *OFF          $EFILE                         memo fine file
242500940223     C                   MOVE      *OFF          $ESCI                          rich.uscita ciclo
242600000105A0732C***                MOVE      *OFF          $RCDOK                         rcd letto valido
242700940509     C                   MOVE      *HIVAL        $LASTV
242800940506     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
242900940506     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
243000940916     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
243100940223     C*
243200940318     C                   MOVE      *ON           $INZD1
243300940318     C                   MOVE      *ON           $INZS1
243400940831     C*
243500940831     C* se pgm richiamato con chiave univoca impostata,
243600940831     C* forzo la emissione della lista e imposto l'ordinamento
243700950414    >C*          UQ1057    IFNE *BLANK
243800950414     C*                    MOVE *ON       $UNIQ
243900950414     C*                    Z-ADD2         $LOP
244000950414     C*                    Z-ADD99        $ORD
244100950414     C*                    ELSE
244200950414     C*                    MOVE *OFF      $UNIQ
244300950414     C*                    ENDIF
244400950330     C*
244500950330     C* Variabili appoggio
244600950330     C*
244700950330     C                   Z-ADD     SFLPAG        WPAG
244800950330     C                   MOVEL     *OFF          WOPR
244900941104     C*
245000941104     C* definisco qual'è la prima videata
245100941104     C* (se è la lista devo comunque inizializzare la videata di
245200941104     C*  parzializzazione perchè il sfl viene caricato in base a quei
245300941104     C*  dati)
245400950411     C*
245500950411     C*
245600990525b0025C***  SOG057        IFNE      *BLANK
245700990525b0025C***                MOVE      2             $LOP
245800990525b0025C***                ENDIF
245900940223     C                   SELECT
246000940223     C     $LOP          WHENEQ    1
246100940223     C                   MOVE      'D1'          $GEST
246200940223     C     $LOP          WHENEQ    2
246300940223     C                   MOVE      'S1'          $GEST
246400941104     C                   EXSR      INZD1
246500941104     C                   MOVE      *OFF          $INZD1
246600941104     C                   ENDSL
246700950411     C*-- Inizializzo Data :
246800950411     C*
246900000119A0735C***** ex-A0732     CALL      'XDT4000'
247000000119  "  C*****  "           PARM      DTF057        XDTAMG           10
247100000119  "  C*****  "DtTemp     PARM                    XDTGMA            6 0
247200000119A0735C***** ex-A0732     PARM      2             XDTSTA            1 0
247300000105     C*
247400950411     C     DTF057        IFGT      *ZEROS
247500000119A0735C*****ex-A0732  DtTemp        AndNe     MaxRic
247600950411     C                   MOVEL     DTF057        SAVDTF
247700950411     C                   ELSE
247800950411     C                   MOVEL     $DT9          SAVDTF
247900950411     C                   END
248000940223     C*
248100940223     C* Valorizzazione campi univoci testate
248200940223     C*
248300940223     C                   CLEAR                   FMTT1
248400940223     C                   MOVEL     KNSIF         NOMSIF
248500941109     C                   MOVEL     XSCDSI        NOMDIT
248600941027     C                   MOVEL     DSPGM         NOMPGM
248700950331     C* recupero il sottotitolo (se c'è)
248800950601     C*          SOTTIT    IFNE *BLANK
248900950601     C*                    MOVELSOTTIT    IDMSG
249000950601     C*                    CALL 'XRTVM'
249100950601     C*                    PARM           IDMSG  27
249200950601     C*                    PARM           TXTMSG100
249300950601     C*                    PARM           ERRMSG  1
249400950601     C*                    PARM *ON       CTRMSG  1        centratura
249500950601     C*                    PARM 30        LENMSG  30       lun output.
249600950601     C*                    ENDIF
249700950410     C*
249800940223     C                   ENDSR
249900941209     C/EJECT
250000941209     C************************************************************
250100941209     C* REPERIMENTO DATI SOCIETA'
250200941209     C************************************************************
250300941209     C     REPSOC        BEGSR
250400941209     C*
250500941209     C                   CALL      'XSOC'
250600941209     C                   PARM                    TIPXSC            6
250700941209     C                   PARM                    SOCXSC            3
250800941209     C                   PARM                    CDSXSC            9 0
250900941209     C                   PARM                    MODXSC            3
251000941209     C                   PARM      *BLANKS       RTNXSC            1
251100941209     C                   PARM                    XSOCDS
251200941209     C                   PARM                    KPJBA
251300941209     C*
251400941209     C                   ENDSR
251500941124     C/EJECT
251600941124     C************************************************************
251700941124     C* REPERIMENTO AUTORIZZAZIONI UTENTE
251800941124     C************************************************************
251900941124     C     REPAUT        BEGSR
252000941124     C*
252100941124     C                   CLEAR                   AUTDS
252200941124     C*
252300941124     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
252400941124     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
252500941124     C                   Z-ADD     0             XCAGRP                         GRUPPO
252600941124     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
252700950407    >C                   MOVEL     'ANGSOG'      XCAFNC                         FUNZIONE
252800941124    >C                   MOVEL     *ZERO         XCAVFU                         VARIABILE
252900941124    >C                   MOVE      'CK'          XCATCK                         TIPO CTRL
253000941124    >C                   MOVE      '1'           XCANAU                         RICH.XNOAUT
253100941124     C*
253200941124     C                   CALL      'XCHKAUT'
253300941124     C                   PARM                    AUTDS
253400941124     C*
253500941124     C* errori/non abilitazione
253600941124     C     XCARTN        IFNE      0
253700941124     C                   EXSR      ENDPGM
253800941124     C                   ELSE
253900950410    >C                   MOVEL     XCADDS        GSOGDS
254000941124     C                   ENDIF
254100941124     C*
254200941124     C                   ENDSR
254300940223     C/EJECT
254400940127     C************************************************************
254500940131     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
254600940127     C************************************************************
254700940131     C     REPDFT        BEGSR
254800940127     C*
254900990524B0025C***                MOVEL     XSCSOC        MPESOC
255000990524B0025C**                 MOVEL     DSPGM         MPEPGM
255100990524B0025C**                 MOVEL     KNMUS         MPEPRF
255200990524  "  C* preparo i parametri di chiamata al driver
255300990524  "  C                   CLEAR                   XMPEDS
255400990524  "  C                   EVAL      TpChiamata = '01'
255500990524  "  C                   EVAL      NChiavi = 5
255600990524  "  C                   EVAL      VLogica = 2
255700990524  "  C*
255800990524  "  C* preparo le chiavi per la chiamata al driver
255900990524  "  C                   CLEAR                   XMPEDS1
256000990524  "  C                   MOVEL     XSCSOC        DSMPESOC
256100990524  "  C                   MOVEL     DSPGM         DSMPEPGM
256200040302B0025C                   MOVEL     KNMUS         DSMPEPRF
256300941027     C*Si reperiscono i dati di default a seconda delle modalità con
256400941027     C*cui il pgm è usato
256500941027     C*MPETIP = 'PX'  pgm richiamato come lista (CMD4)
256600941027     C*MPETIP = 'PV'  pgm richiamato da menù
256700941027     C*MPETIP = 'PB'  x lancio Batch
256800941013     C                   SELECT
256900950410     C     OPZ057        WHENEQ    '01'
257000990525b0025C*******            MOVEL     'PX'          MPETIP
257100990525b0025C                   MOVEL     'PX'          dsMPETIP
257200941013     C                   OTHER
257300990525b0025C***                MOVEL     'PV'          MPETIP
257400990525b0025C                   MOVEL     'PV'          dsMPETIP
257500941013     C                   ENDSL
257600990524B0025C**   K4MPE2        CHAIN(N)  ANMPE02L                           21
257700990524 "   C**   *IN21         IFEQ      *OFF
257800990524 "   C**                 MOVEL     MPEPAR        DSMPE
257900990524 "   C* nel caso sia la prima volta che l'utente apre questa anagrafica
258000990524 "   C* i suoi valori di default vengono decisi a pgm
258100990524 "   C**                 ELSE
258200990524 "   C**                 Z-ADD     1             $LOP
258300990524 "   C**                 Z-ADD     1             $ORD
258400990524 "   C**                 ENDIF
258500990524 "   C*
258600990524 "   C                   CALLB     'XMPE'
258700990524 "   C                   PARM                    XMPEDS
258800990524 "   C                   PARM                    XMPEDS1
258900990524 "   C*
259000990524 "   C     Risultato     IFEQ      '0'
259100990524 "   C                   MOVEL     DSMPEPAR      DSMPE
259200990524 "   C* nel caso sia la prima volta che l'utente apre questa anagrafica
259300990524 "   C* i suoi valori di default vengono decisi a pgm
259400990524 "   C                   ELSE
259500990524 "   C                   Z-ADD     1             $LOP
259600990524 "   C                   Z-ADD     1             $ORD
259700990524 "   C*  elenco altri dati default specifici dell'anagrafica
259800040302B0025C                   ENDIF
259900940131     C*
260000940131     C                   ENDSR
260100940223     C/EJECT
260200940131     C************************************************************
260300940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
260400940131     C************************************************************
260500940225     C     DEFOPZ        BEGSR
260600940131     C*
260700940225     C* Questa routine deve valorizzare la schiera $FL1
260800940225     C* con S o N a seconda della utilizzabilità o meno
260900940225     C* dell'opzione correlata
261000940225     C*
261100950301     C* Inizialmente si considerano tutte le opzioni come valide
261200940317     C     1             DO        $DIMOP        X
261300940225     C                   MOVE      'S'           $FL1(X)
261400941027     C                   ENDDO
261500940715     C*
261600940715     C* Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
261700941018     C* con 1 o 0 a seconda della utilizzabilità o meno
261800940715     C* del tasto funzione correlato
261900940715     C*
262000940715     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
262100940715     C* di compilazione
262200940715     C                   RESET                   $FC1
262300940715     C                   RESET                   $FR1
262400940715     C                   RESET                   $FC2
262500940715     C                   RESET                   $FR2
262600940919     C                   RESET                   $FC3
262700940919     C                   RESET                   $FR3
262800940225     C*
262900940225     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
263000940225     C* invalido alcune opzioni
263100940225     C                   SELECT
263200940225     C* Se pgm richiamato in ricerca disattivo :
263300950410    >C     OPZ057        WHENEQ    '01'
263400940603     C* F3=Fine
263500940603     C                   Z-ADD     3             X
263600941216     C                   MOVE      '0'           $FC1(X)
263700941216     C                   MOVE      '0'           $FC2(X)
263800941216     C                   MOVE      '0'           $FC3(X)
263900950301     C*
264000940225     C* Se pgm richiamato in interrogazione disattivo :
264100950410    >C     OPZ057        WHENEQ    '05'
264200940307     C* F10=Immissione
264300940615     C                   Z-ADD     10            X
264400950301     C                   MOVE      '0'           $FC1(X)
264500950301     C                   MOVE      '0'           $FC2(X)
264600940225     C* 1=Selezione
264700940225     C                   Z-ADD     1             X
264800940225     C     01            LOOKUP    $OP1(X)                                21
264900940225     C     *IN21         IFEQ      *ON
265000940225     C                   MOVE      'N'           $FL1(X)
265100940225     C                   ENDIF
265200940225     C* 2=Modifica
265300940225     C                   Z-ADD     1             X
265400940225     C     02            LOOKUP    $OP1(X)                                21
265500940225     C     *IN21         IFEQ      *ON
265600940225     C                   MOVE      'N'           $FL1(X)
265700940225     C                   ENDIF
265800940225     C* 3=Copia
265900940225     C                   Z-ADD     1             X
266000940225     C     03            LOOKUP    $OP1(X)                                21
266100940225     C     *IN21         IFEQ      *ON
266200940225     C                   MOVE      'N'           $FL1(X)
266300940225     C                   ENDIF
266400940225     C* 4=Annullamento
266500940225     C                   Z-ADD     1             X
266600940225     C     04            LOOKUP    $OP1(X)                                21
266700940225     C     *IN21         IFEQ      *ON
266800940225     C                   MOVE      'N'           $FL1(X)
266900940225     C                   ENDIF
267000940225     C*
267100950410     C     OPZ057        WHENEQ    *BLANKS
267200940225     C* Se pgm non richiamato, disattivo subito :
267300941124     C* F8=Selezione seguente
267400941124     C                   Z-ADD     8             X
267500941206     C                   MOVE      '0'           $FC1(X)
267600941206     C                   MOVE      '0'           $FC2(X)
267700940225     C* 1=Selezione
267800940225     C                   Z-ADD     1             X
267900940225     C     01            LOOKUP    $OP1(X)                                21
268000940225     C     *IN21         IFEQ      *ON
268100940225     C                   MOVE      'N'           $FL1(X)
268200940225     C                   ENDIF
268300060308A1150C* F10=Immissione
268400060308  "  C                   Z-ADD     10            X
268500060308  "  C                   MOVE      '0'           $FC1(X)
268600060308  "  C                   MOVE      '0'           $FC2(X)
268700060308  "  C* 1=Selezione
268800060308  "  C                   Z-ADD     1             X
268900060308  "  C     01            LOOKUP    $OP1(X)                                21
269000060308  "  C     *IN21         IFEQ      *ON
269100060308  "  C                   MOVE      'N'           $FL1(X)
269200060308  "  C                   ENDIF
269300060308  "  C* 2=Modifica
269400060308  "  C                   Z-ADD     1             X
269500060308  "  C     02            LOOKUP    $OP1(X)                                21
269600060308  "  C     *IN21         IFEQ      *ON
269700060308  "  C                   MOVE      'N'           $FL1(X)
269800060308  "  C                   ENDIF
269900060308  "  C* 3=Copia
270000060308  "  C                   Z-ADD     1             X
270100060308  "  C     03            LOOKUP    $OP1(X)                                21
270200060308  "  C     *IN21         IFEQ      *ON
270300060308  "  C                   MOVE      'N'           $FL1(X)
270400060308  "  C                   ENDIF
270500060308  "  C* 4=Annullamento
270600060308  "  C                   Z-ADD     1             X
270700060308  "  C     04            LOOKUP    $OP1(X)                                21
270800060308  "  C     *IN21         IFEQ      *ON
270900060308  "  C                   MOVE      'N'           $FL1(X)
271000060308A1150C                   ENDIF
271100940225     C*
271200950301     C* In tutti gli altri casi, disattivo subito :
271300941228     C                   OTHER
271400941228     C* 1=Selezione
271500941228     C                   Z-ADD     1             X
271600941228     C     01            LOOKUP    $OP1(X)                                21
271700941228     C     *IN21         IFEQ      *ON
271800941228     C                   MOVE      'N'           $FL1(X)
271900941228     C                   ENDIF
272000941228     C*
272100940225     C                   ENDSL
272200941118     C*
272300941118     C* poi, disattivo a seconda delle autorizzazioni :
272400950410     C*- manutenzione
272500950410     C*- (in questo caso condiziona anche l'immissione, la copia e
272600950410     C*-  la cancellazione)
272700950410    >C     MANSOG        IFEQ      'N'
272800950410    >C                   Z-ADD     1             X
272900950410    >C     02            LOOKUP    $OP1(X)                                21
273000950410    >C     *IN21         IFEQ      *ON
273100950410    >C                   MOVE      'N'           $FL1(X)
273200950410    >C                   ENDIF
273300950410    >C                   Z-ADD     1             X
273400950410    >C     03            LOOKUP    $OP1(X)                                21
273500950410    >C     *IN21         IFEQ      *ON
273600950410    >C                   MOVE      'N'           $FL1(X)
273700950410    >C                   ENDIF
273800950410    >C                   Z-ADD     1             X
273900950410    >C     04            LOOKUP    $OP1(X)                                21
274000950410    >C     *IN21         IFEQ      *ON
274100950410    >C                   MOVE      'N'           $FL1(X)
274200950410    >C                   ENDIF
274300950410     C                   Z-ADD     10            X
274400950410     C                   MOVE      '0'           $FC1(X)
274500950410     C                   MOVE      '0'           $FC2(X)
274600950410    >C                   ENDIF
274700950410     C*- visualizzazione
274800950410    >C     VISSOG        IFEQ      'N'
274900950410    >C                   Z-ADD     1             X
275000950410    >C     05            LOOKUP    $OP1(X)                                21
275100950410    >C     *IN21         IFEQ      *ON
275200950410    >C                   MOVE      'N'           $FL1(X)
275300950410    >C                   ENDIF
275400950410    >C                   ENDIF
275500940225     C*
275501100831R027 C*
275502100831  "  C*- inserimento
275505100831  "  C                   if        IMMSOG     <> *On     and
275506100831  "  C*- manutenzione
275507100831  "  C                             MANSOG     <> *On     and
275508100831  "  C*- visualizzazione
275509100831  "  C                             VISSOG     <> *On
275510100831  "  C*
275511100831  "  C* disabilito l'opz. 22=Dati aggiuntivi
275512100831  "  C                   Z-ADD     1             X
275513100831  "  C     22            LOOKUP    $OP1(X)                                21
275514100831  "  C     *IN21         IFEQ      *ON
275515100831  "  C                   MOVE      'N'           $FL1(X)
275516100831  "  C                   ENDIF
275517100831  "  C*
275518100831  "  C                   endif
275519100831R027 C*
275600940225     C                   ENDSR
275700940225     C/EJECT
275800940225     C************************************************************
275900940225     C* REPERIMENTO OPZIONI
276000940225     C************************************************************
276100940225     C     REPOPZ        BEGSR
276200940225     C*
276300940314     C     1             DO        $DIMOP        X
276400940128     C*
276500940223     C     $MS1(X)       IFNE      *BLANK
276600940128     C*
276700940223     C                   MOVEL     $MS1(X)       IDMSG
276800940128     C                   CALL      'XRTVM'
276900940201     C                   PARM                    IDMSG            27
277000940201     C                   PARM                    TXTMSG          100
277100940201     C                   PARM                    ERRMSG            1
277200940128     C*
277300940128     C     ERRMSG        IFNE      *ON
277400940223     C                   MOVEL     TXTMSG        $TX1(X)
277500940128     C                   ELSE
277600940223     C                   MOVEL     *ALL'?'       $TX1(X)
277700941027     C                   ENDIF
277800940128     C*
277900941027     C                   ENDIF
278000940128     C*
278100941027     C                   ENDDO
278200940128     C*
278300940128     C                   ENDSR
278400940506     C/EJECT
278500940506     C************************************************************
278600940506     C* REPERIMENTO TASTI DI FUNZIONE
278700940506     C************************************************************
278800940506     C     REPKEY        BEGSR
278900940506     C                   MOVEA     $FM1          $FM
279000950906     C                   MOVEA     $FC1          $FC
279100940506     C                   EXSR      REPKKK
279200940506     C                   MOVEA     $FX           $FX1
279300940506     C*
279400940506     C                   MOVEA     $FM2          $FM
279500950906     C                   MOVEA     $FC2          $FC
279600940506     C                   EXSR      REPKKK
279700940506     C                   MOVEA     $FX           $FX2
279800940919     C*
279900940919     C                   MOVEA     $FM3          $FM
280000950906     C                   MOVEA     $FC3          $FC
280100940919     C                   EXSR      REPKKK
280200940919     C                   MOVEA     $FX           $FX3
280300940919     C                   ENDSR
280400940506     C/EJECT
280500940506     C************************************************************
280600940506     C* REPERIMENTO TASTI DI FUNZIONE
280700940506     C************************************************************
280800940506     C     REPKKK        BEGSR
280900940506     C*
281000940506     C                   CLEAR                   $FX
281100940506     C     1             DO        24            X
281200940506     C*
281300940506     C     $FM(X)        IFNE      *BLANK
281400950906     C     $FC(X)        ANDNE     '0'
281500940506     C*
281600940506     C                   MOVEL     $FM(X)        IDMSG
281700940506     C                   CALL      'XRTVM'
281800940506     C                   PARM                    IDMSG            27
281900940506     C                   PARM                    TXTMSG          100
282000940506     C                   PARM                    ERRMSG            1
282100940506     C*
282200940506     C     ERRMSG        IFNE      *ON
282300940506     C                   MOVEL     TXTMSG        $FX(X)
282400940506     C                   ELSE
282500940506     C                   MOVEL     *ALL' '       $FX(X)
282600941027     C                   ENDIF
282700940506     C*
282800941027     C                   ENDIF
282900940506     C*
283000941027     C                   ENDDO
283100940506     C*
283200940506     C                   ENDSR
283300940223     C/EJECT
283400940128     C************************************************************
283500940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
283600940128     C************************************************************
283700940223     C     DEFCAM        BEGSR
283800940128     C*
283900940223     C* klist
284000940223     C*
284100000105A0732C* ANCPI01L
284200000105  "  C***  K2CPI1        KLIST
284300000105  "  C***                KFLD                    CPICOD
284400000105  "  C***                KFLD                    CPIDTF
284500000105  "  C* ANCPI02L
284600000105  "  C***  K2CPI2        KLIST
284700000105  "  C***                KFLD                    CPIDTF
284800000105A0732C***                KFLD                    CPICOD
284900950407     C*
285000990525b0025C* ANMPE02L
285100990525  "  C***  K4MPE2        KLIST
285200990525  "  C***                KFLD                    MPESOC
285300990525  "  C***                KFLD                    MPEPGM
285400990525  "  C***                KFLD                    MPETIP
285500040302B0025C***                KFLD                    MPEPRF
285600940223     C*
285700940223     C* Variabili per gestione videate
285800940223     C*
285900940223     C                   MOVE      *BLANK        $GEST             2            fmt in gestione
286000940223     C                   MOVE      *BLANK        $FINE             1            fine pgm
286100940223     C                   MOVE      *BLANK        $INZD1            1            inizializzione D1
286200940223     C                   MOVE      *BLANK        $INZS1            1            inizializzione S1
286300940223     C                   MOVE      *BLANK        $INZD2            1            inizializzione D2
286400940223     C                   MOVE      *BLANK        $LASTG            2            fmt di provenienza
286500940223     C                   MOVE      *BLANK        $LASTV            2            ultima emissione
286600940223     C                   MOVE      *BLANK        $EFILE            1            memo fine file
286700940223     C                   MOVE      *BLANK        $ESCI             1            rich.uscita ciclo
286800000105A0732C***                MOVE      *BLANK        $RCDOK            1            rcd letto valido
286900940224     C                   Z-ADD     0             $PIED1            3 0          piede da emett. D1
287000940224     C                   Z-ADD     0             $PIES1            3 0          piede da emett. S1
287100941213     C                   MOVE      *BLANK        $UNIQ             1            gest.POSTA
287200940223     C*
287300940314     C* Variabili appoggio sempre presenti in tutte le anagrafiche
287400940223     C*
287500940223     C     *LIKE         DEFINE    C1RCD         S1NRR
287600940223     C     *LIKE         DEFINE    C1NRR         WSFL
287700940223     C     *LIKE         DEFINE    C1RCD         WMAX
287800940223     C     *LIKE         DEFINE    C1RCD         WPAG
287900940223     C     *LIKE         DEFINE    $INZS1        WINZS1                                     ne S1
288000940608     C     *LIKE         DEFINE    *INKR         WINKR                                      ne S1
288100950407     C     *LIKE         DEFINE    OPR057        WOPR                                       ne S1
288200000105A0732C***  *LIKE         DEFINE    CPIDTF        SAVDTF                                     ne S1
288300000105A0732C     *LIKE         DEFINE    CPIDTFIVL     SAVDTF                                     ne S1
288400940223     C                   MOVE      *BLANK        WWGRP             3
288500940223     C                   Z-ADD     0             CURR              2 0
288600940223     C                   Z-ADD     0             CURC              2 0
288700940223     C                   MOVE      *BLANK        WIN              99
288800940314     C     *LIKE         DEFINE    C1POS         W1POS
288900950419     C     *LIKE         DEFINE    XDTGMA        W1DTF
289000940314     C*
289100940314     C* Variabili appoggio specifiche della singola anagrafica
289200940314     C*
289300940314     C* campi per gestione F4=Ricerca
289400000105A0732C***  *LIKE         DEFINE    CPICOD        WWSOG
289500000105  "  C     *LIKE         DEFINE    CPISOGG       WWSOG
289600000105  "  C*** ex-A0730  *LIKE         DEFINE    CPISTA        WWSTA
289700000105  "  C     *LIKE         DEFINE    CPISTATO      WWSTA
289800000105  "  C*** ex-A0730  *LIKE         DEFINE    CPIPRV        WWPRV
289900000105A0732C     *LIKE         DEFINE    CPIPROV       WWPRV
290000940223     C* campi per parzializzazioni su rcd (solo alfanumerici)
290100000105A0732C***  *LIKE         DEFINE    CPICOD        SOG999
290200000105  "  C*** ex-A0730  *LIKE         DEFINE    CPISTA        STA999
290300000105  "  C*** ex-A0730 *LIKE         DEFINE    CPIPRV        PRV999
290400000105  "  C*** ex-A0730 *LIKE         DEFINE    CPIIVA        IVA999
290500000105A0732C*** ex-A0730 *LIKE         DEFINE    CPICDF        CDF999
290600940223     C*
290700940223     C* Indici
290800940223     C*
290900940223     C                   Z-ADD     0             X                 3 0
291000940223     C                   Z-ADD     0             Y                 3 0
291100940127     C*
291200940117     C                   ENDSR
291300940223     C/EJECT
291301100830R027 C* Aggiunti elementi di schiera:
291302100831 "   C* SDGMSG    *LIBL     SDO0026  (schiera $MS1 )  e
291303100830R027 C* 22S      <Dati aggiuntivi    (schiera $OP1 )
291400950906**
291500940718PROMSG    *LIBL     OPZ0001
291600940225PROMSG    *LIBL     OPZ0002
291700940131PROMSG    *LIBL     OPZ0003
291800940131PROMSG    *LIBL     OPZ0004
291900940131PROMSG    *LIBL     OPZ0005
291901100831SDGMSG    *LIBL     SDO0026        R027
292000940201**
29210094022501S      <Selezione
29220094022502S      <Modifica
29230094022503S      <Copia
29240094022504S      <Annullamento
29250094022505S      <Visualizzazione
29250110083122S      <Dati aggiuntivi          R027
292600940506** TASTI DI FUNZIONE  D1
292700940506PROMSG    *LIBL     KEY0001  01
292800940506PROMSG    *LIBL     KEY0002  02
292900940506PROMSG    *LIBL     KEY0003  03
293000940506PROMSG    *LIBL     KEY0004  04
293100940506PROMSG    *LIBL     KEY0005  05
293200940506PROMSG    *LIBL     KEY0006  06
293300940506PROMSG    *LIBL     KEY0007  07
293400940506PROMSG    *LIBL     KEY0008  08
293500940506PROMSG    *LIBL     KEY0009  09
293600940506PROMSG    *LIBL     KEY0010  10
293700940506PROMSG    *LIBL     KEY0011  11
293800940506PROMSG    *LIBL     KEY0012  12
293900940506PROMSG    *LIBL     KEY0013  13
294000940506PROMSG    *LIBL     KEY0014  14
294100940506PROMSG    *LIBL     KEY0015  15
294200940506PROMSG    *LIBL     KEY0016  16
294300940506PROMSG    *LIBL     KEY0017  17
294400940506PROMSG    *LIBL     KEY0018  18
294500940506PROMSG    *LIBL     KEY0019  19
294600940506PROMSG    *LIBL     KEY0020  20
294700940506PROMSG    *LIBL     KEY0021  21
294800940506PROMSG    *LIBL     KEY0022  22
294900940506PROMSG    *LIBL     KEY0023  23
295000940506PROMSG    *LIBL     KEY0024  24
295100940506**  ABILITAZIONE D1
2952009407200N  01
2953009407200N  02
2954009407201N  03
2955009407201N  04
2956009407200N  05
2957009407200N  06
2958009407200N  07
2959009412061N  08
2960009407200N  09
2961009504140N  10
2962009407200N  11
2963009407201N  12
2964009407200N  13
2965009407200N  14
2966009407200N  15
2967009407200N  16
2968009407200N  17
2969009407201N  18
2970009407200N  19
2971009407200N  20
2972009407200N  21
2973009407200N  22
2974009509060N  23   Impostare a '1' se si utilizzano opzioni
2975009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
297600940916** TASTI DI FUNZIONE  S1
297700940916PROMSG    *LIBL     KEY0001  01
297800940916PROMSG    *LIBL     KEY0002  02
297900940916PROMSG    *LIBL     KEY0003  03
298000940916PROMSG    *LIBL     KEY0004  04
298100940916PROMSG    *LIBL     KEY0005  05
298200940916PROMSG    *LIBL     KEY0006  06
298300940916PROMSG    *LIBL     KEY0007  07
298400940916PROMSG    *LIBL     KEY0008  08
298500940916PROMSG    *LIBL     KEY0009  09
298600940916PROMSG    *LIBL     KEY0010  10
298700940916PROMSG    *LIBL     KEY0011  11
298800940916PROMSG    *LIBL     KEY0012  12
298900940916PROMSG    *LIBL     KEY0013  13
299000940916PROMSG    *LIBL     KEY0014  14
299100940916PROMSG    *LIBL     KEY0015  15
299200940916PROMSG    *LIBL     KEY0016  16
299300940916PROMSG    *LIBL     KEY0017  17
299400940916PROMSG    *LIBL     KEY0018  18
299500940916PROMSG    *LIBL     KEY0019  19
299600940916PROMSG    *LIBL     KEY0020  20
299700940916PROMSG    *LIBL     KEY0021  21
299800940916PROMSG    *LIBL     KEY0022  22
299900940916PROMSG    *LIBL     KEY0023  23
300000940916PROMSG    *LIBL     KEY0024  24
300100940916**  ABILITAZIONE S1
3002009409160N  01
3003009409160N  02
3004009409161N  03
3005009409160N  04
3006009409160N  05
3007009409160N  06
3008009409160N  07
3009009411241N  08
3010009409160N  09
3011009504141N  10
3012009409160N  11
3013009409161N  12
3014009409160N  13
3015009409160N  14
3016009409160N  15
3017009409160N  16
3018009409161N  17
3019009409161N  18
3020009409160N  19
3021009409160N  20
3022009409160N  21
3023009409160N  22
3024009509061N  23   Impostare a '1' se si utilizzano opzioni
3025009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
302600940916** TASTI DI FUNZIONE  D3
302700940506PROMSG    *LIBL     KEY0001  01
302800940506PROMSG    *LIBL     KEY0002  02
302900940506PROMSG    *LIBL     KEY0003  03
303000940506PROMSG    *LIBL     KEY0004  04
303100940506PROMSG    *LIBL     KEY0005  05
303200940506PROMSG    *LIBL     KEY0006  06
303300940506PROMSG    *LIBL     KEY0007  07
303400940506PROMSG    *LIBL     KEY0008  08
303500940506PROMSG    *LIBL     KEY0009  09
303600940506PROMSG    *LIBL     KEY0010  10
303700940506PROMSG    *LIBL     KEY0011  11
303800940506PROMSG    *LIBL     KEY0012  12
303900940506PROMSG    *LIBL     KEY0013  13
304000940506PROMSG    *LIBL     KEY0014  14
304100940506PROMSG    *LIBL     KEY0015  15
304200940506PROMSG    *LIBL     KEY0016  16
304300940506PROMSG    *LIBL     KEY0017  17
304400940506PROMSG    *LIBL     KEY0018  18
304500940506PROMSG    *LIBL     KEY0019  19
304600940506PROMSG    *LIBL     KEY0020  20
304700940506PROMSG    *LIBL     KEY0021  21
304800940506PROMSG    *LIBL     KEY0022  22
304900940506PROMSG    *LIBL     KEY0023  23
305000940506PROMSG    *LIBL     KEY0024  24
305100940916**  ABILITAZIONE D3
3052009407200N  01
3053009407200N  02
3054009407201N  03
3055009407200N  04
3056009407200N  05
3057009407200N  06
3058009407200N  07
3059009407200N  08
3060009407200N  09
3061009409160N  10
3062009407200N  11
3063009407201N  12
3064009407200N  13
3065009407200N  14
3066009407200N  15
3067009407200N  16
3068009409160N  17
3069009409160N  18
3070009407200N  19
3071009407200N  20
3072009407200N  21
3073009407200N  22
3074009509060N  23   Impostare a '1' se si utilizzano opzioni
3075009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
