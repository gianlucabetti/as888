000100981202     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(*new)
000200960202     H*PARMS CVTOPT(*DATETIME)
000300940211     H DECEDIT('0,') DATEDIT(*DMY.)
000400941115      *
000500941130      *           ANAGRAFICA SOGGETTI
000600941130      *          ---------------------
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
003500940128     F*----------------------------------------------------*
003600970630     FANfrn03L  IF   E           K DISK
003700970630     FANcln07L  IF   E           K DISK
003800940211     F*------------
003900940201     FANMPE02L  UF A E           K DISK
004000940211     F*------------
004100941130     FANA040D   CF   E             WORKSTN
004200941130     F                                     RENAME(A040D1:FMTD1)
004300941130     F                                     RENAME(A040D2:FMTD2)
004400941130     F                                     RENAME(A040D3:FMTD3)
004500941130     F                                     RENAME(A040S1:FMTS1)
004600941130     F                                     RENAME(A040C1:FMTC1)
004700941130     F                                     RENAME(A040T1:FMTT1)
004800941130     F                                     RENAME(A040Z1:FMTZ1)
004900941130     F                                     RENAME(A040Z2:FMTZ2)
005000941130     F                                     RENAME(A040Z3:FMTZ3)
005100940131     F                                     SFILE(FMTS1:S1NRR)
005200940201     F                                     INFDS(DSFMT)
005300940128     D*----------------------------------------------------*
005400940302     D* Schiere per la gestione delle opzioni
005500960202     D* dimensione della schiera $MS1, da non cambiare
005600960202     D $DIMOP          C                   CONST(30)
005700960202     D*
005800960202     D $MS1            S             27    DIM($dimop) CTDATA PERRCD(1)
005900960202     D $TX1            S             50    DIM($dimop) ALT($MS1)
006000960202     D $OP1            S              2  0 DIM($dimop) CTDATA PERRCD(1)
006100960202     D $FL1            S              1    DIM($dimop) ALT($OP1)
006200941214     D $TX             S             50    DIM(50)
006300941214     D $FL             S              1    DIM(50)
006400960202     D* schiera contenente i nomi dei programma richiamati attraverso
006500960202     D* le opzioni personalizzate
006600960202     D $PGO1           S             10    DIM($dimop)
006700960202     D*
006800960202     D* Indice della schiera per le opzioni personalizzate
006900960202     D PosOpz          S              2  0
007000960202     D* numero max opzioni
007100960202     D NumOpz          S              2  0 Inz($DIMOP)
007200960202     D* numero effettive opzioni
007300960202     D OpzLen          S              2  0
007400960202     D*-------------
007500940909     D* Schiere generali per la gestione dei tasti di funzione
007600940506     D $FM             S             27    DIM(24)
007700940506     D $FX             S             50    DIM(24)
007800940506     D $FC             S              1    DIM(24)
007900940506     D $FR             S              1    DIM(24)
008000960202     D* Indice della schiera tasti funzione
008100960202     D PosFun          S              2  0
008200960202     D*-------------
008300940909     D* Schiere per la gestione dei tasti di funzione
008400940506     D* Video D1
008500940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
008600940506     D $FX1            S             50    DIM(24) ALT($FM1)
008700940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
008800940506     D $FR1            S              1    DIM(24) ALT($FC1)
008900940506     D* Schiere per la gestione dei tasti di funzione
009000940506     D* Video S1
009100940506     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
009200940506     D $FX2            S             50    DIM(24) ALT($FM2)
009300940506     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
009400940506     D $FR2            S              1    DIM(24) ALT($FC2)
009500960202     D* Schiera contente i nomi dei programmi da richiamare con i tasti
009600960202     D* funzione personalizzata S1
009700960202     D $PGF2           S             10    DIM(25)
009800940916     D* Schiere per la gestione dei tasti di funzione
009900940916     D* Video D2
010000940916     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
010100940916     D $FX3            S             50    DIM(24) ALT($FM3)
010200940916     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
010300940916     D $FR3            S              1    DIM(24) ALT($FC3)
010400950301     D*
010500950301     D KHI             S              2  0 DIM(4)
010600950301     D KHM             S              2  0 DIM(4)
010700940211     D*-------------
010800940207     D* Valori assunti
010900941111     D DSMPE           DS                  INZ
011000940207     D  $LOP                   1      1  0
011100940831     D  $ORD                   2      3  0
011200950301     D  $CHI                  31     38  0
011300950301     D  CHI                   31     38  0
011400950301     D                                     DIM(4)
011500950301     D  $CHM                  39     46  0
011600950301     D  CHM                   39     46  0
011700950301     D                                     DIM(4)
011800950301     D  $ESPI                 47     47
011900950301     D  $ESPM                 48     48
012000940909     D*-------------
012100941026     D* FUNZIONI ESCLUSIVE DI NOSTRADAMUS
012200941201     D*-------------
012300941201     D* Parametri del PGM di test esistenza commenti ANA724R
012400941201     D A724DS        E DS                  EXTNAME(ANA724DS) INZ
012500941201     D  EXI724       E                     INZ('0')
012600941201     D  RIS724       E                     INZ('0')
012700010305a0871x**CMT724       E                     INZ('0')
012800941201     D*-------------
012900941201     D* Parametri del PGM di gestione commenti ANA725R
013000941201     D A725DS        E DS                  EXTNAME(ANA725DS) INZ
013100941201     D  RET725       E                     INZ('0')
013200941201     D  OPR725       E                     INZ('0')
013300941201     D  ERR725       E                     INZ('0')
013400950320     D**-------------
013500950320     D** Parametri del PGM invio posta ANA727R
013600950320     D A727DS        E DS                  EXTNAME(ANA727DS) INZ
013700950320     D  RET727       E                     INZ('0')
013800950320     D  OPR727       E                     INZ('0')
013900950320     D  ERR727       E                     INZ('0')
014000940207     D*-------------
014100940211     D* Passaggio Parametri
014200940211     D KPJBA         E DS
014300940211     D*-------------
014400940225     D* Reperimento Autorizzazioni
014500941124     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
014600941124     D*-------------
014700941124     D* Maschera dati autorizzazioni
014800941202     D GSOGDS        E DS                  EXTNAME(ANGSOGDS)
014900940325     D*-------------
015000940325     D* Parametri in ricezione
015100941130     D A040DS        E DS                  EXTNAME(ANA040DS)
015200160503R224 X*ex R027 -----
015300160503 "   X*ex R027 Parametri del PGM di manutenzione  dati aggiuntivi
015400160503 "   X*ex R027 DGSD16DS     E DS                  inz
015500160503 "   X*ex R027 RETD16       E                     INZ('0')
015600160503 "   X*ex R027 OPRD16       E                     INZ('0')
015700160503 "   X*ex R027 ERRD16       E                     INZ('0')
015800160503 "   X*ex R027 CMTD16       E                     INZ('0')
015900160712R224 X*ex R027 DtFiVlD16    E                     INZ('0001-01-01')
015901160503R224 D*-------------
015902160503 "   D* Parametri del PGM guida dati aggiuntivi anagrafici
015903160503 "   D SDGSD15DS     E DS                  inz
015904160503 "   D  RETD15       E                     INZ('0')
015905160503 "   D  OPRD15       E                     INZ('0')
015906160503 "   D  ERRD15       E                     INZ('0')
015907160503 "   D  CMTD15       E                     INZ('0')
015908160509 "   D  DtFiVlD15    E                     INZ('0001-01-01')
015909160509R224 D  DtParD15     E                     INZ('0001-01-01')
016000940325     D*-------------
016100960202     D* campo di salvataggio per la DS d'ingresso durante la chiamata
016200960202     D* alla funzione personalizzata
016300960202    >D $SaveDS         S                   like(A040DS)
016400960202     D*-------------
016500950404     D* Parametri per filtro
016600950301     D A043DS        E DS                  EXTNAME(ANA043DS) INZ
016700950301     D  CMT043       E                     INZ('0')
016800950301     D  RET043       E                     INZ('0')
016900950301     D  OPR043       E                     INZ('0')
017000950301     D  ERR043       E                     INZ('0')
017100950301     D  EXF043       E                     INZ('0')
017200950301     D  B01043       E                     EXTFLD(EXITCM043)
017300950301     D  E01043       E                     EXTFLD(ERRTCM043)
017400950301     D  B02043       E                     EXTFLD(EXIIND043)
017500950301     D  E02043       E                     EXTFLD(ERRIND043)
017600950303     D  B03043       E                     EXTFLD(EXICLI043)
017700950303     D  E03043       E                     EXTFLD(ERRCLI043)
017800950303     D  B04043       E                     EXTFLD(EXIFOR043)
017900950303     D  E04043       E                     EXTFLD(ERRFOR043)
018000950303     D  B05043       E                     EXTFLD(EXIRCO043)
018100950303     D  E05043       E                     EXTFLD(ERRRCO043)
018200951003     D  OPD043       E                     INZ('0')
018300941130     D*-------------
018400941130     D* Parametri del PGM di lettura SQL
018500941130     D A042DS        E DS                  EXTNAME(ANA042DS)
018600941201     D  RIC042       E                     INZ('0')
018700950428     D*-------------
018800950428     D* Parametri del PGM di manutenzione ANA057R
018900950428     D A057DS        E DS                  EXTNAME(ANA057DS) INZ
019000950428     D  RET057       E                     INZ('0')
019100950428     D  OPR057       E                     INZ('0')
019200950428     D  ERR057       E                     INZ('0')
019300000918     D*-------------
019400000918A0832 * Ds contenente il record letto da ANA057R1
019500000918  "  D ANCPI00f      E DS                  inz
019600000918  "  D*-------------
019700000918  "  D* Parametri per il pgm di lettura ANA057R1
019800000918A0832D ANA057DS1     E DS                  inz
019900950404     D*-------------
020000950404     D* Parametri per gestione contatti
020100950404     D A060DS        E DS                  EXTNAME(ANA060DS) INZ
020200941202     D*-------------
020300941130     D SOGDS         E DS                  EXTNAME(ANSOG00F)
020400941130     D  SOGCOD       E                     EXTFLD(SOGSOGG)
020500941130     D  SOGDAT       E                     EXTFLD(SOGDTENTR)
020600941130     D  SOGDAU       E                     EXTFLD(SOGDTULMO)
020700941130     D  SOGDAI       E                     EXTFLD(SOGDTINVL)
020800941130     D  SOGDAF       E                     EXTFLD(SOGDTFIVL)
020900941130     D  SOGDAN       E                     EXTFLD(SOGDTNASC)
021000941130     D  SOGDES       E                     EXTFLD(SOGDES)
021100941130     D  SOGDE1       E                     EXTFLD(SOGDESAGG)
021200941130     D  SOGCDF       E                     EXTFLD(SOGCDFISC)
021300941130     D  SOGIVA       E                     EXTFLD(SOGPARTIVA)
021400941130     D  SOGFPI       E                     EXTFLD(SOGFRCPIVA)
021500941130     D  SOGCIA       E                     EXTFLD(SOGISCCIAA)
021600941130     D  SOGSES       E                     EXTFLD(SOGSEX)
021700941130     D  SOGNAT       E                     EXTFLD(SOGNATGIUR)
021800941130     D  SOGCOP       E                     EXTFLD(SOGCODPREC)
021900941130     D  SOGABI       E                     EXTFLD(SOGABI)
022000941130     D  SOGCLG       E                     EXTFLD(SOGCLFSOG)
022100941130     D*-------------
022200941130     D INDDS         E DS                  EXTNAME(ANIND00F)
022300941130     D  INDCOD       E                     EXTFLD(INDSOGG)
022400941130     D  INDTII       E                     EXTFLD(INDTPIND)
022500941130     D  INDCOI       E                     EXTFLD(INDCDIND)
022600941130     D  INDDAU       E                     EXTFLD(INDDTULMO)
022700941130     D  INDDAI       E                     EXTFLD(INDDTINVL)
022800941130     D  INDDAF       E                     EXTFLD(INDDTFIVL)
022900941130     D  INDDES       E                     EXTFLD(INDDESFIL)
023000941130     D  INDDE1       E                     EXTFLD(INDDESAGG)
023100941130     D  INDVIA       E                     EXTFLD(INDINDRIZ)
023200941130     D  INDCIT       E                     EXTFLD(INDLOCALIT)
023300941130     D  INDCAP       E                     EXTFLD(INDCAP)
023400941130     D  INDPRV       E                     EXTFLD(INDPROV)
023500941130     D  INDSTA       E                     EXTFLD(INDSTATO)
023600941130     D  INDCAB       E                     EXTFLD(INDCAB)
023700941130     D  INDLIN       E                     EXTFLD(INDLINGUA)
023800941130     D  INDTLX       E                     EXTFLD(INDTELEX)
023900941130     D  INDTEL       E                     EXTFLD(INDTELEFON)
024000941130     D  INDTLF       E                     EXTFLD(INDTELEFAX)
024100941202     D*-------------
024200941202     D* Parametri per ricerca codice ANATB00F
024300941215     D XTABDS        E DS                  INZ
024400941202     D*-------------
024500941209     D* DS esterna provincia
024600941202     D TABC17        E DS                  EXTNAME(ANGC17DS) INZ
024700941202     D*-------------
024800941209     D* DS esterna stato
024900941202     D TABG12        E DS                  EXTNAME(ANGG12DS) INZ
025000950301     D*-------------
025100950301     D* Bottoni
025200950301     D A051DS        E DS                  EXTNAME(ANA051DS) INZ
025300940211     D*-------------
025400940211     D DSFMT           DS           512
025500940506     D  $TASTO               369    369
025600940211     D  NRG                  370    370
025700940211     D  NCL                  371    371
025800940211     D  SFLNRR               378    379B 0
025900940211     D*-------------
026000940211     D* posizione cursore
026100940211     D CURSOR          DS
026200940211     D  RIRI                   1      2B 0 INZ
026300940211     D  R$$                    2      2
026400940211     D  COCO                   3      4B 0 INZ
026500940211     D  C$$                    4      4
026600940127     D*-------------
026700940127     D* Reperimento nome PGM
026800940127     D STATUS         SDS           333
026900940127     D  DSPGM            *PROC
027000950301     D  PARMS            *PARMS
027100940207     D*-------------
027200940616     D* Costanti
027300940616     D* nr.di righe del sfl (SFLPAG)
027400941130     D SFLPAG          C                   CONST(12)
027500941117     D* data a 0
027600941117     D $DT0            C                   CONST('0001-01-01')
027700950428     D* data massima
027800950428     D $DT9            C                   CONST('9999-12-31')
027900000119A0735D***** ex-A0732   $DT2039         C          CONST('2039-12-31')
028000940616     D*-------------
028100940506     D* Tasti di funzione
028200940506     D F01             C                   CONST(X'31')
028300940506     D F02             C                   CONST(X'32')
028400940506     D F03             C                   CONST(X'33')
028500940506     D F04             C                   CONST(X'34')
028600940506     D F05             C                   CONST(X'35')
028700940506     D F06             C                   CONST(X'36')
028800940506     D F07             C                   CONST(X'37')
028900940506     D F08             C                   CONST(X'38')
029000940506     D F09             C                   CONST(X'39')
029100940506     D F10             C                   CONST(X'3A')
029200940506     D F11             C                   CONST(X'3B')
029300940506     D F12             C                   CONST(X'3C')
029400940506     D F13             C                   CONST(X'B1')
029500940506     D F14             C                   CONST(X'B2')
029600940506     D F15             C                   CONST(X'B3')
029700940506     D F16             C                   CONST(X'B4')
029800940506     D F17             C                   CONST(X'B5')
029900940506     D F18             C                   CONST(X'B6')
030000940506     D F19             C                   CONST(X'B7')
030100940506     D F20             C                   CONST(X'B8')
030200940506     D F21             C                   CONST(X'B9')
030300940506     D F22             C                   CONST(X'BA')
030400940506     D F23             C                   CONST(X'BB')
030500940506     D F24             C                   CONST(X'BC')
030600940506     D ENTER           C                   CONST(X'F1')
030700940506     D ROLDWN          C                   CONST(X'F4')
030800940506     D ROLLUP          C                   CONST(X'F5')
030900941104     D*-------------
031000941104     D* Dati di ambiente ottenuti da XSOC
031100941130     D SOC001        E DS                  EXTNAME(XSOC001DS)
031200941209     D*-------------
031300941130     D* DS Interna per dati di output di XSOC
031400941104     D XSOCDS          DS          1000
031500970217     D*-------------
031600970217     D* Parametri gestione clienti e fornitori
031700970217     D P020DS        E DS                  EXTNAME(ANP020DS)
031800970217     d* salvataggio parametri
031900970217     D P020DSCLI       S                   LIKE(P020DS)
032000970217     D P020DSFOR       S                   LIKE(P020DS)
032100970217     D*-------------
032200970217     D* Parametri per programma di interfaccia altri sistemi informativi
032300970217     D Ana000ds      E DS
032400970217     D*-------------
032500970217     D XPAOUT          DS          2000
032600950301     D*-------------
032700950301     D* Attributi video
032800950301     D* (definisco come costanti qugli attributi video che potrei
032900950301     D*  testare e quindi preferisco assegnargli un valore "parlante")
033000950301     D*  bottone presente
033100950301     D PRES            C                   CONST(X'A1')
033200950301     D*  fornitore annullato
033300950301     D ANNUL           C                   CONST(X'A1')
033400950301     D*  fornitore abilitato in società di riferimento
033500950301     D ABIL            C                   CONST(X'A2')
033600000114A0735D ZDateISO        S               D
033700000918A0832D* Flag per lettura soggetti con ANA057R1
033800000918  "  D Prima           S              1
033900000918A0832D DFOld           S              1
034000020625B0453 *----------------------------------------------------*
034100020625  "   * Controllo chiamata ricorsiva
034200020625  "  D X48ChPgmDS    E DS                  INZ
034300020625B0453 *----------------------------------------------------*
034400940302     I*
034500940211     I/EJECT
034600940128     I*----------------------------------------------------*
034700950301     I*-------------
034800950301     IFMTD2
034900950301     I              D2ANAI                      KHI(1)
035000950301     I              D2INDI                      KHI(2)
035100950301     I              D2CDCI                      KHI(3)
035200950301     I              D2TCMI                      KHI(4)
035300950301     I              D2ANAM                      KHM(1)
035400950301     I              D2INDM                      KHM(2)
035500950301     I              D2CDCM                      KHM(3)
035600950301     I              D2TCMM                      KHM(4)
035700950301     I*-------------
035800940207     I*-------------
035900940127     I*
036000940127     C*----------------------------------------------------*
036100940127     C*                MAIN LINE PROGRAM
036200940127     C*----------------------------------------------------*
036300940127     C*
036400940223     C* inizializzazione variabili
036500940223     C                   EXSR      INZVAR
036600940223     C*
036700940223     C     $FINE         DOWEQ     *OFF
036800940131     C     $GEST         CASEQ     'D1'          GESD1                          Parzializz.
036900940131     C     $GEST         CASEQ     'S1'          GESS1                          Lista
037000940131     C     $GEST         CASEQ     'D2'          GESD2                          Valori ass.
037100941027     C                   ENDCS
037200941027     C                   ENDDO
037300940325     C* fine programma
037400940325     C                   EXSR      ENDPGM
037500940225     C*
037600940325     C/EJECT
037700940325     C************************************************************
037800940325     C* FINE PROGRAMMA
037900940325     C************************************************************
038000940325     C     ENDPGM        BEGSR
038100940722     C*
038200940722     C* aggiorno valori assunti
038300941116     C* se NON sono entrato con chiave univoca salvo
038400941130     C     $UNIQ         IFEQ      *OFF
038500940722     C                   EXSR      AGGMPE
038600941013     C                   ENDIF
038700940325     C*
038800940325     C* passaggio dati al pgm chiamante
038900941130     C                   MOVEL     WOPR          OPR040                                     ne S1
039000941130     C                   MOVEL     A040DS        KPJBU
039100940325     C*
039200940325     C* Se pgm Guida non è stato richiamato posso chiudere in LR
039300941130     C     OPZ040        IFEQ      *BLANK
039400940325     C                   SETON                                            LR
039500940325     C* altrimenti chiudo in RT
039600940325     C                   ELSE
039700940325     C                   SETON                                            RT
039800940325     C                   ENDIF
039900940616     C                   RETURN
040000940325     C*
040100940325     C                   ENDSR
040200940224     C/EJECT
040300940201     C************************************************************
040400940201     C* RICERCHE
040500940201     C************************************************************
040600940127     C     SEARCH        BEGSR
040700940127     C*
040800940127     C* determino Riga/Colonna del cursore
040900940127     C                   MOVE      NRG           R$$
041000940127     C                   MOVE      NCL           C$$
041100940127     C                   Z-ADD     RIRI          CURR
041200940127     C                   Z-ADD     COCO          CURC
041300940127     C*
041400941118   B1C                   SELECT
041500940127     C*
041600941130     C* FMTD1  - STATO
041700941130     C     H1NMFL        WHENEQ    'D1STA'
041800941215     C                   RESET                   XTABDS
041900941202     C                   MOVE      '01'          XTAOPZ
042000941202     C                   MOVE      XSCSOC        XTAAZI
042100941202     C                   MOVE      XSCLIN        XTALIN
042200941215     C                   Z-ADD     CURR          XTARIG
042300941215     C                   Z-ADD     CURC          XTACOL
042400941202     C                   CLEAR                   KPJBU
042500941202     C                   MOVEL     XTABDS        KPJBU
042600941202     C                   CALL      'ANGG12R'
042700941202     C                   PARM                    KPJBA
042800941202     C                   MOVEL     KPJBU         XTABDS
042900941202     C     XTAERR        IFNE      '1'
043000941202     C                   MOVE      XTAKEY        WWSTA
043100941202     C                   ELSE
043200941202     C                   MOVE      *BLANK        WWSTA
043300941202     C                   ENDIF
043400940127     C*
043500941130     C     WWSTA         IFNE      *BLANK
043600941130     C                   MOVEL     WWSTA         D1STA
043700941130     C                   ENDIF
043800940127     C* imposto pos. del cursore
043900940127     C                   Z-ADD     CURR          H1RIGA
044000940127     C                   Z-ADD     CURC          H1COLO
044100941130     C*
044200941130     C* FMTD1  - PROVINCIA
044300941130     C     H1NMFL        WHENEQ    'D1PRV'
044400941215     C                   RESET                   XTABDS
044500941202     C                   MOVE      '01'          XTAOPZ
044600941202     C                   MOVE      XSCSOC        XTAAZI
044700941202     C                   MOVE      XSCLIN        XTALIN
044800941215     C                   Z-ADD     CURR          XTARIG
044900941215     C                   Z-ADD     CURC          XTACOL
045000941202     C                   CLEAR                   KPJBU
045100941202     C                   MOVEL     XTABDS        KPJBU
045200941202     C                   CALL      'ANGC17R'
045300941202     C                   PARM                    KPJBA
045400941202     C                   MOVEL     KPJBU         XTABDS
045500941202     C     XTAERR        IFNE      '1'
045600941202     C                   MOVE      XTAKEY        WWPRV
045700941202     C                   ELSE
045800941202     C                   MOVE      *BLANK        WWPRV
045900941202     C                   ENDIF
046000941130     C*
046100941130     C     WWPRV         IFNE      *BLANK
046200941130     C                   MOVEL     WWPRV         D1PRV
046300941130     C                   ENDIF
046400941130     C* imposto pos. del cursore
046500941130     C                   Z-ADD     CURR          H1RIGA
046600941130     C                   Z-ADD     CURC          H1COLO
046700940223     C*
046800940223     C* ricerca non abilitata in questa posizione
046900941118   X1C                   OTHER
047000940929     C* imposto pos. del cursore
047100940929     C                   Z-ADD     CURR          H1RIGA
047200940929     C                   Z-ADD     CURC          H1COLO
047300940929     C                   SETON                                        9899
047400940127     C*
047500941118   E1C                   ENDSL
047600940127     C*
047700940506     C                   MOVE      *HIVAL        $LASTV
047800940127     C                   ENDSR
047900940131     C/EJECT
048000940224     C************************************************************
048100940131     C* RIEMPIMENTO OPZIONI
048200940224     C************************************************************
048300940131     C     RIEOPZ        BEGSR
048400940223     C*
048500940223     C* Input :
048600940223     C* - $TX = schiera con tutti i testi disponibili
048700940223     C* - $FL = schiere con i flag di validità associati ai testi
048800940225     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
048900940223     C*          opzione usato
049000940223     C* - WMXO = variabile contenente il numero massimo di testi
049100940223     C*          opzione usabili
049200940721     C* - $L01 = Lunghezza prima riga
049300940721     C* - $L02 = Lunghezza seconda riga
049400940223     C* Output :
049500940223     C* - WOPZ1 = variabile contenente la prima riga opzioni
049600940223     C* - WOPZ2 = variabile contenente la seconda riga opzioni
049700941213     C* - FF23  = flag che indica se c'è bisogno del tasto F23
049800940223     C*
049900961003     C                   CALLb     'X04OPMSG'
050000940223     C* input
050100940223     C                   PARM                    $TX
050200940223     C                   PARM                    $FL
050300940225     C                   PARM                    WCOZ              3 0
050400940223     C                   PARM                    WMXO              3 0
050500940223     C* output
050600940223     C                   PARM                    WOPZ1            76
050700940223     C                   PARM                    WOPZ2            76
050800941213     C                   PARM                    FF23              1
050900940721    >C* input
051000941220    >C                   PARM      76            $L01              3 0
051100941220    >C                   PARM      76            $L02              3 0
051200940223     C*
051300060224A1148C                   PARM                    $ALLOPT         960
051400060224A1148C*
051500940131     C                   ENDSR
051600940506     C/EJECT
051700940506     C************************************************************
051800940506     C* RIEMPIMENTO TASTI FUNZIONE
051900940506     C************************************************************
052000940506     C     RIEKEY        BEGSR
052100940506     C*
052200940506     C* Input :
052300940506     C* - $FX = schiera con tutti i testi disponibili
052400940506     C* - $FC = schiere con i flag di validità associati ai testi
052500940506     C* - $FR = schiere con i flag di REVERSE IMAGE
052600940506     C* - $ULK = ultimo testo caricato
052700940721     C* - $L01 = Lunghezza prima riga
052800940721     C* - $L02 = Lunghezza seconda riga
052900940506     C* Output :
053000940506     C* - $ULK
053100940609     C* - $KEY1= 1° riga di tasti funzione
053200940609     C* - $KEY2= 2° riga di tasti funzione
053300940506     C*
053400961003     C                   CALLb     'XKEYMSG'
053500940506     C* input
053600940506     C                   PARM                    $FX
053700940506     C                   PARM                    $FC
053800940506     C                   PARM                    $FR
053900940506     C                   PARM                    $ULK              3 0
054000940506     C* output
054100940506     C                   PARM                    $KEY1            79
054200940506     C                   PARM                    $KEY2            79
054300940721    >C* input
054400940721    >C                   PARM                    $L01              3 0
054500940721    >C                   PARM                    $L02              3 0
054600940506     C*
054700060224A1148C                   PARM                    $ALLFUNCT       480
054800060224A1148C*
054900940506     C                   ENDSR
055000940224     C/EJECT
055100940224     C************************************************************
055200940224     C* GESTIONE EMISSIONE TASTI FUNZIONE
055300940224     C************************************************************
055400940224     C     GESPIE        BEGSR
055500940224     C*
055600940224     C                   SELECT
055700940224     C* Parzializzazioni
055800940224     C     $GEST         WHENEQ    'D1'
055900940506     C     $ULKD1        IFEQ      0
056000940506     C* riempimento opzioni
056100940506     C                   EXSR      F24D1
056200940506     C                   ELSE
056300940506     C                   WRITE     FMTZ1
056400940506     C                   ENDIF
056500940506     C* Lista
056600940506     C     $GEST         WHENEQ    'S1'
056700940506     C     $ULKS1        IFEQ      0
056800940506     C                   EXSR      F24S1
056900940506     C                   ELSE
057000940506     C                   WRITE     FMTZ2
057100940506     C                   ENDIF
057200940916     C* Modifica valori assunti
057300940916     C     $GEST         WHENEQ    'D2'
057400940916     C     $ULKD2        IFEQ      0
057500940916     C                   EXSR      F24D2
057600940916     C                   ELSE
057700940916     C                   WRITE     FMTZ3
057800940916     C                   ENDIF
057900940506     C                   ENDSL
058000940506     C*
058100940224     C                   ENDSR
058200940224     C/EJECT
058300940224     C************************************************************
058400940224     C* GESTIONE PARZIALIZZAZIONI
058500940224     C************************************************************
058600940224     C     GESD1         BEGSR
058700940224     C* inizializzazione videata
058800940223     C     $INZD1        IFEQ      *ON
058900940127     C                   EXSR      INZD1
059000940223     C                   MOVE      *OFF          $INZD1
059100941027     C                   ENDIF
059200940223     C* emissione piede videata se proveniente da altra
059300940223     C* salvataggio valore formato di provenienza
059400940202     C     $LASTV        IFNE      'D1'
059500940223     C* salvo stato F17 per sapere cosa fare in caso di F12
059600960202     C                   MOVE      *INKR         WINKR             1
059700940202     C                   WRITE     FMTT1
059800940224     C                   EXSR      GESPIE
059900940202     C                   MOVE      $LASTV        $LASTG
060000940202     C                   MOVE      'D1'          $LASTV
060100940707     C                   ENDIF
060200940707     C*
060300940707     C* se esistono errori sulla videata
060400940707     C* emetto la write del formato a indicatori spenti per vedere
060500940707     C* le eventuali decodifiche
060600940707     C     *IN99         IFEQ      *ON
060700940707     C                   MOVEA     *IN           WIN
060800940707     C                   MOVE      *ALL'0'       IN5098           49
060900940707     C                   MOVEA     IN5098        *IN(50)
061000940707     C                   WRITE     FMTD1
061100940707     C                   MOVEA     WIN           *IN
061200940707     C                   ENDIF
061300940131     C*
061400940131     C                   EXFMT     FMTD1
061500940506     C* controllo abilitazione tasti funzione
061600940506     C*
061700950313     C* memorizzo l'avvenuta EXFMT per richiamo a pgm di sola finestra
061800950313     C                   MOVEL     '1'           WEXF
061900000919A0832C*
062000000919A0832C                   eval      Prima = *blank
062100950313     C* controllo abilitazione tasti funzione
062200950313     C*
062300940506     C                   EXSR      CTRKD1
062400940506     C*
062500940506     C     *IN99         IFEQ      *OFF
062600940131     C*
062700940117     C                   SELECT
062800940916     C* F2=Decodifica
062900940916     C     $TASTO        WHENEQ    F02
063000940916     C                   EXSR      F02D1
063100940131     C* F3=Fine
063200940506     C     $TASTO        WHENEQ    F03
063300940307     C                   EXSR      F03D1
063400940131     C* F4=Ricerca
063500940506     C     $TASTO        WHENEQ    F04
063600940127     C                   EXSR      SEARCH
063700940916     C* F5=Refresh
063800940916     C     $TASTO        WHENEQ    F05
063900940916     C                   EXSR      F05D1
064000941206     C* F8=Successivo
064100941206     C     $TASTO        WHENEQ    F08
064200941206     C                   EXSR      F08D1
064300950306     C* F10=Immissione
064400950306     C     $TASTO        WHENEQ    F10
064500950306     C                   EXSR      F10D1
064600940131     C* F12=Ritorno
064700940506     C     $TASTO        WHENEQ    F12
064800940307     C                   EXSR      F12D1
064900940131     C* F18=Modifica valori assunti
065000940506     C     $TASTO        WHENEQ    F18
065100940307     C                   EXSR      F18D1
065200940224     C* F24=Altri tasti
065300940506     C     $TASTO        WHENEQ    F24
065400940309     C                   EXSR      F24D1
065500940131     C*
065600940117     C                   OTHER
065700940127     C                   EXSR      CTRD1
065800940127     C     *IN99         IFEQ      *OFF
065900940131     C* definisco l'ordinamento
066000940131     C                   SELECT
066100941130     C* << nel caso di più viste logiche e quindi più campi di
066200941130     C*    parzializzazione, qui vanno definiti i criteri per
066300941130     C*    determinare quale VL va letta, a seconda di quali campi
066400941130     C*    sono stati valorizzati >>
066500950310     C     D1COD         WHENNE    *BLANK
066600950310     C     $UNIQ         ANDNE     *ON
066700950310     C                   Z-ADD     7             $ORD
066800941130     C     D1DES         WHENNE    *BLANK
066900981127     C***  D1STR1        ANDEQ     NO
067000940131     C                   Z-ADD     1             $ORD
067100941130     C     D1CIT         WHENNE    *BLANK
067200981127     C***  D1STR2        ANDEQ     NO
067300940131     C                   Z-ADD     2             $ORD
067400941130     C     D1STA         WHENNE    *BLANK
067500941130     C                   Z-ADD     3             $ORD
067600941130     C     D1PRV         WHENNE    *BLANK
067700941130     C                   Z-ADD     4             $ORD
067800941130     C     D1IVA         WHENNE    *BLANK
067900941130     C                   Z-ADD     5             $ORD
068000941130     C     D1CDF         WHENNE    *BLANK
068100941130     C                   Z-ADD     6             $ORD
068200950310     C*Se la ricerca è per stringa e non esiste nessun altro
068300950310     C*ordinamento allora assumo come ordinamento 1
068400981127     C**   D1DES         WHENNE    *BLANK
068500981127     C**   D1STR1        ANDEQ     SI
068600981127     C**                 Z-ADD     1             $ORD
068700981127     C**   D1CIT         WHENNE    *BLANK
068800981127     C**   D1STR2        ANDEQ     SI
068900981127     C**                 Z-ADD     2             $ORD
069000941104     C* imposto l'ordinamento 99 per ottenere in selezione solo il
069100941104     C* rcd con la chiave passata
069200941130     C     $UNIQ         WHENEQ    *ON
069300940831     C                   Z-ADD     99            $ORD
069400940131     C* nel caso non sia stato digitato nulla uso l'ordinamento
069500940131     C* dei valori assunti
069600940131     C                   ENDSL
069700950310     C*
069800940127     C                   MOVE      'S1'          $GEST
069900940223     C                   MOVE      *ON           $INZS1
070000940302     C                   MOVE      *BLANK        W1POS
070100940211     C                   ENDIF
070200940224     C*
070300940117     C                   ENDSL
070400940224     C*
070500940506     C                   ENDIF
070600940117     C                   ENDSR
070700940224     C/EJECT
070800940224     C************************************************************
070900940224     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
071000940224     C************************************************************
071100940127     C     INZD1         BEGSR
071200940318     C*
071300940318     C                   CLEAR                   FMTD1
071400950308     C* Valorizzo costanti a video SI o NO
071500950308     C                   MOVE      XSCON         D1ON1
071600950308     C                   MOVE      XSCOFF        D1OF1
071700950308     C                   MOVE      XSCON         D1ON2
071800950308     C                   MOVE      XSCOFF        D1OF2
071900990708A0684C                   MOVE      XSCON         D1ON3
072000990708A0684C                   MOVE      XSCOFF        D1OF3
072100030807A1020C                   MOVE      XSCON         D1ON4
072200030807A1020C                   MOVE      XSCOFF        D1OF4
072300040723B0733c* se  esiste JRCM accendo indicatore 17
072400040723  "  c                   if        EsisteJCRM = *on
072500040723  "  c                   seton                                        17
072600040723B0733c                   endif
072700941013     C* Se pgm  richiamato con parametri forniti , li imposto nelle
072800941013     C*    parzializzazioni
072900941104     C                   SELECT
073000941104     C* il pgm è richiamato e
073100941104     C* è stata passata la chiave univoca
073200941130     C     $UNIQ         WHENEQ    *ON
073300941213     C                   MOVEL     UQ1040        D1COD
073400950320     C                   MOVEL     NO            D1STR1
073500950320     C                   MOVEL     NO            D1STR2
073600990708A0684C                   MOVEL     NO            D1ANN
073700030807A1020C                   MOVEL     NO            D1SPR
073800941104     C* il pgm è richiamato e
073900941104     C* sono stati passati dei valori di parzializzazione ma non
074000941104     C* la chiave univoca
074100941130     C     OPZ040        WHENNE    *BLANK
074200941130     C* imposto come default i parametri passati
074300941130     C                   MOVEL     DES040        D1DES
074400941130     C                   MOVEL     NO            D1STR1
074500941130     C                   MOVEL     CIT040        D1CIT
074600941130     C                   MOVEL     NO            D1STR2
074700941130     C                   MOVEL     STA040        D1STA
074800941130     C                   MOVEL     PRV040        D1PRV
074900941130     C                   MOVEL     IVA040        D1IVA
075000941130     C                   MOVEL     CDF040        D1CDF
075100941130     C                   MOVEL     COD040        D1COD
075200990709A0684C                   MOVEL     NO            D1ANN
075300030807A1020C                   MOVEL     NO            D1SPR
075400990708     C* il pgm non è richiamato
075500990709     C     OPZ040        WHENEQ    *BLANK
075600941130     C* imposto dei valori di default
075700941130     C                   MOVEL     NO            D1STR1
075800941130     C                   MOVEL     NO            D1STR2
075900990708A0684C                   MOVEL     NO            D1ANN
076000030807A1020C                   MOVEL     NO            D1SPR
076100941115     C                   ENDSL
076200940224     C*
076300940117     C                   ENDSR
076400940506     C/EJECT
076500940506     C************************************************************
076600940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
076700940609     C* INPUT
076800940509     C* $FC1 = Abilitazioni videata D!
076900940509     C* OUTPUT
077000940509     C* *IN97 *IN99 Errori
077100940506     C************************************************************
077200940506     C     CTRKD1        BEGSR
077300940506     C                   SETOFF                                       99
077400940506     C                   MOVEA     $FC1          $FC
077500940506     C                   EXSR      CTRKEY
077600940506     C     *IN99         IFEQ      *ON
077700940506     C                   SETON                                        97
077800940506     C                   ENDIF
077900940506     C                   ENDSR
078000940506     C/EJECT
078100940506     C************************************************************
078200940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA S1
078300940506     C************************************************************
078400940506     C     CTRKS1        BEGSR
078500940506     C                   SETOFF                                       99
078600940506     C                   MOVEA     $FC2          $FC
078700940506     C                   EXSR      CTRKEY
078800940506     C     *IN99         IFEQ      *ON
078900940506     C                   SETON                                        97
079000940506     C                   ENDIF
079100940506     C                   ENDSR
079200940506     C/EJECT
079300940506     C************************************************************
079400940506     C* CONTROLLO ABILITAZIONE TASTI
079500940509     C* INPUT
079600940609     C* $TASTO =Tasto funzione premuto
079700940509     C* $FC = Tasti abilitati
079800940509     C* OUTPUT
079900940509     C* IN99 = ERrore
080000940506     C************************************************************
080100940506     C     CTRKEY        BEGSR
080200940506     C                   SELECT
080300940506     C     $TASTO        WHENEQ    F01
080400960202     C                   z-add     01            posfun
080500940720     C     $FC(1)        IFEQ      '0'
080600940506     C                   SETON                                        99
080700940506     C                   ENDIF
080800940506     C     $TASTO        WHENEQ    F02
080900960202     C                   z-add     02            posfun
081000940720     C     $FC(2)        IFEQ      '0'
081100940506     C                   SETON                                        99
081200940506     C                   ENDIF
081300940506     C     $TASTO        WHENEQ    F03
081400960202     C                   z-add     03            posfun
081500940720     C     $FC(3)        IFEQ      '0'
081600940506     C                   SETON                                        99
081700940506     C                   ENDIF
081800940506     C     $TASTO        WHENEQ    F04
081900960202     C                   z-add     04            posfun
082000940720     C     $FC(4)        IFEQ      '0'
082100940506     C                   SETON                                        99
082200940506     C                   ENDIF
082300940506     C     $TASTO        WHENEQ    F05
082400960202     C                   z-add     05            posfun
082500940720     C     $FC(5)        IFEQ      '0'
082600940506     C                   SETON                                        99
082700940506     C                   ENDIF
082800940506     C     $TASTO        WHENEQ    F06
082900960202     C                   z-add     06            posfun
083000940720     C     $FC(6)        IFEQ      '0'
083100940506     C                   SETON                                        99
083200940506     C                   ENDIF
083300940506     C     $TASTO        WHENEQ    F07
083400960202     C                   z-add     07            posfun
083500940720     C     $FC(7)        IFEQ      '0'
083600940506     C                   SETON                                        99
083700940506     C                   ENDIF
083800940506     C     $TASTO        WHENEQ    F08
083900960202     C                   z-add     08            posfun
084000940720     C     $FC(8)        IFEQ      '0'
084100940506     C                   SETON                                        99
084200940506     C                   ENDIF
084300940615     C     $TASTO        WHENEQ    F09
084400960202     C                   z-add     09            posfun
084500940720     C     $FC(09)       IFEQ      '0'
084600940506     C                   SETON                                        99
084700940506     C                   ENDIF
084800940506     C     $TASTO        WHENEQ    F10
084900960202     C                   z-add     10            posfun
085000940720     C     $FC(10)       IFEQ      '0'
085100940506     C                   SETON                                        99
085200940506     C                   ENDIF
085300940506     C     $TASTO        WHENEQ    F11
085400960202     C                   z-add     11            posfun
085500940720     C     $FC(11)       IFEQ      '0'
085600940506     C                   SETON                                        99
085700940506     C                   ENDIF
085800940506     C     $TASTO        WHENEQ    F12
085900960202     C                   z-add     12            posfun
086000940720     C     $FC(12)       IFEQ      '0'
086100940506     C                   SETON                                        99
086200940506     C                   ENDIF
086300940506     C     $TASTO        WHENEQ    F13
086400960202     C                   z-add     13            posfun
086500940720     C     $FC(13)       IFEQ      '0'
086600940506     C                   SETON                                        99
086700940506     C                   ENDIF
086800940506     C     $TASTO        WHENEQ    F14
086900960202     C                   z-add     14            posfun
087000940720     C     $FC(14)       IFEQ      '0'
087100940506     C                   SETON                                        99
087200940506     C                   ENDIF
087300940506     C     $TASTO        WHENEQ    F15
087400960202     C                   z-add     15            posfun
087500940720     C     $FC(15)       IFEQ      '0'
087600940506     C                   SETON                                        99
087700940506     C                   ENDIF
087800940506     C     $TASTO        WHENEQ    F16
087900960202     C                   z-add     16            posfun
088000940720     C     $FC(16)       IFEQ      '0'
088100940506     C                   SETON                                        99
088200940506     C                   ENDIF
088300940506     C     $TASTO        WHENEQ    F17
088400960202     C                   z-add     17            posfun
088500940720     C     $FC(17)       IFEQ      '0'
088600940506     C                   SETON                                        99
088700940506     C                   ENDIF
088800940506     C     $TASTO        WHENEQ    F18
088900960202     C                   z-add     18            posfun
089000940720     C     $FC(18)       IFEQ      '0'
089100940506     C                   SETON                                        99
089200940506     C                   ENDIF
089300940506     C     $TASTO        WHENEQ    F19
089400960202     C                   z-add     19            posfun
089500940720     C     $FC(19)       IFEQ      '0'
089600940506     C                   SETON                                        99
089700940506     C                   ENDIF
089800940506     C     $TASTO        WHENEQ    F20
089900960202     C                   z-add     20            posfun
090000940720     C     $FC(20)       IFEQ      '0'
090100940506     C                   SETON                                        99
090200940506     C                   ENDIF
090300940506     C     $TASTO        WHENEQ    F21
090400960202     C                   z-add     21            posfun
090500940720     C     $FC(21)       IFEQ      '0'
090600940506     C                   SETON                                        99
090700940506     C                   ENDIF
090800940506     C     $TASTO        WHENEQ    F22
090900960202     C                   z-add     22            posfun
091000940720     C     $FC(22)       IFEQ      '0'
091100940506     C                   SETON                                        99
091200940506     C                   ENDIF
091300940506     C     $TASTO        WHENEQ    F23
091400960202     C                   z-add     23            posfun
091500940720     C     $FC(23)       IFEQ      '0'
091600940506     C                   SETON                                        99
091700940506     C                   ENDIF
091800940506     C     $TASTO        WHENEQ    F24
091900960202     C                   z-add     24            posfun
092000940720     C     $FC(24)       IFEQ      '0'
092100940506     C                   SETON                                        99
092200940506     C                   ENDIF
092300940506     C                   ENDSL
092400940506     C                   ENDSR
092500940916     C/EJECT
092600940916     C************************************************************
092700940916     C* GESTIONE F02 VIDEO D1
092800940916     C************************************************************
092900940916     C     F02D1         BEGSR
093000940916     C*
093100940916     C* per decodificare la videata richiamo la routine dei ctrl
093200940916     C* e poi disattivo gli eventuali messaggi di errore
093300940916     C                   EXSR      CTRD1
093400940916     C*
093500940916     C                   MOVE      *ALL'0'       IN5098
093600940916     C                   MOVEA     IN5098        *IN(50)
093700940916     C*
093800940916     C                   ENDSR
093900940224     C/EJECT
094000940131     C************************************************************
094100940307     C* GESTIONE F03 VIDEO D1
094200940131     C************************************************************
094300940307     C     F03D1         BEGSR
094400940131     C*
094500940307     C                   MOVE      *ON           $FINE
094600941130     C                   MOVE      '1'           RET040
094700940325     C* fine programma
094800940325     C                   EXSR      ENDPGM
094900940307     C*
095000940307     C                   ENDSR
095100940916     C/EJECT
095200940916     C************************************************************
095300940916     C* GESTIONE F05 VIDEO D1
095400940916     C************************************************************
095500940916     C     F05D1         BEGSR
095600940916     C*
095700940916     C                   MOVE      *ON           $INZD1
095800940916     C*
095900940916     C                   ENDSR
096000941206     C/EJECT
096100941206     C************************************************************
096200941206     C* GESTIONE F08 VIDEO D1
096300941206     C************************************************************
096400941206     C     F08D1         BEGSR
096500941206     C*
096600941206     C                   MOVE      *ON           $FINE
096700941206     C                   MOVE      '0'           RET040
096800941206     C*
096900941206     C                   ENDSR
097000950306     C/EJECT
097100950306     C************************************************************
097200950306     C* GESTIONE F10 VIDEO D1
097300950306     C* AGGIUNTA RECORD
097400950306     C************************************************************
097500950306     C     F10D1         BEGSR
097600950306     C*
097700950306     C                   RESET                   A043DS
097800950306     C                   MOVEL     '01'          OPZ043
097900950306     C     CMT040        IFEQ      '1'
098000950306     C                   MOVEL     '1'           CMT043
098100950306     C                   ELSE
098200950306     C                   MOVEL     '0'           CMT043
098300950306     C                   ENDIF
098400950306     C                   MOVEL     WEXF          EXF043
098500950306     C                   MOVEL     $CHI          CHI043
098600950306     C                   MOVEL     $CHM          CHM043
098700950306     C                   MOVEL     $ESPI         ESI043
098800950306     C                   MOVEL     $ESPM         ESM043
098900950306     C                   Z-ADD     CURR          RIG043
099000950306     C                   Z-ADD     CURC          COL043
099100950306     C*
099200950306     C* i bottoni non li passo perchè ovviamente non ci sono
099300950306     C*
099400950306     C                   MOVEL     A043DS        KPJBU
099500950306     C*
099600970509     C                   CALL      'ANA043R'                            21
099700950306     C                   PARM                    KPJBA
099800950306     C                   PARM                    AUTDS
099900950306     C*
100000950306     C                   MOVEL     KPJBU         A043DS
100100950306     C* ritorno da PGM gestione
100200950306     C                   EXSR      RTN043
100300950306     C*
100400950306     C                   MOVE      *HIVAL        $LASTV
100500950306     C*
100600950306     C                   ENDSR
100700940307     C/EJECT
100800940307     C************************************************************
100900940307     C* GESTIONE F12 VIDEO D1
101000940307     C************************************************************
101100940307     C     F12D1         BEGSR
101200940307     C*
101300940307     C*  se la 1a videata è la lista oppure sono arrivato qui con F17,
101400940307     C*  per F12 torno alla lista
101500940307     C     $LOP          IFEQ      2
101600940307     C     WINKR         OREQ      *ON
101700940307     C                   MOVE      'S1'          $GEST
101800940307     C*  altrimenti, per F12 torno a menu
101900940307     C                   ELSE
102000940307     C                   MOVE      *ON           $FINE
102100941130     C                   MOVE      '2'           RET040
102200941027     C                   ENDIF
102300940307     C*
102400940307     C                   ENDSR
102500940307     C/EJECT
102600940307     C************************************************************
102700940307     C* GESTIONE F18 VIDEO D1
102800940307     C************************************************************
102900940307     C     F18D1         BEGSR
103000940307     C*
103100940307     C                   MOVE      'D2'          $GEST
103200940307     C                   MOVEL     *ON           $INZD2
103300940307     C*
103400940307     C                   ENDSR
103500940307     C/EJECT
103600940307     C************************************************************
103700940307     C* GESTIONE F24 VIDEO D1
103800940609     C* input
103900940509     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
104000940509     C* $FX1 =Testi tasti funzione
104100940509     C* $FC1 = Abilitazioni
104200940509     C* $FR1 = Inversioni di fondo
104300940307     C************************************************************
104400940307     C     F24D1         BEGSR
104500940506     C*
104600940506     C                   MOVEA     $FX1          $FX
104700940506     C                   MOVEA     $FC1          $FC
104800940506     C                   MOVEA     $FR1          $FR
104900940506     C                   Z-ADD     $ULKD1        $ULK
105000940929     C                   Z-ADD     79            $L01
105100940929     C                   Z-ADD     79            $L02
105200940506     C                   EXSR      RIEKEY
105300940506     C                   Z-ADD     $ULK          $ULKD1            3 0
105400940720     C                   MOVEA     $FC           $FC1
105500940506     C                   MOVEL     $KEY1         Z1KE1
105600060224A1148C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
105700940506     C                   MOVEL     $KEY2         Z1KE2
105800940506     C                   WRITE     FMTZ1
105900940506     C*
106000940307     C*
106100940307     C                   ENDSR
106200940307     C/EJECT
106300940307     C************************************************************
106400940307     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
106500940307     C************************************************************
106600940307     C     CTRD1         BEGSR
106700940307     C*
106800940127     C                   SETOFF                                       99
106900940127     C* non imposto pos. del cursore
107000940127     C                   Z-ADD     0             H1RIGA
107100940127     C                   Z-ADD     0             H1COLO
107200940131     C*
107300940314    >C* << elencare gli eventuali controlli sui campi di
107400940314    >C*    parzializzazione, ricordando che quasi sempre essi
107500940314    >C*    non ne avranno perchè determinano solo il range di
107600940314    >C*    scrittura della lista e quindi (nel caso di campi
107700940314    >C*    alfanumerici) saranno trattati come chiavi parziali >>
107800940131     C*
107900950310     C*
108000950310     C* ALLINEO SOGGETTO
108100961003     C                   CALLb     'XALLINEA'
108200950310     C                   PARM                    D1COD
108300950310     C                   PARM      8             LEN               5 0
108400950310     C                   PARM                    XSCINP
108500950310     C*
108600990708A0684C     D1ANN         IFNE      SI
108700990708A0684C     D1ANN         ANDNE     NO
108800990708A0684C                   SETON                                        6499
108900990708A0684C                   ENDIF
109000990708     C*
109100950308     C     D1STR1        IFNE      SI
109200950308     C     D1STR1        ANDNE     NO
109300950308     C                   SETON                                        6299
109400950308     C                   ENDIF
109500950308     C*
109600950308     C     D1STR2        IFNE      SI
109700950308     C     D1STR2        ANDNE     NO
109800950308     C                   SETON                                        6399
109900950308     C                   ENDIF
110000030807A1020C*
110100030807  "  C     D1SPR         IFNE      SI
110200030807  "  C     D1SPR         ANDNE     NO
110300030807  "  C                   SETON                                        6599
110400030807A1020C                   ENDIF
110500950308     C*
110600940117     C                   ENDSR
110700940224     C/EJECT
110800940127     C************************************************************
110900940131     C* GESTIONE LISTA
111000940127     C************************************************************
111100940127     C     GESS1         BEGSR
111200940223     C* inizializzazione videata
111300940223     C     $INZS1        IFEQ      *ON
111400940127     C                   EXSR      INZS1
111500940223     C                   MOVE      *OFF          $INZS1
111600940127     C                   ENDIF
111700940223     C* emissione piede videata se proveniente da altra
111800940223     C* salvataggio valore formato di provenienza
111900940202     C     $LASTV        IFNE      'S1'
112000940202     C                   WRITE     FMTT1
112100940224     C                   EXSR      GESPIE
112200940202     C                   MOVE      $LASTV        $LASTG
112300940202     C                   MOVE      'S1'          $LASTV
112400941027     C                   ENDIF
112500940223     C*
112600940223     C     WMAX          IFEQ      0
112700940211     C                   WRITE     FMTD3                                        NO RCD
112800940211     C                   ENDIF
112900940127     C*
113000960202     C* Inizializzazione indice tasti funzione personalizzati
113100960202     C                   Eval      PosFun = 25
113200960202     C*
113300940131     C                   EXFMT     FMTC1
113400940204     C     C1NRR         IFNE      0
113500940204     C                   Z-ADD     C1NRR         WSFL
113600940204     C                   ENDIF
113700940127     C                   Z-ADD     SFLNRR        C1RCD
113800950301     C* memorizzo l'avvenuta EXFMT per richiamo a pgm di sola finestra
113900950301     C                   MOVEL     '1'           WEXF
114000950320     C*Serve per emissione finestra pgm invio anA727r (per open/close
114100950320     C*window)
114200950320     C                   MOVE      '1'           WIN727            1
114300940506     C*
114400940506     C* controllo abilitazione tasti funzione
114500940506     C*
114600940506     C                   EXSR      CTRKS1
114700941205     C*
114800940506     C     *IN99         IFEQ      *OFF
114900940127     C*
115000941027     C                   SELECT
115100940127     C* F3=Fine
115200940506     C     $TASTO        WHENEQ    F03
115300940309     C                   EXSR      F03S1
115400941206     C* F8=Successivo
115500941206     C     $TASTO        WHENEQ    F08
115600941206     C                   EXSR      F08S1
115700940131     C* F10=Immissione
115800940506     C     $TASTO        WHENEQ    F10
115900940309     C                   EXSR      F10S1
116000940131     C* F12=Ritorno
116100940506     C     $TASTO        WHENEQ    F12
116200940309     C                   EXSR      F12S1
116300940131     C* F17=Parzializzazioni
116400940506     C     $TASTO        WHENEQ    F17
116500940309     C                   EXSR      F17S1
116600940131     C* F18=Modifica valori assunti
116700940506     C     $TASTO        WHENEQ    F18
116800940309     C                   EXSR      F18S1
116900940224     C* F23=Altre opzioni
117000940506     C     $TASTO        WHENEQ    F23
117100940309     C                   EXSR      F23S1
117200940224     C* F24=Altri tasti
117300940506     C     $TASTO        WHENEQ    F24
117400940309     C                   EXSR      F24S1
117500940127     C* ROLL-UP
117600940506     C     $TASTO        WHENEQ    ROLLUP
117700940127     C                   EXSR      ROLS1
117800940127     C*
117900960202     C* Personalizzata
118000960202     C     $PGF2(PosFun) Whenne    *BLANK
118100960202     C                   EXSR      FperS1
118200960202     C*
118300941027     C                   OTHER
118400940127     C* CONTROLLO DATI
118500940131     C                   EXSR      CTRC1
118600940201     C     *IN99         IFEQ      *OFF
118700940131     C                   EXSR      CTRS1
118800941027     C                   ENDIF
118900941205     C*
119000941027     C                   ENDSL
119100940127     C*
119200940506     C                   ENDIF
119300941205     C*
119400940127     C                   ENDSR
119500940224     C/EJECT
119600960202     C************************************************************
119700960202     C* Chiamata programma con tasto personalizzato
119800960202     C************************************************************
119900960202     C     FPerS1        BEGSR
120000960202     C*
120100960202     C* salvo la DS di ingresso
120200960202    >C                   MOVEL     A040DS        $SaveDS
120300960202     C*
120400960202    >C                   MOVEL     A040DS        KPJBU
120500960202    >C                   CALL      $PGF2(PosFun)                        22
120600960202     C                   PARM                    KPJBA
120700960202     C                   PARM                    NOMPGM
120800960202     C*
120900960202     C* ripristino la DS di ingresso
121000960202    >C                   MOVEL     $SaveDS       A040DS
121100960202     C*
121200960202     C                   ENDSR
121300960202     C/EJECT
121400940127     C************************************************************
121500940131     C* INIZIALIZZAZIONE LISTA
121600940127     C************************************************************
121700940127     C     INZS1         BEGSR
121800940302     C* pulizia SFL
121900940128     C                   SETOFF                                         3031
122000940131     C                   WRITE     FMTC1
122100940128     C                   SETON                                          31
122200940127     C*
122300940128     C* riempimento opzioni
122400940223     C                   MOVEA     $TX1          $TX
122500940223     C                   MOVEA     $FL1          $FL
122600940309     C                   Z-ADD     0             WCOZ
122700940225     C*  equivale al num.di elementi della schiera $TX1
122800960202     C                   Z-ADD     OPZLEN        WMXO
122900940128     C                   EXSR      RIEOPZ
123000941213     C                   Z-ADD     23            X
123100941213     C                   MOVE      FF23          $FC2(X)
123200940608     C                   MOVEL     WOPZ1         C1OPZ1
123300060224A1148C                   MOVEL     $ALLOPT       H1ALLOPT
123400940608     C                   MOVEL     WOPZ2         C1OPZ2
123500940225     C                   Z-ADD     WCOZ          C1COZ
123600000918A0832C*
123700000918A0832C                   eval      DFOld = *Off
123800000918     C*
123900940128     C* CARICAMENTO SFL A PAGINE
124000940128     C*
124100940201     C                   Z-ADD     0             S1NRR
124200940128     C                   Z-ADD     0             WMAX
124300940224     C*
124400940224     C* Posizionamento su file pilota
124500940302     C                   CLEAR                   C1POS
124600941130     C                   EXSR      SETSOG
124700940302     C* ripristino i valori del posizionamento
124800940302     C                   MOVE      W1POS         C1POS
124900940131     C*
125000940201     C* Lettura file pilota
125100941130     C                   EXSR      REDSOG
125200000918A0832 *
125300000918  "   * Se non è stato letto nessun soggetto e l'ordinamento è per
125400000918  "   * partita IVA o codice fiscale cerco eventuali soggetti per cui
125500000918  "   * i dati fiscali inseriti in parzializzazione siano ormai
125600000918  "   * "scaduti"
125700000918  "  C                   if        $EFile = *On and
125800000918  "  C                             (($Ord = 5 or $Ord = 6) and
125900000918  "  C                              (D1IVA<>*blank or D1CdF<>*blank))
126000000918  "  C                   reset                   ANA057DS1
126100000918  "  C                   eval      Prima0571 = *On
126200000918  "  C                   eval      Prima = *On
126300000918  "  C                   exsr      RedOld
126400000918  "  C                   endif
126500000918A0832 *
126600940131     C*
126700940131     C* Carico 1a pagina
126800050608D1830C* se arrivo dalla parzializzazione devo caricare solo la prima pagina
126900050608  "  C                   IF        $LastV = 'D1'
127000050608  "  C                   Z-ADD     SFLPAG        WPAG
127100050608D1830C                   ENDIF
127200940127     C                   EXSR      ROLS1
127300940201     C                   Z-ADD     SFLPAG        WPAG
127400940127     C*
127500940127     C                   ENDSR
127600940127     C/EJECT
127700940127     C************************************************************
127800940131     C* CARICAMENTO PAGINA LISTA
127900940127     C************************************************************
128000940127     C     ROLS1         BEGSR
128100940127     C*
128200940128     C                   SETOFF                                       32
128300940223     C                   Z-ADD     0             Y
128400940127     C                   Z-ADD     WMAX          S1NRR
128500940127     C*
128600940127     C* Leggo dal file anagrafico per caricare la lista
128700940127     C*
128800000918A0832 * Se l'ordinamento è per partita IVA o per codice fiscale, una
128900000918  "   * volta finito di leggere i soggetti per cui i dati fiscali
129000000918  "   * inseriti in parzializzazione sono "attuali" leggo quelli per
129100000918  "   * cui sono "scaduti"
129200000918  "   *
129300000918  "  X***  $EFILE        DOWEQ     *OFF
129400000918  "  X***  Y             ANDLT     WPAG
129500000918  "   *
129600000918  "  C                   dow       ($EFile = *Off                or
129700000918  "  C                              (($Ord = 5 or $Ord = 6) and
129800000918  "  C                               (D1IVA<>*blank or D1CdF<>*blank)
129900000918  "  C                               and Rec0571 <> *On))        and
130000000918A0832C                             Y < WPag
130100940127     C*
130200000918A0832C                   if        $EFile = *Off or Rec0571 = *Off
130300940131     C                   EXSR      RIES1
130400940127     C*
130500940127     C                   ADD       1             S1NRR
130600940127     C                   ADD       1             Y
130700940127     C*
130800940131     C                   WRITE     FMTS1
130900000918A0832C                   endif
131000940131     C*
131100000918A0832C*
131200000918A0832 * se sto ancora leggendo i rcd con dati fiscali "attuali"...
131300941201     C* richiedo al pgm di lettura il rcd successivo
131400000918A0832C                   if        $EFile = *Off
131500941201     C                   MOVE      '1'           RIC042
131600941201     C*
131700941130     C                   EXSR      REDSOG
131800000918A0832C*
131900000918  "  C* altrimenti, se sto leggendo i soggetti con dati fiscali
132000000918  "  C* "scaduti"
132100000918  "  C                   else
132200000918  "   *
132300000918  "   * richiedo al pgm di lettura il primo rcd oppure i rcd seguenti
132400000918  "  C                   if        Prima = *blank
132500000918  "  C                   eval      Prima = *On
132600000918  "  C                   else
132700000918  "  C                   eval      Prima = *Off
132800000918  "  C                   endif
132900000918  "   *
133000000918  "  C                   reset                   ANA057DS1
133100000918  "  C                   eval      Prima0571 = Prima
133200000918  "   *
133300000918  "  C                   exsr      RedOld
133400000918  "  C*
133500000918A0832C                   endif
133600940128     C*
133700941027     C                   ENDDO
133800000918A0832C*
133900000918  "  C* se sono stati letti tutti i soggetti con dati fiscali
134000000918  "  C* "scaduti" riazzero le variabili necessarie per la lettura di
134100000918  "  C* tali soggetti
134200000919  "  C                   if        DFOld = *On and
134300000918  "  C                             Rec0571 = *On and $EFile = *On
134400000918  "  C                   reset                   ANA057DS1
134500000918  "  C                   eval      Prima = *blank
134600000918A0832C                   endif
134700940127     C*
134800940223     C                   Z-ADD     S1NRR         WMAX                 30
134900940127     C*
135000940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
135100940127     C*
135200940201     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
135300940127     C                   MVR                     RESTO             3 0
135400940131     C     PAGINE        MULT      SFLPAG        C1RCD
135500941027     C     RESTO         IFGT      0
135600940127     C                   ADD       1             C1RCD
135700941027     C                   ELSE
135800940407     C                   SUB       SFLPAG        C1RCD
135900940407     C                   ADD       1             C1RCD
136000941027     C                   ENDIF
136100940128     C*
136200940127     C                   ENDSR
136300940224     C/EJECT
136400940224     C************************************************************
136500940224     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
136600940224     C************************************************************
136700941130     C     SETSOG        BEGSR
136800940224     C*
136900941201     C                   RESET                   A042DS
137000941213     C*
137100941213     C                   MOVEL     D1DES         DES042
137200941213     C                   MOVEL     D1CIT         CIT042
137300941213     C                   MOVEL     D1STA         STA042
137400941213     C                   MOVEL     D1PRV         PRV042
137500941213     C                   MOVEL     D1IVA         IVA042
137600941213     C                   MOVEL     D1COD         COD042
137700941213     C                   MOVEL     D1CDF         CDF042
137800941201     C*
137900941130     C* Gestione posizionamento
138000941130     C* Se il campo W1POS è > *blank imposto il posizionamento con
138100941130     C* W1POS altrimenti lo imposto con il valore di parametrizzazione
138200940224     C                   SELECT
138300941130     C* Ord.1 = ragione sociale
138400940224     C     $ORD          WHENEQ    1
138500941213     C     W1POS         ANDNE     *BLANK
138600941213     C                   MOVEL     W1POS         POS042
138700941130     C* Ord.2 = località
138800941201     C     $ORD          WHENEQ    2
138900941213     C     W1POS         ANDNE     *BLANK
139000941213     C                   MOVEL     W1POS         POS042
139100941130     C* Ord.3 = stato
139200941201     C     $ORD          WHENEQ    3
139300941213     C     W1POS         ANDNE     *BLANK
139400941213     C                   MOVEL     W1POS         POS042
139500941130     C* Ord.4 = provincia
139600941201     C     $ORD          WHENEQ    4
139700941213     C     W1POS         ANDNE     *BLANK
139800941213     C                   MOVEL     W1POS         POS042
139900941130     C* Ord.5 = partita iva
140000941201     C     $ORD          WHENEQ    5
140100941213     C     W1POS         ANDNE     *BLANK
140200941213     C                   MOVEL     W1POS         POS042
140300941130     C* Ord.6 = codice fiscale
140400941201     C     $ORD          WHENEQ    6
140500941213     C     W1POS         ANDNE     *BLANK
140600941213     C                   MOVEL     W1POS         POS042
140700941130     C* Ord.7 = soggetto
140800941201     C     $ORD          WHENEQ    7
140900941213     C     W1POS         ANDNE     *BLANK
141000941213     C                   MOVEL     W1POS         POS042
141100940831     C* Ord.99= Chiave univoca
141200941104     C* (quando viene gestita la chiave univoca NON deve essere gestito
141300941104     C*  il posizionamento C1POS/W1POS)
141400940831     C     $ORD          WHENEQ    99
141500940224     C                   ENDSL
141600940224     C*
141700941130     C* completo la preparazione dei campi in input al pgm ANA042R
141800941205     C     D1STR1        IFEQ      NO
141900941205     C                   MOVE      *OFF          ST1042
142000941205     C                   ELSE
142100941205     C                   MOVE      *ON           ST1042
142200941205     C                   ENDIF
142300941205     C     D1STR2        IFEQ      NO
142400941205     C                   MOVE      *OFF          ST2042
142500941205     C                   ELSE
142600941205     C                   MOVE      *ON           ST2042
142700941205     C                   ENDIF
142800941130     C                   MOVE      $ORD          ORD042
142900941201     C* richiedo al pgm di lettura il 1° rcd con la conseguente
143000941201     C* inizializzazione delle variabili SQL
143100941201     C                   MOVE      '0'           RIC042
143200941130     C*
143300940224     C                   ENDSR
143400940128     C/EJECT
143500940128     C************************************************************
143600940131     C* LETTURA RCD ARCHIVIO PILOTA
143700940128     C************************************************************
143800941130     C     REDSOG        BEGSR
143900940128     C*
144000940131     C                   MOVEL     *OFF          $EFILE
144100940131     C                   MOVEL     *OFF          $RCDOK
144200000918A0832C*
144300000918  "   * Segnalo che sto leggendo un rcd con dati fiscali "attuali"
144400000918  "  C                   if        ($Ord = 5 or $Ord = 6) and
144500000918  "  C                             (D1IVA<>*blank and D1CdF<>*blank)
144600000918  "  C                   eval      DFOld = *Off
144700000918  "  C                   endif
144800000918A0832C*
144900000918     C*
145000941027     C     $EFILE        DOUEQ     *ON
145100990709     C     $RCDOK        OREQ      *ON
145200940131     C*
145300941130     C                   CALL      'ANA042R'
145400941201     C                   PARM                    A042DS
145500941130     C                   PARM                    SOGDS
145600941130     C                   PARM                    INDDS
145700940131     C*
145800941130     C                   MOVE      END042        $EFILE
145900940131     C*
146000941027     C     $EFILE        IFEQ      *OFF
146100941130     C                   EXSR      SELSOG
146200941027     C                   ENDIF
146300940131     C*
146400941027     C                   ENDDO
146500940131     C*
146600941213     C                   MOVE      $EFILE        *IN33                          SflEnd
146700940131     C*
146800940131     C                   ENDSR
146900940224     C/EJECT
147000000918A0832C************************************************************
147100000918  "  C* LETTURA SOGGETTI CON DATI FISCALI SCADUTI
147200000918  "   * Se l'ordinamento è per partita IVA o per codice fiscale, dopo
147300000918  "   * aver reperito i soggetti con dasti fiscali "attuali" reperisco
147400000918  "   * quelli per cui questi dati fiscali sono stati variati
147500000918  "  C************************************************************
147600000918A0832C     REDOLD        BEGSR
147700000918      *
147800000918      * Segnalo che sto leggendo un rcd con dati fiscali "scaduti"
147900000918     C                   eval      DFOld = *On
148000000918     C*
148100000918     C                   clear                   kpjbu
148200000918     C                   reset                   ANCPI00F
148300000919     C                   if        D1Cod <> *blank
148400000919     C                   eval      Sog0571 = D1Cod
148500000919     C                   endif
148600000919     C                   if        D1Sta <> *blank
148700000919     C                   eval      Stato0571 = D1Sta
148800000919     C                   endif
148900000919     C                   if        D1Prv <> *blank
149000000919     C                   eval      Prov0571 = D1Prv
149100000919     C                   endif
149200000919     C                   if        D1IVA <> *blank
149300000918     C                   eval      PIva0571 = D1IVA
149400000919     C                   endif
149500000919     C                   if        D1CdF <> *blank
149600000918     C                   eval      CdFisc0571 = D1CdF
149700000919     C                   endif
149800000918     C                   CALL      'XDT4000'
149900000918     C     DtFivl0571    PARM                    XDTAMG           10
150000000918     C                   PARM      ZDate         XDTGMA            6 0
150100000918     C                   PARM      4             XDTSTA            1 0
150200000918      *
150300000918     C                   movel     ANA057DS1     kpjbu
150400000918      *
150500000918     C                   call      'ANA057R1'
150600000918     C                   parm                    kpjba
150700000918     C                   parm                    ANCPI00F
150800000918      *
150900000918     C                   movel     kpjbu         ANA057DS1
151000000918      *
151100000918     C                   move      Rec0571       *in33
151200000918      *
151300000918A0832C                   ENDSR
151400940224     C************************************************************
151500940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
151600940224     C************************************************************
151700940224     C     CLCPAG        BEGSR
151800940224     C* Input :
151900940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
152000940224     C*          cursore
152100940224     C* - SFLPAG = numero rcd per pagina sfl
152200940224     C* Output :
152300940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
152400940224     C*
152500940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
152600940224     C                   MVR                     RESTO             3 0
152700940224     C     RESTO         IFGT      0
152800940224     C                   ADD       1             PAGINE
152900940224     C                   ENDIF
153000940224     C     PAGINE        MULT      SFLPAG        WPAG
153100940224     C*
153200940224     C                   ENDSR
153300940131     C/EJECT
153400940131     C************************************************************
153500940131     C* SELEZIONI SULLA RIGA LETTA
153600940314     C* impostare a *OFF il campo $RCDOK per scartare un record che
153700940314     C* non rientra nei parametri di parzializzazione
153800940314     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
153900940314     C* superiore stabilito nei parametri
154000940314     C*
154100940131     C************************************************************
154200941130    >C     SELSOG        BEGSR
154300940131     C*
154400990713     C                   MOVEL     *ON           $RCDOK
154500990713     C*
154600990713A0684C* Scarto rcd se annullato se desiderata lista senza annullati
154700990713  "  C     D1Ann         IFEQ      NO
154800060131A0684C*
154900060131B0847X***  sogdaf        IFLE      WDATE
155000060131B0847C     sogdaf        IFLT      WDATE
155100060131A0684C     sogdaf        ANDNE     $DT0
155200990713  "  C                   MOVEL     *OFF          $RCDOK
155300990713  "  C* richiedo al pgm di lettura il rcd successivo
155400990713  "  C                   MOVE      '1'           RIC042
155500990713  "  C                   ENDIF
155600990713A0684C                   ENDIF
155700030807A1020C*
155800030807  "  C* Scarto rcd con natura giuridica se chiesti solo provvisori
155900030807  "  C     D1Spr         IFEQ      SI
156000030807  "  C*
156100030807  "  C     SogNat        IFNE      *Blanks
156200030807  "  C                   MOVEL     *OFF          $RCDOK
156300030807  "  C* richiedo al pgm di lettura il rcd successivo
156400030807  "  C                   MOVE      '1'           RIC042
156500030807  "  C                   ENDIF
156600030807A1020C                   ENDIF
156700940202     C*
156800940202     C                   ENDSR
156900940131     C/EJECT
157000940131     C************************************************************
157100940131     C* RIEMPIMENTO RIGA LISTA
157200940131     C************************************************************
157300940131     C     RIES1         BEGSR
157400940131     C*
157500940131     C                   CLEAR                   FMTS1
157600941130     C* riempio i campi del sfl, compresi gli hidden con i valori
157700941130     C* del rcd letto
157800941130     C*
157900941130     C                   MOVEL     SOGCOD        S1COD
158000941130     C                   MOVEL     SOGDES        S1DES
158100961003     C                   CALLb     'XDT4000'
158200941130     C                   PARM      SOGDAF        XDTAMG           10
158300941130     C                   PARM                    XDTGMA            6 0
158400941130     C                   PARM      2             XDTSTA            1 0
158500941130     C                   MOVEL     XDTGMA        S1DAF
158600941130     C                   MOVEL     INDCIT        S1CIT
158700941130     C                   MOVEL     INDSTA        S1STA
158800941130     C                   MOVEL     INDPRV        S1PRV
158900941130     C                   MOVEL     SOGIVA        S1IVA
159000000918     C                   MOVEL     SOGCDF        S1CDF
159100000920A0835X***                setoff                                       41
159200000920A0835C                   eval      H1Inv = *Off
159300000920A0832 *
159400000918  "  C* Se il soggetto ha dati fiscali "scaduti"
159500000918  "  C                   if        DFOld = *On and
159600000918  "  C                             Rec0571 = *Off
159700000918  "  C                   MOVEL     CPISogg       S1COD
159800000918  "  C                   MOVEL     CPIDesSog1    S1DES
159900000918  "  C                   CALLb     'XDT4000'
160000000918  "  C                   PARM      CPIDtFiVl     XDTAMG           10
160100000918  "  C                   PARM                    XDTGMA            6 0
160200000918  "  C                   PARM      2             XDTSTA            1 0
160300000918  "  C                   MOVEL     XDTGMA        S1DAF
160400000918  "  C                   MOVEL     CPILocalit    S1CIT
160500000918  "  C                   MOVEL     CPIStato      S1STA
160600000918  "  C                   MOVEL     CPIProv       S1PRV
160700000918  "  C                   MOVEL     CPIPartIva    S1IVA
160800000920A0832C                   MOVEL     CPICdFisc     S1CDF
160900000920A0835X***                seton                                        41
161000000920A0835C                   eval      H1Inv = *On
161100000920A0832C                   endif
161200000918A0832 *
161300000918     C*
161400950301     C* reperisco se nota presente
161500950301     C                   RESET                   A724DS
161600950301     C                   MOVEL     '01'          OPZ724
161700950301     C                   MOVEL     XSCSOC        SOC724
161800950301     C                   MOVEL     *BLANK        UNI724
161900950301     C                   MOVEL     'SOG'         TPA724
162000000918A0832X***                MOVEL     SOGCOD        KY1724
162100000918A0832C                   MOVEL     S1Cod         KY1724
162200950301     C                   MOVEL     WDATE         DTS724
162300950301     C                   MOVEL     A724DS        KPJBU
162400950301     C                   CALL      'ANA724R'
162500950301     C                   PARM                    KPJBA
162600950301     C                   MOVEL     KPJBU         A724DS
162700950301     C*
162800950301     C* memorizzo lo stato del bottone TCM
162900950301     C                   MOVE      EXI724        H1B01
163000950301     C                   MOVE      *ZERO         H1E01
163100950301     C*
163200950301     C* memorizzo lo stato del bottone IND
163300950301     C*
163400950301     C                   RESET                   A051DS
163500950301     C                   MOVEL     XSCSOC        SOC051
163600950301     C                   MOVEL     S1COD         SOG051
163700950301     C                   MOVEL     A051DS        KPJBU
163800950301     C                   CALL      'ANA054R'
163900950301     C                   PARM                    KPJBA
164000950301     C                   MOVEL     KPJBU         A051DS
164100950301     C*
164200950301     C                   MOVE      EXI051        H1B02
164300950302     C                   MOVE      *ZERO         H1E02
164400950301     C*
164500950301     C* memorizzo lo stato del bottone CDC
164600950301     C                   RESET                   A051DS
164700950301     C                   MOVEL     XSCSOC        SOC051
164800950301     C                   MOVEL     S1COD         SOG051
164900950301     C                   MOVEL     A051DS        KPJBU
165000950303     C                   CALL      'ANA053R'
165100950301     C                   PARM                    KPJBA
165200950301     C                   MOVEL     KPJBU         A051DS
165300950301     C*
165400950303     C                   MOVE      EXI051        H1B04
165500950303     C                   MOVE      *ZERO         H1E04
165600950303     C*
165700950303     C                   RESET                   A051DS
165800950303     C                   MOVEL     XSCSOC        SOC051
165900950303     C                   MOVEL     S1COD         SOG051
166000950303     C                   MOVEL     A051DS        KPJBU
166100950303     C                   CALL      'ANA055R'
166200950303     C                   PARM                    KPJBA
166300950303     C                   MOVEL     KPJBU         A051DS
166400950303     C*
166500950301     C                   MOVE      EXI051        H1B03
166600950301     C                   MOVE      *ZERO         H1E03
166700950303     C*
166800950303     C                   RESET                   A051DS
166900950303     C                   MOVEL     XSCSOC        SOC051
167000950303     C                   MOVEL     S1COD         SOG051
167100950303     C                   MOVEL     A051DS        KPJBU
167200950303     C                   CALL      'ANA056R'
167300950303     C                   PARM                    KPJBA
167400950303     C                   MOVEL     KPJBU         A051DS
167500950303     C*
167600950303     C                   MOVE      EXI051        H1B05
167700950303     C                   MOVE      *ZERO         H1E05
167800950301     C*
167900990713A0684C* azzero le memorizzazioni degli attributi non bottoni
168000990708A0684C                   MOVEL     *ZERO         H1Ann
168100950301     C*
168200000918A0832C* Se il soggetto ha dati fiscali "attuali"...
168300990708A0684C* testo se annullato
168400000918A0832C                   if        DFOld = *Off
168500000918A0832 *
168600060131B0847x***  sogdaf        IFLE      WDATE
168700060131B0847c     sogdaf        IFLT      WDATE
168800990708A0684C     sogdaf        ANDNE     $DT0
168900060131B0847x***  D1Ann         ANDEQ     SI
169000990708A0684C                   MOVEL     *ON           H1Ann
169100990708A0684C                   ENDIF
169200000918A0832 *
169300000918A0832C                   endif
169400950301     C* imposto quegli attributi del sfl che sono modificabili
169500950301     C* al richiamo del determinato pgm
169600950301     C                   EXSR      ATRS1
169700950301     C*
169800940131     C*
169900940131     C                   ENDSR
170000940309     C/EJECT
170100940309     C************************************************************
170200940309     C* GESTIONE F03 VIDEO S1
170300940309     C************************************************************
170400940309     C     F03S1         BEGSR
170500940309     C*
170600940309     C                   MOVE      *ON           $FINE
170700941130     C                   MOVE      '1'           RET040
170800940325     C* fine programma
170900940325     C                   EXSR      ENDPGM
171000940309     C*
171100940309     C                   ENDSR
171200941206     C/EJECT
171300941206     C************************************************************
171400941206     C* GESTIONE F08 VIDEO S1
171500941206     C************************************************************
171600941206     C     F08S1         BEGSR
171700941206     C*
171800941206     C                   MOVE      *ON           $FINE
171900941206     C                   MOVE      '0'           RET040
172000941206     C*
172100941206     C                   ENDSR
172200940309     C/EJECT
172300940309     C************************************************************
172400940309     C* GESTIONE F10 VIDEO S1
172500940609     C* AGGIUNTA RECORD
172600940309     C************************************************************
172700940309     C     F10S1         BEGSR
172800940309     C*
172900950301     C                   Z-ADD     RIRI          CURR
173000950301     C                   Z-ADD     COCO          CURC
173100950301     C*
173200950301     C                   RESET                   A043DS
173300950301     C                   MOVEL     '01'          OPZ043
173400950301     C     CMT040        IFEQ      '1'
173500950301     C                   MOVEL     '1'           CMT043
173600950301     C                   ELSE
173700950301     C                   MOVEL     '0'           CMT043
173800950301     C                   ENDIF
173900950301     C                   MOVEL     WEXF          EXF043
174000950301     C                   MOVEL     $CHI          CHI043
174100950301     C                   MOVEL     $CHM          CHM043
174200950301     C                   MOVEL     $ESPI         ESI043
174300950301     C                   MOVEL     $ESPM         ESM043
174400950301     C                   Z-ADD     CURR          RIG043
174500950301     C                   Z-ADD     CURC          COL043
174600950301     C                   MOVEL     A043DS        KPJBU
174700950301     C*
174800970509     C                   CALL      'ANA043R'                            21
174900950301     C                   PARM                    KPJBA
175000950301     C                   PARM                    AUTDS
175100950301     C*
175200950301     C                   MOVEL     KPJBU         A043DS
175300950301     C* ritorno da PGM gestione
175400950301     C                   EXSR      RTN043
175500950301     C     WINZS1        IFEQ      *ON
175600950301     C                   MOVE      *ON           $INZS1
175700950301     C* se esistevano già righe sul sfl
175800950301     C* calcolo pagina a cui deve posizionarsi
175900950301     C     WSFL          IFGT      0
176000950301     C                   EXSR      CLCPAG
176100950301     C* altrimenti carico solo la 1a pagina
176200950301     C                   ELSE
176300950301     C                   Z-ADD     SFLPAG        WPAG
176400950301     C                   ENDIF
176500950301     C                   ENDIF
176600950301     C*
176700950301     C                   MOVE      *HIVAL        $LASTV
176800940627     C*
176900940309     C                   ENDSR
177000940309     C/EJECT
177100940309     C************************************************************
177200940309     C* GESTIONE F12 VIDEO S1
177300940309     C************************************************************
177400940309     C     F12S1         BEGSR
177500940309     C*
177600940309     C*  se la 1a videata sono le parzializzazioni, per F12 torno a queste
177700940309     C     $LOP          IFEQ      1
177800940309     C                   MOVE      'D1'          $GEST
177900940309     C*  se la 1a videata è questa, per F12 torno a menu
178000940309     C                   ELSE
178100940309     C                   MOVE      *ON           $FINE
178200941130     C                   MOVE      '2'           RET040
178300941027     C                   ENDIF
178400940309     C*
178500940309     C                   ENDSR
178600940309     C/EJECT
178700940309     C************************************************************
178800940309     C* GESTIONE F17 VIDEO S1
178900940309     C************************************************************
179000940309     C     F17S1         BEGSR
179100940309     C*
179200940309     C                   MOVE      'D1'          $GEST
179300941104     C* se il pgm è stato richiamato con chiave univoca
179400941104     C* e dopo è stata richiesta una parzializzazione
179500941104     C* tolgo la gestione della chiave univoca (azzere/abblenco il
179600941130     C* valore; propongo come ordinamento il 1°)
179700941130     C     $UNIQ         IFEQ      *ON
179800950320     C                   MOVE      *BLANK        UQ1040
179900950321     C                   MOVE      *OFF          $UNIQ
180000941027     C                   ENDIF
180100940831     C     $ORD          IFEQ      99
180200940831     C                   Z-ADD     1             $ORD
180300941104     C                   MOVE      *ON           $INZD1
180400941027     C                   ENDIF
180500940309     C*
180600940309     C                   ENDSR
180700940309     C/EJECT
180800940309     C************************************************************
180900940309     C* GESTIONE F18 VIDEO S1
181000940309     C************************************************************
181100940309     C     F18S1         BEGSR
181200940309     C*
181300940309     C                   MOVE      'D2'          $GEST
181400940309     C                   MOVEL     *ON           $INZD2
181500940831     C     $ORD          IFEQ      99
181600940831     C                   Z-ADD     1             $ORD
181700941027     C                   ENDIF
181800940309     C*
181900940309     C                   ENDSR
182000940309     C/EJECT
182100940309     C************************************************************
182200940309     C* GESTIONE F23 VIDEO S1
182300940309     C************************************************************
182400940309     C     F23S1         BEGSR
182500940309     C*
182600940309     C                   EXSR      RIEOPZ
182700941213     C                   Z-ADD     23            X
182800941213     C                   MOVE      FF23          $FC2(X)
182900940608     C                   MOVEL     WOPZ1         C1OPZ1
183000060224A1148C                   MOVEL     $ALLOPT       H1ALLOPT
183100940608     C                   MOVEL     WOPZ2         C1OPZ2
183200940309     C                   Z-ADD     WCOZ          C1COZ
183300940309     C*
183400940309     C                   ENDSR
183500940309     C/EJECT
183600940309     C************************************************************
183700940309     C* GESTIONE F24 VIDEO S1
183800940309     C************************************************************
183900940309     C     F24S1         BEGSR
184000940309     C*
184100940506     C*
184200940506     C                   MOVEA     $FX2          $FX
184300940506     C                   MOVEA     $FC2          $FC
184400940506     C                   MOVEA     $FR2          $FR
184500940506     C                   Z-ADD     $ULKS1        $ULK
184600940929     C                   Z-ADD     79            $L01
184700940929     C                   Z-ADD     79            $L02
184800940506     C                   EXSR      RIEKEY
184900940506     C                   Z-ADD     $ULK          $ULKS1            3 0
185000940720     C                   MOVEA     $FC           $FC2
185100940506     C                   MOVEL     $KEY1         Z2KE1
185200060224A1148C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
185300940506     C                   MOVEL     $KEY2         Z2KE2
185400940506     C                   WRITE     FMTZ2
185500940506     C*
185600940309     C                   ENDSR
185700940128     C/EJECT
185800940128     C************************************************************
185900940131     C* CONTROLLO TESTATA LISTA
186000940128     C************************************************************
186100940131     C     CTRC1         BEGSR
186200940128     C*
186300940201     C                   MOVE      *OFF          *IN99
186400940131     C*
186500940202     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
186600940202     C* L'INIZIALIZZAZIONE LISTA
186700940202     C*
186800941130     C     C1POS         IFNE      W1POS
186900940202     C                   EXSR      POSS1
187000941027     C                   ENDIF
187100940202     C*
187200940202     C                   ENDSR
187300940202     C/EJECT
187400940202     C************************************************************
187500940202     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
187600940202     C************************************************************
187700940202     C     POSS1         BEGSR
187800940202     C*
187900940202     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
188000940202     C*
188100941118   B1C     WMAX          IFGT      0
188200940202     C                   READC     FMTS1                                  21
188300941118   B2C     *IN21         DOWEQ     *OFF
188400940202     C     *IN99         ANDEQ     *OFF
188500940202     C*
188600941118   B3C     S1OPZ         IFNE      0
188700940202     C                   SETON                                        51  99
188800941118   E3C                   ENDIF
188900940202     C* se rilevata opzione attiva, attivo sflnxtchg
189000941118   B3C     *IN99         IFEQ      *ON
189100940202     C                   MOVE      *ON           *IN32
189200941118     C* imposta attributi della riga di sfl
189300941118     C                   EXSR      ATRS1
189400941118     C*
189500940202     C                   UPDATE    FMTS1
189600941118   X3C                   ELSE
189700940202     C                   READC     FMTS1                                  21
189800941118   E3C                   ENDIF
189900940202     C*
190000941118   E2C                   ENDDO
190100940202     C*
190200941118   E1C                   ENDIF
190300940202     C*
190400940221     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
190500940221     C* AL VALORE DI POSIZIONAMENTO :
190600940221     C* se questo è maggiore del valore del rispettivo campo di
190700940221     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
190800940221     C* con i valori di parzializzazione.
190900940221     C*
191000941118   B1C     *IN99         IFEQ      *OFF
191100940721    >C                   MOVE      *BLANK        W1POS
191200940202     C*
191300941118   B2C                   SELECT
191400940202     C*
191500940202     C     $ORD          WHENEQ    1
191600940202     C*
191700941201     C     C1POS         IFGT      D1DES
191800941201     C                   MOVEL     C1POS         W1POS
191900941027     C                   ENDIF
192000940131     C*
192100940202     C                   MOVE      *ON           *IN99
192200940223     C                   MOVE      *ON           $INZS1
192300940831     C*
192400940831     C     $ORD          WHENEQ    99
192500940831     C*
192600940831     C                   MOVE      *ON           *IN99
192700940831     C                   MOVE      *ON           $INZS1
192800940831     C*
192900940202     C     $ORD          WHENEQ    2
193000940202     C*
193100941201     C     C1POS         IFGT      D1CIT
193200941201     C                   MOVE      C1POS         W1POS
193300941027     C                   ENDIF
193400940202     C*
193500940202     C                   MOVE      *ON           *IN99
193600940223     C                   MOVE      *ON           $INZS1
193700941201     C*
193800941201     C     $ORD          WHENEQ    3
193900941201     C*
194000941201     C     C1POS         IFGT      D1STA
194100941201     C                   MOVE      C1POS         W1POS
194200941201     C                   ENDIF
194300941201     C*
194400941201     C                   MOVE      *ON           *IN99
194500941201     C                   MOVE      *ON           $INZS1
194600941201     C*
194700941201     C     $ORD          WHENEQ    4
194800941201     C*
194900941201     C     C1POS         IFGT      D1PRV
195000941201     C                   MOVE      C1POS         W1POS
195100941201     C                   ENDIF
195200941201     C*
195300941201     C                   MOVE      *ON           *IN99
195400941201     C                   MOVE      *ON           $INZS1
195500941201     C*
195600941201     C     $ORD          WHENEQ    5
195700941201     C*
195800941201     C     C1POS         IFGT      D1IVA
195900941201     C                   MOVE      C1POS         W1POS
196000941201     C                   ENDIF
196100941201     C*
196200941201     C                   MOVE      *ON           *IN99
196300941201     C                   MOVE      *ON           $INZS1
196400941201     C*
196500941201     C     $ORD          WHENEQ    6
196600941201     C*
196700941201     C     C1POS         IFGT      D1CDF
196800941201     C                   MOVE      C1POS         W1POS
196900941201     C                   ENDIF
197000941201     C*
197100941201     C                   MOVE      *ON           *IN99
197200941201     C                   MOVE      *ON           $INZS1
197300941201     C*
197400941201     C     $ORD          WHENEQ    7
197500941201     C*
197600941201     C     C1POS         IFGT      D1COD
197700941201     C                   MOVE      C1POS         W1POS
197800980113     C                   CALLb     'XALLINEA'
197900980113     C                   PARM                    W1POS
198000980113     C                   PARM      8             LEN               5 0
198100980113     C                   PARM                    XSCINP
198200941201     C                   ENDIF
198300941201     C*
198400941201     C                   MOVE      *ON           *IN99
198500941201     C                   MOVE      *ON           $INZS1
198600940202     C*
198700941118   E2C                   ENDSL
198800940202     C*
198900941118   E1C                   ENDIF
199000940202     C*
199100940131     C                   ENDSR
199200940131     C/EJECT
199300940131     C************************************************************
199400940131     C* CONTROLLO OPZIONI LISTA
199500940131     C************************************************************
199600940131     C     CTRS1         BEGSR
199700940131     C*
199800940202     C                   MOVEL     *OFF          $ESCI
199900940201     C                   SETOFF                                       99
200000940131     C                   Z-ADD     0             S1OPZ
200100940131     C*
200200940127     C* Leggo il sfl solo se ci sono rcd
200300941118   B1C     WMAX          IFGT      0
200400940131     C                   READC     FMTS1                                  21
200500940127     C*
200600940131     C* esce se fine sfl o errore che richiede l'uscita
200700941118   B2C     *IN21         DOWEQ     *OFF
200800940131     C     $ESCI         ANDEQ     *OFF
200900940201     C                   Z-ADD     S1NRR         C1RCD
201000940131     C* ctrl su riga
201100940131     C                   EXSR      RECS1
201200940131     C* gestione opzioni
201300941118   B3C     S1OPZ         IFNE      0
201400940201     C     *IN99         ANDEQ     *OFF
201500940131     C                   EXSR      OPZS1
201600941118   E3C                   ENDIF
201700940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
201800941118   B3C     *IN99         IFEQ      *ON
201900940131     C                   MOVE      *ON           *IN32
202000941118   E3C                   ENDIF
202100950301     C*
202200950301     C                   Z-ADD     0             S1OPZ
202300950301     C*
202400941118     C* imposta attributi della riga di sfl
202500941118     C                   EXSR      ATRS1
202600941118     C*
202700940223     C*
202800940127     C                   UPDATE    FMTS1
202900941125     C                   MOVE      *OFF          *IN32
203000940223     C*
203100940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
203200941118   B3C     $ESCI         IFEQ      *OFF
203300940131     C                   READC     FMTS1                                  21
203400940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
203500941118   B4C     *IN21         IFEQ      *ON
203600940201     C                   MOVE      WINZS1        $INZS1
203700050608D1830C                   MOVE      *off          WINZS1
203800050608  "  C* se è avvenuta almeno una modifica
203900050608D1830C                   IF        $INZS1 = *on
204000050608     C* calcolo pagina a cui deve posizionarsi
204100940728     C     WSFL          IFGT      0
204200940728     C                   EXSR      CLCPAG
204300940728     C                   ELSE
204400940728     C                   Z-ADD     SFLPAG        WPAG
204500940728     C                   ENDIF
204600941118   E4C                   ENDIF
204700941118   E4C                   ENDIF
204800050608D1830C                   ENDIF
204900940131     C*
205000941118   E2C                   ENDDO
205100940127     C*
205200941118   E1C                   ENDIF
205300940131     C*
205400940127     C                   ENDSR
205500940127     C/EJECT
205600940127     C************************************************************
205700940131     C* CONTROLLO CAMPI I/O RIGA LISTA
205800940127     C************************************************************
205900940131     C     RECS1         BEGSR
206000940131     C*
206100940201     C* reset indicatori DSPATR
206200940201     C                   MOVE      *ALL'0'       IN4049           10
206300940201     C                   MOVEA     IN4049        *IN(40)
206400940201     C*
206500940131     C* Controllo se Utente Autorizzato alla scelta
206600940202     C     S1OPZ         IFNE      0
206700940131     C                   Z-ADD     1             X
206800940223     C     S1OPZ         LOOKUP    $OP1(X)                                21
206900960202     C* mi salvo l'indice dell' opzione
207000960202     C   21              Z-add     X             PosOpz            2 0
207100940211     C     *IN21         IFEQ      *OFF
207200940223     C     $FL1(X)       OREQ      'N'
207300940131     C                   MOVE      *ON           $ESCI
207400940201     C                   SETON                                        504099
207500940131     C                   ENDIF
207600940202     C                   ENDIF
207700940131     C*
207800940131     C                   ENDSR
207900940224     C/EJECT
208000950301     C************************************************************
208100950301     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
208200950301     C************************************************************
208300950301     C     ATRS1         BEGSR
208400950301     C*
208500950301     C* azzero i bottoni
208600950301     C                   MOVEL     X'A0'         S1B01A
208700950301     C                   MOVEL     X'A0'         S1B02A
208800950301     C                   MOVEL     X'A0'         S1B03A
208900950303     C                   MOVEL     X'A0'         S1B04A
209000950303     C                   MOVEL     X'A0'         S1B05A
209100990713A0684C* azzero gli attributi non bottoni
209200990708A0684C                   MOVEL     X'A0'         S1ANNA
209300950301     C*
209400950301     C* li valorizzo a seconda dello stato ricevuto o testato
209500950301     C     H1B01         IFEQ      1
209600950301     C                   MOVEL     PRES          S1B01A
209700950301     C                   ENDIF
209800950301     C*
209900950301     C     H1B03         IFEQ      1
210000950301     C                   MOVEL     PRES          S1B03A
210100950301     C                   ENDIF
210200950303     C*
210300950303     C     H1B04         IFEQ      1
210400950303     C                   MOVEL     PRES          S1B04A
210500950303     C                   ENDIF
210600950303     C*
210700950303     C     H1B05         IFEQ      1
210800950303     C                   MOVEL     PRES          S1B05A
210900950303     C                   ENDIF
211000950301     C*
211100950301     C     H1B02         IFEQ      1
211200950301     C                   MOVEL     PRES          S1B02A
211300950301     C                   ENDIF
211400990708A0684C     H1Ann         IFEQ      *ON
211500990708A0684C                   MOVEL     ANNUL         S1ANNA
211600990708A0684C                   ENDIF
211700000920A0835 *
211800000920  "  C                   eval      *in41 = H1INV
211900000920A0835 *
212000950301     C*
212100950301     C                   ENDSR
212200941118     C/EJECT
212300941118     C************************************************************
212400941118     C* GESTIONE OPZIONI LISTA
212500941118     C************************************************************
212600941118     C     OPZS1         BEGSR
212700941118     C*
212800941027     C                   SELECT
212900940225     C*
213000940225     C* se opz=Selezione passo la chiave al pgm chiamante
213100940225     C     S1OPZ         WHENEQ    1
213200941130     C* passare nei campi appositi della DS A040DS il valore
213300941130     C* della chiave della VL univoca (se c'è)
213400941130     C                   MOVE      S1OPZ         OPZ040
213500941130     C                   MOVE      S1COD         COD040
213600940225     C                   MOVE      *ON           $ESCI
213700940225     C                   MOVE      *ON           $FINE
213800940909     C*
213900940909     C                   MOVE      *HIVAL        $LASTV
214000950320     C*
214100950320     C* se opz=Invia scrive nella posta
214200950320     C*
214300950320     C     S1OPZ         WHENEQ    10
214400950320     C                   RESET                   A727DS
214500950320     C                   MOVE      '01'          OPZ727
214600950320     C                   MOVEL     S1DES         TXT727
214700950320     C                   MOVEL     'SOG'         TPA727
214800950320     C                   MOVEL     S1COD         KY1727
214900950320     C                   MOVE      'E'           PGM727
215000950320     C                   MOVE      XSCSOC        SOC727
215100950320     C                   MOVE      WIN727        INZ727
215200950320     C                   CLEAR                   KPJBU
215300950320     C                   MOVEL     A727DS        KPJBU
215400950320     C                   CALL      'ANA727R'
215500950320     C                   PARM                    KPJBA
215600950320     C                   MOVEL     KPJBU         A727DS
215700950320     C*
215800950320     C                   EXSR      RTN727
215900950320     C*
216000950320     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
216100950320     C     *IN99         IFEQ      *ON
216200950320     C                   SETON                                        40
216300950320     C                   ENDIF
216400950320     C*
216500950320     C                   MOVE      *HIVAL        $LASTV
216600950301     C*
216700950301     C* COPIA
216800950301     C* passando per il filtro
216900950301     C*
217000950301     C     S1OPZ         WHENEQ    3
217100950301     C*
217200950301     C                   RESET                   A043DS
217300950301     C* gli richiedo sempre la manutenzione
217400950301     C                   MOVEL     '03'          OPZ043
217500950301     C     CMT040        IFEQ      '1'
217600950301     C                   MOVEL     '1'           CMT043
217700950301     C                   ELSE
217800950301     C                   MOVEL     '0'           CMT043
217900950301     C                   ENDIF
218000950301     C                   MOVEL     WEXF          EXF043
218100950301     C                   MOVEL     'S'           FSC043
218200950301     C                   MOVEL     $CHI          CHI043
218300950301     C                   MOVEA     *ZERO         CHM
218400950301     C                   Z-ADD     1             CHM(1)
218500950301     C                   MOVEL     $CHM          CHM043
218600950301     C                   MOVEA     KHM           CHM
218700950301     C                   MOVEL     *OFF          ESI043
218800950301     C                   MOVEL     *OFF          ESM043
218900950301     C                   MOVEL     S1COD         SOG043
219000950301     C                   Z-ADD     CURR          RIG043
219100950301     C                   Z-ADD     CURC          COL043
219200951003     C                   MOVE      *OFF          OPD043
219300950301     C*
219400950301     C                   MOVEL     A043DS        KPJBU
219500950301     C*
219600970509     C                   CALL      'ANA043R'                            21
219700950301     C                   PARM                    KPJBA
219800950301     C                   PARM                    AUTDS
219900950301     C*
220000950301     C                   MOVEL     KPJBU         A043DS
220100950301     C*
220200950301     C* ritorno da PGM gestione
220300950301     C                   EXSR      RTN043
220400950301     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
220500950301     C     *IN99         IFEQ      *ON
220600950301     C                   SETON                                        40
220700950301     C                   ENDIF
220800950301     C*
220900950301     C                   MOVE      *HIVAL        $LASTV
221000940831     C*
221100950301     C* COMMENTI
221200950301     C* passando per il filtro
221300950301     C*
221400950301     C     S1OPZ         WHENEQ    14
221500950301     C*
221600950301     C                   RESET                   A043DS
221700950301     C* gli richiedo sempre la manutenzione
221800950301     C                   MOVEL     '02'          OPZ043
221900950301     C     CMT040        IFEQ      '1'
222000950301     C                   MOVEL     '1'           CMT043
222100950301     C                   ELSE
222200950301     C                   MOVEL     '0'           CMT043
222300950301     C                   ENDIF
222400950301     C                   MOVEL     WEXF          EXF043
222500950301     C                   MOVEL     'S'           FSC043
222600950301     C                   MOVEL     $CHI          CHI043
222700950301     C                   MOVEA     *ZERO         CHM
222800950301     C                   Z-ADD     1             CHM(4)
222900950301     C                   MOVEL     $CHM          CHM043
223000950301     C                   MOVEA     KHM           CHM
223100950301     C                   MOVEL     $ESPI         ESI043
223200950301     C                   MOVEL     *OFF          ESM043
223300950301     C                   MOVEL     S1COD         SOG043
223400950301     C                   Z-ADD     CURR          RIG043
223500950301     C                   Z-ADD     CURC          COL043
223600950301     C                   MOVEL     H1B01         B01043
223700950301     C                   MOVEL     H1E01         E01043
223800951003     C                   MOVE      *ON           OPD043
223900950301     C*
224000950301     C                   MOVEL     A043DS        KPJBU
224100950301     C*
224200970509     C                   CALL      'ANA043R'                            21
224300950301     C                   PARM                    KPJBA
224400950301     C                   PARM                    AUTDS
224500950301     C*
224600950301     C                   MOVEL     KPJBU         A043DS
224700950301     C*
224800950301     C* ritorno da PGM gestione
224900950301     C                   EXSR      RTN043
225000950301     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
225100950301     C     *IN99         IFEQ      *ON
225200950301     C                   SETON                                        40
225300950301     C                   ENDIF
225400950301     C*
225500950301     C                   MOVE      *HIVAL        $LASTV
225600950301     C*
225700950301     C* DATI CODICI COLLEGATI
225800950301     C* passando per il filtro
225900950301     C*
226000950301     C     S1OPZ         WHENEQ    18
226100950301     C*
226200950301     C                   RESET                   A043DS
226300950301     C* gli richiedo sempre la manutenzione
226400950301     C                   MOVEL     '02'          OPZ043
226500950301     C     CMT040        IFEQ      '1'
226600950301     C                   MOVEL     '1'           CMT043
226700950301     C                   ELSE
226800950301     C                   MOVEL     '0'           CMT043
226900950301     C                   ENDIF
227000950301     C                   MOVEL     WEXF          EXF043
227100950301     C                   MOVEL     'S'           FSC043
227200950301     C                   MOVEL     $CHI          CHI043
227300950301     C                   MOVEA     *ZERO         CHM
227400950301     C                   Z-ADD     1             CHM(3)
227500950301     C                   MOVEL     $CHM          CHM043
227600950301     C                   MOVEA     KHM           CHM
227700950301     C                   MOVEL     $ESPI         ESI043
227800950301     C                   MOVEL     *OFF          ESM043
227900950301     C                   MOVEL     S1COD         SOG043
228000950301     C                   Z-ADD     CURR          RIG043
228100950301     C                   Z-ADD     CURC          COL043
228200950301     C                   MOVEL     H1B03         B03043
228300950301     C                   MOVEL     H1E03         E03043
228400950303     C                   MOVEL     H1B04         B04043
228500950303     C                   MOVEL     H1E04         E04043
228600950303     C                   MOVEL     H1B05         B05043
228700950303     C                   MOVEL     H1E05         E05043
228800951003     C                   MOVE      *ON           OPD043
228900950301     C*
229000950301     C                   MOVEL     A043DS        KPJBU
229100950301     C*
229200970509     C                   CALL      'ANA043R'                            21
229300950301     C                   PARM                    KPJBA
229400950428     C                   PARM                    AUTDS
229500950301     C*
229600950301     C                   MOVEL     KPJBU         A043DS
229700950301     C*
229800950301     C* ritorno da PGM gestione
229900950301     C                   EXSR      RTN043
230000950301     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
230100950301     C     *IN99         IFEQ      *ON
230200950301     C                   SETON                                        40
230300950301     C                   ENDIF
230400950301     C*
230500950301     C                   MOVE      *HIVAL        $LASTV
230600950301     C*
230700950301     C* DATI ULTERIORI INDIRIZZI
230800950301     C* passando per il filtro
230900950301     C*
231000950301     C     S1OPZ         WHENEQ    17
231100950301     C*
231200950301     C                   RESET                   A043DS
231300950301     C* gli richiedo sempre la manutenzione
231400950301     C                   MOVEL     '02'          OPZ043
231500950301     C     CMT040        IFEQ      '1'
231600950301     C                   MOVEL     '1'           CMT043
231700950301     C                   ELSE
231800950301     C                   MOVEL     '0'           CMT043
231900950301     C                   ENDIF
232000950301     C                   MOVEL     WEXF          EXF043
232100950301     C                   MOVEL     'S'           FSC043
232200950301     C                   MOVEL     $CHI          CHI043
232300950301     C                   MOVEA     *ZERO         CHM
232400950301     C                   Z-ADD     1             CHM(2)
232500950301     C                   MOVEL     $CHM          CHM043
232600950301     C                   MOVEA     KHM           CHM
232700950301     C                   MOVEL     $ESPI         ESI043
232800950301     C                   MOVEL     *OFF          ESM043
232900950301     C                   MOVEL     S1COD         SOG043
233000950301     C                   Z-ADD     CURR          RIG043
233100950301     C                   Z-ADD     CURC          COL043
233200950301     C                   MOVEL     H1B02         B02043
233300950301     C                   MOVEL     H1E02         E02043
233400951003     C                   MOVE      *ON           OPD043
233500950301     C*
233600950301     C                   MOVEL     A043DS        KPJBU
233700950301     C*
233800970509     C                   CALL      'ANA043R'                            21
233900950301     C                   PARM                    KPJBA
234000950301     C                   PARM                    AUTDS
234100950301     C*
234200950301     C                   MOVEL     KPJBU         A043DS
234300950301     C*
234400950301     C* ritorno da PGM gestione
234500950301     C                   EXSR      RTN043
234600950301     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
234700950301     C     *IN99         IFEQ      *ON
234800950301     C                   SETON                                        40
234900950301     C                   ENDIF
235000950301     C*
235100950301     C                   MOVE      *HIVAL        $LASTV
235200950404     C*
235300950404     C* GESTIONE CONTATTI
235400950404     C* non passa per il filtro
235500950404     C     S1OPZ         WHENEQ    19
235600020625B0453 *
235700020625  "   * ctr chiamata ricorsiva
235800020625  "  C                   reset                   X48ChPgmds
235900020625  "  C                   eval      X48Pgm      = 'ANA060R'
236000020625  "  C                   eval      Kpjbu       = X48ChPgmDS
236100020625  "  C                   call      'X48CHPGM'
236200020625  "  C                   parm                    Kpjba
236300020625  "  C                   movel     Kpjbu         x48ChPgmDS
236400020625  "  C                   if        x48errore   = *Off
236500020625B0453 *
236600950405     C                   RESET                   A060DS
236700950404     C* gli richiedo sempre la manutenzione
236800950404     C                   MOVEL     '02'          OPZ060
236900950404     C     CMT040        IFEQ      '1'
237000950404     C                   MOVEL     '1'           CMT060
237100950404     C                   ELSE
237200950404     C                   MOVEL     '0'           CMT060
237300950404     C                   ENDIF
237400950404     C                   MOVEL     S1COD         SOG060
237500950404     C*
237600950404     C                   MOVEL     A060DS        KPJBU
237700950404     C*
237800950404     C                   CALL      'ANA060R'
237900950404     C                   PARM                    KPJBA
238000950404     C*
238100950404     C                   MOVEL     KPJBU         A060DS
238200950404     C*
238300950404     C                   EXSR      RTN060
238400950404     C*
238500950404     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
238600950404     C     *IN99         IFEQ      *ON
238700950404     C                   SETON                                        40
238800950404     C                   ENDIF
238900950404     C*
239000950404     C                   MOVE      *HIVAL        $LASTV
239100020625B0453 *
239200020625B0453C                   endif
239300100831R027 C*
239400100831  "  C* se opz 22 = Dati aggiuntivi
239500160505R027 C     S1OPZ         WHENEQ    22
239501160505R224  *
239502160505  "   * ctr chiamata ricorsiva
239503160505  "  C                   reset                   X48ChPgmds
239504160505  "  C                   eval      X48Pgm      = 'SDGSD15R'
239505160505  "  C                   eval      Kpjbu       = X48ChPgmDS
239506160505  "  C                   call      'X48CHPGM'
239507160505  "  C                   parm                    Kpjba
239508160505  "  C                   movel     Kpjbu         x48ChPgmDS
239509160505  "  C                   if        x48errore   = *Off
239510160505R224 C*
239600160505R027 C                   EXSR      CallDatiAgg
239700100831  "  C*
239800100831R027 C                   MOVE      *HIVAL        $LASTV
239801160505R224 C*
239802160505R224 C                   endif
239900950301     C*
240000950301     C* DATI ANAGRAFICI
240100950301     C* passando per il filtro
240200950301     C*
240300950301     C     S1OPZ         WHENEQ    40
240400950301     C*
240500950301     C                   RESET                   A043DS
240600950301     C* gli richiedo sempre la manutenzione
240700950301     C                   SELECT
240800950301     C     OPZ040        WHENEQ    *BLANK
240900950301     C     MANSOG        ANDEQ     *OFF
241000950301     C                   MOVE      '05'          OPZ043
241100950301     C     OPZ040        WHENEQ    *BLANK
241200950301     C     MANSOG        ANDEQ     *ON
241300950301     C                   MOVE      '02'          OPZ043
241400950301     C     OPZ040        WHENEQ    '01'
241500950301     C     MANSOG        ANDEQ     *OFF
241600950301     C                   MOVE      '05'          OPZ043
241700950301     C     OPZ040        WHENEQ    '01'
241800950301     C     MANSOG        ANDEQ     *ON
241900950301     C                   MOVE      '02'          OPZ043
242000950301     C     OPZ040        WHENEQ    '05'
242100950301     C                   MOVE      '05'          OPZ043
242200010213B0348C                   Other
242300010213B0348C                   Eval      Opz043 = Opz040
242400950301     C                   ENDSL
242500950301     C     CMT040        IFEQ      '1'
242600950301     C                   MOVEL     '1'           CMT043
242700950301     C                   ELSE
242800950301     C                   MOVEL     '0'           CMT043
242900950301     C                   ENDIF
243000950301     C                   MOVEL     WEXF          EXF043
243100950301     C*                    MOVEL'S'       FSC043
243200950301     C                   MOVEL     $CHI          CHI043
243300950301     C                   MOVEA     *ZERO         CHM
243400950301     C                   Z-ADD     1             CHM(1)
243500950301     C                   MOVEL     $CHM          CHM043
243600950301     C                   MOVEA     KHM           CHM
243700950301     C                   MOVEL     $ESPI         ESI043
243800950301     C                   MOVEL     *OFF          ESM043
243900950301     C                   MOVEL     S1COD         SOG043
244000950301     C                   Z-ADD     CURR          RIG043
244100950301     C                   Z-ADD     CURC          COL043
244200951003     C                   MOVE      *ON           OPD043
244300950301     C* bottoni (non li passo se non fanno strettamente riferimento
244400950301     C*  all'opzione richiesta)
244500950301     C*
244600950301     C                   MOVEL     A043DS        KPJBU
244700950301     C*
244800970509     C                   CALL      'ANA043R'                            21
244900950301     C                   PARM                    KPJBA
245000950301     C                   PARM                    AUTDS
245100950301     C*
245200950301     C                   MOVEL     KPJBU         A043DS
245300950301     C*
245400950301     C* ritorno da PGM gestione
245500950301     C                   EXSR      RTN043
245600950301     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
245700950301     C     *IN99         IFEQ      *ON
245800950301     C                   SETON                                        40
245900950301     C                   ENDIF
246000950301     C*
246100950301     C                   MOVE      *HIVAL        $LASTV
246200950428     C* VARIAZIONI DATI  FISCALI
246300950428     C*
246400950428     C     S1OPZ         WHENEQ    31
246500950428     C*
246600950428     C                   RESET                   A057DS
246700950428     C* gli richiedo sempre la manutenzione
246800950428     C                   SELECT
246900950428     C     OPZ040        WHENEQ    *BLANK
247000950428     C     MANSOG        ANDEQ     *OFF
247100950428     C                   MOVE      '05'          OPZ057
247200950428     C     OPZ040        WHENEQ    *BLANK
247300950428     C     MANSOG        ANDEQ     *ON
247400950428     C                   MOVE      '02'          OPZ057
247500950428     C     OPZ040        WHENEQ    '01'
247600950428     C     MANSOG        ANDEQ     *OFF
247700950428     C                   MOVE      '05'          OPZ057
247800950428     C     OPZ040        WHENEQ    '01'
247900950428     C     MANSOG        ANDEQ     *ON
248000950428     C                   MOVE      '02'          OPZ057
248100950428     C     OPZ040        WHENEQ    '05'
248200950428     C                   MOVE      '05'          OPZ057
248300950428     C                   ENDSL
248400950428     C     CMT040        IFEQ      '1'
248500950428     C                   MOVEL     '1'           CMT057
248600950428     C                   ELSE
248700950428     C                   MOVEL     '0'           CMT057
248800950428     C                   ENDIF
248900950428     C                   MOVEL     S1COD         SOG057
249000990525b0025C********           MOVEL     *ZERO         DTF057
249100000119A0735C***** ex-A0732     MOVEL     $dt2039       DTF057
249200000119A0735C                   Move      $Dt9          DtF057
249300950428     C* bottoni (non li passo se non fanno strettamente riferimento
249400950428     C*  all'opzione richiesta)
249500950428     C*
249600950428     C                   MOVEL     A057DS        KPJBU
249700950428     C*
249800950428     C                   CALL      'ANA057R'
249900950428     C                   PARM                    KPJBA
250000950428     C                   PARM                    AUTDS
250100950428     C*
250200950502     C                   MOVEL     KPJBU         A057DS
250300950428     C*
250400950428     C* ritorno da PGM gestione
250500950428     C                   EXSR      RTN057
250600950428     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
250700950428     C     *IN99         IFEQ      *ON
250800950428     C                   SETON                                        40
250900950428     C                   ENDIF
251000950428     C*
251100950428     C                   MOVE      *HIVAL        $LASTV
251200960202     C*
251300960202     C*
251400960202     C* se selezionata opzione personalizzata, richiamo il programma
251500960202     C* corrispondente
251600960202     C     $PGO1(PosOpz) Whenne    *Blank
251700960202     C                   RESET                   A043DS
251800960202     C* << passare nei campi appositi della DS A043DS il valore
251900960202     C*    della chiave della VL univoca (se c'è) >>
252000960202     C                   MOVEL     S1OPZ         OPZ043
252100960202     C                   MOVEL     'S'           FSC043
252200960202     C     CMT040        IFEQ      '1'
252300960202     C                   MOVEL     '1'           CMT043
252400960202     C                   ELSE
252500960202     C                   MOVEL     '0'           CMT043
252600960202     C                   ENDIF
252700960202     C                   MOVEL     S1COD         SOG043
252800960202     C*
252900960202     C                   MOVEL     A043DS        KPJBU
253000960202     C*
253100960202     C                   CALL      $PGO1(PosOpz)                        22
253200960202     C                   PARM                    KPJBA
253300960202     C                   PARM                    NOMPGM
253400960202     C*
253500960202     C                   MOVEL     KPJBU         A043DS
253600960202     C*
253700960202     C* ritorno da PGM
253800960202     C                   EXSR      RTN000
253900960202     C*
254000940225     C*
254100940225     C* se altre opzioni (o nessuna opzione) richiamo pgm gestione
254200941027     C                   OTHER
254300940225     C*
254400950301     C                   RESET                   A043DS
254500950301     C* << passare nei campi appositi della DS A043DS il valore
254600950301     C*    della chiave della VL univoca (se c'è) >>
254700950301     C                   MOVEL     S1OPZ         OPZ043
254800950302     C                   MOVEL     'S'           FSC043
254900950301     C     CMT040        IFEQ      '1'
255000950301     C                   MOVEL     '1'           CMT043
255100950301     C                   ELSE
255200950301     C                   MOVEL     '0'           CMT043
255300950301     C                   ENDIF
255400950301     C                   MOVEL     WEXF          EXF043
255500950301     C                   MOVEL     $CHI          CHI043
255600950301     C     OPZ043        IFEQ      '04'
255700950301     C                   MOVEA     *ZERO         CHM
255800950301     C                   MOVEL     1             CHM(1)
255900950301     C                   MOVEL     $CHM          CHM043
256000950301     C                   MOVEA     KHM           CHM
256100950301     C                   ELSE
256200950301     C                   MOVEL     $CHM          CHM043
256300950301     C                   ENDIF
256400950301     C                   MOVEL     $ESPI         ESI043
256500950301     C                   MOVEL     $ESPM         ESM043
256600950301     C                   MOVEL     S1COD         SOG043
256700950301     C                   Z-ADD     CURR          RIG043
256800950301     C                   Z-ADD     CURC          COL043
256900950301     C* bottoni
257000950301     C*
257100950301     C                   MOVEL     H1B01         B01043
257200950301     C                   MOVEL     H1E01         E01043
257300950301     C                   MOVEL     H1B02         B02043
257400950301     C                   MOVEL     H1E02         E02043
257500950301     C                   MOVEL     H1B03         B03043
257600950301     C                   MOVEL     H1E03         E03043
257700950303     C                   MOVEL     H1B04         B04043
257800950303     C                   MOVEL     H1E04         E04043
257900950303     C                   MOVEL     H1B05         B05043
258000950303     C                   MOVEL     H1E05         E05043
258100951003     C                   MOVE      *OFF          OPD043
258200950301     C*
258300950301     C                   MOVEL     A043DS        KPJBU
258400950301     C*
258500970509     C                   CALL      'ANA043R'                            21
258600950301     C                   PARM                    KPJBA
258700950301     C                   PARM                    AUTDS
258800950301     C*
258900950301     C                   MOVEL     KPJBU         A043DS
259000950301     C*
259100950301     C* ritorno da PGM gestione
259200950301     C                   EXSR      RTN043
259300950301     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
259400950301     C     *IN99         IFEQ      *ON
259500950301     C                   SETON                                        40
259600950301     C                   ENDIF
259700950301     C*
259800950301     C                   MOVE      *HIVAL        $LASTV
259900950301     C                   ENDSL
260000950301     C*
260100940131     C                   ENDSR
260200940223     C/EJECT
260300160503R224 X*ex R027 ***************************************************
260400160503  "  X*ex R027 Richiamo pgm per dati anagrafici aggiuntivi
260500160503  "  X*ex R027 ***************************************************
260600160503  "  X*ex R027    CallDatiAgg   BEGSR
260700160503  "  X*ex R027
260800160503  "  X*ex R027           RESET                   SDGSD16DS
260900160503  "  X*ex R027
261000160503  "  X*ex R027           MOVEL     '02'          OPZD16
261100160503  "  X*ex R027 CMT040        IFEQ      '1'
261200160503  "  X*ex R027           MOVEL     '1'           CMTD16
261300160503  "  X*ex R027           ELSE
261400160503  "  X*ex R027           MOVEL     '0'           CMTD16
261500160503  "  X*ex R027           ENDIF
261600160503  "  X*ex R027
261700160503  "  X*ex R027           Eval      SoggD16     = S1Cod
261800160503  "  X*ex R027
261900160503  "  X*ex R027           MOVEL     SDGSD16DS     KPJBU
262000160503  "  X*ex R027
262100160503  "  X*ex R027           CALL      'SDGSD16R'
262200160503  "  X*ex R027           PARM                    KPJBA
262300160503  "  X*ex R027
262400160503  "  X*ex R027           MOVEL     KPJBU         SDGSD16DS
262500160503  "  X*ex R027
262600160503  "  X*ex R027 ritorno da PGM gestione
262700160503  "  X*ex R027           EXSR      RTND16
262800160503  "  X*ex R027
262900160503  "  X*ex R027 se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
263000160503  "  X*ex R027 *IN99         IFEQ      *ON
263100160503  "  X*ex R027           SETON                                        40
263200160503  "  X*ex R027           ENDIF
263300160503  "  X*ex R027
263400160503  "  X*ex R027           MOVE      *HIVAL        $LASTV
263500160503  "  X*ex R027
263600160503  "  X*ex R027           ENDSR
263700160503  "  X*ex R027 /EJECT
263800160503  "  X*ex R027******************************************************
263900160503  "  X*ex R027 GESTIONE RITORNO DA PGM DI GESTIONE
264000160503  "  X*ex R027****************************************************
264100160503  "  X*ex R027 RTND16    BEGSR
264200160503  "  X*ex R027
264300160503  "  X*ex R027 modo di ritorno
264400160503  "  X*ex R027
264500160503  "  X*ex R027           SELECT
264600160503  "  X*ex R027 questi modi di utilizzo dei valori di ritorno dal
264700160503  "  X*ex R027 pgm di manutenzione rcd di anagrafica sono delle
264800160503  "  X*ex R027 proposte, normalmente sempre valide, ma modificabili
264900160503  "  X*ex R027 per situazioni particolari
265000160503  "  X*ex R027 1 = F3
265100160503  "  X*ex R027  RETD16        WHENEQ    '1'
265200160503  "  X*ex R027           MOVE      *ON           $ESCI                          sospese READC
265300160503  "  X*ex R027           MOVE      *ON           $FINE                          fine pgm
265400160503  "  X*ex R027           MOVE      '1'           RET040
265500160503  "  X*ex R027 2 = F12
265600160503  "  X*ex R027 RETD16        WHENEQ    '2'
265700160503  "  X*ex R027 se il pgm di manutenzione ha prima una guida, è necessario anche
265800160503  "  X*ex R027 questo test, altrimenti prevale il retD04 e il sfl non viene
265900160503  "  X*ex R027 ricaricato
266000160503  "  X*ex R027 OPRD16        ANDNE     '1'
266100160503  "  X*ex R027
266200160503  "  X*ex R027 se l'opzione con cui è stato chiamato non è la modifica
266300160503  "  X*ex R027 memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
266400160503  "  X*ex R027 OPZD16        IFNE      '02'
266500160503  "  X*ex R027*********** MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
266600160503  "  X*ex R027           else
266700160503  "  X*ex R027           MOVE      *ON           $ESCI                          sospese READC
266800160503  "  X*ex R027           endif
266900160503  "  X*ex R027
267000160503  "  X*ex R027           ENDSL
267100160503  "  X*ex R027
267200160503  "  X*ex R027 operazione eseguite dal pgm chiamato
267300160503  "  X*ex R027
267400160503  "  X*ex R027           SELECT
267500160503  "  X*ex R027 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
267600160503  "  X*ex R027 OPRD16        WHENEQ    '1'
267700160503  "  X*ex R027           MOVE      OPRD16        WOPR                           avverto pgm chiamant
267800160503  "  X*ex R027
267900160503  "  X*ex R027 se l'opzione con cui è stato chiamato non è la modifica
268000160503  "  X*ex R027 memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
268100160503  "  X*ex R027 OPZD16        IFNE      '02'
268200160503  "  X*ex R027*********** MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
268300160503  "  X*ex R027 se l'opzione con cui è stato chiamato è la modifica
268400160503  "  X*ex R027 aggiorno subito la riga di sfl e non richiedo il ripristino sfl
268500160503  "  X*ex R027           ELSE
268600160503  "  X*ex R027*********** EXSR      AGGS1
268700160503  "  X*ex R027           ENDIF
268800160503  "  X*ex R027
268900160503  "  X*ex R027           ENDSL
269000160503  "  X*ex R027
269100160503  "  X*ex R027 funzione non eseguibile per errore :
269200160503  "  X*ex R027
269300160503  "  X*ex R027           SELECT
269400160503  "  X*ex R027 1 = funzione richiamata chiusa in errore
269500160503  "  X*ex R027 ERRD16        WHENEQ    '1'
269600160503  "  X*ex R027           MOVE      *ON           $ESCI                          sospese READC
269700160503  "  X*ex R027           SETON                                        9399      emiss.SFLMSGID
269800160503  "  X*ex R027 2 = funzione richiamata senza la dovuta autorizzazione
269900160503  "  X*ex R027 ERRD16        WHENEQ    '2'
270000160503  "  X*ex R027           MOVE      *ON           $ESCI                          sospese READC
270100160503  "  X*ex R027           SETON                                        9499      emiss.SFLMSGID
270200160503  "  X*ex R027
270300160503  "  X*ex R027           ENDSL
270400160503  "  X*ex R027
270500160503  "  X*ex R027           ENDSR
270600160429R224 X*ex R027 /EJECT
270601160429R224 C************************************************************
270602160429  "  C* Richiamo pgm per dati anagrafici aggiuntivi
270603160429  "  C************************************************************
270604160503R224 C     CallDatiAgg   BEGSR
270605160429     C*
270606160429     C                   RESET                   SDGSD15DS
270607160429     C*
270608160509     C                   MOVEL     '22'          OPZD15
270609160429     C     CMT040        IFEQ      '1'
270610160429     C                   MOVEL     '1'           CMTD15
270611160429     C                   ELSE
270612160429     C                   MOVEL     '0'           CMTD15
270613160429     C                   ENDIF
270614160429     C*
270615160429     C                   Eval      SoggD15     = S1Cod
270616160429     C                   Eval      TpCallD15   = '1'
270617160429     C                   Eval      TpDatiD15   = '1'
270618160610     C* Tutte
270619160610     C                   Eval      DestD15     = '**'
270620160429     C*
270621160429     C                   MOVEL     SDGSD15DS     KPJBU
270622160429     C*
270623160429     C                   CALL      'SDGSD15R'
270624160429     C                   PARM                    KPJBA
270625160429     C*
270626160429     C                   MOVEL     KPJBU         SDGSD15DS
270627160429     C*
270628160429     C* ritorno da PGM gestione
270629160429     C                   EXSR      RTND15
270630160429     C*
270631160429     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
270632160429     C     *IN99         IFEQ      *ON
270633160429     C                   SETON                                        40
270634160429     C                   ENDIF
270635160429     C*
270636160429     C                   MOVE      *HIVAL        $LASTV
270637160429     C*
270638160429R224 C                   ENDSR
270639160429R224 C/EJECT
270640160429R224 C************************************************************
270641160429 "   C* GESTIONE RITORNO DA PGM DI GESTIONE
270642160429 "   C************************************************************
270643160429R224 C     RTND15        BEGSR
270644160429     C*
270645160429     C* modo di ritorno
270646160429     C*
270647160429     C                   SELECT
270648160429     C* questi modi di utilizzo dei valori di ritorno dal
270649160429     C* pgm di manutenzione rcd di anagrafica sono delle
270650160429     C* proposte, normalmente sempre valide, ma modificabili
270651160429     C* per situazioni particolari
270652160429     C* 1 = F3
270653160429    >C     RETD15        WHENEQ    '1'
270654160429     C                   MOVE      *ON           $ESCI                          sospese READC
270655160429     C                   MOVE      *ON           $FINE                          fine pgm
270656160429    >C                   MOVE      '1'           RET040
270657160429     C* 2 = F12
270658160429    >C     RETD15        WHENEQ    '2'
270659160429     C* se il pgm di manutenzione ha prima una guida, è necessario anche
270660160429     c* questo test, altrimenti prevale il retD04 e il sfl non viene
270661160429     C* ricaricato
270662160429    >C     OPRD15        ANDNE     '1'
270663160429     C*
270664160429     C* se l'opzione con cui è stato chiamato non è la modifica
270665160429     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
270666160429    >C     OPZD15        IFNE      '02'
270667160429     C*********          MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
270668160429     C                   else
270669160429     C                   MOVE      *ON           $ESCI                          sospese READC
270670160429     C                   endif
270671160429     C*
270672160429     C                   ENDSL
270673160429     C*
270674160429     C* operazione eseguite dal pgm chiamato
270675160429     C*
270676160429     C                   SELECT
270677160429     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
270678160429    >C     OPRD15        WHENEQ    '1'
270679160503    >C                   MOVE      OPRD15        WOPR                           avverto pgm chiamant
270680160429     C*
270681160429     C* se l'opzione con cui è stato chiamato non è la modifica
270682160429     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
270683160429    >C     OPZD15        IFNE      '02'
270684160429     C**********         MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
270685160429     C* se l'opzione con cui è stato chiamato è la modifica
270686160429     C* aggiorno subito la riga di sfl e non richiedo il ripristino sfl
270687160429     C                   ELSE
270688160429     C***********        EXSR      AGGS1
270689160429     C                   ENDIF
270690160429     C*
270691160429     C                   ENDSL
270692160429     C*
270693160429     C* funzione non eseguibile per errore :
270694160429     C*
270695160429     C                   SELECT
270696160429     C* 1 = funzione richiamata chiusa in errore
270697160429    >C     ERRD15        WHENEQ    '1'
270698160429     C                   MOVE      *ON           $ESCI                          sospese READC
270699160429     C                   SETON                                        9399      emiss.SFLMSGID
270700160429     C* 2 = funzione richiamata senza la dovuta autorizzazione
270701160429    >C     ERRD15        WHENEQ    '2'
270702160429     C                   MOVE      *ON           $ESCI                          sospese READC
270703160429     C                   SETON                                        9499      emiss.SFLMSGID
270704160429     C*
270705160429     C                   ENDSL
270706160429     C*
270707160429R224 C                   ENDSR
270708160429R224 C/EJECT
270709960202     C************************************************************
270800960202     C* GESTIONE RITORNO
270900960202     C************************************************************
271000960202     C     RTN000        BEGSR
271100960202     C*
271200960202     C* modo di ritorno
271300960202     C*
271400960202     C                   SELECT
271500960202     C* questi modi di utilizzo dei valori di ritorno dal
271600960202     C* pgm di manutenzione rcd di anagrafica sono delle
271700960202     C* proposte, normalmente sempre valide, ma modificabili
271800960202     C* per situazioni particolari
271900960202     C* 1 = F3
272000960202     C* non è possibile tornare con F3 dal filtro
272100960202     C     RET043        WHENEQ    '1'
272200960202     C                   MOVE      *ON           $ESCI                          sospese READC
272300960202     C                   MOVE      *ON           $FINE                          fine pgm
272400960202     C* 2 = F12
272500960202     C     RET043        WHENEQ    '2'
272600960202     C                   MOVE      *ON           $ESCI                          sospese READC
272700960202     C*
272800960202     C                   ENDSL
272900960202     C*
273000960202     C* operazione eseguite dal pgm chiamato
273100960202     C*
273200960202     C                   SELECT
273300960202     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
273400960202     C     OPR043        WHENEQ    '1'
273500960202     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
273600960202     C                   MOVE      OPR043        WOPR                           avverto pgm chiamant
273700960202     C*
273800960202     C                   ENDSL
273900960202     C*
274000960202     C* funzione non eseguibile per errore :
274100960202     C*
274200960202     C                   SELECT
274300960202     C* 1 = funzione richiamata chiusa in errore
274400960202     C     ERR043        WHENEQ    '1'
274500960202     C                   MOVE      *ON           $ESCI                          sospese READC
274600960202     C                   SETON                                        5299      emiss.SFLMSGID
274700960202     C* 2 = funzione richiamata senza la dovuta autorizzazione
274800960202     C     ERR043        WHENEQ    '2'
274900960202     C                   MOVE      *ON           $ESCI                          sospese READC
275000960202     C                   SETON                                        5499      emiss.SFLMSGID
275100960202     C*
275200960202     C                   ENDSL
275300960202     C*
275400960202     C                   ENDSR
275500960202     C/EJECT
275600950301     C************************************************************
275700950301     C* GESTIONE RITORNO DA FILTRO
275800950301     C************************************************************
275900950301     C     RTN043        BEGSR
276000970217     C*
276100970217     C* operazione eseguite dal pgm chiamato
276200970217     C*
276300970217     C                   SELECT
276400970217     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
276500970217     C     OPR043        WHENEQ    '1'
276600970217     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
276700970217     C                   MOVE      OPR043        WOPR                           avverto pgm chiamant
276800970630     c                   move      xscsoc        clnsocieta
276900970630     c                   move      sog043        clnsogg
277000970630     c                   move      xscsoc        Frnsocieta
277100970630     c                   move      sog043        frnsogg
277200970630     c* se società non fittizia
277300001009b0278c*****              if        xscscf    <> 'S'
277400001009b0278c                   if        xscscf    <> *on
277500970217     c* se eseguito un aggiornamento, eseguo loop di lettura dei codici
277600970630     C* collegati al soggetto della stessa società
277700970630     C* e per ognuno, a seconda del suo stato,
277800970217     c* cliente o fornitore, viene richiamato il pgm stabilito nei parametri
277900970630     c* del modulo
278000970630     c* CLIENTE
278100970630     C     k02cln07      setll     ancln07l
278200970630     c                   DO        *HIVAL
278300970630    >C     k02cln07      reade     ancln07l                               21
278400970630     C   21              LEAVE
278500970217     c                   movel     p020dscli     p020ds
278600970630  2  c                   if        pgm020 <> *blanks and
278700970630     c                             clnlineav = *blanks and
278800970630     c                             clnfiliale= 0
278900970217     c                   eval      opz000 = opz043
279000970217     c                   eval      societa000 = xscsoc
279100970630     c                   eval      ksc000     = clnksc
279200970625     c                   eval      prv000     = 'S'
279300970217     c                   eval      kpjbu = ana000ds
279400970217     c                   call      'X38CALL'
279500970217     C                   parm                    pgm020
279600970217     C                   parm                    lib020
279700970217     C                   parm                    tpa020
279800970217     C                   parm                    kpjba
279900970217     c                   eval      ana000ds = kpjbu
280000970217     c                   eval      err043 = Err000
280100970217     c                   eval      ret043 = Ret000
280200970217 2e  c                   endif
280300970630     c                   enddo
280400970217     c* fornitore
280500970630     C     k02frn03      setll     anfrn03l
280600970630     c                   DO        *HIVAL
280700970630    >C     k02frn03      reade     anfrn03l                               21
280800970630     C   21              LEAVE
280900970217     c                   movel     p020dsfor     p020ds
281000970630  2  c                   if        pgm020 <> *blanks   and
281100970630     c                             frnfiliale= 0
281200970217     c                   eval      opz000 = opz043
281300970217     c                   eval      societa000 = xscsoc
281400970630     c                   eval      ksc000     = frnksc
281500970625     c                   eval      prv000     = 'S'
281600970217     c                   eval      kpjbu = ana000ds
281700970217     c                   call      'X38CALL'
281800970217     C                   parm                    pgm020
281900970217     C                   parm                    lib020
282000970217     C                   parm                    tpa020
282100970217     C                   parm                    kpjba
282200970217     c                   eval      ana000ds = kpjbu
282300970217     c                   eval      err043 = Err000
282400970217     c                   eval      ret043 = Ret000
282500970217 2e  c                   endif
282600970217     C                   enddo
282700970630     C*
282800970630     C                   ENDIF
282900970217     C*
283000970217     C                   ENDSL
283100950301     C*
283200970217     C*Cmodo di ritorno
283300950301     C*
283400950301     C                   SELECT
283500950301     C* questi modi di utilizzo dei valori di ritorno dal
283600950301     C* pgm di manutenzione rcd di anagrafica sono delle
283700950301     C* proposte, normalmente sempre valide, ma modificabili
283800950301     C* per situazioni particolari
283900950301     C* 1 = F3
284000950301     C* non è possibile tornare con F3 dal filtro
284100950301     C     RET043        WHENEQ    '1'
284200950301     C                   MOVE      *ON           $ESCI                          sospese READC
284300950301     C                   MOVE      *ON           $FINE                          fine pgm
284400950301     C* 2 = F12
284500950301     C     RET043        WHENEQ    '2'
284600950301     C                   MOVE      *ON           $ESCI                          sospese READC
284700950301     C*
284800950301     C                   ENDSL
284900950301     C*
285000950301     C* funzione non eseguibile per errore :
285100950301     C*
285200950301     C                   SELECT
285300950301     C* 1 = funzione richiamata chiusa in errore
285400950301     C     ERR043        WHENEQ    '1'
285500950301     C                   MOVE      *ON           $ESCI                          sospese READC
285600950301     C                   SETON                                        5299      emiss.SFLMSGID
285700950301     C* 2 = funzione richiamata senza la dovuta autorizzazione
285800950301     C     ERR043        WHENEQ    '2'
285900950301     C                   MOVE      *ON           $ESCI                          sospese READC
286000950301     C                   SETON                                        5499      emiss.SFLMSGID
286100950301     C*
286200950301     C                   ENDSL
286300950301     C*
286400950301     C* disattivo il flag di avvenuta EXFMT dopo il richiamo a pgm
286500950301     C* di sola finestra
286600950301     C                   MOVEL     '0'           WEXF
286700950301     C*
286800950301     C* imposta sul SFL lo stato dei bottoni passato dal filtro
286900950301     C                   MOVEL     B01043        H1B01
287000950301     C                   MOVEL     E01043        H1E01
287100950301     C                   MOVEL     B02043        H1B02
287200950301     C                   MOVEL     E02043        H1E02
287300950301     C                   MOVEL     B03043        H1B03
287400950301     C                   MOVEL     E03043        H1E03
287500950303     C                   MOVEL     B04043        H1B04
287600950303     C                   MOVEL     E04043        H1E04
287700950303     C                   MOVEL     B05043        H1B05
287800950303     C                   MOVEL     E05043        H1E05
287900950301     C*
288000950301     C                   ENDSR
288100950320     C/EJECT
288200950320     C************************************************************
288300950428     C* GESTIONE RITORNO DA PGM VARIAZIONI ESTREMI FISCALI
288400950320     C************************************************************
288500950428     C     RTN057        BEGSR
288600950320     C*
288700950320     C* modo di ritorno
288800950320     C*
288900950320     C                   SELECT
289000950320     C* questi modi di utilizzo dei valori di ritorno dal
289100950320     C* pgm di manutenzione rcd di anagrafica sono delle
289200950320     C* proposte, normalmente sempre valide, ma modificabili
289300950320     C* per situazioni particolari
289400950320     C* 1 = F3
289500950428     C     RET057        WHENEQ    '1'
289600950320     C                   MOVE      *ON           $ESCI                          sospese READC
289700950320     C                   MOVE      *ON           $FINE                          fine pgm
289800950320     C* 2 = F12
289900950428     C     RET057        WHENEQ    '2'
290000950320     C                   MOVE      *ON           $ESCI                          sospese READC
290100950320     C*
290200950320     C                   ENDSL
290300950320     C*
290400950320     C* operazione eseguite dal pgm chiamato
290500950320     C*
290600950320     C                   SELECT
290700950320     C* 1 = eseguito aggiornamento
290800950428     C     OPR057        WHENEQ    '1'
290900950320     C*
291000950320     C                   ENDSL
291100950320     C*
291200950320     C* funzione non eseguibile per errore :
291300950320     C*
291400950320     C                   SELECT
291500950320     C* 1 = funzione richiamata chiusa in errore
291600950428     C     ERR057        WHENEQ    '1'
291700950320     C                   MOVE      *ON           $ESCI                          sospese READC
291800950320     C                   SETON                                        5299      emiss.SFLMSGID
291900950320     C* 2 = funzione richiamata senza la dovuta autorizzazione
292000950428     C     ERR057        WHENEQ    '2'
292100950320     C                   MOVE      *ON           $ESCI                          sospese READC
292200950320     C                   SETON                                        5499      emiss.SFLMSGID
292300950320     C*
292400950320     C                   ENDSL
292500950320     C*
292600950320     C                   ENDSR
292700950404     C/EJECT
292800950428     C************************************************************
292900950428     C* GESTIONE RITORNO DA PGM INVIO
293000950428     C************************************************************
293100950428     C     RTN727        BEGSR
293200950428     C*
293300950428     C* modo di ritorno
293400950428     C*
293500950428     C                   SELECT
293600950428     C* questi modi di utilizzo dei valori di ritorno dal
293700950428     C* pgm di manutenzione rcd di anagrafica sono delle
293800950428     C* proposte, normalmente sempre valide, ma modificabili
293900950428     C* per situazioni particolari
294000950428     C* 1 = F3
294100950428     C     RET727        WHENEQ    '1'
294200950428     C                   MOVE      *ON           $ESCI                          sospese READC
294300950428     C                   MOVE      *ON           $FINE                          fine pgm
294400950428     C* 2 = F12
294500950428     C     RET727        WHENEQ    '2'
294600950428     C                   MOVE      *ON           $ESCI                          sospese READC
294700950428     C*
294800950428     C                   ENDSL
294900950428     C*
295000950428     C* operazione eseguite dal pgm chiamato
295100950428     C*
295200950428     C                   SELECT
295300950428     C* 1 = eseguito aggiornamento
295400950428     C     OPR727        WHENEQ    '1'
295500950428     C*
295600950428     C                   ENDSL
295700950428     C*
295800950428     C* funzione non eseguibile per errore :
295900950428     C*
296000950428     C                   SELECT
296100950428     C* 1 = funzione richiamata chiusa in errore
296200950428     C     ERR727        WHENEQ    '1'
296300950428     C                   MOVE      *ON           $ESCI                          sospese READC
296400950428     C                   SETON                                        5299      emiss.SFLMSGID
296500950428     C* 2 = funzione richiamata senza la dovuta autorizzazione
296600950428     C     ERR727        WHENEQ    '2'
296700950428     C                   MOVE      *ON           $ESCI                          sospese READC
296800950428     C                   SETON                                        5499      emiss.SFLMSGID
296900950428     C*
297000950428     C                   ENDSL
297100950428     C*
297200950428     C* disattivo il flag di avvenuta EXFMT dopo il richiamo a pgm
297300950428     C* di sola finestra
297400950428     C                   MOVEL     '0'           WIN727
297500950428     C*
297600950428     C                   ENDSR
297700950428     C/EJECT
297800950404     C************************************************************
297900950404     C* GESTIONE RITORNO DA PGM GESTIONE CONTATTI
298000950404     C************************************************************
298100950404     C     RTN060        BEGSR
298200950404     C*
298300950404     C* modo di ritorno
298400950404     C*
298500950404     C                   SELECT
298600950404     C* questi modi di utilizzo dei valori di ritorno dal
298700950404     C* pgm di manutenzione rcd di anagrafica sono delle
298800950404     C* proposte, normalmente sempre valide, ma modificabili
298900950404     C* per situazioni particolari
299000950404     C* 1 = F3
299100950404     C     RET060        WHENEQ    '1'
299200950404     C                   MOVE      *ON           $ESCI                          sospese READC
299300950404     C                   MOVE      *ON           $FINE                          fine pgm
299400950404     C* 2 = F12
299500950404     C     RET060        WHENEQ    '2'
299600950404     C                   MOVE      *ON           $ESCI                          sospese READC
299700950404     C*
299800950404     C                   ENDSL
299900950404     C*
300000950404     C* operazione eseguite dal pgm chiamato
300100950404     C*
300200950404     C                   SELECT
300300950404     C* 1 = eseguito aggiornamento
300400950404     C     OPR060        WHENEQ    '1'
300500950404     C*
300600950404     C                   ENDSL
300700950404     C*
300800950404     C* funzione non eseguibile per errore :
300900950404     C*
301000950404     C                   SELECT
301100950404     C* 1 = funzione richiamata chiusa in errore
301200950404     C     ERR060        WHENEQ    '1'
301300950404     C                   MOVE      *ON           $ESCI                          sospese READC
301400950404     C                   SETON                                        5299      emiss.SFLMSGID
301500950404     C* 2 = funzione richiamata senza la dovuta autorizzazione
301600950404     C     ERR060        WHENEQ    '2'
301700950404     C                   MOVE      *ON           $ESCI                          sospese READC
301800950404     C                   SETON                                        5499      emiss.SFLMSGID
301900950404     C*
302000950404     C                   ENDSL
302100950404     C*
302200950404     C                   ENDSR
302300950301     C/EJECT
302400940131     C************************************************************
302500940131     C* GESTIONE VALORI ASSUNTI
302600940131     C************************************************************
302700940131     C     GESD2         BEGSR
302800940131     C*
302900941118   B1C     $INZD2        IFEQ      *ON
303000940131     C                   EXSR      INZD2
303100940223     C                   MOVE      *OFF          $INZD2
303200941118   E1C                   ENDIF
303300940202     C*
303400941118   B1C     $LASTV        IFNE      'D2'
303500940923     C                   EXSR      GESPIE
303600940202     C                   MOVE      $LASTV        $LASTG
303700940202     C                   MOVE      'D2'          $LASTV
303800941118   E1C                   ENDIF
303900940131     C*
304000940707     C* se esistono errori sulla videata
304100940707     C* emetto la write del formato a indicatori spenti per vedere
304200940707     C* le eventuali decodifiche
304300941118   B1C     *IN99         IFEQ      *ON
304400940707     C                   MOVEA     *IN           WIN
304500940707     C                   MOVE      *ALL'0'       IN5098           49
304600940707     C                   MOVEA     IN5098        *IN(50)
304700940707     C                   WRITE     FMTD2
304800940707     C                   MOVEA     WIN           *IN
304900941118   E1C                   ENDIF
305000940707     C*
305100940131     C                   EXFMT     FMTD2
305200940131     C*
305300941118   B1C                   SELECT
305400940131     C* F3=Fine
305500940506     C     $TASTO        WHENEQ    F03
305600940309     C                   EXSR      F03D2
305700940131     C* F12=Ritorno
305800940506     C     $TASTO        WHENEQ    F12
305900940309     C                   EXSR      F12D2
306000940916     C* F24=Altri tasti
306100940916     C     $TASTO        WHENEQ    F24
306200940916     C                   EXSR      F24D2
306300940131     C*
306400940131     C                   OTHER
306500940131     C                   EXSR      CTRD2
306600941118   B2C     *IN99         IFEQ      *OFF
306700940211     C                   MOVE      $LASTG        $GEST
306800941118   B3C                   SELECT
306900940202     C     $LASTG        WHENEQ    'S1'
307000940131     C* se è stato modificato l'ordinamento di default, ricarico la lista
307100941118   B4C     $ORD          IFNE      D2ORD
307200940223     C                   MOVE      *ON           $INZS1
307300940302     C                   MOVE      *BLANK        W1POS
307400941118   E4C                   ENDIF
307500940202     C     $LASTG        WHENEQ    'D1'
307600940318     C* per ora niente
307700941118   E3C                   ENDSL
307800940131     C* definisco i nuovi valori di default
307900940201     C                   Z-ADD     D2LOP         $LOP
308000940131     C                   Z-ADD     D2ORD         $ORD
308100950301     C                   MOVEA     KHI           CHI
308200950301     C                   MOVEA     KHM           CHM
308300950301     C     D2ESPI        IFEQ      SI
308400950301     C                   MOVE      *ON           $ESPI
308500950301     C                   ELSE
308600950301     C                   MOVE      *OFF          $ESPI
308700950301     C                   ENDIF
308800950301     C     D2ESPM        IFEQ      SI
308900950301     C                   MOVE      *ON           $ESPM
309000950301     C                   ELSE
309100950301     C                   MOVE      *OFF          $ESPM
309200950301     C                   ENDIF
309300950301     C*
309400941118   E2C                   ENDIF
309500940131     C*
309600941118   E1C                   ENDSL
309700940131     C*
309800940131     C                   ENDSR
309900940223     C/EJECT
310000940131     C************************************************************
310100940211     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
310200940131     C************************************************************
310300940201     C     INZD2         BEGSR
310400940131     C*
310500940131     C                   CLEAR                   FMTD2
310600940201     C                   MOVEL     $LOP          D2LOP
310700940831     C                   MOVE      $ORD          D2ORD
310800950301     C                   MOVEA     CHI           KHI
310900950301     C                   MOVEA     CHM           KHM
311000950301     C     $ESPI         IFEQ      *ON
311100950301     C                   MOVE      SI            D2ESPI
311200950301     C                   ELSE
311300950301     C                   MOVE      NO            D2ESPI
311400950301     C                   ENDIF
311500950301     C     $ESPM         IFEQ      *ON
311600950301     C                   MOVE      SI            D2ESPM
311700950301     C                   ELSE
311800950301     C                   MOVE      NO            D2ESPM
311900950301     C                   ENDIF
312000940131     C*
312100940131     C                   ENDSR
312200940309     C/EJECT
312300940309     C************************************************************
312400940309     C* GESTIONE F03 VIDEO D2
312500940309     C************************************************************
312600940309     C     F03D2         BEGSR
312700940309     C*
312800940309     C                   MOVE      *ON           $FINE
312900940325     C* fine programma
313000940325     C                   EXSR      ENDPGM
313100940309     C*
313200940309     C                   ENDSR
313300940309     C/EJECT
313400940309     C************************************************************
313500940309     C* GESTIONE F12 VIDEO D2
313600940309     C************************************************************
313700940309     C     F12D2         BEGSR
313800940309     C*
313900940309     C                   MOVE      $LASTG        $GEST
314000940309     C*
314100940309     C                   ENDSR
314200940916     C/EJECT
314300940916     C************************************************************
314400940916     C* GESTIONE F24 VIDEO D2
314500940916     C************************************************************
314600940916     C     F24D2         BEGSR
314700940916     C*
314800940916     C                   MOVEA     $FX3          $FX
314900940916     C                   MOVEA     $FC3          $FC
315000940916     C                   MOVEA     $FR3          $FR
315100940916     C                   Z-ADD     $ULKD2        $ULK
315200940929     C                   Z-ADD     56            $L01
315300940929     C                   Z-ADD     56            $L02
315400940916     C                   EXSR      RIEKEY
315500940916     C                   Z-ADD     $ULK          $ULKD2            3 0
315600940916     C                   MOVEA     $FC           $FC3
315700940916     C                   MOVEL     $KEY1         Z3KE1
315800060224A1148C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
315900940916     C                   MOVEL     $KEY2         Z3KE2
316000940916     C                   WRITE     FMTZ3
316100940916     C*
316200940916     C                   ENDSR
316300940223     C/EJECT
316400940131     C************************************************************
316500940131     C* CONTROLLO VIDEATA VALORI ASSUNTI
316600940131     C************************************************************
316700940131     C     CTRD2         BEGSR
316800940131     C*
316900940131     C                   SETOFF                                       99
317000940131     C* non imposto pos. del cursore
317100940131     C                   Z-ADD     0             H1RIGA
317200940131     C                   Z-ADD     0             H1COLO
317300940131     C*
317400940131     C* OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
317500940201     C     D2LOP         IFEQ      0
317600940131     C                   SETON                                          5099
317700941027     C                   ENDIF
317800940131     C*
317900940131     C* OBBLIGATORIO VALORE PER ORDINAMENTO
318000940131     C     D2ORD         IFEQ      0
318100940131     C                   SETON                                          5199
318200941027     C                   ENDIF
318300950301     C*
318400950301     C* CTRL VALIDITA' SEQUENZE DI SCELTA DATI
318500950301     C     1             DO        3             X
318600950301     C     X             ADD       1             X1                3 0
318700950301     C     X1            DO        4             Y
318800950301     C     KHI(X)        IFEQ      KHI(Y)
318900950301     C     KHI(X)        ANDNE     0
319000950301     C                   SETON                                        5299
319100950301     C                   LEAVE
319200950301     C                   ENDIF
319300950301     C                   ENDDO
319400950301     C     *IN52         IFEQ      *ON
319500950301     C                   LEAVE
319600950301     C                   ENDIF
319700950301     C                   ENDDO
319800950301     C*
319900950301     C     1             DO        3             X
320000950301     C     X             ADD       1             X1
320100950301     C     X1            DO        4             Y
320200950301     C     KHM(X)        IFEQ      KHM(Y)
320300950301     C     KHM(X)        ANDNE     0
320400950301     C                   SETON                                        5399
320500950301     C                   LEAVE
320600950301     C                   ENDIF
320700950301     C                   ENDDO
320800950301     C     *IN53         IFEQ      *ON
320900950301     C                   LEAVE
321000950301     C                   ENDIF
321100950301     C                   ENDDO
321200950301     C*
321300950301     C* OBBLIGATORIO VALORE PER ESPANSIONE IMMISSIONE
321400950301     C     D2ESPI        IFNE      SI
321500950301     C     D2ESPI        ANDNE     NO
321600950301     C                   SETON                                          5499
321700950301     C                   ENDIF
321800950301     C*
321900950301     C* OBBLIGATORIO VALORE PER ESPANSIONE MANUTENZIONE
322000950301     C     D2ESPM        IFNE      SI
322100950301     C     D2ESPM        ANDNE     NO
322200950301     C                   SETON                                          5599
322300950301     C                   ENDIF
322400940131     C*
322500940131     C                   ENDSR
322600940223     C/EJECT
322700940201     C************************************************************
322800940201     C* AGGIORNAMENTO VALORI ASSUNTI
322900940201     C************************************************************
323000940201     C     AGGMPE        BEGSR
323100940201     C*
323200940201     C                   CLEAR                   ANMPE000
323300941118     C                   MOVEL     XSCSOC        MPESOC
323400941118     C                   MOVEL     DSPGM         MPEPGM
323500940201     C                   MOVEL     KNMUS         MPEPRF
323600941013     C*
323700941013      *Si memorizzano i dati di default a seconda delle modalità con
323800941013      *cui il pgm è usato
323900941013      *MPETIP = 'PX'  pgm richiamato come lista (CMD4)
324000941013      *MPETIP = 'PV'  pgm richiamato da menù
324100941013      *MPETIP = 'PB'  x lancio Batch
324200941013     C                   SELECT
324300941130     C     OPZ040        WHENEQ    '01'
324400941013     C                   MOVEL     'PX'          MPETIP
324500941013     C                   OTHER
324600941013     C                   MOVEL     'PV'          MPETIP
324700941013     C                   ENDSL
324800941118     C     K4MPE2        CHAIN     ANMPE02L                           21
324900940201     C                   MOVEL     DSMPE         MPEPAR
325000940201     C     *IN21         IFEQ      *ON
325100940201     C                   WRITE     ANMPE000
325200940201     C                   ELSE
325300940201     C                   UPDATE    ANMPE000
325400941027     C                   ENDIF
325500940201     C*
325600940201     C                   ENDSR
325700940223     C/EJECT
325800940131     C************************************************************
325900940131     C* OPERAZIONI INIZIALI
326000940131     C************************************************************
326100940131     C     *INZSR        BEGSR
326200000114A0735C     *DMY          Move      Udate         ZDateISO
326300000114A0735C     *JobRun       Move      ZDateISO      ZDate             6 0
326400940131     C*
326500940127     C* Reperimento parametri
326600940127     C*
326700940117     C     *ENTRY        PLIST
326800940117     C                   PARM                    KPJBA
326900940225     C*
327000940223     C* Reperimento opzioni
327100940127     C*
327200940128     C                   EXSR      REPOPZ
327300940506     C* Reperimento tasti di funzione
327400940506     C*
327500940506     C                   EXSR      REPKEY
327600940506     C*
327700940127     C*
327800960202     C* Reperimento dati società
327900960202     C*
328000960202    >C                   MOVEL     'SOC001'      TIPXSC
328100960202     C                   MOVEL     *BLANK        SOCXSC
328200960202     C                   EXSR      REPSOC
328300960202     C     RTNXSC        IFNE      '1'
328400960202    >C                   MOVEL     XSOCDS        SOC001
328500960202     C                   ELSE
328600960202     C                   MOVE      *ON           $FINE
328700960202     C                   EXSR      ENDPGM
328800960202     C                   ENDIF
328900960202     C*
329000960202     C* Chiamata a routine per il reperimento delle opzioni e funzioni
329100960202     C* personalizzate per S1
329200960202     C                   CAllb     'X46USREXT'
329300960202     C                   PARM                    $OP1
329400960202     C                   PARM                    $TX1
329500960202     C                   PARM                    $FL1
329600960202     C                   PARM                    $PGO1
329700960202     C                   PARM      $dimOp        NumOpz
329800960202     C                   PARM                    $FX2
329900960202     C                   PARM                    $FC2
330000960202     C                   PARM                    $PGF2
330100960202     C                   PARM                    DsPgm
330200960202     C                   PARM                    xsccut
330300960202     C                   PARM                    xscgrp
330400960202     C*
330500960202     C                   Z-add     1             OPZLEN
330600960202     C     *blank        Lookup    $TX1(opzlen)                           21
330700960202     C                   if        *in21 = *on
330800960202     C                   sub       1             OPZLEN
330900960202     C                   else
331000960202     C                   Z-add     $DimOp        OPZLEN
331100960202     C                   endif
331200960202     C*
331300940117     C                   ENDSR
331400941104     C/EJECT
331500941104      ************************************************************
331600941104      * Reperimento dati società
331700941104      ************************************************************
331800941104     C     REPSOC        BEGSR
331900941104      *
332000961003     C                   CALLb     'XSOC'
332100941104     C                   PARM                    TIPXSC            6
332200941104     C                   PARM                    SOCXSC            3
332300941104     C                   PARM                    CDSXSC            9 0
332400941104     C                   PARM                    MODXSC            3
332500941104     C                   PARM      *BLANKS       RTNXSC            1
332600941104     C                   PARM                    XSOCDS
332700941104     C                   PARM                    KPJBA
332800941104      *
332900941104     C                   ENDSR
333000940223     C/EJECT
333100940223     C************************************************************
333200940223     C* INIZIALIZZAZIONE VARIABILI
333300940223     C************************************************************
333400940223     C     INZVAR        BEGSR
333500940223     C*
333600941115      *
333700941115      * Reperimento dati società
333800941115      *
333900941130     C                   MOVEL     'SOC001'      TIPXSC
334000941115     C                   MOVEL     *BLANK        SOCXSC
334100941115     C                   EXSR      REPSOC
334200941115     C     RTNXSC        IFNE      '1'
334300941115     C                   MOVEL     XSOCDS        SOC001
334400941115     C                   ELSE
334500941115     C                   RETURN
334600941115     C                   ENDIF
334700940729     C*
334800940729     C* Dati da PGM chiamante
334900940729     C*
335000941130     C                   MOVEL     KPJBU         A040DS
335100970217     C*
335200970217     C* Reperimento parametri di gestione clienti
335300970217     C*
335400970217     C                   MOVEL     *BLANK        XPARIC
335500970217     C                   MOVEL     XSCSOC        XPASOC
335600970217     C                   MOVEL     'ANCLN00F'    XPAPAR
335700970217     C                   MOVEL     *BLANK        XPAOUT
335800040723B0733C                   clear                   XPAMSG
335900970217     C                   EXSR      REPPAR
336000970217     C     XPAERR        IFEQ      '0'
336100970217     C                   MOVEL     XPAOUT        P020DS
336200970217     C                   MOVEL     P020DS        P020DSCLI
336300970217     C                   ELSE
336400970217     C                   RETURN
336500970217     C                   ENDIF
336600970217     C*
336700970217     C* Reperimento parametri di gestione fornitori
336800970217     C*
336900970217     C                   MOVEL     *BLANK        XPARIC
337000970217     C                   MOVEL     XSCSOC        XPASOC
337100970217     C                   MOVEL     'ANFRN00F'    XPAPAR
337200970217     C                   MOVEL     *BLANK        XPAOUT
337300040723B0733C                   clear                   XPAMSG
337400970217     C                   EXSR      REPPAR
337500970217     C     XPAERR        IFEQ      '0'
337600970217     C                   MOVEL     XPAOUT        P020DS
337700970217     C                   MOVEL     P020DS        P020DSFOR
337800970217     C                   ELSE
337900970217     C                   RETURN
338000970217     C                   ENDIF
338100040723B0733C*
338200040723  "  C* Reperimento parametri di gestione JCRM1
338300040723  "  C*
338400040723  "  c                   clear                   EsisteJCRM        1
338500040723  "  C                   MOVEL     *BLANK        XPARIC
338600040723  "  C                   MOVEL     XSCSOC        XPASOC
338700040723  "  C                   MOVEL(P)  'JCRM1'       XPAPAR
338800040723  "  C                   MOVEL     *BLANK        XPAOUT
338900040723  "  c* chiedo di NON mandare messaggio se non esiste parametro mod.
339000040723  "  C                   MOVEL     *OFF          XPAMSG
339100040723  "  C                   EXSR      REPPAR
339200040723  "  C     XPAERR        IFEQ      '0'
339300040723  "  c                   eval      EsisteJCRM = *on
339400040723B0733C                   ENDIF
339500941130     C*
339600941124     C* Reperimento autorizzazioni
339700941124     C*
339800941130     C                   EXSR      REPAUT
339900941026     C*
340000941026     C* Reperimento valori default utente
340100941026     C*
340200941026     C                   EXSR      REPDFT
340300940506     C*
340400940506     C* Definizione delle opzioni utilizzabili a pgm
340500940506     C*
340600940506     C                   EXSR      DEFOPZ
340700940223     C* Variabili per gestione videate
340800940223     C*
340900940223     C                   MOVE      *OFF          $FINE                          fine pgm
341000940223     C                   MOVE      *OFF          $INZD1                         inizializzione D1
341100940223     C                   MOVE      *OFF          $INZS1                         inizializzione S1
341200940223     C                   MOVE      *OFF          $INZD2                         inizializzione D2
341300940223     C                   MOVE      *OFF          $EFILE                         memo fine file
341400940223     C                   MOVE      *OFF          $ESCI                          rich.uscita ciclo
341500940223     C                   MOVE      *OFF          $RCDOK                         rcd letto valido
341600940509     C                   MOVE      *HIVAL        $LASTV
341700940506     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
341800940506     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
341900940916     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
342000940223     C*
342100940318     C                   MOVE      *ON           $INZD1
342200940318     C                   MOVE      *ON           $INZS1
342300940831     C*
342400940831     C* se pgm richiamato con chiave univoca impostata,
342500940831     C* forzo la emissione della lista e imposto l'ordinamento
342600941213     C     UQ1040        IFNE      *BLANK
342700940831     C                   Z-ADD     2             $LOP
342800941130     C                   Z-ADD     99            $ORD
342900941130     C                   MOVE      *ON           $UNIQ
343000941130     C                   ELSE
343100941130     C                   MOVE      *OFF          $UNIQ
343200941130     C                   ENDIF
343300941104     C*
343400941104     C* definisco qual'è la prima videata
343500941104     C* (se è la lista devo comunque inizializzare la videata di
343600941104     C*  parzializzazione perchè il sfl viene caricato in base a quei
343700941104     C*  dati)
343800940223     C                   SELECT
343900940223     C     $LOP          WHENEQ    1
344000940223     C                   MOVE      'D1'          $GEST
344100940223     C     $LOP          WHENEQ    2
344200940223     C                   MOVE      'S1'          $GEST
344300941104     C                   ENDSL
344400950308     C*
344500950308     C* Variabili appoggio
344600950308     C*
344700950308     C                   Z-ADD     SFLPAG        WPAG
344800950308     C                   MOVEL     *OFF          WOPR                                       ne S1
344900961003     C                   CALLb     'XDT4000'
345000950308     C                   PARM                    XDTAMG           10
345100000114A0735C*****              PARM      UDATE         XDTGMA            6 0
345200000114A0735C                   PARM      ZDate         XDTGMA            6 0
345300950308     C                   PARM      4             XDTSTA            1 0
345400950308     C                   MOVEL     XDTAMG        WDATE
345500950308     C*                    MOVE *BLANK    WFSC
345600950308     C                   MOVE      XSCON         SI
345700950308     C                   MOVE      XSCOFF        NO
345800950308     C*
345900950307     C* Esegue sempre inizializzazione videata parzializzazioni
346000950307     c* perchè controlla nella ULTOPZ il campo D1FSC
346100950307     C                   EXSR      INZD1
346200950307     C                   MOVE      *OFF          $INZD1
346300940223     C*
346400940223     C* Valorizzazione campi univoci testate
346500940223     C*
346600940223     C                   CLEAR                   FMTT1
346700940223     C                   MOVEL     KNSIF         NOMSIF
346800941109     C                   MOVEL     XSCDSI        NOMDIT
346900941027     C                   MOVEL     DSPGM         NOMPGM
347000940223     C*
347100940223     C                   ENDSR
347200970217     C/EJECT
347300970217     C************************************************************
347400970217     C* REPERIMENTO PARAMETRI GESTIONE
347500970217     C************************************************************
347600970217     C     REPPAR        BEGSR
347700970217     C*
347800970217     C                   CALLb     'XPAR'
347900970217     C                   PARM                    XPASOC            3
348000970217     C                   PARM                    XPAPAR            8
348100970217     C                   PARM                    XPAOUT
348200970217     C                   PARM                    XPAERR            1
348300970217     C                   PARM                    XPARIC            1
348400040723B0733C                   PARM                    KPJBAE          502
348500040723  "  C                   PARM                    XPAOPZ            2
348600040723B0733C                   PARM                    XPAMSG            1
348700970217     C*
348800970217     C                   ENDSR
348900941124     C/EJECT
349000941124     C************************************************************
349100941124     C* REPERIMENTO AUTORIZZAZIONI UTENTE
349200941124     C************************************************************
349300941124     C     REPAUT        BEGSR
349400941124     C*
349500941124     C                   CLEAR                   AUTDS
349600941124     C*
349700941124     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
349800941124     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
349900941124     C                   Z-ADD     0             XCAGRP                         GRUPPO
350000941124     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
350100941202     C                   MOVEL     'ANGSOG'      XCAFNC                         FUNZIONE
350200941202     C                   MOVEL     *ZERO         XCAVFU                         VARIABILE
350300941202     C                   MOVE      'CK'          XCATCK                         TIPO CTRL
350400941202     C                   MOVE      '1'           XCANAU                         RICH.XNOAUT
350500941124     C*
350600961003     C                   CALLb     'XCHKAUT'
350700941124     C                   PARM                    AUTDS
350800941124     C*
350900941124     C* errori/non abilitazione
351000941124     C     XCARTN        IFNE      0
351100970704    >C                   MOVE      '2'           Err040
351200941124     C                   EXSR      ENDPGM
351300941124     C                   ELSE
351400941202     C                   MOVEL     XCADDS        GSOGDS
351500941124     C                   ENDIF
351600941124     C*
351700941124     C                   ENDSR
351800940223     C/EJECT
351900940127     C************************************************************
352000940131     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
352100940127     C************************************************************
352200940131     C     REPDFT        BEGSR
352300940127     C*
352400941118     C                   MOVEL     XSCSOC        MPESOC
352500940201     C                   MOVEL     DSPGM         MPEPGM
352600940131     C                   MOVEL     KNMUS         MPEPRF
352700941027     C*Si reperiscono i dati di default a seconda delle modalità con
352800941027     C*cui il pgm è usato
352900941027     C*MPETIP = 'PX'  pgm richiamato come lista (CMD4)
353000941027     C*MPETIP = 'PV'  pgm richiamato da menù
353100941027     C*MPETIP = 'PB'  x lancio Batch
353200941013     C                   SELECT
353300941130     C     OPZ040        WHENEQ    '01'
353400941013     C                   MOVEL     'PX'          MPETIP
353500941013     C                   OTHER
353600941013     C                   MOVEL     'PV'          MPETIP
353700941013     C                   ENDSL
353800941118     C     K4MPE2        CHAIN(N)  ANMPE02L                           21
353900940131     C     *IN21         IFEQ      *OFF
354000940131     C                   MOVEL     MPEPAR        DSMPE
354100950301     C* valorizzo subito i campi video perchè sono usati come appoggio
354200950301     C* nelle chiamate alle singole opzioni che passano per il filtro
354300950301     C                   MOVEA     CHI           KHI
354400950301     C                   MOVEA     CHM           KHM
354500950301     C* nel caso sia la prima volta che l'utente apre questa anagrafica
354600950301     C* i suoi valori di default vengono decisi a pgm
354700940131     C                   ELSE
354800940201     C                   Z-ADD     1             $LOP
354900940131     C                   Z-ADD     1             $ORD
355000950301     C                   Z-ADD     1             CHI(1)
355100950301     C                   MOVE      *OFF          $ESPI
355200950301     C                   MOVE      *OFF          $ESPM
355300941027     C                   ENDIF
355400940131     C*
355500940131     C                   ENDSR
355600940223     C/EJECT
355700940131     C************************************************************
355800940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
355900940131     C************************************************************
356000940225     C     DEFOPZ        BEGSR
356100940131     C*
356200940225     C* Questa routine deve valorizzare la schiera $FL1
356300940225     C* con S o N a seconda della utilizzabilità o meno
356400940225     C* dell'opzione correlata
356500940225     C*
356600940225     C* Inizialmente si considerano tutte le opzioni come valide
356700940317     C     1             DO        $DIMOP        X
356800960202     C* Imposta tutti i flg, delle opzioni non a *blank, a 'S'
356900960202     C                   If        $TX1(X)  <> *Blank
357000960202     C                   MOVE      'S'           $FL1(X)
357100960202     C                   Else
357200960202     C                   Leave
357300960202     C                   EndIf
357400941027     C                   ENDDO
357500940715     C*
357600940715     C* Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
357700941018     C* con 1 o 0 a seconda della utilizzabilità o meno
357800940715     C* del tasto funzione correlato
357900940715     C*
358000940715     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
358100940715     C* di compilazione
358200940715     C                   RESET                   $FC1
358300940715     C                   RESET                   $FR1
358400940715     C                   RESET                   $FC2
358500940715     C                   RESET                   $FR2
358600940919     C                   RESET                   $FC3
358700940919     C                   RESET                   $FR3
358800940225     C*
358900940225     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
359000940225     C* invalido alcune opzioni
359100940225     C                   SELECT
359200940225     C* Se pgm richiamato in ricerca disattivo :
359300941130     C     OPZ040        WHENEQ    '01'
359400940307     C* F10=Immissione
359500940506     C                   Z-ADD     10            X
359600941018     C                   MOVE      '1'           $FC2(X)
359700940603     C* F3=Fine
359800940603     C                   Z-ADD     3             X
359900940720     C                   MOVE      '0'           $FC2(X)
360000940307     C* 2=Modifica
360100940307     C                   Z-ADD     1             X
360200940225     C     02            LOOKUP    $OP1(X)                                21
360300940225     C     *IN21         IFEQ      *ON
360400941018     C                   MOVE      'S'           $FL1(X)
360500940225     C                   ENDIF
360600940225     C* 3=Copia
360700940225     C                   Z-ADD     1             X
360800940225     C     03            LOOKUP    $OP1(X)                                21
360900940225     C     *IN21         IFEQ      *ON
361000941018     C                   MOVE      'S'           $FL1(X)
361100940225     C                   ENDIF
361200940225     C* 4=Annullamento
361300940225     C                   Z-ADD     1             X
361400940225     C     04            LOOKUP    $OP1(X)                                21
361500940225     C     *IN21         IFEQ      *ON
361600941018     C                   MOVE      'S'           $FL1(X)
361700940225     C                   ENDIF
361800940225     C* Se pgm richiamato in interrogazione disattivo :
361900941130     C     OPZ040        WHENEQ    '05'
362000940307     C* F10=Immissione
362100940615     C                   Z-ADD     10            X
362200940720     C                   MOVE      '0'           $FC2(X)
362300940225     C* 1=Selezione
362400940225     C                   Z-ADD     1             X
362500940225     C     01            LOOKUP    $OP1(X)                                21
362600940225     C     *IN21         IFEQ      *ON
362700940225     C                   MOVE      'N'           $FL1(X)
362800940225     C                   ENDIF
362900940225     C* 2=Modifica
363000940225     C                   Z-ADD     1             X
363100940225     C     02            LOOKUP    $OP1(X)                                21
363200940225     C     *IN21         IFEQ      *ON
363300940225     C                   MOVE      'N'           $FL1(X)
363400940225     C                   ENDIF
363500940225     C* 3=Copia
363600940225     C                   Z-ADD     1             X
363700940225     C     03            LOOKUP    $OP1(X)                                21
363800940225     C     *IN21         IFEQ      *ON
363900940225     C                   MOVE      'N'           $FL1(X)
364000940225     C                   ENDIF
364100940225     C* 4=Annullamento
364200940225     C                   Z-ADD     1             X
364300940225     C     04            LOOKUP    $OP1(X)                                21
364400940225     C     *IN21         IFEQ      *ON
364500940225     C                   MOVE      'N'           $FL1(X)
364600940225     C                   ENDIF
364700941227     C*
364800941227     C* Se pgm non richiamato, disattivo subito :
364900941227     C     OPZ040        WHENEQ    *BLANK
365000941227     C* F8=Selezione seguente
365100941227     C                   Z-ADD     8             X
365200941227     C                   MOVE      '0'           $FC1(X)
365300941227     C                   MOVE      '0'           $FC2(X)
365400941227     C* 1=Selezione
365500941227     C                   Z-ADD     1             X
365600941227     C     01            LOOKUP    $OP1(X)                                21
365700941227     C     *IN21         IFEQ      *ON
365800941227     C                   MOVE      'N'           $FL1(X)
365900941227     C                   ENDIF
366000941227     C*
366100941227     C* In tutti gli altri casi, disattivo subito :
366200940225     C                   OTHER
366300940225     C* 1=Selezione
366400940225     C                   Z-ADD     1             X
366500940225     C     01            LOOKUP    $OP1(X)                                21
366600940225     C     *IN21         IFEQ      *ON
366700940225     C                   MOVE      'N'           $FL1(X)
366800940225     C                   ENDIF
366900940225     C*
367000940225     C                   ENDSL
367100941118     C*
367200941118     C* poi, disattivo a seconda delle autorizzazioni :
367300941202     C* - immissione/copia
367400941202     C     IMMSOG        IFEQ      *OFF
367500941202     C* F10=Immissione
367600941202     C                   Z-ADD     10            X
367700970210     C                   MOVE      '0'           $FC1(X)
367800970210     C                   MOVE      '0'           $FC2(X)
367900941202     C* 3=Copia
368000941202     C                   Z-ADD     1             X
368100941202     C     03            LOOKUP    $OP1(X)                                21
368200941202     C     *IN21         IFEQ      *ON
368300941202     C                   MOVE      'N'           $FL1(X)
368400941202     C                   ENDIF
368500941202     C                   ENDIF
368600941202     C* - manutenzione
368700941202     C     MANSOG        IFEQ      *OFF
368800941202     C* 2=Modifica
368900941202     C                   Z-ADD     1             X
369000941202     C     02            LOOKUP    $OP1(X)                                21
369100941202     C     *IN21         IFEQ      *ON
369200941202     C                   MOVE      'N'           $FL1(X)
369300941202     C                   ENDIF
369400941202     C                   ENDIF
369500941202     C* - annullamento
369600941202     C     ANNSOG        IFEQ      *OFF
369700941202     C* 4=Annullamento
369800941202     C                   Z-ADD     1             X
369900941202     C     04            LOOKUP    $OP1(X)                                21
370000941202     C     *IN21         IFEQ      *ON
370100941202     C                   MOVE      'N'           $FL1(X)
370200941202     C                   ENDIF
370300941202     C                   ENDIF
370400941202     C* - visualizzazione
370500941202     C     VISSOG        IFEQ      *OFF
370600941202     C* 5=Visualizzazione
370700941202     C                   Z-ADD     1             X
370800941202     C     05            LOOKUP    $OP1(X)                                21
370900941202     C     *IN21         IFEQ      *ON
371000941202     C                   MOVE      'N'           $FL1(X)
371100941202     C                   ENDIF
371200941202     C                   ENDIF
371300100831R027 C*
371400100831  "  C*- inserimento
371500100831  "  C                   if        IMMSOG     <> *On     and
371600100831  "  C*- manutenzione
371700100831  "  C                             MANSOG     <> *On     and
371800100831  "  C*- visualizzazione
371900100831  "  C                             VISSOG     <> *On
372000100831  "  C*
372100100831  "  C* disabilito l'opz. 22=Dati aggiuntivi
372200100831  "  C                   Z-ADD     1             X
372300100831  "  C     22            LOOKUP    $OP1(X)                                21
372400100831  "  C     *IN21         IFEQ      *ON
372500100831  "  C                   MOVE      'N'           $FL1(X)
372600100831  "  C                   ENDIF
372700100831  "  C*
372800100831  "  C                   endif
372900100831R027 C*
373000940225     C*
373100940225     C                   ENDSR
373200940225     C/EJECT
373300940225     C************************************************************
373400940225     C* REPERIMENTO OPZIONI
373500940225     C************************************************************
373600940225     C     REPOPZ        BEGSR
373700940225     C*
373800940314     C     1             DO        $DIMOP        X
373900940128     C*
374000940223     C     $MS1(X)       IFNE      *BLANK
374100940128     C*
374200940223     C                   MOVEL     $MS1(X)       IDMSG
374300961003     C                   CALLb     'XRTVM'
374400940201     C                   PARM                    IDMSG            27
374500940201     C                   PARM                    TXTMSG          100
374600940201     C                   PARM                    ERRMSG            1
374700940128     C*
374800940128     C     ERRMSG        IFNE      *ON
374900940223     C                   MOVEL     TXTMSG        $TX1(X)
375000940128     C                   ELSE
375100940223     C                   MOVEL     *ALL'?'       $TX1(X)
375200941027     C                   ENDIF
375300940128     C*
375400941027     C                   ENDIF
375500940128     C*
375600941027     C                   ENDDO
375700940128     C*
375800940128     C                   ENDSR
375900940506     C/EJECT
376000940506     C************************************************************
376100940506     C* REPERIMENTO TASTI DI FUNZIONE
376200940506     C************************************************************
376300940506     C     REPKEY        BEGSR
376400940506     C                   MOVEA     $FM1          $FM
376500950906     C                   MOVEA     $FC1          $FC
376600940506     C                   EXSR      REPKKK
376700940506     C                   MOVEA     $FX           $FX1
376800940506     C*
376900940506     C                   MOVEA     $FM2          $FM
377000950906     C                   MOVEA     $FC2          $FC
377100940506     C                   EXSR      REPKKK
377200940506     C                   MOVEA     $FX           $FX2
377300940919     C*
377400940919     C                   MOVEA     $FM3          $FM
377500950906     C                   MOVEA     $FC3          $FC
377600940919     C                   EXSR      REPKKK
377700940919     C                   MOVEA     $FX           $FX3
377800940919     C                   ENDSR
377900940506     C/EJECT
378000940506     C************************************************************
378100940506     C* REPERIMENTO TASTI DI FUNZIONE
378200940506     C************************************************************
378300940506     C     REPKKK        BEGSR
378400940506     C*
378500940506     C                   CLEAR                   $FX
378600940506     C     1             DO        24            X
378700940506     C*
378800940506     C     $FM(X)        IFNE      *BLANK
378900950906     C     $FC(X)        ANDNE     '0'
379000940506     C*
379100940506     C                   MOVEL     $FM(X)        IDMSG
379200961003     C                   CALLb     'XRTVM'
379300940506     C                   PARM                    IDMSG            27
379400940506     C                   PARM                    TXTMSG          100
379500940506     C                   PARM                    ERRMSG            1
379600940506     C*
379700940506     C     ERRMSG        IFNE      *ON
379800940506     C                   MOVEL     TXTMSG        $FX(X)
379900940506     C                   ELSE
380000940506     C                   MOVEL     *ALL' '       $FX(X)
380100941027     C                   ENDIF
380200940506     C*
380300941027     C                   ENDIF
380400940506     C*
380500941027     C                   ENDDO
380600940506     C*
380700940506     C                   ENDSR
380800940223     C/EJECT
380900940128     C************************************************************
381000940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
381100940128     C************************************************************
381200940223     C     DEFCAM        BEGSR
381300940128     C*
381400940223     C* klist
381500940128     C*
381600940128     C* ANMPE02L
381700941118     C     K4MPE2        KLIST
381800941118     C                   KFLD                    MPESOC
381900941118     C                   KFLD                    MPEPGM
382000940128     C                   KFLD                    MPETIP
382100940128     C                   KFLD                    MPEPRF
382200970630     C*
382300970630     C* ANcln07L
382400970630     C     K02cln07      KLIST
382500970630     C                   KFLD                    clnsogg
382600970630     C                   KFLD                    clnsocieta
382700970630     C* ANfrn03L
382800970630     C     K02frn03      KLIST
382900970630     C                   KFLD                    frnsogg
383000970630     C                   KFLD                    frnsocieta
383100940223     C*
383200940223     C* Variabili per gestione videate
383300940223     C*
383400940223     C                   MOVE      *BLANK        $GEST             2            fmt in gestione
383500940223     C                   MOVE      *BLANK        $FINE             1            fine pgm
383600940223     C                   MOVE      *BLANK        $INZD1            1            inizializzione D1
383700940223     C                   MOVE      *BLANK        $INZS1            1            inizializzione S1
383800940223     C                   MOVE      *BLANK        $INZD2            1            inizializzione D2
383900940223     C                   MOVE      *BLANK        $LASTG            2            fmt di provenienza
384000940223     C                   MOVE      *BLANK        $LASTV            2            ultima emissione
384100940223     C                   MOVE      *BLANK        $EFILE            1            memo fine file
384200940223     C                   MOVE      *BLANK        $ESCI             1            rich.uscita ciclo
384300940223     C                   MOVE      *BLANK        $RCDOK            1            rcd letto valido
384400940224     C                   Z-ADD     0             $PIED1            3 0          piede da emett. D1
384500940224     C                   Z-ADD     0             $PIES1            3 0          piede da emett. S1
384600941130     C                   MOVE      *BLANK        $UNIQ             1            key unique
384700940223     C*
384800940314     C* Variabili appoggio sempre presenti in tutte le anagrafiche
384900940223     C*
385000940223     C     *LIKE         DEFINE    C1RCD         S1NRR
385100940223     C     *LIKE         DEFINE    C1NRR         WSFL
385200940223     C     *LIKE         DEFINE    C1RCD         WMAX
385300940223     C     *LIKE         DEFINE    C1RCD         WPAG
385400940223     C     *LIKE         DEFINE    $INZS1        WINZS1                                     ne S1
385500941130     C     *LIKE         DEFINE    OPR040        WOPR                                       ne S1
385600950301     C     *LIKE         DEFINE    EXF043        WEXF                                       ne S1
385700940223     C                   MOVE      *BLANK        WWGRP             3
385800940223     C                   Z-ADD     0             CURR              2 0
385900940223     C                   Z-ADD     0             CURC              2 0
386000940223     C                   MOVE      *BLANK        WIN              99
386100940314     C     *LIKE         DEFINE    C1POS         W1POS
386200941201     C                   MOVE      *BLANK        SI                1
386300941201     C                   MOVE      *BLANK        NO                1
386400940314     C*
386500940314     C* Variabili appoggio specifiche della singola anagrafica
386600940314     C*
386700941206     C     *LIKE         DEFINE    DTS724        WDATE
386800940314     C* campi per gestione F4=Ricerca
386900941130     C     *LIKE         DEFINE    INDSTA        WWSTA
387000941130     C     *LIKE         DEFINE    INDPRV        WWPRV
387100940223     C* campi per parzializzazioni su rcd (solo alfanumerici)
387200941130     C     *LIKE         DEFINE    SOGDES        DES999
387300941130     C     *LIKE         DEFINE    INDCIT        CIT999
387400941130     C     *LIKE         DEFINE    INDSTA        STA999
387500941130     C     *LIKE         DEFINE    INDPRV        PRV999
387600941130     C     *LIKE         DEFINE    SOGIVA        IVA999
387700941130     C     *LIKE         DEFINE    SOGCDF        CDF999
387800941130     C     *LIKE         DEFINE    SOGCOD        COD999
387900940223     C*
388000940223     C* Indici
388100940223     C*
388200940223     C                   Z-ADD     0             X                 3 0
388300940223     C                   Z-ADD     0             Y                 3 0
388400940127     C*
388500940117     C                   ENDSR
388600940223     C/EJECT
388700100831R027 C* Aggiunti elementi di schiera:
388800100831 "   C* SDGMSG    *LIBL     SDO0026  (schiera $MS1 )  e
388900100831R027 C* 22S      <Dati aggiuntivi    (schiera $OP1 )
389000950906**
389100940718PROMSG    *LIBL     OPZ0001
389200940225PROMSG    *LIBL     OPZ0002
389300940131PROMSG    *LIBL     OPZ0003
389400951004PROMSG    *LIBL     OPZ0124
389500940131PROMSG    *LIBL     OPZ0005
389600950320PROMSG    *LIBL     OPZ0010
389700940831PROMSG    *LIBL     OPZ0014
389800941130PROMSG    *LIBL     OPZ0017
389900941130PROMSG    *LIBL     OPZ0018
390000950404PROMSG    *LIBL     OPZ0095
390100100831SDGMSG    *LIBL     SDO0026             R027
390200950404PROMSG    *LIBL     OPZ0096
390300950301PROMSG    *LIBL     OPZ0040
390400940201**
39050094022501S      <Selezione
39060094022502S      <Modifica
39070094022503S      <Copia
39080094022504S      <Annullamento
39090094022505S      <Visualizzazione
39100095032010S      <Invio
39110094083114S      <Gestione commenti
39120094113017S      <Indirizzi
39130094113018S      <Gestione codici collegati
39140095040419S      <Gestione contatti
39150010083122S      <Dati aggiuntivi               R027
39160095040431S      <Variazione dati fiscali
39170095030140S      <Dati anagrafici
391800940506** TASTI DI FUNZIONE  D1
391900940506PROMSG    *LIBL     KEY0001  01
392000940506PROMSG    *LIBL     KEY0002  02
392100940506PROMSG    *LIBL     KEY0003  03
392200940506PROMSG    *LIBL     KEY0004  04
392300940506PROMSG    *LIBL     KEY0005  05
392400940506PROMSG    *LIBL     KEY0006  06
392500940506PROMSG    *LIBL     KEY0007  07
392600940506PROMSG    *LIBL     KEY0008  08
392700940506PROMSG    *LIBL     KEY0009  09
392800940506PROMSG    *LIBL     KEY0010  10
392900940506PROMSG    *LIBL     KEY0011  11
393000940506PROMSG    *LIBL     KEY0012  12
393100940506PROMSG    *LIBL     KEY0013  13
393200940506PROMSG    *LIBL     KEY0014  14
393300940506PROMSG    *LIBL     KEY0015  15
393400940506PROMSG    *LIBL     KEY0016  16
393500940506PROMSG    *LIBL     KEY0017  17
393600940506PROMSG    *LIBL     KEY0018  18
393700940506PROMSG    *LIBL     KEY0019  19
393800940506PROMSG    *LIBL     KEY0020  20
393900940506PROMSG    *LIBL     KEY0021  21
394000940506PROMSG    *LIBL     KEY0022  22
394100940506PROMSG    *LIBL     KEY0023  23
394200940506PROMSG    *LIBL     KEY0024  24
394300940506**  ABILITAZIONE D1
3944009407200N  01
3945009407200N  02
3946009407201N  03
3947009407201N  04
3948009407200N  05
3949009407200N  06
3950009407200N  07
3951009412061N  08
3952009407200N  09
3953009503061N  10
3954009407200N  11
3955009407201N  12
3956009407200N  13
3957009407200N  14
3958009407200N  15
3959009407200N  16
3960009407200N  17
3961009407201N  18
3962009407200N  19
3963009407200N  20
3964009407200N  21
3965009407200N  22
3966009702100N  23   Impostare a '1' se si utilizzano opzioni
3967009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
396800940916** TASTI DI FUNZIONE  S1
396900940916PROMSG    *LIBL     KEY0001  01
397000940916PROMSG    *LIBL     KEY0002  02
397100940916PROMSG    *LIBL     KEY0003  03
397200940916PROMSG    *LIBL     KEY0004  04
397300940916PROMSG    *LIBL     KEY0005  05
397400940916PROMSG    *LIBL     KEY0006  06
397500940916PROMSG    *LIBL     KEY0007  07
397600940916PROMSG    *LIBL     KEY0008  08
397700940916PROMSG    *LIBL     KEY0009  09
397800940916PROMSG    *LIBL     KEY0010  10
397900940916PROMSG    *LIBL     KEY0011  11
398000940916PROMSG    *LIBL     KEY0012  12
398100940916PROMSG    *LIBL     KEY0013  13
398200940916PROMSG    *LIBL     KEY0014  14
398300940916PROMSG    *LIBL     KEY0015  15
398400940916PROMSG    *LIBL     KEY0016  16
398500940916PROMSG    *LIBL     KEY0017  17
398600940916PROMSG    *LIBL     KEY0018  18
398700940916PROMSG    *LIBL     KEY0019  19
398800940916PROMSG    *LIBL     KEY0020  20
398900940916PROMSG    *LIBL     KEY0021  21
399000940916PROMSG    *LIBL     KEY0022  22
399100940916PROMSG    *LIBL     KEY0023  23
399200940916PROMSG    *LIBL     KEY0024  24
399300940916**  ABILITAZIONE S1
3994009409160N  01
3995009409160N  02
3996009409161N  03
3997009409160N  04
3998009409160N  05
3999009409160N  06
4000009409160N  07
4001009411241N  08
4002009409160N  09
4003009409161N  10
4004009409160N  11
4005009409161N  12
4006009409160N  13
4007009409160N  14
4008009409160N  15
4009009409160N  16
4010009409161N  17
4011009409161N  18
4012009409160N  19
4013009409160N  20
4014009409160N  21
4015009409160N  22
4016009509061N  23   Impostare a '1' se si utilizzano opzioni
4017009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
401800940916** TASTI DI FUNZIONE  D3
401900940506PROMSG    *LIBL     KEY0001  01
402000940506PROMSG    *LIBL     KEY0002  02
402100940506PROMSG    *LIBL     KEY0003  03
402200940506PROMSG    *LIBL     KEY0004  04
402300940506PROMSG    *LIBL     KEY0005  05
402400940506PROMSG    *LIBL     KEY0006  06
402500940506PROMSG    *LIBL     KEY0007  07
402600940506PROMSG    *LIBL     KEY0008  08
402700940506PROMSG    *LIBL     KEY0009  09
402800940506PROMSG    *LIBL     KEY0010  10
402900940506PROMSG    *LIBL     KEY0011  11
403000940506PROMSG    *LIBL     KEY0012  12
403100940506PROMSG    *LIBL     KEY0013  13
403200940506PROMSG    *LIBL     KEY0014  14
403300940506PROMSG    *LIBL     KEY0015  15
403400940506PROMSG    *LIBL     KEY0016  16
403500940506PROMSG    *LIBL     KEY0017  17
403600940506PROMSG    *LIBL     KEY0018  18
403700940506PROMSG    *LIBL     KEY0019  19
403800940506PROMSG    *LIBL     KEY0020  20
403900940506PROMSG    *LIBL     KEY0021  21
404000940506PROMSG    *LIBL     KEY0022  22
404100940506PROMSG    *LIBL     KEY0023  23
404200940506PROMSG    *LIBL     KEY0024  24
404300940916**  ABILITAZIONE D3
4044009407200N  01
4045009407200N  02
4046009407201N  03
4047009407200N  04
4048009407200N  05
4049009407200N  06
4050009407200N  07
4051009407200N  08
4052009407200N  09
4053009409160N  10
4054009407200N  11
4055009407201N  12
4056009407200N  13
4057009407200N  14
4058009407200N  15
4059009407200N  16
4060009409160N  17
4061009409160N  18
4062009407200N  19
4063009407200N  20
4064009407200N  21
4065009407200N  22
4066009702100N  23   Impostare a '1' se si utilizzano opzioni
4067009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
