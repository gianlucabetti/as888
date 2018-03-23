000100130828     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND SDG005)
000200131008R134 H*PARMS CVTOPT(*NONE) ACTGRP(SOGG)
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400000000      *
000500000000      *           -----------------------------
000600130828      *                Spesometro clienti/fornitori
000700000000      *              Dati di testata
000800000000      *           -----------------------------
000900000000      *
001000131008R134 F* Cambiato actgrp QILE con SOGG
001100131008R134 X*****PARMS CVTOPT(*NONE) ACTGRP(QILE)
001200000000     F*----------------------------------------------------------------
001300130828     FSDGSG73D  CF   E             WORKSTN
001400130828     F                                     RENAME(SG73D1:FMTD1)
001500130828     F                                     RENAME(SG73D2:FMTD2)
001600130828     F                                     RENAME(SG73D3:FMTD3)
001700130828     F                                     RENAME(SG73S1:FMTS1)
001800130828     F                                     RENAME(SG73C1:FMTC1)
001900130828     F                                     RENAME(SG73T1:FMTT1)
002000130828     F                                     RENAME(SG73Z1:FMTZ1)
002100130828     F                                     RENAME(SG73Z2:FMTZ2)
002200130828     F                                     RENAME(SG73Z3:FMTZ3)
002300000000     F                                     SFILE(FMTS1:S1NRR)
002400000000     F                                     INFDS(DSFMT)
002500000000     D*----------------------------------------------------*
002600000000     D* Prototipazione driver contatti
002700000000     D/COPY *LIBL/SRCCPY,PJDR12PR
002800000000     D*----------------------------------------------------*
002900000000     D* Schiere per la gestione delle opzioni
003000000000     D*
003100000000     D* dimensione della schiera $MS1, da non cambiare
003200000000     D $DIMOP          C                   CONST(30)
003300000000     D $MS1            S             27    DIM($dimop) CTDATA PERRCD(1)
003400000000     D $TX1            S             50    DIM($dimop) ALT($MS1)
003500000000     D $OP1            S              2  0 DIM($dimop) CTDATA PERRCD(1)
003600000000     D $FL1            S              1    DIM($dimop) ALT($OP1)
003700000000     D* schiera contenente i nomi dei programma richiamati attraverso
003800000000     D* le opzioni personalizzate
003900000000     D $PGO1           S             10    DIM($dimop)
004000000000     D $TX             S             50    DIM(50)
004100000000     D $FL             S              1    DIM(50)
004200000000     D* Schiere generali per la gestione dei tasti di funzione
004300000000     D $FM             S             27    DIM(24)
004400000000     D $FX             S             50    DIM(24)
004500000000     D $FC             S              1    DIM(24)
004600000000     D $FR             S              1    DIM(24)
004700000000     D* Schiere per la gestione dei tasti di funzione
004800000000     D* Video D1
004900000000     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
005000000000     D $FX1            S             50    DIM(24) ALT($FM1)
005100000000     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
005200000000     D $FR1            S              1    DIM(24) ALT($FC1)
005300000000     D* Schiera contente i nomi dei programmi da richiamare con i tasti
005400000000     D* funzione personalizzata S1
005500000000     D $PGF2           S             10    DIM(25)
005600000000     D* Schiere per la gestione dei tasti di funzione
005700000000     D* Video S1
005800000000     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
005900000000     D $FX2            S             50    DIM(24) ALT($FM2)
006000000000     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
006100000000     D $FR2            S              1    DIM(24) ALT($FC2)
006200000000     D* Schiere per la gestione dei tasti di funzione
006300000000     D* Video D2
006400000000     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
006500000000     D $FX3            S             50    DIM(24) ALT($FM3)
006600000000     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
006700000000     D $FR3            S              1    DIM(24) ALT($FC3)
006800000000     D*
006900000000     D $D1L01          S              3  0
007000000000     D $D1L02          S              3  0
007100000000     D $S1L01          S              3  0
007200000000     D $S1L02          S              3  0
007300000000     D $C1Op1          S              3  0
007400000000     D $C1Op2          S              3  0
007500000000     D $D2L01          S              3  0
007600000000     D $D2L02          S              3  0
007700000000     D*
007800000000     D*----------------------------------------------------*
007900000000     D* Valori assunti
008000000000     D XMPEDS        E DS                  INZ
008100000000     D XMPEDS1       E DS                  INZ
008200000000     D DSMPE           DS                  INZ
008300000000     D  $LOP                   1      1  0
008400000000     D  $ORD                   2      3  0
008500000000     D*-------------
008600000000     D* Passaggio Parametri
008700000000     D KPJBA         E DS
008800110823     D*-------------
008900110823     D* Salvataggio Kpjbu per richiamo altri pgm
009000110823     D SavKPJBU        S            256
009100000000     D*-------------
009200130828     D* Parametri modulo SDGSPECF
009300130828     D SDGPA08DS     E DS
009301160321R2195D*-------------
009302160321 "   D* Parametri modulo SDBLACK2
009303160321R2195D SDGPA11DS     E DS
009400000000     D*-------------
009500000000     D* Reperimento Autorizzazioni
009600000000     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
009700000000     D*-------------
009800000000     D* Dati per pgm XNOAUT
009900000000     D XNOADS        E DS                  EXTNAME(XNOAUTDS)
010000000000     D*-------------
010100000000     D* Passaggio Autorizzazioni
010200000000     D DSAUT           DS           512
010300000000     D*-------------
010400000000     D* Maschera dati autorizzazioni
010500000000     D CONCIVDS      E DS
010600000000     D*-------------
010700000000     D* Dati di ambiente ottenuti da SDGSOCDR
010800000000     D SOC001        E DS                  EXTNAME(SDGSOCDS )
010900000000     D    SIFSogg                     8    overlay(XScFGO:31)
011000000000     D*-------------
011100000000     D* DS Interna per dati di output di SDGSOCDR
011200000000     D XSOCDS          DS          1000
011300000000     D*-------------
011400000000     D* DS Esterna gestione messaggi
011500000000     D SDGMSGDS      E DS                  INZ
011600000000     D  STS          E                     EXTFLD(MSGSTS)
011700000000     D                                     DIM(333)
011800000000     D  JBA          E                     EXTFLD(MSGJBA)
011900000000     D                                     DIM(502)
012000000000     D*-------------
012100000000     D* campi di lavoro per emissione window di avvertimento
012200000000     D WXMSGInz        S              1A   INZ('1')
012300000000     D*-------------
012400000000     D* Richiamo a SDGTABDR
012500000000     D SDGTABDS      E DS                  inz
012600000000     D*-------------
012700000000     D* Parametri per ricerca codice ANATB00F
012800000000     D XTABDS        E DS                  INZ
012900000000     D*-------------
013000000000     D* Ricerca codici Stati
013100000000     D ANGG12DS      E DS
013200131206R141 D*-------------
013300131206 "   D* Definizioni controllo causale
013400131206 "   D Anope00f      e ds                  inz
013500131206 "   D Refresh         S              1
013600131206 "   D Uso             S              1
013700131206 "   D Causale         S              4
013800131206 "   D*-------------
013900131206 "   D* Ricerca causale
014000131206 "   D ANA070DS      E DS                  INZ
014100131206 "   D  RET070       E                     INZ('0')
014200131206 "   D  OPR070       E                     INZ('0')
014300131206 "   D  CMT070       E                     INZ('0')
014400131206 "   D  ERR070       E                     INZ('0')
014500131206 "   D  FSC070       E                     INZ('S')
014600131206 "   D*----------
014700131206 "   D WData           S               D
014800131206 "   D WAnno           S              4  0
014900131206 "   D WDtAlfa         S             10A
015000131206 "   D DataDs          DS
015100131206 "   D  AnnoDs                        4A
015200131206 "   D   Tratt1                       1A   Inz('-')
015300131206 "   D  MeseDs                        2A
015400131206 "   D   Tratt2                       1A   Inz('-')
015500131206R141 D  GiornoDs                      2A
015600000000     D*-------------
015700000000     D* Ricerca legende
015800000000     D ANA964DS      E DS                  INZ
015900000000     D*-------------
016000000000     D*Controlli validità per valore di una legenda
016100000000     D X07VALDS      E DS                  Inz
016200000000     D*-------------
016300000000     D* DS per controllo conto
016400000000     D ND002DS       E DS                  INZ
016500000000     D*-------------
016600000000     D* Reperimento parametri
016700000000     D XPaOut          DS          2000
016800000000     D*-------------
016900000000     D* Parametri in input
017000130828    >D SDGSG73DS     E DS                  inz
017100000000     D*-------------
017200000000     D* Parametri del PGM di lettura movimenti estratti
017300130829     D* Parametri di INPUT
017400130829     D SDG00502DS    E DS                  INZ
017500130829     D  RIC0502      E                     INZ('0')
017600130829     D*----------
017700130829     D* Parametri di OUTPUT
017800130829     D* per manutenzione di testata, quadri e dettaglio
017900130829     D SDG00592DS    E DS                  INZ
018000130829     D*--------------------
018100130829     D* Parametri di OUTPUT
018200130829     D* per lista quadri a video e creazione tracciato ministeriale
018300130829     D*---------
018400130829     D*---------
018500130829     D* formato AGGREGATO
018600130829     D*---
018700130829     D* SOGGETTI NAZIONALI: FATTURE, NOTE E DOCUMENTI RIEPILOGATIVI
018800130829     D*                     (NOLEGGIO/LEASING)
018900130829     D SDG005FADS    E DS                  INZ
019000130829     D*
019100130829     D*---
019200130829     D* SOGGETTI NAZIONALI: OPERAZIONI SENZA FATTURA
019300130829     D SDG005SADS    E DS                  INZ
019400130829     D*
019500130829     D*---
019600130829     D* SOGGETTI NON RESIDENTI E ACQUISTI DI SERVIZI DA NON RESIDENTI
019700130829     D SDG005BLDS    E DS                  INZ
019800130829     D*---------
019900130829     D*---------
020000130829     D* formato DETTAGLIATO
020100130829     D*---
020200130829     D* SOGGETTI NAZIONALI: FATTURE E DOCUMENTI RIEPILOGATIVI EMESSI
020300130829     D*                     (NOLEGGIO/LEASING)
020400130829     D SDG005FEDS    E DS                  INZ
020500130829     D*---
020600130829     D* SOGGETTI NAZIONALI: FATTURE E DOCUMENTI RIEPILOGATIVI RICEVUTI
020700130829     D SDG005FRDS    E DS                  INZ
020800130829     D*---
020900130829     D* SOGGETTI NAZIONALI: NOTE EMESSE
021000130829     D SDG005NEDS    E DS                  INZ
021100130829     D*---
021200130829     D* SOGGETTI NAZIONALI: NOTE RICEVUTE
021300130829     D SDG005NRDS    E DS                  INZ
021400130829     D*---
021500130829     D* SOGGETTI NAZIONALI: OPERAZIONI SENZA FATTURA
021600130829     D*                     (NOLEGGIO/LEASING)
021700130829     D SDG005DFDS    E DS                  INZ
021800130829     D*---
021900130829     D* SOGGETTI NON RESIDENTI
022000130829     D*                     (NOLEGGIO/LEASING)
022100130829     D SDG005FNDS    E DS                  INZ
022200130829     D*---
022300130829     D* ACQUISTI DI SERVIZI DA NON RESIDENTI
022400130829     D SDG005SEDS    E DS                  INZ
022500131017R136 D*---
022600131017 "   D* SOGGETTI NON RESIDENTI: OPERAZIONI SENZA FATTURA
022700131017 "   D* LEGATE AL TURISMO
022800131017R136 D SDG005TUDS    E DS
022900130829     D*---
023000130829     D* RIEPILOGO
023100130829     D SDG005TADS    E DS                  INZ
023200000000     D*-------------
023300130912     D* manutenzione  dati di testata
023400130828     D SDGSG74DS     E DS                  inz
023500130828     D  RETG74       E                     INZ('0')
023600130828     D  OPRG74       E                     INZ('0')
023700130828     D  ERRG74       E                     INZ('0')
023800130828     D  CMTG74       E                     INZ('0')
023900000000     D*-------------
024000130912     D* lista dettaglio
024100130828     D SDGSG75DS     E DS                  inz
024200130828     D  RETG75       E                     INZ('0')
024300130828     D  OPRG75       E                     INZ('0')
024400130828     D  ERRG75       E                     INZ('0')
024500130828     D  CMTG75       E                     INZ('0')
024600000000     D*-------------
024700000000     D* manutenzione dettaglio
024800130828    >D SDGSG76DS     E DS                  inz
024900130828    >D  RETG76       E                     INZ('0')
025000130828    >D  OPRG76       E                     INZ('0')
025100130828    >D  ERRG76       E                     INZ('0')
025200130828    >D  CMTG76       E                     INZ('0')
025300000000     D*-------------
025400130912     D* lista quadri
025500130828    >D SDGSG77DS     E DS                  inz
025600130828    >D  RETG77       E                     INZ('0')
025700130828    >D  OPRG77       E                     INZ('0')
025800130828    >D  ERRG77       E                     INZ('0')
025900130828    >D  CMTG77       E                     INZ('0')
026000151203R195 D*-------------
026100151203 "   D* Parametri per applicazione soglia a
026200151203 "   D* operazioni con operatori in Black List
026300151203R195 D SDG00508DS    E DS
026400000000     D*-------------
026500000000     D* campo di salvataggio per la DS d'ingresso durante la chiamata
026600000000     D* alla funzione personalizzata
026700130828    >D $SaveDS         S                   like(SDGSG73DS)
026800000000     D*-------------
026900000000     D DSFMT           DS           512
027000000000     D  $TASTO               369    369
027100000000     D  NRG                  370    370
027200000000     D  NCL                  371    371
027300000000      * riga cursore window
027400000000     D  NRiWin               382    382
027500000000      * colonna cursore window
027600000000     D  NCoWin               383    383
027700000000     D  SFLNRR               378    379B 0
027800000000     D*-------------
027900000000     D* posizione cursore
028000000000     D CURSOR          DS
028100000000     D  RIRI                   1      2B 0 INZ
028200000000     D  R$$                    2      2
028300000000     D  COCO                   3      4B 0 INZ
028400000000     D  C$$                    4      4
028500000000     D*-------------
028600000000     D* Costanti
028700000000     D* nr.di righe del sfl (SFLPAG)
028800130927     D SFLPAG          C                   CONST(5)
028900000000     D*-------------
029000000000     D* Tasti di funzione
029100000000     D F01             C                   CONST(X'31')
029200000000     D F02             C                   CONST(X'32')
029300000000     D F03             C                   CONST(X'33')
029400000000     D F04             C                   CONST(X'34')
029500000000     D F05             C                   CONST(X'35')
029600000000     D F06             C                   CONST(X'36')
029700000000     D F07             C                   CONST(X'37')
029800000000     D F08             C                   CONST(X'38')
029900000000     D F09             C                   CONST(X'39')
030000000000     D F10             C                   CONST(X'3A')
030100000000     D F11             C                   CONST(X'3B')
030200000000     D F12             C                   CONST(X'3C')
030300000000     D F13             C                   CONST(X'B1')
030400000000     D F14             C                   CONST(X'B2')
030500000000     D F15             C                   CONST(X'B3')
030600000000     D F16             C                   CONST(X'B4')
030700000000     D F17             C                   CONST(X'B5')
030800000000     D F18             C                   CONST(X'B6')
030900000000     D F19             C                   CONST(X'B7')
031000000000     D F20             C                   CONST(X'B8')
031100000000     D F21             C                   CONST(X'B9')
031200000000     D F22             C                   CONST(X'BA')
031300000000     D F23             C                   CONST(X'BB')
031400000000     D F24             C                   CONST(X'BC')
031500000000     D ENTER           C                   CONST(X'F1')
031600000000     D ROLDWN          C                   CONST(X'F4')
031700000000     D ROLLUP          C                   CONST(X'F5')
031800000000     D*-------------
031900000000     D* Attributi video
032000000000     D Normale         C                   Const(X'32')
032100000000     D Protetto        C                   Const(X'A0')
032200000000     D Annul           C                   CONST(X'A1')
032300000000     D*-------------
032400000000     D* Sottotitolo di testata
032500000000     D SOTTIT          C                   CONST('PROMSG    *LIBL-
032600000000     D                                          COS2504')
032700000000     D SOTTITD1        C                   CONST('SDGMSG    *LIBL-
032800000000     D                                          SDC0001')
032900151201R195 D*-------------
033000151201  "  D* Sottotitolo di testata
033100151201  "  D* "Soggetti Spesometro"
033200151201  "  D SOTTITT1        C                   CONST('SDGMSG    *LIBL-
033300151201  "  D                                          SDC0090')
033400151201  "  D* Sottotitolo di testata Black List
033500151201  "  D* "Soggetti Acquisti da RSM"
033600151201  "  D SOTTITT2        C                   CONST('SDGMSG    *LIBL-
033700151201  "  D                                          SDC0091')
033800151201  "  D* Sottotitolo di testata Black List
033900151201  "  D* "Soggetti Black List"
034000151201  "  D SOTTITT3        C                   CONST('SDGMSG    *LIBL-
034100151201R195 D                                          SDC0092')
034200000000     D*-------------
034300000000     D* Variabili per gestione videate
034400000000     D $Gest           S              2A
034500000000     D $Fine           S              1A
034600000000     D $InzD1          S              1A
034700000000     D $InzS1          S              1A
034800000000     D $InzD2          S              1A
034900000000     D $LastG          S              2A
035000000000     D $LastV          S              2A
035100000000     D $EndFile        S              1A
035200000000     D $Esci           S              1A
035300000000     D $RcdOK          S              1A
035400000000     D $Unique         S              1A
035500000000     D*-------------
035600000000     D* Variabili appoggio sempre presenti in tutte le anagrafiche
035700000000     D S1Nrr           S                   like(C1Rcd)
035800000000     D WSfl            S                   like(C1Nrr)
035900000000     D WMax            S                   like(C1Rcd)
036000000000     D WPag            S                   like(C1Rcd)
036100000000     D WInzS1          S                   like($InzS1)
036200130828     D WOpr            S                   like(OprG73)
036300000000     D CurR            S              3S 0
036400000000     D CurC            S              3S 0
036500000000     D Win             S             99A
036600000000     D W1Pos           S                   like(C1Pos)
036700000000     D WINKR           S              1A
036800000000     D WExf            S              1A
036900000000     D WSogg           S                   Like(DVSSogg)
037000130828     D WFineSCF        S              1A
037100000000     D*-------------
037200000000     D* Indice della schiera tasti funzione
037300000000     D PosFun          S              2  0
037400000000     D*-------------
037500000000     D*  Indici
037600000000     D X               S              5S 0
037700000000     D Y               S              5S 0
037800000000     D* Indice della schiera per le opzioni personalizzate
037900000000     D PosOpz          S              2  0
038000000000     D* numero max opzioni
038100000000     D NumOpz          S              2  0 Inz($DIMOP)
038200000000     D* numero effettive opzioni
038300000000     D OpzLun          S              2  0
038400000000     D*-------------
038500000000     D* ricerca soggetti
038600130829     D ANA040DS      E DS                  INZ
038700000000     D*-------------
038800000000     D* Campi di lavoro
038900000000     D $P1PRO2184      S             10A
039000000000     D*-------------
039100000000     D* Reperimento nome PGM
039200000000     D STATUS         SDS           333
039300000000     D  DSPGM            *PROC
039400000000     D  PARMS            *PARMS
039500000000     D*-------------
039600000000     D* DS esterna per dati soggetti
039700000000     D DVASOG        E DS                  INZ
039800000000     D*-------------
039900000000     D* Definizioni parametri driver anagrafici
040000000000     D Dvaric          S              1
040100000000     D DvaSocieta      S              3
040200000000     D DvaUnita        S              8
040300000000     D DvaStrutt       S             10
040400000000     D DvaDtRif        S               D
040500000000     D DvaLenOut       S              9B 0
040600000000     D DvaSnatura      S              1
040700000000     D DvaCodice       S              8
040800000000     D DvaLineav       S              3
040900000000     D DvaFiliale      S              3  0
041000000000     D DvaSogg         S              8
041100000000     D DvaTpInd        S              2
041200000000     D DvaCdInd        S              3
041300000000     D DvaErrore       S              1
041400000000     D DvaOutput       S           4000
041500000000     D*-------------
041600000000     D* parametri per chiamata al driver dei contatti
041700000000     D $Errore         S              1
041800000000     D $Opz            S              1
041900000000     D $Len            S              9B 0
042000000000     D $Refresh        S              1
042100000000     D Esito           S              1
042200000000     D GesMsg          S              1
042300000000     D Output          S           4000
042400000000     D LenOut          S              9B 0
042500000000     D Struttura       S             10
042600130930     D*-------------
042700130930     D* Campi di lavoro
042800130930     D WIn17           S              1
042900151203R195 D $Continua       S              1A
043000151203R195 D $MsgCd1         S                    Like(XMSGCd1)
043100140327R133 D*-------------
043200140327  "  D* tipo comunicazione Spesometro
043300140327R133 D WTpCom          S              1A
043400000000     C*----------------------------------------------------*
043500000000     C*                MAIN LINE PROGRAM
043600000000     C*----------------------------------------------------*
043700000000     C*
043800000000     C* inizializzazione variabili
043900000000     C                   EXSR      INZVAR
044000000000     C*
044100000000     C     $FINE         DOWEQ     *OFF
044200000000     C     $GEST         CASEQ     'D1'          GESD1                          Parzializz.
044300000000     C     $GEST         CASEQ     'S1'          GESS1                          Lista
044400000000     C     $GEST         CASEQ     'D2'          GESD2                          Valori ass.
044500000000     C                   ENDCS
044600000000     C                   ENDDO
044700000000     C* fine programma
044800000000     C                   EXSR      ENDPGM
044900000000     C*
045000000000     C/EJECT
045100000000     C************************************************************
045200000000     C* FINE PROGRAMMA
045300000000     C************************************************************
045400000000     C     ENDPGM        BEGSR
045500000000     C*
045600000000     C* passaggio dati al pgm chiamante
045700130828    >C                   MOVEL     WOPR          OPRG73                                     ne S1
045800130828    >C                   MOVEL     SDGSG73DS     KPJBU
045900000000     C*
046000000000     C* Se pgm Guida non è stato richiamato posso chiudere in LR
046100130828    >C     OPZG73        IFEQ      *BLANK
046200000000     C                   SETON                                            LR
046300000000     C* altrimenti chiudo in RT
046400000000     C                   ELSE
046500000000     C                   SETON                                            RT
046600000000     C                   ENDIF
046700000000     C                   RETURN
046800000000     C*
046900000000     C                   ENDSR
047000000000     C/EJECT
047100000000     C************************************************************
047200000000     C* RICERCHE
047300000000     C************************************************************
047400000000     C     SEARCH        BEGSR
047500000000     C*
047600000000     C* determino Riga/Colonna del cursore
047700000000     C                   MOVE      NRG           R$$
047800000000     C                   MOVE      NCL           C$$
047900000000     C                   Z-ADD     RIRI          CURR
048000000000     C                   Z-ADD     COCO          CURC
048100000000     C*
048200000000     C                   EXSR      CallAllinea
048300000000     C*
048400000000   B1C                   SELECT
048500000000     C*
048600000000     C*  Stato
048700000000     C     H1NMFL        WHENEQ    'D1STATO'
048800000000     C                   EXSR      F4_Stato
048900140327R133 C*
049000140327 "   C*  Mese
049100140327 "   C     H1NMFL        WHENEQ    'D1MESE'
049200140327R133 C                   EXSR      F4_Mese
049300130911     C*
049400130911     C*  Natura soggetto
049500130911     C     H1NMFL        WHENEQ    'D1CF'
049600130911     C                   EXSR      F4_CF
049700000000     C*
049800000000     C*  Codice IVA
049900000000     C     H1NMFL        WHENEQ    'D1PARTIVA'
050000000000     C                   EXSR      F4_PartIva
050100000000     C*
050200000000     C*  Codice fiscale
050300000000     C     H1NMFL        WHENEQ    'D1CDFISC'
050400000000     C                   EXSR      F4_CdFisc
050500000000     C*
050600000000     C*  Soggetto
050700000000     C     H1NMFL        WHENEQ    'D1SOGG'
050800000000     C                   EXSR      F4_Sogg
050900131206R141 C*
051000131206 "   C*  Causale
051100131206 "   C     H1NMFL        WHENEQ    'D1CAUSALE'
051200131206R141 C                   EXSR      F4_Causale
051300000000     C*
051400000000     C* FMTD1 - Dati attivi/non attivi
051500000000     C     H1NMFL        WHENEQ    'D1ANN  '
051600000000     C                   EXSR      F4_Ann
051700000000     C*
051800000000     C* FMTD1 - Tipo annullamento
051900000000     C     H1NMFL        WHENEQ    'D1TPANN'
052000000000     C                   EXSR      F4_TpAnn
052100130829     C*
052200130829     C* FMTD1 - Quadro di presentazione
052300130829     C     H1NMFL        WHENEQ    'D1QUADRO'
052400130829     C                   EXSR      F4_Quadro
052500130919     C*
052600130919     C* FMTD1 - tipo operazione
052700130919     C     H1NMFL        WHENEQ    'D1TPOPE '
052800130919     C                   EXSR      F4_TpOpe
052900000000     C*
053000000000     C* ricerca non abilitata in questa posizione
053100000000   X1C                   OTHER
053200000000     C                   SETON                                        9899
053300000000     C*
053400000000   E1C                   ENDSL
053500000000     C*
053600000000     C* imposto pos. del cursore
053700000000     C                   Z-ADD     CURR          H1RIGA
053800000000     C                   Z-ADD     CURC          H1COLO
053900000000     C*
054000000000     C                   MOVE      *HIVAL        $LASTV
054100000000     C*
054200000000     C                   ENDSR
054300000000     C/EJECT
054400000000     C************************************************************
054500000000     C* Richiamo programma per allineare i codici
054600000000     C************************************************************
054700000000     C     CallAllinea   BEGSR
054800000000     C*
054900000000     C     D1SOGG        IFNE      *BLANK
055000000000     C                   CALLb     'XALLINEA'
055100000000     C                   PARM                    D1SOGG
055200000000     C                   PARM      8             LEN               5 0
055300000000     C                   PARM                    XSCINP
055400000000     C                   ENDIF
055500000000     C*
055600000000     C                   ENDSR
055700000000      /EJECT
055800000000     C************************************************************
055900000000     C* Ricerca codice Stato del conto
056000000000     C************************************************************
056100000000     C     F4_Stato      BEGSR
056200000000     C*
056300000000     C                   Reset                   XTabDS
056400000000     C*
056500000000     C                   Move      '01'          XTaOpz
056600000000     C                   Move      XScSoc        XTaAzi
056700000000     C                   Move      XScLin        XTaLin
056800000000     C                   Z-add     Curr          XTaRig
056900000000     C                   Z-add     Curc          XTaCol
057000000000     C                   Move      *Blank        XTaKey
057100000000     C*
057200000000     C                   Clear                   KPJBU
057300000000     C                   MoveL     XTabDS        KPJBU
057400000000     C*
057500000000     C                   CALL      'ANGG12R'
057600000000     C                   PARM                    KPJBA
057700000000     C                   MoveL     KPJBU         XTabDS
057800000000     C*
057900000000     C                   if        XTaErr     <> '1'  and
058000000000     C                             XTaKey     <> *Blank
058100000000     C                   Move      XTaKey        D1Stato
058200000000     C                   endif
058300000000     C*
058400000000     C                   ENDSR
058500000000      /EJECT
058600140327R133 C************************************************************
058700140327     C* Ricerca mese di competenza
058800140327R133 C************************************************************
058900140327     C     F4_Mese       BEGSR
059000140327     C*
059100140327     C                   Reset                   ANA964DS
059200140327     C*
059300140327     C                   MoveL     '01'          Opz964
059400140327     C                   MoveL     'ANDIZ000'    Trc964
059500140327     C                   MoveL     'DSPMESE  '   Cam964
059600140327     C                   MoveL     *On           All964
059700140327     C*
059800140327     C                   MoveL     ANA964DS      KPJBU
059900140327     C                   Call      'ANA964R'
060000140327     C                   Parm                    KPJBA
060100140327     C                   MoveL     KPJBU         ANA964DS
060200140327     C*
060300140327     C                   If        Ret964     <> '2'
060400140327     C                   MoveL     Val964        D1Mese
060500140327     C                   EndIf
060600140327     C*
060700140327     C                   EndSr
060800140327R133  /EJECT
060900130911     C************************************************************
061000111003     C* Ricerca natura del conto
061100130911     C************************************************************
061200111003     C     F4_CF         BEGSR
061300111003     C*
061400111003     C                   Reset                   ANA964DS
061500111003     C*
061600111003     C                   MoveL     '01'          Opz964
061700130919     C                   MoveL     'SDGSG73S '   Trc964
061800130919     C                   MoveL     'CFG73    '   Cam964
061900111003     C                   MoveL     *On           All964
062000111003     C*
062100111003     C                   MoveL     ANA964DS      KPJBU
062200111003     C                   CALL      'ANA964R'
062300111003     C                   PARM                    KPJBA
062400111003     C                   MoveL     KPJBU         ANA964DS
062500111003     C*
062600111003     C                   if        Ret964     <> '2'
062700111003     C                   MoveL     Val964        D1CF
062800111003     C                   endif
062900111003     C*
063000111003     C                   ENDSR
063100130911      /EJECT
063200000000     C************************************************************
063300000000     C* Ricerca partita IVA
063400000000     C************************************************************
063500000000     C     F4_PartIva    BEGSR
063600000000     C*
063700130829     C                   RESET                   ANA040DS
063800000000     C*
063900130829     C                   MOVEL     '01'          OPZ040
064000130829     C                   MoveL     D1PartIva     IVA040
064100130829     C                   MoveL     D1Sogg        CoD040
064200130829     C                   MOVEL     ANA040DS      KPJBU
064300130829     C                   CALL      'ANA040R'
064400000000     C                   PARM                    KPJBA
064500000000     C*
064600130829     C                   MOVEL     KPJBU         ANA040DS
064700000000     C*
064800130829     C                   if        Ret040     <> '2'
064900000000     C*
065000130829     C                   MoveL     CoD040        WSogg
065100000000     C                   EXSR      CallDvaSog
065200000000     C*
065300000000     C                   if        DVAErrore  <> '1'
065400000000     C*
065500000000     C                   Eval      %subst(DVASOG:1:DVALENOUT)
065600000000     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
065700000000     C*
065800000000     C                   MoveL     DVSPartIVA    D1PartIva
065900000000     C*
066000000000     C                   endif
066100000000     C*
066200000000     C                   endif
066300000000     C*
066400000000     C                   ENDSR
066500000000      /EJECT
066600000000     C************************************************************
066700000000     C* Ricerca codice fiscale
066800000000     C************************************************************
066900000000     C     F4_CdFisc     BEGSR
067000000000     C*
067100130829     C                   RESET                   ANA040DS
067200000000     C*
067300130829     C                   MOVEL     '01'          OPZ040
067400130829     C                   MoveL     D1CdFisc      CdF040
067500130829     C                   MoveL     D1Sogg        CoD040
067600130829     C                   MOVEL     ANA040DS      KPJBU
067700130829     C                   CALL      'ANA040R'
067800000000     C                   PARM                    KPJBA
067900000000     C*
068000130829     C                   MOVEL     KPJBU         ANA040DS
068100000000     C*
068200130829     C                   if        Ret040     <> '2'
068300000000     C*
068400130829     C                   MoveL     CoD040        WSogg
068500000000     C                   EXSR      CallDvaSog
068600000000     C*
068700000000     C                   if        DVAErrore  <> '1'
068800000000     C*
068900000000     C                   Eval      %subst(DVASOG:1:DVALENOUT)
069000000000     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
069100000000     C*
069200000000     C                   MoveL     DVSCdFisc     D1CdFisc                       *
069300000000     C*
069400000000     C                   endif
069500000000     C*
069600000000     C                   endif
069700000000     C*
069800000000     C                   ENDSR
069900000000     C/EJECT
070000000000     C************************************************************
070100000000     C* Ricerca codice soggetto
070200000000     C************************************************************
070300000000     C     F4_Sogg       BEGSR
070400000000     C*
070500130829     C                   RESET                   ANA040DS
070600000000     C*
070700130829     C                   MOVEL     '01'          OPZ040
070800130829     C                   MoveL     D1Sogg        CoD040
070900130829     C                   MOVEL     ANA040DS      KPJBU
071000130829     C                   CALL      'ANA040R'
071100000000     C                   PARM                    KPJBA
071200000000     C*
071300130829     C                   MOVEL     KPJBU         ANA040DS
071400000000     C*
071500130829     C                   if        Ret040     <> '2'
071600130829     C                   MoveL     CoD040        D1Sogg
071700000000     C                   endif
071800000000     C*
071900000000     C                   ENDSR
072000000000     C/EJECT
072100131206R141 C************************************************************
072200131206 "   C* Ricerca codice causale
072300131206 "   C************************************************************
072400131206R141 C     F4_Causale    BEGSR
072500131206     C*
072600131206     C                   Reset                   ANA070DS
072700131206     C*
072800131206     C                   Move      '01'          Opz070
072900131206     C                   MOVE      '1'           Tpc070
073000131206     C                   CLEAR                   KPJBU
073100131206     C                   MoveL     ANA070DS      KPJBU
073200131206     C*
073300131206     C                   CALL      'ANA070R'
073400131206     C                   PARM                    KPJBA
073500131206     C*
073600131206     C                   MoveL     KPJBU         ANA070DS
073700131206     C*
073800131206     C     Cau070        IFNE      *BLANK
073900131206     C                   MoveL     Cau070        D1Causale
074000131206     C                   ENDIF
074100131206     C*
074200131206R141 C                   ENDSR
074300131206R141  /EJECT
074400000000     C************************************************************
074500000000     C* Ricerca dati attivi/non attivi
074600000000     C************************************************************
074700000000     C     F4_Ann        BEGSR
074800000000     C*
074900000000     C                   reset                   ana964ds
075000000000     C*
075100000000     C                   movel     '01'          Opz964
075200000000     C                   movel     'ANDIZ000 '   Trc964
075300000000     C                   movel     'DSPATT   '   Cam964
075400000000     C*
075500000000     C                   movel     ana964ds      Kpjbu
075600000000     C                   call      'ANA964R'
075700000000     C                   parm                    Kpjba
075800000000     C                   movel     Kpjbu         Ana964ds
075900000000     C*
076000000000     C                   if        Ret964     <> '2'
076100000000     C                   movel     Val964        D1Ann
076200000000     C                   endif
076300000000     C*
076400000000     C                   ENDSR
076500000000     C/EJECT
076600000000     C************************************************************
076700000000     C* Ricerca tipo annullamento
076800000000     C************************************************************
076900000000     C     F4_TpAnn      BEGSR
077000000000     C*
077100000000     C                   reset                   ana964ds
077200000000     C*
077300000000     C                   movel     '01'          Opz964
077400130828     C                   movel     'SDGSCF000'   Trc964
077500130828     C                   movel     'SCFTPANN'    Cam964
077600000000     C*
077700000000     C                   movel     ana964ds      Kpjbu
077800000000     C                   call      'ANA964R'
077900000000     C                   parm                    Kpjba
078000000000     C                   movel     Kpjbu         Ana964ds
078100000000     C*
078200000000     C                   if        Ret964     <> '2'
078300000000     C                   movel     Val964        D1TpAnn
078400000000     C                   endif
078500000000     C*
078600000000     C                   ENDSR
078700000000     C/EJECT
078800130829     C************************************************************
078900130829     C* Ricerca quadro di presentazione
079000130829     C************************************************************
079100130829     C     F4_Quadro     BEGSR
079200111011     C*
079300111011     C                   reset                   ana964ds
079400111011     C*
079500111011     C                   movel     '01'          Opz964
079600160105R195 C*
079700160105 "   C                   Select
079800160105 "   C*
079900160105 "   C* Spesometro
080000160105R195 C                   When      TpComG73    = 'E'
080100160105     C                   movel     'SDGSG73S '   Trc964
080200160105     C                   movel     'QUADROG73 '  Cam964
080300160105R195 C*
080400160105 "   C* Acquisti da San Marino
080500160105 "   C                   When      TpComG73    = 'S'
080600160105 "   C                   movel     'ANDIZ000 '   Trc964
080700160105 "   C                   movel     'SDGQUADRO1'  Cam964
080800160105 "   C*
080900160105 "   C* Black List
081000160105 "   C                   When      TpComG73    = 'B'
081100160105 "   C                   movel     'ANDIZ000 '   Trc964
081200160105 "   C                   movel     'SDGQUADRO2'  Cam964
081300160105 "   C                   endSl
081400160105R195 C*
081500111011     C                   movel     *On           All964
081600111011     C*
081700111011     C                   movel     ana964ds      Kpjbu
081800111011     C                   call      'ANA964R'
081900111011     C                   parm                    Kpjba
082000111011     C                   movel     Kpjbu         Ana964ds
082100111011     C*
082200111011     C                   if        Ret964     <> '2'
082300130828     C                   movel     Val964        D1Quadro
082400111011     C                   endif
082500111011     C*
082600130829     C                   ENDSR
082700130829     C/EJECT
082800130919     C************************************************************
082900130919     C* Ricerca tipo operazione
083000130919     C************************************************************
083100130919     C     F4_TpOpe      BEGSR
083200130919     C*
083300130919     C                   reset                   ana964ds
083400130919     C*
083500130919     C                   movel     '01'          Opz964
083600130919     C                   MoveL     'SDGSG73S '   Trc964
083700130919     C                   MoveL     'TPOPEG73 '   Cam964
083800130919     C                   movel     *On           All964
083900130919     C*
084000130919     C                   movel     ana964ds      Kpjbu
084100130919     C                   call      'ANA964R'
084200130919     C                   parm                    Kpjba
084300130919     C                   movel     Kpjbu         Ana964ds
084400130919     C*
084500130919     C                   if        Ret964     <> '2'
084600130919     C                   movel     Val964        D1TpOpe
084700130919     C                   endif
084800130919     C*
084900130919     C                   ENDSR
085000130919     C/EJECT
085100000000     C************************************************************
085200000000     C* RIEMPIMENTO OPZIONI
085300000000     C************************************************************
085400000000     C     RIEOPZ        BEGSR
085500000000     C*
085600000000     C                   CALLB     'X04OPMSG'
085700000000     C* input
085800000000     C                   PARM                    $TX
085900000000     C                   PARM                    $FL
086000000000     C                   PARM                    WCOZ              3 0
086100000000     C                   PARM                    WMXO              3 0
086200000000     C* output
086300000000     C                   PARM                    WOPZ1            76
086400000000     C                   PARM                    WOPZ2            76
086500000000     C                   PARM                    FF23              1
086600000000     C* input
086700000000     C                   PARM                    $L01              3 0
086800000000     C                   PARM                    $L02              3 0
086900000000     C*
087000000000     C                   PARM                    $ALLOPT         960
087100000000     C*
087200000000     C                   ENDSR
087300000000     C/EJECT
087400000000     C************************************************************
087500000000     C* RIEMPIMENTO TASTI FUNZIONE
087600000000     C************************************************************
087700000000     C     RIEKEY        BEGSR
087800000000     C*
087900000000     C                   CALLB     'XKEYMSG'
088000000000     C* input
088100000000     C                   PARM                    $FX
088200000000     C                   PARM                    $FC
088300000000     C                   PARM                    $FR
088400000000     C                   PARM                    $ULK              3 0
088500000000     C* output
088600000000     C                   PARM                    $KEY1            79
088700000000     C                   PARM                    $KEY2            79
088800000000     C* input
088900000000     C                   PARM                    $L01              3 0
089000000000     C                   PARM                    $L02              3 0
089100000000     C*
089200000000     C                   PARM                    $ALLFUNCT       480
089300000000     C*
089400000000     C                   ENDSR
089500000000     C/EJECT
089600000000     C************************************************************
089700000000     C* GESTIONE EMISSIONE TASTI FUNZIONE
089800000000     C************************************************************
089900000000     C     GESPIE        BEGSR
090000000000     C*
090100000000     C                   SELECT
090200000000     C* Parzializzazioni
090300000000     C     $GEST         WHENEQ    'D1'
090400000000     C     $ULKD1        IFEQ      0
090500000000     C* riempimento opzioni
090600000000     C                   EXSR      F24D1
090700000000     C                   ELSE
090800000000     C                   WRITE     FMTZ1
090900000000     C                   ENDIF
091000000000     C* Lista
091100000000     C     $GEST         WHENEQ    'S1'
091200000000     C     $ULKS1        IFEQ      0
091300000000     C                   EXSR      F24S1
091400000000     C                   ELSE
091500000000     C                   WRITE     FMTZ2
091600000000     C                   ENDIF
091700000000     C* Modifica valori assunti
091800000000     C     $GEST         WHENEQ    'D2'
091900000000     C     $ULKD2        IFEQ      0
092000000000     C                   EXSR      F24D2
092100000000     C                   ELSE
092200000000     C                   WRITE     FMTZ3
092300000000     C                   ENDIF
092400000000     C                   ENDSL
092500000000     C*
092600000000     C                   ENDSR
092700000000     C/EJECT
092800000000     C************************************************************
092900000000     C* GESTIONE PARZIALIZZAZIONI
093000000000     C************************************************************
093100000000     C     GESD1         BEGSR
093200000000     C*
093300000000     C* inizializzazione videata
093400000000     C     $INZD1        IFEQ      *ON
093500000000     C                   EXSR      INZD1
093600000000     C                   MOVE      *OFF          $INZD1
093700000000     C                   ENDIF
093800000000     C* emissione piede videata se proveniente da altra
093900000000     C* salvataggio valore formato di provenienza
094000000000     C     $LASTV        IFNE      'D1'
094100000000     C*
094200000000     C* recupero il sottotitolo (se c'è)
094300000000     C                   Clear                   D1TIT
094400000000     C     SOTTITD1      IFNE      *BLANK
094500000000     C                   MOVEL     SOTTITD1      IDMSG
094600000000     C                   CALLB     'XRTVM'
094700000000     C                   PARM                    IDMSG            27
094800000000     C                   PARM                    TXTMSG          100
094900000000     C                   PARM                    ERRMSG            1
095000000000     C                   PARM      *Off          CTRMSG            1            centratura
095100000000     C                   PARM      15            LENMSG            3 0          lun output.
095200000000     C     ERRMSG        IFNE      *ON
095300000000     C                   MOVEL     TXTMSG        D1TIT
095400000000     C                   ELSE
095500000000     C                   MOVEL     *ALL'?'       D1TIT
095600000000     C                   ENDIF
095700000000     C                   ENDIF
095800000000     C*
095900000000     C* salvo stato F17 per sapere cosa fare in caso di F12
096000000000     C                   MOVE      *INKR         WINKR
096100000000     C                   WRITE     FMTT1
096200000000     C                   EXSR      GESPIE
096300000000     C                   MOVE      $LASTV        $LASTG
096400000000     C                   MOVE      'D1'          $LASTV
096500000000     C                   ENDIF
096600000000     C*
096700000000     C* se esistono errori sulla videata
096800000000     C* emetto la write del formato a indicatori spenti per vedere
096900000000     C* le eventuali decodifiche
097000130930   B1C                   if        (*IN99       = *On   or
097100130930   B1C                              *In17       = *On      )
097200000000     C                   MOVEA     *IN           WIN
097300000000     C                   MOVE      *ALL'0'       IN5098           49
097400000000     C                   MOVEA     IN5098        *IN(50)
097500130930     C                   Eval      WIn17        = *In17
097600130930     C                   Eval      *In17        = *Off
097700000000     C                   WRITE     FMTD1
097800000000     C                   MOVEA     WIN           *IN
097900130930     C                   Eval      *In17        = WIn17
098000000000     C                   ENDIF
098100000000     C*
098200000000     C                   EXFMT     FMTD1
098300000000     C*
098400000000     C* azzero la posizione del cursore
098500000000     C                   Z-ADD     0             H1RIGA
098600000000     C                   Z-ADD     0             H1COLO
098700000000     C*
098800000000     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
098900000000     C* INPUT
099000000000     C* $FC1 = Abilitazioni videata D1
099100000000     C* OUTPUT
099200000000     C* *IN97 + *IN99 Errori
099300000000     C                   MOVEA     $FC1          $FC
099400000000     C                   EXSR      CTRKEY
099500000000     C*
099600000000     C     *IN99         IFEQ      *OFF
099700000000     C*
099800000000     C                   SELECT
099900000000     C* F2=Decodifica
100000000000     C     $TASTO        WHENEQ    F02
100100000000     C                   EXSR      F02D1
100200000000     C* F3=Fine
100300000000     C     $TASTO        WHENEQ    F03
100400000000     C                   EXSR      F03D1
100500000000     C* F4=Ricerca
100600000000     C     $TASTO        WHENEQ    F04
100700000000     C                   EXSR      SEARCH
100800000000     C* F5=Refresh
100900000000     C     $TASTO        WHENEQ    F05
101000000000     C                   EXSR      F05D1
101100000000     C* F8=Successivo
101200000000     C     $TASTO        WHENEQ    F08
101300000000     C                   EXSR      F08D1
101400000000     C* F10=Immissione
101500000000     C     $TASTO        WHENEQ    F10
101600000000     C                   EXSR      F10D1
101700000000     C* F12=Ritorno
101800000000     C     $TASTO        WHENEQ    F12
101900000000     C                   EXSR      F12D1
102000000000     C* F18=Modifica valori assunti
102100000000     C     $TASTO        WHENEQ    F18
102200000000     C                   EXSR      F18D1
102300000000     C* F24=Altri tasti
102400000000     C     $TASTO        WHENEQ    F24
102500000000     C                   EXSR      F24D1
102600000000     C*
102700000000     C                   OTHER
102800000000     C                   EXSR      CTRD1
102900000000     C     *IN99         IFEQ      *OFF
103000000000     C* definisco l'ordinamento
103100000000     C                   EXSR      DEFORD
103200000000     C* richiamo la lista
103300000000     C                   MOVE      'S1'          $GEST
103400000000     C                   MOVE      *ON           $INZS1
103500000000     C                   MOVE      *BLANK        W1POS
103600000000     C                   ENDIF
103700000000     C*
103800000000     C                   ENDSL
103900000000     C*
104000000000     C                   ENDIF
104100000000     C*
104200000000     C                   ENDSR
104300000000     C/EJECT
104400000000     C************************************************************
104500000000     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
104600000000     C************************************************************
104700000000     C     INZD1         BEGSR
104800000000     C*
104900000000     C                   CLEAR                   FMTD1
105000140327R133 C                   Select
105100151201R195 C* Acquisti da San Marino
105200151201R133 C                   When      WTpCom      = 'S'
105300140327 "   C                   Eval      *In11       = *On
105400140327 "   C                   Other
105500140327 "   C                   Eval      *In11       = *Off
105600140327 "   C                   endSl
105700140327R133 C*
105800140327     C*
105900000000     C* Se pgm  richiamato con parametri forniti , li imposto nelle
106000000000     C*    parzializzazioni
106100000000     C                   SELECT
106200000000     C* il pgm è richiamato con chiave univoca per gestione POSTA
106300000000     C     $Unique       WHENEQ    *ON
106400000000     C* il pgm è richiamato e
106500000000     C* sono stati passati dei valori di parzializzazione ma non
106600000000     C* la chiave univoca
106700130828     C     OPZG73        WHENNE    *BLANK
106800000000     C* imposto i valori passati sui campi a video
106900130828     C                   MOVEL     AnnoG73       D1Anno
107000130828     C                   MOVEL     StatoG73      D1Stato
107100130828     C                   MOVEL     PartIvaG73    D1PartIva
107200130828     C                   MOVEL     CdFiscG73     D1CdFisc
107300130828     C                   MOVEL     SoggG73       D1Sogg
107400131206R141 C                   MOVEL     CausaleG73    D1Causale
107500130828     C                   MOVEL     AnnG73        D1Ann
107600130911     C                   MOVEL     CFG73         D1CF
107700000000     C* il pgm non è richiamato
107800130828     C     OPZG73        WHENEQ    *BLANK
107900140418R133 C                   Eval      D1Anno      = *Year
108000151201R195 C* Spesometro
108100140422R133 C                   If        TpComG73     = 'E'
108200151201R195 C                             or
108300151201 "   C* Black List
108400151201R195 C                             TpComG73    = 'B'
108500000000     C                   Eval      D1Anno      = *Year - 1
108600140418R133 c                   EndIf
108700000000     C                   Eval      D1Ann       = '1'
108800140327R133 C                   Eval      D1Mese      = 0
108900000000     c                   ENDSL
109000000000      *
109100151201R195 C* Acquisti da San Marino
109200140422R133 C                   If        TpComG73     = 'S'
109300140422R133 C                   Eval      D1Quadro    = 'SE'
109400140422R133 c                   EndIf
109500140422R133  *
109600151201R195 C* Black List
109700151201 "   C                   If        TpComG73     = 'B'
109800160419R195 C                   Eval      D1Quadro    = 'BL'
109801160419R2214C* mostro solo i dati attivi
109802160419R2214C                   Eval      D1Ann       = '2'
109900160419R195 c                   EndIf
110000160419R195  *
110100000000     c                   ENDSR
110200000000      /EJECT
110300000000      ************************************************************
110400000000      * GESTIONE F02 VIDEO D1
110500000000      ************************************************************
110600000000     C     F02D1         BEGSR
110700000000     C*
110800000000     C* per decodificare la videata richiamo la routine dei ctrl
110900000000     C* e poi disattivo gli eventuali messaggi di errore
111000000000     C                   EXSR      CTRD1
111100000000     C*
111200000000     C                   MOVE      *ALL'0'       IN5098
111300000000     C                   MOVEA     IN5098        *IN(50)
111400000000     C*
111500000000     C                   ENDSR
111600000000     C/EJECT
111700000000     C************************************************************
111800000000     C* GESTIONE F03 VIDEO D1
111900000000     C************************************************************
112000000000     C     F03D1         BEGSR
112100000000     C*
112200000000     C                   MOVE      *ON           $FINE
112300130828    >C                   MOVE      '1'           RETG73
112400000000     C* fine programma
112500000000     C                   EXSR      ENDPGM
112600000000     C*
112700000000     C                   ENDSR
112800000000     C/EJECT
112900000000     C************************************************************
113000000000     C* GESTIONE F05 VIDEO D1
113100000000     C************************************************************
113200000000     C     F05D1         BEGSR
113300000000     C*
113400000000     C                   MOVE      *ON           $INZD1
113500000000     C*
113600000000     C                   ENDSR
113700000000     C/EJECT
113800000000     C************************************************************
113900000000     C* GESTIONE F08 VIDEO D1
114000000000     C************************************************************
114100000000     C     F08D1         BEGSR
114200000000     C*
114300000000     C                   MOVE      *ON           $FINE
114400130828    >C                   MOVE      '0'           RETG73
114500000000     C*
114600000000     C                   ENDSR
114700000000     C/EJECT
114800000000     C************************************************************
114900000000     C* GESTIONE F10 VIDEO D1
115000000000     C************************************************************
115100000000     C     F10D1         BEGSR
115200000000     C*
115300130828    >C                   RESET                   SDGSG74DS
115400130828    >C                   MOVE      xscsoc        SocietaG74
115500130828    >C                   MOVEL     '01'          OPZG74
115600130828    >C     CMTG73        IFEQ      '1'
115700130828    >C                   MOVEL     '1'           CMTG74
115800000000     C                   ELSE
115900130828    >C                   MOVEL     '0'           CMTG74
116000000000     C                   ENDIF
116100130828     C                   Eval      AnnoG74     = D1Anno
116200130828     C                   Eval      AnnG74      = D1Ann
116300151201R195 C*
116400140401R133 C                   Select
116500151201R195 C*
116600151201R195 C* Acquisti da San Marino
116700140401R133 C                   When      TpComG73     = 'S'
116800140401R133 C                   Eval      MeseG74      = D1Mese
116900140401R133 C                   Eval      TpComG74     = 'S'
117000151201R195 C*
117100151201 "   C* Black List
117200151201 "   C                   When      TpComG73     = 'B'
117300151201 "   C                   Eval      MeseG74      = 0
117400151201 "   C                   Eval      TpComG74     = 'B'
117500151201R195 C*
117600140401R133 C                   Other
117700151201R195 C* Spesometro
117800140401R133 C                   Eval      MeseG74     = 0
117900140401R133 C                   Eval      TpComG74     = 'E'
118000140401R133 C                   EndSl
118100151201R195 C*
118200160126R195 C                   Eval      SoggG74     = D1Sogg
118300130828     C                   Eval      TpAnnG74    = D1TpAnn
118400130911     C                   Eval      QuadroG74   = D1Quadro
118500130919     C                   Eval      TpOpeG74    = D1TpOpe
118600000000     C* passare anche le parzializzazioni ricevute relative alla chiave
118700000000     C* univoca :
118800000000     C*  se ce ne sono, potranno essere usate come dft d'immissione
118900130828    >C                   IF        OPZG73 <> *Blank
119000000000     C                   ENDIF
119100000000     C*
119200130828    >C                   CALL      'SDGSG74R'
119300000000     C                   PARM                    KPJBA
119400130828    >C                   PARM                    SDGSG74DS
119500000000     C* ritorno da PGM gestione
119600130828     C                   EXSR      RTNG74
119700000000     C*
119800000000     C                   MOVE      *HIVAL        $LASTV
119900000000     C*
120000000000     C                   ENDSR
120100000000     C/EJECT
120200000000     C************************************************************
120300000000     C* GESTIONE F12 VIDEO D1
120400000000     C************************************************************
120500000000     C     F12D1         BEGSR
120600000000     C*
120700000000     C*  se la 1a videata è la lista oppure sono arrivato qui con F17,
120800000000     C*  per F12 torno alla lista
120900000000     C     $LOP          IFEQ      2
121000000000     C     WINKR         OREQ      *ON
121100000000     C                   MOVE      'S1'          $GEST
121200000000     C*  altrimenti, per F12 torno a menu
121300000000     C                   ELSE
121400000000     C                   MOVE      *ON           $FINE
121500130828    >C                   MOVE      '2'           RETG73
121600000000     C                   ENDIF
121700000000     C*
121800000000     C                   ENDSR
121900000000     C/EJECT
122000000000     C************************************************************
122100000000     C* GESTIONE F18 VIDEO D1
122200000000     C************************************************************
122300000000     C     F18D1         BEGSR
122400000000     C*
122500000000     C                   MOVE      'D2'          $GEST
122600000000     C                   MOVEL     *ON           $INZD2
122700000000     C*
122800000000     C                   ENDSR
122900000000     C/EJECT
123000000000     C************************************************************
123100000000     C* GESTIONE F24 VIDEO D1
123200000000     C* input
123300000000     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
123400000000     C* $FX1 =Testi tasti funzione
123500000000     C* $FC1 = Abilitazioni
123600000000     C* $FR1 = Inversioni di fondo
123700000000     C************************************************************
123800000000     C     F24D1         BEGSR
123900000000     C*
124000000000     C                   MOVEA     $FX1          $FX
124100000000     C                   MOVEA     $FC1          $FC
124200000000     C                   MOVEA     $FR1          $FR
124300000000     C                   Z-ADD     $ULKD1        $ULK
124400000000     C                   Z-ADD     $D1L01        $L01
124500000000     C                   Z-ADD     $D1L02        $L02
124600000000     C                   EXSR      RIEKEY
124700000000     C                   Z-ADD     $ULK          $ULKD1            3 0
124800000000     C                   MOVEA     $FC           $FC1
124900000000     C                   MOVEL     $KEY1         Z1KE1
125000000000     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
125100000000     C                   MOVEL     $KEY2         Z1KE2
125200000000     C                   WRITE     FMTZ1
125300000000     C*
125400000000     C*
125500000000     C                   ENDSR
125600000000     C/EJECT
125700000000     C************************************************************
125800000000     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
125900000000     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
126000000000     C* 93 94 96 97 98
126100000000     C************************************************************
126200000000     C     CTRD1         BEGSR
126300000000     C*
126400000000     C                   SETOFF                                       99
126500000000     C*
126600000000     C                   EXSR      CtrAnno
126700140327R133 C*
126800140327R133 C                   If        WTpCom      = 'S'
126900140327R133 C                   EXSR      CtrMese
127000140327R133 C                   EndIf
127100130911     C*
127200130911     C                   EXSR      CtrCF
127300000000     C*
127400000000     C                   EXSR      CtrStato
127500000000     C*
127600000000     C                   EXSR      CtrSogg
127700131206R141 C*
127800131206R141 C                   EXSR      CtrCausale
127900000000     C*
128000000000     C                   EXSR      CtrAnn
128100130829     C*
128200130829     C                   EXSR      CtrQuadro
128300130919     C*
128400130919     C                   EXSR      CtrTpOpe
128500130829     C*
128600000000     C                   ENDSR
128700000000     C/EJECT
128800000000     C***********************************************************
128900000000     C* Controllo anno di competenza
129000000000     C***********************************************************
129100000000     C     CtrAnno       BEGSR
129200000000     C*
129300000000     C                   if        D1Anno = 0
129400000000     C                   SetOn                                        5399
129500000000     C                   endif
129600000000     C*
129700000000     C                   ENDSR
129800000000     C/EJECT
129900000000     C***********************************************************
130000000000     C* Controllo Stato
130100000000     C***********************************************************
130200000000     C     CtrStato      BEGSR
130300000000     C*
130400000000     C                   Clear                   D1StatoD
130500000000     C                   Clear                   AngG12DS
130600000000     C*
130700000000     C                   if        D1Stato    <> *Blank
130800000000     C*
130900000000     C                   RESET                   SDGTABDS
131000000000     C*
131100000000     C                   Eval      XTbRic      = '1'
131200000000     C                   Eval      XTbAzi      = XScSoc
131300000000     C                   Eval      XTbKey      = *Zero
131400000000     C                   Eval      XTbCod      = 'G12'
131500000000     C                   Move      D1Stato       XTbKey
131600000000     C*
131700000000     C                   CALL      'SDGTABDR'
131800000000     C                   PARM                    SDGTABDS
131900000000     C*
132000000000     C                   if        XTbErr     <> '0'
132100000000     C                   SetOn                                        5299
132200000000     C*
132300000000     C                   else
132400000000     C                   MoveL     XTbUni        AngG12DS
132500000000     C                   MoveL     DesG12        D1StatoD
132600000000     C*
132700000000     C                   endif
132800000000     C*
132900000000     C                   endif
133000000000     C*
133100000000     C                   ENDSR
133200000000     C/EJECT
133300140327R133 C***********************************************************
133400140327     C* Controllo mese
133500140327R133 C***********************************************************
133600140327     C     CtrMese       BEGSR
133700140327     C*
133800140327     C                   Clear                   X07ValDs
133900140327     C*
134000140327     C                   Move      '1'           X07Ric
134100140327     C                   Move      *On           X07All
134200140327     C                   Movel     'ANDIZ000'    X07TRc
134300140327     C                   Movel     'DSPMESE   '  X07Cam
134400140327     C                   Movel     D1Mese        X07VAL
134500140327     C*
134600140327     C                   Callb     'X07VALR'
134700140327     C                   Parm                    X07ValDs
134800140327     C*
134900140327     C     X07ERR        IFEQ      *Off
135000140327     C                   else
135100140327     C                   SetOn                                        7799
135200140327     C                   endif
135300140327     C*
135400140327     C                   ENDSR
135500140327R133 C/EJECT
135600130911     C***********************************************************
135700111003     C* Controllo tipo soggetto
135800130911     C***********************************************************
135900111003     C     CtrCF         BEGSR
136000111003     C*
136100111003     C                   Clear                   D1CFD
136200111003     C*
136300111003     C                   Clear                   X07ValDs
136400111003     C*
136500111003     C                   Move      '1'           X07Ric
136600111003     C                   Move      *On           X07All
136700130919     C                   Movel     'SDGSG73S '   X07TRc
136800130919     C                   Movel     'CFG73    '   X07Cam
136900111003     C                   Movel     D1CF          X07VAL
137000111003     C*
137100111003     C                   Callb     'X07VALR'
137200111003     C                   Parm                    X07ValDs
137300111003     C*
137400111003     C     X07ERR        IFEQ      *Off
137500130911     C                   MOVEL     X07DES        D1CFD
137600111003     C                   else
137700111003     C                   SetOn                                        5499
137800111003     C                   endif
137900111003     C*
138000111003     C                   ENDSR
138100130911     C/EJECT
138200000000     C***********************************************************
138300000000     C* Controllo codice soggetto
138400000000     C***********************************************************
138500000000     C     CtrSogg       BEGSR
138600000000     C*
138700000000     C                   Clear                   D1SoggD
138800000000     C                   if        D1Sogg     <> *Blank
138900000000     C*
139000000000     C     D1SOGG        IFNE      *BLANK
139100000000     C                   CALLb     'XALLINEA'
139200000000     C                   PARM                    D1SOGG
139300000000     C                   PARM      8             LEN               5 0
139400000000     C                   PARM                    XSCINP
139500000000     C                   ENDIF
139600000000     C*
139700000000     C                   MoveL     D1Sogg        WSogg
139800000000     C                   EXSR      CallDvaSog
139900000000     C*
140000000000     C                   if        DVAErrore  <> '1'
140100000000     C*
140200000000     C                   Eval      %subst(DVASOG:1:DVALENOUT)
140300000000     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
140400000000     C*
140500000000     C                   MoveL     DVSDes        D1SoggD
140600000000     C*
140700000000     C                   else
140800000000     C*
140900130930     C                   Eval      *In17       = *On
141000000000     C*
141100000000     C                   endif
141200000000     C*
141300000000     C                   endif
141400000000     C*
141500000000     C                   ENDSR
141600000000     C/EJECT
141700131206R141 C************************************************************
141800131206 "   C* Controllo esistenza causale HD2
141900131206 "   C************************************************************
142000131206R141 C     CtrCausale    BEGSR
142100131206     C*
142200131206     C                   Clear                   D1CausaleD
142300131206     C*
142400131206     C* la causale deve essere valida, controllo con data
142500131206     C* di 10 anni in meno rispetto all'1/01/Anno a video
142600131206     C                   Eval      WAnno       = D1Anno
142700131206     C                   MoveL     WAnno         AnnoDs
142800131206     C                   MoveL     '01'          MeseDs
142900131206     C                   MoveL     '01'          GiornoDs
143000131206     C                   Move      DataDs        WDtAlfa
143100131206     C                   Move      WDtAlfa       WData
143200131206     C     WData         SubDur    10:*Y         WData
143300131206     C                   Move      WData         WDtAlfa
143400131206     C*
143500131206     C                   if        D1Causale  <> *Blank
143600131206     C                   Clear                   ANOPE00F
143700131206     C                   Eval      Lenout = %Size(ANOPE00F)
143800131206     C                   CALLB     'NDMVC003'
143900131206     C                   PARM                    XScSoc
144000131206     C                   PARM                    D1Causale
144100131206     C                   PARM                    WDtAlfa
144200131206     C                   PARM                    Uso
144300131206     C                   PARM      *Off          GesMsg
144400131206     C                   PARM      *Off          Esito
144500131206     C                   PARM      'ANOPE     '  Struttura
144600131206     C                   PARM                    ANOPE00F
144700131206     C                   PARM                    LenOut
144800131206     C                   PARM      *Off          Refresh
144900131206     C*
145000131206     C                   if        Esito     = *on
145100131206     C                   SetOn                                        5199
145200131206     C                   else
145300131206     C*
145400131206     C                   Eval      D1CausaleD  = OPEDesBrev
145500131206     C*
145600131206     C                   endif
145700131206     C*
145800131206     C                   endif
145900131206     C*
146000131206R141 C                   ENDSR
146100131206R141 C/EJECT
146200130919     C***********************************************************
146300130919     C* Controllo tipo operazione
146400130919     C***********************************************************
146500130919     C     CtrTpOpe      BEGSR
146600130919     C*
146700130919     C                   Clear                   D1TpOpeD
146800130919     C*
146900130919     C                   Clear                   X07ValDs
147000130919     C*
147100130919     C                   Move      '1'           X07Ric
147200130919     C                   Move      *On           X07All
147300130919     C                   Movel     'SDGSG73S '   X07TRc
147400130919     C                   Movel     'TPOPEG73 '   X07Cam
147500130919     C                   Movel     D1TpOpe       X07VAL
147600130919     C*
147700130919     C                   Callb     'X07VALR'
147800130919     C                   Parm                    X07ValDs
147900130919     C*
148000130919     C     X07ERR        IFEQ      *Off
148100130919     C                   MOVEL     X07DES        D1TpOpeD
148200130919     C                   else
148300130919     C                   SetOn                                        5599
148400130919     C                   endif
148500130919     C*
148600130919     C                   ENDSR
148700130919     C/EJECT
148800000000     C***********************************************************
148900000000     C* Controllo dati annullati
149000000000     C***********************************************************
149100000000     C     CtrAnn        BEGSR
149200000000     C*
149300000000     C                   Clear                   D1AnnD
149400000000     C*
149500000000     C                   Clear                   X07ValDs
149600000000     C                   Move      '1'           X07Ric
149700000000     C                   Move      *Off          X07All
149800000000     C                   Movel     'ANDIZ000 '   X07TRc
149900000000     C                   Movel     'DSPATT   '   X07Cam
150000000000     C                   Movel     D1Ann         X07VAL
150100000000     C                   Callb     'X07VALR'
150200000000     C                   Parm                    X07ValDs
150300000000     C*
150400000000     C     X07ERR        IFEQ      *Off
150500000000     C                   MOVEL     X07DES        D1AnnD
150600000000     C                   else
150700000000     C                   SetOn                                        5699
150800000000     C                   endif
150900000000     C*
151000000000     C                   Clear                   D1TpAnnD
151100000000     C                   if        D1TpAnn    <>  *blank
151200000000     C*  solo se richiesti solo gli annullati oppure con annullati
151300000000     C                   if        D1Ann      =   '1'    or
151400000000     C                             D1Ann      =   '3'
151500000000     C                   exsr      CtrTpAnn
151600000000     C                   else
151700000000     C                   SetOn                                        5899
151800000000     C                   endif
151900000000     C                   endif
152000000000     C*
152100000000     C                   ENDSR
152200000000     C/EJECT
152300000000     C***********************************************************
152400000000     C* Controllo tipo annullamento
152500000000     C***********************************************************
152600000000     C     CtrTpAnn      BEGSR
152700000000     C*
152800000000     C                   Clear                   X07ValDs
152900000000     C                   Move      '1'           X07Ric
153000000000     C                   Move      *Off          X07All
153100130828     C                   Movel     'SDGSCF000'   X07TRc
153200130828     C                   Movel     'SCFTPANN'    X07Cam
153300000000     C                   Movel     D1TpAnn       X07VAL
153400000000     C                   Callb     'X07VALR'
153500000000     C                   Parm                    X07ValDs
153600000000     C*
153700000000     C     X07ERR        IFEQ      *Off
153800000000     C                   MOVEL     X07DES        D1TpAnnD
153900000000     C                   else
154000000000     C                   SetOn                                        5799
154100000000     C                   endif
154200000000     C*
154300000000     C                   ENDSR
154400000000     C/EJECT
154500130829     C***********************************************************
154600130829     C* Controllo codice quadro di presentazione
154700130829     C***********************************************************
154800130829     C     CtrQuadro     BEGSR
154900111011     C*
155000130828     C                   Clear                   D1QuadroD
155100111011     C*
155200111011     C                   Clear                   X07ValDs
155300111011     C                   Move      '1'           X07Ric
155400111011     C                   Move      *On           X07All
155500160105R195 C*
155600160105 "   C                   Select
155700160105 "   C*
155800160105 "   C* Spesometro
155900160105R195 C                   When      TpComG73    = 'E'
156000130828     C                   Movel     'SDGSG73S '   X07TRc
156100130828     C                   Movel     'QUADROG73 '  X07Cam
156200160105R195 C*
156300160105 "   C* Acquisti da San Marino
156400160105 "   C                   When      TpComG73    = 'S'
156500160105 "   C                   Movel     'ANDIZ000 '   X07TRc
156600160105 "   C                   Movel     'SDGQUADRO1'  X07Cam
156700160105 "   C*
156800160105 "   C* Black List
156900160105 "   C                   When      TpComG73    = 'B'
157000160105 "   C                   Movel     'ANDIZ000 '   X07TRc
157100160105 "   C                   Movel     'SDGQUADRO2'  X07Cam
157200160105 "   C                   endSl
157300160105R195 C*
157400130828     C                   Movel     D1Quadro      X07VAL
157500111011     C                   Callb     'X07VALR'
157600111011     C                   Parm                    X07ValDs
157700111011     C*
157800111011     C     X07ERR        IFEQ      *Off
157900130828     C                   MOVEL     X07DES        D1QuadroD
158000111011     C                   else
158100111011     C                   SetOn                                        5999
158200111011     C                   endif
158300111011     C*
158400111011     C                   ENDSR
158500111011     C/EJECT
158600000000     C************************************************************
158700000000     C* Chiamata al driver dei soggetti
158800000000     C************************************************************
158900000000     C     CallDvaSog    BEGSR
159000000000     C*
159100000000     C                   EVAL      DVALenOut   = %Size(DVASOG)
159200000000     C*
159300000000     C                   CALLB     'NDDVASOG'
159400000000     C                   PARM      '1'           DVARIC
159500000000     C                   PARM      XSCSOC        DVASOCIETA
159600000000     C                   PARM                    DVAUNITA
159700000000     C                   PARM      'DVASOG    '  DVASTRUTT
159800000000     C                   PARM                    DVADTRIF
159900000000     C                   PARM                    DVALENOUT
160000000000     C* Se si accede con il codice cli/for impostare i campi seguenti
160100000000     C* se si accede con il soggetto ometterli e impostare gli altri
160200000000     C*                  PARM                    DVASNATURA
160300000000     C*                  PARM                    DVACODICE
160400000000     C*                  PARM                    DVALINEAV
160500000000     C*                  PARM                    DVAFILIALE
160600000000     C                   PARM                    *omit
160700000000     C                   PARM                    *omit
160800000000     C                   PARM                    *omit
160900000000     C                   PARM                    *omit
161000000000     C                   PARM      WSogg         DVSSOGG
161100000000     C                   PARM      *Blank        DVSTPIND
161200000000     C                   PARM      *Blank        DVSCDIND
161300000000     C                   PARM                    DVAERRORE
161400000000     C                   PARM                    DVAOUTPUT
161500000000     C*
161600000000     C                   ENDSR
161700000000     C/EJECT
161800000000     C************************************************************
161900000000     C* DEFINIZIONE DELL'ORDINAMENTO
162000000000     C************************************************************
162100000000     C     DEFORD        BEGSR
162200000000     C*
162300000000     C* definisco l'ordinamento della lista a seconda dei valori
162400000000     C* impostati nel video di parzializzazione
162500000000     C                   Z-ADD     1             $ORD
162600000000     C*
162700000000     C                   SELECT
162800000000     C* imposto l'ordinamento 99 per ottenere in selezione solo il
162900000000     C* rcd con la chiave passata
163000000000     C     $Unique       WHENEQ    *ON
163100000000     C                   Z-ADD     99            $ORD
163200000000     C* nel caso non sia stato digitato alcun valore in parzializzazione
163300000000     C* utilizzerò l'ordinamento precedentemente memorizzato
163400000000     C* (temporaneamente o definitivamente) nei valori assunti
163500000000     C                   OTHER
163600000000     C                   ENDSL
163700000000     C                   ENDSR
163800000000     C/EJECT
163900000000     C************************************************************
164000000000     C* Chiamata programma con tasto personalizzato
164100000000     C************************************************************
164200000000     C     FPerS1        BEGSR
164300000000     C*
164400000000     C* salvo la DS di ingresso
164500130828    >C                   MOVEL     SDGSG73DS     $SaveDS
164600000000     C*
164700130828    >C                   MOVEL     SDGSG73DS     KPJBU
164800000000     C                   CALL      $PGF2(PosFun)                        22
164900000000     C                   PARM                    KPJBA
165000000000     C*
165100000000     C* ripristino la DS di ingresso
165200130828    >C                   MOVEL     $SaveDS       SDGSG73DS
165300000000     C*
165400000000     C                   ENDSR
165500000000     C/EJECT
165600000000     C************************************************************
165700000000     C* GESTIONE LISTA
165800000000     C************************************************************
165900000000     C     GESS1         BEGSR
166000000000     C*
166100000000     C* inizializzazione videata
166200000000     C     $INZS1        IFEQ      *ON
166300000000     C                   EXSR      INZS1
166400000000     C                   MOVE      *OFF          $INZS1
166500000000     C                   ENDIF
166600000000     C* emissione piede videata se proveniente da altra
166700000000     C* salvataggio valore formato di provenienza
166800000000     C     $LASTV        IFNE      'S1'
166900000000     C                   WRITE     FMTT1
167000000000     C                   EXSR      GESPIE
167100000000     C                   MOVE      $LASTV        $LASTG
167200000000     C                   MOVE      'S1'          $LASTV
167300000000     C                   ENDIF
167400000000     C*
167500000000     C     WMAX          IFEQ      0
167600000000     C                   WRITE     FMTD3                                        NO RCD
167700000000     C                   ENDIF
167800000000     C* Inizializzazione indice tasti funzione personalizzati
167900000000     C                   Eval      PosFun = 25
168000000000     C*
168100000000     C                   EXFMT     FMTC1
168200000000     C*
168300000000     C* azzero la posizione del cursore
168400000000     C                   Z-ADD     0             H1RIGA
168500000000     C                   Z-ADD     0             H1COLO
168600000000     C* Serve per avvertire i pgm di sola finestra di eseguire le operazioni
168700000000     C* di refresh dello sfondo
168800000000     C                   MOVE      '1'           WExf
168900000000     C*
169000000000     C     C1NRR         IFNE      0
169100000000     C                   Z-ADD     C1NRR         WSFL
169200000000     C                   ENDIF
169300000000     C                   IF        SFLNRR > 0
169400000000     C                   Z-ADD     SFLNRR        C1RCD
169500000000     C                   ENDIF
169600000000     C* inizializzazione indice opzioni personalizzate
169700000000     C                   Eval      PosOpz = 0
169800000000     C* CONTROLLO ABILITAZIONE TASTI
169900000000     C* INPUT
170000000000     C* $FC2 = Abilitazioni videata S1
170100000000     C* OUTPUT
170200000000     C* *IN97 + *IN99 Errori
170300000000     C                   MOVEA     $FC2          $FC
170400000000     C                   EXSR      CTRKEY
170500000000     C*
170600000000     C     *IN99         IFEQ      *OFF
170700000000     C*
170800000000     C                   SELECT
170900000000     C* F3=Fine
171000000000     C     $TASTO        WHENEQ    F03
171100000000     C                   EXSR      F03S1
171200000000     C* F8=Successivo
171300000000     C     $TASTO        WHENEQ    F08
171400000000     C                   EXSR      F08S1
171500000000     C* F10=Immissione
171600000000     C     $TASTO        WHENEQ    F10
171700000000     C                   EXSR      F10S1
171800000000     C* F12=Ritorno
171900000000     C     $TASTO        WHENEQ    F12
172000000000     C                   EXSR      F12S1
172100000000     C* F17=Parzializzazioni
172200000000     C     $TASTO        WHENEQ    F17
172300000000     C                   EXSR      F17S1
172400000000     C* F18=Modifica valori assunti
172500000000     C     $TASTO        WHENEQ    F18
172600000000     C                   EXSR      F18S1
172700151204R195 C* F20=Applica Soglia
172800151203  "  C     $TASTO        WHENEQ    F20
172900151203R195 C                   EXSR      F20S1
173000000000     C* F23=Altre opzioni
173100000000     C     $TASTO        WHENEQ    F23
173200000000     C                   EXSR      F23S1
173300000000     C* F24=Altri tasti
173400000000     C     $TASTO        WHENEQ    F24
173500000000     C                   EXSR      F24S1
173600000000     C* ROLL-UP
173700000000     C     $TASTO        WHENEQ    ROLLUP
173800000000     C                   EXSR      ROLS1
173900000000     C* Personalizzata
174000000000     C     $PGF2(PosFun) Whenne    *BLANK
174100000000     C                   EXSR      FperS1
174200000000     C*
174300000000     C                   OTHER
174400000000     C* CONTROLLO DATI
174500000000     C                   EXSR      CTRC1
174600000000     C     *IN99         IFEQ      *OFF
174700000000     C                   EXSR      CTRS1
174800000000     C                   ENDIF
174900000000     C*
175000000000     C                   ENDSL
175100000000     C*
175200000000     C                   ENDIF
175300000000     C*
175400000000     C                   ENDSR
175500000000     C/EJECT
175600000000     C************************************************************
175700000000     C* Caricamento OPZIONI
175800000000     C************************************************************
175900000000    >C     CarOpz        BEGSR
176000000000     C*
176100000000     C* Reperimento tasti di funzione
176200000000     C                   EXSR      REPKEY
176300000000     C*
176400000000     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
176500000000     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
176600000000     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
176700000000     C     $ULKD1        IFEQ      0
176800000000     C                   EXSR      F24D1
176900000000     C                   ELSE
177000000000     C                   WRITE     FMTZ1
177100000000     C                   ENDIF
177200000000     C*
177300000000     C                   ENDSR
177400000000     C/EJECT
177500000000     C************************************************************
177600000000     C* INIZIALIZZAZIONE LISTA
177700000000     C************************************************************
177800000000    >C     INZS1         BEGSR
177900000000     C* pulizia SFL
178000000000     C                   SETOFF                                         3031
178100000000     C                   WRITE     FMTC1
178200000000     C                   SETON                                          31
178300000000     C*
178400000000     C* pulizia del sottotitolo
178500000000     C                   Clear                   D1TIT
178600000000     C*
178700000000     C                   eval      C1Anno  = D1Anno
178800151201R195 C* Acquisti da San Marino
178900140422R133 C                   If        TpComG73     = 'S'
179000140422R133 C                   Eval      C1Mese       = D1Mese
179100140422R133 c                   EndIf
179200000000     C*
179300000000     C* riempimento opzioni
179400000000     C                   MOVEA     $TX1          $TX
179500000000     C                   MOVEA     $FL1          $FL
179600000000     C                   Z-ADD     0             WCOZ
179700000000     C                   Z-ADD     0             WMXO
179800000000     C*  equivale al num.di elementi della schiera $TX1
179900000000     C                   Z-add     OpzLun        WMXO
180000000000     C                   Z-ADD     $C1Op1        $L01
180100000000     C                   Z-ADD     $C1Op2        $L02
180200000000     C                   EXSR      RIEOPZ
180300000000     C                   Z-ADD     23            X
180400000000     C                   MOVE      FF23          $FC2(X)
180500000000     C                   MOVEL     WOPZ1         C1OPZ1
180600000000     C                   MOVEL     $ALLOPT       H1ALLOPT
180700000000     C                   MOVEL     WOPZ2         C1OPZ2
180800000000     C                   Z-ADD     WCOZ          C1COZ
180900000000     C*
181000000000     C* CARICAMENTO SFL A PAGINE
181100000000     C*
181200000000     C                   Z-ADD     0             S1NRR
181300000000     C                   Z-ADD     0             WMAX
181400000000     C*
181500000000     C* Posizionamento su file pilota
181600000000     C                   CLEAR                   C1POS
181700000000     C                   EXSR      SETANA
181800000000     C* ripristino i valori del posizionamento
181900000000     C                   MOVE      W1POS         C1POS
182000000000     C*
182100000000     C* Lettura file pilota
182200000000     C                   EXSR      REDANA
182300000000     C*
182400000000     C* se arrivo dalla parzializzazione devo caricare solo la prima pagina
182500000000     C                   IF        $LastV = 'D1'
182600000000     C                   Z-ADD     SFLPAG        WPAG
182700000000     C                   ENDIF
182800000000     C                   EXSR      ROLS1
182900000000     C* imposto il caricamento sfl del numero massimo di righe per pagina
183000000000     C                   Z-ADD     SFLPAG        WPAG
183100000000     C*
183200000000     C                   ENDSR
183300000000     C/EJECT
183400000000     C************************************************************
183500000000     C* CARICAMENTO PAGINA LISTA
183600000000     C************************************************************
183700000000     C     ROLS1         BEGSR
183800000000     C*
183900000000     C                   SETOFF                                       32
184000000000     C                   Z-ADD     0             Y
184100000000     C                   Z-ADD     WMAX          S1NRR
184200000000     C*
184300000000     C* Leggo dal file anagrafico per caricare la lista
184400000000     C*
184500000000     C     $EndFile      DOWEQ     *OFF
184600000000     C     Y             ANDLT     WPAG
184700000000     C*
184800000000     C                   EXSR      RIES1
184900000000     C*
185000000000     C                   ADD       1             S1NRR
185100000000     C                   ADD       1             Y
185200000000     C*
185300000000     C                   WRITE     FMTS1
185400000000     C*
185500000000     C                   EXSR      REDANA
185600000000     C*
185700000000     C                   ENDDO
185800000000     C*
185900000000     C                   Z-ADD     S1NRR         WMAX                 30
186000000000     C*
186100000000     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
186200000000     C*
186300000000     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
186400000000     C                   MVR                     RESTO             3 0
186500000000     C     PAGINE        MULT      SFLPAG        C1RCD
186600000000     C     RESTO         IFGT      0
186700000000     C                   ADD       1             C1RCD
186800000000     C                   ELSE
186900000000     C                   SUB       SFLPAG        C1RCD
187000000000     C                   ADD       1             C1RCD
187100000000     C                   ENDIF
187200000000     C*
187300000000     C                   ENDSR
187400000000     C/EJECT
187500000000     C************************************************************
187600000000     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
187700000000     C************************************************************
187800000000     C     SETANA        BEGSR
187900000000     C*
188000130828     C                   RESET                   SDG00502DS
188100130829     C*
188200130829     C                   Eval      Soc0502      = XScSoc
188300110824     C*
188400130828     C                   Move      D1Ann         Ann0502
188500130828     C                   Move      D1TpAnn       TpAnn0502
188600130828     C                   if        TpAnn0502   = *Blank
188700130828     C                   MoveL     *Hival        TpAnn0502
188800110824     C                   endif
188900130829     C*
189000140328R133 C                   Select
189100151201R195 C* Acquisti da San Marino
189200140328R133 C                   When      TpComG73     = 'S'
189300140328R133 C                   Eval      TpCom0502    = 'S'
189400151203R195 C* Black List
189500151201 "   C                   When      TpComG73     = 'B'
189600151201R195 C                   Eval      TpCom0502    = 'B'
189700140328R133 C                   Other
189800151201R195 C* Spesometro
189900130829     C                   Eval      TpCom0502    = 'E'
190000140328R133 C                   EndSl
190100151201R195 C*
190200130829     C                   Eval      Anno0502     = D1Anno
190300140328R133 C*
190400140328R133 C                   Select
190500151201R195 C* Acquisti da San Marino
190600140328R133 C                   When      TpComG73     = 'S'
190700140328R133 C                   Eval      Mese0502     = D1Mese
190800140328R133 C                   Other
190900151201R195 C* Spesometro e Black List
191000130829     C                   Eval      Mese0502     = 0
191100140328R133 C                   EndSl
191200130829     C*
191300130829     C                   Eval      CF0502       = D1CF
191400130829     C                   if        CF0502      = *Blank
191500130829     C                   MoveL     *Hival        CF0502
191600130829     C                   endif
191700130829     C*
191800130829     C                   Eval      Stato0502    = D1Stato
191900130829     C                   if        Stato0502   = *Blank
192000130829     C                   MoveL     *Hival        Stato0502
192100130829     C                   endif
192200130829     C*
192300130829     C                   Eval      PIva0502     = D1PartIva
192400130829     C                   if        PIVA0502    = *Blank
192500130829     C                   MoveL     *Hival        PIVA0502
192600130829     C                   endif
192700130829     C*
192800130829     C                   Eval      CdFisc0502   = D1CdFisc
192900130829     C                   if        CdFisc0502  = *Blank
193000130829     C                   MoveL     *Hival        CdFisc0502
193100130829     C                   endif
193200130829     C*
193300130829     C                   Eval      Sogg0502     = D1Sogg
193400130829     C                   if        Sogg0502    = *Blank
193500130829     C                   MoveL     *Hival        Sogg0502
193600130829     C                   endif
193700131206R141 C*
193800131206 "   C                   Eval      Causal0502   = D1Causale
193900131206 "   C                   if        Causal0502  = *Blank
194000131206 "   C                   MoveL     *Hival        Causal0502
194100131206R141 C                   endif
194200130919     C*
194300130919     C                   Eval      TpOpe0502    = D1TpOpe
194400130919     C                   if        TpOpe0502   = *Blank
194500130919     C                   MoveL     *Hival        TpOpe0502
194600130919     C                   endif
194700130829     C*
194800130829     C                   Move      D1Quadro      Quadro0502
194900130829     C                   if        Quadro0502  = *Blank
195000130829     C                   MoveL     *Hival        Quadro0502
195100130829     C                   endif
195200110824     C*
195300130903     C                   MoveL     *Hival        TpN0502
195400130903     C                   MoveL     *Hival        CogRag0502
195500130828     C                   MoveL     *Hival        Nome0502
195600130828     C                   Move      *Hival        DtNasc0502
195700130828     C                   MoveL     *Hival        LocNas0502
195800130828     C                   MoveL     *Hival        PrvNas0502
195900130828     C                   MoveL     *Hival        CodUIC0502
196000130828     C                   MoveL     *Hival        Locali0502
196100130828     C                   MoveL     *Hival        IndirE0502
196200130829     C*
196300130903     C                   MoveL     *Hival        TpNR0502
196400130829     C                   MoveL     *Hival        COGRAR0502
196500130829     C                   MoveL     *Hival        NOMER0502
196600130829     C                   MoveL     *Hival        DTNASR0502
196700130829     C                   MoveL     *Hival        LOCNAR0502
196800130829     C                   MoveL     *Hival        PRVNAR0502
196900130829     C                   MoveL     *Hival        STATOR0502
197000130829     C                   MoveL     *Hival        CODUIR0502
197100130829     C                   MoveL     *Hival        LOCALR0502
197200130829     C                   MoveL     *Hival        INDIER0502
197300130829     C*
197400130829     C                   MoveL     *Hival        PIVAR0502
197500130829     C                   MoveL     *Hival        CDFISR0502
197600130925     C                   MoveL     *Hival        TPOPER0502
197700130925     C                   MoveL     *Hival        NATOPR0502
197800130902     C                   MoveL     *Hival        LEASIR0502
197900130829     C                   MoveL     *Hival        DTREGR0502
198000130829     C                   MoveL     *Hival        NRREGR0502
198100130829     C                   MoveL     *Hival        SERRER0502
198200000000     C*
198300130828     C                   MOVE      $ORD          ORD0502
198400000000     C* richiedo al pgm di lettura il 1° rcd con la conseguente
198500000000     C* inizializzazione delle variabili SQL
198600130828     C                   MOVE      '0'           RIC0502
198700130828     C                   MOVE      '05'          OPZ0502
198800130829     C                   Eval      TpList0502   = '01'
198900130828     C                   MOVE      '01'          TPROTT0502
199000130828     C                   MOVE      '01'          ROTT0502
199100000000     C*
199200000000     C                   SELECT
199300000000     C*
199400000000     C* Gestione posizionamento
199500000000     C* Se il campo W1POS è > *blank imposto il posizionamento con
199600000000     C* W1POS altrimenti lo imposto con il valore di parametrizzazione
199700000000     C*
199800000000    >C* Ord.1 = chiave ordinamento 1
199900000000     C     $ORD          WHENEQ    1
200000000000     C     W1POS         ANDNE     *BLANK
200100130828    >C                   MOVEL     W1POS         POS0502
200200000000     C* Ord.99= Chiave univoca per la POSTA
200300000000     C* (quando viene gestita la chiave univoca NON deve essere gestito
200400000000     C*  il posizionamento C1POS/W1POS)
200500000000     C     $ORD          WHENEQ    99
200600000000     C                   ENDSL
200700000000     C*
200800000000     C                   ENDSR
200900000000     C/EJECT
201000000000     C************************************************************
201100000000     C* LETTURA RCD ARCHIVIO PILOTA
201200000000     C************************************************************
201300000000     C     REDANA        BEGSR
201400000000     C*
201500000000     C                   MOVEL     *OFF          $EndFile
201600000000     C                   MOVEL     *OFF          $RCDOK
201700000000     C*
201800000000     C     $EndFile      DOUEQ     *ON
201900000000     C     $RCDOK        OREQ      *ON
202000000000     C*
202100130828     C                   CALLB     'SDG00502R'
202200000000     C                   PARM                    KPJBA
202300130829     C                   PARM                    SDG00502DS
202400130829     C                   PARM                    SDG00592DS
202500130829     C                   PARM                    SDG005FADS
202600130829     C                   PARM                    SDG005SADS
202700130829     C                   PARM                    SDG005BLDS
202800130829     C                   PARM                    SDG005FEDS
202900130829     C                   PARM                    SDG005FRDS
203000130829     C                   PARM                    SDG005NEDS
203100130829     C                   PARM                    SDG005NRDS
203200130829     C                   PARM                    SDG005DFDS
203300130829     C                   PARM                    SDG005FNDS
203400130829     C                   PARM                    SDG005SEDS
203500131017R136 C                   PARM                    SDG005TUDS
203600130829     C                   PARM                    SDG005TADS
203700000000     C*
203800000000     C* richiedo al pgm di lettura il rcd successivo
203900130828     C                   MOVE      '1'           RIC0502
204000000000     C*
204100130828     C                   MOVE      END0502       $EndFile
204200000000     C*
204300000000     C     $EndFile      IFEQ      *OFF
204400000000     C                   EXSR      SELANA
204500000000     C                   ENDIF
204600000000     C*
204700000000     C                   ENDDO
204800000000     C*
204900000000     C                   MOVE      $EndFile      *IN33                          SflEnd
205000000000     C*
205100000000     C                   ENDSR
205200000000     C/EJECT
205300000000     C************************************************************
205400000000     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
205500000000     C************************************************************
205600000000     C     CLCPAG        BEGSR
205700000000     C* Input :
205800000000     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
205900000000     C*          cursore
206000000000     C* - SFLPAG = numero rcd per pagina sfl
206100000000     C* Output :
206200000000     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
206300000000     C*
206400000000     C     WSFL          DIV       SFLPAG        PAGINE            4 0
206500000000     C                   MVR                     RESTO             3 0
206600000000     C     RESTO         IFGT      0
206700000000     C                   ADD       1             PAGINE
206800000000     C                   ENDIF
206900000000     C     PAGINE        MULT      SFLPAG        WPAG
207000000000     C*
207100000000     C                   ENDSR
207200000000     C/EJECT
207300000000     C************************************************************
207400000000     C* SELEZIONI SULLA RIGA LETTA
207500000000     C* impostare a *OFF il campo $RCDOK per scartare un record che
207600000000     C* non rientra nei parametri di parzializzazione
207700000000     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
207800000000     C* superiore stabilito nei parametri
207900000000     C*
208000000000     C************************************************************
208100000000     C     SELANA        BEGSR
208200000000     C*
208300000000     C                   MOVEL     *ON           $RCDOK
208400000000     C                   MOVE      *hival        ALL9             15
208500000000     C*
208600000000     C* creo limite massimo parzializzazioni :
208700000000     C* per ogni valore alfanumerico creo il limite superiore
208800000000     C* per cui caricare, creando una variabile che contiene
208900000000     C* il valore digitato nel campo a video riempito di "9"
209000000000     C*
209100000000     C                   SELECT
209200000000     C*
209300160329R2203X*ex R195  Acquisti da San Marino
209400160329 "   X*ex R133           When      WTpCom      = 'S'   and
209500160329 "   X*ex R133                     CodUIC0592 <>  037
209600160329 "   X*ex R133           Eval      $RcdOk      = *off
209700160329R2203X*ex R133           Other
209800000000     C*
209900000000     C* SELEZIONI RISPETTO ALLE PARZIALIZZAZIONI
210000000000     C*
210100000000     C                   ENDSL
210200000000     C*
210300000000     C* FACILITAZIONE NEGLI ACCESSI
210400000000     C*
210500000000     C* nel caso che uno dei dati letti sia > del limite superiore
210600000000     C* oppure < del limite inferiore è necessario fare dei
210700000000     C* ragionamenti onde evitare altri accessi inutili;
210800000000     C* questi ragionamenti dovranno tenere conto dell'ordinamento
210900000000     C* attualmente vigente, per cui si potranno verificare i seguenti
211000000000     C* casi :
211100000000     C* 1. un campo è > del limite superiore ed è la 1a chiave della
211200000000     C*    klist dell'ordinamento vigente :
211300000000     C*    nessun altro rcd sarà valido per cui bisogna andare a fine
211400000000     C*    file mediante un *HIVAL SETLL;
211500000000     C* 2. un campo è > del limite superiore e NON è la 1a chiave della
211600000000     C*    klist dell'ordinamento vigente :
211700000000     C*    nessun rcd che verrà letto con gli stessi valori per la
211800000000     C*    precedente porzione di klist sarà valido, per cui
211900000000     C*    conviene saltarli facendo una SETGT apunto con la klist che
212000000000     C*    esclude il campo in esame;
212100000000     C* 3. un campo è < del limite inferiore e NON è la 1a chiave della
212200000000     C*    klist dell'ordinamento vigente :
212300000000     C*    bisogna arrivare al più presto al limite inferiore per cui
212400000000     C*    conviene fare una SETLL con una klist avente come ultimo
212500000000     C*    campo quello in esame, lasciando per gli altri campi gli
212600000000     C*    stessi valori letti ma sostituendo per quello in esame
212700000000     C*    il limite inferiore;
212800000000     C* non è possibile che un campo che è anche chiave primaria
212900000000     C* dell'ordinamento vigente sia < del limite inferiore perchè
213000000000     C* nella routine SETANA la setll viene appunto fatta con il
213100000000     C* limite inferiore;
213200000000     C* non si deve prendere in considerazione il caso che un campo
213300000000     C* non presente nella chiave dell'ordinamento vigente sia
213400000000     C* esterno ai limiti perchè non genererebbe migliorie negli
213500000000     C* accessi;
213600000000     C*
213700000000     C                   IF        $RcdOk = *off
213800000000     C*
213900000000     C                   ENDIF
214000000000     C*
214100000000     C                   ENDSR
214200000000     C/EJECT
214300000000     C************************************************************
214400000000     C* RIEMPIMENTO RIGA LISTA
214500000000     C************************************************************
214600000000     C     RIES1         BEGSR
214700000000     C*
214800000000     C                   CLEAR                   FMTS1
214900000000     C*
215000000000     C* riempio i campi del sfl, compresi gli hidden con i valori
215100000000     C* del rcd letto
215200130828     C                   MoveL     PIva0592      S1PartIva
215300130828     C                   MoveL     CdFisc0592    S1CdFisc
215400130828     C                   MoveL     CogRag0592    S1CogRag
215500130828     C                   MoveL     Nome0592      H1Nome
215600130828     C                   MoveL     DtNasc0592    H1DtNasc
215700130828     C                   MoveL     LocNas0592    H1LocNasc
215800130828     C                   MoveL     PrvNas0592    H1PrvNasc
215900130828     C                   MoveL     Stato0592     H1Stato
216000130828     C                   MoveL     CodUIC0592    H1CodUIC
216100130828     C                   MoveL     Locali0592    H1Localit
216200130828     C                   MoveL     IndirE0592    H1IndirE
216300000000     C*
216400000000     C* azzero le memorizzazioni degli attributi non bottoni
216500000000     C                   MOVEL     *Blank        H1ANNA
216600130828     C                   if        AnnAtt0592  = '1'   or
216700130828     C                             AnnAtt0592  = '9'
216800000000     C                   movel     *ON           H1ANNA
216900000000     C                   endif
217000130828     C                   if        AnnAtt0592  = '1'
217100110824     C                   MoveL     '*'           S1Ann
217200110824     C                   endif
217300000000     C* imposto quegli attributi del sfl che sono modificabili
217400000000     C* al richiamo del determinato pgm
217500000000     C                   EXSR      ATRS1
217600000000     C*
217700000000     C                   ENDSR
217800000000     C/EJECT
217900000000     C************************************************************
218000000000     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
218100000000     C************************************************************
218200000000     C     ATRS1         BEGSR
218300000000     C*
218400000000     C* in questa routine bisogna attivare indicatori o campi programma
218500000000     C* che gestiscono gli attributi di uno o più campi di questa videata;
218600000000     C* attenzione : non è questo il posto in cui eseguire controlli,
218700000000     C*  dovrebbero già arrivare dei flag
218800000000     C*
218900000000     C* azzero gli attributi non bottoni
219000000000     C                   MOVEL     X'A0'         S1ANNA
219100000000     C*
219200000000     C     H1ANNA        IFEQ      *ON
219300000000     C                   MOVEL     ANNUL         S1ANNA
219400000000     C                   ENDIF
219500000000     C*
219600000000     C                   ENDSR
219700000000     C/EJECT
219800000000     C************************************************************
219900000000     C* GESTIONE F08 VIDEO S1
220000000000     C************************************************************
220100000000     C     F08S1         BEGSR
220200000000     C*
220300000000     C                   MOVE      *ON           $FINE
220400130828    >C                   MOVE      '0'           RETG73
220500000000     C*
220600000000     C                   ENDSR
220700000000     C/EJECT
220800000000     C************************************************************
220900000000     C* GESTIONE F03 VIDEO S1
221000000000     C************************************************************
221100000000     C     F03S1         BEGSR
221200000000     C*
221300000000     C                   MOVE      *ON           $FINE
221400130828    >C                   MOVE      '1'           RETG73
221500000000     C* fine programma
221600000000     C                   EXSR      ENDPGM
221700000000     C*
221800000000     C                   ENDSR
221900000000     C/EJECT
222000000000     C************************************************************
222100000000     C* GESTIONE F10 VIDEO S1
222200000000     C* AGGIUNTA RECORD
222300000000     C************************************************************
222400000000     C     F10S1         BEGSR
222500000000     C*
222600130828     C                   RESET                   SDGSG74DS
222700130828     C                   MOVE      xscsoc        SocietaG74
222800130828     C                   MOVEL     '01'          OPZG74
222900130828     C     CMTG73        IFEQ      '1'
223000130828     C                   MOVEL     '1'           CMTG74
223100000000     C                   ELSE
223200130828     C                   MOVEL     '0'           CMTG74
223300000000     C                   ENDIF
223400130828     C                   Eval      AnnoG74     = D1Anno
223500140401R133 C                   Select
223600151201R195 C* Acquisti da San Marino
223700140401R133 C                   When      TpComG73     = 'S'
223800140401R133 C                   Eval      MeseG74      = D1Mese
223900140401R133 C                   Eval      TpComG74     = 'S'
224000151201R195 C*
224100151201 "   C* Black List
224200151201 "   C                   When      TpComG73     = 'B'
224300151201 "   C                   Eval      MeseG74      = 0
224400151201 "   C                   Eval      TpComG74     = 'B'
224500151201R195 C*
224600140401R133 C                   Other
224700151201R195 C*
224800151201R195 C* Spesometro
224900140401R133 C                   Eval      MeseG74     = 0
225000140401R133 C                   Eval      TpComG74     = 'E'
225100140401R133 C                   EndSl
225200151201R195 C*
225300160126R195 C                   Eval      SoggG74     = D1Sogg
225400130828     C                   Eval      AnnG74      = D1Ann
225500130828     C                   Eval      TpAnnG74    = D1TpAnn
225600130911     C                   Eval      QuadroG74   = D1Quadro
225700130919     C                   Eval      TpOpeG74    = D1TpOpe
225800000000     C* passare anche le parzializzazioni ricevute relative alla chiave
225900000000     C* univoca :
226000000000     C*  se ce ne sono, potranno essere usate come dft d'immissione
226100130828    >C                   IF        OPZG73 <> *Blank
226200000000     C                   ENDIF
226300130828    >C                   CALL      'SDGSG74R'
226400000000     C                   PARM                    KPJBA
226500130828    >C                   PARM                    SDGSG74DS
226600000000     C* ritorno da PGM gestione
226700130828    >C                   EXSR      RTNG74
226800000000     C     WINZS1        IFEQ      *ON
226900000000     C                   MOVE      *ON           $INZS1
227000000000     C* se esistevano già righe sul sfl
227100000000     C* calcolo pagina a cui deve posizionarsi
227200000000     C     WSFL          IFGT      0
227300000000     C                   EXSR      CLCPAG
227400000000     C* altrimenti carico solo la 1a pagina
227500000000     C                   ELSE
227600000000     C                   Z-ADD     SFLPAG        WPAG
227700000000     C                   ENDIF
227800000000     C                   ENDIF
227900000000     C*
228000000000     C                   MOVE      *HIVAL        $LASTV
228100000000     C*
228200000000     C                   ENDSR
228300000000     C/EJECT
228400000000     C************************************************************
228500000000     C* GESTIONE F12 VIDEO S1
228600000000     C************************************************************
228700000000     C     F12S1         BEGSR
228800000000     C*
228900000000     C*  se la 1a videata sono le parzializzazioni, per F12 torno a queste
229000000000     C     $LOP          IFEQ      1
229100000000     C                   MOVE      'D1'          $GEST
229200000000     C*  se la 1a videata è questa, per F12 torno a menu
229300000000     C                   ELSE
229400000000     C                   MOVE      *ON           $FINE
229500130828    >C                   MOVE      '2'           RETG73
229600000000     C                   ENDIF
229700000000     C*
229800000000     C                   ENDSR
229900000000     C/EJECT
230000000000     C************************************************************
230100000000     C* GESTIONE F17 VIDEO S1
230200000000     C************************************************************
230300000000     C     F17S1         BEGSR
230400000000     C*
230500000000     C                   MOVE      'D1'          $GEST
230600000000     C* se il pgm è stato richiamato con chiave univoca
230700000000     C* e dopo è stata richiesta una parzializzazione
230800000000     C* tolgo la gestione della chiave univoca (azzere/abblenco il
230900000000     C* valore; propongo come ordinamento il 1°)
231000000000     C     $Unique       IFEQ      *ON
231100000000     C                   MOVE      *OFF          $Unique
231200000000     C                   ENDIF
231300000000     C     $ORD          IFEQ      99
231400000000     C                   Z-ADD     1             $ORD
231500000000     C                   MOVE      *ON           $INZD1
231600000000     C                   ENDIF
231700000000     C*
231800000000     C                   ENDSR
231900000000     C/EJECT
232000000000     C************************************************************
232100000000     C* GESTIONE F18 VIDEO S1
232200000000     C************************************************************
232300000000     C     F18S1         BEGSR
232400000000     C*
232500000000     C                   MOVE      'D2'          $GEST
232600000000     C                   MOVEL     *ON           $INZD2
232700000000     C     $ORD          IFEQ      99
232800000000     C                   Z-ADD     1             $ORD
232900000000     C                   ENDIF
233000000000     C*
233100000000     C                   ENDSR
233200000000     C/EJECT
233300151203R195 C************************************************************
233400151203  "  C* GESTIONE F20 VIDEO S1
233500151203  "  C* Controllo soglia
233600151203  "  C************************************************************
233700151203R195 C     F20S1         BEGSR
233800151203     C*
233900151203     C                   Exsr      CTRCONTINUA
234000151203     C*
234100151203     C                   If        $Continua = *on
234200151203     C*
234300151204     C                   Clear                   SDG00508DS
234400151203     C*
234500151203     C                   Eval      Lr0508      = *On
234600151203     C*
234700151203     C                   Eval      Soc0508     = XscSoc
234800151203     C                   Eval      TpCom0508   = 'B'
234900151203     C                   Eval      Anno0508    = D1Anno
235000151203     C                   Eval      Mese0508    = 0
235100160201     C                   Eval      StpAnn0508  = *Off
235200151203     C*
235300151203     C                   CALLB     'SDG00508R'
235400151203     C                   PARM                    KPJBA
235500151203     C                   PARM                    SDG00508DS
235600151203     C*
235700151203     C                   MOVEL     KPJBU         SDG00508DS
235800151203     C*
235900151203     C* ritorno da PGM gestione
236000151203    >C                   EXSR      RTN508
236100151203     C     WINZS1        IFEQ      *ON
236200151203     C                   MOVE      *ON           $INZS1
236300151203     C* se esistevano già righe sul sfl
236400151203     C* calcolo pagina a cui deve posizionarsi
236500151203     C     WSFL          IFGT      0
236600151203     C                   EXSR      CLCPAG
236700151203     C* altrimenti carico solo la 1a pagina
236800151203     C                   ELSE
236900151203     C                   Z-ADD     SFLPAG        WPAG
237000151203     C                   ENDIF
237100151203     C                   ENDIF
237200151203     C*
237300151203     C                   MOVE      *HIVAL        $LASTV
237400151203     C*
237500151203     C                   Endif
237600151203     C*
237700151203R195 C                   ENDSR
237800151203R195 C/EJECT
237900151203  "  C******************************************************
238000151203  "  C* Controlla se continuare con l'opzione
238100151203  "  C******************************************************
238200151203R195 C     CtrContinua   BEGSR
238300151203     C*
238400151203     C                   Eval      $Continua   = *On
238500151203     C*
238600151203     C                   Eval      $MsgCd1     = 'SDC0099'
238700151203     C*
238800151203     C                   EXSR      MsgPgm
238900151203     C*
239000151203     C                   If        MsgRtn = '2'
239100151203     C                   Eval      $Continua   = *Off
239200151203     C                   Endif
239300151203     C*
239400151203R195 C                   ENDSR
239500151203R195 C/EJECT
239600151203  "  C******************************************************
239700151203  "  C* PGM PER GESTIONE MESSAGGI ERRORE                   *
239800151203  "  C******************************************************
239900151203R!95 C     MSGPGM        BEGSR
240000151203     C*
240100151203     C                   CLEAR                   SDGMSGDS
240200151203     C*
240300151203     C                   MOVEL     'S'           MSGRSP
240400151203     C                   MOVEL     '12'          MSGTPR
240500151203     C                   MOVEL     'S'           MSGEMV
240600151203     C                   Z-ADD     10            MSGRGP
240700151203     C                   MOVEL     'N'           MSGCNL
240800151203     C                   MOVEL     0             MSGMSG
240900151203     C                   MOVEA     STATUS        STS
241000151203     C                   MOVEL     'N'           MSGVID
241100151203     C                   MOVEL     'N'           MSGOPE
241200151203     C                   MOVEL     'N'           MSGSTP
241300151203     C                   MOVEL     'N'           MSGLCK
241400151203     C* Bottoni: Continua/Ritorna
241500151203     C                   EVAL      XMSGCDTXT='COS1025'+'COS1026'
241600151203     C*
241700151203     C                   CALL      'SDGMSGR'
241800151203     C                   PARM                    SDGMSGDS
241900151203     C                   PARM      *BLANKS       XMSGTXT         135
242000151203     C                   PARM      WXMsgInz      XMSGInz           1
242100151203     C                   PARM      $MsgCd1       XMSGCd1           7
242200151203     C                   PARM      *BLANKS       XMSGCd2           7
242300151203     C                   PARM      *BLANKS       XMSGCd3           7
242400151203     C                   PARM      *BLANKS       XMSGSn1           7
242500151203     C*Bottoni: Continua/Ritorna
242600151203     C                   PARM                    XMSGCdTxt        63
242700151203     C*
242800151203     C                   Move      *Off          WXMsgInz
242900151203     C*
243000151203R195 C                   ENDSR
243100151203R195 C/EJECT
243200000000     C************************************************************
243300000000     C* GESTIONE F23 VIDEO S1
243400000000     C************************************************************
243500000000     C     F23S1         BEGSR
243600000000     C*
243700000000     C                   Z-ADD     $C1Op1        $L01
243800000000     C                   Z-ADD     $C1Op2        $L02
243900000000     C                   EXSR      RIEOPZ
244000000000     C                   Z-ADD     23            X
244100000000     C                   MOVE      FF23          $FC2(X)
244200000000     C                   MOVEL     WOPZ1         C1OPZ1
244300000000     C                   MOVEL     $ALLOPT       H1ALLOPT
244400000000     C                   MOVEL     WOPZ2         C1OPZ2
244500000000     C                   Z-ADD     WCOZ          C1COZ
244600000000     C*
244700000000     C                   ENDSR
244800000000     C/EJECT
244900000000     C************************************************************
245000000000     C* GESTIONE F24 VIDEO S1
245100000000     C************************************************************
245200000000     C     F24S1         BEGSR
245300000000     C*
245400000000     C                   MOVEA     $FX2          $FX
245500000000     C                   MOVEA     $FC2          $FC
245600000000     C                   MOVEA     $FR2          $FR
245700000000     C                   Z-ADD     $ULKS1        $ULK
245800000000     C                   Z-ADD     $S1L01        $L01
245900000000     C                   Z-ADD     $S1L02        $L02
246000000000     C                   EXSR      RIEKEY
246100000000     C                   Z-ADD     $ULK          $ULKS1            3 0
246200000000     C                   MOVEA     $FC           $FC2
246300000000     C                   MOVEL     $KEY1         Z2KE1
246400000000     C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
246500000000     C                   MOVEL     $KEY2         Z2KE2
246600000000     C                   WRITE     FMTZ2
246700000000     C*
246800000000     C                   ENDSR
246900000000     C/EJECT
247000000000     C************************************************************
247100000000     C* CONTROLLO TESTATA LISTA
247200000000     C************************************************************
247300000000     C     CTRC1         BEGSR
247400000000     C*
247500000000     C                   MOVE      *OFF          *IN99
247600000000     C*
247700000000     C     C1POS         IFNE      W1POS
247800000000     C                   EXSR      POSS1
247900000000     C                   ENDIF
248000000000     C*
248100000000     C                   ENDSR
248200000000     C/EJECT
248300000000     C************************************************************
248400000000     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
248500000000     C************************************************************
248600000000     C     POSS1         BEGSR
248700000000     C*
248800000000     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
248900000000     C*
249000000000   B1C     WMAX          IFGT      0
249100000000     C                   READC     FMTS1                                  21
249200000000   B2C     *IN21         DOWEQ     *OFF
249300000000     C     *IN99         ANDEQ     *OFF
249400000000     C*
249500000000   B3C     S1OPZ         IFNE      0
249600000000     C                   SETON                                        92  99
249700000000   E3C                   ENDIF
249800000000     C* se rilevata opzione attiva, attivo sflnxtchg
249900000000   B3C     *IN99         IFEQ      *ON
250000000000     C                   MOVE      *ON           *IN32
250100000000     C*
250200000000     C* imposto quegli attributi del sfl che sono modificabili
250300000000     C* al richiamo del determinato pgm
250400000000     C                   EXSR      ATRS1
250500000000     C*
250600000000     C                   UPDATE    FMTS1
250700000000   X3C                   ELSE
250800000000     C                   READC     FMTS1                                  21
250900000000   E3C                   ENDIF
251000000000     C*
251100000000   E2C                   ENDDO
251200000000     C*
251300000000   E1C                   ENDIF
251400000000     C*
251500000000     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
251600000000     C* AL VALORE DI POSIZIONAMENTO :
251700000000     C* se questo è maggiore del valore del rispettivo campo di
251800000000     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
251900000000     C* con i valori di parzializzazione.
252000000000     C*
252100000000   B1C     *IN99         IFEQ      *OFF
252200000000    >C                   MOVE      *BLANK        W1POS
252300000000     C*
252400000000     C* per ogni ordinamento devo testare se il campo dell'inizio elenco
252500000000     C* è maggiore del relativo valore di parzializzazione, nel qual caso
252600000000     C* imposto la variabile del posizionamento (W1Pos) al nuovo valore;
252700000000     C* richiedo comunque un ricaricamento del sfl che, se non si è
252800000000     C* verificata la condizione suddetta avverrà con posizionamento a
252900000000     C* partire dal valore di parzializzazione
253000000000     C*
253100000000   B2C                   SELECT
253200000000    >C* Attenzione : se un ordinamento ha un campo di posizionamento
253300000000    >C* specifico bisogna sostituire la gestione W1Pos/C1Pos con quella
253400000000    >C* dei relativi campi
253500000000     C*
253600000000     C     $ORD          WHENEQ    1
253700000000     C*
253800000000     C* per l'ordinamento 99 = POSTA non viene gestito il posizionamento
253900000000     C*
254000000000   E2C                   ENDSL
254100000000     C*
254200000000     C* richiedo l'inizializzazione sfl
254300000000     C                   MOVE      *ON           $INZS1
254400000000     C*
254500000000   E1C                   ENDIF
254600000000     C*
254700000000     C                   ENDSR
254800000000     C/EJECT
254900000000     C************************************************************
255000000000     C* CONTROLLO OPZIONI LISTA
255100000000     C************************************************************
255200000000     C     CTRS1         BEGSR
255300000000     C*
255400000000     C                   MOVEL     *OFF          $ESCI
255500000000     C                   SETOFF                                       99
255600000000     C                   Z-ADD     0             S1OPZ
255700000000     C*
255800000000     C* Leggo il sfl solo se ci sono rcd
255900000000   B1C     WMAX          IFGT      0
256000000000     C                   READC     FMTS1                                  21
256100000000     C*
256200000000     C* esce se fine sfl o errore che richiede l'uscita
256300000000   B2C     *IN21         DOWEQ     *OFF
256400000000     C     $ESCI         ANDEQ     *OFF
256500000000     C                   Z-ADD     S1NRR         C1RCD
256600000000     C* ctrl su riga
256700000000     C                   EXSR      RECS1
256800000000     C* gestione opzioni
256900000000   B3C     S1OPZ         IFNE      0
257000000000     C     *IN99         ANDEQ     *OFF
257100000000     C                   EXSR      OPZS1
257200000000   E3C                   ENDIF
257300000000     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
257400000000   B3C     *IN99         IFEQ      *ON
257500000000     C                   MOVE      *ON           *IN32
257600000000   E3C                   ENDIF
257700000000     C*
257800000000     C* imposto quegli attributi del sfl che sono modificabili
257900000000     C* al richiamo del determinato pgm
258000000000     C                   EXSR      ATRS1
258100000000     C*
258200000000     C                   Z-ADD     0             S1OPZ
258300000000     C*
258400000000     C                   UPDATE    FMTS1
258500000000     C                   MOVE      *OFF          *IN32
258600000000     C*
258700000000     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
258800000000   B3C     $ESCI         IFEQ      *OFF
258900000000     C                   READC     FMTS1                                  21
259000000000     C* a fine ciclo ripristino il flag richiesta iniz.sfl
259100000000   B4C     *IN21         IFEQ      *ON
259200000000     C                   MOVE      WINZS1        $INZS1
259300000000     C                   MOVE      *off          WINZS1
259400000000     C* se è avvenuta almeno una modifica
259500000000   B5C                   IF        $INZS1 = *on
259600000000     C* calcolo pagina a cui deve posizionarsi
259700000000     C     WSFL          IFGT      0
259800000000     C                   EXSR      CLCPAG
259900000000     C                   ELSE
260000000000     C                   Z-ADD     SFLPAG        WPAG
260100000000     C                   ENDIF
260200000000   E5C                   ENDIF
260300000000   E4C                   ENDIF
260400000000   E3C                   ENDIF
260500000000     C*
260600000000   E2C                   ENDDO
260700000000     C*
260800000000   E1C                   ENDIF
260900000000     C*
261000000000     C                   ENDSR
261100000000     C/EJECT
261200000000     C************************************************************
261300000000     C* CONTROLLO CAMPI I/O RIGA LISTA
261400000000     C************************************************************
261500000000     C     RECS1         BEGSR
261600000000     C*
261700000000     C* reset indicatori DSPATR
261800000000     C                   MOVE      *ALL'0'       IN4049           10
261900000000     C                   MOVEA     IN4049        *IN(40)
262000000000     C*
262100000000     C* Controllo se Utente Autorizzato alla scelta
262200000000     C     S1OPZ         IFNE      0
262300000000     C                   Z-ADD     1             X
262400000000     C     S1OPZ         LOOKUP    $OP1(X)                                21
262500000000     C* mi salvo l'indice dell' opzione
262600000000     C   21              Z-add     X             PosOpz            2 0
262700000000     C     *IN21         IFEQ      *OFF
262800000000     C     $FL1(X)       OREQ      'N'
262900000000     C                   MOVE      *ON           $ESCI
263000000000     C                   SETON                                        409199
263100000000     C                   ENDIF
263200000000     C                   ENDIF
263300000000     C*
263400000000     C*
263500000000     C                   ENDSR
263600000000     C/EJECT
263700000000     C************************************************************
263800000000     C* GESTIONE OPZIONI LISTA
263900000000     C************************************************************
264000000000     C     OPZS1         BEGSR
264100000000     C*
264200000000     C                   Z-ADD     RIRI          CURR
264300000000     C                   Z-ADD     COCO          CURC
264400000000     C*
264500000000     C                   SELECT
264600000000     C*
264700000000     C* se opz=Selezione
264800000000     C* passo la chiave al pgm chiamante
264900000000     C     S1OPZ         WHENEQ    1
265000130828    >C                   MOVE      S1OPZ         OPZG73
265100130828     C* passare nei campi appositi della DS SDGSG73DS il valore
265200000000     C* della chiave della VL univoca (se c'è)
265300000000     C                   MOVE      *ON           $ESCI
265400000000     C                   MOVE      *ON           $FINE
265500000000     C*
265600000000     C                   MOVE      *HIVAL        $LASTV
265700000000     C*
265800000000     C* se opz=Dettaglio
265900000000     C     S1OPZ         WHENEQ    11
266000000000     C                   EXSR      CallListaDett
266100000000     C*
266200000000     C* se opz=quadri
266300000000     C     S1OPZ         WHENEQ    12
266400000000     C                   EXSR      Opzs1_12
266500000000     C*
266600000000     C* se opz=Annulla/Ripristina
266700000000     C     S1OPZ         WHENEQ    44
266800130829     C                   EXSR      AnnRipr
266900000000     C*
267000000000     C                   MOVE      *HIVAL        $LASTV
267100000000     C*
267200000000     C* se selezionata opzione personalizzata, richiamo il programma
267300000000     C* corrispondente
267400000000     C     $PGO1(PosOpz) Whenne    *Blank
267500130828    >C                   RESET                   SDGSG74DS
267600130828    >C                   MOVE      xscsoc        SocietaG74
267700130828    >C                   MOVEL     S1OPZ         OPZG74
267800130828    >C     CMTG73        IFEQ      '1'
267900130828    >C                   MOVEL     '1'           CMTG74
268000000000     C                   ELSE
268100130828    >C                   MOVEL     '0'           CMTG74
268200000000     C                   ENDIF
268300130828     C* passare nei campi appositi della DS SDGSG74DS il valore
268400000000     C* della chiave della VL univoca
268500000000     C*
268600130828    >C                   MOVEL     SDGSG74DS     KPJBU
268700000000     C                   CALL      $PGO1(PosOpz)                        22
268800000000     C                   PARM                    KPJBA
268900000000     C****               PARM                    NOMPGM
269000000000     C*
269100130828    >C                   MOVEL     KPJBU         SDGSG74DS
269200000000     C* ritorno da PGM
269300130828    >C                   EXSR      RTNG74
269400000000     C*
269500000000     C* se altre opzioni
269600000000     C* richiamo pgm gestione anagrafica
269700000000     C                   OTHER
269800000000     C*
269900130828     C                   RESET                   SDGSG74DS
270000000000     C*
270100130828     C                   MOVEL     S1OPZ         OPZG74
270200130828     C     CMTG73        IFEQ      '1'
270300130828     C                   MOVEL     '1'           CMTG74
270400000000     C                   ELSE
270500130828     C                   MOVEL     '0'           CMTG74
270600000000     C                   ENDIF
270700130828     C                   Eval      AnnG74      = D1Ann
270800130828     C                   Eval      TpAnnG74    = D1TpAnn
270900130911     C                   Eval      QuadroG74   = D1Quadro
271000130919     C                   Eval      TpOpeG74    = D1TpOpe
271100000000     C*
271200130828     C                   Move      XScSoc        SocietaG74
271300130828     C                   Eval      AnnoG74     = D1Anno
271400140401R133 C                   Select
271500151201R195 C*
271600151201R195 C* Acquisti da San Marino
271700140401R133 C                   When      TpComG73     = 'S'
271800140401R133 C                   Eval      MeseG74      = D1Mese
271900140401R133 C                   Eval      TpComG74     = 'S'
272000151201R195 C*
272100151201 "   C* Black List
272200151201 "   C                   When      TpComG73     = 'B'
272300151201 "   C                   Eval      MeseG74      = 0
272400151201 "   C                   Eval      TpComG74     = 'B'
272500151201R195 C*
272600140401R133 C                   Other
272700151201R195 C*
272800151201R195 C* Spesometro
272900140401R133 C                   Eval      MeseG74     = 0
273000140401R133 C                   Eval      TpComG74     = 'E'
273100140401R133 C                   EndSl
273200151201R195 C*
273300160126R195 C                   Eval      SoggG74     = D1Sogg
273400130828     C                   Eval      PartIvaG74  = S1PartIva
273500130828     C                   Eval      CdFiscG74   = S1CdFisc
273600130828     C                   Eval      CogRagG74   = S1CogRag
273700130828     C                   Eval      NomeG74     = H1Nome
273800130828     C                   Eval      DtNascG74   = H1DtNasc
273900130828     C                   Eval      LocNascG74  = H1LocNasc
274000130828     C                   Eval      PrvNascG74  = H1PrvNasc
274100130828     C                   Eval      StatoG74    = H1Stato
274200130828     C                   Eval      CodUICG74   = H1CodUIC
274300130828     C                   Eval      LocalitG74  = H1Localit
274400130828     C                   Eval      IndirEG74   = H1IndirE
274500000000     C*
274600130828     C                   CALL      'SDGSG74R'
274700000000     C                   PARM                    KPJBA
274800130828    >C                   PARM                    SDGSG74DS
274900000000     C*
275000000000     C*
275100000000     C* ritorno da PGM gestione
275200130828     C                   EXSR      RTNG74
275300000000     C*
275400000000     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
275500000000     C     *IN99         IFEQ      *ON
275600000000     C                   SETON                                        40
275700000000     C                   ENDIF
275800000000     C*
275900000000     C                   MOVE      *HIVAL        $LASTV
276000000000     C                   ENDSL
276100000000     C*
276200000000     C                   ENDSR
276300000000     C/EJECT
276400000000     C************************************************************
276500000000     C* Chiamata a pgm lista dettaglio
276600000000     C************************************************************
276700000000     C     CallListaDett BEGSR
276800000000     C*
276900130828    >C                   RESET                   SDGSG75DS
277000000000     C*
277100130828    >C                   MOVE      xscsoc        SocietaG75
277200130828    >C                   MOVEL     S1OPZ         OPZG75
277300130828     C                   Eval      AnnoG75     = D1Anno
277400140328R133 C                   Select
277500151201R195 C*
277600151201R195 C* Acquisti da San Marino
277700140328R133 C                   When      TpComG73     = 'S'
277800140328R133 C                   Eval      MeseG75      = D1Mese
277900140328R133 C                   Eval      TpComG75     = 'S'
278000151201R195 C*
278100151201 "   C* Black List
278200151201 "   C                   When      TpComG73     = 'B'
278300151201 "   C                   Eval      MeseG75      = 0
278400151201 "   C                   Eval      TpComG75     = 'B'
278500151201R195 C*
278600140328R133 C                   Other
278700151201R195 C*
278800151201R195 C* Spesometro
278900140328     C                   Eval      MeseG75     = 0
279000140328R133 C                   Eval      TpComG75     = 'E'
279100140328R133 C                   EndSl
279200151201R195 C*
279300130828     C                   Eval      AnnG75      = D1Ann
279400130828     C                   Eval      TpAnnG75    = D1TpAnn
279500130926     C                   Eval      CFG75       = D1CF
279600130919     C                   Eval      TpOpeG75    = D1TpOpe
279700130926     C                   Eval      QuadroG75   = D1Quadro
279800130919     C                   Eval      SoggG75     = D1Sogg
279900131206R141 C                   Eval      CausaleG75  = D1Causale
280000130828     C                   Eval      PartIvaG75  = S1PartIva
280100130828     C                   Eval      CdFiscG75   = S1CdFisc
280200130828     C                   Eval      CogRagG75   = S1CogRag
280300130828     C                   Eval      NomeG75     = H1Nome
280400130828     C                   Eval      DtNascG75   = H1DtNasc
280500130828     C                   Eval      LocNascG75  = H1LocNasc
280600130828     C                   Eval      PrvNascG75  = H1PrvNasc
280700130828     C                   Eval      StatoG75    = H1Stato
280800130828     C                   Eval      CodUICG75   = H1CodUIC
280900130828     C                   Eval      LocalitG75  = H1Localit
281000130828     C                   Eval      IndirEG75   = H1IndirE
281100000000     C*
281200130828     C                   CALL      'SDGSG75R'
281300000000     C                   PARM                    KPJBA
281400130828     C                   PARM                    SDGSG75DS
281500000000     C*
281600130828     C                   EXSR      RTNG75
281700000000     C*
281800000000     C                   ENDSR
281900000000     C/EJECT
282000000000     C************************************************************
282100000000     C* GESTIONE RITORNO DA PGM DI GESTIONE
282200000000     C************************************************************
282300130828    >C     RTNG74        BEGSR
282400000000     C*
282500000000     C* modo di ritorno
282600000000     C*
282700000000     C                   SELECT
282800000000     C* questi modi di utilizzo dei valori di ritorno dal
282900000000     C* pgm di manutenzione rcd di anagrafica sono delle
283000000000     C* proposte, normalmente sempre valide, ma modificabili
283100000000     C* per situazioni particolari
283200000000     C* 1 = F3
283300130828    >C     RETG74        WHENEQ    '1'
283400000000     C                   MOVE      *ON           $ESCI                          sospese READC
283500000000     C                   MOVE      *ON           $FINE                          fine pgm
283600130828    >C                   MOVE      '1'           RETG73
283700000000     C* 2 = F12
283800130828    >C     RETG74        WHENEQ    '2'
283900000000     C* se il pgm di manutenzione ha prima una guida, è necessario anche
284000130828     c* questo test, altrimenti prevale il retG74 e il sfl non viene
284100000000     C* ricaricato
284200130828    >C     OPRG74        ANDNE     '1'
284300000000     C*
284400000000     C* se l'opzione con cui è stato chiamato non è la modifica
284500000000     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
284600130828    >C     OPZG74        IFNE      '02'
284700000000     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
284800000000     C                   else
284900000000     C                   MOVE      *ON           $ESCI                          sospese READC
285000000000     C                   endif
285100000000     C*
285200000000     C                   ENDSL
285300000000     C*
285400000000     C* operazione eseguite dal pgm chiamato
285500000000     C*
285600000000     C                   SELECT
285700000000     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
285800130828    >C     OPRG74        WHENEQ    '1'
285900130828    >C                   MOVE      OPRG74        WOPR                           avverto pgm chiamant
286000000000     C*
286100130828     C                   if        OpzG74      = '01'  or
286200130828     C                             OpzG74      = '03'
286300000000     C                   EXSR      CallDett
286400000000     C                   endif
286500000000     C*
286600000000     C* se l'opzione con cui è stato chiamato non è la modifica
286700000000     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
286800130828    >C     OPZG74        IFNE      '02'
286900000000     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
287000000000     C* se l'opzione con cui è stato chiamato è la modifica
287100000000     C* aggiorno subito la riga di sfl e non richiedo il ripristino sfl
287200000000     C                   ELSE
287300000000     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
287400000000     C                   ENDIF
287500000000     C*
287600000000     C                   ENDSL
287700000000     C*
287800000000     C* funzione non eseguibile per errore :
287900000000     C*
288000000000     C                   SELECT
288100000000     C* 1 = funzione richiamata chiusa in errore
288200130828    >C     ERRG74        WHENEQ    '1'
288300000000     C                   MOVE      *ON           $ESCI                          sospese READC
288400000000     C                   SETON                                        9399      emiss.SFLMSGID
288500000000     C* 2 = funzione richiamata senza la dovuta autorizzazione
288600130828    >C     ERRG74        WHENEQ    '2'
288700000000     C                   MOVE      *ON           $ESCI                          sospese READC
288800000000     C                   SETON                                        9499      emiss.SFLMSGID
288900000000     C*
289000000000     C                   ENDSL
289100000000     C*
289200130828     C                   if        (OPZG74    = '01' and
289300130828    >C                              OPRG74    = '1'      )  or
289400130828     C                             (OPZG74    = '03' and
289500130828    >C                              OPRG74    = '1'      )
289600000000     C                   EXSR      CallListaDett2
289700000000     C                   endif
289800000000     C*
289900000000     C                   ENDSR
290000000000     C/EJECT
290100151203R195 C************************************************************
290200151203 "   C* GESTIONE RITORNO DA PGM DI CONTROLLO SOGLIA
290300151203 "   C************************************************************
290400151203R195 C     RTN508        BEGSR
290500151203     C*
290600151203     C* operazione eseguite dal pgm chiamato
290700151203     C*
290800151203     C*
290900151203     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
291000151203     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
291100151203     C*
291200151203     C* funzione non eseguibile per errore :
291300151203     C*
291400151203     C                   SELECT
291500151203     C* 1 = funzione richiamata chiusa in errore
291600151203    >C     ERR0508       WHENEQ    '1'
291700151203     C                   MOVE      *ON           $ESCI                          sospese READC
291800151203     C                   SETON                                        9399      emiss.SFLMSGID
291900151203     C* 2 = funzione richiamata senza la dovuta autorizzazione
292000151203    >C     ERR0508       WHENEQ    '2'
292100151203     C                   MOVE      *ON           $ESCI                          sospese READC
292200151203     C                   SETON                                        9499      emiss.SFLMSGID
292300151203     C*
292400151203     C                   ENDSL
292500151203     C*
292600151203R195 C                   ENDSR
292700151203R195 C/EJECT
292800000000     C************************************************************
292900000000     C* Chiamata a pgm gestione dati di dettaglio
293000000000     C************************************************************
293100000000     C     CallDett      BEGSR
293200000000     C*
293300130828     C                   RESET                   SDGSG76DS
293400000000     C*
293500130828     C                   MOVE      xscsoc        SocietaG76
293600000000     C*
293700130828     C                   MOVEL     '02'          OPZG76
293800000000     C*
293900130828     C     CMTG73        IFEQ      '1'
294000130828     C                   MOVEL     '1'           CMTG76
294100000000     C                   ELSE
294200130828     C                   MOVEL     '0'           CMTG76
294300000000     C                   ENDIF
294400000000     C*
294500130828     C                   Eval      AnnoG76     = AnnoG74
294600130828     C                   Eval      CFG76       = CFG74
294700130828     C                   Eval      SoggG76     = SoggG74
294800131206R141 C                   Eval      CausaleG76  = CausaleG74
294900130828     C                   Eval      PartIvaG76  = PartIVAG74
295000130828     C                   Eval      CdFiscG76   = CdFiscG74
295100130828     C                   Eval      CogRagG76   = CogRagG74
295200000000     C*
295300130828     C                   Eval      NomeG76     = NomeG74
295400130828     C                   Eval      DtNascG76   = DtNascG74
295500130828     C                   Eval      LocNascG76  = LocNascG74
295600130828     C                   Eval      PrvNascG76  = PrvNascG74
295700130828     C                   Eval      StatoG76    = StatoG74
295800130828     C                   Eval      CodUICG76   = CodUICG74
295900130828     C                   Eval      LocalitG76  = LocalitG74
296000130828     C                   Eval      IndirEG76   = IndirEG74
296100000000     C*
296200130828     C                   Eval      SysG76      = SysG74
296300130828     C                   Eval      NrAsRegG76  = NrAsRegG74
296400130828     C                   Eval      NrRigaG76   = NrRigaG74
296500160211R195 C*
296600160211R195 C                   Eval      TpComG76    = TpComG74
296700000000     C*
296800000000     C* passare anche le parzializzazioni ricevute relative alla chiave
296900000000     C* univoca :
297000000000     C*  se ce ne sono, potranno essere usate come dft d'immissione
297100130828     C                   IF        OPZG73 <> *Blank
297200000000     C                   ENDIF
297300000000     C*
297400130828     C                   CALL      'SDGSG76R'
297500000000     C                   PARM                    KPJBA
297600130828     C                   PARM                    SDGSG76DS
297700000000     C*
297800000000     C* ritorno da PGM gestione
297900130828     C                   EXSR      RTNG76
298000000000     C*
298100000000     C                   ENDSR
298200000000     C/EJECT
298300000000     C************************************************************
298400000000     C* RITORNO DA PGM DI GESTIONE dati di dettaglio
298500000000     C************************************************************
298600130828    >C     RTNG76        BEGSR
298700000000     C*
298800000000     C* modo di ritorno
298900000000     C*
299000000000     C                   SELECT
299100000000     C* questi modi di utilizzo dei valori di ritorno dal
299200000000     C* pgm di manutenzione rcd di anagrafica sono delle
299300000000     C* proposte, normalmente sempre valide, ma modificabili
299400000000     C* per situazioni particolari
299500000000     C* 1 = F3
299600130828    >C     RETG76        WHENEQ    '1'
299700000000     C                   MOVE      *ON           $ESCI                          sospese READC
299800000000     C                   MOVE      *ON           $FINE                          fine pgm
299900130828    >C                   MOVE      '1'           RETG73
300000000000     C* 2 = F12
300100130828    >C     RETG76        WHENEQ    '2'
300200000000     C* se il pgm di manutenzione ha prima una guida, è necessario anche
300300130828     c* questo test, altrimenti prevale il retG74 e il sfl non viene
300400000000     C* ricaricato
300500130828    >C     OPRG76        ANDNE     '1'
300600000000     C                   MOVE      *ON           $ESCI                          sospese READC
300700130828    >C                   MOVE      '2'           RETG73
300800000000     C*
300900000000     C                   ENDSL
301000000000     C*
301100000000     C* operazione eseguite dal pgm chiamato
301200000000     C*
301300000000     C                   SELECT
301400000000     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
301500130828    >C     OPRG76        WHENEQ    '1'
301600130828    >C                   MOVE      OPRG76        WOPR                           avverto pgm chiamant
301700000000     C*
301800000000     C* se l'opzione con cui è stato chiamato non è la modifica
301900000000     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
302000130828    >C**** OPZG76        IFNE      '02'
302100000000     C****               MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
302200000000     C* se l'opzione con cui è stato chiamato è la modifica
302300000000     C* aggiorno subito la riga di sfl e non richiedo il ripristino sfl
302400000000     C****               ELSE
302500000000     C****               EXSR      AGGS1
302600000000     C****               ENDIF
302700000000     C*
302800000000     C                   ENDSL
302900000000     C*
303000000000     C* funzione non eseguibile per errore :
303100000000     C*
303200000000     C                   SELECT
303300000000     C* 1 = funzione richiamata chiusa in errore
303400130828    >C     ERRG76        WHENEQ    '1'
303500000000     C                   MOVE      *ON           $ESCI                          sospese READC
303600000000     C                   SETON                                        9399      emiss.SFLMSGID
303700000000     C* 2 = funzione richiamata senza la dovuta autorizzazione
303800130828    >C     ERRG76        WHENEQ    '2'
303900000000     C                   MOVE      *ON           $ESCI                          sospese READC
304000000000     C                   SETON                                        9499      emiss.SFLMSGID
304100000000     C*
304200000000     C                   ENDSL
304300000000     C*
304400000000     C                   ENDSR
304500000000     C/EJECT
304600000000     C************************************************************
304700000000     C* Chiamata a pgm gestione dati di dettaglio
304800000000     C************************************************************
304900000000     C     CallListaDett2BEGSR
305000000000     C*
305100130828     C                   RESET                   SDGSG75DS
305200000000     C*
305300130828     C                   MOVE      xscsoc        SocietaG75
305400000000     C*
305500130828     C                   MOVEL     '11'          OPZG75
305600000000     C*
305700130828     C     CMTG73        IFEQ      '1'
305800130828     C                   MOVEL     '1'           CMTG75
305900000000     C                   ELSE
306000130828     C                   MOVEL     '0'           CMTG75
306100000000     C                   ENDIF
306200000000     C*
306300130828     C                   Eval      AnnoG75     = AnnoG74
306400140401R133 C                   Eval      MeseG75     = MeseG74
306500140401R133 C                   Eval      TpComG75    = TpComG74
306600130828     C                   Eval      CFG75       = CFG74
306700130828     C                   Eval      SoggG75     = SoggG74
306800131206R141 C                   Eval      CausaleG75  = CausaleG74
306900130828     C                   Eval      PartIvaG75  = PartIvaG74
307000130828     C                   Eval      CdFiscG75   = CdFiscG74
307100130828     C                   Eval      CogRagG75   = CogRagG74
307200000000     C*
307300130828     C                   Eval      NomeG75     = NomeG74
307400130828     C                   Eval      DtNascG75   = DtNascG74
307500130828     C                   Eval      LocNascG75  = LocNascG74
307600130828     C                   Eval      PrvNascG75  = PrvNascG74
307700130828     C                   Eval      StatoG75    = StatoG74
307800130828     C                   Eval      CodUICG75   = CodUICG74
307900130828     C                   Eval      LocalitG75  = LocalitG74
308000130828     C                   Eval      IndirEG75   = IndirEG74
308100000000     C*
308200130828     C                   Eval      SysG75      = SysG74
308300130828     C                   Eval      NrAsRegG75  = NrAsRegG74
308400130828     C                   Eval      NrRigaG75   = NrRigaG74
308500110824     C*
308600000000     C*
308700000000     C* passare anche le parzializzazioni ricevute relative alla chiave
308800000000     C* univoca :
308900000000     C*  se ce ne sono, potranno essere usate come dft d'immissione
309000130828     C                   IF        OPZG73 <> *Blank
309100000000     C                   ENDIF
309200000000     C*
309300130828     C                   CALL      'SDGSG75R'
309400000000     C                   PARM                    KPJBA
309500130828     C                   PARM                    SDGSG75DS
309600000000     C*
309700000000     C* ritorno da PGM gestione
309800130828     C                   EXSR      RTNG75
309900000000     C*
310000000000     C                   ENDSR
310100000000     C/EJECT
310200000000     C************************************************************
310300000000     C* GESTIONE RITORNO DA PGM DI GESTIONE
310400000000     C************************************************************
310500130828    >C     RTNG75        BEGSR
310600000000     C*
310700000000     C* modo di ritorno
310800000000     C*
310900000000     C                   SELECT
311000000000     C* questi modi di utilizzo dei valori di ritorno dal
311100000000     C* pgm di manutenzione rcd di anagrafica sono delle
311200000000     C* proposte, normalmente sempre valide, ma modificabili
311300000000     C* per situazioni particolari
311400000000     C* 1 = F3
311500130828    >C     RETG75        WHENEQ    '1'
311600000000     C                   MOVE      *ON           $ESCI                          sospese READC
311700000000     C                   MOVE      *ON           $FINE                          fine pgm
311800130828    >C                   MOVE      '1'           RETG73
311900000000     C
312000000000     C* 2 = F12
312100130828    >C     RETG75        WHENEQ    '2'
312200000000     C* se il pgm di manutenzione ha prima una guida, è necessario anche
312300130828     c* questo test, altrimenti prevale il retG74 e il sfl non viene
312400000000     C* ricaricato
312500130828    >C     OPRG75        ANDNE     '1'
312600000000     C                   MOVE      *ON           $ESCI                          sospese READC
312700000000     C*
312800000000     C* se l'opzione con cui è stato chiamato non è la modifica
312900000000     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
313000130828    >C     OPZG75        IFNE      '02'
313100000000     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
313200000000     C                   else
313300000000     C                   MOVE      *ON           $ESCI                          sospese READC
313400000000     C                   endif
313500000000     C*
313600000000     C                   ENDSL
313700000000     C*
313800000000     C* operazione eseguite dal pgm chiamato
313900000000     C*
314000000000     C                   SELECT
314100000000     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
314200130828    >C     OPRG75        WHENEQ    '1'
314300130926    >C                   MOVE      OPRG75        WOPR                           avverto pgm chiamant
314400000000     C*
314500000000     C* se l'opzione con cui è stato chiamato non è la modifica
314600000000     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
314700130828    >C     OPZG75        IFNE      '02'
314800000000     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
314900000000     C* se l'opzione con cui è stato chiamato è la modifica
315000000000     C* aggiorno subito la riga di sfl e non richiedo il ripristino sfl
315100000000     C                   ELSE
315200000000     C                   EXSR      AGGS1
315300000000     C                   ENDIF
315400000000     C*
315500000000     C                   ENDSL
315600000000     C*
315700000000     C* funzione non eseguibile per errore :
315800000000     C*
315900000000     C                   SELECT
316000000000     C* 1 = funzione richiamata chiusa in errore
316100130828    >C     ERRG75        WHENEQ    '1'
316200000000     C                   MOVE      *ON           $ESCI                          sospese READC
316300000000     C                   SETON                                        9399      emiss.SFLMSGID
316400000000     C* 2 = funzione richiamata senza la dovuta autorizzazione
316500130828    >C     ERRG75        WHENEQ    '2'
316600000000     C                   MOVE      *ON           $ESCI                          sospese READC
316700000000     C                   SETON                                        9499      emiss.SFLMSGID
316800000000     C*
316900000000     C                   ENDSL
317000000000     C*
317100000000     C                   ENDSR
317200000000     C/EJECT
317300000000     C************************************************************
317400000000     C* Annulla/Ripristina
317500000000     C************************************************************
317600130829     C     AnnRipr       BEGSR
317700000000     C*
317800130829     C                   RESET                   SDG00502DS
317900130829     C*
318000130829     C                   Eval      Soc0502      = XScSoc
318100110824     C*
318200110824     C* passo eventuali parzializzazioni sugli annullati
318300130828     C                   Move      D1Ann         Ann0502
318400130828     C                   Move      D1TpAnn       TpAnn0502
318500130828     C                   if        TpAnn0502   = *Blank
318600130828     C                   MoveL     *Hival        TpAnn0502
318700110824     C                   endif
318800130829     C*
318900140422R133 C                   Select
319000151201R195 C*
319100151201R195 C* Acquisti da San Marino
319200140422R133 C                   When      TpComG73     = 'S'
319300140422R133 C                   Eval      Mese0502     = D1Mese
319400140422R133 C                   Eval      TpCom0502    = 'S'
319500151201R195 C*
319600151201 "   C* Black List
319700151201 "   C                   When      TpComG73     = 'B'
319800151201 "   C                   Eval      Mese0502     = 0
319900151201 "   C                   Eval      TpCom0502    = 'B'
320000151201R195 C*
320100140422R133 C                   Other
320200151201R195 C*
320300151201R195 C* Spesometro
320400140422R133 C                   Eval      Mese0502     = 0
320500140422     C                   Eval      TpCom0502    = 'E'
320600140422R133 C                   EndSl
320700130829     C*
320800130829     C                   Eval      Anno0502     = D1Anno
320900140422R133 C****               Eval      Mese0502     = 0
321000130829     C*
321100130829     C                   Eval      Stato0502    = H1Stato
321200130829     C*
321300130829     C                   Eval      PIva0502     = S1PartIva
321400130829     C*
321500130829     C                   Eval      CdFisc0502   = S1CdFisc
321600131023R137 X****               Move      D1TpAnn       TpAnn0502
321700130919     C*
321800131023R137 C                   Move      D1TpOpe       TpOpe0502
321900130919     C                   if        TpOpe0502   = *Blank
322000130919     C                   MoveL     *Hival        TpOpe0502
322100130919     C                   endif
322200130829     C*
322300130829     C*
322400130829     C* applico eventuale filtro
322500130829     C                   Eval      Quadro0502  = D1Quadro
322600130829     C                   if        Quadro0502  = *Blank
322700130829     C                   MoveL     *Hival        Quadro0502
322800130829     C                   endif
322900000000     C*
323000130912     C                   Eval      TpN0502      = *Blank
323100130912     C                   if        TpN0502     = *Blank
323200130912     C                   MoveL     *Hival        TpN0502
323300130912     C                   endif
323400130912     C*
323500130828     C                   Eval      CogRag0502   = S1CogRag
323600130828     C                   Eval      Nome0502     = H1Nome
323700130828     C                   Eval      DtNasc0502   = H1DtNasc
323800130828     C                   Eval      LocNas0502   = H1LocNasc
323900130828     C                   Eval      PrvNas0502   = H1PrvNasc
324000130828     C                   Eval      CodUIC0502   = H1CodUIC
324100130828     C                   Eval      Locali0502   = H1Localit
324200130828     C                   Eval      IndirE0502   = H1IndirE
324300110824     C*
324400130911     C                   Eval      Sogg0502    = D1Sogg
324500130911     C                   if        Sogg0502    = *Blank
324600130911     C                   MoveL     *Hival        Sogg0502
324700130911     C                   endif
324800131206R141 C*
324900131206 "   C                   Eval      Causal0502  = D1Causale
325000131206 "   C                   if        Causal0502  = *Blank
325100131206 "   C                   MoveL     *Hival        Causal0502
325200131206R141 C                   endif
325300130911     C*
325400130911     C                   Eval      CF0502      = D1CF
325500130911     C                   if        CF0502      = *Blank
325600130911     C                   MoveL     *Hival        CF0502
325700130911     C                   endif
325800130911     C*
325900130911     C                   MoveL     *Hival        COGRAR0502
326000130829     C                   MoveL     *Hival        NOMER0502
326100130829     C                   MoveL     *Hival        DTNASR0502
326200130829     C                   MoveL     *Hival        LOCNAR0502
326300130829     C                   MoveL     *Hival        PRVNAR0502
326400130829     C                   MoveL     *Hival        STATOR0502
326500130829     C                   MoveL     *Hival        CODUIR0502
326600130829     C                   MoveL     *Hival        LOCALR0502
326700130829     C                   MoveL     *Hival        INDIER0502
326800131108R140 C                   MoveL     *Hival        TPNR0502
326900130829     C*
327000130829     C                   MoveL     *Hival        PIVAR0502
327100130829     C                   MoveL     *Hival        CDFISR0502
327200130925     C                   MoveL     *Hival        TPOPER0502
327300130925     C                   MoveL     *Hival        NATOPR0502
327400130902     C                   MoveL     *Hival        LEASIR0502
327500130829     C                   MoveL     *Hival        DTREGR0502
327600130829     C                   MoveL     *Hival        NRREGR0502
327700130829     C                   MoveL     *Hival        SERRER0502
327800000000     C*
327900000000     C* passo lo stato attuale della testata
328000130828     C                   Eval      AnnAtt0502   = H1Anna
328100000000     C*
328200130828     C                   MOVE      $ORD          ORD0502
328300000000     C* richiedo al pgm di lettura il 1° rcd con la conseguente
328400000000     C* inizializzazione delle variabili SQL
328500130828     C                   MOVE      '0'           RIC0502
328600130829     C                   MOVE      '01'          TPList0502
328700130829     C                   MOVE      '44'          OPZ0502
328800130828     C                   MOVE      '02'          TPROTT0502
328900130828     C                   MOVE      '01'          ROTT0502
329000000000     C*
329100130828     C     END0502       DOUEQ     *ON
329200000000     C*
329300130828     C                   CALLB     'SDG00502R'
329400000000     C                   PARM                    KPJBA
329500130828     C                   PARM                    SDG00502DS
329600130828     C                   PARM                    SDG00592DS
329700130911     C                   PARM                    SDG005FADS
329800130911     C                   PARM                    SDG005SADS
329900130911     C                   PARM                    SDG005BLDS
330000130911     C                   PARM                    SDG005FEDS
330100130911     C                   PARM                    SDG005FRDS
330200130911     C                   PARM                    SDG005NEDS
330300130911     C                   PARM                    SDG005NRDS
330400130911     C                   PARM                    SDG005DFDS
330500130911     C                   PARM                    SDG005FNDS
330600130911     C                   PARM                    SDG005SEDS
330700131023R137 C                   PARM                    SDG005TUDS
330800130911     C                   PARM                    SDG005TADS
330900000000     C*
331000000000     C                   move      *on           WOPR
331100000000     C                   move      *on           WINZS1
331200000000     C*
331300000000     C* richiedo al pgm di lettura il rcd successivo
331400130828     C                   MOVE      '1'           RIC0502
331500000000     C*
331600000000     C                   enddo
331700000000     C*
331800000000     C                   ENDSR
331900000000     C/EJECT
332000000000     C************************************************************
332100000000     C* Quadri
332200000000     C************************************************************
332300000000     C     OpzS1_12      BEGSR
332400000000     C*
332500130828    >C                   RESET                   SDGSG77DS
332600000000     C*
332700130828    >C                   MOVE      xscsoc        SocietaG77
332800130828    >C                   MOVEL     S1OPZ         OPZG77
332900130828     C                   Eval      AnnoG77     = D1Anno
333000140401     C                   Eval      AnnoG77     = D1Anno
333100140401R133 C                   Select
333200151201R195 C*
333300151201R195 C* Acquisti da San Marino
333400151201R133 C                   When      WTpCom      = 'S'
333500140401 "   C                   Eval      TpComG77    = 'S'
333600151201R133 C                   Eval      MeseG77     = D1Mese
333700151201R195 C*
333800151201 "   C* Black List
333900151201 "   C                   When      WTpCom       = 'B'
334000151201 "   C                   Eval      TpComG77     = 'B'
334100151201 "   C                   Eval      MeseG77      = 0
334200151201R195 C*
334300151201R133 C                   Other
334400151201R195 C* Spesometro
334500151201R133 C                   Eval      TpComG77    = 'E'
334600140401 "   C                   Eval      MeseG77     = 0
334700140401 "   C                   endSl
334800140401R133 C*
334900130828     C                   Eval      AnnG77      = D1Ann
335000130828     C                   Eval      TpAnnG77    = D1TpAnn
335100130919     C                   Eval      CFG77       = D1CF
335200130919     C                   Eval      QuadroG77   = D1Quadro
335300130919     C                   Eval      TpOpeG77    = D1TpOpe
335400130902     C                   Eval      SoggG77     = D1Sogg
335500131206R141 C                   Eval      CausaleG77  = D1Causale
335600130828     C                   Eval      PartIvaG77  = S1PartIva
335700130828     C                   Eval      CdFiscG77   = S1CdFisc
335800130828     C                   Eval      CogRagG77   = S1CogRag
335900130828     C                   Eval      NomeG77     = H1Nome
336000130828     C                   Eval      DtNascG77   = H1DtNasc
336100130828     C                   Eval      LocNascG77  = H1LocNasc
336200130828     C                   Eval      PrvNascG77  = H1PrvNasc
336300130828     C                   Eval      StatoG77    = H1Stato
336400130828     C                   Eval      CodUICG77   = H1CodUIC
336500130828     C                   Eval      LocalitG77  = H1Localit
336600130828     C                   Eval      IndirEG77   = H1IndirE
336700000000     C*
336800130919     C                   CALL      'SDGSG77R'
336900000000     C                   PARM                    KPJBA
337000130828     C                   PARM                    SDGSG77DS
337100000000     C*
337200130828     C                   EXSR      RTNG77
337300000000     C*
337400000000     C                   ENDSR
337500000000     C/EJECT
337600000000     C************************************************************
337700000000     C* GESTIONE RITORNO DA PGM DI GESTIONE
337800000000     C************************************************************
337900130828    >C     RTNG77        BEGSR
338000000000     C*
338100000000     C* modo di ritorno
338200000000     C*
338300000000     C                   SELECT
338400000000     C* questi modi di utilizzo dei valori di ritorno dal
338500000000     C* pgm di manutenzione rcd di anagrafica sono delle
338600000000     C* proposte, normalmente sempre valide, ma modificabili
338700000000     C* per situazioni particolari
338800000000     C* 1 = F3
338900130828    >C     RETG77        WHENEQ    '1'
339000000000     C                   MOVE      *ON           $ESCI                          sospese READC
339100000000     C                   MOVE      *ON           $FINE                          fine pgm
339200130828    >C                   MOVE      '1'           RETG73
339300000000     C
339400000000     C* 2 = F12
339500130828    >C     RETG77        WHENEQ    '2'
339600000000     C* se il pgm di manutenzione ha prima una guida, è necessario anche
339700130828     c* questo test, altrimenti prevale il retG74 e il sfl non viene
339800000000     C* ricaricato
339900130828    >C     OPRG77        ANDNE     '1'
340000000000     C                   MOVE      *ON           $ESCI                          sospese READC
340100000000     C*
340200000000     C* se l'opzione con cui è stato chiamato non è la modifica
340300000000     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
340400130828    >C     OPZG77        IFNE      '02'
340500000000     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
340600000000     C                   else
340700000000     C                   MOVE      *ON           $ESCI                          sospese READC
340800000000     C                   endif
340900000000     C*
341000000000     C                   ENDSL
341100000000     C*
341200000000     C* operazione eseguite dal pgm chiamato
341300000000     C*
341400000000     C                   SELECT
341500000000     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
341600130828    >C     OPRG77        WHENEQ    '1'
341700130828    >C                   MOVE      OPRG77        WOPR                           avverto pgm chiamant
341800000000     C*
341900000000     C* se l'opzione con cui è stato chiamato non è la modifica
342000000000     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
342100130828    >C     OPZG77        IFNE      '02'
342200000000     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
342300000000     C* se l'opzione con cui è stato chiamato è la modifica
342400000000     C* aggiorno subito la riga di sfl e non richiedo il ripristino sfl
342500000000     C                   ELSE
342600000000     C                   EXSR      AGGS1
342700000000     C                   ENDIF
342800000000     C*
342900000000     C                   ENDSL
343000000000     C*
343100000000     C* funzione non eseguibile per errore :
343200000000     C*
343300000000     C                   SELECT
343400000000     C* 1 = funzione richiamata chiusa in errore
343500130828    >C     ERRG77        WHENEQ    '1'
343600000000     C                   MOVE      *ON           $ESCI                          sospese READC
343700000000     C                   SETON                                        9399      emiss.SFLMSGID
343800000000     C* 2 = funzione richiamata senza la dovuta autorizzazione
343900130828    >C     ERRG77        WHENEQ    '2'
344000000000     C                   MOVE      *ON           $ESCI                          sospese READC
344100000000     C                   SETON                                        9499      emiss.SFLMSGID
344200000000     C*
344300000000     C                   ENDSL
344400000000     C*
344500000000     C                   ENDSR
344600000000     C/EJECT
344700000000     C************************************************************
344800000000     C* AGGIORNAMENTO RIGA LISTA
344900000000     C************************************************************
345000000000     C     AGGS1         BEGSR
345100000000     C*
345200000000     C* riempio i campi del sfl che sono stati ripassati dal pgm
345300000000     C* di manutenzione dopo una opzione 02
345400000000     C*
345500130909     C                   Eval      S1PartIva   = PartIvAG74
345600130909     C                   Eval      S1CdFisc    = CdFiscG74
345700130909     C                   Eval      S1CogRag    = CogRagG74
345800130909     C                   Eval      H1Nome      = NomeG74
345900130909     C                   Eval      H1DtNasc    = DtNascG74
346000130909     C                   Eval      H1LocNasc   = LocNascG74
346100130909     C                   Eval      H1PrvNasc   = PrvNascG74
346200130909     C                   Eval      H1Stato     = StatoG74
346300130909     C                   Eval      H1CodUIC    = CodUICG74
346400130909     C                   Eval      H1Localit   = LocalitG74
346500130909     C                   Eval      H1IndirE    = IndirEG74
346600000000     C*
346700000000     C                   ENDSR
346800000000     C/EJECT
346900000000     C************************************************************
347000000000     C* GESTIONE VALORI ASSUNTI
347100000000     C************************************************************
347200000000     C     GESD2         BEGSR
347300000000     C*
347400000000   B1C     $INZD2        IFEQ      *ON
347500000000     C                   EXSR      INZD2
347600000000     C                   MOVE      *OFF          $INZD2
347700000000   E1C                   ENDIF
347800000000     C*
347900000000   B1C     $LASTV        IFNE      'D2'
348000000000     C                   EXSR      GESPIE
348100000000   B1C     $LASTV        IFNE      *HIVAL
348200000000     C                   MOVE      $LASTV        $LASTG
348300000000     C                   ENDIF
348400000000     C                   MOVE      'D2'          $LASTV
348500000000   E1C                   ENDIF
348600000000     C*
348700000000     C* se esistono errori sulla videata
348800000000     C* emetto la write del formato a indicatori spenti per vedere
348900000000     C* le eventuali decodifiche
349000000000   B1C     *IN99         IFEQ      *ON
349100000000     C                   MOVEA     *IN           WIN
349200000000     C                   MOVE      *ALL'0'       IN5098           49
349300000000     C                   MOVEA     IN5098        *IN(50)
349400000000     C                   WRITE     FMTD2
349500000000     C                   MOVEA     WIN           *IN
349600000000   E1C                   ENDIF
349700000000     C*
349800000000     C                   EXFMT     FMTD2
349900000000     C*
350000000000     C* azzero la posizione del cursore
350100000000     C                   Z-ADD     0             H1RIGA
350200000000     C                   Z-ADD     0             H1COLO
350300000000     C*
350400000000     C* CONTROLLO ABILITAZIONE TASTI
350500000000     C* INPUT
350600000000     C* $FC3 = Abilitazioni videata D2
350700000000     C* OUTPUT
350800000000     C* *IN97 + *IN99 Errori
350900000000     C                   MOVEA     $FC3          $FC
351000000000     C                   EXSR      CTRKEY
351100000000     C*
351200000000   B1C     *IN99         IFEQ      *OFF
351300000000     C*
351400000000   B2C                   SELECT
351500000000     C* F3=Fine
351600000000     C     $TASTO        WHENEQ    F03
351700000000     C                   EXSR      F03D2
351800000000     C* F12=Ritorno
351900000000     C     $TASTO        WHENEQ    F12
352000000000     C                   EXSR      F12D2
352100000000     C* F24=Altri tasti
352200000000     C     $TASTO        WHENEQ    F24
352300000000     C                   EXSR      F24D2
352400000000     C*
352500000000     C                   OTHER
352600000000     C                   EXSR      CTRD2
352700000000   B3C     *IN99         IFEQ      *OFF
352800000000     C                   MOVE      $LASTG        $GEST
352900000000   B4C                   SELECT
353000000000     C     $LASTG        WHENEQ    'S1'
353100000000     C* se è stato modificato l'ordinamento di default, ricarico la lista
353200000000   B5C     $ORD          IFNE      D2ORD
353300000000     C                   MOVE      *ON           $INZS1
353400000000     C                   MOVE      *BLANK        W1POS
353500000000   E5C                   ENDIF
353600000000     C     $LASTG        WHENEQ    'D1'
353700000000     C* per ora niente
353800000000   E4C                   ENDSL
353900000000     C* definisco i nuovi valori di default
354000000000     C                   Z-ADD     D2LOP         $LOP
354100000000     C                   Z-ADD     D2ORD         $ORD
354200000000     C*
354300000000     C* aggiorno valori assunti
354400000000     C* se richiesto il salvataggio
354500000000   B4C     D2BSV         IFEQ      1
354600000000     C                   EXSR      AGGMPE
354700000000   E4C                   ENDIF
354800000000     C*
354900000000   E3C                   ENDIF
355000000000     C*
355100000000   E2C                   ENDSL
355200000000     C*
355300000000   E1C                   ENDIF
355400000000     C*
355500000000     C                   ENDSR
355600000000     C/EJECT
355700000000     C************************************************************
355800000000     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
355900000000     C************************************************************
356000000000     C     INZD2         BEGSR
356100000000     C*
356200000000     C                   CLEAR                   FMTD2
356300000000     C                   EVAL      D2BSVA = Normale
356400000000     C* se sono entrato con chiave univoca
356500000000     C* impedisco il salvataggio valori assunti
356600000000     C     $Unique       IFEQ      *ON
356700000000     C                   EVAL      D2BSVA = Protetto
356800000000     C                   ENDIF
356900000000     C                   MOVEL     $LOP          D2LOP
357000000000     C                   MOVE      $ORD          D2ORD
357100000000     C*
357200000000     C                   ENDSR
357300000000     C/EJECT
357400000000     C************************************************************
357500000000     C* GESTIONE F03 VIDEO D2
357600000000     C************************************************************
357700000000     C     F03D2         BEGSR
357800000000     C*
357900000000     C                   MOVE      *ON           $FINE
358000000000     C* fine programma
358100000000     C                   EXSR      ENDPGM
358200000000     C*
358300000000     C                   ENDSR
358400000000     C/EJECT
358500000000     C************************************************************
358600000000     C* GESTIONE F12 VIDEO D2
358700000000     C************************************************************
358800000000     C     F12D2         BEGSR
358900000000     C*
359000000000     C                   MOVE      $LASTG        $GEST
359100000000     C*
359200000000     C                   ENDSR
359300000000     C/EJECT
359400000000     C************************************************************
359500000000     C* GESTIONE F24 VIDEO D2
359600000000     C************************************************************
359700000000     C     F24D2         BEGSR
359800000000     C*
359900000000     C                   MOVEA     $FX3          $FX
360000000000     C                   MOVEA     $FC3          $FC
360100000000     C                   MOVEA     $FR3          $FR
360200000000     C                   Z-ADD     $ULKD2        $ULK
360300000000     C                   Z-ADD     $D2L01        $L01
360400000000     C                   Z-ADD     $D2L02        $L02
360500000000     C                   EXSR      RIEKEY
360600000000     C                   Z-ADD     $ULK          $ULKD2            3 0
360700000000     C                   MOVEA     $FC           $FC3
360800000000     C                   MOVEL     $KEY1         Z3KE1
360900000000     C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
361000000000     C                   WRITE     FMTZ3
361100000000     C*
361200000000     C                   ENDSR
361300000000     C/EJECT
361400000000     C************************************************************
361500000000     C* CONTROLLO VIDEATA VALORI ASSUNTI
361600000000     C************************************************************
361700000000     C     CTRD2         BEGSR
361800000000     C*
361900000000     C                   SETOFF                                       99
362000000000     C*
362100000000     C* OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
362200000000     C     D2LOP         IFEQ      0
362300000000     C                   SETON                                          5099
362400000000     C                   ENDIF
362500000000     C*
362600000000     C* OBBLIGATORIO VALORE PER ORDINAMENTO
362700000000     C     D2ORD         IFEQ      0
362800000000     C                   SETON                                          5199
362900000000     C                   ENDIF
363000000000     C*
363100000000     C                   ENDSR
363200000000     C/EJECT
363300000000     C************************************************************
363400000000     C* CONTROLLO ABILITAZIONE TASTI
363500000000     C* INPUT
363600000000     C* $TASTO =Tasto funzione premuto
363700000000     C* $FC = Tasti abilitati
363800000000     C* OUTPUT
363900000000     C* IN99 = ERrore
364000000000     C************************************************************
364100000000     C     CTRKEY        BEGSR
364200000000     C*
364300000000     C                   SETOFF                                       99
364400000000     C*
364500000000     C                   SELECT
364600000000     C     $TASTO        WHENEQ    F01
364700000000     C                   z-add     01            posfun
364800000000     C     $FC(1)        IFEQ      '0'
364900000000     C                   SETON                                        9997
365000000000     C                   ENDIF
365100000000     C     $TASTO        WHENEQ    F02
365200000000     C                   z-add     02            posfun
365300000000     C     $FC(2)        IFEQ      '0'
365400000000     C                   SETON                                        9997
365500000000     C                   ENDIF
365600000000     C     $TASTO        WHENEQ    F03
365700000000     C                   z-add     03            posfun
365800000000     C     $FC(3)        IFEQ      '0'
365900000000     C                   SETON                                        9997
366000000000     C                   ENDIF
366100000000     C     $TASTO        WHENEQ    F04
366200000000     C                   z-add     04            posfun
366300000000     C     $FC(4)        IFEQ      '0'
366400000000     C                   SETON                                        9997
366500000000     C                   ENDIF
366600000000     C     $TASTO        WHENEQ    F05
366700000000     C                   z-add     05            posfun
366800000000     C     $FC(5)        IFEQ      '0'
366900000000     C                   SETON                                        9997
367000000000     C                   ENDIF
367100000000     C     $TASTO        WHENEQ    F06
367200000000     C                   z-add     06            posfun
367300000000     C     $FC(6)        IFEQ      '0'
367400000000     C                   SETON                                        9997
367500000000     C                   ENDIF
367600000000     C     $TASTO        WHENEQ    F07
367700000000     C                   z-add     07            posfun
367800000000     C     $FC(7)        IFEQ      '0'
367900000000     C                   SETON                                        9997
368000000000     C                   ENDIF
368100000000     C     $TASTO        WHENEQ    F08
368200000000     C                   z-add     08            posfun
368300000000     C     $FC(8)        IFEQ      '0'
368400000000     C                   SETON                                        9997
368500000000     C                   ENDIF
368600000000     C     $TASTO        WHENEQ    F09
368700000000     C                   z-add     09            posfun
368800000000     C     $FC(09)       IFEQ      '0'
368900000000     C                   SETON                                        9997
369000000000     C                   ENDIF
369100000000     C     $TASTO        WHENEQ    F10
369200000000     C                   z-add     10            posfun
369300000000     C     $FC(10)       IFEQ      '0'
369400000000     C                   SETON                                        9997
369500000000     C                   ENDIF
369600000000     C     $TASTO        WHENEQ    F11
369700000000     C                   z-add     11            posfun
369800000000     C     $FC(11)       IFEQ      '0'
369900000000     C                   SETON                                        9997
370000000000     C                   ENDIF
370100000000     C     $TASTO        WHENEQ    F12
370200000000     C                   z-add     12            posfun
370300000000     C     $FC(12)       IFEQ      '0'
370400000000     C                   SETON                                        9997
370500000000     C                   ENDIF
370600000000     C     $TASTO        WHENEQ    F13
370700000000     C                   z-add     13            posfun
370800000000     C     $FC(13)       IFEQ      '0'
370900000000     C                   SETON                                        9997
371000000000     C                   ENDIF
371100000000     C     $TASTO        WHENEQ    F14
371200000000     C                   z-add     14            posfun
371300000000     C     $FC(14)       IFEQ      '0'
371400000000     C                   SETON                                        9997
371500000000     C                   ENDIF
371600000000     C     $TASTO        WHENEQ    F15
371700000000     C                   z-add     15            posfun
371800000000     C     $FC(15)       IFEQ      '0'
371900000000     C                   SETON                                        9997
372000000000     C                   ENDIF
372100000000     C     $TASTO        WHENEQ    F16
372200000000     C                   z-add     16            posfun
372300000000     C     $FC(16)       IFEQ      '0'
372400000000     C                   SETON                                        9997
372500000000     C                   ENDIF
372600000000     C     $TASTO        WHENEQ    F17
372700000000     C                   z-add     17            posfun
372800000000     C     $FC(17)       IFEQ      '0'
372900000000     C                   SETON                                        9997
373000000000     C                   ENDIF
373100000000     C     $TASTO        WHENEQ    F18
373200000000     C                   z-add     18            posfun
373300000000     C     $FC(18)       IFEQ      '0'
373400000000     C                   SETON                                        9997
373500000000     C                   ENDIF
373600000000     C     $TASTO        WHENEQ    F19
373700000000     C                   z-add     19            posfun
373800000000     C     $FC(19)       IFEQ      '0'
373900000000     C                   SETON                                        9997
374000000000     C                   ENDIF
374100000000     C     $TASTO        WHENEQ    F20
374200000000     C                   z-add     20            posfun
374300000000     C     $FC(20)       IFEQ      '0'
374400000000     C                   SETON                                        9997
374500000000     C                   ENDIF
374600000000     C     $TASTO        WHENEQ    F21
374700000000     C                   z-add     21            posfun
374800000000     C     $FC(21)       IFEQ      '0'
374900000000     C                   SETON                                        9997
375000000000     C                   ENDIF
375100000000     C     $TASTO        WHENEQ    F22
375200000000     C                   z-add     22            posfun
375300000000     C     $FC(22)       IFEQ      '0'
375400000000     C                   SETON                                        9997
375500000000     C                   ENDIF
375600000000     C     $TASTO        WHENEQ    F23
375700000000     C                   z-add     23            posfun
375800000000     C     $FC(23)       IFEQ      '0'
375900000000     C                   SETON                                        9997
376000000000     C                   ENDIF
376100000000     C     $TASTO        WHENEQ    F24
376200000000     C                   z-add     24            posfun
376300000000     C     $FC(24)       IFEQ      '0'
376400000000     C                   SETON                                        9997
376500000000     C                   ENDIF
376600000000     C                   ENDSL
376700000000     C*
376800000000     C                   ENDSR
376900000000     C/EJECT
377000000000     C************************************************************
377100000000     C* AGGIORNAMENTO VALORI ASSUNTI
377200000000     C************************************************************
377300000000     C     AGGMPE        BEGSR
377400000000     C*
377500000000     C* preparo i parametri di chiamata al driver
377600000000     C                   CLEAR                   XMPEDS
377700000000     C                   EVAL      TpChiamata = '03'
377800000000     C                   EVAL      NChiavi = 5
377900000000     C                   EVAL      VLogica = 2
378000000000     C*
378100000000     C* preparo le chiavi per la chiamata al driver
378200000000     C                   CLEAR                   XMPEDS1
378300000000     C                   MOVEL     XSCSOC        DSMPESOC
378400000000     C                   MOVEL     DSPGM         DSMPEPGM
378500000000     C                   MOVEL     KNMUS         DSMPEPRF
378600000000     C*
378700000000     C* Si memorizzano i dati di default a seconda delle modalità con
378800000000     C* cui il pgm è usato
378900000000     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
379000000000     C* MPETIP = 'PV'  pgm richiamato da menù
379100000000     C* MPETIP = 'PB'  x lancio Batch
379200000000     C*
379300000000     C                   SELECT
379400130828    >C     OPZG73        WHENEQ    '01'
379500000000     C                   MOVEL     'PX'          DSMPETIP
379600000000     C                   OTHER
379700000000     C                   MOVEL     'PV'          DSMPETIP
379800000000     C                   ENDSL
379900000000     C* dati
380000000000     C                   MOVEL     DSMPE         DSMPEPAR
380100000000     C*
380200000000     C                   CALLB     'XMPE'
380300000000     C                   PARM                    XMPEDS
380400000000     C                   PARM                    XMPEDS1
380500000000     C*
380600000000     C                   ENDSR
380700000000     C/EJECT
380800000000     C************************************************************
380900000000     C* OPERAZIONI INIZIALI
381000000000     C************************************************************
381100000000     C     *INZSR        BEGSR
381200000000     C*
381300000000     C* Reperimento parametri
381400000000     C*
381500000000     C     *ENTRY        PLIST
381600000000     C                   PARM                    KPJBA
381700000000     C                   PARM                    DSAUT
381800000000     C*
381900000000     C* testo se sono stati passati i dati di autorizzazione
382000000000     C*
382100000000     C     PARMS         IFLT      2
382200000000     C                   MOVE      *BLANK        AUTDS
382300000000     C                   ELSE
382400000000     C                   MOVEL     DSAUT         AUTDS
382500000000     C                   ENDIF
382600000000     C*
382700000000     C* Reperimento dati società
382800000000     C*
382900000000     C                   MOVEL     'SOC001'      TIPXSC
383000000000     C                   MOVEL     *BLANK        SOCXSC
383100000000     C                   EXSR      REPSOC
383200000000     C*
383300000000     C* Controlli dati società
383400000000     C*
383500000000     C     RTNXSC        IFNE      '1'
383600000000     C                   MOVEL     XSOCDS        SOC001
383700000000     C                   ELSE
383800000000     C                   MOVE      *ON           $FINE
383900130828    >C                   MOVE      '1'           ErrG73
384000000000     C                   EXSR      ENDPGM
384100000000     C                   ENDIF
384200000000     C*
384300000000     C* reperisco le dimensioni delle variabili che conterranno i tasti
384400000000     C                   EVAL      $D1L01 = %Size(Z1Ke1)
384500000000     C                   EVAL      $D1L02 = %Size(Z1Ke2)
384600000000     C                   EVAL      $S1L01 = %Size(Z2Ke1)
384700000000     C                   EVAL      $S1L02 = %Size(Z2Ke2)
384800000000     C                   EVAL      $C1Op1 = %Size(C1Opz1)
384900000000     C                   EVAL      $C1Op2 = %Size(C1Opz2)
385000000000     C                   EVAL      $D2L01 = %Size(Z3Ke1)
385100000000     C                   EVAL      $D2L02 = 0
385200000000     C*
385300000000     C* Reperimento opzioni
385400000000     C*
385500000000     C                   EXSR      REPOPZ
385600000000     C*
385700000000     C* Reperimento tasti di funzione
385800000000     C*
385900000000     C                   EXSR      REPKEY
386000000000     C* Chiamata a routine per il reperimento delle opzioni
386100000000     C* personalizzate
386200000000    >C* se si vogliono abilitare i tasti personalizzati in ogni videata
386300000000    >C* ripeterla passando le schiere dei tasti funzione
386400000000     C                   CAllb     'X46USREXT'
386500000000     C                   PARM                    $OP1
386600000000     C                   PARM                    $TX1
386700000000     C                   PARM                    $FL1
386800000000     C                   PARM                    $PGO1
386900000000     C                   PARM      $dimOp        NumOpz
387000000000     C                   PARM                    $FX2
387100000000     C                   PARM                    $FC2
387200000000     C                   PARM                    $PGF2
387300000000     C                   PARM                    DsPgm
387400000000     C                   PARM                    xsccut
387500000000     C                   PARM                    xscgrp
387600000000     C*
387700000000     C                   Z-add     1             OpzLun
387800000000     C     *blank        Lookup    $TX1(OpzLun)                           21
387900000000     C                   if        *in21 = *on
388000000000     C                   sub       1             OpzLun
388100000000     C                   else
388200000000     C                   Z-add     $DimOp        OpzLun
388300000000     C                   endif
388400000000     C*
388500000000     C                   ENDSR
388600000000     C/EJECT
388700000000     C************************************************************
388800000000     C* INIZIALIZZAZIONE VARIABILI
388900000000     C************************************************************
389000000000     C     INZVAR        BEGSR
389100000000     C*
389200000000     C* Dati da PGM chiamante
389300000000     C*
389400130828    >C                   MOVEL     KPJBU         SDGSG73DS
389500140327R133 C                   Eval      WTpCom      = TpComG73
389600000000     C*
389700000000     C* Se società diversa da quella impostata nella *inzsr, rieseguo
389800000000     C* reperimento dati società
389900130828    >C                   IF        SocietaG73 <> xscsoc  and
390000130828    >C                             OpzG73 <>*blank
390100000000     C                   MOVEL     'SOC001'      TIPXSC
390200130828    >C                   MOVEL     SocietaG73    SOCXSC
390300000000     C                   EXSR      REPSOC
390400000000     C     RTNXSC        IFNE      '1'
390500000000     C                   MOVEL     XSOCDS        SOC001
390600000000     C                   ELSE
390700000000     C                   MOVE      *ON           $FINE
390800130828    >C                   MOVE      '1'           ErrG73
390900000000     C                   EXSR      ENDPGM
391000000000     C                   ENDIF
391100000000     C                   ENDIF
391200000000     C*
391201160321R2195C*
391202160321 "   C                   Select
391203160321 "   C* se Spesometro o Acquisti da San marino
391204160321 "   C                   When      TpComG73    = 'E'     or
391205160321R2195C                             TpComG73    = 'S'
391300130828     C* Reperisco i dati di setup del modulo Spesometro
391400130828     C                   EXSR      RepSDGSPECF
391401160321R2195C* se Black List
391402160321 "   C                   When      TpComG73    = 'B'
391403160321 "   C                   EXSR      RepSDBLACK2
391404160321 "   C*
391405160321R2195C                   endSl
391500000000     C*
391600000000     C* Reperimento autorizzazioni
391700000000     C*
391800000000     C* le autorizzazioni potrebbero essere già state passate,
391900000000     C* nel qual caso non vanno reperite
392000000000    >C                   EXSR      REPAUT
392100000000     C*
392200000000     C*
392300000000     C* Reperimento valori default utente
392400000000     C*
392500000000     C                   EXSR      REPDFT
392600000000     C*
392700000000     C* Definizione delle opzioni utilizzabili a pgm
392800000000     C*
392900000000     C                   EXSR      DEFOPZ
393000000000     C*
393100000000     C* Variabili per gestione videate
393200000000     C*
393300000000     C                   MOVE      *OFF          $FINE                          fine pgm
393400000000     C                   MOVE      *OFF          $INZD1                         inizializzione D1
393500000000     C                   MOVE      *OFF          $INZS1                         inizializzione S1
393600000000     C                   MOVE      *OFF          $INZD2                         inizializzione D2
393700000000     C                   MOVE      *OFF          $EndFile                       memo fine file
393800000000     C                   MOVE      *OFF          $ESCI                          rich.uscita ciclo
393900000000     C                   MOVE      *OFF          $RCDOK                         rcd letto valido
394000000000     C                   MOVE      *HIVAL        $LASTV
394100000000     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
394200000000     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
394300000000     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
394400000000     C* azzero la posizione del cursore
394500000000     C                   Z-ADD     0             H1RIGA
394600000000     C                   Z-ADD     0             H1COLO
394700000000     C*
394800000000     C                   MOVE      *ON           $INZD1
394900000000     C                   MOVE      *ON           $INZS1
395000000000     C*
395100000000     C* se pgm richiamato con chiave univoca impostata,
395200000000     C* forzo la emissione della lista e imposto l'ordinamento
395300000000    >C*  in questo esempio la chiave univoca per la posta è Codice
395400000000    >C*  ma qualora ci fosse più di un campo basterebbe testare quello più
395500000000    >C*  significativo (o meglio, quello che si sa essere obbligatorio
395600000000    >C*  ricevere pieno)
395700000000     C                   MOVE      *OFF          $Unique
395800000000     C*
395900000000     C* Variabili appoggio
396000000000     C*
396100000000     C                   Z-ADD     SFLPAG        WPAG
396200000000     C                   MOVEL     *OFF          WOPR
396300000000     C                   CLEAR                   W1POS
396400000000     C*
396500000000     C* definisco qual'è la prima videata
396600000000     C* (se la prima videata risulta la lista, devo comunque caricare
396700000000     C*  la videata di parzializzazione e gestire la definizione
396800000000     C*  dell'ordinamento, perchè, dai default oppure se sono stati
396900000000     C*  passati dei dati, può variare il caricamento)
397000000000     C                   SELECT
397100000000     C     $LOP          WHENEQ    1
397200000000     C                   MOVE      'D1'          $GEST
397300000000     C     $LOP          WHENEQ    2
397400000000     C                   MOVE      'S1'          $GEST
397500000000     C                   EXSR      INZD1
397600000000     C                   MOVE      *OFF          $INZD1
397700000000     C                   EXSR      DEFORD
397800000000     C                   ENDSL
397900000000     C*
398000000000     C* Valorizzazione campi univoci testate
398100000000     C*
398200000000     C                   CLEAR                   FMTT1
398300000000     C                   MOVEL     KNSIF         NOMSIF
398400000000     C                   MOVEL     XSCDSI        NOMDIT
398500000000     C                   MOVEL     KCDAZ         NOMAZN
398600000000     C****               MOVEL     DSPGM         NOMPGM
398700000000     C* recupero il sottotitolo (se c'è)
398800000000     C                   MOVEL     *BLANK        T1TIT
398900140401     C*
399000140401     C     SOTTIT        IFNE      *BLANK
399100140401     C                   MOVEL     SOTTIT        IDMSG
399200000000     C                   CALLB     'XRTVM'
399300000000     C                   PARM                    IDMSG            27
399400000000     C                   PARM                    TXTMSG          100
399500000000     C                   PARM                    ERRMSG            1
399600000000     C                   PARM      *ON           CTRMSG            1            centratura
399700000000     C                   PARM      30            LENMSG            3 0          lun output.
399800000000     C     ERRMSG        IFNE      *ON
399900000000     C                   MOVEL     TXTMSG        T1TIT
400000000000     C                   ELSE
400100000000     C                   MOVEL     *ALL'?'       T1TIT
400200000000     C                   ENDIF
400300140401     C                   ENDIF
400400151201R195 C*
400500151201  "  C* Recupero il titolo con il tipo di comunicazione
400600151201  "  C                   MOVEL     *BLANK        T1TIT
400700151201  "  C*
400800151201  "  C                   Select
400900151201  "  C*
401000151201  "  C* Comunicazione Spesometro
401100151201  "  C                   When      TpComG73   = 'E'
401200151201  "  C                   if        SOTTITT1   <> *BLANK
401300151201  "  C                   MOVEL     SOTTITT1      IDMSG
401400151201  "  C                   endif
401500151201  "  C*
401600151201  "  C* Comunicazione Acquisti da San Marino
401700151201  "  C                   When      TpComG73   = 'S'
401800151201  "  C                   if        SOTTITT2   <> *BLANK
401900151201  "  C                   MOVEL     SOTTITT2      IDMSG
402000151201  "  C                   endif
402100151201  "  C*
402200151201  "  C* Comunicazione Black list
402300151201  "  C                   When      TpComG73   = 'B'
402400151201  "  C                   if        SOTTITT3   <> *BLANK
402500151201  "  C                   MOVEL     SOTTITT3      IDMSG
402600151201  "  C                   endif
402700151201  "  C*
402800151201  "  C                   endsl
402900151201  "  C*
403000151201  "  C                   CALLB     'XRTVM'
403100151201  "  C                   PARM                    IDMSG            27
403200151201  "  C                   PARM                    TXTMSG          100
403300151201  "  C                   PARM                    ERRMSG            1
403400151201  "  C                   PARM      *ON           CTRMSG            1            centratura
403500151201  "  C                   PARM      30            LENMSG            3 0          lun output.
403600151201  "  C     ERRMSG        IFNE      *ON
403700151201  "  C                   MOVEL     TXTMSG        T1TIT
403800151201  "  C                   ELSE
403900151201  "  C                   MOVEL     *ALL'?'       T1TIT
404000151201R195 C                   ENDIF
404100000000     C*
404200000000     C                   ENDSR
404300000000     C/EJECT
404400000000     C************************************************************
404500000000     C* REPERIMENTO DATI SOCIETA'
404600000000     C************************************************************
404700000000     C     REPSOC        BEGSR
404800000000     C*
404900000000     C                   CALL      'SDGSOCDR'
405000000000     C                   PARM                    TIPXSC            6
405100000000     C                   PARM                    SOCXSC            3
405200000000     C                   PARM                    CDSXSC            9 0
405300000000     C                   PARM                    MODXSC            3
405400000000     C                   PARM      *BLANKS       RTNXSC            1
405500000000     C                   PARM                    XSOCDS
405600000000     C                   PARM                    KPJBA
405700000000     C*
405800000000     C                   ENDSR
405900000000     C/EJECT
406000000000     C************************************************************
406100130828     C* Rerimento parametri di setup per Spesometro
406200000000     C************************************************************
406300130828     C     RepSDGSPECF   BEGSR
406400000000     C*
406500130828     C                   Clear                   SDGPA08DS
406600000000     C*
406700000000     C                   CALLB     'XPAR'
406800000000     C                   Parm                    XScSoc
406900130828     C                   Parm      'SDGSPECF'    XPaPar            8
407000000000     C                   Parm                    XPaOut         2000
407100000000     C                   Parm      *OFF          XPaErr            1
407200000000     C                   Parm      ' '           XPaRic            1
407300000000     C*
407400000000     C                   if        XPaErr      = *On
407500000000     C                   EXSR      MsgErr2
407600000000     C                   Eval      $Fine       = *On
407700000000     C                   EXSR      ENDPGM
407800000000     C*
407900000000     C                   else
408000130828     C                   MoveL     XPaOut        SDGPA08DS
408100000000     C*
408200130828     C                   if        AttSCFA08   <> *On
408300000000     C                   EXSR      ENDPGM
408400000000     C                   endif
408500000000     C*
408600000000     C                   endif
408700000000     C*
408800000000     C                   ENDSR
408900000000     C/EJECT
409000000000     C************************************************************
409100000000     C* Messaggio errore 2
409200000000     C************************************************************
409300000000     C     MsgErr2       BEGSR
409400000000     C*
409500000000     C                   Reset                   SDGMSGDS
409600000000     C*
409700000000     C                   MoveL     'S'           MSGEMV
409800000000     C                   Z-Add     10            MSGRGP
409900000000     C                   MoveL     'N'           MSGRSP
410000000000     C                   MoveL     '  '          MSGTPR
410100000000     C*
410200000000     C                   MoveL     XPaPar        $P1PRO2184
410300000000     C*
410400000000     C                   call      'SDGMSGR'
410500000000     C                   parm                    SDGMSGDS
410600000000     C                   parm                    XMsgTxt         135
410700000000     C*
410800000000     C* Modo di inizializzazione della window:
410900000000     C* ('1'->Close + Open  -->I° volta dopo l'emissione del video princip.)
411000000000     C* ('0'->rimane aperto -->tutte le emissioni successive se consecutive,
411100000000     C*                       per mantenere il sottofondo del video princip.)
411200000000     C                   parm      '1'           XMsgInz           1
411300000000     C*
411400000000     C* PRO2184  - Fallito reperimento recupero parametro modulo &1
411500000000     C                   parm      'PRO2184'     XMsgCd1           7
411600000000     C                   parm                    XMsgCd2           7
411700000000     C                   parm                    XMsgCd3           7
411800000000     C                   parm                    XMsgSn1           7
411900000000     C                   parm                    XMsgCdTxT        63
412000000000     C                   parm                    MsgCdTit         14
412100000000     C                   parm                    MsgClPrtf         1
412200000000     C                   parm      $P1PRO2184    VarMsg1         100
412300000000     C                   parm                    VarMsg2         100
412400000000     C                   parm                    VarMsg3         100
412500000000     C*
412600000000     C                   ENDSR
412700000000     C/EJECT
412701160321R2195C************************************************************
412702160321 "   C* Reperimento parametri di setup per Black List
412703160321 "   C************************************************************
412704160321R2195C     RepSDBLACK2   BEGSR
412705160321     C*
412706160321     C                   Clear                   SDGPA11DS
412707160321     C*
412708160321     C                   CALLB     'XPAR'
412709160321     C                   Parm                    XScSoc
412710160321     C                   Parm      'SDBLACK2'    XPaPar            8
412711160321     C                   Parm                    XPaOut         2000
412712160321     C                   Parm      *OFF          XPaErr            1
412713160321     C                   Parm      ' '           XPaRic            1
412714160321     C*
412715160321     C                   if        XPaErr      = *On
412716160321     C                   EXSR      MsgErr2
412717160321     C                   Eval      $Fine       = *On
412718160321     C                   EXSR      ENDPGM
412719160321     C*
412720160321     C                   else
412721160321     C                   MoveL     XPaOut        SDGPA11DS
412722160321     C*
412723160321     C                   if        AttBLA11    <> *On
412724160321     C                   EXSR      ENDPGM
412725160321     C                   endif
412726160321     C*
412727160321     C                   endif
412728160321     C*
412729160321R2195C                   ENDSR
412730160321R2195C/EJECT
412800000000     C************************************************************
412900000000     C* REPERIMENTO AUTORIZZAZIONI UTENTE
413000000000     C************************************************************
413100000000     C     REPAUT        BEGSR
413200000000     C*
413300000000     C                   CLEAR                   AUTDS
413400000000     C                   Clear                   ConCivDs
413500000000     C*
413600000000     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
413700000000     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
413800000000     C                   Z-ADD     0             XCAGRP                         GRUPPO
413900000000     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
414000000000    >C                   MOVEL     'CONCIV'      XCAFNC                         FUNZIONE
414100000000     C                   MOVEL     *ZERO         XCAVFU
414200000000    >C                   MOVE      'CK'          XCATCK                         TIPO CTRL
414300000000     C                   MOVE      '0'           XCANAU                         RICH.XNOAUT
414400000000     C*
414500000000     C                   CALLB     'XCHKAUT'
414600000000     C                   PARM                    AUTDS
414700000000     C*
414800000000     C* errori/non abilitazione
414900000000     C     XCARTN        IFNE      0
415000000000     C*
415100000000     C                   ELSE
415200000000    >C                   MOVEL     XCADDS        CONCIVDS
415300000000     C                   ENDIF
415400000000     C*
415500000000     C                   ENDSR
415600000000     C/EJECT
415700000000     C************************************************************
415800000000     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
415900000000     C************************************************************
416000000000     C     REPDFT        BEGSR
416100000000     C*
416200000000     C* preparo i parametri di chiamata al driver
416300000000     C                   CLEAR                   XMPEDS
416400000000     C                   EVAL      TpChiamata = '01'
416500000000     C                   EVAL      NChiavi = 5
416600000000     C                   EVAL      VLogica = 2
416700000000     C*
416800000000     C* preparo le chiavi per la chiamata al driver
416900000000     C                   CLEAR                   XMPEDS1
417000000000     C                   MOVEL     XSCSOC        DSMPESOC
417100000000     C                   MOVEL     DSPGM         DSMPEPGM
417200000000     C                   MOVEL     KNMUS         DSMPEPRF
417300000000     C* Si reperiscono i dati di default a seconda delle modalità con
417400000000     C* cui il pgm è usato
417500000000     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
417600000000     C* MPETIP = 'PV'  pgm richiamato da menù
417700000000     C* MPETIP = 'PB'  x lancio Batch
417800000000     C                   SELECT
417900130828    >C     OPZG73        WHENEQ    '01'
418000000000     C                   MOVEL     'PX'          DSMPETIP
418100000000     C                   OTHER
418200000000     C                   MOVEL     'PV'          DSMPETIP
418300000000     C                   ENDSL
418400000000     C*
418500000000     C                   CALLB     'XMPE'
418600000000     C                   PARM                    XMPEDS
418700000000     C                   PARM                    XMPEDS1
418800000000     C*
418900000000     C     Risultato     IFEQ      '0'
419000000000     C                   MOVEL     DSMPEPAR      DSMPE
419100000000     C* nel caso sia la prima volta che l'utente apre questa anagrafica
419200000000     C* i suoi valori di default vengono decisi a pgm
419300000000     C                   ELSE
419400000000     C                   Z-ADD     1             $LOP
419500000000     C                   Z-ADD     1             $ORD
419600000000     C                   ENDIF
419700000000     C*
419800000000     C                   ENDSR
419900000000     C/EJECT
420000000000     C************************************************************
420100000000     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
420200000000     C************************************************************
420300000000     C     DEFOPZ        BEGSR
420400000000     C*
420500000000     C* Questa routine deve valorizzare la schiera $FL1
420600000000     C* con S o N a seconda della utilizzabilità o meno
420700000000     C* dell'opzione correlata
420800000000     C*
420900000000     C* Inizialmente si considerano tutte le opzioni come valide
421000000000     C     1             DO        $DIMOP        X
421100000000     C* Imposta tutti i flg delle opzioni non a *blank, a 'S'
421200000000     C                   If        $TX1(X)  <> *Blank
421300000000     C                   MOVE      'S'           $FL1(X)
421400000000     C                   Else
421500000000     C                   Leave
421600000000     C                   EndIf
421700000000     C                   ENDDO
421800000000     C*
421900000000     C* Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
422000000000     C* con 1 o 0 a seconda della utilizzabilità o meno
422100000000     C* del tasto funzione correlato
422200000000     C*
422300000000     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
422400000000     C* di compilazione
422500000000     C                   RESET                   $FC1
422600000000     C                   RESET                   $FR1
422700000000     C                   RESET                   $FC2
422800000000     C                   RESET                   $FR2
422900000000     C                   RESET                   $FC3
423000000000     C                   RESET                   $FR3
423100000000     C*
423200000000     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
423300000000     C* invalido alcune opzioni
423400000000     C                   SELECT
423500000000     C* Se pgm richiamato in ricerca disattivo :
423600130828    >C     OPZG73        WHENEQ    '01'
423700000000     C* F3=Fine
423800000000     C                   Z-ADD     3             X
423900000000     C                   MOVE      '0'           $FC1(X)
424000000000     C                   MOVE      '0'           $FC2(X)
424100000000     C                   MOVE      '0'           $FC3(X)
424200000000     C*
424300000000     C*
424400000000     C* Se pgm richiamato in interrogazione disattivo :
424500130828    >C     OPZG73        WHENEQ    '05'
424600000000     C* F10=Immissione
424700000000     C                   Z-ADD     10            X
424800000000     C                   MOVE      '0'           $FC1(X)
424900000000     C                   MOVE      '0'           $FC2(X)
425000000000     C* 1=Selezione
425100000000     C                   Z-ADD     1             X
425200000000     C     01            LOOKUP    $OP1(X)                                21
425300000000     C     *IN21         IFEQ      *ON
425400000000     C                   MOVE      'N'           $FL1(X)
425500000000     C                   ENDIF
425600000000     C* 2=Modifica
425700000000     C                   Z-ADD     1             X
425800000000     C     02            LOOKUP    $OP1(X)                                21
425900000000     C     *IN21         IFEQ      *ON
426000000000     C                   MOVE      'N'           $FL1(X)
426100000000     C                   ENDIF
426200000000     C* 3=Copia
426300000000     C                   Z-ADD     1             X
426400000000     C     03            LOOKUP    $OP1(X)                                21
426500000000     C     *IN21         IFEQ      *ON
426600000000     C                   MOVE      'N'           $FL1(X)
426700000000     C                   ENDIF
426800000000     C* 4=Annullamento
426900000000     C                   Z-ADD     1             X
427000000000     C     04            LOOKUP    $OP1(X)                                21
427100000000     C     *IN21         IFEQ      *ON
427200000000     C                   MOVE      'N'           $FL1(X)
427300000000     C                   ENDIF
427400000000     C*
427500130828    >C     OPZG73        WHENEQ    *BLANKS
427600000000     C* Se pgm non richiamato, disattivo subito :
427700000000     C* F8=Selezione seguente
427800000000     C                   Z-ADD     8             X
427900000000     C                   MOVE      '0'           $FC1(X)
428000000000     C                   MOVE      '0'           $FC2(X)
428100000000     C* 1=Selezione
428200000000     C                   Z-ADD     1             X
428300000000     C     01            LOOKUP    $OP1(X)                                21
428400000000     C     *IN21         IFEQ      *ON
428500000000     C                   MOVE      'N'           $FL1(X)
428600000000     C                   ENDIF
428700000000     C*
428800000000     C* In tutti gli altri casi, disattivo subito :
428900000000     C                   OTHER
429000000000     C* 1=Selezione
429100000000     C                   Z-ADD     1             X
429200000000     C     01            LOOKUP    $OP1(X)                                21
429300000000     C     *IN21         IFEQ      *ON
429400000000     C                   MOVE      'N'           $FL1(X)
429500000000     C                   ENDIF
429600000000     C*
429700000000     C                   ENDSL
429800160129R195 C* Se non sto elaborando la black list disattivo
429900160129 "   C* il comando F20= Applica soglia.
430000151203 "   C                   If        TpComG73    <> 'B'
430100151203 "   C                   Z-ADD     20            X
430200151203 "   C                   MOVE      '0'           $FC1(X)
430300151203 "   C                   MOVE      '0'           $FC2(X)
430400151203R195 C                   ENDIF
430500000000     C*
430600000000     C*
430700000000     C* poi, disattivo a seconda delle autorizzazioni :
430800000000     C                   IF        ImmAll <> *on
430900000000     C*
431000000000     C* F10=Immissione
431100000000     C                   Z-ADD     10            X
431200000000     C                   MOVE      '0'           $FC1(X)
431300000000     C                   MOVE      '0'           $FC2(X)
431400000000     C*
431500000000     C                   ENDIF
431600000000     C*
431700000000     C                   IF        ManAll <> *on
431800000000     C                   Z-ADD     1             X
431900000000     C     02            LOOKUP    $OP1(X)                                21
432000000000     C     *IN21         IFEQ      *ON
432100000000     C                   MOVE      'N'           $FL1(X)
432200000000     C                   ENDIF
432300000000     C                   ENDIF
432400000000     C*
432500000000     C                   IF        AnnAll <> *on
432600000000     C                   Z-ADD     1             X
432700000000     C     44            LOOKUP    $OP1(X)                                21
432800000000     C     *IN21         IFEQ      *ON
432900000000     C                   MOVE      'N'           $FL1(X)
433000000000     C                   ENDIF
433100000000     C                   ENDIF
433200000000     C*
433300000000     C                   IF        VisAll <> *on
433400000000     C*
433500000000     C                   Z-ADD     1             X
433600000000     C     05            LOOKUP    $OP1(X)                                21
433700000000     C     *IN21         IFEQ      *ON
433800000000     C                   MOVE      'N'           $FL1(X)
433900000000     C                   ENDIF
434000000000     C*
434100000000     C                   Z-ADD     1             X
434200000000     C     20            LOOKUP    $OP1(X)                                21
434300000000     C     *IN21         IFEQ      *ON
434400000000     C                   MOVE      'N'           $FL1(X)
434500000000     C                   ENDIF
434600000000     C*
434700000000     C                   ENDIF
434800000000     C*
434900000000     C*
435000000000     C                   ENDSR
435100000000     C/EJECT
435200000000     C************************************************************
435300000000     C* REPERIMENTO OPZIONI
435400000000     C************************************************************
435500000000     C     REPOPZ        BEGSR
435600000000     C*
435700000000     C     1             DO        $DIMOP        X
435800000000     C*
435900000000     C     $MS1(X)       IFNE      *BLANK
436000000000     C*
436100000000     C                   MOVEL     $MS1(X)       IDMSG
436200000000     C                   CALLB     'XRTVM'
436300000000     C                   PARM                    IDMSG            27
436400000000     C                   PARM                    TXTMSG          100
436500000000     C                   PARM                    ERRMSG            1
436600000000     C*
436700000000     C     ERRMSG        IFNE      *ON
436800000000     C                   MOVEL     TXTMSG        $TX1(X)
436900000000     C                   ELSE
437000000000     C                   MOVEL     *ALL'?'       $TX1(X)
437100000000     C                   ENDIF
437200000000     C*
437300000000     C                   ENDIF
437400000000     C*
437500000000     C                   ENDDO
437600000000     C*
437700000000     C                   ENDSR
437800000000     C/EJECT
437900000000     C************************************************************
438000000000     C* REPERIMENTO TASTI DI FUNZIONE
438100000000     C************************************************************
438200000000     C     REPKEY        BEGSR
438300000000     C                   MOVEA     $FM1          $FM
438400000000     C                   MOVEA     $FC1          $FC
438500000000     C                   EXSR      REPKKK
438600000000     C                   MOVEA     $FX           $FX1
438700000000     C*
438800000000     C                   MOVEA     $FM2          $FM
438900000000     C                   MOVEA     $FC2          $FC
439000000000     C                   EXSR      REPKKK
439100000000     C                   MOVEA     $FX           $FX2
439200000000     C*
439300000000     C                   MOVEA     $FM3          $FM
439400000000     C                   MOVEA     $FC3          $FC
439500000000     C                   EXSR      REPKKK
439600000000     C                   MOVEA     $FX           $FX3
439700000000     C                   ENDSR
439800000000     C/EJECT
439900000000     C************************************************************
440000000000     C* REPERIMENTO TASTI DI FUNZIONE
440100000000     C************************************************************
440200000000     C     REPKKK        BEGSR
440300000000     C*
440400000000     C                   CLEAR                   $FX
440500000000     C*
440600000000     C     1             DO        24            X
440700000000     C*
440800000000     C     $FM(X)        IFNE      *BLANK
440900000000     C     $Fc(X)        andne     '0'
441000000000     C*
441100000000     C                   MOVEL     $FM(X)        IDMSG
441200000000     C                   CALLB     'XRTVM'
441300000000     C                   PARM                    IDMSG            27
441400000000     C                   PARM                    TXTMSG          100
441500000000     C                   PARM                    ERRMSG            1
441600000000     C*
441700000000     C     ERRMSG        IFNE      *ON
441800000000     C                   MOVEL     TXTMSG        $FX(X)
441900000000     C                   ELSE
442000000000     C                   MOVEL     *ALL' '       $FX(X)
442100000000     C                   ENDIF
442200000000     C*
442300000000     C                   ENDIF
442400000000     C*
442500000000     C                   ENDDO
442600000000     C*
442700000000     C                   ENDSR
442800000000     C/EJECT
442900000000     C************************************************************
443000000000     C* DEFINIZIONE KLIST
443100000000     C************************************************************
443200000000     C     DefKlist      BEGSR
443300000000     C*
443400000000     C* klist
443500000000     C*
443600000000     C                   ENDSR
443700000000     C/EJECT
443800000000** $MS1 / $TX1
443900000000PROMSG    *LIBL     OPZ0001
444000000000SDGMSG    *LIBL     SDO0021
444100130828PROMSG    *LIBL     OPZ0005
444200000000PROMSG    *LIBL     OPZ0285
444300000000SDGMSG    *LIBL     SDO0030
444400000000SDGMSG    *LIBL     SDO0024
444500000000** $OP1 / $FL1
44460000000001S      <Selezione
44470000000002S      <Dati anagrafica
44480013082805S      <Visualizzazione
44490000000011S      <Dettaglio
44500000000012S      <Quadri
44510000000044S      <Annulla/Ripristina
445200000000** TASTI DI FUNZIONE  D1 : $FM1 / $FX1
445300000000PROMSG    *LIBL     KEY0001  01
445400000000PROMSG    *LIBL     KEY0002  02
445500000000PROMSG    *LIBL     KEY0003  03
445600000000PROMSG    *LIBL     KEY0004  04
445700000000PROMSG    *LIBL     KEY0005  05
445800000000PROMSG    *LIBL     KEY0006  06
445900000000PROMSG    *LIBL     KEY0007  07
446000000000PROMSG    *LIBL     KEY0008  08
446100000000PROMSG    *LIBL     KEY0009  09
446200000000PROMSG    *LIBL     KEY0010  10
446300151203PROMSG    *LIBL     KEY0011  11
446400000000PROMSG    *LIBL     KEY0012  12
446500000000PROMSG    *LIBL     KEY0013  13
446600000000PROMSG    *LIBL     KEY0014  14
446700000000SDGMSG    *LIBL     SDK0003  15
446800000000PROMSG    *LIBL     KEY0016  16
446900000000SDGMSG    *LIBL     SDK0001  17
447000000000PROMSG    *LIBL     KEY0018  18
447100000000PROMSG    *LIBL     KEY0019  19
447200151203PROMSG    *LIBL     KEY0020  20
447300000000PROMSG    *LIBL     KEY0021  21
447400000000PROMSG    *LIBL     KEY0022  22
447500000000PROMSG    *LIBL     KEY0023  23
447600000000PROMSG    *LIBL     KEY0024  24
447700000000**  ABILITAZIONE D1 : $FC1 / $FR1
4478000000000N  01
4479000000001N  02
4480000000001N  03
4481000000001N  04
4482000000000N  05
4483000000000N  06
4484000000000N  07
4485000000001N  08
4486000000000N  09
4487000000000N  10
4488001512030N  11
4489000000001N  12
4490000000000N  13
4491000000000N  14
4492000000000N  15
4493000000000N  16
4494000000000N  17
4495000000000N  18
4496000000000N  19
4497000000000N  20
4498000000000N  21
4499000000000N  22
4500000000000N  23   Impostare a '1' se si utilizzano opzioni
4501000000001N  24   Lasciare sempre impostata a '1' il valore della riga 24
450200000000** TASTI DI FUNZIONE  S1 : $FM2 / $FX2
450300000000PROMSG    *LIBL     KEY0001  01
450400000000PROMSG    *LIBL     KEY0002  02
450500000000PROMSG    *LIBL     KEY0003  03
450600000000PROMSG    *LIBL     KEY0004  04
450700110823SDGMSG    *LIBL     SDK0013  05
450800000000PROMSG    *LIBL     KEY0006  06
450900000000PROMSG    *LIBL     KEY0007  07
451000000000PROMSG    *LIBL     KEY0008  08
451100000000PROMSG    *LIBL     KEY0009  09
451200000000PROMSG    *LIBL     KEY0010  10
451300110826SDGMSG    *LIBL     SDK0014  11
451400000000PROMSG    *LIBL     KEY0012  12
451500000000PROMSG    *LIBL     KEY0013  13
451600000000PROMSG    *LIBL     KEY0014  14
451700000000PROMSG    *LIBL     KEY0015  15
451800000000PROMSG    *LIBL     KEY0016  16
451900000000SDGMSG    *LIBL     SDK0001  17
452000000000PROMSG    *LIBL     KEY0018  18
452100000000PROMSG    *LIBL     KEY0019  19
452200151203SDGMSG    *LIBL     SDK0021  20                   *R195*
452300000000PROMSG    *LIBL     KEY0021  21
452400000000PROMSG    *LIBL     KEY0022  22
452500000000PROMSG    *LIBL     KEY0023  23
452600000000PROMSG    *LIBL     KEY0024  24
452700000000**  ABILITAZIONE S1 : $FC2 / $FR2
4528000000000N  01
4529000000000N  02
4530000000001N  03
4531000000000N  04
4532001309120N  05
4533000000000N  06
4534000000000N  07
4535000000001N  08
4536000000000N  09
4537000000001N  10
4538001309110N  11
4539000000001N  12
4540000000000N  13
4541000000000N  14
4542000000000N  15
4543000000000N  16
4544000000001N  17
4545000000000N  18
4546000000000N  19
4547001512031N  20                                           *R195*
4548000000000N  21
4549000000000N  22
4550000000001N  23   Impostare a '1' se si utilizzano opzioni
4551000000001N  24   Lasciare sempre impostata a '1' il valore della riga 24
455200000000** TASTI DI FUNZIONE  D3 : $FM3 / $FX3
455300000000PROMSG    *LIBL     KEY0001  01
455400000000PROMSG    *LIBL     KEY0002  02
455500000000PROMSG    *LIBL     KEY0003  03
455600000000PROMSG    *LIBL     KEY0004  04
455700000000PROMSG    *LIBL     KEY0005  05
455800000000PROMSG    *LIBL     KEY0006  06
455900000000PROMSG    *LIBL     KEY0007  07
456000000000PROMSG    *LIBL     KEY0008  08
456100000000PROMSG    *LIBL     KEY0009  09
456200000000PROMSG    *LIBL     KEY0010  10
456300000000PROMSG    *LIBL     KEY0011  11
456400000000PROMSG    *LIBL     KEY0012  12
456500000000PROMSG    *LIBL     KEY0013  13
456600000000PROMSG    *LIBL     KEY0014  14
456700000000PROMSG    *LIBL     KEY0015  15
456800000000PROMSG    *LIBL     KEY0016  16
456900000000SDGMSG    *LIBL     SDK0001  17
457000000000PROMSG    *LIBL     KEY0018  18
457100000000PROMSG    *LIBL     KEY0019  19
457200000000PROMSG    *LIBL     KEY0020  20
457300000000PROMSG    *LIBL     KEY0021  21
457400000000PROMSG    *LIBL     KEY0022  22
457500000000PROMSG    *LIBL     KEY0023  23
457600000000PROMSG    *LIBL     KEY0024  24
457700000000**  ABILITAZIONE D3 : $FC3 / $FR3
4578000000000N  01
4579000000000N  02
4580000000001N  03
4581000000000N  04
4582000000000N  05
4583000000000N  06
4584000000000N  07
4585000000000N  08
4586000000000N  09
4587000000000N  10
4588000000000N  11
4589000000001N  12
4590000000000N  13
4591000000000N  14
4592000000000N  15
4593000000000N  16
4594000000000N  17
4595000000000N  18
4596000000000N  19
4597000000000N  20
4598000000000N  21
4599000000000N  22
4600000000000N  23   Impostare a '1' se si utilizzano opzioni
4601000000001N  24   Lasciare sempre impostata a '1' il valore della riga 24
