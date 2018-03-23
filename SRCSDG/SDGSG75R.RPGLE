000100130906     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND SDG005)
000200131008R134 H*PARMS CVTOPT(*NONE) ACTGRP(DETT)
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400000000      *
000500000000      *           -----------------------------
000600130906      *                Spesometro clienti/fornitori
000700130909      *                dati di dettaglio - guida
000800000000      *           -----------------------------
000900000000      *
001000131008R134 F* Cambiato actgrp QILE con DETT
001100131008R134 X*****PARMS CVTOPT(*NONE) ACTGRP(QILE)
001200000000     F*----------------------------------------------------------------
001300130906     F* Spesometro - Movimenti estratti
001400130906     F* Key: TpCom/Anno/Mese/Sys/NrAsReg/NrRigaM/NrRig   UNIQUE
001500130906     FSDGSCF11L UF   E           K DISK
001600000000     F*-------------
001700110825     F* Wrkfile
001800130906     FSDGSCF00W IF A E           k DISK
001900130906     F                                     RENAME(SDGSCF000:SDGSCF0W)
002000110825     F*------------
002100130906     FSDGSG75D  CF   E             WORKSTN
002200130906     F                                     RENAME(SG75D1:FMTD1)
002300130906     F                                     RENAME(SG75D2:FMTD2)
002400130906     F                                     RENAME(SG75D3:FMTD3)
002500130906     F                                     RENAME(SG75S1:FMTS1)
002600130906     F                                     RENAME(SG75C1:FMTC1)
002700130906     F                                     RENAME(SG75T1:FMTT1)
002800130906     F                                     RENAME(SG75Z1:FMTZ1)
002900130906     F                                     RENAME(SG75Z2:FMTZ2)
003000130906     F                                     RENAME(SG75Z3:FMTZ3)
003100000000     F                                     SFILE(FMTS1:S1NRR)
003200000000     F                                     INFDS(DSFMT)
003300000000     D*----------------------------------------------------*
003400000000     D* Schiere per la gestione delle opzioni
003500000000     D*
003600000000     D* dimensione della schiera $MS1, da non cambiare
003700000000     D $DIMOP          C                   CONST(30)
003800000000     D $MS1            S             27    DIM($dimop) CTDATA PERRCD(1)
003900000000     D $TX1            S             50    DIM($dimop) ALT($MS1)
004000000000     D $OP1            S              2  0 DIM($dimop) CTDATA PERRCD(1)
004100000000     D $FL1            S              1    DIM($dimop) ALT($OP1)
004200000000     D* schiera contenente i nomi dei programma richiamati attraverso
004300000000     D* le opzioni personalizzate
004400000000     D $PGO1           S             10    DIM($dimop)
004500000000     D $TX             S             50    DIM(50)
004600000000     D $FL             S              1    DIM(50)
004700000000     D* Schiere generali per la gestione dei tasti di funzione
004800000000     D $FM             S             27    DIM(24)
004900000000     D $FX             S             50    DIM(24)
005000000000     D $FC             S              1    DIM(24)
005100000000     D $FR             S              1    DIM(24)
005200000000     D* Schiere per la gestione dei tasti di funzione
005300000000     D* Video D1
005400000000     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
005500000000     D $FX1            S             50    DIM(24) ALT($FM1)
005600000000     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
005700000000     D $FR1            S              1    DIM(24) ALT($FC1)
005800000000     D* Schiera contente i nomi dei programmi da richiamare con i tasti
005900000000     D* funzione personalizzata S1
006000000000     D $PGF2           S             10    DIM(25)
006100000000     D* Schiere per la gestione dei tasti di funzione
006200000000     D* Video S1
006300000000     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
006400000000     D $FX2            S             50    DIM(24) ALT($FM2)
006500000000     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
006600000000     D $FR2            S              1    DIM(24) ALT($FC2)
006700000000     D* Schiere per la gestione dei tasti di funzione
006800000000     D* Video D2
006900000000     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
007000000000     D $FX3            S             50    DIM(24) ALT($FM3)
007100000000     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
007200000000     D $FR3            S              1    DIM(24) ALT($FC3)
007300000000     D*
007400000000     D $D1L01          S              3  0
007500000000     D $D1L02          S              3  0
007600000000     D $S1L01          S              3  0
007700000000     D $S1L02          S              3  0
007800000000     D $C1Op1          S              3  0
007900000000     D $C1Op2          S              3  0
008000000000     D $D2L01          S              3  0
008100000000     D $D2L02          S              3  0
008200000000     D*
008300000000     D*----------------------------------------------------*
008400000000     D* Valori assunti
008500000000     D XMPEDS        E DS                  INZ
008600000000     D XMPEDS1       E DS                  INZ
008700000000     D DSMPE           DS                  INZ
008800000000     D  $LOP                   1      1  0
008900000000     D  $ORD                   2      3  0
009000000000     D*-------------
009100000000     D* Passaggio Parametri
009200000000     D KPJBA         E DS
009300000000     D*-------------
009400000000     D* Reperimento Autorizzazioni
009500000000     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
009600000000     D*-------------
009700000000     D* Dati per pgm XNOAUT
009800000000     D XNOADS        E DS                  EXTNAME(XNOAUTDS)
009900000000     D*-------------
010000000000     D* Passaggio Autorizzazioni
010100000000     D DSAUT           DS           512
010200000000     D*-------------
010300000000     D* Maschera dati autorizzazioni
010400000000     D CONCIVDS      E DS
010500000000     D*-------------
010600000000     D* Dati di ambiente ottenuti da SDGSOCDR
010700000000     D SOC001        E DS                  EXTNAME(SDGSOCDS )
010800000000     D*-------------
010900000000     D* DS Interna per dati di output di SDGSOCDR
011000000000     D XSOCDS          DS          1000
011100000000     D*-------------
011200000000     D* DS Esterna gestione messaggi
011300000000     D SDGMSGDS      E DS
011400000000     D  STS          E                     EXTFLD(MSGSTS)
011500000000     D                                     DIM(333)
011600000000     D  JBA          E                     EXTFLD(MSGJBA)
011700000000     D                                     DIM(502)
011800000000     D*-------------
011900000000     D* campi di lavoro per emissione window di avvertimento
012000000000     D WXMSGInz        S              1A   INZ('1')
012100000000     D*-------------
012200000000     D* Richiamo a SDGTABDR
012300000000     D SDGTABDS      E DS                  inz
012400000000     D*-------------
012500000000     D* Parametri per ricerca codice ANATB00F
012600000000     D XTABDS        E DS                  INZ
012700000000     D*-------------
012800000000     D* Ricerca codici Stati in Black List
012900000000     D SDGHD1DS      E DS
013000000000     D*-------------
013100000000     D* Ricerca legende
013200000000     D ANA964DS      E DS                  INZ
013300000000     D*-------------
013400000000     D*Controlli validità per valore di una legenda
013500000000     D X07VALDS      E DS                  Inz
013600000000     D*-------------
013700000000     D* Reperimento parametri
013800000000     D XPaOut          DS          2000
013900000000     D*-------------
014000000000     D* Parametri in input
014100130906    >D SDGSG75DS     E DS
014200130906     D*-------------
014300130906     D* Parametri del PGM di lettura movimenti estratti
014400130906     D* Parametri di INPUT
014500130906     D SDG00502DS    E DS                  INZ
014600130906     D  RIC0502      E                     INZ('0')
014700130906     D*----------
014800130906     D* Parametri di OUTPUT
014900130906     D* per manutenzione di testata, quadri e dettaglio
015000130906     D SDG00592DS    E DS                  INZ
015100130906     D*--------------------
015200130906     D* Parametri di OUTPUT
015300130906     D* per lista quadri a video e creazione tracciato ministeriale
015400130906     D*---------
015500130906     D*---------
015600130906     D* formato AGGREGATO
015700130906     D*---
015800130906     D* SOGGETTI NAZIONALI: FATTURE, NOTE E DOCUMENTI RIEPILOGATIVI
015900130906     D*                     (NOLEGGIO/LEASING)
016000130906     D SDG005FADS    E DS                  INZ
016100130906     D*
016200130906     D*---
016300130906     D* SOGGETTI NAZIONALI: OPERAZIONI SENZA FATTURA
016400130906     D SDG005SADS    E DS                  INZ
016500130906     D*
016600130906     D*---
016700130906     D* SOGGETTI NON RESIDENTI E ACQUISTI DI SERVIZI DA NON RESIDENTI
016800130906     D SDG005BLDS    E DS                  INZ
016900130906     D*---------
017000130906     D*---------
017100130906     D* formato DETTAGLIATO
017200130906     D*---
017300130906     D* SOGGETTI NAZIONALI: FATTURE E DOCUMENTI RIEPILOGATIVI EMESSI
017400130906     D*                     (NOLEGGIO/LEASING)
017500130906     D SDG005FEDS    E DS                  INZ
017600130906     D*---
017700130906     D* SOGGETTI NAZIONALI: FATTURE E DOCUMENTI RIEPILOGATIVI RICEVUTI
017800130906     D SDG005FRDS    E DS                  INZ
017900130906     D*---
018000130906     D* SOGGETTI NAZIONALI: NOTE EMESSE
018100130906     D SDG005NEDS    E DS                  INZ
018200130906     D*---
018300130906     D* SOGGETTI NAZIONALI: NOTE RICEVUTE
018400130906     D SDG005NRDS    E DS                  INZ
018500130906     D*---
018600130906     D* SOGGETTI NAZIONALI: OPERAZIONI SENZA FATTURA
018700130906     D*                     (NOLEGGIO/LEASING)
018800130906     D SDG005DFDS    E DS                  INZ
018900130906     D*---
019000130906     D* SOGGETTI NON RESIDENTI
019100130906     D*                     (NOLEGGIO/LEASING)
019200130906     D SDG005FNDS    E DS                  INZ
019300130906     D*---
019400130906     D* ACQUISTI DI SERVIZI DA NON RESIDENTI
019500130906     D SDG005SEDS    E DS                  INZ
019600131017R136 D*---
019700131017 "   D* SOGGETTI NON RESIDENTI: OPERAZIONI SENZA FATTURA
019800131017 "   D* LEGATE AL TURISMO
019900131017R136 D SDG005TUDS    E DS
020000130906     D*---
020100130906     D* RIEPILOGO
020200130906     D SDG005TADS    E DS                  INZ
020300000000     D*-------------
020400000000     D* Parametri del PGM di manutenzione  dati di dettaglio
020500130906     D SDGSG76DS     E DS                  inz
020600130906     D  RETG76       E                     INZ('0')
020700130906     D  OPRG76       E                     INZ('0')
020800130906     D  ERRG76       E                     INZ('0')
020900130906     D  CMTG76       E                     INZ('0')
021000000000     D*-------------
021100000000     D* Interrogazione conti
021200000000     D ANA399DS      E DS                  Inz
021300000000     D*
021400000000     D WA39Iniz        S                   Like(A39Iniz)
021500000000     D*-------------
021600000000     D* Paramentri richiamo pnota
021700000000     D NDC001DS      E DS                  INZ
021800000000     D  RET001       E                     INZ('0')
021900000000     D  OPR001       E                     INZ('0')
022000000000     D  ERR001       E                     INZ('0')
022100000000     D*-------------
022200000000     D* campo di salvataggio per la DS d'ingresso durante la chiamata
022300000000     D* alla funzione personalizzata
022400130906    >D $SaveDS         S                   like(SDGSG75DS)
022500000000     D*-------------
022600000000     D DSFMT           DS           512
022700000000     D  $TASTO               369    369
022800000000     D  NRG                  370    370
022900000000     D  NCL                  371    371
023000000000      * riga cursore window
023100000000     D  NRiWin               382    382
023200000000      * colonna cursore window
023300000000     D  NCoWin               383    383
023400000000     D  SFLNRR               378    379B 0
023500000000     D*-------------
023600000000     D* posizione cursore
023700000000     D CURSOR          DS
023800000000     D  RIRI                   1      2B 0 INZ
023900000000     D  R$$                    2      2
024000000000     D  COCO                   3      4B 0 INZ
024100000000     D  C$$                    4      4
024200000000     D*-------------
024300000000     D* Costanti
024400000000     D* nr.di righe del sfl (SFLPAG)
024500000000    >D SFLPAG          C                   CONST(9999)
024600000000     D*-------------
024700000000     D* Tasti di funzione
024800000000     D F01             C                   CONST(X'31')
024900000000     D F02             C                   CONST(X'32')
025000000000     D F03             C                   CONST(X'33')
025100000000     D F04             C                   CONST(X'34')
025200000000     D F05             C                   CONST(X'35')
025300000000     D F06             C                   CONST(X'36')
025400000000     D F07             C                   CONST(X'37')
025500000000     D F08             C                   CONST(X'38')
025600000000     D F09             C                   CONST(X'39')
025700000000     D F10             C                   CONST(X'3A')
025800000000     D F11             C                   CONST(X'3B')
025900000000     D F12             C                   CONST(X'3C')
026000000000     D F13             C                   CONST(X'B1')
026100000000     D F14             C                   CONST(X'B2')
026200000000     D F15             C                   CONST(X'B3')
026300000000     D F16             C                   CONST(X'B4')
026400000000     D F17             C                   CONST(X'B5')
026500000000     D F18             C                   CONST(X'B6')
026600000000     D F19             C                   CONST(X'B7')
026700000000     D F20             C                   CONST(X'B8')
026800000000     D F21             C                   CONST(X'B9')
026900000000     D F22             C                   CONST(X'BA')
027000000000     D F23             C                   CONST(X'BB')
027100000000     D F24             C                   CONST(X'BC')
027200000000     D ENTER           C                   CONST(X'F1')
027300000000     D ROLDWN          C                   CONST(X'F4')
027400000000     D ROLLUP          C                   CONST(X'F5')
027500000000     D*-------------
027600000000     D* Attributi video
027700000000     D Normale         C                   Const(X'32')
027800000000     D Protetto        C                   Const(X'A0')
027900000000     D Annul           C                   CONST(X'A1')
028000000000     D*-------------
028100000000     D* Sottotitolo di testata
028200000000     D SOTTIT          C                   CONST('PROMSG    *LIBL-
028300000000     D                                          COS0648')
028400000000     D SOTTITD1        C                   CONST('SDGMSG    *LIBL-
028500000000     D                                          SDC0001')
028501151201R195 D*-------------
028502151201  "  D* Sottotitolo di testata
028503151201  "  D* "Dettaglio Spesometro"
028504151201  "  D SOTTITT1        C                   CONST('SDGMSG    *LIBL-
028505151201  "  D                                          SDC0096')
028506151201  "  D* Sottotitolo di testata Black List
028507151201  "  D* "Dettaglio Aquisti da RSM"
028508151201  "  D SOTTITT2        C                   CONST('SDGMSG    *LIBL-
028509151201  "  D                                          SDC0097')
028510151201  "  D* Sottotitolo di testata Black List
028511151201  "  D* "Soggetti Black List"
028512151201  "  D* "Dettaglio Black List"
028513151201  "  D SOTTITT3        C                   CONST('SDGMSG    *LIBL-
028514151201R195 D                                          SDC0098')
028600000000     D*-------------
028700000000     D* Variabili per gestione videate
028800000000     D $Gest           S              2A
028900000000     D $Fine           S              1A
029000000000     D $InzD1          S              1A
029100000000     D $InzS1          S              1A
029200000000     D $InzD2          S              1A
029300000000     D $LastG          S              2A
029400000000     D $LastV          S              2A
029500000000     D $EndFile        S              1A
029600000000     D $Esci           S              1A
029700000000     D $RcdOK          S              1A
029800000000     D $Unique         S              1A
029900000000     D*-------------
030000000000     D* Variabili appoggio sempre presenti in tutte le anagrafiche
030100000000     D S1Nrr           S                   like(C1Rcd)
030200000000     D WSfl            S                   like(C1Nrr)
030300000000     D WMax            S                   like(C1Rcd)
030400000000     D WPag            S                   like(C1Rcd)
030500000000     D WInzS1          S                   like($InzS1)
030600130906     D WOpr            S                   like(OprG75)
030700000000     D CurR            S              3S 0
030800000000     D CurC            S              3S 0
030900000000     D Win             S             99A
031000000000     D W1Pos           S                   like(C1Pos)
031100000000     D WINKR           S              1A
031200000000     D WExf            S              1A
031300000000     D WSogg           S                   Like(DVSSogg)
031400000000     D*-------------
031500000000     D* Indice della schiera tasti funzione
031600000000     D PosFun          S              2  0
031700000000     D*-------------
031800000000     D*  Indici
031900000000     D X               S              5S 0
032000000000     D Y               S              5S 0
032100000000     D* Indice della schiera per le opzioni personalizzate
032200000000     D PosOpz          S              2  0
032300000000     D* numero max opzioni
032400000000     D NumOpz          S              2  0 Inz($DIMOP)
032500000000     D* numero effettive opzioni
032600000000     D OpzLun          S              2  0
032700000000     D*-------------
032800000000     D* ricerca soggetti
032900000000     D ANA040DS      E DS                  INZ
033000000000     D*-------------
033100000000     D* Campi di lavoro
033200111214R078 D UDateISO        S               D
033300130906     D WKcc            S                   Like(SCFKcc )
033400130906     D WKsc            S                   Like(SCFKsc )
033500110824     D DaSoggetto      S              1A
033600000000     D*-------------
033700000000     D* Reperimento nome PGM
033800000000     D STATUS         SDS           333
033900000000     D  DSPGM            *PROC
034000000000     D  PARMS            *PARMS
034100000000     D*-------------
034200000000     D* DS esterna per dati soggetti
034300000000     D DVASOG        E DS                  INZ
034400000000     D*-------------
034500000000     D* Definizioni parametri driver anagrafici
034600000000     D Dvaric          S              1
034700000000     D DvaSocieta      S              3
034800000000     D DvaUnita        S              8
034900000000     D DvaStrutt       S             10
035000000000     D DvaDtRif        S               D
035100000000     D DvaLenOut       S              9B 0
035200000000     D DvaSnatura      S              1
035300000000     D DvaCodice       S              8
035400000000     D DvaLineav       S              3
035500000000     D DvaFiliale      S              3  0
035600000000     D DvaSogg         S              8
035700000000     D DvaTpInd        S              2
035800000000     D DvaCdInd        S              3
035900000000     D DvaErrore       S              1
036000000000     D DvaOutput       S           4000
036100000000     C*----------------------------------------------------*
036200000000     C*                MAIN LINE PROGRAM
036300000000     C*----------------------------------------------------*
036400000000     C*
036500000000     C* inizializzazione variabili
036600000000     C                   EXSR      INZVAR
036700000000     C*
036800000000     C     $FINE         DOWEQ     *OFF
036900000000     C     $GEST         CASEQ     'D1'          GESD1                          Parzializz.
037000000000     C     $GEST         CASEQ     'S1'          GESS1                          Lista
037100000000     C     $GEST         CASEQ     'D2'          GESD2                          Valori ass.
037200000000     C                   ENDCS
037300000000     C                   ENDDO
037400000000     C* fine programma
037500000000     C                   EXSR      ENDPGM
037600000000     C*
037700000000     C/EJECT
037800000000     C************************************************************
037900000000     C* FINE PROGRAMMA
038000000000     C************************************************************
038100000000     C     ENDPGM        BEGSR
038200000000     C*
038300130906     C* cancello in QTEMP il file di lavoro SDGSCF00W
038400110825     C* con i dettagli selezionati precedentemente
038500130906     C                   CLOSE     SDGSCF00W
038600130906     C                   EXSR      DelSCF00W
038700110825     C*
038800000000     C* passaggio dati al pgm chiamante
038900130906    >C                   MOVEL     WOPR          OPRG75                                     ne S1
039000000000     C*
039100000000     C* Se pgm Guida non è stato richiamato posso chiudere in LR
039200130906    >C     OPZG75        IFEQ      *BLANK
039300000000     C                   SETON                                            LR
039400000000     C* altrimenti chiudo in RT
039500000000     C                   ELSE
039600000000     C                   SETON                                            RT
039700000000     C                   ENDIF
039800000000     C                   RETURN
039900000000     C*
040000000000     C                   ENDSR
040100000000     C/EJECT
040200000000     C************************************************************
040300000000     C* RICERCHE
040400000000     C************************************************************
040500000000     C     SEARCH        BEGSR
040600000000     C*
040700000000     C* determino Riga/Colonna del cursore
040800000000     C                   MOVE      NRG           R$$
040900000000     C                   MOVE      NCL           C$$
041000000000     C                   Z-ADD     RIRI          CURR
041100000000     C                   Z-ADD     COCO          CURC
041200000000     C*
041300000000     C                   EXSR      CallAllinea
041400000000     C*
041500000000   B1C                   SELECT
041600000000     C*
041700000000     C*  Stato
041800000000     C     H1NMFL        WHENEQ    'D1STATO'
041900000000     C                   EXSR      F4_Stato
042000000000     C*
042100000000     C*  Codice IVA
042200000000     C     H1NMFL        WHENEQ    'D1PARTIVA'
042300000000     C                   EXSR      F4_PartIva
042400000000     C*
042500000000     C*  Codice fiscale
042600000000     C     H1NMFL        WHENEQ    'D1CDFISC'
042700000000     C                   EXSR      F4_CdFisc
042800000000     C*
042900000000     C*  Soggetto
043000000000     C     H1NMFL        WHENEQ    'D1SOGG'
043100000000     C                   EXSR      F4_Sogg
043200000000     C*
043300000000     C* FMTD1 - Dati attivi/non attivi
043400000000     C     H1NMFL        WHENEQ    'D1ANN  '
043500000000     C                   EXSR      F4_Ann
043600000000     C*
043700000000     C* FMTD1 - Tipo annullamento
043800000000     C     H1NMFL        WHENEQ    'D1TPANN'
043900000000     C                   EXSR      F4_TpAnn
044000130906     C*
044100130906     C* FMTD1 - Quadro di presentazione
044200130906     C     H1NMFL        WHENEQ    'D1QUADRO'
044300130906     C                   EXSR      F4_Quadro
044400130919     C*
044500130919     C* FMTD1 - tipo operazione
044600130919     C     H1NMFL        WHENEQ    'D1TPOPE '
044700130919     C                   EXSR      F4_TpOpe
044800000000     C*
044900000000     C* ricerca non abilitata in questa posizione
045000000000   X1C                   OTHER
045100000000     C                   SETON                                        9899
045200000000     C*
045300000000   E1C                   ENDSL
045400000000     C*
045500000000     C* imposto pos. del cursore
045600000000     C                   Z-ADD     CURR          H1RIGA
045700000000     C                   Z-ADD     CURC          H1COLO
045800000000     C*
045900000000     C                   MOVE      *HIVAL        $LASTV
046000000000     C*
046100000000     C                   ENDSR
046200000000     C/EJECT
046300000000     C************************************************************
046400000000     C* Richiamo programma per allineare i codici
046500000000     C************************************************************
046600000000     C     CallAllinea   BEGSR
046700000000     C*
046800000000     C     D1SOGG        IFNE      *BLANK
046900000000     C                   CALLb     'XALLINEA'
047000000000     C                   PARM                    D1SOGG
047100000000     C                   PARM      8             LEN               5 0
047200000000     C                   PARM                    XSCINP
047300000000     C                   ENDIF
047400000000     C*
047500000000     C                   ENDSR
047600000000      /EJECT
047700000000     C************************************************************
047800000000     C* Ricerca codice Stato del conto
047900000000     C************************************************************
048000000000     C     F4_Stato      BEGSR
048100000000     C*
048200000000     C                   Reset                   XTabDS
048300000000     C*
048400000000     C                   Move      '01'          XTaOpz
048500000000     C                   Move      XScSoc        XTaAzi
048600000000     C                   Move      XScLin        XTaLin
048700000000     C                   Z-add     Curr          XTaRig
048800000000     C                   Z-add     Curc          XTaCol
048900000000     C                   Move      *Blank        XTaKey
049000000000     C*
049100000000     C                   Clear                   KPJBU
049200000000     C                   MoveL     XTabDS        KPJBU
049300000000     C*
049400000000     C                   CALL      'SDGHD1R'
049500000000     C                   PARM                    KPJBA
049600000000     C                   MoveL     KPJBU         XTabDS
049700000000     C*
049800000000     C                   if        XTaErr     <> '1'  and
049900000000     C                             XTaKey     <> *Blank
050000000000     C                   Move      XTaKey        D1Stato
050100000000     C                   endif
050200000000     C*
050300000000     C                   ENDSR
050400000000      /EJECT
050500000000     C************************************************************
050600000000     C* Ricerca codice IVA
050700000000     C************************************************************
050800000000     C     F4_PartIva    BEGSR
050900000000     C*
051000000000     C                   RESET                   ANA040DS
051100000000     C*
051200000000     C                   MOVEL     '01'          OPZ040
051300000000     C                   MoveL     D1PartIva     IVA040
051400000000     C                   MOVEL     ANA040DS      KPJBU
051500000000     C                   CALL      'ANA040R'
051600000000     C                   PARM                    KPJBA
051700000000     C*
051800000000     C                   MOVEL     KPJBU         ANA040DS
051900000000     C*
052000000000     C                   if        Ret040     <> '2'
052100000000     C*
052200000000     C                   MoveL     Cod040        WSogg
052300000000     C                   EXSR      CallDvaSog
052400000000     C*
052500000000     C                   if        DVAErrore  <> '1'
052600000000     C*
052700000000     C                   Eval      %subst(DVASOG:1:DVALENOUT)
052800000000     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
052900000000     C*
053000000000     C                   MoveL     DVSPartIVA    D1PartIva
053100000000     C*
053200000000     C                   endif
053300000000     C*
053400000000     C                   endif
053500000000     C*
053600000000     C                   ENDSR
053700000000      /EJECT
053800000000     C************************************************************
053900000000     C* Ricerca codice fiscale
054000000000     C************************************************************
054100000000     C     F4_CdFisc     BEGSR
054200000000     C*
054300000000     C                   RESET                   ANA040DS
054400000000     C*
054500000000     C                   MOVEL     '01'          OPZ040
054600000000     C                   MoveL     D1CdFisc      CdF040
054700000000     C                   MOVEL     ANA040DS      KPJBU
054800000000     C                   CALL      'ANA040R'
054900000000     C                   PARM                    KPJBA
055000000000     C*
055100000000     C                   MOVEL     KPJBU         ANA040DS
055200000000     C*
055300000000     C                   if        Ret040     <> '2'
055400000000     C*
055500000000     C                   MoveL     Cod040        WSogg
055600000000     C                   EXSR      CallDvaSog
055700000000     C*
055800000000     C                   if        DVAErrore  <> '1'
055900000000     C*
056000000000     C                   Eval      %subst(DVASOG:1:DVALENOUT)
056100000000     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
056200000000     C*
056300000000     C                   MoveL     DVSCdFisc     D1CdFisc                       *
056400000000     C*
056500000000     C                   endif
056600000000     C*
056700000000     C                   endif
056800000000     C*
056900000000     C                   ENDSR
057000000000     C/EJECT
057100000000     C************************************************************
057200000000     C* Ricerca codice soggetto
057300000000     C************************************************************
057400000000     C     F4_Sogg       BEGSR
057500000000     C*
057600000000     C                   RESET                   ANA040DS
057700000000     C*
057800000000     C                   MOVEL     '01'          OPZ040
057900000000     C                   MoveL     D1Sogg        Cod040
058000000000     C                   MOVEL     ANA040DS      KPJBU
058100000000     C                   CALL      'ANA040R'
058200000000     C                   PARM                    KPJBA
058300000000     C*
058400000000     C                   MOVEL     KPJBU         ANA040DS
058500000000     C*
058600000000     C                   if        Ret040     <> '2'
058700000000     C                   MoveL     Cod040        D1Sogg
058800000000     C                   endif
058900000000     C*
059000000000     C                   ENDSR
059100000000     C/EJECT
059200000000     C************************************************************
059300000000     C* Ricerca dati attivi/non attivi
059400000000     C************************************************************
059500000000     C     F4_Ann        BEGSR
059600000000     C*
059700000000     C                   reset                   ana964ds
059800000000     C*
059900000000     C                   movel     '01'          Opz964
060000000000     C                   movel     'ANDIZ000 '   Trc964
060100000000     C                   movel     'DSPATT   '   Cam964
060200000000     C*
060300000000     C                   movel     ana964ds      Kpjbu
060400000000     C                   call      'ANA964R'
060500000000     C                   parm                    Kpjba
060600000000     C                   movel     Kpjbu         Ana964ds
060700000000     C*
060800000000     C                   if        Ret964     <> '2'
060900000000     C                   movel     Val964        D1Ann
061000000000     C                   endif
061100000000     C*
061200000000     C                   ENDSR
061300000000     C/EJECT
061400000000     C************************************************************
061500000000     C* Ricerca tipo annullamento
061600000000     C************************************************************
061700000000     C     F4_TpAnn      BEGSR
061800000000     C*
061900000000     C                   reset                   ana964ds
062000000000     C*
062100000000     C                   movel     '01'          Opz964
062200130906     C                   movel     'SDGSCF000'   Trc964
062300130906     C                   movel     'SCFTPANN'    Cam964
062400000000     C*
062500000000     C                   movel     ana964ds      Kpjbu
062600000000     C                   call      'ANA964R'
062700000000     C                   parm                    Kpjba
062800000000     C                   movel     Kpjbu         Ana964ds
062900000000     C*
063000000000     C                   if        Ret964     <> '2'
063100000000     C                   movel     Val964        D1TpAnn
063200000000     C                   endif
063300000000     C*
063400000000     C                   ENDSR
063500000000     C/EJECT
063600130906     C************************************************************
063700130906     C* Ricerca quadro di presentazione
063800130906     C************************************************************
063900130906     C     F4_Quadro     BEGSR
064000130906     C*
064100130906     C                   reset                   ana964ds
064200130906     C*
064300130906     C                   movel     '01'          Opz964
064301160105R195 C*
064302160105 "   C                   Select
064303160105 "   C*
064304160105 "   C* Spesometro
064305160105R195 C                   When      TpComG75    = 'E'
064400130906     C                   movel     'SDGSG73S '   Trc964
064500130906     C                   movel     'QUADROG73 '  Cam964
064501160105R195 C*
064502160105 "   C* Acquisti da San Marino
064503160105 "   C                   When      TpComG75    = 'S'
064504160105 "   C                   movel     'ANDIZ000 '   Trc964
064505160105 "   C                   movel     'SDGQUADRO1'  Cam964
064506160105 "   C*
064507160105 "   C* Black List
064508160105 "   C                   When      TpComG75    = 'B'
064509160105 "   C                   movel     'ANDIZ000 '   Trc964
064510160105 "   C                   movel     'SDGQUADRO2'  Cam964
064511160105 "   C                   endSl
064512160105R195 C*
064600130906     C                   movel     *On           All964
064700130906     C*
064800130906     C                   movel     ana964ds      Kpjbu
064900130906     C                   call      'ANA964R'
065000130906     C                   parm                    Kpjba
065100130906     C                   movel     Kpjbu         Ana964ds
065200130906     C*
065300130906     C                   if        Ret964     <> '2'
065400130906     C                   movel     Val964        D1Quadro
065500130906     C                   endif
065600130906     C*
065700130906     C                   ENDSR
065800130906     C/EJECT
065900130919     C************************************************************
066000130919     C* Ricerca tipo operazione
066100130919     C************************************************************
066200130919     C     F4_TpOpe      BEGSR
066300130919     C*
066400130919     C                   reset                   ana964ds
066500130919     C*
066600130919     C                   movel     '01'          Opz964
066700130919     C                   MoveL     'SDGSG73S '   Trc964
066800130919     C                   MoveL     'TPOPEG73 '   Cam964
066900130919     C                   movel     *On           All964
067000130919     C*
067100130919     C                   movel     ana964ds      Kpjbu
067200130919     C                   call      'ANA964R'
067300130919     C                   parm                    Kpjba
067400130919     C                   movel     Kpjbu         Ana964ds
067500130919     C*
067600130919     C                   if        Ret964     <> '2'
067700130919     C                   movel     Val964        D1TpOpe
067800130919     C                   endif
067900130919     C*
068000130919     C                   ENDSR
068100130919     C/EJECT
068200000000     C************************************************************
068300000000     C* RIEMPIMENTO OPZIONI
068400000000     C************************************************************
068500000000     C     RIEOPZ        BEGSR
068600000000     C*
068700000000     C                   CALLB     'X04OPMSG'
068800000000     C* input
068900000000     C                   PARM                    $TX
069000000000     C                   PARM                    $FL
069100000000     C                   PARM                    WCOZ              3 0
069200000000     C                   PARM                    WMXO              3 0
069300000000     C* output
069400000000     C                   PARM                    WOPZ1            76
069500000000     C                   PARM                    WOPZ2            76
069600000000     C                   PARM                    FF23              1
069700000000     C* input
069800000000     C                   PARM                    $L01              3 0
069900000000     C                   PARM                    $L02              3 0
070000000000     C*
070100000000     C                   PARM                    $ALLOPT         960
070200000000     C*
070300000000     C                   ENDSR
070400000000     C/EJECT
070500000000     C************************************************************
070600000000     C* RIEMPIMENTO TASTI FUNZIONE
070700000000     C************************************************************
070800000000     C     RIEKEY        BEGSR
070900000000     C*
071000000000     C                   CALLB     'XKEYMSG'
071100000000     C* input
071200000000     C                   PARM                    $FX
071300000000     C                   PARM                    $FC
071400000000     C                   PARM                    $FR
071500000000     C                   PARM                    $ULK              3 0
071600000000     C* output
071700000000     C                   PARM                    $KEY1            79
071800000000     C                   PARM                    $KEY2            79
071900000000     C* input
072000000000     C                   PARM                    $L01              3 0
072100000000     C                   PARM                    $L02              3 0
072200000000     C*
072300000000     C                   PARM                    $ALLFUNCT       480
072400000000     C*
072500000000     C                   ENDSR
072600000000     C/EJECT
072700000000     C************************************************************
072800000000     C* GESTIONE EMISSIONE TASTI FUNZIONE
072900000000     C************************************************************
073000000000     C     GESPIE        BEGSR
073100000000     C*
073200000000     C                   SELECT
073300000000     C* Parzializzazioni
073400000000     C     $GEST         WHENEQ    'D1'
073500000000     C     $ULKD1        IFEQ      0
073600000000     C* riempimento opzioni
073700000000     C                   EXSR      F24D1
073800000000     C                   ELSE
073900000000     C                   WRITE     FMTZ1
074000000000     C                   ENDIF
074100000000     C* Lista
074200000000     C     $GEST         WHENEQ    'S1'
074300000000     C     $ULKS1        IFEQ      0
074400000000     C                   EXSR      F24S1
074500000000     C                   ELSE
074600000000     C                   WRITE     FMTZ2
074700000000     C                   ENDIF
074800000000     C* Modifica valori assunti
074900000000     C     $GEST         WHENEQ    'D2'
075000000000     C     $ULKD2        IFEQ      0
075100000000     C                   EXSR      F24D2
075200000000     C                   ELSE
075300000000     C                   WRITE     FMTZ3
075400000000     C                   ENDIF
075500000000     C                   ENDSL
075600000000     C*
075700000000     C                   ENDSR
075800000000     C/EJECT
075900000000     C************************************************************
076000000000     C* GESTIONE PARZIALIZZAZIONI
076100000000     C************************************************************
076200000000     C     GESD1         BEGSR
076300000000     C*
076400000000     C* inizializzazione videata
076500000000     C     $INZD1        IFEQ      *ON
076600000000     C                   EXSR      INZD1
076700000000     C                   MOVE      *OFF          $INZD1
076800000000     C                   ENDIF
076900000000     C* emissione piede videata se proveniente da altra
077000000000     C* salvataggio valore formato di provenienza
077100000000     C     $LASTV        IFNE      'D1'
077200000000     C*
077300000000     C* recupero il sottotitolo (se c'è)
077400000000     C                   Clear                   D1TIT
077500000000     C     SOTTITD1      IFNE      *BLANK
077600000000     C                   MOVEL     SOTTITD1      IDMSG
077700000000     C                   CALLB     'XRTVM'
077800000000     C                   PARM                    IDMSG            27
077900000000     C                   PARM                    TXTMSG          100
078000000000     C                   PARM                    ERRMSG            1
078100000000     C                   PARM      *Off          CTRMSG            1            centratura
078200000000     C                   PARM      15            LENMSG            3 0          lun output.
078300000000     C     ERRMSG        IFNE      *ON
078400000000     C                   MOVEL     TXTMSG        D1TIT
078500000000     C                   ELSE
078600000000     C                   MOVEL     *ALL'?'       D1TIT
078700000000     C                   ENDIF
078800000000     C                   ENDIF
078900000000     C*
079000000000     C* salvo stato F17 per sapere cosa fare in caso di F12
079100000000     C                   MOVE      *INKR         WINKR
079200000000     C                   WRITE     FMTT1
079300000000     C                   EXSR      GESPIE
079400000000     C                   MOVE      $LASTV        $LASTG
079500000000     C                   MOVE      'D1'          $LASTV
079600000000     C                   ENDIF
079700000000     C*
079800000000     C* se esistono errori sulla videata
079900000000     C* emetto la write del formato a indicatori spenti per vedere
080000000000     C* le eventuali decodifiche
080100000000     C     *IN99         IFEQ      *ON
080200000000     C                   MOVEA     *IN           WIN
080300000000     C                   MOVE      *ALL'0'       IN5098           49
080400000000     C                   MOVEA     IN5098        *IN(50)
080500000000     C                   WRITE     FMTD1
080600000000     C                   MOVEA     WIN           *IN
080700000000     C                   ENDIF
080800000000     C*
080900000000     C                   EXFMT     FMTD1
081000000000     C*
081100000000     C* azzero la posizione del cursore
081200000000     C                   Z-ADD     0             H1RIGA
081300000000     C                   Z-ADD     0             H1COLO
081400000000     C*
081500000000     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
081600000000     C* INPUT
081700000000     C* $FC1 = Abilitazioni videata D1
081800000000     C* OUTPUT
081900000000     C* *IN97 + *IN99 Errori
082000000000     C                   MOVEA     $FC1          $FC
082100000000     C                   EXSR      CTRKEY
082200000000     C*
082300000000     C     *IN99         IFEQ      *OFF
082400000000     C*
082500000000     C                   SELECT
082600000000     C* F2=Decodifica
082700000000     C     $TASTO        WHENEQ    F02
082800000000     C                   EXSR      F02D1
082900000000     C* F3=Fine
083000000000     C     $TASTO        WHENEQ    F03
083100000000     C                   EXSR      F03D1
083200000000     C* F4=Ricerca
083300000000     C     $TASTO        WHENEQ    F04
083400000000     C                   EXSR      SEARCH
083500000000     C* F5=Refresh
083600000000     C     $TASTO        WHENEQ    F05
083700000000     C                   EXSR      F05D1
083800000000     C* F8=Successivo
083900000000     C     $TASTO        WHENEQ    F08
084000000000     C                   EXSR      F08D1
084100000000     C* F10=Immissione
084200000000     C     $TASTO        WHENEQ    F10
084300000000     C                   EXSR      F10D1
084400000000     C* F12=Ritorno
084500000000     C     $TASTO        WHENEQ    F12
084600000000     C                   EXSR      F12D1
084700000000     C* F18=Modifica valori assunti
084800000000     C     $TASTO        WHENEQ    F18
084900000000     C                   EXSR      F18D1
085000000000     C* F24=Altri tasti
085100000000     C     $TASTO        WHENEQ    F24
085200000000     C                   EXSR      F24D1
085300000000     C*
085400000000     C                   OTHER
085500000000     C                   EXSR      CTRD1
085600000000     C     *IN99         IFEQ      *OFF
085700000000     C* definisco l'ordinamento
085800000000     C                   EXSR      DEFORD
085900000000     C* richiamo la lista
086000000000     C                   MOVE      'S1'          $GEST
086100000000     C                   MOVE      *ON           $INZS1
086200000000     C                   MOVE      *BLANK        W1POS
086300000000     C                   ENDIF
086400000000     C*
086500000000     C                   ENDSL
086600000000     C*
086700000000     C                   ENDIF
086800000000     C*
086900000000     C                   ENDSR
087000000000     C/EJECT
087100000000     C************************************************************
087200000000     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
087300000000     C************************************************************
087400000000     C     INZD1         BEGSR
087500000000     C*
087600000000     C                   CLEAR                   FMTD1
087700000000     C*
087800000000     C* Se pgm  richiamato con parametri forniti , li imposto nelle
087900000000     C*    parzializzazioni
088000000000     C                   SELECT
088100000000     C* il pgm è richiamato con chiave univoca per gestione POSTA
088200000000     C     $Unique       WHENEQ    *ON
088300000000     C* il pgm è richiamato e
088400000000     C* sono stati passati dei valori di parzializzazione ma non
088500000000     C* la chiave univoca
088600130906     C     OPZG75        WHENNE    *BLANK
088700000000     C* imposto i valori passati sui campi a video
088800130906     C                   MOVEL     AnnoG75       D1Anno
088900130906     C                   MOVEL     CFG75         D1CF
089000130906     C                   MOVEL     StatoG75      D1Stato
089100130906     C                   MOVEL     PartIvaG75    D1PartIva
089200130906     C                   MOVEL     CdFiscG75     D1CdFisc
089300130906     C                   MOVEL     SoggG75       D1Sogg
089400130906     C                   MOVEL     AnnG75        D1Ann
089500130906     C                   MOVEL     TpAnnG75      D1TpAnn
089600130906     C                   MOVEL     QuadroG75     D1Quadro
089700130919     C                   MOVEL     TpOpeG75      D1TpOpe
089800000000     C* il pgm non è richiamato
089900130906     C     OPZG75        WHENEQ    *BLANK
090000000000     C                   Eval      D1Ann       = *Blank
090100130906     C                   ENDSL
090200130906     C*
090300130906     C                   ENDSR
090400000000      /EJECT
090500130906     C************************************************************
090600130906     C* GESTIONE F02 VIDEO D1
090700130906     C************************************************************
090800000000     C     F02D1         BEGSR
090900000000     C*
091000000000     C* per decodificare la videata richiamo la routine dei ctrl
091100000000     C* e poi disattivo gli eventuali messaggi di errore
091200000000     C                   EXSR      CTRD1
091300000000     C*
091400000000     C                   MOVE      *ALL'0'       IN5098
091500000000     C                   MOVEA     IN5098        *IN(50)
091600000000     C*
091700000000     C                   ENDSR
091800000000     C/EJECT
091900000000     C************************************************************
092000000000     C* GESTIONE F03 VIDEO D1
092100000000     C************************************************************
092200000000     C     F03D1         BEGSR
092300000000     C*
092400000000     C                   MOVE      *ON           $FINE
092500130906    >C                   MOVE      '1'           RETG75
092600000000     C* fine programma
092700000000     C                   EXSR      ENDPGM
092800000000     C*
092900000000     C                   ENDSR
093000000000     C/EJECT
093100000000     C************************************************************
093200000000     C* GESTIONE F05 VIDEO D1
093300000000     C************************************************************
093400000000     C     F05D1         BEGSR
093500000000     C*
093600000000     C                   MOVE      *ON           $INZD1
093700000000     C*
093800000000     C                   ENDSR
093900000000     C/EJECT
094000000000     C************************************************************
094100000000     C* GESTIONE F08 VIDEO D1
094200000000     C************************************************************
094300000000     C     F08D1         BEGSR
094400000000     C*
094500000000     C                   MOVE      *ON           $FINE
094600130906    >C                   MOVE      '0'           RETG75
094700000000     C*
094800000000     C                   ENDSR
094900000000     C/EJECT
095000000000     C************************************************************
095100000000     C* GESTIONE F10 VIDEO D1
095200000000     C************************************************************
095300000000     C     F10D1         BEGSR
095400000000     C*
095500130906    >C                   RESET                   SDGSG76DS
095600130906    >C                   MOVE      xscsoc        SocietaG76
095700130906    >C                   MOVEL     '01'          OPZG76
095800130906    >C     CMTG75        IFEQ      '1'
095900130906    >C                   MOVEL     '1'           CMTG76
096000000000     C                   ELSE
096100130906    >C                   MOVEL     '0'           CMTG76
096200000000     C                   ENDIF
096300140401R133 C*
096400140401R133 C                   Eval      MeseG76     = 0
096500140401R133 C*
096600140401R133 C                   Select
096601151201R195 C* Acquisti da San Marino
096700140401R133 C                   When      TpComG75    = 'S'
096800140401R133 C                   Eval      TpComG76    = 'S'
096900140401R133 C                   Eval      MeseG76     = MeseG75
096901151201R195 C* Black List
096902151201 "   C                   When      TpComG75    = 'B'
096903151201 "   C                   Eval      TpComG76    = 'B'
096904151201 "   C                   Eval      MeseG76     = 0
096905151201R195 C*
097000140401R133 C                   Other
097001151201R195 C* Spesometro
097100140401R133 C                   Eval      TpComG76    = 'E'
097101151201R195 C                   Eval      MeseG76     = 0
097102151201R195 C*
097200140401R133 C                   EndSl
097300140401R133 C*
097400000000     C* passare anche le parzializzazioni ricevute relative alla chiave
097500000000     C* univoca :
097600000000     C*  se ce ne sono, potranno essere usate come dft d'immissione
097700130906    >C                   IF        OPZG75 <> *Blank
097800000000     C                   ENDIF
097900000000     C*
098000130906    >C                   CALL      'SDGSG76R'
098100000000     C                   PARM                    KPJBA
098200130906    >C                   PARM                    SDGSG76DS
098300000000     C* ritorno da PGM gestione
098400130906     C                   EXSR      RTNG76
098500000000     C*
098600000000     C                   MOVE      *HIVAL        $LASTV
098700000000     C*
098800000000     C                   ENDSR
098900000000     C/EJECT
099000000000     C************************************************************
099100000000     C* GESTIONE F12 VIDEO D1
099200000000     C************************************************************
099300000000     C     F12D1         BEGSR
099400000000     C*
099500000000     C*  se la 1a videata è la lista oppure sono arrivato qui con F17,
099600000000     C*  per F12 torno alla lista
099700000000     C     $LOP          IFEQ      2
099800000000     C     WINKR         OREQ      *ON
099900000000     C                   MOVE      'S1'          $GEST
100000000000     C*  altrimenti, per F12 torno a menu
100100000000     C                   ELSE
100200000000     C                   MOVE      *ON           $FINE
100300130906    >C                   MOVE      '2'           RETG75
100400000000     C                   ENDIF
100500000000     C*
100600000000     C                   ENDSR
100700000000     C/EJECT
100800000000     C************************************************************
100900000000     C* GESTIONE F18 VIDEO D1
101000000000     C************************************************************
101100000000     C     F18D1         BEGSR
101200000000     C*
101300000000     C                   MOVE      'D2'          $GEST
101400000000     C                   MOVEL     *ON           $INZD2
101500000000     C*
101600000000     C                   ENDSR
101700000000     C/EJECT
101800000000     C************************************************************
101900000000     C* GESTIONE F24 VIDEO D1
102000000000     C* input
102100000000     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
102200000000     C* $FX1 =Testi tasti funzione
102300000000     C* $FC1 = Abilitazioni
102400000000     C* $FR1 = Inversioni di fondo
102500000000     C************************************************************
102600000000     C     F24D1         BEGSR
102700000000     C*
102800000000     C                   MOVEA     $FX1          $FX
102900000000     C                   MOVEA     $FC1          $FC
103000000000     C                   MOVEA     $FR1          $FR
103100000000     C                   Z-ADD     $ULKD1        $ULK
103200000000     C                   Z-ADD     $D1L01        $L01
103300000000     C                   Z-ADD     $D1L02        $L02
103400000000     C                   EXSR      RIEKEY
103500000000     C                   Z-ADD     $ULK          $ULKD1            3 0
103600000000     C                   MOVEA     $FC           $FC1
103700000000     C                   MOVEL     $KEY1         Z1KE1
103800000000     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
103900000000     C                   MOVEL     $KEY2         Z1KE2
104000000000     C                   WRITE     FMTZ1
104100000000     C*
104200000000     C*
104300000000     C                   ENDSR
104400000000     C/EJECT
104500000000     C************************************************************
104600000000     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
104700000000     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
104800000000     C* 93 94 96 97 98
104900000000     C************************************************************
105000000000     C     CTRD1         BEGSR
105100000000     C*
105200000000     C                   SETOFF                                       99
105300000000     C*
105400000000     C                   EXSR      CtrAnno
105500000000     C*
105600000000     C                   EXSR      CtrStato
105700000000     C*
105800000000     C                   EXSR      CtrSogg
105900000000     C*
106000000000     C                   EXSR      CtrAnn
106100130906     C*
106200130906     C                   EXSR      CtrQuadro
106300130919     C*
106400130919     C                   EXSR      CtrTpOpe
106500000000     C*
106600000000     C                   ENDSR
106700000000     C/EJECT
106800000000     C***********************************************************
106900000000     C* Controllo anno di competenza
107000000000     C***********************************************************
107100000000     C     CtrAnno       BEGSR
107200000000     C*
107300000000     C                   if        D1Anno = 0
107400000000     C                   SetOn                                        5399
107500000000     C                   endif
107600000000     C*
107700000000     C                   ENDSR
107800000000     C/EJECT
107900000000     C***********************************************************
108000000000     C* Controllo Stato
108100000000     C***********************************************************
108200000000     C     CtrStato      BEGSR
108300000000     C*
108400000000     C                   Clear                   D1StatoD
108500000000     C                   Clear                   SDGHD1DS
108600000000     C*
108700000000     C                   if        D1Stato    <> *Blank
108800000000     C*
108900000000     C                   RESET                   SDGTABDS
109000000000     C*
109100000000     C                   Eval      XTbRic      = '1'
109200000000     C                   Eval      XTbAzi      = XScSoc
109300000000     C                   Eval      XTbKey      = *Zero
109400000000     C                   Eval      XTbCod      = 'HD1'
109500000000     C                   Move      D1Stato       XTbKey
109600000000     C*
109700000000     C                   CALL      'SDGTABDR'
109800000000     C                   PARM                    SDGTABDS
109900000000     C*
110000000000     C                   if        XTbErr     <> '0'
110100000000     C                   SetOn                                        6899
110200000000     C*
110300000000     C                   else
110400000000     C                   MoveL     XTbUni        SDGHD1DS
110500000000     C                   MoveL     DesHD1        D1StatoD
110600000000     C*
110700000000     C                   endif
110800000000     C*
110900000000     C                   endif
111000000000     C*
111100000000     C                   ENDSR
111200000000     C/EJECT
111300000000     C***********************************************************
111400000000     C* Controllo codice soggetto
111500000000     C***********************************************************
111600000000     C     CtrSogg       BEGSR
111700000000     C*
111800000000     C                   Clear                   D1SoggD
111900000000     C                   if        D1Sogg     <> *Blank
112000000000     C*
112100000000     C     D1SOGG        IFNE      *BLANK
112200000000     C                   CALLb     'XALLINEA'
112300000000     C                   PARM                    D1SOGG
112400000000     C                   PARM      8             LEN               5 0
112500000000     C                   PARM                    XSCINP
112600000000     C                   ENDIF
112700000000     C*
112800000000     C                   MoveL     D1Sogg        WSogg
112900000000     C                   EXSR      CallDvaSog
113000000000     C*
113100000000     C                   if        DVAErrore  <> '1'
113200000000     C*
113300000000     C                   Eval      %subst(DVASOG:1:DVALENOUT)
113400000000     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
113500000000     C*
113600000000     C                   MoveL     DVSDes        D1SoggD
113700000000     C*
113800000000     C                   else
113900000000     C*
114000000000     C                   SetOn                                        5599
114100000000     C*
114200000000     C                   endif
114300000000     C*
114400000000     C                   endif
114500000000     C*
114600000000     C                   ENDSR
114700000000     C/EJECT
114800000000     C************************************************************
114900000000     C* Chiamata al driver dei soggetti
115000000000     C************************************************************
115100000000     C     CallDvaSog    BEGSR
115200000000     C*
115300000000     C                   EVAL      DVALenOut   = %Size(DVASOG)
115400000000     C*
115500000000     C                   CALLB     'NDDVASOG'
115600000000     C                   PARM      '1'           DVARIC
115700000000     C                   PARM      XSCSOC        DVASOCIETA
115800000000     C                   PARM                    DVAUNITA
115900000000     C                   PARM      'DVASOG    '  DVASTRUTT
116000000000     C                   PARM                    DVADTRIF
116100000000     C                   PARM                    DVALENOUT
116200000000     C* Se si accede con il codice cli/for impostare i campi seguenti
116300000000     C* se si accede con il soggetto ometterli e impostare gli altri
116400000000     C*                  PARM                    DVASNATURA
116500000000     C*                  PARM                    DVACODICE
116600000000     C*                  PARM                    DVALINEAV
116700000000     C*                  PARM                    DVAFILIALE
116800000000     C                   PARM                    *omit
116900000000     C                   PARM                    *omit
117000000000     C                   PARM                    *omit
117100000000     C                   PARM                    *omit
117200000000     C                   PARM      WSogg         DVSSOGG
117300000000     C                   PARM      *Blank        DVSTPIND
117400000000     C                   PARM      *Blank        DVSCDIND
117500000000     C                   PARM                    DVAERRORE
117600000000     C                   PARM                    DVAOUTPUT
117700000000     C*
117800000000     C                   ENDSR
117900000000     C/EJECT
118000000000     C***********************************************************
118100000000     C* Controllo dati annullati
118200000000     C***********************************************************
118300000000     C     CtrAnn        BEGSR
118400000000     C*
118500000000     C                   Clear                   D1AnnD
118600000000     C*
118700000000     C                   Clear                   X07ValDs
118800000000     C                   Move      '1'           X07Ric
118900000000     C                   Move      *Off          X07All
119000000000     C                   Movel     'ANDIZ000 '   X07TRc
119100000000     C                   Movel     'DSPATT   '   X07Cam
119200000000     C                   Movel     D1Ann         X07VAL
119300000000     C                   Callb     'X07VALR'
119400000000     C                   Parm                    X07ValDs
119500000000     C*
119600000000     C     X07ERR        IFEQ      *Off
119700000000     C                   MOVEL     X07DES        D1AnnD
119800000000     C                   else
119900000000     C                   SetOn                                        5699
120000000000     C                   endif
120100000000     C*
120200000000     C                   Clear                   D1TpAnnD
120300000000     C                   if        D1TpAnn    <>  *blank
120400000000     C*  solo se richiesti solo gli annullati oppure con annullati
120500000000     C                   if        D1Ann      =   '1'    or
120600000000     C                             D1Ann      =   '3'
120700000000     C                   exsr      CtrTpAnn
120800000000     C                   else
120900000000     C                   SetOn                                        5899
121000000000     C                   endif
121100000000     C                   endif
121200000000     C*
121300000000     C                   ENDSR
121400000000     C/EJECT
121500000000     C***********************************************************
121600000000     C* Controllo tipo annullamento
121700000000     C***********************************************************
121800000000     C     CtrTpAnn      BEGSR
121900000000     C*
122000000000     C                   Clear                   X07ValDs
122100000000     C                   Move      '1'           X07Ric
122200000000     C                   Move      *Off          X07All
122300130906     C                   Movel     'SDGSCF000'   X07TRc
122400130906     C                   Movel     'SCFTPANN'    X07Cam
122500000000     C                   Movel     D1TpAnn       X07VAL
122600000000     C                   Callb     'X07VALR'
122700000000     C                   Parm                    X07ValDs
122800000000     C*
122900000000     C     X07ERR        IFEQ      *Off
123000000000     C                   MOVEL     X07DES        D1TpAnnD
123100000000     C                   else
123200000000     C                   SetOn                                        5799
123300000000     C                   endif
123400000000     C*
123500000000     C                   ENDSR
123600000000     C/EJECT
123700130906     C***********************************************************
123800130906     C* Controllo codice quadro di presentazione
123900130906     C***********************************************************
124000130906     C     CtrQuadro     BEGSR
124100130906     C*
124200130906     C                   Clear                   D1QuadroD
124300130906     C*
124400130906     C                   Clear                   X07ValDs
124500130906     C                   Move      '1'           X07Ric
124600130906     C                   Move      *On           X07All
124601160105R195 C*
124602160105 "   C                   Select
124603160105 "   C*
124604160105 "   C* Spesometro
124605160105R195 C                   When      TpComG75    = 'E'
124700130906     C                   Movel     'SDGSG73S '   X07TRc
124800130906     C                   Movel     'QUADROG73 '  X07Cam
124801160105R195 C*
124802160105 "   C* Acquisti da San Marino
124803160105 "   C                   When      TpComG75    = 'S'
124804160105 "   C                   Movel     'ANDIZ000 '   X07TRc
124805160105 "   C                   Movel     'SDGQUADRO1'  X07Cam
124806160105 "   C*
124807160105 "   C* Black List
124808160105 "   C                   When      TpComG75    = 'B'
124809160105 "   C                   Movel     'ANDIZ000 '   X07TRc
124810160105 "   C                   Movel     'SDGQUADRO2'  X07Cam
124811160105 "   C                   endSl
124812160105R195 C*
124900130906     C                   Movel     D1Quadro      X07VAL
125000130906     C                   Callb     'X07VALR'
125100130906     C                   Parm                    X07ValDs
125200130906     C*
125300130906     C     X07ERR        IFEQ      *Off
125400130906     C                   MOVEL     X07DES        D1QuadroD
125500130906     C                   else
125600130906     C                   SetOn                                        5999
125700130906     C                   endif
125800130906     C*
125900130906     C                   ENDSR
126000130906     C/EJECT
126100130919     C***********************************************************
126200130919     C* Controllo tipo operazione
126300130919     C***********************************************************
126400130919     C     CtrTpOpe      BEGSR
126500130919     C*
126600130919     C                   Clear                   D1TpOpeD
126700130919     C*
126800130919     C                   Clear                   X07ValDs
126900130919     C*
127000130919     C                   Move      '1'           X07Ric
127100130919     C                   Move      *On           X07All
127200130919     C                   Movel     'SDGSG73S '   X07TRc
127300130919     C                   Movel     'TPOPEG73 '   X07Cam
127400130919     C                   Movel     D1TpOpe       X07VAL
127500130919     C*
127600130919     C                   Callb     'X07VALR'
127700130919     C                   Parm                    X07ValDs
127800130919     C*
127900130919     C     X07ERR        IFEQ      *Off
128000130919     C                   MOVEL     X07DES        D1TpOpeD
128100130919     C                   else
128200130919     C                   SetOn                                        5599
128300130919     C                   endif
128400130919     C*
128500130919     C                   ENDSR
128600130919     C/EJECT
128700000000     C************************************************************
128800000000     C* DEFINIZIONE DELL'ORDINAMENTO
128900000000     C************************************************************
129000000000     C     DEFORD        BEGSR
129100000000     C*
129200000000     C* definisco l'ordinamento della lista a seconda dei valori
129300000000     C* impostati nel video di parzializzazione
129400000000     C                   Z-ADD     1             $ORD
129500000000     C*
129600000000     C                   SELECT
129700000000     C* imposto l'ordinamento 99 per ottenere in selezione solo il
129800000000     C* rcd con la chiave passata
129900000000     C     $Unique       WHENEQ    *ON
130000000000     C                   Z-ADD     99            $ORD
130100000000     C* nel caso non sia stato digitato alcun valore in parzializzazione
130200000000     C* utilizzerò l'ordinamento precedentemente memorizzato
130300000000     C* (temporaneamente o definitivamente) nei valori assunti
130400000000     C                   OTHER
130500000000     C                   ENDSL
130600000000     C                   ENDSR
130700000000     C/EJECT
130800000000     C************************************************************
130900000000     C* Chiamata programma con tasto personalizzato
131000000000     C************************************************************
131100000000     C     FPerS1        BEGSR
131200000000     C*
131300000000     C*
131400000000     C* salvo la DS di ingresso
131500130906    >C                   MOVEL     SDGSG75DS     $SaveDS
131600000000     C*
131700130906    >C                   MOVEL     SDGSG75DS     KPJBU
131800000000     C                   CALL      $PGF2(PosFun)                        22
131900000000     C                   PARM                    KPJBA
132000000000     C*
132100000000     C* ripristino la DS di ingresso
132200130906    >C                   MOVEL     $SaveDS       SDGSG75DS
132300000000     C*
132400000000     C                   ENDSR
132500000000     C/EJECT
132600000000     C************************************************************
132700000000     C* GESTIONE LISTA
132800000000     C************************************************************
132900000000     C     GESS1         BEGSR
133000000000     C*
133100000000     C* mantengo la situazione 'aperta' o 'chiusa' del sfl drop
133200000000     C                   Eval      *In09     =   C1Mode
133300000000     C*
133400000000     C* inizializzazione videata
133500000000     C     $INZS1        IFEQ      *ON
133600000000     C                   EXSR      INZS1
133700000000     C                   MOVE      *OFF          $INZS1
133800000000     C                   ENDIF
133900000000     C* emissione piede videata se proveniente da altra
134000000000     C* salvataggio valore formato di provenienza
134100000000     C     $LASTV        IFNE      'S1'
134200000000     C                   WRITE     FMTT1
134300000000     C                   EXSR      GESPIE
134400000000     C                   MOVE      $LASTV        $LASTG
134500000000     C                   MOVE      'S1'          $LASTV
134600000000     C                   ENDIF
134700000000     C*
134800000000     C     WMAX          IFEQ      0
134900000000     C                   WRITE     FMTD3                                        NO RCD
135000000000     C                   ENDIF
135100000000     C* Inizializzazione indice tasti funzione personalizzati
135200000000     C                   Eval      PosFun = 25
135300000000     C*
135400000000     C                   EXFMT     FMTC1
135500000000     C*
135600000000     C* azzero la posizione del cursore
135700000000     C                   Z-ADD     0             H1RIGA
135800000000     C                   Z-ADD     0             H1COLO
135900000000     C* Serve per avvertire i pgm di sola finestra di eseguire le operazioni
136000000000     C* di refresh dello sfondo
136100000000     C                   MOVE      '1'           WExf
136200000000     C*
136300000000     C* riemettere il nuovo sfondo del subfile
136400000000     C                   Eval      WA39Iniz    = '0'
136500000000     C*
136600000000     C     C1NRR         IFNE      0
136700000000     C                   Z-ADD     C1NRR         WSFL
136800000000     C                   ENDIF
136900000000     C                   IF        SFLNRR > 0
137000000000     C                   Z-ADD     SFLNRR        C1RCD
137100000000     C                   ENDIF
137200000000     C* inizializzazione indice opzioni personalizzate
137300000000     C                   Eval      PosOpz = 0
137400000000     C* CONTROLLO ABILITAZIONE TASTI
137500000000     C* INPUT
137600000000     C* $FC2 = Abilitazioni videata S1
137700000000     C* OUTPUT
137800000000     C* *IN97 + *IN99 Errori
137900000000     C                   MOVEA     $FC2          $FC
138000000000     C                   EXSR      CTRKEY
138100000000     C*
138200000000     C     *IN99         IFEQ      *OFF
138300000000     C*
138400000000     C                   SELECT
138500000000     C* F3=Fine
138600000000     C     $TASTO        WHENEQ    F03
138700000000     C                   EXSR      F03S1
138800130906     C* F5=Refresh
138900130906     C     $TASTO        WHENEQ    F05
139000130906     C                   EXSR      F05S1
139100000000     C* F8=Successivo
139200000000     C     $TASTO        WHENEQ    F08
139300000000     C                   EXSR      F08S1
139400000000     C* F10=Immissione
139500000000     C     $TASTO        WHENEQ    F10
139600000000     C                   EXSR      F10S1
139700000000     C* F12=Ritorno
139800000000     C     $TASTO        WHENEQ    F12
139900000000     C                   EXSR      F12S1
140000000000     C* F17=Parzializzazioni
140100000000     C     $TASTO        WHENEQ    F17
140200000000     C                   EXSR      F17S1
140300000000     C* F18=Modifica valori assunti
140400000000     C     $TASTO        WHENEQ    F18
140500000000     C                   EXSR      F18S1
140600000000     C* F23=Altre opzioni
140700000000     C     $TASTO        WHENEQ    F23
140800000000     C                   EXSR      F23S1
140900000000     C* F24=Altri tasti
141000000000     C     $TASTO        WHENEQ    F24
141100000000     C                   EXSR      F24S1
141200000000     C* ROLL-UP
141300000000     C     $TASTO        WHENEQ    ROLLUP
141400000000     C                   EXSR      ROLS1
141500000000     C* Personalizzata
141600000000     C     $PGF2(PosFun) Whenne    *BLANK
141700000000     C                   EXSR      FperS1
141800000000     C*
141900000000     C                   OTHER
142000000000     C* CONTROLLO DATI
142100000000     C                   EXSR      CTRC1
142200000000     C     *IN99         IFEQ      *OFF
142300000000     C                   EXSR      CTRS1
142400000000     C                   ENDIF
142500000000     C*
142600000000     C                   ENDSL
142700000000     C*
142800000000     C                   ENDIF
142900000000     C*
143000000000     C                   ENDSR
143100000000     C/EJECT
143200000000     C************************************************************
143300000000     C* Caricamento OPZIONI
143400000000     C************************************************************
143500000000    >C     CarOpz        BEGSR
143600000000     C*
143700000000     C* Reperimento tasti di funzione
143800000000     C                   EXSR      REPKEY
143900000000     C*
144000000000     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
144100000000     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
144200000000     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
144300000000     C     $ULKD1        IFEQ      0
144400000000     C                   EXSR      F24D1
144500000000     C                   ELSE
144600000000     C                   WRITE     FMTZ1
144700000000     C                   ENDIF
144800000000     C*
144900000000     C                   ENDSR
145000000000     C/EJECT
145100000000     C************************************************************
145200000000     C* INIZIALIZZAZIONE LISTA
145300000000     C************************************************************
145400000000    >C     INZS1         BEGSR
145500000000     C* pulizia SFL
145600000000     C                   SETOFF                                         3031
145700000000     C                   WRITE     FMTC1
145800000000     C                   SETON                                          31
145900000000     C*
146000000000     C* pulizia del sottotitolo
146100000000     C                   Clear                   D1TIT
146200000000     C*
146300130906     C                   Eval      C1Anno      = AnnoG75
146400140401R133 C                   Select
146401151201R195 C* Acquisti da San Marino
146500140401 "   C                   When      TpComg75    = 'S'
146600140401 "   C                   Eval      *In11       = *On
146700140401 "   C                   Eval      C1Mese      = MeseG75
146800151201R133 C                   Other
146801151201R195 C* Spesometro o Black List
146900151201R133 C                   Eval      *In11       = *Off
147000140401 "   C                   endSl
147100140401R133 C*
147200130906     C                   Eval      C1PartIva   = PartIvaG75
147300130906     C                   Eval      C1CdFisc    = CdFiscG75
147400130906     C                   Eval      C1CogRag    = CogRagG75
147500130906     C                   Eval      H1Nome      = NomeG75
147600130906     C                   Eval      H1DtNasc    = DtNascG75
147700130906     C                   Eval      H1LocNasc   = LocNascG75
147800130906     C                   Eval      H1PrvNasc   = PrvNascG75
147900130906     C                   Eval      C1Stato     = StatoG75
148000130924     C                   Eval      C1CodUIC    = CodUICG75
148100130906     C                   Eval      H1Localit   = LocalitG75
148200130906     C                   Eval      H1IndirE    = IndirEG75
148300000000     C*
148400000000     C* riempimento opzioni
148500000000     C                   MOVEA     $TX1          $TX
148600000000     C                   MOVEA     $FL1          $FL
148700000000     C                   Z-ADD     0             WCOZ
148800000000     C                   Z-ADD     0             WMXO
148900000000     C*  equivale al num.di elementi della schiera $TX1
149000000000     C                   Z-add     OpzLun        WMXO
149100000000     C                   Z-ADD     $C1Op1        $L01
149200000000     C                   Z-ADD     $C1Op2        $L02
149300000000     C                   EXSR      RIEOPZ
149400000000     C                   Z-ADD     23            X
149500000000     C                   MOVE      FF23          $FC2(X)
149600000000     C                   MOVEL     WOPZ1         C1OPZ1
149700000000     C                   MOVEL     $ALLOPT       H1ALLOPT
149800000000     C                   MOVEL     WOPZ2         C1OPZ2
149900000000     C                   Z-ADD     WCOZ          C1COZ
150000000000     C*
150100000000     C* CARICAMENTO SFL A PAGINE
150200000000     C*
150300000000     C                   Z-ADD     0             S1NRR
150400000000     C                   Z-ADD     0             WMAX
150500000000     C*
150600110825p    C* Gestione del file di lavoro
150700130906p    C                   EXSR      GestSCF00W
150800110825p    C*
150900000000     C* Posizionamento su file pilota
151000000000     C                   CLEAR                   C1POS
151100110825p    C                   EXSR      SETANA1
151200000000     C* ripristino i valori del posizionamento
151300000000     C                   MOVE      W1POS         C1POS
151400000000     C*
151500000000     C* Lettura file pilota
151600110825p    C                   EXSR      REDANA1
151700110825p    C*
151800110825p    C* Lettura workfile
151900110825p    C                   EXSR      SETANA
152000110825p    C                   EXSR      REDANA
152100000000     C*
152200000000     C* se arrivo dalla parzializzazione devo caricare solo la prima pagina
152300000000     C                   IF        $LastV = 'D1'
152400000000     C                   Z-ADD     SFLPAG        WPAG
152500000000     C                   ENDIF
152600000000     C                   EXSR      ROLS1
152700000000     C* imposto il caricamento sfl del numero massimo di righe per pagina
152800000000     C                   Z-ADD     SFLPAG        WPAG
152900000000     C*
153000000000     C                   ENDSR
153100000000     C/EJECT
153200000000     C************************************************************
153300000000     C* CARICAMENTO PAGINA LISTA
153400000000     C************************************************************
153500000000     C     ROLS1         BEGSR
153600000000     C*
153700000000     C                   SETOFF                                       32
153800000000     C                   Z-ADD     0             Y
153900000000     C                   Z-ADD     WMAX          S1NRR
154000000000     C*
154100000000     C* Leggo dal file anagrafico per caricare la lista
154200000000     C*
154300000000     C     $EndFile      DOWEQ     *OFF
154400000000     C     Y             ANDLT     WPAG
154500000000     C*
154600000000     C                   EXSR      RIES1
154700000000     C*
154800000000     C                   ADD       1             S1NRR
154900000000     C                   ADD       1             Y
155000000000     C*
155100000000     C                   WRITE     FMTS1
155200000000     C*
155300000000     C                   EXSR      REDANA
155400000000     C*
155500000000     C                   ENDDO
155600000000     C*
155700000000     C                   Z-ADD     S1NRR         WMAX                 30
155800000000     C*
155900000000     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
156000000000     C*
156100000000     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
156200000000     C                   MVR                     RESTO             3 0
156300000000     C     PAGINE        MULT      SFLPAG        C1RCD
156400000000     C     RESTO         IFGT      0
156500000000     C                   ADD       1             C1RCD
156600000000     C                   ELSE
156700000000     C                   SUB       SFLPAG        C1RCD
156800000000     C                   ADD       1             C1RCD
156900000000     C                   ENDIF
157000000000     C*
157100000000     C                   ENDSR
157200000000     C/EJECT
157300000000     C************************************************************
157400000000     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
157500000000     C************************************************************
157600110825p    C     SETANA1       BEGSR
157700000000     C*
157800130906     C                   RESET                   SDG00502DS
157900130906     C*
158000130912     C                   Eval      Soc0502     = XscSoc
158100110824     C*
158200130912     C                   Eval      Ann0502     = AnnG75
158300130912     C                   Eval      TpAnn0502   = TpAnnG75
158400130912     C                   if        TpAnn0502   = *Blank
158500130912     C                   Eval      TpAnn0502   = *Hival
158600110824     C                   endif
158700140328     c*
158800140328R133 C                   Select
158801151201R195 C* Acquisti da San Marino
158900140328R133 C                   When      TpComG75    = 'S'
159000140328R133 C                   Eval      TpCom0502   = 'S'
159001151201R195 C* Black List
159002151201 "   C                   When      TpComG75    = 'B'
159003151201R195 C                   Eval      TpCom0502   = 'B'
159100140328R133 C                   Other
159101151201R195 C* Spesometro
159200130912     C                   Eval      TpCom0502   = 'E'
159300140328R133 C                   EndSl
159301151201R195 C*
159400130912     C                   Eval      Anno0502    = AnnoG75
159500130912     C                   Eval      Mese0502    = MeseG75
159600130906     C*
159700130912     C                   Eval      CF0502      = CFG75
159800130906     C                   if        CF0502      = *Blank
159900130906     C                   MoveL     *Hival        CF0502
160000130906     C                   endif
160100130906     C*
160200130912     C                   Eval      Sogg0502    = SoggG75
160300130906     C                   if        Sogg0502    = *Blank
160400130906     C                   MoveL     *Hival        Sogg0502
160500130906     C                   endif
160600131206R141 C*
160700131206 "   C                   Eval      Causal0502  = CausaleG75
160800131206 "   C                   if        Causal0502  = *Blank
160900131206 "   C                   MoveL     *Hival        Causal0502
161000131206R141 C                   endif
161100130906     C*
161200130912     C                   Eval      Stato0502   = StatoG75
161300130906     C*
161400130912     C                   Eval      PIva0502    = PartIvaG75
161500130912     C                   Eval      CdFisc0502  = CdFiscG75
161600130906     C*
161700130919     C                   Eval      TpOpe0502   = TpOpeG75
161800130919     C                   if        TpOpe0502   = *Blank
161900130919     C                   MoveL     *Hival        TpOpe0502
162000130906     C                   endif
162100130919     C*
162200130919     C                   Eval      Quadro0502  = QuadroG75
162300130919     C                   if        Quadro0502  = *Blank
162400130919     C                   MoveL     *Hival        Quadro0502
162500130919     C                   endif
162600130906     C*
162700130926     C                   Eval      CogRag0502  = CogRagG75
162800130912     C                   Eval      Nome0502    = NomeG75
162900130912     C                   Eval      DtNasc0502  = DtNascG75
163000130912     C                   Eval      LocNas0502  = LocNascG75
163100130912     C                   Eval      PrvNas0502  = PrvNascG75
163200130912     C                   Eval      CodUIC0502  = CodUICG75
163300130912     C                   Eval      Locali0502  = LocalitG75
163400130912     C                   Eval      IndirE0502  = IndirEG75
163500130912     C*
163600130912     C                   Eval      TpN0502     = *Blank
163700130912     C                   if        TpN0502     = *Blank
163800130912     C                   MoveL     *Hival        TpN0502
163900130912     C                   endif
164000110824     C*
164100130912     C                   Eval      CogRaR0502  = CogRagRG75
164200130912     C                   Eval      NomeR0502   = NomeRG75
164300130912     C                   Eval      DtNasR0502  = DtNasRG75
164400130912     C                   Eval      LocNaR0502  = LocNasRG75
164500130912     C                   Eval      PrvNaR0502  = PrvNasRG75
164600130912     C                   Eval      StatoR0502  = StatoRG75
164700130912     C                   Eval      CodUIR0502  = CodUICRG75
164800130912     C                   Eval      LocalR0502  = LocaliRG75
164900130912     C                   Eval      IndiER0502  = IndirERG75
165000130906     C*
165100130906     C                   Eval      TpNR0502    = TpNRG75
165200130906     C                   Eval      PIVAR0502   = PIVARG75
165300130906     C                   Eval      CDFISR0502  = CDFISCRG75
165400130925     C                   Eval      TPOPER0502  = TPOPERG75
165500130925     C                   Eval      NATOPR0502  = NATOPERG75
165600130906     C                   Eval      LEASIR0502  = LEASINRG75
165700130906     C                   Eval      DTREGR0502  = DTREGRG75
165800130906     C                   Eval      NRREGR0502  = NRREGRG75
165900130906     C                   Eval      SERRER0502  = SERREGRG75
166000110824     C*
166100130912     C                   if        DaSoggetto  = *On
166200130912     C                   Eval      CogRaR0502  = *Hival
166300130912     C                   Eval      NomeR0502   = *Hival
166400130912     C                   Eval      DtNasR0502  = *Hival
166500130912     C                   Eval      LocNaR0502  = *Hival
166600130912     C                   Eval      PrvNaR0502  = *Hival
166700130912     C                   Eval      STATOR0502  = *Hival
166800130912     C                   Eval      CodUIR0502  = *Hival
166900130912     C                   Eval      LocalR0502  = *Hival
167000130912     C                   Eval      IndiER0502  = *Hival
167100130912     C                   Eval      TpNR0502    = *Hival
167200130912     C                   Eval      PIVAR0502   = *Hival
167300130912     C                   Eval      CDFISR0502  = *Hival
167400130925     C                   Eval      TpOPER0502  = *Hival
167500130925     C                   Eval      NATOPR0502  = *Hival
167600130912     C                   Eval      LEASIR0502  = *Hival
167700130912     C                   Eval      DTREGR0502  = *Hival
167800130912     C                   Eval      NRREGR0502  = *Hival
167900130912     C                   Eval      SERRER0502  = *Hival
168000110824     C                   endif
168100000000     C*
168200130906     C                   MOVE      $ORD          ORD0502
168300000000     C* richiedo al pgm di lettura il 1° rcd con la conseguente
168400000000     C* inizializzazione delle variabili SQL
168500130906     C                   MOVE      '0'           RIC0502
168600130906     C                   MOVE      '05'          OPZ0502
168700130906     C                   MOVE      '01'          TPROTT0502
168800130906     C                   MOVE      '03'          ROTT0502
168900130906     C                   MOVE      '01'          TPLIST0502
169000000000     C*
169100000000     C                   SELECT
169200000000     C*
169300000000     C* Gestione posizionamento
169400000000     C* Se il campo W1POS è > *blank imposto il posizionamento con
169500000000     C* W1POS altrimenti lo imposto con il valore di parametrizzazione
169600000000     C*
169700000000    >C* Ord.1 = chiave ordinamento 1
169800000000     C     $ORD          WHENEQ    1
169900130906    >C                   MOVEL     W1POS         POS0502
170000000000     C*
170100000000     C* Ord.99= Chiave univoca per la POSTA
170200000000     C* (quando viene gestita la chiave univoca NON deve essere gestito
170300000000     C*  il posizionamento C1POS/W1POS)
170400000000     C     $ORD          WHENEQ    99
170500000000     C*
170600000000     C                   ENDSL
170700000000     C*
170800000000     C                   ENDSR
170900000000     C/EJECT
171000000000     C************************************************************
171100110825     C* LETTURA RCD ARCHIVIO PILOTA e scrittura su wrkfile
171200000000     C************************************************************
171300110825p    C     REDANA1       BEGSR
171400000000     C*
171500000000     C                   MOVEL     *OFF          $EndFile
171600000000     C                   MOVEL     *OFF          $RCDOK
171700000000     C*
171800000000     C     $EndFile      DOUEQ     *ON
171900000000     C     $RCDOK        OREQ      *ON
172000000000     C*
172100000000     C                   SELECT
172200000000     C*
172300000000     C     $ORD          WHENEQ    1
172400000000     C*
172500130906     C                   CALLB     'SDG00502R'
172600000000     C                   PARM                    KPJBA
172700130906     C                   PARM                    SDG00502DS
172800130906     C                   PARM                    SDG00592DS
172900130906     C                   PARM                    SDG005FADS
173000130906     C                   PARM                    SDG005SADS
173100130906     C                   PARM                    SDG005BLDS
173200130906     C                   PARM                    SDG005FEDS
173300130906     C                   PARM                    SDG005FRDS
173400130906     C                   PARM                    SDG005NEDS
173500130906     C                   PARM                    SDG005NRDS
173600130906     C                   PARM                    SDG005DFDS
173700130906     C                   PARM                    SDG005FNDS
173800130906     C                   PARM                    SDG005SEDS
173900131017R136 C                   PARM                    SDG005TUDS
174000130906     C                   PARM                    SDG005TADS
174100000000     C*
174200000000     C* richiedo al pgm di lettura il rcd successivo
174300130906     C                   MOVE      '1'           RIC0502
174400000000     C*
174500130906     C                   MOVE      END0502       $EndFile
174600000000     C*
174700000000     C     $ORD          WHENEQ    99
174800000000     C* nel caso di lettura dati POSTA, basta leggere in READE con la
174900000000     C* chiave precedentemente riempita nella routine SetAna
175000000000     C*
175100000000     C                   ENDSL
175200000000     C*
175300000000     C     $EndFile      IFEQ      *OFF
175400000000     C                   EXSR      SELANA
175500110825p    C                   If        $RCDOK        = *On
175600130906p    C                   Exsr      ScriviSCFW
175700110825p    C                   Eval      $RCDOK        = *Off
175800110825p    C                   EndIf
175900110825     C*
176000000000     C                   ENDIF
176100000000     C*
176200000000     C                   ENDDO
176300000000     C*
176400000000     C                   MOVE      $EndFile      *IN33                          SflEnd
176500000000     C*
176600000000     C                   ENDSR
176700000000     C/EJECT
176800110825p    C************************************************************
176900110825p    C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
177000110825p    C************************************************************
177100110825p    C     SETANA        BEGSR
177200110825     C*
177300110825     C                   SELECT
177400110825     C*
177500110825     C* Gestione posizionamento
177600110825     C* Se il campo W1POS è > *blank imposto il posizionamento con
177700110825     C* W1POS altrimenti lo imposto con il valore di parametrizzazione
177800110825     C*
177900110825     C* Ord.1 = chiave ordinamento 1
178000110825     C     $ORD          WHENEQ    1
178100130906     C     xscsoc        SETLL     SDGSCF00W
178200110825     C                   ENDSL
178300110825     C*
178400110825     C                   ENDSR
178500110825     C/EJECT
178600110825p    C************************************************************
178700110825p    C* LETTURA RCD ARCHIVIO PILOTA
178800110825p    C************************************************************
178900110825p    C     REDANA        BEGSR
179000110825     C*
179100110825     C                   MOVEL     *OFF          $EndFile
179200110825     C                   MOVEL     *OFF          $RCDOK
179300110825     C*
179400110825     C     $EndFile      DOUEQ     *ON
179500110825     C     $RCDOK        OREQ      *ON
179600110825     C*
179700110825     C                   SELECT
179800110825     C*
179900110825     C     $ORD          WHENEQ    1
180000130906     C                   READ      SDGSCF00W                              21
180100110825     C                   MOVE      *IN21         $EndFile
180200110825     C*
180300110825     C     $ORD          WHENEQ    99
180400110825     C* nel caso di lettura dati POSTA, basta leggere in READE con la
180500110825     C* chiave precedentemente riempita nella routine SetAna
180600110825     C*
180700110825     C                   ENDSL
180800110825     C*
180900110825     C                   MOVEL     *ON           $RCDOK
181000110825     C*
181100110825     C                   ENDDO
181200110825     C*
181300110825     C                   MOVE      $EndFile      *IN33                          SflEnd
181400110825     C*
181500110825     C                   ENDSR
181600110825     C/EJECT
181700000000     C************************************************************
181800000000     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
181900000000     C************************************************************
182000000000     C     CLCPAG        BEGSR
182100000000     C* Input :
182200000000     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
182300000000     C*          cursore
182400000000     C* - SFLPAG = numero rcd per pagina sfl
182500000000     C* Output :
182600000000     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
182700000000     C*
182800000000     C     WSFL          DIV       SFLPAG        PAGINE            4 0
182900000000     C                   MVR                     RESTO             3 0
183000000000     C     RESTO         IFGT      0
183100000000     C                   ADD       1             PAGINE
183200000000     C                   ENDIF
183300000000     C     PAGINE        MULT      SFLPAG        WPAG
183400000000     C*
183500000000     C                   ENDSR
183600000000     C/EJECT
183700000000     C************************************************************
183800000000     C* SELEZIONI SULLA RIGA LETTA
183900000000     C* impostare a *OFF il campo $RCDOK per scartare un record che
184000000000     C* non rientra nei parametri di parzializzazione
184100000000     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
184200000000     C* superiore stabilito nei parametri
184300000000     C*
184400000000     C************************************************************
184500000000     C     SELANA        BEGSR
184600000000     C*
184700000000     C                   MOVEL     *ON           $RCDOK
184800000000     C                   MOVE      *hival        ALL9             15
184900000000     C*
185000000000     C                   IF        $RcdOk = *off
185100000000     C*
185200000000     C                   ENDIF
185300000000     C*
185400000000     C                   ENDSR
185500000000     C/EJECT
185600110825     C************************************************************
185700110825     C* RIEMPIMENTO RIGA LISTA
185800110825     C************************************************************
185900110825     C     RIES1         BEGSR
186000110825     C*
186100110825     C                   CLEAR                   FMTS1
186200110825     C*
186300110825     C* riempio i campi del sfl, compresi gli hidden con i valori
186400110825     C* del rcd letto
186500130919     C*
186600130919     C* se nota di variazione, sottolineo riferimenti documento
186700130919     C                   if        SCFTpDoc    = 'N'
186800130919     C                   Eval      *In12       = *On
186900130919     C                   endif
187000130919     C*
187100130906     C                   Eval      H1Sys       = SCFSys
187200130906     C                   Eval      H1NrAsReg   = SCFNrAsReg
187300130906     C                   Eval      H1NrRigaM   = SCFNrRigaM
187400130906     C                   Eval      H1NrRiga    = SCFNrRiga
187500130906     C                   Eval      H1TpNR      = SCFTpNR
187600130906     C                   Eval      H1CogRagR   = SCFCogRagR
187700130906     C                   Eval      H1NomeR     = SCFNomeR
187800130906     C                   Eval      H1DTNASR    = SCFDTNASR
187900130906     C                   Eval      H1LOCNASR   = SCFLOCNASR
188000130906     C                   Eval      H1PRVNASR   = SCFPRVNASR
188100130906     C                   Eval      H1STATOR    = SCFSTATOR
188200130906     C                   Eval      H1CODUICR   = SCFCODUICR
188300130906     C                   Eval      H1LOCALIR   = SCFLOCALIR
188400130906     C                   Eval      H1INDIRER   = SCFINDIRER
188500130906     C                   Eval      H1PIVAR     = SCFPIVAR
188600130906     C                   Eval      H1CDFISCR   = SCFCDFISCR
188700130906     C                   Eval      H1NATOPER   = SCFNATOPER
188800130906     C                   Eval      H1LEASING   = SCFLEASINR
188900130906     C                   Eval      H1DTREGR    = SCFDTREGR
189000130906     C                   Eval      H1NRREGR    = SCFNRREGR
189100130906     C                   Eval      H1SERREGR   = SCFSERREGR
189200130906     C                   Eval      H1TpDoc     = SCFTpDoc
189300110825     C*
189400130906     C                   MoveL     SCFKcc        S1Kcc
189500130906     C                   MoveL     SCFKsc        S1Ksc
189600130906     C                   if        SCFDtDoc   <> *Loval
189700130906     C     *Jobrun       Move      SCFDtDoc      S1DtDoc
189800110825     C                   endif
189900130906     C                   Z-Add     SCFNrDoc      S1NrDoc
190000130906     C                   Z-Add     SCFNrDoc      H1NrDoc
190100130906     C                   MoveL     SCFSerDoc     H1SerDoc
190200130906     C                   if        SCFDtReg   <> *Loval
190300130906     C     *Jobrun       Move      SCFDtReg      S1DtReg
190400110825     C                   endif
190500130906     C                   Z-Add     SCFNrReg      S1NrReg
190600130906     C                   Z-Add     SCFNrReg      H1NrReg
190700130906     C                   MoveL     SCFSerReg     H1SerReg
190800130906     C                   MoveL     SCFOrigine    S1Origine
190900110825     C*
191000110825     C* azzero le memorizzazioni degli attributi non bottoni
191100110825     C                   MOVEL     *Blank        H1ANNA
191200130906     C                   if        SCFAnn      = '1'       or
191300130906     C                             SCFAnn      = '9'
191400110825     C                   MoveL     *ON           H1ANNA
191500110825     C                   endif
191600110825     C*
191700130918     C                   Z-Add     SCFImpOpe     S1ImpOpe
191800130918     C                   Z-Add     SCFIVAOpe     S1IVAOpe
191900130906     C                   MoveL     SCFCausale    S1Causale
192000130906     C                   Z-Add     SCFAliq       S1Aliq
192100130906     C                   MoveL     SCFRifIVA     S1RifIVA
192200110825     C*
192300110825     C* imposto quegli attributi del sfl che sono modificabili
192400110825     C* al richiamo del determinato pgm
192500110825     C                   EXSR      ATRS1
192600110825     C*
192700110825     C                   ENDSR
192800110825     C/EJECT
192900000000     C************************************************************
193000000000     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
193100000000     C************************************************************
193200000000     C     ATRS1         BEGSR
193300000000     C*
193400000000     C* in questa routine bisogna attivare indicatori o campi programma
193500000000     C* che gestiscono gli attributi di uno o più campi di questa videata;
193600000000     C* attenzione : non è questo il posto in cui eseguire controlli,
193700000000     C*  dovrebbero già arrivare dei flag
193800000000     C*
193900000000     C* azzero gli attributi non bottoni
194000000000     C                   MOVEL     X'A0'         S1ANNA
194100000000     C*
194200000000     C     H1ANNA        IFEQ      *ON
194300000000     C                   MOVEL     ANNUL         S1ANNA
194400000000     C                   ENDIF
194500130919     C*
194600130919     C* se nota di variazione, sottolineo riferimenti documento
194700130919     C                   if        H1TpDoc     = 'N'
194800130919     C                   Eval      *In12       = *On
194900130919     C                   else
195000130919     C                   Eval      *In12       = *Off
195100130919     C                   endif
195200130919     C*
195300000000     C*
195400000000     C                   ENDSR
195500000000     C/EJECT
195600000000     C************************************************************
195700000000     C* GESTIONE F08 VIDEO S1
195800000000     C************************************************************
195900000000     C     F08S1         BEGSR
196000000000     C*
196100000000     C                   MOVE      *ON           $FINE
196200130906    >C                   MOVE      '0'           RETG75
196300000000     C*
196400000000     C                   ENDSR
196500000000     C/EJECT
196600000000     C************************************************************
196700000000     C* GESTIONE F03 VIDEO S1
196800000000     C************************************************************
196900000000     C     F03S1         BEGSR
197000000000     C*
197100000000     C                   MOVE      *ON           $FINE
197200130906    >C                   MOVE      '1'           RETG75
197300000000     C* fine programma
197400000000     C                   EXSR      ENDPGM
197500000000     C*
197600000000     C                   ENDSR
197700000000     C/EJECT
197800130924     C************************************************************
197900130924     C* GESTIONE F05 VIDEO S1
198000130924     C************************************************************
198100130924     C     F05S1         BEGSR
198200130924     C*
198300130924     C                   MOVE      *ON           $INZS1
198400130924     C*
198500130924     C                   ENDSR
198600130924     C/EJECT
198700130924     C************************************************************
198800000000     C* GESTIONE F10 VIDEO S1
198900000000     C* AGGIUNTA RECORD
199000000000     C************************************************************
199100000000     C     F10S1         BEGSR
199200000000     C*
199300130906     C                   RESET                   SDGSG76DS
199400130924     C*
199500130906     C                   MOVE      xscsoc        SocietaG76
199600130906     C                   MOVEL     '01'          OPZG76
199700130906     C     CMTG75        IFEQ      '1'
199800130906     C                   MOVEL     '1'           CMTG76
199900000000     C                   ELSE
200000130906     C                   MOVEL     '0'           CMTG76
200100000000     C                   ENDIF
200200000000     C*
200300130906     C                   Move      XScSoc        SocietaG76
200400130906     C                   Eval      AnnoG76     = C1Anno
200500140401     C                   Eval      MeseG76     = 0
200600140401R133 C*
200700140401R133 C                   Select
200701151201R195 C* Acquisti da San Marino
200800140401R133 C                   When      TpComG75    = 'S'
200900140401R133 C                   Eval      TpComG76    = 'S'
201000140401R133 C                   Eval      MeseG76     = MeseG75
201001151201R195 C* Black List
201002151201 "   C                   When      TpComG75    = 'B'
201003151201 "   C                   Eval      TpComG76    = 'B'
201004151201R195 C                   Eval      MeseG76     = 0
201100140401R133 C                   Other
201101151201R195 C* Spesometro
201200140401R133 C                   Eval      TpComG76    = 'E'
201201151201R195 C                   Eval      MeseG76     = 0
201300140401R133 C                   EndSl
201400140401R133 C*
201500130906     C                   Eval      CFG76       = *Blank
201600130924     C                   Eval      TpNG76      = *Blank
201700130906     C                   Eval      SoggG76     = *Blank
201800131206R141 C                   Eval      CausaleG76  = *Blank
201900130906     C                   Eval      PartIvaG76  = C1PartIva
202000130906     C                   Eval      CdFiscG76   = C1CdFisc
202100130906     C                   Eval      CogRagG76   = C1CogRag
202200000000     C*
202300130906     C                   Eval      NomeG76     = H1Nome
202400130906     C                   Eval      DtNascG76   = H1DtNasc
202500130906     C                   Eval      LocNascG76  = H1LocNasc
202600130906     C                   Eval      PrvNascG76  = H1PrvNasc
202700130906     C                   Eval      StatoG76    = C1Stato
202800130924     C                   Eval      CodUICG76   = C1CodUIC
202900130906     C                   Eval      LocalitG76  = H1Localit
203000130906     C                   Eval      IndirEG76   = H1IndirE
203100130924     C*
203200130924     C                   Eval      QuadroG76   = QuadroG75
203300000000     C*
203400000000     C* passare anche le parzializzazioni ricevute relative alla chiave
203500000000     C* univoca :
203600000000     C*  se ce ne sono, potranno essere usate come dft d'immissione
203700130906    >C                   IF        OPZG75 <> *Blank
203800000000     C                   ENDIF
203900000000     C*
204000130906    >C                   CALL      'SDGSG76R'
204100000000     C                   PARM                    KPJBA
204200130906    >C                   PARM                    SDGSG76DS
204300000000     C* ritorno da PGM gestione
204400130906    >C                   EXSR      RTNG76
204500000000     C     WINZS1        IFEQ      *ON
204600000000     C                   MOVE      *ON           $INZS1
204700000000     C* se esistevano già righe sul sfl
204800000000     C* calcolo pagina a cui deve posizionarsi
204900000000     C     WSFL          IFGT      0
205000000000     C                   EXSR      CLCPAG
205100000000     C* altrimenti carico solo la 1a pagina
205200000000     C                   ELSE
205300000000     C                   Z-ADD     SFLPAG        WPAG
205400000000     C                   ENDIF
205500000000     C                   ENDIF
205600000000     C*
205700000000     C                   MOVE      *HIVAL        $LASTV
205800000000     C*
205900000000     C                   ENDSR
206000000000     C/EJECT
206100000000     C************************************************************
206200000000     C* GESTIONE F12 VIDEO S1
206300000000     C************************************************************
206400000000     C     F12S1         BEGSR
206500000000     C*
206600000000     C*  se la 1a videata sono le parzializzazioni, per F12 torno a queste
206700000000     C     $LOP          IFEQ      1
206800000000     C                   MOVE      'D1'          $GEST
206900000000     C*  se la 1a videata è questa, per F12 torno a menu
207000000000     C                   ELSE
207100000000     C                   MOVE      *ON           $FINE
207200130906    >C                   MOVE      '2'           RETG75
207300000000     C                   ENDIF
207400000000     C*
207500000000     C                   ENDSR
207600000000     C/EJECT
207700000000     C************************************************************
207800000000     C* GESTIONE F17 VIDEO S1
207900000000     C************************************************************
208000000000     C     F17S1         BEGSR
208100000000     C*
208200000000     C                   MOVE      'D1'          $GEST
208300000000     C* se il pgm è stato richiamato con chiave univoca
208400000000     C* e dopo è stata richiesta una parzializzazione
208500000000     C* tolgo la gestione della chiave univoca (azzere/abblenco il
208600000000     C* valore; propongo come ordinamento il 1°)
208700000000     C     $Unique       IFEQ      *ON
208800000000     C                   MOVE      *OFF          $Unique
208900000000     C                   ENDIF
209000000000     C     $ORD          IFEQ      99
209100000000     C                   Z-ADD     1             $ORD
209200000000     C                   MOVE      *ON           $INZD1
209300000000     C                   ENDIF
209400000000     C*
209500000000     C                   ENDSR
209600000000     C/EJECT
209700000000     C************************************************************
209800000000     C* GESTIONE F18 VIDEO S1
209900000000     C************************************************************
210000000000     C     F18S1         BEGSR
210100000000     C*
210200000000     C                   MOVE      'D2'          $GEST
210300000000     C                   MOVEL     *ON           $INZD2
210400000000     C     $ORD          IFEQ      99
210500000000     C                   Z-ADD     1             $ORD
210600000000     C                   ENDIF
210700000000     C*
210800000000     C                   ENDSR
210900000000     C/EJECT
211000000000     C************************************************************
211100000000     C* GESTIONE F23 VIDEO S1
211200000000     C************************************************************
211300000000     C     F23S1         BEGSR
211400000000     C*
211500000000     C                   Z-ADD     $C1Op1        $L01
211600000000     C                   Z-ADD     $C1Op2        $L02
211700000000     C                   EXSR      RIEOPZ
211800000000     C                   Z-ADD     23            X
211900000000     C                   MOVE      FF23          $FC2(X)
212000000000     C                   MOVEL     WOPZ1         C1OPZ1
212100000000     C                   MOVEL     $ALLOPT       H1ALLOPT
212200000000     C                   MOVEL     WOPZ2         C1OPZ2
212300000000     C                   Z-ADD     WCOZ          C1COZ
212400000000     C*
212500000000     C                   ENDSR
212600000000     C/EJECT
212700000000     C************************************************************
212800000000     C* GESTIONE F24 VIDEO S1
212900000000     C************************************************************
213000000000     C     F24S1         BEGSR
213100000000     C*
213200000000     C                   MOVEA     $FX2          $FX
213300000000     C                   MOVEA     $FC2          $FC
213400000000     C                   MOVEA     $FR2          $FR
213500000000     C                   Z-ADD     $ULKS1        $ULK
213600000000     C                   Z-ADD     $S1L01        $L01
213700000000     C                   Z-ADD     $S1L02        $L02
213800000000     C                   EXSR      RIEKEY
213900000000     C                   Z-ADD     $ULK          $ULKS1            3 0
214000000000     C                   MOVEA     $FC           $FC2
214100000000     C                   MOVEL     $KEY1         Z2KE1
214200000000     C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
214300000000     C                   MOVEL     $KEY2         Z2KE2
214400000000     C                   WRITE     FMTZ2
214500000000     C*
214600000000     C                   ENDSR
214700000000     C/EJECT
214800000000     C************************************************************
214900000000     C* CONTROLLO TESTATA LISTA
215000000000     C************************************************************
215100000000     C     CTRC1         BEGSR
215200000000     C*
215300000000     C                   MOVE      *OFF          *IN99
215400000000     C*
215500000000     C     C1POS         IFNE      W1POS
215600000000     C                   EXSR      POSS1
215700000000     C                   ENDIF
215800000000     C*
215900000000     C                   ENDSR
216000000000     C/EJECT
216100000000     C************************************************************
216200000000     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
216300000000     C************************************************************
216400000000     C     POSS1         BEGSR
216500000000     C*
216600000000     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
216700000000     C*
216800000000   B1C     WMAX          IFGT      0
216900000000     C                   READC     FMTS1                                  21
217000000000   B2C     *IN21         DOWEQ     *OFF
217100000000     C     *IN99         ANDEQ     *OFF
217200000000     C*
217300000000   B3C     S1OPZ         IFNE      0
217400000000     C                   SETON                                        92  99
217500000000   E3C                   ENDIF
217600000000     C* se rilevata opzione attiva, attivo sflnxtchg
217700000000   B3C     *IN99         IFEQ      *ON
217800000000     C                   MOVE      *ON           *IN32
217900000000     C*
218000000000     C* imposto quegli attributi del sfl che sono modificabili
218100000000     C* al richiamo del determinato pgm
218200000000     C                   EXSR      ATRS1
218300000000     C*
218400000000     C                   UPDATE    FMTS1
218500000000   X3C                   ELSE
218600000000     C                   READC     FMTS1                                  21
218700000000   E3C                   ENDIF
218800000000     C*
218900000000   E2C                   ENDDO
219000000000     C*
219100000000   E1C                   ENDIF
219200000000     C*
219300000000     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
219400000000     C* AL VALORE DI POSIZIONAMENTO :
219500000000     C* se questo è maggiore del valore del rispettivo campo di
219600000000     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
219700000000     C* con i valori di parzializzazione.
219800000000     C*
219900000000   B1C     *IN99         IFEQ      *OFF
220000000000    >C                   MOVE      *BLANK        W1POS
220100000000     C*
220200000000     C* per ogni ordinamento devo testare se il campo dell'inizio elenco
220300000000     C* è maggiore del relativo valore di parzializzazione, nel qual caso
220400000000     C* imposto la variabile del posizionamento (W1Pos) al nuovo valore;
220500000000     C* richiedo comunque un ricaricamento del sfl che, se non si è
220600000000     C* verificata la condizione suddetta avverrà con posizionamento a
220700000000     C* partire dal valore di parzializzazione
220800000000     C*
220900000000   B2C                   SELECT
221000000000    >C* Attenzione : se un ordinamento ha un campo di posizionamento
221100000000    >C* specifico bisogna sostituire la gestione W1Pos/C1Pos con quella
221200000000    >C* dei relativi campi
221300000000     C*
221400000000     C     $ORD          WHENEQ    1
221500000000     C*
221600000000     C* per l'ordinamento 99 = POSTA non viene gestito il posizionamento
221700000000     C*
221800000000   E2C                   ENDSL
221900000000     C*
222000000000     C* richiedo l'inizializzazione sfl
222100000000     C                   MOVE      *ON           $INZS1
222200000000     C*
222300000000   E1C                   ENDIF
222400000000     C*
222500000000     C                   ENDSR
222600000000     C/EJECT
222700000000     C************************************************************
222800000000     C* CONTROLLO OPZIONI LISTA
222900000000     C************************************************************
223000000000     C     CTRS1         BEGSR
223100000000     C*
223200000000     C                   MOVEL     *OFF          $ESCI
223300000000     C                   SETOFF                                       99
223400000000     C                   Z-ADD     0             S1OPZ
223500000000     C*
223600000000     C* Leggo il sfl solo se ci sono rcd
223700000000   B1C     WMAX          IFGT      0
223800000000     C                   READC     FMTS1                                  21
223900000000     C*
224000000000     C* esce se fine sfl o errore che richiede l'uscita
224100000000   B2C     *IN21         DOWEQ     *OFF
224200000000     C     $ESCI         ANDEQ     *OFF
224300000000     C                   Z-ADD     S1NRR         C1RCD
224400000000     C* ctrl su riga
224500000000     C                   EXSR      RECS1
224600000000     C* gestione opzioni
224700000000   B3C     S1OPZ         IFNE      0
224800000000     C     *IN99         ANDEQ     *OFF
224900000000     C                   EXSR      OPZS1
225000000000   E3C                   ENDIF
225100000000     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
225200000000   B3C     *IN99         IFEQ      *ON
225300000000     C                   MOVE      *ON           *IN32
225400000000   E3C                   ENDIF
225500000000     C*
225600000000     C* imposto quegli attributi del sfl che sono modificabili
225700000000     C* al richiamo del determinato pgm
225800000000     C                   EXSR      ATRS1
225900000000     C*
226000000000     C                   Z-ADD     0             S1OPZ
226100000000     C*
226200000000     C                   UPDATE    FMTS1
226300000000     C                   MOVE      *OFF          *IN32
226400000000     C*
226500000000     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
226600000000   B3C     $ESCI         IFEQ      *OFF
226700000000     C                   READC     FMTS1                                  21
226800000000     C* a fine ciclo ripristino il flag richiesta iniz.sfl
226900000000   B4C     *IN21         IFEQ      *ON
227000000000     C                   MOVE      WINZS1        $INZS1
227100000000     C                   MOVE      *off          WINZS1
227200000000     C* se è avvenuta almeno una modifica
227300000000   B5C                   IF        $INZS1 = *on
227400000000     C* calcolo pagina a cui deve posizionarsi
227500000000     C     WSFL          IFGT      0
227600000000     C                   EXSR      CLCPAG
227700000000     C                   ELSE
227800000000     C                   Z-ADD     SFLPAG        WPAG
227900000000     C                   ENDIF
228000000000   E5C                   ENDIF
228100000000   E4C                   ENDIF
228200000000   E3C                   ENDIF
228300000000     C*
228400000000   E2C                   ENDDO
228500000000     C*
228600000000   E1C                   ENDIF
228700000000     C*
228800000000     C                   ENDSR
228900000000     C/EJECT
229000000000     C************************************************************
229100000000     C* CONTROLLO CAMPI I/O RIGA LISTA
229200000000     C************************************************************
229300000000     C     RECS1         BEGSR
229400000000     C*
229500000000     C* reset indicatori DSPATR
229600000000     C                   MOVE      *ALL'0'       IN4049           10
229700000000     C                   MOVEA     IN4049        *IN(40)
229800000000     C*
229900000000     C* Controllo se Utente Autorizzato alla scelta
230000000000     C     S1OPZ         IFNE      0
230100000000     C                   Z-ADD     1             X
230200000000     C     S1OPZ         LOOKUP    $OP1(X)                                21
230300000000     C* mi salvo l'indice dell' opzione
230400000000     C   21              Z-add     X             PosOpz            2 0
230500000000     C     *IN21         IFEQ      *OFF
230600000000     C     $FL1(X)       OREQ      'N'
230700000000     C                   MOVE      *ON           $ESCI
230800000000     C                   SETON                                        409199
230900000000     C                   ENDIF
231000000000     C                   ENDIF
231100000000     C*
231200000000     C*
231300000000     C                   ENDSR
231400000000     C/EJECT
231500000000     C************************************************************
231600000000     C* GESTIONE OPZIONI LISTA
231700000000     C************************************************************
231800000000     C     OPZS1         BEGSR
231900000000     C*
232000000000     C                   Z-ADD     RIRI          CURR
232100000000     C                   Z-ADD     COCO          CURC
232200000000     C*
232300000000     C                   SELECT
232400000000     C*
232500000000     C* se opz=Selezione
232600000000     C* passo la chiave al pgm chiamante
232700000000     C     S1OPZ         WHENEQ    1
232800130906     C                   MOVE      S1OPZ         OPZG75
232900130906     C* passare nei campi appositi della DS SDGSG75DS il valore
233000000000     C* della chiave della VL univoca (se c'è)
233100000000     C                   MOVE      *ON           $ESCI
233200000000     C                   MOVE      *ON           $FINE
233300000000     C*
233400000000     C                   MOVE      *HIVAL        $LASTV
233500000000     C*
233600000000     C* Interrogazioni conto
233700000000     C     S1OPZ         WHENEQ    15
233800000000     C                   EXSR      Opzs1_15
233900000000     C*
234000000000     C* se opz=41=contabilità
234100000000     C     S1OPZ         WHENEQ    41
234200000000     C                   RESET                   NDC001DS
234300000000     C                   MOVEL     '02'          OPZ001
234400000000     C                   if        S1Origine   = 'A'
234500000000     C                   MOVEL     H1Sys         Uq1001
234600000000     C                   MOVEL     H1NrAsReg     Uq2001
234700000000     C*
234800000000     C                   MOVEL     H1Sys         SYS001
234900000000     C                   MOVEL     H1NrAsReg     NRASREG001
235000000000     C*
235100000000     C                   endif
235200000000     C*
235300000000     C                   if        S1DtReg    <> 0
235400000000     C     *Jobrun       Move      S1DtReg       DTREG1001
235500000000     C     *Jobrun       Move      S1DtReg       DTREG2001
235600000000     C                   endif
235700000000     C                   Z-Add     H1NrReg       NrReg001
235800000000     C                   MoveL     H1SerReg      SerReg001
235900000000     C*
236000000000     C                   if        S1DtDoc    <> 0
236100000000     C     *Jobrun       Move      S1DtDoc       DTDOC001
236200000000     C                   endif
236300000000     C                   Z-Add     H1NrDoc       NrDoc001
236400000000     C                   MoveL     H1SerDoc      SerDoc001
236500000000     C*
236600000000     C*
236700000000     C                   MoveL(P)  NDC001DS      KPJBU
236800000000     C                   CALL      'NDC001R'
236900000000     C                   PARM                    KPJBA
237000000000     C*
237100000000     C                   MoveL     KPJBU         NDC001DS
237200000000     C*
237300000000     C* ritorno da PGM gestione
237400000000     C                   EXSR      RTN001
237500000000     C                   MOVE      *HIVAL        $LASTV
237600000000     C*
237700000000     C* se opz=Annulla/Ripristina
237800000000     C     S1OPZ         WHENEQ    44
237900000000     C*
238000000000     C                   exsr      AnnullaRip
238100000000     C*
238200000000     C                   MOVE      *HIVAL        $LASTV
238300000000     C*
238400000000     C* se selezionata opzione personalizzata, richiamo il programma
238500000000     C* corrispondente
238600000000     C     $PGO1(PosOpz) Whenne    *Blank
238700130906    >C                   RESET                   SDGSG76DS
238800130906    >C                   MOVE      xscsoc        SocietaG76
238900130906    >C                   MOVEL     S1OPZ         OPZG76
239000130906    >C     CMTG75        IFEQ      '1'
239100130906    >C                   MOVEL     '1'           CMTG76
239200000000     C                   ELSE
239300130906    >C                   MOVEL     '0'           CMTG76
239400000000     C                   ENDIF
239500130906     C* passare nei campi appositi della DS SDGSG76DS il valore
239600000000     C* della chiave della VL univoca
239700000000     C*
239800130906    >C                   MOVEL     SDGSG76DS     KPJBU
239900000000     C                   CALL      $PGO1(PosOpz)                        22
240000000000     C                   PARM                    KPJBA
240100000000     C****               PARM                    NOMPGM
240200000000     C*
240300130906    >C                   MOVEL     KPJBU         SDGSG76DS
240400000000     C* ritorno da PGM
240500130906    >C                   EXSR      RTNG76
240600000000     C*
240700000000     C* se altre opzioni
240800000000     C* richiamo pgm gestione anagrafica
240900000000     C                   OTHER
241000000000     C*
241100130906     C                   RESET                   SDGSG76DS
241200000000     C*
241300130906     C                   MOVEL     S1OPZ         OPZG76
241400130906     C     CMTG75        IFEQ      '1'
241500130906     C                   MOVEL     '1'           CMTG76
241600000000     C                   ELSE
241700130906     C                   MOVEL     '0'           CMTG76
241800000000     C                   ENDIF
241900000000     C*
242000130906     C                   Move      XScSoc        SocietaG76
242100130906     C                   Eval      AnnoG76     = C1Anno
242200140401R133 C                   Eval      MeseG76     = 0
242300140401R133 C*
242400140401R133 C                   Select
242401151201R195 C* Acquisti da San Marino
242500140401R133 C                   When      TpComG75    = 'S'
242600140401R133 C                   Eval      TpComG76    = 'S'
242700140401R133 C                   Eval      MeseG76     = MeseG75
242701151201R195 C* Black List
242702151201 "   C                   When      TpComG75    = 'B'
242703151201 "   C                   Eval      TpComG76    = 'B'
242704151201R195 C                   Eval      MeseG76     = 0
242800140401R133 C                   Other
242801151201R195 C* Spesometro
242900140401R133 C                   Eval      TpComG76    = 'E'
242901151201R195 C                   Eval      MeseG76     = 0
243000140401R133 C                   EndSl
243100140401R133 C*
243200130906     C                   Eval      SoggG76     = *Blank
243300131206R141 C                   Eval      CausaleG76  = *Blank
243400130906     C                   Eval      CFG76       = *Blank
243500130906     C                   Eval      PartIvaG76  = C1PartIva
243600130906     C                   Eval      CdFiscG76   = C1CdFisc
243700130906     C                   Eval      CogRagG76   = C1CogRag
243800000000     C*
243900130906     C                   Eval      NomeG76     = H1Nome
244000130906     C                   Eval      DtNascG76   = H1DtNasc
244100130906     C                   Eval      LocNascG76  = H1LocNasc
244200130906     C                   Eval      PrvNascG76  = H1PrvNasc
244300130906     C                   Eval      StatoG76    = C1Stato
244400130924     C                   Eval      CodUICG76   = C1CodUIC
244500130906     C                   Eval      LocalitG76  = H1Localit
244600130906     C                   Eval      IndirEG76   = H1IndirE
244700000000     C*
244800130906     C                   Eval      SysG76      = H1Sys
244900130906     C                   Eval      NrAsRegG76  = H1NrAsReg
245000130906     C                   Eval      NrRigaMG76  = H1NrRigaM
245100130906     C                   Eval      NrRigaG76   = H1NrRiga
245200000000     C*
245300130906     C                   CALL      'SDGSG76R'
245400000000     C                   PARM                    KPJBA
245500130906    >C                   PARM                    SDGSG76DS
245600000000     C*
245700000000     C* ritorno da PGM gestione
245800130906     C                   EXSR      RTNG76
245900000000     C*
246000000000     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
246100000000     C     *IN99         IFEQ      *ON
246200000000     C                   SETON                                        40
246300000000     C                   ENDIF
246400000000     C*
246500000000     C                   MOVE      *HIVAL        $LASTV
246600000000     C                   ENDSL
246700000000     C*
246800000000     C                   ENDSR
246900000000     C/EJECT
247000000000     C************************************************************
247100000000     C* Interrogazioni conto
247200000000     C************************************************************
247300000000     C     OpzS1_15      BEGSR
247400000000     C*
247500000000     C                   Reset                   ANA399DS
247600000000     C*
247700000000     C                   Eval      A39Societa  = XScSoc
247800000000     C                   Eval      A39Ctb      = 'CG'
247900000000     C                   Eval      A39Kcc      = S1Kcc
248000000000     C                   Eval      A39Ksc      = S1Ksc
248100000000     C                   Eval      A39Iniz     = WA39Iniz
248200000000     C*
248300000000     C                   MoveL     ANA399DS      KPJBU
248400000000     C*
248500000000     C                   CALL      'ANA399R'                            21
248600000000     C                   PARM                    KPJBA
248700000000     C*
248800000000     C                   MoveL     KPJBU         ANA399DS
248900000000     C*
249000000000     C* metto 1 in  wa39iniz per non far fare più la close
249100000000     C                   Eval      WA39Iniz    = '1'
249200000000     C*
249300000000     C                   ENDSR
249400000000     C/EJECT
249500000000     C************************************************************
249600000000     C* GESTIONE RITORNO DA PGM primanota generica
249700000000     C************************************************************
249800000000     C     RTN001        BEGSR
249900000000     C*
250000000000     C* modo di ritorno
250100000000     C*
250200000000     C                   SELECT
250300000000     C* 1 = F3
250400000000     C     RET001        WHENEQ    '1'
250500000000     C                   MOVE      *ON           $ESCI
250600000000     C                   MOVE      *ON           $FINE
250700130906     C                   MOVE      '1'           RetG75
250800000000     C* 2 = F12
250900000000     C     RET001        WHENEQ    '2'
251000000000     C     opr001        ANDNE     '1'
251100000000     C                   MOVE      *ON           $ESCI
251200000000     C*
251300000000     C                   ENDSL
251400000000     C*
251500000000     C* operazione eseguite dal pgm chiamato
251600000000     C*
251700000000     C                   SELECT
251800000000     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
251900000000     C     OPR001        WHENEQ    '1'
252000000000     C                   MOVE      OPR001        WOPR                           avverto pgm chiamant
252100000000     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
252200000000     C*
252300000000     C                   ENDSL
252400000000     C*
252500000000     C* funzione non eseguibile per errore :
252600000000     C*
252700000000     C                   SELECT
252800000000     C* 1 = funzione richiamata chiusa in errore
252900000000     C     ERR001        WHENEQ    '1'
253000000000     C                   MOVE      *ON           $ESCI
253100000000     C                   SETON                                        9399
253200000000     C* 2 = funzione richiamata senza la dovuta autorizzazione
253300000000     C     ERR001        WHENEQ    '2'
253400000000     C                   MOVE      *ON           $ESCI
253500000000     C                   SETON                                        9499
253600000000     C*
253700000000     C                   ENDSL
253800000000     C*
253900000000     C                   ENDSR
254000000000     C/EJECT
254100000000     C************************************************************
254200000000     C* GESTIONE RITORNO DA PGM DI GESTIONE
254300000000     C************************************************************
254400130906    >C     RTNG76        BEGSR
254500000000     C*
254600000000     C* modo di ritorno
254700000000     C*
254800000000     C                   SELECT
254900000000     C* questi modi di utilizzo dei valori di ritorno dal
255000000000     C* pgm di manutenzione rcd di anagrafica sono delle
255100000000     C* proposte, normalmente sempre valide, ma modificabili
255200000000     C* per situazioni particolari
255300000000     C* 1 = F3
255400130906    >C     RETG76        WHENEQ    '1'
255500000000     C                   MOVE      *ON           $ESCI                          sospese READC
255600000000     C                   MOVE      *ON           $FINE                          fine pgm
255700130906    >C                   MOVE      '1'           RETG75
255800000000     C* 2 = F12
255900130906    >C     RETG76        WHENEQ    '2'
256000000000     C* se il pgm di manutenzione ha prima una guida, è necessario anche
256100130906     c* questo test, altrimenti prevale il retG76 e il sfl non viene
256200000000     C* ricaricato
256300130906    >C     OPRG76        ANDNE     '1'
256400000000     C*
256500000000     C* se l'opzione con cui è stato chiamato non è la modifica
256600000000     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
256700160418R221 X**** OPZG76        IFNE      '02'
256800160418 "   X****               MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
256900160418R221 X****               else
257000000000     C                   MOVE      *ON           $ESCI                          sospese READC
257100160418R221 X****               endif
257200000000     C*
257300000000     C                   ENDSL
257400000000     C*
257500000000     C* operazione eseguite dal pgm chiamato
257600000000     C*
257700000000     C                   SELECT
257800000000     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
257900130906    >C     OPRG76        WHENEQ    '1'
258000130906    >C                   MOVE      OPRG76        WOPR                           avverto pgm chiamant
258100000000     C*
258200000000     C* se l'opzione con cui è stato chiamato non è la modifica
258300000000     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
258400130906    >C     OPZG76        IFNE      '02'
258500000000     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
258600000000     C* se l'opzione con cui è stato chiamato è la modifica
258700000000     C* aggiorno subito la riga di sfl e non richiedo il ripristino sfl
258800000000     C                   ELSE
258900000000     C                   EXSR      AGGS1
259000000000     C                   ENDIF
259100000000     C*
259200000000     C                   ENDSL
259300000000     C*
259400000000     C* funzione non eseguibile per errore :
259500000000     C*
259600000000     C                   SELECT
259700000000     C* 1 = funzione richiamata chiusa in errore
259800130906    >C     ERRG76        WHENEQ    '1'
259900000000     C                   MOVE      *ON           $ESCI                          sospese READC
260000000000     C                   SETON                                        9399      emiss.SFLMSGID
260100000000     C* 2 = funzione richiamata senza la dovuta autorizzazione
260200130906    >C     ERRG76        WHENEQ    '2'
260300000000     C                   MOVE      *ON           $ESCI                          sospese READC
260400000000     C                   SETON                                        9499      emiss.SFLMSGID
260500000000     C*
260600000000     C                   ENDSL
260700000000     C*
260800000000     C                   ENDSR
260900000000     C/EJECT
261000000000     C************************************************************
261100000000     C* Annulla/Ripristina
261200000000     C************************************************************
261300000000     C     AnnullaRip    BEGSR
261400000000     C*
261500140422R133 C****               Eval      SCFTpCom    = 'E'
261600140422R133 C                   Eval      SCFTpCom    = TpComG75
261700130906     C                   Eval      SCFAnno     = D1Anno
261800140422R133 C*
261900140422R133 C                   Select
261901151201R195 C* Acquisti da San Marino
262000140422R133 C                   When      TpComG75    = 'S'
262100140422R133 C                   Eval      SCFMese     = MeseG75
262200140422R133 C                   Other
262201151201R195 C* Spesometro o Balck List
262300140422     C                   Eval      SCFMese     = 0
262400140422R133 C                   EndSl
262500140422R133 C*
262600130906     C                   Eval      SCFSys      = H1Sys
262700130906     C                   Eval      SCFNrAsReg  = H1NrAsReg
262800130906     C                   Eval      SCFNrRigaM  = H1NrRigaM
262900130906     C                   Eval      SCFNrRiga   = H1NrRiga
263000130906     C*
263100130906     C     K05SCF11      CHAIN     SDGSCF11L                          2122
263200130906     C                   if        *In21       = *Off
263300130906     C*
263400130906     C                   select
263500130906     C* Annulla
263600130906     C                   when      H1Anna      = *Blank
263700130906     C                   eval      SCFAnn      = '1'
263800131023R137 C                   Eval      SCFTpAnn    = '00'
263900130906     C* Ripristina
264000130906     C                   when      H1Anna      = '1'
264100130906     C                   eval      SCFAnn      = *Blank
264200131023R137 C                   Eval      SCFTpAnn    = *Blank
264300130906     C                   endsl
264400130906     C*
264500130906     C                   move      *on           WOPR
264600130906     C                   move      *on           WINZS1
264700130906     C*
264800130906     C                   Eval      SCFDtUlMo   = UDateIso
264900130906     C                   Eval      SCFUteUlMo  = XScCUt
265000130906     C                   Eval      SCFOrigine  = 'M'
265100130906     C*
265200130906     C                   UPDATE    SDGSCF000
265300000000     C*
265400000000     C                   endif
265500000000     C*
265600000000     C                   ENDSR
265700000000     C/EJECT
265800000000     C************************************************************
265900000000     C* AGGIORNAMENTO RIGA LISTA
266000000000     C************************************************************
266100000000     C     AGGS1         BEGSR
266200000000     C*
266300000000     C* riempio i campi del sfl che sono stati ripassati dal pgm
266400000000     C* di manutenzione dopo una opzione 02
266500130924     C*
266600130924     C* se nota di variazione, sottolineo riferimenti documento
266700130924     C                   if        TpDocG76    = 'N'
266800130924     C                   Eval      *In12       = *On
266900130924     C                   else
267000130924     C                   Eval      *In12       = *Off
267100130924     C                   endif
267200130924     C*
267300130924     C*
267400130924     C                   Eval      S1Kcc       = KccG76
267500130924     C                   Eval      S1Ksc       = KscG76
267600130924     C                   if        DtDocG76   <> *Loval
267700130924     C     *JobRun       Move      DtDocG76      S1DtDoc
267800130924     C                   endif
267900130924     C                   Eval      H1NrDoc     = NrDocG76
268000130924     C                   Eval      H1SerDoc    = SerDocG76
268100130924     C                   if        DtRegG76   <> *Loval
268200130924     C     *JobRun       Move      DtRegG76      S1DtReg
268300130924     C                   endif
268400130924     C                   Eval      H1NrReg     = NrRegG76
268500130924     C                   Eval      H1SerReg    = SerRegG76
268600130906     C                   Eval      S1Origine   = OrigineG76
268700130906     C                   Eval      H1Anna      = AnnG76
268800130918     C                   Eval      S1ImpOpe    = ImpOpeG76
268900130918     C                   Eval      S1IVAOpe    = IVAOpeG76
269000130918     C                   Eval      S1Causale   = CausaleG76
269100130906     C                   Eval      S1Aliq      = AliqG76
269200130906     C                   Eval      S1RifIVA    = RifIVAG76
269300000000     C*
269400000000     C                   ENDSR
269500000000     C/EJECT
269600000000     C************************************************************
269700000000     C* GESTIONE VALORI ASSUNTI
269800000000     C************************************************************
269900000000     C     GESD2         BEGSR
270000000000     C*
270100000000   B1C     $INZD2        IFEQ      *ON
270200000000     C                   EXSR      INZD2
270300000000     C                   MOVE      *OFF          $INZD2
270400000000   E1C                   ENDIF
270500000000     C*
270600000000   B1C     $LASTV        IFNE      'D2'
270700000000     C                   EXSR      GESPIE
270800000000   B1C     $LASTV        IFNE      *HIVAL
270900000000     C                   MOVE      $LASTV        $LASTG
271000000000     C                   ENDIF
271100000000     C                   MOVE      'D2'          $LASTV
271200000000   E1C                   ENDIF
271300000000     C*
271400000000     C* se esistono errori sulla videata
271500000000     C* emetto la write del formato a indicatori spenti per vedere
271600000000     C* le eventuali decodifiche
271700000000   B1C     *IN99         IFEQ      *ON
271800000000     C                   MOVEA     *IN           WIN
271900000000     C                   MOVE      *ALL'0'       IN5098           49
272000000000     C                   MOVEA     IN5098        *IN(50)
272100000000     C                   WRITE     FMTD2
272200000000     C                   MOVEA     WIN           *IN
272300000000   E1C                   ENDIF
272400000000     C*
272500000000     C                   EXFMT     FMTD2
272600000000     C*
272700000000     C* azzero la posizione del cursore
272800000000     C                   Z-ADD     0             H1RIGA
272900000000     C                   Z-ADD     0             H1COLO
273000000000     C*
273100000000     C* CONTROLLO ABILITAZIONE TASTI
273200000000     C* INPUT
273300000000     C* $FC3 = Abilitazioni videata D2
273400000000     C* OUTPUT
273500000000     C* *IN97 + *IN99 Errori
273600000000     C                   MOVEA     $FC3          $FC
273700000000     C                   EXSR      CTRKEY
273800000000     C*
273900000000   B1C     *IN99         IFEQ      *OFF
274000000000     C*
274100000000   B2C                   SELECT
274200000000     C* F3=Fine
274300000000     C     $TASTO        WHENEQ    F03
274400000000     C                   EXSR      F03D2
274500000000     C* F12=Ritorno
274600000000     C     $TASTO        WHENEQ    F12
274700000000     C                   EXSR      F12D2
274800000000     C* F24=Altri tasti
274900000000     C     $TASTO        WHENEQ    F24
275000000000     C                   EXSR      F24D2
275100000000     C*
275200000000     C                   OTHER
275300000000     C                   EXSR      CTRD2
275400000000   B3C     *IN99         IFEQ      *OFF
275500000000     C                   MOVE      $LASTG        $GEST
275600000000   B4C                   SELECT
275700000000     C     $LASTG        WHENEQ    'S1'
275800000000     C* se è stato modificato l'ordinamento di default, ricarico la lista
275900000000   B5C     $ORD          IFNE      D2ORD
276000000000     C                   MOVE      *ON           $INZS1
276100000000     C                   MOVE      *BLANK        W1POS
276200000000   E5C                   ENDIF
276300000000     C     $LASTG        WHENEQ    'D1'
276400000000     C* per ora niente
276500000000   E4C                   ENDSL
276600000000     C* definisco i nuovi valori di default
276700000000     C                   Z-ADD     D2LOP         $LOP
276800000000     C                   Z-ADD     D2ORD         $ORD
276900000000     C*
277000000000     C* aggiorno valori assunti
277100000000     C* se richiesto il salvataggio
277200000000   B4C     D2BSV         IFEQ      1
277300000000     C                   EXSR      AGGMPE
277400000000   E4C                   ENDIF
277500000000     C*
277600000000   E3C                   ENDIF
277700000000     C*
277800000000   E2C                   ENDSL
277900000000     C*
278000000000   E1C                   ENDIF
278100000000     C*
278200000000     C                   ENDSR
278300000000     C/EJECT
278400000000     C************************************************************
278500000000     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
278600000000     C************************************************************
278700000000     C     INZD2         BEGSR
278800000000     C*
278900000000     C                   CLEAR                   FMTD2
279000000000     C                   EVAL      D2BSVA = Normale
279100000000     C* se sono entrato con chiave univoca
279200000000     C* impedisco il salvataggio valori assunti
279300000000     C     $Unique       IFEQ      *ON
279400000000     C                   EVAL      D2BSVA = Protetto
279500000000     C                   ENDIF
279600000000     C                   MOVEL     $LOP          D2LOP
279700000000     C                   MOVE      $ORD          D2ORD
279800000000     C*
279900000000     C                   ENDSR
280000000000     C/EJECT
280100000000     C************************************************************
280200000000     C* GESTIONE F03 VIDEO D2
280300000000     C************************************************************
280400000000     C     F03D2         BEGSR
280500000000     C*
280600000000     C                   MOVE      *ON           $FINE
280700000000     C* fine programma
280800000000     C                   EXSR      ENDPGM
280900000000     C*
281000000000     C                   ENDSR
281100000000     C/EJECT
281200000000     C************************************************************
281300000000     C* GESTIONE F12 VIDEO D2
281400000000     C************************************************************
281500000000     C     F12D2         BEGSR
281600000000     C*
281700000000     C                   MOVE      $LASTG        $GEST
281800000000     C*
281900000000     C                   ENDSR
282000000000     C/EJECT
282100000000     C************************************************************
282200000000     C* GESTIONE F24 VIDEO D2
282300000000     C************************************************************
282400000000     C     F24D2         BEGSR
282500000000     C*
282600000000     C                   MOVEA     $FX3          $FX
282700000000     C                   MOVEA     $FC3          $FC
282800000000     C                   MOVEA     $FR3          $FR
282900000000     C                   Z-ADD     $ULKD2        $ULK
283000000000     C                   Z-ADD     $D2L01        $L01
283100000000     C                   Z-ADD     $D2L02        $L02
283200000000     C                   EXSR      RIEKEY
283300000000     C                   Z-ADD     $ULK          $ULKD2            3 0
283400000000     C                   MOVEA     $FC           $FC3
283500000000     C                   MOVEL     $KEY1         Z3KE1
283600000000     C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
283700000000     C                   WRITE     FMTZ3
283800000000     C*
283900000000     C                   ENDSR
284000000000     C/EJECT
284100000000     C************************************************************
284200000000     C* CONTROLLO VIDEATA VALORI ASSUNTI
284300000000     C************************************************************
284400000000     C     CTRD2         BEGSR
284500000000     C*
284600000000     C                   SETOFF                                       99
284700000000     C*
284800000000     C* OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
284900000000     C     D2LOP         IFEQ      0
285000000000     C                   SETON                                          5099
285100000000     C                   ENDIF
285200000000     C*
285300000000     C* OBBLIGATORIO VALORE PER ORDINAMENTO
285400000000     C     D2ORD         IFEQ      0
285500000000     C                   SETON                                          5199
285600000000     C                   ENDIF
285700000000     C*
285800000000     C                   ENDSR
285900000000     C/EJECT
286000000000     C************************************************************
286100000000     C* CONTROLLO ABILITAZIONE TASTI
286200000000     C* INPUT
286300000000     C* $TASTO =Tasto funzione premuto
286400000000     C* $FC = Tasti abilitati
286500000000     C* OUTPUT
286600000000     C* IN99 = ERrore
286700000000     C************************************************************
286800000000     C     CTRKEY        BEGSR
286900000000     C*
287000000000     C                   SETOFF                                       99
287100000000     C*
287200000000     C                   SELECT
287300000000     C     $TASTO        WHENEQ    F01
287400000000     C                   z-add     01            posfun
287500000000     C     $FC(1)        IFEQ      '0'
287600000000     C                   SETON                                        9997
287700000000     C                   ENDIF
287800000000     C     $TASTO        WHENEQ    F02
287900000000     C                   z-add     02            posfun
288000000000     C     $FC(2)        IFEQ      '0'
288100000000     C                   SETON                                        9997
288200000000     C                   ENDIF
288300000000     C     $TASTO        WHENEQ    F03
288400000000     C                   z-add     03            posfun
288500000000     C     $FC(3)        IFEQ      '0'
288600000000     C                   SETON                                        9997
288700000000     C                   ENDIF
288800000000     C     $TASTO        WHENEQ    F04
288900000000     C                   z-add     04            posfun
289000000000     C     $FC(4)        IFEQ      '0'
289100000000     C                   SETON                                        9997
289200000000     C                   ENDIF
289300000000     C     $TASTO        WHENEQ    F05
289400000000     C                   z-add     05            posfun
289500000000     C     $FC(5)        IFEQ      '0'
289600000000     C                   SETON                                        9997
289700000000     C                   ENDIF
289800000000     C     $TASTO        WHENEQ    F06
289900000000     C                   z-add     06            posfun
290000000000     C     $FC(6)        IFEQ      '0'
290100000000     C                   SETON                                        9997
290200000000     C                   ENDIF
290300000000     C     $TASTO        WHENEQ    F07
290400000000     C                   z-add     07            posfun
290500000000     C     $FC(7)        IFEQ      '0'
290600000000     C                   SETON                                        9997
290700000000     C                   ENDIF
290800000000     C     $TASTO        WHENEQ    F08
290900000000     C                   z-add     08            posfun
291000000000     C     $FC(8)        IFEQ      '0'
291100000000     C                   SETON                                        9997
291200000000     C                   ENDIF
291300000000     C     $TASTO        WHENEQ    F09
291400000000     C                   z-add     09            posfun
291500000000     C     $FC(09)       IFEQ      '0'
291600000000     C                   SETON                                        9997
291700000000     C                   ENDIF
291800000000     C     $TASTO        WHENEQ    F10
291900000000     C                   z-add     10            posfun
292000000000     C     $FC(10)       IFEQ      '0'
292100000000     C                   SETON                                        9997
292200000000     C                   ENDIF
292300000000     C     $TASTO        WHENEQ    F11
292400000000     C                   z-add     11            posfun
292500000000     C     $FC(11)       IFEQ      '0'
292600000000     C                   SETON                                        9997
292700000000     C                   ENDIF
292800000000     C     $TASTO        WHENEQ    F12
292900000000     C                   z-add     12            posfun
293000000000     C     $FC(12)       IFEQ      '0'
293100000000     C                   SETON                                        9997
293200000000     C                   ENDIF
293300000000     C     $TASTO        WHENEQ    F13
293400000000     C                   z-add     13            posfun
293500000000     C     $FC(13)       IFEQ      '0'
293600000000     C                   SETON                                        9997
293700000000     C                   ENDIF
293800000000     C     $TASTO        WHENEQ    F14
293900000000     C                   z-add     14            posfun
294000000000     C     $FC(14)       IFEQ      '0'
294100000000     C                   SETON                                        9997
294200000000     C                   ENDIF
294300000000     C     $TASTO        WHENEQ    F15
294400000000     C                   z-add     15            posfun
294500000000     C     $FC(15)       IFEQ      '0'
294600000000     C                   SETON                                        9997
294700000000     C                   ENDIF
294800000000     C     $TASTO        WHENEQ    F16
294900000000     C                   z-add     16            posfun
295000000000     C     $FC(16)       IFEQ      '0'
295100000000     C                   SETON                                        9997
295200000000     C                   ENDIF
295300000000     C     $TASTO        WHENEQ    F17
295400000000     C                   z-add     17            posfun
295500000000     C     $FC(17)       IFEQ      '0'
295600000000     C                   SETON                                        9997
295700000000     C                   ENDIF
295800000000     C     $TASTO        WHENEQ    F18
295900000000     C                   z-add     18            posfun
296000000000     C     $FC(18)       IFEQ      '0'
296100000000     C                   SETON                                        9997
296200000000     C                   ENDIF
296300000000     C     $TASTO        WHENEQ    F19
296400000000     C                   z-add     19            posfun
296500000000     C     $FC(19)       IFEQ      '0'
296600000000     C                   SETON                                        9997
296700000000     C                   ENDIF
296800000000     C     $TASTO        WHENEQ    F20
296900000000     C                   z-add     20            posfun
297000000000     C     $FC(20)       IFEQ      '0'
297100000000     C                   SETON                                        9997
297200000000     C                   ENDIF
297300000000     C     $TASTO        WHENEQ    F21
297400000000     C                   z-add     21            posfun
297500000000     C     $FC(21)       IFEQ      '0'
297600000000     C                   SETON                                        9997
297700000000     C                   ENDIF
297800000000     C     $TASTO        WHENEQ    F22
297900000000     C                   z-add     22            posfun
298000000000     C     $FC(22)       IFEQ      '0'
298100000000     C                   SETON                                        9997
298200000000     C                   ENDIF
298300000000     C     $TASTO        WHENEQ    F23
298400000000     C                   z-add     23            posfun
298500000000     C     $FC(23)       IFEQ      '0'
298600000000     C                   SETON                                        9997
298700000000     C                   ENDIF
298800000000     C     $TASTO        WHENEQ    F24
298900000000     C                   z-add     24            posfun
299000000000     C     $FC(24)       IFEQ      '0'
299100000000     C                   SETON                                        9997
299200000000     C                   ENDIF
299300000000     C                   ENDSL
299400000000     C*
299500000000     C                   ENDSR
299600000000     C/EJECT
299700000000     C************************************************************
299800000000     C* AGGIORNAMENTO VALORI ASSUNTI
299900000000     C************************************************************
300000000000     C     AGGMPE        BEGSR
300100000000     C*
300200000000     C* preparo i parametri di chiamata al driver
300300000000     C                   CLEAR                   XMPEDS
300400000000     C                   EVAL      TpChiamata = '03'
300500000000     C                   EVAL      NChiavi = 5
300600000000     C                   EVAL      VLogica = 2
300700000000     C*
300800000000     C* preparo le chiavi per la chiamata al driver
300900000000     C                   CLEAR                   XMPEDS1
301000000000     C                   MOVEL     XSCSOC        DSMPESOC
301100000000     C                   MOVEL     DSPGM         DSMPEPGM
301200000000     C                   MOVEL     KNMUS         DSMPEPRF
301300000000     C*
301400000000     C* Si memorizzano i dati di default a seconda delle modalità con
301500000000     C* cui il pgm è usato
301600000000     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
301700000000     C* MPETIP = 'PV'  pgm richiamato da menù
301800000000     C* MPETIP = 'PB'  x lancio Batch
301900000000     C*
302000000000     C                   SELECT
302100130906    >C     OPZG75        WHENEQ    '01'
302200000000     C                   MOVEL     'PX'          DSMPETIP
302300000000     C                   OTHER
302400000000     C                   MOVEL     'PV'          DSMPETIP
302500000000     C                   ENDSL
302600000000     C* dati
302700000000     C                   MOVEL     DSMPE         DSMPEPAR
302800000000     C*
302900000000     C                   CALLB     'XMPE'
303000000000     C                   PARM                    XMPEDS
303100000000     C                   PARM                    XMPEDS1
303200000000     C*
303300000000     C                   ENDSR
303400000000     C/EJECT
303500000000     C************************************************************
303600000000     C* OPERAZIONI INIZIALI
303700000000     C************************************************************
303800000000     C     *INZSR        BEGSR
303900000000     C*
304000000000     C* Reperimento parametri
304100000000     C*
304200000000     C     *ENTRY        PLIST
304300000000     C                   PARM                    KPJBA
304400130906     C                   PARM                    SDGSG75DS
304500000000     C*
304600000000     C*
304700000000     C* Reperimento dati società
304800000000     C*
304900000000     C                   MOVEL     'SOC001'      TIPXSC
305000000000     C                   MOVEL     *BLANK        SOCXSC
305100000000     C                   EXSR      REPSOC
305200000000     C*
305300000000     C* Controlli dati società
305400000000     C*
305500000000     C     RTNXSC        IFNE      '1'
305600000000     C                   MOVEL     XSOCDS        SOC001
305700000000     C                   ELSE
305800000000     C                   MOVE      *ON           $FINE
305900130906    >C                   MOVE      '1'           ErrG75
306000000000     C                   EXSR      ENDPGM
306100000000     C                   ENDIF
306200000000     C*
306300000000     C* reperisco le dimensioni delle variabili che conterranno i tasti
306400000000     C                   EVAL      $D1L01 = %Size(Z1Ke1)
306500000000     C                   EVAL      $D1L02 = %Size(Z1Ke2)
306600000000     C                   EVAL      $S1L01 = %Size(Z2Ke1)
306700000000     C                   EVAL      $S1L02 = %Size(Z2Ke2)
306800000000     C                   EVAL      $C1Op1 = %Size(C1Opz1)
306900000000     C                   EVAL      $C1Op2 = %Size(C1Opz2)
307000000000     C                   EVAL      $D2L01 = %Size(Z3Ke1)
307100000000     C                   EVAL      $D2L02 = 0
307200000000     C*
307300000000     C* Reperimento opzioni
307400000000     C*
307500000000     C                   EXSR      REPOPZ
307600000000     C*
307700000000     C* Reperimento tasti di funzione
307800000000     C*
307900000000     C                   EXSR      REPKEY
308000000000     C* Chiamata a routine per il reperimento delle opzioni
308100000000     C* personalizzate
308200000000    >C* se si vogliono abilitare i tasti personalizzati in ogni videata
308300000000    >C* ripeterla passando le schiere dei tasti funzione
308400000000     C                   CAllb     'X46USREXT'
308500000000     C                   PARM                    $OP1
308600000000     C                   PARM                    $TX1
308700000000     C                   PARM                    $FL1
308800000000     C                   PARM                    $PGO1
308900000000     C                   PARM      $dimOp        NumOpz
309000000000     C                   PARM                    $FX2
309100000000     C                   PARM                    $FC2
309200000000     C                   PARM                    $PGF2
309300000000     C                   PARM                    DsPgm
309400000000     C                   PARM                    xsccut
309500000000     C                   PARM                    xscgrp
309600000000     C*
309700000000     C                   Z-add     1             OpzLun
309800000000     C     *blank        Lookup    $TX1(OpzLun)                           21
309900000000     C                   if        *in21 = *on
310000000000     C                   sub       1             OpzLun
310100000000     C                   else
310200000000     C                   Z-add     $DimOp        OpzLun
310300000000     C                   endif
310400111214R078 C*
310500111214R078 C     *DMY          Move      UDate         UDateISO
310600000000     C*
310700000000     C                   ENDSR
310800000000     C/EJECT
310900110825     C************************************************************
311000110825     C* Gestione del file di lavoro con i dettagli selezionati
311100110825     C************************************************************
311200130906     C     GestSCF00W    BEGSR
311300110825     C*
311400130906     C                   CLOSE     SDGSCF00W
311500130906     C* cancello in QTEMP il file di lavoro SDGSCF00W
311600110825     C* con i dettagli selezionati precedentemente
311700130906     C                   EXSR      DelSCF00W
311800110825     C*
311900130906     C* creo in QTEMP il file di lavoro SDGSCF00W
312000130906     C                   EXSR      CrtSCF00W
312100130906     C                   OPEN      SDGSCF00W
312200110825     C*
312300110825     C                   ENDSR
312400110825     C/EJECT
312500110825     C************************************************************
312600110825     C* Cancellazione del file di lavoro con i dettagli selezionati
312700110825     C************************************************************
312800130906     C     DelSCF00W     BEGSR
312900110825     C*
313000110825     C                   Clear                   Cmd              80
313100110825     C*
313200110825     C                   Movel     'DltF'        Cmd
313300110825     C                   Cat       'FILE(':1     Cmd
313400110825     C                   Cat       'QTEMP/':0    Cmd
313500130906     C                   Cat       'SDGSCF00W':0 Cmd
313600110825     C                   Cat       ')':0         Cmd
313700110825     C*
313800110825     C                   Call      'QCMDEXC'                            22
313900110825     C                   Parm                    Cmd
314000110825     C                   Parm      80            $lenx            15 5
314100110825     C*
314200110825     C                   ENDSR
314300110825     C/EJECT
314400110825     C************************************************************
314500110825     C* Creazione del file di lavoro in QTEMP
314600110825     C************************************************************
314700130906     C     CrtSCF00W     Begsr
314800110825     C*
314900130906     C* CRTDUPOBJ  OBJ(SDGSCF00W) FROMLIB(&LIBR) OBJTYPE(*FILE) +
315000110825     C* TOLIB(QTEMP) DATA(*NO)
315100110825     C                   Clear                   Cmd
315200110825     C                   Movel     'CrtDupOBJ'   Cmd
315300110825     C                   Cat       'Obj(':1      Cmd
315400130906     C                   Cat       'SDGSCF00W':0 Cmd
315500110825     C                   Cat       ') FROMLIB(':0Cmd
315600110825     C                   Cat       '*LIBL)':0    Cmd
315700110825     C                   Cat       'ObjType(':1  Cmd
315800110825     C                   Cat       '*FILE)':0    Cmd
315900110825     C                   Cat       'TOLIB(':1    Cmd
316000110825     C                   Cat       'QTEMP)':0    Cmd
316100110825     C                   Cat       'DATA(*NO)':1 Cmd
316200110825     C*
316300110825     C                   Call      'QCMDEXC'                            22
316400110825     C                   Parm                    Cmd
316500110825     C                   Parm      80            $lenx
316600110825     C*
316700110825     C                   ENDSR
316800110825      /EJECT
316900110825     C************************************************************
317000110825     C* Scrittura file lavoro
317100110825     C************************************************************
317200130906     C     ScriviSCFW    BEGSR
317300110825     C*
317400130906     C                   Clear                   SDGSCF000
317500110825     C*
317600130906     C* metto AnnAtt0592  in SCFANN perchè non c'è analogo in SCF
317700110825     C* la scelta fra annullati e non è già stata fatta
317800130906     C                   Eval      SCFAnn      = AnnAtt0592
317900130906     C                   Eval      SCFTpAnn    = TpAnn0592
318000130906     C                   Eval      SCFSocieta  = Soc0592
318100110825     C*
318200130906     C                   Eval      SCFORIGINE  = ORIGIN0592
318300130906     C                   Eval      SCFANNO     = ANNO0592
318400130906     C                   Eval      SCFMESE     = MESE0592
318500130906     C                   Eval      SCFCF       = CF0592
318600130906     C                   Eval      SCFSOGG     = SOGG0592
318700130906     C                   Eval      SCFTPN      = TPN0592
318800130906     C                   Eval      SCFPartIVA  = PIVA0592
318900130906     C                   Eval      SCFCDFISC   = CDFISC0592
319000130906     C                   Eval      SCFIndirE   = COGRAG0592
319100130906     C                   Eval      SCFNome     = NOME0592
319200130906     C                   Eval      SCFDTNASC   = DTNASC0592
319300130906R067 C                   Eval      SCFLOCNASC  = LOCNAS0592
319400130906     C                   Eval      SCFPRVNASC  = PRVNAS0592
319500130906R067 C                   Eval      SCFSTATO    = STATO0592
319600130906     C                   Eval      SCFCODUIC   = CODUIC0592
319700130906     C                   Eval      SCFLOCALIT  = LOCALI0592
319800130906     C                   Eval      SCFINDIRE   = INDIRE0592
319900130906     C                   Eval      SCFKCC      = KCC0592
320000130906     C                   Eval      SCFKSC      = KSC0592
320100130906     C                   Eval      SCFCAUSALE  = CAUSAL0592
320200130906     C                   Eval      SCFALIQ     = ALIQ0592
320300130906     C                   Eval      SCFRIFIVA   = RIFIVA0592
320400130906     C                   Eval      SCFDTDOC    = DTDOC0592
320500130906     C                   Eval      SCFNRDOC    = NRDOC0592
320600130906     C                   Eval      SCFSERDOC   = SERDOC0592
320700130906     C                   Eval      SCFDTREG    = DTREG0592
320800130906     C                   Eval      SCFNRREG    = NRREG0592
320900130906     C                   Eval      SCFSERREG   = SERREG0592
321000130906     C                   Eval      SCFDTPAR    = DTPAR0592
321100130906     C                   Eval      SCFNRPAR    = NRPAR0592
321200130906     C                   Eval      SCFSERPAR   = SERPAR0592
321300130906     C                   Eval      SCFSYS      = SYS0592
321400130906     C                   Eval      SCFNRASREG  = NRASRE0592
321500130906     C                   Eval      SCFNRRIGAM  = NRRIGM0592
321600110825     C*
321700130906     C                   Eval      SCFNRRIGA   = NRRIGA0592
321800130906     C                   Eval      SCFTPDOC    = TPDOC0592
321900130906     C                   Eval      SCFNATOPE   = NATOPE0592
322000130906     C                   Eval      SCFTPOPE    = TPOPE0592
322100130906     C                   Eval      SCFNOIVA    = NOIVA0592
322200130906     C                   Eval      SCFMARGINE  = MARGIN0592
322300130906     C                   Eval      SCFREVERSE  = REVERS0592
322400130906     C                   Eval      SCFAUTOFAT  = AUTOFA0592
322500130906     C                   Eval      SCFLEASING  = LEASIN0592
322600130906     C                   Eval      SCFTPIMP    = TPIMP0592
322700130906     C                   Eval      SCFIMPOPE   = IMPOPE0592
322800130906     C                   Eval      SCFIVAOPE   = IVAOPE0592
322900130906     C                   Eval      SCFIMPTOT   = IMPTOT0592
323000130906     C                   Eval      SCFIVATOT   = IVATOT0592
323100130906     C                   Eval      SCFTPREG    = TPREG0592
323200130906     C                   Eval      SCFLIBRO    = LIBRO0592
323300130906     C                   Eval      SCFVARNOTA  = VARNOT0592
323400130906     C                   Eval      SCFFORMATO  = FORMAT0592
323500130906     C                   Eval      SCFQUADRO   = QUADRO0592
323600130906     C                   Eval      SCFSEQQUA   = SEQQUA0592
323700130906     C                   Eval      SCFCOGRAGR  = COGRAR0592
323800130906     C                   Eval      SCFNOMER    = NOMER0592
323900130906     C                   Eval      SCFDTNASR   = DTNASR0592
324000130906     C                   Eval      SCFLOCNASR  = LOCNAR0592
324100130906     C                   Eval      SCFPRVNASR  = PRVNAR0592
324200130906     C                   Eval      SCFSTATOR   = STATOR0592
324300130906     C                   Eval      SCFCODUICR  = CODUIR0592
324400130906     C                   Eval      SCFLOCALIR  = LOCALR0592
324500130906     C                   Eval      SCFINDIRER  = INDIER0592
324600130906     C                   Eval      SCFTPNR     = TPNR0592
324700130906     C                   Eval      SCFPIVAR    = PIVAR0592
324800130906     C                   Eval      SCFCDFISCR  = CDFISR0592
324900130925     C                   Eval      SCFTPOPER   = TPOPER0592
325000130925     C                   Eval      SCFNATOPER  = NATOPR0592
325100130906     C                   Eval      SCFLEASINR  = LEASIR0592
325200130906     C                   Eval      SCFDTREGR   = DTREGR0592
325300130906     C                   Eval      SCFNRREGR   = NRREGR0592
325400130906     C                   Eval      SCFSERREGR  = SERRER0592
325500110825     C*
325600130906     C                   WRITE     SDGSCF0W
325700130906     C                   FEOD      SDGSCF00W
325800110825     C*
325900110825     C                   ENDSR
326000110825     C/EJECT
326100000000     C************************************************************
326200000000     C* INIZIALIZZAZIONE VARIABILI
326300000000     C************************************************************
326400000000     C     INZVAR        BEGSR
326500000000     C*
326600000000     C* Se società diversa da quella impostata nella *inzsr, rieseguo
326700000000     C* reperimento dati società
326800130906    >C                   IF        SocietaG75 <> xscsoc  and
326900130906    >C                             OpzG75 <>*blank
327000000000     C                   MOVEL     'SOC001'      TIPXSC
327100130906    >C                   MOVEL     SocietaG75    SOCXSC
327200000000     C                   EXSR      REPSOC
327300000000     C     RTNXSC        IFNE      '1'
327400000000     C                   MOVEL     XSOCDS        SOC001
327500000000     C                   ELSE
327600000000     C                   MOVE      *ON           $FINE
327700130906    >C                   MOVE      '1'           ErrG75
327800000000     C                   EXSR      ENDPGM
327900000000     C                   ENDIF
328000000000     C                   ENDIF
328100000000     C*
328200110824     C* il chiamante è il pgm di gestione dei quadri
328300130906     C                   if        OPZG75      = '21'
328400110824     C                   Eval      DaSoggetto  = *Off
328500130906     C                   Eval      OpzG75      = '11'
328600110824     C                   else
328700110824     C* il chiamante è il pgm di gestione dei soggetti
328800110824     C                   Eval      DaSoggetto  = *On
328900110824     C                   endif
329000110824     C*
329100000000     C* Reperimento parametro modulo
329200000000     C                   EXSR      REPPAR
329300000000     C*
329400000000     C* Reperimento autorizzazioni
329500000000     C*
329600000000     C* le autorizzazioni potrebbero essere già state passate,
329700000000     C* nel qual caso non vanno reperite
329800000000    >C                   EXSR      REPAUT
329900000000     C*
330000000000     C* visualizzo il sfl troncato
330100000000     C                   Eval      C1Mode      = *On
330200000000     C*
330300000000     C*
330400000000     C* Reperimento valori default utente
330500000000     C*
330600000000     C                   EXSR      REPDFT
330700000000     C*
330800000000     C* Definizione delle opzioni utilizzabili a pgm
330900000000     C*
331000000000     C                   EXSR      DEFOPZ
331100000000     C*
331200000000     C* Variabili per gestione videate
331300000000     C*
331400000000     C                   MOVE      *OFF          $FINE                          fine pgm
331500000000     C                   MOVE      *OFF          $INZD1                         inizializzione D1
331600000000     C                   MOVE      *OFF          $INZS1                         inizializzione S1
331700000000     C                   MOVE      *OFF          $INZD2                         inizializzione D2
331800000000     C                   MOVE      *OFF          $EndFile                       memo fine file
331900000000     C                   MOVE      *OFF          $ESCI                          rich.uscita ciclo
332000000000     C                   MOVE      *OFF          $RCDOK                         rcd letto valido
332100000000     C                   MOVE      *HIVAL        $LASTV
332200000000     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
332300000000     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
332400000000     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
332500000000     C* azzero la posizione del cursore
332600000000     C                   Z-ADD     0             H1RIGA
332700000000     C                   Z-ADD     0             H1COLO
332800000000     C*
332900000000     C                   MOVE      *ON           $INZD1
333000000000     C                   MOVE      *ON           $INZS1
333100000000     C*
333200000000     C* se pgm richiamato con chiave univoca impostata,
333300000000     C* forzo la emissione della lista e imposto l'ordinamento
333400000000    >C*  in questo esempio la chiave univoca per la posta è Codice
333500000000    >C*  ma qualora ci fosse più di un campo basterebbe testare quello più
333600000000    >C*  significativo (o meglio, quello che si sa essere obbligatorio
333700000000    >C*  ricevere pieno)
333800000000     C                   MOVE      *OFF          $Unique
333900000000     C*
334000000000     C* Variabili appoggio
334100000000     C*
334200000000     C                   Z-ADD     SFLPAG        WPAG
334300000000     C                   MOVEL     *OFF          WOPR
334400000000     C                   CLEAR                   W1POS
334500000000     C*
334600000000     C* definisco qual'è la prima videata
334700000000     C* (se la prima videata risulta la lista, devo comunque caricare
334800000000     C*  la videata di parzializzazione e gestire la definizione
334900000000     C*  dell'ordinamento, perchè, dai default oppure se sono stati
335000000000     C*  passati dei dati, può variare il caricamento)
335100000000     C                   SELECT
335200000000     C     $LOP          WHENEQ    1
335300000000     C                   MOVE      'D1'          $GEST
335400000000     C     $LOP          WHENEQ    2
335500000000     C                   MOVE      'S1'          $GEST
335600000000     C                   EXSR      INZD1
335700000000     C                   MOVE      *OFF          $INZD1
335800000000     C                   EXSR      F02D1
335900000000     C                   EXSR      DEFORD
336000000000     C                   ENDSL
336100000000     C*
336200000000     C* Valorizzazione campi univoci testate
336300000000     C*
336400000000     C                   CLEAR                   FMTT1
336500000000     C                   MOVEL     KNSIF         NOMSIF
336600000000     C                   MOVEL     XSCDSI        NOMDIT
336700000000     C                   MOVEL     KCDAZ         NOMAZN
336800000000     C****               MOVEL     DSPGM         NOMPGM
336900000000     C* recupero il sottotitolo (se c'è)
337000000000     C                   MOVEL     *BLANK        T1TIT
337100000000     C     SOTTIT        IFNE      *BLANK
337200000000     C                   MOVEL     SOTTIT        IDMSG
337300000000     C                   CALLB     'XRTVM'
337400000000     C                   PARM                    IDMSG            27
337500000000     C                   PARM                    TXTMSG          100
337600000000     C                   PARM                    ERRMSG            1
337700000000     C                   PARM      *ON           CTRMSG            1            centratura
337800000000     C                   PARM      30            LENMSG            3 0          lun output.
337900000000     C     ERRMSG        IFNE      *ON
338000000000     C                   MOVEL     TXTMSG        T1TIT
338100000000     C                   ELSE
338200000000     C                   MOVEL     *ALL'?'       T1TIT
338300000000     C                   ENDIF
338400000000     C                   ENDIF
338401151201R195 C*
338402151201  "  C* Recupero il titolo con il tipo di comunicazione
338403151201  "  C                   MOVEL     *BLANK        T1TIT
338404151201  "  C*
338405151201  "  C                   Select
338406151201  "  C*
338407151201  "  C* Comunicazione Spesometro
338408151201  "  C                   When      TpComG75   = 'E'
338409151201  "  C                   if        SOTTITT1   <> *BLANK
338410151201  "  C                   MOVEL     SOTTITT1      IDMSG
338411151201  "  C                   endif
338412151201  "  C*
338413151201  "  C* Comunicazione Acquisti da San Marino
338414151201  "  C                   When      TpComG75   = 'S'
338415151201  "  C                   if        SOTTITT2   <> *BLANK
338416151201  "  C                   MOVEL     SOTTITT2      IDMSG
338417151201  "  C                   endif
338418151201  "  C*
338419151201  "  C* Comunicazione Black list
338420151201  "  C                   When      TpComG75   = 'B'
338421151201  "  C                   if        SOTTITT3   <> *BLANK
338422151201  "  C                   MOVEL     SOTTITT3      IDMSG
338423151201  "  C                   endif
338424151201  "  C*
338425151201  "  C                   endsl
338426151201  "  C*
338427151201  "  C                   CALLB     'XRTVM'
338428151201  "  C                   PARM                    IDMSG            27
338429151201  "  C                   PARM                    TXTMSG          100
338430151201  "  C                   PARM                    ERRMSG            1
338431151201  "  C                   PARM      *ON           CTRMSG            1            centratura
338432151201  "  C                   PARM      30            LENMSG            3 0          lun output.
338433151201  "  C     ERRMSG        IFNE      *ON
338434151201  "  C                   MOVEL     TXTMSG        T1TIT
338435151201  "  C                   ELSE
338436151201  "  C                   MOVEL     *ALL'?'       T1TIT
338437151201R195 C                   ENDIF
338500000000     C*
338600000000     C                   ENDSR
338700000000     C/EJECT
338800000000     C************************************************************
338900000000     C* REPERIMENTO DATI SOCIETA'
339000000000     C************************************************************
339100000000     C     REPSOC        BEGSR
339200000000     C*
339300000000     C                   CALL      'SDGSOCDR'
339400000000     C                   PARM                    TIPXSC            6
339500000000     C                   PARM                    SOCXSC            3
339600000000     C                   PARM                    CDSXSC            9 0
339700000000     C                   PARM                    MODXSC            3
339800000000     C                   PARM      *BLANKS       RTNXSC            1
339900000000     C                   PARM                    XSOCDS
340000000000     C                   PARM                    KPJBA
340100000000     C*
340200000000     C                   ENDSR
340300000000     C/EJECT
340400000000     C************************************************************
340500000000     C* Rerimento parametro modulo
340600000000     C************************************************************
340700000000     C     REPPAR        BEGSR
340800000000     C*
340900000000     C*
341000000000     C                   ENDSR
341100000000     C************************************************************
341200000000     C* REPERIMENTO AUTORIZZAZIONI UTENTE
341300000000     C************************************************************
341400000000     C     REPAUT        BEGSR
341500000000     C*
341600000000     C                   CLEAR                   AUTDS
341700000000     C                   Clear                   ConCivDs
341800000000     C*
341900000000     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
342000000000     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
342100000000     C                   Z-ADD     0             XCAGRP                         GRUPPO
342200000000     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
342300000000    >C                   MOVEL     'CONCIV'      XCAFNC                         FUNZIONE
342400000000     C                   MOVEL     *ZERO         XCAVFU
342500000000    >C                   MOVE      'CK'          XCATCK                         TIPO CTRL
342600000000     C                   MOVE      '0'           XCANAU                         RICH.XNOAUT
342700000000     C*
342800000000     C                   CALLB     'XCHKAUT'
342900000000     C                   PARM                    AUTDS
343000000000     C*
343100000000     C* errori/non abilitazione
343200000000     C     XCARTN        IFNE      0
343300000000     C*
343400000000     C                   ELSE
343500000000    >C                   MOVEL     XCADDS        CONCIVDS
343600000000     C                   ENDIF
343700000000     C*
343800000000     C                   ENDSR
343900000000     C/EJECT
344000000000     C************************************************************
344100000000     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
344200000000     C************************************************************
344300000000     C     REPDFT        BEGSR
344400000000     C*
344500000000     C* preparo i parametri di chiamata al driver
344600000000     C                   CLEAR                   XMPEDS
344700000000     C                   EVAL      TpChiamata = '01'
344800000000     C                   EVAL      NChiavi = 5
344900000000     C                   EVAL      VLogica = 2
345000000000     C*
345100000000     C* preparo le chiavi per la chiamata al driver
345200000000     C                   CLEAR                   XMPEDS1
345300000000     C                   MOVEL     XSCSOC        DSMPESOC
345400000000     C                   MOVEL     DSPGM         DSMPEPGM
345500000000     C                   MOVEL     KNMUS         DSMPEPRF
345600000000     C* Si reperiscono i dati di default a seconda delle modalità con
345700000000     C* cui il pgm è usato
345800000000     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
345900000000     C* MPETIP = 'PV'  pgm richiamato da menù
346000000000     C* MPETIP = 'PB'  x lancio Batch
346100000000     C                   SELECT
346200130906    >C     OPZG75        WHENEQ    '01'
346300000000     C                   MOVEL     'PX'          DSMPETIP
346400000000     C                   OTHER
346500000000     C                   MOVEL     'PV'          DSMPETIP
346600000000     C                   ENDSL
346700000000     C*
346800000000     C                   CALLB     'XMPE'
346900000000     C                   PARM                    XMPEDS
347000000000     C                   PARM                    XMPEDS1
347100000000     C*
347200000000     C     Risultato     IFEQ      '0'
347300000000     C                   MOVEL     DSMPEPAR      DSMPE
347400000000     C* nel caso sia la prima volta che l'utente apre questa anagrafica
347500000000     C* i suoi valori di default vengono decisi a pgm
347600000000     C                   ELSE
347700000000     C                   Z-ADD     2             $LOP
347800000000     C                   Z-ADD     1             $ORD
347900000000     C                   ENDIF
348000000000     C*
348100000000     C                   ENDSR
348200000000     C/EJECT
348300000000     C************************************************************
348400000000     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
348500000000     C************************************************************
348600000000     C     DEFOPZ        BEGSR
348700000000     C*
348800000000     C* Questa routine deve valorizzare la schiera $FL1
348900000000     C* con S o N a seconda della utilizzabilità o meno
349000000000     C* dell'opzione correlata
349100000000     C*
349200000000     C* Inizialmente si considerano tutte le opzioni come valide
349300000000     C     1             DO        $DIMOP        X
349400000000     C* Imposta tutti i flg delle opzioni non a *blank, a 'S'
349500000000     C                   If        $TX1(X)  <> *Blank
349600000000     C                   MOVE      'S'           $FL1(X)
349700000000     C                   Else
349800000000     C                   Leave
349900000000     C                   EndIf
350000000000     C                   ENDDO
350100000000     C*
350200000000     C* Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
350300000000     C* con 1 o 0 a seconda della utilizzabilità o meno
350400000000     C* del tasto funzione correlato
350500000000     C*
350600000000     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
350700000000     C* di compilazione
350800000000     C                   RESET                   $FC1
350900000000     C                   RESET                   $FR1
351000000000     C                   RESET                   $FC2
351100000000     C                   RESET                   $FR2
351200000000     C                   RESET                   $FC3
351300000000     C                   RESET                   $FR3
351400000000     C*
351500000000     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
351600000000     C* invalido alcune opzioni
351700000000     C                   SELECT
351800000000     C* Se pgm richiamato in ricerca disattivo :
351900130906    >C     OPZG75        WHENEQ    '01'
352000000000     C* F3=Fine
352100000000     C                   Z-ADD     3             X
352200000000     C                   MOVE      '0'           $FC1(X)
352300000000     C                   MOVE      '0'           $FC2(X)
352400000000     C                   MOVE      '0'           $FC3(X)
352500000000     C*
352600000000     C*
352700000000     C* Se pgm richiamato in interrogazione disattivo :
352800130906    >C     OPZG75        WHENEQ    '05'
352900000000     C* F10=Immissione
353000000000     C                   Z-ADD     10            X
353100000000     C                   MOVE      '0'           $FC1(X)
353200000000     C                   MOVE      '0'           $FC2(X)
353300000000     C* 1=Selezione
353400000000     C                   Z-ADD     1             X
353500000000     C     01            LOOKUP    $OP1(X)                                21
353600000000     C     *IN21         IFEQ      *ON
353700000000     C                   MOVE      'N'           $FL1(X)
353800000000     C                   ENDIF
353900000000     C* 2=Modifica
354000000000     C                   Z-ADD     1             X
354100000000     C     02            LOOKUP    $OP1(X)                                21
354200000000     C     *IN21         IFEQ      *ON
354300000000     C                   MOVE      'N'           $FL1(X)
354400000000     C                   ENDIF
354500000000     C* 3=Copia
354600000000     C                   Z-ADD     1             X
354700000000     C     03            LOOKUP    $OP1(X)                                21
354800000000     C     *IN21         IFEQ      *ON
354900000000     C                   MOVE      'N'           $FL1(X)
355000000000     C                   ENDIF
355100000000     C* 4=Annullamento
355200000000     C                   Z-ADD     1             X
355300000000     C     04            LOOKUP    $OP1(X)                                21
355400000000     C     *IN21         IFEQ      *ON
355500000000     C                   MOVE      'N'           $FL1(X)
355600000000     C                   ENDIF
355700000000     C*
355800130906    >C     OPZG75        WHENEQ    *BLANKS
355900000000     C* Se pgm non richiamato, disattivo subito :
356000000000     C* F8=Selezione seguente
356100000000     C                   Z-ADD     8             X
356200000000     C                   MOVE      '0'           $FC1(X)
356300000000     C                   MOVE      '0'           $FC2(X)
356400000000     C* 1=Selezione
356500000000     C                   Z-ADD     1             X
356600000000     C     01            LOOKUP    $OP1(X)                                21
356700000000     C     *IN21         IFEQ      *ON
356800000000     C                   MOVE      'N'           $FL1(X)
356900000000     C                   ENDIF
357000000000     C*
357100000000     C* In tutti gli altri casi, disattivo subito :
357200000000     C                   OTHER
357300000000     C* 1=Selezione
357400000000     C                   Z-ADD     1             X
357500000000     C     01            LOOKUP    $OP1(X)                                21
357600000000     C     *IN21         IFEQ      *ON
357700000000     C                   MOVE      'N'           $FL1(X)
357800000000     C                   ENDIF
357900000000     C*
358000000000     C                   ENDSL
358100000000     C*
358200000000     C*
358300000000     C* poi, disattivo a seconda delle autorizzazioni :
358400000000     C                   IF        ImmAll <> *on
358500000000     C*
358600000000     C* F10=Immissione
358700000000     C                   Z-ADD     10            X
358800000000     C                   MOVE      '0'           $FC1(X)
358900000000     C                   MOVE      '0'           $FC2(X)
359000000000     C*
359100000000     C                   Z-ADD     1             X
359200000000     C     03            LOOKUP    $OP1(X)                                21
359300000000     C     *IN21         IFEQ      *ON
359400000000     C                   MOVE      'N'           $FL1(X)
359500000000     C                   ENDIF
359600000000     C*
359700000000     C                   ENDIF
359800000000     C*
359900000000     C                   IF        ManAll <> *on
360000000000     C                   Z-ADD     1             X
360100000000     C     02            LOOKUP    $OP1(X)                                21
360200000000     C     *IN21         IFEQ      *ON
360300000000     C                   MOVE      'N'           $FL1(X)
360400000000     C                   ENDIF
360500000000     C*
360600000000     C                   IF        VisAll <> *on
360700000000     C                   Z-ADD     1             X
360800000000     C     22            LOOKUP    $OP1(X)                                21
360900000000     C     *IN21         IFEQ      *ON
361000000000     C                   MOVE      'N'           $FL1(X)
361100000000     C                   ENDIF
361200000000     C                   endif
361300000000     C                   ENDIF
361400000000     C*
361500000000     C                   IF        AnnAll <> *on
361600000000     C                   Z-ADD     1             X
361700000000     C     04            LOOKUP    $OP1(X)                                21
361800000000     C     *IN21         IFEQ      *ON
361900000000     C                   MOVE      'N'           $FL1(X)
362000000000     C                   ENDIF
362100000000     C*
362200000000     C                   Z-ADD     1             X
362300000000     C     44            LOOKUP    $OP1(X)                                21
362400000000     C     *IN21         IFEQ      *ON
362500000000     C                   MOVE      'N'           $FL1(X)
362600000000     C                   ENDIF
362700000000     C                   ENDIF
362800000000     C*
362900000000     C                   IF        VisAll <> *on
363000000000     C                   Z-ADD     1             X
363100000000     C     05            LOOKUP    $OP1(X)                                21
363200000000     C     *IN21         IFEQ      *ON
363300000000     C                   MOVE      'N'           $FL1(X)
363400000000     C                   ENDIF
363500000000     C                   ENDIF
363600000000     C*
363700000000     C*
363800000000     C                   ENDSR
363900000000     C/EJECT
364000000000     C************************************************************
364100000000     C* REPERIMENTO OPZIONI
364200000000     C************************************************************
364300000000     C     REPOPZ        BEGSR
364400000000     C*
364500000000     C     1             DO        $DIMOP        X
364600000000     C*
364700000000     C     $MS1(X)       IFNE      *BLANK
364800000000     C*
364900000000     C                   MOVEL     $MS1(X)       IDMSG
365000000000     C                   CALLB     'XRTVM'
365100000000     C                   PARM                    IDMSG            27
365200000000     C                   PARM                    TXTMSG          100
365300000000     C                   PARM                    ERRMSG            1
365400000000     C*
365500000000     C     ERRMSG        IFNE      *ON
365600000000     C                   MOVEL     TXTMSG        $TX1(X)
365700000000     C                   ELSE
365800000000     C                   MOVEL     *ALL'?'       $TX1(X)
365900000000     C                   ENDIF
366000000000     C*
366100000000     C                   ENDIF
366200000000     C*
366300000000     C                   ENDDO
366400000000     C*
366500000000     C                   ENDSR
366600000000     C/EJECT
366700000000     C************************************************************
366800000000     C* REPERIMENTO TASTI DI FUNZIONE
366900000000     C************************************************************
367000000000     C     REPKEY        BEGSR
367100000000     C                   MOVEA     $FM1          $FM
367200000000     C                   MOVEA     $FC1          $FC
367300000000     C                   EXSR      REPKKK
367400000000     C                   MOVEA     $FX           $FX1
367500000000     C*
367600000000     C                   MOVEA     $FM2          $FM
367700000000     C                   MOVEA     $FC2          $FC
367800000000     C                   EXSR      REPKKK
367900000000     C                   MOVEA     $FX           $FX2
368000000000     C*
368100000000     C                   MOVEA     $FM3          $FM
368200000000     C                   MOVEA     $FC3          $FC
368300000000     C                   EXSR      REPKKK
368400000000     C                   MOVEA     $FX           $FX3
368500000000     C                   ENDSR
368600000000     C/EJECT
368700000000     C************************************************************
368800000000     C* REPERIMENTO TASTI DI FUNZIONE
368900000000     C************************************************************
369000000000     C     REPKKK        BEGSR
369100000000     C*
369200000000     C                   CLEAR                   $FX
369300000000     C*
369400000000     C     1             DO        24            X
369500000000     C*
369600000000     C     $FM(X)        IFNE      *BLANK
369700000000     C     $Fc(X)        andne     '0'
369800000000     C*
369900000000     C                   MOVEL     $FM(X)        IDMSG
370000000000     C                   CALLB     'XRTVM'
370100000000     C                   PARM                    IDMSG            27
370200000000     C                   PARM                    TXTMSG          100
370300000000     C                   PARM                    ERRMSG            1
370400000000     C*
370500000000     C     ERRMSG        IFNE      *ON
370600000000     C                   MOVEL     TXTMSG        $FX(X)
370700000000     C                   ELSE
370800000000     C                   MOVEL     *ALL' '       $FX(X)
370900000000     C                   ENDIF
371000000000     C*
371100000000     C                   ENDIF
371200000000     C*
371300000000     C                   ENDDO
371400000000     C*
371500000000     C                   ENDSR
371600000000     C/EJECT
371700000000     C************************************************************
371800000000     C* DEFINIZIONE KLIST
371900000000     C************************************************************
372000000000     C     DefKlist      BEGSR
372100000000     C*
372200000000     C* klist
372300000000     C*
372400130906     C* SDGSCF11L
372500130906     C     K05SCF11      KLIST
372600130906     C                   KFLD                    SCFTpCom
372700130906     C                   KFLD                    SCFAnno
372800130906     C                   KFLD                    SCFMese
372900130906     C                   KFLD                    SCFSys
373000130906     C                   KFLD                    SCFNrAsReg
373100130906     C                   KFLD                    SCFNrRigaM
373200130906     C                   KFLD                    SCFNrRiga
373300000000     C*
373400000000     C                   ENDSR
373500000000     C/EJECT
373600000000** $MS1 / $TX1
373700000000PROMSG    *LIBL     OPZ0001
373800000000PROMSG    *LIBL     OPZ0002
373900000000PROMSG    *LIBL     OPZ0003
374000000000PROMSG    *LIBL     OPZ0004
374100000000PROMSG    *LIBL     OPZ0005
374200000000PROMSG    *LIBL     OPZ0146
374300000000PROMSG    *LIBL     OPZ0041
374400000000SDGMSG    *LIBL     SDO0024
374500000000** $OP1 / $FL1
37460000000001S      <Selezione
37470000000002S      <Modifica
37480000000003S      <Copia
37490000000004S      <Cancella
37500000000005S      <Visualizza
37510000000015S      <Interrogazioni conto
37520000000041S      <Contabilità
37530000000044S      <Annulla/Ripristina
375400000000** TASTI DI FUNZIONE  D1 : $FM1 / $FX1
375500000000PROMSG    *LIBL     KEY0001  01
375600000000PROMSG    *LIBL     KEY0002  02
375700000000PROMSG    *LIBL     KEY0003  03
375800000000PROMSG    *LIBL     KEY0004  04
375900000000PROMSG    *LIBL     KEY0005  05
376000000000PROMSG    *LIBL     KEY0006  06
376100000000PROMSG    *LIBL     KEY0007  07
376200000000PROMSG    *LIBL     KEY0008  08
376300000000PROMSG    *LIBL     KEY0009  09
376400000000PROMSG    *LIBL     KEY0010  10
376500000000PROMSG    *LIBL     KEY0011  11
376600000000PROMSG    *LIBL     KEY0012  12
376700000000PROMSG    *LIBL     KEY0013  13
376800000000PROMSG    *LIBL     KEY0014  14
376900000000SDGMSG    *LIBL     SDK0003  15
377000000000PROMSG    *LIBL     KEY0016  16
377100000000SDGMSG    *LIBL     SDK0001  17
377200000000PROMSG    *LIBL     KEY0018  18
377300000000PROMSG    *LIBL     KEY0019  19
377400000000PROMSG    *LIBL     KEY0020  20
377500000000PROMSG    *LIBL     KEY0021  21
377600000000PROMSG    *LIBL     KEY0022  22
377700000000PROMSG    *LIBL     KEY0023  23
377800000000PROMSG    *LIBL     KEY0024  24
377900000000**  ABILITAZIONE D1 : $FC1 / $FR1
3780000000000N  01
3781000000001N  02
3782000000001N  03
3783000000001N  04
3784000000000N  05
3785000000000N  06
3786000000000N  07
3787000000001N  08
3788000000000N  09
3789000000000N  10
3790000000000N  11
3791000000001N  12
3792000000000N  13
3793000000000N  14
3794000000000N  15
3795000000000N  16
3796000000000N  17
3797000000000N  18
3798000000000N  19
3799000000000N  20
3800000000000N  21
3801000000000N  22
3802000000000N  23   Impostare a '1' se si utilizzano opzioni
3803000000001N  24   Lasciare sempre impostata a '1' il valore della riga 24
380400000000** TASTI DI FUNZIONE  S1 : $FM2 / $FX2
380500000000PROMSG    *LIBL     KEY0001  01
380600000000PROMSG    *LIBL     KEY0002  02
380700000000PROMSG    *LIBL     KEY0003  03
380800000000PROMSG    *LIBL     KEY0004  04
380900000000PROMSG    *LIBL     KEY0005  05
381000000000PROMSG    *LIBL     KEY0006  06
381100000000PROMSG    *LIBL     KEY0007  07
381200000000PROMSG    *LIBL     KEY0008  08
381300000000PROMSG    *LIBL     KEY0009  09
381400000000PROMSG    *LIBL     KEY0010  10
381500000000PROMSG    *LIBL     KEY0011  11
381600000000PROMSG    *LIBL     KEY0012  12
381700000000PROMSG    *LIBL     KEY0013  13
381800000000PROMSG    *LIBL     KEY0014  14
381900000000PROMSG    *LIBL     KEY0015  15
382000000000PROMSG    *LIBL     KEY0016  16
382100000000SDGMSG    *LIBL     SDK0001  17
382200000000PROMSG    *LIBL     KEY0018  18
382300000000PROMSG    *LIBL     KEY0019  19
382400000000PROMSG    *LIBL     KEY0020  20
382500000000PROMSG    *LIBL     KEY0021  21
382600000000PROMSG    *LIBL     KEY0022  22
382700000000PROMSG    *LIBL     KEY0023  23
382800000000PROMSG    *LIBL     KEY0024  24
382900000000**  ABILITAZIONE S1 : $FC2 / $FR2
3830000000000N  01
3831000000000N  02
3832000000001N  03
3833000000000N  04
3834001309240N  05
3835001309240N  06
3836000000000N  07
3837000000001N  08
3838000000001N  09
3839000000001N  10
3840000000000N  11
3841000000001N  12
3842000000000N  13
3843000000000N  14
3844000000000N  15
3845000000000N  16
3846000000000N  17
3847000000000N  18
3848000000000N  19
3849000000000N  20
3850000000000N  21
3851000000000N  22
3852000000001N  23   Impostare a '1' se si utilizzano opzioni
3853001202061N  24   Lasciare sempre impostata a '1' il valore della riga 24
385400000000** TASTI DI FUNZIONE  D3 : $FM3 / $FX3
385500000000PROMSG    *LIBL     KEY0001  01
385600000000PROMSG    *LIBL     KEY0002  02
385700000000PROMSG    *LIBL     KEY0003  03
385800000000PROMSG    *LIBL     KEY0004  04
385900000000PROMSG    *LIBL     KEY0005  05
386000000000PROMSG    *LIBL     KEY0006  06
386100000000PROMSG    *LIBL     KEY0007  07
386200000000PROMSG    *LIBL     KEY0008  08
386300000000PROMSG    *LIBL     KEY0009  09
386400000000PROMSG    *LIBL     KEY0010  10
386500000000PROMSG    *LIBL     KEY0011  11
386600000000PROMSG    *LIBL     KEY0012  12
386700000000PROMSG    *LIBL     KEY0013  13
386800000000PROMSG    *LIBL     KEY0014  14
386900000000PROMSG    *LIBL     KEY0015  15
387000000000PROMSG    *LIBL     KEY0016  16
387100000000SDGMSG    *LIBL     SDK0001  17
387200000000PROMSG    *LIBL     KEY0018  18
387300000000PROMSG    *LIBL     KEY0019  19
387400000000PROMSG    *LIBL     KEY0020  20
387500000000PROMSG    *LIBL     KEY0021  21
387600000000PROMSG    *LIBL     KEY0022  22
387700000000PROMSG    *LIBL     KEY0023  23
387800000000PROMSG    *LIBL     KEY0024  24
387900000000**  ABILITAZIONE D3 : $FC3 / $FR3
3880000000000N  01
3881000000000N  02
3882000000001N  03
3883000000000N  04
3884000000000N  05
3885000000000N  06
3886000000000N  07
3887000000000N  08
3888000000000N  09
3889000000000N  10
3890000000000N  11
3891000000001N  12
3892000000000N  13
3893000000000N  14
3894000000000N  15
3895000000000N  16
3896000000000N  17
3897000000000N  18
3898000000000N  19
3899000000000N  20
3900000000000N  21
3901000000000N  22
3902000000000N  23   Impostare a '1' se si utilizzano opzioni
3903000000001N  24   Lasciare sempre impostata a '1' il valore della riga 24
