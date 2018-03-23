000100130829     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND SDG005)
000200131008R134 H*PARMS CVTOPT(*NONE) ACTGRP(QUADRI)
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400000000      *
000500000000      *           -----------------------------
000600130829      *                Spesometro clienti/fornitori
000700130903      *                 Dati per QUADRI    Guida
000800000000      *           -----------------------------
000900131008      *
001000131008R134 F* Cambiato actgrp QILE con QUADRI
001100131008R134 X*****PARMS CVTOPT(*NONE) ACTGRP(QILE)
001200000000      *
001300000000     F*----------------------------------------------------------------
001400130829     FSDGSG77D  CF   E             WORKSTN
001500130829     F                                     RENAME(SG77D1:FMTD1)
001600130829     F                                     RENAME(SG77D2:FMTD2)
001700130829     F                                     RENAME(SG77D3:FMTD3)
001800130903     F                                     RENAME(SG77S1:FMTS1)
001900130829     F                                     RENAME(SG77C1:FMTC1)
002000130925     F                                     RENAME(SG77S2:FMTS2)
002100130925     F                                     RENAME(SG77C2:FMTC2)
002200130829     F                                     RENAME(SG77T1:FMTT1)
002300130829     F                                     RENAME(SG77Z1:FMTZ1)
002400130829     F                                     RENAME(SG77Z2:FMTZ2)
002500130829     F                                     RENAME(SG77Z3:FMTZ3)
002600000000     F                                     SFILE(FMTS1:S1NRR)
002700130925     F                                     SFILE(FMTS2:S1NRR)
002800000000     F                                     INFDS(DSFMT)
002900000000     D*----------------------------------------------------*
003000000000     D* Schiere per la gestione delle opzioni
003100000000     D*
003200000000     D* dimensione della schiera $MS1, da non cambiare
003300000000     D $DIMOP          C                   CONST(30)
003400000000     D $MS1            S             27    DIM($dimop) CTDATA PERRCD(1)
003500000000     D $TX1            S             50    DIM($dimop) ALT($MS1)
003600000000     D $OP1            S              2  0 DIM($dimop) CTDATA PERRCD(1)
003700000000     D $FL1            S              1    DIM($dimop) ALT($OP1)
003800000000     D* schiera contenente i nomi dei programma richiamati attraverso
003900000000     D* le opzioni personalizzate
004000000000     D $PGO1           S             10    DIM($dimop)
004100000000     D $TX             S             50    DIM(50)
004200000000     D $FL             S              1    DIM(50)
004300000000     D* Schiere generali per la gestione dei tasti di funzione
004400000000     D $FM             S             27    DIM(24)
004500000000     D $FX             S             50    DIM(24)
004600000000     D $FC             S              1    DIM(24)
004700000000     D $FR             S              1    DIM(24)
004800000000     D* Schiere per la gestione dei tasti di funzione
004900000000     D* Video D1
005000000000     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
005100000000     D $FX1            S             50    DIM(24) ALT($FM1)
005200000000     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
005300000000     D $FR1            S              1    DIM(24) ALT($FC1)
005400000000     D* Schiera contente i nomi dei programmi da richiamare con i tasti
005500000000     D* funzione personalizzata S1
005600000000     D $PGF2           S             10    DIM(25)
005700000000     D* Schiere per la gestione dei tasti di funzione
005800000000     D* Video S1
005900000000     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
006000000000     D $FX2            S             50    DIM(24) ALT($FM2)
006100000000     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
006200000000     D $FR2            S              1    DIM(24) ALT($FC2)
006300000000     D* Schiere per la gestione dei tasti di funzione
006400000000     D* Video D2
006500000000     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
006600000000     D $FX3            S             50    DIM(24) ALT($FM3)
006700000000     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
006800000000     D $FR3            S              1    DIM(24) ALT($FC3)
006900000000     D*
007000000000     D $D1L01          S              3  0
007100000000     D $D1L02          S              3  0
007200000000     D $S1L01          S              3  0
007300000000     D $S1L02          S              3  0
007400000000     D $C1Op1          S              3  0
007500000000     D $C1Op2          S              3  0
007600000000     D $D2L01          S              3  0
007700000000     D $D2L02          S              3  0
007800000000     D*
007900000000     D*----------------------------------------------------*
008000000000     D* Valori assunti
008100000000     D XMPEDS        E DS                  INZ
008200000000     D XMPEDS1       E DS                  INZ
008300000000     D DSMPE           DS                  INZ
008400000000     D  $LOP                   1      1  0
008500000000     D  $ORD                   2      3  0
008600000000     D*-------------
008700000000     D* Passaggio Parametri
008800000000     D KPJBA         E DS
008900000000     D*-------------
009000000000     D* Reperimento Autorizzazioni
009100000000     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
009200000000     D*-------------
009300000000     D* Dati per pgm XNOAUT
009400000000     D XNOADS        E DS                  EXTNAME(XNOAUTDS)
009500000000     D*-------------
009600000000     D* Passaggio Autorizzazioni
009700000000     D DSAUT           DS           512
009800000000     D*-------------
009900000000     D* Maschera dati autorizzazioni
010000000000     D CONCIVDS      E DS
010100000000     D*-------------
010200000000     D* Dati di ambiente ottenuti da SDGSOCDR
010300000000     D SOC001        E DS                  EXTNAME(SDGSOCDS )
010400000000     D*-------------
010500000000     D* DS Interna per dati di output di SDGSOCDR
010600000000     D XSOCDS          DS          1000
010700000000     D*-------------
010800000000     D* DS Esterna gestione messaggi
010900000000     D SDGMSGDS      E DS
011000000000     D  STS          E                     EXTFLD(MSGSTS)
011100000000     D                                     DIM(333)
011200000000     D  JBA          E                     EXTFLD(MSGJBA)
011300000000     D                                     DIM(502)
011400000000     D*-------------
011500000000     D* campi di lavoro per emissione window di avvertimento
011600000000     D WXMSGInz        S              1A   INZ('1')
011700130906     D*-------------
011800130906     D* Pagametri modulo SDGSPECF
011900130906     D SDGPA08DS     E DS
011901160105R195 D*-------------
011902160105 "   D* Parametri modulo SDBLACL2
011903160105R195 D SDGPA11DS     E DS
012000000000     D*-------------
012100000000     D* Richiamo a SDGTABDR
012200000000     D SDGTABDS      E DS                  inz
012300000000     D*-------------
012400000000     D* Parametri per ricerca codice ANATB00F
012500000000     D XTABDS        E DS                  INZ
012600130906     D*-------------
012700130906     D* Tabella degli Stati
012800130906     D ANGG12DS      E DS
012900000000     D*-------------
013000000000     D* Ricerca legende
013100000000     D ANA964DS      E DS                  INZ
013200000000     D*-------------
013300000000     D*Controlli validità per valore di una legenda
013400000000     D X07VALDS      E DS                  Inz
013500130906     D*-------------
013600130918     D WStato          S                   Like(XScSta )
013700130918     D $CodUIC         S                   Like(CODUICG12)
013800130918     D $UICSoc         S                   Like(CODUICG12)
013900130918     D $UICKsc         S                   Like(CODUICG12)
014000130918     D $NazSoc         S                   Like(NazG12 )
014100130918     D $NazKsc         S                   Like(NazG12 )
014200130918     D $NazISO         S                   Like(NazG12 )
014300000000     D*-------------
014400000000     D* DS per controllo conto
014500000000     D ND002DS       E DS                  INZ
014600000000     D*-------------
014700000000     D* Reperimento parametri
014800000000     D XPaOut          DS          2000
014900000000     D*-------------
015000000000     D* Parametri in input
015100130829    >D SDGSG77DS     E DS
015200130829     D*-------------
015300130829     D* Parametri del PGM di lettura movimenti estratti
015400130829     D* Parametri di INPUT
015500130829     D SDG00502DS    E DS                  INZ
015600130829     D  RIC0502      E                     INZ('0')
015700130829     D*----------
015800130829     D* Parametri di OUTPUT
015900130829     D* per manutenzione di testata, quadri e dettaglio
016000130829     D SDG00592DS    E DS                  INZ
016100130829     D*--------------------
016200130829     D* Parametri di OUTPUT
016300130829     D* per lista quadri a video e creazione tracciato ministeriale
016400130829     D*---------
016500130829     D*---------
016600130829     D* formato AGGREGATO
016700130829     D*---
016800130829     D* SOGGETTI NAZIONALI: FATTURE, NOTE E DOCUMENTI RIEPILOGATIVI
016900130829     D*                     (NOLEGGIO/LEASING)
017000130829     D SDG005FADS    E DS                  INZ
017100130829     D*
017200130829     D*---
017300130829     D* SOGGETTI NAZIONALI: OPERAZIONI SENZA FATTURA
017400130829     D SDG005SADS    E DS                  INZ
017500130829     D*
017600130829     D*---
017700130829     D* SOGGETTI NON RESIDENTI E ACQUISTI DI SERVIZI DA NON RESIDENTI
017800130829     D SDG005BLDS    E DS                  INZ
017900130829     D*---------
018000130829     D*---------
018100130829     D* formato DETTAGLIATO
018200130829     D*---
018300130829     D* SOGGETTI NAZIONALI: FATTURE E DOCUMENTI RIEPILOGATIVI EMESSI
018400130829     D*                     (NOLEGGIO/LEASING)
018500130829     D SDG005FEDS    E DS                  INZ
018600130829     D*---
018700130829     D* SOGGETTI NAZIONALI: FATTURE E DOCUMENTI RIEPILOGATIVI RICEVUTI
018800130829     D SDG005FRDS    E DS                  INZ
018900130829     D*---
019000130829     D* SOGGETTI NAZIONALI: NOTE EMESSE
019100130829     D SDG005NEDS    E DS                  INZ
019200130829     D*---
019300130829     D* SOGGETTI NAZIONALI: NOTE RICEVUTE
019400130829     D SDG005NRDS    E DS                  INZ
019500130829     D*---
019600130829     D* SOGGETTI NAZIONALI: OPERAZIONI SENZA FATTURA
019700130829     D*                     (NOLEGGIO/LEASING)
019800130829     D SDG005DFDS    E DS                  INZ
019900130829     D*---
020000130829     D* SOGGETTI NON RESIDENTI
020100130829     D*                     (NOLEGGIO/LEASING)
020200130829     D SDG005FNDS    E DS                  INZ
020300130829     D*---
020400130829     D* ACQUISTI DI SERVIZI DA NON RESIDENTI
020500130829     D SDG005SEDS    E DS                  INZ
020600131017R136 D*---
020700131017 "   D* SOGGETTI NON RESIDENTI: OPERAZIONI SENZA FATTURA
020800131017 "   D* LEGATE AL TURISMO
020900131017R136 D SDG005TUDS    E DS
021000130829     D*---
021100130829     D* RIEPILOGO
021200130829     D SDG005TADS    E DS                  INZ
021201160129R195 D*-------------
021202160129 "   D* Parametri per applicazione soglia a
021203160129 "   D* operazioni con operatori in Black List
021204160129R195 D SDG00508DS    E DS
021300000000     D*-------------
021400000000     D* Parametri dettaglio quadri
021500130829     D SDGSG75DS     E DS
021600130829     D  RETG75       E                     INZ('0')
021700130829     D  OPRG75       E                     INZ('0')
021800130829     D  ERRG75       E                     INZ('0')
021900130829     D  CMTG75       E                     INZ('0')
022000000000     D*-------------
022100131009R134 X**** Parametri del PGM di visualizzazione totali
022200131009 "   X**** SDGSD80DS     E DS                  inz
022300131009 "   X**** RETD80       E                     INZ('0')
022400131009 "   X**** OPRD80       E                     INZ('0')
022500131009 "   X**** ERRD80       E                     INZ('0')
022600131009R134 X**** CMTD80       E                     INZ('0')
022700000000     D*-------------
022800000000     D* Interrogazione conti
022900000000     D ANA399DS      E DS                  Inz
023000000000     D*
023100000000     D WA39Iniz        S                   Like(A39Iniz)
023200000000     D*-------------
023300000000     D* Paramentri richiamo pnota
023400000000     D NDC001DS      E DS                  INZ
023500000000     D  RET001       E                     INZ('0')
023600000000     D  OPR001       E                     INZ('0')
023700000000     D  ERR001       E                     INZ('0')
023800000000     D*-------------
023900000000     D* campo di salvataggio per la DS d'ingresso durante la chiamata
024000000000     D* alla funzione personalizzata
024100130829    >D $SaveDS         S                   like(SDGSG77DS)
024200000000     D*-------------
024300000000     D DSFMT           DS           512
024400000000     D  $TASTO               369    369
024500000000     D  NRG                  370    370
024600000000     D  NCL                  371    371
024700000000      * riga cursore window
024800000000     D  NRiWin               382    382
024900000000      * colonna cursore window
025000000000     D  NCoWin               383    383
025100000000     D  SFLNRR               378    379B 0
025200000000     D*-------------
025300000000     D* posizione cursore
025400000000     D CURSOR          DS
025500000000     D  RIRI                   1      2B 0 INZ
025600000000     D  R$$                    2      2
025700000000     D  COCO                   3      4B 0 INZ
025800000000     D  C$$                    4      4
025900000000     D*-------------
026000000000     D* Costanti
026100000000     D* nr.di righe del sfl (SFLPAG)
026200130903    >D SFLPAG          S              4  0 inz(9999)
026300000000     D*-------------
026400000000     D* Tasti di funzione
026500000000     D F01             C                   CONST(X'31')
026600000000     D F02             C                   CONST(X'32')
026700000000     D F03             C                   CONST(X'33')
026800000000     D F04             C                   CONST(X'34')
026900000000     D F05             C                   CONST(X'35')
027000000000     D F06             C                   CONST(X'36')
027100000000     D F07             C                   CONST(X'37')
027200000000     D F08             C                   CONST(X'38')
027300000000     D F09             C                   CONST(X'39')
027400000000     D F10             C                   CONST(X'3A')
027500000000     D F11             C                   CONST(X'3B')
027600000000     D F12             C                   CONST(X'3C')
027700000000     D F13             C                   CONST(X'B1')
027800000000     D F14             C                   CONST(X'B2')
027900000000     D F15             C                   CONST(X'B3')
028000000000     D F16             C                   CONST(X'B4')
028100000000     D F17             C                   CONST(X'B5')
028200000000     D F18             C                   CONST(X'B6')
028300000000     D F19             C                   CONST(X'B7')
028400000000     D F20             C                   CONST(X'B8')
028500000000     D F21             C                   CONST(X'B9')
028600000000     D F22             C                   CONST(X'BA')
028700000000     D F23             C                   CONST(X'BB')
028800000000     D F24             C                   CONST(X'BC')
028900000000     D ENTER           C                   CONST(X'F1')
029000000000     D ROLDWN          C                   CONST(X'F4')
029100000000     D ROLLUP          C                   CONST(X'F5')
029200000000     D*-------------
029300000000     D* Attributi video
029400000000     D Normale         C                   Const(X'32')
029500000000     D Protetto        C                   Const(X'A0')
029600000000     D Annul           C                   CONST(X'A1')
029700000000     D*-------------
029800000000     D* Sottotitolo di testata
029900000000     D SOTTIT          C                   CONST('SDGMSG    *LIBL-
030000000000     D                                          SDC0062')
030100000000     D SOTTITD1        C                   CONST('               -
030200000000     D                                                 ')
030201151201R195 D*-------------
030202151201  "  D* Sottotitolo di testata
030203151201  "  D* "Quadri Spesometro"
030204151201  "  D SOTTITT1        C                   CONST('SDGMSG    *LIBL-
030205151201  "  D                                          SDC0093')
030206151201  "  D* Sottotitolo di testata Black List
030207151201  "  D* "Quadri Acquisti da RSM"
030208151201  "  D SOTTITT2        C                   CONST('SDGMSG    *LIBL-
030209151201  "  D                                          SDC0094')
030210151201  "  D* Sottotitolo di testata Black List
030211151201  "  D* "Quadri Black List"
030212151201  "  D SOTTITT3        C                   CONST('SDGMSG    *LIBL-
030213151201R195 D                                          SDC0095')
030300000000     D*-------------
030400000000     D* Variabili per gestione videate
030500000000     D $Gest           S              2A
030600000000     D $Fine           S              1A
030700000000     D $InzD1          S              1A
030800000000     D $InzS1          S              1A
030900000000     D $InzD2          S              1A
031000000000     D $LastG          S              2A
031100000000     D $LastV          S              2A
031200000000     D $EndFile        S              1A
031300000000     D $Esci           S              1A
031400000000     D $RcdOK          S              1A
031500000000     D $Unique         S              1A
031600000000     D*-------------
031700000000     D* Variabili appoggio sempre presenti in tutte le anagrafiche
031800000000     D S1Nrr           S                   like(C1Rcd)
031900000000     D WSfl            S                   like(C1Nrr)
032000000000     D WMax            S                   like(C1Rcd)
032100000000     D WPag            S                   like(C1Rcd)
032200000000     D WInzS1          S                   like($InzS1)
032300130829     D WOpr            S                   like(OprG77)
032400000000     D CurR            S              3S 0
032500000000     D CurC            S              3S 0
032600000000     D Win             S             99A
032700000000     D W1Pos           S                   like(C1Pos)
032800000000     D WINKR           S              1A
032900000000     D WExf            S              1A
033000000000     D WSogg           S                   Like(DVSSogg)
033100000000     D*-------------
033200000000     D* Indice della schiera tasti funzione
033300000000     D PosFun          S              2  0
033400000000     D*-------------
033500000000     D*  Indici
033600000000     D X               S              5S 0
033700000000     D Y               S              5S 0
033800000000     D* Indice della schiera per le opzioni personalizzate
033900000000     D PosOpz          S              2  0
034000000000     D* numero max opzioni
034100000000     D NumOpz          S              2  0 Inz($DIMOP)
034200000000     D* numero effettive opzioni
034300000000     D OpzLun          S              2  0
034400000000     D*-------------
034500000000     D* ricerca soggetti
034600000000     D ANA040DS      E DS                  INZ
034700000000     D*-------------
034800000000     D* Campi di lavoro
034801160129R195 D $Continua       S              1A
034802160129R195 D $MsgCd1         S                    Like(XMSGCd1)
034900000000     D*-------------
035000000000     D* Reperimento nome PGM
035100000000     D STATUS         SDS           333
035200000000     D  DSPGM            *PROC
035300000000     D  PARMS            *PARMS
035400000000     D*-------------
035500000000     D* DS esterna per dati soggetti
035600000000     D DVASOG        E DS                  INZ
035700000000     D*-------------
035800000000     D* Definizioni parametri driver anagrafici
035900000000     D Dvaric          S              1
036000000000     D DvaSocieta      S              3
036100000000     D DvaUnita        S              8
036200000000     D DvaStrutt       S             10
036300000000     D DvaDtRif        S               D
036400000000     D DvaLenOut       S              9B 0
036500000000     D DvaSnatura      S              1
036600000000     D DvaCodice       S              8
036700000000     D DvaLineav       S              3
036800000000     D DvaFiliale      S              3  0
036900000000     D DvaSogg         S              8
037000000000     D DvaTpInd        S              2
037100000000     D DvaCdInd        S              3
037200000000     D DvaErrore       S              1
037300000000     D DvaOutput       S           4000
037400000000     D*-------------
037500000000     D* parametri per chiamata al driver dei contatti
037600000000     D $Errore         S              1
037700000000     D $Opz            S              1
037800000000     D $Len            S              9B 0
037900000000     D $Refresh        S              1
038000000000     D Esito           S              1
038100000000     D GesMsg          S              1
038200000000     D Output          S           4000
038300000000     D LenOut          S              9B 0
038400000000     D Struttura       S             10
038500000000     C*----------------------------------------------------*
038600000000     C*                MAIN LINE PROGRAM
038700000000     C*----------------------------------------------------*
038800000000     C*
038900000000     C* inizializzazione variabili
039000000000     C                   EXSR      INZVAR
039100000000     C*
039200000000     C     $FINE         DOWEQ     *OFF
039300000000     C     $GEST         CASEQ     'D1'          GESD1                          Parzializz.
039400000000     C     $GEST         CASEQ     'S1'          GESS1                          Lista
039500130925     C     $GEST         CASEQ     'S2'          GESS1                          Lista
039600000000     C     $GEST         CASEQ     'D2'          GESD2                          Valori ass.
039700000000     C                   ENDCS
039800000000     C                   ENDDO
039900000000     C* fine programma
040000000000     C                   EXSR      ENDPGM
040100000000     C*
040200000000     C/EJECT
040300000000     C************************************************************
040400000000     C* FINE PROGRAMMA
040500000000     C************************************************************
040600000000     C     ENDPGM        BEGSR
040700000000     C*
040800000000     C* passaggio dati al pgm chiamante
040900130829    >C                   MOVEL     WOPR          OPRG77                                     ne S1
041000000000     C*
041100000000     C* Se pgm Guida non è stato richiamato posso chiudere in LR
041200130829    >C     OPZG77        IFEQ      *BLANK
041300000000     C                   SETON                                            LR
041400000000     C* altrimenti chiudo in RT
041500000000     C                   ELSE
041600000000     C                   SETON                                            RT
041700000000     C                   ENDIF
041800000000     C                   RETURN
041900000000     C*
042000000000     C                   ENDSR
042100000000     C/EJECT
042200000000     C************************************************************
042300000000     C* RICERCHE
042400000000     C************************************************************
042500000000     C     SEARCH        BEGSR
042600000000     C*
042700000000     C* determino Riga/Colonna del cursore
042800000000     C                   MOVE      NRG           R$$
042900000000     C                   MOVE      NCL           C$$
043000000000     C                   Z-ADD     RIRI          CURR
043100000000     C                   Z-ADD     COCO          CURC
043200000000     C*
043300000000     C                   EXSR      CallAllinea
043400000000     C*
043500000000   B1C                   SELECT
043600000000     C*
043700000000     C*  Codice IVA
043800000000     C     H1NMFL        WHENEQ    'D1PARTIVA'
043900000000     C                   EXSR      F4_PartIva
044000000000     C*
044100000000     C*  Codice fiscale
044200000000     C     H1NMFL        WHENEQ    'D1CDFISC'
044300000000     C                   EXSR      F4_CdFisc
044400000000     C*
044500000000     C*  Soggetto
044600000000     C     H1NMFL        WHENEQ    'D1SOGG'
044700000000     C                   EXSR      F4_Sogg
044800000000     C*
044900000000     C* FMTD1 - Dati attivi/non attivi
045000000000     C     H1NMFL        WHENEQ    'D1ANN  '
045100000000     C                   EXSR      F4_Ann
045200000000     C*
045300000000     C* FMTD1 - Tipo annullamento
045400000000     C     H1NMFL        WHENEQ    'D1TPANN'
045500000000     C                   EXSR      F4_TpAnn
045600000000     C*
045700000000     C* ricerca non abilitata in questa posizione
045800000000   X1C                   OTHER
045900000000     C                   SETON                                        9899
046000000000     C*
046100000000   E1C                   ENDSL
046200000000     C*
046300000000     C* imposto pos. del cursore
046400000000     C                   Z-ADD     CURR          H1RIGA
046500000000     C                   Z-ADD     CURC          H1COLO
046600000000     C*
046700000000     C                   MOVE      *HIVAL        $LASTV
046800000000     C*
046900000000     C                   ENDSR
047000000000     C/EJECT
047100000000     C************************************************************
047200000000     C* Richiamo programma per allineare i codici
047300000000     C************************************************************
047400000000     C     CallAllinea   BEGSR
047500000000     C*
047600000000     C     D1SOGG        IFNE      *BLANK
047700000000     C                   CALLb     'XALLINEA'
047800000000     C                   PARM                    D1SOGG
047900000000     C                   PARM      8             LEN               5 0
048000000000     C                   PARM                    XSCINP
048100000000     C                   ENDIF
048200000000     C*
048300000000     C                   ENDSR
048400000000      /EJECT
048500000000     C************************************************************
048600000000     C* Ricerca codice IVA
048700000000     C************************************************************
048800000000     C     F4_PartIva    BEGSR
048900000000     C*
049000000000     C                   RESET                   ANA040DS
049100000000     C*
049200000000     C                   MOVEL     '01'          OPZ040
049300000000     C                   MoveL     D1PartIva     IVA040
049400000000     C                   MOVEL     ANA040DS      KPJBU
049500000000     C                   CALL      'ANA040R'
049600000000     C                   PARM                    KPJBA
049700000000     C*
049800000000     C                   MOVEL     KPJBU         ANA040DS
049900000000     C*
050000000000     C                   if        Ret040     <> '2'
050100000000     C*
050200000000     C                   MoveL     Cod040        WSogg
050300000000     C                   EXSR      CallDvaSog
050400000000     C*
050500000000     C                   if        DVAErrore  <> '1'
050600000000     C*
050700000000     C                   Eval      %subst(DVASOG:1:DVALENOUT)
050800000000     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
050900000000     C*
051000000000     C                   MoveL     DVSPartIVA    D1PartIva
051100000000     C*
051200000000     C                   endif
051300000000     C*
051400000000     C                   endif
051500000000     C*
051600000000     C                   ENDSR
051700000000      /EJECT
051800000000     C************************************************************
051900000000     C* Ricerca codice fiscale
052000000000     C************************************************************
052100000000     C     F4_CdFisc     BEGSR
052200000000     C*
052300000000     C                   RESET                   ANA040DS
052400000000     C*
052500000000     C                   MOVEL     '01'          OPZ040
052600000000     C                   MoveL     D1CdFisc      CdF040
052700000000     C                   MOVEL     ANA040DS      KPJBU
052800000000     C                   CALL      'ANA040R'
052900000000     C                   PARM                    KPJBA
053000000000     C*
053100000000     C                   MOVEL     KPJBU         ANA040DS
053200000000     C*
053300000000     C                   if        Ret040     <> '2'
053400000000     C*
053500000000     C                   MoveL     Cod040        WSogg
053600000000     C                   EXSR      CallDvaSog
053700000000     C*
053800000000     C                   if        DVAErrore  <> '1'
053900000000     C*
054000000000     C                   Eval      %subst(DVASOG:1:DVALENOUT)
054100000000     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
054200000000     C*
054300000000     C                   MoveL     DVSCdFisc     D1CdFisc                       *
054400000000     C*
054500000000     C                   endif
054600000000     C*
054700000000     C                   endif
054800000000     C*
054900000000     C                   ENDSR
055000000000     C/EJECT
055100000000     C************************************************************
055200000000     C* Ricerca codice soggetto
055300000000     C************************************************************
055400000000     C     F4_Sogg       BEGSR
055500000000     C*
055600000000     C                   RESET                   ANA040DS
055700000000     C*
055800000000     C                   MOVEL     '01'          OPZ040
055900000000     C                   MoveL     D1Sogg        Cod040
056000000000     C                   MOVEL     ANA040DS      KPJBU
056100000000     C                   CALL      'ANA040R'
056200000000     C                   PARM                    KPJBA
056300000000     C*
056400000000     C                   MOVEL     KPJBU         ANA040DS
056500000000     C*
056600000000     C                   if        Ret040     <> '2'
056700000000     C                   MoveL     Cod040        D1Sogg
056800000000     C                   endif
056900000000     C*
057000000000     C                   ENDSR
057100000000     C/EJECT
057200000000     C************************************************************
057300000000     C* Ricerca dati attivi/non attivi
057400000000     C************************************************************
057500000000     C     F4_Ann        BEGSR
057600000000     C*
057700000000     C                   reset                   ana964ds
057800000000     C*
057900000000     C                   movel     '01'          Opz964
058000000000     C                   movel     'ANDIZ000 '   Trc964
058100000000     C                   movel     'DSPATT   '   Cam964
058200000000     C*
058300000000     C                   movel     ana964ds      Kpjbu
058400000000     C                   call      'ANA964R'
058500000000     C                   parm                    Kpjba
058600000000     C                   movel     Kpjbu         Ana964ds
058700000000     C*
058800000000     C                   if        Ret964     <> '2'
058900000000     C                   movel     Val964        D1Ann
059000000000     C                   endif
059100000000     C*
059200000000     C                   ENDSR
059300000000     C/EJECT
059400000000     C************************************************************
059500000000     C* Ricerca tipo annullamento
059600000000     C************************************************************
059700000000     C     F4_TpAnn      BEGSR
059800000000     C*
059900000000     C                   reset                   ana964ds
060000000000     C*
060100000000     C                   movel     '01'          Opz964
060200130829     C                   movel     'SDGSCF000'   Trc964
060300130829     C                   movel     'SCFTPANN'    Cam964
060400000000     C*
060500000000     C                   movel     ana964ds      Kpjbu
060600000000     C                   call      'ANA964R'
060700000000     C                   parm                    Kpjba
060800000000     C                   movel     Kpjbu         Ana964ds
060900000000     C*
061000000000     C                   if        Ret964     <> '2'
061100000000     C                   movel     Val964        D1TpAnn
061200000000     C                   endif
061300000000     C*
061400000000     C                   ENDSR
061500000000     C/EJECT
061600000000     C************************************************************
061700000000     C* RIEMPIMENTO OPZIONI
061800000000     C************************************************************
061900000000     C     RIEOPZ        BEGSR
062000000000     C*
062100000000     C                   CALLB     'X04OPMSG'
062200000000     C* input
062300000000     C                   PARM                    $TX
062400000000     C                   PARM                    $FL
062500000000     C                   PARM                    WCOZ              3 0
062600000000     C                   PARM                    WMXO              3 0
062700000000     C* output
062800000000     C                   PARM                    WOPZ1            76
062900000000     C                   PARM                    WOPZ2            76
063000000000     C                   PARM                    FF23              1
063100000000     C* input
063200000000     C                   PARM                    $L01              3 0
063300000000     C                   PARM                    $L02              3 0
063400000000     C*
063500000000     C                   PARM                    $ALLOPT         960
063600000000     C*
063700000000     C                   ENDSR
063800000000     C/EJECT
063900000000     C************************************************************
064000000000     C* RIEMPIMENTO TASTI FUNZIONE
064100000000     C************************************************************
064200000000     C     RIEKEY        BEGSR
064300000000     C*
064400000000     C                   CALLB     'XKEYMSG'
064500000000     C* input
064600000000     C                   PARM                    $FX
064700000000     C                   PARM                    $FC
064800000000     C                   PARM                    $FR
064900000000     C                   PARM                    $ULK              3 0
065000000000     C* output
065100000000     C                   PARM                    $KEY1            79
065200000000     C                   PARM                    $KEY2            79
065300000000     C* input
065400000000     C                   PARM                    $L01              3 0
065500000000     C                   PARM                    $L02              3 0
065600000000     C*
065700000000     C                   PARM                    $ALLFUNCT       480
065800000000     C*
065900000000     C                   ENDSR
066000000000     C/EJECT
066100000000     C************************************************************
066200000000     C* GESTIONE EMISSIONE TASTI FUNZIONE
066300000000     C************************************************************
066400000000     C     GESPIE        BEGSR
066500000000     C*
066600000000     C                   SELECT
066700000000     C* Parzializzazioni
066800000000     C     $GEST         WHENEQ    'D1'
066900000000     C     $ULKD1        IFEQ      0
067000000000     C* riempimento opzioni
067100000000     C                   EXSR      F24D1
067200000000     C                   ELSE
067300000000     C                   WRITE     FMTZ1
067400000000     C                   ENDIF
067500000000     C* Lista
067600000000     C     $GEST         WHENEQ    'S1'
067700130925     C     $GEST         OREQ      'S2'
067800000000     C     $ULKS1        IFEQ      0
067900000000     C                   EXSR      F24S1
068000000000     C                   ELSE
068100000000     C                   WRITE     FMTZ2
068200000000     C                   ENDIF
068300000000     C* Modifica valori assunti
068400000000     C     $GEST         WHENEQ    'D2'
068500000000     C     $ULKD2        IFEQ      0
068600000000     C                   EXSR      F24D2
068700000000     C                   ELSE
068800000000     C                   WRITE     FMTZ3
068900000000     C                   ENDIF
069000000000     C                   ENDSL
069100000000     C*
069200000000     C                   ENDSR
069300000000     C/EJECT
069400000000     C************************************************************
069500000000     C* GESTIONE PARZIALIZZAZIONI
069600000000     C************************************************************
069700000000     C     GESD1         BEGSR
069800000000     C*
069900000000     C* inizializzazione videata
070000000000     C     $INZD1        IFEQ      *ON
070100000000     C                   EXSR      INZD1
070200000000     C                   MOVE      *OFF          $INZD1
070300000000     C                   ENDIF
070400000000     C* emissione piede videata se proveniente da altra
070500000000     C* salvataggio valore formato di provenienza
070600000000     C     $LASTV        IFNE      'D1'
070700000000     C*
070800000000     C* recupero il sottotitolo (se c'è)
070900000000     C                   Clear                   D1TIT
071000000000     C     SOTTITD1      IFNE      *BLANK
071100000000     C                   MOVEL     SOTTITD1      IDMSG
071200000000     C                   CALLB     'XRTVM'
071300000000     C                   PARM                    IDMSG            27
071400000000     C                   PARM                    TXTMSG          100
071500000000     C                   PARM                    ERRMSG            1
071600000000     C                   PARM      *Off          CTRMSG            1            centratura
071700000000     C                   PARM      15            LENMSG            3 0          lun output.
071800000000     C     ERRMSG        IFNE      *ON
071900000000     C                   MOVEL     TXTMSG        D1TIT
072000000000     C                   ELSE
072100000000     C                   MOVEL     *ALL'?'       D1TIT
072200000000     C                   ENDIF
072300000000     C                   ENDIF
072400000000     C*
072500000000     C* salvo stato F17 per sapere cosa fare in caso di F12
072600000000     C                   MOVE      *INKR         WINKR
072700000000     C                   WRITE     FMTT1
072800000000     C                   EXSR      GESPIE
072900000000     C                   MOVE      $LASTV        $LASTG
073000000000     C                   MOVE      'D1'          $LASTV
073100000000     C                   ENDIF
073200000000     C*
073300000000     C* se esistono errori sulla videata
073400000000     C* emetto la write del formato a indicatori spenti per vedere
073500000000     C* le eventuali decodifiche
073600000000     C     *IN99         IFEQ      *ON
073700000000     C                   MOVEA     *IN           WIN
073800000000     C                   MOVE      *ALL'0'       IN5098           49
073900000000     C                   MOVEA     IN5098        *IN(50)
074000000000     C                   WRITE     FMTD1
074100000000     C                   MOVEA     WIN           *IN
074200000000     C                   ENDIF
074300000000     C*
074400000000     C                   EXFMT     FMTD1
074500000000     C*
074600000000     C* azzero la posizione del cursore
074700000000     C                   Z-ADD     0             H1RIGA
074800000000     C                   Z-ADD     0             H1COLO
074900000000     C*
075000000000     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
075100000000     C* INPUT
075200000000     C* $FC1 = Abilitazioni videata D1
075300000000     C* OUTPUT
075400000000     C* *IN97 + *IN99 Errori
075500000000     C                   MOVEA     $FC1          $FC
075600000000     C                   EXSR      CTRKEY
075700000000     C*
075800000000     C     *IN99         IFEQ      *OFF
075900000000     C*
076000000000     C                   SELECT
076100000000     C* F2=Decodifica
076200000000     C     $TASTO        WHENEQ    F02
076300000000     C                   EXSR      F02D1
076400000000     C* F3=Fine
076500000000     C     $TASTO        WHENEQ    F03
076600000000     C                   EXSR      F03D1
076700000000     C* F4=Ricerca
076800000000     C     $TASTO        WHENEQ    F04
076900000000     C                   EXSR      SEARCH
077000000000     C* F5=Refresh
077100000000     C     $TASTO        WHENEQ    F05
077200000000     C                   EXSR      F05D1
077300000000     C* F8=Successivo
077400000000     C     $TASTO        WHENEQ    F08
077500000000     C                   EXSR      F08D1
077600000000     C* F10=Immissione
077700000000     C     $TASTO        WHENEQ    F10
077800000000     C                   EXSR      F10D1
077900000000     C* F12=Ritorno
078000000000     C     $TASTO        WHENEQ    F12
078100000000     C                   EXSR      F12D1
078200000000     C* F18=Modifica valori assunti
078300000000     C     $TASTO        WHENEQ    F18
078400000000     C                   EXSR      F18D1
078500000000     C* F24=Altri tasti
078600000000     C     $TASTO        WHENEQ    F24
078700000000     C                   EXSR      F24D1
078800000000     C*
078900000000     C                   OTHER
079000000000     C                   EXSR      CTRD1
079100000000     C     *IN99         IFEQ      *OFF
079200000000     C* definisco l'ordinamento
079300000000     C                   EXSR      DEFORD
079400000000     C* richiamo la lista
079500130903?????C                   MOVE      'S1'          $GEST
079600000000     C                   MOVE      *ON           $INZS1
079700000000     C                   MOVE      *BLANK        W1POS
079800000000     C                   ENDIF
079900000000     C*
080000000000     C                   ENDSL
080100000000     C*
080200000000     C                   ENDIF
080300000000     C*
080400000000     C                   ENDSR
080500000000     C/EJECT
080600000000     C************************************************************
080700000000     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
080800000000     C************************************************************
080900000000     C     INZD1         BEGSR
081000000000     C*
081100000000     C                   CLEAR                   FMTD1
081200000000     C*
081300000000     C* Se pgm  richiamato con parametri forniti , li imposto nelle
081400000000     C*    parzializzazioni
081500000000     C                   SELECT
081600000000     C* il pgm è richiamato con chiave univoca per gestione POSTA
081700000000     C     $Unique       WHENEQ    *ON
081800000000     C* il pgm è richiamato e
081900000000     C* sono stati passati dei valori di parzializzazione ma non
082000000000     C* la chiave univoca
082100130829     C     OPZG77        WHENNE    *BLANK
082200000000     C* imposto i valori passati sui campi a video
082300130829     C                   MOVEL     AnnoG77       D1Anno
082400130829     C                   MOVEL     StatoG77      D1Stato
082500130829     C                   MOVEL     PartIvaG77    D1PartIva
082600130829     C                   MOVEL     CdFiscG77     D1CdFisc
082700130829     C                   MOVEL     AnnG77        D1Ann
082800130829     C                   MOVEL     TpAnnG77      D1TpAnn
082900000000     C* il pgm non è richiamato
083000130829     C     OPZG77        WHENEQ    *BLANK
083100000000     C                   Eval      D1Ann       = *Blank
083200000000     c                   ENDSL
083300000000      *
083400000000     c                   ENDSR
083500000000      /EJECT
083600000000      ************************************************************
083700000000      * GESTIONE F02 VIDEO D1
083800000000      ************************************************************
083900000000     C     F02D1         BEGSR
084000000000     C*
084100000000     C* per decodificare la videata richiamo la routine dei ctrl
084200000000     C* e poi disattivo gli eventuali messaggi di errore
084300000000     C                   EXSR      CTRD1
084400000000     C*
084500000000     C                   MOVE      *ALL'0'       IN5098
084600000000     C                   MOVEA     IN5098        *IN(50)
084700000000     C*
084800000000     C                   ENDSR
084900000000     C/EJECT
085000000000     C************************************************************
085100000000     C* GESTIONE F03 VIDEO D1
085200000000     C************************************************************
085300000000     C     F03D1         BEGSR
085400000000     C*
085500000000     C                   MOVE      *ON           $FINE
085600130829    >C                   MOVE      '1'           RETG77
085700000000     C* fine programma
085800000000     C                   EXSR      ENDPGM
085900000000     C*
086000000000     C                   ENDSR
086100000000     C/EJECT
086200000000     C************************************************************
086300000000     C* GESTIONE F05 VIDEO D1
086400000000     C************************************************************
086500000000     C     F05D1         BEGSR
086600000000     C*
086700000000     C                   MOVE      *ON           $INZD1
086800000000     C*
086900000000     C                   ENDSR
087000000000     C/EJECT
087100000000     C************************************************************
087200000000     C* GESTIONE F08 VIDEO D1
087300000000     C************************************************************
087400000000     C     F08D1         BEGSR
087500000000     C*
087600000000     C                   MOVE      *ON           $FINE
087700130829    >C                   MOVE      '0'           RETG77
087800000000     C*
087900000000     C                   ENDSR
088000000000     C/EJECT
088100000000     C************************************************************
088200000000     C* GESTIONE F10 VIDEO D1
088300000000     C************************************************************
088400000000     C     F10D1         BEGSR
088500000000     C*
088600000000     C*
088700000000     C                   ENDSR
088800000000     C/EJECT
088900000000     C************************************************************
089000000000     C* GESTIONE F12 VIDEO D1
089100000000     C************************************************************
089200000000     C     F12D1         BEGSR
089300000000     C*
089400000000     C*  se la 1a videata è la lista oppure sono arrivato qui con F17,
089500000000     C*  per F12 torno alla lista
089600000000     C     $LOP          IFEQ      2
089700000000     C     WINKR         OREQ      *ON
089800130903???? C                   MOVE      'S1'          $GEST
089900000000     C*  altrimenti, per F12 torno a menu
090000000000     C                   ELSE
090100000000     C                   MOVE      *ON           $FINE
090200130829    >C                   MOVE      '2'           RETG77
090300000000     C                   ENDIF
090400000000     C*
090500000000     C                   ENDSR
090600000000     C/EJECT
090700000000     C************************************************************
090800000000     C* GESTIONE F18 VIDEO D1
090900000000     C************************************************************
091000000000     C     F18D1         BEGSR
091100000000     C*
091200000000     C                   MOVE      'D2'          $GEST
091300000000     C                   MOVEL     *ON           $INZD2
091400000000     C*
091500000000     C                   ENDSR
091600000000     C/EJECT
091700000000     C************************************************************
091800000000     C* GESTIONE F24 VIDEO D1
091900000000     C* input
092000000000     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
092100000000     C* $FX1 =Testi tasti funzione
092200000000     C* $FC1 = Abilitazioni
092300000000     C* $FR1 = Inversioni di fondo
092400000000     C************************************************************
092500000000     C     F24D1         BEGSR
092600000000     C*
092700000000     C                   MOVEA     $FX1          $FX
092800000000     C                   MOVEA     $FC1          $FC
092900000000     C                   MOVEA     $FR1          $FR
093000000000     C                   Z-ADD     $ULKD1        $ULK
093100000000     C                   Z-ADD     $D1L01        $L01
093200000000     C                   Z-ADD     $D1L02        $L02
093300000000     C                   EXSR      RIEKEY
093400000000     C                   Z-ADD     $ULK          $ULKD1            3 0
093500000000     C                   MOVEA     $FC           $FC1
093600000000     C                   MOVEL     $KEY1         Z1KE1
093700000000     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
093800000000     C                   MOVEL     $KEY2         Z1KE2
093900000000     C                   WRITE     FMTZ1
094000000000     C*
094100000000     C*
094200000000     C                   ENDSR
094300000000     C/EJECT
094400000000     C************************************************************
094500000000     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
094600000000     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
094700000000     C* 93 94 96 97 98
094800000000     C************************************************************
094900000000     C     CTRD1         BEGSR
095000000000     C*
095100000000     C                   SETOFF                                       99
095200000000     C*
095300000000     C                   EXSR      CtrAnno
095400000000     C*
095500000000     C                   EXSR      CtrSogg
095600000000     C*
095700000000     C                   EXSR      CtrAnn
095800000000     C*
095900000000     C                   ENDSR
096000000000     C/EJECT
096100000000     C***********************************************************
096200000000     C* Controllo anno di competenza
096300000000     C***********************************************************
096400000000     C     CtrAnno       BEGSR
096500000000     C*
096600000000     C                   if        D1Anno = 0
096700000000     C                   SetOn                                        5399
096800000000     C                   endif
096900000000     C*
097000000000     C                   ENDSR
097100000000     C/EJECT
097200000000     C***********************************************************
097300000000     C* Controllo codice soggetto
097400000000     C***********************************************************
097500000000     C     CtrSogg       BEGSR
097600000000     C*
097700000000     C                   Clear                   D1SoggD
097800000000     C                   if        D1Sogg     <> *Blank
097900000000     C*
098000000000     C     D1SOGG        IFNE      *BLANK
098100000000     C                   CALLb     'XALLINEA'
098200000000     C                   PARM                    D1SOGG
098300000000     C                   PARM      8             LEN               5 0
098400000000     C                   PARM                    XSCINP
098500000000     C                   ENDIF
098600000000     C*
098700000000     C                   MoveL     D1Sogg        WSogg
098800000000     C                   EXSR      CallDvaSog
098900000000     C*
099000000000     C                   if        DVAErrore  <> '1'
099100000000     C*
099200000000     C                   Eval      %subst(DVASOG:1:DVALENOUT)
099300000000     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
099400000000     C*
099500000000     C                   MoveL     DVSDes        D1SoggD
099600000000     C*
099700000000     C                   else
099800000000     C*
099900000000     C                   SetOn                                        5599
100000000000     C*
100100000000     C                   endif
100200000000     C*
100300000000     C                   endif
100400000000     C*
100500000000     C                   ENDSR
100600000000     C/EJECT
100700000000     C************************************************************
100800000000     C* Chiamata al driver dei soggetti
100900000000     C************************************************************
101000000000     C     CallDvaSog    BEGSR
101100000000     C*
101200000000     C                   EVAL      DVALenOut   = %Size(DVASOG)
101300000000     C*
101400000000     C                   CALLB     'NDDVASOG'
101500000000     C                   PARM      '1'           DVARIC
101600000000     C                   PARM      XSCSOC        DVASOCIETA
101700000000     C                   PARM                    DVAUNITA
101800000000     C                   PARM      'DVASOG    '  DVASTRUTT
101900000000     C                   PARM                    DVADTRIF
102000000000     C                   PARM                    DVALENOUT
102100000000     C* Se si accede con il codice cli/for impostare i campi seguenti
102200000000     C* se si accede con il soggetto ometterli e impostare gli altri
102300000000     C*                  PARM                    DVASNATURA
102400000000     C*                  PARM                    DVACODICE
102500000000     C*                  PARM                    DVALINEAV
102600000000     C*                  PARM                    DVAFILIALE
102700000000     C                   PARM                    *omit
102800000000     C                   PARM                    *omit
102900000000     C                   PARM                    *omit
103000000000     C                   PARM                    *omit
103100000000     C                   PARM      WSogg         DVSSOGG
103200000000     C                   PARM      *Blank        DVSTPIND
103300000000     C                   PARM      *Blank        DVSCDIND
103400000000     C                   PARM                    DVAERRORE
103500000000     C                   PARM                    DVAOUTPUT
103600000000     C*
103700000000     C                   ENDSR
103800000000     C/EJECT
103900000000     C***********************************************************
104000000000     C* Controllo dati annullati
104100000000     C***********************************************************
104200000000     C     CtrAnn        BEGSR
104300000000     C*
104400000000     C                   Clear                   D1AnnD
104500000000     C*
104600000000     C                   Clear                   X07ValDs
104700000000     C                   Move      '1'           X07Ric
104800000000     C                   Move      *Off          X07All
104900000000     C                   Movel     'ANDIZ000 '   X07TRc
105000000000     C                   Movel     'DSPATT   '   X07Cam
105100000000     C                   Movel     D1Ann         X07VAL
105200000000     C                   Callb     'X07VALR'
105300000000     C                   Parm                    X07ValDs
105400000000     C*
105500000000     C     X07ERR        IFEQ      *Off
105600000000     C                   MOVEL     X07DES        D1AnnD
105700000000     C                   else
105800000000     C                   SetOn                                        5699
105900000000     C                   endif
106000000000     C*
106100000000     C                   Clear                   D1TpAnnD
106200000000     C                   if        D1TpAnn    <>  *blank
106300000000     C*  solo se richiesti solo gli annullati oppure con annullati
106400000000     C                   if        D1Ann      =   '1'    or
106500000000     C                             D1Ann      =   '3'
106600000000     C                   exsr      CtrTpAnn
106700000000     C                   else
106800000000     C                   SetOn                                        5899
106900000000     C                   endif
107000000000     C                   endif
107100000000     C*
107200000000     C                   ENDSR
107300000000     C/EJECT
107400000000     C***********************************************************
107500000000     C* Controllo tipo annullamento
107600000000     C***********************************************************
107700000000     C     CtrTpAnn      BEGSR
107800000000     C*
107900000000     C                   Clear                   X07ValDs
108000000000     C                   Move      '1'           X07Ric
108100000000     C                   Move      *Off          X07All
108200130829     C                   Movel     'SDGSCF000'   X07TRc
108300130829     C                   Movel     'SCFTPANN'    X07Cam
108400000000     C                   Movel     D1TpAnn       X07VAL
108500000000     C                   Callb     'X07VALR'
108600000000     C                   Parm                    X07ValDs
108700000000     C*
108800000000     C     X07ERR        IFEQ      *Off
108900000000     C                   MOVEL     X07DES        D1TpAnnD
109000000000     C                   else
109100000000     C                   SetOn                                        5799
109200000000     C                   endif
109300000000     C*
109400000000     C                   ENDSR
109500000000     C/EJECT
109600000000     C************************************************************
109700000000     C* DEFINIZIONE DELL'ORDINAMENTO
109800000000     C************************************************************
109900000000     C     DEFORD        BEGSR
110000000000     C*
110100000000     C* definisco l'ordinamento della lista a seconda dei valori
110200000000     C* impostati nel video di parzializzazione
110300000000     C                   Z-ADD     1             $ORD
110400000000     C*
110500000000     C                   SELECT
110600000000     C* imposto l'ordinamento 99 per ottenere in selezione solo il
110700000000     C* rcd con la chiave passata
110800000000     C     $Unique       WHENEQ    *ON
110900000000     C                   Z-ADD     99            $ORD
111000000000     C* nel caso non sia stato digitato alcun valore in parzializzazione
111100000000     C* utilizzerò l'ordinamento precedentemente memorizzato
111200000000     C* (temporaneamente o definitivamente) nei valori assunti
111300000000     C                   OTHER
111400000000     C                   ENDSL
111500000000     C                   ENDSR
111600000000     C/EJECT
111700000000     C************************************************************
111800000000     C* Chiamata programma con tasto personalizzato
111900000000     C************************************************************
112000000000     C     FPerS1        BEGSR
112100000000     C*
112200000000     C* ATTENZIONE:
112300130829     C* la ds SDGSG77DS è più lunga della KPJBU!!!!!!
112400000000     C*
112500000000     C* salvo la DS di ingresso
112600130829    >C                   MOVEL     SDGSG77DS     $SaveDS
112700000000     C*
112800130829    >C                   MOVEL     SDGSG77DS     KPJBU
112900000000     C                   CALL      $PGF2(PosFun)                        22
113000000000     C                   PARM                    KPJBA
113100000000     C*
113200000000     C* ripristino la DS di ingresso
113300130829    >C                   MOVEL     $SaveDS       SDGSG77DS
113400000000     C*
113500000000     C                   ENDSR
113600000000     C/EJECT
113700000000     C************************************************************
113800000000     C* GESTIONE LISTA
113900000000     C************************************************************
114000000000     C     GESS1         BEGSR
114100000000     C*
114200000000     C* mantengo la situazione 'aperta' o 'chiusa' del sfl drop
114300000000     C                   Eval      *In09     =   C1Mode
114400000000     C*
114500000000     C* inizializzazione videata
114600000000     C     $INZS1        IFEQ      *ON
114700000000     C                   EXSR      INZS1
114800000000     C                   MOVE      *OFF          $INZS1
114900000000     C                   ENDIF
115000000000     C* emissione piede videata se proveniente da altra
115100000000     C* salvataggio valore formato di provenienza
115200000000     C     $LASTV        IFNE      'S1'
115300130925     C     $LASTV        ANDNE     'S2'
115400000000     C                   WRITE     FMTT1
115500000000     C                   EXSR      GESPIE
115600000000     C                   MOVE      $LASTV        $LASTG
115700130903?????C                   MOVE      'S1'          $LASTV
115800000000     C                   ENDIF
115900000000     C*
116000000000     C     WMAX          IFEQ      0
116100000000     C                   WRITE     FMTD3                                        NO RCD
116200000000     C                   ENDIF
116300000000     C* Inizializzazione indice tasti funzione personalizzati
116400000000     C                   Eval      PosFun = 25
116500000000     C*
116600130918     C                   EXSR      RepG12Ksc
116700130903     C                   EXSR      EXFMTC1
116800000000     C*
116900000000     C* azzero la posizione del cursore
117000000000     C                   Z-ADD     0             H1RIGA
117100000000     C                   Z-ADD     0             H1COLO
117200000000     C* Serve per avvertire i pgm di sola finestra di eseguire le operazioni
117300000000     C* di refresh dello sfondo
117400000000     C                   MOVE      '1'           WExf
117500000000     C*
117600000000     C* riemettere il nuovo sfondo del subfile
117700000000     C                   Eval      WA39Iniz    = '0'
117800000000     C*
117900000000     C     C1NRR         IFNE      0
118000000000     C                   Z-ADD     C1NRR         WSFL
118100000000     C                   ENDIF
118200000000     C                   IF        SFLNRR > 0
118300000000     C                   Z-ADD     SFLNRR        C1RCD
118400000000     C                   ENDIF
118500000000     C* inizializzazione indice opzioni personalizzate
118600000000     C                   Eval      PosOpz = 0
118700000000     C* CONTROLLO ABILITAZIONE TASTI
118800000000     C* INPUT
118900000000     C* $FC2 = Abilitazioni videata S1
119000000000     C* OUTPUT
119100000000     C* *IN97 + *IN99 Errori
119200000000     C                   MOVEA     $FC2          $FC
119300000000     C                   EXSR      CTRKEY
119400000000     C*
119500000000     C     *IN99         IFEQ      *OFF
119600000000     C*
119700000000     C                   SELECT
119800000000     C* F3=Fine
119900000000     C     $TASTO        WHENEQ    F03
120000000000     C                   EXSR      F03S1
120100130926     C* F5=Refresh
120200130926     C     $TASTO        WHENEQ    F05
120300130926     C                   EXSR      F05S1
120400000000     C* F8=Successivo
120500000000     C     $TASTO        WHENEQ    F08
120600000000     C                   EXSR      F08S1
120700000000     C* F10=Immissione
120800000000     C     $TASTO        WHENEQ    F10
120900000000     C                   EXSR      F10S1
121000000000     C* F12=Ritorno
121100000000     C     $TASTO        WHENEQ    F12
121200000000     C                   EXSR      F12S1
121300000000     C* F17=Parzializzazioni
121400000000     C     $TASTO        WHENEQ    F17
121500000000     C                   EXSR      F17S1
121600000000     C* F18=Modifica valori assunti
121700000000     C     $TASTO        WHENEQ    F18
121800000000     C                   EXSR      F18S1
121900000000     C* F23=Altre opzioni
122000000000     C     $TASTO        WHENEQ    F23
122100000000     C                   EXSR      F23S1
122200000000     C* F24=Altri tasti
122300000000     C     $TASTO        WHENEQ    F24
122400000000     C                   EXSR      F24S1
122500000000     C* ROLL-UP
122600000000     C     $TASTO        WHENEQ    ROLLUP
122700000000     C                   EXSR      ROLS1
122800000000     C* Personalizzata
122900000000     C     $PGF2(PosFun) Whenne    *BLANK
123000000000     C                   EXSR      FperS1
123100000000     C*
123200000000     C                   OTHER
123300000000     C* CONTROLLO DATI
123400000000     C                   EXSR      CTRC1
123500000000     C     *IN99         IFEQ      *OFF
123600000000     C                   EXSR      CTRS1
123700000000     C                   ENDIF
123800000000     C*
123900000000     C                   ENDSL
124000000000     C*
124100000000     C                   ENDIF
124200000000     C*
124300000000     C                   ENDSR
124400000000     C/EJECT
124500130918     C************************************************************
124600130918     C* Reperisco dati da G12 del conto
124700130918     C************************************************************
124800130918     C     RepG12Ksc     BEGSR
124900130918     C*
125000130919     C                   Eval      $NazKsc     = *Blank
125100130918     C*
125200130918     C                   Eval      WStato      = D1Stato
125300130918     C                   EXSR      RepTabG12
125400130918     C*
125500130930     C                   if        XTBErr      = '0'
125600130918     C*
125700130918     C                   Eval      $NazKsc     = $NazISO
125800130918     C                   if        $NazKsc     = *Blank
125900130918     C                   Eval      $NazKsc     = D1Stato
126000130918     C                   endif
126100130930     C                   endif
126200130918     C*
126300130930     C                   Eval      $UICKsc     = C1CodUIC
126400130930     C*
126500130918     C                   ENDSR
126600130918      /EJECT
126700000000     C************************************************************
126800130903     C* Mostro la lista
126900000000     C************************************************************
127000130903    >C     ExfmtC1       BEGSR
127100000000     C*
127200130903     C                   Select
127300130925     C                   When      Quadro0592  = 'FA'     or
127400130925     C                             Quadro0592  = 'SA'     or
127500130925     C                             Quadro0592  = 'BL'
127600130903     C                   EXFMT     FMTC1
127700130904     C*
127800130904     C                   When      Quadro0592  = 'FE'     or
127900130904     C                             Quadro0592  = 'FR'     or
128000130904     C                             Quadro0592  = 'NE'     or
128100130906     C                             Quadro0592  = 'NR'     or
128200130906     C                             Quadro0592  = 'DF'     or
128300130906     C                             Quadro0592  = 'FN'     or
128400131017R136 C                             Quadro0592  = 'TU'     or
128500130906     C                             Quadro0592  = 'SE'
128600130925     C                   EXFMT     FMTC2
128700130904     C*
128800130906     C                   Other
128900130906     C*
129000130906     C                   Select
129100130925     C*--------------------
129200130906     C* Formato AGGREGATO
129300130906     C                   When      FormatoA08  = 'A'
129400130906     C*
129500130906     C                   EXFMT     FMTC1
129600130906     C*
129700130906     C*--------------------
129800130906     C* Formato DETTAGLIO
129900130906     C                   When      FormatoA08  = 'D'
130000130925     C                   EXFMT     FMTC2
130100130903     C*
130200130906     C                   endSl
130300130906     C*
130400130906     C                   endSl
130500130906     C*
130600130903     C                   ENDSR
130700130903     C/EJECT
130800130903     C************************************************************
130900130903     C* Caricamento OPZIONI
131000130903     C************************************************************
131100130903    >C     CarOpz        BEGSR
131200130903     C*
131300000000     C* Reperimento tasti di funzione
131400000000     C                   EXSR      REPKEY
131500000000     C*
131600000000     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
131700000000     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
131800000000     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
131900000000     C     $ULKD1        IFEQ      0
132000000000     C                   EXSR      F24D1
132100000000     C                   ELSE
132200000000     C                   WRITE     FMTZ1
132300000000     C                   ENDIF
132400000000     C*
132500000000     C                   ENDSR
132600000000     C/EJECT
132700000000     C************************************************************
132800000000     C* INIZIALIZZAZIONE LISTA
132900000000     C************************************************************
133000000000    >C     INZS1         BEGSR
133100000000     C* pulizia SFL
133200000000     C                   SETOFF                                         3031
133300160211     C                   EXSR      WriteC1
133400160211     C                   SETON                                          31
133500140422R133 C*
133600151201R133 C                   Select
133601160105R195 C* Spesometro
133602160105 "   C                   When      TpComG77    = 'E'
133603160105 "   C                   Eval      *In12       = *On
133604160115 "   C*
133605160115R195 C* Acquisti da San Marino
133700151201R133 C                   When      TpComG77    = 'S'
133800160115R133 C                   Eval      *In11       = *On
133801160115R195 C*
133802160115 "   C* Black List
133803160115 "   C                   When      TpComG77    = 'B'
133804160115 "   C                   Eval      *In13       = *On
133805160115R195 C*
133900160115R133 C                   Other
134000140422 "   C                   Eval      *In11       = *Off
134100140422R133 C                   endSl
134200000000     C*
134300000000     C* pulizia del sottotitolo
134400000000     C                   Clear                   D1TIT
134500000000     C*
134600130903     C                   Eval      C1Anno      = D1Anno
134700140422R133 C                   Eval      C1Mese      = MeseG77
134800130903     C                   Eval      C1PartIva   = PartIvaG77
134900130903     C                   Eval      C1CdFisc    = CdFiscG77
135000130903     C                   Eval      C1CogRag    = CogRagG77
135100131003     C                   Eval      H1CogRag    = CogRagG77
135200130911     C                   Eval      C1Stato     = StatoG77
135300130924     C                   Eval      C1CodUIC    = CodUICG77
135400130903     C*
135500130909     C                   Eval      H1Nome      = NomeG77
135600130909     C                   Eval      H1DtNasc    = DtNascG77
135700130909     C                   Eval      H1LocNasc   = LocNascG77
135800130909     C                   Eval      H1PrvNasc   = PrvNascG77
135900130909     C                   Eval      H1Localit   = LocalitG77
136000130909     C                   Eval      H1IndirE    = IndirEG77
136100000000     C*
136200000000     C* riempimento opzioni
136300000000     C                   MOVEA     $TX1          $TX
136400000000     C                   MOVEA     $FL1          $FL
136500000000     C                   Z-ADD     0             WCOZ
136600000000     C                   Z-ADD     0             WMXO
136700000000     C*  equivale al num.di elementi della schiera $TX1
136800000000     C                   Z-add     OpzLun        WMXO
136900000000     C                   Z-ADD     $C1Op1        $L01
137000000000     C                   Z-ADD     $C1Op2        $L02
137100000000     C                   EXSR      RIEOPZ
137200000000     C                   Z-ADD     23            X
137300000000     C                   MOVE      FF23          $FC2(X)
137400000000     C                   MOVEL     WOPZ1         C1OPZ1
137500000000     C                   MOVEL     $ALLOPT       H1ALLOPT
137600000000     C                   MOVEL     WOPZ2         C1OPZ2
137700000000     C                   Z-ADD     WCOZ          C1COZ
137800000000     C*
137900000000     C* CARICAMENTO SFL A PAGINE
138000000000     C*
138100000000     C                   Z-ADD     0             S1NRR
138200000000     C                   Z-ADD     0             WMAX
138300000000     C*
138400000000     C* Posizionamento su file pilota
138500000000     C                   CLEAR                   C1POS
138600000000     C                   EXSR      SETANA
138700000000     C* ripristino i valori del posizionamento
138800000000     C                   MOVE      W1POS         C1POS
138900000000     C*
139000000000     C* Lettura file pilota
139100000000     C                   EXSR      REDANA
139200000000     C*
139300000000     C* se arrivo dalla parzializzazione devo caricare solo la prima pagina
139400000000     C                   IF        $LastV = 'D1'
139500000000     C                   Z-ADD     SFLPAG        WPAG
139600000000     C                   ENDIF
139700000000     C                   EXSR      ROLS1
139800000000     C* imposto il caricamento sfl del numero massimo di righe per pagina
139900000000     C                   Z-ADD     SFLPAG        WPAG
140000000000     C*
140100000000     C                   ENDSR
140200000000     C/EJECT
140300000000     C************************************************************
140400130903     C* Scrittura riga sfl
140500000000     C************************************************************
140600130903     C     WriteC1       BEGSR
140700000000     C*
140800130903     C                   Select
140900130925     C                   When      Quadro0592  = 'FA'     or
141000130925     C                             Quadro0592  = 'SA'     or
141100130925     C                             Quadro0592  = 'BL'
141200130903     C                   Eval      SFLPAG      = 2
141300130903     C                   WRITE     FMTC1
141400130904     C*
141500130904     C*
141600130904     C                   When      Quadro0592  = 'FE'     or
141700130904     C                             Quadro0592  = 'FR'     or
141800130904     C                             Quadro0592  = 'NE'     or
141900130906     C                             Quadro0592  = 'NR'     or
142000130906     C                             Quadro0592  = 'DF'     or
142100130906     C                             Quadro0592  = 'FN'     or
142200131017R136 C                             Quadro0592  = 'TU'     or
142300130906     C                             Quadro0592  = 'SE'
142400130904     C                   Eval      SFLPAG      = 5
142500130925     C                   WRITE     FMTC2
142600130904     C*
142700130903     C                   endSl
142800130903     C*
142900130903     C                   ENDSR
143000130903     C/EJECT
143100130903     C************************************************************
143200130903     C* CARICAMENTO PAGINA LISTA
143300130903     C************************************************************
143400130903     C     ROLS1         BEGSR
143500130903     C*
143600000000     C                   SETOFF                                       32
143700000000     C                   Z-ADD     0             Y
143800000000     C                   Z-ADD     WMAX          S1NRR
143900000000     C*
144000000000     C* Leggo dal file anagrafico per caricare la lista
144100000000     C*
144200000000     C     $EndFile      DOWEQ     *OFF
144300000000     C     Y             ANDLT     WPAG
144400000000     C*
144500130903     C                   EXSR      RieS1
144600000000     C*
144700000000     C                   EXSR      REDANA
144800000000     C*
144900000000     C                   ENDDO
145000000000     C*
145100130903     C                   Z-ADD     S1NRR         WMAX                 30
145200000000     C*
145300000000     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
145400000000     C*
145500000000     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
145600000000     C                   MVR                     RESTO             3 0
145700000000     C     PAGINE        MULT      SFLPAG        C1RCD
145800000000     C     RESTO         IFGT      0
145900000000     C                   ADD       1             C1RCD
146000000000     C                   ELSE
146100000000     C                   SUB       SFLPAG        C1RCD
146200000000     C                   ADD       1             C1RCD
146300000000     C                   ENDIF
146400000000     C*
146500000000     C                   ENDSR
146600000000     C/EJECT
146700130903     C************************************************************
146800130903     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
146900130903     C************************************************************
147000130903     C     SETANA        BEGSR
147100130903     C*
147200130829     C                   RESET                   SDG00502DS
147300130829     C*
147400130919     C                   Eval      Soc0502     = XScSoc
147500000000     C*
147600130919     C                   Eval      Ann0502     = AnnG77
147700130919     C                   Eval      TpAnn0502   = TpAnnG77
147800130829     C                   if        TpAnn0502   = *Blank
147900130829     C                   MoveL     *Hival        TpAnn0502
148000110824     C                   endif
148100110824     C*
148200140401R133 C****               Eval      TpCom0502   = 'E'
148300140401R133 C                   Eval      TpCom0502   = TPCOMG77
148400130919     C                   Eval      Anno0502    = AnnoG77
148500140401R133 C****               Eval      Mese0502    = 0
148600140401R133 C                   Eval      Mese0502    = MeseG77
148601160105R195 C*
148602160105 "   C                   Select
148603160105 "   C*
148604160105 "   C* Comunicazione Spesometro
148605160105 "   C                   When      TpComG77    = 'E'        or
148606160105 "   C* Comunicazione Acquisti da San Marino
148607160105 "   C                             TpComG77    = 'S'
148608160105R195 C*
148700131205R141 C                   if        NoteVarA08  = *Blank     or
148800131205 "   C                             NoteVarA08  = '1'
148900131205 "   C                   Eval      NoteVa0502  = '1'
149000131205 "   C                   else
149100131205 "   C                   Eval      NoteVa0502  = '2'
149200131205R141 C                   endif
149201160105R195 C*
149210160105 "   C*
149211160105 "   C* Comunicazione Black list
149212160105 "   C                   When      TpComG77    = 'B'
149215160105 "   C                   Eval      NoteVa0502  = NoteVarA11
149219160105 "   C*
149220160105R195 C                   endsl
149300130829     C*
149400130902     C                   Eval      CF0502      = CFG77
149500130829     C                   if        CF0502      = *Blank
149600130829     C                   MoveL     *Hival        CF0502
149700130829     C                   endif
149800130902     C*
149900130902     C                   Eval      Sogg0502    = SoggG77
150000130902     C                   if        Sogg0502    = *Blank
150100130902     C                   MoveL     *Hival        Sogg0502
150200130902     C                   endif
150300131206R141 C*
150400131206 "   C                   Eval      Causal0502  = CausaleG77
150500131206 "   C                   if        Causal0502  = *Blank
150600131206 "   C                   MoveL     *Hival        Causal0502
150700131206R141 C                   endif
150800130829     C*
150900130919     C                   Eval      Stato0502   = StatoG77
151000130829     C*
151100130919     C                   Eval      PIva0502    = PartIvaG77
151200130919     C                   Eval      CdFisc0502  = CdFiscG77
151300130919     C*
151400130919     C                   Eval      TpOpe0502   = TpOpeG77
151500130919     C                   if        TpOpe0502   = *Blank
151600130919     C                   MoveL     *Hival        TpOpe0502
151700130919     C                   endif
151800130829     C*
151900130919     C                   Eval      Quadro0502  = QuadroG77
152000130919     C                   if        Quadro0502  = *Blank
152100130829     C                   MoveL     *Hival        Quadro0502
152200130829     C                   endif
152300130902     C*
152400130919     C                   Eval      CogRag0502  = CogRagG77
152500130919     C                   Eval      Nome0502    = NomeG77
152600130919     C                   Eval      DtNasc0502  = DtNascG77
152700130919     C                   Eval      LocNas0502  = LocNascG77
152800130919     C                   Eval      PrvNas0502  = PrvNascG77
152900130919     C                   Eval      CodUIC0502  = CodUICG77
153000130919     C                   Eval      Locali0502  = LocalitG77
153100130919     C                   Eval      IndirE0502  = IndirEG77
153200130829     C*
153300130903     C                   MoveL     *Hival        TpN0502
153400130906     C*
153500130903     C                   MoveL     *Hival        COGRAR0502
153600130829     C                   MoveL     *Hival        NOMER0502
153700130829     C                   MoveL     *Hival        DTNASR0502
153800130829     C                   MoveL     *Hival        LOCNAR0502
153900130829     C                   MoveL     *Hival        PRVNAR0502
154000130829     C                   MoveL     *Hival        STATOR0502
154100130829     C                   MoveL     *Hival        CODUIR0502
154200130829     C                   MoveL     *Hival        LOCALR0502
154300130829     C                   MoveL     *Hival        INDIER0502
154400130829     C*
154500130903     C                   MoveL     *Hival        TpNR0502
154600130903     C                   MoveL     *Hival        PIVAR0502
154700130829     C                   MoveL     *Hival        CDFISR0502
154800130925     C                   MoveL     *Hival        TpOpeR0502
154900130925     C                   MoveL     *Hival        NATOPR0502
155000130902     C                   MoveL     *Hival        LEASIR0502
155100130829     C                   MoveL     *Hival        DTREGR0502
155200130829     C                   MoveL     *Hival        NRREGR0502
155300130829     C                   MoveL     *Hival        SERRER0502
155400000000     C*
155500130829     C                   MOVE      $ORD          ORD0502
155600000000     C* richiedo al pgm di lettura il 1° rcd con la conseguente
155700000000     C* inizializzazione delle variabili SQL
155800130829     C                   MOVE      '0'           RIC0502
155900130829     C                   MOVE      '05'          OPZ0502
156000130829     C                   Eval      TpList0502   = '01'
156100130829     C                   MOVE      '01'          TPROTT0502
156200130829     C                   MOVE      '02'          ROTT0502
156300000000     C*
156400000000     C                   SELECT
156500000000     C*
156600000000     C* Gestione posizionamento
156700000000     C* Se il campo W1POS è > *blank imposto il posizionamento con
156800000000     C* W1POS altrimenti lo imposto con il valore di parametrizzazione
156900000000     C*
157000000000    >C* Ord.1 = chiave ordinamento 1
157100000000     C     $ORD          WHENEQ    1
157200130829    >C                   MOVEL     W1POS         POS0502
157300000000     C*
157400000000     C* Ord.99= Chiave univoca per la POSTA
157500000000     C* (quando viene gestita la chiave univoca NON deve essere gestito
157600000000     C*  il posizionamento C1POS/W1POS)
157700000000     C     $ORD          WHENEQ    99
157800000000     C*
157900000000     C                   ENDSL
158000000000     C*
158100000000     C                   ENDSR
158200000000     C/EJECT
158300000000     C************************************************************
158400000000     C* LETTURA RCD ARCHIVIO PILOTA
158500000000     C************************************************************
158600000000     C     REDANA        BEGSR
158700000000     C*
158800000000     C                   MOVEL     *OFF          $EndFile
158900000000     C                   MOVEL     *OFF          $RCDOK
159000000000     C*
159100000000     C     $EndFile      DOUEQ     *ON
159200000000     C     $RCDOK        OREQ      *ON
159300000000     C*
159400000000     C                   SELECT
159500000000     C*
159600000000     C     $ORD          WHENEQ    1
159700000000     C*
159800130829     C                   CALLB     'SDG00502R'
159900000000     C                   PARM                    KPJBA
160000130902     C                   PARM                    SDG00502DS
160100130902     C                   PARM                    SDG00592DS
160200130902     C                   PARM                    SDG005FADS
160300130902     C                   PARM                    SDG005SADS
160400130902     C                   PARM                    SDG005BLDS
160500130902     C                   PARM                    SDG005FEDS
160600130902     C                   PARM                    SDG005FRDS
160700130902     C                   PARM                    SDG005NEDS
160800130902     C                   PARM                    SDG005NRDS
160900130902     C                   PARM                    SDG005DFDS
161000130902     C                   PARM                    SDG005FNDS
161100130902     C                   PARM                    SDG005SEDS
161200131017R136 C                   PARM                    SDG005TUDS
161300130902     C                   PARM                    SDG005TADS
161400000000     C*
161500000000     C* richiedo al pgm di lettura il rcd successivo
161600130829     C                   MOVE      '1'           RIC0502
161700000000     C*
161800130829     C                   MOVE      END0502       $EndFile
161900000000     C*
162000000000     C     $ORD          WHENEQ    99
162100000000     C* nel caso di lettura dati POSTA, basta leggere in READE con la
162200000000     C* chiave precedentemente riempita nella routine SetAna
162300000000     C*
162400000000     C                   ENDSL
162500000000     C*
162600000000     C*
162700000000     C     $EndFile      IFEQ      *OFF
162800000000     C                   EXSR      SELANA
162900000000     C                   ENDIF
163000000000     C*
163100000000     C                   ENDDO
163200000000     C*
163300000000     C                   MOVE      $EndFile      *IN33                          SflEnd
163400000000     C*
163500000000     C                   ENDSR
163600000000     C/EJECT
163700000000     C************************************************************
163800000000     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
163900000000     C************************************************************
164000000000     C     CLCPAG        BEGSR
164100000000     C* Input :
164200000000     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
164300000000     C*          cursore
164400000000     C* - SFLPAG = numero rcd per pagina sfl
164500000000     C* Output :
164600000000     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
164700000000     C*
164800000000     C     WSFL          DIV       SFLPAG        PAGINE            4 0
164900000000     C                   MVR                     RESTO             3 0
165000000000     C     RESTO         IFGT      0
165100000000     C                   ADD       1             PAGINE
165200000000     C                   ENDIF
165300000000     C     PAGINE        MULT      SFLPAG        WPAG
165400000000     C*
165500000000     C                   ENDSR
165600000000     C/EJECT
165700000000     C************************************************************
165800000000     C* SELEZIONI SULLA RIGA LETTA
165900000000     C* impostare a *OFF il campo $RCDOK per scartare un record che
166000000000     C* non rientra nei parametri di parzializzazione
166100000000     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
166200000000     C* superiore stabilito nei parametri
166300000000     C*
166400000000     C************************************************************
166500000000     C     SELANA        BEGSR
166600000000     C*
166700000000     C                   MOVEL     *ON           $RCDOK
166800000000     C                   MOVE      *hival        ALL9             15
166900000000     C*
167000000000     C* creo limite massimo parzializzazioni :
167100000000     C* per ogni valore alfanumerico creo il limite superiore
167200000000     C* per cui caricare, creando una variabile che contiene
167300000000     C* il valore digitato nel campo a video riempito di "9"
167400000000     C*
167500000000     C                   SELECT
167600000000     C*
167700000000     C* SELEZIONI RISPETTO AI VALORI RCD
167800000000     C*
167900000000     C*
168000000000     C*
168100000000     C* SELEZIONI RISPETTO ALLE PARZIALIZZAZIONI
168200000000     C*
168300000000     C                   ENDSL
168400000000     C*
168500000000     C*
168600000000     C                   IF        $RcdOk = *off
168700000000     C*
168800000000     C                   ENDIF
168900000000     C*
169000000000     C                   ENDSR
169100000000     C/EJECT
169200130903     C************************************************************
169300130903     C* Riempio la riga del sfl
169400130903     C************************************************************
169500130903     C     RieS1         BEGSR
169600130903     C*
169700130903     C                   CLEAR                   FMTS1
169800130925     C                   CLEAR                   FMTS2
169801160211R195 C*
169802160211 "   C                   Select
169803160211 "   C* Spesometro
169804160211 "   C                   When      TpComG77    = 'E'
169805160211 "   C                   Eval      *In12       = *On
169806160211 "   C*
169807160211 "   C* Acquisti da San Marino
169808160211 "   C                   When      TpComG77    = 'S'
169809160211 "   C                   Eval      *In11       = *On
169810160211 "   C*
169811160211 "   C* Black List
169812160211 "   C                   When      TpComG77    = 'B'
169813160211 "   C                   Eval      *In13       = *On
169814160211 "   C*
169815160211 "   C                   Other
169816160211 "   C                   Eval      *In11       = *Off
169817160211R195 C                   endSl
169900130903     C*
170000130903     C* riempio i campi del sfl, compresi gli hidden con i valori
170100130903     C* del rcd letto
170200130903     C                   Eval      H1TpNR      = TpNR0592
170300130911     C                   Eval      H1PIVAR     = PIVAR0592
170400130911     C                   Eval      H1CdFiscR   = CdFisR0592
170500130903     C                   Eval      H1StatoR    = StatoR0592
170600130903     C                   Eval      H1CogRagR   = CogRaR0592
170700130903     C                   Eval      H1NomeR     = NomeR0592
170800130903     C                   Eval      H1DtNasR    = DtNasR0592
170900130903     C                   Eval      H1LocNasR   = LocNaR0592
171000130903     C                   Eval      H1PrvNasR   = PrvNaR0592
171100130903     C                   Eval      H1CodUICR   = CodUIR0592
171200130903     C                   Eval      H1LocaliR   = LocalR0592
171300130911     C                   Eval      H1IndireR   = IndiER0592
171400130925     C                   Eval      H1TpOpeR    = TpOpeR0592
171500130925     C                   Eval      H1NatOpeR   = NatOpR0592
171600130911     C**************     Eval      H1LeasingA  = LeasiR0592
171601160201R195 C                   Eval      H1LeasinR   = LeasiR0592
171700130903     C                   Eval      H1DtRegR    = DtRegR0592
171800130903     C                   Eval      H1NrRegR    = NrRegR0592
171900130903     C                   Eval      H1SerRegR   = SerReR0592
172000130903     C*
172100130903     C                   Select
172200130903     C                   When      Quadro0592  = 'FA'
172300130903     C                   EXSR      RIES1_FA
172400130903     C*
172500130903     C                   ADD       1             S1NRR
172600130903     C                   ADD       1             Y
172700130903     C                   WRITE     FMTS1
172800130903     C*
172900130903     C                   When      Quadro0592  = 'SA'
173000130925     C                   EXSR      RIES1_SA
173100130903     C*
173200130903     C                   ADD       1             S1NRR
173300130903     C                   ADD       1             Y
173400130925     C                   WRITE     FMTS1
173500130904     C*
173600130904     C                   When      Quadro0592  = 'BL'
173700130925     C                   EXSR      RIES1_BL
173800130904     C*
173900130904     C                   ADD       1             S1NRR
174000130904     C                   ADD       1             Y
174100130925     C                   WRITE     FMTS1
174200130904     C*
174300130904     C                   When      Quadro0592  = 'FE'
174400130925     C                   EXSR      RIES2_FE
174500130904     C*
174600130904     C                   ADD       1             S1NRR
174700130904     C                   ADD       1             Y
174800130925     C                   WRITE     FMTS2
174900130904     C*
175000130904     C                   When      Quadro0592  = 'FR'
175100130925     C                   EXSR      RIES2_FR
175200130904     C*
175300130904     C                   ADD       1             S1NRR
175400130904     C                   ADD       1             Y
175500130925     C                   WRITE     FMTS2
175600130904     C*
175700130904     C                   When      Quadro0592  = 'NE'
175800130925     C                   EXSR      RIES2_NE
175900130904     C*
176000130904     C                   ADD       1             S1NRR
176100130904     C                   ADD       1             Y
176200130925     C                   WRITE     FMTS2
176300130904     C*
176400130904     C                   When      Quadro0592  = 'NR'
176500130925     C                   EXSR      RIES2_NR
176600130904     C*
176700130904     C                   ADD       1             S1NRR
176800130904     C                   ADD       1             Y
176900130925     C                   WRITE     FMTS2
177000130904     C*
177100130904     C                   When      Quadro0592  = 'DF'
177200130925     C                   EXSR      RIES2_DF
177300130904     C*
177400130904     C                   ADD       1             S1NRR
177500130904     C                   ADD       1             Y
177600130925     C                   WRITE     FMTS2
177700130906     C*
177800130906     C                   When      Quadro0592  = 'FN'
177900130925     C                   EXSR      RIES2_FN
178000130906     C*
178100130906     C                   ADD       1             S1NRR
178200130906     C                   ADD       1             Y
178300130925     C                   WRITE     FMTS2
178400130906     C*
178500130906     C                   When      Quadro0592  = 'SE'
178600130925     C                   EXSR      RIES2_SE
178700130906     C*
178800130906     C                   ADD       1             S1NRR
178900130906     C                   ADD       1             Y
179000130925     C                   WRITE     FMTS2
179100131018R136 C*
179200131018 "   C                   When      Quadro0592  = 'TU'
179300131018 "   C                   EXSR      RIES2_TU
179400131018 "   C*
179500131018 "   C                   ADD       1             S1NRR
179600131018 "   C                   ADD       1             Y
179700131018R136 C                   WRITE     FMTS2
179800130903     C*
179900130903     C                   endsL
180000130903     C*
180100130903     C                   ENDSR
180200130903     C/EJECT
180300000000     C************************************************************
180400000000     C* RIEMPIMENTO RIGA LISTA
180500000000     C************************************************************
180600130903     C     RIES1_FA      BEGSR
180700000000     C*
180800130829     C                   Eval      S1Quadro    = Quadro05FA
180900130829     C                   Eval      S1NROPEA    = NROPEA05FA
181000130829     C                   Eval      S1NROPEP    = NROPEP05FA
181100130829     C                   Eval      S1IMPATT    = IMPATT05FA
181200130829     C                   Eval      S1IVAATT    = IVAATT05FA
181300130829     C                   Eval      S1MARGIA    = MARGIA05FA
181400130829     C                   Eval      S1IMPNDE    = IMPNDE05FA
181500130829     C                   Eval      S1IVANDE    = IVANDE05FA
181600130829     C                   Eval      S1IMPPAS    = IMPPAS05FA
181700130829     C                   Eval      S1IVAPAS    = IVAPAS05FA
181800130829     C                   Eval      S1MARGIP    = MARGIP05FA
181900130829     C                   Eval      S1IMPNCR    = IMPNCR05FA
182000130829     C                   Eval      S1IVANCR    = IVANCR05FA
182100000000     C*
182200000000     C* azzero le memorizzazioni degli attributi non bottoni
182300130924     C                   MOVEL     Leasin05FA    S1Leasing
182400130924     C                   MOVEL     *Blank        H1LeasingA
182500130904     C                   if        Leasin05FA  = '1'
182600130903     C                   MoveL     *On           H1LeasingA
182700000000     C                   endif
182800130923     C                   if        Leasin05FA  = '0'
182900130923     C                   MoveL     *Off          H1LeasingA
183000130923     C                   endif
183100131015R135 C*
183200131015 "   C                   MOVEL     *Off          H1DocRieA
183300131015 "   C                   if        DocRie05FA  = '1'
183400131015 "   C                   MoveL     *On           H1DocRieA
183500131015 "   C                   endif
183600131015 "   C                   if        DocRie05FA  = '0'
183700131015 "   C                   MoveL     *Off          H1DocRieA
183800131015R135 C                   endif
183900000000     C*
184000000000     C* imposto quegli attributi del sfl che sono modificabili
184100000000     C* al richiamo del determinato pgm
184200000000     C                   EXSR      ATRS1
184300000000     C*
184400000000     C                   ENDSR
184500000000     C/EJECT
184600000000     C************************************************************
184700000000     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
184800000000     C************************************************************
184900000000     C     ATRS1         BEGSR
185000000000     C*
185100000000     C* in questa routine bisogna attivare indicatori o campi programma
185200000000     C* che gestiscono gli attributi di uno o più campi di questa videata;
185300000000     C* attenzione : non è questo il posto in cui eseguire controlli,
185400000000     C*  dovrebbero già arrivare dei flag
185500000000     C*
185600000000     C* azzero gli attributi non bottoni
185700130829     C                   MOVEL     X'A0'         S1LeasingA
185800000000     C*
185900130829     C     H1LeasingA    IFEQ      *ON
186000130829     C                   MOVEL     ANNUL         S1LeasingA
186100000000     C                   ENDIF
186200130925     C*
186300130925     C                   MOVEL     X'A0'         S1OpeNReA
186400130925     C*
186500130925     C     H1OpeNReA     IFEQ      *ON
186600130925     C                   MOVEL     ANNUL         S1OpeNReA
186700130925     C                   ENDIF
186800130925     C*
186900130925     C                   MOVEL     X'A0'         S1AcqSrvA
187000130925     C*
187100130925     C     H1AcqSrvA     IFEQ      *ON
187200130925     C                   MOVEL     ANNUL         S1AcqSrvA
187300130925     C                   ENDIF
187400131015R135 C*
187500131015 "   C                   MOVEL     X'A0'         S1DocRieA
187600131015 "   C*
187700131015 "   C     H1DocRieA     IFEQ      *ON
187800131015 "   C                   MOVEL     ANNUL         S1DocRieA
187900131015R135 C                   ENDIF
188000000000     C*
188100000000     C                   ENDSR
188200000000     C/EJECT
188300130903     C************************************************************
188400130903     C* RIEMPIMENTO RIGA LISTA
188500130903     C************************************************************
188600130925     C     RIES1_SA      BEGSR
188700130903     C*
188800130903     C                   Eval      S1Quadro    = Quadro05SA
188900130925     C                   Eval      S1NrOpeA    = NrOpe05SA
189000130925     C                   Eval      S1ImpAtt    = Import05SA
189100130903     C*
189200130903     C* azzero le memorizzazioni degli attributi non bottoni
189300130925     C                   MOVEL     Leasin05SA    S1Leasing
189400130925     C                   MOVEL     *Blank        H1LeasingA
189500130924     C                   if        Leasin05SA  = '1'
189600130925     C                   MoveL     *On           H1LeasingA
189700130924     C                   endif
189800130924     C                   if        Leasin05SA  = '0'
189900130925     C                   MoveL     *Off          H1LeasingA
190000130924     C                   endif
190100130903     C*
190200130903     C* imposto quegli attributi del sfl che sono modificabili
190300130903     C* al richiamo del determinato pgm
190400130925     C                   EXSR      ATRS1
190500130903     C*
190600130903     C                   ENDSR
190700130903     C/EJECT
190800130904     C************************************************************
190900130904     C* RIEMPIMENTO RIGA LISTA
191000130904     C************************************************************
191100130925     C     RIES1_BL      BEGSR
191200130904     C*
191300130904     C                   Eval      S1Quadro    = Quadro05BL
191400130925     C                   Eval      S1ImpAtt    = ImpAtt05BL
191500130925     C                   Eval      S1IVAAtt    = IVAAtt05BL
191600130925     C                   Eval      S1ImpPas    = ImpPas05BL
191700130925     C                   Eval      S1IVAPas    = IVAPas05BL
191701160115R195 C                   Eval      S1MargiA    = NoIBAt05BL + NoISAt05BL
191702160115 "   C                   Eval      S1MargiP    = NoIVAP05BL
191703160115 "   C                   Eval      S1ImpNDe    = ImpNAt05BL
191704160105 "   C                   Eval      S1IVANDe    = IVANAt05BL
191705160105 "   C                   Eval      S1ImpNCr    = ImpNPa05BL
191706160105R195 C                   Eval      S1IVANCr    = IVANPa05BL
191800130904     C*
191900130904     C* azzero le memorizzazioni degli attributi non bottoni
192000130925     C                   MOVEL     *Off          H1OpeNReA
192100130904     C                   if        OpeNRe05BL  = '1'
192200130925     C                   MoveL     *On           H1OpeNReA
192300130904     C                   endif
192400130904     C                   if        OpeNRe05BL  = '0'
192500130925     C                   MoveL     *Off          H1OpeNReA
192600130904     C                   endif
192700130904     C*
192800130925     C                   MOVEL     *Off          H1AcqSrvA
192900130904     C                   if        AcqSrv05BL  = '1'
193000130925     C                   MoveL     *On           H1AcqSrvA
193100130904     C                   endif
193200130904     C                   if        AcqSrv05BL  = '0'
193300130925     C                   MoveL     *Off          H1AcqSrvA
193400130904     C                   endif
193500130904     C*
193600130904     C* imposto quegli attributi del sfl che sono modificabili
193700130904     C* al richiamo del determinato pgm
193800130925     C                   EXSR      ATRS1
193900130904     C*
194000130904     C                   ENDSR
194100130904     C/EJECT
194200130904     C************************************************************
194300130904     C* RIEMPIMENTO RIGA LISTA
194400130904     C************************************************************
194500130925     C     RIES2_FE      BEGSR
194600130904     C*
194700130904     C                   Eval      S1Quadro    = Quadro05FE
194800130904     C                   if        DtDoc05FE  <> *Loval
194900130925     C     *Jobrun       Move      DtDoc05FE     S2DtDoc
195000130904     C                   endif
195100130904     C                   if        DtReg05FE  <> *Loval
195200130925     C     *Jobrun       Move      DtReg05FE     S2DtReg
195300130904     C                   endif
195400130925     C                   Eval      S2NrDoc     = NrDoc05FE
195500130925     C                   Eval      S2Import    = Import05FE
195600130925     C                   Eval      S2IVA       = IVA05FE
195700130904     C*
195800130904     C* azzero le memorizzazioni degli attributi non bottoni
195900130925     C                   MOVEL     *Off          H2DocRieA
196000130904     C                   if        DocRie05FE  = '1'
196100130925     C                   MoveL     *On           H2DocRieA
196200130904     C                   endif
196300130904     C                   if        DocRie05FE  = '0'
196400130925     C                   MoveL     *Off          H2DocRieA
196500130904     C                   endif
196600130904     C*
196700130925     C                   MOVEL     *Off          H2MarginA
196800130904     C                   if        Margin05FE  = '1'
196900130925     C                   MoveL     *On           H2MarginA
197000130904     C                   endif
197100130904     C                   if        Margin05FE  = '0'
197200130925     C                   MoveL     *Off          H2MarginA
197300130904     C                   endif
197400130904     C*
197500130925     C                   MOVEL     Leasin05FE    S2Leasing
197600130925     C                   MOVEL     *Blank        H2LeasinA
197700130904     C                   if        Leasin05FE  = '1'
197800130925     C                   MoveL     *On           H2LeasinA
197900130904     C                   endif
198000130904     C                   if        Leasin05FE  = '0'
198100130925     C                   MoveL     *Off          H2LeasinA
198200130904     C                   endif
198300130904     C*
198400130925     C                   MOVEL     *Off          H2AutofaA
198500130904     C                   if        Autofa05FE  = '1'
198600130925     C                   MoveL     *On           H2AutofaA
198700130904     C                   endif
198800130904     C                   if        Autofa05FE  = '0'
198900130925     C                   MoveL     *Off          H2AutofaA
199000130904     C                   endif
199100130904     C*
199200130904     C* imposto quegli attributi del sfl che sono modificabili
199300130904     C* al richiamo del determinato pgm
199400130925     C                   EXSR      ATRS2
199500130904     C*
199600130904     C                   ENDSR
199700130904     C/EJECT
199800130904     C************************************************************
199900130904     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
200000130904     C************************************************************
200100130925     C     ATRS2         BEGSR
200200130904     C*
200300130904     C* in questa routine bisogna attivare indicatori o campi programma
200400130904     C* che gestiscono gli attributi di uno o più campi di questa videata;
200500130904     C* attenzione : non è questo il posto in cui eseguire controlli,
200600130904     C*  dovrebbero già arrivare dei flag
200700130904     C*
200800130925     C                   MOVEL     X'A0'         S2DocRieA
200900130904     C*
201000130925     C     H2DocRieA     IFEQ      *ON
201100130925     C                   MOVEL     ANNUL         S2DocRieA
201200130904     C                   ENDIF
201300130904     C*
201400130925     C                   MOVEL     X'A0'         S2MarginA
201500130904     C*
201600130925     C     H2MarginA     IFEQ      *ON
201700130925     C                   MOVEL     ANNUL         S2MarginA
201800130904     C                   ENDIF
201900130904     C*
202000130925     C                   MOVEL     X'A0'         S2LeasinA
202100130904     C*
202200130925     C     H2LeasinA     IFEQ      *ON
202300130925     C                   MOVEL     ANNUL         S2LeasinA
202400130904     C                   ENDIF
202500130904     C*
202600130925     C                   MOVEL     X'A0'         S2AutofaA
202700130904     C*
202800130925     C     H2AutofaA     IFEQ      *ON
202900130925     C                   MOVEL     ANNUL         S2AutofaA
203000130904     C                   ENDIF
203100130904     C*
203200130925     C                   MOVEL     X'A0'         S2ReversA
203300130904     C*
203400130925     C     H2ReversA     IFEQ      *ON
203500130925     C                   MOVEL     ANNUL         S2ReversA
203600130904     C                   ENDIF
203700130904     C*
203800130904     C                   ENDSR
203900130904     C/EJECT
204000130904     C************************************************************
204100130904     C* RIEMPIMENTO RIGA LISTA
204200130904     C************************************************************
204300130925     C     RIES2_FR      BEGSR
204400130904     C*
204500130904     C                   Eval      S1Quadro    = Quadro05FR
204600130904     C                   if        DtDoc05FR  <> *Loval
204700130925     C     *Jobrun       Move      DtDoc05FR     S2DtDoc
204800130904     C                   endif
204900130904     C                   if        DtReg05FR  <> *Loval
205000130925     C     *Jobrun       Move      DtReg05FR     S2DtReg
205100130904     C                   endif
205200130925     C                   Eval      S2Import    = Import05FR
205300130925     C                   Eval      S2IVA       = IVA05FR
205400130904     C*
205500130904     C* azzero le memorizzazioni degli attributi non bottoni
205600130925     C                   MOVEL     *Off          H2DocRieA
205700130904     C                   if        DocRie05FR  = '1'
205800130925     C                   MoveL     *On           H2DocRieA
205900130904     C                   endif
206000130904     C                   if        DocRie05FR  = '0'
206100130925     C                   MoveL     *Off          H2DocRieA
206200130904     C                   endif
206300130904     C*
206400130925     C                   MOVEL     *Off          H2MarginA
206500130904     C                   if        Margin05FR  = '1'
206600130925     C                   MoveL     *On           H2MarginA
206700130904     C                   endif
206800130904     C                   if        Margin05FR  = '0'
206900130925     C                   MoveL     *Off          H2MarginA
207000130904     C                   endif
207100130904     C*
207200130925     C                   MOVEL     *Off          H2ReversA
207300130904     C                   if        Revers05FR  = '1'
207400130925     C                   MoveL     *On           H2ReversA
207500130904     C                   endif
207600130904     C                   if        Revers05FR  = '0'
207700130925     C                   MoveL     *Off          H2ReversA
207800130904     C                   endif
207900130904     C*
208000130925     C                   MOVEL     *Off          H2AutofaA
208100130904     C                   if        Autofa05FR  = '1'
208200130925     C                   MoveL     *On           H2AutofaA
208300130904     C                   endif
208400130904     C                   if        Autofa05FR  = '0'
208500130925     C                   MoveL     *Off          H2AutofaA
208600130904     C                   endif
208700130904     C*
208800130904     C* imposto quegli attributi del sfl che sono modificabili
208900130904     C* al richiamo del determinato pgm
209000130925     C                   EXSR      ATRS2
209100130904     C*
209200130904     C                   ENDSR
209300130904     C/EJECT
209400130904     C************************************************************
209500130904     C* RIEMPIMENTO RIGA LISTA
209600130904     C************************************************************
209700130925     C     RIES2_NE      BEGSR
209800130904     C*
209900130904     C                   Eval      S1Quadro    = Quadro05NE
210000130904     C                   if        DtDoc05NE  <> *Loval
210100130925     C     *Jobrun       Move      DtDoc05NE     S2DtDoc
210200130904     C                   endif
210300130904     C                   if        DtReg05NE  <> *Loval
210400130925     C     *Jobrun       Move      DtReg05NE     S2DtReg
210500130904     C                   endif
210600130925     C                   Eval      S2NrDoc     = NrDoc05NE
210700130925     C                   Eval      S2Import    = Import05NE
210800130925     C                   Eval      S2IVA       = IVA05NE
210900130904     C*
211000130904     C* imposto quegli attributi del sfl che sono modificabili
211100130904     C* al richiamo del determinato pgm
211200130925     C                   EXSR      ATRS2
211300130904     C*
211400130904     C                   ENDSR
211500130904     C/EJECT
211600130904     C************************************************************
211700130904     C* RIEMPIMENTO RIGA LISTA
211800130904     C************************************************************
211900130925     C     RIES2_NR      BEGSR
212000130904     C*
212100130904     C                   Eval      S1Quadro    = Quadro05NR
212200130904     C                   if        DtDoc05NR  <> *Loval
212300130925     C     *Jobrun       Move      DtDoc05NR     S2DtDoc
212400130904     C                   endif
212500130904     C                   if        DtReg05NR  <> *Loval
212600130925     C     *Jobrun       Move      DtReg05NR     S2DtReg
212700130904     C                   endif
212800130925     C                   Eval      S2Import    = Import05NR
212900130925     C                   Eval      S2IVA       = IVA05NR
213000130904     C*
213100130904     C* imposto quegli attributi del sfl che sono modificabili
213200130904     C* al richiamo del determinato pgm
213300130925     C                   EXSR      ATRS2
213400130904     C*
213500130904     C                   ENDSR
213600130904     C/EJECT
213700130904     C************************************************************
213800130904     C* RIEMPIMENTO RIGA LISTA
213900130904     C************************************************************
214000130925     C     RIES2_DF      BEGSR
214100130904     C*
214200130904     C                   Eval      S1Quadro    = Quadro05DF
214300130904     C                   if        DtReg05DF  <> *Loval
214400130925     C     *Jobrun       Move      DtReg05DF     S2DtReg
214500130904     C                   endif
214600130925     C                   Eval      S2Import    = Import05DF
214700130904     C*
214800130904     C* azzero le memorizzazioni degli attributi non bottoni
214900130925     C                   MOVEL     Leasin05DF    S2Leasing
215000130925     C                   MOVEL     *Blank        H2LeasinA
215100130904     C                   if        Leasin05DF  = '1'
215200130925     C                   MoveL     *On           H2LeasinA
215300130904     C                   endif
215400130904     C                   if        Leasin05DF  = '0'
215500130925     C                   MoveL     *Off          H2LeasinA
215600130904     C                   endif
215700130924     C*
215800130904     C*
215900130904     C* imposto quegli attributi del sfl che sono modificabili
216000130904     C* al richiamo del determinato pgm
216100130925     C                   EXSR      ATRS2
216200130904     C*
216300130904     C                   ENDSR
216400130904     C/EJECT
216500130906     C************************************************************
216600130906     C* RIEMPIMENTO RIGA LISTA
216700130906     C************************************************************
216800130925     C     RIES2_FN      BEGSR
216900130906     C*
217000130906     C                   Eval      S1Quadro    = Quadro05FN
217100130906     C                   if        DtDoc05FN  <> *Loval
217200130925     C     *Jobrun       Move      DtDoc05FN     S2DtDoc
217300130906     C                   endif
217400130906     C                   if        DtReg05FN  <> *Loval
217500130925     C     *Jobrun       Move      DtReg05FN     S2DtReg
217600130906     C                   endif
217700130925     C                   Eval      S2NrDoc     = NrDoc05FN
217800130925     C                   Eval      S2Import    = Import05FN
217900130925     C                   Eval      S2IVA       = IVA05FN
218000130906     C*
218100130906     C* azzero le memorizzazioni degli attributi non bottoni
218200130925     C                   MOVEL     Leasin05FN    S2Leasing
218300130925     C                   MOVEL     *Blank        H2LeasinA
218400130906     C                   if        Leasin05FN  = '1'
218500130925     C                   MoveL     *On           H2LeasinA
218600130906     C                   endif
218700130906     C                   if        Leasin05FN  = '0'
218800130925     C                   MoveL     *Off          H2LeasinA
218900130906     C                   endif
219000130906     C*
219100130906     C* imposto quegli attributi del sfl che sono modificabili
219200130906     C* al richiamo del determinato pgm
219300130925     C                   EXSR      ATRS2
219400130906     C*
219500130906     C                   ENDSR
219600130906     C/EJECT
219700130906     C************************************************************
219800130906     C* RIEMPIMENTO RIGA LISTA
219900130906     C************************************************************
220000130925     C     RIES2_SE      BEGSR
220100130906     C*
220200130906     C                   Eval      S1Quadro    = Quadro05SE
220300130906     C                   if        DtDoc05SE  <> *Loval
220400130925     C     *Jobrun       Move      DtDoc05SE     S2DtDoc
220500130906     C                   endif
220600130906     C                   if        DtReg05SE  <> *Loval
220700130925     C     *Jobrun       Move      DtReg05SE     S2DtReg
220800130906     C                   endif
220900130925     C                   Eval      S2NrDoc     = NrDoc05SE
221000130925     C                   Eval      S2Import    = Import05SE
221100130925     C                   Eval      S2IVA       = IVA05SE
221200130906     C*
221300130906     C*
221400130906     C* imposto quegli attributi del sfl che sono modificabili
221500130906     C* al richiamo del determinato pgm
221600130925     C                   EXSR      ATRS2
221700130906     C*
221800130906     C                   ENDSR
221900130906     C/EJECT
222000131017R136 C************************************************************
222100131017 "   C* RIEMPIMENTO RIGA LISTA
222200131017 "   C************************************************************
222300131017R136 C     RIES2_TU      BEGSR
222400131017     C*
222500131017     C                   Eval      S1Quadro    = Quadro05TU
222600131017     C                   if        DtDoc05TU  <> *Loval
222700131017     C     *Jobrun       Move      DtDoc05TU     S2DtDoc
222800131017     C                   endif
222900131017     C                   if        DtReg05TU  <> *Loval
223000131017     C     *Jobrun       Move      DtReg05TU     S2DtReg
223100131017     C                   endif
223200131017     C                   Eval      S2NrDoc     = NrDoc05TU
223300131017     C                   Eval      S2Import    = Import05TU
223400131017     C                   Eval      S2IVA       = IVA05TU
223500131017     C*
223600131017     C*
223700131017     C* imposto quegli attributi del sfl che sono modificabili
223800131017     C* al richiamo del determinato pgm
223900131017     C                   EXSR      ATRS2
224000131017     C*
224100131017R136 C                   ENDSR
224200131017R136 C/EJECT
224300000000     C************************************************************
224400000000     C* GESTIONE F08 VIDEO S1
224500000000     C************************************************************
224600000000     C     F08S1         BEGSR
224700000000     C*
224800000000     C                   MOVE      *ON           $FINE
224900130829    >C                   MOVE      '0'           RETG77
225000000000     C*
225100000000     C                   ENDSR
225200000000     C/EJECT
225300000000     C************************************************************
225400000000     C* GESTIONE F03 VIDEO S1
225500000000     C************************************************************
225600000000     C     F03S1         BEGSR
225700000000     C*
225800000000     C                   MOVE      *ON           $FINE
225900130829    >C                   MOVE      '1'           RETG77
226000000000     C* fine programma
226100000000     C                   EXSR      ENDPGM
226200000000     C*
226300000000     C                   ENDSR
226400000000     C/EJECT
226500130926     C************************************************************
226600130926     C* GESTIONE F05 VIDEO S1
226700130926     C************************************************************
226800130926     C     F05S1         BEGSR
226900130926     C*
227000130926     C                   MOVE      *ON           $INZS1
227100130926     C*
227200130926     C                   ENDSR
227300130926     C/EJECT
227400000000     C************************************************************
227500000000     C* GESTIONE F10 VIDEO S1
227600000000     C* AGGIUNTA RECORD
227700000000     C************************************************************
227800000000     C     F10S1         BEGSR
227900000000     C*
228000000000     C*
228100000000     C                   ENDSR
228200000000     C/EJECT
228300000000     C************************************************************
228400000000     C* GESTIONE F12 VIDEO S1
228500000000     C************************************************************
228600000000     C     F12S1         BEGSR
228700000000     C*
228800000000     C*  se la 1a videata sono le parzializzazioni, per F12 torno a queste
228900130904     C     $LOP          IFEQ      1
229000000000     C                   MOVE      'D1'          $GEST
229100000000     C*  se la 1a videata è questa, per F12 torno a menu
229200000000     C                   ELSE
229300000000     C                   MOVE      *ON           $FINE
229400130829    >C                   MOVE      '2'           RETG77
229500000000     C                   ENDIF
229600000000     C*
229700000000     C                   ENDSR
229800000000     C/EJECT
229900000000     C************************************************************
230000000000     C* GESTIONE F17 VIDEO S1
230100000000     C************************************************************
230200000000     C     F17S1         BEGSR
230300000000     C*
230400000000     C                   MOVE      'D1'          $GEST
230500000000     C* se il pgm è stato richiamato con chiave univoca
230600000000     C* e dopo è stata richiesta una parzializzazione
230700000000     C* tolgo la gestione della chiave univoca (azzere/abblenco il
230800000000     C* valore; propongo come ordinamento il 1°)
230900000000     C     $Unique       IFEQ      *ON
231000000000     C                   MOVE      *OFF          $Unique
231100000000     C                   ENDIF
231200000000     C     $ORD          IFEQ      99
231300000000     C                   Z-ADD     1             $ORD
231400000000     C                   MOVE      *ON           $INZD1
231500000000     C                   ENDIF
231600000000     C*
231700000000     C                   ENDSR
231800000000     C/EJECT
231900000000     C************************************************************
232000000000     C* GESTIONE F18 VIDEO S1
232100000000     C************************************************************
232200000000     C     F18S1         BEGSR
232300000000     C*
232400000000     C                   MOVE      'D2'          $GEST
232500000000     C                   MOVEL     *ON           $INZD2
232600000000     C     $ORD          IFEQ      99
232700000000     C                   Z-ADD     1             $ORD
232800000000     C                   ENDIF
232900000000     C*
233000000000     C                   ENDSR
233100000000     C/EJECT
233200000000     C************************************************************
233300000000     C* GESTIONE F23 VIDEO S1
233400000000     C************************************************************
233500000000     C     F23S1         BEGSR
233600000000     C*
233700000000     C                   Z-ADD     $C1Op1        $L01
233800000000     C                   Z-ADD     $C1Op2        $L02
233900000000     C                   EXSR      RIEOPZ
234000000000     C                   Z-ADD     23            X
234100000000     C                   MOVE      FF23          $FC2(X)
234200000000     C                   MOVEL     WOPZ1         C1OPZ1
234300000000     C                   MOVEL     $ALLOPT       H1ALLOPT
234400000000     C                   MOVEL     WOPZ2         C1OPZ2
234500000000     C                   Z-ADD     WCOZ          C1COZ
234600000000     C*
234700000000     C                   ENDSR
234800000000     C/EJECT
234900000000     C************************************************************
235000000000     C* GESTIONE F24 VIDEO S1
235100000000     C************************************************************
235200000000     C     F24S1         BEGSR
235300000000     C*
235400000000     C                   MOVEA     $FX2          $FX
235500000000     C                   MOVEA     $FC2          $FC
235600000000     C                   MOVEA     $FR2          $FR
235700000000     C                   Z-ADD     $ULKS1        $ULK
235800000000     C                   Z-ADD     $S1L01        $L01
235900000000     C                   Z-ADD     $S1L02        $L02
236000000000     C                   EXSR      RIEKEY
236100000000     C                   Z-ADD     $ULK          $ULKS1            3 0
236200000000     C                   MOVEA     $FC           $FC2
236300000000     C                   MOVEL     $KEY1         Z2KE1
236400000000     C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
236500000000     C                   MOVEL     $KEY2         Z2KE2
236600000000     C                   WRITE     FMTZ2
236700000000     C*
236800000000     C                   ENDSR
236900000000     C/EJECT
237000000000     C************************************************************
237100000000     C* CONTROLLO TESTATA LISTA
237200000000     C************************************************************
237300000000     C     CTRC1         BEGSR
237400000000     C*
237500000000     C                   MOVE      *OFF          *IN99
237600000000     C*
237700000000     C     C1POS         IFNE      W1POS
237800000000     C                   EXSR      POSS1
237900000000     C                   ENDIF
238000000000     C*
238100000000     C                   ENDSR
238200000000     C/EJECT
238300000000     C************************************************************
238400000000     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
238500000000     C************************************************************
238600000000     C     POSS1         BEGSR
238700000000     C*
238800000000     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
238900000000     C*
239000000000   B1C     WMAX          IFGT      0
239100130903     C                   EXSR      ReadCS1
239200000000   B2C     *IN21         DOWEQ     *OFF
239300000000     C     *IN99         ANDEQ     *OFF
239400000000     C*
239500130903   B3C     S1OPZ         IFNE      0
239600000000     C                   SETON                                        92  99
239700000000   E3C                   ENDIF
239800000000     C* se rilevata opzione attiva, attivo sflnxtchg
239900000000   B3C     *IN99         IFEQ      *ON
240000000000     C                   MOVE      *ON           *IN32
240100130903     C                   EXSR      UpdateS1
240200000000   X3C                   ELSE
240300130903     C                   EXSR      ReadCS1
240400000000   E3C                   ENDIF
240500000000     C*
240600000000   E2C                   ENDDO
240700000000     C*
240800000000   E1C                   ENDIF
240900000000     C*
241000000000     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
241100000000     C* AL VALORE DI POSIZIONAMENTO :
241200000000     C* se questo è maggiore del valore del rispettivo campo di
241300000000     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
241400000000     C* con i valori di parzializzazione.
241500000000     C*
241600000000   B1C     *IN99         IFEQ      *OFF
241700000000    >C                   MOVE      *BLANK        W1POS
241800000000     C*
241900000000     C* per ogni ordinamento devo testare se il campo dell'inizio elenco
242000000000     C* è maggiore del relativo valore di parzializzazione, nel qual caso
242100000000     C* imposto la variabile del posizionamento (W1Pos) al nuovo valore;
242200000000     C* richiedo comunque un ricaricamento del sfl che, se non si è
242300000000     C* verificata la condizione suddetta avverrà con posizionamento a
242400000000     C* partire dal valore di parzializzazione
242500000000     C*
242600000000   B2C                   SELECT
242700000000    >C* Attenzione : se un ordinamento ha un campo di posizionamento
242800000000    >C* specifico bisogna sostituire la gestione W1Pos/C1Pos con quella
242900000000    >C* dei relativi campi
243000000000     C*
243100000000     C     $ORD          WHENEQ    1
243200000000     C*
243300000000     C* per l'ordinamento 99 = POSTA non viene gestito il posizionamento
243400000000     C*
243500000000   E2C                   ENDSL
243600000000     C*
243700000000     C* richiedo l'inizializzazione sfl
243800000000     C                   MOVE      *ON           $INZS1
243900000000     C*
244000000000   E1C                   ENDIF
244100000000     C*
244200000000     C                   ENDSR
244300000000     C/EJECT
244400000000     C************************************************************
244500130903     C* Aggiornamento riga sfl
244600000000     C************************************************************
244700130903     C     UpdateS1      BEGSR
244800000000     C*
244900130903     C                   Select
245000130925     C                   When      Quadro0592  = 'FA'     or
245100130925     C                             Quadro0592  = 'SA'     or
245200130925     C                             Quadro0592  = 'BL'
245300130903     C* imposto quegli attributi del sfl che sono modificabili
245400130903     C* al richiamo del determinato pgm
245500130903     C                   EXSR      ATRS1
245600130903     C                   UPDATE    FMTS1
245700130904     C*
245800130904     C                   When      Quadro0592  = 'FE'     or
245900130904     C                             Quadro0592  = 'FR'     or
246000130904     C                             Quadro0592  = 'NE'     or
246100130906     C                             Quadro0592  = 'NR'     or
246200130906     C                             Quadro0592  = 'DF'     or
246300130906     C                             Quadro0592  = 'FN'     or
246400131017R136 C                             Quadro0592  = 'TU'     or
246500130906     C                             Quadro0592  = 'SE'
246600130904     C* imposto quegli attributi del sfl che sono modificabili
246700130904     C* al richiamo del determinato pgm
246800130925     C                   EXSR      ATRS2
246900130925     C                   UPDATE    FMTS2
247000130904     C*
247100130903     C                   endSl
247200130903     C*
247300130903     C                   ENDSR
247400130903     C/EJECT
247500130903     C************************************************************
247600130903     C* Lettura riga SFL
247700130903     C************************************************************
247800130903     C     ReadCS1       BEGSR
247900130903     C*
248000130903     C                   Select
248100130925     C                   When      Quadro0592  = 'FA'     or
248200130925     C                             Quadro0592  = 'SA'     or
248300130925     C                             Quadro0592  = 'BL'
248400130925     C                   READC     FMTS1                                  21
248500130904     C*
248600130904     C                   When      Quadro0592  = 'FE'     or
248700130904     C                             Quadro0592  = 'FR'     or
248800130904     C                             Quadro0592  = 'NE'     or
248900130906     C                             Quadro0592  = 'NR'     or
249000130906     C                             Quadro0592  = 'DF'     or
249100130906     C                             Quadro0592  = 'FN'     or
249200131017R136 C                             Quadro0592  = 'TU'     or
249300130906     C                             Quadro0592  = 'SE'
249400130925     C                   READC     FMTS2                                  21
249500130904     C*
249600130903     C                   endSl
249700130903     C*
249800130903     C                   ENDSR
249900130903     C/EJECT
250000130903     C************************************************************
250100130903     C* CONTROLLO OPZIONI LISTA
250200130903     C************************************************************
250300130903     C     CTRS1         BEGSR
250400130903     C*
250500000000     C                   MOVEL     *OFF          $ESCI
250600000000     C                   SETOFF                                       99
250700000000     C                   Z-ADD     0             S1OPZ
250800000000     C*
250900000000     C* Leggo il sfl solo se ci sono rcd
251000000000   B1C     WMAX          IFGT      0
251100130903     C                   EXSR      ReadCS1
251200000000     C*
251300000000     C* esce se fine sfl o errore che richiede l'uscita
251400000000   B2C     *IN21         DOWEQ     *OFF
251500000000     C     $ESCI         ANDEQ     *OFF
251600000000     C                   Z-ADD     S1NRR         C1RCD
251700000000     C* ctrl su riga
251800000000     C                   EXSR      RECS1
251900000000     C* gestione opzioni
252000000000   B3C     S1OPZ         IFNE      0
252100000000     C     *IN99         ANDEQ     *OFF
252200000000     C                   EXSR      OPZS1
252300000000   E3C                   ENDIF
252400000000     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
252500000000   B3C     *IN99         IFEQ      *ON
252600000000     C                   MOVE      *ON           *IN32
252700000000   E3C                   ENDIF
252800000000     C*
252900000000     C                   Z-ADD     0             S1OPZ
253000000000     C*
253100130903     C                   EXSR      UpdateS1
253200000000     C                   MOVE      *OFF          *IN32
253300000000     C*
253400000000     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
253500000000   B3C     $ESCI         IFEQ      *OFF
253600130903     C                   EXSR      ReadCS1
253700000000     C* a fine ciclo ripristino il flag richiesta iniz.sfl
253800000000   B4C     *IN21         IFEQ      *ON
253900000000     C                   MOVE      WINZS1        $INZS1
254000000000     C                   MOVE      *off          WINZS1
254100000000     C* se è avvenuta almeno una modifica
254200000000   B5C                   IF        $INZS1 = *on
254300000000     C* calcolo pagina a cui deve posizionarsi
254400000000     C     WSFL          IFGT      0
254500000000     C                   EXSR      CLCPAG
254600000000     C                   ELSE
254700000000     C                   Z-ADD     SFLPAG        WPAG
254800000000     C                   ENDIF
254900000000   E5C                   ENDIF
255000000000   E4C                   ENDIF
255100000000   E3C                   ENDIF
255200000000     C*
255300000000   E2C                   ENDDO
255400000000     C*
255500000000   E1C                   ENDIF
255600000000     C*
255700000000     C                   ENDSR
255800000000     C/EJECT
255900000000     C************************************************************
256000000000     C* CONTROLLO CAMPI I/O RIGA LISTA
256100000000     C************************************************************
256200000000     C     RECS1         BEGSR
256300000000     C*
256400000000     C* reset indicatori DSPATR
256500000000     C                   MOVE      *ALL'0'       IN4049           10
256600000000     C                   MOVEA     IN4049        *IN(40)
256700000000     C*
256800000000     C* Controllo se Utente Autorizzato alla scelta
256900000000     C     S1OPZ         IFNE      0
257000000000     C                   Z-ADD     1             X
257100000000     C     S1OPZ         LOOKUP    $OP1(X)                                21
257200000000     C* mi salvo l'indice dell' opzione
257300000000     C   21              Z-add     X             PosOpz            2 0
257400000000     C     *IN21         IFEQ      *OFF
257500000000     C     $FL1(X)       OREQ      'N'
257600000000     C                   MOVE      *ON           $ESCI
257700000000     C                   SETON                                        409199
257800000000     C                   ENDIF
257900000000     C                   ENDIF
258000000000     C*
258100000000     C*
258200000000     C                   ENDSR
258300000000     C/EJECT
258400000000     C************************************************************
258500000000     C* GESTIONE OPZIONI LISTA
258600000000     C************************************************************
258700000000     C     OPZS1         BEGSR
258800000000     C*
258900000000     C                   Z-ADD     RIRI          CURR
259000000000     C                   Z-ADD     COCO          CURC
259100000000     C*
259200000000     C                   SELECT
259300000000     C*
259400000000     C* se opz=Selezione
259500000000     C* passo la chiave al pgm chiamante
259600000000     C     S1OPZ         WHENEQ    1
259700130829    >C                   MOVE      S1OPZ         OPZG77
259800130829     C* passare nei campi appositi della DS SDGSG77DS il valore
259900000000     C* della chiave della VL univoca (se c'è)
260000000000     C                   MOVE      *ON           $ESCI
260100000000     C                   MOVE      *ON           $FINE
260200000000     C*
260300000000     C                   MOVE      *HIVAL        $LASTV
260400000000      *
260500000000      * Dettaglio
260600000000     C     S1OPZ         WHENEQ    11
260700000000     C                   EXSR      Opzs1_11
260701160129R195 C*
260702160129 "   C* se opz=Applica soglia
260703160129 "   C     S1OPZ         WHENEQ    20
260704160129 "   C                   EXSR      Opzs1_20
260705160129R195 C*
260800000000     C*
260900000000     C*
261000000000     C* se altre opzioni
261100000000     C* richiamo pgm gestione anagrafica
261200000000     C                   OTHER
261300000000     C*
261400000000     C*
261500000000     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
261600000000     C     *IN99         IFEQ      *ON
261700000000     C                   SETON                                        40
261800000000     C                   ENDIF
261900000000     C*
262000000000     C                   MOVE      *HIVAL        $LASTV
262100000000     C                   ENDSL
262200000000     C*
262300000000     C                   ENDSR
262400000000     C/EJECT
262500000000     C************************************************************
262600000000     C* Dettaglio
262700000000     C************************************************************
262800000000     C     OpzS1_11      BEGSR
262900000000     C*
263000130829     C                   RESET                   SDGSG75DS
263100000000     C*
263200130829     C                   MOVE      xscsoc        SocietaG75
263300130829     C                   MOVEL     '21'          OPZG75
263400130829     C                   Eval      AnnoG75     = D1Anno
263500140401R133 C****               Eval      MeseG75     = 0
263600140401R133 C                   Eval      MeseG75     = MeseG77
263700140401R133 C                   Eval      TpComG75    = TpComG77
263800130829     C                   Eval      AnnG75      = D1Ann
263900130829     C                   Eval      TpAnnG75    = D1TpAnn
264000130919     C                   Eval      CFG75       = CFG77
264100130919     C                   Eval      TpOpeG75    = TpOpeG77
264200130919     C                   Eval      QuadroG75   = S1Quadro
264300130904     C                   Eval      CdFiscG75   = C1CdFisc
264400130904     C                   Eval      PartIvaG75  = C1PartIVA
264500131003     C                   Eval      CogRagG75   = H1CogRag
264600130911     C                   Eval      StatoG75    = C1Stato
264700130829     C                   Eval      NomeG75     = H1Nome
264800130829     C                   Eval      DtNascG75   = H1DtNasc
264900130829     C                   Eval      LocNascG75  = H1LocNasc
265000130829     C                   Eval      PrvNascG75  = H1PrvNasc
265100130924     C                   Eval      CodUICG75   = C1CodUIC
265200130829     C                   Eval      LocalitG75  = H1Localit
265300130829     C                   Eval      IndirEG75   = H1IndirE
265400130829     C*
265500130909     C                   Eval      TpNRG75     = H1TpNR
265600130904     C                   Eval      PIVARG75    = H1PIVAR
265700130904     C                   Eval      CdFiscRG75  = H1CdFiscR
265800130829     C                   Eval      CogRagRG75  = H1CogRagR
265900130829     C                   Eval      NomeRG75    = H1NomeR
266000130829     C                   Eval      DtNasRG75   = H1DtNasR
266100130829     C                   Eval      LocNasRG75  = H1LocNasR
266200130829     C                   Eval      PrvNasRG75  = H1PrvNasR
266300130829     C                   Eval      StatoRG75   = H1StatoR
266400130829     C                   Eval      CodUICRG75  = H1CodUICR
266500130829     C                   Eval      LocaliRG75  = H1LocaliR
266600130829     C                   Eval      IndirERG75  = H1IndirER
266700130925     C                   Eval      TpOpeRG75   = H1TpOpeR
266800130925     C                   Eval      NatOpeRG75  = H1NatOpeR
266900130926     C                   if        S1Quadro    = 'FA'       or
267000130926     C                             S1Quadro    = 'SA'       or
267100130926     C                             S1Quadro    = 'BL'
267200130926     C                   Eval      LeasinRG75  = S1Leasing
267300130926     C                   else
267400130926     C                   Eval      LeasinRG75  = S2Leasing
267500130926     C                   endif
267600000000     C*
267700130829     C                   Eval      DtRegRG75   = H1DtRegR
267800130829     C                   Eval      NrRegRG75   = H1NrRegR
267900130829     C                   Eval      SerRegRG75  = H1SerRegR
268000000000     C*
268100130829     C                   CALL      'SDGSG75R'
268200000000     C                   PARM                    KPJBA
268300130829     C                   PARM                    SDGSG75DS
268400000000     C*
268500130829     C                   EXSR      RTNG75
268600000000     C*
268700000000     C                   ENDSR
268800000000     C/EJECT
268900000000     C************************************************************
269000000000     C* GESTIONE RITORNO DA PGM DI GESTIONE
269100000000     C************************************************************
269200130829    >C     RTNG75        BEGSR
269300000000     C*
269400000000     C* modo di ritorno
269500000000     C*
269600000000     C                   SELECT
269700000000     C* questi modi di utilizzo dei valori di ritorno dal
269800000000     C* pgm di manutenzione rcd di anagrafica sono delle
269900000000     C* proposte, normalmente sempre valide, ma modificabili
270000000000     C* per situazioni particolari
270100000000     C* 1 = F3
270200130829    >C     RETG75        WHENEQ    '1'
270300000000     C                   MOVE      *ON           $ESCI                          sospese READC
270400000000     C                   MOVE      *ON           $FINE                          fine pgm
270500130829    >C                   MOVE      '1'           RETG77
270600000000     C
270700000000     C* 2 = F12
270800130829    >C     RETG75        WHENEQ    '2'
270900000000     C* se il pgm di manutenzione ha prima una guida, è necessario anche
271000000000     c* questo test, altrimenti prevale il retD74 e il sfl non viene
271100000000     C* ricaricato
271200130829    >C     OPRG75        ANDNE     '1'
271300000000     C                   MOVE      *ON           $ESCI                          sospese READC
271400000000     C*
271500000000     C* se l'opzione con cui è stato chiamato non è la modifica
271600000000     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
271700130829    >C     OPZG75        IFNE      '02'
271800000000     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
271900000000     C                   else
272000000000     C                   MOVE      *ON           $ESCI                          sospese READC
272100000000     C                   endif
272200000000     C*
272300000000     C                   ENDSL
272400000000     C*
272500000000     C* operazione eseguite dal pgm chiamato
272600000000     C*
272700000000     C                   SELECT
272800000000     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
272900130829    >C     OPRG75        WHENEQ    '1'
273000130829    >C                   MOVE      OPRG75        WOPR                           avverto pgm chiamant
273100000000     C*
273200000000     C* se l'opzione con cui è stato chiamato non è la modifica
273300000000     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
273400130829    >C     OPZG75        IFNE      '02'
273500000000     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
273600000000     C* se l'opzione con cui è stato chiamato è la modifica
273700000000     C* aggiorno subito la riga di sfl e non richiedo il ripristino sfl
273800000000     C                   ELSE
273900000000     C                   EXSR      AGGS1
274000000000     C                   ENDIF
274100000000     C*
274200000000     C                   ENDSL
274300000000     C*
274400000000     C* funzione non eseguibile per errore :
274500000000     C*
274600000000     C                   SELECT
274700000000     C* 1 = funzione richiamata chiusa in errore
274800130829    >C     ERRG75        WHENEQ    '1'
274900000000     C                   MOVE      *ON           $ESCI                          sospese READC
275000000000     C                   SETON                                        9399      emiss.SFLMSGID
275100000000     C* 2 = funzione richiamata senza la dovuta autorizzazione
275200130829    >C     ERRG75        WHENEQ    '2'
275300000000     C                   MOVE      *ON           $ESCI                          sospese READC
275400000000     C                   SETON                                        9499      emiss.SFLMSGID
275500000000     C*
275600000000     C                   ENDSL
275700000000     C*
275800000000     C                   ENDSR
275900000000     C/EJECT
275901160129R195 C************************************************************
275902160129 "   C* Applica controllo soglia
275903160129 "   C************************************************************
275904160129R195 C     OpzS1_20      BEGSR
275905160129     C*
275906160129     C                   Exsr      CtrContinua
275907160129     C*
275908160129     C                   If        $Continua='1'
275909160129     C*
275910160201    >C                   Clear                   SDG00508DS
275911160129     C*
275912160129     C                   Eval      Lr0508      = *On
275913160129     C*
275914160201     C                   Eval      Soc0508     = XScSoc
275915160129     C                   Eval      TpCom0508   = TpComG77
275916160129     C                   Eval      Anno0508    = D1Anno
275917160129     C                   Eval      Mese0508    = 0
275918160201     C                   Eval      StpAnn0508  = *Off
275919160129     C*
275920160201     C                   Eval      CogRaR0508  = H1CogRagR
275921160201     C                   Eval      NomeR0508   = H1NomeR
275922160201     C                   Eval      DtNasR0508  = H1DtNasR
275923160201     C                   Eval      LocNaR0508  = H1LocNasR
275924160201     C                   Eval      PrvNaR0508  = H1PrvNasR
275925160201     C                   Eval      StatoR0508  = H1StatoR
275926160201     C                   Eval      CodUIR0508  = H1CodUICR
275927160201     C                   Eval      LocalR0508  = H1LocaliR
275928160201     C                   Eval      IndiER0508  = H1IndirER
275929160201     C                   Eval      TpNR0508    = H1TpNR
275930160201     C                   Eval      PIVAR0508   = H1PIVAR
275931160201     C                   Eval      CdFisR0508  = H1CdFiscR
275932160201     C                   Eval      TpOpeR0508  = H1TpOpeR
275933160201     C                   Eval      NatOpR0508  = H1NatOpeR
275934160201     C                   Eval      LeasiR0508  = H1LeasinR
275935160201     C                   Eval      DtRegR0508  = H1DtRegR
275936160201     C                   Eval      NrRegR0508  = H1NrRegR
275937160201     C                   Eval      SerReR0508  = H1SerRegR
275938160129     C*
275939160129     C                   CALLB     'SDG00508R'
275940160129     C                   PARM                    KPJBA
275941160129     C                   PARM                    SDG00508DS
275942160129     C*
275943160129     C                   Exsr      Rtn508
275944160129     C*
275945160129     C                   Endif
275946160129     C*
275947160129R195 C                   ENDSR
275948160129R195 C/EJECT
275949160129R195 C******************************************************
275950160129  "  C* Controlla se continuare con l'opzione
275951160129  "  C******************************************************
275952160129R195 C     CtrContinua   BEGSR
275953160129     C*
275954160129     C                   Eval      $Continua   = *On
275955160129     C*
275956160129     C                   Eval      $MsgCd1     = 'SDC0099'
275957160129     C*
275958160129     C                   EXSR      MsgPgm
275959160129     C*
275960160129     C                   If        MsgRtn = '2'
275961160129     C                   Eval      $Continua   = *Off
275962160129     C                   Endif
275963160129     C*
275964160129R195 C                   ENDSR
275965160129R195 C/EJECT
275966160129R195 C******************************************************
275967160129  "  C* PGM PER GESTIONE MESSAGGI ERRORE                   *
275968160129  "  C******************************************************
275969160129R!95 C     MSGPGM        BEGSR
275970160129     C*
275971160129     C                   CLEAR                   SDGMSGDS
275972160129     C*
275973160129     C                   MOVEL     'S'           MSGRSP
275974160129     C                   MOVEL     '12'          MSGTPR
275975160129     C                   MOVEL     'S'           MSGEMV
275976160129     C                   Z-ADD     10            MSGRGP
275977160129     C                   MOVEL     'N'           MSGCNL
275978160129     C                   MOVEL     0             MSGMSG
275979160129     C                   MOVEA     STATUS        STS
275980160129     C                   MOVEL     'N'           MSGVID
275981160129     C                   MOVEL     'N'           MSGOPE
275982160129     C                   MOVEL     'N'           MSGSTP
275983160129     C                   MOVEL     'N'           MSGLCK
275984160129     C* Bottoni: Continua/Ritorna
275985160129     C                   EVAL      XMSGCDTXT='COS1025'+'COS1026'
275986160129     C*
275987160129     C                   CALL      'SDGMSGR'
275988160129     C                   PARM                    SDGMSGDS
275989160129     C                   PARM      *BLANKS       XMSGTXT         135
275990160129     C                   PARM      WXMsgInz      XMSGInz           1
275991160129     C                   PARM      $MsgCd1       XMSGCd1           7
275992160129     C                   PARM      *BLANKS       XMSGCd2           7
275993160129     C                   PARM      *BLANKS       XMSGCd3           7
275994160129     C                   PARM      *BLANKS       XMSGSn1           7
275995160129     C*Bottoni: Continua/Ritorna
275996160129     C                   PARM                    XMSGCdTxt        63
275997160129     C*
275998160129     C                   Move      *Off          WXMsgInz
275999160129     C*
276000160129R195 C                   ENDSR
276001160129R195 C/EJECT
276002160129R195 C************************************************************
276003160129 "   C* GESTIONE RITORNO DA PGM DI CONTROLLO SOGLIA
276004160129 "   C************************************************************
276005160129R195 C     RTN508        BEGSR
276006160129     C*
276007160129     C* operazione eseguite dal pgm chiamato
276008160129     C*
276009160129     C*
276010160129     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
276011160129     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
276012160129     C*
276013160129     C* funzione non eseguibile per errore :
276014160129     C*
276015160129     C                   SELECT
276016160129     C* 1 = funzione richiamata chiusa in errore
276017160129    >C     ERR0508       WHENEQ    '1'
276018160129     C                   MOVE      *ON           $ESCI                          sospese READC
276019160129     C                   SETON                                        9399      emiss.SFLMSGID
276020160129     C* 2 = funzione richiamata senza la dovuta autorizzazione
276021160129    >C     ERR0508       WHENEQ    '2'
276022160129     C                   MOVE      *ON           $ESCI                          sospese READC
276023160129     C                   SETON                                        9499      emiss.SFLMSGID
276024160129     C*
276025160129     C                   ENDSL
276026160129     C*
276027160129R195 C                   ENDSR
276028160129R195 C/EJECT
276029000000     C************************************************************
276100000000     C* AGGIORNAMENTO RIGA LISTA
276200000000     C************************************************************
276300000000     C     AGGS1         BEGSR
276400000000     C*
276500000000     C* riempio i campi del sfl che sono stati ripassati dal pgm
276600000000     C* di manutenzione dopo una opzione 02
276700000000     C*
276800000000     C*
276900000000     C                   ENDSR
277000000000     C/EJECT
277100000000     C************************************************************
277200000000     C* GESTIONE VALORI ASSUNTI
277300000000     C************************************************************
277400000000     C     GESD2         BEGSR
277500000000     C*
277600000000   B1C     $INZD2        IFEQ      *ON
277700000000     C                   EXSR      INZD2
277800000000     C                   MOVE      *OFF          $INZD2
277900000000   E1C                   ENDIF
278000000000     C*
278100000000   B1C     $LASTV        IFNE      'D2'
278200000000     C                   EXSR      GESPIE
278300000000   B1C     $LASTV        IFNE      *HIVAL
278400000000     C                   MOVE      $LASTV        $LASTG
278500000000     C                   ENDIF
278600000000     C                   MOVE      'D2'          $LASTV
278700000000   E1C                   ENDIF
278800000000     C*
278900000000     C* se esistono errori sulla videata
279000000000     C* emetto la write del formato a indicatori spenti per vedere
279100000000     C* le eventuali decodifiche
279200000000   B1C     *IN99         IFEQ      *ON
279300000000     C                   MOVEA     *IN           WIN
279400000000     C                   MOVE      *ALL'0'       IN5098           49
279500000000     C                   MOVEA     IN5098        *IN(50)
279600000000     C                   WRITE     FMTD2
279700000000     C                   MOVEA     WIN           *IN
279800000000   E1C                   ENDIF
279900000000     C*
280000000000     C                   EXFMT     FMTD2
280100000000     C*
280200000000     C* azzero la posizione del cursore
280300000000     C                   Z-ADD     0             H1RIGA
280400000000     C                   Z-ADD     0             H1COLO
280500000000     C*
280600000000     C* CONTROLLO ABILITAZIONE TASTI
280700000000     C* INPUT
280800000000     C* $FC3 = Abilitazioni videata D2
280900000000     C* OUTPUT
281000000000     C* *IN97 + *IN99 Errori
281100000000     C                   MOVEA     $FC3          $FC
281200000000     C                   EXSR      CTRKEY
281300000000     C*
281400000000   B1C     *IN99         IFEQ      *OFF
281500000000     C*
281600000000   B2C                   SELECT
281700000000     C* F3=Fine
281800000000     C     $TASTO        WHENEQ    F03
281900000000     C                   EXSR      F03D2
282000000000     C* F12=Ritorno
282100000000     C     $TASTO        WHENEQ    F12
282200000000     C                   EXSR      F12D2
282300000000     C* F24=Altri tasti
282400000000     C     $TASTO        WHENEQ    F24
282500000000     C                   EXSR      F24D2
282600000000     C*
282700000000     C                   OTHER
282800000000     C                   EXSR      CTRD2
282900000000   B3C     *IN99         IFEQ      *OFF
283000000000     C                   MOVE      $LASTG        $GEST
283100000000   B4C                   SELECT
283200000000     C     $LASTG        WHENEQ    'S1'
283300000000     C* se è stato modificato l'ordinamento di default, ricarico la lista
283400000000   B5C     $ORD          IFNE      D2ORD
283500000000     C                   MOVE      *ON           $INZS1
283600000000     C                   MOVE      *BLANK        W1POS
283700000000   E5C                   ENDIF
283800000000     C     $LASTG        WHENEQ    'D1'
283900000000     C* per ora niente
284000000000   E4C                   ENDSL
284100000000     C* definisco i nuovi valori di default
284200000000     C                   Z-ADD     D2LOP         $LOP
284300000000     C                   Z-ADD     D2ORD         $ORD
284400000000     C*
284500000000     C* aggiorno valori assunti
284600000000     C* se richiesto il salvataggio
284700000000   B4C     D2BSV         IFEQ      1
284800000000     C                   EXSR      AGGMPE
284900000000   E4C                   ENDIF
285000000000     C*
285100000000   E3C                   ENDIF
285200000000     C*
285300000000   E2C                   ENDSL
285400000000     C*
285500000000   E1C                   ENDIF
285600000000     C*
285700000000     C                   ENDSR
285800000000     C/EJECT
285900000000     C************************************************************
286000000000     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
286100000000     C************************************************************
286200000000     C     INZD2         BEGSR
286300000000     C*
286400000000     C                   CLEAR                   FMTD2
286500000000     C                   EVAL      D2BSVA = Normale
286600000000     C* se sono entrato con chiave univoca
286700000000     C* impedisco il salvataggio valori assunti
286800000000     C     $Unique       IFEQ      *ON
286900000000     C                   EVAL      D2BSVA = Protetto
287000000000     C                   ENDIF
287100000000     C                   MOVEL     $LOP          D2LOP
287200000000     C                   MOVE      $ORD          D2ORD
287300000000     C*
287400000000     C                   ENDSR
287500000000     C/EJECT
287600000000     C************************************************************
287700000000     C* GESTIONE F03 VIDEO D2
287800000000     C************************************************************
287900000000     C     F03D2         BEGSR
288000000000     C*
288100000000     C                   MOVE      *ON           $FINE
288200000000     C* fine programma
288300000000     C                   EXSR      ENDPGM
288400000000     C*
288500000000     C                   ENDSR
288600000000     C/EJECT
288700000000     C************************************************************
288800000000     C* GESTIONE F12 VIDEO D2
288900000000     C************************************************************
289000000000     C     F12D2         BEGSR
289100000000     C*
289200000000     C                   MOVE      $LASTG        $GEST
289300000000     C*
289400000000     C                   ENDSR
289500000000     C/EJECT
289600000000     C************************************************************
289700000000     C* GESTIONE F24 VIDEO D2
289800000000     C************************************************************
289900000000     C     F24D2         BEGSR
290000000000     C*
290100000000     C                   MOVEA     $FX3          $FX
290200000000     C                   MOVEA     $FC3          $FC
290300000000     C                   MOVEA     $FR3          $FR
290400000000     C                   Z-ADD     $ULKD2        $ULK
290500000000     C                   Z-ADD     $D2L01        $L01
290600000000     C                   Z-ADD     $D2L02        $L02
290700000000     C                   EXSR      RIEKEY
290800000000     C                   Z-ADD     $ULK          $ULKD2            3 0
290900000000     C                   MOVEA     $FC           $FC3
291000000000     C                   MOVEL     $KEY1         Z3KE1
291100000000     C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
291200000000     C                   WRITE     FMTZ3
291300000000     C*
291400000000     C                   ENDSR
291500000000     C/EJECT
291600000000     C************************************************************
291700000000     C* CONTROLLO VIDEATA VALORI ASSUNTI
291800000000     C************************************************************
291900000000     C     CTRD2         BEGSR
292000000000     C*
292100000000     C                   SETOFF                                       99
292200000000     C*
292300000000     C* OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
292400000000     C     D2LOP         IFEQ      0
292500000000     C                   SETON                                          5099
292600000000     C                   ENDIF
292700000000     C*
292800000000     C* OBBLIGATORIO VALORE PER ORDINAMENTO
292900000000     C     D2ORD         IFEQ      0
293000000000     C                   SETON                                          5199
293100000000     C                   ENDIF
293200000000     C*
293300000000     C                   ENDSR
293400000000     C/EJECT
293500000000     C************************************************************
293600000000     C* CONTROLLO ABILITAZIONE TASTI
293700000000     C* INPUT
293800000000     C* $TASTO =Tasto funzione premuto
293900000000     C* $FC = Tasti abilitati
294000000000     C* OUTPUT
294100000000     C* IN99 = ERrore
294200000000     C************************************************************
294300000000     C     CTRKEY        BEGSR
294400000000     C*
294500000000     C                   SETOFF                                       99
294600000000     C*
294700000000     C                   SELECT
294800000000     C     $TASTO        WHENEQ    F01
294900000000     C                   z-add     01            posfun
295000000000     C     $FC(1)        IFEQ      '0'
295100000000     C                   SETON                                        9997
295200000000     C                   ENDIF
295300000000     C     $TASTO        WHENEQ    F02
295400000000     C                   z-add     02            posfun
295500000000     C     $FC(2)        IFEQ      '0'
295600000000     C                   SETON                                        9997
295700000000     C                   ENDIF
295800000000     C     $TASTO        WHENEQ    F03
295900000000     C                   z-add     03            posfun
296000000000     C     $FC(3)        IFEQ      '0'
296100000000     C                   SETON                                        9997
296200000000     C                   ENDIF
296300000000     C     $TASTO        WHENEQ    F04
296400000000     C                   z-add     04            posfun
296500000000     C     $FC(4)        IFEQ      '0'
296600000000     C                   SETON                                        9997
296700000000     C                   ENDIF
296800000000     C     $TASTO        WHENEQ    F05
296900000000     C                   z-add     05            posfun
297000000000     C     $FC(5)        IFEQ      '0'
297100000000     C                   SETON                                        9997
297200000000     C                   ENDIF
297300000000     C     $TASTO        WHENEQ    F06
297400000000     C                   z-add     06            posfun
297500000000     C     $FC(6)        IFEQ      '0'
297600000000     C                   SETON                                        9997
297700000000     C                   ENDIF
297800000000     C     $TASTO        WHENEQ    F07
297900000000     C                   z-add     07            posfun
298000000000     C     $FC(7)        IFEQ      '0'
298100000000     C                   SETON                                        9997
298200000000     C                   ENDIF
298300000000     C     $TASTO        WHENEQ    F08
298400000000     C                   z-add     08            posfun
298500000000     C     $FC(8)        IFEQ      '0'
298600000000     C                   SETON                                        9997
298700000000     C                   ENDIF
298800000000     C     $TASTO        WHENEQ    F09
298900000000     C                   z-add     09            posfun
299000000000     C     $FC(09)       IFEQ      '0'
299100000000     C                   SETON                                        9997
299200000000     C                   ENDIF
299300000000     C     $TASTO        WHENEQ    F10
299400000000     C                   z-add     10            posfun
299500000000     C     $FC(10)       IFEQ      '0'
299600000000     C                   SETON                                        9997
299700000000     C                   ENDIF
299800000000     C     $TASTO        WHENEQ    F11
299900000000     C                   z-add     11            posfun
300000000000     C     $FC(11)       IFEQ      '0'
300100000000     C                   SETON                                        9997
300200000000     C                   ENDIF
300300000000     C     $TASTO        WHENEQ    F12
300400000000     C                   z-add     12            posfun
300500000000     C     $FC(12)       IFEQ      '0'
300600000000     C                   SETON                                        9997
300700000000     C                   ENDIF
300800000000     C     $TASTO        WHENEQ    F13
300900000000     C                   z-add     13            posfun
301000000000     C     $FC(13)       IFEQ      '0'
301100000000     C                   SETON                                        9997
301200000000     C                   ENDIF
301300000000     C     $TASTO        WHENEQ    F14
301400000000     C                   z-add     14            posfun
301500000000     C     $FC(14)       IFEQ      '0'
301600000000     C                   SETON                                        9997
301700000000     C                   ENDIF
301800000000     C     $TASTO        WHENEQ    F15
301900000000     C                   z-add     15            posfun
302000000000     C     $FC(15)       IFEQ      '0'
302100000000     C                   SETON                                        9997
302200000000     C                   ENDIF
302300000000     C     $TASTO        WHENEQ    F16
302400000000     C                   z-add     16            posfun
302500000000     C     $FC(16)       IFEQ      '0'
302600000000     C                   SETON                                        9997
302700000000     C                   ENDIF
302800000000     C     $TASTO        WHENEQ    F17
302900000000     C                   z-add     17            posfun
303000000000     C     $FC(17)       IFEQ      '0'
303100000000     C                   SETON                                        9997
303200000000     C                   ENDIF
303300000000     C     $TASTO        WHENEQ    F18
303400000000     C                   z-add     18            posfun
303500000000     C     $FC(18)       IFEQ      '0'
303600000000     C                   SETON                                        9997
303700000000     C                   ENDIF
303800000000     C     $TASTO        WHENEQ    F19
303900000000     C                   z-add     19            posfun
304000000000     C     $FC(19)       IFEQ      '0'
304100000000     C                   SETON                                        9997
304200000000     C                   ENDIF
304300000000     C     $TASTO        WHENEQ    F20
304400000000     C                   z-add     20            posfun
304500000000     C     $FC(20)       IFEQ      '0'
304600000000     C                   SETON                                        9997
304700000000     C                   ENDIF
304800000000     C     $TASTO        WHENEQ    F21
304900000000     C                   z-add     21            posfun
305000000000     C     $FC(21)       IFEQ      '0'
305100000000     C                   SETON                                        9997
305200000000     C                   ENDIF
305300000000     C     $TASTO        WHENEQ    F22
305400000000     C                   z-add     22            posfun
305500000000     C     $FC(22)       IFEQ      '0'
305600000000     C                   SETON                                        9997
305700000000     C                   ENDIF
305800000000     C     $TASTO        WHENEQ    F23
305900000000     C                   z-add     23            posfun
306000000000     C     $FC(23)       IFEQ      '0'
306100000000     C                   SETON                                        9997
306200000000     C                   ENDIF
306300000000     C     $TASTO        WHENEQ    F24
306400000000     C                   z-add     24            posfun
306500000000     C     $FC(24)       IFEQ      '0'
306600000000     C                   SETON                                        9997
306700000000     C                   ENDIF
306800000000     C                   ENDSL
306900000000     C*
307000000000     C                   ENDSR
307100000000     C/EJECT
307200000000     C************************************************************
307300000000     C* AGGIORNAMENTO VALORI ASSUNTI
307400000000     C************************************************************
307500000000     C     AGGMPE        BEGSR
307600000000     C*
307700000000     C* preparo i parametri di chiamata al driver
307800000000     C                   CLEAR                   XMPEDS
307900000000     C                   EVAL      TpChiamata = '03'
308000000000     C                   EVAL      NChiavi = 5
308100000000     C                   EVAL      VLogica = 2
308200000000     C*
308300000000     C* preparo le chiavi per la chiamata al driver
308400000000     C                   CLEAR                   XMPEDS1
308500000000     C                   MOVEL     XSCSOC        DSMPESOC
308600000000     C                   MOVEL     DSPGM         DSMPEPGM
308700000000     C                   MOVEL     KNMUS         DSMPEPRF
308800000000     C*
308900000000     C* Si memorizzano i dati di default a seconda delle modalità con
309000000000     C* cui il pgm è usato
309100000000     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
309200000000     C* MPETIP = 'PV'  pgm richiamato da menù
309300000000     C* MPETIP = 'PB'  x lancio Batch
309400000000     C*
309500000000     C                   SELECT
309600130829    >C     OPZG77        WHENEQ    '01'
309700000000     C                   MOVEL     'PX'          DSMPETIP
309800000000     C                   OTHER
309900000000     C                   MOVEL     'PV'          DSMPETIP
310000000000     C                   ENDSL
310100000000     C* dati
310200000000     C                   MOVEL     DSMPE         DSMPEPAR
310300000000     C*
310400000000     C                   CALLB     'XMPE'
310500000000     C                   PARM                    XMPEDS
310600000000     C                   PARM                    XMPEDS1
310700000000     C*
310800000000     C                   ENDSR
310900000000     C/EJECT
311000000000     C************************************************************
311100000000     C* OPERAZIONI INIZIALI
311200000000     C************************************************************
311300000000     C     *INZSR        BEGSR
311400000000     C*
311500000000     C* Reperimento parametri
311600000000     C*
311700000000     C     *ENTRY        PLIST
311800000000     C                   PARM                    KPJBA
311900130829     C                   PARM                    SDGSG77DS
312000000000     C*
312100000000     C*
312200000000     C* Reperimento dati società
312300000000     C*
312400000000     C                   MOVEL     'SOC001'      TIPXSC
312500000000     C                   MOVEL     *BLANK        SOCXSC
312600000000     C                   EXSR      REPSOC
312700000000     C*
312800000000     C* Controlli dati società
312900000000     C*
313000000000     C     RTNXSC        IFNE      '1'
313100000000     C                   MOVEL     XSOCDS        SOC001
313200000000     C                   ELSE
313300000000     C                   MOVE      *ON           $FINE
313400130829    >C                   MOVE      '1'           ErrG77
313500000000     C                   EXSR      ENDPGM
313600000000     C                   ENDIF
313700000000     C*
313800000000     C* reperisco le dimensioni delle variabili che conterranno i tasti
313900000000     C                   EVAL      $D1L01 = %Size(Z1Ke1)
314000000000     C                   EVAL      $D1L02 = %Size(Z1Ke2)
314100000000     C                   EVAL      $S1L01 = %Size(Z2Ke1)
314200000000     C                   EVAL      $S1L02 = %Size(Z2Ke2)
314300000000     C                   EVAL      $C1Op1 = %Size(C1Opz1)
314400000000     C                   EVAL      $C1Op2 = %Size(C1Opz2)
314500000000     C                   EVAL      $D2L01 = %Size(Z3Ke1)
314600000000     C                   EVAL      $D2L02 = 0
314700000000     C*
314800000000     C* Reperimento opzioni
314900000000     C*
315000000000     C                   EXSR      REPOPZ
315100000000     C*
315200000000     C* Reperimento tasti di funzione
315300000000     C*
315400000000     C                   EXSR      REPKEY
315500000000     C* Chiamata a routine per il reperimento delle opzioni
315600000000     C* personalizzate
315700000000    >C* se si vogliono abilitare i tasti personalizzati in ogni videata
315800000000    >C* ripeterla passando le schiere dei tasti funzione
315900000000     C                   CAllb     'X46USREXT'
316000000000     C                   PARM                    $OP1
316100000000     C                   PARM                    $TX1
316200000000     C                   PARM                    $FL1
316300000000     C                   PARM                    $PGO1
316400000000     C                   PARM      $dimOp        NumOpz
316500000000     C                   PARM                    $FX2
316600000000     C                   PARM                    $FC2
316700000000     C                   PARM                    $PGF2
316800000000     C                   PARM                    DsPgm
316900000000     C                   PARM                    xsccut
317000000000     C                   PARM                    xscgrp
317100000000     C*
317200000000     C                   Z-add     1             OpzLun
317300000000     C     *blank        Lookup    $TX1(OpzLun)                           21
317400000000     C                   if        *in21 = *on
317500000000     C                   sub       1             OpzLun
317600000000     C                   else
317700000000     C                   Z-add     $DimOp        OpzLun
317800000000     C                   endif
317900000000     C*
318000000000     C                   ENDSR
318100000000     C/EJECT
318200000000     C************************************************************
318300000000     C* INIZIALIZZAZIONE VARIABILI
318400000000     C************************************************************
318500000000     C     INZVAR        BEGSR
318600000000     C*
318700000000     C* Se società diversa da quella impostata nella *inzsr, rieseguo
318800000000     C* reperimento dati società
318900130829    >C                   IF        SocietaG77 <> xscsoc  and
319000130829    >C                             OpzG77 <>*blank
319100000000     C                   MOVEL     'SOC001'      TIPXSC
319200130829    >C                   MOVEL     SocietaG77    SOCXSC
319300000000     C                   EXSR      REPSOC
319400000000     C     RTNXSC        IFNE      '1'
319500000000     C                   MOVEL     XSOCDS        SOC001
319600000000     C                   ELSE
319700000000     C                   MOVE      *ON           $FINE
319800130829    >C                   MOVE      '1'           ErrG77
319900000000     C                   EXSR      ENDPGM
320000000000     C                   ENDIF
320100000000     C                   ENDIF
320200130906     C*
320300130918     C* Reperisco dati della società da tab. G12
320400130918     C                   EXSR      RepG12Soc
320500130906     C*
320501160105R195 C*
320502160105 "   C                   Select
320503160105 "   C* se Spesometro o Acquisti da San marino
320504160105 "   C                   When      TpComG77    = 'E'     or
320505160105R195 C                             TpComG77    = 'S'
320600130906     C                   EXSR      RepSDGSPECF
320601160105R195 C* se Black List
320602160105 "   C                   When      TpComG77    = 'B'
320603160105 "   C                   EXSR      RepSDBLACK2
320604160105 "   C*
320605160105R195 C                   endSl
320700000000     C*
320800000000     C* Reperimento autorizzazioni
320900000000     C*
321000000000     C* le autorizzazioni potrebbero essere già state passate,
321100000000     C* nel qual caso non vanno reperite
321200000000    >C                   EXSR      REPAUT
321300000000     C*
321400000000     C* visualizzo il sfl troncato
321500000000     C                   Eval      C1Mode      = *Off
321600000000     C*
321700000000     C*
321800000000     C* Reperimento valori default utente
321900000000     C*
322000000000     C                   EXSR      REPDFT
322100000000     C*
322200000000     C* Definizione delle opzioni utilizzabili a pgm
322300000000     C*
322400000000     C                   EXSR      DEFOPZ
322500000000     C*
322600000000     C* Variabili per gestione videate
322700000000     C*
322800000000     C                   MOVE      *OFF          $FINE                          fine pgm
322900000000     C                   MOVE      *OFF          $INZD1                         inizializzione D1
323000000000     C                   MOVE      *OFF          $INZS1                         inizializzione S1
323100000000     C                   MOVE      *OFF          $INZD2                         inizializzione D2
323200000000     C                   MOVE      *OFF          $EndFile                       memo fine file
323300000000     C                   MOVE      *OFF          $ESCI                          rich.uscita ciclo
323400000000     C                   MOVE      *OFF          $RCDOK                         rcd letto valido
323500000000     C                   MOVE      *HIVAL        $LASTV
323600000000     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
323700000000     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
323800000000     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
323900000000     C* azzero la posizione del cursore
324000000000     C                   Z-ADD     0             H1RIGA
324100000000     C                   Z-ADD     0             H1COLO
324200000000     C*
324300000000     C                   MOVE      *ON           $INZD1
324400000000     C                   MOVE      *ON           $INZS1
324500000000     C*
324600000000     C* se pgm richiamato con chiave univoca impostata,
324700000000     C* forzo la emissione della lista e imposto l'ordinamento
324800000000    >C*  in questo esempio la chiave univoca per la posta è Codice
324900000000    >C*  ma qualora ci fosse più di un campo basterebbe testare quello più
325000000000    >C*  significativo (o meglio, quello che si sa essere obbligatorio
325100000000    >C*  ricevere pieno)
325200000000     C                   MOVE      *OFF          $Unique
325300000000     C*
325400000000     C* Variabili appoggio
325500000000     C*
325600000000     C                   Z-ADD     SFLPAG        WPAG
325700000000     C                   MOVEL     *OFF          WOPR
325800000000     C                   CLEAR                   W1POS
325900000000     C*
326000000000     C* definisco qual'è la prima videata
326100000000     C* (se la prima videata risulta la lista, devo comunque caricare
326200000000     C*  la videata di parzializzazione e gestire la definizione
326300000000     C*  dell'ordinamento, perchè, dai default oppure se sono stati
326400000000     C*  passati dei dati, può variare il caricamento)
326500000000     C                   SELECT
326600000000     C     $LOP          WHENEQ    1
326700000000     C                   MOVE      'D1'          $GEST
326800000000     C     $LOP          WHENEQ    2
326900130903???? C                   MOVE      'S1'          $GEST
327000000000     C                   EXSR      INZD1
327100000000     C                   MOVE      *OFF          $INZD1
327200000000     C                   EXSR      F02D1
327300000000     C                   EXSR      DEFORD
327400000000     C                   ENDSL
327500000000     C*
327600000000     C* Valorizzazione campi univoci testate
327700000000     C*
327800000000     C                   CLEAR                   FMTT1
327900000000     C                   MOVEL     KNSIF         NOMSIF
328000000000     C                   MOVEL     XSCDSI        NOMDIT
328100000000     C                   MOVEL     KCDAZ         NOMAZN
328200000000     C****               MOVEL     DSPGM         NOMPGM
328300000000     C* recupero il sottotitolo (se c'è)
328400000000     C                   MOVEL     *BLANK        T1TIT
328500000000     C     SOTTIT        IFNE      *BLANK
328600000000     C                   MOVEL     SOTTIT        IDMSG
328700000000     C                   CALLB     'XRTVM'
328800000000     C                   PARM                    IDMSG            27
328900000000     C                   PARM                    TXTMSG          100
329000000000     C                   PARM                    ERRMSG            1
329100000000     C                   PARM      *ON           CTRMSG            1            centratura
329200000000     C                   PARM      30            LENMSG            3 0          lun output.
329300000000     C     ERRMSG        IFNE      *ON
329400000000     C                   MOVEL     TXTMSG        T1TIT
329500000000     C                   ELSE
329600000000     C                   MOVEL     *ALL'?'       T1TIT
329700000000     C                   ENDIF
329800000000     C                   ENDIF
329801151201R195 C*
329802151201  "  C* Recupero il titolo con il tipo di comunicazione
329803151201  "  C                   MOVEL     *BLANK        T1TIT
329804151201  "  C*
329805151201  "  C                   Select
329806151201  "  C*
329807151201  "  C* Comunicazione Spesometro
329808151201  "  C                   When      TpComG77   = 'E'
329809151201  "  C                   if        SOTTITT1   <> *BLANK
329810151201  "  C                   MOVEL     SOTTITT1      IDMSG
329811151201  "  C                   endif
329812151201  "  C*
329813151201  "  C* Comunicazione Acquisti da San Marino
329814151201  "  C                   When      TpComG77   = 'S'
329815151201  "  C                   if        SOTTITT2   <> *BLANK
329816151201  "  C                   MOVEL     SOTTITT2      IDMSG
329817151201  "  C                   endif
329818151201  "  C*
329819151201  "  C* Comunicazione Black list
329820151201  "  C                   When      TpComG77   = 'B'
329821151201  "  C                   if        SOTTITT3   <> *BLANK
329822151201  "  C                   MOVEL     SOTTITT3      IDMSG
329823151201  "  C                   endif
329824151201  "  C*
329825151201  "  C                   endsl
329826151201  "  C*
329827151201  "  C                   CALLB     'XRTVM'
329828151201  "  C                   PARM                    IDMSG            27
329829151201  "  C                   PARM                    TXTMSG          100
329830151201  "  C                   PARM                    ERRMSG            1
329831151201  "  C                   PARM      *ON           CTRMSG            1            centratura
329832151201  "  C                   PARM      30            LENMSG            3 0          lun output.
329833151201  "  C     ERRMSG        IFNE      *ON
329834151201  "  C                   MOVEL     TXTMSG        T1TIT
329835151201  "  C                   ELSE
329836151201  "  C                   MOVEL     *ALL'?'       T1TIT
329837151201R195 C                   ENDIF
329900000000     C*
330000000000     C                   ENDSR
330100000000     C/EJECT
330200000000     C************************************************************
330300000000     C* REPERIMENTO DATI SOCIETA'
330400000000     C************************************************************
330500000000     C     REPSOC        BEGSR
330600000000     C*
330700000000     C                   CALL      'SDGSOCDR'
330800000000     C                   PARM                    TIPXSC            6
330900000000     C                   PARM                    SOCXSC            3
331000000000     C                   PARM                    CDSXSC            9 0
331100000000     C                   PARM                    MODXSC            3
331200000000     C                   PARM      *BLANKS       RTNXSC            1
331300000000     C                   PARM                    XSOCDS
331400000000     C                   PARM                    KPJBA
331500000000     C*
331600000000     C                   ENDSR
331700000000     C/EJECT
331800130918     C************************************************************
331900130918     C* Reperisco dati da G12 della società
332000130918     C************************************************************
332100130918     C     RepG12Soc     BEGSR
332200130918     C*
332300130918     C                   Eval      $UICSoc     = 0
332400130918     C                   Eval      $NazSoc     = *Blank
332500130918     C*
332600130918     C                   Eval      WStato      = XScSta
332700130918     C                   EXSR      RepTabG12
332800130918     C*
332900130918     C                   if        XTBErr     <> '0'
333000130918     C                   Eval      ERRG77      = '1'
333100130918     C                   EXSR      ENDPGM
333200130918     C                   endif
333300130918     C*
333400130918     C                   if        $CodUIC     = 0
333500130918     C                   Eval      ERRG77      = '1'
333600130918     C                   EXSR      ENDPGM
333700130918     C                   endif
333800130918     C*
333900130918     C                   Eval      $UICSoc     = $CodUIC
334000130918     C*
334100130918     C                   Eval      $NazSoc     = $NazISO
334200130918     C                   if        $NazSoc     = *Blank
334300130918     C                   Eval      $NazSoc     = XScSta
334400130918     C                   endif
334500130918     C*
334600130918     C                   ENDSR
334700130918      /EJECT
334800130918     C************************************************************
334900130918     C* Reperisco dati tabella G12
335000130918     C************************************************************
335100130918     C     RepTabG12     BEGSR
335200130918     C*
335300130918     C                   Eval      $CodUIC     = 0
335400130918     C                   Eval      $NazISO     = *Blank
335500130918     C*
335600130918     C                   Clear                   ANGG12DS
335700130918     C                   Clear                   SDGTABDS
335800130918     C*
335900130918     C                   Eval      XTbRic      = '1'
336000130918     C                   Eval      XTbAzi      = XScSoc
336100130918     C                   Eval      XTbKey      = *Zero
336200130918     C                   Eval      XTbCod      = 'G12'
336300130918     C                   Move      WStato        XTbKey
336400130918     C*
336500130918     C                   CALL      'SDGTABDR'
336600130918     C                   Parm                    SDGTABDS
336700130918     C*
336800130918     C                   if        XTBErr      = '0'
336900130918     C                   Eval      ANGG12DS    = XTbUni
337000130918     C*
337100130918     C                   if        %SubSt(ANGG12DS:50:3)   = *Blank
337200130918     C                   Z-Add     0             $CodUIC
337300130918     C                   else
337400130918     C                   Z-Add     CodUICG12     $CodUIC
337500130918     C                   endif
337600130918     C*
337700130918     C                   Eval      $NazISO     = NazG12
337800130918     C*
337900130918     C                   endif
338000130918     C*
338100130918     C                   ENDSR
338200130918      /EJECT
338300130906     C************************************************************
338400130906     C* Reperimento parametri di setup per Spesometro cli/for
338500130906     C************************************************************
338600130906     C     RepSDGSPECF   BEGSR
338700130906     C*
338800130906     C                   Clear                   SDGPA08DS
338900130906     C*
339000130906     C                   CALLB     'XPAR'
339100130906     C                   Parm                    XScSoc
339200130906     C                   Parm      'SDGSPECF'    XPaPar            8
339300130906     C                   Parm                    XPaOut         2000
339400130906     C                   Parm      *OFF          XPaErr            1
339500130906     C                   Parm      ' '           XPaRic            1
339600130906     C*
339700130906     C                   if        XPaErr      = *On
339800130906     C                   Eval      ERRG77      = '1'
339900130906     C                   EXSR      ENDPGM
340000130906     C*
340100130906     C                   else
340200130906     C                   MoveL     XPaOut        SDGPA08DS
340300130906     C*
340400130906     C                   if        AttSCFA08   <> *On
340500130906     C                   Eval      ERRG77      = '1'
340600130906     C                   EXSR      ENDPGM
340700130906     C*
340800130906     C                   endif
340900130906     C*
341000130906     C                   endif
341100130906     C*
341200130906     C                   ENDSR
341300130906     C/EJECT
341301160105R195 C************************************************************
341302160105 "   C* Reperimento parametri di setup per Black List
341303160105 "   C************************************************************
341304160105R195 C     RepSDBLACK2   BEGSR
341305160105     C*
341306160105     C                   Clear                   SDGPA11DS
341307160105     C*
341308160105     C                   CALLB     'XPAR'
341309160105     C                   Parm                    XScSoc
341310160105     C                   Parm      'SDBLACK2'    XPaPar            8
341311160105     C                   Parm                    XPaOut         2000
341312160105     C                   Parm      *OFF          XPaErr            1
341313160105     C                   Parm      ' '           XPaRic            1
341326160105     C*
341327160105     C                   if        XPaErr      = *On
341328160105     C                   Eval      ERRG77      = '1'
341329160105     C                   EXSR      ENDPGM
341330160105     C*
341331160105     C                   else
341332160105     C                   MoveL     XPaOut        SDGPA11DS
341333160105     C*
341334160105     C                   if        AttBLA11    <> *On
341336160105     C                   Eval      ERRG77      = '1'
341337160105     C                   EXSR      ENDPGM
341338160105     C*
341339160105     C                   endif
341340160105     C*
341341160105     C                   endif
341342160105     C*
341343160105R195 C                   ENDSR
341344160105R195 C/EJECT
341400000000     C************************************************************
341500000000     C* REPERIMENTO AUTORIZZAZIONI UTENTE
341600000000     C************************************************************
341700000000     C     REPAUT        BEGSR
341800000000     C*
341900000000     C                   CLEAR                   AUTDS
342000000000     C                   Clear                   ConCivDs
342100000000     C*
342200000000     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
342300000000     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
342400000000     C                   Z-ADD     0             XCAGRP                         GRUPPO
342500000000     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
342600000000    >C                   MOVEL     'CONCIV'      XCAFNC                         FUNZIONE
342700000000     C                   MOVEL     *ZERO         XCAVFU
342800000000    >C                   MOVE      'CK'          XCATCK                         TIPO CTRL
342900000000     C                   MOVE      '0'           XCANAU                         RICH.XNOAUT
343000000000     C*
343100000000     C                   CALLB     'XCHKAUT'
343200000000     C                   PARM                    AUTDS
343300000000     C*
343400000000     C* errori/non abilitazione
343500000000     C     XCARTN        IFNE      0
343600000000     C*
343700000000     C                   ELSE
343800000000    >C                   MOVEL     XCADDS        CONCIVDS
343900000000     C                   ENDIF
344000000000     C*
344100000000     C                   ENDSR
344200000000     C/EJECT
344300000000     C************************************************************
344400000000     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
344500000000     C************************************************************
344600000000     C     REPDFT        BEGSR
344700000000     C*
344800000000     C* preparo i parametri di chiamata al driver
344900000000     C                   CLEAR                   XMPEDS
345000000000     C                   EVAL      TpChiamata = '01'
345100000000     C                   EVAL      NChiavi = 5
345200000000     C                   EVAL      VLogica = 2
345300000000     C*
345400000000     C* preparo le chiavi per la chiamata al driver
345500000000     C                   CLEAR                   XMPEDS1
345600000000     C                   MOVEL     XSCSOC        DSMPESOC
345700000000     C                   MOVEL     DSPGM         DSMPEPGM
345800000000     C                   MOVEL     KNMUS         DSMPEPRF
345900000000     C* Si reperiscono i dati di default a seconda delle modalità con
346000000000     C* cui il pgm è usato
346100000000     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
346200000000     C* MPETIP = 'PV'  pgm richiamato da menù
346300000000     C* MPETIP = 'PB'  x lancio Batch
346400000000     C                   SELECT
346500130829    >C     OPZG77        WHENEQ    '01'
346600000000     C                   MOVEL     'PX'          DSMPETIP
346700000000     C                   OTHER
346800000000     C                   MOVEL     'PV'          DSMPETIP
346900000000     C                   ENDSL
347000000000     C*
347100000000     C                   CALLB     'XMPE'
347200000000     C                   PARM                    XMPEDS
347300000000     C                   PARM                    XMPEDS1
347400000000     C*
347500000000     C     Risultato     IFEQ      '0'
347600000000     C                   MOVEL     DSMPEPAR      DSMPE
347700000000     C* nel caso sia la prima volta che l'utente apre questa anagrafica
347800000000     C* i suoi valori di default vengono decisi a pgm
347900000000     C                   ELSE
348000000000     C                   Z-ADD     2             $LOP
348100000000     C                   Z-ADD     1             $ORD
348200000000     C                   ENDIF
348300000000     C*
348400000000     C                   ENDSR
348500000000     C/EJECT
348600000000     C************************************************************
348700000000     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
348800000000     C************************************************************
348900000000     C     DEFOPZ        BEGSR
349000000000     C*
349100000000     C* Questa routine deve valorizzare la schiera $FL1
349200000000     C* con S o N a seconda della utilizzabilità o meno
349300000000     C* dell'opzione correlata
349400000000     C*
349500000000     C* Inizialmente si considerano tutte le opzioni come valide
349600000000     C     1             DO        $DIMOP        X
349700000000     C* Imposta tutti i flg delle opzioni non a *blank, a 'S'
349800000000     C                   If        $TX1(X)  <> *Blank
349900000000     C                   MOVE      'S'           $FL1(X)
350000000000     C                   Else
350100000000     C                   Leave
350200000000     C                   EndIf
350300000000     C                   ENDDO
350400000000     C*
350500000000     C* Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
350600000000     C* con 1 o 0 a seconda della utilizzabilità o meno
350700000000     C* del tasto funzione correlato
350800000000     C*
350900000000     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
351000000000     C* di compilazione
351100000000     C                   RESET                   $FC1
351200000000     C                   RESET                   $FR1
351300000000     C                   RESET                   $FC2
351400000000     C                   RESET                   $FR2
351500000000     C                   RESET                   $FC3
351600000000     C                   RESET                   $FR3
351700000000     C*
351800000000     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
351900000000     C* invalido alcune opzioni
352000000000     C                   SELECT
352100000000     C* Se pgm richiamato in ricerca disattivo :
352200130829    >C     OPZG77        WHENEQ    '01'
352300000000     C* F3=Fine
352400000000     C                   Z-ADD     3             X
352500000000     C                   MOVE      '0'           $FC1(X)
352600000000     C                   MOVE      '0'           $FC2(X)
352700000000     C                   MOVE      '0'           $FC3(X)
352800000000     C*
352900000000     C*
353000000000     C* Se pgm richiamato in interrogazione disattivo :
353100130829    >C     OPZG77        WHENEQ    '05'
353200000000     C* F10=Immissione
353300000000     C                   Z-ADD     10            X
353400000000     C                   MOVE      '0'           $FC1(X)
353500000000     C                   MOVE      '0'           $FC2(X)
353600000000     C* 1=Selezione
353700000000     C                   Z-ADD     1             X
353800000000     C     01            LOOKUP    $OP1(X)                                21
353900000000     C     *IN21         IFEQ      *ON
354000000000     C                   MOVE      'N'           $FL1(X)
354100000000     C                   ENDIF
354200000000     C* 2=Modifica
354300000000     C                   Z-ADD     1             X
354400000000     C     02            LOOKUP    $OP1(X)                                21
354500000000     C     *IN21         IFEQ      *ON
354600000000     C                   MOVE      'N'           $FL1(X)
354700000000     C                   ENDIF
354800000000     C* 3=Copia
354900000000     C                   Z-ADD     1             X
355000000000     C     03            LOOKUP    $OP1(X)                                21
355100000000     C     *IN21         IFEQ      *ON
355200000000     C                   MOVE      'N'           $FL1(X)
355300000000     C                   ENDIF
355400000000     C* 4=Annullamento
355500000000     C                   Z-ADD     1             X
355600000000     C     04            LOOKUP    $OP1(X)                                21
355700000000     C     *IN21         IFEQ      *ON
355800000000     C                   MOVE      'N'           $FL1(X)
355900000000     C                   ENDIF
356000000000     C*
356100130829    >C     OPZG77        WHENEQ    *BLANKS
356200000000     C* Se pgm non richiamato, disattivo subito :
356300000000     C* F8=Selezione seguente
356400000000     C                   Z-ADD     8             X
356500000000     C                   MOVE      '0'           $FC1(X)
356600000000     C                   MOVE      '0'           $FC2(X)
356700000000     C* 1=Selezione
356800000000     C                   Z-ADD     1             X
356900000000     C     01            LOOKUP    $OP1(X)                                21
357000000000     C     *IN21         IFEQ      *ON
357100000000     C                   MOVE      'N'           $FL1(X)
357200000000     C                   ENDIF
357300000000     C*
357400000000     C* In tutti gli altri casi, disattivo subito :
357500000000     C                   OTHER
357600000000     C* 1=Selezione
357700000000     C                   Z-ADD     1             X
357800000000     C     01            LOOKUP    $OP1(X)                                21
357900000000     C     *IN21         IFEQ      *ON
358000000000     C                   MOVE      'N'           $FL1(X)
358100000000     C                   ENDIF
358200000000     C*
358300000000     C                   ENDSL
358400000000     C*
358401160129R195 C* Se non sto elaborando la black list disattivo l'opzione
358402160129 "   C* 20 = Applica soglia
358403160129 "   C                   If        TpComG77    <> 'B'
358404160129 "   C                   Z-ADD     1             X
358405160129 "   C     20            LOOKUP    $OP1(X)                                21
358406160129 "   C     *IN21         IFEQ      *ON
358407160129 "   C                   MOVE      'N'           $FL1(X)
358408160129 "   C                   EndIF
358413160129R195 C                   ENDIF
358500000000     C*
358600000000     C* poi, disattivo a seconda delle autorizzazioni :
358700000000     C                   IF        ImmAll <> *on
358800000000     C*
358900000000     C* F10=Immissione
359000000000     C                   Z-ADD     10            X
359100000000     C                   MOVE      '0'           $FC1(X)
359200000000     C                   MOVE      '0'           $FC2(X)
359300000000     C*
359400000000     C                   Z-ADD     1             X
359500000000     C     03            LOOKUP    $OP1(X)                                21
359600000000     C     *IN21         IFEQ      *ON
359700000000     C                   MOVE      'N'           $FL1(X)
359800000000     C                   ENDIF
359900000000     C*
360000000000     C                   ENDIF
360100000000     C*
360200000000     C                   IF        ManAll <> *on
360300000000     C                   Z-ADD     1             X
360400000000     C     02            LOOKUP    $OP1(X)                                21
360500000000     C     *IN21         IFEQ      *ON
360600000000     C                   MOVE      'N'           $FL1(X)
360700000000     C                   ENDIF
360800000000     C*
360900000000     C                   IF        VisAll <> *on
361000000000     C                   Z-ADD     1             X
361100000000     C     22            LOOKUP    $OP1(X)                                21
361200000000     C     *IN21         IFEQ      *ON
361300000000     C                   MOVE      'N'           $FL1(X)
361400000000     C                   ENDIF
361500000000     C                   endif
361600000000     C                   ENDIF
361700000000     C*
361800000000     C                   IF        AnnAll <> *on
361900000000     C                   Z-ADD     1             X
362000000000     C     04            LOOKUP    $OP1(X)                                21
362100000000     C     *IN21         IFEQ      *ON
362200000000     C                   MOVE      'N'           $FL1(X)
362300000000     C                   ENDIF
362400000000     C*
362500000000     C                   Z-ADD     1             X
362600000000     C     44            LOOKUP    $OP1(X)                                21
362700000000     C     *IN21         IFEQ      *ON
362800000000     C                   MOVE      'N'           $FL1(X)
362900000000     C                   ENDIF
363000000000     C                   ENDIF
363100000000     C*
363200000000     C                   IF        VisAll <> *on
363300000000     C                   Z-ADD     1             X
363400000000     C     05            LOOKUP    $OP1(X)                                21
363500000000     C     *IN21         IFEQ      *ON
363600000000     C                   MOVE      'N'           $FL1(X)
363700000000     C                   ENDIF
363800000000     C                   ENDIF
363900000000     C*
364000000000     C*
364100000000     C                   ENDSR
364200000000     C/EJECT
364300000000     C************************************************************
364400000000     C* REPERIMENTO OPZIONI
364500000000     C************************************************************
364600000000     C     REPOPZ        BEGSR
364700000000     C*
364800000000     C     1             DO        $DIMOP        X
364900000000     C*
365000000000     C     $MS1(X)       IFNE      *BLANK
365100000000     C*
365200000000     C                   MOVEL     $MS1(X)       IDMSG
365300000000     C                   CALLB     'XRTVM'
365400000000     C                   PARM                    IDMSG            27
365500000000     C                   PARM                    TXTMSG          100
365600000000     C                   PARM                    ERRMSG            1
365700000000     C*
365800000000     C     ERRMSG        IFNE      *ON
365900000000     C                   MOVEL     TXTMSG        $TX1(X)
366000000000     C                   ELSE
366100000000     C                   MOVEL     *ALL'?'       $TX1(X)
366200000000     C                   ENDIF
366300000000     C*
366400000000     C                   ENDIF
366500000000     C*
366600000000     C                   ENDDO
366700000000     C*
366800000000     C                   ENDSR
366900000000     C/EJECT
367000000000     C************************************************************
367100000000     C* REPERIMENTO TASTI DI FUNZIONE
367200000000     C************************************************************
367300000000     C     REPKEY        BEGSR
367400000000     C                   MOVEA     $FM1          $FM
367500000000     C                   MOVEA     $FC1          $FC
367600000000     C                   EXSR      REPKKK
367700000000     C                   MOVEA     $FX           $FX1
367800000000     C*
367900000000     C                   MOVEA     $FM2          $FM
368000000000     C                   MOVEA     $FC2          $FC
368100000000     C                   EXSR      REPKKK
368200000000     C                   MOVEA     $FX           $FX2
368300000000     C*
368400000000     C                   MOVEA     $FM3          $FM
368500000000     C                   MOVEA     $FC3          $FC
368600000000     C                   EXSR      REPKKK
368700000000     C                   MOVEA     $FX           $FX3
368800000000     C                   ENDSR
368900000000     C/EJECT
369000000000     C************************************************************
369100000000     C* REPERIMENTO TASTI DI FUNZIONE
369200000000     C************************************************************
369300000000     C     REPKKK        BEGSR
369400000000     C*
369500000000     C                   CLEAR                   $FX
369600000000     C*
369700000000     C     1             DO        24            X
369800000000     C*
369900000000     C     $FM(X)        IFNE      *BLANK
370000000000     C     $Fc(X)        andne     '0'
370100000000     C*
370200000000     C                   MOVEL     $FM(X)        IDMSG
370300000000     C                   CALLB     'XRTVM'
370400000000     C                   PARM                    IDMSG            27
370500000000     C                   PARM                    TXTMSG          100
370600000000     C                   PARM                    ERRMSG            1
370700000000     C*
370800000000     C     ERRMSG        IFNE      *ON
370900000000     C                   MOVEL     TXTMSG        $FX(X)
371000000000     C                   ELSE
371100000000     C                   MOVEL     *ALL' '       $FX(X)
371200000000     C                   ENDIF
371300000000     C*
371400000000     C                   ENDIF
371500000000     C*
371600000000     C                   ENDDO
371700000000     C*
371800000000     C                   ENDSR
371900000000     C/EJECT
372000000000     C************************************************************
372100000000     C* DEFINIZIONE KLIST
372200000000     C************************************************************
372300000000     C     DefKlist      BEGSR
372400000000     C*
372500000000     C* klist
372600000000     C*
372700000000     C                   ENDSR
372800000000     C/EJECT
372900000000** $MS1 / $TX1
373000000000PROMSG    *LIBL     OPZ0285
373001160129SDGMSG    *LIBL     SDO0053                           *R195*
373100000000** $OP1 / $FL1
37320000000011S      <Dettaglio
37320116012920S      <Verifica Soglia                             *R195*
373300000000** TASTI DI FUNZIONE  D1 : $FM1 / $FX1
373400000000PROMSG    *LIBL     KEY0001  01
373500000000PROMSG    *LIBL     KEY0002  02
373600000000PROMSG    *LIBL     KEY0003  03
373700000000PROMSG    *LIBL     KEY0004  04
373800000000PROMSG    *LIBL     KEY0005  05
373900000000PROMSG    *LIBL     KEY0006  06
374000000000PROMSG    *LIBL     KEY0007  07
374100000000PROMSG    *LIBL     KEY0008  08
374200000000PROMSG    *LIBL     KEY0009  09
374300000000PROMSG    *LIBL     KEY0010  10
374400000000PROMSG    *LIBL     KEY0011  11
374500000000PROMSG    *LIBL     KEY0012  12
374600000000PROMSG    *LIBL     KEY0013  13
374700000000PROMSG    *LIBL     KEY0014  14
374800000000SDGMSG    *LIBL     SDK0003  15
374900000000PROMSG    *LIBL     KEY0016  16
375000000000SDGMSG    *LIBL     SDK0001  17
375100000000PROMSG    *LIBL     KEY0018  18
375200000000PROMSG    *LIBL     KEY0019  19
375300000000PROMSG    *LIBL     KEY0020  20
375400000000PROMSG    *LIBL     KEY0021  21
375500000000PROMSG    *LIBL     KEY0022  22
375600000000PROMSG    *LIBL     KEY0023  23
375700000000PROMSG    *LIBL     KEY0024  24
375800000000**  ABILITAZIONE D1 : $FC1 / $FR1
3759000000000N  01
3760000000001N  02
3761000000001N  03
3762000000001N  04
3763000000000N  05
3764000000000N  06
3765000000000N  07
3766000000001N  08
3767000000000N  09
3768000000000N  10
3769000000000N  11
3770000000001N  12
3771000000000N  13
3772000000000N  14
3773000000000N  15
3774000000000N  16
3775000000000N  17
3776000000000N  18
3777000000000N  19
3778000000000N  20
3779000000000N  21
3780000000000N  22
3781000000000N  23   Impostare a '1' se si utilizzano opzioni
3782000000001N  24   Lasciare sempre impostata a '1' il valore della riga 24
378300000000** TASTI DI FUNZIONE  S1 : $FM2 / $FX2
378400000000PROMSG    *LIBL     KEY0001  01
378500000000PROMSG    *LIBL     KEY0002  02
378600000000PROMSG    *LIBL     KEY0003  03
378700000000PROMSG    *LIBL     KEY0004  04
378800000000PROMSG    *LIBL     KEY0005  05
378900000000PROMSG    *LIBL     KEY0006  06
379000000000PROMSG    *LIBL     KEY0007  07
379100000000PROMSG    *LIBL     KEY0008  08
379200000000PROMSG    *LIBL     KEY0009  09
379300000000PROMSG    *LIBL     KEY0010  10
379400000000PROMSG    *LIBL     KEY0011  11
379500000000PROMSG    *LIBL     KEY0012  12
379600000000PROMSG    *LIBL     KEY0013  13
379700000000PROMSG    *LIBL     KEY0014  14
379800000000PROMSG    *LIBL     KEY0015  15
379900000000PROMSG    *LIBL     KEY0016  16
380000000000SDGMSG    *LIBL     SDK0001  17
380100000000PROMSG    *LIBL     KEY0018  18
380200000000PROMSG    *LIBL     KEY0019  19
380300000000PROMSG    *LIBL     KEY0020  20
380400000000PROMSG    *LIBL     KEY0021  21
380500000000PROMSG    *LIBL     KEY0022  22
380600000000PROMSG    *LIBL     KEY0023  23
380700000000PROMSG    *LIBL     KEY0024  24
380800000000**  ABILITAZIONE S1 : $FC2 / $FR2
3809000000000N  01
3810000000000N  02
3811000000001N  03
3812000000000N  04
3813001309261N  05
3814000000000N  06
3815000000000N  07
3816000000001N  08
3817000000001N  09
3818000000000N  10
3819000000000N  11
3820000000001N  12
3821000000000N  13
3822000000000N  14
3823000000000N  15
3824000000000N  16
3825000000000N  17
3826000000000N  18
3827000000000N  19
3828000000000N  20
3829000000000N  21
3830000000000N  22
3831000000001N  23   Impostare a '1' se si utilizzano opzioni
3832000000001N  24   Lasciare sempre impostata a '1' il valore della riga 24
383300000000** TASTI DI FUNZIONE  D3 : $FM3 / $FX3
383400000000PROMSG    *LIBL     KEY0001  01
383500000000PROMSG    *LIBL     KEY0002  02
383600000000PROMSG    *LIBL     KEY0003  03
383700000000PROMSG    *LIBL     KEY0004  04
383800000000PROMSG    *LIBL     KEY0005  05
383900000000PROMSG    *LIBL     KEY0006  06
384000000000PROMSG    *LIBL     KEY0007  07
384100000000PROMSG    *LIBL     KEY0008  08
384200000000PROMSG    *LIBL     KEY0009  09
384300000000PROMSG    *LIBL     KEY0010  10
384400000000PROMSG    *LIBL     KEY0011  11
384500000000PROMSG    *LIBL     KEY0012  12
384600000000PROMSG    *LIBL     KEY0013  13
384700000000PROMSG    *LIBL     KEY0014  14
384800000000PROMSG    *LIBL     KEY0015  15
384900000000PROMSG    *LIBL     KEY0016  16
385000000000SDGMSG    *LIBL     SDK0001  17
385100000000PROMSG    *LIBL     KEY0018  18
385200000000PROMSG    *LIBL     KEY0019  19
385300000000PROMSG    *LIBL     KEY0020  20
385400000000PROMSG    *LIBL     KEY0021  21
385500000000PROMSG    *LIBL     KEY0022  22
385600000000PROMSG    *LIBL     KEY0023  23
385700000000PROMSG    *LIBL     KEY0024  24
385800000000**  ABILITAZIONE D3 : $FC3 / $FR3
3859000000000N  01
3860000000000N  02
3861000000001N  03
3862000000000N  04
3863000000000N  05
3864000000000N  06
3865000000000N  07
3866000000000N  08
3867000000000N  09
3868000000000N  10
3869000000000N  11
3870000000001N  12
3871000000000N  13
3872000000000N  14
3873000000000N  15
3874000000000N  16
3875000000000N  17
3876000000000N  18
3877000000000N  19
3878000000000N  20
3879000000000N  21
3880000000000N  22
3881000000000N  23   Impostare a '1' se si utilizzano opzioni
3882000000001N  24   Lasciare sempre impostata a '1' il valore della riga 24
