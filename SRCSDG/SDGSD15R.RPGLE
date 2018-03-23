000100090325     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200090325     H*PARMS CVTOPT(*NONE)
000300090325     H DECEDIT('0,') DATEDIT(*DMY.)
000400090325      *
000500160422    > *            GUIDA ANAGRAFICA dati aggiuntivi
000600090930      *           ------------------------------------------------------
000700090325      *
000800090325      *
000900090325      * DOCUMENTI MICROANALISI :
001000090325      *
001100090325      * NOTE TECNICHE :
001200090325      *
001300090325      *
001400090325      * PERSONALIZZAZIONI
001500090325      *
001600090325      *
001700090325      *
001800090325      *               MAPPA INDICATORI
001900090325      *
002000090325      *  21           GENERICO OPERAZIONI I/O
002100090325      *  22           GENERICO ERRORE OPERAZIONI I/O
002200090325      *  30           SFLDSP
002300090325      * N31           SFLCLR
002400090325      *  31           SFLDSPCTL
002500090325      *  32           SFLNXTCHG
002600090325      *  33           SFLEND
002700090325      *  39           OF PRTF
002800090325      *  40 <---> 49  DSPATR ERRORI SU SFL
002900090325      *  50 <---> 89  ERRORI SU VIDEO
003000090325      *  elenco errori speciali:
003100090325      *  90           REGISTRAZIONE GIA' ESISTENTE PER IMMISSIONE O COPIA
003200090325      *  91           OPZIONE NON VALIDA
003300090325      *  92           IMPOSSIBILE RIPOSIZIONAMENTO CON OPZIONI PRESENTI SU LISTA
003400090325      *  93           LA FUNZIONE RICHIAMATA SI E' CHIUSA IN ERRORE
003500090325      *  95           L'OPERAZIONE DI SCRITTURA NON E' ANDATA A BUON FINE
003600090325      *  94           UTENTE NON AUTORIZZATO ALLA FUNZIONE NEL MODO DESIDERATO
003700090325      *  96           ESISTONO ERRORI IN ALTRI SCHERMI
003800090325      *  97           TASTO   NON ABILITATO
003900090325      *  98           RICERCA NON ABIL. NELLA POSIZ.
004000090325      *  99           INDIC. GENERALE DI ERRORE
004100090325     F*----------------------------------------------------*
004200090325    >F*----------------------------------------------------------------
004300090325    >F* Viste logiche utilizzate per i diversi ordinamenti
004400090325    >F*----------------------------------------------------------------
004500160422     F* Dati aggiuntivi anagrafici
004600160422     F* key: Sogg/DtFiVl/Sys/NrAsInt/Dest  UNIQUE
004700160422    >FSDGSO301L IF   E           K DISK
004703160505     F*------------
004704160505     FNDPPA04L  IF   E           K DISK
005700090325     F*------------
005800160422    >FSDGSD15D  CF   E             WORKSTN
005900160422    >F                                     RENAME(SD15D1:FMTD1)
006000160422    >F                                     RENAME(SD15D2:FMTD2)
006100160422    >F                                     RENAME(SD15D3:FMTD3)
006200160422    >F                                     RENAME(SD15S1:FMTS1)
006300160422    >F                                     RENAME(SD15C1:FMTC1)
006301160506    >F                                     RENAME(SD15S2:FMTS2)
006302160506    >F                                     RENAME(SD15C2:FMTC2)
006400160422    >F                                     RENAME(SD15T1:FMTT1)
006500160422    >F                                     RENAME(SD15Z1:FMTZ1)
006600160422    >F                                     RENAME(SD15Z2:FMTZ2)
006700160422    >F                                     RENAME(SD15Z3:FMTZ3)
006800090325     F                                     SFILE(FMTS1:S1NRR)
006801160506     F                                     SFILE(FMTS2:S1NRR)
006900090325     F                                     INFDS(DSFMT)
007000090325     D*----------------------------------------------------*
007100090325     D* Schiere per la gestione delle opzioni
007200090325     D*
007300090325     D* dimensione della schiera $MS1, da non cambiare
007400090325     D $DIMOP          C                   CONST(30)
007500090325     D $MS1            S             27    DIM($dimop) CTDATA PERRCD(1)
007600090325     D $TX1            S             50    DIM($dimop) ALT($MS1)
007700090325     D $OP1            S              2  0 DIM($dimop) CTDATA PERRCD(1)
007800090325     D $FL1            S              1    DIM($dimop) ALT($OP1)
007900090325     D* schiera contenente i nomi dei programma richiamati attraverso
008000090325     D* le opzioni personalizzate
008100090325     D $PGO1           S             10    DIM($dimop)
008200090325     D $TX             S             50    DIM(50)
008300090325     D $FL             S              1    DIM(50)
008400090325     D* Schiere generali per la gestione dei tasti di funzione
008500090325     D $FM             S             27    DIM(24)
008600090325     D $FX             S             50    DIM(24)
008700090325     D $FC             S              1    DIM(24)
008800090325     D $FR             S              1    DIM(24)
008900090325     D* Schiere per la gestione dei tasti di funzione
009000090325     D* Video D1
009100090325     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
009200090325     D $FX1            S             50    DIM(24) ALT($FM1)
009300090325     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
009400090325     D $FR1            S              1    DIM(24) ALT($FC1)
009500090325     D* Schiera contente i nomi dei programmi da richiamare con i tasti
009600090325     D* funzione personalizzata S1
009700090325     D $PGF2           S             10    DIM(25)
009800090325     D* Schiere per la gestione dei tasti di funzione
009900090325     D* Video S1
010000090325     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
010100090325     D $FX2            S             50    DIM(24) ALT($FM2)
010200090325     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
010300090325     D $FR2            S              1    DIM(24) ALT($FC2)
010400090325     D* Schiere per la gestione dei tasti di funzione
010500090325     D* Video D2
010600090325     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
010700090325     D $FX3            S             50    DIM(24) ALT($FM3)
010800090325     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
010900090325     D $FR3            S              1    DIM(24) ALT($FC3)
011000090325     D*
011100090325     D $D1L01          S              3  0
011200090325     D $D1L02          S              3  0
011300090325     D $S1L01          S              3  0
011400090325     D $S1L02          S              3  0
011500090325     D $C1Op1          S              3  0
011600090325     D $C1Op2          S              3  0
011700090325     D $D2L01          S              3  0
011800090325     D $D2L02          S              3  0
011900090325     D*
012000090325     D*-------------
012100090325     D* Indice della schiera tasti funzione
012200090325     D PosFun          S              2  0
012300090325     D/EJECT
012400090325     D*----------------------------------------------------*
012500090325     D*-------------
012600090325     D* Valori assunti
012700090325     D XMPEDS        E DS                  INZ
012800090325     D XMPEDS1       E DS                  INZ
012900090325     D DSMPE           DS                  INZ
013000090325     D  $LOP                   1      1  0
013100090325     D  $ORD                   2      3  0
014900090325     D* Passaggio Parametri
015000090325     D KPJBA         E DS
015100090325     D*-------------
015200090325     D* Reperimento Autorizzazioni
015300090325     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
015400090325     D*-------------
015500090325     D* Passaggio Autorizzazioni
015600090325     D DSAUT           DS           512
015700090325     D*-------------
015800090325     D* Maschera dati autorizzazioni
015801160422     D ANGSOGDS      E DS                  INZ
016000090325     D*-------------
016100090930     D* Dati di ambiente ottenuti da SDGSOCDR
016200090930     D SOC001        E DS                  EXTNAME(SDGSOCDS )
016300090930     D*-------------
016400090930     D* DS Interna per dati di output di SDGSOCDR
016500090325     D XSOCDS          DS          1000
016501160426     D*-------------
016502160426     D* Richiamo a SDGTABDR
016503160426     D SDGTABDS      E DS                  inz
016504160426     D*-------------
016505160426     D* Parametri per ricerca codice ANATB00F
016506160426     D XTabDS        E DS                  inz
016600090325     D*-------------
016700090325     D* Parametri in ricezione
016800160422    >D SDGSD15DS     E DS
016900090325     D*-------------
017000160422     D* Parametri del PGM di manutenzione SDGSD16R
017100160505    >D SDGSD16DS     E DS                  INZ
017200160422    >D  RETD16       E                     INZ('0')
017300160422    >D  OPRD16       E                     INZ('0')
017400160422    >D  ERRD16       E                     INZ('0')
017500160422    >D  CMTD16       E                     INZ('0')
017501160505     D*-------------
017502160505     D* Ricerca capoconti
017503160505     D ANA200DS      E DS                  INZ
017504160505     D*-------------
017505160505     D* Ricerca sottoconti
017506160505     D ANA201DS      E DS                  INZ
017507160505     D*-------------
017508160505     D* Tabella contabilità
017509160505     D ANGG84DS      E DS                  INZ
017510160505     D*-------------
017511160505     D* Contollo autorizzazione contabilità
017512160505     D XAUTCTBDS     E DS                  INZ
017519160509     D*-------------
017520160509     D* Ricerca partite
017521160509     D NDC085DS      E DS                   inz
017522160509     D*-------------
017523160509     D* x ricerca su partite
017524160509     D $DSAUT          DS           512
017525160509     D*-------------
017526160509     D* Schiera partite in ricerca
017527160509     D SkPartite       DS                  Occurs(50)
017528160509     D  SkDtPar                            Like(PPADtPar)
017529160509     D  SkNrPar                            Like(PPANrPar)
017530160509     D  SkSerPar                           Like(PPASerPar)
017531160510     D  SkImportD                    19S 3
017532160510     D  SkDtSca                            like(dtpar085)
017533160510     D  SkSegnoD                      1
017534160510     D  SkImporto                    19S 3
017535160510     D  SkDivisa                           like(Divisa085)
018900090402     D*-------------
019000090325     D* campo di salvataggio per la DS d'ingresso durante la chiamata
019100090325     D* alla funzione personalizzata
019200160422    >D $SaveDS         S                   like(SDGSD15DS)
019300090325     D*-------------
019400090325     D*- Ds per ricerca legende
019500090325     D ANA964DS      E DS                  INZ
019501160422     D*-------------
019600090325     D*Controlli validità per valore di una legenda
019700090325     D X07VALDS      E DS                  Inz
019701160505     D*-------------
019702160505     D* Controllo conti
019703160505     D ND002DS       E DS
019704160505     D*-------------
019705160505     D* Definizioni parametri standard chiamate moduli
019706160505     D$Esito           S              1
019707160505     D$GesMsg          S              1
019708160505     D$LenOut          S              9B 0
019709160505     D$Struttura       S             10
020400090325     D*-------------
020500090422     D* DS Esterna gestione messaggi
020600091020     D SDGMSGDS      E DS
020700090422     D  STS          E                     EXTFLD(MSGSTS)
020800090422     D                                     DIM(333)
020900090422     D  JBA          E                     EXTFLD(MSGJBA)
021000090422     D                                     DIM(502)
021001160426     D*---------
021002160426     D* tabella destinazione dati aggiuntivi
021003160426     D SDGHC4DS      E DS                  INZ
021004160426     D*---------
021005160426     D* ricerca soggetti
021006160426     D ANA040DS      E DS                  INZ
021007160422     D*---------
021008160429     D* Ricerca dati fiscali storicizzati
021009160429     D ANA057DS      E DS                  INZ
021010160429     D*---------
021011160422     D* Dati soggetti
021012160422     D DVASog        E DS
021013160422     D*---------
021014160422     D* driver soggetti
021015160422     D DVARic          S              1
021016160422     D DVASocieta      S              3
021017160422     D DVAUnita        S              8
021018160422     D DVADtRif        S               D
021019160422     D DVAStrutt       S             10
021020160422     D DVALenOut       S              9B 0
021021160422     D DVASnatura      S              1
021022160422     D DVACodice       S              8
021023160422     D DVALineav       S              3
021024160422     D DVAFiliale      S              3  0
021025160422     D DVASogg         S              8
021026160422     D DVATpInd        S              2
021027160422     D DVACdInd        S              3
021028160422     D DVAOutput       S           4000
021029160422     D DVAErrore       S              1
021100090422     D*-------------
021200090325     D DSFMT           DS           512
021300090325     D  $TASTO               369    369
021400090325     D  NRG                  370    370
021500090325     D  NCL                  371    371
021600090325      * riga cursore window
021700090325     D  NRgWin               382    382
021800090325      * colonna cursore window
021900090325     D  NClWin               383    383
022000090325     D  SFLNRR               378    379B 0
022100090325     D*-------------
022200090325     D* posizione cursore
022300090325     D CURSOR          DS
022400090325     D  RIRI                   1      2B 0 INZ
022500090325     D  R$$                    2      2
022600090325     D  COCO                   3      4B 0 INZ
022700090325     D  C$$                    4      4
022800090325     D*-------------
022900090325     D* Reperimento nome PGM
023000090325     D STATUS         SDS           333
023100090325     D  DSPGM            *PROC
023200090325     D  PARMS            *PARMS
023300090325     D*-------------
023400090325     D* Costanti
023500090325     D* nr.di righe del sfl (SFLPAG)
023600160509    >D SFLPAG          C                   CONST(05)
023700090325     D*-------------
023800090325     D* Tasti di funzione
023900090325     D F01             C                   CONST(X'31')
024000090325     D F02             C                   CONST(X'32')
024100090325     D F03             C                   CONST(X'33')
024200090325     D F04             C                   CONST(X'34')
024300090325     D F05             C                   CONST(X'35')
024400090325     D F06             C                   CONST(X'36')
024500090325     D F07             C                   CONST(X'37')
024600090325     D F08             C                   CONST(X'38')
024700090325     D F09             C                   CONST(X'39')
024800090325     D F10             C                   CONST(X'3A')
024900090325     D F11             C                   CONST(X'3B')
025000090325     D F12             C                   CONST(X'3C')
025100090325     D F13             C                   CONST(X'B1')
025200090325     D F14             C                   CONST(X'B2')
025300090325     D F15             C                   CONST(X'B3')
025400090325     D F16             C                   CONST(X'B4')
025500090325     D F17             C                   CONST(X'B5')
025600090325     D F18             C                   CONST(X'B6')
025700090325     D F19             C                   CONST(X'B7')
025800090325     D F20             C                   CONST(X'B8')
025900090325     D F21             C                   CONST(X'B9')
026000090325     D F22             C                   CONST(X'BA')
026100090325     D F23             C                   CONST(X'BB')
026200090325     D F24             C                   CONST(X'BC')
026300090325     D ENTER           C                   CONST(X'F1')
026400090325     D ROLDWN          C                   CONST(X'F4')
026500090325     D ROLLUP          C                   CONST(X'F5')
026600090325     D*-------------
026700090325     D* Attributi video
026800090325     D Normale         C                   Const(X'32')
026900090325     D Protetto        C                   Const(X'A0')
027000090325     D*-------------
027100090325     D* Sottotitolo di testata
027101160503     D $Sottit         S             27A
027102160503     D SOTTIT1         C                   CONST('PROMSG    *LIBL-
027103160503     D                                          COS2504')
027104160503     D SOTTIT2         C                   CONST('PROMSG    *LIBL-
027105160503     D                                          COS0095')
027106160503     D SOTTIT3         C                   CONST('PROMSG    *LIBL-
027107160503     D                                          COS0440')
027300090930     D SOTTITD1        C                   CONST('SDGMSG    *LIBL-
027400090930     D                                          SDC0001')
027500090325     D*-------------
027600090325     D* Variabili per gestione videate
027700090325     D $Gest           S              2A
027800090325     D $Fine           S              1A
027900090325     D $InzD1          S              1A
028000090325     D $InzS1          S              1A
028100090325     D $InzD2          S              1A
028200090325     D $LastG          S              2A
028300090325     D $LastV          S              2A
028400090325     D $EndFile        S              1A
028500090325     D $Esci           S              1A
028600090325     D $RcdOK          S              1A
028700090325     D $Unique         S              1A
028800090325     D*-------------
028900090325     D* Variabili appoggio sempre presenti in tutte le anagrafiche
029000090325     D S1Nrr           S                   like(C1Rcd)
029100090325     D WSfl            S                   like(C1Nrr)
029200090325     D WMax            S                   like(C1Rcd)
029300090325     D WPag            S                   like(C1Rcd)
029400090325     D WInzS1          S                   like($InzS1)
029500160422    >D WOpr            S                   like(OprD15)
029600090325     D CurR            S              3S 0
029700090325     D CurC            S              3S 0
029800090325     D Win             S             99A
029900090325     D W1Pos           S                   like(C1Pos)
030000090325     D WINKR           S              1A
030100090325     D WExf            S              1A
030200090325     D*-------------
030300090325     D* Variabili appoggio specifiche della singola anagrafica
030400090325     D*  campi per parzializzazioni su rcd (solo alfanumerici)
030500160422    >D Sogg999         S                   like(SoggD15)
030600090325     D*  Indici
030700090325     D X               S              5S 0
030800090325     D Y               S              5S 0
030900090325     D* Indice della schiera per le opzioni personalizzate
031000090325     D PosOpz          S              2  0
031100090325     D* numero max opzioni
031200090325     D NumOpz          S              2  0 Inz($DIMOP)
031300090325     D* numero effettive opzioni
031400090325     D OpzLen          S              2  0
031500090325     D*-------------
031600090326     D* campi di lavoro
031700090930     D $FoundRBS       S              1A
031800090326     D $FoundRBL       S              1A
031801160428     D W1DtFiVl        S               D
031900090422     D CallRicors      S              1A   INZ('0')
032000090422     D*-------------
032100090422     D* campo di lavoro per emissione window di avvertimento
032200160422     D WXMSGInz        S              1A   INZ('1')
032201160422     D*-------------
032202160422     D WSogg           S                   Like(DVASogg)
032203160505     D WData           S               D
032204160505     D WDtPar          S               D
032300090325     C*----------------------------------------------------*
032400090325     C*                MAIN LINE PROGRAM
032500090325     C*----------------------------------------------------*
032600090325     C*
032700090325     C* inizializzazione variabili
032800090325     C                   EXSR      INZVAR
032900090325     C*
033000090325     C     $FINE         DOWEQ     *OFF
033100090325     C     $GEST         CASEQ     'D1'          GESD1                          Parzializz.
033200090325     C     $GEST         CASEQ     'S1'          GESS1                          Lista
033300090325     C     $GEST         CASEQ     'D2'          GESD2                          Valori ass.
033400090325     C                   ENDCS
033500090325     C                   ENDDO
033600090325     C* fine programma
033700090325     C                   EXSR      ENDPGM
033800090325     C*
033900090325     C/EJECT
034000090325     C************************************************************
034100090325     C* FINE PROGRAMMA
034200090325     C************************************************************
034300090325     C     ENDPGM        BEGSR
034400090325     C*
034500090325     C* passaggio dati al pgm chiamante
034600160422    >C                   MOVEL     WOPR          OPRD15                                     ne S1
034700160422    >C                   MOVEL     SDGSD15DS     KPJBU
034800090325     C*
034900090325     C* Se pgm Guida non è stato richiamato posso chiudere in LR
035000160422    >C     OPZD15        IFEQ      *BLANK
035100090325     C                   SETON                                            LR
035200090325     C* altrimenti chiudo in RT
035300090325     C                   ELSE
035400090325     C                   SETON                                            RT
035500090325     C                   ENDIF
035600090325     C                   RETURN
035700090325     C*
035800090325     C                   ENDSR
035900090325     C/EJECT
036000090325     C************************************************************
036100090325     C* RICERCHE
036200090325     C************************************************************
036300090325     C     SEARCH        BEGSR
036400090325     C*
036500090325     C* determino Riga/Colonna del cursore
036600090325     C                   MOVE      NRG           R$$
036700090325     C                   MOVE      NCL           C$$
036800090325     C                   Z-ADD     RIRI          CURR
036900090325     C                   Z-ADD     COCO          CURC
037000090325     C*
037100090325   B1C                   SELECT
037101160429     C*
037102160429    >C* FMTD1 - Tipo di dati
037103160429     C     H1NMFL        WHENEQ    'D1TPDATI  '
037104160429     C                   EXSR      F4_TpDati
037105160505     C*
037106160505    >C* FMTD1 - Destinazione
037107160505     C     H1NMFL        WHENEQ    'D1DEST    '
037108160505     C                   EXSR      F4_Dest
037200090422     C*
037300160422    >C* FMTD1 - Soggetto
037500160422     C     H1NMFL        WHENEQ    'D1SOGG    '
037700160422     C                   EXSR      F4_Sogg
037701160429     C*
037702160429     C* FMTD1 - Data fine validità
037703160429     C     H1NMFL        WHENEQ    'D1DTFIVL'
037704160429     C                   EXSR      F4_DtFiVl
037705160505     C*
037706160505     C* FMTD1 - Contabilità
037707160505     C     H1NMFL        WHENEQ    'D1CTB   '
037708160505     C                   EXSR      F4_Ctb
038500090422     C*
038501160505     C* FMTD1 - Capoconto
038502160505     C     H1NMFL        WHENEQ    'D1KCC   '
038503160505     C                   EXSR      F4_Kcc
038504160505     C*
038505160505     C* FMTD1 - sottoconto
038506160505     C     H1NMFL        WHENEQ    'D1KSC   '
038507160505     C                   EXSR      F4_Ksc
038508160509     C*
038509160509     C* FMTD1- PARTITA
038510160509     C     H1NMFL        WHENEQ    'D1DTPAR'
038511160509     C     H1NMFL        OrEq      'D1NRPAR'
038512160509     C     H1NMFL        OrEq      'D1SERPAR'
038513160509     C*
038517160510     C                   if        D1Kcc      <> *Blank and
038518160510     C                             D1Ksc      <> *Blank
038519160510     C                   EXSR      CtrConto
038520160510     C                   if        *In99       = *Off
038521160509     C                   EXSR      F4_Partita
038522160510     C                   endif
038523160510     C                   else
038524160510     C* Campo obbligatorio
038525160510     C                   SetOn                                        5799
038526160510     C                   endif
038527160505     C*
038600090325     C* ricerca non abilitata in questa posizione
038700090325   X1C                   OTHER
038800090325     C                   SETON                                        9899
038900090325     C*
039000090325   E1C                   ENDSL
039100090325     C*
039200090325     C* imposto pos. del cursore
039300090325     C                   Z-ADD     CURR          H1RIGA
039400090325     C                   Z-ADD     CURC          H1COLO
039500090325     C*
039600090325     C                   MOVE      *HIVAL        $LASTV
039700090326     C*
039800090325     C                   ENDSR
039900090325     C/EJECT
039931160429     C************************************************************
039932160429     C* Ricerca tipo di dati
039933160429     C************************************************************
039934160429     C     F4_TpDati     BEGSR
039935160429     C*
039936160429     C                   Reset                   ANA964DS
039937160429     C*
039938160429     C                   MoveL     '01'          Opz964
039939160429     C                   MoveL     'SDGSD15S'    Trc964
039940160429     C                   MoveL     'TPDATID15'   Cam964
039941160429     C*
039942160429     C                   MoveL     ANA964DS      KPJBU
039943160429     C                   Call      'ANA964R'
039944160429     C                   Parm                    KPJBA
039945160429     C                   MoveL     KPJBU         ANA964DS
039946160429     C*
039947160429     C                   if        Ret964     <> '2'
039948160429     C                   MoveL     Val964        D1TpDati
039949160429     C                   endif
039950160429     C*
039951160429     C                   ENDSR
039952160429      /EJECT
039953160505     C************************************************************
039954160505     C* RICERCA Destinazione anagrafica
039955160505     C************************************************************
039956160505     C     F4_Dest       BEGSR
039957160505     C*
039958160505     C                   RESET                   XTABDS
039959160505     C*
039960160505     C                   MOVE      '01'          XTAOPZ
039961160505     C                   MOVE      XSCSOC        XTAAZI
039962160505     C                   MOVE      XSCLIN        XTALIN
039963160505     C                   Z-ADD     CURR          XTARIG
039964160505     C                   Z-ADD     CURC          XTACOL
039965160505     C                   CLEAR                   KPJBU
039966160505     C                   MOVEL     XTABDS        KPJBU
039967160505     C                   CALL      'SDGHC4R'
039968160505     C                   PARM                    KPJBA
039969160505     C                   MOVEL     KPJBU         XTABDS
039970160505     C*
039971160505     C                   if        XTaErr     <> '1'  and
039972160505     C                             XTaKey     <> *Blank
039973160505     C                   Move      XTaKey        D1Dest
039974160505     C                   endif
039975160505     C*
039976160505     C                   ENDSR
039977160505     C/EJECT
040100090422     C************************************************************
040200160422     C* RICERCA soggetto
040300090422     C************************************************************
040400160422     C     F4_Sogg       BEGSR
040401160422     C*
040500090422     C* controllo chiamata ricorsiva
040600160422     C                   Eval      $Pgm       = 'ANA040R '
040700090422     C                   EXSR      CHEKPGM
040800090422     C*
040900090422     C                   if        CallRicors <> *ON
041000160422     C                   Reset                   ANA040DS
041100160422     C                   Movel     '01'          OPZ040
041300160422     C                   Movel     D1Sogg        Cod040
041400160422     C                   Movel     ANA040DS      KPJBU
041500160422     C                   CALL      'ANA040R '
041600090422     C                   PARM                    KPJBA
041700160422     C                   MOVEL     KPJBU         ANA040DS
041800090422     C*
041900160422     C     Cod040        IFNE      *BLANK
042000160422     C                   Movel     Cod040        D1Sogg
042100090422     C                   Endif
042200090422     C*
042300090422     C                   Endif
042400090422     C*
042500090422     C                   ENDSR
042600090422     C/EJECT
042601160429     C************************************************************
042602160429     C* Ricerca dati fiscali storicizzati
042603160429     C************************************************************
042604160429     C     F4_DtFiVl     BEGSR
042605160429     C*
042606160429     C                   RESET                   ANA057DS
042607160429     C*
042608160429     C                   MOVE      '01'          OPZ057
042613160429     C*
042614160429     C                   Eval      Sog057      = D1Sogg
042615160429     C                   if        D1DtFiVl   <> *Zero
042616160429     C     *Jobrun       move      D1DtFiVl      DtF057
042617160429     C                   endif
042618160429     C*
042619160429     C                   CLEAR                   KPJBU
042620160429     C                   MOVEL     ANA057DS      KPJBU
042621160429     C                   CALL      'ANA057R'
042622160429     C                   PARM                    KPJBA
042623160429     C                   MOVEL     KPJBU         ANA057DS
042624160429     C*
042625160429     C                   if        Sog057     <> *Loval
042626160429     C                   Move      Sog057        D1Sogg
042627160429     C                   endif
042628160429     C                   if        DtF057     <> *Loval
042629160429     C     *JobRun       Move      DtF057        D1DtFiVl
042630160429     C                   endif
042631160509     C                   Eval      W1DtFiVl    = DtF057
042632160429     C*
042633160505     C                   ENDSR
042634160505     C/EJECT
044800090422     C************************************************************
044900090422     C* Controllo chiamata ricorsiva
045000090422     C************************************************************
045100090422     C     CHEKPGM       BEGSR
045200090422     C*
045300090422     C                   Eval      CallRicors  = *Off
045400090422     C*
045500090422     C                   Clear                   FlgSts
045600090422     C                   CALL      'XCHKPGM'                            2121
045700090422     C                   Parm                    $PGM             10
045800090422     C                   Parm      '0'           FlgSts            1
045900090422     C*
046000090422     C                   if        *In21       = *Off  and
046100090422     C                             FlgSts      = *On
046200090422     C*
046300090422     C                   Eval      CallRicors  = *On
046400090422     C* Emetto avviso di pgm ripetuto
046500091020     C                   Clear                   SDGMSGDS
046600090422     C                   EXSR      REPMSG
046700091020     C                   EXSR      CallSDGMSGR
046800090422     C                   endif
046900090422     C*
047000090422     C                   ENDSR
047100090422     C/EJECT
047200090422     C************************************************************
047300090422     C* Reperimento testo messaggi
047400090422     C************************************************************
047500090422     C     RepMsg        BEGSR
047600090422     C*
047700090422     C* "Funzione non richiamabile"
047800090422     C                   Eval      IDMSG = 'PROMSG    *LIBL     PRO0430'
047900090422     C                   EXSR      PrendiMsg
048000090422     C                   if        ERRMSG     <> *ON
048100090422     C                   MoveL     TXTMSG        MSGMS1
048200090422     C                   else
048300090422     C                   MoveL     *ALL'?'       MSGMS1
048400090422     C                   endif
048500090422     C*
048600090422     C                   Eval      MsgMs2      = $Pgm
048700090422     C*
048800090422     C* "Causa chiamata ricorsiva"
048900090422     C                   Eval      IDMSG = 'PROMSG    *LIBL     PRO0449'
049000090422     C                   EXSR      PrendiMsg
049100090422     C                   if        ERRMSG     <> *ON
049200090422     C                   MoveL     TXTMSG        MSGMS3
049300090422     C                   else
049400090422     C                   MoveL     *ALL'?'       MSGMS3
049500090422     C                   endif
049600090422     C*
049700090422     C                   ENDSR
049800090422     C/EJECT
049900090422     C************************************************************
050000090422     C* Reperimento testo messaggi
050100090422     C************************************************************
050200090422     C     PrendiMsg     BEGSR
050300090422     C*
050400090422     C                   CALLB     'XRTVM'
050500090422     C                   PARM                    IDMSG            27
050600090422     C                   PARM                    TXTMSG          100
050700090422     C                   PARM                    ERRMSG            1
050800090422     C*
050900090422     C                   ENDSR
051000090422     C/EJECT
051100090422     C************************************************************
051200090422     C* Avvertimento chiamata ricorsiva
051300090422     C************************************************************
051400091020     C     CallSDGMSGR   BEGSR
051500090422     C*
051600090422     C                   MoveL     'N'           MSGRSP
051700090422     C                   MoveL     *Blank        MSGTPR
051800090422     C                   MoveL     'S'           MSGEMV
051900090422     C                   Z-ADD     10            MSGRGP
052000090422     C                   MoveL     'N'           MSGCNL
052100090422     C                   MOVEL     0             MSGMSG
052200090422     C                   MOVEA     STATUS        STS
052300090422     C                   MoveL     'N'           MSGVID
052400090422     C                   MoveL     'N'           MSGOPE
052500090422     C                   MoveL     'N'           MSGSTP
052600090422     C                   MoveL     'N'           MSGLCK
052700090422     C*
052800091020     C                   CALL      'SDGMSGR'
052900091020     C                   PARM                    SDGMSGDS
053000090422     C                   PARM      *Blank        XMSTxt          135
053100090422     C                   PARM      WXMsgInz      XMSGInz           1
053200090422     C*
053300090422     C                   Move      *Off          WXMsgInz
053400090422     C*
053500090422     C                   ENDSR
053600090422     C/EJECT
053601160505     C************************************************************
053602160505     C* Ricerca contabilità
053603160505     C************************************************************
053604160505     C     F4_Ctb        BEGSR
053605160505     C*
053606160505     C                   RESET                   XTABDS
053607160505     C*
053608160505     C                   MOVE      '01'          XTAOPZ
053609160505     C                   MOVE      XSCSOC        XTAAZI
053610160505     C                   MOVE      XSCLIN        XTALIN
053611160505     C                   Z-ADD     CURR          XTARIG
053612160505     C                   Z-ADD     CURC          XTACOL
053613160505     C                   CLEAR                   KPJBU
053614160505     C                   MOVEL     XTABDS        KPJBU
053615160505     C                   CALL      'ANGG84R'
053616160505     C                   PARM                    KPJBA
053617160505     C                   MOVEL     KPJBU         XTABDS
053618160505     C*
053619160505     C                   if        XTaErr     <> '1'  and
053620160505     C                             XTaKey     <> *Blank
053621160505     C                   Move      XTaKey        D1Ctb
053622160505     C                   endif
053623160505     C*
053624160505     C                   ENDSR
053625160505     C/EJECT
053626160505     C************************************************************
053627160505     C* Ricerca capoconto
053628160505     C************************************************************
053629160505     C     F4_Kcc        BEGSR
053630160505     C*
053631160505     C                   RESET                   ANA200DS
053632160505     C*
053633160505     C                   MOVEL     '01'          OPZ200
053634160505     C                   MOVE      D1KCC         KCC200
053635160505     C*
053636160505     C                   MOVEL     ANA200DS      KPJBU
053637160505     C*
053638160505     C                   CALL      'ANA200R'
053639160505     C                   PARM                    KPJBA
053640160505     C*
053641160505     C                   MOVEL     KPJBU         ANA200DS
053642160505     C*
053643160505     C     KCC200        IFNE      *BLANK
053644160505     C                   MOVEL     KCC200        D1KCC
053645160505     C                   ENDIF
053646160505     C*
053647160505     C* imposto pos. del cursore
053648160505     C                   Z-ADD     CURR          H1RIGA
053649160505     C                   Z-ADD     CURC          H1COLO
053650160505     C*
053651160505     C                   ENDSR
053652160505     C/EJECT
053700090325     C************************************************************
053800160505     C* Ricerca sottoconto
053900090325     C************************************************************
054000160505     C     F4_Ksc        BEGSR
054100090325     C*
054101160505     C                   RESET                   ANA201DS
054102160505     C*
054103160505     C                   MOVE      XSCSOC        SOC201
054104160505     C                   MOVE      D1KCC         KCC201
054105160505     C                   MOVE      D1KsC         KSC201
054106160505     C*
054107160505     C                   MOVEL     ANA201DS      KPJBU
054108160505     C                   CALL      'ANA201R'
054109160505     C                   PARM                    KPJBA
054110160505     C                   MOVEL     KPJBU         ANA201DS
054111160505     C*
054112160505     C     KSC201        IFNE      *BLANK
054113160505     C                   MOVEL     KCC201        D1KCC
054114160505     C                   MOVEL     KSC201        D1KSC
054115160505     C                   ENDIF
054116160505     C*
054117160505     C* imposto pos. del cursore
054118160505     C                   Z-ADD     CURR          H1RIGA
054119160505     C                   Z-ADD     CURC          H1COLO
054120160505     C*
054121160505     C                   ENDSR
054122160505     C/EJECT
054123160509     C************************************************************
054124160509     C* GESTIONE F4 su partita
054125160509     C************************************************************
054126160509     C     F4_Partita    BEGSR
054127160509     C*
054128160509     C                   Clear                   NDC085DS
054129160509     C*
054130160509     C                   MOVEL     '01'          OPZ085
054139160509     C                   MOVE      D1Ctb         Ctb085
054140160513     C                   MOVE      *Blank        Unita085
054141160509     C*
054142160509     C                   if        D1Kcc      <> 'C'     and
054143160509     C                             D1Kcc      <> 'F'
054144160509     C*
054145160509     C                   CALLB     'XALLINEA'
054146160509     C                   PARM                    D1Kcc
054147160509     C                   PARM      6             LEN               5 0
054148160509     C                   PARM                    XSCINP
054149160509     C                   endif
054150160509     C*
054151160509     C                   CALLB     'XALLINEA'
054152160509     C                   PARM                    D1Ksc
054153160509     C                   PARM      8             LEN               5 0
054154160509     C                   PARM                    XSCINP
054155160509     C*
054156160509     C* reperisco capoconto se cliente/fornitore
054157160509     C                   if        D1Kcc       = 'C       '     or
054158160509     C                             D1Kcc       = 'F       '
054159160509     C                   CALLB     'XCAPCLIFOR'
054160160509     C                   PARM      XScSoc        XCAPSOCIET        3
054161160509     C     D1Kcc         PARM      D1Kcc         XCAPKCC           6
054162160509     C     D1Ksc         PARM      D1Ksc         XCAPKSC           8
054163160509     C                   endif
054164160509     C*
054165160509     C                   MOVE      D1Kcc         Kcc085
054166160509     C                   MOVE      D1Ksc         Ksc085
054167160510     C* ..Mod.interr.E/C: 1=x partita
054168160509     C                   Eval      Modalit085 =  '1'
054169160510     C* ..Movimenti Defin./Provvis./Gestionali = Si
054170160509     C                   Eval      MovDef085  =  *On
054171160509     C                   Eval      MovProv085 =  *On
054172160509     C                   Eval      MovGes085  =  *On
054173160510     C* ..Ordine: Ascendente per scegliere prima le partite + vecchie
054174160509     C                   Eval      Ord085     =  'A'
054175160510     C* ..Visualizzo le partite in divisa effettiva
054176160509     C                   Eval      DivVis085  =  '1'
054177160510     C* ..Includo la divisa della proposta
054178160509     C                   Eval      DivSel085  =  'I'
054179160509     C**********         Eval      Divisa085  =  D1Divisa
054180160510     C* ..Interrogazione multipla: No
054181160509     C                   Eval      EstMul085  =  *Off
054182160510     C* ..Situaz. partite: 1=Tutte le partite
054183160510     C                   Eval      SitPar085  =  '1'
054184160510     C* ..Ripresa Saldo: No
054185160509     C                   Eval      RipSald085 =  *Off
054186160510     C* ..Selezione multipla : Si
054187160510     C                   Eval      Multip085  =  *On
054188160510     C*
054189160510     C* ..Pulisco la ds multipla prima di chiamare
054190160510     C                   Do        50            $X                4 0
054191160510     C     $x            Occur     SkPartite
054192160509     C                   CLEAR                   SkPartite
054193160509     C                   EndDo
054194160510     C*
054195160509     C                   CLEAR                   $DSAUT
054196160509     C                   MOVEL     NDC085DS      KPJBU
054197160510     C*
054198160509     C                   CALL      'NDC085R'
054199160509     C                   PARM                    KPJBA
054200160509     C                   PARM                    $DSAUT
054201160509     C                   PARM                    SkPartite
054202160510     C*
054203160509     C                   MOVEL     KPJBU         NDC085DS
054204160510     C*
054205160510     C* gestione ritorno dall'F4 su partita
054206160509     C                   EXSR      RTNF4Par
054207160510     C*
054208160509     C                   ENDSR
054209160509     C/EJECT
054210160509     C************************************************************
054211160509     C* Ritorno dall'F4 su partite
054212160509     C************************************************************
054213160509     C     RTNF4Par      BEGSR
054214160510     C*
054215160510     C* Ciclo per trovare almeno una partita...
054216160509     C                   Do        1             $X
054217160509     C     $X            Occur     SkPartite
054218160510     C*
054219160510     C* Se c'è almeno una partita...
054220160509     C     SkNrPar       IfNe      0
054221160510     C*
054222160510     C* ..valorizzo rif. partita
054223160509     C     SkDtPar       IfNe      *Loval
054224160509     C     *JobRun       Move      SkDtPar       D1DtPar
054225160509     C                   EndIf
054226160509     C                   Eval      D1NrPar    =  SkNrPar
054227160509     C                   Eval      D1SerPar   =  SkSerPar
054261160510     C*
054262160509     C                   Leave
054263160509     C                   EndIf
054264160510     C*
054265160509     C                   EndDo
054268160510     C*
054269160509     C                   ENDSR
054270160509     C/EJECT
054271160505     C************************************************************
054272160505     C* RIEMPIMENTO OPZIONI
054273160505     C************************************************************
054274160505     C     RIEOPZ        BEGSR
054275160505     C*
054276090325     C* Input :
054300090325     C* - $TX = schiera con tutti i testi disponibili
054400090325     C* - $FL = schiere con i flag di validità associati ai testi
054500090325     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
054600090325     C*          opzione usato
054700090325     C* - WMXO = variabile contenente il numero massimo di testi
054800090325     C*          opzione usabili
054900090325     C* - $L01 = Lunghezza prima riga
055000090325     C* - $L02 = Lunghezza seconda riga
055100090325     C* Output :
055200090325     C* - WOPZ1 = variabile contenente la prima riga opzioni
055300090325     C* - WOPZ2 = variabile contenente la seconda riga opzioni
055400090325     C* - FF23  = flag che indica se c'è bisogno del tasto F23
055500090325     C*
055600090325     C                   CALLB     'X04OPMSG'
055700090325     C* input
055800090325     C                   PARM                    $TX
055900090325     C                   PARM                    $FL
056000090325     C                   PARM                    WCOZ              3 0
056100090325     C                   PARM                    WMXO              3 0
056200090325     C* output
056300090325     C                   PARM                    WOPZ1            76
056400090325     C                   PARM                    WOPZ2            76
056500090325     C                   PARM                    FF23              1
056600090325     C* input
056700090325     C                   PARM                    $L01              3 0
056800090325     C                   PARM                    $L02              3 0
056900090325     C*
057000090325     C                   PARM                    $ALLOPT         960
057100090325     C*
057200090325     C                   ENDSR
057300090325     C/EJECT
057400090325     C************************************************************
057500090325     C* RIEMPIMENTO TASTI FUNZIONE
057600090325     C************************************************************
057700090325     C     RIEKEY        BEGSR
057800090325     C*
057900090325     C* Input :
058000090325     C* - $FX = schiera con tutti i testi disponibili
058100090325     C* - $FC = schiere con i flag di validità associati ai testi
058200090325     C* - $FR = schiere con i flag di REVERSE IMAGE
058300090325     C* - $ULK = ultimo testo caricato
058400090325     C* - $L01 = Lunghezza prima riga
058500090325     C* - $L02 = Lunghezza seconda riga
058600090325     C* Output :
058700090325     C* - $ULK
058800090325     C* - $KEY1= 1° riga di tasti funzione
058900090325     C* - $KEY2= 2° riga di tasti funzione
059000090325     C*
059100090325     C                   CALLB     'XKEYMSG'
059200090325     C* input
059300090325     C                   PARM                    $FX
059400090325     C                   PARM                    $FC
059500090325     C                   PARM                    $FR
059600090325     C                   PARM                    $ULK              3 0
059700090325     C* output
059800090325     C                   PARM                    $KEY1            79
059900090325     C                   PARM                    $KEY2            79
060000090325     C* input
060100090325     C                   PARM                    $L01              3 0
060200090325     C                   PARM                    $L02              3 0
060300090325     C*
060400090325     C                   PARM                    $ALLFUNCT       480
060500090325     C*
060600090325     C                   ENDSR
060700090325     C/EJECT
060800090325     C************************************************************
060900090325     C* GESTIONE EMISSIONE TASTI FUNZIONE
061000090325     C************************************************************
061100090325     C     GESPIE        BEGSR
061200090325     C*
061300090325     C                   SELECT
061400090325     C* Parzializzazioni
061500090325     C     $GEST         WHENEQ    'D1'
061600090325     C     $ULKD1        IFEQ      0
061700090325     C* riempimento opzioni
061800090325     C                   EXSR      F24D1
061900090325     C                   ELSE
062000090325     C                   WRITE     FMTZ1
062100090325     C                   ENDIF
062200090325     C* Lista
062300090325     C     $GEST         WHENEQ    'S1'
062400090325     C     $ULKS1        IFEQ      0
062500090325     C                   EXSR      F24S1
062600090325     C                   ELSE
062700090325     C                   WRITE     FMTZ2
062800090325     C                   ENDIF
062900090325     C* Modifica valori assunti
063000090325     C     $GEST         WHENEQ    'D2'
063100090325     C     $ULKD2        IFEQ      0
063200090325     C                   EXSR      F24D2
063300090325     C                   ELSE
063400090325     C                   WRITE     FMTZ3
063500090325     C                   ENDIF
063600090325     C                   ENDSL
063700090325     C*
063800090325     C                   ENDSR
063900090325     C/EJECT
064000090325     C************************************************************
064100090325     C* GESTIONE PARZIALIZZAZIONI
064200090325     C************************************************************
064300090325     C     GESD1         BEGSR
064301160513     C*
064302160513     C                   MOVEL     *BLANK        T1TIT
064303160513     C*
064400090325     C* inizializzazione videata
064500090325     C     $INZD1        IFEQ      *ON
064600090325     C                   EXSR      INZD1
064700090325     C                   MOVE      *OFF          $INZD1
064800090325     C                   ENDIF
064900090325     C* emissione piede videata se proveniente da altra
065000090325     C* salvataggio valore formato di provenienza
065100090325     C     $LASTV        IFNE      'D1'
065200090930     C*
065300090930     C* recupero il sottotitolo (se c'è)
065400090930     C                   Clear                   D1TIT
065500090930     C     SOTTITD1      IFNE      *BLANK
065600090930     C                   MOVEL     SOTTITD1      IDMSG
065700090930     C                   CALLB     'XRTVM'
065800090930     C                   PARM                    IDMSG            27
065900090930     C                   PARM                    TXTMSG          100
066000090930     C                   PARM                    ERRMSG            1
066100090930     C                   PARM      *Off          CTRMSG            1            centratura
066200090930     C                   PARM      15            LENMSG            3 0          lun output.
066300090930     C     ERRMSG        IFNE      *ON
066400090930     C                   MOVEL     TXTMSG        D1TIT
066500090930     C                   ELSE
066600090930     C                   MOVEL     *ALL'?'       D1TIT
066700090930     C                   ENDIF
066800090930     C                   ENDIF
066900090930     C*
067000090325     C* salvo stato F17 per sapere cosa fare in caso di F12
067100090325     C                   MOVE      *INKR         WINKR
067200090325     C                   WRITE     FMTT1
067300090325     C                   EXSR      GESPIE
067400090325     C                   MOVE      $LASTV        $LASTG
067500090325     C                   MOVE      'D1'          $LASTV
067600090325     C                   ENDIF
067700090325     C*
067800090325     C* se esistono errori sulla videata
067900090325     C* emetto la write del formato a indicatori spenti per vedere
068000090325     C* le eventuali decodifiche
068100090325     C     *IN99         IFEQ      *ON
068200090325     C                   MOVEA     *IN           WIN
068300090325     C                   MOVE      *ALL'0'       IN5098           49
068400090325     C                   MOVEA     IN5098        *IN(50)
068500090325     C                   WRITE     FMTD1
068600090325     C                   MOVEA     WIN           *IN
068700090325     C                   ENDIF
068800090325     C*
068900090325     C                   EXFMT     FMTD1
069000090325     C*
069100090325     C* azzero la posizione del cursore
069200090325     C                   Z-ADD     0             H1RIGA
069300090325     C                   Z-ADD     0             H1COLO
069400090325     C*
069500090325     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
069600090325     C* INPUT
069700090325     C* $FC1 = Abilitazioni videata D1
069800090325     C* OUTPUT
069900090325     C* *IN97 + *IN99 Errori
070000090325     C                   MOVEA     $FC1          $FC
070100090325     C                   EXSR      CTRKEY
070200090325     C*
070300090325     C     *IN99         IFEQ      *OFF
070400090325     C*
070500090325     C                   SELECT
070600090325     C* F2=Decodifica
070700090325     C     $TASTO        WHENEQ    F02
070800090325     C                   EXSR      F02D1
070900090325     C* F3=Fine
071000090325     C     $TASTO        WHENEQ    F03
071100090325     C                   EXSR      F03D1
071200090325     C* F4=Ricerca
071300090325     C     $TASTO        WHENEQ    F04
071400090325     C                   EXSR      SEARCH
071500090325     C* F5=Refresh
071600090325     C     $TASTO        WHENEQ    F05
071700090325     C                   EXSR      F05D1
071800090325     C* F8=Successivo
071900090325     C     $TASTO        WHENEQ    F08
072000090325     C                   EXSR      F08D1
072100090325     C* F10=Immissione
072200090325     C     $TASTO        WHENEQ    F10
072300090325     C                   EXSR      F10D1
072400090325     C* F12=Ritorno
072500090325     C     $TASTO        WHENEQ    F12
072600090325     C                   EXSR      F12D1
072700090325     C* F18=Modifica valori assunti
072800090325     C     $TASTO        WHENEQ    F18
072900090325     C                   EXSR      F18D1
073000090325     C* F24=Altri tasti
073100090325     C     $TASTO        WHENEQ    F24
073200090325     C                   EXSR      F24D1
073300090325     C*
073400090325     C                   OTHER
073500090325     C                   EXSR      CTRD1
073600090325     C     *IN99         IFEQ      *OFF
073700090325     C* definisco l'ordinamento
073800090325     C                   EXSR      DEFORD
073900090325     C* richiamo la lista
074000090325     C                   MOVE      'S1'          $GEST
074100090325     C                   MOVE      *ON           $INZS1
074200090325     C                   MOVE      *BLANK        W1POS
074300090325     C                   ENDIF
074400090325     C*
074500090325     C                   ENDSL
074600090325     C*
074700090325     C                   ENDIF
074800090325     C*
074900090325     C                   ENDSR
075000090325     C/EJECT
075100090325     C************************************************************
075200090325     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
075300090325     C************************************************************
075400090325     C     INZD1         BEGSR
075500090325     C*
075600090325     C                   CLEAR                   FMTD1
075611160429     C*
075612160429     C                   Select
075613160429     C*
075614160429     C* Anagrafica soggetti
075615160429     C                   When      TpCallD15   = '1'
075616160429     C                   Eval      *In11       = *On
075617160429     C                   Eval      D1TpDati    = '1'
075618160429     C*
075619160429     C* Dati fiscali storicizzati
075620160429     C                   When      TpCallD15   = '2'
075621160503     C                   Eval      *In11       = *On
075622160429     C                   Eval      *In12       = *On
075623160429     C                   Eval      D1TpDati    = '1'
075624160429     C*
075625160429     C* Estensione nominativi di partita
075626160429     C                   When      TpCallD15   = '3'
075627160429     C                   Eval      *In13       = *On
075628160429     C                   Eval      D1TpDati    = '2'
075629160429     C
075630160429     C                   other
075631160429     C* da menu
075632160429     C                   Eval      *In11       = *On
075633160429     C                   Eval      *In12       = *On
075634160429     C                   Eval      *In13       = *On
075635160429     C
075636160429     C                   endsl
075637160429     C*
075700090325     C* Se pgm  richiamato con parametri forniti , li imposto nelle
075800090325     C*    parzializzazioni
075900090325     C                   SELECT
075901160422     C*
076400090325     C* il pgm è richiamato e
076500090325     C* sono stati passati dei valori di parzializzazione ma non
076600090325     C* la chiave univoca
076700160422    >C     OPZD15        WHENNE    *BLANK
076800090325     C* imposto i valori passati sui campi a video
076801160505    >C                   Move      DestD15       D1Dest
076900160422    >C                   MOVE      SoggD15       D1Sogg
076901160422     C                   if        DtFiVlD15  <> *Loval
077000160422    >C     *Jobrun       MOVE      DtFiVlD15     D1DtFiVl
077001160422     C                   endif
077002160509     C                   Eval      W1DtFiVl    = DtFiVlD15
077004160505    >C                   MoveL     CtbD15        D1Ctb
077005160505    >C                   MoveL     KccD15        D1Kcc
077006160505    >C                   MoveL     KscD15        D1Ksc
077007160505     C                   if        DtParD15   <> *Loval
077008160505     C     *JobRun       Move      DtParD15      D1DtPar
077009160505     C                   else
077010160505     C                   Move      *Zeros        D1DtPar
077011160505     C                   ENDIF
077012160505    >C                   Z-Add     NrParD15      D1NrPar
077013160505    >C                   MoveL     SerParD15     D1SerPar
077014160422     C                   Z-Add     SysD15        D1Sys
077015160422     C                   Z-Add     NrAsIntD15    D1NrAsInt
077016160422     C*
077100090422     C* Richiamo decodifica
077200090422     C                   EXSR      F02D1
077201160422     C*
077300090325     C* il pgm non è richiamato
077400160422    >C     OPZD15        WHENEQ    *BLANK
077500090325     C* imposto dei valori di default
077601160505     C                   MOVE      XScDCo        D1Ctb
077602160610     C                   Eval      D1Dest      = '**'
077603160422     C*
077700090325     C                   ENDSL
077800090325     C*
077900090325     C                   ENDSR
078000090325     C/EJECT
078100090325     C************************************************************
078200090325     C* GESTIONE F02 VIDEO D1
078300090325     C************************************************************
078400090325     C     F02D1         BEGSR
078500090325     C*
078600090325     C* per decodificare la videata richiamo la routine dei ctrl
078700090325     C* e poi disattivo gli eventuali messaggi di errore
078800090325     C                   EXSR      CTRD1
078900090325     C*
079000090325     C                   MOVE      *ALL'0'       IN5098
079100090325     C                   MOVEA     IN5098        *IN(50)
079200090325     C*
079300090325     C                   ENDSR
079400090325     C/EJECT
079500090325     C************************************************************
079600090325     C* GESTIONE F03 VIDEO D1
079700090325     C************************************************************
079800090325     C     F03D1         BEGSR
079900090325     C*
080000090325     C                   MOVE      *ON           $FINE
080100160422    >C                   MOVE      '1'           RETD15
080200090325     C* fine programma
080300090325     C                   EXSR      ENDPGM
080400090325     C*
080500090325     C                   ENDSR
080600090325     C/EJECT
080700090325     C************************************************************
080800090325     C* GESTIONE F05 VIDEO D1
080900090325     C************************************************************
081000090325     C     F05D1         BEGSR
081100090325     C*
081200090325     C                   MOVE      *ON           $INZD1
081300090325     C*
081400090325     C                   ENDSR
081500090325     C/EJECT
081600090325     C************************************************************
081700090325     C* GESTIONE F08 VIDEO D1
081800090325     C************************************************************
081900090325     C     F08D1         BEGSR
082000090325     C*
082100090325     C                   MOVE      *ON           $FINE
082200160422    >C                   MOVE      '0'           RETD15
082300090325     C*
082400090325     C                   ENDSR
082500090325     C/EJECT
082600090325     C************************************************************
082700090325     C* GESTIONE F10 VIDEO D1
082800090325     C************************************************************
082900090325     C     F10D1         BEGSR
083000090325     C*
083100160422    >C                   RESET                   SDGSD16DS
083300160422    >C                   MOVEL     '01'          OPZD16
083400160422    >C     CMTD15        IFEQ      '1'
083500160422    >C                   MOVEL     '1'           CMTD16
083600090325     C                   ELSE
083700160422    >C                   MOVEL     '0'           CMTD16
083800090325     C                   ENDIF
083801160503    >C                   Eval      TpCallD16   = TpCallD15
083802160503    >C                   Eval      TpDatiD16   = D1TpDati
083803160503    >C                   Eval      SoggD16     = D1Sogg
083804160429     C                   if        D1DtFiVl   <> *Zero
083805160429    >C     *Jobrun       MOVE      D1DtFiVl      DtFiVlD16
083806160429     C                   endif
083807160513     C                   Eval      CtbD16      = D1Ctb
083808160429     C                   Eval      SysD16      = D1Sys
083809160429     C                   Eval      NrAsIntD16  = D1NrAsInt
083810160510     C                   Eval      KccD16      = D1Kcc
083811160510     C                   Eval      KscD16      = D1Ksc
083812160510     C                   if        D1DtPar    <> *Zero
083813160510     C     *Jobrun       MOVE      D1DtPar       DtParD16
083814160510     C                   endif
083815160510     C                   Eval      NrParD16    = D1NrPar
083816160510     C                   Eval      SerParD16   = D1SerPar
083817160610     C                   if        D1Dest      = '**'
083818160610     C                   Eval      DestD16     = *Blank
083819160610     C                   endif
083820160510     C                   Eval      DestD16     = D1Dest
083900090325     C* passare anche le parzializzazioni ricevute relative alla chiave
084000090325     C* univoca :
084100090325     C*  se ce ne sono, potranno essere usate come dft d'immissione
084200160422    >C                   IF        OPZD15 <> *Blank
084600090325     C                   ENDIF
084700160422    >C                   MOVEL     SDGSD16DS     KPJBU
084800160422    >C                   CALL      'SDGSD16R'
084900090325     C                   PARM                    KPJBA
085000160422    >C                   MOVEL     KPJBU         SDGSD16DS
085100090325     C* ritorno da PGM gestione
085200160422    >C                   EXSR      RTND16
085300090325     C*
085400090325     C                   MOVE      *HIVAL        $LASTV
085500090325     C*
085600090325     C                   ENDSR
085700090325     C/EJECT
085800090325     C************************************************************
085900090325     C* GESTIONE F12 VIDEO D1
086000090325     C************************************************************
086100090325     C     F12D1         BEGSR
086200090325     C*
086300090325     C*  se la 1a videata è la lista oppure sono arrivato qui con F17,
086400090325     C*  per F12 torno alla lista
086500090325     C     $LOP          IFEQ      2
086600090325     C     WINKR         OREQ      *ON
086700090325     C                   MOVE      'S1'          $GEST
086800090930     C                   Eval      $InzD1      = *On
086900090325     C*  altrimenti, per F12 torno a menu
087000090325     C                   ELSE
087100090325     C                   MOVE      *ON           $FINE
087200160422    >C                   MOVE      '2'           RETD15
087300090325     C                   ENDIF
087400090325     C*
087500090325     C                   ENDSR
087600090325     C/EJECT
087700090325     C************************************************************
087800090325     C* GESTIONE F18 VIDEO D1
087900090325     C************************************************************
088000090325     C     F18D1         BEGSR
088100090325     C*
088200090325     C                   MOVE      'D2'          $GEST
088300090325     C                   MOVEL     *ON           $INZD2
088400090325     C*
088500090325     C                   ENDSR
088600090325     C/EJECT
088700090325     C************************************************************
088800090325     C* GESTIONE F24 VIDEO D1
088900090325     C* input
089000090325     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
089100090325     C* $FX1 =Testi tasti funzione
089200090325     C* $FC1 = Abilitazioni
089300090325     C* $FR1 = Inversioni di fondo
089400090325     C************************************************************
089500090325     C     F24D1         BEGSR
089600090325     C*
089700090325     C                   MOVEA     $FX1          $FX
089800090325     C                   MOVEA     $FC1          $FC
089900090325     C                   MOVEA     $FR1          $FR
090000090325     C                   Z-ADD     $ULKD1        $ULK
090100090325     C                   Z-ADD     $D1L01        $L01
090200090325     C                   Z-ADD     $D1L02        $L02
090300090325     C                   EXSR      RIEKEY
090400090325     C                   Z-ADD     $ULK          $ULKD1            3 0
090500090325     C                   MOVEA     $FC           $FC1
090600090325     C                   MOVEL     $KEY1         Z1KE1
090700090325     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
090800090325     C                   MOVEL     $KEY2         Z1KE2
090900090325     C                   WRITE     FMTZ1
091000090325     C*
091100090325     C*
091200090325     C                   ENDSR
091300090325     C/EJECT
091400090325     C************************************************************
091500090325     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
091600090325     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
091700090325     C* 93 94 96 97 98
091800090325     C************************************************************
091900090325     C     CTRD1         BEGSR
092000090325     C*
092100090325     C                   SETOFF                                       99
092200090325     C* Controllo chiave parzializzazione
092300090930     C*
092301160429     C* se chiamata da menu, controllo il tipo di dati solo se
092302160429     C* chiamata da menu
092303160429     C                   if        TpCallD15   = *Blank
092400160429     C                   EXSR      CtrTpDati
092403160429     C                   endif
092404160513     C                   EXSR      RepSottit
092405160505     C*
092406160505     C* Destinazione dati aggiuntivi
092407160505    >C                   EXSR      CtrDest
092408160429     C*
092409160620     C                   Clear                   D1SoggD
092410160506     C                   if        D1TpDati    = '1'
092411160429    >C                   EXSR      CtrSogg
092412160505     C                   endif
092413160505     C*
092414160620     C                   Eval      W1DtFiVl    = *Loval
092415160506     C                   if        D1TpDati    = '1'      and
092416160506     C                             (TpCallD15   = *Blank   or
092417160506     C                              TpCallD15   = '2'        )
092418160422    >C                   EXSR      CtrDtFiVl
092419160505     C                   endif
092420160505     C*
092422160620     C                   Clear                   D1CtbD
092423160620     C                   Clear                   D1KscD
092424160620     C                   Eval      WDtPar      = *Loval
092425160506     C                   if        D1TpDati    = '2'
092426160505     C                   EXSR      CtrCtb
092427160505     C                   EXSR      CtrConto
092428160505     C                   EXSR      CtrPartita
092429160505     C                   EXSR      CtrSys_NrAsInt
092430160505     C                   endif
092700090930     C*
092800090325     C                   ENDSR
092900090325     C/EJECT
092901160429     C*****************************************************
092902160429     C* Tipo di dati
092903160429     C*****************************************************
092904160429     C     CtrTpDati     BEGSR
092905160429     C*
092906160429     C                   Clear                   D1TpDatiD
092907160429     C                   Clear                   X07ValDs
092908160429     C*
092913160429     C*
092914160429     C                   Move      '1'           X07Ric
092915160429     C                   Movel     'SDGSD15S'    X07TRc
092916160429     C                   Movel     'TPDATID15 '  X07Cam
092917160429     C                   Movel     D1TpDati      X07VAL
092918160429     C*
092919160429     C                   Callb     'X07VALR'
092920160429     C                   Parm                    X07ValDs
092921160429     C*
092922160429     C     X07ERR        IFEQ      *Off
092923160429     C                   MOVEL     X07DES        D1TpDatiD
092924160429     C                   else
092925160429     C                   SetOn                                        6099
092926160429     C                   endif
092929160429     C*
092930160429     C                   ENDSR
092931160429     C/EJECT
092932160505     C************************************************************
092933160505     C* Controllo destinazione
092934160505     C************************************************************
092935160505     C     CtrDest       BEGSR
092936160505     C*
092937160505     C                   Clear                   D1DestD
092938160505     C*
092939160505     C                   Clear                   SDGHC4DS
092940160505     C*
092941160505     C                   Clear                   SDGTABDS
092948160505     C*
092949160610     C                   if        D1Dest     <> *Blank
092950160610     C*
092951160610     C                   if        D1Dest      = '**'
092954160610     C* "Tutte"
092955160610     C                   Eval      IDMSG = 'PROMSG    *LIBL     COS0426'
092956160610     C                   EXSR      PrendiMsg
092957160610     C                   if        ERRMSG     <> *ON
092958160610     C                   MoveL     TXTMSG        D1DestD
092959160610     C                   else
092960160610     C                   MoveL     *ALL'?'       D1DestD
092961160610     C                   endif
092963160610     C*
092964160610     C                   else
092965160505     C*
092966160505     C                   Eval      XTbRic      = '1'
092967160505     C                   Eval      XTbAzi      = XScSoc
092968160505     C                   Eval      XTbKey      = *Zero
092969160505     C                   Eval      XTbCod      = 'HC4'
092970160505     C                   Move      D1Dest        XTbKey
092971160505     C*
092972160505     C                   CALL      'SDGTABDR'
092973160505     C                   PARM                    SDGTABDS
092974160505     C*
092975160505     C                   if        XTbErr     <> '0'
092976160505     C                   SetOn                                        5299
092977160505     C*
092978160505     C                   else
092979160505     C                   MoveL     XTbUni        SDGHC4DS
092980160505     C                   MoveL     DesHC4        D1DestD
092981160505     C*
092982160505     C                   endif
092983160505     C*
092984160610     C                   endif
092985160610     C*
092986160505     C                   else
092987160610     C* "Generica"
092988160610     C                   Eval      IDMSG = 'PROMSG    *LIBL     COS1529'
092989160505     C                   EXSR      PrendiMsg
092990160505     C                   if        ERRMSG     <> *ON
092991160505     C                   MoveL     TXTMSG        D1DestD
092992160505     C                   else
092993160505     C                   MoveL     *ALL'?'       D1DestD
092994160505     C                   endif
092995160505     C*
092996160505     C                   endif
092997160505     C*
092998160505     C                   ENDSR
092999160505     C/EJECT
093000090415     C************************************************************
093100160422     C* Controllo soggetto
093200090415     C************************************************************
093300160422     C     CtrSogg       BEGSR
093400090415     C*
093500160422     C                   Clear                   D1SoggD
093706160422     C*
093707160422     C                   if        D1Sogg     <> *Blank
093708160422     C*
093709160422     C                   CALLB     'XALLINEA'
093710160422     C                   PARM                    D1Sogg
093711160422     C                   PARM      8             LEN               5 0
093712160422     C                   PARM                    XSCINP
093713160422     C*
093714160428     C                   Eval      WSogg       = D1Sogg
093719160422     C                   MoveL     'DVASOG    '  DVAStrutt
093720160422     C                   Eval      DVALENOUT   = %size(DvaSog)
093721160422     C*
093722160422     C                   EXSR      CallDvaSog
093723160422     C*
093724160422     C                   if        DVAErrore  <> '1'
093725160422     C                   Eval      %subst(DVASOG:1:DVALENOUT)
093726160422     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
093727160422     C*
093728160426     C                   MoveL     DVSDesFil     D1SoggD
093735160422     C                   else
093737160422     C                   Seton                                        5099
093738160422     C                   endif
093739160422     C*
093740160422     C                   endif
093741160429     C*
093742160429     C* Nominativi di partita
093743160429     C                   if        D1TpDati    = '2'
093744160429     C                   if        D1Sogg     <> *Blank
093746160429     C                   SetOn                                        5499
093747160429     C                   endif
093748160429     C                   endif
095200090415     C*
095300090415     C                   ENDSR
095400090415     C/EJECT
095401160422     C************************************************************
095402160422     C* Chiamata al driver dei soggetti
095403160422     C************************************************************
095404160422     C     CallDvaSog    BEGSR
095405160422     C*
095406160422     C                   CALLB     'NDDVASOG'
095407160422     C                   PARM      '1'           DVARic
095408160422     C                   PARM      XScSoc        DVASocieta
095409160422     C                   PARM      *Blank        DVAUnita
095410160422     C                   PARM                    DVAStrutt
095411160422     C                   PARM                    DVADtRif
095412160422     C                   PARM                    DVALenOut
095413160428     C                   PARM      *Blank        DVASNatura
095414160428     C                   PARM      *Blank        DVACodice
095415160422     C                   PARM      *Blank        DVALineaV
095416160422     C                   PARM      *Zero         DVAFiliale
095417160422     C                   PARM      WSogg         DVASogg
095418160428     C                   PARM      *Blank        DVATpInd
095419160428     C                   PARM      *Blank        DVACdInd
095420160422     C                   PARM      *Off          DVAErrore
095421160422     C                   PARM                    DVAOutPut
095422160422     C*
095423160422     C                   ENDSR
095424160422     C/EJECT
095500090325     C************************************************************
095600160426     C* Controllo data fine validità
095700090325     C************************************************************
095800160426     C     CtrDtFiVl     BEGSR
095801160428     C*
095802160428     C                   Eval      W1DtFiVl    = *Loval
095803160428     C*
095804160428     C                   if        D1DtFiVl   <> 0
095805160428     C     *JobRun       Test(d)                 D1DtFiVl               21
095806160428     C*
095807160428     C                   if        *In21       = *Off
095808160428     C     *JobRun       Move      D1DtFiVl      W1DtFiVl
095809160428     C*
095810160428     C                   else
095811160503     C                   SetOn                                        5599
095812160428     C                   endif
095813160428     C*
095814160428     C                   endif
095815160429     C*
095816160429     C* Nominativi di partita
095817160429     C                   if        D1TpDati    = '2'
095818160429     C                   if        D1DtFiVl   <> *Zero
095819160429     C                   SetOn                                        5699
095820160429     C                   endif
095821160429     C                   endif
095901160426     C*
095902160426     C                   ENDSR
095903160426     C/EJECT
095907160505     C************************************************************
095908160505     C* Controllo contabilità
095909160505     C************************************************************
095910160505     C     CtrCtb        BEGSR
095911160620     C*
095912160620     C                   Clear                   D1CtbD
095913160505     C*
095914160505     C                   Clear                   ANGG84DS
095915160505     C*
095916160505     C                   Clear                   SDGTABDS
095919160505     C*
095920160506     C                   if        D1Ctb       = *Blank
095921160509     C*********          if        D1DtPar    <> *Zero
095922160506     C                   SETON                                        6499
095923160509     C*********          endif
095924160506     C*
095925160506     C                   else
095926160505     C                   Eval      XTbRic      = '1'
095927160505     C                   Eval      XTbAzi      = XScSoc
095928160505     C                   Eval      XTbKey      = *Zero
095929160505     C                   Eval      XTbCod      = 'G84'
095930160505     C                   Move      D1Ctb         XTbKey
095931160505     C*
095932160505     C                   CALL      'SDGTABDR'
095933160505     C                   PARM                    SDGTABDS
095934160505     C*
095935160505     C                   if        XTbErr     <> '0'
095936160505     C                   SETON                                        6499
095937160505     C*
095938160505     C                   else
095939160505     C                   MoveL     XTbUni        ANGG84DS
095940160506     C                   MoveL     DsbG84        D1CtbD
095941160505     C*
095942160505     C                   endif
095943160505     C*
095964160505     C*
095965160505     C* controllo se abilitato alla contabilità
095966160505     C*
095967160505     C                   MOVE      KNMUS         XACPRF
095968160505     C                   MOVE      XSCSOC        XACSOC
095969160505     C                   MOVE      D1Ctb         XACCTB
095970160505     C                   MOVE      *BLANK        XACABL
095971160505     C                   CALL      'XAUTCTB'
095972160505     C                   PARM                    XAUTCTBDS
095973160505     C*
095974160505     C     XACABL        IFEQ      *OFF
095975160505    >C                   SETON                                        6599
095976160505     C                   ENDIF
095977160506     C*
095978160506     C                   endif
095979160505     C*
095980160505     C                   ENDSR
095981160505     C/EJECT
096016160505     C************************************************************
096017160505     C* Controllo conto
096018160505     C************************************************************
096019160505     C     CtrConto      BEGSR
096020160505     C*
096021160505     C                   Clear                   D1KscD
096023160505     C*
096024160505     C                   if        D1Kcc      <> 'C'     and
096025160505     C                             D1Kcc      <> 'F'
096026160505     C                   CALLB     'XALLINEA'
096027160505     C                   PARM                    D1Kcc
096028160505     C                   PARM      6             LEN               5 0
096029160505     C                   PARM                    XSCINP
096030160505     C                   endif
096031160505     C*
096032160505     C                   CALLB     'XALLINEA'
096033160505     C                   PARM                    D1Ksc
096034160505     C                   PARM      8             LEN               5 0
096035160505     C                   PARM                    XSCINP
096036160505     C*
096037160505     C* reperisco capoconto se cliente/fornitore
096038160505     C                   if        D1Kcc       = 'C       '     or
096039160505     C                             D1Kcc       = 'F       '
096040160505     C                   CALLB     'XCAPCLIFOR'
096041160505     C                   PARM      XScSoc        XCAPSOCIET        3
096042160505     C     D1Kcc         PARM      D1Kcc         XCAPKCC           6
096043160505     C     D1Ksc         PARM      D1Ksc         XCAPKSC           8
096044160505     C                   endif
096045160505     C*
096046160509     C                   if        D1Ksc      <> *Blank
096047160505     C* Richiamo modulo controllo conto
096048160505     C                   EXSR      CALLMVC002
096049160505     C                   if        $Esito      = *On
096050160505     C                   SETON                                        7099
096052160505     C                   else
096053160505     C                   MoveL     DesBrev002    D1KscD
096055160505     C*
096056160505     C* Controllo se conto gestito a partite
096057160505     C                   if        TpGest002   = '0'
096058160505     C                   SetOn                                        6199
096061160505     C                   endif
096062160505     C*
096063160505     C                   endif
096064160509     C*
096065160509     C                   endif
096066160505     C*
096067160505     C                   ENDSR
096068160505     C/EJECT
096069160505     C************************************************************
096070160505     C* Controllo conto
096071160505     C************************************************************
096072160505     C     CallMvc002    BEGSR
096073160505     C*
096074160505     C                   Clear                   ND002DS
096075160505     C                   EVAL      $LenOut = %Size(ND002DS)
096076160505     C                   CallB     'NDMVC002'
096077160505     C                   PARM                    xscsoc
096078160505     C                   PARM                    D1Kcc
096079160505     C                   PARM                    D1Ksc
096080160505     C                   PARM                    WData
096081160505     C                   PARM      *OFF          $GesMsg
096082160505     C                   PARM      *OFF          $Esito
096083160505     C                   PARM      'ND002DS'     $Struttura
096084160505     C                   PARM                    ND002DS
096085160505     C                   PARM                    $LenOut
096086160505     C*
096087160505     C                   ENDSR
096088160505     C/EJECT
096089160505     C************************************************************
096090160505     C* Controllo partita
096091160505     C************************************************************
096092160505     C     CtrPartita    BEGSR
096093160505     C*
096094160505     C                   Eval      WDtPar      = *Loval
096095160505     C*
096096160505     C                   if        D1DtPar    <> 0
096097160505     C     *JobRun       TEST(D)                 D1DtPar                21
096098160505     C                   if        *In21       = *On
096099160505     C                   SetOn                                        6299
096100160505     C                   else
096101160505     C     *JobRun       Move      D1DtPar       WDtPar
096102160505     C                   endif
096103160505     C                   endif
096104160505     C*
096105160505     C                   ENDSR
096106160505     C/EJECT
096122160505     C************************************************************
096123160505     C* Controllo riferimenti interni di partita
096124160505     C************************************************************
096125160505     C     CtrSys_NrAsIntBEGSR
096126160505     C*
096127160429     C* Soggetti e dati fiscali storicizzati
096128160429     C                   if        D1TpDati    = '1'
096129160429     C                   if        D1Sys      <> 0   or
096130160429     C                             D1NrAsInt  <> 0
096131160429     C                   SetOn                                        5399
096132160429     C                   endif
096133160429     C                   endif
096134160426     C*
096135160426     C                   ENDSR
096136160426     C/EJECT
097800090325     C************************************************************
097900090325     C* DEFINIZIONE DELL'ORDINAMENTO
098000090325     C************************************************************
098100090325     C     DEFORD        BEGSR
098101160426     C*
098102160426     C                   Z-ADD     1             $ORD
098200090325     C*
098300090325     C* definisco l'ordinamento della lista a seconda dei valori
098400090325     C* impostati nel video di parzializzazione
098500090325     C*
098600090325     C                   SELECT
098700160426    >C     D1Sogg        WHENNE    *BLANK
098800090325     C                   Z-ADD     1             $ORD
098900090325     C* imposto l'ordinamento 99 per ottenere in selezione solo il
099000090325     C* rcd con la chiave passata
099100090325     C     $Unique       WHENEQ    *ON
099200090325     C                   Z-ADD     99            $ORD
099300090325     C* nel caso non sia stato digitato alcun valore in parzializzazione
099400090325     C* utilizzerò l'ordinamento precedentemente memorizzato
099500090325     C* (temporaneamente o definitivamente) nei valori assunti
099600090325     C                   OTHER
099700090325     C                   ENDSL
099800090325     C                   ENDSR
099900090325     C/EJECT
100000090325     C************************************************************
100100090325     C* Chiamata programma con tasto personalizzato
100200090325     C************************************************************
100300090325     C     FPerS1        BEGSR
100400090325     C*
100500090325     C* salvo la DS di ingresso
100600160422    >C                   MOVEL     SDGSD15DS     $SaveDS
100700090325     C*
100800160422    >C                   MOVEL     SDGSD15DS     KPJBU
100900090325     C                   CALL      $PGF2(PosFun)                        22
101000090325     C                   PARM                    KPJBA
101100090325     C                   PARM                    NOMPGM
101200090325     C*
101300090325     C* ripristino la DS di ingresso
101400160422    >C                   MOVEL     $SaveDS       SDGSD15DS
101500090325     C*
101600090325     C                   ENDSR
101700090325     C/EJECT
101800090325     C************************************************************
101900090325     C* GESTIONE LISTA
102000090325     C************************************************************
102100090325     C     GESS1         BEGSR
102101160506     C*
102102160509     C*****              EXSR      UltOpz
102103160509     C*****              EXSR      REPKEY
102104160509     C**************     EXSR      F24S1
102105160506     C*
102200090325     C* inizializzazione videata
102300090325     C     $INZS1        IFEQ      *ON
102400090325     C                   EXSR      INZS1
102500090325     C                   MOVE      *OFF          $INZS1
102600090325     C                   ENDIF
102601160506     C*
102700090325     C* emissione piede videata se proveniente da altra
102800090325     C* salvataggio valore formato di provenienza
102900090325     C     $LASTV        IFNE      'S1'
103000090325     C                   WRITE     FMTT1
103100090325     C                   EXSR      GESPIE
103200090325     C                   MOVE      $LASTV        $LASTG
103300090325     C                   MOVE      'S1'          $LASTV
103400090325     C                   ENDIF
103500090325     C*
103600090325     C     WMAX          IFEQ      0
103700090325     C                   WRITE     FMTD3                                        NO RCD
103800090325     C                   ENDIF
103900090325     C* Inizializzazione indice tasti funzione personalizzati
104000090325     C                   Eval      PosFun = 25
104100090325     C*
104201160506     C                   EXSR      EXFMTC1
104300090325     C*
104400090325     C* azzero la posizione del cursore
104500090325     C                   Z-ADD     0             H1RIGA
104600090325     C                   Z-ADD     0             H1COLO
104700090325     C* Serve per avvertire i pgm di sola finestra di eseguire le operazioni
104800090325     C* di refresh dello sfondo
104900090325     C                   MOVE      '1'           WExf
105000090325     C*
105100090325     C     C1NRR         IFNE      0
105200090325     C                   Z-ADD     C1NRR         WSFL
105300090325     C                   ENDIF
105400090325     C                   IF        SFLNRR > 0
105500090325     C                   Z-ADD     SFLNRR        C1RCD
105600090325     C                   ENDIF
105700090325     C* inizializzazione indice opzioni personalizzate
105800090325     C                   Eval      PosOpz = 0
105900090325     C* CONTROLLO ABILITAZIONE TASTI
106000090325     C* INPUT
106100090325     C* $FC2 = Abilitazioni videata S1
106200090325     C* OUTPUT
106300090325     C* *IN97 + *IN99 Errori
106400090325     C                   MOVEA     $FC2          $FC
106500090325     C                   EXSR      CTRKEY
106600090325     C*
106700090325     C     *IN99         IFEQ      *OFF
106800090325     C*
106900090325     C                   SELECT
107000090325     C* F3=Fine
107100090325     C     $TASTO        WHENEQ    F03
107200090325     C                   EXSR      F03S1
107300090325     C* F8=Successivo
107400090325     C     $TASTO        WHENEQ    F08
107500090325     C                   EXSR      F08S1
107600090325     C* F10=Immissione
107700090325     C     $TASTO        WHENEQ    F10
107800090325     C                   EXSR      F10S1
107900090325     C* F12=Ritorno
108000090325     C     $TASTO        WHENEQ    F12
108100090325     C                   EXSR      F12S1
108200090325     C* F17=Parzializzazioni
108300090325     C     $TASTO        WHENEQ    F17
108400090325     C                   EXSR      F17S1
108500090325     C* F18=Modifica valori assunti
108600090325     C     $TASTO        WHENEQ    F18
108700090325     C                   EXSR      F18S1
108800090325     C* F23=Altre opzioni
108900090325     C     $TASTO        WHENEQ    F23
109000090325     C                   EXSR      F23S1
109100090325     C* F24=Altri tasti
109200090325     C     $TASTO        WHENEQ    F24
109300090325     C                   EXSR      F24S1
109400090325     C* ROLL-UP
109500090325     C     $TASTO        WHENEQ    ROLLUP
109600090325     C                   EXSR      ROLS1
109700090325     C* Personalizzata
109800090325     C     $PGF2(PosFun) Whenne    *BLANK
109900090325     C                   EXSR      FperS1
110000090325     C*
110100090325     C                   OTHER
110200090325     C* CONTROLLO DATI
110300090325     C                   EXSR      CTRC1
110400090325     C     *IN99         IFEQ      *OFF
110500090325     C                   EXSR      CTRS1
110600090325     C                   ENDIF
110700090325     C*
110800090325     C                   ENDSL
110900090325     C*
111000090325     C                   ENDIF
111100090325     C*
111200090325     C                   ENDSR
111300090325     C/EJECT
111301160506     C************************************************************
111302160506     C* Ulteriore definizione di opzioni o tasti funzione
111303160506     C************************************************************
111304160506     C     UltOpz        BEGSR
111305160506     C*
111306160506     C                   Select
111307160506     C*
111308160506     C                   When      D1TpDati   = '1'
111309160506     C* disattivo F9=Altri dati
111310160506     C                   Z-ADD     9             X
111311160506     C                   MOVE      '0'           $FC1(X)
111312160506     C                   MOVE      '0'           $FC2(X)
111313160506     C                   MOVE      '0'           $FC3(X)
111314160506     C*
111315160506     C                   When      D1TpDati   = '2'
111316160506     C* attivo F9=Altri dati
111317160506     C                   Z-ADD     9             X
111318160506     C                   MOVE      '1'           $FC1(X)
111319160506     C                   MOVE      '1'           $FC2(X)
111320160506     C                   MOVE      '1'           $FC3(X)
111321160506     C*
111322160506     C                   endSl
111323160506     C*
111324160506     C                   ENDSR
111325160506     C/EJECT
111326160506     C************************************************************
111327160506     C* Mostro la lista
111328160506     C************************************************************
111329160506    >C     ExfmtC1       BEGSR
111330160509     C*
111331160509     C                   Eval      *In09       = C1Mode
111332160506     C*
111333160506     C                   Select
111334160506     C                   When      D1TpDati    = '1'
111337160509     C*
111338160509     C                   if        C1Sogg     <> *Blank
111339160509     C                   Eval      *In15       = *On
111340160509     C                   else
111341160509     C                   Eval      *In15       = *Off
111342160509     C                   endif
111343160509     C*
111344160509     C                   if        C1DtFiVl   <> *Zero
111345160509     C                   Eval      *In10       = *On
111346160509     C                   else
111347160509     C                   Eval      *In10       = *Off
111348160509     C                   endif
111349160509     C*
111350160506     C                   EXFMT     FMTC1
111351160506     C*
111352160509     C*
111353160506     C                   When      D1TpDati    = '2'
111355160509     C*
111356160513     C                   if        C1Ksc      <> *Blank
111357160509     C                   Eval      *In08       = *On
111358160509     C                   else
111359160509     C                   Eval      *In08       = *Off
111360160509     C                   endif
111361160509     C*
111362160506     C                   EXFMT     FMTC2
111363160506     C*
111364160506     C                   endSl
111365160506     C*
111366160506     C                   ENDSR
111367160506     C/EJECT
111400090325     C************************************************************
111500090325     C* INIZIALIZZAZIONE LISTA
111600090325     C************************************************************
111700090325     C     INZS1         BEGSR
111701160422     C*
111703160428     C                   Clear                   FMTC1
111708160429     C*
111800090325     C* pulizia SFL
111900090325     C                   SETOFF                                         3031
112001160506     C                   EXSR      WriteC1
112100090325     C                   SETON                                          31
112101160422     C*
112102160422     C* riempimento campi del sflctl
112103160506     C                   Select
112104160506     C                   When      D1TpDati    = '1'
112105160422     C                   EXSR      RIEC1
112106160506     C                   When      D1TpDati    = '2'
112107160506     C                   EXSR      RIEC2
112108160506     C                   endSl
112200091001     C*
112300091001     C* pulizia del sottotitolo
112400091001     C                   Clear                   D1TIT
112500090325     C*
112600090325     C* riempimento opzioni
112700090325     C                   MOVEA     $TX1          $TX
112800090325     C                   MOVEA     $FL1          $FL
112900090325     C                   Z-ADD     0             WCOZ
113000090325     C                   Z-ADD     0             WMXO
113100090325     C*  equivale al num.di elementi della schiera $TX1
113200090325     C                   Z-add     OpzLen        WMXO
113300090325     C                   Z-ADD     $C1Op1        $L01
113400090325     C                   Z-ADD     $C1Op2        $L02
113500090325     C                   EXSR      RIEOPZ
113600090325     C                   Z-ADD     23            X
113700090325     C                   MOVE      FF23          $FC2(X)
113800090325     C                   MOVEL     WOPZ1         C1OPZ1
113900090325     C                   MOVEL     $ALLOPT       H1ALLOPT
114000090325     C                   MOVEL     WOPZ2         C1OPZ2
114100090325     C                   Z-ADD     WCOZ          C1COZ
114200090325     C*
114300090325     C* CARICAMENTO SFL A PAGINE
114400090325     C*
114500090325     C                   Z-ADD     0             S1NRR
114600090325     C                   Z-ADD     0             WMAX
114700090325     C*
114800090325     C* Posizionamento su file pilota
114900090325     C                   CLEAR                   C1POS
115000090325     C                   EXSR      SETANA
115100090325     C* ripristino i valori del posizionamento
115200090325     C                   MOVE      W1POS         C1POS
115300090325     C*
115400090325     C* Lettura file pilota
115500090325     C                   EXSR      REDANA
115600090325     C*
115700090325     C* se arrivo dalla parzializzazione devo caricare solo la prima pagina
115800090325     C                   IF        $LastV = 'D1'
115900090325     C                   Z-ADD     SFLPAG        WPAG
116000090325     C                   ENDIF
116100090325     C                   EXSR      ROLS1
116200090325     C* imposto il caricamento sfl del numero massimo di righe per pagina
116300090325     C                   Z-ADD     SFLPAG        WPAG
116400090325     C*
116500090325     C                   ENDSR
116600090325     C/EJECT
116601160506     C************************************************************
116602160506     C* Scrittura riga sfl
116603160506     C************************************************************
116604160506     C     WriteC1       BEGSR
116605160506     C*
116606160506     C                   Select
116607160506     C                   When      D1TpDati    = '1'
116611160506     C                   WRITE     FMTC1
116612160506     C*
116613160506     C*
116614160506     C                   When      D1TpDati    = '2'
116624160506     C                   WRITE     FMTC2
116625160506     C*
116626160506     C                   endSl
116627160506     C*
116628160506     C                   ENDSR
116629160506     C/EJECT
116630160422     C************************************************************
116631160422     C* CARICAMENTO i dati in testata della lista
116632160422     C************************************************************
116633160422     C     RIEC1         BEGSR
116634160509     C*
116639160509     C                   if        D1DtFiVl    = *Zero
116640160509     C* visualizzo il sfl troncato (chiuso) se chiamata da
116641160509     C* anagrafica soggetti
116642160509     C                   if        TpCallD15   = '1'
116643160509     C                   Eval      C1Mode      = *On
116644160509     C                   else
116645160509     C* visualizzo il sfl non troncato (aperto) se data fine
116646160509     C* validità non passata dalla prima videata
116647160509     C                   Eval      C1Mode      = *Off
116648160509     C                   endif
116649160509     C                   else
116650160509     C* visualizzo il sfl troncato (chiuso) se data fine
116651160509     C* validità passata dalla prima videata
116652160509     C                   Eval      C1Mode      = *On
116653160509     C                   endif
116654160422     C*
116655160422     C* se il soggetto è valorizzato, significa che lo è anche
116656160422     C* nella prima videata, quindi vanno già bene i dati che
116657160422     C* ho reperito nella prima videata
116658160422     C                   if        D1Sogg     <> *Blank
116659160422     C*
116660160509     C                   Eval      *In15       = *On
116661160422     C*
116662160422     C                   MoveL     D1Sogg        C1Sogg
116663160428     C                   MoveL     D1SoggD       C1SoggD
116664160426     C                   MoveL     DVSIndriz     C1VIA
116665160426     C                   MoveL     DVSLocalit    C1CIT
116666160422     C                   MoveL     DVSCAP        C1CAP
116667160426     C                   MoveL     DVSProv       C1PRV
116668160426     C                   MoveL     DVSStato      C1STA
116669160422     C*
116670160422     C                   else
116671160422     C*
116672160422     C* se non ricevuto il soggetto dalla parzializzazione,
116673160422     C* allora lo si vedrà in riga di sfl
116674160509     C                   Eval      *In15       = *Off
116675160422     C                   endif
116676160503     C*
116677160503     C*
116678160503     C                   if        D1DtFiVl   <> *Zero
116679160503     C*
116680160503     C                   Eval      *In10       = *On
116681160503     C*
116682160503     C                   MoveL     D1DtFiVl      C1DtFiVl
116683160503     C*
116684160503     C                   else
116685160503     C*
116686160503     C* se non ricevuta la data di fine validità dalla parzializzazione,
116687160503     C* allora la si vedrà in riga di sfl
116688160503     C                   Eval      *In10       = *Off
116689160503     C                   endif
116690160422     C*
116691160422     C                   ENDSR
116692160422     C/EJECT
116693160506     C************************************************************
116694160506     C* CARICAMENTO i dati in testata della lista
116695160506     C************************************************************
116696160506     C     RIEC2         BEGSR
116697160509     C*
116698160513     C* visualizzo il sfl non troncato (aperto)
116700160509     C* se conto non passato dalla prima videata
116703160513     C                   if        D1Ksc       = *Blank
116704160509     C                   Eval      C1Mode      = *Off
116705160509     C                   else
116706160509     C                   Eval      C1Mode      = *On
116707160509     C                   endif
116708160506     C*
116709160506     C* se il conto    è valorizzato, significa che lo è anche
116710160506     C* nella prima videata, quindi vanno già bene i dati che
116711160506     C* ho reperito nella prima videata
116712160506     C                   if        D1Ksc      <> *Blank
116713160506     C*
116714160513     C                   EVAL      C1Kcc       = D1Kcc
116715160513     C                   EVAL      C1Ksc       = D1Ksc
116716160513     C                   EVAL      C1Descr     = Descr002
116717160513     C                   EVAL      C1DesAgg    = DesAgg002
116718160506     C*
116719160506     C                   else
116720160506     C*
116721160509     C* se non ricevuto il sottoconto dalla parzializzazione,
116722160506     C* allora lo si vedrà in riga di sfl
116723160506     C                   endif
116724160509     C*
116725160513     C                   EVAL      C1Ctb       = D1Ctb
116726160513     C                   EVAL      C1CtbD      = D1CtbD
116729160506     C*
116730160506     C*
116731160506     C                   if        D1DtPar    <> *Zero
116732160506     C*
116733160506     C                   Eval      *In10       = *On
116736160506     C*
116737160506     C                   else
116738160506     C*
116739160506     C* se non ricevuta la data di fine validità dalla parzializzazione,
116740160506     C* allora la si vedrà in riga di sfl
116741160506     C                   Eval      *In10       = *Off
116742160506     C                   endif
116743160509     C*
116744160509     C*
116745160509     C                   if        D1Ksc      <> *Blank
116746160509     C*
116747160509     C                   Eval      *In08       = *On
116748160509     C*
116749160513     C                   MoveL     D1Ksc         C1Ksc
116750160509     C*
116751160509     C                   else
116752160509     C*
116753160509     C* se non ricevuto sottoconto dalla parzializzazione,
116754160509     C* allora lo si vedrà in riga di sfl
116755160509     C                   Eval      *In08       = *Off
116756160509     C                   endif
116757160506     C*
116758160506     C                   ENDSR
116759160506     C/EJECT
116760090325     C************************************************************
116800090325     C* CARICAMENTO PAGINA LISTA
116900090325     C************************************************************
117000090325     C     ROLS1         BEGSR
117100090325     C*
117200090325     C                   SETOFF                                       32
117300090325     C                   Z-ADD     0             Y
117400090325     C                   Z-ADD     WMAX          S1NRR
117500090325     C*
117600090325     C* Leggo dal file anagrafico per caricare la lista
117700090325     C*
117800090325     C     $EndFile      DOWEQ     *OFF
117900090325     C     Y             ANDLT     WPAG
118000090325     C*
118100090325     C                   EXSR      RIES1
118700090325     C*
118800090325     C                   EXSR      REDANA
118900090325     C*
119000090325     C                   ENDDO
119100090325     C*
119200090325     C                   Z-ADD     S1NRR         WMAX                 30
119300090325     C*
119400090325     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
119500090325     C*
119600090325     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
119700090325     C                   MVR                     RESTO             3 0
119800090325     C     PAGINE        MULT      SFLPAG        C1RCD
119900090325     C     RESTO         IFGT      0
120000090325     C                   ADD       1             C1RCD
120100090325     C                   ELSE
120200090325     C                   SUB       SFLPAG        C1RCD
120300090325     C                   ADD       1             C1RCD
120400090325     C                   ENDIF
120500090325     C*
120600090325     C                   ENDSR
120700090325     C/EJECT
120800090325     C************************************************************
120900090325     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
121000090325     C************************************************************
121100090325     C     SETANA        BEGSR
121200090325     C*
121300160428     C                   MOVEL     D1Sogg        SO3Sogg
121500090325     C*
121600090325     C                   SELECT
121700090325     C*
121800090325     C* Gestione posizionamento
121900090325     C* Se il campo W1POS è > *blank imposto il posizionamento con
122000090325     C* W1POS altrimenti lo imposto con il valore di parametrizzazione
122100090325     C*
122200090325    >C* Ord.1 = chiave ordinamento 1
122300090325     C     $ORD          WHENEQ    1
122400090325     C     W1POS         IFNE      *BLANK
122500160422    >C                   MOVEL     W1POS         SO3Sogg
122600090325     C                   ELSE
122700160422    >C                   MOVEL     D1Sogg        SO3Sogg
122800090325     C                   ENDIF
122900160422    >C     SO3Sogg       SETLL     SDGSO301L
123000090325     C
123100090325     C* Ord.99= Chiave univoca per la POSTA
123200090325     C* (quando viene gestita la chiave univoca NON deve essere gestito
123300090325     C*  il posizionamento C1POS/W1POS)
123400090325     C     $ORD          WHENEQ    99
123500160422    >C                   MOVEL     D1Sogg        SO3Sogg
123501160422    >C                   if        D1DtFiVl   <> 0
123502160422     C     *JobRun       move      D1DtFiVl      SO3DtFiVl
123503160422     C                   endif
123504160422     C                   Z-Add     D1Sys         SO3Sys
123505160422     C                   Z-Add     D1NrAsInt     SO3NrAsInt
123506160610     C                   if        D1Dest      = '**'
123507160610     C                   Eval      SO3Dest     = *Blank
123508160610     C                   else
123509160422     C                   MOVEL     D1Dest        SO3Dest
123510160610     C                   endif
123600160422    >C     K05SO301      SETLL     SDGSO301L
123700090325     C                   ENDSL
123800090325     C*
123900090325     C                   ENDSR
124000090325     C/EJECT
124100090325     C************************************************************
124200090325     C* LETTURA RCD ARCHIVIO PILOTA
124300090325     C************************************************************
124400090325     C     REDANA        BEGSR
124500090325     C*
124600090325     C                   MOVEL     *OFF          $EndFile
124700090325     C                   MOVEL     *OFF          $RCDOK
124800090325     C*
124900090325     C     $EndFile      DOUEQ     *ON
125000090325     C     $RCDOK        OREQ      *ON
125100090325     C*
125200090325     C                   SELECT
125300090325     C*
125400090325     C     $ORD          WHENEQ    1
125413160426     C*
125500160428     C                   READ      SDGSO301L                              21
125600090325     C*
125700090325     C     $ORD          WHENEQ    99
125800090325    >C* nel caso di lettura dati POSTA, basta leggere in READE con la
125900090325    >C* chiave precedentemente riempita nella routine SetAna
126000160426    >C     K05SO301      READE     SDGSO301L                              21
126100090325     C*
126200090325     C                   ENDSL
126300090325     C*
126400090325     C                   MOVE      *IN21         $EndFile
126500090325     C*
126600090325     C     $EndFile      IFEQ      *OFF
126700090325     C                   EXSR      SELANA
126800090325     C                   ENDIF
126900090325     C*
127000090325     C                   ENDDO
127100090325     C*
127200090325     C                   MOVE      $EndFile      *IN33                          SflEnd
127300090325     C*
127400090325     C                   ENDSR
127500090325     C/EJECT
127600090325     C************************************************************
127700090325     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
127800090325     C************************************************************
127900090325     C     CLCPAG        BEGSR
128000090325     C* Input :
128100090325     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
128200090325     C*          cursore
128300090325     C* - SFLPAG = numero rcd per pagina sfl
128400090325     C* Output :
128500090325     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
128600090325     C*
128700090325     C     WSFL          DIV       SFLPAG        PAGINE            4 0
128800090325     C                   MVR                     RESTO             3 0
128900090325     C     RESTO         IFGT      0
129000090325     C                   ADD       1             PAGINE
129100090325     C                   ENDIF
129200090325     C     PAGINE        MULT      SFLPAG        WPAG
129300090325     C*
129400090325     C                   ENDSR
129500090325     C/EJECT
129600090325     C************************************************************
129700090325     C* SELEZIONI SULLA RIGA LETTA
129800090325     C* impostare a *OFF il campo $RCDOK per scartare un record che
129900090325     C* non rientra nei parametri di parzializzazione
130000090325     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
130100090325     C* superiore stabilito nei parametri
130200090325     C*
130300090325     C************************************************************
130400090325     C     SELANA        BEGSR
130500090325     C*
130600090325     C                   MOVEL     *ON           $RCDOK
130700090325     C                   MOVE      *hival        ALL9             15
130800090325     C* creo limite massimo parzializzazioni :
130900090325     C* per ogni valore alfanumerico creo il limite superiore
131000090325     C* per cui caricare, creando una variabile che contiene
131100090325     C* il valore digitato nel campo a video riempito di "9"
131200160422    >C     D1Sogg        CAT       ALL9:0        Sogg999
131300090325     C*
131400090325     C                   SELECT
131500090325     C*
131600090325     C* SELEZIONI RISPETTO AI VALORI RCD
131700090325     C*
131800090325     C* Scartare i record che non rispondono alle selezioni effettuate
131900090325     C* esempio : esclusione degli annullati.
132000090325     C*
132100090325     C*
132200090325     C* SELEZIONI RISPETTO ALLE PARZIALIZZAZIONI
132204160429     C*
132205160429     C* scelto di vedere i dati dei soggetti e/o i dati
132206160429     C* fiscali storicizzati
132207160429     C                   When      D1TpDati    = '1'    and
132208160429    >C                             SO3Sogg     = *Blank
132209160429    >C                   MOVEL     *OFF          $RCDOK
132216160429     C*
132217160429     C* scelto di vedere i dati dei nominativi di partita
132219160429     C                   When      D1TpDati    = '2'    and
132220160429    >C                             SO3Sogg    <> *Blank
132221160429    >C                   MOVEL     *OFF          $RCDOK
132222160503     C*
132223160503     C* chiamata da anagrafica soggetto: vedo solo i dati del
132224160503     C* soggetto, no quelli storicizzati del soggetto
132225160503     C                   When      TpCallD15   = '1'    and
132226160503    >C                             SO3DtFiVl  <> *Loval
132227160503    >C                   MOVEL     *OFF          $RCDOK
132228160503     C*
132230160503     C* chiamata da anagrafica dati fiscali storicizzati:
132231160503     C* vedo solo i dati storicizzati del soggetto
132232160503     C                   When      TpCallD15   = '2'    and
132233160503    >C                             SO3DtFiVl   = *Loval
132234160503    >C                   MOVEL     *OFF          $RCDOK
132235160503     C*
132236160503     C* chiamata da particolarità di partita:
132237160503     C* vedo solo i dati dei nominativi di partita
132238160503     C                   When      TpCallD15   = '3'          and
132239160503    >C                             (SO3Sogg    <> *Blank   or
132240160503    >C                              SO3DtFiVl  <> *Loval     )
132241160503    >C                   MOVEL     *OFF          $RCDOK
132243160429     C*
132244160428    >C     SO3Sogg       WHENLT    D1Sogg
132245160428    >C     SO3Sogg       ORGT      Sogg999
132246160428    >C                   MOVEL     *OFF          $RCDOK
132247160428     C*
132300160422     C                   WHEN      SO3Dest    <> D1Dest    and
132400160610     C                             D1Dest     <> '**'
132500090325     C                   MOVEL     *OFF          $RCDOK
132501160422     C*
132502160422     C                   WHEN      SO3DtFiVl  <> W1DtFiVl  and
132503160422     C                             W1DtFiVl   <> *Loval
132504160422     C                   MOVEL     *OFF          $RCDOK
132505160422     C*
132506160422     C                   WHEN      SO3Sys     <> D1Sys     and
132507160422     C                             D1Sys      <> *Zero
132508160422     C                   MOVEL     *OFF          $RCDOK
132509160422     C*
132510160422     C                   WHEN      SO3NrAsInt <> D1NrAsInt and
132511160422     C                             D1NrAsInt  <> *Zero
132512160422     C                   MOVEL     *OFF          $RCDOK
133000090325     C*
133100090325     C                   ENDSL
133101160505     C*
133102160505     C*
133103160505     C                   if        $RCDOK      = *On    and
133104160505     C* scelto di vedere i dati dei nominativi di partita
133105160505     C                             D1TpDati    = '2'
133106160505     C*
133107160505     C* con il record anagrafico letto, aggancio la partita
133108160505     C                   Eval      PPASys      = SO3Sys
133109160505     C                   Eval      PPANrAsInt  = SO3NrAsInt
133110160505     C     K02PPA04      CHAIN     NDPPA04L                           21
133111160505     C                   if        *In21       = *Off
133112160505     C*
133113160505     C* se trovo la partita, seleziono in base a eventuali parametri
133114160505     C                   Select
133115160505     C*
133116160505     C                   When      D1Ctb      <> *Blanks      and
133117160505     C                             D1Ctb      <> PPACtb
133118160505     C                   Move      *Off          $RcdOk
133119160505     C*
133120160505     C                   When      D1Kcc      <> *Blanks      and
133121160505     C                             D1Kcc      <> PPAKcc
133122160505     C                   Move      *Off          $RcdOk
133123160505     C*
133124160505     C                   When      D1Ksc      <> *Blanks      and
133125160505     C                             D1Ksc      <> PPAKsc
133126160505     C                   Move      *Off          $RcdOk
133131160505     C*
133132160505     C                   When      WDtPar     <> *Loval       and
133133160505     C                             WDtPar     <> PPADtPar
133134160505     C                   Move      *Off          $RcdOk
133135160505     C*
133136160505     C                   When      D1NrPar    <> *Zero        and
133137160505     C                             D1NrPar    <> PPANrPar
133138160505     C                   Move      *Off          $RcdOk
133139160505     C*
133140160505     C                   When      D1SerPar   <> *Zero        and
133141160505     C                             D1SerPar   <> PPASerPar
133142160505     C                   Move      *Off          $RcdOk
133143160505     C*
133144160505     C                   endSl
133147160505     C*
133148160505     C                   else
133149160505     C*
133150160505     C                   Move      *Off          $RcdOk
133151160505     C*
133152160505     C                   endif
133153160505     C*
133154160505     C                   endif
133200090325     C*
133300090325     C* FACILITAZIONE NEGLI ACCESSI
133400090325     C*
133500090325     C* nel caso che uno dei dati letti sia > del limite superiore
133600090325     C* oppure < del limite inferiore è necessario fare dei
133700090325     C* ragionamenti onde evitare altri accessi inutili;
133800090325     C* questi ragionamenti dovranno tenere conto dell'ordinamento
133900090325     C* attualmente vigente, per cui si potranno verificare i seguenti
134000090325     C* casi :
134100090325     C* 1. un campo è > del limite superiore ed è la 1a chiave della
134200090325     C*    klist dell'ordinamento vigente :
134300090325     C*    nessun altro rcd sarà valido per cui bisogna andare a fine
134400090325     C*    file mediante un *HIVAL SETLL;
134500090325     C* 2. un campo è > del limite superiore e NON è la 1a chiave della
134600090325     C*    klist dell'ordinamento vigente :
134700090325     C*    nessun rcd che verrà letto con gli stessi valori per la
134800090325     C*    precedente porzione di klist sarà valido, per cui
134900090325     C*    conviene saltarli facendo una SETGT apunto con la klist che
135000090325     C*    esclude il campo in esame;
135100090325     C* 3. un campo è < del limite inferiore e NON è la 1a chiave della
135200090325     C*    klist dell'ordinamento vigente :
135300090325     C*    bisogna arrivare al più presto al limite inferiore per cui
135400090325     C*    conviene fare una SETLL con una klist avente come ultimo
135500090325     C*    campo quello in esame, lasciando per gli altri campi gli
135600090325     C*    stessi valori letti ma sostituendo per quello in esame
135700090325     C*    il limite inferiore;
135800090325     C* non è possibile che un campo che è anche chiave primaria
135900090325     C* dell'ordinamento vigente sia < del limite inferiore perchè
136000090325     C* nella routine SETANA la setll viene appunto fatta con il
136100090325     C* limite inferiore;
136200090325     C* non si deve prendere in considerazione il caso che un campo
136300090325     C* non presente nella chiave dell'ordinamento vigente sia
136400090325     C* esterno ai limiti perchè non genererebbe migliorie negli
136500090325     C* accessi;
136600090325     C*
136700090325     C                   IF        $RcdOk = *off
136800090325     C*
136900090325     C                   ENDIF
137000090325     C*
137100090325     C                   ENDSR
137200090325     C/EJECT
137300090325     C************************************************************
137400090325     C* RIEMPIMENTO RIGA LISTA
137500090325     C************************************************************
137600090325     C     RIES1         BEGSR
137700090325     C*
137800090325     C                   CLEAR                   FMTS1
137801160506     C                   CLEAR                   FMTS2
137900090325     C*
138000090325     C* riempio i campi del sfl, compresi gli hidden con i valori
138100090325     C* del rcd letto
138101160506     C                   Select
138102160506     C                   When      D1TpDati    = '1'
138103160506     C                   EXSR      RIES1_Sogg
138104160506     C*
138105160506     C                   ADD       1             S1NRR
138106160506     C                   ADD       1             Y
138107160506     C                   WRITE     FMTS1
138108160506     C*
138109160506     C                   When      D1TpDati    = '2'
138110160506     C                   EXSR      RIES2_PPA
138111160506     C*
138112160506     C                   ADD       1             S1NRR
138113160506     C                   ADD       1             Y
138114160506     C                   WRITE     FMTS2
138115160506     C                   endSl
139900090325     C*
140000090330     C                   ENDSR
140100090325     C/EJECT
140101160506     C************************************************************
140102160506     C* RIEMPIMENTO RIGA LISTA
140103160506     C************************************************************
140104160506     C     RIES1_Sogg    BEGSR
140105160506     C*
140106160506    >C                   MOVEL     SO3Sogg       S1Sogg
140107160506     C                   if        SO3DtFiVl  <> *Loval
140108160506     C     *Jobrun       Move      SO3DtFiVl     S1DtFiVl
140109160506     C                   endif
140110160506     C*
140111160610     C                   MOVEL     SO3Dest       S1Dest
140112160506     C                   if        SO3Dest    <> *Blank
140113160506     C*
140114160506     C                   Clear                   SDGHC4DS
140115160506     C*
140116160506     C                   Clear                   SDGTABDS
140117160506     C*
140118160506     C                   Eval      XTbRic      = '1'
140119160506     C                   Eval      XTbAzi      = XScSoc
140120160506     C                   Eval      XTbKey      = *Zero
140121160506     C                   Eval      XTbCod      = 'HC4'
140122160610     C                   Move      S1Dest        XTbKey
140123160506     C*
140124160506     C                   CALL      'SDGTABDR'
140125160506     C                   PARM                    SDGTABDS
140126160506     C*
140127160506     C                   if        XTbErr     <> '0'
140128160506     C                   MoveL     *ALL'?'       S1DestD
140129160506     C*
140130160506     C                   else
140131160506     C                   MoveL     XTbUni        SDGHC4DS
140132160506     C                   MoveL     DesHC4        S1DestD
140133160506     C*
140134160506     C                   endif
140135160506     C*
140136160506     C                   else
140137160506     C*
140138160506     C* "Generica"
140139160506     C                   Eval      IDMSG = 'PROMSG    *LIBL     COS1529'
140140160506     C                   EXSR      PrendiMsg
140141160506     C                   if        ERRMSG     <> *ON
140142160506     C                   MoveL     TXTMSG        S1DestD
140143160506     C                   else
140144160506     C                   MoveL     *ALL'?'       S1DestD
140145160506     C                   endif
140146160506     C*
140147160506     C                   endif
140148160506     C*
140149160506     C                   MOVEL     SO3Nome       S1Nome
140150160506     C                   MOVEL     SO3CogRag     S1CogRag
140151160513    >C                   Z-Add     SO3Sys        S1Sys
140152160513    >C                   Z-Add     SO3NrAsInt    S1NrAsInt
140153160506     C*
140154160506     C* imposto quegli attributi del sfl che sono modificabili
140155160506     C* al richiamo del determinato pgm
140156160506     C                   EXSR      ATRS1
140157160506     C*
140158160506     C                   ENDSR
140159160506     C/EJECT
142900090325     C************************************************************
143000090325     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
143100090325     C************************************************************
143200090325     C     ATRS1         BEGSR
143300090325     C*
143400090325     C* in questa routine bisogna attivare indicatori o campi programma
143500090325     C* che gestiscono gli attributi di uno o più campi di questa videata;
143600090325     C* attenzione : non è questo il posto in cui eseguire controlli,
143700090325     C*  dovrebbero già arrivare dei flag
143701160429     C*
143702160503     C                   if        C1Sogg     <> *Blank
143703160509     C                   Eval      *In15       = *On
143704160429     C                   endif
143800090325     C*
143801160503     C                   if        S1DtFiVl   <> *Zero     and
143802160503     C                             C1DtFiVl    = *Zero
143803160503     C                   Eval      *In20       = *Off
143804160503     C                   else
143805160506     C                   Eval      *In20       = *On
143806160429     C                   endif
144500090325     C*
144600090325     C                   ENDSR
144700090325     C/EJECT
144701160506     C************************************************************
144702160506     C* RIEMPIMENTO RIGA LISTA
144703160506     C************************************************************
144704160506     C     RIES2_PPA     BEGSR
144705160506     C*
144707160506     C                   if        PPADtPar   <> *Loval
144708160513     C     *Jobrun       Move      PPADtPar      S1DtPar
144709160506     C                   endif
144710160513     C                   Z-Add     PPANrPar      S1NrPar
144711160513     C                   MoveL     PPASerPar     S1SerPar
144715160513     C                   MoveL     PPAKcc        S1Kcc
144716160513     C                   MoveL     PPAKsc        S1Ksc
144717160506     C*
144718160610     C                   MOVEL     SO3Dest       S1Dest
144719160506     C                   if        SO3Dest    <> *Blank
144720160506     C*
144721160506     C                   Clear                   SDGHC4DS
144722160506     C*
144723160506     C                   Clear                   SDGTABDS
144724160506     C*
144725160506     C                   Eval      XTbRic      = '1'
144726160506     C                   Eval      XTbAzi      = XScSoc
144727160506     C                   Eval      XTbKey      = *Zero
144728160506     C                   Eval      XTbCod      = 'HC4'
144729160610     C                   Move      S1Dest        XTbKey
144730160506     C*
144731160506     C                   CALL      'SDGTABDR'
144732160506     C                   PARM                    SDGTABDS
144733160506     C*
144734160506     C                   if        XTbErr     <> '0'
144735160513     C                   MoveL     *ALL'?'       S1DestD
144736160506     C*
144737160506     C                   else
144738160506     C                   MoveL     XTbUni        SDGHC4DS
144739160513     C                   MoveL     DesHC4        S1DestD
144740160506     C*
144741160506     C                   endif
144742160506     C*
144743160506     C                   else
144744160506     C*
144745160506     C* "Generica"
144746160506     C                   Eval      IDMSG = 'PROMSG    *LIBL     COS1529'
144747160506     C                   EXSR      PrendiMsg
144748160506     C                   if        ERRMSG     <> *ON
144749160513     C                   MoveL     TXTMSG        S1DestD
144750160506     C                   else
144751160513     C                   MoveL     *ALL'?'       S1DestD
144752160506     C                   endif
144753160506     C*
144754160506     C                   endif
144755160506     C*
144756160513     C                   MOVEL     SO3CogRag     S2CogRag
144757160513    >C                   Z-Add     SO3Sys        S1Sys
144758160513    >C                   Z-Add     SO3NrAsInt    S1NrAsInt
144759160506     C*
144760160506     C* imposto quegli attributi del sfl che sono modificabili
144761160506     C* al richiamo del determinato pgm
144762160506     C                   EXSR      ATRS2
144763160506     C*
144764160506     C                   ENDSR
144765160506     C/EJECT
144766160506     C************************************************************
144767160506     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
144768160506     C************************************************************
144769160506     C     ATRS2         BEGSR
144770160506     C*
144771160506     C* in questa routine bisogna attivare indicatori o campi programma
144772160506     C* che gestiscono gli attributi di uno o più campi di questa videata;
144773160506     C* attenzione : non è questo il posto in cui eseguire controlli,
144774160506     C*  dovrebbero già arrivare dei flag
144775160506     C*
144776160513     C                   if        S1NrAsInt  <> *Zero
144777160509     C                   Eval      *In15       = *On
144778160506     C                   endif
144786160509     C*
144787160513     C                   if        S1Ksc      <> *Blank
144788160509     C                   Eval      *In19       = *Off
144789160509     C                   else
144790160509     C                   Eval      *In19       = *On
144791160509     C                   endif
144792160506     C*
144793160506     C                   ENDSR
144794160506     C/EJECT
144800090325     C************************************************************
144900090325     C* GESTIONE F08 VIDEO S1
145000090325     C************************************************************
145100090325     C     F08S1         BEGSR
145200090325     C*
145300090325     C                   MOVE      *ON           $FINE
145400160422    >C                   MOVE      '0'           RETD15
145500090325     C*
145600090325     C                   ENDSR
145700090325     C/EJECT
145800090325     C************************************************************
145900090325     C* GESTIONE F03 VIDEO S1
146000090325     C************************************************************
146100090325     C     F03S1         BEGSR
146200090325     C*
146300090325     C                   MOVE      *ON           $FINE
146400160422    >C                   MOVE      '1'           RETD15
146500090325     C* fine programma
146600090325     C                   EXSR      ENDPGM
146700090325     C*
146800090325     C                   ENDSR
146900090325     C/EJECT
147000090325     C************************************************************
147100090325     C* GESTIONE F10 VIDEO S1
147200090325     C* AGGIUNTA RECORD
147300090325     C************************************************************
147400090325     C     F10S1         BEGSR
147500090325     C*
147600160422    >C                   RESET                   SDGSD16DS
147800160422    >C                   MOVEL     '01'          OPZD16
147900160422    >C     CMTD15        IFEQ      '1'
148000160422    >C                   MOVEL     '1'           CMTD16
148100090325     C                   ELSE
148200160422    >C                   MOVEL     '0'           CMTD16
148300090325     C                   ENDIF
148301160503    >C                   Eval      TpCallD16   = TpCallD15
148302160503    >C                   Eval      TpDatiD16   = D1TpDati
148400160426     C                   MOVEL     D1Sogg        SoggD16
148401160426     C                   if        D1DtFiVl   <> *Zero
148402160426     C     *Jobrun       MOVE      D1DtFiVl      DtFivlD16
148403160426     C                   endif
148404160513     C                   Eval      CtbD16      = D1Ctb
148405160429     C                   Z-Add     D1Sys         SysD16
148406160429     C                   Z-Add     D1NrAsInt     NrAsIntD16
148407160510     C                   Eval      KccD16      = D1Kcc
148408160510     C                   Eval      KscD16      = D1Ksc
148409160510     C                   if        D1DtPar    <> *Zero
148410160510     C     *Jobrun       MOVE      D1DtPar       DtParD16
148411160510     C                   endif
148412160510     C                   Eval      NrParD16    = D1NrPar
148413160510     C                   Eval      SerParD16   = D1SerPar
148414160610     C                   if        D1Dest      = '**'
148415160610     C                   Eval      DestD16     = *Blank
148416160610     C                   else
148417160426     C                   MOVEL     D1Dest        DestD16
148418160610     C                   endif
148500090325     C* passare anche le parzializzazioni ricevute relative alla chiave
148600090325     C* univoca :
148700090325     C*  se ce ne sono, potranno essere usate come dft d'immissione
148800160422    >C                   IF        OPZD15 <> *Blank
149000090325     C                   ENDIF
149100160422    >C                   MOVEL     SDGSD16DS     KPJBU
149200160422    >C                   CALL      'SDGSD16R'
149300090325     C                   PARM                    KPJBA
149400160422    >C                   MOVEL     KPJBU         SDGSD16DS
149500090325     C* ritorno da PGM gestione
149600160422    >C                   EXSR      RTND16
149700090325     C     WINZS1        IFEQ      *ON
149800090325     C                   MOVE      *ON           $INZS1
149900090325     C* se esistevano già righe sul sfl
150000090325     C* calcolo pagina a cui deve posizionarsi
150100090325     C     WSFL          IFGT      0
150200090325     C                   EXSR      CLCPAG
150300090325     C* altrimenti carico solo la 1a pagina
150400090325     C                   ELSE
150500090325     C                   Z-ADD     SFLPAG        WPAG
150600090325     C                   ENDIF
150700090325     C                   ENDIF
150800090325     C*
150900090325     C                   MOVE      *HIVAL        $LASTV
151000090325     C*
151100090325     C                   ENDSR
151200090325     C/EJECT
151300090325     C************************************************************
151400090325     C* GESTIONE F12 VIDEO S1
151500090325     C************************************************************
151600090325     C     F12S1         BEGSR
151700090325     C*
151800090325     C*  se la 1a videata sono le parzializzazioni, per F12 torno a queste
151900090325     C     $LOP          IFEQ      1
152000090325     C                   MOVE      'D1'          $GEST
152100090325     C*  se la 1a videata è questa, per F12 torno a menu
152200090325     C                   ELSE
152300090325     C                   MOVE      *ON           $FINE
152400160422    >C                   MOVE      '2'           RETD15
152500090325     C                   ENDIF
152600090325     C*
152700090325     C                   ENDSR
152800090325     C/EJECT
152900090325     C************************************************************
153000090325     C* GESTIONE F17 VIDEO S1
153100090325     C************************************************************
153200090325     C     F17S1         BEGSR
153300090325     C*
153400090325     C                   MOVE      'D1'          $GEST
153500090325     C* se il pgm è stato richiamato con chiave univoca
153600090325     C* e dopo è stata richiesta una parzializzazione
153700090325     C* tolgo la gestione della chiave univoca (azzere/abblenco il
153800090325     C* valore; propongo come ordinamento il 1°)
153900090325     C     $Unique       IFEQ      *ON
154000090325     C                   MOVE      *OFF          $Unique
154300090325     C                   ENDIF
154400090325     C     $ORD          IFEQ      99
154500090325     C                   Z-ADD     1             $ORD
154600090325     C                   MOVE      *ON           $INZD1
154700090325     C                   ENDIF
154800090325     C*
154900090325     C                   ENDSR
155000090325     C/EJECT
155100090325     C************************************************************
155200090325     C* GESTIONE F18 VIDEO S1
155300090325     C************************************************************
155400090325     C     F18S1         BEGSR
155500090325     C*
155600090325     C                   MOVE      'D2'          $GEST
155700090325     C                   MOVEL     *ON           $INZD2
155800090325     C     $ORD          IFEQ      99
155900090325     C                   Z-ADD     1             $ORD
156000090325     C                   ENDIF
156100090325     C*
156200090325     C                   ENDSR
156300090325     C/EJECT
156400090325     C************************************************************
156500090325     C* GESTIONE F23 VIDEO S1
156600090325     C************************************************************
156700090325     C     F23S1         BEGSR
156800090325     C*
156900090325     C                   Z-ADD     $C1Op1        $L01
157000090325     C                   Z-ADD     $C1Op2        $L02
157100090325     C                   EXSR      RIEOPZ
157200090325     C                   Z-ADD     23            X
157300090325     C                   MOVE      FF23          $FC2(X)
157400090325     C                   MOVEL     WOPZ1         C1OPZ1
157500090325     C                   MOVEL     $ALLOPT       H1ALLOPT
157600090325     C                   MOVEL     WOPZ2         C1OPZ2
157700090325     C                   Z-ADD     WCOZ          C1COZ
157800090325     C*
157900090325     C                   ENDSR
158000090325     C/EJECT
158100090325     C************************************************************
158200090325     C* GESTIONE F24 VIDEO S1
158300090325     C************************************************************
158400090325     C     F24S1         BEGSR
158500090325     C*
158600090325     C                   MOVEA     $FX2          $FX
158700090325     C                   MOVEA     $FC2          $FC
158800090325     C                   MOVEA     $FR2          $FR
158900090325     C                   Z-ADD     $ULKS1        $ULK
159000090325     C                   Z-ADD     $S1L01        $L01
159100090325     C                   Z-ADD     $S1L02        $L02
159200090325     C                   EXSR      RIEKEY
159300090325     C                   Z-ADD     $ULK          $ULKS1            3 0
159400090325     C                   MOVEA     $FC           $FC2
159500090325     C                   MOVEL     $KEY1         Z2KE1
159600090325     C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
159700090325     C                   MOVEL     $KEY2         Z2KE2
159800090325     C                   WRITE     FMTZ2
159900090325     C*
160000090325     C                   ENDSR
160100090325     C/EJECT
160200090325     C************************************************************
160300090325     C* CONTROLLO TESTATA LISTA
160400090325     C************************************************************
160500090325     C     CTRC1         BEGSR
160600090325     C*
160700090325     C                   MOVE      *OFF          *IN99
160800090325     C*
160900090325     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
161000090325     C* L'INIZIALIZZAZIONE LISTA
161100090325     C*
161200090325     C     C1POS         IFNE      W1POS
161300090325     C                   EXSR      POSS1
161400090325     C                   ENDIF
161500090325     C*
161600090325     C                   ENDSR
161700090325     C/EJECT
161800090325     C************************************************************
161900090325     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
162000090325     C************************************************************
162100090325     C     POSS1         BEGSR
162200090325     C*
162300090325     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
162400090325     C*
162500090325   B1C     WMAX          IFGT      0
162501160506     C                   EXSR      ReadCS1
162700090325   B2C     *IN21         DOWEQ     *OFF
162800090325     C     *IN99         ANDEQ     *OFF
162900090325     C*
163000090325   B3C     S1OPZ         IFNE      0
163100090325     C                   SETON                                        92  99
163200090325   E3C                   ENDIF
163300090325     C* se rilevata opzione attiva, attivo sflnxtchg
163400090325   B3C     *IN99         IFEQ      *ON
163500090325     C                   MOVE      *ON           *IN32
163600090325     C*
163700090325     C* imposto quegli attributi del sfl che sono modificabili
163800090325     C* al richiamo del determinato pgm
163900090325     C                   EXSR      ATRS1
164000090325     C*
164001160506     C                   EXSR      UpdateS1
164200090325   X3C                   ELSE
164201160506     C                   EXSR      ReadCS1
164400090325   E3C                   ENDIF
164500090325     C*
164600090325   E2C                   ENDDO
164700090325     C*
164800090325   E1C                   ENDIF
164900090325     C*
165000090325     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
165100090325     C* AL VALORE DI POSIZIONAMENTO :
165200090325     C* se questo è maggiore del valore del rispettivo campo di
165300090325     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
165400090325     C* con i valori di parzializzazione.
165500090325     C*
165600090325   B1C     *IN99         IFEQ      *OFF
165700090325    >C                   MOVE      *BLANK        W1POS
165800090325     C*
165900090325     C* per ogni ordinamento devo testare se il campo dell'inizio elenco
166000090325     C* è maggiore del relativo valore di parzializzazione, nel qual caso
166100090325     C* imposto la variabile del posizionamento (W1Pos) al nuovo valore;
166200090325     C* richiedo comunque un ricaricamento del sfl che, se non si è
166300090325     C* verificata la condizione suddetta avverrà con posizionamento a
166400090325     C* partire dal valore di parzializzazione
166500090325     C*
166600090325   B2C                   SELECT
166700090325     C*
166800090325     C     $ORD          WHENEQ    1
166801160428     C*
166802160428     C                   CALLB     'XALLINEA'
166803160428     C                   PARM                    C1Pos
166804160428     C                   PARM      8             LEN               5 0
166805160428     C                   PARM                    XSCINP
166806160428     C*
166900160426    >C     C1POS         IFGT      D1Sogg
167000090325    >C                   MOVEL     C1POS         W1POS
167100090325     C                   ENDIF
167200090325     C*
167300090325     C* per l'ordinamento 99 = POSTA non viene gestito il posizionamento
167400090325     C*
167500090325   E2C                   ENDSL
167600090325     C*
167700090325     C* richiedo l'inizializzazione sfl
167800090325     C                   MOVE      *ON           $INZS1
167900090325     C*
168000090325   E1C                   ENDIF
168100090325     C*
168200090325     C                   ENDSR
168300090325     C/EJECT
168301160506     C************************************************************
168302160506     C* Aggiornamento riga sfl
168303160506     C************************************************************
168304160506     C     UpdateS1      BEGSR
168305160506     C*
168306160506     C                   Select
168307160506     C                   When      D1TpDati    = '1'
168310160506     C* imposto quegli attributi del sfl che sono modificabili
168311160506     C* al richiamo del determinato pgm
168312160506     C                   EXSR      ATRS1
168313160506     C                   UPDATE    FMTS1
168314160506     C*
168315160506     C                   When      D1TpDati    = '2'
168324160506     C* imposto quegli attributi del sfl che sono modificabili
168325160506     C* al richiamo del determinato pgm
168326160506     C                   EXSR      ATRS2
168327160506     C                   UPDATE    FMTS2
168328160506     C*
168329160506     C                   endSl
168330160506     C*
168331160506     C                   ENDSR
168332160506     C/EJECT
168333160506     C************************************************************
168334160506     C* Lettura riga SFL
168335160506     C************************************************************
168336160506     C     ReadCS1       BEGSR
168337160506     C*
168338160506     C                   Select
168339160506     C                   When      D1TpDati    = '1'
168343160506     C                   READC     FMTS1                                  21
168344160506     C*
168345160506     C                   When      D1TpDati    = '2'
168354160506     C                   READC     FMTS2                                  21
168355160506     C*
168356160506     C                   endSl
168357160506     C*
168358160506     C                   ENDSR
168359160506     C/EJECT
168400090325     C************************************************************
168500090325     C* CONTROLLO OPZIONI LISTA
168600090325     C************************************************************
168700090325     C     CTRS1         BEGSR
168800090325     C*
168900090325     C                   MOVEL     *OFF          $ESCI
169000090325     C                   SETOFF                                       99
169100090325     C                   Z-ADD     0             S1OPZ
169200090325     C*
169300090325     C* Leggo il sfl solo se ci sono rcd
169400090325   B1C     WMAX          IFGT      0
169501160506     C                   EXSR      ReadCS1
169600090325     C*
169700090325     C* esce se fine sfl o errore che richiede l'uscita
169800090325   B2C     *IN21         DOWEQ     *OFF
169900090325     C     $ESCI         ANDEQ     *OFF
170000090325     C                   Z-ADD     S1NRR         C1RCD
170100090325     C* ctrl su riga
170200090325     C                   EXSR      RECS1
170300090325     C* gestione opzioni
170400090325   B3C     S1OPZ         IFNE      0
170500090325     C     *IN99         ANDEQ     *OFF
170600090325     C                   EXSR      OPZS1
170700090325   E3C                   ENDIF
170800090325     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
170900090325   B3C     *IN99         IFEQ      *ON
171000090325     C                   MOVE      *ON           *IN32
171100090325   E3C                   ENDIF
171600090325     C*
171700090325     C                   Z-ADD     0             S1OPZ
171800090325     C*
171801160506     C                   EXSR      UpdateS1
172000090325     C                   MOVE      *OFF          *IN32
172100090325     C*
172200090325     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
172300090325   B3C     $ESCI         IFEQ      *OFF
172301160506     C                   EXSR      ReadCS1
172500090325     C* a fine ciclo ripristino il flag richiesta iniz.sfl
172600090325   B4C     *IN21         IFEQ      *ON
172700090325     C                   MOVE      WINZS1        $INZS1
172800090325     C                   MOVE      *off          WINZS1
172900090325     C* se è avvenuta almeno una modifica
173000090325   B5C                   IF        $INZS1 = *on
173100090325     C* calcolo pagina a cui deve posizionarsi
173200090325     C     WSFL          IFGT      0
173300090325     C                   EXSR      CLCPAG
173400090325     C                   ELSE
173500090325     C                   Z-ADD     SFLPAG        WPAG
173600090325     C                   ENDIF
173700090325   E5C                   ENDIF
173800090325   E4C                   ENDIF
173900090325   E3C                   ENDIF
174000090325     C*
174100090325   E2C                   ENDDO
174200090325     C*
174300090325   E1C                   ENDIF
174400090325     C*
174500090325     C                   ENDSR
174600090325     C/EJECT
174700090325     C************************************************************
174800090325     C* CONTROLLO CAMPI I/O RIGA LISTA
174900090325     C************************************************************
175000090325     C     RECS1         BEGSR
175100090325     C*
175200090325     C* reset indicatori DSPATR
175300090325     C                   MOVE      *ALL'0'       IN4049           10
175400090325     C                   MOVEA     IN4049        *IN(40)
175500090325     C*
175600090325     C* Controllo se Utente Autorizzato alla scelta
175700090325     C     S1OPZ         IFNE      0
175800090325     C                   Z-ADD     1             X
175900090325     C     S1OPZ         LOOKUP    $OP1(X)                                21
176000090325     C* mi salvo l'indice dell' opzione
176100090325     C   21              Z-add     X             PosOpz            2 0
176200090325     C     *IN21         IFEQ      *OFF
176300090325     C     $FL1(X)       OREQ      'N'
176400090325     C                   MOVE      *ON           $ESCI
176500090325     C                   SETON                                        409199
176600090325     C                   ENDIF
176700090325     C                   ENDIF
176800090325     C*
177000090325     C                   ENDSR
177100090325     C/EJECT
177200090325     C************************************************************
177300090325     C* GESTIONE OPZIONI LISTA
177400090325     C************************************************************
177500090325     C     OPZS1         BEGSR
177600090325     C*
177700090325     C                   Z-ADD     RIRI          CURR
177800090325     C                   Z-ADD     COCO          CURC
177900090325     C*
178000090325     C                   SELECT
178100090325     C*
192000090325     C*
192100090325     C* se altre opzioni
192200090325     C* richiamo pgm gestione anagrafica
192300090325     C                   OTHER
192400090325     C*
192500160422    >C                   RESET                   SDGSD16DS
192700160422    >C                   MOVEL     S1OPZ         OPZD16
192800160422    >C     CMTD15        IFEQ      '1'
192900160422    >C                   MOVEL     '1'           CMTD16
193000090325     C                   ELSE
193100160422    >C                   MOVEL     '0'           CMTD16
193200090325     C                   ENDIF
193201160503    >C                   Eval      TpCallD16   = TpCallD15
193202160503    >C                   Eval      TpDatiD16   = D1TpDati
193300160422     C* passare nei campi appositi della DS SDGSD16DS il valore
193400090325     C* della chiave della VL univoca
193401160426     C                   if        S1Sogg     <> *Blank
193402160426    >C                   MOVEL     S1Sogg        SoggD16
193403160428     C                   if        S1DtFiVl   <> *Zero
193404160426     C     *JobRun       Move      S1DtFiVl      DtFiVlD16
193405160426     C                   else
193406160426     C                   Eval      DtFiVlD16   = *Loval
193407160426     C                   endif
193408160426     C                   else
193500160426    >C                   MOVEL     C1Sogg        SoggD16
193501160505     C                   if        C1DtFiVl   <> *Zero
193502160426     C     *JobRun       Move      C1DtFiVl      DtFiVlD16
193503160426     C                   else
193504160426     C                   Eval      DtFiVlD16   = *Loval
193505160426     C                   endif
193506160426     C                   endif
193507160426     C*
193508160513     C                   if        C1NrAsInt  <> *Zero
193509160513     C                   Z-add     C1NrAsint     NrAsIntD16
193510160513     C                   Z-add     C1Sys         SysD16
193511160426     C                   else
193512160513     C                   Z-add     S1NrAsint     NrAsIntD16
193513160513     C                   Z-add     S1Sys         SysD16
193514160426     C                   endif
193515160513     C                   Eval      CtbD16      = D1Ctb
193516160513     C                   if        S1Kcc      <> *Blank     and
193517160513     C                             S1Ksc      <> *Blank
193518160513     C                   MoveL     S1Kcc         KccD16
193519160513     C                   MoveL     S1Ksc         KscD16
193520160510     C                   else
193521160513     C                   MoveL     C1Kcc         KccD16
193522160513     C                   MoveL     C1Ksc         KscD16
193523160510     C                   endif
193526160513     C                   if        S1DtPar    <> *Zero
193527160513     C     *Jobrun       MOVE      S1DtPar       DtParD16
193528160510     C                   endif
193529160513     C                   Eval      NrParD16    = S1NrPar
193530160513     C                   Eval      SerParD16   = S1SerPar
193536160429     C*
193537160610    >C                   MOVEL     S1Dest        DestD16
193700090325     C*
193800160422    >C                   MOVEL     SDGSD16DS     KPJBU
193900160422    >C                   CALL      'SDGSD16R'
194000090325     C                   PARM                    KPJBA
194100090325    >C                   PARM                    AUTDS
194200090325     C*
194300160422    >C                   MOVEL     KPJBU         SDGSD16DS
194400090325     C*
194500090325     C* ritorno da PGM gestione
194600160422    >C                   EXSR      RTND16
194700090325     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
194800090325     C     *IN99         IFEQ      *ON
194900090325     C                   SETON                                        40
195000090325     C                   ENDIF
195100090325     C*
195200090325     C                   MOVE      *HIVAL        $LASTV
195300090325     C                   ENDSL
195400090325     C*
195500090325     C                   ENDSR
195600090325     C/EJECT
195700090325     C************************************************************
195800090325     C* GESTIONE RITORNO DA PGM DI GESTIONE
195900090325     C************************************************************
196000160422    >C     RTND16        BEGSR
196100090325     C*
196200090325     C* modo di ritorno
196300090325     C*
196400090325     C                   SELECT
196500090325     C* questi modi di utilizzo dei valori di ritorno dal
196600090325     C* pgm di manutenzione rcd di anagrafica sono delle
196700090325     C* proposte, normalmente sempre valide, ma modificabili
196800090325     C* per situazioni particolari
196900090325     C* 1 = F3
197000160422    >C     RETD16        WHENEQ    '1'
197100090325     C                   MOVE      *ON           $ESCI                          sospese READC
197200090325     C                   MOVE      *ON           $FINE                          fine pgm
197300160422    >C                   MOVE      '1'           RETD15
197400090325     C* 2 = F12
197500160422    >C     RETD16        WHENEQ    '2'
197600090325     C* se il pgm di manutenzione ha prima una guida, è necessario anche
197700160422     c* questo test, altrimenti prevale il retD16 e il sfl non viene
197800090325     C* ricaricato
197900160422    >C     OPRD16        ANDNE     '1'
198000090325     C                   MOVE      *ON           $ESCI                          sospese READC
198100090325     C*
198200090325     C                   ENDSL
198300090325     C*
198400090325     C* operazione eseguite dal pgm chiamato
198500090325     C*
198600090325     C                   SELECT
198700090325     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
198800160422    >C     OPRD16        WHENEQ    '1'
198900160422    >C                   MOVE      OPRD16        WOPR                           avverto pgm chiamant
199000090325     C*
199100090325     C* se l'opzione con cui è stato chiamato non è la modifica
199200090325     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
199300160422    >C     OPZD16        IFNE      '02'
199400090325     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
199500090325     C* se l'opzione con cui è stato chiamato è la modifica
199600090325     C* aggiorno subito la riga di sfl e non richiedo il ripristino sfl
199700090325     C                   ELSE
199800090325     C                   EXSR      AGGS1
199900090325     C                   ENDIF
200000090325     C*
200100090325     C                   ENDSL
200200090325     C*
200300090325     C* funzione non eseguibile per errore :
200400090325     C*
200500090325     C                   SELECT
200600090325     C* 1 = funzione richiamata chiusa in errore
200700160422    >C     ERRD16        WHENEQ    '1'
200800090325     C                   MOVE      *ON           $ESCI                          sospese READC
200900090325     C                   SETON                                        9399      emiss.SFLMSGID
201000090325     C* 2 = funzione richiamata senza la dovuta autorizzazione
201100160422    >C     ERRD16        WHENEQ    '2'
201200090325     C                   MOVE      *ON           $ESCI                          sospese READC
201300090325     C                   SETON                                        9499      emiss.SFLMSGID
201400090325     C*
201500090325     C                   ENDSL
201600090325     C*
201700090325     C                   ENDSR
201800090325     C/EJECT
214300090325     C************************************************************
214400090325     C* AGGIORNAMENTO RIGA LISTA
214500090325     C************************************************************
214600090325     C     AGGS1         BEGSR
214700090325     C*
214800090325     C* riempio i campi del sfl che sono stati ripassati dal pgm
214900090325     C* di manutenzione dopo una opzione 02
215000090325     C*
215200160426     C                   MOVEL     NomeD16       S1Nome
215300160513     C                   MOVEL     CogRagD16     S1CogRag
215301160513     C                   MOVEL     CogRagD16     S2CogRag
215400090325     C*
215500090325     C                   ENDSR
215600090325     C/EJECT
225100090325     C************************************************************
225200090325     C* GESTIONE VALORI ASSUNTI
225300090325     C************************************************************
225400090325     C     GESD2         BEGSR
225500090325     C*
225600090325   B1C     $INZD2        IFEQ      *ON
225700090325     C                   EXSR      INZD2
225800090325     C                   MOVE      *OFF          $INZD2
225900090325   E1C                   ENDIF
226000090325     C*
226100090325   B1C     $LASTV        IFNE      'D2'
226200090325     C                   EXSR      GESPIE
226300090325   B1C     $LASTV        IFNE      *HIVAL
226400090325     C                   MOVE      $LASTV        $LASTG
226500090325     C                   ENDIF
226600090325     C                   MOVE      'D2'          $LASTV
226700090325   E1C                   ENDIF
226800090325     C*
226900090325     C* se esistono errori sulla videata
227000090325     C* emetto la write del formato a indicatori spenti per vedere
227100090325     C* le eventuali decodifiche
227200090325   B1C     *IN99         IFEQ      *ON
227300090325     C                   MOVEA     *IN           WIN
227400090325     C                   MOVE      *ALL'0'       IN5098           49
227500090325     C                   MOVEA     IN5098        *IN(50)
227600090325     C                   WRITE     FMTD2
227700090325     C                   MOVEA     WIN           *IN
227800090325   E1C                   ENDIF
227900090325     C*
228000090325     C                   EXFMT     FMTD2
228100090325     C*
228200090325     C* azzero la posizione del cursore
228300090325     C                   Z-ADD     0             H1RIGA
228400090325     C                   Z-ADD     0             H1COLO
228500090325     C*
228600090325     C* CONTROLLO ABILITAZIONE TASTI
228700090325     C* INPUT
228800090325     C* $FC3 = Abilitazioni videata D2
228900090325     C* OUTPUT
229000090325     C* *IN97 + *IN99 Errori
229100090325     C                   MOVEA     $FC3          $FC
229200090325     C                   EXSR      CTRKEY
229300090325     C*
229400090325   B1C     *IN99         IFEQ      *OFF
229500090325     C*
229600090325   B2C                   SELECT
229700090325     C* F3=Fine
229800090325     C     $TASTO        WHENEQ    F03
229900090325     C                   EXSR      F03D2
230000090325     C* F12=Ritorno
230100090325     C     $TASTO        WHENEQ    F12
230200090325     C                   EXSR      F12D2
230300090325     C* F24=Altri tasti
230400090325     C     $TASTO        WHENEQ    F24
230500090325     C                   EXSR      F24D2
230600090325     C*
230700090325     C                   OTHER
230800090325     C                   EXSR      CTRD2
230900090325   B3C     *IN99         IFEQ      *OFF
231000090325     C                   MOVE      $LASTG        $GEST
231100090325   B4C                   SELECT
231200090325     C     $LASTG        WHENEQ    'S1'
231300090325     C* se è stato modificato l'ordinamento di default, ricarico la lista
231400090325   B5C     $ORD          IFNE      D2ORD
231500090325     C                   MOVE      *ON           $INZS1
231600090325     C                   MOVE      *BLANK        W1POS
231700090325   E5C                   ENDIF
231800090325     C     $LASTG        WHENEQ    'D1'
231900090325     C* per ora niente
232000090325   E4C                   ENDSL
232100090325     C* definisco i nuovi valori di default
232200090325     C                   Z-ADD     D2LOP         $LOP
232300090325     C                   Z-ADD     D2ORD         $ORD
232400090325     C*
232500090325     C* aggiorno valori assunti
232600090325     C* se richiesto il salvataggio
232700090325   B4C     D2BSV         IFEQ      1
232800090325     C                   EXSR      AGGMPE
232900090325   E4C                   ENDIF
233000090325     C*
233100090325   E3C                   ENDIF
233200090325     C*
233300090325   E2C                   ENDSL
233400090325     C*
233500090325   E1C                   ENDIF
233600090325     C*
233700090325     C                   ENDSR
233800090325     C/EJECT
233900090325     C************************************************************
234000090325     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
234100090325     C************************************************************
234200090325     C     INZD2         BEGSR
234300090325     C*
234400090325     C                   CLEAR                   FMTD2
234500090325     C                   EVAL      D2BSVA = Normale
234600090325     C* se sono entrato con chiave univoca
234700090325     C* impedisco il salvataggio valori assunti
234800090325     C     $Unique       IFEQ      *ON
234900090325     C                   EVAL      D2BSVA = Protetto
235000090325     C                   ENDIF
235100090325     C                   MOVEL     $LOP          D2LOP
235200090325     C                   MOVE      $ORD          D2ORD
235300090325     C*
235400090325     C                   ENDSR
235500090325     C/EJECT
235600090325     C************************************************************
235700090325     C* GESTIONE F03 VIDEO D2
235800090325     C************************************************************
235900090325     C     F03D2         BEGSR
236000090325     C*
236100090325     C                   MOVE      *ON           $FINE
236200090325     C* fine programma
236300090325     C                   EXSR      ENDPGM
236400090325     C*
236500090325     C                   ENDSR
236600090325     C/EJECT
236700090325     C************************************************************
236800090325     C* GESTIONE F12 VIDEO D2
236900090325     C************************************************************
237000090325     C     F12D2         BEGSR
237100090325     C*
237200090325     C                   MOVE      $LASTG        $GEST
237300090325     C*
237400090325     C                   ENDSR
237500090325     C/EJECT
237600090325     C************************************************************
237700090325     C* GESTIONE F24 VIDEO D2
237800090325     C************************************************************
237900090325     C     F24D2         BEGSR
238000090325     C*
238100090325     C                   MOVEA     $FX3          $FX
238200090325     C                   MOVEA     $FC3          $FC
238300090325     C                   MOVEA     $FR3          $FR
238400090325     C                   Z-ADD     $ULKD2        $ULK
238500090325     C                   Z-ADD     $D2L01        $L01
238600090325     C                   Z-ADD     $D2L02        $L02
238700090325     C                   EXSR      RIEKEY
238800090325     C                   Z-ADD     $ULK          $ULKD2            3 0
238900090325     C                   MOVEA     $FC           $FC3
239000090325     C                   MOVEL     $KEY1         Z3KE1
239100090325     C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
239200090325     C                   WRITE     FMTZ3
239300090325     C*
239400090325     C                   ENDSR
239500090325     C/EJECT
239600090325     C************************************************************
239700090325     C* CONTROLLO VIDEATA VALORI ASSUNTI
239800090325     C************************************************************
239900090325     C     CTRD2         BEGSR
240000090325     C*
240100090325     C                   SETOFF                                       99
240200090325     C*
240300090325     C* OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
240400090325     C     D2LOP         IFEQ      0
240500090325     C                   SETON                                          5099
240600090325     C                   ENDIF
240700090325     C*
240800090325     C* OBBLIGATORIO VALORE PER ORDINAMENTO
240900090325     C     D2ORD         IFEQ      0
241000090325     C                   SETON                                          5199
241100090325     C                   ENDIF
241200090325     C*
241300090325     C                   ENDSR
241400090325     C/EJECT
241500090325     C************************************************************
241600090325     C* CONTROLLO ABILITAZIONE TASTI
241700090325     C* INPUT
241800090325     C* $TASTO =Tasto funzione premuto
241900090325     C* $FC = Tasti abilitati
242000090325     C* OUTPUT
242100090325     C* IN99 = ERrore
242200090325     C************************************************************
242300090325     C     CTRKEY        BEGSR
242400090325     C*
242500090325     C                   SETOFF                                       99
242600090325     C*
242700090325     C                   SELECT
242800090325     C     $TASTO        WHENEQ    F01
242900090325     C                   z-add     01            posfun
243000090325     C     $FC(1)        IFEQ      '0'
243100090325     C                   SETON                                        9997
243200090325     C                   ENDIF
243300090325     C     $TASTO        WHENEQ    F02
243400090325     C                   z-add     02            posfun
243500090325     C     $FC(2)        IFEQ      '0'
243600090325     C                   SETON                                        9997
243700090325     C                   ENDIF
243800090325     C     $TASTO        WHENEQ    F03
243900090325     C                   z-add     03            posfun
244000090325     C     $FC(3)        IFEQ      '0'
244100090325     C                   SETON                                        9997
244200090325     C                   ENDIF
244300090325     C     $TASTO        WHENEQ    F04
244400090325     C                   z-add     04            posfun
244500090325     C     $FC(4)        IFEQ      '0'
244600090325     C                   SETON                                        9997
244700090325     C                   ENDIF
244800090325     C     $TASTO        WHENEQ    F05
244900090325     C                   z-add     05            posfun
245000090325     C     $FC(5)        IFEQ      '0'
245100090325     C                   SETON                                        9997
245200090325     C                   ENDIF
245300090325     C     $TASTO        WHENEQ    F06
245400090325     C                   z-add     06            posfun
245500090325     C     $FC(6)        IFEQ      '0'
245600090325     C                   SETON                                        9997
245700090325     C                   ENDIF
245800090325     C     $TASTO        WHENEQ    F07
245900090325     C                   z-add     07            posfun
246000090325     C     $FC(7)        IFEQ      '0'
246100090325     C                   SETON                                        9997
246200090325     C                   ENDIF
246300090325     C     $TASTO        WHENEQ    F08
246400090325     C                   z-add     08            posfun
246500090325     C     $FC(8)        IFEQ      '0'
246600090325     C                   SETON                                        9997
246700090325     C                   ENDIF
246800090325     C     $TASTO        WHENEQ    F09
246900090325     C                   z-add     09            posfun
247000090325     C     $FC(09)       IFEQ      '0'
247100090325     C                   SETON                                        9997
247200090325     C                   ENDIF
247300090325     C     $TASTO        WHENEQ    F10
247400090325     C                   z-add     10            posfun
247500090325     C     $FC(10)       IFEQ      '0'
247600090325     C                   SETON                                        9997
247700090325     C                   ENDIF
247800090325     C     $TASTO        WHENEQ    F11
247900090325     C                   z-add     11            posfun
248000090325     C     $FC(11)       IFEQ      '0'
248100090325     C                   SETON                                        9997
248200090325     C                   ENDIF
248300090325     C     $TASTO        WHENEQ    F12
248400090325     C                   z-add     12            posfun
248500090325     C     $FC(12)       IFEQ      '0'
248600090325     C                   SETON                                        9997
248700090325     C                   ENDIF
248800090325     C     $TASTO        WHENEQ    F13
248900090325     C                   z-add     13            posfun
249000090325     C     $FC(13)       IFEQ      '0'
249100090325     C                   SETON                                        9997
249200090325     C                   ENDIF
249300090325     C     $TASTO        WHENEQ    F14
249400090325     C                   z-add     14            posfun
249500090325     C     $FC(14)       IFEQ      '0'
249600090325     C                   SETON                                        9997
249700090325     C                   ENDIF
249800090325     C     $TASTO        WHENEQ    F15
249900090325     C                   z-add     15            posfun
250000090325     C     $FC(15)       IFEQ      '0'
250100090325     C                   SETON                                        9997
250200090325     C                   ENDIF
250300090325     C     $TASTO        WHENEQ    F16
250400090325     C                   z-add     16            posfun
250500090325     C     $FC(16)       IFEQ      '0'
250600090325     C                   SETON                                        9997
250700090325     C                   ENDIF
250800090325     C     $TASTO        WHENEQ    F17
250900090325     C                   z-add     17            posfun
251000090325     C     $FC(17)       IFEQ      '0'
251100090325     C                   SETON                                        9997
251200090325     C                   ENDIF
251300090325     C     $TASTO        WHENEQ    F18
251400090325     C                   z-add     18            posfun
251500090325     C     $FC(18)       IFEQ      '0'
251600090325     C                   SETON                                        9997
251700090325     C                   ENDIF
251800090325     C     $TASTO        WHENEQ    F19
251900090325     C                   z-add     19            posfun
252000090325     C     $FC(19)       IFEQ      '0'
252100090325     C                   SETON                                        9997
252200090325     C                   ENDIF
252300090325     C     $TASTO        WHENEQ    F20
252400090325     C                   z-add     20            posfun
252500090325     C     $FC(20)       IFEQ      '0'
252600090325     C                   SETON                                        9997
252700090325     C                   ENDIF
252800090325     C     $TASTO        WHENEQ    F21
252900090325     C                   z-add     21            posfun
253000090325     C     $FC(21)       IFEQ      '0'
253100090325     C                   SETON                                        9997
253200090325     C                   ENDIF
253300090325     C     $TASTO        WHENEQ    F22
253400090325     C                   z-add     22            posfun
253500090325     C     $FC(22)       IFEQ      '0'
253600090325     C                   SETON                                        9997
253700090325     C                   ENDIF
253800090325     C     $TASTO        WHENEQ    F23
253900090325     C                   z-add     23            posfun
254000090325     C     $FC(23)       IFEQ      '0'
254100090325     C                   SETON                                        9997
254200090325     C                   ENDIF
254300090325     C     $TASTO        WHENEQ    F24
254400090325     C                   z-add     24            posfun
254500090325     C     $FC(24)       IFEQ      '0'
254600090325     C                   SETON                                        9997
254700090325     C                   ENDIF
254800090325     C                   ENDSL
254900090325     C*
255000090325     C                   ENDSR
255100090325     C/EJECT
255200090325     C************************************************************
255300090325     C* AGGIORNAMENTO VALORI ASSUNTI
255400090325     C************************************************************
255500090325     C     AGGMPE        BEGSR
255600090325     C*
255700090325     C* preparo i parametri di chiamata al driver
255800090325     C                   CLEAR                   XMPEDS
255900090325     C                   EVAL      TpChiamata = '03'
256000090325     C                   EVAL      NChiavi = 5
256100090325     C                   EVAL      VLogica = 2
256200090325     C*
256300090325     C* preparo le chiavi per la chiamata al driver
256400090325     C                   CLEAR                   XMPEDS1
256500090325     C                   MOVEL     XSCSOC        DSMPESOC
256600090325     C                   MOVEL     DSPGM         DSMPEPGM
256700090325     C                   MOVEL     KNMUS         DSMPEPRF
256800090325     C*
256900090325     C* Si memorizzano i dati di default a seconda delle modalità con
257000090325     C* cui il pgm è usato
257100090325     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
257200090325     C* MPETIP = 'PV'  pgm richiamato da menù
257300090325     C* MPETIP = 'PB'  x lancio Batch
257400090325     C*
257500090325     C                   SELECT
257600160422    >C     OPZD15        WHENEQ    '01'
257700090325     C                   MOVEL     'PX'          DSMPETIP
257800090325     C                   OTHER
257900090325     C                   MOVEL     'PV'          DSMPETIP
258000090325     C                   ENDSL
258100090325     C* dati
258200090325     C                   MOVEL     DSMPE         DSMPEPAR
258300090325     C*
258400090325     C                   CALLB     'XMPE'
258500090325     C                   PARM                    XMPEDS
258600090325     C                   PARM                    XMPEDS1
258700090325     C*
258800090325     C                   ENDSR
258900090325     C/EJECT
259000090325     C************************************************************
259100090325     C* OPERAZIONI INIZIALI
259200090325     C************************************************************
259300090325     C     *INZSR        BEGSR
259400090325     C*
259500090325     C* Reperimento parametri
259600090325     C*
259700090325     C     *ENTRY        PLIST
259800090325     C                   PARM                    KPJBA
259900090325     C                   PARM                    DSAUT
260000090325     C*
260100090325     C* testo se sono stati passati i dati di autorizzazione
260200090325     C*
260300090325     C     PARMS         IFLT      2
260400090325     C                   MOVE      *BLANK        AUTDS
260500090325     C                   ELSE
260600090325     C                   MOVEL     DSAUT         AUTDS
260700090325     C                   ENDIF
260800090325     C*
260900090325     C* Reperimento dati società
261100090325     C                   MOVEL     'SOC001'      TIPXSC
261200160505     C                   MOVEL     *Blank        SOCXSC
261300090325     C                   EXSR      REPSOC
261400090325     C*
261500090325     C* Controlli dati società
261600090325     C*
261700090325     C     RTNXSC        IFNE      '1'
261800090325     C                   MOVEL     XSOCDS        SOC001
261900090325     C                   ELSE
262000090325     C                   MOVE      *ON           $FINE
262100160422    >C                   MOVE      '1'           ErrD15
262200090325     C                   EXSR      ENDPGM
262300090325     C                   ENDIF
262400090325     C*
262500090325     C* reperisco le dimensioni delle variabili che conterranno i tasti
262600090325     C                   EVAL      $D1L01 = %Size(Z1Ke1)
262700090325     C                   EVAL      $D1L02 = %Size(Z1Ke2)
262800090325     C                   EVAL      $S1L01 = %Size(Z2Ke1)
262900090325     C                   EVAL      $S1L02 = %Size(Z2Ke2)
263000090325     C                   EVAL      $C1Op1 = %Size(C1Opz1)
263100090325     C                   EVAL      $C1Op2 = %Size(C1Opz2)
263200090325     C                   EVAL      $D2L01 = %Size(Z3Ke1)
263300090325     C                   EVAL      $D2L02 = 0
263400090325     C*
263500090325     C* Reperimento opzioni
263600090325     C*
263700090325     C                   EXSR      REPOPZ
263800090325     C*
263900090325     C* Reperimento tasti di funzione
264000090325     C*
264100090325     C                   EXSR      REPKEY
264200090325     C* Chiamata a routine per il reperimento delle opzioni
264300090325     C* personalizzate
264400090325     C                   CAllb     'X46USREXT'
264500090325     C                   PARM                    $OP1
264600090325     C                   PARM                    $TX1
264700090325     C                   PARM                    $FL1
264800090325     C                   PARM                    $PGO1
264900090325     C                   PARM      $dimOp        NumOpz
265000090325     C                   PARM                    $FX2
265100090325     C                   PARM                    $FC2
265200090325     C                   PARM                    $PGF2
265300090325     C                   PARM                    DsPgm
265400090325     C                   PARM                    xsccut
265500090325     C                   PARM                    xscgrp
265600090325     C*
265700090325     C                   Z-add     1             OPZLEN
265800090325     C     *blank        Lookup    $TX1(opzlen)                           21
265900090325     C                   if        *in21 = *on
266000090325     C                   sub       1             OPZLEN
266100090325     C                   else
266200090325     C                   Z-add     $DimOp        OPZLEN
266300090325     C                   endif
266400090325     C*
266500090325     C                   ENDSR
266600090325     C/EJECT
266700090325     C************************************************************
266800090325     C* INIZIALIZZAZIONE VARIABILI
266900090325     C************************************************************
267000090325     C     INZVAR        BEGSR
267100090325     C*
267200090325     C* Dati da PGM chiamante
267300090325     C*
267400160422    >C                   MOVEL     KPJBU         SDGSD15DS
267500090325     C*
267600090325     C* Se società diversa da quella impostata nella *inzsr, rieseguo
267700090325     C* reperimento dati società
267901160505     C                   if        SocietaD15 <> XScSoc and
267902160505     C                             SocietaD15 <> *Blanks
268000090325     C                   MOVEL     'SOC001'      TIPXSC
268100160505    >C                   MOVEL     SocietaD15    SOCXSC
268200090325     C                   EXSR      REPSOC
268300090325     C     RTNXSC        IFNE      '1'
268400090325     C                   MOVEL     XSOCDS        SOC001
268500090325     C                   ELSE
268600090325     C                   MOVE      *ON           $FINE
268700160422    >C                   MOVE      '1'           ErrD15
268800090325     C                   EXSR      ENDPGM
268900090325     C                   ENDIF
269000090325     C                   ENDIF
269100090325     C*
269200090325     C* Reperimento autorizzazioni
269300090325     C*
269400090325     C* le autorizzazioni potrebbero essere già state passate,
269500090325     C* nel qual caso non vanno reperite
269600090325    >C     AUTDS         IFEQ      *BLANK
269700090325    >C                   EXSR      REPAUT
269800090325    >C                   ELSE
269801160422     C                   MoveL     XCADDS        ANGSOGDS
270000090325    >C                   ENDIF
270100090325     C*
270200090325     C* Reperimento valori default utente
270300090325     C*
270400090325     C                   EXSR      REPDFT
270700090325     C*
270800090325     C* Definizione delle opzioni utilizzabili a pgm
270900090325     C*
271000090325     C                   EXSR      DEFOPZ
271001160506     C*
271002160506     C* visualizzo il sfl troncato (chiuso)
271005160506     C                   Eval      C1Mode      = *On
271100090325     C*
271200090325     C* Variabili per gestione videate
271300090325     C*
271400090325     C                   MOVE      *OFF          $FINE                          fine pgm
271500090325     C                   MOVE      *OFF          $INZD1                         inizializzione D1
271600090325     C                   MOVE      *OFF          $INZS1                         inizializzione S1
271700090325     C                   MOVE      *OFF          $INZD2                         inizializzione D2
271800090325     C                   MOVE      *OFF          $EndFile                       memo fine file
271900090325     C                   MOVE      *OFF          $ESCI                          rich.uscita ciclo
272000090325     C                   MOVE      *OFF          $RCDOK                         rcd letto valido
272100090325     C                   MOVE      *HIVAL        $LASTV
272200090325     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
272300090325     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
272400090325     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
272500090325     C* azzero la posizione del cursore
272600090325     C                   Z-ADD     0             H1RIGA
272700090325     C                   Z-ADD     0             H1COLO
272800090325     C*
272900090325     C                   MOVE      *ON           $INZD1
273000090325     C                   MOVE      *ON           $INZS1
273100090325     C*
273200090325     C* se pgm richiamato con chiave univoca impostata,
273300090325     C* forzo la emissione della lista e imposto l'ordinamento
273400160422    >C     OpzD15        IFNE      *BLANK
273500090416     C* Se chiamato non emetto videata parzializzazione
273600090416     C                   Z-ADD     2             $LOP
274300090325     C                   MOVE      *OFF          $Unique
274500090325     C                   ENDIF
274600090325     C*
274700090325     C* Variabili appoggio
274800090325     C*
274900090325     C                   Z-ADD     SFLPAG        WPAG
275000090325     C                   MOVEL     *OFF          WOPR
275100090325     C                   CLEAR                   W1POS
275200090325     C*
275300090325     C* definisco qual'è la prima videata
275400090325     C* (se la prima videata risulta la lista, devo comunque caricare
275500090325     C*  la videata di parzializzazione e gestire la definizione
275600090325     C*  dell'ordinamento, perchè, dai default oppure se sono stati
275700090325     C*  passati dei dati, può variare il caricamento)
275800090325     C                   SELECT
275900090325     C     $LOP          WHENEQ    1
276000090325     C                   MOVE      'D1'          $GEST
276100090325     C     $LOP          WHENEQ    2
276200090325     C                   MOVE      'S1'          $GEST
276300090325     C                   EXSR      INZD1
276400090325     C                   MOVE      *OFF          $INZD1
276500090325     C                   EXSR      DEFORD
276600090325     C                   ENDSL
276700090325     C*
276800090325     C* Valorizzazione campi univoci testate
276900090325     C*
277000090325     C                   CLEAR                   FMTT1
277100090325     C                   MOVEL     KNSIF         NOMSIF
277200090325     C                   MOVEL     XSCDSI        NOMDIT
277300090930     C                   MOVEL     KCDAZ         NOMAZN
277400090930     C                   MOVEL     DSPGM         NOMPGM           10
277401160509     C*
277402160513     C                   EXSR      RepSottit
279100090325     C*
279200090325     C                   ENDSR
279300090325     C/EJECT
279400090325     C************************************************************
279500160509     C* REPERIMENTO sottotitolo di testata
279600090325     C************************************************************
279700160509     C     RepSottit     BEGSR
279800090325     C*
279801160509     C*
279802160509     C* recupero il sottotitolo (se c'è)
279803160509     C                   MOVEL     *BLANK        T1TIT
279804160509     C*
279805160509     C                   Select
279806160509     C* chiamata da anagrafica soggetti oppure
279807160509     C* chiamata da menu con richiesta di dati anagrafici di soggetti
279808160509     C* senza indicare una data di fine validità (cioè si vedono
279809160509     C* ANCHE i dati fiscali storicizzat)
279810160509     C                   When      TpCallD15   = '1'        or
279811160509     C                             (TpCallD15   = ' '   and
279812160509     C                              D1TpDati    = '1'   and
279813160509     C                              D1DtFiVl    = *Zero    )
279814160509     C                   Eval      $Sottit     = Sottit1
279815160509     C*
279816160509     C* chiamata da dati fiscali storicizzati oppure
279817160509     C* chiamata da menu con richiesta di dati anagrafici di soggetti
279818160509     C* indicando una data di fine validità (cioè si vedono
279819160509     C* SOLO  i dati fiscali storicizzat)
279820160509     C                   When      TpCallD15   = '2'        or
279821160509     C                             (TpCallD15   = ' '   and
279822160509     C                              D1TpDati    = '1'   and
279823160509     C                              D1DtFiVl   <> *Zero    )
279824160509     C                   Eval      $Sottit     = Sottit2
279825160509     C*
279826160509     C* chiamata da estensione nominativi di partita oppure
279827160509     C* chiamata da menu con richiesta di nominiativi di partita
279828160509     C                   When      TpCallD15   = '3'        or
279829160509     C                             (TpCallD15   = ' '   and
279830160509     C                              D1TpDati    = '2'      )
279831160509     C                   Eval      $Sottit     = Sottit3
279832160509     C*
279833160509     C                   endSl
279834160509     C     $Sottit       IFNE      *BLANK
279835160509     C                   MOVEL     $Sottit       IDMSG
279836160509     C                   CALLB     'XRTVM'
279837160509     C                   PARM                    IDMSG            27
279838160509     C                   PARM                    TXTMSG          100
279839160509     C                   PARM                    ERRMSG            1
279840160509     C                   PARM      *ON           CTRMSG            1            centratura
279841160509     C                   PARM      30            LENMSG            3 0          lun output.
279842160509     C     ERRMSG        IFNE      *ON
279843160509     C                   MOVEL     TXTMSG        T1TIT
279844160509     C                   ELSE
279845160509     C                   MOVEL     *ALL'?'       T1TIT
279846160509     C                   ENDIF
279847160509     C                   ENDIF
279848160509     C*
279849160509     C                   ENDSR
279850160509     C/EJECT
279851160509     C************************************************************
279852160509     C* REPERIMENTO DATI SOCIETA'
279853160509     C************************************************************
279854160509     C     REPSOC        BEGSR
279855160509     C*
279900090930     C                   CALL      'SDGSOCDR'
280000090325     C                   PARM                    TIPXSC            6
280100090325     C                   PARM                    SOCXSC            3
280200090325     C                   PARM                    CDSXSC            9 0
280300090325     C                   PARM                    MODXSC            3
280400090325     C                   PARM      *BLANKS       RTNXSC            1
280500090325     C                   PARM                    XSOCDS
280600090325     C                   PARM                    KPJBA
280700090325     C*
280800090325     C                   ENDSR
280900090325     C/EJECT
281000090325     C************************************************************
281100090325     C* REPERIMENTO AUTORIZZAZIONI UTENTE
281200090325     C************************************************************
281300090325     C     REPAUT        BEGSR
281400090325     C*
281500090325     C                   CLEAR                   AUTDS
281600090325     C*
281700090325     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
281800090325     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
281900090325     C                   Z-ADD     0             XCAGRP                         GRUPPO
282000090325     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
282001160422     C                   MoveL     'ANGSOG'      XCAFNC                         FUNZIONE
282200090325    >C                   MOVEL     *ZERO         XCAVFU                         VARIABILE
282300090325    >C                   MOVE      'CK'          XCATCK                         TIPO CTRL
282400160422    >C                   MOVE      '1'           XCANAU                         RICH.XNOaut
282500090325     C*
282600090325     C                   CALLB     'XCHKAUT'
282700090325     C                   PARM                    AUTDS
282800090325     C*
282900090325     C* errori/non abilitazione
283000090325     C     XCARTN        IFNE      0
283100160422    >C                   MOVE      '2'           ErrD15
283200090325     C                   EXSR      ENDPGM
283300090325     C                   ELSE
283301160422     C                   MoveL     XCADDS        ANGSOGDS
283500090325     C                   ENDIF
283600090325     C*
283700090325     C                   ENDSR
283800090325     C/EJECT
283900090325     C************************************************************
284000090325     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
284100090325     C************************************************************
284200090325     C     REPDFT        BEGSR
284300090325     C*
284400090325     C* preparo i parametri di chiamata al driver
284500090325     C                   CLEAR                   XMPEDS
284600090325     C                   EVAL      TpChiamata = '01'
284700090325     C                   EVAL      NChiavi = 5
284800090325     C                   EVAL      VLogica = 2
284900090325     C*
285000090325     C* preparo le chiavi per la chiamata al driver
285100090325     C                   CLEAR                   XMPEDS1
285200090325     C                   MOVEL     XSCSOC        DSMPESOC
285300090325     C                   MOVEL     DSPGM         DSMPEPGM
285400090325     C                   MOVEL     KNMUS         DSMPEPRF
285500090325     C* Si reperiscono i dati di default a seconda delle modalità con
285600090325     C* cui il pgm è usato
285700090325     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
285800090325     C* MPETIP = 'PV'  pgm richiamato da menù
285900090325     C* MPETIP = 'PB'  x lancio Batch
286000090325     C                   SELECT
286100160422    >C     OPZD15        WHENEQ    '01'
286200090325     C                   MOVEL     'PX'          DSMPETIP
286300090325     C                   OTHER
286400090325     C                   MOVEL     'PV'          DSMPETIP
286500090325     C                   ENDSL
286600090325     C*
286700090325     C                   CALLB     'XMPE'
286800090325     C                   PARM                    XMPEDS
286900090325     C                   PARM                    XMPEDS1
287000090325     C*
287100090325     C     Risultato     IFEQ      '0'
287200090325     C                   MOVEL     DSMPEPAR      DSMPE
287300090325     C* nel caso sia la prima volta che l'utente apre questa anagrafica
287400090325     C* i suoi valori di default vengono decisi a pgm
287500090325     C                   ELSE
287600090325     C                   Z-ADD     1             $LOP
287700090325     C                   Z-ADD     1             $ORD
287800090325     C                   ENDIF
287900090325     C*
288000090325     C                   ENDSR
288100090325     C/EJECT
293100090325     C************************************************************
293200090325     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
293300090325     C************************************************************
293400090325     C     DEFOPZ        BEGSR
293500090325     C*
293600090325     C* Questa routine deve valorizzare la schiera $FL1
293700090325     C* con S o N a seconda della utilizzabilità o meno
293800090325     C* dell'opzione correlata
293900090325     C*
294000090325     C* Inizialmente si considerano tutte le opzioni come valide
294100090325     C     1             DO        $DIMOP        X
294200090325     C* Imposta tutti i flg delle opzioni non a *blank, a 'S'
294300090325     C                   If        $TX1(X)  <> *Blank
294400090325     C                   MOVE      'S'           $FL1(X)
294500090325     C                   Else
294600090325     C                   Leave
294700090325     C                   EndIf
294800090325     C                   ENDDO
294900090325     C*
295000090325     C* Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
295100090325     C* con 1 o 0 a seconda della utilizzabilità o meno
295200090325     C* del tasto funzione correlato
295300090325     C*
295400090325     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
295500090325     C* di compilazione
295600090325     C                   RESET                   $FC1
295700090325     C                   RESET                   $FR1
295800090325     C                   RESET                   $FC2
295900090325     C                   RESET                   $FR2
296000090325     C                   RESET                   $FC3
296100090325     C                   RESET                   $FR3
296200090325     C*
296300090325     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
296400090325     C* invalido alcune opzioni
296500090325     C                   SELECT
296600090325     C* Se pgm richiamato in ricerca disattivo :
296700160422    >C     OPZD15        WHENEQ    '01'
296800090325     C* F3=Fine
296900090325     C                   Z-ADD     3             X
297000090325     C                   MOVE      '0'           $FC1(X)
297100090325     C                   MOVE      '0'           $FC2(X)
297200090325     C                   MOVE      '0'           $FC3(X)
297300090325     C*
297400090325     c**<aggiungere nello stesso modo le altre opzioni da
297500090325      **<disattivare (quelle specifiche per ogni pgm)
297600090325     C*
297700090325     C* Se pgm richiamato in interrogazione disattivo :
297800160422    >C     OPZD15        WHENEQ    '05'
297900090325     C* F10=Immissione
298000090325     C                   Z-ADD     10            X
298100090325     C                   MOVE      '0'           $FC1(X)
298200090325     C                   MOVE      '0'           $FC2(X)
298300090325     C* 1=Selezione
298400090325     C                   Z-ADD     1             X
298500090325     C     01            LOOKUP    $OP1(X)                                21
298600090325     C     *IN21         IFEQ      *ON
298700090325     C                   MOVE      'N'           $FL1(X)
298800090325     C                   ENDIF
298900090325     C* 2=Modifica
299000090325     C                   Z-ADD     1             X
299100090325     C     02            LOOKUP    $OP1(X)                                21
299200090325     C     *IN21         IFEQ      *ON
299300090325     C                   MOVE      'N'           $FL1(X)
299400090325     C                   ENDIF
299500090325     C* 3=Copia
299600090325     C                   Z-ADD     1             X
299700090325     C     03            LOOKUP    $OP1(X)                                21
299800090325     C     *IN21         IFEQ      *ON
299900090325     C                   MOVE      'N'           $FL1(X)
300000090325     C                   ENDIF
300100090325     C* 4=Annullamento
300200090325     C                   Z-ADD     1             X
300300090325     C     04            LOOKUP    $OP1(X)                                21
300400090325     C     *IN21         IFEQ      *ON
300500090325     C                   MOVE      'N'           $FL1(X)
300600090325     C                   ENDIF
300700090325     C*
300800160422    >C     OPZD15        WHENEQ    *BLANKS
300801160509    >C     OPZD15        orEQ      '22'
300900090325     C* Se pgm non richiamato, disattivo subito :
301000090325     C* F8=Selezione seguente
301100090325     C                   Z-ADD     8             X
301200090325     C                   MOVE      '0'           $FC1(X)
301300090325     C                   MOVE      '0'           $FC2(X)
301400090325     C* 1=Selezione
301500090325     C                   Z-ADD     1             X
301600090325     C     01            LOOKUP    $OP1(X)                                21
301700090325     C     *IN21         IFEQ      *ON
301800090325     C                   MOVE      'N'           $FL1(X)
301900090325     C                   ENDIF
302000090325     C*
302100090325     C* In tutti gli altri casi, disattivo subito :
302200090325     C                   OTHER
302300090325     C* 1=Selezione
302400090325     C                   Z-ADD     1             X
302500090325     C     01            LOOKUP    $OP1(X)                                21
302600090325     C     *IN21         IFEQ      *ON
302700090325     C                   MOVE      'N'           $FL1(X)
302800090325     C                   ENDIF
302900090325     C*
303000090325     C                   ENDSL
303100090325     C*
303200090325     C* poi, disattivo a seconda delle autorizzazioni :
303300160422     C                   IF        ImmSOG    <>  *On
303500090325     C*
303600090325     C* F10=Immissione
303700090325     C                   Z-ADD     10            X
303800090325     C                   MOVE      '0'           $FC1(X)
303900090325     C                   MOVE      '0'           $FC2(X)
304000090325     C*
304100090325     C* 3=Copia
304200090325     C                   Z-ADD     1             X
304300090325     C     03            LOOKUP    $OP1(X)                                21
304400090325     C     *IN21         IFEQ      *ON
304500090325     C                   MOVE      'N'           $FL1(X)
304600090325     C                   ENDIF
304700090325     C*
304800090325     C                   ENDIF
304900090325     C*
304901160422     C                   IF        ManSOG    <>  *On
305200090325     C                   Z-ADD     1             X
305300090325     C     02            LOOKUP    $OP1(X)                                21
305400090325     C     *IN21         IFEQ      *ON
305500090325     C                   MOVE      'N'           $FL1(X)
305600090325     C                   ENDIF
305700090325     C                   ENDIF
305800090325     C*
305801160422     C                   IF        AnnSOG    <>  *On
306100090325     C                   Z-ADD     1             X
306200090325     C     04            LOOKUP    $OP1(X)                                21
306300090325     C     *IN21         IFEQ      *ON
306400090325     C                   MOVE      'N'           $FL1(X)
306500090325     C                   ENDIF
306600090325     C                   ENDIF
306700090325     C*
306701160422     C                   IF        VisSOG    <>  *On
307000090325     C                   Z-ADD     1             X
307100090325     C     05            LOOKUP    $OP1(X)                                21
307200090325     C     *IN21         IFEQ      *ON
307300090325     C                   MOVE      'N'           $FL1(X)
307400090325     C                   ENDIF
307500090325     C                   ENDIF
307600090325     C*
307700090325     C*
307800090325     C                   ENDSR
307900090325     C/EJECT
308000090325     C************************************************************
308100090325     C* REPERIMENTO OPZIONI
308200090325     C************************************************************
308300090325     C     REPOPZ        BEGSR
308400090325     C*
308500090325     C     1             DO        $DIMOP        X
308600090325     C*
308700090325     C     $MS1(X)       IFNE      *BLANK
308800090325     C*
308900090325     C                   MOVEL     $MS1(X)       IDMSG
309000090325     C                   CALLB     'XRTVM'
309100090325     C                   PARM                    IDMSG            27
309200090325     C                   PARM                    TXTMSG          100
309300090325     C                   PARM                    ERRMSG            1
309400090325     C*
309500090325     C     ERRMSG        IFNE      *ON
309600090325     C                   MOVEL     TXTMSG        $TX1(X)
309700090325     C                   ELSE
309800090325     C                   MOVEL     *ALL'?'       $TX1(X)
309900090325     C                   ENDIF
310000090325     C*
310100090325     C                   ENDIF
310200090325     C*
310300090325     C                   ENDDO
310400090325     C*
310500090325     C                   ENDSR
310600090325     C/EJECT
310700090325     C************************************************************
310800090325     C* REPERIMENTO TASTI DI FUNZIONE
310900090325     C************************************************************
311000090325     C     REPKEY        BEGSR
311001160506     C*
311100090325     C                   MOVEA     $FM1          $FM
311200090325     C                   MOVEA     $FC1          $FC
311300090325     C                   EXSR      REPKKK
311400090325     C                   MOVEA     $FX           $FX1
311500090325     C*
311600090325     C                   MOVEA     $FM2          $FM
311700090325     C                   MOVEA     $FC2          $FC
311800090325     C                   EXSR      REPKKK
311900090325     C                   MOVEA     $FX           $FX2
312000090325     C*
312100090325     C                   MOVEA     $FM3          $FM
312200090325     C                   MOVEA     $FC3          $FC
312300090325     C                   EXSR      REPKKK
312400090325     C                   MOVEA     $FX           $FX3
312500090325     C                   ENDSR
312600090325     C/EJECT
312700090325     C************************************************************
312800090325     C* REPERIMENTO TASTI DI FUNZIONE
312900090325     C************************************************************
313000090325     C     REPKKK        BEGSR
313100090325     C*
313200090325     C                   CLEAR                   $FX
313300090325     C     1             DO        24            X
313400090325     C*
313500090325     C     $FM(X)        IFNE      *BLANK
313600090325     C     $Fc(X)        andne     '0'
313700090325     C*
313800090325     C                   MOVEL     $FM(X)        IDMSG
313900090325     C                   CALLB     'XRTVM'
314000090325     C                   PARM                    IDMSG            27
314100090325     C                   PARM                    TXTMSG          100
314200090325     C                   PARM                    ERRMSG            1
314300090325     C*
314400090325     C     ERRMSG        IFNE      *ON
314500090325     C                   MOVEL     TXTMSG        $FX(X)
314600090325     C                   ELSE
314700090325     C                   MOVEL     *ALL' '       $FX(X)
314800090325     C                   ENDIF
314900090325     C*
315000090325     C                   ENDIF
315100090325     C*
315200090325     C                   ENDDO
315300090325     C*
315400090325     C                   ENDSR
315500090325     C/EJECT
315600090325     C************************************************************
315700090325     C* DEFINIZIONE KLIST
315800090325     C************************************************************
315900090325     C     DefKlist      BEGSR
316000090325     C*
316100090325     C* klist
316200090325     C*
316300160426     C* SDGSO3S01L
316400160426     C     K05SO301      KLIST
316500160426    >C                   KFLD                    SO3Sogg
316600160426    >C                   KFLD                    SO3DtFiVl
316601160426    >C                   KFLD                    SO3Sys
316602160505     C                   KFLD                    SO3NrAsInt
316603160505     C                   KFLD                    SO3Dest
316609160505     C*
316610160505     C* NDPPA04L
316611160505     C     K02PPA04      KLIST
316612160505     C                   KFLD                    PPASys
316613160505     C                   KFLD                    PPANrAsInt
317900090325     C*
318000090325     C                   ENDSR
318100090325     C/EJECT
318200090325** $MS1 / $TX1
318300090325PROMSG    *LIBL     OPZ0001
318400090325PROMSG    *LIBL     OPZ0002
318500090325PROMSG    *LIBL     OPZ0003
318600090325PROMSG    *LIBL     OPZ0004
318700090325PROMSG    *LIBL     OPZ0005
318900090325** $OP1 / $FL1
31900009032501S      <Selezione
31910009032502S      <Modifica
31920009032503S      <Copia
31930009032504S      <Annullamento
31940009032505S      <Visualizzazione
319600090325** TASTI DI FUNZIONE  D1 : $FM1 / $FX1
319700090325PROMSG    *LIBL     KEY0001  01
319800090325PROMSG    *LIBL     KEY0002  02
319900090325PROMSG    *LIBL     KEY0003  03
320000090325PROMSG    *LIBL     KEY0004  04
320100090325PROMSG    *LIBL     KEY0005  05
320200090325PROMSG    *LIBL     KEY0006  06
320300090325PROMSG    *LIBL     KEY0007  07
320400090325PROMSG    *LIBL     KEY0008  08
320500090325PROMSG    *LIBL     KEY0009  09
320600090325PROMSG    *LIBL     KEY0010  10
320700090325PROMSG    *LIBL     KEY0011  11
320800090325PROMSG    *LIBL     KEY0012  12
320900090325PROMSG    *LIBL     KEY0013  13
321000090325PROMSG    *LIBL     KEY0014  14
321100090325PROMSG    *LIBL     KEY0015  15
321200090325PROMSG    *LIBL     KEY0016  16
321300090930SDGMSG    *LIBL     SDK0001  17
321400090325PROMSG    *LIBL     KEY0018  18
321500090325PROMSG    *LIBL     KEY0019  19
321600090325PROMSG    *LIBL     KEY0020  20
321700090325PROMSG    *LIBL     KEY0021  21
321800090325PROMSG    *LIBL     KEY0022  22
321900090325PROMSG    *LIBL     KEY0023  23
322000090325PROMSG    *LIBL     KEY0024  24
322100090325**  ABILITAZIONE D1 : $FC1 / $FR1
3222000903250N  01
3223000903251N  02
3224000903251N  03
3225000903251N  04
3226000903250N  05
3227000903250N  06
3228000903250N  07
3229000903251N  08
3230000903250N  09
3231000903251N  10
3232000903250N  11
3233000904021N  12
3234000903250N  13
3235000903250N  14
3236000903250N  15
3237000903250N  16
3238000903250N  17
3239001606100N  18
3240000903250N  19
3241000903250N  20
3242000903250N  21
3243000903250N  22
3244000903250N  23   Impostare a '1' se si utilizzano opzioni
3245000903251N  24   Lasciare sempre impostata a '1' il valore della riga 24
324600090325** TASTI DI FUNZIONE  S1 : $FM2 / $FX2
324700090325PROMSG    *LIBL     KEY0001  01
324800090325PROMSG    *LIBL     KEY0002  02
324900090325PROMSG    *LIBL     KEY0003  03
325000090325PROMSG    *LIBL     KEY0004  04
325100090325PROMSG    *LIBL     KEY0005  05
325200090325PROMSG    *LIBL     KEY0006  06
325300090325PROMSG    *LIBL     KEY0007  07
325400090325PROMSG    *LIBL     KEY0008  08
325500090325PROMSG    *LIBL     KEY0009  09
325600090325PROMSG    *LIBL     KEY0010  10
325700090325PROMSG    *LIBL     KEY0011  11
325800090325PROMSG    *LIBL     KEY0012  12
325900090325PROMSG    *LIBL     KEY0013  13
326000090325PROMSG    *LIBL     KEY0014  14
326100090325PROMSG    *LIBL     KEY0015  15
326200090325PROMSG    *LIBL     KEY0016  16
326300090930SDGMSG    *LIBL     SDK0001  17
326400090325PROMSG    *LIBL     KEY0018  18
326500090325PROMSG    *LIBL     KEY0019  19
326600090325PROMSG    *LIBL     KEY0020  20
326700090325PROMSG    *LIBL     KEY0021  21
326800090325PROMSG    *LIBL     KEY0022  22
326900090325PROMSG    *LIBL     KEY0023  23
327000090325PROMSG    *LIBL     KEY0024  24
327100090325**  ABILITAZIONE S1 : $FC2 / $FR2
3272000903250N  01
3273000903250N  02
3274000903251N  03
3275000903250N  04
3276000903250N  05
3277000903250N  06
3278000903250N  07
3279000903251N  08
3280001605061N  09
3281000903251N  10
3282000903250N  11
3283000903251N  12
3284000903250N  13
3285000903250N  14
3286000903250N  15
3287000903250N  16
3288000903251N  17
3289001605090N  18
3290000903250N  19
3291000903250N  20
3292000903250N  21
3293000903250N  22
3294000903251N  23   Impostare a '1' se si utilizzano opzioni
3295000903251N  24   Lasciare sempre impostata a '1' il valore della riga 24
329600090325** TASTI DI FUNZIONE  D3 : $FM3 / $FX3
329700090325PROMSG    *LIBL     KEY0001  01
329800090325PROMSG    *LIBL     KEY0002  02
329900090325PROMSG    *LIBL     KEY0003  03
330000090325PROMSG    *LIBL     KEY0004  04
330100090325PROMSG    *LIBL     KEY0005  05
330200090325PROMSG    *LIBL     KEY0006  06
330300090325PROMSG    *LIBL     KEY0007  07
330400090325PROMSG    *LIBL     KEY0008  08
330500090325PROMSG    *LIBL     KEY0009  09
330600090325PROMSG    *LIBL     KEY0010  10
330700090325PROMSG    *LIBL     KEY0011  11
330800090325PROMSG    *LIBL     KEY0012  12
330900090325PROMSG    *LIBL     KEY0013  13
331000090325PROMSG    *LIBL     KEY0014  14
331100090325PROMSG    *LIBL     KEY0015  15
331200090325PROMSG    *LIBL     KEY0016  16
331300090325PROMSG    *LIBL     KEY0017  17
331400090325PROMSG    *LIBL     KEY0018  18
331500090325PROMSG    *LIBL     KEY0019  19
331600090325PROMSG    *LIBL     KEY0020  20
331700090325PROMSG    *LIBL     KEY0021  21
331800090325PROMSG    *LIBL     KEY0022  22
331900090325PROMSG    *LIBL     KEY0023  23
332000090325PROMSG    *LIBL     KEY0024  24
332100090325**  ABILITAZIONE D3 : $FC3 / $FR3
3322000903250N  01
3323000903250N  02
3324000903251N  03
3325000903250N  04
3326000903250N  05
3327000903250N  06
3328000903250N  07
3329000903250N  08
3330000903250N  09
3331000903250N  10
3332000903250N  11
3333000903251N  12
3334000903250N  13
3335000903250N  14
3336000903250N  15
3337000903250N  16
3338000903250N  17
3339000903250N  18
3340000903250N  19
3341000903250N  20
3342000903250N  21
3343000903250N  22
3344000904020N  23   Impostare a '1' se si utilizzano opzioni
3345000903251N  24   Lasciare sempre impostata a '1' il valore della riga 24
