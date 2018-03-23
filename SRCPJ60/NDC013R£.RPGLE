000100981214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(pnota)
000200040227D1535H*PARMS BNDDIR(PJXBND PJCBND NDC001BND)
000300970821     H*PARMS CVTOPT(*NONE)
000400970821     H DECEDIT('0,') DATEDIT(*DMY.)
000500040227D1535 ***********************************************
000600040227  "   * aggiunto nelle H*PARMS la BNDDIR: NDC001BND *
000700040227D1535 ***********************************************
000800970821      *
000900970821     >*            GUIDA GESTIONE OPERAZIONI CONTABILI
001000970821      *           -------------------------------------
001100970821      *
001200040406C1742 ***********************************************
001300040407  "   * PROMEMORIA FUNZIONALITÀ TECNICA
001400040406  "   *
001500040407  "   * - L'F13=Selezione multipla, ripete l'opzione solo se la riga
001600040407  "   *   ha le caratteristiche giuste per l'applicazione dell'opz.
001700040406  "   *
001800040406  "   * - Presupposti necessari di validità delle opzioni ad oggi:
001900040406  "   *   - Opz.22= passaggio a definitivo
002000040406  "   *     NON possibile su registrazioni:
002100040406  "   *     - di emissione portafoglio
002200040406  "   *     - con blocco riconciliazione
002300040406  "   *     - di tipo Iva
002400040406  "   *     - di tipo Definitive
002500060403C1742 *     - non c'è l'autorizzazione su CONPNO
002600060403C1921 *     - reg intercompany raccordata e/o con un mov riconciliato
002700060403C1742 *   - Opz.8=Modifica data registrazione
002800040406  "   *     NON possibile su registrazioni:
002900040406  "   *     - di emissione portafoglio
003000040406  "   *     - di tipo Iva
003100040406  "   *     - di tipo Definitive
003200040406  "   *     - reg collegate create con autofatture
003300040406  "   *   - Opz.4=Cancella
003400040406  "   *     NON possibile su registrazioni:
003500040406  "   *     - di emissione portafoglio
003600040406  "   *     - reg collegate create con autofatture
003700040406C1742 ***********************************************
003800970821      *
003900970821      * DOCUMENTI MICROANALISI :
004000970821      *
004100970821      * NOTE TECNICHE :
004200970821      *
004300970821      *
004400970821      * PERSONALIZZAZIONI
004500970821      *
004600990812      *----------------------------------------------------------------
004700990812      * C1212 Aggiunto il controllo che l'utente sia abilitato
004800990812      *       nell'ambito dell'autorizzazione all'unità,
004900990812      *       al tipo registrazione in parzializzazione, se richiesta
005000990812      *----------------------------------------------------------------
005100970821      *
005200970821      *
005300970821      *               MAPPA INDICATORI
005400970821      *
005500970821      *  21           GENERICO OPERAZIONI I/O
005600970821      *  22           GENERICO ERRORE OPERAZIONI I/O
005700970821      *  30           SFLDSP
005800970821      * N31           SFLCLR
005900970821      *  31           SFLDSPCTL
006000970821      *  32           SFLNXTCHG
006100970821      *  33           SFLEND
006200970821      *  39           OF PRTF
006300970821      *  40 <---> 49  DSPATR ERRORI SU SFL
006400970821      *  50 <---> 89  ERROSI SU VIDEO
006500970821      *  elenco errori speciali:
006600970821      *  90           REGISTRAZIONE GIA' ESISTENTE PER IMMISSIONE O COPIA
006700970821      *  91           OPZIONE NON VALIDA
006800970821      *  92           IMPOSSIBILE RIPOSIZIONAMENTO CON OPZIONI PRESENTI SU LISTA
006900970821      *  93           LA FUNZIONE RICHIAMATA SI E' CHIUSA IN ERRORE
007000970821      *  95           L'OPERAZIONE DI SCRITTURA NON E' ANDATA A BUON FINE
007100970821      *  94           UTENTE NON AUTORIZZATO ALLA FUNZIONE NEL MODO DESIDERATO
007200970821      *  96           ESISTONO ERRORI IN ALTRI SCHERMI
007300970821      *  97           TASTO   NON ABILITATO
007400970821      *  98           RICERCA NON ABIL. NELLA POSIZ.
007500970821      *  99           INDIC. GENERALE DI ERRORE
007600970821     F*----------------------------------------------------*
007700040308     F*----------------------------------------------------------------
007800040308     F* Viste logiche utilizzate per i diversi ordinamenti
007900040308     F*----------------------------------------------------------------
008000040308      * eventuali altre viste logiche a seconda delle parzializzazioni
008100040308     F*------------
008200040308     FNdReg13L  IF   E           K DISK
008300040308     F                                     Rename(NdReg000:NdReg13)
008400130618    >FNdReg99L  IF   E           K DISK
008500130618     F                                     Rename(NdReg000:NdReg99)
008600040308     FNdReg04L  IF   E           K DISK
008700040308     F                                     Rename(NdReg000:NdReg04)
008800040308     FNdReg01L  IF   E           K DISK
008900040308     F                                     Rename(NdReg000:NdReg01)
009000040308     F*------------
009100040308     f* controllo validità dell'unità
009200040308     FAnUni01L  IF   E           K DISK
009300040308     F*------------
009400040308     F* Riepilogo registrazioni cancellate
009500040308     FNdc013P1  O    E             PRINTER OFLIND(*IN39)
009600040308     F*------------
009700040308     F* Riepilogo registrazioni modificate
009800040308     FNdc013P2  O    E             PRINTER OFLIND(*IN38)
009900040308     F*------------
010000040308     FNdc013D   CF   E             WORKSTN
010100040308     F                                     RENAME(C013D1:FMTD1)
010200040308     F                                     RENAME(C013D2:FMTD2)
010300040308     F                                     RENAME(C013D3:FMTD3)
010400040308     F                                     RENAME(C013S1:FMTS1)
010500040308     F                                     RENAME(C013C1:FMTC1)
010600040308     F                                     RENAME(C013T1:FMTT1)
010700040308     F                                     RENAME(C013Z1:FMTZ1)
010800040308     F                                     RENAME(C013Z2:FMTZ2)
010900040308     F                                     RENAME(C013Z3:FMTZ3)
011000040308     F                                     RENAME(C013D5:FMTD5)
011100040308     F                                     RENAME(C013Z5:FMTZ5)
011200040308     F                                     RENAME(C013D6:FMTD6)
011300040308     F                                     RENAME(C013Z6:FMTZ6)
011400040308     F                                     SFILE(FMTS1:S1NRR)
011500040308     F                                     INFDS(DSFMT)
011600040308     D*----------------------------------------------------*
011700040308     D* Schiere per la gestione delle opzioni
011800040308     D*
011900040308     D* dimensione della schiera $MS1, da non cambiare
012000040308     D $DIMOP          C                   CONST(30)
012100040308     D $MS1            S             27    DIM($dimop) CTDATA PERRCD(1)
012200040308     D $TX1            S             50    DIM($dimop) ALT($MS1)
012300040308     D $OP1            S              2  0 DIM($dimop) CTDATA PERRCD(1)
012400040308     D $FL1            S              1    DIM($dimop) ALT($OP1)
012500040308     D* schiera contenente i nomi dei programma richiamati attraverso
012600040308     D* le opzioni personalizzate
012700040308     D $PGO1           S             10    DIM($dimop)
012800040308     D $TX             S             50    DIM(50)
012900040308     D $FL             S              1    DIM(50)
013000970821     D* Schiere generali per la gestione dei tasti di funzione
013100970821     D $FM             S             27    DIM(24)
013200970821     D $FX             S             50    DIM(24)
013300970821     D $FC             S              1    DIM(24)
013400970821     D $FR             S              1    DIM(24)
013500970821     D* Schiere per la gestione dei tasti di funzione
013600970821     D* Video D1
013700970821     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
013800970821     D $FX1            S             50    DIM(24) ALT($FM1)
013900970821     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
014000970821     D $FR1            S              1    DIM(24) ALT($FC1)
014100970821     D* Schiera contente i nomi dei programmi da richiamare con i tasti
014200970821     D* funzione personalizzata S1
014300970821     D $PGF2           S             10    DIM(25)
014400970821     D* Schiere per la gestione dei tasti di funzione
014500970821     D* Video S1
014600970821     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
014700970821     D $FX2            S             50    DIM(24) ALT($FM2)
014800970821     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
014900970821     D $FR2            S              1    DIM(24) ALT($FC2)
015000970821     D* Schiere per la gestione dei tasti di funzione
015100970821     D* Video D2
015200970821     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
015300970821     D $FX3            S             50    DIM(24) ALT($FM3)
015400970821     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
015500970821     D $FR3            S              1    DIM(24) ALT($FC3)
015600970821     D*
015700970827     D* Video D5
015800970827     D $FM5            S             27    DIM(24) CTDATA PERRCD(1)
015900970827     D $FX5            S             50    DIM(24) ALT($FM5)
016000970827     D $FC5            S              1    DIM(24) CTDATA PERRCD(1)
016100970827     D $FR5            S              1    DIM(24) ALT($FC5)
016200970902     D*
016300970902     D* Video D6
016400970902     D $FM6            S             27    DIM(24) CTDATA PERRCD(1)
016500970902     D $FX6            S             50    DIM(24) ALT($FM6)
016600970902     D $FC6            S              1    DIM(24) CTDATA PERRCD(1)
016700970902     D $FR6            S              1    DIM(24) ALT($FC6)
016800970827     D*
016900970821     D $D1L01          S              3  0
017000970821     D $D1L02          S              3  0
017100970821     D $S1L01          S              3  0
017200970821     D $S1L02          S              3  0
017300970821     D $C1Op1          S              3  0
017400970821     D $C1Op2          S              3  0
017500970821     D $D2L01          S              3  0
017600970821     D $D2L02          S              3  0
017700970827     D $D5L01          S              3  0
017800970827     D $D5L02          S              3  0
017900970903     D $D6L01          S              3  0
018000970903     D $D6L02          S              3  0
018100970821    > * Inserire qui la definizione di altre variabili per reperire
018200970821    > * la lunghezza di altre videate che usano un piede diverso da
018300970821    > * STD
018400970821     D*
018500970821     D*-------------
018600970821     D* Indice della schiera tasti funzione
018700970821     D PosFun          S              2  0
018800970821     D/EJECT
018900970821     D*----------------------------------------------------*
019000970821     D*-------------
019100970821     D* Valori assunti
019200970821     D XMPEDS        E DS                  INZ
019300970821     D XMPEDS1       E DS                  INZ
019400970821     D DSMPE           DS                  INZ
019500970821     D  $LOP                   1      1  0
019600970821     D  $ORD                   2      3  0
019700970821    >D**$DFn                   4     xx
019800970821     D*-------------
019900970821     D* Passaggio Parametri
020000970821     D KPJBA         E DS
020100970821     D*-------------
020200970821     D* Reperimento Autorizzazioni
020300970821     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
020400970821     D*-------------
020500970821     D* Passaggio Autorizzazioni
020600970821     D DSAUT           DS           512
020700970821     D*-------------
020800970821     D* Maschera dati autorizzazioni
020900040308     D ConPNoDS      E DS
021000970821     D*-------------
021100970821     D* Dati di ambiente ottenuti da XSOC
021200970821     D SOC001        E DS                  EXTNAME(XSOC001DS)
021300970822     D    FgO1                        1    Overlay(XscFgO)
021400030220C1627D    XScLaguna                   1    overlay(XScSkp:8)                    Scadenz. retroattivo
021500970821     D*-------------
021600970821     D* DS Interna per dati di output di XSOC
021700970821     D XSOCDS          DS          1000
021800970821     D*-------------
021900970821     D* Parametri in ricezione
022000040308     D Ndc013DS      E DS
022100970821     D*-------------
022200970821     D* Parametri del PGM di manutenzione Ndc014R
022300040308     X*Ndc014DS      E DS                  inz
022400040308     X**RET014       E                     INZ('0')
022500040308     X**OPR014       E                     INZ('0')
022600040308     X**ERR014       E                     INZ('0')
022700970822     D*-------------
022800970822     D* Contollo autorizzazione contabilità
022900970822     D XAutCtbDs     E DS
023000970822     D*-------------
023100970822     D* Autorizzazioni alla unità
023200970822     D XAutUniDs     E DS
023300990805C1212D XAutUniDS1    E DS
023400970825     D*-------------
023500970825     D* Autorizzazioni alla gestione delle registrazioni
023600970827     D XAutRegDS     E DS                  Inz
023700970822     D*-------------
023800040308     D* Tabella per ricerca e controllo della contabilità
023900040308     D AngG84Ds      E DS
024000040308     D*-------------
024100040308     D* Tabella per ricerca e controllo della classe
024200040308     D AngG81Ds      E DS
024300970822     D*-------------
024400040308     D* Tabella per ricerca e controllo della serie
024500040308     D AngG13Ds      E DS
024600040308     D*-------------
024700040308     D* Controlli validità legende
024800040308     D X07ValDs      E DS
024900040308     D*-------------
025000040308     D* Ricerca legende
025100040308     D Ana964Ds      E DS
025200040308     D*-------------
025300040308     D* Ctrl valori su ANATB
025400040308     D XAtbDs        E Ds
025500970822     D*-------------
025600970822     D* Ricerca su tabelle
025700970822     D XTabDs        E Ds
025800970825     D  XTaErr       E                     Inz('0')
025900970822     D*-------------
026000970822    >D* ricerca unità
026100040308     D Ana009Ds      E DS
026200040308     D  Opz009       E                     Inz('0')
026300040308     D  Ret009       E                     Inz('0')
026400040308     D  Opr009       E                     Inz('0')
026500040308     D  Err009       E                     Inz('0')
026600040308     D*-------------
026700040308     D* Ricerca causale principale
026800040308     D Ana070Ds      E DS                  Inz
026900040308     D  Ret070       E                     Inz('0')
027000040308     D  Opr070       E                     Inz('0')
027100040308     D  Cmt070       E                     Inz('0')
027200040308     D  Err070       E                     Inz('0')
027300040308     D  Fsc070       E                     Inz('S')
027400040308     D*-------------
027500040308     D* Ds campi OPE per controllo validità causale contabile
027600040308     D AnOpe         E DS                  inz ExtName(ANOPE00f)
027700040308     D*-------------
027800970825     D* Parametri per gestione p.nota interattiva
027900970825     D Ndc001Ds      E DS                  Inz
028000970825     D  Ret001       E                     Inz('0')
028100970825     D  Opr001       E                     Inz('0')
028200970825     D  Err001       E                     Inz('0')
028300970828     D*-------------
028400970828     D* DS Esterna gestione messaggi
028500970828     D XMSGDS        E DS
028600970828     D  STS          E                     EXTFLD(MSGSTS)
028700970828     D                                     DIM(333)
028800970828     D  JBA          E                     EXTFLD(MSGJBA)
028900970828     D                                     DIM(502)
029000000128C1315 *-------------
029100000128  "   * Schiera per XMSGR
029200000128  "  D $MsgID          S             27    DIM(3) CTDATA PERRCD(1)
029300000128  "   *-------------
029400000128  "  D RifReg          ds                  inz
029500000128  "  D   GGReg                        2a
029600000128  "  D                                1a   inz('/')
029700000128  "  D   MMReg                        2a
029800000128  "  D                                1a   inz('/')
029900000128  "  D   AAReg                        2a
030000000128  "  D   NrReg                        9p 0
030100000128  "  D   SerReg                       4a
030200000128  "   *-------------
030300000128  "  D GGMMAA          ds
030400000128  "  D  GG                            2s 0
030500000128  "  D  MM                            2s 0
030600000128  "  D  AA                            2s 0
030700000128  "   *-------------
030800000128  "  D $PrimoXMsg      S              1A
030900000128C1315 *-------------
031000970822     D* Definizioni parametri standard chiamate moduli
031100970822     D Esito           S              1
031200970822     D GesMsg          S              1
031300970822     D Lenout          S              9B 0
031400970822     D StrutturaO      S             10
031500970822     D Uso             S              1
031600970822     D OutPut          S           4000
031700970822     D Refresh         S              1
031800970825     D Comit           S              1
031900970825    >D Operazione      S              2
032000970825     D TpRegz          S              1
032100970822     D*-------------
032200970821     D* campo di salvataggio per la DS d'ingresso durante la chiamata
032300970821     D* alla funzione personalizzata
032400970821    >D $SaveDS         S                   like(Ndc013DS)
032500970821     D*-------------
032600970821     D DSFMT           DS           512
032700970821     D  $TASTO               369    369
032800970821     D  NRG                  370    370
032900970821     D  NCL                  371    371
033000970821     D  SFLNRR               378    379B 0
033100970821     D*-------------
033200970821     D* posizione cursore
033300970821     D CURSOR          DS
033400970821     D  RIRI                   1      2B 0 INZ
033500970821     D  R$$                    2      2
033600970821     D  COCO                   3      4B 0 INZ
033700970821     D  C$$                    4      4
033800970821     D*-------------
033900970821     D* Reperimento nome PGM
034000970821     D STATUS         SDS           333
034100970821     D  DSPGM            *PROC
034200970821     D  PARMS            *PARMS
034300970821     D*-------------
034400970821     D* nr.di righe del sfl (SFLPAG)
034500970829     D* lo imposto = 8 in InzVar e temporaneam. = 9999 in F13S1
034600970827     D SFLPAG          S              4S 0
034700970821     D*-------------
034800970827     D* Costanti
034900970821     D* Tasti di funzione
035000970821     D F01             C                   CONST(X'31')
035100970821     D F02             C                   CONST(X'32')
035200970821     D F03             C                   CONST(X'33')
035300970821     D F04             C                   CONST(X'34')
035400970821     D F05             C                   CONST(X'35')
035500970821     D F06             C                   CONST(X'36')
035600970821     D F07             C                   CONST(X'37')
035700970821     D F08             C                   CONST(X'38')
035800970821     D F09             C                   CONST(X'39')
035900970821     D F10             C                   CONST(X'3A')
036000970821     D F11             C                   CONST(X'3B')
036100970821     D F12             C                   CONST(X'3C')
036200970821     D F13             C                   CONST(X'B1')
036300970821     D F14             C                   CONST(X'B2')
036400970821     D F15             C                   CONST(X'B3')
036500970821     D F16             C                   CONST(X'B4')
036600970821     D F17             C                   CONST(X'B5')
036700970821     D F18             C                   CONST(X'B6')
036800970821     D F19             C                   CONST(X'B7')
036900970821     D F20             C                   CONST(X'B8')
037000970821     D F21             C                   CONST(X'B9')
037100970821     D F22             C                   CONST(X'BA')
037200970821     D F23             C                   CONST(X'BB')
037300970821     D F24             C                   CONST(X'BC')
037400970821     D ENTER           C                   CONST(X'F1')
037500970821     D ROLDWN          C                   CONST(X'F4')
037600970821     D ROLLUP          C                   CONST(X'F5')
037700970825     D*-------------
037800970825     D* ATTRIBUTI VIDEO
037900970825     D* (definisco come costanti quegli attributi video che potrei
038000970825     D*  testare e quindi preferisco assegnargli un valore "parlante")
038100970825     D*  bottone presente
038200970825     D PRES            C                   CONST(X'A1')
038300970821     D*-------------
038400970821     D* Sottotitolo di testata
038500970821    > * se non si hanno sottotitoli, mettere tutta la costante = ' '
038600970821     D SOTTIT          C                   CONST(' ')
038700970821     D*-------------
038800970821     D DATMIN          C                   CONST('0001-01-01')
038900970821     D DATMAX          C                   CONST('9999-12-31')
039000970821     D*-------------
039100970821     D* Variabili per gestione videate
039200970821     D $Gest           S              2A
039300970821     D $Fine           S              1A
039400970821     D $InzD1          S              1A
039500970821     D $InzS1          S              1A
039600970821     D $InzD2          S              1A
039700970827     D $InzD5          S              1A
039800970903     D $InzD6          S              1A
039900970821     D $LastG          S              2A
040000970821     D $LastV          S              2A
040100970821     D $EndFile        S              1A
040200970821     D $Esci           S              1A
040300970821     D $RcdOK          S              1A
040400970821     D $Unique         S              1A
040500970821     D*-------------
040600970821     D* Variabili appoggio sempre presenti in tutte le anagrafiche
040700970821     D S1Nrr           S                   like(C1Rcd)
040800970821     D WSfl            S                   like(C1Nrr)
040900970821     D WMax            S                   like(C1Rcd)
041000970821     D WPag            S                   like(C1Rcd)
041100970821     D WInzS1          S                   like($InzS1)
041200040308     D WOpr            S                   like(Opr013)
041300970821     D CurR            S              2S 0
041400970821     D CurC            S              2S 0
041500970821     D Win             S             99A
041600970821     D W1Pos           S                   like(C1Pos)
041700970821     D WINKR           S              1A
041800970821     D*-------------
041900970821     D* Variabili appoggio specifiche della singola anagrafica
042000970821     D*  Indici
042100970821     D X               S              3S 0
042200970821     D Y               S              3S 0
042300970822     D*-------------
042400970822     D* controllo esistenza causale
042500970825     D WCausale        S                   Like(RegCausTes)
042600970825     D* controllo date di registrazione
042700970825     D WDtRegDa        S               D
042800970825     D WDtRegA         S               D
042900970822     D*-------------
043000970822     D*Posizionamento
043100980219     D W1PosDt         S                   like(C1PosDt )
043200970826     D WPosData        S               D
043300970916     D*-------------
043400970916     D*salvataggio tipo registrazione per emissione opzioni abilitate
043500970916     D WTpRegz         S                   Like(D1TpRegz)
043600971217     D WDocIVA         S                   Like(D1DocIVA)
043700970827     D*-------------
043800970828     D*Caricamento nuova data di registrazione o cancellazione registrazioni
043900970828     D Conferma        S              1A
044000970828     D OpenWdw         S              1A
044100970903     D Conferma6       S              1A
044200970903     D OpenWdw6        S              1A
044300970917     D* Definisco un puntatore per la data da modificare in quanto nelle 2
044400970917     D* chiamate al modulo Mvc119 una volta deve essere omessa dalla parm
044500970917     D* list e l'altra deve essere valorizzata con quella impostata a video
044600970917     D Data1           S               D
044700970917     D Data119         S                   Like(Data1) Based(PData119)
044800970917     D* Definisco un puntatore per il tipo registrazione in quanto nelle 2
044900970917     D* chiamate al modulo Mvc119 una volta deve essere omesso dalla parm
045000970917     D* list e l'altra deve essere valorizzato con 'D'.
045100970917     D TpRegz1         S              1A
045200970917     D TpRegz119       S                   Like(TpRegz1) Based(PTpRegz119)
045300970917     D*
045400970903     D NewDtReg        S               D
045500970827     D WNewDtReg       S               D
045600970903     D NewDtReg6       S               D
045700970903     D WNewDtReg6      S               D
045800970902     D OldDtReg        S               D
045900970902     D OldTpReg        S              1A
046000970901     D UnaRiga1        S              1A
046100970901     D UnaRiga2        S              1A
046200970901     D ErroreMod       S              1A
046300970828     D ErroreCanc      S              1A
046400970828     D OpenMsg         S              1A
046500980219     D OpzOk           S              1A
046600970828     D UpdDate         S               D   DatFmt(*Mdy)
046700970822     D*-------------
046800970821     D* Indice della schiera per le opzioni personalizzate
046900970821     D PosOpz          S              2  0
047000970821     D* numero max opzioni
047100970821     D NumOpz          S              2  0 Inz($DIMOP)
047200970821     D* numero effettive opzioni
047300970821     D OpzLen          S              2  0
047400970821     D*-------------
047500000117C1309D ZDateISO        S               D
047600021014C1595D* Attributi video
047700021014C1595D Normale         C                   Const(X'32')
047800021014C1595D Protetto        C                   Const(X'A0')
047900030220C1627 *-------------
048000030220  "   * dati catalogazione Laguna
048100030220  "  D ANLGN00F      E DS
048200030220  "   *-------------
048300030220  "   * Parametri driver Laguna
048400030220  "  D Opzione         S              2A
048500030220  "  D Errore          S              1A
048600030220  "  D Vista           S              2S 0
048700030220  "  D Dati            S           4000A
048800030220  "  D Lunghezza       S              9B 0
048900030220  "  D Commit          S              1A
049000030220  "  D NrChiavi        S              2S 0
049100030220  "  D Alloca          S              1A
049200030220  "  D DataISO         S               D
049300030220  "   *-------------
049400030220  "   * Movimenti contabili
049500030220  "  D NDMOV00F      E DS                  inz
049600030220  "   *-------------
049700030220  "   * Parametri driver ndmov
049800030220  "  D Codoper         s              1A
049900030220  "  D NrrigaM         s              5  0
050000030220  "   *-------------
050100030220  "   * Output modulo controlla conto
050200030220  "  D ND002DS       E DS
050300030226  "   *-------------
050400030226  "   * Variabile globale
050500030226  "   * (ndmv116 ha bisogno di knsif per decatalogare in laguna)
050600030226  "  D £Kpjba          s                   Import like(Kpjba)
050700030226C1627 *
050800040227D1535 *-------------
050900040227  "   * variabili globali per gestire passaggio a definitivo
051000040227  "   * solo se non esiste un mov. vincolato da blocco di riconciliaz
051100060427D1535 * parametri ricevuti
051200060331C1921X*£BlkTes369      s              1                        import
051300060331C1921D £Esito369       s              1                        import
051400060331D1535 * parametri passati al modulo
051500060331C1921D £Operaz369      s              1                        import
051600060331C1921D £Societa369     s                   like(REGSocieta)    import
051700060331D1535D £Sys369         s                   like(REGSys)        import
051800040227D1535D £NrAsReg369     s                   like(REGNrAsReg)    import
051900970821     C*----------------------------------------------------*
052000970821     C*                MAIN LINE PROGRAM
052100970821     C*----------------------------------------------------*
052200970821     C*
052300970821     C* inizializzazione variabili
052400970821     C                   EXSR      INZVAR
052500970821     C*
052600970821     C     $FINE         DOWEQ     *OFF
052700970821     C     $GEST         CASEQ     'D1'          GESD1                          Parzializz.
052800970821     C     $GEST         CASEQ     'S1'          GESS1                          Lista
052900970821     C     $GEST         CASEQ     'D2'          GESD2                          Valori ass.
053000970821     C                   ENDCS
053100970821     C                   ENDDO
053200970821     C* fine programma
053300970821     C                   EXSR      ENDPGM
053400970821     C*
053500970821     C/EJECT
053600970821     C************************************************************
053700970821     C* FINE PROGRAMMA
053800970821     C************************************************************
053900970821     C     ENDPGM        BEGSR
054000970821     C*
054100970821     C* passaggio dati al pgm chiamante
054200040308     C                   MOVEL     WOPR          OPR013                                     ne S1
054300040308     C                   MOVEL     Ndc013DS      KPJBU
054400970821     C*
054500970821     C* Se pgm Guida non è stato richiamato posso chiudere in LR
054600040308     C     OPZ013        IFEQ      *BLANK
054700970821     C                   SETON                                            LR
054800970821     C* altrimenti chiudo in RT
054900970821     C                   ELSE
055000970821     C                   SETON                                            RT
055100970821     C                   ENDIF
055200970821     C                   RETURN
055300970821     C*
055400970821     C                   ENDSR
055500970821     C/EJECT
055600970821     C************************************************************
055700970821     C* RICERCHE
055800970821     C************************************************************
055900970821     C     SEARCH        BEGSR
056000970821     C*
056100970821     C* determino Riga/Colonna del cursore
056200970821     C                   MOVE      NRG           R$$
056300970821     C                   MOVE      NCL           C$$
056400970821     C                   Z-ADD     RIRI          CURR
056500970821     C                   Z-ADD     COCO          CURC
056600970821     C*
056700040308     C                   SELECT
056800970821     C*
056900970822    > *--------------------------------------------------------------
057000970822    > * inserire le routine di ricerca
057100970822    > * RICORDATE: per tutti i campi che in certe condizioni risultano PR,
057200970822    > * NON deve essere possibile richiamare i rispettivi pgm in ricerca,
057300970822    > * SEMMAI richiamarli in visualizzazione (opzione=05)
057400970822    > *--------------------------------------------------------------
057500970822     C*
057600970822     C* FMTD1 - Contabilità
057700970822     C     H1NMFL        Wheneq    'D1CTB'
057800970822     C                   Reset                   XTABDS
057900970822     C                   Move      '01'          XTAOPZ
058000970822     C                   Move      XScSoc        XTAAZI
058100970822     C                   Move      XScLin        XTALIN
058200970822     C                   Z-Add     CURR          XTARIG
058300970822     C                   Z-Add     CURC          XTACOL
058400970822     C                   Clear                   KPJBU
058500970822     C                   Movel     XTabDs        KPJBU
058600970822     C                   Call      'ANGG84R'
058700970822     C                   Parm                    KPJBA
058800970822     C                   Movel     KPJBU         XTabDs
058900970822     C*
059000970822     C                   if        XTaErr    =   *Off   and
059100970822     C                             XTAKey    <>  *Blank
059200970822     C                   move      XtaKey        D1Ctb
059300970822     C                   endif
059400970822     C*
059500970822     C*
059600970822     C* FMTD1 - tipo registrazione
059700970822     C     H1NMFL        Wheneq    'D1TPREGZ'
059800980219     C     H1NMFL        OrEq      'C1POS'
059900970822     C                   Reset                   Ana964Ds
060000970822     C                   MOVE      '01'          Opz964
060100980219     C                   MOVE      '1'           All964
060200970822     C                   MOVEL     'NDREG000'    Trc964
060300970822     C                   MOVEL     'REGTPREGZ'   Cam964
060400970822     C                   Clear                   KPJBU
060500970822     C                   Movel     Ana964Ds      KPJBU
060600970822     C                   Call      'ANA964R'
060700970822     C                   Parm                    KPJBA
060800970822     C                   Movel     KPJBU         Ana964Ds
060900970822     C                   if        Val964   <>   *Blank
061000980219     C                   Select
061100980219     C     H1NMFL        Wheneq    'D1TPREGZ'
061200970822     C                   Eval      D1TpRegz  =   Val964
061300980219     C     H1NMFL        Wheneq    'C1POS'
061400980219     C                   Eval      C1Pos     =   Val964
061500980219     C                   EndSl
061600970822     C                   endif
061700970822     C*
061800970822     C*
061900970822     C* FMTD1 - Causale principale
062000970822     C     H1NMFL        Wheneq    'D1CAUSTES'
062100970822     C                   Reset                   Ana070DS
062200970822     C                   MOVE      '01'          Opz070
062300970822     C                   CLEAR                   KPJBU
062400971222     C                   movel     D1CausTes     Cau070
062500970822     C                   MOVEL     Ana070DS      KPJBU
062600970822     C                   CALL      'ANA070R'
062700970822     C                   PARM                    KPJBA
062800970822     C                   MOVEL     KPJBU         Ana070Ds
062900970822     C                   if        Cau070   <>   *Blank
063000970822     C                   Eval      D1CausTes =   Cau070
063100970822     C                   Endif
063200970822     C*
063300970822     C*
063400970822     C* FMTD1 - Classe
063500970822     C     H1NMFL        Wheneq    'D1CLASSE'
063600970822     C                   Reset                   XTabDs
063700970822     C                   MOVE      '01'          XTAOpz
063800970822     C                   MOVE      XScSoc        XTAAzi
063900970825     C                   MOVE      XScLin        XTALin
064000970822     C                   Z-ADD     CURR          XTARig
064100970822     C                   Z-ADD     CURC          XTACol
064200970822     C                   Clear                   KPJBU
064300970822     C                   Movel     XTabDs        KPJBU
064400970822     C                   Call      'ANGG81R'
064500970822     C                   Parm                    KPJBA
064600970822     C                   Movel     KPJBU         XTabDs
064700970822     C*
064800970822     C                   if        XTaErr    =   *Off   and
064900970822     C                             XTaKey   <>   *Blank
065000970822     C                   Move      XtaKey        D1Classe
065100970822     C                   endif
065200970822     C*
065300970822     C*
065400970822     C* FMTD1 - Serie Registrazione
065500970822     C     H1NMFL        Wheneq    'D1SERREG'
065600970822     C                   Reset                   XTabDs
065700970822     C                   Move      '01'          XTaOpz
065800970822     C                   Move      XScSoc        XTaAzi
065900970822     C                   Move      XScLin        XTaLin
066000970822     C                   Z-ADD     CURR          XTaRig
066100970822     C                   Z-ADD     CURC          XTaCol
066200970822     C                   CLEAR                   KPJBU
066300970822     C                   MOVEL     XTabDs        KPJBU
066400970822     C                   CALL      'ANGG13R'
066500970822     C                   PARM                    KPJBA
066600970822     C                   MOVEL     KPJBU         XTabDs
066700970822     C*
066800970822     C                   if        XTAErr    =   *Off    and
066900970822     C                             XTAKEY   <>   *Blank
067000970822     C                   Move      XtaKey        D1SerReg
067100970822     C                   Endif
067200970822     C*
067300970822     C*
067400970822     C* FMTD1 - registrazioni doc.IVA
067500970822     C     H1NMFL        Wheneq    'D1DOCIVA'
067600970822     C                   Reset                   Ana964Ds
067700970822     C                   Move      '01'          Opz964
067800970822     C                   Movel     'NDREG000'    TRc964
067900970822     C                   Movel     'REGDOCIVA'   Cam964
068000970822     C                   Clear                   KPJBU
068100970822     C                   Movel     Ana964Ds      KPJBU
068200970822     C                   CALL      'ANA964R'
068300970822     C                   PARM                    KPJBA
068400970822     C                   Movel     KPJBU         Ana964Ds
068500970822     C                   if        Val964   <>   *Blank
068600970822     C                   Eval      D1DocIva =    Val964
068700040308     X**********         MOVEL     Des964        D1DocIVAD
068800970822     C                   ENDIF
068900970822     C*
069000970822     C*
069100040308      * FMTD1 - unità
069200040308     C     H1NMFL        Wheneq    'D1UNITA'
069300970822     C* se gestita
069400970822     C     FgO1          andeq     *On
069500040308     C                   Reset                   Ana009Ds
069600040308     C                   Movel     '01'          Opz009
069700040308     C                   Movel     XScSoc        Soc009
069800040308     C                   Movel     Ana009Ds      KPJBU
069900040308     C                   Call      'ANA009R'
070000040308     C                   Parm                    KPJBA
070100040308     C                   Movel     KPJBU         Ana009Ds
070200040308     C
070300040308     C                   if        Uni009   <>   *Blank
070400040308     C                   Move      Uni009        D1Unita
070500040308     C                   endif
070600040308     C*
070700040308     C* ricerca non abilitata in questa posizione
070800040308     C                   OTHER
070900040308     C                   SETON                                        9899
071000040308     C*
071100040308     C                   ENDSL
071200970821     C*
071300970821     C* imposto pos. del cursore
071400970821     C                   Z-ADD     CURR          H1RIGA
071500970821     C                   Z-ADD     CURC          H1COLO
071600970821     C*
071700970821     C                   MOVE      *HIVAL        $LASTV
071800970821     C*
071900970821     C                   ENDSR
072000970821     C/EJECT
072100970821     C************************************************************
072200970821     C* RIEMPIMENTO OPZIONI
072300970821     C************************************************************
072400970821     C     RIEOPZ        BEGSR
072500970821     C*
072600970821     C* Input :
072700970821     C* - $TX = schiera con tutti i testi disponibili
072800970821     C* - $FL = schiere con i flag di validità associati ai testi
072900970821     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
073000970821     C*          opzione usato
073100970821     C* - WMXO = variabile contenente il numero massimo di testi
073200970821     C*          opzione usabili
073300970821     C* - $L01 = Lunghezza prima riga
073400970821     C* - $L02 = Lunghezza seconda riga
073500970821     C* Output :
073600970821     C* - WOPZ1 = variabile contenente la prima riga opzioni
073700970821     C* - WOPZ2 = variabile contenente la seconda riga opzioni
073800970821     C* - FF23  = flag che indica se c'è bisogno del tasto F23
073900970821     C*
074000970826     C*
074100970826     C*
074200970821     C                   CALLB     'X04OPMSG'
074300970821     C* input
074400970821     C                   PARM                    $TX
074500970821     C                   PARM                    $FL
074600970821     C                   PARM                    WCOZ              3 0
074700970821     C                   PARM                    WMXO              3 0
074800970821     C* output
074900970821     C                   PARM                    WOPZ1            76
075000970821     C                   PARM                    WOPZ2            76
075100970821     C                   PARM                    FF23              1
075200970821     C* input
075300970821     C                   PARM                    $L01              3 0
075400970821     C                   PARM                    $L02              3 0
075500970821     C*
075600060224C1913C                   PARM                    $ALLOPT         960
075700060224C1913C*
075800970821     C                   ENDSR
075900970821     C/EJECT
076000970826     C************************************************************
076100970826     C* ULTERIORI DEFINIZIONE OPZIONI UTILIZZABILI A PGM
076200970826     C************************************************************
076300970826     C     ULTOPZ        BEGSR
076400970826     C*
076500970826     C* Questa routine deve eseguire una ulteriore definizione
076600970902     C* delle opzioni a seconda che il tipo registrazione richiesto
076700980219     C* in parzializzazione sia provvisorio/gestionale ('P'/'G') o meno e se
076800970902     C* richieste registrazioni con IVA o senza IVA
076900970826     C*
077000970902     C*
077100980219     C* disabilito:
077200970902     C* 8=Modifica data registrazione
077300980219     C                   if        (D1TpRegz  <>  'P'  and
077400980219     C                              D1TpRegz  <>  'G'      ) or
077500980115     C                             D1DocIva  =   '1'
077600970826     C                   Z-ADD     3             X
077700970826     C     08            LOOKUP    $OP1(X)                                21
077800970826     C     *IN21         IFEQ      *ON
077900970902     C                   MOVE      'N'           $FL1(X)
078000970826     C                   ENDIF
078100970826     C                   endif
078200970826     C*
078300980219     C* disabilito:
078400970902     C* 22=Passaggio a definitivo
078500040326C1742X****************** if        D1TpRegz  <>  'P'  or
078600040326C1742C                   if        D1TpRegz  =   'D'  or
078700040302     C                             D1DocIva  =   '1'
078800970902     C                   Z-ADD     4             X
078900970902     C     22            LOOKUP    $OP1(X)                                21
079000970902     C     *IN21         IFEQ      *ON
079100970902     C                   MOVE      'N'           $FL1(X)
079200970902     C                   ENDIF
079300970902     C                   endif
079400970902     C*
079500970826     C*
079600970826     C                   ENDSR
079700970826     C/EJECT
079800970821     C************************************************************
079900970821     C* RIEMPIMENTO TASTI FUNZIONE
080000970821     C************************************************************
080100970821     C     RIEKEY        BEGSR
080200970821     C*
080300970821     C* Input :
080400970821     C* - $FX = schiera con tutti i testi disponibili
080500970821     C* - $FC = schiere con i flag di validità associati ai testi
080600970821     C* - $FR = schiere con i flag di REVERSE IMAGE
080700970821     C* - $ULK = ultimo testo caricato
080800970821     C* - $L01 = Lunghezza prima riga
080900970821     C* - $L02 = Lunghezza seconda riga
081000970821     C* Output :
081100970821     C* - $ULK
081200970821     C* - $KEY1= 1° riga di tasti funzione
081300970821     C* - $KEY2= 2° riga di tasti funzione
081400970821     C*
081500970821     C                   CALLB     'XKEYMSG'
081600970821     C* input
081700970821     C                   PARM                    $FX
081800970821     C                   PARM                    $FC
081900970821     C                   PARM                    $FR
082000970821     C                   PARM                    $ULK              3 0
082100970821     C* output
082200970821     C                   PARM                    $KEY1            79
082300970821     C                   PARM                    $KEY2            79
082400970821     C* input
082500970821     C                   PARM                    $L01              3 0
082600970821     C                   PARM                    $L02              3 0
082700970821     C*
082800060224C1913C                   PARM                    $ALLFUNCT       480
082900060224C1913C*
083000970821     C                   ENDSR
083100970821     C/EJECT
083200970821     C************************************************************
083300970821     C* GESTIONE EMISSIONE TASTI FUNZIONE
083400970821     C************************************************************
083500970821     C     GESPIE        BEGSR
083600970821     C*
083700970821     C                   SELECT
083800970821     C* Parzializzazioni
083900970821     C     $GEST         WHENEQ    'D1'
084000970821     C     $ULKD1        IFEQ      0
084100970821     C* riempimento opzioni
084200970821     C                   EXSR      F24D1
084300970821     C                   ELSE
084400970821     C                   WRITE     FMTZ1
084500970821     C                   ENDIF
084600970821     C* Lista
084700970821     C     $GEST         WHENEQ    'S1'
084800970821     C     $ULKS1        IFEQ      0
084900970821     C                   EXSR      F24S1
085000970821     C                   ELSE
085100970821     C                   WRITE     FMTZ2
085200970821     C                   ENDIF
085300970821     C* Modifica valori assunti
085400970821     C     $GEST         WHENEQ    'D2'
085500970821     C     $ULKD2        IFEQ      0
085600970821     C                   EXSR      F24D2
085700970821     C                   ELSE
085800970821     C                   WRITE     FMTZ3
085900970821     C                   ENDIF
086000970827     C* finestra di richiesta nuova data registrazione
086100970827     C     $GEST         WHENEQ    'D5'
086200970827     C     $ULKD5        IFEQ      0
086300970827     C                   EXSR      F24D5
086400970827     C                   ELSE
086500970827     C                   WRITE     FMTZ5
086600970827     C                   ENDIF
086700970903     C* finestra di richiesta nuova data registrazione per passaggio a
086800970903     C* definitvo
086900970903     C     $GEST         WHENEQ    'D6'
087000970903     C     $ULKD6        IFEQ      0
087100970903     C                   EXSR      F24D6
087200970903     C                   ELSE
087300970903     C                   WRITE     FMTZ6
087400970903     C                   ENDIF
087500970821     C                   ENDSL
087600970821     C*
087700970821     C                   ENDSR
087800970821     C/EJECT
087900970821     C************************************************************
088000970821     C* GESTIONE PARZIALIZZAZIONI
088100970821     C************************************************************
088200970821     C     GESD1         BEGSR
088300970821     C* inizializzazione videata
088400970821     C     $INZD1        IFEQ      *ON
088500970821     C                   EXSR      INZD1
088600970821     C                   MOVE      *OFF          $INZD1
088700970821     C                   ENDIF
088800970821     C* emissione piede videata se proveniente da altra
088900970821     C* salvataggio valore formato di provenienza
089000970821     C     $LASTV        IFNE      'D1'
089100970821     C* salvo stato F17 per sapere cosa fare in caso di F12
089200970821     C                   MOVE      *INKR         WINKR
089300970821     C                   WRITE     FMTT1
089400970821     C                   EXSR      GESPIE
089500970821     C                   MOVE      $LASTV        $LASTG
089600970821     C                   MOVE      'D1'          $LASTV
089700970821     C                   ENDIF
089800970821     C*
089900970821     C* se esistono errori sulla videata
090000970821     C* emetto la write del formato a indicatori spenti per vedere
090100970821     C* le eventuali decodifiche
090200970821     C     *IN99         IFEQ      *ON
090300970821     C                   MOVEA     *IN           WIN
090400970821     C                   MOVE      *ALL'0'       IN5098           49
090500970821     C                   MOVEA     IN5098        *IN(50)
090600970821     C                   WRITE     FMTD1
090700970821     C                   MOVEA     WIN           *IN
090800970821     C                   ENDIF
090900970821     C*
091000970821     C                   EXFMT     FMTD1
091100970821     C*
091200970821     C* azzero la posizione del cursore
091300970821     C                   Z-ADD     0             H1RIGA
091400970821     C                   Z-ADD     0             H1COLO
091500970821     C*
091600970821     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
091700970821     C* INPUT
091800970821     C* $FC1 = Abilitazioni videata D1
091900970821     C* OUTPUT
092000970821     C* *IN97 + *IN99 Errori
092100970821     C                   MOVEA     $FC1          $FC
092200970821     C                   EXSR      CTRKEY
092300970821     C*
092400970821     C     *IN99         IFEQ      *OFF
092500970821     C*
092600970821     C                   SELECT
092700970821     C* F2=Decodifica
092800970821     C     $TASTO        WHENEQ    F02
092900970821     C                   EXSR      F02D1
093000970821     C* F3=Fine
093100970821     C     $TASTO        WHENEQ    F03
093200970821     C                   EXSR      F03D1
093300970821     C* F4=Ricerca
093400970821     C     $TASTO        WHENEQ    F04
093500980219     C                   EXSR      SEARCH
093600970821     C* F5=Refresh
093700970821     C     $TASTO        WHENEQ    F05
093800970821     C                   EXSR      F05D1
093900970821     C* F8=Successivo
094000970821     C     $TASTO        WHENEQ    F08
094100970821     C                   EXSR      F08D1
094200970821     C* F10=Immissione
094300970821     C     $TASTO        WHENEQ    F10
094400970821     C                   EXSR      F10D1
094500970821     C* F12=Ritorno
094600970821     C     $TASTO        WHENEQ    F12
094700970821     C                   EXSR      F12D1
094800970821     C* F18=Modifica valori assunti
094900970821     C     $TASTO        WHENEQ    F18
095000970821     C                   EXSR      F18D1
095100970821     C* F24=Altri tasti
095200970821     C     $TASTO        WHENEQ    F24
095300970821     C                   EXSR      F24D1
095400970821     C*
095500970821     C                   OTHER
095600970821     C                   EXSR      CTRD1
095700970821     C     *IN99         IFEQ      *OFF
095800970821     C* definisco l'ordinamento
095900970821     C                   EXSR      DEFORD
096000970821     C* richiamo la lista
096100970821     C                   MOVE      'S1'          $GEST
096200970821     C                   MOVE      *ON           $INZS1
096300970821     C                   MOVE      *BLANK        W1POS
096400970822     C                   MOVE      *Zeros        W1PosDt
096500970821     C                   ENDIF
096600970821     C*
096700970821     C                   ENDSL
096800970821     C*
096900970821     C                   ENDIF
097000970821     C*
097100970821     C                   ENDSR
097200970821     C/EJECT
097300970821     C************************************************************
097400970821     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
097500970821     C************************************************************
097600970821     C     INZD1         BEGSR
097700970916     C*
097800970821     C                   CLEAR                   FMTD1
097900971003     C*
098000971003     C* se l'unità non è gestita non la faccio vedere in parzializzazione
098100971003     C                   if        FgO1   =      *Off
098200990805C1212 *Se vado in manutenzione di una riga del sfl,
098300040308  "   *e dopo ritorno sulla guida con F12,
098400040308  "   *l'unità scompare:
098500040308  "   *x condizionare l'unità era stato usato l' indic.
098600040308  "   *43 che è nello stesso range di indicatori
098700040308  "   *usati dal sfl per i suoi attributi
098800040308  "   *(in 'RECS1' il sfl li riazzera)
098900040308  "  X****************** SetOff                                       42
099000990805C1212C                   SetOff                                       39
099100971003     C                   else
099200040308C1212X****************** SetOn                                        42
099300990805C1212C                   SetOn                                        39
099400971003     C                   endif
099500971002     C*
099600970821     C* Se pgm  richiamato con parametri forniti , li imposto nelle
099700970821     C*    parzializzazioni
099800970821     C                   SELECT
099900040308     C     OPZ013        WHENNE    *BLANK
100000970821     C* imposto i valori passati sui campi a video
100100970822     C*
100200970822     C                   Eval      D1Ctb      =  Ctb013
100300970822     C                   Eval      D1TpRegz   =  TpRegz013
100400970822     C                   Eval      D1DocIva   =  DocIva013
100500980220     C     DtRegDa013    ifne      *Loval
100600040308C1309X******Dmy          Move      DtRegDa013    D1DtRegDa
100700000117C1309C     *JobRun       Move      DtRegDa013    D1DtRegDa
100800980305     C                   Eval      WDtRegDa   =  DtRegDa013
100900980305     C                   else
101000980305     C                   Eval      WDtRegDa   =  *Loval
101100970822     C                   endif
101200980220     C     DtRegA013     ifne      *Loval
101300040308C1309X******Dmy          Move      DtRegA013     D1DtRegA
101400000117C1309C     *JobRun       Move      DtRegA013     D1DtRegA
101500980305     C                   Eval      WDtRegA    =  DtRegA013
101600980305     C                   else
101700980305     C                   Eval      WDtRegA    =  *Hival
101800970822     C                   endif
101900970822     C                   Eval      D1KNrAz    =  KNrAz013
102000970822     C                   Eval      D1CausTes  =  CausTes013
102100970822     C                   Eval      D1Classe   =  Classe013
102200970822     C                   Eval      D1SerReg   =  SerReg013
102300970919     C*
102400970822     C* se l'unità è gestita
102500970822     C                   IF        FgO1 = *On
102600970822     C                   Eval      D1Unita   =   Unita013
102700970822     C                   EndIf
102800970821     C* il pgm non è richiamato
102900040308     C     OPZ013        WHENEQ    *BLANK
103000970821     C* imposto dei valori di default
103100970822     C*
103200970821     C                   ENDSL
103300970822     C*
103400970822     C* imposto quei campi che comunque hanno bisogno di un default
103500970822     C                   if        D1Ctb   =  *Blank
103600970822     C                   Eval      D1Ctb   =  'CG'
103700970822     C                   endif
103800970822     C                   if        D1DocIva=  *Blank
103900970822     C                   Eval      D1DocIva=  '0'
104000970822     C                   endif
104100970821     C*
104200970821     C                   ENDSR
104300970821     C/EJECT
104400970821     C************************************************************
104500970821     C* GESTIONE F02 VIDEO D1
104600970821     C************************************************************
104700970821     C     F02D1         BEGSR
104800970821     C*
104900970821     C* per decodificare la videata richiamo la routine dei ctrl
105000970821     C* e poi disattivo gli eventuali messaggi di errore
105100970821     C                   EXSR      CTRD1
105200970821     C*
105300970821     C                   MOVE      *ALL'0'       IN5098
105400970821     C                   MOVEA     IN5098        *IN(50)
105500970821     C*
105600970821     C                   ENDSR
105700970821     C/EJECT
105800970821     C************************************************************
105900970821     C* GESTIONE F03 VIDEO D1
106000970821     C************************************************************
106100970821     C     F03D1         BEGSR
106200970821     C*
106300970821     C                   MOVE      *ON           $FINE
106400040308     C                   MOVE      '1'           RET013
106500970821     C* fine programma
106600970821     C                   EXSR      ENDPGM
106700970821     C*
106800970821     C                   ENDSR
106900970821     C/EJECT
107000970821     C************************************************************
107100970821     C* GESTIONE F05 VIDEO D1
107200970821     C************************************************************
107300970821     C     F05D1         BEGSR
107400970821     C*
107500970821     C                   MOVE      *ON           $INZD1
107600970821     C*
107700970821     C                   ENDSR
107800970821     C/EJECT
107900970821     C************************************************************
108000970821     C* GESTIONE F08 VIDEO D1
108100970821     C************************************************************
108200970821     C     F08D1         BEGSR
108300970821     C*
108400970821     C                   MOVE      *ON           $FINE
108500040308     C                   MOVE      '0'           RET013
108600970821     C*
108700970821     C                   ENDSR
108800970821     C/EJECT
108900970821     C************************************************************
109000970821     C* GESTIONE F10 VIDEO D1
109100970821     C************************************************************
109200970821     C     F10D1         BEGSR
109300970821     C*
109400970821     C*
109500970821     C                   ENDSR
109600970821     C/EJECT
109700970821     C************************************************************
109800970821     C* GESTIONE F12 VIDEO D1
109900970821     C************************************************************
110000970821     C     F12D1         BEGSR
110100970821     C*
110200970821     C*  se la 1a videata è la lista oppure sono arrivato qui con F17,
110300970821     C*  per F12 torno alla lista
110400970821     C     $LOP          IFEQ      2
110500970821     C     WINKR         OREQ      *ON
110600970821     C                   MOVE      'S1'          $GEST
110700970821     C*  altrimenti, per F12 torno a menu
110800970821     C                   ELSE
110900970821     C                   MOVE      *ON           $FINE
111000040308     C                   MOVE      '2'           RET013
111100970821     C                   ENDIF
111200970821     C*
111300970821     C                   ENDSR
111400970821     C/EJECT
111500970821     C************************************************************
111600970821     C* GESTIONE F18 VIDEO D1
111700970821     C************************************************************
111800970821     C     F18D1         BEGSR
111900970821     C*
112000970821     C                   MOVE      'D2'          $GEST
112100970821     C                   MOVEL     *ON           $INZD2
112200970821     C*
112300970821     C                   ENDSR
112400970821     C/EJECT
112500970821     C************************************************************
112600970821     C* GESTIONE F24 VIDEO D1
112700970821     C* input
112800970821     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
112900970821     C* $FX1 =Testi tasti funzione
113000970821     C* $FC1 = Abilitazioni
113100970821     C* $FR1 = Inversioni di fondo
113200970821     C************************************************************
113300970821     C     F24D1         BEGSR
113400970821     C*
113500970821     C                   MOVEA     $FX1          $FX
113600970821     C                   MOVEA     $FC1          $FC
113700970821     C                   MOVEA     $FR1          $FR
113800970821     C                   Z-ADD     $ULKD1        $ULK
113900970821     C                   Z-ADD     $D1L01        $L01
114000970821     C                   Z-ADD     $D1L02        $L02
114100970821     C                   EXSR      RIEKEY
114200970821     C                   Z-ADD     $ULK          $ULKD1            3 0
114300970821     C                   MOVEA     $FC           $FC1
114400970821     C                   MOVEL     $KEY1         Z1KE1
114500060224C1913C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
114600970821     C                   MOVEL     $KEY2         Z1KE2
114700970821     C                   WRITE     FMTZ1
114800970821     C*
114900970821     C*
115000970821     C                   ENDSR
115100970821     C/EJECT
115200970821     C************************************************************
115300970821     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
115400970821     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
115500970821     C* 93 94 96 97 98
115600970821     C************************************************************
115700970821     C     CTRD1         BEGSR
115800970821     C*
115900970821     C                   SETOFF                                       99
116000970821     C*
116100970822    > *  elencare gli eventuali controlli sui campi di
116200970822    > *  parzializzazione, ricordando che quasi sempre essi
116300970822    > *  non ne avranno perchè determinano solo il range di
116400970822    > *  scrittura della lista e quindi (nel caso di campi
116500970822    > *  alfanumerici) saranno trattati come chiavi parziali
116600970822     C*
116700970822     C*
116800970822     C                   Clear                   D1CtbD
116900970822     c* Codice contabilità (obbligatorietà e validità)
117000040308     c                   if        D1Ctb = *Blank
117100970822     c                   SetOn                                        5299
117200040308     c                   else
117300970822     C                   Reset                   XATbDs
117400970822     C                   MOVE      XCaCUt        XTBUTE
117500970822     C                   MOVE      '1'           XTBRIC
117600970822     C                   MOVE      XScSoc        XTBAZI
117700970822     C                   MOVE      *Zero         XTBKEY
117800970822     C                   MOVE      'G84'         XTBCOD
117900970822     C                   MOVE      D1Ctb         XTBKEY
118000970822     C                   CALLb     'XATB'
118100970822     C                   PARM                    XATBDS
118200040308     C     XTBERR        IFEQ      *ON
118300970822     C                   SETON                                        5399
118400040308     C                   ELSE
118500970822     C                   MOVEL     XTBUNI        AngG84Ds
118600970822     C                   MOVEL     DsbG84        D1CtbD
118700970822     C*
118800040308     C                   ENDIF
118900040308     C                   endif
119000970822     C*
119100970822     C* controllo se autorizzato alla contabilità
119200970822     C                   Clear                   XAutCtbDs
119300970822     C                   Eval      XACPrf = KNmUs
119400970822     C                   Eval      XACSoc = XscSoc
119500970822     C                   Eval      XACCtb = D1Ctb
119600970822     C                   Call      'XAUTCTB'
119700970822     C                   Parm                    XAutCtbDs
119800970822     C*
119900040308     C                   If        XACAbl = *off
120000040308     C                   SetOn                                        5499
120100040308     C                   EndIf
120200970822     C*
120300970822     C*
120400980219     C* Flag tipo registrazione
120500980219     C                   Clear                   D1TpRegzD
120600970822     C                   Clear                   X07ValDs
120700970822     C                   Move      '1'           X07Ric
120800980219     C                   Move      '1'           X07All
120900970822     C                   Movel     'NDREG000'    X07TRc
121000970822     C                   Movel     'REGTPREGZ'   X07Cam
121100970822     C                   Movel     D1TpRegz      X07Val
121200970822     C                   Callb     'X07VALR'
121300970822     C                   Parm                    X07ValDS
121400040308     C                   if        X07Err     =  '0'
121500970822     C                   Eval      D1TpRegzD  =  X07Des
121600970822     C*
121700970822     C* verifico compatibilità tra flag tipo registrazione e  contabilità
121800970822     C*
121900970822     C* se l'utente è autorizzato alla contabilità
122000040308     C                   IF        XACAbl   =   *On
122100970822     C* ma non è autorizzato alle operazioni definitive e
122200980220     C* il tipo di registrazione richiesto è 'D'= definitiva
122300040308     C                   IF        XACDef    =   *Off   and
122400980220     C                             D1TpRegz =   'D'     or
122500970822     C* oppure,
122600970822     C* ma non è autorizzato alle operazioni provvisorie e
122700980220     C* il tipo di registrazione richiesto è 'P'= provvisorio
122800040308     C                             XACPro    =   *Off   and
122900980220     C                             D1TpRegz  =   'P'    or
123000970822     C* oppure,
123100970822     C* ma non è autorizzato alle operazioni gestionali  e
123200980220     C* il tipo di registrazione richiesto è 'G'= gestionale
123300040308     C                             XACGes    =   *Off   and
123400980220     C                             D1TpRegz =   'G'
123500970822     C* segnalo la NON compatibilità
123600040308     C                   SetOn                                        5899
123700040308     C                   endif
123800040308     C                   endif
123900970822     C* valore non valido
124000040308     C                   else
124100970822     C                   SetOn                                        5999
124200040308     C                   endif
124300970916     C*
124400970822     C* esistenza/validità causale principale
124500970822     C                   Clear                   D1CausTesD
124600970822     C                   if        D1CausTes  <> *Blank
124700970822     c                   Eval      WCausale   =  D1CausTes
124800970822     c                   ExSr      CallMvc003
124900040308     c                   if        Esito      =  *On
125000970822     c* causale non valida
125100970822     c                   SetOn                                        6099
125200970822     C                   else
125300970822     C* OK
125400970822     c                   Eval      D1CausTesD =  OpeDesBrev
125500040308     c                   endif
125600040308     c                   endif
125700970822     C
125800970822     C* date di registrazione
125900970822     c                   Eval      WDtRegDa  =   *Loval
126000980220     C     D1DtRegDa     ifne      *Zero
126100040308C1309X******Dmy          Test(d)                 D1DtRegDa              21
126200000117C1309C     *JobRun       Test(d)                 D1DtRegDa              21
126300040308     C     *In21         ifeq      *On
126400970822     c* non corretta
126500040308     C                   SetOn                                        6199
126600970822     c                   else
126700970822     c* è corretta,
126800970822     c* salvo la data iniziale
126900040308C1309X******Dmy          move      D1DtRegDa     WDtRegDa
127000000117C1309c     *JobRun       move      D1DtRegDa     WDtRegDa
127100040308     C                   endif
127200040308     C                   endif
127300970822      *
127400970822      * Data scadenza finale
127500970822     c                   Eval      WDtRegA   =   *Hival
127600040308     C     D1DtRegA      ifne      *Zeros
127700970822     c* se è stata inserita, controllo la correttezza
127800040308C1309X******Dmy          Test(d)                 D1DtRegA               21
127900000117C1309C     *JobRun       Test(d)                 D1DtRegA               21
128000040308     C     *In21         ifeq      *On
128100970822     c* non corretta
128200040308     C                   SetOn                                        6299
128300040308     c                   else
128400970822     c* salvo la data finale
128500040308C1309X******Dmy          move      D1DtRegA      WDtRegA
128600000117C1309c     *JobRun       move      D1DtRegA      WDtRegA
128700040308     C                   endif
128800040308     C                   endif
128900970822      *
129000970822      * controllo congruenza tra data iniziale e data finale:
129100970822     c* - OK se data finale  >=  data inizale;
129200040308     C     WDtRegDa      ifne      *Loval
129300980220     C     WDtRegA       andne     *Hival
129400040308     c     WDtRegA       iflt      WDtRegDa
129500040308     C                   SetOn                                        6399
129600040308     c                   endif
129700040308     c                   endif
129800970822     C
129900030318C1636 *
130000030318  "   * nr registrazione da
130100030318  "  C                   if        D1NrRegDa<>0 and
130200030318  "  C                             WDtRegDa=*loval and WDtRegA=*hival
130300030318  "   *  NDC0114 - Numero non ammesso se non inserita data
130400030318  "  C                   eval      *in72 = *on
130500030318  "  C                   eval      *in99 = *on
130600030318  "  C                   endif
130700030318  "   * nr registrazione a
130800030318  "  C                   if        D1NrRegA <>0 and
130900030318  "  C                             WDtRegDa=*loval and WDtRegA=*hival
131000030318  "   *  NDC0114 - Numero non ammesso se non inserita data
131100030318  "  C                   eval      *in73 = *on
131200030318  "  C                   eval      *in99 = *on
131300030318  "  C                   endif
131400030318C1636 *
131500970822     C*
131600970822     C* se classe impostata
131700970825     C                   Clear                   D1ClasseD
131800970822     C                   IF        D1Classe  <>  *Blank
131900970822     C                   Reset                   XAtbDs
132000970822     C                   EVAL      XtbRic    =   '1'
132100970822     C                   EVAL      XtbAzi    =   XScSoc
132200970822     C                   EVAL      XtbKey    =   *Zero
132300970822     C                   EVAL      XtbCod    =   'G81'
132400970822     C                   MOVE      D1Classe      XTbKey
132500970822     C                   Callb     'XATB'
132600970822     C                   Parm                    XAtbDs
132700970822     C                   if        XTbErr = '0'
132800970822     C                   Eval      AngG81Ds   =  XtbUni
132900970822     C                   Eval      D1ClasseD  =  De1G81
133000970822     C                   else
133100970822     C* la classe contabile non esiste
133200970822     C                   SetOn                                        6499
133300970822     C                   Endif
133400970822     C                   Endif
133500970822     C*
133600970822     C*
133700970822     C* se serie impostata
133800970825     C                   Clear                   D1SerRegD
133900970822     C                   IF        D1SerReg  <>  *Blank
134000970822     C                   Reset                   XAtbDs
134100970822     C                   EVAL      XtbRic    =   '1'
134200970822     C                   EVAL      XtbAzi    =   XScSoc
134300970822     C                   EVAL      XtbKey    =   *Zero
134400970822     C                   EVAL      XtbCod    =   'G13'
134500970822     C                   MOVE      D1SerReg      XTbKey
134600970822     C                   Callb     'XATB'
134700970822     C                   Parm                    XAtbDs
134800970822     C                   if        XTbErr = '0'
134900970822     C                   Eval      AngG13Ds   =  XtbUni
135000970822     C                   Eval      D1SerRegD  =  DesG13
135100970822     C                   else
135200970822     C* la serie non esiste
135300970822     C                   SetOn                                        6599
135400970822     C                   Endif
135500970822     C                   Endif
135600970822     C*
135700970822     C*
135800970822     C* flag Doc.IVA valido
135900970822     C                   Clear                   D1DocIvaD
136000970825     C                   if        D1DocIva   =  *Blanks
136100970825     C* obbligatorio
136200970825     C                   SetOn                                          6799
136300970825     C                   else
136400970822     C                   Reset                   X07ValDs
136500970822     C                   Move      '1'           X07RIC
136600970822     C                   Movel     'NDREG000'    X07TRC
136700970822     C                   Movel     'REGDOCIVA'   X07CAM
136800970822     C                   Movel     D1DocIVA      X07VAL
136900970822     C                   Callb     'X07VALR'
137000970822     C                   Parm                    X07ValDs
137100040308     C     X07Err        Ifeq      '0'
137200970822     C                   Movel     X07Des        D1DocIVAD
137300040308     C                   else
137400970822     C                   SetOn                                        6699
137500040308     C                   EndIf
137600040308     C                   EndIf
137700970822     c*
137800970822     c*
137900970822     c* controllo unità SE GESTITA
138000970822     c                   Clear                   D1UnitaD
138100970822     C     FGO1          ifeq      *On
138200970822     c* controllo validità dell'unità
138300970822     c     D1Unita       ifne      *Blank
138400970822     C                   eval      UniSocieta =  XScSoc
138500970822     C                   eval      UniUnita   =  D1Unita
138600970822     C     K02Uni01      Chain     AnUni01l                           21
138700970822     C                   if        *In21 = *On
138800970822     c* unità non trovata
138900970822     c                   SetOn                                        5599
139000970822     C                   else
139100970822     C                   eval      D1UnitaD = UniDesBrev
139200970822     C                   endif
139300970822     C                   endif
139400970822     C*
139500970822     C* controllo autorizzazione all'unità
139600970822     C                   Reset                   XAutUniDs
139700990805C1212C                   RESET                   XAutUniDS1
139800970822     C                   Eval      XAUPrf = KNmUs
139900970822     C                   Eval      XAUSoc = XscSoc
140000970822     C                   Eval      XAUUni = D1Unita
140100970822     C                   Call      'XAUTUNI'
140200970822     C                   Parm                    XAutUniDS
140300990805C1212C                   PARM                    XAutUniDS1
140400970822     C                   If        XAUAbl = *Off
140500970822     C                   SetOn                                        5699
140600990805C1212 *
140700990805  "   * se utente autorizzato alla unità di registrazione
140800990805  "  C                   Else
140900990805  "   *  controllo se tipo registrazione compatibile con lunità
141000990805  "  C                   If        XAUDef = *Off  and
141100990805  "  C                             D1TpRegz = 'D' or
141200990805  "  C                             XAUProv = *Off  and
141300990805  "  C                             D1TpRegz = 'P' or
141400990805  "  C                             XAUGest = *Off  and
141500990805  "  C                             D1TpRegz = 'G'
141600990805  "   *    'Utente non abilitato al tipo registrazione per l'unità scelta'
141700990805  "  C                   SETON                                        5799
141800990805  "  C                   EndIf
141900990805C1212 *
142000970822     C                   EndIf
142100970822     C                   endif
142200970821     C*
142300970821     C                   ENDSR
142400970821     C/EJECT
142500970822     C************************************************************
142600970822     C* Chiamata NDMVC003
142700970822     C************************************************************
142800970822     C     CallMvc003    BEGSR
142900970822     C*
143000970822     C* Controllo causale
143100970822     C                   Clear                   AnOpe
143200970822     C                   EVAL      LenOut = %Size(AnOpe)
143300970822     C                   CallB     'NDMVC003'
143400970822     C                   PARM                    XScSoc
143500970822     C                   PARM                    WCausale
143600970822     C                   PARM                    *Omit
143700970825     C                   PARM      '0'           Uso
143800970822     C                   PARM      *Off          GesMsg
143900970822     C                   PARM      *Off          Esito
144000970822     C                   PARM      'ANOPE'       StrutturaO
144100970822     C     AnOpe         PARM                    OutPut
144200970822     C                   PARM                    LenOut
144300970822     C                   PARM      *On           Refresh
144400970825     C*
144500970822     C                   ENDSR
144600970822     C/EJECT
144700970821     C************************************************************
144800970821     C* DEFINIZIONE DELL'ORDINAMENTO
144900970821     C************************************************************
145000970821     C     DEFORD        BEGSR
145100970821     C*
145200970821     C* definisco l'ordinamento della lista a seconda dei valori
145300970821     C* impostati nel video di parzializzazione
145400970821     C*
145500970821     C                   SELECT
145600970822    > *  nel caso di più viste logiche e quindi più campi di
145700970822    > *  parzializzazione, qui vanno definiti i criteri per
145800970822    > *  determinare quale VL va letta, a seconda di quali campi
145900970822    > *  sono stati valorizzati
146000980219    >C     D1TpRegZ      WHENEq    *BLANK
146100980220     C     D1DtRegDa     ifeq      *Zero
146200980219     C                   Z-ADD     1             $ORD
146300980220     C                   else
146400980220     C                   Z-ADD     2             $ORD
146500980220     C                   endif
146600130618    >C     D1knraz       ifne      0
146700130618    >C     D1unita       andne     *blanks
146800130618     C                   eval      $ord = 3
146900130618     C                   endif
147000980219    >C     D1TpRegZ      WHENNE    *BLANK
147100970822     C                   Z-ADD     2             $ORD
147200970821     C* imposto l'ordinamento 99 per ottenere in selezione solo il
147300970821     C* rcd con la chiave passata
147400970821     C     $Unique       WHENEQ    *ON
147500970821     C                   Z-ADD     99            $ORD
147600970821     C* nel caso non sia stato digitato alcun valore in parzializzazione
147700970821     C* utilizzerò l'ordinamento precedentemente memorizzato
147800970821     C* (temporaneamente o definitivamente) nei valori assunti
147900970821     C                   OTHER
148000970821     C                   ENDSL
148100970821     C                   ENDSR
148200970821     C/EJECT
148300970821     C************************************************************
148400970821     C* Chiamata programma con tasto personalizzato
148500970821     C************************************************************
148600970821     C     FPerS1        BEGSR
148700970821     C*
148800970821     C* salvo la DS di ingresso
148900040308     C                   MOVEL     Ndc013DS      $SaveDS
149000970821     C*
149100040308     C                   MOVEL     Ndc013DS      KPJBU
149200970821     C                   CALL      $PGF2(PosFun)                        22
149300970821     C                   PARM                    KPJBA
149400970821     C                   PARM                    NOMPGM
149500970821     C*
149600970821     C* ripristino la DS di ingresso
149700040308     C                   MOVEL     $SaveDS       Ndc013DS
149800970821     C*
149900970821     C                   ENDSR
150000970821     C/EJECT
150100970821     C************************************************************
150200970821     C* GESTIONE LISTA
150300970821     C************************************************************
150400970821     C     GESS1         BEGSR
150500970827     C*
150600970821     C* inizializzazione videata
150700970821     C     $INZS1        IFEQ      *ON
150800970821     C                   EXSR      INZS1
150900970821     C                   MOVE      *OFF          $INZS1
151000970821     C                   ENDIF
151100970821     C* emissione piede videata se proveniente da altra
151200970821     C* salvataggio valore formato di provenienza
151300970821     C     $LASTV        IFNE      'S1'
151400970821     C                   WRITE     FMTT1
151500970821     C                   EXSR      GESPIE
151600970821     C                   MOVE      $LASTV        $LASTG
151700970821     C                   MOVE      'S1'          $LASTV
151800970821     C                   ENDIF
151900970821     C*
152000970821     C     WMAX          IFEQ      0
152100970821     C                   WRITE     FMTD3                                        NO RCD
152200970821     C                   ENDIF
152300970821     C* Inizializzazione indice tasti funzione personalizzati
152400970821     C                   Eval      PosFun = 25
152500970821     C*
152600970822     C* serve per l'emissione del sfl Drop
152700980305     C* mantengo la situazione 'aperta' o 'chiusa' del sfl drop
152800970822     C                   Eval      *In09     =   C1Mode
152900980219     C*
153000980220     C* se in parzializzazione NON è impostato un tipo di registrazione
153100980220     C* particolare e NEMMENO una data cui considerare le registrazioni
153200980220     C* spengo l'indicatore per posizionarmi con il tipo registrazione,
153300980220     C* in qulaunque altro caso accendo l'indicatore per posizionarmi con
153400980220     C* la data
153500980220     C     D1TpRegz      ifeq      *Blank
153600980220     C     D1DtRegDa     andeq     *Zero
153700980220     C                   SetOff                                       10
153800980219     C                   else
153900980220     C                   SetOn                                        10
154000980219     C                   endif
154100970827     C*
154200970821     C                   EXFMT     FMTC1
154300970821     C*
154400970821     C* azzero la posizione del cursore
154500970821     C                   Z-ADD     0             H1RIGA
154600970821     C                   Z-ADD     0             H1COLO
154700970821     C*
154800970821     C     C1NRR         IFNE      0
154900970821     C                   Z-ADD     C1NRR         WSFL
155000970821     C                   ENDIF
155100970821     C                   IF        SFLNRR > 0
155200970821     C                   Z-ADD     SFLNRR        C1RCD
155300970821     C                   ENDIF
155400970821     C* inizializzazione indice opzioni personalizzate
155500970821     C                   Eval      PosOpz = 0
155600970821     C* CONTROLLO ABILITAZIONE TASTI
155700970821     C* INPUT
155800970821     C* $FC2 = Abilitazioni videata S1
155900970821     C* OUTPUT
156000970821     C* *IN97 + *IN99 Errori
156100970821     C                   MOVEA     $FC2          $FC
156200970821     C                   EXSR      CTRKEY
156300970821     C*
156400970821     C     *IN99         IFEQ      *OFF
156500970821     C*
156600970821     C                   SELECT
156700970821     C* F3=Fine
156800970821     C     $TASTO        WHENEQ    F03
156900970821     C                   EXSR      F03S1
157000980219     C* F4=Richiesta
157100980219     C     $TASTO        WHENEQ    F04
157200980219     C                   EXSR      SEARCH
157300970827     C* F5=Refresh
157400970827     C     $TASTO        WHENEQ    F05
157500970827     C                   EXSR      F05S1
157600970821     C* F8=Successivo
157700970821     C     $TASTO        WHENEQ    F08
157800970821     C                   EXSR      F08S1
157900970821     C* F10=Immissione
158000970821     C     $TASTO        WHENEQ    F10
158100970821     C                   EXSR      F10S1
158200970821     C* F12=Ritorno
158300970821     C     $TASTO        WHENEQ    F12
158400970821     C                   EXSR      F12S1
158500970827     C* F13=Selezione multipla
158600970827     C     $TASTO        WHENEQ    F13
158700970827     C                   EXSR      F13S1
158800970821     C* F17=Parzializzazioni
158900970821     C     $TASTO        WHENEQ    F17
159000970821     C                   EXSR      F17S1
159100970821     C* F18=Modifica valori assunti
159200970821     C     $TASTO        WHENEQ    F18
159300970821     C                   EXSR      F18S1
159400970821     C* F23=Altre opzioni
159500970821     C     $TASTO        WHENEQ    F23
159600970821     C                   EXSR      F23S1
159700970821     C* F24=Altri tasti
159800970821     C     $TASTO        WHENEQ    F24
159900970821     C                   EXSR      F24S1
160000970821     C* ROLL-UP
160100970821     C     $TASTO        WHENEQ    ROLLUP
160200970821     C                   EXSR      ROLS1
160300970821     C* Personalizzata
160400970821     C     $PGF2(PosFun) Whenne    *BLANK
160500970821     C                   EXSR      FperS1
160600970821     C*
160700970821     C                   OTHER
160800970821     C* CONTROLLO DATI
160900970821     C                   EXSR      CTRC1
161000970821     C     *IN99         IFEQ      *OFF
161100970821     C                   EXSR      CTRS1
161200970821     C                   ENDIF
161300970821     C*
161400970821     C                   ENDSL
161500970821     C*
161600970821     C                   ENDIF
161700970821     C*
161800970821     C                   ENDSR
161900970821     C/EJECT
162000970821     C************************************************************
162100970821     C* INIZIALIZZAZIONE LISTA
162200970821     C************************************************************
162300970821     C     INZS1         BEGSR
162400970827     C*
162500970821     C* pulizia SFL
162600970821     C                   SETOFF                                         3031
162700970821     C                   WRITE     FMTC1
162800970821     C                   SETON                                          31
162900970821     C*
163000970821     C* riempimento opzioni
163100970821     C                   MOVEA     $TX1          $TX
163200970821     C                   MOVEA     $FL1          $FL
163300970821     C                   Z-ADD     0             WCOZ
163400970821     C                   Z-ADD     0             WMXO
163500970821     C*  equivale al num.di elementi della schiera $TX1
163600970821     C                   Z-add     OpzLen        WMXO
163700970821     C                   Z-ADD     $C1Op1        $L01
163800970821     C                   Z-ADD     $C1Op2        $L02
163900970821     C                   EXSR      RIEOPZ
164000970821     C                   Z-ADD     23            X
164100970821     C                   MOVE      FF23          $FC2(X)
164200970821     C                   MOVEL     WOPZ1         C1OPZ1
164300060224C1913C                   MOVEL     $ALLOPT       H1ALLOPT
164400970821     C                   MOVEL     WOPZ2         C1OPZ2
164500970821     C                   Z-ADD     WCOZ          C1COZ
164600970821     C*
164700970821     C* CARICAMENTO SFL A PAGINE
164800970821     C*
164900970821     C                   Z-ADD     0             S1NRR
165000970821     C                   Z-ADD     0             WMAX
165100970825     C*
165200970825     C* imposto i campi nel control del sfl
165300970825     C                   Eval      C1Ctb     =   D1Ctb
165400970825     C                   Eval      C1TpRegz  =   D1TpRegz
165500970825     C                   Eval      C1DocIva  =   D1DocIva
165600970826     C* decodifico:
165700970826     C* contabilità
165800970826     C                   Clear                   C1CtbD
165900970826     C                   Reset                   XATbDs
166000970826     C                   MOVE      XCaCUt        XTBUTE
166100970826     C                   MOVE      '1'           XTBRIC
166200970826     C                   MOVE      XScSoc        XTBAZI
166300970826     C                   MOVE      *Zero         XTBKEY
166400970826     C                   MOVE      'G84'         XTBCOD
166500970826     C                   MOVE      C1Ctb         XTBKEY
166600970826     C                   CALLb     'XATB'
166700970826     C                   PARM                    XATBDS
166800040427     C     XTBERR        IFEQ      *Off
166900970826     C                   MOVEL     XTBUNI        AngG84Ds
167000970826     C                   MOVEL     DsbG84        C1CtbD
167100040308     C                   ENDIF
167200970826     C* tipo registrazione
167300980219     C                   Clear                   C1TpRegzD
167400980219     C                   Clear                   X07ValDs
167500970826     C                   Move      '1'           X07Ric
167600970826     C                   Movel     'NDREG000'    X07TRc
167700970826     C                   Movel     'REGTPREGZ'   X07Cam
167800970826     C                   Movel     C1TpRegz      X07Val
167900970826     C                   Callb     'X07VALR'
168000970826     C                   Parm                    X07ValDS
168100040308     C                   if        X07Err     =  '0'
168200970826     C                   Eval      C1TpRegzD  =  X07Des
168300970826     C                   endif
168400970826     C* registrazione documento IVA
168500970826     C                   Clear                   C1DocIvaD
168600970826     C                   Reset                   X07ValDs
168700970826     C                   Move      '1'           X07RIC
168800970826     C                   Movel     'NDREG000'    X07TRC
168900970826     C                   Movel     'REGDOCIVA'   X07CAM
169000970826     C                   Movel     C1DocIVA      X07VAL
169100970826     C                   Callb     'X07VALR'
169200970826     C                   Parm                    X07ValDs
169300040308     C     X07Err        Ifeq      *Off
169400040308     C                   Movel     X07Des        C1DocIVAD
169500040308     C                   endif
169600970821     C*
169700970821     C* Posizionamento su file pilota
169800970822     C                   CLEAR                   C1POS
169900970828     C                   Clear                   C1PosDt
170000970821     C                   EXSR      SETANA
170100970821     C* ripristino i valori del posizionamento
170200970821     C                   MOVE      W1POS         C1POS
170300970828     C                   MOVE      W1PosDt       C1PosDt
170400970822     C* aggiungere gestione altri campi posizionamento se ci sono
170500970821     C*
170600970821     C* Lettura file pilota
170700970821     C                   EXSR      REDANA
170800970821     C*
170900970821     C* se arrivo dalla parzializzazione devo caricare solo la prima pagina
171000970821     C                   IF        $LastV = 'D1'
171100970821     C                   Z-ADD     SFLPAG        WPAG
171200970821     C                   ENDIF
171300970821     C                   EXSR      ROLS1
171400970821     C* imposto il caricamento sfl del numero massimo di righe per pagina
171500970821     C                   Z-ADD     SFLPAG        WPAG
171600970821     C*
171700970821     C                   ENDSR
171800970821     C/EJECT
171900970821     C************************************************************
172000970821     C* CARICAMENTO PAGINA LISTA
172100970821     C************************************************************
172200970821     C     ROLS1         BEGSR
172300970821     C*
172400970821     C                   SETOFF                                       32
172500970821     C                   Z-ADD     0             Y
172600970821     C                   Z-ADD     WMAX          S1NRR
172700970821     C*
172800970821     C* Leggo dal file anagrafico per caricare la lista
172900970821     C*
173000970821     C     $EndFile      DOWEQ     *OFF
173100970821     C     Y             ANDLT     WPAG
173200970821     C*
173300970821     C                   EXSR      RIES1
173400970821     C*
173500970821     C                   ADD       1             S1NRR
173600970821     C                   ADD       1             Y
173700970821     C*
173800970821     C                   WRITE     FMTS1
173900970821     C*
174000970821     C                   EXSR      REDANA
174100970821     C*
174200970821     C                   ENDDO
174300970821     C*
174400970821     C                   Z-ADD     S1NRR         WMAX                 30
174500970821     C*
174600970821     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
174700970821     C*
174800970821     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
174900970821     C                   MVR                     RESTO             3 0
175000970821     C     PAGINE        MULT      SFLPAG        C1RCD
175100970821     C     RESTO         IFGT      0
175200970821     C                   ADD       1             C1RCD
175300970821     C                   ELSE
175400970821     C                   SUB       SFLPAG        C1RCD
175500970821     C                   ADD       1             C1RCD
175600970821     C                   ENDIF
175700970821     C*
175800970821     C                   ENDSR
175900970821     C/EJECT
176000970821     C************************************************************
176100970821     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
176200970821     C************************************************************
176300970821     C     SETANA        BEGSR
176400970821     C*
176500970821     C*
176600970821     C                   SELECT
176700970821     C*
176800970821     C* Gestione posizionamento
176900970821     C* Se il campo W1POS è > *blank imposto il posizionamento con
177000970821     C* W1POS altrimenti lo imposto con il valore di parametrizzazione
177100970821     C*
177200130618    >C* Ord.3 = chiave ordinamento 3 (posizionam. per unità e n. azione)
177300130618     C     $ORD          WHENEQ    3
177400130618     C                   eval      regsocieta = xscsoc
177500130618     C                   eval      regunita = d1unita
177600130618     C                   eval      regknraz = d1knraz
177700130618     C     k03reg99      setll     ndreg99l
177800040308     C* Ord 1 = chiave ordinamento 1 (posizionam. per tipo registrazione)
177900970821     C     $ORD          WHENEQ    1
178000040308     C                   MOVE      XscSoc        RegSocieta
178100980219     C                   Eval      RegCtb     =  D1Ctb
178200980219     C     W1POS         IFNE      *BLANK
178300040308     C                   MOVEL     W1POS         RegTpRegz
178400040308     C     K03Reg13      SETLL     NdReg13L
178500970821     C                   ELSE
178600040308     C     K02Reg13      SETLL     NdReg13L
178700970821     C                   ENDIF
178800040308     C* Ord.2 = chiave ordinamento 2 (posizionam. per data registrazione)
178900980220     C     $ORD          WHENEQ    2
179000030318C1636C                   exsr      SetAnaOrd2
179100030318C1636X* Trasferite le specifiche sotto asteriscate nella routine SetAnaOrd2
179200030318  "  X**se tutte le VL hanno la società in chiave (ed è la principale)
179300030318  "  X**si può impostare una volta per tutte al di fuori dei test
179400030318  "  X**                 MOVE      XscSoc        RegSocieta
179500030318  "  X**                 Eval      RegCtb     =  D1Ctb
179600030318  "  X**   D1TpRegz      ifeq      *Blank
179700030318  "  X**   W1PosDt       IFNE      0
179800030318  "  X**                 Eval      RegDtReg   =  WPosData
179900030318  "  X**                 ELSE
180000030318  "  X**                 Eval      RegDtReg   =  WDtRegDa
180100030318  "  X**                 ENDIF
180200030318  "  X**   K03Reg04      SETLL     NdReg04L
180300030318  "  X**
180400030318  "  X**                 else
180500030318  "  X**                 Eval      RegTpRegz  =  D1TpRegz
180600030318  "  X**                 Eval      RegDocIva  =  D1DocIva
180700030318  "  X**   W1PosDt       IFNE      0
180800030318  "  X**                 Eval      RegDtReg   =  WPosData
180900030318  "  X**                 ELSE
181000030318  "  X**                 Eval      RegDtReg   =  WDtRegDa
181100030318  "  X**                 ENDIF
181200030318  "  X**   K05Reg13      SETLL     NdReg13L
181300030318C1636X**                 endif
181400970821     C* Ord.99= Chiave univoca
181500970821     C* (quando viene gestita la chiave univoca NON deve essere gestito
181600970821     C*  il posizionamento C1POS/W1POS)
181700970821     C     $ORD          WHENEQ    99
181800040308     C*********          MOVEL     D1PRG         ANAPRG
181900040308     C*******            MOVEL     D1PKG         ANAPKG
182000040308     C*****K02___99      SETLL     AN___99
182100970821     C                   ENDSL
182200970821     C*
182300970821     C                   ENDSR
182400970821     C/EJECT
182500030318C1636 ************************************************************
182600030318  "   * Posizionamento ord=2
182700030318  "   * (d1tpregz=0 e dtregda<>0  o  d1tpregz<>0)
182800030318  "  X* parte di specifiche sono state spostate dala routine SetAna    Ord2
182900030318  "   ************************************************************
183000030318  "  C     SetAnaOrd2    Begsr
183100030318C1636 *
183200040308     C* se tutte le VL hanno la società in chiave (ed è la principale)
183300040308     C* si può impostare una volta per tutte al di fuori dei test
183400040308     C                   MOVE      XscSoc        RegSocieta
183500030318     C                   Eval      RegCtb     =  D1Ctb
183600030318C1636
183700030318C1636 * 'tipo registrazione' vuota
183800030318     C     D1TpRegz      ifeq      *Blank
183900030318C1636
184000030318C1636 *  imposto data registrazione
184100030318     C     W1PosDt       IFNE      0
184200030318     C                   Eval      RegDtReg   =  WPosData
184300030318     C                   ELSE
184400030318     C                   Eval      RegDtReg   =  WDtRegDa
184500030318     C                   ENDIF
184600030318C1636
184700030318  "   *   data reg valorizzata   e
184800030318  "   *   'nr registrazione da' valorizzata
184900030318  "  C                   if        REGDtReg <> *loval  and
185000030318  "  C                             D1NrRegDa <> 0
185100030318  "  C                   eval      REGNrReg = D1NrRegDa
185200030318  "   *
185300030318  "   *    'serie registrazione' valorizzata
185400030318  "   *     (e data reg + 'nr reg' valorizzate)
185500030318  "  C                   if        D1SerReg <>*blank
185600030318  "  C                   eval      REGSerReg= D1SerReg
185700030318  "   *
185800030318  "   *     posizionamento per soc/ctb/DtReg/NrReg/SerReg
185900030318  "  C     K05REG04      setll     NDREG04L
186000030318  "   *
186100030318  "   *    'serie registrazione' vuota
186200030318  "   *     (e data reg + 'nr reg' valorizzate)
186300030318  "  C                   else
186400030318  "   *
186500030318  "   *     posizionamento per soc/ctb/DtReg/NrReg
186600030318  "  C     K04REG04      setll     NDREG04L
186700030318  "  C                   endif
186800030318  "   *
186900030318  "   *    data reg vuota oppure 'nr reg da' vuota
187000030318  "  C                   else
187100030318  "   *
187200040308C1636 *    posizionamento per soc/ctb/DtReg
187300040308     C     K03Reg04      SETLL     NdReg04L
187400040427C1636C                   endif
187500030318     C*
187600030318C1636 * 'tipo registrazione' valorizzata
187700030318     C                   else
187800030318     C                   Eval      RegTpRegz  =  D1TpRegz
187900030318     C                   Eval      RegDocIva  =  D1DocIva
188000030318     C     W1PosDt       IFNE      0
188100030318     C                   Eval      RegDtReg   =  WPosData
188200030318     C                   ELSE
188300030318     C                   Eval      RegDtReg   =  WDtRegDa
188400030318     C                   ENDIF
188500040308     C     K05Reg13      SETLL     NdReg13L
188600030318     C                   endif
188700030318C1636 *
188800030318C1636C     XSetAnaOrd2   Endsr
188900970821     C************************************************************
189000970821     C* LETTURA RCD ARCHIVIO PILOTA
189100970821     C************************************************************
189200970821     C     REDANA        BEGSR
189300970821     C*
189400970821     C                   MOVEL     *OFF          $EndFile
189500970821     C                   MOVEL     *OFF          $RCDOK
189600970821     C*
189700970821     C     $EndFile      DOUEQ     *ON
189800970821     C     $RCDOK        OREQ      *ON
189900970821     C*
190000970821     C                   SELECT
190100970821     C     $ORD          WHENEQ    1
190200980219    >C     K02Reg13      READE     NdReg13L                               21
190300980220     C*
190400970821     C     $ORD          WHENEQ    2
190500980220     C     D1TpRegz      ifeq      *Blank
190600040308     C     K02Reg04      READE     NdReg04L                               21
190700980220     C                   else
190800040308     C     K04Reg13      READE     NdReg13L                               21
190900980220     C                   endif
191000130618     C*
191100130618     C     $ORD          WHENEQ    3
191200130618    >C     K03Reg99      READE     NdReg99L                               21
191300980220     C*
191400970821     C     $ORD          WHENEQ    99
191500040308     X**** K02___99      READE     AN___99                                21
191600970821     C                   ENDSL
191700970821     C*
191800970821     C                   MOVE      *IN21         $EndFile
191900970821     C*
192000970821     C     $EndFile      IFEQ      *OFF
192100970821     C                   EXSR      SELANA
192200970821     C                   ENDIF
192300970821     C*
192400970821     C                   ENDDO
192500970821     C*
192600970821     C                   MOVE      *IN21         *IN33                          SflEnd
192700970821     C*
192800970821     C                   ENDSR
192900970821     C/EJECT
193000970821     C************************************************************
193100970821     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
193200970821     C************************************************************
193300970821     C     CLCPAG        BEGSR
193400970821     C* Input :
193500970821     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
193600970821     C*          cursore
193700970821     C* - SFLPAG = numero rcd per pagina sfl
193800970821     C* Output :
193900970821     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
194000970821     C*
194100970821     C     WSFL          DIV       SFLPAG        PAGINE            4 0
194200970821     C                   MVR                     RESTO             3 0
194300970821     C     RESTO         IFGT      0
194400970821     C                   ADD       1             PAGINE
194500970821     C                   ENDIF
194600970821     C     PAGINE        MULT      SFLPAG        WPAG
194700970821     C*
194800970821     C                   ENDSR
194900970821     C/EJECT
195000970821     C************************************************************
195100970821     C* SELEZIONI SULLA RIGA LETTA
195200970821     C* impostare a *OFF il campo $RCDOK per scartare un record che
195300970821     C* non rientra nei parametri di parzializzazione
195400970821     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
195500970821     C* superiore stabilito nei parametri
195600970821     C*
195700970821     C************************************************************
195800970821     C     SELANA        BEGSR
195900970821     C*
196000970821     C                   MOVEL     *ON           $RCDOK
196100970821A1211X*                  MOVEL     *ALL'9'       ALL9             15
196200970821A1211C                   MOVEL     *hival        ALL9             15
196300970821     C* creo limite massimo parzializzazioni :
196400970821     C* per ogni valore alfanumerico creo il limite superiore
196500970821     C* per cui caricare, creando una variabile che contiene
196600970821     C* il valore digitato nel campo a video riempito di "9"
196700970821     C*
196800970821     C*
196900970821     C                   SELECT
197000970821     C*
197100970821     C* SELEZIONI RISPETTO AI VALORI RCD
197200970821     C*
197300970821     C* Scartare i record che non rispondono alle selezioni effettuate
197400970821     C* esempio : esclusione degli annullati.
197500970821     C*
197600970821     C*
197700970821     C* SELEZIONI RISPETTO ALLE PARZIALIZZAZIONI
197800970821     C*
197900040308     C** numero azione modulo base non uguale alla scelta se digitata
198000040308     C                   when      RegKNrAz  <>  D1KNrAz  and
198100040308     C                             D1KNrAz   <>  *Zero
198200040308     C                   MOVEL     *OFF          $RCDOK
198300040308     C** causale principale non uguale alla scelta
198400040308     C                   when      RegCausTes <> D1CausTes and
198500040308     C                             D1CausTes  <> *Blank
198600040308     C                   MOVEL     *OFF          $RCDOK
198700040308     C** data di registrazione  non compresa nella scelta
198800040308     C     RegDtReg      WhenGt    WDtRegA
198900040308     C     RegDtReg      orlt      WDtRegDa
199000040308     C                   MOVEL     *OFF          $RCDOK
199100030318C1636C** nr registrazione  non compresa nella scelta
199200030318  "  C                   When      (REGNrReg > D1NrRegA   and
199300030318  "  C                             D1NrRegA  > 0 )        or
199400030318  "  C                             (REGNrReg < D1NrRegDa and
199500030318  "  C                             D1NrRegDa > 0 )
199600030318C1636C                   MOVEL     *OFF          $RCDOK
199700040308     C** classe non uguale alla scelta
199800040308     C                   when      RegClasse  <> D1Classe  and
199900040308     C                             D1Classe   <> *Blank
200000040308     C                   MOVEL     *OFF          $RCDOK
200100040308     C** serie non uguale alla scelta
200200040308     C                   when      RegSerReg  <> D1SerReg  and
200300040308     C                             D1SerReg   <> *Blank
200400040308     C                   MOVEL     *OFF          $RCDOK
200500040308     C** unità non uguale alla scelta
200600040308     C                   when      RegUnita   <> D1Unita   and
200700040308     C                             D1Unita    <> *Blank
200800040308     C                   MOVEL     *OFF          $RCDOK
200900040308     C** registrazione documento IVA non uguale alla scelta
201000040308     C                   when      RegDocIVA  <> D1DocIVA
201100040308     C                   MOVEL     *OFF          $RCDOK
201200980220     C*
201300980220     C*
201400980220     C** scarto per incompatibilità tra flag tipo registrazione e
201500980220     C** contanilità
201600980220     C*
201700980220     C* se NON è stato richiesto uno specifico tipo registrazione e
201800040308     C                   when      D1TpRegz   =  *Blank  and
201900980220     C* se l'utente è autorizzato alla contabilità
202000040308     C                             XACAbl     =  *On
202100000209      *
202200000209      * ma non è autorizzato alle registrazioni definitive e
202300000209      * il tipo di registrazione letto è 'D'= definitiva
202400040308D0440X***                if        XACDef     =  *Off    and
202500000209D0440C                             and (XACDef = *Off and
202600000209     C                             RegTpRegz  = 'D' or
202700000209      * oppure,
202800000209      * ma non è autorizzato alle operazioni provvisorie e
202900000209      * il tipo di registrazione letto è 'P'= provvisorio
203000040308     C                             XACPro    =   *Off   and
203100040308D0440X***                          D1TpRegz  =   'P'    or
203200000209D0440C                             RegTpRegz = 'P'    or
203300000209      * oppure,
203400000209      * ma non è autorizzato alle operazioni gestionali  e
203500000209      * il tipo di registrazione letto è 'G'= gestionale
203600040308     C                             XACGes    =   *Off   and
203700040308D0440X***                          D1TpRegz =   'G'
203800000209D0440C                             RegTpRegz =   'G')
203900000209      * scarto il record
204000040308     C                   MOVEL     *OFF          $RCDOK
204100040308D0440X***                endif
204200000209  "   *
204300000209  "   * Scarto per incompatibilità tra flag tipo registrazione e
204400000209  "   * unità.
204500000209  "   * se NON è stato richiesto uno specifico tipo registrazione e
204600000209  "  C                   when      D1TpRegz   =  *Blank  and
204700000209  "   * se l'utente è autorizzato alla contabilità
204800000209  "  C                             XAuAbl     =  *On
204900000209  "   *
205000000209  "   * ma non è autorizzato alle registrazioni definitive e
205100000209  "   * il tipo di registrazione letto è 'D'= definitiva
205200000209  "  C                             and (XAUDef = *Off   and
205300000209  "  C                             RegTpRegz = 'D'      or
205400000209  "   * oppure,
205500000209  "   * ma non è autorizzato alle operazioni provvisorie e
205600000209  "   * il tipo di registrazione letto è 'P'= provvisorio
205700000209  "  C                             XAUProv = *Off       and
205800000209  "  C                             RegTpRegz = 'P'      or
205900000209  "   * oppure,
206000000209  "   * ma non è autorizzato alle operazioni gestionali  e
206100000209  "   * il tipo di registrazione letto è 'G'= gestionale
206200000209  "  C                             XAUGest   =   *Off   and
206300000209  "  C                             RegTpRegz =   'G')
206400000209  "   * scarto il record
206500000209D0440C                   MOVEL     *OFF          $RCDOK
206600000209     C*
206700000209     C                   endsl
206800970821     C*
206900970821     C                   ENDSR
207000970821     C/EJECT
207100970821     C************************************************************
207200970821     C* RIEMPIMENTO RIGA LISTA
207300970821     C************************************************************
207400970821     C     RIES1         BEGSR
207500970821     C*
207600970821     C                   CLEAR                   FMTS1
207700970821     C*
207800970821     C* riempio i campi del sfl, compresi gli hidden con i valori
207900970821     C* del rcd letto
208000970822     C                   Eval      H1Sys     =   RegSys
208100970825     C                   Eval      H1NrAsReg =   RegNrAsReg
208200970901     C                   Eval      H1NrAsCol =   RegNrAsCol
208300970901     C                   Eval      H1FunGen  =   RegFunGen
208400980219     C                   Eval      S1TpRegz  =   RegTpRegz
208500040308C1309X******Dmy          MOVE      RegDtReg      S1DtReg
208600000117C1309C     *JobRun       MOVE      RegDtReg      S1DtReg
208700970822     C                   Eval      S1NrReg   =   RegNrReg
208800970822     C                   Eval      S1SerReg  =   RegSerReg
208900970822     C                   Eval      S1CausTes =   RegCausTes
209000970822     C                   Eval      S1Classe  =   RegClasse
209100970825     C                   Eval      S1Divisa  =   RegDivisa
209200970822     C                   Z-Add     RegImporto    S1Importo
209300980220     C     RegDtUlMo     ifne      *Loval
209400040308C1309X******Dmy          Move      RegDtUlMo     S1DtUlMo
209500000117C1309C     *JobRun       Move      RegDtUlMo     S1DtUlMo
209600970825     C                   endif
209700970825     C                   Eval      S1UteUlMo =   RegUteUlMo
209800980220     C     RegDtDoc      ifne      *Loval
209900040308C1309X******Dmy          Move      RegDtDoc      S1DtDoc
210000000117C1309C     *JobRun       Move      RegDtDoc      S1DtDoc
210100970825     C                   endif
210200970825     C                   Eval      S1NrDoc   =   RegNrDoc
210300970825     C                   Eval      S1SerDoc  =   RegSerDoc
210400970825     C                   Eval      S1KNrAz   =   RegKNrAz
210500970825     C                   Eval      S1Unita   =   RegUnita
210600970825     C*
210700970825     C* se non è stata richiesta una causale vuol dire che devo fare il
210800970825     C* controllo con NdMvc003,
210900970825     C                   if        D1CausTes  =  *Blank
211000970825     c                   Eval      WCausale   =  S1CausTes
211100970825     c                   ExSr      CallMvc003
211200970825     C                   if        Esito     <>  '1'
211300970825     C                   Eval      H1Portaf   =  OpePortaf
211400030220C1627C                   Eval      H1TpDocI   =  OpeTpDocI
211500970825     C                   endif
211600970825     C                   else
211700970825     C* altrimenti prendo il valore già reperito in CtrD1
211800970825     C                   Eval      H1Portaf   =  OpePortaf
211900970825     C                   endif
212000040406C1742 * blocco di riconciliazione
212100060331C1921X* ex C1742 ******* clear                   £BlkTes369
212200060331C1921C                   clear                   £Esito369
212300060331C1921C                   move      '1'           £Operaz369
212400060331C1742C                   z-add     H1Sys         £Sys369
212500040406  "  C                   z-add     H1NrAsReg     £NrAsReg369
212600040406  "  C                   callb     'NDC369R'
212700060331C1742C                   parm                    Kpjba
212800060331C1921X*ex C1742 ******** movel     £BlkTes369    H1BlkTes
212900060331C1921C                   movel     £Esito369     H1BlkTes
213000060331  "   *
213100060331  "   * Reg Intercompany RACcordata e/o RIConciliata
213200060331  "  C                   clear                   H1RegIRac
213300060331  "  C                   clear                   H1RegIRic
213400060331  "  C                   move      '2'           £Operaz369
213500060331  "  C                   clear                   £Esito369
213600060331  "  C                   move      XScSoc        £Societa369
213700060331  "  C                   z-add     H1Sys         £Sys369
213800060331  "  C                   z-add     H1NrAsReg     £NrAsReg369
213900060331  "  C                   callb     'NDC369R'
214000060331  "  C                   parm                    Kpjba
214100060331  "  C                   select
214200060331  "  C                   when      £Esito369 = '2'
214300060331  "  C                   move      *on           H1RegIRac
214400060331  "  C                   when      £Esito369 = '3'
214500060331  "  C                   move      *on           H1RegIRic
214600060331  "  C                   when      £Esito369 = '4'
214700060331  "  C                   move      *on           H1RegIRac
214800060331  "  C                   move      *on           H1RegIRic
214900060331  "  C                   endsl
215000060331C1921 *
215100060331C1742 * memorizzo sulla riga se reg. adatta per opz.04=cancella
215200040406  "  C                   exsr      RieH1Opz04
215300040406  "   * memorizzo sulla riga se reg. adatta per opz.08=modifica data
215400040406  "  C                   exsr      RieH1Opz08
215500040406  "   * memorizzo sulla riga se reg. adatta per opz.22=passaggio def
215600040406C1742C                   exsr      RieH1Opz22
215700980220     C*
215800980220     C* imposto anche l'opzione perchè se avessi premuto l'F13
215900980220     C* dovrei caricare l'opzione da replicare
216000980220     C                   ExSr      CtrOpz
216100980220     C                   if        OpzOk    =    *On
216200980220     C                   Z-Add     WOpz          S1Opz
216300040308     C                   endif
216400980220     C                   if        S1Opz     <>  0
216500980220     C                   SetOn                                        32
216600980220     C                   endif
216700030220C1627 *
216800030220  "   * reperimento dati cliente/fornitore reg. iva
216900030220  "  C                   clear                   H1SNatura
217000030220  "  C                   clear                   H1Kcc
217100030220  "  C                   clear                   H1Ksc
217200030220  "  C                   if        D1Dociva  = '1'
217300030220  "  C                   exsr      RepDatiCliFor
217400030220  "  C                   endif
217500030220  "   *
217600030220  "   * Evidenzio esistenza documenti catalogati su Laguna
217700030220  "   * (se laguna attiva in società e richiesto documenti iva)
217800030220  "  C                   if        XScLaguna = *on  and
217900030220  "  C                             D1Dociva  = '1'
218000030220  "  C                   exsr      RepDatiLGN
218100030220  "  C                   endif
218200030220C1627 *
218300970821     C*
218400970821     C* imposto quegli attributi del sfl che sono modificabili
218500970821     C* al richiamo del determinato pgm
218600970821     C                   EXSR      ATRS1
218700970821     C*
218800970821     C                   ENDSR
218900970821     C/EJECT
219000040406C1742 ************************************************************
219100040406  "   * memorizzo sulla riga se reg. adatta per opz. 4=cancella
219200040406  "   ************************************************************
219300040406  "  C     RieH1Opz04    Begsr
219400040406  "   *
219500040406  "  C                   eval      H1Opz04 = *on
219600040406  "   *
219700040406  "   * se registrazione di emnissione portafoglio
219800040406  "  C                   if        H1Portaf   =  '1'        or
219900040406  "   * non è una reg collegata ed è un'autofattura
220000040406  "  C                             (H1NrAsCol > 0    and
220100040406  "  C                              H1FunGen <> 'A'      )
220200040406  "   *
220300040406  "   *  la reg. non può essere cancellata
220400040406  "  C                   eval      H1Opz04 = *off
220500040406  "   *
220600040406  "  C                   endif
220700040406  "   *
220800040406C1742C     XRieH1Opz04   Endsr
220900040406C1742 ************************************************************
221000040406  "   * memorizzo sulla riga se reg. adatta per opz. 8=modifica data
221100040406  "   ************************************************************
221200040406  "  C     RieH1Opz08    Begsr
221300040406  "   *
221400040406  "  C                   eval      H1Opz08 = *on
221500040406  "   *
221600040406  "   * se registrazione di emnissione portafoglio
221700040406  "  C                   if        H1Portaf   =  '1'        or
221800040406  "   * non è una reg collegata ed è un'autofattura
221900040406  "  C                             (H1NrAsCol > 0    and
222000040406  "  C                              H1FunGen <> 'A'      )
222100040406  "   * oppure la registrazione è iva
222200040406  "  C                             or D1DocIVA =  '1'       or
222300040406  "   * oppure la registrazione non è provvisoria o gestionale
222400040406  "  C                             (S1TpRegz<>'P' and S1TpRegZ<>'G')
222500040406  "   *
222600040406  "   *  la reg. non può cambiare la data registrazione
222700040406  "  C                   eval      H1Opz08 = *off
222800040406  "   *
222900040406  "  C                   endif
223000040406  "   *
223100040406C1742C     XRieH1Opz08   Endsr
223200040406C1742 ************************************************************
223300040406  "   * memorizzo sulla riga se reg. adatta per opz. 22=passaggio def
223400040406  "   ************************************************************
223500040406  "  C     RieH1Opz22    Begsr
223600040406  "   *
223700040406  "  C                   eval      H1Opz22 = *on
223800040406  "   *
223900040406  "   * la registrazione è di tipo IVA oppure
224000040406  "  C                   if        C1DocIVA =  '1'   or
224100040406  "   * è definitiva oppure
224200040406  "  C                             S1TpRegz= 'D'     or
224300040406  "   * registrazione di emissione portafoglio oppure
224400040406  "  C                             H1Portaf   =  '1' or
224500040406  "   * reg con blocco di riconciliazione  oppure
224600040406  "  C                             H1BlkTes = *on    or
224700040406  "   * non c'è l'autorizzazione in base a CONPNO
224800040406  "  C                             (S1TpRegZ='P' and PassaggD='2') or
224900060331C1742C                             (S1TpRegZ='G' and PassaggD='1')
225000060331C1921 * reg intercompany raccordata
225100060331  "  C                             or H1RegIRac = *on
225200060331  "   * reg intercompany con movimento riconciliato
225300060331C1921C                             or H1RegIRic = *on
225400060331C1742 *
225500040406  "   *  la reg. non può essere portata in definitivo
225600040406  "  C                   eval      H1Opz22 = *off
225700040406  "   *
225800040406  "  C                   endif
225900040406  "   *
226000040406C1742C     XRieH1Opz22   Endsr
226100970821     C************************************************************
226200970821     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
226300970821     C************************************************************
226400970821     C     ATRS1         BEGSR
226500970821     C*
226600040308      * in questa routine bisogna attivare indicatori o campi programma
226700040308      * che gestiscono gli attributi di uno più campi di questa videata;
226800040308      * attenzione : non è questo il posto in cui eseguire controlli,
226900040308      *  dovrebbero già arrivare dei flag
227000970821     C*
227100970825     C*
227200970825     C* azzero il bottone dell'errore in manutenzionabilità
227300970825     C                   MOVEL     X'A0'         S1Err
227400970825     C*
227500970825     C* lo valorizzo a seconda dello stato ricevuto o testato
227600970825     C     H1Errore      IFEQ      *On
227700970825     C                   MOVEL     PRES          S1Err
227800970825     C                   ENDIF
227900030220C1627 *
228000030217  "   * se c'è valorizzata l'Id di Laguna
228100030220  "  C                   eval      *in42 = *off
228200030220  "  C                   if        H1IdLgn <> 0
228300030220  "   * nr reg in rosso se il docum è catalogato
228400030220  "   * per questa registrazione (fattura o nota)
228500030220  "  C                   eval      *in42 = *on
228600030217C1627C                   endif
228700970821     C*
228800970821     C                   ENDSR
228900970821     C/EJECT
229000030220C1627 ************************************************************
229100030220  "   * reperimento dati cliente/fornitore reg. iva
229200030220  "   ************************************************************
229300030220  "  C     RepDatiCliFor Begsr
229400030220  "   *
229500030220  "   * reperimento conto principale reg. iva
229600030220  "   *
229700030220  "  C                   eval      LenOut = %Size(NDMOV00F)
229800030220  "  C                   callb     'NDDRVMOV'
229900030220  "  C                   parm                    H1Sys
230000030220  "  C                   parm                    H1NrAsReg
230100030220  "   *    Con CodOper='1' il driver esegue una Chain su NDMOV00F
230200030220  "   *    con una chiave che comprende anche il N°Riga
230300030220  "   *    quindi è necessario passare come parametro il N°Riga
230400030220  "  C                   parm      1             NrRigaM
230500030220  "  C                   parm      'NDMOV'       StrutturaO
230600030220  "  C                   parm                    LenOut
230700030220  "  C                   parm      '1'           Codoper
230800030220  "  C                   parm                    NDMOV00F
230900030220  "   *
231000030220  "   *
231100030220  "   * reperimento conto principale reg. iva ok
231200030220  "  C                   if        Codoper <> '9'
231300030220  "   *
231400030220  "   *    salvo dati che (x ora) mi serviranno (solo) per ProJ Doc
231500030220  "  C                   eval      H1Kcc = MOVKcc
231600030220  "  C                   eval      H1Ksc = MOVKsc
231700030220  "   *
231800030220  "   * reperimento sottonatura conto principale reg. iva
231900030220  "  C                   eval      LenOut = %Size(ND002DS)
232000030220  "  C                   callb     'NDMVC002'
232100030220  "  C                   parm                    XScSoc
232200030220  "  C                   parm                    MOVKcc
232300030220  "  C                   parm                    MOVKsc
232400030220  "  C                   parm                    RegDtReg
232500030220  "  C                   parm      *off          GesMsg
232600030220  "  C                   parm                    Esito
232700030220  "  C                   parm      'ND002DS'     StrutturaO
232800030220  "  C                   parm                    ND002DS
232900030220  "  C                   parm                    LenOut
233000030220  "   *
233100030220  "   * reperimento sottonatura conto principale reg. iva ok
233200030220  "  C                   if        Esito <> '1'
233300030220  "   *
233400030220  "   *  salvo dati che (x ora) mi serviranno (solo) per ProJ Doc
233500030220  "  C                   eval      H1SNatura = SNatura002
233600030220  "   *
233700030220  "  C                   endif
233800030220  "   *
233900030220  "  C                   endif
234000030220  "   *
234100030220C1627C     XRepDatiCliForEndsr
234200030220C1627 ************************************************************
234300030220  "   * reperimento dati sui documenti catalogati in Laguna
234400030220  "   ************************************************************
234500030220  "  C     RepDatiLGN    Begsr
234600030220  "   *
234700030220  "  C                   clear                   H1IdLgn
234800030220  "  C                   clear                   Dati
234900030220  "  C                   clear                   ANLGN00F
235000030220  "   *
235100030220  "   * classe laguna      (sia per cli che per for)
235200030220  "  C                   eval      LGNClassLg = 'DOC'
235300030220  "   * chiave primaria 1° (sia per cli che per for)
235400030220  "  C                   eval      LGNKey101 = XScSoc
235500030220  "   *
235600030220  "   *
235700030220  "  C                   select
235800030220  "   *
235900030220  "   * sottonatura cliente
236000030220  "   * preparo dati kiave per fatture attive
236100030220  "  C                   when      H1SNatura = 'C'
236200030220  "  C                   exsr      DriverLGN_Cli
236300030220  "   *
236400030220  "   * sottonatura fornitore
236500030220  "   * preparo dati kiave per fatture passive
236600030220  "  C                   when      H1SNatura = 'F'
236700030220  "  C                   exsr      DriverLGN_For
236800030220  "   *
236900030220  "  C                   endsl
237000030220  "   *
237100030220  "   *
237200030220  "   * chiamata driver laguna
237300030220  "  C                   exsr      DriverLGN
237400030220  "   *
237500030220  "   * valorizzo la chiave di laguna
237600030220  "  C                   if        Errore = *off
237700030220  "  C                   eval      ANLGN00F = Dati
237800030220  "  C                   eval      H1IdLgn   = LGNIdLgn
237900030220  "  C                   endif
238000030220  "   *
238100030220C1627C     XRepDatiLGN   Endsr
238200030220C1627 ************************************************************
238300030220  "   * preparo dati kiave per fatture attive per driver laguna
238400030220  "   ************************************************************
238500030220  "  C     DriverLGN_Cli Begsr
238600030220  "   *
238700030220  "   *    chiavi per fatture attive
238800030220  "  C                   eval      LGNClassPJ = '030'
238900030220  "   *
239000030220  "   *    chiave primaria 2° + 3° + 4° = riferimenti documento
239100030220  "  C                   movel     '0001-01-01'  LGNKey102
239200030220  "  C                   if        S1DtDoc <> 0
239300030220  "  C     *jobrun       movel     S1DtDoc       DataISO
239400030220  "  C                   movel     DataISO       LGNKey102
239500030220  "  C                   endif
239600030220  "  C                   movel     S1NrDoc       LGNKey103
239700030220  "  C                   eval      LGNKey104 = S1SerDoc
239800030220  "   *
239900030220  "  C                   eval      NrChiavi = 6
240000030220  "   *
240100030220C1627C     XDriverLGN_CliEndsr
240200030220C1627 ************************************************************
240300030220  "   * preparo dati kiave per fatture passive per driver laguna
240400030220  "   ************************************************************
240500030220  "  C     DriverLGN_For Begsr
240600030220  "   *
240700030220  "   *    chiavi per fatture passive
240800030220  "  C                   eval      LGNClassPJ = '020'
240900030220  "   *
241000030220  "   *    chiave primaria 2° = tipo documento (fattura/nota)
241100030220  "  C                   select
241200030220  "   *
241300030220  "   *      fattura
241400060303C1627C                   when      H1TpDocI = '0' or
241500060303D1937C                             H1TpDocI = '2' or
241600060303C1627C                             H1TpDocI = '3' or
241700030220  "  C                             H1TpDocI = '5' or
241800030220  "  C                             H1TpDocI = '6'
241900030220  "  C                   movel(p)  'F'           LGNKey102
242000030220  "   *
242100030220  "   *      nota
242200030220  "  C                   when      H1TpDocI = '1' or
242300030220  "  C                             H1TpDocI = '4' or
242400030220  "  C                             H1TpDocI = '7'
242500030220  "  C                   movel(P)  'N'           LGNKey102
242600030220  "   *
242700030220  "  C                   endsl
242800030220  "   *
242900030220  "   *
243000030220  "   *    chiave primaria 3° + 4° + 5° = riferimenti documento
243100030220  "  C                   movel     '0001-01-01'  LGNKey103
243200030220  "  C                   if        S1DtDoc <> 0
243300030220  "  C     *jobrun       movel     S1DtDoc       DataISO
243400030220  "  C                   movel     DataISO       LGNKey103
243500030220  "  C                   movel     S1NrDoc       LGNKey104
243600030220  "  C                   eval      LGNKey105 = S1SerDoc
243700030220  "  C                   endif
243800030220  "   *
243900030220  "   *    chiave primaria 6° = codice fornitore
244000030220  "  C                   eval      LGNKey106 = H1Ksc
244100030220  "  C                   eval      NrChiavi = 8
244200030220  "   *
244300030220C1627C     XDriverLGN_ForEndsr
244400030220C1627 ************************************************************
244500030220  "   * chiamata driver laguna
244600030220  "   ************************************************************
244700030220  "  C     DriverLGN     Begsr
244800030220  "   *
244900030220  "  C                   movel     ANLGN00F      Dati
245000030220  "  C                   eval      Lunghezza = %size(ANLGN00F)
245100030220  "  C                   callb     'PJDR1701R'
245200030220  "  C                   parm      '01'          Opzione                        1
245300030220  "  C                   parm      *off          Errore                         2
245400030220  "  C                   parm      2             Vista                          5
245500030220  "  C                   parm                    Dati                           3
245600030220  "  C                   parm                    Lunghezza                      4
245700030220  "  C                   parm      *off          Commit                         6
245800030220  "  C                   parm                    NrChiavi                       7
245900030220  "  C                   parm      *off          Alloca                         8
246000030220  "  C                   parm                    *omit                          9
246100030220  "   *
246200030220C1627C     XDriverLGN    Endsr
246300970821     C************************************************************
246400970821     C* GESTIONE F08 VIDEO S1
246500970821     C************************************************************
246600970821     C     F08S1         BEGSR
246700970821     C*
246800970821     C                   MOVE      *ON           $FINE
246900040308     C                   MOVE      '0'           RET013
247000970821     C*
247100970821     C                   ENDSR
247200970821     C/EJECT
247300970821     C************************************************************
247400970821     C* GESTIONE F03 VIDEO S1
247500970821     C************************************************************
247600970821     C     F03S1         BEGSR
247700970821     C*
247800970821     C                   MOVE      *ON           $FINE
247900040308     C                   MOVE      '1'           RET013
248000970821     C* fine programma
248100970821     C                   EXSR      ENDPGM
248200970821     C*
248300970821     C                   ENDSR
248400970821     C/EJECT
248500970827     C************************************************************
248600970827     C* GESTIONE F05 VIDEO S1
248700970827     C************************************************************
248800970827     C     F05S1         BEGSR
248900970827     C*
249000970827     C                   MOVE      *ON           $INZS1
249100970827     C*
249200970827     C                   ENDSR
249300970827     C/EJECT
249400970821     C************************************************************
249500970821     C* GESTIONE F10 VIDEO S1
249600970821     C* AGGIUNTA RECORD
249700970821     C************************************************************
249800970821     C     F10S1         BEGSR
249900970821     C*
250000040308    >X*****              RESET                   STD002DS
250100040308    >X*****              MOVE      xscsoc        Societa002
250200040308    >X*****              MOVEL     '01'          OPZ002
250300040308    >X*    CMT013        IFEQ      '1'
250400040308    >X*                  MOVEL     '1'           CMT002
250500040308     X*                  ELSE
250600040308    >X*                  MOVEL     '0'           CMT002
250700040308     X*                  ENDIF
250800040308     X* passare anche le parzializzazioni ricevute relative alla chiave
250900040308     X* univoca : se ce ne sono saranno usate come default d'immissione
251000040308    >X*                  IF        OPZ013 <> *Blank
251100040308    >X*                  MOVEL     OBJ013        OBJ002
251200040308     X*                  ENDIF
251300040308     X* codice di riferimento per le valorizzazioni di default
251400040308    >X*                  MOVE      $DFn          DF1002
251500040308    >X*                  MOVEL     STD002DS      KPJBU
251600040308    >X*                  CALL      'STD002R'
251700040308     X*                  PARM                    KPJBA
251800040308    >X*                  MOVEL     KPJBU         STD002DS
251900040308     X* ritorno da PGM gestione
252000040308    >X*                  EXSR      RTN002
252100040308     X*    WINZS1        IFEQ      *ON
252200040308     X*                  MOVE      *ON           $INZS1
252300040308     X* se esistevano già righe sul sfl
252400040308     X* calcolo pagina a cui deve posizionarsi
252500040308     X*    WSFL          IFGT      0
252600040308     X*                  EXSR      CLCPAG
252700040308     X* altrimenti carico solo la 1a pagina
252800040308     X*                  ELSE
252900040308     X*                  Z-ADD     SFLPAG        WPAG
253000040308     X*                  ENDIF
253100040308     X*                  ENDIF
253200040308     X*
253300040308     X*                  MOVE      *HIVAL        $LASTV
253400040308     X*
253500970821     C                   ENDSR
253600970821     C/EJECT
253700970821     C************************************************************
253800970821     C* GESTIONE F12 VIDEO S1
253900970821     C************************************************************
254000970821     C     F12S1         BEGSR
254100970821     C*
254200970821     C*  se la 1a videata sono le parzializzazioni, per F12 torno a queste
254300970821     C     $LOP          IFEQ      1
254400970821     C                   MOVE      'D1'          $GEST
254500970821     C*  se la 1a videata è questa, per F12 torno a menu
254600970821     C                   ELSE
254700970821     C                   MOVE      *ON           $FINE
254800040308     C                   MOVE      '2'           RET013
254900970821     C                   ENDIF
255000970821     C*
255100970821     C                   ENDSR
255200970821     C/EJECT
255300970827     C************************************************************
255400970827     C* GESTIONE F13 VIDEO S1
255500970827     C************************************************************
255600970827     C     F13S1         BEGSR
255700980220     C*
255800970827     C* E' simile alla funzione F13 del PDM: riporta l'opzione della
255900970827     C* riga del sfl su cui è posizionato il cursore dalla riga
256000970827     C* successiva fino alla fine del sfl stesso,
256100970827     C* senza sovrapporsi alle opzioni preesistenti.
256200980220     C*
256300980220     C* reset indicatori DSPATR
256400980220     C                   MOVE      *ALL'0'       IN4049           10
256500980220     C                   MOVEA     IN4049        *IN(40)
256600970827     C*
256700970827     C* il controllo lo faccio solo se il sfl NON è vuoto
256800040308     C                   If        WMax    <>    0
256900970827     C* determino Riga/Colonna del cursore
257000970827     C                   MOVE      NRG           R$$
257100970827     C                   MOVE      NCL           C$$
257200970827     C                   Z-ADD     RIRI          CURR
257300970827     C                   Z-ADD     COCO          CURC
257400970827     C*
257500970827     C* se sono posizionato sul sfl
257600040308     C     H1NmRc        IFEQ      'C013S1'
257700970827     C* salvo il numero del record su cui sono posizionato
257800060904D2045X*********          Z-Add     C1Nrr         WC1Rcd            3 0
257900060904D2045c                   Z-Add     C1Nrr         WC1Rcd            4 0
258000970827     C* leggo il rcd
258100970827     C     C1NrR         CHAIN     FMTS1                              21
258200970827     C* salvo l'opzione e il numero relativo
258300970827     C                   If        S1Opz   >     0
258400970827     C                   Z-Add     S1Opz         WOpz              2 0
258500060904D2045X***********        Z-Add     C1Nrr         $X                3 0
258600060904D2045c                   Z-Add     C1Nrr         $X                4 0
258700040407C1742 *
258800040407  "   * l'opzione non viene ripetuta sulle righe successive
258900040407  "   * che non hanno le caratteristiche per essere trattate
259000040407  "   * con l'opzione stessa, quindi faccio la stessa cosa sulla
259100040407  "   * riga sulla quale è stato fatto f13
259200040407  "  C                   exsr      CtrOpz
259300040407  "  C                   if        OpzOk = *off
259400040407  "  C                   z-add     0             S1Opz
259500040407  "  C                   endif
259600040407C1742 *
259700970829     C* attivo SflNxtChg
259800970829     C                   SetOn                                        32
259900970829     C                   UpDate    FmtS1
260000970829     C                   SetOff                                       32
260100970827     C*
260200970827     C* mi posiziono sul record successivo
260300970827     C                   Add       1             $X
260400970829     c*
260500040308     C                   DoW       $X      <=    WMax
260600970827     C     $X            Chain     FmtS1                              21
260700970827     C   21              Leave
260800040308     C                   if        S1Opz    =    0
260900980219     C* imposto l'opzione solo se è possibile eseguirla
261000980219     C                   Exsr      CtrOpz
261100040407     C                   if        OpzOk    =    *On
261200970827     C                   Z-Add     WOpz          S1Opz
261300040308     C                   endif
261400040308     C                   endif
261500970829     C* attivo SflNxtChg
261600970829     C                   SetOn                                        32
261700970829     C                   UpDate    FmtS1
261800970829     C                   SetOff                                       32
261900970829     C*
262000970827     C                   Add       1             $X
262100040308     C                   enddo
262200970827     c*
262300970827     C                   Eval      SflPag   =    9999
262400970827     C                   Eval      WPag     =    9999
262500970827     C*
262600970827     C                   ExSr      RolS1
262700970827     C* reimposto sflpag
262800970827     C                   Eval      SflPag   =    8
262900970827     C                   Eval      WPag     =    SflPag
263000970827     C*
263100970827     C* ripristino la posizione iniziale del record
263200970827     c                   Z-Add     WC1Rcd        C1Rcd
263300970827     C* ripristino il campo di salvataggio dell'opzione per non sporca
263400970827     C* re il campo S1Opz in caso di ricaricamento del sfl
263500970827     C                   Z-Add     0             WOpz
263600970827     C*
263700040308     C                   endif
263800970827     C*
263900040308     C                   endif
264000970827     C*
264100040308     C                   endif
264200970827     C*
264300970827     C                   ENDSR
264400970827     C/EJECT
264500970821     C************************************************************
264600980219     C* Controllo opzione da ripetere con l'F13
264700970821     C************************************************************
264800980219     C     CtrOpz        BEGSR
264900970821     C*
265000980219     C                   Eval      OpzOk     =  *On
265100980219     C*
265200980219     C                   Select
265300980219     C* se l'opzione è 4=Cancella
265400980219     C                   When      WOpz      =   04
265500040407C1742 *
265600040407  "   * se la reg. non può essere cancellata
265700040407C1742C                   if        H1Opz04 = *off
265800040407C1742X**se registrazione non manutenzionabile oppure
265900040407  "  X****************** if        H1Portaf   =  '1'                      or
266000040407  "  X**se è collegata ad un'altra e NON è la prima nota collegata(?????)
266100040407C1742X******************           (H1NrAsCol > 0   and  H1FunGen <> 'A')
266200980219     C                   Eval      OpzOk     =  *Off
266300980219     C                   endif
266400980219     C*
266500980219     C* se l'opzione è 8=Modifica data registrazione e
266600980219     C                   When      WOpz      =   08
266700040407C1742 *
266800040407  "   * se la reg. non può essere modificata nella dat reg
266900040407C1742C                   if        H1Opz08 = *off
267000040407C1742X**la registrazione è di tipo IVA oppure
267100040407  "  X****************** if        C1DocIVA =  '1'                        or
267200040407  "  X**NON è nè provvisoria nè gestionale oppure
267300040407  "  X******************           (S1TpRegz <> 'P' and  S1TpRegz <> 'G') or
267400040407  "  X**se registrazione non manutenzionabile oppure
267500040407  "  X******************           H1Portaf   =  '1'                      or
267600040407  "  X**se è collegata ad un'altra e NON è la prima nota collegata(?????)
267700040407C1742X******************           (H1NrAsCol > 0   and  H1FunGen <> 'A')
267800980219     C                   Eval      OpzOk     =  *Off
267900980219     C                   endif
268000980219     C*
268100980219     C* se l'opzione è 22=Passaggio a definitivo  e
268200980219     C                   When      WOpz      =   22
268300040407C1742 *
268400040407  "   * se la reg. non può essere passata in definitivo
268500040407C1742C                   if        H1Opz22 = *off
268600040407C1742X**la registrazione è di tipo IVA oppure
268700040407  "  X****************** if        C1DocIVA =  '1'         or
268800040407  "  X**NON è provvisoria oppure
268900040407  "  X******************           S1TpRegz <> 'P'         or
269000040407  "  X**se registrazione non manutenzionabile
269100040407C1742X******************           H1Portaf   =  '1'
269200980219     C                   Eval      OpzOk     =  *Off
269300980219     C                   endif
269400980219     C*
269500980219     C                   endsl
269600980219     C*
269700980219     C                   ENDSR
269800980219     C/EJECT
269900980219     C************************************************************
270000980219     C* GESTIONE F17 VIDEO S1
270100980219     C************************************************************
270200980219     C     F17S1         BEGSR
270300980219     C*
270400970821     C                   MOVE      'D1'          $GEST
270500970821     C* se il pgm è stato richiamato con chiave univoca
270600970821     C* e dopo è stata richiesta una parzializzazione
270700970821     C* tolgo la gestione della chiave univoca (azzere/abblenco il
270800970821     C* valore; propongo come ordinamento il 1°)
270900970821     C     $Unique       IFEQ      *ON
271000970821     C                   MOVE      *OFF          $Unique
271100040308    >X******             MOVE      *BLANK        UQ1013
271200040308    >X******             MOVE      *BLANK        UQ2013
271300040308    >X*****              Z-ADD     0             UQ3013
271400970821     C                   ENDIF
271500970821     C     $ORD          IFEQ      99
271600970821     C                   Z-ADD     1             $ORD
271700970821     C                   MOVE      *ON           $INZD1
271800970821     C                   ENDIF
271900970821     C*
272000970821     C                   ENDSR
272100970821     C/EJECT
272200970821     C************************************************************
272300970821     C* GESTIONE F18 VIDEO S1
272400970821     C************************************************************
272500970821     C     F18S1         BEGSR
272600970821     C*
272700970821     C                   MOVE      'D2'          $GEST
272800970821     C                   MOVEL     *ON           $INZD2
272900970821     C     $ORD          IFEQ      99
273000970821     C                   Z-ADD     1             $ORD
273100970821     C                   ENDIF
273200970821     C*
273300970821     C                   ENDSR
273400970821     C/EJECT
273500970821     C************************************************************
273600970821     C* GESTIONE F23 VIDEO S1
273700970821     C************************************************************
273800970821     C     F23S1         BEGSR
273900970821     C*
274000970821     C                   Z-ADD     $C1Op1        $L01
274100970821     C                   Z-ADD     $C1Op2        $L02
274200970821     C                   EXSR      RIEOPZ
274300970821     C                   Z-ADD     23            X
274400970821     C                   MOVE      FF23          $FC2(X)
274500970821     C                   MOVEL     WOPZ1         C1OPZ1
274600060224C1913C                   MOVEL     $ALLOPT       H1ALLOPT
274700970821     C                   MOVEL     WOPZ2         C1OPZ2
274800970821     C                   Z-ADD     WCOZ          C1COZ
274900970821     C*
275000970821     C                   ENDSR
275100970821     C/EJECT
275200970821     C************************************************************
275300970821     C* GESTIONE F24 VIDEO S1
275400970821     C************************************************************
275500970821     C     F24S1         BEGSR
275600970821     C*
275700970821     C                   MOVEA     $FX2          $FX
275800970821     C                   MOVEA     $FC2          $FC
275900970821     C                   MOVEA     $FR2          $FR
276000970821     C                   Z-ADD     $ULKS1        $ULK
276100970821     C                   Z-ADD     $S1L01        $L01
276200970821     C                   Z-ADD     $S1L02        $L02
276300970821     C                   EXSR      RIEKEY
276400970821     C                   Z-ADD     $ULK          $ULKS1            3 0
276500970821     C                   MOVEA     $FC           $FC2
276600970821     C                   MOVEL     $KEY1         Z2KE1
276700060224C1913C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
276800970821     C                   MOVEL     $KEY2         Z2KE2
276900970821     C                   WRITE     FMTZ2
277000970821     C*
277100970821     C                   ENDSR
277200970821     C/EJECT
277300970821     C************************************************************
277400970821     C* CONTROLLO TESTATA LISTA
277500970821     C************************************************************
277600970821     C     CTRC1         BEGSR
277700970821     C*
277800970821     C                   MOVE      *OFF          *IN99
277900970821     C*
278000970821     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
278100970821     C* L'INIZIALIZZAZIONE LISTA
278200970821     C*
278300970822     C* controllo campo data di posizionamento sulla lista
278400980219     C     C1PosDt       ifne      *Zero
278500040308C1309X******Dmy          Test(d)                 C1PosDt                21
278600000117C1309C     *JobRun       Test(d)                 C1PosDt                21
278700040308     C     *In21         ifeq      *On
278800970822     c* non corretta
278900040308     C                   SetOn                                        5099
279000970826     C                   else
279100970826     C* Ok
279200040308C1309X******Dmy          Move      C1PosDt       WPosData
279300000117C1309C     *JobRun       Move      C1PosDt       WPosData
279400970826     C
279500040308     C                   endif
279600970826     C                   else
279700970826     C                   Eval      WPosData   =  *Loval
279800040308     C                   endif
279900980219     C*
280000980219     C* controllo campo tipo registrazione di posizionamento sulla lista
280100980219     C                   Clear                   X07ValDs
280200980219     C                   Move      '1'           X07Ric
280300980219     C                   Move      '1'           X07All
280400980219     C                   Movel     'NDREG000'    X07TRc
280500980219     C                   Movel     'REGTPREGZ'   X07Cam
280600980219     C                   Movel     C1Pos         X07Val
280700980219     C                   Callb     'X07VALR'
280800980219     C                   Parm                    X07ValDS
280900040308     C                   if        X07Err     =  '1'
281000980219     C* valore non valido
281100040308     C                   SetOn                                        5799
281200040308     C                   endif
281300970822     C*
281400970822     C*
281500970821     C     C1POS         IFNE      W1POS
281600040308     C* aggiungere in OR altri campi posizionamento se ci sono
281700040308     C     C1PosDt       OrNE      W1PosDt
281800970821     C                   EXSR      POSS1
281900970821     C                   ENDIF
282000970821     C*
282100970821     C                   ENDSR
282200970821     C/EJECT
282300970821     C************************************************************
282400970821     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
282500970821     C************************************************************
282600970821     C     POSS1         BEGSR
282700970821     C*
282800970821     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
282900970821     C*
283000040308     C     WMAX          IFGT      0
283100970821     C                   READC     FMTS1                                  21
283200040308     C     *IN21         DOWEQ     *OFF
283300970821     C     *IN99         ANDEQ     *OFF
283400970821     C*
283500040308     C     S1OPZ         IFNE      0
283600970821     C                   SETON                                        92  99
283700040308     C                   ENDIF
283800970821     C* se rilevata opzione attiva, attivo sflnxtchg
283900040308     C     *IN99         IFEQ      *ON
284000970821     C                   MOVE      *ON           *IN32
284100970821     C*
284200970821     C* imposto quegli attributi del sfl che sono modificabili
284300970821     C* al richiamo del determinato pgm
284400970821     C                   EXSR      ATRS1
284500970821     C*
284600970821     C                   UPDATE    FMTS1
284700040308     C                   ELSE
284800970821     C                   READC     FMTS1                                  21
284900040308     C                   ENDIF
285000970821     C*
285100040308     C                   ENDDO
285200970821     C*
285300040308     C                   ENDIF
285400970821     C*
285500970821     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
285600970821     C* AL VALORE DI POSIZIONAMENTO :
285700970821     C* se questo è maggiore del valore del rispettivo campo di
285800970821     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
285900970821     C* con i valori di parzializzazione.
286000970821     C*
286100040308     C     *IN99         IFEQ      *OFF
286200040308     C                   MOVE      *BLANK        W1POS
286300040308     C                   MOVE      *Zeros        W1PosDt
286400970821     C*
286500970821     C* per ogni ordinamento devo testare se il campo dell'inizio elenco
286600970821     C* è maggiore del relativo valore di parzializzazione, nel qual caso
286700970821     C* imposto la variabile del posizionamento (W1Pos) al nuovo valore;
286800970821     C* richiedo comunque un ricaricamento del sfl che, se non si è
286900970821     C* verificata la condizione suddetta avverrà con posizionamento a
287000970821     C* partire dal valore di parzializzazione
287100970821     C*
287200040308     C                   SELECT
287300040308      * Attenzione : se un ordinamento ha un campo di posizionamento
287400040308      * specifico bisogna sostituire la gestione W1Pos/C1Pos con quella
287500040308      * dei relativi campi
287600970821     C*
287700040308     C     $ORD          WHENEQ    1
287800040308     C     C1POS         IFGT      D1TpRegz
287900040308     C                   MOVEL     C1POS         W1POS
288000040308      * se uno di questi codici prevede l'uso di XALLINEA
288100040308      * bisogna metterlo qui
288200980219     C                   ENDIF
288300970821     C*
288400040308     C     $ORD          WHENEQ    2
288500970822     C* se la data di posizionam. è inferiore alla data finale di registrazi
288600970826     C* one digitata la salvo
288700040308     C     WPosData      ifgt      WDtRegDa
288800040308C1309X******Dmy          Move      WPosData      W1PosDt
288900000117C1309C     *JobRun       Move      WPosData      W1PosDt
289000040308     C                   Endif
289100970821     C*
289200970821     C* per l'ordinamento 99 = POSTA non viene gestito il posizionamento
289300970821     C*
289400040308     C                   ENDSL
289500970821     C*
289600970821     C* richiedo l'inizializzazione sfl
289700970821     C                   MOVE      *ON           $INZS1
289800970821     C*
289900040308     C                   ENDIF
290000970821     C*
290100970821     C                   ENDSR
290200970821     C/EJECT
290300970821     C************************************************************
290400970821     C* CONTROLLO OPZIONI LISTA
290500970821     C************************************************************
290600970821     C     CTRS1         BEGSR
290700970821     C*
290800970821     C                   MOVEL     *OFF          $ESCI
290900970821     C                   SETOFF                                       99
291000970821     C                   Z-ADD     0             S1OPZ
291100970821     C*
291200970821     C* Leggo il sfl solo se ci sono rcd
291300040308     C     WMAX          IFGT      0
291400970821     C                   READC     FMTS1                                  21
291500970828     C*
291600970828     C* imposto che in questo ciclo di lettura devo ancora aprire la finestr
291700970828     C* di richiesta nuova data
291800970828     C                   Eval      OpenWdw   =   *Off
291900970903     C*
292000970903     C* imposto che in questo ciclo di lettura devo ancora aprire la finestr
292100970903     C* di richiesta nuova data
292200970903     C                   Eval      OpenWdw6  =   *Off
292300970828     C*
292400970828     C* imposto che in questo ciclo di lettura devo ancora aprire la finestr
292500970828     C* di messaggio
292600970828     C                   Eval      OpenMsg   =   *Off
292700970828     C*
292800970828     C* imposto che in questo ciclo di lettura deve ancora verificarsi un
292900970828     C* errore in cancellazione
293000970828     C                   Eval      ErroreCanc=   *Off
293100970828     C*
293200970828     C* imposto che in questo ciclo di lettura deve ancora verificarsi un
293300970828     C* errore in modifica
293400970828     C                   Eval      ErroreMod =   *Off
293500970902     C*
293600970902     C* imposto che in questo ciclo di lettura deve ancora verificarsi un
293700970902     C* F12 dal 2 di modifica pulendo la Ds di modifica
293800970902     C                   Reset                   Ndc001Ds
293900970821     C*
294000970821     C* esce se fine sfl o errore che richiede l'uscita
294100040308     C     *IN21         DOWEQ     *OFF
294200970821     C     $ESCI         ANDEQ     *OFF
294300970902     C                   Z-ADD     S1NRR         C1RCD
294400970902     C*
294500970902     C* pulisco i campi di appoggio per la stampa
294600970902     C                   Clear                   OldDtReg
294700970902     C                   Clear                   OldTpReg
294800970821     C* ctrl su riga
294900970821     C                   EXSR      RECS1
295000970821     C* gestione opzioni
295100040308     C     S1OPZ         IFNE      0
295200970821     C     *IN99         ANDEQ     *OFF
295300970821     C                   EXSR      OPZS1
295400040308     C                   ENDIF
295500970821     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
295600040308     C     *IN99         IFEQ      *ON
295700970821     C                   MOVE      *ON           *IN32
295800040308     C                   ENDIF
295900970821     C*
296000970821     C* imposto quegli attributi del sfl che sono modificabili
296100970821     C* al richiamo del determinato pgm
296200970821     C                   EXSR      ATRS1
296300970821     C*
296400970821     C                   Z-ADD     0             S1OPZ
296500970821     C*
296600970821     C                   UPDATE    FMTS1
296700970821     C                   MOVE      *OFF          *IN32
296800970821     C*
296900970821     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
297000040308     C     $ESCI         IFEQ      *OFF
297100970821     C                   READC     FMTS1                                  21
297200970821     C* a fine ciclo ripristino il flag richiesta iniz.sfl
297300040308     C     *IN21         IFEQ      *ON
297400040308     C                   MOVE      WINZS1        $INZS1
297500040308     C* calcolo pagina a cui deve posizionarsi
297600040308     C     WSFL          IFGT      0
297700040308     C                   EXSR      CLCPAG
297800040308     C                   ELSE
297900040308     C                   Z-ADD     SFLPAG        WPAG
298000040308     C                   ENDIF
298100040308     C                   ENDIF
298200040308     C                   ENDIF
298300040308     C*
298400040308     C                   ENDDO
298500040308     C* se quando esco dal ciclo si sono verificati errori in modifica o
298600040308     C* in cancellazione lo segnalo
298700970828     C                   if        ErroreMod  =  *On
298800970828     C                   SetOn                                        7199
298900970828     C                   endif
299000040308     C                   if        ErroreCanc =  *On
299100040308     C                   SetOn                                        7099
299200040308     C                   endif
299300040308     C* se quando esco dal ciclo NON si sono verificati errori
299400040308     C* ricarico il sfl
299500040308     C                   if        *In99      =  *Off  and
299600040308     C                             ErroreMod  =  *Off  and
299700040308     C                             ErroreCanc =  *Off  and
299800040308     C                             Ret001     <> '2'
299900040308     C                   Eval      $InzS1     =  *On
300000040308     C                   endif
300100040308     C*
300200040308     C                   ENDIF
300300040308     C*
300400040308     C                   ENDSR
300500040308     C/EJECT
300600040308     C************************************************************
300700970821     C* CONTROLLO CAMPI I/O RIGA LISTA
300800970821     C************************************************************
300900970821     C     RECS1         BEGSR
301000970821     C*
301100970821     C* reset indicatori DSPATR
301200970821     C                   MOVE      *ALL'0'       IN4049           10
301300970821     C                   MOVEA     IN4049        *IN(40)
301400970821     C*
301500970821     C* Controllo se Utente Autorizzato alla scelta
301600970821     C     S1OPZ         IFNE      0
301700970821     C                   Z-ADD     1             X
301800970821     C     S1OPZ         LOOKUP    $OP1(X)                                21
301900970821     C* mi salvo l'indice dell' opzione
302000970821     C   21              Z-add     X             PosOpz            2 0
302100980219     C*
302200980219     C* segnalo l'errore
302300980219     C                   Select
302400980219     C*
302500980219     C* se l'opzione NON è tra quelle considerate nella schiera caricata in
302600980219     C* tempo di compilazione oppure se NON è stata abilitata (per ragioni
302700980219     C* di autorizzazioni)
302800980219     C                   When      *In21    =    *OFF    or
302900980219     C                             $FL1(X)  =    'N'
303000980219     C                   MOVE      *ON           $ESCI
303100980219     C                   SETON                                        409199
303200980219     C*
303300980219     C* se l'opzione è 8=Modifica data registrazione e
303400980219     C* la registrazione è di tipo IVA oppure NON è provvisoria nè
303500980219     C* gestionale
303600000209D0440 * N.B. Non è corretto basare i controlli seguenti su D1TpRegz,
303700000209  "   *      perchè il suo valore potrebbe essere blank.
303800000209  "   *      Occorre controllare S1TpRegz.
303900000209D0440 *
304000980219     C                   When      S1Opz     =   08                      and
304100040407C1742C                             H1Opz08 = *off
304200040407C1742X**************************** (D1DocIVA =  '1'       or
304300040407  "  X** ex D0440X** ************* (D1TpRegz <> 'P' and D1TpRegz <> 'G '))
304400040407C1742X** ex D0440C *************** (S1TpRegz <> 'P' and S1TpRegz <> 'G'))
304500980219     C                   MOVE      *ON           $ESCI
304600980219     C                   SETON                                        409199
304700980219     C*
304800040326C1742X* se l'opzione è 22=Passaggio a definitivo e
304900040326  "  X* la registrazione è di tipo IVA oppure NON è provvisoria
305000040326  "  X****************** When      S1Opz     =   22         and
305100040326  "  X******************           (D1DocIVA =  '1'   or
305200040326  "  X*** ex 'D0440X*'**            D1TpRegz <> 'P'      )
305300040326  "  X*** ex 'D0440C '**            S1TpRegz <> 'P'      )
305400040326  "  X****************** MOVE      *ON           $ESCI
305500040326C1742X****************** SETON                                        409199
305600040326C1742 *
305700040406  "   * 22=Passaggio a definitivo...
305800040326  "  C                   when      S1Opz = 22
305900040326  "   *
306000040326  "  C                   select
306100040326  "   *
306200040406  "   * ...se registrazione tipo IVA
306300040406  "   *    oppure Definitiva
306400040406  "   *    oppure di Portafoglio
306500040406  "   *    oppure con blocco di riconciliazione...
306600040406  "  C                   when      D1DocIVA = '1'
306700040406  "  C                             or  S1TpRegz = 'D'
306800040406  "  C                             or  H1Portaf = '1'
306900040406  "  C                             or  H1BlkTes = '1'
307000040326  "  C                   move      *on           $Esci
307100040406  "   *    ...PRO0004 - L'opzione non è valida
307200040326  "  C                   eval      *in91 = *on
307300040326  "  C                   eval      *in40 = *on
307400040326  "  C                   eval      *in99 = *on
307500040326  "   *
307600040406  "   * ..se reg.tipo Provvisoria ma si è autorizzati solo per le Gest.
307700040406  "   *  oppure
307800040406  "   * ..se reg.tipo Gestionale ma si è autorizzati solo per le Provv.
307900040326  "  C                   when      (S1TpRegZ='P' and PassaggD='2') or
308000040326  "  C                             (S1TpRegZ='G' and PassaggD='1')
308100040406  "   *   ...PRO1127 - Utente non abilitato alla funzione
308200040406  "   *                in base alle impostazioni di &1 (CONPNO)
308300040326  "  C                   eval      P1PRO1127 = 'CONPNO'
308400040326  "  C                   eval      *in90 = *on
308500040326  "  C                   eval      *in40 = *on
308600040326  "  C                   eval      *in99 = *on
308700040326  "   *
308800040326C1742C                   endsl
308900980219     C*
309000980219     C                   other
309100970827     C* Controllo autorizzazioni solo se  è un'opzione a cui sono abilitato
309200970827     C                   EXSR      CtrAut
309300980219     C*
309400980219     C                   endsl
309500980219     C*
309600970821     C                   ENDIF
309700970821     C*
309800970822    > * introdurre eventuali controlli
309900970821     C*
310000970821     C                   ENDSR
310100970821     C/EJECT
310200970825     C************************************************************
310300970825     C* Controllo autorizzazione
310400970825     C************************************************************
310500970825     C     CtrAut        BEGSR
310600970825     C*
310700970825     C* richiamo XAutReg a seconda del documento e dell'opzione
310800970825     C* passando i campi di autorizzazione relativi alla opzione richiesta
310900970825     C                   EXSR      CallXAR
311000970825     C*
311100970825     C* se non sono abilitato
311200970825     C                   IF        XARAbil = *off
311300970825     C                   SetOn                                        6899
311400970825     C* metto in reverse image il campo d'opzione
311500970825     C                   SetOn                                        40
311600970825     C                   ENDIF
311700970825     C*
311800970825     C                   ENDSR
311900970825     C/EJECT
312000970825     C************************************************************
312100970825     C* Chiamata a XAUTREG
312200970825     C************************************************************
312300970825     C     CallXAR       BEGSR
312400970825     C*
312500970827     C                   Reset                   XAutRegDs
312600970825     C* valorizzo le autorizzazioni per il tipo documento (IVA - non IVA)
312700970825     C                   SELECT
312800970825     C*
312900970825     C* documenti non IVA
313000970825     C                   WHEN      C1DocIVA  =   '0'
313100970825     C                   MOVE      LvRNon        XARAutLivR
313200970825     C* reperisco le autorizzazioni per l'opzione richiesta
313300970825     C                   SELECT
313400970825     C*  opzione = modifica
313500970825     C                   WHEN      S1Opz     =     2    or
313600970825     C*                 opzione = modifica data registrazione
313700970903     C                             S1Opz     =     8   or
313800970903     C*            oppure passaggio provvisorio definitivo
313900970903     C                             S1Opz     =    22
314000970825     C                   EVAL      XARAutTot =   ManNonT
314100970825     C                   EVAL      XARAutGru =   ManNonG
314200970825     C                   EVAL      XARAutPrf =   ManNonP
314300970825     C                   EVAL      XARAutLis =   ManNonL
314400970825     C*  opzione = cancella
314500970825     C                   WHEN      S1Opz     =   4
314600970825     C                   EVAL      XARAutTot =   AnnNonT
314700970825     C                   EVAL      XARAutGru =   AnnNonG
314800970825     C                   EVAL      XARAutPrf =   AnnNonP
314900970825     C                   EVAL      XARAutLis =   AnnNonL
315000970825     C                   ENDSL
315100970825     C*
315200970825     C* documenti IVA
315300970825     C                   WHEN      C1DocIVA  =   '1'
315400970825     C                   MOVE      LvRIVA        XARAutLivR
315500970825     C* reperisco le autorizzazioni per l'opzione richiesta
315600970825     C                   SELECT
315700970825     C*  opzione = modifica
315800970825     C                   WHEN      S1Opz     =    2      or
315900970825     C*                 opzione = modifica data registrazione
316000970827     C                             S1Opz     =    8
316100970825     C                   EVAL      XARAutTot =   ManIVAT
316200970825     C                   EVAL      XARAutGru =   ManIVAG
316300970825     C                   EVAL      XARAutPrf =   ManIVAP
316400970825     C                   EVAL      XARAutLis =   ManIVAL
316500970825     C*  opzione = cancella
316600970825     C                   WHEN      S1Opz     =   4
316700970825     C                   EVAL      XARAutTot =   AnnIVAT
316800970825     C                   EVAL      XARAutGru =   AnnIVAG
316900970825     C                   EVAL      XARAutPrf =   AnnIVAP
317000970825     C                   EVAL      XARAutLis =   AnnIVAL
317100970825     C                   ENDSL
317200970825     C*
317300970825     C                   ENDSL
317400970825     C*
317500970825     C                   EVAL      XARSocieta =  XscSoc
317600970825     C                   EVAL      XARAmbito  =  '1'
317700970825     C                   EVAL      XARUtenteR =  XscCUt
317800970825     C                   MOVE      XscGrp        XARGruppoR
317900970825     C                   EVAL      XARUtenteP =  RegUteUlMo
318000970825     C                   EVAL      XARLivREnt =  RegLivRise
318100970825     C                   CALL      'XAUTREG'
318200970827     C                   PARM                    XAutRegDs
318300970825     C*
318400970825     C                   ENDSR
318500970825     C/EJECT
318600970821     C************************************************************
318700970821     C* GESTIONE OPZIONI LISTA
318800970821     C************************************************************
318900970821     C     OPZS1         BEGSR
319000970821     C*
319100970821     C                   Z-ADD     RIRI          CURR
319200970821     C                   Z-ADD     COCO          CURC
319300970821     C*
319400970821     C                   SELECT
319500000128C1315 *
319600000128  "   * se NrAsReg=0 la riga non può essere manutenibile
319700000128  "  C                   WHEN      S1Opz    <> 0     AND
319800000128  "  C                             H1NrAsReg = 0
319900000128  "   * Window msg bloccante
320000000128  "  C                   EXSR      WinNrAsRe0
320100000128C1315 *
320200970825     C*
320300970825     C* se opz=Modifica
320400040308     C     S1OPZ         WHENEQ    2
320500970825     C* richiamo lista primanota in manutenzione
320600970825     C                   Reset                   Ndc001Ds
320700970825     C                   Eval      DtReg1001  =  *Loval
320800970825     C                   Eval      DtReg2001  =  *Loval
320900970825     C                   Eval      DtLGio1001 =  *Loval
321000970825     C                   Eval      DtLGio2001 =  *Loval
321100970825     C                   Eval      DtUlMo1001 =  *Loval
321200970825     C                   Eval      DtUlMo2001 =  *Loval
321300970825     C                   Eval      DtDoc001   =  *Loval
321400970825     C                   Movel     '02'          Opz001
321500970825     C* passare nei campi appositi della DS Ndc001Ds il valore
321600970825     C* della chiave della VL univoca
321700970825     C                   Movel     H1Sys         UQ1001
321800970825     C                   Movel     H1NrAsReg     UQ2001
321900970825     C*
322000970825     C                   Movel     Ndc001Ds      KPJBU
322100970825     C                   Call      'NDC001R'
322200970825     C                   Parm                    Kpjba
322300970825     C                   Movel     KPJBU         Ndc001Ds
322400970825     C* ritorno da PGM gestione
322500970825     C                   EXSR      RTN001
322600040308     C*
322700040308     C* se opz=Cancella
322800040308     C     S1OPZ         WHENEQ    4
322900040406C1742 *
323000040406  "   * se la reg. non può essere cancellata
323100040406C1742C                   if        H1Opz04 = *off
323200040406C1742X******************
323300040406  "  X**se la registrazione non è manutenzionabile oppure
323400040406  "  X****************** if        H1Portaf   =  '1'        or
323500040406  "  X**se è collegata ad un'altra  e
323600040406  "  X******************           (H1NrAsCol > 0    and
323700040406  "  X**non è una prima nota collegata (?????? )
323800040406C1742X******************            H1FunGen <> 'A'      )
323900040406C1742 *
324000040308     C* allora segnalo l'errore
324100040308     C                   Eval      ErroreCanc=   *On
324200970828     C                   Eval      H1Errore  =   *On
324300970828     C*
324400040406C1742 * se la reg. può essere cancellata
324500040308     C                   else
324600040308     C*
324700040308     C* chiedo la conferma a procedere con la cancellazione se è la prima
324800970828     C* volta nella lettura del ciclo che incontro opzione 4 oppure non è
324900970828     C* stata data conferma
325000970828     C                   if        OpenMsg   =   *Off
325100970828     C                   ExSr      MsgPgm
325200970901     C* imposto apertura finestra messaggio
325300970901     C                   Eval      OpenMsg   =   *On
325400970828     C                   endif
325500970828     C*
325600970828     C                   Select
325700970828     C* Annulla richiesta
325800970828     C                   When      MsgRtn    =   'N'   or
325900970828     C                             MsgRtn    =   'n'
326000970828     C*                    Non faccio nulla
326100970828     C*
326200970828     C* Conferma cancellazione
326300970828     C                   When      MsgRtn    =   'S'   or
326400970828     C                             MsgRtn    =   's'
326500970825     C* richiamo il modulo per cancellare la registrazione
326600970825     C                   ExSr      CallMvc116
326700040308     C     ESITO         IFEQ      *On
326800040308     C* se non può essere cancellata
326900040308     C                   Eval      ErroreCanc=   *On
327000040308     C                   ENDIF
327100040308     C*
327200040308     C                   EndSl
327300040308     C*
327400040308     C* imposto il bottone di Errore o la riga vuota in caso di cancellazion
327500040308     C* andata a buon fine
327600040308     C                   ExSr      AggS1
327700040308     C*
327800040308     C                   endif
327900970821     C*
328000970828     C*
328100970825     C* se opz=Modifica data registrazione
328200970825     C     S1OPZ         WHENEQ    8
328300040406C1742 *
328400040406  "   * se la reg. non può essere modificata nella dat reg
328500040406C1742C                   if        H1Opz08 = *off
328600040406C1742X**se registrazione non manutenzionabile oppure
328700040406  "  X****************** if        H1Portaf   =  '1'        or
328800040406  "  X**se è collegata ad un'altra  e
328900040406  "  X******************           (H1NrAsCol > 0    and
329000040406  "  x**non è una prima nota collegata (????)
329100040406C1742X******************            H1FunGen <> 'A'      )
329200970828     C* segnalo l'errore
329300970828     C                   Eval      ErroreMod =   *On
329400970828     C                   Eval      H1Errore  =   *On
329500040406C1742 *
329600040406C1742 * se la reg. può essere modificata nella dat reg
329700970825     C                   else
329800970828     C*
329900970828     C* metto a video la finestra per reperire la nuova data solo se è la
330000970828     C* prima volta nel ciclo di lettura del sfl che incontro l'opzione 8
330100970828     C* o che comunque non ho ancora confermato una nuova data
330200970828     C                   if        OpenWdw    =  *Off
330300970828     C                   Eval      Conferma   =  *Off
330400970827     C                   move      *On           $Inzd5
330500970827     C                   MOVE      'D5'          $GEST                          fmt in gestione
330600970828     C                   Dow       $Gest      =  'D5'
330700970827     C                   EXSR      GesD5
330800970902     C* segnalo che in questo ciclo ho aperto la window
330900970902     C                   Eval      OpenWdw    =  *On
331000970827     C                   EndDo
331100970828     C                   endif
331200970828     C*
331300970827     C* se è stato premuto l'F06 di conferma
331400970828     C                   if        Conferma   =  *On
331500970827     C* richiamo il modulo per modificare le date di registrazione, libro
331600970827     C* giornale, competenza Co.Ge. e competenza analitica
331700971219     C                   Eval      Data1     =   NewDtReg
331800970917     C                   Eval      PData119  =   %Addr(Data1)
331900970916     C                   Eval      PTpRegz119=   *Null
332000970827     C                   ExSr      CallMvc119
332100970825     C     ESITO         IFEQ      *On
332200970825     C* se non può essere modificata
332300970828     C                   Eval      ErroreMod =   *On
332400970828     C*
332500970825     C                   ENDIF
332600970828     C*
332700970916     C* imposto il bottone di Errore o i campi modificati se operazione
332800970828     C* andata a buon fine
332900970828     C                   ExSr      AggS1
333000970827     C*
333100970827     C                   endif
333200970828     C*
333300970828     C                   endif
333400970902     C*
333500970902     C*
333600970902     C* se opz=Passaggio a definitivo
333700970902     C     S1OPZ         WHENEQ    22
333800040406C1742 *
333900040406  "   * se la reg. non può essere passata in definitivo
334000040406C1742C                   if        H1Opz22 = *off
334100040406C1742X** ex D1535 ******
334200040406  "  X** ex D1535 ****** clear                   £BlkTes369
334300040406  "  X** ex D1535 ****** z-add     H1Sys         £Sys369
334400040406  "  X** ex D1535 ****** z-add     H1NrAsReg     £NrAsReg369
334500040406  "  X** ex D1535 ****** callb     'NDC369R'
334600040406  "  X** ex D1535 ****** parm                    Kpjba
334700040406  "  X** ex D1535 ******
334800040406  "  X**se registrazione non manutenzionabile
334900040406  "  X****************** if        H1Portaf   =  '1'
335000040406C1742X** ex D1535 ******           or £BlkTes369 = *on
335100040406C1742 *
335200970902     C* segnalo l'errore
335300970902     C                   Eval      ErroreMod =   *On
335400970902     C                   Eval      H1Errore  =   *On
335500040406C1742 *
335600040406C1742 * se la reg. può essere passata in definitivo
335700970902     C                   else
335800970902     C*
335900970903     C* metto a video la finestra per reperire la nuova data solo se è la
336000970903     C* prima volta nel ciclo di lettura del sfl che incontro l'opzione 22
336100970903     C* o che comunque non ho ancora confermato il passaggio a definitivo
336200970903     C                   if        OpenWdw6   =  *Off
336300970903     C                   Eval      Conferma6  =  *Off
336400970903     C                   move      *On           $Inzd6
336500970903     C                   MOVE      'D6'          $GEST                          fmt in gestione
336600970903     C                   Dow       $Gest      =  'D6'
336700970903     C                   EXSR      GesD6
336800970903     C* segnalo che in questo ciclo ho aperto la window
336900970903     C                   Eval      OpenWdw6   =  *On
337000970903     C                   EndDo
337100970903     C                   endif
337200970903     C*
337300970903     C* se è stato premuto l'F06 di conferma
337400970903     C                   if        Conferma6  =  *On
337500970903     C* richiamo il modulo per modificare le date di registrazione, libro
337600970903     C* giornale, competenza Co.Ge. e competenza analitica
337700970917     C                   if        NewDtReg6 =   *Loval
337800970917     C                   Eval      PData119  =   *Null
337900970917     C                   else
338000970917     C                   Eval      Data1     =   NewDtReg6
338100970917     C                   Eval      PData119  =   %Addr(Data1)
338200970917     C                   endif
338300970917     C                   Eval      TpRegz1   =   'D'
338400970917     C                   Eval      PTpRegz119=   %Addr(TpRegz1)
338500970903     C                   ExSr      CallMvc119
338600970903     C     ESITO         IFEQ      *On
338700970903     C* se non può essere modificata
338800970903     C                   Eval      ErroreMod =   *On
338900970903     C*
339000970903     C                   ENDIF
339100970903     C*
339200970903     C* imposto il bottone di Errore o i campi modifcati se operazione
339300970903     C* andata a buon fine
339400970903     C                   ExSr      AggS1
339500970903     C*
339600970903     C                   endif
339700970903     C*
339800970903     C                   endif
339900970825     C*
340000970828     C*
340100970825     C* se altre opzioni
340200970821     C* richiamo pgm gestione anagrafica
340300970821     C                   OTHER
340400040308     X*
340500040308     X*                  RESET                   Ndc014DS
340600040308     X*                  MOVE      xscsoc        Societa014
340700040308     X*                  MOVEL     S1OPZ         OPZ014
340800040308     X*    CMT013        IFEQ      '1'
340900040308     X*                  MOVEL     '1'           CMT014
341000040308     X*                  ELSE
341100040308     X*                  MOVEL     '0'           CMT014
341200040308     X*                  ENDIF
341300040308     X* passare nei campi appositi della DS Ndc014DS il valore
341400040308     X* della chiave della VL univoca
341500040308     X*                  MOVEL     H1NrSys       NrSys014
341600040308     X*                  MOVEL     H1NrAsReg     NrAsReg014
341700040308     X*
341800040308     X*                  MOVEL     Ndc014DS      KPJBU
341900040308     X*                  CALL      'NDC014R'
342000040308     X*                  PARM                    KPJBA
342100040308     X* se vengono gestite le autorizzazioni
342200040308     X* le passo al pgm di manutenzione per evitargli di doversele
342300040308     X* riprendere
342400040308     X*                  PARM                    AUTDS
342500040308     X*
342600040308     X*                  MOVEL     KPJBU         Ndc014DS
342700040308     X*
342800040308     X* ritorno da PGM gestione
342900040308     X*                  EXSR      RTN014
343000040308     X* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
343100040308     X*    *IN99         IFEQ      *ON
343200040308     X*                  SETON                                        40
343300040308     X*                  ENDIF
343400040308     X*
343500040308     X*                  MOVE      *HIVAL        $LASTV
343600040308     C                   ENDSL
343700040308     C*
343800040308     C*
343900970821     C                   ENDSR
344000970821     C/EJECT
344100000128C1315 ************************************************************
344200000128  "   * Window a seguito della richiesta manutenzione registraz.
344300000128  "   * priva di Nr assoluto reg.
344400000128  "   ************************************************************
344500000128  "  C     WinNrAsRe0    BEGSR
344600000128  "   *
344700000128  "  C                   CLEAR                   XMSGDS
344800000128  "  C                   CLEAR                   XMSGTXT
344900000128  "   *
345000000128  "   * Registrazione alla quale il msg si riferisce
345100000128  "  C                   RESET                   RifReg
345200000128  "  C                   Z-ADD     S1NrReg       NrReg
345300000128  "  C                   MOVEL     S1DtReg       GGMMAA
345400000128  "  C                   MOVEL     GG            GGReg
345500000128  "  C                   MOVEL     MM            MMReg
345600000128  "  C                   MOVEL     AA            AAReg
345700000128  "  C                   MOVEL     S1SerReg      SerReg
345800000128  "   *
345900000128  "   * Registrazione &1 &2 &3 non utilizzabile
346000000128  "  C                   Eval      IDMSG       = $MSGID(1)
346100000128  "  C                   CALLB     'XRTVM'
346200000128  "  C                   PARM                    IDMSG            27
346300000128  "  C                   PARM      *Blank        TXTMSG          100
346400000128  "  C                   PARM                    ERRMSG            1
346500000128  "  C                   PARM                    *omit
346600000128  "  C                   PARM                    *omit
346700000128  "  C                   PARM      RifReg        Msgdta          100
346800000128  "  C     ERRMSG        IFNE      *ON
346900000128  "  C                   MOVEL     TXTMSG        MSGMS1           70
347000000128  "  C                   ELSE
347100000128  "  C                   MOVEL     *ALL'?'       MSGMS1
347200000128  "  C                   ENDIF
347300000128  "   *
347400000128  "   * perchè priva di Nr assoluto di registrazione
347500000128  "  C                   Eval      IDMSG       = $MSGID(2)
347600000128  "  C                   CALLB     'XRTVM'
347700000128  "  C                   PARM                    IDMSG            27
347800000128  "  C                   PARM      *Blank        TXTMSG          100
347900000128  "  C                   PARM                    ERRMSG            1
348000000128  "  C     ERRMSG        IFNE      *ON
348100000128  "  C                   MOVEL     TXTMSG        MSGMS2           70
348200000128  "  C                   ELSE
348300000128  "  C                   MOVEL     *ALL'?'       MSGMS2
348400000128  "  C                   ENDIF
348500000128  "   *
348600000128  "   * contattare responsabile CED
348700000128  "  C                   Eval      IDMSG       = $MSGID(3)
348800000128  "  C                   CALLB     'XRTVM'
348900000128  "  C                   PARM                    IDMSG            27
349000000128  "  C                   PARM      *Blank        TXTMSG          100
349100000128  "  C                   PARM                    ERRMSG            1
349200000128  "  C     ERRMSG        IFNE      *ON
349300000128  "  C                   MOVEL     TXTMSG        MSGMS3           70
349400000128  "  C                   ELSE
349500000128  "  C                   MOVEL     *ALL'?'       MSGMS3
349600000128  "  C                   ENDIF
349700000128  "   *
349800000128  "  C                   MOVEL     'S'           MSGEMV
349900000128  "  C                   Z-ADD     10            MSGRGP
350000000128  "  C                   MOVEL     'N'           MSGRSP
350100000128  "   *
350200000128  "   *Modo di inizializzazione della window:
350300000128  "   *('1'->Close + Open  -->I° volta dopo l'emissione del video princip.)
350400000128  "   *('0'->rimane aperta -->tutte le emissioni successive se consecutive,
350500000128  "   *                       per mantenere il sottofondo del video princip.)
350600000128  "  C                   Eval      XMSGINZ     = $PrimoXMsg
350700000128  "  C                   If        $PrimoXMsg  = *On
350800000128  "  C                   Eval      $PrimoXMsg  = *Off
350900000128  "  C                   EndIf
351000000128  "   *
351100000128  "  C                   CALL      'XMSGR'
351200000128  "  C                   PARM                    XMSGDS
351300000128  "  C                   PARM                    XMSGTXT         135
351400000128  "  C                   PARM                    XMSGINZ           1
351500000128  "   *
351600000128  "   *
351700000128C1315C     XWinNrAsRe    ENDSR
351800970828     C***********************************************************
351900970828     C* PGM PER GESTIONE MESSAGGIO di conferma cancellazione
352000970828     C***********************************************************
352100970828     C     MSGPGM        BEGSR
352200970828     c*
352300970828     C                   Clear                   XMsgDs
352400970828     c* 1a riga
352500970828     C                   Eval      IDMSG = 'PROMSG    *LIBL     COS0280'
352600970828     C*
352700970828     C                   CALLB     'XRTVM'
352800970828     C                   PARM                    IDMSG            27
352900970828     C                   PARM                    TXTMSG          100
353000970828     C                   PARM                    ERRMSG            1
353100970828     C*
353200970828     C     ERRMSG        IFNE      *ON
353300970828     C                   MOVEL     TXTMSG        MsgMs1
353400970828     C                   ELSE
353500970828     C                   MOVEL     *ALL'?'       MsgMs1
353600970828     C                   ENDIF
353700970828     C*
353800970828     C                   Z-Add     0             MsgMsg
353900970828     C                   Movel     'S'           MsgRsp
354000970828     C                   Movel     'SN'          MsgTpR
354100970828     C                   Movel     'S'           MsgEmV
354200970828     C                   Z-Add     10            MsgRgp
354300970828     C                   Movel     'N'           MsgLck
354400970828     C                   Movea     Status        Sts
354500970828     C                   Movel     'N'           MsgCnl
354600970828     C                   Movel     'N'           MsgVid
354700970828     C                   Movel     'N'           MsgOpe
354800970828     C                   Movel     'N'           MsgStp
354900970828     C                   Call      'XMSGR'
355000970828     C                   Parm                    XMsgDs
355100970828     C*
355200970828     C                   ENDSR
355300970916      /eject
355400970825     C************************************************************
355500970825     C* Chiamata al modulo di cancellazione registrazione
355600970825     C************************************************************
355700970825     C     CallMvc116    BEGSR
355800970825     C*
355900030226C1627 * valorizzo la variabile globale £kpjba utile a ndmvc116
356000030226  "   * per usare knsif nella gestioen Laguna
356100030226  "  C                   if        XScLaguna = *on  and
356200030226  "  C                             D1Dociva  = '1'
356300030226  "  C                   movel     Kpjba         £Kpjba
356400030226  "  C                   endif
356500030226C1627 *
356600970825     C* Chiamo il modulo per cancellare dichiarando di dare il Commit
356700970825     C                   CALLB     'NDMVC116'
356800970825     C                   PARM                    H1Sys
356900970825     C                   PARM                    H1NrAsReg
357000970825     C                   PARM      '02'          Operazione
357100970825     C                   PARM      '0'           GesMsg
357200970825     C                   PARM      *Off          Esito
357300970825     C                   PARM      *On           Comit
357400030226C1627 *
357500030226  "   * pulisco la variabile globale £kpjba, per non sporcare
357600030226  "   * eventuali altre chiamate a ndmvc116
357700030226C1627C                   clear                   £Kpjba
357800970825     C*
357900970825     C                   Endsr
358000970825     C/EJECT
358100970825     C************************************************************
358200970825     C* Chiamata al modulo di modifica data registrazione
358300970825     C************************************************************
358400970825     C     CallMvc119    BEGSR
358500970825     C*
358600970825     C                   Callb     'NDMVC119'
358700970825     C                   Parm                    H1Sys
358800970825     C                   Parm                    H1NrAsReg
358900971216     C                   Parm      '02'          Operazione
359000971002     C                   Parm      *On           GesMsg
359100971002     C                   Parm      *Off          Esito
359200970916     C                   Parm                    TpRegz119
359300970825     C                   Parm                    *Omit
359400970825     C                   Parm                    *Omit
359500970825     C                   Parm                    *Omit
359600971002     C                   Parm                    *Omit
359700971002     C                   Parm      *On           Comit
359800971002     C                   Parm                    Data119
359900971002     C                   Parm                    *Omit
360000971002     C                   Parm                    *Omit
360100971002     C                   Parm                    Data119
360200971002     C                   Parm                    Data119
360300971002     C                   Parm                    Data119
360400970825     C*
360500970825     C                   ENDSR
360600970825     C/EJECT
360700970825     C************************************************************
360800970825     C* GESTIONE RITORNO DA PGM primanota generica
360900970825     C************************************************************
361000970825     C     RTN001        BEGSR
361100970825     C*
361200970825     C* operazione eseguite dal pgm chiamato
361300040308     X**********++       MOVE      OPR001        WOPR
361400970901     C*
361500970825     C                   SELECT
361600970902     C* se = 1 eseguito aggiornamento bisogna aggiornare la riga del sfl
361700040308     C     OPR001        WHENEQ    '1'
361800970902     C**********         Move      *On           WInzS1
361900970901     C* imposto i campi modificati sul slf
362000970901     C                   ExSr      AggS1
362100970825     C                   ENDSL
362200970825     C*
362300970902     C*
362400970902     C                   Select
362500970902     C* se si è usciti con F12 o F03 bisogna fermare il ciclo, altrimenti
362600970902     C* bisogna proseguire con le altre opzioni
362700040308     C     Ret001        WHENne    '0'
362800970902     C                   Move      *On           $Esci
362900970902     C                   endsl
363000970825     C*
363100970825     C*
363200970825     C                   ENDSR
363300970825     C/EJECT
363400040308     X************************************************************
363500040308     X* GESTIONE RITORNO DA PGM DI GESTIONE
363600040308     X************************************************************
363700040308     X*    RTN002        BEGSR
363800040308     X*
363900040308     X* modo di ritorno
364000040308     X*
364100040308     X*                  SELECT
364200040308     X* questi modi di utilizzo dei valori di ritorno dal
364300040308     X* pgm di manutenzione rcd di anagrafica sono delle
364400040308     X* proposte, normalmente sempre valide, ma modificabili
364500040308     X* per situazioni particolari
364600040308     X* 1 = F3
364700040308     X*    RET002        WHENEQ    '1'
364800040308     X*                  MOVE      *ON           $ESCI                          sospese READC
364900040308     X*                  MOVE      *ON           $FINE                          fine pgm
365000040308     X*                  MOVE      '1'           RET013
365100040308     X* 2 = F12
365200040308     X*    RET002        WHENEQ    '2'
365300040308     X* se il pgm di manutenzione ha prima una guida, è necessario anche
365400040308     X* questo test, altrimenti prevale il ret002 e il sfl non viene
365500040308     X* ricaricato
365600040308     X*    OPR002        ANDNE     '1'
365700040308     X*                  MOVE      *ON           $ESCI                          sospese READC
365800040308     X*
365900040308     X*                  ENDSL
366000040308     X*
366100040308     X* operazione eseguite dal pgm chiamato
366200040308     X*
366300040308     X*                  MOVE      OPR002        WOPR                           avverto pgm chiamant
366400040308     X*                  SELECT
366500040308     X* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
366600040308     X*    OPR002        WHENEQ    '1'
366700040308     X*
366800040308     X* se l'opzione con cui è stato chiamato non è la modifica
366900040308     X* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
367000040308     X*    OPZ002        IFNE      '02'
367100040308     X*                  MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
367200040308     X* se l'opzione con cui è stato chiamato è la modifica
367300040308     X* aggiorno subito la riga di sfl e non richiedo il ripristino sfl
367400040308     X*                  ELSE
367500040308     X*                  EXSR      AGGS1
367600040308     X*                  ENDIF
367700040308     X*
367800040308     X*                  ENDSL
367900040308     X*
368000040308     X* funzione non eseguibile per errore :
368100040308     X*
368200040308     X*                  SELECT
368300040308     X* 1 = funzione richiamata chiusa in errore
368400040308     X*    ERR002        WHENEQ    '1'
368500040308     X*                  MOVE      *ON           $ESCI                          sospese READC
368600040308     X*                  SETON                                        9399      emiss.SFLMSGID
368700040308     X* 2 = funzione richiamata senza la dovuta autorizzazione
368800040308     X*    ERR002        WHENEQ    '2'
368900040308     X*                  MOVE      *ON           $ESCI                          sospese READC
369000040308     X*                  SETON                                        9499      emiss.SFLMSGID
369100040308     X*  eventualmente gestire altri codici di errore
369200040308     X*
369300040308     X*                  ENDSL
369400040308     X*
369500040308     X*                  ENDSR
369600970821     C/EJECT
369700970821     C************************************************************
369800970821     C* AGGIORNAMENTO RIGA LISTA
369900970821     C************************************************************
370000970821     C     AGGS1         BEGSR
370100970821     C*
370200970821     C* riempio i campi del sfl che sono stati ripassati dal pgm
370300970821     C* di manutenzione dopo una opzione 02
370400970821     C*
370500970901     C* se è stata richiesta l'opzione di modifica ed è stato fatto un
370600970901     C* aggiornamneto al record, aggiorno il sfl
370700040308     c                   if        S1Opz = 2  and
370800970902     C                             Opr001=   *On
370900970916     C                   Eval      RegSys    =   H1Sys
371000970916     C                   Eval      RegNrAsReg=   H1NrAsReg
371100971216     C     K02Reg01      Chain     NdReg01L                           21
371200040308     C                   if        *In21    = *Off
371300980220     C* se in parzializzazione è stato chiesto di vedere SOLO registrazioni
371400980220     C* Provvisorie, ed è stato effettuato un passaggio a DEFINITIVO della
371500980220     C* registrazione pulisco tutti i campi del sfl
371600040308     C                   if        C1TpRegz  <>  *Blank    and
371700980220     C                             RegTpRegz <>  C1TpRegz
371800980220     C                   Clear                   S1TpRegz
371900980220     C                   Clear                   S1DtReg
372000980220     C                   Clear                   S1NrReg
372100980220     C                   Clear                   S1SerReg
372200980220     C                   Clear                   S1CausTes
372300980220     C                   Clear                   S1Classe
372400980220     C                   Clear                   S1Divisa
372500980220     C                   Clear                   S1Importo
372600980220     C                   Clear                   S1DtUlMo
372700980220     C                   Clear                   S1UteUlMo
372800980220     C                   Clear                   S1DtDoc
372900980220     C                   Clear                   S1NrDoc
373000980220     C                   Clear                   S1SerDoc
373100980220     C                   Clear                   S1KNrAz
373200980220     C                   Clear                   S1Unita
373300980220     C*
373400980220     C* se in parzializzazione NON è stato chiesto di vedere solo le
373500980220     C* registrazione provvisorie oppure NON è stata passata a definitivo
373600980220     C* reimposto i campi sul sfl
373700040308     C                   else
373800980220     C                   Eval      S1TpRegz  =   RegTpRegz
373900040308     C     RegDtReg      ifne      *Loval
374000040308C1309X******DMy          Move      RegDtReg      S1DtReg
374100000117C1309C     *JobRun       Move      RegDtReg      S1DtReg
374200040308     C                   endif
374300980220     C                   Eval      S1NrReg   =   RegNrReg
374400980220     C                   Eval      S1SerReg  =   RegSerReg
374500980220     C                   Eval      S1CausTes =   RegCausTes
374600980220     C                   Eval      S1Classe  =   RegClasse
374700980220     C                   Eval      S1Divisa  =   RegDivisa
374800980220     C                   Eval      S1Importo =   RegImporto
374900040308     C     RegDtUlMo     ifne      *Loval
375000040308C1309X******Dmy          Move      RegDtUlMo     S1DtUlMo
375100000117C1309C     *JobRun       Move      RegDtUlMo     S1DtUlMo
375200040308     C                   endif
375300980220     C                   Eval      S1UteUlMo =   RegUteUlMo
375400040308     C     RegDtDoc      ifne      *Loval
375500040308C1309X******Dmy          Move      RegDtDoc      S1DtDoc
375600000117C1309C     *JobRun       Move      RegDtDoc      S1DtDoc
375700040427     C                   endif
375800980220     C                   Eval      S1NrDoc   =   RegNrDoc
375900980220     C                   Eval      S1SerDoc  =   RegSerDoc
376000980220     C                   Eval      S1KNrAz   =   RegKNrAz
376100980220     C                   Eval      S1Unita   =   RegUnita
376200980220     C*
376300980220     C* fine tipo registrazione
376400040308     C                   endif
376500980220     C* fine trovato record
376600040308     c                   endif
376700980220     C* fine opzione 2
376800040308     c                   endif
376900040308     C*
377000970901     C* se è stata richiesta l'opzione di modifica data registrazione
377100970901     C* ed è andata a buon fine, aggiorno il sfl
377200970901     c                   if        S1Opz = 8  and
377300970901     C                             Esito =   *Off
377400970902     C* salvo la data vecchia della registrazione prima di modificarla con
377500980219     C* la nuova
377600040308C1309X******Dmy          Move      S1DtReg       OldDtReg
377700000117C1309C     *JobRun       Move      S1DtReg       OldDtReg
377800970901     C                   Eval      S1DtReg   =   D5NewDtReg
377900040308C1309X******Dmy          Move      UpdDate       S1DtUlMo
378000000117C1309C     *JobRun       Move      UpdDate       S1DtUlMo
378100970901     C                   Eval      S1UteUlMo =   XScCUt
378200970901     c                   endif
378300040308     C*
378400980219     C* se è stata richiesta l'opzione di cancellazione registrazione
378500970828     C* ed è andata a buon fine, aggiorno il sfl pulendo i campi della riga
378600970828     C* del sfl
378700970917     c                   if        S1Opz = 4                     and
378800970917     C                             Esito =   *Off                and
378900970917     C                             (MsgRtn = 'S' or  MsgRtn= 's')
379000980219     C                   Clear                   S1TpRegz
379100980219     C                   Clear                   S1DtReg
379200970828     C                   Clear                   S1NrReg
379300970901     C                   Clear                   S1SerReg
379400970828     C                   Clear                   S1CausTes
379500970828     C                   Clear                   S1Classe
379600970828     C                   Clear                   S1Divisa
379700970828     C                   Clear                   S1Importo
379800970828     C                   Clear                   S1DtUlMo
379900970828     C                   Clear                   S1UteUlMo
380000970828     C                   Clear                   S1DtDoc
380100970828     C                   Clear                   S1NrDoc
380200970828     C                   Clear                   S1SerDoc
380300970829     C                   Clear                   S1KNrAz
380400970829     C                   Clear                   S1Unita
380500970828     C* rendo protetto e non visibile il campo S1Opz
380600970828     C                   SetOn                                        41
380700970828     c                   endif
380800040308     C*
380900970917     C* se è stata richiesta l'opzione di passaggio a definitivo
381000970917     C* ed è andata a buon fine, aggiorno il sfl pulendo i campi della riga
381100970917     C* del sfl
381200970917     c                   if        S1Opz = 22        and
381300970917     C                             Esito =   *Off
381400971216     C* salvo il vecchio tipo di registrazione e la vecchia data di
381500971216     C* registrazione prima di modificarli
381600980219     C                   Move      S1TpRegz      OldTpReg
381700040308C1309X******Dmy          Move      S1DtReg       OldDtReg
381800000117C1309C     *JobRun       Move      S1DtReg       OldDtReg
381900980219     C                   if        C1TpRegz  <>  'P'
382000980219     C                   Eval      S1TpRegz  =   'D'
382100980219     C                   else
382200980220     C                   Clear                   S1TpRegz
382300980220     C                   Clear                   S1DtReg
382400980219     C                   Clear                   S1NrReg
382500980219     C                   Clear                   S1SerReg
382600980219     C                   Clear                   S1CausTes
382700980219     C                   Clear                   S1Classe
382800980219     C                   Clear                   S1Divisa
382900980219     C                   Clear                   S1Importo
383000980219     C                   Clear                   S1DtUlMo
383100980219     C                   Clear                   S1UteUlMo
383200980219     C                   Clear                   S1DtDoc
383300980219     C                   Clear                   S1NrDoc
383400980219     C                   Clear                   S1SerDoc
383500980219     C                   Clear                   S1KNrAz
383600980219     C                   Clear                   S1Unita
383700980219     C                   endif
383800970917     c                   endif
383900970828     C*
384000970916     C* se è stata richiesta l'opzione Cancella, Modifica data registra
384100970916     C* zione o Passaggio a definitivo e non possibile effettuare la
384200970916     C* modifica
384300970916     c                   if        (S1Opz=4 or S1Opz=8 or S1Opz=22) and
384400970825     C                             Esito     =   *On
384500970825     C                   Movel     *On           H1Errore
384600970825     c                   endif
384700970828     C*
384800970825     C* se è stata richiesta l'opzione Modifica
384900970825     C* e non possibile effettuare la modifica
385000970825     c                   if        S1Opz = 2  and
385100970825     C                             Err001    =   '3'
385200970825     C                   Movel     *On           H1Errore
385300970825     c                   endif
385400970901     C*
385500980219     C* aggiorno i file di stampa con la segnalazione delle modifiche
385600980219     C* effettuate
385700980219     C                   ExSr      AggStampa
385800970901     C*
385900970821     C*
386000970821     C                   ENDSR
386100970821     C/EJECT
386200970821     C************************************************************
386300980219     C* Aggiornamento printer file
386400970821     C************************************************************
386500980219     C     AggStampa     BEGSR
386600970821     C*
386700980219     C* Aggiornamento dei file di stampa per le registrazioni modificate e
386800980219     C* per quelle cancellate
386900980219     C* M.B. i campi validi per entrambe le stampe si chiamano nello stesso
387000980219     C* modo  e per tanto verranno valorizzati nello stesso punto
387100040308     c                   if        (S1Opz = 4  and
387200040308     C                              Esito =   *Off  and
387300040308     C                              MsgRtn = 'S'        )  or
387400040308     C                             (S1Opz = 8       and
387500040308     C                              Esito =   *Off      )  or
387600040308     C                             (S1Opz = 22      and
387700040308     C                              Esito =   *Off      )
387800040308     C                   Eval      RegSys     =   H1Sys
387900040308     C                   Eval      RegNrAsReg =   H1NrAsReg
388000040308     C     K02Reg01      Chain     NdReg01L                           21
388100040308     C                   if        *In21    = *Off
388200040308     C                   Clear                   Riga1
388300040308     C                   Clear                   Riga2
388400040308     C* imposto i campi sul prtf di riepilogo modifiche
388500040308     C     RegDtReg      ifne      *Loval
388600040308C1309X******Dmy          Move      RegDtReg      RdDtReg
388700000117C1309C     *JobRun       Move      RegDtReg      RdDtReg
388800040308     C                   endif
388900980220     C                   Move      RegTpRegz     RdTpRegz
389000980219     C                   Eval      RdNrReg   =   RegNrReg
389100980219     C                   Eval      RdSerReg  =   RegSerReg
389200980219     C                   Eval      RdCausTes =   RegCausTes
389300980219     C                   Eval      RdClasse  =   RegClasse
389400980219     C                   Eval      RdDivisa  =   RegDivisa
389500980219     C                   Eval      RdImporto =   RegImporto
389600040308     C     RegDtUlMo     ifne      *Loval
389700040308C1309X******Dmy          Move      RegDtUlMo     RdDtUlMo
389800000117C1309C     *JobRun       Move      RegDtUlMo     RdDtUlMo
389900040308     C                   endif
390000980219     C                   Eval      RdUteUlMo =   RegUteUlMo
390100040308     C     RegDtDoc      ifne      *Loval
390200040308C1309X******Dmy          Move      RegDtDoc      RdDtDoc
390300000117C1309C     *JobRun       Move      RegDtDoc      RdDtDoc
390400040308     C                   endif
390500980219     C                   Eval      RdNrDoc   =   RegNrDoc
390600980219     C                   Eval      RdSerDoc  =   RegSerDoc
390700980219     C                   Eval      RdKNrAz   =   RegKNrAz
390800980219     C                   Eval      RdUnita   =   RegUnita
390900980219     C* PRTF DI MODIFICA
391000040308     C                   if        S1Opz     =   8  or  S1Opz  =  22
391100040308     C* Stampo la testata se è la prima volta che stampo una riga
391200040308     C                   if        UnaRiga2  = *Off
391300040308     C                   Write     Testata2
391400040308     C                   Eval      UnaRiga2  = *On
391500040308     C                   endif
391600040308     C* verifico se devo fare il salto pagina
391700040308     C   38              Exsr      Over2
391800040308     C* valorizzo la vecchia data (se non è stata modificata stampo quella
391900980219     C* del file)
392000040308     C     OldDtReg      ifne      *Loval
392100040308C1309X******Dmy          Move      OldDtReg      RdOldDtReg
392200000117C1309C     *JobRun       Move      OldDtReg      RdOldDtReg
392300980219     C                   else
392400040308C1309X******Dmy          Move      RegDtReg      RdOldDtReg
392500000117C1309C     *JobRun       Move      RegDtReg      RdOldDtReg
392600040308     C                   endif
392700040308     C* valorizzo il vecchio tipo (se non è stato modificato stampo quello
392800040308     C* del file)
392900040308     C     OldTpReg      ifne      *Blank
393000040308     C                   Move      OldTpReg      RdOldTpReg
393100040308     C                   else
393200040308     C                   Move      RegTpRegz     RdOldTpReg
393300040308     C                   endif
393400040308     C                   Write     Riga2
393500040308     C*
393600040308     C* PRTF DI CANCELLAZIONE
393700040308     C                   else
393800040308     C* Stampo la testata se è la prima volta che stampo una riga
393900040308     C                   if        UnaRiga1  = *Off
394000040308     C                   Write     Testata1
394100980219     C                   Eval      UnaRiga1  = *On
394200040308     C                   endif
394300040308     X********+++        Write     Testata1
394400040308     C* verifico se devo fare il salto pagina
394500040308     C   39              Exsr      Over1
394600040308     C                   Write     Riga1
394700040308     C*
394800040308     C* fine tipo stampa
394900040308     C                   Endif
395000040308     C* fine trovato record su NdReg01L
395100040308     C                   endif
395200040308     C* fine valorizzazione file di stampa
395300040308     C                   endif
395400040308     C*
395500040308     C                   ENDSR
395600040308     C/EJECT
395700040308     C************************************************************
395800040308     C* GESTIONE VALORI ASSUNTI
395900980219     C************************************************************
396000980219     C     GESD2         BEGSR
396100980219     C*
396200040308     C     $INZD2        IFEQ      *ON
396300040308     C                   EXSR      INZD2
396400040308     C                   MOVE      *OFF          $INZD2
396500040308     C                   ENDIF
396600040308     C*
396700040308     C     $LASTV        IFNE      'D2'
396800040308     C                   EXSR      GESPIE
396900040308     C     $LASTV        IFNE      *HIVAL
397000040308     C                   MOVE      $LASTV        $LASTG
397100040308     C                   ENDIF
397200040308     C                   MOVE      'D2'          $LASTV
397300040308     C                   ENDIF
397400040308     C*
397500040308     C* se esistono errori sulla videata
397600040308     C* emetto la write del formato a indicatori spenti per vedere
397700970821     C* le eventuali decodifiche
397800040308     C     *IN99         IFEQ      *ON
397900040308     C                   MOVEA     *IN           WIN
398000040308     C                   MOVE      *ALL'0'       IN5098           49
398100040308     C                   MOVEA     IN5098        *IN(50)
398200040308     C                   WRITE     FMTD2
398300040308     C                   MOVEA     WIN           *IN
398400040308     C                   ENDIF
398500040308     C*
398600040308     C                   EXFMT     FMTD2
398700040308     C*
398800040308     C* azzero la posizione del cursore
398900040308     C                   Z-ADD     0             H1RIGA
399000040308     C                   Z-ADD     0             H1COLO
399100040308     C*
399200040308     C* CONTROLLO ABILITAZIONE TASTI
399300040308     C* INPUT
399400040308     C* $FC3 = Abilitazioni videata D2
399500040308     C* OUTPUT
399600040308     C* *IN97 + *IN99 Errori
399700040308     C                   MOVEA     $FC3          $FC
399800040308     C                   EXSR      CTRKEY
399900040308     C*
400000040308     C     *IN99         IFEQ      *OFF
400100040308     C*
400200040308     C                   SELECT
400300040308     C* F3=Fine
400400040308     C     $TASTO        WHENEQ    F03
400500040308     C                   EXSR      F03D2
400600040308     C* F12=Ritorno
400700040308     C     $TASTO        WHENEQ    F12
400800040308     C                   EXSR      F12D2
400900040308     C* F24=Altri tasti
401000040308     C     $TASTO        WHENEQ    F24
401100040308     C                   EXSR      F24D2
401200040308     C*
401300040308     C                   OTHER
401400040308     C                   EXSR      CTRD2
401500040308     C     *IN99         IFEQ      *OFF
401600040308     C                   MOVE      $LASTG        $GEST
401700040308     C                   SELECT
401800040308     C     $LASTG        WHENEQ    'S1'
401900040308     C* se è stato modificato l'ordinamento di default, ricarico la lista
402000040308     C     $ORD          IFNE      D2ORD
402100040308     C                   MOVE      *ON           $INZS1
402200040308     C                   MOVE      *BLANK        W1POS
402300040308     C                   MOVE      *Zeros        W1PosDt
402400040308     C                   ENDIF
402500040308     C     $LASTG        WHENEQ    'D1'
402600040308     C* per ora niente
402700040308     C                   ENDSL
402800040308     C* definisco i nuovi valori di default
402900040308     C                   Z-ADD     D2LOP         $LOP
403000040308     C                   Z-ADD     D2ORD         $ORD
403100040308     C******             MOVE      D2DFn         $DFn
403200040308      ** elenco altri dati default specifici dell'anagrafica
403300040308     C*
403400040308     C* aggiorno valori assunti
403500040308     C* se richiesto il salvataggio
403600040308     C     D2BSV         IFEQ      1
403700040308     C                   EXSR      AGGMPE
403800040308     C                   ENDIF
403900040308     C*
404000040308     C                   ENDIF
404100040308     C*
404200040308     C                   ENDSL
404300040308     C*
404400040308     C                   ENDIF
404500040308     C*
404600970821     C                   ENDSR
404700970821     C/EJECT
404800970821     C************************************************************
404900970821     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
405000970821     C************************************************************
405100970821     C     INZD2         BEGSR
405200970821     C*
405300970821     C                   CLEAR                   FMTD2
405400021014C1595C                   EVAL      D2BSVA = Normale
405500970821     C* se sono entrato con chiave univoca
405600970821     C* impedisco il salvataggio valori assunti
405700970821     C     $Unique       IFEQ      *ON
405800021014C1595X***                Z-ADD     2             D2BSV
405900021014C1595C                   EVAL      D2BSVA = Protetto
406000970821     C                   ENDIF
406100970821     C                   MOVEL     $LOP          D2LOP
406200970821     C                   MOVE      $ORD          D2ORD
406300970825    >C****               MOVE      $DFn          D2DFn
406400970825    > *  elenco altri dati default specifici dell'anagrafica
406500970821     C*
406600970821     C                   ENDSR
406700970821     C/EJECT
406800970821     C************************************************************
406900970821     C* GESTIONE F03 VIDEO D2
407000970821     C************************************************************
407100970821     C     F03D2         BEGSR
407200970821     C*
407300970821     C                   MOVE      *ON           $FINE
407400970821     C* fine programma
407500970821     C                   EXSR      ENDPGM
407600970821     C*
407700970821     C                   ENDSR
407800970821     C/EJECT
407900970821     C************************************************************
408000970821     C* GESTIONE F12 VIDEO D2
408100970821     C************************************************************
408200970821     C     F12D2         BEGSR
408300970821     C*
408400970821     C                   MOVE      $LASTG        $GEST
408500970821     C*
408600970821     C                   ENDSR
408700970821     C/EJECT
408800970821     C************************************************************
408900970821     C* GESTIONE F24 VIDEO D2
409000970821     C************************************************************
409100970821     C     F24D2         BEGSR
409200970821     C*
409300970821     C                   MOVEA     $FX3          $FX
409400970821     C                   MOVEA     $FC3          $FC
409500970821     C                   MOVEA     $FR3          $FR
409600970821     C                   Z-ADD     $ULKD2        $ULK
409700970821     C                   Z-ADD     $D2L01        $L01
409800970821     C                   Z-ADD     $D2L02        $L02
409900970821     C                   EXSR      RIEKEY
410000970821     C                   Z-ADD     $ULK          $ULKD2            3 0
410100970821     C                   MOVEA     $FC           $FC3
410200970821     C                   MOVEL     $KEY1         Z3KE1
410300060224C1913C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
410400970821     C                   WRITE     FMTZ3
410500970821     C*
410600970821     C                   ENDSR
410700970821     C/EJECT
410800970821     C************************************************************
410900970821     C* CONTROLLO VIDEATA VALORI ASSUNTI
411000970821     C************************************************************
411100970821     C     CTRD2         BEGSR
411200970821     C*
411300970821     C                   SETOFF                                       99
411400970821     C*
411500970821     C* OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
411600970821     C     D2LOP         IFEQ      0
411700970821     C                   SETON                                          5099
411800970821     C                   ENDIF
411900970821     C*
412000970821     C* OBBLIGATORIO VALORE PER ORDINAMENTO
412100970821     C     D2ORD         IFEQ      0
412200970821     C                   SETON                                          5199
412300970821     C                   ENDIF
412400970821     C*
412500040308     X* CONTROLLO ESISTENZA CODICE PER I DEFAULT
412600040308     X***  D2DF1         IFNE      *BLANK
412700040308     X***  D2DF1         SETLL     AN___01                                21
412800040308     X***  *IN21         IFEQ      *OFF
412900040308     X***                SETON                                        5699
413000040308     X***                ENDIF
413100040308     X** ATTENZIONE : se il file del codice per i default può essere
413200040308     X*   lo stesso da cui viene caricato il sfl e, contemporaneamente,
413300040308     X*   è la stessa vista logica usata nell'attuale ordinamento,
413400040308     X*   bisogna riposizionarsi se no, al primo roll-up sbaglia il
413500040308     X*   caricamento
413600040308     X* se ordinamento vigente è lo stesso della v.l. in uso per
413700040308     X* il ctrl del cd riferimento per i default
413800040308     X*    $ORD          IFEQ      2
413900040308     X* e c'è almeno un rcd per il prossimo caricamento
414000040308     X*    $EndFile      ANDEQ     *OFF
414100040308     X* mi riposiziono
414200040308     X*    K05Reg13      SETLL     NdReg13L                               21
414300040308     X*                  ENDIF
414400040308     X*                  ENDIF
414500970825    > *  elenco controlli altri dati default specifici dell'anagrafica
414600970821     C*
414700970821     C                   ENDSR
414800970821     C/EJECT
414900970827     C************************************************************
415000970827     C* GESTIONE VIDEO RECORD D5
415100970827     C************************************************************
415200970827     C     GESD5         BEGSR
415300970827     C*
415400040308     C     $INZD5        IFEQ      *ON
415500040308     C                   EXSR      INZD5
415600040308     C                   MOVE      *OFF          $INZD5
415700040308     C                   ENDIF
415800040308     C*
415900040308     C     $LASTV        IFNE      'D5'
416000040308     C                   EXSR      GESPIE
416100040308     C                   MOVE      $LASTV        $LASTG
416200040308     C                   MOVE      'D5'          $LASTV
416300040308     C                   ENDIF
416400040308     C*
416500040308     C* se esistono errori sulla videata
416600040308     C* emetto la write del formato a indicatori spenti per vedere
416700040308     C* le eventuali decodifiche
416800040308     C     *IN99         IFEQ      *ON
416900040308     C     $GEST         ANDEQ     'D5'
417000040308     C                   MOVEA     *IN           WIN
417100040308     C                   MOVE      *ALL'0'       IN5098           49
417200040308     C                   MOVEA     IN5098        *IN(50)
417300040308     C                   WRITE     FMTD5
417400040308     C                   MOVEA     WIN           *IN
417500040308     C                   ENDIF
417600040308     C*
417700040308     C                   EXFMT     FMTD5
417800040308     C*
417900040308     C* azzero la posizione del cursore
418000040308     C                   Z-ADD     0             H1RIGA
418100040308     C                   Z-ADD     0             H1COLO
418200040308     C*
418300040308     C* controllo abilitazione tasti funzione
418400040308     C                   EXSR      CTRKD5
418500040308     C*
418600040308     C     *IN99         IFEQ      *OFF
418700040308     C*
418800040308     C                   SELECT
418900040308     C* F5=Refresh
419000040308     C     $TASTO        WHENEQ    F05
419100040308     C                   EXSR      F05D5
419200040308     C* F12=Ritorno
419300040308     C     $TASTO        WHENEQ    F12
419400040308     C                   EXSR      F12D5
419500040308     C* F24=Altri tasti
419600040308     C     $TASTO        WHENEQ    F24
419700040308     C                   EXSR      F24D5
419800040308     C*
419900040308     C                   OTHER
420000040308     C                   EXSR      CTRD5
420100040308     C*
420200040308     C     *IN99         IFEQ      *OFF
420300040308     C     $TASTO        ANDEQ     F06
420400040308     C* confermo la modifica della data di registrazione con quella digitata
420500040308     C                   Eval      NewDtReg   =   WNewDtReg
420600040308     C                   Eval      $Gest      =   'S1'
420700040308     C                   Eval      Conferma   =   *On
420800040308     C*
420900040308     C                   ENDIF
421000040308     C*
421100040308     C                   ENDSL
421200040308     C*
421300040308     C                   ENDIF
421400040308     C*
421500040308     C                   ENDSR
421600040308     C/EJECT
421700040308     C************************************************************
421800040308     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D5
421900040308     C************************************************************
422000040308     C     CTRKD5        BEGSR
422100040308     C*
422200970827     C                   SETOFF                                       99
422300970827     C                   MOVEA     $FC5          $FC
422400970827     C                   EXSR      CTRKEY
422500970827     C     *IN99         IFEQ      *ON
422600970827     C                   SETON                                        97
422700970827     C                   ENDIF
422800970827     C*
422900970827     C                   ENDSR
423000970827     C/EJECT
423100970827     C************************************************************
423200970827     C* INIZIALIZZAZIONE VIDEATA D5
423300970827     C************************************************************
423400970827     C     INZD5         BEGSR
423500970827     C*
423600970827     C                   CLEAR                   FMTD5
423700970827     C* emetto l'ultima data impostata e confermata
423800980220     C     NewDtReg      ifne      *Loval
423900040308C1309X******Dmy          Move      NewDtReg      D5NewDtReg
424000000117C1309C     *JobRun       Move      NewDtReg      D5NewDtReg
424100970827     C                   endif
424200970827     C*
424300970827     C                   ENDSR
424400970827     C/EJECT
424500970827     C************************************************************
424600970827     C* GESTIONE F05 VIDEO D5
424700970827     C************************************************************
424800970827     C     F05D5         BEGSR
424900970827     C*
425000970827     C                   MOVE      *ON           $InzD5
425100970827     C*
425200970827     C                   ENDSR
425300970827     C/EJECT
425400970827     C************************************************************
425500970827     C* GESTIONE F12 VIDEO D5
425600970827     C************************************************************
425700970827     C     F12D5         BEGSR
425800970827     C*
425900970827     C                   MOVE      'S1'          $Gest
426000970827     C*
426100970827     C                   ENDSR
426200970827     C/EJECT
426300970827     C************************************************************
426400970827     C* GESTIONE F24 VIDEO D5
426500970827     C************************************************************
426600970827     C     F24D5         BEGSR
426700970827     C*
426800970827     C                   MOVEA     $FX5          $FX
426900970827     C                   MOVEA     $FC5          $FC
427000970827     C                   MOVEA     $FR5          $FR
427100970827     C                   Z-ADD     $ULKD5        $ULK
427200970827     C                   Z-ADD     $D5L01        $L01
427300970827     C                   Z-ADD     $D5L02        $L02
427400970827     C                   EXSR      RIEKEY
427500970827     C                   Z-ADD     $ULK          $ULKD5            3 0
427600970827     C                   MOVEA     $FC           $FC5
427700970827     C                   MOVEL     $KEY1         Z5KE1
427800060224C1913C                   MOVEL     $ALLFUNCT     H5ALLFUNCT
427900970827     C                   WRITE     FMTZ5
428000970827     C*
428100970827     C                   ENDSR
428200970827     C/EJECT
428300970827     C************************************************************
428400970827     C* CONTROLLO VIDEATA D5
428500970827     C************************************************************
428600970827     C     CTRD5         BEGSR
428700970827     C*
428800970827     C                   SETOFF                                       99
428900970827     C*
429000970827     C* data di registrazione obbligatoria
429100980220     C     D5NewDtReg    ifeq      0
429200970827     C                   SetOn                                        5099
429300970827     C                   else
429400970827     C* controllo validità
429500040308C1309X******Dmy          Test(D)                 D5NewDtReg             21
429600000117C1309C     *JobRun       Test(D)                 D5NewDtReg             21
429700970827     C                   If        *In21    =    *Off
429800040308C1309X******Dmy          Move      D5NewDtReg    WNewDtReg
429900000117C1309C     *JobRun       Move      D5NewDtReg    WNewDtReg
430000970827     C                   else
430100970827     C* data errata
430200970827     C                   SetOn                                        5199
430300970827     C                   endif
430400970827     C                   endif
430500970827     C*
430600970827     C                   ENDSR
430700970827     C/EJECT
430800970903     C************************************************************
430900970903     C* GESTIONE VIDEO RECORD D6
431000970903     C************************************************************
431100970903     C     GESD6         BEGSR
431200970903     C*
431300040308     C     $INZD6        IFEQ      *ON
431400040308     C                   EXSR      INZD6
431500040308     C                   MOVE      *OFF          $INZD6
431600040308     C                   ENDIF
431700040308     C*
431800040308     C     $LASTV        IFNE      'D6'
431900040308     C                   EXSR      GESPIE
432000040308     C                   MOVE      $LASTV        $LASTG
432100040308     C                   MOVE      'D6'          $LASTV
432200040308     C                   ENDIF
432300040308     C*
432400040308     C* se esistono errori sulla videata
432500040308     C* emetto la write del formato a indicatori spenti per vedere
432600040308     C* le eventuali decodifiche
432700040308     C     *IN99         IFEQ      *ON
432800040308     C     $GEST         ANDEQ     'D6'
432900040308     C                   MOVEA     *IN           WIN
433000040308     C                   MOVE      *ALL'0'       IN5098           49
433100040308     C                   MOVEA     IN5098        *IN(50)
433200040308     C                   WRITE     FMTD6
433300040308     C                   MOVEA     WIN           *IN
433400040308     C                   ENDIF
433500040308     C*
433600040308     C                   EXFMT     FMTD6
433700040308     C*
433800040308     C* azzero la posizione del cursore
433900040308     C                   Z-ADD     0             H1RIGA
434000040308     C                   Z-ADD     0             H1COLO
434100040308     C*
434200040308     C* controllo abilitazione tasti funzione
434300040308     C                   EXSR      CTRKD6
434400040308     C*
434500040308     C     *IN99         IFEQ      *OFF
434600040308     C*
434700040308     C                   SELECT
434800040308     C* F5=Refresh
434900040308     C     $TASTO        WHENEQ    F05
435000040308     C                   EXSR      F05D6
435100040308     C* F12=Ritorno
435200040308     C     $TASTO        WHENEQ    F12
435300040308     C                   EXSR      F12D6
435400040308     C* F24=Altri tasti
435500040308     C     $TASTO        WHENEQ    F24
435600040308     C                   EXSR      F24D6
435700040308     C*
435800040308     C                   OTHER
435900040308     C                   EXSR      CTRD6
436000040308     C*
436100040308     C     *IN99         IFEQ      *OFF
436200040308     C     $TASTO        ANDEQ     F06
436300040308     C* confermo la modifica della data di registrazione con quella digitata
436400040308     C                   Eval      NewDtReg6  =   WNewDtReg6
436500040308     C                   Eval      $Gest      =   'S1'
436600040308     C                   Eval      Conferma6  =   *On
436700040308     C*
436800040308     C                   ENDIF
436900040308     C*
437000040308     C                   ENDSL
437100040308     C*
437200040308     C                   ENDIF
437300040308     C*
437400040308     C                   ENDSR
437500040308     C/EJECT
437600040308     C************************************************************
437700040308     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D6
437800040308     C************************************************************
437900040308     C     CTRKD6        BEGSR
438000040308     C*
438100970903     C                   SETOFF                                       99
438200970903     C                   MOVEA     $FC6          $FC
438300970903     C                   EXSR      CTRKEY
438400970903     C     *IN99         IFEQ      *ON
438500970903     C                   SETON                                        97
438600970903     C                   ENDIF
438700970903     C*
438800970903     C                   ENDSR
438900970903     C/EJECT
439000970903     C************************************************************
439100970903     C* INIZIALIZZAZIONE VIDEATA D6
439200970903     C************************************************************
439300970903     C     INZD6         BEGSR
439400970903     C*
439500970903     C                   CLEAR                   FMTD6
439600970903     C* emetto l'ultima data impostata e confermata
439700980220     C     NewDtReg6     ifne      *Loval
439800040308C1309X******Dmy          Move      NewDtReg6     D6NewDtReg
439900000117C1309C     *JobRun       Move      NewDtReg6     D6NewDtReg
440000970903     C                   endif
440100970903     C*
440200970903     C                   ENDSR
440300970903     C/EJECT
440400970903     C************************************************************
440500970903     C* GESTIONE F05 VIDEO D6
440600970903     C************************************************************
440700970903     C     F05D6         BEGSR
440800970903     C*
440900970903     C                   MOVE      *ON           $InzD6
441000970903     C*
441100970903     C                   ENDSR
441200970903     C/EJECT
441300970903     C************************************************************
441400970903     C* GESTIONE F12 VIDEO D6
441500970903     C************************************************************
441600970903     C     F12D6         BEGSR
441700970903     C*
441800970903     C                   MOVE      'S1'          $Gest
441900970903     C*
442000970903     C                   ENDSR
442100970903     C/EJECT
442200970903     C************************************************************
442300970903     C* GESTIONE F24 VIDEO D6
442400970903     C************************************************************
442500970903     C     F24D6         BEGSR
442600970903     C*
442700970903     C                   MOVEA     $FX6          $FX
442800970903     C                   MOVEA     $FC6          $FC
442900970903     C                   MOVEA     $FR6          $FR
443000970903     C                   Z-ADD     $ULKD6        $ULK
443100970903     C                   Z-ADD     $D6L01        $L01
443200970903     C                   Z-ADD     $D6L02        $L02
443300970903     C                   EXSR      RIEKEY
443400970903     C                   Z-ADD     $ULK          $ULKD6            3 0
443500970903     C                   MOVEA     $FC           $FC6
443600970903     C                   MOVEL     $KEY1         Z6KE1
443700060224C1913C                   MOVEL     $ALLFUNCT     H6ALLFUNCT
443800970903     C                   WRITE     FMTZ6
443900970903     C*
444000970903     C                   ENDSR
444100970903     C/EJECT
444200970903     C************************************************************
444300970903     C* CONTROLLO VIDEATA D6
444400970903     C************************************************************
444500970903     C     CTRD6         BEGSR
444600970903     C*
444700970903     C                   SETOFF                                       99
444800970903     C*
444900970916     C* NUOVA data NON  obbligatoria
445000980220     C     D6NewDtReg    ifne      0
445100970903     C* controllo validità
445200040308C1309X******Dmy          Test(D)                 D6NewDtReg             21
445300000117C1309C     *JobRun       Test(D)                 D6NewDtReg             21
445400970916     C                   If        *In21      =  *Off
445500040308C1309X******Dmy          Move      D6NewDtReg    WNewDtReg6
445600000117C1309C     *JobRun       Move      D6NewDtReg    WNewDtReg6
445700970903     C                   else
445800970903     C* data errata
445900970903     C                   SetOn                                        5199
446000970903     C                   endif
446100970916     C                   else
446200970916     C                   Eval      WNewDtReg6 =  *Loval
446300970903     C                   endif
446400970903     C*
446500970903     C                   ENDSR
446600970903     C/EJECT
446700970821     C************************************************************
446800970821     C* CONTROLLO ABILITAZIONE TASTI
446900970821     C* INPUT
447000970821     C* $TASTO =Tasto funzione premuto
447100970821     C* $FC = Tasti abilitati
447200970821     C* OUTPUT
447300970821     C* IN99 = ERrore
447400970821     C************************************************************
447500970821     C     CTRKEY        BEGSR
447600970821     C*
447700970821     C                   SETOFF                                       99
447800970821     C*
447900970821     C                   SELECT
448000970821     C     $TASTO        WHENEQ    F01
448100970821     C                   z-add     01            posfun
448200970821     C     $FC(1)        IFEQ      '0'
448300970821     C                   SETON                                        9997
448400970821     C                   ENDIF
448500970821     C     $TASTO        WHENEQ    F02
448600970821     C                   z-add     02            posfun
448700970821     C     $FC(2)        IFEQ      '0'
448800970821     C                   SETON                                        9997
448900970821     C                   ENDIF
449000970821     C     $TASTO        WHENEQ    F03
449100970821     C                   z-add     03            posfun
449200970821     C     $FC(3)        IFEQ      '0'
449300970821     C                   SETON                                        9997
449400970821     C                   ENDIF
449500970821     C     $TASTO        WHENEQ    F04
449600970821     C                   z-add     04            posfun
449700970821     C     $FC(4)        IFEQ      '0'
449800970821     C                   SETON                                        9997
449900970821     C                   ENDIF
450000970821     C     $TASTO        WHENEQ    F05
450100970821     C                   z-add     05            posfun
450200970821     C     $FC(5)        IFEQ      '0'
450300970821     C                   SETON                                        9997
450400970821     C                   ENDIF
450500970821     C     $TASTO        WHENEQ    F06
450600970821     C                   z-add     06            posfun
450700970821     C     $FC(6)        IFEQ      '0'
450800970821     C                   SETON                                        9997
450900970821     C                   ENDIF
451000970821     C     $TASTO        WHENEQ    F07
451100970821     C                   z-add     07            posfun
451200970821     C     $FC(7)        IFEQ      '0'
451300970821     C                   SETON                                        9997
451400970821     C                   ENDIF
451500970821     C     $TASTO        WHENEQ    F08
451600970821     C                   z-add     08            posfun
451700970821     C     $FC(8)        IFEQ      '0'
451800970821     C                   SETON                                        9997
451900970821     C                   ENDIF
452000970821     C     $TASTO        WHENEQ    F09
452100970821     C                   z-add     09            posfun
452200970821     C     $FC(09)       IFEQ      '0'
452300970821     C                   SETON                                        9997
452400970821     C                   ENDIF
452500970821     C     $TASTO        WHENEQ    F10
452600970821     C                   z-add     10            posfun
452700970821     C     $FC(10)       IFEQ      '0'
452800970821     C                   SETON                                        9997
452900970821     C                   ENDIF
453000970821     C     $TASTO        WHENEQ    F11
453100970821     C                   z-add     11            posfun
453200970821     C     $FC(11)       IFEQ      '0'
453300970821     C                   SETON                                        9997
453400970821     C                   ENDIF
453500970821     C     $TASTO        WHENEQ    F12
453600970821     C                   z-add     12            posfun
453700970821     C     $FC(12)       IFEQ      '0'
453800970821     C                   SETON                                        9997
453900970821     C                   ENDIF
454000970821     C     $TASTO        WHENEQ    F13
454100970821     C                   z-add     13            posfun
454200970821     C     $FC(13)       IFEQ      '0'
454300970821     C                   SETON                                        9997
454400970821     C                   ENDIF
454500970821     C     $TASTO        WHENEQ    F14
454600970821     C                   z-add     14            posfun
454700970821     C     $FC(14)       IFEQ      '0'
454800970821     C                   SETON                                        9997
454900970821     C                   ENDIF
455000970821     C     $TASTO        WHENEQ    F15
455100970821     C                   z-add     15            posfun
455200970821     C     $FC(15)       IFEQ      '0'
455300970821     C                   SETON                                        9997
455400970821     C                   ENDIF
455500970821     C     $TASTO        WHENEQ    F16
455600970821     C                   z-add     16            posfun
455700970821     C     $FC(16)       IFEQ      '0'
455800970821     C                   SETON                                        9997
455900970821     C                   ENDIF
456000970821     C     $TASTO        WHENEQ    F17
456100970821     C                   z-add     17            posfun
456200970821     C     $FC(17)       IFEQ      '0'
456300970821     C                   SETON                                        9997
456400970821     C                   ENDIF
456500970821     C     $TASTO        WHENEQ    F18
456600970821     C                   z-add     18            posfun
456700970821     C     $FC(18)       IFEQ      '0'
456800970821     C                   SETON                                        9997
456900970821     C                   ENDIF
457000970821     C     $TASTO        WHENEQ    F19
457100970821     C                   z-add     19            posfun
457200970821     C     $FC(19)       IFEQ      '0'
457300970821     C                   SETON                                        9997
457400970821     C                   ENDIF
457500970821     C     $TASTO        WHENEQ    F20
457600970821     C                   z-add     20            posfun
457700970821     C     $FC(20)       IFEQ      '0'
457800970821     C                   SETON                                        9997
457900970821     C                   ENDIF
458000970821     C     $TASTO        WHENEQ    F21
458100970821     C                   z-add     21            posfun
458200970821     C     $FC(21)       IFEQ      '0'
458300970821     C                   SETON                                        9997
458400970821     C                   ENDIF
458500970821     C     $TASTO        WHENEQ    F22
458600970821     C                   z-add     22            posfun
458700970821     C     $FC(22)       IFEQ      '0'
458800970821     C                   SETON                                        9997
458900970821     C                   ENDIF
459000970821     C     $TASTO        WHENEQ    F23
459100970821     C                   z-add     23            posfun
459200970821     C     $FC(23)       IFEQ      '0'
459300970821     C                   SETON                                        9997
459400970821     C                   ENDIF
459500970821     C     $TASTO        WHENEQ    F24
459600970821     C                   z-add     24            posfun
459700970821     C     $FC(24)       IFEQ      '0'
459800970821     C                   SETON                                        9997
459900970821     C                   ENDIF
460000970821     C                   ENDSL
460100970821     C*
460200970821     C                   ENDSR
460300970821     C/EJECT
460400970821     C************************************************************
460500970821     C* AGGIORNAMENTO VALORI ASSUNTI
460600970821     C************************************************************
460700970821     C     AGGMPE        BEGSR
460800970821     C*
460900970821     C* preparo i parametri di chiamata al driver
461000970821     C                   CLEAR                   XMPEDS
461100970821     C                   EVAL      TpChiamata = '03'
461200970821     C                   EVAL      NChiavi = 5
461300970821     C                   EVAL      VLogica = 2
461400970821     C*
461500970821     C* preparo le chiavi per la chiamata al driver
461600970821     C                   CLEAR                   XMPEDS1
461700970821     C                   MOVEL     XSCSOC        DSMPESOC
461800970821     C                   MOVEL     DSPGM         DSMPEPGM
461900970821     C                   MOVEL     KNMUS         DSMPEPRF
462000970821     C*
462100970821     C* Si memorizzano i dati di default a seconda delle modalità con
462200970821     C* cui il pgm è usato
462300970821     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
462400970821     C* MPETIP = 'PV'  pgm richiamato da menù
462500970821     C* MPETIP = 'PB'  x lancio Batch
462600970821     C*
462700970821     C                   SELECT
462800970821    >C     OPZ013        WHENEQ    '01'
462900970821     C                   MOVEL     'PX'          DSMPETIP
463000970821     C                   OTHER
463100970821     C                   MOVEL     'PV'          DSMPETIP
463200970821     C                   ENDSL
463300970821     C* dati
463400970821     C                   MOVEL     DSMPE         DSMPEPAR
463500970821     C*
463600970821     C                   CALLB     'XMPE'
463700970821     C                   PARM                    XMPEDS
463800970821     C                   PARM                    XMPEDS1
463900970821     C*
464000970821     C                   ENDSR
464100970821     C/EJECT
464200970821     C************************************************************
464300970821     C* OPERAZIONI INIZIALI
464400970821     C************************************************************
464500970821     C     *INZSR        BEGSR
464600000117C1309C     *DMY          Move      Udate         ZDateISO
464700000117C1309C     *JobRun       Move      ZDateISO      ZDate             6 0
464800970821     C*
464900970821     C* Reperimento parametri
465000970821     C*
465100970821     C     *ENTRY        PLIST
465200970821     C                   PARM                    KPJBA
465300970821     C                   PARM                    DSAUT
465400970821     C*
465500970821     C* testo se sono stati passati i dati di autorizzazione
465600970821     C*
465700970821     C     PARMS         IFLT      2
465800970821     C                   MOVE      *BLANK        AUTDS
465900970821     C                   ELSE
466000970821     C                   MOVEL     DSAUT         AUTDS
466100970821     C                   ENDIF
466200970821     C*
466300970821     C* Reperimento dati società
466400970821     C*
466500970821     C                   MOVEL     'SOC001'      TIPXSC
466600970821     C                   MOVEL     *BLANK        SOCXSC
466700970821     C                   EXSR      REPSOC
466800970821     C*
466900970821     C* Controlli dati società
467000970821     C*
467100970821     C     RTNXSC        IFNE      '1'
467200970821     C                   MOVEL     XSOCDS        SOC001
467300970821     C                   ELSE
467400970821     C                   MOVE      *ON           $FINE
467500970821    >C                   MOVE      '1'           Err013
467600970821     C                   EXSR      ENDPGM
467700970821     C                   ENDIF
467800970821     C*
467900970821     C* reperisco le dimensioni delle variabili che conterranno i tasti
468000970821     C                   EVAL      $D1L01 = %Size(Z1Ke1)
468100970821     C                   EVAL      $D1L02 = %Size(Z1Ke2)
468200970821     C                   EVAL      $S1L01 = %Size(Z2Ke1)
468300970821     C                   EVAL      $S1L02 = %Size(Z2Ke2)
468400970821     C                   EVAL      $C1Op1 = %Size(C1Opz1)
468500970821     C                   EVAL      $C1Op2 = %Size(C1Opz2)
468600970821     C                   EVAL      $D2L01 = %Size(Z3Ke1)
468700970821     C                   EVAL      $D2L02 = 0
468800970827     C                   EVAL      $D5L01 = %Size(Z5Ke1)
468900970827     C                   EVAL      $D5L02 = 0
469000970916     C                   EVAL      $D6L01 = %Size(Z6Ke1)
469100970916     C                   EVAL      $D6L02 = 0
469200970825    > * aggiungere altre variabili nel caso di videate indipendenti
469300970825    > * ulteriori a quelle std
469400970821     C*
469500970821     C* Reperimento opzioni
469600970821     C*
469700970821     C                   EXSR      REPOPZ
469800970821     C*
469900970821     C* Reperimento tasti di funzione
470000970821     C*
470100970821     C                   EXSR      REPKEY
470200970821     C* Chiamata a routine per il reperimento delle opzioni
470300970821     C* personalizzate
470400970825    > * se si vogliono abilitare i tasti personalizzati in ogni videata
470500970825    > * ripeterla passando le schiere dei tasti funzione
470600970821     C                   CAllb     'X46USREXT'
470700970821     C                   PARM                    $OP1
470800970821     C                   PARM                    $TX1
470900970821     C                   PARM                    $FL1
471000970821     C                   PARM                    $PGO1
471100970821     C                   PARM      $dimOp        NumOpz
471200970821     C                   PARM                    $FX2
471300970821     C                   PARM                    $FC2
471400970821     C                   PARM                    $PGF2
471500970821     C                   PARM                    DsPgm
471600970821     C                   PARM                    xsccut
471700970821     C                   PARM                    xscgrp
471800970821     C*
471900970821     C                   Z-add     1             OPZLEN
472000970821     C     *blank        Lookup    $TX1(opzlen)                           21
472100970821     C                   if        *in21 = *on
472200970821     C                   sub       1             OPZLEN
472300970821     C                   else
472400970821     C                   Z-add     $DimOp        OPZLEN
472500970821     C                   endif
472600970828     C*
472700970828     C* reperisco la data odierna
472800970828     C     *Eur          Move      *Date         UpdDate
472900970821     C*
473000970821     C                   ENDSR
473100970821     C/EJECT
473200970821     C************************************************************
473300970821     C* INIZIALIZZAZIONE VARIABILI
473400970821     C************************************************************
473500970901     C     InzVar        BegSr
473600970821     C*
473700970821     C* Dati da PGM chiamante
473800970821     C*
473900970821     C                   MOVEL     KPJBU         Ndc013DS
474000970821     C*
474100970821     C* Se società diversa da quella impostata nella *inzsr, rieseguo
474200970821     C* reperimento dati società
474300040308     C                   IF        Societa013 <> xscsoc  and
474400040308     C                             Opz013 <>*blank
474500970821     C                   MOVEL     'SOC001'      TIPXSC
474600040308     C                   MOVEL     Societa013    SOCXSC
474700970821     C                   EXSR      REPSOC
474800970821     C     RTNXSC        IFNE      '1'
474900970821     C                   MOVEL     XSOCDS        SOC001
475000970821     C                   ELSE
475100970821     C                   MOVE      *ON           $FINE
475200040308     C                   MOVE      '1'           Err013
475300970821     C                   EXSR      ENDPGM
475400970821     C                   ENDIF
475500970821     C                   ENDIF
475600970821     C*
475700970821     C* Reperimento autorizzazioni
475800970821     C*
475900040308      ** nel caso l'anagrafica non gestisse le autorizzazioni
476000040308      **  asteriscare la chiamata alla subroutine REPAUT
476100970821     C* le autorizzazioni potrebbero essere già state passate,
476200970821     C* nel qual caso non vanno reperite
476300040308     C     AUTDS         IFEQ      *BLANK
476400040308     C                   EXSR      REPAUT
476500040308     C                   ELSE
476600040308     C                   MOVEL     XCADDS        ConPNoDS
476700040308     C                   ENDIF
476800970821     C*
476900970821     C* Reperimento valori default utente
477000970821     C*
477100970821     C                   EXSR      REPDFT
477200980303      *
477300980303      * Se provengo dal programma NDC324R.... imposto i campi video
477400980303      * perchè questo programma si apre con la lista
477500980303     C
477600980303     C                   If        opz013        = '07'
477700980303     C                   Z-ADD     2             $LOP
477800980303     c                   endif
477900970821     C*
478000970821     C* Definizione delle opzioni utilizzabili a pgm
478100970821     C*
478200970821     C                   EXSR      DEFOPZ
478300970821     C*
478400970821     C* Variabili per gestione videate
478500970821     C*
478600970821     C                   MOVE      *OFF          $FINE                          fine pgm
478700970821     C                   MOVE      *OFF          $INZD1                         inizializzione D1
478800970821     C                   MOVE      *OFF          $INZS1                         inizializzione S1
478900970821     C                   MOVE      *OFF          $INZD2                         inizializzione D2
479000970827     C                   MOVE      *OFF          $INZD5                         inizializzione D2
479100970916     C                   MOVE      *OFF          $INZD6                         inizializzione D2
479200970821     C                   MOVE      *OFF          $EndFile                       memo fine file
479300970821     C                   MOVE      *OFF          $ESCI                          rich.uscita ciclo
479400970821     C                   MOVE      *OFF          $RCDOK                         rcd letto valido
479500970821     C                   MOVE      *HIVAL        $LASTV
479600970821     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
479700970821     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
479800970821     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
479900970827     C                   Z-ADD     0             $ULKD5            3 0          piede da emett. D1
480000970916     C                   Z-ADD     0             $ULKD6            3 0          piede da emett. D1
480100970821     C* azzero la posizione del cursore
480200970821     C                   Z-ADD     0             H1RIGA
480300970821     C                   Z-ADD     0             H1COLO
480400970821     C*
480500970821     C                   MOVE      *ON           $INZD1
480600970821     C                   MOVE      *ON           $INZS1
480700970821     C*
480800970821     C*
480900970821     C* Variabili appoggio
481000970827     C                   Eval      NewDtReg  =   *Loval
481100970903     C                   Eval      NewDtReg6 =   *Loval
481200970901     C                   Eval      UnaRiga1  =   *Off
481300970901     C                   Eval      UnaRiga2  =   *Off
481400970916     C                   Clear                   WTpRegz
481500971217     C                   CLEAR                   WDocIVA
481600970821     C*
481700970827     C* inizialmente imposto il numero di righe per pagina a 8
481800970827     C                   Eval      SflPag    =   8
481900970821     C                   Z-ADD     SFLPAG        WPAG
482000970821     C                   MOVEL     *OFF          WOPR
482100970822     C                   Clear                   W1POS
482200970822     C                   Eval      W1PosDt    =  *Zero
482300970821     C*
482400970821     C* definisco qual'è la prima videata
482500970821     C* (se la prima videata risulta la lista, devo comunque caricare
482600970821     C*  la videata di parzializzazione e gestire la definizione
482700970821     C*  dell'ordinamento, perchè, dai default oppure se sono stati
482800970821     C*  passati dei dati, può variare il caricamento)
482900970821     C                   SELECT
483000970821     C     $LOP          WHENEQ    1
483100970821     C                   MOVE      'D1'          $GEST
483200970821     C     $LOP          WHENEQ    2
483300970821     C                   MOVE      'S1'          $GEST
483400970821     C                   EXSR      INZD1
483500970821     C                   MOVE      *OFF          $INZD1
483600970821     C                   EXSR      DEFORD
483700970821     C                   ENDSL
483800970916     C*
483900970916     C                   MOVE      D1TpRegz      WTpRegz
484000971217     C                   MOVE      D1DocIVA      WDocIVA
484100970821     C*
484200970821     C* Valorizzazione campi univoci testate
484300970821     C*
484400970821     C                   CLEAR                   FMTT1
484500970821     C                   MOVEL     KNSIF         NOMSIF
484600970901     C                   MOVEL     KNSIF         NOMSIs
484700970821     C                   MOVEL     XSCDSI        NOMDIT
484800970821     C                   MOVEL     DSPGM         NOMPGM
484900980305     C*
485000980305     C* visualizzo il sfl troncato
485100980305     C                   Eval      C1Mode    =   *On
485200980305     C*
485300970821     C* recupero il sottotitolo (se c'è)
485400970821     C                   MOVEL     *BLANK        T1TIT
485500970821     C     SOTTIT        IFNE      *BLANK
485600970821     C                   MOVEL     SOTTIT        IDMSG
485700970821     C                   CALLB     'XRTVM'
485800970821     C                   PARM                    IDMSG            27
485900970821     C                   PARM                    TXTMSG          100
486000970821     C                   PARM                    ERRMSG            1
486100970821     C                   PARM      *ON           CTRMSG            1            centratura
486200970821     C                   PARM      30            LENMSG            3 0          lun output.
486300970821     C     ERRMSG        IFNE      *ON
486400970821     C                   MOVEL     TXTMSG        T1TIT
486500970821     C                   ELSE
486600970821     C                   MOVEL     *ALL'?'       T1TIT
486700970821     C                   ENDIF
486800970821     C                   ENDIF
486900970821     C*
487000970901     C*
487100970821     C                   ENDSR
487200970821     C/EJECT
487300970821     C************************************************************
487400970821     C* REPERIMENTO DATI SOCIETA'
487500970821     C************************************************************
487600970821     C     REPSOC        BEGSR
487700970821     C*
487800970821     C                   CALLB     'XSOC'
487900970821     C                   PARM                    TIPXSC            6
488000970821     C                   PARM                    SOCXSC            3
488100970821     C                   PARM                    CDSXSC            9 0
488200970821     C                   PARM                    MODXSC            3
488300970821     C                   PARM      *BLANKS       RTNXSC            1
488400970821     C                   PARM                    XSOCDS
488500970821     C                   PARM                    KPJBA
488600970821     C*
488700970821     C                   ENDSR
488800970821     C/EJECT
488900970821     C************************************************************
489000970821     C* REPERIMENTO AUTORIZZAZIONI UTENTE
489100970821     C************************************************************
489200970821     C     REPAUT        BEGSR
489300970821     C*
489400970821     C                   CLEAR                   AUTDS
489500970821     C*
489600970821     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
489700970821     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
489800970821     C                   Z-ADD     0             XCAGRP                         GRUPPO
489900970821     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
490000970825    >C                   MOVEL     'CONPNO'      XCAFNC                         FUNZIONE
490100970821    >C                   MOVEL     *ZERO         XCAVFU                         VARIABILE
490200970821    >C                   MOVE      'CK'          XCATCK                         TIPO CTRL
490300970821    >C                   MOVE      '1'           XCANAU                         RICH.XNOAUT
490400970821     C*
490500970821     C                   CALLB     'XCHKAUT'
490600970821     C                   PARM                    AUTDS
490700970821     C*
490800970821     C* errori/non abilitazione
490900970821     C     XCARTN        IFNE      0
491000040308     C                   MOVE      '2'           Err013
491100970821     C                   EXSR      ENDPGM
491200970821     C                   ELSE
491300040308     C                   MOVEL     XCADDS        ConPNoDS
491400970821     C                   ENDIF
491500970821     C*
491600970821     C                   ENDSR
491700970821     C/EJECT
491800970821     C************************************************************
491900970821     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
492000970821     C************************************************************
492100970821     C     REPDFT        BEGSR
492200970821     C*
492300970821     C* preparo i parametri di chiamata al driver
492400970821     C                   CLEAR                   XMPEDS
492500970821     C                   EVAL      TpChiamata = '01'
492600970821     C                   EVAL      NChiavi = 5
492700970821     C                   EVAL      VLogica = 2
492800970821     C*
492900970821     C* preparo le chiavi per la chiamata al driver
493000970821     C                   CLEAR                   XMPEDS1
493100970821     C                   MOVEL     XSCSOC        DSMPESOC
493200970821     C                   MOVEL     DSPGM         DSMPEPGM
493300970821     C                   MOVEL     KNMUS         DSMPEPRF
493400970821     C* Si reperiscono i dati di default a seconda delle modalità con
493500970821     C* cui il pgm è usato
493600970821     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
493700970821     C* MPETIP = 'PV'  pgm richiamato da menù
493800970821     C* MPETIP = 'PB'  x lancio Batch
493900970821     C                   SELECT
494000040308     C     OPZ013        WHENEQ    '01'
494100040308     C                   MOVEL     'PX'          DSMPETIP
494200970821     C                   OTHER
494300970821     C                   MOVEL     'PV'          DSMPETIP
494400970821     C                   ENDSL
494500970821     C*
494600970821     C                   CALLB     'XMPE'
494700970821     C                   PARM                    XMPEDS
494800970821     C                   PARM                    XMPEDS1
494900970821     C*
495000970821     C     Risultato     IFEQ      '0'
495100970821     C                   MOVEL     DSMPEPAR      DSMPE
495200970821     C* nel caso sia la prima volta che l'utente apre questa anagrafica
495300970821     C* i suoi valori di default vengono decisi a pgm
495400970821     C                   ELSE
495500970821     C                   Z-ADD     1             $LOP
495600970821     C                   Z-ADD     1             $ORD
495700040308     C*  elenco altri dati default specifici dell'anagrafica
495800970821     C                   ENDIF
495900980218     C*
496000970821     C                   ENDSR
496100970821     C/EJECT
496200970821     C************************************************************
496300970821     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
496400970821     C************************************************************
496500970821     C     DEFOPZ        BEGSR
496600970821     C*
496700970821     C* Questa routine deve valorizzare la schiera $FL1
496800970821     C* con S o N a seconda della utilizzabilità o meno
496900970821     C* dell'opzione correlata
497000970821     C*
497100970821     C* Inizialmente si considerano tutte le opzioni come valide
497200970821     C     1             DO        $DIMOP        X
497300970821     C* Imposta tutti i flg delle opzioni non a *blank, a 'S'
497400970821     C                   If        $TX1(X)  <> *Blank
497500970821     C                   MOVE      'S'           $FL1(X)
497600970821     C                   Else
497700970821     C                   Leave
497800970821     C                   EndIf
497900970821     C                   ENDDO
498000970821     C*
498100970821     C* Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
498200970821     C* con 1 o 0 a seconda della utilizzabilità o meno
498300970821     C* del tasto funzione correlato
498400970821     C*
498500970821     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
498600970821     C* di compilazione
498700970821     C                   RESET                   $FC1
498800970821     C                   RESET                   $FR1
498900970821     C                   RESET                   $FC2
499000970821     C                   RESET                   $FR2
499100970821     C                   RESET                   $FC3
499200970821     C                   RESET                   $FR3
499300970821     C*
499400970821     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
499500970821     C* invalido alcune opzioni
499600970821     C                   SELECT
499700970821     C* Se pgm richiamato in ricerca disattivo :
499800040308     C     OPZ013        WHENEQ    '01'
499900970821     C* F3=Fine
500000970821     C                   Z-ADD     3             X
500100970821     C                   MOVE      '0'           $FC1(X)
500200970821     C                   MOVE      '0'           $FC2(X)
500300970821     C                   MOVE      '0'           $FC3(X)
500400970821     C*
500500040308      *  aggiungere nello stesso modo le altre opzioni da
500600040308      *  disattivare (quelle specifiche per ogni pgm)
500700970821     C*
500800970821     C* Se pgm richiamato in interrogazione disattivo :
500900040308     C     OPZ013        WHENEQ    '05'
501000970821     C* F10=Immissione
501100970821     C                   Z-ADD     10            X
501200970821     C                   MOVE      '0'           $FC1(X)
501300970821     C                   MOVE      '0'           $FC2(X)
501400970821     C* 1=Selezione
501500970821     C                   Z-ADD     1             X
501600970821     C     01            LOOKUP    $OP1(X)                                21
501700970821     C     *IN21         IFEQ      *ON
501800970821     C                   MOVE      'N'           $FL1(X)
501900970821     C                   ENDIF
502000970821     C* 2=Modifica
502100970821     C                   Z-ADD     1             X
502200970821     C     02            LOOKUP    $OP1(X)                                21
502300970821     C     *IN21         IFEQ      *ON
502400970821     C                   MOVE      'N'           $FL1(X)
502500970821     C                   ENDIF
502600970821     C* 3=Copia
502700970821     C                   Z-ADD     1             X
502800970821     C     03            LOOKUP    $OP1(X)                                21
502900970821     C     *IN21         IFEQ      *ON
503000970821     C                   MOVE      'N'           $FL1(X)
503100970821     C                   ENDIF
503200970821     C* 4=Annullamento
503300970821     C                   Z-ADD     1             X
503400970821     C     04            LOOKUP    $OP1(X)                                21
503500970821     C     *IN21         IFEQ      *ON
503600970821     C                   MOVE      'N'           $FL1(X)
503700970821     C                   ENDIF
503800970821     C*
503900970821     C     OPZ013        WHENEQ    *BLANKS
504000970821     C* Se pgm non richiamato, disattivo subito :
504100970821     C* F8=Selezione seguente
504200970821     C                   Z-ADD     8             X
504300970821     C                   MOVE      '0'           $FC1(X)
504400970821     C                   MOVE      '0'           $FC2(X)
504500970821     C* 1=Selezione
504600970821     C                   Z-ADD     1             X
504700970821     C     01            LOOKUP    $OP1(X)                                21
504800970821     C     *IN21         IFEQ      *ON
504900970821     C                   MOVE      'N'           $FL1(X)
505000970821     C                   ENDIF
505100970821     C*
505200970821     C* In tutti gli altri casi, disattivo subito :
505300970821     C                   OTHER
505400970821     C* 1=Selezione
505500970821     C                   Z-ADD     1             X
505600970821     C     01            LOOKUP    $OP1(X)                                21
505700970821     C     *IN21         IFEQ      *ON
505800970821     C                   MOVE      'N'           $FL1(X)
505900970821     C                   ENDIF
506000970821     C*
506100970821     C                   ENDSL
506200970821     C*
506300970821     C* poi, disattivo a seconda delle autorizzazioni :
506400040308     X***  AUT___        IFEQ      *off
506500040308     X****               Z-ADD     1             X
506600040308     X**** 'XX'          LOOKUP    $OP1(X)                                21
506700040308     X**** *IN21         IFEQ      *ON
506800040308     X****               MOVE      'N'           $FL1(X)
506900040308     X****               ENDIF
507000040308     X****               ENDIF
507100040308     C*
507200040308     C                   ENDSR
507300040308     C/EJECT
507400040308     C************************************************************
507500040308     C* REPERIMENTO OPZIONI
507600040308     C************************************************************
507700970821     C     REPOPZ        BEGSR
507800970821     C*
507900970821     C     1             DO        $DIMOP        X
508000970821     C*
508100970821     C     $MS1(X)       IFNE      *BLANK
508200970821     C*
508300970821     C                   MOVEL     $MS1(X)       IDMSG
508400970821     C                   CALLB     'XRTVM'
508500970821     C                   PARM                    IDMSG            27
508600970821     C                   PARM                    TXTMSG          100
508700970821     C                   PARM                    ERRMSG            1
508800970821     C*
508900970821     C     ERRMSG        IFNE      *ON
509000970821     C                   MOVEL     TXTMSG        $TX1(X)
509100970821     C                   ELSE
509200970821     C                   MOVEL     *ALL'?'       $TX1(X)
509300970821     C                   ENDIF
509400970821     C*
509500970821     C                   ENDIF
509600970821     C*
509700970821     C                   ENDDO
509800970821     C*
509900970821     C                   ENDSR
510000970821     C/EJECT
510100970821     C************************************************************
510200970821     C* REPERIMENTO TASTI DI FUNZIONE
510300970821     C************************************************************
510400970821     C     REPKEY        BEGSR
510500970821     C                   MOVEA     $FM1          $FM
510600970821     C                   MOVEA     $FC1          $FC
510700970821     C                   EXSR      REPKKK
510800970821     C                   MOVEA     $FX           $FX1
510900970821     C*
511000970821     C                   MOVEA     $FM2          $FM
511100970821     C                   MOVEA     $FC2          $FC
511200970821     C                   EXSR      REPKKK
511300970821     C                   MOVEA     $FX           $FX2
511400970821     C*
511500970821     C                   MOVEA     $FM3          $FM
511600970821     C                   MOVEA     $FC3          $FC
511700970821     C                   EXSR      REPKKK
511800970821     C                   MOVEA     $FX           $FX3
511900970828     C*
512000970828     C                   MOVEA     $FM5          $FM
512100970828     C                   MOVEA     $FC5          $FC
512200970828     C                   EXSR      REPKKK
512300970828     C                   MOVEA     $FX           $FX5
512400970903     C*
512500970903     C                   MOVEA     $FM6          $FM
512600970903     C                   MOVEA     $FC6          $FC
512700970903     C                   EXSR      REPKKK
512800970903     C                   MOVEA     $FX           $FX6
512900970821     C                   ENDSR
513000970821     C/EJECT
513100970821     C************************************************************
513200970821     C* REPERIMENTO TASTI DI FUNZIONE
513300970821     C************************************************************
513400970821     C     REPKKK        BEGSR
513500970821     C*
513600970821     C                   CLEAR                   $FX
513700970821     C     1             DO        24            X
513800970821     C*
513900970821     C     $FM(X)        IFNE      *BLANK
514000970821     C     $Fc(X)        andne     '0'
514100970821     C*
514200970821     C                   MOVEL     $FM(X)        IDMSG
514300970821     C                   CALLB     'XRTVM'
514400970821     C                   PARM                    IDMSG            27
514500970821     C                   PARM                    TXTMSG          100
514600970821     C                   PARM                    ERRMSG            1
514700970821     C*
514800970821     C     ERRMSG        IFNE      *ON
514900970821     C                   MOVEL     TXTMSG        $FX(X)
515000970821     C                   ELSE
515100970821     C                   MOVEL     *ALL' '       $FX(X)
515200970821     C                   ENDIF
515300970821     C*
515400970821     C                   ENDIF
515500970821     C*
515600970821     C                   ENDDO
515700970821     C*
515800970821     C                   ENDSR
515900970821     C/EJECT
516000970901     C*******************************************************************
516100970901     C* Overflow
516200970901     C*******************************************************************
516300970901     C     Over1         Begsr
516400970901     C*
516500970901     C                   Write     Testata1
516600970901     C                   Eval      *In39 = *Off
516700970901     C*
516800970901     C                   ENDSR
516900970901     C/EJECT
517000970901     C*******************************************************************
517100970901     C* Overflow
517200970901     C*******************************************************************
517300970901     C     Over2         Begsr
517400970901     C*
517500970901     C                   Write     Testata2
517600970901     C                   Eval      *In38 = *Off
517700970901     C*
517800970901     C                   ENDSR
517900970901     C/EJECT
518000970821     C************************************************************
518100970821     C* DEFINIZIONE KLIST
518200970821     C************************************************************
518300970821     C     DefKlist      BEGSR
518400970821     C*
518500970821     C* klist
518600970821     C*
518700040308     C* NdReg13L
518800040308     C     K05Reg13      Klist
518900040308     C                   Kfld                    RegSocieta
519000040308     C                   Kfld                    RegCtb
519100040308     C                   Kfld                    RegTpRegz
519200040308     C                   Kfld                    RegDocIva
519300040308     C                   Kfld                    RegDtReg
519400040308     C* NdReg01L
519500040308     C     K02Reg01      Klist
519600040308     C                   Kfld                    RegSys
519700040308     C                   Kfld                    RegNrAsReg
519800040308     C*
519900040308     C* NdReg13L
520000040308     C     K04Reg13      Klist
520100040308     C                   Kfld                    RegSocieta
520200040308     C                   Kfld                    RegCtb
520300040308     C                   Kfld                    RegTpRegz
520400040308     C                   Kfld                    RegDocIva
520500040308     C*
520600040308     C     K03Reg13      Klist
520700040308     C                   Kfld                    RegSocieta
520800040308     C                   Kfld                    RegCtb
520900040308     C                   Kfld                    RegTpRegz
521000040308     C*
521100040308     C     K02Reg13      Klist
521200040308     C                   Kfld                    RegSocieta
521300040308     C                   Kfld                    RegCtb
521400040308     C*
521500040308     C* NdReg04L
521600030318C1636C     K05Reg04      Klist
521700030318  "  C                   Kfld                    RegSocieta
521800030318  "  C                   Kfld                    RegCtb
521900030318  "  C                   Kfld                    RegDtReg
522000030318  "  C                   Kfld                    RegNrReg
522100030318  "  C                   Kfld                    RegSerReg
522200030318  "  C     K04Reg04      Klist
522300030318  "  C                   Kfld                    RegSocieta
522400030318  "  C                   Kfld                    RegCtb
522500030318  "  C                   Kfld                    RegDtReg
522600030318C1636C                   Kfld                    RegNrReg
522700040308     C     K03Reg04      Klist
522800040308     C                   Kfld                    RegSocieta
522900040308     C                   Kfld                    RegCtb
523000040308     C                   Kfld                    RegDtReg
523100040308     C*
523200040308     C     K02Reg04      Klist
523300040308     C                   Kfld                    RegSocieta
523400040308     C                   Kfld                    RegCtb
523500040308     C*
523600040308     C* AnUni01L
523700040308     C     K02Uni01      Klist
523800040308     C                   Kfld                    UniSocieta
523900040308     C                   Kfld                    UniUnita
524000970822     C*
524100130618    >C* NdReg99L
524200130618    >C     K03Reg99      Klist
524300130618    >C                   Kfld                    RegSocieta
524400130618    >C                   Kfld                    RegUnita
524500130618    >C                   Kfld                    RegKnrAz
524600130618     C*
524700970821     C                   ENDSR
524800970821     C/EJECT
524900970821**
525000970821PROMSG    *LIBL     OPZ0002
525100970821PROMSG    *LIBL     OPZ0004
525200970825PROMSG    *LIBL     OPZ0250
525300970902PROMSG    *LIBL     OPZ0127
525400970821**
52550097082102S      <Modifica
52560097082504S      <Cancella
52570097082508S      <Modifica data registrazione
52580097090222S      <Passaggio a definitivo
525900970821** TASTI DI FUNZIONE  D1
526000970821PROMSG    *LIBL     KEY0001  01
526100970821PROMSG    *LIBL     KEY0002  02
526200970821PROMSG    *LIBL     KEY0003  03
526300970821PROMSG    *LIBL     KEY0004  04
526400970821PROMSG    *LIBL     KEY0005  05
526500970821PROMSG    *LIBL     KEY0006  06
526600970821PROMSG    *LIBL     KEY0007  07
526700970821PROMSG    *LIBL     KEY0008  08
526800970821PROMSG    *LIBL     KEY0009  09
526900970821PROMSG    *LIBL     KEY0010  10
527000970821PROMSG    *LIBL     KEY0011  11
527100970821PROMSG    *LIBL     KEY0012  12
527200970821PROMSG    *LIBL     KEY0013  13
527300970821PROMSG    *LIBL     KEY0014  14
527400970821PROMSG    *LIBL     KEY0015  15
527500970821PROMSG    *LIBL     KEY0016  16
527600970821PROMSG    *LIBL     KEY0017  17
527700970821PROMSG    *LIBL     KEY0018  18
527800970821PROMSG    *LIBL     KEY0019  19
527900970821PROMSG    *LIBL     KEY0020  20
528000970821PROMSG    *LIBL     KEY0021  21
528100970821PROMSG    *LIBL     KEY0022  22
528200970821PROMSG    *LIBL     KEY0023  23
528300970821PROMSG    *LIBL     KEY0024  24
528400970821**  ABILITAZIONE D1
5285009708210N  01
5286009708211N  02
5287009708211N  03
5288009708211N  04
5289009708271N  05
5290009708210N  06
5291009708210N  07
5292009708211N  08
5293009708210N  09
5294009708220N  10
5295009708210N  11
5296009708211N  12
5297009708210N  13
5298009708210N  14
5299009708210N  15
5300009708210N  16
5301009708210N  17
5302009708290N  18
5303009708210N  19
5304009708210N  20
5305009708210N  21
5306009708210N  22
5307009708210N  23   Impostare a '1' se si utilizzano opzioni
5308009708211N  24   Lasciare sempre impostata a '1' il valore della riga 24
530900970821** TASTI DI FUNZIONE  S1
531000970821PROMSG    *LIBL     KEY0001  01
531100970821PROMSG    *LIBL     KEY0002  02
531200970821PROMSG    *LIBL     KEY0003  03
531300970821PROMSG    *LIBL     KEY0004  04
531400970821PROMSG    *LIBL     KEY0005  05
531500970821PROMSG    *LIBL     KEY0006  06
531600970821PROMSG    *LIBL     KEY0007  07
531700970821PROMSG    *LIBL     KEY0008  08
531800970821PROMSG    *LIBL     KEY0009  09
531900970821PROMSG    *LIBL     KEY0010  10
532000970821PROMSG    *LIBL     KEY0011  11
532100970821PROMSG    *LIBL     KEY0012  12
532200970825PROMSG    *LIBL     KEY0061  13
532300970821PROMSG    *LIBL     KEY0014  14
532400970821PROMSG    *LIBL     KEY0015  15
532500970821PROMSG    *LIBL     KEY0016  16
532600970821PROMSG    *LIBL     KEY0017  17
532700970821PROMSG    *LIBL     KEY0018  18
532800970821PROMSG    *LIBL     KEY0019  19
532900970821PROMSG    *LIBL     KEY0020  20
533000970821PROMSG    *LIBL     KEY0021  21
533100970821PROMSG    *LIBL     KEY0022  22
533200970821PROMSG    *LIBL     KEY0023  23
533300970821PROMSG    *LIBL     KEY0024  24
533400970821**  ABILITAZIONE S1
5335009708210N  01
5336009708210N  02
5337009708211N  03
5338009802191N  04
5339009708271N  05
5340009708210N  06
5341009708210N  07
5342009708211N  08
5343009708221N  09
5344009708220N  10
5345009708210N  11
5346009708211N  12
5347009708251N  13
5348009708210N  14
5349009708210N  15
5350009708210N  16
5351009708211N  17
5352009708290N  18
5353009708210N  19
5354009708210N  20
5355009708210N  21
5356009709020N  22
5357009708211N  23   Impostare a '1' se si utilizzano opzioni
5358009708211N  24   Lasciare sempre impostata a '1' il valore della riga 24
535900970821** TASTI DI FUNZIONE  D3
536000970821PROMSG    *LIBL     KEY0001  01
536100970821PROMSG    *LIBL     KEY0002  02
536200970821PROMSG    *LIBL     KEY0003  03
536300970821PROMSG    *LIBL     KEY0004  04
536400970821PROMSG    *LIBL     KEY0005  05
536500970821PROMSG    *LIBL     KEY0006  06
536600970821PROMSG    *LIBL     KEY0007  07
536700970821PROMSG    *LIBL     KEY0008  08
536800970821PROMSG    *LIBL     KEY0009  09
536900970821PROMSG    *LIBL     KEY0010  10
537000970821PROMSG    *LIBL     KEY0011  11
537100970821PROMSG    *LIBL     KEY0012  12
537200970821PROMSG    *LIBL     KEY0013  13
537300970821PROMSG    *LIBL     KEY0014  14
537400970821PROMSG    *LIBL     KEY0015  15
537500970821PROMSG    *LIBL     KEY0016  16
537600970821PROMSG    *LIBL     KEY0017  17
537700970821PROMSG    *LIBL     KEY0018  18
537800970821PROMSG    *LIBL     KEY0019  19
537900970821PROMSG    *LIBL     KEY0020  20
538000970821PROMSG    *LIBL     KEY0021  21
538100970821PROMSG    *LIBL     KEY0022  22
538200970821PROMSG    *LIBL     KEY0023  23
538300970821PROMSG    *LIBL     KEY0024  24
538400970821**  ABILITAZIONE D3
5385009708210N  01
5386009708210N  02
5387009708211N  03
5388009708210N  04
5389009708210N  05
5390009708210N  06
5391009708210N  07
5392009708210N  08
5393009708210N  09
5394009708210N  10
5395009708210N  11
5396009708211N  12
5397009708210N  13
5398009708210N  14
5399009708210N  15
5400009708210N  16
5401009708210N  17
5402009708290N  18
5403009708210N  19
5404009708210N  20
5405009708210N  21
5406009708210N  22
5407009708210N  23   Impostare a '1' se si utilizzano opzioni
5408009708211N  24   Lasciare sempre impostata a '1' il valore della riga 24
540900970827** TASTI DI FUNZIONE  D5
541000970827PROMSG    *LIBL     KEY0001  01
541100970827PROMSG    *LIBL     KEY0002  02
541200970827PROMSG    *LIBL     KEY0003  03
541300970827PROMSG    *LIBL     KEY0004  04
541400970827PROMSG    *LIBL     KEY0005  05
541500970827PROMSG    *LIBL     KEY0006  06
541600970827PROMSG    *LIBL     KEY0007  07
541700970827PROMSG    *LIBL     KEY0008  08
541800970827PROMSG    *LIBL     KEY0009  09
541900970827PROMSG    *LIBL     KEY0010  10
542000970827PROMSG    *LIBL     KEY0011  11
542100970827PROMSG    *LIBL     KEY0012  12
542200970827PROMSG    *LIBL     KEY0013  13
542300970827PROMSG    *LIBL     KEY0014  14
542400970827PROMSG    *LIBL     KEY0015  15
542500970827PROMSG    *LIBL     KEY0016  16
542600970827PROMSG    *LIBL     KEY0017  17
542700970827PROMSG    *LIBL     KEY0018  18
542800970827PROMSG    *LIBL     KEY0019  19
542900970827PROMSG    *LIBL     KEY0020  20
543000970827PROMSG    *LIBL     KEY0021  21
543100970827PROMSG    *LIBL     KEY0022  22
543200970827PROMSG    *LIBL     KEY0023  23
543300970827PROMSG    *LIBL     KEY0024  24
543400970827**  ABILITAZIONE D5
5435009708270N  01
5436009708270N  02
5437009709010N  03
5438009708270N  04
5439009708271N  05
5440009708271N  06
5441009708270N  07
5442009708270N  08
5443009708270N  09
5444009708270N  10
5445009708270N  11
5446009708271N  12
5447009708270N  13
5448009708270N  14
5449009708270N  15
5450009708270N  16
5451009708270N  17
5452009708270N  18
5453009708270N  19
5454009708270N  20
5455009708270N  21
5456009708270N  22
5457009708270N  23   Impostare a '1' se si utilizzano opzioni
5458009708271N  24   Lasciare sempre impostata a '1' il valore della riga 24
545900970902** TASTI DI FUNZIONE  D6
546000970902PROMSG    *LIBL     KEY0001  01
546100970902PROMSG    *LIBL     KEY0002  02
546200970902PROMSG    *LIBL     KEY0003  03
546300970902PROMSG    *LIBL     KEY0004  04
546400970902PROMSG    *LIBL     KEY0005  05
546500970902PROMSG    *LIBL     KEY0006  06
546600970902PROMSG    *LIBL     KEY0007  07
546700970902PROMSG    *LIBL     KEY0008  08
546800970902PROMSG    *LIBL     KEY0009  09
546900970902PROMSG    *LIBL     KEY0010  10
547000970902PROMSG    *LIBL     KEY0011  11
547100970902PROMSG    *LIBL     KEY0012  12
547200970902PROMSG    *LIBL     KEY0013  13
547300970902PROMSG    *LIBL     KEY0014  14
547400970902PROMSG    *LIBL     KEY0015  15
547500970902PROMSG    *LIBL     KEY0016  16
547600970902PROMSG    *LIBL     KEY0017  17
547700970902PROMSG    *LIBL     KEY0018  18
547800970902PROMSG    *LIBL     KEY0019  19
547900970902PROMSG    *LIBL     KEY0020  20
548000970902PROMSG    *LIBL     KEY0021  21
548100970902PROMSG    *LIBL     KEY0022  22
548200970902PROMSG    *LIBL     KEY0023  23
548300970902PROMSG    *LIBL     KEY0024  24
548400970902**  ABILITAZIONE D6
5485009709020N  01
5486009709020N  02
5487009709020N  03
5488009709020N  04
5489009709021N  05
5490009709021N  06
5491009709020N  07
5492009709020N  08
5493009709020N  09
5494009709020N  10
5495009709020N  11
5496009709021N  12
5497009709020N  13
5498009709020N  14
5499009709020N  15
5500009709020N  16
5501009709020N  17
5502009709020N  18
5503009709020N  19
5504009709020N  20
5505009709020N  21
5506009709020N  22
5507009709020N  23   Impostare a '1' se si utilizzano opzioni
5508009709021N  24   Lasciare sempre impostata a '1' il valore della riga 24
550900000128** Costante x XMSGR -$MsgId-
551000000128PROMSG    *LIBL     COS0543    Registrazione &1 &2 &3 non utilizzabile                   ** C1315 **
551100000128PROMSG    *LIBL     COS0563    perchè priva di Nr assoluto di registrazione              ** C1315 **
551200000128PROMSG    *LIBL     COS1016    contattare responsabile CED                               ** C1315 **
