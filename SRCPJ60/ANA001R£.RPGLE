000100941110     H*--------INIZIO SPECIFICHE --------------------------*
000200150909>>>>>H DFTACTGRP(*NO) BNDDIR('PJXBND') ACTGRP(*NEW)
000300150909>>>>>H CVTOPT(*DATETIME)
000400941110     H DECEDIT('0,') DATEDIT(*DMY.)
000500941110      *----------*
000600941110    > * ANA001R  *
000700941110      *----------*
000800941110    > *               ANAGRAFICA PROFILI
000900941110      *          ----------------------------
001000941110      *
001100941110      *
001200941110      * DOCUMENTI MICROANALISI :  NSDAMUS/ANALISI/PROF 001.DOC
001300941110      *
001400941110      * NOTE TECNICHE :
001500941110      *
001600941110      *
001700941110      * PERSONALIZZAZIONI
001800941110      *
001900941110      *
002000941110      *
002100941110      *               MAPPA INDICATORI
002200941110      *  01           CONDIZIONAMENTO CAMPO POSIZ. ALFANUM. (C1POS )
002300941110      *  02           CONDIZIONAMENTO CAMPO POSIZ. NUMERICO (C1POS2)
002400941110      *
002500941110      *  21           GENERICO OPERAZIONI I/O
002600941110      *  22           GENERICO ERRORE OPERAZIONI I/O
002700941110      *  30           SFLDSP
002800941110      * N31           SFLCLR
002900941110      *  31           SFLDSPCTL
003000941110      *  32           SFLNXTCHG
003100941110      *  33           SFLEND
003200941110      *  39           OF PRTF
003300941110      *  40 <---> 49  DSPATR ERRORI SU SFL
003400941110    > *  Specificare l'uso dei singoli indicatori
003500941110      *  50 <---> 98  ERRORI SU VIDEO
003600941110    > *  Specificare l'uso dei singoli indicatori
003700941110      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
003800941110      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
003900941110      *  99           INDIC. GENERALE DI ERRORE
004000941110    > *----------------------------------------------------------------
004100941110    > * Viste logiche utilizzate per i diversi ordinamenti
004200941110    > *----------------------------------------------------------------
004300941110      *------------
004400941110    >FANUTN01L  IF   E           K DISK
004500941110     F                                     RENAME(ANUTN000:ANUTN01)
004600941110      *------------
004700941110    >FANUTN03L  IF   E           K DISK
004800941110     F                                     RENAME(ANUTN000:ANUTN03)
004900941110     F*------------
005000941110     FANMPE02L  UF A E           K DISK
005100941110     F*------------
005200941110    >FANA001D   CF   E             WORKSTN
005300941110     F                                     RENAME(A001D1:FMTD1)
005400941110     F                                     RENAME(A001D2:FMTD2)
005500941110     F                                     RENAME(A001D3:FMTD3)
005600941110     F                                     RENAME(A001S1:FMTS1)
005700941110     F                                     RENAME(A001C1:FMTC1)
005800941110     F                                     RENAME(A001T1:FMTT1)
005900941110     F                                     RENAME(A001Z1:FMTZ1)
006000941110     F                                     RENAME(A001Z2:FMTZ2)
006100941110     F                                     RENAME(A001Z3:FMTZ3)
006200941110     F                                     SFILE(FMTS1:S1NRR)
006300941110     F                                     INFDS(DSFMT)
006400941110     D*----------------------------------------------------*
006500941110     D* Schiere per la gestione delle opzioni
006600941110     D*
006700991201A0709D** $MS1            S             27    DIM(5) CTDATA PERRCD(1)
006800991116  "  D** $TX1            S             50    DIM(5) ALT($MS1)
006900991116  "  D** $OP1            S              2  0 DIM(5) CTDATA PERRCD(1)
007000991116  "  D** $FL1            S              1    DIM(5) ALT($OP1)
007100040302  "  D $MS1            S             27    DIM(6) CTDATA PERRCD(1)
007200991123  "  D $TX1            S             50    DIM(6) ALT($MS1)
007300991123  "  D $OP1            S              2  0 DIM(6) CTDATA PERRCD(1)
007400991201A0709D $FL1            S              1    DIM(6) ALT($OP1)
007500950403     D $TX             S             50    DIM(50)
007600950403     D $FL             S              1    DIM(50)
007700941110     D* Schiere generali per la gestione dei tasti di funzione
007800941110     D $FM             S             27    DIM(24)
007900941110     D $FX             S             50    DIM(24)
008000941110     D $FC             S              1    DIM(24)
008100941110     D $FR             S              1    DIM(24)
008200941110     D* Schiere per la gestione dei tasti di funzione
008300941110     D* Video D1
008400941110     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
008500941110     D $FX1            S             50    DIM(24) ALT($FM1)
008600941110     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
008700941110     D $FR1            S              1    DIM(24) ALT($FC1)
008800991117     D* Schiera contente i nomi dei programmi da richiamare con i tasti
008900991117     D* funzione personalizzata S1
009000991117     D $PGF2           S             10    DIM(25)
009100941110     D* Schiere per la gestione dei tasti di funzione
009200941110     D* Video S1
009300941110     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
009400941110     D $FX2            S             50    DIM(24) ALT($FM2)
009500941110     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
009600941110     D $FR2            S              1    DIM(24) ALT($FC2)
009700941110     D* Schiere per la gestione dei tasti di funzione
009800941110     D* Video D2
009900941110     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
010000941110     D $FX3            S             50    DIM(24) ALT($FM3)
010100941110     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
010200941110     D $FR3            S              1    DIM(24) ALT($FC3)
010300941110     D*
010400941110     D/EJECT
010500941110     D*----------------------------------------------------*
010600941110     D*-------------
010700941110     D* Valori assunti
010800941115     D DSMPE           DS                  INZ
010900941110     D  $LOP                   1      1  0
011000941110     D  $ORD                   2      3  0
011100941110     D*-------------
011200941110     D* Passaggio Parametri
011300941110     D KPJBA         E DS
011400941110     D*-------------
011500941110     D*-------------
011600941110     D* Reperimento Autorizzazioni
011700941110     D*AUT      E DSXCHKAUTDS
011800941110     D*-------------
011900941110     D* Dati per pgm XSOC
012000941110     D XSC001        E DS                  EXTNAME(XSOC001DS)
012100020326A0910D    XscLaguna                   1    overlay(xscSkp:8)                    Scadenz. retroattivo
012200941110      * Passaggio Parametri al PGM XATB
012300941110     D XATBDS        E DS                  INZ
012400941110     D* Parametri in ricezione
012500980720    >D A001DS        E DS                  EXTNAME(ANA001DS) inz
012600941110     D*-------------
012700941110     D* Parametri del PGM di manutenzione STD002R
012800941110    >D A002DS        E DS                  EXTNAME(ANA002DS) INZ
012900941110    >D  RET002       E                     INZ('0')
013000941110    >D  OPR002       E                     INZ('0')
013100941110    >D  ERR002       E                     INZ('0')
013200030423A0997X*** ex-A0709 -------------
013300030423  "  X***   " Parametri per XPAR
013400030423  "  X***   " XPASOC         S              3
013500030423  "  X***   " XPAPAR         S              8
013600030423  "  X***   " XPAOUT         S           2000
013700030423  "  X***   " XPAERR         S              1
013800030423  "  X***   " XPARIC         S              1
013900030423  "  X***   " -----------
014000030423  "  X***   " Parametro modulo SICOM setup generale
014100030423  "  X***   " Sicom         E DS                  INZ
014200030423A0997X*** ex-A0709 -------------
014300941110     D*-------------
014400941110     D DSFMT           DS           512
014500941110     D  $TASTO               369    369
014600941110     D  NRG                  370    370
014700941110     D  NCL                  371    371
014800941110     D  SFLNRR               378    379B 0
014900941110     D*-------------
015000941110     D* posizione cursore
015100941110     D CURSOR          DS
015200941110     D  RIRI                   1      2B 0 INZ
015300941110     D  R$$                    2      2
015400941110     D  COCO                   3      4B 0 INZ
015500941110     D  C$$                    4      4
015600941110     D*-------------
015700941110     D* Reperimento nome PGM
015800941110     D STATUS         SDS           333
015900941110     D  DSPGM            *PROC
016000020705B0453D  PARMS            *PARMS
016100941110     D*-------------
016200941110     D* Costanti
016300941110     D* nr.di righe del sfl (SFLPAG)
016400020705B0453X*SFLPAG          C                   CONST(10)
016500020705B0453 * lo imposto = 9999 se F13                                                f13
016600020705  "  D SFLPAG          S              4S 0                                      f13
016700020705B0453D RigPag          C                   CONST(10)
016800941110     D* dimensione della schiera $MS1
016900991117a0709D*** $DIMOP          C                   CONST(5)
017000991123a0709D $DIMOP          C                   CONST(6)
017100941110     D*-------------
017200941110     D* Tasti di funzione
017300941110     D F01             C                   CONST(X'31')
017400941110     D F02             C                   CONST(X'32')
017500941110     D F03             C                   CONST(X'33')
017600941110     D F04             C                   CONST(X'34')
017700941110     D F05             C                   CONST(X'35')
017800941110     D F06             C                   CONST(X'36')
017900941110     D F07             C                   CONST(X'37')
018000941110     D F08             C                   CONST(X'38')
018100941110     D F09             C                   CONST(X'39')
018200941110     D F10             C                   CONST(X'3A')
018300941110     D F11             C                   CONST(X'3B')
018400941110     D F12             C                   CONST(X'3C')
018500941110     D F13             C                   CONST(X'B1')
018600941110     D F14             C                   CONST(X'B2')
018700941110     D F15             C                   CONST(X'B3')
018800941110     D F16             C                   CONST(X'B4')
018900941110     D F17             C                   CONST(X'B5')
019000941110     D F18             C                   CONST(X'B6')
019100941110     D F19             C                   CONST(X'B7')
019200941110     D F20             C                   CONST(X'B8')
019300941110     D F21             C                   CONST(X'B9')
019400941110     D F22             C                   CONST(X'BA')
019500941110     D F23             C                   CONST(X'BB')
019600941110     D F24             C                   CONST(X'BC')
019700941110     D ENTER           C                   CONST(X'F1')
019800941110     D ROLDWN          C                   CONST(X'F4')
019900941110     D ROLLUP          C                   CONST(X'F5')
020000941110     D*-------------
020100941110     D*
020200941110     D* Parametri per ricerca codice ANATB00F
020300941110     D XTABDS          DS                  INZ
020400941110     D* '  ' = Gestione
020500941110     D* '01' = Ricerca
020600941110     D* '02' = Gestione
020700941110     D* '05' = Visualizzazione
020800941110     D* '11' = Abilitazione
020900941110     D  OPZTAB                 1      2
021000941110     D* 0 = ritorno normale
021100941110     D* 1 = F3
021200941110     D* 2 = F12
021300941110     D  RETTAB                 3      3
021400941110     D* operazione eseguite dal pgm chiamato :
021500941110     D* 0 = nessuna operazione
021600941110     D* 1 = eseguito aggiornamento
021700941110     D  OPRTAB                 4      4
021800941110     D* funzione non eseguibile per errore :
021900941110     D* 0 = nessun errore
022000941110     D* 1 = funzione richiamata chiusa in errore
022100941110     D* Aggiungere eventuali altri codici di errore
022200941110     D  ERRTAB                 5      5
022300941110     D* Azienda
022400941110     D  AZITAB                 6      8
022500941110     D* Lingua
022600941110     D  LINTAB                 9      9
022700941110     D* Codice scelto
022800941110     D  KEYTAB                10     24
022900941110     D* riga cursore chiamante
023000941110     D  RIGTAB                25     26  0
023100941110     D* colonna cursore chiamante
023200941110     D  COLTAB                27     28  0
023201150909>>>>>D  posTab                29     30  0
023300941110     D ERRORE          C                   CONST('Codice errato')
023400941110     D DATA            C                   CONST('0001-01-01')
023500941110      * DS Interna per dati output di XSOC
023600941110     D XSOCDS          DS          1000
023700991201      * Flag di controllo per utente Web
023800991201     D $Trovato        s              1a
023900020705B0453 *====================================================*
024000020705  "   * variabili x gestire selezione multipla (F13)
024100020705  "   * quando sono chiamato con un F4:
024200020705  "   * il pgm che mi chiama con F4, mi passa ds multipla
024300020705  "   * (F13DS che conterrà i rec scelti)
024400020705  "   * ed il nr dei rec scelti (NElemF13DS);
024500020705  "   * se sono stati scelti + rec di quanti
024600020705  "   * la ds multipla possa contenere, vengo richiamato
024700020705  "   * solo per caricare la ds multipla con il resto
024800020705  "   * dei rec scelti.
024900020705  "   *====================================================*
025000020705  "   * Nr max righe caricabili nella DS da passare al chiamante
025100020705  "  D $DimF13DS       C                   Const(100)
025200020705  "   *--------
025300020705  "   * parametro ricevuto
025400020705  "   * (il chiamante può non passarlo)
025500020705  "   * DS da passare al chiamante con i rec selezionati con F13
025600020705  "  D F13DS         E DS                  extname(ANUTN00F)
025700020705  "  D                                     prefix(F13:3)
025800020705  "  D                                     Occurs($DimF13DS)
025900020705  "   *--------
026000020705  "   * parametro ricevuto
026100020705  "   * (obbligatorio se esiste anche il 2° parametro F13DS)
026200020705  "   * indica il nr elementi caricati nella ds multipla
026300020705  "   * (la sua lunghezza dipende dalla dimensione di F13DS)
026400020705  "   * caricherò la ds multipla con il resto dei rec scelti,
026500020705  "   * solo se parametro NElemF13DS= dimensione max ds multipla
026600020705  "  D NElemF13DS      s              3  0
026700020705  "   *--------
026800020705  "   * Indice ds multipla
026900020705  "  D $iF13DS         s              3  0  Inz
027000020705  "   *--------
027100020705  "   * se il chiamante mi ha passato al ds multipla, la caricherò
027200020705  "   * prendendo tutti quei rec del sfl con opz 1;
027300020705  "   * $RieF13 memorizza che a fine pgm dovrò fare il caricamento
027400020705  "  D $RieF13DS       s              1     Inz('0')
027500020705B0453 *----------------------------------------------------*
027501150908>>>>> /COPY GAITRASRC/SRCPROTOPR,QUSROBJD
027502150908>>>>> /COPY QSYSINC/QRPGLESRC,QUSROBJD
027503150908>>>>> /COPY QSYSINC/QRPGLESRC,QUSEC
027600941110      *----------------------------------------------------*
027700941110      *                MAIN LINE PROGRAM
027800941110      *----------------------------------------------------*
027900941110      *
028000941110      * inizializzazione variabili
028100941110     C                   EXSR      INZVAR
028200941110      *
028300941110     C     $FINE         DOWEQ     *OFF
028400941110     C     $GEST         CASEQ     'D1'          GESD1                          Parzializz.
028500941110     C     $GEST         CASEQ     'S1'          GESS1                          Lista
028600941110     C     $GEST         CASEQ     'D2'          GESD2                          Valori ass.
028700941124     C                   ENDCS
028800941124     C                   ENDDO
028900941110      * fine programma
029000941110     C                   EXSR      ENDPGM
029100941110      *
029200941110     C/EJECT
029300941110      ************************************************************
029400941110      * FINE PROGRAMMA
029500941110      ************************************************************
029600941110     C     ENDPGM        BEGSR
029700941110      *
029800941110      * aggiorno valori assunti
029900941110     C                   EXSR      AGGMPE
030000941110     C   KC              MOVE      '1'           RET001
030100941110     C   KL              MOVE      '2'           RET001
030200941110      *
030300020705B0453 *
030400020705  "   *  se mi hanno passato la ds multipla, esiste almeno un 1=selezione,
030500020705  "   *  e non si è verificato alcun errore
030600020705  "   *  riempio la ds multipla con i rec scelti se richiesto
030700020705  "  C                   if        $RieF13DS = *on
030800020705  "  C                   exsr      GesF13DS
030900020705  "  C                   endif
031000020705B0453 *
031100941110      * passaggio dati al pgm chiamante
031200941110    >C                   MOVEL     A001DS        KPJBU
031300941110      *
031400941110      * Se pgm Guida non è stato richiamato posso chiudere in LR
031500941110    >C     OPZ001        IFEQ      *BLANK
031600941110     C                   SETON                                            LR
031700941110      * altrimenti chiudo in RT
031800941110     C                   ELSE
031900941110     C                   SETON                                            RT
032000941110     C                   ENDIF
032100941110     C                   RETURN
032200941110      *
032300941110     C                   ENDSR
032400941110     C/EJECT
032500020705B0453 ************************************************************
032600020705  "   * Gestione ds multipla con i rec scelti
032700020705  "   ************************************************************
032800020705  "  C     GesF13DS      Begsr
032900020705  "   *
033000020705  "   * Contiene il nr elementi caricati nella ds multipla da
033100020705  "   * ripassare al chiamante (se = 0  ho già finito di caricare)
033200020705  "  C                   eval      NElemF13DS = 0
033300020705  "   *
033400020705  "  C                   exsr      ClrF13DS
033500020705  "   *
033600020705  "   * Riempio ds multipla se sfl NON vuoto
033700020705  "  C                   if        WMax <> 0
033800020705  "  C                   exsr      RieF13DS
033900020705  "  C                   endif
034000020705  "   *
034100020705B0453C     XGesF13DS     Endsr
034200020705B0453 ************************************************************
034300020705  "   * Pulisco ds multipla con i rec scelti
034400020705  "   ************************************************************
034500020705  "  C     ClrF13Ds      Begsr
034600020705  "   *
034700020705  "  C     1             do        $DimF13DS     $iF13DS
034800020705  "   *
034900020705  "  C     $iF13DS       occur     F13DS
035000020705  "  C                   clear                   F13DS
035100020705  "   *
035200020705  "  C                   enddo
035300020705  "   *
035400020705B0453C     XClrF13DS     Endsr
035500020705B0453 ************************************************************
035600020705  "   * Riempio ds multipla con i rec scelti
035700020705  "   ************************************************************
035800020705  "  C     RieF13DS      Begsr
035900020705  "   *--------------------------
036000020705  "   * siccome non ho fatto un ciclo completo di READC, ho il puntatore al
036100020705  "   * sfl sposizionato per cui devo eseguire queste operazioni per fare in
036200020705  "   * modo che al prossimo ciclo riparta da capo
036300020705  "   * - leggo l'ultimo rcd del sfl
036400020705  "  C     WMax          chain     FmtS1                              21
036500020705  "   * - se il rcd è da aggiornare ne faccio l'update e ciò che ne consegue
036600020705  "  C                   if        S1Opz   <> *Zero
036700020705  "  C                   exsr      AtrS1
036800020705  "  C                   move      *On           *In32
036900020705  "  C                   update    FmtS1
037000020705  "  C                   move      *Off          *In32
037100020705  "  C                   endif
037200020705  "   * - poi faccio una lettura che sicuramente andrà a vuoto
037300020705  "   *   così è riposizionato
037400020705  "  C                   readc     FmtS1                                  21
037500020705  "   *--------------------------------------------------------
037600020705  "   *
037700020705  "   *
037800020705  "  C                   eval      $iF13DS = 1
037900020705  "   *
038000020705  "  C                   readc     FMTS1                                  21
038100020705  "   *
038200020705  "   * CICLO fino a quando esistono rec del sfl da caricare
038300020705  "   *       o la ds multipla è completa
038400020705  "  C                   dow       *In21 = *Off        and
038500020705  "  C                             $iF13DS  <= $DimF13DS
038600020705  "   *
038700020705  "   *
038800020705  "   *  riempio la ds multipla
038900020705  "  C     $iF13DS       occur     F13DS
039000020705  "   *
039100020705  "  C                   clear                   F13DS
039200020705  "  C                   movel     S1PRF         F13PRF
039300020705  "  C                   movel     S1UFF         F13UFF
039400020705  "  C                   movel     S1GRP         F13GRP
039500020705  "  C                   movel     S1CUT         F13CUT
039600020705  "  C                   movel     S1LIN         F13LIN
039700020705  "   *
039800020705  "   *  nr prossimo elemento da leggere dalla ds multipla
039900020705  "  C                   eval      $iF13DS = $iF13DS + 1
040000020705  "   *
040100020705  "   *  nr elementi caricati nella ds multipla
040200020705  "  C                   eval      NElemF13DS = NElemF13DS + 1
040300020705  "   *
040400020705  "  C                   readc     FMTS1                                  21
040500020705  "   *
040600020705  "  C                   enddo
040700020705  "   *
040800020705  "   *
040900020705  "   * se nel sfl sono rimasti rec con opz.1 scelti non caricati
041000020705  "   * nella ds perchè già caricata al max
041100020705  "  C                   if        *In21 = *Off
041200020705  "   *
041300020705  "   *  riaggiorno l'ultima riga sfl letta con SflNxtChg attivo
041400020705  "   *  così quando il chiamante mi 'richiama', sarà il I°
041500020705  "   *  rec letto
041600020705  "  C                   exsr      AtrS1
041700020705  "  C                   setOn                                        32
041800020705  "  C                   update    FmtS1
041900020705  "  C                   setoff                                       32
042000020705  "   *
042100020705  "   *  window di avvertimento se sono stati scelti + rec
042200020705  "   *  di quanti la ds multipla ne possa contenere
042300020705  "   *
042400020705  "  C                   endif
042500020705  "   *
042600020705B0453C     XRieF13DS     Endsr
042700941110      ************************************************************
042800941110      * RICERCHE
042900941110      ************************************************************
043000941110     C     SEARCH        BEGSR
043100941110      *
043200941110      * determino Riga/Colonna del cursore
043300941110     C                   MOVE      NRG           R$$
043400941110     C                   MOVE      NCL           C$$
043500941110     C                   Z-ADD     RIRI          CURR
043600941110     C                   Z-ADD     COCO          CURC
043700941110      *
043800941110     C                   SELECT
043900941110      *
044000941110    > *--------------------------------------------------------------
044100941110    > * inserire le routine di ricerca
044200941110    > *--------------------------------------------------------------
044300941110      * FMTD1  - MODULO
044400941110    >C     H1NMFL        WHENEQ    'D1GRP'
044500941110     C                   MOVE      *ZERO         WWGRP
044600941110     C                   MOVE      WWGRP         KEYTAB
044700941110     C                   MOVE      '01'          OPZTAB
044800941110     C                   MOVE      XSCSOC        AZITAB
044900941110     C                   MOVE      XSCLIN        LINTAB
045000941110     C                   CLEAR                   KPJBU
045100941110     C                   MOVEL     XTABDS        KPJBU
045200941110     C                   CALL      'ANGG22R'
045300941110     C                   PARM                    KPJBA
045400941110     C                   MOVEL     KPJBU         XTABDS
045500941110     C                   MOVE      KEYTAB        WWGRP
045600941110      *
045700941110    >C     WWGRP         IFNE      *ZEROS
045800941110    >C                   MOVEL     WWGRP         D1GRP
045900941110    >C                   ENDIF
046000941110      * imposto pos. del cursore
046100941110     C                   Z-ADD     CURR          H1RIGA
046200941110     C                   Z-ADD     CURC          H1COLO
046300150909>>>>>** Ricerca e scelta codice ufficio.
046301150909>>>>>
046302150909>>>>>C     H1NMFL        WHENEQ    'D1UFF'
046303150909>>>>>C                   RESET                   xTabDs
046304150909>>>>>C                   MOVE      *BLANK        WWUFF
046305150909>>>>>C                   MOVE      WWUFF         KEYTAB
046306150909>>>>>C                   MOVE      '01'          OPZTAB
046307150909>>>>>C                   MOVE      XSCSOC        AZITAB
046308150909>>>>>C                   MOVE      XSCLIN        LINTAB
046309150909>>>>>C                   CLEAR                   KPJBU
046310150909>>>>>C                   MOVEL     XTABDS        KPJBU
046311150909>>>>>C                   CALL      'ANGG01R'
046312150909>>>>>C                   PARM                    KPJBA
046313150909>>>>>C                   MOVEL     KPJBU         XTABDS
046314150909>>>>>C                   MOVE      KEYTAB        WWUFF
046315150909>>>>> *
0463161509092>>>>C     WWUFF         IFNE      *BLANK
046317150909>>>>>C                   MOVEL     WWUFF         D1UFF
0463181509092->>>C                   ENDIF
046319150909>>>>> * imposto pos. del cursore
046320150909>>>>>C                   Z-ADD     CURR          H1RIGA
046321150909>>>>>C                   Z-ADD     CURC          H1COLO
046400941110      * ricerca non abilitata in questa posizione
046500941110     C                   OTHER
046600941110      * imposto pos. del cursore
046700941110     C                   Z-ADD     CURR          H1RIGA
046800941110     C                   Z-ADD     CURC          H1COLO
046900941110     C                   SETON                                        9899
047000941110      *
047100941110     C                   ENDSL
047200941110      *
047300941110     C                   MOVE      *HIVAL        $LASTV
047400941110     C                   ENDSR
047500941110     C/EJECT
047600941110      ************************************************************
047700941110      * RIEMPIMENTO OPZIONI
047800941110      ************************************************************
047900941110     C     RIEOPZ        BEGSR
048000941110      *
048100941110      * Input :
048200941110      * - $TX = schiera con tutti i testi disponibili
048300941110      * - $FL = schiere con i flag di validità associati ai testi
048400941110      * - WCOZ = variabile contenente l'indice dell'ultimo testo
048500941110      *          opzione usato
048600941110      * - WMXO = variabile contenente il numero massimo di testi
048700941110      *          opzione usabili
048800941110      * - $L01 = Lunghezza prima riga
048900941110      * - $L02 = Lunghezza seconda riga
049000941110      * Output :
049100941110      * - WOPZ1 = variabile contenente la prima riga opzioni
049200941110      * - WOPZ2 = variabile contenente la seconda riga opzioni
049300950403     C* - FF23  = flag che indica se c'è bisogno del tasto F23
049400941110      *
049500950403     C                   CALL      'X04OPMSG'
049600941110      * input
049700941110     C                   PARM                    $TX
049800941110     C                   PARM                    $FL
049900941110     C                   PARM                    WCOZ              3 0
050000941110     C                   PARM                    WMXO              3 0
050100941110      * output
050200941110     C                   PARM                    WOPZ1            76
050300941110     C                   PARM                    WOPZ2            76
050400950403     C                   PARM                    FF23              1
050500941110    > * input
050600941110    >C                   PARM                    $L01              3 0
050700941110    >C                   PARM                    $L02              3 0
050800941110      *
050900060224A1148C                   PARM                    $ALLOPT         960
051000060224A1148C*
051100941110     C                   ENDSR
051200941110     C/EJECT
051300941110      ************************************************************
051400941110      * RIEMPIMENTO TASTI FUNZIONE
051500941110      ************************************************************
051600941110     C     RIEKEY        BEGSR
051700941110      *
051800941110      * Input :
051900941110      * - $FX = schiera con tutti i testi disponibili
052000941110      * - $FC = schiere con i flag di validità associati ai testi
052100941110      * - $FR = schiere con i flag di REVERSE IMAGE
052200941110      * - $ULK = ultimo testo caricato
052300941110      * - $L01 = Lunghezza prima riga
052400941110      * - $L02 = Lunghezza seconda riga
052500941110      * Output :
052600941110      * - $ULK
052700941110      * - $KEY1= 1° riga di tasti funzione
052800941110      * - $KEY2= 2° riga di tasti funzione
052900941110      *
053000941110     C                   CALL      'XKEYMSG'
053100941110      * input
053200941110     C                   PARM                    $FX
053300941110     C                   PARM                    $FC
053400941110     C                   PARM                    $FR
053500941110     C                   PARM                    $ULK              3 0
053600941110      * output
053700941110     C                   PARM                    $KEY1            79
053800941110     C                   PARM                    $KEY2            79
053900941110    > * input
054000941110    >C                   PARM                    $L01              3 0
054100941110    >C                   PARM                    $L02              3 0
054200941110      *
054300060224A1148C                   PARM                    $ALLFUNCT       480
054400060224A1148C*
054500941110     C                   ENDSR
054600941110     C/EJECT
054700941110      ************************************************************
054800941110      * GESTIONE EMISSIONE TASTI FUNZIONE
054900941110      ************************************************************
055000941110     C     GESPIE        BEGSR
055100941110      *
055200941110     C                   SELECT
055300941110      * Parzializzazioni
055400941110     C     $GEST         WHENEQ    'D1'
055500941110     C     $ULKD1        IFEQ      0
055600941110      * riempimento opzioni
055700941110     C                   EXSR      F24D1
055800941110     C                   ELSE
055900941110     C                   WRITE     FMTZ1
056000941110     C                   ENDIF
056100941110      * Lista
056200941110     C     $GEST         WHENEQ    'S1'
056300941110     C     $ULKS1        IFEQ      0
056400941110     C                   EXSR      F24S1
056500941110     C                   ELSE
056600941110     C                   WRITE     FMTZ2
056700941110     C                   ENDIF
056800941110      * Modifica valori assunti
056900941110     C     $GEST         WHENEQ    'D2'
057000941110     C     $ULKD2        IFEQ      0
057100941110     C                   EXSR      F24D2
057200941110     C                   ELSE
057300941110     C                   WRITE     FMTZ3
057400941110     C                   ENDIF
057500941110     C                   ENDSL
057600941110      *
057700941110     C                   ENDSR
057800941110     C/EJECT
057900941110      ************************************************************
058000941110      * GESTIONE PARZIALIZZAZIONI
058100941110      ************************************************************
058200941110     C     GESD1         BEGSR
058300941110      * inizializzazione videata
058400941110     C     $INZD1        IFEQ      *ON
058500941110     C                   EXSR      INZD1
058600941110     C                   MOVE      *OFF          $INZD1
058700941110     C                   END
058800941110      * emissione piede videata se proveniente da altra
058900941110      * salvataggio valore formato di provenienza
059000941110     C     $LASTV        IFNE      'D1'
059100941110      * salvo stato F17 per sapere cosa fare in caso di F12
059200941110     C                   MOVE      *INKR         WINKR
059300941110     C                   WRITE     FMTT1
059400941110     C                   EXSR      GESPIE
059500941110     C                   MOVE      $LASTV        $LASTG
059600941110     C                   MOVE      'D1'          $LASTV
059700941110     C                   ENDIF
059800941110      *
059900941110      * se esistono errori sulla videata
060000941110      * emetto la write del formato a indicatori spenti per vedere
060100941110      * le eventuali decodifiche
060200941110     C     *IN99         IFEQ      *ON
060300941110     C                   MOVEA     *IN           WIN
060400941110     C                   MOVE      *ALL'0'       IN5098           49
060500941110     C                   MOVEA     IN5098        *IN(50)
060600941110     C                   WRITE     FMTD1
060700941110     C                   MOVEA     WIN           *IN
060800941110     C                   ENDIF
060900941110      *
061000941110     C                   EXFMT     FMTD1
061100941110      * controllo abilitazione tasti funzione
061200941110      *
061300941110     C                   EXSR      CTRKD1
061400941110      *
061500941110     C     *IN99         IFEQ      *OFF
061600941110      *
061700941110     C                   SELECT
061800941110      * F2=Decodifica
061900941110     C     $TASTO        WHENEQ    F02
062000941110     C                   EXSR      F02D1
062100941110      * F3=Fine
062200941110     C     $TASTO        WHENEQ    F03
062300941110     C                   EXSR      F03D1
062400941110      * F4=Ricerca
062500941110     C     $TASTO        WHENEQ    F04
062600941110     C                   EXSR      SEARCH
062700941110      * F5=Refresh
062800941110     C     $TASTO        WHENEQ    F05
062900941110     C                   EXSR      F05D1
063000941110      * F12=Ritorno
063100941110     C     $TASTO        WHENEQ    F12
063200941110     C                   EXSR      F12D1
063300941110      * F18=Modifica valori assunti
063400941110     C     $TASTO        WHENEQ    F18
063500941110     C                   EXSR      F18D1
063600941110      * F24=Altri tasti
063700941110     C     $TASTO        WHENEQ    F24
063800941110     C                   EXSR      F24D1
063900941110      *
064000941110     C                   OTHER
064100941110     C                   EXSR      CTRD1
064200941110     C     *IN99         IFEQ      *OFF
064300941110      * definisco l'ordinamento
064400941110     C                   SELECT
064500941110    > * << nel caso di più viste logiche e quindi più campi di
064600941110    > *    parzializzazione, qui vanno definiti i criteri per
064700941110    > *    determinare quale VL va letta, a seconda di quali campi
064800941110    > *    sono stati valorizzati >>
064900941110    >C     D1PRF         WHENNE    *BLANK
065000941110     C                   Z-ADD     1             $ORD
065100941110     C                   SETOFF                                       02
065200941110     C                   SETON                                        01
065300941110    >C     D1GRP         WHENNE    *ZEROS
065400941110     C                   Z-ADD     2             $ORD
065500941110     C                   SETON                                        02
065600941110     C                   SETOFF                                       01
065700941110    > *          D1PRG     WHNE 0
065800941110      *                    Z-ADD99        $ORD
065900941110      * nel caso non sia stato digitato nulla uso l'ordinamento
066000941110      * dei valori assunti
066100941110     C                   ENDSL
066200941110     C                   MOVE      'S1'          $GEST
066300941110     C                   MOVE      *ON           $INZS1
066400941110     C                   MOVE      *BLANK        W1POS
066500941124     C                   Z-ADD     0             W1POS2
066600941110     C                   ENDIF
066700941110      *
066800941110     C                   ENDSL
066900941110      *
067000941110     C                   ENDIF
067100941110     C                   ENDSR
067200941110     C/EJECT
067300941110      ************************************************************
067400941110      * INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
067500941110      ************************************************************
067600941110     C     INZD1         BEGSR
067700941110      *
067800941110     C                   CLEAR                   FMTD1
067801150908>>>>>C                   EVAL      d1uff = '*'                                  Tutto
067900941110      * Se pgm non richiamato
068000941110     C     OPZ001        IFEQ      *BLANK
068100941110    > *                    Z-ADDUDATE     D1DRG
068200941110     C                   ELSE
068300941110      * Se pgm richiamato
068400941110     C                   SELECT
068500941110    >C     PRF001        WHENNE    *BLANK
068600941110    >C                   MOVEL     PRF001        D1PRF
068700941110    >C                   OTHER
068800941110    > ******               Z-ADDUDATE     D1DRG
068900941110     C                   ENDSL
069000941110     C                   END
069100941110      *
069200941110     C                   ENDSR
069300941110     C/EJECT
069400941110      ************************************************************
069500941110      * CONTROLLO ABILITAZIONE TASTI VIDEATA D1
069600941110      * INPUT
069700941110      * $FC1 = Abilitazioni videata D!
069800941110      * OUTPUT
069900941110      * *IN97 *IN99 Errori
070000941110      ************************************************************
070100941110     C     CTRKD1        BEGSR
070200941110     C                   SETOFF                                       99
070300941110     C                   MOVEA     $FC1          $FC
070400941110     C                   EXSR      CTRKEY
070500941110     C     *IN99         IFEQ      *ON
070600941110     C                   SETON                                        97
070700941110     C                   ENDIF
070800941110     C                   ENDSR
070900941110     C/EJECT
071000941110      ************************************************************
071100941110      * CONTROLLO ABILITAZIONE TASTI VIDEATA S1
071200941110      ************************************************************
071300941110     C     CTRKS1        BEGSR
071400941110     C                   SETOFF                                       99
071500941110     C                   MOVEA     $FC2          $FC
071600941110     C                   EXSR      CTRKEY
071700941110     C     *IN99         IFEQ      *ON
071800941110     C                   SETON                                        97
071900941110     C                   ENDIF
072000941110     C                   ENDSR
072100941110     C/EJECT
072200941110      ************************************************************
072300941110      * CONTROLLO ABILITAZIONE TASTI
072400941110      * INPUT
072500941110      * $TASTO =Tasto funzione premuto
072600941110      * $FC = Tasti abilitati
072700941110      * OUTPUT
072800941110      * IN99 = ERrore
072900941110      ************************************************************
073000941110     C     CTRKEY        BEGSR
073100941110     C                   SELECT
073200941110     C     $TASTO        WHENEQ    F01
073300941110     C     $FC(1)        IFEQ      '0'
073400941110     C                   SETON                                        99
073500941110     C                   ENDIF
073600941110     C     $TASTO        WHENEQ    F02
073700941110     C     $FC(2)        IFEQ      '0'
073800941110     C                   SETON                                        99
073900941110     C                   ENDIF
074000941110     C     $TASTO        WHENEQ    F03
074100941110     C     $FC(3)        IFEQ      '0'
074200941110     C                   SETON                                        99
074300941110     C                   ENDIF
074400941110     C     $TASTO        WHENEQ    F04
074500941110     C     $FC(4)        IFEQ      '0'
074600941110     C                   SETON                                        99
074700941110     C                   ENDIF
074800941110     C     $TASTO        WHENEQ    F05
074900941110     C     $FC(5)        IFEQ      '0'
075000941110     C                   SETON                                        99
075100941110     C                   ENDIF
075200941110     C     $TASTO        WHENEQ    F06
075300941110     C     $FC(6)        IFEQ      '0'
075400941110     C                   SETON                                        99
075500941110     C                   ENDIF
075600941110     C     $TASTO        WHENEQ    F07
075700941110     C     $FC(7)        IFEQ      '0'
075800941110     C                   SETON                                        99
075900941110     C                   ENDIF
076000941110     C     $TASTO        WHENEQ    F08
076100941110     C     $FC(8)        IFEQ      '0'
076200941110     C                   SETON                                        99
076300941110     C                   ENDIF
076400941110     C     $TASTO        WHENEQ    F09
076500941110     C     $FC(09)       IFEQ      '0'
076600941110     C                   SETON                                        99
076700941110     C                   ENDIF
076800941110     C     $TASTO        WHENEQ    F10
076900941110     C     $FC(10)       IFEQ      '0'
077000941110     C                   SETON                                        99
077100941110     C                   ENDIF
077200941110     C     $TASTO        WHENEQ    F11
077300941110     C     $FC(11)       IFEQ      '0'
077400941110     C                   SETON                                        99
077500941110     C                   ENDIF
077600941110     C     $TASTO        WHENEQ    F12
077700941110     C     $FC(12)       IFEQ      '0'
077800941110     C                   SETON                                        99
077900941110     C                   ENDIF
078000941110     C     $TASTO        WHENEQ    F13
078100941110     C     $FC(13)       IFEQ      '0'
078200941110     C                   SETON                                        99
078300941110     C                   ENDIF
078400941110     C     $TASTO        WHENEQ    F14
078500941110     C     $FC(14)       IFEQ      '0'
078600941110     C                   SETON                                        99
078700941110     C                   ENDIF
078800941110     C     $TASTO        WHENEQ    F15
078900941110     C     $FC(15)       IFEQ      '0'
079000941110     C                   SETON                                        99
079100941110     C                   ENDIF
079200941110     C     $TASTO        WHENEQ    F16
079300941110     C     $FC(16)       IFEQ      '0'
079400941110     C                   SETON                                        99
079500941110     C                   ENDIF
079600941110     C     $TASTO        WHENEQ    F17
079700941110     C     $FC(17)       IFEQ      '0'
079800941110     C                   SETON                                        99
079900941110     C                   ENDIF
080000941110     C     $TASTO        WHENEQ    F18
080100941110     C     $FC(18)       IFEQ      '0'
080200941110     C                   SETON                                        99
080300941110     C                   ENDIF
080400941110     C     $TASTO        WHENEQ    F19
080500941110     C     $FC(19)       IFEQ      '0'
080600941110     C                   SETON                                        99
080700941110     C                   ENDIF
080800941110     C     $TASTO        WHENEQ    F20
080900941110     C     $FC(20)       IFEQ      '0'
081000941110     C                   SETON                                        99
081100941110     C                   ENDIF
081200941110     C     $TASTO        WHENEQ    F21
081300941110     C     $FC(21)       IFEQ      '0'
081400941110     C                   SETON                                        99
081500941110     C                   ENDIF
081600941110     C     $TASTO        WHENEQ    F22
081700941110     C     $FC(22)       IFEQ      '0'
081800941110     C                   SETON                                        99
081900941110     C                   ENDIF
082000941110     C     $TASTO        WHENEQ    F23
082100941110     C     $FC(23)       IFEQ      '0'
082200941110     C                   SETON                                        99
082300941110     C                   ENDIF
082400941110     C     $TASTO        WHENEQ    F24
082500941110     C     $FC(24)       IFEQ      '0'
082600941110     C                   SETON                                        99
082700941110     C                   ENDIF
082800941110     C                   ENDSL
082900941110     C                   ENDSR
083000941110     C/EJECT
083100941110      ************************************************************
083200941110      * GESTIONE F02 VIDEO D1
083300941110      ************************************************************
083400941110     C     F02D1         BEGSR
083500941110      *
083600941110      * per decodificare la videata richiamo la routine dei ctrl
083700941110      * e poi disattivo gli eventuali messaggi di errore
083800941110     C                   EXSR      CTRD1
083900941110      *
084000941110     C                   MOVE      *ALL'0'       IN5098
084100941110     C                   MOVEA     IN5098        *IN(50)
084200941110      *
084300941110     C                   ENDSR
084400941110     C/EJECT
084500941110      ************************************************************
084600941110      * GESTIONE F03 VIDEO D1
084700941110      ************************************************************
084800941110     C     F03D1         BEGSR
084900941110      *
085000941110     C                   MOVE      *ON           $FINE
085100941110      * fine programma
085200941110     C                   EXSR      ENDPGM
085300941110      *
085400941110     C                   ENDSR
085500941110     C/EJECT
085600941110      ************************************************************
085700941110      * GESTIONE F05 VIDEO D1
085800941110      ************************************************************
085900941110     C     F05D1         BEGSR
086000941110      *
086100941110     C                   MOVE      *ON           $INZD1
086200941110      *
086300941110     C                   ENDSR
086400941110     C/EJECT
086500941110      ************************************************************
086600941110      * GESTIONE F12 VIDEO D1
086700941110      ************************************************************
086800941110     C     F12D1         BEGSR
086900941110      *
087000941110      *  se la 1a videata è la lista oppure sono arrivato qui con F17,
087100941110      *  per F12 torno alla lista
087200941110     C     $LOP          IFEQ      2
087300941110     C     WINKR         OREQ      *ON
087400941110     C                   MOVE      'S1'          $GEST
087500941110      *  altrimenti, per F12 torno a menu
087600941110     C                   ELSE
087700941110     C                   MOVE      *ON           $FINE
087800941110     C                   END
087900941110      *
088000941110     C                   ENDSR
088100941110     C/EJECT
088200941110      ************************************************************
088300941110      * GESTIONE F18 VIDEO D1
088400941110      ************************************************************
088500941110     C     F18D1         BEGSR
088600941110      *
088700941110     C                   MOVE      'D2'          $GEST
088800941110     C                   MOVEL     *ON           $INZD2
088900941110      *
089000941110     C                   ENDSR
089100941110     C/EJECT
089200941110      ************************************************************
089300941110      * GESTIONE F24 VIDEO D1
089400941110      * input
089500941110      * $ULKD1 = Ultimo tasto funzione emesso per video D1
089600941110      * $FX1 =Testi tasti funzione
089700941110      * $FC1 = Abilitazioni
089800941110      * $FR1 = Inversioni di fondo
089900941110      ************************************************************
090000941110     C     F24D1         BEGSR
090100941110      *
090200941110     C                   MOVEA     $FX1          $FX
090300941110     C                   MOVEA     $FC1          $FC
090400941110     C                   MOVEA     $FR1          $FR
090500941110     C                   Z-ADD     $ULKD1        $ULK
090600941110     C                   Z-ADD     79            $L01
090700941110     C                   Z-ADD     79            $L02
090800941110     C                   EXSR      RIEKEY
090900941110     C                   Z-ADD     $ULK          $ULKD1            3 0
091000941110     C                   MOVEA     $FC           $FC1
091100941110     C                   MOVEL     $KEY1         Z1KE1
091200060224A1148C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
091300941110     C                   MOVEL     $KEY2         Z1KE2
091400941110     C                   WRITE     FMTZ1
091500941110      *
091600941110     C                   ENDSR
091700941110     C/EJECT
091800941110      ************************************************************
091900941110      * CONTROLLO VIDEATA PARZIALIZZAZIONI
092000941110      ************************************************************
092100941110     C     CTRD1         BEGSR
092200941110      *
092300941110     C                   SETOFF                                       99
092400941110      * non imposto pos. del cursore
092500941110     C                   Z-ADD     0             H1RIGA
092600941110     C                   Z-ADD     0             H1COLO
092700941110      *
092800941110    > * << elencare gli eventuali controlli sui campi di
092900941110    > *    parzializzazione, ricordando che quasi sempre essi
093000941110    > *    non ne avranno perchè determinano solo il range di
093100941110    > *    scrittura della lista e quindi (nel caso di campi
093200941110    > *    alfanumerici) saranno trattati come chiavi parziali >>
093300941110      *
093400941110     C                   ENDSR
093500941110     C/EJECT
093600941110      ************************************************************
093700941216      * GESTIONE LISTA
093800941110      ************************************************************
093900941110     C     GESS1         BEGSR
094000941110      * inizializzazione videata
094100941110     C     $INZS1        IFEQ      *ON
094200941110     C                   EXSR      INZS1
094300941110     C                   MOVE      *OFF          $INZS1
094400941110     C                   ENDIF
094500941110      * emissione piede videata se proveniente da altra
094600941110      * salvataggio valore formato di provenienza
094700941110     C     $LASTV        IFNE      'S1'
094800941110     C                   WRITE     FMTT1
094900941110     C                   EXSR      GESPIE
095000941110     C                   MOVE      $LASTV        $LASTG
095100941110     C                   MOVE      'S1'          $LASTV
095200941110     C                   END
095300941110      *
095400941110     C     WMAX          IFEQ      0
095500941110     C                   WRITE     FMTD3                                        NO RCD
095600941110     C                   ENDIF
095700941110      *Serve per emissione finestra pgm invio ang727r (per open/close
095800941110      *window)
095900941110     C                   MOVE      '1'           WIN727            1
096000941110      *
096100941110     C                   EXFMT     FMTC1
096200941110     C     C1NRR         IFNE      0
096300941110     C                   Z-ADD     C1NRR         WSFL
096400941110     C                   ENDIF
096500941110     C                   Z-ADD     SFLNRR        C1RCD
096600941110      *
096700941110      * controllo abilitazione tasti funzione
096800941110      *
096900941110     C                   EXSR      CTRKS1
097000941110     C     *IN99         IFEQ      *OFF
097100941110      *
097200941110      *
097300941110     C                   SELECT
097400941110      * F3=Fine
097500941110     C     $TASTO        WHENEQ    F03
097600941110     C                   EXSR      F03S1
097700941110      * F10=Immissione
097800941110     C     $TASTO        WHENEQ    F10
097900941110     C                   EXSR      F10S1
098000941110      * F12=Ritorno
098100941110     C     $TASTO        WHENEQ    F12
098200941110     C                   EXSR      F12S1
098300020705B0453 * F13=Selezione multipla                                                  f13
098400020705  "  C     $TASTO        WHENEQ    F13                                          f13
098500020705B0453C                   EXSR      F13S1                                        f13
098600941110      * F17=Parzializzazioni
098700941110     C     $TASTO        WHENEQ    F17
098800941110     C                   EXSR      F17S1
098900941110      * F18=Modifica valori assunti
099000941110     C     $TASTO        WHENEQ    F18
099100941110     C                   EXSR      F18S1
099200941110      * F23=Altre opzioni
099300941110     C     $TASTO        WHENEQ    F23
099400941110     C                   EXSR      F23S1
099500941110      * F24=Altri tasti
099600941110     C     $TASTO        WHENEQ    F24
099700941110     C                   EXSR      F24S1
099800941110      * ROLL-UP
099900941110     C     $TASTO        WHENEQ    ROLLUP
100000941110     C                   EXSR      ROLS1
100100941110      *
100200941110     C                   OTHER
100300941110      * CONTROLLO DATI
100400941110     C                   EXSR      CTRC1
100500941110     C     *IN99         IFEQ      *OFF
100600941110     C                   EXSR      CTRS1
100700941110     C                   END
100800941110     C                   ENDSL
100900941110      *
101000941110     C                   ENDIF
101100941110     C                   ENDSR
101200941110     C/EJECT
101300941110      ************************************************************
101400941110      * INIZIALIZZAZIONE LISTA
101500941110      ************************************************************
101600941110     C     INZS1         BEGSR
101700941110      * pulizia SFL
101800941110     C                   SETOFF                                         3031
101900941110     C                   WRITE     FMTC1
102000941110     C                   SETON                                          31
102100030423A0997X*** ex-A0709
102200030423A0997X*** ex-A0709 visualizza o nasconde la colonna utenti Web
102300020617A0948X***                if        jbusin='S'
102400030423A0997X*** ex-A0948       if        jbusin<>'N'
102500030423  "  X*** ex-A0709       setoff                                           03
102600030423  "  X***   "            else
102700030423  "  X***   "            seton                                            03
102800030423  "  X***   "            endif
102900030423A0997X*** ex-A0709
103000020326A0910C                   SETOFF                                       04
103100020326  "  C                   IF        xscLaguna = *on
103200020326  "  C                   SETON                                        04
103300020326A0910C                   ENDIF
103400941110      *
103500941110      * riempimento opzioni
103600941110     C                   MOVEA     $TX1          $TX
103700941110     C                   MOVEA     $FL1          $FL
103800941110     C                   Z-ADD     0             WCOZ
103900941110      *  equivale al num.di elementi della schiera $TX1
104000991123     C                   Z-ADD     $DIMOP        WMXO
104100991117     C                   Z-ADD     76            $L01
104200991117     C                   Z-ADD     76            $L02
104300941110     C                   EXSR      RIEOPZ
104400950403     C                   Z-ADD     23            X
104500950403     C                   MOVE      FF23          $FC2(X)
104600941110     C                   MOVEL     WOPZ1         C1OPZ1
104700060224A1148C                   MOVEL     $ALLOPT       H1ALLOPT
104800941110     C                   MOVEL     WOPZ2         C1OPZ2
104900941110     C                   Z-ADD     WCOZ          C1COZ
105000941110      *
105100941110      * CARICAMENTO SFL A PAGINE
105200941110      *
105300941110     C                   Z-ADD     0             S1NRR
105400941110     C                   Z-ADD     0             WMAX
105500941110      *
105600941110      * Posizionamento su file pilota
105700941110     C                   CLEAR                   C1POS2
105800941110     C                   CLEAR                   C1POS
105900941110      *
106000941110    >C                   EXSR      SETUTN
106100941110      * ripristino i valori del posizionamento
106200941110     C     $ORD          IFEQ      2
106300941110     C                   Z-ADD     W1POS2        C1POS2
106400941110     C                   ELSE
106500941110     C                   MOVE      W1POS         C1POS
106600941110     C                   ENDIF
106700941110      *
106800941110      * Lettura file pilota
106900941110    >C                   EXSR      REDUTN
107000941110      *
107100941110      * Carico 1a pagina
107200941110     C                   EXSR      ROLS1
107300941110     C                   Z-ADD     SFLPAG        WPAG
107400941110      *
107500941110     C                   ENDSR
107600941110     C/EJECT
107700941110      ************************************************************
107800941110      * CARICAMENTO PAGINA LISTA
107900941110      ************************************************************
108000941110     C     ROLS1         BEGSR
108100941110      *
108200941110     C                   SETOFF                                       32
108300941110     C                   Z-ADD     0             Y
108400941110     C                   Z-ADD     WMAX          S1NRR
108500941110      *
108600941110      * Leggo dal file anagrafico per caricare la lista
108700941110      *
108800941110     C     $EFILE        DOWEQ     *OFF
108900941110     C     Y             ANDLT     WPAG
109000941110      *
109100941110     C                   EXSR      RIES1
109200941110      *
109300941110     C                   ADD       1             S1NRR
109400941110     C                   ADD       1             Y
109500941110      *
109600941110     C                   WRITE     FMTS1
109700941110      *
109800941110    >C                   EXSR      REDUTN
109900941110      *
110000941110     C                   ENDDO
110100941110      *
110200941110     C                   Z-ADD     S1NRR         WMAX                 30
110300941110      *
110400941110      * POSIZIONAMENTO AL 1° RCD DELLA PAGINA
110500941110      *
110600941110     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
110700941110     C                   MVR                     RESTO             3 0
110800941110     C     PAGINE        MULT      SFLPAG        C1RCD
110900941110     C     RESTO         IFGT      0
111000941110     C                   ADD       1             C1RCD
111100941110     C                   ELSE
111200941110     C                   SUB       SFLPAG        C1RCD
111300941110     C                   ADD       1             C1RCD
111400941110     C                   ENDIF
111500941110      *
111600941110     C                   ENDSR
111700941110     C/EJECT
111800941110      ************************************************************
111900941110      * POSIZIONAMENTO RCD ARCHIVIO PILOTA
112000941110      ************************************************************
112100941110    >C     SETUTN        BEGSR
112200941110      *
112300941110     C                   SELECT
112400941110    > * Ord.1 = Profilo
112500941110     C     $ORD          WHENEQ    1
112600941110    > *-----------------------------------------------------------
112700941110    > * Gestione posizionamento
112800941110    > * Se il campo W1POS è > *blank imposto il posizionamento con
112900941110    > * W1POS altrimenti lo imposto con il valore di parametrizzazione
113000941110    > *-----------------------------------------------------------
113100941110     C     W1POS         IFNE      *BLANK
113200941110    >C                   MOVEL     W1POS         UTNPRF
113300941110     C                   ELSE
113400941110    >C                   MOVEL     D1PRF         UTNPRF
113500941110     C                   END
113600941110    >C     K1UTN1        SETLL     ANUTN01
113700941110      * Ord.2 = Gruppo
113800941110     C     $ORD          WHENEQ    2
113900941124     C     W1POS2        IFNE      0
114000941110    >C                   MOVEL     W1POS2        UTNGRP
114100941110     C                   ELSE
114200941110    >C                   MOVEL     D1GRP         UTNGRP
114300941110     C                   END
114400941110    >C     K3UTN1        SETLL     ANUTN03
114500941110     C                   ENDSL
114600941110      *
114700941110     C                   ENDSR
114800941110     C/EJECT
114900941110      ************************************************************
115000941110      * LETTURA RCD ARCHIVIO PILOTA
115100941110      ************************************************************
115200941110    >C     REDUTN        BEGSR
115300941110      *
115400941110     C                   MOVEL     *OFF          $EFILE
115500941110     C                   MOVEL     *OFF          $RCDOK
115600941110      *
115700941110     C     $EFILE        DOUEQ     *ON
115800941110     C     $RCDOK        OREQ      *ON
115900941110      *
116000941110     C                   SELECT
116100941110     C     $ORD          WHENEQ    1
116200941110    >C                   READ      ANUTN01                                21
116300941110     C     $ORD          WHENEQ    2
116400941110    >C                   READ      ANUTN03                                21
116500941110     C                   ENDSL
116600941110      *
116700941110     C                   MOVE      *IN21         $EFILE
116800941110      *
116900941110     C     $EFILE        IFEQ      *OFF
117000941110     C                   EXSR      SELUTN
117100941110     C                   ENDIF
117200941110      *
117300941110     C                   ENDDO
117400941110      *
117500941110     C                   MOVE      *IN21         *IN33                          SflEnd
117600941110      *
117700941110     C                   ENDSR
117800941110     C/EJECT
117900941110      ************************************************************
118000941110      * CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
118100941110      ************************************************************
118200941110     C     CLCPAG        BEGSR
118300941110      * Input :
118400941110      * - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
118500941110      *          cursore
118600941110      * - SFLPAG = numero rcd per pagina sfl
118700941110      * Output :
118800941110      * - WPAG = pagina fino a cui deve essere ricaricato il sfl
118900941110      *
119000941110     C     WSFL          DIV       SFLPAG        PAGINE            4 0
119100941110     C                   MVR                     RESTO             3 0
119200941110     C     RESTO         IFGT      0
119300941110     C                   ADD       1             PAGINE
119400941110     C                   ENDIF
119500941110     C     PAGINE        MULT      SFLPAG        WPAG
119600941110      *
119700941110     C                   ENDSR
119800941110     C/EJECT
119900941110      ************************************************************
120000941110      * SELEZIONI SULLA RIGA LETTA
120100941110      * impostare a *OFF il campo $RCDOK per scartare un record che
120200941110      * non rientra nei parametri di parzializzazione
120300941110      * Eseguire una *HIVAL SETLL al raggiungimento di un limite
120400941110      * superiore stabilito nei parametri
120500941110      *
120600941110      ************************************************************
120700941110    >C     SELUTN        BEGSR
120800941110      *
120900941110     C                   MOVEL     *ON           $RCDOK
121000941110A1211X*                  MOVEL     *ALL'9'       ALL9             15
121100941110A1211C                   MOVEL     *hival        ALL9             15
121200941110      * creo limite massimo parzializzazioni
121300941110    > * << per ogni valore alfanumerico creo il limite superiore
121400941110    > *    per cui caricare, creando una variabile che contiene
121500941110    > *    il valore digitato nel campo a video riempito di "9" >>
121600941110    >C     D1PRF         CAT       ALL9:0        PRF999
121700941110      *
121800941110     C                   SELECT
121900941110      *
122000941110      * SELEZIONI RISPETTO AI VALORI RCD
122100941110      *
122200941110    > * << elaborare eventuali selezioni che non riguardano le
122300941110    > *    parzializzazioni ma più propriamente i dati del rcd ;
122400941110    > *    esempio : esclusione degli annullati. >>
122500941110      *
122600941110      * SELEZIONI RISPETTO ALLE PARZIALIZZAZIONI
122700941110      *
122800941110      * oggetto non compreso nella scelta
122900941110     C     UTNPRF        WHENLT    D1PRF
123000941110     C     UTNPRF        ORGT      PRF999
123100941110     C                   MOVEL     *OFF          $RCDOK
123200941110     C     UTNPRF        IFGT      PRF999
123300941110     C     $ORD          ANDEQ     1
123400941110     C     *HIVAL        SETLL     ANUTN01
123500941110     C                   ENDIF
123600941110      *
123700941110      * modulo non compreso nella scelta
123800941110     C     UTNGRP        WHENNE    D1GRP
123900941110     C     D1GRP         ANDNE     0
124000941110     C                   MOVEL     *OFF          $RCDOK
124100080303B1033 *
124200080303  "   * evitare gli annullati
124300080303  "  C     UTNAnn        WHENEq    *on
124400080303B1033C                   MOVEL     *OFF          $RCDOK
124500941110      *
124600941110     C                   ENDSL
124700150908>>>>>** Filtro ufficio.
124701150908>>>>>C                   IF        d1uff <> '*' AND d1uff <> utnUff
124702150908>>>>>C                   EVAL      $rcdOk = *OFF
124703150908>>>>>C                   ENDIF
124705150908>>>>>** Filtro codice utente.
124706150908>>>>>C                   IF        d1cut <> *BLANK AND d1cut <> utnCut
124707150908>>>>>C                   EVAL      $rcdOk = *OFF
124708150908>>>>>C                   ENDIF
124709150908>>>>>>*
124800941110     C                   ENDSR
124900941110     C/EJECT
125000941110      ************************************************************
125100941110      * RIEMPIMENTO RIGA LISTA
125200941110      ************************************************************
125300941110     C     RIES1         BEGSR
125400941110      *
125500941110     C                   CLEAR                   FMTS1
125600941110    > * << riempio i campi del sfl, compresi gli hidden con i valori
125700941110    > *    del rcd letto >>
125800941110      *
125900941110    >C                   MOVEL     UTNPRF        S1PRF
126000941110    >C                   MOVEL     UTNUFF        S1UFF
126100941110    >C                   MOVEL     UTNGRP        S1GRP
126200941110    >C                   MOVEL     UTNCUT        S1CUT
126300941110    >C                   MOVEL     UTNLIN        S1LIN
126400020326A0910C                   MOVEL     UTNFlg1       H1Flg1
126500941110      *
126600941110    >C                   MOVE      '1'           XTBRIC
126700941110    >C                   MOVE      XSCLIN        XTBLIN
126800941110    >C                   MOVE      XSCSOC        XTBAZI
126900941110    >C                   MOVE      *ZEROS        XTBKEY
127000941110    >C                   MOVE      S1GRP         XTBKEY
127100941110    >C                   MOVE      'G22'         XTBCOD
127200941110    >C                   EXSR      LETTAB
127300941110    >C     WTBERR        IFEQ      *OFF
127400941110    >C                   MOVEL     XTBUNI        S1DGRP
127500941110    >C                   ELSE
127600941110    >C                   MOVEL     *BLANK        S1DGRP
127700941110    >C                   END
127701150908>>>>>** Reperisco la descrizione del profilo utente, molto più utile di quella
127702150908>>>>>** del gruppo.
127703150908>>>>>C                   CLEAR                   qusec
127704150908>>>>>C                   EVAL      qusbprv = %SIZE(qusec)
127705150908>>>>>C                   CALLP     RetrieveObjectDescription(qusd0200
127706150908>>>>>C                             : %SIZE(qusd0200) : 'OBJD0200'
127707150908>>>>>C                             : utnPrf + 'QSYS' : '*USRPRF' : qusec)
127708150908>>>>>C                   IF        qusei = *BLANK
127709150908>>>>>C                   EVAL      s1dGrp = qustd12
127710150908>>>>>C                   ENDIF
127800020705B0453 *
127900020705  "   *F13-----------------------------------------------------
128000020705  "   * imposto anche l'opzione perchè se avessi premuto l'F13                  f13
128100020705  "   * dovrei caricare l'opzione da replicare                                  f13
128200020705  "  C                   z-add     WOpz          S1Opz                          f13
128300020705  "   *   rec caricato direttamente con un opz:                                 f13
128400020705  "   *   la readc lo deve leggere, quindi attivo il sflnxtchg                  f13
128500020705  "  C                   if        S1Opz     <>  0                              f13
128600020705  "  C                   eval      *in32 = *on                                  f13
128700020705  "  C                   endif                                                  f13
128800020705B0453 *F13-----------------------------------------------------
128900010212A0860C*
129000010212  "  C* visualizza indicatore Contatto
129100010212  "  C                   EVAL      H1Cnt = *off
129200010212  "  C                   IF        UTNTpCnt <> *blank
129300010212  "  C                   EVAL      H1Cnt = *on
129400010212A0860C                   ENDIF
129500991201A0709C*
129600020617A0709C* visualizza indicatore Web
129700020617A0948X***                if        jbusin='S'
129800030423A0997X*** ex-A0948       if        jbusin<>'N'
129900030423A0997X*** ex-A0709       setoff                                           03
130000030423A0709C*
130100991202  "  C* richiamo controllo se utente è Web
130200991202  "  C                   call      'JBA003R'                            22
130300991202  "  C                   PARM                    s1prf
130400991202  "  C                   PARM                    $Trovato
130500991202  "  C*
130600991202  "  C                   if        *in22=*off
130700021216A0709C                   move      $Trovato      h1web
130800021216A0979C***                exsr      atrs1
130900021216A0709C                   endif
131000030423A0997X*** ex-A0709       else
131100030423  "  X***   "
131200030423  "  X***   "            seton                                            03
131300030423A0997X*** ex-A0709       endif
131400991201A0709C*
131500021216A0979C*
131600021216A0979C                   EXSR      AtrS1
131700991201      *
131800941110     C                   ENDSR
131900941110     C/EJECT
132000991201A0709C************************************************************
132100991201  "  C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
132200991201  "  C************************************************************
132300991201  "  C     ATRS1         BEGSR
132400991201  "  C*
132500991201  "  C* in questa routine bisogna attivare indicatori o campi programma
132600991201  "  C* che gestiscono gli attributi di uno o più campi di questa videata;
132700991201  "  C* attenzione : non è questo il posto in cui eseguire controlli,
132800991201  "  C*  dovrebbero già arrivare dei flag
132900991201  "  C*
133000991201  "  C* utente Web
133100991201  "  C                   IF        h1web=*off
133200991201  "  C                   SETON                                        41
133300991201  "  C                   ELSE
133400991201  "  C                   SETOFF                                       41
133500010212A0709C                   ENDIF
133600010212A0860C*
133700010212  "  C* codice contatto
133800010212  "  C                   IF        H1Cnt = *on
133900010212  "  C                   SETON                                        42
134000010212  "  C                   ELSE
134100010212  "  C                   SETOFF                                       42
134200010212A0860C                   ENDIF
134300020326A0910C*
134400020403  "  C* utente abilitato a Laguna
134500020326  "  C                   SETOFF                                       43
134600020403  "  C                   IF        xscLaguna = *on
134700020326  "  C                   IF        H1Flg1 = *on
134800020326  "  C                   SETON                                        43
134900020326  "  C                   ENDIF
135000020403  "  C                   ENDIF
135100020326A0910C*
135200010212A0709C*
135300991201  "  C                   ENDSR
135400991201A0709C/EJECT
135500941110      ************************************************************
135600941110      * GESTIONE F03 VIDEO S1
135700941110      ************************************************************
135800941110     C     F03S1         BEGSR
135900941110      *
136000941110     C                   MOVE      *ON           $FINE
136100941110      * fine programma
136200941110     C                   EXSR      ENDPGM
136300941110      *
136400941110     C                   ENDSR
136500941110     C/EJECT
136600941110      ************************************************************
136700941110      * GESTIONE F10 VIDEO S1
136800941110      * AGGIUNTA RECORD
136900941110      ************************************************************
137000941110     C     F10S1         BEGSR
137100941110      *
137200941110    >C                   RESET                   A002DS
137300941110    >C                   MOVEL     '01'          OPZ002
137400941110    >C                   MOVEL     A002DS        KPJBU
137500941110    >C                   CALL      'ANA002R'
137600941110     C                   PARM                    KPJBA
137700941110    >C                   MOVEL     KPJBU         A002DS
137800941110      * ritorno da PGM gestione
137900941110    >C                   EXSR      RTN002
138000941110     C     WINZS1        IFEQ      *ON
138100941110     C                   MOVE      *ON           $INZS1
138200941110      * se esistevano già righe sul sfl
138300941110      * calcolo pagina a cui deve posizionarsi
138400941110     C     WSFL          IFGT      0
138500941110     C                   EXSR      CLCPAG
138600941110      * altrimenti carico solo la 1a pagina
138700941110     C                   ELSE
138800941110     C                   Z-ADD     SFLPAG        WPAG
138900941110     C                   ENDIF
139000941110     C                   ENDIF
139100941110      *
139200941110     C                   MOVE      *HIVAL        $LASTV
139300941110      *
139400941110     C                   ENDSR
139500941110     C/EJECT
139600941110      ************************************************************
139700941110      * GESTIONE F12 VIDEO S1
139800941110      ************************************************************
139900941110     C     F12S1         BEGSR
140000941110      *
140100941110      *  se la 1a videata sono le parzializzazioni, per F12 torno a queste
140200941110     C     $LOP          IFEQ      1
140300941110     C                   MOVE      'D1'          $GEST
140400941110      *  se la 1a videata è questa, per F12 torno a menu
140500941110     C                   ELSE
140600941110     C                   MOVE      *ON           $FINE
140700941110     C                   END
140800941110      *
140900941110     C                   ENDSR
141000941110     C/EJECT
141100020705B0453 ************************************************************
141200020705  "   * GESTIONE F13 VIDEO S1
141300020705  "   * P.S:
141400020705  "   * E' simile alla funzione F13 del PDM: riporta l'opzione della
141500020705  "   * riga del sfl su cui è posizionato il cursore dalla riga
141600020705  "   * successiva fino alla fine del sfl stesso,
141700020705  "   * senza sovrapporsi alle opzioni preesistenti.
141800020705  "   ************************************************************
141900020705  "  C     F13S1         BEGSR
142000020705  "   *
142100020705  "   * reset indicatori DSPATR
142200020705  "  C                   MOVE      *ALL'0'       IN4149            9
142300020705  "  C                   MOVEA     IN4149        *IN(41)
142400020705  "   *
142500020705  "   * il controllo lo faccio solo se il sfl NON è vuoto
142600020705  "  C                   If        WMax    <>    0
142700020705  "   *
142800020705  "   *    determino Riga/Colonna del cursore
142900020705  "  C                   MOVE      NRG           R$$
143000020705  "  C                   MOVE      NCL           C$$
143100020705  "  C                   Z-ADD     RIRI          CURR
143200020705  "  C                   Z-ADD     COCO          CURC
143300020705  "   *
143400020705  "   * se sono posizionato sul sfl
143500020705  "  C     H1NmRc        IFEQ      'A001S1'
143600020705  "   *
143700020705  "   * salvo il numero del record su cui sono posizionato
143800020705  "  C                   z-add     C1NrR         WC1Rcd            3 0
143900020705  "   *
144000020705  "   * leggo il rcd su cui sono posizionato
144100020705  "  C     C1NrR         CHAIN     FMTS1                              21
144200020705  "   *
144300020705  "  C                   If        S1Opz   >     0
144400020705  "   *
144500020705  "   * Salvo opz e nr relativo in cui si trova l'opz da ripetere
144600020705  "  C                   exsr      F13SavPosOpz
144700020705  "   *
144800020705  "   * Aggiorno le righe caricate fino ad ora sul sfl,
144900020705  "   * con l'opz. da ripetere (solo se non ne esiste già un'altra)
145000020705  "  C                   exsr      F13FinoWMax
145100020705  "   *
145200020705  "   *
145300020705  "   * Carico tutte le righe possibili con l'opz da ripetere
145400020705  "  C                   eval      SflPag   =    9999
145500020705  "  C                   eval      WPag     =    9999
145600020705  "  C                   exsr      RolS1
145700020705  "   *
145800020705  "   * Reimposto sflpag
145900020705  "  C                   eval      SflPag   =    RigPag
146000020705  "  C                   eval      WPag     =    SflPag
146100020705  "   *
146200020705  "   * Ripristino la posizione iniziale del record
146300020705  "  C                   z-add     WC1Rcd        C1Rcd
146400020705  "   *
146500020705  "   * Ripristino il campo di salvataggio dell'opzione per non sporca
146600020705  "   * re il campo S1Opz in caso di ricaricamento del sfl
146700020705  "  C                   z-add     0             WOpz
146800020705  "   *
146900020705  "  C                   endif
147000020705  "   *
147100020705  "  C                   endif
147200020705  "   *
147300020705  "  C                   endif
147400020705  "   *
147500020705B0453C     XF13S1        ENDSR
147600020705B0453 ************************************************************
147700020705  "   * Salvo opzione e numero relativo in cui si trova l'opz
147800020705  "   * da ripetere
147900020705  "   ************************************************************
148000020705  "  C     F13SavPosOpz  BEGSR
148100020705  "   *
148200020705  "  C                   z-Add     S1Opz         WOpz              2 0
148300020705  "  C                   z-Add     C1Nrr         $X                3 0
148400020705  "   *
148500020705  "   *  riaggiorno il rcd con il SflNxtChg attivato
148600020705  "  C                   exsr      AtrS1
148700020705  "  C                   setOn                                        32
148800020705  "  C                   update    FmtS1
148900020705  "  C                   setoff                                       32
149000020705  "   *
149100020705B0453C     XF13SavPosOpz ENDSR
149200020705B0453 ************************************************************
149300020705  "   * Aggiorno le righe caricate fino ad ora sul sfl,
149400020705  "   * con l'opz. da ripetere (solo se non ne esiste già un'altra)
149500020705  "   ************************************************************
149600020705  "  C     F13FinoWMax   BEGSR
149700020705  "   *
149800020705  "  C                   add       1             $X
149900020705  "   *
150000020705  "  C                   DoW       $X      <=    WMax
150100020705  "   *
150200020705  "  C     $X            Chain     FmtS1                              21
150300020705  "  C   21              Leave
150400020705  "   *
150500020705  "  C                   if        S1Opz    =    0
150600020705  "  C                   z-add     WOpz          S1Opz
150700020705  "  C                   endif
150800020705  "   *
150900020705  "  C                   exsr      AtrS1
151000020705  "  C                   SetOn                                        32
151100020705  "  C                   UpDate    FmtS1
151200020705  "  C                   SetOff                                       32
151300020705  "   *
151400020705  "  C                   Add       1             $X
151500020705  "  C                   enddo
151600020705  "   *
151700020705B0453C     XF13FinoWMax  ENDSR
151800941110      ************************************************************
151900941110      * GESTIONE F17 VIDEO S1
152000941110      ************************************************************
152100941110     C     F17S1         BEGSR
152200941110      *
152300941110     C                   MOVE      'D1'          $GEST
152400941110      * forza certi valori nel caso il pgm sia stato richiamato con
152500941110      * chiave specifica
152600941110    > *          D1PRFH    IFNE 0
152700941110    > *                    Z-ADD0         D1PRFH
152800941110      *                    END
152900941110      *          $ORD      IFEQ 99
153000941110      *                    Z-ADD1         $ORD
153100941110      *                    SETON                         01
153200941110      *                    SETOF                         02
153300941110    > *                    Z-ADDUDATE     D1DRG
153400941110      *                    END
153500941110      *
153600941110     C                   ENDSR
153700941110     C/EJECT
153800941110      ************************************************************
153900941110      * GESTIONE F18 VIDEO S1
154000941110      ************************************************************
154100941110     C     F18S1         BEGSR
154200941110      *
154300941110     C                   MOVE      'D2'          $GEST
154400941110     C                   MOVEL     *ON           $INZD2
154500941110     C     $ORD          IFEQ      99
154600941110     C                   Z-ADD     1             $ORD
154700941110     C                   SETON                                        01
154800941110     C                   SETOFF                                       02
154900941110     C                   END
155000941110      *
155100941110     C                   ENDSR
155200941110     C/EJECT
155300941110      ************************************************************
155400941110      * GESTIONE F23 VIDEO S1
155500941110      ************************************************************
155600941110     C     F23S1         BEGSR
155700941110      *
155800950403     C                   Z-ADD     76            $L01
155900950403     C                   Z-ADD     76            $L02
156000941110     C                   EXSR      RIEOPZ
156100950403     C                   Z-ADD     23            X
156200950403     C                   MOVE      FF23          $FC2(X)
156300941110     C                   MOVEL     WOPZ1         C1OPZ1
156400060224A1148C                   MOVEL     $ALLOPT       H1ALLOPT
156500941110     C                   MOVEL     WOPZ2         C1OPZ2
156600941110     C                   Z-ADD     WCOZ          C1COZ
156700941110      *
156800941110     C                   ENDSR
156900941110     C/EJECT
157000941110      ************************************************************
157100941110      * GESTIONE F24 VIDEO S1
157200941110      ************************************************************
157300941110     C     F24S1         BEGSR
157400941110      *
157500941110      *
157600941110     C                   MOVEA     $FX2          $FX
157700941110     C                   MOVEA     $FC2          $FC
157800941110     C                   MOVEA     $FR2          $FR
157900941110     C                   Z-ADD     $ULKS1        $ULK
158000941110     C                   Z-ADD     79            $L01
158100941110     C                   Z-ADD     79            $L02
158200941110     C                   EXSR      RIEKEY
158300941110     C                   Z-ADD     $ULK          $ULKS1            3 0
158400941110     C                   MOVEA     $FC           $FC2
158500941110     C                   MOVEL     $KEY1         Z2KE1
158600060224A1148C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
158700941110     C                   MOVEL     $KEY2         Z2KE2
158800941110     C                   WRITE     FMTZ2
158900941110      *
159000941110     C                   ENDSR
159100941110     C/EJECT
159200941110      ************************************************************
159300941110      * CONTROLLO TESTATA LISTA
159400941110      ************************************************************
159500941110     C     CTRC1         BEGSR
159600941110      *
159700941110     C                   MOVE      *OFF          *IN99
159800941110      *
159900941110      * SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
160000941110      * L'INIZIALIZZAZIONE LISTA
160100941110      *
160200941110     C                   SELECT
160300941110     C     $ORD          WHENEQ    2
160400941110    >C     C1POS2        IFNE      W1POS2
160500941110     C                   EXSR      POSS1
160600941110     C                   END
160700941110      *
160800941110     C                   OTHER
160900941110      *
161000941110    >C     C1POS         IFNE      W1POS
161100941110     C                   EXSR      POSS1
161200941110     C                   END
161300941110     C                   ENDSL
161400941110      *
161500941110     C                   ENDSR
161600941110     C/EJECT
161700941110      ************************************************************
161800941110      * CONTROLLI PER POSIZIONAMENTO SU LISTA
161900941110      ************************************************************
162000941110     C     POSS1         BEGSR
162100941110      *
162200941110      * CONTROLLO SE ESISTONO OPZIONI ATTIVE
162300941110      *
162400941110     C     WMAX          IFGT      0
162500941110     C                   READC     FMTS1                                  21
162600941110     C     *IN21         DOWEQ     *OFF
162700941110     C     *IN99         ANDEQ     *OFF
162800941110      *
162900941110     C     S1OPZ         IFNE      0
163000941110     C                   SETON                                        51  99
163100941110     C                   ENDIF
163200941110      * se rilevata opzione attiva, attivo sflnxtchg
163300941110     C     *IN99         IFEQ      *ON
163400941110     C                   MOVE      *ON           *IN32
163500991201A0709C                   EXSR      ATRS1
163600941110     C                   UPDATE    FMTS1
163700941110     C                   ELSE
163800941110     C                   READC     FMTS1                                  21
163900941110     C                   ENDIF
164000941110      *
164100941110     C                   ENDDO
164200941110      *
164300941110     C                   ENDIF
164400941110      *
164500941110      * SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
164600941110      * AL VALORE DI POSIZIONAMENTO :
164700941110      * se questo è maggiore del valore del rispettivo campo di
164800941110      * parzializzazione lo uso, altrimenti eseguo un ricaricamento
164900941110      * con i valori di parzializzazione.
165000941110      *
165100941110     C     *IN99         IFEQ      *OFF
165200941110    >C                   MOVE      *BLANK        W1POS
165300941124    >C                   Z-ADD     0             W1POS2
165400941110      *
165500941110     C                   SELECT
165600941110      *
165700941110     C     $ORD          WHENEQ    1
165800941110      *
165900941110    >C     C1POS         IFGT      D1PRF
166000941110    >C                   MOVEL     C1POS         W1POS
166100941110     C                   END
166200941110      *
166300941110     C                   MOVE      *ON           *IN99
166400941110     C                   MOVE      *ON           $INZS1
166500941110      *
166600941110     C     $ORD          WHENEQ    99
166700941110      *
166800941110      ***        C1POS     IFGT D1PRG
166900941110      **                   MOVE C1POS     W1POS
167000941110      **                   END
167100941110     C                   MOVE      *ON           *IN99
167200941110     C                   MOVE      *ON           $INZS1
167300941110      *
167400941110      *
167500941110     C     $ORD          WHENEQ    2
167600941110      *
167700941110    >C     C1POS2        IFGT      D1GRP
167800941110    >C                   MOVE      C1POS2        W1POS2
167900941110     C                   END
168000941110      *
168100941110     C                   MOVE      *ON           *IN99
168200941110     C                   MOVE      *ON           $INZS1
168300941110      *
168400941110     C                   ENDSL
168500941110      *
168600941110     C                   ENDIF
168700941110      *
168800941110     C                   ENDSR
168900941110     C/EJECT
169000941110      ************************************************************
169100941110      * CONTROLLO OPZIONI LISTA
169200941110      ************************************************************
169300941110     C     CTRS1         BEGSR
169400941110      *
169500020705B0453C                   move      *off          $RieF13DS
169600941110     C                   MOVEL     *OFF          $ESCI
169700941110     C                   SETOFF                                       99
169800941110     C                   Z-ADD     0             S1OPZ
169900941110      *
170000941110      * Leggo il sfl solo se ci sono rcd
170100941110     C     WMAX          IFGT      0
170200941110     C                   READC     FMTS1                                  21
170300941110      *
170400941110      * esce se fine sfl o errore che richiede l'uscita
170500941110     C     *IN21         DOWEQ     *OFF
170600941110     C     $ESCI         ANDEQ     *OFF
170700941110     C                   Z-ADD     S1NRR         C1RCD
170800941110      * ctrl su riga
170900941110     C                   EXSR      RECS1
171000941110      * gestione opzioni
171100941110     C     S1OPZ         IFNE      0
171200941110     C     *IN99         ANDEQ     *OFF
171300941110     C                   EXSR      OPZS1
171400941110     C                   ENDIF
171500941125      * se rilevato errore, attivo sflnxtchg
171600941110     C     *IN99         IFEQ      *ON
171700941110     C                   MOVE      *ON           *IN32
171800941110     C                   ENDIF
171900941110      *
172000020705B0453 * se mi hanno passato la ds multipla, l'opz è 1=scelta e
172100020705  "   * non ci sono errori memorizzo che a fine pgm dovrò caricare
172200020705  "   * la ds multipla per cui dovrò rileggere le righe del sfl
172300020705  "   * selezionate con 1=selezione quindi accendo il sflnxtchg
172400020705  "  C                   if        Parms>1
172500020705  "  C                             and  S1Opz=1
172600020705  "  C                             and  *in99=*off
172700020705  "  C                   move      *On           $RieF13DS
172800020705  "  C                   move      *On           *In32
172900020705B0453C                   else
173000941110     C                   Z-ADD     0             S1OPZ
173100020705B0453C                   endif
173200941110      *
173300991201A0709C                   exsr      atrs1
173400991201     C                   UPDATE    FMTS1
173500941125     C                   MOVE      *OFF          *IN32
173600941110      *
173700941110      * leggo prossimo rcd dal sfl se no richiesta uscita ciclo
173800941110     C     $ESCI         IFEQ      *OFF
173900941110     C                   READC     FMTS1                                  21
174000941110      * a fine ciclo ripristino il flag richiesta iniz.sfl
174100941110     C     *IN21         IFEQ      *ON
174200020705B0453 *
174300020705  "   *  se mi hanno passato la ds multipla e esiste almeno un 1=selezione,
174400020705  "   *  a fine pgm ripasserò nella ds stessa i rec selezionati
174500020705  "   *  quindi se ho già letto tutti i rec del sfl, vado a fine pgm
174600020705  "  C                   if        $RieF13DS = *on
174700020705  "  C                   MOVE      *ON           $FINE
174800020705  "   *  se non mi hanno passato la ds multipla
174900020705  "   *  ripristino il flag richiesta iniz.sfl
175000020705B0453C                   else
175100941110     C                   MOVE      WINZS1        $INZS1
175200020705B0453C                   endif
175300020705  "  C* se è avvenuta almeno una modifica
175400020705B0453C                   IF        $INZS1 = *on
175500941110      * calcolo pagina a cui deve posizionarsi
175600941110     C     WSFL          IFGT      0
175700941110     C                   EXSR      CLCPAG
175800941110     C                   ELSE
175900941110     C                   Z-ADD     SFLPAG        WPAG
176000941110     C                   ENDIF
176100020705B0453C                   ENDIF
176200941110     C                   ENDIF
176300941110     C                   ENDIF
176400941110      *
176500941110     C                   ENDDO
176600941110      *
176700941110     C                   ENDIF
176800941110      *
176900941110     C                   ENDSR
177000941110     C/EJECT
177100941110      ************************************************************
177200941110      * CONTROLLO CAMPI I/O RIGA LISTA
177300941110      ************************************************************
177400941110     C     RECS1         BEGSR
177500941110      *
177600941110      * reset indicatori DSPATR
177700941110     C                   MOVE      *ALL'0'       IN4049           10
177800941110     C                   MOVEA     IN4049        *IN(40)
177900941110      *
178000941110      * Controllo se Utente Autorizzato alla scelta
178100941110     C     S1OPZ         IFNE      0
178200941110     C                   Z-ADD     1             X
178300941110     C     S1OPZ         LOOKUP    $OP1(X)                                21
178400941110     C     *IN21         IFEQ      *OFF
178500941110     C     $FL1(X)       OREQ      'N'
178600941110     C                   MOVE      *ON           $ESCI
178700941110     C                   SETON                                        504099
178800941110     C                   ENDIF
178900941110     C                   ENDIF
179000941110      *
179100941110     C                   ENDSR
179200941110     C/EJECT
179300941110      ************************************************************
179400941110      * GESTIONE OPZIONI LISTA
179500941110      ************************************************************
179600941110     C     OPZS1         BEGSR
179700991117a0709C*
179800991117  "  C                   Z-ADD     RIRI          CURR
179900991123a0709C                   Z-ADD     COCO          CURC
180000941110      *
180100941110     C                   SELECT
180200941110      *
180300941110      * se opz=Selezione passo la chiave al pgm chiamante
180400020705B0453 * P.S:
180500020705  "   * se mi hanno passato la ds multipla,
180600020705B0453 * i rec selezionati con 1=selezione li ripasserò con la ds suddetta
180700941110     C     S1OPZ         WHENEQ    1
180800020705B0453C                   if        Parms < 2
180900941110    > * << passare nei campi appositi della DS D001DS il valore
181000941110    > *    della chiave della VL univoca (se c'è) >>
181100941110    >C                   MOVE      S1OPZ         OPZ001
181200941110    >C                   MOVE      S1PRF         PRF001
181300031112B0644x****               Z-ADD     S1CUT         CUT001
181400031112B0644C                   move      S1CUT         CUT001
181500941110     C                   MOVE      *ON           $ESCI
181600941110     C                   MOVE      *ON           $FINE
181700941110      *
181800941110     C                   MOVE      *HIVAL        $LASTV
181900020705B0453C                   endif
182000991117     C*
182100991117a0709C* se opz=Utenti remoti
182200991123  "  C     S1opz         Wheneq    11
182300991123  "  C                   MOVE      '01'          OPZjba
182400991123  "  C                   move      '0'           RETjba
182500991123  "  C                   move      '0'           OPRjba
182600991123  "  C                   move      '0'           ERRjba
182700991123  "  C                   MOVE      xscsoc        Societajba
182800991123  "  C                   clear                   RMTUSRjba
182900991123  "  C                   MOVE      S1Prf         ProjUsrjba
183000991117  "  C*
183100991123  "  C                   CALL      'JBA001R'                            22
183200991123  "  C                   parm                    OPZjba            2
183300991123  "  C                   parm                    RETjba            1
183400991123  "  C                   parm                    OPRjba            1
183500991123  "  C                   parm                    ERRjba            1
183600991123  "  C                   parm                    SOCIETAjba        3
183700991123  "  C                   parm                    RMTUSRjba         9
183800991123  "  C                   parm                    PROJUSRjba       10
183900991117  "  C                   PARM                    KPJBA
184000991117  "  C*
184100991117  "  C* ritorno da PGM
184200991201  "  C                   EXSR      RTNjba
184300991201  "  C*
184400991201  "  C* controllo se utente Web
184500991202  "  C*
184600991202  "  C* richiamo controllo se utente è Web
184700991202  "  C                   call      'JBA003R'                            22
184800991202  "  C                   PARM                    s1prf
184900991202  "  C                   PARM                    $Trovato
185000991202  "  C                   if        *in22=*off
185100991202  "  C                   move      $Trovato      h1web
185200991202  "  C                   exsr      atrs1
185300991202A0709C                   endif
185400941110      *
185500941110      * se altre opzioni (o nessuna opzione) richiamo pgm gestione
185600941110     C                   OTHER
185700941110      *
185800941110    >C                   RESET                   A002DS
185900941110    > * << passare nei campi appositi della DS D002DS il valore
186000941110    > *    della chiave della VL univoca (se c'è) >>
186100941110    >C                   MOVEL     S1OPZ         OPZ002
186200941110    >C                   MOVEL     S1PRF         PRF002
186300941110      *
186400941110    >C                   MOVEL     A002DS        KPJBU
186500941110    >C                   CALL      'ANA002R'
186600941110     C                   PARM                    KPJBA
186700941110    >C                   MOVEL     KPJBU         A002DS
186800941110      *
186900941110      * ritorno da PGM gestione
187000941110    >C                   EXSR      RTN002
187100941110      * se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
187200941110     C     *IN99         IFEQ      *ON
187300941110     C                   SETON                                        40
187400941110     C                   ENDIF
187500941110      *
187600941110     C                   MOVE      *HIVAL        $LASTV
187700941110     C                   ENDSL
187800941110      *
187900941110     C                   ENDSR
188000941110     C/EJECT
188100991117a0709C************************************************************
188200991117  "  C* GESTIONE RITORNO DA PGM utenti remoti
188300991117  "  C************************************************************
188400991117  "  C*
188500991117  "  C     RTNjba        BEGSR
188600991117  "  C*
188700991117  "  C* modo di ritorno
188800991117  "  C*
188900991117  "  C                   SELECT
189000991117  "  C* 1 = F3
189100991117  "  C     RETjba        WHENEQ    '1'
189200991117  "  C                   MOVE      *ON           $ESCI                          sospese READC
189300991117  "  C* 2 = F12
189400991117  "  C     RETjba        WHENEQ    '2'
189500991117  "  c     OPRjba        ANDNE     '1'
189600991117  "  C                   MOVE      *ON           $ESCI                          sospese READC
189700991117  "  C*
189800991117  "  C                   ENDSL
189900991117  "  C*
190000991117  "  C* operazione eseguite dal pgm chiamato
190100991117  "  C*
190200991117  "  C                   SELECT
190300991117  "  C* 1 = eseguita scrittura o modifica commento
190400991117  "  C     OPRjba        WHENEQ    '1'
190500991117  "  C*
190600991117  "  C                   ENDSL
190700991117  "  C*
190800991117  "  C* funzione non eseguibile per errore :
190900991117  "  C*
191000991117  "  C                   SELECT
191100991117  "  C* 1 = funzione richiamata chiusa in errore
191200991117  "  C     ERRjba        WHENEQ    '1'
191300991117  "  C                   MOVE      *ON           $ESCI                          sospese READC
191400991117  "  C                   SETON                                        9399      emiss.SFLMSGID
191500991117  "  C* 2 = funzione richiamata senza la dovuta autorizzazione
191600991117  "  C     ERRjba        WHENEQ    '2'
191700991117  "  C                   MOVE      *ON           $ESCI                          sospese READC
191800991117  "  C                   SETON                                        9499      emiss.SFLMSGID
191900991117  "  C*
192000991117  "  C                   ENDSL
192100991117  "  C*
192200991117  "  C                   ENDSR
192300991117a0709C/EJECT
192400941110      ************************************************************
192500941110      * GESTIONE RITORNO DA PGM DI GESTIONE
192600941110      ************************************************************
192700941110    >C     RTN002        BEGSR
192800941110      *
192900941110      * modo di ritorno
193000941110      *
193100941110     C                   SELECT
193200941110    > * << questi modi di utilizzo dei valori di ritorno dal
193300941110    > *    pgm di manutenzione rcd di anagrafica sono delle
193400941110    > *    proposte, normalmente sempre valide, ma modificabili
193500941110    > *    per situazioni particolari >>
193600941110      * 1 = F3
193700941110    >C     RET002        WHENEQ    '1'
193800941110     C                   MOVE      *ON           $ESCI                          sospese READC
193900941110     C                   MOVE      *ON           $FINE                          fine pgm
194000941110      * 2 = F12
194100941110    >C     RET002        WHENEQ    '2'
194200941110     C                   MOVE      *ON           $ESCI                          sospese READC
194300941110      *
194400941110     C                   ENDSL
194500941110      *
194600941110      * operazione eseguite dal pgm chiamato
194700941110      *
194800941110     C                   SELECT
194900941110      * 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
195000941110    >C     OPR002        WHENEQ    '1'
195100941110     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
195200941110      *
195300941110     C                   ENDSL
195400941110      *
195500941110      * funzione non eseguibile per errore :
195600941110      *
195700941110     C                   SELECT
195800941110      * 1 = funzione richiamata chiusa in errore
195900941110    > *  eventualmente gestire altri codici di errore
196000941110    >C     ERR002        WHENEQ    '1'
196100941110     C                   MOVE      *ON           $ESCI                          sospese READC
196200941110     C                   SETON                                        5299      emiss.SFLMSGID
196300941110      *
196400941110     C                   ENDSL
196500941110      *
196600941110     C                   ENDSR
196700941110     C/EJECT
196800941110      ************************************************************
196900941110      * GESTIONE VALORI ASSUNTI
197000941110      ************************************************************
197100941110     C     GESD2         BEGSR
197200941110      *
197300941110     C     $INZD2        IFEQ      *ON
197400941110     C                   EXSR      INZD2
197500941110     C                   MOVE      *OFF          $INZD2
197600941110     C                   END
197700941110      *
197800941110     C     $LASTV        IFNE      'D2'
197900941110     C                   EXSR      GESPIE
198000941110     C                   MOVE      $LASTV        $LASTG
198100941110     C                   MOVE      'D2'          $LASTV
198200941110     C                   ENDIF
198300941110      *
198400941110      * se esistono errori sulla videata
198500941110      * emetto la write del formato a indicatori spenti per vedere
198600941110      * le eventuali decodifiche
198700941110     C     *IN99         IFEQ      *ON
198800941110     C                   MOVEA     *IN           WIN
198900941110     C                   MOVE      *ALL'0'       IN5098           49
199000941110     C                   MOVEA     IN5098        *IN(50)
199100941110     C                   WRITE     FMTD2
199200941110     C                   MOVEA     WIN           *IN
199300941110     C                   ENDIF
199400941110      *
199500941110     C                   EXFMT     FMTD2
199600941110      *
199700941110     C                   SELECT
199800941110      * F3=Fine
199900941110     C     $TASTO        WHENEQ    F03
200000941110     C                   EXSR      F03D2
200100941110      * F12=Ritorno
200200941110     C     $TASTO        WHENEQ    F12
200300941110     C                   EXSR      F12D2
200400941110      * F24=Altri tasti
200500941110     C     $TASTO        WHENEQ    F24
200600941110     C                   EXSR      F24D2
200700941110      *
200800941110     C                   OTHER
200900941110     C                   EXSR      CTRD2
201000941110     C     *IN99         IFEQ      *OFF
201100941110     C                   MOVE      $LASTG        $GEST
201200941110     C                   SELECT
201300941110     C     $LASTG        WHENEQ    'S1'
201400941110      * se è stato modificato l'ordinamento di default, ricarico la lista
201500941110     C     $ORD          IFNE      D2ORD
201600941110     C                   MOVE      *ON           $INZS1
201700941110     C                   MOVE      *BLANK        W1POS
201800941124     C                   Z-ADD     0             W1POS2
201900941110     C                   END
202000941110     C     $LASTG        WHENEQ    'D1'
202100941110      * per ora niente
202200941110     C                   ENDSL
202300941110      * definisco i nuovi valori di default
202400941110     C                   Z-ADD     D2LOP         $LOP
202500941110     C                   Z-ADD     D2ORD         $ORD
202600941124     C                   SELECT
202700941124     C     $ORD          WHENEQ    1
202800941124     C                   SETON                                            01
202900941124     C                   SETOFF                                           02
203000941124     C     $ORD          WHENEQ    2
203100941110     C                   SETON                                            02
203200941124     C                   SETOFF                                           01
203300941124     C                   ENDSL
203400941110    > * << elenco altri dati default >>
203500941110     C                   END
203600941110      *
203700941110     C                   ENDSL
203800941110      *
203900941110     C                   ENDSR
204000941110     C/EJECT
204100941110      ************************************************************
204200941110      * INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
204300941110      ************************************************************
204400941110     C     INZD2         BEGSR
204500941110      *
204600941110     C                   CLEAR                   FMTD2
204700941110     C                   MOVEL     $LOP          D2LOP
204800941110     C                   MOVE      $ORD          D2ORD
204900941110    > * << elenco altri dati default >>
205000941110      *
205100941110     C                   ENDSR
205200941110     C/EJECT
205300941110      ************************************************************
205400941110      * GESTIONE F03 VIDEO D2
205500941110      ************************************************************
205600941110     C     F03D2         BEGSR
205700941110      *
205800941110     C                   MOVE      *ON           $FINE
205900941110      * fine programma
206000941110     C                   EXSR      ENDPGM
206100941110      *
206200941110     C                   ENDSR
206300941110     C/EJECT
206400941110      ************************************************************
206500941110      * GESTIONE F12 VIDEO D2
206600941110      ************************************************************
206700941110     C     F12D2         BEGSR
206800941110      *
206900941110     C                   MOVE      $LASTG        $GEST
207000941110      *
207100941110     C                   ENDSR
207200941110     C/EJECT
207300941110      ************************************************************
207400941110      * GESTIONE F24 VIDEO D2
207500941110      ************************************************************
207600941110     C     F24D2         BEGSR
207700941110      *
207800941110     C                   MOVEA     $FX3          $FX
207900941110     C                   MOVEA     $FC3          $FC
208000941110     C                   MOVEA     $FR3          $FR
208100941110     C                   Z-ADD     $ULKD2        $ULK
208200941110     C                   Z-ADD     56            $L01
208300941110     C                   Z-ADD     56            $L02
208400941110     C                   EXSR      RIEKEY
208500941110     C                   Z-ADD     $ULK          $ULKD2            3 0
208600941110     C                   MOVEA     $FC           $FC3
208700941110     C                   MOVEL     $KEY1         Z3KE1
208800060224A1148C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
208900941110     C                   MOVEL     $KEY2         Z3KE2
209000941110     C                   WRITE     FMTZ3
209100941110      *
209200941110     C                   ENDSR
209300941110     C/EJECT
209400941110      ************************************************************
209500941110      * CONTROLLO VIDEATA VALORI ASSUNTI
209600941110      ************************************************************
209700941110     C     CTRD2         BEGSR
209800941110      *
209900941110     C                   SETOFF                                       99
210000941110      * non imposto pos. del cursore
210100941110     C                   Z-ADD     0             H1RIGA
210200941110     C                   Z-ADD     0             H1COLO
210300941110      *
210400941110      * OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
210500941110     C     D2LOP         IFEQ      0
210600941110     C                   SETON                                          5099
210700941110     C                   END
210800941110      *
210900941110      * OBBLIGATORIO VALORE PER ORDINAMENTO
211000941110     C     D2ORD         IFEQ      0
211100941110     C                   SETON                                          5199
211200941110     C                   END
211300941110    > * << elenco controlli altri dati default >>
211400941110      *
211500941110     C                   ENDSR
211600941110     C/EJECT
211700941110      ************************************************************
211800941110      * AGGIORNAMENTO VALORI ASSUNTI
211900941110      ************************************************************
212000941110     C     AGGMPE        BEGSR
212100941110      *
212200941110     C                   CLEAR                   ANMPE000
212300941110     C                   MOVEL     XSCSOC        MPESOC
212400941110     C                   MOVEL     DSPGM         MPEPGM
212500941110     C                   MOVEL     KNMUS         MPEPRF
212600941110     C                   SELECT
212700941110     C     OPZ001        WHENEQ    '01'
212800941110     C                   MOVEL     'PX'          MPETIP
212900941110     C                   OTHER
213000941110     C                   MOVEL     'PV'          MPETIP
213100941110     C                   ENDSL
213200941110     C     K4MPE2        CHAIN     ANMPE02L                           21
213300941110     C                   MOVEL     DSMPE         MPEPAR
213400941110     C     *IN21         IFEQ      *ON
213500941110     C                   WRITE     ANMPE000
213600941110     C                   ELSE
213700941110     C                   UPDATE    ANMPE000
213800941110     C                   END
213900941110      *
214000941110     C                   ENDSR
214100941110     C/EJECT
214200941110      ************************************************************
214300941110      * OPERAZIONI INIZIALI
214400941110      ************************************************************
214500941110     C     *INZSR        BEGSR
214600941110      *
214700941110      * Reperimento parametri
214800941110      *
214900941110     C     *ENTRY        PLIST
215000941110     C                   PARM                    KPJBA
215100020705B0453C                   parm                    F13DS
215200020705B0453C                   parm                    NElemF13DS
215300941110      *
215400941110      * Reperimento opzioni
215500941110      *
215600941110     C                   EXSR      REPOPZ
215700941110      *
215800941110      * Reperimento tasti di funzione
215900941110      *
216000941110     C                   EXSR      REPKEY
216100941110      *
216200941110      *
216300941110     C                   ENDSR
216400941110     C/EJECT
216500941110      ************************************************************
216600941110      * REPERIMENTO AUTORIZZAZIONI UTENTE
216700941110      ************************************************************
216800941110      *          REPAUT    BEGSR
216900941110      *
217000941110      *                    CLEARXAUT
217100941110      *
217200941110      *                    Z-ADD0         XCAGRP           GRUPPO
217300941110      *                    MOVELKNMUS     XCAPRF           PROFILO
217400941110    > *                    MOVEL'OBJALL'  XCAFNC           FUNZIONE
217500941110      *                    MOVEL*ZERO     XCAVFU           VARIABILE
217600941110      *                    MOVE 'CK'      XCATCK           TIPO CTRL
217700941110      *
217800941110      *                    CALL 'XCHKAUT'
217900941110      *                    PARM           XAUT
218000941110      * utente non abilitato
218100941110      *          XCARTN    IFEQ 0
218200941110    > *<                   MOVELXCADDS    AUTDS
218300941110      *                    ELSE
218400941110    > *<                   CLEARAUTDS
218500941110      *                    END
218600941110      *
218700941110      *                    ENDSR
218800941110     C/EJECT
218900941110      ************************************************************
219000941110      * Reperimento dati società
219100941110      ************************************************************
219200941110     C     REPSOC        BEGSR
219300941110      *
219400941110     C                   CALL      'XSOC'
219500941110     C                   PARM                    TIPXSC            6
219600941110     C                   PARM                    SOCXSC            3
219700941110     C                   PARM                    CDSXSC            9 0
219800941110     C                   PARM                    MODXSC            3
219900941110     C                   PARM      *BLANKS       RTNXSC            1
220000941110     C                   PARM                    XSOCDS
220100941110     C                   PARM                    KPJBA
220200941110      *
220300941110     C                   ENDSR
220400941110     C/EJECT
220500941110      ************************************************************
220600941110      * INIZIALIZZAZIONE VARIABILI
220700941110      ************************************************************
220800941110     C     INZVAR        BEGSR
220900941110      *
221000941110      *
221100941110      * Dati da PGM chiamante
221200941110      *
221300941110    >C                   MOVEL     KPJBU         A001DS
221400941110      *
221500941215      *
221600941215      * Reperimento dati società
221700941215      *
221800941215     C                   MOVEL     'SOC001'      TIPXSC
221900941215     C                   MOVEL     *BLANK        SOCXSC
222000941215     C                   EXSR      REPSOC
222100941215     C     RTNXSC        IFNE      '1'
222200941215     C                   MOVEL     XSOCDS        XSC001
222300941215     C                   ELSE
222400941215     C                   MOVE      *ON           $FINE
222500941215     C                   EXSR      ENDPGM
222600941215     C                   ENDIF
222700030423A0997X*** ex-A0709
222800030423  "  X***   " Reperisco informazioni da parametro Sicom setup generale
222900030423  "  X***   "            Callb     'XPAR'
223000030423  "  X***   "            Parm      XScSoc        XPASOC
223100030423  "  X***   "            Parm      'SICOM'       XPAPAR
223200030423  "  X***   "            Parm      *Blanks       XPAOUT
223300030423  "  X***   "            Parm      *Blanks       XPAERR
223400030423  "  X***   "            Parm      *Blanks       XPARIC
223500030423  "  X***   "
223600030423  "  X***   "            Clear                   Sicom
223700030423  "  X***   " XPAERR        IFEQ      *OFF
223800030423  "  X***   "            MOVEL     XPAOUT        Sicom
223900030423  "  X***   "            ELSE
224000030423  "  X***   "            MOVE      *ON           $FINE
224100030423  " >X***   "            MOVE      '1'           Err001
224200030423  "  X***   "            EXSR      ENDPGM
224300030423A0997X*** ex-A0709       ENDIF
224400941110      * Reperimento valori default utente
224500941110      *
224600941110     C                   EXSR      REPDFT
224700941110      *
224800941110      * Definizione delle opzioni utilizzabili a pgm
224900941110      *
225000941110     C                   EXSR      DEFOPZ
225100941110      * Variabili per gestione videate
225200941110      *
225300941110     C                   MOVE      *OFF          $FINE                          fine pgm
225400941110     C                   MOVE      *OFF          $INZD1                         inizializzione D1
225500941110     C                   MOVE      *OFF          $INZS1                         inizializzione S1
225600941110     C                   MOVE      *OFF          $INZD2                         inizializzione D2
225700941110     C                   MOVE      *OFF          $EFILE                         memo fine file
225800941110     C                   MOVE      *OFF          $ESCI                          rich.uscita ciclo
225900941110     C                   MOVE      *OFF          $RCDOK                         rcd letto valido
226000941110     C                   MOVE      *HIVAL        $LASTV
226100941110     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
226200941110     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
226300941110     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
226400941110      *
226500941110     C                   MOVE      *ON           $INZD1
226600941110     C                   MOVE      *ON           $INZS1
226700941110      *
226800941110      * se pgm richiamato con chiave univoca impostata,
226900941110      * forzo la emissione della lista e imposto l'ordinamento
227000941110      *
227100941110      *
227200941110      *
227300020705B0453 * definisco qual'è la prima videata
227400020705  "   *
227500020705  "   * se non è la 1° volta che sono chiamato per l'F4
227600020705  "   * le variabili sono valorizzate in base al giro precedente
227700020705  "  C                   move      *off          $RieF13DS
227800020705  "   *
227900020705  "   * se ho ricevuto il parametro che contiene il nr elementi
228000020705  "   * caricati in ds multipla (NElemF13DS) con valore =
228100020705  "   * al nr max elementi caricabili nella ds stessa ($DimF13DS)
228200020705  "   * (NElemF13DS è impostato solo qui, passato al chiamante e
228300020705  "   *  ripassato da quest'ultimo se NElemF13DS = $DimF13DS)
228400020705  "   * vado a caricare nella ds multipla gli eventuali
228500020705  "   * ulteriori rec del sfl rimasti con opz.1=scelta
228600020705  "  C                   if        Parms > 1  and
228700020705  "  C                             NElemF13DS = $DimF13DS
228800020705  "  C                   move      *on           $RieF13DS
228900020705  "  C                   exsr      EndPgm
229000020705  "   *
229100020705  "   * se non devo caricare ulteriori rec del sfl
229200020705  "   * sulla ds multipla:
229300020705  "  C                   else
229400020705B0453 *
229500941110      *
229600941116     C                   MOVE      'D1'          $GEST
229700941110     C                   SELECT
229800941110     C     $LOP          WHENEQ    1
229900941110     C                   MOVE      'D1'          $GEST
230000941110     C     $LOP          WHENEQ    2
230100941110     C                   MOVE      'S1'          $GEST
230200941110     C                   EXSR      INZD1
230300941110     C                   MOVE      *OFF          $INZD1
230400941110     C                   ENDSL
230500020705B0453 *
230600020705  "  C                   endif
230700020705B0453 *
230800941110      *
230900941110      * Variabili appoggio
231000941110      *
231100020705B0453C* inizialmente imposto il numero di righe per pagina                      F13
231200020705B0453C                   z-add     RigPag        SflPag                         F13
231300941110     C                   Z-ADD     SFLPAG        WPAG
231400941110      *
231500941110      * Valorizzazione campi univoci testate
231600941110      *
231700941110     C                   CLEAR                   FMTT1
231800941110     C                   MOVEL     KNSIF         NOMSIF
231900941110     C                   MOVEL     XSCDSI        NOMDIT
232000941110     C                   MOVEL     DSPGM         NOMPGM
232100941110      *
232200941110      *
232300941110      * Reperimento azienda
232400941110      *
232500941110      * Accendo indicatori per condizioni su campo posizionamento C1POS
232600941110     C                   SELECT
232700941110     C     $ORD          WHENEQ    1
232800941110     C                   SETON                                            01
232900941110     C     $ORD          WHENEQ    2
233000941110     C                   SETON                                            02
233100941110     C                   ENDSL
233200941110      *
233300941110      *
233400941110     C                   ENDSR
233500941110     C/EJECT
233600941110      ************************************************************
233700941110      * REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
233800941110      ************************************************************
233900941110     C     REPDFT        BEGSR
234000941110      *
234100941110     C                   MOVEL     XSCSOC        MPESOC
234200941110     C                   MOVEL     DSPGM         MPEPGM
234300941110     C                   MOVEL     KNMUS         MPEPRF
234400941110     C                   SELECT
234500941110     C     OPZ001        WHENEQ    '01'
234600941110     C                   MOVEL     'PX'          MPETIP
234700941110     C                   OTHER
234800941110     C                   MOVEL     'PV'          MPETIP
234900941110     C                   ENDSL
235000941110     C     K4MPE2        CHAIN(N)  ANMPE02L                           21
235100941110     C     *IN21         IFEQ      *OFF
235200941110     C                   MOVEL     MPEPAR        DSMPE
235300941110      * nel caso sia la prima volta che l'utente apre questa anagrafica
235400941110      * i suoi valori di default vengono decisi a pgm
235500941110     C                   ELSE
235600941115     C                   CLEAR                   DSMPE
235700941110     C                   Z-ADD     1             $LOP
235800941110     C                   Z-ADD     1             $ORD
235900941110     C                   SETON                                        01
236000941110    > * << elenco altri dati default >>
236100941110     C                   END
236200941110      *
236300941110     C                   ENDSR
236400941110     C/EJECT
236500941110      ************************************************************
236600941110      * DEFINIZIONE OPZIONI UTILIZZABILI A PGM
236700941110      ************************************************************
236800941110     C     DEFOPZ        BEGSR
236900941110      *
237000941110      * Questa routine deve valorizzare la schiera $FL1
237100941110      * con S o N a seconda della utilizzabilità o meno
237200941110      * dell'opzione correlata
237300941110      *
237400941110      * Inizialmente si considerano tutte le opzioni come valide
237500941110     C     1             DO        $DIMOP        X
237600941110     C                   MOVE      'S'           $FL1(X)
237700941110     C                   END
237800941110      *
237900941110      * Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
238000941110      * con S o N a seconda della utilizzabilità o meno
238100941110      * del tasto funzione correlato
238200941110      *
238300941110      * Inizialmente ripristino le abilitazioni come da schiera a tempo
238400941110      * di compilazione
238500941110     C                   RESET                   $FC1
238600941110     C                   RESET                   $FR1
238700941110     C                   RESET                   $FC2
238800941110     C                   RESET                   $FR2
238900941110     C                   RESET                   $FC3
239000941110     C                   RESET                   $FR3
239100030423A0997X*** ex-A0709 l'opz.11=utente remoto è da visualizzare solo se
239200030423A0997X*** ex-A0709 sul p.modulo SICOM il campo  J-Business installato = S
239300020617A0948X***                If        Jbusin <> 'S'
239400030423A0997X*** ex-A0948       If        Jbusin =  'N'
239500030423  "  X*** ex-A0709       Z-ADD     1             X
239600030423  "  X***   "  11            lookup    $OP1(X)                            21
239700030423  "  X***   " 21              MOVE      'N'           $FL1(X)
239800030423A0997X*** ex-A0709       endif
239900941110      *
240000941110      * Dopodichè, a seconda del richiamo e delle autorizzazioni
240100941110      * invalido alcune opzioni
240200941110     C                   SELECT
240300941110      * Se pgm richiamato in ricerca disattivo :
240400941110    >C     OPZ001        WHENEQ    '01'
240500020528A0927X* F10=Immissione
240600020528  "  X*                  Z-ADD     10            X
240700020528A0927X*                  MOVE      '0'           $FC2(X)
240800941110      * F3=Fine
240900941110     C                   Z-ADD     3             X
241000941110     C                   MOVE      '0'           $FC2(X)
241100020528A0927X* 2=Modifica
241200020528  "  X*                  Z-ADD     1             X
241300020528  "  X*    02            LOOKUP    $OP1(X)                                21
241400020528  "  X*    *IN21         IFEQ      *ON
241500020528  "  X*                  MOVE      'N'           $FL1(X)
241600020528  "  X*                  ENDIF
241700020528  "  X* 3=Copia
241800020528  "  X*                  Z-ADD     1             X
241900020528  "  X*    03            LOOKUP    $OP1(X)                                21
242000020528  "  X*    *IN21         IFEQ      *ON
242100020528  "  X*                  MOVE      'N'           $FL1(X)
242200020528  "  X*                  ENDIF
242300020528  "  X* 4=Annullamento
242400020528  "  X*                  Z-ADD     1             X
242500020528  "  X*    04            LOOKUP    $OP1(X)                                21
242600020528  "  X*    *IN21         IFEQ      *ON
242700020528  "  X*                  MOVE      'N'           $FL1(X)
242800020528A0927X*                  ENDIF
242900941110    > * << aggiungere nollo stesso modo le altre opzioni da
243000941110    > *    disattivare (quelle specifiche per ogni pgm) >>
243100941110      * Se pgm richiamato in interrogazione disattivo :
243200941110    >C     OPZ001        WHENEQ    '05'
243300941110      * F10=Immissione
243400941110     C                   Z-ADD     10            X
243500941110     C                   MOVE      '0'           $FC2(X)
243600941110      * 1=Selezione
243700941110     C                   Z-ADD     1             X
243800941110     C     01            LOOKUP    $OP1(X)                                21
243900941110     C     *IN21         IFEQ      *ON
244000941110     C                   MOVE      'N'           $FL1(X)
244100941110     C                   ENDIF
244200941110      * 2=Modifica
244300941110     C                   Z-ADD     1             X
244400941110     C     02            LOOKUP    $OP1(X)                                21
244500941110     C     *IN21         IFEQ      *ON
244600941110     C                   MOVE      'N'           $FL1(X)
244700941110     C                   ENDIF
244800941110      * 3=Copia
244900941110     C                   Z-ADD     1             X
245000941110     C     03            LOOKUP    $OP1(X)                                21
245100941110     C     *IN21         IFEQ      *ON
245200941110     C                   MOVE      'N'           $FL1(X)
245300941110     C                   ENDIF
245400941110      * 4=Annullamento
245500941110     C                   Z-ADD     1             X
245600941110     C     04            LOOKUP    $OP1(X)                                21
245700941110     C     *IN21         IFEQ      *ON
245800941110     C                   MOVE      'N'           $FL1(X)
245900941110     C                   ENDIF
246000941110      *
246100991123     C                   OTHER
246200941110      * Se pgm non richiamato, disattivo subito :
246300941110      * 1=Selezione
246400941110     C                   Z-ADD     1             X
246500941110     C     01            LOOKUP    $OP1(X)                                21
246600941110     C     *IN21         IFEQ      *ON
246700941110     C                   MOVE      'N'           $FL1(X)
246800941110     C                   ENDIF
246900941110      * poi, disattivo a seconda delle autorizzazioni :
247000941110    > *********  AUTXXX    IFEQ 'N'
247100941110    > *********            Z-ADD1         X
247200941110    > *********  'XX'      LOKUP$OP1,X                   21
247300941110    > *********  *IN21     IFEQ *ON
247400941110    > *********            MOVE 'N'       $FL1,X
247500941110    > *********            ENDIF
247600941110    > *********            ENDIF
247700941110      *
247800991123     C                   ENDSL
247900941110      *
248000941110     C                   ENDSR
248100941110     C/EJECT
248200941110      ************************************************************
248300941110      * REPERIMENTO OPZIONI
248400941110      ************************************************************
248500941110     C     REPOPZ        BEGSR
248600941110      *
248700941110     C     1             DO        $DIMOP        X
248800941110      *
248900941110     C     $MS1(X)       IFNE      *BLANK
249000941110      *
249100941110     C                   MOVEL     $MS1(X)       IDMSG
249200941110     C                   CALL      'XRTVM'
249300941110     C                   PARM                    IDMSG            27
249400941110     C                   PARM                    TXTMSG          100
249500941110     C                   PARM                    ERRMSG            1
249600941110      *
249700941110     C     ERRMSG        IFNE      *ON
249800941110     C                   MOVEL     TXTMSG        $TX1(X)
249900941110     C                   ELSE
250000941110     C                   MOVEL     *ALL'?'       $TX1(X)
250100941110     C                   END
250200941110      *
250300941110     C                   END
250400941110      *
250500941110     C                   END
250600941110      *
250700941110     C                   ENDSR
250800941110     C/EJECT
250900941110      ************************************************************
251000941110      * REPERIMENTO TASTI DI FUNZIONE
251100941110      ************************************************************
251200941110     C     REPKEY        BEGSR
251300941110     C                   MOVEA     $FM1          $FM
251400950906     C                   MOVEA     $FC1          $FC
251500941110     C                   EXSR      REPKKK
251600941110     C                   MOVEA     $FX           $FX1
251700941110      *
251800941110     C                   MOVEA     $FM2          $FM
251900950906     C                   MOVEA     $FC2          $FC
252000941110     C                   EXSR      REPKKK
252100941110     C                   MOVEA     $FX           $FX2
252200941110      *
252300941110     C                   MOVEA     $FM3          $FM
252400950906     C                   MOVEA     $FC3          $FC
252500941110     C                   EXSR      REPKKK
252600941110     C                   MOVEA     $FX           $FX3
252700941110     C                   ENDSR
252800941110     C/EJECT
252900941110      ************************************************************
253000941110      * REPERIMENTO TASTI DI FUNZIONE
253100941110      ************************************************************
253200941110     C     REPKKK        BEGSR
253300941110      *
253400941110     C                   CLEAR                   $FX
253500941110     C     1             DO        24            X
253600941110      *
253700941110     C     $FM(X)        IFNE      *BLANK
253800950906     C     $FC(X)        ANDNE     '0'
253900941110      *
254000941110     C                   MOVEL     $FM(X)        IDMSG
254100941110     C                   CALL      'XRTVM'
254200941110     C                   PARM                    IDMSG            27
254300941110     C                   PARM                    TXTMSG          100
254400941110     C                   PARM                    ERRMSG            1
254500941110      *
254600941110     C     ERRMSG        IFNE      *ON
254700941110     C                   MOVEL     TXTMSG        $FX(X)
254800941110     C                   ELSE
254900941110     C                   MOVEL     *ALL' '       $FX(X)
255000941110     C                   END
255100941110      *
255200941110     C                   END
255300941110      *
255400941110     C                   END
255500941110      *
255600941110     C                   ENDSR
255700941110     C/EJECT
255800941110      ************************************************************
255900941110      * DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
256000941110      ************************************************************
256100941110     C     DEFCAM        BEGSR
256200941110      *
256300941110      * klist
256400941110      *
256500941110    > * UTN01L
256600941110    >C     K1UTN1        KLIST
256700941110    >C                   KFLD                    UTNPRF
256800941110      *
256900941110    > * UTN03L
257000941110    >C     K3UTN1        KLIST
257100941110    >C                   KFLD                    UTNGRP
257200941110      * ANMPE02L
257300941110     C     K4MPE2        KLIST
257400941110     C                   KFLD                    MPESOC
257500941110     C                   KFLD                    MPEPGM
257600941110     C                   KFLD                    MPETIP
257700941110     C                   KFLD                    MPEPRF
257800941110      *
257900941110      *
258000941110      * Variabili per gestione videate
258100941110      *
258200941110     C                   MOVE      *BLANK        $GEST             2            fmt in gestione
258300941110     C                   MOVE      *BLANK        $FINE             1            fine pgm
258400941110     C                   MOVE      *BLANK        $INZD1            1            inizializzione D1
258500941110     C                   MOVE      *BLANK        $INZS1            1            inizializzione S1
258600941110     C                   MOVE      *BLANK        $INZD2            1            inizializzione D2
258700941110     C                   MOVE      *BLANK        $LASTG            2            fmt di provenienza
258800941110     C                   MOVE      *BLANK        $LASTV            2            ultima emissione
258900941110     C                   MOVE      *BLANK        $EFILE            1            memo fine file
259000941110     C                   MOVE      *BLANK        $ESCI             1            rich.uscita ciclo
259100941110     C                   MOVE      *BLANK        $RCDOK            1            rcd letto valido
259200941110     C                   Z-ADD     0             $PIED1            3 0          piede da emett. D1
259300941110     C                   Z-ADD     0             $PIES1            3 0          piede da emett. S1
259400941110      *
259500941110      * Variabili appoggio sempre presenti in tutte le anagrafiche
259600941110      *
259700941110     C     *LIKE         DEFINE    C1RCD         S1NRR
259800941110     C     *LIKE         DEFINE    C1NRR         WSFL
259900941110     C     *LIKE         DEFINE    C1RCD         WMAX
260000941110     C     *LIKE         DEFINE    C1RCD         WPAG
260100941110     C     *LIKE         DEFINE    $INZS1        WINZS1                                     ne S1
260200941110     C     *LIKE         DEFINE    *INKR         WINKR                                      ne S1
260300941110     C                   MOVE      *BLANK        WWGRP
260400941110     C                   Z-ADD     0             CURR              2 0
260500941110     C                   Z-ADD     0             CURC              2 0
260600941110     C                   MOVE      *BLANK        WIN              99
260700941110     C     *LIKE         DEFINE    C1POS         W1POS
260800941110     C     *LIKE         DEFINE    C1POS2        W1POS2
260900941110      *
261000941110      * Variabili appoggio specifiche della singola anagrafica
261100941110      *
261200941110      * campi per gestione F4=Ricerca
261300941110    >C     *LIKE         DEFINE    UTNGRP        WWGRP
261400150909>>>>>C     *LIKE         DEFINE    UTNUFF        WWUFF
261401150909      * campi per parzializzazioni su rcd (solo alfanumerici)
261500941110    >C     *LIKE         DEFINE    UTNPRF        PRF999
261600941110      *
261700941110      * Indici
261800941110      *
261900941110     C                   Z-ADD     0             X                 3 0
262000941110     C                   Z-ADD     0             Y                 3 0
262100941110      *
262200941110     C                   ENDSR
262300941110     C/EJECT
262400941110    > ************************************************************
262500941110    > * ACCESSO A TABELLE ANTAB00F
262600941110    > ************************************************************
262700941110    >C     LETTAB        BEGSR
262800941110    > *
262900941110    > * Input :
263000941110    > * - TABGRU/TABCOD = chiave univoca ANTAB00F
263100941110    > * Output :
263200941110    > * - WTBERR = flag segnalazione errore reperimento tabella
263300941110    > *
263400941110    >C                   CALL      'XATB'
263500941110    >C                   PARM                    XATBDS
263600941110    > *
263700941110    >C                   MOVE      *OFF          WTBERR
263800941110    >C     XTBERR        IFEQ      *ON
263900941110    >C                   MOVE      *BLANK        XTBUNI
264000941110    >C                   MOVE      *ON           WTBERR            1
264100941110    >C                   ENDIF
264200941110    > *
264300941110    >C                   ENDSR
264400941110     C/EJECT
264500950906**
264600940718PROMSG    *LIBL     OPZ0001
264700940225PROMSG    *LIBL     OPZ0002
264800940131PROMSG    *LIBL     OPZ0003
264900940131PROMSG    *LIBL     OPZ0004
265000940131PROMSG    *LIBL     OPZ0005
265100991116PROMSG    *LIBL     OPZ0347
265200940201**
26530094022501S      <Selezione
26540094022502S      <Modifica
26550094022503S      <Copia
26560094022504S      <Annullamento
26570094022505S      <Visualizzazione
26580099111611S      <Utenti remoti
265900940506** TASTI DI FUNZIONE  D1
266000940506PROMSG    *LIBL     KEY0001  01
266100940506PROMSG    *LIBL     KEY0002  02
266200940506PROMSG    *LIBL     KEY0003  03
266300940506PROMSG    *LIBL     KEY0004  04
266400940506PROMSG    *LIBL     KEY0005  05
266500940506PROMSG    *LIBL     KEY0006  06
266600940506PROMSG    *LIBL     KEY0007  07
266700940506PROMSG    *LIBL     KEY0008  08
266800940506PROMSG    *LIBL     KEY0009  09
266900940506PROMSG    *LIBL     KEY0010  10
267000940506PROMSG    *LIBL     KEY0011  11
267100940506PROMSG    *LIBL     KEY0012  12
267200940506PROMSG    *LIBL     KEY0013  13
267300940506PROMSG    *LIBL     KEY0014  14
267400940506PROMSG    *LIBL     KEY0015  15
267500940506PROMSG    *LIBL     KEY0016  16
267600940506PROMSG    *LIBL     KEY0017  17
267700940506PROMSG    *LIBL     KEY0018  18
267800940506PROMSG    *LIBL     KEY0019  19
267900940506PROMSG    *LIBL     KEY0020  20
268000940506PROMSG    *LIBL     KEY0021  21
268100940506PROMSG    *LIBL     KEY0022  22
268200940506PROMSG    *LIBL     KEY0023  23
268300940506PROMSG    *LIBL     KEY0024  24
268400940506**  ABILITAZIONE D1
2685009407200N  01
2686009407200N  02
2687009407201N  03
2688009407201N  04
2689009407200N  05
2690009407200N  06
2691009407200N  07
2692009407200N  08
2693009407200N  09
2694009407200N  10
2695009407200N  11
2696009407201N  12
2697009407200N  13
2698009407200N  14
2699009407200N  15
2700009407200N  16
2701009407200N  17
2702009407201N  18
2703009407200N  19
2704009407200N  20
2705009407200N  21
2706009407200N  22
2707009509060N  23   Impostare a '1' se si utilizzano opzioni
2708009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
270900940916** TASTI DI FUNZIONE  S1
271000940916PROMSG    *LIBL     KEY0001  01
271100940916PROMSG    *LIBL     KEY0002  02
271200940916PROMSG    *LIBL     KEY0003  03
271300940916PROMSG    *LIBL     KEY0004  04
271400940916PROMSG    *LIBL     KEY0005  05
271500940916PROMSG    *LIBL     KEY0006  06
271600940916PROMSG    *LIBL     KEY0007  07
271700940916PROMSG    *LIBL     KEY0008  08
271800940916PROMSG    *LIBL     KEY0009  09
271900940916PROMSG    *LIBL     KEY0010  10
272000940916PROMSG    *LIBL     KEY0011  11
272100940916PROMSG    *LIBL     KEY0012  12
272200940916PROMSG    *LIBL     KEY0013  13
272300940916PROMSG    *LIBL     KEY0014  14
272400940916PROMSG    *LIBL     KEY0015  15
272500940916PROMSG    *LIBL     KEY0016  16
272600940916PROMSG    *LIBL     KEY0017  17
272700940916PROMSG    *LIBL     KEY0018  18
272800940916PROMSG    *LIBL     KEY0019  19
272900940916PROMSG    *LIBL     KEY0020  20
273000940916PROMSG    *LIBL     KEY0021  21
273100940916PROMSG    *LIBL     KEY0022  22
273200940916PROMSG    *LIBL     KEY0023  23
273300940916PROMSG    *LIBL     KEY0024  24
273400940916**  ABILITAZIONE S1
2735009409160N  01
2736009409160N  02
2737009409161N  03
2738009409160N  04
2739009409160N  05
2740009409160N  06
2741009409160N  07
2742009409160N  08
2743009409160N  09
2744009409161N  10
2745009409160N  11
2746009409161N  12
2747000207051N  13                                                                                 ### B0453 ###
2748009409160N  14
2749009409160N  15
2750009409160N  16
2751009409161N  17
2752009409161N  18
2753009409160N  19
2754009409160N  20
2755009409160N  21
2756009409160N  22
2757009509060N  23   Impostare a '1' se si utilizzano opzioni
2758009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
275900940916** TASTI DI FUNZIONE  D3
276000940506PROMSG    *LIBL     KEY0001  01
276100940506PROMSG    *LIBL     KEY0002  02
276200940506PROMSG    *LIBL     KEY0003  03
276300940506PROMSG    *LIBL     KEY0004  04
276400940506PROMSG    *LIBL     KEY0005  05
276500940506PROMSG    *LIBL     KEY0006  06
276600940506PROMSG    *LIBL     KEY0007  07
276700940506PROMSG    *LIBL     KEY0008  08
276800940506PROMSG    *LIBL     KEY0009  09
276900940506PROMSG    *LIBL     KEY0010  10
277000940506PROMSG    *LIBL     KEY0011  11
277100940506PROMSG    *LIBL     KEY0012  12
277200940506PROMSG    *LIBL     KEY0013  13
277300940506PROMSG    *LIBL     KEY0014  14
277400940506PROMSG    *LIBL     KEY0015  15
277500940506PROMSG    *LIBL     KEY0016  16
277600940506PROMSG    *LIBL     KEY0017  17
277700940506PROMSG    *LIBL     KEY0018  18
277800940506PROMSG    *LIBL     KEY0019  19
277900940506PROMSG    *LIBL     KEY0020  20
278000940506PROMSG    *LIBL     KEY0021  21
278100940506PROMSG    *LIBL     KEY0022  22
278200940506PROMSG    *LIBL     KEY0023  23
278300940506PROMSG    *LIBL     KEY0024  24
278400940916**  ABILITAZIONE D3
2785009407200N  01
2786009407200N  02
2787009407201N  03
2788009407200N  04
2789009407200N  05
2790009407200N  06
2791009407200N  07
2792009407200N  08
2793009407200N  09
2794009409160N  10
2795009407200N  11
2796009407201N  12
2797009407200N  13
2798009407200N  14
2799009407200N  15
2800009407200N  16
2801009409160N  17
2802009409160N  18
2803009407200N  19
2804009407200N  20
2805009407200N  21
2806009407200N  22
2807009509060N  23   Impostare a '1' se si utilizzano opzioni
2808009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
