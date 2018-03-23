000100991126     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) ACTGRP(PNOTA)
000200981214     H*PARMS BNDDIR(PJXBND PJCBND)
000300960109     H*PARMS CVTOPT(*DATETIME)
000400940211     H DECEDIT('0,') DATEDIT(*DMY.)
000500941115      *
000600960118    > *            gestione manuale prima nota batch
000700960109      *           -----------------------------------
000800940224      *
000900940224      *
001000940224      * DOCUMENTI MICROANALISI :
001100940224      *
001200940224      * NOTE TECNICHE :
001300940224      *
001400940224      *
001500940224      * PERSONALIZZAZIONI
001600940224      *
001700940224      *
001800940224      *
001900940314      *               MAPPA INDICATORI
002000960110      *  11           condiziona emissione unita
002100960110      * N12           posizionamento per data
002200960110      *  13           condizione emissione cliente
002300940307      *  21           GENERICO OPERAZIONI I/O
002400940224      *  22           GENERICO ERRORE OPERAZIONI I/O
002500940224      *  30           SFLDSP
002600940224      * N31           SFLCLR
002700940224      *  31           SFLDSPCTL
002800940224      *  32           SFLNXTCHG
002900940224      *  33           SFLEND
003000940224      *  39           OF PRTF
003100940224      *  40 <---> 49  DSPATR ERRORI SU SFL
003200960109      *  50 <---> 89  ERRORI SU VIDEO
003300960109      *  elenco errori speciali:
003400960109      *  91           OPZIONE NON VALIDA
003500960109      *  92           IMPOSSIBILE RIPOSIZIONAMENTO CON OPZIONI PRESENTI SU LISTA
003600960109      *  93           LA FUNZIONE RICHIAMATA SI E' CHIUSA IN ERRORE
003700960109      *  95           L'OPERAZIONE DI SCRITTURA NON E' ANDATA A BUON FINE
003800960109      *  94           UTENTE NON AUTORIZZATO ALLA FUNZIONE NEL MODO DESIDERATO
003900960109      *  96           ESISTONO ERRORI IN ALTRI SCHERMI
004000960109      *  97           TASTO   NON ABILITATO
004100960109      *  98           RICERCA NON ABIL. NELLA POSIZ.
004200960109      *  99           INDIC. GENERALE DI ERRORE
004300940314    >F*----------------------------------------------------------------
004400940314    >F* Viste logiche utilizzate per i diversi ordinamenti
004500940314    >F*----------------------------------------------------------------
004600940909     F*------------
004700960118    >FNDBHM02L  IF   E           K DISK
004800960118    >F                                     RENAME(NDBHM000:NDBHM02)
004900940211     F*------------
005000960118    >FNDC077D   CF   E             WORKSTN
005100960118     F                                     RENAME(C077D1:FMTD1)
005200960118     F                                     RENAME(C077D2:FMTD2)
005300960118     F                                     RENAME(C077D3:FMTD3)
005400960118     F                                     RENAME(C077S1:FMTS1)
005500960118     F                                     RENAME(C077C1:FMTC1)
005600960118     F                                     RENAME(C077T1:FMTT1)
005700960118     F                                     RENAME(C077Z1:FMTZ1)
005800960118     F                                     RENAME(C077Z2:FMTZ2)
005900960118     F                                     RENAME(C077Z3:FMTZ3)
006000940131     F                                     SFILE(FMTS1:S1NRR)
006100940201     F                                     INFDS(DSFMT)
006200940128     D*----------------------------------------------------*
006300940302     D* Schiere per la gestione delle opzioni
006400950614     D* dimensione della schiera $MS1
006500980720     D $DIMOP          C                   CONST(30)
006600950524     D $MS1            S             27    DIM($dimop) CTDATA PERRCD(1)
006700950524     D $TX1            S             50    DIM($dimop) ALT($MS1)
006800950524     D $OP1            S              2  0 DIM($dimop) CTDATA PERRCD(1)
006900950524     D $FL1            S              1    DIM($dimop) ALT($OP1)
007000941214     D $TX             S             50    DIM(50)
007100941214     D $FL             S              1    DIM(50)
007200980720     D* schiera contenente i nomi dei programma richiamati attraverso
007300980720     D* le opzioni personalizzate
007400980720     D $PGO1           S             10    DIM($dimop)
007500980720     D*
007600980720     D* Indice della schiera per le opzioni personalizzate
007700980720     D PosOpz          S              2  0
007800980720     D* numero max opzioni
007900980720     D NumOpz          S              2  0 Inz($DIMOP)
008000980720     D* numero effettive opzioni
008100980720     D OpzLen          S              2  0
008200001122d0771D*-------------
008300001122  "  D* Video S1: $OpzOk = *on = opzione ok
008400001122  "  D $OpzOk          S              1
008500001122  "  D WC1Rcd          S                   like(c1nrr) inz
008600001122  "  D WOpz            S                   like(S1Opz) inz
008700001122d0771D $X              S                   like(c1nrr) inz
008800980720     D*-------------
008900940909     D* Schiere generali per la gestione dei tasti di funzione
009000940506     D $FM             S             27    DIM(24)
009100940506     D $FX             S             50    DIM(24)
009200940506     D $FC             S              1    DIM(24)
009300940506     D $FR             S              1    DIM(24)
009400980720     D* Indice della schiera tasti funzione
009500980720     D PosFun          S              2  0
009600980720     D*-------------
009700940909     D* Schiere per la gestione dei tasti di funzione
009800940506     D* Video D1
009900940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
010000940506     D $FX1            S             50    DIM(24) ALT($FM1)
010100940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
010200940506     D $FR1            S              1    DIM(24) ALT($FC1)
010300940506     D* Schiere per la gestione dei tasti di funzione
010400940506     D* Video S1
010500940506     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
010600940506     D $FX2            S             50    DIM(24) ALT($FM2)
010700940506     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
010800940506     D $FR2            S              1    DIM(24) ALT($FC2)
010900980720     D* Schiera contente i nomi dei programmi da richiamare con i tasti
011000980720     D* funzione personalizzata S1
011100980720     D $PGF2           S             10    DIM(25)
011200940916     D* Schiere per la gestione dei tasti di funzione
011300940916     D* Video D2
011400940916     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
011500940916     D $FX3            S             50    DIM(24) ALT($FM3)
011600940916     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
011700940916     D $FR3            S              1    DIM(24) ALT($FC3)
011800960119     D*-------------
011900960119     D* DS Esterna gestione messaggi per COMIT
012000960119     D XMSGDS        E DS
012100960119     D  STS          E                     EXTFLD(MSGSTS)
012200960119     D                                     DIM(333)
012300960119     D  JBA          E                     EXTFLD(MSGJBA)
012400960119     D                                     DIM(502)
012500001127d0771D  MSGRSP       E                     INZ('S')
012600001127  "  D  MSGTPR       E                     INZ('SN')
012700001127  "  D  MSGEMV       E                     INZ('S')
012800001127  "  D  MSGMSG       E                     INZ(0)
012900001127  "  D  MSGRGP       E                     INZ(10)
013000001127  "  D  MSGCNL       E                     INZ('N')
013100001127  "  D  MSGVID       E                     INZ('N')
013200001127  "  D  MSGOPE       E                     INZ('N')
013300001127  "  D  MSGSTP       E                     INZ('N')
013400001127  "  D  MSGLCK       E                     INZ('N')
013500001127d0771D*-------------
013600021016C1579D* Parametri utilizzati per chiamata al driver PJC00701R
013700021016  "  D* di annullamento raccordo per c/c di pool
013800021016  "  D §Opzione        S              2
013900021016  "  D §Societa        S              3A
014000021016  "  D §SysReg         S              3P 0
014100021016  "  D §NrAsReg        S              9P 0
014200021016  "  D §NrRigaM        S              5P 0
014300021016  "  D §RifInt         S              9A
014400021016  "  D §SubNum         S              9P 0
014500021016  "  D §Errore         S              1
014600021016  "  D §Dati           S           4000
014700021016  "  D §Lunghezza      S              9B 0
014800021016  "  D §Commit         S              1
014900021016  "  D*----------------
015000021016  "   * DS passaggio dati
015100021016  "  D NDCPRDS       E DS                  Inz ExtName(NDCPR00F)
015200021016C1579D*----------------
015300950726     D*
015400950726     D $D1L01          S              3  0
015500950726     D $D1L02          S              3  0
015600950726     D $S1L01          S              3  0
015700950726     D $S1L02          S              3  0
015800950906     D $C1Op1          S              3  0
015900950906     D $C1Op2          S              3  0
016000950906     D $D2L01          S              3  0
016100950906     D $D2L02          S              3  0
016200940302     D*
016300940128     D*----------------------------------------------------*
016400940211     D*-------------
016500940207     D* Valori assunti
016600950524     D XMPEDS        E DS                  INZ
016700950524     D XMPEDS1       E DS                  INZ
016800941111     D DSMPE           DS                  INZ
016900940207     D  $LOP                   1      1  0
017000940831     D  $ORD                   2      3  0
017100940207     D*-------------
017200940211     D* Passaggio Parametri
017300940211     D KPJBA         E DS
017400940211     D*-------------
017500940225     D* Reperimento Autorizzazioni
017600941124     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
017700941124     D*-------------
017800950410     D* Passaggio Autorizzazioni
017900950410     D DSAUT           DS           512
018000950524     D*-------------
018100950524     D* Maschera dati autorizzazioni
018200960118    >DCONPNODS       E DS
018300960109     D*-------------
018400941209     D* Dati di ambiente ottenuti da XSOC
018500941209    >D SOC001        E DS                  EXTNAME(XSOC001DS)
018600960109     D    FgO1                        1    overlay(XscFgO)
018700960118     D*-------------
018800960118     D XSOCDS          DS          1000
018900960118     D*-------------
019000980618     D* Ricerca lotti contabilizzazione
019100980618    >D NDC070DS1     E DS
019200980618    >D  OPZ0701      E                     INZ('01')
019300980618    >D  ERR0701      E                     INZ('0')
019400980618     D*-------------
019500960118     D* Ricerca causale
019600960118     D ANA070DS      E DS                  INZ
019700960118     D  RET070       E                     INZ('0')
019800960118     D  OPR070       E                     INZ('0')
019900960118     D  CMT070       E                     INZ('0')
020000960118     D  ERR070       E                     INZ('0')
020100960118     D  FSC070       E                     INZ('S')
020200960118     D*-------------
020300960118     D* Causali operative
020400960118     D ANOPE         E DS                  EXTNAME(ANOPE00F)
020500960118     D  OPEDTANN     E                      inz(datmin)
020600960118     D  OPEDTIMM     E                      inz(DATMIN)
020700960118     D  OPEDTULMO    E                      inz(Datmin)
020800960116     D*-------------
020900960116     D* Ricerca legende
021000960118     D ANA964DS      E DS                  INZ
021100960116     D*-------------
021200960116     D* Controlli validità legende
021300960118     D X07VALDS      E DS                  INZ
021400940325     D*-------------
021500940325     D* Parametri in ricezione
021600960118    >D NDC077DS      E DS
021700940325     D*-------------
021800980720     D* campo di salvataggio per la DS d'ingresso durante la chiamata
021900980720     D* alla funzione personalizzata
022000980720    >D $SaveDS         S                   like(ndc077ds)
022100980720     D*-------------
022200960118     D* Parametri del PGM di dettaglio    NDC078R
022300960118    >D NDC078DS      E DS                  inz
022400960118    >D  RET078       E                     INZ('0')
022500960118    >D  ERR078       E                     INZ('0')
022600960119     D*-------------
022700960119     D* Parametri per gestione p.nota interattiva
022800960119     D NDC071DS      E DS                  INZ
022900960119    >D  ERR071       E                     INZ('0')
023000960119     D*-------------
023100960119     D* Parametri per stampa errori/registrazioni
023200960119     D NDC072DS      E DS                  INZ
023300960119     D  ERRori072    E                     INZ('0')
023400960118     D*-------------
023500960118     D* Definizioni parametri standard chiamate moduli
023600960118     D$Esito           S              1
023700960118     D$Refresh         S              1
023800960118     D$gesmsg          S              1
023900960118     D$lenout          S              9B 0
024000960118     D$struttura       S             10
024100940211     D*-------------
024200940211     D DSFMT           DS           512
024300940506     D  $TASTO               369    369
024400940211     D  NRG                  370    370
024500940211     D  NCL                  371    371
024600940211     D  SFLNRR               378    379B 0
024700940211     D*-------------
024800001127d0771D*tasto precedente = F13=selezione multipla
024900001127  "  D  $TastoSav      s                   Like($Tasto) inz
025000001127  "  D*emetto al window di conf.canc. solo x il 1° rec.da cancellare
025100001127  "  D  PrimaVolta     s              1  0 inz(0)
025200001127d0771D*-------------
025300940211     D* posizione cursore
025400940211     D CURSOR          DS
025500940211     D  RIRI                   1      2B 0 INZ
025600940211     D  R$$                    2      2
025700940211     D  COCO                   3      4B 0 INZ
025800940211     D  C$$                    4      4
025900940127     D*-------------
026000940127     D* Reperimento nome PGM
026100940127     D STATUS         SDS           333
026200940127     D  DSPGM            *PROC
026300950411     D  PARMS            *PARMS
026400940207     D*-------------
026500940616     D* Costanti
026600940616     D* nr.di righe del sfl (SFLPAG)
026700001122d0771D* lo imposto =10 in InzVar e temporaneam. = 9999 in F13S1
026800001122  "  x*SFLPAG          C                   CONST(10)
026900001122d0771D SFLPAG          s              4s 0
027000940616     D*-------------
027100940506     D* Tasti di funzione
027200940506     D F01             C                   CONST(X'31')
027300940506     D F02             C                   CONST(X'32')
027400940506     D F03             C                   CONST(X'33')
027500940506     D F04             C                   CONST(X'34')
027600940506     D F05             C                   CONST(X'35')
027700940506     D F06             C                   CONST(X'36')
027800940506     D F07             C                   CONST(X'37')
027900940506     D F08             C                   CONST(X'38')
028000940506     D F09             C                   CONST(X'39')
028100940506     D F10             C                   CONST(X'3A')
028200940506     D F11             C                   CONST(X'3B')
028300940506     D F12             C                   CONST(X'3C')
028400940506     D F13             C                   CONST(X'B1')
028500940506     D F14             C                   CONST(X'B2')
028600940506     D F15             C                   CONST(X'B3')
028700940506     D F16             C                   CONST(X'B4')
028800940506     D F17             C                   CONST(X'B5')
028900940506     D F18             C                   CONST(X'B6')
029000940506     D F19             C                   CONST(X'B7')
029100940506     D F20             C                   CONST(X'B8')
029200940506     D F21             C                   CONST(X'B9')
029300940506     D F22             C                   CONST(X'BA')
029400940506     D F23             C                   CONST(X'BB')
029500940506     D F24             C                   CONST(X'BC')
029600940506     D ENTER           C                   CONST(X'F1')
029700940506     D ROLDWN          C                   CONST(X'F4')
029800940506     D ROLLUP          C                   CONST(X'F5')
029900950524     D*-------------
030000950524     D* Sottotitolo di testata
030100960109     D SOTTIT          C                   CONST('              ')
030200960109     D*-------------
030300960109     D DATMIN          C                   CONST('0001-01-01')
030400960109     D DATMAX          C                   CONST('9999-12-31')
030500950524     D*-------------
030600950606     D* Variabili per gestione videate
030700950606     D $Gest           S              2A
030800950606     D $Fine           S              1A
030900950606     D $InzD1          S              1A
031000950606     D $InzS1          S              1A
031100950606     D $InzD2          S              1A
031200950606     D $LastG          S              2A
031300950606     D $LastV          S              2A
031400950606     D $EndFile        S              1A
031500950606     D $Esci           S              1A
031600950606     D $RcdOK          S              1A
031700950606     D $Unique         S              1A
031800960110     D $date           S             10
031900950606     D*-------------
032000950606     D* Variabili appoggio sempre presenti in tutte le anagrafiche
032100950606     D S1Nrr           S                   like(C1Rcd)
032200950606     D WSfl            S                   like(C1Nrr)
032300950606     D WMax            S                   like(C1Rcd)
032400950606     D WPag            S                   like(C1Rcd)
032500950606     D WInzS1          S                   like($InzS1)
032600960118    >D WOpr            S                   like(Opr077)
032700950606     D CurR            S              2S 0
032800950606     D CurC            S              2S 0
032900950606     D Win             S             99A
033000960109     D W1PosD          S                   like(C1PosD)
033100950606     D WINKR           S              1A
033200960119     D Wxmsginz        S              1A
033300960109     D*-------------
033400960109     D Wdate           S             10
033500960118     D DtRegDaR        S             10
033600960118     D dtRegAR         S             10
033700950606     D*-------------
033800950606     D*  Indici
033900950606     D X               S              3S 0
034000950606     D Y               S              3S 0
034100950606     D*-------------
034200000118C1309D ZDateISO        S               D
034300050228     D*-------------
034400050228D1740D* Chiamata ricorsiva
034500050228D1740D X48CHPGMDS    E DS                  INZ
034600940127     C*----------------------------------------------------*
034700940127     C*                MAIN LINE PROGRAM
034800940127     C*----------------------------------------------------*
034900940127     C*
035000940223     C* inizializzazione variabili
035100940223     C                   EXSR      INZVAR
035200940223     C*
035300940223     C     $FINE         DOWEQ     *OFF
035400940131     C     $GEST         CASEQ     'D1'          GESD1                          Parzializz.
035500940131     C     $GEST         CASEQ     'S1'          GESS1                          Lista
035600940131     C     $GEST         CASEQ     'D2'          GESD2                          Valori ass.
035700941027     C                   ENDCS
035800941027     C                   ENDDO
035900940325     C* fine programma
036000940325     C                   EXSR      ENDPGM
036100940225     C*
036200940325     C/EJECT
036300940325     C************************************************************
036400940325     C* FINE PROGRAMMA
036500940325     C************************************************************
036600940325     C     ENDPGM        BEGSR
036700940325     C*
036800940325     C* passaggio dati al pgm chiamante
036900960118    >C                   MOVEL     WOPR          OPR077                                     ne S1
037000960118    >C                   MOVEL     NDC077DS      KPJBU
037100940325     C*
037200940325     C* Se pgm Guida non è stato richiamato posso chiudere in LR
037300960118    >C     OPZ077        IFEQ      *BLANK
037400940325     C                   SETON                                            LR
037500940325     C* altrimenti chiudo in RT
037600940325     C                   ELSE
037700940325     C                   SETON                                            RT
037800940325     C                   ENDIF
037900940616     C                   RETURN
038000940325     C*
038100940325     C                   ENDSR
038200940224     C/EJECT
038300940201     C************************************************************
038400940201     C* RICERCHE
038500940201     C************************************************************
038600940127     C     SEARCH        BEGSR
038700940127     C*
038800940127     C* determino Riga/Colonna del cursore
038900940127     C                   MOVE      NRG           R$$
039000940127     C                   MOVE      NCL           C$$
039100940127     C                   Z-ADD     RIRI          CURR
039200940127     C                   Z-ADD     COCO          CURC
039300960109     C*
039400941118   B1C                   SELECT
039500980618     C*
039600960118     C* FMTD1 - tipo registrazione
039700960118     C     H1NMFL        WHENEQ    'D1TPREGZ'
039800960118     C                   RESET                   ANA964DS
039900960118     C                   MOVE      '01'          OPZ964
040000960118     C                   MOVEL     'NDREG000'    TRC964
040100960118     C                   MOVEL     'REGTPREGZ'   CAM964
040200960118     C                   MOVEL     *on           ALL964
040300960118     C                   CLEAR                   KPJBU
040400960118     C                   MOVEL     ANA964DS      KPJBU
040500960118     C                   CALL      'ANA964R'
040600960118     C                   PARM                    KPJBA
040700960118     C                   MOVEL     KPJBU         ANA964DS
040800960118     C     Val964        IFNE      *BLANK
040900960118     C                   MOVEL     Val964        D1TpRegz
041000960118     C                   MOVEL     Des964        D1TpRegzD
041100960118     C                   ENDIF
041200960118     c*
041300960118     C* FMTD1 - Causale principale
041400960118     C     H1NMFL        WHENEQ    'D1CAUSTES'
041500960118     C                   RESET                   ANA070DS
041600960118     C                   MOVE      '01'          OPZ070
041700960118     C                   CLEAR                   KPJBU
041800960118     C                   MOVEL     ANA070DS      KPJBU
041900960118     C                   CALL      'ANA070R'
042000960118     C                   PARM                    KPJBA
042100960118     C                   MOVEL     KPJBU         ANA070DS
042200960118     C     Cau070        IFNE      *BLANK
042300960118     C                   MOVEL     Cau070        D1CausTes
042400960118     C                   ENDIF
042500980618     C*
042600980618    >C* FMTD1 - Lotto
042700980618     C     H1NMFL        WHENEQ    'D1RIFINT'
042800980618     C                   RESET                   NDC070DS1
042900980618     c                   MOVE      XSCSoc        Soc0701
043000980618     c                   MOVE      '1'           Inz0701
043100980618    >C                   MOVEL     NDC070DS1     KPJBU
043200980618    >C                   CALL      'NDC070R1'
043300980618     C                   PARM                    KPJBA
043400980618     C                   MOVEL     KPJBU         NDC070DS1
043500980623     C     RET0701       IFEQ      *BLANK
043600980618     C                   MOVEL     Rif0701       D1RifInt
043700980618     C                   ENDIF
043800940223     C*
043900940223     C* ricerca non abilitata in questa posizione
044000941118   X1C                   OTHER
044100940929     C                   SETON                                        9899
044200940127     C*
044300941118   E1C                   ENDSL
044400950612     C*
044500950612     C* imposto pos. del cursore
044600950612     C                   Z-ADD     CURR          H1RIGA
044700950612     C                   Z-ADD     CURC          H1COLO
044800940127     C*
044900940506     C                   MOVE      *HIVAL        $LASTV
045000950612     C*
045100940127     C                   ENDSR
045200940131     C/EJECT
045300940224     C************************************************************
045400940131     C* RIEMPIMENTO OPZIONI
045500940224     C************************************************************
045600940131     C     RIEOPZ        BEGSR
045700940223     C*
045800940223     C* Input :
045900940223     C* - $TX = schiera con tutti i testi disponibili
046000940223     C* - $FL = schiere con i flag di validità associati ai testi
046100940225     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
046200940223     C*          opzione usato
046300940223     C* - WMXO = variabile contenente il numero massimo di testi
046400940223     C*          opzione usabili
046500940721     C* - $L01 = Lunghezza prima riga
046600940721     C* - $L02 = Lunghezza seconda riga
046700940223     C* Output :
046800940223     C* - WOPZ1 = variabile contenente la prima riga opzioni
046900940223     C* - WOPZ2 = variabile contenente la seconda riga opzioni
047000941214     C* - FF23  = flag che indica se c'è bisogno del tasto F23
047100940223     C*
047200950522     C                   CALLB     'X04OPMSG'
047300940223     C* input
047400940223     C                   PARM                    $TX
047500940223     C                   PARM                    $FL
047600940225     C                   PARM                    WCOZ              3 0
047700940223     C                   PARM                    WMXO              3 0
047800940223     C* output
047900940223     C                   PARM                    WOPZ1            76
048000940223     C                   PARM                    WOPZ2            76
048100941214     C                   PARM                    FF23              1
048200950301     C* input
048300950726     C                   PARM                    $L01              3 0
048400950726     C                   PARM                    $L02              3 0
048500940223     C*
048600060224C1913C                   PARM                    $ALLOPT         960
048700060224C1913C*
048800940131     C                   ENDSR
048900940506     C/EJECT
049000940506     C************************************************************
049100940506     C* RIEMPIMENTO TASTI FUNZIONE
049200940506     C************************************************************
049300940506     C     RIEKEY        BEGSR
049400940506     C*
049500940506     C* Input :
049600940506     C* - $FX = schiera con tutti i testi disponibili
049700940506     C* - $FC = schiere con i flag di validità associati ai testi
049800940506     C* - $FR = schiere con i flag di REVERSE IMAGE
049900940506     C* - $ULK = ultimo testo caricato
050000940721     C* - $L01 = Lunghezza prima riga
050100940721     C* - $L02 = Lunghezza seconda riga
050200940506     C* Output :
050300940506     C* - $ULK
050400940609     C* - $KEY1= 1° riga di tasti funzione
050500940609     C* - $KEY2= 2° riga di tasti funzione
050600940506     C*
050700950522     C                   CALLB     'XKEYMSG'
050800940506     C* input
050900940506     C                   PARM                    $FX
051000940506     C                   PARM                    $FC
051100940506     C                   PARM                    $FR
051200940506     C                   PARM                    $ULK              3 0
051300940506     C* output
051400940506     C                   PARM                    $KEY1            79
051500940506     C                   PARM                    $KEY2            79
051600950301     C* input
051700950726     C                   PARM                    $L01              3 0
051800950726     C                   PARM                    $L02              3 0
051900940506     C*
052000060224C1913C                   PARM                    $ALLFUNCT       480
052100060224C1913C*
052200940506     C                   ENDSR
052300940224     C/EJECT
052400940224     C************************************************************
052500940224     C* GESTIONE EMISSIONE TASTI FUNZIONE
052600940224     C************************************************************
052700940224     C     GESPIE        BEGSR
052800940224     C*
052900940224     C                   SELECT
053000940224     C* Parzializzazioni
053100940224     C     $GEST         WHENEQ    'D1'
053200940506     C     $ULKD1        IFEQ      0
053300940506     C* riempimento opzioni
053400940506     C                   EXSR      F24D1
053500940506     C                   ELSE
053600940506     C                   WRITE     FMTZ1
053700940506     C                   ENDIF
053800940506     C* Lista
053900940506     C     $GEST         WHENEQ    'S1'
054000940506     C     $ULKS1        IFEQ      0
054100940506     C                   EXSR      F24S1
054200940506     C                   ELSE
054300940506     C                   WRITE     FMTZ2
054400940506     C                   ENDIF
054500940916     C* Modifica valori assunti
054600940916     C     $GEST         WHENEQ    'D2'
054700940916     C     $ULKD2        IFEQ      0
054800940916     C                   EXSR      F24D2
054900940916     C                   ELSE
055000940916     C                   WRITE     FMTZ3
055100940916     C                   ENDIF
055200940506     C                   ENDSL
055300940506     C*
055400940224     C                   ENDSR
055500940224     C/EJECT
055600940224     C************************************************************
055700940224     C* GESTIONE PARZIALIZZAZIONI
055800940224     C************************************************************
055900940224     C     GESD1         BEGSR
056000940224     C* inizializzazione videata
056100940223     C     $INZD1        IFEQ      *ON
056200940127     C                   EXSR      INZD1
056300940223     C                   MOVE      *OFF          $INZD1
056400941027     C                   ENDIF
056500940223     C* emissione piede videata se proveniente da altra
056600940223     C* salvataggio valore formato di provenienza
056700940202     C     $LASTV        IFNE      'D1'
056800940223     C* salvo stato F17 per sapere cosa fare in caso di F12
056900940223     C                   MOVE      *INKR         WINKR
057000940202     C                   WRITE     FMTT1
057100940224     C                   EXSR      GESPIE
057200940202     C                   MOVE      $LASTV        $LASTG
057300940202     C                   MOVE      'D1'          $LASTV
057400940707     C                   ENDIF
057500940707     C*
057600940707     C* se esistono errori sulla videata
057700940707     C* emetto la write del formato a indicatori spenti per vedere
057800940707     C* le eventuali decodifiche
057900940707     C     *IN99         IFEQ      *ON
058000940707     C                   MOVEA     *IN           WIN
058100940707     C                   MOVE      *ALL'0'       IN5098           49
058200940707     C                   MOVEA     IN5098        *IN(50)
058300940707     C                   WRITE     FMTD1
058400940707     C                   MOVEA     WIN           *IN
058500940707     C                   ENDIF
058600940131     C*
058700940131     C                   EXFMT     FMTD1
058800940506     C*
058900951005     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
059000951005     C* INPUT
059100951005     C* $FC1 = Abilitazioni videata D1
059200951005     C* OUTPUT
059300951005     C* *IN97 + *IN99 Errori
059400951005     C                   MOVEA     $FC1          $FC
059500951005     C                   EXSR      CTRKEY
059600940506     C*
059700940506     C     *IN99         IFEQ      *OFF
059800940131     C*
059900940117     C                   SELECT
060000940916     C* F2=Decodifica
060100940916     C     $TASTO        WHENEQ    F02
060200940916     C                   EXSR      F02D1
060300940131     C* F3=Fine
060400940506     C     $TASTO        WHENEQ    F03
060500940307     C                   EXSR      F03D1
060600940131     C* F4=Ricerca
060700940506     C     $TASTO        WHENEQ    F04
060800940127     C                   EXSR      SEARCH
060900940916     C* F5=Refresh
061000940916     C     $TASTO        WHENEQ    F05
061100940916     C                   EXSR      F05D1
061200941206     C* F8=Successivo
061300941206     C     $TASTO        WHENEQ    F08
061400941206     C                   EXSR      F08D1
061500940131     C* F12=Ritorno
061600940506     C     $TASTO        WHENEQ    F12
061700940307     C                   EXSR      F12D1
061800940131     C* F18=Modifica valori assunti
061900940506     C     $TASTO        WHENEQ    F18
062000940307     C                   EXSR      F18D1
062100940224     C* F24=Altri tasti
062200940506     C     $TASTO        WHENEQ    F24
062300940309     C                   EXSR      F24D1
062400940131     C*
062500940117     C                   OTHER
062600940127     C                   EXSR      CTRD1
062700940127     C     *IN99         IFEQ      *OFF
062800940131     C* definisco l'ordinamento
062900950410     C                   EXSR      DEFORD
063000950410     C* richiamo la lista
063100940127     C                   MOVE      'S1'          $GEST
063200940223     C                   MOVE      *ON           $INZS1
063300960110     C                   clear                   W1POSd
063400940211     C                   ENDIF
063500940224     C*
063600940117     C                   ENDSL
063700940224     C*
063800940506     C                   ENDIF
063900950525     C*
064000940117     C                   ENDSR
064100940224     C/EJECT
064200940224     C************************************************************
064300940224     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
064400940224     C************************************************************
064500940127     C     INZD1         BEGSR
064600940318     C*
064700940318     C                   CLEAR                   FMTD1
064800960118     C*
064900941013     C* Se pgm  richiamato con parametri forniti , li imposto nelle
065000941013     C*    parzializzazioni
065100941104     C                   SELECT
065200941213     C* il pgm è richiamato con chiave univoca per gestione POSTA
065300950606     C     $Unique       WHENEQ    *ON
065400941104     C* il pgm è richiamato e
065500941104     C* sono stati passati dei valori di parzializzazione ma non
065600941104     C* la chiave univoca
065700960118    >C     OPZ077        WHENNE    *BLANK
065800950403     C* imposto i valori passati sui campi a video
065900960109     C                   CALLb     'XDT4000'
066000960118     C                   PARM      dtRegD077     XDTAMG
066100960118     C     D1DtRegDA     PARM                    XDTGMA
066200960109     C                   PARM      2             XDTSTA
066300960109     C                   CALLb     'XDT4000'
066400960118     C                   PARM      dtRegA077     XDTAMG
066500960118     C     D1DtRegA      PARM                    XDTGMA
066600960109     C                   PARM      2             XDTSTA
066700960118    >C                   MOVE      CausTes077    D1CausTes
066800960118    >C                   MOVE      TpRegz077     D1TpRegz
066900960213    >C                   MOVE      Rifint077     D1RifInt
067000941104     C* il pgm non è richiamato
067100960118    >C     OPZ077        WHENEQ    *BLANK
067200950403     C* imposto dei valori di default
067300941115     C                   ENDSL
067400940224     C*
067500940117     C                   ENDSR
067600940916     C/EJECT
067700940916     C************************************************************
067800940916     C* GESTIONE F02 VIDEO D1
067900940916     C************************************************************
068000940916     C     F02D1         BEGSR
068100940916     C*
068200940916     C* per decodificare la videata richiamo la routine dei ctrl
068300940916     C* e poi disattivo gli eventuali messaggi di errore
068400940916     C                   EXSR      CTRD1
068500940916     C*
068600940916     C                   MOVE      *ALL'0'       IN5098
068700940916     C                   MOVEA     IN5098        *IN(50)
068800940916     C*
068900940916     C                   ENDSR
069000940224     C/EJECT
069100940131     C************************************************************
069200940307     C* GESTIONE F03 VIDEO D1
069300940131     C************************************************************
069400940307     C     F03D1         BEGSR
069500940131     C*
069600940307     C                   MOVE      *ON           $FINE
069700960118    >C                   MOVE      '1'           RET077
069800940325     C* fine programma
069900940325     C                   EXSR      ENDPGM
070000940307     C*
070100940307     C                   ENDSR
070200940916     C/EJECT
070300940916     C************************************************************
070400940916     C* GESTIONE F05 VIDEO D1
070500940916     C************************************************************
070600940916     C     F05D1         BEGSR
070700940916     C*
070800940916     C                   MOVE      *ON           $INZD1
070900940916     C*
071000940916     C                   ENDSR
071100941206     C/EJECT
071200941206     C************************************************************
071300941206     C* GESTIONE F08 VIDEO D1
071400941206     C************************************************************
071500941206     C     F08D1         BEGSR
071600941206     C*
071700941206     C                   MOVE      *ON           $FINE
071800960118    >C                   MOVE      '0'           RET077
071900941206     C*
072000941206     C                   ENDSR
072100940307     C/EJECT
072200940307     C************************************************************
072300940307     C* GESTIONE F12 VIDEO D1
072400940307     C************************************************************
072500940307     C     F12D1         BEGSR
072600940307     C*
072700940307     C*  se la 1a videata è la lista oppure sono arrivato qui con F17,
072800940307     C*  per F12 torno alla lista
072900940307     C     $LOP          IFEQ      2
073000940307     C     WINKR         OREQ      *ON
073100940307     C                   MOVE      'S1'          $GEST
073200940307     C*  altrimenti, per F12 torno a menu
073300940307     C                   ELSE
073400940307     C                   MOVE      *ON           $FINE
073500960118    >C                   MOVE      '2'           RET077
073600941027     C                   ENDIF
073700940307     C*
073800940307     C                   ENDSR
073900940307     C/EJECT
074000940307     C************************************************************
074100940307     C* GESTIONE F18 VIDEO D1
074200940307     C************************************************************
074300940307     C     F18D1         BEGSR
074400940307     C*
074500940307     C                   MOVE      'D2'          $GEST
074600940307     C                   MOVEL     *ON           $INZD2
074700940307     C*
074800940307     C                   ENDSR
074900940307     C/EJECT
075000940307     C************************************************************
075100940307     C* GESTIONE F24 VIDEO D1
075200940609     C* input
075300940509     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
075400940509     C* $FX1 =Testi tasti funzione
075500940509     C* $FC1 = Abilitazioni
075600940509     C* $FR1 = Inversioni di fondo
075700940307     C************************************************************
075800940307     C     F24D1         BEGSR
075900940506     C*
076000940506     C                   MOVEA     $FX1          $FX
076100940506     C                   MOVEA     $FC1          $FC
076200940506     C                   MOVEA     $FR1          $FR
076300940506     C                   Z-ADD     $ULKD1        $ULK
076400950726     C                   Z-ADD     $D1L01        $L01
076500950726     C                   Z-ADD     $D1L02        $L02
076600940506     C                   EXSR      RIEKEY
076700940506     C                   Z-ADD     $ULK          $ULKD1            3 0
076800940720     C                   MOVEA     $FC           $FC1
076900940506     C                   MOVEL     $KEY1         Z1KE1
077000060224C1913C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
077100940506     C                   MOVEL     $KEY2         Z1KE2
077200940506     C                   WRITE     FMTZ1
077300940506     C*
077400940307     C*
077500940307     C                   ENDSR
077600940307     C/EJECT
077700940307     C************************************************************
077800940307     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
077900950613     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
078000960109     C* 93 94 96 97 98
078100940307     C************************************************************
078200940307     C     CTRD1         BEGSR
078300940307     C*
078400940127     C                   SETOFF                                       99
078500940127     C* non imposto pos. del cursore
078600940127     C                   Z-ADD     0             H1RIGA
078700940127     C                   Z-ADD     0             H1COLO
078800940131     C*
078900960118     c* Data registrazione da
079000960109     C*
079100960118     c                   eval      dtRegdaR = Datmin
079200960118     c                   if        d1dtRegda <> 0
079300960109     C                   CALLb     'XDT4000'
079400960118     C     dtRegDAR      PARM                    XDTAMG
079500960118     C                   PARM      d1dtRegDA     XDTGMA
079600960109     C                   PARM      3             XDTSTA
079700960110     C                   IF        xdtsta < 0
079800960109     C                   SETON                                        5199
079900960109     C                   Endif
080000960109     C                   Endif
080100960109     C*
080200960118     c* Data registrazione a
080300960109     C*
080400960118     c                   eval      dtRegaR = Datmax
080500960118     c                   if        d1dtRega <> 0
080600960109     C                   CALLb     'XDT4000'
080700960118     C     dtRegAr       PARM                    XDTAMG
080800960118     C                   PARM      d1dtRega      XDTGMA
080900960109     C                   PARM      3             XDTSTA
081000960110     C                   IF        xdtsta < 0
081100960109     C                   SETON                                        5299
081200960109     C                   Endif
081300960109     C                   Endif
081400960109     C*
081500960109     c* Limiti errati
081600960118     c                   if        dtregaR  < dtregdaR
081700960109     C                   SETON                                        5399
081800960109     c                   endif
081900960118     c* causale
082000960118     C                   CLEAR                   D1CaustesD
082100960118     c                   if        d1caustes <> *blanks
082200960118     C                   EVAL      $LenOut = %Size(Anope)
082300960118     C                   CALLB     'NDMVC003'
082400960118     C                   PARM                    xscsoc
082500960118     C                   PARM                    d1caustes
082600960118     C                   PARM      datmin        $date
082700960118     C                   PARM                    Uso               1
082800960118     C                   PARM      *on           $gesmsg
082900960118     C                   PARM      *off          $esito
083000960118     C                   PARM      'ANOPE'       $Struttura
083100960118     C                   PARM                    ANOPE
083200960118     C                   PARM                    $LenOut
083300960118     C                   PARM                    $Refresh
083400960118     c                   if        $esito = *on
083500960118     c                   seton                                        5499
083600960118     c                   else
083700960118     c                   eval      d1caustesd = opedesbrev
083800960118     c                   endif
083900960118     c                   endif
084000960118     C*
084100960118     C* flag tipo registrazione
084200960118     C*
084300960118     c                   clear                   d1tpregzd
084400960118     C                   RESET                   X07ValDS
084500960118     C                   MOVE      '1'           X07RIC
084600960118     C                   MOVEL     'NDREG000'    X07TRC
084700960118     C                   MOVEL     'REGTPREGZ'   X07CAM
084800960118     C                   MOVEL     D1TpRegz      X07VAL
084900960118     C                   MOVEL     *on           X07All
085000961011     C                   CALLb     'X07VALR'
085100960118     C                   PARM                    X07ValDS
085200960118   B C     X07ERR        IFEQ      '0'
085300960118     C                   MOVEL     X07DES        D1TpRegzD
085400960118     c                   else
085500960118     c                   seton                                        5599
085600960118     c                   endif
085700960109     c*
085800940117     C                   ENDSR
085900960109     C/EJECT
086000950410     C************************************************************
086100950410     C* DEFINIZIONE DELL'ORDINAMENTO
086200950410     C************************************************************
086300950410     C     DEFORD        BEGSR
086400950410     C*
086500950410     C* definisco l'ordinamento della lista a seconda dei valori
086600950410     C* impostati nel video di parzializzazione
086700960118     C* Qui l'ordinamento è uno solo: per data reg
086800950410     C                   Z-ADD     1             $ORD
086900960109     c*
087000950410     C                   ENDSR
087100940224     C/EJECT
087200940127     C************************************************************
087300940131     C* GESTIONE LISTA
087400940127     C************************************************************
087500940127     C     GESS1         BEGSR
087600940223     C* inizializzazione videata
087700940223     C     $INZS1        IFEQ      *ON
087800940127     C                   EXSR      INZS1
087900940223     C                   MOVE      *OFF          $INZS1
088000940127     C                   ENDIF
088100940223     C* emissione piede videata se proveniente da altra
088200940223     C* salvataggio valore formato di provenienza
088300940202     C     $LASTV        IFNE      'S1'
088400940202     C                   WRITE     FMTT1
088500940224     C                   EXSR      GESPIE
088600940202     C                   MOVE      $LASTV        $LASTG
088700940202     C                   MOVE      'S1'          $LASTV
088800941027     C                   ENDIF
088900940223     C*
089000940223     C     WMAX          IFEQ      0
089100940211     C                   WRITE     FMTD3                                        NO RCD
089200940211     C                   ENDIF
089300980720     C*
089400980720     C* Inizializzazione indice tasti funzione personalizzati
089500980720     C                   Eval      PosFun = 25
089600940127     C*
089700940131     C                   EXFMT     FMTC1
089800940204     C     C1NRR         IFNE      0
089900940204     C                   Z-ADD     C1NRR         WSFL
090000940204     C                   ENDIF
090100951020     C                   IF        SFLNRR > 0
090200951020     C                   Z-ADD     SFLNRR        C1RCD
090300951020     C                   ENDIF
090400960119     C                   MOVE      '1'           Wxmsginz
090500940506     C*
090600951005     C* CONTROLLO ABILITAZIONE TASTI
090700951005     C* INPUT
090800951005     C* $FC2 = Abilitazioni videata S1
090900951005     C* OUTPUT
091000951005     C* *IN97 + *IN99 Errori
091100951005     C                   MOVEA     $FC2          $FC
091200951005     C                   EXSR      CTRKEY
091300941205     C*
091400940506     C     *IN99         IFEQ      *OFF
091500940127     C*
091600941027     C                   SELECT
091700940127     C* F3=Fine
091800940506     C     $TASTO        WHENEQ    F03
091900940309     C                   EXSR      F03S1
092000941206     C* F8=Successivo
092100941206     C     $TASTO        WHENEQ    F08
092200941206     C                   EXSR      F08S1
092300940131     C* F12=Ritorno
092400940506     C     $TASTO        WHENEQ    F12
092500940309     C                   EXSR      F12S1
092600001122d0771C* F13=Selezione multipla
092700001122  "  C     $TASTO        WHENEQ    F13
092800001127  "  C                   Eval      $TastoSav=$Tasto
092900001122d0771C                   EXSR      F13S1
093000940131     C* F17=Parzializzazioni
093100940506     C     $TASTO        WHENEQ    F17
093200940309     C                   EXSR      F17S1
093300940131     C* F18=Modifica valori assunti
093400940506     C     $TASTO        WHENEQ    F18
093500940309     C                   EXSR      F18S1
093600940224     C* F23=Altre opzioni
093700940506     C     $TASTO        WHENEQ    F23
093800940309     C                   EXSR      F23S1
093900940224     C* F24=Altri tasti
094000940506     C     $TASTO        WHENEQ    F24
094100940309     C                   EXSR      F24S1
094200940127     C* ROLL-UP
094300940506     C     $TASTO        WHENEQ    ROLLUP
094400940127     C                   EXSR      ROLS1
094500980720     C*
094600980720     C* Personalizzata
094700980720     C     $PGF2(PosFun) Whenne    *BLANK
094800980720     C                   EXSR      FperS1
094900940127     C*
095000941027     C                   OTHER
095100940127     C* CONTROLLO DATI
095200940131     C                   EXSR      CTRC1
095300940201     C     *IN99         IFEQ      *OFF
095400940131     C                   EXSR      CTRS1
095500941027     C                   ENDIF
095600941205     C*
095700941027     C                   ENDSL
095800940127     C*
095900940506     C                   ENDIF
096000941205     C*
096100940127     C                   ENDSR
096200940224     C/EJECT
096300980720     C************************************************************
096400980720     C* Chiamata programma con tasto personalizzato
096500980720     C************************************************************
096600980720     C     FPerS1        BEGSR
096700980720     C*
096800980720     C* salvo la DS di ingresso
096900980720    >C                   MOVEL     ndc077ds      $SaveDS
097000980720     C*
097100980720    >C                   MOVEL     ndc077ds      KPJBU
097200980720    >C                   CALL      $PGF2(PosFun)                        22
097300980720     C                   PARM                    KPJBA
097400980720     C                   PARM                    NOMPGM
097500980720     C*
097600980720     C* ripristino la DS di ingresso
097700980720    >C                   MOVEL     $SaveDS       ndc077ds
097800980720     C*
097900980720     C                   ENDSR
098000980720     C/EJECT
098100940127     C************************************************************
098200940131     C* INIZIALIZZAZIONE LISTA
098300940127     C************************************************************
098400940127     C     INZS1         BEGSR
098500001127d0771C*appoggio x F13
098600001127  "  C                   Clear                   $TastoSav
098700001127d0771C                   eval      PrimaVolta=0
098800940302     C* pulizia SFL
098900940128     C                   SETOFF                                         3031
099000940131     C                   WRITE     FMTC1
099100940128     C                   SETON                                          31
099200940127     C*
099300940128     C* riempimento opzioni
099400940223     C                   MOVEA     $TX1          $TX
099500940223     C                   MOVEA     $FL1          $FL
099600940309     C                   Z-ADD     0             WCOZ
099700940225     C*  equivale al num.di elementi della schiera $TX1
099800980720     C                   Z-ADD     OPZLEN        WMXO
099900950906     C                   Z-ADD     $C1Op1        $L01
100000950906     C                   Z-ADD     $C1Op2        $L02
100100940128     C                   EXSR      RIEOPZ
100200941214     C                   Z-ADD     23            X
100300941214     C                   MOVE      FF23          $FC2(X)
100400940608     C                   MOVEL     WOPZ1         C1OPZ1
100500060224C1913C                   MOVEL     $ALLOPT       H1ALLOPT
100600940608     C                   MOVEL     WOPZ2         C1OPZ2
100700940225     C                   Z-ADD     WCOZ          C1COZ
100800940128     C*
100900940128     C* CARICAMENTO SFL A PAGINE
101000940128     C*
101100940201     C                   Z-ADD     0             S1NRR
101200940128     C                   Z-ADD     0             WMAX
101300940224     C*
101400940224     C* Posizionamento su file pilota
101500960110     C                   clear                   C1POSD
101600941214     C                   EXSR      SETANA
101700940302     C* ripristino i valori del posizionamento
101800960109     C                   MOVE      W1POSD        C1POSD
101900940131     C*
102000940201     C* Lettura file pilota
102100941214     C                   EXSR      REDANA
102200940131     C*
102300940131     C* Carico 1a pagina
102400940127     C                   EXSR      ROLS1
102500940201     C                   Z-ADD     SFLPAG        WPAG
102600940127     C*
102700940127     C                   ENDSR
102800940127     C/EJECT
102900940127     C************************************************************
103000940131     C* CARICAMENTO PAGINA LISTA
103100940127     C************************************************************
103200940127     C     ROLS1         BEGSR
103300940127     C*
103400940128     C                   SETOFF                                       32
103500940223     C                   Z-ADD     0             Y
103600940127     C                   Z-ADD     WMAX          S1NRR
103700940127     C*
103800940127     C* Leggo dal file anagrafico per caricare la lista
103900940127     C*
104000950606     C     $EndFile      DOWEQ     *OFF
104100940201     C     Y             ANDLT     WPAG
104200940127     C*
104300940131     C                   EXSR      RIES1
104400940127     C*
104500940127     C                   ADD       1             S1NRR
104600940127     C                   ADD       1             Y
104700940127     C*
104800940131     C                   WRITE     FMTS1
104801170830     C
104802170830     C                   IF        s1nrr = 9999
104803170830     C                   LEAVE
104804170830     C                   ENDIF
104900940131     C*
105000960118     c* Mi posiziono sul subnumero successivo
105100960118     c     K04bhm02      setgt     ndbhm02
105200941214     C                   EXSR      REDANA
105300940128     C*
105400941027     C                   ENDDO
105500940127     C*
105600940223     C                   Z-ADD     S1NRR         WMAX                 30
105700940127     C*
105800940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
105900940127     C*
106000940201     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
106100940127     C                   MVR                     RESTO             3 0
106200940131     C     PAGINE        MULT      SFLPAG        C1RCD
106300941027     C     RESTO         IFGT      0
106400940127     C                   ADD       1             C1RCD
106500941027     C                   ELSE
106600940407     C                   SUB       SFLPAG        C1RCD
106700940407     C                   ADD       1             C1RCD
106800941027     C                   ENDIF
106900940128     C*
107000940127     C                   ENDSR
107100940224     C/EJECT
107200940224     C************************************************************
107300940224     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
107400940224     C************************************************************
107500941214     C     SETANA        BEGSR
107600960118     C                   eval      bhmsocieta = xscsoc
107700940224     C*
107800940224     C                   SELECT
107900941214     C*
108000941214     C* Gestione posizionamento
108100950301     C* Se il campo W1POS è > *blank imposto il posizionamento con
108200950301     C* W1POS altrimenti lo imposto con il valore di parametrizzazione
108300941214     C*
108400960110    >C* Ord.1 = chiave ordinamento 1  data
108500940224     C     $ORD          WHENEQ    1
108600960109     C     W1POSD        IFNE      0
108700960109     C                   CALLb     'XDT4000'
108800960118     C     BHMDTREG      PARM                    XDTAMG
108900960109     C                   PARM      w1POSD        XDTGMA
109000960109     C                   PARM      4             XDTSTA
109100940302     C                   ELSE
109200960109     C                   CALLb     'XDT4000'
109300960118     C     BHMDTREG      PARM                    XDTAMG
109400960118     C                   PARM      d1dtREGDa     XDTGMA
109500960109     C                   PARM      4             XDTSTA
109600941027     C                   ENDIF
109700960118    >C     K02BHM02      SETLL     NDBHM02
109800960109     c*
109900940224     C                   ENDSL
110000940224     C*
110100940224     C                   ENDSR
110200940128     C/EJECT
110300940128     C************************************************************
110400940131     C* LETTURA RCD ARCHIVIO PILOTA
110500940128     C************************************************************
110600941214     C     REDANA        BEGSR
110700940128     C*
110800950606     C                   MOVEL     *OFF          $EndFile
110900940131     C                   MOVEL     *OFF          $RCDOK
111000940131     C*
111100950606     C     $EndFile      DOUEQ     *ON
111200940131     C     $RCDOK        OREQ      *ON
111300940131     C*
111400941027     C                   SELECT
111500940131     C     $ORD          WHENEQ    1
111600960118    >C     Bhmsocieta    READE     NdBhm02                                21
111700941027     C                   ENDSL
111800940131     C*
111900950606     C                   MOVE      *IN21         $EndFile
112000940131     C*
112100950606     C     $EndFile      IFEQ      *OFF
112200941214     C                   EXSR      SELANA
112300941027     C                   ENDIF
112400940131     C*
112500941027     C                   ENDDO
112600940131     C*
112700940131     C                   MOVE      *IN21         *IN33                          SflEnd
112800940131     C*
112900940131     C                   ENDSR
113000940224     C/EJECT
113100940224     C************************************************************
113200940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
113300940224     C************************************************************
113400940224     C     CLCPAG        BEGSR
113500940224     C* Input :
113600940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
113700940224     C*          cursore
113800940224     C* - SFLPAG = numero rcd per pagina sfl
113900940224     C* Output :
114000940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
114100940224     C*
114200940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
114300940224     C                   MVR                     RESTO             3 0
114400940224     C     RESTO         IFGT      0
114500940224     C                   ADD       1             PAGINE
114600940224     C                   ENDIF
114700940224     C     PAGINE        MULT      SFLPAG        WPAG
114800940224     C*
114900940224     C                   ENDSR
115000940131     C/EJECT
115100940131     C************************************************************
115200940131     C* SELEZIONI SULLA RIGA LETTA
115300940314     C* impostare a *OFF il campo $RCDOK per scartare un record che
115400940314     C* non rientra nei parametri di parzializzazione
115500940314     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
115600940314     C* superiore stabilito nei parametri
115700940314     C*
115800940131     C************************************************************
115900941214     C     SELANA        BEGSR
116000940131     C*
116100940202     C                   MOVEL     *ON           $RCDOK
116200940131A1211X*                  MOVEL     *ALL'9'       ALL9             15
116300940131A1211C                   MOVEL     *hival        ALL9             15
116400940314     C*
116500940314     C                   SELECT
116600950301     C*
116700940314     C* SELEZIONI RISPETTO ALLE PARZIALIZZAZIONI
116800960109     C*
116900960109    >c* Data da
117000960118     c                   when      Bhmdtreg < DtregdaR
117100960109     C                   MOVE      *off          $RcdOk
117200960109    >c* Data a
117300960118     c                   when      Bhmdtreg > DtRegaR
117400960109     C                   MOVE      *off          $RcdOk
117500960118    >c* Causale
117600960118     c                   when      d1Caustes <> *blanks and BhmCaustes<>
117700960118     C                             d1Caustes
117800960109     C                   MOVE      *off          $RcdOk
117900960118    >c* tipo registrazione
118000960118     c                   when      d1tpregz<> *blanks and BhmTpRegz<>
118100960118     C                             d1tpregz
118200960109     C                   MOVE      *off          $RcdOk
118300960213    >c* Riferimento interno
118400960213     c                   when      d1RifInt <> *blanks and BhmRifInt <>
118500960213     C                             d1RifInt
118600960213     C                   MOVE      *off          $RcdOk
118700940202     C*
118800940202     C                   ENDSL
118900950503     C*
119000950503     C* FACILITAZIONE NEGLI ACCESSI
119100950503     C*
119200950314     C                   SELECT
119300960109    >C*
119400960109    >c* data  > del limite superiore
119500960109    >C                   WHEN      $ord = 1 and
119600960118     c                             BhmDtreg > DtRegaR
119700960118    >C     *HIVAL        SETLL     ndBhm02L
119800950314     C                   ENDSL
119900940202     C*
120000940202     C                   ENDSR
120100940131     C/EJECT
120200940131     C************************************************************
120300940131     C* RIEMPIMENTO RIGA LISTA
120400940131     C************************************************************
120500940131     C     RIES1         BEGSR
120600940131     C*
120700940131     C                   CLEAR                   FMTS1
120800950301     C*
120900960118     C                   eval      H1rifint = bhmrifint
121000960118     C                   eval      H1subnum = bhmsubnum
121100960109     C                   CALLb     'XDT4000'
121200960118     C                   PARM      bhmDtreg      XDTAMG
121300960118     C     s1dtreg       PARM                    XDTGMA
121400960109     C                   PARM      2             XDTSTA
121500960118     C                   eval      s1nrreg  = bhmnrreg
121600960118     C                   eval      s1serreg = bhmserreg
121700960118     C                   eval      s1tpregz = bhmtpregz
121800960118     C                   eval      s1caustes= bhmcaustes
121900960119     c*
122000960109     C                   CALLb     'XDT4000'
122100960118     C                   PARM      Bhmdtdoc      XDTAMG
122200960118     C     s1dtdoc       PARM                    XDTGMA
122300960109     C                   PARM      2             XDTSTA
122400960118     C                   eval      s1nrdoc  = bhmnrdoc
122500960118     C                   eval      s1serdoc = bhmserdoc
122600960118     C                   eval      s1unita  = bhmunita
122700980618     C                   eval      s1rifint = bhmrifint
122800001122d0771C*
122900001122  "  C* imposto anche l'opzione perchè se avessi premuto l'F13
123000001122  "  C* dovrei caricare l'opzione da replicare
123100001122  "  C                   if        $OpzOk   =    *On
123200001122  "  C                   Eval      S1Opz    =    WOpz
123300001122  "  C                   endif
123400001122  "  C                   if        S1Opz     <>  0
123500001122  "  C                   SetOn                                        32
123600001122d0771C                   endif
123700950301     C*
123800950301     C* imposto quegli attributi del sfl che sono modificabili
123900950301     C* al richiamo del determinato pgm
124000950301     C                   EXSR      ATRS1
124100940131     C*
124200940131     C                   ENDSR
124300950301     C/EJECT
124400950301     C************************************************************
124500950301     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
124600950301     C************************************************************
124700950301     C     ATRS1         BEGSR
124800950301     C*
124900950301     C*
125000950301     C                   ENDSR
125100941206     C/EJECT
125200941206     C************************************************************
125300941206     C* GESTIONE F08 VIDEO S1
125400941206     C************************************************************
125500941206     C     F08S1         BEGSR
125600941206     C*
125700941206     C                   MOVE      *ON           $FINE
125800960118    >C                   MOVE      '0'           RET077
125900941206     C*
126000941206     C                   ENDSR
126100940309     C/EJECT
126200940309     C************************************************************
126300940309     C* GESTIONE F03 VIDEO S1
126400940309     C************************************************************
126500940309     C     F03S1         BEGSR
126600940309     C*
126700940309     C                   MOVE      *ON           $FINE
126800960118    >C                   MOVE      '1'           RET077
126900940325     C* fine programma
127000940325     C                   EXSR      ENDPGM
127100940309     C*
127200940309     C                   ENDSR
127300940309     C/EJECT
127400940309     C************************************************************
127500940309     C* GESTIONE F12 VIDEO S1
127600940309     C************************************************************
127700940309     C     F12S1         BEGSR
127800940309     C*
127900940309     C*  se la 1a videata sono le parzializzazioni, per F12 torno a queste
128000940309     C     $LOP          IFEQ      1
128100940309     C                   MOVE      'D1'          $GEST
128200940309     C*  se la 1a videata è questa, per F12 torno a menu
128300940309     C                   ELSE
128400940309     C                   MOVE      *ON           $FINE
128500960118    >C                   MOVE      '2'           RET077
128600941027     C                   ENDIF
128700940309     C*
128800940309     C                   ENDSR
128900940309     C/EJECT
129000001122d0771C************************************************************
129100001122  "  C* GESTIONE F13 VIDEO S1
129200001122  "  C************************************************************
129300001122  "  C     F13S1         BEGSR
129400001122  "  C*
129500001122  "  C* E' simile alla funzione F13 del PDM: riporta l'opzione della
129600001122  "  C* riga del sfl su cui è posizionato il cursore dalla riga
129700001122  "  C* successiva fino alla fine del sfl stesso,
129800001122  "  C* senza sovrapporsi alle opzioni preesistenti.
129900001122  "  C*
130000001122  "  C* reset indicatori DSPATR
130100001122  "  C                   MOVE      *ALL'0'       IN4049           10
130200001122  "  C                   MOVEA     IN4049        *IN(40)
130300001122  "  C*
130400001122  "  C* il controllo lo faccio solo se il sfl NON è vuoto
130500001122  "  C                   If        WMax    <>    0
130600001122  "  C* salvo il numero del record su cui sono posizionato
130700001122  "  c                   Z-Add     C1Nrr         WC1Rcd
130800001122  "  C* leggo il rcd
130900001122  "  C     C1NrR         CHAIN     FMTS1                              21
131000001122  "  C* salvo l'opzione e il numero relativo
131100001122  "  C                   If        S1Opz   >     0
131200001122  "  C                   Z-Add     S1Opz         WOpz
131300001122  "  c                   Z-Add     C1Nrr         $X
131400001122  "  C*controllo autorizzazione all'opzione
131500001122  "  C                   Exsr      CtrAutPno
131600001122  "  C* attivo SflNxtChg
131700001122  "  C                   SetOn                                        32
131800001122  "  C                   UpDate    FmtS1
131900001122  "  C                   SetOff                                       32
132000001122  "  C                   if        $OpzOk    =    *On
132100001122  "  C*
132200001122  "  C* mi posiziono sul record successivo
132300001122  "  C                   Add       1             $X
132400001122  "  c*
132500001122  "  C                   DoW       $X      <=    WMax
132600001122  "  C     $X            Chain     FmtS1                              21
132700001122  "  C   21              Leave
132800001122  "  C                   if        S1Opz    =    0
132900001122  "  C                   Z-Add     WOpz          S1Opz
133000001122  "  C                   endif
133100001122  "  C* attivo SflNxtChg
133200001122  "  C                   SetOn                                        32
133300001122  "  C                   UpDate    FmtS1
133400001122  "  C                   SetOff                                       32
133500001122  "  C*
133600001122  "  C                   Add       1             $X
133700001122  "  C                   enddo
133800001122  "  c*
133900001122  "  C                   Eval      SflPag   =    9999
134000001122  "  C                   Eval      WPag     =    9999
134100001122  "  C*
134200001122  "  C                   ExSr      RolS1
134300001122  "  C* reimposto sflpag
134400001122  "  C                   Eval      SflPag   =    10
134500001122  "  C                   Eval      WPag     =    SflPag
134600001122  "  C*
134700001122  "  C* ripristino la posizione iniziale del record
134800001122  "  c                   Z-Add     WC1Rcd        C1Rcd
134900001122  "  C* ripristino il campo di salvataggio dell'opzione per non sporca
135000001122  "  C* re il campo S1Opz in caso di ricaricamento del sfl
135100001122  "  C                   Z-Add     0             WOpz
135200001122  "  c*
135300001122  "  C                   ENDIF
135400001122  "  C*
135500001122  "  C                   endif
135600001122  "  C*
135700001122  "  C                   endif
135800001122  "  C*
135900001122  "  C                   ENDSR
136000001122d0771C/EJECT
136100940309     C************************************************************
136200940309     C* GESTIONE F17 VIDEO S1
136300940309     C************************************************************
136400940309     C     F17S1         BEGSR
136500940309     C*
136600940309     C                   MOVE      'D1'          $GEST
136700940309     C*
136800940309     C                   ENDSR
136900940309     C/EJECT
137000940309     C************************************************************
137100940309     C* GESTIONE F18 VIDEO S1
137200940309     C************************************************************
137300940309     C     F18S1         BEGSR
137400940309     C*
137500940309     C                   MOVE      'D2'          $GEST
137600940309     C                   MOVEL     *ON           $INZD2
137700940309     C*
137800940309     C                   ENDSR
137900940309     C/EJECT
138000940309     C************************************************************
138100940309     C* GESTIONE F23 VIDEO S1
138200940309     C************************************************************
138300940309     C     F23S1         BEGSR
138400940309     C*
138500950906     C                   Z-ADD     $C1Op1        $L01
138600950906     C                   Z-ADD     $C1Op2        $L02
138700940309     C                   EXSR      RIEOPZ
138800941214     C                   Z-ADD     23            X
138900941214     C                   MOVE      FF23          $FC2(X)
139000940608     C                   MOVEL     WOPZ1         C1OPZ1
139100060224C1913C                   MOVEL     $ALLOPT       H1ALLOPT
139200940608     C                   MOVEL     WOPZ2         C1OPZ2
139300940309     C                   Z-ADD     WCOZ          C1COZ
139400940309     C*
139500940309     C                   ENDSR
139600940309     C/EJECT
139700940309     C************************************************************
139800940309     C* GESTIONE F24 VIDEO S1
139900940309     C************************************************************
140000940309     C     F24S1         BEGSR
140100940506     C*
140200940506     C                   MOVEA     $FX2          $FX
140300940506     C                   MOVEA     $FC2          $FC
140400940506     C                   MOVEA     $FR2          $FR
140500940506     C                   Z-ADD     $ULKS1        $ULK
140600950726     C                   Z-ADD     $S1L01        $L01
140700950726     C                   Z-ADD     $S1L02        $L02
140800940506     C                   EXSR      RIEKEY
140900940506     C                   Z-ADD     $ULK          $ULKS1            3 0
141000940720     C                   MOVEA     $FC           $FC2
141100940506     C                   MOVEL     $KEY1         Z2KE1
141200060224C1913C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
141300940506     C                   MOVEL     $KEY2         Z2KE2
141400940506     C                   WRITE     FMTZ2
141500940506     C*
141600940309     C                   ENDSR
141700940128     C/EJECT
141800940128     C************************************************************
141900940131     C* CONTROLLO TESTATA LISTA
142000940128     C************************************************************
142100940131     C     CTRC1         BEGSR
142200940128     C*
142300940201     C                   MOVE      *OFF          *IN99
142400960109     C*
142500960109     C                   if        c1posd <> 0
142600960109     C                   CALLb     'XDT4000'
142700960109     C                   PARM                    XDTAMG           10
142800960109     C                   PARM      c1posd        XDTGMA            6 0
142900960109     C                   PARM      3             XDTSTA            1 0
143000960109     C     XDTSTA        IFLT      0
143100960109     C                   SETON                                        6099
143200960109     C                   endif
143300960109     C                   endif
143400940131     C*
143500940202     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
143600940202     C* L'INIZIALIZZAZIONE LISTA
143700940202     C*
143800960109     c                   if        *in99 = *off
143900960118     C     C1POSD        IFNE      W1POSD
144000940202     C                   EXSR      POSS1
144100941027     C                   ENDIF
144200960109     C                   endif
144300940202     C*
144400940202     C                   ENDSR
144500940202     C/EJECT
144600940202     C************************************************************
144700940202     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
144800940202     C************************************************************
144900940202     C     POSS1         BEGSR
145000940202     C*
145100940202     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
145200940202     C*
145300941118   B1C     WMAX          IFGT      0
145400940202     C                   READC     FMTS1                                  21
145500941118   B2C     *IN21         DOWEQ     *OFF
145600940202     C     *IN99         ANDEQ     *OFF
145700940202     C*
145800941118   B3C     S1OPZ         IFNE      0
145900960109     C                   SETON                                        92  99
146000941118   E3C                   ENDIF
146100940202     C* se rilevata opzione attiva, attivo sflnxtchg
146200941118   B3C     *IN99         IFEQ      *ON
146300940202     C                   MOVE      *ON           *IN32
146400950301     C*
146500950301     C* imposto quegli attributi del sfl che sono modificabili
146600950301     C* al richiamo del determinato pgm
146700941118     C                   EXSR      ATRS1
146800941118     C*
146900940202     C                   UPDATE    FMTS1
147000941118   X3C                   ELSE
147100940202     C                   READC     FMTS1                                  21
147200941118   E3C                   ENDIF
147300940202     C*
147400941118   E2C                   ENDDO
147500940202     C*
147600941118   E1C                   ENDIF
147700940202     C*
147800940221     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
147900940221     C* AL VALORE DI POSIZIONAMENTO :
148000940221     C* se questo è maggiore del valore del rispettivo campo di
148100940221     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
148200940221     C* con i valori di parzializzazione.
148300940221     C*
148400941118   B1C     *IN99         IFEQ      *OFF
148500960110    >C                   clear                   W1POSD
148600940202     C*
148700950525     C* per ogni ordinamento devo testare se il campo dell'inizio elenco
148800950525     C* è maggiore del relativo valore di parzializzazione, nel qual caso
148900950525     C* imposto la variabile del posizionamento (W1Pos) al nuovo valore;
149000950525     C* richiedo comunque un ricaricamento del sfl che, se non si è
149100950525     C* verificata la condizione suddetta avverrà con posizionamento a
149200950525     C* partire dal valore di parzializzazione
149300950525     C*
149400941118   B2C                   SELECT
149500940202     C*
149600940202     C     $ORD          WHENEQ    1
149700960109    >C                   MOVEL     C1POSD        W1POSD
149800950525     C*
149900941118   E2C                   ENDSL
150000950525     C*
150100950525     C* richiedo l'inizializzazione sfl
150200950525     C                   MOVE      *ON           $INZS1
150300940202     C*
150400941118   E1C                   ENDIF
150500940202     C*
150600940131     C                   ENDSR
150700940131     C/EJECT
150800940131     C************************************************************
150900940131     C* CONTROLLO OPZIONI LISTA
151000940131     C************************************************************
151100940131     C     CTRS1         BEGSR
151200940131     C*
151300940202     C                   MOVEL     *OFF          $ESCI
151400940201     C                   SETOFF                                       99
151500940131     C                   Z-ADD     0             S1OPZ
151600940131     C*
151700940127     C* Leggo il sfl solo se ci sono rcd
151800941118   B1C     WMAX          IFGT      0
151900940131     C                   READC     FMTS1                                  21
152000940127     C*
152100940131     C* esce se fine sfl o errore che richiede l'uscita
152200941118   B2C     *IN21         DOWEQ     *OFF
152300940131     C     $ESCI         ANDEQ     *OFF
152400940201     C                   Z-ADD     S1NRR         C1RCD
152500940131     C* ctrl su riga
152600940131     C                   EXSR      RECS1
152700940131     C* gestione opzioni
152800941118   B3C     S1OPZ         IFNE      0
152900940201     C     *IN99         ANDEQ     *OFF
153000940131     C                   EXSR      OPZS1
153100941118   E3C                   ENDIF
153200940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
153300941118   B3C     *IN99         IFEQ      *ON
153400940131     C                   MOVE      *ON           *IN32
153500941118   E3C                   ENDIF
153600950301     C*
153700950301     C* imposto quegli attributi del sfl che sono modificabili
153800950301     C* al richiamo del determinato pgm
153900941118     C                   EXSR      ATRS1
154000941118     C*
154100940131     C                   Z-ADD     0             S1OPZ
154200940223     C*
154300940127     C                   UPDATE    FMTS1
154400941125     C                   MOVE      *OFF          *IN32
154500940223     C*
154600940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
154700941118   B3C     $ESCI         IFEQ      *OFF
154800940131     C                   READC     FMTS1                                  21
154900940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
155000941118   B4C     *IN21         IFEQ      *ON
155100940201     C                   MOVE      WINZS1        $INZS1
155200940204     C* calcolo pagina a cui deve posizionarsi
155300940728     C     WSFL          IFGT      0
155400940728     C                   EXSR      CLCPAG
155500940728     C                   ELSE
155600940728     C                   Z-ADD     SFLPAG        WPAG
155700940728     C                   ENDIF
155800941118   E4C                   ENDIF
155900941118   E4C                   ENDIF
156000940131     C*
156100941118   E2C                   ENDDO
156200940127     C*
156300941118   E1C                   ENDIF
156400940131     C*
156500940127     C                   ENDSR
156600940127     C/EJECT
156700940127     C************************************************************
156800940131     C* CONTROLLO CAMPI I/O RIGA LISTA
156900940127     C************************************************************
157000940131     C     RECS1         BEGSR
157100940131     C*
157200940201     C* reset indicatori DSPATR
157300940201     C                   MOVE      *ALL'0'       IN4049           10
157400940201     C                   MOVEA     IN4049        *IN(40)
157500940201     C*
157600940131     C* Controllo se Utente Autorizzato alla scelta
157700940202     C     S1OPZ         IFNE      0
157800940131     C                   Z-ADD     1             X
157900940223     C     S1OPZ         LOOKUP    $OP1(X)                                21
158000980720     C* mi salvo l'indice dell' opzione
158100980720     C   21              Z-add     X             PosOpz            2 0
158200940211     C     *IN21         IFEQ      *OFF
158300940223     C     $FL1(X)       OREQ      'N'
158400940131     C                   MOVE      *ON           $ESCI
158500960109     C                   SETON                                        409199
158600940131     C                   ENDIF
158700001122d0771 * Controllo con autorizzazioni CONPNO
158800001122d0771C                   Exsr      CtrAutPno
158900940202     C                   ENDIF
159000001122d0771x*
159100001122  "  x* Controllo se Utente Autorizzato alla cancellazione
159200001122  "  x***  S1OPZ         IFEQ      4
159300001122  "  x***  ANNPNB        ANDEQ     *off
159400001122  "  x***                SETON                                        409499
159500001122d0771x***                ENDIF
159600960119     C*
159700960119     C* Controllo se richiesta contabilità, che la causale esista
159800960119     C     S1OPZ         IFeq      41
159900960119     c     s1caustes     andeq     *Blanks
160000960119     C                   SETON                                        404199
160100960119     c                   endif
160200940131     C*
160300940131     C                   ENDSR
160400941118     C/EJECT
160500001122d0771C************************************************************
160600001122  "  C* Controllo autorizzazione prima nota batch manuale
160700001122  "  C************************************************************
160800001122  "  C     CtrAutPno     BEGSR
160900001122  "   *
161000001122  "  C                   Eval      $OpzOk     =  *On
161100001122  "   * Cancellazione
161200001122  "  C                   SELECT
161300001122  "  C     $TASTO        WHENEQ    F13
161400001122  "  C*
161500001122  "  C     WOpz          IFeq      4
161600001122  "  C     AnnPnb        Andeq     *off
161700001122  "  C                   MOVE      *ON           $ESCI                          sospese READC
161800001122  "  C                   Eval      $OpzOk     =  *Off
161900001122  "   * ...(F1) L'utente non è autorizzato alla funzione nel modo desiderato
162000001122  "  C                   SETON                                        409499
162100001122  "  C                   EndIf
162200001122  "  C                   OTHER
162300001122  "  C*
162400001122  "  C     S1OPZ         IFeq      4
162500001122  "  C     AnnPnb        Andeq     *off
162600001122  "  C                   MOVE      *ON           $ESCI                          sospese READC
162700001122  "  C                   Eval      $OpzOk     =  *Off
162800001122  "   * ...(F1) L'utente non è autorizzato alla funzione nel modo desiderato
162900001122  "  C                   SETON                                        409499
163000001122  "  C                   EndIf
163100001122  "   *
163200001122  "  C                   ENDSL
163300001122  "   *
163400001122d0771C                   ENDSR
163500941118     C************************************************************
163600941118     C* GESTIONE OPZIONI LISTA
163700941118     C************************************************************
163800941118     C     OPZS1         BEGSR
163900950301     C*
164000950301     C                   Z-ADD     RIRI          CURR
164100950301     C                   Z-ADD     COCO          CURC
164200941118     C*
164300941027     C                   SELECT
164400960215     c*
164500960215     C* se opz=scelta ripassa il numero del lotto
164600960215     C     S1OPZ         WHENEQ    1
164700960215     C                   MOVE      S1OPZ         OPZ077
164800960215     C* passare nei campi appositi della DS C001DS il valore
164900960215     C* della chiave della VL univoca (se c'è)
165000960215     C                   MOVE      H1RifInt      rifint077
165100960215     C                   MOVE      *ON           $ESCI
165200960215     C                   MOVE      *ON           $FINE
165300960215     C*
165400960215     C                   MOVE      *HIVAL        $LASTV
165500960119     c*
165600960119     C* se opz=cancellazione
165700960119     C* emetto xmsg per avvertire, se confermano richiamo pgm di cancella
165800960119     c* zione
165900960119     C     S1OPZ         WHENEQ    4
166000001127d0771C     $TASTOSAV     ANDNE     F13
166100001127d0771C                   Reset                   XMsgDs
166200960119     C* reperisco testi messaggi
166300960119     C                   EXSR      repmsg
166400960119     C*
166500001127d0771x***                Z-ADD     0             MSGMSG
166600001127  "  x***                MOVEL     'S'           MSGRSP
166700001127  "  x***                MOVEL     'SN'          MSGTPR
166800001127  "  x***                MOVEL     'S'           MSGEMV
166900001127  "  x***                Z-ADD     10            MSGRGP
167000001127  "  x***                MOVEL     'N'           MSGLCK
167100001127  "  x***                MOVEL     'N'           MSGCNL
167200001127  "  x***                MOVEL     'N'           MSGVID
167300001127  "  x***                MOVEL     'N'           MSGOPE
167400001127d0771X***                MOVEL     'N'           MSGSTP
167500960119     C                   CALL      'XMSGR'
167600960119     C                   PARM                    XMSGDS
167700960119     C                   PARM                    XMStxt          135
167800960119     C                   PARM      wxmsginz      XMSGinz           1
167900960119     c*
168000960119     C                   EXSR      RTNXMSG
168100960119     c                   move      *off          wxmsginz
168200001127d0771C* se opz=cancellazione +F13=selezione multipla,
168300001127  "  C* emetto 1 sola window dirichiesta conferma cancellazione        la
168400001127  "  C     S1OPZ         WHENEQ    4
168500001127  "  C     $TASTOSAV     ANDEQ     F13
168600001127  "  C*
168700001127  "  C                   If        PrimaVolta=0
168800001127  "  C                   reset                   xmsgds
168900001127  "  C                   eval      xmsgcd1 = 'PRO1473'
169000001127  "  C*
169100001127  "  C                   CALL      'XMSGR'
169200001127  "  C                   PARM                    XMSGDS
169300001127  "  C                   PARM                    XMStxt          135
169400001127  "  C                   PARM      wxmsginz      XMSGinz           1
169500001127  "  C                   PARM                    XMSGCd1           7
169600001127  "  C                   PARM                    XMSGCd2           7
169700001127  "  C                   PARM                    XMSGCd3           7
169800001127  "  C                   PARM                    XMSGSn1           7
169900040308D0771C                   PARM                    XMSGCdTxT        63
170000010619A0842C                   PARM                    MSGCdTit         14
170100010619A0842C                   PARM                    MSGClPrtf         1
170200010619B0395C                   PARM                    VarMsg1         100
170300010619  "  C                   PARM                    VarMsg2         100
170400010619B0395C                   PARM                    VarMsg3         100
170500040308D0771C                   Eval      PrimaVolta=1
170600001127  "  C                   EndIf
170700001127  "  C*ciclo di cancellazione
170800001127  "  C                   EXSR      RtnXMSg
170900001127d0771C                   move      *off          wxmsginz
171000001127     C*
171100960118     c*
171200960118     C* se opz=dettaglio
171300960118     C* richiamo il pgm di visualizzazione dettaglio
171400960118     C     S1OPZ         WHENEQ    15
171500960118     C                   RESET                   NDC078DS
171600960118     c*
171700960118     C                   MOVE      '05'          OPZ078
171800960118     c                   move      xscsoc        societa078
171900960118     C                   move      h1rifint      rifint078
172000960118     C                   move      h1subnum      subnum078
172100960118     c                   movel     ndc078ds      KPJBU
172200960118     C                   CALL      'NDC078R'
172300960118     C                   PARM                    KPJBA
172400960118     c                   movel     kpjbu         NDC078DS
172500960118     C*
172600960118     C* ritorno da PGM movimenti
172700960118     C                   EXSR      RTN078
172800960118     c*
172900960118     C* se opz=contabilità
173000960118     C* richiamo il pgm di primanota
173100960118     C     S1OPZ         WHENEQ    41
173200050228D1740c* prima controllo chiamata ricorsiva (potrebbe succedere da
173300050228  "  c* primanota veloce con creazione automatica rateo)
173400050228  "  c                   reset                   x48chPgmds
173500050228  "  C                   eval      x48pgm = 'NDC071R'
173600050228  "  c                   eval      kpjbu = x48chpgmds
173700050228  "  c                   call      'X48CHPGM'
173800050228  "  c                   parm                    kpjba
173900050228  "  c                   movel     kpjbu         x48chpgmds
174000050228D1740c                   if        x48errore = *off
174100960119     C                   RESET                   NDC071DS
174200960119     c                   eval      societa071 = xscsoc
174300960119     c                   eval      rifint071  = h1rifint
174400960119     C                   CALLb     'XDT4000'
174500960119     C     dtregda071    PARM                    XDTAMG
174600960119     C                   PARM      s1dtreg       XDTGMA
174700960119     C                   PARM      4             XDTSTA
174800960119     c                   eval      dtrega071  = dtregda071
174900960119     c                   eval      subnumd071 = h1subnum
175000960119     c                   eval      subnuma071 = h1subnum
175100960119     c                   eval      interat071 = *on
175200960119     c                   movel     ndc071ds      KPJBU
175300960214     C* non chiamo il cl perchè non mi serve il file msg perchè il pgm
175400960214     c* è in interattivo
175500960214     C                   CALL      'NDC071R'
175600960119     C                   PARM                    KPJBA
175700960119     c                   movel     kpjbu         NDC071DS
175800960119     C*
175900960119     C* ritorno da PGM movimenti
176000960119     C                   EXSR      RTN071
176100050228d1740c                   endif
176200980720     C*
176300980720     C* se selezionata opzione personalizzata, richiamo il programma
176400980720     C* corrispondente
176500980720     C     $PGO1(PosOpz) Whenne    *Blank
176600980720     C                   RESET                   ndc078ds
176700980720     C* << passare nei campi appositi della DS ndc0789DS il valore
176800980720     C*    della chiave della VL univoca (se c'è) >>
176900980720     C                   MOVEL     S1OPZ         OPZ078
177000980720     c*
177100980720     c                   move      xscsoc        societa078
177200980720     C                   move      h1rifint      rifint078
177300980720     C                   move      h1subnum      subnum078
177400980720     c                   movel     ndc078ds      KPJBU
177500980720     C*
177600980720     C                   CALL      $PGO1(PosOpz)                        22
177700980720     C                   PARM                    KPJBA
177800980720     C                   PARM                    NOMPGM
177900980720     C*
178000980720     C                   MOVEL     KPJBU         ndc078ds
178100980720     C*
178200980720     C* ritorno da PGM
178300980720     C                   EXSR      RTN078
178400980720     C*
178500950620     C* se altre opzioni
178600950620     C* richiamo pgm gestione anagrafica
178700941027     C                   OTHER
178800940131     C*
178900940506     C                   MOVE      *HIVAL        $LASTV
179000941027     C                   ENDSL
179100940225     C*
179200940131     C                   ENDSR
179300960118     C/EJECT
179400960118     C************************************************************
179500960118     C* GESTIONE RITORNO DA PGM DETTAGLIO
179600960118     C************************************************************
179700960118    >C     RTN078        BEGSR
179800960118     C*
179900960118     C* modo di ritorno
180000960118     C*
180100960118     C                   SELECT
180200960118     C* questi modi di utilizzo dei valori di ritorno dal
180300960118     C* pgm di manutenzione rcd di anagrafica sono delle
180400960118     C* proposte, normalmente sempre valide, ma modificabili
180500960118     C* per situazioni particolari
180600960118     C* 1 = F3
180700960118    >C     RET078        WHENEQ    '1'
180800960118     C                   MOVE      *ON           $ESCI
180900960118     C                   MOVE      *ON           $FINE
181000960118     C                   MOVE      '1'           RET077
181100960118     C* 2 = F12
181200960118    >C     RET078        WHENEQ    '2'
181300960118     C                   MOVE      *ON           $ESCI
181400960118     C*
181500960118     C                   ENDSL
181600960118     C*
181700960118     C* funzione non eseguibile per errore :
181800960118     C*
181900960118     C                   SELECT
182000960118     C* 1 = funzione richiamata chiusa in errore
182100960118    >C     ERR078        WHENEQ    '1'
182200960118     C                   MOVE      *ON           $ESCI
182300960118     C                   SETON                                        9599
182400960118     C* 2 = funzione richiamata senza la dovuta autorizzazione
182500960118    >C     ERR078        WHENEQ    '2'
182600960118     C                   MOVE      *ON           $ESCI
182700960118     C                   SETON                                        9499
182800960118     C*
182900960118     C                   ENDSL
183000960118     C*
183100960118     C                   ENDSR
183200960119     C/EJECT
183300960119     C************************************************************
183400960119     C* GESTIONE RITORNO DA PGM primanota generica
183500960119     C************************************************************
183600960119     C     RTN071        BEGSR
183700960119     C*
183800960214     C* ricarico subfile perchè se contabilizzata, è stata cancel
183900960214     c* lata da ndbhm, lo ricarico sempre perchè anche se è andata bene
184000960214     c* ci può essere il flag di errore impostato ad *on per le
184100960214     c* registrazioni collegate
184200960119     c                   move      *on           winzs1
184300960119     C*
184400001006D0721c* se ret071='0' e err071='0' significa che è stato dato F6
184500001006D0721c* di conferma da una registrazione di prima nota. In questo caso
184600001006D0721c* imposto ad *on opr077, anche se ovviamente ha senso solo se
184700001006D0721c* si esegue una sola registrazione, in quanto se ne potrebbero
184800001006D0721c* avere più di una con uno stesso rif.interno e in questo caso
184900001006D0721c* OPR077 si riferisce solo all'ultima registrazione eseguita
185000001006D0721C                   IF        Ret071= '0'  and Err071='0'
185100001006D0721C                   eval      Wopr = *on
185200001006d0721c                   endif
185300001006     c*
185400960119     C                   ENDSR
185500940223     C/EJECT
185600940223     C************************************************************
185700960118     C* GESTIONE RITORNO DA PGM XMSG
185800940223     C************************************************************
185900960118    >C     RTNXMSG       BEGSR
186000940223     C*
186100940223     C* modo di ritorno
186200960119     C*
186300960119     C* S = conferma cancellazione
186400960119     c* 1) chiamo pgm stampa passando *off nel campo errori072, e in questo
186500960119     c*    caso la stampa cancella anche
186600960119     c* 2) ricarico subfile
186700960119     C* N = no conferma: non faccio nulla
186800960119     C                   if        msgrtn = 'S'
186900960119     C* 1) stampa e cancellazione registrazione
187000960119     C                   reset                   NDC072DS
187100960119     C                   eval      societa072= xscsoc
187200960119     C                   eval      rifint072 = h1rifint
187300960119     C                   eval      subnum072 = h1subnum
187400960119     C                   eval      dociva072 = *on
187500960119     C                   eval      errori072 = *off
187600960119     C                   eval      cancel072 = *on
187700960119     C*
187800960119     C                   movel     NDC072DS      KPJBU
187900960119     C                   CALL      'NDC072R'
188000960119     C                   PARM                    KPJBA
188100021016C1579C*
188200021016  "  C* annullo raccordo movimento/registrazione per c/c di pool
188300021016  "  C                   ExSr      AnnNDCPR
188400021016  "  C                   if        §Errore   <>  '0'  and
188500021016  "  C                             §Errore   <>  '1'
188600021016  "  C                   SetOn                                        939940
188700021016  "  C                   endif
188800021016  "  C*
188900021016  "  C*
189000021016  "  C* se ci sono errori
189100021016  "  C                   if        *In99      =  *On
189200021016  "  C                   ROLBK
189300021016C1579C                   else
189400960119     C* Eseguo COMMIT
189500960119     C                   COMMIT
189600960119      *
189700960119     c* 2) ricarico subfile
189800001127d0771C*    Se  ho chiesto la selezione multipla
189900001127  "  C*    ricarico una sola volta alla fine.
190000001127d0771C                   If        $TastoSav<>F13 OR
190100001127d0771C                             ($TastoSav=F13 and  WMax>=S1Nrr)
190200960119     c                   move      *on           winzs1
190300001127d0771C                   EndIf
190400021016C1579C*
190500021016  "  C                   endif
190600021016C1579C*
190700021016D0771C                   Else
190800001127  "  C                   If        $TastoSav=F13
190900001127  "  c                   move      *on           winzs1
191000001127d0771C                   EndIf
191100001127     c                   endif
191200940223     C*
191300940223     C                   ENDSR
191400960115     C/EJECT
191500021016C1579 *----------------------------------------------------------
191600021016  "   * Annulla raccordo registrazioni giroconto per c/c pool
191700021016  "   *----------------------------------------------------------
191800021016C1579C     AnnNDCPR      BEGSR
191900021016     C*
192000021016     C     §Errore       DOUNE     '2'
192100021016     C     MSGRTN        ORNE      'R'
192200021016     C*
192300021016     C                   Eval      §Lunghezza   = %Size(NDCPRDS)
192400021016     C                   CALLB     'PJC00701R'
192500021016     C                   parm      '02'          §Opzione                       1
192600021016     C                   parm      XScSoc        §Societa                       7
192700021016     C                   parm      0             §SysReg                        7
192800021016     C                   parm      0             §NrAsReg                       7
192900021016     C                   parm      0             §NrRigaM                       7
193000021016     C                   parm      H1RifInt      §RifInt                        7
193100021016     C                   parm      H1SubNum      §SubNum                        7
193200021016     C                   parm      *Off          §Errore                        2
193300021016     C                   parm                    §Dati                          3
193400021016     C                   parm                    §Lunghezza                     4
193500021016     C                   parm      '1'           §Commit                        6
193600021016     C*
193700021016      *
193800021016      * gestione RCD allocato
193900021016     C     §Errore       IFEQ      '2'
194000021016     C                   MOVEL     'NDCPR00F'    MSGFIL
194100021016     C                   EXSR      MSGPGM
194200021016     C                   ENDIF
194300021016     C                   ENDDO
194400021016      *
194500021016C1579C                   ENDSR
194600021016C1579C******************************************************
194700021016  "  C* PGM PER GESTIONE MESSAGGI ERRORE                   *
194800021016  "  C******************************************************
194900021016C1579C     MSGPGM        BEGSR
195000021016     C*
195100021016     C                   Clear                   XMSGDS
195200021016     C                   Z-ADD     2             MSGMSG
195300021016     C                   MOVEL     'S'           MSGRSP
195400021016     C                   MOVEL     'CR'          MSGTPR
195500021016     C                   MOVEL     'S'           MSGEMV
195600021016     C                   Z-ADD     10            MSGRGP
195700021016     C                   MOVEL     'S'           MSGLCK
195800021016     C                   MOVEA     STATUS        STS
195900021016     C                   MOVEL     'N'           MSGCNL
196000021016     C                   MOVEL     'N'           MSGVID
196100021016     C                   MOVEL     'N'           MSGOPE
196200021016     C                   MOVEL     'N'           MSGSTP
196300021016     C                   CALL      'XMSGR'
196400021016     C                   PARM                    XMSGDS
196500021016     C                   PARM                    XMStxt          135
196600021016     C                   PARM      wxmsginz      XMSGinz           1
196700021016     C                   PARM      *Blank        XMSGCd1           7
196800021016     C                   PARM      *Blank        XMSGCd2           7
196900021016     C                   PARM      *Blank        XMSGCd3           7
197000021016     C                   PARM                    XMSGSn1           7
197100021016     C                   PARM                    XMSGCdTxT        63
197200021016     C                   PARM                    MSGCdTit         14
197300021016     C                   PARM                    MSGClPrtf         1
197400021016     C                   PARM                    VarMsg1         100
197500021016     C                   PARM                    VarMsg2         100
197600021016     C                   PARM                    VarMsg3         100
197700021016     C                   move      *off          wxmsginz
197800021016     C*
197900021016C1579C                   ENDSR
198000950301     C************************************************************
198100950301     C* AGGIORNAMENTO RIGA LISTA
198200950301     C************************************************************
198300950301     C     AGGS1         BEGSR
198400950301     C*
198500950301     C* riempio i campi del sfl che sono stati ripassati dal pgm
198600950301     C* di manutenzione dopo una opzione 02
198700950301     C*
198800950301     C                   ENDSR
198900940223     C/EJECT
199000940131     C************************************************************
199100940131     C* GESTIONE VALORI ASSUNTI
199200940131     C************************************************************
199300940131     C     GESD2         BEGSR
199400940131     C*
199500941118   B1C     $INZD2        IFEQ      *ON
199600940131     C                   EXSR      INZD2
199700940223     C                   MOVE      *OFF          $INZD2
199800941118   E1C                   ENDIF
199900940202     C*
200000941118   B1C     $LASTV        IFNE      'D2'
200100940923     C                   EXSR      GESPIE
200200940202     C                   MOVE      $LASTV        $LASTG
200300940202     C                   MOVE      'D2'          $LASTV
200400941118   E1C                   ENDIF
200500940131     C*
200600940707     C* se esistono errori sulla videata
200700940707     C* emetto la write del formato a indicatori spenti per vedere
200800940707     C* le eventuali decodifiche
200900941118   B1C     *IN99         IFEQ      *ON
201000940707     C                   MOVEA     *IN           WIN
201100940707     C                   MOVE      *ALL'0'       IN5098           49
201200940707     C                   MOVEA     IN5098        *IN(50)
201300940707     C                   WRITE     FMTD2
201400940707     C                   MOVEA     WIN           *IN
201500941118   E1C                   ENDIF
201600940707     C*
201700940131     C                   EXFMT     FMTD2
201800941205     C*
201900951005     C* CONTROLLO ABILITAZIONE TASTI
202000951005     C* INPUT
202100951005     C* $FC3 = Abilitazioni videata D2
202200951005     C* OUTPUT
202300951005     C* *IN97 + *IN99 Errori
202400951005     C                   MOVEA     $FC3          $FC
202500951005     C                   EXSR      CTRKEY
202600941205     C*
202700941205   B1C     *IN99         IFEQ      *OFF
202800941205     C*
202900941205   B2C                   SELECT
203000940131     C* F3=Fine
203100940506     C     $TASTO        WHENEQ    F03
203200940309     C                   EXSR      F03D2
203300940131     C* F12=Ritorno
203400940506     C     $TASTO        WHENEQ    F12
203500940309     C                   EXSR      F12D2
203600940916     C* F24=Altri tasti
203700940916     C     $TASTO        WHENEQ    F24
203800940916     C                   EXSR      F24D2
203900940131     C*
204000940131     C                   OTHER
204100940131     C                   EXSR      CTRD2
204200941205   B3C     *IN99         IFEQ      *OFF
204300940211     C                   MOVE      $LASTG        $GEST
204400941205   B4C                   SELECT
204500940202     C     $LASTG        WHENEQ    'S1'
204600940131     C* se è stato modificato l'ordinamento di default, ricarico la lista
204700941205   B5C     $ORD          IFNE      D2ORD
204800940223     C                   MOVE      *ON           $INZS1
204900960110     C                   clear                   W1POSd
205000941205   E5C                   ENDIF
205100940202     C     $LASTG        WHENEQ    'D1'
205200940318     C* per ora niente
205300941205   E4C                   ENDSL
205400940131     C* definisco i nuovi valori di default
205500940201     C                   Z-ADD     D2LOP         $LOP
205600940131     C                   Z-ADD     D2ORD         $ORD
205700950322     C*
205800950322     C* aggiorno valori assunti
205900950328     C* se richiesto il salvataggio
206000950328   B4C     D2BSV         IFEQ      1
206100950322     C                   EXSR      AGGMPE
206200950322   E4C                   ENDIF
206300950322     C*
206400941205   E3C                   ENDIF
206500940131     C*
206600941205   E2C                   ENDSL
206700940131     C*
206800941205   E1C                   ENDIF
206900941205     C*
207000940131     C                   ENDSR
207100940223     C/EJECT
207200940131     C************************************************************
207300940211     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
207400940131     C************************************************************
207500940201     C     INZD2         BEGSR
207600940131     C*
207700940131     C                   CLEAR                   FMTD2
207800960110     C                   MOVEL     $LOP          D2LOP
207900960110     C                   MOVE      $ORD          D2ORD
208000940131     C*
208100940131     C                   ENDSR
208200940309     C/EJECT
208300940309     C************************************************************
208400940309     C* GESTIONE F03 VIDEO D2
208500940309     C************************************************************
208600940309     C     F03D2         BEGSR
208700940309     C*
208800940309     C                   MOVE      *ON           $FINE
208900940325     C* fine programma
209000940325     C                   EXSR      ENDPGM
209100940309     C*
209200940309     C                   ENDSR
209300940309     C/EJECT
209400940309     C************************************************************
209500940309     C* GESTIONE F12 VIDEO D2
209600940309     C************************************************************
209700940309     C     F12D2         BEGSR
209800940309     C*
209900940309     C                   MOVE      $LASTG        $GEST
210000940309     C*
210100940309     C                   ENDSR
210200940916     C/EJECT
210300940916     C************************************************************
210400940916     C* GESTIONE F24 VIDEO D2
210500940916     C************************************************************
210600940916     C     F24D2         BEGSR
210700940916     C*
210800940916     C                   MOVEA     $FX3          $FX
210900940916     C                   MOVEA     $FC3          $FC
211000940916     C                   MOVEA     $FR3          $FR
211100940916     C                   Z-ADD     $ULKD2        $ULK
211200950726     C                   Z-ADD     $D2L01        $L01
211300950726     C                   Z-ADD     $D2L02        $L02
211400940916     C                   EXSR      RIEKEY
211500940916     C                   Z-ADD     $ULK          $ULKD2            3 0
211600940916     C                   MOVEA     $FC           $FC3
211700940916     C                   MOVEL     $KEY1         Z3KE1
211800060224C1913C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
211900940916     C                   WRITE     FMTZ3
212000940916     C*
212100940916     C                   ENDSR
212200940223     C/EJECT
212300940131     C************************************************************
212400940131     C* CONTROLLO VIDEATA VALORI ASSUNTI
212500940131     C************************************************************
212600940131     C     CTRD2         BEGSR
212700940131     C*
212800940131     C                   SETOFF                                       99
212900940131     C* non imposto pos. del cursore
213000940131     C                   Z-ADD     0             H1RIGA
213100940131     C                   Z-ADD     0             H1COLO
213200940131     C*
213300940131     C* OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
213400940201     C     D2LOP         IFEQ      0
213500940131     C                   SETON                                          5099
213600941027     C                   ENDIF
213700940131     C*
213800940131     C* OBBLIGATORIO VALORE PER ORDINAMENTO
213900940131     C     D2ORD         IFEQ      0
214000960109     C                   SETON                                          5199
214100941027     C                   ENDIF
214200940131     C*
214300940131     C                   ENDSR
214400941205     C/EJECT
214500941205     C************************************************************
214600941205     C* CONTROLLO ABILITAZIONE TASTI
214700941205     C* INPUT
214800941205     C* $TASTO =Tasto funzione premuto
214900941205     C* $FC = Tasti abilitati
215000941205     C* OUTPUT
215100941205     C* IN99 = ERrore
215200941205     C************************************************************
215300941205     C     CTRKEY        BEGSR
215400941205     C*
215500951005     C                   SETOFF                                       99
215600951005     C*
215700941205     C                   SELECT
215800941205     C     $TASTO        WHENEQ    F01
215900980720     C                   z-add     01            posfun
216000941205     C     $FC(1)        IFEQ      '0'
216100951005     C                   SETON                                        9997
216200941205     C                   ENDIF
216300941205     C     $TASTO        WHENEQ    F02
216400980720     C                   z-add     02            posfun
216500941205     C     $FC(2)        IFEQ      '0'
216600951005     C                   SETON                                        9997
216700941205     C                   ENDIF
216800941205     C     $TASTO        WHENEQ    F03
216900980720     C                   z-add     03            posfun
217000941205     C     $FC(3)        IFEQ      '0'
217100951005     C                   SETON                                        9997
217200941205     C                   ENDIF
217300941205     C     $TASTO        WHENEQ    F04
217400980720     C                   z-add     04            posfun
217500941205     C     $FC(4)        IFEQ      '0'
217600951005     C                   SETON                                        9997
217700941205     C                   ENDIF
217800941205     C     $TASTO        WHENEQ    F05
217900980720     C                   z-add     05            posfun
218000941205     C     $FC(5)        IFEQ      '0'
218100951005     C                   SETON                                        9997
218200941205     C                   ENDIF
218300941205     C     $TASTO        WHENEQ    F06
218400980720     C                   z-add     06            posfun
218500941205     C     $FC(6)        IFEQ      '0'
218600951005     C                   SETON                                        9997
218700941205     C                   ENDIF
218800941205     C     $TASTO        WHENEQ    F07
218900980720     C                   z-add     07            posfun
219000941205     C     $FC(7)        IFEQ      '0'
219100951005     C                   SETON                                        9997
219200941205     C                   ENDIF
219300941205     C     $TASTO        WHENEQ    F08
219400980720     C                   z-add     08            posfun
219500941205     C     $FC(8)        IFEQ      '0'
219600951005     C                   SETON                                        9997
219700941205     C                   ENDIF
219800941205     C     $TASTO        WHENEQ    F09
219900980720     C                   z-add     09            posfun
220000941205     C     $FC(09)       IFEQ      '0'
220100951005     C                   SETON                                        9997
220200941205     C                   ENDIF
220300941205     C     $TASTO        WHENEQ    F10
220400980720     C                   z-add     10            posfun
220500941205     C     $FC(10)       IFEQ      '0'
220600951005     C                   SETON                                        9997
220700941205     C                   ENDIF
220800941205     C     $TASTO        WHENEQ    F11
220900980720     C                   z-add     11            posfun
221000941205     C     $FC(11)       IFEQ      '0'
221100951005     C                   SETON                                        9997
221200941205     C                   ENDIF
221300941205     C     $TASTO        WHENEQ    F12
221400980720     C                   z-add     12            posfun
221500941205     C     $FC(12)       IFEQ      '0'
221600951005     C                   SETON                                        9997
221700941205     C                   ENDIF
221800941205     C     $TASTO        WHENEQ    F13
221900980720     C                   z-add     13            posfun
222000941205     C     $FC(13)       IFEQ      '0'
222100951005     C                   SETON                                        9997
222200941205     C                   ENDIF
222300941205     C     $TASTO        WHENEQ    F14
222400980720     C                   z-add     14            posfun
222500941205     C     $FC(14)       IFEQ      '0'
222600951005     C                   SETON                                        9997
222700941205     C                   ENDIF
222800941205     C     $TASTO        WHENEQ    F15
222900980720     C                   z-add     15            posfun
223000941205     C     $FC(15)       IFEQ      '0'
223100951005     C                   SETON                                        9997
223200941205     C                   ENDIF
223300941205     C     $TASTO        WHENEQ    F16
223400980720     C                   z-add     16            posfun
223500941205     C     $FC(16)       IFEQ      '0'
223600951005     C                   SETON                                        9997
223700941205     C                   ENDIF
223800941205     C     $TASTO        WHENEQ    F17
223900980720     C                   z-add     17            posfun
224000941205     C     $FC(17)       IFEQ      '0'
224100951005     C                   SETON                                        9997
224200941205     C                   ENDIF
224300941205     C     $TASTO        WHENEQ    F18
224400980720     C                   z-add     18            posfun
224500941205     C     $FC(18)       IFEQ      '0'
224600951005     C                   SETON                                        9997
224700941205     C                   ENDIF
224800941205     C     $TASTO        WHENEQ    F19
224900980720     C                   z-add     19            posfun
225000941205     C     $FC(19)       IFEQ      '0'
225100951005     C                   SETON                                        9997
225200941205     C                   ENDIF
225300941205     C     $TASTO        WHENEQ    F20
225400980720     C                   z-add     20            posfun
225500941205     C     $FC(20)       IFEQ      '0'
225600951005     C                   SETON                                        9997
225700941205     C                   ENDIF
225800941205     C     $TASTO        WHENEQ    F21
225900980720     C                   z-add     21            posfun
226000941205     C     $FC(21)       IFEQ      '0'
226100951005     C                   SETON                                        9997
226200941205     C                   ENDIF
226300941205     C     $TASTO        WHENEQ    F22
226400980720     C                   z-add     22            posfun
226500941205     C     $FC(22)       IFEQ      '0'
226600951005     C                   SETON                                        9997
226700941205     C                   ENDIF
226800941205     C     $TASTO        WHENEQ    F23
226900980720     C                   z-add     23            posfun
227000941205     C     $FC(23)       IFEQ      '0'
227100951005     C                   SETON                                        9997
227200941205     C                   ENDIF
227300941205     C     $TASTO        WHENEQ    F24
227400980720     C                   z-add     24            posfun
227500941205     C     $FC(24)       IFEQ      '0'
227600951005     C                   SETON                                        9997
227700941205     C                   ENDIF
227800941205     C                   ENDSL
227900941205     C*
228000941205     C                   ENDSR
228100940223     C/EJECT
228200940201     C************************************************************
228300940201     C* AGGIORNAMENTO VALORI ASSUNTI
228400940201     C************************************************************
228500940201     C     AGGMPE        BEGSR
228600940201     C*
228700950524     C* preparo i parametri di chiamata al driver
228800950524     C                   CLEAR                   XMPEDS
228900950524     C                   EVAL      TpChiamata = '03'
229000950524     C                   EVAL      NChiavi = 5
229100950524     C                   EVAL      VLogica = 2
229200950524     C*
229300950524     C* preparo le chiavi per la chiamata al driver
229400950524     C                   CLEAR                   XMPEDS1
229500950524     C                   MOVEL     XSCSOC        DSMPESOC
229600950524     C                   MOVEL     DSPGM         DSMPEPGM
229700950524     C                   MOVEL     KNMUS         DSMPEPRF
229800941013     C*
229900950322     C* Si memorizzano i dati di default a seconda delle modalità con
230000950322     C* cui il pgm è usato
230100950322     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
230200950322     C* MPETIP = 'PV'  pgm richiamato da menù
230300950322     C* MPETIP = 'PB'  x lancio Batch
230400950322     C*
230500941013     C                   SELECT
230600960118    >C     OPZ077        WHENEQ    '01'
230700950524     C                   MOVEL     'PX'          DSMPETIP
230800941013     C                   OTHER
230900950524     C                   MOVEL     'PV'          DSMPETIP
231000941013     C                   ENDSL
231100950524     C* dati
231200950524     C                   MOVEL     DSMPE         DSMPEPAR
231300950524     C*
231400950524     C                   CALLB     'XMPE'
231500950524     C                   PARM                    XMPEDS
231600950524     C                   PARM                    XMPEDS1
231700940201     C*
231800940201     C                   ENDSR
231900940223     C/EJECT
232000940131     C************************************************************
232100940131     C* OPERAZIONI INIZIALI
232200940131     C************************************************************
232300940131     C     *INZSR        BEGSR
232400000118C1309C     *DMY          Move      Udate         ZDateISO
232500000118C1309C     *JobRun       Move      ZDateISO      ZDate             6 0
232600940131     C*
232700940127     C* Reperimento parametri
232800940127     C*
232900940117     C     *ENTRY        PLIST
233000940117     C                   PARM                    KPJBA
233100950410     C                   PARM                    DSAUT
233200950410     C*
233300951010     C* testo se sono stati passati i dati di autorizzazione
233400951010     C*
233500950410     C     PARMS         IFLT      2
233600950410     C                   MOVE      *BLANK        AUTDS
233700950410     C                   ELSE
233800950410     C                   MOVEL     DSAUT         AUTDS
233900950410     C                   ENDIF
234000951206     C*
234100951206     C* Reperimento dati società
234200951206     C*
234300951206    >C                   MOVEL     'SOC001'      TIPXSC
234400951206     C                   MOVEL     *BLANK        SOCXSC
234500951206     C                   EXSR      REPSOC
234600951206     C     RTNXSC        IFNE      '1'
234700951206    >C                   MOVEL     XSOCDS        SOC001
234800951206     C                   ELSE
234900951206     C                   MOVE      *ON           $FINE
235000951206     C                   EXSR      ENDPGM
235100951206     C                   ENDIF
235200960109     c* Data odierna
235300960109     C                   CALLb     'XDT4000'
235400960109     C     WDATE         PARM                    XDTAMG
235500000118C1309C*****              PARM      UDATE         XDTGMA
235600000118C1309C                   PARM      ZDate         XDTGMA
235700960109     C                   PARM      4             XDTSTA
235800950726     C*
235900950726     C* reperisco le dimensioni delle variabili che conterranno i tasti
236000950726     C                   EVAL      $D1L01 = %Size(Z1Ke1)
236100950726     C                   EVAL      $D1L02 = %Size(Z1Ke2)
236200950726     C                   EVAL      $S1L01 = %Size(Z2Ke1)
236300950726     C                   EVAL      $S1L02 = %Size(Z2Ke2)
236400950926     C                   EVAL      $C1Op1 = %Size(C1Opz1)
236500950926     C                   EVAL      $C1Op2 = %Size(C1Opz2)
236600950906     C                   EVAL      $D2L01 = %Size(Z3Ke1)
236700950906     C                   EVAL      $D2L02 = 0
236800940225     C*
236900940223     C* Reperimento opzioni
237000940127     C*
237100940128     C                   EXSR      REPOPZ
237200950406     C*
237300940506     C* Reperimento tasti di funzione
237400940506     C*
237500940506     C                   EXSR      REPKEY
237600980722     C*
237700980722     C* Chiamata a routine per il reperimento delle opzioni e funzioni
237800980722     C* personalizzate per S1
237900980722     C*
238000980722     C                   CAllb     'X46USREXT'
238100980722     C                   PARM                    $OP1
238200980722     C                   PARM                    $TX1
238300980722     C                   PARM                    $FL1
238400980722     C                   PARM                    $PGO1
238500980722     C                   PARM      $dimOp        NumOpz
238600980722     C                   PARM                    $FX2
238700980722     C                   PARM                    $FC2
238800980722     C                   PARM                    $PGF2
238900980722     C                   PARM                    DsPgm
239000980722     C                   PARM                    xsccut
239100980722     C                   PARM                    xscgrp
239200980722     C*
239300980722     C                   Z-add     1             OPZLEN
239400980722     C     *blank        Lookup    $TX1(opzlen)                           21
239500980722     C                   if        *in21 = *on
239600980722     C                   sub       1             OPZLEN
239700980722     C                   else
239800980722     C                   Z-add     $DimOp        OPZLEN
239900980722     C                   endif
240000940127     C*
240100940117     C                   ENDSR
240200940223     C/EJECT
240300940223     C************************************************************
240400940223     C* INIZIALIZZAZIONE VARIABILI
240500940223     C************************************************************
240600940223     C     INZVAR        BEGSR
240700941215     C*
240800941215     C* Dati da PGM chiamante
240900941215     C*
241000960118    >C                   MOVEL     KPJBU         ndc077DS
241100951206     C*
241200951206     C* Se società diversa da quella impostata nella *inzsr, rieseguo
241300951206     C* reperimento dati società
241400960202     C                   If        Societa077 <> xscsoc and
241500960202     c                             societa077 <> *blanks
241600951122    >C                   MOVEL     'SOC001'      TIPXSC
241700960118     C                   MOVEL     Societa077    SOCXSC
241800951122     C                   EXSR      REPSOC
241900951010     C     RTNXSC        IFNE      '1'
242000951010    >C                   MOVEL     XSOCDS        SOC001
242100951010     C                   ELSE
242200951010     C                   MOVE      *ON           $FINE
242300951010     C                   EXSR      ENDPGM
242400951010     C                   ENDIF
242500951206     C                   endif
242600941201     C*
242700941124     C* Reperimento autorizzazioni
242800941124     C*
242900960108    >C     AUTDS         IFEQ      *BLANK
243000941124    >C                   EXSR      REPAUT
243100960108    >C                   ELSE
243200960118    >C                   MOVEL     XCADDS        CONPNODS
243300960108    >C                   ENDIF
243400941026     C*
243500941026     C* Reperimento valori default utente
243600941026     C*
243700941026     C                   EXSR      REPDFT
243800940506     C*
243900940506     C* Definizione delle opzioni utilizzabili a pgm
244000940506     C*
244100940506     C                   EXSR      DEFOPZ
244200950330     C*
244300940223     C* Variabili per gestione videate
244400940223     C*
244500940223     C                   MOVE      *OFF          $FINE                          fine pgm
244600940223     C                   MOVE      *OFF          $INZD1                         inizializzione D1
244700940223     C                   MOVE      *OFF          $INZS1                         inizializzione S1
244800940223     C                   MOVE      *OFF          $INZD2                         inizializzione D2
244900950606     C                   MOVE      *OFF          $EndFile                       memo fine file
245000940223     C                   MOVE      *OFF          $ESCI                          rich.uscita ciclo
245100940223     C                   MOVE      *OFF          $RCDOK                         rcd letto valido
245200940509     C                   MOVE      *HIVAL        $LASTV
245300940506     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
245400940506     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
245500940916     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
245600940223     C*
245700940318     C                   MOVE      *ON           $INZD1
245800940318     C                   MOVE      *ON           $INZS1
245900940831     C*
246000950330     C*
246100950330     C* Variabili appoggio
246200001122d0771C* inizialmente imposto il numero di righe per pagina a 10
246300001122d0771C                   Eval      SflPag    =   10
246400950330     C*
246500950330     C                   Z-ADD     SFLPAG        WPAG
246600950330     C                   MOVEL     *OFF          WOPR
246700960110     C                   clear                   W1POSd
246800941104     C*
246900941104     C* definisco qual'è la prima videata
247000950410     C* (se la prima videata risulta la lista, devo comunque caricare
247100950410     C*  la videata di parzializzazione e gestire la definizione
247200950410     C*  dell'ordinamento, perchè, dai default oppure se sono stati
247300950410     C*  passati dei dati, può variare il caricamento)
247400940223     C                   SELECT
247500940223     C     $LOP          WHENEQ    1
247600940223     C                   MOVE      'D1'          $GEST
247700940223     C     $LOP          WHENEQ    2
247800940223     C                   MOVE      'S1'          $GEST
247900941104     C                   EXSR      INZD1
248000960110     C                   MOVE      *OFF          $INZD1
248100960118     c                   eval      dtregaR = Wdate
248200950410     C                   EXSR      DEFORD
248300941104     C                   ENDSL
248400940223     C*
248500940223     C* Valorizzazione campi univoci testate
248600940223     C*
248700940223     C                   CLEAR                   FMTT1
248800940223     C                   MOVEL     KNSIF         NOMSIF
248900941109     C                   MOVEL     XSCDSI        NOMDIT
249000941027     C                   MOVEL     DSPGM         NOMPGM
249100950331     C* recupero il sottotitolo (se c'è)
249200950331     C                   MOVEL     *BLANK        T1TIT
249300950331     C     SOTTIT        IFNE      *BLANK
249400950331     C                   MOVEL     SOTTIT        IDMSG
249500950522     C                   CALLB     'XRTVM'
249600950331     C                   PARM                    IDMSG            27
249700950331     C                   PARM                    TXTMSG          100
249800950331     C                   PARM                    ERRMSG            1
249900950331     C                   PARM      *ON           CTRMSG            1            centratura
250000950331     C                   PARM      30            LENMSG            3 0          lun output.
250100950331     C     ERRMSG        IFNE      *ON
250200950331     C                   MOVEL     TXTMSG        T1TIT
250300950331     C                   ELSE
250400950331     C                   MOVEL     *ALL'?'       T1TIT
250500950331     C                   ENDIF
250600950331     C                   ENDIF
250700960119     C*
250800960119     C* Eseguo strcmtctl
250900960119     C                   CALLb     'XSTRCMT'
251000980720     C*
251100980720     C                   ENDSR
251200941209     C/EJECT
251300941209     C************************************************************
251400941209     C* REPERIMENTO DATI SOCIETA'
251500941209     C************************************************************
251600941209     C     REPSOC        BEGSR
251700941209     C*
251800950522     C                   CALLB     'XSOC'
251900941209     C                   PARM                    TIPXSC            6
252000941209     C                   PARM                    SOCXSC            3
252100941209     C                   PARM                    CDSXSC            9 0
252200941209     C                   PARM                    MODXSC            3
252300941209     C                   PARM      *BLANKS       RTNXSC            1
252400941209     C                   PARM                    XSOCDS
252500941209     C                   PARM                    KPJBA
252600941209     C*
252700941209     C                   ENDSR
252800941124     C/EJECT
252900941124     C************************************************************
253000941124     C* REPERIMENTO AUTORIZZAZIONI UTENTE
253100941124     C************************************************************
253200941124     C     REPAUT        BEGSR
253300941124     C*
253400941124     C                   CLEAR                   AUTDS
253500941124     C*
253600941124     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
253700941124     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
253800941124     C                   Z-ADD     0             XCAGRP                         GRUPPO
253900941124     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
254000960118    >C                   MOVEL     'CONPNO'      XCAFNC                         FUNZIONE
254100941124    >C                   MOVEL     *ZERO         XCAVFU                         VARIABILE
254200941124    >C                   MOVE      'CK'          XCATCK                         TIPO CTRL
254300941124    >C                   MOVE      '1'           XCANAU                         RICH.XNOAUT
254400941124     C*
254500950522     C                   CALLB     'XCHKAUT'
254600941124     C                   PARM                    AUTDS
254700941124     C*
254800941124     C* errori/non abilitazione
254900941124     C     XCARTN        IFNE      0
255000941124     C                   EXSR      ENDPGM
255100941124     C                   ELSE
255200960118    >C                   MOVEL     XCADDS        CONPNODS
255300941124     C                   ENDIF
255400941124     C*
255500941124     C                   ENDSR
255600940223     C/EJECT
255700940127     C************************************************************
255800940131     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
255900940127     C************************************************************
256000940131     C     REPDFT        BEGSR
256100940127     C*
256200950524     C* preparo i parametri di chiamata al driver
256300950524     C                   CLEAR                   XMPEDS
256400950524     C                   EVAL      TpChiamata = '01'
256500950524     C                   EVAL      NChiavi = 5
256600950524     C                   EVAL      VLogica = 2
256700950524     C*
256800950524     C* preparo le chiavi per la chiamata al driver
256900950524     C                   CLEAR                   XMPEDS1
257000950524     C                   MOVEL     XSCSOC        DSMPESOC
257100950524     C                   MOVEL     DSPGM         DSMPEPGM
257200950524     C                   MOVEL     KNMUS         DSMPEPRF
257300950524     C* Si reperiscono i dati di default a seconda delle modalità con
257400950524     C* cui il pgm è usato
257500950524     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
257600950524     C* MPETIP = 'PV'  pgm richiamato da menù
257700950524     C* MPETIP = 'PB'  x lancio Batch
257800941013     C                   SELECT
257900960118    >C     OPZ077        WHENEQ    '01'
258000950524     C                   MOVEL     'PX'          DSMPETIP
258100941013     C                   OTHER
258200950524     C                   MOVEL     'PV'          DSMPETIP
258300941013     C                   ENDSL
258400950524     C*
258500950524     C                   CALLB     'XMPE'
258600950524     C                   PARM                    XMPEDS
258700950524     C                   PARM                    XMPEDS1
258800950524     C*
258900950524     C     Risultato     IFEQ      '0'
259000950524     C                   MOVEL     DSMPEPAR      DSMPE
259100940131     C* nel caso sia la prima volta che l'utente apre questa anagrafica
259200940131     C* i suoi valori di default vengono decisi a pgm
259300940131     C                   ELSE
259400940201     C                   Z-ADD     1             $LOP
259500940131     C                   Z-ADD     1             $ORD
259600941027     C                   ENDIF
259700940131     C*
259800940131     C                   ENDSR
259900940223     C/EJECT
260000940131     C************************************************************
260100940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
260200940131     C************************************************************
260300940225     C     DEFOPZ        BEGSR
260400940131     C*
260500940225     C* Questa routine deve valorizzare la schiera $FL1
260600940225     C* con S o N a seconda della utilizzabilità o meno
260700940225     C* dell'opzione correlata
260800940225     C*
260900950301     C* Inizialmente si considerano tutte le opzioni come valide
261000940317     C     1             DO        $DIMOP        X
261100940225     C                   MOVE      'S'           $FL1(X)
261200941027     C                   ENDDO
261300940715     C*
261400940715     C* Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
261500941018     C* con 1 o 0 a seconda della utilizzabilità o meno
261600940715     C* del tasto funzione correlato
261700940715     C*
261800940715     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
261900940715     C* di compilazione
262000940715     C                   RESET                   $FC1
262100940715     C                   RESET                   $FR1
262200940715     C                   RESET                   $FC2
262300940715     C                   RESET                   $FR2
262400940919     C                   RESET                   $FC3
262500940919     C                   RESET                   $FR3
262600940225     C*
262700940225     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
262800940225     C* invalido alcune opzioni
262900940225     C                   SELECT
263000940225     C* Se pgm richiamato in ricerca disattivo :
263100960118    >C     OPZ077        WHENEQ    '01'
263200940603     C* F3=Fine
263300940603     C                   Z-ADD     3             X
263400941216     C                   MOVE      '0'           $FC1(X)
263500941216     C                   MOVE      '0'           $FC2(X)
263600941216     C                   MOVE      '0'           $FC3(X)
263700950301     C*
263800940225     C* Se pgm richiamato in interrogazione disattivo :
263900960118    >C     OPZ077        WHENEQ    '05'
264000940225     C* 1=Selezione
264100940225     C                   Z-ADD     1             X
264200940225     C     01            LOOKUP    $OP1(X)                                21
264300940225     C     *IN21         IFEQ      *ON
264400940225     C                   MOVE      'N'           $FL1(X)
264500940225     C                   ENDIF
264600940225     C* 4=Annullamento
264700940225     C                   Z-ADD     1             X
264800940225     C     04            LOOKUP    $OP1(X)                                21
264900940225     C     *IN21         IFEQ      *ON
265000940225     C                   MOVE      'N'           $FL1(X)
265100940225     C                   ENDIF
265200940225     C*
265300960118     C     OPZ077        WHENEQ    *BLANKS
265400940225     C* Se pgm non richiamato, disattivo subito :
265500941124     C* F8=Selezione seguente
265600941124     C                   Z-ADD     8             X
265700941206     C                   MOVE      '0'           $FC1(X)
265800941206     C                   MOVE      '0'           $FC2(X)
265900940225     C* 1=Selezione
266000940225     C                   Z-ADD     1             X
266100940225     C     01            LOOKUP    $OP1(X)                                21
266200940225     C     *IN21         IFEQ      *ON
266300940225     C                   MOVE      'N'           $FL1(X)
266400940225     C                   ENDIF
266500940225     C*
266600950301     C* In tutti gli altri casi, disattivo subito :
266700941228     C                   OTHER
266800941228     C* 1=Selezione
266900941228     C                   Z-ADD     1             X
267000941228     C     01            LOOKUP    $OP1(X)                                21
267100941228     C     *IN21         IFEQ      *ON
267200941228     C                   MOVE      'N'           $FL1(X)
267300941228     C                   ENDIF
267400941228     C*
267500940225     C                   ENDSL
267600940225     C*
267700940225     C                   ENDSR
267800940225     C/EJECT
267900940225     C************************************************************
268000940225     C* REPERIMENTO OPZIONI
268100940225     C************************************************************
268200940225     C     REPOPZ        BEGSR
268300940225     C*
268400940314     C     1             DO        $DIMOP        X
268500940128     C*
268600940223     C     $MS1(X)       IFNE      *BLANK
268700940128     C*
268800940223     C                   MOVEL     $MS1(X)       IDMSG
268900950522     C                   CALLB     'XRTVM'
269000940201     C                   PARM                    IDMSG            27
269100940201     C                   PARM                    TXTMSG          100
269200940201     C                   PARM                    ERRMSG            1
269300940128     C*
269400940128     C     ERRMSG        IFNE      *ON
269500940223     C                   MOVEL     TXTMSG        $TX1(X)
269600940128     C                   ELSE
269700940223     C                   MOVEL     *ALL'?'       $TX1(X)
269800941027     C                   ENDIF
269900940128     C*
270000941027     C                   ENDIF
270100940128     C*
270200941027     C                   ENDDO
270300940128     C*
270400940128     C                   ENDSR
270500940506     C/EJECT
270600940506     C************************************************************
270700940506     C* REPERIMENTO TASTI DI FUNZIONE
270800940506     C************************************************************
270900940506     C     REPKEY        BEGSR
271000940506     C                   MOVEA     $FM1          $FM
271100950721     C                   MOVEA     $FC1          $FC
271200940506     C                   EXSR      REPKKK
271300940506     C                   MOVEA     $FX           $FX1
271400940506     C*
271500940506     C                   MOVEA     $FM2          $FM
271600950721     C                   MOVEA     $FC2          $FC
271700940506     C                   EXSR      REPKKK
271800940506     C                   MOVEA     $FX           $FX2
271900940919     C*
272000940919     C                   MOVEA     $FM3          $FM
272100950721     C                   MOVEA     $FC3          $FC
272200940919     C                   EXSR      REPKKK
272300940919     C                   MOVEA     $FX           $FX3
272400940919     C                   ENDSR
272500940506     C/EJECT
272600940506     C************************************************************
272700940506     C* REPERIMENTO TASTI DI FUNZIONE
272800940506     C************************************************************
272900940506     C     REPKKK        BEGSR
273000940506     C*
273100940506     C                   CLEAR                   $FX
273200940506     C     1             DO        24            X
273300940506     C*
273400940506     C     $FM(X)        IFNE      *BLANK
273500950721     C     $Fc(X)        andne     '0'
273600940506     C*
273700940506     C                   MOVEL     $FM(X)        IDMSG
273800950522     C                   CALLB     'XRTVM'
273900940506     C                   PARM                    IDMSG            27
274000940506     C                   PARM                    TXTMSG          100
274100940506     C                   PARM                    ERRMSG            1
274200940506     C*
274300940506     C     ERRMSG        IFNE      *ON
274400940506     C                   MOVEL     TXTMSG        $FX(X)
274500940506     C                   ELSE
274600940506     C                   MOVEL     *ALL' '       $FX(X)
274700941027     C                   ENDIF
274800940506     C*
274900941027     C                   ENDIF
275000940506     C*
275100941027     C                   ENDDO
275200940506     C*
275300940506     C                   ENDSR
275400940223     C/EJECT
275500940128     C************************************************************
275600950606     C* DEFINIZIONE KLIST
275700940128     C************************************************************
275800950606     C     DefKlist      BEGSR
275900940128     C*
276000940223     C* klist
276100960118    >C* ndbhm02l
276200960118    >C     K02bhm02      KLIST
276300960118    >C                   KFLD                    bhmsocieta
276400960118    >C                   KFLD                    bhmdtreg
276500940223     C*
276600960118    >C     K04bhm02      KLIST
276700960118    >C                   KFLD                    bhmsocieta
276800960118    >C                   KFLD                    bhmdtreg
276900960118    >C                   KFLD                    bhmrifint
277000960118    >C                   KFLD                    bhmsubnum
277100940127     C*
277200940117     C                   ENDSR
277300960119     C/EJECT
277400960119     C************************************************************
277500960119     C* Reperimento testo messaggi per contabilizzazione interattiva
277600960119     C* non riuscita
277700960119     C************************************************************
277800960119     C     REPmsg        BEGSR
277900960119     c*
278000960119     C                   eval      IDMSG = 'PROMSG    *LIBL     COS0209'
278100960119     c                   exsr      prendimsg
278200960119     C     ERRMSG        IFNE      *ON
278300960119     C                   MOVEL     TXTMSG        MSGMS1
278400960119     C                   ELSE
278500960119     C                   MOVEL     *ALL'?'       msgms1
278600960119     C                   ENDIF
278700960119     c*
278800960119     C                   eval      IDMSG = 'PROMSG    *LIBL     COS0210'
278900960119     c                   exsr      prendimsg
279000960119     C     ERRMSG        IFNE      *ON
279100960119     C                   MOVEL     TXTMSG        MSGMS2
279200960119     C                   ELSE
279300960119     C                   MOVEL     *ALL'?'       msgms2
279400960119     C                   ENDIF
279500960119     c*
279600960119     C                   eval      IDMSG = 'PROMSG    *LIBL     COS0211'
279700960119     c                   exsr      prendimsg
279800960119     C     ERRMSG        IFNE      *ON
279900960119     C                   MOVEL     TXTMSG        MSGMS3
280000960119     C                   ELSE
280100960119     C                   MOVEL     *ALL'?'       msgms3
280200960119     C                   ENDIF
280300960119     c*
280400960119     C                   ENDSR
280500960119     C/EJECT
280600960119     C************************************************************
280700960119     C* Reperimento testo messaggi per contabilizzazione interattiva
280800960119     C* non riuscita
280900960119     C************************************************************
281000960119     C     PrendiMsg     BEGSR
281100960119     C*
281200960119     C                   CALLB     'XRTVM'
281300960119     C                   PARM                    IDMSG            27
281400960119     C                   PARM                    TXTMSG          100
281500960119     C                   PARM                    ERRMSG            1
281600960119     C                   PARM      *ON           CTRMSG            1            centratura
281700960125     C                   PARM      70            LENMSG            3 0          lun output.
281800960119     C*
281900960119     C                   ENDSR
282000960119     C/EJECT
282100001122d0771C* per S1: cambiato msg x f13 e abilitato il tasto
282200941214**
282300940718PROMSG    *LIBL     OPZ0001
282400960118PROMSG    *LIBL     OPZ0004
282500960118PROMSG    *LIBL     OPZ0120
282600960118PROMSG    *LIBL     OPZ0041
282700940201**
28280094022501S      <Selezione
28290094022504S      <Annullamento
28300096011815S      <Dettaglio
28310096011841S      <contabilità
283200940506** TASTI DI FUNZIONE  D1
283300940506PROMSG    *LIBL     KEY0001  01
283400940506PROMSG    *LIBL     KEY0002  02
283500940506PROMSG    *LIBL     KEY0003  03
283600940506PROMSG    *LIBL     KEY0004  04
283700940506PROMSG    *LIBL     KEY0005  05
283800940506PROMSG    *LIBL     KEY0006  06
283900940506PROMSG    *LIBL     KEY0007  07
284000940506PROMSG    *LIBL     KEY0008  08
284100940506PROMSG    *LIBL     KEY0009  09
284200940506PROMSG    *LIBL     KEY0010  10
284300940506PROMSG    *LIBL     KEY0011  11
284400940506PROMSG    *LIBL     KEY0012  12
284500940506PROMSG    *LIBL     KEY0013  13
284600940506PROMSG    *LIBL     KEY0014  14
284700940506PROMSG    *LIBL     KEY0015  15
284800940506PROMSG    *LIBL     KEY0016  16
284900940506PROMSG    *LIBL     KEY0017  17
285000940506PROMSG    *LIBL     KEY0018  18
285100940506PROMSG    *LIBL     KEY0019  19
285200940506PROMSG    *LIBL     KEY0020  20
285300940506PROMSG    *LIBL     KEY0021  21
285400940506PROMSG    *LIBL     KEY0022  22
285500940506PROMSG    *LIBL     KEY0023  23
285600940506PROMSG    *LIBL     KEY0024  24
285700940506**  ABILITAZIONE D1
2858009407200N  01
2859009507031N  02
2860009407201N  03
2861009407201N  04
2862009407200N  05
2863009407200N  06
2864009407200N  07
2865009412061N  08
2866009407200N  09
2867009601100N  10
2868009407200N  11
2869009407201N  12
2870009407200N  13
2871009407200N  14
2872009407200N  15
2873009407200N  16
2874009407200N  17
2875009407201N  18
2876009407200N  19
2877009407200N  20
2878009407200N  21
2879009407200N  22
2880009507260N  23   Impostare a '1' se si utilizzano opzioni
2881009507261N  24   Lasciare sempre impostata a '1' il valore della riga 24
288200940916** TASTI DI FUNZIONE  S1
288300940916PROMSG    *LIBL     KEY0001  01
288400940916PROMSG    *LIBL     KEY0002  02
288500940916PROMSG    *LIBL     KEY0003  03
288600940916PROMSG    *LIBL     KEY0004  04
288700940916PROMSG    *LIBL     KEY0005  05
288800940916PROMSG    *LIBL     KEY0006  06
288900940916PROMSG    *LIBL     KEY0007  07
289000940916PROMSG    *LIBL     KEY0008  08
289100940916PROMSG    *LIBL     KEY0009  09
289200940916PROMSG    *LIBL     KEY0010  10
289300940916PROMSG    *LIBL     KEY0011  11
289400940916PROMSG    *LIBL     KEY0012  12
289500001122PROMSG    *LIBL     KEY0061  13
289600940916PROMSG    *LIBL     KEY0014  14
289700940916PROMSG    *LIBL     KEY0015  15
289800940916PROMSG    *LIBL     KEY0016  16
289900940916PROMSG    *LIBL     KEY0017  17
290000940916PROMSG    *LIBL     KEY0018  18
290100940916PROMSG    *LIBL     KEY0019  19
290200940916PROMSG    *LIBL     KEY0020  20
290300940916PROMSG    *LIBL     KEY0021  21
290400940916PROMSG    *LIBL     KEY0022  22
290500940916PROMSG    *LIBL     KEY0023  23
290600940916PROMSG    *LIBL     KEY0024  24
290700940916**  ABILITAZIONE S1
2908009409160N  01
2909009409160N  02
2910009409161N  03
2911009409160N  04
2912009409160N  05
2913009409160N  06
2914009409160N  07
2915009411241N  08
2916009409160N  09
2917009601100N  10
2918009409160N  11
2919009409161N  12
2920000011221N  13
2921009409160N  14
2922009409160N  15
2923009409160N  16
2924009409161N  17
2925009409161N  18
2926009409160N  19
2927009409160N  20
2928009409160N  21
2929009409160N  22
2930009507261N  23   Impostare a '1' se si utilizzano opzioni
2931009507261N  24   Lasciare sempre impostata a '1' il valore della riga 24
293200940916** TASTI DI FUNZIONE  D3
293300940506PROMSG    *LIBL     KEY0001  01
293400940506PROMSG    *LIBL     KEY0002  02
293500940506PROMSG    *LIBL     KEY0003  03
293600940506PROMSG    *LIBL     KEY0004  04
293700940506PROMSG    *LIBL     KEY0005  05
293800940506PROMSG    *LIBL     KEY0006  06
293900940506PROMSG    *LIBL     KEY0007  07
294000940506PROMSG    *LIBL     KEY0008  08
294100940506PROMSG    *LIBL     KEY0009  09
294200940506PROMSG    *LIBL     KEY0010  10
294300940506PROMSG    *LIBL     KEY0011  11
294400940506PROMSG    *LIBL     KEY0012  12
294500940506PROMSG    *LIBL     KEY0013  13
294600940506PROMSG    *LIBL     KEY0014  14
294700940506PROMSG    *LIBL     KEY0015  15
294800940506PROMSG    *LIBL     KEY0016  16
294900940506PROMSG    *LIBL     KEY0017  17
295000940506PROMSG    *LIBL     KEY0018  18
295100940506PROMSG    *LIBL     KEY0019  19
295200940506PROMSG    *LIBL     KEY0020  20
295300940506PROMSG    *LIBL     KEY0021  21
295400940506PROMSG    *LIBL     KEY0022  22
295500940506PROMSG    *LIBL     KEY0023  23
295600940506PROMSG    *LIBL     KEY0024  24
295700940916**  ABILITAZIONE D3
2958009407200N  01
2959009407200N  02
2960009407201N  03
2961009407200N  04
2962009407200N  05
2963009407200N  06
2964009407200N  07
2965009407200N  08
2966009407200N  09
2967009409160N  10
2968009407200N  11
2969009407201N  12
2970009407200N  13
2971009407200N  14
2972009407200N  15
2973009407200N  16
2974009409160N  17
2975009409160N  18
2976009407200N  19
2977009407200N  20
2978009407200N  21
2979009407200N  22
2980009507260N  23   Impostare a '1' se si utilizzano opzioni
2981009507261N  24   Lasciare sempre impostata a '1' il valore della riga 24
