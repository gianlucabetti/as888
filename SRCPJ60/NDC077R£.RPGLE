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
000000060224C1913C                   PARM                    $ALLOPT         960
000000060224C1913C*
048600940131     C                   ENDSR
048700940506     C/EJECT
048800940506     C************************************************************
048900940506     C* RIEMPIMENTO TASTI FUNZIONE
049000940506     C************************************************************
049100940506     C     RIEKEY        BEGSR
049200940506     C*
049300940506     C* Input :
049400940506     C* - $FX = schiera con tutti i testi disponibili
049500940506     C* - $FC = schiere con i flag di validità associati ai testi
049600940506     C* - $FR = schiere con i flag di REVERSE IMAGE
049700940506     C* - $ULK = ultimo testo caricato
049800940721     C* - $L01 = Lunghezza prima riga
049900940721     C* - $L02 = Lunghezza seconda riga
050000940506     C* Output :
050100940506     C* - $ULK
050200940609     C* - $KEY1= 1° riga di tasti funzione
050300940609     C* - $KEY2= 2° riga di tasti funzione
050400940506     C*
050500950522     C                   CALLB     'XKEYMSG'
050600940506     C* input
050700940506     C                   PARM                    $FX
050800940506     C                   PARM                    $FC
050900940506     C                   PARM                    $FR
051000940506     C                   PARM                    $ULK              3 0
051100940506     C* output
051200940506     C                   PARM                    $KEY1            79
051300940506     C                   PARM                    $KEY2            79
051400950301     C* input
051500950726     C                   PARM                    $L01              3 0
051600950726     C                   PARM                    $L02              3 0
051700940506     C*
000000060224C1913C                   PARM                    $ALLFUNCT       480
000000060224C1913C*
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
056600940202     C                   WRITE     FMTT1
056700940224     C                   EXSR      GESPIE
056800940202     C                   MOVE      $LASTV        $LASTG
056900940202     C                   MOVE      'D1'          $LASTV
057000940707     C                   ENDIF
057100940707     C*
057200940707     C* se esistono errori sulla videata
057300940707     C* emetto la write del formato a indicatori spenti per vedere
057400940707     C* le eventuali decodifiche
057500940707     C     *IN99         IFEQ      *ON
057600940707     C                   MOVEA     *IN           WIN
057700940707     C                   MOVE      *ALL'0'       IN5098           49
057800940707     C                   MOVEA     IN5098        *IN(50)
057900940707     C                   WRITE     FMTD1
058000940707     C                   MOVEA     WIN           *IN
058100940707     C                   ENDIF
058200940131     C*
058300940131     C                   EXFMT     FMTD1
058400940506     C*
058500951005     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
058600951005     C* INPUT
058700951005     C* $FC1 = Abilitazioni videata D1
058800951005     C* OUTPUT
058900951005     C* *IN97 + *IN99 Errori
059000951005     C                   MOVEA     $FC1          $FC
059100951005     C                   EXSR      CTRKEY
059200940506     C*
059300940506     C     *IN99         IFEQ      *OFF
059400940131     C*
059500940117     C                   SELECT
059600940916     C* F2=Decodifica
059700940916     C     $TASTO        WHENEQ    F02
059800940916     C                   EXSR      F02D1
059900940131     C* F3=Fine
060000940506     C     $TASTO        WHENEQ    F03
060100940307     C                   EXSR      F03D1
060200940131     C* F4=Ricerca
060300940506     C     $TASTO        WHENEQ    F04
060400940127     C                   EXSR      SEARCH
060500940916     C* F5=Refresh
060600940916     C     $TASTO        WHENEQ    F05
060700940916     C                   EXSR      F05D1
060800941206     C* F8=Successivo
060900941206     C     $TASTO        WHENEQ    F08
061000941206     C                   EXSR      F08D1
061100940131     C* F12=Ritorno
061200940506     C     $TASTO        WHENEQ    F12
061300940307     C                   EXSR      F12D1
061400940131     C* F18=Modifica valori assunti
061500940506     C     $TASTO        WHENEQ    F18
061600940307     C                   EXSR      F18D1
061700940224     C* F24=Altri tasti
061800940506     C     $TASTO        WHENEQ    F24
061900940309     C                   EXSR      F24D1
062000940131     C*
062100940117     C                   OTHER
062200940127     C                   EXSR      CTRD1
062300940127     C     *IN99         IFEQ      *OFF
062400940131     C* definisco l'ordinamento
062500950410     C                   EXSR      DEFORD
062600950410     C* richiamo la lista
062700940127     C                   MOVE      'S1'          $GEST
062800940223     C                   MOVE      *ON           $INZS1
062900960110     C                   clear                   W1POSd
063000940211     C                   ENDIF
063100940224     C*
063200940117     C                   ENDSL
063300940224     C*
063400940506     C                   ENDIF
063500950525     C*
063600940117     C                   ENDSR
063700940224     C/EJECT
063800940224     C************************************************************
063900940224     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
064000940224     C************************************************************
064100940127     C     INZD1         BEGSR
064200940318     C*
064300940318     C                   CLEAR                   FMTD1
064400960118     C*
064500941013     C* Se pgm  richiamato con parametri forniti , li imposto nelle
064600941013     C*    parzializzazioni
064700941104     C                   SELECT
064800941213     C* il pgm è richiamato con chiave univoca per gestione POSTA
064900950606     C     $Unique       WHENEQ    *ON
065000941104     C* il pgm è richiamato e
065100941104     C* sono stati passati dei valori di parzializzazione ma non
065200941104     C* la chiave univoca
065300960118    >C     OPZ077        WHENNE    *BLANK
065400950403     C* imposto i valori passati sui campi a video
065500960109     C                   CALLb     'XDT4000'
065600960118     C                   PARM      dtRegD077     XDTAMG
065700960118     C     D1DtRegDA     PARM                    XDTGMA
065800960109     C                   PARM      2             XDTSTA
065900960109     C                   CALLb     'XDT4000'
066000960118     C                   PARM      dtRegA077     XDTAMG
066100960118     C     D1DtRegA      PARM                    XDTGMA
066200960109     C                   PARM      2             XDTSTA
066300960118    >C                   MOVE      CausTes077    D1CausTes
066400960118    >C                   MOVE      TpRegz077     D1TpRegz
066500960213    >C                   MOVE      Rifint077     D1RifInt
066600941104     C* il pgm non è richiamato
066700960118    >C     OPZ077        WHENEQ    *BLANK
066800950403     C* imposto dei valori di default
066900941115     C                   ENDSL
067000940224     C*
067100940117     C                   ENDSR
067200940916     C/EJECT
067300940916     C************************************************************
067400940916     C* GESTIONE F02 VIDEO D1
067500940916     C************************************************************
067600940916     C     F02D1         BEGSR
067700940916     C*
067800940916     C* per decodificare la videata richiamo la routine dei ctrl
067900940916     C* e poi disattivo gli eventuali messaggi di errore
068000940916     C                   EXSR      CTRD1
068100940916     C*
068200940916     C                   MOVE      *ALL'0'       IN5098
068300940916     C                   MOVEA     IN5098        *IN(50)
068400940916     C*
068500940916     C                   ENDSR
068600940224     C/EJECT
068700940131     C************************************************************
068800940307     C* GESTIONE F03 VIDEO D1
068900940131     C************************************************************
069000940307     C     F03D1         BEGSR
069100940131     C*
069200940307     C                   MOVE      *ON           $FINE
069300960118    >C                   MOVE      '1'           RET077
069400940325     C* fine programma
069500940325     C                   EXSR      ENDPGM
069600940307     C*
069700940307     C                   ENDSR
069800940916     C/EJECT
069900940916     C************************************************************
070000940916     C* GESTIONE F05 VIDEO D1
070100940916     C************************************************************
070200940916     C     F05D1         BEGSR
070300940916     C*
070400940916     C                   MOVE      *ON           $INZD1
070500940916     C*
070600940916     C                   ENDSR
070700941206     C/EJECT
070800941206     C************************************************************
070900941206     C* GESTIONE F08 VIDEO D1
071000941206     C************************************************************
071100941206     C     F08D1         BEGSR
071200941206     C*
071300941206     C                   MOVE      *ON           $FINE
071400960118    >C                   MOVE      '0'           RET077
071500941206     C*
071600941206     C                   ENDSR
071700940307     C/EJECT
071800940307     C************************************************************
071900940307     C* GESTIONE F12 VIDEO D1
072000940307     C************************************************************
072100940307     C     F12D1         BEGSR
072200940307     C*
072300940307     C*  se la 1a videata è la lista oppure sono arrivato qui con F17,
072400940307     C*  per F12 torno alla lista
072500940307     C     $LOP          IFEQ      2
072600940307     C     WINKR         OREQ      *ON
072700940307     C                   MOVE      'S1'          $GEST
072800940307     C*  altrimenti, per F12 torno a menu
072900940307     C                   ELSE
073000940307     C                   MOVE      *ON           $FINE
073100960118    >C                   MOVE      '2'           RET077
073200941027     C                   ENDIF
073300940307     C*
073400940307     C                   ENDSR
073500940307     C/EJECT
073600940307     C************************************************************
073700940307     C* GESTIONE F18 VIDEO D1
073800940307     C************************************************************
073900940307     C     F18D1         BEGSR
074000940307     C*
074100940307     C                   MOVE      'D2'          $GEST
074200940307     C                   MOVEL     *ON           $INZD2
074300940307     C*
074400940307     C                   ENDSR
074500940307     C/EJECT
074600940307     C************************************************************
074700940307     C* GESTIONE F24 VIDEO D1
074800940609     C* input
074900940509     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
075000940509     C* $FX1 =Testi tasti funzione
075100940509     C* $FC1 = Abilitazioni
075200940509     C* $FR1 = Inversioni di fondo
075300940307     C************************************************************
075400940307     C     F24D1         BEGSR
075500940506     C*
075600940506     C                   MOVEA     $FX1          $FX
075700940506     C                   MOVEA     $FC1          $FC
075800940506     C                   MOVEA     $FR1          $FR
075900940506     C                   Z-ADD     $ULKD1        $ULK
076000950726     C                   Z-ADD     $D1L01        $L01
076100950726     C                   Z-ADD     $D1L02        $L02
076200940506     C                   EXSR      RIEKEY
076300940506     C                   Z-ADD     $ULK          $ULKD1            3 0
076400940720     C                   MOVEA     $FC           $FC1
076500940506     C                   MOVEL     $KEY1         Z1KE1
000000060224C1913C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
076600940506     C                   MOVEL     $KEY2         Z1KE2
076700940506     C                   WRITE     FMTZ1
076800940506     C*
076900940307     C*
077000940307     C                   ENDSR
077100940307     C/EJECT
077200940307     C************************************************************
077300940307     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
077400950613     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
077500960109     C* 93 94 96 97 98
077600940307     C************************************************************
077700940307     C     CTRD1         BEGSR
077800940307     C*
077900940127     C                   SETOFF                                       99
078000940127     C* non imposto pos. del cursore
078100940127     C                   Z-ADD     0             H1RIGA
078200940127     C                   Z-ADD     0             H1COLO
078300940131     C*
078400960118     c* Data registrazione da
078500960109     C*
078600960118     c                   eval      dtRegdaR = Datmin
078700960118     c                   if        d1dtRegda <> 0
078800960109     C                   CALLb     'XDT4000'
078900960118     C     dtRegDAR      PARM                    XDTAMG
079000960118     C                   PARM      d1dtRegDA     XDTGMA
079100960109     C                   PARM      3             XDTSTA
079200960110     C                   IF        xdtsta < 0
079300960109     C                   SETON                                        5199
079400960109     C                   Endif
079500960109     C                   Endif
079600960109     C*
079700960118     c* Data registrazione a
079800960109     C*
079900960118     c                   eval      dtRegaR = Datmax
080000960118     c                   if        d1dtRega <> 0
080100960109     C                   CALLb     'XDT4000'
080200960118     C     dtRegAr       PARM                    XDTAMG
080300960118     C                   PARM      d1dtRega      XDTGMA
080400960109     C                   PARM      3             XDTSTA
080500960110     C                   IF        xdtsta < 0
080600960109     C                   SETON                                        5299
080700960109     C                   Endif
080800960109     C                   Endif
080900960109     C*
081000960109     c* Limiti errati
081100960118     c                   if        dtregaR  < dtregdaR
081200960109     C                   SETON                                        5399
081300960109     c                   endif
081400960118     c* causale
081500960118     C                   CLEAR                   D1CaustesD
081600960118     c                   if        d1caustes <> *blanks
081700960118     C                   EVAL      $LenOut = %Size(Anope)
081800960118     C                   CALLB     'NDMVC003'
081900960118     C                   PARM                    xscsoc
082000960118     C                   PARM                    d1caustes
082100960118     C                   PARM      datmin        $date
082200960118     C                   PARM                    Uso               1
082300960118     C                   PARM      *on           $gesmsg
082400960118     C                   PARM      *off          $esito
082500960118     C                   PARM      'ANOPE'       $Struttura
082600960118     C                   PARM                    ANOPE
082700960118     C                   PARM                    $LenOut
082800960118     C                   PARM                    $Refresh
082900960118     c                   if        $esito = *on
083000960118     c                   seton                                        5499
083100960118     c                   else
083200960118     c                   eval      d1caustesd = opedesbrev
083300960118     c                   endif
083400960118     c                   endif
083500960118     C*
083600960118     C* flag tipo registrazione
083700960118     C*
083800960118     c                   clear                   d1tpregzd
083900960118     C                   RESET                   X07ValDS
084000960118     C                   MOVE      '1'           X07RIC
084100960118     C                   MOVEL     'NDREG000'    X07TRC
084200960118     C                   MOVEL     'REGTPREGZ'   X07CAM
084300960118     C                   MOVEL     D1TpRegz      X07VAL
084400960118     C                   MOVEL     *on           X07All
084500961011     C                   CALLb     'X07VALR'
084600960118     C                   PARM                    X07ValDS
084700960118   B C     X07ERR        IFEQ      '0'
084800960118     C                   MOVEL     X07DES        D1TpRegzD
084900960118     c                   else
085000960118     c                   seton                                        5599
085100960118     c                   endif
085200960109     c*
085300940117     C                   ENDSR
085400960109     C/EJECT
085500950410     C************************************************************
085600950410     C* DEFINIZIONE DELL'ORDINAMENTO
085700950410     C************************************************************
085800950410     C     DEFORD        BEGSR
085900950410     C*
086000950410     C* definisco l'ordinamento della lista a seconda dei valori
086100950410     C* impostati nel video di parzializzazione
086200960118     C* Qui l'ordinamento è uno solo: per data reg
086300950410     C                   Z-ADD     1             $ORD
086400960109     c*
086500950410     C                   ENDSR
086600940224     C/EJECT
086700940127     C************************************************************
086800940131     C* GESTIONE LISTA
086900940127     C************************************************************
087000940127     C     GESS1         BEGSR
087100940223     C* inizializzazione videata
087200940223     C     $INZS1        IFEQ      *ON
087300940127     C                   EXSR      INZS1
087400940223     C                   MOVE      *OFF          $INZS1
087500940127     C                   ENDIF
087600940223     C* emissione piede videata se proveniente da altra
087700940223     C* salvataggio valore formato di provenienza
087800940202     C     $LASTV        IFNE      'S1'
087900940202     C                   WRITE     FMTT1
088000940224     C                   EXSR      GESPIE
088100940202     C                   MOVE      $LASTV        $LASTG
088200940202     C                   MOVE      'S1'          $LASTV
088300941027     C                   ENDIF
088400940223     C*
088500940223     C     WMAX          IFEQ      0
088600940211     C                   WRITE     FMTD3                                        NO RCD
088700940211     C                   ENDIF
088800980720     C*
088900980720     C* Inizializzazione indice tasti funzione personalizzati
089000980720     C                   Eval      PosFun = 25
089100940127     C*
089200940131     C                   EXFMT     FMTC1
089300940204     C     C1NRR         IFNE      0
089400940204     C                   Z-ADD     C1NRR         WSFL
089500940204     C                   ENDIF
089600951020     C                   IF        SFLNRR > 0
089700951020     C                   Z-ADD     SFLNRR        C1RCD
089800951020     C                   ENDIF
089900960119     C                   MOVE      '1'           Wxmsginz
090000940506     C*
090100951005     C* CONTROLLO ABILITAZIONE TASTI
090200951005     C* INPUT
090300951005     C* $FC2 = Abilitazioni videata S1
090400951005     C* OUTPUT
090500951005     C* *IN97 + *IN99 Errori
090600951005     C                   MOVEA     $FC2          $FC
090700951005     C                   EXSR      CTRKEY
090800941205     C*
090900940506     C     *IN99         IFEQ      *OFF
091000940127     C*
091100941027     C                   SELECT
091200940127     C* F3=Fine
091300940506     C     $TASTO        WHENEQ    F03
091400940309     C                   EXSR      F03S1
091500941206     C* F8=Successivo
091600941206     C     $TASTO        WHENEQ    F08
091700941206     C                   EXSR      F08S1
091800940131     C* F12=Ritorno
091900940506     C     $TASTO        WHENEQ    F12
092000940309     C                   EXSR      F12S1
092100001122d0771C* F13=Selezione multipla
092200001122  "  C     $TASTO        WHENEQ    F13
092300001127  "  C                   Eval      $TastoSav=$Tasto
092400001122d0771C                   EXSR      F13S1
092500940131     C* F17=Parzializzazioni
092600940506     C     $TASTO        WHENEQ    F17
092700940309     C                   EXSR      F17S1
092800940131     C* F18=Modifica valori assunti
092900940506     C     $TASTO        WHENEQ    F18
093000940309     C                   EXSR      F18S1
093100940224     C* F23=Altre opzioni
093200940506     C     $TASTO        WHENEQ    F23
093300940309     C                   EXSR      F23S1
093400940224     C* F24=Altri tasti
093500940506     C     $TASTO        WHENEQ    F24
093600940309     C                   EXSR      F24S1
093700940127     C* ROLL-UP
093800940506     C     $TASTO        WHENEQ    ROLLUP
093900940127     C                   EXSR      ROLS1
094000980720     C*
094100980720     C* Personalizzata
094200980720     C     $PGF2(PosFun) Whenne    *BLANK
094300980720     C                   EXSR      FperS1
094400940127     C*
094500941027     C                   OTHER
094600940127     C* CONTROLLO DATI
094700940131     C                   EXSR      CTRC1
094800940201     C     *IN99         IFEQ      *OFF
094900940131     C                   EXSR      CTRS1
095000941027     C                   ENDIF
095100941205     C*
095200941027     C                   ENDSL
095300940127     C*
095400940506     C                   ENDIF
095500941205     C*
095600940127     C                   ENDSR
095700940224     C/EJECT
095800980720     C************************************************************
095900980720     C* Chiamata programma con tasto personalizzato
096000980720     C************************************************************
096100980720     C     FPerS1        BEGSR
096200980720     C*
096300980720     C* salvo la DS di ingresso
096400980720    >C                   MOVEL     ndc077ds      $SaveDS
096500980720     C*
096600980720    >C                   MOVEL     ndc077ds      KPJBU
096700980720    >C                   CALL      $PGF2(PosFun)                        22
096800980720     C                   PARM                    KPJBA
096900980720     C                   PARM                    NOMPGM
097000980720     C*
097100980720     C* ripristino la DS di ingresso
097200980720    >C                   MOVEL     $SaveDS       ndc077ds
097300980720     C*
097400980720     C                   ENDSR
097500980720     C/EJECT
097600940127     C************************************************************
097700940131     C* INIZIALIZZAZIONE LISTA
097800940127     C************************************************************
097900940127     C     INZS1         BEGSR
098000001127d0771C*appoggio x F13
098100001127  "  C                   Clear                   $TastoSav
098200001127d0771C                   eval      PrimaVolta=0
098300940302     C* pulizia SFL
098400940128     C                   SETOFF                                         3031
098500940131     C                   WRITE     FMTC1
098600940128     C                   SETON                                          31
098700940127     C*
098800940128     C* riempimento opzioni
098900940223     C                   MOVEA     $TX1          $TX
099000940223     C                   MOVEA     $FL1          $FL
099100940309     C                   Z-ADD     0             WCOZ
099200940225     C*  equivale al num.di elementi della schiera $TX1
099300980720     C                   Z-ADD     OPZLEN        WMXO
099400950906     C                   Z-ADD     $C1Op1        $L01
099500950906     C                   Z-ADD     $C1Op2        $L02
099600940128     C                   EXSR      RIEOPZ
099700941214     C                   Z-ADD     23            X
099800941214     C                   MOVE      FF23          $FC2(X)
099900940608     C                   MOVEL     WOPZ1         C1OPZ1
000000060224C1913C                   MOVEL     $ALLOPT       H1ALLOPT
100000940608     C                   MOVEL     WOPZ2         C1OPZ2
100100940225     C                   Z-ADD     WCOZ          C1COZ
100200940128     C*
100300940128     C* CARICAMENTO SFL A PAGINE
100400940128     C*
100500940201     C                   Z-ADD     0             S1NRR
100600940128     C                   Z-ADD     0             WMAX
100700940224     C*
100800940224     C* Posizionamento su file pilota
100900960110     C                   clear                   C1POSD
101000941214     C                   EXSR      SETANA
101100940302     C* ripristino i valori del posizionamento
101200960109     C                   MOVE      W1POSD        C1POSD
101300940131     C*
101400940201     C* Lettura file pilota
101500941214     C                   EXSR      REDANA
101600940131     C*
101700940131     C* Carico 1a pagina
101800940127     C                   EXSR      ROLS1
101900940201     C                   Z-ADD     SFLPAG        WPAG
102000940127     C*
102100940127     C                   ENDSR
102200940127     C/EJECT
102300940127     C************************************************************
102400940131     C* CARICAMENTO PAGINA LISTA
102500940127     C************************************************************
102600940127     C     ROLS1         BEGSR
102700940127     C*
102800940128     C                   SETOFF                                       32
102900940223     C                   Z-ADD     0             Y
103000940127     C                   Z-ADD     WMAX          S1NRR
103100940127     C*
103200940127     C* Leggo dal file anagrafico per caricare la lista
103300940127     C*
103400950606     C     $EndFile      DOWEQ     *OFF
103500940201     C     Y             ANDLT     WPAG
103600940127     C*
103700940131     C                   EXSR      RIES1
103800940127     C*
103900940127     C                   ADD       1             S1NRR
104000940127     C                   ADD       1             Y
104100940127     C*
104200940131     C                   WRITE     FMTS1
104300940131     C*
104400960118     c* Mi posiziono sul subnumero successivo
104500960118     c     K04bhm02      setgt     ndbhm02
104600941214     C                   EXSR      REDANA
104700940128     C*
104800941027     C                   ENDDO
104900940127     C*
105000940223     C                   Z-ADD     S1NRR         WMAX                 30
105100940127     C*
105200940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
105300940127     C*
105400940201     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
105500940127     C                   MVR                     RESTO             3 0
105600940131     C     PAGINE        MULT      SFLPAG        C1RCD
105700941027     C     RESTO         IFGT      0
105800940127     C                   ADD       1             C1RCD
105900941027     C                   ELSE
106000940407     C                   SUB       SFLPAG        C1RCD
106100940407     C                   ADD       1             C1RCD
106200941027     C                   ENDIF
106300940128     C*
106400940127     C                   ENDSR
106500940224     C/EJECT
106600940224     C************************************************************
106700940224     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
106800940224     C************************************************************
106900941214     C     SETANA        BEGSR
107000960118     C                   eval      bhmsocieta = xscsoc
107100940224     C*
107200940224     C                   SELECT
107300941214     C*
107400941214     C* Gestione posizionamento
107500950301     C* Se il campo W1POS è > *blank imposto il posizionamento con
107600950301     C* W1POS altrimenti lo imposto con il valore di parametrizzazione
107700941214     C*
107800960110    >C* Ord.1 = chiave ordinamento 1  data
107900940224     C     $ORD          WHENEQ    1
108000960109     C     W1POSD        IFNE      0
108100960109     C                   CALLb     'XDT4000'
108200960118     C     BHMDTREG      PARM                    XDTAMG
108300960109     C                   PARM      w1POSD        XDTGMA
108400960109     C                   PARM      4             XDTSTA
108500940302     C                   ELSE
108600960109     C                   CALLb     'XDT4000'
108700960118     C     BHMDTREG      PARM                    XDTAMG
108800960118     C                   PARM      d1dtREGDa     XDTGMA
108900960109     C                   PARM      4             XDTSTA
109000941027     C                   ENDIF
109100960118    >C     K02BHM02      SETLL     NDBHM02
109200960109     c*
109300940224     C                   ENDSL
109400940224     C*
109500940224     C                   ENDSR
109600940128     C/EJECT
109700940128     C************************************************************
109800940131     C* LETTURA RCD ARCHIVIO PILOTA
109900940128     C************************************************************
110000941214     C     REDANA        BEGSR
110100940128     C*
110200950606     C                   MOVEL     *OFF          $EndFile
110300940131     C                   MOVEL     *OFF          $RCDOK
110400940131     C*
110500950606     C     $EndFile      DOUEQ     *ON
110600940131     C     $RCDOK        OREQ      *ON
110700940131     C*
110800941027     C                   SELECT
110900940131     C     $ORD          WHENEQ    1
111000960118    >C     Bhmsocieta    READE     NdBhm02                                21
111100941027     C                   ENDSL
111200940131     C*
111300950606     C                   MOVE      *IN21         $EndFile
111400940131     C*
111500950606     C     $EndFile      IFEQ      *OFF
111600941214     C                   EXSR      SELANA
111700941027     C                   ENDIF
111800940131     C*
111900941027     C                   ENDDO
112000940131     C*
112100940131     C                   MOVE      *IN21         *IN33                          SflEnd
112200940131     C*
112300940131     C                   ENDSR
112400940224     C/EJECT
112500940224     C************************************************************
112600940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
112700940224     C************************************************************
112800940224     C     CLCPAG        BEGSR
112900940224     C* Input :
113000940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
113100940224     C*          cursore
113200940224     C* - SFLPAG = numero rcd per pagina sfl
113300940224     C* Output :
113400940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
113500940224     C*
113600940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
113700940224     C                   MVR                     RESTO             3 0
113800940224     C     RESTO         IFGT      0
113900940224     C                   ADD       1             PAGINE
114000940224     C                   ENDIF
114100940224     C     PAGINE        MULT      SFLPAG        WPAG
114200940224     C*
114300940224     C                   ENDSR
114400940131     C/EJECT
114500940131     C************************************************************
114600940131     C* SELEZIONI SULLA RIGA LETTA
114700940314     C* impostare a *OFF il campo $RCDOK per scartare un record che
114800940314     C* non rientra nei parametri di parzializzazione
114900940314     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
115000940314     C* superiore stabilito nei parametri
115100940314     C*
115200940131     C************************************************************
115300941214     C     SELANA        BEGSR
115400940131     C*
115500940202     C                   MOVEL     *ON           $RCDOK
115600940131A1211X*                  MOVEL     *ALL'9'       ALL9             15
115600940131A1211C                   MOVEL     *hival        ALL9             15
115700940314     C*
115800940314     C                   SELECT
115900950301     C*
116000940314     C* SELEZIONI RISPETTO ALLE PARZIALIZZAZIONI
116100960109     C*
116200960109    >c* Data da
116300960118     c                   when      Bhmdtreg < DtregdaR
116400960109     C                   MOVE      *off          $RcdOk
116500960109    >c* Data a
116600960118     c                   when      Bhmdtreg > DtRegaR
116700960109     C                   MOVE      *off          $RcdOk
116800960118    >c* Causale
116900960118     c                   when      d1Caustes <> *blanks and BhmCaustes<>
117000960118     C                             d1Caustes
117100960109     C                   MOVE      *off          $RcdOk
117200960118    >c* tipo registrazione
117300960118     c                   when      d1tpregz<> *blanks and BhmTpRegz<>
117400960118     C                             d1tpregz
117500960109     C                   MOVE      *off          $RcdOk
117600960213    >c* Riferimento interno
117700960213     c                   when      d1RifInt <> *blanks and BhmRifInt <>
117800960213     C                             d1RifInt
117900960213     C                   MOVE      *off          $RcdOk
118000940202     C*
118100940202     C                   ENDSL
118200950503     C*
118300950503     C* FACILITAZIONE NEGLI ACCESSI
118400950503     C*
118500950314     C                   SELECT
118600960109    >C*
118700960109    >c* data  > del limite superiore
118800960109    >C                   WHEN      $ord = 1 and
118900960118     c                             BhmDtreg > DtRegaR
119000960118    >C     *HIVAL        SETLL     ndBhm02L
119100950314     C                   ENDSL
119200940202     C*
119300940202     C                   ENDSR
119400940131     C/EJECT
119500940131     C************************************************************
119600940131     C* RIEMPIMENTO RIGA LISTA
119700940131     C************************************************************
119800940131     C     RIES1         BEGSR
119900940131     C*
120000940131     C                   CLEAR                   FMTS1
120100950301     C*
120200960118     C                   eval      H1rifint = bhmrifint
120300960118     C                   eval      H1subnum = bhmsubnum
120400960109     C                   CALLb     'XDT4000'
120500960118     C                   PARM      bhmDtreg      XDTAMG
120600960118     C     s1dtreg       PARM                    XDTGMA
120700960109     C                   PARM      2             XDTSTA
120800960118     C                   eval      s1nrreg  = bhmnrreg
120900960118     C                   eval      s1serreg = bhmserreg
121000960118     C                   eval      s1tpregz = bhmtpregz
121100960118     C                   eval      s1caustes= bhmcaustes
121200960119     c*
121300960109     C                   CALLb     'XDT4000'
121400960118     C                   PARM      Bhmdtdoc      XDTAMG
121500960118     C     s1dtdoc       PARM                    XDTGMA
121600960109     C                   PARM      2             XDTSTA
121700960118     C                   eval      s1nrdoc  = bhmnrdoc
121800960118     C                   eval      s1serdoc = bhmserdoc
121900960118     C                   eval      s1unita  = bhmunita
122000980618     C                   eval      s1rifint = bhmrifint
122100001122d0771C*
122200001122  "  C* imposto anche l'opzione perchè se avessi premuto l'F13
122300001122  "  C* dovrei caricare l'opzione da replicare
122400001122  "  C                   if        $OpzOk   =    *On
122500001122  "  C                   Eval      S1Opz    =    WOpz
122600001122  "  C                   endif
122700001122  "  C                   if        S1Opz     <>  0
122800001122  "  C                   SetOn                                        32
122900001122d0771C                   endif
123000950301     C*
123100950301     C* imposto quegli attributi del sfl che sono modificabili
123200950301     C* al richiamo del determinato pgm
123300950301     C                   EXSR      ATRS1
123400940131     C*
123500940131     C                   ENDSR
123600950301     C/EJECT
123700950301     C************************************************************
123800950301     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
123900950301     C************************************************************
124000950301     C     ATRS1         BEGSR
124100950301     C*
124200950301     C*
124300950301     C                   ENDSR
124400941206     C/EJECT
124500941206     C************************************************************
124600941206     C* GESTIONE F08 VIDEO S1
124700941206     C************************************************************
124800941206     C     F08S1         BEGSR
124900941206     C*
125000941206     C                   MOVE      *ON           $FINE
125100960118    >C                   MOVE      '0'           RET077
125200941206     C*
125300941206     C                   ENDSR
125400940309     C/EJECT
125500940309     C************************************************************
125600940309     C* GESTIONE F03 VIDEO S1
125700940309     C************************************************************
125800940309     C     F03S1         BEGSR
125900940309     C*
126000940309     C                   MOVE      *ON           $FINE
126100960118    >C                   MOVE      '1'           RET077
126200940325     C* fine programma
126300940325     C                   EXSR      ENDPGM
126400940309     C*
126500940309     C                   ENDSR
126600940309     C/EJECT
126700940309     C************************************************************
126800940309     C* GESTIONE F12 VIDEO S1
126900940309     C************************************************************
127000940309     C     F12S1         BEGSR
127100940309     C*
127200940309     C*  se la 1a videata sono le parzializzazioni, per F12 torno a queste
127300940309     C     $LOP          IFEQ      1
127400940309     C                   MOVE      'D1'          $GEST
127500940309     C*  se la 1a videata è questa, per F12 torno a menu
127600940309     C                   ELSE
127700940309     C                   MOVE      *ON           $FINE
127800960118    >C                   MOVE      '2'           RET077
127900941027     C                   ENDIF
128000940309     C*
128100940309     C                   ENDSR
128200940309     C/EJECT
128300001122d0771C************************************************************
128400001122  "  C* GESTIONE F13 VIDEO S1
128500001122  "  C************************************************************
128600001122  "  C     F13S1         BEGSR
128700001122  "  C*
128800001122  "  C* E' simile alla funzione F13 del PDM: riporta l'opzione della
128900001122  "  C* riga del sfl su cui è posizionato il cursore dalla riga
129000001122  "  C* successiva fino alla fine del sfl stesso,
129100001122  "  C* senza sovrapporsi alle opzioni preesistenti.
129200001122  "  C*
129300001122  "  C* reset indicatori DSPATR
129400001122  "  C                   MOVE      *ALL'0'       IN4049           10
129500001122  "  C                   MOVEA     IN4049        *IN(40)
129600001122  "  C*
129700001122  "  C* il controllo lo faccio solo se il sfl NON è vuoto
129800001122  "  C                   If        WMax    <>    0
129900001122  "  C* salvo il numero del record su cui sono posizionato
130000001122  "  c                   Z-Add     C1Nrr         WC1Rcd
130100001122  "  C* leggo il rcd
130200001122  "  C     C1NrR         CHAIN     FMTS1                              21
130300001122  "  C* salvo l'opzione e il numero relativo
130400001122  "  C                   If        S1Opz   >     0
130500001122  "  C                   Z-Add     S1Opz         WOpz
130600001122  "  c                   Z-Add     C1Nrr         $X
130700001122  "  C*controllo autorizzazione all'opzione
130800001122  "  C                   Exsr      CtrAutPno
130900001122  "  C* attivo SflNxtChg
131000001122  "  C                   SetOn                                        32
131100001122  "  C                   UpDate    FmtS1
131200001122  "  C                   SetOff                                       32
131300001122  "  C                   if        $OpzOk    =    *On
131400001122  "  C*
131500001122  "  C* mi posiziono sul record successivo
131600001122  "  C                   Add       1             $X
131700001122  "  c*
131800001122  "  C                   DoW       $X      <=    WMax
131900001122  "  C     $X            Chain     FmtS1                              21
132000001122  "  C   21              Leave
132100001122  "  C                   if        S1Opz    =    0
132200001122  "  C                   Z-Add     WOpz          S1Opz
132300001122  "  C                   endif
132400001122  "  C* attivo SflNxtChg
132500001122  "  C                   SetOn                                        32
132600001122  "  C                   UpDate    FmtS1
132700001122  "  C                   SetOff                                       32
132800001122  "  C*
132900001122  "  C                   Add       1             $X
133000001122  "  C                   enddo
133100001122  "  c*
133200001122  "  C                   Eval      SflPag   =    9999
133300001122  "  C                   Eval      WPag     =    9999
133400001122  "  C*
133500001122  "  C                   ExSr      RolS1
133600001122  "  C* reimposto sflpag
133700001122  "  C                   Eval      SflPag   =    10
133800001122  "  C                   Eval      WPag     =    SflPag
133900001122  "  C*
134000001122  "  C* ripristino la posizione iniziale del record
134100001122  "  c                   Z-Add     WC1Rcd        C1Rcd
134200001122  "  C* ripristino il campo di salvataggio dell'opzione per non sporca
134300001122  "  C* re il campo S1Opz in caso di ricaricamento del sfl
134400001122  "  C                   Z-Add     0             WOpz
134500001122  "  c*
134600001122  "  C                   ENDIF
134700001122  "  C*
134800001122  "  C                   endif
134900001122  "  C*
135000001122  "  C                   endif
135100001122  "  C*
135200001122  "  C                   ENDSR
135300001122d0771C/EJECT
135400940309     C************************************************************
135500940309     C* GESTIONE F17 VIDEO S1
135600940309     C************************************************************
135700940309     C     F17S1         BEGSR
135800940309     C*
135900940309     C                   MOVE      'D1'          $GEST
136000940309     C*
136100940309     C                   ENDSR
136200940309     C/EJECT
136300940309     C************************************************************
136400940309     C* GESTIONE F18 VIDEO S1
136500940309     C************************************************************
136600940309     C     F18S1         BEGSR
136700940309     C*
136800940309     C                   MOVE      'D2'          $GEST
136900940309     C                   MOVEL     *ON           $INZD2
137000940309     C*
137100940309     C                   ENDSR
137200940309     C/EJECT
137300940309     C************************************************************
137400940309     C* GESTIONE F23 VIDEO S1
137500940309     C************************************************************
137600940309     C     F23S1         BEGSR
137700940309     C*
137800950906     C                   Z-ADD     $C1Op1        $L01
137900950906     C                   Z-ADD     $C1Op2        $L02
138000940309     C                   EXSR      RIEOPZ
138100941214     C                   Z-ADD     23            X
138200941214     C                   MOVE      FF23          $FC2(X)
138300940608     C                   MOVEL     WOPZ1         C1OPZ1
000000060224C1913C                   MOVEL     $ALLOPT       H1ALLOPT
138400940608     C                   MOVEL     WOPZ2         C1OPZ2
138500940309     C                   Z-ADD     WCOZ          C1COZ
138600940309     C*
138700940309     C                   ENDSR
138800940309     C/EJECT
138900940309     C************************************************************
139000940309     C* GESTIONE F24 VIDEO S1
139100940309     C************************************************************
139200940309     C     F24S1         BEGSR
139300940506     C*
139400940506     C                   MOVEA     $FX2          $FX
139500940506     C                   MOVEA     $FC2          $FC
139600940506     C                   MOVEA     $FR2          $FR
139700940506     C                   Z-ADD     $ULKS1        $ULK
139800950726     C                   Z-ADD     $S1L01        $L01
139900950726     C                   Z-ADD     $S1L02        $L02
140000940506     C                   EXSR      RIEKEY
140100940506     C                   Z-ADD     $ULK          $ULKS1            3 0
140200940720     C                   MOVEA     $FC           $FC2
140300940506     C                   MOVEL     $KEY1         Z2KE1
000000060224C1913C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
140400940506     C                   MOVEL     $KEY2         Z2KE2
140500940506     C                   WRITE     FMTZ2
140600940506     C*
140700940309     C                   ENDSR
140800940128     C/EJECT
140900940128     C************************************************************
141000940131     C* CONTROLLO TESTATA LISTA
141100940128     C************************************************************
141200940131     C     CTRC1         BEGSR
141300940128     C*
141400940201     C                   MOVE      *OFF          *IN99
141500960109     C*
141600960109     C                   if        c1posd <> 0
141700960109     C                   CALLb     'XDT4000'
141800960109     C                   PARM                    XDTAMG           10
141900960109     C                   PARM      c1posd        XDTGMA            6 0
142000960109     C                   PARM      3             XDTSTA            1 0
142100960109     C     XDTSTA        IFLT      0
142200960109     C                   SETON                                        6099
142300960109     C                   endif
142400960109     C                   endif
142500940131     C*
142600940202     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
142700940202     C* L'INIZIALIZZAZIONE LISTA
142800940202     C*
142900960109     c                   if        *in99 = *off
143000960118     C     C1POSD        IFNE      W1POSD
143100940202     C                   EXSR      POSS1
143200941027     C                   ENDIF
143300960109     C                   endif
143400940202     C*
143500940202     C                   ENDSR
143600940202     C/EJECT
143700940202     C************************************************************
143800940202     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
143900940202     C************************************************************
144000940202     C     POSS1         BEGSR
144100940202     C*
144200940202     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
144300940202     C*
144400941118   B1C     WMAX          IFGT      0
144500940202     C                   READC     FMTS1                                  21
144600941118   B2C     *IN21         DOWEQ     *OFF
144700940202     C     *IN99         ANDEQ     *OFF
144800940202     C*
144900941118   B3C     S1OPZ         IFNE      0
145000960109     C                   SETON                                        92  99
145100941118   E3C                   ENDIF
145200940202     C* se rilevata opzione attiva, attivo sflnxtchg
145300941118   B3C     *IN99         IFEQ      *ON
145400940202     C                   MOVE      *ON           *IN32
145500950301     C*
145600950301     C* imposto quegli attributi del sfl che sono modificabili
145700950301     C* al richiamo del determinato pgm
145800941118     C                   EXSR      ATRS1
145900941118     C*
146000940202     C                   UPDATE    FMTS1
146100941118   X3C                   ELSE
146200940202     C                   READC     FMTS1                                  21
146300941118   E3C                   ENDIF
146400940202     C*
146500941118   E2C                   ENDDO
146600940202     C*
146700941118   E1C                   ENDIF
146800940202     C*
146900940221     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
147000940221     C* AL VALORE DI POSIZIONAMENTO :
147100940221     C* se questo è maggiore del valore del rispettivo campo di
147200940221     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
147300940221     C* con i valori di parzializzazione.
147400940221     C*
147500941118   B1C     *IN99         IFEQ      *OFF
147600960110    >C                   clear                   W1POSD
147700940202     C*
147800950525     C* per ogni ordinamento devo testare se il campo dell'inizio elenco
147900950525     C* è maggiore del relativo valore di parzializzazione, nel qual caso
148000950525     C* imposto la variabile del posizionamento (W1Pos) al nuovo valore;
148100950525     C* richiedo comunque un ricaricamento del sfl che, se non si è
148200950525     C* verificata la condizione suddetta avverrà con posizionamento a
148300950525     C* partire dal valore di parzializzazione
148400950525     C*
148500941118   B2C                   SELECT
148600940202     C*
148700940202     C     $ORD          WHENEQ    1
148800960109    >C                   MOVEL     C1POSD        W1POSD
148900950525     C*
149000941118   E2C                   ENDSL
149100950525     C*
149200950525     C* richiedo l'inizializzazione sfl
149300950525     C                   MOVE      *ON           $INZS1
149400940202     C*
149500941118   E1C                   ENDIF
149600940202     C*
149700940131     C                   ENDSR
149800940131     C/EJECT
149900940131     C************************************************************
150000940131     C* CONTROLLO OPZIONI LISTA
150100940131     C************************************************************
150200940131     C     CTRS1         BEGSR
150300940131     C*
150400940202     C                   MOVEL     *OFF          $ESCI
150500940201     C                   SETOFF                                       99
150600940131     C                   Z-ADD     0             S1OPZ
150700940131     C*
150800940127     C* Leggo il sfl solo se ci sono rcd
150900941118   B1C     WMAX          IFGT      0
151000940131     C                   READC     FMTS1                                  21
151100940127     C*
151200940131     C* esce se fine sfl o errore che richiede l'uscita
151300941118   B2C     *IN21         DOWEQ     *OFF
151400940131     C     $ESCI         ANDEQ     *OFF
151500940201     C                   Z-ADD     S1NRR         C1RCD
151600940131     C* ctrl su riga
151700940131     C                   EXSR      RECS1
151800940131     C* gestione opzioni
151900941118   B3C     S1OPZ         IFNE      0
152000940201     C     *IN99         ANDEQ     *OFF
152100940131     C                   EXSR      OPZS1
152200941118   E3C                   ENDIF
152300940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
152400941118   B3C     *IN99         IFEQ      *ON
152500940131     C                   MOVE      *ON           *IN32
152600941118   E3C                   ENDIF
152700950301     C*
152800950301     C* imposto quegli attributi del sfl che sono modificabili
152900950301     C* al richiamo del determinato pgm
153000941118     C                   EXSR      ATRS1
153100941118     C*
153200940131     C                   Z-ADD     0             S1OPZ
153300940223     C*
153400940127     C                   UPDATE    FMTS1
153500941125     C                   MOVE      *OFF          *IN32
153600940223     C*
153700940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
153800941118   B3C     $ESCI         IFEQ      *OFF
153900940131     C                   READC     FMTS1                                  21
154000940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
154100941118   B4C     *IN21         IFEQ      *ON
154200940201     C                   MOVE      WINZS1        $INZS1
154300940204     C* calcolo pagina a cui deve posizionarsi
154400940728     C     WSFL          IFGT      0
154500940728     C                   EXSR      CLCPAG
154600940728     C                   ELSE
154700940728     C                   Z-ADD     SFLPAG        WPAG
154800940728     C                   ENDIF
154900941118   E4C                   ENDIF
155000941118   E4C                   ENDIF
155100940131     C*
155200941118   E2C                   ENDDO
155300940127     C*
155400941118   E1C                   ENDIF
155500940131     C*
155600940127     C                   ENDSR
155700940127     C/EJECT
155800940127     C************************************************************
155900940131     C* CONTROLLO CAMPI I/O RIGA LISTA
156000940127     C************************************************************
156100940131     C     RECS1         BEGSR
156200940131     C*
156300940201     C* reset indicatori DSPATR
156400940201     C                   MOVE      *ALL'0'       IN4049           10
156500940201     C                   MOVEA     IN4049        *IN(40)
156600940201     C*
156700940131     C* Controllo se Utente Autorizzato alla scelta
156800940202     C     S1OPZ         IFNE      0
156900940131     C                   Z-ADD     1             X
157000940223     C     S1OPZ         LOOKUP    $OP1(X)                                21
157100980720     C* mi salvo l'indice dell' opzione
157200980720     C   21              Z-add     X             PosOpz            2 0
157300940211     C     *IN21         IFEQ      *OFF
157400940223     C     $FL1(X)       OREQ      'N'
157500940131     C                   MOVE      *ON           $ESCI
157600960109     C                   SETON                                        409199
157700940131     C                   ENDIF
157800001122d0771 * Controllo con autorizzazioni CONPNO
157900001122d0771C                   Exsr      CtrAutPno
158000940202     C                   ENDIF
158100001122d0771x*
158200001122  "  x* Controllo se Utente Autorizzato alla cancellazione
158300001122  "  x***  S1OPZ         IFEQ      4
158400001122  "  x***  ANNPNB        ANDEQ     *off
158500001122  "  x***                SETON                                        409499
158600001122d0771x***                ENDIF
158700960119     C*
158800960119     C* Controllo se richiesta contabilità, che la causale esista
158900960119     C     S1OPZ         IFeq      41
159000960119     c     s1caustes     andeq     *Blanks
159100960119     C                   SETON                                        404199
159200960119     c                   endif
159300940131     C*
159400940131     C                   ENDSR
159500941118     C/EJECT
159600001122d0771C************************************************************
159700001122  "  C* Controllo autorizzazione prima nota batch manuale
159800001122  "  C************************************************************
159900001122  "  C     CtrAutPno     BEGSR
160000001122  "   *
160100001122  "  C                   Eval      $OpzOk     =  *On
160200001122  "   * Cancellazione
160300001122  "  C                   SELECT
160400001122  "  C     $TASTO        WHENEQ    F13
160500001122  "  C*
160600001122  "  C     WOpz          IFeq      4
160700001122  "  C     AnnPnb        Andeq     *off
160800001122  "  C                   MOVE      *ON           $ESCI                          sospese READC
160900001122  "  C                   Eval      $OpzOk     =  *Off
161000001122  "   * ...(F1) L'utente non è autorizzato alla funzione nel modo desiderato
161100001122  "  C                   SETON                                        409499
161200001122  "  C                   EndIf
161300001122  "  C                   OTHER
161400001122  "  C*
161500001122  "  C     S1OPZ         IFeq      4
161600001122  "  C     AnnPnb        Andeq     *off
161700001122  "  C                   MOVE      *ON           $ESCI                          sospese READC
161800001122  "  C                   Eval      $OpzOk     =  *Off
161900001122  "   * ...(F1) L'utente non è autorizzato alla funzione nel modo desiderato
162000001122  "  C                   SETON                                        409499
162100001122  "  C                   EndIf
162200001122  "   *
162300001122  "  C                   ENDSL
162400001122  "   *
162500001122d0771C                   ENDSR
162600941118     C************************************************************
162700941118     C* GESTIONE OPZIONI LISTA
162800941118     C************************************************************
162900941118     C     OPZS1         BEGSR
163000950301     C*
163100950301     C                   Z-ADD     RIRI          CURR
163200950301     C                   Z-ADD     COCO          CURC
163300941118     C*
163400941027     C                   SELECT
163500960215     c*
163600960215     C* se opz=scelta ripassa il numero del lotto
163700960215     C     S1OPZ         WHENEQ    1
163800960215     C                   MOVE      S1OPZ         OPZ077
163900960215     C* passare nei campi appositi della DS C001DS il valore
164000960215     C* della chiave della VL univoca (se c'è)
164100960215     C                   MOVE      H1RifInt      rifint077
164200960215     C                   MOVE      *ON           $ESCI
164300960215     C                   MOVE      *ON           $FINE
164400960215     C*
164500960215     C                   MOVE      *HIVAL        $LASTV
164600960119     c*
164700960119     C* se opz=cancellazione
164800960119     C* emetto xmsg per avvertire, se confermano richiamo pgm di cancella
164900960119     c* zione
165000960119     C     S1OPZ         WHENEQ    4
165100001127d0771C     $TASTOSAV     ANDNE     F13
165200001127d0771C                   Reset                   XMsgDs
165300960119     C* reperisco testi messaggi
165400960119     C                   EXSR      repmsg
165500960119     C*
165600001127d0771x***                Z-ADD     0             MSGMSG
165700001127  "  x***                MOVEL     'S'           MSGRSP
165800001127  "  x***                MOVEL     'SN'          MSGTPR
165900001127  "  x***                MOVEL     'S'           MSGEMV
166000001127  "  x***                Z-ADD     10            MSGRGP
166100001127  "  x***                MOVEL     'N'           MSGLCK
166200001127  "  x***                MOVEL     'N'           MSGCNL
166300001127  "  x***                MOVEL     'N'           MSGVID
166400001127  "  x***                MOVEL     'N'           MSGOPE
166500001127d0771X***                MOVEL     'N'           MSGSTP
166600960119     C                   CALL      'XMSGR'
166700960119     C                   PARM                    XMSGDS
166800960119     C                   PARM                    XMStxt          135
166900960119     C                   PARM      wxmsginz      XMSGinz           1
167000960119     c*
167100960119     C                   EXSR      RTNXMSG
167200960119     c                   move      *off          wxmsginz
167300001127d0771C* se opz=cancellazione +F13=selezione multipla,
167400001127  "  C* emetto 1 sola window dirichiesta conferma cancellazione        la
167500001127  "  C     S1OPZ         WHENEQ    4
167600001127  "  C     $TASTOSAV     ANDEQ     F13
167700001127  "  C*
167800001127  "  C                   If        PrimaVolta=0
167900001127  "  C                   reset                   xmsgds
168000001127  "  C                   eval      xmsgcd1 = 'PRO1473'
168100001127  "  C*
168200001127  "  C                   CALL      'XMSGR'
168300001127  "  C                   PARM                    XMSGDS
168400001127  "  C                   PARM                    XMStxt          135
168500001127  "  C                   PARM      wxmsginz      XMSGinz           1
168600001127  "  C                   PARM                    XMSGCd1           7
168700001127  "  C                   PARM                    XMSGCd2           7
168800001127  "  C                   PARM                    XMSGCd3           7
168900001127  "  C                   PARM                    XMSGSn1           7
169000040308D0771C                   PARM                    XMSGCdTxT        63
169100010619A0842C                   PARM                    MSGCdTit         14
169200010619A0842C                   PARM                    MSGClPrtf         1
169300010619B0395C                   PARM                    VarMsg1         100
169400010619  "  C                   PARM                    VarMsg2         100
169500010619B0395C                   PARM                    VarMsg3         100
169600040308D0771C                   Eval      PrimaVolta=1
169700001127  "  C                   EndIf
169800001127  "  C*ciclo di cancellazione
169900001127  "  C                   EXSR      RtnXMSg
170000001127d0771C                   move      *off          wxmsginz
170100001127     C*
170200960118     c*
170300960118     C* se opz=dettaglio
170400960118     C* richiamo il pgm di visualizzazione dettaglio
170500960118     C     S1OPZ         WHENEQ    15
170600960118     C                   RESET                   NDC078DS
170700960118     c*
170800960118     C                   MOVE      '05'          OPZ078
170900960118     c                   move      xscsoc        societa078
171000960118     C                   move      h1rifint      rifint078
171100960118     C                   move      h1subnum      subnum078
171200960118     c                   movel     ndc078ds      KPJBU
171300960118     C                   CALL      'NDC078R'
171400960118     C                   PARM                    KPJBA
171500960118     c                   movel     kpjbu         NDC078DS
171600960118     C*
171700960118     C* ritorno da PGM movimenti
171800960118     C                   EXSR      RTN078
171900960118     c*
172000960118     C* se opz=contabilità
172100960118     C* richiamo il pgm di primanota
172200960118     C     S1OPZ         WHENEQ    41
172300050228D1740c* prima controllo chiamata ricorsiva (potrebbe succedere da
172400050228  "  c* primanota veloce con creazione automatica rateo)
172500050228  "  c                   reset                   x48chPgmds
172600050228  "  C                   eval      x48pgm = 'NDC071R'
172700050228  "  c                   eval      kpjbu = x48chpgmds
172800050228  "  c                   call      'X48CHPGM'
172900050228  "  c                   parm                    kpjba
173000050228  "  c                   movel     kpjbu         x48chpgmds
173100050228D1740c                   if        x48errore = *off
173200960119     C                   RESET                   NDC071DS
173300960119     c                   eval      societa071 = xscsoc
173400960119     c                   eval      rifint071  = h1rifint
173500960119     C                   CALLb     'XDT4000'
173600960119     C     dtregda071    PARM                    XDTAMG
173700960119     C                   PARM      s1dtreg       XDTGMA
173800960119     C                   PARM      4             XDTSTA
173900960119     c                   eval      dtrega071  = dtregda071
174000960119     c                   eval      subnumd071 = h1subnum
174100960119     c                   eval      subnuma071 = h1subnum
174200960119     c                   eval      interat071 = *on
174300960119     c                   movel     ndc071ds      KPJBU
174400960214     C* non chiamo il cl perchè non mi serve il file msg perchè il pgm
174500960214     c* è in interattivo
174600960214     C                   CALL      'NDC071R'
174700960119     C                   PARM                    KPJBA
174800960119     c                   movel     kpjbu         NDC071DS
174900960119     C*
175000960119     C* ritorno da PGM movimenti
175100960119     C                   EXSR      RTN071
175200050228d1740c                   endif
175300980720     C*
175400980720     C* se selezionata opzione personalizzata, richiamo il programma
175500980720     C* corrispondente
175600980720     C     $PGO1(PosOpz) Whenne    *Blank
175700980720     C                   RESET                   ndc078ds
175800980720     C* << passare nei campi appositi della DS ndc0789DS il valore
175900980720     C*    della chiave della VL univoca (se c'è) >>
176000980720     C                   MOVEL     S1OPZ         OPZ078
176100980720     c*
176200980720     c                   move      xscsoc        societa078
176300980720     C                   move      h1rifint      rifint078
176400980720     C                   move      h1subnum      subnum078
176500980720     c                   movel     ndc078ds      KPJBU
176600980720     C*
176700980720     C                   CALL      $PGO1(PosOpz)                        22
176800980720     C                   PARM                    KPJBA
176900980720     C                   PARM                    NOMPGM
177000980720     C*
177100980720     C                   MOVEL     KPJBU         ndc078ds
177200980720     C*
177300980720     C* ritorno da PGM
177400980720     C                   EXSR      RTN078
177500980720     C*
177600950620     C* se altre opzioni
177700950620     C* richiamo pgm gestione anagrafica
177800941027     C                   OTHER
177900940131     C*
178000940506     C                   MOVE      *HIVAL        $LASTV
178100941027     C                   ENDSL
178200940225     C*
178300940131     C                   ENDSR
178400960118     C/EJECT
178500960118     C************************************************************
178600960118     C* GESTIONE RITORNO DA PGM DETTAGLIO
178700960118     C************************************************************
178800960118    >C     RTN078        BEGSR
178900960118     C*
179000960118     C* modo di ritorno
179100960118     C*
179200960118     C                   SELECT
179300960118     C* questi modi di utilizzo dei valori di ritorno dal
179400960118     C* pgm di manutenzione rcd di anagrafica sono delle
179500960118     C* proposte, normalmente sempre valide, ma modificabili
179600960118     C* per situazioni particolari
179700960118     C* 1 = F3
179800960118    >C     RET078        WHENEQ    '1'
179900960118     C                   MOVE      *ON           $ESCI
180000960118     C                   MOVE      *ON           $FINE
180100960118     C                   MOVE      '1'           RET077
180200960118     C* 2 = F12
180300960118    >C     RET078        WHENEQ    '2'
180400960118     C                   MOVE      *ON           $ESCI
180500960118     C*
180600960118     C                   ENDSL
180700960118     C*
180800960118     C* funzione non eseguibile per errore :
180900960118     C*
181000960118     C                   SELECT
181100960118     C* 1 = funzione richiamata chiusa in errore
181200960118    >C     ERR078        WHENEQ    '1'
181300960118     C                   MOVE      *ON           $ESCI
181400960118     C                   SETON                                        9599
181500960118     C* 2 = funzione richiamata senza la dovuta autorizzazione
181600960118    >C     ERR078        WHENEQ    '2'
181700960118     C                   MOVE      *ON           $ESCI
181800960118     C                   SETON                                        9499
181900960118     C*
182000960118     C                   ENDSL
182100960118     C*
182200960118     C                   ENDSR
182300960119     C/EJECT
182400960119     C************************************************************
182500960119     C* GESTIONE RITORNO DA PGM primanota generica
182600960119     C************************************************************
182700960119     C     RTN071        BEGSR
182800960119     C*
182900960214     C* ricarico subfile perchè se contabilizzata, è stata cancel
183000960214     c* lata da ndbhm, lo ricarico sempre perchè anche se è andata bene
183100960214     c* ci può essere il flag di errore impostato ad *on per le
183200960214     c* registrazioni collegate
183300960119     c                   move      *on           winzs1
183400960119     C*
183500001006D0721c* se ret071='0' e err071='0' significa che è stato dato F6
183600001006D0721c* di conferma da una registrazione di prima nota. In questo caso
183700001006D0721c* imposto ad *on opr077, anche se ovviamente ha senso solo se
183800001006D0721c* si esegue una sola registrazione, in quanto se ne potrebbero
183900001006D0721c* avere più di una con uno stesso rif.interno e in questo caso
184000001006D0721c* OPR077 si riferisce solo all'ultima registrazione eseguita
184100001006D0721C                   IF        Ret071= '0'  and Err071='0'
184200001006D0721C                   eval      Wopr = *on
184300001006d0721c                   endif
184400001006     c*
184500960119     C                   ENDSR
184600940223     C/EJECT
184700940223     C************************************************************
184800960118     C* GESTIONE RITORNO DA PGM XMSG
184900940223     C************************************************************
185000960118    >C     RTNXMSG       BEGSR
185100940223     C*
185200940223     C* modo di ritorno
185300960119     C*
185400960119     C* S = conferma cancellazione
185500960119     c* 1) chiamo pgm stampa passando *off nel campo errori072, e in questo
185600960119     c*    caso la stampa cancella anche
185700960119     c* 2) ricarico subfile
185800960119     C* N = no conferma: non faccio nulla
185900960119     C                   if        msgrtn = 'S'
186000960119     C* 1) stampa e cancellazione registrazione
186100960119     C                   reset                   NDC072DS
186200960119     C                   eval      societa072= xscsoc
186300960119     C                   eval      rifint072 = h1rifint
186400960119     C                   eval      subnum072 = h1subnum
186500960119     C                   eval      dociva072 = *on
186600960119     C                   eval      errori072 = *off
186700960119     C                   eval      cancel072 = *on
186800960119     C*
186900960119     C                   movel     NDC072DS      KPJBU
187000960119     C                   CALL      'NDC072R'
187100960119     C                   PARM                    KPJBA
187200021016C1579C*
187300021016  "  C* annullo raccordo movimento/registrazione per c/c di pool
187400021016  "  C                   ExSr      AnnNDCPR
187500021016  "  C                   if        §Errore   <>  '0'  and
187600021016  "  C                             §Errore   <>  '1'
187700021016  "  C                   SetOn                                        939940
187800021016  "  C                   endif
187900021016  "  C*
188000021016  "  C*
188100021016  "  C* se ci sono errori
188200021016  "  C                   if        *In99      =  *On
188300021016  "  C                   ROLBK
188400021016C1579C                   else
188500960119     C* Eseguo COMMIT
188600960119     C                   COMMIT
188700960119      *
188800960119     c* 2) ricarico subfile
188900001127d0771C*    Se  ho chiesto la selezione multipla
189000001127  "  C*    ricarico una sola volta alla fine.
189100001127d0771C                   If        $TastoSav<>F13 OR
189200001127d0771C                             ($TastoSav=F13 and  WMax>=S1Nrr)
189300960119     c                   move      *on           winzs1
189400001127d0771C                   EndIf
189500021016C1579C*
189600021016  "  C                   endif
189700021016C1579C*
189800021016D0771C                   Else
189900001127  "  C                   If        $TastoSav=F13
190000001127  "  c                   move      *on           winzs1
190100001127d0771C                   EndIf
190200001127     c                   endif
190300940223     C*
190400940223     C                   ENDSR
190500960115     C/EJECT
190600021016C1579 *----------------------------------------------------------
190700021016  "   * Annulla raccordo registrazioni giroconto per c/c pool
190800021016  "   *----------------------------------------------------------
190900021016C1579C     AnnNDCPR      BEGSR
191000021016     C*
191100021016     C     §Errore       DOUNE     '2'
191200021016     C     MSGRTN        ORNE      'R'
191300021016     C*
191400021016     C                   Eval      §Lunghezza   = %Size(NDCPRDS)
191500021016     C                   CALLB     'PJC00701R'
191600021016     C                   parm      '02'          §Opzione                       1
191700021016     C                   parm      XScSoc        §Societa                       7
191800021016     C                   parm      0             §SysReg                        7
191900021016     C                   parm      0             §NrAsReg                       7
192000021016     C                   parm      0             §NrRigaM                       7
192100021016     C                   parm      H1RifInt      §RifInt                        7
192200021016     C                   parm      H1SubNum      §SubNum                        7
192300021016     C                   parm      *Off          §Errore                        2
192400021016     C                   parm                    §Dati                          3
192500021016     C                   parm                    §Lunghezza                     4
192600021016     C                   parm      '1'           §Commit                        6
192700021016     C*
192800021016      *
192900021016      * gestione RCD allocato
193000021016     C     §Errore       IFEQ      '2'
193100021016     C                   MOVEL     'NDCPR00F'    MSGFIL
193200021016     C                   EXSR      MSGPGM
193300021016     C                   ENDIF
193400021016     C                   ENDDO
193500021016      *
193600021016C1579C                   ENDSR
193700021016C1579C******************************************************
193800021016  "  C* PGM PER GESTIONE MESSAGGI ERRORE                   *
193900021016  "  C******************************************************
194000021016C1579C     MSGPGM        BEGSR
194100021016     C*
194200021016     C                   Clear                   XMSGDS
194300021016     C                   Z-ADD     2             MSGMSG
194400021016     C                   MOVEL     'S'           MSGRSP
194500021016     C                   MOVEL     'CR'          MSGTPR
194600021016     C                   MOVEL     'S'           MSGEMV
194700021016     C                   Z-ADD     10            MSGRGP
194800021016     C                   MOVEL     'S'           MSGLCK
194900021016     C                   MOVEA     STATUS        STS
195000021016     C                   MOVEL     'N'           MSGCNL
195100021016     C                   MOVEL     'N'           MSGVID
195200021016     C                   MOVEL     'N'           MSGOPE
195300021016     C                   MOVEL     'N'           MSGSTP
195400021016     C                   CALL      'XMSGR'
195500021016     C                   PARM                    XMSGDS
195600021016     C                   PARM                    XMStxt          135
195700021016     C                   PARM      wxmsginz      XMSGinz           1
195800021016     C                   PARM      *Blank        XMSGCd1           7
195900021016     C                   PARM      *Blank        XMSGCd2           7
196000021016     C                   PARM      *Blank        XMSGCd3           7
196100021016     C                   PARM                    XMSGSn1           7
196200021016     C                   PARM                    XMSGCdTxT        63
196300021016     C                   PARM                    MSGCdTit         14
196400021016     C                   PARM                    MSGClPrtf         1
196500021016     C                   PARM                    VarMsg1         100
196600021016     C                   PARM                    VarMsg2         100
196700021016     C                   PARM                    VarMsg3         100
196800021016     C                   move      *off          wxmsginz
196900021016     C*
197000021016C1579C                   ENDSR
197100950301     C************************************************************
197200950301     C* AGGIORNAMENTO RIGA LISTA
197300950301     C************************************************************
197400950301     C     AGGS1         BEGSR
197500950301     C*
197600950301     C* riempio i campi del sfl che sono stati ripassati dal pgm
197700950301     C* di manutenzione dopo una opzione 02
197800950301     C*
197900950301     C                   ENDSR
198000940223     C/EJECT
198100940131     C************************************************************
198200940131     C* GESTIONE VALORI ASSUNTI
198300940131     C************************************************************
198400940131     C     GESD2         BEGSR
198500940131     C*
198600941118   B1C     $INZD2        IFEQ      *ON
198700940131     C                   EXSR      INZD2
198800940223     C                   MOVE      *OFF          $INZD2
198900941118   E1C                   ENDIF
199000940202     C*
199100941118   B1C     $LASTV        IFNE      'D2'
199200940923     C                   EXSR      GESPIE
199300940202     C                   MOVE      $LASTV        $LASTG
199400940202     C                   MOVE      'D2'          $LASTV
199500941118   E1C                   ENDIF
199600940131     C*
199700940707     C* se esistono errori sulla videata
199800940707     C* emetto la write del formato a indicatori spenti per vedere
199900940707     C* le eventuali decodifiche
200000941118   B1C     *IN99         IFEQ      *ON
200100940707     C                   MOVEA     *IN           WIN
200200940707     C                   MOVE      *ALL'0'       IN5098           49
200300940707     C                   MOVEA     IN5098        *IN(50)
200400940707     C                   WRITE     FMTD2
200500940707     C                   MOVEA     WIN           *IN
200600941118   E1C                   ENDIF
200700940707     C*
200800940131     C                   EXFMT     FMTD2
200900941205     C*
201000951005     C* CONTROLLO ABILITAZIONE TASTI
201100951005     C* INPUT
201200951005     C* $FC3 = Abilitazioni videata D2
201300951005     C* OUTPUT
201400951005     C* *IN97 + *IN99 Errori
201500951005     C                   MOVEA     $FC3          $FC
201600951005     C                   EXSR      CTRKEY
201700941205     C*
201800941205   B1C     *IN99         IFEQ      *OFF
201900941205     C*
202000941205   B2C                   SELECT
202100940131     C* F3=Fine
202200940506     C     $TASTO        WHENEQ    F03
202300940309     C                   EXSR      F03D2
202400940131     C* F12=Ritorno
202500940506     C     $TASTO        WHENEQ    F12
202600940309     C                   EXSR      F12D2
202700940916     C* F24=Altri tasti
202800940916     C     $TASTO        WHENEQ    F24
202900940916     C                   EXSR      F24D2
203000940131     C*
203100940131     C                   OTHER
203200940131     C                   EXSR      CTRD2
203300941205   B3C     *IN99         IFEQ      *OFF
203400940211     C                   MOVE      $LASTG        $GEST
203500941205   B4C                   SELECT
203600940202     C     $LASTG        WHENEQ    'S1'
203700940131     C* se è stato modificato l'ordinamento di default, ricarico la lista
203800941205   B5C     $ORD          IFNE      D2ORD
203900940223     C                   MOVE      *ON           $INZS1
204000960110     C                   clear                   W1POSd
204100941205   E5C                   ENDIF
204200940202     C     $LASTG        WHENEQ    'D1'
204300940318     C* per ora niente
204400941205   E4C                   ENDSL
204500940131     C* definisco i nuovi valori di default
204600940201     C                   Z-ADD     D2LOP         $LOP
204700940131     C                   Z-ADD     D2ORD         $ORD
204800950322     C*
204900950322     C* aggiorno valori assunti
205000950328     C* se richiesto il salvataggio
205100950328   B4C     D2BSV         IFEQ      1
205200950322     C                   EXSR      AGGMPE
205300950322   E4C                   ENDIF
205400950322     C*
205500941205   E3C                   ENDIF
205600940131     C*
205700941205   E2C                   ENDSL
205800940131     C*
205900941205   E1C                   ENDIF
206000941205     C*
206100940131     C                   ENDSR
206200940223     C/EJECT
206300940131     C************************************************************
206400940211     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
206500940131     C************************************************************
206600940201     C     INZD2         BEGSR
206700940131     C*
206800940131     C                   CLEAR                   FMTD2
206900960110     C                   MOVEL     $LOP          D2LOP
207000960110     C                   MOVE      $ORD          D2ORD
207100940131     C*
207200940131     C                   ENDSR
207300940309     C/EJECT
207400940309     C************************************************************
207500940309     C* GESTIONE F03 VIDEO D2
207600940309     C************************************************************
207700940309     C     F03D2         BEGSR
207800940309     C*
207900940309     C                   MOVE      *ON           $FINE
208000940325     C* fine programma
208100940325     C                   EXSR      ENDPGM
208200940309     C*
208300940309     C                   ENDSR
208400940309     C/EJECT
208500940309     C************************************************************
208600940309     C* GESTIONE F12 VIDEO D2
208700940309     C************************************************************
208800940309     C     F12D2         BEGSR
208900940309     C*
209000940309     C                   MOVE      $LASTG        $GEST
209100940309     C*
209200940309     C                   ENDSR
209300940916     C/EJECT
209400940916     C************************************************************
209500940916     C* GESTIONE F24 VIDEO D2
209600940916     C************************************************************
209700940916     C     F24D2         BEGSR
209800940916     C*
209900940916     C                   MOVEA     $FX3          $FX
210000940916     C                   MOVEA     $FC3          $FC
210100940916     C                   MOVEA     $FR3          $FR
210200940916     C                   Z-ADD     $ULKD2        $ULK
210300950726     C                   Z-ADD     $D2L01        $L01
210400950726     C                   Z-ADD     $D2L02        $L02
210500940916     C                   EXSR      RIEKEY
210600940916     C                   Z-ADD     $ULK          $ULKD2            3 0
210700940916     C                   MOVEA     $FC           $FC3
210800940916     C                   MOVEL     $KEY1         Z3KE1
000000060224C1913C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
210900940916     C                   WRITE     FMTZ3
211000940916     C*
211100940916     C                   ENDSR
211200940223     C/EJECT
211300940131     C************************************************************
211400940131     C* CONTROLLO VIDEATA VALORI ASSUNTI
211500940131     C************************************************************
211600940131     C     CTRD2         BEGSR
211700940131     C*
211800940131     C                   SETOFF                                       99
211900940131     C* non imposto pos. del cursore
212000940131     C                   Z-ADD     0             H1RIGA
212100940131     C                   Z-ADD     0             H1COLO
212200940131     C*
212300940131     C* OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
212400940201     C     D2LOP         IFEQ      0
212500940131     C                   SETON                                          5099
212600941027     C                   ENDIF
212700940131     C*
212800940131     C* OBBLIGATORIO VALORE PER ORDINAMENTO
212900940131     C     D2ORD         IFEQ      0
213000960109     C                   SETON                                          5199
213100941027     C                   ENDIF
213200940131     C*
213300940131     C                   ENDSR
213400941205     C/EJECT
213500941205     C************************************************************
213600941205     C* CONTROLLO ABILITAZIONE TASTI
213700941205     C* INPUT
213800941205     C* $TASTO =Tasto funzione premuto
213900941205     C* $FC = Tasti abilitati
214000941205     C* OUTPUT
214100941205     C* IN99 = ERrore
214200941205     C************************************************************
214300941205     C     CTRKEY        BEGSR
214400941205     C*
214500951005     C                   SETOFF                                       99
214600951005     C*
214700941205     C                   SELECT
214800941205     C     $TASTO        WHENEQ    F01
214900980720     C                   z-add     01            posfun
215000941205     C     $FC(1)        IFEQ      '0'
215100951005     C                   SETON                                        9997
215200941205     C                   ENDIF
215300941205     C     $TASTO        WHENEQ    F02
215400980720     C                   z-add     02            posfun
215500941205     C     $FC(2)        IFEQ      '0'
215600951005     C                   SETON                                        9997
215700941205     C                   ENDIF
215800941205     C     $TASTO        WHENEQ    F03
215900980720     C                   z-add     03            posfun
216000941205     C     $FC(3)        IFEQ      '0'
216100951005     C                   SETON                                        9997
216200941205     C                   ENDIF
216300941205     C     $TASTO        WHENEQ    F04
216400980720     C                   z-add     04            posfun
216500941205     C     $FC(4)        IFEQ      '0'
216600951005     C                   SETON                                        9997
216700941205     C                   ENDIF
216800941205     C     $TASTO        WHENEQ    F05
216900980720     C                   z-add     05            posfun
217000941205     C     $FC(5)        IFEQ      '0'
217100951005     C                   SETON                                        9997
217200941205     C                   ENDIF
217300941205     C     $TASTO        WHENEQ    F06
217400980720     C                   z-add     06            posfun
217500941205     C     $FC(6)        IFEQ      '0'
217600951005     C                   SETON                                        9997
217700941205     C                   ENDIF
217800941205     C     $TASTO        WHENEQ    F07
217900980720     C                   z-add     07            posfun
218000941205     C     $FC(7)        IFEQ      '0'
218100951005     C                   SETON                                        9997
218200941205     C                   ENDIF
218300941205     C     $TASTO        WHENEQ    F08
218400980720     C                   z-add     08            posfun
218500941205     C     $FC(8)        IFEQ      '0'
218600951005     C                   SETON                                        9997
218700941205     C                   ENDIF
218800941205     C     $TASTO        WHENEQ    F09
218900980720     C                   z-add     09            posfun
219000941205     C     $FC(09)       IFEQ      '0'
219100951005     C                   SETON                                        9997
219200941205     C                   ENDIF
219300941205     C     $TASTO        WHENEQ    F10
219400980720     C                   z-add     10            posfun
219500941205     C     $FC(10)       IFEQ      '0'
219600951005     C                   SETON                                        9997
219700941205     C                   ENDIF
219800941205     C     $TASTO        WHENEQ    F11
219900980720     C                   z-add     11            posfun
220000941205     C     $FC(11)       IFEQ      '0'
220100951005     C                   SETON                                        9997
220200941205     C                   ENDIF
220300941205     C     $TASTO        WHENEQ    F12
220400980720     C                   z-add     12            posfun
220500941205     C     $FC(12)       IFEQ      '0'
220600951005     C                   SETON                                        9997
220700941205     C                   ENDIF
220800941205     C     $TASTO        WHENEQ    F13
220900980720     C                   z-add     13            posfun
221000941205     C     $FC(13)       IFEQ      '0'
221100951005     C                   SETON                                        9997
221200941205     C                   ENDIF
221300941205     C     $TASTO        WHENEQ    F14
221400980720     C                   z-add     14            posfun
221500941205     C     $FC(14)       IFEQ      '0'
221600951005     C                   SETON                                        9997
221700941205     C                   ENDIF
221800941205     C     $TASTO        WHENEQ    F15
221900980720     C                   z-add     15            posfun
222000941205     C     $FC(15)       IFEQ      '0'
222100951005     C                   SETON                                        9997
222200941205     C                   ENDIF
222300941205     C     $TASTO        WHENEQ    F16
222400980720     C                   z-add     16            posfun
222500941205     C     $FC(16)       IFEQ      '0'
222600951005     C                   SETON                                        9997
222700941205     C                   ENDIF
222800941205     C     $TASTO        WHENEQ    F17
222900980720     C                   z-add     17            posfun
223000941205     C     $FC(17)       IFEQ      '0'
223100951005     C                   SETON                                        9997
223200941205     C                   ENDIF
223300941205     C     $TASTO        WHENEQ    F18
223400980720     C                   z-add     18            posfun
223500941205     C     $FC(18)       IFEQ      '0'
223600951005     C                   SETON                                        9997
223700941205     C                   ENDIF
223800941205     C     $TASTO        WHENEQ    F19
223900980720     C                   z-add     19            posfun
224000941205     C     $FC(19)       IFEQ      '0'
224100951005     C                   SETON                                        9997
224200941205     C                   ENDIF
224300941205     C     $TASTO        WHENEQ    F20
224400980720     C                   z-add     20            posfun
224500941205     C     $FC(20)       IFEQ      '0'
224600951005     C                   SETON                                        9997
224700941205     C                   ENDIF
224800941205     C     $TASTO        WHENEQ    F21
224900980720     C                   z-add     21            posfun
225000941205     C     $FC(21)       IFEQ      '0'
225100951005     C                   SETON                                        9997
225200941205     C                   ENDIF
225300941205     C     $TASTO        WHENEQ    F22
225400980720     C                   z-add     22            posfun
225500941205     C     $FC(22)       IFEQ      '0'
225600951005     C                   SETON                                        9997
225700941205     C                   ENDIF
225800941205     C     $TASTO        WHENEQ    F23
225900980720     C                   z-add     23            posfun
226000941205     C     $FC(23)       IFEQ      '0'
226100951005     C                   SETON                                        9997
226200941205     C                   ENDIF
226300941205     C     $TASTO        WHENEQ    F24
226400980720     C                   z-add     24            posfun
226500941205     C     $FC(24)       IFEQ      '0'
226600951005     C                   SETON                                        9997
226700941205     C                   ENDIF
226800941205     C                   ENDSL
226900941205     C*
227000941205     C                   ENDSR
227100940223     C/EJECT
227200940201     C************************************************************
227300940201     C* AGGIORNAMENTO VALORI ASSUNTI
227400940201     C************************************************************
227500940201     C     AGGMPE        BEGSR
227600940201     C*
227700950524     C* preparo i parametri di chiamata al driver
227800950524     C                   CLEAR                   XMPEDS
227900950524     C                   EVAL      TpChiamata = '03'
228000950524     C                   EVAL      NChiavi = 5
228100950524     C                   EVAL      VLogica = 2
228200950524     C*
228300950524     C* preparo le chiavi per la chiamata al driver
228400950524     C                   CLEAR                   XMPEDS1
228500950524     C                   MOVEL     XSCSOC        DSMPESOC
228600950524     C                   MOVEL     DSPGM         DSMPEPGM
228700950524     C                   MOVEL     KNMUS         DSMPEPRF
228800941013     C*
228900950322     C* Si memorizzano i dati di default a seconda delle modalità con
229000950322     C* cui il pgm è usato
229100950322     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
229200950322     C* MPETIP = 'PV'  pgm richiamato da menù
229300950322     C* MPETIP = 'PB'  x lancio Batch
229400950322     C*
229500941013     C                   SELECT
229600960118    >C     OPZ077        WHENEQ    '01'
229700950524     C                   MOVEL     'PX'          DSMPETIP
229800941013     C                   OTHER
229900950524     C                   MOVEL     'PV'          DSMPETIP
230000941013     C                   ENDSL
230100950524     C* dati
230200950524     C                   MOVEL     DSMPE         DSMPEPAR
230300950524     C*
230400950524     C                   CALLB     'XMPE'
230500950524     C                   PARM                    XMPEDS
230600950524     C                   PARM                    XMPEDS1
230700940201     C*
230800940201     C                   ENDSR
230900940223     C/EJECT
231000940131     C************************************************************
231100940131     C* OPERAZIONI INIZIALI
231200940131     C************************************************************
231300940131     C     *INZSR        BEGSR
231400000118C1309C     *DMY          Move      Udate         ZDateISO
231500000118C1309C     *JobRun       Move      ZDateISO      ZDate             6 0
231600940131     C*
231700940127     C* Reperimento parametri
231800940127     C*
231900940117     C     *ENTRY        PLIST
232000940117     C                   PARM                    KPJBA
232100950410     C                   PARM                    DSAUT
232200950410     C*
232300951010     C* testo se sono stati passati i dati di autorizzazione
232400951010     C*
232500950410     C     PARMS         IFLT      2
232600950410     C                   MOVE      *BLANK        AUTDS
232700950410     C                   ELSE
232800950410     C                   MOVEL     DSAUT         AUTDS
232900950410     C                   ENDIF
233000951206     C*
233100951206     C* Reperimento dati società
233200951206     C*
233300951206    >C                   MOVEL     'SOC001'      TIPXSC
233400951206     C                   MOVEL     *BLANK        SOCXSC
233500951206     C                   EXSR      REPSOC
233600951206     C     RTNXSC        IFNE      '1'
233700951206    >C                   MOVEL     XSOCDS        SOC001
233800951206     C                   ELSE
233900951206     C                   MOVE      *ON           $FINE
234000951206     C                   EXSR      ENDPGM
234100951206     C                   ENDIF
234200960109     c* Data odierna
234300960109     C                   CALLb     'XDT4000'
234400960109     C     WDATE         PARM                    XDTAMG
234500000118C1309C*****              PARM      UDATE         XDTGMA
234600000118C1309C                   PARM      ZDate         XDTGMA
234700960109     C                   PARM      4             XDTSTA
234800950726     C*
234900950726     C* reperisco le dimensioni delle variabili che conterranno i tasti
235000950726     C                   EVAL      $D1L01 = %Size(Z1Ke1)
235100950726     C                   EVAL      $D1L02 = %Size(Z1Ke2)
235200950726     C                   EVAL      $S1L01 = %Size(Z2Ke1)
235300950726     C                   EVAL      $S1L02 = %Size(Z2Ke2)
235400950926     C                   EVAL      $C1Op1 = %Size(C1Opz1)
235500950926     C                   EVAL      $C1Op2 = %Size(C1Opz2)
235600950906     C                   EVAL      $D2L01 = %Size(Z3Ke1)
235700950906     C                   EVAL      $D2L02 = 0
235800940225     C*
235900940223     C* Reperimento opzioni
236000940127     C*
236100940128     C                   EXSR      REPOPZ
236200950406     C*
236300940506     C* Reperimento tasti di funzione
236400940506     C*
236500940506     C                   EXSR      REPKEY
236600980722     C*
236700980722     C* Chiamata a routine per il reperimento delle opzioni e funzioni
236800980722     C* personalizzate per S1
236900980722     C*
237000980722     C                   CAllb     'X46USREXT'
237100980722     C                   PARM                    $OP1
237200980722     C                   PARM                    $TX1
237300980722     C                   PARM                    $FL1
237400980722     C                   PARM                    $PGO1
237500980722     C                   PARM      $dimOp        NumOpz
237600980722     C                   PARM                    $FX2
237700980722     C                   PARM                    $FC2
237800980722     C                   PARM                    $PGF2
237900980722     C                   PARM                    DsPgm
238000980722     C                   PARM                    xsccut
238100980722     C                   PARM                    xscgrp
238200980722     C*
238300980722     C                   Z-add     1             OPZLEN
238400980722     C     *blank        Lookup    $TX1(opzlen)                           21
238500980722     C                   if        *in21 = *on
238600980722     C                   sub       1             OPZLEN
238700980722     C                   else
238800980722     C                   Z-add     $DimOp        OPZLEN
238900980722     C                   endif
239000940127     C*
239100940117     C                   ENDSR
239200940223     C/EJECT
239300940223     C************************************************************
239400940223     C* INIZIALIZZAZIONE VARIABILI
239500940223     C************************************************************
239600940223     C     INZVAR        BEGSR
239700941215     C*
239800941215     C* Dati da PGM chiamante
239900941215     C*
240000960118    >C                   MOVEL     KPJBU         ndc077DS
240100951206     C*
240200951206     C* Se società diversa da quella impostata nella *inzsr, rieseguo
240300951206     C* reperimento dati società
240400960202     C                   If        Societa077 <> xscsoc and
240500960202     c                             societa077 <> *blanks
240600951122    >C                   MOVEL     'SOC001'      TIPXSC
240700960118     C                   MOVEL     Societa077    SOCXSC
240800951122     C                   EXSR      REPSOC
240900951010     C     RTNXSC        IFNE      '1'
241000951010    >C                   MOVEL     XSOCDS        SOC001
241100951010     C                   ELSE
241200951010     C                   MOVE      *ON           $FINE
241300951010     C                   EXSR      ENDPGM
241400951010     C                   ENDIF
241500951206     C                   endif
241600941201     C*
241700941124     C* Reperimento autorizzazioni
241800941124     C*
241900960108    >C     AUTDS         IFEQ      *BLANK
242000941124    >C                   EXSR      REPAUT
242100960108    >C                   ELSE
242200960118    >C                   MOVEL     XCADDS        CONPNODS
242300960108    >C                   ENDIF
242400941026     C*
242500941026     C* Reperimento valori default utente
242600941026     C*
242700941026     C                   EXSR      REPDFT
242800940506     C*
242900940506     C* Definizione delle opzioni utilizzabili a pgm
243000940506     C*
243100940506     C                   EXSR      DEFOPZ
243200950330     C*
243300940223     C* Variabili per gestione videate
243400940223     C*
243500940223     C                   MOVE      *OFF          $FINE                          fine pgm
243600940223     C                   MOVE      *OFF          $INZD1                         inizializzione D1
243700940223     C                   MOVE      *OFF          $INZS1                         inizializzione S1
243800940223     C                   MOVE      *OFF          $INZD2                         inizializzione D2
243900950606     C                   MOVE      *OFF          $EndFile                       memo fine file
244000940223     C                   MOVE      *OFF          $ESCI                          rich.uscita ciclo
244100940223     C                   MOVE      *OFF          $RCDOK                         rcd letto valido
244200940509     C                   MOVE      *HIVAL        $LASTV
244300940506     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
244400940506     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
244500940916     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
244600940223     C*
244700940318     C                   MOVE      *ON           $INZD1
244800940318     C                   MOVE      *ON           $INZS1
244900940831     C*
245000950330     C*
245100950330     C* Variabili appoggio
245200001122d0771C* inizialmente imposto il numero di righe per pagina a 10
245300001122d0771C                   Eval      SflPag    =   10
245400950330     C*
245500950330     C                   Z-ADD     SFLPAG        WPAG
245600950330     C                   MOVEL     *OFF          WOPR
245700960110     C                   clear                   W1POSd
245800941104     C*
245900941104     C* definisco qual'è la prima videata
246000950410     C* (se la prima videata risulta la lista, devo comunque caricare
246100950410     C*  la videata di parzializzazione e gestire la definizione
246200950410     C*  dell'ordinamento, perchè, dai default oppure se sono stati
246300950410     C*  passati dei dati, può variare il caricamento)
246400940223     C                   SELECT
246500940223     C     $LOP          WHENEQ    1
246600940223     C                   MOVE      'D1'          $GEST
246700940223     C     $LOP          WHENEQ    2
246800940223     C                   MOVE      'S1'          $GEST
246900941104     C                   EXSR      INZD1
247000960110     C                   MOVE      *OFF          $INZD1
247100960118     c                   eval      dtregaR = Wdate
247200950410     C                   EXSR      DEFORD
247300941104     C                   ENDSL
247400940223     C*
247500940223     C* Valorizzazione campi univoci testate
247600940223     C*
247700940223     C                   CLEAR                   FMTT1
247800940223     C                   MOVEL     KNSIF         NOMSIF
247900941109     C                   MOVEL     XSCDSI        NOMDIT
248000941027     C                   MOVEL     DSPGM         NOMPGM
248100950331     C* recupero il sottotitolo (se c'è)
248200950331     C                   MOVEL     *BLANK        T1TIT
248300950331     C     SOTTIT        IFNE      *BLANK
248400950331     C                   MOVEL     SOTTIT        IDMSG
248500950522     C                   CALLB     'XRTVM'
248600950331     C                   PARM                    IDMSG            27
248700950331     C                   PARM                    TXTMSG          100
248800950331     C                   PARM                    ERRMSG            1
248900950331     C                   PARM      *ON           CTRMSG            1            centratura
249000950331     C                   PARM      30            LENMSG            3 0          lun output.
249100950331     C     ERRMSG        IFNE      *ON
249200950331     C                   MOVEL     TXTMSG        T1TIT
249300950331     C                   ELSE
249400950331     C                   MOVEL     *ALL'?'       T1TIT
249500950331     C                   ENDIF
249600950331     C                   ENDIF
249700960119     C*
249800960119     C* Eseguo strcmtctl
249900960119     C                   CALLb     'XSTRCMT'
250000980720     C*
250100980720     C                   ENDSR
250200941209     C/EJECT
250300941209     C************************************************************
250400941209     C* REPERIMENTO DATI SOCIETA'
250500941209     C************************************************************
250600941209     C     REPSOC        BEGSR
250700941209     C*
250800950522     C                   CALLB     'XSOC'
250900941209     C                   PARM                    TIPXSC            6
251000941209     C                   PARM                    SOCXSC            3
251100941209     C                   PARM                    CDSXSC            9 0
251200941209     C                   PARM                    MODXSC            3
251300941209     C                   PARM      *BLANKS       RTNXSC            1
251400941209     C                   PARM                    XSOCDS
251500941209     C                   PARM                    KPJBA
251600941209     C*
251700941209     C                   ENDSR
251800941124     C/EJECT
251900941124     C************************************************************
252000941124     C* REPERIMENTO AUTORIZZAZIONI UTENTE
252100941124     C************************************************************
252200941124     C     REPAUT        BEGSR
252300941124     C*
252400941124     C                   CLEAR                   AUTDS
252500941124     C*
252600941124     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
252700941124     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
252800941124     C                   Z-ADD     0             XCAGRP                         GRUPPO
252900941124     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
253000960118    >C                   MOVEL     'CONPNO'      XCAFNC                         FUNZIONE
253100941124    >C                   MOVEL     *ZERO         XCAVFU                         VARIABILE
253200941124    >C                   MOVE      'CK'          XCATCK                         TIPO CTRL
253300941124    >C                   MOVE      '1'           XCANAU                         RICH.XNOAUT
253400941124     C*
253500950522     C                   CALLB     'XCHKAUT'
253600941124     C                   PARM                    AUTDS
253700941124     C*
253800941124     C* errori/non abilitazione
253900941124     C     XCARTN        IFNE      0
254000941124     C                   EXSR      ENDPGM
254100941124     C                   ELSE
254200960118    >C                   MOVEL     XCADDS        CONPNODS
254300941124     C                   ENDIF
254400941124     C*
254500941124     C                   ENDSR
254600940223     C/EJECT
254700940127     C************************************************************
254800940131     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
254900940127     C************************************************************
255000940131     C     REPDFT        BEGSR
255100940127     C*
255200950524     C* preparo i parametri di chiamata al driver
255300950524     C                   CLEAR                   XMPEDS
255400950524     C                   EVAL      TpChiamata = '01'
255500950524     C                   EVAL      NChiavi = 5
255600950524     C                   EVAL      VLogica = 2
255700950524     C*
255800950524     C* preparo le chiavi per la chiamata al driver
255900950524     C                   CLEAR                   XMPEDS1
256000950524     C                   MOVEL     XSCSOC        DSMPESOC
256100950524     C                   MOVEL     DSPGM         DSMPEPGM
256200950524     C                   MOVEL     KNMUS         DSMPEPRF
256300950524     C* Si reperiscono i dati di default a seconda delle modalità con
256400950524     C* cui il pgm è usato
256500950524     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
256600950524     C* MPETIP = 'PV'  pgm richiamato da menù
256700950524     C* MPETIP = 'PB'  x lancio Batch
256800941013     C                   SELECT
256900960118    >C     OPZ077        WHENEQ    '01'
257000950524     C                   MOVEL     'PX'          DSMPETIP
257100941013     C                   OTHER
257200950524     C                   MOVEL     'PV'          DSMPETIP
257300941013     C                   ENDSL
257400950524     C*
257500950524     C                   CALLB     'XMPE'
257600950524     C                   PARM                    XMPEDS
257700950524     C                   PARM                    XMPEDS1
257800950524     C*
257900950524     C     Risultato     IFEQ      '0'
258000950524     C                   MOVEL     DSMPEPAR      DSMPE
258100940131     C* nel caso sia la prima volta che l'utente apre questa anagrafica
258200940131     C* i suoi valori di default vengono decisi a pgm
258300940131     C                   ELSE
258400940201     C                   Z-ADD     1             $LOP
258500940131     C                   Z-ADD     1             $ORD
258600941027     C                   ENDIF
258700940131     C*
258800940131     C                   ENDSR
258900940223     C/EJECT
259000940131     C************************************************************
259100940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
259200940131     C************************************************************
259300940225     C     DEFOPZ        BEGSR
259400940131     C*
259500940225     C* Questa routine deve valorizzare la schiera $FL1
259600940225     C* con S o N a seconda della utilizzabilità o meno
259700940225     C* dell'opzione correlata
259800940225     C*
259900950301     C* Inizialmente si considerano tutte le opzioni come valide
260000940317     C     1             DO        $DIMOP        X
260100940225     C                   MOVE      'S'           $FL1(X)
260200941027     C                   ENDDO
260300940715     C*
260400940715     C* Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
260500941018     C* con 1 o 0 a seconda della utilizzabilità o meno
260600940715     C* del tasto funzione correlato
260700940715     C*
260800940715     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
260900940715     C* di compilazione
261000940715     C                   RESET                   $FC1
261100940715     C                   RESET                   $FR1
261200940715     C                   RESET                   $FC2
261300940715     C                   RESET                   $FR2
261400940919     C                   RESET                   $FC3
261500940919     C                   RESET                   $FR3
261600940225     C*
261700940225     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
261800940225     C* invalido alcune opzioni
261900940225     C                   SELECT
262000940225     C* Se pgm richiamato in ricerca disattivo :
262100960118    >C     OPZ077        WHENEQ    '01'
262200940603     C* F3=Fine
262300940603     C                   Z-ADD     3             X
262400941216     C                   MOVE      '0'           $FC1(X)
262500941216     C                   MOVE      '0'           $FC2(X)
262600941216     C                   MOVE      '0'           $FC3(X)
262700950301     C*
262800940225     C* Se pgm richiamato in interrogazione disattivo :
262900960118    >C     OPZ077        WHENEQ    '05'
263000940225     C* 1=Selezione
263100940225     C                   Z-ADD     1             X
263200940225     C     01            LOOKUP    $OP1(X)                                21
263300940225     C     *IN21         IFEQ      *ON
263400940225     C                   MOVE      'N'           $FL1(X)
263500940225     C                   ENDIF
263600940225     C* 4=Annullamento
263700940225     C                   Z-ADD     1             X
263800940225     C     04            LOOKUP    $OP1(X)                                21
263900940225     C     *IN21         IFEQ      *ON
264000940225     C                   MOVE      'N'           $FL1(X)
264100940225     C                   ENDIF
264200940225     C*
264300960118     C     OPZ077        WHENEQ    *BLANKS
264400940225     C* Se pgm non richiamato, disattivo subito :
264500941124     C* F8=Selezione seguente
264600941124     C                   Z-ADD     8             X
264700941206     C                   MOVE      '0'           $FC1(X)
264800941206     C                   MOVE      '0'           $FC2(X)
264900940225     C* 1=Selezione
265000940225     C                   Z-ADD     1             X
265100940225     C     01            LOOKUP    $OP1(X)                                21
265200940225     C     *IN21         IFEQ      *ON
265300940225     C                   MOVE      'N'           $FL1(X)
265400940225     C                   ENDIF
265500940225     C*
265600950301     C* In tutti gli altri casi, disattivo subito :
265700941228     C                   OTHER
265800941228     C* 1=Selezione
265900941228     C                   Z-ADD     1             X
266000941228     C     01            LOOKUP    $OP1(X)                                21
266100941228     C     *IN21         IFEQ      *ON
266200941228     C                   MOVE      'N'           $FL1(X)
266300941228     C                   ENDIF
266400941228     C*
266500940225     C                   ENDSL
266600940225     C*
266700940225     C                   ENDSR
266800940225     C/EJECT
266900940225     C************************************************************
267000940225     C* REPERIMENTO OPZIONI
267100940225     C************************************************************
267200940225     C     REPOPZ        BEGSR
267300940225     C*
267400940314     C     1             DO        $DIMOP        X
267500940128     C*
267600940223     C     $MS1(X)       IFNE      *BLANK
267700940128     C*
267800940223     C                   MOVEL     $MS1(X)       IDMSG
267900950522     C                   CALLB     'XRTVM'
268000940201     C                   PARM                    IDMSG            27
268100940201     C                   PARM                    TXTMSG          100
268200940201     C                   PARM                    ERRMSG            1
268300940128     C*
268400940128     C     ERRMSG        IFNE      *ON
268500940223     C                   MOVEL     TXTMSG        $TX1(X)
268600940128     C                   ELSE
268700940223     C                   MOVEL     *ALL'?'       $TX1(X)
268800941027     C                   ENDIF
268900940128     C*
269000941027     C                   ENDIF
269100940128     C*
269200941027     C                   ENDDO
269300940128     C*
269400940128     C                   ENDSR
269500940506     C/EJECT
269600940506     C************************************************************
269700940506     C* REPERIMENTO TASTI DI FUNZIONE
269800940506     C************************************************************
269900940506     C     REPKEY        BEGSR
270000940506     C                   MOVEA     $FM1          $FM
270100950721     C                   MOVEA     $FC1          $FC
270200940506     C                   EXSR      REPKKK
270300940506     C                   MOVEA     $FX           $FX1
270400940506     C*
270500940506     C                   MOVEA     $FM2          $FM
270600950721     C                   MOVEA     $FC2          $FC
270700940506     C                   EXSR      REPKKK
270800940506     C                   MOVEA     $FX           $FX2
270900940919     C*
271000940919     C                   MOVEA     $FM3          $FM
271100950721     C                   MOVEA     $FC3          $FC
271200940919     C                   EXSR      REPKKK
271300940919     C                   MOVEA     $FX           $FX3
271400940919     C                   ENDSR
271500940506     C/EJECT
271600940506     C************************************************************
271700940506     C* REPERIMENTO TASTI DI FUNZIONE
271800940506     C************************************************************
271900940506     C     REPKKK        BEGSR
272000940506     C*
272100940506     C                   CLEAR                   $FX
272200940506     C     1             DO        24            X
272300940506     C*
272400940506     C     $FM(X)        IFNE      *BLANK
272500950721     C     $Fc(X)        andne     '0'
272600940506     C*
272700940506     C                   MOVEL     $FM(X)        IDMSG
272800950522     C                   CALLB     'XRTVM'
272900940506     C                   PARM                    IDMSG            27
273000940506     C                   PARM                    TXTMSG          100
273100940506     C                   PARM                    ERRMSG            1
273200940506     C*
273300940506     C     ERRMSG        IFNE      *ON
273400940506     C                   MOVEL     TXTMSG        $FX(X)
273500940506     C                   ELSE
273600940506     C                   MOVEL     *ALL' '       $FX(X)
273700941027     C                   ENDIF
273800940506     C*
273900941027     C                   ENDIF
274000940506     C*
274100941027     C                   ENDDO
274200940506     C*
274300940506     C                   ENDSR
274400940223     C/EJECT
274500940128     C************************************************************
274600950606     C* DEFINIZIONE KLIST
274700940128     C************************************************************
274800950606     C     DefKlist      BEGSR
274900940128     C*
275000940223     C* klist
275100960118    >C* ndbhm02l
275200960118    >C     K02bhm02      KLIST
275300960118    >C                   KFLD                    bhmsocieta
275400960118    >C                   KFLD                    bhmdtreg
275500940223     C*
275600960118    >C     K04bhm02      KLIST
275700960118    >C                   KFLD                    bhmsocieta
275800960118    >C                   KFLD                    bhmdtreg
275900960118    >C                   KFLD                    bhmrifint
276000960118    >C                   KFLD                    bhmsubnum
276100940127     C*
276200940117     C                   ENDSR
276300960119     C/EJECT
276400960119     C************************************************************
276500960119     C* Reperimento testo messaggi per contabilizzazione interattiva
276600960119     C* non riuscita
276700960119     C************************************************************
276800960119     C     REPmsg        BEGSR
276900960119     c*
277000960119     C                   eval      IDMSG = 'PROMSG    *LIBL     COS0209'
277100960119     c                   exsr      prendimsg
277200960119     C     ERRMSG        IFNE      *ON
277300960119     C                   MOVEL     TXTMSG        MSGMS1
277400960119     C                   ELSE
277500960119     C                   MOVEL     *ALL'?'       msgms1
277600960119     C                   ENDIF
277700960119     c*
277800960119     C                   eval      IDMSG = 'PROMSG    *LIBL     COS0210'
277900960119     c                   exsr      prendimsg
278000960119     C     ERRMSG        IFNE      *ON
278100960119     C                   MOVEL     TXTMSG        MSGMS2
278200960119     C                   ELSE
278300960119     C                   MOVEL     *ALL'?'       msgms2
278400960119     C                   ENDIF
278500960119     c*
278600960119     C                   eval      IDMSG = 'PROMSG    *LIBL     COS0211'
278700960119     c                   exsr      prendimsg
278800960119     C     ERRMSG        IFNE      *ON
278900960119     C                   MOVEL     TXTMSG        MSGMS3
279000960119     C                   ELSE
279100960119     C                   MOVEL     *ALL'?'       msgms3
279200960119     C                   ENDIF
279300960119     c*
279400960119     C                   ENDSR
279500960119     C/EJECT
279600960119     C************************************************************
279700960119     C* Reperimento testo messaggi per contabilizzazione interattiva
279800960119     C* non riuscita
279900960119     C************************************************************
280000960119     C     PrendiMsg     BEGSR
280100960119     C*
280200960119     C                   CALLB     'XRTVM'
280300960119     C                   PARM                    IDMSG            27
280400960119     C                   PARM                    TXTMSG          100
280500960119     C                   PARM                    ERRMSG            1
280600960119     C                   PARM      *ON           CTRMSG            1            centratura
280700960125     C                   PARM      70            LENMSG            3 0          lun output.
280800960119     C*
280900960119     C                   ENDSR
281000960119     C/EJECT
281100001122d0771C* per S1: cambiato msg x f13 e abilitato il tasto
281200941214**
281300940718PROMSG    *LIBL     OPZ0001
281400960118PROMSG    *LIBL     OPZ0004
281500960118PROMSG    *LIBL     OPZ0120
281600960118PROMSG    *LIBL     OPZ0041
281700940201**
28180094022501S      <Selezione
28190094022504S      <Annullamento
28200096011815S      <Dettaglio
28210096011841S      <contabilità
282200940506** TASTI DI FUNZIONE  D1
282300940506PROMSG    *LIBL     KEY0001  01
282400940506PROMSG    *LIBL     KEY0002  02
282500940506PROMSG    *LIBL     KEY0003  03
282600940506PROMSG    *LIBL     KEY0004  04
282700940506PROMSG    *LIBL     KEY0005  05
282800940506PROMSG    *LIBL     KEY0006  06
282900940506PROMSG    *LIBL     KEY0007  07
283000940506PROMSG    *LIBL     KEY0008  08
283100940506PROMSG    *LIBL     KEY0009  09
283200940506PROMSG    *LIBL     KEY0010  10
283300940506PROMSG    *LIBL     KEY0011  11
283400940506PROMSG    *LIBL     KEY0012  12
283500940506PROMSG    *LIBL     KEY0013  13
283600940506PROMSG    *LIBL     KEY0014  14
283700940506PROMSG    *LIBL     KEY0015  15
283800940506PROMSG    *LIBL     KEY0016  16
283900940506PROMSG    *LIBL     KEY0017  17
284000940506PROMSG    *LIBL     KEY0018  18
284100940506PROMSG    *LIBL     KEY0019  19
284200940506PROMSG    *LIBL     KEY0020  20
284300940506PROMSG    *LIBL     KEY0021  21
284400940506PROMSG    *LIBL     KEY0022  22
284500940506PROMSG    *LIBL     KEY0023  23
284600940506PROMSG    *LIBL     KEY0024  24
284700940506**  ABILITAZIONE D1
2848009407200N  01
2849009507031N  02
2850009407201N  03
2851009407201N  04
2852009407200N  05
2853009407200N  06
2854009407200N  07
2855009412061N  08
2856009407200N  09
2857009601100N  10
2858009407200N  11
2859009407201N  12
2860009407200N  13
2861009407200N  14
2862009407200N  15
2863009407200N  16
2864009407200N  17
2865009407201N  18
2866009407200N  19
2867009407200N  20
2868009407200N  21
2869009407200N  22
2870009507260N  23   Impostare a '1' se si utilizzano opzioni
2871009507261N  24   Lasciare sempre impostata a '1' il valore della riga 24
287200940916** TASTI DI FUNZIONE  S1
287300940916PROMSG    *LIBL     KEY0001  01
287400940916PROMSG    *LIBL     KEY0002  02
287500940916PROMSG    *LIBL     KEY0003  03
287600940916PROMSG    *LIBL     KEY0004  04
287700940916PROMSG    *LIBL     KEY0005  05
287800940916PROMSG    *LIBL     KEY0006  06
287900940916PROMSG    *LIBL     KEY0007  07
288000940916PROMSG    *LIBL     KEY0008  08
288100940916PROMSG    *LIBL     KEY0009  09
288200940916PROMSG    *LIBL     KEY0010  10
288300940916PROMSG    *LIBL     KEY0011  11
288400940916PROMSG    *LIBL     KEY0012  12
288500001122PROMSG    *LIBL     KEY0061  13
288600940916PROMSG    *LIBL     KEY0014  14
288700940916PROMSG    *LIBL     KEY0015  15
288800940916PROMSG    *LIBL     KEY0016  16
288900940916PROMSG    *LIBL     KEY0017  17
289000940916PROMSG    *LIBL     KEY0018  18
289100940916PROMSG    *LIBL     KEY0019  19
289200940916PROMSG    *LIBL     KEY0020  20
289300940916PROMSG    *LIBL     KEY0021  21
289400940916PROMSG    *LIBL     KEY0022  22
289500940916PROMSG    *LIBL     KEY0023  23
289600940916PROMSG    *LIBL     KEY0024  24
289700940916**  ABILITAZIONE S1
2898009409160N  01
2899009409160N  02
2900009409161N  03
2901009409160N  04
2902009409160N  05
2903009409160N  06
2904009409160N  07
2905009411241N  08
2906009409160N  09
2907009601100N  10
2908009409160N  11
2909009409161N  12
2910000011221N  13
2911009409160N  14
2912009409160N  15
2913009409160N  16
2914009409161N  17
2915009409161N  18
2916009409160N  19
2917009409160N  20
2918009409160N  21
2919009409160N  22
2920009507261N  23   Impostare a '1' se si utilizzano opzioni
2921009507261N  24   Lasciare sempre impostata a '1' il valore della riga 24
292200940916** TASTI DI FUNZIONE  D3
292300940506PROMSG    *LIBL     KEY0001  01
292400940506PROMSG    *LIBL     KEY0002  02
292500940506PROMSG    *LIBL     KEY0003  03
292600940506PROMSG    *LIBL     KEY0004  04
292700940506PROMSG    *LIBL     KEY0005  05
292800940506PROMSG    *LIBL     KEY0006  06
292900940506PROMSG    *LIBL     KEY0007  07
293000940506PROMSG    *LIBL     KEY0008  08
293100940506PROMSG    *LIBL     KEY0009  09
293200940506PROMSG    *LIBL     KEY0010  10
293300940506PROMSG    *LIBL     KEY0011  11
293400940506PROMSG    *LIBL     KEY0012  12
293500940506PROMSG    *LIBL     KEY0013  13
293600940506PROMSG    *LIBL     KEY0014  14
293700940506PROMSG    *LIBL     KEY0015  15
293800940506PROMSG    *LIBL     KEY0016  16
293900940506PROMSG    *LIBL     KEY0017  17
294000940506PROMSG    *LIBL     KEY0018  18
294100940506PROMSG    *LIBL     KEY0019  19
294200940506PROMSG    *LIBL     KEY0020  20
294300940506PROMSG    *LIBL     KEY0021  21
294400940506PROMSG    *LIBL     KEY0022  22
294500940506PROMSG    *LIBL     KEY0023  23
294600940506PROMSG    *LIBL     KEY0024  24
294700940916**  ABILITAZIONE D3
2948009407200N  01
2949009407200N  02
2950009407201N  03
2951009407200N  04
2952009407200N  05
2953009407200N  06
2954009407200N  07
2955009407200N  08
2956009407200N  09
2957009409160N  10
2958009407200N  11
2959009407201N  12
2960009407200N  13
2961009407200N  14
2962009407200N  15
2963009407200N  16
2964009409160N  17
2965009409160N  18
2966009407200N  19
2967009407200N  20
2968009407200N  21
2969009407200N  22
2970009507260N  23   Impostare a '1' se si utilizzano opzioni
2971009507261N  24   Lasciare sempre impostata a '1' il valore della riga 24
