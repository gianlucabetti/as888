000100981214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(QILE)
000200981214     H*PARMS BNDDIR(PJXBND PJCBND)
000300940211     H DECEDIT('0,') DATEDIT(*DMY.)
000400941115      *
000500950703    > *            GUIDA ANAGRAFICA particolarità di partita
000600950703      *           ------------------------------------------
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
002900940224      *  50 <---> 98  ERRORI SU VIDEO
003000950526      *  96           ERRORE SPECIALE : ERRORI IN ALTRE VIDEATE
003100940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
003200940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
003300940224      *  99           INDIC. GENERALE DI ERRORE
003400940128     F*----------------------------------------------------*
003500940314    >F*----------------------------------------------------------------
003600940314    >F* Viste logiche utilizzate per i diversi ordinamenti
003700940314    >F*----------------------------------------------------------------
003800940909     F*------------
003900950703    >FNDPPA01L  IF   E           K DISK
004000950703    >F                                     RENAME(NDPPA000:NDPPA01)
004100960313    >FNDPPA03L  IF   E           K DISK
004200960313    >F                                     RENAME(NDPPA000:NDPPA03)
004300950704    >FNDPPS01L  IF   E           K DISK
004400950704    >F                                     RENAME(NDPPS000:NDPPS01)
004500950704    >FNDPSC01L  IF   E           K DISK
004600950704    >F                                     RENAME(NDPSC000:NDPSC01)
004700950704    >FNDMIX01L  IF   E           K DISK
004800950704    >F                                     RENAME(NDMIX000:NDMIX01)
004900950706    >FNDPAS02J  IF   E           K DISK
005000950706    >F                                     RENAME(NDPAS002:NDPAS02)
005100981218    >FNDCPA01L  IF   E           K DISK
005200981218    >F                                     RENAME(NDCPA000:NDCPA01)
005300950906    >FANUNI01L  IF   E           K DISK
005400950906    >F                                     RENAME(ANUNI000:ANUNI01)
005500940211     F*------------
005600950703    >FNDC060D   CF   E             WORKSTN
005700950703     F                                     RENAME(C060D1:FMTD1)
005800950703     F                                     RENAME(C060D2:FMTD2)
005900950703     F                                     RENAME(C060D3:FMTD3)
006000950907     F                                     RENAME(C060D4:FMTD4)
006100950703     F                                     RENAME(C060S1:FMTS1)
006200950703     F                                     RENAME(C060C1:FMTC1)
006300950703     F                                     RENAME(C060T1:FMTT1)
006400950703     F                                     RENAME(C060Z1:FMTZ1)
006500950703     F                                     RENAME(C060Z2:FMTZ2)
006600950703     F                                     RENAME(C060Z3:FMTZ3)
006700950907     F                                     RENAME(C060Z4:FMTZ4)
006800940131     F                                     SFILE(FMTS1:S1NRR)
006900940201     F                                     INFDS(DSFMT)
007000940128     D*----------------------------------------------------*
007100940302     D* Schiere per la gestione delle opzioni
007200940302     D*
007300950614     D* dimensione della schiera $MS1
007400980731     D $DIMOP          C                   CONST(30)
007500950524     D $MS1            S             27    DIM($dimop) CTDATA PERRCD(1)
007600950524     D $TX1            S             50    DIM($dimop) ALT($MS1)
007700950524     D $OP1            S              2  0 DIM($dimop) CTDATA PERRCD(1)
007800950524     D $FL1            S              1    DIM($dimop) ALT($OP1)
007900941214     D $TX             S             50    DIM(50)
008000941214     D $FL             S              1    DIM(50)
008100980720     D* schiera contenente i nomi dei programma richiamati attraverso
008200980720     D* le opzioni personalizzate
008300980720     D $PGO1           S             10    DIM($dimop)
008400980720     D*
008500980720     D* Indice della schiera per le opzioni personalizzate
008600980720     D PosOpz          S              2  0
008700980720     D* numero max opzioni
008800980720     D NumOpz          S              2  0 Inz($DIMOP)
008900980720     D* numero effettive opzioni
009000980720     D OpzLen          S              2  0
009100980720     D*-------------
009200940909     D* Schiere generali per la gestione dei tasti di funzione
009300940506     D $FM             S             27    DIM(24)
009400940506     D $FX             S             50    DIM(24)
009500940506     D $FC             S              1    DIM(24)
009600940506     D $FR             S              1    DIM(24)
009700980720     D* Indice della schiera tasti funzione
009800980720     D PosFun          S              2  0
009900940909     D* Schiere per la gestione dei tasti di funzione
010000940506     D* Video D1
010100940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
010200940506     D $FX1            S             50    DIM(24) ALT($FM1)
010300940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
010400940506     D $FR1            S              1    DIM(24) ALT($FC1)
010500940506     D* Schiere per la gestione dei tasti di funzione
010600940506     D* Video S1
010700940506     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
010800940506     D $FX2            S             50    DIM(24) ALT($FM2)
010900940506     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
011000940506     D $FR2            S              1    DIM(24) ALT($FC2)
011100980720     D* Schiera contente i nomi dei programmi da richiamare con i tasti
011200980720     D* funzione personalizzata S1
011300980720     D $PGF2           S             10    DIM(25)
011400940916     D* Schiere per la gestione dei tasti di funzione
011500940916     D* Video D2
011600940916     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
011700940916     D $FX3            S             50    DIM(24) ALT($FM3)
011800940916     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
011900940916     D $FR3            S              1    DIM(24) ALT($FC3)
012000950907     D* Schiere per la gestione dei tasti di funzione
012100950907     D* Video D4
012200950907     D $FM4            S             27    DIM(24) CTDATA PERRCD(1)
012300950907     D $FX4            S             50    DIM(24) ALT($FM4)
012400950907     D $FC4            S              1    DIM(24) CTDATA PERRCD(1)
012500950907     D $FR4            S              1    DIM(24) ALT($FC4)
012600940302     D*
012700940211     D/EJECT
012800940128     D*----------------------------------------------------*
012900940207     D* Valori assunti
013000950524     D XMPEDS        E DS                  INZ
013100950524     D XMPEDS1       E DS                  INZ
013200941111     D DSMPE           DS                  INZ
013300940207     D  $LOP                   1      1  0
013400950704     D  $ORD                   2      3  0
013500950524     D*-------------
013600950704     D* Parametri del PGM di test esistenza commenti ANA724R
013700950704     D A724DS        E DS                  EXTNAME(ANA724DS) INZ
013800950704     D  EXI724       E                     INZ('0')
013900950704     D  RIS724       E                     INZ('0')
014000010305a0871x**CMT724       E                     INZ('0')
014100950704     D*-------------
014200950524     D* Parametri del PGM di gestione commenti ANA725R
014300950707     D ANA725DS      E DS                  INZ
014400950301     D  RET725       E                     INZ('0')
014500950301     D  OPR725       E                     INZ('0')
014600950301     D  ERR725       E                     INZ('0')
014700950524     D*-------------
014800950524     D* Parametri del PGM invio posta ANA727R
014900950707     D ANA727DS      E DS                  INZ
015000950301     D  RET727       E                     INZ('0')
015100950301     D  OPR727       E                     INZ('0')
015200950301     D  ERR727       E                     INZ('0')
015300940207     D*-------------
015400940211     D* Passaggio Parametri
015500940211     D KPJBA         E DS
015600940211     D*-------------
015700940225     D* Reperimento Autorizzazioni
015800941124     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
015900941124     D*-------------
016000950410     D* Passaggio Autorizzazioni
016100950410     D DSAUT           DS           512
016200950524     D*-------------
016300950524     D* Maschera dati autorizzazioni
016400950704    >D CONPPADS      E DS
016500950906     D*-------------
016600950906     D* Contollo autorizzazione contabilità
016700950906     D XCTBDS        E DS                  EXTNAME(XAUTCTBDS)
016800950906     D*-------------
016900950906     D* Autorizzazioni alla unità
017000950906     D XAutUniDS     E DS
017100941209     D*-------------
017200941209     D* Dati di ambiente ottenuti da XSOC
017300941209    >D SOC001        E DS                  EXTNAME(XSOC001DS)
017400950906     D    FgO1                        1    overlay(XscFgO)
017500941209     D*-------------
017600941209     D* DS Interna per dati di output di XSOC
017700941209     D XSOCDS          DS          1000
017800940325     D*-------------
017900940325     D* Parametri in ricezione
018000950703    >D NDC060DS      E DS
018001170223R224 X*ex R027 -----
018002170223 "   X*ex R027 Parametri del PGM di manutenzione  dati aggiuntivi
018003170223 "   X*ex R027 SDGSD16DS     E DS                  inz
018004170223 "   X*ex R027 RETD16       E                     INZ('0')
018005170223 "   X*ex R027 OPRD16       E                     INZ('0')
018006170223 "   X*ex R027 ERRD16       E                     INZ('0')
018007170223R224 X*ex R027 CMTD16       E                     INZ('0')
018008170223R224 D*-------------
018009170223 "   D* Parametri del PGM guida dati aggiuntivi anagrafici
018010170223 "   D SDGSD15DS     E DS                  inz
018011170223 "   D  RETD15       E                     INZ('0')
018012170223 "   D  OPRD15       E                     INZ('0')
018013170223 "   D  ERRD15       E                     INZ('0')
018014170223R224 D  CMTD15       E                     INZ('0')
018100940325     D*-------------
018200980720     D* campo di salvataggio per la DS d'ingresso durante la chiamata
018300980720     D* alla funzione personalizzata
018400980720    >D $SaveDS         S                   like(NDC060DS)
018500980720     D*-------------
018600950703     D* Parametri del PGM di manutenzione NDC061R
018700950704    >D NDC061DS      E DS                  inz
018800950703    >D  RET061       E                     INZ('0')
018900950703    >D  OPR061       E                     INZ('0')
019000950703    >D  ERR061       E                     INZ('0')
019100950707     D*-------------
019200950707     D* Parametri del PGM di manutenzione NDC063R scadenze/rate
019300950707    >D NDC063DS      E DS                  inz
019400950707    >D  RET063       E                     INZ('0')
019500950707    >D  OPR063       E                     INZ('0')
019600950707    >D  ERR063       E                     INZ('0')
019700950707     D*-------------
019800950707     D* Parametri del PGM di manutenzione NDC065R mix
019900950707    >D NDC065DS      E DS                  inz
020000950707    >D  RET065       E                     INZ('0')
020100950707    >D  OPR065       E                     INZ('0')
020200950707    >D  ERR065       E                     INZ('0')
020300960123     D*-------------
020400981218     D* Parametri del PGM di manutenzione NDC066R1 est. nominativi
020500981218    >D NDC066DS1     E DS                  inz
020600981218    >D  RET0661      E                     INZ('0')
020700981218    >D  OPR0661      E                     INZ('0')
020800981218    >D  ERR0661      E                     INZ('0')
020900981218     D*-------------
021000960123     D* Interrogazione conti
021100960123     D ANA399DS      E DS                  inz
021200950906     D*-------------
021300950906     D* Richiamo a XATB
021400950906     D XATBDS        E DS
021500950906     D*-------------
021600950906     D* Parametri per ricerca codice ANATB00F
021700950906     D XTABDS        E DS
021800950907     D*-------------
021900950907     D* Tabella blocco amministrativo
022000950907     D TAB0AB        E DS                  EXTNAME(ANG0ABDS)
022100950906     D*-------------
022200950906     D* Tabella contabilità
022300950906     D TABG84        E DS                  EXTNAME(ANGG84DS)
022400950906     D*-------------
022500950906     D* Ricerca unità
022600950906    >D A009DS        E DS                  EXTNAME(ANA009DS) INZ
022700950704     D*-------------
022800950704     D* Ricerca capoconti
022900950704     D A200DS        E DS                  EXTNAME(ANA200DS) INZ
023000950704     D*-------------
023100950704     D* Ricerca sottoconti
023200950704     D A201DS        E DS                  EXTNAME(ANA201DS) INZ
023300950704     D*-------------
023400950704     D* Ricerca legende
023500950704     D A964DS        E DS                  EXTNAME(ANA964DS) INZ
023600950704     D*-------------
023700950704     D* Controllo conti
023800950704     D ND002DS       E DS
023900950704     D*-------------
024000950704     D* Controlli validità legende
024100950704     D X07DS         E DS                  EXTNAME(X07VALDS) INZ
024200960314     D*-------------
024300960314     D* DS per input DVCDP
024400960314     DDVCDPDS        E DS                  INZ
024500960314     D* Ds per output
024600960314     DDVOCDPDS       E DS                  INZ
024700960314     D*-------------
024800960314     D*  DS per input DVAGE
024900960314     D DVAGE         E DS                  EXTNAME(DVAGEDS) INZ
025000960314     D*  Ds per output DVAGE
025100960314     D DVOAGE        E DS                  EXTNAME(DVOAGEDS) INZ
025200950704     D*-------------
025300950704     D* Definizioni parametri standard chiamate moduli
025400950704     D$Esito           S              1
025500950704     D$gesmsg          S              1
025600950704     D$lenout          S              9B 0
025700950704     D$struttura       S             10
025800950704     D$kcc             S              6
025900950704     D$ksc             S              8
026000940211     D*-------------
026100940211     D DSFMT           DS           512
026200940506     D  $TASTO               369    369
026300940211     D  NRG                  370    370
026400940211     D  NCL                  371    371
026500940211     D  SFLNRR               378    379B 0
026600940211     D*-------------
026700940211     D* posizione cursore
026800940211     D CURSOR          DS
026900940211     D  RIRI                   1      2B 0 INZ
027000940211     D  R$$                    2      2
027100940211     D  COCO                   3      4B 0 INZ
027200940211     D  C$$                    4      4
027300940127     D*-------------
027400940127     D* Reperimento nome PGM
027500940127     D STATUS         SDS           333
027600940127     D  DSPGM            *PROC
027700950411     D  PARMS            *PARMS
027800950704     D*-------------
027900950704     D* Attributi video
028000950704     D* (definisco come costanti qugli attributi video che potrei
028100950704     D*  testare e quindi preferisco assegnargli un valore "parlante")
028200950704     D*  bottone presente
028300950704     D PRESENTE        C                   CONST(X'A1')
028400940207     D*-------------
028500940616     D* Costanti
028600940616     D* nr.di righe del sfl (SFLPAG)
028700960313     D SFLPAG          C                   CONST(9)
028800940616     D*-------------
028900940506     D* Tasti di funzione
029000940506     D F01             C                   CONST(X'31')
029100940506     D F02             C                   CONST(X'32')
029200940506     D F03             C                   CONST(X'33')
029300940506     D F04             C                   CONST(X'34')
029400940506     D F05             C                   CONST(X'35')
029500940506     D F06             C                   CONST(X'36')
029600940506     D F07             C                   CONST(X'37')
029700940506     D F08             C                   CONST(X'38')
029800940506     D F09             C                   CONST(X'39')
029900940506     D F10             C                   CONST(X'3A')
030000940506     D F11             C                   CONST(X'3B')
030100940506     D F12             C                   CONST(X'3C')
030200940506     D F13             C                   CONST(X'B1')
030300940506     D F14             C                   CONST(X'B2')
030400940506     D F15             C                   CONST(X'B3')
030500940506     D F16             C                   CONST(X'B4')
030600940506     D F17             C                   CONST(X'B5')
030700940506     D F18             C                   CONST(X'B6')
030800940506     D F19             C                   CONST(X'B7')
030900940506     D F20             C                   CONST(X'B8')
031000940506     D F21             C                   CONST(X'B9')
031100940506     D F22             C                   CONST(X'BA')
031200940506     D F23             C                   CONST(X'BB')
031300940506     D F24             C                   CONST(X'BC')
031400940506     D ENTER           C                   CONST(X'F1')
031500940506     D ROLDWN          C                   CONST(X'F4')
031600940506     D ROLLUP          C                   CONST(X'F5')
031700950704     D PARTPART        C                   CONST('Particolarità di partita')
031800950524     D*-------------
031900950524     D* Sottotitolo di testata
032000950703    >D* se non si hanno sottotitoli, mettere tutta la costante = ' '
032100950703     D SOTTIT          C                   CONST('              ')
032200950524     D*-------------
032300950606     D* Variabili per gestione videate
032400950606     D $Gest           S              2A
032500950606     D $Fine           S              1A
032600950606     D $InzD1          S              1A
032700950606     D $InzS1          S              1A
032800950606     D $InzD2          S              1A
032900950907     D $InzD4          S              1A
033000950606     D $LastG          S              2A
033100950606     D $LastV          S              2A
033200950606     D $EndFile        S              1A
033300950606     D $Esci           S              1A
033400950606     D $RcdOK          S              1A
033500950606     D $Unique         S              1A
033600950606     D*-------------
033700950606     D* Variabili appoggio sempre presenti in tutte le anagrafiche
033800950606     D S1Nrr           S                   like(C1Rcd)
033900951109     D SFLNRRSAV       S                   like(SFLNRR)
034000950606     D WSfl            S                   like(C1Nrr)
034100950606     D WMax            S                   like(C1Rcd)
034200950606     D WPag            S                   like(C1Rcd)
034300950606     D WInzS1          S                   like($InzS1)
034400950703    >D WOpr            S                   like(Opr060)
034500950606     D CurR            S              2S 0
034600950606     D CurC            S              2S 0
034700950606     D Win             S             99A
034800950606     D W1Pos           S                   like(C1Pos)
034900950606     D WINKR           S              1A
035000950704     D*------------
035100950704     D* Campi di lavoro
035101170223R027 D $DtFiVl         S               D
035200950704     D Wdate           S               D
035300960313     D D1dtchiuR       S               D
035400950704     D WWORD           S                   like(D1ORD)
035500950906     D WWctb           S                   like(D1ctb)
035600950907     D WWbka           S                   like(D4bkamm)
035700960313     D datmin          C                   const('0001-01-01')
035800960313     D datmax          C                   const('9999-12-31')
035801170223R224 D*----------------------------------------------------*
035802170223  "  D* Controllo chiamata ricorsiva
035803170223  "  D X48ChPgmDS    E DS                  INZ
035804170223R224 D*----------------------------------------------------*
035900950606     D*-------------
036000950606     D*  Indici
036100950606     D X               S              3S 0
036200950606     D Y               S              3S 0
036300950606     D*-------------
036400000118C1309D ZDateISO        S               D
036500021014C1595D* Attributi video
036600021014C1595D Normale         C                   Const(X'32')
036700021014C1595D Protetto        C                   Const(X'A0')
036800940127     C*----------------------------------------------------*
036900940127     C*                MAIN LINE PROGRAM
037000940127     C*----------------------------------------------------*
037100940127     C*
037200940223     C* inizializzazione variabili
037300940223     C                   EXSR      INZVAR
037400940223     C*
037500940223     C     $FINE         DOWEQ     *OFF
037600940131     C     $GEST         CASEQ     'D1'          GESD1
037700940131     C     $GEST         CASEQ     'S1'          GESS1
037800940131     C     $GEST         CASEQ     'D2'          GESD2
037900941027     C                   ENDCS
038000941027     C                   ENDDO
038100940325     C* fine programma
038200940325     C                   EXSR      ENDPGM
038300940225     C*
038400940325     C/EJECT
038500940325     C************************************************************
038600940325     C* FINE PROGRAMMA
038700940325     C************************************************************
038800940325     C     ENDPGM        BEGSR
038900940325     C*
039000940325     C* passaggio dati al pgm chiamante
039100950703    >C                   MOVEL     WOPR          OPR060
039200950703    >C                   MOVEL     NDC060DS      KPJBU
039300940325     C*
039400940325     C* Se pgm Guida non è stato richiamato posso chiudere in LR
039500950703    >C     OPZ060        IFEQ      *BLANK
039600940325     C                   SETON                                            LR
039700940325     C* altrimenti chiudo in RT
039800940325     C                   ELSE
039900940325     C                   SETON                                            RT
040000940325     C                   ENDIF
040100940616     C                   RETURN
040200940325     C*
040300940325     C                   ENDSR
040400940224     C/EJECT
040500940201     C************************************************************
040600940201     C* RICERCHE
040700940201     C************************************************************
040800940127     C     SEARCH        BEGSR
040900940127     C*
041000940127     C* determino Riga/Colonna del cursore
041100940127     C                   MOVE      NRG           R$$
041200940127     C                   MOVE      NCL           C$$
041300940127     C                   Z-ADD     RIRI          CURR
041400940127     C                   Z-ADD     COCO          CURC
041500950704     C*
041600950704     C     D1KCC         IFNE      *BLANKS
041700950704     C     D1KCC         ANDNE     'C     '
041800950704     C     D1KCC         ANDNE     'F     '
041900961011     C                   CALLb     'XALLINEA'
042000950704     C                   PARM                    D1KCC
042100950704     C                   PARM      6             LEN               5 0
042200950704     C                   PARM                    XSCINP
042300950704     C                   ENDIF
042400950704     C*
042500950704     C     D1KSC         IFNE      *BLANKS
042600961011     C                   CALLb     'XALLINEA'
042700950704     C                   PARM                    D1KSC
042800950704     C                   PARM      8             LEN               5 0
042900950704     C                   PARM                    XSCINP
043000950704     C                   ENDIF
043100940127     C*
043200941118   B1C                   SELECT
043300940127     C*
043400950703    >C*--------------------------------------------------------------
043500950703    >C* inserire le routine di ricerca
043600950703    >C*--------------------------------------------------------------
043700950906    >C* FMTD1 - UNITA
043800950906    >C     H1NMFL        WHENEQ    'D1UNITA'
043900950906    >C                   RESET                   A009DS
044000950906    >C                   MOVEL     '01'          OPZ009
044100950906    >C                   MOVEL     XSCSOC        SOC009
044200950906    >C                   MOVEL     A009DS        KPJBU
044300950906    >C                   CALL      'ANA009R'
044400950906     C                   PARM                    KPJBA
044500950906    >C                   MOVEL     KPJBU         A009DS
044600950906     C*
0447009509062   >C     UNI009        IFNE      *BLANK
044800950906    >C                   MOVE      UNI009        D1UNITA
0449009509062-  >C                   ENDIF
045000950906      * imposto pos. del cursore
045100950906     C                   Z-ADD     CURR          H1RIGA
045200950906     C                   Z-ADD     CURC          H1COLO
045300950906     C*
045400950906    >C*  FMTD1  - Contabilità
045500950906     C     H1NMFL        WHENEQ    'D1CTB'
045600950906     C                   RESET                   XTABDS
045700950906     C                   MOVE      *BLANK        WWCTB
045800950906     C                   MOVE      WWCTB         XTAKEY
045900950906     C                   MOVE      '01'          XTAOPZ
046000950906     C                   MOVE      XSCSOC        XTAAZI
046100950906     C                   MOVE      XSCLIN        XTALIN
046200950906     C                   Z-ADD     CURR          XTARIG
046300950906     C                   Z-ADD     CURC          XTACOL
046400950906     C                   CLEAR                   KPJBU
046500950906     C                   MOVEL     XTABDS        KPJBU
046600950906     C                   CALL      'ANGG84R'
046700950906     C                   PARM                    KPJBA
046800950906     C                   MOVEL     KPJBU         XTABDS
046900950906     C     XTAERR        IFNE      '1'
047000950906     C                   MOVE      XTAKEY        WWCTB
047100950906     C                   ELSE
047200950906     C                   MOVE      *BLANK        WWCTB
047300950906     C                   ENDIF
047400950906     C*
047500950906     C     WWCTB         IFNE      *BLANK
047600950906     C                   MOVEL     WWCTB         D1CTB
047700950906     C                   ENDIF
047800950906     C* imposto pos. del cursore
047900950906     C                   Z-ADD     CURR          H1RIGA
048000950906     C                   Z-ADD     CURC          H1COLO
048100950703    >C* FMTD1 - KCC
048200950703    >C     H1NMFL        WHENEQ    'D1KCC'
048300950704     C                   RESET                   A200DS
048400950704     C                   MOVEL     '01'          OPZ200
048500950704     C                   MOVE      D1KCC         KCC200
048600950704     C                   MOVEL     A200DS        KPJBU
048700950704     C                   CALL      'ANA200R'
048800950704     C                   PARM                    KPJBA
048900950704     C                   MOVEL     KPJBU         A200DS
049000950704     C*
049100950704     C     KCC200        IFNE      *BLANK
049200950704     C                   MOVEL     KCC200        D1KCC
049300950704     C                   ENDIF
049400950704     C* imposto pos. del cursore
049500950704     C                   Z-ADD     CURR          H1RIGA
049600950704     C                   Z-ADD     CURC          H1COLO
049700950704     C*
049800950704     C* FMTD1  - SOTTOCONTO
049900950704     C     H1NMFL        WHENEQ    'D1KSC'
050000950704     C                   RESET                   A201DS
050100950704     C                   MOVE      XSCSOC        SOC201
050200950704     C                   MOVE      D1KCC         KCC201
050300950704     C                   MOVE      D1KsC         KSC201
050400950704     C                   MOVEL     A201DS        KPJBU
050500950704     C                   CALL      'ANA201R'
050600950704     C                   PARM                    KPJBA
050700950704     C                   MOVEL     KPJBU         A201DS
050800950704     C*
050900950704     C     KSC201        IFNE      *BLANK
051000950704     C                   MOVEL     KCC201        D1KCC
051100950704     C                   MOVEL     KSC201        D1KSC
051200950704     C                   ENDIF
051300950704     C* imposto pos. del cursore
051400950704     C                   Z-ADD     CURR          H1RIGA
051500950704     C                   Z-ADD     CURC          H1COLO
051600950704     C*
051700950704     C* FMTD1  - Ascendente/discendente
051800950704     C     H1NMFL        WHENEQ    'D1ORD'
051900950704     C                   RESET                   A964DS
052000950704     C                   MOVEL     '01'          OPZ964
052100950704     C                   MOVEL     'ANDIZ000'    TRC964
052200950704     C                   MOVEL     'DSPORD'      CAM964
052300950704     C                   MOVEL     A964DS        KPJBU
052400950704     C                   CALL      'ANA964R'
052500950704     C                   PARM                    KPJBA
052600950704     C                   MOVEL     KPJBU         A964DS
052700950704     C                   MOVEL     VAL964        WWORD
052800950704     C*
052900950704     C     WWORD         IFNE      *BLANK
053000950704     C                   MOVEL     WWORD         D1ORD
053100950704     C                   ENDIF
053200950704     C* imposto pos. del cursore
053300950704     C                   Z-ADD     CURR          H1RIGA
053400950704     C                   Z-ADD     CURC          H1COLO
053500950907     C*
053600950907     C* FMTD4  - BLOCCO AMMINISTRATIVO
053700950907     C     H1NMFL        WHENEQ    'D4BKAMM'
053800950907     C                   RESET                   XTABDS
053900950907     C                   MOVE      *BLANK        WWBKA
054000950907     C                   MOVE      WWBKA         XTAKEY
054100950907     C                   MOVE      '01'          XTAOPZ
054200950907     C                   MOVE      XSCSOC        XTAAZI
054300950907     C                   MOVE      XSCLIN        XTALIN
054400950907     C                   Z-ADD     CURR          XTARIG
054500950907     C                   Z-ADD     CURC          XTACOL
054600950907     C                   CLEAR                   KPJBU
054700950907     C                   MOVEL     XTABDS        KPJBU
054800950907     C                   CALL      'ANG0ABR'
054900950907     C                   PARM                    KPJBA
055000950907     C                   MOVEL     KPJBU         XTABDS
055100950907     C     XTAERR        IFNE      '1'
055200950907     C                   MOVE      XTAKEY        WWBKA
055300950907     C                   ELSE
055400950907     C                   MOVE      *BLANK        WWBKA
055500950907     C                   ENDIF
055600950907     C*
055700950907     C     WWBKA         IFNE      *BLANK
055800950907     C                   MOVEL     WWBKA         D4BKAMM
055900950907     C                   ENDIF
056000950907     C* imposto pos. del cursore
056100950907     C                   Z-ADD     CURR          H1RIGA
056200950907     C                   Z-ADD     CURC          H1COLO
056300940223     C*
056400940223     C* ricerca non abilitata in questa posizione
056500941118   X1C                   OTHER
056600940929     C                   SETON                                        9899
056700940127     C*
056800941118   E1C                   ENDSL
056900950612     C*
057000950612     C* imposto pos. del cursore
057100950612     C                   Z-ADD     CURR          H1RIGA
057200950612     C                   Z-ADD     CURC          H1COLO
057300940127     C*
057400940506     C                   MOVE      *HIVAL        $LASTV
057500950612     C*
057600940127     C                   ENDSR
057700940131     C/EJECT
057800940224     C************************************************************
057900940131     C* RIEMPIMENTO OPZIONI
058000940224     C************************************************************
058100940131     C     RIEOPZ        BEGSR
058200940223     C*
058300940223     C* Input :
058400940223     C* - $TX = schiera con tutti i testi disponibili
058500940223     C* - $FL = schiere con i flag di validità associati ai testi
058600940225     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
058700940223     C*          opzione usato
058800940223     C* - WMXO = variabile contenente il numero massimo di testi
058900940223     C*          opzione usabili
059000940721     C* - $L01 = Lunghezza prima riga
059100940721     C* - $L02 = Lunghezza seconda riga
059200940223     C* Output :
059300940223     C* - WOPZ1 = variabile contenente la prima riga opzioni
059400940223     C* - WOPZ2 = variabile contenente la seconda riga opzioni
059500941214     C* - FF23  = flag che indica se c'è bisogno del tasto F23
059600940223     C*
059700950522     C                   CALLB     'X04OPMSG'
059800940223     C* input
059900940223     C                   PARM                    $TX
060000940223     C                   PARM                    $FL
060100940225     C                   PARM                    WCOZ              3 0
060200940223     C                   PARM                    WMXO              3 0
060300940223     C* output
060400940223     C                   PARM                    WOPZ1            76
060500940223     C                   PARM                    WOPZ2            76
060600941214     C                   PARM                    FF23              1
060700950301     C* input
060800940721    >C                   PARM                    $L01              3 0
060900950703    >C                   PARM      0             $L02              3 0
061000940223     C*
061100060224C1913C                   PARM                    $ALLOPT         960
061200060224C1913C*
061300940131     C                   ENDSR
061400940506     C/EJECT
061500940506     C************************************************************
061600940506     C* RIEMPIMENTO TASTI FUNZIONE
061700940506     C************************************************************
061800940506     C     RIEKEY        BEGSR
061900940506     C*
062000940506     C* Input :
062100940506     C* - $FX = schiera con tutti i testi disponibili
062200940506     C* - $FC = schiere con i flag di validità associati ai testi
062300940506     C* - $FR = schiere con i flag di REVERSE IMAGE
062400940506     C* - $ULK = ultimo testo caricato
062500940721     C* - $L01 = Lunghezza prima riga
062600940721     C* - $L02 = Lunghezza seconda riga
062700940506     C* Output :
062800940506     C* - $ULK
062900940609     C* - $KEY1= 1° riga di tasti funzione
063000940609     C* - $KEY2= 2° riga di tasti funzione
063100940506     C*
063200950522     C                   CALLB     'XKEYMSG'
063300940506     C* input
063400940506     C                   PARM                    $FX
063500940506     C                   PARM                    $FC
063600940506     C                   PARM                    $FR
063700940506     C                   PARM                    $ULK              3 0
063800940506     C* output
063900940506     C                   PARM                    $KEY1            79
064000940506     C                   PARM                    $KEY2            79
064100950301     C* input
064200940721    >C                   PARM                    $L01              3 0
064300940721    >C                   PARM                    $L02              3 0
064400940506     C*
064500060224C1913C                   PARM                    $ALLFUNCT       480
064600060224C1913C*
064700940506     C                   ENDSR
064800940224     C/EJECT
064900940224     C************************************************************
065000940224     C* GESTIONE EMISSIONE TASTI FUNZIONE
065100940224     C************************************************************
065200940224     C     GESPIE        BEGSR
065300940224     C*
065400940224     C                   SELECT
065500940224     C* Parzializzazioni
065600940224     C     $GEST         WHENEQ    'D1'
065700940506     C     $ULKD1        IFEQ      0
065800940506     C* riempimento opzioni
065900940506     C                   EXSR      F24D1
066000940506     C                   ELSE
066100940506     C                   WRITE     FMTZ1
066200940506     C                   ENDIF
066300940506     C* Lista
066400940506     C     $GEST         WHENEQ    'S1'
066500940506     C     $ULKS1        IFEQ      0
066600940506     C                   EXSR      F24S1
066700940506     C                   ELSE
066800940506     C                   WRITE     FMTZ2
066900940506     C                   ENDIF
067000940916     C* Modifica valori assunti
067100940916     C     $GEST         WHENEQ    'D2'
067200940916     C     $ULKD2        IFEQ      0
067300940916     C                   EXSR      F24D2
067400940916     C                   ELSE
067500940916     C                   WRITE     FMTZ3
067600940916     C                   ENDIF
067700950907     C* Blocco amministrativo
067800950907     C     $GEST         WHENEQ    'D4'
067900950907     C     $ULKD4        IFEQ      0
068000950907     C                   EXSR      F24D4
068100950907     C                   ELSE
068200950907     C                   WRITE     FMTZ4
068300950907     C                   ENDIF
068400940506     C                   ENDSL
068500940506     C*
068600940224     C                   ENDSR
068700940224     C/EJECT
068800940224     C************************************************************
068900940224     C* GESTIONE PARZIALIZZAZIONI
069000940224     C************************************************************
069100940224     C     GESD1         BEGSR
069200940224     C* inizializzazione videata
069300940223     C     $INZD1        IFEQ      *ON
069400940127     C                   EXSR      INZD1
069500940223     C                   MOVE      *OFF          $INZD1
069600941027     C                   ENDIF
069700940223     C* emissione piede videata se proveniente da altra
069800940223     C* salvataggio valore formato di provenienza
069900940202     C     $LASTV        IFNE      'D1'
070000940223     C* salvo stato F17 per sapere cosa fare in caso di F12
070100940223     C                   MOVE      *INKR         WINKR
070200940202     C                   WRITE     FMTT1
070300940224     C                   EXSR      GESPIE
070400940202     C                   MOVE      $LASTV        $LASTG
070500940202     C                   MOVE      'D1'          $LASTV
070600940707     C                   ENDIF
070700940707     C*
070800940707     C* se esistono errori sulla videata
070900940707     C* emetto la write del formato a indicatori spenti per vedere
071000940707     C* le eventuali decodifiche
071100940707     C     *IN99         IFEQ      *ON
071200940707     C                   MOVEA     *IN           WIN
071300940707     C                   MOVE      *ALL'0'       IN5098           49
071400940707     C                   MOVEA     IN5098        *IN(50)
071500940707     C                   WRITE     FMTD1
071600940707     C                   MOVEA     WIN           *IN
071700940707     C                   ENDIF
071800940131     C*
071900940131     C                   EXFMT     FMTD1
072000940506     C*
072100941205     C* controllo abilitazione tasti funzione
072200940506     C                   EXSR      CTRKD1
072300940506     C*
072400940506     C     *IN99         IFEQ      *OFF
072500940131     C*
072600940117     C                   SELECT
072700940916     C* F2=Decodifica
072800940916     C     $TASTO        WHENEQ    F02
072900940916     C                   EXSR      F02D1
073000940131     C* F3=Fine
073100940506     C     $TASTO        WHENEQ    F03
073200940307     C                   EXSR      F03D1
073300940131     C* F4=Ricerca
073400940506     C     $TASTO        WHENEQ    F04
073500940127     C                   EXSR      SEARCH
073600940916     C* F5=Refresh
073700940916     C     $TASTO        WHENEQ    F05
073800940916     C                   EXSR      F05D1
073900941206     C* F8=Successivo
074000941206     C     $TASTO        WHENEQ    F08
074100941206     C                   EXSR      F08D1
074200950301     C* F10=Immissione
074300950301     C     $TASTO        WHENEQ    F10
074400950301     C                   EXSR      F10D1
074500940131     C* F12=Ritorno
074600940506     C     $TASTO        WHENEQ    F12
074700940307     C                   EXSR      F12D1
074800940131     C* F18=Modifica valori assunti
074900940506     C     $TASTO        WHENEQ    F18
075000940307     C                   EXSR      F18D1
075100940224     C* F24=Altri tasti
075200940506     C     $TASTO        WHENEQ    F24
075300940309     C                   EXSR      F24D1
075400940131     C*
075500940117     C                   OTHER
075600940127     C                   EXSR      CTRD1
075700940127     C     *IN99         IFEQ      *OFF
075800940131     C* definisco l'ordinamento
075900950410     C                   EXSR      DEFORD
076000950410     C* richiamo la lista
076100940127     C                   MOVE      'S1'          $GEST
076200940223     C                   MOVE      *ON           $INZS1
076300940302     C                   MOVE      *BLANK        W1POS
076400940211     C                   ENDIF
076500940224     C*
076600940117     C                   ENDSL
076700940224     C*
076800940506     C                   ENDIF
076900950525     C*
077000940117     C                   ENDSR
077100940224     C/EJECT
077200940224     C************************************************************
077300940224     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
077400940224     C************************************************************
077500940127     C     INZD1         BEGSR
077600940318     C*
077700940318     C                   CLEAR                   FMTD1
077800950704     C                   MOVE      'D'           D1ORD
077900950707     C                   Z-ADD     1             $ORD
078000960313     c                   move      xscon         d1on1
078100960313     c                   move      xscoff        d1off1
078200950906      *
078300950906      * Impostazione indicatore 11 per gestione campo unità
078400950906     C                   if        fgo1=*on
078500950906     C                   eval      *in11=*on
078600950906     C                   else
078700950906     C                   eval      *in11=*off
078800950906     C                   endif
078900941013     C* Se pgm  richiamato con parametri forniti , li imposto nelle
079000941013     C*    parzializzazioni
079100941104     C                   SELECT
079200941213     C* il pgm è richiamato con chiave univoca per gestione POSTA
079300950606     C     $Unique       WHENEQ    *ON
079400960327    >C                   MOVE      unita060      D1unita
079500950906    >C                   MOVE      UQ1060        D1CTB
079600950906    >C                   MOVE      UQ2060        D1KCC
079700950906    >C                   MOVE      UQ3060        D1KSC
079800950817     C     Uq3060        IFGT      *loval
079900000118C1309C******dmy          MOVE      UQ4060        D1DTPAR
080000000118C1309C     *JobRun       MOVE      UQ4060        D1DTPAR
080100950817     C                   ELSE
080200950817    >C                   Z-ADD     0             D1DTPAR
080300950817     C                   ENDIF
080400950906    >C                   Z-ADD     UQ5060        D1NRPAR
080500950906    >C                   MOVE      UQ6060        D1SERPAR
080600960313    >C                   MOVE      parchiu060    D1PARCHIU
080700941104     C* il pgm è richiamato e
080800941104     C* sono stati passati dei valori di parzializzazione ma non
080900941104     C* la chiave univoca
081000950703    >C     OPZ060        WHENNE    *BLANK
081100950906    >C                   MOVE      unita060      D1unita
081200950906    >C                   MOVE      ctb060        D1ctb
081300950704    >C                   MOVE      KCC060        D1KCC
081400950704    >C                   MOVE      KSC060        D1KSC
081500950705     C     DTPAR060      IFNE      *LOVAL
081600000118C1309C******dmy          move      DTPAR060      D1DTPAR
081700000118C1309C     *JobRun       move      DTPAR060      D1DTPAR
081800950705     C                   else
081900950705     C                   MOVE      *zeros        D1DTPAR
082000950705     C                   ENDIF
082100950704    >C                   Z-ADD     NRPAR060      D1NRPAR
082200950704    >C                   MOVE      SERPAR060     D1SERPAR
082300960313    >C                   MOVE      parchiu060    D1PARCHIU
082400960313     C     DTCHIU060     IFNE      *LOVAL
082500000118C1309C******dmy          move      DTCHIU060     D1DTCHIU
082600000118C1309C     *JobRun       move      DTCHIU060     D1DTCHIU
082700960313     C                   else
082800960313     C                   MOVE      *zeros        D1DTCHIU
082900960313     C                   ENDIF
083000950403     C* imposto i valori passati sui campi a video
083100941104     C* il pgm non è richiamato
083200950703    >C     OPZ060        WHENEQ    *BLANK
083300950403     C* imposto dei valori di default
083400950906     C                   MOVE      xscdco        d1ctb
083500950906     C                   MOVE      xscdun        d1unita
083600941115     C                   ENDSL
083700940224     C*
083800940117     C                   ENDSR
083900940506     C/EJECT
084000940506     C************************************************************
084100940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
084200940609     C* INPUT
084300941205     C* $FC1 = Abilitazioni videata D1
084400940509     C* OUTPUT
084500940509     C* *IN97 *IN99 Errori
084600940506     C************************************************************
084700940506     C     CTRKD1        BEGSR
084800941205     C*
084900940506     C                   SETOFF                                       99
085000940506     C                   MOVEA     $FC1          $FC
085100940506     C                   EXSR      CTRKEY
085200940506     C     *IN99         IFEQ      *ON
085300940506     C                   SETON                                        97
085400940506     C                   ENDIF
085500941205     C*
085600940506     C                   ENDSR
085700940916     C/EJECT
085800940916     C************************************************************
085900940916     C* GESTIONE F02 VIDEO D1
086000940916     C************************************************************
086100940916     C     F02D1         BEGSR
086200940916     C*
086300940916     C* per decodificare la videata richiamo la routine dei ctrl
086400940916     C* e poi disattivo gli eventuali messaggi di errore
086500940916     C                   EXSR      CTRD1
086600940916     C*
086700940916     C                   MOVE      *ALL'0'       IN5098
086800940916     C                   MOVEA     IN5098        *IN(50)
086900940916     C*
087000940916     C                   ENDSR
087100940224     C/EJECT
087200940131     C************************************************************
087300940307     C* GESTIONE F03 VIDEO D1
087400940131     C************************************************************
087500940307     C     F03D1         BEGSR
087600940131     C*
087700940307     C                   MOVE      *ON           $FINE
087800950703    >C                   MOVE      '1'           RET060
087900940325     C* fine programma
088000940325     C                   EXSR      ENDPGM
088100940307     C*
088200940307     C                   ENDSR
088300940916     C/EJECT
088400940916     C************************************************************
088500940916     C* GESTIONE F05 VIDEO D1
088600940916     C************************************************************
088700940916     C     F05D1         BEGSR
088800940916     C*
088900940916     C                   MOVE      *ON           $INZD1
089000940916     C*
089100940916     C                   ENDSR
089200941206     C/EJECT
089300941206     C************************************************************
089400941206     C* GESTIONE F08 VIDEO D1
089500941206     C************************************************************
089600941206     C     F08D1         BEGSR
089700941206     C*
089800941206     C                   MOVE      *ON           $FINE
089900950703    >C                   MOVE      '0'           RET060
090000941206     C*
090100941206     C                   ENDSR
090200950301     C/EJECT
090300950301     C************************************************************
090400950301     C* GESTIONE F10 VIDEO D1
090500950301     C************************************************************
090600950301     C     F10D1         BEGSR
090700950301     C*
090800950301     C*
090900950301     C                   ENDSR
091000940307     C/EJECT
091100940307     C************************************************************
091200940307     C* GESTIONE F12 VIDEO D1
091300940307     C************************************************************
091400940307     C     F12D1         BEGSR
091500940307     C*
091600940307     C*  se la 1a videata è la lista oppure sono arrivato qui con F17,
091700940307     C*  per F12 torno alla lista
091800940307     C     $LOP          IFEQ      2
091900940307     C     WINKR         OREQ      *ON
092000940307     C                   MOVE      'S1'          $GEST
092100940307     C*  altrimenti, per F12 torno a menu
092200940307     C                   ELSE
092300940307     C                   MOVE      *ON           $FINE
092400950703    >C                   MOVE      '2'           RET060
092500941027     C                   ENDIF
092600940307     C*
092700940307     C                   ENDSR
092800940307     C/EJECT
092900940307     C************************************************************
093000940307     C* GESTIONE F18 VIDEO D1
093100940307     C************************************************************
093200940307     C     F18D1         BEGSR
093300940307     C*
093400940307     C                   MOVE      'D2'          $GEST
093500940307     C                   MOVEL     *ON           $INZD2
093600940307     C*
093700940307     C                   ENDSR
093800940307     C/EJECT
093900940307     C************************************************************
094000940307     C* GESTIONE F24 VIDEO D1
094100940609     C* input
094200940509     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
094300940509     C* $FX1 =Testi tasti funzione
094400940509     C* $FC1 = Abilitazioni
094500940509     C* $FR1 = Inversioni di fondo
094600940307     C************************************************************
094700940307     C     F24D1         BEGSR
094800940506     C*
094900940506     C                   MOVEA     $FX1          $FX
095000940506     C                   MOVEA     $FC1          $FC
095100940506     C                   MOVEA     $FR1          $FR
095200940506     C                   Z-ADD     $ULKD1        $ULK
095300941220    >C                   Z-ADD     79            $L01
095400941220    >C                   Z-ADD     79            $L02
095500940506     C                   EXSR      RIEKEY
095600940506     C                   Z-ADD     $ULK          $ULKD1            3 0
095700940720     C                   MOVEA     $FC           $FC1
095800940506     C                   MOVEL     $KEY1         Z1KE1
095900060224C1913C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
096000940506     C                   MOVEL     $KEY2         Z1KE2
096100940506     C                   WRITE     FMTZ1
096200940506     C*
096300940307     C*
096400940307     C                   ENDSR
096500940307     C/EJECT
096600940307     C************************************************************
096700940307     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
096800950613     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
096900950613     C* 52 54 96 97 98
097000940307     C************************************************************
097100940307     C     CTRD1         BEGSR
097200940307     C*
097300940127     C                   SETOFF                                       99
097400940127     C* non imposto pos. del cursore
097500940127     C                   Z-ADD     0             H1RIGA
097600940127     C                   Z-ADD     0             H1COLO
097700950906     c*Unita
097800950912     C     FGO1          ifeq      *on
097900950906     C                   MOVE      *BLANKS       D1Unitad
098000950906     C     D1Unita       IFNE      *BLANKS
098100950906     C                   MOVE      XSCsoc        unisocieta
098200950906     C                   MOVE      D1unita       uniunita
098300950906     C     K02UNI01      chain     anuni01l                           21
098400950906     C     *in21         IFEQ      *off
098500950906     C                   MOVEL     unidesbrev    D1unitad
098600950912     C                   ELSE
098700950912     C                   SETON                                        5699
098800950912     C                   END
098900950912     C                   endif
099000950906     C*
099100950906     C* controllo autorizzazione
099200950906     C*
099300950906     C                   RESET                   XAutUniDS
099400950906     C                   EVAL      XAUPrf = KNmUs
099500950906     C                   EVAL      XAUSoc = XscSoc
099600950906     C                   EVAL      XAUUni = D1Unita
099700950906     C                   CALL      'XAUTUNI'
099800950906     C                   PARM                    XAutUniDS
099900950906     C                   IF        XAUAbl = *off
100000950906     C                   SETON                                        5799
100100950906     C                   ENDIF
100200950906     C                   END
100300950906     C*
100400950906     c* Codice contabilità
100500950906     C                   MOVE      *BLANKS       D1CTBD
100600950906     C                   MOVE      XCACUT        XTBUTE
100700950906     C                   MOVE      '1'           XTBRIC
100800950906     C                   MOVE      XSCSOC        XTBAZI
100900950906     C                   MOVE      *ZERO         XTBKEY
101000950906     C                   MOVE      'G84'         XTBCOD
101100950906     C                   MOVE      D1CTB         XTBKEY
101200961011     C                   CALLb     'XATB'
101300950906     C                   PARM                    XATBDS
101400950906     C     XTBERR        IFEQ      *ON
101500950906     C                   SETON                                        5499
101600950906     C                   ELSE
101700950906     C                   MOVEL     XTBUNI        TABG84
101800951103     C                   MOVEL     DSBG84        D1CTBD
101900950906     C*
102000950906     C* controllo se abilitato alla contabilità
102100950906     C*
102200950906     C                   MOVE      KNMUS         XACPRF
102300950906     C                   MOVE      XSCSOC        XACSOC
102400950906     C                   MOVE      D1CTB         XACCTB
102500950906     C                   MOVE      *BLANK        XACABL
102600950906     C                   CALL      'XAUTCTB'
102700950906     C                   PARM                    XCTBDS
102800950906     C*
102900950906     C     XACABL        IFEQ      *OFF
103000950906    >C                   SETON                                        5599
103100950906     C                   ENDIF
103200950906     C                   ENDIF
103300950704     C*
103400950704     C* Controllo del conto ( Deve essere preente anche il sottoconto)
103500950704     C*
103600950704     C                   CLEAR                   D1KSCD
103700950704     C     D1KCC         IFNE      'C'
103800950704     C     D1KCC         ANDNE     'F'
103900961011     C                   CALLb     'XALLINEA'
104000950704     C                   PARM                    D1KCC
104100950704     C                   PARM      6             LEN               5 0
104200950704     C                   PARM                    XSCINP
104300950704     C                   ENDIF
104400950704     C*
104500961011     C                   CALLb     'XALLINEA'
104600950704     C                   PARM                    D1KSC
104700950704     C                   PARM      8             LEN               5 0
104800950704     C                   PARM                    XSCINP
104900950704     C*
105000950704     C                   MOVE      D1KCC         $kcc
105100950704     C                   MOVE      D1KSC         $ksc
105200950704     C*
105300950704     C* reperisco capoconto se cliente/fornitore
105400950704   B1C     D1KCC         IFEQ      'C       '
105500950704     C     D1KCC         OREQ      'F       '
105600950704     C                   MOVE      XSCSOC        XCAPSOCIET        3
105700950704     C                   MOVE      D1KCC         XCAPKCC           6
105800950704     C                   MOVE      D1KSC         XCAPKSC           8
105900950704     C                   CALLB     'XCAPCLIFOR'
106000950704     C                   PARM                    XCAPSOCIET
106100950704     C                   PARM                    XCAPKCC
106200950704     C                   PARM                    XCAPKSC
106300950704     C                   MOVE      XCAPKCC       D1KCC
106400950704     C                   MOVE      XCAPKCC       $kcc
106500950704     C                   MOVE      XCAPKsC       $ksc
106600950704   E1C                   ENDIF
106700950704     C*
106800950704     C* Richiamo modulo controllo conto
106900950704     C                   EXSR      CALLMVC002
107000950704     C* Errore
107100950704   B1C     $Esito        IFEQ      *ON
107200950704     C     D1KSC         OREQ      *BLANKS
107300950704     C                   SETON                                        5099
107400950704     C                   ELSE
107500950704     C                   MOVEL     DESBREV002    D1KSCD
107600950704     C*
107700950704     C* Controllo se conto gestito a partite
107800951027     C                   if        tpgest002 = '0'
107900950704     C                   SETON                                        9951
108000950704     C                   ENDIF
108100950704     C*
108200981221     C* Definisco opzioni possibili per conti o per cli/for
108300981218     C*
108400981218     C* (cli/for)
108500950704     C                   if        snatura002 =  'C' or snatura002 ='F'
108600950704     C* 32=mix
108700950704     C                   Z-ADD     1             X
108800950704     C     32            LOOKUP    $OP1(X)                                21
108900950704     C     *IN21         IFEQ      *ON
109000950704     C                   MOVE      'S'           $FL1(X)
109100950704     C                   ENDIF
109200950704     C* 33=Ratei
109300950704     C                   Z-ADD     1             X
109400950704     C     33            LOOKUP    $OP1(X)                                21
109500950704     C     *IN21         IFEQ      *ON
109600950704     C                   MOVE      'S'           $FL1(X)
109700950704     C                   ENDIF
109800950906     C* 60=Blocca
109900950911     C     OPZ060        ifne      '05'
110000950911     C*
110100950906     C                   Z-ADD     1             X
110200950906     C     60            LOOKUP    $OP1(X)                                21
110300950906     C     *IN21         IFEQ      *ON
110400950906     C                   MOVE      'S'           $FL1(X)
110500950906     C                   ENDIF
110600950906     C* 61=Sblocca
110700950906     C                   Z-ADD     1             X
110800950906     C     61            LOOKUP    $OP1(X)                                21
110900950906     C     *IN21         IFEQ      *ON
111000950906     C                   MOVE      'S'           $FL1(X)
111100950906     C                   ENDIF
111200950911     C                   ELSE
111300950911     C                   Z-ADD     1             X
111400950911     C     60            LOOKUP    $OP1(X)                                21
111500950911     C     *IN21         IFEQ      *ON
111600950911     C                   MOVE      'N'           $FL1(X)
111700950911     C                   ENDIF
111800950911     C* 61=Sblocca
111900950911     C                   Z-ADD     1             X
112000950911     C     61            LOOKUP    $OP1(X)                                21
112100950911     C     *IN21         IFEQ      *ON
112200950911     C                   MOVE      'N'           $FL1(X)
112300950911     C                   ENDIF
112400950911     C*
112500950911     C                   ENDIF
112600950704     C*
112700981218     C* (conti)
112800950704     C                   ELSE
112900950704     C* 32=mix
113000950704     C                   Z-ADD     1             X
113100950704     C     32            LOOKUP    $OP1(X)                                21
113200950704     C     *IN21         IFEQ      *ON
113300950704     C                   MOVE      'N'           $FL1(X)
113400950704     C                   ENDIF
113500950704     C* 33=Ratei
113600950704     C                   Z-ADD     1             X
113700950704     C     33            LOOKUP    $OP1(X)                                21
113800950704     C     *IN21         IFEQ      *ON
113900950704     C                   MOVE      'N'           $FL1(X)
114000950704     C                   ENDIF
114100950906     C* 60=Blocca
114200950906     C                   Z-ADD     1             X
114300950906     C     60            LOOKUP    $OP1(X)                                21
114400950906     C     *IN21         IFEQ      *ON
114500950906     C                   MOVE      'N'           $FL1(X)
114600950906     C                   ENDIF
114700950906     C* 61=Sblocca
114800950906     C                   Z-ADD     1             X
114900950906     C     61            LOOKUP    $OP1(X)                                21
115000950906     C     *IN21         IFEQ      *ON
115100950906     C                   MOVE      'N'           $FL1(X)
115200950906     C                   ENDIF
115300950704     c*
115400950704   E1C                   ENDIF
115500981218     C*
115600981218     C* 34=Estensione nominativi partite
115700981218     C                   if        tecnico002 = '2'
115800981218     C                   Z-ADD     1             X
115900981218     C     34            LOOKUP    $OP1(X)                                21
116000981218     C     *IN21         IFEQ      *ON
116100981218     C                   MOVE      'S'           $FL1(X)
116200981218     C                   ENDIF
116201170223R027 C*
116202170223 "   C                   Z-ADD     1             X
116203170223 "   C     22            LOOKUP    $OP1(X)                                21
116204170223 "   C     *IN21         IFEQ      *ON
116205170223 "   C                   MOVE      'S'           $FL1(X)
116206170223 "   C                   ENDIF
116207170223R027 C*
116300981218     C                   else
116400981218     C                   Z-ADD     1             X
116500981218     C     34            LOOKUP    $OP1(X)                                21
116600981218     C     *IN21         IFEQ      *ON
116700981218     C                   MOVE      'N'           $FL1(X)
116800981218     C                   ENDIF
116801170223R027 C*
116802170223 "   C                   Z-ADD     1             X
116803170223 "   C     22            LOOKUP    $OP1(X)                                21
116804170223 "   C     *IN21         IFEQ      *ON
116805170223 "   C                   MOVE      'N'           $FL1(X)
116806170223 "   C                   ENDIF
116807170223R027 C*
116900981218     C                   endif
117000981218     C*
117100950704   E1C                   ENDIF
117200950714     C*
117300950714     C* Controllo data
117400950714     C     D1DTPAR       IFNE      0
117500000118C1309C******dmy          TEST(D)                 d1dtpar                21
117600000118C1309C     *JobRun       TEST(D)                 d1dtpar                21
117700950714     C     *IN21         IFeq      *ON
117800950714     C                   SETON                                        5299
117900950714     C                   Endif
118000950714     C                   Endif
118100950704     C*
118200950704     C* Controllo ordinamento
118300950704     C                   MOVE      *BLANK        D1ORDD
118400950704     C                   RESET                   X07DS
118500950704     C                   MOVE      '1'           X07RIC
118600950704     C                   MOVEL     'ANDIZ000'    X07TRC
118700950704     C                   MOVEL     'DSPORD'      X07CAM
118800950704     C                   MOVEL     D1ORD         X07VAL
118900950704     C                   MOVE      *OFF          X07ALL
119000961011     C                   CALLb     'X07VALR'
119100950704     C                   PARM                    X07DS
119200950704   B3C     X07ERR        IFEQ      '0'
119300950704     C                   MOVEL     X07DES        D1ORDD
119400950704   X3C                   ELSE
119500950704     C                   SETON                                        9953
119600950704   E3C                   ENDIF
119700960313     C*
119800960313     C* Controllo flag partite chiuse
119900960313     C                   IF        d1parchiu <> xscon and
120000960313     c                             d1parchiu <> xscoff and
120100960313     c                             d1parchiu <> *blanks
120200960313     c                   seton                                        5899
120300960313     c                   endif
120400960313     C*
120500960313     C* Controllo data chiusura
120600960313     c                   move      *hival        d1dtchiuR
120700960313     C     D1DTCHIU      IFNE      0
120800000118C1309C******dmy          TEST(D)                 d1dtchiu               21
120900000118C1309C     *JobRun       TEST(D)                 d1dtchiu               21
121000960313     C     *IN21         IFeq      *ON
121100960313     C                   SETON                                        5999
121200960313     c                   else
121300000118C1309c******dmy          move      d1dtchiu      d1dtchiuR
121400000118C1309c     *JobRun       move      d1dtchiu      d1dtchiuR
121500960313     C                   Endif
121600960313     C                   Endif
121700950704     C*
121800940117     C                   ENDSR
121900950704     C/EJECT
122000950704     C*----------------------------------------------------------
122100950704     C* Chiamata NDMVC002
122200950704     C*----------------------------------------------------------
122300950704     C     CallMvc002    BEGSR
122400950704     C*
122500950704     C* Controllo conto
122600950704     C                   Clear                   ND002DS
122700950704     C                   EVAL      $LenOut = %Size(ND002DS)
122800950704     C                   CallB     'NDMVC002'
122900950704     C                   PARM                    xscsoc
123000950704     C                   PARM                    $kcc
123100950704     C                   PARM                    $ksc
123200950704     C                   PARM                    Wdate
123300950704     C                   PARM      *OFF          $GesMsg
123400950704     C                   PARM      *OFF          $Esito
123500950704     C                   PARM      'ND002DS'     $Struttura
123600950704     C                   PARM                    ND002DS
123700950704     C                   PARM                    $LenOut
123800950704     C*
123900950704     C                   ENDSR
124000950410     C/EJECT
124100950410     C************************************************************
124200950410     C* DEFINIZIONE DELL'ORDINAMENTO
124300950410     C************************************************************
124400950410     C     DEFORD        BEGSR
124500950410     C*
124600950410     C* definisco l'ordinamento della lista a seconda dei valori
124700950410     C* impostati nel video di parzializzazione
124800950410     C*
124900950410     C                   SELECT
125000950704    >C     D1ORD         WHENEQ    'D'
125100950410     C                   Z-ADD     1             $ORD
125200950704    >C     D1ORD         WHENeq    'A'
125300950410     C                   Z-ADD     2             $ORD
125400950410     C* nel caso non sia stato digitato alcun valore in parzializzazione
125500950410     C* utilizzerò l'ordinamento precedentemente memorizzato
125600950410     C* (temporaneamente o definitivamente) nei valori assunti
125700950410     C                   OTHER
125800950410     C                   ENDSL
125900950717     C*
126000950717     C* imposto l'ordinamento 99 per ottenere in selezione solo il
126100950717     C* rcd con la chiave passata
126200950717     C     $Unique       IFEQ      *ON
126300950717     C                   Z-ADD     99            $ORD
126400950717     C                   endif
126500950717     C*
126600950410     C                   ENDSR
126700940224     C/EJECT
126800940127     C************************************************************
126900940131     C* GESTIONE LISTA
127000940127     C************************************************************
127100940127     C     GESS1         BEGSR
127200940223     C* inizializzazione videata
127300940223     C     $INZS1        IFEQ      *ON
127400940127     C                   EXSR      INZS1
127500940223     C                   MOVE      *OFF          $INZS1
127600940127     C                   ENDIF
127700940223     C* emissione piede videata se proveniente da altra
127800940223     C* salvataggio valore formato di provenienza
127900940202     C     $LASTV        IFNE      'S1'
128000940202     C                   WRITE     FMTT1
128100940224     C                   EXSR      GESPIE
128200940202     C                   MOVE      $LASTV        $LASTG
128300940202     C                   MOVE      'S1'          $LASTV
128400941027     C                   ENDIF
128500940223     C*
128600940223     C     WMAX          IFEQ      0
128700940211     C                   WRITE     FMTD3
128800940211     C                   ENDIF
128900980720     C* Inizializzazione indice tasti funzione personalizzati
129000980720     C                   Eval      PosFun = 25
129100980720     C*
129200960313     C                   Eval      *in09=c1mode
129300950606     C*Serve per emissione finestra pgm invio ANA727r (per open/close
129400940831     C*window)
129500940831     C                   MOVE      '1'           WIN727            1
129600940127     C*
129700940131     C                   EXFMT     FMTC1
129800940204     C     C1NRR         IFNE      0
129900940204     C                   Z-ADD     C1NRR         WSFL
130000940204     C                   ENDIF
130100951109     C*
130200951109     C     SFLNRR        IFNE      0
130300951109     C                   Z-ADD     SFLNRR        C1RCD
130400951109     C                   Z-ADD     SFLNRR        SFLNRRSAV
130500951109     C                   ELSE
130600951109     C                   Z-ADD     SFLNRRSAV     C1RCD
130700951109     C                   ENDIF
130800940506     C*
130900940506     C* controllo abilitazione tasti funzione
131000940506     C                   EXSR      CTRKS1
131100941205     C*
131200940506     C     *IN99         IFEQ      *OFF
131300940127     C*
131400941027     C                   SELECT
131500940127     C* F3=Fine
131600940506     C     $TASTO        WHENEQ    F03
131700940309     C                   EXSR      F03S1
131800941206     C* F8=Successivo
131900941206     C     $TASTO        WHENEQ    F08
132000941206     C                   EXSR      F08S1
132100940131     C* F10=Immissione
132200940506     C     $TASTO        WHENEQ    F10
132300940309     C                   EXSR      F10S1
132400940131     C* F12=Ritorno
132500940506     C     $TASTO        WHENEQ    F12
132600960123     C                   EXSR      F12S1
132700960123     C* F15=Interrogazioni
132800960123     C     $TASTO        WHENEQ    F15
132900960123     C                   EXSR      F15S1
133000940131     C* F17=Parzializzazioni
133100940506     C     $TASTO        WHENEQ    F17
133200940309     C                   EXSR      F17S1
133300940131     C* F18=Modifica valori assunti
133400940506     C     $TASTO        WHENEQ    F18
133500940309     C                   EXSR      F18S1
133600940224     C* F23=Altre opzioni
133700940506     C     $TASTO        WHENEQ    F23
133800940309     C                   EXSR      F23S1
133900940224     C* F24=Altri tasti
134000940506     C     $TASTO        WHENEQ    F24
134100940309     C                   EXSR      F24S1
134200940127     C* ROLL-UP
134300940506     C     $TASTO        WHENEQ    ROLLUP
134400940127     C                   EXSR      ROLS1
134500980720     C*
134600980720     C* Personalizzata
134700980720     C     $PGF2(PosFun) Whenne    *BLANK
134800980720     C                   EXSR      FperS1
134900940127     C*
135000941027     C                   OTHER
135100940127     C* CONTROLLO DATI
135200940131     C                   EXSR      CTRC1
135300940201     C     *IN99         IFEQ      *OFF
135400940131     C                   EXSR      CTRS1
135500941027     C                   ENDIF
135600941205     C*
135700941027     C                   ENDSL
135800940127     C*
135900940506     C                   ENDIF
136000941205     C*
136100940127     C                   ENDSR
136200940224     C/EJECT
136300980720     C************************************************************
136400980720     C* Chiamata programma con tasto personalizzato
136500980720     C************************************************************
136600980720     C     FPerS1        BEGSR
136700980720     C*
136800980720     C* salvo la DS di ingresso
136900980720    >C                   MOVEL     NDC060DS      $SaveDS
137000980720     C*
137100980720    >C                   MOVEL     NDC060DS      KPJBU
137200980720    >C                   CALL      $PGF2(PosFun)                        22
137300980720     C                   PARM                    KPJBA
137400980720     C                   PARM                    NOMPGM
137500980720     C*
137600980720     C* ripristino la DS di ingresso
137700980720    >C                   MOVEL     $SaveDS       NDC060DS
137800980720     C*
137900980720     C                   ENDSR
138000980720     C/EJECT
138100940127     C************************************************************
138200940131     C* INIZIALIZZAZIONE LISTA
138300940127     C************************************************************
138400940127     C     INZS1         BEGSR
138500940302     C* pulizia SFL
138600940128     C                   SETOFF                                         3031
138700940131     C                   WRITE     FMTC1
138800940128     C                   SETON                                          31
138900940127     C*
139000940128     C* riempimento opzioni
139100940223     C                   MOVEA     $TX1          $TX
139200940223     C                   MOVEA     $FL1          $FL
139300940309     C                   Z-ADD     0             WCOZ
139400940225     C*  equivale al num.di elementi della schiera $TX1
139500980720     C                   Z-ADD     OPZLEN        WMXO
139600941220    >C                   Z-ADD     76            $L01
139700941220    >C                   Z-ADD     76            $L02
139800940128     C                   EXSR      RIEOPZ
139900941214     C                   Z-ADD     23            X
140000941214     C                   MOVE      FF23          $FC2(X)
140100940608     C                   MOVEL     WOPZ1         C1OPZ1
140200950703     C**                 MOVEL     WOPZ2         C1OPZ2
140300060224C1913C                   MOVEL     $ALLOPT       H1ALLOPT
140400940225     C                   Z-ADD     WCOZ          C1COZ
140500950704     C* sottoconto control
140600950906     C                   EVAL      C1unita = d1unita
140700950906     C                   EVAL      C1ctb = d1ctb
140800950704     C                   EVAL      C1KCC = d1kcc
140900950704     C                   EVAL      C1KsC = d1ksc
141000970130     c                   if        c1unita <> *blanks
141100950906     C                   EVAL      C1unitad= unidesbrev
141200970130     c                   endif
141300951103     C                   EVAL      C1ctbd= desg84
141400950704     C                   EVAL      C1DESCR = DESCR002
141500950704     C                   EVAL      C1DESAGG = DESAGG002
141600960313     c                   if        $unique    =  *on
141700960313     C* visualizzo il sfl non troncato se passata chiave unique
141800960313     C                   Eval      c1mode=*off
141900960313     c                   else
142000960313     C                   Eval      c1mode=*on
142100960313     c                   endif
142200940128     C*
142300940128     C* CARICAMENTO SFL A PAGINE
142400940128     C*
142500940201     C                   Z-ADD     0             S1NRR
142600940128     C                   Z-ADD     0             WMAX
142700940224     C*
142800940224     C* Posizionamento su file pilota
142900940302     C                   CLEAR                   C1POS
143000941214     C                   EXSR      SETANA
143100940302     C* ripristino i valori del posizionamento
143200940302     C                   MOVE      W1POS         C1POS
143300950704    >C* aggiungere gestione altri campi posizionamento se ci sono
143400940131     C*
143500940201     C* Lettura file pilota
143600941214     C                   EXSR      REDANA
143700940131     C*
143800940131     C* Carico 1a pagina
143900940127     C                   EXSR      ROLS1
144000940201     C                   Z-ADD     SFLPAG        WPAG
144100940127     C*
144200940127     C                   ENDSR
144300940127     C/EJECT
144400940127     C************************************************************
144500940131     C* CARICAMENTO PAGINA LISTA
144600940127     C************************************************************
144700940127     C     ROLS1         BEGSR
144800940127     C*
144900940128     C                   SETOFF                                       32
145000940223     C                   Z-ADD     0             Y
145100940127     C                   Z-ADD     WMAX          S1NRR
145200940127     C*
145300940127     C* Leggo dal file anagrafico per caricare la lista
145400940127     C*
145500950606     C     $EndFile      DOWEQ     *OFF
145600940201     C     Y             ANDLT     WPAG
145700940127     C*
145800940131     C                   EXSR      RIES1
145900940127     C*
146000940127     C                   ADD       1             S1NRR
146100940127     C                   ADD       1             Y
146200940127     C*
146300940131     C                   WRITE     FMTS1
146400940131     C*
146500941214     C                   EXSR      REDANA
146600940128     C*
146700941027     C                   ENDDO
146800940127     C*
146900940223     C                   Z-ADD     S1NRR         WMAX                 30
147000940127     C*
147100940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
147200940127     C*
147300940201     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
147400940127     C                   MVR                     RESTO             3 0
147500940131     C     PAGINE        MULT      SFLPAG        C1RCD
147600941027     C     RESTO         IFGT      0
147700940127     C                   ADD       1             C1RCD
147800941027     C                   ELSE
147900940407     C                   SUB       SFLPAG        C1RCD
148000940407     C                   ADD       1             C1RCD
148100941027     C                   ENDIF
148200940128     C*
148300940127     C                   ENDSR
148400940224     C/EJECT
148500940224     C************************************************************
148600940224     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
148700940224     C************************************************************
148800941214     C     SETANA        BEGSR
148900940224     C*
149000950704     C                   MOVE      XSCSOC        PPASOCIETA
149100950906     C                   MOVE      D1ctb         PPActb
149200950704     C                   MOVE      D1KCC         PPAKCC
149300950704     C                   MOVE      D1KSC         PPAKSC
149400950704     C*
149500940224     C                   SELECT
149600941214     C*
149700941214     C* Gestione posizionamento
149800950301     C* Se il campo W1POS è > *blank imposto il posizionamento con
149900950301     C* W1POS altrimenti lo imposto con il valore di parametrizzazione
150000941214     C*
150100950704    >C* Ord.1 = chiave ordinamento 1 (discendente)
150200940224     C     $ORD          WHENEQ    1
150300950705     C                   MOVE      *HIVAL        PPANRPAR
150400950705     C                   MOVE      *hival        PPASERPAR
150500950705     C*
150600950705   B1C     W1POS         IFNE      0
150700000118C1309C******dmy          MOVE      W1POS         ppadtpar
150800000118C1309C     *JobRun       MOVE      W1POS         ppadtpar
150900940302     C                   ELSE
151000950705   B2C     D1DTPAR       IFEQ      0
151100950704    >C                   MOVE      *hival        ppadtpar
151200950704     C                   ELSE
151300000118C1309C******DMY          MOVEL     D1DTPAR       ppadtpar
151400000118C1309C     *JobRun       MOVEL     D1DTPAR       ppadtpar
151500950705   B3C     D1NRPAR       ifne      0
151600950704     C                   MOVE      D1NRPAR       PPANRPAR
151700950705   E3C                   endif
151800950705   B3C     D1SERPAR      ifne      *blanks
151900950704     C                   MOVE      D1SERPAR      PPASERPAR
152000950705   E3C                   endif
152100950705   E2C                   ENDIF
152200950705   E1C                   ENDIF
152300960313     c                   if        d1parchiu = xscoff
152400960313    >C     K07PPA01      SETgt     NDPPA03
152500960313     c                   else
152600951214    >C     K07PPA01      SETgt     NDPPA01
152700960313     c                   endif
152800950704    >C* Ord.2 = chiave ordinamento 2 (ascendente)
152900940224     C     $ORD          WHENEQ    2
153000950705     C                   CLEAR                   PPANRPAR
153100950705     C                   CLEAR                   PPASERPAR
153200950705   B1C     W1POS         IFNE      0
153300000118C1309C******dmy          MOVE      W1POS         ppadtpar
153400000118C1309C     *JobRun       MOVE      W1POS         ppadtpar
153500940302     C                   ELSE
153600950705   B2C     D1DTPAR       IFEQ      0
153700950704    >C                   MOVE      *loval        ppadtpar
153800950704     C                   ELSE
153900000118C1309C******DMY          MOVEL     D1DTPAR       ppadtpar
154000000118C1309C     *JobRun       MOVEL     D1DTPAR       ppadtpar
154100950705   B3C     D1nrpar       ifne      *ZEROS
154200950704     C                   MOVE      D1NRPAR       PPANRPAR
154300950705   E3C                   ENDIF
154400950705   B3C     D1SERPAR      ifne      *BLANKS
154500950704     C                   MOVE      D1SERPAR      PPASERPAR
154600950705   E3C                   ENDIF
154700950705   E2C                   ENDIF
154800950705   E1C                   ENDIF
154900960313     c                   if        d1parchiu = xscoff
155000960313    >C     K07PPA01      SETLL     NDPPA03
155100960313     c                   else
155200950906    >C     K07PPA01      SETLL     NDPPA01
155300960313     c                   endif
155400940831     C* Ord.99= Chiave univoca
155500941104     C* (quando viene gestita la chiave univoca NON deve essere gestito
155600941104     C*  il posizionamento C1POS/W1POS)
155700940831     C     $ORD          WHENEQ    99
155800950906    >C                   MOVEL     D1ctb         PPActb
155900950704    >C                   MOVEL     D1KCC         PPAKCC
156000950704    >C                   MOVEL     D1KSC         PPAKSC
156100950817     C     D1DtPar       IFGT      0
156200000118C1309C******dmy          MOVEL     D1dtpar       PPAdtpar
156300000118C1309C     *JobRun       MOVEL     D1dtpar       PPAdtpar
156400950817     C                   ELSE
156500950817     C                   MOVE      *loval        PPADtPar
156600950817     C                   ENDIF
156700950704    >C                   MOVEL     D1nrpar       PPAnrpar
156800950704    >C                   MOVEL     D1serpar      PPAserpar
156900960313     c                   if        d1parchiu = xscoff
157000960313    >C     K07ppa01      SETLL     ndppa03
157100960313     c                   else
157200960313    >C     K07ppa01      SETLL     ndppa01
157300960313     c                   endif
157400940224     C                   ENDSL
157500940224     C*
157600940224     C                   ENDSR
157700940128     C/EJECT
157800940128     C************************************************************
157900940131     C* LETTURA RCD ARCHIVIO PILOTA
158000940128     C************************************************************
158100941214     C     REDANA        BEGSR
158200940128     C*
158300950606     C                   MOVEL     *OFF          $EndFile
158400940131     C                   MOVEL     *OFF          $RCDOK
158500940131     C*
158600950606     C     $EndFile      DOUEQ     *ON
158700940131     C     $RCDOK        OREQ      *ON
158800940131     C*
158900941027     C                   SELECT
159000940131     C     $ORD          WHENEQ    1
159100960313     c                   if        d1parchiu = xscoff
159200960313    >C     K04PPA01      READpe    NDPPA03                                21
159300960313     c                   else
159400951214    >C     K04PPA01      READpe    NDPPA01                                21
159500960313     c                   endif
159600940131     C     $ORD          WHENEQ    2
159700960313     c                   if        d1parchiu = xscoff
159800960313    >C     K04PPA01      READE     NDPPA03                                21
159900960313     c                   else
160000960313    >C     K04PPA01      READE     NDPPA01                                21
160100960313     c                   endif
160200940831     C     $ORD          WHENEQ    99
160300950906    >C     K07PPA01      READE     NDPPA01                                21
160400941027     C                   ENDSL
160500940131     C*
160600950606     C                   MOVE      *IN21         $EndFile
160700940131     C*
160800950606     C     $EndFile      IFEQ      *OFF
160900941214     C                   EXSR      SELANA
161000941027     C                   ENDIF
161100940131     C*
161200941027     C                   ENDDO
161300940131     C*
161400940131     C                   MOVE      *IN21         *IN33
161500940131     C*
161600940131     C                   ENDSR
161700940224     C/EJECT
161800940224     C************************************************************
161900940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
162000940224     C************************************************************
162100940224     C     CLCPAG        BEGSR
162200940224     C* Input :
162300940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
162400940224     C*          cursore
162500940224     C* - SFLPAG = numero rcd per pagina sfl
162600940224     C* Output :
162700940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
162800940224     C*
162900940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
163000940224     C                   MVR                     RESTO             3 0
163100940224     C     RESTO         IFGT      0
163200940224     C                   ADD       1             PAGINE
163300940224     C                   ENDIF
163400940224     C     PAGINE        MULT      SFLPAG        WPAG
163500940224     C*
163600940224     C                   ENDSR
163700940131     C/EJECT
163800940131     C************************************************************
163900940131     C* SELEZIONI SULLA RIGA LETTA
164000940314     C* impostare a *OFF il campo $RCDOK per scartare un record che
164100940314     C* non rientra nei parametri di parzializzazione
164200940314     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
164300940314     C* superiore stabilito nei parametri
164400940314     C*
164500940131     C************************************************************
164600941214     C     SELANA        BEGSR
164700940131     C*
164800940202     C                   MOVEL     *ON           $RCDOK
164900940131A1211X*                  MOVEL     *ALL'9'       ALL9             15
165000940131A1211C                   MOVEL     *hival        ALL9             15
165100950301     C* creo limite massimo parzializzazioni :
165200950301     C* per ogni valore alfanumerico creo il limite superiore
165300950301     C* per cui caricare, creando una variabile che contiene
165400950301     C* il valore digitato nel campo a video riempito di "9"
165500940314     C*
165600950314     C*
165700950704     C***                SELECT
165800940314     C*
165900940314     C* SELEZIONI RISPETTO AI VALORI RCD
166000940314     C*
166100950314     C* Scartare i record che non rispondono alle selezioni effettuate
166200950301     C* esempio : esclusione degli annullati.
166300950906     c*
166400950906     C* Unità diversa
166500950906     C     D1UNITA       ifne      *blanks
166600950906     C     PPAUNITA      ANDNE     D1UNITA
166700950906     C                   move      *off          $RCDok
166800950906     C                   ENDIF
166900950301     C*
167000960313     C* Partite chiuse
167100960313     c     d1parchiu     ifeq      xscon
167200960313     C     Ppaparchiu    andeq     *off
167300960313     C                   move      *off          $RCDok
167400960313     C                   ENDIF
167500960314     C* data Partite chiuse (anche per entrambe)
167600960314     c     d1parchiu     ifne      xscon
167700960314     C     Ppadtchiu     andgt     d1DtchiuR
167800960313     C                   move      *off          $RCDok
167900960313     C                   ENDIF
168000940202     C*
168100950704     C***                ENDSL
168200950503     C*
168300950503     C* FACILITAZIONE NEGLI ACCESSI
168400950503     C*
168500950503     C* nel caso che uno dei dati letti sia > del limite superiore
168600950503     C* oppure < del limite inferiore è necessario fare dei
168700950503     C* ragionamenti onde evitare altri accessi inutili;
168800950503     C* questi ragionamenti dovranno tenere conto dell'ordinamento
168900950503     C* attualmente vigente, per cui si potranno verificare i seguenti
169000950503     C* casi :
169100950504     C* 1. un campo è > del limite superiore ed è la 1a chiave della
169200950504     C*    klist dell'ordinamento vigente :
169300950503     C*    nessun altro rcd sarà valido per cui bisogna andare a fine
169400950504     C*    file mediante un *HIVAL SETLL;
169500950504     C* 2. un campo è > del limite superiore e NON è la 1a chiave della
169600950504     C*    klist dell'ordinamento vigente :
169700950504     C*    nessun rcd che verrà letto con gli stessi valori per la
169800950504     C*    precedente porzione di klist sarà valido, per cui
169900950504     C*    conviene saltarli facendo una SETGT apunto con la klist che
170000950504     C*    esclude il campo in esame;
170100950504     C* 3. un campo è < del limite inferiore e NON è la 1a chiave della
170200950504     C*    klist dell'ordinamento vigente :
170300950503     C*    bisogna arrivare al più presto al limite inferiore per cui
170400950503     C*    conviene fare una SETLL con una klist avente come ultimo
170500950503     C*    campo quello in esame, lasciando per gli altri campi gli
170600950503     C*    stessi valori letti ma sostituendo per quello in esame
170700950503     C*    il limite inferiore;
170800950503     C* non è possibile che un campo che è anche chiave primaria
170900950503     C* dell'ordinamento vigente sia < del limite inferiore perchè
171000950503     C* nella routine SETANA la setll viene appunto fatta con il
171100950503     C* limite inferiore;
171200950503     C* non si deve prendere in considerazione il caso che un campo
171300950503     C* non presente nella chiave dell'ordinamento vigente sia
171400950503     C* esterno ai limiti perchè non genererebbe migliorie negli
171500950503     C* accessi;
171600950704    >C*
171700950503     C*
171800940202     C*
171900940202     C                   ENDSR
172000940131     C/EJECT
172100940131     C************************************************************
172200940131     C* RIEMPIMENTO RIGA LISTA
172300940131     C************************************************************
172400940131     C     RIES1         BEGSR
172500940131     C*
172600940131     C                   CLEAR                   FMTS1
172700950704     C* azzero le memorizzazioni degli attributi non bottoni
172800950704     C                   MOVEL     *ZERO         mixh
172900981218     C                   MOVEL     *ZERO         cpah
173000950704     C                   MOVEL     *ZERO         ratscah
173100950704     C                   MOVEL     *ZERO         commentih
173200950301     C*
173300950301     C* riempio i campi del sfl, compresi gli hidden con i valori
173400950301     C* del rcd letto
173500950817     C     PPADtPar      IFGT      *loval
173600000118C1309C******dmy          MOVEL     ppadtpar      S1dtpar
173700000118C1309C     *JobRun       MOVEL     ppadtpar      S1dtpar
173800950817     C                   ELSE
173900950817    >C                   Z-ADD     0             S1dtpar
174000950817     C                   ENDIF
174100950704     C                   MOVE      PPANRPAR      S1NRPAR
174200950704     C                   MOVE      PPASERPAR     S1SERPAR
174300950714     C                   MOVE      PPASYS        H1SYS
174400950714     C                   MOVE      PPANRASINT    H1NRASINT
174500960313     c                   move      ppaunita      s1unita
174600950704     C*
174700950704     C                   IF        snatura002 = 'C' or snatura002 = 'F'
174800950704     C* Se cliente/ fornitore ( sottonatura = 'C o F') leggo file estensione
174900950704     C     K02PPA01      CHAIN     NDPPS01                            22
175000950704     C                   if        *in22 = *off
175100950705     C     PPSDTDOC      IFNE      *loval
175200000118C1309C******dmy          move      ppsdtdoc      s1dtdoc
175300000118C1309C     *JobRun       move      ppsdtdoc      s1dtdoc
175400950705     C                   else
175500950705     C                   eval      s1dtDOC = *zeros
175600950705     C                   endif
175700950704     C                   EVAL      S1nrDOC = PPSnrDOC
175800950704     C                   EVAL      S1serdoc = PPSserDOC
175900950906     C                   EVAL      S1bkamm  = PPSbkamm
176000960313     C                   EVAL      S1condpag= PPScondpag
176100960313     C                   EVAL      S1kscage = PPSkscage
176200080929A1270C                   MOVE      Ppsboninc     H1Boninc
176300950704     C                   endif
176400960314     C*  decodifica condizione di pagamento
176500960314     c                   if        s1condpag <> *blanks
176600960314     C                   reset                   DVCDPDS
176700960314     C                   eval      DCPRIC='1'
176800960314     C                   eval      DCPSOC=XSCSOC
176900960314     C                   eval      DCPUNI=d1unita
177000960314     C                   eval      DCPVIS='1'
177100960314     C                   eval      DCPALC=*OFF
177200960314     C                   eval      DCPNRK=1
177300960314     C                   eval      DCPLIN=xsclin
177400960314     C                   eval      DCPCOD=s1condpag
177500960314     C                   eval      DCPERR=*OFF
177600960314     C                   CALL      'DVCDP'
177700960314     C                   PARM                    DVCDPDS
177800960314     C                   PARM                    DVOCDPDS
177900960314   2 C                   if        DCPERR=*OFF
178000960314     C                   eval      s1condpagd=cdpdescr
178100960314   2-C                   endif
178200960314   2-C                   endif
178300960314     c* decodifica agente
178400960314     c                   if        s1kscage  <> *blanks
178500960314     C                   RESET                   DVAGE
178600960314     C                   MOVE      '1'           DAGRIC
178700960314     C                   MOVE      xscsoc        DAGSOC
178800960314     C                   MOVE      d1unita       DAGUNI
178900960314     C                   MOVE      '1'           DAGVIS
179000960314     C                   MOVE      *OFF          DAGALC
179100960314     C                   MOVE      1             DAGNRK
179200960314     C                   CLEAR                   DAGDTA
179300960314     C                   MOVEL     s1kscage      DAGKSC
179400960314     C                   MOVE      'DVOAGEDS'    DAGSET
179500960314     C                   MOVE      *OFF          DAGERR
179600960314     C                   CALL      'DVAGE'
179700960314     C                   PARM                    DVAGE
179800960314     C                   PARM                    DVOAGE
179900960314     C*
180000960314     C     DAGERR        IFEQ      *OFF
180100960314     C                   MOVEL     AGERAGsoc     s1kscaged
180200960314     c                   endif
180300960314     c                   endif
180400950704     c*
180500950704     C* leggo file MIX di partita
180600950704     C     K02PPA01      SETLL     NDMIX01                                22
180700950704     C                   if        *in22 = *on
180800950704     C                   eval      MIXH  = *ON
180900950704     C                   endif
181000981218     c*
181100981218     C* leggo file Estensione nominativi partite
181200981218     C     K02PPA01      SETLL     NDCPA01                                22
181300981218     C                   if        *in22 = *on
181400981218     C                   eval      CPAH  = *ON
181500981218     C                   endif
181600950704     c*
181700950704     C* leggo file SCADENZE/ratei
181800950704     C     K02PPA01      SETLL     NDPSC01                                22
181900950704     C                   if        *in22 = *on
182000950704     C                   eval      RATSCAH = *ON
182100950704     C                   endif
182200950704     C*
182300960119     C                   endif
182400960123     c*
182500960123     C* partita chiusa
182600960123     c                   if        ppaparchiu = *on
182700960123     C                   eval      s1chiusa = xscon
182800960123     c                   else
182900960123     C                   eval      s1chiusa = *blanks
183000960123     c                   endif
183100950704     C
183200950704     C*
183300950704     C* reperisco se commento presente
183400950704     C                   MOVE      *BLANK        EXI724
183500950704     C                   RESET                   A724DS
183600950704     C                   MOVEL     '01'          OPZ724
183700950704     C                   MOVEL     XscSOC        SOC724
183800950704     C                   MOVEL     *BLANK        UNI724
183900950704     C                   MOVEL     'PPA'         TPA724
184000950907     C                   MOVEL     C1CTB         KY1724
184100950907     C                   MOVEL     C1KCC         KY2724
184200950907     C                   MOVE      C1KSC         KY2724
184300950907    >C                   MOVEL     S1DTPAR       KY3724
184400950907    >C                   MOVEL     s1nrpar       KY4724
184500950907    >C                   MOVE      s1serpar      KY4724
184600950704     C     *eur          MOVEL     *DATE         DTS724
184700950704     C                   MOVEL     A724DS        KPJBU
184800950704     C                   CALL      'ANA724R'
184900950704     C                   PARM                    KPJBA
185000950704     C                   MOVEL     KPJBU         A724DS
185100950704     C* memorizzo lo stato del  bottone TCM
185200950704     C                   MOVE      EXI724        COMMENTIH
185300950301     C*
185400950301     C* imposto quegli attributi del sfl che sono modificabili
185500950301     C* al richiamo del determinato pgm
185600950301     C                   EXSR      ATRS1
185700940131     C*
185800940131     C                   ENDSR
185900950301     C/EJECT
186000950301     C************************************************************
186100950301     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
186200950301     C************************************************************
186300950301     C     ATRS1         BEGSR
186400950301     C*
186500950704     C* azzero gli attributi non bottoni
186600950704     C                   MOVEL     X'A0'         MIXA
186700981218     C                   MOVEL     X'A0'         CPAA
186800950704     C                   MOVEL     X'A0'         RATSCAA
186900950704     C                   MOVEL     X'A0'         COMMENTIA
187000950704     C*
187100950704     C* li valorizzo a seconda dello stato ricevuto o testato
187200950704     C*
187300950704     C     MIXH          IFEQ      *on
187400950704     C                   MOVEL     PRESente      MIXA
187500950704     C                   ENDIF
187600981218     C*
187700981218     C     CPAH          IFEQ      *on
187800981218     C                   MOVEL     PRESente      CPAA
187900981218     C                   ENDIF
188000950704     C*
188100950704     C     RATSCAH       IFEQ      *on
188200950704     C                   MOVEL     PRESente      RATSCAA
188300950704     C                   ENDIF
188400950704     C*
188500950704     C     COMMENTIH     IFEQ      *on
188600950704     C                   MOVEL     PRESente      COMMENTIA
188700950704     C                   ENDIF
188800950704    >C* impostare le condizioni per ripristinare gli attributi
188900950704    >C* della riga del sfl (se ce ne sono)
189000950301     C*
189100950301     C                   ENDSR
189200941205     C/EJECT
189300941205     C************************************************************
189400941205     C* CONTROLLO ABILITAZIONE TASTI VIDEATA S1
189500941205     C* INPUT
189600941205     C* $FC2 = Abilitazioni videata S1
189700941205     C* OUTPUT
189800941205     C* *IN97 *IN99 Errori
189900941205     C************************************************************
190000941205     C     CTRKS1        BEGSR
190100941205     C*
190200941205     C                   SETOFF                                       99
190300941205     C                   MOVEA     $FC2          $FC
190400941205     C                   EXSR      CTRKEY
190500941205     C     *IN99         IFEQ      *ON
190600941205     C                   SETON                                        97
190700941205     C                   ENDIF
190800941205     C*
190900941205     C                   ENDSR
191000941206     C/EJECT
191100941206     C************************************************************
191200941206     C* GESTIONE F08 VIDEO S1
191300941206     C************************************************************
191400941206     C     F08S1         BEGSR
191500941206     C*
191600941206     C                   MOVE      *ON           $FINE
191700950703    >C                   MOVE      '0'           RET060
191800941206     C*
191900941206     C                   ENDSR
192000940309     C/EJECT
192100940309     C************************************************************
192200940309     C* GESTIONE F03 VIDEO S1
192300940309     C************************************************************
192400940309     C     F03S1         BEGSR
192500940309     C*
192600940309     C                   MOVE      *ON           $FINE
192700950703    >C                   MOVE      '1'           RET060
192800940325     C* fine programma
192900940325     C                   EXSR      ENDPGM
193000940309     C*
193100940309     C                   ENDSR
193200940309     C/EJECT
193300940309     C************************************************************
193400940309     C* GESTIONE F10 VIDEO S1
193500940609     C* AGGIUNTA RECORD
193600940309     C************************************************************
193700940309     C     F10S1         BEGSR
193800940309     C*
193900940627     C*
194000940309     C                   ENDSR
194100940309     C/EJECT
194200940309     C************************************************************
194300940309     C* GESTIONE F12 VIDEO S1
194400940309     C************************************************************
194500940309     C     F12S1         BEGSR
194600940309     C*
194700940309     C*  se la 1a videata sono le parzializzazioni, per F12 torno a queste
194800940309     C     $LOP          IFEQ      1
194900940309     C                   MOVE      'D1'          $GEST
195000940309     C*  se la 1a videata è questa, per F12 torno a menu
195100940309     C                   ELSE
195200940309     C                   MOVE      *ON           $FINE
195300950703    >C                   MOVE      '2'           RET060
195400941027     C                   ENDIF
195500940309     C*
195600940309     C                   ENDSR
195700960123     C/EJECT
195800960123     C************************************************************
195900960123     C* GESTIONE F15 VIDEO S1
196000960123     C************************************************************
196100960123     C     F15S1         BEGSR
196200960123     C*
196300960123     C                   reset                   ana399ds
196400960123     C                   eval      A39societa = XSCSOC
196500960123     C                   eval      A39unita = c1unita
196600960123     C                   eval      A39ctb = d1ctb
196700960123     C                   eval      A39kcc = c1kcc
196800960123     C                   eval      A39ksc = c1ksc
196900960123     C****               eval      A39SNATURA =
197000960123     C                   movel     ana399ds      kpjbu
197100960123     C                   CALL      'ANA399R'                            21
197200960123     C                   PARM                    KPJBA
197300960123     C     A39ERRORI     IFEQ      '0'
197400960123     C                   endif
197500960123     C*
197600960123     C                   ENDSR
197700940309     C/EJECT
197800940309     C************************************************************
197900940309     C* GESTIONE F17 VIDEO S1
198000940309     C************************************************************
198100940309     C     F17S1         BEGSR
198200940309     C*
198300940309     C                   MOVE      'D1'          $GEST
198400941104     C* se il pgm è stato richiamato con chiave univoca
198500941104     C* e dopo è stata richiesta una parzializzazione
198600941104     C* tolgo la gestione della chiave univoca (azzere/abblenco il
198700941104     C* valore; propongo come ordinamento il 1°)
198800950606     C     $Unique       IFEQ      *ON
198900950606     C                   MOVE      *OFF          $Unique
199000950906    >C                   MOVE      *BLANK        UQ1060
199100950906    >C                   MOVE      *BLANK        UQ2060
199200950906    >C                   MOVE      *BLANK        UQ3060
199300950906    >C                   MOVE      *loval        UQ4060
199400950906     C                   Z-ADD     0             UQ5060
199500950906    >C                   MOVE      *BLANK        UQ6060
199600941027     C                   ENDIF
199700940831     C     $ORD          IFEQ      99
199800940831     C                   Z-ADD     1             $ORD
199900941104     C                   MOVE      *ON           $INZD1
200000941027     C                   ENDIF
200100940309     C*
200200940309     C                   ENDSR
200300940309     C/EJECT
200400940309     C************************************************************
200500940309     C* GESTIONE F18 VIDEO S1
200600940309     C************************************************************
200700940309     C     F18S1         BEGSR
200800940309     C*
200900940309     C                   MOVE      'D2'          $GEST
201000940309     C                   MOVEL     *ON           $INZD2
201100940831     C     $ORD          IFEQ      99
201200940831     C                   Z-ADD     1             $ORD
201300941027     C                   ENDIF
201400940309     C*
201500940309     C                   ENDSR
201600940309     C/EJECT
201700940309     C************************************************************
201800940309     C* GESTIONE F23 VIDEO S1
201900940309     C************************************************************
202000940309     C     F23S1         BEGSR
202100940309     C*
202200941220    >C                   Z-ADD     76            $L01
202300941220    >C                   Z-ADD     76            $L02
202400940309     C                   EXSR      RIEOPZ
202500941214     C                   Z-ADD     23            X
202600941214     C                   MOVE      FF23          $FC2(X)
202700940608     C                   MOVEL     WOPZ1         C1OPZ1
202800950703     C**                 MOVEL     WOPZ2         C1OPZ2
202900060224C1913C                   MOVEL     $ALLOPT       H1ALLOPT
203000940309     C                   Z-ADD     WCOZ          C1COZ
203100940309     C*
203200940309     C                   ENDSR
203300940309     C/EJECT
203400940309     C************************************************************
203500940309     C* GESTIONE F24 VIDEO S1
203600940309     C************************************************************
203700940309     C     F24S1         BEGSR
203800940506     C*
203900940506     C                   MOVEA     $FX2          $FX
204000940506     C                   MOVEA     $FC2          $FC
204100940506     C                   MOVEA     $FR2          $FR
204200940506     C                   Z-ADD     $ULKS1        $ULK
204300941220    >C                   Z-ADD     79            $L01
204400941220    >C                   Z-ADD     79            $L02
204500940506     C                   EXSR      RIEKEY
204600940506     C                   Z-ADD     $ULK          $ULKS1            3 0
204700940720     C                   MOVEA     $FC           $FC2
204800940506     C                   MOVEL     $KEY1         Z2KE1
204900060224C1913C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
205000940506     C                   MOVEL     $KEY2         Z2KE2
205100940506     C                   WRITE     FMTZ2
205200940506     C*
205300940309     C                   ENDSR
205400940128     C/EJECT
205500940128     C************************************************************
205600940131     C* CONTROLLO TESTATA LISTA
205700940128     C************************************************************
205800940131     C     CTRC1         BEGSR
205900940128     C*
206000940201     C                   MOVE      *OFF          *IN99
206100951109     C*
206200951109     C                   if        c1pos <> 0
206300951109     C                   CALLb     'XDT4000'
206400951109     C                   PARM                    XDTAMG           10
206500951109     C                   PARM      c1pos         XDTGMA            6 0
206600951109     C                   PARM      3             XDTSTA            1 0
206700951109     C     XDTSTA        IFLT      0
206800951109     C                   SETON                                        6099
206900951109     C                   endif
207000951109     C                   endif
207100940131     C*
207200940202     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
207300950707     C* L'INIZIALIZZAZIONE LISTA   (lo attivo anche se w1pos = c1pos, per ri
207400950707     C* partire dal primo numero partita
207500950707     C     C1POS         IFNE      W1POS
207600950704    >C* aggiungere in OR altri campi posizionamento se ci sono
207700940202     C                   EXSR      POSS1
207800950707     C                   ENDIF
207900940202     C*
208000940202     C                   ENDSR
208100940202     C/EJECT
208200940202     C************************************************************
208300940202     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
208400940202     C************************************************************
208500940202     C     POSS1         BEGSR
208600940202     C*
208700940202     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
208800940202     C*
208900941118   B1C     WMAX          IFGT      0
209000940202     C                   READC     FMTS1                                  21
209100941118   B2C     *IN21         DOWEQ     *OFF
209200940202     C     *IN99         ANDEQ     *OFF
209300940202     C*
209400941118   B3C     S1OPZ         IFNE      0
209500940202     C                   SETON                                        51  99
209600941118   E3C                   ENDIF
209700940202     C* se rilevata opzione attiva, attivo sflnxtchg
209800941118   B3C     *IN99         IFEQ      *ON
209900940202     C                   MOVE      *ON           *IN32
210000950301     C*
210100950301     C* imposto quegli attributi del sfl che sono modificabili
210200950301     C* al richiamo del determinato pgm
210300941118     C                   EXSR      ATRS1
210400941118     C*
210500940202     C                   UPDATE    FMTS1
210600941118   X3C                   ELSE
210700940202     C                   READC     FMTS1                                  21
210800941118   E3C                   ENDIF
210900940202     C*
211000941118   E2C                   ENDDO
211100940202     C*
211200941118   E1C                   ENDIF
211300940202     C*
211400940221     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
211500940221     C* AL VALORE DI POSIZIONAMENTO :
211600940221     C* se questo è maggiore del valore del rispettivo campo di
211700940221     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
211800940221     C* con i valori di parzializzazione.
211900940221     C*
212000941118   B1C     *IN99         IFEQ      *OFF
212100950704    >C                   MOVE      *ZEROS        W1POS
212200940202     C*
212300950525     C* per ogni ordinamento devo testare se il campo dell'inizio elenco
212400950525     C* è maggiore del relativo valore di parzializzazione, nel qual caso
212500950525     C* imposto la variabile del posizionamento (W1Pos) al nuovo valore;
212600950525     C* richiedo comunque un ricaricamento del sfl che, se non si è
212700950525     C* verificata la condizione suddetta avverrà con posizionamento a
212800950525     C* partire dal valore di parzializzazione
212900950525     C*
213000941118   B2C                   SELECT
213100950704    >C* Attenzione : se un ordinamento ha un campo di posizionamento
213200950704    >C* specifico bisogna sostituire la gestione W1Pos/C1Pos con quella
213300950704    >C* dei relativi campi
213400940202     C*
213500940202     C     $ORD          WHENEQ    1
213600950406     C                   MOVEL     C1POS         W1POS
213700940202     C*
213800940202     C     $ORD          WHENEQ    2
213900941214     C                   MOVE      C1POS         W1POS
214000950525     C*
214100950525     C* per l'ordinamento 99 = POSTA non viene gestito il posizionamento
214200950525     C*
214300941118   E2C                   ENDSL
214400950525     C*
214500950525     C* richiedo l'inizializzazione sfl
214600950525     C                   MOVE      *ON           *IN99
214700950525     C                   MOVE      *ON           $INZS1
214800940202     C*
214900941118   E1C                   ENDIF
215000940202     C*
215100940131     C                   ENDSR
215200940131     C/EJECT
215300940131     C************************************************************
215400940131     C* CONTROLLO OPZIONI LISTA
215500940131     C************************************************************
215600940131     C     CTRS1         BEGSR
215700940131     C*
215800940202     C                   MOVEL     *OFF          $ESCI
215900940201     C                   SETOFF                                       99
216000940131     C                   Z-ADD     0             S1OPZ
216100940131     C*
216200940127     C* Leggo il sfl solo se ci sono rcd
216300941118   B1C     WMAX          IFGT      0
216400940131     C                   READC     FMTS1                                  21
216500940127     C*
216600940131     C* esce se fine sfl o errore che richiede l'uscita
216700941118   B2C     *IN21         DOWEQ     *OFF
216800940131     C     $ESCI         ANDEQ     *OFF
216900940201     C                   Z-ADD     S1NRR         C1RCD
217000940131     C* ctrl su riga
217100940131     C                   EXSR      RECS1
217200940131     C* gestione opzioni
217300941118   B3C     S1OPZ         IFNE      0
217400940201     C     *IN99         ANDEQ     *OFF
217500940131     C                   EXSR      OPZS1
217600941118   E3C                   ENDIF
217700940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
217800941118   B3C     *IN99         IFEQ      *ON
217900940131     C                   MOVE      *ON           *IN32
218000941118   E3C                   ENDIF
218100950301     C*
218200950301     C* imposto quegli attributi del sfl che sono modificabili
218300950301     C* al richiamo del determinato pgm
218400941118     C                   EXSR      ATRS1
218500941118     C*
218600940131     C                   Z-ADD     0             S1OPZ
218700940223     C*
218800940127     C                   UPDATE    FMTS1
218900941125     C                   MOVE      *OFF          *IN32
219000940223     C*
219100940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
219200941118   B3C     $ESCI         IFEQ      *OFF
219300940131     C                   READC     FMTS1                                  21
219400940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
219500941118   B4C     *IN21         IFEQ      *ON
219600940201     C                   MOVE      WINZS1        $INZS1
219700940204     C* calcolo pagina a cui deve posizionarsi
219800940728     C     WSFL          IFGT      0
219900940728     C                   EXSR      CLCPAG
220000940728     C                   ELSE
220100940728     C                   Z-ADD     SFLPAG        WPAG
220200940728     C                   ENDIF
220300941118   E4C                   ENDIF
220400941118   E4C                   ENDIF
220500940131     C*
220600941118   E2C                   ENDDO
220700940127     C*
220800941118   E1C                   ENDIF
220900940131     C*
221000940127     C                   ENDSR
221100940127     C/EJECT
221200940127     C************************************************************
221300940131     C* CONTROLLO CAMPI I/O RIGA LISTA
221400940127     C************************************************************
221500940131     C     RECS1         BEGSR
221600940131     C*
221700940201     C* reset indicatori DSPATR
221800940201     C                   MOVE      *ALL'0'       IN4049           10
221900940201     C                   MOVEA     IN4049        *IN(40)
222000940201     C*
222100940131     C* Controllo se Utente Autorizzato alla scelta
222200940202     C     S1OPZ         IFNE      0
222300940131     C                   Z-ADD     1             X
222400940223     C     S1OPZ         LOOKUP    $OP1(X)                                21
222500980720     C* mi salvo l'indice dell' opzione
222600980720     C   21              Z-add     X             PosOpz            2 0
222700940211     C     *IN21         IFEQ      *OFF
222800940223     C     $FL1(X)       OREQ      'N'
222900940131     C                   MOVE      *ON           $ESCI
223000940201     C                   SETON                                        504099
223100940131     C                   ENDIF
223200940202     C                   ENDIF
223300950706     C*
223400950706     C* Controllo se cancellazione possibile in quanto non esistono moviment
223500940131     C*
223600950706     C     S1OPZ         IFEQ      4
223700950706     C                   MOVE      PPASOCIETA    PASSOCIETA
223800950906     C                   MOVE      ppactb        PASCTB
223900950706     C                   MOVE      C1KCC         PASKCC
224000950706     C                   MOVE      C1KSC         PASKSC
224100950817     C     S1DtPar       IFGT      0
224200000118C1309C******dmy          MOVE      s1dtpar       pasdtpar
224300000118C1309C     *JobRun       MOVE      s1dtpar       pasdtpar
224400950817     C                   ELSE
224500950817    >C                   MOVE      *loval        PASDtPar
224600950817     C                   ENDIF
224700950706     C                   move      s1nrpar       pasnrpar
224800950706     C                   move      s1serpar      passerpar
224900950706     C     K07PAS02      SETLL     NDPAS02                                21
225000950706     C                   IF        *in21 =  *on
225100950706     C                   SETON                                        554099
225200950706     C                   ENDIF
225300950706     C                   ENDIF
225400990812C1221C*
225500990906  "  C* reperisco l'autorizzazione dell'utente rispetto a questa
225600990812  "  C* partita (potrebbe avere una autorizzazione specifica per
225700990812  "  C*  unità)
225800990812  "  C                   CLEAR                   AUTDS
225900990812  "  C                   EVAL      XCAUNI = S1Unita
226000990812  "  C                   EXSR      RepAut
226100990906  "  C*
226200990812  "  C* controllo l'autorizzazione per quelle opzioni che non hanno
226300990906  "  C* un richiamo ad un pgm ma vengono smaltite da questo
226400990812  "  C                   IF        *in99 = *off
226500990906  "  C* l'opzione blocco/sblocco hanno una loro autorizzazione
226600990906  "  C                   SELECT
226700990906  "  C                   WHEN      S1Opz = 60
226800990812  "  C                   SELECT
226900990812  "  C* particolarità partite clienti
227000990812  "  C                   WHEN      SNatura002 = 'C'
227100990906  "  C                   IF        BlcPrCl <> *on
227200990812  "  C                   SETON                                        564099
227300990812  "  C                   ENDIF
227400990812  "  C                   WHEN      SNatura002 = 'F'
227500990906  "  C                   IF        BlcPrFr <> *on
227600990812  "  C                   SETON                                        564099
227700990812  "  C                   ENDIF
227800990812  "  C                   ENDSL
227900990906  "  C                   WHEN      S1Opz = 61
228000990906  "  C                   SELECT
228100990906  "  C* particolarità partite clienti
228200990906  "  C                   WHEN      SNatura002 = 'C'
228300990906  "  C                   IF        SblPrCl <> *on
228400990906  "  C                   SETON                                        564099
228500990906  "  C                   ENDIF
228600990906  "  C                   WHEN      SNatura002 = 'F'
228700990906  "  C                   IF        SblPrFr <> *on
228800990906  "  C                   SETON                                        564099
228900990906  "  C                   ENDIF
229000990906  "  C                   ENDSL
229100990906  "  C                   ENDSL
229200990812  "  C*                                                                iment
229300990906  "  C   56              MOVE      *ON           $ESCI
229400990812C1221C                   ENDIF
229500990812     C*
229600080929A1270C* controllo se possibile blocco sblocco: non ci devono essere
229700080929  "  C* bonifici in corso
229800080929  "  C                   if        S1Opz = 60  or s1opz= 61
229900080929  "  c                   if        H1Boninc=*on
230000080929  "  C                   SETON                                        614099
230100080929  "  C                   MOVE      *ON           $ESCI
230200080929  "  c                   endif
230300080929A1270c                   endif
230400940131     C                   ENDSR
230500941118     C/EJECT
230600941118     C************************************************************
230700941118     C* GESTIONE OPZIONI LISTA
230800941118     C************************************************************
230900941118     C     OPZS1         BEGSR
231000950301     C*
231100950301     C                   Z-ADD     RIRI          CURR
231200950301     C                   Z-ADD     COCO          CURC
231300941118     C*
231400941027     C                   SELECT
231500940225     C*
231600950620     C* se opz=Selezione
231700950620     C* passo la chiave al pgm chiamante
231800940225     C     S1OPZ         WHENEQ    1
231900950707    >C                   MOVE      S1OPZ         OPZ060
232000950703     C* passare nei campi appositi della DS STC060DS il valore
232100950301     C* della chiave della VL univoca (se c'è)
232200950906    >C                   MOVE      C1unita       unita060
232300950906    >C                   MOVE      C1ctb         ctb060
232400950707    >C                   MOVE      C1KCC         KCC060
232500950707    >C                   MOVE      C1KSC         KSC060
232600950817     C     S1DtPar       IFGT      0
232700000118C1309C******dmy          MOVE      s1dtpar       dtpar060
232800000118C1309C     *JobRun       MOVE      s1dtpar       dtpar060
232900950817     C                   ELSE
233000950817    >C                   MOVE      *loval        dtpar060
233100950817     C                   ENDIF
233200950707     C                   MOVE      S1NRPAR       NRPAR060
233300950707     C                   MOVE      S1SERPAR      SERPAR060
233400940225     C                   MOVE      *ON           $ESCI
233500940225     C                   MOVE      *ON           $FINE
233600940909     C*
233700940909     C                   MOVE      *HIVAL        $LASTV
233800950620     C* se opz=Invia
233900950620     C* scrive nella posta
234000940831     C     S1OPZ         WHENEQ    10
234100950606     C                   RESET                   ANA727DS
234200950331     C                   MOVE      '01'          OPZ727
234300950704    >C                   MOVEL     PARTPART      TXT727
234400950704    >C                   MOVEL     'PPA'         TPA727
234500950907    >C                   MOVEL     C1CTB         KY1727
234600950907    >C                   MOVEL     C1KCC         KY2727
234700950907    >C                   MOVE      C1KSC         KY2727
234800950907    >C                   MOVEL     S1DTPAR       KY3727
234900950907    >C                   MOVEL     s1nrpar       KY4727
235000950907    >C                   MOVE      s1serpar      KY4727
235100950331     C                   MOVE      'E'           PGM727
235200950410     C                   MOVE      XSCSOC        SOC727
235300950331     C                   MOVE      WIN727        INZ727
235400950331     C                   CLEAR                   KPJBU
235500950606     C                   MOVEL     ANA727DS      KPJBU
235600950331     C                   CALL      'ANA727R'
235700950331     C                   PARM                    KPJBA
235800950606     C                   MOVEL     KPJBU         ANA727DS
235900950331     C*
236000950331     C                   EXSR      RTN727
236100950331     C*
236200950331     C                   MOVE      *HIVAL        $LASTV
236300940831     C*
236400950620     C* se opz=Commenti
236500950620     C* richiamo il pgm di gestione
236600940831     C     S1OPZ         WHENEQ    14
236700950606     C                   RESET                   ANA725DS
236800950703    >C     OPZ060        IFEQ      '04'
236900950703    >C     OPZ060        OREQ      '05'
237000950704    >C* oppure se l'utente è abilitato per la funzione specifica di
237100950704    >C* questa anagrafica solo alla visualizzazione
237200941214     C                   MOVE      '05'          OPZ725
237300941214     C                   ELSE
237400941214     C                   MOVE      '02'          OPZ725
237500941214     C                   ENDIF
237600950704    >C                   MOVE      'PPA'         TPA725
237700941117     C* inserire chiavi commenti
237800950907    >C                   MOVEL     C1CTB         KY1725
237900950907    >C                   MOVEL     C1KCC         KY2725
238000950907    >C                   MOVE      C1KSC         KY2725
238100950907    >C                   MOVEL     S1DTPAR       KY3725
238200950907    >C                   MOVEL     s1nrpar       KY4725
238300950907    >C                   MOVE      s1serpar      KY4725
238400941214     C                   MOVE      *BLANK        KPJBU
238500950606     C                   MOVEL     ANA725DS      KPJBU
238600941026     C                   CALL      'ANA725R'
238700940831     C                   PARM                    KPJBA
238800950606     C                   MOVEL     KPJBU         ANA725DS
238900941124     C*
239000941124     C* ritorno da PGM commenti
239100941124     C                   EXSR      RTN725
239200940831     C*
239300940831     C                   MOVE      *HIVAL        $LASTV
239400950707     C*
239500950707     C* se opz=Rate/scadenze
239600950707     C* richiamo il pgm di gestione
239700950707     C     S1OPZ         WHENEQ    33
239800950707     C                   RESET                   NDC063DS
239900950707    >C     OPZ060        IFEQ      '04'
240000950707    >C     OPZ060        OREQ      '05'
240100950707    >C* oppure se l'utente è abilitato per la funzione specifica di
240200950707    >C* questa anagrafica solo alla visualizzazione
240300950707     C                   MOVE      '05'          OPZ063
240400950707     C                   ELSE
240500950707     C                   MOVE      '02'          OPZ063
240600950707     C                   ENDIF
240700950907     C                   MOVE      C1ctb         ctb063
240800950707     C                   MOVE      C1KCC         KCC063
240900950707     C                   MOVE      C1KSC         KSC063
241000950817     C     S1DtPar       IFGT      0
241100000118C1309C******dmy          MOVE      S1DTPAR       dtpar063
241200000118C1309C     *JobRun       MOVE      S1DTPAR       dtpar063
241300950817     C                   ELSE
241400950817    >C                   MOVE      *loval        dtpar063
241500950817     C                   ENDIF
241600950707     C                   move      s1nrpar       nrpar063
241700950707     C                   move      s1serpar      serpar063
241800950707     C                   MOVE      *BLANK        KPJBU
241900950707     C                   MOVEL     NDC063DS      KPJBU
242000950707     C                   CALL      'NDC063R'
242100950707     C                   PARM                    KPJBA
242200950707     C                   MOVEL     KPJBU         NDC063DS
242300950707     C*
242400950707     C* ritorno da PGM commenti
242500950707     C                   EXSR      RTN063
242600950707     C*
242700950707     C                   MOVE      *HIVAL        $LASTV
242800950707     C*
242900950707     C* se opz=MIX
243000950707     C* richiamo il pgm di gestione
243100950707     C     S1OPZ         WHENEQ    32
243200950707     C                   RESET                   NDC065DS
243300950707    >C     OPZ060        IFEQ      '04'
243400950707    >C     OPZ060        OREQ      '05'
243500950707    >C* oppure se l'utente è abilitato per la funzione specifica di
243600950707    >C* questa anagrafica solo alla visualizzazione
243700950707     C                   MOVE      '05'          OPZ065
243800950707     C                   ELSE
243900950707     C                   MOVE      '02'          OPZ065
244000950707     C                   ENDIF
244100950906     C                   MOVE      C1unita       unita065
244200950906     C                   MOVE      C1ctb         ctb065
244300950707     C                   MOVE      C1KCC         KCC065
244400950707     C                   MOVE      C1KSC         KSC065
244500950817     C     S1DtPar       IFGT      0
244600000118C1309C******dmy          MOVE      S1DTPAR       dtpar065
244700000118C1309C     *JobRun       MOVE      S1DTPAR       dtpar065
244800950817     C                   ELSE
244900950817     C                   MOVE      *loval        DtPar065
245000950817     C                   ENDIF
245100950707     C                   move      s1nrpar       nrpar065
245200950707     C                   move      s1serpar      serpar065
245300950718     C*imposto chiave unique per la lista
245400950906    >C                   MOVEL     ctb065        UQ1065
245500950906    >C                   MOVEL     kcc065        UQ2065
245600950906    >C                   MOVEL     ksc065        UQ3065
245700950906     C                   move      dtpar065      UQ4065
245800950906     C                   move      nrpar065      UQ5065
245900950906     C                   move      serpar065     UQ6065
246000950707     C                   MOVE      *BLANK        KPJBU
246100950707     C                   MOVEL     NDC065DS      KPJBU
246200950707     C                   CALL      'NDC065R'
246300950707     C                   PARM                    KPJBA
246400950707     C                   MOVEL     KPJBU         NDC065DS
246500950707     C*
246600950707     C* ritorno da PGM commenti
246700950707     C                   EXSR      RTN065
246800950707     C*
246900950707     C                   MOVE      *HIVAL        $LASTV
247000980721     C*
247100980721     C* se selezionata opzione personalizzata, richiamo il programma
247200980721     C* corrispondente
247300980721     C     $PGO1(PosOpz) Whenne    *Blank
247400980731     C                   RESET                   NDC061DS
247500980721     C* passare nei campi appositi della DS NDC061DS il valore
247600980721     C* della chiave della VL univoca
247700980731     C                   MOVEL     S1OPZ         OPZ061
247800980731    >C                   MOVEL     C1ctb         ctb061
247900980731    >C                   MOVEL     C1KCC         KCC061
248000980731    >C                   MOVEL     C1KSC         KSC061
248100980731     C     S1DtPar       IFGT      0
248200000118C1309C******dmy          move      s1dtpar       dtpar061
248300000118C1309C     *JobRun       move      s1dtpar       dtpar061
248400980731     C                   ELSE
248500980731     C                   MOVE      *loval        DtPar061
248600980731     C                   ENDIF
248700980731     C                   move      s1nrpar       nrpar061
248800980731     C                   move      s1serpar      serpar061
248900980731    >C                   MOVEL     Snatura002    snatura61
249000980731    >C                   MOVEL     livris002     livris061
249100980731     C                   MOVEL     ndc061DS      KPJBU
249200980730     C*
249300980730     C                   CALL      $PGO1(PosOpz)                        22
249400980730     C                   PARM                    KPJBA
249500980730     C                   PARM                    NOMPGM
249600980730     C*
249700980731     C                   MOVEL     KPJBU         ndc061DS
249800980730     C*
249900980730     C* ritorno da PGM
250000980731     C                   EXSR      RTN061
250001170223R027 C*
250002170223  "  C* se opz 22 = Dati aggiuntivi
250003170223R027 C     S1OPZ         WHENEQ    22
250004170223R224  *
250005170223  "   * ctr chiamata ricorsiva
250006170223  "  C                   reset                   X48ChPgmds
250007170223  "  C                   eval      X48Pgm      = 'SDGSD15R'
250008170223  "  C                   eval      Kpjbu       = X48ChPgmDS
250009170223  "  C                   call      'X48CHPGM'
250010170223  "  C                   parm                    Kpjba
250011170223  "  C                   movel     Kpjbu         x48ChPgmDS
250012170223  "  C                   if        x48errore   = *Off
250013170223R224 C*
250014170223R027 C                   EXSR      CallDatiAgg
250015170223  "  C*
250016170223R027 C                   MOVE      *HIVAL        $LASTV
250017170223R224 C*
250018170223R224 C                   endif
250100981218     C*
250200981218     C* se opz=Estensione nominativi partite
250300981218     C* richiamo il pgm di gestione
250400981218     C     S1OPZ         WHENEQ    34
250500981218     C                   RESET                   NDC066DS1
250600981218     C* Se record di estensione nominativi partite non esiste
250700981218     C*  e non richiesto annullamento/visualizzazione
250800981218     C*  oppure lo stesso è presente allora elabora
250900981218     C     CPAH          IFEQ      *OFF
251000981218     C     OPZ060        ANDNE     '04'
251100981218     C     OPZ060        ANDNE     '05'
251200981218     C     CPAH          OREQ      *ON
251300981218     C* Immissione
251400981218     C     CPAH          IfEq      *OFF
251500981218     C                   MOVE      '01'          OPZ0661
251600981218     C                   Else
251700981218     C     OPZ060        IfEq      *BLANK
251800981218     C                   MOVE      '02'          OPZ0661
251900981218     C                   Else
252000981218     C                   MOVE      OPZ060        OPZ0661
252100981218     C                   EndIf
252200981218     C                   EndIf
252300981218     C*
252400981218     C                   MOVE      C1KCC         KCC0661
252500981218     C                   MOVE      C1KSC         KSC0661
252600981218     C                   MOVEL     C1DESCR       DESBRE0661
252700981218     C     S1DtPar       IFGT      0
252800000118C1309C******DMY          MOVE      S1DTPAR       dtpar0661
252900000118C1309C     *JobRun       MOVE      S1DTPAR       dtpar0661
253000981218     C                   ELSE
253100981218     C                   MOVE      *loval        DtPar0661
253200981218     C                   ENDIF
253300981218     C                   move      s1nrpar       nrpar0661
253400981218     C                   move      s1serpar      serpar0661
253500981218     C*imposto chiave unique per la lista
253600981218     C                   MOVE      H1SYS         SYS0661
253700981218     C                   MOVE      H1NRASINT     NRASIN0661
253800981218     C*
253900981218     C                   MOVE      *BLANK        KPJBU
254000981218     C                   MOVEL     NDC066DS1     KPJBU
254100981218     C                   CALL      'NDC066R1'
254200981218     C                   PARM                    KPJBA
254300981218     C                   MOVEL     KPJBU         NDC066DS1
254400981218     C*
254500981218     C* ritorno da PGM
254600981218     C                   EXSR      RTN0661
254700981218     C                   EndIf
254800981218     C*
254900981218     C                   MOVE      *HIVAL        $LASTV
255000940225     C*
255100950620     C* se altre opzioni
255200950620     C* richiamo pgm gestione anagrafica
255300950907     C* anche per opzione blocco/sblocco, dove eseguirò solo aggiornamento
255400950907     C* archivio
255500941027     C                   OTHER
255600950907     c*
255700950907     C* se opz=Blocca emetto videata richiesta codice blocco
255800950907     C     S1OPZ         IFEQ      60
255900950907     C                   MOVE      'D4'          $GEST
256000950907     C                   move      *on           $INZD4
256100950907     C                   eval      $TAsto = ENTER
256200950907     c* Ciclo richiesta codice blocco per errore o enter
256300950907     C     *in99         doweq     *on
256400950907     C     $tasto        orne      F06
256500950907     C     $tasto        andne     F03
256600950907     C     $tasto        andne     F12
256700950907     C                   EXSR      GESD4
256800950907     C                   enddo
256900950907     C                   endif
257000950907     C*
257100950907     C* Ripristino qui(per f6 e f12 il subfile)
257200950907     C                   MOVE      'S1'          $GEST
257300950907     c* Richiesta uscita da videata D4
257400950907     C     $tasto        ifeq      F12
257500950907     C                   MOVE      *ON           *in99
257600950907     C                   endif
257700950907     C* Se tutto ok richiamo pgm di manutenzione senza far vedere videata
257800950907     C* solo per aggiornare flag blocco
257900950907     c* per manutenzione o flag sblocco
258000950907     C     S1OPZ         ifne      60
258100950907     C*
258200950907     C     S1OPZ         oreq      60
258300950907     C     *IN99         ANDEQ     *off
258400950907     C     $tasto        andeq     F06
258500950907     C*
258600950703    >C                   RESET                   NDC061DS
258700950703    >C                   MOVEL     S1OPZ         OPZ061
258800950703    >C     CMT060        IFEQ      '1'
258900950703    >C                   MOVEL     '1'           CMT061
259000941214     C                   ELSE
259100950703    >C                   MOVEL     '0'           CMT061
259200941214     C                   ENDIF
259300950703     C* passare nei campi appositi della DS NDC061DS il valore
259400950310     C* della chiave della VL univoca
259500950906    >C                   MOVEL     C1ctb         ctb061
259600950704    >C                   MOVEL     C1KCC         KCC061
259700950704    >C                   MOVEL     C1KSC         KSC061
259800950817     C     S1DtPar       IFGT      0
259900000118C1309C******dmy          move      s1dtpar       dtpar061
260000000118C1309C     *JobRun       move      s1dtpar       dtpar061
260100950817     C                   ELSE
260200950817     C                   MOVE      *loval        DtPar061
260300950817     C                   ENDIF
260400950704     C                   move      s1nrpar       nrpar061
260500950704     C                   move      s1serpar      serpar061
260600950705    >C                   MOVEL     Snatura002    snatura61
260700950706    >C                   MOVEL     livris002     livris061
260800950907     C* passo codice blocco
260900950907     C     S1OPZ         IFEQ      60
261000950907     C                   MOVE      D4BKAMM       BKAMM061
261100950907     C                   endif
261200950705     C*
261300950705     C* Chiamate differenti a seconda se il conto è cli/for o conti
261400950703    >C                   MOVEL     NDC061DS      KPJBU
261500950705     C                   if        snatura002 = 'C' or
261600950705     C                             snatura002 = 'F'
261700950703    >C                   CALL      'NDC061R'
261800940314     C                   PARM                    KPJBA
261900950704    >C                   PARM                    AUTDS
262000950705     C                   ELSE
262100950705    >C                   CALL      'NDC062R'
262200950705     C                   PARM                    KPJBA
262300950705    >C                   PARM                    AUTDS
262400950705     C                   ENDIF
262500950301     C*
262600950703    >C                   MOVEL     KPJBU         NDC061DS
262700940201     C*
262800940223     C* ritorno da PGM gestione
262900950703    >C                   EXSR      RTN061
263000940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
263100941027     C     *IN99         IFEQ      *ON
263200940223     C                   SETON                                        40
263300941027     C                   ENDIF
263400950907     C                   ENDIF
263500940131     C*
263600940506     C                   MOVE      *HIVAL        $LASTV
263700950907     C*
263800950907     C*
263900941027     C                   ENDSL
264000940225     C*
264100940131     C                   ENDSR
264200940223     C/EJECT
264201170223R224 X*ex R027 ***************************************************
264202170223 "   X*ex R027 Richiamo pgm per dati anagrafici aggiuntivi
264203170223 "   X*ex R027 ***************************************************
264204170223 "   X*ex R027 CallDatiAgg   BEGSR
264205170223 "   X*ex R027
264206170223 "   X*ex R027           RESET                   SDGSD16DS
264207170223 "   X*ex R027
264208170223 "   X*ex R027           MOVEL     '02'          OPZD16
264209170223 "   X*ex R027 CMT060        IFEQ      '1'
264210170223 "   X*ex R027           MOVEL     '1'           CMTD16
264211170223 "   X*ex R027           ELSE
264212170223 "   X*ex R027           MOVEL     '0'           CMTD16
264213170223 "   X*ex R027           ENDIF
264214170223 "   X*ex R027
264215170223 "   X*ex R027           Eval      SoggD16     = *Blank
264216170223 "   X*ex R027           Eval      DtFiVlD16   = *Loval
264217170223 "   X*ex R027           Eval      SysD16      = H1Sys
264218170223 "   X*ex R027           Eval      NrAsIntD16  = H1NrAsInt
264219170223 "   X*ex R027
264220170223 "   X*ex R027           MOVEL     SDGSD16DS     KPJBU
264221170223 "   X*ex R027
264222170223 "   X*ex R027           CALL      'SDGSD16R'
264223170223 "   X*ex R027           PARM                    KPJBA
264224170223 "   X*ex R027
264225170223 "   X*ex R027           MOVEL     KPJBU         SDGSD16DS
264226170223 "   X*ex R027
264227170223 "   X*ex R027 ritorno da PGM gestione
264228170223 "   X*ex R027           EXSR      RTND16
264229170223 "   X*ex R027
264230170223 "   X*ex R027 se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
264231170223 "   X*ex R027 *IN99         IFEQ      *ON
264232170223 "   X*ex R027           SETON                                        40
264233170223 "   X*ex R027           ENDIF
264234170223 "   X*ex R027
264235170223 "   X*ex R027           MOVE      *HIVAL        $LASTV
264236170223 "   X*ex R027
264237170223 "   X*ex R027           ENDSR
264238170223 "   X*ex R027 /EJECT
264239170223 "   X*ex R027 ***************************************************
264240170223 "   X*ex R027 GESTIONE RITORNO DA PGM DI GESTIONE
264241170223 "   X*ex R027 ***************************************************
264242170223 "   X*ex R027 RTND16        BEGSR
264243170223 "   X*ex R027
264244170223 "   X*ex R027 modo di ritorno
264245170223 "   X*ex R027
264246170223 "   X*ex R027           SELECT
264247170223 "   X*ex R027 questi modi di utilizzo dei valori di ritorno dal
264248170223 "   X*ex R027 pgm di manutenzione rcd di anagrafica sono delle
264249170223 "   X*ex R027 proposte, normalmente sempre valide, ma modificabili
264250170223 "   X*ex R027 per situazioni particolari
264251170223 "   X*ex R027 1 = F3
264252170223 "   X*ex R027 RETD16        WHENEQ    '1'
264253170223 "   X*ex R027           MOVE      *ON           $ESCI                          sospese READC
264254170223 "   X*ex R027           MOVE      *ON           $FINE                          fine pgm
264255170223 "   X*ex R027           MOVE      '1'           RET060
264256170223 "   X*ex R027 2 = F12
264257170223 "   X*ex R027 RETD16        WHENEQ    '2'
264258170223 "   X*ex R027 se il pgm di manutenzione ha prima una guida, è necessario anche
264259170223 "   X*ex R027 questo test, altrimenti prevale il retD04 e il sfl non viene
264260170223 "   X*ex R027 ricaricato
264261170223 "   X*ex R027 OPRD16        ANDNE     '1'
264262170223 "   X*ex R027
264263170223 "   X*ex R027 se l'opzione con cui è stato chiamato non è la modifica
264264170223 "   X*ex R027 memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
264265170223 "   X*ex R027 OPZD16        IFNE      '02'
264266170223 "   X*ex R027           MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
264267170223 "   X*ex R027           else
264268170223 "   X*ex R027           MOVE      *ON           $ESCI                          sospese READC
264269170223 "   X*ex R027           endif
264270170223 "   X*ex R027
264271170223 "   X*ex R027           ENDSL
264272170223 "   X*ex R027
264273170223 "   X*ex R027 operazione eseguite dal pgm chiamato
264274170223 "   X*ex R027
264275170223 "   X*ex R027           SELECT
264276170223 "   X*ex R027 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
264277170223 "   X*ex R027 OPRD16        WHENEQ    '1'
264278170223 "   X*ex R027           MOVE      OPRD16        WOPR                           avverto pgm chiamant
264279170223 "   X*ex R027
264280170223 "   X*ex R027 se l'opzione con cui è stato chiamato non è la modifica
264281170223 "   X*ex R027 memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
264282170223 "   X*ex R027 OPZD16        IFNE      '02'
264283170223 "   X*ex R027           MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
264284170223 "   X*ex R027 se l'opzione con cui è stato chiamato è la modifica
264285170223 "   X*ex R027 aggiorno subito la riga di sfl e non richiedo il ripristino sfl
264286170223 "   X*ex R027           ELSE
264287170223 "   X*ex R027 ***********        EXSR      AGGS1
264288170223 "   X*ex R027           ENDIF
264289170223 "   X*ex R027
264290170223 "   X*ex R027           ENDSL
264291170223 "   X*ex R027
264292170223 "   X*ex R027 funzione non eseguibile per errore :
264293170223 "   X*ex R027
264294170223 "   X*ex R027           SELECT
264295170223 "   X*ex R027 1 = funzione richiamata chiusa in errore
264296170223 "   X*ex R027 ERRD16        WHENEQ    '1'
264297170223 "   X*ex R027           MOVE      *ON           $ESCI                          sospese READC
264298170223 "   X*ex R027           SETON                                        9399      emiss.SFLMSGID
264299170223 "   X*ex R027 2 = funzione richiamata senza la dovuta autorizzazione
264300170223 "   X*ex R027 ERRD16        WHENEQ    '2'
264301170223 "   X*ex R027           MOVE      *ON           $ESCI                          sospese READC
264302170223 "   X*ex R027           SETON                                        9499      emiss.SFLMSGID
264303170223 "   X*ex R027
264304170223 "   X*ex R027           ENDSL
264305170223 "   X*ex R027
264306170223 "   X*ex R027           ENDSR
264307170223R224 X*ex R027 /EJECT
264308170223R224 C************************************************************
264309170223  "  C* Richiamo pgm per dati anagrafici aggiuntivi
264310170223  "  C************************************************************
264311170223R224 C     CallDatiAgg   BEGSR
264312170223     C*
264313170223     C                   RESET                   SDGSD15DS
264314170223     C*
264315170223     C                   MOVEL     S1Opz         OPZD15
264316170223     C     CMT060        IFEQ      '1'
264317170223     C                   MOVEL     '1'           CMTD15
264318170223     C                   ELSE
264319170223     C                   MOVEL     '0'           CMTD15
264320170223     C                   ENDIF
264321170223     C*
264322170223     C                   Eval      SoggD15     = *Blank
264323170223     C                   Eval      DtFiVlD15   = *Loval
264324170223     C                   Eval      SysD15      = H1Sys
264325170223     C                   Eval      NrAsIntD15  = H1NrAsInt
264326170223     C                   Eval      CtbD15      = C1Ctb
264327170223     C     *Jobrun       Move      S1DtPar       DtParD15
264328170223     C                   Eval      NrParD15    = S1NrPar
264329170223     C                   Eval      SerParD15   = S1SerPar
264330170223     C                   Eval      KccD15      = C1Kcc
264331170223     C                   Eval      KscD15      = C1Ksc
264332170223     C                   Eval      TpCallD15   = '3'
264333170223     C                   Eval      TpDatiD15   = '2'
264334170223     C* Tutte le destinazioni
264335170223     C                   Eval      DestD15     = '**'
264336170223     C*
264337170223     C                   MOVEL     SDGSD15DS     KPJBU
264338170223     C*
264339170223     C                   CALL      'SDGSD15R'
264340170223     C                   PARM                    KPJBA
264341170223     C*
264342170223     C                   MOVEL     KPJBU         SDGSD15DS
264343170223     C*
264344170223     C* ritorno da PGM gestione
264345170223     C                   EXSR      RTND15
264346170223     C*
264347170223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
264348170223     C     *IN99         IFEQ      *ON
264349170223     C                   SETON                                        40
264350170223     C                   ENDIF
264351170223     C*
264352170223     C                   MOVE      *HIVAL        $LASTV
264353170223     C*
264354170223R224 C                   ENDSR
264355170223R224 C/EJECT
264356170223R224 C************************************************************
264357170223 "   C* GESTIONE RITORNO DA PGM DI GESTIONE
264358170223 "   C************************************************************
264359170223R224 C     RTND15        BEGSR
264360170223     C*
264361170223     C* modo di ritorno
264362170223     C*
264363170223     C                   SELECT
264364170223     C* questi modi di utilizzo dei valori di ritorno dal
264365170223     C* pgm di manutenzione rcd di anagrafica sono delle
264366170223     C* proposte, normalmente sempre valide, ma modificabili
264367170223     C* per situazioni particolari
264368170223     C* 1 = F3
264369170223    >C     RETD15        WHENEQ    '1'
264370170223     C                   MOVE      *ON           $ESCI                          sospese READC
264371170223     C                   MOVE      *ON           $FINE                          fine pgm
264372170223    >C                   MOVE      '1'           RET060
264373170223     C* 2 = F12
264374170223    >C     RETD15        WHENEQ    '2'
264375170223     C* se il pgm di manutenzione ha prima una guida, è necessario anche
264376170223     c* questo test, altrimenti prevale il retD04 e il sfl non viene
264377170223     C* ricaricato
264378170223    >C     OPRD15        ANDNE     '1'
264379170223     C*
264380170223     C* se l'opzione con cui è stato chiamato non è la modifica
264381170223     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
264382170223    >C     OPZD15        IFNE      '02'
264383170223     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
264384170223     C                   else
264385170223     C                   MOVE      *ON           $ESCI                          sospese READC
264386170223     C                   endif
264387170223     C*
264388170223     C                   ENDSL
264389170223     C*
264390170223     C* operazione eseguite dal pgm chiamato
264391170223     C*
264392170223     C                   SELECT
264393170223     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
264394170223    >C     OPRD15        WHENEQ    '1'
264395170223    >C                   MOVE      OPRD15        WOPR                           avverto pgm chiamant
264396170223     C*
264397170223     C* se l'opzione con cui è stato chiamato non è la modifica
264398170223     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
264399170223    >C     OPZD15        IFNE      '02'
264400170223     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
264401170223     C* se l'opzione con cui è stato chiamato è la modifica
264402170223     C* aggiorno subito la riga di sfl e non richiedo il ripristino sfl
264403170223     C                   ELSE
264404170223     C***********        EXSR      AGGS1
264405170223     C                   ENDIF
264406170223     C*
264407170223     C                   ENDSL
264408170223     C*
264409170223     C* funzione non eseguibile per errore :
264410170223     C*
264411170223     C                   SELECT
264412170223     C* 1 = funzione richiamata chiusa in errore
264413170223    >C     ERRD15        WHENEQ    '1'
264414170223     C                   MOVE      *ON           $ESCI                          sospese READC
264415170223     C                   SETON                                        9399      emiss.SFLMSGID
264416170223     C* 2 = funzione richiamata senza la dovuta autorizzazione
264417170223    >C     ERRD15        WHENEQ    '2'
264418170223     C                   MOVE      *ON           $ESCI                          sospese READC
264419170223     C                   SETON                                        9499      emiss.SFLMSGID
264420170223     C*
264421170223     C                   ENDSL
264422170223     C*
264423170223R224 C                   ENDSR
264424170223R224 C/EJECT
264425940223     C************************************************************
264426940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
264500940223     C************************************************************
264600950703    >C     RTN061        BEGSR
264700940223     C*
264800940223     C* modo di ritorno
264900940223     C*
265000941027     C                   SELECT
265100950301     C* questi modi di utilizzo dei valori di ritorno dal
265200950301     C* pgm di manutenzione rcd di anagrafica sono delle
265300950301     C* proposte, normalmente sempre valide, ma modificabili
265400950301     C* per situazioni particolari
265500940223     C* 1 = F3
265600950703    >C     RET061        WHENEQ    '1'
265700940224     C                   MOVE      *ON           $ESCI
265800940223     C                   MOVE      *ON           $FINE
265900950703     C                   MOVE      '1'           RET060
266000940223     C* 2 = F12
266100950703    >C     RET061        WHENEQ    '2'
266200940223     C                   MOVE      *ON           $ESCI
266300940223     C*
266400941027     C                   ENDSL
266500940223     C*
266600940223     C* operazione eseguite dal pgm chiamato
266700940223     C*
266800950703    >C                   MOVE      OPR061        WOPR
266900941027     C                   SELECT
267000940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
267100950703    >C     OPR061        WHENEQ    '1'
267200950301     C*
267300950301     C* se l'opzione con cui è stato chiamato non è la modifica
267400950301     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
267500950703    >C     OPZ061        IFNE      '02'
267600940223     C                   MOVE      *ON           WINZS1
267700950301     C* se l'opzione con cui è stato chiamato è la modifica
267800950301     C* aggiorno subito la riga di sfl e non richiedo il ripristino sfl
267900950301     C                   ELSE
268000950301     C                   EXSR      AGGS1
268100950301     C                   ENDIF
268200940223     C*
268300941027     C                   ENDSL
268400940223     C*
268500940223     C* funzione non eseguibile per errore :
268600940223     C*
268700941027     C                   SELECT
268800940223     C* 1 = funzione richiamata chiusa in errore
268900950703    >C     ERR061        WHENEQ    '1'
269000940223     C                   MOVE      *ON           $ESCI
269100940223     C                   SETON                                        5299
269200950301     C* 2 = funzione richiamata senza la dovuta autorizzazione
269300950703    >C     ERR061        WHENEQ    '2'
269400950301     C                   MOVE      *ON           $ESCI
269500950301     C                   SETON                                        5499
269600941027     C*
269700941027     C                   ENDSL
269800940223     C*
269900940223     C                   ENDSR
270000950707     C/EJECT
270100950707     C************************************************************
270200950707     C* GESTIONE RITORNO DA PGM DI GESTIONE SCADENZE/rate
270300950707     C************************************************************
270400950707    >C     RTN063        BEGSR
270500950707     C*
270600950707     C* modo di ritorno
270700950707     C*
270800950707     C                   SELECT
270900950707     C* questi modi di utilizzo dei valori di ritorno dal
271000950707     C* pgm di manutenzione rcd di anagrafica sono delle
271100950707     C* proposte, normalmente sempre valide, ma modificabili
271200950707     C* per situazioni particolari
271300950707     C* 1 = F3
271400950707    >C     RET063        WHENEQ    '1'
271500950707     C                   MOVE      *ON           $ESCI
271600950707     C                   MOVE      *ON           $FINE
271700950707     C                   MOVE      '1'           RET060
271800950707     C* 2 = F12
271900950707    >C     RET063        WHENEQ    '2'
272000950707     C                   MOVE      *ON           $ESCI
272100950707     C*
272200950707     C                   ENDSL
272300950707     C*
272400950707     C* operazione eseguite dal pgm chiamato
272500950707     C*
272600950707    >C                   MOVE      OPR063        WOPR
272700950707     C                   SELECT
272800950707     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
272900950707    >C     OPR063        WHENEQ    '1'
273000950707     c*
273100950707     C* Controllo se ci sono sempre rate o sono state cancellate
273200950714     C                   MOVE      H1SYS         PPASYS
273300950714     C                   MOVE      H1NRASINT     PPANRASINT
273400950714     C     K02PPA01      SETLL     NDPSC01                                22
273500950707     C                   if        *in22 = *on
273600950707     C                   eval      RATSCAH = *ON
273700950707     C                   ELSE
273800950707     C                   eval      RATSCAH = *Off
273900950707     C                   endif
274000950707     C*
274100950707     C* se l'opzione con cui è stato chiamato non è la modifica
274200950707     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
274300950707    >C     OPZ063        IFNE      '02'
274400950707     C                   MOVE      *ON           WINZS1
274500950707     C* se l'opzione con cui è stato chiamato è la modifica
274600950707     C* aggiorno subito la riga di sfl e non richiedo il ripristino sfl
274700950707     C                   ELSE
274800950707     C                   EXSR      AGGS1
274900950707     C                   ENDIF
275000950707     C*
275100950707     C                   ENDSL
275200950707     C*
275300950707     C* funzione non eseguibile per errore :
275400950707     C*
275500950707     C                   SELECT
275600950707     C* 1 = funzione richiamata chiusa in errore
275700950707    >C     ERR063        WHENEQ    '1'
275800950707     C                   MOVE      *ON           $ESCI
275900950707     C                   SETON                                        5299
276000950707     C* 2 = funzione richiamata senza la dovuta autorizzazione
276100950707    >C     ERR063        WHENEQ    '2'
276200950707     C                   MOVE      *ON           $ESCI
276300950707     C                   SETON                                        5499
276400950707     C*
276500950707     C                   ENDSL
276600950707     C*
276700950707     C                   ENDSR
276800950707     C/EJECT
276900950707     C************************************************************
277000950707     C* GESTIONE RITORNO DA PGM DI GESTIONE MIX DI PARTITA
277100950707     C************************************************************
277200950707    >C     RTN065        BEGSR
277300950707     C*
277400950707     C* modo di ritorno
277500950707     C*
277600950707     C                   SELECT
277700950707     C* questi modi di utilizzo dei valori di ritorno dal
277800950707     C* pgm di manutenzione rcd di anagrafica sono delle
277900950707     C* proposte, normalmente sempre valide, ma modificabili
278000950707     C* per situazioni particolari
278100950707     C* 1 = F3
278200950707    >C     RET065        WHENEQ    '1'
278300950707     C                   MOVE      *ON           $ESCI
278400950707     C                   MOVE      *ON           $FINE
278500950707     C                   MOVE      '1'           RET060
278600950707     C* 2 = F12
278700950707    >C     RET065        WHENEQ    '2'
278800950707     C                   MOVE      *ON           $ESCI
278900950707     C*
279000950707     C                   ENDSL
279100950707     C*
279200950707     C* operazione eseguite dal pgm chiamato
279300950707     C*
279400950707    >C                   MOVE      OPR065        WOPR
279500950707     C                   SELECT
279600950707     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
279700950707    >C     OPR065        WHENEQ    '1'
279800950707     c*
279900950707     C* Controllo se ci sono sempre rate o sono state cancellate
280000950714     C                   MOVE      H1SYS         PPASYS
280100950714     C                   MOVE      H1NRASINT     PPANRASINT
280200950707     C     K02PPA01      SETLL     NDMIX01                                22
280300950707     C                   if        *in22 = *on
280400950707     C                   eval      MIXH    = *ON
280500950707     C                   ELSE
280600950707     C                   eval      MIXH    = *Off
280700950707     C                   endif
280800950707     C*
280900950707     C* se l'opzione con cui è stato chiamato non è la modifica
281000950707     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
281100950707    >C     OPZ065        IFNE      '02'
281200950707     C                   MOVE      *ON           WINZS1
281300950707     C* se l'opzione con cui è stato chiamato è la modifica
281400950707     C* aggiorno subito la riga di sfl e non richiedo il ripristino sfl
281500950707     C                   ELSE
281600950707     C                   EXSR      AGGS1
281700950707     C                   ENDIF
281800950707     C*
281900950707     C                   ENDSL
282000950707     C*
282100950707     C* funzione non eseguibile per errore :
282200950707     C*
282300950707     C                   SELECT
282400950707     C* 1 = funzione richiamata chiusa in errore
282500950707    >C     ERR065        WHENEQ    '1'
282600950707     C                   MOVE      *ON           $ESCI
282700950707     C                   SETON                                        5299
282800950707     C* 2 = funzione richiamata senza la dovuta autorizzazione
282900950707    >C     ERR065        WHENEQ    '2'
283000950707     C                   MOVE      *ON           $ESCI
283100950707     C                   SETON                                        5499
283200950707     C*
283300950707     C                   ENDSL
283400950707     C*
283500950707     C                   ENDSR
283600950301     C/EJECT
283700981218     C************************************************************
283800981218     C* GESTIONE RITORNO DA PGM DI GESTIONE Estensione nominativi
283900981218     C************************************************************
284000981218    >C     RTN0661       BEGSR
284100981218     C*
284200981218     C* modo di ritorno
284300981218     C*
284400981218     C                   SELECT
284500981218     C* questi modi di utilizzo dei valori di ritorno dal
284600981218     C* pgm di manutenzione rcd di anagrafica sono delle
284700981218     C* proposte, normalmente sempre valide, ma modificabili
284800981218     C* per situazioni particolari
284900981218     C* 1 = F3
285000981218    >C     RET0661       WHENEQ    '1'
285100981218     C                   MOVE      *ON           $ESCI
285200981218     C                   MOVE      *ON           $FINE
285300981218     C                   MOVE      '1'           RET060
285400981218     C* 2 = F12
285500981218    >C     RET0661       WHENEQ    '2'
285600981218     C                   MOVE      *ON           $ESCI
285700981218     C*
285800981218     C                   ENDSL
285900981218     C*
286000981218     C* operazione eseguite dal pgm chiamato
286100981218     C*
286200981218    >C                   MOVE      OPR0661       WOPR
286300981218     C                   SELECT
286400981218     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
286500981218    >C     OPR0661       WHENEQ    '1'
286600981218     c*
286700981218     C* Controllo se ci sono record su NDCPA00F
286800981218     C                   MOVE      H1SYS         PPASYS
286900981218     C                   MOVE      H1NRASINT     PPANRASINT
287000981218     C     K02PPA01      SETLL     NDCPA01                                22
287100981218     C                   if        *in22 = *on
287200981218     C                   eval      CPAH    = *ON
287300981218     C                   ELSE
287400981218     C                   eval      CPAH    = *Off
287500981218     C                   endif
287600981218     C*
287700981218     C* se l'opzione con cui è stato chiamato non è la modifica
287800981218     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
287900981218    >C     OPZ0661       IFNE      '02'
288000981218     C                   MOVE      *ON           WINZS1
288100981218     C* se l'opzione con cui è stato chiamato è la modifica
288200981218     C* aggiorno subito la riga di sfl e non richiedo il ripristino sfl
288300981218     C                   ELSE
288400981218     C                   EXSR      AGGS1
288500981218     C                   ENDIF
288600981218     C*
288700981218     C                   ENDSL
288800981218     C*
288900981218     C* funzione non eseguibile per errore :
289000981218     C*
289100981218     C                   SELECT
289200981218     C* 1 = funzione richiamata chiusa in errore
289300981218    >C     ERR0661       WHENEQ    '1'
289400981218     C                   MOVE      *ON           $ESCI
289500981218     C                   SETON                                        5299
289600981218     C* 2 = funzione richiamata senza la dovuta autorizzazione
289700981218    >C     ERR0661       WHENEQ    '2'
289800981218     C                   MOVE      *ON           $ESCI
289900981218     C                   SETON                                        5499
290000981218     C*
290100981218     C                   ENDSL
290200981218     C*
290300981218     C                   ENDSR
290400981218     C/EJECT
290500950301     C************************************************************
290600950301     C* AGGIORNAMENTO RIGA LISTA
290700950301     C************************************************************
290800950301     C     AGGS1         BEGSR
290900950301     C*
291000950301     C* riempio i campi del sfl che sono stati ripassati dal pgm
291100950301     C* di manutenzione dopo una opzione 02
291200950301     C*
291300950705     C     DTDOC061      IFNE      *LOVAL
291400000118C1309C******dmy          MOVEL     DTDOC061      S1Dtdoc
291500000118C1309C     *JobRun       MOVEL     DTDOC061      S1Dtdoc
291600950817     C                   ELSE
291700950817    >C                   MOVEL     *zero         S1Dtdoc
291800950817     C                   ENDIF
291900950704     C                   MOVE      NRDOC061      S1NRDOC
292000950704     C                   MOVE      SERDOC061     S1SERDOC
292100950906     C                   MOVE      bkamm061      S1bkamm
292200960313     C                   MOVE      condpag061    S1condpag
292300960313     C                   MOVE      unita061      S1unita
292400981006     C                   MOVE      unita061      unita060
292500960313     C                   MOVE      kscage061     S1kscage
292600990317     c                   if        Chiusa061 = *on
292700990317     C                   MOVE      xscon         S1chiusa
292800990317     c                   else
292900990317     c                   clear                   s1Chiusa
293000990317     c                   endif
293100960314     C*  decodifica condizione di pagamento
293200960314     c                   if        s1condpag <> *blanks
293300960314     C                   reset                   DVCDPDS
293400960314     C                   eval      DCPRIC='1'
293500960314     C                   eval      DCPSOC=XSCSOC
293600960314     C                   eval      DCPUNI=d1unita
293700960314     C                   eval      DCPVIS='1'
293800960314     C                   eval      DCPALC=*OFF
293900960314     C                   eval      DCPNRK=1
294000960314     C                   eval      DCPLIN=xsclin
294100960314     C                   eval      DCPCOD=s1condpag
294200960314     C                   eval      DCPERR=*OFF
294300960314     C                   CALL      'DVCDP'
294400960314     C                   PARM                    DVCDPDS
294500960314     C                   PARM                    DVOCDPDS
294600960314   2 C                   if        DCPERR=*OFF
294700960314     C                   eval      s1condpagd=cdpdescr
294800960314   2-C                   endif
294900960314   2-C                   endif
295000960314     c* decodifica agente
295100960314     c                   if        s1kscage  <> *blanks
295200960314     C                   RESET                   DVAGE
295300960314     C                   MOVE      '1'           DAGRIC
295400960314     C                   MOVE      xscsoc        DAGSOC
295500960314     C                   MOVE      d1unita       DAGUNI
295600960314     C                   MOVE      '1'           DAGVIS
295700960314     C                   MOVE      *OFF          DAGALC
295800960314     C                   MOVE      1             DAGNRK
295900960314     C                   CLEAR                   DAGDTA
296000960314     C                   MOVEL     s1kscage      DAGKSC
296100960314     C                   MOVE      'DVOAGEDS'    DAGSET
296200960314     C                   MOVE      *OFF          DAGERR
296300960314     C                   CALL      'DVAGE'
296400960314     C                   PARM                    DVAGE
296500960314     C                   PARM                    DVOAGE
296600960314     C*
296700960314     C     DAGERR        IFEQ      *OFF
296800960314     C                   MOVEL     AGERAGsoc     s1kscaged
296900960314     c                   endif
297000960314     c                   endif
297100950301     C*
297200950301     C                   ENDSR
297300941124     C/EJECT
297400941124     C************************************************************
297500941124     C* GESTIONE RITORNO DA PGM DEI COMMENTI
297600941124     C************************************************************
297700941124     C     RTN725        BEGSR
297800941124     C*
297900941124     C* modo di ritorno
298000941124     C*
298100941124     C                   SELECT
298200950301     C* questi modi di utilizzo dei valori di ritorno dal
298300950301     C* pgm di manutenzione rcd di anagrafica sono delle
298400950301     C* proposte, normalmente sempre valide, ma modificabili
298500950301     C* per situazioni particolari
298600941124     C* 1 = F3
298700941124     C     RET725        WHENEQ    '1'
298800941124     C                   MOVE      *ON           $ESCI
298900941124     C                   MOVE      *ON           $FINE
299000941124     C* 2 = F12
299100941124     C     RET725        WHENEQ    '2'
299200941124     C                   MOVE      *ON           $ESCI
299300941124     C*
299400941124     C                   ENDSL
299500941124     C*
299600941124     C* operazione eseguite dal pgm chiamato
299700941124     C*
299800941124     C                   SELECT
299900941124     C* 1 = eseguita scrittura o modifica commento
300000941124     C     OPR725        WHENEQ    '1'
300100950704    >C* segnalare su sfl guida l'avvenuto aggiornamento
300200950704     C*
300300950704     C* reperisco se commento presente
300400950704     C                   MOVE      *BLANK        EXI724
300500950704     C                   RESET                   A724DS
300600950704     C                   MOVEL     '01'          OPZ724
300700950704     C                   MOVEL     XscSOC        SOC724
300800950704     C                   MOVEL     *BLANK        UNI724
300900950704     C                   MOVEL     'PPA'         TPA724
301000950907     C                   MOVEL     C1CTB         KY1724
301100950907     C                   MOVEL     C1KCC         KY2724
301200950907     C                   MOVE      C1KSC         KY2724
301300950907    >C                   MOVEL     S1DTPAR       KY3724
301400950907    >C                   MOVEL     s1nrpar       KY4724
301500950907    >C                   MOVE      s1serpar      KY4724
301600950704     C     *eur          MOVEL     *DATE         DTS724
301700950704     C                   MOVEL     A724DS        KPJBU
301800950704     C                   CALL      'ANA724R'
301900950704     C                   PARM                    KPJBA
302000950704     C                   MOVEL     KPJBU         A724DS
302100950704     C* memorizzo lo stato del  bottone TCM
302200950704     C                   MOVE      EXI724        COMMENTIH
302300941124     C*
302400941124     C                   ENDSL
302500941124     C*
302600941124     C* funzione non eseguibile per errore :
302700941124     C*
302800941124     C                   SELECT
302900941124     C* 1 = funzione richiamata chiusa in errore
303000941124     C     ERR725        WHENEQ    '1'
303100941124     C                   MOVE      *ON           $ESCI
303200941124     C                   SETON                                        5299
303300950331     C* 2 = funzione richiamata senza la dovuta autorizzazione
303400950331     C     ERR725        WHENEQ    '2'
303500950331     C                   MOVE      *ON           $ESCI
303600950331     C                   SETON                                        5499
303700950704    >C*  eventualmente gestire altri codici di errore
303800941124     C*
303900941124     C                   ENDSL
304000941124     C*
304100941124     C                   ENDSR
304200950331     C/EJECT
304300950331     C************************************************************
304400950331     C* GESTIONE RITORNO DA PGM INVIO
304500950331     C************************************************************
304600950331     C     RTN727        BEGSR
304700950331     C*
304800950331     C* modo di ritorno
304900950331     C*
305000950331     C                   SELECT
305100950331     C* questi modi di utilizzo dei valori di ritorno dal
305200950331     C* pgm di manutenzione rcd di anagrafica sono delle
305300950331     C* proposte, normalmente sempre valide, ma modificabili
305400950331     C* per situazioni particolari
305500950331     C* 1 = F3
305600950331     C     RET727        WHENEQ    '1'
305700950331     C                   MOVE      *ON           $ESCI
305800950331     C                   MOVE      *ON           $FINE
305900950331     C* 2 = F12
306000950331     C     RET727        WHENEQ    '2'
306100950331     C                   MOVE      *ON           $ESCI
306200950331     C*
306300950331     C                   ENDSL
306400950331     C*
306500950331     C* operazione eseguite dal pgm chiamato
306600950331     C*
306700950331     C                   SELECT
306800950331     C* 1 = eseguito aggiornamento
306900950331     C     OPR727        WHENEQ    '1'
307000950331     C*
307100950331     C                   ENDSL
307200950331     C*
307300950331     C* funzione non eseguibile per errore :
307400950331     C*
307500950331     C                   SELECT
307600950331     C* 1 = funzione richiamata chiusa in errore
307700950331     C     ERR727        WHENEQ    '1'
307800950331     C                   MOVE      *ON           $ESCI
307900950331     C                   SETON                                        5299
308000950331     C* 2 = funzione richiamata senza la dovuta autorizzazione
308100950331     C     ERR727        WHENEQ    '2'
308200950331     C                   MOVE      *ON           $ESCI
308300950331     C                   SETON                                        5499
308400950331     C*
308500950331     C                   ENDSL
308600950331     C*
308700950331     C* disattivo il flag di avvenuta EXFMT dopo il richiamo a pgm
308800950331     C* di sola finestra
308900950331     C                   MOVEL     '0'           WIN727
309000950331     C*
309100950331     C                   ENDSR
309200940223     C/EJECT
309300940131     C************************************************************
309400940131     C* GESTIONE VALORI ASSUNTI
309500940131     C************************************************************
309600940131     C     GESD2         BEGSR
309700940131     C*
309800941118   B1C     $INZD2        IFEQ      *ON
309900940131     C                   EXSR      INZD2
310000940223     C                   MOVE      *OFF          $INZD2
310100941118   E1C                   ENDIF
310200940202     C*
310300941118   B1C     $LASTV        IFNE      'D2'
310400940923     C                   EXSR      GESPIE
310500940202     C                   MOVE      $LASTV        $LASTG
310600940202     C                   MOVE      'D2'          $LASTV
310700941118   E1C                   ENDIF
310800940131     C*
310900940707     C* se esistono errori sulla videata
311000940707     C* emetto la write del formato a indicatori spenti per vedere
311100940707     C* le eventuali decodifiche
311200941118   B1C     *IN99         IFEQ      *ON
311300940707     C                   MOVEA     *IN           WIN
311400940707     C                   MOVE      *ALL'0'       IN5098           49
311500940707     C                   MOVEA     IN5098        *IN(50)
311600940707     C                   WRITE     FMTD2
311700940707     C                   MOVEA     WIN           *IN
311800941118   E1C                   ENDIF
311900940707     C*
312000940131     C                   EXFMT     FMTD2
312100941205     C*
312200941205     C* controllo abilitazione tasti funzione
312300941205     C                   EXSR      CTRKD2
312400941205     C*
312500941205   B1C     *IN99         IFEQ      *OFF
312600941205     C*
312700941205   B2C                   SELECT
312800940131     C* F3=Fine
312900940506     C     $TASTO        WHENEQ    F03
313000940309     C                   EXSR      F03D2
313100940131     C* F12=Ritorno
313200940506     C     $TASTO        WHENEQ    F12
313300940309     C                   EXSR      F12D2
313400940916     C* F24=Altri tasti
313500940916     C     $TASTO        WHENEQ    F24
313600940916     C                   EXSR      F24D2
313700940131     C*
313800940131     C                   OTHER
313900940131     C                   EXSR      CTRD2
314000941205   B3C     *IN99         IFEQ      *OFF
314100940211     C                   MOVE      $LASTG        $GEST
314200941205   B4C                   SELECT
314300940202     C     $LASTG        WHENEQ    'S1'
314400940131     C* se è stato modificato l'ordinamento di default, ricarico la lista
314500950704   B5C*    $ORD          IFNE      d2ORD
314600950704     C*                  MOVE      *ON           $INZS1
314700950704     C*                  MOVE      *BLANK        W1POS
314800950704   E5C*                  ENDIF
314900940202     C     $LASTG        WHENEQ    'D1'
315000940318     C* per ora niente
315100941205   E4C                   ENDSL
315200940131     C* definisco i nuovi valori di default
315300940201     C                   Z-ADD     D2LOP         $LOP
315400950704     C*                  Z-ADD     D2ORD         $ORD
315500950704    >C*  elenco altri dati default specifici dell'anagrafica
315600950322     C*
315700950322     C* aggiorno valori assunti
315800950328     C* se richiesto il salvataggio
315900950328   B4C     D2BSV         IFEQ      1
316000950322     C                   EXSR      AGGMPE
316100950322   E4C                   ENDIF
316200950322     C*
316300941205   E3C                   ENDIF
316400940131     C*
316500941205   E2C                   ENDSL
316600940131     C*
316700941205   E1C                   ENDIF
316800941205     C*
316900940131     C                   ENDSR
317000940223     C/EJECT
317100940131     C************************************************************
317200940211     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
317300940131     C************************************************************
317400940201     C     INZD2         BEGSR
317500940131     C*
317600940131     C                   CLEAR                   FMTD2
317700021014C1595C                   EVAL      D2BSVA = Normale
317800950328     C* se sono entrato con chiave univoca
317900950328     C* impedisco il salvataggio valori assunti
318000950606     C     $Unique       IFEQ      *ON
318100021014C1595X***                Z-ADD     2             D2BSV
318200021014C1595C                   EVAL      D2BSVA = Protetto
318300950328     C                   ENDIF
318400940201     C                   MOVEL     $LOP          D2LOP
318500950704     C***                MOVE      $ORD          D2ORD
318600940131     C*
318700940131     C                   ENDSR
318800941205     C/EJECT
318900941205     C************************************************************
319000941205     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D2
319100941205     C* INPUT
319200941205     C* $FC3 = Abilitazioni videata D2
319300941205     C* OUTPUT
319400941205     C* *IN97 *IN99 Errori
319500941205     C************************************************************
319600941205     C     CTRKD2        BEGSR
319700941205     C*
319800941205     C                   SETOFF                                       99
319900941205     C                   MOVEA     $FC3          $FC
320000941205     C                   EXSR      CTRKEY
320100941205     C     *IN99         IFEQ      *ON
320200941205     C                   SETON                                        97
320300941205     C                   ENDIF
320400941205     C*
320500941205     C                   ENDSR
320600940309     C/EJECT
320700940309     C************************************************************
320800940309     C* GESTIONE F03 VIDEO D2
320900940309     C************************************************************
321000940309     C     F03D2         BEGSR
321100940309     C*
321200940309     C                   MOVE      *ON           $FINE
321300940325     C* fine programma
321400940325     C                   EXSR      ENDPGM
321500940309     C*
321600940309     C                   ENDSR
321700940309     C/EJECT
321800940309     C************************************************************
321900940309     C* GESTIONE F12 VIDEO D2
322000940309     C************************************************************
322100940309     C     F12D2         BEGSR
322200940309     C*
322300940309     C                   MOVE      $LASTG        $GEST
322400940309     C*
322500940309     C                   ENDSR
322600940916     C/EJECT
322700940916     C************************************************************
322800940916     C* GESTIONE F24 VIDEO D2
322900940916     C************************************************************
323000940916     C     F24D2         BEGSR
323100940916     C*
323200940916     C                   MOVEA     $FX3          $FX
323300940916     C                   MOVEA     $FC3          $FC
323400940916     C                   MOVEA     $FR3          $FR
323500940916     C                   Z-ADD     $ULKD2        $ULK
323600950403    >C                   Z-ADD     44            $L01
323700950328    >C                   Z-ADD     0             $L02
323800940916     C                   EXSR      RIEKEY
323900940916     C                   Z-ADD     $ULK          $ULKD2            3 0
324000940916     C                   MOVEA     $FC           $FC3
324100940916     C                   MOVEL     $KEY1         Z3KE1
324200060224C1913C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
324300940916     C                   WRITE     FMTZ3
324400940916     C*
324500940916     C                   ENDSR
324600940223     C/EJECT
324700940131     C************************************************************
324800940131     C* CONTROLLO VIDEATA VALORI ASSUNTI
324900940131     C************************************************************
325000940131     C     CTRD2         BEGSR
325100940131     C*
325200940131     C                   SETOFF                                       99
325300940131     C* non imposto pos. del cursore
325400940131     C                   Z-ADD     0             H1RIGA
325500940131     C                   Z-ADD     0             H1COLO
325600940131     C*
325700940131     C* OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
325800940201     C     D2LOP         IFEQ      0
325900940131     C                   SETON                                          5099
326000941027     C                   ENDIF
326100940131     C*
326200940131     C* OBBLIGATORIO VALORE PER ORDINAMENTO
326300950704     C*    D2ORD         IFEQ      0
326400950704     C*                  SETON                                          5199
326500950704     C*                  ENDIF
326600950403     C*
326700950704    >C*  elenco controlli altri dati default specifici dell'anagrafica
326800940131     C*
326900940131     C                   ENDSR
327000950907     C/EJECT
327100950907     C************************************************************
327200950907     C* GESTIONE richiesta codice blocco amministrativo
327300950907     C************************************************************
327400950907     C     GESD4         BEGSR
327500950907     C*
327600950907   B1C     $INZD4        IFEQ      *ON
327700950907     C                   EXSR      INZD4
327800950907     C                   MOVE      *OFF          $INZD4
327900950907   E1C                   ENDIF
328000950907     C*
328100950907   B1C     $LASTV        IFNE      'D4'
328200950907     C                   EXSR      GESPIE
328300950907     C                   MOVE      $LASTV        $LASTG
328400950907     C                   MOVE      'D4'          $LASTV
328500950907   E1C                   ENDIF
328600950907     C*
328700950907     C* se esistono errori sulla videata
328800950907     C* emetto la write del formato a indicatori spenti per vedere
328900950907     C* le eventuali decodifiche
329000950907   B1C     *IN99         IFEQ      *ON
329100950907     C                   MOVEA     *IN           WIN
329200950907     C                   MOVE      *ALL'0'       IN5098           49
329300950907     C                   MOVEA     IN5098        *IN(50)
329400950907     C                   WRITE     FMTD4
329500950907     C                   MOVEA     WIN           *IN
329600950907   E1C                   ENDIF
329700950907     C*
329800950907     C                   EXFMT     FMTD4
329900950907     C*
330000950907     C* controllo abilitazione tasti funzione
330100950907     C                   EXSR      CTRKD4
330200950907     C*
330300950907   B1C     *IN99         IFEQ      *OFF
330400950907     C*
330500950907   B2C                   SELECT
330600950907     C* F3=Fine
330700950907     C     $TASTO        WHENEQ    F03
330800950907     C                   EXSR      F03D4
330900950907     C* F4=Ricerca
331000950907     C     $TASTO        WHENEQ    F04
331100950907     C                   EXSR      SEARCH
331200950907     C* F24=Altri tasti
331300950907     C     $TASTO        WHENEQ    F24
331400950907     C                   EXSR      F24D4
331500950907     C*
331600950907     C                   OTHER
331700950907     C     $TASTO        ifne      F12
331800950907     C                   EXSR      CTRD4
331900950907     C                   endif
332000950907     C*
332100950907   E2C                   ENDSL
332200950907     C*
332300950907   E1C                   ENDIF
332400950907     C*
332500950907     C                   ENDSR
332600950907     C/EJECT
332700950907     C************************************************************
332800950907     C* INIZIALIZZAZIONE VIDEATA blocco amministrativo
332900950907     C************************************************************
333000950907     C     INZD4         BEGSR
333100950907     C*
333200950907     C                   CLEAR                   FMTD4
333300950908     C                   MOVE      S1BKAMM       D4BKAMM
333400950907     C*
333500950907     C                   ENDSR
333600950907     C/EJECT
333700950907     C************************************************************
333800950907     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D4
333900950907     C* INPUT
334000950907     C* $FC4 = Abilitazioni videata D4
334100950907     C* OUTPUT
334200950907     C* *IN97 *IN99 Errori
334300950907     C************************************************************
334400950907     C     CTRKD4        BEGSR
334500950907     C*
334600950907     C                   SETOFF                                       99
334700950907     C                   MOVEA     $FC4          $FC
334800950907     C                   EXSR      CTRKEY
334900950907     C     *IN99         IFEQ      *ON
335000950907     C                   SETON                                        97
335100950907     C                   ENDIF
335200950907     C*
335300950907     C                   ENDSR
335400950907     C/EJECT
335500950907     C************************************************************
335600950907     C* GESTIONE F03 VIDEO D4
335700950907     C************************************************************
335800950907     C     F03D4         BEGSR
335900950907     C*
336000950907     C                   MOVE      *ON           $FINE
336100950907     C* fine programma
336200950907     C                   EXSR      ENDPGM
336300950907     C*
336400950907     C                   ENDSR
336500950907     C/EJECT
336600950907     C************************************************************
336700950907     C* GESTIONE F24 VIDEO D4
336800950907     C************************************************************
336900950907     C     F24D4         BEGSR
337000950907     C*
337100950907     C                   MOVEA     $FX4          $FX
337200950907     C                   MOVEA     $FC4          $FC
337300950907     C                   MOVEA     $FR4          $FR
337400950907     C                   Z-ADD     $ULKD4        $ULK
337500950907    >C                   Z-ADD     44            $L01
337600950907    >C                   Z-ADD     0             $L02
337700950907     C                   EXSR      RIEKEY
337800950907     C                   Z-ADD     $ULK          $ULKD4            3 0
337900950907     C                   MOVEA     $FC           $FC4
338000950907     C                   MOVEL     $KEY1         Z4KE1
338100060224C1913C                   MOVEL     $ALLFUNCT     H4ALLFUNCT
338200950907     C                   WRITE     FMTZ4
338300950907     C*
338400950907     C                   ENDSR
338500950907     C/EJECT
338600950907     C************************************************************
338700950907     C* CONTROLLO VIDEATA richiesta codice blocco
338800950907     C************************************************************
338900950907     C     CTRD4         BEGSR
339000950907     C*
339100950907     C                   SETOFF                                       99
339200950907     C* non imposto pos. del cursore
339300950907     C                   Z-ADD     0             H1RIGA
339400950907     C                   Z-ADD     0             H1COLO
339500950907     C*
339600950907     C* Blocco amministrativo
339700950907     C                   CLEAR                   D4BKAMMD
339800950907     C                   MOVE      XSCCUT        XTBUTE
339900950907     C                   MOVE      '1'           XTBRIC
340000950907     C                   MOVE      XSCSOC        XTBAZI
340100950907     C                   MOVE      *ZERO         XTBKEY
340200950907     C                   MOVE      '0AB'         XTBCOD
340300950907     C                   MOVE      D4BKAmm       XTBKEY
340400961011     C                   CALLb     'XATB'
340500950907     C                   PARM                    XATBDS
340600950907     C     XTBERR        IFEQ      '1'
340700950907     C                   SETON                                          5099
340800950907     C                   ELSE
340900950907     C                   MOVEL     XTBUNI        TAB0AB
341000950907     C                   MOVEL     DES0AB        D4BKAMMD
341100950907     C                   ENDIF
341200950907     C*
341300950907     C                   ENDSR
341400941205     C/EJECT
341500941205     C************************************************************
341600941205     C* CONTROLLO ABILITAZIONE TASTI
341700941205     C* INPUT
341800941205     C* $TASTO =Tasto funzione premuto
341900941205     C* $FC = Tasti abilitati
342000941205     C* OUTPUT
342100941205     C* IN99 = ERrore
342200941205     C************************************************************
342300941205     C     CTRKEY        BEGSR
342400941205     C*
342500941205     C                   SELECT
342600941205     C     $TASTO        WHENEQ    F01
342700980720     C                   z-add     01            posfun
342800941205     C     $FC(1)        IFEQ      '0'
342900941205     C                   SETON                                        99
343000941205     C                   ENDIF
343100941205     C     $TASTO        WHENEQ    F02
343200980720     C                   z-add     02            posfun
343300941205     C     $FC(2)        IFEQ      '0'
343400941205     C                   SETON                                        99
343500941205     C                   ENDIF
343600941205     C     $TASTO        WHENEQ    F03
343700980720     C                   z-add     03            posfun
343800941205     C     $FC(3)        IFEQ      '0'
343900941205     C                   SETON                                        99
344000941205     C                   ENDIF
344100941205     C     $TASTO        WHENEQ    F04
344200980720     C                   z-add     04            posfun
344300941205     C     $FC(4)        IFEQ      '0'
344400941205     C                   SETON                                        99
344500941205     C                   ENDIF
344600941205     C     $TASTO        WHENEQ    F05
344700980720     C                   z-add     05            posfun
344800980720     C                   z-add     01            posfun
344900941205     C     $FC(5)        IFEQ      '0'
345000941205     C                   SETON                                        99
345100941205     C                   ENDIF
345200941205     C     $TASTO        WHENEQ    F06
345300980720     C                   z-add     06            posfun
345400941205     C     $FC(6)        IFEQ      '0'
345500941205     C                   SETON                                        99
345600941205     C                   ENDIF
345700941205     C     $TASTO        WHENEQ    F07
345800980720     C                   z-add     07            posfun
345900941205     C     $FC(7)        IFEQ      '0'
346000941205     C                   SETON                                        99
346100941205     C                   ENDIF
346200941205     C     $TASTO        WHENEQ    F08
346300980720     C                   z-add     08            posfun
346400941205     C     $FC(8)        IFEQ      '0'
346500941205     C                   SETON                                        99
346600941205     C                   ENDIF
346700941205     C     $TASTO        WHENEQ    F09
346800980720     C                   z-add     09            posfun
346900941205     C     $FC(09)       IFEQ      '0'
347000941205     C                   SETON                                        99
347100941205     C                   ENDIF
347200941205     C     $TASTO        WHENEQ    F10
347300980720     C                   z-add     10            posfun
347400941205     C     $FC(10)       IFEQ      '0'
347500941205     C                   SETON                                        99
347600941205     C                   ENDIF
347700941205     C     $TASTO        WHENEQ    F11
347800980720     C                   z-add     11            posfun
347900941205     C     $FC(11)       IFEQ      '0'
348000941205     C                   SETON                                        99
348100941205     C                   ENDIF
348200941205     C     $TASTO        WHENEQ    F12
348300980720     C                   z-add     12            posfun
348400941205     C     $FC(12)       IFEQ      '0'
348500941205     C                   SETON                                        99
348600941205     C                   ENDIF
348700941205     C     $TASTO        WHENEQ    F13
348800980720     C                   z-add     13            posfun
348900941205     C     $FC(13)       IFEQ      '0'
349000941205     C                   SETON                                        99
349100941205     C                   ENDIF
349200941205     C     $TASTO        WHENEQ    F14
349300980720     C                   z-add     14            posfun
349400941205     C     $FC(14)       IFEQ      '0'
349500941205     C                   SETON                                        99
349600941205     C                   ENDIF
349700941205     C     $TASTO        WHENEQ    F15
349800980720     C                   z-add     15            posfun
349900941205     C     $FC(15)       IFEQ      '0'
350000941205     C                   SETON                                        99
350100941205     C                   ENDIF
350200941205     C     $TASTO        WHENEQ    F16
350300980720     C                   z-add     16            posfun
350400941205     C     $FC(16)       IFEQ      '0'
350500941205     C                   SETON                                        99
350600941205     C                   ENDIF
350700941205     C     $TASTO        WHENEQ    F17
350800980720     C                   z-add     17            posfun
350900941205     C     $FC(17)       IFEQ      '0'
351000941205     C                   SETON                                        99
351100941205     C                   ENDIF
351200941205     C     $TASTO        WHENEQ    F18
351300980720     C                   z-add     18            posfun
351400941205     C     $FC(18)       IFEQ      '0'
351500941205     C                   SETON                                        99
351600941205     C                   ENDIF
351700941205     C     $TASTO        WHENEQ    F19
351800980720     C                   z-add     19            posfun
351900941205     C     $FC(19)       IFEQ      '0'
352000941205     C                   SETON                                        99
352100941205     C                   ENDIF
352200941205     C     $TASTO        WHENEQ    F20
352300980720     C                   z-add     20            posfun
352400941205     C     $FC(20)       IFEQ      '0'
352500941205     C                   SETON                                        99
352600941205     C                   ENDIF
352700941205     C     $TASTO        WHENEQ    F21
352800980720     C                   z-add     21            posfun
352900941205     C     $FC(21)       IFEQ      '0'
353000941205     C                   SETON                                        99
353100941205     C                   ENDIF
353200941205     C     $TASTO        WHENEQ    F22
353300980720     C                   z-add     22            posfun
353400941205     C     $FC(22)       IFEQ      '0'
353500941205     C                   SETON                                        99
353600941205     C                   ENDIF
353700941205     C     $TASTO        WHENEQ    F23
353800980720     C                   z-add     23            posfun
353900941205     C     $FC(23)       IFEQ      '0'
354000941205     C                   SETON                                        99
354100941205     C                   ENDIF
354200941205     C     $TASTO        WHENEQ    F24
354300980720     C                   z-add     24            posfun
354400941205     C     $FC(24)       IFEQ      '0'
354500941205     C                   SETON                                        99
354600941205     C                   ENDIF
354700941205     C                   ENDSL
354800941205     C*
354900941205     C                   ENDSR
355000940223     C/EJECT
355100940201     C************************************************************
355200940201     C* AGGIORNAMENTO VALORI ASSUNTI
355300940201     C************************************************************
355400940201     C     AGGMPE        BEGSR
355500940201     C*
355600950524     C* preparo i parametri di chiamata al driver
355700950524     C                   CLEAR                   XMPEDS
355800950524     C                   EVAL      TpChiamata = '03'
355900950524     C                   EVAL      NChiavi = 5
356000950524     C                   EVAL      VLogica = 2
356100950524     C*
356200950524     C* preparo le chiavi per la chiamata al driver
356300950524     C                   CLEAR                   XMPEDS1
356400950524     C                   MOVEL     XSCSOC        DSMPESOC
356500950524     C                   MOVEL     DSPGM         DSMPEPGM
356600950524     C                   MOVEL     KNMUS         DSMPEPRF
356700941013     C*
356800950322     C* Si memorizzano i dati di default a seconda delle modalità con
356900950322     C* cui il pgm è usato
357000950322     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
357100950322     C* MPETIP = 'PV'  pgm richiamato da menù
357200950322     C* MPETIP = 'PB'  x lancio Batch
357300950322     C*
357400941013     C                   SELECT
357500950703    >C     OPZ060        WHENEQ    '01'
357600950524     C                   MOVEL     'PX'          DSMPETIP
357700941013     C                   OTHER
357800950524     C                   MOVEL     'PV'          DSMPETIP
357900941013     C                   ENDSL
358000950524     C* dati
358100950524     C                   MOVEL     DSMPE         DSMPEPAR
358200950524     C*
358300950524     C                   CALLB     'XMPE'
358400950524     C                   PARM                    XMPEDS
358500950524     C                   PARM                    XMPEDS1
358600940201     C*
358700940201     C                   ENDSR
358800940223     C/EJECT
358900940131     C************************************************************
359000940131     C* OPERAZIONI INIZIALI
359100940131     C************************************************************
359200940131     C     *INZSR        BEGSR
359300000118C1309C     *DMY          Move      Udate         ZDateISO
359400000118C1309C     *JobRun       Move      ZDateISO      ZDate             6 0
359500940131     C*
359600940127     C* Reperimento parametri
359700940127     C*
359800940117     C     *ENTRY        PLIST
359900940117     C                   PARM                    KPJBA
360000950410     C                   PARM                    DSAUT
360100950410     C*
360200950410     C     PARMS         IFLT      2
360300950410     C                   MOVE      *BLANK        AUTDS
360400950410     C                   ELSE
360500950410     C                   MOVEL     DSAUT         AUTDS
360600950410     C                   ENDIF
360700980722      * Reperimento dati società
360800980722      *
360900980722     C                   MOVEL     'SOC001'      TIPXSC
361000980722     C                   MOVEL     *BLANK        SOCXSC
361100980722     C                   EXSR      REPSOC
361200980722     C     RTNXSC        IFNE      '1'
361300980722     C                   MOVEL     XSOCDS        SOC001
361400980722     C                   ELSE
361500980722     C                   MOVE      *ON           $FINE
361600980722     C                   EXSR      ENDPGM
361700980722     C                   ENDIF
361800940225     C*
361900940223     C* Reperimento opzioni
362000940127     C*
362100940128     C                   EXSR      REPOPZ
362200950406     C*
362300940506     C* Reperimento tasti di funzione
362400940506     C*
362500940506     C                   EXSR      REPKEY
362600940506     C*
362700980722     C* Chiamata a routine per il reperimento delle opzioni
362800980722     C* personalizzate
362900980722     C                   CAllb     'X46USREXT'
363000980722     C                   PARM                    $OP1
363100980722     C                   PARM                    $TX1
363200980722     C                   PARM                    $FL1
363300980722     C                   PARM                    $PGO1
363400980722     C                   PARM      $dimOp        NumOpz
363500980722     C                   PARM                    $FX2
363600980722     C                   PARM                    $FC2
363700980722     C                   PARM                    $PGF2
363800980722     C                   PARM                    DsPgm
363900980722     C                   PARM                    xsccut
364000980722     C                   PARM                    xscgrp
364100980722     C*
364200980722     C                   Z-add     1             OPZLEN
364300980722     C     *blank        Lookup    $TX1(opzlen)                           21
364400980722     C                   if        *in21 = *on
364500980722     C                   sub       1             OPZLEN
364600980722     C                   else
364700980722     C                   Z-add     $DimOp        OPZLEN
364800980722     C                   endif
364900980722     C*
365000940127     C*
365100940117     C                   ENDSR
365200940223     C/EJECT
365300940223     C************************************************************
365400940223     C* INIZIALIZZAZIONE VARIABILI
365500940223     C************************************************************
365600940223     C     INZVAR        BEGSR
365700941215     C*
365800941215     C* Dati da PGM chiamante
365900941215     C*
366000950704    >C                   MOVEL     KPJBU         NDC060DS
366100981111      * Reperimento dati società se cambiata
366200981111      *
366300981111     c                   if        societa060 <> xscsoc and
366400981111     c                             societa060 <> *blanks
366500981111     C                   MOVEL     'SOC001'      TIPXSC
366600981111     C                   MOVEL     societa060    SOCXSC
366700981111     C                   EXSR      REPSOC
366800981111     C     RTNXSC        IFNE      '1'
366900981111     C                   MOVEL     XSOCDS        SOC001
367000981111     C                   ELSE
367100981111     C                   MOVE      *ON           $FINE
367200981111     C                   EXSR      ENDPGM
367300981111     C                   ENDIF
367400981111     C                   ENDIF
367500950705     c*
367600950705     C* Non faccio fare il comit al pgm di manutenzione
367700950705     C                   eval      CMT060  = *on
367800941201     C*
367900941124     C* Reperimento autorizzazioni
368000941124     C*
368100990812C1221C* non serve qui perchè ancora non si sa con quale unità!!!!!
368200990812C1221C****************** EXSR      REPAUT
368300941026     C*
368400941026     C* Reperimento valori default utente
368500941026     C*
368600941026     C                   EXSR      REPDFT
368700940506     C*
368800940506     C* Definizione delle opzioni utilizzabili a pgm
368900940506     C*
369000940506     C                   EXSR      DEFOPZ
369100950330     C*
369200940223     C* Variabili per gestione videate
369300940223     C*
369400940223     C                   MOVE      *OFF          $FINE
369500940223     C                   MOVE      *OFF          $INZD1
369600940223     C                   MOVE      *OFF          $INZS1
369700940223     C                   MOVE      *OFF          $INZD2
369800950606     C                   MOVE      *OFF          $EndFile
369900940223     C                   MOVE      *OFF          $ESCI
370000940223     C                   MOVE      *OFF          $RCDOK
370100940509     C                   MOVE      *HIVAL        $LASTV
370200940506     C                   Z-ADD     0             $ULKD1            3 0
370300940506     C                   Z-ADD     0             $ULKS1            3 0
370400940916     C                   Z-ADD     0             $ULKD2            3 0
370500950907     C                   Z-ADD     0             $ULKD4            3 0
370600960313     C* visualizzo il sfl troncato
370700960313     C                   Eval      c1mode=*on
370800940223     C*
370900940318     C                   MOVE      *ON           $INZD1
371000940318     C                   MOVE      *ON           $INZS1
371100940831     C*
371200940831     C* se pgm richiamato con chiave univoca impostata,
371300940831     C* forzo la emissione della lista e imposto l'ordinamento
371400950703    >C     UQ1060        IFNE      *BLANK
371500950606     C                   MOVE      *ON           $Unique
371600941213     C                   Z-ADD     2             $LOP
371700950301     C                   Z-ADD     99            $ORD
371800941213     C                   ELSE
371900950606     C                   MOVE      *OFF          $Unique
372000941213     C                   ENDIF
372100950330     C*
372200950330     C* Variabili appoggio
372300950330     C*
372400950330     C                   Z-ADD     SFLPAG        WPAG
372500950330     C                   MOVEL     *OFF          WOPR
372600950515     C                   CLEAR                   W1POS
372700941104     C*
372800941104     C* definisco qual'è la prima videata
372900950410     C* (se la prima videata risulta la lista, devo comunque caricare
373000950410     C*  la videata di parzializzazione e gestire la definizione
373100950410     C*  dell'ordinamento, perchè, dai default oppure se sono stati
373200950410     C*  passati dei dati, può variare il caricamento)
373300950704     C* se la videata D1 dà errore, enmetto la parzializzazione anche
373400950704     C* se era memorizzata la lista
373500940223     C                   SELECT
373600940223     C     $LOP          WHENEQ    1
373700940223     C                   MOVE      'D1'          $GEST
373800940223     C     $LOP          WHENEQ    2
373900940223     C                   MOVE      'S1'          $GEST
374000941104     C                   EXSR      INZD1
374100941104     C                   MOVE      *OFF          $INZD1
374200950410     C                   EXSR      DEFORD
374300950704     C* Controlli da eseguire se prima videata è lista
374400950704     C                   EXSR      CTRD1
374500950707     C                   if        *IN99 = *on
374600950704     C                   MOVE      'D1'          $GEST
374700950704     C                   MOVE      *ON           $INZD1
374800950705     C                   EVAL      $LOP  = 1
374900950704     C                   endif
375000941104     C                   ENDSL
375100940223     C*
375200940223     C* Valorizzazione campi univoci testate
375300940223     C*
375400940223     C                   CLEAR                   FMTT1
375500940223     C                   MOVEL     KNSIF         NOMSIF
375600941109     C                   MOVEL     XSCDSI        NOMDIT
375700941027     C                   MOVEL     DSPGM         NOMPGM
375800980721     C*
375900950331     C* recupero il sottotitolo (se c'è)
376000950331     C                   MOVEL     *BLANK        T1TIT
376100950331     C     SOTTIT        IFNE      *BLANK
376200950331     C                   MOVEL     SOTTIT        IDMSG
376300950522     C                   CALLB     'XRTVM'
376400950331     C                   PARM                    IDMSG            27
376500950331     C                   PARM                    TXTMSG          100
376600950331     C                   PARM                    ERRMSG            1
376700950331     C                   PARM      *ON           CTRMSG            1
376800950331     C                   PARM      30            LENMSG            3 0
376900950331     C     ERRMSG        IFNE      *ON
377000950331     C                   MOVEL     TXTMSG        T1TIT
377100950331     C                   ELSE
377200950331     C                   MOVEL     *ALL'?'       T1TIT
377300950331     C                   ENDIF
377400950331     C                   ENDIF
377500940223     C*
377600940223     C                   ENDSR
377700941209     C/EJECT
377800941209     C************************************************************
377900941209     C* REPERIMENTO DATI SOCIETA'
378000941209     C************************************************************
378100941209     C     REPSOC        BEGSR
378200941209     C*
378300950522     C                   CALLB     'XSOC'
378400941209     C                   PARM                    TIPXSC            6
378500941209     C                   PARM                    SOCXSC            3
378600941209     C                   PARM                    CDSXSC            9 0
378700941209     C                   PARM                    MODXSC            3
378800941209     C                   PARM      *BLANKS       RTNXSC            1
378900941209     C                   PARM                    XSOCDS
379000941209     C                   PARM                    KPJBA
379100941209     C*
379200941209     C                   ENDSR
379300941124     C/EJECT
379400941124     C************************************************************
379500941124     C* REPERIMENTO AUTORIZZAZIONI UTENTE
379600941124     C************************************************************
379700941124     C     REPAUT        BEGSR
379800990812     C*
379900990812C1221C* asteriscate la clear e la valorizzazione di xcauni perchè
380000990812  "  C* spostate alla chiamata
380100990812C1221C****************** CLEAR                   AUTDS
380200941124     C*
380300941124     C                   MOVEL     XSCSOC        XCASOC
380400990812C1221C****************** MOVEL     XSCDUN        XCAUNI
380500941124     C                   Z-ADD     0             XCAGRP
380600941124     C                   MOVEL     KNMUS         XCAPRF
380700950704    >C                   MOVEL     'CONPPA'      XCAFNC
380800941124    >C                   MOVEL     *ZERO         XCAVFU
380900941124    >C                   MOVE      'CK'          XCATCK
381000941124    >C                   MOVE      '1'           XCANAU
381100941124     C*
381200950522     C                   CALLB     'XCHKAUT'
381300941124     C                   PARM                    AUTDS
381400941124     C*
381500941124     C* errori/non abilitazione
381600941124     C     XCARTN        IFNE      0
381700941124     C                   EXSR      ENDPGM
381800941124     C                   ELSE
381900950704    >C                   MOVEL     XCADDS        CONPPADS
382000941124     C                   ENDIF
382100941124     C*
382200941124     C                   ENDSR
382300940223     C/EJECT
382400940127     C************************************************************
382500940131     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
382600940127     C************************************************************
382700940131     C     REPDFT        BEGSR
382800940127     C*
382900950524     C* preparo i parametri di chiamata al driver
383000950524     C                   CLEAR                   XMPEDS
383100950524     C                   EVAL      TpChiamata = '01'
383200950524     C                   EVAL      NChiavi = 5
383300950524     C                   EVAL      VLogica = 2
383400950524     C*
383500950524     C* preparo le chiavi per la chiamata al driver
383600950524     C                   CLEAR                   XMPEDS1
383700950524     C                   MOVEL     XSCSOC        DSMPESOC
383800950524     C                   MOVEL     DSPGM         DSMPEPGM
383900950524     C                   MOVEL     KNMUS         DSMPEPRF
384000950524     C* Si reperiscono i dati di default a seconda delle modalità con
384100950524     C* cui il pgm è usato
384200950524     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
384300950524     C* MPETIP = 'PV'  pgm richiamato da menù
384400950524     C* MPETIP = 'PB'  x lancio Batch
384500941013     C                   SELECT
384600950703    >C     OPZ060        WHENEQ    '01'
384700950524     C                   MOVEL     'PX'          DSMPETIP
384800941013     C                   OTHER
384900950524     C                   MOVEL     'PV'          DSMPETIP
385000941013     C                   ENDSL
385100950524     C*
385200950524     C                   CALLB     'XMPE'
385300950524     C                   PARM                    XMPEDS
385400950524     C                   PARM                    XMPEDS1
385500950524     C*
385600950524     C     Risultato     IFEQ      '0'
385700950524     C                   MOVEL     DSMPEPAR      DSMPE
385800940131     C* nel caso sia la prima volta che l'utente apre questa anagrafica
385900940131     C* i suoi valori di default vengono decisi a pgm
386000940131     C                   ELSE
386100940201     C                   Z-ADD     1             $LOP
386200940131     C                   Z-ADD     1             $ORD
386300950704    >C*  elenco altri dati default specifici dell'anagrafica
386400941027     C                   ENDIF
386500940131     C*
386600940131     C                   ENDSR
386700940223     C/EJECT
386800940131     C************************************************************
386900940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
387000940131     C************************************************************
387100940225     C     DEFOPZ        BEGSR
387200940131     C*
387300940225     C* Questa routine deve valorizzare la schiera $FL1
387400940225     C* con S o N a seconda della utilizzabilità o meno
387500940225     C* dell'opzione correlata
387600940225     C*
387700950301     C* Inizialmente si considerano tutte le opzioni come valide
387800940317     C     1             DO        $DIMOP        X
387900940225     C                   MOVE      'S'           $FL1(X)
388000941027     C                   ENDDO
388100940715     C*
388200940715     C* Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
388300941018     C* con 1 o 0 a seconda della utilizzabilità o meno
388400940715     C* del tasto funzione correlato
388500940715     C*
388600940715     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
388700940715     C* di compilazione
388800940715     C                   RESET                   $FC1
388900940715     C                   RESET                   $FR1
389000940715     C                   RESET                   $FC2
389100940715     C                   RESET                   $FR2
389200940919     C                   RESET                   $FC3
389300940919     C                   RESET                   $FR3
389400940225     C*
389500940225     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
389600940225     C* invalido alcune opzioni
389700940225     C                   SELECT
389800940225     C* Se pgm richiamato in ricerca disattivo :
389900950703    >C     OPZ060        WHENEQ    '01'
390000940603     C* F3=Fine
390100940603     C                   Z-ADD     3             X
390200941216     C                   MOVE      '0'           $FC1(X)
390300941216     C                   MOVE      '0'           $FC2(X)
390400941216     C                   MOVE      '0'           $FC3(X)
390500950116     C*
390600950704    >C*  aggiungere nello stesso modo le altre opzioni da
390700950704    >C*  disattivare (quelle specifiche per ogni pgm)
390800950301     C*
390900940225     C* Se pgm richiamato in interrogazione disattivo :
391000950703    >C     OPZ060        WHENEQ    '05'
391100940307     C* F10=Immissione
391200940615     C                   Z-ADD     10            X
391300950301     C                   MOVE      '0'           $FC1(X)
391400950301     C                   MOVE      '0'           $FC2(X)
391500940225     C* 1=Selezione
391600940225     C                   Z-ADD     1             X
391700940225     C     01            LOOKUP    $OP1(X)                                21
391800940225     C     *IN21         IFEQ      *ON
391900940225     C                   MOVE      'N'           $FL1(X)
392000940225     C                   ENDIF
392100940225     C* 2=Modifica
392200940225     C                   Z-ADD     1             X
392300940225     C     02            LOOKUP    $OP1(X)                                21
392400940225     C     *IN21         IFEQ      *ON
392500940225     C                   MOVE      'N'           $FL1(X)
392600940225     C                   ENDIF
392700940225     C* 3=Copia
392800940225     C                   Z-ADD     1             X
392900940225     C     03            LOOKUP    $OP1(X)                                21
393000940225     C     *IN21         IFEQ      *ON
393100940225     C                   MOVE      'N'           $FL1(X)
393200940225     C                   ENDIF
393300940225     C* 4=Annullamento
393400940225     C                   Z-ADD     1             X
393500940225     C     04            LOOKUP    $OP1(X)                                21
393600940225     C     *IN21         IFEQ      *ON
393700940225     C                   MOVE      'N'           $FL1(X)
393800940225     C                   ENDIF
393900940225     C*
394000950703     C     OPZ060        WHENEQ    *BLANKS
394100940225     C* Se pgm non richiamato, disattivo subito :
394200941124     C* F8=Selezione seguente
394300941124     C                   Z-ADD     8             X
394400941206     C                   MOVE      '0'           $FC1(X)
394500941206     C                   MOVE      '0'           $FC2(X)
394600940225     C* 1=Selezione
394700940225     C                   Z-ADD     1             X
394800940225     C     01            LOOKUP    $OP1(X)                                21
394900940225     C     *IN21         IFEQ      *ON
395000940225     C                   MOVE      'N'           $FL1(X)
395100940225     C                   ENDIF
395200940225     C*
395300950301     C* In tutti gli altri casi, disattivo subito :
395400941228     C                   OTHER
395500941228     C* 1=Selezione
395600941228     C                   Z-ADD     1             X
395700941228     C     01            LOOKUP    $OP1(X)                                21
395800941228     C     *IN21         IFEQ      *ON
395900941228     C                   MOVE      'N'           $FL1(X)
396000941228     C                   ENDIF
396100941228     C*
396200940225     C                   ENDSL
396300950704     c*
396400950704     C* disattivo sempre la copia (NON E' possibile)
396500950704     C* 3=Copia
396600950704     C                   Z-ADD     1             X
396700950704     C     03            LOOKUP    $OP1(X)                                21
396800950704     C     *IN21         IFEQ      *ON
396900950704     C                   MOVE      'N'           $FL1(X)
397000950704     C                   ENDIF
397100941118     C*
397200941118     C* poi, disattivo a seconda delle autorizzazioni :
397300950704    >C*    AUTXXX        IFEQ      'N'
397400950704    >C*                  Z-ADD     1             X
397500950704    >C*    'XX'          LOOKUP    $OP1(X)                                21
397600950704    >C*    *IN21         IFEQ      *ON
397700950704    >C*                  MOVE      'N'           $FL1(X)
397800950704    >C*                  ENDIF
397900950704    >C*                  ENDIF
398000940225     C*
398100940225     C                   ENDSR
398200940225     C/EJECT
398300940225     C************************************************************
398400940225     C* REPERIMENTO OPZIONI
398500940225     C************************************************************
398600940225     C     REPOPZ        BEGSR
398700940225     C*
398800940314     C     1             DO        $DIMOP        X
398900940128     C*
399000940223     C     $MS1(X)       IFNE      *BLANK
399100940128     C*
399200940223     C                   MOVEL     $MS1(X)       IDMSG
399300950522     C                   CALLB     'XRTVM'
399400940201     C                   PARM                    IDMSG            27
399500940201     C                   PARM                    TXTMSG          100
399600940201     C                   PARM                    ERRMSG            1
399700940128     C*
399800940128     C     ERRMSG        IFNE      *ON
399900940223     C                   MOVEL     TXTMSG        $TX1(X)
400000940128     C                   ELSE
400100940223     C                   MOVEL     *ALL'?'       $TX1(X)
400200941027     C                   ENDIF
400300940128     C*
400400941027     C                   ENDIF
400500940128     C*
400600941027     C                   ENDDO
400700940128     C*
400800940128     C                   ENDSR
400900940506     C/EJECT
401000940506     C************************************************************
401100940506     C* REPERIMENTO TASTI DI FUNZIONE
401200940506     C************************************************************
401300940506     C     REPKEY        BEGSR
401400940506     C                   MOVEA     $FM1          $FM
401500950906     C                   MOVEA     $Fc1          $Fc
401600940506     C                   EXSR      REPKKK
401700940506     C                   MOVEA     $FX           $FX1
401800940506     C*
401900940506     C                   MOVEA     $FM2          $FM
402000950906     C                   MOVEA     $Fc2          $Fc
402100940506     C                   EXSR      REPKKK
402200940506     C                   MOVEA     $FX           $FX2
402300940919     C*
402400940919     C                   MOVEA     $FM3          $FM
402500950906     C                   MOVEA     $Fc3          $Fc
402600940919     C                   EXSR      REPKKK
402700940919     C                   MOVEA     $FX           $FX3
402800950907     C*
402900950907     C                   MOVEA     $FM4          $FM
403000950907     C                   MOVEA     $Fc4          $Fc
403100950907     C                   EXSR      REPKKK
403200950907     C                   MOVEA     $FX           $FX4
403300940919     C                   ENDSR
403400940506     C/EJECT
403500940506     C************************************************************
403600940506     C* REPERIMENTO TASTI DI FUNZIONE
403700940506     C************************************************************
403800940506     C     REPKKK        BEGSR
403900940506     C*
404000940506     C                   CLEAR                   $FX
404100940506     C     1             DO        24            X
404200940506     C*
404300940506     C     $FM(X)        IFNE      *BLANK
404400950906     C     $Fc(X)        andne     '0'
404500940506     C*
404600940506     C                   MOVEL     $FM(X)        IDMSG
404700950522     C                   CALLB     'XRTVM'
404800940506     C                   PARM                    IDMSG            27
404900940506     C                   PARM                    TXTMSG          100
405000940506     C                   PARM                    ERRMSG            1
405100940506     C*
405200940506     C     ERRMSG        IFNE      *ON
405300940506     C                   MOVEL     TXTMSG        $FX(X)
405400940506     C                   ELSE
405500940506     C                   MOVEL     *ALL' '       $FX(X)
405600941027     C                   ENDIF
405700940506     C*
405800941027     C                   ENDIF
405900940506     C*
406000941027     C                   ENDDO
406100940506     C*
406200940506     C                   ENDSR
406300940223     C/EJECT
406400940128     C************************************************************
406500950606     C* DEFINIZIONE KLIST
406600940128     C************************************************************
406700950606     C     DefKlist      BEGSR
406800940128     C*
406900940223     C* klist
407000940223     C*
407100950704    >C* ndppa01L
407200950906    >C     K04PPA01      KLIST
407300950704    >C                   KFLD                    PPASOCIETA
407400950906    >C                   KFLD                    PPActb
407500950704    >C                   KFLD                    PPAKCC
407600950704    >C                   KFLD                    PPAKSC
407700940127     C*
407800950906    >C     K07PPA01      KLIST
407900950704    >C                   KFLD                    PPASOCIETA
408000950906    >C                   KFLD                    PPActb
408100950704    >C                   KFLD                    PPAKCC
408200950704    >C                   KFLD                    PPAKSC
408300950704    >C                   KFLD                    PPADTPAR
408400950704    >C                   KFLD                    PPANRPAR
408500950704    >C                   KFLD                    PPASERPAR
408600950906    >C* x file correlati
408700950704    >C     K02PPA01      KLIST
408800950704    >C                   KFLD                    PPASYS
408900950704    >C                   KFLD                    PPANRASINT
409000950706    >C* ndpas02j
409100950706    >C     K07pas02      KLIST
409200950706    >C                   KFLD                    PasSocieta
409300950706    >C                   KFLD                    Pasctb
409400950706    >C                   KFLD                    Paskcc
409500950706    >C                   KFLD                    Pasksc
409600950706    >C                   KFLD                    Pasdtpar
409700950706    >C                   KFLD                    Pasnrpar
409800950706    >C                   KFLD                    Passerpar
409900950906    >C* anuni01L
410000950906    >C     K02uni01      KLIST
410100950906    >C                   KFLD                    uniSOCIETA
410200950906    >C                   KFLD                    uniunita
410300950704     C*
410400940117     C                   ENDSR
410500940223     C/EJECT
410501170223R027 C* Aggiunti elementi di schiera:
410502170223 "   C* SDGMSG    *LIBL     SDO0026  (schiera $MS1 )  e
410503170223R027 C* 22S      <Dati aggiuntivi    (schiera $OP1 )
410600941214**
410700940718PROMSG    *LIBL     OPZ0001
410800940225PROMSG    *LIBL     OPZ0002
410900940131PROMSG    *LIBL     OPZ0003
411000940131PROMSG    *LIBL     OPZ0004
411100940131PROMSG    *LIBL     OPZ0005
411200940831PROMSG    *LIBL     OPZ0010
411300940831PROMSG    *LIBL     OPZ0014
411301170223SDGMSG    *LIBL     SDO0026         R027
411400950704PROMSG    *LIBL     OPZ0111
411500950704PROMSG    *LIBL     OPZ0112
411600981218PROMSG    *LIBL     OPZ0322
411700950906PROMSG    *LIBL     OPZ0118
411800950906PROMSG    *LIBL     OPZ0119
411900940201**
41200094022501S      <Selezione
41210094022502S      <Modifica
41220094022503S      <Copia
41230094022504S      <Annullamento
41240094022505S      <Visualizzazione
41250094083110S      <Invio
41260094083114S      <Gestione commenti
41260117022322S      <Dati aggiuntivi           R027
41270095070432S      <Mix
41280095070433S      <Rate
41290098121834S      <Estensione nominativi
41300095090660S      <Blocca
41310095090661S      <Sblocca
413200940506** TASTI DI FUNZIONE  D1
413300940506PROMSG    *LIBL     KEY0001  01
413400940506PROMSG    *LIBL     KEY0002  02
413500940506PROMSG    *LIBL     KEY0003  03
413600940506PROMSG    *LIBL     KEY0004  04
413700940506PROMSG    *LIBL     KEY0005  05
413800940506PROMSG    *LIBL     KEY0006  06
413900940506PROMSG    *LIBL     KEY0007  07
414000940506PROMSG    *LIBL     KEY0008  08
414100940506PROMSG    *LIBL     KEY0009  09
414200940506PROMSG    *LIBL     KEY0010  10
414300940506PROMSG    *LIBL     KEY0011  11
414400940506PROMSG    *LIBL     KEY0012  12
414500940506PROMSG    *LIBL     KEY0013  13
414600940506PROMSG    *LIBL     KEY0014  14
414700940506PROMSG    *LIBL     KEY0015  15
414800940506PROMSG    *LIBL     KEY0016  16
414900940506PROMSG    *LIBL     KEY0017  17
415000940506PROMSG    *LIBL     KEY0018  18
415100940506PROMSG    *LIBL     KEY0019  19
415200940506PROMSG    *LIBL     KEY0020  20
415300940506PROMSG    *LIBL     KEY0021  21
415400940506PROMSG    *LIBL     KEY0022  22
415500940506PROMSG    *LIBL     KEY0023  23
415600940506PROMSG    *LIBL     KEY0024  24
415700940506**  ABILITAZIONE D1
4158009407200N  01
4159009507041N  02
4160009407201N  03
4161009407201N  04
4162009407200N  05
4163009407200N  06
4164009407200N  07
4165009412061N  08
4166009407200N  09
4167009507040N  10
4168009407200N  11
4169009407201N  12
4170009407200N  13
4171009407200N  14
4172009407200N  15
4173009407200N  16
4174009407200N  17
4175009407201N  18
4176009407200N  19
4177009407200N  20
4178009407200N  21
4179009407200N  22
4180009509130N  23   Impostare a '1' se si utilizzano opzioni
4181009509131N  24   Lasciare sempre impostata a '1' il valore della riga 24
418200940916** TASTI DI FUNZIONE  S1
418300940916PROMSG    *LIBL     KEY0001  01
418400940916PROMSG    *LIBL     KEY0002  02
418500940916PROMSG    *LIBL     KEY0003  03
418600940916PROMSG    *LIBL     KEY0004  04
418700940916PROMSG    *LIBL     KEY0005  05
418800940916PROMSG    *LIBL     KEY0006  06
418900940916PROMSG    *LIBL     KEY0007  07
419000940916PROMSG    *LIBL     KEY0008  08
419100940916PROMSG    *LIBL     KEY0009  09
419200940916PROMSG    *LIBL     KEY0010  10
419300940916PROMSG    *LIBL     KEY0011  11
419400940916PROMSG    *LIBL     KEY0012  12
419500940916PROMSG    *LIBL     KEY0013  13
419600940916PROMSG    *LIBL     KEY0014  14
419700960123PROMSG    *LIBL     KEY0053  15
419800940916PROMSG    *LIBL     KEY0016  16
419900940916PROMSG    *LIBL     KEY0017  17
420000940916PROMSG    *LIBL     KEY0018  18
420100940916PROMSG    *LIBL     KEY0019  19
420200940916PROMSG    *LIBL     KEY0020  20
420300940916PROMSG    *LIBL     KEY0021  21
420400940916PROMSG    *LIBL     KEY0022  22
420500940916PROMSG    *LIBL     KEY0023  23
420600940916PROMSG    *LIBL     KEY0024  24
420700940916**  ABILITAZIONE S1
4208009409160N  01
4209009409160N  02
4210009409161N  03
4211009409160N  04
4212009409160N  05
4213009409160N  06
4214009409160N  07
4215009411241N  08
4216009603131N  09
4217009507040N  10
4218009409160N  11
4219009409161N  12
4220009409160N  13
4221009409160N  14
4222009601231N  15
4223009409160N  16
4224009409161N  17
4225009409161N  18
4226009409160N  19
4227009409160N  20
4228009409160N  21
4229009409160N  22
4230009509131N  23   Impostare a '1' se si utilizzano opzioni
4231009509131N  24   Lasciare sempre impostata a '1' il valore della riga 24
423200940916** TASTI DI FUNZIONE  D3
423300940506PROMSG    *LIBL     KEY0001  01
423400940506PROMSG    *LIBL     KEY0002  02
423500940506PROMSG    *LIBL     KEY0003  03
423600940506PROMSG    *LIBL     KEY0004  04
423700940506PROMSG    *LIBL     KEY0005  05
423800940506PROMSG    *LIBL     KEY0006  06
423900940506PROMSG    *LIBL     KEY0007  07
424000940506PROMSG    *LIBL     KEY0008  08
424100940506PROMSG    *LIBL     KEY0009  09
424200940506PROMSG    *LIBL     KEY0010  10
424300940506PROMSG    *LIBL     KEY0011  11
424400940506PROMSG    *LIBL     KEY0012  12
424500940506PROMSG    *LIBL     KEY0013  13
424600940506PROMSG    *LIBL     KEY0014  14
424700940506PROMSG    *LIBL     KEY0015  15
424800940506PROMSG    *LIBL     KEY0016  16
424900940506PROMSG    *LIBL     KEY0017  17
425000940506PROMSG    *LIBL     KEY0018  18
425100940506PROMSG    *LIBL     KEY0019  19
425200940506PROMSG    *LIBL     KEY0020  20
425300940506PROMSG    *LIBL     KEY0021  21
425400940506PROMSG    *LIBL     KEY0022  22
425500940506PROMSG    *LIBL     KEY0023  23
425600940506PROMSG    *LIBL     KEY0024  24
425700940916**  ABILITAZIONE D3
4258009407200N  01
4259009407200N  02
4260009407201N  03
4261009407200N  04
4262009407200N  05
4263009407200N  06
4264009407200N  07
4265009407200N  08
4266009407200N  09
4267009409160N  10
4268009407200N  11
4269009407201N  12
4270009407200N  13
4271009407200N  14
4272009407200N  15
4273009407200N  16
4274009409160N  17
4275009409160N  18
4276009407200N  19
4277009407200N  20
4278009407200N  21
4279009407200N  22
4280009509130N  23   Impostare a '1' se si utilizzano opzioni
4281009509131N  24   Lasciare sempre impostata a '1' il valore della riga 24
428200950907** TASTI DI FUNZIONE  D4
428300950907PROMSG    *LIBL     KEY0001  01
428400950907PROMSG    *LIBL     KEY0002  02
428500950907PROMSG    *LIBL     KEY0003  03
428600950907PROMSG    *LIBL     KEY0004  04
428700950907PROMSG    *LIBL     KEY0005  05
428800950907PROMSG    *LIBL     KEY0006  06
428900950907PROMSG    *LIBL     KEY0007  07
429000950907PROMSG    *LIBL     KEY0008  08
429100950907PROMSG    *LIBL     KEY0009  09
429200950907PROMSG    *LIBL     KEY0010  10
429300950907PROMSG    *LIBL     KEY0011  11
429400950907PROMSG    *LIBL     KEY0012  12
429500950907PROMSG    *LIBL     KEY0013  13
429600950907PROMSG    *LIBL     KEY0014  14
429700950907PROMSG    *LIBL     KEY0015  15
429800950907PROMSG    *LIBL     KEY0016  16
429900950907PROMSG    *LIBL     KEY0017  17
430000950907PROMSG    *LIBL     KEY0018  18
430100950907PROMSG    *LIBL     KEY0019  19
430200950907PROMSG    *LIBL     KEY0020  20
430300950907PROMSG    *LIBL     KEY0021  21
430400950907PROMSG    *LIBL     KEY0022  22
430500950907PROMSG    *LIBL     KEY0023  23
430600950907PROMSG    *LIBL     KEY0024  24
430700950907**  ABILITAZIONE D4
4308009509070N  01
4309009509070N  02
4310009509132N  03
4311009509071N  04
4312009509070N  05
4313009509071N  06
4314009509070N  07
4315009509070N  08
4316009509070N  09
4317009509070N  10
4318009509070N  11
4319009509132N  12
4320009509070N  13
4321009509070N  14
4322009509070N  15
4323009509070N  16
4324009509070N  17
4325009509070N  18
4326009509070N  19
4327009509070N  20
4328009509070N  21
4329009509070N  22
4330009509130N  23   Impostare a '1' se si utilizzano opzioni
4331009509131N  24   Lasciare sempre impostata a '1' il valore della riga 24
