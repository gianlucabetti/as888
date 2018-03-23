000100150909>>>>>H DFTACTGRP(*NO) BNDDIR('PJXBND') ACTGRP('QILE')
000200940223     H DECEDIT('0,') DATEDIT(*DMY.)
000300940127      *----------*
000400950322      * ANA002R  *
000500940127      *----------*
000600941012      *
000700010307      *            MANUTENZIONE ANAGRAFICA PROFILI
000800940223      *
000900940223      *
001000941013      * DOCUMENTI MICROANALISI :  NSDAMUS/ANALISI/PROF 001.DOC
001100940223      *
001200940223      * NOTE TECNICHE :
001300940223      *
001400940223      *
001500940223      * PERSONALIZZAZIONI
001600940223      *
001700940223      *
001800940223      *
001900940223      *               MAPPA INDICATORI STD
002000940223      *
002100940223      *  02           PROTECT CAMPI CHIAVE SE OPZ=MODIFICA
002200940223      *  21           GENERICO OPERAZIONI I/O
002300940223      *  22           GENERICO ERRORE OPERAZIONI I/O
002400940223      *  30           SFLDSP
002500940223      * N31           SFLCLR
002600940128      *  31           SFLDSPCTL
002700940128      *  32           SFLNXTCHG
002800940128      *  33           SFLEND
002900940128      *  39           OF PRTF
003000940315      *  40 <---> 49  DSPATR ERRORI SU SFL
003100950322      *  Specificare l'uso dei singoli indicatori
003200941104      *  50 <---> 56  ERRORI SU VIDEO  USATI
003300941104      *  56           NON E' POSSIBILE REPERIRE UN NUOVO CODICE UTENTE  PRO0043
003400941104      *  57 <---> 98  ERRORI SU VIDEO  DISPONIBILI
003500940317    > *  Specificare l'uso dei singoli indicatori
003600940510      *  96           ERRORE SPECIALE : ERRORI IN ALTRE VIDEATE
003700940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
003800940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
003900940128      *  99           INDIC. GENERALE DI ERRORE
004000941006      *----------------------------------------------------*
004100941012    >FANUTN01L  UF A E           K DISK
004200941104    >FANUTN02L  IF   E           K DISK
004300941104     F                                     RENAME(ANUTN000:ANUTN02)
004400941012    >FANA002D   CF   E             WORKSTN
004500941012     F                                     RENAME(A002D1:FMTD1)
004600941006     F                                     RENAME(A002T1:FMTT1)
004700941006     F                                     RENAME(A002Z1:FMTZ1)
004800941006    > * eventuali schermi supplementari
004900940201     F                                     INFDS(DSFMT)
005000941006      *----------------------------------------------------*
005100950426     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
005200950426     D $ID             S             27    DIM(5) ALT($OZ)
005300941006      * Schiere per la gestione dei tasti di funzione
005400940506     D $FM             S             27    DIM(24)
005500940506     D $FX             S             50    DIM(24)
005600940506     D $FC             S              1    DIM(24)
005700940506     D $FR             S              1    DIM(24)
005800941006      * Video D1
005900940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
006000940506     D $FX1            S             50    DIM(24) ALT($FM1)
006100940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
006200940506     D $FR1            S              1    DIM(24) ALT($FC1)
006300941006      *----------------------------------------------------*
006400031112B0644D Cd              S              1    DIM(26) CTDATA PERRCD(26)
006500941006      *-------------
006600941006      * Passaggio Parametri
006700940127     D KPJBA         E DS
006800941006      *-------------
006900941006      * DS Esterna gestione messaggi per COMIT
007000940318     D XMSGDS        E DS
007100940318     D  STS          E                     EXTFLD(MSGSTS)
007200940721     D                                     DIM(333)
007300940318     D  JBA          E                     EXTFLD(MSGJBA)
007400940721     D                                     DIM(502)
007500941006      *-------------
007600941006      * Passaggio Parametri al PGM XATB
007700941007     D XATBDS        E DS                  INZ
007800941006      * Passaggio Parametri al PGM di manutenzione
007900941006    >D A002DS        E DS                  EXTNAME(ANA002DS)
008000950322      *DS per passaggio dati pgm User Exit
008100950322     D XUKDS         E DS                  EXTNAME(XUSRKEYDS)
008200021008A0970D*-------------
008300021008  "  D* Ricerca legende
008400021008  "  D A964DS        E DS                  EXTNAME(ANA964DS)
008500021008  "  D* Controlli validità legende
008600021008A0970D X07DS         E DS                  EXTNAME(X07VALDS)
008700941006    > *-----------------------------------------------------------
008800941006    > * inserire tutte le altre DS di passaggio parametri
008900941006    > *-----------------------------------------------------------
009000941006      *-------------
009100940201     D DSFMT           DS
009200940506     D  $TASTO               369    369
009300940201     D  NRG                  370    370
009400940201     D  NCL                  371    371
009500941006      *-------------
009600941006      * posizione cursore
009700940201     D CURSOR          DS
009800940223     D  RIRI                   1      2B 0 INZ
009900940201     D  R$$                    2      2
010000940223     D  COCO                   3      4B 0 INZ
010100940201     D  C$$                    4      4
010200941006      *-------------
010300941006      * Reperimento nome PGM
010400940207     D STATUS         SDS           333
010500940207     D  DSPGM            *PROC
010600941006      *-------------
010700941006      * Costanti
010800941006      *-------------
010900941006      * Tasti di funzione
011000940506     D F01             C                   CONST(X'31')
011100940506     D F02             C                   CONST(X'32')
011200940506     D F03             C                   CONST(X'33')
011300940506     D F04             C                   CONST(X'34')
011400940506     D F05             C                   CONST(X'35')
011500940506     D F06             C                   CONST(X'36')
011600940506     D F07             C                   CONST(X'37')
011700940506     D F08             C                   CONST(X'38')
011800940506     D F09             C                   CONST(X'39')
011900940506     D F10             C                   CONST(X'3A')
012000940506     D F11             C                   CONST(X'3B')
012100940506     D F12             C                   CONST(X'3C')
012200940506     D F13             C                   CONST(X'B1')
012300940506     D F14             C                   CONST(X'B2')
012400940506     D F15             C                   CONST(X'B3')
012500940506     D F16             C                   CONST(X'B4')
012600940506     D F17             C                   CONST(X'B5')
012700940506     D F18             C                   CONST(X'B6')
012800940506     D F19             C                   CONST(X'B7')
012900940506     D F20             C                   CONST(X'B8')
013000940506     D F21             C                   CONST(X'B9')
013100940506     D F22             C                   CONST(X'BA')
013200940506     D F23             C                   CONST(X'BB')
013300940506     D F24             C                   CONST(X'BC')
013400940506     D ENTER           C                   CONST(X'F1')
013500940506     D ROLDWN          C                   CONST(X'F4')
013600940506     D ROLLUP          C                   CONST(X'F5')
013700950426     D*-------------
013800950426     D* Sottotitolo di testata
013900950426     D SOTTIT          C                   CONST('                    ')
014000941012      *
014100941012      * Parametri per ricerca codice ANATB00F
014200941012     D XTABDS          DS                  INZ
014300941012      * '  ' = Gestione
014400941012      * '01' = Ricerca
014500941012      * '02' = Gestione
014600941012      * '05' = Visualizzazione
014700941012      * '11' = Abilitazione
014800941012     D  OPZTAB                 1      2
014900941012      * 0 = ritorno normale
015000941012      * 1 = F3
015100941012      * 2 = F12
015200941012     D  RETTAB                 3      3
015300941012      * operazione eseguite dal pgm chiamato :
015400941012      * 0 = nessuna operazione
015500941012      * 1 = eseguito aggiornamento
015600941012     D  OPRTAB                 4      4
015700941012      * funzione non eseguibile per errore :
015800941012      * 0 = nessun errore
015900941012      * 1 = funzione richiamata chiusa in errore
016000941012      * Aggiungere eventuali altri codici di errore
016100941012     D  ERRTAB                 5      5
016200941012      * Azienda
016300941012     D  AZITAB                 6      8
016400941012      * Lingua
016500941012     D  LINTAB                 9      9
016600941012      * Codice scelto
016700941012     D  KEYTAB                10     24
016800941012      * riga cursore chiamante
016900941012     D  RIGTAB                25     26  0
017000941012      * colonna cursore chiamante
017100941012     D  COLTAB                27     28  0
017200020701      * posizionamento SFL
017300020701E1199D  POSTAB                29     30  0
017400941012     D ERRORE          C                   CONST('Codice errato')
017500941012     D DATA            C                   CONST('0001-01-01')
017600941107      *-------------
017700941107      * Dati di ambiente ottenuti da XSOC
017800941103     D SOC001        E DS                  EXTNAME(XSOC001DS)
017900020403A0910D    XscLaguna                   1    overlay(xscSkp:8)                    Scadenz. retroattivo
018000031112B0644D    XscGest                     1    overlay(xscSkp:3)
018100941103      * DS Interna per dati di output di XSOC
018200941103     D XSOCDS          DS          1000
018300010206A0860D*-------------
018400010206  "  D* Ricerca soggetti
018500010206  "  D ANA040DS      E DS                  inz
018600010206  "  D*-------------
018700010206  "  D* Ricerca altri indirizzi
018800010206  "  D ANA045DS      E DS                  inz
018900010208  "  D*-------------
019000010208  "  D* Ricerca contatti x soggetto
019100010208  "  D ANA060DS      E DS                  inz
019200010206  "  D*-------------
019300010206  "  D* DS esterna per decodifica soggetto
019400010206  "  D DVASOG        E DS                  INZ
019500010206  "  D* Definizioni parametri driver anagrafici
019600010206  "  DDvaric           S              1
019700010206  "  DDvaSocieta       S              3
019800010206  "  DDvaUnita         S              8
019900010206  "  DDvaStrutt        S             10
020000010206  "  DDvaDtRif         S               D
020100010206  "  DDvaLenOut        S              9B 0
020200010206  "  DDvaSnatura       S              1
020300010206  "  DDvaCodice        S              8
020400010206  "  DDvaLineav        S              3
020500010206  "  DDvaFiliale       S              3  0
020600010206  "  DDvaSogg          S              8
020700010206  "  DDvaTpInd         S              2
020800010206  "  DDvaCdInd         S              3
020900010206  "  DDvaErrore        S              1
021000010206  "  DDvaOutput        S           4000
021100010207  "  D*-------------
021200010207  "  D* PROTOTIPAZIONE DELLE PROCEDURE
021300010320  "  D/COPY *LIBL/SRCCPY,PJDR12PR
021400010208  "  D*-------------
021500010208  "  D Dati            S           4000
021600010207A0860D*-------------
021700020403A0910D* Chiamata a Laguna
021800021120A0910D Cmd670          S             15
021900021120A0979x*Parm670         S             30    dim(30)
022000071018A1244x*Parm670         S             44    dim(30)  ex-A0979
022100071018A1244D Parm670         S             44    dim(70)
022200021120A0910D Err670          S              1
022300020403  "  D Ret670          S             32A
022400020403A0910D*-------------
022500030428G1286 * Parametri XPAR
022600030428  "  D XPaSoc          S              3A
022700030428  "  D XPaPar          S              8A
022800030428  "  D XPaRic          S              1A
022900030428  "  D XPaErr          S              1A
023000030428  "  D XPaOut          DS          2000
023100030428  "   *-------------
023200030428  "   * Dati p.m. setup generale
023300030428  "  D SICOM         E DS                  Inz
023400030428  "   *-------------
023500030428  "   * Parametri pgm allineamento dati GesLot
023600030428  "  D ANUTNDS       E DS                  ExtName(ANUTN00F)
023700030428  "  D OpzGLA001       S              2A
023800030428  "  D ErrGLA001       S              1A
023900030428G1286 *-------------
024000150909>>>>> /COPY GAITRASRC/SRCPROTOPR,QUSROBJD
024001150909>>>>> /COPY QSYSINC/QRPGLESRC,QUSROBJD
024002150909>>>>> /COPY QSYSINC/QRPGLESRC,QUSEC
024003150909      *----------------------------------------------------*
024100941107      *                                                    *
024200941107      *                                                    *
024300941006      *----------------------------------------------------*
024400941006      *                MAIN LINE PROGRAM
024500941006      *----------------------------------------------------*
024600941006      *
024700941006      * inizializzazione variabili
024800940223     C                   EXSR      INZVAR
024900941006      *
025000940223     C     $FINE         DOWEQ     *OFF
025100940202     C     $GEST         CASEQ     'D1'          GESD1
025200940117     C                   END
025300940117     C                   END
025400941006      *
025500941006      * fine programma
025600940325     C                   EXSR      ENDPGM
025700940325     C/EJECT
025800941006      ************************************************************
025900941107      * RICERCHE                                                 *
026000941006      ************************************************************
026100940325     C     ENDPGM        BEGSR
026200941006      *
026300941006      * disallocazione rcd nel caso di modifica/annullamento
026400940722    >C     OPZ002        IFEQ      '02'
026500940722    >C     OPZ002        OREQ      '04'
026600941006    >C                   UNLOCK    ANUTN01L                             22
026700940325     C                   END
026800941006      *
026900941006      * passaggio dati al pgm chiamante
027000941006    >C                   MOVEL     A002DS        KPJBU
027100941006      *
027200940325     C                   SETON                                            RT
027300940616     C                   RETURN
027400941006      *
027500940325     C                   ENDSR
027600940325     C/EJECT
027700941006      ************************************************************
027800941107      * RICERCHE                                                 *
027900941006      ************************************************************
028000940127     C     SEARCH        BEGSR
028100941006      *
028200941006      * determino Riga/Colonna del cursore
028300940127     C                   MOVE      NRG           R$$
028400940127     C                   MOVE      NCL           C$$
028500940127     C                   Z-ADD     RIRI          CURR
028600940127     C                   Z-ADD     COCO          CURC
028700941006      *
028800941006      * I campi che al momento dell'F4 risultano protetti non devono
028900941006      * essere abilitati alla funzione di ricerca
029000941006      *
0291009410071    C                   SELECT
029200941006      *
029300941006      * FMTD1  - PACCHETTO
029400941107    >C     H1NMFL        WHENEQ    'D1GRP'
029500941012     C                   MOVE      *ZERO         WWGRP
029600941012     C                   MOVE      WWGRP         KEYTAB
029700941012     C                   MOVE      '01'          OPZTAB
029800941103     C                   MOVE      XSCSOC        AZITAB
029900941103     C                   MOVE      XSCLIN        LINTAB
030000941012     C                   CLEAR                   KPJBU
030100941012     C                   MOVEL     XTABDS        KPJBU
030200941012     C                   CALL      'ANGG22R'
030300941012     C                   PARM                    KPJBA
030400941012     C                   MOVEL     KPJBU         XTABDS
030500941012     C                   MOVE      KEYTAB        WWGRP
030600941107      *
0307009410122   >C     WWGRP         IFNE      *ZEROS
030800941012    >C                   MOVEL     WWGRP         D1GRP
030900941012    >C                   MOVEL     *BLANK        D1DGRP
0310009410122-  >C                   ENDIF
031100941006      * imposto pos. del cursore
031200941007     C                   Z-ADD     CURR          H1RIGA
031300941007     C                   Z-ADD     CURC          H1COLO
031400941012      *
031500941012    >C     H1NMFL        WHENEQ    'D1UFF'
031600941012     C                   MOVE      *BLANK        WWUFF
031700941012     C                   MOVE      WWUFF         KEYTAB
031800941012     C                   MOVE      '01'          OPZTAB
031900941103     C                   MOVE      XSCSOC        AZITAB
032000941103     C                   MOVE      XSCLIN        LINTAB
032100941012     C                   CLEAR                   KPJBU
032200941012     C                   MOVEL     XTABDS        KPJBU
032300941012     C                   CALL      'ANGG01R'
032400941012     C                   PARM                    KPJBA
032500941012     C                   MOVEL     KPJBU         XTABDS
032600941012     C                   MOVE      KEYTAB        WWUFF
032700941107      *
0328009410122   >C     WWUFF         IFNE      *ZERO
032900941012    >C                   MOVEL     WWUFF         D1UFF
033000941012    >C                   MOVEL     *BLANK        D1DUFF
0331009410122-  >C                   ENDIF
033200941201      * imposto pos. del cursore
033300941201     C                   Z-ADD     CURR          H1RIGA
033400941201     C                   Z-ADD     CURC          H1COLO
033500941012      *
033600941012    >C     H1NMFL        WHENEQ    'D1LIN'
033700941012     C                   MOVE      *BLANK        WWLIN
033800941012     C                   MOVE      WWLIN         KEYTAB
033900941012     C                   MOVE      '01'          OPZTAB
034000941103     C                   MOVE      XSCSOC        AZITAB
034100941103     C                   MOVE      XSCLIN        LINTAB
034200941012     C                   CLEAR                   KPJBU
034300941012     C                   MOVEL     XTABDS        KPJBU
034400941012     C                   CALL      'ANGG15R'
034500941012     C                   PARM                    KPJBA
034600941012     C                   MOVEL     KPJBU         XTABDS
034700941012     C                   MOVE      KEYTAB        WWLIN
034800941012     C*
0349009410122   >C     WWLIN         IFNE      *BLANKS
035000941012    >C                   MOVEL     WWLIN         D1LIN
035100941012    >C                   MOVEL     *BLANK        D1DLIN
0352009410122-  >C                   ENDIF
035300941012      * imposto pos. del cursore
035400941012     C                   Z-ADD     CURR          H1RIGA
035500941012     C                   Z-ADD     CURC          H1COLO
035600010206A0860C*
035700010206  "  C* FMTD1  - SOGGETTO
035800010206  "  C     H1NMFL        WHENEQ    'D1SOGG'
035900010206  "  C                   RESET                   ANA040DS
036000010206  "  C                   MOVEL     '01'          OPZ040
036100010206  "  C                   MOVEL     ANA040DS      KPJBU
036200010206  "  C                   CALL      'ANA040R'
036300010206  "  C                   PARM                    KPJBA
036400010206  "  C                   MOVEL     KPJBU         ANA040DS
036500010206  "  C*
036600010206  "  C     COD040        IFNE      *BLANK
036700010206  "  C                   MOVEL     COD040        D1SOGG
036800010206  "  C                   ENDIF
036900010206  "  C* imposto pos. del cursore
037000010206  "  C                   Z-ADD     CURR          H1RIGA
037100010206  "  C                   Z-ADD     CURC          H1COLO
037200010206  "  C*
037300010206  "  C* FMTD1  - TIPO INDIRIZZO
037400010206  "  C     H1NMFL        WHENEQ    'D1TPIND'
037500010206  "  C                   RESET                   ANA045DS
037600010206  "  C                   MOVEL     '01'          OPZ045
037700010206  "  C                   CALLB     'XALLINEA'
037800010206  "  C                   PARM                    D1Sogg
037900010206  "  C                   PARM      8             LEN               5 0
038000010206  "  C                   PARM                    XSCINP
038100010206  "  C                   MOVEL     D1SOGG        COD045
038200010206  "  C                   MOVEL     ANA045DS      KPJBU
038300010206  "  C                   CALL      'ANA045R'
038400010206  "  C                   PARM                    KPJBA
038500010206  "  C                   MOVEL     KPJBU         ANA045DS
038600010206  "  C*
038700010206  "  C     TII045        IFNE      *BLANK
038800010206  "  C                   MOVEL     TII045        D1TpInd
038900010206  "  C                   MOVEL     COI045        D1CdInd
039000010206  "  C                   ENDIF
039100010206  "  C* imposto pos. del cursore
039200010206  "  C                   Z-ADD     CURR          H1RIGA
039300010206  "  C                   Z-ADD     CURC          H1COLO
039400010208  "  C*
039500010208  "  C* FMTD1  - TIPO CONTATTO
039600010208  "  C     H1NMFL        WHENEQ    'D1TPCNT'
039700010208  "  C                   RESET                   ANA060DS
039800010208  "  C                   MOVEL     '01'          OPZ060
039900010208  "  C                   MOVEL     '0'           CMT060
040000010208  "  C                   CALLB     'XALLINEA'
040100010208  "  C                   PARM                    D1Sogg
040200010208  "  C                   PARM      8             LEN               5 0
040300010208  "  C                   PARM                    XSCINP
040400010208  "  C                   MOVEL     D1Sogg        Sog060
040500010208  "  C                   MOVEL     D1TpInd       TPI060
040600010208  "  C                   MOVEL     D1CdInd       CDI060
040700010208  "  C                   MOVEL     ANA060DS      KPJBU
040800010208  "  C                   CALL      'ANA060R'
040900010208  "  C                   PARM                    KPJBA
041000010208  "  C                   MOVEL     KPJBU         ANA060DS
041100010208  "  C*
041200010208  "  C     TPC060        IFNE      *BLANK
041300010208  "  C                   MOVEL     TPC060        D1TpCnt
041400010213  "  C                   MOVEL     CdCnt060      D1CdCnt
041500010208  "  C                   ENDIF
041600010208  "  C* imposto pos. del cursore
041700010208  "  C                   Z-ADD     CURR          H1RIGA
041800010208  "  C                   Z-ADD     CURC          H1COLO
041900010208A0860C*
042000061109B0905C*
042100061109B0905C* sostituisce le specifiche siglate A0907
042200021008  "  C* FMTD1  - Notifica se WEB
042300061109  "  C* FMTD1  - Notifica preferenziale
042400021008  "  C     H1NMFL        WHENEQ    'D1FLG3'
042500021008  "  C                   RESET                   A964DS
042600021008  "  C                   MOVEL     '01'          OPZ964
042700061109  "  x***                MOVEL     *on           ALL964
042800061109  "  x***                MOVEL     'ANUTN000'    TRC964
042900061109  "  x***                MOVEL     'UTNFLG3'     CAM964
043000061109  "  C                   MOVEL     *off          ALL964
043100061109  "  C                   MOVEL     'JCMIT000'    TRC964
043200061109  "  C                   MOVEL     'MITNOTIFY'   CAM964
043300021008  "  C                   MOVEL     A964DS        KPJBU
043400021008  "  C                   CALL      'ANA964R'
043500021008  "  C                   PARM                    KPJBA
043600021008  "  C                   MOVEL     KPJBU         A964DS
043700021008  "  C*
043800021008  "  C     VAL964        IFNE      *BLANK
043900021008  "  C                   MOVEL     VAL964        D1Flg3
044000021008  "  C                   MOVEL     DES964        D1Flg3D
044100021008  "  C                   ENDIF
044200061109B0905C*
044300941006      * ricerca non abilitata in questa posizione
0444009410071O   C                   OTHER
044500941006      * imposto pos. del cursore
044600941007     C                   Z-ADD     CURR          H1RIGA
044700941007     C                   Z-ADD     CURC          H1COLO
044800941007     C                   SETON                                        9899
044900941007      *
0450009410071-   C                   ENDSL
045100941006      *
045200940506     C                   MOVE      *HIVAL        $LASTV
045300940127     C                   ENDSR
045400940225     C/EJECT
045500941006      ************************************************************
045600941107      * GESTIONE EMISSIONE TASTI FUNZIONE                        *
045700941006      ************************************************************
045800940225     C     GESPIE        BEGSR
045900941006      *
046000940225     C                   SELECT
046100941006      *
046200940506     C     $GEST         WHENEQ    'D1'
046300940506     C     $ULKD1        IFEQ      0
046400941006      * riempimento opzioni
046500940506     C                   EXSR      F24D1
046600940506     C                   ELSE
046700940506     C                   WRITE     FMTZ1
046800940506     C                   ENDIF
046900941006      *
047000940225     C                   ENDSL
047100941006      *
047200940225     C                   ENDSR
047300940131     C/EJECT
047400941006      ************************************************************
047500941006      * GESTIONE VIDEO RECORD D1
047600941006      ************************************************************
047700940127     C     GESD1         BEGSR
047800941006      * inizializzazione videata
047900940223     C     $INZD1        IFEQ      *ON
048000940127     C                   EXSR      INZD1
048100940223     C                   MOVE      *OFF          $INZD1
048200940117     C                   END
048300941006      * emissione piede videata se proveniente da altra
048400941006      * salvataggio valore formato di provenienza
048500940207     C     $LASTV        IFNE      'D1'
048600940207     C                   WRITE     FMTT1
048700940225     C                   EXSR      GESPIE
048800940202     C                   MOVE      $LASTV        $LASTG
048900940202     C                   MOVE      'D1'          $LASTV
049000940707     C                   ENDIF
049100941006      *
049200941006      * se esistono errori sulla videata
049300941006      * emetto la write del formato a indicatori spenti per vedere
049400941006      * le eventuali decodifiche
049500941006      *
049600940707     C     *IN99         IFEQ      *ON
049700940707     C     $GEST         ANDEQ     'D1'
049800940707     C                   MOVEA     *IN           WIN
049900940707     C                   MOVE      *ALL'0'       IN5098           49
050000940707     C                   MOVEA     IN5098        *IN(50)
050100940707     C                   WRITE     FMTD1
050200940707     C                   MOVEA     WIN           *IN
050300940707     C                   ENDIF
050400941006      *
050500940203     C                   SELECT
050600941006      * Annullamento
050700940722    >C     OPZ002        WHENEQ    '04'
050800941006      * Visualizzazione
050900940722    >C     OPZ002        OREQ      '05'
051000940203     C                   WRITE     FMTD1
051100940203     C                   EXFMT     PROTECT
051200941006      * Immissione/Modifica/Copia
051300940203     C                   OTHER
051400940203     C                   EXFMT     FMTD1
051500940203     C                   ENDSL
051600941006      * controllo abilitazione tasti funzione
051700941006      *
051800940506     C                   EXSR      CTRKD1
051900941006      *
052000940506     C     *IN99         IFEQ      *OFF
052100941006      *
0522009402031    C                   SELECT
052300941006      * F2=Decodifica
052400941006     C     $TASTO        WHENEQ    F02
052500940916     C                   EXSR      F02D1
052600941006      * F3=Fine
052700940506     C     $TASTO        WHENEQ    F03
052800940309     C                   EXSR      F03D1
052900941006      * F4=Ricerca
053000940506     C     $TASTO        WHENEQ    F04
053100940127     C                   EXSR      SEARCH
053200941006      * F5=Refresh
053300940506     C     $TASTO        WHENEQ    F05
053400940309     C                   EXSR      F05D1
053500941006      * F8=Successivo
053600940506     C     $TASTO        WHENEQ    F08
053700940309     C                   EXSR      F08D1
053800941006      * F12=Ritorno
053900940506     C     $TASTO        WHENEQ    F12
054000940309     C                   EXSR      F12D1
054100941006      * F24=Altri tasti
054200940506     C     $TASTO        WHENEQ    F24
054300940315     C                   EXSR      F24D1
054400941006      *
0545009402031O   C                   OTHER
054600940127     C                   EXSR      CTRD1
054700941006      * aggiungere le chiamate ai controlli delle videate supplementari
054800010206    >x****       *IN99     IFEQ *ON
054900010206    >x**++                 SETON                         96
055000010206    >x*****                ENDIF
055100010206    >x****                 ENDIF
055200941006      *
0553009405062    C     $TASTO        IFEQ      F06
055400940207     C     *IN99         ANDEQ     *OFF
055500941006      * eseguo aggiornamento
055600941006    >C                   EXSR      AGGUTN
055700941006      * eseguo operazioni del dopo-aggiornamento
0558009403183    C     *IN99         IFEQ      *OFF
055900940318     C                   EXSR      GESAGG
0560009403183-   C                   ENDIF
0561009402242-   C                   ENDIF
056200941006      *
0563009402241-   C                   ENDSL
056400941006      *
0565009405062-   C                   ENDIF
056600940117     C                   ENDSR
056700940506     C/EJECT
056800941006      ************************************************************
056900941006      * CONTROLLO ABILITAZIONE TASTI VIDEATA D1
057000941006      ************************************************************
057100940506     C     CTRKD1        BEGSR
057200940506     C                   SETOFF                                       99
057300940506     C                   MOVEA     $FC1          $FC
057400940506     C                   EXSR      CTRKEY
057500940506     C     *IN99         IFEQ      *ON
057600940506     C                   SETON                                        97
057700940506     C                   ENDIF
057800940506     C                   ENDSR
057900940117      ************************************************************
058000940207      * INIZIALIZZAZIONE VIDEATA DATI
058100940117      ************************************************************
058200940127     C     INZD1         BEGSR
058300941006      *
058400940131     C                   CLEAR                   FMTD1
058500941006      *
058600940225     C                   Z-ADD     1             $PIED1
058700020403A0910C                   EVAL      D1On1 = xscon
058800020403  "  C                   EVAL      D1Off1 = xscoff
058900020403  "  C                   SETOFF                                       04
059000020403  "  C                   IF        xscLaguna = *on
059100020403  "  C                   SETON                                        04
059200020403A0910C                   ENDIF
059300021008A0970C                   EVAL      D1On2 = xscon
059400021008A0970C                   EVAL      D1Off2 = xscoff
059500941006      *
059600941006      * valorizzo la chiave univoca per reperire i dati
059700941006      * se non immissione
0598009407221   >C     OPZ002        IFNE      '01'
059900941006    > *-------------------------------------------------------------
060000941006    > * impostare i campi chiave ricevuti
060100941006    > *-------------------------------------------------------------
060200941006    >C                   MOVEL     PRF002        UTNPRF
060300941006      *
0604009403182    C     *IN22         DOUEQ     *OFF
060500940318     C     MSGRTN        ORNE      'R'
060600941006      * se Modifica/Annullamento, il rcd che leggo viene bloccato
0607009407223   >C     OPZ002        IFEQ      '02'
060800940722    >C     OPZ002        OREQ      '04'
060900941006    >C     K1UTN1        CHAIN     ANUTN000                           2122
061000941006      * se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
0611009403183E   C                   ELSE
061200941006    >C     K1UTN1        CHAIN(N)  ANUTN000                           2122
0613009403183-   C                   ENDIF
061400941006      *
061500941006      * Gestione RCD allocato
0616009403183    C     *IN22         IFEQ      *ON
061700941006    >C                   MOVEL     'ANUTN00F'    MSGFIL
061800940318     C                   EXSR      MSGPGM
0619009403183-   C                   ENDIF
0620009403182-   C                   ENDDO
062100941006      *
062200941006      * se il rcd da gestire non viene trovato
062300941006      * oppure è allocato da altro pgm
062400941006      * ritorno al pgm guida avvertendolo dell'errore
0625009402242    C     *IN21         IFEQ      *ON
062600940318     C     *IN22         OREQ      *ON
062700940224     C                   MOVE      *ON           $FINE
062800940722    >C                   MOVE      '1'           ERR002
062900941006      *
063000941006      * altrimenti
0631009402242E   C                   ELSE
063200941006      * riempio la videata
063300940224     C                   EXSR      RIED1
063400941006      * richiamo routine dei ctrl per decodificarla
063500940614     C                   EXSR      CTRD1
063600940224     C                   MOVE      *ALL'0'       IN5098
063700940224     C                   MOVEA     IN5098        *IN(50)
063800941006      * definizione indicatori su video
063900940224     C                   SETOFF                                       02
0640009402243    C                   SELECT
064100940722    >C     OPZ002        WHENEQ    '02'
064200940224     C                   SETON                                        02        PR campi key
064300940722    >C     OPZ002        WHENEQ    '05'
0644009402243-   C                   ENDSL
064500941006      *
0646009402242-   C                   ENDIF
064700941006      *
0648009402241-   C                   ENDIF
064900941006      *
065000940117     C                   ENDSR
065100940207     C/EJECT
065200941006      ************************************************************
065300941006      * RIEMPIMENTO VIDEATA  D1
065400941006      ************************************************************
065500940207     C     RIED1         BEGSR
065600941006      *
065700941006    > *----------------------------------------------------------
065800941006    > * impostare i campi della videata
065900941006    > *----------------------------------------------------------
066000941006    >C                   MOVEL     UTNPRF        D1PRF
066100941006    >C                   MOVEL     UTNUFF        D1UFF
066200941006    >C                   MOVEL     UTNGRP        D1GRP
066300941006    >C                   MOVEL     UTNCUT        D1CUT
066400941006    >C                   MOVEL     UTNVFD        D1VFD
066500941006    >C                   MOVEL     UTNLIN        D1LIN
066600010212A0860C                   MOVEL     UTNSogg       D1Sogg
066700010212  "  C                   MOVEL     UTNTpInd      D1TpInd
066800010212  "  C                   MOVEL     UTNCdInd      D1CdInd
066900010212  "  C                   MOVEL     UTNTpCnt      D1TpCnt
067000010212A0860C                   MOVEL     UTNCdCnt      D1CdCnt
067100020403A0910C                   MOVEL     xscoff        D1Flg1
067200020403  "  C                   MOVEL     xscoff        H1Flg1
067300020403  "  C                   IF        UTNFlg1 = *on
067400020403  "  C                   MOVEL     xscon         D1Flg1
067500020403  "  C                   MOVEL     xscon         H1Flg1
067600020403A0910C                   ENDIF
067700021009A0970C                   MOVEL     xscoff        D1Flg2
067800021009A0970C                   IF        UTNFlg2 = *on
067900021009A0970C                   MOVEL     xscon         D1Flg2
068000021009A0970C                   ENDIF
068100021009A0970C                   EVAL      D1Flg3 = UTNFlg3
068200941006      *
068300940207     C                   ENDSR
068400940916     C/EJECT
068500941006      ************************************************************
068600941006      * GESTIONE F02 VIDEO D1
068700941006      ************************************************************
068800940916     C     F02D1         BEGSR
068900941006      *
069000941006      * per decodificare la videata richiamo la routine dei ctrl
069100941006      * e poi disattivo gli eventuali messaggi di errore
069200940916     C                   EXSR      CTRD1
069300941006      *
069400940916     C                   MOVE      *ALL'0'       IN5098
069500940916     C                   MOVEA     IN5098        *IN(50)
069600941006      *
069700940916     C                   ENDSR
069800940309     C/EJECT
069900941006      ************************************************************
070000941006      * GESTIONE F03 VIDEO D1
070100941006      ************************************************************
070200940309     C     F03D1         BEGSR
070300941006      *
070400940309     C                   MOVE      *ON           $FINE
070500940722    >C                   MOVE      '1'           RET002
070600941006      * fine programma
070700940325     C                   EXSR      ENDPGM
070800941006      *
070900940325     C                   ENDSR
071000940309     C/EJECT
071100941006      ************************************************************
071200941006      * GESTIONE F05 VIDEO D1
071300941006      ************************************************************
071400940309     C     F05D1         BEGSR
071500941006      *
071600940309     C                   MOVE      *ON           $INZD1
071700941006      *
071800940309     C                   ENDSR
071900940309     C/EJECT
072000941006      ************************************************************
072100941006      * GESTIONE F08 VIDEO D1
072200941006      ************************************************************
072300940309     C     F08D1         BEGSR
072400941006      *
072500940309     C                   MOVE      *ON           $FINE
072600940722    >C                   MOVE      '0'           RET002
072700941006      *
072800940309     C                   ENDSR
072900940309     C/EJECT
073000941006      ************************************************************
073100941006      * GESTIONE F12 VIDEO D1
073200941006      ************************************************************
073300940309     C     F12D1         BEGSR
073400941006      *
073500940309     C                   MOVE      *ON           $FINE
073600940722    >C                   MOVE      '2'           RET002
073700941006      *
073800940309     C                   ENDSR
073900940309     C/EJECT
074000941006      ************************************************************
074100941006      * GESTIONE F24 VIDEO D1
074200941006      ************************************************************
074300940309     C     F24D1         BEGSR
074400941006      *
074500941006      *
074600940506     C                   MOVEA     $FX1          $FX
074700940506     C                   MOVEA     $FC1          $FC
074800940506     C                   MOVEA     $FR1          $FR
074900940506     C                   Z-ADD     $ULKD1        $ULK
075000941007     C                   Z-ADD     79            $L01
075100941007     C                   Z-ADD     79            $L02
075200940506     C                   EXSR      RIEKEY
075300940506     C                   Z-ADD     $ULK          $ULKD1            3 0
075400940720     C                   MOVEA     $FC           $FC1
075500940506     C                   MOVEL     $KEY1         Z1KE1
075600060224A1148C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
075700940506     C                   MOVEL     $KEY2         Z1KE2
075800940506     C                   WRITE     FMTZ1
075900941006      *
076000940309     C                   ENDSR
076100940207     C/EJECT
076200941006      ************************************************************
076300941006      * CONTROLLO VIDEATA
076400941006      ************************************************************
076500940127     C     CTRD1         BEGSR
076600941006      *
076700940127     C                   SETOFF                                       99
076800941006      * non imposto pos. del cursore
076900940127     C                   Z-ADD     0             H1RIGA
077000940127     C                   Z-ADD     0             H1COLO
077100941006      *
077200941006      * CTRL DA NON EFFETTUARE SE ANNULLAMENTO
077300941006      *
0774009407221   >C     OPZ002        IFNE      '04'
077500941006      *
077600941006    > *-------------------------------------------------------------
077700941006    > * inserire i controlli
077800941006    > * Accendere l'indicatore 99 per ogni errore
077900941006    > *-------------------------------------------------------------
078000941006    > * CTRL OBBLIGATORIETA' OGGETTO
078100941006    >C     D1PRF         IFEQ      *BLANK
078200941006    >C                   SETON                                        5099
078201150909>>>>>C                   CLEAR                   d1dPrf
078202150909>>>>>C                   ELSE
078300150909>>>>>** Reperisco la descrizione del profilo utente.
078302150909>>>>>C                   CLEAR                   qusec
078303150909>>>>>C                   EVAL      qusbprv = %SIZE(qusec)
078304150909>>>>>C                   CALLP     RetrieveObjectDescription(qusd0200
078305150909>>>>>C                             : %SIZE(qusd0200) : 'OBJD0200'
078306150909>>>>>C                             : d1Prf + 'QSYS' : '*USRPRF' : qusec)
078307150909>>>>>C                   IF        qusei = *BLANK
078308150909>>>>>C                   EVAL      d1dPrf = qustd12
078309150909>>>>>C                   ELSE
078310150909>>>>>C                   EVAL      d1dPrf = *ALL'?'
078311150909>>>>>C                   ENDIF
078312150909    >C                   END
078400941006      *
078500941006      * CTRL GRUPPO
078600941006    >C     D1GRP         IFNE      *ZEROS
078700941007    >C                   MOVE      '1'           XTBRIC
078800941103    >C                   MOVE      XSCLIN        XTBLIN
078900941103    >C                   MOVE      XSCSOC        XTBAZI
079000941007    >C                   MOVE      *ZEROS        XTBKEY
079100941006    >C                   MOVE      D1GRP         XTBKEY
079200941006    >C                   MOVE      'G22'         XTBCOD
079300941006    >C                   EXSR      LETTAB
079400941006    >C     WTBERR        IFEQ      *OFF
079500941006    >C                   MOVEL     XTBUNI        D1DGRP
079600941006    >C                   ELSE
079700941007    >C                   SETON                                        5199
079800941006    >C                   END
079900941006    > *****                ELSE
080000941007    > *****                SETON                     5199
080100941006    >C                   END
080200941006      * CTRL UFFICIO
080300941007    >C     D1UFF         IFNE      *BLANK
080400941103    >C                   MOVE      XSCLIN        XTBLIN
080500941103    >C                   MOVE      XSCSOC        XTBAZI
080600941007    >C                   MOVE      '1'           XTBRIC
080700941006    >C                   MOVE      *ZEROS        XTBKEY
080800941006    >C                   MOVE      D1UFF         XTBKEY
080900941006    >C                   MOVE      'G01'         XTBCOD
081000941006    >C                   EXSR      LETTAB
081100941006    >C     WTBERR        IFEQ      *OFF
081200941006    >C                   MOVEL     XTBUNI        D1DUFF
081300941006    >C                   ELSE
081400941006    >C                   SETON                                        5299
081500941006    >C                   END
081600941006    > *****                ELSE
081700941006    > *****                SETON                     5299
081800941006    >C                   END
081900941006      *
082000941006      * CTRL LINGUA
082100941114    >C     D1LIN         IFNE      *BLANKS
082200941012     C                   MOVE      '1'           XTBRIC
082300941006    >C                   MOVE      *ZEROS        XTBKEY
082400941006    >C                   MOVE      D1LIN         XTBKEY
082500941006    >C                   MOVE      'G15'         XTBCOD
082600941006    >C                   EXSR      LETTAB
082700941006    >C     WTBERR        IFEQ      *OFF
082800941006    >C                   MOVEL     XTBUNI        D1DLIN
082900941006    >C                   ELSE
083000941006    >C                   SETON                                        5499
083100941006    >C                   END
083200941006    > *****                ELSE
083300941006    > *****                SETON                     5499
083400941006    >C                   ENDIF
083500010206A0860C*
083600010206  "  C                   EVAL      D1Des     = *blank
083700010208  "  C                   EVAL      D1Nome    = *blank
083800010206  "  C* se è presente almeno un dato del soggetto/indirizzo
083900010206  "  C                   IF        D1Sogg <> *blank
084000010206  "  C                             or D1TpInd <> *blank
084100010206  "  C                             or D1CdInd <> *blank
084200010206  "  C* controllo che esista
084300010206  "  C                   MOVE      '1'           DVARIC
084400010206  "  C                   MOVE      XSCSoc        DVASOCIETA
084500010206  "  C                   CALLB     'XALLINEA'
084600010206  "  C                   PARM                    D1Sogg
084700010206  "  C                   PARM      8             LEN               5 0
084800010206  "  C                   PARM                    XSCINP
084900010206  "  C                   MOVE      D1Sogg        DVASOGG
085000010206  "  C                   MOVE      D1TpInd       DVATpInd
085100010206  "  C                   MOVE      D1CdInd       DVACdInd
085200010206  "  C                   MOVEL     'DVASOG'      DVAstrutt
085300010206  "  C                   EVAL      DVALenOut = %Size(DVASOG)
085400010206  "  C                   CALLB     'NDDVASOG'    PDVASOG
085500010206  "  C                   IF        DVAErrore<>*ON
085600010206  "  C                   EVAL      %subst(DVASOG:1:DVALENOUT)
085700010206  "  C                              = %subst(DVAOUTPUT:1:DVALENOUT)
085800010206  "  C                   EVAL      D1Des     = DVSDesFil
085900010207  "  C*
086000010207  "  C* se è presente almeno un dato del soggetto/indirizzo
086100010207  "  C* deve essere presente almeno il tipo contatto
086200010207  "  C                   IF        D1TpCnt = *blank
086300010208  "  C                   SETON                                        6099
086400010208  "  C* il contatto deve essere legato al soggetto
086500010207  "  C                   ELSE
086600010208  "  C                   EVAL      Dati = ANCNT00F
086700010320  "  C                   IF        DR1202CNT(D1Sogg   :
086800010207  "  C                                       D1TpInd  :
086900010207  "  C                                       D1CdInd  :
087000010207  "  C                                       D1TpCnt  :
087100010207  "  C                                       D1CdCnt  :
087200010207  "  C                                       '1'      :
087300010207  "  C                                       %size(ANCNT00F) :
087400010212  "  C                                       ANCNT00F )
087500010207  "  C                                       <> 0
087600010208  "  C                   SETON                                        5899
087700010207  "  C                   ELSE
087800010208  "  C                   EVAL      D1Nome = CNTNome
087900010207  "  C                   ENDIF
088000010207  "  C                   ENDIF
088100010208  "  C* se il soggetto + indirizzo non esiste, do errore
088200010206  "  C                   ELSE
088300010208  "  C                   SETON                                        5799
088400010206  "  C                   ENDIF
088500010206A0860C                   ENDIF
088600020403A0910C*
088700020403  "  C* se Laguna è attivo per la società
088800020403  "  C                   IF        xscLaguna = *on
088900020403  "  C* controllo che il flag sia SI o NO
089000020403  "  C                   IF        D1Flg1 <> xscon
089100020403  "  C                             and D1Flg1 <> xscoff
089200020403  "  C                   SETON                                        6199
089300020403  "  C                   ENDIF
089400020403  "  C                   ELSE
089500020403  "  C                   EVAL      D1Flg1 = xscoff
089600020403  "  C                   ENDIF
089700020403A0910C*
089800021008A0970C*
089900021008  "  C* controllo che il flag msg di architettura via Posta sia SI o NO
090000021008  "  C                   IF        D1Flg2 <> xscon
090100021008  "  C                             and D1Flg2 <> xscoff
090200021008  "  C                   SETON                                        6299
090300021008  "  C                   ENDIF
090400021008  "  C*
090500021008  "  C* ctrl notifica
090600021008  "  C                   MOVEL     *BLANK        D1Flg3D
090700070122A0970C                   MOVE      '1'           X07RIC
090800061109B0905x***                MOVEL     *on           X07ALL
090900061109  "  x***                MOVEL     'ANUTN000'    X07TRC
091000061109B0905x***                MOVEL     'UTNFLG3'     X07CAM
091100061109B0905C                   MOVEL     *off          X07ALL
091200061109  "  C                   MOVEL     'JCMIT000'    X07TRC
091300061109B0905C                   MOVEL     'MITNOTIFY'   X07CAM
091400070122A0970C                   MOVEL     D1Flg3        X07VAL
091500021008  "  C                   CALLb     'X07VALR'
091600021008  "  C                   PARM                    X07DS
091700021008  "  C     X07ERR        IFEQ      '0'
091800021008  "  C                   MOVEL     X07DES        D1Flg3D
091900021008  "  C                   ELSE
092000021008  "  C                   SETON                                        6399
092100021008  "  C                   ENDIF
092200021008A0970C*
092300941006    > *
092400941006      * CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
092500940722    >C     OPZ002        IFEQ      '01'
092600940722    >C     OPZ002        OREQ      '03'
092700941006    > *-------------------------------------------------------------
092800941006    > * impostare i campi chiave e verificare l'esistenza del record
092900941006    > *-------------------------------------------------------------
093000941006    >C                   MOVEL     D1PRF         UTNPRF
093100941006    >C     K1UTN1        SETLL     ANUTN000                               21
093200940224     C     *IN21         IFEQ      '1'
093300941007     C                   SETON                                        5599
093400940223     C                   ENDIF
093500940224     C                   ENDIF
093600941006      *
0937009402241-   C                   END
093800941006      *
093900940117     C                   ENDSR
094000940207     C/EJECT
094100941006    > ************************************************************
094200941006    > * ACCESSO A TABELLE ANTAB00F
094300941006    > ************************************************************
094400941006    >C     LETTAB        BEGSR
094500941006    > *
094600941006    > * Input :
094700941006    > * - TABGRU/TABCOD = chiave univoca ANTAB00F
094800941006    > * Output :
094900941006    > * - WTBERR = flag segnalazione errore reperimento tabella
095000941006    > *
095100941006    >C                   CALL      'XATB'
095200941006    >C                   PARM                    XATBDS
095300941006    > *
095400941006    >C                   MOVE      *OFF          WTBERR
095500941006    >C     XTBERR        IFEQ      *ON
095600941006    >C                   MOVE      *BLANK        XTBUNI
095700941006    >C                   MOVE      *ON           WTBERR
095800941006    >C                   ENDIF
095900941006    > *
096000941006    >C                   ENDSR
096100940224     C/EJECT
096200941006      ************************************************************
096300010208    > * AGGIORNAMENTO ANAGRAFICA
096400941006      ************************************************************
096500941006    >C     AGGUTN        BEGSR
096600941006      *
096700940207     C                   SELECT
096800940722    >C     OPZ002        WHENEQ    '01'
096900940722    >C     OPZ002        OREQ      '03'
097000941104      *Reperisco il nuovo codice utente
097100941104    >C                   EXSR      REPCUT
097200941104     C     *IN99         IFEQ      '0'
097300941104    >C                   EXSR      RIEUTN
097400941006    >C                   WRITE     ANUTN000                             22
097500941104     C                   ENDIF
097600941006      * se rcd non scrivibile attivo errore
097700940318     C     *IN22         IFEQ      *ON
097800940318    >C                   SETON                                        5999
097900940318     C                   END
098000940722    >C     OPZ002        WHENEQ    '02'
098100941006    >C                   EXSR      RIEUTN
098200941006    >C                   UPDATE    ANUTN000
098300940722    >C     OPZ002        WHENEQ    '04'
098400080303B1033C                   EVAL      UTNAnn  = *on
098500080303B1033C                   Update    ANUTN000
098600080303B1033C***                DELETE    ANUTN000
098700940207     C                   ENDSL
098800020403A0910C*
098900020403  "  C* se laguna è attivo
099000020403  "  C                   IF        xscLaguna = *on   and
099100020403  "  C* se l'utente non era abilitato a Laguna ma ora sì
099200020419  "  C                             H1Flg1 <> xscon and D1Flg1 = xscon
099300020403  "  C* effettuo il Login su Laguna
099400020403  "  C                   EXSR      LoginLGN
099500020403  "  C                   ENDIF
099600020403  "  C* !!! manca il DeLogin su Laguna !!!
099700020403A0910C*
099800030428G1286 * se è attivo allineo i dati dell'utente in GesLot
099900030428  "  C                   If        GesLot = *On
100000030428  "  C                   ExSr      AllineaGesLot
100100030428G1286C                   EndIf
100200950322      *
100300950322      * richiama pgm User Exit
100400950322      *
100500950322     C     USRERR        IFEQ      *BLANK
100600950322     C     *IN99         ANDEQ     *OFF
100700950322      * imposta chiave univoca  per user exit
100800950322     C                   MOVE      XSCSOC        XUKSOC
100900950322     C                   MOVE      XSCDUN        XUKDUN
101000950322     C                   MOVEL     NOMPGM        XUKPGM
101100950322    >C                   MOVEL     UTNPRF        XUKFL1
101200950322    >C                   MOVEL     XUKDS         KPJBU
101300950322     C     NOMPGM        CAT       'U':0         USREXT           10
101400950322     C                   CALL      USREXT                               21
101500950322     C                   PARM                    KPJBA
101600950322     C     *IN21         IFEQ      *ON
101700950322     C                   MOVE      *ON           USRERR            1
101800950322     C                   ENDIF
101900950322     C                   ENDIF
102000941006      *
102100940203     C                   ENDSR
102200940224     C/EJECT
102300030428G1286 ****************************************************************
102400030428  "   * Allineamento utenti GesLot
102500030428  "   ****************************************************************
102600030428G1286C     AllineaGesLot BEGSR
102700030428      *
102800030428     C                   Eval      OpzGLA001 = Opz002
102900030428     C                   Eval      ErrGLA001 = *Off
103000030429      *
103100030430     C                   Call      'GLA001R'
103200030429     C                   Parm                    ANUTNDS
103300030428     C                   Parm                    OpzGLA001
103400030428     C                   Parm                    ErrGLA001
103500030428      *
103600030428G1286C                   ENDSR
103700941006      ************************************************************
103800941006      * RIEMPIMENTO FILE IN AGGIORNAMENTO
103900941006      ************************************************************
104000941006    >C     RIEUTN        BEGSR
104100941006      *
104200941006     C                   CLEAR                   ANUTN000
104300941006    > *-------------------------------------------------------------
104400941006    > * impostare i campi del file
104500941006    > *-------------------------------------------------------------
104600941006    >C                   MOVEL     D1PRF         UTNPRF
104700941006    >C                   MOVEL     D1UFF         UTNUFF
104800941006    >C                   MOVEL     D1GRP         UTNGRP
104900941006    >C                   MOVEL     D1CUT         UTNCUT
105000941006    >C                   MOVEL     D1VFD         UTNVFD
105100941006    >C                   MOVEL     D1LIN         UTNLIN
105200010212A0860C                   EVAL      UTNSogg   =   D1Sogg
105300010212  "  C                   EVAL      UTNTpInd  =   D1TpInd
105400010212  "  C                   EVAL      UTNCdInd  =   D1CdInd
105500010212  "  C                   EVAL      UTNTpCnt  =   D1TpCnt
105600010212A0860C                   EVAL      UTNCdCnt  =   D1CdCnt
105700020403A0910C                   MOVEL     *off          UTNFlg1
105800020403  "  C                   IF        D1Flg1 = xscon
105900020403  "  C                   MOVEL     *on           UTNFlg1
106000020403A0910C                   ENDIF
106100021008A0970C                   MOVEL     *off          UTNFlg2
106200021008  "  C                   IF        D1Flg2 = xscon
106300021008  "  C                   MOVEL     *on           UTNFlg2
106400021009  "  C                   ENDIF
106500021009A0970C                   MOVEL     D1Flg3        UTNFlg3
106600941006      *
106700940207     C                   ENDSR
106800940224     C/EJECT
106900941006      ************************************************************
107000941006      * GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
107100941006      ************************************************************
107200940224     C     GESAGG        BEGSR
107300941006      *
107400941006      * segnalo al pgm chiamante l'avvenuta conferma
107500940722    >C                   MOVE      *ON           OPR002
107600941006      * segnalo al pgm chiamante l'aver premuto l'F6
107700940722    >C                   MOVE      '0'           RET002
107800941006      *
1079009402241    C                   SELECT
108000941006      * nel caso di immissione, ripristino la videata iniziale
108100940722    >C     OPZ002        WHENEQ    '01'
108200940224     C                   MOVE      *ON           $INZD1
108300941006      * altrimenti ritorno al pgm chiamante
108400940224     C                   OTHER
108500940224     C                   MOVE      *ON           $FINE
1086009402241-   C                   ENDSL
108700941006      *
108800940224     C                   ENDSR
108900940318     C/EJECT
109000020403A0910 ******************************************************
109100020403  "   * Login su Laguna
109200020403  "   ******************************************************
109300020403A0910C     LoginLgn      BEGSR
109400941006      *
109500020415     C                   MOVEL(P)  'CREA_UTENTE' Cmd670
109600020403     C                   CLEAR                   Parm670
109700020419     C                   EVAL      Parm670(1) = UTNPrf
109800021120A0970C                   EVAL      Parm670(2) = knsif
109900020403      *
110000071018A1244x***                CALL      'ANA670R'
110100071018A1244C                   CALL      'PJXX038R'
110200020403     C                   PARM                    xscsoc
110300020403     C                   PARM                    Cmd670
110400020403     C                   PARM                    Parm670
110500020403     C                   PARM                    Err670
110600020403     C                   PARM                    Ret670
110700020403      *
110800020403A0910C                   ENDSR
110900020403      ******************************************************
111000020403      * PGM PER GESTIONE MESSAGGI ERRORE                   *
111100020403      ******************************************************
111200020403     C     MSGPGM        BEGSR
111300020403      *
111400940318     C                   Z-ADD     2             MSGMSG
111500940318     C                   MOVEL     'S'           MSGRSP
111600940318     C                   MOVEL     'CR'          MSGTPR
111700940318     C                   MOVEL     'S'           MSGEMV
111800940318     C                   Z-ADD     10            MSGRGP
111900940318     C                   MOVEL     'S'           MSGLCK
112000940318     C                   MOVEA     STATUS        STS
112100940318     C                   MOVEL     'N'           MSGCNL
112200940318     C                   MOVEL     'N'           MSGVID
112300940318     C                   MOVEL     'N'           MSGOPE
112400940318     C                   MOVEL     'N'           MSGSTP
112500940318     C                   CALL      'XMSGR'
112600940318     C                   PARM                    XMSGDS
112700941006      *
112800940318     C                   ENDSR
112900940207     C/EJECT
113000941006      ************************************************************
113100941006      * OPERAZIONI INIZIALI
113200941006      ************************************************************
113300940131     C     *INZSR        BEGSR
113400941006      *
113500941006      * Reperimento parametri
113600941006      *
113700940117     C     *ENTRY        PLIST
113800940117     C                   PARM                    KPJBA
113900941103      *
114000941103      * Reperimento dati società
114100941103      *
114200941103     C                   MOVEL     'SOC001'      TIPXSC
114300941103     C                   MOVEL     *BLANK        SOCXSC
114400941103     C                   EXSR      REPSOC
114500941103     C     RTNXSC        IFNE      '1'
114600941103     C                   MOVEL     XSOCDS        SOC001
114700941103     C                   ELSE
114800941103     C                   MOVE      *ON           $FINE
114900941103     C                   EXSR      ENDPGM
115000941103     C                   ENDIF
115100941006      *
115200030428G1286 * Reperimento p.m.'SICOM'
115300030428  "  C                   Eval      XPaSoc = XScSoc
115400030428  "  C                   Eval      XPaPar = 'SICOM'
115500030428  "  C                   Eval      XPaRic = *Blank
115600030428  "  C                   Clear                   XPaOut
115700030428  "  C                   Eval      XPaErr = *Off
115800030428  "  C                   ExSr      CallXPAR
115900030428  "  C                   If        XPaErr = *Off
116000030428  "  C                   MoveL     XPaOut        SICOM
116100030428  "  C                   Else
116200030428  "  C                   Clear                   SICOM
116300030428G1286C                   EndIf
116400941006      *
116500941006      * Reperimento tasti di funzione
116600941006      *
116700940506     C                   EXSR      REPKEY
116800941006      *
116900940223     C                   ENDSR
117000940223     C/EJECT
117100030428G1286 ************************************************************
117200030428  "   * Driver parametri modulo
117300030428  "   ************************************************************
117400030428G1286C     CallXPAR      BEGSR
117500030428      *
117600030428     C                   CallB     'XPAR'
117700030428     C                   Parm                    XPaSoc
117800030428     C                   Parm                    XPaPar
117900030428     C                   Parm                    XPaOut
118000030428     C                   Parm                    XPaErr
118100030428     C                   Parm                    XPaRic
118200030428      *
118300030428G1286C                   ENDSR
118400941006      ************************************************************
118500941006      * REPERIMENTO TASTI DI FUNZIONE
118600941006      ************************************************************
118700940506     C     REPKEY        BEGSR
118800940506     C                   MOVEA     $FM1          $FM
118900950906     C                   MOVEA     $FC1          $FC
119000940506     C                   EXSR      REPKKK
119100940506     C                   MOVEA     $FX           $FX1
119200941006      *
119300940614     C                   ENDSR
119400940506     C/EJECT
119500941006      ************************************************************
119600941006      * REPERIMENTO TASTI DI FUNZIONE
119700941006      ************************************************************
119800940506     C     REPKKK        BEGSR
119900941006      *
120000940506     C                   CLEAR                   $FX
120100940506     C     1             DO        24            X
120200941006      *
120300940506     C     $FM(X)        IFNE      *BLANK
120400950906     C     $FC(X)        ANDNE     '0'
120500941006      *
120600940506     C                   MOVEL     $FM(X)        IDMSG
120700940506     C                   CALL      'XRTVM'
120800940506     C                   PARM                    IDMSG            27
120900940506     C                   PARM                    TXTMSG          100
121000940506     C                   PARM                    ERRMSG            1
121100941006      *
121200940506     C     ERRMSG        IFNE      *ON
121300940506     C                   MOVEL     TXTMSG        $FX(X)
121400940506     C                   ELSE
121500940506     C                   MOVEL     *ALL' '       $FX(X)
121600940506     C                   END
121700941006      *
121800940506     C                   END
121900941006      *
122000940506     C                   END
122100941006      *
122200940506     C                   ENDSR
122300940509     C/EJECT
122400941006      ************************************************************
122500941006      * INIZIALIZZAZIONE VARIABILI
122600941006      ************************************************************
122700940223     C     INZVAR        BEGSR
122800941006      *
122900941006      * Dati da PGM guida
123000941006      *
123100941006    >C                   MOVEL     KPJBU         A002DS
123200941006      *
123300941006      * Definizione dei tasti funzione utilizzabili a pgm
123400941006      *
123500940715     C                   EXSR      DEFOPZ
123600941006      *
123700941006      * Variabili per gestione videate
123800941006      *
123900940223     C                   MOVE      'D1'          $GEST                          fmt in gestione
124000940223     C                   MOVE      *OFF          $FINE                          fine pgm
124100940223     C                   MOVE      *ON           $INZD1                         inizializzione D1
124200940224     C                   MOVE      *BLANK        $LASTG                         fmt di provenienza
124300940224     C                   MOVE      *BLANK        $LASTV                         ultima emissione
124400940506     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
124500941006      *
124600941006      * Variabili appoggio
124700941006      *
124800940224     C                   Z-ADD     0             CURR
124900940224     C                   Z-ADD     0             CURC
125000941006      *
125100941006      * Valorizzazione campi univoci testate
125200941006      *
125300940207     C                   CLEAR                   FMTT1
125400940207     C                   MOVEL     KNSIF         NOMSIF
125500941110     C                   MOVEL     XSCDSI        NOMDIT
125600941027     C                   MOVEL     DSPGM         NOMPGM
125700950426     C* recupero il sottotitolo (se c'è)
125800950426     C                   MOVEL     *BLANK        T1TIT
125900950426     C     SOTTIT        IFNE      *BLANK
126000950426     C                   MOVEL     SOTTIT        IDMSG
126100950426     C                   CALL      'XRTVM'
126200950426     C                   PARM                    IDMSG            27
126300950426     C                   PARM                    TXTMSG          100
126400950426     C                   PARM                    ERRMSG            1
126500950426     C                   PARM      *ON           CTRMSG            1            centratura
126600950426     C                   PARM      30            LENMSG            3 0          lun output.
126700950426     C     ERRMSG        IFNE      *ON
126800950426     C                   MOVEL     TXTMSG        T1TIT
126900950426     C                   ELSE
127000950426     C                   MOVEL     *ALL'?'       T1TIT
127100950426     C                   ENDIF
127200950426     C                   ENDIF
127300950426     C* recupero il tipo operazione
127400940224     C                   Z-ADD     1             X
127500940722    >C     OPZ002        LOOKUP    $OZ(X)                                 21
127600940224     C     *IN21         IFEQ      *ON
127700950426     C                   MOVEL     $ID(X)        IDMSG
127800950426     C                   CALL      'XRTVM'
127900950426     C                   PARM                    IDMSG            27
128000950426     C                   PARM                    TXTMSG          100
128100950426     C                   PARM                    ERRMSG            1
128200950426     C                   PARM      *ON           CTRMSG            1            centratura
128300950426     C                   PARM      30            LENMSG            3 0          lun output.
128400950426     C     ERRMSG        IFNE      *ON
128500950426     C                   MOVEL     TXTMSG        T1OPE
128600950426     C                   ELSE
128700950426     C                   MOVEL     *ALL'?'       T1OPE
128800950426     C                   ENDIF
128900950426     C                   ENDIF
129000950426     C*
129100941006      *
129200941006      * Inizializzazione/Decodifica prima videata
129300941006      *
129400940224     C                   EXSR      INZD1
129500940224     C                   MOVE      *OFF          $INZD1
129600941007     C*
129700941007     C* Reperimento azienda
129800941006      *
129900940117     C                   ENDSR
130000940715     C/EJECT
130100941006      ************************************************************
130200941006      * DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
130300941006      ************************************************************
130400940715     C     DEFOPZ        BEGSR
130500941006      *
130600941006      * Questa routine deve valorizzare la schiera $FC1/$FR1
130700941006      * con S o N a seconda della utilizzabilità o meno
130800941006      * del tasto funzione correlato
130900941006      *
131000941006      * Inizialmente ripristino le abilitazioni come da schiera a tempo
131100941006      * di compilazione
131200940715     C                   RESET                   $FC1
131300940715     C                   RESET                   $FR1
131400941006      *
131500941006      * Dopodichè, a seconda del richiamo e delle autorizzazioni
131600941006      * invalido alcune opzioni
131700940715     C                   SELECT
131800941006      * Se pgm richiamato in interrogazione disattivo :
131900940722    >C     OPZ002        WHENEQ    '05'
132000941006      * F4=Ricerca
132100940715     C                   Z-ADD     4             X
132200940720     C                   MOVE      '0'           $FC1(X)
132300941006      * F5=Ripristino
132400940715     C                   Z-ADD     5             X
132500940720     C                   MOVE      '0'           $FC1(X)
132600941006      * F6=Conferma
132700940715     C                   Z-ADD     06            X
132800940720     C                   MOVE      '0'           $FC1(X)
132900941006      * Se pgm richiamato in annullamento disattivo :
133000941117    >C     OPZ002        WHENEQ    '04'
133100941006      * F4=Ricerca
133200940715     C                   Z-ADD     4             X
133300940720     C                   MOVE      '0'           $FC1(X)
133400941006      * F5=Ripristino
133500940715     C                   Z-ADD     5             X
133600940720     C                   MOVE      '0'           $FC1(X)
133700941006      *
133800940715     C                   OTHER
133900941006      *
134000940715     C                   ENDSL
134100941006      *
134200941006      * Disattivo a seconda delle autorizzazioni :
134300941006    > *********  AUTXXX    IFEQ 'N'
134400941006    > *********            Z-ADD1         X
134500941006    > *********  'XX'      LOKUP$OP1,X                   21
134600941006    > *********  *IN21     IFEQ *ON
134700941006    > *********            MOVE 'N'       $FL1,X
134800941006    > *********            ENDIF
134900941006    > *********            ENDIF
135000941006      *
135100940715     C                   ENDSR
135200941006      ************************************************************
135300941006      * DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
135400941006      ************************************************************
135500940223     C     DEFCAM        BEGSR
135600941006      *
135700941006      * klist
135800941006      *
135900941006    > * xxUTN01L
136000941006    >C     K1UTN1        KLIST
136100941006    >C                   KFLD                    UTNPRF
136200010206A0860C*
136300010206  "  C* PLIST per NDVASOG
136400010206  "  C     PDVASOG       PLIST
136500010206  "  C                   PARM                    DVARIC
136600010206  "  C                   PARM                    DVASOCIETA
136700010206  "  C                   PARM                    DVAUNITA
136800010206  "  C                   PARM                    DVASTRUTT
136900010206  "  C                   PARM                    DVADTRIF
137000010206  "  C                   PARM                    DVALENOUT
137100010206  "  C                   PARM                    *omit
137200010206  "  C                   PARM                    *omit
137300010206  "  C                   PARM                    *omit
137400010206  "  C                   PARM                    *omit
137500010206  "  C                   PARM                    DVASOGG
137600010206  "  C                   PARM                    DVATPIND
137700010206  "  C                   PARM                    DVACDIND
137800010206  "  C                   PARM                    DVAERRORE
137900010206A0860C                   PARM                    DVAOUTPUT
138000941006      *
138100941006      * Variabili per gestione videate
138200941006      *
138300940223     C                   MOVE      *BLANK        $GEST             2            fmt in gestione
138400940223     C                   MOVE      *BLANK        $FINE             1            fine pgm
138500940223     C                   MOVE      *BLANK        $INZD1            1            inizializzione D1
138600940223     C                   MOVE      *BLANK        $LASTG            2            fmt di provenienza
138700940223     C                   MOVE      *BLANK        $LASTV            2            ultima emissione
138800940225     C                   Z-ADD     0             $PIED1            3 0          piede da emett. D1
138900941006      *
139000941006      * Variabili appoggio
139100941006      *
139200941012     C     *LIKE         DEFINE    UTNGRP        WWGRP
139300941012     C     *LIKE         DEFINE    UTNUFF        WWUFF
139400941012     C     *LIKE         DEFINE    UTNLIN        WWLIN
139500941012      *
139600940223     C                   Z-ADD     0             CURR              2 0
139700940223     C                   Z-ADD     0             CURC              2 0
139800940223     C                   MOVE      *ZEROS        WIN              99
139900940224     C                   MOVE      *BLANK        WTBERR            1
140000941006      *
140100941006      * Indici
140200941006      *
140300940223     C                   Z-ADD     0             X                 3 0
140400940223     C                   Z-ADD     0             Y                 3 0
140500941006      *
140600940117     C                   ENDSR
140700941006      ************************************************************
140800940509     C/EJECT
140900941006      ************************************************************
141000941006      * CONTROLLO ABILITAZIONE TASTI
141100941006      ************************************************************
141200940509     C     CTRKEY        BEGSR
141300940509     C                   SELECT
141400940509     C     $TASTO        WHENEQ    F01
141500940720     C     $FC(1)        IFEQ      '0'
141600940509     C                   SETON                                        99
141700940509     C                   ENDIF
141800940509     C     $TASTO        WHENEQ    F02
141900940720     C     $FC(2)        IFEQ      '0'
142000940509     C                   SETON                                        99
142100940509     C                   ENDIF
142200940509     C     $TASTO        WHENEQ    F03
142300940720     C     $FC(3)        IFEQ      '0'
142400940509     C                   SETON                                        99
142500940509     C                   ENDIF
142600940509     C     $TASTO        WHENEQ    F04
142700940720     C     $FC(4)        IFEQ      '0'
142800940509     C                   SETON                                        99
142900940509     C                   ENDIF
143000940509     C     $TASTO        WHENEQ    F05
143100940720     C     $FC(5)        IFEQ      '0'
143200940509     C                   SETON                                        99
143300940509     C                   ENDIF
143400940509     C     $TASTO        WHENEQ    F06
143500940720     C     $FC(6)        IFEQ      '0'
143600940509     C                   SETON                                        99
143700940509     C                   ENDIF
143800940509     C     $TASTO        WHENEQ    F07
143900940720     C     $FC(7)        IFEQ      '0'
144000940509     C                   SETON                                        99
144100940509     C                   ENDIF
144200940509     C     $TASTO        WHENEQ    F08
144300940720     C     $FC(8)        IFEQ      '0'
144400940509     C                   SETON                                        99
144500940509     C                   ENDIF
144600940509     C     $TASTO        WHENEQ    F09
144700940720     C     $FC(09)       IFEQ      '0'
144800940509     C                   SETON                                        99
144900940509     C                   ENDIF
145000940509     C     $TASTO        WHENEQ    F10
145100940720     C     $FC(10)       IFEQ      '0'
145200940509     C                   SETON                                        99
145300940509     C                   ENDIF
145400940509     C     $TASTO        WHENEQ    F11
145500940720     C     $FC(11)       IFEQ      '0'
145600940509     C                   SETON                                        99
145700940509     C                   ENDIF
145800940509     C     $TASTO        WHENEQ    F12
145900940720     C     $FC(12)       IFEQ      '0'
146000940509     C                   SETON                                        99
146100940509     C                   ENDIF
146200940509     C     $TASTO        WHENEQ    F13
146300940720     C     $FC(13)       IFEQ      '0'
146400940509     C                   SETON                                        99
146500940509     C                   ENDIF
146600940509     C     $TASTO        WHENEQ    F14
146700940720     C     $FC(14)       IFEQ      '0'
146800940509     C                   SETON                                        99
146900940509     C                   ENDIF
147000940509     C     $TASTO        WHENEQ    F15
147100940720     C     $FC(15)       IFEQ      '0'
147200940509     C                   SETON                                        99
147300940509     C                   ENDIF
147400940509     C     $TASTO        WHENEQ    F16
147500940720     C     $FC(16)       IFEQ      '0'
147600940509     C                   SETON                                        99
147700940509     C                   ENDIF
147800940509     C     $TASTO        WHENEQ    F17
147900940720     C     $FC(17)       IFEQ      '0'
148000940509     C                   SETON                                        99
148100940509     C                   ENDIF
148200940509     C     $TASTO        WHENEQ    F18
148300940720     C     $FC(18)       IFEQ      '0'
148400940509     C                   SETON                                        99
148500940509     C                   ENDIF
148600940509     C     $TASTO        WHENEQ    F19
148700940720     C     $FC(19)       IFEQ      '0'
148800940509     C                   SETON                                        99
148900940509     C                   ENDIF
149000940509     C     $TASTO        WHENEQ    F20
149100940720     C     $FC(20)       IFEQ      '0'
149200940509     C                   SETON                                        99
149300940509     C                   ENDIF
149400940509     C     $TASTO        WHENEQ    F21
149500940720     C     $FC(21)       IFEQ      '0'
149600940509     C                   SETON                                        99
149700940509     C                   ENDIF
149800940509     C     $TASTO        WHENEQ    F22
149900940720     C     $FC(22)       IFEQ      '0'
150000940509     C                   SETON                                        99
150100940509     C                   ENDIF
150200940509     C     $TASTO        WHENEQ    F23
150300940720     C     $FC(23)       IFEQ      '0'
150400940509     C                   SETON                                        99
150500940509     C                   ENDIF
150600940509     C     $TASTO        WHENEQ    F24
150700940720     C     $FC(24)       IFEQ      '0'
150800940509     C                   SETON                                        99
150900940509     C                   ENDIF
151000940509     C                   ENDSL
151100940509     C                   ENDSR
151200940509     C/EJECT
151300941006      ************************************************************
151400941006      * RIEMPIMENTO TASTI FUNZIONE
151500941006      ************************************************************
151600940509     C     RIEKEY        BEGSR
151700941006      *
151800941006      * Input :
151900941006      * - $FX = schiera con tutti i testi disponibili
152000941006      * - $FC = schiere con i flag di validità associati ai testi
152100941006      * - $FR = schiere con i flag di REVERSE IMAGE
152200941006      * - $ULK = ultimo testo caricato
152300941006      * - $L01 = Lunghezza prima riga
152400941006      * - $L02 = Lunghezza seconda riga
152500941006      * Output :
152600941006      * - $ULK
152700941006      * - $KEY1= 1° riga di tasti funzione
152800941006      * - $KEY2= 2° riga di tasti funzione
152900941006      *
153000940509     C                   CALL      'XKEYMSG'
153100941006      * input
153200940509     C                   PARM                    $FX
153300940509     C                   PARM                    $FC
153400940509     C                   PARM                    $FR
153500940509     C                   PARM                    $ULK              3 0
153600941006      * output
153700940509     C                   PARM                    $KEY1            79
153800940509     C                   PARM                    $KEY2            79
153900941006    > * input
154000940722    >C                   PARM                    $L01              3 0
154100940722    >C                   PARM                    $L02              3 0
154200941006      *
154300060224A1148C                   PARM                    $ALLFUNCT       480
154400060224A1148C*
154500940509     C                   ENDSR
154600941103     C/EJECT
154700941103      ************************************************************
154800941103      * Reperimento dati società
154900941103      ************************************************************
155000941103     C     REPSOC        BEGSR
155100941103      *
155200941103     C                   CALL      'XSOC'
155300941103     C                   PARM                    TIPXSC            6
155400941103     C                   PARM                    SOCXSC            3
155500941103     C                   PARM                    CDSXSC            9 0
155600941103     C                   PARM                    MODXSC            3
155700941103     C                   PARM      *BLANKS       RTNXSC            1
155800941103     C                   PARM                    XSOCDS
155900941103     C                   PARM                    KPJBA
156000941103      *
156100941103     C                   ENDSR
156200941104     C/EJECT
156300941104      ************************************************************
156400941104      * Reperimento codice utente per immissione copia
156500941104      ************************************************************
156600941104     C     REPCUT        BEGSR
156700031112     c*
156800031112B0644C                   clear                   NewCut1
156900941104      *
157000941104     C     *HIVAL        SETLL     ANUTN02L
157100941104     C                   READP     ANUTN02L                               22
157200941104     C     *IN22         IFEQ      '0'
157300941115     C                   MOVE      UTNCUT        APP3N             3 0
157400941115     C     APP3N         ADD       1             NEWCUT            4 0
157500941104     C     NEWCUT        IFGT      999
157600031112B0606c* cerco buchi nella numerazione
157700031112B0606X*************      exsr      SearchBuc  ELIMINARE ROUTINE
157800031112B0644C* cerco prima buchi nei numeri
157900031112  "  c                   exsr      SearchBucN
158000031112  "  C* se non trovato cerco poi buchi nelle lettere se ammesse
158100031204  "  c* escluso proj distribuzione dalla release 4.1 !!!!
158200031204  "  c                   if        xscgest <> '2' and
158300031112  "  c                             xscgest <> '3' and
158400031112  "  c                             xscgest <> '6' and
158500031112  "  c                             xscgest <> '7'
158600031112  "  C     NewCut1       IFeq      *blanks
158700031112  "  c                   exsr      SearchBucL
158800031112  "  C                   endif
158900031112B0644C                   endif
159000030903B0606x****               SETON                                        9956
159100031112B0644C* se trovati buchi
159200031112  "  C     NewCut1       IFne      *blanks
159300031112  "  c                   move      Newcut1       D1Cut
159400031112  "  c                   else
159500031112  "  C* Se non trovati buchi, scatta la numerazione alfanumerica
159600031204  "  c* solo se però non presente il nostro gestionale,
159700031204  "  c* escluso proj distribuzione dalla release 4.1 !!!!
159800031204  "  c* in quanto vi sono ancora dei file con utente numerico
159900031204  "  c                   if        xscgest <> '2' and
160000031112  "  c                             xscgest <> '3' and
160100031112  "  c                             xscgest <> '6' and
160200031112  "  c                             xscgest <> '7'
160300031112  "  c                   move      'Z99'         WrkCut            3
160400031112  "  c                   exsr      CdAlfa
160500031112  "  C* se non fallita numerazione alfanumerica
160600031112  "  C     NewCut1       IFne      *blanks
160700031112  "  c                   move      Newcut1       D1Cut
160800031112  "  c                   else
160900031112  "  c                   SETON                                        9956
161000031112  "  C                   endif
161100031112  "  c*
161200031112  "  c                   else
161300031112  "  c                   SETON                                        9956
161400031112  "  C                   endif
161500031112  "  c*
161600031112  "  C                   endif
161700031112  "  C                   ELSE
161800031112  "  X***                Z-ADD     NEWCUT        D1CUT
161900031112B0644c                   move      NEWCUT        D1CUT
162000941104     C                   ENDIF
162100941104     C                   ELSE
162200941104     C                   SETON                                        9956
162300941104     C                   ENDIF
162400941104      *
162500941104     C                   ENDSR
162600941103     C/EJECT
162700031112B0644 ************************************************************
162800031112      * cerca buchi nella numerazione  LETTERE
162900030903      * parte dall'inizio e trova il primo buco.
163000031112B0644 ************************************************************
163100031112     C     SearchBucL    BEGSR
163200031112     c*
163300031112     c                   clear                   newcut1           3
163400030903     c*
163500030903     c                   z-add     0             $2                2 0
163600030903      *
163700030903     C     *Loval        SETLL     ANUTN02L
163800030903     c                   do        *hival
163900030903     C                   READ      ANUTN02L                               22
164000030903     C   22              leave
164100030903     c*
164200030903     c                   if        $2 = 99
164300031113     c* cerco con un'altra lettera
164400031113     c                   z-add     0             $2
164500031113     c                   iter
164600030903     c                   endif
164700030903     c*
164800030903     c* se non ci sono le lettere, utilizzo le 3 cifre, altrimenti le 2
164900030903     c* cifre
165000030903     c                   movel     utncut        Primo             1
165100030903     c                   move      utncut        Wrk2N             2 0
165200030903     c*
165300030903     c                   select
165400031112     c* solo lettere
165500030903     c                   when      primo <'0'
165600030903     c                   if        wrk2N <> $2
165700031112     c                   movel     Primo         Newcut1
165800031112     c                   move      $2            NewCut1
165900030903     c                   leave
166000030903     c                   endif
166100031112     c* se numero esco dal ciclo
166200030903     c                   when      primo >='0'
166300031112     C                   leave
166400030903     c                   endsl
166500030903     c*
166600030903     c                   add       1             $2
166700030903     c*
166800030903     c                   enddo
166900030903      *
167000031112B0644C                   ENDSR
167100031112B0644 ************************************************************
167200031112      * cerca buchi nella numerazione  NUMERI
167300031112      * parte dall'inizio e trova il primo buco.
167400031112B0644 ************************************************************
167500031112     C     SearchBucN    BEGSR
167600031112     c*
167700031112B0644c                   clear                   newcut1           3
167800031112     c*
167900031112     c                   z-add     1             $3                3 0
168000031112      * mi posiziono dal primo numero
168100031112     C     '001'         SETLL     ANUTN02L
168200031112     c                   do        *hival
168300031112     C                   READ      ANUTN02L                               22
168400031112     C   22              leave
168500031112     c*
168600031112     c                   if        $3 = 999
168700031112     c                   leave
168800031112     c                   endif
168900031112     c*
169000031112     c* se non ci sono le lettere, utilizzo le 3 cifre, altrimenti le 2
169100031112     c* cifre
169200031112     c                   movel     utncut        Primo             1
169300031112     c                   move      utncut        Wrk3N             3 0
169400031112     c*
169500031112     c                   select
169600031112     c* se lettere (non è possibile)
169700031112     c                   when      primo <'0'
169800031112     c                   leave
169900031112     c* se numeri
170000031112     c                   when      primo >='0'
170100031112     c                   if        wrk3N <> $3
170200031112     c                   move      $3            NewCut1
170300031112     c                   leave
170400031112     c                   endif
170500031112     c                   endsl
170600031112     c*
170700031112     c                   add       1             $3
170800031112     c*
170900031112     c                   enddo
171000031112      *
171100031112B0644C                   ENDSR
171200031112B0644 ************************************************************
171300031112      * Reperimento codice utente alfanumerico
171400031112B0644 ************************************************************
171500031112     C     Cdalfa        BEGSR
171600031112     c*
171700031112     c                   clear                   newcut1
171800031112     c*
171900031112     C     WrkCut        SETLL     ANUTN02L
172000031112     C                   READP     ANUTN02L                               22
172100031112     c*
172200031112     C     *IN22         IFEQ      '1'
172300031112     C                   MOVE      'A00'         newcut1
172400031112     C                   else
172500031112     c                   movel     utncut        Primo             1
172600031112     c                   move      utncut        APP2N             2 0
172700031112     C     APP2N         ADD       1             NEWCUT2           3 0
172800031112     C     NEWCUT2       IFGT      99
172900031112     c* quando è maggiore di 99
173000031112     c* imposto la lettera successiva
173100031112     c                   z-add     1             $x                3 0
173200031112     c     primo         lookup    cd($X)                                 13
173300031112     c                   eval      primo =cd($X+1)
173400031112     c                   eval      newcut1  =  primo + '00'
173500031112     c                   else
173600031112     c                   move      newcut2       newcut1
173700031112     c                   movel     primo         newcut1
173800031112     c                   endif
173900031112     c*
174000031112     c                   endif
174100031112     c*
174200031112b0644C                   ENDSR
174300031112      *
174400950906**
17450095042601PROMSG    *LIBL     COS0001
17460095042602PROMSG    *LIBL     COS0002
17470095042603PROMSG    *LIBL     COS0003
17480095042604PROMSG    *LIBL     COS0004
17490095042605PROMSG    *LIBL     COS0005
175000940509** TASTI DI FUNZIONE  D1
175100940509PROMSG    *LIBL     KEY0001  01
175200940509PROMSG    *LIBL     KEY0002  02
175300940509PROMSG    *LIBL     KEY0003  03
175400940509PROMSG    *LIBL     KEY0004  04
175500940509PROMSG    *LIBL     KEY0005  05
175600940509PROMSG    *LIBL     KEY0006  06
175700940509PROMSG    *LIBL     KEY0007  07
175800940509PROMSG    *LIBL     KEY0008  08
175900940509PROMSG    *LIBL     KEY0009  09
176000940509PROMSG    *LIBL     KEY0010  10
176100940509PROMSG    *LIBL     KEY0011  11
176200940509PROMSG    *LIBL     KEY0012  12
176300940509PROMSG    *LIBL     KEY0013  13
176400940509PROMSG    *LIBL     KEY0014  14
176500940509PROMSG    *LIBL     KEY0015  15
176600940509PROMSG    *LIBL     KEY0016  16
176700940509PROMSG    *LIBL     KEY0017  17
176800940509PROMSG    *LIBL     KEY0018  18
176900940509PROMSG    *LIBL     KEY0019  19
177000940509PROMSG    *LIBL     KEY0020  20
177100940509PROMSG    *LIBL     KEY0021  21
177200940509PROMSG    *LIBL     KEY0022  22
177300940509PROMSG    *LIBL     KEY0023  23
177400940509PROMSG    *LIBL     KEY0024  24
177500940509**  ABILITAZIONE D1
1776009407200N  01
1777009407200N  02
1778009407201N  03
1779009407201N  04
1780009407201N  05
1781009407201N  06
1782009407200N  07
1783009407201N  08
1784009407200N  09
1785009410100N  10
1786009407200N  11
1787009407201N  12
1788009407200N  13
1789009407200N  14
1790009407200N  15
1791009407200N  16
1792009407200N  17
1793009407200N  18
1794009407200N  19
1795009407200N  20
1796009407200N  21
1797009407200N  22
1798009509060N  23   Impostare a '1' se si utilizzano opzioni
1799009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
180000031112**  codici utenti B0644
180100031112ABCDEFGHIJKLMNOPQRSTUVWXYZ
