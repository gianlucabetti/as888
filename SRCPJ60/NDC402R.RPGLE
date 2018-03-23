000100981215     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(QILE)
000200981215     H*PARMS BNDDIR(PJXBND PJCBND)
000300951004     H*PARMS CVTOPT(*DATETIME)
000400940223     H DECEDIT('0,') DATEDIT(*DMY.)
000500940117      *
000600951013    > *         LANCIO STAMPA REGISTRO DICHIARAZIONI DI INTENTO
000700951013      *        -------------------------------------------------
000800940223      *
000900940223      *
001000940223      * DOCUMENTI MICROANALISI :
001100940223      *
001200940223      * NOTE TECNICHE :
001300950301    > *  1) ELIMINARE LE SPECIFICHE RELATIVE AL COMMIT/ROLLBACK SE
001400950301    > *     QUESTO NON DEVE ESSERE GESTITO
001500941214      *
001600940223      * PERSONALIZZAZIONI
001700940223      *
001800940223      *
001900940223      *
002000940223      *               MAPPA INDICATORI STD
002100940223      *
002200940223      *  02           PROTECT CAMPI CHIAVE SE OPZ=MODIFICA
002300940223      *  21           GENERICO OPERAZIONI I/O
002400940223      *  22           GENERICO ERRORE OPERAZIONI I/O
002500940223      *  30           SFLDSP
002600940223      * N31           SFLCLR
002700940128      *  31           SFLDSPCTL
002800940128      *  32           SFLNXTCHG
002900940128      *  33           SFLEND
003000940128      *  39           OF PRTF
003100991217      *  40 <---> 49  DSPATR ERRORI SU SFL
003200940317    > *  Specificare l'uso dei singoli indicatori
003300940315      *  50 <---> 98  ERRORI SU VIDEO
003400940317    > *  Specificare l'uso dei singoli indicatori
003500940510      *  96           ERRORE SPECIALE : ERRORI IN ALTRE VIDEATE
003600940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
003700940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
003800940128      *  99           INDIC. GENERALE DI ERRORE
003900940128     F*----------------------------------------------------*
004000951013    >FANMEL01L  IF   E           K DISK
004100951013     F*------------
004200951011    >FNDC402D   CF   E             WORKSTN
004300951011     F                                     RENAME(C402D1:FMTD1)
004400951011     F                                     RENAME(C402D2:FMTD2)
004500951011     F                                     RENAME(C402T1:FMTT1)
004600951011     F                                     RENAME(C402Z1:FMTZ1)
004700951011     F                                     RENAME(C402Z2:FMTZ2)
004800940201     F                                     INFDS(DSFMT)
004900940128     D*----------------------------------------------------*
005000940506     D* Schiere per la gestione dei tasti di funzione
005100940506     D $FM             S             27    DIM(24)
005200940506     D $FX             S             50    DIM(24)
005300940506     D $FC             S              1    DIM(24)
005400940506     D $FR             S              1    DIM(24)
005500940506     D* Video D1
005600940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
005700940506     D $FX1            S             50    DIM(24) ALT($FM1)
005800940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
005900940506     D $FR1            S              1    DIM(24) ALT($FC1)
006000951011     D* Video D2
006100951011     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
006200951011     D $FX2            S             50    DIM(24) ALT($FM2)
006300951011     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
006400951011     D $FR2            S              1    DIM(24) ALT($FC2)
006500940128     D*----------------------------------------------------*
006600950726     D $D1L01          S              3  0
006700950726     D $D1L02          S              3  0
006800951011     D $D2L01          S              3  0
006900951011     D $D2L02          S              3  0
007000940127     D*-------------
007100940127     D* Passaggio Parametri
007200940127     D KPJBA         E DS
007300940318     D*-------------
007400940318     D* DS Esterna gestione messaggi per COMIT
007500940318     D XMSGDS        E DS
007600940318     D  STS          E                     EXTFLD(MSGSTS)
007700940721     D                                     DIM(333)
007800940318     D  JBA          E                     EXTFLD(MSGJBA)
007900940721     D                                     DIM(502)
008000950301     D*-------------
008100950301     D* DS gestione gruppo  o azienda
008200950301     D XPAOUT          DS          2000
008300950126     D*-------------
008400950126     D* Reperimento nome PGM
008500950126     D STATUS         SDS           333
008600950126     D  DSPGM            *PROC
008700950411     D  PARMS            *PARMS
008800941209     D*-------------
008900941209     D* Dati di ambiente ottenuti da XSOC
009000941209    >D SOC001        E DS                  EXTNAME(XSOC001DS)
009100941209     D*-------------
009200941209     D* DS Interna per dati di output di XSOC
009300941209     D XSOCDS          DS          1000
009400940203     D*-------------
009500940201     D DSFMT           DS
009600940506     D  $TASTO               369    369
009700940201     D  NRG                  370    370
009800940201     D  NCL                  371    371
009900950314     D XUKDS         E DS                  EXTNAME(XUSRKEYDS)
010000940201     D*-------------
010100940201     D* posizione cursore
010200940201     D CURSOR          DS
010300940223     D  RIRI                   1      2B 0 INZ
010400940201     D  R$$                    2      2
010500940223     D  COCO                   3      4B 0 INZ
010600940201     D  C$$                    4      4
010700940225     D*-------------
010800951011     D* Parametri pgm NDC403R
010900951011    >D NDC403DS      E DS
011000031118     D*-------------
011100031118A1047D* Passaggio Parametri al PGM di manutenzione intestazione
011200031118A1047D NDC098DS      E DS                  INZ
011300951011     D*-------------
011400951012     D* Ricerca legende
011500951012     D A964DS        E DS                  EXTNAME(ANA964DS)
011600951012     D*-------------
011700951011     D* Controlli validità legende
011800951011     D X07DS         E DS                  EXTNAME(X07VALDS)
011900951004     D*-------------
012000951016     D* Sottodefinizione campo MELSTRINGA
012100951016     D PLAREG        E DS                  EXTNAME(PLAREGDS)
012200951016     D*-------------
012300940616     D* Costanti
012400951013     D DatMin          C                   const('0001-01-01')
012500951016     D DatMax          C                   const('9999-12-31')
012600940225     D*-------------
012700940506     D* Tasti di funzione
012800940506     D F01             C                   CONST(X'31')
012900940506     D F02             C                   CONST(X'32')
013000940506     D F03             C                   CONST(X'33')
013100940506     D F04             C                   CONST(X'34')
013200940506     D F05             C                   CONST(X'35')
013300940506     D F06             C                   CONST(X'36')
013400940506     D F07             C                   CONST(X'37')
013500940506     D F08             C                   CONST(X'38')
013600940506     D F09             C                   CONST(X'39')
013700940506     D F10             C                   CONST(X'3A')
013800940506     D F11             C                   CONST(X'3B')
013900940506     D F12             C                   CONST(X'3C')
014000940506     D F13             C                   CONST(X'B1')
014100940506     D F14             C                   CONST(X'B2')
014200940506     D F15             C                   CONST(X'B3')
014300940506     D F16             C                   CONST(X'B4')
014400940506     D F17             C                   CONST(X'B5')
014500940506     D F18             C                   CONST(X'B6')
014600940506     D F19             C                   CONST(X'B7')
014700940506     D F20             C                   CONST(X'B8')
014800940506     D F21             C                   CONST(X'B9')
014900940506     D F22             C                   CONST(X'BA')
015000940506     D F23             C                   CONST(X'BB')
015100940506     D F24             C                   CONST(X'BC')
015200940506     D ENTER           C                   CONST(X'F1')
015300940506     D ROLDWN          C                   CONST(X'F4')
015400940506     D ROLLUP          C                   CONST(X'F5')
015500950606     D*-------------
015600950606     D* Sottotitolo di testata
015700950728     D SOTTIT          C                   CONST('                            ')
015800950606     D*-------------
015900950606     D* Variabili per gestione videate
016000950606     D $Gest           S              2A
016100950606     D $Fine           S              1A
016200950606     D $InzD1          S              1A
016300951011     D $InzD2          S              1A
016400950606     D $LastG          S              2A
016500950606     D $LastV          S              2A
016600950606     D*-------------
016700950606     D* Variabili appoggio sempre presenti in tutte le anagrafiche
016800950606     D CurR            S              2S 0
016900950606     D CurC            S              2S 0
017000950606     D Win             S             99A
017100950606     D*-------------
017200950606     D* Variabili appoggio specifiche della singola anagrafica
017300950728     D*-------------
017400950606     D*  Indici
017500950606     D X               S              3S 0
017600950606     D Y               S              3S 0
017700950606     D*-------------
017800000118C1309D ZDateISO        S               D
017900940127     C*----------------------------------------------------*
018000940127     C*                MAIN LINE PROGRAM
018100940127     C*----------------------------------------------------*
018200940127     C*
018300940223     C* inizializzazione variabili
018400940223     C                   EXSR      INZVAR
018500940223     C*
018600940223     C     $FINE         DOWEQ     *OFF
018700940202     C     $GEST         CASEQ     'D1'          GESD1
018800951011     C     $GEST         CASEQ     'D2'          GESD2
018900941027     C                   ENDCS
019000941027     C                   ENDDO
019100940201     C*
019200940325     C* fine programma
019300940325     C                   EXSR      ENDPGM
019400940325     C/EJECT
019500940325     C************************************************************
019600950731     C* FINE PROGRAMMA
019700940325     C************************************************************
019800940325     C     ENDPGM        BEGSR
019900941214     C*
020000941214     C* se ho confermato l'uscita proseguo
020100941214     C     $FINE         IFEQ      *ON
020200940325     C*
020300940325     C                   SETON                                            RT
020400940616     C                   RETURN
020500941214     C*
020600941214     C                   ENDIF
020700940325     C*
020800940325     C                   ENDSR
020900940325     C/EJECT
021000951011     C************************************************************
021100951011     C* RICERCHE
021200951011     C************************************************************
021300951011     C     SEARCH        BEGSR
021400951011     C*
021500951011     C* determino Riga/Colonna del cursore
021600951011     C                   MOVE      NRG           R$$
021700951011     C                   MOVE      NCL           C$$
021800951011     C                   Z-ADD     RIRI          CURR
021900951011     C                   Z-ADD     COCO          CURC
022000951011     C*
022100951011   B1C                   SELECT
022200951011     C*
022300951011     C* FMTD1  - Clienti/Fornitori
022400951011     C     H1NMFL        WHENEQ    'D1COF'
022500951011     C                   RESET                   A964DS
022600951011     C                   MOVEL     '01'          OPZ964
022700951011     C                   MOVEL     'ANDIZ000'    TRC964
022800951011     C                   MOVEL     'DSPCLIFOR'   CAM964
022900951011     C                   MOVEL     *OFF          ALL964
023000951011     C                   MOVEL     A964DS        KPJBU
023100951011     C                   CALL      'ANA964R'
023200951011     C                   PARM                    KPJBA
023300951011     C                   MOVEL     KPJBU         A964DS
023400951011     C*
023500951011     C     VAL964        IFNE      *BLANK
023600951011     C                   MOVEL     VAL964        D1COF
023700951011     C                   MOVEL     DES964        D1COFD
023800951011     C                   ENDIF
023900951011     C* imposto pos. del cursore
024000951011     C                   Z-ADD     CURR          H1RIGA
024100951011     C                   Z-ADD     CURC          H1COLO
024200951011     C*
024300951011     C* ricerca non abilitata in questa posizione
024400951011   X1C                   OTHER
024500951011     C* imposto pos. del cursore
024600951011     C                   Z-ADD     CURR          H1RIGA
024700951011     C                   Z-ADD     CURC          H1COLO
024800951011     C                   SETON                                        9899
024900951011     C*
025000951011   E1C                   ENDSL
025100951011     C*
025200951011     C                   MOVE      *HIVAL        $LASTV
025300951011     C*
025400951011     C                   ENDSR
025500951011     C/EJECT
025600940225     C************************************************************
025700940225     C* GESTIONE EMISSIONE TASTI FUNZIONE
025800940225     C************************************************************
025900940225     C     GESPIE        BEGSR
026000940225     C*
026100940225     C                   SELECT
026200940225     C*
026300940506     C     $GEST         WHENEQ    'D1'
026400940506     C     $ULKD1        IFEQ      0
026500940506     C* riempimento opzioni
026600940506     C                   EXSR      F24D1
026700940506     C                   ELSE
026800940506     C                   WRITE     FMTZ1
026900940506     C                   ENDIF
027000951011     C*
027100951011     C     $GEST         WHENEQ    'D2'
027200951011     C     $ULKD2        IFEQ      0
027300951011     C                   EXSR      F24D2
027400951011     C                   ELSE
027500951011     C                   WRITE     FMTZ2
027600951011     C                   ENDIF
027700940225     C*
027800940225     C                   ENDSL
027900940225     C*
028000940225     C                   ENDSR
028100940131     C/EJECT
028200940131     C************************************************************
028300951011     C* GESTIONE VIDEO D1
028400940131     C************************************************************
028500940127     C     GESD1         BEGSR
028600940225     C* inizializzazione videata
028700941117   B1C     $INZD1        IFEQ      *ON
028800940127     C                   EXSR      INZD1
028900940223     C                   MOVE      *OFF          $INZD1
029000941117   E1C                   ENDIF
029100940225     C* emissione piede videata se proveniente da altra
029200940225     C* salvataggio valore formato di provenienza
029300941117   B1C     $LASTV        IFNE      'D1'
029400940207     C                   WRITE     FMTT1
029500940225     C                   EXSR      GESPIE
029600940202     C                   MOVE      $LASTV        $LASTG
029700940202     C                   MOVE      'D1'          $LASTV
029800941117   E1C                   ENDIF
029900940707     C*
030000940707     C* se esistono errori sulla videata
030100940707     C* emetto la write del formato a indicatori spenti per vedere
030200940707     C* le eventuali decodifiche
030300941117   B1C     *IN99         IFEQ      *ON
030400940707     C     $GEST         ANDEQ     'D1'
030500940707     C                   MOVEA     *IN           WIN
030600940707     C                   MOVE      *ALL'0'       IN5098           49
030700940707     C                   MOVEA     IN5098        *IN(50)
030800940707     C                   WRITE     FMTD1
030900940707     C                   MOVEA     WIN           *IN
031000941117   E1C                   ENDIF
031100950731     C*
031200940203     C                   EXFMT     FMTD1
031300950731   E1C*
031400940506     C* controllo abilitazione tasti funzione
031500940506     C                   EXSR      CTRKD1
031600940506     C*
031700941117   B1C     *IN99         IFEQ      *OFF
031800940131     C*
031900941117   B2C                   SELECT
032000951011     C* F2=Decodifica
032100951011     C     $TASTO        WHENEQ    F02
032200951011     C                   EXSR      F02D1
032300940131     C* F3=Fine
032400940506     C     $TASTO        WHENEQ    F03
032500940309     C                   EXSR      F03D1
032600951011     C* F4=Ricerca
032700951011     C     $TASTO        WHENEQ    F04
032800951011     C                   EXSR      SEARCH
032900940223     C* F8=Successivo
033000940506     C     $TASTO        WHENEQ    F08
033100940309     C                   EXSR      F08D1
033200940131     C* F12=Ritorno
033300940506     C     $TASTO        WHENEQ    F12
033400940309     C                   EXSR      F12D1
033500940315     C* F24=Altri tasti
033600940506     C     $TASTO        WHENEQ    F24
033700940315     C                   EXSR      F24D1
033800940131     C*
033900941117   X2C                   OTHER
034000950731     C*
034100940127     C                   EXSR      CTRD1
034200940203     C*
034300951004     C     *IN99         IFEQ      *OFF
034400951011     C                   MOVE      'D2'          $GEST
034500951011     C                   MOVE      *ON           $INZD2
034600951004     C                   ENDIF
034700951004     C*
034800941117   E2C                   ENDSL
034900940224     C*
035000941117   E1C                   ENDIF
035100941027     C*
035200940117     C                   ENDSR
035300940506     C/EJECT
035400951011     C************************************************************
035500951011     C* INIZIALIZZAZIONE VIDEATA D1
035600951011     C************************************************************
035700951011     C     INZD1         BEGSR
035800951011     C*
035900951011     C                   CLEAR                   FMTD1
036000951011     C*
036100951011     C                   ENDSR
036200951011     C/EJECT
036300951011     C************************************************************
036400951011     C* GESTIONE F02 VIDEO D1
036500951011     C************************************************************
036600951011     C     F02D1         BEGSR
036700951011     C*
036800951011     C* per decodificare la videata richiamo la routine dei ctrl
036900951011     C* e poi disattivo gli eventuali messaggi di errore
037000951011     C                   EXSR      CTRD1
037100951011     C*
037200951011     C                   MOVE      *ALL'0'       IN5098
037300951011     C                   MOVEA     IN5098        *IN(50)
037400951011     C*
037500951011     C                   ENDSR
037600951011     C/EJECT
037700951011     C************************************************************
037800951011     C* GESTIONE F03 VIDEO D1
037900951011     C************************************************************
038000951011     C     F03D1         BEGSR
038100951011     C*
038200951011     C                   MOVE      *ON           $FINE
038300951011     C* fine programma
038400951011     C                   EXSR      ENDPGM
038500951011     C*
038600951011     C                   ENDSR
038700951011     C/EJECT
038800951011     C************************************************************
038900951011     C* GESTIONE F08 VIDEO D1
039000951011     C************************************************************
039100951011     C     F08D1         BEGSR
039200951011     C*
039300951011     C                   MOVE      *ON           $FINE
039400951011     C*
039500951011     C                   ENDSR
039600951011     C/EJECT
039700951011     C************************************************************
039800951011     C* GESTIONE F12 VIDEO D1
039900951011     C************************************************************
040000951011     C     F12D1         BEGSR
040100951011     C*
040200951011     C                   MOVE      *ON           $FINE
040300951011     C*
040400951011     C                   ENDSR
040500951011     C/EJECT
040600951011     C************************************************************
040700951011     C* GESTIONE F24 VIDEO D1
040800951011     C************************************************************
040900951011     C     F24D1         BEGSR
041000951011     C*
041100951011     C                   MOVEA     $FX1          $FX
041200951011     C                   MOVEA     $FC1          $FC
041300951011     C                   MOVEA     $FR1          $FR
041400951011     C                   Z-ADD     $ULKD1        $ULK
041500951011     C                   Z-ADD     $D1L01        $L01
041600951011     C                   Z-ADD     $D1L02        $L02
041700951011     C                   EXSR      RIEKEY
041800951011     C                   Z-ADD     $ULK          $ULKD1            3 0
041900951011     C                   MOVEA     $FC           $FC1
042000951011     C                   MOVEL     $KEY1         Z1KE1
042100060224C1913C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
042200951011     C                   MOVEL     $KEY2         Z1KE2
042300951011     C                   WRITE     FMTZ1
042400951011     C*
042500951011     C                   ENDSR
042600951011     C/EJECT
042700951011     C************************************************************
042800951011     C* CONTROLLO VIDEATA
042900951011     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
043000951011     C* 95 96 97 98
043100951011     C************************************************************
043200951011     C     CTRD1         BEGSR
043300951011     C*
043400951011     C                   SETOFF                                       99
043500951011     C* non imposto pos. del cursore
043600951011     C                   Z-ADD     0             H1RIGA
043700951011     C                   Z-ADD     0             H1COLO
043800951011     C*
043900951013     C* Controllo C/F
044000951011     C                   MOVE      *blank        d1cofd
044100951011     C                   RESET                   X07DS
044200951011     C                   MOVE      '1'           X07RIC
044300951011     C                   MOVEL     'ANDIZ000'    X07TRC
044400951011     C                   MOVEL     'DSPCLIFOR'   X07CAM
044500951011     C                   MOVEL     d1cof         X07VAL
044600951011     C                   MOVE      *OFF          X07ALL
044700961008     C                   CALLb     'X07VALR'
044800951011     C                   PARM                    X07DS
044900951011   B3C     X07ERR        IFEQ      '0'
045000951011     C                   MOVEL     X07DES        d1cofd
045100951011     C                   ELSE
045200951011     C                   seton                                        5099
045300951011   E3C                   ENDIF
045400951011     C*
045500951011     C                   ENDSR
045600951011     C/EJECT
045700951011     C************************************************************
045800951011     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
045900951011     C************************************************************
046000951011     C     CTRKD1        BEGSR
046100951011     C*
046200951011     C                   SETOFF                                       99
046300951011     C                   MOVEA     $FC1          $FC
046400951011     C                   EXSR      CTRKEY
046500951011     C     *IN99         IFEQ      *ON
046600951011     C                   SETON                                        97
046700951011     C                   ENDIF
046800951011     C*
046900951011     C                   ENDSR
047000951011     C/EJECT
047100951011     C************************************************************
047200951011     C* GESTIONE VIDEO 2
047300951011     C************************************************************
047400951011     C     GESD2         BEGSR
047500951011     C*
047600951011   B1C     $INZD2        IFEQ      *ON
047700951011     C                   EXSR      INZD2
047800951011     C                   MOVE      *OFF          $INZD2
047900951011   E1C                   ENDIF
048000951011     C*
048100951011   B1C     $LASTV        IFNE      'D2'
048200951011     C                   WRITE     FMTT1
048300951011     C                   EXSR      GESPIE
048400951011     C                   MOVE      $LASTV        $LASTG
048500951011     C                   MOVE      'D2'          $LASTV
048600951011   E1C                   ENDIF
048700951011     C*
048800951011     C* se esistono errori sulla videata
048900951011     C* emetto la write del formato a indicatori spenti per vedere
049000951011     C* le eventuali decodifiche
049100951011   B1C     *IN99         IFEQ      *ON
049200951011     C                   MOVEA     *IN           WIN
049300951011     C                   MOVE      *ALL'0'       IN5098           49
049400951011     C                   MOVEA     IN5098        *IN(50)
049500951011     C                   WRITE     FMTD2
049600951011     C                   MOVEA     WIN           *IN
049700951011   E1C                   ENDIF
049800951011     C*
049900951011     C                   EXFMT     FMTD2
050000951011     C*
050100951011     C* controllo abilitazione tasti funzione
050200951011     C                   EXSR      CTRKD2
050300951011     C*
050400951011   B1C     *IN99         IFEQ      *OFF
050500951011     C*
050600951011   B2C                   SELECT
050700951011     C* F2=Decodifica
050800951011     C     $TASTO        WHENEQ    F02
050900951011     C                   EXSR      F02D2
051000951011     C* F3=Fine
051100951011     C     $TASTO        WHENEQ    F03
051200951011     C                   EXSR      F03D2
051300951011     C* F12=Ritorno
051400951011     C     $TASTO        WHENEQ    F12
051500951011     C                   EXSR      F12D2
051600031118A1047C* F14=Impostazione intestazione
051700031118  "  C     $TASTO        WHENEQ    F14
051800031118A1047C                   EXSR      F14D2
051900990104     C* F21=Pe.batch
052000990104     C     $TASTO        WHENEQ    F21
052100990104     C                   EXSR      F21D2
052200951011     C* F24=Altri tasti
052300951011     C     $TASTO        WHENEQ    F24
052400951011     C                   EXSR      F24D2
052500951011     C*
052600951011     C                   OTHER
052700951011     C                   EXSR      CTRD2
052800951011     C     *IN99         IFEQ      *OFF
052900951011     C     $tasto        andeq     F06
053000951011     C                   EXSR      LANCIO
053100951011     C                   ENDIF
053200951011     C*
053300951011   E2C                   ENDSL
053400951011     C*
053500951011   E1C                   ENDIF
053600951011     C*
053700951011     C                   ENDSR
053800951011     C/EJECT
053900951011     C************************************************************
054000951011     C* INIZIALIZZAZIONE VIDEATA D2
054100951011     C************************************************************
054200951011     C     INZD2         BEGSR
054300951011     C*
054400951011     C                   CLEAR                   FMTD2
054500951011     C*
054600951011     C                   MOVE      XSCON         D2ON1
054700951011     C                   MOVE      XSCOFF        D2OF1
054800991217B0136C                   MOVE      XSCON         D2ON2
054900991217B0136C                   MOVE      XSCOFF        D2OF2
055000031118A1047C                   MOVE      XSCON         D2ON3
055100031118A1047C                   MOVE      XSCOFF        D2OF3
055101160701R229 C                   MOVE      XSCON         D2ON4
055102160701R229 C                   MOVE      XSCOFF        D2OF4
055200951011     C*
055300951011     C                   move      d1cof         d2cof
055400951011     C                   move      d1cofd        d2cofd
055500951016     C*
055600951026     C                   CALLb     'XDT4000'
055700951016     C                   PARM                    XDTAMG           10
055800000118C1309C*****              PARM      udate         XDTGMA            6 0
055900000118C1309C                   PARM      ZDate         XDTGMA            6 0
056000951016     C                   PARM      4             XDTSTA            1 0
056100951016     C                   movel     XDTAMG        d2anno
056200951016     C*
056300951016     C* Reperisco ultimo nr. protocollo stampato su bollato
056400951016     C* e lo propongo (incrementato) nel nr. inizio
056500951016     C                   if        d2cof = 'C'
056600951016     C                   move      'PLACLI'      meltipo
056700951016     C                   else
056800951016     C                   move      'PLAFOR'      meltipo
056900951016     C                   endif
057000951016     C                   move      xscsoc        melsocieta
057100951016     C                   move      *blank        melunita
057200951016     C                   move      *blank        melctb
057300951016     C                   move      *blank        melkeygest
057400951016     C                   move      DatMax        meldata
057500951016     C     k6mel1        setgt     anmel01l
057600951016     C     k5mel1        readpe    anmel01l                               21
057700951016     C                   if        *in21 = *off
057800951016     C                   movel     melstringa    plareg
057900061219D2104C* Se l'anno è lo stesso che sto stampando...
058000061219D2104c                   if        AnnoProt=d2anno
058100951016     C                   move      unrprot       d2nri
058200951016     C                   add       1             d2nri
058300061219D2104C                   else
058400061219  "  c                   z-add     1             d2nri
058500061219D2104C                   endif
058600951016     C                   endif
058700951016     C*
058800951016     C* Reperisco ultimo nr. protocollo inserito e lo propongo nel nr. fine
058900951016     C                   MOVE      '1'           XNMRIC
059000951016     C                   MOVE      XSCSOC        XNMSOC
059100951016     C                   MOVE      *BLANKS       XNMUNI
059200951016     C                   MOVE      *BLANKS       XNMCTB
059300951016     C                   MOVE      *BLANKS       XNMKEY
059400951016     C                   if        d2cof = 'C'
059500951016     C                   MOVE      'PCLI'        XNMSER
059600951016     C                   else
059700951016     C                   MOVE      'PFOR'        XNMSER
059800951016     C                   endif
059900951016     C                   MOVE      *OFF          XNMCMT
060000951016     C                   MOVE      *OFF          XNMIVA
060100951016     C                   MOVE      *OFF          XNMREG
060200951026     C                   CALLb     'XDT4000'
060300951016     C                   PARM                    XDTAMG           10
060400000118C1309C*****              PARM      udate         XDTGMA            6 0
060500000118C1309C                   PARM      ZDate         XDTGMA            6 0
060600951016     C                   PARM      4             XDTSTA            1 0
060700951016     C                   MOVE      XDTAMG        XNMDAT
060800961008     C                   CALLb     'XNMR'        PNMR
060900951016     C     XNMERR        IFEQ      *OFF
061000951016     C                   Z-ADD     XNMNUM        d2nrf
061100951016     C                   END
061200951016     C*
061300951016     C                   move      xscoff        d2bol
061400000118C1309C*****              move      udate         d2dtb
061500000118C1309C                   move      ZDate         d2dtb
061600991217B0136C                   Eval      D2StpDIT    = XScOff
061700991217  "   *Se Clienti, nascondo "Stampa data invio lettere"
061800991217  "   *(stamperà sempre la data protocollo interno)
061900991217  "  C                   If        D2Cof = 'C'
062000991217  "  C                   Seton                                        40
062100991217  "  C                   Else
062200991217  "  C                   Setoff                                       40
062300991217B0136C                   EndIf
062400951016     C*
062500031118A1047c                   move      xscoff        d2intesta
062501160701R229 c                   move      xscoff        d2StpProt
062600031118     C*
062700951011     C                   ENDSR
062800951011     C/EJECT
062900951011     C************************************************************
063000951011     C* GESTIONE F02 VIDEO D2
063100951011     C************************************************************
063200951011     C     F02D2         BEGSR
063300951011     C*
063400951011     C* per decodificare la videata richiamo la routine dei ctrl
063500951011     C* e poi disattivo gli eventuali messaggi di errore
063600951011     C                   EXSR      CTRD2
063700951011     C*
063800951011     C                   MOVE      *ALL'0'       IN5098
063900951011     C                   MOVEA     IN5098        *IN(50)
064000951011     C*
064100951011     C                   ENDSR
064200951011     C/EJECT
064300951011     C************************************************************
064400951011     C* GESTIONE F03 VIDEO D2
064500951011     C************************************************************
064600951011     C     F03D2         BEGSR
064700951011     C*
064800951011     C                   MOVE      *ON           $FINE
064900951011     C* fine programma
065000951011     C                   EXSR      ENDPGM
065100951011     C*
065200951011     C                   ENDSR
065300951011     C/EJECT
065400951011     C************************************************************
065500951011     C* GESTIONE F12 VIDEO D2
065600951011     C************************************************************
065700951011     C     F12D2         BEGSR
065800951011     C*
065900951011     C                   MOVE      'D1'          $GEST
066000951011     C*
066100951011     C                   ENDSR
066200031118A1047C************************************************************
066300031118     C* Impostazione intestazione
066400031118     C************************************************************
066500031118     C     F14D2         BEGSR
066600031118     C*
066700031118     C                   reset                   NDC098DS
066800031118     C                   eval      tip098='DICHINT'
066900031118     C                   move      d2cof         tip098
067000031118     C                   eval      opz098='02'
067100031118     c                   eval      kpjbu = ndc098ds
067200031118     C                   CALL      'NDC098R'
067300031118     C                   parm                    kpjba
067400031118     c                   eval      ndc098ds = kpjbu
067500031118     c* F3=Fine
067600031118     c                   if        ret098='1'
067700031118     C                   MOVE      *ON           $FINE
067800031118     C                   EXSR      ENDPGM
067900031118     c                   endif
068000031118     C*
068100031118A1047C                   ENDSR
068200951011     C/EJECT
068300990104     C************************************************************
068400990104     C* GESTIONE F21 VIDEO D2
068500990104     C************************************************************
068600990104     C     F21D2         BEGSR
068700990104     C*
068800990104     C                   MOVE      'C403'        KCOAZ
068900990104     C                   CALL      'BCH09'
069000990104     C                   PARM                    KPJBA
069100990104     C*
069200990104     C                   ENDSR
069300990104     C/EJECT
069400951011     C************************************************************
069500951011     C* GESTIONE F24 VIDEO D2
069600951011     C************************************************************
069700951011     C     F24D2         BEGSR
069800951011     C*
069900951011     C                   MOVEA     $FX2          $FX
070000951011     C                   MOVEA     $FC2          $FC
070100951011     C                   MOVEA     $FR2          $FR
070200951011     C                   Z-ADD     $ULKD2        $ULK
070300951011     C                   Z-ADD     $D2L01        $L01
070400951011     C                   Z-ADD     $D2L02        $L02
070500951011     C                   EXSR      RIEKEY
070600951011     C                   Z-ADD     $ULK          $ULKD2            3 0
070700951011     C                   MOVEA     $FC           $FC2
070800951011     C                   MOVEL     $KEY1         Z2KE1
070900060224C1913C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
071000040219D1540C                   MOVEL     $KEY2         Z2KE2
071100951011     C                   WRITE     FMTZ2
071200951011     C*
071300951011     C                   ENDSR
071400951011     C/EJECT
071500951011     C************************************************************
071600951011     C* CONTROLLO VIDEO 2
071700951011     C************************************************************
071800951011     C     CTRD2         BEGSR
071900951011     C*
072000951011     C                   SETOFF                                       99
072100951011     C* non imposto pos. del cursore
072200951011     C                   Z-ADD     0             H1RIGA
072300951011     C                   Z-ADD     0             H1COLO
072400951011     C*
072500951016     C* Anno
072600951011     C                   if        d2anno = 0
072700951011     C                   seton                                        6099
072800951011     C                   endif
072900951011     C*
073000951016     C* Nr. Inizio/Fine
073100951011     C                   if        d2nri > d2nrf
073200951011     C                   seton                                        6199
073300951011     C                   endif
073400951011     C*
073500951016     C* Stampa/Data bollato
073600951011     C                   if        d2bol <> xscon and d2bol <> xscoff
073700951011     C                   seton                                        6299
073800951011     C                   endif
073900951011     C*
074000951013     C                   if        d2dtb <> 0
074100951026     C                   CALLb     'XDT4000'
074200951013     C                   PARM                    XDTAMG           10
074300951013     C                   PARM      d2dtb         XDTGMA            6 0
074400951013     C                   PARM      3             XDTSTA            1 0
074500951013     C                   if        XDTSTA <> 0
074600951013     C                   seton                                        6399
074700951013     C                   else
074800951013     C                   move      XDTAMG        databoll         10
074900951013     C                   endif
075000951013     C                   endif
075100951013     C*
075200951013     C                   if        d2bol = xscon and d2dtb = 0
075300951013     C                   seton                                        6599
075400951013     C                   endif
075500951013     C*
075600951013     C* Se richiesto bollato controllo inesistenza su ANMEL
075700951013     C                   if        *in99 = *off and d2bol = xscon
075800951013     C                   if        d2cof = 'C'
075900951013    >C                   MOVEL     'PLACLI'      MELTIPO
076000951013     C                   else
076100951013    >C                   MOVEL     'PLAFOR'      MELTIPO
076200951013     C                   endif
076300951013    >C                   MOVEL     xscsoc        MELSOCIETA
076400951013    >C                   MOVEL     *blank        MELUNITA
076500951013    >C                   MOVEL     *blank        MELCTB
076600951013    >C                   MOVEL     *blank        MELKEYGEST
076700951013    >C                   MOVEL     databoll      MELDATA
076800951013    >C     k6mel1        chain     anmel000                           21
076900951013     C                   if        *in21 = *off
077000951013     C                   seton                                        6499
077100951013     C                   endif
077200951013     C                   endif
077300991217B0136 *
077400991217  "   * Stampa Data Invio
077500991217  "  C                   If        D2StpDIT   <> XScOn   AND
077600991217  "  C                             D2StpDIT   <> XScOff
077700991217  "  C                   SetOn                                        6699
077800991217  "  C                   EndIf
077900991217B0136 *
078000031118A1047 *
078100031118  "   * Stampa INTESTAZIONE
078200031118  "  C                   If        D2Intesta  <> XScOn   AND
078300031118  "  C                             D2Intesta  <> XScOff
078400031118  "  C                   SetOn                                        6799
078500031118  "  C                   EndIf
078600031118A1047 *
078601160701R229  *
078602160701  "   * Stampa protocollo dichiarazione inviata
078603160701  "  C                   If        D2Stpprot  <> XScOn   AND
078604160701  "  C                             D2StpProt  <> XScOff
078605160701  "  C                   SetOn                                        6899
078606160701  "  C                   EndIf
078607160701R229  *
078700951011     C                   ENDSR
078800951011     C/EJECT
078900951011     C************************************************************
079000951011     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D2
079100951011     C************************************************************
079200951011     C     CTRKD2        BEGSR
079300951011     C*
079400951011     C                   SETOFF                                       99
079500951011     C                   MOVEA     $FC2          $FC
079600951011     C                   EXSR      CTRKEY
079700951011     C     *IN99         IFEQ      *ON
079800951011     C                   SETON                                        97
079900951011     C                   ENDIF
080000951011     C*
080100951011     C                   ENDSR
080200951011     C/EJECT
080300941205     C************************************************************
080400941205     C* CONTROLLO ABILITAZIONE TASTI
080500941205     C************************************************************
080600941205     C     CTRKEY        BEGSR
080700941205     C*
080800941205     C                   SELECT
080900941205     C     $TASTO        WHENEQ    F01
081000941205     C     $FC(1)        IFEQ      '0'
081100941205     C                   SETON                                        99
081200941205     C                   ENDIF
081300941205     C     $TASTO        WHENEQ    F02
081400941205     C     $FC(2)        IFEQ      '0'
081500941205     C                   SETON                                        99
081600941205     C                   ENDIF
081700941205     C     $TASTO        WHENEQ    F03
081800941205     C     $FC(3)        IFEQ      '0'
081900941205     C                   SETON                                        99
082000941205     C                   ENDIF
082100941205     C     $TASTO        WHENEQ    F04
082200941205     C     $FC(4)        IFEQ      '0'
082300941205     C                   SETON                                        99
082400941205     C                   ENDIF
082500941205     C     $TASTO        WHENEQ    F05
082600941205     C     $FC(5)        IFEQ      '0'
082700941205     C                   SETON                                        99
082800941205     C                   ENDIF
082900941205     C     $TASTO        WHENEQ    F06
083000941205     C     $FC(6)        IFEQ      '0'
083100941205     C                   SETON                                        99
083200941205     C                   ENDIF
083300941205     C     $TASTO        WHENEQ    F07
083400941205     C     $FC(7)        IFEQ      '0'
083500941205     C                   SETON                                        99
083600941205     C                   ENDIF
083700941205     C     $TASTO        WHENEQ    F08
083800941205     C     $FC(8)        IFEQ      '0'
083900941205     C                   SETON                                        99
084000941205     C                   ENDIF
084100941205     C     $TASTO        WHENEQ    F09
084200941205     C     $FC(09)       IFEQ      '0'
084300941205     C                   SETON                                        99
084400941205     C                   ENDIF
084500941205     C     $TASTO        WHENEQ    F10
084600941205     C     $FC(10)       IFEQ      '0'
084700941205     C                   SETON                                        99
084800941205     C                   ENDIF
084900941205     C     $TASTO        WHENEQ    F11
085000941205     C     $FC(11)       IFEQ      '0'
085100941205     C                   SETON                                        99
085200941205     C                   ENDIF
085300941205     C     $TASTO        WHENEQ    F12
085400941205     C     $FC(12)       IFEQ      '0'
085500941205     C                   SETON                                        99
085600941205     C                   ENDIF
085700941205     C     $TASTO        WHENEQ    F13
085800941205     C     $FC(13)       IFEQ      '0'
085900941205     C                   SETON                                        99
086000941205     C                   ENDIF
086100941205     C     $TASTO        WHENEQ    F14
086200941205     C     $FC(14)       IFEQ      '0'
086300941205     C                   SETON                                        99
086400941205     C                   ENDIF
086500941205     C     $TASTO        WHENEQ    F15
086600941205     C     $FC(15)       IFEQ      '0'
086700941205     C                   SETON                                        99
086800941205     C                   ENDIF
086900941205     C     $TASTO        WHENEQ    F16
087000941205     C     $FC(16)       IFEQ      '0'
087100941205     C                   SETON                                        99
087200941205     C                   ENDIF
087300941205     C     $TASTO        WHENEQ    F17
087400941205     C     $FC(17)       IFEQ      '0'
087500941205     C                   SETON                                        99
087600941205     C                   ENDIF
087700941205     C     $TASTO        WHENEQ    F18
087800941205     C     $FC(18)       IFEQ      '0'
087900941205     C                   SETON                                        99
088000941205     C                   ENDIF
088100941205     C     $TASTO        WHENEQ    F19
088200941205     C     $FC(19)       IFEQ      '0'
088300941205     C                   SETON                                        99
088400941205     C                   ENDIF
088500941205     C     $TASTO        WHENEQ    F20
088600941205     C     $FC(20)       IFEQ      '0'
088700941205     C                   SETON                                        99
088800941205     C                   ENDIF
088900941205     C     $TASTO        WHENEQ    F21
089000941205     C     $FC(21)       IFEQ      '0'
089100941205     C                   SETON                                        99
089200941205     C                   ENDIF
089300941205     C     $TASTO        WHENEQ    F22
089400941205     C     $FC(22)       IFEQ      '0'
089500941205     C                   SETON                                        99
089600941205     C                   ENDIF
089700941205     C     $TASTO        WHENEQ    F23
089800941205     C     $FC(23)       IFEQ      '0'
089900941205     C                   SETON                                        99
090000941205     C                   ENDIF
090100941205     C     $TASTO        WHENEQ    F24
090200941205     C     $FC(24)       IFEQ      '0'
090300941205     C                   SETON                                        99
090400941205     C                   ENDIF
090500941205     C                   ENDSL
090600941205     C*
090700941205     C                   ENDSR
090800940318     C/EJECT
090900940318     C******************************************************
091000940322     C* PGM PER GESTIONE MESSAGGI ERRORE                   *
091100940318     C******************************************************
091200940318     C     MSGPGM        BEGSR
091300940318     C*
091400940318     C                   Z-ADD     2             MSGMSG
091500940318     C                   MOVEL     'S'           MSGRSP
091600940318     C                   MOVEL     'CR'          MSGTPR
091700940318     C                   MOVEL     'S'           MSGEMV
091800940318     C                   Z-ADD     10            MSGRGP
091900940318     C                   MOVEL     'S'           MSGLCK
092000940318     C                   MOVEA     STATUS        STS
092100940318     C                   MOVEL     'N'           MSGCNL
092200940318     C                   MOVEL     'N'           MSGVID
092300940318     C                   MOVEL     'N'           MSGOPE
092400940318     C                   MOVEL     'N'           MSGSTP
092500950522     C                   CALL      'XMSGR'
092600940318     C                   PARM                    XMSGDS
092700940318     C*
092800940318     C                   ENDSR
092900940207     C/EJECT
093000951011     C************************************************************
093100951011     C* LANCIO STAMPA REGISTRO PROTOCOLLI
093200951011     C************************************************************
093300951011     C     LANCIO        BEGSR
093400951011     C*
093500951013     C                   move      xscsoc        soc403
093600951013     C                   move      d2cof         cof403
093700951011     C                   z-add     d2anno        anno403
093800951011     C                   z-add     d2nri         nri403
093900951011     C                   if        d2nrf = 0
094000951011     C                   move      *all'9'       nrf403
094100951011     C                   else
094200951011     C                   z-add     d2nrf         nrf403
094300951011     C                   endif
094400951011     C                   if        d2bol = xscon
094500951011     C                   move      *on           bol403
094600951013     C                   move      databoll      dtb403
094700951011     C                   else
094800951011     C                   move      *off          bol403
094900951013     C                   move      DatMin        dtb403
095000951011     C                   endif
095100991217     C*
095200991217B0136C                   If        D2StpDIT    = XScOn
095300991217  "  C                   Move      *On           StpDIT403
095400991217  "  C                   Else
095500991217  "  C                   Move      *Off          StpDIT403
095600991217B0136C                   endif
095700951011     C*
095800031118A1047C                   If        D2Intesta   = XScOn
095900031118  "  C                   Move      *On           Intesta403
096000031118  "  C                   Else
096100031118  "  C                   Move      *Off          Intesta403
096200031118A1047C                   endif
096201160701R229 C                   If        D2StpProt   = XScOn
096202160701  "  C                   Move      *On           StpProt403
096203160701  "  C                   Else
096204160701  "  C                   Move      *Off          StpProt403
096205160701R229 C                   endif
096300951011     C                   movel     ndc403ds      KPJBU
096400951011     C                   move      'C403'        KCOAZ
096500951013     C                   CALL      'BCH10'
096600951011     C                   PARM                    KPJBA
096700951011     C*
096800951013     C                   MOVE      'D1'          $GEST
096900951013     C                   MOVE      *ON           $INZD1                         inizializzione D1
097000951011     C*
097100951011     C                   ENDSR
097200951011     C/EJECT
097300940131     C************************************************************
097400940131     C* OPERAZIONI INIZIALI
097500940131     C************************************************************
097600940131     C     *INZSR        BEGSR
097700000118C1309C     *DMY          Move      Udate         ZDateISO
097800000118C1309C     *JobRun       Move      ZDateISO      ZDate             6 0
097900940131     C*
098000940127     C* Reperimento parametri
098100940127     C*
098200940117     C     *ENTRY        PLIST
098300940117     C                   PARM                    KPJBA
098400950726     C*
098500950726     C* reperisco le dimensioni delle variabili che conterranno i tasti
098600950726     C                   EVAL      $D1L01 = %Size(Z1Ke1)
098700950726     C                   EVAL      $D1L02 = %Size(Z1Ke2)
098800951011     C                   EVAL      $D2L01 = %Size(Z2Ke1)
098900951011     C                   EVAL      $D2L02 = %Size(Z2Ke2)
099000940506     C*
099100940506     C* Reperimento tasti di funzione
099200940506     C*
099300940506     C                   EXSR      REPKEY
099400940506     C*
099500940223     C                   ENDSR
099600940223     C/EJECT
099700940506     C************************************************************
099800940506     C* REPERIMENTO TASTI DI FUNZIONE
099900940506     C************************************************************
100000940506     C     REPKEY        BEGSR
100100951011     C*
100200940506     C                   MOVEA     $FM1          $FM
100300950721     C                   MOVEA     $Fc1          $Fc
100400940506     C                   EXSR      REPKKK
100500940506     C                   MOVEA     $FX           $FX1
100600940614     C*
100700951011     C                   MOVEA     $FM2          $FM
100800951011     C                   MOVEA     $FC2          $FC
100900951011     C                   EXSR      REPKKK
101000951011     C                   MOVEA     $FX           $FX2
101100951011     C*
101200940614     C                   ENDSR
101300940506     C/EJECT
101400940506     C************************************************************
101500940506     C* REPERIMENTO TASTI DI FUNZIONE
101600940506     C************************************************************
101700940506     C     REPKKK        BEGSR
101800940506     C*
101900940506     C                   CLEAR                   $FX
102000940506     C     1             DO        24            X
102100940506     C*
102200940506     C     $FM(X)        IFNE      *BLANK
102300950721     C     $Fc(X)        andne     '0'
102400940506     C*
102500940506     C                   MOVEL     $FM(X)        IDMSG
102600950522     C                   CALLB     'XRTVM'
102700940506     C                   PARM                    IDMSG            27
102800940506     C                   PARM                    TXTMSG          100
102900940506     C                   PARM                    ERRMSG            1
103000940506     C*
103100940506     C     ERRMSG        IFNE      *ON
103200940506     C                   MOVEL     TXTMSG        $FX(X)
103300940506     C                   ELSE
103400940506     C                   MOVEL     *ALL' '       $FX(X)
103500941027     C                   ENDIF
103600940506     C*
103700941027     C                   ENDIF
103800940506     C*
103900941027     C                   ENDDO
104000940506     C*
104100940506     C                   ENDSR
104200940509     C/EJECT
104300940223     C************************************************************
104400940223     C* INIZIALIZZAZIONE VARIABILI
104500940223     C************************************************************
104600940223     C     INZVAR        BEGSR
104700941201     C*
104800941201     C* Reperimento dati società
104900941201     C*
105000941115    >C                   MOVEL     'SOC001'      TIPXSC
105100941115     C                   MOVEL     *BLANK        SOCXSC
105200941115     C                   EXSR      REPSOC
105300941115     C     RTNXSC        IFNE      '1'
105400941115     C                   MOVEL     XSOCDS        SOC001
105500941115     C                   ELSE
105600941115     C                   RETURN
105700941115     C                   ENDIF
105800940715     C*
105900940715     C* Definizione dei tasti funzione utilizzabili a pgm
106000940715     C*
106100940715     C                   EXSR      DEFOPZ
106200940128     C*
106300940223     C* Variabili per gestione videate
106400940128     C*
106500940223     C                   MOVE      'D1'          $GEST                          fmt in gestione
106600940223     C                   MOVE      *OFF          $FINE                          fine pgm
106700940223     C                   MOVE      *ON           $INZD1                         inizializzione D1
106800951011     C                   MOVE      *ON           $INZD2                         inizializzione D2
106900940224     C                   MOVE      *BLANK        $LASTG                         fmt di provenienza
107000940224     C                   MOVE      *BLANK        $LASTV                         ultima emissione
107100940506     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
107200951011     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
107300940127     C*
107400940223     C* Variabili appoggio
107500940127     C*
107600940224     C                   Z-ADD     0             CURR
107700940224     C                   Z-ADD     0             CURC
107800940207     C*
107900940207     C* Valorizzazione campi univoci testate
108000940207     C*
108100940207     C                   CLEAR                   FMTT1
108200940207     C                   MOVEL     KNSIF         NOMSIF
108300941110     C                   MOVEL     XSCDSI        NOMDIT
108400941027     C                   MOVEL     DSPGM         NOMPGM
108500950331     C* recupero il sottotitolo (se c'è)
108600950331     C                   MOVEL     *BLANK        T1TIT
108700950331     C     SOTTIT        IFNE      *BLANK
108800950331     C                   MOVEL     SOTTIT        IDMSG
108900950522     C                   CALLB     'XRTVM'
109000950331     C                   PARM                    IDMSG            27
109100950331     C                   PARM                    TXTMSG          100
109200950331     C                   PARM                    ERRMSG            1
109300950331     C                   PARM      *ON           CTRMSG            1            centratura
109400950331     C                   PARM      30            LENMSG            3 0          lun output.
109500950331     C     ERRMSG        IFNE      *ON
109600950331     C                   MOVEL     TXTMSG        T1TIT
109700950331     C                   ELSE
109800950331     C                   MOVEL     *ALL'?'       T1TIT
109900950331     C                   ENDIF
110000950331     C                   ENDIF
110100940224     C*
110200940224     C* Inizializzazione/Decodifica prima videata
110300940224     C*
110400940224     C                   EXSR      INZD1
110500940224     C                   MOVE      *OFF          $INZD1
110600940127     C*
110700940117     C                   ENDSR
110800941209     C/EJECT
110900941209     C************************************************************
111000941209     C* REPERIMENTO DATI SOCIETA'
111100941209     C************************************************************
111200941209     C     REPSOC        BEGSR
111300941209     C*
111400950522     C                   CALLB     'XSOC'
111500941209     C                   PARM                    TIPXSC            6
111600941209     C                   PARM                    SOCXSC            3
111700941209     C                   PARM                    CDSXSC            9 0
111800941209     C                   PARM                    MODXSC            3
111900941209     C                   PARM      *BLANKS       RTNXSC            1
112000941209     C                   PARM                    XSOCDS
112100941209     C                   PARM                    KPJBA
112200941209     C*
112300941209     C                   ENDSR
112400950301     C/EJECT
112500940715     C************************************************************
112600940715     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
112700940715     C************************************************************
112800940715     C     DEFOPZ        BEGSR
112900940715     C*
113000951011     C* Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
113100940715     C* con S o N a seconda della utilizzabilità o meno
113200940715     C* del tasto funzione correlato
113300940715     C*
113400940715     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
113500940715     C* di compilazione
113600940715     C                   RESET                   $FC1
113700940715     C                   RESET                   $FR1
113800951011     C                   RESET                   $FC2
113900951011     C                   RESET                   $FR2
114000940715     C*
114100940715     C                   ENDSR
114200950403     C/EJECT
114300940128     C************************************************************
114400950606     C* DEFINIZIONE KLIST
114500940223     C************************************************************
114600950606     C     DefKlist      BEGSR
114700940223     C*
114800940223     C* klist
114900940207     C*
115000951013    >C* ANMEL01L
115100951013    >C     K6MEL1        KLIST
115200951013    >C                   KFLD                    MELTIPO
115300951013    >C                   KFLD                    MELSOCIETA
115400951013    >C                   KFLD                    MELUNITA
115500951013    >C                   KFLD                    MELCTB
115600951013    >C                   KFLD                    MELKEYGEST
115700951013    >C                   KFLD                    MELDATA
115800951016     C*
115900951016    >C     K5MEL1        KLIST
116000951016    >C                   KFLD                    MELTIPO
116100951016    >C                   KFLD                    MELSOCIETA
116200951016    >C                   KFLD                    MELUNITA
116300951016    >C                   KFLD                    MELCTB
116400951016    >C                   KFLD                    MELKEYGEST
116500951016     C*
116600951016     C* PLIST per XNUM
116700951016     C     PNMR          PLIST
116800951016     C                   PARM                    XNMRIC            1
116900951016     C                   PARM                    XNMSOC            3
117000951016     C                   PARM                    XNMUNI            8
117100951016     C                   PARM                    XNMCTB            2
117200951016     C                   PARM                    XNMKEY            8
117300951016     C                   PARM                    XNMSER            4
117400951016     C                   PARM                    XNMCMT            1
117500951016     C                   PARM                    XNMDAT           10
117600951016     C                   PARM                    XNMIVA            1
117700951016     C                   PARM                    XNMREG            1
117800951016     C                   PARM                    XNMERR            1
117900951016     C                   PARM                    XNMNUM            9 0
118000951016     C                   PARM                    XNMAUT            1
118100940127     C*
118200940117     C                   ENDSR
118300940509     C/EJECT
118400940509     C************************************************************
118500940509     C* RIEMPIMENTO TASTI FUNZIONE
118600940509     C************************************************************
118700940509     C     RIEKEY        BEGSR
118800940509     C*
118900940509     C* Input :
119000940509     C* - $FX = schiera con tutti i testi disponibili
119100940509     C* - $FC = schiere con i flag di validità associati ai testi
119200940509     C* - $FR = schiere con i flag di REVERSE IMAGE
119300940509     C* - $ULK = ultimo testo caricato
119400940722     C* - $L01 = Lunghezza prima riga
119500940722     C* - $L02 = Lunghezza seconda riga
119600940509     C* Output :
119700940509     C* - $ULK
119800940609     C* - $KEY1= 1° riga di tasti funzione
119900940609     C* - $KEY2= 2° riga di tasti funzione
120000940509     C*
120100950522     C                   CALLB     'XKEYMSG'
120200940509     C* input
120300940509     C                   PARM                    $FX
120400940509     C                   PARM                    $FC
120500940509     C                   PARM                    $FR
120600940509     C                   PARM                    $ULK              3 0
120700940509     C* output
120800940509     C                   PARM                    $KEY1            79
120900940509     C                   PARM                    $KEY2            79
121000950726     C* input
121100950726     C                   PARM                    $L01              3 0
121200950726     C                   PARM                    $L02              3 0
121300940509     C*
121400060224C1913C                   PARM                    $ALLFUNCT       480
121500060224C1913C*
121600940509     C                   ENDSR
121700031118     c* A1047 : Modificato tasto F14 per la videata D2 CON key0238
121800031118     c*         e abilitafo F14
121900940509** TASTI DI FUNZIONE  D1
122000940509PROMSG    *LIBL     KEY0001  01
122100940509PROMSG    *LIBL     KEY0002  02
122200940509PROMSG    *LIBL     KEY0003  03
122300940509PROMSG    *LIBL     KEY0004  04
122400940509PROMSG    *LIBL     KEY0005  05
122500940509PROMSG    *LIBL     KEY0006  06
122600940509PROMSG    *LIBL     KEY0007  07
122700940509PROMSG    *LIBL     KEY0008  08
122800940509PROMSG    *LIBL     KEY0009  09
122900940509PROMSG    *LIBL     KEY0010  10
123000940509PROMSG    *LIBL     KEY0011  11
123100940509PROMSG    *LIBL     KEY0012  12
123200940509PROMSG    *LIBL     KEY0013  13
123300940509PROMSG    *LIBL     KEY0014  14
123400940509PROMSG    *LIBL     KEY0015  15
123500940509PROMSG    *LIBL     KEY0016  16
123600940509PROMSG    *LIBL     KEY0017  17
123700940509PROMSG    *LIBL     KEY0018  18
123800940509PROMSG    *LIBL     KEY0019  19
123900940509PROMSG    *LIBL     KEY0020  20
124000940509PROMSG    *LIBL     KEY0021  21
124100940509PROMSG    *LIBL     KEY0022  22
124200940509PROMSG    *LIBL     KEY0023  23
124300940509PROMSG    *LIBL     KEY0024  24
124400940509**  ABILITAZIONE D1
1245009407200N  01
1246009510111N  02
1247009407201N  03
1248009510111N  04
1249009507310N  05
1250009510110N  06
1251009407200N  07
1252009510030N  08
1253009407200N  09
1254009411160N  10
1255009407200N  11
1256009511291N  12
1257009407200N  13
1258009407200N  14
1259009407200N  15
1260009510030N  16
1261009407200N  17
1262009407200N  18
1263009407200N  19
1264009407200N  20
1265000402190N  21
1266009407200N  22
1267009507260N  23   Impostare a '1' se si utilizzano opzioni
1268009507261N  24   Lasciare sempre impostata a '1' il valore della riga 24
126900951011** TASTI DI FUNZIONE  D2
127000951011PROMSG    *LIBL     KEY0001  01
127100951011PROMSG    *LIBL     KEY0002  02
127200951011PROMSG    *LIBL     KEY0003  03
127300951011PROMSG    *LIBL     KEY0004  04
127400951011PROMSG    *LIBL     KEY0005  05
127500951011PROMSG    *LIBL     KEY0006  06
127600951011PROMSG    *LIBL     KEY0007  07
127700951011PROMSG    *LIBL     KEY0008  08
127800951011PROMSG    *LIBL     KEY0009  09
127900951011PROMSG    *LIBL     KEY0010  10
128000951011PROMSG    *LIBL     KEY0011  11
128100951011PROMSG    *LIBL     KEY0012  12
128200951011PROMSG    *LIBL     KEY0013  13
128300031118PROMSG    *LIBL     KEY0238  14
128400951011PROMSG    *LIBL     KEY0015  15
128500951011PROMSG    *LIBL     KEY0016  16
128600951011PROMSG    *LIBL     KEY0017  17
128700951011PROMSG    *LIBL     KEY0018  18
128800951011PROMSG    *LIBL     KEY0019  19
128900951011PROMSG    *LIBL     KEY0020  20
129000951011PROMSG    *LIBL     KEY0021  21
129100951011PROMSG    *LIBL     KEY0022  22
129200951011PROMSG    *LIBL     KEY0023  23
129300951011PROMSG    *LIBL     KEY0024  24
129400951011**  ABILITAZIONE D2
1295009510110N  01
1296009510110N  02
1297009510111N  03
1298009510110N  04
1299009510110N  05
1300009510111N  06
1301009510110N  07
1302009510110N  08
1303009510110N  09
1304009510110N  10
1305009510110N  11
1306009510111N  12
1307009510110N  13
1308000311181N  14
1309009510110N  15
1310009510110N  16
1311009510110N  17
1312009510110N  18
1313009510110N  19
1314009510110N  20
1315009901041N  21
1316009510110N  22
1317009510110N  23   Impostare a '1' se si utilizzano opzioni
1318009510111N  24   Lasciare sempre impostata a '1' il valore della riga 24
