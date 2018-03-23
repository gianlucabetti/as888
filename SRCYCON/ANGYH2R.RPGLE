000100000113     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200970214     H*PARMS CVTOPT(*DATETIME)
000300940211     H DECEDIT('0,') DATEDIT(*DMY.)
000400940224      *----------*
000500940317    > * STD001R  *
000600940224      *----------*
000700940317    > *               TITOLO PROGRAMMA
000800940224      *          ------------------------------
000900940224      *
001000940224      *
001100940224      * DOCUMENTI MICROANALISI :
001200940224      *
001300940224      * NOTE TECNICHE :
001400940224      *
001500940224      *
001600940224      * PERSONALIZZAZIONI
001700940224      *
001800940224      *
001900940224      *
002000940314      *               MAPPA INDICATORI
002100940224      *
002200940307      *  21           GENERICO OPERAZIONI I/O
002300940224      *  22           GENERICO ERRORE OPERAZIONI I/O
002400940224      *  30           SFLDSP
002500940224      * N31           SFLCLR
002600940224      *  31           SFLDSPCTL
002700940224      *  32           SFLNXTCHG
002800940224      *  33           SFLEND
002900940224      *  39           OF PRTF
003000940224      *  40 <---> 49  DSPATR ERRORI SU SFL
003100940608      *  Specificare l'uso dei singoli indicatori
003200940224      *  50 <---> 98  ERRORI SU VIDEO
003300940608      *  Specificare l'uso dei singoli indicatori
003400940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
003500940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
003600940224      *  99           INDIC. GENERALE DI ERRORE
003700940128     F*----------------------------------------------------*
003800940721$022 F*NTBT01L  IF A E           K DISK
003900940715$001 FANGYH2D   CF   E             WORKSTN USROPN
004000940607     F                                     SFILE(S1:S1NRR)
004100940201     F                                     INFDS(DSFMT)
004200940714     FANABL01L  UF A E           K DISK
004300940128     D*----------------------------------------------------*
004400940302     D* Schiere per la gestione delle opzioni
004500940302     D*
004600950503    >D $MS1            S             27    DIM(14) CTDATA PERRCD(1)
004700950503    >D $TX1            S             50    DIM(14) ALT($MS1)
004800940728    >D $OP1            S              2  0 DIM(14) CTDATA PERRCD(1)
004900940728    >D $FL1            S              1    DIM(14) ALT($OP1)
005000950503     D $TX             S             50    DIM(50)
005100950503     D $FL             S              1    DIM(50)
005200940506     D* Schiere per la gestione dei tasti di funzione
005300940506     D $FM             S             27    DIM(24)
005400940506     D $FX             S             50    DIM(24)
005500940506     D $FC             S              1    DIM(24)
005600940506     D $FR             S              1    DIM(24)
005700940506     D* Schiere per la gestione dei tasti di funzione
005800940506     D* Video S1
005900940506     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
006000940506     D $FX2            S             50    DIM(24) ALT($FM2)
006100940506     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
006200940506     D $FR2            S              1    DIM(24) ALT($FC2)
006300940616     D* campi in lingua
006400940616$010 D*LIN             S              6    DIM(0011) CTDATA PERRCD(001)
006500940616$010 D*LIND            S             50    DIM(0011) ALT(LIN)
006600940616$010 D*LPOS            S              3  0 DIM(0011) CTDATA PERRCD(001)
006700940616$010 D*LLUN            S              3  0 DIM(0011) ALT(LPOS)
006800940211     D* Passaggio Parametri
006900940211     D KPJBA         E DS
007000940325     D*-------------
007100940620     D* DS Esterna autorizzazioni tabelle
007200940620     D XCHATB        E DS                  EXTNAME(XCHKATBDS)
007300940620     D*-------------
007400940620     D* DS  restituzione autorizzazioni
007500940620     D* IND=indice
007600940620     D* GRU=man. di gruppo
007700940620     D* AZI=man. azienda
007800941111     D AUTDS           DS
007900940620     D  MANIND                 1      1
008000940620     D  MANGRU                 2      2
008100940620     D  MANAZI                 3      3
008200940607     D* Richiamo a XATB
008300940607     D XATBDS        E DS
008400941103     D*-------------
008500941103      * DS Società
008600941103     D SOC001        E DS                  EXTNAME(XSOC001DS)
008700960123    >D  SALDOC       E                     EXTFLD(XSCSALDOC)
008800960123    >D  SALDOA       E                     EXTFLD(XSCSALDOA)
008900941103     D*-------------
009000941103      * DS Interna per dati utente
009100941103     D XSOCDS          DS          1000
009200940607     D*
009300940325     D* Parametri in ricezione
009400970708     D XTABDS        E DS
009500940325     D*-------------
009600940325     D* Parametri del PGM di manutenzione STD002R
009700940607    >D D002DS          DS
009800940607     D  OPZ002                 1      2
009900940607     D* 0 = ritorno normale
010000940607     D* 1 = F3
010100940607     D* 2 = F12
010200940607     D  RET002                 3      3
010300940607     D* operazione eseguite dal pgm chiamato :
010400940607     D* 0 = nessuna operazione
010500940607     D* 1 = eseguito aggiornamento
010600940607     D  OPR002                 4      4
010700940607     D* funzione non eseguibile per errore :
010800940607     D* 0 = nessun errore
010900940607     D* 1 = funzione richiamata chiusa in errore
011000940607     D* Aggiungere eventuali altri codici di errore
011100940607     D  ERR002                 5      5
011200940607     D* Azienda
011300940607     D  AZI002                 6      8
011400940607     D* Lingua
011500940607     D  LIN002                 9      9
011600940607     D* Codice scelto
011700940607     D  KEY002                10     24
011800970509     D* Autorizzazione
011900970509     D  AUT002                25     25
012000970509     D* Unita
012100970509     D  UNI002                26     33
012200940211     D*-------------
012300940211     D DSFMT           DS           512
012400940506     D  $TASTO               369    369
012500940211     D  NRG                  370    370
012600940211     D  NCL                  371    371
012700940211     D  SFLNRR               378    379B 0
012800940207     D*-------------
012900940207     D* Nome PGM a video
013000940207     D                 DS
013100940207     D  PROGR                  1     10
013200940207     D  ASTER1                 1      1    INZ('*')
013300940207     D  SIGLA                  2      9
013400940207     D  ASTER2                10     10    INZ('*')
013500940729     D*
013600940729     D*
013700940127     D*-------------
013800940127     D* Reperimento nome PGM
013900940127     D STATUS         SDS           333
014000940127     D  DSPGM            *PROC
014100940607     D* descrizione tabella
014200940607$003 D EXTDS         E DS                  EXTNAME(ANGYH2DS)
014300010406X0113D*-------------
014400010406  "  D* Reperimento titolo tabella
014500010406  "  D  WRKTIT         S             30A
014600010406  "  D  WRKTIT2        S            100A
014700010406  "  D  WRKTBCOD       S                   LIKE(XTBCOD)
014800010406  "  D  LENTIT         S              3  0
014900010406X0113D  I              S              3  0
015000940207     D*-------------
015100940211     D* COSTANTI
015200940211     D*-------------
015300940607     D SFLPAG          C                   CONST(10)
015400940728     D $DIMOP          C                   CONST(14)
015500940314     D* dimensione della schiera $MS1
015600940506     D*
015700940506     D* Tasti di funzione
015800940506     D F01             C                   CONST(X'31')
015900940506     D F02             C                   CONST(X'32')
016000940506     D F03             C                   CONST(X'33')
016100940506     D F04             C                   CONST(X'34')
016200940506     D F05             C                   CONST(X'35')
016300940506     D F06             C                   CONST(X'36')
016400940506     D F07             C                   CONST(X'37')
016500940506     D F08             C                   CONST(X'38')
016600940506     D F09             C                   CONST(X'39')
016700940506     D F10             C                   CONST(X'3A')
016800940506     D F11             C                   CONST(X'3B')
016900940506     D F12             C                   CONST(X'3C')
017000940506     D F13             C                   CONST(X'B1')
017100940506     D F14             C                   CONST(X'B2')
017200940506     D F15             C                   CONST(X'B3')
017300940506     D F16             C                   CONST(X'B4')
017400940506     D F17             C                   CONST(X'B5')
017500940506     D F18             C                   CONST(X'B6')
017600940506     D F19             C                   CONST(X'B7')
017700940506     D F20             C                   CONST(X'B8')
017800940506     D F21             C                   CONST(X'B9')
017900940506     D F22             C                   CONST(X'BA')
018000940506     D F23             C                   CONST(X'BB')
018100940506     D F24             C                   CONST(X'BC')
018200940506     D ENTER           C                   CONST(X'F1')
018300940506     D ROLDWN          C                   CONST(X'F4')
018400940506     D ROLLUP          C                   CONST(X'F5')
018500940302     I*
018600940211     I/EJECT
018700940207     I*-------------
018800940607     IS1
018900940607$015 I              S1YH2DES                    YH2DESYH2
019000940207     I*-------------
019100940127     I*
019200940127     C*----------------------------------------------------*
019300940127     C*                MAIN LINE PROGRAM
019400940127     C*----------------------------------------------------*
019500940127     C*
019600940223     C* inizializzazione variabili
019700940223     C                   EXSR      INZVAR
019800940223     C*
019900940223     C     $FINE         DOWEQ     *OFF
020000940131     C     $GEST         CASEQ     'S1'          GESS1
020100940117     C                   END
020200940117     C                   END
020300940325     C*
020400940325     C* fine programma
020500940325     C                   EXSR      ENDPGM
020600940225     C*
020700940325     C/EJECT
020800940325     C************************************************************
020900940325     C* FINE PROGRAMMA
021000940325     C************************************************************
021100940325     C     ENDPGM        BEGSR
021200940325     C*
021300940325     C* passaggio dati al pgm chiamante
021400940715     C                   MOVE      *BLANKS       KPJBU
021500940607     C                   MOVEL     XTABDS        KPJBU
021600940325     C*
021700940325     C* Se pgm Guida non è stato richiamato posso chiudere in LR
021800970708     C     XtaOpz        IFEQ      *BLANK
021900940325     C                   SETON                                            LR
022000960906     C                   RETURN
022100940325     C* altrimenti chiudo in RT
022200940325     C                   ELSE
022300940325     C                   SETON                                            RT
022400960906     C                   RETURN
022500940325     C                   ENDIF
022600940325     C*
022700940325     C                   ENDSR
022800940131     C/EJECT
022900940224     C************************************************************
023000940131     C* RIEMPIMENTO OPZIONI
023100940224     C************************************************************
023200940131     C     RIEOPZ        BEGSR
023300940223     C*
023400940223     C* Input :
023500940223     C* - $TX = schiera con tutti i testi disponibili
023600940223     C* - $FL = schiere con i flag di validità associati ai testi
023700940225     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
023800940223     C*          opzione usato
023900940223     C* - WMXO = variabile contenente il numero massimo di testi
024000940223     C*          opzione usabili
024100940223     C* Output :
024200940223     C* - WOPZ1 = variabile contenente la prima riga opzioni
024300940223     C* - WOPZ2 = variabile contenente la seconda riga opzioni
024400950502     C* - FF23  = flag che indica se c'è bisogno del tasto F23
024500940223     C*
024600970214     C                   CALLB     'X04OPMSG'
024700940223     C* input
024800940223     C                   PARM                    $TX
024900940223     C                   PARM                    $FL
025000940225     C                   PARM                    WCOZ              3 0
025100940223     C                   PARM                    WMXO              3 0
025200940223     C* output
025300940223     C                   PARM                    WOPZ1            76
025400940223     C                   PARM                    WOPZ2            76
025500950502     C                   PARM                    FF23              1
025600940607     C* input
025700940607$018 C                   PARM      37            $L01              3 0
025800940607     C                   PARM      0             $L02              3 0
025900940223     C*
026000060224     C                   PARM                    $ALLOPT         960
026100060224     C*
026200940131     C                   ENDSR
026300940506     C/EJECT
026400940506     C************************************************************
026500940506     C* RIEMPIMENTO TASTI FUNZIONE
026600940506     C************************************************************
026700940506     C     RIEKEY        BEGSR
026800940506     C*
026900940506     C* Input :
027000940506     C* - $FX = schiera con tutti i testi disponibili
027100940506     C* - $FC = schiere con i flag di validità associati ai testi
027200940506     C* - $FR = schiere con i flag di REVERSE IMAGE
027300940506     C* - $ULK = ultimo testo caricato
027400940506     C* Output :
027500940506     C* - $ULK
027600940506     c* - $KEY1= 1° riga di tasti funzione
027700940506     c* - $KEY2= 2° riga di tasti funzione
027800940506     C*
027900970214     C                   CALLB     'XKEYMSG'
028000940506     C* input
028100940506     C                   PARM                    $FX
028200940506     C                   PARM                    $FC
028300940506     C                   PARM                    $FR
028400940506     C                   PARM                    $ULK              3 0
028500940506     C* output
028600940506     C                   PARM                    $KEY1            79
028700940506     C                   PARM                    $KEY2            79
028800940607     C* input
028900940607$018 C                   PARM      37            $L01              3 0
029000941004$018 C                   PARM      37            $L02              3 0
029100940506     C*
029200060224     C                   PARM                    $ALLFUNCT       480
029300060224     C*
029400940506     C                   ENDSR
029500940224     C/EJECT
029600940224     C************************************************************
029700940224     C* GESTIONE EMISSIONE TASTI FUNZIONE
029800940224     C************************************************************
029900940224     C     GESPIE        BEGSR
030000940224     C*
030100940224     C                   SELECT
030200940506     C* Lista
030300940506     C     $GEST         WHENEQ    'S1'
030400940506     C     $ULKS1        IFEQ      0
030500940506     C                   EXSR      F24S1
030600940506     C                   ELSE
030700940607     C                   WRITE     Z1
030800940506     C                   ENDIF
030900940506     C                   ENDSL
031000940506     C*
031100940224     C                   ENDSR
031200940506     C/EJECT
031300940506     C************************************************************
031400940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA S1
031500940506     C************************************************************
031600940506     C     CTRKS1        BEGSR
031700940506     C                   SETOFF                                       99
031800940506     C                   MOVEA     $FC2          $FC
031900940506     C                   EXSR      CTRKEY
032000940506     C     *IN99         IFEQ      *ON
032100940506     C                   SETON                                        97
032200940506     C                   ENDIF
032300940607     C*
032400940506     C                   ENDSR
032500940506     C/EJECT
032600940506     C************************************************************
032700940506     C* CONTROLLO ABILITAZIONE TASTI
032800940509     C* INPUT
032900940509     c* $TASTO =Tasto funzione premuto
033000940509     C* $FC = Tasti abilitati
033100940509     C* OUTPUT
033200940509     C* IN99 = ERrore
033300940506     C************************************************************
033400940506     C     CTRKEY        BEGSR
033500940506     C                   SELECT
033600940506     C     $TASTO        WHENEQ    F01
033700940720     C     $FC(1)        IFEQ      '0'
033800940506     C                   SETON                                        99
033900940506     C                   ENDIF
034000940506     C     $TASTO        WHENEQ    F02
034100940720     C     $FC(2)        IFEQ      '0'
034200940506     C                   SETON                                        99
034300940506     C                   ENDIF
034400940506     C     $TASTO        WHENEQ    F03
034500940720     C     $FC(3)        IFEQ      '0'
034600940506     C                   SETON                                        99
034700940506     C                   ENDIF
034800940506     C     $TASTO        WHENEQ    F04
034900940720     C     $FC(4)        IFEQ      '0'
035000940506     C                   SETON                                        99
035100940506     C                   ENDIF
035200940506     C     $TASTO        WHENEQ    F05
035300940720     C     $FC(5)        IFEQ      '0'
035400940506     C                   SETON                                        99
035500940506     C                   ENDIF
035600940506     C     $TASTO        WHENEQ    F06
035700940720     C     $FC(6)        IFEQ      '0'
035800940506     C                   SETON                                        99
035900940506     C                   ENDIF
036000940506     C     $TASTO        WHENEQ    F07
036100940720     C     $FC(7)        IFEQ      '0'
036200940506     C                   SETON                                        99
036300940506     C                   ENDIF
036400940506     C     $TASTO        WHENEQ    F08
036500940720     C     $FC(8)        IFEQ      '0'
036600940506     C                   SETON                                        99
036700940506     C                   ENDIF
036800940506     C     $TASTO        WHENEQ    F09
036900940720     C     $FC(09)       IFEQ      '0'
037000940506     C                   SETON                                        99
037100940506     C                   ENDIF
037200940506     C     $TASTO        WHENEQ    F10
037300940720     C     $FC(10)       IFEQ      '0'
037400940506     C                   SETON                                        99
037500940506     C                   ENDIF
037600940506     C     $TASTO        WHENEQ    F11
037700940720     C     $FC(11)       IFEQ      '0'
037800940506     C                   SETON                                        99
037900940506     C                   ENDIF
038000940506     C     $TASTO        WHENEQ    F12
038100940720     C     $FC(12)       IFEQ      '0'
038200940506     C                   SETON                                        99
038300940506     C                   ENDIF
038400940506     C     $TASTO        WHENEQ    F13
038500940720     C     $FC(13)       IFEQ      '0'
038600940506     C                   SETON                                        99
038700940506     C                   ENDIF
038800940506     C     $TASTO        WHENEQ    F14
038900940720     C     $FC(14)       IFEQ      '0'
039000940506     C                   SETON                                        99
039100940506     C                   ENDIF
039200940506     C     $TASTO        WHENEQ    F15
039300940720     C     $FC(15)       IFEQ      '0'
039400940506     C                   SETON                                        99
039500940506     C                   ENDIF
039600940506     C     $TASTO        WHENEQ    F16
039700940720     C     $FC(16)       IFEQ      '0'
039800940506     C                   SETON                                        99
039900940506     C                   ENDIF
040000940506     C     $TASTO        WHENEQ    F17
040100940720     C     $FC(17)       IFEQ      '0'
040200940506     C                   SETON                                        99
040300940506     C                   ENDIF
040400940506     C     $TASTO        WHENEQ    F18
040500940720     C     $FC(18)       IFEQ      '0'
040600940506     C                   SETON                                        99
040700940506     C                   ENDIF
040800940506     C     $TASTO        WHENEQ    F19
040900940720     C     $FC(19)       IFEQ      '0'
041000940506     C                   SETON                                        99
041100940506     C                   ENDIF
041200940506     C     $TASTO        WHENEQ    F20
041300940720     C     $FC(20)       IFEQ      '0'
041400940506     C                   SETON                                        99
041500940506     C                   ENDIF
041600940506     C     $TASTO        WHENEQ    F21
041700940720     C     $FC(21)       IFEQ      '0'
041800940506     C                   SETON                                        99
041900940506     C                   ENDIF
042000940506     C     $TASTO        WHENEQ    F22
042100940720     C     $FC(22)       IFEQ      '0'
042200940506     C                   SETON                                        99
042300940506     C                   ENDIF
042400940506     C     $TASTO        WHENEQ    F23
042500940720     C     $FC(23)       IFEQ      '0'
042600940506     C                   SETON                                        99
042700940506     C                   ENDIF
042800940506     C     $TASTO        WHENEQ    F24
042900940720     C     $FC(24)       IFEQ      '0'
043000940506     C                   SETON                                        99
043100940506     C                   ENDIF
043200940506     C                   ENDSL
043300940506     C                   ENDSR
043400940224     C/EJECT
043500940127     C************************************************************
043600940131     C* GESTIONE LISTA
043700940127     C************************************************************
043800940127     C     GESS1         BEGSR
043900940223     C* inizializzazione videata
044000940223     C     $INZS1        IFEQ      *ON
044100940127     C                   EXSR      INZS1
044200940223     C                   MOVE      *OFF          $INZS1
044300940127     C                   ENDIF
044400940223     C* emissione piede videata se proveniente da altra
044500940223     C* salvataggio valore formato di provenienza
044600940202     C     $LASTV        IFNE      'S1'
044700940607!!   C*                    WRITEFMTT1
044800940224     C                   EXSR      GESPIE
044900940202     C                   MOVE      $LASTV        $LASTG
045000940202     C                   MOVE      'S1'          $LASTV
045100940202     C                   END
045200940223     C*
045300940223     C     WMAX          IFEQ      0
045400940607     C                   WRITE     D1
045500940211     C                   ENDIF
045600940729     C                   MOVE      '1'           WIN727            1
045700940127     C*
045800940607     C                   EXFMT     C1
045900940204     C     C1NRR         IFNE      0
046000940204     C                   Z-ADD     C1NRR         WSFL
046100940204     C                   ENDIF
046200940127     C                   Z-ADD     SFLNRR        C1RCD
046300940506     C*
046400940506     C* controllo abilitazione tasti funzione
046500940506     C*
046600940506     C                   EXSR      CTRKS1
046700940506     C     *IN99         IFEQ      *OFF
046800940506     C*
046900940127     C*
0470009401271    C                   SELECT
047100940127     C* F3=Fine
047200940506     C     $TASTO        WHENEQ    F03
047300940309     C                   EXSR      F03S1
047400940131     C* F10=Immissione
047500940506     C     $TASTO        WHENEQ    F10
047600940309     C                   EXSR      F10S1
047700940131     C* F12=Ritorno
047800940506     C     $TASTO        WHENEQ    F12
047900940309     C                   EXSR      F12S1
048000940224     C* F23=Altre opzioni
048100940506     C     $TASTO        WHENEQ    F23
048200940309     C                   EXSR      F23S1
048300940224     C* F24=Altri tasti
048400940506     C     $TASTO        WHENEQ    F24
048500940309     C                   EXSR      F24S1
048600940127     C* ROLL-UP
048700940506     C     $TASTO        WHENEQ    ROLLUP
048800940127     C                   EXSR      ROLS1
048900940127     C*
0490009401271O   C                   OTHER
049100940127     C* CONTROLLO DATI
049200940131     C                   EXSR      CTRC1
049300940201     C     *IN99         IFEQ      *OFF
049400940131     C                   EXSR      CTRS1
049500940131     C                   END
0496009401271-   C                   ENDSL
049700940127     C*
049800940506     C                   ENDIF
049900940127     C                   ENDSR
050000940224     C/EJECT
050100940127     C************************************************************
050200940131     C* INIZIALIZZAZIONE LISTA
050300940127     C************************************************************
050400940127     C     INZS1         BEGSR
050500940302     C* pulizia SFL
050600940128     C                   SETOFF                                         3031
050700940607     C                   WRITE     C1
050800940128     C                   SETON                                          31
050900010406X0113C*
051000010406  "  C* reperimento titolo
051100010406  "  C                   move      xtbcod        WrkTbcod
051200010406  "  C                   eval      xtbkey      = *zero
051300010406  "  C                   move      xtbcod        xtbkey
051400010406  "  C                   eval      xtbazi      = xscsoc
051500010406  "  C                   eval      xtblin      = xsclin
051600010406  "  C                   eval      xtbcod      = 'AAA'
051700010406  "  C                   eval      xtbric      = '1'
051800010406  "  C                   eval      xtbalc      = '0'
051900010406  "  C                   CALLB     'XATB'
052000010406  "  C                   PARM                    XATBDS
052100010406  "  C                   if        xtberr <> *off
052200010406  "  C                   movel     *all'?'       WrkTit
052300010406  "  C                   else
052400010406  "  C                   movel     xtbuni        WrkTit
052500010406  "  C                   endif
052600010406  "  C*
052700010406  "  C* centratura
052800010406  "  C                   move      WrkTbcod      xtbcod
052900010406  "  C                   eval      WrkTit2=%trimr(Wrktit)+X'22'+xtbcod
053000010406  "  C                             +X'20'
053100010406  "  C     ' '           CHECKR    Wrktit2       I
053200010406  "  C                   call      'X71CENTRA'
053300010406  "  C                   parm                    Wrktit2
053400010406  "  C                   parm                    I
053500010406  "  C                   parm                    lentit
053600010406X0113C                   movel     Wrktit2       C1tit
053700940127     C*
053800940128     C* riempimento opzioni
053900940223     C                   MOVEA     $TX1          $TX
054000940223     C                   MOVEA     $FL1          $FL
054100940309     C                   Z-ADD     0             WCOZ
054200940225     C*  equivale al num.di elementi della schiera $TX1
054300940509     C                   Z-ADD     $DIMOP        WMXO
054400940128     C                   EXSR      RIEOPZ
054500950502     C                   Z-ADD     23            X
054600950502     C                   MOVE      FF23          $FC2(X)
054700940608     C                   MOVEL     WOPZ1         C1OPZ1
054800060224     C                   MOVEL     $ALLOPT       H1ALLOPT
054900940608     C**                   MOVELWOPZ2     C1OPZ2
055000940225     C                   Z-ADD     WCOZ          C1COZ
055100940128     C*
055200940128     C* CARICAMENTO SFL A PAGINE
055300940128     C*
055400940201     C                   Z-ADD     0             S1NRR
055500940128     C                   Z-ADD     0             WMAX
055600940224     C*
055700940224     C* Posizionamento su file pilota
055800940302     C                   CLEAR                   C1POS
055900940316    >C                   EXSR      SETANA
056000940608    >C                   EXSR      REDANA
056100940302     C* ripristino i valori del posizionamento
056200940302     C                   MOVE      W1POS         C1POS
056300940131     C*
056400940131     C* Carico 1a pagina
056500940127     C                   EXSR      ROLS1
056600940201     C                   Z-ADD     SFLPAG        WPAG
056700940127     C*
056800940127     C                   ENDSR
056900940127     C/EJECT
057000940127     C************************************************************
057100940131     C* CARICAMENTO PAGINA LISTA
057200940127     C************************************************************
057300940127     C     ROLS1         BEGSR
057400940127     C*
057500940128     C                   SETOFF                                       32
057600940223     C                   Z-ADD     0             Y
057700940127     C                   Z-ADD     WMAX          S1NRR
057800940127     C*
057900940127     C* Leggo dal file anagrafico per caricare la lista
058000940127     C*
0581009401311    C     $EFILE        DOWEQ     *OFF
058200940201     C     Y             ANDLT     WPAG
058300940127     C*
058400940131     C                   EXSR      RIES1
058500940127     C*
058600940127     C                   ADD       1             S1NRR
058700940127     C                   ADD       1             Y
058800940714     C                   MOVE      $IN41         *IN41
058900940127     C*
059000940607     C                   WRITE     S1
059100940131     C*
059200940316    >C                   EXSR      REDANA
059300940128     C*
0594009401271-   C                   ENDDO
059500940127     C*
059600940223     C                   Z-ADD     S1NRR         WMAX                 30
059700940127     C*
059800940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
059900940127     C*
060000940201     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
060100940127     C                   MVR                     RESTO             3 0
060200940131     C     PAGINE        MULT      SFLPAG        C1RCD
0603009401271    C     RESTO         IFGT      0
060400940127     C                   ADD       1             C1RCD
0605009401271E   C                   ELSE
060600940407     C                   SUB       SFLPAG        C1RCD
060700940407     C                   ADD       1             C1RCD
0608009401271-   C                   ENDIF
060900940128     C*
061000940127     C                   ENDSR
061100940224     C/EJECT
061200940224     C************************************************************
061300940224     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
061400940224     C************************************************************
061500940607     C     SETANA        BEGSR
061600940224     C*
061700941004     C                   MOVE      *BLANKS       XTBKEY
061800970708     C                   MOVE      XtaAzi        XTBAZI
061900970708     C                   MOVE      XtaLin        XTBLIN
062000940608     C                   MOVE      '9'           XTBRIC
062100940607     C                   MOVE      '0'           XTBALC
062200950622     C                   MOVE      XSCCUT        XTBUTE
062300940607     C                   MOVE      '0'           XTBERR
062400941121     C**         W1POS     IFNE *BLANK
062500941004     C                   MOVE      *ZEROS        XTBKEY
062600940608     C                   MOVE      W1POS         XTBKEY
062700941121     C**                   END
062800970214     C                   CALLB     'XATB'
062900940607     C                   PARM                    XATBDS
063000940607     C*
063100940224     C*
063200940224     C                   ENDSR
063300940128     C/EJECT
063400940128     C************************************************************
063500940131     C* LETTURA RCD ARCHIVIO PILOTA
063600940128     C************************************************************
063700940607     C     REDANA        BEGSR
063800940128     C*
063900940131     C                   MOVEL     *OFF          $RCDOK
064000940131     C*
0641009401311    C     $EFILE        DOUEQ     *ON
064200940131     C     $RCDOK        OREQ      *ON
064300940131     C*
064400940607     C                   MOVE      '3'           XTBRIC
064500940607     C                   MOVE      '0'           XTBERR
064600970214     C                   CALLB     'XATB'
064700940607     C                   PARM                    XATBDS
064800940607     C*
064900940607     C     XTBERR        IFNE      '0'
065000940607     C                   MOVEL     *ON           $EFILE
065100010214     C                   ELSE
065200010214     C                   MOVEL     *OFF          $EFILE
065300940607     C                   ENDIF
065400940131     C*
0655009401312    C     $EFILE        IFEQ      *OFF
065600940607     C                   EXSR      SELANA
065700950502     C                   MOVE      XTBKEY        SAVKEY
065800950502     C                   MOVE      XTBGRU        SAVGRU
065900950502     C                   MOVE      XTBUNI        SAVUNI
066000970509     C                   MOVE      XTBUNT        SAVUNT
0661009401312-   C                   ENDIF
066200940131     C*
0663009401311-   C                   ENDDO
066400940131     C*
066500940607     C                   MOVE      $EFILE        *IN33
066600940131     C*
066700940131     C                   ENDSR
066800940224     C/EJECT
066900940224     C************************************************************
067000940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
067100940224     C************************************************************
067200940224     C     CLCPAG        BEGSR
067300940224     C* Input :
067400940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
067500940224     C*          cursore
067600940224     C* - SFLPAG = numero rcd per pagina sfl
067700940224     C* Output :
067800940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
067900940224     C*
068000940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
068100940224     C                   MVR                     RESTO             3 0
068200940224     C     RESTO         IFGT      0
068300940224     C                   ADD       1             PAGINE
068400940224     C                   ENDIF
068500940224     C     PAGINE        MULT      SFLPAG        WPAG
068600940224     C*
068700940224     C                   ENDSR
068800940131     C/EJECT
068900940131     C************************************************************
069000940131     C* SELEZIONI SULLA RIGA LETTA
069100940314     C* impostare a *OFF il campo $RCDOK per scartare un record che
069200940314     C* non rientra nei parametri di parzializzazione
069300940314     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
069400940314     C* superiore stabilito nei parametri
069500940314     C*
069600940131     C************************************************************
069700940607     C     SELANA        BEGSR
069800940131     C*
069900940202     C                   MOVEL     *ON           $RCDOK
070000940714     C* Se aperto il pgm con opzione 11
070100940714     C* carico solo i record di gruppo
070200970708     C     XtaOpz        IFEQ      '11'
070300940714     C     XTBGRU        ANDNE     'G'
070400940714     C                   MOVEL     *OFF          $RCDOK
070500940714     C                   END
070600940714     C* Carica in alta intensità solo i record
070700940714     C* abilitati
070800940714     C                   MOVE      '0'           $IN41
070900970708     C     XtaOpz        IFEQ      '11'
071000940714     C     XTBGRU        ANDEQ     'G'
071100940715     C     XTBAZB        ANDNE     'N'
071200940714     C                   MOVE      '1'           $IN41
071300940714     C                   END
071400940714     C*
071500940202     C                   ENDSR
071600940131     C/EJECT
071700940131     C************************************************************
071800940131     C* RIEMPIMENTO RIGA LISTA
071900940131     C************************************************************
072000940131     C     RIES1         BEGSR
072100950502     C*                    MOVE XTBKEY    S1COD
072200950502     C*                    MOVE XTBGRU    S1GRU
072300950502     C                   MOVEL     SAVUNI        EXTDS
072400950502     C                   MOVE      SAVKEY        S1COD
072500950502     C                   MOVE      SAVGRU        S1GRU
072600970512     C                   MOVE      SAVUNT        S1UNITA           8
072700981209$027 C
072800950502     C     *LIKE         DEFINE    XTBUNI        SAVUNI
072900950502     C     *LIKE         DEFINE    XTBKEY        SAVKEY
073000950502     C     *LIKE         DEFINE    XTBGRU        SAVGRU
073100970509     C     *LIKE         DEFINE    XTBUNT        SAVUNT
073200940131     C*
073300940131     C                   ENDSR
073400940309     C/EJECT
073500940309     C************************************************************
073600940309     C* GESTIONE F03 VIDEO S1
073700940309     C************************************************************
073800940309     C     F03S1         BEGSR
073900940309     C*
074000940309     C                   MOVE      *ON           $FINE
074100970708     C                   MOVE      '1'           XtaRet
074200940325     C* fine programma
074300940325     C                   EXSR      ENDPGM
074400940309     C*
074500940309     C                   ENDSR
074600940309     C/EJECT
074700940309     C************************************************************
074800940309     C* GESTIONE F10 VIDEO S1
074900940314     c* AGGIUNTA RECORD
075000940309     C************************************************************
075100940309     C     F10S1         BEGSR
075200940309     C*
075300940607     C                   RESET                   D002DS
075400940607     C                   MOVEL     '01'          OPZ002
075500970708     C                   MOVE      XtaLin        LIN002
075600970708     C                   MOVE      XtaAzi        AZI002
075700970512     C                   MOVE      S1UNITA       UNI002
075800940715     C                   MOVE      *BLANKS       KPJBU
075900940607     C                   MOVEL     D002DS        KPJBU
076000940608$004 C                   CALL      'ANGYH2R2'
076100940309     C                   PARM                    KPJBA
076200940607     C                   MOVEL     KPJBU         D002DS
076300940309     C* ritorno da PGM gestione
076400940309     C                   EXSR      GESRET
076500940309     C     WINZS1        IFEQ      *ON
076600940309     C                   MOVE      *ON           $INZS1
076700940309     C* se esistevano già righe sul sfl
076800940309     C* calcolo pagina a cui deve posizionarsi
076900940309     C     WSFL          IFGT      0
077000940309     C                   EXSR      CLCPAG
077100940309     C* altrimenti carico solo la 1a pagina
077200940309     C                   ELSE
077300940309     C                   Z-ADD     SFLPAG        WPAG
077400940309     C                   ENDIF
077500940309     C                   ENDIF
077600940309     C*
077700940712     C                   MOVE      *HIVAL        $LASTV
077800940309     C                   ENDSR
077900940309     C/EJECT
078000940309     C************************************************************
078100940309     C* GESTIONE F12 VIDEO S1
078200940309     C************************************************************
078300940309     C     F12S1         BEGSR
078400940309     C*
078500970708     C                   MOVE      '2'           XtaRet
078600940309     C                   MOVE      *ON           $FINE
078700940309     C*
078800940309     C                   ENDSR
078900940309     C/EJECT
079000940309     C************************************************************
079100940309     C* GESTIONE F23 VIDEO S1
079200940309     C************************************************************
079300940309     C     F23S1         BEGSR
079400940309     C*
079500940309     C                   EXSR      RIEOPZ
079600950502     C                   Z-ADD     23            X
079700950502     C                   MOVE      FF23          $FC2(X)
079800940608     C                   MOVEL     WOPZ1         C1OPZ1
079900060224     C                   MOVEL     $ALLOPT       H1ALLOPT
080000940608     C**                   MOVELWOPZ2     C1OPZ2
080100940309     C                   Z-ADD     WCOZ          C1COZ
080200940309     C*
080300940309     C                   ENDSR
080400940309     C/EJECT
080500940309     C************************************************************
080600940309     C* GESTIONE F24 VIDEO S1
080700940309     C************************************************************
080800940309     C     F24S1         BEGSR
080900940309     C*
081000940506     C*
081100940506     C                   MOVEA     $FX2          $FX
081200940506     C                   MOVEA     $FC2          $FC
081300940506     C                   MOVEA     $FR2          $FR
081400940506     C                   Z-ADD     $ULKS1        $ULK
081500940506     C                   EXSR      RIEKEY
081600941017     C                   MOVEA     $FC           $FC2
081700940506     C                   Z-ADD     $ULK          $ULKS1            3 0
081800940607     C                   MOVEL     $KEY1         Z1KE1
081900060224     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
082000941004     C                   MOVEL     $KEY2         Z1KE2
082100940607     C                   WRITE     Z1
082200940506     C*
082300940309     C                   ENDSR
082400940128     C/EJECT
082500940128     C************************************************************
082600940131     C* CONTROLLO TESTATA LISTA
082700940128     C************************************************************
082800940131     C     CTRC1         BEGSR
082900940128     C*
083000940201     C                   MOVE      *OFF          *IN99
083100940131     C*
083200940202     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
083300940202     C* L'INIZIALIZZAZIONE LISTA
083400940202     C*
083500940207     C     C1POS         IFNE      W1POS
083600940202     C                   EXSR      POSS1
083700940202     C                   END
083800940202     C*
083900940202     C                   ENDSR
084000940202     C/EJECT
084100940202     C************************************************************
084200940202     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
084300940202     C************************************************************
084400940202     C     POSS1         BEGSR
084500940202     C*
084600940202     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
084700940202     C*
0848009402021    C     WMAX          IFGT      0
084900940607     C                   READC     S1                                     21
0850009402022    C     *IN21         DOWEQ     *OFF
085100940202     C     *IN99         ANDEQ     *OFF
085200940202     C*
0853009402023    C     S1OPZ         IFNE      0
085400940202     C                   SETON                                        51  99
0855009402023-   C                   ENDIF
085600940202     C* se rilevata opzione attiva, attivo sflnxtchg
0857009402023    C     *IN99         IFEQ      *ON
085800940202     C                   MOVE      *ON           *IN32
085900940714     C                   MOVE      $IN41         *IN41
086000940607     C                   UPDATE    S1
0861009402023E   C                   ELSE
086200940607     C                   READC     S1                                     21
0863009402023-   C                   ENDIF
086400940202     C*
0865009402022-   C                   ENDDO
086600940202     C*
0867009402021-   C                   ENDIF
086800940202     C*
086900940221     C*
0870009402021    C     *IN99         IFEQ      *OFF
087100941121     C                   CLEAR                   W1POS
087200940202     C*
087300940302     C                   MOVEL     C1POS         W1POS
087400940131     C*
087500940202     C                   MOVE      *ON           *IN99
087600940223     C                   MOVE      *ON           $INZS1
087700940202     C*
087800940202     C*
0879009402021-   C                   ENDIF
088000940202     C*
088100940131     C                   ENDSR
088200940131     C/EJECT
088300940131     C************************************************************
088400940131     C* CONTROLLO OPZIONI LISTA
088500940131     C************************************************************
088600940131     C     CTRS1         BEGSR
088700940131     C*
088800940202     C                   MOVEL     *OFF          $ESCI
088900940201     C                   SETOFF                                       99
089000940131     C                   Z-ADD     0             S1OPZ
089100940131     C*
089200940127     C* Leggo il sfl solo se ci sono rcd
0893009401311    C     WMAX          IFGT      0
089400940607     C                   READC     S1                                     21
089500940127     C*
089600940131     C* esce se fine sfl o errore che richiede l'uscita
0897009401312    C     *IN21         DOWEQ     *OFF
089800940131     C     $ESCI         ANDEQ     *OFF
089900940201     C                   Z-ADD     S1NRR         C1RCD
090000940131     C* ctrl su riga
090100940131     C                   EXSR      RECS1
090200940131     C* gestione opzioni
0903009401313    C     S1OPZ         IFNE      0
090400940201     C     *IN99         ANDEQ     *OFF
090500940131     C                   EXSR      OPZS1
0906009401273-   C                   ENDIF
090700940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0908009402013    C     *IN99         IFEQ      *ON
090900940201     C     $ESCI         OREQ      *ON
091000940131     C                   MOVE      *ON           *IN32
091100940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
091200940204     C* la ripristinerò a conclusione del ciclo di READC
091300940223     C                   MOVE      *OFF          $INZS1
0914009402233-   C                   ENDIF
091500940223     C*
091600940131     C                   Z-ADD     0             S1OPZ
091700940223     C*
091800940714     C                   MOVE      $IN41         *IN41
091900940607     C                   UPDATE    S1
092000940223     C*
092100940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0922009401313    C     $ESCI         IFEQ      *OFF
092300940607     C                   READC     S1                                     21
092400940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0925009402014    C     *IN21         IFEQ      *ON
092600940201     C                   MOVE      WINZS1        $INZS1
092700940204     C* calcolo pagina a cui deve posizionarsi
092800940224     C                   EXSR      CLCPAG
0929009402014-   C                   ENDIF
0930009402013-   C                   ENDIF
093100940131     C*
0932009401272-   C                   ENDDO
093300940127     C*
0934009401311-   C                   ENDIF
093500940131     C*
093600940127     C                   ENDSR
093700940127     C/EJECT
093800940127     C************************************************************
093900940131     C* CONTROLLO CAMPI I/O RIGA LISTA
094000940127     C************************************************************
094100940131     C     RECS1         BEGSR
094200940131     C*
094300940201     C* reset indicatori DSPATR
094400940201     C                   MOVE      *ALL'0'       IN4049           10
094500940201     C                   MOVEA     IN4049        *IN(40)
094600940201     C*
094700940131     C* Controllo se Utente Autorizzato alla scelta
094800940202     C     S1OPZ         IFNE      0
094900940131     C                   Z-ADD     1             X
095000940223     C     S1OPZ         LOOKUP    $OP1(X)                                21
095100940211     C     *IN21         IFEQ      *OFF
095200940223     C     $FL1(X)       OREQ      'N'
095300940131     C                   MOVE      *ON           $ESCI
095400940201     C                   SETON                                        504099
095500940131     C                   ENDIF
095600940131     C*
095700940620     c* Controllo autorizzazione alla modifica su record di gruppo
095800940620     C* Reperisco tipo codice (gruppo/azienda)
095900940620     C                   MOVE      *ALL'0'       XTBKEY
096000940620     C                   MOVE      S1COD         XTBKEY
096100970512     C                   MOVE      S1UNITA       XTBUNT
096200940620     C                   MOVE      '0'           XTBALC
096300940620     C                   MOVE      '1'           XTBRIC
096400970214     C                   CALLB     'XATB'
096500940620     C                   PARM                    XATBDS
096600940620     C* Reperisco autorizzazioni utente
096700940620     C                   CLEAR                   XCHATB
096800941118     C                   MOVE      XSCSOC        XCTSOC
096900941118     C                   MOVE      XSCDUN        XCTUNI
097000940620     C                   MOVEL     KNMUS         XCTPRF
097100940620     C                   MOVE      XTBCOD        XCTCOD
097200940620     C                   CALL      'XCHKATB'
097300940620     C                   PARM                    XCHATB
097400941111     C                   MOVEL     XCTDDS        AUTDS
097500941027     C     XCTRTN        IFEQ      1
097600941027     C                   MOVEL     'N'           MANGRU
097700941027     C                   ENDIF
097800940620     C* Non ammessa manutenzione di gruppo
097900940620   1 C     XTBGRU        IFEQ      'G'
098000940620     C     MANGRU        ANDEQ     'N'
098100940620   2 C     S1OPZ         IFEQ      2
098200940729     C     S1OPZ         OREQ      3
098300940620     C     S1OPZ         OREQ      4
098400940620     C                   SETON                                        5399
098500940620   2 C                   ENDIF
098600940620   1 C                   ENDIF
098700940620     C* Non ammessa manutenzione di azienda
098800940620   1 C     XTBGRU        IFEQ      'A'
098900940620     C     MANAZI        ANDEQ     'N'
099000940620   2 C     S1OPZ         IFEQ      2
099100940729     C     S1OPZ         OREQ      3
099200940620     C     S1OPZ         OREQ      4
099300940620     C                   SETON                                        5499
099400940620   2 C                   ENDIF
099500940620   1 C                   ENDIF
099600940607     C                   ENDIF
099700940131     C                   ENDSR
099800940224     C/EJECT
099900940131     C************************************************************
100000940131     C* GESTIONE OPZIONI LISTA
100100940131     C************************************************************
100200940131     C     OPZS1         BEGSR
100300940201     C*
1004009402251    C                   SELECT
100500940225     C*
100600940225     C* se opz=Selezione passo la chiave al pgm chiamante
100700940225     C     S1OPZ         WHENEQ    1
100800970708     C                   MOVE      S1OPZ         XtaOpz
100900970708     C                   MOVE      S1COD         XtaKey
101000970708     C                   MOVE      S1UNIta       xtaUni
101100940225     C                   MOVE      *ON           $ESCI
101200940225     C                   MOVE      *ON           $FINE
101300940729     C*
101400940714     C*
101500940714     C* se opz=Abilita scrive su archivio abilitazioni
101600940714     C     S1OPZ         WHENEQ    11
101700941103     C                   MOVE      XSCSOC        ABLAZI
101800940714     C                   MOVE      *ZEROS        ABLKEY
101900940714     C                   MOVE      S1COD         ABLKEY
102000970512     C                   MOVE      S1UNITA       ABLUNT
102100940714     C                   MOVE      XTBCOD        ABLCOD
102200940714     C                   WRITE     ANABL000                             22
102300940714     C*
102400940714     C* se opz=Disabil.cancella su archivio abilitazioni
102500940714     C     S1OPZ         WHENEQ    12
102600941103     C                   MOVE      XSCSOC        ABLAZI
102700940714     C                   MOVE      *ZEROS        ABLKEY
102800940714     C                   MOVE      S1COD         ABLKEY
102900970512     C                   MOVE      S1UNITA       ABLUNT
103000940714     C                   MOVE      XTBCOD        ABLCOD
103100940714     C     KABL          DELETE    ANABL000                           22
103200940729     C*
103300940225     C*
103400940225     C* se altre opzioni (o nessuna opzione) richiamo pgm gestione
1035009402251O   C                   OTHER
103600940225     C*
103700940616     C                   RESET                   D002DS
103800940607     C                   MOVEL     S1OPZ         OPZ002
103900940616     C                   MOVE      *ZERO         KEY002
104000940607     C                   MOVE      S1COD         KEY002
104100970708     C                   MOVE      XtaLin        LIN002
104200970708     C                   MOVE      XtaAzi        AZI002
104300940715     C                   MOVE      *BLANKS       KPJBU
104400940607     C                   MOVEL     D002DS        KPJBU
104500940608$004 C                   CALL      'ANGYH2R2'
104600940607     C                   PARM                    KPJBA
104700940607     C                   MOVEL     KPJBU         D002DS
104800940201     C*
104900940223     C* ritorno da PGM gestione
105000940223     C                   EXSR      GESRET
105100940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
1052009402252    C     *IN99         IFEQ      *ON
105300940223     C                   SETON                                        40
1054009402252-   C                   ENDIF
105500940131     C*
105600940506     C                   MOVE      *HIVAL        $LASTV
1057009402251-   C                   ENDSL
105800940225     C*
105900940131     C                   ENDSR
106000940223     C/EJECT
106100940223     C************************************************************
106200940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
106300940223     C************************************************************
106400940223     C     GESRET        BEGSR
106500940223     C*
106600940223     C* modo di ritorno
106700940223     C*
1068009402231    C                   SELECT
106900940314    >C* << questi modi di utilizzo dei valori di ritorno dal
107000940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
107100940314    >C*    proposte, normalmente sempre valide, ma modificabili
107200940314    >C*    per situazioni particolari >>
107300940223     C* 1 = F3
107400940314    >C     RET002        WHENEQ    '1'
107500940224     C                   MOVE      *ON           $ESCI
107600940223     C                   MOVE      *ON           $FINE
107700940223     C* 2 = F12
107800940314    >C     RET002        WHENEQ    '2'
107900940223     C                   MOVE      *ON           $ESCI
108000940223     C*
1081009402231-   C                   ENDSL
108200940223     C*
108300940223     C* operazione eseguite dal pgm chiamato
108400940223     C*
1085009402231    C                   SELECT
108600940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
108700940314    >C     OPR002        WHENEQ    '1'
108800940223     C                   MOVE      *ON           WINZS1
108900940223     C*
1090009402231-   C                   ENDSL
109100940223     C*
109200940223     C* funzione non eseguibile per errore :
109300940223     C*
1094009402231    C                   SELECT
109500940223     C* 1 = funzione richiamata chiusa in errore
109600940316    >C*  eventualmente gestire altri codici di errore
109700940314    >C     ERR002        WHENEQ    '1'
109800940223     C                   MOVE      *ON           $ESCI
109900940223     C                   SETON                                        5299
110000940223     C*
1101009402231-   C                   ENDSL
110200940223     C*
110300940223     C                   ENDSR
110400940223     C/EJECT
110500940131     C************************************************************
110600940131     C* OPERAZIONI INIZIALI
110700940131     C************************************************************
110800940131     C     *INZSR        BEGSR
110900940131     C*
111000940127     C* Reperimento parametri
111100940127     C*
111200940117     C     *ENTRY        PLIST
111300940117     C                   PARM                    KPJBA
111400940714     C*
111500940223     C* Reperimento opzioni
111600940127     C*
111700940128     C                   EXSR      REPOPZ
111800940506     C*
111900940506     C* Reperimento tasti di funzione
112000940506     C*
112100940506     C                   EXSR      REPKEY
112200940506     C*
112300940607$007 C                   MOVE      'YH2'         XTBCOD
112400010406X0113C*
112500010406  "  C* reperisco le dimensioni della variabile del titolo
112600010406X0113C                   EVAL      LENTIT = %Size(C1Tit)
112700940127     C*
112800940117     C                   ENDSR
112900940225     C/EJECT
113000941103      ************************************************************
113100941103      * Reperimento dati società
113200941103      ************************************************************
113300941103     C     REPSOC        BEGSR
113400941103      *
113500970214     C                   CALLB     'XSOC'
113600941103     C                   PARM                    TIPXSC            6
113700941103     C                   PARM                    SOCXSC            3
113800941103     C                   PARM                    CDSXSC            9 0
113900941103     C                   PARM                    MODXSC            3
114000941103     C                   PARM      *BLANKS       RTNXSC            1
114100941103     C                   PARM                    XSOCDS
114200941103     C                   PARM                    KPJBA
114300941103      *
114400941103     C                   ENDSR
114500940223     C************************************************************
114600940223     C* INIZIALIZZAZIONE VARIABILI
114700940223     C************************************************************
114800940223     C     INZVAR        BEGSR
114900970926     C*
115000970926     C* Dati da PGM chiamante
115100970926     C*
115200970926     C                   MOVEL     KPJBU         XTABDS
115300951124     C*
115400951124     C* Reperimento codice lingua e codice azienda
115500951124     C*
115600951124     C                   MOVEL     'SOC001'      TIPXSC
115700951124     C                   EXSR      REPSOC
115800951124     C     RTNXSC        IFNE      '1'
115900951124     C                   MOVEL     XSOCDS        SOC001
116000951124     C                   ELSE
116100951124     C                   MOVE      *ON           $FINE
116200951124     C* fine programma
116300951124     C                   EXSR      ENDPGM
116400951124     C                   ENDIF
116500951124     C* Lingua e codice azienda
116600970708     C                   MOVE      XSCLIN        XtaLin
116700970708     C                   MOVE      XSCSOC        XtaAzi
116800940715     C*
116900940715     C                   RESET                   $FC2
117000940715     C*
117100940715$021 C                   CLOSE     ANGYH2D
117200940715$021 C                   OPEN      ANGYH2D
117300940506     C*
117400940506     C* Definizione delle opzioni utilizzabili a pgm
117500940506     C*
117600940506     C                   EXSR      DEFOPZ
117700940718     C* Pulizia campi e indicatori
117800940718     C                   CLEAR                   W1POS
117900940718     C                   MOVE      *ALL'0'       IN4049           10
118000940718     C                   MOVEA     IN4049        *IN(40)
118100940718     C                   CLEAR                   S1OPZ
118200940223     C* Variabili per gestione videate
118300940223     C*
118400940223     C                   MOVE      *OFF          $FINE
118500940223     C                   MOVE      *OFF          $INZS1
118600940223     C                   MOVE      *OFF          $EFILE
118700940223     C                   MOVE      *OFF          $ESCI
118800940223     C                   MOVE      *OFF          $RCDOK
118900940509     C                   MOVE      *HIVAL        $LASTV
119000940506     C                   Z-ADD     0             $ULKS1            3 0
119100940223     C*
119200940318     C                   MOVE      *ON           $INZS1
119300940223     C                   MOVE      'S1'          $GEST
119400940223     C*
119500940223     C* Variabili appoggio
119600940223     C*
119700940223     C                   Z-ADD     SFLPAG        WPAG
119800940223     C*
119900940223     C* Valorizzazione campi univoci testate
120000940223     C*
120100940607     C*                    CLEART1
120200940607     C*                    MOVELKNSIF     NOMSIF
120300940607     C*                    MOVELRAGUTE    NOMDIT
120400940607     C*                    MOVELDSPGM     SIGLA
120500940607     C*                    MOVELPROGR     NOMPGM
120600940223     C*
120700940608     c* posizione window
120800010406X0113c* modificata la seguente riga
120900010406$012 C                   Z-ADD     39            PCOL              2 0
121000940613     C                   Z-ADD     09            R
121100940608     C     80            SUB       PCOL          C
121200940608     C                   SUB       3             C
121300970708     C     XtaOpz        IFNE      '  '
121400970708     C     XtaRig        IFGT      11
121500940608     C                   Z-ADD     1             R
121600940608     C                   ENDIF
121700940608     C*
121800970708     C     XtaCol        IFGT      40
121900940608     C                   Z-ADD     2             C
122000940608     C                   ENDIF
122100940608     C                   ENDIF
122200940616     C*
122300970708     C     XtaOpz        IFNE      '01'
122400940616$019 C                   DO        00            I                 3 0
122500940721$022 C*                  MOVEL     LIN(I)        TBTFLD
122600940721$022 C*                  CLEAR                   TBTKEY
122700940721$022 C*                  CLEAR                   TBTAZI
122800940721$022 C*                  CLEAR                   TBTLIN
122900940721$007 C                   MOVE      'YH2'         TBTCOD            3
123000940721$022 C*    K5TBT1        CHAIN     ANTBT01L                           21
123100940721$022 C*    *IN21         IFEQ      *ON
123200940721$022 C*                  Z-ADD     LLUN(I)       TBTLUN
123300940721$022 C*                  Z-ADD     LPOS(I)       TBTPOS
123400940721$022 C*                  MOVEL     LIND(I)       TBTDES
123500940721$022 C*                  WRITE     ANTBT000
123600940721$022 C*                  ENDIF
123700940616     C                   ENDDO
123800940616     C                   ENDIF
123900940608     C*
124000940722     c*
124100940722     c* Se richiamato con OPZ 11  Non controlla abilitazione all'aziend
124200970708     C     XtaOpz        IFEQ      '11'
124300940722     C                   MOVE      'N'           XTBGAB
124400940722     C                   ELSE
124500940722     C                   MOVE      'S'           XTBGAB
124600940722     C                   ENDIF
124700940223     C                   ENDSR
124800940223     C/EJECT
124900940131     C************************************************************
125000940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
125100940131     C************************************************************
125200940225     C     DEFOPZ        BEGSR
125300940131     C*
125400940225     C* Questa routine deve valorizzare la schiera $FL1
125500940225     C* con S o N a seconda della utilizzabilità o meno
125600940225     C* dell'opzione correlata
125700940225     C*
125800940714     C* Inizialmente si considerano tutte le opzioni come non valide
125900940728     C     1             DO        14            X
126000940714     C                   MOVE      'N'           $FL1(X)
126100940225     C                   END
126200940225     C*
126300940225     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
126400940714     C* rendo valide alcune opzioni
126500940714     C* i tasti invece li disattivo
126600940225     C                   SELECT
126700940714     C*
126800940714     C* Se pgm richiamato in RICERCA attivo :
126900940714     C*
127000970708    >C     XtaOpz        WHENEQ    '01'
127100940714     C* F3=Fine
127200940714     C                   Z-ADD     3             X
127300940720     C                   MOVE      '0'           $FC2(X)
127400940714     C* F10=Immissione
127500940714     C                   Z-ADD     10            X
127600940919     C                   MOVE      '1'           $FC2(X)
127700940714     C* 1=Selezione
127800940714     C                   Z-ADD     1             X
127900940714     C     01            LOOKUP    $OP1(X)                                21
128000940714     C     *IN21         IFEQ      *ON
128100940714     C                   MOVE      'S'           $FL1(X)
128200940714     C                   ENDIF
128300940729     C* 2=Modifica
128400940729     C                   Z-ADD     1             X
128500940729     C     02            LOOKUP    $OP1(X)                                21
128600940729     C     *IN21         IFEQ      *ON
128700940729     C                   MOVE      'S'           $FL1(X)
128800940729     C                   ENDIF
128900940729     C* 3=Copia
129000940729     C                   Z-ADD     1             X
129100940729     C     03            LOOKUP    $OP1(X)                                21
129200940729     C     *IN21         IFEQ      *ON
129300940729     C                   MOVE      'S'           $FL1(X)
129400940729     C                   ENDIF
129500940729     C* 4=Annullamento
129600940729     C                   Z-ADD     1             X
129700940729     C     04            LOOKUP    $OP1(X)                                21
129800940729     C     *IN21         IFEQ      *ON
129900940729     C                   MOVE      'S'           $FL1(X)
130000940729     C                   ENDIF
130100940714     C* 5=Visualizzazione
130200940714     C                   Z-ADD     1             X
130300940714     C     05            LOOKUP    $OP1(X)                                21
130400940714     C     *IN21         IFEQ      *ON
130500940714     C                   MOVE      'S'           $FL1(X)
130600940714     C                   ENDIF
130700940728     C*14=Commenti
130800940831     C**                   Z-ADD1         X
130900940831     C**         14        LOKUP$OP1,X                   21
131000940831     C**         *IN21     IFEQ *ON
131100940831     C**                   MOVE 'S'       $FL1,X
131200940831     C**                   ENDIF
131300940714     C*
131400940714     C* Se pgm richiamato in INTERROGAZIONE attivo :
131500940714     C*
131600970708    >C     XtaOpz        WHENEQ    '05'
131700940714     C* F10=Immissione
131800940714     C                   Z-ADD     10            X
131900940720     C                   MOVE      '0'           $FC2(X)
132000940714     C* F3=Fine
132100940714     C                   Z-ADD     3             X
132200940720     C                   MOVE      '0'           $FC2(X)
132300940714     C* 5=Visualizzazione
132400940225     C                   Z-ADD     1             X
132500940714     C     05            LOOKUP    $OP1(X)                                21
132600940225     C     *IN21         IFEQ      *ON
132700940714     C                   MOVE      'S'           $FL1(X)
132800940225     C                   ENDIF
132900940728     C*14=Commenti
133000940831     C**                   Z-ADD1         X
133100940831     C**         14        LOKUP$OP1,X                   21
133200940831     C**         *IN21     IFEQ *ON
133300940831     C**                   MOVE 'S'       $FL1,X
133400940831     C**                   ENDIF
133500940831     C*
133600940714     C* Se pgm richiamato in ABILITAZIONE attivo :
133700940714     C*
133800970708    >C     XtaOpz        WHENEQ    '11'
133900940714     C* F10=Immissione
134000940714     C                   Z-ADD     10            X
134100940720     C                   MOVE      '0'           $FC2(X)
134200940714     C* F3=Fine
134300940714     C                   Z-ADD     3             X
134400940720     C                   MOVE      '0'           $FC2(X)
134500940714     C* 5=Visualizzazione
134600940714     C                   Z-ADD     1             X
134700940714     C     05            LOOKUP    $OP1(X)                                21
134800940714     C     *IN21         IFEQ      *ON
134900940714     C                   MOVE      'S'           $FL1(X)
135000940714     C                   ENDIF
135100940714     C* 11=Abilitazione
135200940714     C                   Z-ADD     1             X
135300940714     C     11            LOOKUP    $OP1(X)                                21
135400940714     C     *IN21         IFEQ      *ON
135500940714     C                   MOVE      'S'           $FL1(X)
135600940714     C                   ENDIF
135700940714     C* 12=Disabilita
135800940714     C                   Z-ADD     1             X
135900940714     C     12            LOOKUP    $OP1(X)                                21
136000940714     C     *IN21         IFEQ      *ON
136100940714     C                   MOVE      'S'           $FL1(X)
136200940714     C                   ENDIF
136300940714     C*
136400940714     C* Se pgm richiamato in GESTIONE  attivo :
136500940714     C*
136600970708    >C     XtaOpz        WHENEQ    '02'
136700940714     C* F3=Fine
136800940714     C                   Z-ADD     3             X
136900940720     C                   MOVE      '0'           $FC2(X)
137000940714     C* 2=Modifica
137100940714     C                   Z-ADD     1             X
137200940714     C     02            LOOKUP    $OP1(X)                                21
137300940714     C     *IN21         IFEQ      *ON
137400940714     C                   MOVE      'S'           $FL1(X)
137500940714     C                   ENDIF
137600940714     C* 3=Copia
137700940714     C                   Z-ADD     1             X
137800940714     C     03            LOOKUP    $OP1(X)                                21
137900940714     C     *IN21         IFEQ      *ON
138000940714     C                   MOVE      'S'           $FL1(X)
138100940714     C                   ENDIF
138200940714     C* 4=Annullamento
138300940714     C                   Z-ADD     1             X
138400940714     C     04            LOOKUP    $OP1(X)                                21
138500940714     C     *IN21         IFEQ      *ON
138600940714     C                   MOVE      'S'           $FL1(X)
138700940714     C                   ENDIF
138800940714     C* 5=Visualizzazione
138900940714     C                   Z-ADD     1             X
139000940714     C     05            LOOKUP    $OP1(X)                                21
139100940714     C     *IN21         IFEQ      *ON
139200940714     C                   MOVE      'S'           $FL1(X)
139300940714     C                   ENDIF
139400940729     C*14=Commenti
139500940831     C**                   Z-ADD1         X
139600940831     C**         14        LOKUP$OP1,X                   21
139700940831     C**         *IN21     IFEQ *ON
139800940831     C**                   MOVE 'S'       $FL1,X
139900940831     C**                   ENDIF
140000940225     C*
140100940225     C                   OTHER
140200940714     C* Se pgm non richiamato, attivo TUTTO  :
140300940714     C* 2=Modifica
140400940714     C                   Z-ADD     1             X
140500940714     C     02            LOOKUP    $OP1(X)                                21
140600940714     C     *IN21         IFEQ      *ON
140700940714     C                   MOVE      'S'           $FL1(X)
140800940714     C                   ENDIF
140900940714     C* 3=Copia
141000940714     C                   Z-ADD     1             X
141100940714     C     03            LOOKUP    $OP1(X)                                21
141200940714     C     *IN21         IFEQ      *ON
141300940714     C                   MOVE      'S'           $FL1(X)
141400940714     C                   ENDIF
141500940714     C* 4=Annullamento
141600940714     C                   Z-ADD     1             X
141700940714     C     04            LOOKUP    $OP1(X)                                21
141800940714     C     *IN21         IFEQ      *ON
141900940714     C                   MOVE      'S'           $FL1(X)
142000940714     C                   ENDIF
142100940714     C* 5=Visualizzazione
142200940714     C                   Z-ADD     1             X
142300940714     C     05            LOOKUP    $OP1(X)                                21
142400940714     C     *IN21         IFEQ      *ON
142500940714     C                   MOVE      'S'           $FL1(X)
142600940714     C                   ENDIF
142700940714     C* 11=Abilitazione
142800940714     C                   Z-ADD     1             X
142900940714     C     11            LOOKUP    $OP1(X)                                21
143000940714     C     *IN21         IFEQ      *ON
143100940714     C                   MOVE      'S'           $FL1(X)
143200940714     C                   ENDIF
143300940714     C* 12=Disabilita
143400940714     C                   Z-ADD     1             X
143500940714     C     12            LOOKUP    $OP1(X)                                21
143600940714     C     *IN21         IFEQ      *ON
143700940714     C                   MOVE      'S'           $FL1(X)
143800940714     C                   ENDIF
143900940728     C*14=Commenti
144000940831     C**                   Z-ADD1         X
144100940831     C**         14        LOKUP$OP1,X                   21
144200940831     C**         *IN21     IFEQ *ON
144300940831     C**                   MOVE 'S'       $FL1,X
144400940831     C**                   ENDIF
144500940225     C* poi, disattivo a seconda delle autorizzazioni :
144600940314    >C*********  AUTXXX    IFEQ 'N'
144700940314    >C*********            Z-ADD1         X
144800940314    >C*********  'XX'      LOKUP$OP1,X                   21
144900940314    >C*********  *IN21     IFEQ *ON
145000940314    >C*********            MOVE 'N'       $FL1,X
145100940314    >C*********            ENDIF
145200940314    >C*********            ENDIF
145300940225     C*
145400940225     C                   ENDSL
145500940225     C*
145600940225     C                   ENDSR
145700940225     C/EJECT
145800940225     C************************************************************
145900940225     C* REPERIMENTO OPZIONI
146000940225     C************************************************************
146100940225     C     REPOPZ        BEGSR
146200940225     C*
146300940314     C     1             DO        $DIMOP        X
146400940128     C*
146500940223     C     $MS1(X)       IFNE      *BLANK
146600940128     C*
146700940223     C                   MOVEL     $MS1(X)       IDMSG
146800970214     C                   CALLB     'XRTVM'
146900940201     C                   PARM                    IDMSG            27
147000940201     C                   PARM                    TXTMSG          100
147100940201     C                   PARM                    ERRMSG            1
147200940128     C*
147300940128     C     ERRMSG        IFNE      *ON
147400940223     C                   MOVEL     TXTMSG        $TX1(X)
147500940128     C                   ELSE
147600940223     C                   MOVEL     *ALL'?'       $TX1(X)
147700940128     C                   END
147800940128     C*
147900940128     C                   END
148000940128     C*
148100940128     C                   END
148200940128     C*
148300940128     C                   ENDSR
148400940506     C/EJECT
148500940506     C************************************************************
148600940506     C* REPERIMENTO TASTI DI FUNZIONE
148700940506     C************************************************************
148800940506     C     REPKEY        BEGSR
148900940506     C*
149000940506     C                   MOVEA     $FM2          $FM
149100940506     C                   EXSR      REPKKK
149200940506     C                   MOVEA     $FX           $FX2
149300940506     C                   ENDSR
149400940506     C/EJECT
149500940506     C************************************************************
149600940506     C* REPERIMENTO TASTI DI FUNZIONE
149700940506     C************************************************************
149800940506     C     REPKKK        BEGSR
149900940506     C*
150000940506     C                   CLEAR                   $FX
150100940506     C     1             DO        24            X
150200940506     C*
150300940506     C     $FM(X)        IFNE      *BLANK
150400940506     C*
150500940506     C                   MOVEL     $FM(X)        IDMSG
150600970214     C                   CALLB     'XRTVM'
150700940506     C                   PARM                    IDMSG            27
150800940506     C                   PARM                    TXTMSG          100
150900940506     C                   PARM                    ERRMSG            1
151000940506     C*
151100940506     C     ERRMSG        IFNE      *ON
151200940506     C                   MOVEL     TXTMSG        $FX(X)
151300940506     C                   ELSE
151400940506     C                   MOVEL     *ALL' '       $FX(X)
151500940506     C                   END
151600940506     C*
151700940506     C                   END
151800940506     C*
151900940506     C                   END
152000940506     C*
152100940506     C                   ENDSR
152200940223     C/EJECT
152300940128     C************************************************************
152400940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
152500940128     C************************************************************
152600940223     C     DEFCAM        BEGSR
152700940128     C*
152800940223     C* Variabili per gestione videate
152900940223     C*
153000940223     C                   MOVE      *BLANK        $GEST             2
153100940223     C                   MOVE      *BLANK        $FINE             1
153200940223     C                   MOVE      *BLANK        $INZS1            1
153300940223     C                   MOVE      *BLANK        $LASTG            2
153400940223     C                   MOVE      *BLANK        $LASTV            2
153500940223     C                   MOVE      *BLANK        $EFILE            1
153600940223     C                   MOVE      *BLANK        $ESCI             1
153700940223     C                   MOVE      *BLANK        $RCDOK            1
153800940223     C*
153900940314     C* Variabili appoggio sempre presenti in tutte le anagrafiche
154000940223     C*
154100940223     C     *LIKE         DEFINE    C1RCD         S1NRR
154200940223     C     *LIKE         DEFINE    C1NRR         WSFL
154300940223     C     *LIKE         DEFINE    C1RCD         WMAX
154400940223     C     *LIKE         DEFINE    C1RCD         WPAG
154500940223     C     *LIKE         DEFINE    $INZS1        WINZS1
154600940223     C                   MOVE      *BLANK        WWGRP             3
154700940223     C                   Z-ADD     0             CURR              2 0
154800940223     C                   Z-ADD     0             CURC              2 0
154900940223     C                   MOVE      *BLANK        WIN              99
155000940314     C     *LIKE         DEFINE    C1POS         W1POS
155100940620     C*
155200940721$022 C*    K5TBT1        KLIST
155300940721$022 C*                  KFLD                    TBTAZI
155400940721$022 C*                  KFLD                    TBTCOD
155500940721$022 C*                  KFLD                    TBTKEY
155600940721$022 C*                  KFLD                    TBTLIN
155700940721$022 C*                  KFLD                    TBTFLD
155800940714     C*
155900940714     C     KABL          KLIST
156000940714     C                   KFLD                    ABLCOD
156100940714     C                   KFLD                    ABLKEY
156200940714     C                   KFLD                    ABLAZI
156300940314     C*
156400940314     C*
156500940223     C* Indici
156600940223     C*
156700940223     C                   Z-ADD     0             X                 3 0
156800940223     C                   Z-ADD     0             Y                 3 0
156900940127     C*
157000940117     C                   ENDSR
157100940223     C/EJECT
157200940616**
157300940225PROMSG    *LIBL     OPZ0001
157400940225PROMSG    *LIBL     OPZ0002
157500940131PROMSG    *LIBL     OPZ0003
157600940131PROMSG    *LIBL     OPZ0004
157700940131PROMSG    *LIBL     OPZ0005
157800940224PROMSG    *LIBL     OPZ0006
157900940224PROMSG    *LIBL     OPZ0007
158000940224PROMSG    *LIBL     OPZ0008
158100940224PROMSG    *LIBL     OPZ0009
158200970716PROMSG    *LIBL     OPZ0010
158300970716PROMSG    *LIBL     OPZ0013
158400970716PROMSG    *LIBL     OPZ0012
158500970716PROMSG    *LIBL     OPZ0011
158600970716PROMSG    *LIBL     OPZ0014
158700940201**
15880094022501S      <Selezione
15890094022502S      <Modifica
15900094022503S      <Copia
15910094022504S      <Annullamento
15920094022505S      <Visualizzazione
15930094060706N      <prova
15940094060707N      <prova
15950094060708N      <prova
15960094060709N      <prova
15970094072910S      <Invia
15980094071411S      <Abilita gruppo
15990094071412S      <Disabilita
16000094072813N      <PROVA
16010094083114N      <Commenti
160200940506** TASTI DI FUNZIONE  S1
160300940506PROMSG    *LIBL     KEY0001  01
160400940506PROMSG    *LIBL     KEY0002  02
160500940506PROMSG    *LIBL     KEY0003  03
160600940506PROMSG    *LIBL     KEY0004  04
160700940506PROMSG    *LIBL     KEY0005  05
160800940506PROMSG    *LIBL     KEY0006  06
160900940506PROMSG    *LIBL     KEY0007  07
161000940506PROMSG    *LIBL     KEY0008  08
161100940506PROMSG    *LIBL     KEY0009  09
161200970716PROMSG    *LIBL     KEY0010  10
161300970716PROMSG    *LIBL     KEY0011  11
161400970716PROMSG    *LIBL     KEY0012  12
161500970716PROMSG    *LIBL     KEY0013  13
161600970716PROMSG    *LIBL     KEY0014  14
161700970716PROMSG    *LIBL     KEY0015  15
161800970716PROMSG    *LIBL     KEY0016  16
161900970716PROMSG    *LIBL     KEY0017  17
162000970716PROMSG    *LIBL     KEY0018  18
162100970716PROMSG    *LIBL     KEY0019  19
162200940506PROMSG    *LIBL     KEY0020  20
162300940506PROMSG    *LIBL     KEY0021  21
162400940506PROMSG    *LIBL     KEY0022  22
162500940506PROMSG    *LIBL     KEY0023  23
162600940506PROMSG    *LIBL     KEY0024  24
162700940506**  ABILITAZIONE S1
1628009410140N  01
1629009407200N  02
1630009407201N  03
1631009407200N  04
1632009407200N  05
1633009407200N  06
1634009407200N  07
1635009407200N  08
1636009407200N  09
1637009407201N  10
1638009407200N  11
1639009407201N  12
1640009407200N  13
1641009407200N  14
1642009407200N  15
1643009407200N  16
1644009407200N  17
1645009407200N  18
1646009407200N  19
1647009407200N  20
1648009407200N  21
1649009407200N  22
1650009505020N  23
1651009407200N  24
