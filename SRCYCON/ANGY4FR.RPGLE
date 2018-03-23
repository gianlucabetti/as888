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
003900940715$001 FANGY4FD   CF   E             WORKSTN USROPN
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
014200940607$003 D EXTDS         E DS                  EXTNAME(ANGY4FDS)
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
018700940127     C*----------------------------------------------------*
018800940127     C*                MAIN LINE PROGRAM
018900940127     C*----------------------------------------------------*
019000940127     C*
019100940223     C* inizializzazione variabili
019200940223     C                   EXSR      INZVAR
019300940223     C*
019400940223     C     $FINE         DOWEQ     *OFF
019500940131     C     $GEST         CASEQ     'S1'          GESS1
019600940117     C                   END
019700940117     C                   END
019800940325     C*
019900940325     C* fine programma
020000940325     C                   EXSR      ENDPGM
020100940225     C*
020200940325     C/EJECT
020300940325     C************************************************************
020400940325     C* FINE PROGRAMMA
020500940325     C************************************************************
020600940325     C     ENDPGM        BEGSR
020700940325     C*
020800940325     C* passaggio dati al pgm chiamante
020900940715     C                   MOVE      *BLANKS       KPJBU
021000940607     C                   MOVEL     XTABDS        KPJBU
021100940325     C*
021200940325     C* Se pgm Guida non è stato richiamato posso chiudere in LR
021300970708     C     XtaOpz        IFEQ      *BLANK
021400940325     C                   SETON                                            LR
021500960906     C                   RETURN
021600940325     C* altrimenti chiudo in RT
021700940325     C                   ELSE
021800940325     C                   SETON                                            RT
021900960906     C                   RETURN
022000940325     C                   ENDIF
022100940325     C*
022200940325     C                   ENDSR
022300940131     C/EJECT
022400940224     C************************************************************
022500940131     C* RIEMPIMENTO OPZIONI
022600940224     C************************************************************
022700940131     C     RIEOPZ        BEGSR
022800940223     C*
022900940223     C* Input :
023000940223     C* - $TX = schiera con tutti i testi disponibili
023100940223     C* - $FL = schiere con i flag di validità associati ai testi
023200940225     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
023300940223     C*          opzione usato
023400940223     C* - WMXO = variabile contenente il numero massimo di testi
023500940223     C*          opzione usabili
023600940223     C* Output :
023700940223     C* - WOPZ1 = variabile contenente la prima riga opzioni
023800940223     C* - WOPZ2 = variabile contenente la seconda riga opzioni
023900950502     C* - FF23  = flag che indica se c'è bisogno del tasto F23
024000940223     C*
024100970214     C                   CALLB     'X04OPMSG'
024200940223     C* input
024300940223     C                   PARM                    $TX
024400940223     C                   PARM                    $FL
024500940225     C                   PARM                    WCOZ              3 0
024600940223     C                   PARM                    WMXO              3 0
024700940223     C* output
024800940223     C                   PARM                    WOPZ1            76
024900940223     C                   PARM                    WOPZ2            76
025000950502     C                   PARM                    FF23              1
025100940607     C* input
025200940607$018 C                   PARM      61            $L01              3 0
025300940607     C                   PARM      0             $L02              3 0
025400940223     C*
025500940131     C                   ENDSR
025600940506     C/EJECT
025700940506     C************************************************************
025800940506     C* RIEMPIMENTO TASTI FUNZIONE
025900940506     C************************************************************
026000940506     C     RIEKEY        BEGSR
026100940506     C*
026200940506     C* Input :
026300940506     C* - $FX = schiera con tutti i testi disponibili
026400940506     C* - $FC = schiere con i flag di validità associati ai testi
026500940506     C* - $FR = schiere con i flag di REVERSE IMAGE
026600940506     C* - $ULK = ultimo testo caricato
026700940506     C* Output :
026800940506     C* - $ULK
026900940506     c* - $KEY1= 1° riga di tasti funzione
027000940506     c* - $KEY2= 2° riga di tasti funzione
027100940506     C*
027200970214     C                   CALLB     'XKEYMSG'
027300940506     C* input
027400940506     C                   PARM                    $FX
027500940506     C                   PARM                    $FC
027600940506     C                   PARM                    $FR
027700940506     C                   PARM                    $ULK              3 0
027800940506     C* output
027900940506     C                   PARM                    $KEY1            79
028000940506     C                   PARM                    $KEY2            79
028100940607     C* input
028200940607$018 C                   PARM      61            $L01              3 0
028300941004$018 C                   PARM      61            $L02              3 0
028400940506     C*
028500940506     C                   ENDSR
028600940224     C/EJECT
028700940224     C************************************************************
028800940224     C* GESTIONE EMISSIONE TASTI FUNZIONE
028900940224     C************************************************************
029000940224     C     GESPIE        BEGSR
029100940224     C*
029200940224     C                   SELECT
029300940506     C* Lista
029400940506     C     $GEST         WHENEQ    'S1'
029500940506     C     $ULKS1        IFEQ      0
029600940506     C                   EXSR      F24S1
029700940506     C                   ELSE
029800940607     C                   WRITE     Z1
029900940506     C                   ENDIF
030000940506     C                   ENDSL
030100940506     C*
030200940224     C                   ENDSR
030300940506     C/EJECT
030400940506     C************************************************************
030500940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA S1
030600940506     C************************************************************
030700940506     C     CTRKS1        BEGSR
030800940506     C                   SETOFF                                       99
030900940506     C                   MOVEA     $FC2          $FC
031000940506     C                   EXSR      CTRKEY
031100940506     C     *IN99         IFEQ      *ON
031200940506     C                   SETON                                        97
031300940506     C                   ENDIF
031400940607     C*
031500940506     C                   ENDSR
031600940506     C/EJECT
031700940506     C************************************************************
031800940506     C* CONTROLLO ABILITAZIONE TASTI
031900940509     C* INPUT
032000940509     c* $TASTO =Tasto funzione premuto
032100940509     C* $FC = Tasti abilitati
032200940509     C* OUTPUT
032300940509     C* IN99 = ERrore
032400940506     C************************************************************
032500940506     C     CTRKEY        BEGSR
032600940506     C                   SELECT
032700940506     C     $TASTO        WHENEQ    F01
032800940720     C     $FC(1)        IFEQ      '0'
032900940506     C                   SETON                                        99
033000940506     C                   ENDIF
033100940506     C     $TASTO        WHENEQ    F02
033200940720     C     $FC(2)        IFEQ      '0'
033300940506     C                   SETON                                        99
033400940506     C                   ENDIF
033500940506     C     $TASTO        WHENEQ    F03
033600940720     C     $FC(3)        IFEQ      '0'
033700940506     C                   SETON                                        99
033800940506     C                   ENDIF
033900940506     C     $TASTO        WHENEQ    F04
034000940720     C     $FC(4)        IFEQ      '0'
034100940506     C                   SETON                                        99
034200940506     C                   ENDIF
034300940506     C     $TASTO        WHENEQ    F05
034400940720     C     $FC(5)        IFEQ      '0'
034500940506     C                   SETON                                        99
034600940506     C                   ENDIF
034700940506     C     $TASTO        WHENEQ    F06
034800940720     C     $FC(6)        IFEQ      '0'
034900940506     C                   SETON                                        99
035000940506     C                   ENDIF
035100940506     C     $TASTO        WHENEQ    F07
035200940720     C     $FC(7)        IFEQ      '0'
035300940506     C                   SETON                                        99
035400940506     C                   ENDIF
035500940506     C     $TASTO        WHENEQ    F08
035600940720     C     $FC(8)        IFEQ      '0'
035700940506     C                   SETON                                        99
035800940506     C                   ENDIF
035900940506     C     $TASTO        WHENEQ    F09
036000940720     C     $FC(09)       IFEQ      '0'
036100940506     C                   SETON                                        99
036200940506     C                   ENDIF
036300940506     C     $TASTO        WHENEQ    F10
036400940720     C     $FC(10)       IFEQ      '0'
036500940506     C                   SETON                                        99
036600940506     C                   ENDIF
036700940506     C     $TASTO        WHENEQ    F11
036800940720     C     $FC(11)       IFEQ      '0'
036900940506     C                   SETON                                        99
037000940506     C                   ENDIF
037100940506     C     $TASTO        WHENEQ    F12
037200940720     C     $FC(12)       IFEQ      '0'
037300940506     C                   SETON                                        99
037400940506     C                   ENDIF
037500940506     C     $TASTO        WHENEQ    F13
037600940720     C     $FC(13)       IFEQ      '0'
037700940506     C                   SETON                                        99
037800940506     C                   ENDIF
037900940506     C     $TASTO        WHENEQ    F14
038000940720     C     $FC(14)       IFEQ      '0'
038100940506     C                   SETON                                        99
038200940506     C                   ENDIF
038300940506     C     $TASTO        WHENEQ    F15
038400940720     C     $FC(15)       IFEQ      '0'
038500940506     C                   SETON                                        99
038600940506     C                   ENDIF
038700940506     C     $TASTO        WHENEQ    F16
038800940720     C     $FC(16)       IFEQ      '0'
038900940506     C                   SETON                                        99
039000940506     C                   ENDIF
039100940506     C     $TASTO        WHENEQ    F17
039200940720     C     $FC(17)       IFEQ      '0'
039300940506     C                   SETON                                        99
039400940506     C                   ENDIF
039500940506     C     $TASTO        WHENEQ    F18
039600940720     C     $FC(18)       IFEQ      '0'
039700940506     C                   SETON                                        99
039800940506     C                   ENDIF
039900940506     C     $TASTO        WHENEQ    F19
040000940720     C     $FC(19)       IFEQ      '0'
040100940506     C                   SETON                                        99
040200940506     C                   ENDIF
040300940506     C     $TASTO        WHENEQ    F20
040400940720     C     $FC(20)       IFEQ      '0'
040500940506     C                   SETON                                        99
040600940506     C                   ENDIF
040700940506     C     $TASTO        WHENEQ    F21
040800940720     C     $FC(21)       IFEQ      '0'
040900940506     C                   SETON                                        99
041000940506     C                   ENDIF
041100940506     C     $TASTO        WHENEQ    F22
041200940720     C     $FC(22)       IFEQ      '0'
041300940506     C                   SETON                                        99
041400940506     C                   ENDIF
041500940506     C     $TASTO        WHENEQ    F23
041600940720     C     $FC(23)       IFEQ      '0'
041700940506     C                   SETON                                        99
041800940506     C                   ENDIF
041900940506     C     $TASTO        WHENEQ    F24
042000940720     C     $FC(24)       IFEQ      '0'
042100940506     C                   SETON                                        99
042200940506     C                   ENDIF
042300940506     C                   ENDSL
042400940506     C                   ENDSR
042500940224     C/EJECT
042600940127     C************************************************************
042700940131     C* GESTIONE LISTA
042800940127     C************************************************************
042900940127     C     GESS1         BEGSR
043000940223     C* inizializzazione videata
043100940223     C     $INZS1        IFEQ      *ON
043200940127     C                   EXSR      INZS1
043300940223     C                   MOVE      *OFF          $INZS1
043400940127     C                   ENDIF
043500940223     C* emissione piede videata se proveniente da altra
043600940223     C* salvataggio valore formato di provenienza
043700940202     C     $LASTV        IFNE      'S1'
043800940607!!   C*                    WRITEFMTT1
043900940224     C                   EXSR      GESPIE
044000940202     C                   MOVE      $LASTV        $LASTG
044100940202     C                   MOVE      'S1'          $LASTV
044200940202     C                   END
044300940223     C*
044400940223     C     WMAX          IFEQ      0
044500940607     C                   WRITE     D1
044600940211     C                   ENDIF
044700940729     C                   MOVE      '1'           WIN727            1
044800940127     C*
044900940607     C                   EXFMT     C1
045000940204     C     C1NRR         IFNE      0
045100940204     C                   Z-ADD     C1NRR         WSFL
045200940204     C                   ENDIF
045300940127     C                   Z-ADD     SFLNRR        C1RCD
045400940506     C*
045500940506     C* controllo abilitazione tasti funzione
045600940506     C*
045700940506     C                   EXSR      CTRKS1
045800940506     C     *IN99         IFEQ      *OFF
045900940506     C*
046000940127     C*
0461009401271    C                   SELECT
046200940127     C* F3=Fine
046300940506     C     $TASTO        WHENEQ    F03
046400940309     C                   EXSR      F03S1
046500940131     C* F10=Immissione
046600940506     C     $TASTO        WHENEQ    F10
046700940309     C                   EXSR      F10S1
046800940131     C* F12=Ritorno
046900940506     C     $TASTO        WHENEQ    F12
047000940309     C                   EXSR      F12S1
047100940224     C* F23=Altre opzioni
047200940506     C     $TASTO        WHENEQ    F23
047300940309     C                   EXSR      F23S1
047400940224     C* F24=Altri tasti
047500940506     C     $TASTO        WHENEQ    F24
047600940309     C                   EXSR      F24S1
047700940127     C* ROLL-UP
047800940506     C     $TASTO        WHENEQ    ROLLUP
047900940127     C                   EXSR      ROLS1
048000940127     C*
0481009401271O   C                   OTHER
048200940127     C* CONTROLLO DATI
048300940131     C                   EXSR      CTRC1
048400940201     C     *IN99         IFEQ      *OFF
048500940131     C                   EXSR      CTRS1
048600940131     C                   END
0487009401271-   C                   ENDSL
048800940127     C*
048900940506     C                   ENDIF
049000940127     C                   ENDSR
049100940224     C/EJECT
049200940127     C************************************************************
049300940131     C* INIZIALIZZAZIONE LISTA
049400940127     C************************************************************
049500940127     C     INZS1         BEGSR
049600940302     C* pulizia SFL
049700940128     C                   SETOFF                                         3031
049800940607     C                   WRITE     C1
049900940128     C                   SETON                                          31
050000010406X0113C*
050100010406  "  C* reperimento titolo
050200010406  "  C                   move      xtbcod        WrkTbcod
050300010406  "  C                   eval      xtbkey      = *zero
050400010406  "  C                   move      xtbcod        xtbkey
050500010406  "  C                   eval      xtbazi      = xscsoc
050600010406  "  C                   eval      xtblin      = xsclin
050700010406  "  C                   eval      xtbcod      = 'AAA'
050800010406  "  C                   eval      xtbric      = '1'
050900010406  "  C                   eval      xtbalc      = '0'
051000010406  "  C                   CALLB     'XATB'
051100010406  "  C                   PARM                    XATBDS
051200010406  "  C                   if        xtberr <> *off
051300010406  "  C                   movel     *all'?'       WrkTit
051400010406  "  C                   else
051500010406  "  C                   movel     xtbuni        WrkTit
051600010406  "  C                   endif
051700010406  "  C*
051800010406  "  C* centratura
051900010406  "  C                   move      WrkTbcod      xtbcod
052000010406  "  C                   eval      WrkTit2=%trimr(Wrktit)+X'22'+xtbcod
052100010406  "  C                             +X'20'
052200010406  "  C     ' '           CHECKR    Wrktit2       I
052300010406  "  C                   call      'X71CENTRA'
052400010406  "  C                   parm                    Wrktit2
052500010406  "  C                   parm                    I
052600010406  "  C                   parm                    lentit
052700010406X0113C                   movel     Wrktit2       C1tit
052800940127     C*
052900940128     C* riempimento opzioni
053000940223     C                   MOVEA     $TX1          $TX
053100940223     C                   MOVEA     $FL1          $FL
053200940309     C                   Z-ADD     0             WCOZ
053300940225     C*  equivale al num.di elementi della schiera $TX1
053400940509     C                   Z-ADD     $DIMOP        WMXO
053500940128     C                   EXSR      RIEOPZ
053600950502     C                   Z-ADD     23            X
053700950502     C                   MOVE      FF23          $FC2(X)
053800940608     C                   MOVEL     WOPZ1         C1OPZ1
053900940608     C**                   MOVELWOPZ2     C1OPZ2
054000940225     C                   Z-ADD     WCOZ          C1COZ
054100940128     C*
054200940128     C* CARICAMENTO SFL A PAGINE
054300940128     C*
054400940201     C                   Z-ADD     0             S1NRR
054500940128     C                   Z-ADD     0             WMAX
054600940224     C*
054700940224     C* Posizionamento su file pilota
054800940302     C                   CLEAR                   C1POS
054900940316    >C                   EXSR      SETANA
055000940608    >C                   EXSR      REDANA
055100940302     C* ripristino i valori del posizionamento
055200940302     C                   MOVE      W1POS         C1POS
055300940131     C*
055400940131     C* Carico 1a pagina
055500940127     C                   EXSR      ROLS1
055600940201     C                   Z-ADD     SFLPAG        WPAG
055700940127     C*
055800940127     C                   ENDSR
055900940127     C/EJECT
056000940127     C************************************************************
056100940131     C* CARICAMENTO PAGINA LISTA
056200940127     C************************************************************
056300940127     C     ROLS1         BEGSR
056400940127     C*
056500940128     C                   SETOFF                                       32
056600940223     C                   Z-ADD     0             Y
056700940127     C                   Z-ADD     WMAX          S1NRR
056800940127     C*
056900940127     C* Leggo dal file anagrafico per caricare la lista
057000940127     C*
0571009401311    C     $EFILE        DOWEQ     *OFF
057200940201     C     Y             ANDLT     WPAG
057300940127     C*
057400940131     C                   EXSR      RIES1
057500940127     C*
057600940127     C                   ADD       1             S1NRR
057700940127     C                   ADD       1             Y
057800940714     C                   MOVE      $IN41         *IN41
057900940127     C*
058000940607     C                   WRITE     S1
058100940131     C*
058200940316    >C                   EXSR      REDANA
058300940128     C*
0584009401271-   C                   ENDDO
058500940127     C*
058600940223     C                   Z-ADD     S1NRR         WMAX                 30
058700940127     C*
058800940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
058900940127     C*
059000940201     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
059100940127     C                   MVR                     RESTO             3 0
059200940131     C     PAGINE        MULT      SFLPAG        C1RCD
0593009401271    C     RESTO         IFGT      0
059400940127     C                   ADD       1             C1RCD
0595009401271E   C                   ELSE
059600940407     C                   SUB       SFLPAG        C1RCD
059700940407     C                   ADD       1             C1RCD
0598009401271-   C                   ENDIF
059900940128     C*
060000940127     C                   ENDSR
060100940224     C/EJECT
060200940224     C************************************************************
060300940224     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
060400940224     C************************************************************
060500940607     C     SETANA        BEGSR
060600940224     C*
060700941004     C                   MOVE      *BLANKS       XTBKEY
060800970708     C                   MOVE      XtaAzi        XTBAZI
060900970708     C                   MOVE      XtaLin        XTBLIN
061000940608     C                   MOVE      '9'           XTBRIC
061100940607     C                   MOVE      '0'           XTBALC
061200950622     C                   MOVE      XSCCUT        XTBUTE
061300940607     C                   MOVE      '0'           XTBERR
061400941121     C**         W1POS     IFNE *BLANK
061500941004     C                   MOVE      *ZEROS        XTBKEY
061600940608     C                   MOVE      W1POS         XTBKEY
061700941121     C**                   END
061800970214     C                   CALLB     'XATB'
061900940607     C                   PARM                    XATBDS
062000940607     C*
062100940224     C*
062200940224     C                   ENDSR
062300940128     C/EJECT
062400940128     C************************************************************
062500940131     C* LETTURA RCD ARCHIVIO PILOTA
062600940128     C************************************************************
062700940607     C     REDANA        BEGSR
062800940128     C*
062900940131     C                   MOVEL     *OFF          $RCDOK
063000940131     C*
0631009401311    C     $EFILE        DOUEQ     *ON
063200940131     C     $RCDOK        OREQ      *ON
063300940131     C*
063400940607     C                   MOVE      '3'           XTBRIC
063500940607     C                   MOVE      '0'           XTBERR
063600970214     C                   CALLB     'XATB'
063700940607     C                   PARM                    XATBDS
063800940607     C*
063900940607     C     XTBERR        IFNE      '0'
064000940607     C                   MOVEL     *ON           $EFILE
064100010214     C                   ELSE
064200010214     C                   MOVEL     *OFF          $EFILE
064300940607     C                   ENDIF
064400940131     C*
0645009401312    C     $EFILE        IFEQ      *OFF
064600940607     C                   EXSR      SELANA
064700950502     C                   MOVE      XTBKEY        SAVKEY
064800950502     C                   MOVE      XTBGRU        SAVGRU
064900950502     C                   MOVE      XTBUNI        SAVUNI
065000970509     C                   MOVE      XTBUNT        SAVUNT
0651009401312-   C                   ENDIF
065200940131     C*
0653009401311-   C                   ENDDO
065400940131     C*
065500940607     C                   MOVE      $EFILE        *IN33
065600940131     C*
065700940131     C                   ENDSR
065800940224     C/EJECT
065900940224     C************************************************************
066000940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
066100940224     C************************************************************
066200940224     C     CLCPAG        BEGSR
066300940224     C* Input :
066400940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
066500940224     C*          cursore
066600940224     C* - SFLPAG = numero rcd per pagina sfl
066700940224     C* Output :
066800940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
066900940224     C*
067000940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
067100940224     C                   MVR                     RESTO             3 0
067200940224     C     RESTO         IFGT      0
067300940224     C                   ADD       1             PAGINE
067400940224     C                   ENDIF
067500940224     C     PAGINE        MULT      SFLPAG        WPAG
067600940224     C*
067700940224     C                   ENDSR
067800940131     C/EJECT
067900940131     C************************************************************
068000940131     C* SELEZIONI SULLA RIGA LETTA
068100940314     C* impostare a *OFF il campo $RCDOK per scartare un record che
068200940314     C* non rientra nei parametri di parzializzazione
068300940314     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
068400940314     C* superiore stabilito nei parametri
068500940314     C*
068600940131     C************************************************************
068700940607     C     SELANA        BEGSR
068800940131     C*
068900940202     C                   MOVEL     *ON           $RCDOK
069000940714     C* Se aperto il pgm con opzione 11
069100940714     C* carico solo i record di gruppo
069200970708     C     XtaOpz        IFEQ      '11'
069300940714     C     XTBGRU        ANDNE     'G'
069400940714     C                   MOVEL     *OFF          $RCDOK
069500940714     C                   END
069600940714     C* Carica in alta intensità solo i record
069700940714     C* abilitati
069800940714     C                   MOVE      '0'           $IN41
069900970708     C     XtaOpz        IFEQ      '11'
070000940714     C     XTBGRU        ANDEQ     'G'
070100940715     C     XTBAZB        ANDNE     'N'
070200940714     C                   MOVE      '1'           $IN41
070300940714     C                   END
070400940714     C*
070500940202     C                   ENDSR
070600940131     C/EJECT
070700940131     C************************************************************
070800940131     C* RIEMPIMENTO RIGA LISTA
070900940131     C************************************************************
071000940131     C     RIES1         BEGSR
071100950502     C*                    MOVE XTBKEY    S1COD
071200950502     C*                    MOVE XTBGRU    S1GRU
071300950502     C                   MOVEL     SAVUNI        EXTDS
071400950502     C                   MOVE      SAVKEY        S1COD
071500950502     C                   MOVE      SAVGRU        S1GRU
071600970512     C                   MOVE      SAVUNT        S1UNITA           8
071700040422     C                   EVAL      S1FIRME = FIRMEY4F
071800981209$027 C
071900950502     C     *LIKE         DEFINE    XTBUNI        SAVUNI
072000950502     C     *LIKE         DEFINE    XTBKEY        SAVKEY
072100950502     C     *LIKE         DEFINE    XTBGRU        SAVGRU
072200970509     C     *LIKE         DEFINE    XTBUNT        SAVUNT
072300940131     C*
072400940131     C                   ENDSR
072500940309     C/EJECT
072600940309     C************************************************************
072700940309     C* GESTIONE F03 VIDEO S1
072800940309     C************************************************************
072900940309     C     F03S1         BEGSR
073000940309     C*
073100940309     C                   MOVE      *ON           $FINE
073200970708     C                   MOVE      '1'           XtaRet
073300940325     C* fine programma
073400940325     C                   EXSR      ENDPGM
073500940309     C*
073600940309     C                   ENDSR
073700940309     C/EJECT
073800940309     C************************************************************
073900940309     C* GESTIONE F10 VIDEO S1
074000940314     c* AGGIUNTA RECORD
074100940309     C************************************************************
074200940309     C     F10S1         BEGSR
074300940309     C*
074400940607     C                   RESET                   D002DS
074500940607     C                   MOVEL     '01'          OPZ002
074600970708     C                   MOVE      XtaLin        LIN002
074700970708     C                   MOVE      XtaAzi        AZI002
074800970512     C                   MOVE      S1UNITA       UNI002
074900940715     C                   MOVE      *BLANKS       KPJBU
075000940607     C                   MOVEL     D002DS        KPJBU
075100940608$004 C                   CALL      'ANGY4FR2'
075200940309     C                   PARM                    KPJBA
075300940607     C                   MOVEL     KPJBU         D002DS
075400940309     C* ritorno da PGM gestione
075500940309     C                   EXSR      GESRET
075600940309     C     WINZS1        IFEQ      *ON
075700940309     C                   MOVE      *ON           $INZS1
075800940309     C* se esistevano già righe sul sfl
075900940309     C* calcolo pagina a cui deve posizionarsi
076000940309     C     WSFL          IFGT      0
076100940309     C                   EXSR      CLCPAG
076200940309     C* altrimenti carico solo la 1a pagina
076300940309     C                   ELSE
076400940309     C                   Z-ADD     SFLPAG        WPAG
076500940309     C                   ENDIF
076600940309     C                   ENDIF
076700940309     C*
076800940712     C                   MOVE      *HIVAL        $LASTV
076900940309     C                   ENDSR
077000940309     C/EJECT
077100940309     C************************************************************
077200940309     C* GESTIONE F12 VIDEO S1
077300940309     C************************************************************
077400940309     C     F12S1         BEGSR
077500940309     C*
077600970708     C                   MOVE      '2'           XtaRet
077700940309     C                   MOVE      *ON           $FINE
077800940309     C*
077900940309     C                   ENDSR
078000940309     C/EJECT
078100940309     C************************************************************
078200940309     C* GESTIONE F23 VIDEO S1
078300940309     C************************************************************
078400940309     C     F23S1         BEGSR
078500940309     C*
078600940309     C                   EXSR      RIEOPZ
078700950502     C                   Z-ADD     23            X
078800950502     C                   MOVE      FF23          $FC2(X)
078900940608     C                   MOVEL     WOPZ1         C1OPZ1
079000940608     C**                   MOVELWOPZ2     C1OPZ2
079100940309     C                   Z-ADD     WCOZ          C1COZ
079200940309     C*
079300940309     C                   ENDSR
079400940309     C/EJECT
079500940309     C************************************************************
079600940309     C* GESTIONE F24 VIDEO S1
079700940309     C************************************************************
079800940309     C     F24S1         BEGSR
079900940309     C*
080000940506     C*
080100940506     C                   MOVEA     $FX2          $FX
080200940506     C                   MOVEA     $FC2          $FC
080300940506     C                   MOVEA     $FR2          $FR
080400940506     C                   Z-ADD     $ULKS1        $ULK
080500940506     C                   EXSR      RIEKEY
080600941017     C                   MOVEA     $FC           $FC2
080700940506     C                   Z-ADD     $ULK          $ULKS1            3 0
080800940607     C                   MOVEL     $KEY1         Z1KE1
080900941004     C                   MOVEL     $KEY2         Z1KE2
081000940607     C                   WRITE     Z1
081100940506     C*
081200940309     C                   ENDSR
081300940128     C/EJECT
081400940128     C************************************************************
081500940131     C* CONTROLLO TESTATA LISTA
081600940128     C************************************************************
081700940131     C     CTRC1         BEGSR
081800940128     C*
081900940201     C                   MOVE      *OFF          *IN99
082000940131     C*
082100940202     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
082200940202     C* L'INIZIALIZZAZIONE LISTA
082300940202     C*
082400940207     C     C1POS         IFNE      W1POS
082500940202     C                   EXSR      POSS1
082600940202     C                   END
082700940202     C*
082800940202     C                   ENDSR
082900940202     C/EJECT
083000940202     C************************************************************
083100940202     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
083200940202     C************************************************************
083300940202     C     POSS1         BEGSR
083400940202     C*
083500940202     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
083600940202     C*
0837009402021    C     WMAX          IFGT      0
083800940607     C                   READC     S1                                     21
0839009402022    C     *IN21         DOWEQ     *OFF
084000940202     C     *IN99         ANDEQ     *OFF
084100940202     C*
0842009402023    C     S1OPZ         IFNE      0
084300940202     C                   SETON                                        51  99
0844009402023-   C                   ENDIF
084500940202     C* se rilevata opzione attiva, attivo sflnxtchg
0846009402023    C     *IN99         IFEQ      *ON
084700940202     C                   MOVE      *ON           *IN32
084800940714     C                   MOVE      $IN41         *IN41
084900940607     C                   UPDATE    S1
0850009402023E   C                   ELSE
085100940607     C                   READC     S1                                     21
0852009402023-   C                   ENDIF
085300940202     C*
0854009402022-   C                   ENDDO
085500940202     C*
0856009402021-   C                   ENDIF
085700940202     C*
085800940221     C*
0859009402021    C     *IN99         IFEQ      *OFF
086000941121     C                   CLEAR                   W1POS
086100940202     C*
086200940302     C                   MOVEL     C1POS         W1POS
086300940131     C*
086400940202     C                   MOVE      *ON           *IN99
086500940223     C                   MOVE      *ON           $INZS1
086600940202     C*
086700940202     C*
0868009402021-   C                   ENDIF
086900940202     C*
087000940131     C                   ENDSR
087100940131     C/EJECT
087200940131     C************************************************************
087300940131     C* CONTROLLO OPZIONI LISTA
087400940131     C************************************************************
087500940131     C     CTRS1         BEGSR
087600940131     C*
087700940202     C                   MOVEL     *OFF          $ESCI
087800940201     C                   SETOFF                                       99
087900940131     C                   Z-ADD     0             S1OPZ
088000940131     C*
088100940127     C* Leggo il sfl solo se ci sono rcd
0882009401311    C     WMAX          IFGT      0
088300940607     C                   READC     S1                                     21
088400940127     C*
088500940131     C* esce se fine sfl o errore che richiede l'uscita
0886009401312    C     *IN21         DOWEQ     *OFF
088700940131     C     $ESCI         ANDEQ     *OFF
088800940201     C                   Z-ADD     S1NRR         C1RCD
088900940131     C* ctrl su riga
089000940131     C                   EXSR      RECS1
089100940131     C* gestione opzioni
0892009401313    C     S1OPZ         IFNE      0
089300940201     C     *IN99         ANDEQ     *OFF
089400940131     C                   EXSR      OPZS1
0895009401273-   C                   ENDIF
089600940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0897009402013    C     *IN99         IFEQ      *ON
089800940201     C     $ESCI         OREQ      *ON
089900940131     C                   MOVE      *ON           *IN32
090000940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
090100940204     C* la ripristinerò a conclusione del ciclo di READC
090200940223     C                   MOVE      *OFF          $INZS1
0903009402233-   C                   ENDIF
090400940223     C*
090500940131     C                   Z-ADD     0             S1OPZ
090600940223     C*
090700940714     C                   MOVE      $IN41         *IN41
090800940607     C                   UPDATE    S1
090900940223     C*
091000940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0911009401313    C     $ESCI         IFEQ      *OFF
091200940607     C                   READC     S1                                     21
091300940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0914009402014    C     *IN21         IFEQ      *ON
091500940201     C                   MOVE      WINZS1        $INZS1
091600940204     C* calcolo pagina a cui deve posizionarsi
091700940224     C                   EXSR      CLCPAG
0918009402014-   C                   ENDIF
0919009402013-   C                   ENDIF
092000940131     C*
0921009401272-   C                   ENDDO
092200940127     C*
0923009401311-   C                   ENDIF
092400940131     C*
092500940127     C                   ENDSR
092600940127     C/EJECT
092700940127     C************************************************************
092800940131     C* CONTROLLO CAMPI I/O RIGA LISTA
092900940127     C************************************************************
093000940131     C     RECS1         BEGSR
093100940131     C*
093200940201     C* reset indicatori DSPATR
093300940201     C                   MOVE      *ALL'0'       IN4049           10
093400940201     C                   MOVEA     IN4049        *IN(40)
093500940201     C*
093600940131     C* Controllo se Utente Autorizzato alla scelta
093700940202     C     S1OPZ         IFNE      0
093800940131     C                   Z-ADD     1             X
093900940223     C     S1OPZ         LOOKUP    $OP1(X)                                21
094000940211     C     *IN21         IFEQ      *OFF
094100940223     C     $FL1(X)       OREQ      'N'
094200940131     C                   MOVE      *ON           $ESCI
094300940201     C                   SETON                                        504099
094400940131     C                   ENDIF
094500940131     C*
094600940620     c* Controllo autorizzazione alla modifica su record di gruppo
094700940620     C* Reperisco tipo codice (gruppo/azienda)
094800940620     C                   MOVE      *ALL'0'       XTBKEY
094900940620     C                   MOVE      S1COD         XTBKEY
095000970512     C                   MOVE      S1UNITA       XTBUNT
095100940620     C                   MOVE      '0'           XTBALC
095200940620     C                   MOVE      '1'           XTBRIC
095300970214     C                   CALLB     'XATB'
095400940620     C                   PARM                    XATBDS
095500940620     C* Reperisco autorizzazioni utente
095600940620     C                   CLEAR                   XCHATB
095700941118     C                   MOVE      XSCSOC        XCTSOC
095800941118     C                   MOVE      XSCDUN        XCTUNI
095900940620     C                   MOVEL     KNMUS         XCTPRF
096000940620     C                   MOVE      XTBCOD        XCTCOD
096100940620     C                   CALL      'XCHKATB'
096200940620     C                   PARM                    XCHATB
096300941111     C                   MOVEL     XCTDDS        AUTDS
096400941027     C     XCTRTN        IFEQ      1
096500941027     C                   MOVEL     'N'           MANGRU
096600941027     C                   ENDIF
096700940620     C* Non ammessa manutenzione di gruppo
096800940620   1 C     XTBGRU        IFEQ      'G'
096900940620     C     MANGRU        ANDEQ     'N'
097000940620   2 C     S1OPZ         IFEQ      2
097100940729     C     S1OPZ         OREQ      3
097200940620     C     S1OPZ         OREQ      4
097300940620     C                   SETON                                        5399
097400940620   2 C                   ENDIF
097500940620   1 C                   ENDIF
097600940620     C* Non ammessa manutenzione di azienda
097700940620   1 C     XTBGRU        IFEQ      'A'
097800940620     C     MANAZI        ANDEQ     'N'
097900940620   2 C     S1OPZ         IFEQ      2
098000940729     C     S1OPZ         OREQ      3
098100940620     C     S1OPZ         OREQ      4
098200940620     C                   SETON                                        5499
098300940620   2 C                   ENDIF
098400940620   1 C                   ENDIF
098500940607     C                   ENDIF
098600940131     C                   ENDSR
098700940224     C/EJECT
098800940131     C************************************************************
098900940131     C* GESTIONE OPZIONI LISTA
099000940131     C************************************************************
099100940131     C     OPZS1         BEGSR
099200940201     C*
0993009402251    C                   SELECT
099400940225     C*
099500940225     C* se opz=Selezione passo la chiave al pgm chiamante
099600940225     C     S1OPZ         WHENEQ    1
099700970708     C                   MOVE      S1OPZ         XtaOpz
099800970708     C                   MOVE      S1COD         XtaKey
099900970708     C                   MOVE      S1UNIta       xtaUni
100000940225     C                   MOVE      *ON           $ESCI
100100940225     C                   MOVE      *ON           $FINE
100200940729     C*
100300940714     C*
100400940714     C* se opz=Abilita scrive su archivio abilitazioni
100500940714     C     S1OPZ         WHENEQ    11
100600941103     C                   MOVE      XSCSOC        ABLAZI
100700940714     C                   MOVE      *ZEROS        ABLKEY
100800940714     C                   MOVE      S1COD         ABLKEY
100900970512     C                   MOVE      S1UNITA       ABLUNT
101000940714     C                   MOVE      XTBCOD        ABLCOD
101100940714     C                   WRITE     ANABL000                             22
101200940714     C*
101300940714     C* se opz=Disabil.cancella su archivio abilitazioni
101400940714     C     S1OPZ         WHENEQ    12
101500941103     C                   MOVE      XSCSOC        ABLAZI
101600940714     C                   MOVE      *ZEROS        ABLKEY
101700940714     C                   MOVE      S1COD         ABLKEY
101800970512     C                   MOVE      S1UNITA       ABLUNT
101900940714     C                   MOVE      XTBCOD        ABLCOD
102000940714     C     KABL          DELETE    ANABL000                           22
102100940729     C*
102200940225     C*
102300940225     C* se altre opzioni (o nessuna opzione) richiamo pgm gestione
1024009402251O   C                   OTHER
102500940225     C*
102600940616     C                   RESET                   D002DS
102700940607     C                   MOVEL     S1OPZ         OPZ002
102800940616     C                   MOVE      *ZERO         KEY002
102900940607     C                   MOVE      S1COD         KEY002
103000970708     C                   MOVE      XtaLin        LIN002
103100970708     C                   MOVE      XtaAzi        AZI002
103200940715     C                   MOVE      *BLANKS       KPJBU
103300940607     C                   MOVEL     D002DS        KPJBU
103400940608$004 C                   CALL      'ANGY4FR2'
103500940607     C                   PARM                    KPJBA
103600940607     C                   MOVEL     KPJBU         D002DS
103700940201     C*
103800940223     C* ritorno da PGM gestione
103900940223     C                   EXSR      GESRET
104000940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
1041009402252    C     *IN99         IFEQ      *ON
104200940223     C                   SETON                                        40
1043009402252-   C                   ENDIF
104400940131     C*
104500940506     C                   MOVE      *HIVAL        $LASTV
1046009402251-   C                   ENDSL
104700940225     C*
104800940131     C                   ENDSR
104900940223     C/EJECT
105000940223     C************************************************************
105100940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
105200940223     C************************************************************
105300940223     C     GESRET        BEGSR
105400940223     C*
105500940223     C* modo di ritorno
105600940223     C*
1057009402231    C                   SELECT
105800940314    >C* << questi modi di utilizzo dei valori di ritorno dal
105900940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
106000940314    >C*    proposte, normalmente sempre valide, ma modificabili
106100940314    >C*    per situazioni particolari >>
106200940223     C* 1 = F3
106300940314    >C     RET002        WHENEQ    '1'
106400940224     C                   MOVE      *ON           $ESCI
106500940223     C                   MOVE      *ON           $FINE
106600940223     C* 2 = F12
106700940314    >C     RET002        WHENEQ    '2'
106800940223     C                   MOVE      *ON           $ESCI
106900940223     C*
1070009402231-   C                   ENDSL
107100940223     C*
107200940223     C* operazione eseguite dal pgm chiamato
107300940223     C*
1074009402231    C                   SELECT
107500940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
107600940314    >C     OPR002        WHENEQ    '1'
107700940223     C                   MOVE      *ON           WINZS1
107800940223     C*
1079009402231-   C                   ENDSL
108000940223     C*
108100940223     C* funzione non eseguibile per errore :
108200940223     C*
1083009402231    C                   SELECT
108400940223     C* 1 = funzione richiamata chiusa in errore
108500940316    >C*  eventualmente gestire altri codici di errore
108600940314    >C     ERR002        WHENEQ    '1'
108700940223     C                   MOVE      *ON           $ESCI
108800940223     C                   SETON                                        5299
108900940223     C*
1090009402231-   C                   ENDSL
109100940223     C*
109200940223     C                   ENDSR
109300940223     C/EJECT
109400940131     C************************************************************
109500940131     C* OPERAZIONI INIZIALI
109600940131     C************************************************************
109700940131     C     *INZSR        BEGSR
109800940131     C*
109900940127     C* Reperimento parametri
110000940127     C*
110100940117     C     *ENTRY        PLIST
110200940117     C                   PARM                    KPJBA
110300940714     C*
110400940223     C* Reperimento opzioni
110500940127     C*
110600940128     C                   EXSR      REPOPZ
110700940506     C*
110800940506     C* Reperimento tasti di funzione
110900940506     C*
111000940506     C                   EXSR      REPKEY
111100940506     C*
111200940607$007 C                   MOVE      'Y4F'         XTBCOD
111300010406X0113C*
111400010406  "  C* reperisco le dimensioni della variabile del titolo
111500010406X0113C                   EVAL      LENTIT = %Size(C1Tit)
111600940127     C*
111700940117     C                   ENDSR
111800940225     C/EJECT
111900941103      ************************************************************
112000941103      * Reperimento dati società
112100941103      ************************************************************
112200941103     C     REPSOC        BEGSR
112300941103      *
112400970214     C                   CALLB     'XSOC'
112500941103     C                   PARM                    TIPXSC            6
112600941103     C                   PARM                    SOCXSC            3
112700941103     C                   PARM                    CDSXSC            9 0
112800941103     C                   PARM                    MODXSC            3
112900941103     C                   PARM      *BLANKS       RTNXSC            1
113000941103     C                   PARM                    XSOCDS
113100941103     C                   PARM                    KPJBA
113200941103      *
113300941103     C                   ENDSR
113400940223     C************************************************************
113500940223     C* INIZIALIZZAZIONE VARIABILI
113600940223     C************************************************************
113700940223     C     INZVAR        BEGSR
113800970926     C*
113900970926     C* Dati da PGM chiamante
114000970926     C*
114100970926     C                   MOVEL     KPJBU         XTABDS
114200951124     C*
114300951124     C* Reperimento codice lingua e codice azienda
114400951124     C*
114500951124     C                   MOVEL     'SOC001'      TIPXSC
114600951124     C                   EXSR      REPSOC
114700951124     C     RTNXSC        IFNE      '1'
114800951124     C                   MOVEL     XSOCDS        SOC001
114900951124     C                   ELSE
115000951124     C                   MOVE      *ON           $FINE
115100951124     C* fine programma
115200951124     C                   EXSR      ENDPGM
115300951124     C                   ENDIF
115400951124     C* Lingua e codice azienda
115500970708     C                   MOVE      XSCLIN        XtaLin
115600970708     C                   MOVE      XSCSOC        XtaAzi
115700940715     C*
115800940715     C                   RESET                   $FC2
115900940715     C*
116000940715$021 C                   CLOSE     ANGY4FD
116100940715$021 C                   OPEN      ANGY4FD
116200940506     C*
116300940506     C* Definizione delle opzioni utilizzabili a pgm
116400940506     C*
116500940506     C                   EXSR      DEFOPZ
116600940718     C* Pulizia campi e indicatori
116700940718     C                   CLEAR                   W1POS
116800940718     C                   MOVE      *ALL'0'       IN4049           10
116900940718     C                   MOVEA     IN4049        *IN(40)
117000940718     C                   CLEAR                   S1OPZ
117100940223     C* Variabili per gestione videate
117200940223     C*
117300940223     C                   MOVE      *OFF          $FINE
117400940223     C                   MOVE      *OFF          $INZS1
117500940223     C                   MOVE      *OFF          $EFILE
117600940223     C                   MOVE      *OFF          $ESCI
117700940223     C                   MOVE      *OFF          $RCDOK
117800940509     C                   MOVE      *HIVAL        $LASTV
117900940506     C                   Z-ADD     0             $ULKS1            3 0
118000940223     C*
118100940318     C                   MOVE      *ON           $INZS1
118200940223     C                   MOVE      'S1'          $GEST
118300940223     C*
118400940223     C* Variabili appoggio
118500940223     C*
118600940223     C                   Z-ADD     SFLPAG        WPAG
118700940223     C*
118800940223     C* Valorizzazione campi univoci testate
118900940223     C*
119000940607     C*                    CLEART1
119100940607     C*                    MOVELKNSIF     NOMSIF
119200940607     C*                    MOVELRAGUTE    NOMDIT
119300940607     C*                    MOVELDSPGM     SIGLA
119400940607     C*                    MOVELPROGR     NOMPGM
119500940223     C*
119600940608     c* posizione window
119700010406X0113c* modificata la seguente riga
119800010406$012 C                   Z-ADD     63            PCOL              2 0
119900940613     C                   Z-ADD     09            R
120000940608     C     80            SUB       PCOL          C
120100940608     C                   SUB       3             C
120200970708     C     XtaOpz        IFNE      '  '
120300970708     C     XtaRig        IFGT      11
120400940608     C                   Z-ADD     1             R
120500940608     C                   ENDIF
120600940608     C*
120700970708     C     XtaCol        IFGT      40
120800940608     C                   Z-ADD     2             C
120900940608     C                   ENDIF
121000940608     C                   ENDIF
121100940616     C*
121200970708     C     XtaOpz        IFNE      '01'
121300940616$019 C                   DO        00            I                 3 0
121400940721$022 C*                  MOVEL     LIN(I)        TBTFLD
121500940721$022 C*                  CLEAR                   TBTKEY
121600940721$022 C*                  CLEAR                   TBTAZI
121700940721$022 C*                  CLEAR                   TBTLIN
121800940721$007 C                   MOVE      'Y4F'         TBTCOD            3
121900940721$022 C*    K5TBT1        CHAIN     ANTBT01L                           21
122000940721$022 C*    *IN21         IFEQ      *ON
122100940721$022 C*                  Z-ADD     LLUN(I)       TBTLUN
122200940721$022 C*                  Z-ADD     LPOS(I)       TBTPOS
122300940721$022 C*                  MOVEL     LIND(I)       TBTDES
122400940721$022 C*                  WRITE     ANTBT000
122500940721$022 C*                  ENDIF
122600940616     C                   ENDDO
122700940616     C                   ENDIF
122800940608     C*
122900940722     c*
123000940722     c* Se richiamato con OPZ 11  Non controlla abilitazione all'aziend
123100970708     C     XtaOpz        IFEQ      '11'
123200940722     C                   MOVE      'N'           XTBGAB
123300940722     C                   ELSE
123400940722     C                   MOVE      'S'           XTBGAB
123500940722     C                   ENDIF
123600940223     C                   ENDSR
123700940223     C/EJECT
123800940131     C************************************************************
123900940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
124000940131     C************************************************************
124100940225     C     DEFOPZ        BEGSR
124200940131     C*
124300940225     C* Questa routine deve valorizzare la schiera $FL1
124400940225     C* con S o N a seconda della utilizzabilità o meno
124500940225     C* dell'opzione correlata
124600940225     C*
124700940714     C* Inizialmente si considerano tutte le opzioni come non valide
124800940728     C     1             DO        14            X
124900940714     C                   MOVE      'N'           $FL1(X)
125000940225     C                   END
125100940225     C*
125200940225     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
125300940714     C* rendo valide alcune opzioni
125400940714     C* i tasti invece li disattivo
125500940225     C                   SELECT
125600940714     C*
125700940714     C* Se pgm richiamato in RICERCA attivo :
125800940714     C*
125900970708    >C     XtaOpz        WHENEQ    '01'
126000940714     C* F3=Fine
126100940714     C                   Z-ADD     3             X
126200940720     C                   MOVE      '0'           $FC2(X)
126300940714     C* F10=Immissione
126400940714     C                   Z-ADD     10            X
126500940919     C                   MOVE      '1'           $FC2(X)
126600940714     C* 1=Selezione
126700940714     C                   Z-ADD     1             X
126800940714     C     01            LOOKUP    $OP1(X)                                21
126900940714     C     *IN21         IFEQ      *ON
127000940714     C                   MOVE      'S'           $FL1(X)
127100940714     C                   ENDIF
127200940729     C* 2=Modifica
127300940729     C                   Z-ADD     1             X
127400940729     C     02            LOOKUP    $OP1(X)                                21
127500940729     C     *IN21         IFEQ      *ON
127600940729     C                   MOVE      'S'           $FL1(X)
127700940729     C                   ENDIF
127800940729     C* 3=Copia
127900940729     C                   Z-ADD     1             X
128000940729     C     03            LOOKUP    $OP1(X)                                21
128100940729     C     *IN21         IFEQ      *ON
128200940729     C                   MOVE      'S'           $FL1(X)
128300940729     C                   ENDIF
128400940729     C* 4=Annullamento
128500940729     C                   Z-ADD     1             X
128600940729     C     04            LOOKUP    $OP1(X)                                21
128700940729     C     *IN21         IFEQ      *ON
128800940729     C                   MOVE      'S'           $FL1(X)
128900940729     C                   ENDIF
129000940714     C* 5=Visualizzazione
129100940714     C                   Z-ADD     1             X
129200940714     C     05            LOOKUP    $OP1(X)                                21
129300940714     C     *IN21         IFEQ      *ON
129400940714     C                   MOVE      'S'           $FL1(X)
129500940714     C                   ENDIF
129600940728     C*14=Commenti
129700940831     C**                   Z-ADD1         X
129800940831     C**         14        LOKUP$OP1,X                   21
129900940831     C**         *IN21     IFEQ *ON
130000940831     C**                   MOVE 'S'       $FL1,X
130100940831     C**                   ENDIF
130200940714     C*
130300940714     C* Se pgm richiamato in INTERROGAZIONE attivo :
130400940714     C*
130500970708    >C     XtaOpz        WHENEQ    '05'
130600940714     C* F10=Immissione
130700940714     C                   Z-ADD     10            X
130800940720     C                   MOVE      '0'           $FC2(X)
130900940714     C* F3=Fine
131000940714     C                   Z-ADD     3             X
131100940720     C                   MOVE      '0'           $FC2(X)
131200940714     C* 5=Visualizzazione
131300940225     C                   Z-ADD     1             X
131400940714     C     05            LOOKUP    $OP1(X)                                21
131500940225     C     *IN21         IFEQ      *ON
131600940714     C                   MOVE      'S'           $FL1(X)
131700940225     C                   ENDIF
131800940728     C*14=Commenti
131900940831     C**                   Z-ADD1         X
132000940831     C**         14        LOKUP$OP1,X                   21
132100940831     C**         *IN21     IFEQ *ON
132200940831     C**                   MOVE 'S'       $FL1,X
132300940831     C**                   ENDIF
132400940831     C*
132500940714     C* Se pgm richiamato in ABILITAZIONE attivo :
132600940714     C*
132700970708    >C     XtaOpz        WHENEQ    '11'
132800940714     C* F10=Immissione
132900940714     C                   Z-ADD     10            X
133000940720     C                   MOVE      '0'           $FC2(X)
133100940714     C* F3=Fine
133200940714     C                   Z-ADD     3             X
133300940720     C                   MOVE      '0'           $FC2(X)
133400940714     C* 5=Visualizzazione
133500940714     C                   Z-ADD     1             X
133600940714     C     05            LOOKUP    $OP1(X)                                21
133700940714     C     *IN21         IFEQ      *ON
133800940714     C                   MOVE      'S'           $FL1(X)
133900940714     C                   ENDIF
134000940714     C* 11=Abilitazione
134100940714     C                   Z-ADD     1             X
134200940714     C     11            LOOKUP    $OP1(X)                                21
134300940714     C     *IN21         IFEQ      *ON
134400940714     C                   MOVE      'S'           $FL1(X)
134500940714     C                   ENDIF
134600940714     C* 12=Disabilita
134700940714     C                   Z-ADD     1             X
134800940714     C     12            LOOKUP    $OP1(X)                                21
134900940714     C     *IN21         IFEQ      *ON
135000940714     C                   MOVE      'S'           $FL1(X)
135100940714     C                   ENDIF
135200940714     C*
135300940714     C* Se pgm richiamato in GESTIONE  attivo :
135400940714     C*
135500970708    >C     XtaOpz        WHENEQ    '02'
135600940714     C* F3=Fine
135700940714     C                   Z-ADD     3             X
135800940720     C                   MOVE      '0'           $FC2(X)
135900940714     C* 2=Modifica
136000940714     C                   Z-ADD     1             X
136100940714     C     02            LOOKUP    $OP1(X)                                21
136200940714     C     *IN21         IFEQ      *ON
136300940714     C                   MOVE      'S'           $FL1(X)
136400940714     C                   ENDIF
136500940714     C* 3=Copia
136600940714     C                   Z-ADD     1             X
136700940714     C     03            LOOKUP    $OP1(X)                                21
136800940714     C     *IN21         IFEQ      *ON
136900940714     C                   MOVE      'S'           $FL1(X)
137000940714     C                   ENDIF
137100940714     C* 4=Annullamento
137200940714     C                   Z-ADD     1             X
137300940714     C     04            LOOKUP    $OP1(X)                                21
137400940714     C     *IN21         IFEQ      *ON
137500940714     C                   MOVE      'S'           $FL1(X)
137600940714     C                   ENDIF
137700940714     C* 5=Visualizzazione
137800940714     C                   Z-ADD     1             X
137900940714     C     05            LOOKUP    $OP1(X)                                21
138000940714     C     *IN21         IFEQ      *ON
138100940714     C                   MOVE      'S'           $FL1(X)
138200940714     C                   ENDIF
138300940729     C*14=Commenti
138400940831     C**                   Z-ADD1         X
138500940831     C**         14        LOKUP$OP1,X                   21
138600940831     C**         *IN21     IFEQ *ON
138700940831     C**                   MOVE 'S'       $FL1,X
138800940831     C**                   ENDIF
138900940225     C*
139000940225     C                   OTHER
139100940714     C* Se pgm non richiamato, attivo TUTTO  :
139200940714     C* 2=Modifica
139300940714     C                   Z-ADD     1             X
139400940714     C     02            LOOKUP    $OP1(X)                                21
139500940714     C     *IN21         IFEQ      *ON
139600940714     C                   MOVE      'S'           $FL1(X)
139700940714     C                   ENDIF
139800940714     C* 3=Copia
139900940714     C                   Z-ADD     1             X
140000940714     C     03            LOOKUP    $OP1(X)                                21
140100940714     C     *IN21         IFEQ      *ON
140200940714     C                   MOVE      'S'           $FL1(X)
140300940714     C                   ENDIF
140400940714     C* 4=Annullamento
140500940714     C                   Z-ADD     1             X
140600940714     C     04            LOOKUP    $OP1(X)                                21
140700940714     C     *IN21         IFEQ      *ON
140800940714     C                   MOVE      'S'           $FL1(X)
140900940714     C                   ENDIF
141000940714     C* 5=Visualizzazione
141100940714     C                   Z-ADD     1             X
141200940714     C     05            LOOKUP    $OP1(X)                                21
141300940714     C     *IN21         IFEQ      *ON
141400940714     C                   MOVE      'S'           $FL1(X)
141500940714     C                   ENDIF
141600940714     C* 11=Abilitazione
141700940714     C                   Z-ADD     1             X
141800940714     C     11            LOOKUP    $OP1(X)                                21
141900940714     C     *IN21         IFEQ      *ON
142000940714     C                   MOVE      'S'           $FL1(X)
142100940714     C                   ENDIF
142200940714     C* 12=Disabilita
142300940714     C                   Z-ADD     1             X
142400940714     C     12            LOOKUP    $OP1(X)                                21
142500940714     C     *IN21         IFEQ      *ON
142600940714     C                   MOVE      'S'           $FL1(X)
142700940714     C                   ENDIF
142800940728     C*14=Commenti
142900940831     C**                   Z-ADD1         X
143000940831     C**         14        LOKUP$OP1,X                   21
143100940831     C**         *IN21     IFEQ *ON
143200940831     C**                   MOVE 'S'       $FL1,X
143300940831     C**                   ENDIF
143400940225     C* poi, disattivo a seconda delle autorizzazioni :
143500940314    >C*********  AUTXXX    IFEQ 'N'
143600940314    >C*********            Z-ADD1         X
143700940314    >C*********  'XX'      LOKUP$OP1,X                   21
143800940314    >C*********  *IN21     IFEQ *ON
143900940314    >C*********            MOVE 'N'       $FL1,X
144000940314    >C*********            ENDIF
144100940314    >C*********            ENDIF
144200940225     C*
144300940225     C                   ENDSL
144400940225     C*
144500940225     C                   ENDSR
144600940225     C/EJECT
144700940225     C************************************************************
144800940225     C* REPERIMENTO OPZIONI
144900940225     C************************************************************
145000940225     C     REPOPZ        BEGSR
145100940225     C*
145200940314     C     1             DO        $DIMOP        X
145300940128     C*
145400940223     C     $MS1(X)       IFNE      *BLANK
145500940128     C*
145600940223     C                   MOVEL     $MS1(X)       IDMSG
145700970214     C                   CALLB     'XRTVM'
145800940201     C                   PARM                    IDMSG            27
145900940201     C                   PARM                    TXTMSG          100
146000940201     C                   PARM                    ERRMSG            1
146100940128     C*
146200940128     C     ERRMSG        IFNE      *ON
146300940223     C                   MOVEL     TXTMSG        $TX1(X)
146400940128     C                   ELSE
146500940223     C                   MOVEL     *ALL'?'       $TX1(X)
146600940128     C                   END
146700940128     C*
146800940128     C                   END
146900940128     C*
147000940128     C                   END
147100940128     C*
147200940128     C                   ENDSR
147300940506     C/EJECT
147400940506     C************************************************************
147500940506     C* REPERIMENTO TASTI DI FUNZIONE
147600940506     C************************************************************
147700940506     C     REPKEY        BEGSR
147800940506     C*
147900940506     C                   MOVEA     $FM2          $FM
148000940506     C                   EXSR      REPKKK
148100940506     C                   MOVEA     $FX           $FX2
148200940506     C                   ENDSR
148300940506     C/EJECT
148400940506     C************************************************************
148500940506     C* REPERIMENTO TASTI DI FUNZIONE
148600940506     C************************************************************
148700940506     C     REPKKK        BEGSR
148800940506     C*
148900940506     C                   CLEAR                   $FX
149000940506     C     1             DO        24            X
149100940506     C*
149200940506     C     $FM(X)        IFNE      *BLANK
149300940506     C*
149400940506     C                   MOVEL     $FM(X)        IDMSG
149500970214     C                   CALLB     'XRTVM'
149600940506     C                   PARM                    IDMSG            27
149700940506     C                   PARM                    TXTMSG          100
149800940506     C                   PARM                    ERRMSG            1
149900940506     C*
150000940506     C     ERRMSG        IFNE      *ON
150100940506     C                   MOVEL     TXTMSG        $FX(X)
150200940506     C                   ELSE
150300940506     C                   MOVEL     *ALL' '       $FX(X)
150400940506     C                   END
150500940506     C*
150600940506     C                   END
150700940506     C*
150800940506     C                   END
150900940506     C*
151000940506     C                   ENDSR
151100940223     C/EJECT
151200940128     C************************************************************
151300940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
151400940128     C************************************************************
151500940223     C     DEFCAM        BEGSR
151600940128     C*
151700940223     C* Variabili per gestione videate
151800940223     C*
151900940223     C                   MOVE      *BLANK        $GEST             2
152000940223     C                   MOVE      *BLANK        $FINE             1
152100940223     C                   MOVE      *BLANK        $INZS1            1
152200940223     C                   MOVE      *BLANK        $LASTG            2
152300940223     C                   MOVE      *BLANK        $LASTV            2
152400940223     C                   MOVE      *BLANK        $EFILE            1
152500940223     C                   MOVE      *BLANK        $ESCI             1
152600940223     C                   MOVE      *BLANK        $RCDOK            1
152700940223     C*
152800940314     C* Variabili appoggio sempre presenti in tutte le anagrafiche
152900940223     C*
153000940223     C     *LIKE         DEFINE    C1RCD         S1NRR
153100940223     C     *LIKE         DEFINE    C1NRR         WSFL
153200940223     C     *LIKE         DEFINE    C1RCD         WMAX
153300940223     C     *LIKE         DEFINE    C1RCD         WPAG
153400940223     C     *LIKE         DEFINE    $INZS1        WINZS1
153500940223     C                   MOVE      *BLANK        WWGRP             3
153600940223     C                   Z-ADD     0             CURR              2 0
153700940223     C                   Z-ADD     0             CURC              2 0
153800940223     C                   MOVE      *BLANK        WIN              99
153900940314     C     *LIKE         DEFINE    C1POS         W1POS
154000940620     C*
154100940721$022 C*    K5TBT1        KLIST
154200940721$022 C*                  KFLD                    TBTAZI
154300940721$022 C*                  KFLD                    TBTCOD
154400940721$022 C*                  KFLD                    TBTKEY
154500940721$022 C*                  KFLD                    TBTLIN
154600940721$022 C*                  KFLD                    TBTFLD
154700940714     C*
154800940714     C     KABL          KLIST
154900940714     C                   KFLD                    ABLCOD
155000940714     C                   KFLD                    ABLKEY
155100940714     C                   KFLD                    ABLAZI
155200940314     C*
155300940314     C*
155400940223     C* Indici
155500940223     C*
155600940223     C                   Z-ADD     0             X                 3 0
155700940223     C                   Z-ADD     0             Y                 3 0
155800940127     C*
155900940117     C                   ENDSR
156000940223     C/EJECT
156100940616**
156200940225PROMSG    *LIBL     OPZ0001
156300940225PROMSG    *LIBL     OPZ0002
156400940131PROMSG    *LIBL     OPZ0003
156500940131PROMSG    *LIBL     OPZ0004
156600940131PROMSG    *LIBL     OPZ0005
156700940224PROMSG    *LIBL     OPZ0006
156800940224PROMSG    *LIBL     OPZ0007
156900940224PROMSG    *LIBL     OPZ0008
157000940224PROMSG    *LIBL     OPZ0009
157100970716PROMSG    *LIBL     OPZ0010
157200970716PROMSG    *LIBL     OPZ0013
157300970716PROMSG    *LIBL     OPZ0012
157400970716PROMSG    *LIBL     OPZ0011
157500970716PROMSG    *LIBL     OPZ0014
157600940201**
15770094022501S      <Selezione
15780094022502S      <Modifica
15790094022503S      <Copia
15800094022504S      <Annullamento
15810094022505S      <Visualizzazione
15820094060706N      <prova
15830094060707N      <prova
15840094060708N      <prova
15850094060709N      <prova
15860094072910S      <Invia
15870094071411S      <Abilita gruppo
15880094071412S      <Disabilita
15890094072813N      <PROVA
15900094083114N      <Commenti
159100940506** TASTI DI FUNZIONE  S1
159200940506PROMSG    *LIBL     KEY0001  01
159300940506PROMSG    *LIBL     KEY0002  02
159400940506PROMSG    *LIBL     KEY0003  03
159500940506PROMSG    *LIBL     KEY0004  04
159600940506PROMSG    *LIBL     KEY0005  05
159700940506PROMSG    *LIBL     KEY0006  06
159800940506PROMSG    *LIBL     KEY0007  07
159900940506PROMSG    *LIBL     KEY0008  08
160000940506PROMSG    *LIBL     KEY0009  09
160100970716PROMSG    *LIBL     KEY0010  10
160200970716PROMSG    *LIBL     KEY0011  11
160300970716PROMSG    *LIBL     KEY0012  12
160400970716PROMSG    *LIBL     KEY0013  13
160500970716PROMSG    *LIBL     KEY0014  14
160600970716PROMSG    *LIBL     KEY0015  15
160700970716PROMSG    *LIBL     KEY0016  16
160800970716PROMSG    *LIBL     KEY0017  17
160900970716PROMSG    *LIBL     KEY0018  18
161000970716PROMSG    *LIBL     KEY0019  19
161100940506PROMSG    *LIBL     KEY0020  20
161200940506PROMSG    *LIBL     KEY0021  21
161300940506PROMSG    *LIBL     KEY0022  22
161400940506PROMSG    *LIBL     KEY0023  23
161500940506PROMSG    *LIBL     KEY0024  24
161600940506**  ABILITAZIONE S1
1617009410140N  01
1618009407200N  02
1619009407201N  03
1620009407200N  04
1621009407200N  05
1622009407200N  06
1623009407200N  07
1624009407200N  08
1625009407200N  09
1626009407201N  10
1627009407200N  11
1628009407201N  12
1629009407200N  13
1630009407200N  14
1631009407200N  15
1632009407200N  16
1633009407200N  17
1634009407200N  18
1635009407200N  19
1636009407200N  20
1637009407200N  21
1638009407200N  22
1639009505020N  23
1640009407200N  24
