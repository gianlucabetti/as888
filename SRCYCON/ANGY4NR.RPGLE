000100970214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200970214     H*PARMS CVTOPT(*DATETIME)
000300940211     H DECEDIT('0,') DATEDIT(*DMY.)
000400130412     H DFTACTGRP(*NO) BNDDIR('PJXBND')
000500940224      *----------*
000600940317    > * STD001R  *
000700940224      *----------*
000800940317    > *               TITOLO PROGRAMMA
000900940224      *          ------------------------------
001000940224      *
001100940224      *
001200940224      * DOCUMENTI MICROANALISI :
001300940224      *
001400940224      * NOTE TECNICHE :
001500940224      *
001600940224      *
001700940224      * PERSONALIZZAZIONI
001800940224      *
001900940224      *
002000940224      *
002100940314      *               MAPPA INDICATORI
002200940224      *
002300940307      *  21           GENERICO OPERAZIONI I/O
002400940224      *  22           GENERICO ERRORE OPERAZIONI I/O
002500940224      *  30           SFLDSP
002600940224      * N31           SFLCLR
002700940224      *  31           SFLDSPCTL
002800940224      *  32           SFLNXTCHG
002900940224      *  33           SFLEND
003000940224      *  39           OF PRTF
003100940224      *  40 <---> 49  DSPATR ERRORI SU SFL
003200940608      *  Specificare l'uso dei singoli indicatori
003300940224      *  50 <---> 98  ERRORI SU VIDEO
003400940608      *  Specificare l'uso dei singoli indicatori
003500940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
003600940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
003700940224      *  99           INDIC. GENERALE DI ERRORE
003800940128     F*----------------------------------------------------*
003900940721$022 F*NTBT01L  IF A E           K DISK
004000940715$001 FANGY4ND   CF   E             WORKSTN USROPN
004100940607     F                                     SFILE(S1:S1NRR)
004200940201     F                                     INFDS(DSFMT)
004300940714     FANABL01L  UF A E           K DISK
004400940128     D*----------------------------------------------------*
004500940302     D* Schiere per la gestione delle opzioni
004600940302     D*
004700950503    >D $MS1            S             27    DIM(14) CTDATA PERRCD(1)
004800950503    >D $TX1            S             50    DIM(14) ALT($MS1)
004900940728    >D $OP1            S              2  0 DIM(14) CTDATA PERRCD(1)
005000940728    >D $FL1            S              1    DIM(14) ALT($OP1)
005100950503     D $TX             S             50    DIM(50)
005200950503     D $FL             S              1    DIM(50)
005300940506     D* Schiere per la gestione dei tasti di funzione
005400940506     D $FM             S             27    DIM(24)
005500940506     D $FX             S             50    DIM(24)
005600940506     D $FC             S              1    DIM(24)
005700940506     D $FR             S              1    DIM(24)
005800940506     D* Schiere per la gestione dei tasti di funzione
005900940506     D* Video S1
006000940506     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
006100940506     D $FX2            S             50    DIM(24) ALT($FM2)
006200940506     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
006300940506     D $FR2            S              1    DIM(24) ALT($FC2)
006400940616     D* campi in lingua
006500940616$010 D*LIN             S              6    DIM(0011) CTDATA PERRCD(001)
006600940616$010 D*LIND            S             50    DIM(0011) ALT(LIN)
006700940616$010 D*LPOS            S              3  0 DIM(0011) CTDATA PERRCD(001)
006800940616$010 D*LLUN            S              3  0 DIM(0011) ALT(LPOS)
006900940211     D* Passaggio Parametri
007000940211     D KPJBA         E DS
007100940325     D*-------------
007200940620     D* DS Esterna autorizzazioni tabelle
007300940620     D XCHATB        E DS                  EXTNAME(XCHKATBDS)
007400940620     D*-------------
007500940620     D* DS  restituzione autorizzazioni
007600940620     D* IND=indice
007700940620     D* GRU=man. di gruppo
007800940620     D* AZI=man. azienda
007900941111     D AUTDS           DS
008000940620     D  MANIND                 1      1
008100940620     D  MANGRU                 2      2
008200940620     D  MANAZI                 3      3
008300940607     D* Richiamo a XATB
008400940607     D XATBDS        E DS
008500941103     D*-------------
008600941103      * DS Società
008700941103     D SOC001        E DS                  EXTNAME(XSOC001DS)
008800960123    >D  SALDOC       E                     EXTFLD(XSCSALDOC)
008900960123    >D  SALDOA       E                     EXTFLD(XSCSALDOA)
009000941103     D*-------------
009100941103      * DS Interna per dati utente
009200941103     D XSOCDS          DS          1000
009300940607     D*
009400940325     D* Parametri in ricezione
009500970708     D XTABDS        E DS
009600940325     D*-------------
009700940325     D* Parametri del PGM di manutenzione STD002R
009800940607    >D D002DS          DS
009900940607     D  OPZ002                 1      2
010000940607     D* 0 = ritorno normale
010100940607     D* 1 = F3
010200940607     D* 2 = F12
010300940607     D  RET002                 3      3
010400940607     D* operazione eseguite dal pgm chiamato :
010500940607     D* 0 = nessuna operazione
010600940607     D* 1 = eseguito aggiornamento
010700940607     D  OPR002                 4      4
010800940607     D* funzione non eseguibile per errore :
010900940607     D* 0 = nessun errore
011000940607     D* 1 = funzione richiamata chiusa in errore
011100940607     D* Aggiungere eventuali altri codici di errore
011200940607     D  ERR002                 5      5
011300940607     D* Azienda
011400940607     D  AZI002                 6      8
011500940607     D* Lingua
011600940607     D  LIN002                 9      9
011700940607     D* Codice scelto
011800940607     D  KEY002                10     24
011900970509     D* Autorizzazione
012000970509     D  AUT002                25     25
012100970509     D* Unita
012200970509     D  UNI002                26     33
012300940211     D*-------------
012400940211     D DSFMT           DS           512
012500940506     D  $TASTO               369    369
012600940211     D  NRG                  370    370
012700940211     D  NCL                  371    371
012800940211     D  SFLNRR               378    379B 0
012900940207     D*-------------
013000940207     D* Nome PGM a video
013100940207     D                 DS
013200940207     D  PROGR                  1     10
013300940207     D  ASTER1                 1      1    INZ('*')
013400940207     D  SIGLA                  2      9
013500940207     D  ASTER2                10     10    INZ('*')
013600940729     D*
013700940729     D*
013800940127     D*-------------
013900940127     D* Reperimento nome PGM
014000940127     D STATUS         SDS           333
014100940127     D  DSPGM            *PROC
014200940607     D* descrizione tabella
014300940607$003 D EXTDS         E DS                  EXTNAME(ANGY4NDS)
014400070129$003 D tibs34ds      E DS
014500070129     D §azute        E DS
014600070129     D                                     EXTNAME(azute00f)
014700070129     D                                     DTAARA
014800070129     D §DatiUte      E DS
014900070129     D                                     EXTNAME(dDatiUte)
015000070129     D                                     DTAARA
015100940207     D*-------------
015200940211     D* COSTANTI
015300940211     D*-------------
015400940607     D SFLPAG          C                   CONST(10)
015500940728     D $DIMOP          C                   CONST(14)
015600940314     D* dimensione della schiera $MS1
015700940506     D*
015800940506     D* Tasti di funzione
015900940506     D F01             C                   CONST(X'31')
016000940506     D F02             C                   CONST(X'32')
016100940506     D F03             C                   CONST(X'33')
016200940506     D F04             C                   CONST(X'34')
016300940506     D F05             C                   CONST(X'35')
016400940506     D F06             C                   CONST(X'36')
016500940506     D F07             C                   CONST(X'37')
016600940506     D F08             C                   CONST(X'38')
016700940506     D F09             C                   CONST(X'39')
016800940506     D F10             C                   CONST(X'3A')
016900940506     D F11             C                   CONST(X'3B')
017000940506     D F12             C                   CONST(X'3C')
017100940506     D F13             C                   CONST(X'B1')
017200940506     D F14             C                   CONST(X'B2')
017300940506     D F15             C                   CONST(X'B3')
017400940506     D F16             C                   CONST(X'B4')
017500940506     D F17             C                   CONST(X'B5')
017600940506     D F18             C                   CONST(X'B6')
017700940506     D F19             C                   CONST(X'B7')
017800940506     D F20             C                   CONST(X'B8')
017900940506     D F21             C                   CONST(X'B9')
018000940506     D F22             C                   CONST(X'BA')
018100940506     D F23             C                   CONST(X'BB')
018200940506     D F24             C                   CONST(X'BC')
018300940506     D ENTER           C                   CONST(X'F1')
018400940506     D ROLDWN          C                   CONST(X'F4')
018500940506     D ROLLUP          C                   CONST(X'F5')
018600940302     I*
018700940211     I/EJECT
018800940207     I*-------------
018900940607     IS1
019000030828$015 I              S1DES                       §4NDES
019100030828$015 I              S1DE1                       §4NDE1
019200940207     I*-------------
019300940127     I*
019400940127     C*----------------------------------------------------*
019500940127     C*                MAIN LINE PROGRAM
019600940127     C*----------------------------------------------------*
019700940127     C*
019800940223     C* inizializzazione variabili
019900940223     C                   EXSR      INZVAR
020000940223     C*
020100940223     C     $FINE         DOWEQ     *OFF
020200940131     C     $GEST         CASEQ     'S1'          GESS1
020300940117     C                   END
020400940117     C                   END
020500940325     C*
020600940325     C* fine programma
020700940325     C                   EXSR      ENDPGM
020800940225     C*
020900940325     C/EJECT
021000940325     C************************************************************
021100940325     C* FINE PROGRAMMA
021200940325     C************************************************************
021300940325     C     ENDPGM        BEGSR
021400940325     C*
021500940325     C* passaggio dati al pgm chiamante
021600940715     C                   MOVE      *BLANKS       KPJBU
021700940607     C                   MOVEL     XTABDS        KPJBU
021800940325     C*
021900940325     C* Se pgm Guida non è stato richiamato posso chiudere in LR
022000970708     C     XtaOpz        IFEQ      *BLANK
022100940325     C                   SETON                                            LR
022200960906     C                   RETURN
022300940325     C* altrimenti chiudo in RT
022400940325     C                   ELSE
022500940325     C                   SETON                                            RT
022600960906     C                   RETURN
022700940325     C                   ENDIF
022800940325     C*
022900940325     C                   ENDSR
023000940131     C/EJECT
023100940224     C************************************************************
023200940131     C* RIEMPIMENTO OPZIONI
023300940224     C************************************************************
023400940131     C     RIEOPZ        BEGSR
023500940223     C*
023600940223     C* Input :
023700940223     C* - $TX = schiera con tutti i testi disponibili
023800940223     C* - $FL = schiere con i flag di validità associati ai testi
023900940225     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
024000940223     C*          opzione usato
024100940223     C* - WMXO = variabile contenente il numero massimo di testi
024200940223     C*          opzione usabili
024300940223     C* Output :
024400940223     C* - WOPZ1 = variabile contenente la prima riga opzioni
024500940223     C* - WOPZ2 = variabile contenente la seconda riga opzioni
024600950502     C* - FF23  = flag che indica se c'è bisogno del tasto F23
024700940223     C*
024800970214     C                   CALLB     'X04OPMSG'
024900940223     C* input
025000940223     C                   PARM                    $TX
025100940223     C                   PARM                    $FL
025200940225     C                   PARM                    WCOZ              3 0
025300940223     C                   PARM                    WMXO              3 0
025400940223     C* output
025500940223     C                   PARM                    WOPZ1            76
025600940223     C                   PARM                    WOPZ2            76
025700950502     C                   PARM                    FF23              1
025800940607     C* input
025900940607$018 C                   PARM      49            $L01              3 0
026000940607     C                   PARM      0             $L02              3 0
026100940223     C*
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
028900940607$018 C                   PARM      49            $L01              3 0
029000941004$018 C                   PARM      49            $L02              3 0
029100940506     C*
029200940506     C                   ENDSR
029300940224     C/EJECT
029400940224     C************************************************************
029500940224     C* GESTIONE EMISSIONE TASTI FUNZIONE
029600940224     C************************************************************
029700940224     C     GESPIE        BEGSR
029800940224     C*
029900940224     C                   SELECT
030000940506     C* Lista
030100940506     C     $GEST         WHENEQ    'S1'
030200940506     C     $ULKS1        IFEQ      0
030300940506     C                   EXSR      F24S1
030400940506     C                   ELSE
030500940607     C                   WRITE     Z1
030600940506     C                   ENDIF
030700940506     C                   ENDSL
030800940506     C*
030900940224     C                   ENDSR
031000940506     C/EJECT
031100940506     C************************************************************
031200940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA S1
031300940506     C************************************************************
031400940506     C     CTRKS1        BEGSR
031500940506     C                   SETOFF                                       99
031600940506     C                   MOVEA     $FC2          $FC
031700940506     C                   EXSR      CTRKEY
031800940506     C     *IN99         IFEQ      *ON
031900940506     C                   SETON                                        97
032000940506     C                   ENDIF
032100940607     C*
032200940506     C                   ENDSR
032300940506     C/EJECT
032400940506     C************************************************************
032500940506     C* CONTROLLO ABILITAZIONE TASTI
032600940509     C* INPUT
032700940509     c* $TASTO =Tasto funzione premuto
032800940509     C* $FC = Tasti abilitati
032900940509     C* OUTPUT
033000940509     C* IN99 = ERrore
033100940506     C************************************************************
033200940506     C     CTRKEY        BEGSR
033300940506     C                   SELECT
033400940506     C     $TASTO        WHENEQ    F01
033500940720     C     $FC(1)        IFEQ      '0'
033600940506     C                   SETON                                        99
033700940506     C                   ENDIF
033800940506     C     $TASTO        WHENEQ    F02
033900940720     C     $FC(2)        IFEQ      '0'
034000940506     C                   SETON                                        99
034100940506     C                   ENDIF
034200940506     C     $TASTO        WHENEQ    F03
034300940720     C     $FC(3)        IFEQ      '0'
034400940506     C                   SETON                                        99
034500940506     C                   ENDIF
034600940506     C     $TASTO        WHENEQ    F04
034700940720     C     $FC(4)        IFEQ      '0'
034800940506     C                   SETON                                        99
034900940506     C                   ENDIF
035000940506     C     $TASTO        WHENEQ    F05
035100940720     C     $FC(5)        IFEQ      '0'
035200940506     C                   SETON                                        99
035300940506     C                   ENDIF
035400940506     C     $TASTO        WHENEQ    F06
035500940720     C     $FC(6)        IFEQ      '0'
035600940506     C                   SETON                                        99
035700940506     C                   ENDIF
035800940506     C     $TASTO        WHENEQ    F07
035900940720     C     $FC(7)        IFEQ      '0'
036000940506     C                   SETON                                        99
036100940506     C                   ENDIF
036200940506     C     $TASTO        WHENEQ    F08
036300940720     C     $FC(8)        IFEQ      '0'
036400940506     C                   SETON                                        99
036500940506     C                   ENDIF
036600940506     C     $TASTO        WHENEQ    F09
036700940720     C     $FC(09)       IFEQ      '0'
036800940506     C                   SETON                                        99
036900940506     C                   ENDIF
037000940506     C     $TASTO        WHENEQ    F10
037100940720     C     $FC(10)       IFEQ      '0'
037200940506     C                   SETON                                        99
037300940506     C                   ENDIF
037400940506     C     $TASTO        WHENEQ    F11
037500940720     C     $FC(11)       IFEQ      '0'
037600940506     C                   SETON                                        99
037700940506     C                   ENDIF
037800940506     C     $TASTO        WHENEQ    F12
037900940720     C     $FC(12)       IFEQ      '0'
038000940506     C                   SETON                                        99
038100940506     C                   ENDIF
038200940506     C     $TASTO        WHENEQ    F13
038300940720     C     $FC(13)       IFEQ      '0'
038400940506     C                   SETON                                        99
038500940506     C                   ENDIF
038600940506     C     $TASTO        WHENEQ    F14
038700940720     C     $FC(14)       IFEQ      '0'
038800940506     C                   SETON                                        99
038900940506     C                   ENDIF
039000940506     C     $TASTO        WHENEQ    F15
039100940720     C     $FC(15)       IFEQ      '0'
039200940506     C                   SETON                                        99
039300940506     C                   ENDIF
039400940506     C     $TASTO        WHENEQ    F16
039500940720     C     $FC(16)       IFEQ      '0'
039600940506     C                   SETON                                        99
039700940506     C                   ENDIF
039800940506     C     $TASTO        WHENEQ    F17
039900940720     C     $FC(17)       IFEQ      '0'
040000940506     C                   SETON                                        99
040100940506     C                   ENDIF
040200940506     C     $TASTO        WHENEQ    F18
040300940720     C     $FC(18)       IFEQ      '0'
040400940506     C                   SETON                                        99
040500940506     C                   ENDIF
040600940506     C     $TASTO        WHENEQ    F19
040700940720     C     $FC(19)       IFEQ      '0'
040800940506     C                   SETON                                        99
040900940506     C                   ENDIF
041000940506     C     $TASTO        WHENEQ    F20
041100940720     C     $FC(20)       IFEQ      '0'
041200940506     C                   SETON                                        99
041300940506     C                   ENDIF
041400940506     C     $TASTO        WHENEQ    F21
041500940720     C     $FC(21)       IFEQ      '0'
041600940506     C                   SETON                                        99
041700940506     C                   ENDIF
041800940506     C     $TASTO        WHENEQ    F22
041900940720     C     $FC(22)       IFEQ      '0'
042000940506     C                   SETON                                        99
042100940506     C                   ENDIF
042200940506     C     $TASTO        WHENEQ    F23
042300940720     C     $FC(23)       IFEQ      '0'
042400940506     C                   SETON                                        99
042500940506     C                   ENDIF
042600940506     C     $TASTO        WHENEQ    F24
042700940720     C     $FC(24)       IFEQ      '0'
042800940506     C                   SETON                                        99
042900940506     C                   ENDIF
043000940506     C                   ENDSL
043100940506     C                   ENDSR
043200940224     C/EJECT
043300940127     C************************************************************
043400940131     C* GESTIONE LISTA
043500940127     C************************************************************
043600940127     C     GESS1         BEGSR
043700940223     C* inizializzazione videata
043800940223     C     $INZS1        IFEQ      *ON
043900940127     C                   EXSR      INZS1
044000940223     C                   MOVE      *OFF          $INZS1
044100940127     C                   ENDIF
044200940223     C* emissione piede videata se proveniente da altra
044300940223     C* salvataggio valore formato di provenienza
044400940202     C     $LASTV        IFNE      'S1'
044500940607!!   C*                    WRITEFMTT1
044600940224     C                   EXSR      GESPIE
044700940202     C                   MOVE      $LASTV        $LASTG
044800940202     C                   MOVE      'S1'          $LASTV
044900940202     C                   END
045000940223     C*
045100940223     C     WMAX          IFEQ      0
045200940607     C                   WRITE     D1
045300940211     C                   ENDIF
045400940729     C                   MOVE      '1'           WIN727            1
045500940127     C*
045600940607     C                   EXFMT     C1
045700940204     C     C1NRR         IFNE      0
045800940204     C                   Z-ADD     C1NRR         WSFL
045900940204     C                   ENDIF
046000940127     C                   Z-ADD     SFLNRR        C1RCD
046100940506     C*
046200940506     C* controllo abilitazione tasti funzione
046300940506     C*
046400940506     C                   EXSR      CTRKS1
046500940506     C     *IN99         IFEQ      *OFF
046600940506     C*
046700940127     C*
0468009401271    C                   SELECT
046900940127     C* F3=Fine
047000940506     C     $TASTO        WHENEQ    F03
047100940309     C                   EXSR      F03S1
047200940131     C* F10=Immissione
047300940506     C     $TASTO        WHENEQ    F10
047400940309     C                   EXSR      F10S1
047500940131     C* F12=Ritorno
047600940506     C     $TASTO        WHENEQ    F12
047700940309     C                   EXSR      F12S1
047800940224     C* F23=Altre opzioni
047900940506     C     $TASTO        WHENEQ    F23
048000940309     C                   EXSR      F23S1
048100940224     C* F24=Altri tasti
048200940506     C     $TASTO        WHENEQ    F24
048300940309     C                   EXSR      F24S1
048400940127     C* ROLL-UP
048500940506     C     $TASTO        WHENEQ    ROLLUP
048600940127     C                   EXSR      ROLS1
048700940127     C*
0488009401271O   C                   OTHER
048900940127     C* CONTROLLO DATI
049000940131     C                   EXSR      CTRC1
049100940201     C     *IN99         IFEQ      *OFF
049200940131     C                   EXSR      CTRS1
049300940131     C                   END
0494009401271-   C                   ENDSL
049500940127     C*
049600940506     C                   ENDIF
049700940127     C                   ENDSR
049800940224     C/EJECT
049900940127     C************************************************************
050000940131     C* INIZIALIZZAZIONE LISTA
050100940127     C************************************************************
050200940127     C     INZS1         BEGSR
050300940302     C* pulizia SFL
050400940128     C                   SETOFF                                         3031
050500940607     C                   WRITE     C1
050600940128     C                   SETON                                          31
050700940127     C*
050800940128     C* riempimento opzioni
050900940223     C                   MOVEA     $TX1          $TX
051000940223     C                   MOVEA     $FL1          $FL
051100940309     C                   Z-ADD     0             WCOZ
051200940225     C*  equivale al num.di elementi della schiera $TX1
051300940509     C                   Z-ADD     $DIMOP        WMXO
051400940128     C                   EXSR      RIEOPZ
051500950502     C                   Z-ADD     23            X
051600950502     C                   MOVE      FF23          $FC2(X)
051700940608     C                   MOVEL     WOPZ1         C1OPZ1
051800940608     C**                   MOVELWOPZ2     C1OPZ2
051900940225     C                   Z-ADD     WCOZ          C1COZ
052000940128     C*
052100940128     C* CARICAMENTO SFL A PAGINE
052200940128     C*
052300940201     C                   Z-ADD     0             S1NRR
052400940128     C                   Z-ADD     0             WMAX
052500940224     C*
052600940224     C* Posizionamento su file pilota
052700940302     C                   CLEAR                   C1POS
052800940316    >C                   EXSR      SETANA
052900940608    >C                   EXSR      REDANA
053000940302     C* ripristino i valori del posizionamento
053100940302     C                   MOVE      W1POS         C1POS
053200940131     C*
053300940131     C* Carico 1a pagina
053400940127     C                   EXSR      ROLS1
053500940201     C                   Z-ADD     SFLPAG        WPAG
053600940127     C*
053700940127     C                   ENDSR
053800940127     C/EJECT
053900940127     C************************************************************
054000940131     C* CARICAMENTO PAGINA LISTA
054100940127     C************************************************************
054200940127     C     ROLS1         BEGSR
054300940127     C*
054400940128     C                   SETOFF                                       32
054500940223     C                   Z-ADD     0             Y
054600940127     C                   Z-ADD     WMAX          S1NRR
054700940127     C*
054800940127     C* Leggo dal file anagrafico per caricare la lista
054900940127     C*
0550009401311    C     $EFILE        DOWEQ     *OFF
055100940201     C     Y             ANDLT     WPAG
055200070129     C*
055300940131     C                   EXSR      RIES1
055400070129     C*
055500070129     C                   IF        xtaOpz <> '01' OR (
055600070129     C                             (§4nSocUso = *BLANK OR §4nSocUso = xscSoc)
055700070129     C                             AND
055800070129     C                             (§4nLvlUte = *BLANK OR §4nLvlUte = dutLpo
055900070129     C                             OR (§4nLvlUte = 'P' AND (dutLpo = '1' OR
056000070129     C                             dutLpo = '2')))
056100070129     C                             )
056200940127     C*
056300940127     C                   ADD       1             S1NRR
056400940127     C                   ADD       1             Y
056500940714     C                   MOVE      $IN41         *IN41
056600940127     C*
056700940607     C                   WRITE     S1
056800940131     C*
056900070129     C                   ENDIF
057000070129     C*
057100940316    >C                   EXSR      REDANA
057200940128     C*
0573009401271-   C                   ENDDO
057400940127     C*
057500940223     C                   Z-ADD     S1NRR         WMAX                 30
057600940127     C*
057700940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
057800940127     C*
057900940201     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
058000940127     C                   MVR                     RESTO             3 0
058100940131     C     PAGINE        MULT      SFLPAG        C1RCD
0582009401271    C     RESTO         IFGT      0
058300940127     C                   ADD       1             C1RCD
0584009401271E   C                   ELSE
058500940407     C                   SUB       SFLPAG        C1RCD
058600940407     C                   ADD       1             C1RCD
0587009401271-   C                   ENDIF
058800940128     C*
058900940127     C                   ENDSR
059000940224     C/EJECT
059100940224     C************************************************************
059200940224     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
059300940224     C************************************************************
059400940607     C     SETANA        BEGSR
059500940224     C*
059600941004     C                   MOVE      *BLANKS       XTBKEY
059700970708     C                   MOVE      XtaAzi        XTBAZI
059800970708     C                   MOVE      XtaLin        XTBLIN
059900940608     C                   MOVE      '9'           XTBRIC
060000940607     C                   MOVE      '0'           XTBALC
060100950622     C                   MOVE      XSCCUT        XTBUTE
060200940607     C                   MOVE      '0'           XTBERR
060300941121     C**         W1POS     IFNE *BLANK
060400941004     C                   MOVE      *ZEROS        XTBKEY
060500980722     C                   MOVEl     W1POS         XTBKEY
060600941121     C**                   END
060700970214     C                   CALLB     'XATB'
060800940607     C                   PARM                    XATBDS
060900940607     C*
061000940607     C     XTBERR        IFNE      '0'
061100940607     C                   MOVEL     *ON           $EFILE
061200940607     C                   ENDIF
061300940224     C*
061400940224     C                   ENDSR
061500940128     C/EJECT
061600940128     C************************************************************
061700940131     C* LETTURA RCD ARCHIVIO PILOTA
061800940128     C************************************************************
061900940607     C     REDANA        BEGSR
062000940128     C*
062100940131     C                   MOVEL     *OFF          $EFILE
062200940131     C                   MOVEL     *OFF          $RCDOK
062300940131     C*
0624009401311    C     $EFILE        DOUEQ     *ON
062500940131     C     $RCDOK        OREQ      *ON
062600940131     C*
062700940607     C                   MOVE      '3'           XTBRIC
062800940607     C                   MOVE      '0'           XTBERR
062900970214     C                   CALLB     'XATB'
063000940607     C                   PARM                    XATBDS
063100940607     C*
063200940607     C     XTBERR        IFNE      '0'
063300940607     C                   MOVEL     *ON           $EFILE
063400940607     C                   ENDIF
063500940131     C*
0636009401312    C     $EFILE        IFEQ      *OFF
063700940607     C                   EXSR      SELANA
063800980722     C                   MOVEl     XTBKEY        SAVKEY
063900950502     C                   MOVE      XTBGRU        SAVGRU
064000950502     C                   MOVE      XTBUNI        SAVUNI
064100970509     C                   MOVE      XTBUNT        SAVUNT
0642009401312-   C                   ENDIF
064300940131     C*
0644009401311-   C                   ENDDO
064500940131     C*
064600940607     C                   MOVE      $EFILE        *IN33
064700940131     C*
064800940131     C                   ENDSR
064900940224     C/EJECT
065000940224     C************************************************************
065100940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
065200940224     C************************************************************
065300940224     C     CLCPAG        BEGSR
065400940224     C* Input :
065500940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
065600940224     C*          cursore
065700940224     C* - SFLPAG = numero rcd per pagina sfl
065800940224     C* Output :
065900940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
066000940224     C*
066100940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
066200940224     C                   MVR                     RESTO             3 0
066300940224     C     RESTO         IFGT      0
066400940224     C                   ADD       1             PAGINE
066500940224     C                   ENDIF
066600940224     C     PAGINE        MULT      SFLPAG        WPAG
066700940224     C*
066800940224     C                   ENDSR
066900940131     C/EJECT
067000940131     C************************************************************
067100940131     C* SELEZIONI SULLA RIGA LETTA
067200940314     C* impostare a *OFF il campo $RCDOK per scartare un record che
067300940314     C* non rientra nei parametri di parzializzazione
067400940314     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
067500940314     C* superiore stabilito nei parametri
067600940314     C*
067700940131     C************************************************************
067800940607     C     SELANA        BEGSR
067900940131     C*
068000940202     C                   MOVEL     *ON           $RCDOK
068100940714     C* Se aperto il pgm con opzione 11
068200940714     C* carico solo i record di gruppo
068300970708     C     XtaOpz        IFEQ      '11'
068400940714     C     XTBGRU        ANDNE     'G'
068500940714     C                   MOVEL     *OFF          $RCDOK
068600940714     C                   END
068700940714     C* Carica in alta intensità solo i record
068800940714     C* abilitati
068900940714     C                   MOVE      '0'           $IN41
069000970708     C     XtaOpz        IFEQ      '11'
069100940714     C     XTBGRU        ANDEQ     'G'
069200940715     C     XTBAZB        ANDNE     'N'
069300940714     C                   MOVE      '1'           $IN41
069400940714     C                   END
069500940714     C*
069600940202     C                   ENDSR
069700940131     C/EJECT
069800940131     C************************************************************
069900940131     C* RIEMPIMENTO RIGA LISTA
070000940131     C************************************************************
070100940131     C     RIES1         BEGSR
070200950502     C                   MOVEL     SAVUNI        EXTDS
070300980722     C                   MOVEl     SAVKEY        S1COD
070400950502     C                   MOVE      SAVGRU        S1GRU
070500970512     C                   MOVE      SAVUNT        S1UNITA           8
070600950502     C     *LIKE         DEFINE    XTBUNI        SAVUNI
070700950502     C     *LIKE         DEFINE    XTBKEY        SAVKEY
070800950502     C     *LIKE         DEFINE    XTBGRU        SAVGRU
070900970509     C     *LIKE         DEFINE    XTBUNT        SAVUNT
071000940131     C*
071100940131     C                   ENDSR
071200940309     C/EJECT
071300940309     C************************************************************
071400940309     C* GESTIONE F03 VIDEO S1
071500940309     C************************************************************
071600940309     C     F03S1         BEGSR
071700940309     C*
071800940309     C                   MOVE      *ON           $FINE
071900970708     C                   MOVE      '1'           XtaRet
072000940325     C* fine programma
072100940325     C                   EXSR      ENDPGM
072200940309     C*
072300940309     C                   ENDSR
072400940309     C/EJECT
072500940309     C************************************************************
072600940309     C* GESTIONE F10 VIDEO S1
072700940314     c* AGGIUNTA RECORD
072800940309     C************************************************************
072900940309     C     F10S1         BEGSR
073000940309     C*
073100940607     C                   RESET                   D002DS
073200940607     C                   MOVEL     '01'          OPZ002
073300970708     C                   MOVE      XtaLin        LIN002
073400970708     C                   MOVE      XtaAzi        AZI002
073500980722     C                   MOVEl     S1UNITA       UNI002
073600940715     C                   MOVE      *BLANKS       KPJBU
073700940607     C                   MOVEL     D002DS        KPJBU
073800940608$004 C                   CALL      'ANGY4NR2'
073900940309     C                   PARM                    KPJBA
074000940607     C                   MOVEL     KPJBU         D002DS
074100940309     C* ritorno da PGM gestione
074200940309     C                   EXSR      GESRET
074300940309     C     WINZS1        IFEQ      *ON
074400940309     C                   MOVE      *ON           $INZS1
074500940309     C* se esistevano già righe sul sfl
074600940309     C* calcolo pagina a cui deve posizionarsi
074700940309     C     WSFL          IFGT      0
074800940309     C                   EXSR      CLCPAG
074900940309     C* altrimenti carico solo la 1a pagina
075000940309     C                   ELSE
075100940309     C                   Z-ADD     SFLPAG        WPAG
075200940309     C                   ENDIF
075300940309     C                   ENDIF
075400940309     C*
075500940712     C                   MOVE      *HIVAL        $LASTV
075600940309     C                   ENDSR
075700940309     C/EJECT
075800940309     C************************************************************
075900940309     C* GESTIONE F12 VIDEO S1
076000940309     C************************************************************
076100940309     C     F12S1         BEGSR
076200940309     C*
076300970708     C                   MOVE      '2'           XtaRet
076400940309     C                   MOVE      *ON           $FINE
076500940309     C*
076600940309     C                   ENDSR
076700940309     C/EJECT
076800940309     C************************************************************
076900940309     C* GESTIONE F23 VIDEO S1
077000940309     C************************************************************
077100940309     C     F23S1         BEGSR
077200940309     C*
077300940309     C                   EXSR      RIEOPZ
077400950502     C                   Z-ADD     23            X
077500950502     C                   MOVE      FF23          $FC2(X)
077600940608     C                   MOVEL     WOPZ1         C1OPZ1
077700940608     C**                   MOVELWOPZ2     C1OPZ2
077800940309     C                   Z-ADD     WCOZ          C1COZ
077900940309     C*
078000940309     C                   ENDSR
078100940309     C/EJECT
078200940309     C************************************************************
078300940309     C* GESTIONE F24 VIDEO S1
078400940309     C************************************************************
078500940309     C     F24S1         BEGSR
078600940309     C*
078700940506     C*
078800940506     C                   MOVEA     $FX2          $FX
078900940506     C                   MOVEA     $FC2          $FC
079000940506     C                   MOVEA     $FR2          $FR
079100940506     C                   Z-ADD     $ULKS1        $ULK
079200940506     C                   EXSR      RIEKEY
079300941017     C                   MOVEA     $FC           $FC2
079400940506     C                   Z-ADD     $ULK          $ULKS1            3 0
079500940607     C                   MOVEL     $KEY1         Z1KE1
079600941004     C                   MOVEL     $KEY2         Z1KE2
079700940607     C                   WRITE     Z1
079800940506     C*
079900940309     C                   ENDSR
080000940128     C/EJECT
080100940128     C************************************************************
080200940131     C* CONTROLLO TESTATA LISTA
080300940128     C************************************************************
080400940131     C     CTRC1         BEGSR
080500940128     C*
080600940201     C                   MOVE      *OFF          *IN99
080700940131     C*
080800940202     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
080900940202     C* L'INIZIALIZZAZIONE LISTA
081000940202     C*
081100940207     C     C1POS         IFNE      W1POS
081200940202     C                   EXSR      POSS1
081300940202     C                   END
081400940202     C*
081500940202     C                   ENDSR
081600940202     C/EJECT
081700940202     C************************************************************
081800940202     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
081900940202     C************************************************************
082000940202     C     POSS1         BEGSR
082100940202     C*
082200940202     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
082300940202     C*
0824009402021    C     WMAX          IFGT      0
082500940607     C                   READC     S1                                     21
0826009402022    C     *IN21         DOWEQ     *OFF
082700940202     C     *IN99         ANDEQ     *OFF
082800940202     C*
0829009402023    C     S1OPZ         IFNE      0
083000940202     C                   SETON                                        51  99
0831009402023-   C                   ENDIF
083200940202     C* se rilevata opzione attiva, attivo sflnxtchg
0833009402023    C     *IN99         IFEQ      *ON
083400940202     C                   MOVE      *ON           *IN32
083500940714     C                   MOVE      $IN41         *IN41
083600940607     C                   UPDATE    S1
0837009402023E   C                   ELSE
083800940607     C                   READC     S1                                     21
0839009402023-   C                   ENDIF
084000940202     C*
0841009402022-   C                   ENDDO
084200940202     C*
0843009402021-   C                   ENDIF
084400940202     C*
084500940221     C*
0846009402021    C     *IN99         IFEQ      *OFF
084700941121     C                   CLEAR                   W1POS
084800940202     C*
084900940302     C                   MOVEL     C1POS         W1POS
085000940131     C*
085100940202     C                   MOVE      *ON           *IN99
085200940223     C                   MOVE      *ON           $INZS1
085300940202     C*
085400940202     C*
0855009402021-   C                   ENDIF
085600940202     C*
085700940131     C                   ENDSR
085800940131     C/EJECT
085900940131     C************************************************************
086000940131     C* CONTROLLO OPZIONI LISTA
086100940131     C************************************************************
086200940131     C     CTRS1         BEGSR
086300940131     C*
086400940202     C                   MOVEL     *OFF          $ESCI
086500940201     C                   SETOFF                                       99
086600940131     C                   Z-ADD     0             S1OPZ
086700940131     C*
086800940127     C* Leggo il sfl solo se ci sono rcd
0869009401311    C     WMAX          IFGT      0
087000940607     C                   READC     S1                                     21
087100940127     C*
087200940131     C* esce se fine sfl o errore che richiede l'uscita
0873009401312    C     *IN21         DOWEQ     *OFF
087400940131     C     $ESCI         ANDEQ     *OFF
087500940201     C                   Z-ADD     S1NRR         C1RCD
087600940131     C* ctrl su riga
087700940131     C                   EXSR      RECS1
087800940131     C* gestione opzioni
0879009401313    C     S1OPZ         IFNE      0
088000940201     C     *IN99         ANDEQ     *OFF
088100940131     C                   EXSR      OPZS1
0882009401273-   C                   ENDIF
088300940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0884009402013    C     *IN99         IFEQ      *ON
088500940201     C     $ESCI         OREQ      *ON
088600940131     C                   MOVE      *ON           *IN32
088700940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
088800940204     C* la ripristinerò a conclusione del ciclo di READC
088900940223     C                   MOVE      *OFF          $INZS1
0890009402233-   C                   ENDIF
089100940223     C*
089200940131     C                   Z-ADD     0             S1OPZ
089300940223     C*
089400940714     C                   MOVE      $IN41         *IN41
089500940607     C                   UPDATE    S1
089600940223     C*
089700940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0898009401313    C     $ESCI         IFEQ      *OFF
089900940607     C                   READC     S1                                     21
090000940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0901009402014    C     *IN21         IFEQ      *ON
090200940201     C                   MOVE      WINZS1        $INZS1
090300940204     C* calcolo pagina a cui deve posizionarsi
090400940224     C                   EXSR      CLCPAG
0905009402014-   C                   ENDIF
0906009402013-   C                   ENDIF
090700940131     C*
0908009401272-   C                   ENDDO
090900940127     C*
0910009401311-   C                   ENDIF
091100940131     C*
091200940127     C                   ENDSR
091300940127     C/EJECT
091400940127     C************************************************************
091500940131     C* CONTROLLO CAMPI I/O RIGA LISTA
091600940127     C************************************************************
091700940131     C     RECS1         BEGSR
091800940131     C*
091900940201     C* reset indicatori DSPATR
092000940201     C                   MOVE      *ALL'0'       IN4049           10
092100940201     C                   MOVEA     IN4049        *IN(40)
092200940201     C*
092300940131     C* Controllo se Utente Autorizzato alla scelta
092400940202     C     S1OPZ         IFNE      0
092500940131     C                   Z-ADD     1             X
092600940223     C     S1OPZ         LOOKUP    $OP1(X)                                21
092700940211     C     *IN21         IFEQ      *OFF
092800940223     C     $FL1(X)       OREQ      'N'
092900940131     C                   MOVE      *ON           $ESCI
093000940201     C                   SETON                                        504099
093100940131     C                   ENDIF
093200940131     C*
093300940620     c* Controllo autorizzazione alla modifica su record di gruppo
093400940620     C* Reperisco tipo codice (gruppo/azienda)
093500940620     C                   MOVE      *ALL'0'       XTBKEY
093600940620     C                   MOVE      S1COD         XTBKEY
093700970512     C                   MOVE      S1UNITA       XTBUNT
093800940620     C                   MOVE      '0'           XTBALC
093900940620     C                   MOVE      '1'           XTBRIC
094000970214     C                   CALLB     'XATB'
094100940620     C                   PARM                    XATBDS
094200940620     C* Reperisco autorizzazioni utente
094300940620     C                   CLEAR                   XCHATB
094400941118     C                   MOVE      XSCSOC        XCTSOC
094500941118     C                   MOVE      XSCDUN        XCTUNI
094600940620     C                   MOVEL     KNMUS         XCTPRF
094700940620     C                   MOVE      XTBCOD        XCTCOD
094800940620     C                   CALL      'XCHKATB'
094900940620     C                   PARM                    XCHATB
095000941111     C                   MOVEL     XCTDDS        AUTDS
095100941027     C     XCTRTN        IFEQ      1
095200941027     C                   MOVEL     'N'           MANGRU
095300941027     C                   ENDIF
095400940620     C* Non ammessa manutenzione di gruppo
095500940620   1 C     XTBGRU        IFEQ      'G'
095600940620     C     MANGRU        ANDEQ     'N'
095700940620   2 C     S1OPZ         IFEQ      2
095800940729     C     S1OPZ         OREQ      3
095900940620     C     S1OPZ         OREQ      4
096000940620     C                   SETON                                        5399
096100940620   2 C                   ENDIF
096200940620   1 C                   ENDIF
096300940620     C* Non ammessa manutenzione di azienda
096400940620   1 C     XTBGRU        IFEQ      'A'
096500940620     C     MANAZI        ANDEQ     'N'
096600940620   2 C     S1OPZ         IFEQ      2
096700940729     C     S1OPZ         OREQ      3
096800940620     C     S1OPZ         OREQ      4
096900940620     C                   SETON                                        5499
097000940620   2 C                   ENDIF
097100940620   1 C                   ENDIF
097200940607     C                   ENDIF
097300940131     C                   ENDSR
097400940224     C/EJECT
097500940131     C************************************************************
097600940131     C* GESTIONE OPZIONI LISTA
097700940131     C************************************************************
097800940131     C     OPZS1         BEGSR
097900940201     C*
0980009402251    C                   SELECT
098100940225     C*
098200940225     C* se opz=Selezione passo la chiave al pgm chiamante
098300940225     C     S1OPZ         WHENEQ    1
098400980722     C                   MOVE      S1OPZ         XtaOpz
098500980722     C                   MOVEl     S1COD         XtaKey
098600970708     C                   MOVE      S1UNIta       xtaUni
098700940225     C                   MOVE      *ON           $ESCI
098800940225     C                   MOVE      *ON           $FINE
098900940729     C*
099000940714     C*
099100940714     C* se opz=Abilita scrive su archivio abilitazioni
099200940714     C     S1OPZ         WHENEQ    11
099300941103     C                   MOVE      XSCSOC        ABLAZI
099400940714     C                   MOVE      *ZEROS        ABLKEY
099500980722     C                   MOVEl     S1COD         ABLKEY
099600970512     C                   MOVE      S1UNITA       ABLUNT
099700940714     C                   MOVE      XTBCOD        ABLCOD
099800940714     C                   WRITE     ANABL000                             22
099900940714     C*
100000940714     C* se opz=Disabil.cancella su archivio abilitazioni
100100940714     C     S1OPZ         WHENEQ    12
100200941103     C                   MOVE      XSCSOC        ABLAZI
100300940714     C                   MOVE      *ZEROS        ABLKEY
100400980722     C                   MOVEl     S1COD         ABLKEY
100500970512     C                   MOVE      S1UNITA       ABLUNT
100600940714     C                   MOVE      XTBCOD        ABLCOD
100700940714     C     KABL          DELETE    ANABL000                           22
100800940729     C*
100900940225     C*
101000940225     C* se altre opzioni (o nessuna opzione) richiamo pgm gestione
1011009402251O   C                   OTHER
101200940225     C*
101300940616     C                   RESET                   D002DS
101400940607     C                   MOVEL     S1OPZ         OPZ002
101500980722     C                   MOVE      *blanks       KEY002
101600980722     C                   MOVEl     S1COD         KEY002
101700970708     C                   MOVE      XtaLin        LIN002
101800970708     C                   MOVE      XtaAzi        AZI002
101900940715     C                   MOVE      *BLANKS       KPJBU
102000940607     C                   MOVEL     D002DS        KPJBU
102100940608$004 C                   CALL      'ANGY4NR2'
102200940607     C                   PARM                    KPJBA
102300940607     C                   MOVEL     KPJBU         D002DS
102400940201     C*
102500940223     C* ritorno da PGM gestione
102600940223     C                   EXSR      GESRET
102700940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
1028009402252    C     *IN99         IFEQ      *ON
102900940223     C                   SETON                                        40
1030009402252-   C                   ENDIF
103100940131     C*
103200940506     C                   MOVE      *HIVAL        $LASTV
1033009402251-   C                   ENDSL
103400940225     C*
103500940131     C                   ENDSR
103600940223     C/EJECT
103700940223     C************************************************************
103800940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
103900940223     C************************************************************
104000940223     C     GESRET        BEGSR
104100940223     C*
104200940223     C* modo di ritorno
104300940223     C*
1044009402231    C                   SELECT
104500940314    >C* << questi modi di utilizzo dei valori di ritorno dal
104600940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
104700940314    >C*    proposte, normalmente sempre valide, ma modificabili
104800940314    >C*    per situazioni particolari >>
104900940223     C* 1 = F3
105000940314    >C     RET002        WHENEQ    '1'
105100940224     C                   MOVE      *ON           $ESCI
105200940223     C                   MOVE      *ON           $FINE
105300940223     C* 2 = F12
105400940314    >C     RET002        WHENEQ    '2'
105500940223     C                   MOVE      *ON           $ESCI
105600940223     C*
1057009402231-   C                   ENDSL
105800940223     C*
105900940223     C* operazione eseguite dal pgm chiamato
106000940223     C*
1061009402231    C                   SELECT
106200940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
106300940314    >C     OPR002        WHENEQ    '1'
106400940223     C                   MOVE      *ON           WINZS1
106500940223     C*
1066009402231-   C                   ENDSL
106700940223     C*
106800940223     C* funzione non eseguibile per errore :
106900940223     C*
1070009402231    C                   SELECT
107100940223     C* 1 = funzione richiamata chiusa in errore
107200940316    >C*  eventualmente gestire altri codici di errore
107300940314    >C     ERR002        WHENEQ    '1'
107400940223     C                   MOVE      *ON           $ESCI
107500940223     C                   SETON                                        5299
107600940223     C*
1077009402231-   C                   ENDSL
107800940223     C*
107900940223     C                   ENDSR
108000940223     C/EJECT
108100940131     C************************************************************
108200940131     C* OPERAZIONI INIZIALI
108300940131     C************************************************************
108400940131     C     *INZSR        BEGSR
108500940131     C*
108600940127     C* Reperimento parametri
108700940127     C*
108800940117     C     *ENTRY        PLIST
108900940117     C                   PARM                    KPJBA
109000940714     C*
109100940714     C* Reperimento Codice utente
109200940714     C*
109300950622     C*          KNMUS     CHAINANUTN01L             21
109400940225     C*
109500940223     C* Reperimento opzioni
109600940127     C*
109700940128     C                   EXSR      REPOPZ
109800940506     C*
109900940506     C* Reperimento tasti di funzione
110000940506     C*
110100940506     C                   EXSR      REPKEY
110200940506     C*
110300940607$007 C                   MOVE      'Y4N'         XTBCOD
110400070129     C*
110500070129     C                   in(E)     *dtaara
110600070129     C                   IF        %Error  or  RSUT = *blanks
110700070213     C                   CLEAR                   Tibs34Ds
110800070213     C                   EVAL      i34Ute = kNmUs
110900070129     C                   call      'TIBS34R'
111000070129     C                   parm                    Tibs34Ds
111100070129     C                   in        *dtaara
111200070129     c                   ENDIF
111300940127     C*
111400940117     C                   ENDSR
111500940225     C/EJECT
111600941103      ************************************************************
111700941103      * Reperimento dati società
111800941103      ************************************************************
111900941103     C     REPSOC        BEGSR
112000941103      *
112100970214     C                   CALLB     'XSOC'
112200941103     C                   PARM                    TIPXSC            6
112300941103     C                   PARM                    SOCXSC            3
112400941103     C                   PARM                    CDSXSC            9 0
112500941103     C                   PARM                    MODXSC            3
112600941103     C                   PARM      *BLANKS       RTNXSC            1
112700941103     C                   PARM                    XSOCDS
112800941103     C                   PARM                    KPJBA
112900941103      *
113000941103     C                   ENDSR
113100940223     C************************************************************
113200940223     C* INIZIALIZZAZIONE VARIABILI
113300940223     C************************************************************
113400940223     C     INZVAR        BEGSR
113500970926     C*
113600970926     C* Dati da PGM chiamante
113700970926     C*
113800970926     C                   MOVEL     KPJBU         XTABDS
113900951124     C*
114000951124     C* Reperimento codice lingua e codice azienda
114100951124     C*
114200951124     C                   MOVEL     'SOC001'      TIPXSC
114300951124     C                   EXSR      REPSOC
114400951124     C     RTNXSC        IFNE      '1'
114500951124     C                   MOVEL     XSOCDS        SOC001
114600951124     C                   ELSE
114700951124     C                   MOVE      *ON           $FINE
114800951124     C* fine programma
114900951124     C                   EXSR      ENDPGM
115000951124     C                   ENDIF
115100951124     C* Lingua e codice azienda
115200970708     C                   MOVE      XSCLIN        XtaLin
115300970708     C                   MOVE      XSCSOC        XtaAzi
115400940715     C*
115500940715     C                   RESET                   $FC2
115600940715     C*
115700940715$021 C                   CLOSE     ANGY4ND
115800940715$021 C                   OPEN      ANGY4ND
115900940506     C*
116000940506     C* Definizione delle opzioni utilizzabili a pgm
116100940506     C*
116200940506     C                   EXSR      DEFOPZ
116300940718     C* Pulizia campi e indicatori
116400940718     C                   CLEAR                   W1POS
116500940718     C                   MOVE      *ALL'0'       IN4049           10
116600940718     C                   MOVEA     IN4049        *IN(40)
116700940718     C                   CLEAR                   S1OPZ
116800940223     C* Variabili per gestione videate
116900940223     C*
117000940223     C                   MOVE      *OFF          $FINE
117100940223     C                   MOVE      *OFF          $INZS1
117200940223     C                   MOVE      *OFF          $EFILE
117300940223     C                   MOVE      *OFF          $ESCI
117400940223     C                   MOVE      *OFF          $RCDOK
117500940509     C                   MOVE      *HIVAL        $LASTV
117600940506     C                   Z-ADD     0             $ULKS1            3 0
117700940223     C*
117800940318     C                   MOVE      *ON           $INZS1
117900940223     C                   MOVE      'S1'          $GEST
118000940223     C*
118100940223     C* Variabili appoggio
118200940223     C*
118300940223     C                   Z-ADD     SFLPAG        WPAG
118400940223     C*
118500940223     C* Valorizzazione campi univoci testate
118600940223     C*
118700940607     C*                    CLEART1
118800940607     C*                    MOVELKNSIF     NOMSIF
118900940607     C*                    MOVELRAGUTE    NOMDIT
119000940607     C*                    MOVELDSPGM     SIGLA
119100940607     C*                    MOVELPROGR     NOMPGM
119200940223     C*
119300940608     c* posizione window
119400980722$012 C                   Z-ADD     60            PCOL              2 0
119500940613     C                   Z-ADD     09            R
119600940608     C     80            SUB       PCOL          C
119700940608     C                   SUB       3             C
119800970708     C     XtaOpz        IFNE      '  '
119900970708     C     XtaRig        IFGT      11
120000940608     C                   Z-ADD     1             R
120100940608     C                   ENDIF
120200940608     C*
120300970708     C     XtaCol        IFGT      40
120400940608     C                   Z-ADD     2             C
120500940608     C                   ENDIF
120600940608     C                   ENDIF
120700940616     C*
120800970708     C     XtaOpz        IFNE      '01'
120900940616$019 C                   DO        00            I                 3 0
121000940721$022 C*                  MOVEL     LIN(I)        TBTFLD
121100940721$022 C*                  CLEAR                   TBTKEY
121200940721$022 C*                  CLEAR                   TBTAZI
121300940721$022 C*                  CLEAR                   TBTLIN
121400940721$007 C                   MOVE      'Y4N'         TBTCOD            3
121500940721$022 C*    K5TBT1        CHAIN     ANTBT01L                           21
121600940721$022 C*    *IN21         IFEQ      *ON
121700940721$022 C*                  Z-ADD     LLUN(I)       TBTLUN
121800940721$022 C*                  Z-ADD     LPOS(I)       TBTPOS
121900940721$022 C*                  MOVEL     LIND(I)       TBTDES
122000940721$022 C*                  WRITE     ANTBT000
122100940721$022 C*                  ENDIF
122200940616     C                   ENDDO
122300940616     C                   ENDIF
122400940608     C*
122500940722     c*
122600940722     c* Se richiamato con OPZ 11  Non controlla abilitazione all'aziend
122700970708     C     XtaOpz        IFEQ      '11'
122800940722     C                   MOVE      'N'           XTBGAB
122900940722     C                   ELSE
123000940722     C                   MOVE      'S'           XTBGAB
123100940722     C                   ENDIF
123200940223     C                   ENDSR
123300940223     C/EJECT
123400940131     C************************************************************
123500940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
123600940131     C************************************************************
123700940225     C     DEFOPZ        BEGSR
123800940131     C*
123900940225     C* Questa routine deve valorizzare la schiera $FL1
124000940225     C* con S o N a seconda della utilizzabilità o meno
124100940225     C* dell'opzione correlata
124200940225     C*
124300940714     C* Inizialmente si considerano tutte le opzioni come non valide
124400940728     C     1             DO        14            X
124500940714     C                   MOVE      'N'           $FL1(X)
124600940225     C                   END
124700940225     C*
124800940225     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
124900940714     C* rendo valide alcune opzioni
125000940714     C* i tasti invece li disattivo
125100940225     C                   SELECT
125200940714     C*
125300940714     C* Se pgm richiamato in RICERCA attivo :
125400940714     C*
125500970708    >C     XtaOpz        WHENEQ    '01'
125600940714     C* F3=Fine
125700940714     C                   Z-ADD     3             X
125800940720     C                   MOVE      '0'           $FC2(X)
125900940714     C* F10=Immissione
126000940714     C                   Z-ADD     10            X
126100940919     C                   MOVE      '1'           $FC2(X)
126200940714     C* 1=Selezione
126300940714     C                   Z-ADD     1             X
126400940714     C     01            LOOKUP    $OP1(X)                                21
126500940714     C     *IN21         IFEQ      *ON
126600940714     C                   MOVE      'S'           $FL1(X)
126700940714     C                   ENDIF
126800940729     C* 2=Modifica
126900940729     C                   Z-ADD     1             X
127000940729     C     02            LOOKUP    $OP1(X)                                21
127100940729     C     *IN21         IFEQ      *ON
127200940729     C                   MOVE      'S'           $FL1(X)
127300940729     C                   ENDIF
127400940729     C* 3=Copia
127500940729     C                   Z-ADD     1             X
127600940729     C     03            LOOKUP    $OP1(X)                                21
127700940729     C     *IN21         IFEQ      *ON
127800940729     C                   MOVE      'S'           $FL1(X)
127900940729     C                   ENDIF
128000940729     C* 4=Annullamento
128100940729     C                   Z-ADD     1             X
128200940729     C     04            LOOKUP    $OP1(X)                                21
128300940729     C     *IN21         IFEQ      *ON
128400940729     C                   MOVE      'S'           $FL1(X)
128500940729     C                   ENDIF
128600940714     C* 5=Visualizzazione
128700940714     C                   Z-ADD     1             X
128800940714     C     05            LOOKUP    $OP1(X)                                21
128900940714     C     *IN21         IFEQ      *ON
129000940714     C                   MOVE      'S'           $FL1(X)
129100940714     C                   ENDIF
129200940728     C*14=Commenti
129300940831     C**                   Z-ADD1         X
129400940831     C**         14        LOKUP$OP1,X                   21
129500940831     C**         *IN21     IFEQ *ON
129600940831     C**                   MOVE 'S'       $FL1,X
129700940831     C**                   ENDIF
129800940714     C*
129900940714     C* Se pgm richiamato in INTERROGAZIONE attivo :
130000940714     C*
130100970708    >C     XtaOpz        WHENEQ    '05'
130200940714     C* F10=Immissione
130300940714     C                   Z-ADD     10            X
130400940720     C                   MOVE      '0'           $FC2(X)
130500940714     C* F3=Fine
130600940714     C                   Z-ADD     3             X
130700940720     C                   MOVE      '0'           $FC2(X)
130800940714     C* 5=Visualizzazione
130900940225     C                   Z-ADD     1             X
131000940714     C     05            LOOKUP    $OP1(X)                                21
131100940225     C     *IN21         IFEQ      *ON
131200940714     C                   MOVE      'S'           $FL1(X)
131300940225     C                   ENDIF
131400940728     C*14=Commenti
131500940831     C**                   Z-ADD1         X
131600940831     C**         14        LOKUP$OP1,X                   21
131700940831     C**         *IN21     IFEQ *ON
131800940831     C**                   MOVE 'S'       $FL1,X
131900940831     C**                   ENDIF
132000940831     C*
132100940714     C* Se pgm richiamato in ABILITAZIONE attivo :
132200940714     C*
132300970708    >C     XtaOpz        WHENEQ    '11'
132400940714     C* F10=Immissione
132500940714     C                   Z-ADD     10            X
132600940720     C                   MOVE      '0'           $FC2(X)
132700940714     C* F3=Fine
132800940714     C                   Z-ADD     3             X
132900940720     C                   MOVE      '0'           $FC2(X)
133000940714     C* 5=Visualizzazione
133100940714     C                   Z-ADD     1             X
133200940714     C     05            LOOKUP    $OP1(X)                                21
133300940714     C     *IN21         IFEQ      *ON
133400940714     C                   MOVE      'S'           $FL1(X)
133500940714     C                   ENDIF
133600940714     C* 11=Abilitazione
133700940714     C                   Z-ADD     1             X
133800940714     C     11            LOOKUP    $OP1(X)                                21
133900940714     C     *IN21         IFEQ      *ON
134000940714     C                   MOVE      'S'           $FL1(X)
134100940714     C                   ENDIF
134200940714     C* 12=Disabilita
134300940714     C                   Z-ADD     1             X
134400940714     C     12            LOOKUP    $OP1(X)                                21
134500940714     C     *IN21         IFEQ      *ON
134600940714     C                   MOVE      'S'           $FL1(X)
134700940714     C                   ENDIF
134800940714     C*
134900940714     C* Se pgm richiamato in GESTIONE  attivo :
135000940714     C*
135100970708    >C     XtaOpz        WHENEQ    '02'
135200940714     C* F3=Fine
135300940714     C                   Z-ADD     3             X
135400940720     C                   MOVE      '0'           $FC2(X)
135500940714     C* 2=Modifica
135600940714     C                   Z-ADD     1             X
135700940714     C     02            LOOKUP    $OP1(X)                                21
135800940714     C     *IN21         IFEQ      *ON
135900940714     C                   MOVE      'S'           $FL1(X)
136000940714     C                   ENDIF
136100940714     C* 3=Copia
136200940714     C                   Z-ADD     1             X
136300940714     C     03            LOOKUP    $OP1(X)                                21
136400940714     C     *IN21         IFEQ      *ON
136500940714     C                   MOVE      'S'           $FL1(X)
136600940714     C                   ENDIF
136700940714     C* 4=Annullamento
136800940714     C                   Z-ADD     1             X
136900940714     C     04            LOOKUP    $OP1(X)                                21
137000940714     C     *IN21         IFEQ      *ON
137100940714     C                   MOVE      'S'           $FL1(X)
137200940714     C                   ENDIF
137300940714     C* 5=Visualizzazione
137400940714     C                   Z-ADD     1             X
137500940714     C     05            LOOKUP    $OP1(X)                                21
137600940714     C     *IN21         IFEQ      *ON
137700940714     C                   MOVE      'S'           $FL1(X)
137800940714     C                   ENDIF
137900940729     C*14=Commenti
138000940831     C**                   Z-ADD1         X
138100940831     C**         14        LOKUP$OP1,X                   21
138200940831     C**         *IN21     IFEQ *ON
138300940831     C**                   MOVE 'S'       $FL1,X
138400940831     C**                   ENDIF
138500940225     C*
138600940225     C                   OTHER
138700940714     C* Se pgm non richiamato, attivo TUTTO  :
138800940714     C* 2=Modifica
138900940714     C                   Z-ADD     1             X
139000940714     C     02            LOOKUP    $OP1(X)                                21
139100940714     C     *IN21         IFEQ      *ON
139200940714     C                   MOVE      'S'           $FL1(X)
139300940714     C                   ENDIF
139400940714     C* 3=Copia
139500940714     C                   Z-ADD     1             X
139600940714     C     03            LOOKUP    $OP1(X)                                21
139700940714     C     *IN21         IFEQ      *ON
139800940714     C                   MOVE      'S'           $FL1(X)
139900940714     C                   ENDIF
140000940714     C* 4=Annullamento
140100940714     C                   Z-ADD     1             X
140200940714     C     04            LOOKUP    $OP1(X)                                21
140300940714     C     *IN21         IFEQ      *ON
140400940714     C                   MOVE      'S'           $FL1(X)
140500940714     C                   ENDIF
140600940714     C* 5=Visualizzazione
140700940714     C                   Z-ADD     1             X
140800940714     C     05            LOOKUP    $OP1(X)                                21
140900940714     C     *IN21         IFEQ      *ON
141000940714     C                   MOVE      'S'           $FL1(X)
141100940714     C                   ENDIF
141200940714     C* 11=Abilitazione
141300940714     C                   Z-ADD     1             X
141400940714     C     11            LOOKUP    $OP1(X)                                21
141500940714     C     *IN21         IFEQ      *ON
141600940714     C                   MOVE      'S'           $FL1(X)
141700940714     C                   ENDIF
141800940714     C* 12=Disabilita
141900940714     C                   Z-ADD     1             X
142000940714     C     12            LOOKUP    $OP1(X)                                21
142100940714     C     *IN21         IFEQ      *ON
142200940714     C                   MOVE      'S'           $FL1(X)
142300940714     C                   ENDIF
142400940728     C*14=Commenti
142500940831     C**                   Z-ADD1         X
142600940831     C**         14        LOKUP$OP1,X                   21
142700940831     C**         *IN21     IFEQ *ON
142800940831     C**                   MOVE 'S'       $FL1,X
142900940831     C**                   ENDIF
143000940225     C* poi, disattivo a seconda delle autorizzazioni :
143100940314    >C*********  AUTXXX    IFEQ 'N'
143200940314    >C*********            Z-ADD1         X
143300940314    >C*********  'XX'      LOKUP$OP1,X                   21
143400940314    >C*********  *IN21     IFEQ *ON
143500940314    >C*********            MOVE 'N'       $FL1,X
143600940314    >C*********            ENDIF
143700940314    >C*********            ENDIF
143800940225     C*
143900940225     C                   ENDSL
144000940225     C*
144100940225     C                   ENDSR
144200940225     C/EJECT
144300940225     C************************************************************
144400940225     C* REPERIMENTO OPZIONI
144500940225     C************************************************************
144600940225     C     REPOPZ        BEGSR
144700940225     C*
144800940314     C     1             DO        $DIMOP        X
144900940128     C*
145000940223     C     $MS1(X)       IFNE      *BLANK
145100940128     C*
145200940223     C                   MOVEL     $MS1(X)       IDMSG
145300970214     C                   CALLB     'XRTVM'
145400940201     C                   PARM                    IDMSG            27
145500940201     C                   PARM                    TXTMSG          100
145600940201     C                   PARM                    ERRMSG            1
145700940128     C*
145800940128     C     ERRMSG        IFNE      *ON
145900940223     C                   MOVEL     TXTMSG        $TX1(X)
146000940128     C                   ELSE
146100940223     C                   MOVEL     *ALL'?'       $TX1(X)
146200940128     C                   END
146300940128     C*
146400940128     C                   END
146500940128     C*
146600940128     C                   END
146700940128     C*
146800940128     C                   ENDSR
146900940506     C/EJECT
147000940506     C************************************************************
147100940506     C* REPERIMENTO TASTI DI FUNZIONE
147200940506     C************************************************************
147300940506     C     REPKEY        BEGSR
147400940506     C*
147500940506     C                   MOVEA     $FM2          $FM
147600940506     C                   EXSR      REPKKK
147700940506     C                   MOVEA     $FX           $FX2
147800940506     C                   ENDSR
147900940506     C/EJECT
148000940506     C************************************************************
148100940506     C* REPERIMENTO TASTI DI FUNZIONE
148200940506     C************************************************************
148300940506     C     REPKKK        BEGSR
148400940506     C*
148500940506     C                   CLEAR                   $FX
148600940506     C     1             DO        24            X
148700940506     C*
148800940506     C     $FM(X)        IFNE      *BLANK
148900940506     C*
149000940506     C                   MOVEL     $FM(X)        IDMSG
149100970214     C                   CALLB     'XRTVM'
149200940506     C                   PARM                    IDMSG            27
149300940506     C                   PARM                    TXTMSG          100
149400940506     C                   PARM                    ERRMSG            1
149500940506     C*
149600940506     C     ERRMSG        IFNE      *ON
149700940506     C                   MOVEL     TXTMSG        $FX(X)
149800940506     C                   ELSE
149900940506     C                   MOVEL     *ALL' '       $FX(X)
150000940506     C                   END
150100940506     C*
150200940506     C                   END
150300940506     C*
150400940506     C                   END
150500940506     C*
150600940506     C                   ENDSR
150700940223     C/EJECT
150800940128     C************************************************************
150900940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
151000940128     C************************************************************
151100940223     C     DEFCAM        BEGSR
151200940128     C*
151300940223     C* Variabili per gestione videate
151400940223     C*
151500940223     C                   MOVE      *BLANK        $GEST             2
151600940223     C                   MOVE      *BLANK        $FINE             1
151700940223     C                   MOVE      *BLANK        $INZS1            1
151800940223     C                   MOVE      *BLANK        $LASTG            2
151900940223     C                   MOVE      *BLANK        $LASTV            2
152000940223     C                   MOVE      *BLANK        $EFILE            1
152100940223     C                   MOVE      *BLANK        $ESCI             1
152200940223     C                   MOVE      *BLANK        $RCDOK            1
152300940223     C*
152400940314     C* Variabili appoggio sempre presenti in tutte le anagrafiche
152500940223     C*
152600940223     C     *LIKE         DEFINE    C1RCD         S1NRR
152700940223     C     *LIKE         DEFINE    C1NRR         WSFL
152800940223     C     *LIKE         DEFINE    C1RCD         WMAX
152900940223     C     *LIKE         DEFINE    C1RCD         WPAG
153000940223     C     *LIKE         DEFINE    $INZS1        WINZS1
153100940223     C                   MOVE      *BLANK        WWGRP             3
153200940223     C                   Z-ADD     0             CURR              2 0
153300940223     C                   Z-ADD     0             CURC              2 0
153400940223     C                   MOVE      *BLANK        WIN              99
153500940314     C     *LIKE         DEFINE    C1POS         W1POS
153600940620     C*
153700940721$022 C*    K5TBT1        KLIST
153800940721$022 C*                  KFLD                    TBTAZI
153900940721$022 C*                  KFLD                    TBTCOD
154000940721$022 C*                  KFLD                    TBTKEY
154100940721$022 C*                  KFLD                    TBTLIN
154200940721$022 C*                  KFLD                    TBTFLD
154300940714     C*
154400940714     C     KABL          KLIST
154500940714     C                   KFLD                    ABLCOD
154600940714     C                   KFLD                    ABLKEY
154700940714     C                   KFLD                    ABLAZI
154800940314     C*
154900940314     C*
155000940223     C* Indici
155100940223     C*
155200940223     C                   Z-ADD     0             X                 3 0
155300940223     C                   Z-ADD     0             Y                 3 0
155400940127     C*
155500940117     C                   ENDSR
155600940223     C/EJECT
155700940616**
155800940225PROMSG    *LIBL     OPZ0001
155900940225PROMSG    *LIBL     OPZ0002
156000940131PROMSG    *LIBL     OPZ0003
156100940131PROMSG    *LIBL     OPZ0004
156200940131PROMSG    *LIBL     OPZ0005
156300940224PROMSG    *LIBL     OPZ0006
156400940224PROMSG    *LIBL     OPZ0007
156500940224PROMSG    *LIBL     OPZ0008
156600940224PROMSG    *LIBL     OPZ0009
156700970716PROMSG    *LIBL     OPZ0010
156800970716PROMSG    *LIBL     OPZ0013
156900970716PROMSG    *LIBL     OPZ0012
157000970716PROMSG    *LIBL     OPZ0011
157100970716PROMSG    *LIBL     OPZ0014
157200940201**
15730094022501S      <Selezione
15740094022502S      <Modifica
15750094022503S      <Copia
15760094022504S      <Annullamento
15770094022505S      <Visualizzazione
15780094060706N      <prova
15790094060707N      <prova
15800094060708N      <prova
15810094060709N      <prova
15820094072910S      <Invia
15830094071411S      <Abilita gruppo
15840094071412S      <Disabilita
15850094072813N      <PROVA
15860094083114N      <Commenti
158700940506** TASTI DI FUNZIONE  S1
158800940506PROMSG    *LIBL     KEY0001  01
158900940506PROMSG    *LIBL     KEY0002  02
159000940506PROMSG    *LIBL     KEY0003  03
159100940506PROMSG    *LIBL     KEY0004  04
159200940506PROMSG    *LIBL     KEY0005  05
159300940506PROMSG    *LIBL     KEY0006  06
159400940506PROMSG    *LIBL     KEY0007  07
159500940506PROMSG    *LIBL     KEY0008  08
159600940506PROMSG    *LIBL     KEY0009  09
159700970716PROMSG    *LIBL     KEY0010  10
159800970716PROMSG    *LIBL     KEY0011  11
159900970716PROMSG    *LIBL     KEY0012  12
160000970716PROMSG    *LIBL     KEY0013  13
160100970716PROMSG    *LIBL     KEY0014  14
160200970716PROMSG    *LIBL     KEY0015  15
160300970716PROMSG    *LIBL     KEY0016  16
160400970716PROMSG    *LIBL     KEY0017  17
160500970716PROMSG    *LIBL     KEY0018  18
160600970716PROMSG    *LIBL     KEY0019  19
160700940506PROMSG    *LIBL     KEY0020  20
160800940506PROMSG    *LIBL     KEY0021  21
160900940506PROMSG    *LIBL     KEY0022  22
161000940506PROMSG    *LIBL     KEY0023  23
161100940506PROMSG    *LIBL     KEY0024  24
161200940506**  ABILITAZIONE S1
1613009410140N  01
1614009407200N  02
1615009407201N  03
1616009407200N  04
1617009407200N  05
1618009407200N  06
1619009407200N  07
1620009407200N  08
1621009407200N  09
1622009407201N  10
1623009407200N  11
1624009407201N  12
1625009407200N  13
1626009407200N  14
1627009407200N  15
1628009407200N  16
1629009407200N  17
1630009407200N  18
1631009407200N  19
1632009407200N  20
1633009407200N  21
1634009407200N  22
1635009505020N  23
1636009407200N  24
