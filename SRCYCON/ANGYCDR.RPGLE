000100970214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
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
003900980921$001 FangYCDD   CF   E             WORKSTN USROPN
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
014200980921$003 D EXTDS         E DS                  EXTNAME(angYCDDS)
014300940207     D*-------------
014400940211     D* COSTANTI
014500940211     D*-------------
014600940607     D SFLPAG          C                   CONST(10)
014700940728     D $DIMOP          C                   CONST(14)
014800940314     D* dimensione della schiera $MS1
014900940506     D*
015000940506     D* Tasti di funzione
015100940506     D F01             C                   CONST(X'31')
015200940506     D F02             C                   CONST(X'32')
015300940506     D F03             C                   CONST(X'33')
015400940506     D F04             C                   CONST(X'34')
015500940506     D F05             C                   CONST(X'35')
015600940506     D F06             C                   CONST(X'36')
015700940506     D F07             C                   CONST(X'37')
015800940506     D F08             C                   CONST(X'38')
015900940506     D F09             C                   CONST(X'39')
016000940506     D F10             C                   CONST(X'3A')
016100940506     D F11             C                   CONST(X'3B')
016200940506     D F12             C                   CONST(X'3C')
016300940506     D F13             C                   CONST(X'B1')
016400940506     D F14             C                   CONST(X'B2')
016500940506     D F15             C                   CONST(X'B3')
016600940506     D F16             C                   CONST(X'B4')
016700940506     D F17             C                   CONST(X'B5')
016800940506     D F18             C                   CONST(X'B6')
016900940506     D F19             C                   CONST(X'B7')
017000940506     D F20             C                   CONST(X'B8')
017100940506     D F21             C                   CONST(X'B9')
017200940506     D F22             C                   CONST(X'BA')
017300940506     D F23             C                   CONST(X'BB')
017400940506     D F24             C                   CONST(X'BC')
017500940506     D ENTER           C                   CONST(X'F1')
017600940506     D ROLDWN          C                   CONST(X'F4')
017700940506     D ROLLUP          C                   CONST(X'F5')
017800940302     I*
017900940211     I/EJECT
018000940207     I*-------------
018100940607     IS1
018200980921$015 I              S1KC1                       §CDKC1
018300980921$015 I              S1KS1                       §CDKS1
018400980921$015 I              S1KC2                       §CDKC2
018500980921$015 I              S1KS2                       §CDKS2
018600980921$015 I              S1KC3                       §CDKC3
018700980921$015 I              S1KS3                       §CDKS3
018800980921$015 I              S1KC4                       §CDKC4
018900980921$015 I              S1KS4                       §CDKS4
019000980921$015 I              S1KS5                       §CDKS5
019100980921$015 I              S1KC5                       §CDKC5
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
025900940607$018 C                   PARM      70            $L01              3 0
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
028900940607$018 C                   PARM      70            $L01              3 0
029000941004$018 C                   PARM      70            $L02              3 0
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
055200940127     C*
055300940131     C                   EXSR      RIES1
055400940127     C*
055500940127     C                   ADD       1             S1NRR
055600940127     C                   ADD       1             Y
055700940714     C                   MOVE      $IN41         *IN41
055800940127     C*
055900940607     C                   WRITE     S1
056000940131     C*
056100940316    >C                   EXSR      REDANA
056200940128     C*
0563009401271-   C                   ENDDO
056400940127     C*
056500940223     C                   Z-ADD     S1NRR         WMAX                 30
056600940127     C*
056700940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
056800940127     C*
056900940201     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
057000940127     C                   MVR                     RESTO             3 0
057100940131     C     PAGINE        MULT      SFLPAG        C1RCD
0572009401271    C     RESTO         IFGT      0
057300940127     C                   ADD       1             C1RCD
0574009401271E   C                   ELSE
057500940407     C                   SUB       SFLPAG        C1RCD
057600940407     C                   ADD       1             C1RCD
0577009401271-   C                   ENDIF
057800940128     C*
057900940127     C                   ENDSR
058000940224     C/EJECT
058100940224     C************************************************************
058200940224     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
058300940224     C************************************************************
058400940607     C     SETANA        BEGSR
058500940224     C*
058600941004     C                   MOVE      *BLANKS       XTBKEY
058700970708     C                   MOVE      XtaAzi        XTBAZI
058800970708     C                   MOVE      XtaLin        XTBLIN
058900940608     C                   MOVE      '9'           XTBRIC
059000940607     C                   MOVE      '0'           XTBALC
059100950622     C                   MOVE      XSCCUT        XTBUTE
059200940607     C                   MOVE      '0'           XTBERR
059300941121     C**         W1POS     IFNE *BLANK
059400941004     C                   MOVE      *ZEROS        XTBKEY
059500940608     C                   MOVE      W1POS         XTBKEY
059600941121     C**                   END
059700970214     C                   CALLB     'XATB'
059800940607     C                   PARM                    XATBDS
059900940607     C*
060000940607     C     XTBERR        IFNE      '0'
060100940607     C                   MOVEL     *ON           $EFILE
060200940607     C                   ENDIF
060300940224     C*
060400940224     C                   ENDSR
060500940128     C/EJECT
060600940128     C************************************************************
060700940131     C* LETTURA RCD ARCHIVIO PILOTA
060800940128     C************************************************************
060900940607     C     REDANA        BEGSR
061000940128     C*
061100940131     C                   MOVEL     *OFF          $EFILE
061200940131     C                   MOVEL     *OFF          $RCDOK
061300940131     C*
0614009401311    C     $EFILE        DOUEQ     *ON
061500940131     C     $RCDOK        OREQ      *ON
061600940131     C*
061700940607     C                   MOVE      '3'           XTBRIC
061800940607     C                   MOVE      '0'           XTBERR
061900970214     C                   CALLB     'XATB'
062000940607     C                   PARM                    XATBDS
062100940607     C*
062200940607     C     XTBERR        IFNE      '0'
062300940607     C                   MOVEL     *ON           $EFILE
062400940607     C                   ENDIF
062500940131     C*
0626009401312    C     $EFILE        IFEQ      *OFF
062700940607     C                   EXSR      SELANA
062800950502     C                   MOVE      XTBKEY        SAVKEY
062900950502     C                   MOVE      XTBGRU        SAVGRU
063000950502     C                   MOVE      XTBUNI        SAVUNI
063100970509     C                   MOVE      XTBUNT        SAVUNT
0632009401312-   C                   ENDIF
063300940131     C*
0634009401311-   C                   ENDDO
063500940131     C*
063600940607     C                   MOVE      $EFILE        *IN33
063700940131     C*
063800940131     C                   ENDSR
063900940224     C/EJECT
064000940224     C************************************************************
064100940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
064200940224     C************************************************************
064300940224     C     CLCPAG        BEGSR
064400940224     C* Input :
064500940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
064600940224     C*          cursore
064700940224     C* - SFLPAG = numero rcd per pagina sfl
064800940224     C* Output :
064900940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
065000940224     C*
065100940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
065200940224     C                   MVR                     RESTO             3 0
065300940224     C     RESTO         IFGT      0
065400940224     C                   ADD       1             PAGINE
065500940224     C                   ENDIF
065600940224     C     PAGINE        MULT      SFLPAG        WPAG
065700940224     C*
065800940224     C                   ENDSR
065900940131     C/EJECT
066000940131     C************************************************************
066100940131     C* SELEZIONI SULLA RIGA LETTA
066200940314     C* impostare a *OFF il campo $RCDOK per scartare un record che
066300940314     C* non rientra nei parametri di parzializzazione
066400940314     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
066500940314     C* superiore stabilito nei parametri
066600940314     C*
066700940131     C************************************************************
066800940607     C     SELANA        BEGSR
066900940131     C*
067000940202     C                   MOVEL     *ON           $RCDOK
067100940714     C* Se aperto il pgm con opzione 11
067200940714     C* carico solo i record di gruppo
067300970708     C     XtaOpz        IFEQ      '11'
067400940714     C     XTBGRU        ANDNE     'G'
067500940714     C                   MOVEL     *OFF          $RCDOK
067600940714     C                   END
067700940714     C* Carica in alta intensità solo i record
067800940714     C* abilitati
067900940714     C                   MOVE      '0'           $IN41
068000970708     C     XtaOpz        IFEQ      '11'
068100940714     C     XTBGRU        ANDEQ     'G'
068200940715     C     XTBAZB        ANDNE     'N'
068300940714     C                   MOVE      '1'           $IN41
068400940714     C                   END
068500940714     C*
068600940202     C                   ENDSR
068700940131     C/EJECT
068800940131     C************************************************************
068900940131     C* RIEMPIMENTO RIGA LISTA
069000940131     C************************************************************
069100940131     C     RIES1         BEGSR
069200950502     C*                    MOVE XTBKEY    S1COD
069300950502     C*                    MOVE XTBGRU    S1GRU
069400950502     C                   MOVEL     SAVUNI        EXTDS
069500950502     C                   MOVE      SAVKEY        S1COD
069600950502     C                   MOVE      SAVGRU        S1GRU
069700970512     C                   MOVE      SAVUNT        S1UNITA           8
069800950502     C     *LIKE         DEFINE    XTBUNI        SAVUNI
069900950502     C     *LIKE         DEFINE    XTBKEY        SAVKEY
070000950502     C     *LIKE         DEFINE    XTBGRU        SAVGRU
070100970509     C     *LIKE         DEFINE    XTBUNT        SAVUNT
070200940131     C*
070300940131     C                   ENDSR
070400940309     C/EJECT
070500940309     C************************************************************
070600940309     C* GESTIONE F03 VIDEO S1
070700940309     C************************************************************
070800940309     C     F03S1         BEGSR
070900940309     C*
071000940309     C                   MOVE      *ON           $FINE
071100970708     C                   MOVE      '1'           XtaRet
071200940325     C* fine programma
071300940325     C                   EXSR      ENDPGM
071400940309     C*
071500940309     C                   ENDSR
071600940309     C/EJECT
071700940309     C************************************************************
071800940309     C* GESTIONE F10 VIDEO S1
071900940314     c* AGGIUNTA RECORD
072000940309     C************************************************************
072100940309     C     F10S1         BEGSR
072200940309     C*
072300940607     C                   RESET                   D002DS
072400940607     C                   MOVEL     '01'          OPZ002
072500970708     C                   MOVE      XtaLin        LIN002
072600970708     C                   MOVE      XtaAzi        AZI002
072700970512     C                   MOVE      S1UNITA       UNI002
072800940715     C                   MOVE      *BLANKS       KPJBU
072900940607     C                   MOVEL     D002DS        KPJBU
073000980921$004 C                   CALL      'ANGYCDR2'
073100940309     C                   PARM                    KPJBA
073200940607     C                   MOVEL     KPJBU         D002DS
073300940309     C* ritorno da PGM gestione
073400940309     C                   EXSR      GESRET
073500940309     C     WINZS1        IFEQ      *ON
073600940309     C                   MOVE      *ON           $INZS1
073700940309     C* se esistevano già righe sul sfl
073800940309     C* calcolo pagina a cui deve posizionarsi
073900940309     C     WSFL          IFGT      0
074000940309     C                   EXSR      CLCPAG
074100940309     C* altrimenti carico solo la 1a pagina
074200940309     C                   ELSE
074300940309     C                   Z-ADD     SFLPAG        WPAG
074400940309     C                   ENDIF
074500940309     C                   ENDIF
074600940309     C*
074700940712     C                   MOVE      *HIVAL        $LASTV
074800940309     C                   ENDSR
074900940309     C/EJECT
075000940309     C************************************************************
075100940309     C* GESTIONE F12 VIDEO S1
075200940309     C************************************************************
075300940309     C     F12S1         BEGSR
075400940309     C*
075500970708     C                   MOVE      '2'           XtaRet
075600940309     C                   MOVE      *ON           $FINE
075700940309     C*
075800940309     C                   ENDSR
075900940309     C/EJECT
076000940309     C************************************************************
076100940309     C* GESTIONE F23 VIDEO S1
076200940309     C************************************************************
076300940309     C     F23S1         BEGSR
076400940309     C*
076500940309     C                   EXSR      RIEOPZ
076600950502     C                   Z-ADD     23            X
076700950502     C                   MOVE      FF23          $FC2(X)
076800940608     C                   MOVEL     WOPZ1         C1OPZ1
076900940608     C**                   MOVELWOPZ2     C1OPZ2
077000940309     C                   Z-ADD     WCOZ          C1COZ
077100940309     C*
077200940309     C                   ENDSR
077300940309     C/EJECT
077400940309     C************************************************************
077500940309     C* GESTIONE F24 VIDEO S1
077600940309     C************************************************************
077700940309     C     F24S1         BEGSR
077800940309     C*
077900940506     C*
078000940506     C                   MOVEA     $FX2          $FX
078100940506     C                   MOVEA     $FC2          $FC
078200940506     C                   MOVEA     $FR2          $FR
078300940506     C                   Z-ADD     $ULKS1        $ULK
078400940506     C                   EXSR      RIEKEY
078500941017     C                   MOVEA     $FC           $FC2
078600940506     C                   Z-ADD     $ULK          $ULKS1            3 0
078700940607     C                   MOVEL     $KEY1         Z1KE1
078800941004     C                   MOVEL     $KEY2         Z1KE2
078900940607     C                   WRITE     Z1
079000940506     C*
079100940309     C                   ENDSR
079200940128     C/EJECT
079300940128     C************************************************************
079400940131     C* CONTROLLO TESTATA LISTA
079500940128     C************************************************************
079600940131     C     CTRC1         BEGSR
079700940128     C*
079800940201     C                   MOVE      *OFF          *IN99
079900940131     C*
080000940202     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
080100940202     C* L'INIZIALIZZAZIONE LISTA
080200940202     C*
080300940207     C     C1POS         IFNE      W1POS
080400940202     C                   EXSR      POSS1
080500940202     C                   END
080600940202     C*
080700940202     C                   ENDSR
080800940202     C/EJECT
080900940202     C************************************************************
081000940202     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
081100940202     C************************************************************
081200940202     C     POSS1         BEGSR
081300940202     C*
081400940202     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
081500940202     C*
0816009402021    C     WMAX          IFGT      0
081700940607     C                   READC     S1                                     21
0818009402022    C     *IN21         DOWEQ     *OFF
081900940202     C     *IN99         ANDEQ     *OFF
082000940202     C*
0821009402023    C     S1OPZ         IFNE      0
082200940202     C                   SETON                                        51  99
0823009402023-   C                   ENDIF
082400940202     C* se rilevata opzione attiva, attivo sflnxtchg
0825009402023    C     *IN99         IFEQ      *ON
082600940202     C                   MOVE      *ON           *IN32
082700940714     C                   MOVE      $IN41         *IN41
082800940607     C                   UPDATE    S1
0829009402023E   C                   ELSE
083000940607     C                   READC     S1                                     21
0831009402023-   C                   ENDIF
083200940202     C*
0833009402022-   C                   ENDDO
083400940202     C*
0835009402021-   C                   ENDIF
083600940202     C*
083700940221     C*
0838009402021    C     *IN99         IFEQ      *OFF
083900941121     C                   CLEAR                   W1POS
084000940202     C*
084100940302     C                   MOVEL     C1POS         W1POS
084200940131     C*
084300940202     C                   MOVE      *ON           *IN99
084400940223     C                   MOVE      *ON           $INZS1
084500940202     C*
084600940202     C*
0847009402021-   C                   ENDIF
084800940202     C*
084900940131     C                   ENDSR
085000940131     C/EJECT
085100940131     C************************************************************
085200940131     C* CONTROLLO OPZIONI LISTA
085300940131     C************************************************************
085400940131     C     CTRS1         BEGSR
085500940131     C*
085600940202     C                   MOVEL     *OFF          $ESCI
085700940201     C                   SETOFF                                       99
085800940131     C                   Z-ADD     0             S1OPZ
085900940131     C*
086000940127     C* Leggo il sfl solo se ci sono rcd
0861009401311    C     WMAX          IFGT      0
086200940607     C                   READC     S1                                     21
086300940127     C*
086400940131     C* esce se fine sfl o errore che richiede l'uscita
0865009401312    C     *IN21         DOWEQ     *OFF
086600940131     C     $ESCI         ANDEQ     *OFF
086700940201     C                   Z-ADD     S1NRR         C1RCD
086800940131     C* ctrl su riga
086900940131     C                   EXSR      RECS1
087000940131     C* gestione opzioni
0871009401313    C     S1OPZ         IFNE      0
087200940201     C     *IN99         ANDEQ     *OFF
087300940131     C                   EXSR      OPZS1
0874009401273-   C                   ENDIF
087500940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0876009402013    C     *IN99         IFEQ      *ON
087700940201     C     $ESCI         OREQ      *ON
087800940131     C                   MOVE      *ON           *IN32
087900940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
088000940204     C* la ripristinerò a conclusione del ciclo di READC
088100940223     C                   MOVE      *OFF          $INZS1
0882009402233-   C                   ENDIF
088300940223     C*
088400940131     C                   Z-ADD     0             S1OPZ
088500940223     C*
088600940714     C                   MOVE      $IN41         *IN41
088700940607     C                   UPDATE    S1
088800940223     C*
088900940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0890009401313    C     $ESCI         IFEQ      *OFF
089100940607     C                   READC     S1                                     21
089200940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0893009402014    C     *IN21         IFEQ      *ON
089400940201     C                   MOVE      WINZS1        $INZS1
089500940204     C* calcolo pagina a cui deve posizionarsi
089600940224     C                   EXSR      CLCPAG
0897009402014-   C                   ENDIF
0898009402013-   C                   ENDIF
089900940131     C*
0900009401272-   C                   ENDDO
090100940127     C*
0902009401311-   C                   ENDIF
090300940131     C*
090400940127     C                   ENDSR
090500940127     C/EJECT
090600940127     C************************************************************
090700940131     C* CONTROLLO CAMPI I/O RIGA LISTA
090800940127     C************************************************************
090900940131     C     RECS1         BEGSR
091000940131     C*
091100940201     C* reset indicatori DSPATR
091200940201     C                   MOVE      *ALL'0'       IN4049           10
091300940201     C                   MOVEA     IN4049        *IN(40)
091400940201     C*
091500940131     C* Controllo se Utente Autorizzato alla scelta
091600940202     C     S1OPZ         IFNE      0
091700940131     C                   Z-ADD     1             X
091800940223     C     S1OPZ         LOOKUP    $OP1(X)                                21
091900940211     C     *IN21         IFEQ      *OFF
092000940223     C     $FL1(X)       OREQ      'N'
092100940131     C                   MOVE      *ON           $ESCI
092200940201     C                   SETON                                        504099
092300940131     C                   ENDIF
092400940131     C*
092500940620     c* Controllo autorizzazione alla modifica su record di gruppo
092600940620     C* Reperisco tipo codice (gruppo/azienda)
092700940620     C                   MOVE      *ALL'0'       XTBKEY
092800940620     C                   MOVE      S1COD         XTBKEY
092900970512     C                   MOVE      S1UNITA       XTBUNT
093000940620     C                   MOVE      '0'           XTBALC
093100940620     C                   MOVE      '1'           XTBRIC
093200970214     C                   CALLB     'XATB'
093300940620     C                   PARM                    XATBDS
093400940620     C* Reperisco autorizzazioni utente
093500940620     C                   CLEAR                   XCHATB
093600941118     C                   MOVE      XSCSOC        XCTSOC
093700941118     C                   MOVE      XSCDUN        XCTUNI
093800940620     C                   MOVEL     KNMUS         XCTPRF
093900940620     C                   MOVE      XTBCOD        XCTCOD
094000940620     C                   CALL      'XCHKATB'
094100940620     C                   PARM                    XCHATB
094200941111     C                   MOVEL     XCTDDS        AUTDS
094300941027     C     XCTRTN        IFEQ      1
094400941027     C                   MOVEL     'N'           MANGRU
094500941027     C                   ENDIF
094600940620     C* Non ammessa manutenzione di gruppo
094700940620   1 C     XTBGRU        IFEQ      'G'
094800940620     C     MANGRU        ANDEQ     'N'
094900940620   2 C     S1OPZ         IFEQ      2
095000940729     C     S1OPZ         OREQ      3
095100940620     C     S1OPZ         OREQ      4
095200940620     C                   SETON                                        5399
095300940620   2 C                   ENDIF
095400940620   1 C                   ENDIF
095500940620     C* Non ammessa manutenzione di azienda
095600940620   1 C     XTBGRU        IFEQ      'A'
095700940620     C     MANAZI        ANDEQ     'N'
095800940620   2 C     S1OPZ         IFEQ      2
095900940729     C     S1OPZ         OREQ      3
096000940620     C     S1OPZ         OREQ      4
096100940620     C                   SETON                                        5499
096200940620   2 C                   ENDIF
096300940620   1 C                   ENDIF
096400940607     C                   ENDIF
096500940131     C                   ENDSR
096600940224     C/EJECT
096700940131     C************************************************************
096800940131     C* GESTIONE OPZIONI LISTA
096900940131     C************************************************************
097000940131     C     OPZS1         BEGSR
097100940201     C*
0972009402251    C                   SELECT
097300940225     C*
097400940225     C* se opz=Selezione passo la chiave al pgm chiamante
097500940225     C     S1OPZ         WHENEQ    1
097600970708     C                   MOVE      S1OPZ         XtaOpz
097700970708     C                   MOVE      S1COD         XtaKey
097800970708     C                   MOVE      S1UNIta       xtaUni
097900940225     C                   MOVE      *ON           $ESCI
098000940225     C                   MOVE      *ON           $FINE
098100940729     C*
098200940714     C*
098300940714     C* se opz=Abilita scrive su archivio abilitazioni
098400940714     C     S1OPZ         WHENEQ    11
098500941103     C                   MOVE      XSCSOC        ABLAZI
098600940714     C                   MOVE      *ZEROS        ABLKEY
098700940714     C                   MOVE      S1COD         ABLKEY
098800970512     C                   MOVE      S1UNITA       ABLUNT
098900940714     C                   MOVE      XTBCOD        ABLCOD
099000940714     C                   WRITE     ANABL000                             22
099100940714     C*
099200940714     C* se opz=Disabil.cancella su archivio abilitazioni
099300940714     C     S1OPZ         WHENEQ    12
099400941103     C                   MOVE      XSCSOC        ABLAZI
099500940714     C                   MOVE      *ZEROS        ABLKEY
099600940714     C                   MOVE      S1COD         ABLKEY
099700970512     C                   MOVE      S1UNITA       ABLUNT
099800940714     C                   MOVE      XTBCOD        ABLCOD
099900940714     C     KABL          DELETE    ANABL000                           22
100000940729     C*
100100940225     C*
100200940225     C* se altre opzioni (o nessuna opzione) richiamo pgm gestione
1003009402251O   C                   OTHER
100400940225     C*
100500940616     C                   RESET                   D002DS
100600940607     C                   MOVEL     S1OPZ         OPZ002
100700940616     C                   MOVE      *ZERO         KEY002
100800940607     C                   MOVE      S1COD         KEY002
100900970708     C                   MOVE      XtaLin        LIN002
101000970708     C                   MOVE      XtaAzi        AZI002
101100940715     C                   MOVE      *BLANKS       KPJBU
101200940607     C                   MOVEL     D002DS        KPJBU
101300980921$004 C                   CALL      'ANGYCDR2'
101400940607     C                   PARM                    KPJBA
101500940607     C                   MOVEL     KPJBU         D002DS
101600940201     C*
101700940223     C* ritorno da PGM gestione
101800940223     C                   EXSR      GESRET
101900940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
1020009402252    C     *IN99         IFEQ      *ON
102100940223     C                   SETON                                        40
1022009402252-   C                   ENDIF
102300940131     C*
102400940506     C                   MOVE      *HIVAL        $LASTV
1025009402251-   C                   ENDSL
102600940225     C*
102700940131     C                   ENDSR
102800940223     C/EJECT
102900940223     C************************************************************
103000940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
103100940223     C************************************************************
103200940223     C     GESRET        BEGSR
103300940223     C*
103400940223     C* modo di ritorno
103500940223     C*
1036009402231    C                   SELECT
103700940314    >C* << questi modi di utilizzo dei valori di ritorno dal
103800940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
103900940314    >C*    proposte, normalmente sempre valide, ma modificabili
104000940314    >C*    per situazioni particolari >>
104100940223     C* 1 = F3
104200940314    >C     RET002        WHENEQ    '1'
104300940224     C                   MOVE      *ON           $ESCI
104400940223     C                   MOVE      *ON           $FINE
104500940223     C* 2 = F12
104600940314    >C     RET002        WHENEQ    '2'
104700940223     C                   MOVE      *ON           $ESCI
104800940223     C*
1049009402231-   C                   ENDSL
105000940223     C*
105100940223     C* operazione eseguite dal pgm chiamato
105200940223     C*
1053009402231    C                   SELECT
105400940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
105500940314    >C     OPR002        WHENEQ    '1'
105600940223     C                   MOVE      *ON           WINZS1
105700940223     C*
1058009402231-   C                   ENDSL
105900940223     C*
106000940223     C* funzione non eseguibile per errore :
106100940223     C*
1062009402231    C                   SELECT
106300940223     C* 1 = funzione richiamata chiusa in errore
106400940316    >C*  eventualmente gestire altri codici di errore
106500940314    >C     ERR002        WHENEQ    '1'
106600940223     C                   MOVE      *ON           $ESCI
106700940223     C                   SETON                                        5299
106800940223     C*
1069009402231-   C                   ENDSL
107000940223     C*
107100940223     C                   ENDSR
107200940223     C/EJECT
107300940131     C************************************************************
107400940131     C* OPERAZIONI INIZIALI
107500940131     C************************************************************
107600940131     C     *INZSR        BEGSR
107700940131     C*
107800940127     C* Reperimento parametri
107900940127     C*
108000940117     C     *ENTRY        PLIST
108100940117     C                   PARM                    KPJBA
108200940714     C*
108300940714     C* Reperimento Codice utente
108400940714     C*
108500950622     C*          KNMUS     CHAINANUTN01L             21
108600940225     C*
108700940223     C* Reperimento opzioni
108800940127     C*
108900940128     C                   EXSR      REPOPZ
109000940506     C*
109100940506     C* Reperimento tasti di funzione
109200940506     C*
109300940506     C                   EXSR      REPKEY
109400940506     C*
109500940607$007 C                   MOVE      'YCD'         XTBCOD
109600940127     C*
109700940117     C                   ENDSR
109800940225     C/EJECT
109900941103      ************************************************************
110000941103      * Reperimento dati società
110100941103      ************************************************************
110200941103     C     REPSOC        BEGSR
110300941103      *
110400970214     C                   CALLB     'XSOC'
110500941103     C                   PARM                    TIPXSC            6
110600941103     C                   PARM                    SOCXSC            3
110700941103     C                   PARM                    CDSXSC            9 0
110800941103     C                   PARM                    MODXSC            3
110900941103     C                   PARM      *BLANKS       RTNXSC            1
111000941103     C                   PARM                    XSOCDS
111100941103     C                   PARM                    KPJBA
111200941103      *
111300941103     C                   ENDSR
111400940223     C************************************************************
111500940223     C* INIZIALIZZAZIONE VARIABILI
111600940223     C************************************************************
111700940223     C     INZVAR        BEGSR
111800970926     C*
111900970926     C* Dati da PGM chiamante
112000970926     C*
112100970926     C                   MOVEL     KPJBU         XTABDS
112200951124     C*
112300951124     C* Reperimento codice lingua e codice azienda
112400951124     C*
112500951124     C                   MOVEL     'SOC001'      TIPXSC
112600951124     C                   EXSR      REPSOC
112700951124     C     RTNXSC        IFNE      '1'
112800951124     C                   MOVEL     XSOCDS        SOC001
112900951124     C                   ELSE
113000951124     C                   MOVE      *ON           $FINE
113100951124     C* fine programma
113200951124     C                   EXSR      ENDPGM
113300951124     C                   ENDIF
113400951124     C* Lingua e codice azienda
113500970708     C                   MOVE      XSCLIN        XtaLin
113600970708     C                   MOVE      XSCSOC        XtaAzi
113700940715     C*
113800940715     C                   RESET                   $FC2
113900940715     C*
114000980921$021 C                   CLOSE     angYCDD
114100980921$021 C                   OPEN      angYCDD
114200940506     C*
114300940506     C* Definizione delle opzioni utilizzabili a pgm
114400940506     C*
114500940506     C                   EXSR      DEFOPZ
114600940718     C* Pulizia campi e indicatori
114700940718     C                   CLEAR                   W1POS
114800940718     C                   MOVE      *ALL'0'       IN4049           10
114900940718     C                   MOVEA     IN4049        *IN(40)
115000940718     C                   CLEAR                   S1OPZ
115100940223     C* Variabili per gestione videate
115200940223     C*
115300940223     C                   MOVE      *OFF          $FINE
115400940223     C                   MOVE      *OFF          $INZS1
115500940223     C                   MOVE      *OFF          $EFILE
115600940223     C                   MOVE      *OFF          $ESCI
115700940223     C                   MOVE      *OFF          $RCDOK
115800940509     C                   MOVE      *HIVAL        $LASTV
115900940506     C                   Z-ADD     0             $ULKS1            3 0
116000940223     C*
116100940318     C                   MOVE      *ON           $INZS1
116200940223     C                   MOVE      'S1'          $GEST
116300940223     C*
116400940223     C* Variabili appoggio
116500940223     C*
116600940223     C                   Z-ADD     SFLPAG        WPAG
116700940223     C*
116800940223     C* Valorizzazione campi univoci testate
116900940223     C*
117000940607     C*                    CLEART1
117100940607     C*                    MOVELKNSIF     NOMSIF
117200940607     C*                    MOVELRAGUTE    NOMDIT
117300940607     C*                    MOVELDSPGM     SIGLA
117400940607     C*                    MOVELPROGR     NOMPGM
117500940223     C*
117600940608     c* posizione window
117700940608$012 C                   Z-ADD     72            PCOL              2 0
117800940613     C                   Z-ADD     09            R
117900940608     C     80            SUB       PCOL          C
118000940608     C                   SUB       3             C
118100970708     C     XtaOpz        IFNE      '  '
118200970708     C     XtaRig        IFGT      11
118300940608     C                   Z-ADD     1             R
118400940608     C                   ENDIF
118500940608     C*
118600970708     C     XtaCol        IFGT      40
118700940608     C                   Z-ADD     2             C
118800940608     C                   ENDIF
118900940608     C                   ENDIF
119000940616     C*
119100970708     C     XtaOpz        IFNE      '01'
119200940616$019 C                   DO        00            I                 3 0
119300940721$022 C*                  MOVEL     LIN(I)        TBTFLD
119400940721$022 C*                  CLEAR                   TBTKEY
119500940721$022 C*                  CLEAR                   TBTAZI
119600940721$022 C*                  CLEAR                   TBTLIN
119700940721$007 C                   MOVE      'YCD'         TBTCOD            3
119800940721$022 C*    K5TBT1        CHAIN     ANTBT01L                           21
119900940721$022 C*    *IN21         IFEQ      *ON
120000940721$022 C*                  Z-ADD     LLUN(I)       TBTLUN
120100940721$022 C*                  Z-ADD     LPOS(I)       TBTPOS
120200940721$022 C*                  MOVEL     LIND(I)       TBTDES
120300940721$022 C*                  WRITE     ANTBT000
120400940721$022 C*                  ENDIF
120500940616     C                   ENDDO
120600940616     C                   ENDIF
120700940608     C*
120800940722     c*
120900940722     c* Se richiamato con OPZ 11  Non controlla abilitazione all'aziend
121000970708     C     XtaOpz        IFEQ      '11'
121100940722     C                   MOVE      'N'           XTBGAB
121200940722     C                   ELSE
121300940722     C                   MOVE      'S'           XTBGAB
121400940722     C                   ENDIF
121500940223     C                   ENDSR
121600940223     C/EJECT
121700940131     C************************************************************
121800940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
121900940131     C************************************************************
122000940225     C     DEFOPZ        BEGSR
122100940131     C*
122200940225     C* Questa routine deve valorizzare la schiera $FL1
122300940225     C* con S o N a seconda della utilizzabilità o meno
122400940225     C* dell'opzione correlata
122500940225     C*
122600940714     C* Inizialmente si considerano tutte le opzioni come non valide
122700940728     C     1             DO        14            X
122800940714     C                   MOVE      'N'           $FL1(X)
122900940225     C                   END
123000940225     C*
123100940225     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
123200940714     C* rendo valide alcune opzioni
123300940714     C* i tasti invece li disattivo
123400940225     C                   SELECT
123500940714     C*
123600940714     C* Se pgm richiamato in RICERCA attivo :
123700940714     C*
123800970708    >C     XtaOpz        WHENEQ    '01'
123900940714     C* F3=Fine
124000940714     C                   Z-ADD     3             X
124100940720     C                   MOVE      '0'           $FC2(X)
124200940714     C* F10=Immissione
124300940714     C                   Z-ADD     10            X
124400940919     C                   MOVE      '1'           $FC2(X)
124500940714     C* 1=Selezione
124600940714     C                   Z-ADD     1             X
124700940714     C     01            LOOKUP    $OP1(X)                                21
124800940714     C     *IN21         IFEQ      *ON
124900940714     C                   MOVE      'S'           $FL1(X)
125000940714     C                   ENDIF
125100940729     C* 2=Modifica
125200940729     C                   Z-ADD     1             X
125300940729     C     02            LOOKUP    $OP1(X)                                21
125400940729     C     *IN21         IFEQ      *ON
125500940729     C                   MOVE      'S'           $FL1(X)
125600940729     C                   ENDIF
125700940729     C* 3=Copia
125800940729     C                   Z-ADD     1             X
125900940729     C     03            LOOKUP    $OP1(X)                                21
126000940729     C     *IN21         IFEQ      *ON
126100940729     C                   MOVE      'S'           $FL1(X)
126200940729     C                   ENDIF
126300940729     C* 4=Annullamento
126400940729     C                   Z-ADD     1             X
126500940729     C     04            LOOKUP    $OP1(X)                                21
126600940729     C     *IN21         IFEQ      *ON
126700940729     C                   MOVE      'S'           $FL1(X)
126800940729     C                   ENDIF
126900940714     C* 5=Visualizzazione
127000940714     C                   Z-ADD     1             X
127100940714     C     05            LOOKUP    $OP1(X)                                21
127200940714     C     *IN21         IFEQ      *ON
127300940714     C                   MOVE      'S'           $FL1(X)
127400940714     C                   ENDIF
127500940728     C*14=Commenti
127600940831     C**                   Z-ADD1         X
127700940831     C**         14        LOKUP$OP1,X                   21
127800940831     C**         *IN21     IFEQ *ON
127900940831     C**                   MOVE 'S'       $FL1,X
128000940831     C**                   ENDIF
128100940714     C*
128200940714     C* Se pgm richiamato in INTERROGAZIONE attivo :
128300940714     C*
128400970708    >C     XtaOpz        WHENEQ    '05'
128500940714     C* F10=Immissione
128600940714     C                   Z-ADD     10            X
128700940720     C                   MOVE      '0'           $FC2(X)
128800940714     C* F3=Fine
128900940714     C                   Z-ADD     3             X
129000940720     C                   MOVE      '0'           $FC2(X)
129100940714     C* 5=Visualizzazione
129200940225     C                   Z-ADD     1             X
129300940714     C     05            LOOKUP    $OP1(X)                                21
129400940225     C     *IN21         IFEQ      *ON
129500940714     C                   MOVE      'S'           $FL1(X)
129600940225     C                   ENDIF
129700940728     C*14=Commenti
129800940831     C**                   Z-ADD1         X
129900940831     C**         14        LOKUP$OP1,X                   21
130000940831     C**         *IN21     IFEQ *ON
130100940831     C**                   MOVE 'S'       $FL1,X
130200940831     C**                   ENDIF
130300940831     C*
130400940714     C* Se pgm richiamato in ABILITAZIONE attivo :
130500940714     C*
130600970708    >C     XtaOpz        WHENEQ    '11'
130700940714     C* F10=Immissione
130800940714     C                   Z-ADD     10            X
130900940720     C                   MOVE      '0'           $FC2(X)
131000940714     C* F3=Fine
131100940714     C                   Z-ADD     3             X
131200940720     C                   MOVE      '0'           $FC2(X)
131300940714     C* 5=Visualizzazione
131400940714     C                   Z-ADD     1             X
131500940714     C     05            LOOKUP    $OP1(X)                                21
131600940714     C     *IN21         IFEQ      *ON
131700940714     C                   MOVE      'S'           $FL1(X)
131800940714     C                   ENDIF
131900940714     C* 11=Abilitazione
132000940714     C                   Z-ADD     1             X
132100940714     C     11            LOOKUP    $OP1(X)                                21
132200940714     C     *IN21         IFEQ      *ON
132300940714     C                   MOVE      'S'           $FL1(X)
132400940714     C                   ENDIF
132500940714     C* 12=Disabilita
132600940714     C                   Z-ADD     1             X
132700940714     C     12            LOOKUP    $OP1(X)                                21
132800940714     C     *IN21         IFEQ      *ON
132900940714     C                   MOVE      'S'           $FL1(X)
133000940714     C                   ENDIF
133100940714     C*
133200940714     C* Se pgm richiamato in GESTIONE  attivo :
133300940714     C*
133400970708    >C     XtaOpz        WHENEQ    '02'
133500940714     C* F3=Fine
133600940714     C                   Z-ADD     3             X
133700940720     C                   MOVE      '0'           $FC2(X)
133800940714     C* 2=Modifica
133900940714     C                   Z-ADD     1             X
134000940714     C     02            LOOKUP    $OP1(X)                                21
134100940714     C     *IN21         IFEQ      *ON
134200940714     C                   MOVE      'S'           $FL1(X)
134300940714     C                   ENDIF
134400940714     C* 3=Copia
134500940714     C                   Z-ADD     1             X
134600940714     C     03            LOOKUP    $OP1(X)                                21
134700940714     C     *IN21         IFEQ      *ON
134800940714     C                   MOVE      'S'           $FL1(X)
134900940714     C                   ENDIF
135000940714     C* 4=Annullamento
135100940714     C                   Z-ADD     1             X
135200940714     C     04            LOOKUP    $OP1(X)                                21
135300940714     C     *IN21         IFEQ      *ON
135400940714     C                   MOVE      'S'           $FL1(X)
135500940714     C                   ENDIF
135600940714     C* 5=Visualizzazione
135700940714     C                   Z-ADD     1             X
135800940714     C     05            LOOKUP    $OP1(X)                                21
135900940714     C     *IN21         IFEQ      *ON
136000940714     C                   MOVE      'S'           $FL1(X)
136100940714     C                   ENDIF
136200940729     C*14=Commenti
136300940831     C**                   Z-ADD1         X
136400940831     C**         14        LOKUP$OP1,X                   21
136500940831     C**         *IN21     IFEQ *ON
136600940831     C**                   MOVE 'S'       $FL1,X
136700940831     C**                   ENDIF
136800940225     C*
136900940225     C                   OTHER
137000940714     C* Se pgm non richiamato, attivo TUTTO  :
137100940714     C* 2=Modifica
137200940714     C                   Z-ADD     1             X
137300940714     C     02            LOOKUP    $OP1(X)                                21
137400940714     C     *IN21         IFEQ      *ON
137500940714     C                   MOVE      'S'           $FL1(X)
137600940714     C                   ENDIF
137700940714     C* 3=Copia
137800940714     C                   Z-ADD     1             X
137900940714     C     03            LOOKUP    $OP1(X)                                21
138000940714     C     *IN21         IFEQ      *ON
138100940714     C                   MOVE      'S'           $FL1(X)
138200940714     C                   ENDIF
138300940714     C* 4=Annullamento
138400940714     C                   Z-ADD     1             X
138500940714     C     04            LOOKUP    $OP1(X)                                21
138600940714     C     *IN21         IFEQ      *ON
138700940714     C                   MOVE      'S'           $FL1(X)
138800940714     C                   ENDIF
138900940714     C* 5=Visualizzazione
139000940714     C                   Z-ADD     1             X
139100940714     C     05            LOOKUP    $OP1(X)                                21
139200940714     C     *IN21         IFEQ      *ON
139300940714     C                   MOVE      'S'           $FL1(X)
139400940714     C                   ENDIF
139500940714     C* 11=Abilitazione
139600940714     C                   Z-ADD     1             X
139700940714     C     11            LOOKUP    $OP1(X)                                21
139800940714     C     *IN21         IFEQ      *ON
139900940714     C                   MOVE      'S'           $FL1(X)
140000940714     C                   ENDIF
140100940714     C* 12=Disabilita
140200940714     C                   Z-ADD     1             X
140300940714     C     12            LOOKUP    $OP1(X)                                21
140400940714     C     *IN21         IFEQ      *ON
140500940714     C                   MOVE      'S'           $FL1(X)
140600940714     C                   ENDIF
140700940728     C*14=Commenti
140800940831     C**                   Z-ADD1         X
140900940831     C**         14        LOKUP$OP1,X                   21
141000940831     C**         *IN21     IFEQ *ON
141100940831     C**                   MOVE 'S'       $FL1,X
141200940831     C**                   ENDIF
141300940225     C* poi, disattivo a seconda delle autorizzazioni :
141400940314    >C*********  AUTXXX    IFEQ 'N'
141500940314    >C*********            Z-ADD1         X
141600940314    >C*********  'XX'      LOKUP$OP1,X                   21
141700940314    >C*********  *IN21     IFEQ *ON
141800940314    >C*********            MOVE 'N'       $FL1,X
141900940314    >C*********            ENDIF
142000940314    >C*********            ENDIF
142100940225     C*
142200940225     C                   ENDSL
142300940225     C*
142400940225     C                   ENDSR
142500940225     C/EJECT
142600940225     C************************************************************
142700940225     C* REPERIMENTO OPZIONI
142800940225     C************************************************************
142900940225     C     REPOPZ        BEGSR
143000940225     C*
143100940314     C     1             DO        $DIMOP        X
143200940128     C*
143300940223     C     $MS1(X)       IFNE      *BLANK
143400940128     C*
143500940223     C                   MOVEL     $MS1(X)       IDMSG
143600970214     C                   CALLB     'XRTVM'
143700940201     C                   PARM                    IDMSG            27
143800940201     C                   PARM                    TXTMSG          100
143900940201     C                   PARM                    ERRMSG            1
144000940128     C*
144100940128     C     ERRMSG        IFNE      *ON
144200940223     C                   MOVEL     TXTMSG        $TX1(X)
144300940128     C                   ELSE
144400940223     C                   MOVEL     *ALL'?'       $TX1(X)
144500940128     C                   END
144600940128     C*
144700940128     C                   END
144800940128     C*
144900940128     C                   END
145000940128     C*
145100940128     C                   ENDSR
145200940506     C/EJECT
145300940506     C************************************************************
145400940506     C* REPERIMENTO TASTI DI FUNZIONE
145500940506     C************************************************************
145600940506     C     REPKEY        BEGSR
145700940506     C*
145800940506     C                   MOVEA     $FM2          $FM
145900940506     C                   EXSR      REPKKK
146000940506     C                   MOVEA     $FX           $FX2
146100940506     C                   ENDSR
146200940506     C/EJECT
146300940506     C************************************************************
146400940506     C* REPERIMENTO TASTI DI FUNZIONE
146500940506     C************************************************************
146600940506     C     REPKKK        BEGSR
146700940506     C*
146800940506     C                   CLEAR                   $FX
146900940506     C     1             DO        24            X
147000940506     C*
147100940506     C     $FM(X)        IFNE      *BLANK
147200940506     C*
147300940506     C                   MOVEL     $FM(X)        IDMSG
147400970214     C                   CALLB     'XRTVM'
147500940506     C                   PARM                    IDMSG            27
147600940506     C                   PARM                    TXTMSG          100
147700940506     C                   PARM                    ERRMSG            1
147800940506     C*
147900940506     C     ERRMSG        IFNE      *ON
148000940506     C                   MOVEL     TXTMSG        $FX(X)
148100940506     C                   ELSE
148200940506     C                   MOVEL     *ALL' '       $FX(X)
148300940506     C                   END
148400940506     C*
148500940506     C                   END
148600940506     C*
148700940506     C                   END
148800940506     C*
148900940506     C                   ENDSR
149000940223     C/EJECT
149100940128     C************************************************************
149200940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
149300940128     C************************************************************
149400940223     C     DEFCAM        BEGSR
149500940128     C*
149600940223     C* Variabili per gestione videate
149700940223     C*
149800940223     C                   MOVE      *BLANK        $GEST             2
149900940223     C                   MOVE      *BLANK        $FINE             1
150000940223     C                   MOVE      *BLANK        $INZS1            1
150100940223     C                   MOVE      *BLANK        $LASTG            2
150200940223     C                   MOVE      *BLANK        $LASTV            2
150300940223     C                   MOVE      *BLANK        $EFILE            1
150400940223     C                   MOVE      *BLANK        $ESCI             1
150500940223     C                   MOVE      *BLANK        $RCDOK            1
150600940223     C*
150700940314     C* Variabili appoggio sempre presenti in tutte le anagrafiche
150800940223     C*
150900940223     C     *LIKE         DEFINE    C1RCD         S1NRR
151000940223     C     *LIKE         DEFINE    C1NRR         WSFL
151100940223     C     *LIKE         DEFINE    C1RCD         WMAX
151200940223     C     *LIKE         DEFINE    C1RCD         WPAG
151300940223     C     *LIKE         DEFINE    $INZS1        WINZS1
151400940223     C                   MOVE      *BLANK        WWGRP             3
151500940223     C                   Z-ADD     0             CURR              2 0
151600940223     C                   Z-ADD     0             CURC              2 0
151700940223     C                   MOVE      *BLANK        WIN              99
151800940314     C     *LIKE         DEFINE    C1POS         W1POS
151900940620     C*
152000940721$022 C*    K5TBT1        KLIST
152100940721$022 C*                  KFLD                    TBTAZI
152200940721$022 C*                  KFLD                    TBTCOD
152300940721$022 C*                  KFLD                    TBTKEY
152400940721$022 C*                  KFLD                    TBTLIN
152500940721$022 C*                  KFLD                    TBTFLD
152600940714     C*
152700940714     C     KABL          KLIST
152800940714     C                   KFLD                    ABLCOD
152900940714     C                   KFLD                    ABLKEY
153000940714     C                   KFLD                    ABLAZI
153100940314     C*
153200940314     C*
153300940223     C* Indici
153400940223     C*
153500940223     C                   Z-ADD     0             X                 3 0
153600940223     C                   Z-ADD     0             Y                 3 0
153700940127     C*
153800940117     C                   ENDSR
153900940223     C/EJECT
154000940616**
154100940225PROMSG    *LIBL     OPZ0001
154200940225PROMSG    *LIBL     OPZ0002
154300940131PROMSG    *LIBL     OPZ0003
154400940131PROMSG    *LIBL     OPZ0004
154500940131PROMSG    *LIBL     OPZ0005
154600940224PROMSG    *LIBL     OPZ0006
154700940224PROMSG    *LIBL     OPZ0007
154800940224PROMSG    *LIBL     OPZ0008
154900940224PROMSG    *LIBL     OPZ0009
155000970716PROMSG    *LIBL     OPZ0010
155100970716PROMSG    *LIBL     OPZ0013
155200970716PROMSG    *LIBL     OPZ0012
155300970716PROMSG    *LIBL     OPZ0011
155400970716PROMSG    *LIBL     OPZ0014
155500940201**
15560094022501S      <Selezione
15570094022502S      <Modifica
15580094022503S      <Copia
15590094022504S      <Annullamento
15600094022505S      <Visualizzazione
15610094060706N      <prova
15620094060707N      <prova
15630094060708N      <prova
15640094060709N      <prova
15650094072910S      <Invia
15660094071411S      <Abilita gruppo
15670094071412S      <Disabilita
15680094072813N      <PROVA
15690094083114N      <Commenti
157000940506** TASTI DI FUNZIONE  S1
157100940506PROMSG    *LIBL     KEY0001  01
157200940506PROMSG    *LIBL     KEY0002  02
157300940506PROMSG    *LIBL     KEY0003  03
157400940506PROMSG    *LIBL     KEY0004  04
157500940506PROMSG    *LIBL     KEY0005  05
157600940506PROMSG    *LIBL     KEY0006  06
157700940506PROMSG    *LIBL     KEY0007  07
157800940506PROMSG    *LIBL     KEY0008  08
157900940506PROMSG    *LIBL     KEY0009  09
158000970716PROMSG    *LIBL     KEY0010  10
158100970716PROMSG    *LIBL     KEY0011  11
158200970716PROMSG    *LIBL     KEY0012  12
158300970716PROMSG    *LIBL     KEY0013  13
158400970716PROMSG    *LIBL     KEY0014  14
158500970716PROMSG    *LIBL     KEY0015  15
158600970716PROMSG    *LIBL     KEY0016  16
158700970716PROMSG    *LIBL     KEY0017  17
158800970716PROMSG    *LIBL     KEY0018  18
158900970716PROMSG    *LIBL     KEY0019  19
159000940506PROMSG    *LIBL     KEY0020  20
159100940506PROMSG    *LIBL     KEY0021  21
159200940506PROMSG    *LIBL     KEY0022  22
159300940506PROMSG    *LIBL     KEY0023  23
159400940506PROMSG    *LIBL     KEY0024  24
159500940506**  ABILITAZIONE S1
1596009410140N  01
1597009407200N  02
1598009407201N  03
1599009407200N  04
1600009407200N  05
1601009407200N  06
1602009407200N  07
1603009407200N  08
1604009407200N  09
1605009407201N  10
1606009407200N  11
1607009407201N  12
1608009407200N  13
1609009407200N  14
1610009407200N  15
1611009407200N  16
1612009407200N  17
1613009407200N  18
1614009407200N  19
1615009407200N  20
1616009407200N  21
1617009407200N  22
1618009505020N  23
1619009407200N  24
