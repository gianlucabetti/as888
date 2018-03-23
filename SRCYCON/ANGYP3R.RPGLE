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
003900940715$001 FANGYP3D   CF   E             WORKSTN USROPN
004000940607     F                                     SFILE(S1:S1NRR)
004100940201     F                                     INFDS(DSFMT)
004200940714     FANABL01L  UF A E           K DISK
004300040617     F*--------
004400040617     FANCLN01L  IF   E           K DISK
004500940128     D*----------------------------------------------------*
004600940302     D* Schiere per la gestione delle opzioni
004700940302     D*
004800950503    >D $MS1            S             27    DIM(14) CTDATA PERRCD(1)
004900950503    >D $TX1            S             50    DIM(14) ALT($MS1)
005000940728    >D $OP1            S              2  0 DIM(14) CTDATA PERRCD(1)
005100940728    >D $FL1            S              1    DIM(14) ALT($OP1)
005200950503     D $TX             S             50    DIM(50)
005300950503     D $FL             S              1    DIM(50)
005400940506     D* Schiere per la gestione dei tasti di funzione
005500940506     D $FM             S             27    DIM(24)
005600940506     D $FX             S             50    DIM(24)
005700940506     D $FC             S              1    DIM(24)
005800940506     D $FR             S              1    DIM(24)
005900940506     D* Schiere per la gestione dei tasti di funzione
006000940506     D* Video S1
006100940506     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
006200940506     D $FX2            S             50    DIM(24) ALT($FM2)
006300940506     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
006400940506     D $FR2            S              1    DIM(24) ALT($FC2)
006500940616     D* campi in lingua
006600940616$010 D*LIN             S              6    DIM(0011) CTDATA PERRCD(001)
006700940616$010 D*LIND            S             50    DIM(0011) ALT(LIN)
006800940616$010 D*LPOS            S              3  0 DIM(0011) CTDATA PERRCD(001)
006900940616$010 D*LLUN            S              3  0 DIM(0011) ALT(LPOS)
007000940211     D* Passaggio Parametri
007100940211     D KPJBA         E DS
007200940325     D*-------------
007300940620     D* DS Esterna autorizzazioni tabelle
007400940620     D XCHATB        E DS                  EXTNAME(XCHKATBDS)
007500940620     D*-------------
007600940620     D* DS  restituzione autorizzazioni
007700940620     D* IND=indice
007800940620     D* GRU=man. di gruppo
007900940620     D* AZI=man. azienda
008000941111     D AUTDS           DS
008100940620     D  MANIND                 1      1
008200940620     D  MANGRU                 2      2
008300940620     D  MANAZI                 3      3
008400940607     D* Richiamo a XATB
008500940607     D XATBDS        E DS
008600941103     D*-------------
008700941103      * DS Società
008800941103     D SOC001        E DS                  EXTNAME(XSOC001DS)
008900960123    >D  SALDOC       E                     EXTFLD(XSCSALDOC)
009000960123    >D  SALDOA       E                     EXTFLD(XSCSALDOA)
009100941103     D*-------------
009200941103      * DS Interna per dati utente
009300941103     D XSOCDS          DS          1000
009400940607     D*
009500940325     D* Parametri in ricezione
009600970708     D XTABDS        E DS
009700940325     D*-------------
009800940325     D* Parametri del PGM di manutenzione STD002R
009900940607    >D D002DS          DS
010000940607     D  OPZ002                 1      2
010100940607     D* 0 = ritorno normale
010200940607     D* 1 = F3
010300940607     D* 2 = F12
010400940607     D  RET002                 3      3
010500940607     D* operazione eseguite dal pgm chiamato :
010600940607     D* 0 = nessuna operazione
010700940607     D* 1 = eseguito aggiornamento
010800940607     D  OPR002                 4      4
010900940607     D* funzione non eseguibile per errore :
011000940607     D* 0 = nessun errore
011100940607     D* 1 = funzione richiamata chiusa in errore
011200940607     D* Aggiungere eventuali altri codici di errore
011300940607     D  ERR002                 5      5
011400940607     D* Azienda
011500940607     D  AZI002                 6      8
011600940607     D* Lingua
011700940607     D  LIN002                 9      9
011800940607     D* Codice scelto
011900940607     D  KEY002                10     24
012000970509     D* Autorizzazione
012100970509     D  AUT002                25     25
012200970509     D* Unita
012300970509     D  UNI002                26     33
012400940211     D*-------------
012500940211     D DSFMT           DS           512
012600940506     D  $TASTO               369    369
012700940211     D  NRG                  370    370
012800940211     D  NCL                  371    371
012900940211     D  SFLNRR               378    379B 0
013000940207     D*-------------
013100940207     D* Nome PGM a video
013200940207     D                 DS
013300940207     D  PROGR                  1     10
013400940207     D  ASTER1                 1      1    INZ('*')
013500940207     D  SIGLA                  2      9
013600940207     D  ASTER2                10     10    INZ('*')
013700940729     D*
013800940729     D*
013900940127     D*-------------
014000940127     D* Reperimento nome PGM
014100940127     D STATUS         SDS           333
014200940127     D  DSPGM            *PROC
014300940607     D* descrizione tabella
014400940607$003 D EXTDS         E DS                  EXTNAME(ANGYP3DS)
014500010406X0113D*-------------
014600010406  "  D* Reperimento titolo tabella
014700010406  "  D  WRKTIT         S             30A
014800010406  "  D  WRKTIT2        S            100A
014900010406  "  D  WRKTBCOD       S                   LIKE(XTBCOD)
015000010406  "  D  LENTIT         S              3  0
015100010406X0113D  I              S              3  0
015200940207     D*-------------
015300940211     D* COSTANTI
015400940211     D*-------------
015500940607     D SFLPAG          C                   CONST(10)
015600940728     D $DIMOP          C                   CONST(14)
015700940314     D* dimensione della schiera $MS1
015800940506     D*
015900940506     D* Tasti di funzione
016000940506     D F01             C                   CONST(X'31')
016100940506     D F02             C                   CONST(X'32')
016200940506     D F03             C                   CONST(X'33')
016300940506     D F04             C                   CONST(X'34')
016400940506     D F05             C                   CONST(X'35')
016500940506     D F06             C                   CONST(X'36')
016600940506     D F07             C                   CONST(X'37')
016700940506     D F08             C                   CONST(X'38')
016800940506     D F09             C                   CONST(X'39')
016900940506     D F10             C                   CONST(X'3A')
017000940506     D F11             C                   CONST(X'3B')
017100940506     D F12             C                   CONST(X'3C')
017200940506     D F13             C                   CONST(X'B1')
017300940506     D F14             C                   CONST(X'B2')
017400940506     D F15             C                   CONST(X'B3')
017500940506     D F16             C                   CONST(X'B4')
017600940506     D F17             C                   CONST(X'B5')
017700940506     D F18             C                   CONST(X'B6')
017800940506     D F19             C                   CONST(X'B7')
017900940506     D F20             C                   CONST(X'B8')
018000940506     D F21             C                   CONST(X'B9')
018100940506     D F22             C                   CONST(X'BA')
018200940506     D F23             C                   CONST(X'BB')
018300940506     D F24             C                   CONST(X'BC')
018400940506     D ENTER           C                   CONST(X'F1')
018500940506     D ROLDWN          C                   CONST(X'F4')
018600940506     D ROLLUP          C                   CONST(X'F5')
018700940302     I*
018800940211     I/EJECT
018900940207     I*-------------
019000940607     IS1
019100940607$015 I              S1RIMLUN                    RIMLUNYP3
019200040616$015 I              S1RIMMAR                    RIMMARYP3
019300040616$015 I              S1RIMMER                    RIMMERYP3
019400040616$015 I              S1RIMGIO                    RIMGIOYP3
019500040616$015 I              S1RIMVEN                    RIMVENYP3
019600940207     I*-------------
019700940127     I*
019800940127     C*----------------------------------------------------*
019900940127     C*                MAIN LINE PROGRAM
020000940127     C*----------------------------------------------------*
020100940127     C*
020200940223     C* inizializzazione variabili
020300940223     C                   EXSR      INZVAR
020400940223     C*
020500940223     C     $FINE         DOWEQ     *OFF
020600940131     C     $GEST         CASEQ     'S1'          GESS1
020700940117     C                   END
020800940117     C                   END
020900940325     C*
021000940325     C* fine programma
021100940325     C                   EXSR      ENDPGM
021200940225     C*
021300940325     C/EJECT
021400940325     C************************************************************
021500940325     C* FINE PROGRAMMA
021600940325     C************************************************************
021700940325     C     ENDPGM        BEGSR
021800940325     C*
021900940325     C* passaggio dati al pgm chiamante
022000940715     C                   MOVE      *BLANKS       KPJBU
022100940607     C                   MOVEL     XTABDS        KPJBU
022200940325     C*
022300940325     C* Se pgm Guida non è stato richiamato posso chiudere in LR
022400970708     C     XtaOpz        IFEQ      *BLANK
022500940325     C                   SETON                                            LR
022600960906     C                   RETURN
022700940325     C* altrimenti chiudo in RT
022800940325     C                   ELSE
022900940325     C                   SETON                                            RT
023000960906     C                   RETURN
023100940325     C                   ENDIF
023200940325     C*
023300940325     C                   ENDSR
023400940131     C/EJECT
023500940224     C************************************************************
023600940131     C* RIEMPIMENTO OPZIONI
023700940224     C************************************************************
023800940131     C     RIEOPZ        BEGSR
023900940223     C*
024000940223     C* Input :
024100940223     C* - $TX = schiera con tutti i testi disponibili
024200940223     C* - $FL = schiere con i flag di validità associati ai testi
024300940225     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
024400940223     C*          opzione usato
024500940223     C* - WMXO = variabile contenente il numero massimo di testi
024600940223     C*          opzione usabili
024700940223     C* Output :
024800940223     C* - WOPZ1 = variabile contenente la prima riga opzioni
024900940223     C* - WOPZ2 = variabile contenente la seconda riga opzioni
025000950502     C* - FF23  = flag che indica se c'è bisogno del tasto F23
025100940223     C*
025200970214     C                   CALLB     'X04OPMSG'
025300940223     C* input
025400940223     C                   PARM                    $TX
025500940223     C                   PARM                    $FL
025600940225     C                   PARM                    WCOZ              3 0
025700940223     C                   PARM                    WMXO              3 0
025800940223     C* output
025900940223     C                   PARM                    WOPZ1            76
026000940223     C                   PARM                    WOPZ2            76
026100950502     C                   PARM                    FF23              1
026200940607     C* input
026300940607$018 C                   PARM      44            $L01              3 0
026400940607     C                   PARM      0             $L02              3 0
026500940223     C*
026600940131     C                   ENDSR
026700940506     C/EJECT
026800940506     C************************************************************
026900940506     C* RIEMPIMENTO TASTI FUNZIONE
027000940506     C************************************************************
027100940506     C     RIEKEY        BEGSR
027200940506     C*
027300940506     C* Input :
027400940506     C* - $FX = schiera con tutti i testi disponibili
027500940506     C* - $FC = schiere con i flag di validità associati ai testi
027600940506     C* - $FR = schiere con i flag di REVERSE IMAGE
027700940506     C* - $ULK = ultimo testo caricato
027800940506     C* Output :
027900940506     C* - $ULK
028000940506     c* - $KEY1= 1° riga di tasti funzione
028100940506     c* - $KEY2= 2° riga di tasti funzione
028200940506     C*
028300970214     C                   CALLB     'XKEYMSG'
028400940506     C* input
028500940506     C                   PARM                    $FX
028600940506     C                   PARM                    $FC
028700940506     C                   PARM                    $FR
028800940506     C                   PARM                    $ULK              3 0
028900940506     C* output
029000940506     C                   PARM                    $KEY1            79
029100940506     C                   PARM                    $KEY2            79
029200940607     C* input
029300940607$018 C                   PARM      44            $L01              3 0
029400941004$018 C                   PARM      44            $L02              3 0
029500940506     C*
029600940506     C                   ENDSR
029700940224     C/EJECT
029800940224     C************************************************************
029900940224     C* GESTIONE EMISSIONE TASTI FUNZIONE
030000940224     C************************************************************
030100940224     C     GESPIE        BEGSR
030200940224     C*
030300940224     C                   SELECT
030400940506     C* Lista
030500940506     C     $GEST         WHENEQ    'S1'
030600940506     C     $ULKS1        IFEQ      0
030700940506     C                   EXSR      F24S1
030800940506     C                   ELSE
030900940607     C                   WRITE     Z1
031000940506     C                   ENDIF
031100940506     C                   ENDSL
031200940506     C*
031300940224     C                   ENDSR
031400940506     C/EJECT
031500940506     C************************************************************
031600940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA S1
031700940506     C************************************************************
031800940506     C     CTRKS1        BEGSR
031900940506     C                   SETOFF                                       99
032000940506     C                   MOVEA     $FC2          $FC
032100940506     C                   EXSR      CTRKEY
032200940506     C     *IN99         IFEQ      *ON
032300940506     C                   SETON                                        97
032400940506     C                   ENDIF
032500940607     C*
032600940506     C                   ENDSR
032700940506     C/EJECT
032800940506     C************************************************************
032900940506     C* CONTROLLO ABILITAZIONE TASTI
033000940509     C* INPUT
033100940509     c* $TASTO =Tasto funzione premuto
033200940509     C* $FC = Tasti abilitati
033300940509     C* OUTPUT
033400940509     C* IN99 = ERrore
033500940506     C************************************************************
033600940506     C     CTRKEY        BEGSR
033700940506     C                   SELECT
033800940506     C     $TASTO        WHENEQ    F01
033900940720     C     $FC(1)        IFEQ      '0'
034000940506     C                   SETON                                        99
034100940506     C                   ENDIF
034200940506     C     $TASTO        WHENEQ    F02
034300940720     C     $FC(2)        IFEQ      '0'
034400940506     C                   SETON                                        99
034500940506     C                   ENDIF
034600940506     C     $TASTO        WHENEQ    F03
034700940720     C     $FC(3)        IFEQ      '0'
034800940506     C                   SETON                                        99
034900940506     C                   ENDIF
035000940506     C     $TASTO        WHENEQ    F04
035100940720     C     $FC(4)        IFEQ      '0'
035200940506     C                   SETON                                        99
035300940506     C                   ENDIF
035400940506     C     $TASTO        WHENEQ    F05
035500940720     C     $FC(5)        IFEQ      '0'
035600940506     C                   SETON                                        99
035700940506     C                   ENDIF
035800940506     C     $TASTO        WHENEQ    F06
035900940720     C     $FC(6)        IFEQ      '0'
036000940506     C                   SETON                                        99
036100940506     C                   ENDIF
036200940506     C     $TASTO        WHENEQ    F07
036300940720     C     $FC(7)        IFEQ      '0'
036400940506     C                   SETON                                        99
036500940506     C                   ENDIF
036600940506     C     $TASTO        WHENEQ    F08
036700940720     C     $FC(8)        IFEQ      '0'
036800940506     C                   SETON                                        99
036900940506     C                   ENDIF
037000940506     C     $TASTO        WHENEQ    F09
037100940720     C     $FC(09)       IFEQ      '0'
037200940506     C                   SETON                                        99
037300940506     C                   ENDIF
037400940506     C     $TASTO        WHENEQ    F10
037500940720     C     $FC(10)       IFEQ      '0'
037600940506     C                   SETON                                        99
037700940506     C                   ENDIF
037800940506     C     $TASTO        WHENEQ    F11
037900940720     C     $FC(11)       IFEQ      '0'
038000940506     C                   SETON                                        99
038100940506     C                   ENDIF
038200940506     C     $TASTO        WHENEQ    F12
038300940720     C     $FC(12)       IFEQ      '0'
038400940506     C                   SETON                                        99
038500940506     C                   ENDIF
038600940506     C     $TASTO        WHENEQ    F13
038700940720     C     $FC(13)       IFEQ      '0'
038800940506     C                   SETON                                        99
038900940506     C                   ENDIF
039000940506     C     $TASTO        WHENEQ    F14
039100940720     C     $FC(14)       IFEQ      '0'
039200940506     C                   SETON                                        99
039300940506     C                   ENDIF
039400940506     C     $TASTO        WHENEQ    F15
039500940720     C     $FC(15)       IFEQ      '0'
039600940506     C                   SETON                                        99
039700940506     C                   ENDIF
039800940506     C     $TASTO        WHENEQ    F16
039900940720     C     $FC(16)       IFEQ      '0'
040000940506     C                   SETON                                        99
040100940506     C                   ENDIF
040200940506     C     $TASTO        WHENEQ    F17
040300940720     C     $FC(17)       IFEQ      '0'
040400940506     C                   SETON                                        99
040500940506     C                   ENDIF
040600940506     C     $TASTO        WHENEQ    F18
040700940720     C     $FC(18)       IFEQ      '0'
040800940506     C                   SETON                                        99
040900940506     C                   ENDIF
041000940506     C     $TASTO        WHENEQ    F19
041100940720     C     $FC(19)       IFEQ      '0'
041200940506     C                   SETON                                        99
041300940506     C                   ENDIF
041400940506     C     $TASTO        WHENEQ    F20
041500940720     C     $FC(20)       IFEQ      '0'
041600940506     C                   SETON                                        99
041700940506     C                   ENDIF
041800940506     C     $TASTO        WHENEQ    F21
041900940720     C     $FC(21)       IFEQ      '0'
042000940506     C                   SETON                                        99
042100940506     C                   ENDIF
042200940506     C     $TASTO        WHENEQ    F22
042300940720     C     $FC(22)       IFEQ      '0'
042400940506     C                   SETON                                        99
042500940506     C                   ENDIF
042600940506     C     $TASTO        WHENEQ    F23
042700940720     C     $FC(23)       IFEQ      '0'
042800940506     C                   SETON                                        99
042900940506     C                   ENDIF
043000940506     C     $TASTO        WHENEQ    F24
043100940720     C     $FC(24)       IFEQ      '0'
043200940506     C                   SETON                                        99
043300940506     C                   ENDIF
043400940506     C                   ENDSL
043500940506     C                   ENDSR
043600940224     C/EJECT
043700940127     C************************************************************
043800940131     C* GESTIONE LISTA
043900940127     C************************************************************
044000940127     C     GESS1         BEGSR
044100940223     C* inizializzazione videata
044200940223     C     $INZS1        IFEQ      *ON
044300940127     C                   EXSR      INZS1
044400940223     C                   MOVE      *OFF          $INZS1
044500940127     C                   ENDIF
044600940223     C* emissione piede videata se proveniente da altra
044700940223     C* salvataggio valore formato di provenienza
044800940202     C     $LASTV        IFNE      'S1'
044900940607!!   C*                    WRITEFMTT1
045000940224     C                   EXSR      GESPIE
045100940202     C                   MOVE      $LASTV        $LASTG
045200940202     C                   MOVE      'S1'          $LASTV
045300940202     C                   END
045400940223     C*
045500940223     C     WMAX          IFEQ      0
045600940607     C                   WRITE     D1
045700940211     C                   ENDIF
045800940729     C                   MOVE      '1'           WIN727            1
045900940127     C*
046000940607     C                   EXFMT     C1
046100940204     C     C1NRR         IFNE      0
046200940204     C                   Z-ADD     C1NRR         WSFL
046300940204     C                   ENDIF
046400940127     C                   Z-ADD     SFLNRR        C1RCD
046500940506     C*
046600940506     C* controllo abilitazione tasti funzione
046700940506     C*
046800940506     C                   EXSR      CTRKS1
046900940506     C     *IN99         IFEQ      *OFF
047000940506     C*
047100940127     C*
0472009401271    C                   SELECT
047300940127     C* F3=Fine
047400940506     C     $TASTO        WHENEQ    F03
047500940309     C                   EXSR      F03S1
047600940131     C* F10=Immissione
047700940506     C     $TASTO        WHENEQ    F10
047800940309     C                   EXSR      F10S1
047900940131     C* F12=Ritorno
048000940506     C     $TASTO        WHENEQ    F12
048100940309     C                   EXSR      F12S1
048200940224     C* F23=Altre opzioni
048300940506     C     $TASTO        WHENEQ    F23
048400940309     C                   EXSR      F23S1
048500940224     C* F24=Altri tasti
048600940506     C     $TASTO        WHENEQ    F24
048700940309     C                   EXSR      F24S1
048800940127     C* ROLL-UP
048900940506     C     $TASTO        WHENEQ    ROLLUP
049000940127     C                   EXSR      ROLS1
049100940127     C*
0492009401271O   C                   OTHER
049300940127     C* CONTROLLO DATI
049400940131     C                   EXSR      CTRC1
049500940201     C     *IN99         IFEQ      *OFF
049600940131     C                   EXSR      CTRS1
049700940131     C                   END
0498009401271-   C                   ENDSL
049900940127     C*
050000940506     C                   ENDIF
050100940127     C                   ENDSR
050200940224     C/EJECT
050300940127     C************************************************************
050400940131     C* INIZIALIZZAZIONE LISTA
050500940127     C************************************************************
050600940127     C     INZS1         BEGSR
050700940302     C* pulizia SFL
050800940128     C                   SETOFF                                         3031
050900940607     C                   WRITE     C1
051000940128     C                   SETON                                          31
051100010406X0113C*
051200010406  "  C* reperimento titolo
051300010406  "  C                   move      xtbcod        WrkTbcod
051400040617  "  C                   eval      xtbkey      = *zeros
051500040617  "  C                   move      xtbcod        xtbkey
051600010406  "  C                   eval      xtbazi      = xscsoc
051700010406  "  C                   eval      xtblin      = xsclin
051800010406  "  C                   eval      xtbcod      = 'AAA'
051900010406  "  C                   eval      xtbric      = '1'
052000010406  "  C                   eval      xtbalc      = '0'
052100010406  "  C                   CALLB     'XATB'
052200010406  "  C                   PARM                    XATBDS
052300010406  "  C                   if        xtberr <> *off
052400010406  "  C                   movel     *all'?'       WrkTit
052500010406  "  C                   else
052600010406  "  C                   movel     xtbuni        WrkTit
052700010406  "  C                   endif
052800010406  "  C*
052900010406  "  C* centratura
053000010406  "  C                   move      WrkTbcod      xtbcod
053100010406  "  C                   eval      WrkTit2=%trimr(Wrktit)+X'22'+xtbcod
053200010406  "  C                             +X'20'
053300010406  "  C     ' '           CHECKR    Wrktit2       I
053400010406  "  C                   call      'X71CENTRA'
053500010406  "  C                   parm                    Wrktit2
053600010406  "  C                   parm                    I
053700010406  "  C                   parm                    lentit
053800010406X0113C                   movel     Wrktit2       C1tit
053900940127     C*
054000940128     C* riempimento opzioni
054100940223     C                   MOVEA     $TX1          $TX
054200940223     C                   MOVEA     $FL1          $FL
054300940309     C                   Z-ADD     0             WCOZ
054400940225     C*  equivale al num.di elementi della schiera $TX1
054500940509     C                   Z-ADD     $DIMOP        WMXO
054600940128     C                   EXSR      RIEOPZ
054700950502     C                   Z-ADD     23            X
054800950502     C                   MOVE      FF23          $FC2(X)
054900940608     C                   MOVEL     WOPZ1         C1OPZ1
055000940608     C**                   MOVELWOPZ2     C1OPZ2
055100940225     C                   Z-ADD     WCOZ          C1COZ
055200940128     C*
055300940128     C* CARICAMENTO SFL A PAGINE
055400940128     C*
055500940201     C                   Z-ADD     0             S1NRR
055600940128     C                   Z-ADD     0             WMAX
055700940224     C*
055800940224     C* Posizionamento su file pilota
055900940302     C                   CLEAR                   C1POS
056000940316    >C                   EXSR      SETANA
056100940608    >C                   EXSR      REDANA
056200940302     C* ripristino i valori del posizionamento
056300940302     C                   MOVE      W1POS         C1POS
056400940131     C*
056500940131     C* Carico 1a pagina
056600940127     C                   EXSR      ROLS1
056700940201     C                   Z-ADD     SFLPAG        WPAG
056800940127     C*
056900940127     C                   ENDSR
057000940127     C/EJECT
057100940127     C************************************************************
057200940131     C* CARICAMENTO PAGINA LISTA
057300940127     C************************************************************
057400940127     C     ROLS1         BEGSR
057500940127     C*
057600940128     C                   SETOFF                                       32
057700940223     C                   Z-ADD     0             Y
057800940127     C                   Z-ADD     WMAX          S1NRR
057900940127     C*
058000940127     C* Leggo dal file anagrafico per caricare la lista
058100940127     C*
0582009401311    C     $EFILE        DOWEQ     *OFF
058300940201     C     Y             ANDLT     WPAG
058400940127     C*
058500940131     C                   EXSR      RIES1
058600940127     C*
058700940127     C                   ADD       1             S1NRR
058800940127     C                   ADD       1             Y
058900940714     C                   MOVE      $IN41         *IN41
059000940127     C*
059100940607     C                   WRITE     S1
059200940131     C*
059300940316    >C                   EXSR      REDANA
059400940128     C*
0595009401271-   C                   ENDDO
059600940127     C*
059700940223     C                   Z-ADD     S1NRR         WMAX                 30
059800940127     C*
059900940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
060000940127     C*
060100940201     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
060200940127     C                   MVR                     RESTO             3 0
060300940131     C     PAGINE        MULT      SFLPAG        C1RCD
0604009401271    C     RESTO         IFGT      0
060500940127     C                   ADD       1             C1RCD
0606009401271E   C                   ELSE
060700940407     C                   SUB       SFLPAG        C1RCD
060800940407     C                   ADD       1             C1RCD
0609009401271-   C                   ENDIF
061000940128     C*
061100940127     C                   ENDSR
061200940224     C/EJECT
061300940224     C************************************************************
061400940224     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
061500940224     C************************************************************
061600940607     C     SETANA        BEGSR
061700940224     C*
061800941004     C                   MOVE      *BLANKS       XTBKEY
061900970708     C                   MOVE      XtaAzi        XTBAZI
062000970708     C                   MOVE      XtaLin        XTBLIN
062100940608     C                   MOVE      '9'           XTBRIC
062200940607     C                   MOVE      '0'           XTBALC
062300950622     C                   MOVE      XSCCUT        XTBUTE
062400940607     C                   MOVE      '0'           XTBERR
062500941121     C**         W1POS     IFNE *BLANK
062600040616     C                   MOVE      *blank        XTBKEY
062700040616     C                   MOVEl     W1POS         XTBKEY
062800941121     C**                   END
062900970214     C                   CALLB     'XATB'
063000940607     C                   PARM                    XATBDS
063100940607     C*
063200940224     C*
063300940224     C                   ENDSR
063400940128     C/EJECT
063500940128     C************************************************************
063600940131     C* LETTURA RCD ARCHIVIO PILOTA
063700940128     C************************************************************
063800940607     C     REDANA        BEGSR
063900940128     C*
064000940131     C                   MOVEL     *OFF          $RCDOK
064100940131     C*
0642009401311    C     $EFILE        DOUEQ     *ON
064300940131     C     $RCDOK        OREQ      *ON
064400940131     C*
064500940607     C                   MOVE      '3'           XTBRIC
064600940607     C                   MOVE      '0'           XTBERR
064700970214     C                   CALLB     'XATB'
064800940607     C                   PARM                    XATBDS
064900940607     C*
065000940607     C     XTBERR        IFNE      '0'
065100940607     C                   MOVEL     *ON           $EFILE
065200010214     C                   ELSE
065300010214     C                   MOVEL     *OFF          $EFILE
065400940607     C                   ENDIF
065500940131     C*
0656009401312    C     $EFILE        IFEQ      *OFF
065700940607     C                   EXSR      SELANA
065800040616     C                   MOVEl     XTBKEY        SAVKEY
065900950502     C                   MOVE      XTBGRU        SAVGRU
066000950502     C                   MOVE      XTBUNI        SAVUNI
066100970509     C                   MOVE      XTBUNT        SAVUNT
0662009401312-   C                   ENDIF
066300940131     C*
0664009401311-   C                   ENDDO
066500940131     C*
066600940607     C                   MOVE      $EFILE        *IN33
066700940131     C*
066800940131     C                   ENDSR
066900940224     C/EJECT
067000940224     C************************************************************
067100940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
067200940224     C************************************************************
067300940224     C     CLCPAG        BEGSR
067400940224     C* Input :
067500940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
067600940224     C*          cursore
067700940224     C* - SFLPAG = numero rcd per pagina sfl
067800940224     C* Output :
067900940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
068000940224     C*
068100940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
068200940224     C                   MVR                     RESTO             3 0
068300940224     C     RESTO         IFGT      0
068400940224     C                   ADD       1             PAGINE
068500940224     C                   ENDIF
068600940224     C     PAGINE        MULT      SFLPAG        WPAG
068700940224     C*
068800940224     C                   ENDSR
068900940131     C/EJECT
069000940131     C************************************************************
069100940131     C* SELEZIONI SULLA RIGA LETTA
069200940314     C* impostare a *OFF il campo $RCDOK per scartare un record che
069300940314     C* non rientra nei parametri di parzializzazione
069400940314     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
069500940314     C* superiore stabilito nei parametri
069600940314     C*
069700940131     C************************************************************
069800940607     C     SELANA        BEGSR
069900940131     C*
070000940202     C                   MOVEL     *ON           $RCDOK
070100940714     C* Se aperto il pgm con opzione 11
070200940714     C* carico solo i record di gruppo
070300970708     C     XtaOpz        IFEQ      '11'
070400940714     C     XTBGRU        ANDNE     'G'
070500940714     C                   MOVEL     *OFF          $RCDOK
070600940714     C                   END
070700940714     C* Carica in alta intensità solo i record
070800940714     C* abilitati
070900940714     C                   MOVE      '0'           $IN41
071000970708     C     XtaOpz        IFEQ      '11'
071100940714     C     XTBGRU        ANDEQ     'G'
071200940715     C     XTBAZB        ANDNE     'N'
071300940714     C                   MOVE      '1'           $IN41
071400940714     C                   END
071500940714     C*
071600940202     C                   ENDSR
071700940131     C/EJECT
071800940131     C************************************************************
071900940131     C* RIEMPIMENTO RIGA LISTA
072000940131     C************************************************************
072100940131     C     RIES1         BEGSR
072200950502     C                   MOVEL     SAVUNI        EXTDS
072300040617     C                   MOVEl     SAVKEY        S1COD
072400050211     C                   IF        S1Cod = *ZERO
072500050211     C                   EVAL      S1DES = 'Rimborso standard'
072600050211     C                   ELSE
072700040617     C     KEYcln        CHAIN     ancln01l                           66
072800040617     c  n66              eval      S1DES  = CLNDESBREV
072900050211     C                   ENDIF
073000950502     C                   MOVE      SAVGRU        S1GRU
073100970512     C                   MOVE      SAVUNT        S1UNITA           8
073200981209$025 C     RIMLUNYP3     IFEQ      *ON
073300981209$025 C                   MOVE      XSCON         RIMLUNYP3
073400981209$025 C                   ELSE
073500981209$025 C                   MOVE      XSCOFF        RIMLUNYP3
073600981209$025 C                   ENDIF
073700981209$025 C     RIMMARYP3     IFEQ      *ON
073800981209$025 C                   MOVE      XSCON         RIMMARYP3
073900981209$025 C                   ELSE
074000981209$025 C                   MOVE      XSCOFF        RIMMARYP3
074100981209$025 C                   ENDIF
074200981209$025 C     RIMMERYP3     IFEQ      *ON
074300981209$025 C                   MOVE      XSCON         RIMMERYP3
074400981209$025 C                   ELSE
074500981209$025 C                   MOVE      XSCOFF        RIMMERYP3
074600981209$025 C                   ENDIF
074700981209$025 C     RIMGIOYP3     IFEQ      *ON
074800981209$025 C                   MOVE      XSCON         RIMGIOYP3
074900981209$025 C                   ELSE
075000981209$025 C                   MOVE      XSCOFF        RIMGIOYP3
075100981209$025 C                   ENDIF
075200981209$025 C     RIMVENYP3     IFEQ      *ON
075300981209$025 C                   MOVE      XSCON         RIMVENYP3
075400981209$025 C                   ELSE
075500981209$025 C                   MOVE      XSCOFF        RIMVENYP3
075600981209$025 C                   ENDIF
075700981209$027 C
075800950502     C     *LIKE         DEFINE    XTBUNI        SAVUNI
075900950502     C     *LIKE         DEFINE    XTBKEY        SAVKEY
076000950502     C     *LIKE         DEFINE    XTBGRU        SAVGRU
076100970509     C     *LIKE         DEFINE    XTBUNT        SAVUNT
076200940131     C*
076300940131     C                   ENDSR
076400940309     C/EJECT
076500940309     C************************************************************
076600940309     C* GESTIONE F03 VIDEO S1
076700940309     C************************************************************
076800940309     C     F03S1         BEGSR
076900940309     C*
077000940309     C                   MOVE      *ON           $FINE
077100970708     C                   MOVE      '1'           XtaRet
077200940325     C* fine programma
077300940325     C                   EXSR      ENDPGM
077400940309     C*
077500940309     C                   ENDSR
077600940309     C/EJECT
077700940309     C************************************************************
077800940309     C* GESTIONE F10 VIDEO S1
077900940314     c* AGGIUNTA RECORD
078000940309     C************************************************************
078100940309     C     F10S1         BEGSR
078200940309     C*
078300940607     C                   RESET                   D002DS
078400940607     C                   MOVEL     '01'          OPZ002
078500970708     C                   MOVE      XtaLin        LIN002
078600970708     C                   MOVE      XtaAzi        AZI002
078700970512     C                   MOVE      S1UNITA       UNI002
078800940715     C                   MOVE      *BLANKS       KPJBU
078900940607     C                   MOVEL     D002DS        KPJBU
079000940608$004 C                   CALL      'ANGYP3R2'
079100940309     C                   PARM                    KPJBA
079200940607     C                   MOVEL     KPJBU         D002DS
079300940309     C* ritorno da PGM gestione
079400940309     C                   EXSR      GESRET
079500940309     C     WINZS1        IFEQ      *ON
079600940309     C                   MOVE      *ON           $INZS1
079700940309     C* se esistevano già righe sul sfl
079800940309     C* calcolo pagina a cui deve posizionarsi
079900940309     C     WSFL          IFGT      0
080000940309     C                   EXSR      CLCPAG
080100940309     C* altrimenti carico solo la 1a pagina
080200940309     C                   ELSE
080300940309     C                   Z-ADD     SFLPAG        WPAG
080400940309     C                   ENDIF
080500940309     C                   ENDIF
080600940309     C*
080700940712     C                   MOVE      *HIVAL        $LASTV
080800940309     C                   ENDSR
080900940309     C/EJECT
081000940309     C************************************************************
081100940309     C* GESTIONE F12 VIDEO S1
081200940309     C************************************************************
081300940309     C     F12S1         BEGSR
081400940309     C*
081500970708     C                   MOVE      '2'           XtaRet
081600940309     C                   MOVE      *ON           $FINE
081700940309     C*
081800940309     C                   ENDSR
081900940309     C/EJECT
082000940309     C************************************************************
082100940309     C* GESTIONE F23 VIDEO S1
082200940309     C************************************************************
082300940309     C     F23S1         BEGSR
082400940309     C*
082500940309     C                   EXSR      RIEOPZ
082600950502     C                   Z-ADD     23            X
082700950502     C                   MOVE      FF23          $FC2(X)
082800940608     C                   MOVEL     WOPZ1         C1OPZ1
082900940608     C**                   MOVELWOPZ2     C1OPZ2
083000940309     C                   Z-ADD     WCOZ          C1COZ
083100940309     C*
083200940309     C                   ENDSR
083300940309     C/EJECT
083400940309     C************************************************************
083500940309     C* GESTIONE F24 VIDEO S1
083600940309     C************************************************************
083700940309     C     F24S1         BEGSR
083800940309     C*
083900940506     C*
084000940506     C                   MOVEA     $FX2          $FX
084100940506     C                   MOVEA     $FC2          $FC
084200940506     C                   MOVEA     $FR2          $FR
084300940506     C                   Z-ADD     $ULKS1        $ULK
084400940506     C                   EXSR      RIEKEY
084500941017     C                   MOVEA     $FC           $FC2
084600940506     C                   Z-ADD     $ULK          $ULKS1            3 0
084700940607     C                   MOVEL     $KEY1         Z1KE1
084800941004     C                   MOVEL     $KEY2         Z1KE2
084900940607     C                   WRITE     Z1
085000940506     C*
085100940309     C                   ENDSR
085200940128     C/EJECT
085300940128     C************************************************************
085400940131     C* CONTROLLO TESTATA LISTA
085500940128     C************************************************************
085600940131     C     CTRC1         BEGSR
085700940128     C*
085800940201     C                   MOVE      *OFF          *IN99
085900940131     C*
086000940202     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
086100940202     C* L'INIZIALIZZAZIONE LISTA
086200940202     C*
086300940207     C     C1POS         IFNE      W1POS
086400940202     C                   EXSR      POSS1
086500940202     C                   END
086600940202     C*
086700940202     C                   ENDSR
086800940202     C/EJECT
086900940202     C************************************************************
087000940202     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
087100940202     C************************************************************
087200940202     C     POSS1         BEGSR
087300940202     C*
087400940202     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
087500940202     C*
0876009402021    C     WMAX          IFGT      0
087700940607     C                   READC     S1                                     21
0878009402022    C     *IN21         DOWEQ     *OFF
087900940202     C     *IN99         ANDEQ     *OFF
088000940202     C*
0881009402023    C     S1OPZ         IFNE      0
088200940202     C                   SETON                                        51  99
0883009402023-   C                   ENDIF
088400940202     C* se rilevata opzione attiva, attivo sflnxtchg
0885009402023    C     *IN99         IFEQ      *ON
088600940202     C                   MOVE      *ON           *IN32
088700940714     C                   MOVE      $IN41         *IN41
088800940607     C                   UPDATE    S1
0889009402023E   C                   ELSE
089000940607     C                   READC     S1                                     21
0891009402023-   C                   ENDIF
089200940202     C*
0893009402022-   C                   ENDDO
089400940202     C*
0895009402021-   C                   ENDIF
089600940202     C*
089700940221     C*
0898009402021    C     *IN99         IFEQ      *OFF
089900941121     C                   CLEAR                   W1POS
090000940202     C*
090100940302     C                   MOVEL     C1POS         W1POS
090200940131     C*
090300940202     C                   MOVE      *ON           *IN99
090400940223     C                   MOVE      *ON           $INZS1
090500940202     C*
090600940202     C*
0907009402021-   C                   ENDIF
090800940202     C*
090900940131     C                   ENDSR
091000940131     C/EJECT
091100940131     C************************************************************
091200940131     C* CONTROLLO OPZIONI LISTA
091300940131     C************************************************************
091400940131     C     CTRS1         BEGSR
091500940131     C*
091600940202     C                   MOVEL     *OFF          $ESCI
091700940201     C                   SETOFF                                       99
091800940131     C                   Z-ADD     0             S1OPZ
091900940131     C*
092000940127     C* Leggo il sfl solo se ci sono rcd
0921009401311    C     WMAX          IFGT      0
092200940607     C                   READC     S1                                     21
092300940127     C*
092400940131     C* esce se fine sfl o errore che richiede l'uscita
0925009401312    C     *IN21         DOWEQ     *OFF
092600940131     C     $ESCI         ANDEQ     *OFF
092700940201     C                   Z-ADD     S1NRR         C1RCD
092800940131     C* ctrl su riga
092900940131     C                   EXSR      RECS1
093000940131     C* gestione opzioni
0931009401313    C     S1OPZ         IFNE      0
093200940201     C     *IN99         ANDEQ     *OFF
093300940131     C                   EXSR      OPZS1
0934009401273-   C                   ENDIF
093500940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0936009402013    C     *IN99         IFEQ      *ON
093700940201     C     $ESCI         OREQ      *ON
093800940131     C                   MOVE      *ON           *IN32
093900940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
094000940204     C* la ripristinerò a conclusione del ciclo di READC
094100940223     C                   MOVE      *OFF          $INZS1
0942009402233-   C                   ENDIF
094300940223     C*
094400940131     C                   Z-ADD     0             S1OPZ
094500940223     C*
094600940714     C                   MOVE      $IN41         *IN41
094700940607     C                   UPDATE    S1
094800940223     C*
094900940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0950009401313    C     $ESCI         IFEQ      *OFF
095100940607     C                   READC     S1                                     21
095200940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0953009402014    C     *IN21         IFEQ      *ON
095400940201     C                   MOVE      WINZS1        $INZS1
095500940204     C* calcolo pagina a cui deve posizionarsi
095600940224     C                   EXSR      CLCPAG
0957009402014-   C                   ENDIF
0958009402013-   C                   ENDIF
095900940131     C*
0960009401272-   C                   ENDDO
096100940127     C*
0962009401311-   C                   ENDIF
096300940131     C*
096400940127     C                   ENDSR
096500940127     C/EJECT
096600940127     C************************************************************
096700940131     C* CONTROLLO CAMPI I/O RIGA LISTA
096800940127     C************************************************************
096900940131     C     RECS1         BEGSR
097000940131     C*
097100940201     C* reset indicatori DSPATR
097200940201     C                   MOVE      *ALL'0'       IN4049           10
097300940201     C                   MOVEA     IN4049        *IN(40)
097400940201     C*
097500940131     C* Controllo se Utente Autorizzato alla scelta
097600940202     C     S1OPZ         IFNE      0
097700940131     C                   Z-ADD     1             X
097800940223     C     S1OPZ         LOOKUP    $OP1(X)                                21
097900940211     C     *IN21         IFEQ      *OFF
098000940223     C     $FL1(X)       OREQ      'N'
098100940131     C                   MOVE      *ON           $ESCI
098200940201     C                   SETON                                        504099
098300940131     C                   ENDIF
098400940131     C*
098500940620     c* Controllo autorizzazione alla modifica su record di gruppo
098600940620     C* Reperisco tipo codice (gruppo/azienda)
098700040616     C                   MOVE      *blank        XTBKEY
098800040616     C                   MOVEl     S1COD         XTBKEY
098900970512     C                   MOVE      S1UNITA       XTBUNT
099000940620     C                   MOVE      '0'           XTBALC
099100940620     C                   MOVE      '1'           XTBRIC
099200970214     C                   CALLB     'XATB'
099300940620     C                   PARM                    XATBDS
099400940620     C* Reperisco autorizzazioni utente
099500940620     C                   CLEAR                   XCHATB
099600941118     C                   MOVE      XSCSOC        XCTSOC
099700941118     C                   MOVE      XSCDUN        XCTUNI
099800940620     C                   MOVEL     KNMUS         XCTPRF
099900940620     C                   MOVE      XTBCOD        XCTCOD
100000940620     C                   CALL      'XCHKATB'
100100940620     C                   PARM                    XCHATB
100200941111     C                   MOVEL     XCTDDS        AUTDS
100300941027     C     XCTRTN        IFEQ      1
100400941027     C                   MOVEL     'N'           MANGRU
100500941027     C                   ENDIF
100600940620     C* Non ammessa manutenzione di gruppo
100700940620   1 C     XTBGRU        IFEQ      'G'
100800940620     C     MANGRU        ANDEQ     'N'
100900940620   2 C     S1OPZ         IFEQ      2
101000940729     C     S1OPZ         OREQ      3
101100940620     C     S1OPZ         OREQ      4
101200940620     C                   SETON                                        5399
101300940620   2 C                   ENDIF
101400940620   1 C                   ENDIF
101500940620     C* Non ammessa manutenzione di azienda
101600940620   1 C     XTBGRU        IFEQ      'A'
101700940620     C     MANAZI        ANDEQ     'N'
101800940620   2 C     S1OPZ         IFEQ      2
101900940729     C     S1OPZ         OREQ      3
102000940620     C     S1OPZ         OREQ      4
102100940620     C                   SETON                                        5499
102200940620   2 C                   ENDIF
102300940620   1 C                   ENDIF
102400940607     C                   ENDIF
102500940131     C                   ENDSR
102600940224     C/EJECT
102700940131     C************************************************************
102800940131     C* GESTIONE OPZIONI LISTA
102900940131     C************************************************************
103000940131     C     OPZS1         BEGSR
103100940201     C*
1032009402251    C                   SELECT
103300940225     C*
103400940225     C* se opz=Selezione passo la chiave al pgm chiamante
103500940225     C     S1OPZ         WHENEQ    1
103600970708     C                   MOVE      S1OPZ         XtaOpz
103700040616     C                   MOVEl     S1COD         XtaKey
103800970708     C                   MOVE      S1UNIta       xtaUni
103900940225     C                   MOVE      *ON           $ESCI
104000940225     C                   MOVE      *ON           $FINE
104100940729     C*
104200940714     C*
104300940714     C* se opz=Abilita scrive su archivio abilitazioni
104400940714     C     S1OPZ         WHENEQ    11
104500941103     C                   MOVE      XSCSOC        ABLAZI
104600040616     C                   MOVE      *blank        ABLKEY
104700040616     C                   MOVEl     S1COD         ABLKEY
104800970512     C                   MOVE      S1UNITA       ABLUNT
104900940714     C                   MOVE      XTBCOD        ABLCOD
105000940714     C                   WRITE     ANABL000                             22
105100940714     C*
105200940714     C* se opz=Disabil.cancella su archivio abilitazioni
105300940714     C     S1OPZ         WHENEQ    12
105400941103     C                   MOVE      XSCSOC        ABLAZI
105500040616     C                   MOVE      *blank        ABLKEY
105600040616     C                   MOVEl     S1COD         ABLKEY
105700970512     C                   MOVE      S1UNITA       ABLUNT
105800940714     C                   MOVE      XTBCOD        ABLCOD
105900940714     C     KABL          DELETE    ANABL000                           22
106000940729     C*
106100940225     C*
106200940225     C* se altre opzioni (o nessuna opzione) richiamo pgm gestione
1063009402251O   C                   OTHER
106400940225     C*
106500940616     C                   RESET                   D002DS
106600940607     C                   MOVEL     S1OPZ         OPZ002
106700040616     C                   MOVE      *blank        KEY002
106800040616     C                   MOVEl     S1COD         KEY002
106900970708     C                   MOVE      XtaLin        LIN002
107000970708     C                   MOVE      XtaAzi        AZI002
107100940715     C                   MOVE      *BLANKS       KPJBU
107200940607     C                   MOVEL     D002DS        KPJBU
107300940608$004 C                   CALL      'ANGYP3R2'
107400940607     C                   PARM                    KPJBA
107500940607     C                   MOVEL     KPJBU         D002DS
107600940201     C*
107700940223     C* ritorno da PGM gestione
107800940223     C                   EXSR      GESRET
107900940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
1080009402252    C     *IN99         IFEQ      *ON
108100940223     C                   SETON                                        40
1082009402252-   C                   ENDIF
108300940131     C*
108400940506     C                   MOVE      *HIVAL        $LASTV
1085009402251-   C                   ENDSL
108600940225     C*
108700940131     C                   ENDSR
108800940223     C/EJECT
108900940223     C************************************************************
109000940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
109100940223     C************************************************************
109200940223     C     GESRET        BEGSR
109300940223     C*
109400940223     C* modo di ritorno
109500940223     C*
1096009402231    C                   SELECT
109700940314    >C* << questi modi di utilizzo dei valori di ritorno dal
109800940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
109900940314    >C*    proposte, normalmente sempre valide, ma modificabili
110000940314    >C*    per situazioni particolari >>
110100940223     C* 1 = F3
110200940314    >C     RET002        WHENEQ    '1'
110300940224     C                   MOVE      *ON           $ESCI
110400940223     C                   MOVE      *ON           $FINE
110500940223     C* 2 = F12
110600940314    >C     RET002        WHENEQ    '2'
110700940223     C                   MOVE      *ON           $ESCI
110800940223     C*
1109009402231-   C                   ENDSL
111000940223     C*
111100940223     C* operazione eseguite dal pgm chiamato
111200940223     C*
1113009402231    C                   SELECT
111400940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
111500940314    >C     OPR002        WHENEQ    '1'
111600940223     C                   MOVE      *ON           WINZS1
111700940223     C*
1118009402231-   C                   ENDSL
111900940223     C*
112000940223     C* funzione non eseguibile per errore :
112100940223     C*
1122009402231    C                   SELECT
112300940223     C* 1 = funzione richiamata chiusa in errore
112400940316    >C*  eventualmente gestire altri codici di errore
112500940314    >C     ERR002        WHENEQ    '1'
112600940223     C                   MOVE      *ON           $ESCI
112700940223     C                   SETON                                        5299
112800940223     C*
1129009402231-   C                   ENDSL
113000940223     C*
113100940223     C                   ENDSR
113200940223     C/EJECT
113300940131     C************************************************************
113400940131     C* OPERAZIONI INIZIALI
113500940131     C************************************************************
113600940131     C     *INZSR        BEGSR
113700940131     C*
113800940127     C* Reperimento parametri
113900940127     C*
114000940117     C     *ENTRY        PLIST
114100940117     C                   PARM                    KPJBA
114200940714     C*
114300940223     C* Reperimento opzioni
114400940127     C*
114500940128     C                   EXSR      REPOPZ
114600940506     C*
114700940506     C* Reperimento tasti di funzione
114800940506     C*
114900940506     C                   EXSR      REPKEY
115000940506     C*
115100940607$007 C                   MOVE      'YP3'         XTBCOD
115200010406X0113C*
115300010406  "  C* reperisco le dimensioni della variabile del titolo
115400010406X0113C                   EVAL      LENTIT = %Size(C1Tit)
115500940127     C*
115600940117     C                   ENDSR
115700940225     C/EJECT
115800941103      ************************************************************
115900941103      * Reperimento dati società
116000941103      ************************************************************
116100941103     C     REPSOC        BEGSR
116200941103      *
116300970214     C                   CALLB     'XSOC'
116400941103     C                   PARM                    TIPXSC            6
116500941103     C                   PARM                    SOCXSC            3
116600941103     C                   PARM                    CDSXSC            9 0
116700941103     C                   PARM                    MODXSC            3
116800941103     C                   PARM      *BLANKS       RTNXSC            1
116900941103     C                   PARM                    XSOCDS
117000941103     C                   PARM                    KPJBA
117100941103      *
117200941103     C                   ENDSR
117300940223     C************************************************************
117400940223     C* INIZIALIZZAZIONE VARIABILI
117500940223     C************************************************************
117600940223     C     INZVAR        BEGSR
117700970926     C*
117800970926     C* Dati da PGM chiamante
117900970926     C*
118000970926     C                   MOVEL     KPJBU         XTABDS
118100951124     C*
118200951124     C* Reperimento codice lingua e codice azienda
118300951124     C*
118400951124     C                   MOVEL     'SOC001'      TIPXSC
118500951124     C                   EXSR      REPSOC
118600951124     C     RTNXSC        IFNE      '1'
118700951124     C                   MOVEL     XSOCDS        SOC001
118800951124     C                   ELSE
118900951124     C                   MOVE      *ON           $FINE
119000951124     C* fine programma
119100951124     C                   EXSR      ENDPGM
119200951124     C                   ENDIF
119300951124     C* Lingua e codice azienda
119400970708     C                   MOVE      XSCLIN        XtaLin
119500970708     C                   MOVE      XSCSOC        XtaAzi
119600940715     C*
119700940715     C                   RESET                   $FC2
119800940715     C*
119900940715$021 C                   CLOSE     ANGYP3D
120000940715$021 C                   OPEN      ANGYP3D
120100940506     C*
120200940506     C* Definizione delle opzioni utilizzabili a pgm
120300940506     C*
120400940506     C                   EXSR      DEFOPZ
120500940718     C* Pulizia campi e indicatori
120600940718     C                   CLEAR                   W1POS
120700940718     C                   MOVE      *ALL'0'       IN4049           10
120800940718     C                   MOVEA     IN4049        *IN(40)
120900940718     C                   CLEAR                   S1OPZ
121000940223     C* Variabili per gestione videate
121100940223     C*
121200940223     C                   MOVE      *OFF          $FINE
121300940223     C                   MOVE      *OFF          $INZS1
121400940223     C                   MOVE      *OFF          $EFILE
121500940223     C                   MOVE      *OFF          $ESCI
121600940223     C                   MOVE      *OFF          $RCDOK
121700940509     C                   MOVE      *HIVAL        $LASTV
121800940506     C                   Z-ADD     0             $ULKS1            3 0
121900940223     C*
122000940318     C                   MOVE      *ON           $INZS1
122100940223     C                   MOVE      'S1'          $GEST
122200940223     C*
122300940223     C* Variabili appoggio
122400940223     C*
122500940223     C                   Z-ADD     SFLPAG        WPAG
122600940223     C*
122700940223     C* Valorizzazione campi univoci testate
122800940223     C*
122900940607     C*                    CLEART1
123000940607     C*                    MOVELKNSIF     NOMSIF
123100940607     C*                    MOVELRAGUTE    NOMDIT
123200940607     C*                    MOVELDSPGM     SIGLA
123300940607     C*                    MOVELPROGR     NOMPGM
123400940223     C*
123500940608     c* posizione window
123600010406X0113c* modificata la seguente riga
123700040617$012 C                   Z-ADD     66            PCOL              2 0
123800940613     C                   Z-ADD     09            R
123900940608     C     80            SUB       PCOL          C
124000940608     C                   SUB       3             C
124100970708     C     XtaOpz        IFNE      '  '
124200970708     C     XtaRig        IFGT      11
124300940608     C                   Z-ADD     1             R
124400940608     C                   ENDIF
124500940608     C*
124600970708     C     XtaCol        IFGT      40
124700940608     C                   Z-ADD     2             C
124800940608     C                   ENDIF
124900940608     C                   ENDIF
125000940616     C*
125100970708     C     XtaOpz        IFNE      '01'
125200940616$019 C                   DO        00            I                 3 0
125300940721$022 C*                  MOVEL     LIN(I)        TBTFLD
125400940721$022 C*                  CLEAR                   TBTKEY
125500940721$022 C*                  CLEAR                   TBTAZI
125600940721$022 C*                  CLEAR                   TBTLIN
125700940721$007 C                   MOVE      'YP3'         TBTCOD            3
125800940721$022 C*    K5TBT1        CHAIN     ANTBT01L                           21
125900940721$022 C*    *IN21         IFEQ      *ON
126000940721$022 C*                  Z-ADD     LLUN(I)       TBTLUN
126100940721$022 C*                  Z-ADD     LPOS(I)       TBTPOS
126200940721$022 C*                  MOVEL     LIND(I)       TBTDES
126300940721$022 C*                  WRITE     ANTBT000
126400940721$022 C*                  ENDIF
126500940616     C                   ENDDO
126600940616     C                   ENDIF
126700940608     C*
126800940722     c*
126900940722     c* Se richiamato con OPZ 11  Non controlla abilitazione all'aziend
127000970708     C     XtaOpz        IFEQ      '11'
127100940722     C                   MOVE      'N'           XTBGAB
127200940722     C                   ELSE
127300940722     C                   MOVE      'S'           XTBGAB
127400940722     C                   ENDIF
127500940223     C                   ENDSR
127600940223     C/EJECT
127700940131     C************************************************************
127800940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
127900940131     C************************************************************
128000940225     C     DEFOPZ        BEGSR
128100940131     C*
128200940225     C* Questa routine deve valorizzare la schiera $FL1
128300940225     C* con S o N a seconda della utilizzabilità o meno
128400940225     C* dell'opzione correlata
128500940225     C*
128600940714     C* Inizialmente si considerano tutte le opzioni come non valide
128700940728     C     1             DO        14            X
128800940714     C                   MOVE      'N'           $FL1(X)
128900940225     C                   END
129000940225     C*
129100940225     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
129200940714     C* rendo valide alcune opzioni
129300940714     C* i tasti invece li disattivo
129400940225     C                   SELECT
129500940714     C*
129600940714     C* Se pgm richiamato in RICERCA attivo :
129700940714     C*
129800970708    >C     XtaOpz        WHENEQ    '01'
129900940714     C* F3=Fine
130000940714     C                   Z-ADD     3             X
130100940720     C                   MOVE      '0'           $FC2(X)
130200940714     C* F10=Immissione
130300940714     C                   Z-ADD     10            X
130400940919     C                   MOVE      '1'           $FC2(X)
130500940714     C* 1=Selezione
130600940714     C                   Z-ADD     1             X
130700940714     C     01            LOOKUP    $OP1(X)                                21
130800940714     C     *IN21         IFEQ      *ON
130900940714     C                   MOVE      'S'           $FL1(X)
131000940714     C                   ENDIF
131100940729     C* 2=Modifica
131200940729     C                   Z-ADD     1             X
131300940729     C     02            LOOKUP    $OP1(X)                                21
131400940729     C     *IN21         IFEQ      *ON
131500940729     C                   MOVE      'S'           $FL1(X)
131600940729     C                   ENDIF
131700940729     C* 3=Copia
131800940729     C                   Z-ADD     1             X
131900940729     C     03            LOOKUP    $OP1(X)                                21
132000940729     C     *IN21         IFEQ      *ON
132100940729     C                   MOVE      'S'           $FL1(X)
132200940729     C                   ENDIF
132300940729     C* 4=Annullamento
132400940729     C                   Z-ADD     1             X
132500940729     C     04            LOOKUP    $OP1(X)                                21
132600940729     C     *IN21         IFEQ      *ON
132700940729     C                   MOVE      'S'           $FL1(X)
132800940729     C                   ENDIF
132900940714     C* 5=Visualizzazione
133000940714     C                   Z-ADD     1             X
133100940714     C     05            LOOKUP    $OP1(X)                                21
133200940714     C     *IN21         IFEQ      *ON
133300940714     C                   MOVE      'S'           $FL1(X)
133400940714     C                   ENDIF
133500940728     C*14=Commenti
133600940831     C**                   Z-ADD1         X
133700940831     C**         14        LOKUP$OP1,X                   21
133800940831     C**         *IN21     IFEQ *ON
133900940831     C**                   MOVE 'S'       $FL1,X
134000940831     C**                   ENDIF
134100940714     C*
134200940714     C* Se pgm richiamato in INTERROGAZIONE attivo :
134300940714     C*
134400970708    >C     XtaOpz        WHENEQ    '05'
134500940714     C* F10=Immissione
134600940714     C                   Z-ADD     10            X
134700940720     C                   MOVE      '0'           $FC2(X)
134800940714     C* F3=Fine
134900940714     C                   Z-ADD     3             X
135000940720     C                   MOVE      '0'           $FC2(X)
135100940714     C* 5=Visualizzazione
135200940225     C                   Z-ADD     1             X
135300940714     C     05            LOOKUP    $OP1(X)                                21
135400940225     C     *IN21         IFEQ      *ON
135500940714     C                   MOVE      'S'           $FL1(X)
135600940225     C                   ENDIF
135700940728     C*14=Commenti
135800940831     C**                   Z-ADD1         X
135900940831     C**         14        LOKUP$OP1,X                   21
136000940831     C**         *IN21     IFEQ *ON
136100940831     C**                   MOVE 'S'       $FL1,X
136200940831     C**                   ENDIF
136300940831     C*
136400940714     C* Se pgm richiamato in ABILITAZIONE attivo :
136500940714     C*
136600970708    >C     XtaOpz        WHENEQ    '11'
136700940714     C* F10=Immissione
136800940714     C                   Z-ADD     10            X
136900940720     C                   MOVE      '0'           $FC2(X)
137000940714     C* F3=Fine
137100940714     C                   Z-ADD     3             X
137200940720     C                   MOVE      '0'           $FC2(X)
137300940714     C* 5=Visualizzazione
137400940714     C                   Z-ADD     1             X
137500940714     C     05            LOOKUP    $OP1(X)                                21
137600940714     C     *IN21         IFEQ      *ON
137700940714     C                   MOVE      'S'           $FL1(X)
137800940714     C                   ENDIF
137900940714     C* 11=Abilitazione
138000940714     C                   Z-ADD     1             X
138100940714     C     11            LOOKUP    $OP1(X)                                21
138200940714     C     *IN21         IFEQ      *ON
138300940714     C                   MOVE      'S'           $FL1(X)
138400940714     C                   ENDIF
138500940714     C* 12=Disabilita
138600940714     C                   Z-ADD     1             X
138700940714     C     12            LOOKUP    $OP1(X)                                21
138800940714     C     *IN21         IFEQ      *ON
138900940714     C                   MOVE      'S'           $FL1(X)
139000940714     C                   ENDIF
139100940714     C*
139200940714     C* Se pgm richiamato in GESTIONE  attivo :
139300940714     C*
139400970708    >C     XtaOpz        WHENEQ    '02'
139500940714     C* F3=Fine
139600940714     C                   Z-ADD     3             X
139700940720     C                   MOVE      '0'           $FC2(X)
139800940714     C* 2=Modifica
139900940714     C                   Z-ADD     1             X
140000940714     C     02            LOOKUP    $OP1(X)                                21
140100940714     C     *IN21         IFEQ      *ON
140200940714     C                   MOVE      'S'           $FL1(X)
140300940714     C                   ENDIF
140400940714     C* 3=Copia
140500940714     C                   Z-ADD     1             X
140600940714     C     03            LOOKUP    $OP1(X)                                21
140700940714     C     *IN21         IFEQ      *ON
140800940714     C                   MOVE      'S'           $FL1(X)
140900940714     C                   ENDIF
141000940714     C* 4=Annullamento
141100940714     C                   Z-ADD     1             X
141200940714     C     04            LOOKUP    $OP1(X)                                21
141300940714     C     *IN21         IFEQ      *ON
141400940714     C                   MOVE      'S'           $FL1(X)
141500940714     C                   ENDIF
141600940714     C* 5=Visualizzazione
141700940714     C                   Z-ADD     1             X
141800940714     C     05            LOOKUP    $OP1(X)                                21
141900940714     C     *IN21         IFEQ      *ON
142000940714     C                   MOVE      'S'           $FL1(X)
142100940714     C                   ENDIF
142200940729     C*14=Commenti
142300940831     C**                   Z-ADD1         X
142400940831     C**         14        LOKUP$OP1,X                   21
142500940831     C**         *IN21     IFEQ *ON
142600940831     C**                   MOVE 'S'       $FL1,X
142700940831     C**                   ENDIF
142800940225     C*
142900940225     C                   OTHER
143000940714     C* Se pgm non richiamato, attivo TUTTO  :
143100940714     C* 2=Modifica
143200940714     C                   Z-ADD     1             X
143300940714     C     02            LOOKUP    $OP1(X)                                21
143400940714     C     *IN21         IFEQ      *ON
143500940714     C                   MOVE      'S'           $FL1(X)
143600940714     C                   ENDIF
143700940714     C* 3=Copia
143800940714     C                   Z-ADD     1             X
143900940714     C     03            LOOKUP    $OP1(X)                                21
144000940714     C     *IN21         IFEQ      *ON
144100940714     C                   MOVE      'S'           $FL1(X)
144200940714     C                   ENDIF
144300940714     C* 4=Annullamento
144400940714     C                   Z-ADD     1             X
144500940714     C     04            LOOKUP    $OP1(X)                                21
144600940714     C     *IN21         IFEQ      *ON
144700940714     C                   MOVE      'S'           $FL1(X)
144800940714     C                   ENDIF
144900940714     C* 5=Visualizzazione
145000940714     C                   Z-ADD     1             X
145100940714     C     05            LOOKUP    $OP1(X)                                21
145200940714     C     *IN21         IFEQ      *ON
145300940714     C                   MOVE      'S'           $FL1(X)
145400940714     C                   ENDIF
145500940714     C* 11=Abilitazione
145600940714     C                   Z-ADD     1             X
145700940714     C     11            LOOKUP    $OP1(X)                                21
145800940714     C     *IN21         IFEQ      *ON
145900940714     C                   MOVE      'S'           $FL1(X)
146000940714     C                   ENDIF
146100940714     C* 12=Disabilita
146200940714     C                   Z-ADD     1             X
146300940714     C     12            LOOKUP    $OP1(X)                                21
146400940714     C     *IN21         IFEQ      *ON
146500940714     C                   MOVE      'S'           $FL1(X)
146600940714     C                   ENDIF
146700940728     C*14=Commenti
146800940831     C**                   Z-ADD1         X
146900940831     C**         14        LOKUP$OP1,X                   21
147000940831     C**         *IN21     IFEQ *ON
147100940831     C**                   MOVE 'S'       $FL1,X
147200940831     C**                   ENDIF
147300940225     C* poi, disattivo a seconda delle autorizzazioni :
147400940314    >C*********  AUTXXX    IFEQ 'N'
147500940314    >C*********            Z-ADD1         X
147600940314    >C*********  'XX'      LOKUP$OP1,X                   21
147700940314    >C*********  *IN21     IFEQ *ON
147800940314    >C*********            MOVE 'N'       $FL1,X
147900940314    >C*********            ENDIF
148000940314    >C*********            ENDIF
148100940225     C*
148200940225     C                   ENDSL
148300940225     C*
148400940225     C                   ENDSR
148500940225     C/EJECT
148600940225     C************************************************************
148700940225     C* REPERIMENTO OPZIONI
148800940225     C************************************************************
148900940225     C     REPOPZ        BEGSR
149000940225     C*
149100940314     C     1             DO        $DIMOP        X
149200940128     C*
149300940223     C     $MS1(X)       IFNE      *BLANK
149400940128     C*
149500940223     C                   MOVEL     $MS1(X)       IDMSG
149600970214     C                   CALLB     'XRTVM'
149700940201     C                   PARM                    IDMSG            27
149800940201     C                   PARM                    TXTMSG          100
149900940201     C                   PARM                    ERRMSG            1
150000940128     C*
150100940128     C     ERRMSG        IFNE      *ON
150200940223     C                   MOVEL     TXTMSG        $TX1(X)
150300940128     C                   ELSE
150400940223     C                   MOVEL     *ALL'?'       $TX1(X)
150500940128     C                   END
150600940128     C*
150700940128     C                   END
150800940128     C*
150900940128     C                   END
151000940128     C*
151100940128     C                   ENDSR
151200940506     C/EJECT
151300940506     C************************************************************
151400940506     C* REPERIMENTO TASTI DI FUNZIONE
151500940506     C************************************************************
151600940506     C     REPKEY        BEGSR
151700940506     C*
151800940506     C                   MOVEA     $FM2          $FM
151900940506     C                   EXSR      REPKKK
152000940506     C                   MOVEA     $FX           $FX2
152100940506     C                   ENDSR
152200940506     C/EJECT
152300940506     C************************************************************
152400940506     C* REPERIMENTO TASTI DI FUNZIONE
152500940506     C************************************************************
152600940506     C     REPKKK        BEGSR
152700940506     C*
152800940506     C                   CLEAR                   $FX
152900940506     C     1             DO        24            X
153000940506     C*
153100940506     C     $FM(X)        IFNE      *BLANK
153200940506     C*
153300940506     C                   MOVEL     $FM(X)        IDMSG
153400970214     C                   CALLB     'XRTVM'
153500940506     C                   PARM                    IDMSG            27
153600940506     C                   PARM                    TXTMSG          100
153700940506     C                   PARM                    ERRMSG            1
153800940506     C*
153900940506     C     ERRMSG        IFNE      *ON
154000940506     C                   MOVEL     TXTMSG        $FX(X)
154100940506     C                   ELSE
154200940506     C                   MOVEL     *ALL' '       $FX(X)
154300940506     C                   END
154400940506     C*
154500940506     C                   END
154600940506     C*
154700940506     C                   END
154800940506     C*
154900940506     C                   ENDSR
155000940223     C/EJECT
155100940128     C************************************************************
155200940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
155300940128     C************************************************************
155400940223     C     DEFCAM        BEGSR
155500940128     C*
155600940223     C* Variabili per gestione videate
155700940223     C*
155800940223     C                   MOVE      *BLANK        $GEST             2
155900940223     C                   MOVE      *BLANK        $FINE             1
156000940223     C                   MOVE      *BLANK        $INZS1            1
156100940223     C                   MOVE      *BLANK        $LASTG            2
156200940223     C                   MOVE      *BLANK        $LASTV            2
156300940223     C                   MOVE      *BLANK        $EFILE            1
156400940223     C                   MOVE      *BLANK        $ESCI             1
156500940223     C                   MOVE      *BLANK        $RCDOK            1
156600940223     C*
156700940314     C* Variabili appoggio sempre presenti in tutte le anagrafiche
156800940223     C*
156900940223     C     *LIKE         DEFINE    C1RCD         S1NRR
157000940223     C     *LIKE         DEFINE    C1NRR         WSFL
157100940223     C     *LIKE         DEFINE    C1RCD         WMAX
157200940223     C     *LIKE         DEFINE    C1RCD         WPAG
157300940223     C     *LIKE         DEFINE    $INZS1        WINZS1
157400940223     C                   MOVE      *BLANK        WWGRP             3
157500940223     C                   Z-ADD     0             CURR              2 0
157600940223     C                   Z-ADD     0             CURC              2 0
157700940223     C                   MOVE      *BLANK        WIN              99
157800940314     C     *LIKE         DEFINE    C1POS         W1POS
157900940620     C*
158000940721$022 C*    K5TBT1        KLIST
158100940721$022 C*                  KFLD                    TBTAZI
158200940721$022 C*                  KFLD                    TBTCOD
158300940721$022 C*                  KFLD                    TBTKEY
158400940721$022 C*                  KFLD                    TBTLIN
158500940721$022 C*                  KFLD                    TBTFLD
158600940714     C*
158700940714     C     KABL          KLIST
158800940714     C                   KFLD                    ABLCOD
158900940714     C                   KFLD                    ABLKEY
159000940714     C                   KFLD                    ABLAZI
159100940314     C*
159200040617     C*
159300040617     C     KEYCLN        KLIST
159400040617     C                   KFLD                    xscSOC
159500040617     C                   KFLD                    s1cod
159600940314     C*
159700940223     C* Indici
159800940223     C*
159900940223     C                   Z-ADD     0             X                 3 0
160000940223     C                   Z-ADD     0             Y                 3 0
160100940127     C*
160200940117     C                   ENDSR
160300940223     C/EJECT
160400940616**
160500940225PROMSG    *LIBL     OPZ0001
160600940225PROMSG    *LIBL     OPZ0002
160700940131PROMSG    *LIBL     OPZ0003
160800940131PROMSG    *LIBL     OPZ0004
160900940131PROMSG    *LIBL     OPZ0005
161000940224PROMSG    *LIBL     OPZ0006
161100940224PROMSG    *LIBL     OPZ0007
161200940224PROMSG    *LIBL     OPZ0008
161300940224PROMSG    *LIBL     OPZ0009
161400970716PROMSG    *LIBL     OPZ0010
161500970716PROMSG    *LIBL     OPZ0013
161600970716PROMSG    *LIBL     OPZ0012
161700970716PROMSG    *LIBL     OPZ0011
161800970716PROMSG    *LIBL     OPZ0014
161900940201**
16200094022501S      <Selezione
16210094022502S      <Modifica
16220094022503S      <Copia
16230094022504S      <Annullamento
16240094022505S      <Visualizzazione
16250094060706N      <prova
16260094060707N      <prova
16270094060708N      <prova
16280094060709N      <prova
16290094072910S      <Invia
16300094071411S      <Abilita gruppo
16310094071412S      <Disabilita
16320094072813N      <PROVA
16330094083114N      <Commenti
163400940506** TASTI DI FUNZIONE  S1
163500940506PROMSG    *LIBL     KEY0001  01
163600940506PROMSG    *LIBL     KEY0002  02
163700940506PROMSG    *LIBL     KEY0003  03
163800940506PROMSG    *LIBL     KEY0004  04
163900940506PROMSG    *LIBL     KEY0005  05
164000940506PROMSG    *LIBL     KEY0006  06
164100940506PROMSG    *LIBL     KEY0007  07
164200940506PROMSG    *LIBL     KEY0008  08
164300940506PROMSG    *LIBL     KEY0009  09
164400970716PROMSG    *LIBL     KEY0010  10
164500970716PROMSG    *LIBL     KEY0011  11
164600970716PROMSG    *LIBL     KEY0012  12
164700970716PROMSG    *LIBL     KEY0013  13
164800970716PROMSG    *LIBL     KEY0014  14
164900970716PROMSG    *LIBL     KEY0015  15
165000970716PROMSG    *LIBL     KEY0016  16
165100970716PROMSG    *LIBL     KEY0017  17
165200970716PROMSG    *LIBL     KEY0018  18
165300970716PROMSG    *LIBL     KEY0019  19
165400940506PROMSG    *LIBL     KEY0020  20
165500940506PROMSG    *LIBL     KEY0021  21
165600940506PROMSG    *LIBL     KEY0022  22
165700940506PROMSG    *LIBL     KEY0023  23
165800940506PROMSG    *LIBL     KEY0024  24
165900940506**  ABILITAZIONE S1
1660009410140N  01
1661009407200N  02
1662009407201N  03
1663009407200N  04
1664009407200N  05
1665009407200N  06
1666009407200N  07
1667009407200N  08
1668009407200N  09
1669009407201N  10
1670009407200N  11
1671009407201N  12
1672009407200N  13
1673009407200N  14
1674009407200N  15
1675009407200N  16
1676009407200N  17
1677009407200N  18
1678009407200N  19
1679009407200N  20
1680009407200N  21
1681009407200N  22
1682009505020N  23
1683009407200N  24
