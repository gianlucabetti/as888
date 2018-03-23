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
003900940715$001 FANGY07D   CF   E             WORKSTN USROPN
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
014200940607$003 D EXTDS         E DS                  EXTNAME(ANGY07DS)
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
018900940607$015 I              S1DES                       DESY07
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
025700940607$018 C                   PARM      51            $L01              3 0
025800940607     C                   PARM      0             $L02              3 0
025900940223     C*
026000940131     C                   ENDSR
026100940506     C/EJECT
026200940506     C************************************************************
026300940506     C* RIEMPIMENTO TASTI FUNZIONE
026400940506     C************************************************************
026500940506     C     RIEKEY        BEGSR
026600940506     C*
026700940506     C* Input :
026800940506     C* - $FX = schiera con tutti i testi disponibili
026900940506     C* - $FC = schiere con i flag di validità associati ai testi
027000940506     C* - $FR = schiere con i flag di REVERSE IMAGE
027100940506     C* - $ULK = ultimo testo caricato
027200940506     C* Output :
027300940506     C* - $ULK
027400940506     c* - $KEY1= 1° riga di tasti funzione
027500940506     c* - $KEY2= 2° riga di tasti funzione
027600940506     C*
027700970214     C                   CALLB     'XKEYMSG'
027800940506     C* input
027900940506     C                   PARM                    $FX
028000940506     C                   PARM                    $FC
028100940506     C                   PARM                    $FR
028200940506     C                   PARM                    $ULK              3 0
028300940506     C* output
028400940506     C                   PARM                    $KEY1            79
028500940506     C                   PARM                    $KEY2            79
028600940607     C* input
028700940607$018 C                   PARM      51            $L01              3 0
028800941004$018 C                   PARM      51            $L02              3 0
028900940506     C*
029000940506     C                   ENDSR
029100940224     C/EJECT
029200940224     C************************************************************
029300940224     C* GESTIONE EMISSIONE TASTI FUNZIONE
029400940224     C************************************************************
029500940224     C     GESPIE        BEGSR
029600940224     C*
029700940224     C                   SELECT
029800940506     C* Lista
029900940506     C     $GEST         WHENEQ    'S1'
030000940506     C     $ULKS1        IFEQ      0
030100940506     C                   EXSR      F24S1
030200940506     C                   ELSE
030300940607     C                   WRITE     Z1
030400940506     C                   ENDIF
030500940506     C                   ENDSL
030600940506     C*
030700940224     C                   ENDSR
030800940506     C/EJECT
030900940506     C************************************************************
031000940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA S1
031100940506     C************************************************************
031200940506     C     CTRKS1        BEGSR
031300940506     C                   SETOFF                                       99
031400940506     C                   MOVEA     $FC2          $FC
031500940506     C                   EXSR      CTRKEY
031600940506     C     *IN99         IFEQ      *ON
031700940506     C                   SETON                                        97
031800940506     C                   ENDIF
031900940607     C*
032000940506     C                   ENDSR
032100940506     C/EJECT
032200940506     C************************************************************
032300940506     C* CONTROLLO ABILITAZIONE TASTI
032400940509     C* INPUT
032500940509     c* $TASTO =Tasto funzione premuto
032600940509     C* $FC = Tasti abilitati
032700940509     C* OUTPUT
032800940509     C* IN99 = ERrore
032900940506     C************************************************************
033000940506     C     CTRKEY        BEGSR
033100940506     C                   SELECT
033200940506     C     $TASTO        WHENEQ    F01
033300940720     C     $FC(1)        IFEQ      '0'
033400940506     C                   SETON                                        99
033500940506     C                   ENDIF
033600940506     C     $TASTO        WHENEQ    F02
033700940720     C     $FC(2)        IFEQ      '0'
033800940506     C                   SETON                                        99
033900940506     C                   ENDIF
034000940506     C     $TASTO        WHENEQ    F03
034100940720     C     $FC(3)        IFEQ      '0'
034200940506     C                   SETON                                        99
034300940506     C                   ENDIF
034400940506     C     $TASTO        WHENEQ    F04
034500940720     C     $FC(4)        IFEQ      '0'
034600940506     C                   SETON                                        99
034700940506     C                   ENDIF
034800940506     C     $TASTO        WHENEQ    F05
034900940720     C     $FC(5)        IFEQ      '0'
035000940506     C                   SETON                                        99
035100940506     C                   ENDIF
035200940506     C     $TASTO        WHENEQ    F06
035300940720     C     $FC(6)        IFEQ      '0'
035400940506     C                   SETON                                        99
035500940506     C                   ENDIF
035600940506     C     $TASTO        WHENEQ    F07
035700940720     C     $FC(7)        IFEQ      '0'
035800940506     C                   SETON                                        99
035900940506     C                   ENDIF
036000940506     C     $TASTO        WHENEQ    F08
036100940720     C     $FC(8)        IFEQ      '0'
036200940506     C                   SETON                                        99
036300940506     C                   ENDIF
036400940506     C     $TASTO        WHENEQ    F09
036500940720     C     $FC(09)       IFEQ      '0'
036600940506     C                   SETON                                        99
036700940506     C                   ENDIF
036800940506     C     $TASTO        WHENEQ    F10
036900940720     C     $FC(10)       IFEQ      '0'
037000940506     C                   SETON                                        99
037100940506     C                   ENDIF
037200940506     C     $TASTO        WHENEQ    F11
037300940720     C     $FC(11)       IFEQ      '0'
037400940506     C                   SETON                                        99
037500940506     C                   ENDIF
037600940506     C     $TASTO        WHENEQ    F12
037700940720     C     $FC(12)       IFEQ      '0'
037800940506     C                   SETON                                        99
037900940506     C                   ENDIF
038000940506     C     $TASTO        WHENEQ    F13
038100940720     C     $FC(13)       IFEQ      '0'
038200940506     C                   SETON                                        99
038300940506     C                   ENDIF
038400940506     C     $TASTO        WHENEQ    F14
038500940720     C     $FC(14)       IFEQ      '0'
038600940506     C                   SETON                                        99
038700940506     C                   ENDIF
038800940506     C     $TASTO        WHENEQ    F15
038900940720     C     $FC(15)       IFEQ      '0'
039000940506     C                   SETON                                        99
039100940506     C                   ENDIF
039200940506     C     $TASTO        WHENEQ    F16
039300940720     C     $FC(16)       IFEQ      '0'
039400940506     C                   SETON                                        99
039500940506     C                   ENDIF
039600940506     C     $TASTO        WHENEQ    F17
039700940720     C     $FC(17)       IFEQ      '0'
039800940506     C                   SETON                                        99
039900940506     C                   ENDIF
040000940506     C     $TASTO        WHENEQ    F18
040100940720     C     $FC(18)       IFEQ      '0'
040200940506     C                   SETON                                        99
040300940506     C                   ENDIF
040400940506     C     $TASTO        WHENEQ    F19
040500940720     C     $FC(19)       IFEQ      '0'
040600940506     C                   SETON                                        99
040700940506     C                   ENDIF
040800940506     C     $TASTO        WHENEQ    F20
040900940720     C     $FC(20)       IFEQ      '0'
041000940506     C                   SETON                                        99
041100940506     C                   ENDIF
041200940506     C     $TASTO        WHENEQ    F21
041300940720     C     $FC(21)       IFEQ      '0'
041400940506     C                   SETON                                        99
041500940506     C                   ENDIF
041600940506     C     $TASTO        WHENEQ    F22
041700940720     C     $FC(22)       IFEQ      '0'
041800940506     C                   SETON                                        99
041900940506     C                   ENDIF
042000940506     C     $TASTO        WHENEQ    F23
042100940720     C     $FC(23)       IFEQ      '0'
042200940506     C                   SETON                                        99
042300940506     C                   ENDIF
042400940506     C     $TASTO        WHENEQ    F24
042500940720     C     $FC(24)       IFEQ      '0'
042600940506     C                   SETON                                        99
042700940506     C                   ENDIF
042800940506     C                   ENDSL
042900940506     C                   ENDSR
043000940224     C/EJECT
043100940127     C************************************************************
043200940131     C* GESTIONE LISTA
043300940127     C************************************************************
043400940127     C     GESS1         BEGSR
043500940223     C* inizializzazione videata
043600940223     C     $INZS1        IFEQ      *ON
043700940127     C                   EXSR      INZS1
043800940223     C                   MOVE      *OFF          $INZS1
043900940127     C                   ENDIF
044000940223     C* emissione piede videata se proveniente da altra
044100940223     C* salvataggio valore formato di provenienza
044200940202     C     $LASTV        IFNE      'S1'
044300940607!!   C*                    WRITEFMTT1
044400940224     C                   EXSR      GESPIE
044500940202     C                   MOVE      $LASTV        $LASTG
044600940202     C                   MOVE      'S1'          $LASTV
044700940202     C                   END
044800940223     C*
044900940223     C     WMAX          IFEQ      0
045000940607     C                   WRITE     D1
045100940211     C                   ENDIF
045200940729     C                   MOVE      '1'           WIN727            1
045300940127     C*
045400940607     C                   EXFMT     C1
045500940204     C     C1NRR         IFNE      0
045600940204     C                   Z-ADD     C1NRR         WSFL
045700940204     C                   ENDIF
045800940127     C                   Z-ADD     SFLNRR        C1RCD
045900940506     C*
046000940506     C* controllo abilitazione tasti funzione
046100940506     C*
046200940506     C                   EXSR      CTRKS1
046300940506     C     *IN99         IFEQ      *OFF
046400940506     C*
046500940127     C*
0466009401271    C                   SELECT
046700940127     C* F3=Fine
046800940506     C     $TASTO        WHENEQ    F03
046900940309     C                   EXSR      F03S1
047000940131     C* F10=Immissione
047100940506     C     $TASTO        WHENEQ    F10
047200940309     C                   EXSR      F10S1
047300940131     C* F12=Ritorno
047400940506     C     $TASTO        WHENEQ    F12
047500940309     C                   EXSR      F12S1
047600940224     C* F23=Altre opzioni
047700940506     C     $TASTO        WHENEQ    F23
047800940309     C                   EXSR      F23S1
047900940224     C* F24=Altri tasti
048000940506     C     $TASTO        WHENEQ    F24
048100940309     C                   EXSR      F24S1
048200940127     C* ROLL-UP
048300940506     C     $TASTO        WHENEQ    ROLLUP
048400940127     C                   EXSR      ROLS1
048500940127     C*
0486009401271O   C                   OTHER
048700940127     C* CONTROLLO DATI
048800940131     C                   EXSR      CTRC1
048900940201     C     *IN99         IFEQ      *OFF
049000940131     C                   EXSR      CTRS1
049100940131     C                   END
0492009401271-   C                   ENDSL
049300940127     C*
049400940506     C                   ENDIF
049500940127     C                   ENDSR
049600940224     C/EJECT
049700940127     C************************************************************
049800940131     C* INIZIALIZZAZIONE LISTA
049900940127     C************************************************************
050000940127     C     INZS1         BEGSR
050100940302     C* pulizia SFL
050200940128     C                   SETOFF                                         3031
050300940607     C                   WRITE     C1
050400940128     C                   SETON                                          31
050500010406X0113C*
050600010406  "  C* reperimento titolo
050700010406  "  C                   move      xtbcod        WrkTbcod
050800010406  "  C                   eval      xtbkey      = *zero
050900010406  "  C                   move      xtbcod        xtbkey
051000010406  "  C                   eval      xtbazi      = xscsoc
051100010406  "  C                   eval      xtblin      = xsclin
051200010406  "  C                   eval      xtbcod      = 'AAA'
051300010406  "  C                   eval      xtbric      = '1'
051400010406  "  C                   eval      xtbalc      = '0'
051500010406  "  C                   CALLB     'XATB'
051600010406  "  C                   PARM                    XATBDS
051700010406  "  C                   if        xtberr <> *off
051800010406  "  C                   movel     *all'?'       WrkTit
051900010406  "  C                   else
052000010406  "  C                   movel     xtbuni        WrkTit
052100010406  "  C                   endif
052200010406  "  C*
052300010406  "  C* centratura
052400010406  "  C                   move      WrkTbcod      xtbcod
052500010406  "  C                   eval      WrkTit2=%trimr(Wrktit)+X'22'+xtbcod
052600010406  "  C                             +X'20'
052700010406  "  C     ' '           CHECKR    Wrktit2       I
052800010406  "  C                   call      'X71CENTRA'
052900010406  "  C                   parm                    Wrktit2
053000010406  "  C                   parm                    I
053100010406  "  C                   parm                    lentit
053200010406X0113C                   movel     Wrktit2       C1tit
053300940127     C*
053400940128     C* riempimento opzioni
053500940223     C                   MOVEA     $TX1          $TX
053600940223     C                   MOVEA     $FL1          $FL
053700940309     C                   Z-ADD     0             WCOZ
053800940225     C*  equivale al num.di elementi della schiera $TX1
053900940509     C                   Z-ADD     $DIMOP        WMXO
054000940128     C                   EXSR      RIEOPZ
054100950502     C                   Z-ADD     23            X
054200950502     C                   MOVE      FF23          $FC2(X)
054300940608     C                   MOVEL     WOPZ1         C1OPZ1
054400940608     C**                   MOVELWOPZ2     C1OPZ2
054500940225     C                   Z-ADD     WCOZ          C1COZ
054600940128     C*
054700940128     C* CARICAMENTO SFL A PAGINE
054800940128     C*
054900940201     C                   Z-ADD     0             S1NRR
055000940128     C                   Z-ADD     0             WMAX
055100940224     C*
055200940224     C* Posizionamento su file pilota
055300940302     C                   CLEAR                   C1POS
055400940316    >C                   EXSR      SETANA
055500940608    >C                   EXSR      REDANA
055600940302     C* ripristino i valori del posizionamento
055700940302     C                   MOVE      W1POS         C1POS
055800940131     C*
055900940131     C* Carico 1a pagina
056000940127     C                   EXSR      ROLS1
056100940201     C                   Z-ADD     SFLPAG        WPAG
056200940127     C*
056300940127     C                   ENDSR
056400940127     C/EJECT
056500940127     C************************************************************
056600940131     C* CARICAMENTO PAGINA LISTA
056700940127     C************************************************************
056800940127     C     ROLS1         BEGSR
056900940127     C*
057000940128     C                   SETOFF                                       32
057100940223     C                   Z-ADD     0             Y
057200940127     C                   Z-ADD     WMAX          S1NRR
057300940127     C*
057400940127     C* Leggo dal file anagrafico per caricare la lista
057500940127     C*
0576009401311    C     $EFILE        DOWEQ     *OFF
057700940201     C     Y             ANDLT     WPAG
057800940127     C*
057900940131     C                   EXSR      RIES1
058000940127     C*
058100940127     C                   ADD       1             S1NRR
058200940127     C                   ADD       1             Y
058300940714     C                   MOVE      $IN41         *IN41
058400940127     C*
058500940607     C                   WRITE     S1
058600940131     C*
058700940316    >C                   EXSR      REDANA
058800940128     C*
0589009401271-   C                   ENDDO
059000940127     C*
059100940223     C                   Z-ADD     S1NRR         WMAX                 30
059200940127     C*
059300940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
059400940127     C*
059500940201     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
059600940127     C                   MVR                     RESTO             3 0
059700940131     C     PAGINE        MULT      SFLPAG        C1RCD
0598009401271    C     RESTO         IFGT      0
059900940127     C                   ADD       1             C1RCD
0600009401271E   C                   ELSE
060100940407     C                   SUB       SFLPAG        C1RCD
060200940407     C                   ADD       1             C1RCD
0603009401271-   C                   ENDIF
060400940128     C*
060500940127     C                   ENDSR
060600940224     C/EJECT
060700940224     C************************************************************
060800940224     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
060900940224     C************************************************************
061000940607     C     SETANA        BEGSR
061100940224     C*
061200941004     C                   MOVE      *BLANKS       XTBKEY
061300970708     C                   MOVE      XtaAzi        XTBAZI
061400970708     C                   MOVE      XtaLin        XTBLIN
061500940608     C                   MOVE      '9'           XTBRIC
061600940607     C                   MOVE      '0'           XTBALC
061700950622     C                   MOVE      XSCCUT        XTBUTE
061800940607     C                   MOVE      '0'           XTBERR
061900941121     C**         W1POS     IFNE *BLANK
062000941004     C                   MOVE      *ZEROS        XTBKEY
062100940608     C                   MOVE      W1POS         XTBKEY
062200941121     C**                   END
062300970214     C                   CALLB     'XATB'
062400940607     C                   PARM                    XATBDS
062500940607     C*
062600940224     C*
062700940224     C                   ENDSR
062800940128     C/EJECT
062900940128     C************************************************************
063000940131     C* LETTURA RCD ARCHIVIO PILOTA
063100940128     C************************************************************
063200940607     C     REDANA        BEGSR
063300940128     C*
063400940131     C                   MOVEL     *OFF          $RCDOK
063500940131     C*
0636009401311    C     $EFILE        DOUEQ     *ON
063700940131     C     $RCDOK        OREQ      *ON
063800940131     C*
063900940607     C                   MOVE      '3'           XTBRIC
064000940607     C                   MOVE      '0'           XTBERR
064100970214     C                   CALLB     'XATB'
064200940607     C                   PARM                    XATBDS
064300940607     C*
064400940607     C     XTBERR        IFNE      '0'
064500940607     C                   MOVEL     *ON           $EFILE
064600010214     C                   ELSE
064700010214     C                   MOVEL     *OFF          $EFILE
064800940607     C                   ENDIF
064900940131     C*
0650009401312    C     $EFILE        IFEQ      *OFF
065100940607     C                   EXSR      SELANA
065200950502     C                   MOVE      XTBKEY        SAVKEY
065300950502     C                   MOVE      XTBGRU        SAVGRU
065400950502     C                   MOVE      XTBUNI        SAVUNI
065500970509     C                   MOVE      XTBUNT        SAVUNT
0656009401312-   C                   ENDIF
065700940131     C*
0658009401311-   C                   ENDDO
065900940131     C*
066000940607     C                   MOVE      $EFILE        *IN33
066100940131     C*
066200940131     C                   ENDSR
066300940224     C/EJECT
066400940224     C************************************************************
066500940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
066600940224     C************************************************************
066700940224     C     CLCPAG        BEGSR
066800940224     C* Input :
066900940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
067000940224     C*          cursore
067100940224     C* - SFLPAG = numero rcd per pagina sfl
067200940224     C* Output :
067300940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
067400940224     C*
067500940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
067600940224     C                   MVR                     RESTO             3 0
067700940224     C     RESTO         IFGT      0
067800940224     C                   ADD       1             PAGINE
067900940224     C                   ENDIF
068000940224     C     PAGINE        MULT      SFLPAG        WPAG
068100940224     C*
068200940224     C                   ENDSR
068300940131     C/EJECT
068400940131     C************************************************************
068500940131     C* SELEZIONI SULLA RIGA LETTA
068600940314     C* impostare a *OFF il campo $RCDOK per scartare un record che
068700940314     C* non rientra nei parametri di parzializzazione
068800940314     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
068900940314     C* superiore stabilito nei parametri
069000940314     C*
069100940131     C************************************************************
069200940607     C     SELANA        BEGSR
069300940131     C*
069400940202     C                   MOVEL     *ON           $RCDOK
069500940714     C* Se aperto il pgm con opzione 11
069600940714     C* carico solo i record di gruppo
069700970708     C     XtaOpz        IFEQ      '11'
069800940714     C     XTBGRU        ANDNE     'G'
069900940714     C                   MOVEL     *OFF          $RCDOK
070000940714     C                   END
070100940714     C* Carica in alta intensità solo i record
070200940714     C* abilitati
070300940714     C                   MOVE      '0'           $IN41
070400970708     C     XtaOpz        IFEQ      '11'
070500940714     C     XTBGRU        ANDEQ     'G'
070600940715     C     XTBAZB        ANDNE     'N'
070700940714     C                   MOVE      '1'           $IN41
070800940714     C                   END
070900940714     C*
071000940202     C                   ENDSR
071100940131     C/EJECT
071200940131     C************************************************************
071300940131     C* RIEMPIMENTO RIGA LISTA
071400940131     C************************************************************
071500940131     C     RIES1         BEGSR
071600950502     C*                    MOVE XTBKEY    S1COD
071700950502     C*                    MOVE XTBGRU    S1GRU
071800950502     C                   MOVEL     SAVUNI        EXTDS
071900950502     C                   MOVE      SAVKEY        S1COD
072000950502     C                   MOVE      SAVGRU        S1GRU
072100970512     C                   MOVE      SAVUNT        S1UNITA           8
072200981209$027 C
072300950502     C     *LIKE         DEFINE    XTBUNI        SAVUNI
072400950502     C     *LIKE         DEFINE    XTBKEY        SAVKEY
072500950502     C     *LIKE         DEFINE    XTBGRU        SAVGRU
072600970509     C     *LIKE         DEFINE    XTBUNT        SAVUNT
072700940131     C*
072800940131     C                   ENDSR
072900940309     C/EJECT
073000940309     C************************************************************
073100940309     C* GESTIONE F03 VIDEO S1
073200940309     C************************************************************
073300940309     C     F03S1         BEGSR
073400940309     C*
073500940309     C                   MOVE      *ON           $FINE
073600970708     C                   MOVE      '1'           XtaRet
073700940325     C* fine programma
073800940325     C                   EXSR      ENDPGM
073900940309     C*
074000940309     C                   ENDSR
074100940309     C/EJECT
074200940309     C************************************************************
074300940309     C* GESTIONE F10 VIDEO S1
074400940314     c* AGGIUNTA RECORD
074500940309     C************************************************************
074600940309     C     F10S1         BEGSR
074700940309     C*
074800940607     C                   RESET                   D002DS
074900940607     C                   MOVEL     '01'          OPZ002
075000970708     C                   MOVE      XtaLin        LIN002
075100970708     C                   MOVE      XtaAzi        AZI002
075200970512     C                   MOVE      S1UNITA       UNI002
075300940715     C                   MOVE      *BLANKS       KPJBU
075400940607     C                   MOVEL     D002DS        KPJBU
075500940608$004 C                   CALL      'ANGY07R2'
075600940309     C                   PARM                    KPJBA
075700940607     C                   MOVEL     KPJBU         D002DS
075800940309     C* ritorno da PGM gestione
075900940309     C                   EXSR      GESRET
076000940309     C     WINZS1        IFEQ      *ON
076100940309     C                   MOVE      *ON           $INZS1
076200940309     C* se esistevano già righe sul sfl
076300940309     C* calcolo pagina a cui deve posizionarsi
076400940309     C     WSFL          IFGT      0
076500940309     C                   EXSR      CLCPAG
076600940309     C* altrimenti carico solo la 1a pagina
076700940309     C                   ELSE
076800940309     C                   Z-ADD     SFLPAG        WPAG
076900940309     C                   ENDIF
077000940309     C                   ENDIF
077100940309     C*
077200940712     C                   MOVE      *HIVAL        $LASTV
077300940309     C                   ENDSR
077400940309     C/EJECT
077500940309     C************************************************************
077600940309     C* GESTIONE F12 VIDEO S1
077700940309     C************************************************************
077800940309     C     F12S1         BEGSR
077900940309     C*
078000970708     C                   MOVE      '2'           XtaRet
078100940309     C                   MOVE      *ON           $FINE
078200940309     C*
078300940309     C                   ENDSR
078400940309     C/EJECT
078500940309     C************************************************************
078600940309     C* GESTIONE F23 VIDEO S1
078700940309     C************************************************************
078800940309     C     F23S1         BEGSR
078900940309     C*
079000940309     C                   EXSR      RIEOPZ
079100950502     C                   Z-ADD     23            X
079200950502     C                   MOVE      FF23          $FC2(X)
079300940608     C                   MOVEL     WOPZ1         C1OPZ1
079400940608     C**                   MOVELWOPZ2     C1OPZ2
079500940309     C                   Z-ADD     WCOZ          C1COZ
079600940309     C*
079700940309     C                   ENDSR
079800940309     C/EJECT
079900940309     C************************************************************
080000940309     C* GESTIONE F24 VIDEO S1
080100940309     C************************************************************
080200940309     C     F24S1         BEGSR
080300940309     C*
080400940506     C*
080500940506     C                   MOVEA     $FX2          $FX
080600940506     C                   MOVEA     $FC2          $FC
080700940506     C                   MOVEA     $FR2          $FR
080800940506     C                   Z-ADD     $ULKS1        $ULK
080900940506     C                   EXSR      RIEKEY
081000941017     C                   MOVEA     $FC           $FC2
081100940506     C                   Z-ADD     $ULK          $ULKS1            3 0
081200940607     C                   MOVEL     $KEY1         Z1KE1
081300941004     C                   MOVEL     $KEY2         Z1KE2
081400940607     C                   WRITE     Z1
081500940506     C*
081600940309     C                   ENDSR
081700940128     C/EJECT
081800940128     C************************************************************
081900940131     C* CONTROLLO TESTATA LISTA
082000940128     C************************************************************
082100940131     C     CTRC1         BEGSR
082200940128     C*
082300940201     C                   MOVE      *OFF          *IN99
082400940131     C*
082500940202     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
082600940202     C* L'INIZIALIZZAZIONE LISTA
082700940202     C*
082800940207     C     C1POS         IFNE      W1POS
082900940202     C                   EXSR      POSS1
083000940202     C                   END
083100940202     C*
083200940202     C                   ENDSR
083300940202     C/EJECT
083400940202     C************************************************************
083500940202     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
083600940202     C************************************************************
083700940202     C     POSS1         BEGSR
083800940202     C*
083900940202     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
084000940202     C*
0841009402021    C     WMAX          IFGT      0
084200940607     C                   READC     S1                                     21
0843009402022    C     *IN21         DOWEQ     *OFF
084400940202     C     *IN99         ANDEQ     *OFF
084500940202     C*
0846009402023    C     S1OPZ         IFNE      0
084700940202     C                   SETON                                        51  99
0848009402023-   C                   ENDIF
084900940202     C* se rilevata opzione attiva, attivo sflnxtchg
0850009402023    C     *IN99         IFEQ      *ON
085100940202     C                   MOVE      *ON           *IN32
085200940714     C                   MOVE      $IN41         *IN41
085300940607     C                   UPDATE    S1
0854009402023E   C                   ELSE
085500940607     C                   READC     S1                                     21
0856009402023-   C                   ENDIF
085700940202     C*
0858009402022-   C                   ENDDO
085900940202     C*
0860009402021-   C                   ENDIF
086100940202     C*
086200940221     C*
0863009402021    C     *IN99         IFEQ      *OFF
086400941121     C                   CLEAR                   W1POS
086500940202     C*
086600940302     C                   MOVEL     C1POS         W1POS
086700940131     C*
086800940202     C                   MOVE      *ON           *IN99
086900940223     C                   MOVE      *ON           $INZS1
087000940202     C*
087100940202     C*
0872009402021-   C                   ENDIF
087300940202     C*
087400940131     C                   ENDSR
087500940131     C/EJECT
087600940131     C************************************************************
087700940131     C* CONTROLLO OPZIONI LISTA
087800940131     C************************************************************
087900940131     C     CTRS1         BEGSR
088000940131     C*
088100940202     C                   MOVEL     *OFF          $ESCI
088200940201     C                   SETOFF                                       99
088300940131     C                   Z-ADD     0             S1OPZ
088400940131     C*
088500940127     C* Leggo il sfl solo se ci sono rcd
0886009401311    C     WMAX          IFGT      0
088700940607     C                   READC     S1                                     21
088800940127     C*
088900940131     C* esce se fine sfl o errore che richiede l'uscita
0890009401312    C     *IN21         DOWEQ     *OFF
089100940131     C     $ESCI         ANDEQ     *OFF
089200940201     C                   Z-ADD     S1NRR         C1RCD
089300940131     C* ctrl su riga
089400940131     C                   EXSR      RECS1
089500940131     C* gestione opzioni
0896009401313    C     S1OPZ         IFNE      0
089700940201     C     *IN99         ANDEQ     *OFF
089800940131     C                   EXSR      OPZS1
0899009401273-   C                   ENDIF
090000940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0901009402013    C     *IN99         IFEQ      *ON
090200940201     C     $ESCI         OREQ      *ON
090300940131     C                   MOVE      *ON           *IN32
090400940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
090500940204     C* la ripristinerò a conclusione del ciclo di READC
090600940223     C                   MOVE      *OFF          $INZS1
0907009402233-   C                   ENDIF
090800940223     C*
090900940131     C                   Z-ADD     0             S1OPZ
091000940223     C*
091100940714     C                   MOVE      $IN41         *IN41
091200940607     C                   UPDATE    S1
091300940223     C*
091400940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0915009401313    C     $ESCI         IFEQ      *OFF
091600940607     C                   READC     S1                                     21
091700940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0918009402014    C     *IN21         IFEQ      *ON
091900940201     C                   MOVE      WINZS1        $INZS1
092000940204     C* calcolo pagina a cui deve posizionarsi
092100940224     C                   EXSR      CLCPAG
0922009402014-   C                   ENDIF
0923009402013-   C                   ENDIF
092400940131     C*
0925009401272-   C                   ENDDO
092600940127     C*
0927009401311-   C                   ENDIF
092800940131     C*
092900940127     C                   ENDSR
093000940127     C/EJECT
093100940127     C************************************************************
093200940131     C* CONTROLLO CAMPI I/O RIGA LISTA
093300940127     C************************************************************
093400940131     C     RECS1         BEGSR
093500940131     C*
093600940201     C* reset indicatori DSPATR
093700940201     C                   MOVE      *ALL'0'       IN4049           10
093800940201     C                   MOVEA     IN4049        *IN(40)
093900940201     C*
094000940131     C* Controllo se Utente Autorizzato alla scelta
094100940202     C     S1OPZ         IFNE      0
094200940131     C                   Z-ADD     1             X
094300940223     C     S1OPZ         LOOKUP    $OP1(X)                                21
094400940211     C     *IN21         IFEQ      *OFF
094500940223     C     $FL1(X)       OREQ      'N'
094600940131     C                   MOVE      *ON           $ESCI
094700940201     C                   SETON                                        504099
094800940131     C                   ENDIF
094900940131     C*
095000940620     c* Controllo autorizzazione alla modifica su record di gruppo
095100940620     C* Reperisco tipo codice (gruppo/azienda)
095200940620     C                   MOVE      *ALL'0'       XTBKEY
095300940620     C                   MOVE      S1COD         XTBKEY
095400970512     C                   MOVE      S1UNITA       XTBUNT
095500940620     C                   MOVE      '0'           XTBALC
095600940620     C                   MOVE      '1'           XTBRIC
095700970214     C                   CALLB     'XATB'
095800940620     C                   PARM                    XATBDS
095900940620     C* Reperisco autorizzazioni utente
096000940620     C                   CLEAR                   XCHATB
096100941118     C                   MOVE      XSCSOC        XCTSOC
096200941118     C                   MOVE      XSCDUN        XCTUNI
096300940620     C                   MOVEL     KNMUS         XCTPRF
096400940620     C                   MOVE      XTBCOD        XCTCOD
096500940620     C                   CALL      'XCHKATB'
096600940620     C                   PARM                    XCHATB
096700941111     C                   MOVEL     XCTDDS        AUTDS
096800941027     C     XCTRTN        IFEQ      1
096900941027     C                   MOVEL     'N'           MANGRU
097000941027     C                   ENDIF
097100940620     C* Non ammessa manutenzione di gruppo
097200940620   1 C     XTBGRU        IFEQ      'G'
097300940620     C     MANGRU        ANDEQ     'N'
097400940620   2 C     S1OPZ         IFEQ      2
097500940729     C     S1OPZ         OREQ      3
097600940620     C     S1OPZ         OREQ      4
097700940620     C                   SETON                                        5399
097800940620   2 C                   ENDIF
097900940620   1 C                   ENDIF
098000940620     C* Non ammessa manutenzione di azienda
098100940620   1 C     XTBGRU        IFEQ      'A'
098200940620     C     MANAZI        ANDEQ     'N'
098300940620   2 C     S1OPZ         IFEQ      2
098400940729     C     S1OPZ         OREQ      3
098500940620     C     S1OPZ         OREQ      4
098600940620     C                   SETON                                        5499
098700940620   2 C                   ENDIF
098800940620   1 C                   ENDIF
098900940607     C                   ENDIF
099000940131     C                   ENDSR
099100940224     C/EJECT
099200940131     C************************************************************
099300940131     C* GESTIONE OPZIONI LISTA
099400940131     C************************************************************
099500940131     C     OPZS1         BEGSR
099600940201     C*
0997009402251    C                   SELECT
099800940225     C*
099900940225     C* se opz=Selezione passo la chiave al pgm chiamante
100000940225     C     S1OPZ         WHENEQ    1
100100970708     C                   MOVE      S1OPZ         XtaOpz
100200970708     C                   MOVE      S1COD         XtaKey
100300970708     C                   MOVE      S1UNIta       xtaUni
100400940225     C                   MOVE      *ON           $ESCI
100500940225     C                   MOVE      *ON           $FINE
100600940729     C*
100700940714     C*
100800940714     C* se opz=Abilita scrive su archivio abilitazioni
100900940714     C     S1OPZ         WHENEQ    11
101000941103     C                   MOVE      XSCSOC        ABLAZI
101100940714     C                   MOVE      *ZEROS        ABLKEY
101200940714     C                   MOVE      S1COD         ABLKEY
101300970512     C                   MOVE      S1UNITA       ABLUNT
101400940714     C                   MOVE      XTBCOD        ABLCOD
101500940714     C                   WRITE     ANABL000                             22
101600940714     C*
101700940714     C* se opz=Disabil.cancella su archivio abilitazioni
101800940714     C     S1OPZ         WHENEQ    12
101900941103     C                   MOVE      XSCSOC        ABLAZI
102000940714     C                   MOVE      *ZEROS        ABLKEY
102100940714     C                   MOVE      S1COD         ABLKEY
102200970512     C                   MOVE      S1UNITA       ABLUNT
102300940714     C                   MOVE      XTBCOD        ABLCOD
102400940714     C     KABL          DELETE    ANABL000                           22
102500940729     C*
102600940225     C*
102700940225     C* se altre opzioni (o nessuna opzione) richiamo pgm gestione
1028009402251O   C                   OTHER
102900940225     C*
103000940616     C                   RESET                   D002DS
103100940607     C                   MOVEL     S1OPZ         OPZ002
103200940616     C                   MOVE      *ZERO         KEY002
103300940607     C                   MOVE      S1COD         KEY002
103400970708     C                   MOVE      XtaLin        LIN002
103500970708     C                   MOVE      XtaAzi        AZI002
103600940715     C                   MOVE      *BLANKS       KPJBU
103700940607     C                   MOVEL     D002DS        KPJBU
103800940608$004 C                   CALL      'ANGY07R2'
103900940607     C                   PARM                    KPJBA
104000940607     C                   MOVEL     KPJBU         D002DS
104100940201     C*
104200940223     C* ritorno da PGM gestione
104300940223     C                   EXSR      GESRET
104400940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
1045009402252    C     *IN99         IFEQ      *ON
104600940223     C                   SETON                                        40
1047009402252-   C                   ENDIF
104800940131     C*
104900940506     C                   MOVE      *HIVAL        $LASTV
1050009402251-   C                   ENDSL
105100940225     C*
105200940131     C                   ENDSR
105300940223     C/EJECT
105400940223     C************************************************************
105500940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
105600940223     C************************************************************
105700940223     C     GESRET        BEGSR
105800940223     C*
105900940223     C* modo di ritorno
106000940223     C*
1061009402231    C                   SELECT
106200940314    >C* << questi modi di utilizzo dei valori di ritorno dal
106300940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
106400940314    >C*    proposte, normalmente sempre valide, ma modificabili
106500940314    >C*    per situazioni particolari >>
106600940223     C* 1 = F3
106700940314    >C     RET002        WHENEQ    '1'
106800940224     C                   MOVE      *ON           $ESCI
106900940223     C                   MOVE      *ON           $FINE
107000940223     C* 2 = F12
107100940314    >C     RET002        WHENEQ    '2'
107200940223     C                   MOVE      *ON           $ESCI
107300940223     C*
1074009402231-   C                   ENDSL
107500940223     C*
107600940223     C* operazione eseguite dal pgm chiamato
107700940223     C*
1078009402231    C                   SELECT
107900940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
108000940314    >C     OPR002        WHENEQ    '1'
108100940223     C                   MOVE      *ON           WINZS1
108200940223     C*
1083009402231-   C                   ENDSL
108400940223     C*
108500940223     C* funzione non eseguibile per errore :
108600940223     C*
1087009402231    C                   SELECT
108800940223     C* 1 = funzione richiamata chiusa in errore
108900940316    >C*  eventualmente gestire altri codici di errore
109000940314    >C     ERR002        WHENEQ    '1'
109100940223     C                   MOVE      *ON           $ESCI
109200940223     C                   SETON                                        5299
109300940223     C*
1094009402231-   C                   ENDSL
109500940223     C*
109600940223     C                   ENDSR
109700940223     C/EJECT
109800940131     C************************************************************
109900940131     C* OPERAZIONI INIZIALI
110000940131     C************************************************************
110100940131     C     *INZSR        BEGSR
110200940131     C*
110300940127     C* Reperimento parametri
110400940127     C*
110500940117     C     *ENTRY        PLIST
110600940117     C                   PARM                    KPJBA
110700940714     C*
110800940223     C* Reperimento opzioni
110900940127     C*
111000940128     C                   EXSR      REPOPZ
111100940506     C*
111200940506     C* Reperimento tasti di funzione
111300940506     C*
111400940506     C                   EXSR      REPKEY
111500940506     C*
111600940607$007 C                   MOVE      'Y07'         XTBCOD
111700010406X0113C*
111800010406  "  C* reperisco le dimensioni della variabile del titolo
111900010406X0113C                   EVAL      LENTIT = %Size(C1Tit)
112000940127     C*
112100940117     C                   ENDSR
112200940225     C/EJECT
112300941103      ************************************************************
112400941103      * Reperimento dati società
112500941103      ************************************************************
112600941103     C     REPSOC        BEGSR
112700941103      *
112800970214     C                   CALLB     'XSOC'
112900941103     C                   PARM                    TIPXSC            6
113000941103     C                   PARM                    SOCXSC            3
113100941103     C                   PARM                    CDSXSC            9 0
113200941103     C                   PARM                    MODXSC            3
113300941103     C                   PARM      *BLANKS       RTNXSC            1
113400941103     C                   PARM                    XSOCDS
113500941103     C                   PARM                    KPJBA
113600941103      *
113700941103     C                   ENDSR
113800940223     C************************************************************
113900940223     C* INIZIALIZZAZIONE VARIABILI
114000940223     C************************************************************
114100940223     C     INZVAR        BEGSR
114200970926     C*
114300970926     C* Dati da PGM chiamante
114400970926     C*
114500970926     C                   MOVEL     KPJBU         XTABDS
114600951124     C*
114700951124     C* Reperimento codice lingua e codice azienda
114800951124     C*
114900951124     C                   MOVEL     'SOC001'      TIPXSC
115000951124     C                   EXSR      REPSOC
115100951124     C     RTNXSC        IFNE      '1'
115200951124     C                   MOVEL     XSOCDS        SOC001
115300951124     C                   ELSE
115400951124     C                   MOVE      *ON           $FINE
115500951124     C* fine programma
115600951124     C                   EXSR      ENDPGM
115700951124     C                   ENDIF
115800951124     C* Lingua e codice azienda
115900970708     C                   MOVE      XSCLIN        XtaLin
116000970708     C                   MOVE      XSCSOC        XtaAzi
116100940715     C*
116200940715     C                   RESET                   $FC2
116300940715     C*
116400940715$021 C                   CLOSE     ANGY07D
116500940715$021 C                   OPEN      ANGY07D
116600940506     C*
116700940506     C* Definizione delle opzioni utilizzabili a pgm
116800940506     C*
116900940506     C                   EXSR      DEFOPZ
117000940718     C* Pulizia campi e indicatori
117100940718     C                   CLEAR                   W1POS
117200940718     C                   MOVE      *ALL'0'       IN4049           10
117300940718     C                   MOVEA     IN4049        *IN(40)
117400940718     C                   CLEAR                   S1OPZ
117500940223     C* Variabili per gestione videate
117600940223     C*
117700940223     C                   MOVE      *OFF          $FINE
117800940223     C                   MOVE      *OFF          $INZS1
117900940223     C                   MOVE      *OFF          $EFILE
118000940223     C                   MOVE      *OFF          $ESCI
118100940223     C                   MOVE      *OFF          $RCDOK
118200940509     C                   MOVE      *HIVAL        $LASTV
118300940506     C                   Z-ADD     0             $ULKS1            3 0
118400940223     C*
118500940318     C                   MOVE      *ON           $INZS1
118600940223     C                   MOVE      'S1'          $GEST
118700940223     C*
118800940223     C* Variabili appoggio
118900940223     C*
119000940223     C                   Z-ADD     SFLPAG        WPAG
119100940223     C*
119200940223     C* Valorizzazione campi univoci testate
119300940223     C*
119400940607     C*                    CLEART1
119500940607     C*                    MOVELKNSIF     NOMSIF
119600940607     C*                    MOVELRAGUTE    NOMDIT
119700940607     C*                    MOVELDSPGM     SIGLA
119800940607     C*                    MOVELPROGR     NOMPGM
119900940223     C*
120000940608     c* posizione window
120100010406X0113c* modificata la seguente riga
120200010406$012 C                   Z-ADD     53            PCOL              2 0
120300940613     C                   Z-ADD     09            R
120400940608     C     80            SUB       PCOL          C
120500940608     C                   SUB       3             C
120600970708     C     XtaOpz        IFNE      '  '
120700970708     C     XtaRig        IFGT      11
120800940608     C                   Z-ADD     1             R
120900940608     C                   ENDIF
121000940608     C*
121100970708     C     XtaCol        IFGT      40
121200940608     C                   Z-ADD     2             C
121300940608     C                   ENDIF
121400940608     C                   ENDIF
121500940616     C*
121600970708     C     XtaOpz        IFNE      '01'
121700940616$019 C                   DO        00            I                 3 0
121800940721$022 C*                  MOVEL     LIN(I)        TBTFLD
121900940721$022 C*                  CLEAR                   TBTKEY
122000940721$022 C*                  CLEAR                   TBTAZI
122100940721$022 C*                  CLEAR                   TBTLIN
122200940721$007 C                   MOVE      'Y07'         TBTCOD            3
122300940721$022 C*    K5TBT1        CHAIN     ANTBT01L                           21
122400940721$022 C*    *IN21         IFEQ      *ON
122500940721$022 C*                  Z-ADD     LLUN(I)       TBTLUN
122600940721$022 C*                  Z-ADD     LPOS(I)       TBTPOS
122700940721$022 C*                  MOVEL     LIND(I)       TBTDES
122800940721$022 C*                  WRITE     ANTBT000
122900940721$022 C*                  ENDIF
123000940616     C                   ENDDO
123100940616     C                   ENDIF
123200940608     C*
123300940722     c*
123400940722     c* Se richiamato con OPZ 11  Non controlla abilitazione all'aziend
123500970708     C     XtaOpz        IFEQ      '11'
123600940722     C                   MOVE      'N'           XTBGAB
123700940722     C                   ELSE
123800940722     C                   MOVE      'S'           XTBGAB
123900940722     C                   ENDIF
124000940223     C                   ENDSR
124100940223     C/EJECT
124200940131     C************************************************************
124300940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
124400940131     C************************************************************
124500940225     C     DEFOPZ        BEGSR
124600940131     C*
124700940225     C* Questa routine deve valorizzare la schiera $FL1
124800940225     C* con S o N a seconda della utilizzabilità o meno
124900940225     C* dell'opzione correlata
125000940225     C*
125100940714     C* Inizialmente si considerano tutte le opzioni come non valide
125200940728     C     1             DO        14            X
125300940714     C                   MOVE      'N'           $FL1(X)
125400940225     C                   END
125500940225     C*
125600940225     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
125700940714     C* rendo valide alcune opzioni
125800940714     C* i tasti invece li disattivo
125900940225     C                   SELECT
126000940714     C*
126100940714     C* Se pgm richiamato in RICERCA attivo :
126200940714     C*
126300970708    >C     XtaOpz        WHENEQ    '01'
126400940714     C* F3=Fine
126500940714     C                   Z-ADD     3             X
126600940720     C                   MOVE      '0'           $FC2(X)
126700940714     C* F10=Immissione
126800940714     C                   Z-ADD     10            X
126900940919     C                   MOVE      '1'           $FC2(X)
127000940714     C* 1=Selezione
127100940714     C                   Z-ADD     1             X
127200940714     C     01            LOOKUP    $OP1(X)                                21
127300940714     C     *IN21         IFEQ      *ON
127400940714     C                   MOVE      'S'           $FL1(X)
127500940714     C                   ENDIF
127600940729     C* 2=Modifica
127700940729     C                   Z-ADD     1             X
127800940729     C     02            LOOKUP    $OP1(X)                                21
127900940729     C     *IN21         IFEQ      *ON
128000940729     C                   MOVE      'S'           $FL1(X)
128100940729     C                   ENDIF
128200940729     C* 3=Copia
128300940729     C                   Z-ADD     1             X
128400940729     C     03            LOOKUP    $OP1(X)                                21
128500940729     C     *IN21         IFEQ      *ON
128600940729     C                   MOVE      'S'           $FL1(X)
128700940729     C                   ENDIF
128800940729     C* 4=Annullamento
128900940729     C                   Z-ADD     1             X
129000940729     C     04            LOOKUP    $OP1(X)                                21
129100940729     C     *IN21         IFEQ      *ON
129200940729     C                   MOVE      'S'           $FL1(X)
129300940729     C                   ENDIF
129400940714     C* 5=Visualizzazione
129500940714     C                   Z-ADD     1             X
129600940714     C     05            LOOKUP    $OP1(X)                                21
129700940714     C     *IN21         IFEQ      *ON
129800940714     C                   MOVE      'S'           $FL1(X)
129900940714     C                   ENDIF
130000940728     C*14=Commenti
130100940831     C**                   Z-ADD1         X
130200940831     C**         14        LOKUP$OP1,X                   21
130300940831     C**         *IN21     IFEQ *ON
130400940831     C**                   MOVE 'S'       $FL1,X
130500940831     C**                   ENDIF
130600940714     C*
130700940714     C* Se pgm richiamato in INTERROGAZIONE attivo :
130800940714     C*
130900970708    >C     XtaOpz        WHENEQ    '05'
131000940714     C* F10=Immissione
131100940714     C                   Z-ADD     10            X
131200940720     C                   MOVE      '0'           $FC2(X)
131300940714     C* F3=Fine
131400940714     C                   Z-ADD     3             X
131500940720     C                   MOVE      '0'           $FC2(X)
131600940714     C* 5=Visualizzazione
131700940225     C                   Z-ADD     1             X
131800940714     C     05            LOOKUP    $OP1(X)                                21
131900940225     C     *IN21         IFEQ      *ON
132000940714     C                   MOVE      'S'           $FL1(X)
132100940225     C                   ENDIF
132200940728     C*14=Commenti
132300940831     C**                   Z-ADD1         X
132400940831     C**         14        LOKUP$OP1,X                   21
132500940831     C**         *IN21     IFEQ *ON
132600940831     C**                   MOVE 'S'       $FL1,X
132700940831     C**                   ENDIF
132800940831     C*
132900940714     C* Se pgm richiamato in ABILITAZIONE attivo :
133000940714     C*
133100970708    >C     XtaOpz        WHENEQ    '11'
133200940714     C* F10=Immissione
133300940714     C                   Z-ADD     10            X
133400940720     C                   MOVE      '0'           $FC2(X)
133500940714     C* F3=Fine
133600940714     C                   Z-ADD     3             X
133700940720     C                   MOVE      '0'           $FC2(X)
133800940714     C* 5=Visualizzazione
133900940714     C                   Z-ADD     1             X
134000940714     C     05            LOOKUP    $OP1(X)                                21
134100940714     C     *IN21         IFEQ      *ON
134200940714     C                   MOVE      'S'           $FL1(X)
134300940714     C                   ENDIF
134400940714     C* 11=Abilitazione
134500940714     C                   Z-ADD     1             X
134600940714     C     11            LOOKUP    $OP1(X)                                21
134700940714     C     *IN21         IFEQ      *ON
134800940714     C                   MOVE      'S'           $FL1(X)
134900940714     C                   ENDIF
135000940714     C* 12=Disabilita
135100940714     C                   Z-ADD     1             X
135200940714     C     12            LOOKUP    $OP1(X)                                21
135300940714     C     *IN21         IFEQ      *ON
135400940714     C                   MOVE      'S'           $FL1(X)
135500940714     C                   ENDIF
135600940714     C*
135700940714     C* Se pgm richiamato in GESTIONE  attivo :
135800940714     C*
135900970708    >C     XtaOpz        WHENEQ    '02'
136000940714     C* F3=Fine
136100940714     C                   Z-ADD     3             X
136200940720     C                   MOVE      '0'           $FC2(X)
136300940714     C* 2=Modifica
136400940714     C                   Z-ADD     1             X
136500940714     C     02            LOOKUP    $OP1(X)                                21
136600940714     C     *IN21         IFEQ      *ON
136700940714     C                   MOVE      'S'           $FL1(X)
136800940714     C                   ENDIF
136900940714     C* 3=Copia
137000940714     C                   Z-ADD     1             X
137100940714     C     03            LOOKUP    $OP1(X)                                21
137200940714     C     *IN21         IFEQ      *ON
137300940714     C                   MOVE      'S'           $FL1(X)
137400940714     C                   ENDIF
137500940714     C* 4=Annullamento
137600940714     C                   Z-ADD     1             X
137700940714     C     04            LOOKUP    $OP1(X)                                21
137800940714     C     *IN21         IFEQ      *ON
137900940714     C                   MOVE      'S'           $FL1(X)
138000940714     C                   ENDIF
138100940714     C* 5=Visualizzazione
138200940714     C                   Z-ADD     1             X
138300940714     C     05            LOOKUP    $OP1(X)                                21
138400940714     C     *IN21         IFEQ      *ON
138500940714     C                   MOVE      'S'           $FL1(X)
138600940714     C                   ENDIF
138700940729     C*14=Commenti
138800940831     C**                   Z-ADD1         X
138900940831     C**         14        LOKUP$OP1,X                   21
139000940831     C**         *IN21     IFEQ *ON
139100940831     C**                   MOVE 'S'       $FL1,X
139200940831     C**                   ENDIF
139300940225     C*
139400940225     C                   OTHER
139500940714     C* Se pgm non richiamato, attivo TUTTO  :
139600940714     C* 2=Modifica
139700940714     C                   Z-ADD     1             X
139800940714     C     02            LOOKUP    $OP1(X)                                21
139900940714     C     *IN21         IFEQ      *ON
140000940714     C                   MOVE      'S'           $FL1(X)
140100940714     C                   ENDIF
140200940714     C* 3=Copia
140300940714     C                   Z-ADD     1             X
140400940714     C     03            LOOKUP    $OP1(X)                                21
140500940714     C     *IN21         IFEQ      *ON
140600940714     C                   MOVE      'S'           $FL1(X)
140700940714     C                   ENDIF
140800940714     C* 4=Annullamento
140900940714     C                   Z-ADD     1             X
141000940714     C     04            LOOKUP    $OP1(X)                                21
141100940714     C     *IN21         IFEQ      *ON
141200940714     C                   MOVE      'S'           $FL1(X)
141300940714     C                   ENDIF
141400940714     C* 5=Visualizzazione
141500940714     C                   Z-ADD     1             X
141600940714     C     05            LOOKUP    $OP1(X)                                21
141700940714     C     *IN21         IFEQ      *ON
141800940714     C                   MOVE      'S'           $FL1(X)
141900940714     C                   ENDIF
142000940714     C* 11=Abilitazione
142100940714     C                   Z-ADD     1             X
142200940714     C     11            LOOKUP    $OP1(X)                                21
142300940714     C     *IN21         IFEQ      *ON
142400940714     C                   MOVE      'S'           $FL1(X)
142500940714     C                   ENDIF
142600940714     C* 12=Disabilita
142700940714     C                   Z-ADD     1             X
142800940714     C     12            LOOKUP    $OP1(X)                                21
142900940714     C     *IN21         IFEQ      *ON
143000940714     C                   MOVE      'S'           $FL1(X)
143100940714     C                   ENDIF
143200940728     C*14=Commenti
143300940831     C**                   Z-ADD1         X
143400940831     C**         14        LOKUP$OP1,X                   21
143500940831     C**         *IN21     IFEQ *ON
143600940831     C**                   MOVE 'S'       $FL1,X
143700940831     C**                   ENDIF
143800940225     C* poi, disattivo a seconda delle autorizzazioni :
143900940314    >C*********  AUTXXX    IFEQ 'N'
144000940314    >C*********            Z-ADD1         X
144100940314    >C*********  'XX'      LOKUP$OP1,X                   21
144200940314    >C*********  *IN21     IFEQ *ON
144300940314    >C*********            MOVE 'N'       $FL1,X
144400940314    >C*********            ENDIF
144500940314    >C*********            ENDIF
144600940225     C*
144700940225     C                   ENDSL
144800940225     C*
144900940225     C                   ENDSR
145000940225     C/EJECT
145100940225     C************************************************************
145200940225     C* REPERIMENTO OPZIONI
145300940225     C************************************************************
145400940225     C     REPOPZ        BEGSR
145500940225     C*
145600940314     C     1             DO        $DIMOP        X
145700940128     C*
145800940223     C     $MS1(X)       IFNE      *BLANK
145900940128     C*
146000940223     C                   MOVEL     $MS1(X)       IDMSG
146100970214     C                   CALLB     'XRTVM'
146200940201     C                   PARM                    IDMSG            27
146300940201     C                   PARM                    TXTMSG          100
146400940201     C                   PARM                    ERRMSG            1
146500940128     C*
146600940128     C     ERRMSG        IFNE      *ON
146700940223     C                   MOVEL     TXTMSG        $TX1(X)
146800940128     C                   ELSE
146900940223     C                   MOVEL     *ALL'?'       $TX1(X)
147000940128     C                   END
147100940128     C*
147200940128     C                   END
147300940128     C*
147400940128     C                   END
147500940128     C*
147600940128     C                   ENDSR
147700940506     C/EJECT
147800940506     C************************************************************
147900940506     C* REPERIMENTO TASTI DI FUNZIONE
148000940506     C************************************************************
148100940506     C     REPKEY        BEGSR
148200940506     C*
148300940506     C                   MOVEA     $FM2          $FM
148400940506     C                   EXSR      REPKKK
148500940506     C                   MOVEA     $FX           $FX2
148600940506     C                   ENDSR
148700940506     C/EJECT
148800940506     C************************************************************
148900940506     C* REPERIMENTO TASTI DI FUNZIONE
149000940506     C************************************************************
149100940506     C     REPKKK        BEGSR
149200940506     C*
149300940506     C                   CLEAR                   $FX
149400940506     C     1             DO        24            X
149500940506     C*
149600940506     C     $FM(X)        IFNE      *BLANK
149700940506     C*
149800940506     C                   MOVEL     $FM(X)        IDMSG
149900970214     C                   CALLB     'XRTVM'
150000940506     C                   PARM                    IDMSG            27
150100940506     C                   PARM                    TXTMSG          100
150200940506     C                   PARM                    ERRMSG            1
150300940506     C*
150400940506     C     ERRMSG        IFNE      *ON
150500940506     C                   MOVEL     TXTMSG        $FX(X)
150600940506     C                   ELSE
150700940506     C                   MOVEL     *ALL' '       $FX(X)
150800940506     C                   END
150900940506     C*
151000940506     C                   END
151100940506     C*
151200940506     C                   END
151300940506     C*
151400940506     C                   ENDSR
151500940223     C/EJECT
151600940128     C************************************************************
151700940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
151800940128     C************************************************************
151900940223     C     DEFCAM        BEGSR
152000940128     C*
152100940223     C* Variabili per gestione videate
152200940223     C*
152300940223     C                   MOVE      *BLANK        $GEST             2
152400940223     C                   MOVE      *BLANK        $FINE             1
152500940223     C                   MOVE      *BLANK        $INZS1            1
152600940223     C                   MOVE      *BLANK        $LASTG            2
152700940223     C                   MOVE      *BLANK        $LASTV            2
152800940223     C                   MOVE      *BLANK        $EFILE            1
152900940223     C                   MOVE      *BLANK        $ESCI             1
153000940223     C                   MOVE      *BLANK        $RCDOK            1
153100940223     C*
153200940314     C* Variabili appoggio sempre presenti in tutte le anagrafiche
153300940223     C*
153400940223     C     *LIKE         DEFINE    C1RCD         S1NRR
153500940223     C     *LIKE         DEFINE    C1NRR         WSFL
153600940223     C     *LIKE         DEFINE    C1RCD         WMAX
153700940223     C     *LIKE         DEFINE    C1RCD         WPAG
153800940223     C     *LIKE         DEFINE    $INZS1        WINZS1
153900940223     C                   MOVE      *BLANK        WWGRP             3
154000940223     C                   Z-ADD     0             CURR              2 0
154100940223     C                   Z-ADD     0             CURC              2 0
154200940223     C                   MOVE      *BLANK        WIN              99
154300940314     C     *LIKE         DEFINE    C1POS         W1POS
154400940620     C*
154500940721$022 C*    K5TBT1        KLIST
154600940721$022 C*                  KFLD                    TBTAZI
154700940721$022 C*                  KFLD                    TBTCOD
154800940721$022 C*                  KFLD                    TBTKEY
154900940721$022 C*                  KFLD                    TBTLIN
155000940721$022 C*                  KFLD                    TBTFLD
155100940714     C*
155200940714     C     KABL          KLIST
155300940714     C                   KFLD                    ABLCOD
155400940714     C                   KFLD                    ABLKEY
155500940714     C                   KFLD                    ABLAZI
155600940314     C*
155700940314     C*
155800940223     C* Indici
155900940223     C*
156000940223     C                   Z-ADD     0             X                 3 0
156100940223     C                   Z-ADD     0             Y                 3 0
156200940127     C*
156300940117     C                   ENDSR
156400940223     C/EJECT
156500940616**
156600940225PROMSG    *LIBL     OPZ0001
156700940225PROMSG    *LIBL     OPZ0002
156800940131PROMSG    *LIBL     OPZ0003
156900940131PROMSG    *LIBL     OPZ0004
157000940131PROMSG    *LIBL     OPZ0005
157100940224PROMSG    *LIBL     OPZ0006
157200940224PROMSG    *LIBL     OPZ0007
157300940224PROMSG    *LIBL     OPZ0008
157400940224PROMSG    *LIBL     OPZ0009
157500970716PROMSG    *LIBL     OPZ0010
157600970716PROMSG    *LIBL     OPZ0013
157700970716PROMSG    *LIBL     OPZ0012
157800970716PROMSG    *LIBL     OPZ0011
157900970716PROMSG    *LIBL     OPZ0014
158000940201**
15810094022501S      <Selezione
15820094022502S      <Modifica
15830094022503S      <Copia
15840094022504S      <Annullamento
15850094022505S      <Visualizzazione
15860094060706N      <prova
15870094060707N      <prova
15880094060708N      <prova
15890094060709N      <prova
15900094072910S      <Invia
15910094071411S      <Abilita gruppo
15920094071412S      <Disabilita
15930094072813N      <PROVA
15940094083114N      <Commenti
159500940506** TASTI DI FUNZIONE  S1
159600940506PROMSG    *LIBL     KEY0001  01
159700940506PROMSG    *LIBL     KEY0002  02
159800940506PROMSG    *LIBL     KEY0003  03
159900940506PROMSG    *LIBL     KEY0004  04
160000940506PROMSG    *LIBL     KEY0005  05
160100940506PROMSG    *LIBL     KEY0006  06
160200940506PROMSG    *LIBL     KEY0007  07
160300940506PROMSG    *LIBL     KEY0008  08
160400940506PROMSG    *LIBL     KEY0009  09
160500970716PROMSG    *LIBL     KEY0010  10
160600970716PROMSG    *LIBL     KEY0011  11
160700970716PROMSG    *LIBL     KEY0012  12
160800970716PROMSG    *LIBL     KEY0013  13
160900970716PROMSG    *LIBL     KEY0014  14
161000970716PROMSG    *LIBL     KEY0015  15
161100970716PROMSG    *LIBL     KEY0016  16
161200970716PROMSG    *LIBL     KEY0017  17
161300970716PROMSG    *LIBL     KEY0018  18
161400970716PROMSG    *LIBL     KEY0019  19
161500940506PROMSG    *LIBL     KEY0020  20
161600940506PROMSG    *LIBL     KEY0021  21
161700940506PROMSG    *LIBL     KEY0022  22
161800940506PROMSG    *LIBL     KEY0023  23
161900940506PROMSG    *LIBL     KEY0024  24
162000940506**  ABILITAZIONE S1
1621009410140N  01
1622009407200N  02
1623009407201N  03
1624009407200N  04
1625009407200N  05
1626009407200N  06
1627009407200N  07
1628009407200N  08
1629009407200N  09
1630009407201N  10
1631009407200N  11
1632009407201N  12
1633009407200N  13
1634009407200N  14
1635009407200N  15
1636009407200N  16
1637009407200N  17
1638009407200N  18
1639009407200N  19
1640009407200N  20
1641009407200N  21
1642009407200N  22
1643009505020N  23
1644009407200N  24
