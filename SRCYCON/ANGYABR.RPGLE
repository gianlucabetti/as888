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
003900940715$001 FANGYABD   CF   E             WORKSTN USROPN
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
014200940607$003 D EXTDS         E DS                  EXTNAME(ANGYABDS)
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
018200940607$015 I              S1NAS                       NASYAB
018300940207     I*-------------
018400940127     I*
018500940127     C*----------------------------------------------------*
018600940127     C*                MAIN LINE PROGRAM
018700940127     C*----------------------------------------------------*
018800940127     C*
018900940223     C* inizializzazione variabili
019000940223     C                   EXSR      INZVAR
019100940223     C*
019200940223     C     $FINE         DOWEQ     *OFF
019300940131     C     $GEST         CASEQ     'S1'          GESS1
019400940117     C                   END
019500940117     C                   END
019600940325     C*
019700940325     C* fine programma
019800940325     C                   EXSR      ENDPGM
019900940225     C*
020000940325     C/EJECT
020100940325     C************************************************************
020200940325     C* FINE PROGRAMMA
020300940325     C************************************************************
020400940325     C     ENDPGM        BEGSR
020500940325     C*
020600940325     C* passaggio dati al pgm chiamante
020700940715     C                   MOVE      *BLANKS       KPJBU
020800940607     C                   MOVEL     XTABDS        KPJBU
020900940325     C*
021000940325     C* Se pgm Guida non è stato richiamato posso chiudere in LR
021100970708     C     XtaOpz        IFEQ      *BLANK
021200940325     C                   SETON                                            LR
021300960906     C                   RETURN
021400940325     C* altrimenti chiudo in RT
021500940325     C                   ELSE
021600940325     C                   SETON                                            RT
021700960906     C                   RETURN
021800940325     C                   ENDIF
021900940325     C*
022000940325     C                   ENDSR
022100940131     C/EJECT
022200940224     C************************************************************
022300940131     C* RIEMPIMENTO OPZIONI
022400940224     C************************************************************
022500940131     C     RIEOPZ        BEGSR
022600940223     C*
022700940223     C* Input :
022800940223     C* - $TX = schiera con tutti i testi disponibili
022900940223     C* - $FL = schiere con i flag di validità associati ai testi
023000940225     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
023100940223     C*          opzione usato
023200940223     C* - WMXO = variabile contenente il numero massimo di testi
023300940223     C*          opzione usabili
023400940223     C* Output :
023500940223     C* - WOPZ1 = variabile contenente la prima riga opzioni
023600940223     C* - WOPZ2 = variabile contenente la seconda riga opzioni
023700950502     C* - FF23  = flag che indica se c'è bisogno del tasto F23
023800940223     C*
023900970214     C                   CALLB     'X04OPMSG'
024000940223     C* input
024100940223     C                   PARM                    $TX
024200940223     C                   PARM                    $FL
024300940225     C                   PARM                    WCOZ              3 0
024400940223     C                   PARM                    WMXO              3 0
024500940223     C* output
024600940223     C                   PARM                    WOPZ1            76
024700940223     C                   PARM                    WOPZ2            76
024800950502     C                   PARM                    FF23              1
024900940607     C* input
025000940607$018 C                   PARM      32            $L01              3 0
025100940607     C                   PARM      0             $L02              3 0
025200940223     C*
025300940131     C                   ENDSR
025400940506     C/EJECT
025500940506     C************************************************************
025600940506     C* RIEMPIMENTO TASTI FUNZIONE
025700940506     C************************************************************
025800940506     C     RIEKEY        BEGSR
025900940506     C*
026000940506     C* Input :
026100940506     C* - $FX = schiera con tutti i testi disponibili
026200940506     C* - $FC = schiere con i flag di validità associati ai testi
026300940506     C* - $FR = schiere con i flag di REVERSE IMAGE
026400940506     C* - $ULK = ultimo testo caricato
026500940506     C* Output :
026600940506     C* - $ULK
026700940506     c* - $KEY1= 1° riga di tasti funzione
026800940506     c* - $KEY2= 2° riga di tasti funzione
026900940506     C*
027000970214     C                   CALLB     'XKEYMSG'
027100940506     C* input
027200940506     C                   PARM                    $FX
027300940506     C                   PARM                    $FC
027400940506     C                   PARM                    $FR
027500940506     C                   PARM                    $ULK              3 0
027600940506     C* output
027700940506     C                   PARM                    $KEY1            79
027800940506     C                   PARM                    $KEY2            79
027900940607     C* input
028000940607$018 C                   PARM      32            $L01              3 0
028100941004$018 C                   PARM      32            $L02              3 0
028200940506     C*
028300940506     C                   ENDSR
028400940224     C/EJECT
028500940224     C************************************************************
028600940224     C* GESTIONE EMISSIONE TASTI FUNZIONE
028700940224     C************************************************************
028800940224     C     GESPIE        BEGSR
028900940224     C*
029000940224     C                   SELECT
029100940506     C* Lista
029200940506     C     $GEST         WHENEQ    'S1'
029300940506     C     $ULKS1        IFEQ      0
029400940506     C                   EXSR      F24S1
029500940506     C                   ELSE
029600940607     C                   WRITE     Z1
029700940506     C                   ENDIF
029800940506     C                   ENDSL
029900940506     C*
030000940224     C                   ENDSR
030100940506     C/EJECT
030200940506     C************************************************************
030300940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA S1
030400940506     C************************************************************
030500940506     C     CTRKS1        BEGSR
030600940506     C                   SETOFF                                       99
030700940506     C                   MOVEA     $FC2          $FC
030800940506     C                   EXSR      CTRKEY
030900940506     C     *IN99         IFEQ      *ON
031000940506     C                   SETON                                        97
031100940506     C                   ENDIF
031200940607     C*
031300940506     C                   ENDSR
031400940506     C/EJECT
031500940506     C************************************************************
031600940506     C* CONTROLLO ABILITAZIONE TASTI
031700940509     C* INPUT
031800940509     c* $TASTO =Tasto funzione premuto
031900940509     C* $FC = Tasti abilitati
032000940509     C* OUTPUT
032100940509     C* IN99 = ERrore
032200940506     C************************************************************
032300940506     C     CTRKEY        BEGSR
032400940506     C                   SELECT
032500940506     C     $TASTO        WHENEQ    F01
032600940720     C     $FC(1)        IFEQ      '0'
032700940506     C                   SETON                                        99
032800940506     C                   ENDIF
032900940506     C     $TASTO        WHENEQ    F02
033000940720     C     $FC(2)        IFEQ      '0'
033100940506     C                   SETON                                        99
033200940506     C                   ENDIF
033300940506     C     $TASTO        WHENEQ    F03
033400940720     C     $FC(3)        IFEQ      '0'
033500940506     C                   SETON                                        99
033600940506     C                   ENDIF
033700940506     C     $TASTO        WHENEQ    F04
033800940720     C     $FC(4)        IFEQ      '0'
033900940506     C                   SETON                                        99
034000940506     C                   ENDIF
034100940506     C     $TASTO        WHENEQ    F05
034200940720     C     $FC(5)        IFEQ      '0'
034300940506     C                   SETON                                        99
034400940506     C                   ENDIF
034500940506     C     $TASTO        WHENEQ    F06
034600940720     C     $FC(6)        IFEQ      '0'
034700940506     C                   SETON                                        99
034800940506     C                   ENDIF
034900940506     C     $TASTO        WHENEQ    F07
035000940720     C     $FC(7)        IFEQ      '0'
035100940506     C                   SETON                                        99
035200940506     C                   ENDIF
035300940506     C     $TASTO        WHENEQ    F08
035400940720     C     $FC(8)        IFEQ      '0'
035500940506     C                   SETON                                        99
035600940506     C                   ENDIF
035700940506     C     $TASTO        WHENEQ    F09
035800940720     C     $FC(09)       IFEQ      '0'
035900940506     C                   SETON                                        99
036000940506     C                   ENDIF
036100940506     C     $TASTO        WHENEQ    F10
036200940720     C     $FC(10)       IFEQ      '0'
036300940506     C                   SETON                                        99
036400940506     C                   ENDIF
036500940506     C     $TASTO        WHENEQ    F11
036600940720     C     $FC(11)       IFEQ      '0'
036700940506     C                   SETON                                        99
036800940506     C                   ENDIF
036900940506     C     $TASTO        WHENEQ    F12
037000940720     C     $FC(12)       IFEQ      '0'
037100940506     C                   SETON                                        99
037200940506     C                   ENDIF
037300940506     C     $TASTO        WHENEQ    F13
037400940720     C     $FC(13)       IFEQ      '0'
037500940506     C                   SETON                                        99
037600940506     C                   ENDIF
037700940506     C     $TASTO        WHENEQ    F14
037800940720     C     $FC(14)       IFEQ      '0'
037900940506     C                   SETON                                        99
038000940506     C                   ENDIF
038100940506     C     $TASTO        WHENEQ    F15
038200940720     C     $FC(15)       IFEQ      '0'
038300940506     C                   SETON                                        99
038400940506     C                   ENDIF
038500940506     C     $TASTO        WHENEQ    F16
038600940720     C     $FC(16)       IFEQ      '0'
038700940506     C                   SETON                                        99
038800940506     C                   ENDIF
038900940506     C     $TASTO        WHENEQ    F17
039000940720     C     $FC(17)       IFEQ      '0'
039100940506     C                   SETON                                        99
039200940506     C                   ENDIF
039300940506     C     $TASTO        WHENEQ    F18
039400940720     C     $FC(18)       IFEQ      '0'
039500940506     C                   SETON                                        99
039600940506     C                   ENDIF
039700940506     C     $TASTO        WHENEQ    F19
039800940720     C     $FC(19)       IFEQ      '0'
039900940506     C                   SETON                                        99
040000940506     C                   ENDIF
040100940506     C     $TASTO        WHENEQ    F20
040200940720     C     $FC(20)       IFEQ      '0'
040300940506     C                   SETON                                        99
040400940506     C                   ENDIF
040500940506     C     $TASTO        WHENEQ    F21
040600940720     C     $FC(21)       IFEQ      '0'
040700940506     C                   SETON                                        99
040800940506     C                   ENDIF
040900940506     C     $TASTO        WHENEQ    F22
041000940720     C     $FC(22)       IFEQ      '0'
041100940506     C                   SETON                                        99
041200940506     C                   ENDIF
041300940506     C     $TASTO        WHENEQ    F23
041400940720     C     $FC(23)       IFEQ      '0'
041500940506     C                   SETON                                        99
041600940506     C                   ENDIF
041700940506     C     $TASTO        WHENEQ    F24
041800940720     C     $FC(24)       IFEQ      '0'
041900940506     C                   SETON                                        99
042000940506     C                   ENDIF
042100940506     C                   ENDSL
042200940506     C                   ENDSR
042300940224     C/EJECT
042400940127     C************************************************************
042500940131     C* GESTIONE LISTA
042600940127     C************************************************************
042700940127     C     GESS1         BEGSR
042800940223     C* inizializzazione videata
042900940223     C     $INZS1        IFEQ      *ON
043000940127     C                   EXSR      INZS1
043100940223     C                   MOVE      *OFF          $INZS1
043200940127     C                   ENDIF
043300940223     C* emissione piede videata se proveniente da altra
043400940223     C* salvataggio valore formato di provenienza
043500940202     C     $LASTV        IFNE      'S1'
043600940607!!   C*                    WRITEFMTT1
043700940224     C                   EXSR      GESPIE
043800940202     C                   MOVE      $LASTV        $LASTG
043900940202     C                   MOVE      'S1'          $LASTV
044000940202     C                   END
044100940223     C*
044200940223     C     WMAX          IFEQ      0
044300940607     C                   WRITE     D1
044400940211     C                   ENDIF
044500940729     C                   MOVE      '1'           WIN727            1
044600940127     C*
044700940607     C                   EXFMT     C1
044800940204     C     C1NRR         IFNE      0
044900940204     C                   Z-ADD     C1NRR         WSFL
045000940204     C                   ENDIF
045100940127     C                   Z-ADD     SFLNRR        C1RCD
045200940506     C*
045300940506     C* controllo abilitazione tasti funzione
045400940506     C*
045500940506     C                   EXSR      CTRKS1
045600940506     C     *IN99         IFEQ      *OFF
045700940506     C*
045800940127     C*
0459009401271    C                   SELECT
046000940127     C* F3=Fine
046100940506     C     $TASTO        WHENEQ    F03
046200940309     C                   EXSR      F03S1
046300940131     C* F10=Immissione
046400940506     C     $TASTO        WHENEQ    F10
046500940309     C                   EXSR      F10S1
046600940131     C* F12=Ritorno
046700940506     C     $TASTO        WHENEQ    F12
046800940309     C                   EXSR      F12S1
046900940224     C* F23=Altre opzioni
047000940506     C     $TASTO        WHENEQ    F23
047100940309     C                   EXSR      F23S1
047200940224     C* F24=Altri tasti
047300940506     C     $TASTO        WHENEQ    F24
047400940309     C                   EXSR      F24S1
047500940127     C* ROLL-UP
047600940506     C     $TASTO        WHENEQ    ROLLUP
047700940127     C                   EXSR      ROLS1
047800940127     C*
0479009401271O   C                   OTHER
048000940127     C* CONTROLLO DATI
048100940131     C                   EXSR      CTRC1
048200940201     C     *IN99         IFEQ      *OFF
048300940131     C                   EXSR      CTRS1
048400940131     C                   END
0485009401271-   C                   ENDSL
048600940127     C*
048700940506     C                   ENDIF
048800940127     C                   ENDSR
048900940224     C/EJECT
049000940127     C************************************************************
049100940131     C* INIZIALIZZAZIONE LISTA
049200940127     C************************************************************
049300940127     C     INZS1         BEGSR
049400940302     C* pulizia SFL
049500940128     C                   SETOFF                                         3031
049600940607     C                   WRITE     C1
049700940128     C                   SETON                                          31
049800940127     C*
049900940128     C* riempimento opzioni
050000940223     C                   MOVEA     $TX1          $TX
050100940223     C                   MOVEA     $FL1          $FL
050200940309     C                   Z-ADD     0             WCOZ
050300940225     C*  equivale al num.di elementi della schiera $TX1
050400940509     C                   Z-ADD     $DIMOP        WMXO
050500940128     C                   EXSR      RIEOPZ
050600950502     C                   Z-ADD     23            X
050700950502     C                   MOVE      FF23          $FC2(X)
050800940608     C                   MOVEL     WOPZ1         C1OPZ1
050900940608     C**                   MOVELWOPZ2     C1OPZ2
051000940225     C                   Z-ADD     WCOZ          C1COZ
051100940128     C*
051200940128     C* CARICAMENTO SFL A PAGINE
051300940128     C*
051400940201     C                   Z-ADD     0             S1NRR
051500940128     C                   Z-ADD     0             WMAX
051600940224     C*
051700940224     C* Posizionamento su file pilota
051800940302     C                   CLEAR                   C1POS
051900940316    >C                   EXSR      SETANA
052000940608    >C                   EXSR      REDANA
052100940302     C* ripristino i valori del posizionamento
052200940302     C                   MOVE      W1POS         C1POS
052300940131     C*
052400940131     C* Carico 1a pagina
052500940127     C                   EXSR      ROLS1
052600940201     C                   Z-ADD     SFLPAG        WPAG
052700940127     C*
052800940127     C                   ENDSR
052900940127     C/EJECT
053000940127     C************************************************************
053100940131     C* CARICAMENTO PAGINA LISTA
053200940127     C************************************************************
053300940127     C     ROLS1         BEGSR
053400940127     C*
053500940128     C                   SETOFF                                       32
053600940223     C                   Z-ADD     0             Y
053700940127     C                   Z-ADD     WMAX          S1NRR
053800940127     C*
053900940127     C* Leggo dal file anagrafico per caricare la lista
054000940127     C*
0541009401311    C     $EFILE        DOWEQ     *OFF
054200940201     C     Y             ANDLT     WPAG
054300940127     C*
054400940131     C                   EXSR      RIES1
054500940127     C*
054600940127     C                   ADD       1             S1NRR
054700940127     C                   ADD       1             Y
054800940714     C                   MOVE      $IN41         *IN41
054900940127     C*
055000940607     C                   WRITE     S1
055100940131     C*
055200940316    >C                   EXSR      REDANA
055300940128     C*
0554009401271-   C                   ENDDO
055500940127     C*
055600940223     C                   Z-ADD     S1NRR         WMAX                 30
055700940127     C*
055800940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
055900940127     C*
056000940201     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
056100940127     C                   MVR                     RESTO             3 0
056200940131     C     PAGINE        MULT      SFLPAG        C1RCD
0563009401271    C     RESTO         IFGT      0
056400940127     C                   ADD       1             C1RCD
0565009401271E   C                   ELSE
056600940407     C                   SUB       SFLPAG        C1RCD
056700940407     C                   ADD       1             C1RCD
0568009401271-   C                   ENDIF
056900940128     C*
057000940127     C                   ENDSR
057100940224     C/EJECT
057200940224     C************************************************************
057300940224     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
057400940224     C************************************************************
057500940607     C     SETANA        BEGSR
057600940224     C*
057700941004     C                   MOVE      *BLANKS       XTBKEY
057800970708     C                   MOVE      XtaAzi        XTBAZI
057900970708     C                   MOVE      XtaLin        XTBLIN
058000940608     C                   MOVE      '9'           XTBRIC
058100940607     C                   MOVE      '0'           XTBALC
058200950622     C                   MOVE      XSCCUT        XTBUTE
058300940607     C                   MOVE      '0'           XTBERR
058400941121     C**         W1POS     IFNE *BLANK
058500941004     C                   MOVE      *ZEROS        XTBKEY
058600940608     C                   MOVE      W1POS         XTBKEY
058700941121     C**                   END
058800970214     C                   CALLB     'XATB'
058900940607     C                   PARM                    XATBDS
059000940607     C*
059100940607     C     XTBERR        IFNE      '0'
059200940607     C                   MOVEL     *ON           $EFILE
059300940607     C                   ENDIF
059400940224     C*
059500940224     C                   ENDSR
059600940128     C/EJECT
059700940128     C************************************************************
059800940131     C* LETTURA RCD ARCHIVIO PILOTA
059900940128     C************************************************************
060000940607     C     REDANA        BEGSR
060100940128     C*
060200940131     C                   MOVEL     *OFF          $EFILE
060300940131     C                   MOVEL     *OFF          $RCDOK
060400940131     C*
0605009401311    C     $EFILE        DOUEQ     *ON
060600940131     C     $RCDOK        OREQ      *ON
060700940131     C*
060800940607     C                   MOVE      '3'           XTBRIC
060900940607     C                   MOVE      '0'           XTBERR
061000970214     C                   CALLB     'XATB'
061100940607     C                   PARM                    XATBDS
061200940607     C*
061300940607     C     XTBERR        IFNE      '0'
061400940607     C                   MOVEL     *ON           $EFILE
061500940607     C                   ENDIF
061600940131     C*
0617009401312    C     $EFILE        IFEQ      *OFF
061800940607     C                   EXSR      SELANA
061900950502     C                   MOVE      XTBKEY        SAVKEY
062000950502     C                   MOVE      XTBGRU        SAVGRU
062100950502     C                   MOVE      XTBUNI        SAVUNI
062200970509     C                   MOVE      XTBUNT        SAVUNT
0623009401312-   C                   ENDIF
062400940131     C*
0625009401311-   C                   ENDDO
062600940131     C*
062700940607     C                   MOVE      $EFILE        *IN33
062800940131     C*
062900940131     C                   ENDSR
063000940224     C/EJECT
063100940224     C************************************************************
063200940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
063300940224     C************************************************************
063400940224     C     CLCPAG        BEGSR
063500940224     C* Input :
063600940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
063700940224     C*          cursore
063800940224     C* - SFLPAG = numero rcd per pagina sfl
063900940224     C* Output :
064000940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
064100940224     C*
064200940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
064300940224     C                   MVR                     RESTO             3 0
064400940224     C     RESTO         IFGT      0
064500940224     C                   ADD       1             PAGINE
064600940224     C                   ENDIF
064700940224     C     PAGINE        MULT      SFLPAG        WPAG
064800940224     C*
064900940224     C                   ENDSR
065000940131     C/EJECT
065100940131     C************************************************************
065200940131     C* SELEZIONI SULLA RIGA LETTA
065300940314     C* impostare a *OFF il campo $RCDOK per scartare un record che
065400940314     C* non rientra nei parametri di parzializzazione
065500940314     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
065600940314     C* superiore stabilito nei parametri
065700940314     C*
065800940131     C************************************************************
065900940607     C     SELANA        BEGSR
066000940131     C*
066100940202     C                   MOVEL     *ON           $RCDOK
066200940714     C* Se aperto il pgm con opzione 11
066300940714     C* carico solo i record di gruppo
066400970708     C     XtaOpz        IFEQ      '11'
066500940714     C     XTBGRU        ANDNE     'G'
066600940714     C                   MOVEL     *OFF          $RCDOK
066700940714     C                   END
066800940714     C* Carica in alta intensità solo i record
066900940714     C* abilitati
067000940714     C                   MOVE      '0'           $IN41
067100970708     C     XtaOpz        IFEQ      '11'
067200940714     C     XTBGRU        ANDEQ     'G'
067300940715     C     XTBAZB        ANDNE     'N'
067400940714     C                   MOVE      '1'           $IN41
067500940714     C                   END
067600940714     C*
067700940202     C                   ENDSR
067800940131     C/EJECT
067900940131     C************************************************************
068000940131     C* RIEMPIMENTO RIGA LISTA
068100940131     C************************************************************
068200940131     C     RIES1         BEGSR
068300950502     C*                    MOVE XTBKEY    S1COD
068400950502     C*                    MOVE XTBGRU    S1GRU
068500950502     C                   MOVEL     SAVUNI        EXTDS
068600950502     C                   MOVE      SAVKEY        S1COD
068700950502     C                   MOVE      SAVGRU        S1GRU
068800970512     C                   MOVE      SAVUNT        S1UNITA           8
068900950502     C     *LIKE         DEFINE    XTBUNI        SAVUNI
069000950502     C     *LIKE         DEFINE    XTBKEY        SAVKEY
069100950502     C     *LIKE         DEFINE    XTBGRU        SAVGRU
069200970509     C     *LIKE         DEFINE    XTBUNT        SAVUNT
069300940131     C*
069400940131     C                   ENDSR
069500940309     C/EJECT
069600940309     C************************************************************
069700940309     C* GESTIONE F03 VIDEO S1
069800940309     C************************************************************
069900940309     C     F03S1         BEGSR
070000940309     C*
070100940309     C                   MOVE      *ON           $FINE
070200970708     C                   MOVE      '1'           XtaRet
070300940325     C* fine programma
070400940325     C                   EXSR      ENDPGM
070500940309     C*
070600940309     C                   ENDSR
070700940309     C/EJECT
070800940309     C************************************************************
070900940309     C* GESTIONE F10 VIDEO S1
071000940314     c* AGGIUNTA RECORD
071100940309     C************************************************************
071200940309     C     F10S1         BEGSR
071300940309     C*
071400940607     C                   RESET                   D002DS
071500940607     C                   MOVEL     '01'          OPZ002
071600970708     C                   MOVE      XtaLin        LIN002
071700970708     C                   MOVE      XtaAzi        AZI002
071800970512     C                   MOVE      S1UNITA       UNI002
071900940715     C                   MOVE      *BLANKS       KPJBU
072000940607     C                   MOVEL     D002DS        KPJBU
072100940608$004 C                   CALL      'ANGYABR2'
072200940309     C                   PARM                    KPJBA
072300940607     C                   MOVEL     KPJBU         D002DS
072400940309     C* ritorno da PGM gestione
072500940309     C                   EXSR      GESRET
072600940309     C     WINZS1        IFEQ      *ON
072700940309     C                   MOVE      *ON           $INZS1
072800940309     C* se esistevano già righe sul sfl
072900940309     C* calcolo pagina a cui deve posizionarsi
073000940309     C     WSFL          IFGT      0
073100940309     C                   EXSR      CLCPAG
073200940309     C* altrimenti carico solo la 1a pagina
073300940309     C                   ELSE
073400940309     C                   Z-ADD     SFLPAG        WPAG
073500940309     C                   ENDIF
073600940309     C                   ENDIF
073700940309     C*
073800940712     C                   MOVE      *HIVAL        $LASTV
073900940309     C                   ENDSR
074000940309     C/EJECT
074100940309     C************************************************************
074200940309     C* GESTIONE F12 VIDEO S1
074300940309     C************************************************************
074400940309     C     F12S1         BEGSR
074500940309     C*
074600970708     C                   MOVE      '2'           XtaRet
074700940309     C                   MOVE      *ON           $FINE
074800940309     C*
074900940309     C                   ENDSR
075000940309     C/EJECT
075100940309     C************************************************************
075200940309     C* GESTIONE F23 VIDEO S1
075300940309     C************************************************************
075400940309     C     F23S1         BEGSR
075500940309     C*
075600940309     C                   EXSR      RIEOPZ
075700950502     C                   Z-ADD     23            X
075800950502     C                   MOVE      FF23          $FC2(X)
075900940608     C                   MOVEL     WOPZ1         C1OPZ1
076000940608     C**                   MOVELWOPZ2     C1OPZ2
076100940309     C                   Z-ADD     WCOZ          C1COZ
076200940309     C*
076300940309     C                   ENDSR
076400940309     C/EJECT
076500940309     C************************************************************
076600940309     C* GESTIONE F24 VIDEO S1
076700940309     C************************************************************
076800940309     C     F24S1         BEGSR
076900940309     C*
077000940506     C*
077100940506     C                   MOVEA     $FX2          $FX
077200940506     C                   MOVEA     $FC2          $FC
077300940506     C                   MOVEA     $FR2          $FR
077400940506     C                   Z-ADD     $ULKS1        $ULK
077500940506     C                   EXSR      RIEKEY
077600941017     C                   MOVEA     $FC           $FC2
077700940506     C                   Z-ADD     $ULK          $ULKS1            3 0
077800940607     C                   MOVEL     $KEY1         Z1KE1
077900941004     C                   MOVEL     $KEY2         Z1KE2
078000940607     C                   WRITE     Z1
078100940506     C*
078200940309     C                   ENDSR
078300940128     C/EJECT
078400940128     C************************************************************
078500940131     C* CONTROLLO TESTATA LISTA
078600940128     C************************************************************
078700940131     C     CTRC1         BEGSR
078800940128     C*
078900940201     C                   MOVE      *OFF          *IN99
079000940131     C*
079100940202     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
079200940202     C* L'INIZIALIZZAZIONE LISTA
079300940202     C*
079400940207     C     C1POS         IFNE      W1POS
079500940202     C                   EXSR      POSS1
079600940202     C                   END
079700940202     C*
079800940202     C                   ENDSR
079900940202     C/EJECT
080000940202     C************************************************************
080100940202     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
080200940202     C************************************************************
080300940202     C     POSS1         BEGSR
080400940202     C*
080500940202     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
080600940202     C*
0807009402021    C     WMAX          IFGT      0
080800940607     C                   READC     S1                                     21
0809009402022    C     *IN21         DOWEQ     *OFF
081000940202     C     *IN99         ANDEQ     *OFF
081100940202     C*
0812009402023    C     S1OPZ         IFNE      0
081300940202     C                   SETON                                        51  99
0814009402023-   C                   ENDIF
081500940202     C* se rilevata opzione attiva, attivo sflnxtchg
0816009402023    C     *IN99         IFEQ      *ON
081700940202     C                   MOVE      *ON           *IN32
081800940714     C                   MOVE      $IN41         *IN41
081900940607     C                   UPDATE    S1
0820009402023E   C                   ELSE
082100940607     C                   READC     S1                                     21
0822009402023-   C                   ENDIF
082300940202     C*
0824009402022-   C                   ENDDO
082500940202     C*
0826009402021-   C                   ENDIF
082700940202     C*
082800940221     C*
0829009402021    C     *IN99         IFEQ      *OFF
083000941121     C                   CLEAR                   W1POS
083100940202     C*
083200940302     C                   MOVEL     C1POS         W1POS
083300940131     C*
083400940202     C                   MOVE      *ON           *IN99
083500940223     C                   MOVE      *ON           $INZS1
083600940202     C*
083700940202     C*
0838009402021-   C                   ENDIF
083900940202     C*
084000940131     C                   ENDSR
084100940131     C/EJECT
084200940131     C************************************************************
084300940131     C* CONTROLLO OPZIONI LISTA
084400940131     C************************************************************
084500940131     C     CTRS1         BEGSR
084600940131     C*
084700940202     C                   MOVEL     *OFF          $ESCI
084800940201     C                   SETOFF                                       99
084900940131     C                   Z-ADD     0             S1OPZ
085000940131     C*
085100940127     C* Leggo il sfl solo se ci sono rcd
0852009401311    C     WMAX          IFGT      0
085300940607     C                   READC     S1                                     21
085400940127     C*
085500940131     C* esce se fine sfl o errore che richiede l'uscita
0856009401312    C     *IN21         DOWEQ     *OFF
085700940131     C     $ESCI         ANDEQ     *OFF
085800940201     C                   Z-ADD     S1NRR         C1RCD
085900940131     C* ctrl su riga
086000940131     C                   EXSR      RECS1
086100940131     C* gestione opzioni
0862009401313    C     S1OPZ         IFNE      0
086300940201     C     *IN99         ANDEQ     *OFF
086400940131     C                   EXSR      OPZS1
0865009401273-   C                   ENDIF
086600940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0867009402013    C     *IN99         IFEQ      *ON
086800940201     C     $ESCI         OREQ      *ON
086900940131     C                   MOVE      *ON           *IN32
087000940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
087100940204     C* la ripristinerò a conclusione del ciclo di READC
087200940223     C                   MOVE      *OFF          $INZS1
0873009402233-   C                   ENDIF
087400940223     C*
087500940131     C                   Z-ADD     0             S1OPZ
087600940223     C*
087700940714     C                   MOVE      $IN41         *IN41
087800940607     C                   UPDATE    S1
087900940223     C*
088000940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0881009401313    C     $ESCI         IFEQ      *OFF
088200940607     C                   READC     S1                                     21
088300940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0884009402014    C     *IN21         IFEQ      *ON
088500940201     C                   MOVE      WINZS1        $INZS1
088600940204     C* calcolo pagina a cui deve posizionarsi
088700940224     C                   EXSR      CLCPAG
0888009402014-   C                   ENDIF
0889009402013-   C                   ENDIF
089000940131     C*
0891009401272-   C                   ENDDO
089200940127     C*
0893009401311-   C                   ENDIF
089400940131     C*
089500940127     C                   ENDSR
089600940127     C/EJECT
089700940127     C************************************************************
089800940131     C* CONTROLLO CAMPI I/O RIGA LISTA
089900940127     C************************************************************
090000940131     C     RECS1         BEGSR
090100940131     C*
090200940201     C* reset indicatori DSPATR
090300940201     C                   MOVE      *ALL'0'       IN4049           10
090400940201     C                   MOVEA     IN4049        *IN(40)
090500940201     C*
090600940131     C* Controllo se Utente Autorizzato alla scelta
090700940202     C     S1OPZ         IFNE      0
090800940131     C                   Z-ADD     1             X
090900940223     C     S1OPZ         LOOKUP    $OP1(X)                                21
091000940211     C     *IN21         IFEQ      *OFF
091100940223     C     $FL1(X)       OREQ      'N'
091200940131     C                   MOVE      *ON           $ESCI
091300981221     C***                SETON                                        504099
091400940131     C                   ENDIF
091500940131     C*
091600940620     c* Controllo autorizzazione alla modifica su record di gruppo
091700940620     C* Reperisco tipo codice (gruppo/azienda)
091800940620     C                   MOVE      *ALL'0'       XTBKEY
091900940620     C                   MOVE      S1COD         XTBKEY
092000970512     C                   MOVE      S1UNITA       XTBUNT
092100940620     C                   MOVE      '0'           XTBALC
092200940620     C                   MOVE      '1'           XTBRIC
092300970214     C                   CALLB     'XATB'
092400940620     C                   PARM                    XATBDS
092500940620     C* Reperisco autorizzazioni utente
092600940620     C                   CLEAR                   XCHATB
092700941118     C                   MOVE      XSCSOC        XCTSOC
092800941118     C                   MOVE      XSCDUN        XCTUNI
092900940620     C                   MOVEL     KNMUS         XCTPRF
093000940620     C                   MOVE      XTBCOD        XCTCOD
093100940620     C                   CALL      'XCHKATB'
093200940620     C                   PARM                    XCHATB
093300941111     C                   MOVEL     XCTDDS        AUTDS
093400941027     C     XCTRTN        IFEQ      1
093500941027     C                   MOVEL     'N'           MANGRU
093600941027     C                   ENDIF
093700940620     C* Non ammessa manutenzione di gruppo
093800940620   1 C     XTBGRU        IFEQ      'G'
093900940620     C     MANGRU        ANDEQ     'N'
094000940620   2 C     S1OPZ         IFEQ      2
094100940729     C     S1OPZ         OREQ      3
094200940620     C     S1OPZ         OREQ      4
094300981221     C***                SETON                                        5399
094400940620   2 C                   ENDIF
094500940620   1 C                   ENDIF
094600940620     C* Non ammessa manutenzione di azienda
094700940620   1 C     XTBGRU        IFEQ      'A'
094800940620     C     MANAZI        ANDEQ     'N'
094900940620   2 C     S1OPZ         IFEQ      2
095000940729     C     S1OPZ         OREQ      3
095100940620     C     S1OPZ         OREQ      4
095200981221     C***                SETON                                        5499
095300940620   2 C                   ENDIF
095400940620   1 C                   ENDIF
095500940607     C                   ENDIF
095600940131     C                   ENDSR
095700940224     C/EJECT
095800940131     C************************************************************
095900940131     C* GESTIONE OPZIONI LISTA
096000940131     C************************************************************
096100940131     C     OPZS1         BEGSR
096200940201     C*
0963009402251    C                   SELECT
096400940225     C*
096500940225     C* se opz=Selezione passo la chiave al pgm chiamante
096600940225     C     S1OPZ         WHENEQ    1
096700970708     C                   MOVE      S1OPZ         XtaOpz
096800970708     C                   MOVE      S1COD         XtaKey
096900970708     C                   MOVE      S1UNIta       xtaUni
097000940225     C                   MOVE      *ON           $ESCI
097100940225     C                   MOVE      *ON           $FINE
097200940729     C*
097300940714     C*
097400940714     C* se opz=Abilita scrive su archivio abilitazioni
097500940714     C     S1OPZ         WHENEQ    11
097600941103     C                   MOVE      XSCSOC        ABLAZI
097700940714     C                   MOVE      *ZEROS        ABLKEY
097800940714     C                   MOVE      S1COD         ABLKEY
097900970512     C                   MOVE      S1UNITA       ABLUNT
098000940714     C                   MOVE      XTBCOD        ABLCOD
098100940714     C                   WRITE     ANABL000                             22
098200940714     C*
098300940714     C* se opz=Disabil.cancella su archivio abilitazioni
098400940714     C     S1OPZ         WHENEQ    12
098500941103     C                   MOVE      XSCSOC        ABLAZI
098600940714     C                   MOVE      *ZEROS        ABLKEY
098700940714     C                   MOVE      S1COD         ABLKEY
098800970512     C                   MOVE      S1UNITA       ABLUNT
098900940714     C                   MOVE      XTBCOD        ABLCOD
099000940714     C     KABL          DELETE    ANABL000                           22
099100940729     C*
099200940225     C*
099300940225     C* se altre opzioni (o nessuna opzione) richiamo pgm gestione
0994009402251O   C                   OTHER
099500940225     C*
099600940616     C                   RESET                   D002DS
099700940607     C                   MOVEL     S1OPZ         OPZ002
099800940616     C                   MOVE      *ZERO         KEY002
099900940607     C                   MOVE      S1COD         KEY002
100000970708     C                   MOVE      XtaLin        LIN002
100100970708     C                   MOVE      XtaAzi        AZI002
100200940715     C                   MOVE      *BLANKS       KPJBU
100300940607     C                   MOVEL     D002DS        KPJBU
100400940608$004 C                   CALL      'ANGYABR2'
100500940607     C                   PARM                    KPJBA
100600940607     C                   MOVEL     KPJBU         D002DS
100700940201     C*
100800940223     C* ritorno da PGM gestione
100900940223     C                   EXSR      GESRET
101000940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
1011009402252    C     *IN99         IFEQ      *ON
101200940223     C                   SETON                                        40
1013009402252-   C                   ENDIF
101400940131     C*
101500940506     C                   MOVE      *HIVAL        $LASTV
1016009402251-   C                   ENDSL
101700940225     C*
101800940131     C                   ENDSR
101900940223     C/EJECT
102000940223     C************************************************************
102100940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
102200940223     C************************************************************
102300940223     C     GESRET        BEGSR
102400940223     C*
102500940223     C* modo di ritorno
102600940223     C*
1027009402231    C                   SELECT
102800940314    >C* << questi modi di utilizzo dei valori di ritorno dal
102900940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
103000940314    >C*    proposte, normalmente sempre valide, ma modificabili
103100940314    >C*    per situazioni particolari >>
103200940223     C* 1 = F3
103300940314    >C     RET002        WHENEQ    '1'
103400940224     C                   MOVE      *ON           $ESCI
103500940223     C                   MOVE      *ON           $FINE
103600940223     C* 2 = F12
103700940314    >C     RET002        WHENEQ    '2'
103800940223     C                   MOVE      *ON           $ESCI
103900940223     C*
1040009402231-   C                   ENDSL
104100940223     C*
104200940223     C* operazione eseguite dal pgm chiamato
104300940223     C*
1044009402231    C                   SELECT
104500940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
104600940314    >C     OPR002        WHENEQ    '1'
104700940223     C                   MOVE      *ON           WINZS1
104800940223     C*
1049009402231-   C                   ENDSL
105000940223     C*
105100940223     C* funzione non eseguibile per errore :
105200940223     C*
1053009402231    C                   SELECT
105400940223     C* 1 = funzione richiamata chiusa in errore
105500940316    >C*  eventualmente gestire altri codici di errore
105600940314    >C     ERR002        WHENEQ    '1'
105700940223     C                   MOVE      *ON           $ESCI
105800940223     C                   SETON                                        5299
105900940223     C*
1060009402231-   C                   ENDSL
106100940223     C*
106200940223     C                   ENDSR
106300940223     C/EJECT
106400940131     C************************************************************
106500940131     C* OPERAZIONI INIZIALI
106600940131     C************************************************************
106700940131     C     *INZSR        BEGSR
106800940131     C*
106900940127     C* Reperimento parametri
107000940127     C*
107100940117     C     *ENTRY        PLIST
107200940117     C                   PARM                    KPJBA
107300940714     C*
107400940714     C* Reperimento Codice utente
107500940714     C*
107600950622     C*          KNMUS     CHAINANUTN01L             21
107700940225     C*
107800940223     C* Reperimento opzioni
107900940127     C*
108000940128     C                   EXSR      REPOPZ
108100940506     C*
108200940506     C* Reperimento tasti di funzione
108300940506     C*
108400940506     C                   EXSR      REPKEY
108500940506     C*
108600940607$007 C                   MOVE      'YAB'         XTBCOD
108700940127     C*
108800940117     C                   ENDSR
108900940225     C/EJECT
109000941103      ************************************************************
109100941103      * Reperimento dati società
109200941103      ************************************************************
109300941103     C     REPSOC        BEGSR
109400941103      *
109500970214     C                   CALLB     'XSOC'
109600941103     C                   PARM                    TIPXSC            6
109700941103     C                   PARM                    SOCXSC            3
109800941103     C                   PARM                    CDSXSC            9 0
109900941103     C                   PARM                    MODXSC            3
110000941103     C                   PARM      *BLANKS       RTNXSC            1
110100941103     C                   PARM                    XSOCDS
110200941103     C                   PARM                    KPJBA
110300941103      *
110400941103     C                   ENDSR
110500940223     C************************************************************
110600940223     C* INIZIALIZZAZIONE VARIABILI
110700940223     C************************************************************
110800940223     C     INZVAR        BEGSR
110900970926     C*
111000970926     C* Dati da PGM chiamante
111100970926     C*
111200970926     C                   MOVEL     KPJBU         XTABDS
111300951124     C*
111400951124     C* Reperimento codice lingua e codice azienda
111500951124     C*
111600951124     C                   MOVEL     'SOC001'      TIPXSC
111700951124     C                   EXSR      REPSOC
111800951124     C     RTNXSC        IFNE      '1'
111900951124     C                   MOVEL     XSOCDS        SOC001
112000951124     C                   ELSE
112100951124     C                   MOVE      *ON           $FINE
112200951124     C* fine programma
112300951124     C                   EXSR      ENDPGM
112400951124     C                   ENDIF
112500951124     C* Lingua e codice azienda
112600970708     C                   MOVE      XSCLIN        XtaLin
112700970708     C                   MOVE      XSCSOC        XtaAzi
112800940715     C*
112900940715     C                   RESET                   $FC2
113000940715     C*
113100940715$021 C                   CLOSE     ANGYABD
113200940715$021 C                   OPEN      ANGYABD
113300940506     C*
113400940506     C* Definizione delle opzioni utilizzabili a pgm
113500940506     C*
113600940506     C                   EXSR      DEFOPZ
113700940718     C* Pulizia campi e indicatori
113800940718     C                   CLEAR                   W1POS
113900940718     C                   MOVE      *ALL'0'       IN4049           10
114000940718     C                   MOVEA     IN4049        *IN(40)
114100940718     C                   CLEAR                   S1OPZ
114200940223     C* Variabili per gestione videate
114300940223     C*
114400940223     C                   MOVE      *OFF          $FINE
114500940223     C                   MOVE      *OFF          $INZS1
114600940223     C                   MOVE      *OFF          $EFILE
114700940223     C                   MOVE      *OFF          $ESCI
114800940223     C                   MOVE      *OFF          $RCDOK
114900940509     C                   MOVE      *HIVAL        $LASTV
115000940506     C                   Z-ADD     0             $ULKS1            3 0
115100940223     C*
115200940318     C                   MOVE      *ON           $INZS1
115300940223     C                   MOVE      'S1'          $GEST
115400940223     C*
115500940223     C* Variabili appoggio
115600940223     C*
115700940223     C                   Z-ADD     SFLPAG        WPAG
115800940223     C*
115900940223     C* Valorizzazione campi univoci testate
116000940223     C*
116100940607     C*                    CLEART1
116200940607     C*                    MOVELKNSIF     NOMSIF
116300940607     C*                    MOVELRAGUTE    NOMDIT
116400940607     C*                    MOVELDSPGM     SIGLA
116500940607     C*                    MOVELPROGR     NOMPGM
116600940223     C*
116700940608     c* posizione window
116800940608$012 C                   Z-ADD     34            PCOL              2 0
116900940613     C                   Z-ADD     09            R
117000940608     C     80            SUB       PCOL          C
117100940608     C                   SUB       3             C
117200970708     C     XtaOpz        IFNE      '  '
117300970708     C     XtaRig        IFGT      11
117400940608     C                   Z-ADD     1             R
117500940608     C                   ENDIF
117600940608     C*
117700970708     C     XtaCol        IFGT      40
117800940608     C                   Z-ADD     2             C
117900940608     C                   ENDIF
118000940608     C                   ENDIF
118100940616     C*
118200970708     C     XtaOpz        IFNE      '01'
118300940616$019 C                   DO        00            I                 3 0
118400940721$022 C*                  MOVEL     LIN(I)        TBTFLD
118500940721$022 C*                  CLEAR                   TBTKEY
118600940721$022 C*                  CLEAR                   TBTAZI
118700940721$022 C*                  CLEAR                   TBTLIN
118800940721$007 C                   MOVE      'YAB'         TBTCOD            3
118900940721$022 C*    K5TBT1        CHAIN     ANTBT01L                           21
119000940721$022 C*    *IN21         IFEQ      *ON
119100940721$022 C*                  Z-ADD     LLUN(I)       TBTLUN
119200940721$022 C*                  Z-ADD     LPOS(I)       TBTPOS
119300940721$022 C*                  MOVEL     LIND(I)       TBTDES
119400940721$022 C*                  WRITE     ANTBT000
119500940721$022 C*                  ENDIF
119600940616     C                   ENDDO
119700940616     C                   ENDIF
119800940608     C*
119900940722     c*
120000940722     c* Se richiamato con OPZ 11  Non controlla abilitazione all'aziend
120100970708     C     XtaOpz        IFEQ      '11'
120200940722     C                   MOVE      'N'           XTBGAB
120300940722     C                   ELSE
120400940722     C                   MOVE      'S'           XTBGAB
120500940722     C                   ENDIF
120600940223     C                   ENDSR
120700940223     C/EJECT
120800940131     C************************************************************
120900940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
121000940131     C************************************************************
121100940225     C     DEFOPZ        BEGSR
121200940131     C*
121300940225     C* Questa routine deve valorizzare la schiera $FL1
121400940225     C* con S o N a seconda della utilizzabilità o meno
121500940225     C* dell'opzione correlata
121600940225     C*
121700940714     C* Inizialmente si considerano tutte le opzioni come non valide
121800940728     C     1             DO        14            X
121900940714     C                   MOVE      'N'           $FL1(X)
122000940225     C                   END
122100940225     C*
122200940225     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
122300940714     C* rendo valide alcune opzioni
122400940714     C* i tasti invece li disattivo
122500940225     C                   SELECT
122600940714     C*
122700940714     C* Se pgm richiamato in RICERCA attivo :
122800940714     C*
122900970708    >C     XtaOpz        WHENEQ    '01'
123000940714     C* F3=Fine
123100940714     C                   Z-ADD     3             X
123200940720     C                   MOVE      '0'           $FC2(X)
123300940714     C* F10=Immissione
123400940714     C                   Z-ADD     10            X
123500940919     C                   MOVE      '1'           $FC2(X)
123600940714     C* 1=Selezione
123700940714     C                   Z-ADD     1             X
123800940714     C     01            LOOKUP    $OP1(X)                                21
123900940714     C     *IN21         IFEQ      *ON
124000940714     C                   MOVE      'S'           $FL1(X)
124100940714     C                   ENDIF
124200940729     C* 2=Modifica
124300940729     C                   Z-ADD     1             X
124400940729     C     02            LOOKUP    $OP1(X)                                21
124500940729     C     *IN21         IFEQ      *ON
124600940729     C                   MOVE      'S'           $FL1(X)
124700940729     C                   ENDIF
124800940729     C* 3=Copia
124900940729     C                   Z-ADD     1             X
125000940729     C     03            LOOKUP    $OP1(X)                                21
125100940729     C     *IN21         IFEQ      *ON
125200940729     C                   MOVE      'S'           $FL1(X)
125300940729     C                   ENDIF
125400940729     C* 4=Annullamento
125500940729     C                   Z-ADD     1             X
125600940729     C     04            LOOKUP    $OP1(X)                                21
125700940729     C     *IN21         IFEQ      *ON
125800940729     C                   MOVE      'S'           $FL1(X)
125900940729     C                   ENDIF
126000940714     C* 5=Visualizzazione
126100940714     C                   Z-ADD     1             X
126200940714     C     05            LOOKUP    $OP1(X)                                21
126300940714     C     *IN21         IFEQ      *ON
126400940714     C                   MOVE      'S'           $FL1(X)
126500940714     C                   ENDIF
126600940728     C*14=Commenti
126700940831     C**                   Z-ADD1         X
126800940831     C**         14        LOKUP$OP1,X                   21
126900940831     C**         *IN21     IFEQ *ON
127000940831     C**                   MOVE 'S'       $FL1,X
127100940831     C**                   ENDIF
127200940714     C*
127300940714     C* Se pgm richiamato in INTERROGAZIONE attivo :
127400940714     C*
127500970708    >C     XtaOpz        WHENEQ    '05'
127600940714     C* F10=Immissione
127700940714     C                   Z-ADD     10            X
127800940720     C                   MOVE      '0'           $FC2(X)
127900940714     C* F3=Fine
128000940714     C                   Z-ADD     3             X
128100940720     C                   MOVE      '0'           $FC2(X)
128200940714     C* 5=Visualizzazione
128300940225     C                   Z-ADD     1             X
128400940714     C     05            LOOKUP    $OP1(X)                                21
128500940225     C     *IN21         IFEQ      *ON
128600940714     C                   MOVE      'S'           $FL1(X)
128700940225     C                   ENDIF
128800940728     C*14=Commenti
128900940831     C**                   Z-ADD1         X
129000940831     C**         14        LOKUP$OP1,X                   21
129100940831     C**         *IN21     IFEQ *ON
129200940831     C**                   MOVE 'S'       $FL1,X
129300940831     C**                   ENDIF
129400940831     C*
129500940714     C* Se pgm richiamato in ABILITAZIONE attivo :
129600940714     C*
129700970708    >C     XtaOpz        WHENEQ    '11'
129800940714     C* F10=Immissione
129900940714     C                   Z-ADD     10            X
130000940720     C                   MOVE      '0'           $FC2(X)
130100940714     C* F3=Fine
130200940714     C                   Z-ADD     3             X
130300940720     C                   MOVE      '0'           $FC2(X)
130400940714     C* 5=Visualizzazione
130500940714     C                   Z-ADD     1             X
130600940714     C     05            LOOKUP    $OP1(X)                                21
130700940714     C     *IN21         IFEQ      *ON
130800940714     C                   MOVE      'S'           $FL1(X)
130900940714     C                   ENDIF
131000940714     C* 11=Abilitazione
131100940714     C                   Z-ADD     1             X
131200940714     C     11            LOOKUP    $OP1(X)                                21
131300940714     C     *IN21         IFEQ      *ON
131400940714     C                   MOVE      'S'           $FL1(X)
131500940714     C                   ENDIF
131600940714     C* 12=Disabilita
131700940714     C                   Z-ADD     1             X
131800940714     C     12            LOOKUP    $OP1(X)                                21
131900940714     C     *IN21         IFEQ      *ON
132000940714     C                   MOVE      'S'           $FL1(X)
132100940714     C                   ENDIF
132200940714     C*
132300940714     C* Se pgm richiamato in GESTIONE  attivo :
132400940714     C*
132500970708    >C     XtaOpz        WHENEQ    '02'
132600940714     C* F3=Fine
132700940714     C                   Z-ADD     3             X
132800940720     C                   MOVE      '0'           $FC2(X)
132900940714     C* 2=Modifica
133000940714     C                   Z-ADD     1             X
133100940714     C     02            LOOKUP    $OP1(X)                                21
133200940714     C     *IN21         IFEQ      *ON
133300940714     C                   MOVE      'S'           $FL1(X)
133400940714     C                   ENDIF
133500940714     C* 3=Copia
133600940714     C                   Z-ADD     1             X
133700940714     C     03            LOOKUP    $OP1(X)                                21
133800940714     C     *IN21         IFEQ      *ON
133900940714     C                   MOVE      'S'           $FL1(X)
134000940714     C                   ENDIF
134100940714     C* 4=Annullamento
134200940714     C                   Z-ADD     1             X
134300940714     C     04            LOOKUP    $OP1(X)                                21
134400940714     C     *IN21         IFEQ      *ON
134500940714     C                   MOVE      'S'           $FL1(X)
134600940714     C                   ENDIF
134700940714     C* 5=Visualizzazione
134800940714     C                   Z-ADD     1             X
134900940714     C     05            LOOKUP    $OP1(X)                                21
135000940714     C     *IN21         IFEQ      *ON
135100940714     C                   MOVE      'S'           $FL1(X)
135200940714     C                   ENDIF
135300940729     C*14=Commenti
135400940831     C**                   Z-ADD1         X
135500940831     C**         14        LOKUP$OP1,X                   21
135600940831     C**         *IN21     IFEQ *ON
135700940831     C**                   MOVE 'S'       $FL1,X
135800940831     C**                   ENDIF
135900940225     C*
136000940225     C                   OTHER
136100940714     C* Se pgm non richiamato, attivo TUTTO  :
136200940714     C* 2=Modifica
136300940714     C                   Z-ADD     1             X
136400940714     C     02            LOOKUP    $OP1(X)                                21
136500940714     C     *IN21         IFEQ      *ON
136600940714     C                   MOVE      'S'           $FL1(X)
136700940714     C                   ENDIF
136800940714     C* 3=Copia
136900940714     C                   Z-ADD     1             X
137000940714     C     03            LOOKUP    $OP1(X)                                21
137100940714     C     *IN21         IFEQ      *ON
137200940714     C                   MOVE      'S'           $FL1(X)
137300940714     C                   ENDIF
137400940714     C* 4=Annullamento
137500940714     C                   Z-ADD     1             X
137600940714     C     04            LOOKUP    $OP1(X)                                21
137700940714     C     *IN21         IFEQ      *ON
137800940714     C                   MOVE      'S'           $FL1(X)
137900940714     C                   ENDIF
138000940714     C* 5=Visualizzazione
138100940714     C                   Z-ADD     1             X
138200940714     C     05            LOOKUP    $OP1(X)                                21
138300940714     C     *IN21         IFEQ      *ON
138400940714     C                   MOVE      'S'           $FL1(X)
138500940714     C                   ENDIF
138600940714     C* 11=Abilitazione
138700940714     C                   Z-ADD     1             X
138800940714     C     11            LOOKUP    $OP1(X)                                21
138900940714     C     *IN21         IFEQ      *ON
139000940714     C                   MOVE      'S'           $FL1(X)
139100940714     C                   ENDIF
139200940714     C* 12=Disabilita
139300940714     C                   Z-ADD     1             X
139400940714     C     12            LOOKUP    $OP1(X)                                21
139500940714     C     *IN21         IFEQ      *ON
139600940714     C                   MOVE      'S'           $FL1(X)
139700940714     C                   ENDIF
139800940728     C*14=Commenti
139900940831     C**                   Z-ADD1         X
140000940831     C**         14        LOKUP$OP1,X                   21
140100940831     C**         *IN21     IFEQ *ON
140200940831     C**                   MOVE 'S'       $FL1,X
140300940831     C**                   ENDIF
140400940225     C* poi, disattivo a seconda delle autorizzazioni :
140500940314    >C*********  AUTXXX    IFEQ 'N'
140600940314    >C*********            Z-ADD1         X
140700940314    >C*********  'XX'      LOKUP$OP1,X                   21
140800940314    >C*********  *IN21     IFEQ *ON
140900940314    >C*********            MOVE 'N'       $FL1,X
141000940314    >C*********            ENDIF
141100940314    >C*********            ENDIF
141200940225     C*
141300940225     C                   ENDSL
141400940225     C*
141500940225     C                   ENDSR
141600940225     C/EJECT
141700940225     C************************************************************
141800940225     C* REPERIMENTO OPZIONI
141900940225     C************************************************************
142000940225     C     REPOPZ        BEGSR
142100940225     C*
142200940314     C     1             DO        $DIMOP        X
142300940128     C*
142400940223     C     $MS1(X)       IFNE      *BLANK
142500940128     C*
142600940223     C                   MOVEL     $MS1(X)       IDMSG
142700970214     C                   CALLB     'XRTVM'
142800940201     C                   PARM                    IDMSG            27
142900940201     C                   PARM                    TXTMSG          100
143000940201     C                   PARM                    ERRMSG            1
143100940128     C*
143200940128     C     ERRMSG        IFNE      *ON
143300940223     C                   MOVEL     TXTMSG        $TX1(X)
143400940128     C                   ELSE
143500940223     C                   MOVEL     *ALL'?'       $TX1(X)
143600940128     C                   END
143700940128     C*
143800940128     C                   END
143900940128     C*
144000940128     C                   END
144100940128     C*
144200940128     C                   ENDSR
144300940506     C/EJECT
144400940506     C************************************************************
144500940506     C* REPERIMENTO TASTI DI FUNZIONE
144600940506     C************************************************************
144700940506     C     REPKEY        BEGSR
144800940506     C*
144900940506     C                   MOVEA     $FM2          $FM
145000940506     C                   EXSR      REPKKK
145100940506     C                   MOVEA     $FX           $FX2
145200940506     C                   ENDSR
145300940506     C/EJECT
145400940506     C************************************************************
145500940506     C* REPERIMENTO TASTI DI FUNZIONE
145600940506     C************************************************************
145700940506     C     REPKKK        BEGSR
145800940506     C*
145900940506     C                   CLEAR                   $FX
146000940506     C     1             DO        24            X
146100940506     C*
146200940506     C     $FM(X)        IFNE      *BLANK
146300940506     C*
146400940506     C                   MOVEL     $FM(X)        IDMSG
146500970214     C                   CALLB     'XRTVM'
146600940506     C                   PARM                    IDMSG            27
146700940506     C                   PARM                    TXTMSG          100
146800940506     C                   PARM                    ERRMSG            1
146900940506     C*
147000940506     C     ERRMSG        IFNE      *ON
147100940506     C                   MOVEL     TXTMSG        $FX(X)
147200940506     C                   ELSE
147300940506     C                   MOVEL     *ALL' '       $FX(X)
147400940506     C                   END
147500940506     C*
147600940506     C                   END
147700940506     C*
147800940506     C                   END
147900940506     C*
148000940506     C                   ENDSR
148100940223     C/EJECT
148200940128     C************************************************************
148300940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
148400940128     C************************************************************
148500940223     C     DEFCAM        BEGSR
148600940128     C*
148700940223     C* Variabili per gestione videate
148800940223     C*
148900940223     C                   MOVE      *BLANK        $GEST             2
149000940223     C                   MOVE      *BLANK        $FINE             1
149100940223     C                   MOVE      *BLANK        $INZS1            1
149200940223     C                   MOVE      *BLANK        $LASTG            2
149300940223     C                   MOVE      *BLANK        $LASTV            2
149400940223     C                   MOVE      *BLANK        $EFILE            1
149500940223     C                   MOVE      *BLANK        $ESCI             1
149600940223     C                   MOVE      *BLANK        $RCDOK            1
149700940223     C*
149800940314     C* Variabili appoggio sempre presenti in tutte le anagrafiche
149900940223     C*
150000940223     C     *LIKE         DEFINE    C1RCD         S1NRR
150100940223     C     *LIKE         DEFINE    C1NRR         WSFL
150200940223     C     *LIKE         DEFINE    C1RCD         WMAX
150300940223     C     *LIKE         DEFINE    C1RCD         WPAG
150400940223     C     *LIKE         DEFINE    $INZS1        WINZS1
150500940223     C                   MOVE      *BLANK        WWGRP             3
150600940223     C                   Z-ADD     0             CURR              2 0
150700940223     C                   Z-ADD     0             CURC              2 0
150800940223     C                   MOVE      *BLANK        WIN              99
150900940314     C     *LIKE         DEFINE    C1POS         W1POS
151000940620     C*
151100940721$022 C*    K5TBT1        KLIST
151200940721$022 C*                  KFLD                    TBTAZI
151300940721$022 C*                  KFLD                    TBTCOD
151400940721$022 C*                  KFLD                    TBTKEY
151500940721$022 C*                  KFLD                    TBTLIN
151600940721$022 C*                  KFLD                    TBTFLD
151700940714     C*
151800940714     C     KABL          KLIST
151900940714     C                   KFLD                    ABLCOD
152000940714     C                   KFLD                    ABLKEY
152100940714     C                   KFLD                    ABLAZI
152200940314     C*
152300940314     C*
152400940223     C* Indici
152500940223     C*
152600940223     C                   Z-ADD     0             X                 3 0
152700940223     C                   Z-ADD     0             Y                 3 0
152800940127     C*
152900940117     C                   ENDSR
153000940223     C/EJECT
153100940616**
153200940225PROMSG    *LIBL     OPZ0001
153300940225PROMSG    *LIBL     OPZ0002
153400940131PROMSG    *LIBL     OPZ0003
153500940131PROMSG    *LIBL     OPZ0004
153600940131PROMSG    *LIBL     OPZ0005
153700940224PROMSG    *LIBL     OPZ0006
153800940224PROMSG    *LIBL     OPZ0007
153900940224PROMSG    *LIBL     OPZ0008
154000940224PROMSG    *LIBL     OPZ0009
154100970716PROMSG    *LIBL     OPZ0010
154200970716PROMSG    *LIBL     OPZ0013
154300970716PROMSG    *LIBL     OPZ0012
154400970716PROMSG    *LIBL     OPZ0011
154500970716PROMSG    *LIBL     OPZ0014
154600940201**
15470094022501S      <Selezione
15480094022502S      <Modifica
15490094022503S      <Copia
15500094022504S      <Annullamento
15510094022505S      <Visualizzazione
15520094060706N      <prova
15530094060707N      <prova
15540094060708N      <prova
15550094060709N      <prova
15560094072910S      <Invia
15570094071411S      <Abilita gruppo
15580094071412S      <Disabilita
15590094072813N      <PROVA
15600094083114N      <Commenti
156100940506** TASTI DI FUNZIONE  S1
156200940506PROMSG    *LIBL     KEY0001  01
156300940506PROMSG    *LIBL     KEY0002  02
156400940506PROMSG    *LIBL     KEY0003  03
156500940506PROMSG    *LIBL     KEY0004  04
156600940506PROMSG    *LIBL     KEY0005  05
156700940506PROMSG    *LIBL     KEY0006  06
156800940506PROMSG    *LIBL     KEY0007  07
156900940506PROMSG    *LIBL     KEY0008  08
157000940506PROMSG    *LIBL     KEY0009  09
157100970716PROMSG    *LIBL     KEY0010  10
157200970716PROMSG    *LIBL     KEY0011  11
157300970716PROMSG    *LIBL     KEY0012  12
157400970716PROMSG    *LIBL     KEY0013  13
157500970716PROMSG    *LIBL     KEY0014  14
157600970716PROMSG    *LIBL     KEY0015  15
157700970716PROMSG    *LIBL     KEY0016  16
157800970716PROMSG    *LIBL     KEY0017  17
157900970716PROMSG    *LIBL     KEY0018  18
158000970716PROMSG    *LIBL     KEY0019  19
158100940506PROMSG    *LIBL     KEY0020  20
158200940506PROMSG    *LIBL     KEY0021  21
158300940506PROMSG    *LIBL     KEY0022  22
158400940506PROMSG    *LIBL     KEY0023  23
158500940506PROMSG    *LIBL     KEY0024  24
158600940506**  ABILITAZIONE S1
1587009410140N  01
1588009407200N  02
1589009407201N  03
1590009407200N  04
1591009407200N  05
1592009407200N  06
1593009407200N  07
1594009407200N  08
1595009407200N  09
1596009407201N  10
1597009407200N  11
1598009407201N  12
1599009407200N  13
1600009407200N  14
1601009407200N  15
1602009407200N  16
1603009407200N  17
1604009407200N  18
1605009407200N  19
1606009407200N  20
1607009407200N  21
1608009407200N  22
1609009505020N  23
1610009407200N  24
