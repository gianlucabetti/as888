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
003900940715$001 FANGY4QD   CF   E             WORKSTN USROPN
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
014200940607$003 D EXTDS         E DS                  EXTNAME(ANGY4QDS)
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
018200981013$015 I              S1BAN                       §4QBAN
018300981013$015 I              S1LIN                       §4QLIN
018400981013$015 I              S1ABI                       §4QABI
018500981013$015 I              S1CAB                       §4QCAB
018600981013$015 I              S1CCB                       §4QCCB
018700940207     I*-------------
018800940127     I*
018900940127     C*----------------------------------------------------*
019000940127     C*                MAIN LINE PROGRAM
019100940127     C*----------------------------------------------------*
019200940127     C*
019300940223     C* inizializzazione variabili
019400940223     C                   EXSR      INZVAR
019500940223     C*
019600940223     C     $FINE         DOWEQ     *OFF
019700940131     C     $GEST         CASEQ     'S1'          GESS1
019800940117     C                   END
019900940117     C                   END
020000940325     C*
020100940325     C* fine programma
020200940325     C                   EXSR      ENDPGM
020300940225     C*
020400940325     C/EJECT
020500940325     C************************************************************
020600940325     C* FINE PROGRAMMA
020700940325     C************************************************************
020800940325     C     ENDPGM        BEGSR
020900940325     C*
021000940325     C* passaggio dati al pgm chiamante
021100940715     C                   MOVE      *BLANKS       KPJBU
021200940607     C                   MOVEL     XTABDS        KPJBU
021300940325     C*
021400940325     C* Se pgm Guida non è stato richiamato posso chiudere in LR
021500970708     C     XtaOpz        IFEQ      *BLANK
021600940325     C                   SETON                                            LR
021700960906     C                   RETURN
021800940325     C* altrimenti chiudo in RT
021900940325     C                   ELSE
022000940325     C                   SETON                                            RT
022100960906     C                   RETURN
022200940325     C                   ENDIF
022300940325     C*
022400940325     C                   ENDSR
022500940131     C/EJECT
022600940224     C************************************************************
022700940131     C* RIEMPIMENTO OPZIONI
022800940224     C************************************************************
022900940131     C     RIEOPZ        BEGSR
023000940223     C*
023100940223     C* Input :
023200940223     C* - $TX = schiera con tutti i testi disponibili
023300940223     C* - $FL = schiere con i flag di validità associati ai testi
023400940225     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
023500940223     C*          opzione usato
023600940223     C* - WMXO = variabile contenente il numero massimo di testi
023700940223     C*          opzione usabili
023800940223     C* Output :
023900940223     C* - WOPZ1 = variabile contenente la prima riga opzioni
024000940223     C* - WOPZ2 = variabile contenente la seconda riga opzioni
024100950502     C* - FF23  = flag che indica se c'è bisogno del tasto F23
024200940223     C*
024300970214     C                   CALLB     'X04OPMSG'
024400940223     C* input
024500940223     C                   PARM                    $TX
024600940223     C                   PARM                    $FL
024700940225     C                   PARM                    WCOZ              3 0
024800940223     C                   PARM                    WMXO              3 0
024900940223     C* output
025000940223     C                   PARM                    WOPZ1            76
025100940223     C                   PARM                    WOPZ2            76
025200950502     C                   PARM                    FF23              1
025300940607     C* input
025400940607$018 C                   PARM      65            $L01              3 0
025500940607     C                   PARM      0             $L02              3 0
025600940223     C*
025700940131     C                   ENDSR
025800940506     C/EJECT
025900940506     C************************************************************
026000940506     C* RIEMPIMENTO TASTI FUNZIONE
026100940506     C************************************************************
026200940506     C     RIEKEY        BEGSR
026300940506     C*
026400940506     C* Input :
026500940506     C* - $FX = schiera con tutti i testi disponibili
026600940506     C* - $FC = schiere con i flag di validità associati ai testi
026700940506     C* - $FR = schiere con i flag di REVERSE IMAGE
026800940506     C* - $ULK = ultimo testo caricato
026900940506     C* Output :
027000940506     C* - $ULK
027100940506     c* - $KEY1= 1° riga di tasti funzione
027200940506     c* - $KEY2= 2° riga di tasti funzione
027300940506     C*
027400970214     C                   CALLB     'XKEYMSG'
027500940506     C* input
027600940506     C                   PARM                    $FX
027700940506     C                   PARM                    $FC
027800940506     C                   PARM                    $FR
027900940506     C                   PARM                    $ULK              3 0
028000940506     C* output
028100940506     C                   PARM                    $KEY1            79
028200940506     C                   PARM                    $KEY2            79
028300940607     C* input
028400940607$018 C                   PARM      65            $L01              3 0
028500941004$018 C                   PARM      65            $L02              3 0
028600940506     C*
028700940506     C                   ENDSR
028800940224     C/EJECT
028900940224     C************************************************************
029000940224     C* GESTIONE EMISSIONE TASTI FUNZIONE
029100940224     C************************************************************
029200940224     C     GESPIE        BEGSR
029300940224     C*
029400940224     C                   SELECT
029500940506     C* Lista
029600940506     C     $GEST         WHENEQ    'S1'
029700940506     C     $ULKS1        IFEQ      0
029800940506     C                   EXSR      F24S1
029900940506     C                   ELSE
030000940607     C                   WRITE     Z1
030100940506     C                   ENDIF
030200940506     C                   ENDSL
030300940506     C*
030400940224     C                   ENDSR
030500940506     C/EJECT
030600940506     C************************************************************
030700940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA S1
030800940506     C************************************************************
030900940506     C     CTRKS1        BEGSR
031000940506     C                   SETOFF                                       99
031100940506     C                   MOVEA     $FC2          $FC
031200940506     C                   EXSR      CTRKEY
031300940506     C     *IN99         IFEQ      *ON
031400940506     C                   SETON                                        97
031500940506     C                   ENDIF
031600940607     C*
031700940506     C                   ENDSR
031800940506     C/EJECT
031900940506     C************************************************************
032000940506     C* CONTROLLO ABILITAZIONE TASTI
032100940509     C* INPUT
032200940509     c* $TASTO =Tasto funzione premuto
032300940509     C* $FC = Tasti abilitati
032400940509     C* OUTPUT
032500940509     C* IN99 = ERrore
032600940506     C************************************************************
032700940506     C     CTRKEY        BEGSR
032800940506     C                   SELECT
032900940506     C     $TASTO        WHENEQ    F01
033000940720     C     $FC(1)        IFEQ      '0'
033100940506     C                   SETON                                        99
033200940506     C                   ENDIF
033300940506     C     $TASTO        WHENEQ    F02
033400940720     C     $FC(2)        IFEQ      '0'
033500940506     C                   SETON                                        99
033600940506     C                   ENDIF
033700940506     C     $TASTO        WHENEQ    F03
033800940720     C     $FC(3)        IFEQ      '0'
033900940506     C                   SETON                                        99
034000940506     C                   ENDIF
034100940506     C     $TASTO        WHENEQ    F04
034200940720     C     $FC(4)        IFEQ      '0'
034300940506     C                   SETON                                        99
034400940506     C                   ENDIF
034500940506     C     $TASTO        WHENEQ    F05
034600940720     C     $FC(5)        IFEQ      '0'
034700940506     C                   SETON                                        99
034800940506     C                   ENDIF
034900940506     C     $TASTO        WHENEQ    F06
035000940720     C     $FC(6)        IFEQ      '0'
035100940506     C                   SETON                                        99
035200940506     C                   ENDIF
035300940506     C     $TASTO        WHENEQ    F07
035400940720     C     $FC(7)        IFEQ      '0'
035500940506     C                   SETON                                        99
035600940506     C                   ENDIF
035700940506     C     $TASTO        WHENEQ    F08
035800940720     C     $FC(8)        IFEQ      '0'
035900940506     C                   SETON                                        99
036000940506     C                   ENDIF
036100940506     C     $TASTO        WHENEQ    F09
036200940720     C     $FC(09)       IFEQ      '0'
036300940506     C                   SETON                                        99
036400940506     C                   ENDIF
036500940506     C     $TASTO        WHENEQ    F10
036600940720     C     $FC(10)       IFEQ      '0'
036700940506     C                   SETON                                        99
036800940506     C                   ENDIF
036900940506     C     $TASTO        WHENEQ    F11
037000940720     C     $FC(11)       IFEQ      '0'
037100940506     C                   SETON                                        99
037200940506     C                   ENDIF
037300940506     C     $TASTO        WHENEQ    F12
037400940720     C     $FC(12)       IFEQ      '0'
037500940506     C                   SETON                                        99
037600940506     C                   ENDIF
037700940506     C     $TASTO        WHENEQ    F13
037800940720     C     $FC(13)       IFEQ      '0'
037900940506     C                   SETON                                        99
038000940506     C                   ENDIF
038100940506     C     $TASTO        WHENEQ    F14
038200940720     C     $FC(14)       IFEQ      '0'
038300940506     C                   SETON                                        99
038400940506     C                   ENDIF
038500940506     C     $TASTO        WHENEQ    F15
038600940720     C     $FC(15)       IFEQ      '0'
038700940506     C                   SETON                                        99
038800940506     C                   ENDIF
038900940506     C     $TASTO        WHENEQ    F16
039000940720     C     $FC(16)       IFEQ      '0'
039100940506     C                   SETON                                        99
039200940506     C                   ENDIF
039300940506     C     $TASTO        WHENEQ    F17
039400940720     C     $FC(17)       IFEQ      '0'
039500940506     C                   SETON                                        99
039600940506     C                   ENDIF
039700940506     C     $TASTO        WHENEQ    F18
039800940720     C     $FC(18)       IFEQ      '0'
039900940506     C                   SETON                                        99
040000940506     C                   ENDIF
040100940506     C     $TASTO        WHENEQ    F19
040200940720     C     $FC(19)       IFEQ      '0'
040300940506     C                   SETON                                        99
040400940506     C                   ENDIF
040500940506     C     $TASTO        WHENEQ    F20
040600940720     C     $FC(20)       IFEQ      '0'
040700940506     C                   SETON                                        99
040800940506     C                   ENDIF
040900940506     C     $TASTO        WHENEQ    F21
041000940720     C     $FC(21)       IFEQ      '0'
041100940506     C                   SETON                                        99
041200940506     C                   ENDIF
041300940506     C     $TASTO        WHENEQ    F22
041400940720     C     $FC(22)       IFEQ      '0'
041500940506     C                   SETON                                        99
041600940506     C                   ENDIF
041700940506     C     $TASTO        WHENEQ    F23
041800940720     C     $FC(23)       IFEQ      '0'
041900940506     C                   SETON                                        99
042000940506     C                   ENDIF
042100940506     C     $TASTO        WHENEQ    F24
042200940720     C     $FC(24)       IFEQ      '0'
042300940506     C                   SETON                                        99
042400940506     C                   ENDIF
042500940506     C                   ENDSL
042600940506     C                   ENDSR
042700940224     C/EJECT
042800940127     C************************************************************
042900940131     C* GESTIONE LISTA
043000940127     C************************************************************
043100940127     C     GESS1         BEGSR
043200940223     C* inizializzazione videata
043300940223     C     $INZS1        IFEQ      *ON
043400940127     C                   EXSR      INZS1
043500940223     C                   MOVE      *OFF          $INZS1
043600940127     C                   ENDIF
043700940223     C* emissione piede videata se proveniente da altra
043800940223     C* salvataggio valore formato di provenienza
043900940202     C     $LASTV        IFNE      'S1'
044000940607!!   C*                    WRITEFMTT1
044100940224     C                   EXSR      GESPIE
044200940202     C                   MOVE      $LASTV        $LASTG
044300940202     C                   MOVE      'S1'          $LASTV
044400940202     C                   END
044500940223     C*
044600940223     C     WMAX          IFEQ      0
044700940607     C                   WRITE     D1
044800940211     C                   ENDIF
044900940729     C                   MOVE      '1'           WIN727            1
045000940127     C*
045100940607     C                   EXFMT     C1
045200940204     C     C1NRR         IFNE      0
045300940204     C                   Z-ADD     C1NRR         WSFL
045400940204     C                   ENDIF
045500940127     C                   Z-ADD     SFLNRR        C1RCD
045600940506     C*
045700940506     C* controllo abilitazione tasti funzione
045800940506     C*
045900940506     C                   EXSR      CTRKS1
046000940506     C     *IN99         IFEQ      *OFF
046100940506     C*
046200940127     C*
0463009401271    C                   SELECT
046400940127     C* F3=Fine
046500940506     C     $TASTO        WHENEQ    F03
046600940309     C                   EXSR      F03S1
046700940131     C* F10=Immissione
046800940506     C     $TASTO        WHENEQ    F10
046900940309     C                   EXSR      F10S1
047000940131     C* F12=Ritorno
047100940506     C     $TASTO        WHENEQ    F12
047200940309     C                   EXSR      F12S1
047300940224     C* F23=Altre opzioni
047400940506     C     $TASTO        WHENEQ    F23
047500940309     C                   EXSR      F23S1
047600940224     C* F24=Altri tasti
047700940506     C     $TASTO        WHENEQ    F24
047800940309     C                   EXSR      F24S1
047900940127     C* ROLL-UP
048000940506     C     $TASTO        WHENEQ    ROLLUP
048100940127     C                   EXSR      ROLS1
048200940127     C*
0483009401271O   C                   OTHER
048400940127     C* CONTROLLO DATI
048500940131     C                   EXSR      CTRC1
048600940201     C     *IN99         IFEQ      *OFF
048700940131     C                   EXSR      CTRS1
048800940131     C                   END
0489009401271-   C                   ENDSL
049000940127     C*
049100940506     C                   ENDIF
049200940127     C                   ENDSR
049300940224     C/EJECT
049400940127     C************************************************************
049500940131     C* INIZIALIZZAZIONE LISTA
049600940127     C************************************************************
049700940127     C     INZS1         BEGSR
049800940302     C* pulizia SFL
049900940128     C                   SETOFF                                         3031
050000940607     C                   WRITE     C1
050100940128     C                   SETON                                          31
050200940127     C*
050300940128     C* riempimento opzioni
050400940223     C                   MOVEA     $TX1          $TX
050500940223     C                   MOVEA     $FL1          $FL
050600940309     C                   Z-ADD     0             WCOZ
050700940225     C*  equivale al num.di elementi della schiera $TX1
050800940509     C                   Z-ADD     $DIMOP        WMXO
050900940128     C                   EXSR      RIEOPZ
051000950502     C                   Z-ADD     23            X
051100950502     C                   MOVE      FF23          $FC2(X)
051200940608     C                   MOVEL     WOPZ1         C1OPZ1
051300940608     C**                   MOVELWOPZ2     C1OPZ2
051400940225     C                   Z-ADD     WCOZ          C1COZ
051500940128     C*
051600940128     C* CARICAMENTO SFL A PAGINE
051700940128     C*
051800940201     C                   Z-ADD     0             S1NRR
051900940128     C                   Z-ADD     0             WMAX
052000940224     C*
052100940224     C* Posizionamento su file pilota
052200940302     C                   CLEAR                   C1POS
052300940316    >C                   EXSR      SETANA
052400940608    >C                   EXSR      REDANA
052500940302     C* ripristino i valori del posizionamento
052600940302     C                   MOVE      W1POS         C1POS
052700940131     C*
052800940131     C* Carico 1a pagina
052900940127     C                   EXSR      ROLS1
053000940201     C                   Z-ADD     SFLPAG        WPAG
053100940127     C*
053200940127     C                   ENDSR
053300940127     C/EJECT
053400940127     C************************************************************
053500940131     C* CARICAMENTO PAGINA LISTA
053600940127     C************************************************************
053700940127     C     ROLS1         BEGSR
053800940127     C*
053900940128     C                   SETOFF                                       32
054000940223     C                   Z-ADD     0             Y
054100940127     C                   Z-ADD     WMAX          S1NRR
054200940127     C*
054300940127     C* Leggo dal file anagrafico per caricare la lista
054400940127     C*
0545009401311    C     $EFILE        DOWEQ     *OFF
054600940201     C     Y             ANDLT     WPAG
054700940127     C*
054800940131     C                   EXSR      RIES1
054900940127     C*
055000940127     C                   ADD       1             S1NRR
055100940127     C                   ADD       1             Y
055200940714     C                   MOVE      $IN41         *IN41
055300940127     C*
055400940607     C                   WRITE     S1
055500940131     C*
055600940316    >C                   EXSR      REDANA
055700940128     C*
0558009401271-   C                   ENDDO
055900940127     C*
056000940223     C                   Z-ADD     S1NRR         WMAX                 30
056100940127     C*
056200940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
056300940127     C*
056400940201     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
056500940127     C                   MVR                     RESTO             3 0
056600940131     C     PAGINE        MULT      SFLPAG        C1RCD
0567009401271    C     RESTO         IFGT      0
056800940127     C                   ADD       1             C1RCD
0569009401271E   C                   ELSE
057000940407     C                   SUB       SFLPAG        C1RCD
057100940407     C                   ADD       1             C1RCD
0572009401271-   C                   ENDIF
057300940128     C*
057400940127     C                   ENDSR
057500940224     C/EJECT
057600940224     C************************************************************
057700940224     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
057800940224     C************************************************************
057900940607     C     SETANA        BEGSR
058000940224     C*
058100941004     C                   MOVE      *BLANKS       XTBKEY
058200970708     C                   MOVE      XtaAzi        XTBAZI
058300970708     C                   MOVE      XtaLin        XTBLIN
058400940608     C                   MOVE      '9'           XTBRIC
058500940607     C                   MOVE      '0'           XTBALC
058600950622     C                   MOVE      XSCCUT        XTBUTE
058700940607     C                   MOVE      '0'           XTBERR
058800941121     C**         W1POS     IFNE *BLANK
058900941004     C                   MOVE      *ZEROS        XTBKEY
059000940608     C                   MOVE      W1POS         XTBKEY
059100941121     C**                   END
059200970214     C                   CALLB     'XATB'
059300940607     C                   PARM                    XATBDS
059400940607     C*
059500940607     C     XTBERR        IFNE      '0'
059600940607     C                   MOVEL     *ON           $EFILE
059700940607     C                   ENDIF
059800940224     C*
059900940224     C                   ENDSR
060000940128     C/EJECT
060100940128     C************************************************************
060200940131     C* LETTURA RCD ARCHIVIO PILOTA
060300940128     C************************************************************
060400940607     C     REDANA        BEGSR
060500940128     C*
060600940131     C                   MOVEL     *OFF          $EFILE
060700940131     C                   MOVEL     *OFF          $RCDOK
060800940131     C*
0609009401311    C     $EFILE        DOUEQ     *ON
061000940131     C     $RCDOK        OREQ      *ON
061100940131     C*
061200940607     C                   MOVE      '3'           XTBRIC
061300940607     C                   MOVE      '0'           XTBERR
061400970214     C                   CALLB     'XATB'
061500940607     C                   PARM                    XATBDS
061600940607     C*
061700940607     C     XTBERR        IFNE      '0'
061800940607     C                   MOVEL     *ON           $EFILE
061900940607     C                   ENDIF
062000940131     C*
0621009401312    C     $EFILE        IFEQ      *OFF
062200940607     C                   EXSR      SELANA
062300950502     C                   MOVE      XTBKEY        SAVKEY
062400950502     C                   MOVE      XTBGRU        SAVGRU
062500950502     C                   MOVE      XTBUNI        SAVUNI
062600970509     C                   MOVE      XTBUNT        SAVUNT
0627009401312-   C                   ENDIF
062800940131     C*
0629009401311-   C                   ENDDO
063000940131     C*
063100940607     C                   MOVE      $EFILE        *IN33
063200940131     C*
063300940131     C                   ENDSR
063400940224     C/EJECT
063500940224     C************************************************************
063600940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
063700940224     C************************************************************
063800940224     C     CLCPAG        BEGSR
063900940224     C* Input :
064000940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
064100940224     C*          cursore
064200940224     C* - SFLPAG = numero rcd per pagina sfl
064300940224     C* Output :
064400940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
064500940224     C*
064600940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
064700940224     C                   MVR                     RESTO             3 0
064800940224     C     RESTO         IFGT      0
064900940224     C                   ADD       1             PAGINE
065000940224     C                   ENDIF
065100940224     C     PAGINE        MULT      SFLPAG        WPAG
065200940224     C*
065300940224     C                   ENDSR
065400940131     C/EJECT
065500940131     C************************************************************
065600940131     C* SELEZIONI SULLA RIGA LETTA
065700940314     C* impostare a *OFF il campo $RCDOK per scartare un record che
065800940314     C* non rientra nei parametri di parzializzazione
065900940314     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
066000940314     C* superiore stabilito nei parametri
066100940314     C*
066200940131     C************************************************************
066300940607     C     SELANA        BEGSR
066400940131     C*
066500940202     C                   MOVEL     *ON           $RCDOK
066600940714     C* Se aperto il pgm con opzione 11
066700940714     C* carico solo i record di gruppo
066800970708     C     XtaOpz        IFEQ      '11'
066900940714     C     XTBGRU        ANDNE     'G'
067000940714     C                   MOVEL     *OFF          $RCDOK
067100940714     C                   END
067200940714     C* Carica in alta intensità solo i record
067300940714     C* abilitati
067400940714     C                   MOVE      '0'           $IN41
067500970708     C     XtaOpz        IFEQ      '11'
067600940714     C     XTBGRU        ANDEQ     'G'
067700940715     C     XTBAZB        ANDNE     'N'
067800940714     C                   MOVE      '1'           $IN41
067900940714     C                   END
068000940714     C*
068100940202     C                   ENDSR
068200940131     C/EJECT
068300940131     C************************************************************
068400940131     C* RIEMPIMENTO RIGA LISTA
068500940131     C************************************************************
068600940131     C     RIES1         BEGSR
068700950502     C*                    MOVE XTBKEY    S1COD
068800950502     C*                    MOVE XTBGRU    S1GRU
068900950502     C                   MOVEL     SAVUNI        EXTDS
069000950502     C                   MOVE      SAVKEY        S1COD
069100950502     C                   MOVE      SAVGRU        S1GRU
069200970512     C                   MOVE      SAVUNT        S1UNITA           8
069300950502     C     *LIKE         DEFINE    XTBUNI        SAVUNI
069400950502     C     *LIKE         DEFINE    XTBKEY        SAVKEY
069500950502     C     *LIKE         DEFINE    XTBGRU        SAVGRU
069600970509     C     *LIKE         DEFINE    XTBUNT        SAVUNT
069700940131     C*
069800940131     C                   ENDSR
069900940309     C/EJECT
070000940309     C************************************************************
070100940309     C* GESTIONE F03 VIDEO S1
070200940309     C************************************************************
070300940309     C     F03S1         BEGSR
070400940309     C*
070500940309     C                   MOVE      *ON           $FINE
070600970708     C                   MOVE      '1'           XtaRet
070700940325     C* fine programma
070800940325     C                   EXSR      ENDPGM
070900940309     C*
071000940309     C                   ENDSR
071100940309     C/EJECT
071200940309     C************************************************************
071300940309     C* GESTIONE F10 VIDEO S1
071400940314     c* AGGIUNTA RECORD
071500940309     C************************************************************
071600940309     C     F10S1         BEGSR
071700940309     C*
071800940607     C                   RESET                   D002DS
071900940607     C                   MOVEL     '01'          OPZ002
072000970708     C                   MOVE      XtaLin        LIN002
072100970708     C                   MOVE      XtaAzi        AZI002
072200970512     C                   MOVE      S1UNITA       UNI002
072300940715     C                   MOVE      *BLANKS       KPJBU
072400940607     C                   MOVEL     D002DS        KPJBU
072500940608$004 C                   CALL      'ANGY4QR2'
072600940309     C                   PARM                    KPJBA
072700940607     C                   MOVEL     KPJBU         D002DS
072800940309     C* ritorno da PGM gestione
072900940309     C                   EXSR      GESRET
073000940309     C     WINZS1        IFEQ      *ON
073100940309     C                   MOVE      *ON           $INZS1
073200940309     C* se esistevano già righe sul sfl
073300940309     C* calcolo pagina a cui deve posizionarsi
073400940309     C     WSFL          IFGT      0
073500940309     C                   EXSR      CLCPAG
073600940309     C* altrimenti carico solo la 1a pagina
073700940309     C                   ELSE
073800940309     C                   Z-ADD     SFLPAG        WPAG
073900940309     C                   ENDIF
074000940309     C                   ENDIF
074100940309     C*
074200940712     C                   MOVE      *HIVAL        $LASTV
074300940309     C                   ENDSR
074400940309     C/EJECT
074500940309     C************************************************************
074600940309     C* GESTIONE F12 VIDEO S1
074700940309     C************************************************************
074800940309     C     F12S1         BEGSR
074900940309     C*
075000970708     C                   MOVE      '2'           XtaRet
075100940309     C                   MOVE      *ON           $FINE
075200940309     C*
075300940309     C                   ENDSR
075400940309     C/EJECT
075500940309     C************************************************************
075600940309     C* GESTIONE F23 VIDEO S1
075700940309     C************************************************************
075800940309     C     F23S1         BEGSR
075900940309     C*
076000940309     C                   EXSR      RIEOPZ
076100950502     C                   Z-ADD     23            X
076200950502     C                   MOVE      FF23          $FC2(X)
076300940608     C                   MOVEL     WOPZ1         C1OPZ1
076400940608     C**                   MOVELWOPZ2     C1OPZ2
076500940309     C                   Z-ADD     WCOZ          C1COZ
076600940309     C*
076700940309     C                   ENDSR
076800940309     C/EJECT
076900940309     C************************************************************
077000940309     C* GESTIONE F24 VIDEO S1
077100940309     C************************************************************
077200940309     C     F24S1         BEGSR
077300940309     C*
077400940506     C*
077500940506     C                   MOVEA     $FX2          $FX
077600940506     C                   MOVEA     $FC2          $FC
077700940506     C                   MOVEA     $FR2          $FR
077800940506     C                   Z-ADD     $ULKS1        $ULK
077900940506     C                   EXSR      RIEKEY
078000941017     C                   MOVEA     $FC           $FC2
078100940506     C                   Z-ADD     $ULK          $ULKS1            3 0
078200940607     C                   MOVEL     $KEY1         Z1KE1
078300941004     C                   MOVEL     $KEY2         Z1KE2
078400940607     C                   WRITE     Z1
078500940506     C*
078600940309     C                   ENDSR
078700940128     C/EJECT
078800940128     C************************************************************
078900940131     C* CONTROLLO TESTATA LISTA
079000940128     C************************************************************
079100940131     C     CTRC1         BEGSR
079200940128     C*
079300940201     C                   MOVE      *OFF          *IN99
079400940131     C*
079500940202     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
079600940202     C* L'INIZIALIZZAZIONE LISTA
079700940202     C*
079800940207     C     C1POS         IFNE      W1POS
079900940202     C                   EXSR      POSS1
080000940202     C                   END
080100940202     C*
080200940202     C                   ENDSR
080300940202     C/EJECT
080400940202     C************************************************************
080500940202     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
080600940202     C************************************************************
080700940202     C     POSS1         BEGSR
080800940202     C*
080900940202     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
081000940202     C*
0811009402021    C     WMAX          IFGT      0
081200940607     C                   READC     S1                                     21
0813009402022    C     *IN21         DOWEQ     *OFF
081400940202     C     *IN99         ANDEQ     *OFF
081500940202     C*
0816009402023    C     S1OPZ         IFNE      0
081700940202     C                   SETON                                        51  99
0818009402023-   C                   ENDIF
081900940202     C* se rilevata opzione attiva, attivo sflnxtchg
0820009402023    C     *IN99         IFEQ      *ON
082100940202     C                   MOVE      *ON           *IN32
082200940714     C                   MOVE      $IN41         *IN41
082300940607     C                   UPDATE    S1
0824009402023E   C                   ELSE
082500940607     C                   READC     S1                                     21
0826009402023-   C                   ENDIF
082700940202     C*
0828009402022-   C                   ENDDO
082900940202     C*
0830009402021-   C                   ENDIF
083100940202     C*
083200940221     C*
0833009402021    C     *IN99         IFEQ      *OFF
083400941121     C                   CLEAR                   W1POS
083500940202     C*
083600940302     C                   MOVEL     C1POS         W1POS
083700940131     C*
083800940202     C                   MOVE      *ON           *IN99
083900940223     C                   MOVE      *ON           $INZS1
084000940202     C*
084100940202     C*
0842009402021-   C                   ENDIF
084300940202     C*
084400940131     C                   ENDSR
084500940131     C/EJECT
084600940131     C************************************************************
084700940131     C* CONTROLLO OPZIONI LISTA
084800940131     C************************************************************
084900940131     C     CTRS1         BEGSR
085000940131     C*
085100940202     C                   MOVEL     *OFF          $ESCI
085200940201     C                   SETOFF                                       99
085300940131     C                   Z-ADD     0             S1OPZ
085400940131     C*
085500940127     C* Leggo il sfl solo se ci sono rcd
0856009401311    C     WMAX          IFGT      0
085700940607     C                   READC     S1                                     21
085800940127     C*
085900940131     C* esce se fine sfl o errore che richiede l'uscita
0860009401312    C     *IN21         DOWEQ     *OFF
086100940131     C     $ESCI         ANDEQ     *OFF
086200940201     C                   Z-ADD     S1NRR         C1RCD
086300940131     C* ctrl su riga
086400940131     C                   EXSR      RECS1
086500940131     C* gestione opzioni
0866009401313    C     S1OPZ         IFNE      0
086700940201     C     *IN99         ANDEQ     *OFF
086800940131     C                   EXSR      OPZS1
0869009401273-   C                   ENDIF
087000940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0871009402013    C     *IN99         IFEQ      *ON
087200940201     C     $ESCI         OREQ      *ON
087300940131     C                   MOVE      *ON           *IN32
087400940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
087500940204     C* la ripristinerò a conclusione del ciclo di READC
087600940223     C                   MOVE      *OFF          $INZS1
0877009402233-   C                   ENDIF
087800940223     C*
087900940131     C                   Z-ADD     0             S1OPZ
088000940223     C*
088100940714     C                   MOVE      $IN41         *IN41
088200940607     C                   UPDATE    S1
088300940223     C*
088400940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0885009401313    C     $ESCI         IFEQ      *OFF
088600940607     C                   READC     S1                                     21
088700940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0888009402014    C     *IN21         IFEQ      *ON
088900940201     C                   MOVE      WINZS1        $INZS1
089000940204     C* calcolo pagina a cui deve posizionarsi
089100940224     C                   EXSR      CLCPAG
0892009402014-   C                   ENDIF
0893009402013-   C                   ENDIF
089400940131     C*
0895009401272-   C                   ENDDO
089600940127     C*
0897009401311-   C                   ENDIF
089800940131     C*
089900940127     C                   ENDSR
090000940127     C/EJECT
090100940127     C************************************************************
090200940131     C* CONTROLLO CAMPI I/O RIGA LISTA
090300940127     C************************************************************
090400940131     C     RECS1         BEGSR
090500940131     C*
090600940201     C* reset indicatori DSPATR
090700940201     C                   MOVE      *ALL'0'       IN4049           10
090800940201     C                   MOVEA     IN4049        *IN(40)
090900940201     C*
091000940131     C* Controllo se Utente Autorizzato alla scelta
091100940202     C     S1OPZ         IFNE      0
091200940131     C                   Z-ADD     1             X
091300940223     C     S1OPZ         LOOKUP    $OP1(X)                                21
091400940211     C     *IN21         IFEQ      *OFF
091500940223     C     $FL1(X)       OREQ      'N'
091600940131     C                   MOVE      *ON           $ESCI
091700981210     C***                SETON                                        504099
091800940131     C                   ENDIF
091900940131     C*
092000940620     c* Controllo autorizzazione alla modifica su record di gruppo
092100940620     C* Reperisco tipo codice (gruppo/azienda)
092200940620     C                   MOVE      *ALL'0'       XTBKEY
092300940620     C                   MOVE      S1COD         XTBKEY
092400970512     C                   MOVE      S1UNITA       XTBUNT
092500940620     C                   MOVE      '0'           XTBALC
092600940620     C                   MOVE      '1'           XTBRIC
092700970214     C                   CALLB     'XATB'
092800940620     C                   PARM                    XATBDS
092900940620     C* Reperisco autorizzazioni utente
093000940620     C                   CLEAR                   XCHATB
093100941118     C                   MOVE      XSCSOC        XCTSOC
093200941118     C                   MOVE      XSCDUN        XCTUNI
093300940620     C                   MOVEL     KNMUS         XCTPRF
093400940620     C                   MOVE      XTBCOD        XCTCOD
093500940620     C                   CALL      'XCHKATB'
093600940620     C                   PARM                    XCHATB
093700941111     C                   MOVEL     XCTDDS        AUTDS
093800941027     C     XCTRTN        IFEQ      1
093900941027     C                   MOVEL     'N'           MANGRU
094000941027     C                   ENDIF
094100940620     C* Non ammessa manutenzione di gruppo
094200940620   1 C     XTBGRU        IFEQ      'G'
094300940620     C     MANGRU        ANDEQ     'N'
094400940620   2 C     S1OPZ         IFEQ      2
094500940729     C     S1OPZ         OREQ      3
094600940620     C     S1OPZ         OREQ      4
094700981210     C***                SETON                                        5399
094800940620   2 C                   ENDIF
094900940620   1 C                   ENDIF
095000940620     C* Non ammessa manutenzione di azienda
095100940620   1 C     XTBGRU        IFEQ      'A'
095200940620     C     MANAZI        ANDEQ     'N'
095300940620   2 C     S1OPZ         IFEQ      2
095400940729     C     S1OPZ         OREQ      3
095500940620     C     S1OPZ         OREQ      4
095600981210     C***                SETON                                        5499
095700940620   2 C                   ENDIF
095800940620   1 C                   ENDIF
095900940607     C                   ENDIF
096000940131     C                   ENDSR
096100940224     C/EJECT
096200940131     C************************************************************
096300940131     C* GESTIONE OPZIONI LISTA
096400940131     C************************************************************
096500940131     C     OPZS1         BEGSR
096600940201     C*
0967009402251    C                   SELECT
096800940225     C*
096900940225     C* se opz=Selezione passo la chiave al pgm chiamante
097000940225     C     S1OPZ         WHENEQ    1
097100970708     C                   MOVE      S1OPZ         XtaOpz
097200970708     C                   MOVE      S1COD         XtaKey
097300970708     C                   MOVE      S1UNIta       xtaUni
097400940225     C                   MOVE      *ON           $ESCI
097500940225     C                   MOVE      *ON           $FINE
097600940729     C*
097700940714     C*
097800940714     C* se opz=Abilita scrive su archivio abilitazioni
097900940714     C     S1OPZ         WHENEQ    11
098000941103     C                   MOVE      XSCSOC        ABLAZI
098100940714     C                   MOVE      *ZEROS        ABLKEY
098200940714     C                   MOVE      S1COD         ABLKEY
098300970512     C                   MOVE      S1UNITA       ABLUNT
098400940714     C                   MOVE      XTBCOD        ABLCOD
098500940714     C                   WRITE     ANABL000                             22
098600940714     C*
098700940714     C* se opz=Disabil.cancella su archivio abilitazioni
098800940714     C     S1OPZ         WHENEQ    12
098900941103     C                   MOVE      XSCSOC        ABLAZI
099000940714     C                   MOVE      *ZEROS        ABLKEY
099100940714     C                   MOVE      S1COD         ABLKEY
099200970512     C                   MOVE      S1UNITA       ABLUNT
099300940714     C                   MOVE      XTBCOD        ABLCOD
099400940714     C     KABL          DELETE    ANABL000                           22
099500940729     C*
099600940225     C*
099700940225     C* se altre opzioni (o nessuna opzione) richiamo pgm gestione
0998009402251O   C                   OTHER
099900940225     C*
100000940616     C                   RESET                   D002DS
100100940607     C                   MOVEL     S1OPZ         OPZ002
100200940616     C                   MOVE      *ZERO         KEY002
100300940607     C                   MOVE      S1COD         KEY002
100400970708     C                   MOVE      XtaLin        LIN002
100500970708     C                   MOVE      XtaAzi        AZI002
100600940715     C                   MOVE      *BLANKS       KPJBU
100700940607     C                   MOVEL     D002DS        KPJBU
100800940608$004 C                   CALL      'ANGY4QR2'
100900940607     C                   PARM                    KPJBA
101000940607     C                   MOVEL     KPJBU         D002DS
101100940201     C*
101200940223     C* ritorno da PGM gestione
101300940223     C                   EXSR      GESRET
101400940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
1015009402252    C     *IN99         IFEQ      *ON
101600981210     C***                SETON                                        40
1017009402252-   C                   ENDIF
101800940131     C*
101900940506     C                   MOVE      *HIVAL        $LASTV
1020009402251-   C                   ENDSL
102100940225     C*
102200940131     C                   ENDSR
102300940223     C/EJECT
102400940223     C************************************************************
102500940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
102600940223     C************************************************************
102700940223     C     GESRET        BEGSR
102800940223     C*
102900940223     C* modo di ritorno
103000940223     C*
1031009402231    C                   SELECT
103200940314    >C* << questi modi di utilizzo dei valori di ritorno dal
103300940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
103400940314    >C*    proposte, normalmente sempre valide, ma modificabili
103500940314    >C*    per situazioni particolari >>
103600940223     C* 1 = F3
103700940314    >C     RET002        WHENEQ    '1'
103800940224     C                   MOVE      *ON           $ESCI
103900940223     C                   MOVE      *ON           $FINE
104000940223     C* 2 = F12
104100940314    >C     RET002        WHENEQ    '2'
104200940223     C                   MOVE      *ON           $ESCI
104300940223     C*
1044009402231-   C                   ENDSL
104500940223     C*
104600940223     C* operazione eseguite dal pgm chiamato
104700940223     C*
1048009402231    C                   SELECT
104900940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
105000940314    >C     OPR002        WHENEQ    '1'
105100940223     C                   MOVE      *ON           WINZS1
105200940223     C*
1053009402231-   C                   ENDSL
105400940223     C*
105500940223     C* funzione non eseguibile per errore :
105600940223     C*
1057009402231    C                   SELECT
105800940223     C* 1 = funzione richiamata chiusa in errore
105900940316    >C*  eventualmente gestire altri codici di errore
106000940314    >C     ERR002        WHENEQ    '1'
106100940223     C                   MOVE      *ON           $ESCI
106200981210     C***                SETON                                        5299
106300940223     C*
1064009402231-   C                   ENDSL
106500940223     C*
106600940223     C                   ENDSR
106700940223     C/EJECT
106800940131     C************************************************************
106900940131     C* OPERAZIONI INIZIALI
107000940131     C************************************************************
107100940131     C     *INZSR        BEGSR
107200940131     C*
107300940127     C* Reperimento parametri
107400940127     C*
107500940117     C     *ENTRY        PLIST
107600940117     C                   PARM                    KPJBA
107700940714     C*
107800940714     C* Reperimento Codice utente
107900940714     C*
108000950622     C*          KNMUS     CHAINANUTN01L             21
108100940225     C*
108200940223     C* Reperimento opzioni
108300940127     C*
108400940128     C                   EXSR      REPOPZ
108500940506     C*
108600940506     C* Reperimento tasti di funzione
108700940506     C*
108800940506     C                   EXSR      REPKEY
108900940506     C*
109000940607$007 C                   MOVE      'Y4Q'         XTBCOD
109100940127     C*
109200940117     C                   ENDSR
109300940225     C/EJECT
109400941103      ************************************************************
109500941103      * Reperimento dati società
109600941103      ************************************************************
109700941103     C     REPSOC        BEGSR
109800941103      *
109900970214     C                   CALLB     'XSOC'
110000941103     C                   PARM                    TIPXSC            6
110100941103     C                   PARM                    SOCXSC            3
110200941103     C                   PARM                    CDSXSC            9 0
110300941103     C                   PARM                    MODXSC            3
110400941103     C                   PARM      *BLANKS       RTNXSC            1
110500941103     C                   PARM                    XSOCDS
110600941103     C                   PARM                    KPJBA
110700941103      *
110800941103     C                   ENDSR
110900940223     C************************************************************
111000940223     C* INIZIALIZZAZIONE VARIABILI
111100940223     C************************************************************
111200940223     C     INZVAR        BEGSR
111300970926     C*
111400970926     C* Dati da PGM chiamante
111500970926     C*
111600970926     C                   MOVEL     KPJBU         XTABDS
111700951124     C*
111800951124     C* Reperimento codice lingua e codice azienda
111900951124     C*
112000951124     C                   MOVEL     'SOC001'      TIPXSC
112100951124     C                   EXSR      REPSOC
112200951124     C     RTNXSC        IFNE      '1'
112300951124     C                   MOVEL     XSOCDS        SOC001
112400951124     C                   ELSE
112500951124     C                   MOVE      *ON           $FINE
112600951124     C* fine programma
112700951124     C                   EXSR      ENDPGM
112800951124     C                   ENDIF
112900951124     C* Lingua e codice azienda
113000970708     C                   MOVE      XSCLIN        XtaLin
113100970708     C                   MOVE      XSCSOC        XtaAzi
113200940715     C*
113300940715     C                   RESET                   $FC2
113400940715     C*
113500940715$021 C                   CLOSE     ANGY4QD
113600940715$021 C                   OPEN      ANGY4QD
113700940506     C*
113800940506     C* Definizione delle opzioni utilizzabili a pgm
113900940506     C*
114000940506     C                   EXSR      DEFOPZ
114100940718     C* Pulizia campi e indicatori
114200940718     C                   CLEAR                   W1POS
114300940718     C                   MOVE      *ALL'0'       IN4049           10
114400940718     C                   MOVEA     IN4049        *IN(40)
114500940718     C                   CLEAR                   S1OPZ
114600940223     C* Variabili per gestione videate
114700940223     C*
114800940223     C                   MOVE      *OFF          $FINE
114900940223     C                   MOVE      *OFF          $INZS1
115000940223     C                   MOVE      *OFF          $EFILE
115100940223     C                   MOVE      *OFF          $ESCI
115200940223     C                   MOVE      *OFF          $RCDOK
115300940509     C                   MOVE      *HIVAL        $LASTV
115400940506     C                   Z-ADD     0             $ULKS1            3 0
115500940223     C*
115600940318     C                   MOVE      *ON           $INZS1
115700940223     C                   MOVE      'S1'          $GEST
115800940223     C*
115900940223     C* Variabili appoggio
116000940223     C*
116100940223     C                   Z-ADD     SFLPAG        WPAG
116200940223     C*
116300940223     C* Valorizzazione campi univoci testate
116400940223     C*
116500940607     C*                    CLEART1
116600940607     C*                    MOVELKNSIF     NOMSIF
116700940607     C*                    MOVELRAGUTE    NOMDIT
116800940607     C*                    MOVELDSPGM     SIGLA
116900940607     C*                    MOVELPROGR     NOMPGM
117000940223     C*
117100940608     c* posizione window
117200940608$012 C                   Z-ADD     67            PCOL              2 0
117300940613     C                   Z-ADD     09            R
117400940608     C     80            SUB       PCOL          C
117500940608     C                   SUB       3             C
117600970708     C     XtaOpz        IFNE      '  '
117700970708     C     XtaRig        IFGT      11
117800940608     C                   Z-ADD     1             R
117900940608     C                   ENDIF
118000940608     C*
118100970708     C     XtaCol        IFGT      40
118200940608     C                   Z-ADD     2             C
118300940608     C                   ENDIF
118400940608     C                   ENDIF
118500940616     C*
118600970708     C     XtaOpz        IFNE      '01'
118700940616$019 C                   DO        00            I                 3 0
118800940721$022 C*                  MOVEL     LIN(I)        TBTFLD
118900940721$022 C*                  CLEAR                   TBTKEY
119000940721$022 C*                  CLEAR                   TBTAZI
119100940721$022 C*                  CLEAR                   TBTLIN
119200940721$007 C                   MOVE      'Y4Q'         TBTCOD            3
119300940721$022 C*    K5TBT1        CHAIN     ANTBT01L                           21
119400940721$022 C*    *IN21         IFEQ      *ON
119500940721$022 C*                  Z-ADD     LLUN(I)       TBTLUN
119600940721$022 C*                  Z-ADD     LPOS(I)       TBTPOS
119700940721$022 C*                  MOVEL     LIND(I)       TBTDES
119800940721$022 C*                  WRITE     ANTBT000
119900940721$022 C*                  ENDIF
120000940616     C                   ENDDO
120100940616     C                   ENDIF
120200940608     C*
120300940722     c*
120400940722     c* Se richiamato con OPZ 11  Non controlla abilitazione all'aziend
120500970708     C     XtaOpz        IFEQ      '11'
120600940722     C                   MOVE      'N'           XTBGAB
120700940722     C                   ELSE
120800940722     C                   MOVE      'S'           XTBGAB
120900940722     C                   ENDIF
121000940223     C                   ENDSR
121100940223     C/EJECT
121200940131     C************************************************************
121300940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
121400940131     C************************************************************
121500940225     C     DEFOPZ        BEGSR
121600940131     C*
121700940225     C* Questa routine deve valorizzare la schiera $FL1
121800940225     C* con S o N a seconda della utilizzabilità o meno
121900940225     C* dell'opzione correlata
122000940225     C*
122100940714     C* Inizialmente si considerano tutte le opzioni come non valide
122200940728     C     1             DO        14            X
122300940714     C                   MOVE      'N'           $FL1(X)
122400940225     C                   END
122500940225     C*
122600940225     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
122700940714     C* rendo valide alcune opzioni
122800940714     C* i tasti invece li disattivo
122900940225     C                   SELECT
123000940714     C*
123100940714     C* Se pgm richiamato in RICERCA attivo :
123200940714     C*
123300970708    >C     XtaOpz        WHENEQ    '01'
123400940714     C* F3=Fine
123500940714     C                   Z-ADD     3             X
123600940720     C                   MOVE      '0'           $FC2(X)
123700940714     C* F10=Immissione
123800940714     C                   Z-ADD     10            X
123900940919     C                   MOVE      '1'           $FC2(X)
124000940714     C* 1=Selezione
124100940714     C                   Z-ADD     1             X
124200940714     C     01            LOOKUP    $OP1(X)                                21
124300940714     C     *IN21         IFEQ      *ON
124400940714     C                   MOVE      'S'           $FL1(X)
124500940714     C                   ENDIF
124600940729     C* 2=Modifica
124700940729     C                   Z-ADD     1             X
124800940729     C     02            LOOKUP    $OP1(X)                                21
124900940729     C     *IN21         IFEQ      *ON
125000940729     C                   MOVE      'S'           $FL1(X)
125100940729     C                   ENDIF
125200940729     C* 3=Copia
125300940729     C                   Z-ADD     1             X
125400940729     C     03            LOOKUP    $OP1(X)                                21
125500940729     C     *IN21         IFEQ      *ON
125600940729     C                   MOVE      'S'           $FL1(X)
125700940729     C                   ENDIF
125800940729     C* 4=Annullamento
125900940729     C                   Z-ADD     1             X
126000940729     C     04            LOOKUP    $OP1(X)                                21
126100940729     C     *IN21         IFEQ      *ON
126200940729     C                   MOVE      'S'           $FL1(X)
126300940729     C                   ENDIF
126400940714     C* 5=Visualizzazione
126500940714     C                   Z-ADD     1             X
126600940714     C     05            LOOKUP    $OP1(X)                                21
126700940714     C     *IN21         IFEQ      *ON
126800940714     C                   MOVE      'S'           $FL1(X)
126900940714     C                   ENDIF
127000940728     C*14=Commenti
127100940831     C**                   Z-ADD1         X
127200940831     C**         14        LOKUP$OP1,X                   21
127300940831     C**         *IN21     IFEQ *ON
127400940831     C**                   MOVE 'S'       $FL1,X
127500940831     C**                   ENDIF
127600940714     C*
127700940714     C* Se pgm richiamato in INTERROGAZIONE attivo :
127800940714     C*
127900970708    >C     XtaOpz        WHENEQ    '05'
128000940714     C* F10=Immissione
128100940714     C                   Z-ADD     10            X
128200940720     C                   MOVE      '0'           $FC2(X)
128300940714     C* F3=Fine
128400940714     C                   Z-ADD     3             X
128500940720     C                   MOVE      '0'           $FC2(X)
128600940714     C* 5=Visualizzazione
128700940225     C                   Z-ADD     1             X
128800940714     C     05            LOOKUP    $OP1(X)                                21
128900940225     C     *IN21         IFEQ      *ON
129000940714     C                   MOVE      'S'           $FL1(X)
129100940225     C                   ENDIF
129200940728     C*14=Commenti
129300940831     C**                   Z-ADD1         X
129400940831     C**         14        LOKUP$OP1,X                   21
129500940831     C**         *IN21     IFEQ *ON
129600940831     C**                   MOVE 'S'       $FL1,X
129700940831     C**                   ENDIF
129800940831     C*
129900940714     C* Se pgm richiamato in ABILITAZIONE attivo :
130000940714     C*
130100970708    >C     XtaOpz        WHENEQ    '11'
130200940714     C* F10=Immissione
130300940714     C                   Z-ADD     10            X
130400940720     C                   MOVE      '0'           $FC2(X)
130500940714     C* F3=Fine
130600940714     C                   Z-ADD     3             X
130700940720     C                   MOVE      '0'           $FC2(X)
130800940714     C* 5=Visualizzazione
130900940714     C                   Z-ADD     1             X
131000940714     C     05            LOOKUP    $OP1(X)                                21
131100940714     C     *IN21         IFEQ      *ON
131200940714     C                   MOVE      'S'           $FL1(X)
131300940714     C                   ENDIF
131400940714     C* 11=Abilitazione
131500940714     C                   Z-ADD     1             X
131600940714     C     11            LOOKUP    $OP1(X)                                21
131700940714     C     *IN21         IFEQ      *ON
131800940714     C                   MOVE      'S'           $FL1(X)
131900940714     C                   ENDIF
132000940714     C* 12=Disabilita
132100940714     C                   Z-ADD     1             X
132200940714     C     12            LOOKUP    $OP1(X)                                21
132300940714     C     *IN21         IFEQ      *ON
132400940714     C                   MOVE      'S'           $FL1(X)
132500940714     C                   ENDIF
132600940714     C*
132700940714     C* Se pgm richiamato in GESTIONE  attivo :
132800940714     C*
132900970708    >C     XtaOpz        WHENEQ    '02'
133000940714     C* F3=Fine
133100940714     C                   Z-ADD     3             X
133200940720     C                   MOVE      '0'           $FC2(X)
133300940714     C* 2=Modifica
133400940714     C                   Z-ADD     1             X
133500940714     C     02            LOOKUP    $OP1(X)                                21
133600940714     C     *IN21         IFEQ      *ON
133700940714     C                   MOVE      'S'           $FL1(X)
133800940714     C                   ENDIF
133900940714     C* 3=Copia
134000940714     C                   Z-ADD     1             X
134100940714     C     03            LOOKUP    $OP1(X)                                21
134200940714     C     *IN21         IFEQ      *ON
134300940714     C                   MOVE      'S'           $FL1(X)
134400940714     C                   ENDIF
134500940714     C* 4=Annullamento
134600940714     C                   Z-ADD     1             X
134700940714     C     04            LOOKUP    $OP1(X)                                21
134800940714     C     *IN21         IFEQ      *ON
134900940714     C                   MOVE      'S'           $FL1(X)
135000940714     C                   ENDIF
135100940714     C* 5=Visualizzazione
135200940714     C                   Z-ADD     1             X
135300940714     C     05            LOOKUP    $OP1(X)                                21
135400940714     C     *IN21         IFEQ      *ON
135500940714     C                   MOVE      'S'           $FL1(X)
135600940714     C                   ENDIF
135700940729     C*14=Commenti
135800940831     C**                   Z-ADD1         X
135900940831     C**         14        LOKUP$OP1,X                   21
136000940831     C**         *IN21     IFEQ *ON
136100940831     C**                   MOVE 'S'       $FL1,X
136200940831     C**                   ENDIF
136300940225     C*
136400940225     C                   OTHER
136500940714     C* Se pgm non richiamato, attivo TUTTO  :
136600940714     C* 2=Modifica
136700940714     C                   Z-ADD     1             X
136800940714     C     02            LOOKUP    $OP1(X)                                21
136900940714     C     *IN21         IFEQ      *ON
137000940714     C                   MOVE      'S'           $FL1(X)
137100940714     C                   ENDIF
137200940714     C* 3=Copia
137300940714     C                   Z-ADD     1             X
137400940714     C     03            LOOKUP    $OP1(X)                                21
137500940714     C     *IN21         IFEQ      *ON
137600940714     C                   MOVE      'S'           $FL1(X)
137700940714     C                   ENDIF
137800940714     C* 4=Annullamento
137900940714     C                   Z-ADD     1             X
138000940714     C     04            LOOKUP    $OP1(X)                                21
138100940714     C     *IN21         IFEQ      *ON
138200940714     C                   MOVE      'S'           $FL1(X)
138300940714     C                   ENDIF
138400940714     C* 5=Visualizzazione
138500940714     C                   Z-ADD     1             X
138600940714     C     05            LOOKUP    $OP1(X)                                21
138700940714     C     *IN21         IFEQ      *ON
138800940714     C                   MOVE      'S'           $FL1(X)
138900940714     C                   ENDIF
139000940714     C* 11=Abilitazione
139100940714     C                   Z-ADD     1             X
139200940714     C     11            LOOKUP    $OP1(X)                                21
139300940714     C     *IN21         IFEQ      *ON
139400940714     C                   MOVE      'S'           $FL1(X)
139500940714     C                   ENDIF
139600940714     C* 12=Disabilita
139700940714     C                   Z-ADD     1             X
139800940714     C     12            LOOKUP    $OP1(X)                                21
139900940714     C     *IN21         IFEQ      *ON
140000940714     C                   MOVE      'S'           $FL1(X)
140100940714     C                   ENDIF
140200940728     C*14=Commenti
140300940831     C**                   Z-ADD1         X
140400940831     C**         14        LOKUP$OP1,X                   21
140500940831     C**         *IN21     IFEQ *ON
140600940831     C**                   MOVE 'S'       $FL1,X
140700940831     C**                   ENDIF
140800940225     C* poi, disattivo a seconda delle autorizzazioni :
140900940314    >C*********  AUTXXX    IFEQ 'N'
141000940314    >C*********            Z-ADD1         X
141100940314    >C*********  'XX'      LOKUP$OP1,X                   21
141200940314    >C*********  *IN21     IFEQ *ON
141300940314    >C*********            MOVE 'N'       $FL1,X
141400940314    >C*********            ENDIF
141500940314    >C*********            ENDIF
141600940225     C*
141700940225     C                   ENDSL
141800940225     C*
141900940225     C                   ENDSR
142000940225     C/EJECT
142100940225     C************************************************************
142200940225     C* REPERIMENTO OPZIONI
142300940225     C************************************************************
142400940225     C     REPOPZ        BEGSR
142500940225     C*
142600940314     C     1             DO        $DIMOP        X
142700940128     C*
142800940223     C     $MS1(X)       IFNE      *BLANK
142900940128     C*
143000940223     C                   MOVEL     $MS1(X)       IDMSG
143100970214     C                   CALLB     'XRTVM'
143200940201     C                   PARM                    IDMSG            27
143300940201     C                   PARM                    TXTMSG          100
143400940201     C                   PARM                    ERRMSG            1
143500940128     C*
143600940128     C     ERRMSG        IFNE      *ON
143700940223     C                   MOVEL     TXTMSG        $TX1(X)
143800940128     C                   ELSE
143900940223     C                   MOVEL     *ALL'?'       $TX1(X)
144000940128     C                   END
144100940128     C*
144200940128     C                   END
144300940128     C*
144400940128     C                   END
144500940128     C*
144600940128     C                   ENDSR
144700940506     C/EJECT
144800940506     C************************************************************
144900940506     C* REPERIMENTO TASTI DI FUNZIONE
145000940506     C************************************************************
145100940506     C     REPKEY        BEGSR
145200940506     C*
145300940506     C                   MOVEA     $FM2          $FM
145400940506     C                   EXSR      REPKKK
145500940506     C                   MOVEA     $FX           $FX2
145600940506     C                   ENDSR
145700940506     C/EJECT
145800940506     C************************************************************
145900940506     C* REPERIMENTO TASTI DI FUNZIONE
146000940506     C************************************************************
146100940506     C     REPKKK        BEGSR
146200940506     C*
146300940506     C                   CLEAR                   $FX
146400940506     C     1             DO        24            X
146500940506     C*
146600940506     C     $FM(X)        IFNE      *BLANK
146700940506     C*
146800940506     C                   MOVEL     $FM(X)        IDMSG
146900970214     C                   CALLB     'XRTVM'
147000940506     C                   PARM                    IDMSG            27
147100940506     C                   PARM                    TXTMSG          100
147200940506     C                   PARM                    ERRMSG            1
147300940506     C*
147400940506     C     ERRMSG        IFNE      *ON
147500940506     C                   MOVEL     TXTMSG        $FX(X)
147600940506     C                   ELSE
147700940506     C                   MOVEL     *ALL' '       $FX(X)
147800940506     C                   END
147900940506     C*
148000940506     C                   END
148100940506     C*
148200940506     C                   END
148300940506     C*
148400940506     C                   ENDSR
148500940223     C/EJECT
148600940128     C************************************************************
148700940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
148800940128     C************************************************************
148900940223     C     DEFCAM        BEGSR
149000940128     C*
149100940223     C* Variabili per gestione videate
149200940223     C*
149300940223     C                   MOVE      *BLANK        $GEST             2
149400940223     C                   MOVE      *BLANK        $FINE             1
149500940223     C                   MOVE      *BLANK        $INZS1            1
149600940223     C                   MOVE      *BLANK        $LASTG            2
149700940223     C                   MOVE      *BLANK        $LASTV            2
149800940223     C                   MOVE      *BLANK        $EFILE            1
149900940223     C                   MOVE      *BLANK        $ESCI             1
150000940223     C                   MOVE      *BLANK        $RCDOK            1
150100940223     C*
150200940314     C* Variabili appoggio sempre presenti in tutte le anagrafiche
150300940223     C*
150400940223     C     *LIKE         DEFINE    C1RCD         S1NRR
150500940223     C     *LIKE         DEFINE    C1NRR         WSFL
150600940223     C     *LIKE         DEFINE    C1RCD         WMAX
150700940223     C     *LIKE         DEFINE    C1RCD         WPAG
150800940223     C     *LIKE         DEFINE    $INZS1        WINZS1
150900940223     C                   MOVE      *BLANK        WWGRP             3
151000940223     C                   Z-ADD     0             CURR              2 0
151100940223     C                   Z-ADD     0             CURC              2 0
151200940223     C                   MOVE      *BLANK        WIN              99
151300940314     C     *LIKE         DEFINE    C1POS         W1POS
151400940620     C*
151500940721$022 C*    K5TBT1        KLIST
151600940721$022 C*                  KFLD                    TBTAZI
151700940721$022 C*                  KFLD                    TBTCOD
151800940721$022 C*                  KFLD                    TBTKEY
151900940721$022 C*                  KFLD                    TBTLIN
152000940721$022 C*                  KFLD                    TBTFLD
152100940714     C*
152200940714     C     KABL          KLIST
152300940714     C                   KFLD                    ABLCOD
152400940714     C                   KFLD                    ABLKEY
152500940714     C                   KFLD                    ABLAZI
152600940314     C*
152700940314     C*
152800940223     C* Indici
152900940223     C*
153000940223     C                   Z-ADD     0             X                 3 0
153100940223     C                   Z-ADD     0             Y                 3 0
153200940127     C*
153300940117     C                   ENDSR
153400940223     C/EJECT
153500940616**
153600940225PROMSG    *LIBL     OPZ0001
153700940225PROMSG    *LIBL     OPZ0002
153800940131PROMSG    *LIBL     OPZ0003
153900940131PROMSG    *LIBL     OPZ0004
154000940131PROMSG    *LIBL     OPZ0005
154100940224PROMSG    *LIBL     OPZ0006
154200940224PROMSG    *LIBL     OPZ0007
154300940224PROMSG    *LIBL     OPZ0008
154400940224PROMSG    *LIBL     OPZ0009
154500970716PROMSG    *LIBL     OPZ0010
154600970716PROMSG    *LIBL     OPZ0013
154700970716PROMSG    *LIBL     OPZ0012
154800970716PROMSG    *LIBL     OPZ0011
154900970716PROMSG    *LIBL     OPZ0014
155000940201**
15510094022501S      <Selezione
15520094022502S      <Modifica
15530094022503S      <Copia
15540094022504S      <Annullamento
15550094022505S      <Visualizzazione
15560094060706N      <prova
15570094060707N      <prova
15580094060708N      <prova
15590094060709N      <prova
15600094072910S      <Invia
15610094071411S      <Abilita gruppo
15620094071412S      <Disabilita
15630094072813N      <PROVA
15640094083114N      <Commenti
156500940506** TASTI DI FUNZIONE  S1
156600940506PROMSG    *LIBL     KEY0001  01
156700940506PROMSG    *LIBL     KEY0002  02
156800940506PROMSG    *LIBL     KEY0003  03
156900940506PROMSG    *LIBL     KEY0004  04
157000940506PROMSG    *LIBL     KEY0005  05
157100940506PROMSG    *LIBL     KEY0006  06
157200940506PROMSG    *LIBL     KEY0007  07
157300940506PROMSG    *LIBL     KEY0008  08
157400940506PROMSG    *LIBL     KEY0009  09
157500970716PROMSG    *LIBL     KEY0010  10
157600970716PROMSG    *LIBL     KEY0011  11
157700970716PROMSG    *LIBL     KEY0012  12
157800970716PROMSG    *LIBL     KEY0013  13
157900970716PROMSG    *LIBL     KEY0014  14
158000970716PROMSG    *LIBL     KEY0015  15
158100970716PROMSG    *LIBL     KEY0016  16
158200970716PROMSG    *LIBL     KEY0017  17
158300970716PROMSG    *LIBL     KEY0018  18
158400970716PROMSG    *LIBL     KEY0019  19
158500940506PROMSG    *LIBL     KEY0020  20
158600940506PROMSG    *LIBL     KEY0021  21
158700940506PROMSG    *LIBL     KEY0022  22
158800940506PROMSG    *LIBL     KEY0023  23
158900940506PROMSG    *LIBL     KEY0024  24
159000940506**  ABILITAZIONE S1
1591009410140N  01
1592009407200N  02
1593009407201N  03
1594009407200N  04
1595009407200N  05
1596009407200N  06
1597009407200N  07
1598009407200N  08
1599009407200N  09
1600009407201N  10
1601009407200N  11
1602009407201N  12
1603009407200N  13
1604009407200N  14
1605009407200N  15
1606009407200N  16
1607009407200N  17
1608009407200N  18
1609009407200N  19
1610009407200N  20
1611009407200N  21
1612009407200N  22
1613009505020N  23
1614009407200N  24
