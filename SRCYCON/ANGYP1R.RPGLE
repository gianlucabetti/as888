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
003900940715$001 FANGYP1D   CF   E             WORKSTN USROPN
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
014200940607$003 D EXTDS         E DS                  EXTNAME(ANGYP1DS)
014300940207     D*-------------
014400940211     D* COSTANTI
014500940211     D*-------------
014600980717     D SFLPAG          C                   CONST(05)
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
018200980717$015 I              S1MAN                       §P1MAN
018300980717$015 I              S1ACC                       §P1ACC
018400980717$015 I              S1RGH                       §P1RGH
018500980717$015 I              S1STB                       §P1STB
018600990205$015 I              S1DET                       §P1DET
018700990205$015 I              S1DAN                       §P1DAN
018800980717$015 I              S1GES                       §P1GES
018900940207     I*-------------
019000940127     I*
019100940127     C*----------------------------------------------------*
019200940127     C*                MAIN LINE PROGRAM
019300940127     C*----------------------------------------------------*
019400940127     C*
019500940223     C* inizializzazione variabili
019600940223     C                   EXSR      INZVAR
019700940223     C*
019800940223     C     $FINE         DOWEQ     *OFF
019900940131     C     $GEST         CASEQ     'S1'          GESS1
020000940117     C                   END
020100940117     C                   END
020200940325     C*
020300940325     C* fine programma
020400940325     C                   EXSR      ENDPGM
020500940225     C*
020600940325     C/EJECT
020700940325     C************************************************************
020800940325     C* FINE PROGRAMMA
020900940325     C************************************************************
021000940325     C     ENDPGM        BEGSR
021100940325     C*
021200940325     C* passaggio dati al pgm chiamante
021300940715     C                   MOVE      *BLANKS       KPJBU
021400940607     C                   MOVEL     XTABDS        KPJBU
021500940325     C*
021600940325     C* Se pgm Guida non è stato richiamato posso chiudere in LR
021700970708     C     XtaOpz        IFEQ      *BLANK
021800940325     C                   SETON                                            LR
021900960906     C                   RETURN
022000940325     C* altrimenti chiudo in RT
022100940325     C                   ELSE
022200940325     C                   SETON                                            RT
022300960906     C                   RETURN
022400940325     C                   ENDIF
022500940325     C*
022600940325     C                   ENDSR
022700940131     C/EJECT
022800940224     C************************************************************
022900940131     C* RIEMPIMENTO OPZIONI
023000940224     C************************************************************
023100940131     C     RIEOPZ        BEGSR
023200940223     C*
023300940223     C* Input :
023400940223     C* - $TX = schiera con tutti i testi disponibili
023500940223     C* - $FL = schiere con i flag di validità associati ai testi
023600940225     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
023700940223     C*          opzione usato
023800940223     C* - WMXO = variabile contenente il numero massimo di testi
023900940223     C*          opzione usabili
024000940223     C* Output :
024100940223     C* - WOPZ1 = variabile contenente la prima riga opzioni
024200940223     C* - WOPZ2 = variabile contenente la seconda riga opzioni
024300950502     C* - FF23  = flag che indica se c'è bisogno del tasto F23
024400940223     C*
024500970214     C                   CALLB     'X04OPMSG'
024600940223     C* input
024700940223     C                   PARM                    $TX
024800940223     C                   PARM                    $FL
024900940225     C                   PARM                    WCOZ              3 0
025000940223     C                   PARM                    WMXO              3 0
025100940223     C* output
025200940223     C                   PARM                    WOPZ1            76
025300940223     C                   PARM                    WOPZ2            76
025400950502     C                   PARM                    FF23              1
025500940607     C* input
025600940607$018 C                   PARM      39            $L01              3 0
025700940607     C                   PARM      0             $L02              3 0
025800940223     C*
025900940131     C                   ENDSR
026000940506     C/EJECT
026100940506     C************************************************************
026200940506     C* RIEMPIMENTO TASTI FUNZIONE
026300940506     C************************************************************
026400940506     C     RIEKEY        BEGSR
026500940506     C*
026600940506     C* Input :
026700940506     C* - $FX = schiera con tutti i testi disponibili
026800940506     C* - $FC = schiere con i flag di validità associati ai testi
026900940506     C* - $FR = schiere con i flag di REVERSE IMAGE
027000940506     C* - $ULK = ultimo testo caricato
027100940506     C* Output :
027200940506     C* - $ULK
027300940506     c* - $KEY1= 1° riga di tasti funzione
027400940506     c* - $KEY2= 2° riga di tasti funzione
027500940506     C*
027600970214     C                   CALLB     'XKEYMSG'
027700940506     C* input
027800940506     C                   PARM                    $FX
027900940506     C                   PARM                    $FC
028000940506     C                   PARM                    $FR
028100940506     C                   PARM                    $ULK              3 0
028200940506     C* output
028300940506     C                   PARM                    $KEY1            79
028400940506     C                   PARM                    $KEY2            79
028500940607     C* input
028600940607$018 C                   PARM      39            $L01              3 0
028700941004$018 C                   PARM      39            $L02              3 0
028800940506     C*
028900940506     C                   ENDSR
029000940224     C/EJECT
029100940224     C************************************************************
029200940224     C* GESTIONE EMISSIONE TASTI FUNZIONE
029300940224     C************************************************************
029400940224     C     GESPIE        BEGSR
029500940224     C*
029600940224     C                   SELECT
029700940506     C* Lista
029800940506     C     $GEST         WHENEQ    'S1'
029900940506     C     $ULKS1        IFEQ      0
030000940506     C                   EXSR      F24S1
030100940506     C                   ELSE
030200940607     C                   WRITE     Z1
030300940506     C                   ENDIF
030400940506     C                   ENDSL
030500940506     C*
030600940224     C                   ENDSR
030700940506     C/EJECT
030800940506     C************************************************************
030900940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA S1
031000940506     C************************************************************
031100940506     C     CTRKS1        BEGSR
031200940506     C                   SETOFF                                       99
031300940506     C                   MOVEA     $FC2          $FC
031400940506     C                   EXSR      CTRKEY
031500940506     C     *IN99         IFEQ      *ON
031600940506     C                   SETON                                        97
031700940506     C                   ENDIF
031800940607     C*
031900940506     C                   ENDSR
032000940506     C/EJECT
032100940506     C************************************************************
032200940506     C* CONTROLLO ABILITAZIONE TASTI
032300940509     C* INPUT
032400940509     c* $TASTO =Tasto funzione premuto
032500940509     C* $FC = Tasti abilitati
032600940509     C* OUTPUT
032700940509     C* IN99 = ERrore
032800940506     C************************************************************
032900940506     C     CTRKEY        BEGSR
033000940506     C                   SELECT
033100940506     C     $TASTO        WHENEQ    F01
033200940720     C     $FC(1)        IFEQ      '0'
033300940506     C                   SETON                                        99
033400940506     C                   ENDIF
033500940506     C     $TASTO        WHENEQ    F02
033600940720     C     $FC(2)        IFEQ      '0'
033700940506     C                   SETON                                        99
033800940506     C                   ENDIF
033900940506     C     $TASTO        WHENEQ    F03
034000940720     C     $FC(3)        IFEQ      '0'
034100940506     C                   SETON                                        99
034200940506     C                   ENDIF
034300940506     C     $TASTO        WHENEQ    F04
034400940720     C     $FC(4)        IFEQ      '0'
034500940506     C                   SETON                                        99
034600940506     C                   ENDIF
034700940506     C     $TASTO        WHENEQ    F05
034800940720     C     $FC(5)        IFEQ      '0'
034900940506     C                   SETON                                        99
035000940506     C                   ENDIF
035100940506     C     $TASTO        WHENEQ    F06
035200940720     C     $FC(6)        IFEQ      '0'
035300940506     C                   SETON                                        99
035400940506     C                   ENDIF
035500940506     C     $TASTO        WHENEQ    F07
035600940720     C     $FC(7)        IFEQ      '0'
035700940506     C                   SETON                                        99
035800940506     C                   ENDIF
035900940506     C     $TASTO        WHENEQ    F08
036000940720     C     $FC(8)        IFEQ      '0'
036100940506     C                   SETON                                        99
036200940506     C                   ENDIF
036300940506     C     $TASTO        WHENEQ    F09
036400940720     C     $FC(09)       IFEQ      '0'
036500940506     C                   SETON                                        99
036600940506     C                   ENDIF
036700940506     C     $TASTO        WHENEQ    F10
036800940720     C     $FC(10)       IFEQ      '0'
036900940506     C                   SETON                                        99
037000940506     C                   ENDIF
037100940506     C     $TASTO        WHENEQ    F11
037200940720     C     $FC(11)       IFEQ      '0'
037300940506     C                   SETON                                        99
037400940506     C                   ENDIF
037500940506     C     $TASTO        WHENEQ    F12
037600940720     C     $FC(12)       IFEQ      '0'
037700940506     C                   SETON                                        99
037800940506     C                   ENDIF
037900940506     C     $TASTO        WHENEQ    F13
038000940720     C     $FC(13)       IFEQ      '0'
038100940506     C                   SETON                                        99
038200940506     C                   ENDIF
038300940506     C     $TASTO        WHENEQ    F14
038400940720     C     $FC(14)       IFEQ      '0'
038500940506     C                   SETON                                        99
038600940506     C                   ENDIF
038700940506     C     $TASTO        WHENEQ    F15
038800940720     C     $FC(15)       IFEQ      '0'
038900940506     C                   SETON                                        99
039000940506     C                   ENDIF
039100940506     C     $TASTO        WHENEQ    F16
039200940720     C     $FC(16)       IFEQ      '0'
039300940506     C                   SETON                                        99
039400940506     C                   ENDIF
039500940506     C     $TASTO        WHENEQ    F17
039600940720     C     $FC(17)       IFEQ      '0'
039700940506     C                   SETON                                        99
039800940506     C                   ENDIF
039900940506     C     $TASTO        WHENEQ    F18
040000940720     C     $FC(18)       IFEQ      '0'
040100940506     C                   SETON                                        99
040200940506     C                   ENDIF
040300940506     C     $TASTO        WHENEQ    F19
040400940720     C     $FC(19)       IFEQ      '0'
040500940506     C                   SETON                                        99
040600940506     C                   ENDIF
040700940506     C     $TASTO        WHENEQ    F20
040800940720     C     $FC(20)       IFEQ      '0'
040900940506     C                   SETON                                        99
041000940506     C                   ENDIF
041100940506     C     $TASTO        WHENEQ    F21
041200940720     C     $FC(21)       IFEQ      '0'
041300940506     C                   SETON                                        99
041400940506     C                   ENDIF
041500940506     C     $TASTO        WHENEQ    F22
041600940720     C     $FC(22)       IFEQ      '0'
041700940506     C                   SETON                                        99
041800940506     C                   ENDIF
041900940506     C     $TASTO        WHENEQ    F23
042000940720     C     $FC(23)       IFEQ      '0'
042100940506     C                   SETON                                        99
042200940506     C                   ENDIF
042300940506     C     $TASTO        WHENEQ    F24
042400940720     C     $FC(24)       IFEQ      '0'
042500940506     C                   SETON                                        99
042600940506     C                   ENDIF
042700940506     C                   ENDSL
042800940506     C                   ENDSR
042900940224     C/EJECT
043000940127     C************************************************************
043100940131     C* GESTIONE LISTA
043200940127     C************************************************************
043300940127     C     GESS1         BEGSR
043400940223     C* inizializzazione videata
043500940223     C     $INZS1        IFEQ      *ON
043600940127     C                   EXSR      INZS1
043700940223     C                   MOVE      *OFF          $INZS1
043800940127     C                   ENDIF
043900940223     C* emissione piede videata se proveniente da altra
044000940223     C* salvataggio valore formato di provenienza
044100940202     C     $LASTV        IFNE      'S1'
044200940607!!   C*                    WRITEFMTT1
044300940224     C                   EXSR      GESPIE
044400940202     C                   MOVE      $LASTV        $LASTG
044500940202     C                   MOVE      'S1'          $LASTV
044600940202     C                   END
044700940223     C*
044800940223     C     WMAX          IFEQ      0
044900940607     C                   WRITE     D1
045000940211     C                   ENDIF
045100940729     C                   MOVE      '1'           WIN727            1
045200940127     C*
045300940607     C                   EXFMT     C1
045400940204     C     C1NRR         IFNE      0
045500940204     C                   Z-ADD     C1NRR         WSFL
045600940204     C                   ENDIF
045700940127     C                   Z-ADD     SFLNRR        C1RCD
045800940506     C*
045900940506     C* controllo abilitazione tasti funzione
046000940506     C*
046100940506     C                   EXSR      CTRKS1
046200940506     C     *IN99         IFEQ      *OFF
046300940506     C*
046400940127     C*
0465009401271    C                   SELECT
046600940127     C* F3=Fine
046700940506     C     $TASTO        WHENEQ    F03
046800940309     C                   EXSR      F03S1
046900940131     C* F10=Immissione
047000940506     C     $TASTO        WHENEQ    F10
047100940309     C                   EXSR      F10S1
047200940131     C* F12=Ritorno
047300940506     C     $TASTO        WHENEQ    F12
047400940309     C                   EXSR      F12S1
047500940224     C* F23=Altre opzioni
047600940506     C     $TASTO        WHENEQ    F23
047700940309     C                   EXSR      F23S1
047800940224     C* F24=Altri tasti
047900940506     C     $TASTO        WHENEQ    F24
048000940309     C                   EXSR      F24S1
048100940127     C* ROLL-UP
048200940506     C     $TASTO        WHENEQ    ROLLUP
048300940127     C                   EXSR      ROLS1
048400940127     C*
0485009401271O   C                   OTHER
048600940127     C* CONTROLLO DATI
048700940131     C                   EXSR      CTRC1
048800940201     C     *IN99         IFEQ      *OFF
048900940131     C                   EXSR      CTRS1
049000940131     C                   END
0491009401271-   C                   ENDSL
049200940127     C*
049300940506     C                   ENDIF
049400940127     C                   ENDSR
049500940224     C/EJECT
049600940127     C************************************************************
049700940131     C* INIZIALIZZAZIONE LISTA
049800940127     C************************************************************
049900940127     C     INZS1         BEGSR
050000940302     C* pulizia SFL
050100940128     C                   SETOFF                                         3031
050200940607     C                   WRITE     C1
050300940128     C                   SETON                                          31
050400940127     C*
050500940128     C* riempimento opzioni
050600940223     C                   MOVEA     $TX1          $TX
050700940223     C                   MOVEA     $FL1          $FL
050800940309     C                   Z-ADD     0             WCOZ
050900940225     C*  equivale al num.di elementi della schiera $TX1
051000940509     C                   Z-ADD     $DIMOP        WMXO
051100940128     C                   EXSR      RIEOPZ
051200950502     C                   Z-ADD     23            X
051300950502     C                   MOVE      FF23          $FC2(X)
051400940608     C                   MOVEL     WOPZ1         C1OPZ1
051500940608     C**                   MOVELWOPZ2     C1OPZ2
051600940225     C                   Z-ADD     WCOZ          C1COZ
051700940128     C*
051800940128     C* CARICAMENTO SFL A PAGINE
051900940128     C*
052000940201     C                   Z-ADD     0             S1NRR
052100940128     C                   Z-ADD     0             WMAX
052200940224     C*
052300940224     C* Posizionamento su file pilota
052400940302     C                   CLEAR                   C1POS
052500940316    >C                   EXSR      SETANA
052600940608    >C                   EXSR      REDANA
052700940302     C* ripristino i valori del posizionamento
052800940302     C                   MOVE      W1POS         C1POS
052900940131     C*
053000940131     C* Carico 1a pagina
053100940127     C                   EXSR      ROLS1
053200940201     C                   Z-ADD     SFLPAG        WPAG
053300940127     C*
053400940127     C                   ENDSR
053500940127     C/EJECT
053600940127     C************************************************************
053700940131     C* CARICAMENTO PAGINA LISTA
053800940127     C************************************************************
053900940127     C     ROLS1         BEGSR
054000940127     C*
054100940128     C                   SETOFF                                       32
054200940223     C                   Z-ADD     0             Y
054300940127     C                   Z-ADD     WMAX          S1NRR
054400940127     C*
054500940127     C* Leggo dal file anagrafico per caricare la lista
054600940127     C*
0547009401311    C     $EFILE        DOWEQ     *OFF
054800940201     C     Y             ANDLT     WPAG
054900940127     C*
055000940131     C                   EXSR      RIES1
055100940127     C*
055200940127     C                   ADD       1             S1NRR
055300940127     C                   ADD       1             Y
055400940714     C                   MOVE      $IN41         *IN41
055500940127     C*
055600940607     C                   WRITE     S1
055700940131     C*
055800940316    >C                   EXSR      REDANA
055900940128     C*
0560009401271-   C                   ENDDO
056100940127     C*
056200940223     C                   Z-ADD     S1NRR         WMAX                 30
056300940127     C*
056400940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
056500940127     C*
056600940201     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
056700940127     C                   MVR                     RESTO             3 0
056800940131     C     PAGINE        MULT      SFLPAG        C1RCD
0569009401271    C     RESTO         IFGT      0
057000940127     C                   ADD       1             C1RCD
0571009401271E   C                   ELSE
057200940407     C                   SUB       SFLPAG        C1RCD
057300940407     C                   ADD       1             C1RCD
0574009401271-   C                   ENDIF
057500940128     C*
057600940127     C                   ENDSR
057700940224     C/EJECT
057800940224     C************************************************************
057900940224     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
058000940224     C************************************************************
058100940607     C     SETANA        BEGSR
058200940224     C*
058300941004     C                   MOVE      *BLANKS       XTBKEY
058400970708     C                   MOVE      XtaAzi        XTBAZI
058500970708     C                   MOVE      XtaLin        XTBLIN
058600940608     C                   MOVE      '9'           XTBRIC
058700940607     C                   MOVE      '0'           XTBALC
058800950622     C                   MOVE      XSCCUT        XTBUTE
058900940607     C                   MOVE      '0'           XTBERR
059000941121     C**         W1POS     IFNE *BLANK
059100941004     C                   MOVE      *ZEROS        XTBKEY
059200940608     C                   MOVE      W1POS         XTBKEY
059300941121     C**                   END
059400970214     C                   CALLB     'XATB'
059500940607     C                   PARM                    XATBDS
059600940607     C*
059700940607     C     XTBERR        IFNE      '0'
059800940607     C                   MOVEL     *ON           $EFILE
059900940607     C                   ENDIF
060000940224     C*
060100940224     C                   ENDSR
060200940128     C/EJECT
060300940128     C************************************************************
060400940131     C* LETTURA RCD ARCHIVIO PILOTA
060500940128     C************************************************************
060600940607     C     REDANA        BEGSR
060700940128     C*
060800940131     C                   MOVEL     *OFF          $EFILE
060900940131     C                   MOVEL     *OFF          $RCDOK
061000940131     C*
0611009401311    C     $EFILE        DOUEQ     *ON
061200940131     C     $RCDOK        OREQ      *ON
061300940131     C*
061400940607     C                   MOVE      '3'           XTBRIC
061500940607     C                   MOVE      '0'           XTBERR
061600970214     C                   CALLB     'XATB'
061700940607     C                   PARM                    XATBDS
061800940607     C*
061900940607     C     XTBERR        IFNE      '0'
062000940607     C                   MOVEL     *ON           $EFILE
062100940607     C                   ENDIF
062200940131     C*
0623009401312    C     $EFILE        IFEQ      *OFF
062400940607     C                   EXSR      SELANA
062500950502     C                   MOVE      XTBKEY        SAVKEY
062600950502     C                   MOVE      XTBGRU        SAVGRU
062700950502     C                   MOVE      XTBUNI        SAVUNI
062800970509     C                   MOVE      XTBUNT        SAVUNT
0629009401312-   C                   ENDIF
063000940131     C*
0631009401311-   C                   ENDDO
063200940131     C*
063300940607     C                   MOVE      $EFILE        *IN33
063400940131     C*
063500940131     C                   ENDSR
063600940224     C/EJECT
063700940224     C************************************************************
063800940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
063900940224     C************************************************************
064000940224     C     CLCPAG        BEGSR
064100940224     C* Input :
064200940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
064300940224     C*          cursore
064400940224     C* - SFLPAG = numero rcd per pagina sfl
064500940224     C* Output :
064600940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
064700940224     C*
064800940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
064900940224     C                   MVR                     RESTO             3 0
065000940224     C     RESTO         IFGT      0
065100940224     C                   ADD       1             PAGINE
065200940224     C                   ENDIF
065300940224     C     PAGINE        MULT      SFLPAG        WPAG
065400940224     C*
065500940224     C                   ENDSR
065600940131     C/EJECT
065700940131     C************************************************************
065800940131     C* SELEZIONI SULLA RIGA LETTA
065900940314     C* impostare a *OFF il campo $RCDOK per scartare un record che
066000940314     C* non rientra nei parametri di parzializzazione
066100940314     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
066200940314     C* superiore stabilito nei parametri
066300940314     C*
066400940131     C************************************************************
066500940607     C     SELANA        BEGSR
066600940131     C*
066700940202     C                   MOVEL     *ON           $RCDOK
066800940714     C* Se aperto il pgm con opzione 11
066900940714     C* carico solo i record di gruppo
067000970708     C     XtaOpz        IFEQ      '11'
067100940714     C     XTBGRU        ANDNE     'G'
067200940714     C                   MOVEL     *OFF          $RCDOK
067300940714     C                   END
067400940714     C* Carica in alta intensità solo i record
067500940714     C* abilitati
067600940714     C                   MOVE      '0'           $IN41
067700970708     C     XtaOpz        IFEQ      '11'
067800940714     C     XTBGRU        ANDEQ     'G'
067900940715     C     XTBAZB        ANDNE     'N'
068000940714     C                   MOVE      '1'           $IN41
068100940714     C                   END
068200940714     C*
068300940202     C                   ENDSR
068400940131     C/EJECT
068500940131     C************************************************************
068600940131     C* RIEMPIMENTO RIGA LISTA
068700940131     C************************************************************
068800940131     C     RIES1         BEGSR
068900950502     C*                    MOVE XTBKEY    S1COD
069000950502     C*                    MOVE XTBGRU    S1GRU
069100950502     C                   MOVEL     SAVUNI        EXTDS
069200950502     C                   MOVE      SAVKEY        S1COD
069300950502     C                   MOVE      SAVGRU        S1GRU
069400970512     C                   MOVE      SAVUNT        S1UNITA           8
069500950502     C     *LIKE         DEFINE    XTBUNI        SAVUNI
069600950502     C     *LIKE         DEFINE    XTBKEY        SAVKEY
069700950502     C     *LIKE         DEFINE    XTBGRU        SAVGRU
069800970509     C     *LIKE         DEFINE    XTBUNT        SAVUNT
069900940131     C*
070000940131     C                   ENDSR
070100940309     C/EJECT
070200940309     C************************************************************
070300940309     C* GESTIONE F03 VIDEO S1
070400940309     C************************************************************
070500940309     C     F03S1         BEGSR
070600940309     C*
070700940309     C                   MOVE      *ON           $FINE
070800970708     C                   MOVE      '1'           XtaRet
070900940325     C* fine programma
071000940325     C                   EXSR      ENDPGM
071100940309     C*
071200940309     C                   ENDSR
071300940309     C/EJECT
071400940309     C************************************************************
071500940309     C* GESTIONE F10 VIDEO S1
071600940314     c* AGGIUNTA RECORD
071700940309     C************************************************************
071800940309     C     F10S1         BEGSR
071900940309     C*
072000940607     C                   RESET                   D002DS
072100940607     C                   MOVEL     '01'          OPZ002
072200970708     C                   MOVE      XtaLin        LIN002
072300970708     C                   MOVE      XtaAzi        AZI002
072400970512     C                   MOVE      S1UNITA       UNI002
072500940715     C                   MOVE      *BLANKS       KPJBU
072600940607     C                   MOVEL     D002DS        KPJBU
072700940608$004 C                   CALL      'ANGYP1R2'
072800940309     C                   PARM                    KPJBA
072900940607     C                   MOVEL     KPJBU         D002DS
073000940309     C* ritorno da PGM gestione
073100940309     C                   EXSR      GESRET
073200940309     C     WINZS1        IFEQ      *ON
073300940309     C                   MOVE      *ON           $INZS1
073400940309     C* se esistevano già righe sul sfl
073500940309     C* calcolo pagina a cui deve posizionarsi
073600940309     C     WSFL          IFGT      0
073700940309     C                   EXSR      CLCPAG
073800940309     C* altrimenti carico solo la 1a pagina
073900940309     C                   ELSE
074000940309     C                   Z-ADD     SFLPAG        WPAG
074100940309     C                   ENDIF
074200940309     C                   ENDIF
074300940309     C*
074400940712     C                   MOVE      *HIVAL        $LASTV
074500940309     C                   ENDSR
074600940309     C/EJECT
074700940309     C************************************************************
074800940309     C* GESTIONE F12 VIDEO S1
074900940309     C************************************************************
075000940309     C     F12S1         BEGSR
075100940309     C*
075200970708     C                   MOVE      '2'           XtaRet
075300940309     C                   MOVE      *ON           $FINE
075400940309     C*
075500940309     C                   ENDSR
075600940309     C/EJECT
075700940309     C************************************************************
075800940309     C* GESTIONE F23 VIDEO S1
075900940309     C************************************************************
076000940309     C     F23S1         BEGSR
076100940309     C*
076200940309     C                   EXSR      RIEOPZ
076300950502     C                   Z-ADD     23            X
076400950502     C                   MOVE      FF23          $FC2(X)
076500940608     C                   MOVEL     WOPZ1         C1OPZ1
076600940608     C**                   MOVELWOPZ2     C1OPZ2
076700940309     C                   Z-ADD     WCOZ          C1COZ
076800940309     C*
076900940309     C                   ENDSR
077000940309     C/EJECT
077100940309     C************************************************************
077200940309     C* GESTIONE F24 VIDEO S1
077300940309     C************************************************************
077400940309     C     F24S1         BEGSR
077500940309     C*
077600940506     C*
077700940506     C                   MOVEA     $FX2          $FX
077800940506     C                   MOVEA     $FC2          $FC
077900940506     C                   MOVEA     $FR2          $FR
078000940506     C                   Z-ADD     $ULKS1        $ULK
078100940506     C                   EXSR      RIEKEY
078200941017     C                   MOVEA     $FC           $FC2
078300940506     C                   Z-ADD     $ULK          $ULKS1            3 0
078400940607     C                   MOVEL     $KEY1         Z1KE1
078500941004     C                   MOVEL     $KEY2         Z1KE2
078600940607     C                   WRITE     Z1
078700940506     C*
078800940309     C                   ENDSR
078900940128     C/EJECT
079000940128     C************************************************************
079100940131     C* CONTROLLO TESTATA LISTA
079200940128     C************************************************************
079300940131     C     CTRC1         BEGSR
079400940128     C*
079500940201     C                   MOVE      *OFF          *IN99
079600940131     C*
079700940202     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
079800940202     C* L'INIZIALIZZAZIONE LISTA
079900940202     C*
080000940207     C     C1POS         IFNE      W1POS
080100940202     C                   EXSR      POSS1
080200940202     C                   END
080300940202     C*
080400940202     C                   ENDSR
080500940202     C/EJECT
080600940202     C************************************************************
080700940202     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
080800940202     C************************************************************
080900940202     C     POSS1         BEGSR
081000940202     C*
081100940202     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
081200940202     C*
0813009402021    C     WMAX          IFGT      0
081400940607     C                   READC     S1                                     21
0815009402022    C     *IN21         DOWEQ     *OFF
081600940202     C     *IN99         ANDEQ     *OFF
081700940202     C*
0818009402023    C     S1OPZ         IFNE      0
081900940202     C                   SETON                                        51  99
0820009402023-   C                   ENDIF
082100940202     C* se rilevata opzione attiva, attivo sflnxtchg
0822009402023    C     *IN99         IFEQ      *ON
082300940202     C                   MOVE      *ON           *IN32
082400940714     C                   MOVE      $IN41         *IN41
082500940607     C                   UPDATE    S1
0826009402023E   C                   ELSE
082700940607     C                   READC     S1                                     21
0828009402023-   C                   ENDIF
082900940202     C*
0830009402022-   C                   ENDDO
083100940202     C*
0832009402021-   C                   ENDIF
083300940202     C*
083400940221     C*
0835009402021    C     *IN99         IFEQ      *OFF
083600941121     C                   CLEAR                   W1POS
083700940202     C*
083800940302     C                   MOVEL     C1POS         W1POS
083900940131     C*
084000940202     C                   MOVE      *ON           *IN99
084100940223     C                   MOVE      *ON           $INZS1
084200940202     C*
084300940202     C*
0844009402021-   C                   ENDIF
084500940202     C*
084600940131     C                   ENDSR
084700940131     C/EJECT
084800940131     C************************************************************
084900940131     C* CONTROLLO OPZIONI LISTA
085000940131     C************************************************************
085100940131     C     CTRS1         BEGSR
085200940131     C*
085300940202     C                   MOVEL     *OFF          $ESCI
085400940201     C                   SETOFF                                       99
085500940131     C                   Z-ADD     0             S1OPZ
085600940131     C*
085700940127     C* Leggo il sfl solo se ci sono rcd
0858009401311    C     WMAX          IFGT      0
085900940607     C                   READC     S1                                     21
086000940127     C*
086100940131     C* esce se fine sfl o errore che richiede l'uscita
0862009401312    C     *IN21         DOWEQ     *OFF
086300940131     C     $ESCI         ANDEQ     *OFF
086400940201     C                   Z-ADD     S1NRR         C1RCD
086500940131     C* ctrl su riga
086600940131     C                   EXSR      RECS1
086700940131     C* gestione opzioni
0868009401313    C     S1OPZ         IFNE      0
086900940201     C     *IN99         ANDEQ     *OFF
087000940131     C                   EXSR      OPZS1
0871009401273-   C                   ENDIF
087200940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0873009402013    C     *IN99         IFEQ      *ON
087400940201     C     $ESCI         OREQ      *ON
087500940131     C                   MOVE      *ON           *IN32
087600940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
087700940204     C* la ripristinerò a conclusione del ciclo di READC
087800940223     C                   MOVE      *OFF          $INZS1
0879009402233-   C                   ENDIF
088000940223     C*
088100940131     C                   Z-ADD     0             S1OPZ
088200940223     C*
088300940714     C                   MOVE      $IN41         *IN41
088400940607     C                   UPDATE    S1
088500940223     C*
088600940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0887009401313    C     $ESCI         IFEQ      *OFF
088800940607     C                   READC     S1                                     21
088900940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0890009402014    C     *IN21         IFEQ      *ON
089100940201     C                   MOVE      WINZS1        $INZS1
089200940204     C* calcolo pagina a cui deve posizionarsi
089300940224     C                   EXSR      CLCPAG
0894009402014-   C                   ENDIF
0895009402013-   C                   ENDIF
089600940131     C*
0897009401272-   C                   ENDDO
089800940127     C*
0899009401311-   C                   ENDIF
090000940131     C*
090100940127     C                   ENDSR
090200940127     C/EJECT
090300940127     C************************************************************
090400940131     C* CONTROLLO CAMPI I/O RIGA LISTA
090500940127     C************************************************************
090600940131     C     RECS1         BEGSR
090700940131     C*
090800940201     C* reset indicatori DSPATR
090900940201     C                   MOVE      *ALL'0'       IN4049           10
091000940201     C                   MOVEA     IN4049        *IN(40)
091100940201     C*
091200940131     C* Controllo se Utente Autorizzato alla scelta
091300940202     C     S1OPZ         IFNE      0
091400940131     C                   Z-ADD     1             X
091500940223     C     S1OPZ         LOOKUP    $OP1(X)                                21
091600940211     C     *IN21         IFEQ      *OFF
091700940223     C     $FL1(X)       OREQ      'N'
091800940131     C                   MOVE      *ON           $ESCI
091900940201     C                   SETON                                        504099
092000940131     C                   ENDIF
092100940131     C*
092200940620     c* Controllo autorizzazione alla modifica su record di gruppo
092300940620     C* Reperisco tipo codice (gruppo/azienda)
092400940620     C                   MOVE      *ALL'0'       XTBKEY
092500940620     C                   MOVE      S1COD         XTBKEY
092600970512     C                   MOVE      S1UNITA       XTBUNT
092700940620     C                   MOVE      '0'           XTBALC
092800940620     C                   MOVE      '1'           XTBRIC
092900970214     C                   CALLB     'XATB'
093000940620     C                   PARM                    XATBDS
093100940620     C* Reperisco autorizzazioni utente
093200940620     C                   CLEAR                   XCHATB
093300941118     C                   MOVE      XSCSOC        XCTSOC
093400941118     C                   MOVE      XSCDUN        XCTUNI
093500940620     C                   MOVEL     KNMUS         XCTPRF
093600940620     C                   MOVE      XTBCOD        XCTCOD
093700940620     C                   CALL      'XCHKATB'
093800940620     C                   PARM                    XCHATB
093900941111     C                   MOVEL     XCTDDS        AUTDS
094000941027     C     XCTRTN        IFEQ      1
094100941027     C                   MOVEL     'N'           MANGRU
094200941027     C                   ENDIF
094300940620     C* Non ammessa manutenzione di gruppo
094400940620   1 C     XTBGRU        IFEQ      'G'
094500940620     C     MANGRU        ANDEQ     'N'
094600940620   2 C     S1OPZ         IFEQ      2
094700940729     C     S1OPZ         OREQ      3
094800940620     C     S1OPZ         OREQ      4
094900940620     C                   SETON                                        5399
095000940620   2 C                   ENDIF
095100940620   1 C                   ENDIF
095200940620     C* Non ammessa manutenzione di azienda
095300940620   1 C     XTBGRU        IFEQ      'A'
095400940620     C     MANAZI        ANDEQ     'N'
095500940620   2 C     S1OPZ         IFEQ      2
095600940729     C     S1OPZ         OREQ      3
095700940620     C     S1OPZ         OREQ      4
095800940620     C                   SETON                                        5499
095900940620   2 C                   ENDIF
096000940620   1 C                   ENDIF
096100940607     C                   ENDIF
096200940131     C                   ENDSR
096300940224     C/EJECT
096400940131     C************************************************************
096500940131     C* GESTIONE OPZIONI LISTA
096600940131     C************************************************************
096700940131     C     OPZS1         BEGSR
096800940201     C*
0969009402251    C                   SELECT
097000940225     C*
097100940225     C* se opz=Selezione passo la chiave al pgm chiamante
097200940225     C     S1OPZ         WHENEQ    1
097300970708     C                   MOVE      S1OPZ         XtaOpz
097400970708     C                   MOVE      S1COD         XtaKey
097500970708     C                   MOVE      S1UNIta       xtaUni
097600940225     C                   MOVE      *ON           $ESCI
097700940225     C                   MOVE      *ON           $FINE
097800940729     C*
097900940714     C*
098000940714     C* se opz=Abilita scrive su archivio abilitazioni
098100940714     C     S1OPZ         WHENEQ    11
098200941103     C                   MOVE      XSCSOC        ABLAZI
098300940714     C                   MOVE      *ZEROS        ABLKEY
098400940714     C                   MOVE      S1COD         ABLKEY
098500970512     C                   MOVE      S1UNITA       ABLUNT
098600940714     C                   MOVE      XTBCOD        ABLCOD
098700940714     C                   WRITE     ANABL000                             22
098800940714     C*
098900940714     C* se opz=Disabil.cancella su archivio abilitazioni
099000940714     C     S1OPZ         WHENEQ    12
099100941103     C                   MOVE      XSCSOC        ABLAZI
099200940714     C                   MOVE      *ZEROS        ABLKEY
099300940714     C                   MOVE      S1COD         ABLKEY
099400970512     C                   MOVE      S1UNITA       ABLUNT
099500940714     C                   MOVE      XTBCOD        ABLCOD
099600940714     C     KABL          DELETE    ANABL000                           22
099700940729     C*
099800940225     C*
099900940225     C* se altre opzioni (o nessuna opzione) richiamo pgm gestione
1000009402251O   C                   OTHER
100100940225     C*
100200940616     C                   RESET                   D002DS
100300940607     C                   MOVEL     S1OPZ         OPZ002
100400940616     C                   MOVE      *ZERO         KEY002
100500940607     C                   MOVE      S1COD         KEY002
100600970708     C                   MOVE      XtaLin        LIN002
100700970708     C                   MOVE      XtaAzi        AZI002
100800940715     C                   MOVE      *BLANKS       KPJBU
100900940607     C                   MOVEL     D002DS        KPJBU
101000940608$004 C                   CALL      'ANGYP1R2'
101100940607     C                   PARM                    KPJBA
101200940607     C                   MOVEL     KPJBU         D002DS
101300940201     C*
101400940223     C* ritorno da PGM gestione
101500940223     C                   EXSR      GESRET
101600940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
1017009402252    C     *IN99         IFEQ      *ON
101800940223     C                   SETON                                        40
1019009402252-   C                   ENDIF
102000940131     C*
102100940506     C                   MOVE      *HIVAL        $LASTV
1022009402251-   C                   ENDSL
102300940225     C*
102400940131     C                   ENDSR
102500940223     C/EJECT
102600940223     C************************************************************
102700940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
102800940223     C************************************************************
102900940223     C     GESRET        BEGSR
103000940223     C*
103100940223     C* modo di ritorno
103200940223     C*
1033009402231    C                   SELECT
103400940314    >C* << questi modi di utilizzo dei valori di ritorno dal
103500940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
103600940314    >C*    proposte, normalmente sempre valide, ma modificabili
103700940314    >C*    per situazioni particolari >>
103800940223     C* 1 = F3
103900940314    >C     RET002        WHENEQ    '1'
104000940224     C                   MOVE      *ON           $ESCI
104100940223     C                   MOVE      *ON           $FINE
104200940223     C* 2 = F12
104300940314    >C     RET002        WHENEQ    '2'
104400940223     C                   MOVE      *ON           $ESCI
104500940223     C*
1046009402231-   C                   ENDSL
104700940223     C*
104800940223     C* operazione eseguite dal pgm chiamato
104900940223     C*
1050009402231    C                   SELECT
105100940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
105200940314    >C     OPR002        WHENEQ    '1'
105300940223     C                   MOVE      *ON           WINZS1
105400940223     C*
1055009402231-   C                   ENDSL
105600940223     C*
105700940223     C* funzione non eseguibile per errore :
105800940223     C*
1059009402231    C                   SELECT
106000940223     C* 1 = funzione richiamata chiusa in errore
106100940316    >C*  eventualmente gestire altri codici di errore
106200940314    >C     ERR002        WHENEQ    '1'
106300940223     C                   MOVE      *ON           $ESCI
106400940223     C                   SETON                                        5299
106500940223     C*
1066009402231-   C                   ENDSL
106700940223     C*
106800940223     C                   ENDSR
106900940223     C/EJECT
107000940131     C************************************************************
107100940131     C* OPERAZIONI INIZIALI
107200940131     C************************************************************
107300940131     C     *INZSR        BEGSR
107400940131     C*
107500940127     C* Reperimento parametri
107600940127     C*
107700940117     C     *ENTRY        PLIST
107800940117     C                   PARM                    KPJBA
107900940714     C*
108000940714     C* Reperimento Codice utente
108100940714     C*
108200950622     C*          KNMUS     CHAINANUTN01L             21
108300940225     C*
108400940223     C* Reperimento opzioni
108500940127     C*
108600940128     C                   EXSR      REPOPZ
108700940506     C*
108800940506     C* Reperimento tasti di funzione
108900940506     C*
109000940506     C                   EXSR      REPKEY
109100940506     C*
109200940607$007 C                   MOVE      'YP1'         XTBCOD
109300940127     C*
109400940117     C                   ENDSR
109500940225     C/EJECT
109600941103      ************************************************************
109700941103      * Reperimento dati società
109800941103      ************************************************************
109900941103     C     REPSOC        BEGSR
110000941103      *
110100970214     C                   CALLB     'XSOC'
110200941103     C                   PARM                    TIPXSC            6
110300941103     C                   PARM                    SOCXSC            3
110400941103     C                   PARM                    CDSXSC            9 0
110500941103     C                   PARM                    MODXSC            3
110600941103     C                   PARM      *BLANKS       RTNXSC            1
110700941103     C                   PARM                    XSOCDS
110800941103     C                   PARM                    KPJBA
110900941103      *
111000941103     C                   ENDSR
111100940223     C************************************************************
111200940223     C* INIZIALIZZAZIONE VARIABILI
111300940223     C************************************************************
111400940223     C     INZVAR        BEGSR
111500970926     C*
111600970926     C* Dati da PGM chiamante
111700970926     C*
111800970926     C                   MOVEL     KPJBU         XTABDS
111900951124     C*
112000951124     C* Reperimento codice lingua e codice azienda
112100951124     C*
112200951124     C                   MOVEL     'SOC001'      TIPXSC
112300951124     C                   EXSR      REPSOC
112400951124     C     RTNXSC        IFNE      '1'
112500951124     C                   MOVEL     XSOCDS        SOC001
112600951124     C                   ELSE
112700951124     C                   MOVE      *ON           $FINE
112800951124     C* fine programma
112900951124     C                   EXSR      ENDPGM
113000951124     C                   ENDIF
113100951124     C* Lingua e codice azienda
113200970708     C                   MOVE      XSCLIN        XtaLin
113300970708     C                   MOVE      XSCSOC        XtaAzi
113400940715     C*
113500940715     C                   RESET                   $FC2
113600940715     C*
113700940715$021 C                   CLOSE     ANGYP1D
113800940715$021 C                   OPEN      ANGYP1D
113900940506     C*
114000940506     C* Definizione delle opzioni utilizzabili a pgm
114100940506     C*
114200940506     C                   EXSR      DEFOPZ
114300940718     C* Pulizia campi e indicatori
114400940718     C                   CLEAR                   W1POS
114500940718     C                   MOVE      *ALL'0'       IN4049           10
114600940718     C                   MOVEA     IN4049        *IN(40)
114700940718     C                   CLEAR                   S1OPZ
114800940223     C* Variabili per gestione videate
114900940223     C*
115000940223     C                   MOVE      *OFF          $FINE
115100940223     C                   MOVE      *OFF          $INZS1
115200940223     C                   MOVE      *OFF          $EFILE
115300940223     C                   MOVE      *OFF          $ESCI
115400940223     C                   MOVE      *OFF          $RCDOK
115500940509     C                   MOVE      *HIVAL        $LASTV
115600940506     C                   Z-ADD     0             $ULKS1            3 0
115700940223     C*
115800940318     C                   MOVE      *ON           $INZS1
115900940223     C                   MOVE      'S1'          $GEST
116000940223     C*
116100940223     C* Variabili appoggio
116200940223     C*
116300940223     C                   Z-ADD     SFLPAG        WPAG
116400940223     C*
116500940223     C* Valorizzazione campi univoci testate
116600940223     C*
116700940607     C*                    CLEART1
116800940607     C*                    MOVELKNSIF     NOMSIF
116900940607     C*                    MOVELRAGUTE    NOMDIT
117000940607     C*                    MOVELDSPGM     SIGLA
117100940607     C*                    MOVELPROGR     NOMPGM
117200940223     C*
117300940608     c* posizione window
117400940608$012 C                   Z-ADD     41            PCOL              2 0
117500940613     C                   Z-ADD     09            R
117600940608     C     80            SUB       PCOL          C
117700940608     C                   SUB       3             C
117800970708     C     XtaOpz        IFNE      '  '
117900970708     C     XtaRig        IFGT      11
118000940608     C                   Z-ADD     1             R
118100940608     C                   ENDIF
118200940608     C*
118300970708     C     XtaCol        IFGT      40
118400940608     C                   Z-ADD     2             C
118500940608     C                   ENDIF
118600940608     C                   ENDIF
118700940616     C*
118800970708     C     XtaOpz        IFNE      '01'
118900940616$019 C                   DO        00            I                 3 0
119000940721$022 C*                  MOVEL     LIN(I)        TBTFLD
119100940721$022 C*                  CLEAR                   TBTKEY
119200940721$022 C*                  CLEAR                   TBTAZI
119300940721$022 C*                  CLEAR                   TBTLIN
119400940721$007 C                   MOVE      'YP1'         TBTCOD            3
119500940721$022 C*    K5TBT1        CHAIN     ANTBT01L                           21
119600940721$022 C*    *IN21         IFEQ      *ON
119700940721$022 C*                  Z-ADD     LLUN(I)       TBTLUN
119800940721$022 C*                  Z-ADD     LPOS(I)       TBTPOS
119900940721$022 C*                  MOVEL     LIND(I)       TBTDES
120000940721$022 C*                  WRITE     ANTBT000
120100940721$022 C*                  ENDIF
120200940616     C                   ENDDO
120300940616     C                   ENDIF
120400940608     C*
120500940722     c*
120600940722     c* Se richiamato con OPZ 11  Non controlla abilitazione all'aziend
120700970708     C     XtaOpz        IFEQ      '11'
120800940722     C                   MOVE      'N'           XTBGAB
120900940722     C                   ELSE
121000940722     C                   MOVE      'S'           XTBGAB
121100940722     C                   ENDIF
121200940223     C                   ENDSR
121300940223     C/EJECT
121400940131     C************************************************************
121500940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
121600940131     C************************************************************
121700940225     C     DEFOPZ        BEGSR
121800940131     C*
121900940225     C* Questa routine deve valorizzare la schiera $FL1
122000940225     C* con S o N a seconda della utilizzabilità o meno
122100940225     C* dell'opzione correlata
122200940225     C*
122300940714     C* Inizialmente si considerano tutte le opzioni come non valide
122400940728     C     1             DO        14            X
122500940714     C                   MOVE      'N'           $FL1(X)
122600940225     C                   END
122700940225     C*
122800940225     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
122900940714     C* rendo valide alcune opzioni
123000940714     C* i tasti invece li disattivo
123100940225     C                   SELECT
123200940714     C*
123300940714     C* Se pgm richiamato in RICERCA attivo :
123400940714     C*
123500970708    >C     XtaOpz        WHENEQ    '01'
123600940714     C* F3=Fine
123700940714     C                   Z-ADD     3             X
123800940720     C                   MOVE      '0'           $FC2(X)
123900940714     C* F10=Immissione
124000940714     C                   Z-ADD     10            X
124100940919     C                   MOVE      '1'           $FC2(X)
124200940714     C* 1=Selezione
124300940714     C                   Z-ADD     1             X
124400940714     C     01            LOOKUP    $OP1(X)                                21
124500940714     C     *IN21         IFEQ      *ON
124600940714     C                   MOVE      'S'           $FL1(X)
124700940714     C                   ENDIF
124800940729     C* 2=Modifica
124900940729     C                   Z-ADD     1             X
125000940729     C     02            LOOKUP    $OP1(X)                                21
125100940729     C     *IN21         IFEQ      *ON
125200940729     C                   MOVE      'S'           $FL1(X)
125300940729     C                   ENDIF
125400940729     C* 3=Copia
125500940729     C                   Z-ADD     1             X
125600940729     C     03            LOOKUP    $OP1(X)                                21
125700940729     C     *IN21         IFEQ      *ON
125800940729     C                   MOVE      'S'           $FL1(X)
125900940729     C                   ENDIF
126000940729     C* 4=Annullamento
126100940729     C                   Z-ADD     1             X
126200940729     C     04            LOOKUP    $OP1(X)                                21
126300940729     C     *IN21         IFEQ      *ON
126400940729     C                   MOVE      'S'           $FL1(X)
126500940729     C                   ENDIF
126600940714     C* 5=Visualizzazione
126700940714     C                   Z-ADD     1             X
126800940714     C     05            LOOKUP    $OP1(X)                                21
126900940714     C     *IN21         IFEQ      *ON
127000940714     C                   MOVE      'S'           $FL1(X)
127100940714     C                   ENDIF
127200940728     C*14=Commenti
127300940831     C**                   Z-ADD1         X
127400940831     C**         14        LOKUP$OP1,X                   21
127500940831     C**         *IN21     IFEQ *ON
127600940831     C**                   MOVE 'S'       $FL1,X
127700940831     C**                   ENDIF
127800940714     C*
127900940714     C* Se pgm richiamato in INTERROGAZIONE attivo :
128000940714     C*
128100970708    >C     XtaOpz        WHENEQ    '05'
128200940714     C* F10=Immissione
128300940714     C                   Z-ADD     10            X
128400940720     C                   MOVE      '0'           $FC2(X)
128500940714     C* F3=Fine
128600940714     C                   Z-ADD     3             X
128700940720     C                   MOVE      '0'           $FC2(X)
128800940714     C* 5=Visualizzazione
128900940225     C                   Z-ADD     1             X
129000940714     C     05            LOOKUP    $OP1(X)                                21
129100940225     C     *IN21         IFEQ      *ON
129200940714     C                   MOVE      'S'           $FL1(X)
129300940225     C                   ENDIF
129400940728     C*14=Commenti
129500940831     C**                   Z-ADD1         X
129600940831     C**         14        LOKUP$OP1,X                   21
129700940831     C**         *IN21     IFEQ *ON
129800940831     C**                   MOVE 'S'       $FL1,X
129900940831     C**                   ENDIF
130000940831     C*
130100940714     C* Se pgm richiamato in ABILITAZIONE attivo :
130200940714     C*
130300970708    >C     XtaOpz        WHENEQ    '11'
130400940714     C* F10=Immissione
130500940714     C                   Z-ADD     10            X
130600940720     C                   MOVE      '0'           $FC2(X)
130700940714     C* F3=Fine
130800940714     C                   Z-ADD     3             X
130900940720     C                   MOVE      '0'           $FC2(X)
131000940714     C* 5=Visualizzazione
131100940714     C                   Z-ADD     1             X
131200940714     C     05            LOOKUP    $OP1(X)                                21
131300940714     C     *IN21         IFEQ      *ON
131400940714     C                   MOVE      'S'           $FL1(X)
131500940714     C                   ENDIF
131600940714     C* 11=Abilitazione
131700940714     C                   Z-ADD     1             X
131800940714     C     11            LOOKUP    $OP1(X)                                21
131900940714     C     *IN21         IFEQ      *ON
132000940714     C                   MOVE      'S'           $FL1(X)
132100940714     C                   ENDIF
132200940714     C* 12=Disabilita
132300940714     C                   Z-ADD     1             X
132400940714     C     12            LOOKUP    $OP1(X)                                21
132500940714     C     *IN21         IFEQ      *ON
132600940714     C                   MOVE      'S'           $FL1(X)
132700940714     C                   ENDIF
132800940714     C*
132900940714     C* Se pgm richiamato in GESTIONE  attivo :
133000940714     C*
133100970708    >C     XtaOpz        WHENEQ    '02'
133200940714     C* F3=Fine
133300940714     C                   Z-ADD     3             X
133400940720     C                   MOVE      '0'           $FC2(X)
133500940714     C* 2=Modifica
133600940714     C                   Z-ADD     1             X
133700940714     C     02            LOOKUP    $OP1(X)                                21
133800940714     C     *IN21         IFEQ      *ON
133900940714     C                   MOVE      'S'           $FL1(X)
134000940714     C                   ENDIF
134100940714     C* 3=Copia
134200940714     C                   Z-ADD     1             X
134300940714     C     03            LOOKUP    $OP1(X)                                21
134400940714     C     *IN21         IFEQ      *ON
134500940714     C                   MOVE      'S'           $FL1(X)
134600940714     C                   ENDIF
134700940714     C* 4=Annullamento
134800940714     C                   Z-ADD     1             X
134900940714     C     04            LOOKUP    $OP1(X)                                21
135000940714     C     *IN21         IFEQ      *ON
135100940714     C                   MOVE      'S'           $FL1(X)
135200940714     C                   ENDIF
135300940714     C* 5=Visualizzazione
135400940714     C                   Z-ADD     1             X
135500940714     C     05            LOOKUP    $OP1(X)                                21
135600940714     C     *IN21         IFEQ      *ON
135700940714     C                   MOVE      'S'           $FL1(X)
135800940714     C                   ENDIF
135900940729     C*14=Commenti
136000940831     C**                   Z-ADD1         X
136100940831     C**         14        LOKUP$OP1,X                   21
136200940831     C**         *IN21     IFEQ *ON
136300940831     C**                   MOVE 'S'       $FL1,X
136400940831     C**                   ENDIF
136500940225     C*
136600940225     C                   OTHER
136700940714     C* Se pgm non richiamato, attivo TUTTO  :
136800940714     C* 2=Modifica
136900940714     C                   Z-ADD     1             X
137000940714     C     02            LOOKUP    $OP1(X)                                21
137100940714     C     *IN21         IFEQ      *ON
137200940714     C                   MOVE      'S'           $FL1(X)
137300940714     C                   ENDIF
137400940714     C* 3=Copia
137500940714     C                   Z-ADD     1             X
137600940714     C     03            LOOKUP    $OP1(X)                                21
137700940714     C     *IN21         IFEQ      *ON
137800940714     C                   MOVE      'S'           $FL1(X)
137900940714     C                   ENDIF
138000940714     C* 4=Annullamento
138100940714     C                   Z-ADD     1             X
138200940714     C     04            LOOKUP    $OP1(X)                                21
138300940714     C     *IN21         IFEQ      *ON
138400940714     C                   MOVE      'S'           $FL1(X)
138500940714     C                   ENDIF
138600940714     C* 5=Visualizzazione
138700940714     C                   Z-ADD     1             X
138800940714     C     05            LOOKUP    $OP1(X)                                21
138900940714     C     *IN21         IFEQ      *ON
139000940714     C                   MOVE      'S'           $FL1(X)
139100940714     C                   ENDIF
139200940714     C* 11=Abilitazione
139300940714     C                   Z-ADD     1             X
139400940714     C     11            LOOKUP    $OP1(X)                                21
139500940714     C     *IN21         IFEQ      *ON
139600940714     C                   MOVE      'S'           $FL1(X)
139700940714     C                   ENDIF
139800940714     C* 12=Disabilita
139900940714     C                   Z-ADD     1             X
140000940714     C     12            LOOKUP    $OP1(X)                                21
140100940714     C     *IN21         IFEQ      *ON
140200940714     C                   MOVE      'S'           $FL1(X)
140300940714     C                   ENDIF
140400940728     C*14=Commenti
140500940831     C**                   Z-ADD1         X
140600940831     C**         14        LOKUP$OP1,X                   21
140700940831     C**         *IN21     IFEQ *ON
140800940831     C**                   MOVE 'S'       $FL1,X
140900940831     C**                   ENDIF
141000940225     C* poi, disattivo a seconda delle autorizzazioni :
141100940314    >C*********  AUTXXX    IFEQ 'N'
141200940314    >C*********            Z-ADD1         X
141300940314    >C*********  'XX'      LOKUP$OP1,X                   21
141400940314    >C*********  *IN21     IFEQ *ON
141500940314    >C*********            MOVE 'N'       $FL1,X
141600940314    >C*********            ENDIF
141700940314    >C*********            ENDIF
141800940225     C*
141900940225     C                   ENDSL
142000940225     C*
142100940225     C                   ENDSR
142200940225     C/EJECT
142300940225     C************************************************************
142400940225     C* REPERIMENTO OPZIONI
142500940225     C************************************************************
142600940225     C     REPOPZ        BEGSR
142700940225     C*
142800940314     C     1             DO        $DIMOP        X
142900940128     C*
143000940223     C     $MS1(X)       IFNE      *BLANK
143100940128     C*
143200940223     C                   MOVEL     $MS1(X)       IDMSG
143300970214     C                   CALLB     'XRTVM'
143400940201     C                   PARM                    IDMSG            27
143500940201     C                   PARM                    TXTMSG          100
143600940201     C                   PARM                    ERRMSG            1
143700940128     C*
143800940128     C     ERRMSG        IFNE      *ON
143900940223     C                   MOVEL     TXTMSG        $TX1(X)
144000940128     C                   ELSE
144100940223     C                   MOVEL     *ALL'?'       $TX1(X)
144200940128     C                   END
144300940128     C*
144400940128     C                   END
144500940128     C*
144600940128     C                   END
144700940128     C*
144800940128     C                   ENDSR
144900940506     C/EJECT
145000940506     C************************************************************
145100940506     C* REPERIMENTO TASTI DI FUNZIONE
145200940506     C************************************************************
145300940506     C     REPKEY        BEGSR
145400940506     C*
145500940506     C                   MOVEA     $FM2          $FM
145600940506     C                   EXSR      REPKKK
145700940506     C                   MOVEA     $FX           $FX2
145800940506     C                   ENDSR
145900940506     C/EJECT
146000940506     C************************************************************
146100940506     C* REPERIMENTO TASTI DI FUNZIONE
146200940506     C************************************************************
146300940506     C     REPKKK        BEGSR
146400940506     C*
146500940506     C                   CLEAR                   $FX
146600940506     C     1             DO        24            X
146700940506     C*
146800940506     C     $FM(X)        IFNE      *BLANK
146900940506     C*
147000940506     C                   MOVEL     $FM(X)        IDMSG
147100970214     C                   CALLB     'XRTVM'
147200940506     C                   PARM                    IDMSG            27
147300940506     C                   PARM                    TXTMSG          100
147400940506     C                   PARM                    ERRMSG            1
147500940506     C*
147600940506     C     ERRMSG        IFNE      *ON
147700940506     C                   MOVEL     TXTMSG        $FX(X)
147800940506     C                   ELSE
147900940506     C                   MOVEL     *ALL' '       $FX(X)
148000940506     C                   END
148100940506     C*
148200940506     C                   END
148300940506     C*
148400940506     C                   END
148500940506     C*
148600940506     C                   ENDSR
148700940223     C/EJECT
148800940128     C************************************************************
148900940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
149000940128     C************************************************************
149100940223     C     DEFCAM        BEGSR
149200940128     C*
149300940223     C* Variabili per gestione videate
149400940223     C*
149500940223     C                   MOVE      *BLANK        $GEST             2
149600940223     C                   MOVE      *BLANK        $FINE             1
149700940223     C                   MOVE      *BLANK        $INZS1            1
149800940223     C                   MOVE      *BLANK        $LASTG            2
149900940223     C                   MOVE      *BLANK        $LASTV            2
150000940223     C                   MOVE      *BLANK        $EFILE            1
150100940223     C                   MOVE      *BLANK        $ESCI             1
150200940223     C                   MOVE      *BLANK        $RCDOK            1
150300940223     C*
150400940314     C* Variabili appoggio sempre presenti in tutte le anagrafiche
150500940223     C*
150600940223     C     *LIKE         DEFINE    C1RCD         S1NRR
150700940223     C     *LIKE         DEFINE    C1NRR         WSFL
150800940223     C     *LIKE         DEFINE    C1RCD         WMAX
150900940223     C     *LIKE         DEFINE    C1RCD         WPAG
151000940223     C     *LIKE         DEFINE    $INZS1        WINZS1
151100940223     C                   MOVE      *BLANK        WWGRP             3
151200940223     C                   Z-ADD     0             CURR              2 0
151300940223     C                   Z-ADD     0             CURC              2 0
151400940223     C                   MOVE      *BLANK        WIN              99
151500940314     C     *LIKE         DEFINE    C1POS         W1POS
151600940620     C*
151700940721$022 C*    K5TBT1        KLIST
151800940721$022 C*                  KFLD                    TBTAZI
151900940721$022 C*                  KFLD                    TBTCOD
152000940721$022 C*                  KFLD                    TBTKEY
152100940721$022 C*                  KFLD                    TBTLIN
152200940721$022 C*                  KFLD                    TBTFLD
152300940714     C*
152400940714     C     KABL          KLIST
152500940714     C                   KFLD                    ABLCOD
152600940714     C                   KFLD                    ABLKEY
152700940714     C                   KFLD                    ABLAZI
152800940314     C*
152900940314     C*
153000940223     C* Indici
153100940223     C*
153200940223     C                   Z-ADD     0             X                 3 0
153300940223     C                   Z-ADD     0             Y                 3 0
153400940127     C*
153500940117     C                   ENDSR
153600940223     C/EJECT
153700940616**
153800940225PROMSG    *LIBL     OPZ0001
153900940225PROMSG    *LIBL     OPZ0002
154000940131PROMSG    *LIBL     OPZ0003
154100940131PROMSG    *LIBL     OPZ0004
154200940131PROMSG    *LIBL     OPZ0005
154300940224PROMSG    *LIBL     OPZ0006
154400940224PROMSG    *LIBL     OPZ0007
154500940224PROMSG    *LIBL     OPZ0008
154600940224PROMSG    *LIBL     OPZ0009
154700970716PROMSG    *LIBL     OPZ0010
154800970716PROMSG    *LIBL     OPZ0013
154900970716PROMSG    *LIBL     OPZ0012
155000970716PROMSG    *LIBL     OPZ0011
155100970716PROMSG    *LIBL     OPZ0014
155200940201**
15530094022501S      <Selezione
15540094022502S      <Modifica
15550094022503S      <Copia
15560094022504S      <Annullamento
15570094022505S      <Visualizzazione
15580094060706N      <prova
15590094060707N      <prova
15600094060708N      <prova
15610094060709N      <prova
15620094072910S      <Invia
15630094071411S      <Abilita gruppo
15640094071412S      <Disabilita
15650094072813N      <PROVA
15660094083114N      <Commenti
156700940506** TASTI DI FUNZIONE  S1
156800940506PROMSG    *LIBL     KEY0001  01
156900940506PROMSG    *LIBL     KEY0002  02
157000940506PROMSG    *LIBL     KEY0003  03
157100940506PROMSG    *LIBL     KEY0004  04
157200940506PROMSG    *LIBL     KEY0005  05
157300940506PROMSG    *LIBL     KEY0006  06
157400940506PROMSG    *LIBL     KEY0007  07
157500940506PROMSG    *LIBL     KEY0008  08
157600940506PROMSG    *LIBL     KEY0009  09
157700970716PROMSG    *LIBL     KEY0010  10
157800970716PROMSG    *LIBL     KEY0011  11
157900970716PROMSG    *LIBL     KEY0012  12
158000970716PROMSG    *LIBL     KEY0013  13
158100970716PROMSG    *LIBL     KEY0014  14
158200970716PROMSG    *LIBL     KEY0015  15
158300970716PROMSG    *LIBL     KEY0016  16
158400970716PROMSG    *LIBL     KEY0017  17
158500970716PROMSG    *LIBL     KEY0018  18
158600970716PROMSG    *LIBL     KEY0019  19
158700940506PROMSG    *LIBL     KEY0020  20
158800940506PROMSG    *LIBL     KEY0021  21
158900940506PROMSG    *LIBL     KEY0022  22
159000940506PROMSG    *LIBL     KEY0023  23
159100940506PROMSG    *LIBL     KEY0024  24
159200940506**  ABILITAZIONE S1
1593009410140N  01
1594009407200N  02
1595009407201N  03
1596009407200N  04
1597009407200N  05
1598009407200N  06
1599009407200N  07
1600009407200N  08
1601009407200N  09
1602009407201N  10
1603009407200N  11
1604009407201N  12
1605009407200N  13
1606009407200N  14
1607009407200N  15
1608009407200N  16
1609009407200N  17
1610009407200N  18
1611009407200N  19
1612009407200N  20
1613009407200N  21
1614009407200N  22
1615009505020N  23
1616009407200N  24
