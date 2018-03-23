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
003800980909$001 FANGY4ZD   CF   E             WORKSTN USROPN
003900940607     F                                     SFILE(S1:S1NRR)
004000940201     F                                     INFDS(DSFMT)
004100940714     FANABL01L  UF A E           K DISK
004200940128     D*----------------------------------------------------*
004300940302     D* Schiere per la gestione delle opzioni
004400940302     D*
004500950503    >D $MS1            S             27    DIM(14) CTDATA PERRCD(1)
004600950503    >D $TX1            S             50    DIM(14) ALT($MS1)
004700940728    >D $OP1            S              2  0 DIM(14) CTDATA PERRCD(1)
004800940728    >D $FL1            S              1    DIM(14) ALT($OP1)
004900950503     D $TX             S             50    DIM(50)
005000950503     D $FL             S              1    DIM(50)
005100940506     D* Schiere per la gestione dei tasti di funzione
005200940506     D $FM             S             27    DIM(24)
005300940506     D $FX             S             50    DIM(24)
005400940506     D $FC             S              1    DIM(24)
005500940506     D $FR             S              1    DIM(24)
005600940506     D* Schiere per la gestione dei tasti di funzione
005700940506     D* Video S1
005800940506     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
005900940506     D $FX2            S             50    DIM(24) ALT($FM2)
006000940506     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
006100940506     D $FR2            S              1    DIM(24) ALT($FC2)
006200940211     D* Passaggio Parametri
006300940211     D KPJBA         E DS
006400940325     D*-------------
006500940620     D* DS Esterna autorizzazioni tabelle
006600940620     D XCHATB        E DS                  EXTNAME(XCHKATBDS)
006700940620     D*-------------
006800940620     D* DS  restituzione autorizzazioni
006900940620     D* IND=indice
007000940620     D* GRU=man. di gruppo
007100940620     D* AZI=man. azienda
007200941111     D AUTDS           DS
007300940620     D  MANIND                 1      1
007400940620     D  MANGRU                 2      2
007500940620     D  MANAZI                 3      3
007600940607     D* Richiamo a XATB
007700940607     D XATBDS        E DS
007800941103     D*-------------
007900941103      * DS Società
008000941103     D SOC001        E DS                  EXTNAME(XSOC001DS)
008100960123    >D  SALDOC       E                     EXTFLD(XSCSALDOC)
008200960123    >D  SALDOA       E                     EXTFLD(XSCSALDOA)
008300941103     D*-------------
008400941103      * DS Interna per dati utente
008500941103     D XSOCDS          DS          1000
008600940607     D*
008700940325     D* Parametri in ricezione
008800970708     D XTABDS        E DS
008900940325     D*-------------
009000940325     D* Parametri del PGM di manutenzione STD002R
009100940607    >D D002DS          DS
009200940607     D  OPZ002                 1      2
009300940607     D* 0 = ritorno normale
009400940607     D* 1 = F3
009500940607     D* 2 = F12
009600940607     D  RET002                 3      3
009700940607     D* operazione eseguite dal pgm chiamato :
009800940607     D* 0 = nessuna operazione
009900940607     D* 1 = eseguito aggiornamento
010000940607     D  OPR002                 4      4
010100940607     D* funzione non eseguibile per errore :
010200940607     D* 0 = nessun errore
010300940607     D* 1 = funzione richiamata chiusa in errore
010400940607     D* Aggiungere eventuali altri codici di errore
010500940607     D  ERR002                 5      5
010600940607     D* Azienda
010700940607     D  AZI002                 6      8
010800940607     D* Lingua
010900940607     D  LIN002                 9      9
011000940607     D* Codice scelto
011100940607     D  KEY002                10     24
011200970509     D* Autorizzazione
011300970509     D  AUT002                25     25
011400970509     D* Unita
011500970509     D  UNI002                26     33
011600940211     D*-------------
011700940211     D DSFMT           DS           512
011800940506     D  $TASTO               369    369
011900940211     D  NRG                  370    370
012000940211     D  NCL                  371    371
012100940211     D  SFLNRR               378    379B 0
012200940207     D*-------------
012300940207     D* Nome PGM a video
012400940207     D                 DS
012500940207     D  PROGR                  1     10
012600940207     D  ASTER1                 1      1    INZ('*')
012700940207     D  SIGLA                  2      9
012800940207     D  ASTER2                10     10    INZ('*')
012900940729     D*
013000940729     D*
013100940127     D*-------------
013200940127     D* Reperimento nome PGM
013300940127     D STATUS         SDS           333
013400940127     D  DSPGM            *PROC
013500940607     D* descrizione tabella
013600980909$003 D EXTDS         E DS                  EXTNAME(ANGY4ZDS)
013700940207     D*-------------
013800940211     D* COSTANTI
013900940211     D*-------------
014000940607     D SFLPAG          C                   CONST(10)
014100940728     D $DIMOP          C                   CONST(14)
014200940314     D* dimensione della schiera $MS1
014300940506     D*
014400940506     D* Tasti di funzione
014500940506     D F01             C                   CONST(X'31')
014600940506     D F02             C                   CONST(X'32')
014700940506     D F03             C                   CONST(X'33')
014800940506     D F04             C                   CONST(X'34')
014900940506     D F05             C                   CONST(X'35')
015000940506     D F06             C                   CONST(X'36')
015100940506     D F07             C                   CONST(X'37')
015200940506     D F08             C                   CONST(X'38')
015300940506     D F09             C                   CONST(X'39')
015400940506     D F10             C                   CONST(X'3A')
015500940506     D F11             C                   CONST(X'3B')
015600940506     D F12             C                   CONST(X'3C')
015700940506     D F13             C                   CONST(X'B1')
015800940506     D F14             C                   CONST(X'B2')
015900940506     D F15             C                   CONST(X'B3')
016000940506     D F16             C                   CONST(X'B4')
016100940506     D F17             C                   CONST(X'B5')
016200940506     D F18             C                   CONST(X'B6')
016300940506     D F19             C                   CONST(X'B7')
016400940506     D F20             C                   CONST(X'B8')
016500940506     D F21             C                   CONST(X'B9')
016600940506     D F22             C                   CONST(X'BA')
016700940506     D F23             C                   CONST(X'BB')
016800940506     D F24             C                   CONST(X'BC')
016900940506     D ENTER           C                   CONST(X'F1')
017000940506     D ROLDWN          C                   CONST(X'F4')
017100940506     D ROLLUP          C                   CONST(X'F5')
017200940302     I*
017300940211     I/EJECT
017400940207     I*-------------
017500940607     IS1
017600980909$015 I              S1DES                       §4zDES
017700980909$015 I              S1KCC                       §4zKCC
017800980909$015 I              S1KSC                       §4zKSC
017900980909$015 I              S1VOC                       §4zVOC
018000940207     I*-------------
018100940127     I*
018200940127     C*----------------------------------------------------*
018300940127     C*                MAIN LINE PROGRAM
018400940127     C*----------------------------------------------------*
018500940127     C*
018600940223     C* inizializzazione variabili
018700940223     C                   EXSR      INZVAR
018800940223     C*
018900940223     C     $FINE         DOWEQ     *OFF
019000940131     C     $GEST         CASEQ     'S1'          GESS1
019100940117     C                   END
019200940117     C                   END
019300940325     C*
019400940325     C* fine programma
019500940325     C                   EXSR      ENDPGM
019600940225     C*
019700940325     C/EJECT
019800940325     C************************************************************
019900940325     C* FINE PROGRAMMA
020000940325     C************************************************************
020100940325     C     ENDPGM        BEGSR
020200940325     C*
020300940325     C* passaggio dati al pgm chiamante
020400940715     C                   MOVE      *BLANKS       KPJBU
020500940607     C                   MOVEL     XTABDS        KPJBU
020600940325     C*
020700940325     C* Se pgm Guida non è stato richiamato posso chiudere in LR
020800970708     C     XtaOpz        IFEQ      *BLANK
020900940325     C                   SETON                                            LR
021000960906     C                   RETURN
021100940325     C* altrimenti chiudo in RT
021200940325     C                   ELSE
021300940325     C                   SETON                                            RT
021400960906     C                   RETURN
021500940325     C                   ENDIF
021600940325     C*
021700940325     C                   ENDSR
021800940131     C/EJECT
021900940224     C************************************************************
022000940131     C* RIEMPIMENTO OPZIONI
022100940224     C************************************************************
022200940131     C     RIEOPZ        BEGSR
022300940223     C*
022400940223     C* Input :
022500940223     C* - $TX = schiera con tutti i testi disponibili
022600940223     C* - $FL = schiere con i flag di validità associati ai testi
022700940225     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
022800940223     C*          opzione usato
022900940223     C* - WMXO = variabile contenente il numero massimo di testi
023000940223     C*          opzione usabili
023100940223     C* Output :
023200940223     C* - WOPZ1 = variabile contenente la prima riga opzioni
023300940223     C* - WOPZ2 = variabile contenente la seconda riga opzioni
023400950502     C* - FF23  = flag che indica se c'è bisogno del tasto F23
023500940223     C*
023600970214     C                   CALLB     'X04OPMSG'
023700940223     C* input
023800940223     C                   PARM                    $TX
023900940223     C                   PARM                    $FL
024000940225     C                   PARM                    WCOZ              3 0
024100940223     C                   PARM                    WMXO              3 0
024200940223     C* output
024300940223     C                   PARM                    WOPZ1            76
024400940223     C                   PARM                    WOPZ2            76
024500950502     C                   PARM                    FF23              1
024600940607     C* input
024700940607$018 C                   PARM      37            $L01              3 0
024800940607     C                   PARM      0             $L02              3 0
024900940223     C*
025000940131     C                   ENDSR
025100940506     C/EJECT
025200940506     C************************************************************
025300940506     C* RIEMPIMENTO TASTI FUNZIONE
025400940506     C************************************************************
025500940506     C     RIEKEY        BEGSR
025600940506     C*
025700940506     C* Input :
025800940506     C* - $FX = schiera con tutti i testi disponibili
025900940506     C* - $FC = schiere con i flag di validità associati ai testi
026000940506     C* - $FR = schiere con i flag di REVERSE IMAGE
026100940506     C* - $ULK = ultimo testo caricato
026200940506     C* Output :
026300940506     C* - $ULK
026400940506     c* - $KEY1= 1° riga di tasti funzione
026500940506     c* - $KEY2= 2° riga di tasti funzione
026600940506     C*
026700970214     C                   CALLB     'XKEYMSG'
026800940506     C* input
026900940506     C                   PARM                    $FX
027000940506     C                   PARM                    $FC
027100940506     C                   PARM                    $FR
027200940506     C                   PARM                    $ULK              3 0
027300940506     C* output
027400940506     C                   PARM                    $KEY1            79
027500940506     C                   PARM                    $KEY2            79
027600940607     C* input
027700940607$018 C                   PARM      37            $L01              3 0
027800941004$018 C                   PARM      37            $L02              3 0
027900940506     C*
028000940506     C                   ENDSR
028100940224     C/EJECT
028200940224     C************************************************************
028300940224     C* GESTIONE EMISSIONE TASTI FUNZIONE
028400940224     C************************************************************
028500940224     C     GESPIE        BEGSR
028600940224     C*
028700940224     C                   SELECT
028800940506     C* Lista
028900940506     C     $GEST         WHENEQ    'S1'
029000940506     C     $ULKS1        IFEQ      0
029100940506     C                   EXSR      F24S1
029200940506     C                   ELSE
029300940607     C                   WRITE     Z1
029400940506     C                   ENDIF
029500940506     C                   ENDSL
029600940506     C*
029700940224     C                   ENDSR
029800940506     C/EJECT
029900940506     C************************************************************
030000940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA S1
030100940506     C************************************************************
030200940506     C     CTRKS1        BEGSR
030300940506     C                   SETOFF                                       99
030400940506     C                   MOVEA     $FC2          $FC
030500940506     C                   EXSR      CTRKEY
030600940506     C     *IN99         IFEQ      *ON
030700940506     C                   SETON                                        97
030800940506     C                   ENDIF
030900940607     C*
031000940506     C                   ENDSR
031100940506     C/EJECT
031200940506     C************************************************************
031300940506     C* CONTROLLO ABILITAZIONE TASTI
031400940509     C* INPUT
031500940509     c* $TASTO =Tasto funzione premuto
031600940509     C* $FC = Tasti abilitati
031700940509     C* OUTPUT
031800940509     C* IN99 = ERrore
031900940506     C************************************************************
032000940506     C     CTRKEY        BEGSR
032100940506     C                   SELECT
032200940506     C     $TASTO        WHENEQ    F01
032300940720     C     $FC(1)        IFEQ      '0'
032400940506     C                   SETON                                        99
032500940506     C                   ENDIF
032600940506     C     $TASTO        WHENEQ    F02
032700940720     C     $FC(2)        IFEQ      '0'
032800940506     C                   SETON                                        99
032900940506     C                   ENDIF
033000940506     C     $TASTO        WHENEQ    F03
033100940720     C     $FC(3)        IFEQ      '0'
033200940506     C                   SETON                                        99
033300940506     C                   ENDIF
033400940506     C     $TASTO        WHENEQ    F04
033500940720     C     $FC(4)        IFEQ      '0'
033600940506     C                   SETON                                        99
033700940506     C                   ENDIF
033800940506     C     $TASTO        WHENEQ    F05
033900940720     C     $FC(5)        IFEQ      '0'
034000940506     C                   SETON                                        99
034100940506     C                   ENDIF
034200940506     C     $TASTO        WHENEQ    F06
034300940720     C     $FC(6)        IFEQ      '0'
034400940506     C                   SETON                                        99
034500940506     C                   ENDIF
034600940506     C     $TASTO        WHENEQ    F07
034700940720     C     $FC(7)        IFEQ      '0'
034800940506     C                   SETON                                        99
034900940506     C                   ENDIF
035000940506     C     $TASTO        WHENEQ    F08
035100940720     C     $FC(8)        IFEQ      '0'
035200940506     C                   SETON                                        99
035300940506     C                   ENDIF
035400940506     C     $TASTO        WHENEQ    F09
035500940720     C     $FC(09)       IFEQ      '0'
035600940506     C                   SETON                                        99
035700940506     C                   ENDIF
035800940506     C     $TASTO        WHENEQ    F10
035900940720     C     $FC(10)       IFEQ      '0'
036000940506     C                   SETON                                        99
036100940506     C                   ENDIF
036200940506     C     $TASTO        WHENEQ    F11
036300940720     C     $FC(11)       IFEQ      '0'
036400940506     C                   SETON                                        99
036500940506     C                   ENDIF
036600940506     C     $TASTO        WHENEQ    F12
036700940720     C     $FC(12)       IFEQ      '0'
036800940506     C                   SETON                                        99
036900940506     C                   ENDIF
037000940506     C     $TASTO        WHENEQ    F13
037100940720     C     $FC(13)       IFEQ      '0'
037200940506     C                   SETON                                        99
037300940506     C                   ENDIF
037400940506     C     $TASTO        WHENEQ    F14
037500940720     C     $FC(14)       IFEQ      '0'
037600940506     C                   SETON                                        99
037700940506     C                   ENDIF
037800940506     C     $TASTO        WHENEQ    F15
037900940720     C     $FC(15)       IFEQ      '0'
038000940506     C                   SETON                                        99
038100940506     C                   ENDIF
038200940506     C     $TASTO        WHENEQ    F16
038300940720     C     $FC(16)       IFEQ      '0'
038400940506     C                   SETON                                        99
038500940506     C                   ENDIF
038600940506     C     $TASTO        WHENEQ    F17
038700940720     C     $FC(17)       IFEQ      '0'
038800940506     C                   SETON                                        99
038900940506     C                   ENDIF
039000940506     C     $TASTO        WHENEQ    F18
039100940720     C     $FC(18)       IFEQ      '0'
039200940506     C                   SETON                                        99
039300940506     C                   ENDIF
039400940506     C     $TASTO        WHENEQ    F19
039500940720     C     $FC(19)       IFEQ      '0'
039600940506     C                   SETON                                        99
039700940506     C                   ENDIF
039800940506     C     $TASTO        WHENEQ    F20
039900940720     C     $FC(20)       IFEQ      '0'
040000940506     C                   SETON                                        99
040100940506     C                   ENDIF
040200940506     C     $TASTO        WHENEQ    F21
040300940720     C     $FC(21)       IFEQ      '0'
040400940506     C                   SETON                                        99
040500940506     C                   ENDIF
040600940506     C     $TASTO        WHENEQ    F22
040700940720     C     $FC(22)       IFEQ      '0'
040800940506     C                   SETON                                        99
040900940506     C                   ENDIF
041000940506     C     $TASTO        WHENEQ    F23
041100940720     C     $FC(23)       IFEQ      '0'
041200940506     C                   SETON                                        99
041300940506     C                   ENDIF
041400940506     C     $TASTO        WHENEQ    F24
041500940720     C     $FC(24)       IFEQ      '0'
041600940506     C                   SETON                                        99
041700940506     C                   ENDIF
041800940506     C                   ENDSL
041900940506     C                   ENDSR
042000940224     C/EJECT
042100940127     C************************************************************
042200940131     C* GESTIONE LISTA
042300940127     C************************************************************
042400940127     C     GESS1         BEGSR
042500940223     C* inizializzazione videata
042600940223     C     $INZS1        IFEQ      *ON
042700940127     C                   EXSR      INZS1
042800940223     C                   MOVE      *OFF          $INZS1
042900940127     C                   ENDIF
043000940223     C* emissione piede videata se proveniente da altra
043100940223     C* salvataggio valore formato di provenienza
043200940202     C     $LASTV        IFNE      'S1'
043300940607!!   C*                    WRITEFMTT1
043400940224     C                   EXSR      GESPIE
043500940202     C                   MOVE      $LASTV        $LASTG
043600940202     C                   MOVE      'S1'          $LASTV
043700940202     C                   END
043800940223     C*
043900940223     C     WMAX          IFEQ      0
044000940607     C                   WRITE     D1
044100940211     C                   ENDIF
044200940729     C                   MOVE      '1'           WIN727            1
044300940127     C*
044400940607     C                   EXFMT     C1
044500940204     C     C1NRR         IFNE      0
044600940204     C                   Z-ADD     C1NRR         WSFL
044700940204     C                   ENDIF
044800940127     C                   Z-ADD     SFLNRR        C1RCD
044900940506     C*
045000940506     C* controllo abilitazione tasti funzione
045100940506     C*
045200940506     C                   EXSR      CTRKS1
045300940506     C     *IN99         IFEQ      *OFF
045400940506     C*
045500940127     C*
0456009401271    C                   SELECT
045700940127     C* F3=Fine
045800940506     C     $TASTO        WHENEQ    F03
045900940309     C                   EXSR      F03S1
046000940131     C* F10=Immissione
046100940506     C     $TASTO        WHENEQ    F10
046200940309     C                   EXSR      F10S1
046300940131     C* F12=Ritorno
046400940506     C     $TASTO        WHENEQ    F12
046500940309     C                   EXSR      F12S1
046600940224     C* F23=Altre opzioni
046700940506     C     $TASTO        WHENEQ    F23
046800940309     C                   EXSR      F23S1
046900940224     C* F24=Altri tasti
047000940506     C     $TASTO        WHENEQ    F24
047100940309     C                   EXSR      F24S1
047200940127     C* ROLL-UP
047300940506     C     $TASTO        WHENEQ    ROLLUP
047400940127     C                   EXSR      ROLS1
047500940127     C*
0476009401271O   C                   OTHER
047700940127     C* CONTROLLO DATI
047800940131     C                   EXSR      CTRC1
047900940201     C     *IN99         IFEQ      *OFF
048000940131     C                   EXSR      CTRS1
048100940131     C                   END
0482009401271-   C                   ENDSL
048300940127     C*
048400940506     C                   ENDIF
048500940127     C                   ENDSR
048600940224     C/EJECT
048700940127     C************************************************************
048800940131     C* INIZIALIZZAZIONE LISTA
048900940127     C************************************************************
049000940127     C     INZS1         BEGSR
049100940302     C* pulizia SFL
049200940128     C                   SETOFF                                         3031
049300940607     C                   WRITE     C1
049400940128     C                   SETON                                          31
049500940127     C*
049600940128     C* riempimento opzioni
049700940223     C                   MOVEA     $TX1          $TX
049800940223     C                   MOVEA     $FL1          $FL
049900940309     C                   Z-ADD     0             WCOZ
050000940225     C*  equivale al num.di elementi della schiera $TX1
050100940509     C                   Z-ADD     $DIMOP        WMXO
050200940128     C                   EXSR      RIEOPZ
050300950502     C                   Z-ADD     23            X
050400950502     C                   MOVE      FF23          $FC2(X)
050500940608     C                   MOVEL     WOPZ1         C1OPZ1
050600940225     C                   Z-ADD     WCOZ          C1COZ
050700940128     C*
050800940128     C* CARICAMENTO SFL A PAGINE
050900940128     C*
051000940201     C                   Z-ADD     0             S1NRR
051100940128     C                   Z-ADD     0             WMAX
051200940224     C*
051300940224     C* Posizionamento su file pilota
051400940302     C                   CLEAR                   C1POS
051500940316    >C                   EXSR      SETANA
051600940608    >C                   EXSR      REDANA
051700940302     C* ripristino i valori del posizionamento
051800940302     C                   MOVE      W1POS         C1POS
051900940131     C*
052000940131     C* Carico 1a pagina
052100940127     C                   EXSR      ROLS1
052200940201     C                   Z-ADD     SFLPAG        WPAG
052300940127     C*
052400940127     C                   ENDSR
052500940127     C/EJECT
052600940127     C************************************************************
052700940131     C* CARICAMENTO PAGINA LISTA
052800940127     C************************************************************
052900940127     C     ROLS1         BEGSR
053000940127     C*
053100940128     C                   SETOFF                                       32
053200940223     C                   Z-ADD     0             Y
053300940127     C                   Z-ADD     WMAX          S1NRR
053400940127     C*
053500940127     C* Leggo dal file anagrafico per caricare la lista
053600940127     C*
0537009401311    C     $EFILE        DOWEQ     *OFF
053800940201     C     Y             ANDLT     WPAG
053900940127     C*
054000940131     C                   EXSR      RIES1
054100940127     C*
054200940127     C                   ADD       1             S1NRR
054300940127     C                   ADD       1             Y
054400940714     C                   MOVE      $IN41         *IN41
054500940127     C*
054600940607     C                   WRITE     S1
054700940131     C*
054800940316    >C                   EXSR      REDANA
054900940128     C*
0550009401271-   C                   ENDDO
055100940127     C*
055200940223     C                   Z-ADD     S1NRR         WMAX                 30
055300940127     C*
055400940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
055500940127     C*
055600940201     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
055700940127     C                   MVR                     RESTO             3 0
055800940131     C     PAGINE        MULT      SFLPAG        C1RCD
0559009401271    C     RESTO         IFGT      0
056000940127     C                   ADD       1             C1RCD
0561009401271E   C                   ELSE
056200940407     C                   SUB       SFLPAG        C1RCD
056300940407     C                   ADD       1             C1RCD
0564009401271-   C                   ENDIF
056500940128     C*
056600940127     C                   ENDSR
056700940224     C/EJECT
056800940224     C************************************************************
056900940224     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
057000940224     C************************************************************
057100940607     C     SETANA        BEGSR
057200940224     C*
057300941004     C                   MOVE      *BLANKS       XTBKEY
057400970708     C                   MOVE      XtaAzi        XTBAZI
057500970708     C                   MOVE      XtaLin        XTBLIN
057600940608     C                   MOVE      '9'           XTBRIC
057700940607     C                   MOVE      '0'           XTBALC
057800950622     C                   MOVE      XSCCUT        XTBUTE
057900940607     C                   MOVE      '0'           XTBERR
058000980909     C                   MOVE      *blanks       XTBKEY
058100980909     C                   MOVEl     W1POS         XTBKEY
058200970214     C                   CALLB     'XATB'
058300940607     C                   PARM                    XATBDS
058400940607     C*
058500940607     C     XTBERR        IFNE      '0'
058600940607     C                   MOVEL     *ON           $EFILE
058700940607     C                   ENDIF
058800940224     C*
058900940224     C                   ENDSR
059000940128     C/EJECT
059100940128     C************************************************************
059200940131     C* LETTURA RCD ARCHIVIO PILOTA
059300940128     C************************************************************
059400940607     C     REDANA        BEGSR
059500940128     C*
059600940131     C                   MOVEL     *OFF          $EFILE
059700940131     C                   MOVEL     *OFF          $RCDOK
059800940131     C*
0599009401311    C     $EFILE        DOUEQ     *ON
060000940131     C     $RCDOK        OREQ      *ON
060100940131     C*
060200940607     C                   MOVE      '3'           XTBRIC
060300940607     C                   MOVE      '0'           XTBERR
060400970214     C                   CALLB     'XATB'
060500940607     C                   PARM                    XATBDS
060600940607     C*
060700940607     C     XTBERR        IFNE      '0'
060800940607     C                   MOVEL     *ON           $EFILE
060900940607     C                   ENDIF
061000940131     C*
0611009401312    C     $EFILE        IFEQ      *OFF
061200940607     C                   EXSR      SELANA
061300950502     C                   MOVE      XTBKEY        SAVKEY
061400950502     C                   MOVE      XTBGRU        SAVGRU
061500950502     C                   MOVE      XTBUNI        SAVUNI
061600970509     C                   MOVE      XTBUNT        SAVUNT
0617009401312-   C                   ENDIF
061800940131     C*
0619009401311-   C                   ENDDO
062000940131     C*
062100940607     C                   MOVE      $EFILE        *IN33
062200940131     C*
062300940131     C                   ENDSR
062400940224     C/EJECT
062500940224     C************************************************************
062600940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
062700940224     C************************************************************
062800940224     C     CLCPAG        BEGSR
062900940224     C* Input :
063000940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
063100940224     C*          cursore
063200940224     C* - SFLPAG = numero rcd per pagina sfl
063300940224     C* Output :
063400940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
063500940224     C*
063600940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
063700940224     C                   MVR                     RESTO             3 0
063800940224     C     RESTO         IFGT      0
063900940224     C                   ADD       1             PAGINE
064000940224     C                   ENDIF
064100940224     C     PAGINE        MULT      SFLPAG        WPAG
064200940224     C*
064300940224     C                   ENDSR
064400940131     C/EJECT
064500940131     C************************************************************
064600940131     C* SELEZIONI SULLA RIGA LETTA
064700940314     C* impostare a *OFF il campo $RCDOK per scartare un record che
064800940314     C* non rientra nei parametri di parzializzazione
064900940314     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
065000940314     C* superiore stabilito nei parametri
065100940314     C*
065200940131     C************************************************************
065300940607     C     SELANA        BEGSR
065400940131     C*
065500940202     C                   MOVEL     *ON           $RCDOK
065600940714     C* Se aperto il pgm con opzione 11
065700940714     C* carico solo i record di gruppo
065800970708     C     XtaOpz        IFEQ      '11'
065900940714     C     XTBGRU        ANDNE     'G'
066000940714     C                   MOVEL     *OFF          $RCDOK
066100940714     C                   END
066200940714     C* Carica in alta intensità solo i record
066300940714     C* abilitati
066400940714     C                   MOVE      '0'           $IN41
066500970708     C     XtaOpz        IFEQ      '11'
066600940714     C     XTBGRU        ANDEQ     'G'
066700940715     C     XTBAZB        ANDNE     'N'
066800940714     C                   MOVE      '1'           $IN41
066900940714     C                   END
067000940714     C*
067100940202     C                   ENDSR
067200940131     C/EJECT
067300940131     C************************************************************
067400940131     C* RIEMPIMENTO RIGA LISTA
067500940131     C************************************************************
067600940131     C     RIES1         BEGSR
067700950502     C                   MOVEL     SAVUNI        EXTDS
067800980909     C                   MOVEl     SAVKEY        S1COD
067900950502     C                   MOVE      SAVGRU        S1GRU
068000970512     C                   MOVE      SAVUNT        S1UNITA           8
068100950502     C     *LIKE         DEFINE    XTBUNI        SAVUNI
068200950502     C     *LIKE         DEFINE    XTBKEY        SAVKEY
068300950502     C     *LIKE         DEFINE    XTBGRU        SAVGRU
068400970509     C     *LIKE         DEFINE    XTBUNT        SAVUNT
068500940131     C*
068600940131     C                   ENDSR
068700940309     C/EJECT
068800940309     C************************************************************
068900940309     C* GESTIONE F03 VIDEO S1
069000940309     C************************************************************
069100940309     C     F03S1         BEGSR
069200940309     C*
069300940309     C                   MOVE      *ON           $FINE
069400970708     C                   MOVE      '1'           XtaRet
069500940325     C* fine programma
069600940325     C                   EXSR      ENDPGM
069700940309     C*
069800940309     C                   ENDSR
069900940309     C/EJECT
070000940309     C************************************************************
070100940309     C* GESTIONE F10 VIDEO S1
070200940314     c* AGGIUNTA RECORD
070300940309     C************************************************************
070400940309     C     F10S1         BEGSR
070500940309     C*
070600940607     C                   RESET                   D002DS
070700940607     C                   MOVEL     '01'          OPZ002
070800970708     C                   MOVE      XtaLin        LIN002
070900970708     C                   MOVE      XtaAzi        AZI002
071000970512     C                   MOVE      S1UNITA       UNI002
071100940715     C                   MOVE      *BLANKS       KPJBU
071200940607     C                   MOVEL     D002DS        KPJBU
071300980909$004 C                   CALL      'ANGY4ZR2'
071400940309     C                   PARM                    KPJBA
071500940607     C                   MOVEL     KPJBU         D002DS
071600940309     C* ritorno da PGM gestione
071700940309     C                   EXSR      GESRET
071800940309     C     WINZS1        IFEQ      *ON
071900940309     C                   MOVE      *ON           $INZS1
072000940309     C* se esistevano già righe sul sfl
072100940309     C* calcolo pagina a cui deve posizionarsi
072200940309     C     WSFL          IFGT      0
072300940309     C                   EXSR      CLCPAG
072400940309     C* altrimenti carico solo la 1a pagina
072500940309     C                   ELSE
072600940309     C                   Z-ADD     SFLPAG        WPAG
072700940309     C                   ENDIF
072800940309     C                   ENDIF
072900940309     C*
073000940712     C                   MOVE      *HIVAL        $LASTV
073100940309     C                   ENDSR
073200940309     C/EJECT
073300940309     C************************************************************
073400940309     C* GESTIONE F12 VIDEO S1
073500940309     C************************************************************
073600940309     C     F12S1         BEGSR
073700940309     C*
073800970708     C                   MOVE      '2'           XtaRet
073900940309     C                   MOVE      *ON           $FINE
074000940309     C*
074100940309     C                   ENDSR
074200940309     C/EJECT
074300940309     C************************************************************
074400940309     C* GESTIONE F23 VIDEO S1
074500940309     C************************************************************
074600940309     C     F23S1         BEGSR
074700940309     C*
074800940309     C                   EXSR      RIEOPZ
074900950502     C                   Z-ADD     23            X
075000950502     C                   MOVE      FF23          $FC2(X)
075100940608     C                   MOVEL     WOPZ1         C1OPZ1
075200940309     C                   Z-ADD     WCOZ          C1COZ
075300940309     C*
075400940309     C                   ENDSR
075500940309     C/EJECT
075600940309     C************************************************************
075700940309     C* GESTIONE F24 VIDEO S1
075800940309     C************************************************************
075900940309     C     F24S1         BEGSR
076000940309     C*
076100940506     C*
076200940506     C                   MOVEA     $FX2          $FX
076300940506     C                   MOVEA     $FC2          $FC
076400940506     C                   MOVEA     $FR2          $FR
076500940506     C                   Z-ADD     $ULKS1        $ULK
076600940506     C                   EXSR      RIEKEY
076700941017     C                   MOVEA     $FC           $FC2
076800940506     C                   Z-ADD     $ULK          $ULKS1            3 0
076900940607     C                   MOVEL     $KEY1         Z1KE1
077000941004     C                   MOVEL     $KEY2         Z1KE2
077100940607     C                   WRITE     Z1
077200940506     C*
077300940309     C                   ENDSR
077400940128     C/EJECT
077500940128     C************************************************************
077600940131     C* CONTROLLO TESTATA LISTA
077700940128     C************************************************************
077800940131     C     CTRC1         BEGSR
077900940128     C*
078000940201     C                   MOVE      *OFF          *IN99
078100940131     C*
078200940202     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
078300940202     C* L'INIZIALIZZAZIONE LISTA
078400940202     C*
078500940207     C     C1POS         IFNE      W1POS
078600940202     C                   EXSR      POSS1
078700940202     C                   END
078800940202     C*
078900940202     C                   ENDSR
079000940202     C/EJECT
079100940202     C************************************************************
079200940202     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
079300940202     C************************************************************
079400940202     C     POSS1         BEGSR
079500940202     C*
079600940202     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
079700940202     C*
0798009402021    C     WMAX          IFGT      0
079900940607     C                   READC     S1                                     21
0800009402022    C     *IN21         DOWEQ     *OFF
080100940202     C     *IN99         ANDEQ     *OFF
080200940202     C*
0803009402023    C     S1OPZ         IFNE      0
080400940202     C                   SETON                                        51  99
0805009402023-   C                   ENDIF
080600940202     C* se rilevata opzione attiva, attivo sflnxtchg
0807009402023    C     *IN99         IFEQ      *ON
080800940202     C                   MOVE      *ON           *IN32
080900940714     C                   MOVE      $IN41         *IN41
081000940607     C                   UPDATE    S1
0811009402023E   C                   ELSE
081200940607     C                   READC     S1                                     21
0813009402023-   C                   ENDIF
081400940202     C*
0815009402022-   C                   ENDDO
081600940202     C*
0817009402021-   C                   ENDIF
081800940202     C*
081900940221     C*
0820009402021    C     *IN99         IFEQ      *OFF
082100941121     C                   CLEAR                   W1POS
082200940202     C*
082300940302     C                   MOVEL     C1POS         W1POS
082400940131     C*
082500940202     C                   MOVE      *ON           *IN99
082600940223     C                   MOVE      *ON           $INZS1
082700940202     C*
082800940202     C*
0829009402021-   C                   ENDIF
083000940202     C*
083100940131     C                   ENDSR
083200940131     C/EJECT
083300940131     C************************************************************
083400940131     C* CONTROLLO OPZIONI LISTA
083500940131     C************************************************************
083600940131     C     CTRS1         BEGSR
083700940131     C*
083800940202     C                   MOVEL     *OFF          $ESCI
083900940201     C                   SETOFF                                       99
084000940131     C                   Z-ADD     0             S1OPZ
084100940131     C*
084200940127     C* Leggo il sfl solo se ci sono rcd
0843009401311    C     WMAX          IFGT      0
084400940607     C                   READC     S1                                     21
084500940127     C*
084600940131     C* esce se fine sfl o errore che richiede l'uscita
0847009401312    C     *IN21         DOWEQ     *OFF
084800940131     C     $ESCI         ANDEQ     *OFF
084900940201     C                   Z-ADD     S1NRR         C1RCD
085000940131     C* ctrl su riga
085100940131     C                   EXSR      RECS1
085200940131     C* gestione opzioni
0853009401313    C     S1OPZ         IFNE      0
085400940201     C     *IN99         ANDEQ     *OFF
085500940131     C                   EXSR      OPZS1
0856009401273-   C                   ENDIF
085700940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0858009402013    C     *IN99         IFEQ      *ON
085900940201     C     $ESCI         OREQ      *ON
086000940131     C                   MOVE      *ON           *IN32
086100940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
086200940204     C* la ripristinerò a conclusione del ciclo di READC
086300940223     C                   MOVE      *OFF          $INZS1
0864009402233-   C                   ENDIF
086500940223     C*
086600940131     C                   Z-ADD     0             S1OPZ
086700940223     C*
086800940714     C                   MOVE      $IN41         *IN41
086900940607     C                   UPDATE    S1
087000940223     C*
087100940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0872009401313    C     $ESCI         IFEQ      *OFF
087300940607     C                   READC     S1                                     21
087400940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0875009402014    C     *IN21         IFEQ      *ON
087600940201     C                   MOVE      WINZS1        $INZS1
087700940204     C* calcolo pagina a cui deve posizionarsi
087800940224     C                   EXSR      CLCPAG
0879009402014-   C                   ENDIF
0880009402013-   C                   ENDIF
088100940131     C*
0882009401272-   C                   ENDDO
088300940127     C*
0884009401311-   C                   ENDIF
088500940131     C*
088600940127     C                   ENDSR
088700940127     C/EJECT
088800940127     C************************************************************
088900940131     C* CONTROLLO CAMPI I/O RIGA LISTA
089000940127     C************************************************************
089100940131     C     RECS1         BEGSR
089200940131     C*
089300940201     C* reset indicatori DSPATR
089400940201     C                   MOVE      *ALL'0'       IN4049           10
089500940201     C                   MOVEA     IN4049        *IN(40)
089600940201     C*
089700940131     C* Controllo se Utente Autorizzato alla scelta
089800940202     C     S1OPZ         IFNE      0
089900940131     C                   Z-ADD     1             X
090000940223     C     S1OPZ         LOOKUP    $OP1(X)                                21
090100940211     C     *IN21         IFEQ      *OFF
090200940223     C     $FL1(X)       OREQ      'N'
090300940131     C                   MOVE      *ON           $ESCI
090400940201     C                   SETON                                        504099
090500940131     C                   ENDIF
090600940131     C*
090700940620     c* Controllo autorizzazione alla modifica su record di gruppo
090800940620     C* Reperisco tipo codice (gruppo/azienda)
090900980909     C                   MOVE      *blanks       XTBKEY
091000980909     C                   MOVEl     S1COD         XTBKEY
091100970512     C                   MOVE      S1UNITA       XTBUNT
091200940620     C                   MOVE      '0'           XTBALC
091300940620     C                   MOVE      '1'           XTBRIC
091400970214     C                   CALLB     'XATB'
091500940620     C                   PARM                    XATBDS
091600940620     C* Reperisco autorizzazioni utente
091700940620     C                   CLEAR                   XCHATB
091800941118     C                   MOVE      XSCSOC        XCTSOC
091900941118     C                   MOVE      XSCDUN        XCTUNI
092000940620     C                   MOVEL     KNMUS         XCTPRF
092100940620     C                   MOVE      XTBCOD        XCTCOD
092200940620     C                   CALL      'XCHKATB'
092300940620     C                   PARM                    XCHATB
092400941111     C                   MOVEL     XCTDDS        AUTDS
092500941027     C     XCTRTN        IFEQ      1
092600941027     C                   MOVEL     'N'           MANGRU
092700941027     C                   ENDIF
092800940620     C* Non ammessa manutenzione di gruppo
092900940620   1 C     XTBGRU        IFEQ      'G'
093000940620     C     MANGRU        ANDEQ     'N'
093100940620   2 C     S1OPZ         IFEQ      2
093200940729     C     S1OPZ         OREQ      3
093300940620     C     S1OPZ         OREQ      4
093400940620     C                   SETON                                        5399
093500940620   2 C                   ENDIF
093600940620   1 C                   ENDIF
093700940620     C* Non ammessa manutenzione di azienda
093800940620   1 C     XTBGRU        IFEQ      'A'
093900940620     C     MANAZI        ANDEQ     'N'
094000940620   2 C     S1OPZ         IFEQ      2
094100940729     C     S1OPZ         OREQ      3
094200940620     C     S1OPZ         OREQ      4
094300940620     C                   SETON                                        5499
094400940620   2 C                   ENDIF
094500940620   1 C                   ENDIF
094600940607     C                   ENDIF
094700940131     C                   ENDSR
094800940224     C/EJECT
094900940131     C************************************************************
095000940131     C* GESTIONE OPZIONI LISTA
095100940131     C************************************************************
095200940131     C     OPZS1         BEGSR
095300940201     C*
0954009402251    C                   SELECT
095500940225     C*
095600940225     C* se opz=Selezione passo la chiave al pgm chiamante
095700940225     C     S1OPZ         WHENEQ    1
095800970708     C                   MOVE      S1OPZ         XtaOpz
095900980909     C                   MOVEl     S1COD         XtaKey
096000970708     C                   MOVE      S1UNIta       xtaUni
096100940225     C                   MOVE      *ON           $ESCI
096200940225     C                   MOVE      *ON           $FINE
096300940729     C*
096400940714     C*
096500940714     C* se opz=Abilita scrive su archivio abilitazioni
096600940714     C     S1OPZ         WHENEQ    11
096700941103     C                   MOVE      XSCSOC        ABLAZI
096800940714     C                   MOVE      *ZEROS        ABLKEY
096900980909     C                   MOVEl     S1COD         ABLKEY
097000970512     C                   MOVE      S1UNITA       ABLUNT
097100940714     C                   MOVE      XTBCOD        ABLCOD
097200940714     C                   WRITE     ANABL000                             22
097300940714     C*
097400940714     C* se opz=Disabil.cancella su archivio abilitazioni
097500940714     C     S1OPZ         WHENEQ    12
097600941103     C                   MOVE      XSCSOC        ABLAZI
097700940714     C                   MOVE      *ZEROS        ABLKEY
097800980909     C                   MOVEl     S1COD         ABLKEY
097900970512     C                   MOVE      S1UNITA       ABLUNT
098000940714     C                   MOVE      XTBCOD        ABLCOD
098100940714     C     KABL          DELETE    ANABL000                           22
098200940729     C*
098300940225     C*
098400940225     C* se altre opzioni (o nessuna opzione) richiamo pgm gestione
0985009402251O   C                   OTHER
098600940225     C*
098700940616     C                   RESET                   D002DS
098800940607     C                   MOVEL     S1OPZ         OPZ002
098900980909     C                   MOVE      *blanks       KEY002
099000980909     C                   MOVEl     S1COD         KEY002
099100970708     C                   MOVE      XtaLin        LIN002
099200970708     C                   MOVE      XtaAzi        AZI002
099300940715     C                   MOVE      *BLANKS       KPJBU
099400940607     C                   MOVEL     D002DS        KPJBU
099500980909$004 C                   CALL      'ANGY4ZR2'
099600940607     C                   PARM                    KPJBA
099700940607     C                   MOVEL     KPJBU         D002DS
099800940201     C*
099900940223     C* ritorno da PGM gestione
100000940223     C                   EXSR      GESRET
100100940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
1002009402252    C     *IN99         IFEQ      *ON
100300940223     C                   SETON                                        40
1004009402252-   C                   ENDIF
100500940131     C*
100600940506     C                   MOVE      *HIVAL        $LASTV
1007009402251-   C                   ENDSL
100800940225     C*
100900940131     C                   ENDSR
101000940223     C/EJECT
101100940223     C************************************************************
101200940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
101300940223     C************************************************************
101400940223     C     GESRET        BEGSR
101500940223     C*
101600940223     C* modo di ritorno
101700940223     C*
1018009402231    C                   SELECT
101900940314    >C* << questi modi di utilizzo dei valori di ritorno dal
102000940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
102100940314    >C*    proposte, normalmente sempre valide, ma modificabili
102200940314    >C*    per situazioni particolari >>
102300940223     C* 1 = F3
102400940314    >C     RET002        WHENEQ    '1'
102500940224     C                   MOVE      *ON           $ESCI
102600940223     C                   MOVE      *ON           $FINE
102700940223     C* 2 = F12
102800940314    >C     RET002        WHENEQ    '2'
102900940223     C                   MOVE      *ON           $ESCI
103000940223     C*
1031009402231-   C                   ENDSL
103200940223     C*
103300940223     C* operazione eseguite dal pgm chiamato
103400940223     C*
1035009402231    C                   SELECT
103600940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
103700940314    >C     OPR002        WHENEQ    '1'
103800940223     C                   MOVE      *ON           WINZS1
103900940223     C*
1040009402231-   C                   ENDSL
104100940223     C*
104200940223     C* funzione non eseguibile per errore :
104300940223     C*
1044009402231    C                   SELECT
104500940223     C* 1 = funzione richiamata chiusa in errore
104600940316    >C*  eventualmente gestire altri codici di errore
104700940314    >C     ERR002        WHENEQ    '1'
104800940223     C                   MOVE      *ON           $ESCI
104900940223     C                   SETON                                        5299
105000940223     C*
1051009402231-   C                   ENDSL
105200940223     C*
105300940223     C                   ENDSR
105400940223     C/EJECT
105500940131     C************************************************************
105600940131     C* OPERAZIONI INIZIALI
105700940131     C************************************************************
105800940131     C     *INZSR        BEGSR
105900940131     C*
106000940127     C* Reperimento parametri
106100940127     C*
106200940117     C     *ENTRY        PLIST
106300940117     C                   PARM                    KPJBA
106400940714     C*
106500940223     C* Reperimento opzioni
106600940127     C*
106700940128     C                   EXSR      REPOPZ
106800940506     C*
106900940506     C* Reperimento tasti di funzione
107000940506     C*
107100940506     C                   EXSR      REPKEY
107200940506     C*
107300940607$007 C                   MOVE      'Y4Z'         XTBCOD
107400940127     C*
107500940117     C                   ENDSR
107600940225     C/EJECT
107700941103      ************************************************************
107800941103      * Reperimento dati società
107900941103      ************************************************************
108000941103     C     REPSOC        BEGSR
108100941103      *
108200970214     C                   CALLB     'XSOC'
108300941103     C                   PARM                    TIPXSC            6
108400941103     C                   PARM                    SOCXSC            3
108500941103     C                   PARM                    CDSXSC            9 0
108600941103     C                   PARM                    MODXSC            3
108700941103     C                   PARM      *BLANKS       RTNXSC            1
108800941103     C                   PARM                    XSOCDS
108900941103     C                   PARM                    KPJBA
109000941103      *
109100941103     C                   ENDSR
109200940223     C************************************************************
109300940223     C* INIZIALIZZAZIONE VARIABILI
109400940223     C************************************************************
109500940223     C     INZVAR        BEGSR
109600970926     C*
109700970926     C* Dati da PGM chiamante
109800970926     C*
109900970926     C                   MOVEL     KPJBU         XTABDS
110000951124     C*
110100951124     C* Reperimento codice lingua e codice azienda
110200951124     C*
110300951124     C                   MOVEL     'SOC001'      TIPXSC
110400951124     C                   EXSR      REPSOC
110500951124     C     RTNXSC        IFNE      '1'
110600951124     C                   MOVEL     XSOCDS        SOC001
110700951124     C                   ELSE
110800951124     C                   MOVE      *ON           $FINE
110900951124     C* fine programma
111000951124     C                   EXSR      ENDPGM
111100951124     C                   ENDIF
111200951124     C* Lingua e codice azienda
111300970708     C                   MOVE      XSCLIN        XtaLin
111400970708     C                   MOVE      XSCSOC        XtaAzi
111500940715     C*
111600940715     C                   RESET                   $FC2
111700940715     C*
111800980909$021 C                   CLOSE     ANGY4ZD
111900980909$021 C                   OPEN      ANGY4ZD
112000940506     C*
112100940506     C* Definizione delle opzioni utilizzabili a pgm
112200940506     C*
112300940506     C                   EXSR      DEFOPZ
112400940718     C* Pulizia campi e indicatori
112500940718     C                   CLEAR                   W1POS
112600940718     C                   MOVE      *ALL'0'       IN4049           10
112700940718     C                   MOVEA     IN4049        *IN(40)
112800940718     C                   CLEAR                   S1OPZ
112900940223     C* Variabili per gestione videate
113000940223     C*
113100940223     C                   MOVE      *OFF          $FINE
113200940223     C                   MOVE      *OFF          $INZS1
113300940223     C                   MOVE      *OFF          $EFILE
113400940223     C                   MOVE      *OFF          $ESCI
113500940223     C                   MOVE      *OFF          $RCDOK
113600940509     C                   MOVE      *HIVAL        $LASTV
113700940506     C                   Z-ADD     0             $ULKS1            3 0
113800940223     C*
113900940318     C                   MOVE      *ON           $INZS1
114000940223     C                   MOVE      'S1'          $GEST
114100940223     C*
114200940223     C* Variabili appoggio
114300940223     C*
114400940223     C                   Z-ADD     SFLPAG        WPAG
114500940223     C*
114600940608     c* posizione window
114700940608$012 C                   Z-ADD     39            PCOL              2 0
114800940613     C                   Z-ADD     09            R
114900940608     C     80            SUB       PCOL          C
115000940608     C                   SUB       3             C
115100970708     C     XtaOpz        IFNE      '  '
115200970708     C     XtaRig        IFGT      11
115300940608     C                   Z-ADD     1             R
115400940608     C                   ENDIF
115500940608     C*
115600970708     C     XtaCol        IFGT      40
115700940608     C                   Z-ADD     2             C
115800940608     C                   ENDIF
115900940608     C                   ENDIF
116000940616     C*
116100970708     C     XtaOpz        IFNE      '01'
116200940616$019 C                   DO        00            I                 3 0
116300940721$007 C                   MOVE      'Y4Z'         TBTCOD            3
116400940616     C                   ENDDO
116500940616     C                   ENDIF
116600940608     C*
116700940722     c*
116800940722     c* Se richiamato con OPZ 11  Non controlla abilitazione all'aziend
116900970708     C     XtaOpz        IFEQ      '11'
117000940722     C                   MOVE      'N'           XTBGAB
117100940722     C                   ELSE
117200940722     C                   MOVE      'S'           XTBGAB
117300940722     C                   ENDIF
117400940223     C                   ENDSR
117500940223     C/EJECT
117600940131     C************************************************************
117700940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
117800940131     C************************************************************
117900940225     C     DEFOPZ        BEGSR
118000940131     C*
118100940225     C* Questa routine deve valorizzare la schiera $FL1
118200940225     C* con S o N a seconda della utilizzabilità o meno
118300940225     C* dell'opzione correlata
118400940225     C*
118500940714     C* Inizialmente si considerano tutte le opzioni come non valide
118600940728     C     1             DO        14            X
118700940714     C                   MOVE      'N'           $FL1(X)
118800940225     C                   END
118900940225     C*
119000940225     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
119100940714     C* rendo valide alcune opzioni
119200940714     C* i tasti invece li disattivo
119300940225     C                   SELECT
119400940714     C*
119500940714     C* Se pgm richiamato in RICERCA attivo :
119600940714     C*
119700970708    >C     XtaOpz        WHENEQ    '01'
119800940714     C* F3=Fine
119900940714     C                   Z-ADD     3             X
120000940720     C                   MOVE      '0'           $FC2(X)
120100940714     C* F10=Immissione
120200940714     C                   Z-ADD     10            X
120300940919     C                   MOVE      '1'           $FC2(X)
120400940714     C* 1=Selezione
120500940714     C                   Z-ADD     1             X
120600940714     C     01            LOOKUP    $OP1(X)                                21
120700940714     C     *IN21         IFEQ      *ON
120800940714     C                   MOVE      'S'           $FL1(X)
120900940714     C                   ENDIF
121000940729     C* 2=Modifica
121100940729     C                   Z-ADD     1             X
121200940729     C     02            LOOKUP    $OP1(X)                                21
121300940729     C     *IN21         IFEQ      *ON
121400940729     C                   MOVE      'S'           $FL1(X)
121500940729     C                   ENDIF
121600940729     C* 3=Copia
121700940729     C                   Z-ADD     1             X
121800940729     C     03            LOOKUP    $OP1(X)                                21
121900940729     C     *IN21         IFEQ      *ON
122000940729     C                   MOVE      'S'           $FL1(X)
122100940729     C                   ENDIF
122200940729     C* 4=Annullamento
122300940729     C                   Z-ADD     1             X
122400940729     C     04            LOOKUP    $OP1(X)                                21
122500940729     C     *IN21         IFEQ      *ON
122600940729     C                   MOVE      'S'           $FL1(X)
122700940729     C                   ENDIF
122800940714     C* 5=Visualizzazione
122900940714     C                   Z-ADD     1             X
123000940714     C     05            LOOKUP    $OP1(X)                                21
123100940714     C     *IN21         IFEQ      *ON
123200940714     C                   MOVE      'S'           $FL1(X)
123300940714     C                   ENDIF
123400940714     C*
123500940714     C* Se pgm richiamato in INTERROGAZIONE attivo :
123600940714     C*
123700970708    >C     XtaOpz        WHENEQ    '05'
123800940714     C* F10=Immissione
123900940714     C                   Z-ADD     10            X
124000940720     C                   MOVE      '0'           $FC2(X)
124100940714     C* F3=Fine
124200940714     C                   Z-ADD     3             X
124300940720     C                   MOVE      '0'           $FC2(X)
124400940714     C* 5=Visualizzazione
124500940225     C                   Z-ADD     1             X
124600940714     C     05            LOOKUP    $OP1(X)                                21
124700940225     C     *IN21         IFEQ      *ON
124800940714     C                   MOVE      'S'           $FL1(X)
124900940225     C                   ENDIF
125000940831     C*
125100940714     C* Se pgm richiamato in ABILITAZIONE attivo :
125200940714     C*
125300970708    >C     XtaOpz        WHENEQ    '11'
125400940714     C* F10=Immissione
125500940714     C                   Z-ADD     10            X
125600940720     C                   MOVE      '0'           $FC2(X)
125700940714     C* F3=Fine
125800940714     C                   Z-ADD     3             X
125900940720     C                   MOVE      '0'           $FC2(X)
126000940714     C* 5=Visualizzazione
126100940714     C                   Z-ADD     1             X
126200940714     C     05            LOOKUP    $OP1(X)                                21
126300940714     C     *IN21         IFEQ      *ON
126400940714     C                   MOVE      'S'           $FL1(X)
126500940714     C                   ENDIF
126600940714     C* 11=Abilitazione
126700940714     C                   Z-ADD     1             X
126800940714     C     11            LOOKUP    $OP1(X)                                21
126900940714     C     *IN21         IFEQ      *ON
127000940714     C                   MOVE      'S'           $FL1(X)
127100940714     C                   ENDIF
127200940714     C* 12=Disabilita
127300940714     C                   Z-ADD     1             X
127400940714     C     12            LOOKUP    $OP1(X)                                21
127500940714     C     *IN21         IFEQ      *ON
127600940714     C                   MOVE      'S'           $FL1(X)
127700940714     C                   ENDIF
127800940714     C*
127900940714     C* Se pgm richiamato in GESTIONE  attivo :
128000940714     C*
128100970708    >C     XtaOpz        WHENEQ    '02'
128200940714     C* F3=Fine
128300940714     C                   Z-ADD     3             X
128400940720     C                   MOVE      '0'           $FC2(X)
128500940714     C* 2=Modifica
128600940714     C                   Z-ADD     1             X
128700940714     C     02            LOOKUP    $OP1(X)                                21
128800940714     C     *IN21         IFEQ      *ON
128900940714     C                   MOVE      'S'           $FL1(X)
129000940714     C                   ENDIF
129100940714     C* 3=Copia
129200940714     C                   Z-ADD     1             X
129300940714     C     03            LOOKUP    $OP1(X)                                21
129400940714     C     *IN21         IFEQ      *ON
129500940714     C                   MOVE      'S'           $FL1(X)
129600940714     C                   ENDIF
129700940714     C* 4=Annullamento
129800940714     C                   Z-ADD     1             X
129900940714     C     04            LOOKUP    $OP1(X)                                21
130000940714     C     *IN21         IFEQ      *ON
130100940714     C                   MOVE      'S'           $FL1(X)
130200940714     C                   ENDIF
130300940714     C* 5=Visualizzazione
130400940714     C                   Z-ADD     1             X
130500940714     C     05            LOOKUP    $OP1(X)                                21
130600940714     C     *IN21         IFEQ      *ON
130700940714     C                   MOVE      'S'           $FL1(X)
130800940714     C                   ENDIF
130900940225     C                   OTHER
131000940714     C* Se pgm non richiamato, attivo TUTTO  :
131100940714     C* 2=Modifica
131200940714     C                   Z-ADD     1             X
131300940714     C     02            LOOKUP    $OP1(X)                                21
131400940714     C     *IN21         IFEQ      *ON
131500940714     C                   MOVE      'S'           $FL1(X)
131600940714     C                   ENDIF
131700940714     C* 3=Copia
131800940714     C                   Z-ADD     1             X
131900940714     C     03            LOOKUP    $OP1(X)                                21
132000940714     C     *IN21         IFEQ      *ON
132100940714     C                   MOVE      'S'           $FL1(X)
132200940714     C                   ENDIF
132300940714     C* 4=Annullamento
132400940714     C                   Z-ADD     1             X
132500940714     C     04            LOOKUP    $OP1(X)                                21
132600940714     C     *IN21         IFEQ      *ON
132700940714     C                   MOVE      'S'           $FL1(X)
132800940714     C                   ENDIF
132900940714     C* 5=Visualizzazione
133000940714     C                   Z-ADD     1             X
133100940714     C     05            LOOKUP    $OP1(X)                                21
133200940714     C     *IN21         IFEQ      *ON
133300940714     C                   MOVE      'S'           $FL1(X)
133400940714     C                   ENDIF
133500940714     C* 11=Abilitazione
133600940714     C                   Z-ADD     1             X
133700940714     C     11            LOOKUP    $OP1(X)                                21
133800940714     C     *IN21         IFEQ      *ON
133900940714     C                   MOVE      'S'           $FL1(X)
134000940714     C                   ENDIF
134100940714     C* 12=Disabilita
134200940714     C                   Z-ADD     1             X
134300940714     C     12            LOOKUP    $OP1(X)                                21
134400940714     C     *IN21         IFEQ      *ON
134500940714     C                   MOVE      'S'           $FL1(X)
134600940714     C                   ENDIF
134700940225     C                   ENDSL
134800940225     C*
134900940225     C                   ENDSR
135000940225     C/EJECT
135100940225     C************************************************************
135200940225     C* REPERIMENTO OPZIONI
135300940225     C************************************************************
135400940225     C     REPOPZ        BEGSR
135500940225     C*
135600940314     C     1             DO        $DIMOP        X
135700940128     C*
135800940223     C     $MS1(X)       IFNE      *BLANK
135900940128     C*
136000940223     C                   MOVEL     $MS1(X)       IDMSG
136100970214     C                   CALLB     'XRTVM'
136200940201     C                   PARM                    IDMSG            27
136300940201     C                   PARM                    TXTMSG          100
136400940201     C                   PARM                    ERRMSG            1
136500940128     C*
136600940128     C     ERRMSG        IFNE      *ON
136700940223     C                   MOVEL     TXTMSG        $TX1(X)
136800940128     C                   ELSE
136900940223     C                   MOVEL     *ALL'?'       $TX1(X)
137000940128     C                   END
137100940128     C*
137200940128     C                   END
137300940128     C*
137400940128     C                   END
137500940128     C*
137600940128     C                   ENDSR
137700940506     C/EJECT
137800940506     C************************************************************
137900940506     C* REPERIMENTO TASTI DI FUNZIONE
138000940506     C************************************************************
138100940506     C     REPKEY        BEGSR
138200940506     C*
138300940506     C                   MOVEA     $FM2          $FM
138400940506     C                   EXSR      REPKKK
138500940506     C                   MOVEA     $FX           $FX2
138600940506     C                   ENDSR
138700940506     C/EJECT
138800940506     C************************************************************
138900940506     C* REPERIMENTO TASTI DI FUNZIONE
139000940506     C************************************************************
139100940506     C     REPKKK        BEGSR
139200940506     C*
139300940506     C                   CLEAR                   $FX
139400940506     C     1             DO        24            X
139500940506     C*
139600940506     C     $FM(X)        IFNE      *BLANK
139700940506     C*
139800940506     C                   MOVEL     $FM(X)        IDMSG
139900970214     C                   CALLB     'XRTVM'
140000940506     C                   PARM                    IDMSG            27
140100940506     C                   PARM                    TXTMSG          100
140200940506     C                   PARM                    ERRMSG            1
140300940506     C*
140400940506     C     ERRMSG        IFNE      *ON
140500940506     C                   MOVEL     TXTMSG        $FX(X)
140600940506     C                   ELSE
140700940506     C                   MOVEL     *ALL' '       $FX(X)
140800940506     C                   END
140900940506     C*
141000940506     C                   END
141100940506     C*
141200940506     C                   END
141300940506     C*
141400940506     C                   ENDSR
141500940223     C/EJECT
141600940128     C************************************************************
141700940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
141800940128     C************************************************************
141900940223     C     DEFCAM        BEGSR
142000940128     C*
142100940223     C* Variabili per gestione videate
142200940223     C*
142300940223     C                   MOVE      *BLANK        $GEST             2
142400940223     C                   MOVE      *BLANK        $FINE             1
142500940223     C                   MOVE      *BLANK        $INZS1            1
142600940223     C                   MOVE      *BLANK        $LASTG            2
142700940223     C                   MOVE      *BLANK        $LASTV            2
142800940223     C                   MOVE      *BLANK        $EFILE            1
142900940223     C                   MOVE      *BLANK        $ESCI             1
143000940223     C                   MOVE      *BLANK        $RCDOK            1
143100940223     C*
143200940314     C* Variabili appoggio sempre presenti in tutte le anagrafiche
143300940223     C*
143400940223     C     *LIKE         DEFINE    C1RCD         S1NRR
143500940223     C     *LIKE         DEFINE    C1NRR         WSFL
143600940223     C     *LIKE         DEFINE    C1RCD         WMAX
143700940223     C     *LIKE         DEFINE    C1RCD         WPAG
143800940223     C     *LIKE         DEFINE    $INZS1        WINZS1
143900940223     C                   MOVE      *BLANK        WWGRP             3
144000940223     C                   Z-ADD     0             CURR              2 0
144100940223     C                   Z-ADD     0             CURC              2 0
144200940223     C                   MOVE      *BLANK        WIN              99
144300940314     C     *LIKE         DEFINE    C1POS         W1POS
144400940620     C*
144500940714     C     KABL          KLIST
144600940714     C                   KFLD                    ABLCOD
144700940714     C                   KFLD                    ABLKEY
144800940714     C                   KFLD                    ABLAZI
144900940314     C*
145000940223     C* Indici
145100940223     C*
145200940223     C                   Z-ADD     0             X                 3 0
145300940223     C                   Z-ADD     0             Y                 3 0
145400940127     C*
145500940117     C                   ENDSR
145600940223     C/EJECT
145700940616**
145800940225PROMSG    *LIBL     OPZ0001
145900940225PROMSG    *LIBL     OPZ0002
146000940131PROMSG    *LIBL     OPZ0003
146100940131PROMSG    *LIBL     OPZ0004
146200940131PROMSG    *LIBL     OPZ0005
146300940224PROMSG    *LIBL     OPZ0006
146400940224PROMSG    *LIBL     OPZ0007
146500940224PROMSG    *LIBL     OPZ0008
146600940224PROMSG    *LIBL     OPZ0009
146700970716PROMSG    *LIBL     OPZ0010
146800970716PROMSG    *LIBL     OPZ0013
146900970716PROMSG    *LIBL     OPZ0012
147000970716PROMSG    *LIBL     OPZ0011
147100970716PROMSG    *LIBL     OPZ0014
147200940201**
14730094022501S      <Selezione
14740094022502S      <Modifica
14750094022503S      <Copia
14760094022504S      <Annullamento
14770094022505S      <Visualizzazione
14780094060706N      <prova
14790094060707N      <prova
14800094060708N      <prova
14810094060709N      <prova
14820094072910S      <Invia
14830094071411S      <Abilita gruppo
14840094071412S      <Disabilita
14850094072813N      <PROVA
14860094083114N      <Commenti
148700940506** TASTI DI FUNZIONE  S1
148800940506PROMSG    *LIBL     KEY0001  01
148900940506PROMSG    *LIBL     KEY0002  02
149000940506PROMSG    *LIBL     KEY0003  03
149100940506PROMSG    *LIBL     KEY0004  04
149200940506PROMSG    *LIBL     KEY0005  05
149300940506PROMSG    *LIBL     KEY0006  06
149400940506PROMSG    *LIBL     KEY0007  07
149500940506PROMSG    *LIBL     KEY0008  08
149600940506PROMSG    *LIBL     KEY0009  09
149700970716PROMSG    *LIBL     KEY0010  10
149800970716PROMSG    *LIBL     KEY0011  11
149900970716PROMSG    *LIBL     KEY0012  12
150000970716PROMSG    *LIBL     KEY0013  13
150100970716PROMSG    *LIBL     KEY0014  14
150200970716PROMSG    *LIBL     KEY0015  15
150300970716PROMSG    *LIBL     KEY0016  16
150400970716PROMSG    *LIBL     KEY0017  17
150500970716PROMSG    *LIBL     KEY0018  18
150600970716PROMSG    *LIBL     KEY0019  19
150700940506PROMSG    *LIBL     KEY0020  20
150800940506PROMSG    *LIBL     KEY0021  21
150900940506PROMSG    *LIBL     KEY0022  22
151000940506PROMSG    *LIBL     KEY0023  23
151100940506PROMSG    *LIBL     KEY0024  24
151200940506**  ABILITAZIONE S1
1513009410140N  01
1514009407200N  02
1515009407201N  03
1516009407200N  04
1517009407200N  05
1518009407200N  06
1519009407200N  07
1520009407200N  08
1521009407200N  09
1522009407201N  10
1523009407200N  11
1524009407201N  12
1525009407200N  13
1526009407200N  14
1527009407200N  15
1528009407200N  16
1529009407200N  17
1530009407200N  18
1531009407200N  19
1532009407200N  20
1533009407200N  21
1534009407200N  22
1535009505020N  23
1536009407200N  24
