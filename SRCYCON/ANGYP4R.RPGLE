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
003800990215$001 FANGYP4D   CF   E             WORKSTN USROPN
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
006200940616     D* campi in lingua
006300940616$010 D*LIN             S              6    DIM(0011) CTDATA PERRCD(001)
006400940616$010 D*LIND            S             50    DIM(0011) ALT(LIN)
006500940616$010 D*LPOS            S              3  0 DIM(0011) CTDATA PERRCD(001)
006600940616$010 D*LLUN            S              3  0 DIM(0011) ALT(LPOS)
006700940211     D* Passaggio Parametri
006800940211     D KPJBA         E DS
006900940325     D*-------------
007000940620     D* DS Esterna autorizzazioni tabelle
007100940620     D XCHATB        E DS                  EXTNAME(XCHKATBDS)
007200940620     D*-------------
007300940620     D* DS  restituzione autorizzazioni
007400940620     D* IND=indice
007500940620     D* GRU=man. di gruppo
007600940620     D* AZI=man. azienda
007700941111     D AUTDS           DS
007800940620     D  MANIND                 1      1
007900940620     D  MANGRU                 2      2
008000940620     D  MANAZI                 3      3
008100940607     D* Richiamo a XATB
008200940607     D XATBDS        E DS
008300941103     D*-------------
008400941103      * DS Società
008500941103     D SOC001        E DS                  EXTNAME(XSOC001DS)
008600960123    >D  SALDOC       E                     EXTFLD(XSCSALDOC)
008700960123    >D  SALDOA       E                     EXTFLD(XSCSALDOA)
008800941103     D*-------------
008900941103      * DS Interna per dati utente
009000941103     D XSOCDS          DS          1000
009100940607     D*
009200940325     D* Parametri in ricezione
009300970708     D XTABDS        E DS
009400940325     D*-------------
009500940325     D* Parametri del PGM di manutenzione STD002R
009600940607    >D D002DS          DS
009700940607     D  OPZ002                 1      2
009800940607     D* 0 = ritorno normale
009900940607     D* 1 = F3
010000940607     D* 2 = F12
010100940607     D  RET002                 3      3
010200940607     D* operazione eseguite dal pgm chiamato :
010300940607     D* 0 = nessuna operazione
010400940607     D* 1 = eseguito aggiornamento
010500940607     D  OPR002                 4      4
010600940607     D* funzione non eseguibile per errore :
010700940607     D* 0 = nessun errore
010800940607     D* 1 = funzione richiamata chiusa in errore
010900940607     D* Aggiungere eventuali altri codici di errore
011000940607     D  ERR002                 5      5
011100940607     D* Azienda
011200940607     D  AZI002                 6      8
011300940607     D* Lingua
011400940607     D  LIN002                 9      9
011500940607     D* Codice scelto
011600940607     D  KEY002                10     24
011700970509     D* Autorizzazione
011800970509     D  AUT002                25     25
011900970509     D* Unita
012000970509     D  UNI002                26     33
012100940211     D*-------------
012200940211     D DSFMT           DS           512
012300940506     D  $TASTO               369    369
012400940211     D  NRG                  370    370
012500940211     D  NCL                  371    371
012600940211     D  SFLNRR               378    379B 0
012700940207     D*-------------
012800940207     D* Nome PGM a video
012900940207     D                 DS
013000940207     D  PROGR                  1     10
013100940207     D  ASTER1                 1      1    INZ('*')
013200940207     D  SIGLA                  2      9
013300940207     D  ASTER2                10     10    INZ('*')
013400940729     D*
013500940729     D*
013600940127     D*-------------
013700940127     D* Reperimento nome PGM
013800940127     D STATUS         SDS           333
013900940127     D  DSPGM            *PROC
014000940607     D* descrizione tabella
014100990215$003 D EXTDS         E DS                  EXTNAME(ANGYP4DS)
014200940207     D*-------------
014300940211     D* COSTANTI
014400940211     D*-------------
014500940607     D SFLPAG          C                   CONST(10)
014600940728     D $DIMOP          C                   CONST(14)
014700940314     D* dimensione della schiera $MS1
014800940506     D*
014900940506     D* Tasti di funzione
015000940506     D F01             C                   CONST(X'31')
015100940506     D F02             C                   CONST(X'32')
015200940506     D F03             C                   CONST(X'33')
015300940506     D F04             C                   CONST(X'34')
015400940506     D F05             C                   CONST(X'35')
015500940506     D F06             C                   CONST(X'36')
015600940506     D F07             C                   CONST(X'37')
015700940506     D F08             C                   CONST(X'38')
015800940506     D F09             C                   CONST(X'39')
015900940506     D F10             C                   CONST(X'3A')
016000940506     D F11             C                   CONST(X'3B')
016100940506     D F12             C                   CONST(X'3C')
016200940506     D F13             C                   CONST(X'B1')
016300940506     D F14             C                   CONST(X'B2')
016400940506     D F15             C                   CONST(X'B3')
016500940506     D F16             C                   CONST(X'B4')
016600940506     D F17             C                   CONST(X'B5')
016700940506     D F18             C                   CONST(X'B6')
016800940506     D F19             C                   CONST(X'B7')
016900940506     D F20             C                   CONST(X'B8')
017000940506     D F21             C                   CONST(X'B9')
017100940506     D F22             C                   CONST(X'BA')
017200940506     D F23             C                   CONST(X'BB')
017300940506     D F24             C                   CONST(X'BC')
017400940506     D ENTER           C                   CONST(X'F1')
017500940506     D ROLDWN          C                   CONST(X'F4')
017600940506     D ROLLUP          C                   CONST(X'F5')
017700940302     I*
017800940211     I/EJECT
017900940207     I*-------------
018000940607     IS1
018100990215$015 I              S1KCD                       §P4KCD
018200990215$015 I              S1KSD                       §P4KSD
018300990215$015 I              S1KCA                       §P4KCA
018400990215$015 I              S1KSA                       §P4KSA
018500990215$015 I              S1CDD                       §P4CDD
018600990215$015 I              S1CDA                       §P4CDA
018700990215$015 I              S1CAD                       §P4CAD
018800990215$015 I              S1CAA                       §P4CAA
018900990215$015 I              S1TDD                       §P4TDD
019000990215$015 I              S1TDA                       §P4TDA
019100940207     I*-------------
019200940127     I*
019300940127     C*----------------------------------------------------*
019400940127     C*                MAIN LINE PROGRAM
019500940127     C*----------------------------------------------------*
019600940127     C*
019700940223     C* inizializzazione variabili
019800940223     C                   EXSR      INZVAR
019900940223     C*
020000940223     C     $FINE         DOWEQ     *OFF
020100940131     C     $GEST         CASEQ     'S1'          GESS1
020200940117     C                   END
020300940117     C                   END
020400990215     C*
020500940325     C* fine programma
020600940325     C                   EXSR      ENDPGM
020700940225     C*
020800940325     C/EJECT
020900940325     C************************************************************
021000940325     C* FINE PROGRAMMA
021100940325     C************************************************************
021200940325     C     ENDPGM        BEGSR
021300940325     C*
021400940325     C* passaggio dati al pgm chiamante
021500940715     C                   MOVE      *BLANKS       KPJBU
021600940607     C                   MOVEL     XTABDS        KPJBU
021700940325     C*
021800940325     C* Se pgm Guida non è stato richiamato posso chiudere in LR
021900970708     C     XtaOpz        IFEQ      *BLANK
022000940325     C                   SETON                                            LR
022100960906     C                   RETURN
022200940325     C* altrimenti chiudo in RT
022300940325     C                   ELSE
022400940325     C                   SETON                                            RT
022500960906     C                   RETURN
022600940325     C                   ENDIF
022700940325     C*
022800940325     C                   ENDSR
022900940131     C/EJECT
023000940224     C************************************************************
023100940131     C* RIEMPIMENTO OPZIONI
023200940224     C************************************************************
023300940131     C     RIEOPZ        BEGSR
023400940223     C*
023500940223     C* Input :
023600940223     C* - $TX = schiera con tutti i testi disponibili
023700940223     C* - $FL = schiere con i flag di validità associati ai testi
023800940225     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
023900940223     C*          opzione usato
024000940223     C* - WMXO = variabile contenente il numero massimo di testi
024100940223     C*          opzione usabili
024200940223     C* Output :
024300940223     C* - WOPZ1 = variabile contenente la prima riga opzioni
024400940223     C* - WOPZ2 = variabile contenente la seconda riga opzioni
024500950502     C* - FF23  = flag che indica se c'è bisogno del tasto F23
024600940223     C*
024700970214     C                   CALLB     'X04OPMSG'
024800940223     C* input
024900940223     C                   PARM                    $TX
025000940223     C                   PARM                    $FL
025100940225     C                   PARM                    WCOZ              3 0
025200940223     C                   PARM                    WMXO              3 0
025300940223     C* output
025400940223     C                   PARM                    WOPZ1            76
025500940223     C                   PARM                    WOPZ2            76
025600950502     C                   PARM                    FF23              1
025700940607     C* input
025800940607$018 C                   PARM      48            $L01              3 0
025900940607     C                   PARM      0             $L02              3 0
026000940223     C*
026100940131     C                   ENDSR
026200940506     C/EJECT
026300940506     C************************************************************
026400940506     C* RIEMPIMENTO TASTI FUNZIONE
026500940506     C************************************************************
026600940506     C     RIEKEY        BEGSR
026700940506     C*
026800940506     C* Input :
026900940506     C* - $FX = schiera con tutti i testi disponibili
027000940506     C* - $FC = schiere con i flag di validità associati ai testi
027100940506     C* - $FR = schiere con i flag di REVERSE IMAGE
027200940506     C* - $ULK = ultimo testo caricato
027300940506     C* Output :
027400940506     C* - $ULK
027500940506     c* - $KEY1= 1° riga di tasti funzione
027600940506     c* - $KEY2= 2° riga di tasti funzione
027700940506     C*
027800970214     C                   CALLB     'XKEYMSG'
027900940506     C* input
028000940506     C                   PARM                    $FX
028100940506     C                   PARM                    $FC
028200940506     C                   PARM                    $FR
028300940506     C                   PARM                    $ULK              3 0
028400940506     C* output
028500940506     C                   PARM                    $KEY1            79
028600940506     C                   PARM                    $KEY2            79
028700940607     C* input
028800940607$018 C                   PARM      48            $L01              3 0
028900941004$018 C                   PARM      48            $L02              3 0
029000940506     C*
029100940506     C                   ENDSR
029200940224     C/EJECT
029300940224     C************************************************************
029400940224     C* GESTIONE EMISSIONE TASTI FUNZIONE
029500940224     C************************************************************
029600940224     C     GESPIE        BEGSR
029700940224     C*
029800940224     C                   SELECT
029900940506     C* Lista
030000940506     C     $GEST         WHENEQ    'S1'
030100940506     C     $ULKS1        IFEQ      0
030200940506     C                   EXSR      F24S1
030300940506     C                   ELSE
030400940607     C                   WRITE     Z1
030500940506     C                   ENDIF
030600940506     C                   ENDSL
030700940506     C*
030800940224     C                   ENDSR
030900940506     C/EJECT
031000940506     C************************************************************
031100940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA S1
031200940506     C************************************************************
031300940506     C     CTRKS1        BEGSR
031400940506     C                   SETOFF                                       99
031500940506     C                   MOVEA     $FC2          $FC
031600940506     C                   EXSR      CTRKEY
031700940506     C     *IN99         IFEQ      *ON
031800940506     C                   SETON                                        97
031900940506     C                   ENDIF
032000940607     C*
032100940506     C                   ENDSR
032200940506     C/EJECT
032300940506     C************************************************************
032400940506     C* CONTROLLO ABILITAZIONE TASTI
032500940509     C* INPUT
032600940509     c* $TASTO =Tasto funzione premuto
032700940509     C* $FC = Tasti abilitati
032800940509     C* OUTPUT
032900940509     C* IN99 = ERrore
033000940506     C************************************************************
033100940506     C     CTRKEY        BEGSR
033200940506     C                   SELECT
033300940506     C     $TASTO        WHENEQ    F01
033400940720     C     $FC(1)        IFEQ      '0'
033500940506     C                   SETON                                        99
033600940506     C                   ENDIF
033700940506     C     $TASTO        WHENEQ    F02
033800940720     C     $FC(2)        IFEQ      '0'
033900940506     C                   SETON                                        99
034000940506     C                   ENDIF
034100940506     C     $TASTO        WHENEQ    F03
034200940720     C     $FC(3)        IFEQ      '0'
034300940506     C                   SETON                                        99
034400940506     C                   ENDIF
034500940506     C     $TASTO        WHENEQ    F04
034600940720     C     $FC(4)        IFEQ      '0'
034700940506     C                   SETON                                        99
034800940506     C                   ENDIF
034900940506     C     $TASTO        WHENEQ    F05
035000940720     C     $FC(5)        IFEQ      '0'
035100940506     C                   SETON                                        99
035200940506     C                   ENDIF
035300940506     C     $TASTO        WHENEQ    F06
035400940720     C     $FC(6)        IFEQ      '0'
035500940506     C                   SETON                                        99
035600940506     C                   ENDIF
035700940506     C     $TASTO        WHENEQ    F07
035800940720     C     $FC(7)        IFEQ      '0'
035900940506     C                   SETON                                        99
036000940506     C                   ENDIF
036100940506     C     $TASTO        WHENEQ    F08
036200940720     C     $FC(8)        IFEQ      '0'
036300940506     C                   SETON                                        99
036400940506     C                   ENDIF
036500940506     C     $TASTO        WHENEQ    F09
036600940720     C     $FC(09)       IFEQ      '0'
036700940506     C                   SETON                                        99
036800940506     C                   ENDIF
036900940506     C     $TASTO        WHENEQ    F10
037000940720     C     $FC(10)       IFEQ      '0'
037100940506     C                   SETON                                        99
037200940506     C                   ENDIF
037300940506     C     $TASTO        WHENEQ    F11
037400940720     C     $FC(11)       IFEQ      '0'
037500940506     C                   SETON                                        99
037600940506     C                   ENDIF
037700940506     C     $TASTO        WHENEQ    F12
037800940720     C     $FC(12)       IFEQ      '0'
037900940506     C                   SETON                                        99
038000940506     C                   ENDIF
038100940506     C     $TASTO        WHENEQ    F13
038200940720     C     $FC(13)       IFEQ      '0'
038300940506     C                   SETON                                        99
038400940506     C                   ENDIF
038500940506     C     $TASTO        WHENEQ    F14
038600940720     C     $FC(14)       IFEQ      '0'
038700940506     C                   SETON                                        99
038800940506     C                   ENDIF
038900940506     C     $TASTO        WHENEQ    F15
039000940720     C     $FC(15)       IFEQ      '0'
039100940506     C                   SETON                                        99
039200940506     C                   ENDIF
039300940506     C     $TASTO        WHENEQ    F16
039400940720     C     $FC(16)       IFEQ      '0'
039500940506     C                   SETON                                        99
039600940506     C                   ENDIF
039700940506     C     $TASTO        WHENEQ    F17
039800940720     C     $FC(17)       IFEQ      '0'
039900940506     C                   SETON                                        99
040000940506     C                   ENDIF
040100940506     C     $TASTO        WHENEQ    F18
040200940720     C     $FC(18)       IFEQ      '0'
040300940506     C                   SETON                                        99
040400940506     C                   ENDIF
040500940506     C     $TASTO        WHENEQ    F19
040600940720     C     $FC(19)       IFEQ      '0'
040700940506     C                   SETON                                        99
040800940506     C                   ENDIF
040900940506     C     $TASTO        WHENEQ    F20
041000940720     C     $FC(20)       IFEQ      '0'
041100940506     C                   SETON                                        99
041200940506     C                   ENDIF
041300940506     C     $TASTO        WHENEQ    F21
041400940720     C     $FC(21)       IFEQ      '0'
041500940506     C                   SETON                                        99
041600940506     C                   ENDIF
041700940506     C     $TASTO        WHENEQ    F22
041800940720     C     $FC(22)       IFEQ      '0'
041900940506     C                   SETON                                        99
042000940506     C                   ENDIF
042100940506     C     $TASTO        WHENEQ    F23
042200940720     C     $FC(23)       IFEQ      '0'
042300940506     C                   SETON                                        99
042400940506     C                   ENDIF
042500940506     C     $TASTO        WHENEQ    F24
042600940720     C     $FC(24)       IFEQ      '0'
042700940506     C                   SETON                                        99
042800940506     C                   ENDIF
042900940506     C                   ENDSL
043000940506     C                   ENDSR
043100940224     C/EJECT
043200940127     C************************************************************
043300940131     C* GESTIONE LISTA
043400940127     C************************************************************
043500940127     C     GESS1         BEGSR
043600940223     C* inizializzazione videata
043700940223     C     $INZS1        IFEQ      *ON
043800940127     C                   EXSR      INZS1
043900940223     C                   MOVE      *OFF          $INZS1
044000940127     C                   ENDIF
044100940223     C* emissione piede videata se proveniente da altra
044200940223     C* salvataggio valore formato di provenienza
044300940202     C     $LASTV        IFNE      'S1'
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
050500940127     C*
050600940128     C* riempimento opzioni
050700940223     C                   MOVEA     $TX1          $TX
050800940223     C                   MOVEA     $FL1          $FL
050900940309     C                   Z-ADD     0             WCOZ
051000940225     C*  equivale al num.di elementi della schiera $TX1
051100940509     C                   Z-ADD     $DIMOP        WMXO
051200940128     C                   EXSR      RIEOPZ
051300950502     C                   Z-ADD     23            X
051400950502     C                   MOVE      FF23          $FC2(X)
051500940608     C                   MOVEL     WOPZ1         C1OPZ1
051600940608     C**                   MOVELWOPZ2     C1OPZ2
051700940225     C                   Z-ADD     WCOZ          C1COZ
051800940128     C*
051900940128     C* CARICAMENTO SFL A PAGINE
052000940128     C*
052100940201     C                   Z-ADD     0             S1NRR
052200940128     C                   Z-ADD     0             WMAX
052300940224     C*
052400940224     C* Posizionamento su file pilota
052500940302     C                   CLEAR                   C1POS
052600940316    >C                   EXSR      SETANA
052700940608    >C                   EXSR      REDANA
052800940302     C* ripristino i valori del posizionamento
052900940302     C                   MOVE      W1POS         C1POS
053000940131     C*
053100940131     C* Carico 1a pagina
053200940127     C                   EXSR      ROLS1
053300940201     C                   Z-ADD     SFLPAG        WPAG
053400940127     C*
053500940127     C                   ENDSR
053600940127     C/EJECT
053700940127     C************************************************************
053800940131     C* CARICAMENTO PAGINA LISTA
053900940127     C************************************************************
054000940127     C     ROLS1         BEGSR
054100940127     C*
054200940128     C                   SETOFF                                       32
054300940223     C                   Z-ADD     0             Y
054400940127     C                   Z-ADD     WMAX          S1NRR
054500940127     C*
054600940127     C* Leggo dal file anagrafico per caricare la lista
054700940127     C*
0548009401311    C     $EFILE        DOWEQ     *OFF
054900940201     C     Y             ANDLT     WPAG
055000940127     C*
055100940131     C                   EXSR      RIES1
055200940127     C*
055300940127     C                   ADD       1             S1NRR
055400940127     C                   ADD       1             Y
055500940714     C                   MOVE      $IN41         *IN41
055600940127     C*
055700940607     C                   WRITE     S1
055800940131     C*
055900940316    >C                   EXSR      REDANA
056000940128     C*
0561009401271-   C                   ENDDO
056200940127     C*
056300940223     C                   Z-ADD     S1NRR         WMAX                 30
056400940127     C*
056500940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
056600940127     C*
056700940201     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
056800940127     C                   MVR                     RESTO             3 0
056900940131     C     PAGINE        MULT      SFLPAG        C1RCD
0570009401271    C     RESTO         IFGT      0
057100940127     C                   ADD       1             C1RCD
0572009401271E   C                   ELSE
057300940407     C                   SUB       SFLPAG        C1RCD
057400940407     C                   ADD       1             C1RCD
0575009401271-   C                   ENDIF
057600940128     C*
057700940127     C                   ENDSR
057800940224     C/EJECT
057900940224     C************************************************************
058000940224     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
058100940224     C************************************************************
058200940607     C     SETANA        BEGSR
058300940224     C*
058400941004     C                   MOVE      *BLANKS       XTBKEY
058500970708     C                   MOVE      XtaAzi        XTBAZI
058600970708     C                   MOVE      XtaLin        XTBLIN
058700940608     C                   MOVE      '9'           XTBRIC
058800940607     C                   MOVE      '0'           XTBALC
058900950622     C                   MOVE      XSCCUT        XTBUTE
059000940607     C                   MOVE      '0'           XTBERR
059100941121     C**         W1POS     IFNE *BLANK
059200941004     C                   MOVE      *ZEROS        XTBKEY
059300940608     C                   MOVE      W1POS         XTBKEY
059400941121     C**                   END
059500970214     C                   CALLB     'XATB'
059600940607     C                   PARM                    XATBDS
059700940607     C*
059800940607     C     XTBERR        IFNE      '0'
059900940607     C                   MOVEL     *ON           $EFILE
060000940607     C                   ENDIF
060100940224     C*
060200940224     C                   ENDSR
060300940128     C/EJECT
060400940128     C************************************************************
060500940131     C* LETTURA RCD ARCHIVIO PILOTA
060600940128     C************************************************************
060700940607     C     REDANA        BEGSR
060800940128     C*
060900940131     C                   MOVEL     *OFF          $EFILE
061000940131     C                   MOVEL     *OFF          $RCDOK
061100940131     C*
0612009401311    C     $EFILE        DOUEQ     *ON
061300940131     C     $RCDOK        OREQ      *ON
061400940131     C*
061500940607     C                   MOVE      '3'           XTBRIC
061600940607     C                   MOVE      '0'           XTBERR
061700970214     C                   CALLB     'XATB'
061800940607     C                   PARM                    XATBDS
061900940607     C*
062000940607     C     XTBERR        IFNE      '0'
062100940607     C                   MOVEL     *ON           $EFILE
062200940607     C                   ENDIF
062300940131     C*
0624009401312    C     $EFILE        IFEQ      *OFF
062500940607     C                   EXSR      SELANA
062600950502     C                   MOVE      XTBKEY        SAVKEY
062700950502     C                   MOVE      XTBGRU        SAVGRU
062800950502     C                   MOVE      XTBUNI        SAVUNI
062900970509     C                   MOVE      XTBUNT        SAVUNT
0630009401312-   C                   ENDIF
063100940131     C*
0632009401311-   C                   ENDDO
063300940131     C*
063400940607     C                   MOVE      $EFILE        *IN33
063500940131     C*
063600940131     C                   ENDSR
063700940224     C/EJECT
063800940224     C************************************************************
063900940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
064000940224     C************************************************************
064100940224     C     CLCPAG        BEGSR
064200940224     C* Input :
064300940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
064400940224     C*          cursore
064500940224     C* - SFLPAG = numero rcd per pagina sfl
064600940224     C* Output :
064700940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
064800940224     C*
064900940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
065000940224     C                   MVR                     RESTO             3 0
065100940224     C     RESTO         IFGT      0
065200940224     C                   ADD       1             PAGINE
065300940224     C                   ENDIF
065400940224     C     PAGINE        MULT      SFLPAG        WPAG
065500940224     C*
065600940224     C                   ENDSR
065700940131     C/EJECT
065800940131     C************************************************************
065900940131     C* SELEZIONI SULLA RIGA LETTA
066000940314     C* impostare a *OFF il campo $RCDOK per scartare un record che
066100940314     C* non rientra nei parametri di parzializzazione
066200940314     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
066300940314     C* superiore stabilito nei parametri
066400940314     C*
066500940131     C************************************************************
066600940607     C     SELANA        BEGSR
066700940131     C*
066800940202     C                   MOVEL     *ON           $RCDOK
066900940714     C* Se aperto il pgm con opzione 11
067000940714     C* carico solo i record di gruppo
067100970708     C     XtaOpz        IFEQ      '11'
067200940714     C     XTBGRU        ANDNE     'G'
067300940714     C                   MOVEL     *OFF          $RCDOK
067400940714     C                   END
067500940714     C* Carica in alta intensità solo i record
067600940714     C* abilitati
067700940714     C                   MOVE      '0'           $IN41
067800970708     C     XtaOpz        IFEQ      '11'
067900940714     C     XTBGRU        ANDEQ     'G'
068000940715     C     XTBAZB        ANDNE     'N'
068100940714     C                   MOVE      '1'           $IN41
068200940714     C                   END
068300940714     C*
068400940202     C                   ENDSR
068500940131     C/EJECT
068600940131     C************************************************************
068700940131     C* RIEMPIMENTO RIGA LISTA
068800940131     C************************************************************
068900940131     C     RIES1         BEGSR
069000950502     C*                    MOVE XTBKEY    S1COD
069100950502     C*                    MOVE XTBGRU    S1GRU
069200950502     C                   MOVEL     SAVUNI        EXTDS
069300950502     C                   MOVE      SAVKEY        S1COD
069400950502     C                   MOVE      SAVGRU        S1GRU
069500970512     C                   MOVE      SAVUNT        S1UNITA           8
069600950502     C     *LIKE         DEFINE    XTBUNI        SAVUNI
069700950502     C     *LIKE         DEFINE    XTBKEY        SAVKEY
069800950502     C     *LIKE         DEFINE    XTBGRU        SAVGRU
069900970509     C     *LIKE         DEFINE    XTBUNT        SAVUNT
070000940131     C*
070100940131     C                   ENDSR
070200940309     C/EJECT
070300940309     C************************************************************
070400940309     C* GESTIONE F03 VIDEO S1
070500940309     C************************************************************
070600940309     C     F03S1         BEGSR
070700940309     C*
070800940309     C                   MOVE      *ON           $FINE
070900970708     C                   MOVE      '1'           XtaRet
071000940325     C* fine programma
071100940325     C                   EXSR      ENDPGM
071200940309     C*
071300940309     C                   ENDSR
071400940309     C/EJECT
071500940309     C************************************************************
071600940309     C* GESTIONE F10 VIDEO S1
071700940314     c* AGGIUNTA RECORD
071800940309     C************************************************************
071900940309     C     F10S1         BEGSR
072000940309     C*
072100940607     C                   RESET                   D002DS
072200940607     C                   MOVEL     '01'          OPZ002
072300970708     C                   MOVE      XtaLin        LIN002
072400970708     C                   MOVE      XtaAzi        AZI002
072500970512     C                   MOVE      S1UNITA       UNI002
072600940715     C                   MOVE      *BLANKS       KPJBU
072700940607     C                   MOVEL     D002DS        KPJBU
072800990215$004 C                   CALL      'ANGYP4R2'
072900940309     C                   PARM                    KPJBA
073000940607     C                   MOVEL     KPJBU         D002DS
073100940309     C* ritorno da PGM gestione
073200940309     C                   EXSR      GESRET
073300940309     C     WINZS1        IFEQ      *ON
073400940309     C                   MOVE      *ON           $INZS1
073500940309     C* se esistevano già righe sul sfl
073600940309     C* calcolo pagina a cui deve posizionarsi
073700940309     C     WSFL          IFGT      0
073800940309     C                   EXSR      CLCPAG
073900940309     C* altrimenti carico solo la 1a pagina
074000940309     C                   ELSE
074100940309     C                   Z-ADD     SFLPAG        WPAG
074200940309     C                   ENDIF
074300940309     C                   ENDIF
074400940309     C*
074500940712     C                   MOVE      *HIVAL        $LASTV
074600940309     C                   ENDSR
074700940309     C/EJECT
074800940309     C************************************************************
074900940309     C* GESTIONE F12 VIDEO S1
075000940309     C************************************************************
075100940309     C     F12S1         BEGSR
075200940309     C*
075300970708     C                   MOVE      '2'           XtaRet
075400940309     C                   MOVE      *ON           $FINE
075500940309     C*
075600940309     C                   ENDSR
075700940309     C/EJECT
075800940309     C************************************************************
075900940309     C* GESTIONE F23 VIDEO S1
076000940309     C************************************************************
076100940309     C     F23S1         BEGSR
076200940309     C*
076300940309     C                   EXSR      RIEOPZ
076400950502     C                   Z-ADD     23            X
076500950502     C                   MOVE      FF23          $FC2(X)
076600940608     C                   MOVEL     WOPZ1         C1OPZ1
076700940608     C**                   MOVELWOPZ2     C1OPZ2
076800940309     C                   Z-ADD     WCOZ          C1COZ
076900940309     C*
077000940309     C                   ENDSR
077100940309     C/EJECT
077200940309     C************************************************************
077300940309     C* GESTIONE F24 VIDEO S1
077400940309     C************************************************************
077500940309     C     F24S1         BEGSR
077600940309     C*
077700940506     C*
077800940506     C                   MOVEA     $FX2          $FX
077900940506     C                   MOVEA     $FC2          $FC
078000940506     C                   MOVEA     $FR2          $FR
078100940506     C                   Z-ADD     $ULKS1        $ULK
078200940506     C                   EXSR      RIEKEY
078300941017     C                   MOVEA     $FC           $FC2
078400940506     C                   Z-ADD     $ULK          $ULKS1            3 0
078500940607     C                   MOVEL     $KEY1         Z1KE1
078600941004     C                   MOVEL     $KEY2         Z1KE2
078700940607     C                   WRITE     Z1
078800940506     C*
078900940309     C                   ENDSR
079000940128     C/EJECT
079100940128     C************************************************************
079200940131     C* CONTROLLO TESTATA LISTA
079300940128     C************************************************************
079400940131     C     CTRC1         BEGSR
079500940128     C*
079600940201     C                   MOVE      *OFF          *IN99
079700940131     C*
079800940202     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
079900940202     C* L'INIZIALIZZAZIONE LISTA
080000940202     C*
080100940207     C     C1POS         IFNE      W1POS
080200940202     C                   EXSR      POSS1
080300940202     C                   END
080400940202     C*
080500940202     C                   ENDSR
080600940202     C/EJECT
080700940202     C************************************************************
080800940202     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
080900940202     C************************************************************
081000940202     C     POSS1         BEGSR
081100940202     C*
081200940202     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
081300940202     C*
0814009402021    C     WMAX          IFGT      0
081500940607     C                   READC     S1                                     21
0816009402022    C     *IN21         DOWEQ     *OFF
081700940202     C     *IN99         ANDEQ     *OFF
081800940202     C*
0819009402023    C     S1OPZ         IFNE      0
082000940202     C                   SETON                                        51  99
0821009402023-   C                   ENDIF
082200940202     C* se rilevata opzione attiva, attivo sflnxtchg
0823009402023    C     *IN99         IFEQ      *ON
082400940202     C                   MOVE      *ON           *IN32
082500940714     C                   MOVE      $IN41         *IN41
082600940607     C                   UPDATE    S1
0827009402023E   C                   ELSE
082800940607     C                   READC     S1                                     21
0829009402023-   C                   ENDIF
083000940202     C*
0831009402022-   C                   ENDDO
083200940202     C*
0833009402021-   C                   ENDIF
083400940202     C*
083500940221     C*
0836009402021    C     *IN99         IFEQ      *OFF
083700941121     C                   CLEAR                   W1POS
083800940202     C*
083900940302     C                   MOVEL     C1POS         W1POS
084000940131     C*
084100940202     C                   MOVE      *ON           *IN99
084200940223     C                   MOVE      *ON           $INZS1
084300940202     C*
084400940202     C*
0845009402021-   C                   ENDIF
084600940202     C*
084700940131     C                   ENDSR
084800940131     C/EJECT
084900940131     C************************************************************
085000940131     C* CONTROLLO OPZIONI LISTA
085100940131     C************************************************************
085200940131     C     CTRS1         BEGSR
085300940131     C*
085400940202     C                   MOVEL     *OFF          $ESCI
085500940201     C                   SETOFF                                       99
085600940131     C                   Z-ADD     0             S1OPZ
085700940131     C*
085800940127     C* Leggo il sfl solo se ci sono rcd
0859009401311    C     WMAX          IFGT      0
086000940607     C                   READC     S1                                     21
086100940127     C*
086200940131     C* esce se fine sfl o errore che richiede l'uscita
0863009401312    C     *IN21         DOWEQ     *OFF
086400940131     C     $ESCI         ANDEQ     *OFF
086500940201     C                   Z-ADD     S1NRR         C1RCD
086600940131     C* ctrl su riga
086700940131     C                   EXSR      RECS1
086800940131     C* gestione opzioni
0869009401313    C     S1OPZ         IFNE      0
087000940201     C     *IN99         ANDEQ     *OFF
087100940131     C                   EXSR      OPZS1
0872009401273-   C                   ENDIF
087300940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0874009402013    C     *IN99         IFEQ      *ON
087500940201     C     $ESCI         OREQ      *ON
087600940131     C                   MOVE      *ON           *IN32
087700940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
087800940204     C* la ripristinerò a conclusione del ciclo di READC
087900940223     C                   MOVE      *OFF          $INZS1
0880009402233-   C                   ENDIF
088100940223     C*
088200940131     C                   Z-ADD     0             S1OPZ
088300940223     C*
088400940714     C                   MOVE      $IN41         *IN41
088500940607     C                   UPDATE    S1
088600940223     C*
088700940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0888009401313    C     $ESCI         IFEQ      *OFF
088900940607     C                   READC     S1                                     21
089000940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0891009402014    C     *IN21         IFEQ      *ON
089200940201     C                   MOVE      WINZS1        $INZS1
089300940204     C* calcolo pagina a cui deve posizionarsi
089400940224     C                   EXSR      CLCPAG
0895009402014-   C                   ENDIF
0896009402013-   C                   ENDIF
089700940131     C*
0898009401272-   C                   ENDDO
089900940127     C*
0900009401311-   C                   ENDIF
090100940131     C*
090200940127     C                   ENDSR
090300940127     C/EJECT
090400940127     C************************************************************
090500940131     C* CONTROLLO CAMPI I/O RIGA LISTA
090600940127     C************************************************************
090700940131     C     RECS1         BEGSR
090800940131     C*
090900940201     C* reset indicatori DSPATR
091000940201     C                   MOVE      *ALL'0'       IN4049           10
091100940201     C                   MOVEA     IN4049        *IN(40)
091200940201     C*
091300940131     C* Controllo se Utente Autorizzato alla scelta
091400940202     C     S1OPZ         IFNE      0
091500940131     C                   Z-ADD     1             X
091600940223     C     S1OPZ         LOOKUP    $OP1(X)                                21
091700940211     C     *IN21         IFEQ      *OFF
091800940223     C     $FL1(X)       OREQ      'N'
091900940131     C                   MOVE      *ON           $ESCI
092000940201     C                   SETON                                        504099
092100940131     C                   ENDIF
092200940131     C*
092300940620     c* Controllo autorizzazione alla modifica su record di gruppo
092400940620     C* Reperisco tipo codice (gruppo/azienda)
092500940620     C                   MOVE      *ALL'0'       XTBKEY
092600940620     C                   MOVE      S1COD         XTBKEY
092700970512     C                   MOVE      S1UNITA       XTBUNT
092800940620     C                   MOVE      '0'           XTBALC
092900940620     C                   MOVE      '1'           XTBRIC
093000970214     C                   CALLB     'XATB'
093100940620     C                   PARM                    XATBDS
093200940620     C* Reperisco autorizzazioni utente
093300940620     C                   CLEAR                   XCHATB
093400941118     C                   MOVE      XSCSOC        XCTSOC
093500941118     C                   MOVE      XSCDUN        XCTUNI
093600940620     C                   MOVEL     KNMUS         XCTPRF
093700940620     C                   MOVE      XTBCOD        XCTCOD
093800940620     C                   CALL      'XCHKATB'
093900940620     C                   PARM                    XCHATB
094000941111     C                   MOVEL     XCTDDS        AUTDS
094100941027     C     XCTRTN        IFEQ      1
094200941027     C                   MOVEL     'N'           MANGRU
094300941027     C                   ENDIF
094400940620     C* Non ammessa manutenzione di gruppo
094500940620   1 C     XTBGRU        IFEQ      'G'
094600940620     C     MANGRU        ANDEQ     'N'
094700940620   2 C     S1OPZ         IFEQ      2
094800940729     C     S1OPZ         OREQ      3
094900940620     C     S1OPZ         OREQ      4
095000940620     C                   SETON                                        5399
095100940620   2 C                   ENDIF
095200940620   1 C                   ENDIF
095300940620     C* Non ammessa manutenzione di azienda
095400940620   1 C     XTBGRU        IFEQ      'A'
095500940620     C     MANAZI        ANDEQ     'N'
095600940620   2 C     S1OPZ         IFEQ      2
095700940729     C     S1OPZ         OREQ      3
095800940620     C     S1OPZ         OREQ      4
095900940620     C                   SETON                                        5499
096000940620   2 C                   ENDIF
096100940620   1 C                   ENDIF
096200940607     C                   ENDIF
096300940131     C                   ENDSR
096400940224     C/EJECT
096500940131     C************************************************************
096600940131     C* GESTIONE OPZIONI LISTA
096700940131     C************************************************************
096800940131     C     OPZS1         BEGSR
096900940201     C*
0970009402251    C                   SELECT
097100940225     C*
097200940225     C* se opz=Selezione passo la chiave al pgm chiamante
097300940225     C     S1OPZ         WHENEQ    1
097400970708     C                   MOVE      S1OPZ         XtaOpz
097500970708     C                   MOVE      S1COD         XtaKey
097600970708     C                   MOVE      S1UNIta       xtaUni
097700940225     C                   MOVE      *ON           $ESCI
097800940225     C                   MOVE      *ON           $FINE
097900940729     C*
098000940714     C*
098100940714     C* se opz=Abilita scrive su archivio abilitazioni
098200940714     C     S1OPZ         WHENEQ    11
098300941103     C                   MOVE      XSCSOC        ABLAZI
098400940714     C                   MOVE      *ZEROS        ABLKEY
098500940714     C                   MOVE      S1COD         ABLKEY
098600970512     C                   MOVE      S1UNITA       ABLUNT
098700940714     C                   MOVE      XTBCOD        ABLCOD
098800940714     C                   WRITE     ANABL000                             22
098900940714     C*
099000940714     C* se opz=Disabil.cancella su archivio abilitazioni
099100940714     C     S1OPZ         WHENEQ    12
099200941103     C                   MOVE      XSCSOC        ABLAZI
099300940714     C                   MOVE      *ZEROS        ABLKEY
099400940714     C                   MOVE      S1COD         ABLKEY
099500970512     C                   MOVE      S1UNITA       ABLUNT
099600940714     C                   MOVE      XTBCOD        ABLCOD
099700940714     C     KABL          DELETE    ANABL000                           22
099800940729     C*
099900940225     C*
100000940225     C* se altre opzioni (o nessuna opzione) richiamo pgm gestione
1001009402251O   C                   OTHER
100200940225     C*
100300940616     C                   RESET                   D002DS
100400940607     C                   MOVEL     S1OPZ         OPZ002
100500940616     C                   MOVE      *ZERO         KEY002
100600940607     C                   MOVE      S1COD         KEY002
100700970708     C                   MOVE      XtaLin        LIN002
100800970708     C                   MOVE      XtaAzi        AZI002
100900940715     C                   MOVE      *BLANKS       KPJBU
101000940607     C                   MOVEL     D002DS        KPJBU
101100990215$004 C                   CALL      'ANGYP4R2'
101200940607     C                   PARM                    KPJBA
101300940607     C                   MOVEL     KPJBU         D002DS
101400940201     C*
101500940223     C* ritorno da PGM gestione
101600940223     C                   EXSR      GESRET
101700940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
1018009402252    C     *IN99         IFEQ      *ON
101900940223     C                   SETON                                        40
1020009402252-   C                   ENDIF
102100940131     C*
102200940506     C                   MOVE      *HIVAL        $LASTV
1023009402251-   C                   ENDSL
102400940225     C*
102500940131     C                   ENDSR
102600940223     C/EJECT
102700940223     C************************************************************
102800940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
102900940223     C************************************************************
103000940223     C     GESRET        BEGSR
103100940223     C*
103200940223     C* modo di ritorno
103300940223     C*
1034009402231    C                   SELECT
103500940314    >C* << questi modi di utilizzo dei valori di ritorno dal
103600940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
103700940314    >C*    proposte, normalmente sempre valide, ma modificabili
103800940314    >C*    per situazioni particolari >>
103900940223     C* 1 = F3
104000940314    >C     RET002        WHENEQ    '1'
104100940224     C                   MOVE      *ON           $ESCI
104200940223     C                   MOVE      *ON           $FINE
104300940223     C* 2 = F12
104400940314    >C     RET002        WHENEQ    '2'
104500940223     C                   MOVE      *ON           $ESCI
104600940223     C*
1047009402231-   C                   ENDSL
104800940223     C*
104900940223     C* operazione eseguite dal pgm chiamato
105000940223     C*
1051009402231    C                   SELECT
105200940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
105300940314    >C     OPR002        WHENEQ    '1'
105400940223     C                   MOVE      *ON           WINZS1
105500940223     C*
1056009402231-   C                   ENDSL
105700940223     C*
105800940223     C* funzione non eseguibile per errore :
105900940223     C*
1060009402231    C                   SELECT
106100940223     C* 1 = funzione richiamata chiusa in errore
106200940316    >C*  eventualmente gestire altri codici di errore
106300940314    >C     ERR002        WHENEQ    '1'
106400940223     C                   MOVE      *ON           $ESCI
106500940223     C                   SETON                                        5299
106600940223     C*
1067009402231-   C                   ENDSL
106800940223     C*
106900940223     C                   ENDSR
107000940223     C/EJECT
107100940131     C************************************************************
107200940131     C* OPERAZIONI INIZIALI
107300940131     C************************************************************
107400940131     C     *INZSR        BEGSR
107500940131     C*
107600940127     C* Reperimento parametri
107700940127     C*
107800940117     C     *ENTRY        PLIST
107900940117     C                   PARM                    KPJBA
108000940714     C*
108100940714     C* Reperimento Codice utente
108200940714     C*
108300950622     C*          KNMUS     CHAINANUTN01L             21
108400940225     C*
108500940223     C* Reperimento opzioni
108600940127     C*
108700940128     C                   EXSR      REPOPZ
108800940506     C*
108900940506     C* Reperimento tasti di funzione
109000940506     C*
109100940506     C                   EXSR      REPKEY
109200940506     C*
109300990215$007 C                   MOVE      'YP4'         XTBCOD
109400940127     C*
109500940117     C                   ENDSR
109600940225     C/EJECT
109700941103      ************************************************************
109800941103      * Reperimento dati società
109900941103      ************************************************************
110000941103     C     REPSOC        BEGSR
110100941103      *
110200970214     C                   CALLB     'XSOC'
110300941103     C                   PARM                    TIPXSC            6
110400941103     C                   PARM                    SOCXSC            3
110500941103     C                   PARM                    CDSXSC            9 0
110600941103     C                   PARM                    MODXSC            3
110700941103     C                   PARM      *BLANKS       RTNXSC            1
110800941103     C                   PARM                    XSOCDS
110900941103     C                   PARM                    KPJBA
111000941103      *
111100941103     C                   ENDSR
111200940223     C************************************************************
111300940223     C* INIZIALIZZAZIONE VARIABILI
111400940223     C************************************************************
111500940223     C     INZVAR        BEGSR
111600970926     C*
111700970926     C* Dati da PGM chiamante
111800970926     C*
111900970926     C                   MOVEL     KPJBU         XTABDS
112000951124     C*
112100951124     C* Reperimento codice lingua e codice azienda
112200951124     C*
112300951124     C                   MOVEL     'SOC001'      TIPXSC
112400951124     C                   EXSR      REPSOC
112500951124     C     RTNXSC        IFNE      '1'
112600951124     C                   MOVEL     XSOCDS        SOC001
112700951124     C                   ELSE
112800951124     C                   MOVE      *ON           $FINE
112900951124     C* fine programma
113000951124     C                   EXSR      ENDPGM
113100951124     C                   ENDIF
113200951124     C* Lingua e codice azienda
113300970708     C                   MOVE      XSCLIN        XtaLin
113400970708     C                   MOVE      XSCSOC        XtaAzi
113500940715     C*
113600940715     C                   RESET                   $FC2
113700940715     C*
113800990215$021 C                   CLOSE     ANGYP4D
113900990215$021 C                   OPEN      ANGYP4D
114000940506     C*
114100940506     C* Definizione delle opzioni utilizzabili a pgm
114200940506     C*
114300940506     C                   EXSR      DEFOPZ
114400940718     C* Pulizia campi e indicatori
114500940718     C                   CLEAR                   W1POS
114600940718     C                   MOVE      *ALL'0'       IN4049           10
114700940718     C                   MOVEA     IN4049        *IN(40)
114800940718     C                   CLEAR                   S1OPZ
114900940223     C* Variabili per gestione videate
115000940223     C*
115100940223     C                   MOVE      *OFF          $FINE
115200940223     C                   MOVE      *OFF          $INZS1
115300940223     C                   MOVE      *OFF          $EFILE
115400940223     C                   MOVE      *OFF          $ESCI
115500940223     C                   MOVE      *OFF          $RCDOK
115600940509     C                   MOVE      *HIVAL        $LASTV
115700940506     C                   Z-ADD     0             $ULKS1            3 0
115800940223     C*
115900940318     C                   MOVE      *ON           $INZS1
116000940223     C                   MOVE      'S1'          $GEST
116100940223     C*
116200940223     C* Variabili appoggio
116300940223     C*
116400940223     C                   Z-ADD     SFLPAG        WPAG
116500940223     C*
116600940223     C* Valorizzazione campi univoci testate
116700940223     C*
116800940607     C*                    CLEART1
116900940607     C*                    MOVELKNSIF     NOMSIF
117000940607     C*                    MOVELRAGUTE    NOMDIT
117100940607     C*                    MOVELDSPGM     SIGLA
117200940607     C*                    MOVELPROGR     NOMPGM
117300940223     C*
117400940608     c* posizione window
117500940608$012 C                   Z-ADD     50            PCOL              2 0
117600940613     C                   Z-ADD     09            R
117700940608     C     80            SUB       PCOL          C
117800940608     C                   SUB       3             C
117900970708     C     XtaOpz        IFNE      '  '
118000970708     C     XtaRig        IFGT      11
118100940608     C                   Z-ADD     1             R
118200940608     C                   ENDIF
118300940608     C*
118400970708     C     XtaCol        IFGT      40
118500940608     C                   Z-ADD     2             C
118600940608     C                   ENDIF
118700940608     C                   ENDIF
118800940616     C*
118900970708     C     XtaOpz        IFNE      '01'
119000940616$019 C                   DO        00            I                 3 0
119100940721$022 C*                  MOVEL     LIN(I)        TBTFLD
119200940721$022 C*                  CLEAR                   TBTKEY
119300940721$022 C*                  CLEAR                   TBTAZI
119400940721$022 C*                  CLEAR                   TBTLIN
119500940721$007 C                   MOVE      'YP4'         TBTCOD            3
119600940721$022 C*    K5TBT1        CHAIN     ANTBT01L                           21
119700940721$022 C*    *IN21         IFEQ      *ON
119800940721$022 C*                  Z-ADD     LLUN(I)       TBTLUN
119900940721$022 C*                  Z-ADD     LPOS(I)       TBTPOS
120000940721$022 C*                  MOVEL     LIND(I)       TBTDES
120100940721$022 C*                  WRITE     ANTBT000
120200940721$022 C*                  ENDIF
120300940616     C                   ENDDO
120400940616     C                   ENDIF
120500940608     C*
120600940722     c*
120700940722     c* Se richiamato con OPZ 11  Non controlla abilitazione all'aziend
120800970708     C     XtaOpz        IFEQ      '11'
120900940722     C                   MOVE      'N'           XTBGAB
121000940722     C                   ELSE
121100940722     C                   MOVE      'S'           XTBGAB
121200940722     C                   ENDIF
121300940223     C                   ENDSR
121400940223     C/EJECT
121500940131     C************************************************************
121600940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
121700940131     C************************************************************
121800940225     C     DEFOPZ        BEGSR
121900940131     C*
122000940225     C* Questa routine deve valorizzare la schiera $FL1
122100940225     C* con S o N a seconda della utilizzabilità o meno
122200940225     C* dell'opzione correlata
122300940225     C*
122400940714     C* Inizialmente si considerano tutte le opzioni come non valide
122500940728     C     1             DO        14            X
122600940714     C                   MOVE      'N'           $FL1(X)
122700940225     C                   END
122800940225     C*
122900940225     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
123000940714     C* rendo valide alcune opzioni
123100940714     C* i tasti invece li disattivo
123200940225     C                   SELECT
123300940714     C*
123400940714     C* Se pgm richiamato in RICERCA attivo :
123500940714     C*
123600970708    >C     XtaOpz        WHENEQ    '01'
123700940714     C* F3=Fine
123800940714     C                   Z-ADD     3             X
123900940720     C                   MOVE      '0'           $FC2(X)
124000940714     C* F10=Immissione
124100940714     C                   Z-ADD     10            X
124200940919     C                   MOVE      '1'           $FC2(X)
124300940714     C* 1=Selezione
124400940714     C                   Z-ADD     1             X
124500940714     C     01            LOOKUP    $OP1(X)                                21
124600940714     C     *IN21         IFEQ      *ON
124700940714     C                   MOVE      'S'           $FL1(X)
124800940714     C                   ENDIF
124900940729     C* 2=Modifica
125000940729     C                   Z-ADD     1             X
125100940729     C     02            LOOKUP    $OP1(X)                                21
125200940729     C     *IN21         IFEQ      *ON
125300940729     C                   MOVE      'S'           $FL1(X)
125400940729     C                   ENDIF
125500940729     C* 3=Copia
125600940729     C                   Z-ADD     1             X
125700940729     C     03            LOOKUP    $OP1(X)                                21
125800940729     C     *IN21         IFEQ      *ON
125900940729     C                   MOVE      'S'           $FL1(X)
126000940729     C                   ENDIF
126100940729     C* 4=Annullamento
126200940729     C                   Z-ADD     1             X
126300940729     C     04            LOOKUP    $OP1(X)                                21
126400940729     C     *IN21         IFEQ      *ON
126500940729     C                   MOVE      'S'           $FL1(X)
126600940729     C                   ENDIF
126700940714     C* 5=Visualizzazione
126800940714     C                   Z-ADD     1             X
126900940714     C     05            LOOKUP    $OP1(X)                                21
127000940714     C     *IN21         IFEQ      *ON
127100940714     C                   MOVE      'S'           $FL1(X)
127200940714     C                   ENDIF
127300940728     C*14=Commenti
127400940831     C**                   Z-ADD1         X
127500940831     C**         14        LOKUP$OP1,X                   21
127600940831     C**         *IN21     IFEQ *ON
127700940831     C**                   MOVE 'S'       $FL1,X
127800940831     C**                   ENDIF
127900940714     C*
128000940714     C* Se pgm richiamato in INTERROGAZIONE attivo :
128100940714     C*
128200970708    >C     XtaOpz        WHENEQ    '05'
128300940714     C* F10=Immissione
128400940714     C                   Z-ADD     10            X
128500940720     C                   MOVE      '0'           $FC2(X)
128600940714     C* F3=Fine
128700940714     C                   Z-ADD     3             X
128800940720     C                   MOVE      '0'           $FC2(X)
128900940714     C* 5=Visualizzazione
129000940225     C                   Z-ADD     1             X
129100940714     C     05            LOOKUP    $OP1(X)                                21
129200940225     C     *IN21         IFEQ      *ON
129300940714     C                   MOVE      'S'           $FL1(X)
129400940225     C                   ENDIF
129500940728     C*14=Commenti
129600940831     C**                   Z-ADD1         X
129700940831     C**         14        LOKUP$OP1,X                   21
129800940831     C**         *IN21     IFEQ *ON
129900940831     C**                   MOVE 'S'       $FL1,X
130000940831     C**                   ENDIF
130100940831     C*
130200940714     C* Se pgm richiamato in ABILITAZIONE attivo :
130300940714     C*
130400970708    >C     XtaOpz        WHENEQ    '11'
130500940714     C* F10=Immissione
130600940714     C                   Z-ADD     10            X
130700940720     C                   MOVE      '0'           $FC2(X)
130800940714     C* F3=Fine
130900940714     C                   Z-ADD     3             X
131000940720     C                   MOVE      '0'           $FC2(X)
131100940714     C* 5=Visualizzazione
131200940714     C                   Z-ADD     1             X
131300940714     C     05            LOOKUP    $OP1(X)                                21
131400940714     C     *IN21         IFEQ      *ON
131500940714     C                   MOVE      'S'           $FL1(X)
131600940714     C                   ENDIF
131700940714     C* 11=Abilitazione
131800940714     C                   Z-ADD     1             X
131900940714     C     11            LOOKUP    $OP1(X)                                21
132000940714     C     *IN21         IFEQ      *ON
132100940714     C                   MOVE      'S'           $FL1(X)
132200940714     C                   ENDIF
132300940714     C* 12=Disabilita
132400940714     C                   Z-ADD     1             X
132500940714     C     12            LOOKUP    $OP1(X)                                21
132600940714     C     *IN21         IFEQ      *ON
132700940714     C                   MOVE      'S'           $FL1(X)
132800940714     C                   ENDIF
132900940714     C*
133000940714     C* Se pgm richiamato in GESTIONE  attivo :
133100940714     C*
133200970708    >C     XtaOpz        WHENEQ    '02'
133300940714     C* F3=Fine
133400940714     C                   Z-ADD     3             X
133500940720     C                   MOVE      '0'           $FC2(X)
133600940714     C* 2=Modifica
133700940714     C                   Z-ADD     1             X
133800940714     C     02            LOOKUP    $OP1(X)                                21
133900940714     C     *IN21         IFEQ      *ON
134000940714     C                   MOVE      'S'           $FL1(X)
134100940714     C                   ENDIF
134200940714     C* 3=Copia
134300940714     C                   Z-ADD     1             X
134400940714     C     03            LOOKUP    $OP1(X)                                21
134500940714     C     *IN21         IFEQ      *ON
134600940714     C                   MOVE      'S'           $FL1(X)
134700940714     C                   ENDIF
134800940714     C* 4=Annullamento
134900940714     C                   Z-ADD     1             X
135000940714     C     04            LOOKUP    $OP1(X)                                21
135100940714     C     *IN21         IFEQ      *ON
135200940714     C                   MOVE      'S'           $FL1(X)
135300940714     C                   ENDIF
135400940714     C* 5=Visualizzazione
135500940714     C                   Z-ADD     1             X
135600940714     C     05            LOOKUP    $OP1(X)                                21
135700940714     C     *IN21         IFEQ      *ON
135800940714     C                   MOVE      'S'           $FL1(X)
135900940714     C                   ENDIF
136000940729     C*14=Commenti
136100940831     C**                   Z-ADD1         X
136200940831     C**         14        LOKUP$OP1,X                   21
136300940831     C**         *IN21     IFEQ *ON
136400940831     C**                   MOVE 'S'       $FL1,X
136500940831     C**                   ENDIF
136600940225     C*
136700940225     C                   OTHER
136800940714     C* Se pgm non richiamato, attivo TUTTO  :
136900940714     C* 2=Modifica
137000940714     C                   Z-ADD     1             X
137100940714     C     02            LOOKUP    $OP1(X)                                21
137200940714     C     *IN21         IFEQ      *ON
137300940714     C                   MOVE      'S'           $FL1(X)
137400940714     C                   ENDIF
137500940714     C* 3=Copia
137600940714     C                   Z-ADD     1             X
137700940714     C     03            LOOKUP    $OP1(X)                                21
137800940714     C     *IN21         IFEQ      *ON
137900940714     C                   MOVE      'S'           $FL1(X)
138000940714     C                   ENDIF
138100940714     C* 4=Annullamento
138200940714     C                   Z-ADD     1             X
138300940714     C     04            LOOKUP    $OP1(X)                                21
138400940714     C     *IN21         IFEQ      *ON
138500940714     C                   MOVE      'S'           $FL1(X)
138600940714     C                   ENDIF
138700940714     C* 5=Visualizzazione
138800940714     C                   Z-ADD     1             X
138900940714     C     05            LOOKUP    $OP1(X)                                21
139000940714     C     *IN21         IFEQ      *ON
139100940714     C                   MOVE      'S'           $FL1(X)
139200940714     C                   ENDIF
139300940714     C* 11=Abilitazione
139400940714     C                   Z-ADD     1             X
139500940714     C     11            LOOKUP    $OP1(X)                                21
139600940714     C     *IN21         IFEQ      *ON
139700940714     C                   MOVE      'S'           $FL1(X)
139800940714     C                   ENDIF
139900940714     C* 12=Disabilita
140000940714     C                   Z-ADD     1             X
140100940714     C     12            LOOKUP    $OP1(X)                                21
140200940714     C     *IN21         IFEQ      *ON
140300940714     C                   MOVE      'S'           $FL1(X)
140400940714     C                   ENDIF
140500940728     C*14=Commenti
140600940831     C**                   Z-ADD1         X
140700940831     C**         14        LOKUP$OP1,X                   21
140800940831     C**         *IN21     IFEQ *ON
140900940831     C**                   MOVE 'S'       $FL1,X
141000940831     C**                   ENDIF
141100940225     C* poi, disattivo a seconda delle autorizzazioni :
141200940314    >C*********  AUTXXX    IFEQ 'N'
141300940314    >C*********            Z-ADD1         X
141400940314    >C*********  'XX'      LOKUP$OP1,X                   21
141500940314    >C*********  *IN21     IFEQ *ON
141600940314    >C*********            MOVE 'N'       $FL1,X
141700940314    >C*********            ENDIF
141800940314    >C*********            ENDIF
141900940225     C*
142000940225     C                   ENDSL
142100940225     C*
142200940225     C                   ENDSR
142300940225     C/EJECT
142400940225     C************************************************************
142500940225     C* REPERIMENTO OPZIONI
142600940225     C************************************************************
142700940225     C     REPOPZ        BEGSR
142800940225     C*
142900940314     C     1             DO        $DIMOP        X
143000940128     C*
143100940223     C     $MS1(X)       IFNE      *BLANK
143200940128     C*
143300940223     C                   MOVEL     $MS1(X)       IDMSG
143400970214     C                   CALLB     'XRTVM'
143500940201     C                   PARM                    IDMSG            27
143600940201     C                   PARM                    TXTMSG          100
143700940201     C                   PARM                    ERRMSG            1
143800940128     C*
143900940128     C     ERRMSG        IFNE      *ON
144000940223     C                   MOVEL     TXTMSG        $TX1(X)
144100940128     C                   ELSE
144200940223     C                   MOVEL     *ALL'?'       $TX1(X)
144300940128     C                   END
144400940128     C*
144500940128     C                   END
144600940128     C*
144700940128     C                   END
144800940128     C*
144900940128     C                   ENDSR
145000940506     C/EJECT
145100940506     C************************************************************
145200940506     C* REPERIMENTO TASTI DI FUNZIONE
145300940506     C************************************************************
145400940506     C     REPKEY        BEGSR
145500940506     C*
145600940506     C                   MOVEA     $FM2          $FM
145700940506     C                   EXSR      REPKKK
145800940506     C                   MOVEA     $FX           $FX2
145900940506     C                   ENDSR
146000940506     C/EJECT
146100940506     C************************************************************
146200940506     C* REPERIMENTO TASTI DI FUNZIONE
146300940506     C************************************************************
146400940506     C     REPKKK        BEGSR
146500940506     C*
146600940506     C                   CLEAR                   $FX
146700940506     C     1             DO        24            X
146800940506     C*
146900940506     C     $FM(X)        IFNE      *BLANK
147000940506     C*
147100940506     C                   MOVEL     $FM(X)        IDMSG
147200970214     C                   CALLB     'XRTVM'
147300940506     C                   PARM                    IDMSG            27
147400940506     C                   PARM                    TXTMSG          100
147500940506     C                   PARM                    ERRMSG            1
147600940506     C*
147700940506     C     ERRMSG        IFNE      *ON
147800940506     C                   MOVEL     TXTMSG        $FX(X)
147900940506     C                   ELSE
148000940506     C                   MOVEL     *ALL' '       $FX(X)
148100940506     C                   END
148200940506     C*
148300940506     C                   END
148400940506     C*
148500940506     C                   END
148600940506     C*
148700940506     C                   ENDSR
148800940223     C/EJECT
148900940128     C************************************************************
149000940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
149100940128     C************************************************************
149200940223     C     DEFCAM        BEGSR
149300940128     C*
149400940223     C* Variabili per gestione videate
149500940223     C*
149600940223     C                   MOVE      *BLANK        $GEST             2
149700940223     C                   MOVE      *BLANK        $FINE             1
149800940223     C                   MOVE      *BLANK        $INZS1            1
149900940223     C                   MOVE      *BLANK        $LASTG            2
150000940223     C                   MOVE      *BLANK        $LASTV            2
150100940223     C                   MOVE      *BLANK        $EFILE            1
150200940223     C                   MOVE      *BLANK        $ESCI             1
150300940223     C                   MOVE      *BLANK        $RCDOK            1
150400940223     C*
150500940314     C* Variabili appoggio sempre presenti in tutte le anagrafiche
150600940223     C*
150700940223     C     *LIKE         DEFINE    C1RCD         S1NRR
150800940223     C     *LIKE         DEFINE    C1NRR         WSFL
150900940223     C     *LIKE         DEFINE    C1RCD         WMAX
151000940223     C     *LIKE         DEFINE    C1RCD         WPAG
151100940223     C     *LIKE         DEFINE    $INZS1        WINZS1
151200940223     C                   MOVE      *BLANK        WWGRP             3
151300940223     C                   Z-ADD     0             CURR              2 0
151400940223     C                   Z-ADD     0             CURC              2 0
151500940223     C                   MOVE      *BLANK        WIN              99
151600940314     C     *LIKE         DEFINE    C1POS         W1POS
151700940620     C*
151800940721$022 C*    K5TBT1        KLIST
151900940721$022 C*                  KFLD                    TBTAZI
152000940721$022 C*                  KFLD                    TBTCOD
152100940721$022 C*                  KFLD                    TBTKEY
152200940721$022 C*                  KFLD                    TBTLIN
152300940721$022 C*                  KFLD                    TBTFLD
152400940714     C*
152500940714     C     KABL          KLIST
152600940714     C                   KFLD                    ABLCOD
152700940714     C                   KFLD                    ABLKEY
152800940714     C                   KFLD                    ABLAZI
152900940314     C*
153000940314     C*
153100940223     C* Indici
153200940223     C*
153300940223     C                   Z-ADD     0             X                 3 0
153400940223     C                   Z-ADD     0             Y                 3 0
153500940127     C*
153600940117     C                   ENDSR
153700940223     C/EJECT
153800940616**
153900940225PROMSG    *LIBL     OPZ0001
154000940225PROMSG    *LIBL     OPZ0002
154100940131PROMSG    *LIBL     OPZ0003
154200940131PROMSG    *LIBL     OPZ0004
154300940131PROMSG    *LIBL     OPZ0005
154400940224PROMSG    *LIBL     OPZ0006
154500940224PROMSG    *LIBL     OPZ0007
154600940224PROMSG    *LIBL     OPZ0008
154700940224PROMSG    *LIBL     OPZ0009
154800970716PROMSG    *LIBL     OPZ0010
154900970716PROMSG    *LIBL     OPZ0013
155000970716PROMSG    *LIBL     OPZ0012
155100970716PROMSG    *LIBL     OPZ0011
155200970716PROMSG    *LIBL     OPZ0014
155300940201**
15540094022501S      <Selezione
15550094022502S      <Modifica
15560094022503S      <Copia
15570094022504S      <Annullamento
15580094022505S      <Visualizzazione
15590094060706N      <prova
15600094060707N      <prova
15610094060708N      <prova
15620094060709N      <prova
15630094072910S      <Invia
15640094071411S      <Abilita gruppo
15650094071412S      <Disabilita
15660094072813N      <PROVA
15670094083114N      <Commenti
156800940506** TASTI DI FUNZIONE  S1
156900940506PROMSG    *LIBL     KEY0001  01
157000940506PROMSG    *LIBL     KEY0002  02
157100940506PROMSG    *LIBL     KEY0003  03
157200940506PROMSG    *LIBL     KEY0004  04
157300940506PROMSG    *LIBL     KEY0005  05
157400940506PROMSG    *LIBL     KEY0006  06
157500940506PROMSG    *LIBL     KEY0007  07
157600940506PROMSG    *LIBL     KEY0008  08
157700940506PROMSG    *LIBL     KEY0009  09
157800970716PROMSG    *LIBL     KEY0010  10
157900970716PROMSG    *LIBL     KEY0011  11
158000970716PROMSG    *LIBL     KEY0012  12
158100970716PROMSG    *LIBL     KEY0013  13
158200970716PROMSG    *LIBL     KEY0014  14
158300970716PROMSG    *LIBL     KEY0015  15
158400970716PROMSG    *LIBL     KEY0016  16
158500970716PROMSG    *LIBL     KEY0017  17
158600970716PROMSG    *LIBL     KEY0018  18
158700970716PROMSG    *LIBL     KEY0019  19
158800940506PROMSG    *LIBL     KEY0020  20
158900940506PROMSG    *LIBL     KEY0021  21
159000940506PROMSG    *LIBL     KEY0022  22
159100940506PROMSG    *LIBL     KEY0023  23
159200940506PROMSG    *LIBL     KEY0024  24
159300940506**  ABILITAZIONE S1
1594009410140N  01
1595009407200N  02
1596009407201N  03
1597009407200N  04
1598009407200N  05
1599009407200N  06
1600009407200N  07
1601009407200N  08
1602009407200N  09
1603009407201N  10
1604009407200N  11
1605009407201N  12
1606009407200N  13
1607009407200N  14
1608009407200N  15
1609009407200N  16
1610009407200N  17
1611009407200N  18
1612009407200N  19
1613009407200N  20
1614009407200N  21
1615009407200N  22
1616009505020N  23
1617009407200N  24
