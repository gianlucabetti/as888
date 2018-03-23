000100941027     H*PARMS OPTION(*NOXREF)
000200940607     H*------GESTIONE TABELLA
000300940211     H DECEDIT('0,') DATEDIT(*DMY.)
000400980924      *
000500980924    > *             attributi contabili - tipo gestione
000600980924      *          ----------------------------------------
000700940224      *
000800940224      *
000900940224      * DOCUMENTI MICROANALISI :
001000940224      *
001100940224      * NOTE TECNICHE :
001200940224      *
001300940224      *
001400940224      * PERSONALIZZAZIONI
001500940224      *
001600940224      *
001700940224      *
001800940314      *               MAPPA INDICATORI
001900940224      *
002000940307      *  21           GENERICO OPERAZIONI I/O
002100940224      *  22           GENERICO ERRORE OPERAZIONI I/O
002200940224      *  30           SFLDSP
002300940224      * N31           SFLCLR
002400940224      *  31           SFLDSPCTL
002500940224      *  32           SFLNXTCHG
002600940224      *  33           SFLEND
002700940224      *  39           OF PRTF
002800940224      *  40 <---> 49  DSPATR ERRORI SU SFL
002900940608      *  Specificare l'uso dei singoli indicatori
003000940224      *  50 <---> 98  ERRORI SU VIDEO
003100940608      *  Specificare l'uso dei singoli indicatori
003200940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
003300940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
003400940224      *  99           INDIC. GENERALE DI ERRORE
003500940128     F*----------------------------------------------------*
003600940721$022 FANTBT01L  IF A E           K DISK
003700980924$001 FANGYA2D   CF   E             WORKSTN USROPN
003800940607     F                                     SFILE(S1:S1NRR)
003900940201     F                                     INFDS(DSFMT)
004000940714     FANABL01L  UF A E           K DISK
004100940128     D*----------------------------------------------------*
004200940302     D* Schiere per la gestione delle opzioni
004300940302     D*
004400940728    >D $MS1            S             27    DIM(14) CTDATA PERRCD(1)
004500940728    >D $TX1            S             18    DIM(14) ALT($MS1)
004600940728    >D $OP1            S              2  0 DIM(14) CTDATA PERRCD(1)
004700940728    >D $FL1            S              1    DIM(14) ALT($OP1)
004800940223     D $TX             S             18    DIM(50)
004900940223     D $FL             S              1    DIM(50)
005000940506     D* Schiere per la gestione dei tasti di funzione
005100940506     D $FM             S             27    DIM(24)
005200940506     D $FX             S             50    DIM(24)
005300940506     D $FC             S              1    DIM(24)
005400940506     D $FR             S              1    DIM(24)
005500940506     D* Schiere per la gestione dei tasti di funzione
005600940506     D* Video S1
005700940506     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
005800940506     D $FX2            S             50    DIM(24) ALT($FM2)
005900940506     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
006000940506     D $FR2            S              1    DIM(24) ALT($FC2)
006100940616     D* campi in lingua
006200940616$010 D LIN             S              6    DIM(0002) CTDATA PERRCD(001)
006300940616$010 D LIND            S             50    DIM(0002) ALT(LIN)
006400940616$010 D LPOS            S              3  0 DIM(0002) CTDATA PERRCD(001)
006500940616$010 D LLUN            S              3  0 DIM(0002) ALT(LPOS)
006600940211     D* Passaggio Parametri
006700940211     D KPJBA         E DS
006800940325     D*-------------
006900940620     D* DS Esterna autorizzazioni tabelle
007000940620     D XCHATB        E DS                  EXTNAME(XCHKATBDS)
007100940620     D*-------------
007200940620     D* DS  restituzione autorizzazioni
007300940620     D* IND=indice
007400940620     D* GRU=man. di gruppo
007500940620     D* AZI=man. azienda
007600941111     D AUTDS           DS
007700940620     D  MANIND                 1      1
007800940620     D  MANGRU                 2      2
007900940620     D  MANAZI                 3      3
008000940607     D* Richiamo a XATB
008100940607     D XATBDS        E DS
008200941103     D*-------------
008300941103      * DS Società
008400941103     D SOC001        E DS                  EXTNAME(XSOC001DS)
008500941103     D*-------------
008600941103      * DS Interna per dati utente
008700941103     D XSOCDS          DS          1000
008800940607     D*
008900940325     D* Parametri in ricezione
009000940607     D XTABDS          DS
009100940607     D* '  ' = Gestione
009200940607     D* '01' = Ricerca
009300940607     D* '02' = Gestione
009400940607     D* '05' = Visualizzazione
009500940714     D* '11' = Abilitazione
009600940607     D  OPZ001                 1      2
009700940607     D* 0 = ritorno normale
009800940607     D* 1 = F3
009900940607     D* 2 = F12
010000940607     D  RET001                 3      3
010100940607     D* operazione eseguite dal pgm chiamato :
010200940607     D* 0 = nessuna operazione
010300940607     D* 1 = eseguito aggiornamento
010400940607     D  OPR001                 4      4
010500940607     D* funzione non eseguibile per errore :
010600940607     D* 0 = nessun errore
010700940607     D* 1 = funzione richiamata chiusa in errore
010800940607     D* Aggiungere eventuali altri codici di errore
010900940607     D  ERR001                 5      5
011000940607     D* Azienda
011100940607     D  AZI001                 6      8
011200940607     D* Lingua
011300940607     D  LIN001                 9      9
011400940607     D* Codice scelto
011500940607     D  KEY001                10     24
011600940608     D* riga cursore chiamante
011700940608     D  RIG001                25     26  0
011800940608     D* colonna cursore chiamante
011900940608     D  COL001                27     28  0
012000940325     D*-------------
012100940325     D* Parametri del PGM di manutenzione STD002R
012200940607    >D D002DS          DS
012300940607     D  OPZ002                 1      2
012400940607     D* 0 = ritorno normale
012500940607     D* 1 = F3
012600940607     D* 2 = F12
012700940607     D  RET002                 3      3
012800940607     D* operazione eseguite dal pgm chiamato :
012900940607     D* 0 = nessuna operazione
013000940607     D* 1 = eseguito aggiornamento
013100940607     D  OPR002                 4      4
013200940607     D* funzione non eseguibile per errore :
013300940607     D* 0 = nessun errore
013400940607     D* 1 = funzione richiamata chiusa in errore
013500940607     D* Aggiungere eventuali altri codici di errore
013600940607     D  ERR002                 5      5
013700940607     D* Azienda
013800940607     D  AZI002                 6      8
013900940607     D* Lingua
014000940607     D  LIN002                 9      9
014100940607     D* Codice scelto
014200940607     D  KEY002                10     24
014300940211     D*-------------
014400940211     D DSFMT           DS           512
014500940506     D  $TASTO               369    369
014600940211     D  NRG                  370    370
014700940211     D  NCL                  371    371
014800940211     D  SFLNRR               378    379B 0
014900940207     D*-------------
015000940207     D* Nome PGM a video
015100940207     D                 DS
015200940207     D  PROGR                  1     10
015300940207     D  ASTER1                 1      1    INZ('*')
015400940207     D  SIGLA                  2      9
015500940207     D  ASTER2                10     10    INZ('*')
015600940729     D*
015700940729     D*
015800940127     D*-------------
015900940127     D* Reperimento nome PGM
016000940127     D STATUS         SDS           333
016100940127     D  DSPGM            *PROC
016200940607     D* descrizione tabella
016300980924$003 D EXTDS         E DS                  EXTNAME(ANGYA2DS)
016400940207     D*-------------
016500940211     D* COSTANTI
016600940211     D*-------------
016700940607     D SFLPAG          C                   CONST(05)
016800940728     D $DIMOP          C                   CONST(14)
016900940314     D* dimensione della schiera $MS1
017000940506     D*
017100940506     D* Tasti di funzione
017200940506     D F01             C                   CONST(X'31')
017300940506     D F02             C                   CONST(X'32')
017400940506     D F03             C                   CONST(X'33')
017500940506     D F04             C                   CONST(X'34')
017600940506     D F05             C                   CONST(X'35')
017700940506     D F06             C                   CONST(X'36')
017800940506     D F07             C                   CONST(X'37')
017900940506     D F08             C                   CONST(X'38')
018000940506     D F09             C                   CONST(X'39')
018100940506     D F10             C                   CONST(X'3A')
018200940506     D F11             C                   CONST(X'3B')
018300940506     D F12             C                   CONST(X'3C')
018400940506     D F13             C                   CONST(X'B1')
018500940506     D F14             C                   CONST(X'B2')
018600940506     D F15             C                   CONST(X'B3')
018700940506     D F16             C                   CONST(X'B4')
018800940506     D F17             C                   CONST(X'B5')
018900940506     D F18             C                   CONST(X'B6')
019000940506     D F19             C                   CONST(X'B7')
019100940506     D F20             C                   CONST(X'B8')
019200940506     D F21             C                   CONST(X'B9')
019300940506     D F22             C                   CONST(X'BA')
019400940506     D F23             C                   CONST(X'BB')
019500940506     D F24             C                   CONST(X'BC')
019600940506     D ENTER           C                   CONST(X'F1')
019700940506     D ROLDWN          C                   CONST(X'F4')
019800940506     D ROLLUP          C                   CONST(X'F5')
019900940302     I*
020000940211     I/EJECT
020100940207     I*-------------
020200940607     IS1
020300980924     I              S1DES                       DESYA2
020400980924     I              S1TPG                       TPGYA2
020500940207     I*-------------
020600940127     I*
020700940127     C*----------------------------------------------------*
020800940127     C*                MAIN LINE PROGRAM
020900940127     C*----------------------------------------------------*
021000940127     C*
021100940223     C* inizializzazione variabili
021200940223     C                   EXSR      INZVAR
021300940223     C*
021400940223     C     $FINE         DOWEQ     *OFF
021500940131     C     $GEST         CASEQ     'S1'          GESS1                          Lista
021600940117     C                   END
021700940117     C                   END
021800940325     C*
021900940325     C* fine programma
022000940325     C                   EXSR      ENDPGM
022100940225     C*
022200940325     C/EJECT
022300940325     C************************************************************
022400940325     C* FINE PROGRAMMA
022500940325     C************************************************************
022600940325     C     ENDPGM        BEGSR
022700940325     C*
022800940325     C* passaggio dati al pgm chiamante
022900940715     C                   MOVE      *BLANKS       KPJBU
023000940607     C                   MOVEL     XTABDS        KPJBU
023100940325     C*
023200940325     C* Se pgm Guida non è stato richiamato posso chiudere in LR
023300940607     C     OPZ001        IFEQ      *BLANK
023400940325     C                   SETON                                            LR
023500940325     C* altrimenti chiudo in RT
023600940325     C                   ELSE
023700940325     C                   SETON                                            RT
023800940325     C                   ENDIF
023900940325     C*
024000940325     C                   ENDSR
024100940131     C/EJECT
024200940224     C************************************************************
024300940131     C* RIEMPIMENTO OPZIONI
024400940224     C************************************************************
024500940131     C     RIEOPZ        BEGSR
024600940223     C*
024700940223     C* Input :
024800940223     C* - $TX = schiera con tutti i testi disponibili
024900940223     C* - $FL = schiere con i flag di validità associati ai testi
025000940225     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
025100940223     C*          opzione usato
025200940223     C* - WMXO = variabile contenente il numero massimo di testi
025300940223     C*          opzione usabili
025400940223     C* Output :
025500940223     C* - WOPZ1 = variabile contenente la prima riga opzioni
025600940223     C* - WOPZ2 = variabile contenente la seconda riga opzioni
025700940223     C*
025800940223     C                   CALL      'XOPZMSG'
025900940223     C* input
026000940223     C                   PARM                    $TX
026100940223     C                   PARM                    $FL
026200940225     C                   PARM                    WCOZ              3 0
026300940223     C                   PARM                    WMXO              3 0
026400940223     C* output
026500940223     C                   PARM                    WOPZ1            76
026600940223     C                   PARM                    WOPZ2            76
026700940607     C* input
026800940607$018 C                   PARM      56            $L01              3 0
026900940607     C                   PARM      0             $L02              3 0
027000940223     C*
027100940131     C                   ENDSR
027200940506     C/EJECT
027300940506     C************************************************************
027400940506     C* RIEMPIMENTO TASTI FUNZIONE
027500940506     C************************************************************
027600940506     C     RIEKEY        BEGSR
027700940506     C*
027800940506     C* Input :
027900940506     C* - $FX = schiera con tutti i testi disponibili
028000940506     C* - $FC = schiere con i flag di validità associati ai testi
028100940506     C* - $FR = schiere con i flag di REVERSE IMAGE
028200940506     C* - $ULK = ultimo testo caricato
028300940506     C* Output :
028400940506     C* - $ULK
028500940506     c* - $KEY1= 1° riga di tasti funzione
028600940506     c* - $KEY2= 2° riga di tasti funzione
028700940506     C*
028800940506     C                   CALL      'XKEYMSG'
028900940506     C* input
029000940506     C                   PARM                    $FX
029100940506     C                   PARM                    $FC
029200940506     C                   PARM                    $FR
029300940506     C                   PARM                    $ULK              3 0
029400940506     C* output
029500940506     C                   PARM                    $KEY1            79
029600940506     C                   PARM                    $KEY2            79
029700940607     C* input
029800940607$018 C                   PARM      56            $L01              3 0
029900941004$018 C                   PARM      56            $L02              3 0
030000940506     C*
030100940506     C                   ENDSR
030200940224     C/EJECT
030300940224     C************************************************************
030400940224     C* GESTIONE EMISSIONE TASTI FUNZIONE
030500940224     C************************************************************
030600940224     C     GESPIE        BEGSR
030700940224     C*
030800940224     C                   SELECT
030900940506     C* Lista
031000940506     C     $GEST         WHENEQ    'S1'
031100940506     C     $ULKS1        IFEQ      0
031200940506     C                   EXSR      F24S1
031300940506     C                   ELSE
031400940607     C                   WRITE     Z1
031500940506     C                   ENDIF
031600940506     C                   ENDSL
031700940506     C*
031800940224     C                   ENDSR
031900940506     C/EJECT
032000940506     C************************************************************
032100940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA S1
032200940506     C************************************************************
032300940506     C     CTRKS1        BEGSR
032400940506     C                   SETOFF                                       99
032500940506     C                   MOVEA     $FC2          $FC
032600940506     C                   EXSR      CTRKEY
032700940506     C     *IN99         IFEQ      *ON
032800940506     C                   SETON                                        97
032900940506     C                   ENDIF
033000940607     C*
033100940506     C                   ENDSR
033200940506     C/EJECT
033300940506     C************************************************************
033400940506     C* CONTROLLO ABILITAZIONE TASTI
033500940509     C* INPUT
033600940509     c* $TASTO =Tasto funzione premuto
033700940509     C* $FC = Tasti abilitati
033800940509     C* OUTPUT
033900940509     C* IN99 = ERrore
034000940506     C************************************************************
034100940506     C     CTRKEY        BEGSR
034200940506     C                   SELECT
034300940506     C     $TASTO        WHENEQ    F01
034400940720     C     $FC(1)        IFEQ      '0'
034500940506     C                   SETON                                        99
034600940506     C                   ENDIF
034700940506     C     $TASTO        WHENEQ    F02
034800940720     C     $FC(2)        IFEQ      '0'
034900940506     C                   SETON                                        99
035000940506     C                   ENDIF
035100940506     C     $TASTO        WHENEQ    F03
035200940720     C     $FC(3)        IFEQ      '0'
035300940506     C                   SETON                                        99
035400940506     C                   ENDIF
035500940506     C     $TASTO        WHENEQ    F04
035600940720     C     $FC(4)        IFEQ      '0'
035700940506     C                   SETON                                        99
035800940506     C                   ENDIF
035900940506     C     $TASTO        WHENEQ    F05
036000940720     C     $FC(5)        IFEQ      '0'
036100940506     C                   SETON                                        99
036200940506     C                   ENDIF
036300940506     C     $TASTO        WHENEQ    F06
036400940720     C     $FC(6)        IFEQ      '0'
036500940506     C                   SETON                                        99
036600940506     C                   ENDIF
036700940506     C     $TASTO        WHENEQ    F07
036800940720     C     $FC(7)        IFEQ      '0'
036900940506     C                   SETON                                        99
037000940506     C                   ENDIF
037100940506     C     $TASTO        WHENEQ    F08
037200940720     C     $FC(8)        IFEQ      '0'
037300940506     C                   SETON                                        99
037400940506     C                   ENDIF
037500940506     C     $TASTO        WHENEQ    F09
037600940720     C     $FC(09)       IFEQ      '0'
037700940506     C                   SETON                                        99
037800940506     C                   ENDIF
037900940506     C     $TASTO        WHENEQ    F10
038000940720     C     $FC(10)       IFEQ      '0'
038100940506     C                   SETON                                        99
038200940506     C                   ENDIF
038300940506     C     $TASTO        WHENEQ    F11
038400940720     C     $FC(11)       IFEQ      '0'
038500940506     C                   SETON                                        99
038600940506     C                   ENDIF
038700940506     C     $TASTO        WHENEQ    F12
038800940720     C     $FC(12)       IFEQ      '0'
038900940506     C                   SETON                                        99
039000940506     C                   ENDIF
039100940506     C     $TASTO        WHENEQ    F13
039200940720     C     $FC(13)       IFEQ      '0'
039300940506     C                   SETON                                        99
039400940506     C                   ENDIF
039500940506     C     $TASTO        WHENEQ    F14
039600940720     C     $FC(14)       IFEQ      '0'
039700940506     C                   SETON                                        99
039800940506     C                   ENDIF
039900940506     C     $TASTO        WHENEQ    F15
040000940720     C     $FC(15)       IFEQ      '0'
040100940506     C                   SETON                                        99
040200940506     C                   ENDIF
040300940506     C     $TASTO        WHENEQ    F16
040400940720     C     $FC(16)       IFEQ      '0'
040500940506     C                   SETON                                        99
040600940506     C                   ENDIF
040700940506     C     $TASTO        WHENEQ    F17
040800940720     C     $FC(17)       IFEQ      '0'
040900940506     C                   SETON                                        99
041000940506     C                   ENDIF
041100940506     C     $TASTO        WHENEQ    F18
041200940720     C     $FC(18)       IFEQ      '0'
041300940506     C                   SETON                                        99
041400940506     C                   ENDIF
041500940506     C     $TASTO        WHENEQ    F19
041600940720     C     $FC(19)       IFEQ      '0'
041700940506     C                   SETON                                        99
041800940506     C                   ENDIF
041900940506     C     $TASTO        WHENEQ    F20
042000940720     C     $FC(20)       IFEQ      '0'
042100940506     C                   SETON                                        99
042200940506     C                   ENDIF
042300940506     C     $TASTO        WHENEQ    F21
042400940720     C     $FC(21)       IFEQ      '0'
042500940506     C                   SETON                                        99
042600940506     C                   ENDIF
042700940506     C     $TASTO        WHENEQ    F22
042800940720     C     $FC(22)       IFEQ      '0'
042900940506     C                   SETON                                        99
043000940506     C                   ENDIF
043100940506     C     $TASTO        WHENEQ    F23
043200940720     C     $FC(23)       IFEQ      '0'
043300940506     C                   SETON                                        99
043400940506     C                   ENDIF
043500940506     C     $TASTO        WHENEQ    F24
043600940720     C     $FC(24)       IFEQ      '0'
043700940506     C                   SETON                                        99
043800940506     C                   ENDIF
043900940506     C                   ENDSL
044000940506     C                   ENDSR
044100940224     C/EJECT
044200940127     C************************************************************
044300940131     C* GESTIONE LISTA
044400940127     C************************************************************
044500940127     C     GESS1         BEGSR
044600940223     C* inizializzazione videata
044700940223     C     $INZS1        IFEQ      *ON
044800940127     C                   EXSR      INZS1
044900940223     C                   MOVE      *OFF          $INZS1
045000940127     C                   ENDIF
045100940223     C* emissione piede videata se proveniente da altra
045200940223     C* salvataggio valore formato di provenienza
045300940202     C     $LASTV        IFNE      'S1'
045400940224     C                   EXSR      GESPIE
045500940202     C                   MOVE      $LASTV        $LASTG
045600940202     C                   MOVE      'S1'          $LASTV
045700940202     C                   END
045800940223     C*
045900940223     C     WMAX          IFEQ      0
046000940607     C                   WRITE     D1                                           NO RCD
046100940211     C                   ENDIF
046200940729     C                   MOVE      '1'           WIN727            1
046300940127     C*
046400940607     C                   EXFMT     C1
046500940204     C     C1NRR         IFNE      0
046600940204     C                   Z-ADD     C1NRR         WSFL
046700940204     C                   ENDIF
046800940127     C                   Z-ADD     SFLNRR        C1RCD
046900940506     C*
047000940506     C* controllo abilitazione tasti funzione
047100940506     C*
047200940506     C                   EXSR      CTRKS1
047300940506     C     *IN99         IFEQ      *OFF
047400940506     C*
047500940127     C*
0476009401271    C                   SELECT
047700940127     C* F3=Fine
047800940506     C     $TASTO        WHENEQ    F03
047900940309     C                   EXSR      F03S1
048000940131     C* F10=Immissione
048100940506     C     $TASTO        WHENEQ    F10
048200940309     C                   EXSR      F10S1
048300940131     C* F12=Ritorno
048400940506     C     $TASTO        WHENEQ    F12
048500940309     C                   EXSR      F12S1
048600940224     C* F23=Altre opzioni
048700940506     C     $TASTO        WHENEQ    F23
048800940309     C                   EXSR      F23S1
048900940224     C* F24=Altri tasti
049000940506     C     $TASTO        WHENEQ    F24
049100940309     C                   EXSR      F24S1
049200940127     C* ROLL-UP
049300940506     C     $TASTO        WHENEQ    ROLLUP
049400940127     C                   EXSR      ROLS1
049500940127     C*
0496009401271O   C                   OTHER
049700940127     C* CONTROLLO DATI
049800940131     C                   EXSR      CTRC1
049900940201     C     *IN99         IFEQ      *OFF
050000940131     C                   EXSR      CTRS1
050100940131     C                   END
0502009401271-   C                   ENDSL
050300940127     C*
050400940506     C                   ENDIF
050500940127     C                   ENDSR
050600940224     C/EJECT
050700940127     C************************************************************
050800940131     C* INIZIALIZZAZIONE LISTA
050900940127     C************************************************************
051000940127     C     INZS1         BEGSR
051100940302     C* pulizia SFL
051200940128     C                   SETOFF                                         3031
051300940607     C                   WRITE     C1
051400940128     C                   SETON                                          31
051500940127     C*
051600940128     C* riempimento opzioni
051700940223     C                   MOVEA     $TX1          $TX
051800940223     C                   MOVEA     $FL1          $FL
051900940309     C                   Z-ADD     0             WCOZ
052000940225     C*  equivale al num.di elementi della schiera $TX1
052100940509     C                   Z-ADD     $DIMOP        WMXO
052200940128     C                   EXSR      RIEOPZ
052300940608     C                   MOVEL     WOPZ1         C1OPZ1
052400940225     C                   Z-ADD     WCOZ          C1COZ
052500940128     C*
052600940128     C* CARICAMENTO SFL A PAGINE
052700940128     C*
052800940201     C                   Z-ADD     0             S1NRR
052900940128     C                   Z-ADD     0             WMAX
053000940224     C*
053100940224     C* Posizionamento su file pilota
053200940302     C                   CLEAR                   C1POS
053300940316    >C                   EXSR      SETANA
053400940608    >C                   EXSR      REDANA
053500940302     C* ripristino i valori del posizionamento
053600940302     C                   MOVE      W1POS         C1POS
053700940131     C*
053800940131     C* Carico 1a pagina
053900940127     C                   EXSR      ROLS1
054000940201     C                   Z-ADD     SFLPAG        WPAG
054100940127     C*
054200940127     C                   ENDSR
054300940127     C/EJECT
054400940127     C************************************************************
054500940131     C* CARICAMENTO PAGINA LISTA
054600940127     C************************************************************
054700940127     C     ROLS1         BEGSR
054800940127     C*
054900940128     C                   SETOFF                                       32
055000940223     C                   Z-ADD     0             Y
055100940127     C                   Z-ADD     WMAX          S1NRR
055200940127     C*
055300940127     C* Leggo dal file anagrafico per caricare la lista
055400940127     C*
0555009401311    C     $EFILE        DOWEQ     *OFF
055600940201     C     Y             ANDLT     WPAG
055700940127     C*
055800940131     C                   EXSR      RIES1
055900940127     C*
056000940127     C                   ADD       1             S1NRR
056100940127     C                   ADD       1             Y
056200940714     C                   MOVE      $IN41         *IN41
056300940127     C*
056400940607     C                   WRITE     S1
056500940131     C*
056600940316    >C                   EXSR      REDANA
056700940128     C*
0568009401271-   C                   ENDDO
056900940127     C*
057000940223     C                   Z-ADD     S1NRR         WMAX                 30
057100940127     C*
057200940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
057300940127     C*
057400940201     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
057500940127     C                   MVR                     RESTO             3 0
057600940131     C     PAGINE        MULT      SFLPAG        C1RCD
0577009401271    C     RESTO         IFGT      0
057800940127     C                   ADD       1             C1RCD
0579009401271E   C                   ELSE
058000940407     C                   SUB       SFLPAG        C1RCD
058100940407     C                   ADD       1             C1RCD
0582009401271-   C                   ENDIF
058300940128     C*
058400940127     C                   ENDSR
058500940224     C/EJECT
058600940224     C************************************************************
058700940224     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
058800940224     C************************************************************
058900940607     C     SETANA        BEGSR
059000940224     C*
059100941004     C                   MOVE      *BLANKS       XTBKEY
059200940607     C                   MOVE      AZI001        XTBAZI
059300940607     C                   MOVE      LIN001        XTBLIN
059400940608     C                   MOVE      '9'           XTBRIC
059500940607     C                   MOVE      '0'           XTBALC
059600940713     C                   MOVE      XSCCUT        XTBUTE
059700940607     C                   MOVE      '0'           XTBERR
059800941121     C**         W1POS     IFNE *BLANK
059900941004     C                   MOVE      *ZEROS        XTBKEY
060000940608     C                   MOVE      W1POS         XTBKEY
060100941121     C**                   END
060200940607     C                   CALL      'XATB'
060300940607     C                   PARM                    XATBDS
060400940607     C*
060500940607     C     XTBERR        IFNE      '0'
060600940607     C                   MOVEL     *ON           $EFILE
060700940607     C                   ENDIF
060800940224     C*
060900940224     C                   ENDSR
061000940128     C/EJECT
061100940128     C************************************************************
061200940131     C* LETTURA RCD ARCHIVIO PILOTA
061300940128     C************************************************************
061400940607     C     REDANA        BEGSR
061500940128     C*
061600940131     C                   MOVEL     *OFF          $EFILE
061700940131     C                   MOVEL     *OFF          $RCDOK
061800940131     C*
0619009401311    C     $EFILE        DOUEQ     *ON
062000940131     C     $RCDOK        OREQ      *ON
062100940131     C*
062200940607     C                   MOVE      '3'           XTBRIC
062300940607     C                   MOVE      '0'           XTBERR
062400940607     C                   CALL      'XATB'
062500940607     C                   PARM                    XATBDS
062600940607     C*
062700940607     C     XTBERR        IFNE      '0'
062800940607     C                   MOVEL     *ON           $EFILE
062900940607     C                   ENDIF
063000940131     C*
0631009401312    C     $EFILE        IFEQ      *OFF
063200940607     C                   EXSR      SELANA
063300940607     C                   MOVE      XTBKEY        SAVKEY
063400940607     C                   MOVE      XTBGRU        SAVGRU
063500940607     C                   MOVE      XTBUNI        SAVUNI
0636009401312-   C                   ENDIF
063700940131     C*
0638009401311-   C                   ENDDO
063900940131     C*
064000940607     C                   MOVE      $EFILE        *IN33                          SflEnd
064100940131     C*
064200940131     C                   ENDSR
064300940224     C/EJECT
064400940224     C************************************************************
064500940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
064600940224     C************************************************************
064700940224     C     CLCPAG        BEGSR
064800940224     C* Input :
064900940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
065000940224     C*          cursore
065100940224     C* - SFLPAG = numero rcd per pagina sfl
065200940224     C* Output :
065300940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
065400940224     C*
065500940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
065600940224     C                   MVR                     RESTO             3 0
065700940224     C     RESTO         IFGT      0
065800940224     C                   ADD       1             PAGINE
065900940224     C                   ENDIF
066000940224     C     PAGINE        MULT      SFLPAG        WPAG
066100940224     C*
066200940224     C                   ENDSR
066300940131     C/EJECT
066400940131     C************************************************************
066500940131     C* SELEZIONI SULLA RIGA LETTA
066600940314     C* impostare a *OFF il campo $RCDOK per scartare un record che
066700940314     C* non rientra nei parametri di parzializzazione
066800940314     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
066900940314     C* superiore stabilito nei parametri
067000940314     C*
067100940131     C************************************************************
067200940607     C     SELANA        BEGSR
067300940131     C*
067400940202     C                   MOVEL     *ON           $RCDOK
067500940714     C* Se aperto il pgm con opzione 11
067600940714     C* carico solo i record di gruppo
067700940714     C     OPZ001        IFEQ      '11'
067800940714     C     XTBGRU        ANDNE     'G'
067900940714     C                   MOVEL     *OFF          $RCDOK
068000940714     C                   END
068100940714     C* Carica in alta intensità solo i record
068200940714     C* abilitati
068300940714     C                   MOVE      '0'           $IN41
068400940714     C     OPZ001        IFEQ      '11'
068500940714     C     XTBGRU        ANDEQ     'G'
068600940715     C     XTBAZB        ANDNE     'N'
068700940714     C                   MOVE      '1'           $IN41
068800940714     C                   END
068900940714     C*
069000940202     C                   ENDSR
069100940131     C/EJECT
069200940131     C************************************************************
069300940131     C* RIEMPIMENTO RIGA LISTA
069400940131     C************************************************************
069500940131     C     RIES1         BEGSR
069600940607     C                   MOVEL     SAVUNI        EXTDS
069700940607     C                   MOVE      SAVKEY        S1COD
069800940607     C                   MOVE      SAVGRU        S1GRU
069900940607     C     *LIKE         DEFINE    XTBUNI        SAVUNI
070000940607     C     *LIKE         DEFINE    XTBKEY        SAVKEY
070100940607     C     *LIKE         DEFINE    XTBGRU        SAVGRU
070200940131     C*
070300940131     C                   ENDSR
070400940309     C/EJECT
070500940309     C************************************************************
070600940309     C* GESTIONE F03 VIDEO S1
070700940309     C************************************************************
070800940309     C     F03S1         BEGSR
070900940309     C*
071000940309     C                   MOVE      *ON           $FINE
071100940722     C                   MOVE      '1'           RET001
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
072500941005     C                   MOVE      LIN001        LIN002
072600941005     C                   MOVE      AZI001        AZI002
072700940715     C                   MOVE      *BLANKS       KPJBU
072800940607     C                   MOVEL     D002DS        KPJBU
072900980924$004 C                   CALL      'ANGYA2R2'
073000940309     C                   PARM                    KPJBA
073100940607     C                   MOVEL     KPJBU         D002DS
073200940309     C* ritorno da PGM gestione
073300940309     C                   EXSR      GESRET
073400940309     C     WINZS1        IFEQ      *ON
073500940309     C                   MOVE      *ON           $INZS1
073600940309     C* se esistevano già righe sul sfl
073700940309     C* calcolo pagina a cui deve posizionarsi
073800940309     C     WSFL          IFGT      0
073900940309     C                   EXSR      CLCPAG
074000940309     C* altrimenti carico solo la 1a pagina
074100940309     C                   ELSE
074200940309     C                   Z-ADD     SFLPAG        WPAG
074300940309     C                   ENDIF
074400940309     C                   ENDIF
074500940309     C*
074600940712     C                   MOVE      *HIVAL        $LASTV
074700940309     C                   ENDSR
074800940309     C/EJECT
074900940309     C************************************************************
075000940309     C* GESTIONE F12 VIDEO S1
075100940309     C************************************************************
075200940309     C     F12S1         BEGSR
075300940309     C*
075400940722     C                   MOVE      '2'           RET001
075500940309     C                   MOVE      *ON           $FINE
075600940309     C*
075700940309     C                   ENDSR
075800940309     C/EJECT
075900940309     C************************************************************
076000940309     C* GESTIONE F23 VIDEO S1
076100940309     C************************************************************
076200940309     C     F23S1         BEGSR
076300940309     C*
076400940309     C                   EXSR      RIEOPZ
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
092600940620     C                   MOVE      '0'           XTBALC
092700940620     C                   MOVE      '1'           XTBRIC
092800940620     C                   CALL      'XATB'
092900940620     C                   PARM                    XATBDS
093000940620     C* Reperisco autorizzazioni utente
093100940620     C                   CLEAR                   XCHATB
093200941118     C                   MOVE      XSCSOC        XCTSOC
093300941118     C                   MOVE      XSCDUN        XCTUNI
093400940620     C                   MOVEL     KNMUS         XCTPRF
093500940620     C                   MOVE      XTBCOD        XCTCOD
093600940620     C                   CALL      'XCHKATB'
093700940620     C                   PARM                    XCHATB
093800941111     C                   MOVEL     XCTDDS        AUTDS
093900941027     C     XCTRTN        IFEQ      1
094000941027     C                   MOVEL     'N'           MANGRU
094100941027     C                   ENDIF
094200940620     C* Non ammessa manutenzione di gruppo
094300940620   1 C     XTBGRU        IFEQ      'G'
094400940620     C     MANGRU        ANDEQ     'N'
094500940620   2 C     S1OPZ         IFEQ      2
094600940729     C     S1OPZ         OREQ      3
094700940620     C     S1OPZ         OREQ      4
094800940620     C                   SETON                                        5399
094900940620   2 C                   ENDIF
095000940620   1 C                   ENDIF
095100940620     C* Non ammessa manutenzione di azienda
095200940620   1 C     XTBGRU        IFEQ      'A'
095300940620     C     MANAZI        ANDEQ     'N'
095400940620   2 C     S1OPZ         IFEQ      2
095500940729     C     S1OPZ         OREQ      3
095600940620     C     S1OPZ         OREQ      4
095700940620     C                   SETON                                        5499
095800940620   2 C                   ENDIF
095900940620   1 C                   ENDIF
096000940607     C                   ENDIF
096100940131     C                   ENDSR
096200940224     C/EJECT
096300940131     C************************************************************
096400940131     C* GESTIONE OPZIONI LISTA
096500940131     C************************************************************
096600940131     C     OPZS1         BEGSR
096700940201     C*
0968009402251    C                   SELECT
096900940225     C*
097000940225     C* se opz=Selezione passo la chiave al pgm chiamante
097100940225     C     S1OPZ         WHENEQ    1
097200940607     C                   MOVE      S1OPZ         OPZ001
097300940607     C                   MOVE      S1COD         KEY001
097400940225     C                   MOVE      *ON           $ESCI
097500940225     C                   MOVE      *ON           $FINE
097600940729     C*
097700940714     C*
097800940714     C* se opz=Abilita scrive su archivio abilitazioni
097900940714     C     S1OPZ         WHENEQ    11
098000941103     C                   MOVE      XSCSOC        ABLAZI
098100940714     C                   MOVE      *ZEROS        ABLKEY
098200940714     C                   MOVE      S1COD         ABLKEY
098300940714     C                   MOVE      XTBCOD        ABLCOD
098400940714     C                   WRITE     ANABL000                             22
098500940714     C*
098600940714     C* se opz=Disabil.cancella su archivio abilitazioni
098700940714     C     S1OPZ         WHENEQ    12
098800941103     C                   MOVE      XSCSOC        ABLAZI
098900940714     C                   MOVE      *ZEROS        ABLKEY
099000940714     C                   MOVE      S1COD         ABLKEY
099100940714     C                   MOVE      XTBCOD        ABLCOD
099200940714     C     KABL          DELETE    ANABL000                           22
099300940729     C*
099400940225     C*
099500940225     C* se altre opzioni (o nessuna opzione) richiamo pgm gestione
0996009402251O   C                   OTHER
099700940225     C*
099800940616     C                   RESET                   D002DS
099900940607     C                   MOVEL     S1OPZ         OPZ002
100000940616     C                   MOVE      *ZERO         KEY002
100100940607     C                   MOVE      S1COD         KEY002
100200941005     C                   MOVE      LIN001        LIN002
100300941005     C                   MOVE      AZI001        AZI002
100400940715     C                   MOVE      *BLANKS       KPJBU
100500940607     C                   MOVEL     D002DS        KPJBU
100600980924$004 C                   CALL      'ANGYA2R2'
100700940607     C                   PARM                    KPJBA
100800940607     C                   MOVEL     KPJBU         D002DS
100900940201     C*
101000940223     C* ritorno da PGM gestione
101100940223     C                   EXSR      GESRET
101200940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
1013009402252    C     *IN99         IFEQ      *ON
101400940223     C                   SETON                                        40
1015009402252-   C                   ENDIF
101600940131     C*
101700940506     C                   MOVE      *HIVAL        $LASTV
1018009402251-   C                   ENDSL
101900940225     C*
102000940131     C                   ENDSR
102100940223     C/EJECT
102200940223     C************************************************************
102300940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
102400940223     C************************************************************
102500940223     C     GESRET        BEGSR
102600940223     C*
102700940223     C* modo di ritorno
102800940223     C*
1029009402231    C                   SELECT
103000940314    >C* << questi modi di utilizzo dei valori di ritorno dal
103100940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
103200940314    >C*    proposte, normalmente sempre valide, ma modificabili
103300940314    >C*    per situazioni particolari >>
103400940223     C* 1 = F3
103500940314    >C     RET002        WHENEQ    '1'
103600940224     C                   MOVE      *ON           $ESCI                          sospese READC
103700940223     C                   MOVE      *ON           $FINE                          fine pgm
103800940223     C* 2 = F12
103900940314    >C     RET002        WHENEQ    '2'
104000940223     C                   MOVE      *ON           $ESCI                          sospese READC
104100940223     C*
1042009402231-   C                   ENDSL
104300940223     C*
104400940223     C* operazione eseguite dal pgm chiamato
104500940223     C*
1046009402231    C                   SELECT
104700940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
104800940314    >C     OPR002        WHENEQ    '1'
104900940223     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
105000940223     C*
1051009402231-   C                   ENDSL
105200940223     C*
105300940223     C* funzione non eseguibile per errore :
105400940223     C*
1055009402231    C                   SELECT
105600940223     C* 1 = funzione richiamata chiusa in errore
105700940316    >C*  eventualmente gestire altri codici di errore
105800940314    >C     ERR002        WHENEQ    '1'
105900940223     C                   MOVE      *ON           $ESCI                          sospese READC
106000940223     C                   SETON                                        5299      emiss.SFLMSGID
106100940223     C*
1062009402231-   C                   ENDSL
106300940223     C*
106400940223     C                   ENDSR
106500940223     C/EJECT
106600940131     C************************************************************
106700940131     C* OPERAZIONI INIZIALI
106800940131     C************************************************************
106900940131     C     *INZSR        BEGSR
107000940131     C*
107100940127     C* Reperimento parametri
107200940127     C*
107300940117     C     *ENTRY        PLIST
107400940117     C                   PARM                    KPJBA
107500940714     C*
107600940714     C*
107700940127     C*
107800940223     C* Reperimento opzioni
107900940127     C*
108000940128     C                   EXSR      REPOPZ
108100940506     C*
108200940506     C* Reperimento tasti di funzione
108300940506     C*
108400940506     C                   EXSR      REPKEY
108500940506     C*
108600980924$007 C                   MOVE      'YA2'         XTBCOD                         *
108700940127     C*
108800940117     C                   ENDSR
108900940225     C/EJECT
109000941103      ************************************************************
109100941103      * Reperimento dati società
109200941103      ************************************************************
109300941103     C     REPSOC        BEGSR
109400941103      *
109500941103     C                   CALL      'XSOC'
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
110900940223     C* Reperimento codice lingua e codice azienda
111000940223     C*
111100940223     C                   MOVEL     'SOC001'      TIPXSC
111200940223     C                   EXSR      REPSOC
111300940223     C     RTNXSC        IFNE      '1'
111400940223     C                   MOVEL     XSOCDS        SOC001
111500940223     C                   ELSE
111600940223     C                   MOVE      *ON           $FINE
111700940223     C* fine programma
111800940223     C                   EXSR      ENDPGM
111900940223     C                   ENDIF
112000940223     C* Lingua e codice azienda
112100940223     C                   MOVE      XSCLIN        LIN001
112200940223     C                   MOVE      XSCSOC        AZI001
112300940715     C*
112400940715     C                   RESET                   $FC2
112500940715     C*
112600980924$021 C                   CLOSE     ANGYA2D
112700980924$021 C                   OPEN      ANGYA2D
112800940714     C*
112900940714     C* Dati da PGM chiamante
113000940714     C*
113100940714     C                   MOVEL     KPJBU         XTABDS
113200940506     C*
113300940506     C* Definizione delle opzioni utilizzabili a pgm
113400940506     C*
113500940506     C                   EXSR      DEFOPZ
113600940718     C* Pulizia campi e indicatori
113700940718     C                   CLEAR                   W1POS
113800940718     C                   MOVE      *ALL'0'       IN4049           10
113900940718     C                   MOVEA     IN4049        *IN(40)
114000940718     C                   CLEAR                   S1OPZ
114100940223     C* Variabili per gestione videate
114200940223     C*
114300940223     C                   MOVE      *OFF          $FINE                          fine pgm
114400940223     C                   MOVE      *OFF          $INZS1                         inizializzione S1
114500940223     C                   MOVE      *OFF          $EFILE                         memo fine file
114600940223     C                   MOVE      *OFF          $ESCI                          rich.uscita ciclo
114700940223     C                   MOVE      *OFF          $RCDOK                         rcd letto valido
114800940509     C                   MOVE      *HIVAL        $LASTV
114900940506     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
115000940223     C*
115100940318     C                   MOVE      *ON           $INZS1
115200940223     C                   MOVE      'S1'          $GEST
115300940223     C*
115400940223     C* Variabili appoggio
115500940223     C*
115600940223     C                   Z-ADD     SFLPAG        WPAG
115700940223     C*
115800940223     C* Valorizzazione campi univoci testate
115900940223     C*
116000940607     C*                    CLEART1
116100940607     C*                    MOVELKNSIF     NOMSIF
116200940607     C*                    MOVELRAGUTE    NOMDIT
116300940607     C*                    MOVELDSPGM     SIGLA
116400940607     C*                    MOVELPROGR     NOMPGM
116500940223     C*
116600940608     c* posizione window
116700940608$012 C                   Z-ADD     58            PCOL              2 0
116800940613     C                   Z-ADD     09            R
116900940608     C     80            SUB       PCOL          C
117000940608     C                   SUB       3             C
117100940608     C     OPZ001        IFNE      '  '
117200940608     C     RIG001        IFGT      11
117300940608     C                   Z-ADD     1             R
117400940608     C                   ENDIF
117500940608     C*
117600940608     C     COL001        IFGT      40
117700940608     C                   Z-ADD     2             C
117800940608     C                   ENDIF
117900940608     C                   ENDIF
118000940616     C*
118100940616     C     OPZ001        IFNE      '01'
118200940616$019 C                   DO        02            I                 3 0
118300940721$022 C                   MOVEL     LIN(I)        TBTFLD
118400940721$022 C                   CLEAR                   TBTKEY
118500940721$022 C                   CLEAR                   TBTAZI
118600940721$022 C                   CLEAR                   TBTLIN
118700980924$007 C                   MOVE      'YA2'         TBTCOD            3            *
118800940721$022 C     K5TBT1        CHAIN     ANTBT01L                           21
118900940721$022 C     *IN21         IFEQ      *ON
119000940721$022 C                   Z-ADD     LLUN(I)       TBTLUN
119100940721$022 C                   Z-ADD     LPOS(I)       TBTPOS
119200940721$022 C                   MOVEL     LIND(I)       TBTDES
119300940721$022 C                   WRITE     ANTBT000
119400940721$022 C                   ENDIF
119500940616     C                   ENDDO
119600940616     C                   ENDIF
119700940608     C*
119800940722     c*
119900940722     c* Se richiamato con OPZ 11  Non controlla abilitazione all'aziend
120000940722     C     OPZ001        IFEQ      '11'
120100940722     C                   MOVE      'N'           XTBGAB
120200940722     C                   ELSE
120300940722     C                   MOVE      'S'           XTBGAB
120400940722     C                   ENDIF
120500940223     C                   ENDSR
120600940223     C/EJECT
120700940131     C************************************************************
120800940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
120900940131     C************************************************************
121000940225     C     DEFOPZ        BEGSR
121100940131     C*
121200940225     C* Questa routine deve valorizzare la schiera $FL1
121300940225     C* con S o N a seconda della utilizzabilità o meno
121400940225     C* dell'opzione correlata
121500940225     C*
121600940714     C* Inizialmente si considerano tutte le opzioni come non valide
121700940728     C     1             DO        14            X
121800940714     C                   MOVE      'N'           $FL1(X)
121900940225     C                   END
122000940225     C*
122100940225     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
122200940714     C* rendo valide alcune opzioni
122300940714     C* i tasti invece li disattivo
122400940225     C                   SELECT
122500940714     C*
122600940714     C* Se pgm richiamato in RICERCA attivo :
122700940714     C*
122800940316    >C     OPZ001        WHENEQ    '01'
122900940714     C* F3=Fine
123000940714     C                   Z-ADD     3             X
123100940720     C                   MOVE      '0'           $FC2(X)
123200940714     C* F10=Immissione
123300940714     C                   Z-ADD     10            X
123400940919     C                   MOVE      '1'           $FC2(X)
123500940714     C* 1=Selezione
123600940714     C                   Z-ADD     1             X
123700940714     C     01            LOOKUP    $OP1(X)                                21
123800940714     C     *IN21         IFEQ      *ON
123900940714     C                   MOVE      'S'           $FL1(X)
124000940714     C                   ENDIF
124100940729     C* 2=Modifica
124200940729     C                   Z-ADD     1             X
124300940729     C     02            LOOKUP    $OP1(X)                                21
124400940729     C     *IN21         IFEQ      *ON
124500940729     C                   MOVE      'S'           $FL1(X)
124600940729     C                   ENDIF
124700940729     C* 3=Copia
124800940729     C                   Z-ADD     1             X
124900940729     C     03            LOOKUP    $OP1(X)                                21
125000940729     C     *IN21         IFEQ      *ON
125100940729     C                   MOVE      'S'           $FL1(X)
125200940729     C                   ENDIF
125300940729     C* 4=Annullamento
125400940729     C                   Z-ADD     1             X
125500940729     C     04            LOOKUP    $OP1(X)                                21
125600940729     C     *IN21         IFEQ      *ON
125700940729     C                   MOVE      'S'           $FL1(X)
125800940729     C                   ENDIF
125900940714     C* 5=Visualizzazione
126000940714     C                   Z-ADD     1             X
126100940714     C     05            LOOKUP    $OP1(X)                                21
126200940714     C     *IN21         IFEQ      *ON
126300940714     C                   MOVE      'S'           $FL1(X)
126400940714     C                   ENDIF
126500940728     C*14=Commenti
126600940831     C**                   Z-ADD1         X
126700940831     C**         14        LOKUP$OP1,X                   21
126800940831     C**         *IN21     IFEQ *ON
126900940831     C**                   MOVE 'S'       $FL1,X
127000940831     C**                   ENDIF
127100940714     C*
127200940714     C* Se pgm richiamato in INTERROGAZIONE attivo :
127300940714     C*
127400940603    >C     OPZ001        WHENEQ    '05'
127500940714     C* F10=Immissione
127600940714     C                   Z-ADD     10            X
127700940720     C                   MOVE      '0'           $FC2(X)
127800940714     C* F3=Fine
127900940714     C                   Z-ADD     3             X
128000940720     C                   MOVE      '0'           $FC2(X)
128100940714     C* 5=Visualizzazione
128200940225     C                   Z-ADD     1             X
128300940714     C     05            LOOKUP    $OP1(X)                                21
128400940225     C     *IN21         IFEQ      *ON
128500940714     C                   MOVE      'S'           $FL1(X)
128600940225     C                   ENDIF
128700940728     C*14=Commenti
128800940831     C**                   Z-ADD1         X
128900940831     C**         14        LOKUP$OP1,X                   21
129000940831     C**         *IN21     IFEQ *ON
129100940831     C**                   MOVE 'S'       $FL1,X
129200940831     C**                   ENDIF
129300940831     C*
129400940714     C* Se pgm richiamato in ABILITAZIONE attivo :
129500940714     C*
129600940714    >C     OPZ001        WHENEQ    '11'
129700940714     C* F10=Immissione
129800940714     C                   Z-ADD     10            X
129900940720     C                   MOVE      '0'           $FC2(X)
130000940714     C* F3=Fine
130100940714     C                   Z-ADD     3             X
130200940720     C                   MOVE      '0'           $FC2(X)
130300940714     C* 5=Visualizzazione
130400940714     C                   Z-ADD     1             X
130500940714     C     05            LOOKUP    $OP1(X)                                21
130600940714     C     *IN21         IFEQ      *ON
130700940714     C                   MOVE      'S'           $FL1(X)
130800940714     C                   ENDIF
130900940714     C* 11=Abilitazione
131000940714     C                   Z-ADD     1             X
131100940714     C     11            LOOKUP    $OP1(X)                                21
131200940714     C     *IN21         IFEQ      *ON
131300940714     C                   MOVE      'S'           $FL1(X)
131400940714     C                   ENDIF
131500940714     C* 12=Disabilita
131600940714     C                   Z-ADD     1             X
131700940714     C     12            LOOKUP    $OP1(X)                                21
131800940714     C     *IN21         IFEQ      *ON
131900940714     C                   MOVE      'S'           $FL1(X)
132000940714     C                   ENDIF
132100940714     C*
132200940714     C* Se pgm richiamato in GESTIONE  attivo :
132300940714     C*
132400940714    >C     OPZ001        WHENEQ    '02'
132500940714     C* F3=Fine
132600940714     C                   Z-ADD     3             X
132700940720     C                   MOVE      '0'           $FC2(X)
132800940714     C* 2=Modifica
132900940714     C                   Z-ADD     1             X
133000940714     C     02            LOOKUP    $OP1(X)                                21
133100940714     C     *IN21         IFEQ      *ON
133200940714     C                   MOVE      'S'           $FL1(X)
133300940714     C                   ENDIF
133400940714     C* 3=Copia
133500940714     C                   Z-ADD     1             X
133600940714     C     03            LOOKUP    $OP1(X)                                21
133700940714     C     *IN21         IFEQ      *ON
133800940714     C                   MOVE      'S'           $FL1(X)
133900940714     C                   ENDIF
134000940714     C* 4=Annullamento
134100940714     C                   Z-ADD     1             X
134200940714     C     04            LOOKUP    $OP1(X)                                21
134300940714     C     *IN21         IFEQ      *ON
134400940714     C                   MOVE      'S'           $FL1(X)
134500940714     C                   ENDIF
134600940714     C* 5=Visualizzazione
134700940714     C                   Z-ADD     1             X
134800940714     C     05            LOOKUP    $OP1(X)                                21
134900940714     C     *IN21         IFEQ      *ON
135000940714     C                   MOVE      'S'           $FL1(X)
135100940714     C                   ENDIF
135200940729     C*14=Commenti
135300940831     C**                   Z-ADD1         X
135400940831     C**         14        LOKUP$OP1,X                   21
135500940831     C**         *IN21     IFEQ *ON
135600940831     C**                   MOVE 'S'       $FL1,X
135700940831     C**                   ENDIF
135800940225     C*
135900940225     C                   OTHER
136000940714     C* Se pgm non richiamato, attivo TUTTO  :
136100940714     C* 2=Modifica
136200940714     C                   Z-ADD     1             X
136300940714     C     02            LOOKUP    $OP1(X)                                21
136400940714     C     *IN21         IFEQ      *ON
136500940714     C                   MOVE      'S'           $FL1(X)
136600940714     C                   ENDIF
136700940714     C* 3=Copia
136800940714     C                   Z-ADD     1             X
136900940714     C     03            LOOKUP    $OP1(X)                                21
137000940714     C     *IN21         IFEQ      *ON
137100940714     C                   MOVE      'S'           $FL1(X)
137200940714     C                   ENDIF
137300940714     C* 4=Annullamento
137400940714     C                   Z-ADD     1             X
137500940714     C     04            LOOKUP    $OP1(X)                                21
137600940714     C     *IN21         IFEQ      *ON
137700940714     C                   MOVE      'S'           $FL1(X)
137800940714     C                   ENDIF
137900940714     C* 5=Visualizzazione
138000940714     C                   Z-ADD     1             X
138100940714     C     05            LOOKUP    $OP1(X)                                21
138200940714     C     *IN21         IFEQ      *ON
138300940714     C                   MOVE      'S'           $FL1(X)
138400940714     C                   ENDIF
138500940714     C* 11=Abilitazione
138600940714     C                   Z-ADD     1             X
138700940714     C     11            LOOKUP    $OP1(X)                                21
138800940714     C     *IN21         IFEQ      *ON
138900940714     C                   MOVE      'S'           $FL1(X)
139000940714     C                   ENDIF
139100940714     C* 12=Disabilita
139200940714     C                   Z-ADD     1             X
139300940714     C     12            LOOKUP    $OP1(X)                                21
139400940714     C     *IN21         IFEQ      *ON
139500940714     C                   MOVE      'S'           $FL1(X)
139600940714     C                   ENDIF
139700940728     C*14=Commenti
139800940831     C**                   Z-ADD1         X
139900940831     C**         14        LOKUP$OP1,X                   21
140000940831     C**         *IN21     IFEQ *ON
140100940831     C**                   MOVE 'S'       $FL1,X
140200940831     C**                   ENDIF
140300940225     C* poi, disattivo a seconda delle autorizzazioni :
140400940314    >C*********  AUTXXX    IFEQ 'N'
140500940314    >C*********            Z-ADD1         X
140600940314    >C*********  'XX'      LOKUP$OP1,X                   21
140700940314    >C*********  *IN21     IFEQ *ON
140800940314    >C*********            MOVE 'N'       $FL1,X
140900940314    >C*********            ENDIF
141000940314    >C*********            ENDIF
141100940225     C*
141200940225     C                   ENDSL
141300940225     C*
141400940225     C                   ENDSR
141500940225     C/EJECT
141600940225     C************************************************************
141700940225     C* REPERIMENTO OPZIONI
141800940225     C************************************************************
141900940225     C     REPOPZ        BEGSR
142000940225     C*
142100940314     C     1             DO        $DIMOP        X
142200940128     C*
142300940223     C     $MS1(X)       IFNE      *BLANK
142400940128     C*
142500940223     C                   MOVEL     $MS1(X)       IDMSG
142600940128     C                   CALL      'XRTVM'
142700940201     C                   PARM                    IDMSG            27
142800940201     C                   PARM                    TXTMSG          100
142900940201     C                   PARM                    ERRMSG            1
143000940128     C*
143100940128     C     ERRMSG        IFNE      *ON
143200940223     C                   MOVEL     TXTMSG        $TX1(X)
143300940128     C                   ELSE
143400940223     C                   MOVEL     *ALL'?'       $TX1(X)
143500940128     C                   END
143600940128     C*
143700940128     C                   END
143800940128     C*
143900940128     C                   END
144000940128     C*
144100940128     C                   ENDSR
144200940506     C/EJECT
144300940506     C************************************************************
144400940506     C* REPERIMENTO TASTI DI FUNZIONE
144500940506     C************************************************************
144600940506     C     REPKEY        BEGSR
144700940506     C*
144800940506     C                   MOVEA     $FM2          $FM
144900940506     C                   EXSR      REPKKK
145000940506     C                   MOVEA     $FX           $FX2
145100940506     C                   ENDSR
145200940506     C/EJECT
145300940506     C************************************************************
145400940506     C* REPERIMENTO TASTI DI FUNZIONE
145500940506     C************************************************************
145600940506     C     REPKKK        BEGSR
145700940506     C*
145800940506     C                   CLEAR                   $FX
145900940506     C     1             DO        24            X
146000940506     C*
146100940506     C     $FM(X)        IFNE      *BLANK
146200940506     C*
146300940506     C                   MOVEL     $FM(X)        IDMSG
146400940506     C                   CALL      'XRTVM'
146500940506     C                   PARM                    IDMSG            27
146600940506     C                   PARM                    TXTMSG          100
146700940506     C                   PARM                    ERRMSG            1
146800940506     C*
146900940506     C     ERRMSG        IFNE      *ON
147000940506     C                   MOVEL     TXTMSG        $FX(X)
147100940506     C                   ELSE
147200940506     C                   MOVEL     *ALL' '       $FX(X)
147300940506     C                   END
147400940506     C*
147500940506     C                   END
147600940506     C*
147700940506     C                   END
147800940506     C*
147900940506     C                   ENDSR
148000940223     C/EJECT
148100940128     C************************************************************
148200940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
148300940128     C************************************************************
148400940223     C     DEFCAM        BEGSR
148500940128     C*
148600940223     C* Variabili per gestione videate
148700940223     C*
148800940223     C                   MOVE      *BLANK        $GEST             2            fmt in gestione
148900940223     C                   MOVE      *BLANK        $FINE             1            fine pgm
149000940223     C                   MOVE      *BLANK        $INZS1            1            inizializzione S1
149100940223     C                   MOVE      *BLANK        $LASTG            2            fmt di provenienza
149200940223     C                   MOVE      *BLANK        $LASTV            2            ultima emissione
149300940223     C                   MOVE      *BLANK        $EFILE            1            memo fine file
149400940223     C                   MOVE      *BLANK        $ESCI             1            rich.uscita ciclo
149500940223     C                   MOVE      *BLANK        $RCDOK            1            rcd letto valido
149600940223     C*
149700940314     C* Variabili appoggio sempre presenti in tutte le anagrafiche
149800940223     C*
149900940223     C     *LIKE         DEFINE    C1RCD         S1NRR
150000940223     C     *LIKE         DEFINE    C1NRR         WSFL
150100940223     C     *LIKE         DEFINE    C1RCD         WMAX
150200940223     C     *LIKE         DEFINE    C1RCD         WPAG
150300940223     C     *LIKE         DEFINE    $INZS1        WINZS1                                     ne S1
150400940223     C                   MOVE      *BLANK        WWGRP             3
150500940223     C                   Z-ADD     0             CURR              2 0
150600940223     C                   Z-ADD     0             CURC              2 0
150700940223     C                   MOVE      *BLANK        WIN              99
150800940314     C     *LIKE         DEFINE    C1POS         W1POS
150900940620     C*
151000940721$022 C     K5TBT1        KLIST
151100940721$022 C                   KFLD                    TBTAZI
151200940721$022 C                   KFLD                    TBTCOD
151300940721$022 C                   KFLD                    TBTKEY
151400940721$022 C                   KFLD                    TBTLIN
151500940721$022 C                   KFLD                    TBTFLD
151600940714     C*
151700940714     C     KABL          KLIST
151800940714     C                   KFLD                    ABLCOD
151900940714     C                   KFLD                    ABLKEY
152000940714     C                   KFLD                    ABLAZI
152100940314     C*
152200940314     C*
152300940223     C* Indici
152400940223     C*
152500940223     C                   Z-ADD     0             X                 3 0
152600940223     C                   Z-ADD     0             Y                 3 0
152700940127     C*
152800940117     C                   ENDSR
152900940223     C/EJECT
153000940616**
153100940225PROMSG    *LIBL     OPZ0001
153200940225PROMSG    *LIBL     OPZ0002
153300940131PROMSG    *LIBL     OPZ0003
153400940131PROMSG    *LIBL     OPZ0004
153500940131PROMSG    *LIBL     OPZ0005
153600940224PROMSG    *LIBL     OPZ0006
153700940224PROMSG    *LIBL     OPZ0007
153800940224PROMSG    *LIBL     OPZ0008
153900940224PROMSG    *LIBL     OPZ0009
154000940714PROMSG    *LIBL     OPZ0010
154100940714PROMSG    *LIBL     OPZ0013
154200940714PROMSG    *LIBL     OPZ0012
154300940728PROMSG    *LIBL     OPZ0011
154400940728PROMSG    *LIBL     OPZ0014
154500940201**
15460094022501S      <Selezione
15470094022502S      <Modifica
15480094022503S      <Copia
15490094022504S      <Annullamento
15500094022505S      <Visualizzazione
15510094060706N      <prova
15520094060707N      <prova
15530094060708N      <prova
15540094060709N      <prova
15550094072910S      <Invia
15560094071411S      <Abilita gruppo
15570094071412S      <Disabilita
15580094072813N      <PROVA
15590094083114N      <Commenti
156000940506** TASTI DI FUNZIONE  S1
156100940506PROMSG    *LIBL     KEY0001  01
156200940506PROMSG    *LIBL     KEY0002  02
156300940506PROMSG    *LIBL     KEY0003  03
156400940506PROMSG    *LIBL     KEY0004  04
156500940506PROMSG    *LIBL     KEY0005  05
156600940506PROMSG    *LIBL     KEY0006  06
156700940506PROMSG    *LIBL     KEY0007  07
156800940506PROMSG    *LIBL     KEY0008  08
156900940506PROMSG    *LIBL     KEY0009  09
157000940506PROMSG    *LIBL     KEY0010  10
157100940506PROMSG    *LIBL     KEY0011  11
157200940506PROMSG    *LIBL     KEY0012  12
157300940506PROMSG    *LIBL     KEY0013  13
157400940506PROMSG    *LIBL     KEY0014  14
157500940506PROMSG    *LIBL     KEY0015  15
157600940506PROMSG    *LIBL     KEY0016  16
157700940506PROMSG    *LIBL     KEY0017  17
157800940506PROMSG    *LIBL     KEY0018  18
157900940506PROMSG    *LIBL     KEY0019  19
158000940506PROMSG    *LIBL     KEY0020  20
158100940506PROMSG    *LIBL     KEY0021  21
158200940506PROMSG    *LIBL     KEY0022  22
158300940506PROMSG    *LIBL     KEY0023  23
158400940506PROMSG    *LIBL     KEY0024  24
158500940506**  ABILITAZIONE S1
1586009410140N  01
1587009407200N  02
1588009407201N  03
1589009407200N  04
1590009407200N  05
1591009407200N  06
1592009407200N  07
1593009407200N  08
1594009407200N  09
1595009407201N  10
1596009407200N  11
1597009407201N  12
1598009407200N  13
1599009407200N  14
1600009407200N  15
1601009407200N  16
1602009407200N  17
1603009407200N  18
1604009407200N  19
1605009407200N  20
1606009407200N  21
1607009407200N  22
1608009407201N  23
1609009407200N  24
161000940616**
161100940616DES   Descrizione ................
161200940616DEB   Descrizione breve ..........
161300940616**
161400940616001030
161500940616031008
