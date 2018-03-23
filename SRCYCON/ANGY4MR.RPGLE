000100970214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200970214     H*PARMS CVTOPT(*DATETIME)
000300940211     H DECEDIT('0,') DATEDIT(*DMY.)
000400940224      *----------*
000500980722    > * ANGY4MR  *
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
003800980722$022 F*NTBT01L  IF A E           K DISK
003900940715$001 FANGY4MD   CF   E             WORKSTN USROPN
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
014200940607$003 D EXTDS         E DS                  EXTNAME(ANGY4MDS)
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
018200980722$015 I              S1CD1                       §4MCD1
018300980722$015 I              S1CD2                       §4MCD2
018400980722$015 I              S1CD3                       §4MCD3
018500980722$015 I              S1IVA                       §4MIVA
018600980722$015 I              S1ESE                       §4MESE
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
025400940607$018 C                   PARM      35            $L01              3 0
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
028400940607$018 C                   PARM      35            $L01              3 0
028500941004$018 C                   PARM      35            $L02              3 0
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
068700980722     C*
068800950502     C                   MOVEL     SAVUNI        EXTDS
068900980731     C                   MOVEl     SAVKEY        S1COD
069000950502     C                   MOVE      SAVGRU        S1GRU
069100970512     C                   MOVE      SAVUNT        S1UNITA           8
069200950502     C     *LIKE         DEFINE    XTBUNI        SAVUNI
069300950502     C     *LIKE         DEFINE    XTBKEY        SAVKEY
069400950502     C     *LIKE         DEFINE    XTBGRU        SAVGRU
069500970509     C     *LIKE         DEFINE    XTBUNT        SAVUNT
069600940131     C*
069700940131     C                   ENDSR
069800940309     C/EJECT
069900940309     C************************************************************
070000940309     C* GESTIONE F03 VIDEO S1
070100940309     C************************************************************
070200940309     C     F03S1         BEGSR
070300940309     C*
070400940309     C                   MOVE      *ON           $FINE
070500970708     C                   MOVE      '1'           XtaRet
070600940325     C* fine programma
070700940325     C                   EXSR      ENDPGM
070800940309     C*
070900940309     C                   ENDSR
071000940309     C/EJECT
071100940309     C************************************************************
071200940309     C* GESTIONE F10 VIDEO S1
071300940314     c* AGGIUNTA RECORD
071400940309     C************************************************************
071500940309     C     F10S1         BEGSR
071600940309     C*
071700940607     C                   RESET                   D002DS
071800940607     C                   MOVEL     '01'          OPZ002
071900970708     C                   MOVE      XtaLin        LIN002
072000970708     C                   MOVE      XtaAzi        AZI002
072100970512     C                   MOVE      S1UNITA       UNI002
072200940715     C                   MOVE      *BLANKS       KPJBU
072300940607     C                   MOVEL     D002DS        KPJBU
072400940608$004 C                   CALL      'ANGY4MR2'
072500940309     C                   PARM                    KPJBA
072600940607     C                   MOVEL     KPJBU         D002DS
072700940309     C* ritorno da PGM gestione
072800940309     C                   EXSR      GESRET
072900940309     C     WINZS1        IFEQ      *ON
073000940309     C                   MOVE      *ON           $INZS1
073100940309     C* se esistevano già righe sul sfl
073200940309     C* calcolo pagina a cui deve posizionarsi
073300940309     C     WSFL          IFGT      0
073400940309     C                   EXSR      CLCPAG
073500940309     C* altrimenti carico solo la 1a pagina
073600940309     C                   ELSE
073700940309     C                   Z-ADD     SFLPAG        WPAG
073800940309     C                   ENDIF
073900940309     C                   ENDIF
074000940309     C*
074100940712     C                   MOVE      *HIVAL        $LASTV
074200940309     C                   ENDSR
074300940309     C/EJECT
074400940309     C************************************************************
074500940309     C* GESTIONE F12 VIDEO S1
074600940309     C************************************************************
074700940309     C     F12S1         BEGSR
074800940309     C*
074900970708     C                   MOVE      '2'           XtaRet
075000940309     C                   MOVE      *ON           $FINE
075100940309     C*
075200940309     C                   ENDSR
075300940309     C/EJECT
075400940309     C************************************************************
075500940309     C* GESTIONE F23 VIDEO S1
075600940309     C************************************************************
075700940309     C     F23S1         BEGSR
075800940309     C*
075900940309     C                   EXSR      RIEOPZ
076000950502     C                   Z-ADD     23            X
076100950502     C                   MOVE      FF23          $FC2(X)
076200940608     C                   MOVEL     WOPZ1         C1OPZ1
076300940608     C**                   MOVELWOPZ2     C1OPZ2
076400940309     C                   Z-ADD     WCOZ          C1COZ
076500940309     C*
076600940309     C                   ENDSR
076700940309     C/EJECT
076800940309     C************************************************************
076900940309     C* GESTIONE F24 VIDEO S1
077000940309     C************************************************************
077100940309     C     F24S1         BEGSR
077200940309     C*
077300940506     C*
077400940506     C                   MOVEA     $FX2          $FX
077500940506     C                   MOVEA     $FC2          $FC
077600940506     C                   MOVEA     $FR2          $FR
077700940506     C                   Z-ADD     $ULKS1        $ULK
077800940506     C                   EXSR      RIEKEY
077900941017     C                   MOVEA     $FC           $FC2
078000940506     C                   Z-ADD     $ULK          $ULKS1            3 0
078100940607     C                   MOVEL     $KEY1         Z1KE1
078200941004     C                   MOVEL     $KEY2         Z1KE2
078300940607     C                   WRITE     Z1
078400940506     C*
078500940309     C                   ENDSR
078600940128     C/EJECT
078700940128     C************************************************************
078800940131     C* CONTROLLO TESTATA LISTA
078900940128     C************************************************************
079000940131     C     CTRC1         BEGSR
079100940128     C*
079200940201     C                   MOVE      *OFF          *IN99
079300940131     C*
079400940202     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
079500940202     C* L'INIZIALIZZAZIONE LISTA
079600940202     C*
079700940207     C     C1POS         IFNE      W1POS
079800940202     C                   EXSR      POSS1
079900940202     C                   END
080000940202     C*
080100940202     C                   ENDSR
080200940202     C/EJECT
080300940202     C************************************************************
080400940202     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
080500940202     C************************************************************
080600940202     C     POSS1         BEGSR
080700940202     C*
080800940202     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
080900940202     C*
0810009402021    C     WMAX          IFGT      0
081100940607     C                   READC     S1                                     21
0812009402022    C     *IN21         DOWEQ     *OFF
081300940202     C     *IN99         ANDEQ     *OFF
081400940202     C*
0815009402023    C     S1OPZ         IFNE      0
081600940202     C                   SETON                                        51  99
0817009402023-   C                   ENDIF
081800940202     C* se rilevata opzione attiva, attivo sflnxtchg
0819009402023    C     *IN99         IFEQ      *ON
082000940202     C                   MOVE      *ON           *IN32
082100940714     C                   MOVE      $IN41         *IN41
082200940607     C                   UPDATE    S1
0823009402023E   C                   ELSE
082400940607     C                   READC     S1                                     21
0825009402023-   C                   ENDIF
082600940202     C*
0827009402022-   C                   ENDDO
082800940202     C*
0829009402021-   C                   ENDIF
083000940202     C*
083100940221     C*
0832009402021    C     *IN99         IFEQ      *OFF
083300941121     C                   CLEAR                   W1POS
083400940202     C*
083500940302     C                   MOVEL     C1POS         W1POS
083600940131     C*
083700940202     C                   MOVE      *ON           *IN99
083800940223     C                   MOVE      *ON           $INZS1
083900940202     C*
084000940202     C*
0841009402021-   C                   ENDIF
084200940202     C*
084300940131     C                   ENDSR
084400940131     C/EJECT
084500940131     C************************************************************
084600940131     C* CONTROLLO OPZIONI LISTA
084700940131     C************************************************************
084800940131     C     CTRS1         BEGSR
084900940131     C*
085000940202     C                   MOVEL     *OFF          $ESCI
085100940201     C                   SETOFF                                       99
085200940131     C                   Z-ADD     0             S1OPZ
085300940131     C*
085400940127     C* Leggo il sfl solo se ci sono rcd
0855009401311    C     WMAX          IFGT      0
085600940607     C                   READC     S1                                     21
085700940127     C*
085800940131     C* esce se fine sfl o errore che richiede l'uscita
0859009401312    C     *IN21         DOWEQ     *OFF
086000940131     C     $ESCI         ANDEQ     *OFF
086100940201     C                   Z-ADD     S1NRR         C1RCD
086200940131     C* ctrl su riga
086300940131     C                   EXSR      RECS1
086400940131     C* gestione opzioni
0865009401313    C     S1OPZ         IFNE      0
086600940201     C     *IN99         ANDEQ     *OFF
086700940131     C                   EXSR      OPZS1
0868009401273-   C                   ENDIF
086900940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0870009402013    C     *IN99         IFEQ      *ON
087100940201     C     $ESCI         OREQ      *ON
087200940131     C                   MOVE      *ON           *IN32
087300940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
087400940204     C* la ripristinerò a conclusione del ciclo di READC
087500940223     C                   MOVE      *OFF          $INZS1
0876009402233-   C                   ENDIF
087700940223     C*
087800940131     C                   Z-ADD     0             S1OPZ
087900940223     C*
088000940714     C                   MOVE      $IN41         *IN41
088100940607     C                   UPDATE    S1
088200940223     C*
088300940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0884009401313    C     $ESCI         IFEQ      *OFF
088500940607     C                   READC     S1                                     21
088600940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0887009402014    C     *IN21         IFEQ      *ON
088800940201     C                   MOVE      WINZS1        $INZS1
088900940204     C* calcolo pagina a cui deve posizionarsi
089000940224     C                   EXSR      CLCPAG
0891009402014-   C                   ENDIF
0892009402013-   C                   ENDIF
089300940131     C*
0894009401272-   C                   ENDDO
089500940127     C*
0896009401311-   C                   ENDIF
089700940131     C*
089800940127     C                   ENDSR
089900940127     C/EJECT
090000940127     C************************************************************
090100940131     C* CONTROLLO CAMPI I/O RIGA LISTA
090200940127     C************************************************************
090300940131     C     RECS1         BEGSR
090400940131     C*
090500940201     C* reset indicatori DSPATR
090600940201     C                   MOVE      *ALL'0'       IN4049           10
090700940201     C                   MOVEA     IN4049        *IN(40)
090800940201     C*
090900940131     C* Controllo se Utente Autorizzato alla scelta
091000940202     C     S1OPZ         IFNE      0
091100940131     C                   Z-ADD     1             X
091200940223     C     S1OPZ         LOOKUP    $OP1(X)                                21
091300940211     C     *IN21         IFEQ      *OFF
091400940223     C     $FL1(X)       OREQ      'N'
091500940131     C                   MOVE      *ON           $ESCI
091600940201     C                   SETON                                        504099
091700940131     C                   ENDIF
091800940131     C*
091900940620     c* Controllo autorizzazione alla modifica su record di gruppo
092000940620     C* Reperisco tipo codice (gruppo/azienda)
092100980731     C                   MOVE      *blanks       XTBKEY
092200980731     C                   MOVEl     S1COD         XTBKEY
092300970512     C                   MOVE      S1UNITA       XTBUNT
092400940620     C                   MOVE      '0'           XTBALC
092500940620     C                   MOVE      '1'           XTBRIC
092600970214     C                   CALLB     'XATB'
092700940620     C                   PARM                    XATBDS
092800940620     C* Reperisco autorizzazioni utente
092900940620     C                   CLEAR                   XCHATB
093000941118     C                   MOVE      XSCSOC        XCTSOC
093100941118     C                   MOVE      XSCDUN        XCTUNI
093200940620     C                   MOVEL     KNMUS         XCTPRF
093300940620     C                   MOVE      XTBCOD        XCTCOD
093400940620     C                   CALL      'XCHKATB'
093500940620     C                   PARM                    XCHATB
093600941111     C                   MOVEL     XCTDDS        AUTDS
093700941027     C     XCTRTN        IFEQ      1
093800941027     C                   MOVEL     'N'           MANGRU
093900941027     C                   ENDIF
094000940620     C* Non ammessa manutenzione di gruppo
094100940620   1 C     XTBGRU        IFEQ      'G'
094200940620     C     MANGRU        ANDEQ     'N'
094300940620   2 C     S1OPZ         IFEQ      2
094400940729     C     S1OPZ         OREQ      3
094500940620     C     S1OPZ         OREQ      4
094600940620     C                   SETON                                        5399
094700940620   2 C                   ENDIF
094800940620   1 C                   ENDIF
094900940620     C* Non ammessa manutenzione di azienda
095000940620   1 C     XTBGRU        IFEQ      'A'
095100940620     C     MANAZI        ANDEQ     'N'
095200940620   2 C     S1OPZ         IFEQ      2
095300940729     C     S1OPZ         OREQ      3
095400940620     C     S1OPZ         OREQ      4
095500940620     C                   SETON                                        5499
095600940620   2 C                   ENDIF
095700940620   1 C                   ENDIF
095800940607     C                   ENDIF
095900940131     C                   ENDSR
096000940224     C/EJECT
096100940131     C************************************************************
096200940131     C* GESTIONE OPZIONI LISTA
096300940131     C************************************************************
096400940131     C     OPZS1         BEGSR
096500940201     C*
0966009402251    C                   SELECT
096700940225     C*
096800940225     C* se opz=Selezione passo la chiave al pgm chiamante
096900940225     C     S1OPZ         WHENEQ    1
097000970708     C                   MOVE      S1OPZ         XtaOpz
097100980731     C                   MOVEl     S1COD         XtaKey
097200970708     C                   MOVE      S1UNIta       xtaUni
097300940225     C                   MOVE      *ON           $ESCI
097400940225     C                   MOVE      *ON           $FINE
097500940729     C*
097600940714     C*
097700940714     C* se opz=Abilita scrive su archivio abilitazioni
097800940714     C     S1OPZ         WHENEQ    11
097900941103     C                   MOVE      XSCSOC        ABLAZI
098000980731     C                   MOVE      *blanks       ABLKEY
098100980731     C                   MOVEl     S1COD         ABLKEY
098200970512     C                   MOVE      S1UNITA       ABLUNT
098300980731     C                   MOVE      XTBCOD        ABLCOD
098400940714     C                   WRITE     ANABL000                             22
098500940714     C*
098600940714     C* se opz=Disabil.cancella su archivio abilitazioni
098700940714     C     S1OPZ         WHENEQ    12
098800941103     C                   MOVE      XSCSOC        ABLAZI
098900980731     C                   MOVE      *blanks       ABLKEY
099000980731     C                   MOVEl     S1COD         ABLKEY
099100970512     C                   MOVE      S1UNITA       ABLUNT
099200940714     C                   MOVE      XTBCOD        ABLCOD
099300940714     C     KABL          DELETE    ANABL000                           22
099400940729     C*
099500940225     C*
099600940225     C* se altre opzioni (o nessuna opzione) richiamo pgm gestione
0997009402251O   C                   OTHER
099800940225     C*
099900940616     C                   RESET                   D002DS
100000940607     C                   MOVEL     S1OPZ         OPZ002
100100980731     C                   MOVE      *blanks       KEY002
100200980731     C                   MOVEl     S1COD         KEY002
100300970708     C                   MOVE      XtaLin        LIN002
100400970708     C                   MOVE      XtaAzi        AZI002
100500940715     C                   MOVE      *BLANKS       KPJBU
100600940607     C                   MOVEL     D002DS        KPJBU
100700940608$004 C                   CALL      'ANGY4MR2'
100800940607     C                   PARM                    KPJBA
100900940607     C                   MOVEL     KPJBU         D002DS
101000940201     C*
101100940223     C* ritorno da PGM gestione
101200940223     C                   EXSR      GESRET
101300940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
1014009402252    C     *IN99         IFEQ      *ON
101500940223     C                   SETON                                        40
1016009402252-   C                   ENDIF
101700940131     C*
101800940506     C                   MOVE      *HIVAL        $LASTV
1019009402251-   C                   ENDSL
102000940225     C*
102100940131     C                   ENDSR
102200940223     C/EJECT
102300940223     C************************************************************
102400940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
102500940223     C************************************************************
102600940223     C     GESRET        BEGSR
102700940223     C*
102800940223     C* modo di ritorno
102900940223     C*
1030009402231    C                   SELECT
103100940314    >C* << questi modi di utilizzo dei valori di ritorno dal
103200940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
103300940314    >C*    proposte, normalmente sempre valide, ma modificabili
103400940314    >C*    per situazioni particolari >>
103500940223     C* 1 = F3
103600940314    >C     RET002        WHENEQ    '1'
103700940224     C                   MOVE      *ON           $ESCI
103800940223     C                   MOVE      *ON           $FINE
103900940223     C* 2 = F12
104000940314    >C     RET002        WHENEQ    '2'
104100940223     C                   MOVE      *ON           $ESCI
104200940223     C*
1043009402231-   C                   ENDSL
104400940223     C*
104500940223     C* operazione eseguite dal pgm chiamato
104600940223     C*
1047009402231    C                   SELECT
104800940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
104900940314    >C     OPR002        WHENEQ    '1'
105000940223     C                   MOVE      *ON           WINZS1
105100940223     C*
1052009402231-   C                   ENDSL
105300940223     C*
105400940223     C* funzione non eseguibile per errore :
105500940223     C*
1056009402231    C                   SELECT
105700940223     C* 1 = funzione richiamata chiusa in errore
105800940316    >C*  eventualmente gestire altri codici di errore
105900940314    >C     ERR002        WHENEQ    '1'
106000940223     C                   MOVE      *ON           $ESCI
106100940223     C                   SETON                                        5299
106200940223     C*
1063009402231-   C                   ENDSL
106400940223     C*
106500940223     C                   ENDSR
106600940223     C/EJECT
106700940131     C************************************************************
106800940131     C* OPERAZIONI INIZIALI
106900940131     C************************************************************
107000940131     C     *INZSR        BEGSR
107100940131     C*
107200940127     C* Reperimento parametri
107300940127     C*
107400940117     C     *ENTRY        PLIST
107500940117     C                   PARM                    KPJBA
107600940714     C*
107700940714     C* Reperimento Codice utente
107800940714     C*
107900950622     C*          KNMUS     CHAINANUTN01L             21
108000940225     C*
108100940223     C* Reperimento opzioni
108200940127     C*
108300940128     C                   EXSR      REPOPZ
108400940506     C*
108500940506     C* Reperimento tasti di funzione
108600940506     C*
108700940506     C                   EXSR      REPKEY
108800940506     C*
108900940607$007 C                   MOVE      'Y4M'         XTBCOD
109000940127     C*
109100940117     C                   ENDSR
109200940225     C/EJECT
109300941103      ************************************************************
109400941103      * Reperimento dati società
109500941103      ************************************************************
109600941103     C     REPSOC        BEGSR
109700941103      *
109800970214     C                   CALLB     'XSOC'
109900941103     C                   PARM                    TIPXSC            6
110000941103     C                   PARM                    SOCXSC            3
110100941103     C                   PARM                    CDSXSC            9 0
110200941103     C                   PARM                    MODXSC            3
110300941103     C                   PARM      *BLANKS       RTNXSC            1
110400941103     C                   PARM                    XSOCDS
110500941103     C                   PARM                    KPJBA
110600941103      *
110700941103     C                   ENDSR
110800940223     C************************************************************
110900940223     C* INIZIALIZZAZIONE VARIABILI
111000940223     C************************************************************
111100940223     C     INZVAR        BEGSR
111200970926     C*
111300970926     C* Dati da PGM chiamante
111400970926     C*
111500970926     C                   MOVEL     KPJBU         XTABDS
111600951124     C*
111700951124     C* Reperimento codice lingua e codice azienda
111800951124     C*
111900951124     C                   MOVEL     'SOC001'      TIPXSC
112000951124     C                   EXSR      REPSOC
112100951124     C     RTNXSC        IFNE      '1'
112200951124     C                   MOVEL     XSOCDS        SOC001
112300951124     C                   ELSE
112400951124     C                   MOVE      *ON           $FINE
112500951124     C* fine programma
112600951124     C                   EXSR      ENDPGM
112700951124     C                   ENDIF
112800951124     C* Lingua e codice azienda
112900970708     C                   MOVE      XSCLIN        XtaLin
113000970708     C                   MOVE      XSCSOC        XtaAzi
113100940715     C*
113200940715     C                   RESET                   $FC2
113300940715     C*
113400940715$021 C                   CLOSE     ANGY4MD
113500940715$021 C                   OPEN      ANGY4MD
113600940506     C*
113700940506     C* Definizione delle opzioni utilizzabili a pgm
113800940506     C*
113900940506     C                   EXSR      DEFOPZ
114000940718     C* Pulizia campi e indicatori
114100940718     C                   CLEAR                   W1POS
114200940718     C                   MOVE      *ALL'0'       IN4049           10
114300940718     C                   MOVEA     IN4049        *IN(40)
114400940718     C                   CLEAR                   S1OPZ
114500940223     C* Variabili per gestione videate
114600940223     C*
114700940223     C                   MOVE      *OFF          $FINE
114800940223     C                   MOVE      *OFF          $INZS1
114900940223     C                   MOVE      *OFF          $EFILE
115000940223     C                   MOVE      *OFF          $ESCI
115100940223     C                   MOVE      *OFF          $RCDOK
115200940509     C                   MOVE      *HIVAL        $LASTV
115300940506     C                   Z-ADD     0             $ULKS1            3 0
115400940223     C*
115500940318     C                   MOVE      *ON           $INZS1
115600940223     C                   MOVE      'S1'          $GEST
115700940223     C*
115800940223     C* Variabili appoggio
115900940223     C*
116000940223     C                   Z-ADD     SFLPAG        WPAG
116100940223     C*
116200940223     C* Valorizzazione campi univoci testate
116300940223     C*
116400940607     C*                    CLEART1
116500940607     C*                    MOVELKNSIF     NOMSIF
116600940607     C*                    MOVELRAGUTE    NOMDIT
116700940607     C*                    MOVELDSPGM     SIGLA
116800940607     C*                    MOVELPROGR     NOMPGM
116900940223     C*
117000940608     c* posizione window
117100940608$012 C                   Z-ADD     37            PCOL              2 0
117200940613     C                   Z-ADD     09            R
117300940608     C     80            SUB       PCOL          C
117400940608     C                   SUB       3             C
117500970708     C     XtaOpz        IFNE      '  '
117600970708     C     XtaRig        IFGT      11
117700940608     C                   Z-ADD     1             R
117800940608     C                   ENDIF
117900940608     C*
118000970708     C     XtaCol        IFGT      40
118100940608     C                   Z-ADD     2             C
118200940608     C                   ENDIF
118300940608     C                   ENDIF
118400940616     C*
118500970708     C     XtaOpz        IFNE      '01'
118600940616$019 C                   DO        00            I                 3 0
118700940721$022 C*                  MOVEL     LIN(I)        TBTFLD
118800940721$022 C*                  CLEAR                   TBTKEY
118900940721$022 C*                  CLEAR                   TBTAZI
119000940721$022 C*                  CLEAR                   TBTLIN
119100940721$007 C                   MOVE      'Y4M'         TBTCOD            3
119200940721$022 C*    K5TBT1        CHAIN     ANTBT01L                           21
119300940721$022 C*    *IN21         IFEQ      *ON
119400940721$022 C*                  Z-ADD     LLUN(I)       TBTLUN
119500940721$022 C*                  Z-ADD     LPOS(I)       TBTPOS
119600940721$022 C*                  MOVEL     LIND(I)       TBTDES
119700940721$022 C*                  WRITE     ANTBT000
119800940721$022 C*                  ENDIF
119900940616     C                   ENDDO
120000940616     C                   ENDIF
120100940608     C*
120200940722     c*
120300940722     c* Se richiamato con OPZ 11  Non controlla abilitazione all'aziend
120400970708     C     XtaOpz        IFEQ      '11'
120500940722     C                   MOVE      'N'           XTBGAB
120600940722     C                   ELSE
120700940722     C                   MOVE      'S'           XTBGAB
120800940722     C                   ENDIF
120900940223     C                   ENDSR
121000940223     C/EJECT
121100940131     C************************************************************
121200940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
121300940131     C************************************************************
121400940225     C     DEFOPZ        BEGSR
121500940131     C*
121600940225     C* Questa routine deve valorizzare la schiera $FL1
121700940225     C* con S o N a seconda della utilizzabilità o meno
121800940225     C* dell'opzione correlata
121900940225     C*
122000940714     C* Inizialmente si considerano tutte le opzioni come non valide
122100940728     C     1             DO        14            X
122200940714     C                   MOVE      'N'           $FL1(X)
122300940225     C                   END
122400940225     C*
122500940225     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
122600940714     C* rendo valide alcune opzioni
122700940714     C* i tasti invece li disattivo
122800940225     C                   SELECT
122900940714     C*
123000940714     C* Se pgm richiamato in RICERCA attivo :
123100940714     C*
123200970708    >C     XtaOpz        WHENEQ    '01'
123300940714     C* F3=Fine
123400940714     C                   Z-ADD     3             X
123500940720     C                   MOVE      '0'           $FC2(X)
123600940714     C* F10=Immissione
123700940714     C                   Z-ADD     10            X
123800940919     C                   MOVE      '1'           $FC2(X)
123900940714     C* 1=Selezione
124000940714     C                   Z-ADD     1             X
124100940714     C     01            LOOKUP    $OP1(X)                                21
124200940714     C     *IN21         IFEQ      *ON
124300940714     C                   MOVE      'S'           $FL1(X)
124400940714     C                   ENDIF
124500940729     C* 2=Modifica
124600940729     C                   Z-ADD     1             X
124700940729     C     02            LOOKUP    $OP1(X)                                21
124800940729     C     *IN21         IFEQ      *ON
124900940729     C                   MOVE      'S'           $FL1(X)
125000940729     C                   ENDIF
125100940729     C* 3=Copia
125200940729     C                   Z-ADD     1             X
125300940729     C     03            LOOKUP    $OP1(X)                                21
125400940729     C     *IN21         IFEQ      *ON
125500940729     C                   MOVE      'S'           $FL1(X)
125600940729     C                   ENDIF
125700940729     C* 4=Annullamento
125800940729     C                   Z-ADD     1             X
125900940729     C     04            LOOKUP    $OP1(X)                                21
126000940729     C     *IN21         IFEQ      *ON
126100940729     C                   MOVE      'S'           $FL1(X)
126200940729     C                   ENDIF
126300940714     C* 5=Visualizzazione
126400940714     C                   Z-ADD     1             X
126500940714     C     05            LOOKUP    $OP1(X)                                21
126600940714     C     *IN21         IFEQ      *ON
126700940714     C                   MOVE      'S'           $FL1(X)
126800940714     C                   ENDIF
126900940728     C*14=Commenti
127000940831     C**                   Z-ADD1         X
127100940831     C**         14        LOKUP$OP1,X                   21
127200940831     C**         *IN21     IFEQ *ON
127300940831     C**                   MOVE 'S'       $FL1,X
127400940831     C**                   ENDIF
127500940714     C*
127600940714     C* Se pgm richiamato in INTERROGAZIONE attivo :
127700940714     C*
127800970708    >C     XtaOpz        WHENEQ    '05'
127900940714     C* F10=Immissione
128000940714     C                   Z-ADD     10            X
128100940720     C                   MOVE      '0'           $FC2(X)
128200940714     C* F3=Fine
128300940714     C                   Z-ADD     3             X
128400940720     C                   MOVE      '0'           $FC2(X)
128500940714     C* 5=Visualizzazione
128600940225     C                   Z-ADD     1             X
128700940714     C     05            LOOKUP    $OP1(X)                                21
128800940225     C     *IN21         IFEQ      *ON
128900940714     C                   MOVE      'S'           $FL1(X)
129000940225     C                   ENDIF
129100940728     C*14=Commenti
129200940831     C**                   Z-ADD1         X
129300940831     C**         14        LOKUP$OP1,X                   21
129400940831     C**         *IN21     IFEQ *ON
129500940831     C**                   MOVE 'S'       $FL1,X
129600940831     C**                   ENDIF
129700940831     C*
129800940714     C* Se pgm richiamato in ABILITAZIONE attivo :
129900940714     C*
130000970708    >C     XtaOpz        WHENEQ    '11'
130100940714     C* F10=Immissione
130200940714     C                   Z-ADD     10            X
130300940720     C                   MOVE      '0'           $FC2(X)
130400940714     C* F3=Fine
130500940714     C                   Z-ADD     3             X
130600940720     C                   MOVE      '0'           $FC2(X)
130700940714     C* 5=Visualizzazione
130800940714     C                   Z-ADD     1             X
130900940714     C     05            LOOKUP    $OP1(X)                                21
131000940714     C     *IN21         IFEQ      *ON
131100940714     C                   MOVE      'S'           $FL1(X)
131200940714     C                   ENDIF
131300940714     C* 11=Abilitazione
131400940714     C                   Z-ADD     1             X
131500940714     C     11            LOOKUP    $OP1(X)                                21
131600940714     C     *IN21         IFEQ      *ON
131700940714     C                   MOVE      'S'           $FL1(X)
131800940714     C                   ENDIF
131900940714     C* 12=Disabilita
132000940714     C                   Z-ADD     1             X
132100940714     C     12            LOOKUP    $OP1(X)                                21
132200940714     C     *IN21         IFEQ      *ON
132300940714     C                   MOVE      'S'           $FL1(X)
132400940714     C                   ENDIF
132500940714     C*
132600940714     C* Se pgm richiamato in GESTIONE  attivo :
132700940714     C*
132800970708    >C     XtaOpz        WHENEQ    '02'
132900940714     C* F3=Fine
133000940714     C                   Z-ADD     3             X
133100940720     C                   MOVE      '0'           $FC2(X)
133200940714     C* 2=Modifica
133300940714     C                   Z-ADD     1             X
133400940714     C     02            LOOKUP    $OP1(X)                                21
133500940714     C     *IN21         IFEQ      *ON
133600940714     C                   MOVE      'S'           $FL1(X)
133700940714     C                   ENDIF
133800940714     C* 3=Copia
133900940714     C                   Z-ADD     1             X
134000940714     C     03            LOOKUP    $OP1(X)                                21
134100940714     C     *IN21         IFEQ      *ON
134200940714     C                   MOVE      'S'           $FL1(X)
134300940714     C                   ENDIF
134400940714     C* 4=Annullamento
134500940714     C                   Z-ADD     1             X
134600940714     C     04            LOOKUP    $OP1(X)                                21
134700940714     C     *IN21         IFEQ      *ON
134800940714     C                   MOVE      'S'           $FL1(X)
134900940714     C                   ENDIF
135000940714     C* 5=Visualizzazione
135100940714     C                   Z-ADD     1             X
135200940714     C     05            LOOKUP    $OP1(X)                                21
135300940714     C     *IN21         IFEQ      *ON
135400940714     C                   MOVE      'S'           $FL1(X)
135500940714     C                   ENDIF
135600940729     C*14=Commenti
135700940831     C**                   Z-ADD1         X
135800940831     C**         14        LOKUP$OP1,X                   21
135900940831     C**         *IN21     IFEQ *ON
136000940831     C**                   MOVE 'S'       $FL1,X
136100940831     C**                   ENDIF
136200940225     C*
136300940225     C                   OTHER
136400940714     C* Se pgm non richiamato, attivo TUTTO  :
136500940714     C* 2=Modifica
136600940714     C                   Z-ADD     1             X
136700940714     C     02            LOOKUP    $OP1(X)                                21
136800940714     C     *IN21         IFEQ      *ON
136900940714     C                   MOVE      'S'           $FL1(X)
137000940714     C                   ENDIF
137100940714     C* 3=Copia
137200940714     C                   Z-ADD     1             X
137300940714     C     03            LOOKUP    $OP1(X)                                21
137400940714     C     *IN21         IFEQ      *ON
137500940714     C                   MOVE      'S'           $FL1(X)
137600940714     C                   ENDIF
137700940714     C* 4=Annullamento
137800940714     C                   Z-ADD     1             X
137900940714     C     04            LOOKUP    $OP1(X)                                21
138000940714     C     *IN21         IFEQ      *ON
138100940714     C                   MOVE      'S'           $FL1(X)
138200940714     C                   ENDIF
138300940714     C* 5=Visualizzazione
138400940714     C                   Z-ADD     1             X
138500940714     C     05            LOOKUP    $OP1(X)                                21
138600940714     C     *IN21         IFEQ      *ON
138700940714     C                   MOVE      'S'           $FL1(X)
138800940714     C                   ENDIF
138900940714     C* 11=Abilitazione
139000940714     C                   Z-ADD     1             X
139100940714     C     11            LOOKUP    $OP1(X)                                21
139200940714     C     *IN21         IFEQ      *ON
139300940714     C                   MOVE      'S'           $FL1(X)
139400940714     C                   ENDIF
139500940714     C* 12=Disabilita
139600940714     C                   Z-ADD     1             X
139700940714     C     12            LOOKUP    $OP1(X)                                21
139800940714     C     *IN21         IFEQ      *ON
139900940714     C                   MOVE      'S'           $FL1(X)
140000940714     C                   ENDIF
140100940728     C*14=Commenti
140200940831     C**                   Z-ADD1         X
140300940831     C**         14        LOKUP$OP1,X                   21
140400940831     C**         *IN21     IFEQ *ON
140500940831     C**                   MOVE 'S'       $FL1,X
140600940831     C**                   ENDIF
140700940225     C* poi, disattivo a seconda delle autorizzazioni :
140800940314    >C*********  AUTXXX    IFEQ 'N'
140900940314    >C*********            Z-ADD1         X
141000940314    >C*********  'XX'      LOKUP$OP1,X                   21
141100940314    >C*********  *IN21     IFEQ *ON
141200940314    >C*********            MOVE 'N'       $FL1,X
141300940314    >C*********            ENDIF
141400940314    >C*********            ENDIF
141500940225     C*
141600940225     C                   ENDSL
141700940225     C*
141800940225     C                   ENDSR
141900940225     C/EJECT
142000940225     C************************************************************
142100940225     C* REPERIMENTO OPZIONI
142200940225     C************************************************************
142300940225     C     REPOPZ        BEGSR
142400940225     C*
142500940314     C     1             DO        $DIMOP        X
142600940128     C*
142700940223     C     $MS1(X)       IFNE      *BLANK
142800940128     C*
142900940223     C                   MOVEL     $MS1(X)       IDMSG
143000970214     C                   CALLB     'XRTVM'
143100940201     C                   PARM                    IDMSG            27
143200940201     C                   PARM                    TXTMSG          100
143300940201     C                   PARM                    ERRMSG            1
143400940128     C*
143500940128     C     ERRMSG        IFNE      *ON
143600940223     C                   MOVEL     TXTMSG        $TX1(X)
143700940128     C                   ELSE
143800940223     C                   MOVEL     *ALL'?'       $TX1(X)
143900940128     C                   END
144000940128     C*
144100940128     C                   END
144200940128     C*
144300940128     C                   END
144400940128     C*
144500940128     C                   ENDSR
144600940506     C/EJECT
144700940506     C************************************************************
144800940506     C* REPERIMENTO TASTI DI FUNZIONE
144900940506     C************************************************************
145000940506     C     REPKEY        BEGSR
145100940506     C*
145200940506     C                   MOVEA     $FM2          $FM
145300940506     C                   EXSR      REPKKK
145400940506     C                   MOVEA     $FX           $FX2
145500940506     C                   ENDSR
145600940506     C/EJECT
145700940506     C************************************************************
145800940506     C* REPERIMENTO TASTI DI FUNZIONE
145900940506     C************************************************************
146000940506     C     REPKKK        BEGSR
146100940506     C*
146200940506     C                   CLEAR                   $FX
146300940506     C     1             DO        24            X
146400940506     C*
146500940506     C     $FM(X)        IFNE      *BLANK
146600940506     C*
146700940506     C                   MOVEL     $FM(X)        IDMSG
146800970214     C                   CALLB     'XRTVM'
146900940506     C                   PARM                    IDMSG            27
147000940506     C                   PARM                    TXTMSG          100
147100940506     C                   PARM                    ERRMSG            1
147200940506     C*
147300940506     C     ERRMSG        IFNE      *ON
147400940506     C                   MOVEL     TXTMSG        $FX(X)
147500940506     C                   ELSE
147600940506     C                   MOVEL     *ALL' '       $FX(X)
147700940506     C                   END
147800940506     C*
147900940506     C                   END
148000940506     C*
148100940506     C                   END
148200940506     C*
148300940506     C                   ENDSR
148400940223     C/EJECT
148500940128     C************************************************************
148600940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
148700940128     C************************************************************
148800940223     C     DEFCAM        BEGSR
148900940128     C*
149000940223     C* Variabili per gestione videate
149100940223     C*
149200940223     C                   MOVE      *BLANK        $GEST             2
149300940223     C                   MOVE      *BLANK        $FINE             1
149400940223     C                   MOVE      *BLANK        $INZS1            1
149500940223     C                   MOVE      *BLANK        $LASTG            2
149600940223     C                   MOVE      *BLANK        $LASTV            2
149700940223     C                   MOVE      *BLANK        $EFILE            1
149800940223     C                   MOVE      *BLANK        $ESCI             1
149900940223     C                   MOVE      *BLANK        $RCDOK            1
150000940223     C*
150100940314     C* Variabili appoggio sempre presenti in tutte le anagrafiche
150200940223     C*
150300940223     C     *LIKE         DEFINE    C1RCD         S1NRR
150400940223     C     *LIKE         DEFINE    C1NRR         WSFL
150500940223     C     *LIKE         DEFINE    C1RCD         WMAX
150600940223     C     *LIKE         DEFINE    C1RCD         WPAG
150700940223     C     *LIKE         DEFINE    $INZS1        WINZS1
150800940223     C                   MOVE      *BLANK        WWGRP             3
150900940223     C                   Z-ADD     0             CURR              2 0
151000940223     C                   Z-ADD     0             CURC              2 0
151100940223     C                   MOVE      *BLANK        WIN              99
151200940314     C     *LIKE         DEFINE    C1POS         W1POS
151300940620     C*
151400940721$022 C*    K5TBT1        KLIST
151500940721$022 C*                  KFLD                    TBTAZI
151600940721$022 C*                  KFLD                    TBTCOD
151700940721$022 C*                  KFLD                    TBTKEY
151800940721$022 C*                  KFLD                    TBTLIN
151900940721$022 C*                  KFLD                    TBTFLD
152000940714     C*
152100940714     C     KABL          KLIST
152200940714     C                   KFLD                    ABLCOD
152300940714     C                   KFLD                    ABLKEY
152400940714     C                   KFLD                    ABLAZI
152500940314     C*
152600940314     C*
152700940223     C* Indici
152800940223     C*
152900940223     C                   Z-ADD     0             X                 3 0
153000940223     C                   Z-ADD     0             Y                 3 0
153100940127     C*
153200940117     C                   ENDSR
153300940223     C/EJECT
153400940616**
153500940225PROMSG    *LIBL     OPZ0001
153600940225PROMSG    *LIBL     OPZ0002
153700940131PROMSG    *LIBL     OPZ0003
153800940131PROMSG    *LIBL     OPZ0004
153900940131PROMSG    *LIBL     OPZ0005
154000940224PROMSG    *LIBL     OPZ0006
154100940224PROMSG    *LIBL     OPZ0007
154200940224PROMSG    *LIBL     OPZ0008
154300940224PROMSG    *LIBL     OPZ0009
154400970716PROMSG    *LIBL     OPZ0010
154500970716PROMSG    *LIBL     OPZ0013
154600970716PROMSG    *LIBL     OPZ0012
154700970716PROMSG    *LIBL     OPZ0011
154800970716PROMSG    *LIBL     OPZ0014
154900940201**
15500094022501S      <Selezione
15510094022502S      <Modifica
15520094022503S      <Copia
15530094022504S      <Annullamento
15540094022505S      <Visualizzazione
15550094060706N      <prova
15560094060707N      <prova
15570094060708N      <prova
15580094060709N      <prova
15590094072910S      <Invia
15600094071411S      <Abilita gruppo
15610094071412S      <Disabilita
15620094072813N      <PROVA
15630094083114N      <Commenti
156400940506** TASTI DI FUNZIONE  S1
156500940506PROMSG    *LIBL     KEY0001  01
156600940506PROMSG    *LIBL     KEY0002  02
156700940506PROMSG    *LIBL     KEY0003  03
156800940506PROMSG    *LIBL     KEY0004  04
156900940506PROMSG    *LIBL     KEY0005  05
157000940506PROMSG    *LIBL     KEY0006  06
157100940506PROMSG    *LIBL     KEY0007  07
157200940506PROMSG    *LIBL     KEY0008  08
157300940506PROMSG    *LIBL     KEY0009  09
157400970716PROMSG    *LIBL     KEY0010  10
157500970716PROMSG    *LIBL     KEY0011  11
157600970716PROMSG    *LIBL     KEY0012  12
157700970716PROMSG    *LIBL     KEY0013  13
157800970716PROMSG    *LIBL     KEY0014  14
157900970716PROMSG    *LIBL     KEY0015  15
158000970716PROMSG    *LIBL     KEY0016  16
158100970716PROMSG    *LIBL     KEY0017  17
158200970716PROMSG    *LIBL     KEY0018  18
158300970716PROMSG    *LIBL     KEY0019  19
158400940506PROMSG    *LIBL     KEY0020  20
158500940506PROMSG    *LIBL     KEY0021  21
158600940506PROMSG    *LIBL     KEY0022  22
158700940506PROMSG    *LIBL     KEY0023  23
158800940506PROMSG    *LIBL     KEY0024  24
158900940506**  ABILITAZIONE S1
1590009410140N  01
1591009407200N  02
1592009407201N  03
1593009407200N  04
1594009407200N  05
1595009407200N  06
1596009407200N  07
1597009407200N  08
1598009407200N  09
1599009407201N  10
1600009407200N  11
1601009407201N  12
1602009407200N  13
1603009407200N  14
1604009407200N  15
1605009407200N  16
1606009407200N  17
1607009407200N  18
1608009407200N  19
1609009407200N  20
1610009407200N  21
1611009407200N  22
1612009505020N  23
1613009407200N  24
