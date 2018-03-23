000100100120     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200100120     H*PARMS CVTOPT(*DATETIME)
000300100120     H DECEDIT('0,') DATEDIT(*DMY.)
000400100120      *
000500100120      *
000600100120      *          ------------------------------
000700160805    > *               TABELLA HK1
000800100120      *          ------------------------------
000801161020R235  * Pgm creato con il task R235 CU Percipiente in regime agevolato
000802161020R235  *
000900100120     F*----------------------------------------------------*
001000100120     FANTBT01L  IF A E           K DISK
001100160805     FSDGHK1D   CF   E             WORKSTN USROPN
001200100120     F                                     SFILE(S1:S1NRR)
001300100120     F                                     INFDS(DSFMT)
001400100120     FANABL01L  UF A E           K DISK
001500100120     D*----------------------------------------------------*
001600100120     D* Schiere per la gestione delle opzioni
001700100120     D*
001800100120    >D $MS1            S             27    DIM(14) CTDATA PERRCD(1)
001900100120    >D $TX1            S             50    DIM(14) ALT($MS1)
002000100120    >D $OP1            S              2  0 DIM(14) CTDATA PERRCD(1)
002100100120    >D $FL1            S              1    DIM(14) ALT($OP1)
002200100120     D $TX             S             50    DIM(50)
002300100120     D $FL             S              1    DIM(50)
002400100120     D* Schiere per la gestione dei tasti di funzione
002500100120     D $FM             S             27    DIM(24)
002600100120     D $FX             S             50    DIM(24)
002700100120     D $FC             S              1    DIM(24)
002800100120     D $FR             S              1    DIM(24)
002900100120     D* Schiere per la gestione dei tasti di funzione
003000100120     D* Video S1
003100100120     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
003200100120     D $FX2            S             50    DIM(24) ALT($FM2)
003300100120     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
003400100120     D $FR2            S              1    DIM(24) ALT($FC2)
003500100120     D* campi in lingua
003600100120     D LIN             S              6    DIM(0001) CTDATA PERRCD(001)
003700100120     D LIND            S             50    DIM(0001) ALT(LIN)
003800100120     D LPOS            S              3  0 DIM(0001) CTDATA PERRCD(001)
003900100120     D LLUN            S              3  0 DIM(0001) ALT(LPOS)
004000100120     D* DS  restituzione autorizzazioni
004100100120     D* IND=indice
004200100120     D* GRU=man. di gruppo
004300100120     D* AZI=man. azienda
004400100120     D AUTDS           DS
004500100120     D  MANIND                 1      1
004600100120     D  MANGRU                 2      2
004700100120     D  MANAZI                 3      3
004800100120     D*-------------
004900100120     D* Richiamo a SDGTABDR
005000100120     D SDGTABDS      E DS                  inz
005100100120     D*-------------
005200100120     D* Passaggio Parametri
005300100120     D KPJBA         E DS
005400100120     D*-------------
005500100120     D* DS Esterna autorizzazioni tabelle
005600100120     D XCHATB        E DS                  EXTNAME(XCHKATBDS)
005700100120     D*-------------
005800100120      * DS Società
005900100120     D SOC001        E DS                  EXTNAME(SDGSOCDS)
006000100120     D*-------------
006100100120      * DS Interna per dati utente
006200100120     D XSOCDS          DS          1000
006300100120     D*-------------
006400100120     D* Parametri in ricezione
006500100120     D XTABDS        E DS
006600100120     D*-------------
006700100120    >D D002DS          DS
006800100120     D  OPZ002                 1      2
006900100120     D* 0 = ritorno normale
007000100120     D* 1 = F3
007100100120     D* 2 = F12
007200100120     D  RET002                 3      3
007300100120     D* operazione eseguite dal pgm chiamato :
007400100120     D* 0 = nessuna operazione
007500100120     D* 1 = eseguito aggiornamento
007600100120     D  OPR002                 4      4
007700100120     D* funzione non eseguibile per errore :
007800100120     D* 0 = nessun errore
007900100120     D* 1 = funzione richiamata chiusa in errore
008000100120     D* Aggiungere eventuali altri codici di errore
008100100120     D  ERR002                 5      5
008200100120     D* Azienda
008300100120     D  AZI002                 6      8
008400100120     D* Lingua
008500100120     D  LIN002                 9      9
008600100120     D* Codice scelto
008700100120     D  KEY002                10     24
008800100120     D* Autorizzazione
008900100120     D  AUT002                25     25
009000100120     D* Unita
009100100120     D  UNI002                26     33
009200100120     D*-------------
009300100120     D DSFMT           DS           512
009400100120     D  $TASTO               369    369
009500100120     D  NRG                  370    370
009600100120     D  NCL                  371    371
009700100120     D  SFLNRR               378    379B 0
009800100120     D*-------------
009900100120     D* Nome PGM a video
010000100120     D                 DS
010100100120     D  PROGR                  1     10
010200100120     D  ASTER1                 1      1    INZ('*')
010300100120     D  SIGLA                  2      9
010400100120     D  ASTER2                10     10    INZ('*')
010500100120     D*
010600100120     D*
010700100120     D*-------------
010800100120     D* Reperimento nome PGM
010900100120     D STATUS         SDS           333
011000100120     D  DSPGM            *PROC
011100100120     D* descrizione tabella
011200160805     D EXTDS         E DS                  EXTNAME(SDGHK1DS)
011300100120     D*-------------
011400100120     D* Reperimento titolo tabella
011500100120     D  WRKTIT         S             30A
011600100120     D  WRKTIT2        S            100A
011700100120     D  WRKTBCOD       S                   LIKE(XTBCOD)
011800100120     D  LENTIT         S              3  0
011900100120     D  I              S              3  0
012000100120     D*-------------
012100100120     D* COSTANTI
012200100120     D*-------------
012300100120     D SFLPAG          C                   CONST(10)
012400100120     D $DIMOP          C                   CONST(14)
012500100120     D* dimensione della schiera $MS1
012600100120     D*
012700100120     D* Tasti di funzione
012800100120     D F01             C                   CONST(X'31')
012900100120     D F02             C                   CONST(X'32')
013000100120     D F03             C                   CONST(X'33')
013100100120     D F04             C                   CONST(X'34')
013200100120     D F05             C                   CONST(X'35')
013300100120     D F06             C                   CONST(X'36')
013400100120     D F07             C                   CONST(X'37')
013500100120     D F08             C                   CONST(X'38')
013600100120     D F09             C                   CONST(X'39')
013700100120     D F10             C                   CONST(X'3A')
013800100120     D F11             C                   CONST(X'3B')
013900100120     D F12             C                   CONST(X'3C')
014000100120     D F13             C                   CONST(X'B1')
014100100120     D F14             C                   CONST(X'B2')
014200100120     D F15             C                   CONST(X'B3')
014300100120     D F16             C                   CONST(X'B4')
014400100120     D F17             C                   CONST(X'B5')
014500100120     D F18             C                   CONST(X'B6')
014600100120     D F19             C                   CONST(X'B7')
014700100120     D F20             C                   CONST(X'B8')
014800100120     D F21             C                   CONST(X'B9')
014900100120     D F22             C                   CONST(X'BA')
015000100120     D F23             C                   CONST(X'BB')
015100100120     D F24             C                   CONST(X'BC')
015200100120     D ENTER           C                   CONST(X'F1')
015300100120     D ROLDWN          C                   CONST(X'F4')
015400100120     D ROLLUP          C                   CONST(X'F5')
015500100120     I*
015600100120     I/EJECT
015700100120     I*-------------
015800100120     IS1
016000160805     I              S1DES                       DESHK1
016100100120     I*-------------
016200100120     I*
016300100120     C*----------------------------------------------------*
016400100120     C*                MAIN LINE PROGRAM
016500100120     C*----------------------------------------------------*
016600100120     C*
016700100120     C* inizializzazione variabili
016800100120     C                   EXSR      INZVAR
016900100120     C*
017000100120     C     $FINE         DOWEQ     *OFF
017100100120     C     $GEST         CASEQ     'S1'          GESS1
017200100120     C                   END
017300100120     C                   END
017400100120     C*
017500100120     C* fine programma
017600100120     C                   EXSR      ENDPGM
017700100120     C*
017800100120     C/EJECT
017900100120     C************************************************************
018000100120     C* FINE PROGRAMMA
018100100120     C************************************************************
018200100120     C     ENDPGM        BEGSR
018300100120     C*
018400100120     C* passaggio dati al pgm chiamante
018500100120     C                   MOVE      *BLANKS       KPJBU
018600100120     C                   MOVEL     XTABDS        KPJBU
018700100120     C*
018800100120     C* Se pgm Guida non è stato richiamato posso chiudere in LR
018900100120     C     XtaOpz        IFEQ      *BLANK
019000100120     C                   SETON                                            LR
019100100120     C                   RETURN
019200100120     C* altrimenti chiudo in RT
019300100120     C                   ELSE
019400100120     C                   SETON                                            RT
019500100120     C                   RETURN
019600100120     C                   ENDIF
019700100120     C*
019800100120     C                   ENDSR
019900100120     C/EJECT
020000100120     C************************************************************
020100100120     C* RIEMPIMENTO OPZIONI
020200100120     C************************************************************
020300100120     C     RIEOPZ        BEGSR
020400100120     C*
020500100120     C* Input :
020600100120     C* - $TX = schiera con tutti i testi disponibili
020700100120     C* - $FL = schiere con i flag di validità associati ai testi
020800100120     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
020900100120     C*          opzione usato
021000100120     C* - WMXO = variabile contenente il numero massimo di testi
021100100120     C*          opzione usabili
021200100120     C* Output :
021300100120     C* - WOPZ1 = variabile contenente la prima riga opzioni
021400100120     C* - WOPZ2 = variabile contenente la seconda riga opzioni
021500100120     C* - FF23  = flag che indica se c'è bisogno del tasto F23
021600100120     C*
021700100120     C                   CALLB     'X04OPMSG'
021800100120     C* input
021900100120     C                   PARM                    $TX
022000100120     C                   PARM                    $FL
022100100120     C                   PARM                    WCOZ              3 0
022200100120     C                   PARM                    WMXO              3 0
022300100120     C* output
022400100120     C                   PARM                    WOPZ1            76
022500100120     C                   PARM                    WOPZ2            76
022600100120     C                   PARM                    FF23              1
022700100120     C* input
022800100120     C                   PARM      42            $L01              3 0
022900100120     C                   PARM      42            $L02              3 0
023000100120     C*
023100100120     C                   PARM                    $ALLOPT         960
023200100120     C*
023300100120     C                   ENDSR
023400100120     C/EJECT
023500100120     C************************************************************
023600100120     C* RIEMPIMENTO TASTI FUNZIONE
023700100120     C************************************************************
023800100120     C     RIEKEY        BEGSR
023900100120     C*
024000100120     C* Input :
024100100120     C* - $FX = schiera con tutti i testi disponibili
024200100120     C* - $FC = schiere con i flag di validità associati ai testi
024300100120     C* - $FR = schiere con i flag di REVERSE IMAGE
024400100120     C* - $ULK = ultimo testo caricato
024500100120     C* Output :
024600100120     C* - $ULK
024700100120     c* - $KEY1= 1° riga di tasti funzione
024800100120     c* - $KEY2= 2° riga di tasti funzione
024900100120     C*
025000100120     C                   CALLB     'XKEYMSG'
025100100120     C* input
025200100120     C                   PARM                    $FX
025300100120     C                   PARM                    $FC
025400100120     C                   PARM                    $FR
025500100120     C                   PARM                    $ULK              3 0
025600100120     C* output
025700100120     C                   PARM                    $KEY1            79
025800100120     C                   PARM                    $KEY2            79
025900100120     C* input
026000100120     C                   PARM      42            $L01              3 0
026100100120     C                   PARM      42            $L02              3 0
026200100120     C*
026300100120     C                   PARM                    $ALLFUNCT       480
026400100120     C*
026500100120     C                   ENDSR
026600100120     C/EJECT
026700100120     C************************************************************
026800100120     C* GESTIONE EMISSIONE TASTI FUNZIONE
026900100120     C************************************************************
027000100120     C     GESPIE        BEGSR
027100100120     C*
027200100120     C                   SELECT
027300100120     C* Lista
027400100120     C     $GEST         WHENEQ    'S1'
027500100120     C     $ULKS1        IFEQ      0
027600100120     C                   EXSR      F24S1
027700100120     C                   ELSE
027800100120     C                   WRITE     Z1
027900100120     C                   ENDIF
028000100120     C                   ENDSL
028100100120     C*
028200100120     C                   ENDSR
028300100120     C/EJECT
028400100120     C************************************************************
028500100120     C* CONTROLLO ABILITAZIONE TASTI VIDEATA S1
028600100120     C************************************************************
028700100120     C     CTRKS1        BEGSR
028800100120     C                   SETOFF                                       99
028900100120     C                   MOVEA     $FC2          $FC
029000100120     C                   EXSR      CTRKEY
029100100120     C     *IN99         IFEQ      *ON
029200100120     C                   SETON                                        97
029300100120     C                   ENDIF
029400100120     C*
029500100120     C                   ENDSR
029600100120     C/EJECT
029700100120     C************************************************************
029800100120     C* CONTROLLO ABILITAZIONE TASTI
029900100120     C* INPUT
030000100120     c* $TASTO =Tasto funzione premuto
030100100120     C* $FC = Tasti abilitati
030200100120     C* OUTPUT
030300100120     C* IN99 = ERrore
030400100120     C************************************************************
030500100120     C     CTRKEY        BEGSR
030600100120     C                   SELECT
030700100120     C     $TASTO        WHENEQ    F01
030800100120     C     $FC(1)        IFEQ      '0'
030900100120     C                   SETON                                        99
031000100120     C                   ENDIF
031100100120     C     $TASTO        WHENEQ    F02
031200100120     C     $FC(2)        IFEQ      '0'
031300100120     C                   SETON                                        99
031400100120     C                   ENDIF
031500100120     C     $TASTO        WHENEQ    F03
031600100120     C     $FC(3)        IFEQ      '0'
031700100120     C                   SETON                                        99
031800100120     C                   ENDIF
031900100120     C     $TASTO        WHENEQ    F04
032000100120     C     $FC(4)        IFEQ      '0'
032100100120     C                   SETON                                        99
032200100120     C                   ENDIF
032300100120     C     $TASTO        WHENEQ    F05
032400100120     C     $FC(5)        IFEQ      '0'
032500100120     C                   SETON                                        99
032600100120     C                   ENDIF
032700100120     C     $TASTO        WHENEQ    F06
032800100120     C     $FC(6)        IFEQ      '0'
032900100120     C                   SETON                                        99
033000100120     C                   ENDIF
033100100120     C     $TASTO        WHENEQ    F07
033200100120     C     $FC(7)        IFEQ      '0'
033300100120     C                   SETON                                        99
033400100120     C                   ENDIF
033500100120     C     $TASTO        WHENEQ    F08
033600100120     C     $FC(8)        IFEQ      '0'
033700100120     C                   SETON                                        99
033800100120     C                   ENDIF
033900100120     C     $TASTO        WHENEQ    F09
034000100120     C     $FC(09)       IFEQ      '0'
034100100120     C                   SETON                                        99
034200100120     C                   ENDIF
034300100120     C     $TASTO        WHENEQ    F10
034400100120     C     $FC(10)       IFEQ      '0'
034500100120     C                   SETON                                        99
034600100120     C                   ENDIF
034700100120     C     $TASTO        WHENEQ    F11
034800100120     C     $FC(11)       IFEQ      '0'
034900100120     C                   SETON                                        99
035000100120     C                   ENDIF
035100100120     C     $TASTO        WHENEQ    F12
035200100120     C     $FC(12)       IFEQ      '0'
035300100120     C                   SETON                                        99
035400100120     C                   ENDIF
035500100120     C     $TASTO        WHENEQ    F13
035600100120     C     $FC(13)       IFEQ      '0'
035700100120     C                   SETON                                        99
035800100120     C                   ENDIF
035900100120     C     $TASTO        WHENEQ    F14
036000100120     C     $FC(14)       IFEQ      '0'
036100100120     C                   SETON                                        99
036200100120     C                   ENDIF
036300100120     C     $TASTO        WHENEQ    F15
036400100120     C     $FC(15)       IFEQ      '0'
036500100120     C                   SETON                                        99
036600100120     C                   ENDIF
036700100120     C     $TASTO        WHENEQ    F16
036800100120     C     $FC(16)       IFEQ      '0'
036900100120     C                   SETON                                        99
037000100120     C                   ENDIF
037100100120     C     $TASTO        WHENEQ    F17
037200100120     C     $FC(17)       IFEQ      '0'
037300100120     C                   SETON                                        99
037400100120     C                   ENDIF
037500100120     C     $TASTO        WHENEQ    F18
037600100120     C     $FC(18)       IFEQ      '0'
037700100120     C                   SETON                                        99
037800100120     C                   ENDIF
037900100120     C     $TASTO        WHENEQ    F19
038000100120     C     $FC(19)       IFEQ      '0'
038100100120     C                   SETON                                        99
038200100120     C                   ENDIF
038300100120     C     $TASTO        WHENEQ    F20
038400100120     C     $FC(20)       IFEQ      '0'
038500100120     C                   SETON                                        99
038600100120     C                   ENDIF
038700100120     C     $TASTO        WHENEQ    F21
038800100120     C     $FC(21)       IFEQ      '0'
038900100120     C                   SETON                                        99
039000100120     C                   ENDIF
039100100120     C     $TASTO        WHENEQ    F22
039200100120     C     $FC(22)       IFEQ      '0'
039300100120     C                   SETON                                        99
039400100120     C                   ENDIF
039500100120     C     $TASTO        WHENEQ    F23
039600100120     C     $FC(23)       IFEQ      '0'
039700100120     C                   SETON                                        99
039800100120     C                   ENDIF
039900100120     C     $TASTO        WHENEQ    F24
040000100120     C     $FC(24)       IFEQ      '0'
040100100120     C                   SETON                                        99
040200100120     C                   ENDIF
040300100120     C                   ENDSL
040400100120     C                   ENDSR
040500100120     C/EJECT
040600100120     C************************************************************
040700100120     C* GESTIONE LISTA
040800100120     C************************************************************
040900100120     C     GESS1         BEGSR
041000100120     C* inizializzazione videata
041100100120     C     $INZS1        IFEQ      *ON
041200100120     C                   EXSR      INZS1
041300100120     C                   MOVE      *OFF          $INZS1
041400100120     C                   ENDIF
041500100120     C* emissione piede videata se proveniente da altra
041600100120     C* salvataggio valore formato di provenienza
041700100120     C     $LASTV        IFNE      'S1'
041800100120!!   C*                    WRITEFMTT1
041900100120     C                   EXSR      GESPIE
042000100120     C                   MOVE      $LASTV        $LASTG
042100100120     C                   MOVE      'S1'          $LASTV
042200100120     C                   END
042300100120     C*
042400100120     C     WMAX          IFEQ      0
042500100120     C                   WRITE     D1
042600100120     C                   ENDIF
042700100120     C                   MOVE      '1'           WIN727            1
042800100120     C*
042900100120     C                   EXFMT     C1
043000100120     C     C1NRR         IFNE      0
043100100120     C                   Z-ADD     C1NRR         WSFL
043200100120     C                   ENDIF
043300100120     C                   Z-ADD     SFLNRR        C1RCD
043400100120     C*
043500100120     C* controllo abilitazione tasti funzione
043600100120     C*
043700100120     C                   EXSR      CTRKS1
043800100120     C     *IN99         IFEQ      *OFF
043900100120     C*
044000100120     C*
0441001001201    C                   SELECT
044200100120     C* F3=Fine
044300100120     C     $TASTO        WHENEQ    F03
044400100120     C                   EXSR      F03S1
044500100120     C* F10=Immissione
044600100120     C     $TASTO        WHENEQ    F10
044700100120     C                   EXSR      F10S1
044800100120     C* F12=Ritorno
044900100120     C     $TASTO        WHENEQ    F12
045000100120     C                   EXSR      F12S1
045100100120     C* F23=Altre opzioni
045200100120     C     $TASTO        WHENEQ    F23
045300100120     C                   EXSR      F23S1
045400100120     C* F24=Altri tasti
045500100120     C     $TASTO        WHENEQ    F24
045600100120     C                   EXSR      F24S1
045700100120     C* ROLL-UP
045800100120     C     $TASTO        WHENEQ    ROLLUP
045900100120     C                   EXSR      ROLS1
046000100120     C*
046100100120     C                   OTHER
046200100120     C* CONTROLLO DATI
046300100120     C                   EXSR      CTRC1
046400100120     C     *IN99         IFEQ      *OFF
046500100120     C                   EXSR      CTRS1
046600100120     C                   END
046700100120     C                   ENDSL
046800100120     C*
046900100120     C                   ENDIF
047000100120     C                   ENDSR
047100100120     C/EJECT
047200100120     C************************************************************
047300100120     C* INIZIALIZZAZIONE LISTA
047400100120     C************************************************************
047500100120     C     INZS1         BEGSR
047600100120     C* pulizia SFL
047700100120     C                   SETOFF                                         3031
047800100120     C                   WRITE     C1
047900100120     C                   SETON                                          31
048000100120     C*
048100100120     C* reperimento titolo
048200100120     C                   Reset                   SDGTABDS
048300100120     C*
048400100120     C                   move      xtbcod        WrkTbcod
048500100120     C                   eval      xtbkey      = *zero
048600100120     C                   move      xtbcod        xtbkey
048700100120     C                   eval      xtbazi      = xscsoc
048800100120     C                   eval      xtblin      = xsclin
048900100120     C                   eval      xtbcod      = 'AAA'
049000100120     C                   eval      xtbric      = '1'
049100100120     C                   eval      xtbalc      = '0'
049200100120     C                   CALL      'SDGTABDR'
049300100120     C                   PARM                    SDGTABDS
049400100120     C*
049500100120     C                   if        xtberr <> *off
049600100120     C                   movel     *all'?'       WrkTit
049700100120     C                   else
049800100120     C                   movel     xtbuni        WrkTit
049900100120     C                   endif
050000100120     C*
050100100120     C* centratura
050200100120     C                   move      WrkTbcod      xtbcod
050300100120     C                   eval      WrkTit2=%trimr(Wrktit)+X'22'+xtbcod
050400100120     C                             +X'20'
050500100120     C     ' '           CHECKR    Wrktit2       I
050600100120     C                   call      'X71CENTRA'
050700100120     C                   parm                    Wrktit2
050800100120     C                   parm                    I
050900100120     C                   parm                    lentit
051000100120     C                   movel     Wrktit2       C1tit
051100100120     C*
051200100120     C* riempimento opzioni
051300100120     C                   MOVEA     $TX1          $TX
051400100120     C                   MOVEA     $FL1          $FL
051500100120     C                   Z-ADD     0             WCOZ
051600100120     C*  equivale al num.di elementi della schiera $TX1
051700100120     C                   Z-ADD     $DIMOP        WMXO
051800100120     C                   EXSR      RIEOPZ
051900100120     C                   Z-ADD     23            X
052000100120     C                   MOVE      FF23          $FC2(X)
052100100120     C                   MOVEL     WOPZ1         C1OPZ1
052200100120     C                   MOVEL     $ALLOPT       H1ALLOPT
052300100120     C                   MOVEL     WOPZ2         C1OPZ2
052400100120     C                   Z-ADD     WCOZ          C1COZ
052500100120     C*
052600100120     C* CARICAMENTO SFL A PAGINE
052700100120     C*
052800100120     C                   Z-ADD     0             S1NRR
052900100120     C                   Z-ADD     0             WMAX
053000100120     C*
053100100120     C* Posizionamento su file pilota
053200100120     C                   CLEAR                   C1POS
053300100120    >C                   EXSR      SETANA
053400100120    >C                   EXSR      REDANA
053500100120     C* ripristino i valori del posizionamento
053600100120     C                   MOVE      W1POS         C1POS
053700100120     C*
053800100120     C* Carico 1a pagina
053900100120     C                   EXSR      ROLS1
054000100120     C                   Z-ADD     SFLPAG        WPAG
054100100120     C*
054200100120     C                   ENDSR
054300100120     C/EJECT
054400100120     C************************************************************
054500100120     C* CARICAMENTO PAGINA LISTA
054600100120     C************************************************************
054700100120     C     ROLS1         BEGSR
054800100120     C*
054900100120     C                   SETOFF                                       32
055000100120     C                   Z-ADD     0             Y
055100100120     C                   Z-ADD     WMAX          S1NRR
055200100120     C*
055300100120     C* Leggo dal file anagrafico per caricare la lista
055400100120     C*
0555001001201    C     $EFILE        DOWEQ     *OFF
055600100120     C     Y             ANDLT     WPAG
055700100120     C*
055800100120     C                   EXSR      RIES1
055900100120     C*
056000100120     C                   ADD       1             S1NRR
056100100120     C                   ADD       1             Y
056200100120     C                   MOVE      $IN41         *IN41
056300100120     C*
056400100120     C                   WRITE     S1
056500100120     C*
056600100120    >C                   EXSR      REDANA
056700100120     C*
0568001001201-   C                   ENDDO
056900100120     C*
057000100120     C                   Z-ADD     S1NRR         WMAX                 30
057100100120     C*
057200100120     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
057300100120     C*
057400100120     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
057500100120     C                   MVR                     RESTO             3 0
057600100120     C     PAGINE        MULT      SFLPAG        C1RCD
0577001001201    C     RESTO         IFGT      0
057800100120     C                   ADD       1             C1RCD
0579001001201E   C                   ELSE
058000100120     C                   SUB       SFLPAG        C1RCD
058100100120     C                   ADD       1             C1RCD
0582001001201-   C                   ENDIF
058300100120     C*
058400100120     C                   ENDSR
058500100120     C/EJECT
058600100120     C************************************************************
058700100120     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
058800100120     C************************************************************
058900100120     C     SETANA        BEGSR
059000100120     C*
059100100120     C                   MOVE      *BLANKS       XTBKEY
059200100120     C                   MOVE      XtaAzi        XTBAZI
059300100120     C                   MOVE      XtaLin        XTBLIN
059400100120     C                   MOVE      '9'           XTBRIC
059500100120     C                   MOVE      '0'           XTBALC
059600100120     C                   MOVE      XSCCUT        XTBUTE
059700100120     C                   MOVE      '0'           XTBERR
059800100120     C                   MOVE      *ZEROS        XTBKEY
059900100120     C                   MOVE      W1POS         XTBKEY
060000100120     C                   CALL      'SDGTABDR'
060100100120     C                   PARM                    SDGTABDS
060200100120     C*
060300100120     C                   ENDSR
060400100120     C/EJECT
060500100120     C************************************************************
060600100120     C* LETTURA RCD ARCHIVIO PILOTA
060700100120     C************************************************************
060800100120     C     REDANA        BEGSR
060900100120     C*
061000100120     C                   MOVEL     *OFF          $RCDOK
061100100120     C*
0612001001201    C     $EFILE        DOUEQ     *ON
061300100120     C     $RCDOK        OREQ      *ON
061400100120     C*
061500100120     C                   MOVE      '3'           XTBRIC
061600100120     C                   MOVE      '0'           XTBERR
061700100120     C*
061800100120     C                   CALL      'SDGTABDR'
061900100120     C                   PARM                    SDGTABDS
062000100120     C*
062100100120     C     XTBERR        IFNE      '0'
062200100120     C                   MOVEL     *ON           $EFILE
062300100120     C                   ELSE
062400100120     C                   MOVEL     *OFF          $EFILE
062500100120     C                   ENDIF
062600100120     C*
0627001001202    C     $EFILE        IFEQ      *OFF
062800100120     C                   EXSR      SELANA
062900100120     C                   MOVE      XTBKEY        SAVKEY
063000100120     C                   MOVE      XTBGRU        SAVGRU
063100100120     C                   MOVE      XTBUNI        SAVUNI
063200100120     C                   MOVE      XTBUNT        SAVUNT
0633001001202-   C                   ENDIF
063400100120     C*
0635001001201-   C                   ENDDO
063600100120     C*
063700100120     C                   MOVE      $EFILE        *IN33
063800100120     C*
063900100120     C                   ENDSR
064000100120     C/EJECT
064100100120     C************************************************************
064200100120     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
064300100120     C************************************************************
064400100120     C     CLCPAG        BEGSR
064500100120     C* Input :
064600100120     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
064700100120     C*          cursore
064800100120     C* - SFLPAG = numero rcd per pagina sfl
064900100120     C* Output :
065000100120     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
065100100120     C*
065200100120     C     WSFL          DIV       SFLPAG        PAGINE            4 0
065300100120     C                   MVR                     RESTO             3 0
065400100120     C     RESTO         IFGT      0
065500100120     C                   ADD       1             PAGINE
065600100120     C                   ENDIF
065700100120     C     PAGINE        MULT      SFLPAG        WPAG
065800100120     C*
065900100120     C                   ENDSR
066000100120     C/EJECT
066100100120     C************************************************************
066200100120     C* SELEZIONI SULLA RIGA LETTA
066300100120     C* impostare a *OFF il campo $RCDOK per scartare un record che
066400100120     C* non rientra nei parametri di parzializzazione
066500100120     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
066600100120     C* superiore stabilito nei parametri
066700100120     C*
066800100120     C************************************************************
066900100120     C     SELANA        BEGSR
067000100120     C*
067100100120     C                   MOVEL     *ON           $RCDOK
067200100120     C* Se aperto il pgm con opzione 11
067300100120     C* carico solo i record di gruppo
067400100120     C     XtaOpz        IFEQ      '11'
067500100120     C     XTBGRU        ANDNE     'G'
067600100120     C                   MOVEL     *OFF          $RCDOK
067700100120     C                   END
067800100120     C* Carica in alta intensità solo i record
067900100120     C* abilitati
068000100120     C                   MOVE      '0'           $IN41
068100100120     C     XtaOpz        IFEQ      '11'
068200100120     C     XTBGRU        ANDEQ     'G'
068300100120     C     XTBAZB        ANDNE     'N'
068400100120     C                   MOVE      '1'           $IN41
068500100120     C                   END
068600100120     C*
068700100120     C                   ENDSR
068800100120     C/EJECT
068900100120     C************************************************************
069000100120     C* RIEMPIMENTO RIGA LISTA
069100100120     C************************************************************
069200100120     C     RIES1         BEGSR
069300100709     C*
069400100120     C                   MOVEL     SAVUNI        EXTDS
069500100709     C* per vedere meglio se causale di beni o di servizi,
069600100709     C* se causale di Beni, allineo a destra il codice
069700100709     C* se causale di Servizi, allineo a sinistra il codice
069800100120     C                   MOVE      SAVKEY        S1COD
070400100709     C*
070500100120     C                   MOVE      SAVGRU        S1GRU
070600100120     C                   MOVE      SAVUNT        S1UNITA           8
070700100709     C*
070800100120     C     *LIKE         DEFINE    XTBUNI        SAVUNI
070900100120     C     *LIKE         DEFINE    XTBKEY        SAVKEY
071000100120     C     *LIKE         DEFINE    XTBGRU        SAVGRU
071100100120     C     *LIKE         DEFINE    XTBUNT        SAVUNT
071200100120     C*
071300100120     C                   ENDSR
071400100120     C/EJECT
071500100120     C************************************************************
071600100120     C* GESTIONE F03 VIDEO S1
071700100120     C************************************************************
071800100120     C     F03S1         BEGSR
071900100120     C*
072000100120     C                   MOVE      *ON           $FINE
072100100120     C                   MOVE      '1'           XtaRet
072200100120     C* fine programma
072300100120     C                   EXSR      ENDPGM
072400100120     C*
072500100120     C                   ENDSR
072600100120     C/EJECT
072700100120     C************************************************************
072800100120     C* GESTIONE F10 VIDEO S1
072900100120     c* AGGIUNTA RECORD
073000100120     C************************************************************
073100100120     C     F10S1         BEGSR
073200100120     C*
073300100120     C                   RESET                   D002DS
073400100120     C                   MOVEL     '01'          OPZ002
073500100120     C                   MOVE      XtaLin        LIN002
073600100120     C                   MOVE      XtaAzi        AZI002
073700100120     C                   MOVE      S1UNITA       UNI002
073800100618     C*
073900100618     C                   MOVE      *Blank        S1Cod
074000100618     C                   MOVE      *ALL'0'       KEY002
074100100618     C*
074200100618     C                   MOVE      S1Cod         KEY002
074300100618     C*
074400100120     C                   MOVE      *BLANKS       KPJBU
074500100120     C                   MOVEL     D002DS        KPJBU
074600160805$004 C                   CALL      'SDGHK1R2'
074700100120     C                   PARM                    KPJBA
074800100120     C                   MOVEL     KPJBU         D002DS
074900100120     C* ritorno da PGM gestione
075000100120     C                   EXSR      GESRET
075100100120     C     WINZS1        IFEQ      *ON
075200100120     C                   MOVE      *ON           $INZS1
075300100120     C* se esistevano già righe sul sfl
075400100120     C* calcolo pagina a cui deve posizionarsi
075500100120     C     WSFL          IFGT      0
075600100120     C                   EXSR      CLCPAG
075700100120     C* altrimenti carico solo la 1a pagina
075800100120     C                   ELSE
075900100120     C                   Z-ADD     SFLPAG        WPAG
076000100120     C                   ENDIF
076100100120     C                   ENDIF
076200100120     C*
076300100120     C                   MOVE      *HIVAL        $LASTV
076400100120     C                   ENDSR
076500100120     C/EJECT
076600100120     C************************************************************
076700100120     C* GESTIONE F12 VIDEO S1
076800100120     C************************************************************
076900100120     C     F12S1         BEGSR
077000100120     C*
077100100120     C                   MOVE      '2'           XtaRet
077200100120     C                   MOVE      *ON           $FINE
077300100120     C*
077400100120     C                   ENDSR
077500100120     C/EJECT
077600100120     C************************************************************
077700100120     C* GESTIONE F23 VIDEO S1
077800100120     C************************************************************
077900100120     C     F23S1         BEGSR
078000100120     C*
078100100120     C                   EXSR      RIEOPZ
078200100120     C                   Z-ADD     23            X
078300100120     C                   MOVE      FF23          $FC2(X)
078400100120     C                   MOVEL     WOPZ1         C1OPZ1
078500100120     C                   MOVEL     $ALLOPT       H1ALLOPT
078600100120     C                   MOVEL     WOPZ2         C1OPZ2
078700100120     C                   Z-ADD     WCOZ          C1COZ
078800100120     C*
078900100120     C                   ENDSR
079000100120     C/EJECT
079100100120     C************************************************************
079200100120     C* GESTIONE F24 VIDEO S1
079300100120     C************************************************************
079400100120     C     F24S1         BEGSR
079500100120     C*
079600100120     C*
079700100120     C                   MOVEA     $FX2          $FX
079800100120     C                   MOVEA     $FC2          $FC
079900100120     C                   MOVEA     $FR2          $FR
080000100120     C                   Z-ADD     $ULKS1        $ULK
080100100120     C                   EXSR      RIEKEY
080200100120     C                   MOVEA     $FC           $FC2
080300100120     C                   Z-ADD     $ULK          $ULKS1            3 0
080400100120     C                   MOVEL     $KEY1         Z1KE1
080500100120     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
080600100120     C                   MOVEL     $KEY2         Z1KE2
080700100120     C                   WRITE     Z1
080800100120     C*
080900100120     C                   ENDSR
081000100120     C/EJECT
081100100120     C************************************************************
081200100120     C* CONTROLLO TESTATA LISTA
081300100120     C************************************************************
081400100120     C     CTRC1         BEGSR
081500100120     C*
081600100120     C                   MOVE      *OFF          *IN99
081700100120     C*
081800100120     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
081900100120     C* L'INIZIALIZZAZIONE LISTA
082000100120     C*
082100100120     C     C1POS         IFNE      W1POS
082200100120     C                   EXSR      POSS1
082300100120     C                   END
082400100120     C*
082500100120     C                   ENDSR
082600100120     C/EJECT
082700100120     C************************************************************
082800100120     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
082900100120     C************************************************************
083000100120     C     POSS1         BEGSR
083100100120     C*
083200100120     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
083300100120     C*
0834001001201    C     WMAX          IFGT      0
083500100120     C                   READC     S1                                     21
0836001001202    C     *IN21         DOWEQ     *OFF
083700100120     C     *IN99         ANDEQ     *OFF
083800100120     C*
0839001001203    C     S1OPZ         IFNE      0
084000100120     C                   SETON                                        51  99
0841001001203-   C                   ENDIF
084200100120     C* se rilevata opzione attiva, attivo sflnxtchg
0843001001203    C     *IN99         IFEQ      *ON
084400100120     C                   MOVE      *ON           *IN32
084500100120     C                   MOVE      $IN41         *IN41
084600100120     C                   UPDATE    S1
0847001001203E   C                   ELSE
084800100120     C                   READC     S1                                     21
0849001001203-   C                   ENDIF
085000100120     C*
0851001001202-   C                   ENDDO
085200100120     C*
0853001001201-   C                   ENDIF
085400100120     C*
085500100120     C*
0856001001201    C     *IN99         IFEQ      *OFF
085700100120     C                   CLEAR                   W1POS
085800100120     C*
085900100120     C                   MOVEL     C1POS         W1POS
086000100120     C*
086100100120     C                   MOVE      *ON           *IN99
086200100120     C                   MOVE      *ON           $INZS1
086300100120     C*
086400100120     C*
0865001001201-   C                   ENDIF
086600100120     C*
086700100120     C                   ENDSR
086800100120     C/EJECT
086900100120     C************************************************************
087000100120     C* CONTROLLO OPZIONI LISTA
087100100120     C************************************************************
087200100120     C     CTRS1         BEGSR
087300100120     C*
087400100120     C                   MOVEL     *OFF          $ESCI
087500100120     C                   SETOFF                                       99
087600100120     C                   Z-ADD     0             S1OPZ
087700100120     C*
087800100120     C* Leggo il sfl solo se ci sono rcd
0879001001201    C     WMAX          IFGT      0
088000100120     C                   READC     S1                                     21
088100100120     C*
088200100120     C* esce se fine sfl o errore che richiede l'uscita
0883001001202    C     *IN21         DOWEQ     *OFF
088400100120     C     $ESCI         ANDEQ     *OFF
088500100120     C                   Z-ADD     S1NRR         C1RCD
088600100120     C* ctrl su riga
088700100120     C                   EXSR      RECS1
088800100120     C* gestione opzioni
0889001001203    C     S1OPZ         IFNE      0
089000100120     C     *IN99         ANDEQ     *OFF
089100100120     C                   EXSR      OPZS1
0892001001203-   C                   ENDIF
089300100120     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0894001001203    C     *IN99         IFEQ      *ON
089500100120     C     $ESCI         OREQ      *ON
089600100120     C                   MOVE      *ON           *IN32
089700100120     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
089800100120     C* la ripristinerò a conclusione del ciclo di READC
089900100120     C                   MOVE      *OFF          $INZS1
0900001001203-   C                   ENDIF
090100100120     C*
090200100120     C                   Z-ADD     0             S1OPZ
090300100120     C*
090400100120     C                   MOVE      $IN41         *IN41
090500100120     C                   UPDATE    S1
090600100120     C*
090700100120     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0908001001203    C     $ESCI         IFEQ      *OFF
090900100120     C                   READC     S1                                     21
091000100120     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0911001001204    C     *IN21         IFEQ      *ON
091200100120     C                   MOVE      WINZS1        $INZS1
091300100120     C* calcolo pagina a cui deve posizionarsi
091400100120     C                   EXSR      CLCPAG
0915001001204-   C                   ENDIF
0916001001203-   C                   ENDIF
091700100120     C*
0918001001202-   C                   ENDDO
091900100120     C*
0920001001201-   C                   ENDIF
092100100120     C*
092200100120     C                   ENDSR
092300100120     C/EJECT
092400100120     C************************************************************
092500100120     C* CONTROLLO CAMPI I/O RIGA LISTA
092600100120     C************************************************************
092700100120     C     RECS1         BEGSR
092800100120     C*
092900100120     C* reset indicatori DSPATR
093000100120     C                   MOVE      *ALL'0'       IN4049           10
093100100120     C                   MOVEA     IN4049        *IN(40)
093200100120     C*
093300100120     C* Controllo se Utente Autorizzato alla scelta
093400100120     C     S1OPZ         IFNE      0
093500100120     C                   Z-ADD     1             X
093600100120     C     S1OPZ         LOOKUP    $OP1(X)                                21
093700100120     C     *IN21         IFEQ      *OFF
093800100120     C     $FL1(X)       OREQ      'N'
093900100120     C                   MOVE      *ON           $ESCI
094000100120     C                   SETON                                        504099
094100100120     C                   ENDIF
094200100120     C*
094300100120     c* Controllo autorizzazione alla modifica su record di gruppo
094400100120     C* Reperisco tipo codice (gruppo/azienda)
094500100120     C                   MOVE      *ALL'0'       XTBKEY
094600100120     C                   MOVE      S1COD         XTBKEY
094700100120     C                   MOVE      S1UNITA       XTBUNT
094800100120     C                   MOVE      '0'           XTBALC
094900100120     C                   MOVE      '1'           XTBRIC
095000100120     C                   CALL      'SDGTABDR'
095100100120     C                   PARM                    SDGTABDS
095200100120     C* Reperisco autorizzazioni utente
095300100120     C                   CLEAR                   XCHATB
095400100120     C                   MOVE      XSCSOC        XCTSOC
095500100120     C                   MOVE      XSCDUN        XCTUNI
095600100120     C                   MOVEL     KNMUS         XCTPRF
095700100120     C                   MOVE      XTBCOD        XCTCOD
095800100120     C                   CALL      'XCHKATB'
095900100120     C                   PARM                    XCHATB
096000100120     C                   MOVEL     XCTDDS        AUTDS
096100100120     C     XCTRTN        IFEQ      1
096200100120     C                   MOVEL     'N'           MANGRU
096300100120     C                   ENDIF
096400100120     C* Non ammessa manutenzione di gruppo
096500100120   1 C     XTBGRU        IFEQ      'G'
096600100120     C     MANGRU        ANDEQ     'N'
096700100120   2 C     S1OPZ         IFEQ      2
096800100120     C     S1OPZ         OREQ      3
096900100120     C     S1OPZ         OREQ      4
097000100120     C                   SETON                                        5399
097100100120   2 C                   ENDIF
097200100120   1 C                   ENDIF
097300100120     C* Non ammessa manutenzione di azienda
097400100120   1 C     XTBGRU        IFEQ      'A'
097500100120     C     MANAZI        ANDEQ     'N'
097600100120   2 C     S1OPZ         IFEQ      2
097700100120     C     S1OPZ         OREQ      3
097800100120     C     S1OPZ         OREQ      4
097900100120     C                   SETON                                        5499
098000100120   2 C                   ENDIF
098100100120   1 C                   ENDIF
098200100120     C                   ENDIF
098300100120     C                   ENDSR
098400100120     C/EJECT
098500100120     C************************************************************
098600100120     C* GESTIONE OPZIONI LISTA
098700100120     C************************************************************
098800100120     C     OPZS1         BEGSR
098900100120     C*
0990001001201    C                   SELECT
099100100120     C*
099200100120     C* se opz=Selezione passo la chiave al pgm chiamante
099300100120     C     S1OPZ         WHENEQ    1
099400100120     C                   MOVE      S1OPZ         XtaOpz
099500100120     C                   MOVE      S1COD         XtaKey
099600100120     C                   MOVE      S1UNIta       xtaUni
099700100120     C                   MOVE      *ON           $ESCI
099800100120     C                   MOVE      *ON           $FINE
099900100120     C*
100000100120     C*
100100100120     C* se opz=Abilita scrive su archivio abilitazioni
100200100120     C     S1OPZ         WHENEQ    11
100300100120     C                   MOVE      XSCSOC        ABLAZI
100400100120     C                   MOVE      *ZEROS        ABLKEY
100500100120     C                   MOVE      S1COD         ABLKEY
100600100120     C                   MOVE      S1UNITA       ABLUNT
100700100120     C                   MOVE      XTBCOD        ABLCOD
100800100120     C                   WRITE     ANABL000                             22
100900100120     C*
101000100120     C* se opz=Disabil.cancella su archivio abilitazioni
101100100120     C     S1OPZ         WHENEQ    12
101200100120     C                   MOVE      XSCSOC        ABLAZI
101300100120     C                   MOVE      *ZEROS        ABLKEY
101400100120     C                   MOVE      S1COD         ABLKEY
101500100120     C                   MOVE      S1UNITA       ABLUNT
101600100120     C                   MOVE      XTBCOD        ABLCOD
101700100120     C     KABL          DELETE    ANABL000                           22
101800100120     C*
101900100120     C*
102000100120     C* se altre opzioni (o nessuna opzione) richiamo pgm gestione
1021001001201O   C                   OTHER
102200100120     C*
102300100120     C                   RESET                   D002DS
102400100120     C                   MOVEL     S1OPZ         OPZ002
102500100120     C                   MOVE      *ZERO         KEY002
102600100120     C                   MOVE      S1COD         KEY002
102700100120     C                   MOVE      XtaLin        LIN002
102800100120     C                   MOVE      XtaAzi        AZI002
102900100120     C                   MOVE      *BLANKS       KPJBU
103000100120     C                   MOVEL     D002DS        KPJBU
103100160805$004 C                   CALL      'SDGHK1R2'
103200100120     C                   PARM                    KPJBA
103300100120     C                   MOVEL     KPJBU         D002DS
103400100120     C*
103500100120     C* ritorno da PGM gestione
103600100120     C                   EXSR      GESRET
103700100120     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
1038001001202    C     *IN99         IFEQ      *ON
103900100120     C                   SETON                                        40
1040001001202-   C                   ENDIF
104100100120     C*
104200100120     C                   MOVE      *HIVAL        $LASTV
1043001001201-   C                   ENDSL
104400100120     C*
104500100120     C                   ENDSR
104600100120     C/EJECT
104700100120     C************************************************************
104800100120     C* GESTIONE RITORNO DA PGM DI GESTIONE
104900100120     C************************************************************
105000100120     C     GESRET        BEGSR
105100100120     C*
105200100120     C* modo di ritorno
105300100120     C*
1054001001201    C                   SELECT
105500100120    >C* << questi modi di utilizzo dei valori di ritorno dal
105600100120    >C*    pgm di manutenzione rcd di anagrafica sono delle
105700100120    >C*    proposte, normalmente sempre valide, ma modificabili
105800100120    >C*    per situazioni particolari >>
105900100120     C* 1 = F3
106000100120    >C     RET002        WHENEQ    '1'
106100100120     C                   MOVE      *ON           $ESCI
106200100120     C                   MOVE      *ON           $FINE
106300100120     C* 2 = F12
106400100120    >C     RET002        WHENEQ    '2'
106500100120     C                   MOVE      *ON           $ESCI
106600100120     C*
1067001001201-   C                   ENDSL
106800100120     C*
106900100120     C* operazione eseguite dal pgm chiamato
107000100120     C*
1071001001201    C                   SELECT
107200100120     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
107300100120    >C     OPR002        WHENEQ    '1'
107400100120     C                   MOVE      *ON           WINZS1
107500100120     C*
1076001001201-   C                   ENDSL
107700100120     C*
107800100120     C* funzione non eseguibile per errore :
107900100120     C*
1080001001201    C                   SELECT
108100100120     C* 1 = funzione richiamata chiusa in errore
108200100120    >C*  eventualmente gestire altri codici di errore
108300100120    >C     ERR002        WHENEQ    '1'
108400100120     C                   MOVE      *ON           $ESCI
108500100120     C                   SETON                                        5299
108600100120     C*
1087001001201-   C                   ENDSL
108800100120     C*
108900100120     C                   ENDSR
109000100120     C/EJECT
109100100120     C************************************************************
109200100120     C* OPERAZIONI INIZIALI
109300100120     C************************************************************
109400100120     C     *INZSR        BEGSR
109500100120     C*
109600100120     C* Reperimento parametri
109700100120     C*
109800100120     C     *ENTRY        PLIST
109900100120     C                   PARM                    KPJBA
110000100120     C*
110100100120     C* Reperimento opzioni
110200100120     C*
110300100120     C                   EXSR      REPOPZ
110400100120     C*
110500100120     C* Reperimento tasti di funzione
110600100120     C*
110700100120     C                   EXSR      REPKEY
110800100120     C*
110900160805$007 C                   MOVE      'HK1'         XTBCOD
111000100120     C*
111100100120     C* reperisco le dimensioni della variabile del titolo
111200100120     C                   EVAL      LENTIT = %Size(C1Tit)
111300100120     C*
111400100120     C                   ENDSR
111500100120     C/EJECT
111600100120      ************************************************************
111700100120      * Reperimento dati società
111800100120      ************************************************************
111900100120     C     REPSOC        BEGSR
112000100120      *
112100100120     C                   CALL      'SDGSOCDR'
112200100120     C                   PARM                    TIPXSC            6
112300100120     C                   PARM                    SOCXSC            3
112400100120     C                   PARM                    CDSXSC            9 0
112500100120     C                   PARM                    MODXSC            3
112600100120     C                   PARM      *BLANKS       RTNXSC            1
112700100120     C                   PARM                    XSOCDS
112800100120     C                   PARM                    KPJBA
112900100120      *
113000100120     C                   ENDSR
113100100120     C************************************************************
113200100120     C* INIZIALIZZAZIONE VARIABILI
113300100120     C************************************************************
113400100120     C     INZVAR        BEGSR
113500100120     C*
113600100120     C* Dati da PGM chiamante
113700100120     C*
113800100120     C                   MOVEL     KPJBU         XTABDS
113900100120     C*
114000100120     C* Reperimento codice lingua e codice azienda
114100100120     C*
114200100120     C                   MOVEL     'SOC001'      TIPXSC
114300100120     C                   EXSR      REPSOC
114400100120     C     RTNXSC        IFNE      '1'
114500100120     C                   MOVEL     XSOCDS        SOC001
114600100120     C                   ELSE
114700100120     C                   MOVE      *ON           $FINE
114800100120     C* fine programma
114900100120     C                   EXSR      ENDPGM
115000100120     C                   ENDIF
115100100120     C* Lingua e codice azienda
115200100120     C                   MOVE      XSCLIN        XtaLin
115300100120     C                   MOVE      XSCSOC        XtaAzi
115400100120     C*
115500100120     C                   RESET                   $FC2
115600100120     C*
115700160805     C                   CLOSE     SDGHK1D
115800160805     C                   OPEN      SDGHK1D
115900100120     C*
116000100120     C* Definizione delle opzioni utilizzabili a pgm
116100100120     C*
116200100120     C                   EXSR      DEFOPZ
116300100120     C* Pulizia campi e indicatori
116400100120     C                   CLEAR                   W1POS
116500100120     C                   MOVE      *ALL'0'       IN4049           10
116600100120     C                   MOVEA     IN4049        *IN(40)
116700100120     C                   CLEAR                   S1OPZ
116800100120     C* Variabili per gestione videate
116900100120     C*
117000100120     C                   MOVE      *OFF          $FINE
117100100120     C                   MOVE      *OFF          $INZS1
117200100120     C                   MOVE      *OFF          $EFILE
117300100120     C                   MOVE      *OFF          $ESCI
117400100120     C                   MOVE      *OFF          $RCDOK
117500100120     C                   MOVE      *HIVAL        $LASTV
117600100120     C                   Z-ADD     0             $ULKS1            3 0
117700100120     C*
117800100120     C                   MOVE      *ON           $INZS1
117900100120     C                   MOVE      'S1'          $GEST
118000100120     C*
118100100120     C* Variabili appoggio
118200100120     C*
118300100120     C                   Z-ADD     SFLPAG        WPAG
118400100120     C*
118500100120     C* Valorizzazione campi univoci testate
118600100120     C*
118700100120     C*                    CLEART1
118800100120     C*                    MOVELKNSIF     NOMSIF
118900100120     C*                    MOVELRAGUTE    NOMDIT
119000100120     C*                    MOVELDSPGM     SIGLA
119100100120     C*                    MOVELPROGR     NOMPGM
119200100120     C*
119300100120     c* posizione window
119400100120$012 C                   Z-ADD     44            PCOL              2 0
119500100120     C                   Z-ADD     09            R
119600100120     C     80            SUB       PCOL          C
119700100120     C                   SUB       3             C
119800100120     C     XtaOpz        IFNE      '  '
119900100120     C     XtaRig        IFGT      11
120000100120     C                   Z-ADD     1             R
120100100120     C                   ENDIF
120200100120     C*
120300100120     C     XtaCol        IFGT      40
120400100120     C                   Z-ADD     2             C
120500100120     C                   ENDIF
120600100120     C                   ENDIF
120700100120     C*
120800100120     C     XtaOpz        IFNE      '01'
120900100120     C                   DO        01            I                 3 0
121000100120     C                   MOVEL     LIN(I)        TBTFLD
121100100120     C                   CLEAR                   TBTKEY
121200100120     C                   CLEAR                   TBTAZI
121300100120     C                   CLEAR                   TBTLIN
121400160805     C                   MOVE      'HK1'         TBTCOD            3
121500100120     C     K5TBT1        CHAIN     ANTBT01L                           21
121600100120     C     *IN21         IFEQ      *ON
121700100120     C                   Z-ADD     LLUN(I)       TBTLUN
121800100120     C                   Z-ADD     LPOS(I)       TBTPOS
121900100120     C                   MOVEL     LIND(I)       TBTDES
122000100120     C                   WRITE     ANTBT000
122100100120     C                   ENDIF
122200100120     C                   ENDDO
122300100120     C                   ENDIF
122400100120     C*
122500100120     c*
122600100120     c* Se richiamato con OPZ 11  Non controlla abilitazione all'aziend
122700100120     C     XtaOpz        IFEQ      '11'
122800100120     C                   MOVE      'N'           XTBGAB
122900100120     C                   ELSE
123000100120     C                   MOVE      'S'           XTBGAB
123100100120     C                   ENDIF
123200100120     C                   ENDSR
123300100120     C/EJECT
123400100120     C************************************************************
123500100120     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
123600100120     C************************************************************
123700100120     C     DEFOPZ        BEGSR
123800100120     C*
123900100120     C* Questa routine deve valorizzare la schiera $FL1
124000100120     C* con S o N a seconda della utilizzabilità o meno
124100100120     C* dell'opzione correlata
124200100120     C*
124300100120     C* Inizialmente si considerano tutte le opzioni come non valide
124400100120     C     1             DO        14            X
124500100120     C                   MOVE      'N'           $FL1(X)
124600100120     C                   END
124700100120     C*
124800100120     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
124900100120     C* rendo valide alcune opzioni
125000100120     C* i tasti invece li disattivo
125100100120     C                   SELECT
125200100120     C*
125300100120     C* Se pgm richiamato in RICERCA attivo :
125400100120     C*
125500100120    >C     XtaOpz        WHENEQ    '01'
125600100120     C* F3=Fine
125700100120     C                   Z-ADD     3             X
125800100120     C                   MOVE      '0'           $FC2(X)
125900100120     C* F10=Immissione
126000100120     C                   Z-ADD     10            X
126100100120     C                   MOVE      '1'           $FC2(X)
126200100120     C* 1=Selezione
126300100120     C                   Z-ADD     1             X
126400100120     C     01            LOOKUP    $OP1(X)                                21
126500100120     C     *IN21         IFEQ      *ON
126600100120     C                   MOVE      'S'           $FL1(X)
126700100120     C                   ENDIF
126800100120     C* 2=Modifica
126900100120     C                   Z-ADD     1             X
127000100120     C     02            LOOKUP    $OP1(X)                                21
127100100120     C     *IN21         IFEQ      *ON
127200100120     C                   MOVE      'S'           $FL1(X)
127300100120     C                   ENDIF
127400100120     C* 3=Copia
127500100120     C                   Z-ADD     1             X
127600100120     C     03            LOOKUP    $OP1(X)                                21
127700100120     C     *IN21         IFEQ      *ON
127800100120     C                   MOVE      'S'           $FL1(X)
127900100120     C                   ENDIF
128000100120     C* 4=Annullamento
128100100120     C                   Z-ADD     1             X
128200100120     C     04            LOOKUP    $OP1(X)                                21
128300100120     C     *IN21         IFEQ      *ON
128400100120     C                   MOVE      'S'           $FL1(X)
128500100120     C                   ENDIF
128600100120     C* 5=Visualizzazione
128700100120     C                   Z-ADD     1             X
128800100120     C     05            LOOKUP    $OP1(X)                                21
128900100120     C     *IN21         IFEQ      *ON
129000100120     C                   MOVE      'S'           $FL1(X)
129100100120     C                   ENDIF
129200100120     C*14=Commenti
129300100120     C**                   Z-ADD1         X
129400100120     C**         14        LOKUP$OP1,X                   21
129500100120     C**         *IN21     IFEQ *ON
129600100120     C**                   MOVE 'S'       $FL1,X
129700100120     C**                   ENDIF
129800100120     C*
129900100120     C* Se pgm richiamato in INTERROGAZIONE attivo :
130000100120     C*
130100100120    >C     XtaOpz        WHENEQ    '05'
130200100120     C* F10=Immissione
130300100120     C                   Z-ADD     10            X
130400100120     C                   MOVE      '0'           $FC2(X)
130500100120     C* F3=Fine
130600100120     C                   Z-ADD     3             X
130700100120     C                   MOVE      '0'           $FC2(X)
130800100120     C* 5=Visualizzazione
130900100120     C                   Z-ADD     1             X
131000100120     C     05            LOOKUP    $OP1(X)                                21
131100100120     C     *IN21         IFEQ      *ON
131200100120     C                   MOVE      'S'           $FL1(X)
131300100120     C                   ENDIF
131400100120     C*14=Commenti
131500100120     C**                   Z-ADD1         X
131600100120     C**         14        LOKUP$OP1,X                   21
131700100120     C**         *IN21     IFEQ *ON
131800100120     C**                   MOVE 'S'       $FL1,X
131900100120     C**                   ENDIF
132000100120     C*
132100100120     C* Se pgm richiamato in ABILITAZIONE attivo :
132200100120     C*
132300100120    >C     XtaOpz        WHENEQ    '11'
132400100120     C* F10=Immissione
132500100120     C                   Z-ADD     10            X
132600100120     C                   MOVE      '0'           $FC2(X)
132700100120     C* F3=Fine
132800100120     C                   Z-ADD     3             X
132900100120     C                   MOVE      '0'           $FC2(X)
133000100120     C* 5=Visualizzazione
133100100120     C                   Z-ADD     1             X
133200100120     C     05            LOOKUP    $OP1(X)                                21
133300100120     C     *IN21         IFEQ      *ON
133400100120     C                   MOVE      'S'           $FL1(X)
133500100120     C                   ENDIF
133600100120     C* 11=Abilitazione
133700100120     C                   Z-ADD     1             X
133800100120     C     11            LOOKUP    $OP1(X)                                21
133900100120     C     *IN21         IFEQ      *ON
134000100120     C                   MOVE      'S'           $FL1(X)
134100100120     C                   ENDIF
134200100120     C* 12=Disabilita
134300100120     C                   Z-ADD     1             X
134400100120     C     12            LOOKUP    $OP1(X)                                21
134500100120     C     *IN21         IFEQ      *ON
134600100120     C                   MOVE      'S'           $FL1(X)
134700100120     C                   ENDIF
134800100120     C*
134900100120     C* Se pgm richiamato in GESTIONE  attivo :
135000100120     C*
135100100120    >C     XtaOpz        WHENEQ    '02'
135200100120     C* F3=Fine
135300100120     C                   Z-ADD     3             X
135400100120     C                   MOVE      '0'           $FC2(X)
135500100120     C* 2=Modifica
135600100120     C                   Z-ADD     1             X
135700100120     C     02            LOOKUP    $OP1(X)                                21
135800100120     C     *IN21         IFEQ      *ON
135900100120     C                   MOVE      'S'           $FL1(X)
136000100120     C                   ENDIF
136100100120     C* 3=Copia
136200100120     C                   Z-ADD     1             X
136300100120     C     03            LOOKUP    $OP1(X)                                21
136400100120     C     *IN21         IFEQ      *ON
136500100120     C                   MOVE      'S'           $FL1(X)
136600100120     C                   ENDIF
136700100120     C* 4=Annullamento
136800100120     C                   Z-ADD     1             X
136900100120     C     04            LOOKUP    $OP1(X)                                21
137000100120     C     *IN21         IFEQ      *ON
137100100120     C                   MOVE      'S'           $FL1(X)
137200100120     C                   ENDIF
137300100120     C* 5=Visualizzazione
137400100120     C                   Z-ADD     1             X
137500100120     C     05            LOOKUP    $OP1(X)                                21
137600100120     C     *IN21         IFEQ      *ON
137700100120     C                   MOVE      'S'           $FL1(X)
137800100120     C                   ENDIF
137900100120     C*14=Commenti
138000100120     C**                   Z-ADD1         X
138100100120     C**         14        LOKUP$OP1,X                   21
138200100120     C**         *IN21     IFEQ *ON
138300100120     C**                   MOVE 'S'       $FL1,X
138400100120     C**                   ENDIF
138500100120     C*
138600100120     C                   OTHER
138700100120     C* Se pgm non richiamato, attivo TUTTO  :
138800100120     C* 2=Modifica
138900100120     C                   Z-ADD     1             X
139000100120     C     02            LOOKUP    $OP1(X)                                21
139100100120     C     *IN21         IFEQ      *ON
139200100120     C                   MOVE      'S'           $FL1(X)
139300100120     C                   ENDIF
139400100120     C* 3=Copia
139500100120     C                   Z-ADD     1             X
139600100120     C     03            LOOKUP    $OP1(X)                                21
139700100120     C     *IN21         IFEQ      *ON
139800100120     C                   MOVE      'S'           $FL1(X)
139900100120     C                   ENDIF
140000100120     C* 4=Annullamento
140100100120     C                   Z-ADD     1             X
140200100120     C     04            LOOKUP    $OP1(X)                                21
140300100120     C     *IN21         IFEQ      *ON
140400100120     C                   MOVE      'S'           $FL1(X)
140500100120     C                   ENDIF
140600100120     C* 5=Visualizzazione
140700100120     C                   Z-ADD     1             X
140800100120     C     05            LOOKUP    $OP1(X)                                21
140900100120     C     *IN21         IFEQ      *ON
141000100120     C                   MOVE      'S'           $FL1(X)
141100100120     C                   ENDIF
141200100120     C* 11=Abilitazione
141300100120     C                   Z-ADD     1             X
141400100120     C     11            LOOKUP    $OP1(X)                                21
141500100120     C     *IN21         IFEQ      *ON
141600100120     C                   MOVE      'S'           $FL1(X)
141700100120     C                   ENDIF
141800100120     C* 12=Disabilita
141900100120     C                   Z-ADD     1             X
142000100120     C     12            LOOKUP    $OP1(X)                                21
142100100120     C     *IN21         IFEQ      *ON
142200100120     C                   MOVE      'S'           $FL1(X)
142300100120     C                   ENDIF
142400100120     C*14=Commenti
142500100120     C**                   Z-ADD1         X
142600100120     C**         14        LOKUP$OP1,X                   21
142700100120     C**         *IN21     IFEQ *ON
142800100120     C**                   MOVE 'S'       $FL1,X
142900100120     C**                   ENDIF
143000100120     C* poi, disattivo a seconda delle autorizzazioni :
143100100120    >C*********  AUTXXX    IFEQ 'N'
143200100120    >C*********            Z-ADD1         X
143300100120    >C*********  'XX'      LOKUP$OP1,X                   21
143400100120    >C*********  *IN21     IFEQ *ON
143500100120    >C*********            MOVE 'N'       $FL1,X
143600100120    >C*********            ENDIF
143700100120    >C*********            ENDIF
143800100120     C*
143900100120     C                   ENDSL
144000100120     C*
144100100120     C                   ENDSR
144200100120     C/EJECT
144300100120     C************************************************************
144400100120     C* REPERIMENTO OPZIONI
144500100120     C************************************************************
144600100120     C     REPOPZ        BEGSR
144700100120     C*
144800100120     C     1             DO        $DIMOP        X
144900100120     C*
145000100120     C     $MS1(X)       IFNE      *BLANK
145100100120     C*
145200100120     C                   MOVEL     $MS1(X)       IDMSG
145300100120     C                   CALLB     'XRTVM'
145400100120     C                   PARM                    IDMSG            27
145500100120     C                   PARM                    TXTMSG          100
145600100120     C                   PARM                    ERRMSG            1
145700100120     C*
145800100120     C     ERRMSG        IFNE      *ON
145900100120     C                   MOVEL     TXTMSG        $TX1(X)
146000100120     C                   ELSE
146100100120     C                   MOVEL     *ALL'?'       $TX1(X)
146200100120     C                   END
146300100120     C*
146400100120     C                   END
146500100120     C*
146600100120     C                   END
146700100120     C*
146800100120     C                   ENDSR
146900100120     C/EJECT
147000100120     C************************************************************
147100100120     C* REPERIMENTO TASTI DI FUNZIONE
147200100120     C************************************************************
147300100120     C     REPKEY        BEGSR
147400100120     C*
147500100120     C                   MOVEA     $FM2          $FM
147600100120     C                   EXSR      REPKKK
147700100120     C                   MOVEA     $FX           $FX2
147800100120     C                   ENDSR
147900100120     C/EJECT
148000100120     C************************************************************
148100100120     C* REPERIMENTO TASTI DI FUNZIONE
148200100120     C************************************************************
148300100120     C     REPKKK        BEGSR
148400100120     C*
148500100120     C                   CLEAR                   $FX
148600100120     C     1             DO        24            X
148700100120     C*
148800100120     C     $FM(X)        IFNE      *BLANK
148900100120     C*
149000100120     C                   MOVEL     $FM(X)        IDMSG
149100100120     C                   CALLB     'XRTVM'
149200100120     C                   PARM                    IDMSG            27
149300100120     C                   PARM                    TXTMSG          100
149400100120     C                   PARM                    ERRMSG            1
149500100120     C*
149600100120     C     ERRMSG        IFNE      *ON
149700100120     C                   MOVEL     TXTMSG        $FX(X)
149800100120     C                   ELSE
149900100120     C                   MOVEL     *ALL' '       $FX(X)
150000100120     C                   END
150100100120     C*
150200100120     C                   END
150300100120     C*
150400100120     C                   END
150500100120     C*
150600100120     C                   ENDSR
150700100120     C/EJECT
150800100120     C************************************************************
150900100120     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
151000100120     C************************************************************
151100100120     C     DEFCAM        BEGSR
151200100120     C*
151300100120     C* Variabili per gestione videate
151400100120     C*
151500100120     C                   MOVE      *BLANK        $GEST             2
151600100120     C                   MOVE      *BLANK        $FINE             1
151700100120     C                   MOVE      *BLANK        $INZS1            1
151800100120     C                   MOVE      *BLANK        $LASTG            2
151900100120     C                   MOVE      *BLANK        $LASTV            2
152000100120     C                   MOVE      *BLANK        $EFILE            1
152100100120     C                   MOVE      *BLANK        $ESCI             1
152200100120     C                   MOVE      *BLANK        $RCDOK            1
152300100120     C*
152400100120     C* Variabili appoggio sempre presenti in tutte le anagrafiche
152500100120     C*
152600100120     C     *LIKE         DEFINE    C1RCD         S1NRR
152700100120     C     *LIKE         DEFINE    C1NRR         WSFL
152800100120     C     *LIKE         DEFINE    C1RCD         WMAX
152900100120     C     *LIKE         DEFINE    C1RCD         WPAG
153000100120     C     *LIKE         DEFINE    $INZS1        WINZS1
153100100120     C                   MOVE      *BLANK        WWGRP             3
153200100120     C                   Z-ADD     0             CURR              2 0
153300100120     C                   Z-ADD     0             CURC              2 0
153400100120     C                   MOVE      *BLANK        WIN              99
153500100120     C     *LIKE         DEFINE    C1POS         W1POS
153600100120     C*
153700100120$022 C     K5TBT1        KLIST
153800100120$022 C                   KFLD                    TBTAZI
153900100120$022 C                   KFLD                    TBTCOD
154000100120$022 C                   KFLD                    TBTKEY
154100100120$022 C                   KFLD                    TBTLIN
154200100120$022 C                   KFLD                    TBTFLD
154300100120     C*
154400100120     C     KABL          KLIST
154500100120     C                   KFLD                    ABLCOD
154600100120     C                   KFLD                    ABLKEY
154700100120     C                   KFLD                    ABLAZI
154800100120     C*
154900100120     C*
155000100120     C* Indici
155100100120     C*
155200100120     C                   Z-ADD     0             X                 3 0
155300100120     C                   Z-ADD     0             Y                 3 0
155400100120     C*
155500100120     C                   ENDSR
155600100120     C/EJECT
155700100120**
155800100120PROMSG    *LIBL     OPZ0001
155900100120PROMSG    *LIBL     OPZ0002
156000100120PROMSG    *LIBL     OPZ0003
156100100120PROMSG    *LIBL     OPZ0004
156200100120PROMSG    *LIBL     OPZ0005
156300100120PROMSG    *LIBL     OPZ0010
156400100120PROMSG    *LIBL     OPZ0013
156500100120PROMSG    *LIBL     OPZ0012
156600100120PROMSG    *LIBL     OPZ0014
156700100120**
15680010012001S      <Selezione
15690010012002S      <Modifica
15700010012003S      <Copia
15710010012004S      <Annullamento
15720010012005S      <Visualizzazione
15730010012010S      <Invia
15740010012011S      <Abilita gruppo
15750010012012S      <Disabilita
15760010012014N      <Commenti
157700100120** TASTI DI FUNZIONE  S1
157800100120PROMSG    *LIBL     KEY0001  01
157900100120PROMSG    *LIBL     KEY0002  02
158000100120PROMSG    *LIBL     KEY0003  03
158100100120PROMSG    *LIBL     KEY0004  04
158200100120PROMSG    *LIBL     KEY0005  05
158300100120PROMSG    *LIBL     KEY0006  06
158400100120PROMSG    *LIBL     KEY0007  07
158500100120PROMSG    *LIBL     KEY0008  08
158600100120PROMSG    *LIBL     KEY0009  09
158700100120PROMSG    *LIBL     KEY0010  10
158800100120PROMSG    *LIBL     KEY0011  11
158900100120PROMSG    *LIBL     KEY0012  12
159000100120PROMSG    *LIBL     KEY0013  13
159100100120PROMSG    *LIBL     KEY0014  14
159200100120PROMSG    *LIBL     KEY0015  15
159300100120PROMSG    *LIBL     KEY0016  16
159400100120SDGMSG    *LIBL     SDK0001  17
159500100120PROMSG    *LIBL     KEY0018  18
159600100120PROMSG    *LIBL     KEY0019  19
159700100120PROMSG    *LIBL     KEY0020  20
159800100120PROMSG    *LIBL     KEY0021  21
159900100120PROMSG    *LIBL     KEY0022  22
160000100120PROMSG    *LIBL     KEY0023  23
160100100120PROMSG    *LIBL     KEY0024  24
160200100120**  ABILITAZIONE S1
1603001001200N  01
1604001001200N  02
1605001001201N  03
1606001001200N  04
1607001001200N  05
1608001001200N  06
1609001001200N  07
1610001001200N  08
1611001001200N  09
1612001001201N  10
1613001001200N  11
1614001001201N  12
1615001001200N  13
1616001001200N  14
1617001001200N  15
1618001001200N  16
1619001001200N  17
1620001001200N  18
1621001001200N  19
1622001001200N  20
1623001001200N  21
1624001001200N  22
1625001001200N  23
1626001001200N  24
162700100120**
162800100120DESCR Descrizione ................
162900100120**
163000100120001020
