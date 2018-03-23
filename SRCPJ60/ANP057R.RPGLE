000100000704     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200000704     H*PARMS CVTOPT(*NONE)
000300000704     H DECEDIT('0,') DATEDIT(*DMY.)
000400000704      *
000500000704    > *            GESTIONE DATI PARAMETRI: NDCL1S solleciti
000600000704      *
000700000704     F*----------------------------------------------------*
000800000705    >FANDOM01L  IF   E           K DISK
000900000705     F*------------
001000000704    >FANP057D   CF   E             WORKSTN
001100000704     F                                     RENAME(P057D1:FMTD1)
001200000704     F                                     RENAME(P057D2:FMTD2)
001300090305c2125F                                     RENAME(P057D3:FMTD3)
001400131127C2232F                                     RENAME(P057D4:FMTD4)
001500000704     F                                     RENAME(P057T1:FMTT1)
001600000704     F                                     RENAME(P057Z1:FMTZ1)
001700000704     F                                     INFDS(DSFMT)
001800000704     D*----------------------------------------------------*
001900000704     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
002000000704     D $ID             S             27    DIM(5) ALT($OZ)
002100000704     D* Schiere per la gestione dei tasti di funzione
002200000704     D $FM             S             27    DIM(24)
002300000704     D $FX             S             50    DIM(24)
002400000704     D $FC             S              1    DIM(24)
002500000704     D $FR             S              1    DIM(24)
002600090305     D* Video D1/D3
002700131127C2232D* fino a D4
002800000704     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
002900000704     D $FX1            S             50    DIM(24) ALT($FM1)
003000000704     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
003100000704     D $FR1            S              1    DIM(24) ALT($FC1)
003200000704     D*--------------------
003300000704     D* Sequenza tipi riferimenti anagrafici
003400000704     D TpRif           S              3    DIM(8)
003500000704     D $X              S              1  0
003600000704     D $X1             S              1  0
003700000704     D $Y              S              1  0
003800000704     D*----------------------------------------------------*
003900000704     D* Passaggio Parametri
004000000704     D KPJBA         E DS
004100000704     D*-------------
004200000704     D* Parametri ricevuti
004300000704     D ANPAR         E DS                  EXTNAME(ANPARDS)
004400000704     D  ANPSOC       E                     EXTFLD(ANPSOCIETA)
004500000704     D  ANPPAR       E                     EXTFLD(ANPPARAME)
004600000704     D  ANPDAT       E                     EXTFLD(ANPDATI)
004700000704     D                                     DIM(2000)
004800000704     D  ANPERR       E                     EXTFLD(ANPERRORE)
004900000704     D*-------------
005000000704     D* Dati per pgm XSOC
005100000704     D SOC001        E DS                  EXTNAME(XSOC001DS)
005200000704     D    Fgo                         1    dim(50) overlay(XscFgO)
005300000705     D    XScNrDecMC                  1S 0 Overlay(XScSkP:6)
005400000705     D*-------------
005500000705     D* appoggio per calcoli decimali
005600000705     D W1903           S             19P 3
005700000704     D*-------------
005800000704      * DS Interna per dati utente
005900000704     D XSOCDS          DS          1000
006000000704     D*-------------
006100000704     D* Ds esterna dati
006200000705     D ANP057DS      E DS                  INZ
006300000704     D  $DT                    1   2000
006400000704     D                                     DIM(2000)
006500051103     D*-------------
006600051103     D* Parametri per ricerca/controllo codice ANATB00F
006700051103c1879D XTABDS        E DS                  INZ
006800051103c1879D XATBDS        E DS                  INZ
006900000704     D*-------------
007000000704     D* Ricerca legende
007100000704     D Ana964DS      E DS                  INZ
007200000704     D*-------------
007300000704     D* Controlli validità legende
007400000704     D X07DS         E DS                  EXTNAME(X07VALDS) INZ
007500000704     D*-------------
007600000704     D* Domini
007700000704     D ANA940Ds      E DS                  Inz
007800000704     D*-------------
007900051103C1879D* DS tabella posizioni attributi contabili
008000051103C1879D Ang0A2Ds      E DS                  Inz
008100000704     D*-------------
008200090305C2125D* DS tabella segnalazioni credito
008300090305  "  D Ang044Ds      E DS                  Inz
008400090305  "  D*-------------
008500090305  "  D* DS tabella blocchi
008600090422  "  D Ang0ABDs      E DS                  Inz
008700090422  "  D*-------------
008800090422  "  D* DS tabella commenti
008900090422C2125D AngG20Ds      E DS                  Inz
009000090305     D*-------------
009100000704     D DSFMT           DS
009200000704     D  $TASTO               369    369
009300000704     D  NRG                  370    370
009400000704     D  NCL                  371    371
009500000704     D*-------------
009600000704     D* posizione cursore
009700000704     D CURSOR          DS
009800000704     D  RIRI                   1      2B 0 INZ
009900000704     D  R$$                    2      2
010000000704     D  COCO                   3      4B 0 INZ
010100000704     D  C$$                    4      4
010200000704     D*-------------
010300000704     D* Reperimento nome PGM
010400000704     D STATUS         SDS           333
010500000704     D  DSPGM            *PROC
010600000704     D*-------------
010700000704     D* Costanti
010800000704     D*-------------
010900000704     D* Tasti di funzione
011000000704     D F01             C                   CONST(X'31')
011100000704     D F02             C                   CONST(X'32')
011200000704     D F03             C                   CONST(X'33')
011300000704     D F04             C                   CONST(X'34')
011400000704     D F05             C                   CONST(X'35')
011500000704     D F06             C                   CONST(X'36')
011600000704     D F07             C                   CONST(X'37')
011700000704     D F08             C                   CONST(X'38')
011800000704     D F09             C                   CONST(X'39')
011900000704     D F10             C                   CONST(X'3A')
012000000704     D F11             C                   CONST(X'3B')
012100000704     D F12             C                   CONST(X'3C')
012200000704     D F13             C                   CONST(X'B1')
012300000704     D F14             C                   CONST(X'B2')
012400000704     D F15             C                   CONST(X'B3')
012500000704     D F16             C                   CONST(X'B4')
012600000704     D F17             C                   CONST(X'B5')
012700000704     D F18             C                   CONST(X'B6')
012800000704     D F19             C                   CONST(X'B7')
012900000704     D F20             C                   CONST(X'B8')
013000000704     D F21             C                   CONST(X'B9')
013100000704     D F22             C                   CONST(X'BA')
013200000704     D F23             C                   CONST(X'BB')
013300000704     D F24             C                   CONST(X'BC')
013400000704     D ENTER           C                   CONST(X'F1')
013500000704     D ROLDWN          C                   CONST(X'F4')
013600000704     D ROLLUP          C                   CONST(X'F5')
013700000704     D*
013800000710     D*-------------
013900000710     D* PROTOTIPAZIONE DELLE PROCEDURE
014000000710     D/COPY *LIBL/SRCCPY,PJX002PR
014100000704     C*----------------------------------------------------*
014200000704     C*                MAIN LINE PROGRAM
014300000704     C*----------------------------------------------------*
014400000704     C*
014500000704     C* inizializzazione variabili
014600000704     C                   EXSR      INZVAR
014700000704     C*
014800000704     C     $FINE         DOWEQ     *OFF
014900000704     C     $GEST         CASEQ     'D1'          GESD1
015000000704     C     $GEST         CASEQ     'D2'          GESD2
015100090305c2125C     $GEST         CASEQ     'D3'          GESD3
015200131127c2232C     $GEST         CASEQ     'D4'          GESD4
015300000704     C                   ENDCS
015400000704     C                   ENDDO
015500000704     C*
015600000704     C* fine programma
015700000704     C                   EXSR      ENDPGM
015800000704     C/EJECT
015900000704     C************************************************************
016000000704     C* RICERCHE
016100000704     C************************************************************
016200000704     C     ENDPGM        BEGSR
016300000704     C*
016400000704     C                   SETON                                            RT
016500000704     C                   RETURN
016600000704     C*
016700000704     C                   ENDSR
016800000704     C/EJECT
016900000704     C************************************************************
017000000704     C* RICERCHE
017100000704     C************************************************************
017200000704     C     SEARCH        BEGSR
017300000704     C*
017400000704     C* determino Riga/Colonna del cursore
017500000704     C                   MOVE      NRG           R$$
017600000704     C                   MOVE      NCL           C$$
017700000704     C                   Z-ADD     RIRI          CURR
017800000704     C                   Z-ADD     COCO          CURC
017900000704     C*
018000000704     C* I campi che al momento dell'F4 risultano protetti non devono
018100000704     C* essere abilitati alla funzione di ricerca
018200000704     C*
018300000704     C                   SELECT
018400000704     C*
018500000704    >C* Dominio per selezioni anagrafiche o commerciali
018600000704     C     H1NMFL        WHENEQ    'D1DOMANA'
018700000704     C     H1NMFL        OREQ      'D1DOMCOM'
018800000704     C                   RESET                   ANA940DS
018900000704     C                   MOVEL     '01'          OPZ940
019000000704     C                   SELECT
019100000704     C     H1NMFL        WHENEQ    'D1DOMANA'
019200000704     C                   MOVE      D1DOMANA      DOM940
019300000704     C     H1NMFL        WHENEQ    'D1DOMCOM'
019400000704     C                   MOVE      D1DOMCOM      DOM940
019500000704     C                   ENDSL
019600000704     C                   MOVEL(P)  ANA940DS      KPJBU
019700000704     C                   CALL      'ANA940R'
019800000704     C                   PARM                    KPJBA
019900000704     C                   MOVEL     KPJBU         ANA940DS
020000000704     C*
020100000704     C     DOM940        IFNE      *BLANK
020200000704     C                   SELECT
020300000704     C     H1NMFL        WHENEQ    'D1DOMANA'
020400000704     C                   MOVEL     DOM940        D1DOMANA
020500000704     C     H1NMFL        WHENEQ    'D1DOMCOM'
020600000704     C                   MOVEL     DOM940        D1DOMCOM
020700000704     C                   ENDSL
020800000704     C                   ENDIF
020900000704     C*
021000000705     C* Agente
021100000705     C     H1NMFL        WHENEQ    'D1GESAGE'
021200000704     C                   RESET                   ANA964DS
021300000704     C                   MOVEL     '01'          OPZ964
021400000704     C                   MOVEL     'ANDIZ000'    TRC964
021500000704     C                   MOVEL     'DSPTPAGE'    CAM964
021600000704     C                   MOVE      *OFF          ALL964
021700000704     C                   MOVEL(P)  ANA964DS      KPJBU
021800000704     C                   CALL      'ANA964R'
021900000704     C                   PARM                    KPJBA
022000000705     C                   MOVEL     KPJBU         ANA964DS
022100000704     C*
022200000704     C     RET964        IFEQ      *BLANK
022300000705     C                   MOVEL     VAL964        D1GESAGE
022400000704     C                   ENDIF
022500140320     C*
022600140320     C* TIPO CALCOLO GIORNI
022700140320     C     H1NMFL        WHENEQ    'D4CALCGG'
022800140320     C                   RESET                   ANA964DS
022900140320     C                   MOVEL     '01'          OPZ964
023000140320     C                   MOVEL     'ANBCO000'    TRC964
023100140320     C                   MOVEL     'BCOCALDTV'   CAM964
023200140320     C                   MOVE      *OFF          ALL964
023300140320     C                   MOVEL(P)  ANA964DS      KPJBU
023400140320     C                   CALL      'ANA964R'
023500140320     C                   PARM                    KPJBA
023600140320     C                   MOVEL     KPJBU         ANA964DS
023700140320     C*
023800140320     C     RET964        IFEQ      *BLANK
023900140320     C                   MOVEL     VAL964        D4CALCGGD
024000140320     C                   ENDIF
024100000704     C*
024200030123C1619C* giorni
024300030123  "  C     H1NMFL        WHENEQ    'D2DIVISOR'
024400030123  "  C                   RESET                   ANA964DS
024500030123  "  C                   MOVEL     '01'          OPZ964
024600030123  "  C                   MOVEL     'ANDIZ000'    TRC964
024700030123  "  C                   MOVEL     'DSPGGINT'    CAM964
024800030123  "  C                   MOVE      *OFF          ALL964
024900030123  "  C                   MOVEL(P)  ANA964DS      KPJBU
025000030123  "  C                   CALL      'ANA964R'
025100030123  "  C                   PARM                    KPJBA
025200030123  "  C                   MOVEL     KPJBU         ANA964DS
025300030123  "  C*
025400030123  "  C     RET964        IFEQ      *BLANK
025500030123  "  C                   MOVEL     VAL964        D2DIVISOR
025600030123  "  C                   MOVEL     DES964        D2DIVISORD
025700030123C1619C                   ENDIF
025800000705     C* Annullamento partite
025900000705     C     H1NMFL        WHENEQ    'D1ANNPAR'
026000000705     C                   RESET                   ANA964DS
026100000704     C                   MOVEL     '01'          OPZ964
026200000704     C                   MOVEL     'ANDIZ000'    TRC964
026300000704     C                   MOVEL     'DSPTPANN'    CAM964
026400000704     C                   MOVE      *OFF          ALL964
026500000704     C                   MOVEL(P)  ANA964DS      KPJBU
026600000704     C                   CALL      'ANA964R'
026700000704     C                   PARM                    KPJBA
026800000705     C                   MOVEL     KPJBU         ANA964DS
026900000704     C*
027000000704     C     RET964        IFEQ      *BLANK
027100000705     C                   MOVEL     VAL964        D1ANNPAR
027200000704     C                   ENDIF
027300000704     C*
027400000704     C* Modalità stampa lettera
027500000705     C     H1NMFL        WHENEQ    'D2STPLET'
027600000705     C                   RESET                   ANA964DS
027700000704     C                   MOVEL     '01'          OPZ964
027800000704     C                   MOVEL     'ANDIZ000'    TRC964
027900000704     C                   MOVEL     'DSPTPLETTS'  CAM964
028000000704     C                   MOVE      *OFF          ALL964
028100000704     C                   MOVEL(P)  ANA964DS      KPJBU
028200000704     C                   CALL      'ANA964R'
028300000704     C                   PARM                    KPJBA
028400000705     C                   MOVEL     KPJBU         ANA964DS
028500000704     C*
028600000704     C     RET964        IFEQ      *BLANK
028700000705     C                   MOVEL     VAL964        D2STPLET
028800000704     C                   ENDIF
028900001003     C*
029000001003     C* Tipo gravità
029100001003     C     H1NMFL        WHENEQ    'D2TIPGRA'
029200001003     C                   RESET                   ANA964DS
029300001003     C                   MOVEL     '01'          OPZ964
029400001003     C                   MOVEL     'ANDIZ000'    TRC964
029500001003     C                   MOVEL     'DSPTIPGRA'   CAM964
029600001003     C                   MOVE      *ON           ALL964
029700001003     C                   MOVEL(P)  ANA964DS      KPJBU
029800001003     C                   CALL      'ANA964R'
029900001003     C                   PARM                    KPJBA
030000001003     C                   MOVEL     KPJBU         ANA964DS
030100001003     C*
030200001003     C     RET964        IFEQ      *BLANK
030300001003     C                   MOVEL     VAL964        D2TIPGRA
030400001003     C                   ENDIF
030500030123     C*
030600051103C1879C* Posizione attributo contabile
030700051103  "  C     H1NMFL        WHENEQ    'D2POSATTR'
030800051103  "  C                   reset                   xtabds
030900051103  "  C                   MOVE      '01'          xtaopz
031000051103  "  C                   MOVE      XSCSOC        xtaazi
031100051103  "  C                   MOVE      XSCLIN        xtalin
031200051103  "  C                   CLEAR                   KPJBU
031300051103  "  C                   MOVEL     XTABDS        KPJBU
031400051103  "  C                   CALL      'ANG0A2R'
031500051103  "  C                   PARM                    KPJBA
031600051103  "  C                   MOVEL     KPJBU         XTABDS
031700051103  "  C     XtaRet        IFNE      '2'
031800051103  "  C                   MOVE      xtakey        D2POSATTR
031900051103C1879c                   endif
032000090305C2125C* regola segnalazioni
032100090305  "  C     H1NMFL        WHENEQ    'D3REGCRE'
032200090305  "  C     H1NMFL        oreq      'D3IMPCRE'
032300090305  "  C                   RESET                   XTABDS
032400090305  "  C                   MOVE      '01'          XTAOPZ
032500090305  "  C                   MOVE      XSCSOC        XTAAZI
032600090305  "  C                   MOVE      XSCLIN        XTALIN
032700090305  "  C                   CLEAR                   KPJBU
032800090305  "  C                   MOVEL     XTABDS        KPJBU
032900090305  "  C                   CALL      'ANG044R'
033000090305  "  C                   PARM                    KPJBA
033100090305  "  C                   MOVEL     KPJBU         XTABDS
033200090305  "  C     XTARET        IFNE      '2'
033300090305  "  C                   if        H1Nmfl='D3REGCRE'
033400090305  "  C                   MOVE      XTAKEY        D3RegCrE
033500090305  "  C                   else
033600090305  "  C                   MOVE      XTAKEY        D3ImpCrE
033700090305  "  c                   endif
033800090305  "  C                   ENDIF
033900090305  "  C* tabella blocchi
034000090305  "  C     H1NMFL        WHENEQ    'D3BKLSCA'
034100090305  "  C                   RESET                   XTABDS
034200090305  "  C                   MOVE      '01'          XTAOPZ
034300090305  "  C                   MOVE      XSCSOC        XTAAZI
034400090305  "  C                   MOVE      XSCLIN        XTALIN
034500090305  "  C                   CLEAR                   KPJBU
034600090305  "  C                   MOVEL     XTABDS        KPJBU
034700090305  "  C                   CALL      'ANG0ABR'
034800090305  "  C                   PARM                    KPJBA
034900090305  "  C                   MOVEL     KPJBU         XTABDS
035000090305  "  C     XTARET        IFNE      '2'
035100090305  "  C                   MOVE      XTAKEY        D3BklSca
035200090305C2125C                   ENDIF
035300000704     c*
035400000704     C* ricerca non abilitata in questa posizione
035500000704     C                   OTHER
035600000704     C                   SETON                                        9899
035700000704     C*
035800000704     C                   ENDSL
035900000704     C*
036000000704     C* imposto pos. del cursore
036100000704     C                   Z-ADD     CURR          H1RIGA
036200000704     C                   Z-ADD     CURC          H1COLO
036300000704     C                   MOVE      *HIVAL        $LASTV
036400000704     c*
036500000704     C                   ENDSR
036600000704     C/EJECT
036700000704     C************************************************************
036800000704     C* GESTIONE EMISSIONE TASTI FUNZIONE
036900000704     C************************************************************
037000000704     C     GESPIE        BEGSR
037100000704     C*
037200000704     C                   SELECT
037300000704     C*
037400000704     C     $GEST         WHENEQ    'D1'
037500000704     C     $ULKD1        IFEQ      0
037600000704     C* riempimento opzioni
037700000704     C                   EXSR      F24D1
037800000704     C                   ELSE
037900000704     C                   WRITE     FMTZ1
038000000704     C                   ENDIF
038100000704     C*
038200000704     C     $GEST         WHENEQ    'D2'
038300000704     C     $ULKD1        IFEQ      0
038400000704     C* riempimento opzioni
038500000704     C                   EXSR      F24D2
038600000704     C                   ELSE
038700000704     C                   WRITE     FMTZ1
038800000704     C                   ENDIF
038900090305C2125C*
039000090305  "  C     $GEST         WHENEQ    'D3'
039100090305  "  C     $ULKD1        IFEQ      0
039200090305  "  C* riempimento opzioni
039300090305  "  C                   EXSR      F24D3
039400090305  "  C                   ELSE
039500090305  "  C                   WRITE     FMTZ1
039600090305C2125C                   ENDIF
039700131127C2232C*
039800131127  "  C     $GEST         WHENEQ    'D4'
039900131127  "  C     $ULKD1        IFEQ      0
040000131127  "  C* riempimento opzioni
040100131127  "  C                   EXSR      F24D4
040200131127  "  C                   ELSE
040300131127  "  C                   WRITE     FMTZ1
040400131127C2232C                   ENDIF
040500000704     C                   ENDSL
040600000704     C*
040700000704     C                   ENDSR
040800000704     C/EJECT
040900000704     C************************************************************
041000000704     C* GESTIONE VIDEO RECORD D1
041100000704     C************************************************************
041200000704     C     GESD1         BEGSR
041300000704     C* inizializzazione videata
041400000704   1BC     $INZD1        IFEQ      *ON
041500000704     C                   EXSR      INZD1
041600000704     C                   MOVE      *OFF          $INZD1
041700000704   1EC                   ENDIF
041800000704     C* emissione piede videata se proveniente da altra
041900000704     C* salvataggio valore formato di provenienza
042000000704   1BC     $LASTV        IFNE      'D1'
042100000704     C                   WRITE     FMTT1
042200000704     C                   EXSR      GESPIE
042300000704     C                   MOVE      $LASTV        $LASTG
042400000704     C                   MOVE      'D1'          $LASTV
042500000704   1EC                   ENDIF
042600000704     C*
042700000704     C* se esistono errori sulla videata
042800000704     C* emetto la write del formato a indicatori spenti per vedere
042900000704     C* le eventuali decodifiche
043000000704   1BC     *IN99         IFEQ      *ON
043100000704     C     $GEST         ANDEQ     'D1'
043200000704     C                   MOVEA     *IN           WIN
043300000704     C                   MOVE      *ALL'0'       IN5098           49
043400000704     C                   MOVEA     IN5098        *IN(50)
043500000704     C                   WRITE     FMTD1
043600000704     C                   MOVEA     WIN           *IN
043700000704   1EC                   ENDIF
043800000704     C*
043900000704   1BC                   SELECT
044000000704     C* Annullamento
044100000704    >C     ANPOPZ        WHENEQ    '04'
044200000704     C* Visualizzazione
044300000704    >C     ANPOPZ        OREQ      '05'
044400000704     C                   WRITE     FMTD1
044500000704     C                   EXFMT     PROTECT
044600000704     C* Immissione/Modifica/Copia
044700000704   1XC                   OTHER
044800000704     C                   EXFMT     FMTD1
044900000704   1EC                   ENDSL
045000000704     C* controllo abilitazione tasti funzione
045100000704     C*
045200000704     C                   EXSR      CTRKD1
045300000704     C*
045400000704   1BC     *IN99         IFEQ      *OFF
045500000704     C*
045600000704   2BC                   SELECT
045700000704     C* ROLLUP
045800000704    >C     $TASTO        WHENEQ    ROLLUP
045900000704    >C                   MOVE      'D2'          $GEST
046000000704     C* F2=Decodifica
046100000704     C     $TASTO        WHENEQ    F02
046200000704     C                   EXSR      F02D1
046300000704     C* F3=Fine
046400000704     C     $TASTO        WHENEQ    F03
046500000704     C                   EXSR      F03D1
046600000704     C* F4=Ricerca
046700000704     C     $TASTO        WHENEQ    F04
046800000704     C                   EXSR      SEARCH
046900000704     C* F5=Refresh
047000000704     C     $TASTO        WHENEQ    F05
047100000704     C                   EXSR      F05D1
047200000704     C* F8=Successivo
047300000704     C     $TASTO        WHENEQ    F08
047400000704     C                   EXSR      F08D1
047500000704     C* F12=Ritorno
047600000704     C     $TASTO        WHENEQ    F12
047700000704     C                   EXSR      F12D1
047800000704     C* F24=Altri tasti
047900000704     C     $TASTO        WHENEQ    F24
048000000704     C                   EXSR      F24D1
048100000704     C*
048200000704   2XC                   OTHER
048300000704     C                   EXSR      CTRD1
048400000704     C* aggiungere le chiamate ai controlli delle videate supplementari
048500000704    >C     *IN99         IFEQ      *OFF
048600000704    >C                   EXSR      CTRD2
048700090305C2125C  N99              EXSR      CTRD3
048800131127C2232C  N99              EXSR      CTRD4
048900000704    >C     *IN99         IFEQ      *ON
049000000704    >C* Spengo gli indicatori di errore per il controllo su altri schermi
049100000704    >C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
049200000704    >C* videate
049300000704    >C                   MOVE      *ALL'0'       IN5098           49
049400000704    >C                   MOVEA     IN5098        *IN(50)
049500000704    >C                   SETON                                        9996
049600000704    >C                   ENDIF
049700000704    >C                   ENDIF
049800000704     C*
049900000704   3BC     $TASTO        IFEQ      F06
050000000704     C     *IN99         ANDEQ     *OFF
050100000704     C* eseguo operazioni per aggiornamento
050200000704   4BC     *IN99         IFEQ      *OFF
050300000704     C                   EXSR      GESAGG
050400000704   4EC                   ENDIF
050500000704   3EC                   ENDIF
050600000704     C*
050700000704   2EC                   ENDSL
050800000704     C*
050900000704   1EC                   ENDIF
051000000704     C*
051100000704     C                   ENDSR
051200000704     C/EJECT
051300000704     C************************************************************
051400000704     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
051500000704     C************************************************************
051600000704     C     CTRKD1        BEGSR
051700000704     C                   SETOFF                                       99
051800000704     C                   MOVEA     $FC1          $FC
051900000704     C                   EXSR      CTRKEY
052000000704     C     *IN99         IFEQ      *ON
052100000704     C                   SETON                                        97
052200000704     C                   ENDIF
052300000704     C                   ENDSR
052400000704      ************************************************************
052500000704      * INIZIALIZZAZIONE VIDEATA DATI
052600000704      ************************************************************
052700000704     C     INZD1         BEGSR
052800000704     C*
052900000704     C                   CLEAR                   FMTD1
053000000704    >C                   CLEAR                   FMTD2
053100000704     C                   MOVE      ANPPAR        D1PAR
053200000704     C*-                                                   *
053300000704     c                   move      xscon         d1on1
053400000704     c                   move      xscon         d1on2
053500000704     c                   move      xscon         d1on3
053600000704     c                   move      xscon         d1on4
053700000704     c                   move      xscon         d1on5
053800000704     c                   move      xscon         d1on6
053900000704     c                   move      xscon         d1on7
054000000705     c                   move      xscon         d1on8
054100000705     c                   move      xscon         d1on9
054200090305c2125c                   move      xscon         d3on9
054300090305  "  c                   move      xscon         d3on10
054400090305  "  c                   move      xscon         d3on11
054500090305c2125c                   move      xscon         d3on12
054600110419c2184c                   move      xscon         d3on13
054700130906c2226c                   move      xscon         d3on14
054800130916c2226c                   move      xscon         d3on15
054900000704     c                   move      xscoff        d1of1
055000000704     c                   move      xscoff        d1of2
055100000704     c                   move      xscoff        d1of3
055200000704     c                   move      xscoff        d1of4
055300000704     c                   move      xscoff        d1of5
055400000704     c                   move      xscoff        d1of6
055500000704     c                   move      xscoff        d1of7
055600000705     c                   move      xscoff        d1of8
055700000705     c                   move      xscoff        d1of9
055800010330     c                   move      xscon         d2on1
055900010330     c                   move      xscoff        d2of1
056000100518c2156c                   move      xscon         d2on2
056100100518  "  c                   move      xscoff        d2off2
056200100518  "  c                   move      xscon         d2on3
056300100518c2156c                   move      xscoff        d2off3
056400130404C2221c                   move      xscon         d2on4
056500130412  "  c                   move      xscoff        d2off4
056600130412  "  c                   move      xscon         d2on5
056700130412C2221c                   move      xscoff        d2off5
056800130524C2224c                   move      xscon         d2on6
056900130524C2224c                   move      xscoff        d2off6
057000090305c2125c                   move      xscoff        d3off9
057100090305  "  c                   move      xscoff        d3off10
057200090305  "  c                   move      xscoff        d3off11
057300090305c2125c                   move      xscoff        d3off12
057400110419c2184c                   move      xscoff        d3off13
057500130906c2226c                   move      xscoff        d3off14
057600130916c2226c                   move      xscoff        d3off15
057700100518c2156c* flag fissi attività recupero credito
057800110502C2184x****               move      xscon         d1flagsca
057900110502  "  c* va sempre calcolato tranne quando gira in notte
058000110502C2184c                   move      xscoff        d1flagsca
058100110502c2156c                   move      'F'           d1flagann
058200100421  "  c                   move      xscoff        d1flagris
058300100421  "  c                   move      xscoff        d1flageffR
058400100518  "  c                   move      xscon         d1flagries
058500100518c2156c                   move      xscoff        d2Flagcau
058600110428c2184c                   move      xscoff        d1flagcre
058700110428c2184c                   move      xscoff        d1flagave
058800100518c2156c* flag fissi solleciti
058900100518  "  c                   move      xscoff        d2DaNSol
059000100518c2156c                   move      xscoff        d2CliSav
059100000704     C*
059200000704     C                   Z-ADD     1             $PIED1
059300000704     C*
059400000704     C* valorizzo la chiave univoca per reperire i dati
059500000704     C* se non immissione
059600000704   1>C     ANPOPZ        IFNE      '01'
059700000704     C*
059800000704     C* riempio la videata
059900000704     C                   EXSR      RIED1
060000000704     C* Aggiungere le routine di riempimento dei record succcessivi
060100000704    >C                   EXSR      RIED2
060200090305C2125C* Aggiungere le routine di riempimento dei record succcessivi
060300090305C2125C                   EXSR      RIED3
060400131127C2232C* Aggiungere le routine di riempimento dei record succcessivi
060500131127C2232C                   EXSR      RIED4
060600000704     C* richiamo routine dei ctrl per decodificarla
060700000704     C                   EXSR      CTRD1
060800000704     C* Aggiungere le routine di controllo dei record succcessivi
060900000704    >C                   EXSR      CTRD2
061000090305C2125C* Aggiungere le routine di controllo dei record succcessivi
061100090305C2125C                   EXSR      CTRD3
061200131127C2232C* Aggiungere le routine di controllo dei record succcessivi
061300131127C2232C                   EXSR      CTRD4
061400000704     C                   MOVE      *ALL'0'       IN5098
061500000704     C                   MOVEA     IN5098        *IN(50)
061600000704     C*
061700000704     C*se immissione
061800000704     C                   ELSE
061900000704     C*-                                                   *
062000000704     C                   MOVE      xscoff        D1GesUni                       *
062100130905C2226C                   MOVE      xscoff        D1GesUniR                      *
062200130915  "  C                   MOVE      xscoff        D3GesUnii                      *
062300130915C2226C                   MOVE      xscoff        D3StpAtt                       *
062400000704     C                   MOVE      xscoff        D1Scadenz                      *
062500000705     C                   MOVE      xscoff        D1AltriTi                      *
062600000705     C                   MOVE      xscoff        D1GesRis                       *
062700000705     C                   MOVE      xscoff        D1EffARis                      *
062800000705     C                   MOVE      xscoff        D1EffASca                      *
062900000705C1369C                   MOVE      XScOff        D1NoteCre                      *
063000000705C1369C                   MOVE      XScOff        D1PartAve                      *
063100000705C1369C                   MOVE      XScOff        D1RiePar                       *
063200000704   1EC                   ENDIF
063300000704     C*
063400000704     C                   ENDSR
063500000704     C/EJECT
063600000704     C************************************************************
063700000704     C* RIEMPIMENTO VIDEATA  D1
063800000704     C************************************************************
063900000704     C     RIED1         BEGSR
064000000704    >C*
064100000705     c                   If        GesUni057  = 'D'
064200000704     c                   move      xscon         D1GesUni
064300000704     c                   else
064400000704     c                   move      xscoff        D1GesUni
064500000704     c                   Endif
064600130905C2226C* Gestione unità recupero credito
064700130905  "  C                   If        GesUniR057 = 'D'
064800130905  "  C                   move      xscon         D1GesUniR
064900130905  "  C                   else
065000130905  "  C                   move      xscoff        D1GesUniR
065100130905  "  C                   Endif
065200130905C2226C*
065300000705     C                   Move      DomAna057     D1DomAna
065400000705     C                   Move      DomCom057     D1DomCom
065500000705     C                   If        Scadenz057 =  *on
065600000705     C                   Eval      D1Scadenz  =  XScOn
065700000704     C                   else
065800000705     C                   Eval      D1Scadenz  =  XScOff
065900000704     C                   Endif
066000130905C2226C                   If        FlagSca057 <> *blanks
066100130905  "  C                   If        FlagSca057 =  *on
066200130905  "  C                   Eval      D1FlagSca  =  XScOn
066300130905  "  C                   else
066400130905  "  C                   Eval      D1FlagSca  =  XScOff
066500130905  "  C                   Endif
066600130905C2226C                   Endif
066700000705     C                   Move      GesAge057     D1GesAge
066800000705     C                   Move      AnnPar057     D1AnnPar
066900000705     C                   If        AltriTi057 =  XScOn
067000000705     C                   Eval      D1AltriTi  =  XScOn
067100000704     C                   else
067200000705     C                   Eval      D1AltriTi  =  XScOff
067300000704     C                   Endif
067400000705     C                   If        GesRis057  =  XScOn
067500000705     C                   Eval      D1GesRis   =  XScOn
067600000704     C                   else
067700000705     C                   Eval      D1GesRis   =  XScOff
067800000704     C                   Endif
067900130905C2226C                   If        FlagRis057 <> *blanks
068000130905  "  C                   If        FlagRis057 =  XScOn
068100130905  "  C                   Eval      D1FlagRis  =  XScOn
068200130905  "  C                   else
068300130905  "  C                   Eval      D1FlagRis  =  XScOff
068400130905  "  C                   Endif
068500130905C2226C                   Endif
068600000705     C                   If        EffARis057 =  XScOn
068700000705     C                   Eval      D1EffARis  =  XScOn
068800000704     C                   else
068900000705     C                   Eval      D1EffARis  =  XScOff
069000000704     C                   Endif
069100130905C2226C                   If        FlagEffr57 <> *blanks
069200130905  "  C                   If        FlagEffr57 =  XScOn
069300130905  "  C                   Eval      D1FlagEffr =  XScOn
069400130905  "  C                   else
069500130905  "  C                   Eval      D1FlagEffr =  XScOff
069600130905  "  C                   Endif
069700130905C2226C                   Endif
069800000705     C                   If        EffASca057 =  XScOn
069900000705     C                   Eval      D1EffASca  =  XScOn
070000000704     C                   else
070100000705     C                   Eval      D1EffASca  =  XScOff
070200000704     C                   Endif
070300000705     C                   If        NoteCre057 =  XScOn
070400000705     C                   Eval      D1NoteCre  =  XScOn
070500000704     C                   else
070600000705     C                   Eval      D1NoteCre  =  XScOff
070700000704     C                   Endif
070800130905C2226C                   If        FlagCre057 <> *blanks
070900130905  "  C                   If        FlagCre057 =  XScOn
071000130905  "  C                   Eval      D1FlagCre  =  XScOn
071100130905  "  C                   else
071200130905  "  C                   Eval      D1FlagCre  =  XScOff
071300130905  "  C                   Endif
071400130905C2226C                   Endif
071500000705     C                   If        PartAve057 =  XScOn
071600000705     C                   Eval      D1PartAve  =  XScOn
071700000704     C                   else
071800000705     C                   Eval      D1PartAve  =  XScOff
071900000704     C                   Endif
072000130905C2226C                   If        FlagAve057 <> *blanks
072100130905  "  C                   If        FlagAve057 =  XScOn
072200130905  "  C                   Eval      D1FlagAve  =  XScOn
072300130905  "  C                   else
072400130905  "  C                   Eval      D1FlagAve  =  XScOff
072500130905  "  C                   Endif
072600130905C2226C                   Endif
072700000705     C                   If        RiePar057  =  XScOn
072800000705     C                   Eval      D1RiePar   =  XScOn
072900000704     C                   else
073000000705     C                   Eval      D1RiePar   =  XScOff
073100000704     C                   Endif
073200130905C2226C                   If        FlagRies57 <> *blanks
073300130905  "  C                   If        FlagRies57 =  XScOn
073400130905  "  C                   Eval      D1FlagRies =  XScOn
073500130905  "  C                   else
073600130905  "  C                   Eval      D1FlagRies =  XScOff
073700130905  "  C                   Endif
073800130905C2226C                   Endif
073900000704     C*
074000000704     C                   ENDSR
074100000704     C/EJECT
074200000704     C************************************************************
074300000704     C* GESTIONE F02 VIDEO D1
074400000704     C************************************************************
074500000704     C     F02D1         BEGSR
074600000704     C*
074700000704     C* per decodificare la videata richiamo la routine dei ctrl
074800000704     C* e poi disattivo gli eventuali messaggi di errore
074900000704     C                   EXSR      CTRD1
075000000704     C*
075100000704     C                   MOVE      *ALL'0'       IN5098
075200000704     C                   MOVEA     IN5098        *IN(50)
075300000704     C*
075400000704     C                   ENDSR
075500000704     C/EJECT
075600000704     C************************************************************
075700000704     C* GESTIONE F03 VIDEO D1
075800000704     C************************************************************
075900000704     C     F03D1         BEGSR
076000000704     C*
076100000704     C                   MOVE      *ON           $FINE
076200000704     C* fine programma
076300000704     C                   EXSR      ENDPGM
076400000704     C*
076500000704     C                   ENDSR
076600000704     C/EJECT
076700000704     C************************************************************
076800000704     C* GESTIONE F05 VIDEO D1
076900000704     C************************************************************
077000000704     C     F05D1         BEGSR
077100000704     C*
077200000704     C                   MOVE      *ON           $INZD1
077300000704     C*
077400000704     C                   ENDSR
077500000704     C/EJECT
077600000704     C************************************************************
077700000704     C* GESTIONE F08 VIDEO D1
077800000704     C************************************************************
077900000704     C     F08D1         BEGSR
078000000704     C*
078100000704     C                   MOVE      *ON           $FINE
078200000704     C*
078300000704     C                   ENDSR
078400000704     C/EJECT
078500000704     C************************************************************
078600000704     C* GESTIONE F12 VIDEO D1
078700000704     C************************************************************
078800000704     C     F12D1         BEGSR
078900000704     C*
079000000704     C                   MOVE      *ON           $FINE
079100000704     C*
079200000704     C                   ENDSR
079300000704     C/EJECT
079400000704     C************************************************************
079500000704     C* GESTIONE F24 VIDEO D1
079600000704     C************************************************************
079700000704     C     F24D1         BEGSR
079800000704     C*
079900000704     C                   MOVEA     $FX1          $FX
080000000704     C                   MOVEA     $FC1          $FC
080100000704     C                   MOVEA     $FR1          $FR
080200000704     C                   Z-ADD     $ULKD1        $ULK
080300000704     C                   EXSR      RIEKEY
080400000704     C                   Z-ADD     $ULK          $ULKD1            3 0
080500000704     C                   MOVEA     $FC           $FC1
080600000704     C                   MOVEL     $KEY1         Z1KE1
080700060224A1148C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
080800000704     C                   MOVEL     $KEY2         Z1KE2
080900000704     C                   WRITE     FMTZ1
081000000704     C*
081100000704     C                   ENDSR
081200000704     C/EJECT
081300000704     C************************************************************
081400000704     C* CONTROLLO VIDEATA
081500000704     C************************************************************
081600000704     C     CTRD1         BEGSR
081700000704     C*
081800000704     C                   SETOFF                                       99
081900000704     C* non imposto pos. del cursore
082000000704     C                   Z-ADD     0             H1RIGA
082100000704     C                   Z-ADD     0             H1COLO
082200000704     C*                                                    *
082300000704     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
082400000704     C*
082500000704     C     ANPOPZ        IFNE      '04'
082600000704     C*
082700000705      * Gestione  unità
082800000704     C                   If        D1GesUni<> XscOn and D1GesUni<> XscOff
082900000705     C                   SetOn                                        5099
083000000704     C                   Else
083100000705     C                   If        D1GesUni=XscOn and Fgo(1)=*Off
083200000705     C                   SetOn                                        5199
083300000704     C                   EndIf
083400000704     C                   EndIf
083500130905C2226 * Gestione  unità  recupero credito
083600130905  "  C                   If        D1GesUniR<>XscOn and D1GesUniR<>XscOff
083700130905  "  C                   SetOn                                        6799
083800130905  "  C                   Else
083900130905  "  C                   If        D1GesUniR=XscOn and Fgo(1)=*Off
084000130905  "  C                   SetOn                                        6899
084100130905  "  C                   EndIf
084200130905C2226C                   EndIf
084300000704     C*
084400000705     C* Domini
084500000705     C     D1DomAna      Ifeq      *blank
084600000705     C                   seton                                        5299
084700000705     C                   else
084800000705     C     D1DomAna      Chain     AnDom01l                           21
084900000704     C     *IN21         IfEq      *Off
085000000705     C                   Movel     DomDes        D1DomAnaD
085100000704     C                   Else
085200000705     C                   SetOn                                        5399
085300000704     C                   EndIf
085400000705     C                   EndIf
085500000704     C*
085600000705     C     D1Domcom      Ifeq      *blank
085700000705     C                   seton                                        5499
085800000705     C                   else
085900000705     C     D1DomCom      Chain     AnDom01l                           21
086000000704     C     *IN21         IfEq      *off
086100000705     C                   Movel     DomDes        D1DomComd
086200000704     C                   Else
086300000705     C                   SetOn                                        5599
086400000704     C                   EndIf
086500000705     C                   EndIf
086600000705     C*
086700000705      * Utilizzo scadenzario in linea
086800000705     C     D1Scadenz     IfNe      XScOn
086900000705     C     D1Scadenz     AndNe     XScOff
087000090305C2125X***                SetOn                                        5690
087100090305C2125C                   SetOn                                        5699
087200000705     C                   EndIf
087300130905C2226 * Utilizzo scadenzario in linea recupero credito
087400130905  "  C     D1FlagSca     IfNe      XScOn
087500130905  "  C     D1FlagSca     AndNe     XScOff
087600130905  "  C                   SetOn                                        6999
087700130905C2226C                   EndIf
087800000704     C*
087900000705     C* Agente
088000000705     C                   MOVEL     *BLANK        D1GESAGED
088100000704     C                   RESET                   X07DS
088200000704     C                   MOVE      '1'           X07RIC
088300000704     C                   MOVEL     'ANDIZ000'    X07TRC
088400000704     C                   MOVEL     'DSPTPAGE'    X07CAM
088500000705     C                   MOVE      *OFF          X07ALL
088600000705     C                   MOVEL     D1GESAGE      X07VAL
088700000704     C                   CALLB     'X07VALR'
088800000704     C                   PARM                    X07DS
088900000705     C     X07ERR        IFEQ      '0'
089000000705     C                   MOVEL     X07DES        D1GESAGED
089100000705     C                   ELSE
089200000705     C                   SETON                                        5799
089300000705     C                   ENDIF
089400000704     C*
089500000705     C* Annullamento partite
089600000705     C                   MOVEL     *BLANK        D1ANNPARD
089700000704     C                   RESET                   X07DS
089800000704     C                   MOVE      '1'           X07RIC
089900000704     C                   MOVEL     'ANDIZ000'    X07TRC
090000000704     C                   MOVEL     'DSPTPANN'    X07CAM
090100000705     C                   MOVE      *OFF          X07ALL
090200000705     C                   MOVEL     D1ANNPAR      X07VAL
090300000704     C                   CALLB     'X07VALR'
090400000704     C                   PARM                    X07DS
090500000705     C     X07ERR        IFEQ      '0'
090600000705     C                   MOVEL     X07DES        D1ANNPARD
090700000705     C                   ELSE
090800000705     C                   SETON                                        5899
090900000705     C                   ENDIF
091000110412c2184C* Annullamento partite recupero credito
091100110412  "  C                   RESET                   X07DS
091200110412  "  C                   MOVE      '1'           X07RIC
091300110412  "  C                   MOVEL     'ANDIZ000'    X07TRC
091400110412  "  C                   MOVEL     'DSPTPANN'    X07CAM
091500110412  "  C                   MOVE      *OFF          X07ALL
091600110412  "  C                   MOVEL     D1FLAGANN     X07VAL
091700110412  "  C                   CALLB     'X07VALR'
091800110412  "  C                   PARM                    X07DS
091900110412  "  C     X07ERR        IFEQ      '0'
092000110412  "  c     d1annparD     cat       '|':0         d1annpard
092100110412  "  C     D1ANNPARD     CAT       X07DES:0      D1ANNPARD
092200110412c2184C                   ENDIF
092300000705     C*
092400000705      * Altri titoli a scadere
092500000705     C     D1AltriTi     IfNe      XScOn
092600000705     C     D1AltriTi     AndNe     XScOff
092700000705     C                   SetOn                                        5999
092800000705     C                   EndIf
092900000705     C*
093000000705      * Gestione del rischio
093100000705     C     D1GesRis      IfNe      XScOn
093200000705     C     D1GesRis      AndNe     XScOff
093300000705     C                   SetOn                                        6099
093400000705     C                   EndIf
093500130905C2226 * Gestione del rischio Recupero Credito
093600130905  "  C     D1FlagRis     IfNe      XScOn
093700130905  "  C     D1FlagRis     AndNe     XScOff
093800130905  "  C                   SetOn                                        7199
093900130905C2226C                   EndIf
094000000705     C*
094100000705      * Effetti a rischio
094200000705     C     D1EffARis     IfNe      XScOn
094300000705     C     D1EffARis     AndNe     XScOff
094400000705     C                   SetOn                                        6199
094500000705     C                   EndIf
094600130905C2226 * Effetti a rischio Recupero Credito
094700130905  "  C     D1FlagEffr    IfNe      XScOn
094800130905  "  C     D1FlagEffr    AndNe     XScOff
094900130905  "  C                   SetOn                                        7399
095000130905C2226C                   EndIf
095100040719D1639x* NO!! I DUE FLAG SONO INDIPENDENTI L'UNO DALL'ALTRO
095200040719  "  x* controllo congruenza gestione del rischio e estraz.effetti
095300040719  "  x* a rischio
095400040719  "  x***                If        D1GesRis = xscoff
095500040719  "  x***                          And D1EffARis = xscon
095600040719  "  x***                SetOn                                        6299
095700040719D1639x***                EndIf
095800000705     C*
095900000705      * Effetti a scadere
096000000705     C     D1EffASca     IfNe      XScOn
096100000705     C     D1EffASca     AndNe     XScOff
096200000705     C                   SetOn                                        6399
096300000705     C                   EndIf
096400000705     C*
096500000705      * Note credito
096600000705     C     D1NoteCre     IfNe      XScOn
096700000705     C     D1NoteCre     AndNe     XScOff
096800000705     C                   SetOn                                        6499
096900000705     C                   EndIf
097000130905C2226 * Note credito  Recupero Credito
097100130905  "  C     D1FlagCre     IfNe      XScOn
097200130905  "  C     D1FlagCre     AndNe     XScOff
097300130905  "  C                   SetOn                                        7599
097400130905C2226C                   EndIf
097500000705     C*
097600000705      * Partite con saldo avere
097700000705     C     D1PartAve     IfNe      XScOn
097800000705     C     D1PartAve     AndNe     XScOff
097900000705     C                   SetOn                                        6599
098000000705     C                   EndIf
098100130905C2226 * Partite con saldo avere Recupero Credito
098200130905  "  C     D1FlagAve     IfNe      XScOn
098300130905  "  C     D1FlagAve     AndNe     XScOff
098400130905  "  C                   SetOn                                        7699
098500130905C2226C                   EndIf
098600000705     C*
098700000705      * Riestrazione partite non modificate
098800000705     C     D1RiePar      IfNe      XScOn
098900000705     C     D1RiePar      AndNe     XScOff
099000000705     C                   SetOn                                        6699
099100000705     C                   EndIf
099200130905C2226 * Riestrazione partite non modificate
099300130905  "  C     D1FlagRies    IfNe      XScOn
099400130905  "  C     D1FlagRies    AndNe     XScOff
099500130905  "  C                   SetOn                                        7799
099600130905C2226C                   EndIf
099700000704     C*
099800000705     C                   ENDIF
099900000704     C*
100000000704     C*
100100000704     C                   ENDSR
100200000704     C/EJECT
100300000704     C************************************************************
100400000704     C* GESTIONE VIDEO RECORD  D2
100500000704     C************************************************************
100600000704    >C     GESD2         BEGSR
100700000704     C* emissione piede videata se proveniente da altra
100800000704     C* salvataggio valore formato di provenienza
100900000704   B1C     $LASTV        IFNE      'D2'
101000000704     C                   WRITE     FMTT1
101100000704     C                   EXSR      GESPIE
101200000704     C                   MOVE      $LASTV        $LASTG
101300000704     C                   MOVE      'D2'          $LASTV
101400000704   E1C                   ENDIF
101500000704     C*
101600000704     C* se esistono errori sulla videata
101700000704     C* emetto la write del formato a indicatori spenti per vedere
101800000704     C* le eventuali decodifiche
101900000704   B1C     *IN99         IFEQ      *ON
102000000704     C     $GEST         ANDEQ     'D2'
102100000704     C                   MOVEA     *IN           WIN
102200000704     C                   MOVE      *ALL'0'       IN5098           49
102300000704     C                   MOVEA     IN5098        *IN(50)
102400000704     C                   WRITE     FMTD2
102500000704     C                   MOVEA     WIN           *IN
102600000704   E1C                   ENDIF
102700000704     C*
102800000704   B1C                   SELECT
102900000704     C* Annullamento
103000000704    >C     ANPOPZ        WHENEQ    '04'
103100000704     C* Visualizzazione
103200000704    >C     ANPOPZ        OREQ      '05'
103300000704     C                   WRITE     FMTD2
103400000704     C                   EXFMT     PROTECT
103500000704     C* Immissione/Modifica/Copia
103600000704   X1C                   OTHER
103700000704     C                   EXFMT     FMTD2
103800000704   E1C                   ENDSL
103900000704     C*
104000000704     C* azzero la posizione del cursore
104100000704     C                   Z-ADD     0             H1RIGA
104200000704     C                   Z-ADD     0             H1COLO
104300000704     C*
104400000704     C* controllo abilitazione tasti funzione
104500000704     C                   EXSR      CTRKD1
104600000704     C*
104700000704   B1C     *IN99         IFEQ      *OFF
104800000704     C*
104900000704   B2C                   SELECT
105000090305C2125C* ROLLUP
105100090305  " >C     $TASTO        WHENEQ    ROLLUP
105200090305C2125C                   MOVE      'D3'          $GEST
105300000704     C* ROLDOWN
105400000704     C     $TASTO        WHENEQ    ROLDWN
105500000704     C                   MOVE      'D1'          $GEST
105600000704     C* F3=Fine
105700000704     C     $TASTO        WHENEQ    F03
105800000704     C                   EXSR      F03D1
105900000704     C* F4=Ricerca
106000000704     C     $TASTO        WHENEQ    F04
106100000704     C                   EXSR      SEARCH
106200000704     C* F5=Refresh
106300000704     C     $TASTO        WHENEQ    F05
106400000704     C                   EXSR      F05D2
106500000704     C* F8=Successivo
106600000704     C     $TASTO        WHENEQ    F08
106700000704     C                   EXSR      F08D1
106800000704     C* F12=Ritorno
106900000704     C     $TASTO        WHENEQ    F12
107000000704     C                   EXSR      F12D1
107100000704     C* F24=Altri tasti
107200000704     C     $TASTO        WHENEQ    F24
107300000704     C                   EXSR      F24D2
107400000704     C* Personalizzata
107500000704     C***  $PGF1(PosFun) Whenne    *BLANK
107600000704     C***                EXSR      FperD1
107700000704     C*
107800000704   X2C                   OTHER
107900000704     C                   EXSR      CTRD2
108000000704   B3C     *IN99         IFEQ      *OFF
108100000704     C                   EXSR      CTRD1
108200090305C2125C  N99              EXSR      CTRD3
108300131127C2232C  N99              EXSR      CTRD4
108400000704     C     *IN99         IFEQ      *ON
108500000704     C* Spengo gli indicatori di errore per il controllo su altri schermi
108600000704     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
108700000704     C* videate
108800000704     C                   MOVE      *ALL'0'       IN5098           49
108900000704     C                   MOVEA     IN5098        *IN(50)
109000000704     C                   SETON                                        9996                       96
109100000704     C                   ENDIF
109200000704     C                   ENDIF
109300000704     C*
109400000704     C*
109500000704   B3C     $TASTO        IFEQ      F06
109600000704     C     *IN99         ANDEQ     *OFF
109700000704     C* eseguo operazioni del dopo-aggiornamento
109800000704   B4C     *IN99         IFEQ      *OFF
109900000704     C                   EXSR      GESAGG
110000000704   E4C                   ENDIF
110100000704   E3C                   ENDIF
110200000704     C*
110300000704   E2C                   ENDSL
110400000704     C*
110500000704   E1C                   ENDIF
110600000704     C*
110700000704    >C                   ENDSR
110800000704     C/EJECT
110900000704     C************************************************************
111000000704     C* RIEMPIMENTO VIDEATA D2
111100000704     C************************************************************
111200000704    >C     RIED2         BEGSR
111300000704     C*
111400001003     C                   Move      TipGra057     D2TipGra
111500000705     C                   Move      StpLet057     D2StpLet
111600000705     C                   Z-add     ImpMin057     D2ImpMin
111700010330     c                   if        StpEstr057= *on
111800010330     C                   move      xscon         D2CauAnn
111900010330     c                   else
112000010330     C                   move      xscoff        D2CauAnn
112100010330     c                   endif
112200030123     c* ho dovuto fare così il test in quanto c'era della robaccia e
112300030123     c* non erano tutte e tre le posizioni a blank, quindi ne ho
112400030123     c* dovuta testare una alla volta
112500030123c1619c                   if        %Subst(anp057DS:53:1) = *blanks or
112600030123  "  c                             %Subst(anp057DS:54:1) = *blanks or
112700030123  "  c                             %Subst(anp057DS:55:1) = *blanks
112800030123  "  c                   eval      d2divisor =*zeros
112900030123  "  c                   else
113000030123  "  C                   Move      Divisor057    D2Divisor
113100030123c1619c                   endif
113200030311c1632c* idem
113300030311  "  c                   if        %subst(anp057ds:56:1) = *blanks or
113400030311  "  c                             %subst(anp057ds:57:1) = *blanks or
113500030311  "  c                             %subst(anp057ds:58:1) = *blanks or
113600030311  "  c                             %subst(anp057ds:59:1) = *blanks or
113700030311  "  c                             %subst(anp057ds:60:1) = *blanks or
113800030311  "  c                             %subst(anp057ds:61:1) = *blanks or
113900030311  "  c                             %subst(anp057ds:62:1) = *blanks or
114000030311  "  c                             %subst(anp057ds:63:1) = *blanks or
114100030311  "  c                             %subst(anp057ds:64:1) = *blanks or
114200030311  "  c                             %subst(anp057ds:65:1)= *blanks or
114300030311  "  c                             %subst(anp057ds:66:1)= *blanks or
114400030311  "  c                             %subst(anp057ds:67:1)= *blanks or
114500030311  "  c                             %subst(anp057ds:68:1)= *blanks or
114600030311  "  c                             %subst(anp057ds:69:1)= *blanks or
114700030311  "  c                             %subst(anp057ds:70:1)= *blanks
114800030311  "  c                   eval      %subst(anp057ds:56:15)= *zeros
114900030311  "  c                   else
115000030311  "  c                   z-add     speinv057     d2speinv
115100030311c1632c                   endif
115200000704     C*
115300051103C1879C                   move      posattr057    d2posattr
115400100518c2156c                   if        DaNSol057 = *on
115500100518  "  C                   move      xscon         D2DaNSolA
115600100518  "  c                   else
115700100518  "  C                   move      xscoff        D2DaNSolA
115800100518  "  c                   endif
115900100518  "  c                   if        CliSAv057 = *on
116000100518  "  C                   move      xscon         D2CliSAvA
116100100518  "  c                   else
116200100518  "  C                   move      xscoff        D2CliSavA
116300100518c2156c                   endif
116400130412C2221C* Attività diversa da sollecito ma da sollecitare
116500130404  "  c                   if        ADaSOl057 = *on
116600130404  "  C                   move      xscon         D2AdaSol
116700130404  "  c                   else
116800130404  "  C                   move      xscoff        D2AdaSol
116900130412  "  c                   endif
117000130412  "  C* Scaduto non compreso nel rec.credito o con attività chiusa
117100130412  "  C* ma da sollecitare.
117200130412  "  c                   if        SolNoRe057= *on
117300130412  "  C                   move      xscon         D2SolNoRe
117400130412  "  c                   else
117500130412  "  C                   move      xscoff        D2SolNoRe
117600130412C2221c                   endif
117700130524C2224C* Raggruppamento per effetto in stampa sollecito
117800130524  "  C                   if        RagrEff057= *on
117900130524  "  C                   move      xscon         D2RagrEff
118000130524  "  C                   else
118100130524  "  C                   move      xscoff        D2RagrEff
118200130524C2224c                   endif
118300051103     C*
118400000704    >C                   ENDSR
118500000704     C/EJECT
118600000704     C************************************************************
118700000704     C* GESTIONE F05 VIDEO D2
118800000704     C* il tasto F5 su qualsiasi videata provoca il refresh di tutte
118900000704     C* e il riposizionamento alla prima
119000000704     C************************************************************
119100000704    >C     F05D2         BEGSR
119200000704     C*
119300000704     C                   MOVE      *ON           $INZD1
119400000704     C                   MOVE      'D1'          $GEST
119500000704     C*
119600000704    >C                   ENDSR
119700000704     C************************************************************
119800000704     C* GESTIONE F24 VIDEO D2
119900000704     C************************************************************
120000000704    >C     F24D2         BEGSR
120100000704     C*
120200000704     C                   MOVEA     $FX1          $FX
120300000704     C                   MOVEA     $FC1          $FC
120400000704     C                   MOVEA     $FR1          $FR
120500000704     C                   Z-ADD     $ULKD1        $ULK
120600000704     C***                Z-ADD     $D1L01        $L01
120700000704     C***                Z-ADD     $D1L02        $L02
120800000704     C                   EXSR      RIEKEY
120900000704     C                   Z-ADD     $ULK          $ULKD1            3 0
121000000704     C                   MOVEA     $FC           $FC1
121100000704     C                   MOVEL     $KEY1         Z1KE1
121200060224A1148C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
121300000704     C                   MOVEL     $KEY2         Z1KE2
121400000704     C                   WRITE     FMTZ1
121500000704     C*
121600000704    >C                   ENDSR
121700000704     C************************************************************
121800000704     C* CONTROLLO VIDEATA  D2
121900000704     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
122000000704     C* 95 96 97 98
122100000704     C************************************************************
122200000704    >C     CTRD2         BEGSR
122300000704     C*
122400000704     C                   SETOFF                                       99
122500000704     C*
122600000704     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
122700000704     C*
122800000704    >C     ANPOPZ        IFNE      '04'
122900001003     C*
123000001003     C* Tipo gravità da utilizzare
123100001003     C                   MOVEL     *BLANK        D2TIPGRAD
123200001003     C                   RESET                   X07DS
123300001003     C                   MOVE      '1'           X07RIC
123400001003     C                   MOVEL     'ANDIZ000'    X07TRC
123500001003     C                   MOVEL     'DSPTIPGRA'   X07CAM
123600001003     C                   MOVE      *ON           X07ALL
123700001003     C                   MOVEL     D2TIPGRA      X07VAL
123800001003     C                   CALLB     'X07VALR'
123900001003     C                   PARM                    X07DS
124000001003     C     X07ERR        IFEQ      '0'
124100001003     C                   MOVEL     X07DES        D2TIPGRAD
124200001003     C                   ELSE
124300001003     C                   SETON                                        6999
124400001003     C                   ENDIF
124500110502c2184C* gravità '2' ammessa solo se estrazione automatica solleciti
124600110502  "  c                   if        D2TipGra ='2'     and
124700110502  "  c                             D3EsAutom <> xscon
124800110502  "  C                   SETON                                        7899
124900110502c2184C                   ENDIF
125000010330     C* stampa causa partite annullate
125100010330     c                   if        D2CauAnn <> xscon and
125200010330     c                             D2CauAnn <> xscoff
125300010330     C                   SETON                                        7099
125400010330     C                   ENDIF
125500000705     C*
125600000705     C* Modalità di stampa
125700000705     C                   MOVEL     *BLANK        D2STPLETD
125800000705     C                   RESET                   X07DS
125900000705     C                   MOVE      '1'           X07RIC
126000000705     C                   MOVEL     'ANDIZ000'    X07TRC
126100000705     C                   MOVEL     'DSPTPLETTS'  X07CAM
126200000705     C                   MOVE      *OFF          X07ALL
126300000705     C                   MOVEL     D2STPLET      X07VAL
126400000705     C                   CALLB     'X07VALR'
126500000705     C                   PARM                    X07DS
126600000705     C     X07ERR        IFEQ      '0'
126700000705     C                   MOVEL     X07DES        D2STPLETD
126800000705     C                   ELSE
126900000705     C                   SETON                                        6799
127000000705     C                   ENDIF
127100000705     C*
127200000705     C* Importo minimo lettera mc
127300000705     C*  controllo decimali
127400000710     C                   If        D2ImpMin <> 0
127500000705     C                   Z-Add     D2ImpMin      W1903
127600000710     C                   If        X0201NrDec(W1903 :XscNrDecMc)= *on
127700000710     C                   Seton                                        6899
127800000710     C                   EndIf
127900000710     C                   EndIf
128000030123     C*
128100030123C1619C* Gionri calcolo interessi
128200030123 "   C                   MOVEL     *BLANK        D2DIVISORD
128300030123 "   C                   RESET                   X07DS
128400030123 "   C                   MOVE      '1'           X07RIC
128500030123 "   C                   MOVEL     'ANDIZ000'    X07TRC
128600030123 "   C                   MOVEL     'DSPGGINT'    X07CAM
128700030123 "   C                   MOVE      *OFF          X07ALL
128800030123 "   C                   MOVEL     D2DIVISOR     X07VAL
128900030123 "   C                   CALLB     'X07VALR'
129000030123 "   C                   PARM                    X07DS
129100030123 "   C     X07ERR        IFEQ      '0'
129200030123 "   C                   MOVEL     X07DES        D2DIVISORD
129300030123 "   C                   ELSE
129400030123 "   C                   SETON                                        7199
129500040302C1619C                   ENDIF
129600030311c1632C* Spese per invio lettera mc
129700030311  "  C*  controllo decimali
129800030311  "  C                   If        D2SpeInv <> 0
129900030311  "  C                   Z-Add     D2SpeInv      W1903
130000030311  "  C                   If        X0201NrDec(W1903 :XscNrDecMc)= *on
130100030311  "  C                   Seton                                        7299
130200030311  "  C                   EndIf
130300030311c1632C                   EndIf
130400051103     c*
130500051103c1879c* pos.attributo contabile
130600051103  "  c                   clear                   d2posattrD
130700051103  "  c                   if        D2PosAttr <> *blanks
130800051103  "  c                   reset                   XatBds
130900051103  "  C                   EVAL      XtbRic = '1'
131000051103  "  C                   EVAL      XtbAzi = XscSoc
131100051103  "  C                   EVAL      XtbKey = *zero
131200051103  "  C                   EVAL      XtbCod = '0A2'
131300051103  "  C                   MOVE      D2PosAttr     XtbKey
131400051103  "  C                   CALLB     'XATB'
131500051103  "  C                   PARM                    XAtbDS
131600051103  "  C                   IF        XtbErr <> '0'
131700051103  "  C                   SETON                                        7399
131800051103  "  C                   ELSE
131900051103  "  C                   EVAL      ANG0A2DS = XtbUni
132000051103  "  C                   EVAL      D2PosAttrD= DeB0A2
132100051103  "  C                   ENDIF
132200051103c1879c                   endif
132300100518c2156C* cliente/partita/rata/condpag/ da non sollecitare
132400100518  "  c                   if        D2DaNSolA<> xscon and
132500100518  "  c                             D2DaNSolA<> xscoff
132600100518  "  C                   SETON                                        7599
132700100518  "  C                   ENDIF
132800100518  "  C* cliente con saldo avere
132900100518  "  c                   if        D2CliSavA<> xscon and
133000100518  "  c                             D2CliSavA<> xscoff
133100100518  "  C                   SETON                                        7699
133200100518c2156C                   ENDIF
133300130404C2221C* Attività diversa da sollecito ma da sollecitare
133400130404  "  c                   if        D2AdaSol <> xscon and
133500130404  "  c                             D2AdaSol <> xscoff
133600130404  "  C                   SETON                                        7999
133700130412  "  C                   ENDIF
133800130412  "  C* Scaduto non presente in rec. cred o con attività chiusa
133900130412  "  C* ma da sollecitare
134000130412  "  c                   if        D2SolNoRe <> xscon and
134100130412  "  c                             D2SolNoRe <> xscoff
134200130412  "  C                   SETON                                        8099
134300130412C2221C                   ENDIF
134400130524C2224C* Raggruppamento per effetto in stampa sollecito
134500130524  "  c                   if        D2RagrEff <> xscon and
134600130524  "  c                             D2RagrEff <> xscoff
134700130524  "  C                   SETON                                        8199
134800130524C2224C                   ENDIF
134900000704     C*
135000000704     C                   ENDIF
135100000704     C*
135200000704     C                   ENDSR
135300000704     C/EJECT
135400090305C2125C************************************************************
135500090305     C* GESTIONE VIDEO RECORD  D3
135600090305     C************************************************************
135700090305C2125C     GESD3         BEGSR
135800090305     C* emissione piede videata se proveniente da altra
135900090305     C* salvataggio valore formato di provenienza
136000090305   B1C     $LASTV        IFNE      'D3'
136100090305     C                   WRITE     FMTT1
136200090305     C                   EXSR      GESPIE
136300090305     C                   MOVE      $LASTV        $LASTG
136400090305     C                   MOVE      'D3'          $LASTV
136500090305   E1C                   ENDIF
136600090305     C*
136700090305     C* se esistono errori sulla videata
136800090305     C* emetto la write del formato a indicatori spenti per vedere
136900090305     C* le eventuali decodifiche
137000090305   B1C     *IN99         IFEQ      *ON
137100090305     C     $GEST         ANDEQ     'D3'
137200090305     C                   MOVEA     *IN           WIN
137300090305     C                   MOVE      *ALL'0'       IN5098           49
137400090305     C                   MOVEA     IN5098        *IN(50)
137500090305     C                   WRITE     FMTD3
137600090305     C                   MOVEA     WIN           *IN
137700090305   E1C                   ENDIF
137800090305     C*
137900090305   B1C                   SELECT
138000090305     C* Annullamento
138100090305    >C     ANPOPZ        WHENEQ    '04'
138200090305     C* Visualizzazione
138300090305    >C     ANPOPZ        OREQ      '05'
138400090305     C                   WRITE     FMTD3
138500090305     C                   EXFMT     PROTECT
138600090305     C* Immissione/Modifica/Copia
138700090305   X1C                   OTHER
138800090305     C                   EXFMT     FMTD3
138900090305   E1C                   ENDSL
139000090305     C*
139100090305     C* azzero la posizione del cursore
139200090305     C                   Z-ADD     0             H1RIGA
139300090305     C                   Z-ADD     0             H1COLO
139400090305     C*
139500090305     C* controllo abilitazione tasti funzione
139600090305     C                   EXSR      CTRKD1
139700090305     C*
139800090305   B1C     *IN99         IFEQ      *OFF
139900090305     C*
140000090305   B2C                   SELECT
140100090305     C* ROLDOWN
140200090305     C     $TASTO        WHENEQ    ROLDWN
140300090305     C                   MOVE      'D2'          $GEST
140400131127C2232C* ROLLUP
140500131127  "  C     $TASTO        WHENEQ    ROLLUP
140600131127C2232C                   MOVE      'D4'          $GEST
140700090305     C* F3=Fine
140800090305     C     $TASTO        WHENEQ    F03
140900090305     C                   EXSR      F03D1
141000090305     C* F4=Ricerca
141100090305     C     $TASTO        WHENEQ    F04
141200090305     C                   EXSR      SEARCH
141300090305     C* F5=Refresh
141400090305     C     $TASTO        WHENEQ    F05
141500090305     C                   EXSR      F05D3
141600090305     C* F8=Successivo
141700090305     C     $TASTO        WHENEQ    F08
141800090305     C                   EXSR      F08D1
141900090305     C* F12=Ritorno
142000090305     C     $TASTO        WHENEQ    F12
142100090305     C                   EXSR      F12D1
142200090305     C* F24=Altri tasti
142300090305     C     $TASTO        WHENEQ    F24
142400090305     C                   EXSR      F24D3
142500090305     C* Personalizzata
142600090305     C***  $PGF1(PosFun) Whenne    *BLANK
142700090305     C***                EXSR      FperD1
142800090305     C*
142900090305   X2C                   OTHER
143000090305     C                   EXSR      CTRD3
143100090305   B3C     *IN99         IFEQ      *OFF
143200090305     C                   EXSR      CTRD1
143300090305     C  N99              EXSR      CTRD2
143400090305     C     *IN99         IFEQ      *ON
143500090305     C* Spengo gli indicatori di errore per il controllo su altri schermi
143600090305     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
143700090305     C* videate
143800090305     C                   MOVE      *ALL'0'       IN5098           49
143900090305     C                   MOVEA     IN5098        *IN(50)
144000090305     C                   SETON                                        9996                       96
144100090305     C                   ENDIF
144200090305     C                   ENDIF
144300090305     C*
144400090305     C*
144500090305   B3C     $TASTO        IFEQ      F06
144600090305     C     *IN99         ANDEQ     *OFF
144700090305     C* eseguo operazioni del dopo-aggiornamento
144800090305   B4C     *IN99         IFEQ      *OFF
144900090305     C                   EXSR      GESAGG
145000090305   E4C                   ENDIF
145100090305   E3C                   ENDIF
145200090305     C*
145300090305   E2C                   ENDSL
145400090305     C*
145500090305   E1C                   ENDIF
145600090305     C*
145700090305C2125C                   ENDSR
145800090305C2125C************************************************************
145900090305     C* RIEMPIMENTO VIDEATA D3
146000090305C2125C************************************************************
146100090305    >C     RIED3         BEGSR
146200090305     C*
146300090305     C                   If        AvvSca057 = *On
146400090305     C                   Move      XscOn         D3AvvSCa
146500090305     C                   Else
146600090305     C                   Move      XscOff        D3AvvSCa
146700090305     C                   EndIf
146800090305     c                   move      RegCre057     D3RegCRe
146900090305     C*
147000090305     C                   If        Sollec057 = *On
147100090305     C                   Move      XscOn         D3Sollec
147200090305     C                   Else
147300090305     C                   Move      XscOff        D3Sollec
147400090305     C                   EndIf
147500090305     C*
147600090305     C                   If        Impegn057 = *On
147700090305     C                   Move      XscOn         D3Impegni
147800090305     C                   Else
147900090305     C                   Move      XscOff        D3Impegni
148000090305     C                   EndIf
148100090305     c                   move      ImpCre057     D3ImpCre
148200090305     c                   if        %Subst(anp057ds:220:2) = *blanks
148300090305     c                   move      *zeros        D3GGTol
148400090305     c                   else
148500090305     c                   z-add     GGTol057      D3GGTol
148600090305     c                   endif
148700090305     c                   if        %Subst(anp057ds:222:2) = *blanks
148800090305     c                   move      *zeros        D3GGCad
148900090305     c                   else
149000090305     c                   z-add     GGCad057      D3GGCad
149100090305     c                   endif
149200090305     c*
149300090305     c                   movel     IndIFS057     D3IndIfs
149400090305     C*
149500090305     C                   If        AggBkl057 = *On
149600090305     C                   Move      XscOn         D3AggBkl
149700090305     C                   Else
149800090305     C                   Move      XscOff        D3AggBkl
149900090305     C                   EndIf
150000090305     c                   move      BklSca057     D3BklSca
150100110419     c*
150200110419c2184C                   If        Esautom057 = *On
150300110419  "  C                   Move      XscOn         D3EsAutom
150400110419  "  C                   Else
150500110419  "  C                   Move      XscOff        D3EsAutom
150600110419c2184C                   EndIf
150700130906C2226C* Gestione unità impostazioni recupero credito
150800130906  "  C                   If        GesUniI057 = 'D'
150900130906  "  C                   move      xscon         D3GesUniI
151000130906  "  C                   else
151100130906  "  C                   move      xscoff        D3GesUniI
151200130906  "  C                   Endif
151300130915  "  C*
151400130915  "  C* Stampa lista attività recupero credito
151500130915  "  C                   If        StpAtt057 = *on
151600130915  "  C                   move      xscon         D3StpAtt
151700130915  "  C                   else
151800130915  "  C                   move      xscoff        D3StpAtt
151900130915  "  C                   Endif
152000130915C2226C*
152100090305     C*
152200090305c2125C                   ENDSR
152300090305     C/EJECT
152400090305     C************************************************************
152500090305     C* GESTIONE F05 VIDEO D2
152600090305     C* il tasto F5 su qualsiasi videata provoca il refresh di tutte
152700090305     C* e il riposizionamento alla prima
152800090305     C************************************************************
152900090305c2125C     F05D3         BEGSR
153000090305     C*
153100090305     C                   MOVE      *ON           $INZD1
153200090305     C                   MOVE      'D3'          $GEST
153300090305     C*
153400090305c2125C                   ENDSR
153500090305     C************************************************************
153600090305     C* GESTIONE F24 VIDEO D2
153700090305     C************************************************************
153800090305c2125C     F24D3         BEGSR
153900090305     C*
154000090305     C                   MOVEA     $FX1          $FX
154100090305     C                   MOVEA     $FC1          $FC
154200090305     C                   MOVEA     $FR1          $FR
154300090305     C                   Z-ADD     $ULKD1        $ULK
154400090305     C***                Z-ADD     $D1L01        $L01
154500090305     C***                Z-ADD     $D1L02        $L02
154600090305     C                   EXSR      RIEKEY
154700090305     C                   Z-ADD     $ULK          $ULKD1            3 0
154800090305     C                   MOVEA     $FC           $FC1
154900090305     C                   MOVEL     $KEY1         Z1KE1
155000090305     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
155100090305     C                   MOVEL     $KEY2         Z1KE2
155200090305     C                   WRITE     FMTZ1
155300090305     C*
155400090305c2125C                   ENDSR
155500090305     C************************************************************
155600090305     C* CONTROLLO VIDEATA  D3
155700090305     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
155800090305     C* 95 96 97 98
155900090305     C************************************************************
156000090305c2125C     CTRD3         BEGSR
156100090305     C*
156200090305     C                   SETOFF                                       99
156300090305     C*
156400090305     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
156500090305     C*
156600090305    >C     ANPOPZ        IFNE      '04'
156700090305     C*
156800090305     C* avvisi di scadenza
156900090305     C                   If        D3AvvSca <> XscOn And D3AvvSca <> XscOff
157000090305     C                   SetOn                                        5299
157100090305     C                   EndIf
157200090305     C* solleciti
157300090305     C                   If        D3Sollec <> XscOn And D3Sollec <> XscOff
157400090305     C                   SetOn                                        5599
157500090305     C                   EndIf
157600090422     c* se si nei solleciti, la gestione unità deve essere la stessa
157700090422     c* tra i solleciti e la gestione unità della tabella G20
157800090422     c                   if        D3Sollec =xscon
157900090422     c                   reset                   XatBds
158000090422     C                   EVAL      XtbRic = '1'
158100090422     C                   EVAL      XtbAzi = XscSoc
158200090422     C                   EVAL      XtbKey = *zero
158300090422     C                   EVAL      XtbCod = 'G20'
158400090422     C                   MOVE      *zeros        XtbKey
158500090422     c                   move      'REC'         Xtbkey
158600090422     C                   CALLB     'XATB'
158700090422     C                   PARM                    XAtbDS
158800090422     C                   IF        XtbErr =  '0'
158900090422     C                   EVAL      ANGG20DS = XtbUni
159000130905C2226C* Sostituito controllo con gestione unità di recupero credito
159100130905  "  C* invece che di sollecito.
159200130905  "  X**                 IF        d1GesUni =xscon and gesuniG20 <> *on
159300130905  "  X**                           OR
159400130905  "  X**                           d1GesUni =xscoff and gesuniG20 <> *off
159500130905  "  X**                 seton                                        7499
159600130905  "  X**                 endif
159700130905  "  c                   IF        d1GesUniR=xscon and gesuniG20 <> *on
159800130905  "  c                             OR
159900130905  "  c                             d1GesUniR=xscoff and gesuniG20 <> *off
160000130905  "  c                   seton                                        7499
160100130905C2226C                   endif
160200090422     C                   endif
160300090422     c                   endif
160400090305     C* impegni
160500090305     C                   If        D3Impegni<> XscOn And D3Impegni<> XscOff
160600090305     C                   SetOn                                        5999
160700090305     C                   EndIf
160800100421     c*
160900100421c2156x* controllo regola segnalazioni
161000100421  "  x*                  clear                   D3regCreD
161100100421  "  x*                  if        D3RegCre  <> *blanks
161200100421  "  x*                  Reset                   XAtbDs
161300100421  "  x*                  MOVE      '1'           XTBRIC
161400100421  "  x*                  MOVE      XSCSOC        XTBAZI
161500100421  "  x*                  MOVE      *ZERO         XTBKEY
161600100421  "  x*                  MOVE      '044'         XTBCOD
161700100421  "  x*                  MOVE      D3RegCre      XTBKEY
161800100421  "  x*                  CALLb     'XATB'
161900100421  "  x*                  PARM                    XATBDS
162000100421  "2bx*    XTBERR        IFEQ      '0'
162100100421  "  x*                  MOVEL     XTBUNI        ang044ds
162200100421  "  x*                  MOVEL     Des044        d3RegCreD
162300100421  "2xx*                  ELSE
162400100421  "  x*                  SETON                                        5399
162500100421  "2ex*                  ENDIF
162600100421  "  x*                  endif
162700100421  "  x* controllo regola segnalazione impegni
162800100421  "  x*                  clear                   D3ImpCreD
162900100421  "  x*                  if        D3ImpCre  <> *blanks
163000100421  "  x*                  Reset                   XAtbDs
163100100421  "  x*                  MOVE      '1'           XTBRIC
163200100421  "  x*                  MOVE      XSCSOC        XTBAZI
163300100421  "  x*                  MOVE      *ZERO         XTBKEY
163400100421  "  x*                  MOVE      '044'         XTBCOD
163500100421  "  x*                  MOVE      D3ImpCre      XTBKEY
163600100421  "  x*                  CALLb     'XATB'
163700100421  "  x*                  PARM                    XATBDS
163800100421  "2bx*    XTBERR        IFEQ      '0'
163900100421  "  x*                  MOVEL     XTBUNI        ang044ds
164000100421  "  x*                  MOVEL     Des044        d3ImpCreD
164100100421  "2xx*                  ELSE
164200100421  "  x*                  SETON                                        6099
164300100421  "2ex*                  ENDIF
164400100421c2156x*                  endif
164500090305     C* blocchi impegni
164600090305     C                   If        D3AggBkl <> XscOn And D3AggBkl <> XscOff
164700090305     C                   SetOn                                        6799
164800090305     C                   EndIf
164900090305     c*
165000090305     C* controllo codice blocco
165100090305     c                   clear                   D3BklScaD
165200090305     c                   if        D3BklSca  <> *blanks
165300090305     C                   Reset                   XAtbDs
165400090305     C                   MOVE      '1'           XTBRIC
165500090305     C                   MOVE      XSCSOC        XTBAZI
165600090305     C                   MOVE      *ZERO         XTBKEY
165700090305     C                   MOVE      '0AB'         XTBCOD
165800090305     C                   MOVE      D3BklSca      XTBKEY
165900090305     C                   CALLb     'XATB'
166000090305     C                   PARM                    XATBDS
166100090305   2bC     XTBERR        IFEQ      '0'
166200090305     C                   MOVEL     XTBUNI        ang0abds
166300090305     C                   MOVEL     De20AB        d3BklScaD
166400090305   2xC                   ELSE
166500090305     C                   SETON                                        6899
166600090305   2eC                   ENDIF
166700090305     C                   endif
166800110419c2184C* estrazione automatica
166900110419  "  C                   If        D3EsAutom<> XscOn And D3EsAutom<> XscOff
167000110419  "  C                   SetOn                                        7799
167100110421  "  C                   EndIf
167200110421  "  c* se automatica, il tipo gravità può essere solo '2'
167300110421  "  c                   if        d3Esautom=xscon
167400110421  "  c                   eval      d2TipGra='2'
167500110421c2184c                   endif
167600130906C2226 * Gestione  unità  impostazioni recupero credito
167700130906  "  C                   If        D3GesUniI<>XscOn and D3GesUniI<>XscOff
167800130906  "  C                   SetOn                                        6999
167900130906  "  C                   Else
168000130906  "  C                   If        D3GesUniI=XscOn and Fgo(1)=*Off
168100130906  "  C                   SetOn                                        7099
168200130907  "  C                   Else
168300130907  "  C                   If        D3GesUnii=XscOn and D1GesUniR <> Xscon
168400130907  "  C                   SetOn                                        7199
168500130907  "  C                   EndIf
168600130907  "  C                   EndIf
168700130915  "  C                   EndIf
168800130915  "   * Stampa Lista attività di recupero credito
168900130915  "  C                   If        D3StpAtt<>XscOn and D3StpAtt<>XscOff
169000130915  "  C                   SetOn                                        7399
169100130915C2226C                   Endif
169200090305     C*
169300090305     C                   ENDIF
169400090305     C*
169500090305c2125C                   ENDSR
169600090305     C/EJECT
169700131127C2232C************************************************************
169800131127  "  C* GESTIONE VIDEO RECORD  D4
169900131127  "  C************************************************************
170000131127C2232C     GESD4         BEGSR
170100131127     C* emissione piede videata se proveniente da altra
170200131127     C* salvataggio valore formato di provenienza
170300131127   B1C     $LASTV        IFNE      'D4'
170400131127     C                   WRITE     FMTT1
170500131127     C                   EXSR      GESPIE
170600131127     C                   MOVE      $LASTV        $LASTG
170700131127     C                   MOVE      'D4'          $LASTV
170800131127   E1C                   ENDIF
170900131127     C*
171000131127     C* se esistono errori sulla videata
171100131127     C* emetto la write del formato a indicatori spenti per vedere
171200131127     C* le eventuali decodifiche
171300131127   B1C     *IN99         IFEQ      *ON
171400131127     C     $GEST         ANDEQ     'D4'
171500131127     C                   MOVEA     *IN           WIN
171600131127     C                   MOVE      *ALL'0'       IN5098           49
171700131127     C                   MOVEA     IN5098        *IN(50)
171800131127     C                   WRITE     FMTD4
171900131127     C                   MOVEA     WIN           *IN
172000131127   E1C                   ENDIF
172100131127     C*
172200131127   B1C                   SELECT
172300131127     C* Annullamento
172400131127    >C     ANPOPZ        WHENEQ    '04'
172500131127     C* Visualizzazione
172600131127    >C     ANPOPZ        OREQ      '05'
172700131127     C                   WRITE     FMTD4
172800131127     C                   EXFMT     PROTECT
172900131127     C* Immissione/Modifica/Copia
173000131127   X1C                   OTHER
173100131127     C                   EXFMT     FMTD4
173200131127   E1C                   ENDSL
173300131127     C*
173400131127     C* azzero la posizione del cursore
173500131127     C                   Z-ADD     0             H1RIGA
173600131127     C                   Z-ADD     0             H1COLO
173700131127     C*
173800131127     C* controllo abilitazione tasti funzione
173900131127     C                   EXSR      CTRKD1
174000131127     C*
174100131127   B1C     *IN99         IFEQ      *OFF
174200131127     C*
174300131127   B2C                   SELECT
174400131127     C* ROLDOWN
174500131127     C     $TASTO        WHENEQ    ROLDWN
174600131127     C                   MOVE      'D3'          $GEST
174700131127     C* F3=Fine
174800131127     C     $TASTO        WHENEQ    F03
174900131127     C                   EXSR      F03D1
175000131127     C* F4=Ricerca
175100131127     C     $TASTO        WHENEQ    F04
175200131127     C                   EXSR      SEARCH
175300131127     C* F5=Refresh
175400131127     C     $TASTO        WHENEQ    F05
175500131127     C                   EXSR      F05D4
175600131127     C* F8=Successivo
175700131127     C     $TASTO        WHENEQ    F08
175800131127     C                   EXSR      F08D1
175900131127     C* F12=Ritorno
176000131127     C     $TASTO        WHENEQ    F12
176100131127     C                   EXSR      F12D1
176200131127     C* F24=Altri tasti
176300131127     C     $TASTO        WHENEQ    F24
176400131127     C                   EXSR      F24D4
176500131127     C* Personalizzata
176600131127     C***  $PGF1(PosFun) Whenne    *BLANK
176700131127     C***                EXSR      FperD1
176800131127     C*
176900131127   X2C                   OTHER
177000131127     C                   EXSR      CTRD4
177100131127   B3C     *IN99         IFEQ      *OFF
177200131127     C                   EXSR      CTRD1
177300131127     C  N99              EXSR      CTRD2
177400131127C2232C  N99              EXSR      CTRD3
177500131127     C     *IN99         IFEQ      *ON
177600131127     C* Spengo gli indicatori di errore per il controllo su altri schermi
177700131127     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
177800131127     C* videate
177900131127     C                   MOVE      *ALL'0'       IN5098           49
178000131127     C                   MOVEA     IN5098        *IN(50)
178100131127     C                   SETON                                        9996                       96
178200131127     C                   ENDIF
178300131127     C                   ENDIF
178400131127     C*
178500131127     C*
178600131127   B3C     $TASTO        IFEQ      F06
178700131127     C     *IN99         ANDEQ     *OFF
178800131127     C* eseguo operazioni del dopo-aggiornamento
178900131127   B4C     *IN99         IFEQ      *OFF
179000131127     C                   EXSR      GESAGG
179100131127   E4C                   ENDIF
179200131127   E3C                   ENDIF
179300131127     C*
179400131127   E2C                   ENDSL
179500131127     C*
179600131127   E1C                   ENDIF
179700131127     C*
179800131127C2232C                   ENDSR
179900131127  "  C************************************************************
180000131127  "  C* RIEMPIMENTO VIDEATA D4
180100131127  "  C************************************************************
180200131127C2232C     RIED4         BEGSR
180300131127     C*
180400131128     C                   Movel     Liberoi057    D4Liberoi
180500140311     C                   Movel     CalcGG057     D4CalcGG
180600131127     C*
180700131127     C*
180800131127C2232C                   ENDSR
180900131127  "  C/EJECT
181000131127  "  C************************************************************
181100131127  "  C* GESTIONE F05 VIDEO D4
181200131127  "  C* il tasto F5 su qualsiasi videata provoca il refresh di tutte
181300131127  "  C* e il riposizionamento alla prima
181400131127  "  C************************************************************
181500131127c2232C     F05D4         BEGSR
181600131127     C*
181700131127     C                   MOVE      *ON           $INZD1
181800131127     C                   MOVE      'D4'          $GEST
181900131127     C*
182000131127C2232C                   ENDSR
182100131127  "  C************************************************************
182200131127  "  C* GESTIONE F24 VIDEO D4
182300131127  "  C************************************************************
182400131127C2232C     F24D4         BEGSR
182500131127     C*
182600131127     C                   MOVEA     $FX1          $FX
182700131127     C                   MOVEA     $FC1          $FC
182800131127     C                   MOVEA     $FR1          $FR
182900131127     C                   Z-ADD     $ULKD1        $ULK
183000131127     C***                Z-ADD     $D1L01        $L01
183100131127     C***                Z-ADD     $D1L02        $L02
183200131127     C                   EXSR      RIEKEY
183300131127     C                   Z-ADD     $ULK          $ULKD1            3 0
183400131127     C                   MOVEA     $FC           $FC1
183500131127     C                   MOVEL     $KEY1         Z1KE1
183600131127     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
183700131127     C                   MOVEL     $KEY2         Z1KE2
183800131127     C                   WRITE     FMTZ1
183900131127     C*
184000131127C2232C                   ENDSR
184100131127  "  C************************************************************
184200131127  "  C* CONTROLLO VIDEATA  D4
184300131127  "  C* non usare i seguenti indicatori (perchè relativi a D1MSG):
184400131127  "  C* 95 96 97 98
184500131127  "  C************************************************************
184600131127C2232C     CTRD4         BEGSR
184700131127     C*
184800131127     C                   SETOFF                                       99
184900131127     C*
185000131127     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
185100131127     C*
185200131127    >C     ANPOPZ        IFNE      '04'
185300140320     C*
185400140320     C* TIPO CALCOLO GIORNI
185500140320     C                   MOVEL     *BLANK        D4CALCGGD
185600140320     C                   RESET                   X07DS
185700140320     C                   MOVE      '1'           X07RIC
185800140320     C                   MOVEL     'ANBCO000'    X07TRC
185900140320     C                   MOVEL     'BCOCALDTV'   X07CAM
186000140320     C                   MOVE      *OFF          X07ALL
186100140320     C                   MOVEL     D4calcgg      X07VAL
186200140320     C                   CALLB     'X07VALR'
186300140320     C                   PARM                    X07DS
186400140320     C     X07ERR        IFEQ      '0'
186500140320     C                   MOVEL     X07DES        D4CALCGGD
186600140320     C                   ELSE
186700140320     C                   SETON                                        6699
186800140320     C                   ENDIF
186900140311  "   *
187000131127     C                   ENDIF
187100131127     C*
187200131127C2232C                   ENDSR
187300131127C2232C/EJECT
187400000704     C************************************************************
187500000704     C* RIEMPIMENTO DS per aggiornamento
187600000704     C************************************************************
187700000704    >C     RIEDAT        BEGSR
187800000704     C*
187900000705     C                   If        D1GesUni  =   XscOn
188000000705     C                   Move      'D'           GesUni057
188100000705     C                   Else
188200000705     C                   Move      'G'           GesUni057
188300000705     C                   EndIf
188400130905C2226C                   If        D1GesUniR =   XscOn
188500130905  "  C                   Move      'D'           GesUniR057
188600130905  "  C                   Else
188700130905  "  C                   Move      'G'           GesUniR057
188800130906  "  C                   EndIf
188900130906  "  C                   If        D3GesUniI =   XscOn
189000130906  "  C                   Move      'D'           GesUniI057
189100130906  "  C                   Else
189200130906  "  C                   Move      'G'           GesUniI057
189300130906C2226C                   EndIf
189400000705     C                   Move      D1DomAna      DomAna057
189500000705     C                   Move      D1DomCom      DomCom057
189600000705     C                   If        D1Scadenz =  XScOn
189700000705     C                   Eval      Scadenz057=   *on
189800000705     C                   Else
189900000705     C                   Eval      Scadenz057=   *off
190000000705     C                   EndIf
190100130905C2226C                   If        D1Flagsca =  XScOn
190200130905  "  C                   Eval      FlagSca057=   *on
190300130905  "  C                   Else
190400130905  "  C                   Eval      FlagSca057=   *off
190500130905C2226C                   EndIf
190600000705     C                   Move      D1GesAge      GesAge057
190700000705     C                   Move      D1AnnPar      AnnPar057
190800000705     C                   If        D1AltriTi =   XScOn
190900000705     C                   Eval      AltriTi057=   XScOn
191000000705     C                   Else
191100000705     C                   Eval      AltriTi057=   XScOff
191200000705     C                   EndIf
191300000705     C                   If        D1GesRis  =   XScOn
191400000705     C                   Eval      GesRis057 =   XScOn
191500000705     C                   Else
191600000705     C                   Eval      GesRis057 =   XScOff
191700000705     C                   EndIf
191800130905C2226C                   If        D1FlagRis =   XScOn
191900130905  "  C                   Eval      FlagRis057=   XScOn
192000130905  "  C                   Else
192100130905  "  C                   Eval      FlagRis057=   XScOff
192200130905C2226C                   EndIf
192300000705     C                   If        D1EffARis =   XScOn
192400000705     C                   Eval      EffARis057=   XScOn
192500000705     C                   Else
192600000705     C                   Eval      EffARis057=   XScOff
192700000705     C                   Endif
192800130905C2226C                   If        D1FlagRis =   XScOn
192900130905  "  C                   Eval      FlagRis057=   XScOn
193000130905  "  C                   Else
193100130905  "  C                   Eval      FlagRis057=   XScOff
193200130905C2226C                   Endif
193300000705     C                   If        D1EffASca =   XScOn
193400000705     C                   Eval      EffASca057=   XScOn
193500000705     C                   Else
193600000705     C                   Eval      EffASca057=   XScOff
193700000705     C                   EndIf
193800000705     C                   If        D1NoteCre =   XScOn
193900000705     C                   Eval      NoteCre057=   XScOn
194000000705     C                   Else
194100000705     C                   Eval      NoteCre057=   XScOff
194200000705     C                   EndIf
194300130905C2226C                   If        D1FlagCre =   XScOn
194400130905  "  C                   Eval      FlagCre057=   XScOn
194500130905  "  C                   Else
194600130905  "  C                   Eval      FlagCre057=   XScOff
194700130905C2226C                   EndIf
194800000705     C                   If        D1PartAve =   XScOn
194900000705     C                   Eval      PartAve057=   XScOn
195000000705     C                   Else
195100000705     C                   Eval      PartAve057=   XScOff
195200000705     C                   EndIf
195300130905C2226C                   If        D1FlagAve =   XScOn
195400130905  "  C                   Eval      FlagAve057=   XScOn
195500130905  "  C                   Else
195600130905  "  C                   Eval      FlagAve057=   XScOff
195700130905C2226C                   EndIf
195800000705     C                   If        D1RiePar  =   XScOn
195900000705     C                   Eval      RiePar057 =   XScOn
196000000705     C                   Else
196100000705     C                   Eval      RiePar057 =   XScOff
196200000705     C                   EndIf
196300130905C2226C                   If        D1FlagRies=   XScOn
196400130905  "  C                   Eval      FlagRies57=   XScOn
196500130905  "  C                   Else
196600130905  "  C                   Eval      FlagRies57=   XScOff
196700130905C2226C                   EndIf
196800001003     C                   Move      D2TipGra      TipGra057
196900000705     C                   Move      D2StpLet      StpLet057
197000000705     C                   Z-Add     D2ImpMin      ImpMin057
197100010330     c                   if        d2CauAnn  = xscon
197200010330     C                   move      *on           StpEstr057
197300010330     c                   else
197400010330     C                   move      *off          StpEstr057
197500010330     c                   endif
197600030123c1619C                   Move      D2Divisor     Divisor057
197700030311c1632C                   z-add     D2speinv      SpeInv057
197800051103c1879C                   move      D2posattr     Posattr057
197900090305C2125C                   If        D3AvvSca = XscOn
198000090305  "  C                   Movel     *On           AvvSca057                      *
198100090305  "  C                   Else
198200090305  "  C                   Movel     *Off          AvvSca057
198300090305  "  C                   EndIf
198400090305  "  c                   move      D3RegCre      RegCre057
198500090305  "  C                   If        D3Sollec = XscOn
198600090305  "  C                   Movel     *On           Sollec057                      *
198700090305  "  C                   Else
198800090305  "  C                   Movel     *Off          Sollec057
198900090305  "  C                   EndIf
199000090305  "  C                   If        D3Impegni= XscOn
199100090305  "  C                   Movel     *On           Impegn057                      *
199200090305  "  C                   Else
199300090305  "  C                   Movel     *Off          Impegn057
199400090305  "  C                   EndIf
199500090305  "  c                   move      D3ImpCre      ImpCre057
199600090305  "  c                   z-add     D3GGTol       GGtol057
199700090305  "  c                   z-add     D3GGCad       GGCad057
199800090305  "  c                   movel     D3IndIfs      IndIfs057
199900090305  "  C                   If        D3AggBkl = XscOn
200000090305  "  C                   Movel     *On           AggBkl057                      *
200100090305  "  C                   Else
200200090305  "  C                   Movel     *Off          AggBkl057
200300090305  "  C                   EndIf
200400090305C2125c                   move      D3BklSca      BklSca057
200500100518c2156C                   If        D2DaNSolA= XscOn
200600100518  "  C                   Movel     *On           DaNSol057                      *
200700100518  "  C                   Else
200800100518  "  C                   Movel     *Off          DaNSol057
200900100518  "  C                   EndIf
201000100518  "  C                   If        D2CliSavA= XscOn
201100100518  "  C                   Movel     *On           CliSav057                      *
201200100518  "  C                   Else
201300100518  "  C                   Movel     *Off          CliSav057
201400100518c2156C                   EndIf
201500130404C2221C* Attività non di sollecito ma da sollecitare
201600130404  "  C                   If        D2AdaSol = XscOn
201700130404  "  C                   Movel     *On           ADaSol057                      *
201800130404  "  C                   Else
201900130404  "  C                   Movel     *Off          ADaSol057
202000130412  "  C                   EndIf
202100130412  "  C* Scaduto non presente in recupero credito o attività
202200130412  "  C* chiusa ma da sollecitare.
202300130412  "  C                   If        D2SolNoRe =  XscOn
202400130412  "  C                   Movel     *On           SolNoRe057                     *
202500130412  "  C                   Else
202600130412  "  C                   Movel     *Off          SolNoRe057
202700130412C2221C                   EndIf
202800130524C2224C* Raggruppamento per effetto in stampa sollecito
202900130524  "  C                   If        D2RagrEff =  XscOn
203000130524  "  C                   Movel     *On           RagrEff057                     *
203100130524  "  C                   Else
203200130524  "  C                   Movel     *Off          RagrEff057
203300130524C2224C                   EndIf
203400110419c2184C                   If        D3EsAutom= XscOn
203500110419  "  C                   Movel     *On           EsAutom057                     *
203600110421  "  c                   move      '2'           TipGra057
203700110419  "  C                   Else
203800110419  "  C                   Movel     *Off          EsAutom057
203900110419c2184C                   EndIf
204000130915C2226C                   If        D3StpAtt  =   XscOn
204100130915  "  C                   Move      *on           StpAtt057
204200130915  "  C                   Else
204300130915  "  C                   Move      *off          StpAtt057
204400130915C2226C                   EndIf
204500131128C2232C                   Movel     D4Liberoi     Liberoi057
204600140311C2232C                   Movel     D4CalcGG      CalcGG057
204700000704     C*
204800000704     C                   ENDSR
204900000704     C/EJECT
205000000704     C************************************************************
205100000704     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
205200000704     C************************************************************
205300000704     C     GESAGG        BEGSR
205400000704     C* Riempio campi
205500000704     C                   EXSR      RIEDAT
205600000704     C                   MOVEA     $DT           ANPDAT
205700000704     C*
205800000704     C* segnalo al pgm chiamante l'avvenuta conferma
205900000704    >C                   MOVE      *ON           ANPOPR
206000000704     C*
206100000704     C                   MOVE      *ON           $FINE
206200000704     C*
206300000704     C                   ENDSR
206400000704     C/EJECT
206500000704     C************************************************************
206600000704     C* OPERAZIONI INIZIALI
206700000704     C************************************************************
206800000704     C     *INZSR        BEGSR
206900000704     C*
207000000704     C* Reperimento parametri
207100000704     C*
207200000704     C     *ENTRY        PLIST
207300000704     C                   PARM                    ANPAR
207400000704     C                   PARM                    KPJBA
207500000704      *
207600000704      * Reperimento dati società
207700000704      *
207800000704    >C                   MOVEL     'SOC001'      TIPXSC
207900000704     C                   MOVEL     *BLANK        SOCXSC
208000000704     C                   EXSR      REPSOC
208100000704     C     RTNXSC        IFNE      '1'
208200000704     C                   MOVEL     XSOCDS        SOC001
208300000704     C                   ELSE
208400000704     C                   RETURN
208500000704     C                   ENDIF
208600000704     C*
208700000704     C* Reperimento tasti di funzione
208800000704     C*
208900000704     C                   EXSR      REPKEY
209000000704     C*
209100000704     C                   ENDSR
209200000704     C/EJECT
209300000704     C************************************************************
209400000704     C* REPERIMENTO PARAMETRI GESTIONE
209500000704     C************************************************************
209600000704     C     REPPAR        BEGSR
209700000704     C*
209800000704     C                   CALLb     'XPAR'
209900000704     C                   PARM                    XPASOC            3
210000000704     C                   PARM                    XPAPAR            8
210100000704     C                   PARM                    XPAOUT         2000
210200000704     C                   PARM                    XPAERR            1
210300000704     C                   PARM                    XPARIC            1
210400000704     C*
210500000704     C                   ENDSR
210600000704     C/EJECT
210700000704     C************************************************************
210800000704     C* REPERIMENTO TASTI DI FUNZIONE
210900000704     C************************************************************
211000000704     C     REPKEY        BEGSR
211100000704     C                   MOVEA     $FM1          $FM
211200000704     C                   EXSR      REPKKK
211300000704     C                   MOVEA     $FX           $FX1
211400000704     C*
211500000704     C                   ENDSR
211600000704     C/EJECT
211700000704     C************************************************************
211800000704     C* REPERIMENTO TASTI DI FUNZIONE
211900000704     C************************************************************
212000000704     C     REPKKK        BEGSR
212100000704     C*
212200000704     C                   CLEAR                   $FX
212300000704     C     1             DO        24            X
212400000704     C*
212500000704     C     $FM(X)        IFNE      *BLANK
212600000704     C     $FC(X)        ANDNE     '0'
212700000704     C*
212800000704     C                   MOVEL     $FM(X)        IDMSG
212900000704     C                   CALLB     'XRTVM'
213000000704     C                   PARM                    IDMSG            27
213100000704     C                   PARM                    TXTMSG          100
213200000704     C                   PARM                    ERRMSG            1
213300000704     C*
213400000704     C     ERRMSG        IFNE      *ON
213500000704     C                   MOVEL     TXTMSG        $FX(X)
213600000704     C                   ELSE
213700000704     C                   MOVEL     *ALL' '       $FX(X)
213800000704     C                   ENDIF
213900000704     C*
214000000704     C                   ENDIF
214100000704     C*
214200000704     C                   ENDDO
214300000704     C*
214400000704     C                   ENDSR
214500000704     C/EJECT
214600000704      ************************************************************
214700000704      * Reperimento dati società
214800000704      ************************************************************
214900000704     C     REPSOC        BEGSR
215000000704      *
215100000704     C                   CALLB     'XSOC'
215200000704     C                   PARM                    TIPXSC            6
215300000704     C                   PARM                    SOCXSC            3
215400000704     C                   PARM                    CDSXSC            9 0
215500000704     C                   PARM                    MODXSC            3
215600000704     C                   PARM      *BLANKS       RTNXSC            1
215700000704     C                   PARM                    XSOCDS
215800000704     C                   PARM                    KPJBA
215900000704      *
216000000704     C                   ENDSR
216100000704     C/EJECT
216200000704     C************************************************************
216300000704     C* INIZIALIZZAZIONE VARIABILI
216400000704     C************************************************************
216500000704     C     INZVAR        BEGSR
216600000704     c*
216700000704     C                   MOVEA     ANPDAT        $DT
216800000704     C*
216900000704     C* Definizione dei tasti funzione utilizzabili a pgm
217000000704     C*
217100000704     C                   EXSR      DEFOPZ
217200000704     C*
217300000704     C* Variabili per gestione videate
217400000704     C*
217500000704     C                   MOVE      'D1'          $GEST                          fmt in gestione
217600000704     C                   MOVE      *OFF          $FINE                          fine pgm
217700000704     C                   MOVE      *ON           $INZD1                         inizializzione D1
217800000704     C                   MOVE      *BLANK        $LASTG                         fmt di provenienza
217900000704     C                   MOVE      *BLANK        $LASTV                         ultima emissione
218000000704     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
218100000704     C*
218200000704     C* Variabili appoggio
218300000704     C*
218400000704     C                   Z-ADD     0             CURR
218500000704     C                   Z-ADD     0             CURC
218600000704     C*
218700000704     C* Valorizzazione campi univoci testate
218800000704     C                   CLEAR                   FMTT1
218900000704     C                   MOVEL     KNSIF         NOMSIF
219000000704     C                   MOVEL     XSCDSI        NOMDIT
219100000704     C                   MOVEL     DSPGM         NOMPGM
219200000704     C                   Z-ADD     1             X
219300000704    >C     ANPOPZ        LOOKUP    $OZ(X)                                 21
219400000704     C     *IN21         IFEQ      *ON
219500000704     C                   MOVEL     $ID(X)        IDMSG
219600000704     C                   CALLB     'XRTVM'
219700000704     C                   PARM                    IDMSG            27
219800000704     C                   PARM                    TXTMSG          100
219900000704     C                   PARM                    ERRMSG            1
220000000704     C                   PARM      *ON           CTRMSG            1            centratura
220100000704     C                   PARM      30            LENMSG            3 0          lun output.
220200000704     C     ERRMSG        IFNE      *ON
220300000704     C                   MOVEL     TXTMSG        T1OPE
220400000704     C                   ELSE
220500000704     C                   MOVEL     *ALL'?'       T1OPE
220600000704     C                   ENDIF
220700000704     C                   ENDIF
220800000704     C*
220900000704     C* Inizializzazione/Decodifica prima videata
221000000704     C*
221100000704     C                   EXSR      INZD1
221200000704     C                   MOVE      *OFF          $INZD1
221300000704     C*
221400000704     C                   ENDSR
221500000704     C/EJECT
221600000704     C************************************************************
221700000704     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
221800000704     C************************************************************
221900000704     C     DEFOPZ        BEGSR
222000000704     C*
222100000704     C* Questa routine deve valorizzare la schiera $FC1/$FR1
222200000704     C* con S o N a seconda della utilizzabilità o meno
222300000704     C* del tasto funzione correlato
222400000704     C*
222500000704     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
222600000704     C* di compilazione
222700000704     C                   RESET                   $FC1
222800000704     C                   RESET                   $FR1
222900000704     C*
223000000704     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
223100000704     C* invalido alcune opzioni
223200000704     C                   SELECT
223300000704     C* Se pgm richiamato in interrogazione disattivo :
223400000704    >C     ANPOPZ        WHENEQ    '05'
223500000704     C* F5=Ripristino
223600000704     C                   Z-ADD     5             X
223700000704     C                   MOVE      '0'           $FC1(X)
223800000704     C* Se pgm richiamato in annullamento disattivo :
223900000704    >C     ANPOPZ        WHENEQ    '04'
224000000704     C* F5=Ripristino
224100000704     C                   Z-ADD     5             X
224200000704     C                   MOVE      '0'           $FC1(X)
224300000704     C*
224400000704     C                   OTHER
224500000704     C*
224600000704     C                   ENDSL
224700000704     C*
224800000704     C                   ENDSR
224900000704     C************************************************************
225000000704     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
225100000704     C************************************************************
225200000704     C     DEFCAM        BEGSR
225300000704     C*
225400000704     C* Variabili per gestione videate
225500000704     C*
225600000704     C                   MOVE      *BLANK        $GEST             2            fmt in gestione
225700000704     C                   MOVE      *BLANK        $FINE             1            fine pgm
225800000704     C                   MOVE      *BLANK        $INZD1            1            inizializzione D1
225900000704     C                   MOVE      *BLANK        $LASTG            2            fmt di provenienza
226000000704     C                   MOVE      *BLANK        $LASTV            2            ultima emissione
226100000704     C                   Z-ADD     0             $PIED1            3 0          piede da emett. D1
226200000704     C*
226300000704     C* Variabili appoggio
226400000704     C*
226500000704     C                   Z-ADD     0             CURR              2 0
226600000704     C                   Z-ADD     0             CURC              2 0
226700000704     C                   MOVE      *ZEROS        WIN              99
226800000704     C*
226900000704     C* Indici
227000000704     C*
227100000705     C                   Z-ADD     0             X                 3 0
227200000705     C***                Z-ADD     0             Y                 3 0
227300000704     C*
227400000704     C                   ENDSR
227500000704     C************************************************************
227600000704     C/EJECT
227700000704     C************************************************************
227800000704     C* CONTROLLO ABILITAZIONE TASTI
227900000704     C************************************************************
228000000704     C     CTRKEY        BEGSR
228100000704     C                   SELECT
228200000704     C     $TASTO        WHENEQ    F01
228300000704     C     $FC(1)        IFEQ      '0'
228400000704     C                   SETON                                        99
228500000704     C                   ENDIF
228600000704     C     $TASTO        WHENEQ    F02
228700000704     C     $FC(2)        IFEQ      '0'
228800000704     C                   SETON                                        99
228900000704     C                   ENDIF
229000000704     C     $TASTO        WHENEQ    F03
229100000704     C     $FC(3)        IFEQ      '0'
229200000704     C                   SETON                                        99
229300000704     C                   ENDIF
229400000704     C     $TASTO        WHENEQ    F04
229500000704     C     $FC(4)        IFEQ      '0'
229600000704     C                   SETON                                        99
229700000704     C                   ENDIF
229800000704     C     $TASTO        WHENEQ    F05
229900000704     C     $FC(5)        IFEQ      '0'
230000000704     C                   SETON                                        99
230100000704     C                   ENDIF
230200000704     C     $TASTO        WHENEQ    F06
230300000704     C     $FC(6)        IFEQ      '0'
230400000704     C                   SETON                                        99
230500000704     C                   ENDIF
230600000704     C     $TASTO        WHENEQ    F07
230700000704     C     $FC(7)        IFEQ      '0'
230800000704     C                   SETON                                        99
230900000704     C                   ENDIF
231000000704     C     $TASTO        WHENEQ    F08
231100000704     C     $FC(8)        IFEQ      '0'
231200000704     C                   SETON                                        99
231300000704     C                   ENDIF
231400000704     C     $TASTO        WHENEQ    F09
231500000704     C     $FC(09)       IFEQ      '0'
231600000704     C                   SETON                                        99
231700000704     C                   ENDIF
231800000704     C     $TASTO        WHENEQ    F10
231900000704     C     $FC(10)       IFEQ      '0'
232000000704     C                   SETON                                        99
232100000704     C                   ENDIF
232200000704     C     $TASTO        WHENEQ    F11
232300000704     C     $FC(11)       IFEQ      '0'
232400000704     C                   SETON                                        99
232500000704     C                   ENDIF
232600000704     C     $TASTO        WHENEQ    F12
232700000704     C     $FC(12)       IFEQ      '0'
232800000704     C                   SETON                                        99
232900000704     C                   ENDIF
233000000704     C     $TASTO        WHENEQ    F13
233100000704     C     $FC(13)       IFEQ      '0'
233200000704     C                   SETON                                        99
233300000704     C                   ENDIF
233400000704     C     $TASTO        WHENEQ    F14
233500000704     C     $FC(14)       IFEQ      '0'
233600000704     C                   SETON                                        99
233700000704     C                   ENDIF
233800000704     C     $TASTO        WHENEQ    F15
233900000704     C     $FC(15)       IFEQ      '0'
234000000704     C                   SETON                                        99
234100000704     C                   ENDIF
234200000704     C     $TASTO        WHENEQ    F16
234300000704     C     $FC(16)       IFEQ      '0'
234400000704     C                   SETON                                        99
234500000704     C                   ENDIF
234600000704     C     $TASTO        WHENEQ    F17
234700000704     C     $FC(17)       IFEQ      '0'
234800000704     C                   SETON                                        99
234900000704     C                   ENDIF
235000000704     C     $TASTO        WHENEQ    F18
235100000704     C     $FC(18)       IFEQ      '0'
235200000704     C                   SETON                                        99
235300000704     C                   ENDIF
235400000704     C     $TASTO        WHENEQ    F19
235500000704     C     $FC(19)       IFEQ      '0'
235600000704     C                   SETON                                        99
235700000704     C                   ENDIF
235800000704     C     $TASTO        WHENEQ    F20
235900000704     C     $FC(20)       IFEQ      '0'
236000000704     C                   SETON                                        99
236100000704     C                   ENDIF
236200000704     C     $TASTO        WHENEQ    F21
236300000704     C     $FC(21)       IFEQ      '0'
236400000704     C                   SETON                                        99
236500000704     C                   ENDIF
236600000704     C     $TASTO        WHENEQ    F22
236700000704     C     $FC(22)       IFEQ      '0'
236800000704     C                   SETON                                        99
236900000704     C                   ENDIF
237000000704     C     $TASTO        WHENEQ    F23
237100000704     C     $FC(23)       IFEQ      '0'
237200000704     C                   SETON                                        99
237300000704     C                   ENDIF
237400000704     C     $TASTO        WHENEQ    F24
237500000704     C     $FC(24)       IFEQ      '0'
237600000704     C                   SETON                                        99
237700000704     C                   ENDIF
237800000704     C                   ENDSL
237900000704     C                   ENDSR
238000000704     C/EJECT
238100000704     C************************************************************
238200000704     C* RIEMPIMENTO TASTI FUNZIONE
238300000704     C************************************************************
238400000704     C     RIEKEY        BEGSR
238500000704     C*
238600000704     C* Input :
238700000704     C* - $FX = schiera con tutti i testi disponibili
238800000704     C* - $FC = schiere con i flag di validità associati ai testi
238900000704     C* - $FR = schiere con i flag di REVERSE IMAGE
239000000704     C* - $ULK = ultimo testo caricato
239100000704     C* - $L01 = Lunghezza prima riga
239200000704     C* - $L02 = Lunghezza seconda riga
239300000704     C* Output :
239400000704     C* - $ULK
239500000704     C* - $KEY1= 1° riga di tasti funzione
239600000704     C* - $KEY2= 2° riga di tasti funzione
239700000704     C*
239800000704     C                   CALLB     'XKEYMSG'
239900000704     C* input
240000000704     C                   PARM                    $FX
240100000704     C                   PARM                    $FC
240200000704     C                   PARM                    $FR
240300000704     C                   PARM                    $ULK              3 0
240400000704     C* output
240500000704     C                   PARM                    $KEY1            79
240600000704     C                   PARM                    $KEY2            79
240700000704    >C* input
240800000704    >C                   PARM      79            $L01              3 0
240900000704    >C                   PARM      79            $L02              3 0
241000000704     C*
241100060224A1148C                   PARM                    $ALLFUNCT       480
241200060224A1148C*
241300000704     C                   ENDSR
241400000704**
24150000070401PROMSG    *LIBL     COS0001    immissione
24160000070402PROMSG    *LIBL     COS0002    modifica
24170000070403PROMSG    *LIBL     COS0003    copia
24180000070404PROMSG    *LIBL     COS0004    annullamento
24190000070405PROMSG    *LIBL     COS0005    visualizzazione
242000000704** TASTI DI FUNZIONE  D1
242100000704PROMSG    *LIBL     KEY0001  01
242200000704PROMSG    *LIBL     KEY0002  02
242300000704PROMSG    *LIBL     KEY0003  03
242400000704PROMSG    *LIBL     KEY0004  04
242500000704PROMSG    *LIBL     KEY0005  05
242600000704PROMSG    *LIBL     KEY0006  06
242700000704PROMSG    *LIBL     KEY0007  07
242800000704PROMSG    *LIBL     KEY0008  08
242900000704PROMSG    *LIBL     KEY0009  09
243000000704PROMSG    *LIBL     KEY0010  10
243100000704PROMSG    *LIBL     KEY0011  11
243200000704PROMSG    *LIBL     KEY0012  12
243300000704PROMSG    *LIBL     KEY0013  13
243400000704PROMSG    *LIBL     KEY0014  14
243500000704PROMSG    *LIBL     KEY0015  15
243600000704PROMSG    *LIBL     KEY0016  16
243700000704PROMSG    *LIBL     KEY0017  17
243800000704PROMSG    *LIBL     KEY0018  18
243900000704PROMSG    *LIBL     KEY0019  19
244000000704PROMSG    *LIBL     KEY0020  20
244100000704PROMSG    *LIBL     KEY0021  21
244200000704PROMSG    *LIBL     KEY0022  22
244300000704PROMSG    *LIBL     KEY0023  23
244400000704PROMSG    *LIBL     KEY0024  24
244500000704**  ABILITAZIONE D1
2446000007040N  01
2447000007040N  02
2448000007040N  03
2449000007041N  04
2450000007041N  05
2451000007041N  06
2452000007040N  07
2453000007040N  08
2454000007040N  09
2455000007040N  10
2456000007040N  11
2457000007041N  12
2458000007040N  13
2459000007040N  14
2460000007040N  15
2461000007040N  16
2462000007040N  17
2463000007040N  18
2464000007040N  19
2465000007040N  20
2466000007040N  21
2467000007040N  22
2468000007040N  23
2469000007040N  24   Lasciare sempre impostata a '0' la colonna $FC
