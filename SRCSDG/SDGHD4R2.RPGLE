000100100120     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200100120     H*PARMS CVTOPT(*DATETIME)
000300100120     H DECEDIT('0,') DATEDIT(*DMY.)
000400100120      *
000500100120      *                  ---------------------------
000600170616      *                     GESTIONE TABELLA  HD4
000700100120      *                  ---------------------------
000800100120      *
000900100120     F*----------------------------------------------------*
001000100120     FANTBT01L  UF   E           K DISK
001100100120     F*---
001200170616     FSDGHD4D2  CF   E             WORKSTN
001300100120     F                                     INFDS(DSFMT)
001400100120     D*----------------------------------------------------*
001500100120     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
001600100120     D $ID             S             27    DIM(5) ALT($OZ)
001700100120     D* Schiere per la gestione dei tasti di funzione
001800100120     D $FM             S             27    DIM(24)
001900100120     D $FX             S             50    DIM(24)
002000100120     D $FC             S              1    DIM(24)
002100100120     D $FR             S              1    DIM(24)
002200100120     D* Video D1
002300100120     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
002400100120     D $FX1            S             50    DIM(24) ALT($FM1)
002500100120     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
002600100120     D $FR1            S              1    DIM(24) ALT($FC1)
002700100120     D*
002800100120     D $D1L01          S              3  0
002900100120     D $D1L02          S              3  0
003000100120     D* campi in lingua
003100100120$010 D LIN             S             06    DIM(0001) CTDATA PERRCD(001)
003200100120$010 D LIND            S             50    DIM(0001) ALT(LIN)
003300170616$003 D EXTDS         E DS                  EXTNAME(SDGHD4DS) INZ
003400100120     D* Passaggio Parametri
003500100120     D KPJBA         E DS
003600100120     D*-------------
003700100120     D SDGTABDS      E DS                  INZ
003800100618     D*-------------
003900100618     D* Tabella degli Stati
004000100618     D ANGG12DS      E DS                  INZ
004100100120     D*-------------
004200100120     D* Parametri per ricerca codice ANATB00F
004300100618     D XTABDS        E DS                  inz
004400100120     D*-------------
004500100120      * Società
004600100120     D SOC001        E DS                  EXTNAME(SDGSOCDS)
004700100120     D*-------------
004800100120      * DS Interna per dati utente
004900100120     D XSOCDS          DS          1000
005000100120     D*-------------
005100100120     D* DS Esterna autorizzazioni tabelle
005200100120     D XCHATB        E DS                  EXTNAME(XCHKATBDS)
005300100120     D*
005400100120     D* DS  restituzione autorizzazioni
005500100120     D* IND=indice
005600100120     D* GRU=man. di gruppo
005700100120     D* AZI=man. azienda
005800100120     D* ABI=abilit. ai riservati
005900100120     D AUTDS           DS
006000100120     D  MANIND                 1      1
006100100120     D  MANGRU                 2      2
006200100120     D  MANAZI                 3      3
006300100120     D  ABIRIS                 4      4
006400100120     D*-------------
006500100120     D* opzione richiesta
006600100120    >D D002DS          DS
006700100120     D  OPZ002                 1      2
006800100120     D* 0 = ritorno normale
006900100120     D* 1 = F3
007000100120     D* 2 = F12
007100100120     D  RET002                 3      3
007200100120     D* operazione eseguite dal pgm chiamato :
007300100120     D* 0 = nessuna operazione
007400100120     D* 1 = eseguito aggiornamento
007500100120     D  OPR002                 4      4
007600100120     D* funzione non eseguibile per errore :
007700100120     D* 0 = nessun errore
007800100120     D* 1 = funzione richiamata chiusa in errore
007900100120     D* Aggiungere eventuali altri codici di errore
008000100120     D  ERR002                 5      5
008100100120     D* Azienda
008200100120     D  AZI002                 6      8
008300100120     D* Lingua
008400100120     D  LIN002                 9      9
008500100120     D* Codice scelto
008600100120     D  KEY002                10     24
008700100120     D* Autorizzazione
008800100120     D  AUT002                25     25
008900100120     D* Unita
009000100120     D  UNI002                26     33
009100100120     D*-------------
009200100120     D DSFMT           DS
009300100120     D  $TASTO               369    369
009400100120     D  NRG                  370    370
009500100120     D  NCL                  371    371
009600100120     D*-------------
009700100120     D* posizione cursore
009800100120     D CURSOR          DS
009900100120     D  RIRI                   1      2B 0 INZ
010000100120     D  R$$                    2      2
010100100120     D  COCO                   3      4B 0 INZ
010200100120     D  C$$                    4      4
010300100120     D*-------------
010400100120     D* Reperimento nome PGM
010500100120     D STATUS         SDS           333
010600100120     D  DSPGM            *PROC
010700100120     D*-------------
010800100120     D* Reperimento titolo tabella
010900100120     D  WRKTIT         S             35A
011000100120     D  WRKTBCOD       S                   LIKE(XTBCOD)
011100100120     D  LENTIT         S              3  0
011200100120     D  I              S              3  0
011300100120     D*-------------
011400100120     D* COSTANTI
011500100120     D*-------------
011600100120     D*
011700100120     D* Tasti di funzione
011800100120     D F01             C                   CONST(X'31')
011900100120     D F02             C                   CONST(X'32')
012000100120     D F03             C                   CONST(X'33')
012100100120     D F04             C                   CONST(X'34')
012200100120     D F05             C                   CONST(X'35')
012300100120     D F06             C                   CONST(X'36')
012400100120     D F07             C                   CONST(X'37')
012500100120     D F08             C                   CONST(X'38')
012600100120     D F09             C                   CONST(X'39')
012700100120     D F10             C                   CONST(X'3A')
012800100120     D F11             C                   CONST(X'3B')
012900100120     D F12             C                   CONST(X'3C')
013000100120     D F13             C                   CONST(X'B1')
013100100120     D F14             C                   CONST(X'B2')
013200100120     D F15             C                   CONST(X'B3')
013300100120     D F16             C                   CONST(X'B4')
013400100120     D F17             C                   CONST(X'B5')
013500100120     D F18             C                   CONST(X'B6')
013600100120     D F19             C                   CONST(X'B7')
013700100120     D F20             C                   CONST(X'B8')
013800100120     D F21             C                   CONST(X'B9')
013900100120     D F22             C                   CONST(X'BA')
014000100120     D F23             C                   CONST(X'BB')
014100100120     D F24             C                   CONST(X'BC')
014200100120     D ENTER           C                   CONST(X'F1')
014300100120     D ROLDWN          C                   CONST(X'F4')
014400100120     D ROLLUP          C                   CONST(X'F5')
014500100120     D*-------------
014600100120     D*
014700100120     D DATA            C                   CONST('0001-01-01')
014800100120     I*----------------------------------------------------*
014900100120     ID1
015000170616$005 I              D1DESCR                     DESCRHD4
015100170616$005 I              D1DES                       DESHD4
015101170616$005 I              D1CODISO                    CODISOHD4
015200170616$005 I              D1CODISO2                   CODISO2HD4
015300100120     I*
015400100120     C*----------------------------------------------------*
015500100120     C*                MAIN LINE PROGRAM
015600100120     C*----------------------------------------------------*
015700100120     C*
015800100120     C* inizializzazione variabili
015900100120     C                   EXSR      INZVAR
016000100120     C*
016100100120     C     $FINE         DOWEQ     *OFF
016200100120     C     $GEST         CASEQ     'D1'          GESD1
016300100120     C                   END
016400100120     C                   END
016500100120     C*
016600100120     C* fine programma
016700100120     C                   EXSR      ENDPGM
016800100120     C/EJECT
016900100120     C************************************************************
017000100120     C* RICERCHE
017100100120     C************************************************************
017200100120     C     ENDPGM        BEGSR
017300100120     C*
017400100120     C* disallocazione rcd nel caso di
017500170616$007 C                   MOVE      'HD4'         XTBCOD
017600100120    >C                   MOVE      '7'           XTBRIC
017700100120     C*
017800100120     C                   CALL      'SDGTABDR'
017900100120     C                   PARM                    SDGTABDS
018000100120     C*
018100100120     C* passaggio dati al pgm chiamante
018200100120     C                   MOVE      *BLANKS       KPJBU
018300100120    >C                   MOVEL     D002DS        KPJBU
018400100120     C*
018500100120     C                   SETON                                            RT
018600100120     C*
018700100120     C                   ENDSR
018800100120     C/EJECT
018900100120     C************************************************************
019000100120     C* RICERCHE
019100100120     C************************************************************
019200100120     C     SEARCH        BEGSR
019300100120     C*
019400100120     C* determino Riga/Colonna del cursore
019500100120     C                   MOVE      NRG           R$$
019600100120     C                   MOVE      NCL           C$$
019700100120     C                   Z-ADD     RIRI          CURR
019800100120     C                   Z-ADD     COCO          CURC
019900100120     C*
020000100120     C* I campi che al momento dell'F4 risultano protetti non devono
020100100120     C* essere abilitati alla funzione di ricerca
020200100120     C*
0203001001201    C                   SELECT
020400100618     C* FMTD1 - Stato
020500100618     C     H1NMFL        WHENEQ    'D1COD'
020600100120     C                   RESET                   XTABDS
020700100618     C*
020800100618     C                   Select
020900100618     C     OPZ002        WHENEQ    '01'
021000100618     C     OPZ002        OREQ      '03'
021100100618     C                   MOVE      '01'          XTAOPZ
021200100618     C*
021300100618     C                   OTHER
021400100618     C                   MOVE      '05'          XTAOPZ
021500100618     C                   endSL
021600100618     C*
021700100120     C                   MOVE      XSCSOC        XTAAZI
021800100120     C                   MOVE      XSCLIN        XTALIN
021900100120     C                   Z-ADD     CURR          XTARIG
022000100120     C                   Z-ADD     CURC          XTACOL
022100100120     C                   CLEAR                   KPJBU
022200100120     C                   MOVEL     XTABDS        KPJBU
022300100618     C                   CALL      'ANGG12R'
022400100120     C                   PARM                    KPJBA
022500100120     C                   MOVEL     KPJBU         XTABDS
022600100120     C     XTARet        IFNE      '2'
022700100618     C                   MOVE      XTAKEY        D1Cod
022800100618     C*
022900100618     C                   Clear                   SDGTABDS
023000100618     C*
023100100618     C                   MOVE      XSCCUT        XTBUTE
023200100618     C                   MOVE      '1'           XTBRIC
023300100618     C                   MOVE      XSCSOC        XTBAZI
023400100618     C                   MOVE      *ZERO         XTBKEY
023500100618     C                   MOVE      'G12'         XTBCOD
023600100618     C                   MOVE      D1Cod         XTBKEY
023700100618     C*
023800100618     C                   CALL      'SDGTABDR'
023900100618     C                   PARM                    SDGTABDS
024000100618     C*
024100100618     C     XTBERR        IFEQ      *ON
024200100618     C                   ELSE
024300100618     C                   MOVEL     XTBUNI        ANGG12DS
024400100618     C*
024500170616     C                   if        DescrHD4    = *Blank
024600170616     C                   MOVEL     DesG12        DescrHD4
025200100618     C                   endif
025201170616R067 C                   if        CodISOG12  <> *blanks     and
025202170616     C                             CodISOHD4   = *Blank
025204170616R067 C                   MOVEL     CodISOG12     CodISOHD4
025205170616     C                   endif
025400100618     C*
025500100618     C                   ENDIF
025600100120     C                   ENDIF
025700100120     C*
025800100120     C* ricerca non abilitata in questa posizione
0259001001201O   C                   OTHER
026000100120     C                   SETON                                        9899
026100100120     C* imposto pos. del cursore
026200100120     C                   Z-ADD     CURR          H1RIGA
026300100120     C                   Z-ADD     CURC          H1COLO
026400100120     C*
0265001001201-   C                   ENDSL
026600100120     C*
026700100120     C                   MOVE      *HIVAL        $LASTV
026800100120     C                   ENDSR
026900100120     C/EJECT
027000100120     C************************************************************
027100100120     C* TRADUZIONI
027200100120     C************************************************************
027300100120     C     LINGUA        BEGSR
027400100120     C*
027500100120     C* determino Riga/Colonna del cursore
027600100120     C                   MOVE      NRG           R$$
027700100120     C                   MOVE      NCL           C$$
027800100120     C                   Z-ADD     RIRI          CURR
027900100120     C                   Z-ADD     COCO          CURC
028000100120     C*
028100100120     C*
028200100120     C*
028300100120     C* FMTD1  - CAMPI DA TRADURRE
028400100120     C* imposto pos. del cursore
028500100120     C                   Z-ADD     CURR          H1RIGA
028600100120     C                   Z-ADD     CURC          H1COLO
028700100120     x****               MOVEL     H1NMFL        W005              5
028800100120     x****               MOVE      W005          W003              3
028900100120     x****               MOVEL     W003          W006              6
029000100120     C                   MOVEL     H1NMFL        W008              8
029100100120     C                   MOVE      W008          W003              6
029200100120     C                   MOVE      W008          W006              6
029300100120     C                   MOVE      *ZERO         KEY              15
029400100120     C                   MOVE      D1COD         KEY
029500100120     C                   SELECT
029600100120$014 C     W003          WHENEQ    'DESCR '
029700100120$014 C                   Z-ADD     1             I                 3 0
029800100120$014 C     W006          LOOKUP    LIN(I)                                 21
029900100120$014 C                   CALL      'ANA101R '
030000100120$014 C                   PARM      W003          CAMPO            10
030100170616$014 C                   PARM      'HD4'         COD               3
030200100120$014 C                   PARM                    KEY              15
030300100120$014 C                   PARM      020           LUN               3 0
030400100120$014 C                   PARM      001           POS               3 0
030500170616$014 C     DESCRHD4      PARM      DESCRHD4      DES              50
030600100120$014 C                   PARM      LIND(I)       FLD              50
030700100120$014 C                   PARM      XTBAZI        AZI               3
030800100120$014 C                   PARM      LIN002        LNG               1
030900100120$014 C                   PARM      OPZ002        OPZ               2
031000100120     C*
031100100120     C*
031200100120     C* ricerca non abilitata in questa posizione
0313001001201O   C                   OTHER
031400100120     C                   SETON                                        98
031500100120     C*
0316001001201-   C                   ENDSL
031700100120     C*
031800100120     C                   MOVE      *HIVAL        $LASTV
031900100120     C                   ENDSR
032000100120     C/EJECT
032100100120     C************************************************************
032200100120     C* GESTIONE EMISSIONE TASTI FUNZIONE
032300100120     C************************************************************
032400100120     C     GESPIE        BEGSR
032500100120     C*
032600100120     C                   SELECT
032700100120     C*
032800100120     C     $GEST         WHENEQ    'D1'
032900100120     C     $ULKD1        IFEQ      0
033000100120     C* riempimento opzioni
033100100120     C                   EXSR      F24D1
033200100120     C                   ELSE
033300100120     C                   WRITE     Z1
033400100120     C                   ENDIF
033500100120     C*
033600100120     C*
033700100120     C                   ENDSL
033800100120     C*
033900100120     C                   ENDSR
034000100120     C/EJECT
034100100120     C************************************************************
034200100120     C* GESTIONE VIDEO RECORD D1
034300100120     C************************************************************
034400100120     C     GESD1         BEGSR
034500100120     C* inizializzazione videata
034600100120     C     $INZD1        IFEQ      *ON
034700100120     C                   EXSR      INZD1
034800100120     C                   MOVE      *OFF          $INZD1
034900100120     C                   END
035000100120     C* emissione piede videata se proveniente da altra
035100100120     C* salvataggio valore formato di provenienza
035200100120     C     $LASTV        IFNE      'D1'
035300100120     C                   WRITE     T1
035400100120     C                   EXSR      GESPIE
035500100120     C                   MOVE      $LASTV        $LASTG
035600100120     C                   MOVE      'D1'          $LASTV
035700100120     C                   END
035800100120     C*
035900100120     C                   SELECT
036000100120     C* Annullamento
036100100120    >C     OPZ002        WHENEQ    '04'
036200100120     C* Visualizzazione
036300100120    >C     OPZ002        OREQ      '05'
036400100120     C                   SETON                                        0203
036500100120     C* Manutenzione
036600100120    >C     OPZ002        WHENEQ    '02'
036700100120     C                   SETON                                        02
036800100120     C                   SETOFF                                       03
036900100120     C*
037000100120     C                   OTHER
037100100120     C*
037200100120     C                   SETOFF                                       0203
037300100120     C                   ENDSL
037400100120     C*
037500100120     C* Immissione/Modifica/Copia
037600100120     C                   if        *In99 = *On
037700100120     C                   SETOFF                                       99
037800100120     C                   WRITE     D1
037900100120     C                   SETON                                        99
038000100120     C                   endif
038100100120     C* Immissione/Modifica/Copia
038200100120     C                   EXFMT     D1
038300100120     C* controllo abilitazione tasti funzione
038400100120     C*
038500100120     C                   EXSR      CTRKD1
038600100120     C*
038700100120     C     *IN99         IFEQ      *OFF
038800100120     C*
0389001001201    C                   SELECT
039000100120     c* ROLLUP
039100100120     C* F3=Fine
039200100120     C     $TASTO        WHENEQ    F03
039300100120     C                   EXSR      F03D1
039400100120     C* F4=Ricerca
039500100120     C     $TASTO        WHENEQ    F04
039600100120     C                   EXSR      SEARCH
039700100120     C* F5=Refresh
039800100120     C     $TASTO        WHENEQ    F05
039900100120     C                   EXSR      F05D1
040000100120     C* F8=Successivo
040100100120     C     $TASTO        WHENEQ    F08
040200100120     C                   EXSR      F08D1
040300100120     C* F12=Ritorno
040400100120     C     $TASTO        WHENEQ    F12
040500100120     C                   EXSR      F12D1
040600100120     C* F24=Altri tasti
040700100120     C     $TASTO        WHENEQ    F24
040800100120     C                   EXSR      F24D1
040900100120     C*
0410001001201O   C                   OTHER
041100100120     C                   EXSR      CTRD1
041200100120     C     *IN99         IFEQ      *OFF
041300100120     C                   SELECT
041400100120     C* F7=Traduzione
041500100120     C     $TASTO        WHENEQ    F07
041600100120     C                   EXSR      LINGUA
041700100120     C* F6
0418001001202    C     $TASTO        WHENEQ    F06
041900100120     C* eseguo aggiornamento
042000100120    >C                   EXSR      AGGANA
042100100120     C* eseguo operazioni del dopo-aggiornamento
0422001001203    C     *IN99         IFEQ      *OFF
042300100120     C                   EXSR      GESAGG
0424001001203-   C                   ENDIF
0425001001202-   C                   ENDSL
042600100120     C                   ENDIF
042700100120     C*
0428001001201-   C                   ENDSL
042900100120     C*
0430001001202-   C                   ENDIF
043100100120     C                   ENDSR
043200100120     C/EJECT
043300100120     C************************************************************
043400100120     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
043500100120     C************************************************************
043600100120     C     CTRKD1        BEGSR
043700100120     C                   SETOFF                                       99
043800100120     C                   MOVEA     $FC1          $FC
043900100120     C                   EXSR      CTRKEY
044000100120     C     *IN99         IFEQ      *ON
044100100120     C                   SETON                                        97
044200100120     C                   ENDIF
044300100120     C                   ENDSR
044400100120      ************************************************************
044500100120      * INIZIALIZZAZIONE VIDEATA DATI
044600100120      ************************************************************
044700100120     C     INZD1         BEGSR
044800100120     C*
044900100120     C                   CLEAR                   D1
045000100120     C*
045100100120     C* reperimento titolo
045200100120     C                   clear                   SDGTABDS
045300100120     C                   eval      xtbkey      = *zero
045400100120     C* inserita la seguenta riga
045500170616     C                   move      'HD4'         xtbkey
045600100120     C                   eval      xtbazi      = xscsoc
045700100120     C                   eval      xtblin      = xsclin
045800100120     C                   eval      xtbcod      = 'AAA'
045900100120     C                   eval      xtbric      = '1'
046000100120     C                   eval      xtbalc      = '0'
046100100120     C*
046200100120     C                   CALL      'SDGTABDR'
046300100120     C                   PARM                    SDGTABDS
046400100120     C*
046500100120     C                   if        xtberr <> *off
046600100120     C                   movel     *all'?'       WrkTit
046700100120     C                   else
046800100120     C                   movel     xtbuni        WrkTit
046900100120     C                   move      '     '       WrkTit
047000100120     C                   endif
047100100120     C*
047200100120     C* centratura
047300100120     C                   eval      WrkTit=%trimr(Wrktit)
047400100120     C     ' '           CHECKR    Wrktit        I
047500100120     C                   call      'X71CENTRA'
047600100120     C                   parm                    Wrktit
047700100120     C                   parm                    I
047800100120     C                   parm                    lentit
047900100120     C                   movel     Wrktit        T1tit
048000100120     C*
048100100120     C* Aggiunta riga seguente
048200100120     C
048300100120     C                   MOVE      '0'           D1RIS
048400100120     C                   MOVE      KEY002        D1COD
048500100120     C                   MOVE      AZI002        XTBAZI
048600100120     C                   MOVE      LIN002        XTBLIN
048700100120     C                   MOVE      *ALL'0'       XTBKEY
048800100120     C                   MOVE      KEY002        XTBKEY
048900100120     C                   MOVE      UNI002        XTBUNT
049000170616     C                   MOVE      'HD4'         XTBCOD
049100100120     C*
049200100120     C*
049300100120     C* valorizzo la chiave univoca per reperire i dati
049400100120     C* se non immissione
0495001001201   >C     OPZ002        IFNE      '01'
049600100120    >C*-------------------------------------------------------------
049700100120    >C* impostare i campi chiave ricevuti
049800100120    >C*-------------------------------------------------------------
049900100120     C*
050000100120     C*
050100100120     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
0502001001202   >C     OPZ002        IFEQ      '02'
050300100120    >C     OPZ002        OREQ      '04'
050400100120     C                   MOVE      '1'           XTBALC
050500100120     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
0506001001202E   C                   ELSE
050700100120     C                   MOVE      '0'           XTBALC
0508001001202-   C                   ENDIF
050900100120     C*
051000100120     C                   MOVE      '1'           XTBRIC
051100100120     C*
051200100120     C                   CALL      'SDGTABDR'
051300100120     C                   PARM                    SDGTABDS
051400100120     C*
051500100120     C     XTBERR        IFEQ      '0'
051600100120     C                   MOVEL     XTBUNI        EXTDS
051700100120     c                   move      xtbunt        d1uniTA           8
051800100120     C* Giro la data se non blank
051900100120     c                   move      xtbdsc        xtbdscA          10
052000100120     C                   MOVE      XTBRIS        D1RIS
052100100120     C     XTBDSCa       IFNE      DATA
052200100120     C                   CALLB     'XDT4000'
052300100120     C                   PARM      XTBDSCA       XDTAMG           10
052400100120     C                   PARM      *ZEROS        XDTGMA            6 0
052500100120     C                   PARM      2             XDTSTA            1 0
052600100120     C                   MOVE      XDTGMA        D1DSC
052700100120     C                   ELSE
052800100120     C                   MOVE      *ZEROS        D1DSC
052900100120     C                   ENDIF
053000100120     C                   ENDIF
053100100120     C*
053200100120     C*
053300100120     C* se il rcd da gestire non viene trovato
053400100120     C* oppure è allocato da altro pgm
053500100120     C* ritorno al pgm guida avvertendolo dell'errore
0536001001202    C     XTBERR        IFNE      '0'
053700100120     C                   MOVE      *ON           $FINE
053800100120    >C                   MOVE      '1'           ERR002
053900100120     C*
054000100120     C* altrimenti
0541001001202E   C                   ELSE
054200100120     C* riempio la videata
054300100120     C                   EXSR      RIED1
054400100120     C* richiamo routine dei ctrl per decodificarla
054500100120    >C                   EXSR      CTRD1
054600100120     C                   MOVEL     *IN99         SAV99             1
054700100120     C* Se la gestione è più videate
054800100120    >C                   EXSR      CTRD2
054900100120    >C     *IN99         IFEQ      *ON
055000100120    >C                   MOVEL     *IN99         SAV99             1
055100100120    >C                   ENDIF
055200100120     C                   MOVE      *ALL'0'       IN5098
055300100120     C                   MOVEA     IN5098        *IN(50)
055400100120     C* definizione indicatori su video
055500100120     C                   SETOFF                                       02
0556001001203    C                   SELECT
055700100120    >C     OPZ002        WHENEQ    '02'
055800100120     C                   SETON                                        02
055900100120    >C     OPZ002        WHENEQ    '05'
056000100120     C* disabilita tasti
056100100120     C                   MOVE      '0'           $FC(6)
056200100120     C                   MOVE      '0'           $FC(5)
056300100120     C                   MOVE      '0'           $FC(4)
0564001001203-   C                   ENDSL
056500100120     C*
0566001001202-   C                   ENDIF
056700100120     C*
0568001001201-   C                   ENDIF
056900100120     C*
057000100120     C                   ENDSR
057100100120     C/EJECT
057200100120     C************************************************************
057300100120     C* RIEMPIMENTO VIDEATA  D1
057400100120     C************************************************************
057500100120     C     RIED1         BEGSR
057600100120     C*
057700100120$025 C
057800100120     C*
057900100120     C                   ENDSR
058000100120     C/EJECT
058100100120     C************************************************************
058200100120     C* GESTIONE F03 VIDEO D1
058300100120     C************************************************************
058400100120     C     F03D1         BEGSR
058500100120     C*
058600100120     C                   MOVE      *ON           $FINE
058700100120    >C                   MOVE      '1'           RET002
058800100120     C* fine programma
058900100120     C                   EXSR      ENDPGM
059000100120     C*
059100100120     C                   ENDSR
059200100120     C/EJECT
059300100120     C************************************************************
059400100120     C* GESTIONE F05 VIDEO D1
059500100120     C************************************************************
059600100120     C     F05D1         BEGSR
059700100120     C*
059800100120     C                   MOVE      *ON           $INZD1
059900100120     C*
060000100120     C                   ENDSR
060100100120     C/EJECT
060200100120     C************************************************************
060300100120     C* GESTIONE F08 VIDEO D1
060400100120     C************************************************************
060500100120     C     F08D1         BEGSR
060600100120     C*
060700100120     C                   MOVE      *ON           $FINE
060800100120    >C                   MOVE      '0'           RET002
060900100120     C*
061000100120     C                   ENDSR
061100100120     C/EJECT
061200100120     C************************************************************
061300100120     C* GESTIONE F12 VIDEO D1
061400100120     C************************************************************
061500100120     C     F12D1         BEGSR
061600100120     C*
061700100120     C                   MOVE      *ON           $FINE
061800100120    >C                   MOVE      '2'           RET002
061900100120     C*
062000100120     C                   ENDSR
062100100120     C/EJECT
062200100120     C************************************************************
062300100120     C* GESTIONE F24 VIDEO D1
062400100120     C************************************************************
062500100120     C     F24D1         BEGSR
062600100120     C*
062700100120     C*
062800100120     C                   MOVEA     $FX1          $FX
062900100120     C                   MOVEA     $FC1          $FC
063000100120     C                   MOVEA     $FR1          $FR
063100100120     C                   Z-ADD     $ULKD1        $ULK
063200100120     C                   Z-ADD     $D1L01        $L01
063300100120     C                   Z-ADD     $D1L02        $L02
063400100120     C                   EXSR      RIEKEY
063500100120     C                   MOVEA     $FC           $FC1
063600100120     C                   Z-ADD     $ULK          $ULKD1            3 0
063700100120     C                   MOVEL     $KEY1         Z1KE1
063800100120     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
063900100120     C                   MOVEL     $KEY2         Z1KE2
064000100120     C                   WRITE     Z1
064100100120     C*
064200100120     C                   ENDSR
064300100120     C/EJECT
064400100120     C************************************************************
064500100120     C* CONTROLLO VIDEATA
064600100120     C************************************************************
064700100120     C     CTRD1         BEGSR
064800100120     C*
064900100120     C                   SETOFF                                       99
065000100120     C* non imposto pos. del cursore
065100100120     C                   Z-ADD     0             H1RIGA
065200100120     C                   Z-ADD     0             H1COLO
065300100120     C*
065400100120     C* CTRL DATA
065500100120     C*
065600100120     C                   MOVE      DATA          WRKDSC           10
065700100120     C     D1DSC         IFNE      0
065800100120     C                   CALLB     'XDT4000'
065900100120     C                   PARM                    XDTAMG           10
066000100120     C                   PARM      D1DSC         XDTGMA            6 0
066100100120     C                   PARM      3             XDTSTA            1 0
066200100120     C     XDTSTA        IFNE      0
066300100120     C                   SETON                                        5599
066400100120     C                   ELSE
066500100120     C                   MOVE      XDTAMG        WRKDSC
066600100120     C                   ENDIF
066700100120     C                   ENDIF
066800100120     C*
066900100120     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
067000100120     C*
0671001001201   >C     OPZ002        IFNE      '04'
067200100618     C*
067300100618     C* codice stato
067400100618     C                   EXSR      CtrCod
067500100618     C*
067600100618     C                   EXSR      CtrDescr
067700100618     C*
067701170616     C* codice stato Iso Num. di 3
067702170616     C                   EXSR      CtrCodISO
067800170704     C* controllo immissione di almeno un codice iso
067900170704     C                   EXSR      CtrUnISO
068000100120     C*
068100100618     C*
068200100120     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
068300100120    >C     OPZ002        IFEQ      '01'
068400100120    >C     OPZ002        OREQ      '03'
068500100120    >C*-------------------------------------------------------------
068600100120    >C* impostare i campi chiave e verificare l'esistenza del record
068700100120    >C*-------------------------------------------------------------
068800170616$007 C                   MOVE      'HD4'         XTBCOD
068900100120     C                   MOVE      *ALL'0'       XTBKEY
069000100120    >C                   MOVE      D1COD         XTBKEY
069100100120    >C                   MOVE      D1UNITA       XTBUNT
069200100120     C                   MOVE      '0'           XTBALC
069300100120     C                   MOVE      '1'           XTBRIC
069400100120     C                   CALL      'SDGTABDR'
069500100120     C                   PARM                    SDGTABDS
069600100120     C     XTBERR        IFEQ      '0'
069700100120     C                   SETON                                        5999
069800100120     C                   ENDIF
069900100120     C* Controllo se compreso nei range indice
070000100120     C     XTBERR        IFEQ      '3'
070100100120    >C                   SETON                                        5899
070200100120     C                   END
070300100120     C*
070400100120     C* REPERISCO AUTORIZZAZIONI UTENTE
070500100120     C                   CLEAR                   XCHATB
070600100120     C                   MOVE      XSCSOC        XCTSOC
070700100120     C                   MOVE      XSCDUN        XCTUNI
070800100120     C                   MOVEL     KNMUS         XCTPRF
070900100120     C                   MOVE      XTBCOD        XCTCOD
071000100120     C                   CALL      'XCHKATB'
071100100120     C                   PARM                    XCHATB
071200100120     C                   MOVEL     XCTDDS        AUTDS
071300100120     C     XCTRTN        IFEQ      1
071400100120     C                   MOVEL     'N'           MANGRU
071500100120     C                   ENDIF
071600100120     C* Non ammessa immissione di gruppo
071700100120     C     XTBGRU        IFEQ      'G'
071800100120     C     MANGRU        ANDEQ     'N'
071900100120     C                   SETON                                        5799
072000100120     C                   END
072100100120     C* Non ammessa immissione di azienda
072200100120     C     XTBGRU        IFEQ      'A'
072300100120     C     MANAZI        ANDEQ     'N'
072400100120     C                   SETON                                        5699
072500100120     C                   END
072600100120     C* codice riservato non accessibile
072700100120     C     XTBRIS        IFGT      ABIRIS
072800100120    >C     OPZ002        ANDNE     '01'
072900100120    >C     OPZ002        ANDNE     '03'
073000100120     C                   SETON                                        5499
073100100120     C                   END
073200100120     C                   ENDIF
073300100120     C*
0734001001201-   C                   END
073500100120     C*
073600100120     C     *IN99         IFEQ      *ON
073700100120     C     $GEST         ANDEQ     'D1'
073800100120     C     OPZ002        ANDNE     '07'
073900100120     C                   MOVEA     *IN           WIN
074000100120     C                   MOVE      *ALL'0'       IN5098           49
074100100120     C                   MOVEA     IN5098        *IN(50)
074200100120     C                   WRITE     D1
074300100120     C                   MOVEA     WIN           *IN
074400100120     C                   ENDIF
074500100120     C*
074600100120     C                   ENDSR
074700100120     C/EJECT
074800100618     C************************************************************
074900100618     C* Controllo codice tabella
075000100618     C************************************************************
075100100618     C     CtrCod        BEGSR
075200100618     C*
075300100618     C     D1COD         IFEQ      *ALL' '
075400100618     C                   SETON                                        5099
075500100618     C                   ENDIF
075600100618     C*
075700100618     C                   Clear                   SDGTABDS
075800100618     C*
075900100618     C                   MOVE      XSCCUT        XTBUTE
076000100618     C                   MOVE      '1'           XTBRIC
076100100618     C                   MOVE      XSCSOC        XTBAZI
076200100618     C                   MOVE      *ZERO         XTBKEY
076300100618     C                   MOVE      'G12'         XTBCOD
076400100618     C                   MOVE      D1Cod         XTBKEY
076500100618     C*
076600100618     C                   CALL      'SDGTABDR'
076700100618     C                   PARM                    SDGTABDS
076800100618     C*
076900100618     C     XTBERR        IFEQ      *ON
077000100618     C                   SETON                                        5199
077100100618     C                   ELSE
077200100618     C                   MOVEL     XTBUNI        ANGG12DS
077300100618     C*
077400170616     C                   if        DescrHD4    = *Blank
077500170616     C                   MOVEL     DesG12        DescrHD4
078200100618     C                   endif
078201170616     C*
078202170616R073 C                   if        CodISOG12  <> *Blanks     and
078203170616     C                             CodISOHD4   = *Blank
078204170616R073 C                   MOVEL     CodISOG12     CodISOHD4
078205170616     C                   endif
078300100618     C*
078400100618     C                   ENDIF
078500100618     C*
078600100618     C                   ENDSR
078700100618     C/EJECT
078800100618     C************************************************************
078900100618     C* Controllo descrizione
079000100618     C************************************************************
079100100618     C     CtrDescr      BEGSR
079200100618     C*
079300170616     C     DESCRHD4      IFEQ      *BLANK
079400100618     C                   SETON                                        5299
079500100618     C                   ENDIF
079600100618     C*
079700170616     C     DESHD4        IFEQ      *BLANK
079800170616     C                   Eval      DesHD4      = DescrHD4
079900100618     C                   ENDIF
080000100618     C*
080100100618     C                   ENDSR
080200100618     C/EJECT
080300100120     C************************************************************
080400170616     C* Controllo codice Stato ISO
080500100120     C************************************************************
080600170616     C     CtrCodISO     BEGSR
080700100618     C*
081005170704     C* deve essere numerico
081006170704     C     CodIsoHD4     IFNE      *Blanks
081007170704     C     CodIsoHD4     ANDLT     '000'
081008170616     C                   SETON                                        5599
081009170616     C                   EndIF
081100100618     C*
081200100618     C                   ENDSR
081201170616     C/EJECT
081202170616     C************************************************************
081203170704     C* Controllo immissione di almeno uno dei due codici ISO
081204170616     C************************************************************
081205170704     C     CtrUnIso      BEGSR
081206170616     C*
081207170704     C* deve essere numerico
081208170704     C     CodIsoHD4     IFEQ      *BLANK
081209170704     C     CodIso2HD4    ANDEQ     *BLANK
081210170704     C                   SETON                                        5399
081211170704     C                   Endif
081214170616     C*
081215170616     C                   ENDSR
081300100618     C/EJECT
081400100618     C************************************************************
081500100618     C* CONTROLLO VIDEATA D2
081600100618     C************************************************************
081700100618     C     CTRD2         BEGSR
081800100618     C*
081900100120     C                   ENDSR
082000100120     C/EJECT
082100100120     C************************************************************
082200100120    >C* AGGIORNAMENTO ANAGRAFICA
082300100120     C************************************************************
082400100120    >C     AGGANA        BEGSR
082500100120     C*
082600170616$007 C                   MOVE      'HD4'         XTBCOD
082700100120     C                   MOVE      *ALL'0'       XTBKEY
082800100120     C                   MOVE      D1COD         XTBKEY
082900100120     C                   MOVE      D1UNITA       XTBUNT
083000100120     C*
083100100120     C                   SELECT
083200100120    >C     OPZ002        WHENEQ    '01'
083300100120    >C     OPZ002        OREQ      '03'
083400100120    >C                   EXSR      RIEANA
083500100120     C                   MOVE      XSCCUT        XTBUTE
083600100120     C                   MOVE      '5'           XTBRIC
083700100120     C                   CALL      'SDGTABDR'
083800100120     C                   PARM                    SDGTABDS
083900100120     C*
084000100120     C* se rcd non scrivibile attivo errore
084100100120     C     XTBERR        IFEQ      '1'
084200100120    >C                   SETON                                        5999
084300100120     C                   END
084400100120    >C     OPZ002        WHENEQ    '02'
084500100120     C                   MOVE      '1'           XTBRIC
084600100120     C                   MOVE      '1'           XTBALC
084700100120     C                   CALL      'SDGTABDR'
084800100120     C                   PARM                    SDGTABDS
084900100120    >C                   EXSR      RIEANA
085000100120     C                   MOVE      XSCCUT        XTBUTE
085100100120     C                   MOVE      '4'           XTBRIC
085200100120     C                   CALL      'SDGTABDR'
085300100120     C                   PARM                    SDGTABDS
085400100120    >C     OPZ002        WHENEQ    '04'
085500100120     C                   MOVE      '1'           XTBRIC
085600100120     C                   MOVE      '1'           XTBALC
085700100120     C                   CALL      'SDGTABDR'
085800100120     C                   PARM                    SDGTABDS
085900100120     C*
086000100120     C                   MOVE      '6'           XTBRIC
086100100120     C                   CALL      'SDGTABDR'
086200100120     C                   PARM                    SDGTABDS
086300100120     C*
086400100120     C* annullo traduzioni
086500100120     C     XTBERR        IFEQ      '0'
086600100120     C                   EXSR      ANNTBT
086700100120     C                   END
086800100120     C*
086900100120     C                   ENDSL
087000100120     C*
087100100120     C                   ENDSR
087200100120     C/EJECT
087300100120     C************************************************************
087400100120     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
087500100120     C************************************************************
087600100120     C     RIEANA        BEGSR
087700100120     C                   CLEAR                   XTBUNI
087800100120     C                   MOVE      WRKDSC        XTBDSC
087900100120     C                   MOVE      D1RIS         XTBRIS
088000100120$026 C
088100100120     C                   MOVEL     EXTDS         XTBUNI
088200100120     C                   ENDSR
088300100120     C/EJECT
088400100120     C************************************************************
088500100120     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
088600100120     C************************************************************
088700100120     C     GESAGG        BEGSR
088800100120     C*
088900100120     C* segnalo al pgm chiamante l'avvenuta conferma
089000100120    >C                   MOVE      *ON           OPR002
089100100120     C* segnalo al pgm chiamante l'aver premuto l'F6
089200100120    >C                   MOVE      '0'           RET002
089300100120     C*
0894001001201    C                   SELECT
089500100120     C* nel caso di immissione, ripristino la videata iniziale
089600100120    >C     OPZ002        WHENEQ    '01'
089700100120     C                   MOVE      *ON           $INZD1
089800100120     C* altrimenti ritorno al pgm chiamante
089900100120     C                   OTHER
090000100120     C                   MOVE      *ON           $FINE
0901001001201-   C                   ENDSL
090200100120     C*
090300100120     C                   ENDSR
090400100120     C/EJECT
090500100120     C************************************************************
090600100120    >C* ANNULLAMENTO ARCHIVIO TRADUZIONI
090700100120     C************************************************************
090800100120    >C     ANNTBT        BEGSR
090900100120     C                   MOVE      XTBAZI        TBTAZI
091000100120     C                   MOVE      XTBCOD        TBTCOD
091100100120     C                   MOVE      XTBKEY        TBTKEY
091200100120     C     KTBT          SETLL     ANTBT01L
091300100120     C                   DO        *HIVAL
091400100120     C     KTBT          DELETE    ANTBT01L                           21
091500100120     C  N21              END
091600100120     C*
091700100120     C                   ENDSR
091800100120     C/EJECT
091900100120     C************************************************************
092000100120     C* OPERAZIONI INIZIALI
092100100120     C************************************************************
092200100120     C     *INZSR        BEGSR
092300100120     C*
092400100120     C* Reperimento parametri
092500100120     C*
092600100120     C     *ENTRY        PLIST
092700100120     C                   PARM                    KPJBA
092800100120     C*
092900100120     C* Reperimento Codice utente
093000100120     C*
093100100120     C*
093200100120     C* reperisco le dimensioni delle variabili che conterranno i tasti
093300100120     C                   EVAL      $D1L01 = %Size(Z1Ke1)
093400100120     C                   EVAL      $D1L02 = %Size(Z1Ke2)
093500100120     C*
093600100120     C* Reperimento tasti di funzione
093700100120     C*
093800100120     C                   EXSR      REPKEY
093900100120     C*
094000100120     C* KLIST
094100100120     C*
094200100120     C     KTBT          KLIST
094300100120     C                   KFLD                    TBTAZI
094400100120     C                   KFLD                    TBTCOD
094500100120     C                   KFLD                    TBTKEY
094600100120     C*
094700100120     C* reperisco le dimensioni della variabile del titolo
094800100120     C                   EVAL      LENTIT = %Size(T1Tit)
094900100120     C*
095000100120     C                   ENDSR
095100100120     C/EJECT
095200100120      ************************************************************
095300100120      * Reperimento dati società
095400100120      ************************************************************
095500100120     C     REPSOC        BEGSR
095600100120      *
095700100120     C                   CALL      'SDGSOCDR'
095800100120     C                   PARM                    TIPXSC            6
095900100120     C                   PARM                    SOCXSC            3
096000100120     C                   PARM                    CDSXSC            9 0
096100100120     C                   PARM                    MODXSC            3
096200100120     C                   PARM      *BLANKS       RTNXSC            1
096300100120     C                   PARM                    XSOCDS
096400100120     C                   PARM                    KPJBA
096500100120      *
096600100120     C                   ENDSR
096700100120     C/EJECT
096800100120     C************************************************************
096900100120     C* REPERIMENTO TASTI DI FUNZIONE
097000100120     C************************************************************
097100100120     C     REPKEY        BEGSR
097200100120     C                   MOVEA     $FM1          $FM
097300100120     C                   EXSR      REPKKK
097400100120     C                   MOVEA     $FX           $FX1
097500100120     C                   ENDSR
097600100120     C/EJECT
097700100120     C************************************************************
097800100120     C* REPERIMENTO TASTI DI FUNZIONE
097900100120     C************************************************************
098000100120     C     REPKKK        BEGSR
098100100120     C*
098200100120     C                   CLEAR                   $FX
098300100120     C     1             DO        24            X
098400100120     C*
098500100120     C     $FM(X)        IFNE      *BLANK
098600100120     C*
098700100120     C                   MOVEL     $FM(X)        IDMSG
098800100120     C                   CALLB     'XRTVM'
098900100120     C                   PARM                    IDMSG            27
099000100120     C                   PARM                    TXTMSG          100
099100100120     C                   PARM                    ERRMSG            1
099200100120     C*
099300100120     C     ERRMSG        IFNE      *ON
099400100120     C                   MOVEL     TXTMSG        $FX(X)
099500100120     C                   ELSE
099600100120     C                   MOVEL     *ALL' '       $FX(X)
099700100120     C                   END
099800100120     C*
099900100120     C                   END
100000100120     C*
100100100120     C                   END
100200100120     C*
100300100120     C                   ENDSR
100400100120     C/EJECT
100500100120     C************************************************************
100600100120     C* INIZIALIZZAZIONE VARIABILI
100700100120     C************************************************************
100800100120     C     INZVAR        BEGSR
100900100120     C*
101000100120     C                   MOVEL     'SOC001'      TIPXSC
101100100120     C                   EXSR      REPSOC
101200100120     C     RTNXSC        IFNE      '1'
101300100120     C                   MOVEL     XSOCDS        SOC001
101400100120     C                   ELSE
101500100120     C                   MOVE      *ON           $FINE
101600100120     C* fine programma
101700100120     C                   EXSR      ENDPGM
101800100120     C                   ENDIF
101900100120     C*
102000100120     C* Dati da PGM guida
102100100120     C*
102200100120    >C                   MOVEL     KPJBU         D002DS
102300100120     C*
102400100120     C* Variabili per gestione videate
102500100120     C*
102600100120     C                   MOVE      'D1'          $GEST
102700100120     C                   MOVE      *OFF          $FINE
102800100120     C                   MOVE      *ON           $INZD1
102900100120     C                   MOVE      *BLANK        $LASTG
103000100120     C                   MOVE      *BLANK        $LASTV
103100100120     C                   Z-ADD     0             $ULKD1            3 0
103200100120     C*
103300100120     C* Variabili appoggio
103400100120     C*
103500100120     C                   Z-ADD     0             CURR
103600100120     C                   Z-ADD     0             CURC
103700100120     C*
103800100120     C* Valorizzazione campi univoci testate
103900100120     C*
104000100120     C                   CLEAR                   T1
104100100120     C                   MOVEL     KNSIF         NOMSIF
104200100120     C                   MOVEL     XSCDSI        NOMDIT
104300100120     C                   MOVEL     KCDAZ         NOMAZN
104400100120     C                   Z-ADD     1             X
104500100120    >C     OPZ002        LOOKUP    $OZ(X)                                 21
104600100120     C     *IN21         IFEQ      *ON
104700100120     C                   MOVEL     $ID(X)        IDMSG
104800100120     C                   CALLB     'XRTVM'
104900100120     C                   PARM                    IDMSG            27
105000100120     C                   PARM                    TXTMSG          100
105100100120     C                   PARM                    ERRMSG            1
105200100120     C                   PARM      *Off          CTRMSG            1
105300100120     C                   PARM      30            LENMSG            3 0
105400100120     C     ERRMSG        IFNE      *ON
105500100120     C                   MOVEL     TXTMSG        T1OPE
105600100120     C                   ELSE
105700100120     C                   MOVEL     *ALL'?'       T1OPE
105800100120     C                   ENDIF
105900100120     C                   END
106000100120     C*
106100100120     C* Inizializzazione/Decodifica prima videata
106200100120     C*
106300100120     C                   EXSR      INZD1
106400100120     C                   MOVE      *OFF          $INZD1
106500100120     C* Se opzione = '07' richiesti solo controlli
106600100120     C     OPZ002        IFEQ      '07'
106700100120     C                   MOVEL     *IN99         ERR002
106800100120     C                   MOVEL     '0'           RET002
106900100120     C                   MOVEL     *ON           $FINE
107000100120     C                   ENDIF
107100100120     c*
107200100120     c* a seconda della lingua richiesta o meno, abilito o disablito F7
107300100120$029 C                   eval      $fc1(7)=*on
107400100120     C*
107500100120     C                   ENDSR
107600100120     C************************************************************
107700100120     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
107800100120     C************************************************************
107900100120     C     DEFCAM        BEGSR
108000100120     C*
108100100120     C* klist
108200100120     C*
108300100120     C*
108400100120     C* Variabili per gestione videate
108500100120     C*
108600100120     C                   MOVE      *BLANK        $GEST             2
108700100120     C                   MOVE      *BLANK        $FINE             1
108800100120     C                   MOVE      *BLANK        $INZD1            1
108900100120     C                   MOVE      *BLANK        $LASTG            2
109000100120     C                   MOVE      *BLANK        $LASTV            2
109100100120     C*
109200100120     C* Variabili appoggio
109300100120     C*
109400100120     C                   Z-ADD     0             CURR              2 0
109500100120     C                   Z-ADD     0             CURC              2 0
109600100120     C                   MOVE      *ZEROS        WIN              99
109700100120     C                   MOVE      *BLANK        WTBERR            1
109800100120     C*
109900100120     C* Indici
110000100120     C*
110100100120     C                   Z-ADD     0             X                 3 0
110200100120     C                   Z-ADD     0             Y                 3 0
110300100120     C*
110400100120     C                   ENDSR
110500100120     C************************************************************
110600100120     C/EJECT
110700100120     C************************************************************
110800100120     C* CONTROLLO ABILITAZIONE TASTI
110900100120     C************************************************************
111000100120     C     CTRKEY        BEGSR
111100100120     C                   SELECT
111200100120     C     $TASTO        WHENEQ    F01
111300100120     C     $FC(1)        IFEQ      '0'
111400100120     C                   SETON                                        99
111500100120     C                   ENDIF
111600100120     C     $TASTO        WHENEQ    F02
111700100120     C     $FC(2)        IFEQ      '0'
111800100120     C                   SETON                                        99
111900100120     C                   ENDIF
112000100120     C     $TASTO        WHENEQ    F03
112100100120     C     $FC(3)        IFEQ      '0'
112200100120     C                   SETON                                        99
112300100120     C                   ENDIF
112400100120     C     $TASTO        WHENEQ    F04
112500100120     C     $FC(4)        IFEQ      '0'
112600100120     C                   SETON                                        99
112700100120     C                   ENDIF
112800100120     C     $TASTO        WHENEQ    F05
112900100120     C     $FC(5)        IFEQ      '0'
113000100120     C                   SETON                                        99
113100100120     C                   ENDIF
113200100120     C     $TASTO        WHENEQ    F06
113300100120     C     $FC(6)        IFEQ      '0'
113400100120     C                   SETON                                        99
113500100120     C                   ENDIF
113600100120     C     $TASTO        WHENEQ    F07
113700100120     C     $FC(7)        IFEQ      '0'
113800100120     C                   SETON                                        99
113900100120     C                   ENDIF
114000100120     C     $TASTO        WHENEQ    F08
114100100120     C     $FC(8)        IFEQ      '0'
114200100120     C                   SETON                                        99
114300100120     C                   ENDIF
114400100120     C     $TASTO        WHENEQ    F09
114500100120     C     $FC(09)       IFEQ      '0'
114600100120     C                   SETON                                        99
114700100120     C                   ENDIF
114800100120     C     $TASTO        WHENEQ    F10
114900100120     C     $FC(10)       IFEQ      '0'
115000100120     C                   SETON                                        99
115100100120     C                   ENDIF
115200100120     C     $TASTO        WHENEQ    F11
115300100120     C     $FC(11)       IFEQ      '0'
115400100120     C                   SETON                                        99
115500100120     C                   ENDIF
115600100120     C     $TASTO        WHENEQ    F12
115700100120     C     $FC(12)       IFEQ      '0'
115800100120     C                   SETON                                        99
115900100120     C                   ENDIF
116000100120     C     $TASTO        WHENEQ    F13
116100100120     C     $FC(13)       IFEQ      '0'
116200100120     C                   SETON                                        99
116300100120     C                   ENDIF
116400100120     C     $TASTO        WHENEQ    F14
116500100120     C     $FC(14)       IFEQ      '0'
116600100120     C                   SETON                                        99
116700100120     C                   ENDIF
116800100120     C     $TASTO        WHENEQ    F15
116900100120     C     $FC(15)       IFEQ      '0'
117000100120     C                   SETON                                        99
117100100120     C                   ENDIF
117200100120     C     $TASTO        WHENEQ    F16
117300100120     C     $FC(16)       IFEQ      '0'
117400100120     C                   SETON                                        99
117500100120     C                   ENDIF
117600100120     C     $TASTO        WHENEQ    F17
117700100120     C     $FC(17)       IFEQ      '0'
117800100120     C                   SETON                                        99
117900100120     C                   ENDIF
118000100120     C     $TASTO        WHENEQ    F18
118100100120     C     $FC(18)       IFEQ      '0'
118200100120     C                   SETON                                        99
118300100120     C                   ENDIF
118400100120     C     $TASTO        WHENEQ    F19
118500100120     C     $FC(19)       IFEQ      '0'
118600100120     C                   SETON                                        99
118700100120     C                   ENDIF
118800100120     C     $TASTO        WHENEQ    F20
118900100120     C     $FC(20)       IFEQ      '0'
119000100120     C                   SETON                                        99
119100100120     C                   ENDIF
119200100120     C     $TASTO        WHENEQ    F21
119300100120     C     $FC(21)       IFEQ      '0'
119400100120     C                   SETON                                        99
119500100120     C                   ENDIF
119600100120     C     $TASTO        WHENEQ    F22
119700100120     C     $FC(22)       IFEQ      '0'
119800100120     C                   SETON                                        99
119900100120     C                   ENDIF
120000100120     C     $TASTO        WHENEQ    F23
120100100120     C     $FC(23)       IFEQ      '0'
120200100120     C                   SETON                                        99
120300100120     C                   ENDIF
120400100120     C     $TASTO        WHENEQ    F24
120500100120     C     $FC(24)       IFEQ      '0'
120600100120     C                   SETON                                        99
120700100120     C                   ENDIF
120800100120     C                   ENDSL
120900100120     C                   ENDSR
121000100120     C/EJECT
121100100120     C************************************************************
121200100120     C* RIEMPIMENTO TASTI FUNZIONE
121300100120     C************************************************************
121400100120     C     RIEKEY        BEGSR
121500100120     C*
121600100120     C* Input :
121700100120     C* - $FX = schiera con tutti i testi disponibili
121800100120     C* - $FC = schiere con i flag di validità associati ai testi
121900100120     C* - $FR = schiere con i flag di REVERSE IMAGE
122000100120     C* - $ULK = ultimo testo caricato
122100100120     C* Output :
122200100120     C* - $ULK
122300100120     c* - $KEY1= 1° riga di tasti funzione
122400100120     c* - $KEY2= 2° riga di tasti funzione
122500100120     C*
122600100120     C                   CALLB     'XKEYMSG'
122700100120     C* input
122800100120     C                   PARM                    $FX
122900100120     C                   PARM                    $FC
123000100120     C                   PARM                    $FR
123100100120     C                   PARM                    $ULK              3 0
123200100120     C* output
123300100120     C                   PARM                    $KEY1            79
123400100120     C                   PARM                    $KEY2            79
123500100120     C* input
123600100120     C                   PARM                    $L01              3 0
123700100120     C                   PARM                    $L02              3 0
123800100120     C*
123900100120     C                   PARM                    $ALLFUNCT       480
124000100120     C*
124100100120     C                   ENDSR
124200100120     C/EJECT
124300100120**
12440010012001SDGMSG    *LIBL     SDC0002    inserimento
12450010012002PROMSG    *LIBL     COS0002    modifica
12460010012003PROMSG    *LIBL     COS0003    copia
12470010012004PROMSG    *LIBL     COS0004    annullamento
12480010012005PROMSG    *LIBL     COS0005    visualizzazione
124900100120** TASTI DI FUNZIONE  D1
125000100120PROMSG    *LIBL     KEY0001  01
125100100120PROMSG    *LIBL     KEY0002  02
125200100120PROMSG    *LIBL     KEY0003  03
125300100120PROMSG    *LIBL     KEY0004  04
125400100120PROMSG    *LIBL     KEY0005  05
125500100120PROMSG    *LIBL     KEY0006  06
125600100120PROMSG    *LIBL     KEY0007  07
125700100120PROMSG    *LIBL     KEY0008  08
125800100120PROMSG    *LIBL     KEY0009  09
125900100120PROMSG    *LIBL     KEY0010  10
126000100120PROMSG    *LIBL     KEY0011  11
126100100120PROMSG    *LIBL     KEY0012  12
126200100120PROMSG    *LIBL     KEY0013  13
126300100120PROMSG    *LIBL     KEY0014  14
126400100120PROMSG    *LIBL     KEY0015  15
126500100120PROMSG    *LIBL     KEY0016  16
126600100120SDGMSG    *LIBL     SDK0001  17
126700100120PROMSG    *LIBL     KEY0018  18
126800100120PROMSG    *LIBL     KEY0019  19
126900100120PROMSG    *LIBL     KEY0020  20
127000100120PROMSG    *LIBL     KEY0021  21
127100100120PROMSG    *LIBL     KEY0022  22
127200100120PROMSG    *LIBL     KEY0023  23
127300100120PROMSG    *LIBL     KEY0024  24
127400100120**  ABILITAZIONE D1
1275001001200N  01
1276001001200N  02
1277001001201N  03
1278001001201N  04
1279001001201N  05
1280001001201N  06
1281001001201N  07
1282001001201N  08
1283001001200N  09
1284001001200N  10
1285001001200N  11
1286001001201N  12
1287001001200N  13
1288001001200N  14
1289001001200N  15
1290001001200N  16
1291001001200N  17
1292001001200N  18
1293001001200N  19
1294001001200N  20
1295001001200N  21
1296001001200N  22
1297001001200N  23
1298001001200N  24
129900100120**
130000100120DESCR Descrizione ................
