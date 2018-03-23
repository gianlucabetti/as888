000100100120     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200100120     H*PARMS CVTOPT(*DATETIME)
000300100120     H DECEDIT('0,') DATEDIT(*DMY.)
000400100120      *
000500100120      *                  ---------------------------
000600160805      *                     GESTIONE TABELLA  HK1
000700100120      *                  ---------------------------
000701161020      *
000702161020R235  * Pgm creato con il task R235 CU Percipiente in regime agevolato
000703161020R235  *
000900100120     F*----------------------------------------------------*
001000100120     FANTBT01L  UF   E           K DISK
001100100120     F*---
001200160805     FSDGHK1D2  CF   E             WORKSTN
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
003300160805$003 D EXTDS         E DS                  EXTNAME(SDGHK1DS) INZ
003400100120     D* Passaggio Parametri
003500100120     D KPJBA         E DS
003600100618     D*-------------
003700100618     D SDGTABDS      E DS                  INZ
003800100120     D*-------------
003900100120      * Società
004000100120     D SOC001        E DS                  EXTNAME(SDGSOCDS)
004100100120     D*-------------
004200100120      * DS Interna per dati utente
004300100120     D XSOCDS          DS          1000
004400100618     D*-------------
004500100618     D* Ricerca legende
004600100618     D ANA964DS      E DS                  INZ
004700100618     D*---------------
004800100618     D X07VALDS      E DS
004900100618     D*-------------
005000100618     D* Ricerca causale
005100100618     D ANA070DS      E DS                  INZ
005200100618     D  RET070       E                     INZ('0')
005300100618     D  OPR070       E                     INZ('0')
005400100618     D  CMT070       E                     INZ('0')
005500100618     D  ERR070       E                     INZ('0')
005600100618     D  FSC070       E                     INZ('S')
005700100120     D*-------------
005800100120     D* DS Esterna autorizzazioni tabelle
005900100120     D XCHATB        E DS                  EXTNAME(XCHKATBDS)
006000100120     D*
006100100120     D* DS  restituzione autorizzazioni
006200100120     D* IND=indice
006300100120     D* GRU=man. di gruppo
006400100120     D* AZI=man. azienda
006500100120     D* ABI=abilit. ai riservati
006600100120     D AUTDS           DS
006700100120     D  MANIND                 1      1
006800100120     D  MANGRU                 2      2
006900100120     D  MANAZI                 3      3
007000100120     D  ABIRIS                 4      4
007100100120     D*-------------
007200100120     D* opzione richiesta
007300100120    >D D002DS          DS
007400100120     D  OPZ002                 1      2
007500100120     D* 0 = ritorno normale
007600100120     D* 1 = F3
007700100120     D* 2 = F12
007800100120     D  RET002                 3      3
007900100120     D* operazione eseguite dal pgm chiamato :
008000100120     D* 0 = nessuna operazione
008100100120     D* 1 = eseguito aggiornamento
008200100120     D  OPR002                 4      4
008300100120     D* funzione non eseguibile per errore :
008400100120     D* 0 = nessun errore
008500100120     D* 1 = funzione richiamata chiusa in errore
008600100120     D* Aggiungere eventuali altri codici di errore
008700100120     D  ERR002                 5      5
008800100120     D* Azienda
008900100120     D  AZI002                 6      8
009000100120     D* Lingua
009100100120     D  LIN002                 9      9
009200100120     D* Codice scelto
009300100120     D  KEY002                10     24
009400100120     D* Autorizzazione
009500100120     D  AUT002                25     25
009600100120     D* Unita
009700100120     D  UNI002                26     33
009800100618     D*-------------
009900100618     D UDateISO        S               D
010000100120     D*-------------
010100100120     D DSFMT           DS
010200100120     D  $TASTO               369    369
010300100120     D  NRG                  370    370
010400100120     D  NCL                  371    371
010500100120     D*-------------
010600100120     D* posizione cursore
010700100120     D CURSOR          DS
010800100120     D  RIRI                   1      2B 0 INZ
010900100120     D  R$$                    2      2
011000100120     D  COCO                   3      4B 0 INZ
011100100120     D  C$$                    4      4
011200100120     D*-------------
011300100120     D* Reperimento nome PGM
011400100120     D STATUS         SDS           333
011500100120     D  DSPGM            *PROC
011600100120     D*-------------
011700100120     D* Reperimento titolo tabella
011800100120     D  WRKTIT         S             35A
011900100120     D  WRKTBCOD       S                   LIKE(XTBCOD)
012000100120     D  LENTIT         S              3  0
012100100120     D  I              S              3  0
012200100618     D*-------------
012300100618     D* DS Interna per scomposizione campo chiave
012400100709     D D1Cod           DS             4
012500100709     D  D1Cau                  1      4
012600100120     D*-------------
012700100120     D* COSTANTI
012800100120     D*-------------
012900100120     D*
013000100120     D* Tasti di funzione
013100100120     D F01             C                   CONST(X'31')
013200100120     D F02             C                   CONST(X'32')
013300100120     D F03             C                   CONST(X'33')
013400100120     D F04             C                   CONST(X'34')
013500100120     D F05             C                   CONST(X'35')
013600100120     D F06             C                   CONST(X'36')
013700100120     D F07             C                   CONST(X'37')
013800100120     D F08             C                   CONST(X'38')
013900100120     D F09             C                   CONST(X'39')
014000100120     D F10             C                   CONST(X'3A')
014100100120     D F11             C                   CONST(X'3B')
014200100120     D F12             C                   CONST(X'3C')
014300100120     D F13             C                   CONST(X'B1')
014400100120     D F14             C                   CONST(X'B2')
014500100120     D F15             C                   CONST(X'B3')
014600100120     D F16             C                   CONST(X'B4')
014700100120     D F17             C                   CONST(X'B5')
014800100120     D F18             C                   CONST(X'B6')
014900100120     D F19             C                   CONST(X'B7')
015000100120     D F20             C                   CONST(X'B8')
015100100120     D F21             C                   CONST(X'B9')
015200100120     D F22             C                   CONST(X'BA')
015300100120     D F23             C                   CONST(X'BB')
015400100120     D F24             C                   CONST(X'BC')
015500100120     D ENTER           C                   CONST(X'F1')
015600100120     D ROLDWN          C                   CONST(X'F4')
015700100120     D ROLLUP          C                   CONST(X'F5')
015800100120     D*-------------
015900100120     D*
016000100120     D DATA            C                   CONST('0001-01-01')
016100100618     D*-------------
016200100618     D* Controllo causale operativa
016300100618     D ANOPE         E DS                  EXTNAME(ANOPE00F) INZ
016400100618     D*-------------
016500100618     D* Definizioni parametri standard chiamate moduli
016600100618     D $Esito          s              1
016700100618     D $GesMsg         s              1
016800100618     D $LenOut         s              9B 0
016900100618     D $Struttura      s             10
017000100618     D $Refresh        S              1
017100100618     D $Uso            S              1
017200100120     I*----------------------------------------------------*
017300100120     ID1
017400160805$005 I              D1DESCR                     DESCRHK1
017500160805$005 I              D1DES                       DESHK1
017700100120     I*
017800100120     C*----------------------------------------------------*
017900100120     C*                MAIN LINE PROGRAM
018000100120     C*----------------------------------------------------*
018100100120     C*
018200100120     C* inizializzazione variabili
018300100120     C                   EXSR      INZVAR
018400100120     C*
018500100120     C     $FINE         DOWEQ     *OFF
018600100120     C     $GEST         CASEQ     'D1'          GESD1
018700100120     C                   END
018800100120     C                   END
018900100120     C*
019000100120     C* fine programma
019100100120     C                   EXSR      ENDPGM
019200100120     C/EJECT
019300100120     C************************************************************
019400100120     C* RICERCHE
019500100120     C************************************************************
019600100120     C     ENDPGM        BEGSR
019700100120     C*
019800100120     C* disallocazione rcd nel caso di
019900160805$007 C                   MOVE      'HK1'         XTBCOD
020000100120    >C                   MOVE      '7'           XTBRIC
020100100120     C*
020200100120     C                   CALL      'SDGTABDR'
020300100120     C                   PARM                    SDGTABDS
020400100120     C*
020500100120     C* passaggio dati al pgm chiamante
020600100120     C                   MOVE      *BLANKS       KPJBU
020700100120    >C                   MOVEL     D002DS        KPJBU
020800100120     C*
020900100120     C                   SETON                                            RT
021000100120     C*
021100100120     C                   ENDSR
021200100120     C/EJECT
021300100120     C************************************************************
021400100120     C* RICERCHE
021500100120     C************************************************************
021600100709     C     SEARCH        BEGSR
021700100120     C*
021800100120     C* determino Riga/Colonna del cursore
021900100120     C                   MOVE      NRG           R$$
022000100120     C                   MOVE      NCL           C$$
022100100120     C                   Z-ADD     RIRI          CURR
022200100120     C                   Z-ADD     COCO          CURC
022300100120     C*
022400100120     C* I campi che al momento dell'F4 risultano protetti non devono
022500100120     C* essere abilitati alla funzione di ricerca
022600100120     C*
022700100618     C                   SELECT
025300100618     C*
025400100618     C*
025500100618     C* FMTD1 - Causale di contabilità
025600100618     C     H1NMFL        WHENEQ    'D1CAU'
025700100618     C                   RESET                   ANA070DS
025800100618     C*
025900100618     C                   Select
026000100618     C     OPZ002        WHENEQ    '01'
026100100618     C     OPZ002        OREQ      '03'
026200100618     C                   MOVE      '01'          Opz070
026300100618     C*
026400100618     C                   OTHER
026500100618     C                   MOVE      '05'          Opz070
026600100618     C                   endSL
026700100618     C*
026800160805     C                   MOVE      '0'           Tpc070
026900100618     C                   CLEAR                   KPJBU
027000100618     C                   MOVEL     ANA070DS      KPJBU
027100100618     C                   CALL      'ANA070R'
027200100618     C                   PARM                    KPJBA
027300100618     C                   MOVEL     KPJBU         ANA070DS
027400100618     C     Cau070        IFNE      *BLANK
027500100618     C                   MOVEL     Cau070        D1Cau
027600100618     C                   ENDIF
027700100120     C*
027800100120     C* ricerca non abilitata in questa posizione
0279001001201O   C                   OTHER
028000100120     C                   SETON                                        9899
028100100120     C* imposto pos. del cursore
028200100120     C                   Z-ADD     CURR          H1RIGA
028300100120     C                   Z-ADD     CURC          H1COLO
028400100120     C*
0285001001201-   C                   ENDSL
028600100120     C*
028700100120     C                   MOVE      *HIVAL        $LASTV
028800100120     C                   ENDSR
028900100120     C/EJECT
029000100120     C************************************************************
029100100120     C* TRADUZIONI
029200100120     C************************************************************
029300100120     C     LINGUA        BEGSR
029400100120     C*
029500100120     C* determino Riga/Colonna del cursore
029600100120     C                   MOVE      NRG           R$$
029700100120     C                   MOVE      NCL           C$$
029800100120     C                   Z-ADD     RIRI          CURR
029900100120     C                   Z-ADD     COCO          CURC
030000100120     C*
030100100120     C*
030200100120     C*
030300100120     C* FMTD1  - CAMPI DA TRADURRE
030400100120     C* imposto pos. del cursore
030500100120     C                   Z-ADD     CURR          H1RIGA
030600100120     C                   Z-ADD     CURC          H1COLO
030700100120     x****               MOVEL     H1NMFL        W005              5
030800100120     x****               MOVE      W005          W003              3
030900100120     x****               MOVEL     W003          W006              6
031000100120     C                   MOVEL     H1NMFL        W008              8
031100100120     C                   MOVE      W008          W003              6
031200100120     C                   MOVE      W008          W006              6
031300100120     C                   MOVE      *ZERO         KEY              15
031400100120     C                   MOVE      D1COD         KEY
031500100120     C                   SELECT
031600100120$014 C     W003          WHENEQ    'DESCR '
031700100120$014 C                   Z-ADD     1             I                 3 0
031800100120$014 C     W006          LOOKUP    LIN(I)                                 21
031900100120$014 C                   CALL      'ANA101R '
032000100120$014 C                   PARM      W003          CAMPO            10
032100160805$014 C                   PARM      'HK1'         COD               3
032200100120$014 C                   PARM                    KEY              15
032300100120$014 C                   PARM      020           LUN               3 0
032400100120$014 C                   PARM      001           POS               3 0
032500160805$014 C     DESCRHK1      PARM      DESCRHK1      DES              50
032600100120$014 C                   PARM      LIND(I)       FLD              50
032700100120$014 C                   PARM      XTBAZI        AZI               3
032800100120$014 C                   PARM      LIN002        LNG               1
032900100120$014 C                   PARM      OPZ002        OPZ               2
033000100120     C*
033100100120     C*
033200100120     C* ricerca non abilitata in questa posizione
0333001001201O   C                   OTHER
033400100120     C                   SETON                                        98
033500100120     C*
0336001001201-   C                   ENDSL
033700100120     C*
033800100120     C                   MOVE      *HIVAL        $LASTV
033900100120     C                   ENDSR
034000100120     C/EJECT
034100100120     C************************************************************
034200100120     C* GESTIONE EMISSIONE TASTI FUNZIONE
034300100120     C************************************************************
034400100120     C     GESPIE        BEGSR
034500100120     C*
034600100120     C                   SELECT
034700100120     C*
034800100120     C     $GEST         WHENEQ    'D1'
034900100120     C     $ULKD1        IFEQ      0
035000100120     C* riempimento opzioni
035100100120     C                   EXSR      F24D1
035200100120     C                   ELSE
035300100120     C                   WRITE     Z1
035400100120     C                   ENDIF
035500100120     C*
035600100120     C*
035700100120     C                   ENDSL
035800100120     C*
035900100120     C                   ENDSR
036000100120     C/EJECT
036100100120     C************************************************************
036200100120     C* GESTIONE VIDEO RECORD D1
036300100120     C************************************************************
036400100120     C     GESD1         BEGSR
036500100120     C* inizializzazione videata
036600100120     C     $INZD1        IFEQ      *ON
036700100120     C                   EXSR      INZD1
036800100120     C                   MOVE      *OFF          $INZD1
036900100120     C                   END
037000100120     C* emissione piede videata se proveniente da altra
037100100120     C* salvataggio valore formato di provenienza
037200100120     C     $LASTV        IFNE      'D1'
037300100120     C                   WRITE     T1
037400100120     C                   EXSR      GESPIE
037500100120     C                   MOVE      $LASTV        $LASTG
037600100120     C                   MOVE      'D1'          $LASTV
037700100120     C                   END
037800100120     C*
037900100120     C                   SELECT
038000100120     C* Annullamento
038100100120    >C     OPZ002        WHENEQ    '04'
038200100120     C* Visualizzazione
038300100120    >C     OPZ002        OREQ      '05'
038400100120     C                   SETON                                        0203
038500100120     C* Manutenzione
038600100120    >C     OPZ002        WHENEQ    '02'
038700100120     C                   SETON                                        02
038800100120     C                   SETOFF                                       03
038900100120     C*
039000100120     C                   OTHER
039100100120     C*
039200100120     C                   SETOFF                                       0203
039300100120     C                   ENDSL
039400100120     C*
039500100120     C* Immissione/Modifica/Copia
039600100120     C                   if        *In99 = *On
039700100120     C                   SETOFF                                       99
039800100120     C                   WRITE     D1
039900100120     C                   SETON                                        99
040000100120     C                   endif
040100100120     C* Immissione/Modifica/Copia
040200100120     C                   EXFMT     D1
040300100120     C* controllo abilitazione tasti funzione
040400100120     C*
040500100120     C                   EXSR      CTRKD1
040600100120     C*
040700100120     C     *IN99         IFEQ      *OFF
040800100120     C*
0409001001201    C                   SELECT
041000100120     c* ROLLUP
041100100120     C* F3=Fine
041200100120     C     $TASTO        WHENEQ    F03
041300100120     C                   EXSR      F03D1
041400100120     C* F4=Ricerca
041500100120     C     $TASTO        WHENEQ    F04
041600100120     C                   EXSR      SEARCH
041700100120     C* F5=Refresh
041800100120     C     $TASTO        WHENEQ    F05
041900100120     C                   EXSR      F05D1
042000100120     C* F8=Successivo
042100100120     C     $TASTO        WHENEQ    F08
042200100120     C                   EXSR      F08D1
042300100120     C* F12=Ritorno
042400100120     C     $TASTO        WHENEQ    F12
042500100120     C                   EXSR      F12D1
042600100120     C* F24=Altri tasti
042700100120     C     $TASTO        WHENEQ    F24
042800100120     C                   EXSR      F24D1
042900100120     C*
0430001001201O   C                   OTHER
043100100120     C                   EXSR      CTRD1
043200100120     C     *IN99         IFEQ      *OFF
043300100120     C                   SELECT
043400100120     C* F7=Traduzione
043500100120     C     $TASTO        WHENEQ    F07
043600100120     C                   EXSR      LINGUA
043700100120     C* F6
0438001001202    C     $TASTO        WHENEQ    F06
043900100120     C* eseguo aggiornamento
044000100120    >C                   EXSR      AGGANA
044100100120     C* eseguo operazioni del dopo-aggiornamento
0442001001203    C     *IN99         IFEQ      *OFF
044300100120     C                   EXSR      GESAGG
0444001001203-   C                   ENDIF
0445001001202-   C                   ENDSL
044600100120     C                   ENDIF
044700100120     C*
0448001001201-   C                   ENDSL
044900100120     C*
0450001001202-   C                   ENDIF
045100100120     C                   ENDSR
045200100120     C/EJECT
045300100120     C************************************************************
045400100120     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
045500100120     C************************************************************
045600100120     C     CTRKD1        BEGSR
045700100120     C                   SETOFF                                       99
045800100120     C                   MOVEA     $FC1          $FC
045900100120     C                   EXSR      CTRKEY
046000100120     C     *IN99         IFEQ      *ON
046100100120     C                   SETON                                        97
046200100120     C                   ENDIF
046300100120     C                   ENDSR
046400100120      ************************************************************
046500100120      * INIZIALIZZAZIONE VIDEATA DATI
046600100120      ************************************************************
046700100120     C     INZD1         BEGSR
046800100120     C*
046900100120     C                   CLEAR                   D1
047000100120     C*
047100100120     C* reperimento titolo
047200100120     C                   clear                   SDGTABDS
047300100120     C                   eval      xtbkey      = *zero
047400100120     C* inserita la seguenta riga
047500160805     C                   move      'HK1'         xtbkey
047600100120     C                   eval      xtbazi      = xscsoc
047700100120     C                   eval      xtblin      = xsclin
047800100120     C                   eval      xtbcod      = 'AAA'
047900100120     C                   eval      xtbric      = '1'
048000100120     C                   eval      xtbalc      = '0'
048100100120     C*
048200100120     C                   CALL      'SDGTABDR'
048300100120     C                   PARM                    SDGTABDS
048400100120     C*
048500100120     C                   if        xtberr <> *off
048600100120     C                   movel     *all'?'       WrkTit
048700100120     C                   else
048800100120     C                   movel     xtbuni        WrkTit
048900100120     C                   move      '     '       WrkTit
049000100120     C                   endif
049100100120     C*
049200100120     C* centratura
049300100120     C                   eval      WrkTit=%trimr(Wrktit)
049400100120     C     ' '           CHECKR    Wrktit        I
049500100120     C                   call      'X71CENTRA'
049600100120     C                   parm                    Wrktit
049700100120     C                   parm                    I
049800100120     C                   parm                    lentit
049900100120     C                   movel     Wrktit        T1tit
050000100120     C*
050100100120     C* Aggiunta riga seguente
050200100120     C
050300100120     C                   MOVE      '0'           D1RIS
050400100120     C                   MOVE      KEY002        D1COD
050500100120     C                   MOVE      AZI002        XTBAZI
050600100120     C                   MOVE      LIN002        XTBLIN
050700100120     C                   MOVE      *ALL'0'       XTBKEY
050800100120     C                   MOVE      KEY002        XTBKEY
050900100120     C                   MOVE      UNI002        XTBUNT
051000160805     C                   MOVE      'HK1'         XTBCOD
051100100120     C*
051200100120     C*
051300100120     C* valorizzo la chiave univoca per reperire i dati
051400100120     C* se non immissione
0515001001201   >C     OPZ002        IFNE      '01'
051600100120    >C*-------------------------------------------------------------
051700100120    >C* impostare i campi chiave ricevuti
051800100120    >C*-------------------------------------------------------------
051900100120     C*
052000100120     C*
052100100120     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
0522001001202   >C     OPZ002        IFEQ      '02'
052300100120    >C     OPZ002        OREQ      '04'
052400100120     C                   MOVE      '1'           XTBALC
052500100120     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
0526001001202E   C                   ELSE
052700100120     C                   MOVE      '0'           XTBALC
0528001001202-   C                   ENDIF
052900100120     C*
053000100120     C                   MOVE      '1'           XTBRIC
053100100120     C*
053200100120     C                   CALL      'SDGTABDR'
053300100120     C                   PARM                    SDGTABDS
053400100120     C*
053500100120     C     XTBERR        IFEQ      '0'
053600100120     C                   MOVEL     XTBUNI        EXTDS
053700100120     c                   move      xtbunt        d1uniTA           8
053800100120     C* Giro la data se non blank
053900100120     c                   move      xtbdsc        xtbdscA          10
054000100120     C                   MOVE      XTBRIS        D1RIS
054100100120     C     XTBDSCa       IFNE      DATA
054200100120     C                   CALLB     'XDT4000'
054300100120     C                   PARM      XTBDSCA       XDTAMG           10
054400100120     C                   PARM      *ZEROS        XDTGMA            6 0
054500100120     C                   PARM      2             XDTSTA            1 0
054600100120     C                   MOVE      XDTGMA        D1DSC
054700100120     C                   ELSE
054800100120     C                   MOVE      *ZEROS        D1DSC
054900100120     C                   ENDIF
055000100120     C                   ENDIF
055100100120     C*
055200100120     C*
055300100120     C* se il rcd da gestire non viene trovato
055400100120     C* oppure è allocato da altro pgm
055500100120     C* ritorno al pgm guida avvertendolo dell'errore
0556001001202    C     XTBERR        IFNE      '0'
055700100120     C                   MOVE      *ON           $FINE
055800100120    >C                   MOVE      '1'           ERR002
055900100120     C*
056000100120     C* altrimenti
0561001001202E   C                   ELSE
056200100120     C* riempio la videata
056300100120     C                   EXSR      RIED1
056400100120     C* richiamo routine dei ctrl per decodificarla
056500100120    >C                   EXSR      CTRD1
056600100120     C                   MOVEL     *IN99         SAV99             1
056700100120     C* Se la gestione è più videate
056800100120    >C                   EXSR      CTRD2
056900100120    >C     *IN99         IFEQ      *ON
057000100120    >C                   MOVEL     *IN99         SAV99             1
057100100120    >C                   ENDIF
057200100120     C                   MOVE      *ALL'0'       IN5098
057300100120     C                   MOVEA     IN5098        *IN(50)
057400100120     C* definizione indicatori su video
057500100120     C                   SETOFF                                       02
0576001001203    C                   SELECT
057700100120    >C     OPZ002        WHENEQ    '02'
057800100120     C                   SETON                                        02
057900100120    >C     OPZ002        WHENEQ    '05'
058000100120     C* disabilita tasti
058100100120     C                   MOVE      '0'           $FC(6)
058200100120     C                   MOVE      '0'           $FC(5)
058300100120     C                   MOVE      '0'           $FC(4)
0584001001203-   C                   ENDSL
058500100120     C*
0586001001202-   C                   ENDIF
058700100120     C*
0588001001201-   C                   ENDIF
058900100120     C*
059000100120     C                   ENDSR
059100100120     C/EJECT
059200100120     C************************************************************
059300100120     C* RIEMPIMENTO VIDEATA  D1
059400100120     C************************************************************
059500100120     C     RIED1         BEGSR
059600100120     C*
059700100120$025 C
059800100120     C*
059900100120     C                   ENDSR
060000100120     C/EJECT
060100100120     C************************************************************
060200100120     C* GESTIONE F03 VIDEO D1
060300100120     C************************************************************
060400100120     C     F03D1         BEGSR
060500100120     C*
060600100120     C                   MOVE      *ON           $FINE
060700100120    >C                   MOVE      '1'           RET002
060800100120     C* fine programma
060900100120     C                   EXSR      ENDPGM
061000100120     C*
061100100120     C                   ENDSR
061200100120     C/EJECT
061300100120     C************************************************************
061400100120     C* GESTIONE F05 VIDEO D1
061500100120     C************************************************************
061600100120     C     F05D1         BEGSR
061700100120     C*
061800100120     C                   MOVE      *ON           $INZD1
061900100120     C*
062000100120     C                   ENDSR
062100100120     C/EJECT
062200100120     C************************************************************
062300100120     C* GESTIONE F08 VIDEO D1
062400100120     C************************************************************
062500100120     C     F08D1         BEGSR
062600100120     C*
062700100120     C                   MOVE      *ON           $FINE
062800100120    >C                   MOVE      '0'           RET002
062900100120     C*
063000100120     C                   ENDSR
063100100120     C/EJECT
063200100120     C************************************************************
063300100120     C* GESTIONE F12 VIDEO D1
063400100120     C************************************************************
063500100120     C     F12D1         BEGSR
063600100120     C*
063700100120     C                   MOVE      *ON           $FINE
063800100120    >C                   MOVE      '2'           RET002
063900100120     C*
064000100120     C                   ENDSR
064100100120     C/EJECT
064200100120     C************************************************************
064300100120     C* GESTIONE F24 VIDEO D1
064400100120     C************************************************************
064500100120     C     F24D1         BEGSR
064600100120     C*
064700100120     C*
064800100120     C                   MOVEA     $FX1          $FX
064900100120     C                   MOVEA     $FC1          $FC
065000100120     C                   MOVEA     $FR1          $FR
065100100120     C                   Z-ADD     $ULKD1        $ULK
065200100120     C                   Z-ADD     $D1L01        $L01
065300100120     C                   Z-ADD     $D1L02        $L02
065400100120     C                   EXSR      RIEKEY
065500100120     C                   MOVEA     $FC           $FC1
065600100120     C                   Z-ADD     $ULK          $ULKD1            3 0
065700100120     C                   MOVEL     $KEY1         Z1KE1
065800100120     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
065900100120     C                   MOVEL     $KEY2         Z1KE2
066000100120     C                   WRITE     Z1
066100100120     C*
066200100120     C                   ENDSR
066300100120     C/EJECT
066400100120     C************************************************************
066500100120     C* CONTROLLO VIDEATA
066600100120     C************************************************************
066700100120     C     CTRD1         BEGSR
066800100120     C*
066900100120     C                   SETOFF                                       99
067000100120     C* non imposto pos. del cursore
067100100120     C                   Z-ADD     0             H1RIGA
067200100120     C                   Z-ADD     0             H1COLO
067300100120     C*
067400100120     C* CTRL DATA
067500100120     C*
067600100120     C                   MOVE      DATA          WRKDSC           10
067700100120     C     D1DSC         IFNE      0
067800100120     C                   CALLB     'XDT4000'
067900100120     C                   PARM                    XDTAMG           10
068000100120     C                   PARM      D1DSC         XDTGMA            6 0
068100100120     C                   PARM      3             XDTSTA            1 0
068200100120     C     XDTSTA        IFNE      0
068300100120     C                   SETON                                        5599
068400100120     C                   ELSE
068500100120     C                   MOVE      XDTAMG        WRKDSC
068600100120     C                   ENDIF
068700100120     C                   ENDIF
068800100120     C*
068900100120     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
069000100120     C*
0691001001201   >C     OPZ002        IFNE      '04'
069600100618     C*
069700100618     C* causale operativa
069800100618     C                   EXSR      CtrCau
069900100618     C*
070000100618     C                   EXSR      CtrDescr
070100100120     C*
070200100618     C*
070300100120     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
070400100120    >C     OPZ002        IFEQ      '01'
070500100120    >C     OPZ002        OREQ      '03'
070600100120    >C*-------------------------------------------------------------
070700100120    >C* impostare i campi chiave e verificare l'esistenza del record
070800100120    >C*-------------------------------------------------------------
070900160805$007 C                   MOVE      'HK1'         XTBCOD
071000100120     C                   MOVE      *ALL'0'       XTBKEY
071100100120    >C                   MOVE      D1COD         XTBKEY
071200100120    >C                   MOVE      D1UNITA       XTBUNT
071300100120     C                   MOVE      '0'           XTBALC
071400100120     C                   MOVE      '1'           XTBRIC
071500100120     C                   CALL      'SDGTABDR'
071600100120     C                   PARM                    SDGTABDS
071700100120     C     XTBERR        IFEQ      '0'
071800100120     C                   SETON                                        5999
071900100120     C                   ENDIF
072000100120     C* Controllo se compreso nei range indice
072100100120     C     XTBERR        IFEQ      '3'
072200100120    >C                   SETON                                        5899
072300100120     C                   END
072400100120     C*
072500100120     C* REPERISCO AUTORIZZAZIONI UTENTE
072600100120     C                   CLEAR                   XCHATB
072700100120     C                   MOVE      XSCSOC        XCTSOC
072800100120     C                   MOVE      XSCDUN        XCTUNI
072900100120     C                   MOVEL     KNMUS         XCTPRF
073000100120     C                   MOVE      XTBCOD        XCTCOD
073100100120     C                   CALL      'XCHKATB'
073200100120     C                   PARM                    XCHATB
073300100120     C                   MOVEL     XCTDDS        AUTDS
073400100120     C     XCTRTN        IFEQ      1
073500100120     C                   MOVEL     'N'           MANGRU
073600100120     C                   ENDIF
073700100120     C* Non ammessa immissione di gruppo
073800100120     C     XTBGRU        IFEQ      'G'
073900100120     C     MANGRU        ANDEQ     'N'
074000100120     C                   SETON                                        5799
074100100120     C                   END
074200100120     C* Non ammessa immissione di azienda
074300100120     C     XTBGRU        IFEQ      'A'
074400100120     C     MANAZI        ANDEQ     'N'
074500100120     C                   SETON                                        5699
074600100120     C                   END
074700100120     C* codice riservato non accessibile
074800100120     C     XTBRIS        IFGT      ABIRIS
074900100120    >C     OPZ002        ANDNE     '01'
075000100120    >C     OPZ002        ANDNE     '03'
075100100120     C                   SETON                                        5499
075200100120     C                   END
075300100120     C                   ENDIF
075400100120     C*
0755001001201-   C                   END
075600100120     C*
075700100120     C     *IN99         IFEQ      *ON
075800100120     C     $GEST         ANDEQ     'D1'
075900100120     C     OPZ002        ANDNE     '07'
076000100120     C                   MOVEA     *IN           WIN
076100100120     C                   MOVE      *ALL'0'       IN5098           49
076200100120     C                   MOVEA     IN5098        *IN(50)
076300100120     C                   WRITE     D1
076400100120     C                   MOVEA     WIN           *IN
076500100120     C                   ENDIF
076600100120     C*
076700100120     C                   ENDSR
076800100120     C/EJECT
079300100618     C************************************************************
079400100618     C* Controllo causale operativa
079500100618     C************************************************************
079600100618     C     CtrCau        BEGSR
079700100618     C*
079800100618     C                   EXSR      CallMvc003
079900100618     C                   if        $Esito      = *On
080000100618     C                   SetOn                                        5199
080100100618     c                   else
080200100618     C*
080300160805     C* la sottotipologia deve essere per Pagamenti
080400160805     C                   if        OPESTP     <> 'P'
080600160805     C*******            SetOn                                        6499
080700100618     C                   endif
080800100618     C*
080900160805     C* la tipologia deve essere per registrazioni non IVA
081000160805     C                   if        OPETPC     <> '0'
081100100618     C                   SetOn                                        6599
081200100618     C                   endif
081800100618     C*
081900160805     C                   if        DescrHK1    = *Blank
082000160805     c                   Eval      DescrHK1    = OPEDesBrev
082100100618     C                   endif
082200100618     C*
082300100618     C                   endif
082400100618     C*
082500100618     C                   ENDSR
082600100618     C/EJECT
082700100618     C************************************************************
082800100618     C* Chiamata al driver per controllo causale operativa
082900100618     C************************************************************
083000100618     C     CallMvc003    BEGSR
083100100618     C*
083200100618     C                   Clear                   ANOPE
083300100618     C*
083400100618     C                   EVAL      $LenOut = %Size(ANOPE)
083500100618     C                   CALLB     'NDMVC003'
083600100618     C                   PARM                    XScSoc
083700100618     C                   PARM                    D1Cau
083800100618     C                   PARM                    UDateIso
083900100618     C                   PARM      '1'           $Uso
084000100618     C                   PARM      *off          $GesMsg
084100100618     C                   PARM                    $Esito
084200100618     C                   PARM      'ANOPE'       $Struttura
084300100618     C                   PARM                    ANOPE
084400100618     C                   PARM                    $LenOut
084500100618     C                   PARM      *On           $Refresh
084600100618     C*
084700100618     C                   ENDSR
084800100618     C/EJECT
084900100618     C************************************************************
085000100618     C* Controllo descrizione
085100100618     C************************************************************
085200100618     C     CtrDescr      BEGSR
085300100618     C*
085400160805     C     DESCRHK1      IFEQ      *BLANK
085500100618     C                   SETON                                        5299
085600100618     C                   ENDIF
085700100618     C*
085800160805     C     DESHK1        IFEQ      *BLANK
085900160805     C                   Eval      DesHK1      = DescrHK1
086000100618     C                   ENDIF
086100100618     C*
086200100618     C                   ENDSR
086300100618     C/EJECT
086400100618     C************************************************************
086500100618     C* CONTROLLO VIDEATA D2
086600100618     C************************************************************
086700100618     C     CTRD2         BEGSR
086800100618     C*
086900100120     C                   ENDSR
087000100120     C/EJECT
087100100120     C************************************************************
087200100120    >C* AGGIORNAMENTO ANAGRAFICA
087300100120     C************************************************************
087400100120    >C     AGGANA        BEGSR
087500100120     C*
087600160805$007 C                   MOVE      'HK1'         XTBCOD
087700100120     C                   MOVE      *ALL'0'       XTBKEY
087800100120     C                   MOVE      D1COD         XTBKEY
087900100120     C                   MOVE      D1UNITA       XTBUNT
088000100120     C*
088100100120     C                   SELECT
088200100120    >C     OPZ002        WHENEQ    '01'
088300100120    >C     OPZ002        OREQ      '03'
088400100120    >C                   EXSR      RIEANA
088500100120     C                   MOVE      XSCCUT        XTBUTE
088600100120     C                   MOVE      '5'           XTBRIC
088700100120     C                   CALL      'SDGTABDR'
088800100120     C                   PARM                    SDGTABDS
088900100120     C*
089000100120     C* se rcd non scrivibile attivo errore
089100100120     C     XTBERR        IFEQ      '1'
089200100120    >C                   SETON                                        5999
089300100120     C                   END
089400100120    >C     OPZ002        WHENEQ    '02'
089500100120     C                   MOVE      '1'           XTBRIC
089600100120     C                   MOVE      '1'           XTBALC
089700100120     C                   CALL      'SDGTABDR'
089800100120     C                   PARM                    SDGTABDS
089900100120    >C                   EXSR      RIEANA
090000100120     C                   MOVE      XSCCUT        XTBUTE
090100100120     C                   MOVE      '4'           XTBRIC
090200100120     C                   CALL      'SDGTABDR'
090300100120     C                   PARM                    SDGTABDS
090400100120    >C     OPZ002        WHENEQ    '04'
090500100120     C                   MOVE      '1'           XTBRIC
090600100120     C                   MOVE      '1'           XTBALC
090700100120     C                   CALL      'SDGTABDR'
090800100120     C                   PARM                    SDGTABDS
090900100120     C*
091000100120     C                   MOVE      '6'           XTBRIC
091100100120     C                   CALL      'SDGTABDR'
091200100120     C                   PARM                    SDGTABDS
091300100120     C*
091400100120     C* annullo traduzioni
091500100120     C     XTBERR        IFEQ      '0'
091600100120     C                   EXSR      ANNTBT
091700100120     C                   END
091800100120     C*
091900100120     C                   ENDSL
092000100120     C*
092100100120     C                   ENDSR
092200100120     C/EJECT
092300100120     C************************************************************
092400100120     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
092500100120     C************************************************************
092600100120     C     RIEANA        BEGSR
092700100120     C                   CLEAR                   XTBUNI
092800100120     C                   MOVE      WRKDSC        XTBDSC
092900100120     C                   MOVE      D1RIS         XTBRIS
093000100120$026 C
093100100120     C                   MOVEL     EXTDS         XTBUNI
093200100120     C                   ENDSR
093300100120     C/EJECT
093400100120     C************************************************************
093500100120     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
093600100120     C************************************************************
093700100120     C     GESAGG        BEGSR
093800100120     C*
093900100120     C* segnalo al pgm chiamante l'avvenuta conferma
094000100120    >C                   MOVE      *ON           OPR002
094100100120     C* segnalo al pgm chiamante l'aver premuto l'F6
094200100120    >C                   MOVE      '0'           RET002
094300100120     C*
0944001001201    C                   SELECT
094500100120     C* nel caso di immissione, ripristino la videata iniziale
094600100120    >C     OPZ002        WHENEQ    '01'
094700100120     C                   MOVE      *ON           $INZD1
094800100120     C* altrimenti ritorno al pgm chiamante
094900100120     C                   OTHER
095000100120     C                   MOVE      *ON           $FINE
0951001001201-   C                   ENDSL
095200100120     C*
095300100120     C                   ENDSR
095400100120     C/EJECT
095500100120     C************************************************************
095600100120    >C* ANNULLAMENTO ARCHIVIO TRADUZIONI
095700100120     C************************************************************
095800100120    >C     ANNTBT        BEGSR
095900100120     C                   MOVE      XTBAZI        TBTAZI
096000100120     C                   MOVE      XTBCOD        TBTCOD
096100100120     C                   MOVE      XTBKEY        TBTKEY
096200100120     C     KTBT          SETLL     ANTBT01L
096300100120     C                   DO        *HIVAL
096400100120     C     KTBT          DELETE    ANTBT01L                           21
096500100120     C  N21              END
096600100120     C*
096700100120     C                   ENDSR
096800100120     C/EJECT
096900100120     C************************************************************
097000100120     C* OPERAZIONI INIZIALI
097100100120     C************************************************************
097200100120     C     *INZSR        BEGSR
097300100618     C*
097400100618     C     *DMY          Move      Udate         UDateISO
097500100120     C*
097600100120     C* Reperimento parametri
097700100120     C*
097800100120     C     *ENTRY        PLIST
097900100120     C                   PARM                    KPJBA
098000100120     C*
098100100120     C* Reperimento Codice utente
098200100120     C*
098300100120     C*
098400100120     C* reperisco le dimensioni delle variabili che conterranno i tasti
098500100120     C                   EVAL      $D1L01 = %Size(Z1Ke1)
098600100120     C                   EVAL      $D1L02 = %Size(Z1Ke2)
098700100120     C*
098800100120     C* Reperimento tasti di funzione
098900100120     C*
099000100120     C                   EXSR      REPKEY
099100100120     C*
099200100120     C* KLIST
099300100120     C*
099400100120     C     KTBT          KLIST
099500100120     C                   KFLD                    TBTAZI
099600100120     C                   KFLD                    TBTCOD
099700100120     C                   KFLD                    TBTKEY
099800100120     C*
099900100120     C* reperisco le dimensioni della variabile del titolo
100000100120     C                   EVAL      LENTIT = %Size(T1Tit)
100100100120     C*
100200100120     C                   ENDSR
100300100120     C/EJECT
100400100120      ************************************************************
100500100120      * Reperimento dati società
100600100120      ************************************************************
100700100120     C     REPSOC        BEGSR
100800100120      *
100900100120     C                   CALL      'SDGSOCDR'
101000100120     C                   PARM                    TIPXSC            6
101100100120     C                   PARM                    SOCXSC            3
101200100120     C                   PARM                    CDSXSC            9 0
101300100120     C                   PARM                    MODXSC            3
101400100120     C                   PARM      *BLANKS       RTNXSC            1
101500100120     C                   PARM                    XSOCDS
101600100120     C                   PARM                    KPJBA
101700100120      *
101800100120     C                   ENDSR
101900100120     C/EJECT
102000100120     C************************************************************
102100100120     C* REPERIMENTO TASTI DI FUNZIONE
102200100120     C************************************************************
102300100120     C     REPKEY        BEGSR
102400100120     C                   MOVEA     $FM1          $FM
102500100120     C                   EXSR      REPKKK
102600100120     C                   MOVEA     $FX           $FX1
102700100120     C                   ENDSR
102800100120     C/EJECT
102900100120     C************************************************************
103000100120     C* REPERIMENTO TASTI DI FUNZIONE
103100100120     C************************************************************
103200100120     C     REPKKK        BEGSR
103300100120     C*
103400100120     C                   CLEAR                   $FX
103500100120     C     1             DO        24            X
103600100120     C*
103700100120     C     $FM(X)        IFNE      *BLANK
103800100120     C*
103900100120     C                   MOVEL     $FM(X)        IDMSG
104000100120     C                   CALLB     'XRTVM'
104100100120     C                   PARM                    IDMSG            27
104200100120     C                   PARM                    TXTMSG          100
104300100120     C                   PARM                    ERRMSG            1
104400100120     C*
104500100120     C     ERRMSG        IFNE      *ON
104600100120     C                   MOVEL     TXTMSG        $FX(X)
104700100120     C                   ELSE
104800100120     C                   MOVEL     *ALL' '       $FX(X)
104900100120     C                   END
105000100120     C*
105100100120     C                   END
105200100120     C*
105300100120     C                   END
105400100120     C*
105500100120     C                   ENDSR
105600100120     C/EJECT
105700100120     C************************************************************
105800100120     C* INIZIALIZZAZIONE VARIABILI
105900100120     C************************************************************
106000100120     C     INZVAR        BEGSR
106100100120     C*
106200100120     C                   MOVEL     'SOC001'      TIPXSC
106300100120     C                   EXSR      REPSOC
106400100120     C     RTNXSC        IFNE      '1'
106500100120     C                   MOVEL     XSOCDS        SOC001
106600100120     C                   ELSE
106700100120     C                   MOVE      *ON           $FINE
106800100120     C* fine programma
106900100120     C                   EXSR      ENDPGM
107000100120     C                   ENDIF
107100100120     C*
107200100120     C* Dati da PGM guida
107300100120     C*
107400100120    >C                   MOVEL     KPJBU         D002DS
107500100120     C*
107600100120     C* Variabili per gestione videate
107700100120     C*
107800100120     C                   MOVE      'D1'          $GEST
107900100120     C                   MOVE      *OFF          $FINE
108000100120     C                   MOVE      *ON           $INZD1
108100100120     C                   MOVE      *BLANK        $LASTG
108200100120     C                   MOVE      *BLANK        $LASTV
108300100120     C                   Z-ADD     0             $ULKD1            3 0
108400100120     C*
108500100120     C* Variabili appoggio
108600100120     C*
108700100120     C                   Z-ADD     0             CURR
108800100120     C                   Z-ADD     0             CURC
108900100120     C*
109000100120     C* Valorizzazione campi univoci testate
109100100120     C*
109200100120     C                   CLEAR                   T1
109300100120     C                   MOVEL     KNSIF         NOMSIF
109400100120     C                   MOVEL     XSCDSI        NOMDIT
109500100120     C                   MOVEL     KCDAZ         NOMAZN
109600100120     C                   Z-ADD     1             X
109700100120    >C     OPZ002        LOOKUP    $OZ(X)                                 21
109800100120     C     *IN21         IFEQ      *ON
109900100120     C                   MOVEL     $ID(X)        IDMSG
110000100120     C                   CALLB     'XRTVM'
110100100120     C                   PARM                    IDMSG            27
110200100120     C                   PARM                    TXTMSG          100
110300100120     C                   PARM                    ERRMSG            1
110400100120     C                   PARM      *Off          CTRMSG            1
110500100120     C                   PARM      30            LENMSG            3 0
110600100120     C     ERRMSG        IFNE      *ON
110700100120     C                   MOVEL     TXTMSG        T1OPE
110800100120     C                   ELSE
110900100120     C                   MOVEL     *ALL'?'       T1OPE
111000100120     C                   ENDIF
111100100120     C                   END
111200100120     C*
111300100120     C* Inizializzazione/Decodifica prima videata
111400100120     C*
111500100120     C                   EXSR      INZD1
111600100120     C                   MOVE      *OFF          $INZD1
111700100120     C* Se opzione = '07' richiesti solo controlli
111800100120     C     OPZ002        IFEQ      '07'
111900100120     C                   MOVEL     *IN99         ERR002
112000100120     C                   MOVEL     '0'           RET002
112100100120     C                   MOVEL     *ON           $FINE
112200100120     C                   ENDIF
112300100120     c*
112400100120     c* a seconda della lingua richiesta o meno, abilito o disablito F7
112500100120$029 C                   eval      $fc1(7)=*on
112600100120     C*
112700100120     C                   ENDSR
112800100120     C************************************************************
112900100120     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
113000100120     C************************************************************
113100100120     C     DEFCAM        BEGSR
113200100120     C*
113300100120     C* klist
113400100120     C*
113500100120     C*
113600100120     C* Variabili per gestione videate
113700100120     C*
113800100120     C                   MOVE      *BLANK        $GEST             2
113900100120     C                   MOVE      *BLANK        $FINE             1
114000100120     C                   MOVE      *BLANK        $INZD1            1
114100100120     C                   MOVE      *BLANK        $LASTG            2
114200100120     C                   MOVE      *BLANK        $LASTV            2
114300100120     C*
114400100120     C* Variabili appoggio
114500100120     C*
114600100120     C                   Z-ADD     0             CURR              2 0
114700100120     C                   Z-ADD     0             CURC              2 0
114800100120     C                   MOVE      *ZEROS        WIN              99
114900100120     C                   MOVE      *BLANK        WTBERR            1
115000100120     C*
115100100120     C* Indici
115200100120     C*
115300100120     C                   Z-ADD     0             X                 3 0
115400100120     C                   Z-ADD     0             Y                 3 0
115500100120     C*
115600100120     C                   ENDSR
115700100120     C************************************************************
115800100120     C/EJECT
115900100120     C************************************************************
116000100120     C* CONTROLLO ABILITAZIONE TASTI
116100100120     C************************************************************
116200100120     C     CTRKEY        BEGSR
116300100120     C                   SELECT
116400100120     C     $TASTO        WHENEQ    F01
116500100120     C     $FC(1)        IFEQ      '0'
116600100120     C                   SETON                                        99
116700100120     C                   ENDIF
116800100120     C     $TASTO        WHENEQ    F02
116900100120     C     $FC(2)        IFEQ      '0'
117000100120     C                   SETON                                        99
117100100120     C                   ENDIF
117200100120     C     $TASTO        WHENEQ    F03
117300100120     C     $FC(3)        IFEQ      '0'
117400100120     C                   SETON                                        99
117500100120     C                   ENDIF
117600100120     C     $TASTO        WHENEQ    F04
117700100120     C     $FC(4)        IFEQ      '0'
117800100120     C                   SETON                                        99
117900100120     C                   ENDIF
118000100120     C     $TASTO        WHENEQ    F05
118100100120     C     $FC(5)        IFEQ      '0'
118200100120     C                   SETON                                        99
118300100120     C                   ENDIF
118400100120     C     $TASTO        WHENEQ    F06
118500100120     C     $FC(6)        IFEQ      '0'
118600100120     C                   SETON                                        99
118700100120     C                   ENDIF
118800100120     C     $TASTO        WHENEQ    F07
118900100120     C     $FC(7)        IFEQ      '0'
119000100120     C                   SETON                                        99
119100100120     C                   ENDIF
119200100120     C     $TASTO        WHENEQ    F08
119300100120     C     $FC(8)        IFEQ      '0'
119400100120     C                   SETON                                        99
119500100120     C                   ENDIF
119600100120     C     $TASTO        WHENEQ    F09
119700100120     C     $FC(09)       IFEQ      '0'
119800100120     C                   SETON                                        99
119900100120     C                   ENDIF
120000100120     C     $TASTO        WHENEQ    F10
120100100120     C     $FC(10)       IFEQ      '0'
120200100120     C                   SETON                                        99
120300100120     C                   ENDIF
120400100120     C     $TASTO        WHENEQ    F11
120500100120     C     $FC(11)       IFEQ      '0'
120600100120     C                   SETON                                        99
120700100120     C                   ENDIF
120800100120     C     $TASTO        WHENEQ    F12
120900100120     C     $FC(12)       IFEQ      '0'
121000100120     C                   SETON                                        99
121100100120     C                   ENDIF
121200100120     C     $TASTO        WHENEQ    F13
121300100120     C     $FC(13)       IFEQ      '0'
121400100120     C                   SETON                                        99
121500100120     C                   ENDIF
121600100120     C     $TASTO        WHENEQ    F14
121700100120     C     $FC(14)       IFEQ      '0'
121800100120     C                   SETON                                        99
121900100120     C                   ENDIF
122000100120     C     $TASTO        WHENEQ    F15
122100100120     C     $FC(15)       IFEQ      '0'
122200100120     C                   SETON                                        99
122300100120     C                   ENDIF
122400100120     C     $TASTO        WHENEQ    F16
122500100120     C     $FC(16)       IFEQ      '0'
122600100120     C                   SETON                                        99
122700100120     C                   ENDIF
122800100120     C     $TASTO        WHENEQ    F17
122900100120     C     $FC(17)       IFEQ      '0'
123000100120     C                   SETON                                        99
123100100120     C                   ENDIF
123200100120     C     $TASTO        WHENEQ    F18
123300100120     C     $FC(18)       IFEQ      '0'
123400100120     C                   SETON                                        99
123500100120     C                   ENDIF
123600100120     C     $TASTO        WHENEQ    F19
123700100120     C     $FC(19)       IFEQ      '0'
123800100120     C                   SETON                                        99
123900100120     C                   ENDIF
124000100120     C     $TASTO        WHENEQ    F20
124100100120     C     $FC(20)       IFEQ      '0'
124200100120     C                   SETON                                        99
124300100120     C                   ENDIF
124400100120     C     $TASTO        WHENEQ    F21
124500100120     C     $FC(21)       IFEQ      '0'
124600100120     C                   SETON                                        99
124700100120     C                   ENDIF
124800100120     C     $TASTO        WHENEQ    F22
124900100120     C     $FC(22)       IFEQ      '0'
125000100120     C                   SETON                                        99
125100100120     C                   ENDIF
125200100120     C     $TASTO        WHENEQ    F23
125300100120     C     $FC(23)       IFEQ      '0'
125400100120     C                   SETON                                        99
125500100120     C                   ENDIF
125600100120     C     $TASTO        WHENEQ    F24
125700100120     C     $FC(24)       IFEQ      '0'
125800100120     C                   SETON                                        99
125900100120     C                   ENDIF
126000100120     C                   ENDSL
126100100120     C                   ENDSR
126200100120     C/EJECT
126300100120     C************************************************************
126400100120     C* RIEMPIMENTO TASTI FUNZIONE
126500100120     C************************************************************
126600100120     C     RIEKEY        BEGSR
126700100120     C*
126800100120     C* Input :
126900100120     C* - $FX = schiera con tutti i testi disponibili
127000100120     C* - $FC = schiere con i flag di validità associati ai testi
127100100120     C* - $FR = schiere con i flag di REVERSE IMAGE
127200100120     C* - $ULK = ultimo testo caricato
127300100120     C* Output :
127400100120     C* - $ULK
127500100120     c* - $KEY1= 1° riga di tasti funzione
127600100120     c* - $KEY2= 2° riga di tasti funzione
127700100120     C*
127800100120     C                   CALLB     'XKEYMSG'
127900100120     C* input
128000100120     C                   PARM                    $FX
128100100120     C                   PARM                    $FC
128200100120     C                   PARM                    $FR
128300100120     C                   PARM                    $ULK              3 0
128400100120     C* output
128500100120     C                   PARM                    $KEY1            79
128600100120     C                   PARM                    $KEY2            79
128700100120     C* input
128800100120     C                   PARM                    $L01              3 0
128900100120     C                   PARM                    $L02              3 0
129000100120     C*
129100100120     C                   PARM                    $ALLFUNCT       480
129200100120     C*
129300100120     C                   ENDSR
129400100120     C/EJECT
129500100120**
12960010012001SDGMSG    *LIBL     SDC0002    inserimento
12970010012002PROMSG    *LIBL     COS0002    modifica
12980010012003PROMSG    *LIBL     COS0003    copia
12990010012004PROMSG    *LIBL     COS0004    annullamento
13000010012005PROMSG    *LIBL     COS0005    visualizzazione
130100100120** TASTI DI FUNZIONE  D1
130200100120PROMSG    *LIBL     KEY0001  01
130300100120PROMSG    *LIBL     KEY0002  02
130400100120PROMSG    *LIBL     KEY0003  03
130500100120PROMSG    *LIBL     KEY0004  04
130600100120PROMSG    *LIBL     KEY0005  05
130700100120PROMSG    *LIBL     KEY0006  06
130800100120PROMSG    *LIBL     KEY0007  07
130900100120PROMSG    *LIBL     KEY0008  08
131000100120PROMSG    *LIBL     KEY0009  09
131100100120PROMSG    *LIBL     KEY0010  10
131200100120PROMSG    *LIBL     KEY0011  11
131300100120PROMSG    *LIBL     KEY0012  12
131400100120PROMSG    *LIBL     KEY0013  13
131500100120PROMSG    *LIBL     KEY0014  14
131600100120PROMSG    *LIBL     KEY0015  15
131700100120PROMSG    *LIBL     KEY0016  16
131800100120SDGMSG    *LIBL     SDK0001  17
131900100120PROMSG    *LIBL     KEY0018  18
132000100120PROMSG    *LIBL     KEY0019  19
132100100120PROMSG    *LIBL     KEY0020  20
132200100120PROMSG    *LIBL     KEY0021  21
132300100120PROMSG    *LIBL     KEY0022  22
132400100120PROMSG    *LIBL     KEY0023  23
132500100120PROMSG    *LIBL     KEY0024  24
132600100120**  ABILITAZIONE D1
1327001001200N  01
1328001001200N  02
1329001001201N  03
1330001001201N  04
1331001001201N  05
1332001001201N  06
1333001001201N  07
1334001001201N  08
1335001001200N  09
1336001001200N  10
1337001001200N  11
1338001001201N  12
1339001001200N  13
1340001001200N  14
1341001001200N  15
1342001001200N  16
1343001001200N  17
1344001001200N  18
1345001001200N  19
1346001001200N  20
1347001001200N  21
1348001001200N  22
1349001001200N  23
1350001001200N  24
135100100120**
135200100120DESCR Descrizione ................
