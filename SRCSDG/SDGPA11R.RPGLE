000100100210     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200100210     H*PARMS CVTOPT(*NONE)
000300100210     H DECEDIT('0,') DATEDIT(*DMY.)
000400100210      *
000500100827      *       parametri per Elenchi Black list
000600100210      *
000700100210     F*----------------------------------------------------*
000800151023     FSDGPA11D  CF   E             WORKSTN
000900151023     F                                     RENAME(PA11D1:FMTD1)
001000151023     F                                     RENAME(PA11D2:FMTD2)
001100151023     F                                     RENAME(PA11T1:FMTT1)
001200151023     F                                     RENAME(PA11Z1:FMTZ1)
001300100210     F                                     INFDS(DSFMT)
001400100210     D*----------------------------------------------------*
001500100210     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
001600100210     D $ID             S             27    DIM(5) ALT($OZ)
001700100210     D* Schiere per la gestione dei tasti di funzione
001800100210     D $FM             S             27    DIM(24)
001900100210     D $FX             S             50    DIM(24)
002000100210     D $FC             S              1    DIM(24)
002100100210     D $FR             S              1    DIM(24)
002200100210     D* Video D1
002300100210     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
002400100210     D $FX1            S             50    DIM(24) ALT($FM1)
002500100210     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
002600100210     D $FR1            S              1    DIM(24) ALT($FC1)
002700100210     D*
002800100210     D*----------------------------------------------------*
002900100210     D* Passaggio Parametri
003000100210     D KPJBA         E DS
003100100210     D*-------------
003200100210     D* Sottotitolo di testata
003300100210     D SOTTIT          C                   CONST('               -
003400100210     D                                                 ')
003500100210     D*-------------
003600100210     D* Dati di ambiente ottenuti da SDGSOCDR
003700100210     D SOC001        E DS                  EXTNAME(SDGSOCDS )
003800100210     D*-------------
003900100210     D* DS Interna per dati di output di SDGSOCDR
004000100210     D XSOCDS          DS          1000
004100100210     D*-------------
004200100210     D* Ds esterna dati
004300151023     D SDPA11        E DS                  EXTNAME(SDGPA11DS)
004400100210     D  $DT                    1   2000
004500100210     D                                     DIM(2000)
004600120306R091 D*-------------
004700120306 "   D* Parametri del PGM SDGSD20R per importi soglia
004800120306 "   D SDGSD20DS     E DS                  inz
004900120306 "   D  RETD20       E                     INZ('0')
005000120306 "   D  OPRD20       E                     INZ('0')
005100120306 "   D  ERRD20       E                     INZ('0')
005200120306R091 D* CMTD20       E                     INZ('0')
005300100210     D*-------------
005400100210     D* Parametri ricevuti
005500100210     D ANPAR         E DS                  EXTNAME(ANPARDS)
005600100210     D  ANPSOC       E                     EXTFLD(ANPSOCIETA)
005700100210     D  ANPPAR       E                     EXTFLD(ANPPARAME)
005800100210     D  ANPDAT       E                     EXTFLD(ANPDATI)
005900100210     D                                     DIM(2000)
006000100210     D  ANPERR       E                     EXTFLD(ANPERRORE)
006100100210     D*-------------
006200100210     D* Parametri per ricerca codice ANATB00F
006300100210     D XTABDS        E DS                  inz
006400100210     D*-------------
006500100210     D* Parametri per controllo codice ANATB00F
006600100210     D SDGTABDS      E DS                  INZ
006700100210     D*-------------
006800100210     D* Ds per tabella codici attributi contabili
006900100210     D ANG0A1DS      E DS                  INZ
007000100827     D*-------------
007100100827     D* Ds per ricerca legende
007200100827     D ANA964DS      E DS                  INZ
007300100827     D*-------------
007400100827     D* Controlli validità per valore di una legenda
007500100827     D X07VALDS      E DS                  Inz
007600100210     D*-------------
007700100210     D DSFMT           DS
007800100210     D  $TASTO               369    369
007900100210     D  NRG                  370    370
008000100210     D  NCL                  371    371
008100100210     D*-------------
008200100210     D* posizione cursore
008300100210     D CURSOR          DS
008400100210     D  RIRI                   1      2B 0 INZ
008500100210     D  R$$                    2      2
008600100210     D  COCO                   3      4B 0 INZ
008700100210     D  C$$                    4      4
008800100210     D*-------------
008900100210     D* Costanti
009000100210     D*-------------
009100100210     D* Tasti di funzione
009200100210     D F01             C                   CONST(X'31')
009300100210     D F02             C                   CONST(X'32')
009400100210     D F03             C                   CONST(X'33')
009500100210     D F04             C                   CONST(X'34')
009600100210     D F05             C                   CONST(X'35')
009700100210     D F06             C                   CONST(X'36')
009800100210     D F07             C                   CONST(X'37')
009900100210     D F08             C                   CONST(X'38')
010000100210     D F09             C                   CONST(X'39')
010100100210     D F10             C                   CONST(X'3A')
010200100210     D F11             C                   CONST(X'3B')
010300100210     D F12             C                   CONST(X'3C')
010400100210     D F13             C                   CONST(X'B1')
010500100210     D F14             C                   CONST(X'B2')
010600100210     D F15             C                   CONST(X'B3')
010700100210     D F16             C                   CONST(X'B4')
010800100210     D F17             C                   CONST(X'B5')
010900100210     D F18             C                   CONST(X'B6')
011000100210     D F19             C                   CONST(X'B7')
011100100210     D F20             C                   CONST(X'B8')
011200100210     D F21             C                   CONST(X'B9')
011300100210     D F22             C                   CONST(X'BA')
011400100210     D F23             C                   CONST(X'BB')
011500100210     D F24             C                   CONST(X'BC')
011600100210     D ENTER           C                   CONST(X'F1')
011700100210     D ROLDWN          C                   CONST(X'F4')
011800100210     D ROLLUP          C                   CONST(X'F5')
011900100210     D*-------------
012000100210     D* Reperimento nome PGM
012100100210     D STATUS         SDS           333
012200100210     D  DSPGM            *PROC
012300100210     C*----------------------------------------------------*
012400100210     C*                MAIN LINE PROGRAM
012500100210     C*----------------------------------------------------*
012600100210     C*
012700100210     C                   EXSR      INZVAR
012800100210     C*
012900100210     C     $FINE         DOWEQ     *OFF
013000100210     C     $GEST         CASEQ     'D1'          GESD1
013100100216     C     $GEST         CASEQ     'D2'          GESD2
013200100210     C                   ENDCS
013300100210     C                   ENDDO
013400100210     C*
013500100210     C                   EXSR      ENDPGM
013600100210     C/EJECT
013700100210     C************************************************************
013800100210     C* FINE PROGRAMMA
013900100210     C************************************************************
014000100210     C     ENDPGM        BEGSR
014100100210     C*
014200100210     C                   SETON                                            RT
014300100210     C                   RETURN
014400100210     C*
014500100210     C                   ENDSR
014600100210     C/EJECT
014700100210     C************************************************************
014800100210     C* RICERCHE
014900100210     C************************************************************
015000100210     C     SEARCH        BEGSR
015100100210     C*
015200100210     C* determino Riga/Colonna del cursore
015300100210     C                   MOVE      NRG           R$$
015400100210     C                   MOVE      NCL           C$$
015500100210     C                   Z-ADD     RIRI          CURR
015600100210     C                   Z-ADD     COCO          CURC
015700100210     C*
015800100210     C* I campi che al momento dell'F4 risultano protetti non devono
015900100210     C* essere abilitati alla funzione di ricerca
016000100210     C*
016100100827     C                   SELECT
016200100909     C*
016300100909     C* tipo data di riferimento
016400100909     C                   When      H1NMFL       = 'D1TPDTRIF'
016500100909     C                   EXSR      F4_TpDtRif
016600100210     C*
016700100827     C*
016800100827     C* Valore per conto escluso dalla presentazione
016900100827     C* Valore per conto con operazioni di servizi
017000100827     C* Valore per conto con operazioni di beni
017100100827     C                   When      H1NMFL       = 'D1BLESCL'   or
017200100827     C                             H1NMFL       = 'D1BLSERV'   or
017300100827     C                             H1NMFL       = 'D1BLBENI'
017400100210     C                   EXSR      F4_Def
017401151028     C*
017402151028     C* Valore di default
017403151028     C                   When      H1NMFL       = 'D1BENISRV'
017404151028     C                   EXSR      F4_BeniSrv
017405151203     C*
017406151203     C* FMTD1 - Gestione note di variazione
017407151203     C     H1NMFL        WHENEQ    'D1NOTEVAR'
017408151203     C                   EXSR      F4_NoteVar
017500100210     C*
017600100210     C* ricerca non abilitata in questa posizione
017700100210     C                   OTHER
017800100210     C                   SETON                                        9899
017900100210     C*
018000100210     C                   ENDSL
018100100210     C*
018200100210     C* imposto pos. del cursore
018300100210     C                   Z-ADD     CURR          H1RIGA
018400100210     C                   Z-ADD     CURC          H1COLO
018500100210     C                   MOVE      *HIVAL        $LASTV
018600100210     c*
018700100210     C                   ENDSR
018800100210     C/EJECT
018900100909     C************************************************************
019000100909     C* Ricerca tipo data di riferimento
019100100909     C************************************************************
019200100909     C     F4_TpDtRif    BEGSR
019300100909     C*
019400100909     C                   Reset                   ANA964DS
019500100909     C*
019600100909     C                   MoveL     '01'          Opz964
019700151023     C                   MoveL     'SDGPA11S'    Trc964
019800151023     C                   MoveL     'TPDTRIFA11'  Cam964
019900100909     C                   MoveL     *On           All964
020000100909     C*
020100100909     C                   MoveL     ANA964DS      KPJBU
020200100909     C                   Call      'ANA964R'
020300100909     C                   Parm                    KPJBA
020400100909     C                   MoveL     KPJBU         ANA964DS
020500100909     C*
020600100909     C                   if        Ret964     <> '2'
020700100909     C                   MoveL     Val964        D1TpDtRif
020800100909     C                   endif
020900100909     C*
021000100909     C                   ENDSR
021100100909      /EJECT
021200100210     C************************************************************
021300100210     C* Ricerca valori di default per attributo contabile
021400100210     C************************************************************
021500100210     C     F4_Def        BEGSR
021600100210     C*
021700100210     C                   RESET                   XTABDS
021800100210     C*
021900100210     C                   MOVE      '01'          XTAOPZ
022000100210     C                   MOVE      XSCSOC        XTAAZI
022100100210     C                   MOVE      XSCLIN        XTALIN
022200100210     C                   Z-ADD     CURR          XTARIG
022300100210     C                   Z-ADD     CURC          XTACOL
022400100210     C                   MOVE      *ZERO         XTaKey
022500100210     C*
022600100827     C                   Move      D1BlPos       XTaPos
022700100827     C*
022800100827     C                   Select
022900100827     C* Valore per conto escluso dalla presentazione
023000100827     C                   When      H1NMFL       = 'D1BLESCL'
023100100827     C                   MoveL(P)  D1BlEscl      Wrk6              6
023200100827     C* Valore per conto con operazioni di servizi
023300100827     C                   When      H1NMFL       = 'D1BLSERV'
023400100827     C                   MoveL(P)  D1BlServ      Wrk6              6
023500100827     C* Valore per conto con operazioni di beni
023600100827     C                   When      H1NMFL       = 'D1BLBENI'
023700100827     C                   MoveL(P)  D1BlBeni      Wrk6              6
023800100827     C                   endSL
023900100827     C*
024000100210     C                   Move      Wrk6          XTaKey
024100100210     C*
024200100210     C                   MoveL     XTABDS        KPJBU
024300100210     C                   CALL      'ANG0A1R'
024400100210     C                   PARM                    KPJBA
024500100210     C*
024600100210     C                   MoveL     KPJBU         XTABDS
024700100210     C                   if        XTARet     <> '2'
024800100210     C*
024900100210     C                   Select
025000100827     C* Valore per conto escluso dalla presentazione
025100100827     C                   When      H1NMFL       = 'D1BLESCL'
025200100827     C                   Move      XTAKEY        D1BLEscl
025300100827     C* Valore per conto con operazioni di servizi
025400100827     C                   When      H1NMFL       = 'D1BLSERV'
025500100827     C                   Move      XTAKEY        D1BlServ
025600100827     C* Valore per conto con operazioni di beni
025700100827     C                   When      H1NMFL       = 'D1BLBENI'
025800100827     C                   Move      XTAKEY        D1BlBeni
025900100210     C                   endSL
026000100210     C*
026100100210     C                   endif
026200100210     C*
026300100210     C                   ENDSR
026400100210     C/EJECT
026402151028     C************************************************************
026403151028     C* Ricerca default per beni/servizi
026404151028     C************************************************************
026405151028     C     F4_BeniSrv    BEGSR
026406151028     C*
026407151028     C                   RESET                   ANA964DS
026408151028     C*
026409151028     C                   Move      '01'          Opz964
026410151029     C                   MOVEL     'SDGPA11S  '  TRC964
026411151029     C                   MOVEL     'BENISRVA11'  CAM964
026412151028     C                   MOVE      *ON           ALL964
026413151028     C                   MOVEL     ANA964DS      KPJBU
026414151028     C                   CALL      'ANA964R'
026415151028     C                   PARM                    KPJBA
026416151028     C                   MOVEL     KPJBU         ANA964DS
026417151028     C     RET964        IFEQ      *BLANK
026418151028     C                   MOVEL     VAL964        D1BeniSrv
026419151028     C                   ENDIF
026420151028     C*
026421151028     C                   ENDSR
026422151028     C/EJECT
026423151203     C************************************************************
026424151203     C* Ricerca valori per Gestione note di variazione
026425151203     C************************************************************
026426151203     C     F4_NoteVar    BEGSR
026427151203     C*
026428151203     C                   RESET                   ANA964DS
026429151203     C*
026430151203     C                   Move      '01'          Opz964
026431151203     C                   MOVEL     'SDGPA11S  '  TRC964
026432151203     C                   MOVEL     'NOTEVARA11'  CAM964
026433151203     C                   MOVE      *ON           ALL964
026434151203     C                   MOVEL     ANA964DS      KPJBU
026435151203     C                   CALL      'ANA964R'
026436151203     C                   PARM                    KPJBA
026437151203     C                   MOVEL     KPJBU         ANA964DS
026438151203     C     RET964        IFEQ      *BLANK
026439151203     C                   MOVEL     VAL964        D1NoteVar
026440151203     C                   ENDIF
026441151203     c*
026442151203     C                   ENDSR
026443151203     C/EJECT
026500100210     C************************************************************
026600100210     C* GESTIONE EMISSIONE TASTI FUNZIONE
026700100210     C************************************************************
026800100210     C     GESPIE        BEGSR
026900100210     C*
027000100210     C                   SELECT
027100100210     C*
027200100210     C     $GEST         WHENEQ    'D1'
027300100210     C     $ULKD1        IFEQ      0
027400100210     C* riempimento opzioni
027500100210     C                   EXSR      F24D1
027600100210     C                   ELSE
027700100210     C                   WRITE     FMTZ1
027800100210     C                   ENDIF
027900100210     C*
028000100210     C     $GEST         WHENEQ    'D2'
028100100210     C     $ULKD1        IFEQ      0
028200100210     C* riempimento opzioni
028300100216     C                   EXSR      F24D2
028400100210     C                   ELSE
028500100210     C                   WRITE     FMTZ1
028600100210     C                   ENDIF
028700100210     C                   ENDSL
028800100210     C*
028900100210     C                   ENDSR
029000100210     C/EJECT
029100100210     C************************************************************
029200100210     C* GESTIONE VIDEO RECORD D1
029300100210     C************************************************************
029400100210     C     GESD1         BEGSR
029500100216     C*
029600100210     C* inizializzazione videata
029700100210     C     $INZD1        IFEQ      *ON
029800100210     C                   EXSR      INZD1
029900100210     C                   MOVE      *OFF          $INZD1
030000100210   1EC                   ENDIF
030100100210     C* emissione piede videata se proveniente da altra
030200100210     C* salvataggio valore formato di provenienza
030300100210   1BC     $LASTV        IFNE      'D1'
030400100210     C*
030500100210     C                   WRITE     FMTT1
030600100210     C                   EXSR      GESPIE
030700100210     C                   MOVE      $LASTV        $LASTG
030800100210     C                   MOVE      'D1'          $LASTV
030900100210   1EC                   ENDIF
031000100210     C*
031100100210     C* se esistono errori sulla videata
031200100210     C* emetto la write del formato a indicatori spenti per vedere
031300100210     C* le eventuali decodifiche
031400100210     C     *IN99         IFEQ      *ON
031500100210     C     $GEST         ANDEQ     'D1'
031600100210     C                   MOVEA     *IN           WIN
031700100210     C                   MOVE      *ALL'0'       IN5098           49
031800100210     C                   MOVEA     IN5098        *IN(50)
031900100210     C                   WRITE     FMTD1
032000100210     C                   MOVEA     WIN           *IN
032100100210     C                   ENDIF
032200100210     C*
032300100210     C                   SELECT
032400100210     C* Annullamento
032500100210     C     ANPOPZ        WHENEQ    '04'
032600100210     C* Visualizzazione
032700100210     C     ANPOPZ        OREQ      '05'
032800100210     C                   WRITE     FMTD1
032900100210     C                   EXFMT     PROTECT
033000100210     C* Immissione/Modifica/Copia
033100100210     C                   OTHER
033200100210     C                   EXFMT     FMTD1
033300100210     C                   ENDSL
033400100210     C* controllo abilitazione tasti funzione
033500100210     C*
033600100210     C                   EXSR      CTRKD1
033700100210     C*
033800100210     C     *IN99         IFEQ      *OFF
033900100210     C*
034000100210     C                   SELECT
034100100216     C* ROLLUP
034200160201     C**** $TASTO        WHENEQ    ROLLUP
034300160201     C****               MOVE      'D2'          $GEST
034400100210     C* F2=Decodifica
034500100210     C     $TASTO        WHENEQ    F02
034600100210     C                   EXSR      F02D1
034700100210     C* F3=Fine
034800100210     C     $TASTO        WHENEQ    F03
034900100210     C                   EXSR      F03D1
035000100210     C* F4=Ricerca
035100100210     C     $TASTO        WHENEQ    F04
035200100210     C                   EXSR      SEARCH
035300100210     C* F5=Refresh
035400100210     C     $TASTO        WHENEQ    F05
035500100210     C                   EXSR      F05D1
035600100210     C* F8=Successivo
035700100210     C     $TASTO        WHENEQ    F08
035800100210     C                   EXSR      F08D1
035900100210     C* F12=Ritorno
036000100210     C     $TASTO        WHENEQ    F12
036100100210     C                   EXSR      F12D1
036200120306R091 C* F13=Importi soglia
036300120306 "   C     $TASTO        WHENEQ    F13
036400120306R091 C                   EXSR      F13D1
036500100210     C* F24=Altri tasti
036600100210     C     $TASTO        WHENEQ    F24
036700100210     C                   EXSR      F24D1
036800100210     C*
036900100210     C                   OTHER
037000100210     C                   EXSR      CTRD1
037100100216     C     *IN99         IFEQ      *OFF
037200100216     C                   EXSR      CTRD2
037300100216     C     *IN99         IFEQ      *ON
037400100216     C* Spengo gli indicatori di errore perchè, nel caso si siano usati
037500100216     C* indicatori uguali in videate differenti, genererei conflitti
037600100216     C                   MOVE      *ALL'0'       IN5098           49
037700100216     C                   MOVEA     IN5098        *IN(50)
037800100216     C                   SETON                                        9996                       96
037900100216     C                   ENDIF
038000100216     C*
038100100216     C                   ENDIF
038200100216     C*
038300100210     C     $TASTO        IFEQ      F06
038400100210     C* eseguo operazioni per aggiornamento
038500100210     C     *IN99         IFEQ      *OFF
038600100210     C                   EXSR      GESAGG
038700100210     C                   ENDIF
038800100210     C                   ENDIF
038900100210     C*
039000100210     C                   ENDSL
039100100210     C*
039200100210     C                   ENDIF
039300100210     C*
039400100210     C                   ENDSR
039500100210     C/EJECT
039600100210     C************************************************************
039700100210     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
039800100210     C************************************************************
039900100210     C     CTRKD1        BEGSR
040000100210     C                   SETOFF                                       99
040100100210     C                   MOVEA     $FC1          $FC
040200100210     C                   EXSR      CTRKEY
040300100210     C     *IN99         IFEQ      *ON
040400100210     C                   SETON                                        97
040500100210     C                   ENDIF
040600100210     C                   ENDSR
040700100210     C************************************************************
040800100210     C* INIZIALIZZAZIONE VIDEATA DATI
040900100210     C************************************************************
041000100210     C     INZD1         BEGSR
041100100210     C*
041200100210     C                   CLEAR                   FMTD1
041300100216     C                   CLEAR                   FMTD2
041400100210     C*
041500100830     C                   move      xscon         d1on1
041600100830     C                   move      xscoff        d1off1
042300100210     C*
042400100210     C                   MOVE      ANPPAR        D1PAR
042500100210     C*
042600100210     C                   Z-ADD     1             $PIED1
042700100210     C*
042800100210     C     ANPOPZ        IFNE      '01'
042900100210     C                   EXSR      RIED1
043000100216     C                   EXSR      RIED2
043100100210     C                   EXSR      CTRD1
043200100216     C                   EXSR      CTRD2
043300100210     C                   MOVE      *ALL'0'       IN5098
043400100210     C                   MOVEA     IN5098        *IN(50)
043500100210     C*
043600100210     C                   ELSE
043700160226     C                   Eval      D1TpDtRif   = 'R'
043900100210     C*
044000100210     C                   ENDIF
044100100210     C*
044200100210     C                   ENDSR
044300100210     C/EJECT
044400100210     C************************************************************
044500100210     C* RIEMPIMENTO VIDEATA  D1
044600100210     C************************************************************
044700100210     C     RIED1         BEGSR
044800100210     C*
044900151023     C                   if        AttBLA11    = *On
045000100827     C                   MoveL     XScOn         D1AttBL
045100100311     C                   else
045200100827     C                   MoveL     XScOff        D1AttBL
045300100311     C                   endif
045400151023     C                   Eval      D1TpDtRif   = TpDtRifA11
046200100827     C*
046300151023     C                   Eval      D1BLPos     = BLPosA11
046400151023     C                   Eval      D1BLEscl    = BLEsclA11
046500151023     C                   Eval      D1BLServ    = BLServA11
046600151023     C                   Eval      D1BLBeni    = BLBeniA11
046601151023     C                   Eval      D1BeniSrv   = BeniSRVA11
047300100827     C*
047400151023     C                   Eval      D1IdProd    = IdProdA11
047401151203     C*
047402151203     C                   Eval      D1NoteVar   = NoteVarA11
047500100210     C*
047600100210     C                   ENDSR
047700100210     C/EJECT
047800100210     C************************************************************
047900100210     C* GESTIONE F02 VIDEO D1
048000100210     C************************************************************
048100100210     C     F02D1         BEGSR
048200100210     C*
048300100210     C* per decodificare la videata richiamo la routine dei ctrl
048400100210     C* e poi disattivo gli eventuali messaggi di errore
048500100210     C                   EXSR      CTRD1
048600100210     C*
048700100210     C                   MOVE      *ALL'0'       IN5098
048800100210     C                   MOVEA     IN5098        *IN(50)
048900100210     C*
049000100210     C                   ENDSR
049100100210     C/EJECT
049200100210     C************************************************************
049300100210     C* GESTIONE F03 VIDEO D1
049400100210     C************************************************************
049500100210     C     F03D1         BEGSR
049600100210     C*
049700100210     C                   MOVE      *ON           $FINE
049800100210     C* fine programma
049900100210     C                   EXSR      ENDPGM
050000100210     C*
050100100210     C                   ENDSR
050200100210     C/EJECT
050300100210     C************************************************************
050400100210     C* GESTIONE F05 VIDEO D1
050500100210     C************************************************************
050600100210     C     F05D1         BEGSR
050700100210     C*
050800100210     C                   MOVE      *ON           $INZD1
050900100210     C*
051000100210     C                   ENDSR
051100100210     C/EJECT
051200100210     C************************************************************
051300100210     C* GESTIONE F08 VIDEO D1
051400100210     C************************************************************
051500100210     C     F08D1         BEGSR
051600100210     C*
051700100210     C                   MOVE      *ON           $FINE
051800100210     C*
051900100210     C                   ENDSR
052000100210     C/EJECT
052100100210     C************************************************************
052200100210     C* GESTIONE F12 VIDEO D1
052300100210     C************************************************************
052400100210     C     F12D1         BEGSR
052500100210     C*
052600100210     C                   MOVE      *ON           $FINE
052700100210     C*
052800100210     C                   ENDSR
052900100210     C/EJECT
053000120306R091 C************************************************************
053100120306 "   C* GESTIONE F13 VIDEO D1
053200120306 "   C************************************************************
053300120306R091 C     F13D1         BEGSR
053400120306     C*
053500120306     C                   RESET                   SDGSD20DS
053600120306     C*
053700120306     C                   MOVE      xscsoc        SocietaD20
053800120306     C                   MOVEL     '02'          OPZD20
053900120306     C*
054000120306     C                   MOVEL     SDGSD20DS     KPJBU
054100120306     C                   CALL      'SDGSD20R'
054200120306     C                   PARM                    KPJBA
054300120306     C                   MOVEL     KPJBU         SDGSD20DS
054400120306     C* ritorno da PGM gestione
054500120306     C                   EXSR      RTND20
054600120306     C*
054700120306     C                   MOVE      *HIVAL        $LASTV
054800120306     C*
054900120306R091 C                   ENDSR
055000120306R091 C/EJECT
055100120306R091 C************************************************************
055200120306 "   C* GESTIONE RITORNO DA PGM importi soglia
055300120306 "   C************************************************************
055400120306R091 C     RTND20        BEGSR
055500120306     C*
055600120306     C* modo di ritorno
055700120306     C*
055800120306     C                   SELECT
055900120306     C* questi modi di utilizzo dei valori di ritorno dal
056000120306     C* pgm di manutenzione rcd di anagrafica sono delle
056100120306     C* proposte, normalmente sempre valide, ma modificabili
056200120306     C* per situazioni particolari
056300120306     C* 1 = F3
056400120306     C     RETD20        WHENEQ    '1'
056500120306     C                   MOVE      *ON           $FINE                          fine pgm
056600120306     C* 2 = F12
056700120306     C     RETD20        WHENEQ    '2'
056800120306     C* se il pgm di manutenzione ha prima una guida, è necessario anche
056900120306     c* questo test, altrimenti prevale il ret002 e il sfl non viene
057000120306     C* ricaricato
057100120306     C     OPRD20        ANDNE     '1'
057200120306     C*
057300120306     C                   ENDSL
057400120306     C*
057500120306     C*
057600120306     C* funzione non eseguibile per errore :
057700120306     C                   SELECT
057800120306     C* 1 = funzione richiamata chiusa in errore
057900120306     C     ERRD20        WHENEQ    '1'
058000120306     C                   SETON                                        9399
058100120306     C* 2 = funzione richiamata senza la dovuta autorizzazione
058200120306     C     ERRD20        WHENEQ    '2'
058300120306     C                   SETON                                        9499
058400120306     C*
058500120306     C                   ENDSL
058600120306     C*
058700120306R091 C                   ENDSR
058800120306R091 C/EJECT
058900100210     C************************************************************
059000100210     C* GESTIONE F24 VIDEO D1
059100100210     C************************************************************
059200100210     C     F24D1         BEGSR
059300100210     C*
059400100210     C                   MOVEA     $FX1          $FX
059500100210     C                   MOVEA     $FC1          $FC
059600100210     C                   MOVEA     $FR1          $FR
059700100210     C                   Z-ADD     $ULKD1        $ULK
059800100210     C                   EXSR      RIEKEY
059900100210     C                   Z-ADD     $ULK          $ULKD1            3 0
060000100210     C                   MOVEA     $FC           $FC1
060100100210     C                   MOVEL     $KEY1         Z1KE1
060200100210     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
060300100210     C                   MOVEL     $KEY2         Z1KE2
060400100210     C                   WRITE     FMTZ1
060500100210     C*
060600100210     C                   ENDSR
060700100210     C/EJECT
060800100210     C************************************************************
060900100210     C* CONTROLLO VIDEATA
061000100210     C************************************************************
061100100210     C     CTRD1         BEGSR
061200100210     C*
061300100210     C                   SETOFF                                       99
061400100210     C* non imposto pos. del cursore
061500100210     C                   Z-ADD     0             H1RIGA
061600100210     C                   Z-ADD     0             H1COLO
061700100216     C*
061800100210     C*
061900100210     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
062000100210     C*
062100100210     C     ANPOPZ        IFNE      '04'
062200100311     C*
062300100827     C                   EXSR      CtrAttBL
062400100311     C*
062500100827     C     D1AttBL       IFEQ      XScOn
062600100909     C*
062700100909     C* Tipo data di riferimento
062800100909     C                   EXSR      CtrTpDtRif
063500100907     C*
063600100907     C* Black list, pos.attributo
063700100827     C                   EXSR      CtrBLPos
063800100827     C*
063900100827     C* Valore per esclusione da elenchi
064000100827     C                   EXSR      CtrBLEscl
064001151023     C*
064002151023     C* Valore per beni
064003151023     C                   EXSR      CtrBLBeni
064100100827     C*
064200100827     C* Valore per servizi
064300100827     C                   EXSR      CtrBLServ
064301151023     C*
065002151023     C* valore  di default tra beni/servizi
065003151023     C                   EXSR      CtrBeniSrv
065004151023     C*
065100100827     C* Identificativo del produttore del software
065200100827     C                   EXSR      CtrIdProd
065201151203     C*
065202151203     C* gestione note di variazione
065203151203     C                   EXSR      CtrNoteVar
065300100210     C*
065400100210     C                   ENDIF
065500100311     C*
065600100311     C                   ENDIF
065700100210     C*
065800100210     C                   ENDSR
065900100210     C/EJECT
066000100311     C************************************************************
066100100311     C* Controllo flag per modulo attivato
066200100311     C************************************************************
066300100827     C     CtrAttBL      BEGSR
066400100311     C*
066500100827     C                   if        D1AttBL    <> XScOn   and
066600100827     C                             D1AttBL    <> XScOff
066700100827     C                   SetOn                                        5099
066800100311     C                   endif
066900100311     C*
067000100311     C                   ENDSR
067100100311     C/EJECT
067200100909     C***********************************************************
067300100909     C* Controllo il tipo data di riferimento
067400100909     C***********************************************************
067500100909     C     CtrTpDtRif    BEGSR
067600100909     C*
067700100909     C                   Clear                   X07ValDs
067800100909     C                   Clear                   D1TpDtRifD
067900100909     C*
068000100909     C                   Move      '1'           X07Ric
068100100909     C                   Move      *On           X07All
068200151023     C                   Movel     'SDGPA11S'    X07TRc
068300151023     C                   Movel     'TPDTRIFA11'  X07Cam
068400100909     C                   Movel     D1TpDtRif     X07VAL
068500100909     C*
068600100909     C                   Callb     'X07VALR'
068700100909     C                   Parm                    X07ValDs
068800100909     C*
068900100909     C     X07ERR        IFEQ      *Off
069000100909     C                   EVAL      D1TpDtRifD = X07Des
069100100909     C                   else
069200151023     C                   SetOn                                        5199
069300100909     C                   endif
069400100909     C*
069500100909     C                   ENDSR
072300100907     C/EJECT
072400100210     C************************************************************
072500100827     C* Controllo posizione  BLACK LIST
072600100210     C************************************************************
072700100827     C     CtrBLPos      BEGSR
072800100210     C*
072900100827     C                   if        D1BLPos     = *Zero
073000100906     C********           SetOn                                        5299
073100100210     C                   endif
073200100210     C*
073300100210     C                   ENDSR
073400100210     C/EJECT
073500100210     C************************************************************
073600100827     C* Controllo valore per esclusione da comunicazione
073700100210     C************************************************************
073800100827     C     CtrBLEscl     BEGSR
073900100210     C*
074000100827     C                   Clear                   D1BLEsclD
074100100210     C*
074200100906     C                   if        D1BLPos     > *Zero
074300100906     C*
074400100827     C                   if        D1BLEscl    = *Blanks
074500100827     C                   SetOn                                        5399
074600100210     C                   else
074700100210     C*
074800100827     C                   MoveL(P)  D1BLPos       Wrk6              6
074900100827     C                   Move      D1BLEscl      Wrk6
075000100210     C                   EXSR      CtrDef
075100100210     C*
075200100210     C                   if        XTBERR      = *On
075300100827     C                   SetOn                                        5499
075400100210     C                   else
075500100210     C                   MoveL     XTBUNI        ANG0A1DS
075600100827     C                   MoveL     Des0A1        D1BLEsclD
075700100210     C                   endif
075800100210     C*
075900100210     C                   endif
076000100210     C*
076100100906     C                   else
076200100906     C*
076300100906     C                   Eval      D1BLEscl    = *Blanks
076400100906     C                   endif
076500100906     C*
076600100210     C                   ENDSR
076700100210     C/EJECT
076800100827     C************************************************************
076900100827     C* Controllo valore per servizi
077000100827     C************************************************************
077100100827     C     CtrBLServ     BEGSR
077200100827     C*
077300100827     C                   Clear                   D1BLServD
077400100906     C*
077500100906     C                   if        D1BLPos     > *Zero
077600100827     C*
077700100827     C                   if        D1BLServ    = *Blanks
077800100827     C                   SetOn                                        5599
077900100827     C                   else
078000100827     C*
078100100827     C                   MoveL(P)  D1BLPos       Wrk6              6
078200100827     C                   Move      D1BLServ      Wrk6
078300100827     C                   EXSR      CtrDef
078400100827     C*
078500100827     C                   if        XTBERR      = *On
078600100827     C                   SetOn                                        5699
078700100827     C                   else
078800100827     C                   MoveL     XTBUNI        ANG0A1DS
078900100827     C                   MoveL     Des0A1        D1BLServD
079000100827     C                   endif
079100100827     C*
079200100827     C                   endif
079300100906     C*
079400100906     C                   else
079500100906     C*
079600100906     C                   Eval      D1BLServ    = *Blanks
079700100906     C*
079800100906     C                   endif
079900100827     C*
080000100827     C                   ENDSR
080100100827     C/EJECT
080200100827     C************************************************************
080300100827     C* Controllo valore per beni
080400100827     C************************************************************
080500100827     C     CtrBLBeni     BEGSR
080600100827     C*
080700100827     C                   Clear                   D1BLBeniD
080800100827     C*
080900100906     C                   if        D1BLPos     > *Zero
081000100906     C*
081100100827     C                   if        D1BLBeni    = *Blanks
081200100827     C                   SetOn                                        5799
081300100827     C                   else
081400100827     C*
081500100827     C                   MoveL(P)  D1BLPos       Wrk6              6
081600100827     C                   Move      D1BLBeni      Wrk6
081700100827     C                   EXSR      CtrDef
081800100827     C*
081900100827     C                   if        XTBERR      = *On
082000100827     C                   SetOn                                        5899
082100100827     C                   else
082200100827     C                   MoveL     XTBUNI        ANG0A1DS
082300100827     C                   MoveL     Des0A1        D1BLBeniD
082400100827     C                   endif
082500100827     C*
082600100827     C                   endif
082700100906     C*
082800100906     C                   else
082900100906     C*
083000100906     C                   Eval      D1BLBeni    = *Blanks
083100100827     C*
083200100906     C                   endif
083300100906     C*
083400100827     C                   ENDSR
083401151023     C/EJECT
083402151023     C************************************************************
083403151023     C* Controllo default tra beni e servizi
083404151023     C************************************************************
083405151023     C     CtrBeniSrv    BEGSR
083406151023     C*
083407151023     C*
083408151023     C                   Clear                   D1BeniSrvD
083409151023     C*
083410151023     C                   RESET                   X07VALDS
083411151023     C*
083412151023     C                   MOVE      '1'           X07RIC
083413151029     C                   MOVEL     'SDGPA11S  '  X07TRC
083414151029     C                   MOVEL     'BENISRVA11'  X07CAM
083415151023     C                   MOVEL     D1BeniSrv     X07VAL
083416151023     C                   MOVE      *ON           X07ALL
083417151023     C                   CALLB     'X07VALR'
083418151023     C                   PARM                    X07VALDS
083419151023     C     X07ERR        IFEQ      *Off
083420151023     C                   MOVEL     X07DES        D1BeniSrvD
083421151023     C                   ELSE
083422151023     C                   SETON                                        6199
083423151023     C                   ENDIF
083424151023     C*
083425151023     C                   ENDSR
083426151023     C/EJECT
084800100210     C************************************************************
084900100210     C* Controllo valori di default per attributo contabile indicato
085000100210     C************************************************************
085100100210     C     CtrDef        BEGSR
085200100210     C*
085300100210     C                   Clear                   SDGTABDS
085400100210     C*
085500100210     C                   Move      '1'           XTBRIC
085600100210     C                   Move      XSCSOC        XTBAZI
085700100210     C                   Move      *ZERO         XTBKEY
085800100210     C                   Move      '0A1'         XTBCOD
085900100210     C                   Move      Wrk6          XTBKey
086000100210     C*
086100100210     C                   CALL      'SDGTABDR'
086200100210     C                   PARM                    SDGTABDS
086300100827     C*
086400100827     C                   ENDSR
086500100827     C/EJECT
086600100827     C************************************************************
086700100827     C* Controllo Identificativo del produttore del software
086800100827     C************************************************************
086900100827     C     CtrIdProd     BEGSR
087000100827     C*
087100100827     C                   if        D1IdProd    = *Blank
087200100827     C                   Eval      D1IdProd    = XScCdF
087300100827     C                   endif
087400100827     C*
087500100827     C                   if        D1IdProd    = *Blank
087600100827     C                   SetOn                                        5999
087700100827     C                   endif
087800100210     C*
087900100210     C                   ENDSR
088000100210     C/EJECT
088001151203     C************************************************************
088002151203     C* Controllo gestione note di variazione
088003151203     C************************************************************
088004151203     C     CtrNoteVar    BEGSR
088005151203     C*
088006151203     C                   Clear                   D1NoteVarD
088007151203     C*
088008151203     C                   RESET                   X07VALDS
088009151203     C*
088010151203     C                   MOVE      '1'           X07RIC
088011151203     C                   MOVEL     'SDGPA11S  '  X07TRC
088012151203     C                   MOVEL     'NOTEVARA11'  X07CAM
088013151203     C                   MOVEL     D1NoteVar     X07VAL
088014151203     C                   MOVE      *ON           X07ALL
088015151203     C                   CALLB     'X07VALR'
088016151203     C                   PARM                    X07VALDS
088017151203     C     X07ERR        IFEQ      *Off
088018151203     C                   MOVEL     X07DES        D1NoteVarD
088019151203     C                   ELSE
088020151203     C                   SETON                                        6099
088021151203     C                   ENDIF
088022151203     C*
088023151203     C                   ENDSR
088024151203     C/EJECT
088100100216     C************************************************************
088200100216     C* GESTIONE VIDEO RECORD D2
088300100216     C************************************************************
088400100216     C     GESD2         BEGSR
088500100216     C*
088600100216     C* emissione piede videata se proveniente da altra
088700100216     C* salvataggio valore formato di provenienza
088800100216   B1C     $LASTV        IFNE      'D2'
088900100216     C                   WRITE     FMTT1
089000100216     C                   EXSR      GESPIE
089100100216     C                   MOVE      $LASTV        $LASTG
089200100216     C                   MOVE      'D2'          $LASTV
089300100216   E1C                   ENDIF
089400100216     C*
089500100216     C* se esistono errori sulla videata
089600100216     C* emetto la write del formato a indicatori spenti per vedere
089700100216     C* le eventuali decodifiche
089800100216     C     *IN99         IFEQ      *ON
089900100216     C     $GEST         ANDEQ     'D2'
090000100216     C                   MOVEA     *IN           WIN
090100100216     C                   MOVE      *ALL'0'       IN5098           49
090200100216     C                   MOVEA     IN5098        *IN(50)
090300100216     C                   WRITE     FMTD2
090400100216     C                   MOVEA     WIN           *IN
090500100216     C                   ENDIF
090600100216     C*
090700100216     C                   SELECT
090800100216     C* Annullamento
090900100216     C     ANPOPZ        WHENEQ    '04'
091000100216     C* Visualizzazione
091100100216     C     ANPOPZ        OREQ      '05'
091200100216     C                   WRITE     FMTD2
091300100216     C                   EXFMT     PROTECT
091400100216     C* Immissione/Modifica/Copia
091500100216     C                   OTHER
091600100216     C                   EXFMT     FMTD2
091700100216     C                   ENDSL
091800100216     C*
091900100216     C* azzero la posizione del cursore
092000100216     C                   Z-ADD     0             H1RIGA
092100100216     C                   Z-ADD     0             H1COLO
092200100216     C*
092300100216     C* controllo abilitazione tasti funzione
092400100216     C                   EXSR      CTRKD1
092500100216     C*
092600100216   B1C     *IN99         IFEQ      *OFF
092700100216     C*
092800100216   B2C                   SELECT
092900100216     C* ROLDOWN
093000100216     C     $TASTO        WHENEQ    ROLDWN
093100100216     C                   MOVE      'D1'          $GEST
093200100216     C* F3=Fine
093300100216     C     $TASTO        WHENEQ    F03
093400100216     C                   EXSR      F03D1
093500100216     C* F4=Ricerca
093600100216     C     $TASTO        WHENEQ    F04
093700100216     C                   EXSR      SEARCH
093800100216     C* F5=Refresh
093900100216     C     $TASTO        WHENEQ    F05
094000100216     C                   EXSR      F05D2
094100100216     C* F8=Successivo
094200100216     C     $TASTO        WHENEQ    F08
094300100216     C                   EXSR      F08D1
094400100216     C* F12=Ritorno
094500100216     C     $TASTO        WHENEQ    F12
094600100216     C                   EXSR      F12D1
094700151023     C* F13=Importi soglia
094800151023     C     $TASTO        WHENEQ    F13
094900151023     C                   EXSR      F13D1
095000100216     C* F24=Altri tasti
095100100216     C     $TASTO        WHENEQ    F24
095200100216     C                   EXSR      F24D2
095300100216     C*
095400100216     C                   OTHER
095500100216     C                   EXSR      CTRD2
095600100216     C     *IN99         IFEQ      *OFF
095700100216     C                   EXSR      CTRD1
095800100216     C     *IN99         IFEQ      *On
095900100216     C* Spengo gli indicatori di errore per il controllo su altri schermi
096000100216     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
096100100216     C* videate
096200100216     C                   MOVE      *ALL'0'       IN5098           49
096300100216     C                   MOVEA     IN5098        *IN(50)
096400100216     C                   SETON                                        9996                       96
096500100216     C                   ENDIF
096600100216     C*
096700100216     C                   ENDIF
096800100216     C*
096900100216     C*
097000100216     C     $TASTO        IFEQ      F06
097100100216     C     *IN99         ANDEQ     *OFF
097200100216     C* eseguo operazioni del dopo-aggiornamento
097300100216     C     *IN99         IFEQ      *OFF
097400100216     C                   EXSR      GESAGG
097500100216     C                   ENDIF
097600100216     C                   ENDIF
097700100216     C*
097800100216     C                   ENDSL
097900100216     C*
098000100216     C                   ENDIF
098100100216     C*
098200100216     C                   ENDSR
098300100216     C/EJECT
098400100216     C************************************************************
098500100216     C* RIEMPIMENTO VIDEATA D2
098600100216     C************************************************************
098700100216     C     RIED2         BEGSR
098800100216     C*
098900100216     C*
099000100216     C                   ENDSR
099100100216     C/EJECT
099200100216     C************************************************************
099300100216     C* GESTIONE F05 VIDEO D2
099400100216     C* il tasto F5 su qualsiasi videata provoca il refresh di tutte
099500100216     C* e il riposizionamento alla prima
099600100216     C************************************************************
099700100216     C     F05D2         BEGSR
099800100216     C*
099900100216     C                   MOVE      *ON           $INZD1
100000100216     C                   MOVE      'D1'          $GEST
100100100216     C*
100200100216     C                   ENDSR
100300100216     C/EJECT
100400100216     C************************************************************
100500100216     C* GESTIONE F24 VIDEO D2
100600100216     C************************************************************
100700100216     C     F24D2         BEGSR
100800100216     C*
100900100216     C                   MOVEA     $FX1          $FX
101000100216     C                   MOVEA     $FC1          $FC
101100100216     C                   MOVEA     $FR1          $FR
101200100216     C                   Z-ADD     $ULKD1        $ULK
101300100216     C                   EXSR      RIEKEY
101400100216     C                   Z-ADD     $ULK          $ULKD1            3 0
101500100216     C                   MOVEA     $FC           $FC1
101600100216     C                   MOVEL     $KEY1         Z1KE1
101700100216     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
101800100216     C                   MOVEL     $KEY2         Z1KE2
101900100216     C                   WRITE     FMTZ1
102000100216     C*
102100100216     C                   ENDSR
102200100216     C/EJECT
102300100216     C************************************************************
102400100216     C* CONTROLLO VIDEATA  D2
102500100216     C************************************************************
102600100216     C     CTRD2         BEGSR
102700100216     C*
102800100216     C                   SETOFF                                       99
102900100216     C*
103000100216     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
103100100216     C*
103200100216     C     ANPOPZ        IFNE      '04'
103300100827     C     D1AttBL       andEQ     XScOn
103400100216     C*
103500100216     C                   ENDIF
103600100216     C*
103700100216     C                   ENDSR
103800100216     C/EJECT
103900100210     C************************************************************
104000100210     C* RIEMPIMENTO DS per aggiornamento
104100100210     C************************************************************
104200100210     C     RIEDAT        BEGSR
104300100311     C*
104400100827     C                   if        D1AttBL     = XScOn
104500151023     C                   Move      *On           AttBLA11
104600100311     C                   else
104700151023     C                   Move      *Off          AttBLA11
104800100311     C                   endif
104900151023     C                   Eval      TpDtRifA11  = D1TpDtRif
105700100210     C*
105800151023     C                   Eval      BLPosA11    = D1BLPos
105801151023     C                   Eval      BLEsclA11   = D1BLEscl
106000151023     C                   Eval      BLServA11   = D1BLServ
106100151023     C                   Eval      BLBeniA11   = D1BLBeni
106101151023     C                   Eval      BENISrvA11  = D1BeniSrv
106200100901     C*
106800151023     C                   Eval      IDProdA11   = D1IdProd
106801151203     C*
106802151203     C                   Eval      NoteVarA11  = D1NoteVar
106900100210     C*
107000100210     C                   ENDSR
107100100210     C/EJECT
107200100210     C************************************************************
107300100210     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
107400100210     C************************************************************
107500100210     C     GESAGG        BEGSR
107600100210     C*
107700100210     C* Riempio campi
107800100210     C                   EXSR      RIEDAT
107900100210     C                   MOVEA     $DT           ANPDAT
108000100210     C*
108100100210     C* segnalo al pgm chiamante l'avvenuta conferma
108200100210     C                   MOVE      *ON           ANPOPR
108300100210     C*
108400100210     C                   MOVE      *ON           $FINE
108500100210     C*
108600100210     C                   ENDSR
108700100210     C/EJECT
108800100210     C************************************************************
108900100210     C* OPERAZIONI INIZIALI
109000100210     C************************************************************
109100100210     C     *INZSR        BEGSR
109200100210     C*
109300100210     C* Reperimento parametri
109400100210     C*
109500100210     C     *ENTRY        PLIST
109600100210     C                   PARM                    ANPAR
109700100210     C                   PARM                    KPJBA
109800100210     C*
109900100210     C* Reperimento dati società
110000100210     C*
110100100210     C                   MOVEL     'SOC001'      TIPXSC
110200100210     C                   MOVEL     *BLANK        SOCXSC
110300100210     C                   EXSR      REPSOC
110400100210     C     RTNXSC        IFNE      '1'
110500100210     C                   MOVEL     XSOCDS        SOC001
110600100210     C                   ELSE
110700100210     C                   RETURN
110800100210     C                   ENDIF
110900100210     C*
111000100210     C* Reperimento tasti di funzione
111100100210     C*
111200100210     C                   EXSR      REPKEY
111300100210     C*
111400100210     C                   ENDSR
111500100210     C/EJECT
111600100210     C************************************************************
111700100210     C* REPERIMENTO TASTI DI FUNZIONE
111800100210     C************************************************************
111900100210     C     REPKEY        BEGSR
112000100210     C                   MOVEA     $FM1          $FM
112100100210     C                   EXSR      REPKKK
112200100210     C                   MOVEA     $FX           $FX1
112300100210     C*
112400100210     C                   ENDSR
112500100210     C/EJECT
112600100210     C************************************************************
112700100210     C* REPERIMENTO TASTI DI FUNZIONE
112800100210     C************************************************************
112900100210     C     REPKKK        BEGSR
113000100210     C*
113100100210     C                   CLEAR                   $FX
113200100210     C     1             DO        24            X
113300100210     C*
113400100210     C     $FM(X)        IFNE      *BLANK
113500100210     C     $FC(X)        ANDNE     '0'
113600100210     C*
113700100210     C                   MOVEL     $FM(X)        IDMSG
113800100210     C                   CALLB     'XRTVM'
113900100210     C                   PARM                    IDMSG            27
114000100210     C                   PARM                    TXTMSG          100
114100100210     C                   PARM                    ERRMSG            1
114200100210     C*
114300100210     C     ERRMSG        IFNE      *ON
114400100210     C                   MOVEL     TXTMSG        $FX(X)
114500100210     C                   ELSE
114600100210     C                   MOVEL     *ALL' '       $FX(X)
114700100210     C                   ENDIF
114800100210     C*
114900100210     C                   ENDIF
115000100210     C*
115100100210     C                   ENDDO
115200100210     C*
115300100210     C                   ENDSR
115400100210     C/EJECT
115500100210     C************************************************************
115600100210     C* Reperimento dati società
115700100210     C************************************************************
115800100210     C     REPSOC        BEGSR
115900100210     C*
116000100210     C                   CALL      'SDGSOCDR'
116100100210     C                   PARM                    TIPXSC            6
116200100210     C                   PARM                    SOCXSC            3
116300100210     C                   PARM                    CDSXSC            9 0
116400100210     C                   PARM                    MODXSC            3
116500100210     C                   PARM      *BLANKS       RTNXSC            1
116600100210     C                   PARM                    XSOCDS
116700100210     C                   PARM                    KPJBA
116800100210     C*
116900100210     C                   ENDSR
117000100210     C/EJECT
117100100210     C************************************************************
117200100210     C* INIZIALIZZAZIONE VARIABILI
117300100210     C************************************************************
117400100210     C     INZVAR        BEGSR
117500100210     c*
117600100210     C                   MOVEA     ANPDAT        $DT
117700100210     C*
117800100210     C* Definizione dei tasti funzione utilizzabili a pgm
117900100210     C*
118000100210     C                   EXSR      DEFOPZ
118100100210     C*
118200100210     C* Variabili per gestione videate
118300100210     C*
118400100210     C                   MOVE      'D1'          $GEST                          fmt in gestione
118500100210     C                   MOVE      *OFF          $FINE                          fine pgm
118600100210     C                   MOVE      *ON           $INZD1                         inizializzione D1
118700100210     C                   MOVE      *BLANK        $LASTG                         fmt di provenienza
118800100210     C                   MOVE      *BLANK        $LASTV                         ultima emissione
118900100210     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
119000100210     C*
119100100210     C* Variabili appoggio
119200100210     C*
119300100210     C                   Z-ADD     0             CURR
119400100210     C                   Z-ADD     0             CURC
119500100210     C*
119600100210     C* Valorizzazione campi univoci testate
119700100210     C*
119800100210     C                   CLEAR                   FMTT1
119900100210     C                   MOVEL     XSCDSI        NOMDIT
120000100210     C                   MOVEL     KCDAZ         NOMAZN
120100100210     C                   MOVEL     KnSif         NOMSIF
120200100210     C* recupero il sottotitolo (se c'è)
120300100210     C                   MOVEL     *BLANK        T1TIT
120400100210     C     SOTTIT        IFNE      *BLANK
120500100210     C                   MOVEL     SOTTIT        IDMSG
120600100210     C                   CALLB     'XRTVM'
120700100210     C                   PARM                    IDMSG            27
120800100210     C                   PARM                    TXTMSG          100
120900100210     C                   PARM                    ERRMSG            1
121000100210     C                   PARM      *ON           CTRMSG            1            centratura
121100100210     C                   PARM      30            LENMSG            3 0          lun output.
121200100210     C     ERRMSG        IFNE      *ON
121300100210     C                   MOVEL     TXTMSG        T1TIT
121400100210     C                   ELSE
121500100210     C                   MOVEL     *ALL'?'       T1TIT
121600100210     C                   ENDIF
121700100210     C                   ENDIF
121800100210     C*
121900100210     C* Inizializzazione/Decodifica prima videata
122000100210     C*
122100100210     C                   EXSR      INZD1
122200100210     C                   MOVE      *OFF          $INZD1
122300100210     C*
122400100210     C                   ENDSR
122500100210     C/EJECT
122600100210     C************************************************************
122700100210     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
122800100210     C************************************************************
122900100210     C     DEFOPZ        BEGSR
123000100210     C*
123100100210     C* Questa routine deve valorizzare la schiera $FC1/$FR1
123200100210     C* con S o N a seconda della utilizzabilità o meno
123300100210     C* del tasto funzione correlato
123400100210     C*
123500100210     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
123600100210     C* di compilazione
123700100210     C                   RESET                   $FC1
123800100210     C                   RESET                   $FR1
123900100210     C*
124000100210     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
124100100210     C* invalido alcune opzioni
124200100210     C                   SELECT
124300100210     C* Se pgm richiamato in interrogazione disattivo :
124400100210     C     ANPOPZ        WHENEQ    '05'
124500100210     C* F5=Ripristino
124600100210     C                   Z-ADD     5             X
124700100210     C                   MOVE      '0'           $FC1(X)
124800100210     C* Se pgm richiamato in annullamento disattivo :
124900100210     C     ANPOPZ        WHENEQ    '04'
125000100210     C* F5=Ripristino
125100100210     C                   Z-ADD     5             X
125200100210     C                   MOVE      '0'           $FC1(X)
125300100210     C*
125400100210     C                   OTHER
125500100210     C*
125600100210     C                   ENDSL
125700100210     C*
125800100210     C                   ENDSR
125900100210     C************************************************************
126000100210     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
126100100210     C************************************************************
126200100210     C     DEFCAM        BEGSR
126300100210     C*
126400100210     C* Variabili per gestione videate
126500100210     C*
126600100210     C                   MOVE      *BLANK        $GEST             2            fmt in gestione
126700100210     C                   MOVE      *BLANK        $FINE             1            fine pgm
126800100210     C                   MOVE      *BLANK        $INZD1            1            inizializzione D1
126900100210     C                   MOVE      *BLANK        $LASTG            2            fmt di provenienza
127000100210     C                   MOVE      *BLANK        $LASTV            2            ultima emissione
127100100210     C                   Z-ADD     0             $PIED1            3 0          piede da emett. D1
127200100210     C*
127300100210     C* Variabili appoggio
127400100210     C*
127500100210     C                   Z-ADD     0             CURR              2 0
127600100210     C                   Z-ADD     0             CURC              2 0
127700100210     C                   MOVE      *ZEROS        WIN              99
127800100210     C                   MOVE      *BLANK        WTBERR            1
127900100210     C*
128000100210     C* Indici
128100100210     C*
128200100210     C                   Z-ADD     0             X                 3 0
128300100210     C                   Z-ADD     0             Y                 3 0
128400100210     C*
128500100210     C                   ENDSR
128600100210     C/EJECT
128700100210     C************************************************************
128800100210     C* CONTROLLO ABILITAZIONE TASTI
128900100210     C************************************************************
129000100210     C     CTRKEY        BEGSR
129100100210     C                   SELECT
129200100210     C     $TASTO        WHENEQ    F01
129300100210     C     $FC(1)        IFEQ      '0'
129400100210     C                   SETON                                        99
129500100210     C                   ENDIF
129600100210     C     $TASTO        WHENEQ    F02
129700100210     C     $FC(2)        IFEQ      '0'
129800100210     C                   SETON                                        99
129900100210     C                   ENDIF
130000100210     C     $TASTO        WHENEQ    F03
130100100210     C     $FC(3)        IFEQ      '0'
130200100210     C                   SETON                                        99
130300100210     C                   ENDIF
130400100210     C     $TASTO        WHENEQ    F04
130500100210     C     $FC(4)        IFEQ      '0'
130600100210     C                   SETON                                        99
130700100210     C                   ENDIF
130800100210     C     $TASTO        WHENEQ    F05
130900100210     C     $FC(5)        IFEQ      '0'
131000100210     C                   SETON                                        99
131100100210     C                   ENDIF
131200100210     C     $TASTO        WHENEQ    F06
131300100210     C     $FC(6)        IFEQ      '0'
131400100210     C                   SETON                                        99
131500100210     C                   ENDIF
131600100210     C     $TASTO        WHENEQ    F07
131700100210     C     $FC(7)        IFEQ      '0'
131800100210     C                   SETON                                        99
131900100210     C                   ENDIF
132000100210     C     $TASTO        WHENEQ    F08
132100100210     C     $FC(8)        IFEQ      '0'
132200100210     C                   SETON                                        99
132300100210     C                   ENDIF
132400100210     C     $TASTO        WHENEQ    F09
132500100210     C     $FC(09)       IFEQ      '0'
132600100210     C                   SETON                                        99
132700100210     C                   ENDIF
132800100210     C     $TASTO        WHENEQ    F10
132900100210     C     $FC(10)       IFEQ      '0'
133000100210     C                   SETON                                        99
133100100210     C                   ENDIF
133200100210     C     $TASTO        WHENEQ    F11
133300100210     C     $FC(11)       IFEQ      '0'
133400100210     C                   SETON                                        99
133500100210     C                   ENDIF
133600100210     C     $TASTO        WHENEQ    F12
133700100210     C     $FC(12)       IFEQ      '0'
133800100210     C                   SETON                                        99
133900100210     C                   ENDIF
134000100210     C     $TASTO        WHENEQ    F13
134100100210     C     $FC(13)       IFEQ      '0'
134200100210     C                   SETON                                        99
134300100210     C                   ENDIF
134400100210     C     $TASTO        WHENEQ    F14
134500100210     C     $FC(14)       IFEQ      '0'
134600100210     C                   SETON                                        99
134700100210     C                   ENDIF
134800100210     C     $TASTO        WHENEQ    F15
134900100210     C     $FC(15)       IFEQ      '0'
135000100210     C                   SETON                                        99
135100100210     C                   ENDIF
135200100210     C     $TASTO        WHENEQ    F16
135300100210     C     $FC(16)       IFEQ      '0'
135400100210     C                   SETON                                        99
135500100210     C                   ENDIF
135600100210     C     $TASTO        WHENEQ    F17
135700100210     C     $FC(17)       IFEQ      '0'
135800100210     C                   SETON                                        99
135900100210     C                   ENDIF
136000100210     C     $TASTO        WHENEQ    F18
136100100210     C     $FC(18)       IFEQ      '0'
136200100210     C                   SETON                                        99
136300100210     C                   ENDIF
136400100210     C     $TASTO        WHENEQ    F19
136500100210     C     $FC(19)       IFEQ      '0'
136600100210     C                   SETON                                        99
136700100210     C                   ENDIF
136800100210     C     $TASTO        WHENEQ    F20
136900100210     C     $FC(20)       IFEQ      '0'
137000100210     C                   SETON                                        99
137100100210     C                   ENDIF
137200100210     C     $TASTO        WHENEQ    F21
137300100210     C     $FC(21)       IFEQ      '0'
137400100210     C                   SETON                                        99
137500100210     C                   ENDIF
137600100210     C     $TASTO        WHENEQ    F22
137700100210     C     $FC(22)       IFEQ      '0'
137800100210     C                   SETON                                        99
137900100210     C                   ENDIF
138000100210     C     $TASTO        WHENEQ    F23
138100100210     C     $FC(23)       IFEQ      '0'
138200100210     C                   SETON                                        99
138300100210     C                   ENDIF
138400100210     C     $TASTO        WHENEQ    F24
138500100210     C     $FC(24)       IFEQ      '0'
138600100210     C                   SETON                                        99
138700100210     C                   ENDIF
138800100210     C                   ENDSL
138900100210     C                   ENDSR
139000100210     C/EJECT
139100100210     C************************************************************
139200100210     C* RIEMPIMENTO TASTI FUNZIONE
139300100210     C************************************************************
139400100210     C     RIEKEY        BEGSR
139500100210     C*
139600100210     C* Input :
139700100210     C* - $FX = schiera con tutti i testi disponibili
139800100210     C* - $FC = schiere con i flag di validità associati ai testi
139900100210     C* - $FR = schiere con i flag di REVERSE IMAGE
140000100210     C* - $ULK = ultimo testo caricato
140100100210     C* - $L01 = Lunghezza prima riga
140200100210     C* - $L02 = Lunghezza seconda riga
140300100210     C* Output :
140400100210     C* - $ULK
140500100210     C* - $KEY1= 1° riga di tasti funzione
140600100210     C* - $KEY2= 2° riga di tasti funzione
140700100210     C*
140800100210     C                   CALLB     'XKEYMSG'
140900100210     C* input
141000100210     C                   PARM                    $FX
141100100210     C                   PARM                    $FC
141200100210     C                   PARM                    $FR
141300100210     C                   PARM                    $ULK              3 0
141400100210     C* output
141500100210     C                   PARM                    $KEY1            79
141600100210     C                   PARM                    $KEY2            79
141700100210     C* input
141800100210     C                   PARM      79            $L01              3 0
141900100210     C                   PARM      79            $L02              3 0
142000100210     C*
142100100210     C                   PARM                    $ALLFUNCT       480
142200100210     C*
142300100210     C                   ENDSR
142400100210     C************************************************************
142500120306R091 C* sostituito PROMSG    *LIBL     KEY0013  13
142600120306R091 C*        con SDGMSG    *LIBL     SDK0016  13
142700100210**   C
14280010021001SDGMSG    *LIBL     SDC0002    inserimento
14290010021002PROMSG    *LIBL     COS0A02    modifica
14300010021003PROMSG    *LIBL     COS0003    copia
14310010021004PROMSG    *LIBL     COS0004    annullamento
14320010021005PROMSG    *LIBL     COS0005    visualizzazione
143300100210** TASTI DI FUNZIONE  D1
143400100210PROMSG    *LIBL     KEY0001  01
143500100210PROMSG    *LIBL     KEY0A02  02
143600100210PROMSG    *LIBL     KEY0003  03
143700100210PROMSG    *LIBL     KEY0004  04
143800100210PROMSG    *LIBL     KEY0005  05
143900100210PROMSG    *LIBL     KEY0006  06
144000100210PROMSG    *LIBL     KEY0007  07
144100100210PROMSG    *LIBL     KEY0008  08
144200100210PROMSG    *LIBL     KEY0009  09
144300100210PROMSG    *LIBL     KEY0010  10
144400100210PROMSG    *LIBL     KEY0011  11
144500100210PROMSG    *LIBL     KEY0012  12
144600120306SDGMSG    *LIBL     SDK0016  13        ## R091
144700100210PROMSG    *LIBL     KEY0014  14
144800100210PROMSG    *LIBL     KEY0015  15
144900100210PROMSG    *LIBL     KEY0016  16
145000100210SDGMSG    *LIBL     SDK0001  17
145100100210PROMSG    *LIBL     KEY0018  18
145200100210PROMSG    *LIBL     KEY0019  19
145300100210PROMSG    *LIBL     KEYA020  20
145400100210PROMSG    *LIBL     KEYA021  21
145500100210PROMSG    *LIBL     KEYA022  22
145600100210PROMSG    *LIBL     KEYA023  23
145700100210PROMSG    *LIBL     KEYA024  24
145800100210**  ABILITAZIONE D1
1459001002100N  01
1460001002100N  02
1461001002100N  03
1462001002101N  04
1463001002101N  05
1464001002101N  06
1465001002100N  07
1466001002100N  08
1467001002100N  09
1468001002100N  10
1469001002100N  11
1470001002101N  12
1471001203061N  13      ## R091 attivato tasto F13=Importi soglia
1472001002100N  14
1473001002100N  15
1474001002100N  16
1475001002100N  17
1476001002100N  18
1477001002100N  19
1478001002100N  20
1479001002100N  21
1480001002100N  22
1481001002100N  23
1482001002100N  24   Lasciare sempre impostata a '0' la colonna $FC
