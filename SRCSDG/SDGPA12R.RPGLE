000100100210     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200100210     H*PARMS CVTOPT(*NONE)
000300100210     H DECEDIT('0,') DATEDIT(*DMY.)
000400100210      *
000500170529      *       Parametri per SPESOMETRO trimestrale
000600170529      *
000700170529R258  * Questo pgm è stato creato con il task R258 Comunicaz. Spesometro Trimestrale
000800100210      *
000900100210     F*----------------------------------------------------*
001000170529     FSDGPA12D  CF   E             WORKSTN
001100170529     F                                     RENAME(PA12D1:FMTD1)
001200170529     F                                     RENAME(PA12D2:FMTD2)
001300170529     F                                     RENAME(PA12T1:FMTT1)
001400170529     F                                     RENAME(PA12Z1:FMTZ1)
001500100210     F                                     INFDS(DSFMT)
001600100210     D*----------------------------------------------------*
001700100210     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
001800100210     D $ID             S             27    DIM(5) ALT($OZ)
001900100210     D* Schiere per la gestione dei tasti di funzione
002000100210     D $FM             S             27    DIM(24)
002100100210     D $FX             S             50    DIM(24)
002200100210     D $FC             S              1    DIM(24)
002300100210     D $FR             S              1    DIM(24)
002400100210     D* Video D1
002500100210     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
002600100210     D $FX1            S             50    DIM(24) ALT($FM1)
002700100210     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
002800100210     D $FR1            S              1    DIM(24) ALT($FC1)
002900100210     D*
003000100210     D*----------------------------------------------------*
003100100210     D* Passaggio Parametri
003200100210     D KPJBA         E DS
003300100210     D*-------------
003400100210     D* Sottotitolo di testata
003500100210     D SOTTIT          C                   CONST('               -
003600100210     D                                                 ')
003700100210     D*-------------
003800100210     D* Dati di ambiente ottenuti da SDGSOCDR
003900100210     D SOC001        E DS                  EXTNAME(SDGSOCDS )
004000100210     D*-------------
004100100210     D* DS Interna per dati di output di SDGSOCDR
004200100210     D XSOCDS          DS          1000
004300110901     D*-------------
004400110901     D* Ds per tabella codici attributi contabili
004500110901     D ANG0A1DS      E DS                  INZ
004600110901     D*-------------
004700110901     D ANA964DS      E DS                  INZ
004800110901     D*-------------
004900110901     D* Controlli validità per valore di una legenda
005000110901     D X07VALDS      E DS                  Inz
005100130823     D*-------------
005200130823     D* Parametri per ricerca codice ANATB00F
005300130823     D XTABDS        E DS                  inz
005400110901     D*-------------
005500110901     D* Parametri per controllo codice ANATB00F
005600110901     D SDGTABDS      E DS                  INZ
005700170530     D*-------------
005800170530     D* tabella destinazione dati aggiuntivi
005900170530     D SDGHC4DS      E DS                  INZ
006000170530     D*-------------
006100110901     D*-------------
006200100210     D* Ds esterna dati
006300170529     D SDPA12        E DS                  EXTNAME(SDGPA12DS)
006400100210     D  $DT                    1   2000
006500100210     D                                     DIM(2000)
006600100210     D*-------------
006700100210     D* Parametri ricevuti
006800100210     D ANPAR         E DS                  EXTNAME(ANPARDS)
006900100210     D  ANPSOC       E                     EXTFLD(ANPSOCIETA)
007000100210     D  ANPPAR       E                     EXTFLD(ANPPARAME)
007100100210     D  ANPDAT       E                     EXTFLD(ANPDATI)
007200100210     D                                     DIM(2000)
007300100210     D  ANPERR       E                     EXTFLD(ANPERRORE)
007400100210     D*-------------
007500100210     D DSFMT           DS
007600100210     D  $TASTO               369    369
007700100210     D  NRG                  370    370
007800100210     D  NCL                  371    371
007900100210     D*-------------
008000100210     D* posizione cursore
008100100210     D CURSOR          DS
008200100210     D  RIRI                   1      2B 0 INZ
008300100210     D  R$$                    2      2
008400100210     D  COCO                   3      4B 0 INZ
008500100210     D  C$$                    4      4
008600170704     D*-------------
008700170704     D* Regole di estrazione
008800170704     D SDGSG70DS     E DS
008900170704     D  RETG70       E                     INZ('0')
009000170704     D  OPRG70       E                     INZ('0')
009100170704     D  ERRG70       E                     INZ('0')
009200100210     D*-------------
009300100210     D* Costanti
009400100210     D*-------------
009500100210     D* Tasti di funzione
009600100210     D F01             C                   CONST(X'31')
009700100210     D F02             C                   CONST(X'32')
009800100210     D F03             C                   CONST(X'33')
009900100210     D F04             C                   CONST(X'34')
010000100210     D F05             C                   CONST(X'35')
010100100210     D F06             C                   CONST(X'36')
010200100210     D F07             C                   CONST(X'37')
010300100210     D F08             C                   CONST(X'38')
010400100210     D F09             C                   CONST(X'39')
010500100210     D F10             C                   CONST(X'3A')
010600100210     D F11             C                   CONST(X'3B')
010700100210     D F12             C                   CONST(X'3C')
010800100210     D F13             C                   CONST(X'B1')
010900100210     D F14             C                   CONST(X'B2')
011000100210     D F15             C                   CONST(X'B3')
011100100210     D F16             C                   CONST(X'B4')
011200100210     D F17             C                   CONST(X'B5')
011300100210     D F18             C                   CONST(X'B6')
011400100210     D F19             C                   CONST(X'B7')
011500100210     D F20             C                   CONST(X'B8')
011600100210     D F21             C                   CONST(X'B9')
011700100210     D F22             C                   CONST(X'BA')
011800100210     D F23             C                   CONST(X'BB')
011900100210     D F24             C                   CONST(X'BC')
012000100210     D ENTER           C                   CONST(X'F1')
012100100210     D ROLDWN          C                   CONST(X'F4')
012200100210     D ROLLUP          C                   CONST(X'F5')
012300100210     D*-------------
012400100210     D* Reperimento nome PGM
012500100210     D STATUS         SDS           333
012600100210     D  DSPGM            *PROC
012700100210     C*----------------------------------------------------*
012800100210     C*                MAIN LINE PROGRAM
012900100210     C*----------------------------------------------------*
013000100210     C*
013100100210     C                   EXSR      INZVAR
013200100210     C*
013300100210     C     $FINE         DOWEQ     *OFF
013400100210     C     $GEST         CASEQ     'D1'          GESD1
013500100216     C     $GEST         CASEQ     'D2'          GESD2
013600100210     C                   ENDCS
013700100210     C                   ENDDO
013800100210     C*
013900100210     C                   EXSR      ENDPGM
014000100210     C/EJECT
014100100210     C************************************************************
014200100210     C* FINE PROGRAMMA
014300100210     C************************************************************
014400100210     C     ENDPGM        BEGSR
014500100210     C*
014600100210     C                   SETON                                            RT
014700100210     C                   RETURN
014800100210     C*
014900100210     C                   ENDSR
015000100210     C/EJECT
015100100210     C************************************************************
015200100210     C* RICERCHE
015300100210     C************************************************************
015400100210     C     SEARCH        BEGSR
015500100210     C*
015600100210     C* determino Riga/Colonna del cursore
015700100210     C                   MOVE      NRG           R$$
015800100210     C                   MOVE      NCL           C$$
015900100210     C                   Z-ADD     RIRI          CURR
016000100210     C                   Z-ADD     COCO          CURC
016100100210     C*
016200100210     C* I campi che al momento dell'F4 risultano protetti non devono
016300100210     C* essere abilitati alla funzione di ricerca
016400100210     C*
016500100827     C                   SELECT
016600170706     C*
016700170706     C*
016800170706     C* Tipo data riferimento clienti
016900170706     C                   When      H1NMFL       = 'D1DTRIFC '
017000170706     C                   EXSR      F4_DtRifC
017100170706     C*
017200170706     C*
017300170530     C*
017400170530     C*  Destinazione dati anagrafici aggiuntivi
017500170530     C* (Sede, Stabile Organizzazione, Rappresentante fiscale)
017600170530     C     H1NMFL        WHENEQ    'D1DESTS  '
017700170530     C     H1NMFL        OREQ      'D1DESTO  '
017800170530     C     H1NMFL        OREQ      'D1DESTR  '
017900170530     C                   EXSR      F4_Dest
018000170530     C*
018100130823     C*
018200130823     C* Valore per conto con operazioni di servizi
018300130823     C* Valore per conto con operazioni di beni
018400130823     C                   When      H1NMFL       = 'D1BENI   '  or
018500130823     C                             H1NMFL       = 'D1SERVIZI'
018600130823     C                   EXSR      F4_ValBenSrv
018700130823     C*
018800130823     C* Valore di default
018900130823     C                   When      H1NMFL       = 'D1BENISRV'
019000130823     C                   EXSR      F4_BeniSrv
019100130823     C*
019200100210     C* ricerca non abilitata in questa posizione
019300100210     C                   OTHER
019400100210     C                   SETON                                        9899
019500100210     C*
019600100210     C                   ENDSL
019700110901     C*
019800100210     C* imposto pos. del cursore
019900100210     C                   Z-ADD     CURR          H1RIGA
020000100210     C                   Z-ADD     CURC          H1COLO
020100100210     C                   MOVE      *HIVAL        $LASTV
020200170530     C*
020300100210     C                   ENDSR
020400170530      /EJECT
020500170706     C************************************************************
020600170706     C* Ricerca valore per tipo data riferimento clienti
020700170706     C************************************************************
020800170706     C     F4_DtRifc     BEGSR
020900170706     C*
021000170706     C                   RESET                   ANA964DS
021100170706     C*
021200170706     C                   Move      '01'          Opz964
021300170706     C                   MOVEL     'SDGPA12S  '  TRC964
021400170707     C                   MOVEL     'DTRIFCA12 '  CAM964
021500170706     C                   MOVE      *ON           ALL964
021600170706     C                   MOVEL     ANA964DS      KPJBU
021700170706     C                   CALL      'ANA964R'
021800170706     C                   PARM                    KPJBA
021900170706     C                   MOVEL     KPJBU         ANA964DS
022000170706     C     RET964        IFEQ      *BLANK
022100170706     C                   MOVEL     VAL964        D1DtRifC
022200170706     C                   ENDIF
022300170706     C*
022400170706     C                   ENDSR
022500170706      /EJECT
022600170530     C************************************************************
022700170530     C* Ricerca valori per Destinazione dati anagrafici aggiuntivi
022800170530     C************************************************************
022900170530     C     F4_Dest       BEGSR
023000170530     C*
023100170530     C                   RESET                   XTABDS
023200170530     C*
023300170530     C                   MOVE      '01'          XTAOPZ
023400170530     C                   MOVE      XSCSOC        XTAAZI
023500170530     C                   MOVE      XSCLIN        XTALIN
023600170530     C                   Z-ADD     CURR          XTARIG
023700170530     C                   Z-ADD     CURC          XTACOL
023800170530     C                   CLEAR                   KPJBU
023900170530     C                   MOVEL     XTABDS        KPJBU
024000170530     C                   CALL      'SDGHC4R'
024100170530     C                   PARM                    KPJBA
024200170530     C                   MOVEL     KPJBU         XTABDS
024300170530     C*
024400170530     C                   if        XTaErr     <> '1'  and
024500170530     C                             XTaKey     <> *Blank
024600170530     C                   Select
024700170530     C     H1NMFL        WHENEQ    'D1DESTS  '
024800170530     C                   Move      XTaKey        D1DestS
024900170530     C     H1NMFL        WHENEQ    'D1DESTO  '
025000170530     C                   Move      XTaKey        D1DestO
025100170530     C     H1NMFL        WHENEQ    'D1DESTR  '
025200170530     C                   Move      XTaKey        D1DestR
025300170530     C                   endSl
025400170530     C*
025500170530     C                   endif
025600170530     C*
025700170530     C                   ENDSR
025800170530     C/EJECT
025900130823     C************************************************************
026000130823     C* Ricerca valori per attributo contabile
026100130823     C************************************************************
026200130823     C     F4_ValBenSrv  BEGSR
026300130823     C*
026400130823     C                   RESET                   XTABDS
026500130823     C*
026600130823     C                   MOVE      '01'          XTAOPZ
026700130823     C                   MOVE      XSCSOC        XTAAZI
026800130823     C                   MOVE      XSCLIN        XTALIN
026900130823     C                   Z-ADD     CURR          XTARIG
027000130823     C                   Z-ADD     CURC          XTACOL
027100130823     C                   MOVE      *ZERO         XTaKey
027200130823     C*
027300130823     C                   Move      D1PosAtt      XTaPos
027400130823     C*
027500130823     C                   Select
027600130823     C* Valore per conto con operazioni di beni
027700130823     C                   When      H1NMFL       = 'D1BENI   '
027800130823     C                   MoveL(P)  D1Beni        Wrk6              6
027900130823     C* Valore per conto con operazioni di servizi
028000130823     C                   When      H1NMFL       = 'D1SERVIZI'
028100130823     C                   MoveL(P)  D1Servizi     Wrk6              6
028200130823     C                   endSL
028300130823     C*
028400130823     C                   Move      Wrk6          XTaKey
028500130823     C*
028600130823     C                   MoveL     XTABDS        KPJBU
028700130823     C                   CALL      'ANG0A1R'
028800130823     C                   PARM                    KPJBA
028900130823     C*
029000130823     C                   MoveL     KPJBU         XTABDS
029100130823     C                   if        XTARet     <> '2'
029200130823     C*
029300130823     C                   Select
029400130823     C* Valore per conto con operazioni di beni
029500130823     C                   When      H1NMFL       = 'D1BENI  '
029600130823     C                   Move      XTAKEY        D1Beni
029700130823     C* Valore per conto con operazioni di servizi
029800130823     C                   When      H1NMFL       = 'D1SERVIZI'
029900130823     C                   Move      XTAKEY        D1Servizi
030000130823     C                   endSL
030100130823     C*
030200130823     C                   endif
030300130823     c*
030400130823     C                   ENDSR
030500130823     C/EJECT
030600130823     C************************************************************
030700130823     C* Ricerca default per beni/servizi
030800130823     C************************************************************
030900130823     C     F4_BeniSrv    BEGSR
031000130823     C*
031100130823     C                   RESET                   ANA964DS
031200130823     C*
031300130823     C                   Move      '01'          Opz964
031400170529     C                   MOVEL     'SDGPA12S  '  TRC964
031500170529     C                   MOVEL     'BENISRVA12'  CAM964
031600130823     C                   MOVE      *ON           ALL964
031700130823     C                   MOVEL     ANA964DS      KPJBU
031800130823     C                   CALL      'ANA964R'
031900130823     C                   PARM                    KPJBA
032000130823     C                   MOVEL     KPJBU         ANA964DS
032100130823     C     RET964        IFEQ      *BLANK
032200130823     C                   MOVEL     VAL964        D1BeniSrv
032300130823     C                   ENDIF
032400130823     C*
032500130823     C                   ENDSR
032600130823     C/EJECT
032700100210     C************************************************************
032800100210     C* GESTIONE EMISSIONE TASTI FUNZIONE
032900100210     C************************************************************
033000100210     C     GESPIE        BEGSR
033100100210     C*
033200100210     C                   SELECT
033300100210     C*
033400100210     C     $GEST         WHENEQ    'D1'
033500100210     C     $ULKD1        IFEQ      0
033600100210     C* riempimento opzioni
033700100210     C                   EXSR      F24D1
033800100210     C                   ELSE
033900100210     C                   WRITE     FMTZ1
034000100210     C                   ENDIF
034100100210     C*
034200100210     C     $GEST         WHENEQ    'D2'
034300100210     C     $ULKD1        IFEQ      0
034400100210     C* riempimento opzioni
034500100216     C                   EXSR      F24D2
034600100210     C                   ELSE
034700100210     C                   WRITE     FMTZ1
034800100210     C                   ENDIF
034900100210     C                   ENDSL
035000100210     C*
035100100210     C                   ENDSR
035200100210     C/EJECT
035300100210     C************************************************************
035400100210     C* GESTIONE VIDEO RECORD D1
035500100210     C************************************************************
035600100210     C     GESD1         BEGSR
035700100216     C*
035800100210     C* inizializzazione videata
035900100210     C     $INZD1        IFEQ      *ON
036000100210     C                   EXSR      INZD1
036100100210     C                   MOVE      *OFF          $INZD1
036200100210   1EC                   ENDIF
036300100210     C* emissione piede videata se proveniente da altra
036400100210     C* salvataggio valore formato di provenienza
036500100210   1BC     $LASTV        IFNE      'D1'
036600100210     C*
036700100210     C                   WRITE     FMTT1
036800100210     C                   EXSR      GESPIE
036900100210     C                   MOVE      $LASTV        $LASTG
037000100210     C                   MOVE      'D1'          $LASTV
037100100210   1EC                   ENDIF
037200100210     C*
037300100210     C* se esistono errori sulla videata
037400100210     C* emetto la write del formato a indicatori spenti per vedere
037500100210     C* le eventuali decodifiche
037600100210     C     *IN99         IFEQ      *ON
037700100210     C     $GEST         ANDEQ     'D1'
037800100210     C                   MOVEA     *IN           WIN
037900100210     C                   MOVE      *ALL'0'       IN5098           49
038000100210     C                   MOVEA     IN5098        *IN(50)
038100100210     C                   WRITE     FMTD1
038200100210     C                   MOVEA     WIN           *IN
038300100210     C                   ENDIF
038400100210     C*
038500100210     C                   SELECT
038600100210     C* Annullamento
038700100210     C     ANPOPZ        WHENEQ    '04'
038800100210     C* Visualizzazione
038900100210     C     ANPOPZ        OREQ      '05'
039000100210     C                   WRITE     FMTD1
039100100210     C                   EXFMT     PROTECT
039200100210     C* Immissione/Modifica/Copia
039300100210     C                   OTHER
039400100210     C                   EXFMT     FMTD1
039500100210     C                   ENDSL
039600100210     C* controllo abilitazione tasti funzione
039700100210     C*
039800100210     C                   EXSR      CTRKD1
039900100210     C*
040000100210     C     *IN99         IFEQ      *OFF
040100100210     C*
040200100210     C                   SELECT
040300100216     C* ROLLUP
040400111024     C**** $TASTO        WHENEQ    ROLLUP
040500111024     C****               MOVE      'D2'          $GEST
040600100210     C* F2=Decodifica
040700100210     C     $TASTO        WHENEQ    F02
040800100210     C                   EXSR      F02D1
040900100210     C* F3=Fine
041000100210     C     $TASTO        WHENEQ    F03
041100100210     C                   EXSR      F03D1
041200100210     C* F4=Ricerca
041300100210     C     $TASTO        WHENEQ    F04
041400100210     C                   EXSR      SEARCH
041500100210     C* F5=Refresh
041600100210     C     $TASTO        WHENEQ    F05
041700100210     C                   EXSR      F05D1
041800100210     C* F8=Successivo
041900100210     C     $TASTO        WHENEQ    F08
042000100210     C                   EXSR      F08D1
042100100210     C* F12=Ritorno
042200100210     C     $TASTO        WHENEQ    F12
042300100210     C                   EXSR      F12D1
042400170704     C* F15=Codici Iso Stati
042500170704     C     $TASTO        WHENEQ    F15
042600170704     C                   EXSR      F15D1
042700170704     C* F19=Impostazioni regole
042800170704     C     $TASTO        WHENEQ    F19
042900170704     C                   EXSR      F19D1
043000100210     C* F24=Altri tasti
043100100210     C     $TASTO        WHENEQ    F24
043200100210     C                   EXSR      F24D1
043300100210     C*
043400100210     C                   OTHER
043500100210     C                   EXSR      CTRD1
043600100216     C     *IN99         IFEQ      *OFF
043700111024     C****               EXSR      CTRD2
043800100216     C     *IN99         IFEQ      *ON
043900100216     C* Spengo gli indicatori di errore perchè, nel caso si siano usati
044000100216     C* indicatori uguali in videate differenti, genererei conflitti
044100100216     C                   MOVE      *ALL'0'       IN5098           49
044200100216     C                   MOVEA     IN5098        *IN(50)
044300100216     C                   SETON                                        9996                       96
044400100216     C                   ENDIF
044500100216     C*
044600100216     C                   ENDIF
044700100216     C*
044800100210     C     $TASTO        IFEQ      F06
044900100210     C* eseguo operazioni per aggiornamento
045000100210     C     *IN99         IFEQ      *OFF
045100100210     C                   EXSR      GESAGG
045200100210     C                   ENDIF
045300100210     C                   ENDIF
045400100210     C*
045500100210     C                   ENDSL
045600100210     C*
045700100210     C                   ENDIF
045800100210     C*
045900100210     C                   ENDSR
046000100210     C/EJECT
046100100210     C************************************************************
046200100210     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
046300100210     C************************************************************
046400100210     C     CTRKD1        BEGSR
046500100210     C                   SETOFF                                       99
046600100210     C                   MOVEA     $FC1          $FC
046700100210     C                   EXSR      CTRKEY
046800100210     C     *IN99         IFEQ      *ON
046900100210     C                   SETON                                        97
047000100210     C                   ENDIF
047100100210     C                   ENDSR
047200100210     C************************************************************
047300100210     C* INIZIALIZZAZIONE VIDEATA DATI
047400100210     C************************************************************
047500100210     C     INZD1         BEGSR
047600100210     C*
047700100210     C                   CLEAR                   FMTD1
047800100216     C                   CLEAR                   FMTD2
047900100210     C*
048000100830     C                   move      xscon         d1on1
048100100830     C                   move      xscoff        d1off1
048200100210     C*
048300100210     C                   MOVE      ANPPAR        D1PAR
048400100210     C*
048500100210     C                   Z-ADD     1             $PIED1
048600100210     C*
048700100210     C     ANPOPZ        IFNE      '01'
048800100210     C                   EXSR      RIED1
048900100216     C                   EXSR      RIED2
049000100210     C                   EXSR      CTRD1
049100111024     C****               EXSR      CTRD2
049200100210     C                   MOVE      *ALL'0'       IN5098
049300100210     C                   MOVEA     IN5098        *IN(50)
049400100210     C*
049500100210     C                   ELSE
049600100210     C*
049700100210     C                   ENDIF
049800100210     C*
049900100210     C                   ENDSR
050000100210     C/EJECT
050100100210     C************************************************************
050200100210     C* RIEMPIMENTO VIDEATA  D1
050300100210     C************************************************************
050400100210     C     RIED1         BEGSR
050500100210     C*
050600170529     C                   if        AttCSTA12   = *On
050700170529     C                   MoveL     XScOn         D1AttCST
050800100311     C                   else
050900170529     C                   MoveL     XScOff        D1AttCST
051000100311     C                   endif
051001170707     C*
051002170707     C                   Eval      D1DtRifc    = DtRifcA12
051200170529     C                   Eval      D1PosAtt    = PosAttA12
051300170530     C                   Eval      D1Dests     = DestsA12
051400170530     C                   Eval      D1Desto     = DestoA12
051500170530     C                   Eval      D1Destr     = DestrA12
051600170529     C                   Eval      D1Beni      = BeniA12
051700170529     C                   Eval      D1Servizi   = ServiziA12
051800170529     C                   Eval      D1BeniSrv   = BeniSrvA12
051900130904     C*
052000100210     C                   ENDSR
052100100210     C/EJECT
052200100210     C************************************************************
052300100210     C* GESTIONE F02 VIDEO D1
052400100210     C************************************************************
052500100210     C     F02D1         BEGSR
052600100210     C*
052700100210     C* per decodificare la videata richiamo la routine dei ctrl
052800100210     C* e poi disattivo gli eventuali messaggi di errore
052900100210     C                   EXSR      CTRD1
053000100210     C*
053100100210     C                   MOVE      *ALL'0'       IN5098
053200100210     C                   MOVEA     IN5098        *IN(50)
053300100210     C*
053400100210     C                   ENDSR
053500100210     C/EJECT
053600100210     C************************************************************
053700100210     C* GESTIONE F03 VIDEO D1
053800100210     C************************************************************
053900100210     C     F03D1         BEGSR
054000100210     C*
054100100210     C                   MOVE      *ON           $FINE
054200100210     C* fine programma
054300100210     C                   EXSR      ENDPGM
054400100210     C*
054500100210     C                   ENDSR
054600100210     C/EJECT
054700100210     C************************************************************
054800100210     C* GESTIONE F05 VIDEO D1
054900100210     C************************************************************
055000100210     C     F05D1         BEGSR
055100100210     C*
055200100210     C                   MOVE      *ON           $INZD1
055300100210     C*
055400100210     C                   ENDSR
055500100210     C/EJECT
055600100210     C************************************************************
055700100210     C* GESTIONE F08 VIDEO D1
055800100210     C************************************************************
055900100210     C     F08D1         BEGSR
056000100210     C*
056100100210     C                   MOVE      *ON           $FINE
056200100210     C*
056300100210     C                   ENDSR
056400100210     C/EJECT
056500100210     C************************************************************
056600100210     C* GESTIONE F12 VIDEO D1
056700100210     C************************************************************
056800100210     C     F12D1         BEGSR
056900100210     C*
057000100210     C                   MOVE      *ON           $FINE
057100100210     C*
057200100210     C                   ENDSR
057300170704     C/EJECT
057400170704     C************************************************************
057500170704     C* GESTIONE F15 VIDEO D1
057600170704     C************************************************************
057700170704     C     F15D1         BEGSR
057800170704     C*
057900170704     C* tabella dei codici Iso degli stati
058000170801     C                   Reset                   XTABDS
058100170704     C*
058200170704     C                   Move      '02'          XTaOpz
058300170704     C                   Move      XScSoc        XTaAzi
058400170704     C                   Move      XScLin        XTaLin
058500170704     C                   Z-add     Curr          XTaRig
058600170704     C                   Z-add     Curc          XTaCol
058700170704     C                   Move      *Blank        XTaKey
058800170704     C*
058900170704     C                   Clear                   KPJBU
059000170801     C                   Movel     XTABDS        KPJBU
059100170704     C*
059200170704     C                   CALL      'SDGHD4R'
059300170704     C                   PARM                    KPJBA
059400170704     C*
059500170704     C                   MOVE      *HIVAL        $LASTV
059600170704     C*
059700170704     C                   ENDSR
059800100210     C/EJECT
059900170704     C************************************************************
060000170704     C* GESTIONE F19 VIDEO D1
060100170704     C************************************************************
060200170704     C     F19D1         BEGSR
060300170704     C*
060400170704     C* regole
060500170704     C*
060600170704     C*
060700170704     C                   Reset                   SDGSG70DS
060800170704     C*
060900170704     C                   MoveL     XScSoc        SocietaG70
061000170704     C*
061100170704     C* Spesometro Trimestrale
061200170704     C                   Eval      TpComG70    = 'T'
061300170704     C*
061400170704     C                   movel     SDGSG70DS     Kpjbu
061500170704     C*
061600170704     C                   CALL      'SDGSG70R'
061700170704     C                   PARM                    Kpjba
061800170704     C*
061900170704     C                   MOVE      *HIVAL        $LASTV
062000170704     C*
062100170704     C                   ENDSR
062200170704     C/EJECT
062300100210     C************************************************************
062400100210     C* GESTIONE F24 VIDEO D1
062500100210     C************************************************************
062600100210     C     F24D1         BEGSR
062700100210     C*
062800100210     C                   MOVEA     $FX1          $FX
062900100210     C                   MOVEA     $FC1          $FC
063000100210     C                   MOVEA     $FR1          $FR
063100100210     C                   Z-ADD     $ULKD1        $ULK
063200100210     C                   EXSR      RIEKEY
063300100210     C                   Z-ADD     $ULK          $ULKD1            3 0
063400100210     C                   MOVEA     $FC           $FC1
063500100210     C                   MOVEL     $KEY1         Z1KE1
063600100210     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
063700100210     C                   MOVEL     $KEY2         Z1KE2
063800100210     C                   WRITE     FMTZ1
063900100210     C*
064000100210     C                   ENDSR
064100100210     C/EJECT
064200100210     C************************************************************
064300100210     C* CONTROLLO VIDEATA
064400100210     C************************************************************
064500100210     C     CTRD1         BEGSR
064600100210     C*
064700100210     C                   SETOFF                                       99
064800100210     C* non imposto pos. del cursore
064900100210     C                   Z-ADD     0             H1RIGA
065000100210     C                   Z-ADD     0             H1COLO
065100100216     C*
065200100210     C*
065300100210     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
065400100210     C*
065500100210     C     ANPOPZ        IFNE      '04'
065600100311     C*
065700170529     C                   EXSR      CtrAttCST
065800110901     C*
065900170529     C     D1AttCST      IFEQ      XScOn
066000170706     C*
066100170706     C* Tipo data riferimento clienti
066200170706     C                   EXSR      Ctrdtrifc
066300170530     C*
066400170530     C* Destinazione dati aggiuntivi
066500170530     C* per Sede
066600170530     C* i dati relativi alla sede sono opzionali, mentre
066700170530     C* quelli riferiti alla stabile organizzazione
066800170530     C* e al rappresentante fiscale vanno valorizzati solo se presenti
066900170530     C* I dati della Sede vanno indicati ANCHE SE NON Italia
067000170530     C                   EXSR      CtrDestS
067100170530     C*
067200170530     C* per Stabile Organizzazione
067300170530     C* gli elementi indicati in questa sezione devono
067400170530     C* essere valorizzati nei soli
067500170530     C* casi in cui il cedente/prestatore è un soggetto NON residente e ha effettuato la/le
067600170530     C* transazione/i oggetto del documento tramite l?organizzazione residente sul territorio
067700170530     C* nazionale. Si riferiscono alla stabile organizzazione in Italia.
067800170530     C                   EXSR      CtrDestO
067900170530     C*
068000170530     C* per Rappresentante Fiscale
068100170530     C* gli elementi indicati in questa sezione devono
068200170530     C* essere valorizzati qualora il cedente/prestatore si avvalga di
068300170530     C* un rappresentante fiscale in Italia
068400170530     C* ai sensi del DPR 633 del 1972 e successive modifiche ed integrazioni
068500170530     C                   EXSR      CtrDestR
068600170530     C*
068700130823     C*
068800130823     C* Valore per beni
068900130823     C                   EXSR      CtrBeni
069000130823     C*
069100130823     C* Valore per servizi
069200130823     C                   EXSR      CtrServizi
069300130823     C*
069400130823     C* valrore di default tra beni/servizi
069500130823     C                   EXSR      CtrBeniSrv
069600130904     C*
069700130823     C                   ENDIF
069800100311     C*
069900100311     C                   ENDIF
070000100210     C*
070100100210     C                   ENDSR
070200100210     C/EJECT
070300170706     C************************************************************
070400170706     C* Controllo tipo data riferimento per i clienti
070500170706     C************************************************************
070600170706     C     CtrDtRifC     BEGSR
070700170706     C*
070800170706     C*
070900170706     C                   Clear                   D1DtRifCD
071000170706     C*
071100170706     C                   RESET                   X07VALDS
071200170706     C*
071300170706     C                   MOVE      '1'           X07RIC
071400170706     C                   MOVEL     'SDGPA12S  '  X07TRC
071500170706     C                   MOVEL     'DTRIFCA12 '  X07CAM
071600170706     C                   MOVEL     D1DtRifc      X07VAL
071700170706     C                   MOVE      *ON           X07ALL
071800170706     C                   CALLB     'X07VALR'
071900170706     C                   PARM                    X07VALDS
072000170706     C     X07ERR        IFEQ      *Off
072100170706     C                   MOVEL     X07DES        D1DtRifCD
072200170706     C                   ELSE
072300170706     C                   SETON                                        5199
072400170706     C                   ENDIF
072500170706     C*
072600170706     C                   ENDSR
072700170706     C/EJECT
072800100311     C************************************************************
072900100311     C* Controllo flag per modulo attivato
073000100311     C************************************************************
073100170529     C     CtrAttCST     BEGSR
073200100311     C*
073300170529     C                   if        D1AttCST   <> XScOn   and
073400170529     C                             D1AttCST   <> XScOff
073500100827     C                   SetOn                                        5099
073600100311     C                   endif
073700100311     C*
073800100311     C                   ENDSR
073900170530     C/EJECT
074000170530     C************************************************************
074100170530     C* Controllo destinazione Sede
074200170530     C************************************************************
074300170530     C     CtrDestS      BEGSR
074400170530     C*
074500170530     C                   Clear                   D1DestSD
074600170530     C*
074700170530     C                   Clear                   SDGHC4DS
074800170530     C*
074900170530     C                   Clear                   SDGTABDS
075000170530     C*
075100170704     C                   If        D1Dests <> *Blanks
075200170704     C*
075300170530     C                   Eval      XTbRic      = '1'
075400170530     C                   Eval      XTbAzi      = XScSoc
075500170530     C                   Eval      XTbKey      = *Zero
075600170530     C                   Eval      XTbCod      = 'HC4'
075700170530     C                   Move      D1DestS       XTbKey
075800170530     C*
075900170530     C                   CALL      'SDGTABDR'
076000170530     C                   PARM                    SDGTABDS
076100170530     C*
076200170530     C                   if        XTbErr     <> '0'
076300170530     C                   SetOn                                        6099
076400170530     C*
076500170530     C                   else
076600170530     C                   MoveL     XTbUni        SDGHC4DS
076700170530     C                   MoveL     DesHC4        D1DestSD
076800170704     C*
076900170704     C                   endif
077000170704     C*
077100170704     C                   else
077200170530     C* "Generica"
077300170530     C                   Eval      IDMSG = 'PROMSG    *LIBL     COS1529'
077400170530     C                   EXSR      PrendiMsg
077500170530     C                   if        ERRMSG     <> *ON
077600170530     C                   MoveL     TXTMSG        D1DestSD
077700170530     C                   else
077800170530     C                   MoveL     *ALL'?'       D1DestSD
077900170530     C                   endif
078000170530     C*
078100170530     C                   endif
078200170530     C*
078300170530     C                   ENDSR
078400170530     C/EJECT
078500170530     C************************************************************
078600170530     C* Controllo destinazione Stabile Organizzazione
078700170530     C************************************************************
078800170530     C     CtrDestO      BEGSR
078900170530     C*
079000170530     C                   Clear                   D1DestOD
079100170530     C*
079200170530     C                   Clear                   SDGHC4DS
079300170530     C*
079400170530     C                   Clear                   SDGTABDS
079500170530     C*
079600170530     C                   Eval      XTbRic      = '1'
079700170530     C                   Eval      XTbAzi      = XScSoc
079800170530     C                   Eval      XTbKey      = *Zero
079900170530     C                   Eval      XTbCod      = 'HC4'
080000170530     C                   Move      D1DestO       XTbKey
080100170530     C*
080200170530     C                   CALL      'SDGTABDR'
080300170530     C                   PARM                    SDGTABDS
080400170530     C*
080500170530     C                   if        XTbErr     <> '0'
080600170530     C                   SetOn                                        6199
080700170530     C*
080800170530     C                   else
080900170530     C                   MoveL     XTbUni        SDGHC4DS
081000170530     C                   MoveL     DesHC4        D1DestOD
081100170530     C                   endif
081200170530     C*
081300170530     C                   ENDSR
081400170530     C/EJECT
081500170530     C************************************************************
081600170530     C* Controllo destinazione Rappresentante Fiscale
081700170530     C************************************************************
081800170530     C     CtrDestR      BEGSR
081900170530     C*
082000170530     C                   Clear                   D1DestRD
082100170530     C*
082200170530     C                   Clear                   SDGHC4DS
082300170530     C*
082400170530     C                   Clear                   SDGTABDS
082500170530     C*
082600170530     C                   Eval      XTbRic      = '1'
082700170530     C                   Eval      XTbAzi      = XScSoc
082800170530     C                   Eval      XTbKey      = *Zero
082900170530     C                   Eval      XTbCod      = 'HC4'
083000170530     C                   Move      D1DestR       XTbKey
083100170530     C*
083200170530     C                   CALL      'SDGTABDR'
083300170530     C                   PARM                    SDGTABDS
083400170530     C*
083500170530     C                   if        XTbErr     <> '0'
083600170530     C                   SetOn                                        6299
083700170530     C*
083800170530     C                   else
083900170530     C                   MoveL     XTbUni        SDGHC4DS
084000170530     C                   MoveL     DesHC4        D1DestRD
084100170530     C                   endif
084200170530     C*
084300170530     C                   ENDSR
084400170530     C/EJECT
084500170530     C************************************************************
084600170530     C* Reperimento testo messaggi
084700170530     C************************************************************
084800170530     C     PrendiMsg     BEGSR
084900170530     C*
085000170530     C                   CALLB     'XRTVM'
085100170530     C                   PARM                    IDMSG            27
085200170530     C                   PARM                    TXTMSG          100
085300170530     C                   PARM                    ERRMSG            1
085400170530     C*
085500170530     C                   ENDSR
085600100311     C/EJECT
085700130823     C************************************************************
085800130823     C* Controllo valore per beni
085900130823     C************************************************************
086000130823     C     CtrBeni       BEGSR
086100130823     C*
086200130823     C                   Clear                   D1BeniD
086300130823     C*
086400130823     C                   if        D1PosAtt    > *Zero
086500130823     C*
086600130823     C                   if        D1Beni      = *Blanks
086700130823     C                   SetOn                                        5799
086800130823     C                   else
086900130823     C*
087000130823     C                   MoveL(P)  D1PosAtt      Wrk6              6
087100130823     C                   Move      D1Beni        Wrk6
087200130823     C                   EXSR      CtrValAtt
087300130823     C*
087400130823     C                   if        XTBERR      = *On
087500130823     C                   SetOn                                        5899
087600130823     C                   else
087700130823     C                   MoveL     XTBUNI        ANG0A1DS
087800130823     C                   MoveL     Des0A1        D1BeniD
087900130823     C                   endif
088000130823     C*
088100130823     C                   endif
088200130823     C*
088300130823     C                   else
088400130823     C*
088500130823     C                   Eval      D1Beni      = *Blanks
088600130823     C*
088700130823     C                   endif
088800130823     C*
088900130823     C                   ENDSR
089000130823     C/EJECT
089100130823     C************************************************************
089200130823     C* Controllo valore per servizi
089300130823     C************************************************************
089400130823     C     CtrServizi    BEGSR
089500130823     C*
089600130823     C                   Clear                   D1ServizD
089700130823     C*
089800130823     C                   if        D1PosAtt    > *Zero
089900130823     C*
090000130823     C                   if        D1Servizi   = *Blanks
090100130823     C                   SetOn                                        5599
090200130823     C                   else
090300130823     C*
090400130823     C                   MoveL(P)  D1PosAtt      Wrk6              6
090500130823     C                   Move      D1Servizi     Wrk6
090600130823     C                   EXSR      CtrValAtt
090700130823     C*
090800130823     C                   if        XTBERR      = *On
090900130823     C                   SetOn                                        5699
091000130823     C                   else
091100130823     C                   MoveL     XTBUNI        ANG0A1DS
091200130823     C                   MoveL     Des0A1        D1ServizD
091300130823     C                   endif
091400130823     C*
091500130823     C                   endif
091600130823     C*
091700130823     C                   else
091800130823     C*
091900130823     C                   Eval      D1Servizi   = *Blanks
092000130823     C*
092100130823     C                   endif
092200130823     C*
092300130823     C                   ENDSR
092400130823     C/EJECT
092500130823     C************************************************************
092600130823     C* Controllo valore attributo contabile
092700130823     C************************************************************
092800130823     C     CtrValAtt     BEGSR
092900130823     C*
093000130823     C                   Clear                   SDGTABDS
093100130823     C*
093200130823     C                   Move      '1'           XTBRIC
093300130823     C                   Move      XSCSOC        XTBAZI
093400130823     C                   Move      *ZERO         XTBKEY
093500130823     C                   Move      '0A1'         XTBCOD
093600130823     C                   Move      Wrk6          XTBKey
093700130823     C*
093800130823     C                   CALL      'SDGTABDR'
093900130823     C                   PARM                    SDGTABDS
094000130823     C*
094100130823     C                   ENDSR
094200130823     C/EJECT
094300130823     C************************************************************
094400130823     C* Controllo default tra beni e servizi
094500130823     C************************************************************
094600130823     C     CtrBeniSrv    BEGSR
094700130823     C*
094800130823     C*
094900130823     C                   Clear                   D1BeniSrvD
095000130823     C*
095100130823     C                   RESET                   X07VALDS
095200130823     C*
095300130823     C                   MOVE      '1'           X07RIC
095400170529     C                   MOVEL     'SDGPA12S  '  X07TRC
095500170529     C                   MOVEL     'BENISRVA12'  X07CAM
095600130823     C                   MOVEL     D1BeniSrv     X07VAL
095700130823     C                   MOVE      *ON           X07ALL
095800130823     C                   CALLB     'X07VALR'
095900130823     C                   PARM                    X07VALDS
096000130823     C     X07ERR        IFEQ      *Off
096100130823     C                   MOVEL     X07DES        D1BeniSrvD
096200130823     C                   ELSE
096300130823     C                   SETON                                        5999
096400130823     C                   ENDIF
096500130823     C*
096600130823     C                   ENDSR
096700130904     C/EJECT
096800100216     C************************************************************
096900100216     C* GESTIONE VIDEO RECORD D2
097000100216     C************************************************************
097100100216     C     GESD2         BEGSR
097200100216     C*
097300100216     C* emissione piede videata se proveniente da altra
097400100216     C* salvataggio valore formato di provenienza
097500100216   B1C     $LASTV        IFNE      'D2'
097600100216     C                   WRITE     FMTT1
097700100216     C                   EXSR      GESPIE
097800100216     C                   MOVE      $LASTV        $LASTG
097900100216     C                   MOVE      'D2'          $LASTV
098000100216   E1C                   ENDIF
098100100216     C*
098200100216     C* se esistono errori sulla videata
098300100216     C* emetto la write del formato a indicatori spenti per vedere
098400100216     C* le eventuali decodifiche
098500100216     C     *IN99         IFEQ      *ON
098600100216     C     $GEST         ANDEQ     'D2'
098700100216     C                   MOVEA     *IN           WIN
098800100216     C                   MOVE      *ALL'0'       IN5098           49
098900100216     C                   MOVEA     IN5098        *IN(50)
099000100216     C                   WRITE     FMTD2
099100100216     C                   MOVEA     WIN           *IN
099200100216     C                   ENDIF
099300100216     C*
099400100216     C                   SELECT
099500100216     C* Annullamento
099600100216     C     ANPOPZ        WHENEQ    '04'
099700100216     C* Visualizzazione
099800100216     C     ANPOPZ        OREQ      '05'
099900100216     C                   WRITE     FMTD2
100000100216     C                   EXFMT     PROTECT
100100100216     C* Immissione/Modifica/Copia
100200100216     C                   OTHER
100300100216     C                   EXFMT     FMTD2
100400100216     C                   ENDSL
100500100216     C*
100600100216     C* azzero la posizione del cursore
100700100216     C                   Z-ADD     0             H1RIGA
100800100216     C                   Z-ADD     0             H1COLO
100900100216     C*
101000100216     C* controllo abilitazione tasti funzione
101100100216     C                   EXSR      CTRKD1
101200100216     C*
101300100216   B1C     *IN99         IFEQ      *OFF
101400100216     C*
101500100216   B2C                   SELECT
101600100216     C* ROLDOWN
101700100216     C     $TASTO        WHENEQ    ROLDWN
101800100216     C                   MOVE      'D1'          $GEST
101900100216     C* F3=Fine
102000100216     C     $TASTO        WHENEQ    F03
102100100216     C                   EXSR      F03D1
102200100216     C* F4=Ricerca
102300100216     C     $TASTO        WHENEQ    F04
102400100216     C                   EXSR      SEARCH
102500100216     C* F5=Refresh
102600100216     C     $TASTO        WHENEQ    F05
102700100216     C                   EXSR      F05D2
102800100216     C* F8=Successivo
102900100216     C     $TASTO        WHENEQ    F08
103000100216     C                   EXSR      F08D1
103100100216     C* F12=Ritorno
103200100216     C     $TASTO        WHENEQ    F12
103300100216     C                   EXSR      F12D1
103400100216     C* F24=Altri tasti
103500100216     C     $TASTO        WHENEQ    F24
103600100216     C                   EXSR      F24D2
103700100216     C*
103800100216     C                   OTHER
103900100216     C                   EXSR      CTRD2
104000100216     C     *IN99         IFEQ      *OFF
104100100216     C                   EXSR      CTRD1
104200100216     C     *IN99         IFEQ      *On
104300100216     C* Spengo gli indicatori di errore per il controllo su altri schermi
104400100216     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
104500100216     C* videate
104600100216     C                   MOVE      *ALL'0'       IN5098           49
104700100216     C                   MOVEA     IN5098        *IN(50)
104800100216     C                   SETON                                        9996                       96
104900100216     C                   ENDIF
105000100216     C*
105100100216     C                   ENDIF
105200100216     C*
105300100216     C*
105400100216     C     $TASTO        IFEQ      F06
105500100216     C     *IN99         ANDEQ     *OFF
105600100216     C* eseguo operazioni del dopo-aggiornamento
105700100216     C     *IN99         IFEQ      *OFF
105800100216     C                   EXSR      GESAGG
105900100216     C                   ENDIF
106000100216     C                   ENDIF
106100100216     C*
106200100216     C                   ENDSL
106300100216     C*
106400100216     C                   ENDIF
106500100216     C*
106600100216     C                   ENDSR
106700100216     C/EJECT
106800100216     C************************************************************
106900100216     C* RIEMPIMENTO VIDEATA D2
107000100216     C************************************************************
107100100216     C     RIED2         BEGSR
107200100216     C*
107300100216     C*
107400100216     C                   ENDSR
107500100216     C/EJECT
107600100216     C************************************************************
107700100216     C* GESTIONE F05 VIDEO D2
107800100216     C* il tasto F5 su qualsiasi videata provoca il refresh di tutte
107900100216     C* e il riposizionamento alla prima
108000100216     C************************************************************
108100100216     C     F05D2         BEGSR
108200100216     C*
108300100216     C                   MOVE      *ON           $INZD1
108400100216     C                   MOVE      'D1'          $GEST
108500100216     C*
108600100216     C                   ENDSR
108700100216     C/EJECT
108800100216     C************************************************************
108900100216     C* GESTIONE F24 VIDEO D2
109000100216     C************************************************************
109100100216     C     F24D2         BEGSR
109200100216     C*
109300100216     C                   MOVEA     $FX1          $FX
109400100216     C                   MOVEA     $FC1          $FC
109500100216     C                   MOVEA     $FR1          $FR
109600100216     C                   Z-ADD     $ULKD1        $ULK
109700100216     C                   EXSR      RIEKEY
109800100216     C                   Z-ADD     $ULK          $ULKD1            3 0
109900100216     C                   MOVEA     $FC           $FC1
110000100216     C                   MOVEL     $KEY1         Z1KE1
110100100216     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
110200100216     C                   MOVEL     $KEY2         Z1KE2
110300100216     C                   WRITE     FMTZ1
110400100216     C*
110500100216     C                   ENDSR
110600100216     C/EJECT
110700100216     C************************************************************
110800100216     C* CONTROLLO VIDEATA  D2
110900100216     C************************************************************
111000100216     C     CTRD2         BEGSR
111100100216     C*
111200100216     C                   SETOFF                                       99
111300100216     C*
111400100216     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
111500100216     C*
111600100216     C     ANPOPZ        IFNE      '04'
111700170529     C     D1AttCST      andEQ     XScOn
111800100216     C*
111900100216     C                   ENDIF
112000100216     C*
112100100216     C                   ENDSR
112200100216     C/EJECT
112300100210     C************************************************************
112400100210     C* RIEMPIMENTO DS per aggiornamento
112500100210     C************************************************************
112600100210     C     RIEDAT        BEGSR
112700100311     C*
112800170529     C                   if        D1AttCST    = XScOn
112900170529     C                   Move      *On           AttCSTA12
113000100311     C                   else
113100170529     C                   Move      *Off          AttCSTA12
113200100311     C                   endif
113300130823     C*
113301170707     C                   Eval      DtRifCA12   = D1DtRifC
113400170529     C                   Eval      PosAttA12   = D1PosAtt
113500170530     C                   Eval      DestsA12    = D1Dests
113600170530     C                   Eval      DestoA12    = D1Desto
113700170530     C                   Eval      DestrA12    = D1Destr
113800170529     C                   Eval      BeniA12     = D1Beni
113900170529     C                   Eval      ServiziA12  = D1Servizi
114000170529     C                   Eval      BeniSrvA12  = D1BeniSrv
114100130904     C*
114200100210     C                   ENDSR
114300100210     C/EJECT
114400100210     C************************************************************
114500100210     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
114600100210     C************************************************************
114700100210     C     GESAGG        BEGSR
114800100210     C*
114900100210     C* Riempio campi
115000100210     C                   EXSR      RIEDAT
115100100210     C                   MOVEA     $DT           ANPDAT
115200100210     C*
115300100210     C* segnalo al pgm chiamante l'avvenuta conferma
115400100210     C                   MOVE      *ON           ANPOPR
115500100210     C*
115600100210     C                   MOVE      *ON           $FINE
115700100210     C*
115800100210     C                   ENDSR
115900100210     C/EJECT
116000100210     C************************************************************
116100100210     C* OPERAZIONI INIZIALI
116200100210     C************************************************************
116300100210     C     *INZSR        BEGSR
116400100210     C*
116500100210     C* Reperimento parametri
116600100210     C*
116700100210     C     *ENTRY        PLIST
116800100210     C                   PARM                    ANPAR
116900100210     C                   PARM                    KPJBA
117000100210     C*
117100100210     C* Reperimento dati società
117200100210     C*
117300100210     C                   MOVEL     'SOC001'      TIPXSC
117400100210     C                   MOVEL     *BLANK        SOCXSC
117500100210     C                   EXSR      REPSOC
117600100210     C     RTNXSC        IFNE      '1'
117700100210     C                   MOVEL     XSOCDS        SOC001
117800100210     C                   ELSE
117900100210     C                   RETURN
118000100210     C                   ENDIF
118100100210     C*
118200100210     C* Reperimento tasti di funzione
118300100210     C*
118400100210     C                   EXSR      REPKEY
118500100210     C*
118600100210     C                   ENDSR
118700100210     C/EJECT
118800100210     C************************************************************
118900100210     C* REPERIMENTO TASTI DI FUNZIONE
119000100210     C************************************************************
119100100210     C     REPKEY        BEGSR
119200100210     C                   MOVEA     $FM1          $FM
119300100210     C                   EXSR      REPKKK
119400100210     C                   MOVEA     $FX           $FX1
119500100210     C*
119600100210     C                   ENDSR
119700100210     C/EJECT
119800100210     C************************************************************
119900100210     C* REPERIMENTO TASTI DI FUNZIONE
120000100210     C************************************************************
120100100210     C     REPKKK        BEGSR
120200100210     C*
120300100210     C                   CLEAR                   $FX
120400100210     C     1             DO        24            X
120500100210     C*
120600100210     C     $FM(X)        IFNE      *BLANK
120700100210     C     $FC(X)        ANDNE     '0'
120800100210     C*
120900100210     C                   MOVEL     $FM(X)        IDMSG
121000100210     C                   CALLB     'XRTVM'
121100100210     C                   PARM                    IDMSG            27
121200100210     C                   PARM                    TXTMSG          100
121300100210     C                   PARM                    ERRMSG            1
121400100210     C*
121500100210     C     ERRMSG        IFNE      *ON
121600100210     C                   MOVEL     TXTMSG        $FX(X)
121700100210     C                   ELSE
121800100210     C                   MOVEL     *ALL' '       $FX(X)
121900100210     C                   ENDIF
122000100210     C*
122100100210     C                   ENDIF
122200100210     C*
122300100210     C                   ENDDO
122400100210     C*
122500100210     C                   ENDSR
122600100210     C/EJECT
122700100210     C************************************************************
122800100210     C* Reperimento dati società
122900100210     C************************************************************
123000100210     C     REPSOC        BEGSR
123100100210     C*
123200100210     C                   CALL      'SDGSOCDR'
123300100210     C                   PARM                    TIPXSC            6
123400100210     C                   PARM                    SOCXSC            3
123500100210     C                   PARM                    CDSXSC            9 0
123600100210     C                   PARM                    MODXSC            3
123700100210     C                   PARM      *BLANKS       RTNXSC            1
123800100210     C                   PARM                    XSOCDS
123900100210     C                   PARM                    KPJBA
124000100210     C*
124100100210     C                   ENDSR
124200100210     C/EJECT
124300100210     C************************************************************
124400100210     C* INIZIALIZZAZIONE VARIABILI
124500100210     C************************************************************
124600100210     C     INZVAR        BEGSR
124700100210     c*
124800100210     C                   MOVEA     ANPDAT        $DT
124900100210     C*
125000100210     C* Definizione dei tasti funzione utilizzabili a pgm
125100100210     C*
125200100210     C                   EXSR      DEFOPZ
125300100210     C*
125400100210     C* Variabili per gestione videate
125500100210     C*
125600100210     C                   MOVE      'D1'          $GEST                          fmt in gestione
125700100210     C                   MOVE      *OFF          $FINE                          fine pgm
125800100210     C                   MOVE      *ON           $INZD1                         inizializzione D1
125900100210     C                   MOVE      *BLANK        $LASTG                         fmt di provenienza
126000100210     C                   MOVE      *BLANK        $LASTV                         ultima emissione
126100100210     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
126200100210     C*
126300100210     C* Variabili appoggio
126400100210     C*
126500100210     C                   Z-ADD     0             CURR
126600100210     C                   Z-ADD     0             CURC
126700100210     C*
126800100210     C* Valorizzazione campi univoci testate
126900100210     C*
127000100210     C                   CLEAR                   FMTT1
127100100210     C                   MOVEL     XSCDSI        NOMDIT
127200100210     C                   MOVEL     KCDAZ         NOMAZN
127300100210     C                   MOVEL     KnSif         NOMSIF
127400100210     C* recupero il sottotitolo (se c'è)
127500100210     C                   MOVEL     *BLANK        T1TIT
127600100210     C     SOTTIT        IFNE      *BLANK
127700100210     C                   MOVEL     SOTTIT        IDMSG
127800100210     C                   CALLB     'XRTVM'
127900100210     C                   PARM                    IDMSG            27
128000100210     C                   PARM                    TXTMSG          100
128100100210     C                   PARM                    ERRMSG            1
128200100210     C                   PARM      *ON           CTRMSG            1            centratura
128300100210     C                   PARM      30            LENMSG            3 0          lun output.
128400100210     C     ERRMSG        IFNE      *ON
128500100210     C                   MOVEL     TXTMSG        T1TIT
128600100210     C                   ELSE
128700100210     C                   MOVEL     *ALL'?'       T1TIT
128800100210     C                   ENDIF
128900100210     C                   ENDIF
129000100210     C*
129100100210     C* Inizializzazione/Decodifica prima videata
129200100210     C*
129300100210     C                   EXSR      INZD1
129400100210     C                   MOVE      *OFF          $INZD1
129500100210     C*
129600100210     C                   ENDSR
129700100210     C/EJECT
129800100210     C************************************************************
129900100210     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
130000100210     C************************************************************
130100100210     C     DEFOPZ        BEGSR
130200100210     C*
130300100210     C* Questa routine deve valorizzare la schiera $FC1/$FR1
130400100210     C* con S o N a seconda della utilizzabilità o meno
130500100210     C* del tasto funzione correlato
130600100210     C*
130700100210     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
130800100210     C* di compilazione
130900100210     C                   RESET                   $FC1
131000100210     C                   RESET                   $FR1
131100100210     C*
131200100210     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
131300100210     C* invalido alcune opzioni
131400100210     C                   SELECT
131500100210     C* Se pgm richiamato in interrogazione disattivo :
131600100210     C     ANPOPZ        WHENEQ    '05'
131700100210     C* F5=Ripristino
131800100210     C                   Z-ADD     5             X
131900100210     C                   MOVE      '0'           $FC1(X)
132000100210     C* Se pgm richiamato in annullamento disattivo :
132100100210     C     ANPOPZ        WHENEQ    '04'
132200100210     C* F5=Ripristino
132300100210     C                   Z-ADD     5             X
132400100210     C                   MOVE      '0'           $FC1(X)
132500100210     C*
132600100210     C                   OTHER
132700100210     C*
132800100210     C                   ENDSL
132900100210     C*
133000100210     C                   ENDSR
133100100210     C************************************************************
133200100210     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
133300100210     C************************************************************
133400100210     C     DEFCAM        BEGSR
133500100210     C*
133600100210     C* Variabili per gestione videate
133700100210     C*
133800100210     C                   MOVE      *BLANK        $GEST             2            fmt in gestione
133900100210     C                   MOVE      *BLANK        $FINE             1            fine pgm
134000100210     C                   MOVE      *BLANK        $INZD1            1            inizializzione D1
134100100210     C                   MOVE      *BLANK        $LASTG            2            fmt di provenienza
134200100210     C                   MOVE      *BLANK        $LASTV            2            ultima emissione
134300100210     C                   Z-ADD     0             $PIED1            3 0          piede da emett. D1
134400100210     C*
134500100210     C* Variabili appoggio
134600100210     C*
134700100210     C                   Z-ADD     0             CURR              2 0
134800100210     C                   Z-ADD     0             CURC              2 0
134900100210     C                   MOVE      *ZEROS        WIN              99
135000100210     C                   MOVE      *BLANK        WTBERR            1
135100100210     C*
135200100210     C* Indici
135300100210     C*
135400100210     C                   Z-ADD     0             X                 3 0
135500100210     C                   Z-ADD     0             Y                 3 0
135600100210     C*
135700100210     C                   ENDSR
135800100210     C/EJECT
135900100210     C************************************************************
136000100210     C* CONTROLLO ABILITAZIONE TASTI
136100100210     C************************************************************
136200100210     C     CTRKEY        BEGSR
136300100210     C                   SELECT
136400100210     C     $TASTO        WHENEQ    F01
136500100210     C     $FC(1)        IFEQ      '0'
136600100210     C                   SETON                                        99
136700100210     C                   ENDIF
136800100210     C     $TASTO        WHENEQ    F02
136900100210     C     $FC(2)        IFEQ      '0'
137000100210     C                   SETON                                        99
137100100210     C                   ENDIF
137200100210     C     $TASTO        WHENEQ    F03
137300100210     C     $FC(3)        IFEQ      '0'
137400100210     C                   SETON                                        99
137500100210     C                   ENDIF
137600100210     C     $TASTO        WHENEQ    F04
137700100210     C     $FC(4)        IFEQ      '0'
137800100210     C                   SETON                                        99
137900100210     C                   ENDIF
138000100210     C     $TASTO        WHENEQ    F05
138100100210     C     $FC(5)        IFEQ      '0'
138200100210     C                   SETON                                        99
138300100210     C                   ENDIF
138400100210     C     $TASTO        WHENEQ    F06
138500100210     C     $FC(6)        IFEQ      '0'
138600100210     C                   SETON                                        99
138700100210     C                   ENDIF
138800100210     C     $TASTO        WHENEQ    F07
138900100210     C     $FC(7)        IFEQ      '0'
139000100210     C                   SETON                                        99
139100100210     C                   ENDIF
139200100210     C     $TASTO        WHENEQ    F08
139300100210     C     $FC(8)        IFEQ      '0'
139400100210     C                   SETON                                        99
139500100210     C                   ENDIF
139600100210     C     $TASTO        WHENEQ    F09
139700100210     C     $FC(09)       IFEQ      '0'
139800100210     C                   SETON                                        99
139900100210     C                   ENDIF
140000100210     C     $TASTO        WHENEQ    F10
140100100210     C     $FC(10)       IFEQ      '0'
140200100210     C                   SETON                                        99
140300100210     C                   ENDIF
140400100210     C     $TASTO        WHENEQ    F11
140500100210     C     $FC(11)       IFEQ      '0'
140600100210     C                   SETON                                        99
140700100210     C                   ENDIF
140800100210     C     $TASTO        WHENEQ    F12
140900100210     C     $FC(12)       IFEQ      '0'
141000100210     C                   SETON                                        99
141100100210     C                   ENDIF
141200100210     C     $TASTO        WHENEQ    F13
141300100210     C     $FC(13)       IFEQ      '0'
141400100210     C                   SETON                                        99
141500100210     C                   ENDIF
141600100210     C     $TASTO        WHENEQ    F14
141700100210     C     $FC(14)       IFEQ      '0'
141800100210     C                   SETON                                        99
141900100210     C                   ENDIF
142000100210     C     $TASTO        WHENEQ    F15
142100100210     C     $FC(15)       IFEQ      '0'
142200100210     C                   SETON                                        99
142300100210     C                   ENDIF
142400100210     C     $TASTO        WHENEQ    F16
142500100210     C     $FC(16)       IFEQ      '0'
142600100210     C                   SETON                                        99
142700100210     C                   ENDIF
142800100210     C     $TASTO        WHENEQ    F17
142900100210     C     $FC(17)       IFEQ      '0'
143000100210     C                   SETON                                        99
143100100210     C                   ENDIF
143200100210     C     $TASTO        WHENEQ    F18
143300100210     C     $FC(18)       IFEQ      '0'
143400100210     C                   SETON                                        99
143500100210     C                   ENDIF
143600100210     C     $TASTO        WHENEQ    F19
143700100210     C     $FC(19)       IFEQ      '0'
143800100210     C                   SETON                                        99
143900100210     C                   ENDIF
144000100210     C     $TASTO        WHENEQ    F20
144100100210     C     $FC(20)       IFEQ      '0'
144200100210     C                   SETON                                        99
144300100210     C                   ENDIF
144400100210     C     $TASTO        WHENEQ    F21
144500100210     C     $FC(21)       IFEQ      '0'
144600100210     C                   SETON                                        99
144700100210     C                   ENDIF
144800100210     C     $TASTO        WHENEQ    F22
144900100210     C     $FC(22)       IFEQ      '0'
145000100210     C                   SETON                                        99
145100100210     C                   ENDIF
145200100210     C     $TASTO        WHENEQ    F23
145300100210     C     $FC(23)       IFEQ      '0'
145400100210     C                   SETON                                        99
145500100210     C                   ENDIF
145600100210     C     $TASTO        WHENEQ    F24
145700100210     C     $FC(24)       IFEQ      '0'
145800100210     C                   SETON                                        99
145900100210     C                   ENDIF
146000100210     C                   ENDSL
146100100210     C                   ENDSR
146200100210     C/EJECT
146300100210     C************************************************************
146400100210     C* RIEMPIMENTO TASTI FUNZIONE
146500100210     C************************************************************
146600100210     C     RIEKEY        BEGSR
146700100210     C*
146800100210     C* Input :
146900100210     C* - $FX = schiera con tutti i testi disponibili
147000100210     C* - $FC = schiere con i flag di validità associati ai testi
147100100210     C* - $FR = schiere con i flag di REVERSE IMAGE
147200100210     C* - $ULK = ultimo testo caricato
147300100210     C* - $L01 = Lunghezza prima riga
147400100210     C* - $L02 = Lunghezza seconda riga
147500100210     C* Output :
147600100210     C* - $ULK
147700100210     C* - $KEY1= 1° riga di tasti funzione
147800100210     C* - $KEY2= 2° riga di tasti funzione
147900100210     C*
148000100210     C                   CALLB     'XKEYMSG'
148100100210     C* input
148200100210     C                   PARM                    $FX
148300100210     C                   PARM                    $FC
148400100210     C                   PARM                    $FR
148500100210     C                   PARM                    $ULK              3 0
148600100210     C* output
148700100210     C                   PARM                    $KEY1            79
148800100210     C                   PARM                    $KEY2            79
148900100210     C* input
149000100210     C                   PARM      79            $L01              3 0
149100100210     C                   PARM      79            $L02              3 0
149200100210     C*
149300100210     C                   PARM                    $ALLFUNCT       480
149400100210     C*
149500100210     C                   ENDSR
149600100210     C************************************************************
149700100210**   C
14980010021001SDGMSG    *LIBL     SDC0002    inserimento
14990010021002PROMSG    *LIBL     COS0A02    modifica
15000010021003PROMSG    *LIBL     COS0003    copia
15010010021004PROMSG    *LIBL     COS0004    annullamento
15020010021005PROMSG    *LIBL     COS0005    visualizzazione
150300100210** TASTI DI FUNZIONE  D1
150400100210PROMSG    *LIBL     KEY0001  01
150500100210PROMSG    *LIBL     KEY0A02  02
150600100210PROMSG    *LIBL     KEY0003  03
150700100210PROMSG    *LIBL     KEY0004  04
150800100210PROMSG    *LIBL     KEY0005  05
150900100210PROMSG    *LIBL     KEY0006  06
151000100210PROMSG    *LIBL     KEY0007  07
151100100210PROMSG    *LIBL     KEY0008  08
151200100210PROMSG    *LIBL     KEY0009  09
151300100210PROMSG    *LIBL     KEY0010  10
151400100210PROMSG    *LIBL     KEY0011  11
151500100210PROMSG    *LIBL     KEY0012  12
151600100210PROMSG    *LIBL     KEY0013  13
151700120206SDGMSG    *LIBL     SDK0015  14
151800170704SDGMSG    *LIBL     SDK0030  15
151900100210PROMSG    *LIBL     KEY0016  16
152000100210SDGMSG    *LIBL     SDK0001  17
152100100210PROMSG    *LIBL     KEY0018  18
152200170704PROMSG    *LIBL     KEY0248  19
152300100210PROMSG    *LIBL     KEYA020  20
152400100210PROMSG    *LIBL     KEYA021  21
152500100210PROMSG    *LIBL     KEYA022  22
152600100210PROMSG    *LIBL     KEYA023  23
152700100210PROMSG    *LIBL     KEYA024  24
152800100210**  ABILITAZIONE D1
1529001002100N  01
1530001002100N  02
1531001002100N  03
1532001002101N  04
1533001002101N  05
1534001002101N  06
1535001002100N  07
1536001002100N  08
1537001002100N  09
1538001002100N  10
1539001002100N  11
1540001002101N  12
1541001002100N  13
1542001309270N  14
1543001707041N  15
1544001002100N  16
1545001002100N  17
1546001002100N  18
1547001707041N  19
1548001002100N  20
1549001002100N  21
1550001002100N  22
1551001002100N  23
1552001707040N  24   Lasciare sempre impostata a '0' la colonna $FC
