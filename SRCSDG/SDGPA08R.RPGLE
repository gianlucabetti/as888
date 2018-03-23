000100100210     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200100210     H*PARMS CVTOPT(*NONE)
000300100210     H DECEDIT('0,') DATEDIT(*DMY.)
000400100210      *
000500130821      *       Parametri per SPESOMETRO clienti/fornitori
000600100210      *
000700100210     F*----------------------------------------------------*
000800130821     FSDGPA08D  CF   E             WORKSTN
000900130821     F                                     RENAME(PA08D1:FMTD1)
001000130821     F                                     RENAME(PA08D2:FMTD2)
001100130821     F                                     RENAME(PA08T1:FMTT1)
001200130821     F                                     RENAME(PA08Z1:FMTZ1)
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
004100110901     D*-------------
004200110901     D* Ds per tabella codici attributi contabili
004300110901     D ANG0A1DS      E DS                  INZ
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
005700110901     D*-------------
005800100210     D* Ds esterna dati
005900130821     D SDPA08        E DS                  EXTNAME(SDGPA08DS)
006000100210     D  $DT                    1   2000
006100100210     D                                     DIM(2000)
006200100210     D*-------------
006300100210     D* Parametri ricevuti
006400100210     D ANPAR         E DS                  EXTNAME(ANPARDS)
006500100210     D  ANPSOC       E                     EXTFLD(ANPSOCIETA)
006600100210     D  ANPPAR       E                     EXTFLD(ANPPARAME)
006700100210     D  ANPDAT       E                     EXTFLD(ANPDATI)
006800100210     D                                     DIM(2000)
006900100210     D  ANPERR       E                     EXTFLD(ANPERRORE)
007000100210     D*-------------
007100100210     D DSFMT           DS
007200100210     D  $TASTO               369    369
007300100210     D  NRG                  370    370
007400100210     D  NCL                  371    371
007500100210     D*-------------
007600100210     D* posizione cursore
007700100210     D CURSOR          DS
007800100210     D  RIRI                   1      2B 0 INZ
007900100210     D  R$$                    2      2
008000100210     D  COCO                   3      4B 0 INZ
008100100210     D  C$$                    4      4
008200100210     D*-------------
008300100210     D* Costanti
008400100210     D*-------------
008500100210     D* Tasti di funzione
008600100210     D F01             C                   CONST(X'31')
008700100210     D F02             C                   CONST(X'32')
008800100210     D F03             C                   CONST(X'33')
008900100210     D F04             C                   CONST(X'34')
009000100210     D F05             C                   CONST(X'35')
009100100210     D F06             C                   CONST(X'36')
009200100210     D F07             C                   CONST(X'37')
009300100210     D F08             C                   CONST(X'38')
009400100210     D F09             C                   CONST(X'39')
009500100210     D F10             C                   CONST(X'3A')
009600100210     D F11             C                   CONST(X'3B')
009700100210     D F12             C                   CONST(X'3C')
009800100210     D F13             C                   CONST(X'B1')
009900100210     D F14             C                   CONST(X'B2')
010000100210     D F15             C                   CONST(X'B3')
010100100210     D F16             C                   CONST(X'B4')
010200100210     D F17             C                   CONST(X'B5')
010300100210     D F18             C                   CONST(X'B6')
010400100210     D F19             C                   CONST(X'B7')
010500100210     D F20             C                   CONST(X'B8')
010600100210     D F21             C                   CONST(X'B9')
010700100210     D F22             C                   CONST(X'BA')
010800100210     D F23             C                   CONST(X'BB')
010900100210     D F24             C                   CONST(X'BC')
011000100210     D ENTER           C                   CONST(X'F1')
011100100210     D ROLDWN          C                   CONST(X'F4')
011200100210     D ROLLUP          C                   CONST(X'F5')
011300100210     D*-------------
011400100210     D* Reperimento nome PGM
011500100210     D STATUS         SDS           333
011600100210     D  DSPGM            *PROC
011700100210     C*----------------------------------------------------*
011800100210     C*                MAIN LINE PROGRAM
011900100210     C*----------------------------------------------------*
012000100210     C*
012100100210     C                   EXSR      INZVAR
012200100210     C*
012300100210     C     $FINE         DOWEQ     *OFF
012400100210     C     $GEST         CASEQ     'D1'          GESD1
012500100216     C     $GEST         CASEQ     'D2'          GESD2
012600100210     C                   ENDCS
012700100210     C                   ENDDO
012800100210     C*
012900100210     C                   EXSR      ENDPGM
013000100210     C/EJECT
013100100210     C************************************************************
013200100210     C* FINE PROGRAMMA
013300100210     C************************************************************
013400100210     C     ENDPGM        BEGSR
013500100210     C*
013600100210     C                   SETON                                            RT
013700100210     C                   RETURN
013800100210     C*
013900100210     C                   ENDSR
014000100210     C/EJECT
014100100210     C************************************************************
014200100210     C* RICERCHE
014300100210     C************************************************************
014400100210     C     SEARCH        BEGSR
014500100210     C*
014600100210     C* determino Riga/Colonna del cursore
014700100210     C                   MOVE      NRG           R$$
014800100210     C                   MOVE      NCL           C$$
014900100210     C                   Z-ADD     RIRI          CURR
015000100210     C                   Z-ADD     COCO          CURC
015100100210     C*
015200100210     C* I campi che al momento dell'F4 risultano protetti non devono
015300100210     C* essere abilitati alla funzione di ricerca
015400100210     C*
015500100827     C                   SELECT
015600111024     C*
015700130821     C* FMTD1 - Formato di comunicazione
015800130821     C     H1NMFL        WHENEQ    'D1FORMATO'
015900130823     C                   EXSR      F4_Formato
015901131015R135 C*
015902131015 "   C* FMTD1 - Modalità di aggregazione dei Doc. di Riepilogo
015903131015 "   C*         in caso di Formato Aggregato
015904131015 "   C     H1NMFL        WHENEQ    'D1DOCRIE '
015905131015R135 C                   EXSR      F4_DocRie
015906131128R132 C*
015907131128 "   C* FMTD1 - Gestione note di variazione
015909131128 "   C     H1NMFL        WHENEQ    'D1NOTEVAR'
015910131128R132 C                   EXSR      F4_NoteVar
016000130823     C*
016100130823     C* Valore per conto con operazioni di servizi
016200130823     C* Valore per conto con operazioni di beni
016300130823     C                   When      H1NMFL       = 'D1BENI   '  or
016400130823     C                             H1NMFL       = 'D1SERVIZI'
016500130823     C                   EXSR      F4_ValBenSrv
016600130823     C*
016700130823     C* Valore di default
016800130823     C                   When      H1NMFL       = 'D1BENISRV'
016900130823     C                   EXSR      F4_BeniSrv
017000130823     C*
017100100210     C* ricerca non abilitata in questa posizione
017200100210     C                   OTHER
017300100210     C                   SETON                                        9899
017400100210     C*
017500100210     C                   ENDSL
017600110901     C*
017700100210     C* imposto pos. del cursore
017800100210     C                   Z-ADD     CURR          H1RIGA
017900100210     C                   Z-ADD     CURC          H1COLO
018000100210     C                   MOVE      *HIVAL        $LASTV
018100100210     c*
018200100210     C                   ENDSR
018300100210     C/EJECT
018400130823     C************************************************************
018500130823     C* Ricerca valori per formato comunicazione
018600130823     C************************************************************
018700130823     C     F4_Formato    BEGSR
018800130823     C*
018900130823     C                   RESET                   ANA964DS
019000130823     C*
019100130823     C                   Move      '01'          Opz964
019200130823     C                   MOVEL     'SDGPA08S  '  TRC964
019300130823     C                   MOVEL     'FORMATOA08'  CAM964
019400130823     C                   MOVE      *ON           ALL964
019500130823     C                   MOVEL     ANA964DS      KPJBU
019600130823     C                   CALL      'ANA964R'
019700130823     C                   PARM                    KPJBA
019800130823     C                   MOVEL     KPJBU         ANA964DS
019900130823     C     RET964        IFEQ      *BLANK
020000130823     C                   MOVEL     VAL964        D1Formato
020100130823     C                   ENDIF
020200130823     c*
020300130823     C                   ENDSR
020400130823     C/EJECT
020401131015R135 C************************************************************
020402131015 "   C* Ricerca valori per Documento di riepilogo
020403131015 "   C************************************************************
020404131015R135 C     F4_DocRie     BEGSR
020405131015     C*
020406131015     C                   RESET                   ANA964DS
020407131015     C*
020408131015     C                   Move      '01'          Opz964
020409131015     C                   MOVEL     'SDGPA08S  '  TRC964
020410131015     C                   MOVEL     'DOCRIEA08 '  CAM964
020411131015     C                   MOVE      *ON           ALL964
020412131015     C                   MOVEL     ANA964DS      KPJBU
020413131015     C                   CALL      'ANA964R'
020414131015     C                   PARM                    KPJBA
020415131015     C                   MOVEL     KPJBU         ANA964DS
020416131015     C     RET964        IFEQ      *BLANK
020417131015     C                   MOVEL     VAL964        D1DocRie
020418131015     C                   ENDIF
020419131015     c*
020420131015R135 C                   ENDSR
020421131015R135 C/EJECT
020422131128R132 C************************************************************
020423131128 "   C* Ricerca valori per Gestione note di variazione
020424131128 "   C************************************************************
020425131128R132 C     F4_NoteVar    BEGSR
020426131128     C*
020427131128     C                   RESET                   ANA964DS
020428131128     C*
020429131128     C                   Move      '01'          Opz964
020430131128     C                   MOVEL     'SDGPA08S  '  TRC964
020431131128     C                   MOVEL     'NOTEVARA08'  CAM964
020432131128     C                   MOVE      *ON           ALL964
020433131128     C                   MOVEL     ANA964DS      KPJBU
020434131128     C                   CALL      'ANA964R'
020435131128     C                   PARM                    KPJBA
020436131128     C                   MOVEL     KPJBU         ANA964DS
020437131128     C     RET964        IFEQ      *BLANK
020438131128     C                   MOVEL     VAL964        D1NoteVar
020439131128     C                   ENDIF
020440131128     c*
020441131128R132 C                   ENDSR
020442131128R132 C/EJECT
020500130823     C************************************************************
020600130823     C* Ricerca valori per attributo contabile
020700130823     C************************************************************
020800130823     C     F4_ValBenSrv  BEGSR
020900130823     C*
021000130823     C                   RESET                   XTABDS
021100130823     C*
021200130823     C                   MOVE      '01'          XTAOPZ
021300130823     C                   MOVE      XSCSOC        XTAAZI
021400130823     C                   MOVE      XSCLIN        XTALIN
021500130823     C                   Z-ADD     CURR          XTARIG
021600130823     C                   Z-ADD     CURC          XTACOL
021700130823     C                   MOVE      *ZERO         XTaKey
021800130823     C*
021900130823     C                   Move      D1PosAtt      XTaPos
022000130823     C*
022100130823     C                   Select
022200130823     C* Valore per conto con operazioni di beni
022300130823     C                   When      H1NMFL       = 'D1BENI   '
022400130823     C                   MoveL(P)  D1Beni        Wrk6              6
022500130823     C* Valore per conto con operazioni di servizi
022600130823     C                   When      H1NMFL       = 'D1SERVIZI'
022700130823     C                   MoveL(P)  D1Servizi     Wrk6              6
022800130823     C                   endSL
022900130823     C*
023000130823     C                   Move      Wrk6          XTaKey
023100130823     C*
023200130823     C                   MoveL     XTABDS        KPJBU
023300130823     C                   CALL      'ANG0A1R'
023400130823     C                   PARM                    KPJBA
023500130823     C*
023600130823     C                   MoveL     KPJBU         XTABDS
023700130823     C                   if        XTARet     <> '2'
023800130823     C*
023900130823     C                   Select
024000130823     C* Valore per conto con operazioni di beni
024100130823     C                   When      H1NMFL       = 'D1BENI  '
024200130823     C                   Move      XTAKEY        D1Beni
024300130823     C* Valore per conto con operazioni di servizi
024400130823     C                   When      H1NMFL       = 'D1SERVIZI'
024500130823     C                   Move      XTAKEY        D1Servizi
024600130823     C                   endSL
024700130823     C*
024800130823     C                   endif
024900130823     c*
025000130823     C                   ENDSR
025100130823     C/EJECT
025200130823     C************************************************************
025300130823     C* Ricerca default per beni/servizi
025400130823     C************************************************************
025500130823     C     F4_BeniSrv    BEGSR
025600130823     C*
025700130823     C                   RESET                   ANA964DS
025800130823     C*
025900130823     C                   Move      '01'          Opz964
026000130823     C                   MOVEL     'SDGPA08S  '  TRC964
026100130823     C                   MOVEL     'BENISRVA08'  CAM964
026200130823     C                   MOVE      *ON           ALL964
026300130823     C                   MOVEL     ANA964DS      KPJBU
026400130823     C                   CALL      'ANA964R'
026500130823     C                   PARM                    KPJBA
026600130823     C                   MOVEL     KPJBU         ANA964DS
026700130823     C     RET964        IFEQ      *BLANK
026800130823     C                   MOVEL     VAL964        D1BeniSrv
026900130823     C                   ENDIF
027000130823     C*
027100130823     C                   ENDSR
027200130823     C/EJECT
027300100210     C************************************************************
027400100210     C* GESTIONE EMISSIONE TASTI FUNZIONE
027500100210     C************************************************************
027600100210     C     GESPIE        BEGSR
027700100210     C*
027800100210     C                   SELECT
027900100210     C*
028000100210     C     $GEST         WHENEQ    'D1'
028100100210     C     $ULKD1        IFEQ      0
028200100210     C* riempimento opzioni
028300100210     C                   EXSR      F24D1
028400100210     C                   ELSE
028500100210     C                   WRITE     FMTZ1
028600100210     C                   ENDIF
028700100210     C*
028800100210     C     $GEST         WHENEQ    'D2'
028900100210     C     $ULKD1        IFEQ      0
029000100210     C* riempimento opzioni
029100100216     C                   EXSR      F24D2
029200100210     C                   ELSE
029300100210     C                   WRITE     FMTZ1
029400100210     C                   ENDIF
029500100210     C                   ENDSL
029600100210     C*
029700100210     C                   ENDSR
029800100210     C/EJECT
029900100210     C************************************************************
030000100210     C* GESTIONE VIDEO RECORD D1
030100100210     C************************************************************
030200100210     C     GESD1         BEGSR
030300100216     C*
030400100210     C* inizializzazione videata
030500100210     C     $INZD1        IFEQ      *ON
030600100210     C                   EXSR      INZD1
030700100210     C                   MOVE      *OFF          $INZD1
030800100210   1EC                   ENDIF
030900100210     C* emissione piede videata se proveniente da altra
031000100210     C* salvataggio valore formato di provenienza
031100100210   1BC     $LASTV        IFNE      'D1'
031200100210     C*
031300100210     C                   WRITE     FMTT1
031400100210     C                   EXSR      GESPIE
031500100210     C                   MOVE      $LASTV        $LASTG
031600100210     C                   MOVE      'D1'          $LASTV
031700100210   1EC                   ENDIF
031800100210     C*
031900100210     C* se esistono errori sulla videata
032000100210     C* emetto la write del formato a indicatori spenti per vedere
032100100210     C* le eventuali decodifiche
032200100210     C     *IN99         IFEQ      *ON
032300100210     C     $GEST         ANDEQ     'D1'
032400100210     C                   MOVEA     *IN           WIN
032500100210     C                   MOVE      *ALL'0'       IN5098           49
032600100210     C                   MOVEA     IN5098        *IN(50)
032700100210     C                   WRITE     FMTD1
032800100210     C                   MOVEA     WIN           *IN
032900100210     C                   ENDIF
033000100210     C*
033100100210     C                   SELECT
033200100210     C* Annullamento
033300100210     C     ANPOPZ        WHENEQ    '04'
033400100210     C* Visualizzazione
033500100210     C     ANPOPZ        OREQ      '05'
033600100210     C                   WRITE     FMTD1
033700100210     C                   EXFMT     PROTECT
033800100210     C* Immissione/Modifica/Copia
033900100210     C                   OTHER
034000100210     C                   EXFMT     FMTD1
034100100210     C                   ENDSL
034200100210     C* controllo abilitazione tasti funzione
034300100210     C*
034400100210     C                   EXSR      CTRKD1
034500100210     C*
034600100210     C     *IN99         IFEQ      *OFF
034700100210     C*
034800100210     C                   SELECT
034900100216     C* ROLLUP
035000111024     C**** $TASTO        WHENEQ    ROLLUP
035100111024     C****               MOVE      'D2'          $GEST
035200100210     C* F2=Decodifica
035300100210     C     $TASTO        WHENEQ    F02
035400100210     C                   EXSR      F02D1
035500100210     C* F3=Fine
035600100210     C     $TASTO        WHENEQ    F03
035700100210     C                   EXSR      F03D1
035800100210     C* F4=Ricerca
035900100210     C     $TASTO        WHENEQ    F04
036000100210     C                   EXSR      SEARCH
036100100210     C* F5=Refresh
036200100210     C     $TASTO        WHENEQ    F05
036300100210     C                   EXSR      F05D1
036400100210     C* F8=Successivo
036500100210     C     $TASTO        WHENEQ    F08
036600100210     C                   EXSR      F08D1
036700100210     C* F12=Ritorno
036800100210     C     $TASTO        WHENEQ    F12
036900100210     C                   EXSR      F12D1
037300100210     C* F24=Altri tasti
037400100210     C     $TASTO        WHENEQ    F24
037500100210     C                   EXSR      F24D1
037600100210     C*
037700100210     C                   OTHER
037800100210     C                   EXSR      CTRD1
037900100216     C     *IN99         IFEQ      *OFF
038000111024     C****               EXSR      CTRD2
038100100216     C     *IN99         IFEQ      *ON
038200100216     C* Spengo gli indicatori di errore perchè, nel caso si siano usati
038300100216     C* indicatori uguali in videate differenti, genererei conflitti
038400100216     C                   MOVE      *ALL'0'       IN5098           49
038500100216     C                   MOVEA     IN5098        *IN(50)
038600100216     C                   SETON                                        9996                       96
038700100216     C                   ENDIF
038800100216     C*
038900100216     C                   ENDIF
039000100216     C*
039100100210     C     $TASTO        IFEQ      F06
039200100210     C* eseguo operazioni per aggiornamento
039300100210     C     *IN99         IFEQ      *OFF
039400100210     C                   EXSR      GESAGG
039500100210     C                   ENDIF
039600100210     C                   ENDIF
039700100210     C*
039800100210     C                   ENDSL
039900100210     C*
040000100210     C                   ENDIF
040100100210     C*
040200100210     C                   ENDSR
040300100210     C/EJECT
040400100210     C************************************************************
040500100210     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
040600100210     C************************************************************
040700100210     C     CTRKD1        BEGSR
040800100210     C                   SETOFF                                       99
040900100210     C                   MOVEA     $FC1          $FC
041000100210     C                   EXSR      CTRKEY
041100100210     C     *IN99         IFEQ      *ON
041200100210     C                   SETON                                        97
041300100210     C                   ENDIF
041400100210     C                   ENDSR
041500100210     C************************************************************
041600100210     C* INIZIALIZZAZIONE VIDEATA DATI
041700100210     C************************************************************
041800100210     C     INZD1         BEGSR
041900100210     C*
042000100210     C                   CLEAR                   FMTD1
042100100216     C                   CLEAR                   FMTD2
042200100210     C*
042300100830     C                   move      xscon         d1on1
042400100830     C                   move      xscoff        d1off1
042500100210     C*
042600100210     C                   MOVE      ANPPAR        D1PAR
042700100210     C*
042800100210     C                   Z-ADD     1             $PIED1
042900100210     C*
043000100210     C     ANPOPZ        IFNE      '01'
043100100210     C                   EXSR      RIED1
043200100216     C                   EXSR      RIED2
043300100210     C                   EXSR      CTRD1
043400111024     C****               EXSR      CTRD2
043500100210     C                   MOVE      *ALL'0'       IN5098
043600100210     C                   MOVEA     IN5098        *IN(50)
043700100210     C*
043800100210     C                   ELSE
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
044900130821     C                   if        AttSCFA08   = *On
045000130821     C                   MoveL     XScOn         D1AttSCF
045100100311     C                   else
045200130821     C                   MoveL     XScOff        D1AttSCF
045300100311     C                   endif
045400110808     C*
045500130823     C                   Eval      D1Formato   = FormatoA08
045502131015R135 C                   Eval      D1DocRie    = DocRieA08
045600130823     C*
045700130823     C                   Eval      D1PosAtt    = PosAttA08
045800130823     C                   Eval      D1Beni      = BeniA08
045900130823     C                   Eval      D1Servizi   = ServiziA08
046000130823     C                   Eval      D1BeniSrv   = BeniSrvA08
046300130904     C*
046400130904     C                   Eval      D1IdProd    = IdProdA08
046401131128R132 C*
046402131128R132 C                   Eval      D1NoteVar   = NoteVarA08
046500130904     C*
046600100210     C                   ENDSR
046700100210     C/EJECT
046800100210     C************************************************************
046900100210     C* GESTIONE F02 VIDEO D1
047000100210     C************************************************************
047100100210     C     F02D1         BEGSR
047200100210     C*
047300100210     C* per decodificare la videata richiamo la routine dei ctrl
047400100210     C* e poi disattivo gli eventuali messaggi di errore
047500100210     C                   EXSR      CTRD1
047600100210     C*
047700100210     C                   MOVE      *ALL'0'       IN5098
047800100210     C                   MOVEA     IN5098        *IN(50)
047900100210     C*
048000100210     C                   ENDSR
048100100210     C/EJECT
048200100210     C************************************************************
048300100210     C* GESTIONE F03 VIDEO D1
048400100210     C************************************************************
048500100210     C     F03D1         BEGSR
048600100210     C*
048700100210     C                   MOVE      *ON           $FINE
048800100210     C* fine programma
048900100210     C                   EXSR      ENDPGM
049000100210     C*
049100100210     C                   ENDSR
049200100210     C/EJECT
049300100210     C************************************************************
049400100210     C* GESTIONE F05 VIDEO D1
049500100210     C************************************************************
049600100210     C     F05D1         BEGSR
049700100210     C*
049800100210     C                   MOVE      *ON           $INZD1
049900100210     C*
050000100210     C                   ENDSR
050100100210     C/EJECT
050200100210     C************************************************************
050300100210     C* GESTIONE F08 VIDEO D1
050400100210     C************************************************************
050500100210     C     F08D1         BEGSR
050600100210     C*
050700100210     C                   MOVE      *ON           $FINE
050800100210     C*
050900100210     C                   ENDSR
051000100210     C/EJECT
051100100210     C************************************************************
051200100210     C* GESTIONE F12 VIDEO D1
051300100210     C************************************************************
051400100210     C     F12D1         BEGSR
051500100210     C*
051600100210     C                   MOVE      *ON           $FINE
051700100210     C*
051800100210     C                   ENDSR
051900100210     C/EJECT
054000100210     C************************************************************
054100100210     C* GESTIONE F24 VIDEO D1
054200100210     C************************************************************
054300100210     C     F24D1         BEGSR
054400100210     C*
054500100210     C                   MOVEA     $FX1          $FX
054600100210     C                   MOVEA     $FC1          $FC
054700100210     C                   MOVEA     $FR1          $FR
054800100210     C                   Z-ADD     $ULKD1        $ULK
054900100210     C                   EXSR      RIEKEY
055000100210     C                   Z-ADD     $ULK          $ULKD1            3 0
055100100210     C                   MOVEA     $FC           $FC1
055200100210     C                   MOVEL     $KEY1         Z1KE1
055300100210     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
055400100210     C                   MOVEL     $KEY2         Z1KE2
055500100210     C                   WRITE     FMTZ1
055600100210     C*
055700100210     C                   ENDSR
055800100210     C/EJECT
055900100210     C************************************************************
056000100210     C* CONTROLLO VIDEATA
056100100210     C************************************************************
056200100210     C     CTRD1         BEGSR
056300100210     C*
056400100210     C                   SETOFF                                       99
056500100210     C* non imposto pos. del cursore
056600100210     C                   Z-ADD     0             H1RIGA
056700100210     C                   Z-ADD     0             H1COLO
056800100216     C*
056900100210     C*
057000100210     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
057100100210     C*
057200100210     C     ANPOPZ        IFNE      '04'
057300100311     C*
057400130821     C                   EXSR      CtrAttSCF
057500110901     C*
057600130823     C     D1AttSCF      IFEQ      XScOn
057700130823     C*
057800130821     C* formato di comunicazione Elenchi Cli/For
057900130821     C                   EXSR      CtrFormato
057901131015R135 C*
057902131015 "   C* modalità di aggregazione per Doc.Riepilogo in
057903131015 "   C* caso di Formato Aggregato
057904131015R135 C                   EXSR      CtrDocRie
058000130823     C*
058100130823     C* Valore per beni
058200130823     C                   EXSR      CtrBeni
058300130823     C*
058400130823     C* Valore per servizi
058500130823     C                   EXSR      CtrServizi
058600130823     C*
058700130823     C* valrore di default tra beni/servizi
058800130823     C                   EXSR      CtrBeniSrv
058900130823     C*
059000130904     C* Identificativo del produttore del software
059100130904     C                   EXSR      CtrIdProd
059101131128R132 C*
059102131128 "   C* gestione note di variazione
059104131128R132 C                   EXSR      CtrNoteVar
059200130904     C*
059300130823     C                   ENDIF
059400100311     C*
059500100311     C                   ENDIF
059600100210     C*
059700100210     C                   ENDSR
059800100210     C/EJECT
059900100311     C************************************************************
060000100311     C* Controllo flag per modulo attivato
060100100311     C************************************************************
060200130821     C     CtrAttSCF     BEGSR
060300100311     C*
060400130821     C                   if        D1AttSCF   <> XScOn   and
060500130821     C                             D1AttSCF   <> XScOff
060600100827     C                   SetOn                                        5099
060700100311     C                   endif
060800100311     C*
060900100311     C                   ENDSR
061000100311     C/EJECT
061100111024     C************************************************************
061200130821     C* Controllo formato di comunicazione Elenchi Cli/For
061300111024     C************************************************************
061400130821     C     CtrFormato    BEGSR
061500111024     C*
061600111024     C*
061700130821     C                   Clear                   D1FormatoD
061800111024     C*
061900111024     C                   RESET                   X07VALDS
062000111024     C*
062100111024     C                   MOVE      '1'           X07RIC
062200130821     C                   MOVEL     'SDGPA08S  '  X07TRC
062300130821     C                   MOVEL     'FORMATOA08'  X07CAM
062400130821     C                   MOVEL     D1Formato     X07VAL
062500111024     C                   MOVE      *ON           X07ALL
062600111024     C                   CALLB     'X07VALR'
062700111024     C                   PARM                    X07VALDS
062800111024     C     X07ERR        IFEQ      *Off
062900130821     C                   MOVEL     X07DES        D1FormatoD
063000111024     C                   ELSE
063100130823     C                   SETON                                        5299
063200111024     C                   ENDIF
063300111024     C*
063400111024     C                   ENDSR
063500111024     C/EJECT
063501131015R135 C************************************************************
063502131015 "   C* Controllo modalità di aggregazione per doc.riepilogo
063503131015 "   C************************************************************
063504131015R135 C     CtrDocRie     BEGSR
063505131015     C*
063507131015     C                   Clear                   D1DocRieD
063508131015     C*
063509131015     C                   if        D1Formato   = 'D'
063510131015     C                   Eval      D1DocRie    = 'D'
063511131015     C                   endif
063512131015     C*
063513131015     C                   RESET                   X07VALDS
063514131015     C*
063515131015     C                   MOVE      '1'           X07RIC
063516131015     C                   MOVEL     'SDGPA08S  '  X07TRC
063517131015     C                   MOVEL     'DOCRIEA08 '  X07CAM
063518131015     C                   MOVEL     D1DocRie      X07VAL
063519131015     C                   MOVE      *ON           X07ALL
063520131015     C                   CALLB     'X07VALR'
063521131015     C                   PARM                    X07VALDS
063522131015     C     X07ERR        IFEQ      *Off
063523131015     C                   MOVEL     X07DES        D1DocRieD
063524131015     C                   ELSE
063525131015     C                   SETON                                        5399
063526131015     C                   ENDIF
063527131015     C*
063528131015R135 C                   ENDSR
063529131015R135 C/EJECT
063530131128R132 C************************************************************
063531131128 "   C* Controllo gestione note di variazione
063532131128 "   C************************************************************
063533131128R132 C     CtrNoteVar    BEGSR
063534131128     C*
063535131128     C                   Clear                   D1NoteVarD
063540131128     C*
063541131128     C                   RESET                   X07VALDS
063542131128     C*
063543131128     C                   MOVE      '1'           X07RIC
063544131128     C                   MOVEL     'SDGPA08S  '  X07TRC
063545131128     C                   MOVEL     'NOTEVARA08'  X07CAM
063546131128     C                   MOVEL     D1NoteVar     X07VAL
063547131128     C                   MOVE      *ON           X07ALL
063548131128     C                   CALLB     'X07VALR'
063549131128     C                   PARM                    X07VALDS
063550131128     C     X07ERR        IFEQ      *Off
063551131128     C                   MOVEL     X07DES        D1NoteVarD
063552131128     C                   ELSE
063553131128     C                   SETON                                        6199
063554131128     C                   ENDIF
063555131128     C*
063556131128R132 C                   ENDSR
063557131128R132 C/EJECT
063600130823     C************************************************************
063700130823     C* Controllo valore per beni
063800130823     C************************************************************
063900130823     C     CtrBeni       BEGSR
064000130823     C*
064100130823     C                   Clear                   D1BeniD
064200130823     C*
064300130823     C                   if        D1PosAtt    > *Zero
064400130823     C*
064500130823     C                   if        D1Beni      = *Blanks
064600130823     C                   SetOn                                        5799
064700130823     C                   else
064800130823     C*
064900130823     C                   MoveL(P)  D1PosAtt      Wrk6              6
065000130823     C                   Move      D1Beni        Wrk6
065100130823     C                   EXSR      CtrValAtt
065200130823     C*
065300130823     C                   if        XTBERR      = *On
065400130823     C                   SetOn                                        5899
065500130823     C                   else
065600130823     C                   MoveL     XTBUNI        ANG0A1DS
065700130823     C                   MoveL     Des0A1        D1BeniD
065800130823     C                   endif
065900130823     C*
066000130823     C                   endif
066100130823     C*
066200130823     C                   else
066300130823     C*
066400130823     C                   Eval      D1Beni      = *Blanks
066500130823     C*
066600130823     C                   endif
066700130823     C*
066800130823     C                   ENDSR
066900130823     C/EJECT
067000130823     C************************************************************
067100130823     C* Controllo valore per servizi
067200130823     C************************************************************
067300130823     C     CtrServizi    BEGSR
067400130823     C*
067500130823     C                   Clear                   D1ServizD
067600130823     C*
067700130823     C                   if        D1PosAtt    > *Zero
067800130823     C*
067900130823     C                   if        D1Servizi   = *Blanks
068000130823     C                   SetOn                                        5599
068100130823     C                   else
068200130823     C*
068300130823     C                   MoveL(P)  D1PosAtt      Wrk6              6
068400130823     C                   Move      D1Servizi     Wrk6
068500130823     C                   EXSR      CtrValAtt
068600130823     C*
068700130823     C                   if        XTBERR      = *On
068800130823     C                   SetOn                                        5699
068900130823     C                   else
069000130823     C                   MoveL     XTBUNI        ANG0A1DS
069100130823     C                   MoveL     Des0A1        D1ServizD
069200130823     C                   endif
069300130823     C*
069400130823     C                   endif
069500130823     C*
069600130823     C                   else
069700130823     C*
069800130823     C                   Eval      D1Servizi   = *Blanks
069900130823     C*
070000130823     C                   endif
070100130823     C*
070200130823     C                   ENDSR
070300130823     C/EJECT
070400130823     C************************************************************
070500130823     C* Controllo valore attributo contabile
070600130823     C************************************************************
070700130823     C     CtrValAtt     BEGSR
070800130823     C*
070900130823     C                   Clear                   SDGTABDS
071000130823     C*
071100130823     C                   Move      '1'           XTBRIC
071200130823     C                   Move      XSCSOC        XTBAZI
071300130823     C                   Move      *ZERO         XTBKEY
071400130823     C                   Move      '0A1'         XTBCOD
071500130823     C                   Move      Wrk6          XTBKey
071600130823     C*
071700130823     C                   CALL      'SDGTABDR'
071800130823     C                   PARM                    SDGTABDS
071900130823     C*
072000130823     C                   ENDSR
072100130823     C/EJECT
072200130823     C************************************************************
072300130823     C* Controllo default tra beni e servizi
072400130823     C************************************************************
072500130823     C     CtrBeniSrv    BEGSR
072600130823     C*
072700130823     C*
072800130823     C                   Clear                   D1BeniSrvD
072900130823     C*
073000130823     C                   RESET                   X07VALDS
073100130823     C*
073200130823     C                   MOVE      '1'           X07RIC
073300130823     C                   MOVEL     'SDGPA08S  '  X07TRC
073400130823     C                   MOVEL     'BENISRVA08'  X07CAM
073500130823     C                   MOVEL     D1BeniSrv     X07VAL
073600130823     C                   MOVE      *ON           X07ALL
073700130823     C                   CALLB     'X07VALR'
073800130823     C                   PARM                    X07VALDS
073900130823     C     X07ERR        IFEQ      *Off
074000130823     C                   MOVEL     X07DES        D1BeniSrvD
074100130823     C                   ELSE
074200130823     C                   SETON                                        5999
074300130823     C                   ENDIF
074400130823     C*
074500130823     C                   ENDSR
074600130823     C/EJECT
074700130904     C************************************************************
074800130904     C* Controllo Identificativo del produttore del software
074900130904     C************************************************************
075000130904     C     CtrIdProd     BEGSR
075100130904     C*
075200130904     C                   if        D1IdProd    = *Blank
075300130904     C                   Eval      D1IdProd    = XScCdF
075400130904     C                   endif
075500130904     C*
075600130904     C                   if        D1IdProd    = *Blank
075700130904     C                   SetOn                                        6099
075800130904     C                   endif
075900130904     C*
076000130904     C                   ENDSR
076100130904     C/EJECT
076200100216     C************************************************************
076300100216     C* GESTIONE VIDEO RECORD D2
076400100216     C************************************************************
076500100216     C     GESD2         BEGSR
076600100216     C*
076700100216     C* emissione piede videata se proveniente da altra
076800100216     C* salvataggio valore formato di provenienza
076900100216   B1C     $LASTV        IFNE      'D2'
077000100216     C                   WRITE     FMTT1
077100100216     C                   EXSR      GESPIE
077200100216     C                   MOVE      $LASTV        $LASTG
077300100216     C                   MOVE      'D2'          $LASTV
077400100216   E1C                   ENDIF
077500100216     C*
077600100216     C* se esistono errori sulla videata
077700100216     C* emetto la write del formato a indicatori spenti per vedere
077800100216     C* le eventuali decodifiche
077900100216     C     *IN99         IFEQ      *ON
078000100216     C     $GEST         ANDEQ     'D2'
078100100216     C                   MOVEA     *IN           WIN
078200100216     C                   MOVE      *ALL'0'       IN5098           49
078300100216     C                   MOVEA     IN5098        *IN(50)
078400100216     C                   WRITE     FMTD2
078500100216     C                   MOVEA     WIN           *IN
078600100216     C                   ENDIF
078700100216     C*
078800100216     C                   SELECT
078900100216     C* Annullamento
079000100216     C     ANPOPZ        WHENEQ    '04'
079100100216     C* Visualizzazione
079200100216     C     ANPOPZ        OREQ      '05'
079300100216     C                   WRITE     FMTD2
079400100216     C                   EXFMT     PROTECT
079500100216     C* Immissione/Modifica/Copia
079600100216     C                   OTHER
079700100216     C                   EXFMT     FMTD2
079800100216     C                   ENDSL
079900100216     C*
080000100216     C* azzero la posizione del cursore
080100100216     C                   Z-ADD     0             H1RIGA
080200100216     C                   Z-ADD     0             H1COLO
080300100216     C*
080400100216     C* controllo abilitazione tasti funzione
080500100216     C                   EXSR      CTRKD1
080600100216     C*
080700100216   B1C     *IN99         IFEQ      *OFF
080800100216     C*
080900100216   B2C                   SELECT
081000100216     C* ROLDOWN
081100100216     C     $TASTO        WHENEQ    ROLDWN
081200100216     C                   MOVE      'D1'          $GEST
081300100216     C* F3=Fine
081400100216     C     $TASTO        WHENEQ    F03
081500100216     C                   EXSR      F03D1
081600100216     C* F4=Ricerca
081700100216     C     $TASTO        WHENEQ    F04
081800100216     C                   EXSR      SEARCH
081900100216     C* F5=Refresh
082000100216     C     $TASTO        WHENEQ    F05
082100100216     C                   EXSR      F05D2
082200100216     C* F8=Successivo
082300100216     C     $TASTO        WHENEQ    F08
082400100216     C                   EXSR      F08D1
082500100216     C* F12=Ritorno
082600100216     C     $TASTO        WHENEQ    F12
082700100216     C                   EXSR      F12D1
082800100216     C* F24=Altri tasti
082900100216     C     $TASTO        WHENEQ    F24
083000100216     C                   EXSR      F24D2
083100100216     C*
083200100216     C                   OTHER
083300100216     C                   EXSR      CTRD2
083400100216     C     *IN99         IFEQ      *OFF
083500100216     C                   EXSR      CTRD1
083600100216     C     *IN99         IFEQ      *On
083700100216     C* Spengo gli indicatori di errore per il controllo su altri schermi
083800100216     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
083900100216     C* videate
084000100216     C                   MOVE      *ALL'0'       IN5098           49
084100100216     C                   MOVEA     IN5098        *IN(50)
084200100216     C                   SETON                                        9996                       96
084300100216     C                   ENDIF
084400100216     C*
084500100216     C                   ENDIF
084600100216     C*
084700100216     C*
084800100216     C     $TASTO        IFEQ      F06
084900100216     C     *IN99         ANDEQ     *OFF
085000100216     C* eseguo operazioni del dopo-aggiornamento
085100100216     C     *IN99         IFEQ      *OFF
085200100216     C                   EXSR      GESAGG
085300100216     C                   ENDIF
085400100216     C                   ENDIF
085500100216     C*
085600100216     C                   ENDSL
085700100216     C*
085800100216     C                   ENDIF
085900100216     C*
086000100216     C                   ENDSR
086100100216     C/EJECT
086200100216     C************************************************************
086300100216     C* RIEMPIMENTO VIDEATA D2
086400100216     C************************************************************
086500100216     C     RIED2         BEGSR
086600100216     C*
086700100216     C*
086800100216     C                   ENDSR
086900100216     C/EJECT
087000100216     C************************************************************
087100100216     C* GESTIONE F05 VIDEO D2
087200100216     C* il tasto F5 su qualsiasi videata provoca il refresh di tutte
087300100216     C* e il riposizionamento alla prima
087400100216     C************************************************************
087500100216     C     F05D2         BEGSR
087600100216     C*
087700100216     C                   MOVE      *ON           $INZD1
087800100216     C                   MOVE      'D1'          $GEST
087900100216     C*
088000100216     C                   ENDSR
088100100216     C/EJECT
088200100216     C************************************************************
088300100216     C* GESTIONE F24 VIDEO D2
088400100216     C************************************************************
088500100216     C     F24D2         BEGSR
088600100216     C*
088700100216     C                   MOVEA     $FX1          $FX
088800100216     C                   MOVEA     $FC1          $FC
088900100216     C                   MOVEA     $FR1          $FR
089000100216     C                   Z-ADD     $ULKD1        $ULK
089100100216     C                   EXSR      RIEKEY
089200100216     C                   Z-ADD     $ULK          $ULKD1            3 0
089300100216     C                   MOVEA     $FC           $FC1
089400100216     C                   MOVEL     $KEY1         Z1KE1
089500100216     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
089600100216     C                   MOVEL     $KEY2         Z1KE2
089700100216     C                   WRITE     FMTZ1
089800100216     C*
089900100216     C                   ENDSR
090000100216     C/EJECT
090100100216     C************************************************************
090200100216     C* CONTROLLO VIDEATA  D2
090300100216     C************************************************************
090400100216     C     CTRD2         BEGSR
090500100216     C*
090600100216     C                   SETOFF                                       99
090700100216     C*
090800100216     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
090900100216     C*
091000100216     C     ANPOPZ        IFNE      '04'
091100130821     C     D1AttSCF      andEQ     XScOn
091200100216     C*
091300100216     C                   ENDIF
091400100216     C*
091500100216     C                   ENDSR
091600100216     C/EJECT
091700100210     C************************************************************
091800100210     C* RIEMPIMENTO DS per aggiornamento
091900100210     C************************************************************
092000100210     C     RIEDAT        BEGSR
092100100311     C*
092200130821     C                   if        D1AttSCF    = XScOn
092300130821     C                   Move      *On           AttSCFA08
092400100311     C                   else
092500130821     C                   Move      *Off          AttSCFA08
092600100311     C                   endif
092700100210     C*
092800130821     C                   Eval      FormatoA08  = D1Formato
092801131015R135 C                   Eval      DocRieA08   = D1DocRie
092900130823     C*
093000130823     C                   Eval      PosAttA08   = D1PosAtt
093100130823     C                   Eval      BeniA08     = D1Beni
093200130823     C                   Eval      ServiziA08  = D1Servizi
093300130823     C                   Eval      BeniSrvA08  = D1BeniSrv
093600130904     C*
093700130904     C                   Eval      IDProdA08   = D1IdProd
093701131128R132 C*
093702131128R132 C                   Eval      NoteVarA08  = D1NoteVar
093800130904     C*
093900100210     C                   ENDSR
094000100210     C/EJECT
094100100210     C************************************************************
094200100210     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
094300100210     C************************************************************
094400100210     C     GESAGG        BEGSR
094500100210     C*
094600100210     C* Riempio campi
094700100210     C                   EXSR      RIEDAT
094800100210     C                   MOVEA     $DT           ANPDAT
094900100210     C*
095000100210     C* segnalo al pgm chiamante l'avvenuta conferma
095100100210     C                   MOVE      *ON           ANPOPR
095200100210     C*
095300100210     C                   MOVE      *ON           $FINE
095400100210     C*
095500100210     C                   ENDSR
095600100210     C/EJECT
095700100210     C************************************************************
095800100210     C* OPERAZIONI INIZIALI
095900100210     C************************************************************
096000100210     C     *INZSR        BEGSR
096100100210     C*
096200100210     C* Reperimento parametri
096300100210     C*
096400100210     C     *ENTRY        PLIST
096500100210     C                   PARM                    ANPAR
096600100210     C                   PARM                    KPJBA
096700100210     C*
096800100210     C* Reperimento dati società
096900100210     C*
097000100210     C                   MOVEL     'SOC001'      TIPXSC
097100100210     C                   MOVEL     *BLANK        SOCXSC
097200100210     C                   EXSR      REPSOC
097300100210     C     RTNXSC        IFNE      '1'
097400100210     C                   MOVEL     XSOCDS        SOC001
097500100210     C                   ELSE
097600100210     C                   RETURN
097700100210     C                   ENDIF
097800100210     C*
097900100210     C* Reperimento tasti di funzione
098000100210     C*
098100100210     C                   EXSR      REPKEY
098200100210     C*
098300100210     C                   ENDSR
098400100210     C/EJECT
098500100210     C************************************************************
098600100210     C* REPERIMENTO TASTI DI FUNZIONE
098700100210     C************************************************************
098800100210     C     REPKEY        BEGSR
098900100210     C                   MOVEA     $FM1          $FM
099000100210     C                   EXSR      REPKKK
099100100210     C                   MOVEA     $FX           $FX1
099200100210     C*
099300100210     C                   ENDSR
099400100210     C/EJECT
099500100210     C************************************************************
099600100210     C* REPERIMENTO TASTI DI FUNZIONE
099700100210     C************************************************************
099800100210     C     REPKKK        BEGSR
099900100210     C*
100000100210     C                   CLEAR                   $FX
100100100210     C     1             DO        24            X
100200100210     C*
100300100210     C     $FM(X)        IFNE      *BLANK
100400100210     C     $FC(X)        ANDNE     '0'
100500100210     C*
100600100210     C                   MOVEL     $FM(X)        IDMSG
100700100210     C                   CALLB     'XRTVM'
100800100210     C                   PARM                    IDMSG            27
100900100210     C                   PARM                    TXTMSG          100
101000100210     C                   PARM                    ERRMSG            1
101100100210     C*
101200100210     C     ERRMSG        IFNE      *ON
101300100210     C                   MOVEL     TXTMSG        $FX(X)
101400100210     C                   ELSE
101500100210     C                   MOVEL     *ALL' '       $FX(X)
101600100210     C                   ENDIF
101700100210     C*
101800100210     C                   ENDIF
101900100210     C*
102000100210     C                   ENDDO
102100100210     C*
102200100210     C                   ENDSR
102300100210     C/EJECT
102400100210     C************************************************************
102500100210     C* Reperimento dati società
102600100210     C************************************************************
102700100210     C     REPSOC        BEGSR
102800100210     C*
102900100210     C                   CALL      'SDGSOCDR'
103000100210     C                   PARM                    TIPXSC            6
103100100210     C                   PARM                    SOCXSC            3
103200100210     C                   PARM                    CDSXSC            9 0
103300100210     C                   PARM                    MODXSC            3
103400100210     C                   PARM      *BLANKS       RTNXSC            1
103500100210     C                   PARM                    XSOCDS
103600100210     C                   PARM                    KPJBA
103700100210     C*
103800100210     C                   ENDSR
103900100210     C/EJECT
104000100210     C************************************************************
104100100210     C* INIZIALIZZAZIONE VARIABILI
104200100210     C************************************************************
104300100210     C     INZVAR        BEGSR
104400100210     c*
104500100210     C                   MOVEA     ANPDAT        $DT
104600100210     C*
104700100210     C* Definizione dei tasti funzione utilizzabili a pgm
104800100210     C*
104900100210     C                   EXSR      DEFOPZ
105000100210     C*
105100100210     C* Variabili per gestione videate
105200100210     C*
105300100210     C                   MOVE      'D1'          $GEST                          fmt in gestione
105400100210     C                   MOVE      *OFF          $FINE                          fine pgm
105500100210     C                   MOVE      *ON           $INZD1                         inizializzione D1
105600100210     C                   MOVE      *BLANK        $LASTG                         fmt di provenienza
105700100210     C                   MOVE      *BLANK        $LASTV                         ultima emissione
105800100210     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
105900100210     C*
106000100210     C* Variabili appoggio
106100100210     C*
106200100210     C                   Z-ADD     0             CURR
106300100210     C                   Z-ADD     0             CURC
106400100210     C*
106500100210     C* Valorizzazione campi univoci testate
106600100210     C*
106700100210     C                   CLEAR                   FMTT1
106800100210     C                   MOVEL     XSCDSI        NOMDIT
106900100210     C                   MOVEL     KCDAZ         NOMAZN
107000100210     C                   MOVEL     KnSif         NOMSIF
107100100210     C* recupero il sottotitolo (se c'è)
107200100210     C                   MOVEL     *BLANK        T1TIT
107300100210     C     SOTTIT        IFNE      *BLANK
107400100210     C                   MOVEL     SOTTIT        IDMSG
107500100210     C                   CALLB     'XRTVM'
107600100210     C                   PARM                    IDMSG            27
107700100210     C                   PARM                    TXTMSG          100
107800100210     C                   PARM                    ERRMSG            1
107900100210     C                   PARM      *ON           CTRMSG            1            centratura
108000100210     C                   PARM      30            LENMSG            3 0          lun output.
108100100210     C     ERRMSG        IFNE      *ON
108200100210     C                   MOVEL     TXTMSG        T1TIT
108300100210     C                   ELSE
108400100210     C                   MOVEL     *ALL'?'       T1TIT
108500100210     C                   ENDIF
108600100210     C                   ENDIF
108700100210     C*
108800100210     C* Inizializzazione/Decodifica prima videata
108900100210     C*
109000100210     C                   EXSR      INZD1
109100100210     C                   MOVE      *OFF          $INZD1
109200100210     C*
109300100210     C                   ENDSR
109400100210     C/EJECT
109500100210     C************************************************************
109600100210     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
109700100210     C************************************************************
109800100210     C     DEFOPZ        BEGSR
109900100210     C*
110000100210     C* Questa routine deve valorizzare la schiera $FC1/$FR1
110100100210     C* con S o N a seconda della utilizzabilità o meno
110200100210     C* del tasto funzione correlato
110300100210     C*
110400100210     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
110500100210     C* di compilazione
110600100210     C                   RESET                   $FC1
110700100210     C                   RESET                   $FR1
110800100210     C*
110900100210     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
111000100210     C* invalido alcune opzioni
111100100210     C                   SELECT
111200100210     C* Se pgm richiamato in interrogazione disattivo :
111300100210     C     ANPOPZ        WHENEQ    '05'
111400100210     C* F5=Ripristino
111500100210     C                   Z-ADD     5             X
111600100210     C                   MOVE      '0'           $FC1(X)
111700100210     C* Se pgm richiamato in annullamento disattivo :
111800100210     C     ANPOPZ        WHENEQ    '04'
111900100210     C* F5=Ripristino
112000100210     C                   Z-ADD     5             X
112100100210     C                   MOVE      '0'           $FC1(X)
112200100210     C*
112300100210     C                   OTHER
112400100210     C*
112500100210     C                   ENDSL
112600100210     C*
112700100210     C                   ENDSR
112800100210     C************************************************************
112900100210     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
113000100210     C************************************************************
113100100210     C     DEFCAM        BEGSR
113200100210     C*
113300100210     C* Variabili per gestione videate
113400100210     C*
113500100210     C                   MOVE      *BLANK        $GEST             2            fmt in gestione
113600100210     C                   MOVE      *BLANK        $FINE             1            fine pgm
113700100210     C                   MOVE      *BLANK        $INZD1            1            inizializzione D1
113800100210     C                   MOVE      *BLANK        $LASTG            2            fmt di provenienza
113900100210     C                   MOVE      *BLANK        $LASTV            2            ultima emissione
114000100210     C                   Z-ADD     0             $PIED1            3 0          piede da emett. D1
114100100210     C*
114200100210     C* Variabili appoggio
114300100210     C*
114400100210     C                   Z-ADD     0             CURR              2 0
114500100210     C                   Z-ADD     0             CURC              2 0
114600100210     C                   MOVE      *ZEROS        WIN              99
114700100210     C                   MOVE      *BLANK        WTBERR            1
114800100210     C*
114900100210     C* Indici
115000100210     C*
115100100210     C                   Z-ADD     0             X                 3 0
115200100210     C                   Z-ADD     0             Y                 3 0
115300100210     C*
115400100210     C                   ENDSR
115500100210     C/EJECT
115600100210     C************************************************************
115700100210     C* CONTROLLO ABILITAZIONE TASTI
115800100210     C************************************************************
115900100210     C     CTRKEY        BEGSR
116000100210     C                   SELECT
116100100210     C     $TASTO        WHENEQ    F01
116200100210     C     $FC(1)        IFEQ      '0'
116300100210     C                   SETON                                        99
116400100210     C                   ENDIF
116500100210     C     $TASTO        WHENEQ    F02
116600100210     C     $FC(2)        IFEQ      '0'
116700100210     C                   SETON                                        99
116800100210     C                   ENDIF
116900100210     C     $TASTO        WHENEQ    F03
117000100210     C     $FC(3)        IFEQ      '0'
117100100210     C                   SETON                                        99
117200100210     C                   ENDIF
117300100210     C     $TASTO        WHENEQ    F04
117400100210     C     $FC(4)        IFEQ      '0'
117500100210     C                   SETON                                        99
117600100210     C                   ENDIF
117700100210     C     $TASTO        WHENEQ    F05
117800100210     C     $FC(5)        IFEQ      '0'
117900100210     C                   SETON                                        99
118000100210     C                   ENDIF
118100100210     C     $TASTO        WHENEQ    F06
118200100210     C     $FC(6)        IFEQ      '0'
118300100210     C                   SETON                                        99
118400100210     C                   ENDIF
118500100210     C     $TASTO        WHENEQ    F07
118600100210     C     $FC(7)        IFEQ      '0'
118700100210     C                   SETON                                        99
118800100210     C                   ENDIF
118900100210     C     $TASTO        WHENEQ    F08
119000100210     C     $FC(8)        IFEQ      '0'
119100100210     C                   SETON                                        99
119200100210     C                   ENDIF
119300100210     C     $TASTO        WHENEQ    F09
119400100210     C     $FC(09)       IFEQ      '0'
119500100210     C                   SETON                                        99
119600100210     C                   ENDIF
119700100210     C     $TASTO        WHENEQ    F10
119800100210     C     $FC(10)       IFEQ      '0'
119900100210     C                   SETON                                        99
120000100210     C                   ENDIF
120100100210     C     $TASTO        WHENEQ    F11
120200100210     C     $FC(11)       IFEQ      '0'
120300100210     C                   SETON                                        99
120400100210     C                   ENDIF
120500100210     C     $TASTO        WHENEQ    F12
120600100210     C     $FC(12)       IFEQ      '0'
120700100210     C                   SETON                                        99
120800100210     C                   ENDIF
120900100210     C     $TASTO        WHENEQ    F13
121000100210     C     $FC(13)       IFEQ      '0'
121100100210     C                   SETON                                        99
121200100210     C                   ENDIF
121300100210     C     $TASTO        WHENEQ    F14
121400100210     C     $FC(14)       IFEQ      '0'
121500100210     C                   SETON                                        99
121600100210     C                   ENDIF
121700100210     C     $TASTO        WHENEQ    F15
121800100210     C     $FC(15)       IFEQ      '0'
121900100210     C                   SETON                                        99
122000100210     C                   ENDIF
122100100210     C     $TASTO        WHENEQ    F16
122200100210     C     $FC(16)       IFEQ      '0'
122300100210     C                   SETON                                        99
122400100210     C                   ENDIF
122500100210     C     $TASTO        WHENEQ    F17
122600100210     C     $FC(17)       IFEQ      '0'
122700100210     C                   SETON                                        99
122800100210     C                   ENDIF
122900100210     C     $TASTO        WHENEQ    F18
123000100210     C     $FC(18)       IFEQ      '0'
123100100210     C                   SETON                                        99
123200100210     C                   ENDIF
123300100210     C     $TASTO        WHENEQ    F19
123400100210     C     $FC(19)       IFEQ      '0'
123500100210     C                   SETON                                        99
123600100210     C                   ENDIF
123700100210     C     $TASTO        WHENEQ    F20
123800100210     C     $FC(20)       IFEQ      '0'
123900100210     C                   SETON                                        99
124000100210     C                   ENDIF
124100100210     C     $TASTO        WHENEQ    F21
124200100210     C     $FC(21)       IFEQ      '0'
124300100210     C                   SETON                                        99
124400100210     C                   ENDIF
124500100210     C     $TASTO        WHENEQ    F22
124600100210     C     $FC(22)       IFEQ      '0'
124700100210     C                   SETON                                        99
124800100210     C                   ENDIF
124900100210     C     $TASTO        WHENEQ    F23
125000100210     C     $FC(23)       IFEQ      '0'
125100100210     C                   SETON                                        99
125200100210     C                   ENDIF
125300100210     C     $TASTO        WHENEQ    F24
125400100210     C     $FC(24)       IFEQ      '0'
125500100210     C                   SETON                                        99
125600100210     C                   ENDIF
125700100210     C                   ENDSL
125800100210     C                   ENDSR
125900100210     C/EJECT
126000100210     C************************************************************
126100100210     C* RIEMPIMENTO TASTI FUNZIONE
126200100210     C************************************************************
126300100210     C     RIEKEY        BEGSR
126400100210     C*
126500100210     C* Input :
126600100210     C* - $FX = schiera con tutti i testi disponibili
126700100210     C* - $FC = schiere con i flag di validità associati ai testi
126800100210     C* - $FR = schiere con i flag di REVERSE IMAGE
126900100210     C* - $ULK = ultimo testo caricato
127000100210     C* - $L01 = Lunghezza prima riga
127100100210     C* - $L02 = Lunghezza seconda riga
127200100210     C* Output :
127300100210     C* - $ULK
127400100210     C* - $KEY1= 1° riga di tasti funzione
127500100210     C* - $KEY2= 2° riga di tasti funzione
127600100210     C*
127700100210     C                   CALLB     'XKEYMSG'
127800100210     C* input
127900100210     C                   PARM                    $FX
128000100210     C                   PARM                    $FC
128100100210     C                   PARM                    $FR
128200100210     C                   PARM                    $ULK              3 0
128300100210     C* output
128400100210     C                   PARM                    $KEY1            79
128500100210     C                   PARM                    $KEY2            79
128600100210     C* input
128700100210     C                   PARM      79            $L01              3 0
128800100210     C                   PARM      79            $L02              3 0
128900100210     C*
129000100210     C                   PARM                    $ALLFUNCT       480
129100100210     C*
129200100210     C                   ENDSR
129300100210     C************************************************************
129400100210**   C
12950010021001SDGMSG    *LIBL     SDC0002    inserimento
12960010021002PROMSG    *LIBL     COS0A02    modifica
12970010021003PROMSG    *LIBL     COS0003    copia
12980010021004PROMSG    *LIBL     COS0004    annullamento
12990010021005PROMSG    *LIBL     COS0005    visualizzazione
130000100210** TASTI DI FUNZIONE  D1
130100100210PROMSG    *LIBL     KEY0001  01
130200100210PROMSG    *LIBL     KEY0A02  02
130300100210PROMSG    *LIBL     KEY0003  03
130400100210PROMSG    *LIBL     KEY0004  04
130500100210PROMSG    *LIBL     KEY0005  05
130600100210PROMSG    *LIBL     KEY0006  06
130700100210PROMSG    *LIBL     KEY0007  07
130800100210PROMSG    *LIBL     KEY0008  08
130900100210PROMSG    *LIBL     KEY0009  09
131000100210PROMSG    *LIBL     KEY0010  10
131100100210PROMSG    *LIBL     KEY0011  11
131200100210PROMSG    *LIBL     KEY0012  12
131300100210PROMSG    *LIBL     KEY0013  13
131400120206SDGMSG    *LIBL     SDK0015  14
131500100210PROMSG    *LIBL     KEY0015  15
131600100210PROMSG    *LIBL     KEY0016  16
131700100210SDGMSG    *LIBL     SDK0001  17
131800100210PROMSG    *LIBL     KEY0018  18
131900100210PROMSG    *LIBL     KEY0019  19
132000100210PROMSG    *LIBL     KEYA020  20
132100100210PROMSG    *LIBL     KEYA021  21
132200100210PROMSG    *LIBL     KEYA022  22
132300100210PROMSG    *LIBL     KEYA023  23
132400100210PROMSG    *LIBL     KEYA024  24
132500100210**  ABILITAZIONE D1
1326001002100N  01
1327001002100N  02
1328001002100N  03
1329001002101N  04
1330001002101N  05
1331001002101N  06
1332001002100N  07
1333001002100N  08
1334001002100N  09
1335001002100N  10
1336001002100N  11
1337001002101N  12
1338001002100N  13
1339001309270N  14
1340001002100N  15
1341001002100N  16
1342001002100N  17
1343001002100N  18
1344001002100N  19
1345001002100N  20
1346001002100N  21
1347001002100N  22
1348001002100N  23
1349001002100N  24   Lasciare sempre impostata a '0' la colonna $FC
