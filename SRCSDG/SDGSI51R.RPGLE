000100100705     H*PARMS OPTION(*NOXREF *NODEBUGIO) DFTACTGRP(*NO)
000200100705     H*PARMS BNDDIR(PJXBND PJCBND)
000300100705     H*PARMS ACTGRP(QILE)
000400100705      *****************************************************************
000500170823     H DECEDIT('0,') DATEDIT(*DMY.) BNDDIR('PJXBND':'PJCBND')
000501170823     H actgrp('QILE')  DFTACTGRP(*NO)
000600100705      *****************************************************************
000700100705      *
000800170526      *                Spesometro trimestrale XML
000900100705      *                    Lancio estrazione
001000100705      *           -------------------------------------
001100170529      *
001200170529R258  * Questo pgm è stato creato con il task R258 Comunicaz. Spesometro Trimestrale
001300100705      *
001400100705      *****************************************************************
001500100705      *------------
001600170526     FSDGSI51D  CF   E             WORKSTN
001700170526     F                                     RENAME(SI51T1:FMTT1)
001800170526     F                                     RENAME(SI51D1:FMTD1)
001900170526     F                                     RENAME(SI51Z1:FMTZ1)
002000100705     F* modifica valori assunti
002100170526     F                                     RENAME(SI51D3:FMTD3)
002200170526     F                                     RENAME(SI51Z3:FMTZ3)
002300100705     F                                     INFDS(DSFMT)
002400100705      *****************************************************************
002500100705     D* Schiere per la gestione dei tasti di funzione
002600100705     D $FM             S             27    DIM(24)
002700100705     D $FX             S             50    DIM(24)
002800100705     D $FC             S              1    DIM(24)
002900100705     D $FR             S              1    DIM(24)
003000100705     D* Video D1
003100100705     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
003200100705     D $FX1            S             50    DIM(24) ALT($FM1)
003300100705     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
003400100705     D $FR1            S              1    DIM(24) ALT($FC1)
003500100705     D* Video D3 Valori assunti
003600100705     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
003700100705     D $FX3            S             50    DIM(24) ALT($FM3)
003800100705     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
003900100705     D $FR3            S              1    DIM(24) ALT($FC3)
004000100705     D*-------------
004100100705     D* Passaggio Parametri
004200100705     D KPJBA         E DS
004300100705     D*-------------
004400100705     D* Valori assunti
004500100705     D XMPEDS        E DS                  INZ
004600100705     D XMPEDS1       E DS                  INZ
004700100705     D DSMPE           DS                  INZ
004800100705     D  $VAL                          1
004900100820     D $OkExeLan       S              1
005000100820     D $P1COS2161      S             20A
005100100830     D $P1PRO2184      S             10A
005200100820     D*----------------
005300100820     D* ds per nome file
005400100820     D                 DS
005500100820     D  WAnno                  3      6  0
005600100820     D  WPrgInvio              9     10  0
005700100820     D  WNumero                1     10
005800100705     D*-------------
005900100705     D* Attributi video
006000100705     D Normale         C                   Const(X'32')
006100100705     D Protetto        C                   Const(X'A0')
006200100705     D*-------------
006300100705     D* Reperimento nome PGM
006400100705     D STATUS         SDS           333
006500100705     D  DSPGM            *PROC
006600100705     D  PARMS            *PARMS
006700100705     D*-------------
006800100705     D* Dati di ambiente ottenuti da SDGSOCDR
006900100705     D SOC001        E DS                  EXTNAME(SDGSOCDS )
007000100705     D*-------------
007100100705     D* DS Interna per dati di output di SDGSOCDR
007200100705     D XSOCDS          DS          1000
007300100820     D*-------------
007400100820     D* DS Esterna gestione messaggi
007500100820     D SDGMSGDS      E DS                  INZ
007600100820     D  STS          E                     EXTFLD(MSGSTS)
007700100820     D                                     DIM(333)
007800100820     D  JBA          E                     EXTFLD(MSGJBA)
007900100820     D                                     DIM(502)
008000100705     D*-------------
008100100705     D DSFMT           DS
008200100705     D  $TASTO               369    369
008300100705     D  NRG                  370    370
008400100705     D  NCL                  371    371
008500100705     D*-------------
008600100705     D* posizione cursore
008700100705     D CURSOR          DS
008800100705     D  RIRI                   1      2B 0 INZ
008900100705     D  R$$                    2      2
009000100705     D  COCO                   3      4B 0 INZ
009100100705     D  C$$                    4      4
009200170529     D*-------------
009300170529     D* Parametri modulo SDSPETRI
009400170529     D SDGPA12DS     E DS
009500170529     D*-------------
009600170529     D* Parametri modulo SCARSUMA
009700170529     D ANP022DS      E DS
009800170529     D*-------------
009900170529     D* Passaggio Parametri al PGM BATCH
010000170529     D SDGSI52DS     E DS
010100170529     D  RETI52       E                     INZ('0')
010200170529     D  OPRI52       E                     INZ('0')
010300170529     D  ERRI52       E                     INZ('0')
010400100705     D*-------------
010500100705     D* Tasti di funzione
010600100705     D F01             C                   CONST(X'31')
010700100705     D F02             C                   CONST(X'32')
010800100705     D F03             C                   CONST(X'33')
010900100705     D F04             C                   CONST(X'34')
011000100705     D F05             C                   CONST(X'35')
011100100705     D F06             C                   CONST(X'36')
011200100705     D F07             C                   CONST(X'37')
011300100705     D F08             C                   CONST(X'38')
011400100705     D F09             C                   CONST(X'39')
011500100705     D F10             C                   CONST(X'3A')
011600100705     D F11             C                   CONST(X'3B')
011700100705     D F12             C                   CONST(X'3C')
011800100705     D F13             C                   CONST(X'B1')
011900100705     D F14             C                   CONST(X'B2')
012000100705     D F15             C                   CONST(X'B3')
012100100705     D F16             C                   CONST(X'B4')
012200100705     D F17             C                   CONST(X'B5')
012300100705     D F18             C                   CONST(X'B6')
012400100705     D F19             C                   CONST(X'B7')
012500100705     D F20             C                   CONST(X'B8')
012600100705     D F21             C                   CONST(X'B9')
012700100705     D F22             C                   CONST(X'BA')
012800100705     D F23             C                   CONST(X'BB')
012900100705     D F24             C                   CONST(X'BC')
013000100705     D ENTER           C                   CONST(X'F1')
013100100705     D ROLDWN          C                   CONST(X'F4')
013200100705     D ROLLUP          C                   CONST(X'F5')
013300100705     D*-------------
013400170529     D* Sottotitolo di testata
013500170529     D SOTTITD1        C                   CONST('SDGMSG    *LIBL-
013600170529     D                                          SDC0053')
013700170529     D*-------------
013800170529     D* Sottotitolo di testata
013900170529     D SOTTITT1        C                   CONST('               -
014000170529     D                                                 ')
014100100705     D*-------------
014200131004     D* Maschera dati autorizzazioni
014300100705     D CONCIVDS      E DS
014400100705     D*-------------
014500100705     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
014600100705     D*-------------
014700100705     D* Passaggio Autorizzazioni
014800100705     D DSAUT           DS           512
014900100705     D*-------------
015000100705     D* Dati per pgm XNOAUT
015100100705     D XNOADS        E DS                  EXTNAME(XNOAUTDS)
015200100705     D*-------------
015300100705     D* Ricerca legende
015400100705     D Ana964DS      E DS
015500100705     D*-------------
015600100705     D* Ricerca contabilità
015700100705     D ANGG84DS      E DS
015800100705     D*-------------
015900100705     D* Autorizzazioni alla contabilità
016000100705     D XCtbDS        E DS                  EXTNAME(XAUTCTBDS)
016100100705     D*-------------
016200100705     D* Parametri per ricerca codice ANATB00F
016300170529     D XTabDS        E DS                  inz
016400170529     D*-------------
016500170529     D* Ricerca codici Stati
016600170529     D ANGG12DS      E DS
016700100705     D*-------------
016800100705     D* Controlli validità legende
016900100705     D X07ValDS      E DS
017000100705     D*-------------
017100100705     D* Richiamo a SDGTABDR
017200100705     D SDGTABDS      E DS                  inz
017300170529     D*-------------
017400100705     D* DS esterna per dati soggetti
017500100705     D DVASOG        E DS                  INZ
017600100705     D*-------------
017700100705     D* Definizioni parametri driver anagrafici
017800100705     D Dvaric          S              1
017900100705     D DvaSocieta      S              3
018000100705     D DvaUnita        S              8
018100100705     D DvaStrutt       S             10
018200100705     D DvaDtRif        S               D
018300100705     D DvaLenOut       S              9B 0
018400100705     D DvaSnatura      S              1
018500100705     D DvaCodice       S              8
018600100705     D DvaLineav       S              3
018700100705     D DvaFiliale      S              3  0
018800100705     D DvaSogg         S              8
018900100705     D DvaTpInd        S              2
019000100705     D DvaCdInd        S              3
019100100705     D DvaErrore       S              1
019200100705     D DvaOutput       S           4000
019300100705     D*-------------
019400100705     D* Controllo conti
019500100705     D ND002DS       E DS                  INZ
019600100705     D*-------------
019700100705     D* Definizioni parametri standard chiamate moduli
019800100705     D Esito           S              1
019900100705     D GesMsg          S              1
020000100705     D LenOut          S              9B 0
020100100705     D Data            S             10
020200100705     D StrutturaO      S             10
020300100705     D*-------------
020400100705     D* ricerca soggetti
020500100705     D ANA040DS      E DS                  INZ
020600100705     D*-------------
020700100705     D* Ricerca capoconto
020800100705     D ANA200DS      E DS                  INZ
020900100705     D  RET200       E                     INZ('0')
021000100705     D  OPR200       E                     INZ('0')
021100100705     D  ERR200       E                     INZ('0')
021200100705     D  CMT200       E                     INZ('0')
021300100706     D*-------------
021400100706     D* Passaggio Parametri al PGM clienti
021500100706     D ANA300DS      E DS
021600100706     D  CMT300       E                     INZ('0')
021700100706     D  RET300       E                     INZ('0')
021800100706     D  OPR300       E                     INZ('0')
021900100706     D  ERR300       E                     INZ('0')
022000100706     D  FSC300       E                     INZ('S')
022100100706     D*-------------
022200100706     D* Passaggio Parametri al PGM fornitori
022300100706     D ANA400DS      E DS
022400100706     D  CMT400       E                     INZ('0')
022500100706     D  RET400       E                     INZ('0')
022600100706     D  OPR400       E                     INZ('0')
022700100706     D  ERR400       E                     INZ('0')
022800100706     D  FSC400       E                     INZ('S')
022900100705     D*-------------
023000100705     D WSogg           S                   Like(DVSSogg)
023100100705     D WKcc            S              6A
023200100705     D WKsc            S              8A
023300100705     D*-------------
023400170529     D UDateISO        S               D    datfmt(*iso)
023500170529     D*-------------
023600170529     D* tipo comunicazione Spesometro
023700170529     D WTpCom          S              1A
023800170529     C******************************************************
023900100705     C*                MAIN LINE PROGRAM
024000100705     C******************************************************
024100100705     C*
024200100705     C* Inizializzazioni
024300100705     C                   EXSR      INZVAR
024400100705     C*
024500100705     C     $FINE         DOWEQ     *OFF
024600100705     C     $GEST         CASEQ     'D1'          GESD1
024700100705     C     $GEST         CASEQ     'D3'          GESD3
024800100705     C                   ENDCS
024900100705     C                   ENDDO
025000100705     C*
025100100705     C* fine programma
025200100705     C                   EXSR      ENDPGM
025300100705     C/EJECT
025400100705     C************************************************************
025500100705     C* Fine pgm
025600100705     C************************************************************
025700100705     C     ENDPGM        BEGSR
025800100705     C*
025900100705     C* se ho confermato l'uscita proseguo
026000100705     C     $FINE         IFEQ      *ON
026100100705     C*
026200100705     C                   SETON                                            LR
026300100705     C                   RETURN
026400100705     C*
026500100705     C                   ENDIF
026600100705     C*
026700170526     C                   ENDSR
026800100705     C/EJECT
026900100705     C************************************************************
027000100705     C* RICERCHE
027100100705     C************************************************************
027200100705     C     SEARCH        BEGSR
027300100705     C*
027400100705     C* determino Riga/Colonna del cursore
027500100705     C                   MOVE      NRG           R$$
027600100705     C                   MOVE      NCL           C$$
027700100705     C                   Z-ADD     RIRI          CURR
027800100705     C                   Z-ADD     COCO          CURC
027900100705     C*
028000100705     C                   EXSR      CallAllinea
028100100705     C*
028200100705     C                   SELECT
028300100705     C*
028400100705     C*  Contabilità civilistica
028500100705     C     H1NMFL        WHENEQ    'D1CTB'
028600100705     C                   EXSR      F4_Ctb
028700100715     C*
028800100715     C*  Tipo di registrazione contabili
028900100715     C     H1NMFL        WHENEQ    'D1TPREGZ'
029000100715     C                   EXSR      F4_TpRegz
029100170526     C*
029200170526     C*  Trimestre
029300170526     C     H1NMFL        WHENEQ    'D1TRIM'
029400170526     C                   EXSR      F4_Trim
029500170717     C*
029600170717     C*  Tipo di estrazione
029700170717     C     H1NMFL        WHENEQ    'D1TPREC'
029800170717     C                   EXSR      F4_TpRec
029900170717     C*
030000170717     C*  Stato
030100170717     C     H1NMFL        WHENEQ    'D1STATO2'
030200170717     C                   EXSR      F4_Stato2
030300170529     C*
030400100705     C*  Natura soggetto
030500100705     C     H1NMFL        WHENEQ    'D1CF'
030600100705     C                   EXSR      F4_CF
030700100705     C*
030800170714     C*  Partita IVA
030900170714     C     H1NMFL        WHENEQ    'D1PARTIVA'
031000170714     C                   EXSR      F4_PARTIVA
031100100705     C*
031200100705     C*  Codice fiscale
031300170529     C     H1NMFL        WHENEQ    'D1CDFISC'
031400170529     C                   EXSR      F4_CdFisc
031500100705     C*
031600100705     C*  Soggetto
031700100705     C     H1NMFL        WHENEQ    'D1SOGG'
031800100705     C                   EXSR      F4_Sogg
031900100705     C*
032000100705     C*  Capoconti di estrazione
032100100705     C     H1NMFL        WHENEQ    'D1KCC1'
032200100705     C     H1NMFL        OREQ      'D1KCC2'
032300100705     C     H1NMFL        OREQ      'D1KCC3'
032400100705     C     H1NMFL        OREQ      'D1KCC4'
032500100705     C     H1NMFL        OREQ      'D1KCC5'
032600100705     C     H1NMFL        OREQ      'D1KCC6'
032700100705     C                   EXSR      F4_Kcc
032800100705     C*
032900100706     C*  conto di estrazione singolo - cliente
033000100705     C     H1NMFL        WHENEQ    'D1KSC'
033100100706    >C     D1CF          ANDEQ     'C'
033200100706     C                   EXSR      F4_Cliente
033300100706     C*
033400100706     C*  conto di estrazione singolo - fornitore
033500100706     C     H1NMFL        WHENEQ    'D1KSC'
033600100706    >C     D1CF          ANDEQ     'F'
033700100706     C                   EXSR      F4_Fornitore
033800100706     C*
033900100706     C*  conto di estrazione singolo - manca la sottonatura
034000100706     C     H1NMFL        WHENEQ    'D1KSC'
034100100706     C     D1CF          ANDEQ     *Blank
034200100706     C                   SetOn                                        6799
034300170529     C*
034400100705     C* ricerca non abilitata in questa posizione
034500100705     C                   OTHER
034600100705     C* imposto pos. del cursore
034700100705     C                   Z-ADD     CURR          H1RIGA
034800100705     C                   Z-ADD     CURC          H1COLO
034900100705     C                   SETON                                        9899
035000100705     C*
035100100705     C                   ENDSL
035200100705     C*
035300100705     C                   MOVE      *HIVAL        $LASTV
035400100705     C*
035500170526     C                   ENDSR
035600100705      /EJECT
035700100705     C************************************************************
035800100705     C* Richiamo programma per allineare i codici
035900100705     C************************************************************
036000100705     C     CallAllinea   BEGSR
036100100705     C*
036200100705     C     D1SOGG        IFNE      *BLANK
036300100705     C                   CALLb     'XALLINEA'
036400100705     C                   PARM                    D1SOGG
036500100705     C                   PARM      8             LEN               5 0
036600100705     C                   PARM                    XSCINP
036700100705     C                   ENDIF
036800100705     C*
036900100705     C     D1KCC1        IFNE      *BLANKS
037000100705     C                   CALLB     'XALLINEA'
037100100705     C                   PARM                    D1KCC1
037200100705     C                   PARM      6             LEN               5 0
037300100705     C                   PARM                    XSCINP
037400100705     C                   ENDIF
037500100705     C*
037600100705     C     D1KCC2        IFNE      *BLANKS
037700100705     C                   CALLB     'XALLINEA'
037800100705     C                   PARM                    D1KCC2
037900100705     C                   PARM      6             LEN               5 0
038000100705     C                   PARM                    XSCINP
038100100705     C                   ENDIF
038200100705     C*
038300100705     C     D1KCC3        IFNE      *BLANKS
038400100705     C                   CALLB     'XALLINEA'
038500100705     C                   PARM                    D1KCC3
038600100705     C                   PARM      6             LEN               5 0
038700100705     C                   PARM                    XSCINP
038800100705     C                   ENDIF
038900100705     C*
039000100705     C     D1KCC4        IFNE      *BLANKS
039100100705     C                   CALLB     'XALLINEA'
039200100705     C                   PARM                    D1KCC4
039300100705     C                   PARM      6             LEN               5 0
039400100705     C                   PARM                    XSCINP
039500100705     C                   ENDIF
039600100705     C*
039700100705     C     D1KCC5        IFNE      *BLANKS
039800100705     C                   CALLB     'XALLINEA'
039900100705     C                   PARM                    D1KCC5
040000100705     C                   PARM      6             LEN               5 0
040100100705     C                   PARM                    XSCINP
040200100705     C                   ENDIF
040300100705     C*
040400100705     C     D1KCC6        IFNE      *BLANKS
040500100705     C                   CALLB     'XALLINEA'
040600100705     C                   PARM                    D1KCC6
040700100705     C                   PARM      6             LEN               5 0
040800100705     C                   PARM                    XSCINP
040900100705     C                   ENDIF
041000100705     C*
041100100705     C     D1KSC         IFNE      *BLANKS
041200100705     C                   CALLB     'XALLINEA'
041300100705     C                   PARM                    D1KSC
041400100705     C                   PARM      8             LEN               5 0
041500100705     C                   PARM                    XSCINP
041600100705     C                   ENDIF
041700100705     C*
041800170526     C                   ENDSR
041900100705      /EJECT
042000100705     C************************************************************
042100100705     C* Ricerca contabilità civilistica
042200100705     C************************************************************
042300100705     C     F4_Ctb        BEGSR
042400100705     C*
042500100705     C                   Reset                   XTabDS
042600100705     C*
042700100705     C                   Move      '05'          XTaOpz
042800100705     C                   Move      XScSoc        XTaAzi
042900100705     C                   Move      XScLin        XTaLin
043000100705     C                   Z-add     Curr          XTaRig
043100100705     C                   Z-add     Curc          XTaCol
043200100705     C                   Move      *Blank        XTaKey
043300100705     C*
043400100705     C                   Clear                   KPJBU
043500100705     C                   MoveL     XTabDS        KPJBU
043600100705     C*
043700100705     C                   CALL      'ANGG84R'
043800100705     C                   PARM                    KPJBA
043900100705     C                   MoveL     KPJBU         XTabDS
044000100705     C*
044100100705     C                   if        XTaErr <> '1'  and
044200100705     C                             XTaKey <> *blank
044300100705     C                   Move      XTaKey        D1Ctb
044400100705     C                   endif
044500100705     C*
044600170526     C                   ENDSR
044700100705      /EJECT
044800100715     C************************************************************
044900100715     C* Ricerca tipo registrazione
045000100715     C************************************************************
045100100715     C     F4_TpRegz     BEGSR
045200100715     C*
045300100715     C                   Reset                   ANA964DS
045400100715     C*
045500100715     C                   MoveL     '05'          Opz964
045600100715     C                   MoveL     'ANDIZ000'    Trc964
045700100715     C                   MoveL     'DSPTPREGZ'   Cam964
045800100715     C*
045900100715     C                   MoveL     ANA964DS      KPJBU
046000100715     C                   Call      'ANA964R'
046100100715     C                   Parm                    KPJBA
046200100715     C                   MoveL     KPJBU         ANA964DS
046300100715     C*
046400100715     C                   if        Ret964     <> '2'
046500100715     C                   MoveL     Val964        D1TpRegz
046600100715     C                   endif
046700100715     C*
046800170526     C                   ENDSR
046900100715      /EJECT
047000170526     C************************************************************
047100170526     C* Ricerca trimestre di competenza
047200170526     C************************************************************
047300170526     C     F4_Trim       BEGSR
047400140327     C*
047500140327     C                   Reset                   ANA964DS
047600140327     C*
047700140327     C                   MoveL     '01'          Opz964
047800140327     C                   MoveL     'ANDIZ000'    Trc964
047900170529     C                   MoveL     'SDGTRIM  '   Cam964
048000140327     C                   MoveL     *On           All964
048100140327     C*
048200140327     C                   MoveL     ANA964DS      KPJBU
048300140327     C                   Call      'ANA964R'
048400140327     C                   Parm                    KPJBA
048500140327     C                   MoveL     KPJBU         ANA964DS
048600140327     C*
048700170526     C                   if        Ret964     <> '2'
048800170526     C                   MoveL     Val964        D1Trim
048900170526     C                   endif
049000140327     C*
049100170526     C                   ENDSR
049200170526      /EJECT
049300170717     C************************************************************
049400170717     C* Ricerca tipo di estrazione dati
049500170717     C************************************************************
049600170717     C     F4_TpRec      BEGSR
049700170717     C*
049800170717     C                   Reset                   ANA964DS
049900170717     C*
050000170717     C                   MoveL     '01'          Opz964
050100170717     C                   MoveL     'SDGSI52S '   Trc964
050200170717     C                   MoveL     'TPRECI52 '   Cam964
050300170717     C                   MoveL     *On           All964
050400170717     C*
050500170717     C                   MoveL     ANA964DS      KPJBU
050600170717     C                   CALL      'ANA964R'
050700170717     C                   PARM                    KPJBA
050800170717     C                   MoveL     KPJBU         ANA964DS
050900170717     C*
051000170717     C                   if        Ret964     <> '2'
051100170717     C                   MoveL     Val964        D1TpRec
051200170717     C                   endif
051300170717     C*
051400170717     C                   ENDSR
051500170717      /EJECT
051600170717     C************************************************************
051700170717     C* Ricerca codice Stato di residenza del conto
051800170717     C************************************************************
051900170717     C     F4_Stato2     BEGSR
052000170717     C*
052100170717     C                   Reset                   XTabDS
052200170717     C*
052300170717     C                   Move      '01'          XTaOpz
052400170717     C                   Move      XScSoc        XTaAzi
052500170717     C                   Move      XScLin        XTaLin
052600170717     C                   Z-add     Curr          XTaRig
052700170717     C                   Z-add     Curc          XTaCol
052800170717     C                   Move      *Blank        XTaKey
052900170717     C*
053000170717     C                   Clear                   KPJBU
053100170717     C                   MoveL     XTabDS        KPJBU
053200170717     C*
053300170717     C                   CALL      'ANGG12R'
053400170717     C                   PARM                    KPJBA
053500170717     C*
053600170717     C                   MoveL     KPJBU         XTabDS
053700170717     C*
053800170717     C                   if        XTaErr     <> '1'  and
053900170717     C                             XTaKey     <> *Blank
054000170717     C                   Move      XTaKey        D1Stato2
054100170717     C                   endif
054200170717     C*
054300170717     C                   ENDSR
054400170717      /EJECT
054500100705     C************************************************************
054600100705     C* Ricerca natura del conto
054700100705     C************************************************************
054800100705     C     F4_CF         BEGSR
054900100705     C*
055000100705     C                   Reset                   ANA964DS
055100100705     C*
055200100705     C                   MoveL     '01'          Opz964
055300100705     C                   MoveL     'ANDIZ000'    Trc964
055400100705     C                   MoveL     'DSPCLIFOR'   Cam964
055500100705     C                   MoveL     *On           All964
055600100705     C*
055700100705     C                   MoveL     ANA964DS      KPJBU
055800100705     C                   CALL      'ANA964R'
055900100705     C                   PARM                    KPJBA
056000100705     C                   MoveL     KPJBU         ANA964DS
056100100705     C*
056200100705     C                   if        Ret964     <> '2'
056300100705     C                   MoveL     Val964        D1CF
056400100705     C                   endif
056500100705     C*
056600170526     C                   ENDSR
056700100705      /EJECT
056800100705     C************************************************************
056900170714     C* Ricerca Partita IVA
057000100705     C************************************************************
057100170714     C     F4_PartIVA    BEGSR
057200100705     C*
057300100705     C                   RESET                   ANA040DS
057400100705     C*
057500100705     C                   MOVEL     '01'          OPZ040
057600170714     C                   MoveL     D1PartIVA     IVA040
057700100705     C                   MOVEL     ANA040DS      KPJBU
057800100705     C                   CALL      'ANA040R'
057900100705     C                   PARM                    KPJBA
058000100705     C*
058100100705     C                   MOVEL     KPJBU         ANA040DS
058200100705     C*
058300100705     C                   if        Ret040     <> '2'
058400100705     C*
058500100705     C                   MoveL     Cod040        WSogg
058600100705     C                   EXSR      CallDvaSog
058700100705     C*
058800100705     C                   if        DVAErrore  <> '1'
058900100705     C*
059000100705     C                   Eval      %subst(DVASOG:1:DVALENOUT)
059100100705     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
059200100705     C*
059300170714     C                   MoveL     DVSPartIVA    D1PartIVA
059400100705     C*
059500100705     C                   endif
059600100705     C*
059700100705     C                   endif
059800100705     C*
059900170526     C                   ENDSR
060000100705      /EJECT
060100100705     C************************************************************
060200100705     C* Ricerca codice fiscale
060300100705     C************************************************************
060400100705     C     F4_CdFisc     BEGSR
060500100705     C*
060600100705     C                   RESET                   ANA040DS
060700100705     C*
060800100705     C                   MOVEL     '01'          OPZ040
060900170529     C                   MoveL     D1CdFisc      CdF040
061000100705     C                   MOVEL     ANA040DS      KPJBU
061100100705     C                   CALL      'ANA040R'
061200100705     C                   PARM                    KPJBA
061300100705     C*
061400100705     C                   MOVEL     KPJBU         ANA040DS
061500100705     C*
061600100705     C                   if        Ret040     <> '2'
061700100705     C*
061800100705     C                   MoveL     Cod040        WSogg
061900100705     C                   EXSR      CallDvaSog
062000100705     C*
062100100705     C                   if        DVAErrore  <> '1'
062200100705     C*
062300100705     C                   Eval      %subst(DVASOG:1:DVALENOUT)
062400100705     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
062500100705     C*
062600170529     C                   MoveL     DVSCdFisc     D1CdFisc
062700100705     C*
062800100705     C                   endif
062900100705     C*
063000100705     C                   endif
063100100705     C*
063200170526     C                   ENDSR
063300100705     C/EJECT
063400100705     C************************************************************
063500100705     C* Ricerca codice soggetto
063600100705     C************************************************************
063700100705     C     F4_Sogg       BEGSR
063800100705     C*
063900100705     C                   RESET                   ANA040DS
064000100705     C*
064100100705     C                   MOVEL     '01'          OPZ040
064200100705     C                   MoveL     D1Sogg        Cod040
064300100705     C                   MOVEL     ANA040DS      KPJBU
064400100705     C                   CALL      'ANA040R'
064500100705     C                   PARM                    KPJBA
064600100705     C*
064700100705     C                   MOVEL     KPJBU         ANA040DS
064800100705     C*
064900100705     C                   if        Ret040     <> '2'
065000100705     C                   MoveL     Cod040        D1Sogg
065100100705     C                   endif
065200100705     C*
065300170526     C                   ENDSR
065400100705     C/EJECT
065500100705     C************************************************************
065600100705     C* Ricerca capoconto
065700100705     C************************************************************
065800100705     C     F4_Kcc        BEGSR
065900100705     C*
066000100705     C                   RESET                   ANA200DS
066100100705     C*
066200100705     C                   MoveL     '01'          OPZ200
066300100705     C*
066400100705     C                   Select
066500100706     C                   When      H1NMFL      = 'D1KCC1'
066600100705     C                   MoveL     D1Kcc1        KCC200
066700100705     C*
066800100706     C                   When      H1NMFL      = 'D1KCC2'
066900100705     C                   MoveL     D1Kcc2        KCC200
067000100705     C*
067100100706     C                   When      H1NMFL      = 'D1KCC3'
067200100705     C                   MoveL     D1Kcc3        KCC200
067300100705     C*
067400100706     C                   When      H1NMFL      = 'D1KCC4'
067500100705     C                   MoveL     D1Kcc4        KCC200
067600100705     C*
067700100706     C                   When      H1NMFL      = 'D1KCC5'
067800100705     C                   MoveL     D1Kcc5        KCC200
067900100705     C*
068000100706     C                   When      H1NMFL      = 'D1KCC6'
068100100705     C                   MoveL     D1Kcc6        KCC200
068200100705     C                   endSl
068300100705     C*
068400100705     C                   MoveL     D1CF          SNA200
068500100705     C*
068600100705     C                   MoveL     ANA200DS      KPJBU
068700100705     C*
068800100705     C                   CALL      'ANA200R'
068900100705     C                   PARM                    KPJBA
069000100705     C*
069100100705     C                   MoveL     KPJBU         ANA200DS
069200100705     C*
069300100705     C                   if        Kcc200     <> *BLANK
069400100705     C*
069500100705     C                   Select
069600100706     C                   When      H1NMFL      = 'D1KCC1'
069700100705     C                   MoveL     Kcc200        D1Kcc1
069800100705     C*
069900100706     C                   When      H1NMFL      = 'D1KCC2'
070000100705     C                   MoveL     Kcc200        D1Kcc2
070100100705     C*
070200100706     C                   When      H1NMFL      = 'D1KCC3'
070300100705     C                   MoveL     Kcc200        D1Kcc3
070400100705     C*
070500100706     C                   When      H1NMFL      = 'D1KCC4'
070600100705     C                   MoveL     Kcc200        D1Kcc4
070700100705     C*
070800100706     C                   When      H1NMFL      = 'D1KCC5'
070900100705     C                   MoveL     Kcc200        D1Kcc5
071000100705     C*
071100100706     C                   When      H1NMFL      = 'D1KCC6'
071200100705     C                   MoveL     Kcc200        D1Kcc6
071300100705     C                   endSl
071400100705     C*
071500100705     C                   endif
071600100705     C*
071700170526     C                   ENDSR
071800100705     C/EJECT
071900100705     C************************************************************
072000100706     C* Ricerca cliente di estrazione
072100100705     C************************************************************
072200100706     C     F4_Cliente    BEGSR
072300100706     C*
072400100706    >C                   RESET                   ANA300DS
072500100706     C*
072600100706    >C                   MOVEL     '01'          OPZ300
072700100706    >C                   MOVEL     D1Ksc         KSC300
072800100706    >C                   MOVEL     ANA300DS      KPJBU
072900100706     C*
073000100706    >C                   CALL      'ANA300R'                            21
073100100706     C                   PARM                    KPJBA
073200100706     C*
073300100706    >C                   MOVEL     KPJBU         ANA300DS
073400100706     C*
0735001007062   >C     KSC300        IFNE      *BLANK
073600100706    >C                   MOVE      KSC300        D1Ksc
0737001007062-  >C                   ENDIF
073800100706     C*
073900170526     C                   ENDSR
074000100706     C/EJECT
074100100706     C************************************************************
074200100706     C* Ricerca fornitore di estrazione
074300100706     C************************************************************
074400100706     C     F4_Fornitore  BEGSR
074500100706     C*
074600100706    >C                   RESET                   ANA400DS
074700100706     C*
074800100706    >C                   MOVEL     '01'          OPZ400
074900100706    >C                   MOVEL     D1Ksc         KSC400
075000100706     C*
075100100706    >C                   MOVEL     ANA400DS      KPJBU
075200100706     C*
075300100706    >C                   CALL      'ANA400R'                            21
075400100706     C                   PARM                    KPJBA
075500100706     C*
075600100706    >C                   MOVEL     KPJBU         ANA400DS
075700100706     C*
0758001007062   >C     KSC400        IFNE      *BLANK
075900100706    >C                   MOVE      KSC400        D1Ksc
0760001007062-  >C                   ENDIF
076100100705     C*
076200170526     C                   ENDSR
076300170529      /EJECT
076400170529     C************************************************************
076500170529     C* GESTIONE EMISSIONE TASTI FUNZIONE
076600100705     C************************************************************
076700100705     C     GESPIE        BEGSR
076800100705     C*
076900100705     C                   SELECT
077000100705     C*
077100100705     C     $GEST         WHENEQ    'D1'
077200100705     C     $ULKD1        IFEQ      0
077300100705     C* riempimento opzioni
077400100705     C                   EXSR      F24D1
077500100705     C                   ELSE
077600100705     C                   WRITE     FMTZ1
077700100705     C                   ENDIF
077800100705     C*
077900100705     C* Modifica valori assunti
078000100705     C     $GEST         WHENEQ    'D3'
078100100705     C     $ULKD3        IFEQ      0
078200100705     C                   EXSR      F24D3
078300100705     C                   ELSE
078400100705     C                   WRITE     FMTZ3
078500170529     C                   ENDIF
078600170704     C*
078700170529     C                   ENDSL
078800100705     C*
078900170526     C                   ENDSR
079000100705     C/EJECT
079100100705     C************************************************************
079200100705     C* GESTIONE VIDEO RECORD D1
079300100705     C************************************************************
079400100705     C     GESD1         BEGSR
079500110422     C*
079600100705     C* inizializzazione videata
079700100705     C     $INZD1        IFEQ      *ON
079800100705     C                   EXSR      INZD1
079900100705     C                   MOVE      *OFF          $INZD1
080000100705     C                   ENDIF
080100100705     C* emissione piede videata se proveniente da altra
080200100705     C* salvataggio valore formato di provenienza
080300100705     C     $LASTV        IFNE      'D1'
080400100705     C                   WRITE     FMTT1
080500100705     C                   EXSR      GESPIE
080600100705     C                   MOVE      $LASTV        $LASTG
080700100705     C                   MOVE      'D1'          $LASTV
080800100705     C                   ENDIF
080900100705     C*
081000100705     C* se esistono errori sulla videata
081100100705     C* emetto la write del formato a indicatori spenti per vedere
081200100705     C* le eventuali decodifiche
081300100705     C     *IN99         IFEQ      *ON
081400100705     C     $GEST         ANDEQ     'D1'
081500100705     C                   MOVEA     *IN           WIN
081600100705     C                   MOVE      *ALL'0'       IN5098           49
081700100705     C                   MOVEA     IN5098        *IN(50)
081800100705     C                   WRITE     FMTD1
081900100705     C                   MOVEA     WIN           *IN
082000100705     C                   ENDIF
082100100705     C*
082200100705     C                   EXFMT     FMTD1
082300100705     C*
082400100705     C* controllo abilitazione tasti funzione
082500100705     C                   EXSR      CTRKD1
082600100705     C*
082700100705     C     *IN99         IFEQ      *OFF
082800100705     C*
082900100705     C                   SELECT
083000100705     C* F2=Decodifica
083100100705     C     $TASTO        WHENEQ    F02
083200100705     C                   EXSR      F02D1
083300100705     C* F3=Fine
083400100705     C     $TASTO        WHENEQ    F03
083500100705     C                   EXSR      F03D1
083600100705      * F4=Ricerca
083700100705     C     $TASTO        WHENEQ    F04
083800100705     C                   EXSR      SEARCH
083900100705     C* F5=Refresh
084000100705     C     $TASTO        WHENEQ    F05
084100100705     C                   EXSR      F05D1
084200100705     C* F12=Ritorno
084300100705     C     $TASTO        WHENEQ    F12
084400100705     C                   EXSR      F12D1
084500110422     C* F14=Regole per estrazione movimenti
084600100705     C     $TASTO        WHENEQ    F14
084700100705     C                   EXSR      F14D1
084800100705     C* F18=Valori assunti
084900100705     C     $TASTO        WHENEQ    F18
085000100705     C                   EXSR      F18D1
085100170529     C* F21=Pers. batch
085200170529     C     $TASTO        WHENEQ    F21
085300170529     C                   EXSR      F21D1
085400170529     C* F24=Altri tasti
085500170529     C     $TASTO        WHENEQ    F24
085600170529     C                   EXSR      F24D1
085700100705     C*
085800100705     C                   OTHER
085900100705     C                   EXSR      CTRD1
086000100705     C*
086100100705     C     $TASTO        IFEQ      F06
086200100705     C     *IN99         ANDEQ     *OFF
086300100820     C*
086400100820     C* Controllo se esiste già il file con il nome reperito
086500100820     C* Se lo trovo,
086600100820     C* segnalo che il file di spedizione per il periodo
086700100820     C* in esame è stato trovato nel catalogo e che pertanto
086800100820     C* potrebbe essere già stato spedito
086900100820     C                   EXSR      CtrFileTel
087000100820     C*
087100100820     C                   if        $OkExeLan   = *On
087200100705     C                   EXSR      EXELAN
087300100705     C                   EXSR      Endpgm
087400100820     C                   endif
087500100820     C*
087600100705     C                   ENDIF
087700100705     C*
087800100705     C                   ENDSL
087900100705     C*
088000100705     C                   ENDIF
088100100705     C*
088200170526     C                   ENDSR
088300100705     C/EJECT
088400100820     C************************************************************
088500100820     C* Verifico esistenza file con stesso nome
088600100820     C************************************************************
088700100820     C     CtrFileTel    BegSr
088800100820     C*
088900100820     C* Reperimento nome file telematico
089000100820     C                   EXSR      RepNomeF
089100100820     C*
089200100820     C*  CHKOBJ     OBJ($LIBRF/&FILE) OBJTYPE(*FILE)
089300100820     C                   Eval      Cmd         =
089400100820     C                             'CHKOBJ OBJ(' +
089500100820     C                             %Trim(Lib022) + '/' +
089600100820     C                             %Trim($File)  +
089700100820     C                             ') ' +
089800100820     C                             'OBJTYPE(*FILE)'
089900100820     C*
090000100820     C                   Call      'QCMDEXC'                            21
090100100820     C                   Parm                    Cmd             120
090200100820     C                   Parm      120           LenCmd           15 5
090300100820     C*
090400100820     C*
090500100820     C                   Eval      $OkExeLan   = *Off
090600100820     C*
090700100820     C* se il file è presente nella libreria lo segnalo
090800100820     C                   If        *In21       = *Off
090900100820     C*
091000100820     C                   EXSR      MsgErr
091100100820     C*
091200100820     C                   Select
091300100820     C*
091400100820     C* richiesto di continuare
091500100820     C                   When      MsgRtn      = '1'
091600100820     C                   Eval      $OkExeLan   = *On
091700100820     C*
091800100820     C*
091900100820     C* richiesto di non continuare
092000100820     C                   When      MsgRtn      = '2'
092100100820     C                   Eval      $OkExeLan   = *Off
092200100820     C*
092300100820     C                   endsl
092400100820     C*
092500100820     C                   else
092600100820     C* se file non presente nella libreria, eseguo lancio
092700100820     C                   Eval      $OkExeLan   = *On
092800100820     C*
092900100820     C                   endIf
093000100820     C*
093100170526     C                   ENDSR
093200100820     C/EJECT
093300100820     C************************************************************
093400100820     C* Reperimento nome file telematico
093500100820     C************************************************************
093600100820     C     RepNomeF      BEGSR
093700100820     C*
093800100820     C                   Eval      WNumero     = *All'0'
093900100820     C                   Z-Add     D1Anno        WAnno
094000100820     C                   Z-Add     0             WPrgInvio
094100100820     C                   Move      WNumero       $Numero
094200100820     C*
094300100820     C                   CALL      'X35NOMFR'
094400100820     C                   PARM                    XScSoc
094500100820     C                   PARM      *Blank        $Unita            8
094600110422     C                   PARM      'E3'          $CdScarico        2
094700100820     C                   PARM                    $Numero           9
094800100820     C                   PARM                    $File            10
094900100820     C                   PARM                    Lib022
095000100820     C                   PARM      *Off          $Errore           1
095100100820     C*
095200100820     C                   if        $Errore    <> '0'
095300100820     C                   Seton                                        9499
095400100820     C                   endif
095500100820     C*
095600170526     C                   ENDSR
095700100820     C/EJECT
095800100820     C************************************************************
095900100820     C* Messaggio errore
096000100820     C************************************************************
096100100820     C     MsgErr        BEGSR
096200100820     C*
096300100820     C                   Reset                   SDGMSGDS
096400100820     C*
096500100820     C* Risposte possibili
096600100820     C* "1) Continuare      2) Annulla
096700100820     C                   Eval      IDMSG       =
096800100820     C                             'PROMSG    *LIBL     COS1204'
096900100820     C                   CALLB     'XRTVM'
097000100820     C                   PARM                    IDMSG
097100100820     C                   PARM      *Blank        TXTMSG
097200100820     C                   PARM                    ERRMSG
097300100820     C     ERRMSG        IFNE      *ON
097400100820     C                   MoveL     TXTMSG        XMSGTXT
097500100820     C                   ELSE
097600100820     C                   MoveL     *ALL'?'       XMSGTXT
097700100820     C                   ENDIf
097800100820     C*
097900100820     C                   MoveL     'S'           MSGEMV
098000100820     C                   Z-Add     10            MSGRGP
098100100820     C                   MoveL     'S'           MSGRSP
098200100820     C                   MoveL     '12'          MSGTPR
098300100820      *
098400100820     C                   Movel     $File         $P1COS2161
098500100820     C                   Move      Lib022        $P1COS2161
098600100820      *
098700100820     C                   call      'SDGMSGR'
098800100820     C                   parm                    SDGMSGDS
098900100820     C                   parm                    XMsgTxt         135
099000100820     C*
099100100820     C* Modo di inizializzazione della window:
099200100820     C* ('1'->Close + Open  -->I° volta dopo l'emissione del video princip.)
099300100820     C* ('0'->rimane aperto -->tutte le emissioni successive se consecutive,
099400100820     C*                       per mantenere il sottofondo del video princip.)
099500100820     C                   parm      '1'           XMsgInz           1
099600100820     C*
099700100820      * COS2161-Il file &1 esiste già nella libreria &2'
099800100820     C                   parm      'COS2161'     XMsgCd1           7
099900100820     C                   parm                    XMsgCd2           7
100000100820     C                   parm                    XMsgCd3           7
100100100820     C                   parm                    XMsgSn1           7
100200100820     C                   parm                    XMsgCdTxT        63
100300100820     C                   parm                    MsgCdTit         14
100400100820     C                   parm                    MsgClPrtf         1
100500100820     C                   parm      $P1COS2161    VarMsg1         100
100600100820     C                   parm                    VarMsg2         100
100700100820     C                   parm                    VarMsg3         100
100800100820     C*
100900170526     C                   ENDSR
101000100820     C/EJECT
101100100705     C************************************************************
101200100705     C* INIZIALIZZAZIONE VIDEATA DATI
101300100705     C************************************************************
101400100705     C     INZD1         BEGSR
101500100705     C*
101600100705     C                   CLEAR                   FMTD1
101700100705     C*
101800100723     C                   Eval      D1On1       = XScOn
101900100723     C                   Eval      D1Off1      = XScOff
102000110720     C                   Eval      D1On2       = XScOn
102100110720     C                   Eval      D1Off2      = XScOff
102200100705     C*
102300100705     C* Se vuole impostazione lancio da ultima elaborazione, li imposto da anmpe
102400100705     C     $Val          IFEQ      *ON
102500100705     C* riempio la videata da parametri memorizzati
102600100705     C                   EXSR      RIED1
102700100705     C*
102800100705     C                   ELSE
102900170529     C                   Eval      D1Anno      = *Year
103000100705     C                   Eval      D1Ctb       = 'CG'
103100100715     C                   Eval      D1TpRegz    = 'D'
103200170811fm3  C                   Eval      D1TpRec     = 'O'
103300110715     C                   Eval      D1DelManu   = XScOff
103400110720     C                   Eval      D1StpAnn    = XScOff
103500100705     C                   endIf
103600100705     C*
103700100705     C* richiamo routine dei ctrl per decodificarla
103800100705     C                   EXSR      CTRD1
103900100705     C                   MOVE      *ALL'0'       IN5098
104000100705     C                   MOVEA     IN5098        *IN(50)
104100100705     C*
104200100705     C                   EXSR      ATRD1
104300100705     C*
104400170526     C                   ENDSR
104500100705     C/EJECT
104600100705     C************************************************************
104700100705     C* RIEMPIMENTO VIDEATA  D1
104800100705     C************************************************************
104900100705     C     RIED1         BEGSR
105000100705     C*
105100100705     C*imposto i campi della videata dai parametri memorizzati su anmpe
105200100705     C*
105300170526     C                   ENDSR
105400100705     C/EJECT
105500100705     C************************************************************
105600100705     C* RIPRISTINA GLI ATTRIBUTI DEL VIDEO
105700100705     C************************************************************
105800100705     C     ATRD1         BEGSR
105900100705     C*
106000100705     C*
106100170526     C                   ENDSR
106200100705     C/EJECT
106300100705      ************************************************************
106400100705     C* GESTIONE F02 VIDEO D1
106500100705     C************************************************************
106600100705     C     F02D1         BEGSR
106700100705     C*
106800100705     C* per decodificare la videata richiamo la routine dei ctrl
106900100705     C* e poi disattivo gli eventuali messaggi di errore
107000100705     C                   EXSR      CTRD1
107100100705     C*
107200100705     C                   MOVE      *ALL'0'       IN5098
107300100705     C                   MOVEA     IN5098        *IN(50)
107400100705     C*
107500170526     C                   ENDSR
107600100705     C/EJECT
107700100705     C************************************************************
107800100705     C* GESTIONE F03 VIDEO D1
107900100705     C************************************************************
108000100705     C     F03D1         BEGSR
108100100705     C*
108200100705     C                   MOVE      *ON           $FINE
108300100705      * fine programma
108400100705     C                   EXSR      ENDPGM
108500100705     C*
108600170526     C                   ENDSR
108700100705     C/EJECT
108800100705     C************************************************************
108900100705     C* GESTIONE F05 VIDEO D1
109000100705     C************************************************************
109100100705     C     F05D1         BEGSR
109200100705     C*
109300100705     C                   MOVE      *ON           $INZD1
109400100705     C*
109500170526     C                   ENDSR
109600100705     C/EJECT
109700100705     C************************************************************
109800100705     C* GESTIONE F12 VIDEO D1
109900100705     C************************************************************
110000100705     C     F12D1         BEGSR
110100100705     C*
110200100705     C                   MOVE      *ON           $FINE
110300100705      * fine programma
110400100705     C                   EXSR      ENDPGM
110500100705     C*
110600170526     C                   ENDSR
110700100705     C/EJECT
110800100705     C************************************************************
110900100705     C* GESTIONE F14 VIDEO D1
111000100705     C************************************************************
111100100705     C     F14D1         BEGSR
111200100705     C*
111300100705     C                   MOVE      *HIVAL        $LASTV
111400100705     C*
111500170526     C                   ENDSR
111600100705     C/EJECT
111700100705     C************************************************************
111800100705     C* GESTIONE F18 VIDEO D1
111900100705     C************************************************************
112000100705     C     F18D1         BEGSR
112100100705     C*
112200100705     C                   MOVE      'D3'          $GEST
112300100705     C                   MOVE      *ON           $INZD3
112400100705     C*
112500170526     C                   ENDSR
112600100705     C/EJECT
112700100705     C************************************************************
112800100705     C* GESTIONE F21 VIDEO D1
112900100705     C************************************************************
113000100705     C     F21D1         BEGSR
113100100705     C*
113200100705     C                   MOVEl     '1'           KRITB
113300170526     C                   MOVE      'SI52'        KCOAZ
113400100705     C                   CALL      'BCH09'
113500100705     C                   PARM                    KPJBA
113600100705     C*
113700170526     C                   ENDSR
113800100705     C/EJECT
113900100705     C************************************************************
114000100705     C* GESTIONE F24 VIDEO D1
114100100705     C************************************************************
114200100705     C     F24D1         BEGSR
114300100705     C*
114400100705     C                   MOVEA     $FX1          $FX
114500100705     C                   MOVEA     $FC1          $FC
114600100705     C                   MOVEA     $FR1          $FR
114700100705     C                   Z-ADD     $ULKD1        $ULK
114800100705     C                   Z-ADD     79            $L01
114900100705     C                   Z-ADD     79            $L02
115000100705     C                   EXSR      RIEKEY
115100100705     C                   Z-ADD     $ULK          $ULKD1            3 0
115200100705     C                   MOVEA     $FC           $FC1
115300100705     C                   MOVEL     $KEY1         Z1KE1
115400100705     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
115500100705     C                   MOVEL     $KEY2         Z1KE2
115600100705     C                   WRITE     FMTZ1
115700100705     C*
115800170526     C                   ENDSR
115900100705     C/EJECT
116000100705     C************************************************************
116100100705     C* GESTIONE F24 VIDEO D3
116200100705     C************************************************************
116300100705     C     F24D3         BEGSR
116400100705     C*
116500100705     C                   MOVEA     $FX3          $FX
116600100705     C                   MOVEA     $FC3          $FC
116700100705     C                   MOVEA     $FR3          $FR
116800100705     C                   Z-ADD     $ULKD3        $ULK
116900100705     C                   Z-ADD     44            $L01
117000100705     C                   Z-ADD     0             $L02
117100100705     C                   EXSR      RIEKEY
117200100705     C                   Z-ADD     $ULK          $ULKD3            3 0
117300100705     C                   MOVEA     $FC           $FC3
117400100705     C                   MOVEL     $KEY1         Z3KE1
117500100705     C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
117600100705     C                   WRITE     FMTZ3
117700100705     C*
117800170526     C                   ENDSR
117900100705     C/EJECT
118000100705     C************************************************************
118100100705     C* CONTROLLO VIDEATA
118200100705     C************************************************************
118300100705     C     CTRD1         BEGSR
118400100705     C*
118500100705     C                   SETOFF                                       99
118600100705     C* non imposto pos. del cursore
118700100705     C                   Z-ADD     0             H1RIGA
118800100705     C                   Z-ADD     0             H1COLO
118900100705     C*
119000100705     C                   EXSR      CtrCtb
119100100715     C*
119200100715     C                   EXSR      CtrTpRegz
119300140327     C*
119400170526     C                   EXSR      CtrTrim
119500100705     C                   EXSR      CtrAnno
119600170529     C*
119700170529     C                   EXSR      CtrStato
119800170717     C*
119900170717     C                   EXSR      CtrTpRec
120000110422     C*
120100110422     C                   EXSR      CtrCF
120200100705     C*
120300100705     C                   EXSR      CtrSogg
120400100705     C*
120500100705     C                   EXSR      CtrKsc
120600100705     C*
120700100705     C                   EXSR      CtrKcc1
120800100705     C*
120900100705     C                   EXSR      CtrKcc2
121000100705     C*
121100100705     C                   EXSR      CtrKcc3
121200100705     C*
121300100705     C                   EXSR      CtrKcc4
121400100705     C*
121500100705     C                   EXSR      CtrKcc5
121600100705     C*
121700100705     C                   EXSR      CtrKcc6
121800110715     C*
121900110715     C                   EXSR      CtrDelManu
122000110720     C*
122100110720     C                   EXSR      CtrStpAnn
122200100705     C*
122300170526     C                   ENDSR
122400100705     C/EJECT
122500100705     C***********************************************************
122600100705     C* Controllo contabilità
122700100705     C***********************************************************
122800100705     C     CtrCtb        BEGSR
122900100705     C*
123000100705     C                   Clear                   D1CtbD
123100100705     C                   RESET                   SDGTABDS
123200100705     C*
123300100705     C                   Eval      XTbRic      = '1'
123400100705     C                   Eval      XTbAzi      = XScSoc
123500100705     C                   Eval      XTbKey      = *Zero
123600100705     C                   Eval      XTbCod      = 'G84'
123700100705     C                   Move      D1Ctb         XTbKey
123800100705     C*
123900100705     C                   CALL      'SDGTABDR'
124000100705     C                   PARM                    SDGTABDS
124100100705     C*
124200100705     C                   if        XTbErr     <> '0'
124300100705     C                   SetOn                                        5199
124400100705     C*
124500100705     C                   else
124600100705     C                   MoveL     XTbUni        ANGG84DS
124700100705     C                   MoveL     DsBG84        D1CtbD
124800100705     C*
124900100705     C                   Eval      XACPrf      = KNmUs
125000100705     C                   Eval      XACSoc      = XScSoc
125100100705     C                   Eval      XACCtb      = D1Ctb
125200100705     C                   CALL      'XAUTCTB'
125300100705     C                   PARM                    XCtbDS
125400100705     C*
125500100705     C                   if        XACAbl      = *Off
125600100705     C                   SetOn                                        5299
125700100705     C                   endif
125800100705     C*
125900100705     C                   endif
126000100715     C*
126100170526     C                   ENDSR
126200100715     C/EJECT
126300100715     C***********************************************************
126400100715     C* Controllo tipo di registrazione
126500100715     C***********************************************************
126600100715     C     CtrTpRegz     BEGSR
126700100715     C*
126800100723     C                   Clear                   D1TpRegzD
126900100715     C                   Clear                   X07ValDs
127000100715     C*
127100100715     C                   Move      '1'           X07Ric
127200100715     C                   Movel     'ANDIZ000'    X07TRc
127300100715     C                   Movel     'DSPTPREGZ '  X07Cam
127400100715     C                   Movel     D1TpRegz      X07VAL
127500100715     C*
127600100715     C                   Callb     'X07VALR'
127700100715     C                   Parm                    X07ValDs
127800100715     C*
127900100715     C     X07ERR        IFEQ      *Off
128000100715     C                   MOVEL     X07DES        D1TpRegzD
128100100715     C                   else
128200100715     C                   SetOn                                        5099
128300100715     C                   endif
128400100705     C*
128500170526     C                   ENDSR
128600100705     C/EJECT
128700100705     C***********************************************************
128800100705     C* Controllo anno di competenza
128900100705     C***********************************************************
129000100705     C     CtrAnno       BEGSR
129100100705     C*
129200100705     C                   if        D1Anno = 0
129300100705     C                   SetOn                                        5399
129400100705     C                   endif
129500100705     C*
129600170526     C                   ENDSR
129700100705     C/EJECT
129800170526     C***********************************************************
129900170526     C* Controllo trimestre
130000170526     C***********************************************************
130100170526     C     CtrTrim       BEGSR
130200140327     C*
130300140327     C                   Clear                   X07ValDs
130400140327     C*
130500140327     C                   Move      '1'           X07Ric
130600140327     C                   Move      *On           X07All
130700140327     C                   Movel     'ANDIZ000'    X07TRc
130800170529     C                   Movel     'SDGTRIM   '  X07Cam
130900170526     C                   Movel     D1Trim        X07VAL
131000140327     C*
131100140327     C                   Callb     'X07VALR'
131200140327     C                   Parm                    X07ValDs
131300140327     C*
131400140327     C     X07ERR        IFEQ      *Off
131500170529     C                   MOVEL     X07DES        D1TrimD
131600140327     C                   else
131700170529     C                   SetOn                                        7899
131800140327     C                   endif
131900140327     C*
132000170526     C                   ENDSR
132100170526     C/EJECT
132200170529     C***********************************************************
132300170529     C* Controllo Stato
132400170529     C***********************************************************
132500170529     C     CtrStato      BEGSR
132600151123     C*
132700170714     C                   Clear                   D1Stato2D
132800151123     C                   Clear                   AngG12DS
132900151123     C*
133000170714     C                   if        D1Stato2   <> *Blank
133100151123     C*
133200151123     C                   RESET                   SDGTABDS
133300151123     C*
133400151123     C                   Eval      XTbRic      = '1'
133500151123     C                   Eval      XTbAzi      = XScSoc
133600151123     C                   Eval      XTbKey      = *Zero
133700151123     C                   Eval      XTbCod      = 'G12'
133800151123     C*
133900170714     C                   Move      D1Stato2      XTbKey
134000151123     C*
134100151123     C                   CALL      'SDGTABDR'
134200151123     C                   PARM                    SDGTABDS
134300160307     C*
134400151123     C                   if        XTbErr     <> '0'
134500151123     C                   SetOn                                        5999
134600151123     C*
134700151123     C                   else
134800151123     C                   MoveL     XTbUni        AngG12DS
134900170714     C                   MoveL     DesG12        D1Stato2D
135000151123     C*
135100151123     C                   endif
135200151123     C*
135300151123     C                   endif
135400151123     C*
135500170529     C                   ENDSR
135600170529     C/EJECT
135700170717     C***********************************************************
135800170717     C* Controllo tipo estrazione dati
135900170717     C***********************************************************
136000170717     C     CtrTpRec      BEGSR
136100170717     C*
136200170717     C                   Clear                   D1TpRecD
136300170717     C*
136400170717     C                   Clear                   X07ValDs
136500170717     C*
136600170717     C                   Move      '1'           X07Ric
136700170717     C                   Move      *On           X07All
136800170717     C                   Movel     'SDGSI52S '   X07TRc
136900170717     C                   Movel     'TPRECI52 '   X07Cam
137000170717     C                   Movel     D1TpRec       X07VAL
137100170717     C*
137200170717     C                   CallB     'X07VALR'
137300170717     C                   Parm                    X07ValDs
137400170717     C*
137500170717     C     X07ERR        IFEQ      *Off
137600170717     C                   MOVEL     X07DES        D1TpRecD
137700170717     C                   else
137800170717     C                   SetOn                                        7999
137900170717     C                   endif
138000170717     C*
138100170717     C                   ENDSR
138200170717     C/EJECT
138300110422     C***********************************************************
138400110422     C* Controllo tipo soggetto
138500110422     C***********************************************************
138600110422     C     CtrCF         BEGSR
138700110422     C*
138800100705     C                   Clear                   D1CFD
138900100705     C*
139000100705     C                   Clear                   X07ValDs
139100100705     C*
139200100705     C                   Move      '1'           X07Ric
139300100705     C                   Move      *On           X07All
139400100705     C                   Movel     'ANDIZ000'    X07TRc
139500100705     C                   Movel     'DSPCLIFOR'   X07Cam
139600100705     C                   Movel     D1CF          X07VAL
139700100705     C*
139800100705     C                   Callb     'X07VALR'
139900100705     C                   Parm                    X07ValDs
140000100705     C*
140100100705     C     X07ERR        IFEQ      *Off
140200100705     C                   MOVEL     X07DES        D1CFD
140300100705     C                   else
140400100705     C                   SetOn                                        5499
140500100705     C                   endif
140600100705     C*
140700170526     C                   ENDSR
140800100705     C/EJECT
140900100705     C***********************************************************
141000100705     C* Controllo codice soggetto
141100100705     C***********************************************************
141200100705     C     CtrSogg       BEGSR
141300100705     C*
141400100705     C                   Clear                   D1SoggD
141500100705     C                   if        D1Sogg     <> *Blank
141600100705     C*
141700100705     C     D1SOGG        IFNE      *BLANK
141800100705     C                   CALLb     'XALLINEA'
141900100705     C                   PARM                    D1SOGG
142000100705     C                   PARM      8             LEN               5 0
142100100705     C                   PARM                    XSCINP
142200100705     C                   ENDIF
142300100705     C*
142400100705     C                   MoveL     D1Sogg        WSogg
142500100705     C                   EXSR      CallDvaSog
142600100705     C*
142700100705     C                   if        DVAErrore  <> '1'
142800100705     C*
142900100705     C                   Eval      %subst(DVASOG:1:DVALENOUT)
143000100705     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
143100100705     C*
143200100705     C                   MoveL     DVSDes        D1SoggD
143300100705     C*
143400100705     C                   else
143500100705     C*
143600100705     C                   SetOn                                        5599
143700100705     C*
143800100705     C                   endif
143900100705     C*
144000100705     C                   endif
144100100705     C*
144200170526     C                   ENDSR
144300100705     C/EJECT
144400100705     C************************************************************
144500100705     C* Chiamata al driver dei soggetti
144600100705     C************************************************************
144700100705     C     CallDvaSog    BEGSR
144800100705     C*
144900100705     C                   EVAL      DVALenOut = %Size(DVASOG)
145000100705     C*
145100100705     C                   CALLB     'NDDVASOG'
145200100705     C                   PARM      '1'           DVARIC
145300100705     C                   PARM      XScSoc        DVASOCIETA
145400100705     C                   PARM                    DVAUNITA
145500100705     C                   PARM      'DVASOG    '  DVASTRUTT
145600100705     C                   PARM                    DVADTRIF
145700100705     C                   PARM                    DVALENOUT
145800100705     C* Se si accede con il codice cli/for impostare i campi seguenti
145900100705     C* se si accede con il soggetto ometterli e impostare gli altri
146000100705     C*                  PARM                    DVASNATURA
146100100705     C*                  PARM                    DVACODICE
146200100705     C*                  PARM                    DVALINEAV
146300100705     C*                  PARM                    DVAFILIALE
146400100705     C                   PARM                    *omit
146500100705     C                   PARM                    *omit
146600100705     C                   PARM                    *omit
146700100705     C                   PARM                    *omit
146800100705     C                   PARM      WSogg         DVSSOGG
146900100705     C                   PARM      *Blank        DVSTPIND
147000100705     C                   PARM      *Blank        DVSCDIND
147100100705     C                   PARM                    DVAERRORE
147200100705     C                   PARM                    DVAOUTPUT
147300100705     C*
147400170526     C                   ENDSR
147500100705     C/EJECT
147600100705     C***********************************************************
147700100705     C* Controllo capoconto 1
147800100705     C***********************************************************
147900100705     C     CtrKcc1       BEGSR
148000100705     C*
148100100705     C                   Clear                   WKcc
148200100705     C                   Clear                   WKsc
148300100705     C*
148400100705     C                   if        D1KCC1     <> *Blank
148500100705     C*
148600100705     C                   CALLB     'XALLINEA'
148700100705     C                   PARM                    D1KCC1
148800100705     C                   PARM      6             LEN               5 0
148900100705     C                   PARM                    XSCINP
149000100705     C*
149100100705     C                   Eval      WKsc        = *Blank
149200100705     C                   Eval      WKcc        = D1Kcc1
149300100705     C                   EXSR      CALLMVC002
149400100705     C                   if        Esito       = *On
149500100706     C                   SetOn                                        6199
149600100706     C                   else
149700100706     C*
149800100706     C                   if        (SNatura002 <> 'C'  and
149900100706     C                              SNatura002 <> 'F'      ) or
150000100706     C                             (SNatura002 <> D1CF and
150100100706     C                              D1CF       <> *Blank   )
150200100706     C                   SetOn                                        7199
150300100706     C                   endif
150400100706     C*
150500100705     C                   endif
150600100705     C                   endif
150700100705     C*
150800170526     C                   ENDSR
150900100705     C/EJECT
151000100705     C***********************************************************
151100100705     C* Controllo capoconto 2
151200100705     C***********************************************************
151300100705     C     CtrKcc2       BEGSR
151400100705     C*
151500100705     C                   Clear                   WKcc
151600100705     C                   Clear                   WKsc
151700100705     C*
151800100705     C                   if        D1KCC2     <> *Blank
151900100705     C*
152000100705     C                   CALLB     'XALLINEA'
152100100705     C                   PARM                    D1KCC2
152200100705     C                   PARM      6             LEN               5 0
152300100705     C                   PARM                    XSCINP
152400100705     C*
152500100705     C                   Eval      WKsc        = *Blank
152600100705     C                   Eval      WKcc        = D1Kcc2
152700100705     C                   EXSR      CALLMVC002
152800100705     C                   if        Esito       = *On
152900100706     C                   SetOn                                        6299
153000100706     C                   else
153100100706     C*
153200100706     C                   if        (SNatura002 <> 'C'  and
153300100706     C                              SNatura002 <> 'F'      ) or
153400100706     C                             (SNatura002 <> D1CF and
153500100706     C                              D1CF       <> *Blank   )
153600100706     C                   SetOn                                        7299
153700100706     C                   endif
153800100706     C*
153900100705     C                   endif
154000100705     C                   endif
154100100705     C*
154200170526     C                   ENDSR
154300100705     C/EJECT
154400100705     C***********************************************************
154500100705     C* Controllo capoconto 3
154600100705     C***********************************************************
154700100705     C     CtrKcc3       BEGSR
154800100705     C*
154900100705     C                   Clear                   WKcc
155000100705     C                   Clear                   WKsc
155100100705     C*
155200100705     C                   if        D1KCC3     <> *Blank
155300100705     C*
155400100705     C                   CALLB     'XALLINEA'
155500100705     C                   PARM                    D1KCC3
155600100705     C                   PARM      6             LEN               5 0
155700100705     C                   PARM                    XSCINP
155800100705     C*
155900100705     C                   Eval      WKsc        = *Blank
156000100705     C                   Eval      WKcc        = D1Kcc3
156100100705     C                   EXSR      CALLMVC002
156200100705     C                   if        Esito       = *On
156300100706     C                   SetOn                                        6399
156400100706     C                   else
156500100706     C*
156600100706     C                   if        (SNatura002 <> 'C'  and
156700100706     C                              SNatura002 <> 'F'      ) or
156800100706     C                             (SNatura002 <> D1CF and
156900100706     C                              D1CF       <> *Blank   )
157000100706     C                   SetOn                                        7399
157100100706     C                   endif
157200100706     C*
157300100705     C                   endif
157400100705     C                   endif
157500100705     C*
157600170526     C                   ENDSR
157700100705     C/EJECT
157800100705     C***********************************************************
157900100705     C* Controllo capoconto 4
158000100705     C***********************************************************
158100100705     C     CtrKcc4       BEGSR
158200100705     C*
158300100705     C                   Clear                   WKcc
158400100705     C                   Clear                   WKsc
158500100705     C*
158600100705     C                   if        D1KCC4     <> *Blank
158700100705     C*
158800100705     C                   CALLB     'XALLINEA'
158900100705     C                   PARM                    D1KCC4
159000100705     C                   PARM      6             LEN               5 0
159100100705     C                   PARM                    XSCINP
159200100705     C*
159300100705     C                   Eval      WKsc        = *Blank
159400100705     C                   Eval      WKcc        = D1Kcc4
159500100705     C                   EXSR      CALLMVC002
159600100705     C                   if        Esito       = *On
159700100706     C                   SetOn                                        6499
159800100706     C                   else
159900100706     C*
160000100706     C                   if        (SNatura002 <> 'C'  and
160100100706     C                              SNatura002 <> 'F'      ) or
160200100706     C                             (SNatura002 <> D1CF and
160300100706     C                              D1CF       <> *Blank   )
160400100706     C                   SetOn                                        7499
160500100706     C                   endif
160600100706     C*
160700100705     C                   endif
160800100705     C                   endif
160900100705     C*
161000170526     C                   ENDSR
161100100705     C/EJECT
161200100705     C***********************************************************
161300100705     C* Controllo capoconto 5
161400100705     C***********************************************************
161500100705     C     CtrKcc5       BEGSR
161600100705     C*
161700100705     C                   Clear                   WKcc
161800100705     C                   Clear                   WKsc
161900100705     C*
162000100705     C                   if        D1KCC5     <> *Blank
162100100705     C*
162200100705     C                   CALLB     'XALLINEA'
162300100705     C                   PARM                    D1KCC5
162400100705     C                   PARM      6             LEN               5 0
162500100705     C                   PARM                    XSCINP
162600100705     C*
162700100705     C                   Eval      WKsc        = *Blank
162800100705     C                   Eval      WKcc        = D1Kcc5
162900100705     C                   EXSR      CALLMVC002
163000100705     C                   if        Esito       = *On
163100100706     C                   SetOn                                        6599
163200100706     C                   else
163300100706     C*
163400100706     C                   if        (SNatura002 <> 'C'  and
163500100706     C                              SNatura002 <> 'F'      ) or
163600100706     C                             (SNatura002 <> D1CF and
163700100706     C                              D1CF       <> *Blank   )
163800100706     C                   SetOn                                        7599
163900100706     C                   endif
164000100706     C*
164100100705     C                   endif
164200100705     C                   endif
164300100705     C*
164400170526     C                   ENDSR
164500100705     C/EJECT
164600100705     C***********************************************************
164700100705     C* Controllo capoconto 6
164800100705     C***********************************************************
164900100705     C     CtrKcc6       BEGSR
165000100705     C*
165100100705     C                   Clear                   WKcc
165200100705     C                   Clear                   WKsc
165300100705     C*
165400100705     C                   if        D1KCC6     <> *Blank
165500100705     C*
165600100705     C                   CALLB     'XALLINEA'
165700100705     C                   PARM                    D1KCC6
165800100705     C                   PARM      6             LEN               5 0
165900100705     C                   PARM                    XSCINP
166000100705     C*
166100100705     C                   Eval      WKsc        = *Blank
166200100705     C                   Eval      WKcc        = D1Kcc6
166300100705     C                   EXSR      CALLMVC002
166400100705     C                   if        Esito       = *On
166500100706     C                   SetOn                                        6699
166600100706     C                   else
166700100706     C*
166800100706     C                   if        (SNatura002 <> 'C'  and
166900100706     C                              SNatura002 <> 'F'      ) or
167000100706     C                             (SNatura002 <> D1CF and
167100100706     C                              D1CF       <> *Blank   )
167200100706     C                   SetOn                                        7699
167300100706     C                   endif
167400100706     C*
167500100705     C                   endif
167600100705     C                   endif
167700100705     C*
167800170526     C                   ENDSR
167900100705     C/EJECT
168000100705     C***********************************************************
168100100705     C* Controllo conto
168200100705     C***********************************************************
168300100705     C     CtrKsc        BEGSR
168400100705     C*
168500100705     C                   Clear                   WKcc
168600100705     C                   Clear                   WKsc
168700100705     C*
168800100705     C                   Clear                   D1KSCD
168900100705     C*
169000100705     C                   if        D1Ksc      <> *Blank
169100100705     C*
169200100705     C     D1KSC         IFNE      *Blanks
169300100705     C                   CALLB     'XALLINEA'
169400100705     C                   PARM                    D1KSC
169500100705     C                   PARM      8             LEN               5 0
169600100705     C                   PARM                    XSCINP
169700100705     C                   ENDIF
169800100720     C*
169900100720     C* obbligatorio indicare la sottonatura per
170000100720     C* controllare il conto singolo
170100100720     C                   if        D1CF        = *Blank
170200100720     C                   SetOn                                        6799
170300100720     C                   else
170400100705     C*
170500100705     C* reperisco capoconto se cliente/fornitore
170600100705     C     D1CF          IFEQ      'C       '
170700100705     C     D1CF          OREQ      'F       '
170800100705     C                   CALLB     'XCAPCLIFOR'
170900100705     C                   PARM                    XScSoc
171000100706     C                   PARM      D1CF          WKcc
171100100705     C                   PARM                    D1Ksc
171200100705     C                   ENDIF
171300100705     C*
171400100705     C*
171500100705     C                   Eval      WKsc        = D1Ksc
171600100705     C                   EXSR      CALLMVC002
171700100705     C                   if        Esito       = *On
171800100705     C                   SetOn                                        5699
171900100705     C                   else
172000100705     C                   MoveL     DesBrev002    D1KSCD
172100100705     C                   endif
172200100705     C*
172300100705     C                   endif
172400100720     C*
172500100720     C                   endif
172600100705     C*
172700170526     C                   ENDSR
172800100705     C/EJECT
172900100705     C***********************************************************
173000100705     C* Driver per controllo conto
173100100705     C***********************************************************
173200100705     C     CallMvc002    BEGSR
173300100705     C*
173400100705     C                   Clear                   ND002DS
173500100705     C*
173600100705     C                   EVAL      LenOut = %Size(ND002DS)
173700100705     C*
173800100705     C                   CallB     'NDMVC002'
173900100705     C                   PARM                    XScSoc
174000100705     C                   PARM                    WKCC
174100100705     C                   PARM                    WKSC
174200100705     C                   PARM                    Data
174300100705     C                   PARM      *OFF          GesMsg
174400100705     C                   PARM      *OFF          Esito
174500100705     C                   PARM      'ND002DS'     StrutturaO
174600100705     C                   PARM                    ND002DS
174700100705     C                   PARM                    LenOut
174800100705     C*
174900170526     C                   ENDSR
175000100705     C/EJECT
175100110715     C***********************************************************
175200110715     C* Controllo cancellazione movimenti manuali
175300110715     C***********************************************************
175400110715     C     CtrDelManu    BEGSR
175500110715     C*
175600110715     C                   if        D1DelManu  <> XScOn   and
175700110715     C                             D1DelManu  <> XScOff
175800110715     C                   SetOn                                        5799
175900110715     C                   endif
176000110715     C*
176100170526     C                   ENDSR
176200110715     C/EJECT
176300110720     C***********************************************************
176400110720     C* Controllo stampa annullamenti
176500110720     C***********************************************************
176600110720     C     CtrStpAnn     BEGSR
176700110720     C*
176800110720     C                   if        D1StpAnn   <> XScOn   and
176900110720     C                             D1StpAnn   <> XScOff
177000110720     C                   SetOn                                        5899
177100110720     C                   endif
177200110720     C*
177300170526     C                   ENDSR
177400110720     C/EJECT
177500100705     C************************************************************
177600100705     C* GESTIONE VALORI ASSUNTI
177700100705     C************************************************************
177800100705     C     GESD3         BEGSR
177900100705     C*
178000100705   B1C     $INZD3        IFEQ      *ON
178100100705     C                   EXSR      INZD3
178200100705     C                   MOVE      *OFF          $INZD3
178300100705   E1C                   ENDIF
178400100705     C*
178500100705   B1C     $LASTV        IFNE      'D3'
178600100705     C                   EXSR      GESPIE
178700100705     C                   MOVE      $LASTV        $LASTG
178800100705     C                   MOVE      'D3'          $LASTV
178900100705   E1C                   ENDIF
179000100705     C*
179100100705     C* se esistono errori sulla videata
179200100705     C* emetto la write del formato a indicatori spenti per vedere
179300100705     C* le eventuali decodifiche
179400100705   B1C     *IN99         IFEQ      *ON
179500100705     C                   MOVEA     *IN           WIN
179600100705     C                   MOVE      *ALL'0'       IN5098           49
179700100705     C                   MOVEA     IN5098        *IN(50)
179800100705     C                   WRITE     FMTD3
179900100705     C                   MOVEA     WIN           *IN
180000100705   E1C                   ENDIF
180100100705     C*
180200100705     C                   EXFMT     FMTD3
180300100705     C*
180400100705     C* controllo abilitazione tasti funzione
180500100705     C                   EXSR      CTRKD3
180600100705     C*
180700100705   B1C     *IN99         IFEQ      *OFF
180800100705     C*
180900100705   B2C                   SELECT
181000100705     C* F3=Fine
181100100705     C     $TASTO        WHENEQ    F03
181200100705     C                   EXSR      F03D1
181300100705     C* F12=Ritorno
181400100705     C     $TASTO        WHENEQ    F12
181500100705     C                   EXSR      F12D3
181600100705     C* F24=Altri tasti
181700100705     C     $TASTO        WHENEQ    F24
181800100705     C                   EXSR      F24D3
181900100705     C*
182000100705     C                   OTHER
182100100705     C                   EXSR      CTRD3
182200100705     C     *IN99         IFEQ      *OFF
182300100705     C                   MOVE      $LASTG        $GEST
182400100705     C*
182500100705     C* definisco i nuovi valori di default
182600100705     C*
182700100705     C                   ENDIF
182800100705     C*
182900100705   E2C                   ENDSL
183000100705     C*
183100100705     C* aggiorno valori assunti
183200100705     C* se richiesto il salvataggio
183300100705     C     D3BSV         IFEQ      1
183400100705     C                   EXSR      AGGMPE
183500100705     C                   ENDIF
183600100705     C*
183700100705   E1C                   ENDIF
183800100705     C*
183900170526     C                   ENDSR
184000100705     C/EJECT
184100100705     C************************************************************
184200100705     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
184300100705     C************************************************************
184400100705     C     INZD3         BEGSR
184500100705     C*
184600100705     C                   CLEAR                   FMTD3
184700100705     C*
184800100705     C                   EVAL      D3BSVA      = Normale
184900100705     C*
185000100705     C*
185100170526     C                   ENDSR
185200100705     C/EJECT
185300100705     C************************************************************
185400100705     C* GESTIONE F12 VIDEO D3
185500100705     C************************************************************
185600100705     C     F12D3         BEGSR
185700100705     C*
185800100705     C                   MOVE      $LASTG        $GEST
185900100705     C*
186000170526     C                   ENDSR
186100100705     C/EJECT
186200100705     C************************************************************
186300100705     C* CONTROLLO VIDEATA  D3
186400100705     C************************************************************
186500100705     C     CTRD3         BEGSR
186600100705     C*
186700100705     C                   SETOFF                                       99
186800100705     C* non imposto pos. del cursore
186900100705     C                   Z-ADD     0             H1RIGA
187000100705     C                   Z-ADD     0             H1COLO
187100100705     C*
187200100705     C* Controllo VALORE
187300100705     C**** D3VAL         IFNE      XSCON
187400100705     C**** D3VAL         ANDNE     XSCOFF
187500100705     C****               SETON                                        5099
187600100705     C****               ENDIF
187700100705     C*
187800100705     C**** D3VAL         IFEQ      XSCON
187900100705     C****               MOVE      *ON           $VAL
188000100705     C****               ELSE
188100100705     C****               MOVE      *OFF          $val
188200100705     C****               endif
188300100705     C*
188400170526     C                   ENDSR
188500100705     C/EJECT
188600100705     C************************************************************
188700100705     C* ESECUZIONE LANCIO
188800100705     C************************************************************
188900100705     C     EXELAN        BEGSR
189000100705     C*
189100170526     C                   reset                   SDGSI52DS
189200100705     C*
189300170526     C                   MoveL     XScSoc        SocietaI52
189400170526     C                   MoveL     D1Ctb         CtbI52
189500170526     C                   MoveL     D1TpRegz      TpRegzI52
189600170526     C                   Z-Add     D1Anno        AnnoI52
189700170526     C                   MoveL     D1Trim        TrimI52
189800170526     C                   Z-Add     0             MeseI52
189900170526     C*
190000170717     C                   MoveL     D1TpRec       TpRecI52
190100170526     C                   MoveL     D1CF          CFI52
190200170714     C                   MoveL     D1PartIVA     PartIVAI52
190300170529     C                   MoveL     D1CdFisc      CdFiscI52
190400170526     C                   MoveL     D1Sogg        SoggI52
190500170526     C                   MoveL     D1Kcc1        Kcc1I52
190600170526     C                   MoveL     D1Kcc2        Kcc2I52
190700170526     C                   MoveL     D1Kcc3        Kcc3I52
190800170526     C                   MoveL     D1Kcc4        Kcc4I52
190900170526     C                   MoveL     D1Kcc5        Kcc5I52
191000170526     C                   MoveL     D1Kcc6        Kcc6I52
191100100707     C                   Clear                   WKcc
191200100707     C                   if        D1Ksc      <> *Blank
191300100707     C                   CALLB     'XCAPCLIFOR'
191400100707     C                   PARM                    XScSoc
191500100707     C                   PARM      D1CF          WKcc
191600100707     C                   PARM                    D1Ksc
191700100707     C                   endif
191800170526     C                   MoveL     WKcc          KccI52
191900170526     C                   MoveL     D1Ksc         KscI52
192000110715     C                   if        D1DelManu   = XScOn
192100170526     C                   Eval      DelManuI52  = *On
192200110715     C                   else
192300170526     C                   Eval      DelManuI52  = *Off
192400110715     C                   endif
192500110720     C                   if        D1StpAnn    = XScOn
192600170526     C                   Eval      StpAnnI52   = *On
192700110720     C                   else
192800170526     C                   Eval      StpAnnI52   = *Off
192900110720     C                   endif
193000100705     C*
193100170529     C* Spesometro Trimestrale
193200170714     C                   Eval      Stato2I52   = D1Stato2
193300170529     C                   Eval      TpComI52    = WTpCom
193400170529     C*
193500170529     C                   movel     SDGSI52DS     Kpjbu
193600100705     C*
193700100719     C                   Move      0             Debug             1 0
193800100719     C                   if        Debug       = 1
193900170526     C                   CALL      'SDGSI52R'
194000100719     C                   PARM                    Kpjba
194100100705     C*
194200100705     C                   ELSE
194300100705     C*
194400170822     C                   MOVE      'SX52'        KCoAz
194500100705     C                   call      'BCH10'
194600100705     C                   parm                    Kpjba
194700100705     C*
194800100705     C                   ENDIF
194900100705     C*
195000100705     C* aggiorno valori assunti
195100100705     C*****              EXSR      AGGMPE
195200100705     C* vado a fine
195300100705     C                   MOVEL     *on           $Fine
195400100705     C*
195500170526     C     XExeLan       ENDSR
195600100705     C/EJECT
195700100705     C************************************************************
195800100705     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
195900100705     C************************************************************
196000100705     C     CTRKD1        BEGSR
196100100705     C*
196200100705     C                   SETOFF                                       99
196300100705     C                   MOVEA     $FC1          $FC
196400100705     C                   EXSR      CTRKEY
196500100705     C     *IN99         IFEQ      *ON
196600100705     C                   SETON                                        97
196700100705     C                   ENDIF
196800100705     C*
196900170526     C                   ENDSR
197000100705     C/EJECT
197100100705     C************************************************************
197200100705     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D3
197300100705     C* INPUT
197400100705     C* $FC3 = Abilitazioni videata D3
197500100705     C* OUTPUT
197600100705     C* *IN97 *IN99 Errori
197700100705     C************************************************************
197800100705     C     CTRKD3        BEGSR
197900100705     C*
198000100705     C                   SETOFF                                       99
198100100705     C                   MOVEA     $FC3          $FC
198200100705     C                   EXSR      CTRKEY
198300100705     C     *IN99         IFEQ      *ON
198400100705     C                   SETON                                        97
198500100705     C                   ENDIF
198600100705     C*
198700170526     C                   ENDSR
198800100705     C/EJECT
198900100705     C************************************************************
199000100705     C* CONTROLLO ABILITAZIONE TASTI
199100100705     C************************************************************
199200100705     C     CTRKEY        BEGSR
199300100705     C*
199400100705     C                   SELECT
199500100705     C     $TASTO        WHENEQ    F01
199600100705     C     $FC(1)        IFEQ      '0'
199700100705     C                   SETON                                        99
199800100705     C                   ENDIF
199900100705     C     $TASTO        WHENEQ    F02
200000100705     C     $FC(2)        IFEQ      '0'
200100100705     C                   SETON                                        99
200200100705     C                   ENDIF
200300100705     C     $TASTO        WHENEQ    F03
200400100705     C     $FC(3)        IFEQ      '0'
200500100705     C                   SETON                                        99
200600100705     C                   ENDIF
200700100705     C     $TASTO        WHENEQ    F04
200800100705     C     $FC(4)        IFEQ      '0'
200900100705     C                   SETON                                        99
201000100705     C                   ENDIF
201100100705     C     $TASTO        WHENEQ    F05
201200100705     C     $FC(5)        IFEQ      '0'
201300100705     C                   SETON                                        99
201400100705     C                   ENDIF
201500100705     C     $TASTO        WHENEQ    F06
201600100705     C     $FC(6)        IFEQ      '0'
201700100705     C                   SETON                                        99
201800100705     C                   ENDIF
201900100705     C     $TASTO        WHENEQ    F07
202000100705     C     $FC(7)        IFEQ      '0'
202100100705     C                   SETON                                        99
202200100705     C                   ENDIF
202300100705     C     $TASTO        WHENEQ    F08
202400100705     C     $FC(8)        IFEQ      '0'
202500100705     C                   SETON                                        99
202600100705     C                   ENDIF
202700100705     C     $TASTO        WHENEQ    F09
202800100705     C     $FC(09)       IFEQ      '0'
202900100705     C                   SETON                                        99
203000100705     C                   ENDIF
203100100705     C     $TASTO        WHENEQ    F10
203200100705     C     $FC(10)       IFEQ      '0'
203300100705     C                   SETON                                        99
203400100705     C                   ENDIF
203500100705     C     $TASTO        WHENEQ    F11
203600100705     C     $FC(11)       IFEQ      '0'
203700100705     C                   SETON                                        99
203800100705     C                   ENDIF
203900100705     C     $TASTO        WHENEQ    F12
204000100705     C     $FC(12)       IFEQ      '0'
204100100705     C                   SETON                                        99
204200100705     C                   ENDIF
204300100705     C     $TASTO        WHENEQ    F13
204400100705     C     $FC(13)       IFEQ      '0'
204500100705     C                   SETON                                        99
204600100705     C                   ENDIF
204700100705     C     $TASTO        WHENEQ    F14
204800100705     C     $FC(14)       IFEQ      '0'
204900100705     C                   SETON                                        99
205000100705     C                   ENDIF
205100100705     C     $TASTO        WHENEQ    F15
205200100705     C     $FC(15)       IFEQ      '0'
205300100705     C                   SETON                                        99
205400100705     C                   ENDIF
205500100705     C     $TASTO        WHENEQ    F16
205600100705     C     $FC(16)       IFEQ      '0'
205700100705     C                   SETON                                        99
205800100705     C                   ENDIF
205900100705     C     $TASTO        WHENEQ    F17
206000100705     C     $FC(17)       IFEQ      '0'
206100100705     C                   SETON                                        99
206200100705     C                   ENDIF
206300100705     C     $TASTO        WHENEQ    F18
206400100705     C     $FC(18)       IFEQ      '0'
206500100705     C                   SETON                                        99
206600100705     C                   ENDIF
206700100705     C     $TASTO        WHENEQ    F19
206800100705     C     $FC(19)       IFEQ      '0'
206900100705     C                   SETON                                        99
207000100705     C                   ENDIF
207100100705     C     $TASTO        WHENEQ    F20
207200100705     C     $FC(20)       IFEQ      '0'
207300100705     C                   SETON                                        99
207400100705     C                   ENDIF
207500100705     C     $TASTO        WHENEQ    F21
207600100705     C     $FC(21)       IFEQ      '0'
207700100705     C                   SETON                                        99
207800100705     C                   ENDIF
207900100705     C     $TASTO        WHENEQ    F22
208000100705     C     $FC(22)       IFEQ      '0'
208100100705     C                   SETON                                        99
208200100705     C                   ENDIF
208300100705     C     $TASTO        WHENEQ    F23
208400100705     C     $FC(23)       IFEQ      '0'
208500100705     C                   SETON                                        99
208600100705     C                   ENDIF
208700100705     C     $TASTO        WHENEQ    F24
208800100705     C     $FC(24)       IFEQ      '0'
208900100705     C                   SETON                                        99
209000100705     C                   ENDIF
209100100705     C                   ENDSL
209200100705     C*
209300170526     C                   ENDSR
209400100705     C/EJECT
209500100705     C************************************************************
209600100705     C* OPERAZIONI INIZIALI
209700100705     C************************************************************
209800100705     C     *INZSR        BEGSR
209900100705     C*
210000100705     C     *ENTRY        PLIST
210100100705     C                   PARM                    KPJBA
210200100705     C*
210300100705     C* Reperimento tasti di funzione
210400100705     C                   EXSR      REPKEY
210500100705     C*
210600100705     C     *DMY          Move      UDate         UDateISO
210700100705     C*
210800170526     C                   ENDSR
210900100705     C/EJECT
211000100705     C************************************************************
211100100705     C* REPERIMENTO TASTI DI FUNZIONE
211200100705     C************************************************************
211300100705     C     REPKEY        BEGSR
211400100705     C*
211500100705     C                   MOVEA     $FM1          $FM
211600100705     C                   MOVEA     $Fc1          $Fc
211700100705     C                   EXSR      REPKKK
211800100705     C                   MOVEA     $FX           $FX1
211900100705     C*
212000100705     C                   MOVEA     $FM3          $FM
212100100705     C                   MOVEA     $Fc3          $FM
212200100705     C                   EXSR      REPKKK
212300100705     C                   MOVEA     $FX           $FX3
212400100705     C*
212500170526     C                   ENDSR
212600100705     C/EJECT
212700100705     C************************************************************
212800100705     C* REPERIMENTO TASTI DI FUNZIONE
212900100705     C************************************************************
213000100705     C     REPKKK        BEGSR
213100100705     C*
213200100705     C                   CLEAR                   $FX
213300100705     C     1             DO        24            X
213400100705     C*
213500100705     C     $FM(X)        IFNE      *BLANK
213600100705     C     $fc(X)        andne     '0'
213700100705     C*
213800100705     C                   MOVEL     $FM(X)        IDMSG
213900100705     C                   CALLB     'XRTVM'
214000100705     C                   PARM                    IDMSG            27
214100100705     C                   PARM                    TXTMSG          100
214200100705     C                   PARM                    ERRMSG            1
214300100705     C*
214400100705     C     ERRMSG        IFNE      *ON
214500100705     C                   MOVEL     TXTMSG        $FX(X)
214600100705     C                   ELSE
214700100705     C                   MOVEL     *ALL' '       $FX(X)
214800100705     C                   ENDIF
214900100705     C*
215000100705     C                   ENDIF
215100100705     C*
215200100705     C                   ENDDO
215300100705     C*
215400170526     C                   ENDSR
215500100705     C/EJECT
215600100705     C************************************************************
215700100705     C* INIZIALIZZAZIONE VARIABILI
215800100705     C************************************************************
215900100705     C     INZVAR        BEGSR
216000100705     C*
216100100705     C* Reperimento dati società
216200100705     C                   MOVEL     'SOC001'      TIPXSC
216300100705     C                   MOVEL     *BLANK        SOCXSC
216400100705     C                   EXSR      REPSOC
216500100705     C     RTNXSC        IFNE      '1'
216600100705     C                   MOVEL     XSOCDS        SOC001
216700100705     C                   ELSE
216800100705     C                   RETURN
216900100705     C                   ENDIF
217000170529     C*
217100170529     C* reperimento tipo comunicazione
217200170529     C                   MOVEL     KPJBU         WTpCom
217300170529     C                   if        WTpCom      = *Blank
217400170529     C                   Eval      WTpCom      = 'T'
217500170529     c                   endif
217600170529     C*
217700170529     C* Reperimento autorizzazioni
217800170529     C                   EXSR      REPAUT
217900170529     C*
218000170529     C* se Spesometro Trimestrale
218100170529     C                   if        WTpCom    = 'T'
218200170529     C* Reperisco i dati di setup del modulo Spesometro Trimestrale
218300170529     C                   EXSR      RepSDSPETRI
218400170529     C                   endif
218500100820     C*
218600100820     C* Reperisco la libreria in cui vengono memorizzati i file
218700100820     C                   EXSR      RepLibFile
218800100705     C*
218900100705     C* Reperimento valori default utente
219000100705     C                   EXSR      REPDFT
219100100705     C*
219200100705     C* Definizione dei tasti funzione utilizzabili a pgm
219300100705     C                   EXSR      DEFOPZ
219400100705     C*
219500100705     C* Variabili per gestione videate
219600100705     C                   MOVE      'D1'          $GEST                          fmt in gestione
219700100705     C                   MOVE      *ON           $INZD1                         inizializzione D1
219800100705     C                   MOVE      *OFF          $FINE
219900100705     C                   MOVE      *BLANK        $LASTG
220000100705     C                   MOVE      *BLANK        $LASTV
220100100705     C                   Z-ADD     0             $ULKD1            3 0
220200100705     C                   Z-ADD     0             $ULKD3            3 0
220300100705     C*
220400100705     C* Variabili appoggio
220500100705     C                   Z-ADD     0             CURR
220600100705     C                   Z-ADD     0             CURC
220700100705     C*
220800100705     C* Valorizzazione campi univoci testate
220900100705     C                   CLEAR                   FMTT1
221000100705     C                   MOVEL     KNSIF         NOMSIF
221100100705     C                   MOVEL     XSCDSI        NOMDIT
221200100705     C                   MOVEL     KCDAZ         NOMAZN
221300100705     C                   MOVEL     DSPGM         NOMPGM           10
221400100705     C* recupero il sottotitolo (se c'è)
221500100706     C                   MOVEL     *BLANK        D1TIT
221600131004     C     SOTTITD1      IFNE      *BLANK
221700131004     C                   MOVEL     SOTTITD1      IDMSG
221800100705     C                   CALLB     'XRTVM'
221900100705     C                   PARM                    IDMSG            27
222000100705     C                   PARM                    TXTMSG          100
222100100705     C                   PARM                    ERRMSG            1
222200100705     C     ERRMSG        IFNE      *ON
222300100706     C                   MOVEL     TXTMSG        D1TIT
222400100705     C                   ELSE
222500100706     C                   MOVEL     *ALL'?'       D1TIT
222600100705     C                   ENDIF
222700131004     C                   ENDIF
222800170529     C*
222900170529     C*
223000170529     C* recupero il titolo con il tipo comunicazione
223100170529     C                   MOVEL     *BLANK        T1TIT
223200170529     C*
223300170529     C*
223400170529     C* Comunicazione Spesometro Trimestrale
223500170704     C                   if        SOTTITT1   <> *BLANK
223600170704     C*
223700170529     C                   if        WTpCom    = 'T'
223800170529     C                   MOVEL     SOTTITT1      IDMSG
223900170529     C                   endif
224000170529     C*
224100170529     C                   CALLB     'XRTVM'
224200170529     C                   PARM                    IDMSG            27
224300170529     C                   PARM                    TXTMSG          100
224400170529     C                   PARM                    ERRMSG            1
224500170529     C                   PARM      *ON           CTRMSG            1            centratura
224600170529     C                   PARM      30            LENMSG            3 0          lun output.
224700170529     C     ERRMSG        IFNE      *ON
224800170529     C                   MOVEL     TXTMSG        T1TIT
224900170529     C                   ELSE
225000170529     C                   MOVEL     *ALL'?'       T1TIT
225100170529     C                   ENDIF
225200170704     C*
225300170704     C                   endif
225400100705     C*
225500100705     C* Inizializzazione/Decodifica prima videata
225600100705     C                   EXSR      INZD1
225700100705     C                   MOVE      *OFF          $INZD1
225800100705     C*
225900170526     C                   ENDSR
226000100705     C/EJECT
226100100705     C************************************************************
226200100705     C* REPERIMENTO DATI SOCIETA'
226300100705     C************************************************************
226400100705     C     REPSOC        BEGSR
226500100705     C*
226600100705     C                   CALL      'SDGSOCDR'
226700100705     C                   PARM                    TIPXSC            6
226800100705     C                   PARM                    SOCXSC            3
226900100705     C                   PARM                    CDSXSC            9 0
227000100705     C                   PARM                    MODXSC            3
227100100705     C                   PARM      *BLANKS       RTNXSC            1
227200100705     C                   PARM                    XSOCDS
227300100705     C                   PARM                    KPJBA
227400100705     C*
227500170526     C                   ENDSR
227600100705     C/EJECT
227700100705     C************************************************************
227800100705     C* REPERIMENTO AUTORIZZAZIONI UTENTE
227900100705     C************************************************************
228000100705     C     REPAUT        BEGSR
228100100705     C*
228200100705     C                   CLEAR                   AUTDS
228300100705     C*
228400100705     C                   MOVEL     XSCSOC        XCASOC
228500100705     C                   MOVEL     XSCDUN        XCAUNI
228600100705     C                   Z-ADD     0             XCAGRP
228700100705     C                   MOVEL     KNMUS         XCAPRF
228800100705     C                   MOVEL     'CONCIV'      XCAFNC
228900100705     C                   MOVEL     *ZERO         XCAVFU
229000100705     C                   MOVE      'CK'          XCATCK
229100100705     C                   MOVE      '1'           XCANAU
229200100705     C*
229300100705     C                   CALLB     'XCHKAUT'
229400100705     C                   PARM                    AUTDS
229500100705     C*
229600100705     C* errori/non abilitazione
229700100705     C     XCARTN        IFNE      0
229800100705     C                   MOVE      *ON           $fine
229900100705     C                   EXSR      ENDPGM
230000100705     C                   ELSE
230100100705     C                   MOVEL     XCADDS        CONCIVDS
230200100705     C* Se non autorizzati alla funzione                                     ed
230300100705     C     ESTALL        IFEQ      *OFF
230400100705     C                   MOVE      XCAFNC        XNOFNC
230500100705     C                   MOVE      XCAPRF        XNOPRF
230600100705     C                   MOVE      XCACUT        XNOCUT
230700100705     C                   MOVE      XCATCK        XNOTCK
230800100705     C                   MOVE      '1'           XNOERR
230900100705     C                   CALL      'XNOAUTR'
231000100705     C                   PARM                    XNOADS
231100100705     C                   MOVE      *ON           $fine
231200100705     C                   EXSR      ENDPGM
231300100705     C                   ENDIF
231400100705     C                   ENDIF
231500100705     C*
231600170526     C                   ENDSR
231700100705     C/EJECT
231800100827     C************************************************************
231900170529     C* Rerimento parametri di setup per Spesometro Trimestrale
232000100827     C************************************************************
232100170529     C     RepSDSPETRI   BEGSR
232200100827     C*
232300170529     C                   Clear                   SDGPA12DS
232400100827     C*
232500100827     C                   CALLB     'XPAR'
232600100827     C                   Parm                    XScSoc
232700170529     C                   Parm      'SDSPETRI'    XPaPar            8
232800100827     C                   Parm                    XPaOut         2000
232900100827     C                   Parm      *OFF          XPaErr            1
233000100827     C                   Parm      ' '           XPaRic            1
233100100827     C*
233200100827     C                   if        XPaErr      = *On
233300100830     C                   EXSR      MsgErr2
233400100827     C                   Eval      $Fine       = *On
233500100827     C                   EXSR      ENDPGM
233600100827     C*
233700100827     C                   else
233800170529     C                   MoveL     XPaOut        SDGPA12DS
233900100827     C*
234000170529     C                   if        AttCSTA12  <> *On
234100100830     C                   EXSR      MsgErr2
234200100827     C                   Eval      $Fine       = *On
234300100827     C                   EXSR      ENDPGM
234400100827     C*
234500100827     C                   endif
234600100827     C*
234700100827     C                   endif
234800100827     C*
234900170526     C                   ENDSR
235000100827     C/EJECT
235100100830     C************************************************************
235200100830     C* Messaggio errore 2
235300100830     C************************************************************
235400100830     C     MsgErr2       BEGSR
235500100830     C*
235600100830     C                   Reset                   SDGMSGDS
235700100830     C*
235800100830     C                   MoveL     'S'           MSGEMV
235900100830     C                   Z-Add     10            MSGRGP
236000100830     C                   MoveL     'N'           MSGRSP
236100100830     C                   MoveL     '  '          MSGTPR
236200100830     C*
236300100830     C                   MoveL     XPaPar        $P1PRO2184
236400100830     C*
236500100830     C                   call      'SDGMSGR'
236600100830     C                   parm                    SDGMSGDS
236700100830     C                   parm                    XMsgTxt         135
236800100830     C*
236900100830     C* Modo di inizializzazione della window:
237000100830     C* ('1'->Close + Open  -->I° volta dopo l'emissione del video princip.)
237100100830     C* ('0'->rimane aperto -->tutte le emissioni successive se consecutive,
237200100830     C*                       per mantenere il sottofondo del video princip.)
237300100830     C                   parm      '1'           XMsgInz           1
237400100830     C*
237500100830     C* PRO2184  - Fallito reperimento recupero parametro modulo &1
237600100830     C                   parm      'PRO2184'     XMsgCd1           7
237700100830     C                   parm                    XMsgCd2           7
237800100830     C                   parm                    XMsgCd3           7
237900100830     C                   parm                    XMsgSn1           7
238000100830     C                   parm                    XMsgCdTxT        63
238100100830     C                   parm                    MsgCdTit         14
238200100830     C                   parm                    MsgClPrtf         1
238300100830     C                   parm      $P1PRO2184    VarMsg1         100
238400100830     C                   parm                    VarMsg2         100
238500100830     C                   parm                    VarMsg3         100
238600100830     C*
238700170526     C                   ENDSR
238800100830     C/EJECT
238900100820     C************************************************************
239000100820     C* Rerimento libreria per scarico su supporto magnetico
239100100820     C************************************************************
239200100820     C     RepLibFile    BEGSR
239300100820     C*
239400100820     C                   Clear                   ANP022DS
239500100820     C*
239600100820     C                   CALLB     'XPAR'
239700100820     C                   Parm                    XScSoc
239800100820     C                   Parm      'SCARSUMA'    XPaPar            8
239900100820     C                   Parm                    XPaOut         2000
240000100820     C                   Parm      *OFF          XPaErr            1
240100100820     C                   Parm      ' '           XPaRic            1
240200100820     C*
240300100820     C                   if        XPaErr      = *On
240400100820     C*
240500100820     C* Errore: fallito reperimento recupero parametro modulo &1
240600100820     C* Valorizzo le variabili del msg PRO2184
240700100830     C                   EXSR      MsgErr2
240800100830     C                   Eval      $Fine       = *On
240900100827     C                   EXSR      ENDPGM
241000100820     C*
241100100820     C                   else
241200100820     C                   MoveL     XPaOut        ANP022DS
241300100820     C                   endif
241400100820     C*
241500170526     C                   ENDSR
241600100820     C/EJECT
241700100705     C************************************************************
241800100705     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
241900100705     C************************************************************
242000100705     C     REPDFT        BEGSR
242100100705     C*
242200100705     C* preparo i parametri di chiamata al driver
242300100705     C                   CLEAR                   XMPEDS
242400100705     C                   EVAL      TpChiamata = '01'
242500100705     C                   EVAL      NChiavi = 5
242600100705     C                   EVAL      VLogica = 2
242700100705     C*
242800100705     C* preparo le chiavi per la chiamata al driver
242900100705     C                   CLEAR                   XMPEDS1
243000100705     C                   MOVEL     XSCSOC        DSMPESOC
243100100705     C                   MOVEL     DSPGM         DSMPEPGM
243200100705     C                   MOVEL     KNMUS         DSMPEPRF
243300100705     C* Si reperiscono i dati di default a seconda delle modalità con
243400100705     C* cui il pgm è usato
243500100705     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
243600100705     C* MPETIP = 'PV'  pgm richiamato da menù
243700100705     C* MPETIP = 'PB'  x lancio Batch
243800100705     C                   MOVEL     'PB'          DSMPETIP
243900100705     C*
244000100705     C                   CALLB     'XMPE'
244100100705     C                   PARM                    XMPEDS
244200100705     C                   PARM                    XMPEDS1
244300100705     C*
244400100705     C     Risultato     IFEQ      '0'
244500100705     C                   MOVEL     DSMPEPAR      DSMPE
244600100705     C* nel caso sia la prima volta che l'utente apre questa anagrafica
244700100705     C* i suoi valori di default vengono decisi a pgm
244800100705     C                   ELSE
244900100705     C**************+    MOVE      *On           $Val
245000100705     C                   EndIf
245100100705     C*
245200170526     C                   ENDSR
245300100705     C/EJECT
245400100705     C************************************************************
245500100705     C* AGGIORNAMENTO VALORI ASSUNTI
245600100705     C************************************************************
245700100705     C     AGGMPE        BEGSR
245800100705     C*
245900100705     C* preparo i parametri di chiamata al driver
246000100705     C                   CLEAR                   XMPEDS
246100100705     C                   EVAL      TpChiamata = '03'
246200100705     C                   EVAL      NChiavi = 5
246300100705     C                   EVAL      VLogica = 2
246400100705     C*
246500100705     C* preparo le chiavi per la chiamata al driver
246600100705     C                   CLEAR                   XMPEDS1
246700100705     C                   MOVEL     XSCSOC        DSMPESOC
246800100705     C                   MOVEL     DSPGM         DSMPEPGM
246900100705     C                   MOVEL     KNMUS         DSMPEPRF
247000100705     C*
247100100705     C* Si memorizzano i dati di default a seconda delle modalità con
247200100705     C* cui il pgm è usato
247300100705     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
247400100705     C* MPETIP = 'PV'  pgm richiamato da menù
247500100705     C* MPETIP = 'PB'  x lancio Batch
247600100705     C*
247700100705     C                   MOVEL     'PB'          DSMPETIP
247800100705     C* dati
247900100705     C*
248000100705     C                   MOVEL     DSMPE         DSMPEPAR
248100100705     C*
248200100705     C                   CALLB     'XMPE'
248300100705     C                   PARM                    XMPEDS
248400100705     C                   PARM                    XMPEDS1
248500100705     C*
248600170526     C                   ENDSR
248700100705     C/EJECT
248800100705     C************************************************************
248900100705     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
249000100705     C************************************************************
249100100705     C     DEFOPZ        BEGSR
249200100705     C*
249300100705     C* Questa routine deve valorizzare la schiera $FC1/$FR1
249400100705     C* con S o N a seconda della utilizzabilità o meno
249500100705     C* del tasto funzione correlato
249600100705     C*
249700100705     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
249800100705     C* di compilazione
249900100705     C                   RESET                   $FC1
250000100705     C                   RESET                   $FR1
250100100705     C                   RESET                   $FC3
250200100705     C                   RESET                   $FR3
250300100705     C*
250400170526     C                   ENDSR
250500100705     C/EJECT
250600100705     C************************************************************
250700100705     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
250800100705     C************************************************************
250900100705     C     DEFCAM        BEGSR
251000100705     C*
251100100705     C* klist
251200100705     C*
251300100705     C* Variabili per gestione videate
251400100705     C*
251500100705     C                   MOVE      *BLANK        $GEST             2
251600100705     C                   MOVE      *BLANK        $FINE             1
251700100705     C                   MOVE      *BLANK        $INZD1            1
251800100705     C                   MOVE      *BLANK        $INZD3            1
251900100705     C                   MOVE      *BLANK        $LASTG            2
252000100705     C                   MOVE      *BLANK        $LASTV            2
252100100705     C*
252200100705     C* Variabili appoggio
252300100705     C*
252400100705     C                   Z-ADD     0             CURR              2 0
252500100705     C                   Z-ADD     0             CURC              2 0
252600100705     C                   MOVE      *ZEROS        WIN              99
252700100705     C*
252800100705     C* Indici
252900100705     C*
253000100705     C                   Z-ADD     0             X                 3 0
253100100705     C                   Z-ADD     0             Y                 3 0
253200100705     C*
253300170526     C                   ENDSR
253400100705     C/EJECT
253500100705     C************************************************************
253600100705     C* RIEMPIMENTO TASTI FUNZIONE
253700100705     C************************************************************
253800100705     C     RIEKEY        BEGSR
253900100705     C*
254000100705     C* Input :
254100100705     C* - $FX = schiera con tutti i testi disponibili
254200100705     C* - $FC = schiere con i flag di validità associati ai testi
254300100705     C* - $FR = schiere con i flag di REVERSE IMAGE
254400100705     C* - $ULK = ultimo testo caricato
254500100705     C* - $L01 = Lunghezza prima riga
254600100705     C* - $L02 = Lunghezza seconda riga
254700100705     C* Output :
254800100705     C* - $ULK
254900100705     C* - $KEY1= 1° riga di tasti funzione
255000100705     C* - $KEY2= 2° riga di tasti funzione
255100100705     C*
255200100705     C                   CALLB     'XKEYMSG'
255300100705     C* input
255400100705     C                   PARM                    $FX
255500100705     C                   PARM                    $FC
255600100705     C                   PARM                    $FR
255700100705     C                   PARM                    $ULK              3 0
255800100705     C* output
255900100705     C                   PARM                    $KEY1            79
256000100705     C                   PARM                    $KEY2            79
256100100705     C* input
256200100705     C                   PARM                    $L01              3 0
256300100705     C                   PARM                    $L02              3 0
256400100705     C*
256500100705     C                   PARM                    $ALLFUNCT       480
256600100705     C*
256700170526     C                   ENDSR
256800100705     C/EJECT
256900100705      *------------------------------------------------------------
257000100705** TASTI DI FUNZIONE  D1
257100100705PROMSG    *LIBL     KEY0001  01
257200100705PROMSG    *LIBL     KEY0002  02
257300100705PROMSG    *LIBL     KEY0003  03
257400100705PROMSG    *LIBL     KEY0004  04
257500100705PROMSG    *LIBL     KEY0005  05
257600100705PROMSG    *LIBL     KEY0006  06
257700100705PROMSG    *LIBL     KEY0007  07
257800100705PROMSG    *LIBL     KEY0008  08
257900100705PROMSG    *LIBL     KEY0009  09
258000100705PROMSG    *LIBL     KEY0010  10
258100100705PROMSG    *LIBL     KEY0011  11
258200100705PROMSG    *LIBL     KEY0012  12
258300100705PROMSG    *LIBL     KEY0013  13
258400110422PROMSG    *LIBL     KEY0014  14
258500110422SDGMSG    *LIBL     KEY0015  15
258600100705PROMSG    *LIBL     KEY0016  16
258700110422SDGMSG    *LIBL     SDK0010  17
258800100705PROMSG    *LIBL     KEY0018  18
258900170704PROMSG    *LIBL     KEY0019  19
259000100705PROMSG    *LIBL     KEY0020  20
259100100705PROMSG    *LIBL     KEY0021  21
259200170704PROMSG    *LIBL     KEY0022  22
259300100705PROMSG    *LIBL     KEY0023  23
259400100705PROMSG    *LIBL     KEY0024  24
259500100705**  ABILITAZIONE D1
2596001007050N  01
2597001007050N  02
2598001007051N  03
2599001007051N  04
2600001007050N  05
2601001007051N  06
2602001007050N  07
2603001007050N  08
2604001007050N  09
2605001007050N  10
2606001104220N  11
2607001007051N  12
2608001007050N  13
2609001104220N  14
2610001104220N  15
2611001007050N  16
2612001705290N  17
2613001007060N  18
2614001707040N  19
2615001007050N  20
2616001007051N  21
2617001707040N  22
2618001007050N  23   Impostare a '1' se si usano le opzioni
2619001007051N  24   Lasciare sempre impostata a '1' il valore della riga 24
262000100705** TASTI DI FUNZIONE  D3
262100100705PROMSG    *LIBL     KEY0001  01
262200100705PROMSG    *LIBL     KEY0002  02
262300100705PROMSG    *LIBL     KEY0003  03
262400100705PROMSG    *LIBL     KEY0004  04
262500100705PROMSG    *LIBL     KEY0005  05
262600100705PROMSG    *LIBL     KEY0006  06
262700100705PROMSG    *LIBL     KEY0007  07
262800100705PROMSG    *LIBL     KEY0008  08
262900100705PROMSG    *LIBL     KEY0009  09
263000100705PROMSG    *LIBL     KEY0010  10
263100100705PROMSG    *LIBL     KEY0011  11
263200100705PROMSG    *LIBL     KEY0012  12
263300100705PROMSG    *LIBL     KEY0013  13
263400100705PROMSG    *LIBL     KEY0014  14
263500100705PROMSG    *LIBL     KEY0015  15
263600100705PROMSG    *LIBL     KEY0016  16
263700100705PROMSG    *LIBL     KEY0017  17
263800100705PROMSG    *LIBL     KEY0018  18
263900100705PROMSG    *LIBL     KEY0019  19
264000100705PROMSG    *LIBL     KEY0020  20
264100100705PROMSG    *LIBL     KEY0021  21
264200100705PROMSG    *LIBL     KEY0022  22
264300100705PROMSG    *LIBL     KEY0023  23
264400100705PROMSG    *LIBL     KEY0024  24
264500100705**  ABILITAZIONE D3
2646001007050N  01
2647001007050N  02
2648001007051N  03
2649001007050N  04
2650001007050N  05
2651001007050N  06
2652001007050N  07
2653001007050N  08
2654001007050N  09
2655001007050N  10
2656001007050N  11
2657001007051N  12
2658001007050N  13
2659001007050N  14
2660001007050N  15
2661001007050N  16
2662001007050N  17
2663001007050N  18
2664001007050N  19
2665001007050N  20
2666001007050N  21
2667001007050N  22
2668001007050N  23   Impostare a '1' se si usano le opzioni
2669001007051N  24   Lasciare sempre impostata a '1' il valore della riga 24
