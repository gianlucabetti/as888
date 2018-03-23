000100001211     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PKXBND) ACTGRP(PROBAS)
000200001211     H*PARMS CVTOPT(*NONE)
000300960222     H DECEDIT('0,') DATEDIT(*DMY.)
000301021111     Hoption(*SRCSTMT : *NODEBUGIO)
000400940309      ********************************************************************
000500940309      *  BAK850R -  ANALISI AZIONI UTENTE                                *
000600940309      ********************************************************************
000700940310      *                                                                  *
000800940310      *------------------------------------------------------------------*
000900940309      * USO DEGI INDICATORI:                                             *
001000940309      *------------------------------------------------------------------*
001100940310      *                                                                  *
001200940309      *--- K850D0                                                        *
001300940311      * 90 - NON PRESENTI AZIONI NEI LIMITI INDICATI                     *
001400940315      * 91 - TIPO AZIONI NON CORRETTO                                    *
001500940310      * 92 - SISTEMA INFORMATIVO INESISTENTE                             *
001600940309      * 93 - ORA NON CORRETTA                                            *
001700940309      * 94 - DATA NON CORRETTA                                           *
001800940322      * 95 - TIPO ANALISI AZIONI NON CORRETTO                            *
001900940309      * 96 - CODA INPUT INESISTENTE                                      *
002000940309      * 97 - TERMINALE INESISTENTE                                       *
002100940310      * 98 - PROFILO NON VALIDO                                          *
002200940309      *                                                                  *
002300940321      *--- K850S1/C1                                                     *
002400940316      * 69 - ROLLUP                                                      *
002500940309      * 70 - SFLCLR / N70 - SFLDSPCTL                                    *
002600940309      * 71 - SFLDSP                                                      *
002700940309      * 72 - SFLEND                                                      *
002800940321      * 75 - SFLNXTCHG                                                   *
002900940318      * 83 - LAVORO NON CONGELATO                                        *
003000940318      * 84 - LAVORO NON CANCELLATO                                       *
003100940318      * 85 - LAVORO NON TROVATO                                          *
003200940318      * 86 - LAVORO NON CONGELATO                                        *
003300940321      * 87 - AZIONE NON RIESEGUITA                                       *
003400940310      *                                                                  *
003500940317      *--- K850D4                                                        *
003600951107      * 77 - ORA NON CORRETTA                                            *
003700951107      * 78 - DATA NON CORRETTA                                           *
003800940321      * 80 - ERRORE NUOVA CODA                                           *
003900951107      * 81 - PRIORITA' NON CORRETTA                                      *
004000940322      * 82 - LAVORO NON MODIFICATO                                       *
004100940317      *                                                                  *
004200940309      *---                                                               *
004300940318      * 40 - READC                                                       *
004400940321      * 49 - WORK                                                        *
004500940321      * 50 - WORK                                                        *
004600940309      * 99 - ERRORE GENERICO                                             *
004700940310      *------------------------------------------------------------------*
004800020905      *------------------------------------------------------------------*
004900940310      *--- File Input
005000991014     FKPLOG04L  IF   E           K DISK
005100991014     F                                     RENAME(KPLOG000:KPLOG004)
005200021104     FKPLOG08L  IF   E           K DISK    RENAME(KPLOG000:KPLOG008)
005201021111     FKPLOG09L  IF   E           K DISK    RENAME(KPLOG000:KPLOG009)
005300940318     FKFSIF01L  IF   E           K DISK
005400020515X0127FKPPRF01L  IF   E           K DISK
005500940318      *--- File Update
005600940323     FKPLOG01L  UF   E           K DISK
005700940323     F                                     RENAME(KPLOG000:KPLOG001)
005800940310      *--- File Display
005900940309     FBAK850D   CF   E             WORKSTN
006000940309     F                                     SFILE(K850S1:S1NRR)
006100940309     F                                     INFDS(INFS1)
006200940309      *------------------------------------------------------------------*
006300940314      *--- Schiera per QCMDEXC
006400951109     D CMD             S             80    DIM(7) CTDATA PERRCD(1)
006500951109     D CDS             S             80    DIM(1) CTDATA PERRCD(1)
006600940314      *--- Schiera Mesi dell'anno
006700940309     D MES             S              2  0 DIM(12) CTDATA PERRCD(12)
006800940314      *--- Schiera WORK
006900940314     D SK1             S              1    DIM(50)
007000960206     D JB              S              1    DIM(60)
007100991020     D WA1             S              1    DIM(4)
007200991020     D WA2             S              1    DIM(4)
007300940309      *------------------------------------------------------------------*
007400020905av1  D DSLOG         E DS                  extname(kplog00f)
007500020905     D KPJBA         E DS
007600940309      *--- INFDS del Video
007700940309     D INFS1           DS           512
007800930708     D  BINRIG               370    370
007900930708     D  BINCOL               371    371
008000930708     D  SFLNRR               378    379B 0
008100940309      *--- DS Data
008200940309     D                 DS
008300940309     D  DATA                   1      6  0
008400940309     D  GG                     1      2  0
008500940309     D  MM                     3      4  0
008600940309     D  AA                     5      6  0
008700991014      *--- DS Data
008800991014     D                 DS
008900991018     D  DATAV                  1      7  0
009000991018     D  SSV                    1      1  0
009100991018     D  AAV                    2      3  0
009200991018     D  MMV                    4      5  0
009300991018     D  GGV                    6      7  0
009500951107     D DATORA          DS
009600951107     D  DATS                   1      6  0
009700951114     D  DATAA                  1      6
009800951114     D  ORAA                   7     12
009900951114     D  ORAS                   7     12  0
010000951106     D  GG4                    1      2  0
010100951106     D  MM4                    3      4  0
010200951106     D  AA4                    5      6  0
010300951107     D  ORA4                   7      8  0
010400951107     D  MIN4                   9     10  0
010500951107     D  SEC4                  11     12  0
010600960329     D*
010700941024     D SCHEDS          DS
010800941024     D  DSCHE                  1      6
010900941024     D  OSCHE                  7     10
011000940314      *--- DS per CHKOBJ
011100940309     D                 DS
011200951109     D  CMD1                   1     80
011300940309     D  NOME                  12     21
011400940309     D  TIPO                  32     38
011500940318      *--- DS per Comandi JOB
011600951114     D* PARAMETRI DI I/O di BAK036R
011700951114     D DS036           DS
011800951114     D  JNAM36                 1     10
011900951114     D  UNAM36                11     20
012000951114     D  JNBR36                21     26
012100951114     D  JOBQ36                27     36
012200951124     D  JOBP36                37     37
012300970317     D  JSTS36                38     47
012400940314     D                 DS
012500951109     D  CMD2                   1     80
012600940321     D  NUMJOB                12     17
012700940321     D  NOMLAV                12     21
012800940315     D  PRUNOJ                19     39
012900960222     D  PTYBLK                41     50
013000940323     D  NEWPR                 49     49
013100940321     D  NEWJQ                 58     67
013200951109      *--- DS per Comandi JOB
013300951109     D                 DS
013400951109     D  CMD3                   1     80
013500951109     D  NMLAV                 12     21
013600951109     D  NEWDS                 50     55
013700951109     D  NEWOS                 66     71
013800940310      *--- DS per KPJBA Azione
013900940317     D JBADS           DS           502
014000940317     D  NPU                    1     10
014100940317     D  NTM                   20     29
014200940317     D  NRL                   47     52  0
014300940315     D  STJ                   53     53
014400940315     D  CDA                   60     63
014500940316     D  TIA                   64     64
014600940310     D  NSI                   92    101
014700940315     D  PTY                  155    155
014800941024     D  JBUF                 209    246
014900941024     D  JBU1                 247    306
015000940317     D  JBU2                 307    366
015100940317     D  JBU3                 367    426
015200940317     D  JBU4                 427    486
015300940317     D  JBU5                 487    502
015400990910X0026D*-------------
015500990908  "  D* Dati di ambiente ottenuti da XSOC
015600990908  "  D SOC001        E DS                  EXTNAME(XSOC001DS)
015700990908  "  D  SALDOC       E                     EXTFLD(XSCSALDOC)
015800990908  "  D  SALDOA       E                     EXTFLD(XSCSALDOA)
015900990908  "  D*-------------
016000990908  "  D* DS Interna per dati di output di XSOC
016100990908  "  D XSOCDS          DS          1000
016200990908  "  D*-------------
016300990908  "  D* REPERISCO NOME PGM
016400990908  "  D STATUS         SDS           333
016500990910X0026D  DSPGM            *PROC
016600001211X0099D*-------------
016700001211  "  D* Ricerca azioni
016800001211  "  D BAK986DS      E DS                  INZ
016900001211X0099D*-------------
017000001031X0097 * S.I.
017100001031  "  D K988DS        E DS                  EXTNAME(BAK988DS)
017200001031  "  D  NSI988       E                     EXTFLD(KNMSI988)
017300001031  "  D  DSI988       E                     EXTFLD(KDSSI988)
017400001031  "   * PROFILI
017500001031  "  D K980DS        E DS                  EXTNAME(BAK980DS)
017600001031  "  D  PRF980       E                     EXTFLD(PROFILO980)
017700001031  "  D  DES980       E                     EXTFLD(DESCR980)
017800001031  "  D  MNI980       E                     EXTFLD(MENUINZ980)
017900001031  "  D  SIF980       E                     EXTFLD(SISINF980)
018000020515  "  D*-------------
018100001031  "  D CURSOR          DS
018200001031  "  D  RIRI                   1      2B 0 INZ
018300001031  "  D  R$$                    2      2
018400001031  "  D  COCO                   3      4B 0 INZ
018500001031X0097D  C$$                    4      4
018600020515X0127D*-------------
018700020515  "  D* Scomposizione campo PRFUSF
018800020515  "  D PRFUSF          DS
018900020515  "  D  PRFPgA                 1     10A
019000020515  "  D  PRFLbA                11     20A
019100020515  "  D  PRFAAU                21     21A
019200020515X0127D*-------------
019300940314      *-- Costanti
019400960201     D PAGINA          C                   CONST(12)
019500940314     D DAORC           C                   CONST('-Immiss.-')
019600940314     D COPRC           C                   CONST('- Input -')
019700940314     D DAORA           C                   CONST(' -Inizio-')
019800940314     D COPRA           C                   CONST('- Esec. -')
019900940314     D DAORE           C                   CONST(' - Fine -')
020000940314     D COPRE           C                   CONST('- Esec. -')
020100940322      *
020200020516X0127D*-------------
020300020516X0127D $PgmAziUte      S              1A
020400001211X0099D File            S             10A
020500001211  "  D SisInf          S             10A
020600001211  "  D Lingua          S              1A
020700001211  "  D Scope           S              1A
020800001211  "  D Errore          S              1A
020900001211  "  D LibL            S            240A
021000001211  "  D $OvrDB          S              1A
021100001211X0099D*-------------
021200021025av2  D WPrUt           S             10A
021201021111av2  D ZPrUt           S             10A
021300021104      *=============================================================================================
021400021104av3  D KQU             S                   like(d0cinp)
021500021104av3  D KUS             S                   like(d0prut)
021600021104av3  D $wildUser       S               N
021700021104av3  D $wildQueue      S               N
021800021104av3  D $wildTerm       S               N
021900021104     D d0term_low      S                   like(d0term)
022000021104     D d0term_hig      S                   like(d0term)
022100021104     D d0cinp_low      S                   like(d0cinp)
022200021104     D d0cinp_hig      S                   like(d0cinp)
022300021104      *=============================================================================================
022400940309      *----------------------------------------------------*
022500940309      *   FLUSSO PRINCIPALE                                *
022600940309      *----------------------------------------------------*
022700940322      *
022800960201     C                   SETON                                        09
022900940309     C                   EXSR      DEFCAM
023000940309     C     *INKC         DOWEQ     '0'
023100990910X0026C     *INKL         ANDEQ     '0'
023200940311      * Emissione video Guida
023300001031X0097C     EMTD0         TAG
023400001031  "  C     *IN99         IFEQ      *ON
023500001031  "  C                   SETOFF                                       99
023600001031  "  C                   WRITE     K850D0
023700001031  "  C                   SETON                                        99
023800001031X0097C                   ENDIF
023900940309     C                   EXFMT     K850D0
024000940309     C     *INKC         IFEQ      '0'
024100990910X0026C     *INKL         ANDEQ     '0'
024200001031X0097 * Ricerca
024300001031  "  C     *INKD         IFEQ      *ON
024400001031  "  C                   EXSR      SEARCH
024500001031  "  C                   GOTO      EMTD0
024600001031X0097C                   ENDIF
024700940311      * Controllo video Guida
024800951124     C                   EXSR      CTRD0
024900940309     C     *IN99         IFEQ      '0'
025000951108     C     SUBF          TAG
025100940310      * Gestione Subfile Azioni
025200940309     C                   EXSR      GESS1
025300940309     C                   ENDIF
025400940309     C                   ENDIF
025500940309     C                   ENDDO
025600940309      * Fine Elaborazione
025700930708     C                   SETON                                        LR
025800940309      *
025900940309      *----------------------------------------------------*
026000940311      * CTRD0 - CONTROLLO VIDEO GUIDA                      *
026100940309      *----------------------------------------------------*
026200940309      *
026300940309     C     CTRD0         BEGSR
026400940309     C                   SETOFF                                       999897
026500940322     C                   SETOFF                                       969594
026600940322     C                   SETOFF                                       939291
026700940322     C                   SETOFF                                       90
026800940322      * Controllo Tipo Analisi Azioni
026900940322     C     D0TIAN        IFNE      'C'
027000940322     C     D0TIAN        ANDNE     'A'
027100940322     C     D0TIAN        ANDNE     'E'
027200940322     C                   SETON                                        9599
027300940322     C                   ENDIF
027400940315      * Controllo Tipo Azioni
027500940315     C     D0TIAZ        IFNE      ' '
027600940315     C     D0TIAZ        ANDNE     'B'
027700940315     C     D0TIAZ        ANDNE     'I'
027800940315     C                   SETON                                        9199
027900940315     C                   ENDIF
028000021025     C     D0PRUT        IFNE      *BLANK
028100021025      * Controllo esistenza Profilo Utente
028200021025av2  C                   If        %SubSt(D0PrUt:
028300021025av2  C                              %Len(%TrimR(D0PrUt)):1) <> '*'
028400021025     C                   MOVE      CMD(1)        CMD1
028500021025     C                   MOVEL     D0PRUT        NOME
028600021025     C                   MOVEL     '*USRPRF'     TIPO
028700021025     C                   CALL      'QCMDEXC'                            98
028800021025     C                   PARM                    CMD1
028900021025     C                   PARM      80            LUNG
029000021025     C   98              SETON                                        99
029100021025av2  C                   EndIf
029200021025     C                   ENDIF
029300940309     C     D0SINF        IFNE      *BLANK
029400940309      * Controllo esistenza Sistema Informativo
029500940309     C     D0SINF        CHAIN     KFSIF01L                           92
029600940309     C   92              SETON                                        99
029700940309     C                   ENDIF
029800021104      *
029900021104      *
030000940309     C     D0TERM        IFNE      *BLANK
030100021104av2  C                   If        %SubSt(D0Term:
030200021104av2  C                              %Len(%TrimR(D0Term)):1) <> '*'
030300940309      * Controllo esistenza Terminale
030400940314     C                   MOVE      CMD(1)        CMD1
030500940309     C                   MOVEL     D0TERM        NOME
030600940309     C                   MOVEL     '*DEVD  '     TIPO
030700940309     C                   CALL      'QCMDEXC'                            97
030800940314     C                   PARM                    CMD1
030900021002     C                   PARM      80            LUNG             15 5
031000940309     C   97              SETON                                        99
031100021104     C                   endif
031200940309     C                   ENDIF
031300021104      *
031400021104      *
031500940309     C     D0CINP        IFNE      *BLANK
031600021104av2  C                   If        %SubSt(D0Cinp:
031700021104av2  C                              %Len(%TrimR(D0Cinp)):1) <> '*'
031800940309      * Controllo esistenza Coda Input
031900940314     C                   MOVE      CMD(1)        CMD1
032000940309     C                   MOVEL     D0CINP        NOME
032100940309     C                   MOVEL     '*JOBQ  '     TIPO
032200940309     C                   CALL      'QCMDEXC'                            96
032300940314     C                   PARM                    CMD1
032400951109     C                   PARM      80            LUNG
032500940309     C   96              SETON                                        99
032600021104     C                   endif
032700940309     C                   ENDIF
032800940309      * Controllo Data
032900951124     C     D0TIAN        IFNE      'C'
033000951124     C     D0MM          ORNE      0
033100951124     C     D0AA          ORNE      0
033200951124     C     D0GG          ORNE      0
033300951124     C*
033400940309     C     D0MM          IFGT      12
033500940310     C     D0MM          ORLT      1
033600940309     C                   SETON                                        9499
033700940309     C                   ELSE
033800940309     C     D0MM          IFEQ      2
033900940309     C     D0AA          DIV       4             RIS               2 0
034000940309     C                   MVR                     RES               1 0
034100940309     C     RES           IFEQ      0
034200940309     C                   Z-ADD     29            MES(2)
034300941202     C                   ELSE
034400941202     C                   Z-ADD     28            MES(2)
034500940309     C                   ENDIF
034600940309     C                   ENDIF
034700940309     C     D0GG          IFGT      MES(D0MM)
034800940310     C     D0GG          ORLT      1
034900940309     C                   SETON                                        9499
035000940309     C                   ENDIF
035100940309     C                   ENDIF
035200951124     C                   ENDIF
035300940309      * Controllo Ora
035400940309     C     D0ORA         IFGT      23
035500940309     C     D0MIN         ORGT      59
035600940309     C     D0SEC         ORGT      59
035700940309     C                   SETON                                        9399
035800940309     C                   ENDIF
035900930708     C                   ENDSR
036000940309      *
036100940309      *----------------------------------------------------*
036200940310      * GESS1 - GESTIONE SFL AZIONI                        *
036300940309      *----------------------------------------------------*
036400940309      *
036500940309     C     GESS1         BEGSR
036600021104      *
036700021104      *:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
036800021104     C                   eval      $wildUser =%scan('*':d0prut)>0
036802021115 "   C*****              eval      ZPrUt = %SubSt(D0PrUt:1:
036803021115 "   C*****                        %Len(%TrimR(D0PrUt))-1)
036900021104     C                   eval      $wildQueue=%scan('*':d0cinp)>0
037000021104     C                   eval      $wildTerm =%scan('*':d0term)>0
037100021104      *:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
037200021104      *
037300951108     C     *INKE         DOUEQ     '0'
037400940310      * Inizializzazione Subfile
037500940310     C                   EXSR      INZS1
037600991014      * Ora parzializzazione
037700991014     C                   Z-ADD     D0ORA         GG
037800991014     C                   Z-ADD     D0MIN         MM
037900991014     C                   Z-ADD     D0SEC         AA
038000991014     C                   Z-ADD     DATA          ORA               6 0
038100940311      * Inversione Data parzializzazione
038200940311     C                   Z-ADD     D0GG          AA
038300940311     C                   Z-ADD     D0MM          MM
038400940311     C                   Z-ADD     D0AA          GG
038500021104      *=====================================================
038600021104      */////////////////////////////////////////////////////
038700021104      *=====================================================
038800940315      * Posizionamento e prima Lettura per Tipo Analisi Azioni
038900940311     C                   SELECT
039000021104      *=====================================================
039100940322     C     D0TIAN        WHENEQ    'C'
039200021104av3  C                   eval      KUS=d0prut
039300021104av3  C                   eval      KQU=d0cinp
039400940311     C                   Z-ADD     0             KDF
039500991014     C                   Z-ADD     0             KOF
039600940311     C                   Z-ADD     0             KDI
039700991014     C                   Z-ADD     0             KOI
039800021104     C                   if        d0prut<>*blank and not $wildUser
039900021111     C                   if        d0cinp<>*blank and not $wildQueue
040000021111     C     KLOG9         SETLL     KPLOG09L
040100021111     C     KLOG9         READE     KPLOG09L                               72
040200021111     C                   else
040300021104     C     KLOG8         SETLL     KPLOG08L
040400021104     C     KLOG8         READE     KPLOG08L                               72
040500021111     C                   endif
040600021104     C                   else
040700991014     C     KLOG2         SETLL     KPLOG04L
040800991014     C     KLOG2         READE     KPLOG04L                               72
040900021104     C                   endif
041000021104av1  C                   EXSR      TESTCK
041100021104      *=====================================================
041200020906     C     D0TIAN        WHENEQ    'A'
041300021104av3  C                   eval      KUS=d0prut
041400021104av3  C                   eval      KQU=d0cinp
041500940311     C                   Z-ADD     0             KDF
041600991014     C                   Z-ADD     0             KOF
041700940311     C                   Z-ADD     DATA          KDI
041800991018     C     D0AA          IFLT      40
041900991018     C                   MOVEL     1             KDI
042000991018     C                   ENDIF
042100991014     C                   Z-ADD     ORA           KOI
042200021104     C                   if        d0prut<>*blank and not $wildUser
042300021111     C                   if        d0cinp<>*blank and not $wildQueue
042400021111     C     KLOG9         SETLL     KPLOG09L
042500021111     C     KLOG9A        READE     KPLOG09L                               72
042600021111     C                   else
042700021104     C     KLOG8         SETLL     KPLOG08L
042800021104     C     KLOG8A        READE     KPLOG08L                               72
042900021111     C                   endif
043000021104     C                   else
043100991014     C     KLOG2         SETLL     KPLOG04L
043200991014     C     KDF           READE     KPLOG04L                               72
043300021104     C                   endif
043400020906av1  C                   EXSR      TESTCK
043500021104      *=====================================================
043600940322     C     D0TIAN        WHENEQ    'E'
043700021104av3  C                   eval      KUS=d0prut
043800021104av3  C                   eval      KQU=d0cinp
043900940311     C                   Z-ADD     DATA          KDF
044000991018     C     D0AA          IFLT      40
044100991018     C                   MOVEL     1             KDF
044200991018     C                   ENDIF
044300991014     C                   Z-ADD     ORA           KOF
044400991014     C                   Z-ADD     0             KDI
044500021104     C                   Z-ADD     0             KOI
044600021104     C                   if        d0prut<>*blank and not $wildUser
044700021111     C                   if        d0cinp<>*blank and not $wildQueue
044800021111     C     KLOG9         SETLL     KPLOG09L
044900021111     C     klog9b        READe     KPLOG09L                               72
045000021111     C                   else
045100021104     C     KLOG8         SETLL     KPLOG08L
045200021104     C     klog8b        READe     KPLOG08L                               72
045300021111     C                   endif
045400021104     C                   else
045500021104     C     KLOG2         SETLL     KPLOG04L
045600021104     C                   READ      KPLOG04L                               72
045700021104     C                   endif
045800020906av1  C                   EXSR      TESTCK
045900940311     C                   ENDSL
046000021104      *=====================================================
046100021104      */////////////////////////////////////////////////////
046200021104      *=====================================================
046300940311      * Caricamento 1a pagina Subfile
046400940311     C                   EXSR      ROLLS1
046500940311     C     S1NRR         COMP      0                                      90
046600940311     C     *IN90         IFEQ      '0'
046700940314     C                   Z-ADD     S1NRR         C1NRR
046800940311      * Caricamento 2a pagina Subfile
046900940311     C                   EXSR      ROLLS1
047000940311     C     *INKC         DOWEQ     '0'
047100940311     C     *INKE         ANDEQ     '0'
047200940315     C     *INKL         ANDEQ     '0'
047300940315      * Emissione Subfile Azioni
047400940315     C                   EXSR      EMTS1
047500940311     C     *INKC         IFEQ      '0'
047600940311     C     *INKE         ANDEQ     '0'
047700940315     C     *INKL         ANDEQ     '0'
047800940311     C     *IN69         IFEQ      '1'
047900940314      * Caricamento altre 2 pagine
048000940314     C                   EXSR      ROLLS1
048100940314     C                   Z-ADD     S1NRR         C1NRR
048200940311     C                   EXSR      ROLLS1
048300940311     C                   ELSE
048400940314      * Controllo opzioni inserite
048500940314     C                   EXSR      CTRS1
048600940311     C                   ENDIF
048700940311     C                   ENDIF
048800940311     C                   ENDDO
048900990920X0026C                   MOVE      '0'           *INKL
049000940311     C                   ENDIF
049100940311     C                   ENDDO
049200940309     C                   ENDSR
049300940310      *
049400940310      *----------------------------------------------------*
049500940310      * INZS1 - INIZIALIZZAZIONE SFL AZIONI                *
049600940310      *----------------------------------------------------*
049700940310      *
049800940310     C     INZS1         BEGSR
049900940322     C                   SETOFF                                       KE
050000940310     C                   Z-ADD     0             S1NRR             4 0
050100940311     C                   Z-ADD     0             NMAX              4 0
050200940311     C                   Z-ADD     0             C1NRR             4 0
050300940310     C                   SETON                                          70
050400940310     C                   SETOFF                                         71
050500940310     C                   WRITE     K850C1
050600940310     C                   SETOFF                                         70
050700020516X0127C* se pgm richiamato in modalità "analisi azioni utente" e
050800020516  "  C*  l'utente ha possibilità limitate --> visualizzo solo opz.5
050900021001  "  C                   If        $PgmAziUte = *On and  PRFAAU = '1'
051000020515  "  C                   Eval      *In08 = *On
051100020515  "  C                   Else
051200020515  "  C                   Eval      *In08 = *Off
051300020515X0127C                   EndIf
051400021001      * ---------------------------------------------
051500021001      * exit x controllo se l' utente è abilitato
051600021001      * ---------------------------------------------
051700021001     C     *in08         ifeq      '0'
051800021001av1  C                   call      'BAK850U3'                           99
051801021111  "  C                   PARM                    KPJBA
051900021001av1  C                   parm                    okko
052000021111     C     okko          ifeq      'KO'
052100021001  "  C                   move      '1'           *in08
052200021001X0127C                   EndIf
052300021001X0127C                   EndIf
052400021001      *
052500021001     C                   ENDSR
052600940310      *----------------------------------------------------*
052700940311      * ROLLS1 - CARICAMENTO SFL AZIONI (PAGINA)           *
052800940310      *----------------------------------------------------*
052900940310      *
053000940311     C     ROLLS1        BEGSR
053100940322     C                   SETOFF                                       75
053200940322     C                   MOVE      *BLANK        S1OPZ
053300940311     C                   Z-ADD     0             NP                2 0
053400940311     C                   Z-ADD     NMAX          S1NRR
053500940310     C     *IN72         DOWEQ     '0'
053600940317     C     NP            ANDLT     PAGINA
053700940311      * Controlla altre Parzializzazioni
053800940311     C                   EXSR      PARZIA
053900940311     C     *IN50         IFEQ      '0'
054000940310      * Valorizza campi Subfile
054100940311     C                   EXSR      VALS1
054200940310     C                   ADD       1             S1NRR
054300940311     C                   ADD       1             NP
054301021111      *
054302021111     C                   EXSR      KMSG
054303021111      *
054400940310     C                   WRITE     K850S1
054500940311     C                   ENDIF
054600940315      * Lettura per Tipo Analisi Azioni
054700940310     C                   SELECT
054800940322     C     D0TIAN        WHENEQ    'C'
054900021104     C                   if        d0prut<>*blank and not $wildUser
055000021111     C                   if        d0cinp<>*blank and not $wildQueue
055100021111     C     KLOG9         READE     KPLOG09L                               72
055200021111     C                   else
055300021104     C     KLOG8         READE     KPLOG08L                               72
055400021111     C                   endif
055500021104     C                   else
055600991014     C     KLOG2         READE     KPLOG04L                               72
055700021104     C                   endif
055800020906av1  C                   EXSR      TESTCK
055900940322     C     D0TIAN        WHENEQ    'A'
056000021104     C                   if        d0prut<>*blank and not $wildUser
056100021111     C                   if        d0cinp<>*blank and not $wildQueue
056200021111     C     KLOG9A        READE     KPLOG09L                               72
056300021111     C                   else
056400021104     C     KLOG8A        READE     KPLOG08L                               72
056500021111     C                   endif
056600021104     C                   else
056700991014     C     KDF           READE     KPLOG04L                               72
056800021104     C                   endif
056900020906av1  C                   EXSR      TESTCK
057000940322     C     D0TIAN        WHENEQ    'E'
057100021104     C                   if        d0prut<>*blank and not $wildUser
057200021111     C                   if        d0cinp<>*blank and not $wildQueue
057300021111     C     klog9b        READe     KPLOG09L                               72
057400021111     C                   else
057500021104     C     klog8b        READe     KPLOG08L                               72
057600021111     C                   endif
057700021104     C                   else
057800991014     C                   READ      KPLOG04L                               72
057900021104     C                   endif
058000020906av1  C                   EXSR      TESTCK
058100940310     C                   ENDSL
058200940310     C                   ENDDO
058300940311     C                   Z-ADD     S1NRR         NMAX                 71
058400991014     C** SALVA GLI ULTIMI CAMPI LETTI
058500991014     C  N72              DO
058600991014     C                   MOVE      LOGATT        SAVATT
058700991014     C                   MOVE      LOGNRA        SAVNRA
058800991014     C                   MOVE      LOGNME        SAVNME
058900991014     C                   MOVE      LOGCBI        SAVCBI
059000991014     C                   MOVE      LOGCBE        SAVCBE
059100991014     C                   MOVE      LOGDAC        SAVDAC
059200991014     C                   MOVE      LOGORC        SAVORC
059300991014     C                   MOVE      LOGDAI        SAVDAI
059400991014     C                   MOVE      LOGORI        SAVORI
059500991014     C                   MOVE      LOGDAF        SAVDAF
059600991014     C                   MOVE      LOGORF        SAVORF
059700991014     C                   MOVE      LOGDEA        SAVDEA
059800991014     C                   MOVEL     LOGKPJ        SAVKP1          256
059900991014     C                   MOVE      LOGKPJ        SAVKP2          256
060000991014     C                   MOVE      LOGSTP        SAVSTP
060100991014     C                   END
060200940310     C                   ENDSR
060300940311      *
060400940315      *----------------------------------------------------*
060500940315      * EMTS1 - EMISSIONE SUBFILE AZIONI                   *
060600940315      *----------------------------------------------------*
060700940315      *
060800940315     C     EMTS1         BEGSR
060900940315      * Emissione video Titolo per Tipo Analisi Azione
061000940315     C                   SELECT
061100940322     C     D0TIAN        WHENEQ    'C'
061200940315     C                   WRITE     K850D1C
061300940315     C                   MOVEL     DAORC         C1DAOR
061400940315     C                   MOVEL     COPRC         C1COPR
061500940322     C     D0TIAN        WHENEQ    'A'
061600940315     C                   WRITE     K850D1A
061700940315     C                   MOVEL     DAORA         C1DAOR
061800940315     C                   MOVEL     COPRA         C1COPR
061900940322     C     D0TIAN        WHENEQ    'E'
062000940315     C                   WRITE     K850D1E
062100940315     C                   MOVEL     DAORE         C1DAOR
062200940315     C                   MOVEL     COPRE         C1COPR
062300940315     C                   ENDSL
062400940315      * Emissione video Tasti Funzionali
062500940315     C                   WRITE     K850D2
062600940315      * Emissione Subfile
062700940315     C                   EXFMT     K850C1
062800960201     C                   MOVE      C1MODE        *IN09
062900940322     C                   Z-ADD     SFLNRR        C1NRR
063000940315     C                   ENDSR
063100940315      *
063200940311      *----------------------------------------------------*
063300940311      * PARZIA - CONTROLLA ALTRE PARZIALIZZAZIONI          *
063400940311      *----------------------------------------------------*
063500940311      *
063600940311     C     PARZIA        BEGSR
063700940311     C                   SETOFF                                       50
063800940317     C                   MOVEL     LOGKPJ        JBADS
063900021002av1  C                   MOVE      *All'9'       All9             20
064000021025     C
064100991018      * Solo anomale
064200940406     C     D0ERR         IFEQ      'S'
064300940406     C     LOGATT        ANDNE     'E'
064400940406     C                   SETON                                        50
064500940406     C                   ENDIF
064600991018      * Codice azione
064700940418     C     D0AZN         IFNE      *BLANK
064800991020     C                   MOVEA     D0AZN         WA1
064900991020     C                   MOVEA     CDA           WA2
065000991020     C                   Z-ADD     1             WX                1 0
065100991020     C     ' '           LOOKUP    WA1(WX)                                58
065200991020     C   58              MOVEA     *BLANK        WA2(WX)
065300991020     C                   MOVEA     WA1           WA1C              4
065400991020     C                   MOVEA     WA2           WA2C              4
065500991020     C     WA1C          IFNE      WA2C
065600940418     C                   SETON                                        50
065700991020     C                   ENDIF
065800940418     C                   ENDIF
065900940315      * Tipo Azione
066000940315     C     D0TIAZ        IFNE      *BLANK
066100940315     C     D0TIAZ        IFEQ      'I'
066200940316     C     TIA           ANDNE     *BLANK
066300940315     C                   SETON                                        50
066400940315     C                   ELSE
066500940315     C     D0TIAZ        IFEQ      'B'
066600940316     C     TIA           ANDNE     'B'
066700940315     C                   SETON                                        50
066800940315     C                   ENDIF
066900940315     C                   ENDIF
067000940315     C                   ENDIF
067100940311      * Profilo Utente
067200021002AV1  C**N50D0PRUT        IFNE      *BLANK
067300021002 "   C**** D0PRUT        ANDNE     NPU
067400021002 "   C****               SETON                                        50
067500021002 "   C****               ENDIF
067600021025av1  C  N50              DO
067700021104     C                   if        d0prut<>*blank
067800021025av2  C                   If        %SubSt(D0PrUt:
067900021025 "   C                              %Len(%TrimR(D0PrUt)):1) = '*'
068000021025 "   C                   Eval      WPrUt = %SubSt(D0PrUt:1:
068100021025 "   C                             %Len(%TrimR(D0PrUt))-1)
068200021025 "   C**** D0PRUT        Cat       All9:0        PRUT999          10
068300021025 "   C     WPrUt         Cat       All9:0        PRUT999          10
068400021002 "   C                   If        NPU < D0PRUT or
068500021002 "   C                             NPU > PRUT999
068600021002 "   C                   Eval      *In50 = *On
068700021002 "   C                   EndIf
068800021025 "   C                   Else
068900021025 "   C     D0PRUT        IFNE      *BLANK
069000021025 "   C     D0PRUT        ANDNE     NPU
069100021025 "   C                   SETON                                        50
069200021025 "   C                   ENDIF
069300021025av2  C                   EndIf
069400021104     C                   endif
069500021002AV1  C                   ENDDO
069600940311      * Sistema Informativo
069700940311     C  N50D0SINF        IFNE      *BLANK
069800940311     C     D0SINF        ANDNE     NSI
069900940311     C                   SETON                                        50
070000940311     C                   ENDIF
070100021104      *&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
070200940311      * Terminale
070300021104     C**N50D0TERM        IFNE      *BLANK
070400021104     C**   D0TERM        ANDNE     NTM
070500021104     C**                 SETON                                        50
070600021104     C**                 ENDIF
070700021104     C                   if        not *in50 and
070800021104     C                             D0TERM<>*blank
070900021104     C                   if        $wildTerm
071000021104     C                   eval      d0term_low= %SubSt(D0Term:1:
071100021104     C                             %Len(%TrimR(D0term))-1)
071200021104     C                   eval      d0term_hig= %SubSt(D0Term:1:
071300021104     C                             %Len(%TrimR(D0term))-1)+all9
071400021104     C                   if        NTM < D0term_low or
071500021104     C                             NTM > d0term_hig
071600021104     C                   eval      *in50 = *on
071700021104     C                   endif
071800021104     C                   else
071900021104     C                   if        D0TERM<>NTM
072000021104     C                   eval      *in50=*on
072100021104     C                   endif
072200021104     C                   endif
072300021104     C                   endif
072400021104      *&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
072500021104      *
072600021104      *&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
072700940311      * Coda Input/Esecuzione
072800021104     C                   if        not *in50 and d0cinp<>*blank
072900021104     C                   if        $wildQueue
073000021104     C                   eval      d0cinp_low= %SubSt(D0cinp:1:
073100021104     C                             %Len(%TrimR(D0cinp))-1)
073200021104     C                   eval      d0cinp_hig= %SubSt(D0cinp:1:
073300021104     C                             %Len(%TrimR(D0cinp))-1)+all9
073400021104     C                   else
073500021104     C                   eval      d0cinp_low= d0cinp
073600021104     C                   eval      d0cinp_hig= d0cinp
073700021104     C                   endif
073800021104      *                  ==========
073900991018      * SE CODA INPUT BLANK VALORIZZO IL CAMPO
074000960419     C                   MOVEL     LOGCBI        S1CODA
074100960419     C     S1CODA        IFEQ      *BLANK
074200960419     C                   CLEAR                   DS036
074300960419     C                   MOVEL     LOGNME        JNAM36
074400960419     C                   MOVEL     '*ALL'        UNAM36
074500960419     C                   MOVEL     '*ALL'        JNBR36
074600960419     C                   CALL      'BAK036R'                            99
074700960419     C                   PARM                    DS036
074800960419     C                   MOVEL     JOBQ36        S1CODA
074900960419     C                   ENDIF
075000021104      *                  ==========
075100940314     C                   SELECT
075200940322     C     D0TIAN        WHENEQ    'C'
075300021104     C                   if        s1coda<d0cinp_low or
075400021104     C                             s1coda>d0cinp_hig
075500021104     C                   eval      *in50=*on
075600021104     C                   endif
075700021104     C*    D0CINP        IFNE      S1CODA
075800021104     C*                  SETON                                        50
075900021104     C*                  ENDIF
076000940322     C     D0TIAN        WHENEQ    'A'
076100940322     C     D0TIAN        OREQ      'E'
076200021104     C                   if        logcbe<d0cinp_low or
076300021104     C                             logcbe>d0cinp_hig
076400021104     C                   eval      *in50=*on
076500021104     C                   endif
076600021104     C*    D0CINP        IFNE      LOGCBE
076700021104     C*                  SETON                                        50
076800021104     C*                  ENDIF
076900940314     C                   ENDSL
077000940311     C                   ENDIF
077100021104      *&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
077200021104      *
077300021104      *
077400940311      * Ora
077500940311     C  N50D0ORA         IFNE      *ZERO
077600940311     C     D0MIN         ORNE      *ZERO
077700940311     C     D0SEC         ORNE      *ZERO
077800940314     C                   Z-ADD     D0ORA         GG
077900940314     C                   Z-ADD     D0MIN         MM
078000940314     C                   Z-ADD     D0SEC         AA
078100991014     C                   Z-ADD     D0GG          GGV
078200991014     C                   Z-ADD     D0MM          MMV
078300991014     C                   Z-ADD     D0AA          AAV
078400991018     C     AAV           IFLT      40
078500991018     C                   Z-ADD     1             SSV
078600991018     C                   ELSE
078700991018     C                   Z-ADD     0             SSV
078800991018     C                   ENDIF
078900991018      *
079000940314     C                   SELECT
079100940322     C     D0TIAN        WHENEQ    'C'
079200991018     C     LOGORC        IFLT      DATA
079300991018     C     LOGDAF        ANDLE     DATAV
079400940314     C                   SETON                                        50
079500940314     C                   ENDIF
079600940322     C     D0TIAN        WHENEQ    'A'
079700991018     C     LOGORI        IFLT      DATA
079800991018     C     LOGDAF        ANDLE     DATAV
079900940314     C                   SETON                                        50
080000940314     C                   ENDIF
080100940322     C     D0TIAN        WHENEQ    'E'
080200940314     C     LOGORF        IFLT      DATA
080300991014     C     LOGDAF        ANDLE     DATAV
080400940314     C                   SETON                                        50
080500940314     C                   ENDIF
080600940314     C                   ENDSL
080700940311     C                   ENDIF
080800940311     C                   ENDSR
080900940310      *
081000940311      *----------------------------------------------------*
081100940311      * VALS1 - VALORIZZA CAMPI SUBFILE AZIONI             *
081200940311      *----------------------------------------------------*
081300940311      *
081400940311     C     VALS1         BEGSR
081500960329     C     STJ           IFEQ      'H'
081600960329     C                   MOVE      'H'           S1HLD
081700960329     C                   ELSE
081800960329     C                   CLEAR                   S1HLD
081900960329     C                   END
082000940406     C     LOGATT        IFEQ      'E'
082100940406     C                   MOVE      'E'           S1ERR
082200940406     C                   ELSE
082300940406     C                   CLEAR                   S1ERR
082400940406     C                   ENDIF
082500940311     C                   MOVEL     LOGDEA        S1DESA
082600961119     C                   MOVEL     LOGDEA        S1DESA
082700991014     C* SE AZIONE ANNULLATA LO SEGNALA NELLA DESCRIZIONE
082800991014     C     LOGATT        IFEQ      'A'
082900991014     C                   MOVEL     'A'           S1ERR
083000991014     C                   END
083100960129     C                   MOVEL     LOGDEA        S1DES
083200940311     C                   MOVEL     NPU           S1PRUT
083300940311     C                   MOVEL     NSI           S1SINF
083400940314     C                   Z-ADD     NRL           S1NUML
083500940316     C                   Z-ADD     LOGNRA        S1NRAZ
083600940314     C                   MOVEL     LOGNME        S1NOML
083700940315     C                   MOVEL     PTY           S1PTY
083800940311     C                   SELECT
083900940322     C     D0TIAN        WHENEQ    'C'
084000941024     C                   MOVEL     JBUF          SCHEDS
084100941024     C     SCHEDS        IFNE      *BLANK
084200951124     C     SCHEDS        ANDNE     *ALL'0'
084300941024     C                   MOVEL     'S'           S1ERR
084400941024     C                   ENDIF
084500940311     C                   Z-ADD     LOGDAC        DATA
084600940311     C                   EXSR      INVDAT
084700940311     C                   Z-ADD     DATA          S1DATA
084800940311     C                   Z-ADD     LOGORC        S1ORA
084900940311     C                   MOVEL     LOGCBI        S1CODA
085000951114     C     S1CODA        IFEQ      *BLANK
085100951124     C     S1PTY         OREQ      *BLANK
085200951114     C                   CLEAR                   DS036
085300951114     C                   MOVEL     LOGNME        JNAM36
085400951114     C                   MOVEL     '*ALL'        UNAM36
085500951114     C                   MOVEL     '*ALL'        JNBR36
085600951114     C                   CALL      'BAK036R'                            99
085700951114     C                   PARM                    DS036
085800951114     C                   MOVEL     JOBQ36        S1CODA
085900951124     C                   MOVEL     JOBP36        S1PTY
086000951114     C                   ENDIF
086100940322     C     D0TIAN        WHENEQ    'A'
086200940311     C                   Z-ADD     LOGDAI        DATA
086300940311     C                   EXSR      INVDAT
086400940311     C                   Z-ADD     DATA          S1DATA
086500940311     C                   Z-ADD     LOGORI        S1ORA
086600940311     C                   MOVEL     LOGCBE        S1CODA
086700940322     C     D0TIAN        WHENEQ    'E'
086800940311     C                   Z-ADD     LOGDAF        DATA
086900940311     C                   EXSR      INVDAT
087000940311     C                   Z-ADD     DATA          S1DATA
087100940311     C                   Z-ADD     LOGORF        S1ORA
087200940311     C                   MOVEL     LOGCBE        S1CODA
087300940311     C                   ENDSL
087400020516X0127X*****PRO850        IFEQ      'U'
087500020516  "  C* se pgm richiamato in modalità "analisi azioni utente"
087600020516  "  C*  l'utente non può agire sulle azioni di altri utenti
087700020516X0127C     $PgmAziUte    IFEQ      *On
087800940418     C     NPU           ANDNE     KNMUS
087900940418     C                   SETON                                        76
088000940418     C                   MOVE      *ON           S1PROT
088100940418     C                   ELSE
088200940418     C                   SETOFF                                       76
088300940418     C                   MOVE      *OFF          S1PROT
088400940418     C                   ENDIF
088500940311     C                   ENDSR
088600940322      *
088700940314      *----------------------------------------------------*
088800940314      * CTRS1 - CONTROLLO OPZIONI                          *
088900940314      *----------------------------------------------------*
089000940322      *
089100940314     C     CTRS1         BEGSR
089200951108     C                   Z-ADD     0             $OPZ
089300960206     C                   MOVE      ' '           PRIMO7
089400940322     C                   SETOFF                                       50
089500951109     C                   SETOFF                                       10
089600940314     C                   READC     K850S1                                 40
089700940314     C     *IN40         DOWEQ     '0'
089800940322     C                   SETOFF                                       9975
089900020516X0127C* se pgm richiamato in modalità "analisi azioni utente" e
090000020516  "  C*  l'utente ha possibilità limitate --> è abilitata solo opz.5
090100020516  "  C                   If        $PgmAziUte = *On and
090200020516  "  C                             PRFAAU = '1' and
090300020516  "  C                             S1Opz <> *Blank and S1Opz <> '5'
090400020516  "  C                   SetOn                                        9974
090500020516X0127C                   Else
090600940314     C                   SELECT
090700021028      * 1=Gestione messaggi
090800021028     C
090900021028     C     S1OPZ         WHENEQ    '1'
091000021028     C                   CALL      'BAK220'
091100021028     C                   PARM                    S1NRAZ
091200940314      * 2=Modifica
091300940314     C     S1OPZ         WHENEQ    '2'
091400951107     C                   Z-ADD     2             $OPZ
091500941026     C     S1NRAZ        CHAIN(N)  KPLOG01L                           99
091600991014     C  N99LOGATT        IFEQ      'A'
091700991014     C                   SETON                                        99
091800991014     C                   END
091900940323     C  N50
092000940323     CAN 99              SETON                                        82
092100940323     C     *IN99         IFEQ      '0'
092200940323     C                   MOVEL     LOGKPJ        JBADS
092300940323     C     TIA           IFEQ      'B'
092400940321     C                   MOVEL     S1NOML        D4NOML
092500951103     C                   MOVE      S1CODA        D4NEWC
092600951103     C                   MOVE      S1PTY         D4NEWP
092700951114     C     JBUF          IFNE      *BLANK
092800951109     C                   MOVEL     JBUF          DATORA
092900951114     C                   ELSE
093000951114     C                   CLEAR                   DATS
093100951114     C                   CLEAR                   ORAS
093200951114     C                   ENDIF
093300951114     C     DATAA         IFNE      *BLANK
093400951114     C                   MOVE      GG4           D4AA
093500951114     C                   MOVE      MM4           D4MM
093600951114     C                   MOVE      AA4           D4GG
093700951114     C                   ENDIF
093800951114     C     ORAA          IFNE      *BLANK
093900951114     C                   MOVE      ORA4          D4ORA
094000951114     C                   MOVE      MIN4          D4MIN
094100951114     C                   ENDIF
094200940317     C     *INKL         DOUEQ     '1'
094300940321     C     *INKC         OREQ      '1'
094400940321     C     *IN80         OREQ      '0'
094500951107     C     *IN81         ANDEQ     '0'
094600951107     C     *IN77         ANDEQ     '0'
094700951107     C     *IN78         ANDEQ     '0'
094800951108     C     *IN79         ANDEQ     '0'
094900960222     C                   SETOFF                                       07
095000940317     C                   EXFMT     K850D4
095100940317     C     *INKL         IFEQ      '0'
095200940317     C                   EXSR      CTRD4
095300951106      * Inversione Data parzializzazione
095400951114     C     D4GG          IFNE      0
095500951114     C     D4MM          ORNE      0
095600951114     C     D4GG          ORNE      0
095700951106     C                   Z-ADD     D4GG          AA4
095800951106     C                   Z-ADD     D4MM          MM4
095900951106     C                   Z-ADD     D4AA          GG4
096000951114     C                   ENDIF
096100951106      * Ora
096200951114     C     D4ORA         IFNE      *ZERO
096300951114     C     D4MIN         ORNE      *ZERO
096400951114     C     D4SEC         ORNE      *ZERO
096500951107     C                   Z-ADD     D4ORA         ORA4
096600951107     C                   Z-ADD     D4MIN         MIN4
096700951107     C                   Z-ADD     D4SEC         SEC4
096800951114     C                   ENDIF
096900951107     C*
097000940318     C     *IN80         IFEQ      '0'
097100951107     C     *IN81         ANDEQ     '0'
097200951107     C     *IN77         ANDEQ     '0'
097300951107     C     *IN78         ANDEQ     '0'
097400951108     C     *IN79         ANDEQ     '0'
097500940321     C                   MOVE      CMD(2)        CMD2
097600940321     C                   MOVEL     LOGKPJ        JBADS
097700940321     C                   MOVEL     LOGNME        NOMLAV
097800940321     C                   MOVEL     D4NEWP        NEWPR
097900940321     C                   MOVEL     D4NEWC        NEWJQ
098000940323     C                   CALL      'QCMDEXC'                            99
098100940318     C                   PARM                    CMD2
098200951109     C                   PARM      80            LUNG
098300951109     C  N50
098400951109     CAN 99              SETON                                        82
098500951109     C     DATS          IFNE      *ZERO
098600951114     C     ORAS          ORNE      *ZERO
098700951109     C                   SETON                                        10
098800951109     C* Data / Ora schedulazione
098900951109     C                   MOVE      CDS(1)        CMD3
099000951109     C                   MOVEL     LOGKPJ        JBADS
099100951109     C                   MOVEL     LOGNME        NMLAV
099200951114     C                   CLEAR                   NEWDS
099300951114     C                   CLEAR                   NEWOS
099400951109     C     DATS          IFNE      *ZERO
099500951114     C                   MOVEL     DATS          DATA
099600951114     C                   EXSR      INVDAT
099700951114     C                   MOVEL     DATA          NEWDS
099800951109     C                   ENDIF
099900951114     C     ORAS          IFNE      *ZERO
100000951109     C                   MOVEL     ORAS          NEWOS
100100951114     C                   ENDIF
100200951109     C                   CALL      'QCMDEXC'                            99
100300951109     C                   PARM                    CMD3
100400951109     C                   PARM      80            LUNG
100500940323     C  N50
100600940323     CAN 99              SETON                                        82
100700951109     C                   ENDIF
100800940323     C     *IN99         IFEQ      '0'
100900941026     C     S1NRAZ        CHAIN     KPLOG01L                           99
101000940323     C                   MOVEL     D4NEWC        LOGCBI
101100940323     C                   MOVEL     D4NEWP        PTY
101200951107     C                   MOVEL     DATORA        JBUF
101300951109     C     *IN10         IFEQ      '1'
101400951109     C                   MOVEL     'S'           STJ
101500951109     C                   ENDIF
101600940323     C                   MOVEL     JBADS         LOGKPJ
101700941026     C  N99              UPDATE    KPLOG001
101800940321     C                   ENDIF
101900940318     C                   ENDIF
102000940317     C                   ENDIF
102100940317     C                   ENDDO
102200940323     C                   ELSE
102300940323     C                   SETON                                        99
102400940323     C  N50              SETON                                        89
102500940323     C                   ENDIF
102600940323     C                   ENDIF
102700940318     C   KL              SETOFF                                       KL
102800990831X0024C                   MOVE      '1'           *INKE
102900940314      * 3=Congelamento
103000940314     C     S1OPZ         WHENEQ    '3'
103100940323     C     S1NRAZ        CHAIN     KPLOG01L                           99
103200991014     C  N99LOGATT        IFEQ      'A'
103300991014     C                   SETON                                        99
103400991014     C                   END
103500940323     C  N50
103600940323     CAN 99              SETON                                        83
103700940323     C     *IN99         IFEQ      '0'
103800940321     C                   MOVE      CMD(3)        CMD2
103900940321     C                   MOVEL     LOGKPJ        JBADS
104000940321     C     TIA           IFEQ      'B'
104100940321     C                   MOVEL     LOGNME        NOMLAV
104200940321     C                   ELSE
104300940321     C                   EXSR      VALCMD
104400940321     C                   ENDIF
104500940323     C                   CALL      'QCMDEXC'                            99
104600940318     C                   PARM                    CMD2
104700951109     C                   PARM      80            LUNG
104800940323     C  N50
104900940323     CAN 99              SETON                                        83
105000940323     C     *IN99         IFEQ      '0'
105100940321     C                   MOVEL     LOGKPJ        JBADS
105200940321     C                   MOVEL     'H'           STJ
105300940321     C                   MOVEL     JBADS         LOGKPJ
105400940323     C                   UPDATE    KPLOG001
105500940321     C                   ENDIF
105600940318     C                   ENDIF
105700940314      * 4=Annullamento
105800940314     C     S1OPZ         WHENEQ    '4'
105900951124     C     D0TIAN        ANDNE     'E'
106000951114     C  NKF              SETON                                        419975
106100940323     C     S1NRAZ        CHAIN     KPLOG01L                           99
106200991014     C  N99LOGATT        IFEQ      'A'
106300991014     C                   SETON                                        99
106400991014     C                   END
106500940323     C  N50
106600940323     CAN 99              SETON                                        84
106700951106     C     *IN99         IFEQ      '0'
106800970317     C                   MOVEL     LOGKPJ        JBADS
106900951106     C     *INKF         IFEQ      '1'
107000970317     C*
107100970317     C     D0TIAN        IFEQ      'C'
107200970317     C* SE ANALISI AZIONI IN CODA ANNULLO SOLO SE NON È ATTIVO
107300970317     C                   EXSR      CHKACT
107400970317     C*
107500970317     C     ACTIVE        IFEQ      *ON
107600970317     C                   UNLOCK    KPLOG01L
107700970317     C                   GOTO      A001
107800970317     C                   ENDIF
107900970317     C                   ENDIF
108000970317     C*
108100951106     C                   MOVE      CMD(4)        CMD2
108200940321     C                   MOVEL     LOGKPJ        JBADS
108300940321     C     TIA           IFEQ      'B'
108400940321     C                   MOVEL     LOGNME        NOMLAV
108500940321     C                   ELSE
108600940321     C                   EXSR      VALCMD
108700940321     C                   ENDIF
108800951106     C                   CALL      'QCMDEXC'                            99
108900940318     C                   PARM                    CMD2
109000951109     C                   PARM      80            LUNG
109100940323     C  N50
109200940323     CAN 99              SETON                                        84
109300940318     C                   MOVEL     'A'           LOGATT
109400940318     C                   TIME                    ORA               6 0
109500940321     C                   Z-ADD     ORA           LOGORF
109600951109     C                   Z-ADD     UDATE         DATA
109700940318     C                   EXSR      INVDAT
109800940318     C                   Z-ADD     DATA          LOGDAF
109900991018      * se l' anno è inferiore a 40 assumo che siamo nel XXI° secolo
110000991018     C     GG            IFLT      40
110100991018     C                   MOVEL     1             LOGDAF
110200991018     C                   ENDIF
110300940321     C     LOGDAI        IFEQ      0
110400940321     C                   Z-ADD     LOGDAF        LOGDAI
110500940321     C                   Z-ADD     ORA           LOGORI
110600951106     C                   ENDIF
110700951106     C                   UPDATE    KPLOG001
110800951106     C                   ENDIF
110900940318     C                   ENDIF
111000970317     C     A001          TAG
111100940314      * 5=Visualizzazione
111200940314     C     S1OPZ         WHENEQ    '5'
111300941026     C     S1NRAZ        CHAIN(N)  KPLOG01L                           99
111400991014     C  N99LOGATT        IFEQ      'A'
111500991014     C                   SETON                                        99
111600991014     C                   END
111700940323     C  N50
111800940323     CAN 99              SETON                                        85
111900940323     C     *IN99         IFEQ      '0'
112000940316     C                   EXSR      VALD3
112100940315     C                   EXFMT     K850D3
112200940315     C                   ENDIF
112300940322     C   KL              SETOFF                                       KL
112400940314      * 6=Rilascio
112500940314     C     S1OPZ         WHENEQ    '6'
112600940323     C     S1NRAZ        CHAIN     KPLOG01L                           99
112700991014     C  N99LOGATT        IFEQ      'A'
112800991014     C                   SETON                                        99
112900991014     C                   END
113000940323     C  N50
113100940323     CAN 99              SETON                                        86
113200940323     C     *IN99         IFEQ      '0'
113300940321     C                   MOVE      CMD(6)        CMD2
113400940321     C                   MOVEL     LOGKPJ        JBADS
113500940321     C     TIA           IFEQ      'B'
113600940321     C                   MOVEL     LOGNME        NOMLAV
113700940321     C                   ELSE
113800940321     C                   EXSR      VALCMD
113900940321     C                   ENDIF
114000940323     C                   CALL      'QCMDEXC'                            99
114100940318     C                   PARM                    CMD2
114200951109     C                   PARM      80            LUNG
114300940323     C  N50
114400940323     CAN 99              SETON                                        86
114500940323     C     *IN99         IFEQ      '0'
114600940321     C                   MOVEL     LOGKPJ        JBADS
114700940321     C                   MOVEL     ' '           STJ
114800940321     C                   MOVEL     JBADS         LOGKPJ
114900940323     C                   UPDATE    KPLOG001
115000940321     C                   ENDIF
115100940318     C                   ENDIF
115200960206      * 7=Modifica MULTIPLA
115300960206     C     S1OPZ         WHENEQ    '7'
115400991014     C     S1ERR         IFNE      'A'
115500960206     C                   Z-ADD     7             $OPZ
115600960206B1   C     PRIMO7        IFEQ      ' '
115700960206     C                   CLEAR                   K850D4
115800960206     C                   CLEAR                   DATS
115900960206     C                   CLEAR                   ORAS
116000960206B2   C     *INKL         DOUEQ     '1'
116100960206     C     *INKC         OREQ      '1'
116200960206     C     *IN80         OREQ      '0'
116300960206     C     *IN81         ANDEQ     '0'
116400960206     C     *IN77         ANDEQ     '0'
116500960206     C     *IN78         ANDEQ     '0'
116600960206     C     *IN79         ANDEQ     '0'
116700960222     C                   SETON                                        07
116800960222     C                   EXFMT     K850D4
116900960206B3   C     *INKL         IFEQ      '0'
117000960206     C                   EXSR      CTRD4
117100960206E3   C                   ENDIF
117200960206E2   C                   ENDDO
117300960206     C*
117400960206     C     *INKL         IFEQ      '0'
117500960206     C     *INKC         ANDEQ     '0'
117600960206     C                   MOVE      '1'           PRIMO7            1
117700960206     C                   ENDIF
117800960206E1   C                   ENDIF
117900960206     C*
118000960206B1   C     *INKL         IFEQ      '0'
118100960206     C     *INKC         ANDEQ     '0'
118200960215     C     S1NRAZ        CHAIN(N)  KPLOG01L                           99
118300960206     C                   MOVE      CMD(2)        CMD2
118400960222     C                   CLEAR                   PTYBLK
118500960206     C                   MOVEL     LOGKPJ        JBADS
118600960206     C                   MOVEL     LOGNME        NOMLAV
118700960206     C                   MOVEL     D4NEWC        NEWJQ
118800960206     C                   CALL      'QCMDEXC'                            99
118900960222     C                   PARM                    CMD2
119000960206     C                   PARM      80            LUNG
119100960206     C  N50
119200960206     CAN 99              SETON                                        82
119300960206     C*
119400960206     C     S1NRAZ        CHAIN     KPLOG01L                           99
119500960206     C                   MOVEL     D4NEWC        LOGCBI
119600960206B2   C     *IN10         IFEQ      '1'
119700960206     C                   MOVEL     'S'           STJ
119800960206E2   C                   ENDIF
119900960206     C  N99              UPDATE    KPLOG001
120000960206B1   C                   ENDIF
120100960206     C   KL              SETOFF                                       KL
120200991014     C                   END
120300940323      * 8=DSPJOB
120400940314     C     S1OPZ         WHENEQ    '8'
120500941026     C     S1NRAZ        CHAIN(N)  KPLOG01L                           99
120600991014     C  N99LOGATT        IFEQ      'A'
120700991014     C                   SETON                                        99
120800991014     C                   END
120900940323     C  N50
121000940323     CAN 99              SETON                                        85
121100940323     C     *IN99         IFEQ      '0'
121200940321     C                   MOVE      CMD(5)        CMD2
121300940321     C                   MOVEL     LOGKPJ        JBADS
121400940321     C     TIA           IFEQ      'B'
121500940321     C                   MOVEL     LOGNME        NOMLAV
121600940321     C                   ELSE
121700940318     C                   EXSR      VALCMD
121800940321     C                   ENDIF
121900940323     C                   CALL      'QCMDEXC'                            99
122000940314     C                   PARM                    CMD2
122100951109     C                   PARM      80            LUNG
122200940323     C  N50
122300940323     CAN 99              SETON                                        85
122400940321     C                   ENDIF
122500940314      * 9=Riesecuzione
122600940314     C     S1OPZ         WHENEQ    '9'
122700941026     C     S1NRAZ        CHAIN(N)  KPLOG01L                           99
122800991014     C  N99LOGATT        IFEQ      'A'
122900991014     C                   SETON                                        99
123000991014     C                   END
123100940323     C  N50
123200940323     CAN 99              SETON                                        87
123300940323     C     *IN99         IFEQ      '0'
123400940321     C                   MOVEL     LOGKPJ        JBADS
123500940321     C     TIA           IFEQ      'B'
123600940323     C     LOGDAF        IFNE      0
123700940322     C                   MOVEL     KPJBA         JBADS
123800020516      *
123900940321     C                   MOVEL     LOGKPJ        KPJBA
124000960206     C                   CALL      'BAK850R1'
124100960206     C                   PARM                    KPJBU
124200960206     C                   PARM      '0'           EXIT              1
124300960206     C     EXIT          IFEQ      '0'
124400940418     C                   CALL      'BCH09'
124500940321     C                   PARM                    KPJBA
124600940418     C                   CALL      'BCH10'
124700940418     C                   PARM                    KPJBA
124800020516      *
124900960206     C                   ENDIF
125000960206     C                   MOVEL     JBADS         KPJBA
125100940321     C                   ELSE
125200940323     C                   SETON                                        99
125300940323     C  N50              SETON                                        88
125400940321     C                   ENDIF
125500940323     C                   ELSE
125600940323     C                   SETON                                        99
125700940323     C  N50              SETON                                        89
125800940323     C                   ENDIF
125900940321     C                   ENDIF
126000940314     C                   ENDSL
126100020516X0127C                   EndIf
126200940322      * Gestione Errore
126300940321     C     *IN99         IFEQ      '1'
126400940321     C  N50              Z-ADD     S1NRR         C1NRR
126500940321     C  N50              SETON                                        50
126600940321     C                   SETON                                        75
126700940321     C                   ELSE
126800951114     C  N41              MOVE      *BLANK        S1OPZ
126900940321     C                   ENDIF
126901021111     C                   exsr      kmsg
127000951114     C                   UPDATE    K850S1
127100940314     C                   READC     K850S1                                 40
127200991014     C**
127300991014     C** A FINE LETTURA DEL SFL REIMPOSTA I CAMPI SPORCATI
127400991014     C** DA UNA EVENTUALE SCELTA DI OPZIONE...
127500991014     C     *IN40         IFEQ      '1'
127600991014     C                   MOVE      SAVATT        LOGATT
127700991014     C                   MOVE      SAVNRA        LOGNRA
127800991014     C                   MOVE      SAVNME        LOGNME
127900991014     C                   MOVE      SAVCBI        LOGCBI
128000991014     C                   MOVE      SAVCBE        LOGCBE
128100991014     C                   MOVE      SAVDAC        LOGDAC
128200991014     C                   MOVE      SAVORC        LOGORC
128300991014     C                   MOVE      SAVDAI        LOGDAI
128400991014     C                   MOVE      SAVORI        LOGORI
128500991014     C                   MOVE      SAVDAF        LOGDAF
128600991014     C                   MOVE      SAVORF        LOGORF
128700991014     C                   MOVE      SAVDEA        LOGDEA
128800991014     C                   MOVEL     SAVKP1        LOGKPJ
128900991014     C                   MOVE      SAVKP2        LOGKPJ
129000991014     C                   MOVE      SAVSTP        LOGSTP
129100991014     C                   ENDIF
129200951108     C                   ENDDO
129300940314     C                   ENDSR
129400940317      *
129500940317      *----------------------------------------------------*
129600940317      * CTRD4 - CONTR. WINDOWS MODIFICA CODA E PRIORITA'   *
129700940317      *----------------------------------------------------*
129800940317      *
129900940317     C     CTRD4         BEGSR
130000951107     C*
130100951107     C                   SETOFF                                       8081
130200951107     C                   SETOFF                                       7778
130300951108     C                   SETOFF                                       7379
130400940317     C                   MOVE      CMD(1)        CMD1
130500940317     C                   MOVEL     D4NEWC        NOME
130600940317     C                   MOVEL     '*JOBQ  '     TIPO
130700940318     C                   CALL      'QCMDEXC'                            80
130800940317     C                   PARM                    CMD1
130900951109     C                   PARM      80            LUNG
131000021001      * ---------------------------------------------
131100021001      * exit x controllo se la coda va bene
131200021001      * ---------------------------------------------
131300021001     c     *in80         ifeq      '1'
131400021001av1  C                   call      'BAK850U2'                           99
131500021111av1  C                   parm                    KPJBA
131600021001av1  C                   parm                    D4NEWC
131700021001av1  C                   parm                    okko
131800021001     C     *in99         ifeq      '0'
131900021111     C     okko          ifeq      'KO'
132000021001     C                   move      '1'           *in80
132100021001     C                   end
132200021001     C                   end
132300021001     C                   end
132400021001      * ---------------------------------
132500951108      * Controllo Priorità
132600960222     C  N07D4NEWP        IFNE      '6'
132700951107     C     D4NEWP        ANDNE     '5'
132800951107     C     D4NEWP        ANDNE     '4'
132900960329     C     D4NEWP        ANDNE     '7'
133000990831X0024C     D4NEWP        ANDNE     '8'
133100990831X0024C     D4NEWP        ANDNE     '9'
133200951107     C                   SETON                                        81
133300951107     C                   ENDIF
133400951107      * Controllo Data
133500960222     C  N07D4AA          IFNE      0
133600951114     C     D4MM          ORNE      0
133700951114     C     D4GG          ORNE      0
133800951107     C     D4MM          IFGT      12
133900951107     C     D4MM          ORLT      1
134000951107     C                   SETON                                        78
134100951107     C                   ELSE
134200951107     C     D4MM          IFEQ      2
134300951107     C     D4AA          DIV       4             RIS               2 0
134400951107     C                   MVR                     RES               1 0
134500951107     C     RES           IFEQ      0
134600951107     C                   Z-ADD     29            MES(2)
134700951107     C                   ELSE
134800951107     C                   Z-ADD     28            MES(2)
134900951107     C                   ENDIF
135000951107     C                   ENDIF
135100951107     C     D4GG          IFGT      MES(D4MM)
135200951107     C     D4GG          ORLT      1
135300951107     C                   SETON                                        78
135400951107     C                   ENDIF
135500951107     C                   ENDIF
135600951114     C                   ENDIF
135700951107      * Controllo Ora
135800960222     C  N07D4ORA         IFNE      0
135900951114     C     D4MIN         ORNE      0
136000951107     C     D4ORA         IFGT      23
136100951107     C     D4MIN         ORGT      59
136200951107     C     D4SEC         ORGT      59
136300951107     C                   SETON                                        77
136400951107     C                   ENDIF
136500951114     C                   ENDIF
136600951108      *
136700960222     C  N07D4GG          IFEQ      *ZERO
136800951124     C     D4MM          ANDEQ     *ZERO
136900951124     C     D4AA          ANDEQ     *ZERO
137000951124     C     S1ERR         ANDEQ     'S'
137100951124     C                   SETON                                        73
137200951124     C                   ENDIF
137300951108      *
137400960222     C  N07D4ORA         IFEQ      *ZERO
137500951124     C     D4MIN         ANDEQ     *ZERO
137600951124     C     D4SEC         ANDEQ     *ZERO
137700951124     C     S1ERR         ANDEQ     'S'
137800951124     C                   SETON                                        79
137900951124     C                   ENDIF
138000940317     C                   ENDSR
138100940316      *
138200940316      *----------------------------------------------------*
138300940316      * VALD3 - VALORIZZAZIONE VIDEATA VISUALIZZ.          *
138400940316      *----------------------------------------------------*
138500940316      *
138600940316     C     VALD3         BEGSR
138700940316     C                   Z-ADD     LOGNRA        D3NRAZ
138800940316     C                   MOVEL     LOGDEA        D3DEAZ
138900940316     C                   MOVEL     LOGNME        D3NOML
139000940316     C                   MOVEL     LOGCBI        D3CBIN
139100940316     C                   MOVEL     LOGCBE        D3CBES
139200951107     C*
139300940316     C                   Z-ADD     LOGDAC        DATA
139400940316     C                   EXSR      INVDAT
139500940316     C                   Z-ADD     DATA          D3DINS
139600940316     C                   Z-ADD     LOGORC        D3OINS
139700951107     C*
139800960329     C                   MOVEL     LOGKPJ        JBADS
139900960329     C     JBUF          IFNE      *BLANK
140000960329     C                   MOVEL     JBUF          SCHEDS
140100960329     C                   ELSE
140200960329     C                   CLEAR                   SCHEDS
140300960329     C                   ENDIF
140400960329     C                   MOVEL     DSCHE         DATA
140500951114     C                   EXSR      INVDAT
140600951114     C                   Z-ADD     DATA          D3DSCH
140700941024     C                   MOVEL     OSCHE         D3OSCH
140800951107     C*
140900940316     C                   Z-ADD     LOGDAI        DATA
141000940316     C                   EXSR      INVDAT
141100940316     C                   Z-ADD     DATA          D3DINI
141200940316     C                   Z-ADD     LOGORI        D3OINI
141300940316     C                   Z-ADD     LOGDAF        DATA
141400940316     C                   EXSR      INVDAT
141500940316     C                   Z-ADD     DATA          D3DFIN
141600940316     C                   Z-ADD     LOGORF        D3OFIN
141700940317     C                   MOVEL     LOGKPJ        JBADS
141800940316     C                   MOVEL     CDA           D3COAZ
141900940316     C                   MOVEL     NPU           D3PRUT
142000940316     C                   MOVEL     NSI           D3SINF
142100940316     C                   Z-ADD     NRL           D3NUML
142200940316     C                   MOVEL     NTM           D3TERM
142300940316     C                   MOVEL     STJ           D3STJB
142400940316     C                   MOVEL     PTY           D3PRJB
142500960205     C*
142600960205     C                   MOVEA     JBU1          JB
142700960205     C                   EXSR      JJ
142800960206     C                   MOVEA     JB            D3JBU1
142900960205     C*
143000960205     C                   MOVEA     JBU2          JB
143100960205     C                   EXSR      JJ
143200960206     C                   MOVEA     JB            D3JBU2
143300960205     C*
143400960206     C                   MOVEA     JBU3          JB
143500960205     C                   EXSR      JJ
143600960206     C                   MOVEA     JB            D3JBU3
143700960206     C*
143800960206     C                   MOVEA     JBU4          JB
143900960206     C                   EXSR      JJ
144000960206     C                   MOVEA     JB            D3JBU4
144100960206     C                   MOVEA     JBU5          JB
144200960206     C                   EXSR      JJ
144300960206     C                   MOVEA     JB            D3JBU5
144400940316     C                   ENDSR
144500960205     C     JJ            BEGSR
144600960206     C                   DO        60            J                 3 0
144700960205     C     JB(J)         IFLT      X'40'
144800960205     C                   MOVE      X'1C'         JB(J)
144900960205     C                   ENDIF
145000960205     C                   ENDDO
145100960205     C                   ENDSR
145200940316      *
145300940316      *----------------------------------------------------*
145400940318      * VALCMD - VALORIZZ. DS COMANDI JOB (QCMDEXC)        *
145500940316      *----------------------------------------------------*
145600940316      *
145700940318     C     VALCMD        BEGSR
145800940321     C                   MOVEL     S1NUML        NUMJOB
145900940316     C                   MOVEA     *BLANK        SK1
146000940316     C                   MOVEA     S1PRUT        SK1(1)
146100940418     C                   Z-ADD     1             X                 3 0
146200940321     C     ' '           LOOKUP    SK1(X)                                 49
146300940316     C                   MOVEA     '/'           SK1(X)
146400940316     C                   ADD       1             X
146500940316     C                   MOVEA     S1NOML        SK1(X)
146600940316     C                   MOVEA     SK1           PRUNOJ
146700940316     C                   ENDSR
146800970317      *
146900970317      *----------------------------------------------------*
147000970317      * CHKACT - CONTROLLA CHE LAVORO NON SIA ATTIVO*
147100970317      *----------------------------------------------------*
147200970317      *
147300970317     C     CHKACT        BEGSR
147400970317     C                   CLEAR                   DS036
147500970317     C                   MOVEL     LOGNME        JNAM36
147600970317     C                   MOVEL     '*ALL'        UNAM36
147700970317     C                   MOVEL     '*ALL'        JNBR36
147800970317     C                   CALL      'BAK036R'                            99
147900970317     C                   PARM                    DS036
148000970317     C*
148100970317     C     JSTS36        IFEQ      '*ACTIVE'
148200970317     C                   MOVEL     *ON           ACTIVE            1
148300970317     C                   ELSE
148400970317     C* CONGELA
148500970317     C                   MOVE      CMD(3)        CMD2
148600970317     C                   MOVEL     LOGKPJ        JBADS
148700970317     C     TIA           IFEQ      'B'
148800970317     C                   MOVEL     LOGNME        NOMLAV
148900970317     C                   ELSE
149000970317     C                   EXSR      VALCMD
149100970317     C                   ENDIF
149200970317     C                   CALL      'QCMDEXC'                            99
149300970317     C                   PARM                    CMD2
149400970317     C                   PARM      80            LUNG
149500970317     C* RITESTA
149600970317     C                   MOVEL     *OFF          ACTIVE
149700970317     C                   CLEAR                   DS036
149800970317     C                   MOVEL     LOGNME        JNAM36
149900970317     C                   MOVEL     '*ALL'        UNAM36
150000970317     C                   MOVEL     '*ALL'        JNBR36
150100970317     C                   CALL      'BAK036R'                            99
150200970317     C                   PARM                    DS036
150300970317     C*
150400970317     C     JSTS36        IFEQ      '*ACTIVE'
150500970317     C                   MOVEL     *ON           ACTIVE            1
150600970317     C* SCONGELA
150700970317     C                   MOVE      CMD(6)        CMD2
150800970317     C                   MOVEL     LOGKPJ        JBADS
150900970317     C     TIA           IFEQ      'B'
151000970317     C                   MOVEL     LOGNME        NOMLAV
151100970317     C                   ELSE
151200970317     C                   EXSR      VALCMD
151300970317     C                   ENDIF
151400970317     C                   CALL      'QCMDEXC'                            99
151500970317     C                   PARM                    CMD2
151600970317     C                   PARM      80            LUNG
151700970317     C                   ENDIF
151800970317     C                   ENDIF
151900970317     C                   ENDSR
152000970317      *
152100940310      *----------------------------------------------------*
152200940310      * INVDAT - INVERSIONE DATA                           *
152300940310      *----------------------------------------------------*
152400940310      *
152500940310     C     INVDAT        BEGSR
152600940310     C                   Z-ADD     DATA          DATROV            6 0
152700940310     C                   MOVEL     AA            DATROV
152800940310     C                   MOVE      GG            DATROV
152900940310     C                   Z-ADD     DATROV        DATA
153000940310     C                   ENDSR
153100940316      *
153200951106      *----------------------------------------------------*
153300951106      * INVDAT1- INVERSIONE DATA SCHEDULAZIONE             *
153400951106      *----------------------------------------------------*
153500951106      *
153600951106     C     INVDA1        BEGSR
153700951106     C                   Z-ADD     DATS          DATRO1            6 0
153800951106     C                   MOVEL     AA4           DATRO1
153900951106     C                   MOVE      GG4           DATRO1
154000951106     C                   Z-ADD     DATRO1        DATS
154100951106     C                   ENDSR
154200951106      *
154300990910X0026 *----------------------------------------------------
154400990908  "  C* INIZIALIZZAZIONE TESTATA DEL VIDEO
154500990908  "   *----------------------------------------------------
154600990908  "  C     INZT1         BEGSR
154700990908  "   *
154800990908  "  C* Reperisco dati società
154900990908  "  C                   CLEAR                   XSOCDS
155000990908  "  C                   CALL      'BAK500R'
155100990908  "  C                   PARM                    XSOCDS
155200990908  "  C*
155300990908  "  C* Controlli dati società
155400990908  "  C     XSOCDS        IFNE      *BLANKS
155500990908  "  C                   MOVEL     XSOCDS        SOC001
155600990908  "  C                   ENDIF
155700990908  "  C*
155800990908  "  C* Valorizzazione campi univoci testate
155900990908  "  C                   MOVEL     KNSIF         NOMSIF
156000990908  "  C                   MOVEL     XSCDSI        NOMDIT
156100990908  "  C                   MOVEL     DSPGM         NOMPGM
156200990908  "   *
156300990908  "  C                   ENDSR
156400990910X0026C/EJECT
156500940309      *----------------------------------------------------*
156600940309      * DEFCAM - DEFINIZIONE CAMPI E KLIST                 *
156700940309      *----------------------------------------------------*
156800940309      *
156900930708     C     DEFCAM        BEGSR
157000991013      *
157100940309     C     *ENTRY        PLIST
157200940309     C                   PARM                    KPJBA
157300991013      *
157400990910X0026C                   EXSR      INZT1
157500991013      *
157600020516X0127X*****              MOVEL     KPJBU         PRO850            1
157700020516  "   * verifico se pgm chiamato in modalità "analisi azioni" (azione K850)
157800020516  "   *  oppure "analisi azioni utente" (azione K851)
157900020516  "  C                   If        KCDAZ = 'K851'
158000020516  "  C                   Eval      $PgmAziUte = *On
158100020516  "  C                   Else
158200020516  "  C                   Eval      $PgmAziUte = *Off
158300020516  "  C                   EndIf
158400020516X0127 *
158500951107     C                   Z-ADD     0             $OPZ              1 0
158600951108     C                   MOVE      *BLANK        $INZS1            1
158700991014      *
158800940310     C     *LIKE         DEFINE    LOGDAI        KDI
158900991014     C     *LIKE         DEFINE    LOGORI        KOI
159000940310     C     *LIKE         DEFINE    LOGDAF        KDF
159100991014     C     *LIKE         DEFINE    LOGORF        KOF
159200991014     C     *LIKE         DEFINE    LOGATT        SAVATT
159300991014     C     *LIKE         DEFINE    LOGNRA        SAVNRA
159400991014     C     *LIKE         DEFINE    LOGNME        SAVNME
159500991014     C     *LIKE         DEFINE    LOGCBI        SAVCBI
159600991014     C     *LIKE         DEFINE    LOGCBE        SAVCBE
159700991014     C     *LIKE         DEFINE    LOGDAC        SAVDAC
159800991014     C     *LIKE         DEFINE    LOGORC        SAVORC
159900991014     C     *LIKE         DEFINE    LOGDAI        SAVDAI
160000991014     C     *LIKE         DEFINE    LOGORI        SAVORI
160100991014     C     *LIKE         DEFINE    LOGDAF        SAVDAF
160200991014     C     *LIKE         DEFINE    LOGORF        SAVORF
160300991014     C     *LIKE         DEFINE    LOGDEA        SAVDEA
160400991014     C     *LIKE         DEFINE    LOGSTP        SAVSTP
160500951109     C     *LIKE         DEFINE    DATS          WWDAT
160600991013      *
160700021104av3  C     KLOG8         KLIST
160800021104av3  C                   KFLD                    KUS
160900021104av3  C                   KFLD                    KDF
161000021104av3  C                   KFLD                    KOF
161100021104av3  C                   KFLD                    KDI
161200021104av3  C                   KFLD                    KOI
161300021104av3  C     KLOG8A        KLIST
161400021104av3  C                   KFLD                    KUS
161500021104av3  C                   KFLD                    KDF
161600021104av3  C     KLOG8B        KLIST
161700021104av3  C                   KFLD                    KUS
161800021104      *
161900021104av3  C     KLOG9         KLIST
162100021104av3  C                   KFLD                    KQU
162200021104av3  C                   KFLD                    KDF
162300021104av3  C                   KFLD                    KOF
162400021104av3  C                   KFLD                    KDI
162500021104av3  C                   KFLD                    KOI
162600021104av3  C     KLOG9A        KLIST
162800021104av3  C                   KFLD                    KQU
162900021104av3  C                   KFLD                    KDF
163000021104av3  C     KLOG9B        KLIST
163200021104av3  C                   KFLD                    KQU
163300021104     C     KLOG2         KLIST
163400940310     C                   KFLD                    KDF
163500991014     C                   KFLD                    KOF
163600940310     C                   KFLD                    KDI
163700991014     C                   KFLD                    KOI
163800940311      * Default video Guida
163900020516X0127C* se pgm richiamato in modalità "analisi azioni utente"...
164000020516  "  X*****PRO850        IFEQ      'U'
164100020516  "  C     $PgmAziUte    IFEQ      *On
164200020516X0127C* - propongo il profilo
164300951107     C                   MOVEL     KNMUS         D0PRUT
164400020516X0127C* - se l'utente ha possibilità limitate --> proteggo profilo
164500020516  "  C                   If        PRFAAU = '1'
164600020516  "  C                   Eval      *In06 = *On
164700020515  "  C                   Else
164800020516  "  C                   Eval      *In06 = *Off
164900020515X0127C                   EndIf
165000951107     C                   ENDIF
165100940309     C                   Z-ADD     UDATE         DATA
165200940309     C                   Z-ADD     GG            D0GG
165300940309     C                   Z-ADD     MM            D0MM
165400940309     C                   Z-ADD     AA            D0AA
165500940421     C                   MOVE      'N'           D0ERR
165600951108     C                   MOVE      *OFF          $INZS1
165700001031X0097C                   Z-ADD     0             CURR              2 0
165800001031  "  C                   Z-ADD     0             CURC              2 0
165900001031  "  C                   Z-ADD     0             H1RIGA
166000001031X0097C                   Z-ADD     0             H1COLO
166100020515      *
166200930708     C                   ENDSR
166300001031X0097C***************************************************************
166400001031  "  C* RICERCHE
166500001031  "  C***************************************************************
166600001031X0097C     SEARCH        BEGSR
166700001031     C*
166800001031     C* determino Riga/Colonna del cursore
166900001106     C                   MOVE      BINRIG        R$$
167000001106     C                   MOVE      BINCOL        C$$
167100001031     C                   Z-ADD     RIRI          CURR
167200001031     C                   Z-ADD     COCO          CURC
167300001031     C*
167400001031     C                   SELECT
167500001031     C*
167600001031     C* Profilo
167700001031     C     H1NMFL        WHENEQ    'D0PRUT'
167800020516X0127C     *In06         ANDEQ     *Off
167900001031     C                   RESET                   K980DS
168000001031     C                   MOVE      '01'          OPZ980
168100001031     C                   MOVEL     K980DS        KPJBU
168200001031     C                   CALL      'BAK980R'
168300001031     C                   PARM                    KPJBA
168400001031     C                   MOVEL     KPJBU         K980DS
168500001031     C     PRF980        IFNE      *BLANK
168600001031     C                   MOVEL     PRF980        D0PRUT
168700001031     C                   ENDIF
168800001031     C*
168900001031     C* Sistema informativo
169000001031     C     H1NMFL        WHENEQ    'D0SINF'
169100001031     C                   RESET                   K988DS
169200001031     C                   MOVE      '01'          OPZ988
169300001031     C                   MOVEL     K988DS        KPJBU
169400001031     C                   CALL      'BAK988R'
169500001031     C                   PARM                    KPJBA
169600001031     C                   MOVEL     KPJBU         K988DS
169700001031     C     NSI988        IFNE      *BLANK
169800001031     C                   MOVEL     NSI988        D0SINF
169900001031     C                   ENDIF
170000001211X0099C*
170100001211  "  C* FMTD1 - Azione
170200001211  "  C     H1NMFL        WHENEQ    'D0AZN'
170300001211  "  C* se s.i. impostato correttamente eseguo OVRDBF in libl del s.i.
170400001211  "  C                   Eval      $OvrDB = *Off
170500001211  "  C                   If        D0SINF <> *Blank
170600001211  "  C                   CallB     'BAK003R1'
170700001211  "  C                   Parm                    D0SINF
170800001211  "  C                   Parm                    KFLRC
170900001211  "  C                   Parm                    LibL
171000001211  "  C                   Parm                    Errore
171100001211  "  C                   If        Errore = *Off
171200001211  "  C* eseguo OVRDBF file azioni
171300001211  "  C                   Eval      $OvrDB = *On
171400001211  "  C                   Eval      File = 'KFAZN10F'
171500001211  "  C                   Eval      SisInf = D0SINF
171600001211  "  C                   Eval      Lingua = KFLRC
171700001211  "  C                   Eval      Scope = 'J'
171800001211  "  C                   ExSr      ExeOVRDBF
171900001211  "  C                   EndIf
172000001211  "  C                   EndIf
172100001211  "  C* pgm ricerca azioni
172200001211  "  C                   Reset                   BAK986DS
172300001211  "  C                   Eval      Opz986 = '01'
172400001211  "  C                   MoveL     BAK986DS      KPJBU
172500001211  "  C                   CALL      'BAK986R'
172600001211  "  C                   PARM                    KPJBA
172700001211  "  C                   MoveL     KPJBU         BAK986DS
172800001211  "  C                   If        KCAZN986 <> *Blank
172900001211  "  C                   Eval      D0AZN = KCAZN986
173000001211  "  C                   EndIf
173100001211  "  C* se eseguito OVRDBF eseguo DLTOVR file azioni
173200001211  "  C                   If        $OvrDB = *On
173300001211  "  C                   ExSr      ExeDLTOVR
173400001211X0099C                   EndIf
173500001031     C*
173600001031     C* Ricerca non abilitata in questa posizione
173700001031     C                   OTHER
173800001031     C                   SETON                                        6899
173900001031     C*
174000001031     C                   ENDSL
174100001031     C* imposto pos. del cursore
174200001031     C                   Z-ADD     CURR          H1RIGA
174300001031     C                   Z-ADD     CURC          H1COLO
174400001031     C*
174500001031X0097C                   ENDSR
174600001211X0099C****************************************************************
174700001211  "  C* OVRDBF file in lista librerie S.I.                           *
174800001211  "  C****************************************************************
174900001211X0099C     ExeOVRDBF     BEGSR
175000001211     C*
175100001211     C                   Call      'BAK931R'
175200001211     C                   Parm                    File
175300001211     C                   Parm                    SisInf
175400001211     C                   Parm                    Lingua
175500001211     C                   Parm                    Scope
175600001211     C                   Parm                    Errore
175700001211     C*
175800001211X0099C                   ENDSR
175900001211X0099C****************************************************************
176000001211  "  C* DLTOVR file                                                  *
176100001211  "  C****************************************************************
176200001211X0099C     ExeDLTOVR     BEGSR
176300001211     C*
176400001211     C                   Call      'BAK932C'
176500001211     C                   Parm                    File
176600001211     C                   Parm                    Scope
176700001211     C*
176800001211X0099C                   ENDSR
176900020515X0127C****************************************************************
177000020515  "  C* Operazioni iniziali
177100020515  "  C****************************************************************
177200020515X0127C     *INZSR        BEGSR
177300020515     C*
177400020515     C* aggancio profilo per reperire possibilità dell'utente nel pgm
177500020516     C     KNMUS         Chain     KPPRF01L
177600020515     C*
177700020515X0127C                   ENDSR
177701021111X0127C****************************************************************
177702021111  "  C* Controlla se esistono messaggi
177703021111  "  C****************************************************************
177704021111X0127C     KMSG          BEGSR
177705021111      *
177706021111     C                   move      *off          *in55
177707021111av1  C                   move      '  '          okko
177708021111      *
177709021111av1  C                   call      'BAK850U4'                           99
177710021111av1  C                   parm                    S1NRAZ
177711021111av1  C                   parm                    okko
177712021111      *
177713021111     c     okko          ifeq      'OK'
177714021111     c                   move      *on           *in55
177715021111     c                   end
177716021111     C*
177717021111X0127C                   ENDSR
177800020905av1  C****************************************************************
177900020905av1  C* Controllo record in lettura
178000020905av1  C****************************************************************
178100020906av1  C     TESTCK        BEGSR
178200020905av1  C*
178300020905av1   * inserimento exit routine per controllo letture
178400020905av1  C     tcki          tag
178500020905av1  C                   setoff                                       99
178600020905av1  C                   move      '  '          okko              2
178700020905av1  C     *in72         ifeq      '0'
178800021001av1  C                   call      'BAK850U1'                           99
178900020905av1  C                   parm                    dslog
179000021001av1  C                   parm                    okko
179100020905av1  C     *in99         cabeq     '1'           tckf
179200020905av1   *
179300020905av1  C     okko          ifeq      'KO'
179400020905av1   * ri_Lettura per Tipo Analisi Azioni
179500020905av1  C                   SELECT
179501021111     C*---------------------------------------------------------------------
179600020905av1  C     D0TIAN        WHENEQ    'C'
179700021104     C                   if        d0prut<>*blank and not $wildUser
179800021111     C                   if        d0cinp<>*blank and not $wildQueue
179900021111     C     KLOG9         READE     KPLOG09L                               72
180000021111     C                   else
180100021104     C     KLOG8         READE     KPLOG08L                               72
180200021111     C                   endif
180300021104     C                   else
180400020905av1  C     KLOG2         READE     KPLOG04L                               72
180500021104     C                   endif
180501021111     C*---------------------------------------------------------------------
180600020905av1  C     D0TIAN        WHENEQ    'A'
180700021104     C                   if        d0prut<>*blank and not $wildUser
180800021111     C                   if        d0cinp<>*blank and not $wildQueue
180900021111     C     KLOG9A        READE     KPLOG09L                               72
181000021111     C                   else
181100021104     C     KLOG8A        READE     KPLOG08L                               72
181200021111     C                   endif
181300021104     C                   else
181400020905av1  C     KDF           READE     KPLOG04L                               72
181500021104     C                   endif
181501021111     C*---------------------------------------------------------------------
181600020905av1  C     D0TIAN        WHENEQ    'E'
181700021104     C                   if        d0prut<>*blank and not $wildUser
181800021111     C                   if        d0cinp<>*blank and not $wildQueue
181900021111     C     KLOG9B        READE     KPLOG09L                               72
182000021111     C                   else
182100021104     C     KLOG8B        READE     KPLOG08L                               72
182200021111     C                   endif
182300021104     C                   else
182400020905av1  C                   READ      KPLOG04L                               72
182500021104     C                   endif
182600020905av1  C                   ENDSL
182601021111     C*---------------------------------------------------------------------
182700020905av1  C
182800020905av1  C                   goto      tcki
182900020905av1  C
183000020905av1  C                   endif
183100020905av1  C                   endif
183200020905av1   *
183300020905av1  C     tckf          tag
183400020905av1  C*
183500020905av1  C                   ENDSR
183600020905      *
183700940309** 'CMD' - Schiera per QCMDX
183800940309CHKOBJ OBJ(          ) OBJTYPE(       )
183900951109CHGJOB JOB(      /                     ) JOBPTY( )  JOBQ(          )
184000940318HLDJOB JOB(      /                     )
184100940318ENDJOB JOB(      /                     ) OPTION(*IMMED)
184200940318DSPJOB JOB(      /                     )
184300940318RLSJOB JOB(      /                     )
184400951109** 'CDS' - Schiera per QCMDX
184500951109CHGJOB JOB(      /                     ) SCDDATE(      ) SCDTIME(      )
184600940309** 'MES' - Schiera Mesi dell'anno
184700940309312831303130313130313031
