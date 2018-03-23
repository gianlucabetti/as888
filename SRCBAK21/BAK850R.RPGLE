000100001211     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PKXBND) ACTGRP(PROBAS)
000200001211     H*PARMS CVTOPT(*NONE)
000300960222     H DECEDIT('0,') DATEDIT(*DMY.)
000400021111     Hoption(*SRCSTMT : *NODEBUGIO)
000500940309      ********************************************************************
000600940309      *  BAK850R -  ANALISI AZIONI UTENTE                                *
000700940309      ********************************************************************
000800940310      *                                                                  *
000900940310      *------------------------------------------------------------------*
001000940309      * USO DEGI INDICATORI:                                             *
001100940309      *------------------------------------------------------------------*
001200940310      *                                                                  *
001300940309      *--- K850D0                                                        *
001400940311      * 90 - NON PRESENTI AZIONI NEI LIMITI INDICATI                     *
001500940315      * 91 - TIPO AZIONI NON CORRETTO                                    *
001600940310      * 92 - SISTEMA INFORMATIVO INESISTENTE                             *
001700940309      * 93 - ORA NON CORRETTA                                            *
001800940309      * 94 - DATA NON CORRETTA                                           *
001900940322      * 95 - TIPO ANALISI AZIONI NON CORRETTO                            *
002000940309      * 96 - CODA INPUT INESISTENTE                                      *
002100940309      * 97 - TERMINALE INESISTENTE                                       *
002200940310      * 98 - PROFILO NON VALIDO                                          *
002300940309      *                                                                  *
002400940321      *--- K850S1/C1                                                     *
002500940316      * 69 - ROLLUP                                                      *
002600940309      * 70 - SFLCLR / N70 - SFLDSPCTL                                    *
002700940309      * 71 - SFLDSP                                                      *
002800940309      * 72 - SFLEND                                                      *
002900940321      * 75 - SFLNXTCHG                                                   *
003000940318      * 83 - LAVORO NON CONGELATO                                        *
003100940318      * 84 - LAVORO NON CANCELLATO                                       *
003200940318      * 85 - LAVORO NON TROVATO                                          *
003300940318      * 86 - LAVORO NON CONGELATO                                        *
003400940321      * 87 - AZIONE NON RIESEGUITA                                       *
003500940310      *                                                                  *
003600940317      *--- K850D4                                                        *
003700951107      * 77 - ORA NON CORRETTA                                            *
003800951107      * 78 - DATA NON CORRETTA                                           *
003900940321      * 80 - ERRORE NUOVA CODA                                           *
004000951107      * 81 - PRIORITA' NON CORRETTA                                      *
004100940322      * 82 - LAVORO NON MODIFICATO                                       *
004200940317      *                                                                  *
004300940309      *---                                                               *
004400940318      * 40 - READC                                                       *
004500940321      * 49 - WORK                                                        *
004600940321      * 50 - WORK                                                        *
004700940309      * 99 - ERRORE GENERICO                                             *
004800940310      *------------------------------------------------------------------*
004900020905      *------------------------------------------------------------------*
005000940310      *--- File Input
005100991014     FKPLOG04L  IF   E           K DISK
005200991014     F                                     RENAME(KPLOG000:KPLOG004)
005300021104     FKPLOG08L  IF   E           K DISK    RENAME(KPLOG000:KPLOG008)
005400021111     FKPLOG09L  IF   E           K DISK    RENAME(KPLOG000:KPLOG009)
005500940318     FKFSIF01L  IF   E           K DISK
005600020515X0127FKPPRF01L  IF   E           K DISK
005700940318      *--- File Update
005800940323     FKPLOG01L  UF   E           K DISK
005900940323     F                                     RENAME(KPLOG000:KPLOG001)
006000940310      *--- File Display
006100940309     FBAK850D   CF   E             WORKSTN
006200940309     F                                     SFILE(K850S1:S1NRR)
006300940309     F                                     INFDS(INFS1)
006400940309      *------------------------------------------------------------------*
006500940314      *--- Schiera per QCMDEXC
006600951109     D CMD             S             80    DIM(7) CTDATA PERRCD(1)
006700951109     D CDS             S             80    DIM(1) CTDATA PERRCD(1)
006800940314      *--- Schiera Mesi dell'anno
006900940309     D MES             S              2  0 DIM(12) CTDATA PERRCD(12)
007000940314      *--- Schiera WORK
007100940314     D SK1             S              1    DIM(50)
007200960206     D JB              S              1    DIM(60)
007300991020     D WA1             S              1    DIM(4)
007400991020     D WA2             S              1    DIM(4)
007500940309      *------------------------------------------------------------------*
007600020905av1  D DSLOG         E DS                  extname(kplog00f)
007700020905     D KPJBA         E DS
007800940309      *--- INFDS del Video
007900940309     D INFS1           DS           512
008000930708     D  BINRIG               370    370
008100930708     D  BINCOL               371    371
008200930708     D  SFLNRR               378    379B 0
008300940309      *--- DS Data
008400940309     D                 DS
008500940309     D  DATA                   1      6  0
008600940309     D  GG                     1      2  0
008700940309     D  MM                     3      4  0
008800940309     D  AA                     5      6  0
008900991014      *--- DS Data
009000991014     D                 DS
009100991018     D  DATAV                  1      7  0
009200991018     D  SSV                    1      1  0
009300991018     D  AAV                    2      3  0
009400991018     D  MMV                    4      5  0
009500991018     D  GGV                    6      7  0
009600951107     D DATORA          DS
009700951107     D  DATS                   1      6  0
009800951114     D  DATAA                  1      6
009900951114     D  ORAA                   7     12
010000951114     D  ORAS                   7     12  0
010100951106     D  GG4                    1      2  0
010200951106     D  MM4                    3      4  0
010300951106     D  AA4                    5      6  0
010400951107     D  ORA4                   7      8  0
010500951107     D  MIN4                   9     10  0
010600951107     D  SEC4                  11     12  0
010700960329     D*
010800941024     D SCHEDS          DS
010900941024     D  DSCHE                  1      6
011000941024     D  OSCHE                  7     10
011100940314      *--- DS per CHKOBJ
011200940309     D                 DS
011300951109     D  CMD1                   1     80
011400940309     D  NOME                  12     21
011500940309     D  TIPO                  32     38
011600940318      *--- DS per Comandi JOB
011700951114     D* PARAMETRI DI I/O di BAK036R
011800951114     D DS036           DS
011900951114     D  JNAM36                 1     10
012000951114     D  UNAM36                11     20
012100951114     D  JNBR36                21     26
012200951114     D  JOBQ36                27     36
012300951124     D  JOBP36                37     37
012400970317     D  JSTS36                38     47
012500940314     D                 DS
012600951109     D  CMD2                   1     80
012700940321     D  NUMJOB                12     17
012800940321     D  NOMLAV                12     21
012900940315     D  PRUNOJ                19     39
013000960222     D  PTYBLK                41     50
013100940323     D  NEWPR                 49     49
013200940321     D  NEWJQ                 58     67
013300951109      *--- DS per Comandi JOB
013400951109     D                 DS
013500951109     D  CMD3                   1     80
013600951109     D  NMLAV                 12     21
013700951109     D  NEWDS                 50     55
013800951109     D  NEWOS                 66     71
013900940310      *--- DS per KPJBA Azione
014000940317     D JBADS           DS           502
014100940317     D  NPU                    1     10
014200940317     D  NTM                   20     29
014300940317     D  NRL                   47     52  0
014400940315     D  STJ                   53     53
014500940315     D  CDA                   60     63
014600940316     D  TIA                   64     64
014700940310     D  NSI                   92    101
014800940315     D  PTY                  155    155
014900941024     D  JBUF                 209    246
015000941024     D  JBU1                 247    306
015100940317     D  JBU2                 307    366
015200940317     D  JBU3                 367    426
015300940317     D  JBU4                 427    486
015400940317     D  JBU5                 487    502
015500990910X0026D*-------------
015600990908  "  D* Dati di ambiente ottenuti da XSOC
015700990908  "  D SOC001        E DS                  EXTNAME(XSOC001DS)
015800990908  "  D  SALDOC       E                     EXTFLD(XSCSALDOC)
015900990908  "  D  SALDOA       E                     EXTFLD(XSCSALDOA)
016000990908  "  D*-------------
016100990908  "  D* DS Interna per dati di output di XSOC
016200990908  "  D XSOCDS          DS          1000
016300990908  "  D*-------------
016400990908  "  D* REPERISCO NOME PGM
016500990908  "  D STATUS         SDS           333
016600990910X0026D  DSPGM            *PROC
016700001211X0099D*-------------
016800001211  "  D* Ricerca azioni
016900001211  "  D BAK986DS      E DS                  INZ
017000001211X0099D*-------------
017100001031X0097 * S.I.
017200001031  "  D K988DS        E DS                  EXTNAME(BAK988DS)
017300001031  "  D  NSI988       E                     EXTFLD(KNMSI988)
017400001031  "  D  DSI988       E                     EXTFLD(KDSSI988)
017500001031  "   * PROFILI
017600001031  "  D K980DS        E DS                  EXTNAME(BAK980DS)
017700001031  "  D  PRF980       E                     EXTFLD(PROFILO980)
017800001031  "  D  DES980       E                     EXTFLD(DESCR980)
017900001031  "  D  MNI980       E                     EXTFLD(MENUINZ980)
018000001031  "  D  SIF980       E                     EXTFLD(SISINF980)
018100020515  "  D*-------------
018200001031  "  D CURSOR          DS
018300001031  "  D  RIRI                   1      2B 0 INZ
018400001031  "  D  R$$                    2      2
018500001031  "  D  COCO                   3      4B 0 INZ
018600001031X0097D  C$$                    4      4
018700020515X0127D*-------------
018800020515  "  D* Scomposizione campo PRFUSF
018900020515  "  D PRFUSF          DS
019000020515  "  D  PRFPgA                 1     10A
019100020515  "  D  PRFLbA                11     20A
019200020515  "  D  PRFAAU                21     21A
019300020515X0127D*-------------
019400940314      *-- Costanti
019500960201     D PAGINA          C                   CONST(12)
019600940314     D DAORC           C                   CONST('-Immiss.-')
019700940314     D COPRC           C                   CONST('- Input -')
019800940314     D DAORA           C                   CONST(' -Inizio-')
019900940314     D COPRA           C                   CONST('- Esec. -')
020000940314     D DAORE           C                   CONST(' - Fine -')
020100940314     D COPRE           C                   CONST('- Esec. -')
020200940322      *
020300020516X0127D*-------------
020400020516X0127D $PgmAziUte      S              1A
020500001211X0099D File            S             10A
020600001211  "  D SisInf          S             10A
020700001211  "  D Lingua          S              1A
020800001211  "  D Scope           S              1A
020900001211  "  D Errore          S              1A
021000001211  "  D LibL            S            240A
021100001211  "  D $OvrDB          S              1A
021200001211X0099D*-------------
021300021025av2  D WPrUt           S             10A
021400021111av2  D ZPrUt           S             10A
021500021104      *=============================================================================================
021600021104av3  D KQU             S                   like(d0cinp)
021700021104av3  D KUS             S                   like(d0prut)
021800021104av3  D $wildUser       S               N
021900021104av3  D $wildQueue      S               N
022000021104av3  D $wildTerm       S               N
022100021104     D d0term_low      S                   like(d0term)
022200021104     D d0term_hig      S                   like(d0term)
022300021104     D d0cinp_low      S                   like(d0cinp)
022400021104     D d0cinp_hig      S                   like(d0cinp)
022500021127     D d0prut_low      S                   like(d0prut)
022600021127     D d0prut_hig      S                   like(d0prut)
022700021104      *=============================================================================================
022800940309      *----------------------------------------------------*
022900940309      *   FLUSSO PRINCIPALE                                *
023000940309      *----------------------------------------------------*
023100940322      *
023200960201     C                   SETON                                        09
023300940309     C                   EXSR      DEFCAM
023400940309     C     *INKC         DOWEQ     '0'
023500990910X0026C     *INKL         ANDEQ     '0'
023600940311      * Emissione video Guida
023700001031X0097C     EMTD0         TAG
023800001031  "  C     *IN99         IFEQ      *ON
023900001031  "  C                   SETOFF                                       99
024000001031  "  C                   WRITE     K850D0
024100001031  "  C                   SETON                                        99
024200001031X0097C                   ENDIF
024300940309     C                   EXFMT     K850D0
024400940309     C     *INKC         IFEQ      '0'
024500990910X0026C     *INKL         ANDEQ     '0'
024600001031X0097 * Ricerca
024700001031  "  C     *INKD         IFEQ      *ON
024800001031  "  C                   EXSR      SEARCH
024900001031  "  C                   GOTO      EMTD0
025000001031X0097C                   ENDIF
025100940311      * Controllo video Guida
025200951124     C                   EXSR      CTRD0
025300940309     C     *IN99         IFEQ      '0'
025400951108     C     SUBF          TAG
025500940310      * Gestione Subfile Azioni
025600940309     C                   EXSR      GESS1
025700940309     C                   ENDIF
025800940309     C                   ENDIF
025900940309     C                   ENDDO
026000940309      * Fine Elaborazione
026100930708     C                   SETON                                        LR
026200940309      *
026300940309      *----------------------------------------------------*
026400940311      * CTRD0 - CONTROLLO VIDEO GUIDA                      *
026500940309      *----------------------------------------------------*
026600940309      *
026700940309     C     CTRD0         BEGSR
026800940309     C                   SETOFF                                       999897
026900940322     C                   SETOFF                                       969594
027000940322     C                   SETOFF                                       939291
027100940322     C                   SETOFF                                       90
027200940322      * Controllo Tipo Analisi Azioni
027300940322     C     D0TIAN        IFNE      'C'
027400021127     C     D0TIAN        ANDNE     'A'
027500940322     C     D0TIAN        ANDNE     'E'
027600940322     C                   SETON                                        9599
027700940322     C                   ENDIF
027800940315      * Controllo Tipo Azioni
027900940315     C     D0TIAZ        IFNE      ' '
028000940315     C     D0TIAZ        ANDNE     'B'
028100940315     C     D0TIAZ        ANDNE     'I'
028200940315     C                   SETON                                        9199
028300940315     C                   ENDIF
028400021025     C     D0PRUT        IFNE      *BLANK
028500021025      * Controllo esistenza Profilo Utente
028600021025av2  C                   If        %SubSt(D0PrUt:
028700021025av2  C                              %Len(%TrimR(D0PrUt)):1) <> '*'
028800021025     C                   MOVE      CMD(1)        CMD1
028900021025     C                   MOVEL     D0PRUT        NOME
029000021025     C                   MOVEL     '*USRPRF'     TIPO
029100021025     C                   CALL      'QCMDEXC'                            98
029200021025     C                   PARM                    CMD1
029300021025     C                   PARM      80            LUNG
029400021025     C   98              SETON                                        99
029500021025av2  C                   EndIf
029600021025     C                   ENDIF
029700940309     C     D0SINF        IFNE      *BLANK
029800940309      * Controllo esistenza Sistema Informativo
029900940309     C     D0SINF        CHAIN     KFSIF01L                           92
030000940309     C   92              SETON                                        99
030100940309     C                   ENDIF
030200021104      *
030300021104      *
030400940309     C     D0TERM        IFNE      *BLANK
030500021104av2  C                   If        %SubSt(D0Term:
030600021104av2  C                              %Len(%TrimR(D0Term)):1) <> '*'
030700940309      * Controllo esistenza Terminale
030800940314     C                   MOVE      CMD(1)        CMD1
030900940309     C                   MOVEL     D0TERM        NOME
031000940309     C                   MOVEL     '*DEVD  '     TIPO
031100940309     C                   CALL      'QCMDEXC'                            97
031200940314     C                   PARM                    CMD1
031300021002     C                   PARM      80            LUNG             15 5
031400940309     C   97              SETON                                        99
031500021104     C                   endif
031600940309     C                   ENDIF
031700021104      *
031800021104      *
031900940309     C     D0CINP        IFNE      *BLANK
032000021104av2  C                   If        %SubSt(D0Cinp:
032100021104av2  C                              %Len(%TrimR(D0Cinp)):1) <> '*'
032200940309      * Controllo esistenza Coda Input
032300940314     C                   MOVE      CMD(1)        CMD1
032400940309     C                   MOVEL     D0CINP        NOME
032500940309     C                   MOVEL     '*JOBQ  '     TIPO
032600940309     C                   CALL      'QCMDEXC'                            96
032700940314     C                   PARM                    CMD1
032800951109     C                   PARM      80            LUNG
032900940309     C   96              SETON                                        99
033000021104     C                   endif
033100940309     C                   ENDIF
033200940309      * Controllo Data
033300021127*QQQQC**   D0TIAN        IFNE      'C'
033400021127     C     D0TIAN        IFEQ      'E'
033500951124     C     D0MM          ORNE      0
033600951124     C     D0AA          ORNE      0
033700951124     C     D0GG          ORNE      0
033800951124     C*
033900940309     C     D0MM          IFGT      12
034000940310     C     D0MM          ORLT      1
034100940309     C                   SETON                                        9499
034200940309     C                   ELSE
034300940309     C     D0MM          IFEQ      2
034400940309     C     D0AA          DIV       4             RIS               2 0
034500940309     C                   MVR                     RES               1 0
034600940309     C     RES           IFEQ      0
034700940309     C                   Z-ADD     29            MES(2)
034800941202     C                   ELSE
034900941202     C                   Z-ADD     28            MES(2)
035000940309     C                   ENDIF
035100940309     C                   ENDIF
035200940309     C     D0GG          IFGT      MES(D0MM)
035300940310     C     D0GG          ORLT      1
035400940309     C                   SETON                                        9499
035500940309     C                   ENDIF
035600940309     C                   ENDIF
035700951124     C                   ENDIF
035800940309      * Controllo Ora
035900940309     C     D0ORA         IFGT      23
036000940309     C     D0MIN         ORGT      59
036100940309     C     D0SEC         ORGT      59
036200940309     C                   SETON                                        9399
036300940309     C                   ENDIF
036400930708     C                   ENDSR
036500940309      *
036600940309      *----------------------------------------------------*
036700940310      * GESS1 - GESTIONE SFL AZIONI                        *
036800940309      *----------------------------------------------------*
036900940309      *
037000021127?     *=============================================================================================
037100940309     C     GESS1         BEGSR
037200021104      *
037300021104      *:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
037400021104     C                   eval      $wildUser =%scan('*':d0prut)>0
037500021104     C                   eval      $wildQueue=%scan('*':d0cinp)>0
037600021104     C                   eval      $wildTerm =%scan('*':d0term)>0
037700021127av1  C                   MOVE      *All'9'       All9             20
037800021104      *:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
037900021104      *
038000951108     C     *INKE         DOUEQ     '0'
038100940310      * Inizializzazione Subfile
038200940310     C                   EXSR      INZS1
038300991014      * Ora parzializzazione
038400991014     C                   Z-ADD     D0ORA         GG
038500991014     C                   Z-ADD     D0MIN         MM
038600991014     C                   Z-ADD     D0SEC         AA
038700991014     C                   Z-ADD     DATA          ORA               6 0
038800940311      * Inversione Data parzializzazione
038900940311     C                   Z-ADD     D0GG          AA
039000940311     C                   Z-ADD     D0MM          MM
039100940311     C                   Z-ADD     D0AA          GG
039200021127?     *=============================================================================================
039300021127?     * Imposta profilo partenza/arrivo
039400021127     C                   eval      D0Prut_low = D0prut
039500021127     C                   eval      D0Prut_hig = D0prut
039600021127     C                   if        d0prut<>*blank
039700021127     C                   if        %SubSt(D0PrUt:
039800021127     C                              %Len(%TrimR(D0PrUt)):1) = '*'
039900021127     C                   eval      D0Prut_low = %SubSt(D0PrUt:1:
040000021127     C                             %Len(%TrimR(D0PrUt))-1)
040100021127     C     D0Prut_low    Cat       All9:0        d0Prut_hig
040200021127     C                   endif
040300021127     C                   endif
040400021127?     *
040500021127     C                   eval      D0cinp_low = D0cinp
040600021127     C                   eval      D0cinp_hig = D0cinp
040700021127     C                   if        d0cinp<>*blank
040800021127     C                   if        %SubSt(D0Cinp:
040900021127     C                              %Len(%TrimR(D0cinp)):1) = '*'
041000021127     C                   eval      D0cinp_low = %SubSt(D0cinp:1:
041100021127     C                             %Len(%TrimR(D0cinp))-1)
041200021127     C     D0cinp_low    Cat       All9:0        d0cinp_hig
041300021127     C                   endif
041400021127     C                   endif
041500021127?     *=============================================================================================
041600940315      * Posizionamento e prima Lettura per Tipo Analisi Azioni
041700940311     C                   SELECT
041800021104      *=====================================================
041900940322     C     D0TIAN        WHENEQ    'C'
042000021127av3  C                   eval      KUS=d0prut_low
042100021127av3  C                   eval      KQU=d0cinp_low
042200940311     C                   Z-ADD     0             KDF
042300991014     C                   Z-ADD     0             KOF
042400940311     C                   Z-ADD     0             KDI
042500991014     C                   Z-ADD     0             KOI
042600021127?     *:::::::::::::::::::::::::::::::::::::::::::::::::::::
042700021127     C                   if        d0prut<>*blank
042800021127     C                   if        d0cinp<>*blank
042900021111     C     KLOG9         SETLL     KPLOG09L
043000021127     C                   if        $wildQueue
043100021127     C                   READ      KPLOG09L                               72
043200021127     C                   eval      *in72=*in72 or (LOGCBE>d0cinp_hig)
043300021127     C                             or  LOGDAF<>0 or logorf<>0
043400021127     C                             or  LOGDAI<>0 or logori<>0
043500021127     C                   else
043600021127     C     KLOG9         READE     KPLOG09L                               72
043700021127     C                   endif
043800021127?     *
043900021111     C                   else
044000021104     C     KLOG8         SETLL     KPLOG08L
044100021127     C                   if        $wildUSER
044200021127     C                   READ      KPLOG08L                               72
044300021127     C                   eval      *in72=*in72 or (LOGUSR>d0prut_hig)
044400021127     C                             or  LOGDAF<>0 or logorf<>0
044500021127     C                             or  LOGDAI<>0 or logori<>0
044600021127     C                   else
044700021127     C     KLOG8         READE     KPLOG08L                               72
044800021127     C                   endif
044900021127?     *
045000021111     C                   endif
045100021104     C                   else
045200021127?     *:::::::::::::::::::::::::::::::::::::::::::::::::::::
045300991014     C     KLOG2         SETLL     KPLOG04L
045400991014     C     KLOG2         READE     KPLOG04L                               72
045500021127?     *:::::::::::::::::::::::::::::::::::::::::::::::::::::
045600021104     C                   endif
045700021104av1  C                   EXSR      TESTCK
045800021127?     *:::::::::::::::::::::::::::::::::::::::::::::::::::::
045900021104      *=====================================================
046000020906     C     D0TIAN        WHENEQ    'A'
046100021127av3  C                   eval      KUS=d0prut_low
046200021127av3  C                   eval      KQU=d0cinp_low
046300021127QQQQQC                   Z-ADD     0             KDF
046400021127QQQQQC                   Z-ADD     0             KOF
046500021127QQQQQC                   Z-ADD     1             KDI
046600021127QQQQQC                   Z-ADD     1             KOI
046700021127     C                   if        d0prut<>*blank
046800021127     C                   if        d0cinp<>*blank
046900021111     C     KLOG9         SETLL     KPLOG09L
047000021127     C                   if        $wildQueue
047100021127     C                   eval      *in72=*in72 or (LOGCBE>d0cinp_hig)
047200021127     C                   eval      *in72=*in72 or (LOGDAF>KDF       )
047300021127     C                   else
047400021127     C     KLOG9A        READE     KPLOG09L                               72
047500021127     C                   endif
047600021111     C                   else
047700021104     C     KLOG8         SETLL     KPLOG08L
047800021127     C                   if        $wildUser
047900021127     C                   READ      KPLOG08L                               72
048000021127     C                   eval      *in72=*in72 or (LOGUSR>d0prut_hig)
048100021127     C                   else
048200021127     C     KLOG8A        READE     KPLOG08L                               72
048300021127     C                   endif
048400021111     C                   endif
048500021104     C                   else
048600991014     C     KLOG2         SETLL     KPLOG04L
048700991014     C     KDF           READE     KPLOG04L                               72
048800021104     C                   endif
048900020906av1  C                   EXSR      TESTCK
049000021104      *=====================================================
049100940322     C     D0TIAN        WHENEQ    'E'
049200021127av3  C                   eval      KUS=d0prut_low
049300021127av3  C                   eval      KQU=d0cinp_low
049400940311     C                   Z-ADD     DATA          KDF
049500991018     C     D0AA          IFLT      40
049600991018     C                   MOVEL     1             KDF
049700991018     C                   ENDIF
049800991014     C                   Z-ADD     ORA           KOF
049900991014     C                   Z-ADD     0             KDI
050000021104     C                   Z-ADD     0             KOI
050100021127     C                   if        d0prut<>*blank
050200021127     C                   if        d0cinp<>*blank
050300021111     C     KLOG9         SETLL     KPLOG09L
050400021127     C                   if        $wildQueue
050500021127     C                   READ      KPLOG09L                               72
050600021127     C                   eval      *in72=*in72 or (LOGCBE>d0cinp_hig)
050700021127     C                   else
050800021127     C     klog9b        READe     KPLOG09L                               72
050900021127     C                   endif
051000021111     C                   else
051100021104     C     KLOG8         SETLL     KPLOG08L
051200021127     C                   if        $wildUser
051300021127     C                   READ      KPLOG08L                               72
051400021127     C                   eval      *in72=*in72 or (LOGUSR>d0prut_hig)
051500021127     C                   else
051600021104     C     klog8b        READe     KPLOG08L                               72
051700021111     C                   endif
051800021127     C                   endif
051900021104     C                   else
052000021104     C     KLOG2         SETLL     KPLOG04L
052100021104     C                   READ      KPLOG04L                               72
052200021104     C                   endif
052300020906av1  C                   EXSR      TESTCK
052400940311     C                   ENDSL
052500021104      *=====================================================
052600021104      */////////////////////////////////////////////////////
052700021104      *=====================================================
052800940311      * Caricamento 1a pagina Subfile
052900940311     C                   EXSR      ROLLS1
053000940311     C     S1NRR         COMP      0                                      90
053100940311     C     *IN90         IFEQ      '0'
053200940314     C                   Z-ADD     S1NRR         C1NRR
053300940311      * Caricamento 2a pagina Subfile
053400940311     C                   EXSR      ROLLS1
053500940311     C     *INKC         DOWEQ     '0'
053600940311     C     *INKE         ANDEQ     '0'
053700940315     C     *INKL         ANDEQ     '0'
053800940315      * Emissione Subfile Azioni
053900940315     C                   EXSR      EMTS1
054000940311     C     *INKC         IFEQ      '0'
054100940311     C     *INKE         ANDEQ     '0'
054200940315     C     *INKL         ANDEQ     '0'
054300940311     C     *IN69         IFEQ      '1'
054400940314      * Caricamento altre 2 pagine
054500940314     C                   EXSR      ROLLS1
054600940314     C                   Z-ADD     S1NRR         C1NRR
054700940311     C                   EXSR      ROLLS1
054800940311     C                   ELSE
054900940314      * Controllo opzioni inserite
055000940314     C                   EXSR      CTRS1
055100940311     C                   ENDIF
055200940311     C                   ENDIF
055300940311     C                   ENDDO
055400990920X0026C                   MOVE      '0'           *INKL
055500940311     C                   ENDIF
055600940311     C                   ENDDO
055700940309     C                   ENDSR
055800940310      *
055900940310      *----------------------------------------------------*
056000940310      * INZS1 - INIZIALIZZAZIONE SFL AZIONI                *
056100940310      *----------------------------------------------------*
056200940310      *
056300940310     C     INZS1         BEGSR
056400940322     C                   SETOFF                                       KE
056500940310     C                   Z-ADD     0             S1NRR             4 0
056600940311     C                   Z-ADD     0             NMAX              4 0
056700940311     C                   Z-ADD     0             C1NRR             4 0
056800940310     C                   SETON                                          70
056900940310     C                   SETOFF                                         71
057000940310     C                   WRITE     K850C1
057100940310     C                   SETOFF                                         70
057200020516X0127C* se pgm richiamato in modalità "analisi azioni utente" e
057300020516  "  C*  l'utente ha possibilità limitate --> visualizzo solo opz.5
057400021001  "  C                   If        $PgmAziUte = *On and  PRFAAU = '1'
057500020515  "  C                   Eval      *In08 = *On
057600020515  "  C                   Else
057700020515  "  C                   Eval      *In08 = *Off
057800020515X0127C                   EndIf
057900021001      * ---------------------------------------------
058000021001      * exit x controllo se l' utente è abilitato
058100021001      * ---------------------------------------------
058200021001     C     *in08         ifeq      '0'
058300021001av1  C                   call      'BAK850U3'                           99
058400021111  "  C                   PARM                    KPJBA
058500021127av1  C                   parm      *blanks       okko
058600021111     C     okko          ifeq      'KO'
058700021001  "  C                   move      '1'           *in08
058800021001X0127C                   EndIf
058900021001X0127C                   EndIf
059000021001      *
059100021001     C                   ENDSR
059200940310      *----------------------------------------------------*
059300940311      * ROLLS1 - CARICAMENTO SFL AZIONI (PAGINA)           *
059400940310      *----------------------------------------------------*
059500940310      *
059600940311     C     ROLLS1        BEGSR
059700940322     C                   SETOFF                                       75
059800940322     C                   MOVE      *BLANK        S1OPZ
059900940311     C                   Z-ADD     0             NP                2 0
060000940311     C                   Z-ADD     NMAX          S1NRR
060100940310     C     *IN72         DOWEQ     '0'
060200940317     C     NP            ANDLT     PAGINA
060300940311      * Controlla altre Parzializzazioni
060400940311     C                   EXSR      PARZIA
060500940311     C     *IN50         IFEQ      '0'
060600940310      * Valorizza campi Subfile
060700940311     C                   EXSR      VALS1
060800940310     C                   ADD       1             S1NRR
060900940311     C                   ADD       1             NP
061000021111      *
061100021111     C                   EXSR      KMSG
061200021111      *
061300940310     C                   WRITE     K850S1
061400940311     C                   ENDIF
061500940315      * Lettura per Tipo Analisi Azioni
061600940310     C                   SELECT
061700940322     C     D0TIAN        WHENEQ    'C'
061800021127     C                   if        d0prut<>*blank
061900021127     C                   if        d0cinp<>*blank
062000021127     C                   if        $wildQueue
062100021127     C                   READ      KPLOG09L                               72
062200021127     C                   eval      *in72=*in72 or (LOGCBE>d0cinp_hig)
062300021127     C***                          or  LOGDAF<>0 or logorf<>0
062400021127     C***                          or  LOGDAI<>0 or logori<>0
062500021127     C                   else
062600021127     C     KLOG9         READE     KPLOG09L                               72
062700021127     C                   endif
062800021111     C                   else
062900021127     C                   if        $wildUser
063000021127     C                   READ      KPLOG08L                               72
063100021127     C                   eval      *in72=*in72 or (LOGUSR>d0prut_hig)
063200021127     C***                          or  LOGDAF<>0 or logorf<>0
063300021127     C***                          or  LOGDAI<>0 or logori<>0
063400021127     C                   else
063500021104     C     KLOG8         READE     KPLOG08L                               72
063600021111     C                   endif
063700021127     C                   endif
063800021104     C                   else
063900991014     C     KLOG2         READE     KPLOG04L                               72
064000021104     C                   endif
064100020906av1  C                   EXSR      TESTCK
064200940322     C     D0TIAN        WHENEQ    'A'
064300021127     C                   if        d0prut<>*blank
064400021127     C                   if        d0cinp<>*blank
064500021127     C                   if        $wildQueue
064600021127     C                   READ      KPLOG09L                               72
064700021127     C                   eval      *in72=*in72 or (LOGCBE>d0cinp_hig)
064800021127     C                   eval      *in72=*in72 or (LOGDAF>KDF       )
064900021127     C                   else
065000021111     C     KLOG9A        READE     KPLOG09L                               72
065100021127     C                   endif
065200021111     C                   else
065300021127     C                   if        $wildUser
065400021127     C                   READ      KPLOG08L                               72
065500021127     C                   eval      *in72=*in72 or (LOGUSR>d0prut_hig)
065600021127     C                   else
065700021104     C     KLOG8A        READE     KPLOG08L                               72
065800021111     C                   endif
065900021127     C                   endif
066000021104     C                   else
066100991014     C     KDF           READE     KPLOG04L                               72
066200021104     C                   endif
066300020906av1  C                   EXSR      TESTCK
066400940322     C     D0TIAN        WHENEQ    'E'
066500021127     C                   if        d0prut<>*blank
066600021127     C                   if        d0cinp<>*blank
066700021127     C                   if        $wildQueue
066800021127     C                   READ      KPLOG09L                               72
066900021127     C                   eval      *in72=*in72 or (LOGCBE>d0cinp_hig)
067000021127     C                   else
067100021111     C     klog9b        READe     KPLOG09L                               72
067200021127     C                   endif
067300021111     C                   else
067400021127     C                   if        $wildUser
067500021127     C                   READ      KPLOG08L                               72
067600021127     C                   eval      *in72=*in72 or (LOGUSR>d0prut_hig)
067700021127     C                   else
067800021127     C     klog8b        READe     KPLOG08L                               72
067900021127     C                   endif
068000021111     C                   endif
068100021104     C                   else
068200991014     C                   READ      KPLOG04L                               72
068300021104     C                   endif
068400020906av1  C                   EXSR      TESTCK
068500940310     C                   ENDSL
068600940310     C                   ENDDO
068700940311     C                   Z-ADD     S1NRR         NMAX                 71
068800991014     C** SALVA GLI ULTIMI CAMPI LETTI
068900991014     C  N72              DO
069000991014     C                   MOVE      LOGATT        SAVATT
069100991014     C                   MOVE      LOGNRA        SAVNRA
069200991014     C                   MOVE      LOGNME        SAVNME
069300991014     C                   MOVE      LOGCBI        SAVCBI
069400991014     C                   MOVE      LOGCBE        SAVCBE
069500991014     C                   MOVE      LOGDAC        SAVDAC
069600991014     C                   MOVE      LOGORC        SAVORC
069700991014     C                   MOVE      LOGDAI        SAVDAI
069800991014     C                   MOVE      LOGORI        SAVORI
069900991014     C                   MOVE      LOGDAF        SAVDAF
070000991014     C                   MOVE      LOGORF        SAVORF
070100991014     C                   MOVE      LOGDEA        SAVDEA
070200991014     C                   MOVEL     LOGKPJ        SAVKP1          256
070300991014     C                   MOVE      LOGKPJ        SAVKP2          256
070400991014     C                   MOVE      LOGSTP        SAVSTP
070500991014     C                   END
070600940310     C                   ENDSR
070700940311      *
070800940315      *----------------------------------------------------*
070900940315      * EMTS1 - EMISSIONE SUBFILE AZIONI                   *
071000940315      *----------------------------------------------------*
071100940315      *
071200940315     C     EMTS1         BEGSR
071300940315      * Emissione video Titolo per Tipo Analisi Azione
071400940315     C                   SELECT
071500940322     C     D0TIAN        WHENEQ    'C'
071600940315     C                   WRITE     K850D1C
071700940315     C                   MOVEL     DAORC         C1DAOR
071800940315     C                   MOVEL     COPRC         C1COPR
071900940322     C     D0TIAN        WHENEQ    'A'
072000940315     C                   WRITE     K850D1A
072100940315     C                   MOVEL     DAORA         C1DAOR
072200940315     C                   MOVEL     COPRA         C1COPR
072300940322     C     D0TIAN        WHENEQ    'E'
072400940315     C                   WRITE     K850D1E
072500940315     C                   MOVEL     DAORE         C1DAOR
072600940315     C                   MOVEL     COPRE         C1COPR
072700940315     C                   ENDSL
072800940315      * Emissione video Tasti Funzionali
072900940315     C                   WRITE     K850D2
073000940315      * Emissione Subfile
073100940315     C                   EXFMT     K850C1
073200960201     C                   MOVE      C1MODE        *IN09
073300940322     C                   Z-ADD     SFLNRR        C1NRR
073400940315     C                   ENDSR
073500940315      *
073600940311      *----------------------------------------------------*
073700940311      * PARZIA - CONTROLLA ALTRE PARZIALIZZAZIONI          *
073800940311      *----------------------------------------------------*
073900940311      *
074000940311     C     PARZIA        BEGSR
074100940311     C                   SETOFF                                       50
074200940317     C                   MOVEL     LOGKPJ        JBADS
074300021025     C
074400991018      * Solo anomale
074500940406     C     D0ERR         IFEQ      'S'
074600940406     C     LOGATT        ANDNE     'E'
074700940406     C                   SETON                                        50
074800940406     C                   ENDIF
074900991018      * Codice azione
075000940418     C     D0AZN         IFNE      *BLANK
075100991020     C                   MOVEA     D0AZN         WA1
075200991020     C                   MOVEA     CDA           WA2
075300991020     C                   Z-ADD     1             WX                1 0
075400991020     C     ' '           LOOKUP    WA1(WX)                                58
075500991020     C   58              MOVEA     *BLANK        WA2(WX)
075600991020     C                   MOVEA     WA1           WA1C              4
075700991020     C                   MOVEA     WA2           WA2C              4
075800991020     C     WA1C          IFNE      WA2C
075900940418     C                   SETON                                        50
076000991020     C                   ENDIF
076100940418     C                   ENDIF
076200940315      * Tipo Azione
076300940315     C     D0TIAZ        IFNE      *BLANK
076400940315     C     D0TIAZ        IFEQ      'I'
076500940316     C     TIA           ANDNE     *BLANK
076600940315     C                   SETON                                        50
076700940315     C                   ELSE
076800940315     C     D0TIAZ        IFEQ      'B'
076900940316     C     TIA           ANDNE     'B'
077000940315     C                   SETON                                        50
077100940315     C                   ENDIF
077200940315     C                   ENDIF
077300940315     C                   ENDIF
077400940311      * Profilo Utente
077500021002AV1  C**N50D0PRUT        IFNE      *BLANK
077600021002 "   C**** D0PRUT        ANDNE     NPU
077700021002 "   C****               SETON                                        50
077800021002 "   C****               ENDIF
077900021025av1  C  N50              DO
078000021104     C                   if        d0prut<>*blank
078100021025av2  C                   If        %SubSt(D0PrUt:
078200021025 "   C                              %Len(%TrimR(D0PrUt)):1) = '*'
078300021025 "   C                   Eval      WPrUt = %SubSt(D0PrUt:1:
078400021025 "   C                             %Len(%TrimR(D0PrUt))-1)
078500021025 "   C**** D0PRUT        Cat       All9:0        PRUT999          10
078600021025 "   C     WPrUt         Cat       All9:0        PRUT999          10
078700021127 "   C*******************If        NPU < D0PRUT or
078800021127 "   C                   If        NPU < wPRUT or
078900021002 "   C                             NPU > PRUT999
079000021002 "   C                   Eval      *In50 = *On
079100021002 "   C                   EndIf
079200021025 "   C                   Else
079300021025 "   C     D0PRUT        IFNE      *BLANK
079400021025 "   C     D0PRUT        ANDNE     NPU
079500021025 "   C                   SETON                                        50
079600021025 "   C                   ENDIF
079700021025av2  C                   EndIf
079800021104     C                   endif
079900021002AV1  C                   ENDDO
080000940311      * Sistema Informativo
080100940311     C  N50D0SINF        IFNE      *BLANK
080200940311     C     D0SINF        ANDNE     NSI
080300940311     C                   SETON                                        50
080400940311     C                   ENDIF
080500021104      *&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
080600940311      * Terminale
080700021104     C**N50D0TERM        IFNE      *BLANK
080800021104     C**   D0TERM        ANDNE     NTM
080900021104     C**                 SETON                                        50
081000021104     C**                 ENDIF
081100021104     C                   if        not *in50 and
081200021104     C                             D0TERM<>*blank
081300021104     C                   if        $wildTerm
081400021104     C                   eval      d0term_low= %SubSt(D0Term:1:
081500021104     C                             %Len(%TrimR(D0term))-1)
081600021104     C                   eval      d0term_hig= %SubSt(D0Term:1:
081700021104     C                             %Len(%TrimR(D0term))-1)+all9
081800021104     C                   if        NTM < D0term_low or
081900021104     C                             NTM > d0term_hig
082000021104     C                   eval      *in50 = *on
082100021104     C                   endif
082200021104     C                   else
082300021104     C                   if        D0TERM<>NTM
082400021104     C                   eval      *in50=*on
082500021104     C                   endif
082600021104     C                   endif
082700021104     C                   endif
082800021104      *&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
082900021104      *
083000021104      *&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
083100940311      * Coda Input/Esecuzione
083200021104     C                   if        not *in50 and d0cinp<>*blank
083300021104     C                   if        $wildQueue
083400021104     C                   eval      d0cinp_low= %SubSt(D0cinp:1:
083500021104     C                             %Len(%TrimR(D0cinp))-1)
083600021104     C                   eval      d0cinp_hig= %SubSt(D0cinp:1:
083700021104     C                             %Len(%TrimR(D0cinp))-1)+all9
083800021104     C                   else
083900021104     C                   eval      d0cinp_low= d0cinp
084000021104     C                   eval      d0cinp_hig= d0cinp
084100021104     C                   endif
084200021104      *                  ==========
084300991018      * SE CODA INPUT BLANK VALORIZZO IL CAMPO
084400960419     C                   MOVEL     LOGCBI        S1CODA
084500960419     C     S1CODA        IFEQ      *BLANK
084600960419     C                   CLEAR                   DS036
084700960419     C                   MOVEL     LOGNME        JNAM36
084800960419     C                   MOVEL     '*ALL'        UNAM36
084900960419     C                   MOVEL     '*ALL'        JNBR36
085000960419     C                   CALL      'BAK036R'                            99
085100960419     C                   PARM                    DS036
085200960419     C                   MOVEL     JOBQ36        S1CODA
085300960419     C                   ENDIF
085400021104      *                  ==========
085500940314     C                   SELECT
085600940322     C     D0TIAN        WHENEQ    'C'
085700021104     C                   if        s1coda<d0cinp_low or
085800021104     C                             s1coda>d0cinp_hig
085900021104     C                   eval      *in50=*on
086000021104     C                   endif
086100021104     C*    D0CINP        IFNE      S1CODA
086200021104     C*                  SETON                                        50
086300021104     C*                  ENDIF
086400940322     C     D0TIAN        WHENEQ    'A'
086500940322     C     D0TIAN        OREQ      'E'
086600021104     C                   if        logcbe<d0cinp_low or
086700021104     C                             logcbe>d0cinp_hig
086800021104     C                   eval      *in50=*on
086900021104     C                   endif
087000021104     C*    D0CINP        IFNE      LOGCBE
087100021104     C*                  SETON                                        50
087200021104     C*                  ENDIF
087300940314     C                   ENDSL
087400940311     C                   ENDIF
087500021104      *&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
087600021104      *
087700021104      *
087800940311      * Ora
087900940311     C  N50D0ORA         IFNE      *ZERO
088000940311     C     D0MIN         ORNE      *ZERO
088100940311     C     D0SEC         ORNE      *ZERO
088200940314     C                   Z-ADD     D0ORA         GG
088300940314     C                   Z-ADD     D0MIN         MM
088400940314     C                   Z-ADD     D0SEC         AA
088500991014     C                   Z-ADD     D0GG          GGV
088600991014     C                   Z-ADD     D0MM          MMV
088700991014     C                   Z-ADD     D0AA          AAV
088800991018     C     AAV           IFLT      40
088900991018     C                   Z-ADD     1             SSV
089000991018     C                   ELSE
089100991018     C                   Z-ADD     0             SSV
089200991018     C                   ENDIF
089300991018      *
089400940314     C                   SELECT
089500940322     C     D0TIAN        WHENEQ    'C'
089600991018     C     LOGORC        IFLT      DATA
089700991018     C     LOGDAF        ANDLE     DATAV
089800940314     C                   SETON                                        50
089900940314     C                   ENDIF
090000021127     C     D0TIAN        WHENEQ    'A'
090100021127     C     LOGORI        IFLT      DATA
090200021127     C     LOGDAF        ANDLE     DATAV
090300021127     C                   SETON                                        50
090400021127     C                   ENDIF
090500940322     C     D0TIAN        WHENEQ    'E'
090600940314     C     LOGORF        IFLT      DATA
090700991014     C     LOGDAF        ANDLE     DATAV
090800940314     C                   SETON                                        50
090900940314     C                   ENDIF
091000940314     C                   ENDSL
091100940311     C                   ENDIF
091200021127      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
091300021127      * %%%%%%%
091400021127     C                   if        D0TIAN='C'
091500021127     C                   if        $wildQueue
091600021127     C                   eval      *in50=*in50
091700021127     C                             or  LOGDAF<>0 or logorf<>0
091800021127     C                             or  LOGDAI<>0 or logori<>0
091900021127     C                   endif
092000021127     C                   if        $wildUser
092100021127     C                   eval      *in50=*in50
092200021127     C                             or  LOGDAF<>0 or logorf<>0
092300021127     C                             or  LOGDAI<>0 or logori<>0
092400021127     C                   endif
092500021127     C                   endif
092600021127      *
092700021127     C                   if        D0TIAN='A'
092800021127     C                   if        $wildUser
092900021127     C                   eval      *in50=*in50 or (LOGUSR>d0prut_hig)
093000021127     C                   eval      *in50=*in50 or logdaf<>0
093100021127     C                   eval      *in50=*in50 or logdai= 0
093200021127     C                   endif
093300021127     C                   endif
093400021127      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
093500940311     C                   ENDSR
093600940310      *
093700940311      *----------------------------------------------------*
093800940311      * VALS1 - VALORIZZA CAMPI SUBFILE AZIONI             *
093900940311      *----------------------------------------------------*
094000940311      *
094100940311     C     VALS1         BEGSR
094200960329     C     STJ           IFEQ      'H'
094300960329     C                   MOVE      'H'           S1HLD
094400960329     C                   ELSE
094500960329     C                   CLEAR                   S1HLD
094600960329     C                   END
094700940406     C     LOGATT        IFEQ      'E'
094800940406     C                   MOVE      'E'           S1ERR
094900940406     C                   ELSE
095000940406     C                   CLEAR                   S1ERR
095100940406     C                   ENDIF
095200940311     C                   MOVEL     LOGDEA        S1DESA
095300961119     C                   MOVEL     LOGDEA        S1DESA
095400991014     C* SE AZIONE ANNULLATA LO SEGNALA NELLA DESCRIZIONE
095500991014     C     LOGATT        IFEQ      'A'
095600991014     C                   MOVEL     'A'           S1ERR
095700991014     C                   END
095800960129     C                   MOVEL     LOGDEA        S1DES
095900940311     C                   MOVEL     NPU           S1PRUT
096000940311     C                   MOVEL     NSI           S1SINF
096100940314     C                   Z-ADD     NRL           S1NUML
096200940316     C                   Z-ADD     LOGNRA        S1NRAZ
096300940314     C                   MOVEL     LOGNME        S1NOML
096400940315     C                   MOVEL     PTY           S1PTY
096500940311     C                   SELECT
096600940322     C     D0TIAN        WHENEQ    'C'
096700941024     C                   MOVEL     JBUF          SCHEDS
096800941024     C     SCHEDS        IFNE      *BLANK
096900951124     C     SCHEDS        ANDNE     *ALL'0'
097000941024     C                   MOVEL     'S'           S1ERR
097100941024     C                   ENDIF
097200940311     C                   Z-ADD     LOGDAC        DATA
097300940311     C                   EXSR      INVDAT
097400940311     C                   Z-ADD     DATA          S1DATA
097500940311     C                   Z-ADD     LOGORC        S1ORA
097600940311     C                   MOVEL     LOGCBI        S1CODA
097700951114     C     S1CODA        IFEQ      *BLANK
097800951124     C     S1PTY         OREQ      *BLANK
097900951114     C                   CLEAR                   DS036
098000951114     C                   MOVEL     LOGNME        JNAM36
098100951114     C                   MOVEL     '*ALL'        UNAM36
098200951114     C                   MOVEL     '*ALL'        JNBR36
098300951114     C                   CALL      'BAK036R'                            99
098400951114     C                   PARM                    DS036
098500951114     C                   MOVEL     JOBQ36        S1CODA
098600951124     C                   MOVEL     JOBP36        S1PTY
098700951114     C                   ENDIF
098800940322     C     D0TIAN        WHENEQ    'A'
098900940311     C                   Z-ADD     LOGDAI        DATA
099000940311     C                   EXSR      INVDAT
099100940311     C                   Z-ADD     DATA          S1DATA
099200940311     C                   Z-ADD     LOGORI        S1ORA
099300940311     C                   MOVEL     LOGCBE        S1CODA
099400940322     C     D0TIAN        WHENEQ    'E'
099500940311     C                   Z-ADD     LOGDAF        DATA
099600940311     C                   EXSR      INVDAT
099700940311     C                   Z-ADD     DATA          S1DATA
099800940311     C                   Z-ADD     LOGORF        S1ORA
099900940311     C                   MOVEL     LOGCBE        S1CODA
100000940311     C                   ENDSL
100100020516X0127X*****PRO850        IFEQ      'U'
100200020516  "  C* se pgm richiamato in modalità "analisi azioni utente"
100300020516  "  C*  l'utente non può agire sulle azioni di altri utenti
100400020516X0127C     $PgmAziUte    IFEQ      *On
100500940418     C     NPU           ANDNE     KNMUS
100600940418     C                   SETON                                        76
100700940418     C                   MOVE      *ON           S1PROT
100800940418     C                   ELSE
100900940418     C                   SETOFF                                       76
101000940418     C                   MOVE      *OFF          S1PROT
101100940418     C                   ENDIF
101200940311     C                   ENDSR
101300940322      *
101400940314      *----------------------------------------------------*
101500940314      * CTRS1 - CONTROLLO OPZIONI                          *
101600940314      *----------------------------------------------------*
101700940322      *
101800940314     C     CTRS1         BEGSR
101900951108     C                   Z-ADD     0             $OPZ
102000960206     C                   MOVE      ' '           PRIMO7
102100940322     C                   SETOFF                                       50
102200951109     C                   SETOFF                                       10
102300940314     C                   READC     K850S1                                 40
102400940314     C     *IN40         DOWEQ     '0'
102500940322     C                   SETOFF                                       9975
102600020516X0127C* se pgm richiamato in modalità "analisi azioni utente" e
102700020516  "  C*  l'utente ha possibilità limitate --> è abilitata solo opz.5
102800020516  "  C                   If        $PgmAziUte = *On and
102900020516  "  C                             PRFAAU = '1' and
103000020516  "  C                             S1Opz <> *Blank and S1Opz <> '5'
103100020516  "  C                   SetOn                                        9974
103200020516X0127C                   Else
103300940314     C                   SELECT
103400021028      * 1=Gestione messaggi
103500021028     C
103600021028     C     S1OPZ         WHENEQ    '1'
103700021028     C                   CALL      'BAK220'
103800021028     C                   PARM                    S1NRAZ
103900940314      * 2=Modifica
104000940314     C     S1OPZ         WHENEQ    '2'
104100951107     C                   Z-ADD     2             $OPZ
104200941026     C     S1NRAZ        CHAIN(N)  KPLOG01L                           99
104300991014     C  N99LOGATT        IFEQ      'A'
104400991014     C                   SETON                                        99
104500991014     C                   END
104600940323     C  N50
104700940323     CAN 99              SETON                                        82
104800940323     C     *IN99         IFEQ      '0'
104900940323     C                   MOVEL     LOGKPJ        JBADS
105000940323     C     TIA           IFEQ      'B'
105100940321     C                   MOVEL     S1NOML        D4NOML
105200951103     C                   MOVE      S1CODA        D4NEWC
105300951103     C                   MOVE      S1PTY         D4NEWP
105400951114     C     JBUF          IFNE      *BLANK
105500951109     C                   MOVEL     JBUF          DATORA
105600951114     C                   ELSE
105700951114     C                   CLEAR                   DATS
105800951114     C                   CLEAR                   ORAS
105900951114     C                   ENDIF
106000951114     C     DATAA         IFNE      *BLANK
106100951114     C                   MOVE      GG4           D4AA
106200951114     C                   MOVE      MM4           D4MM
106300951114     C                   MOVE      AA4           D4GG
106400951114     C                   ENDIF
106500951114     C     ORAA          IFNE      *BLANK
106600951114     C                   MOVE      ORA4          D4ORA
106700951114     C                   MOVE      MIN4          D4MIN
106800951114     C                   ENDIF
106900940317     C     *INKL         DOUEQ     '1'
107000940321     C     *INKC         OREQ      '1'
107100940321     C     *IN80         OREQ      '0'
107200951107     C     *IN81         ANDEQ     '0'
107300951107     C     *IN77         ANDEQ     '0'
107400951107     C     *IN78         ANDEQ     '0'
107500951108     C     *IN79         ANDEQ     '0'
107600960222     C                   SETOFF                                       07
107700940317     C                   EXFMT     K850D4
107800940317     C     *INKL         IFEQ      '0'
107900940317     C                   EXSR      CTRD4
108000951106      * Inversione Data parzializzazione
108100951114     C     D4GG          IFNE      0
108200951114     C     D4MM          ORNE      0
108300951114     C     D4GG          ORNE      0
108400951106     C                   Z-ADD     D4GG          AA4
108500951106     C                   Z-ADD     D4MM          MM4
108600951106     C                   Z-ADD     D4AA          GG4
108700951114     C                   ENDIF
108800951106      * Ora
108900951114     C     D4ORA         IFNE      *ZERO
109000951114     C     D4MIN         ORNE      *ZERO
109100951114     C     D4SEC         ORNE      *ZERO
109200951107     C                   Z-ADD     D4ORA         ORA4
109300951107     C                   Z-ADD     D4MIN         MIN4
109400951107     C                   Z-ADD     D4SEC         SEC4
109500951114     C                   ENDIF
109600951107     C*
109700940318     C     *IN80         IFEQ      '0'
109800951107     C     *IN81         ANDEQ     '0'
109900951107     C     *IN77         ANDEQ     '0'
110000951107     C     *IN78         ANDEQ     '0'
110100951108     C     *IN79         ANDEQ     '0'
110200940321     C                   MOVE      CMD(2)        CMD2
110300940321     C                   MOVEL     LOGKPJ        JBADS
110400940321     C                   MOVEL     LOGNME        NOMLAV
110500940321     C                   MOVEL     D4NEWP        NEWPR
110600940321     C                   MOVEL     D4NEWC        NEWJQ
110700940323     C                   CALL      'QCMDEXC'                            99
110800940318     C                   PARM                    CMD2
110900951109     C                   PARM      80            LUNG
111000951109     C  N50
111100951109     CAN 99              SETON                                        82
111200951109     C     DATS          IFNE      *ZERO
111300951114     C     ORAS          ORNE      *ZERO
111400951109     C                   SETON                                        10
111500951109     C* Data / Ora schedulazione
111600951109     C                   MOVE      CDS(1)        CMD3
111700951109     C                   MOVEL     LOGKPJ        JBADS
111800951109     C                   MOVEL     LOGNME        NMLAV
111900951114     C                   CLEAR                   NEWDS
112000951114     C                   CLEAR                   NEWOS
112100951109     C     DATS          IFNE      *ZERO
112200951114     C                   MOVEL     DATS          DATA
112300951114     C                   EXSR      INVDAT
112400951114     C                   MOVEL     DATA          NEWDS
112500951109     C                   ENDIF
112600951114     C     ORAS          IFNE      *ZERO
112700951109     C                   MOVEL     ORAS          NEWOS
112800951114     C                   ENDIF
112900951109     C                   CALL      'QCMDEXC'                            99
113000951109     C                   PARM                    CMD3
113100951109     C                   PARM      80            LUNG
113200940323     C  N50
113300940323     CAN 99              SETON                                        82
113400951109     C                   ENDIF
113500940323     C     *IN99         IFEQ      '0'
113600941026     C     S1NRAZ        CHAIN     KPLOG01L                           99
113700940323     C                   MOVEL     D4NEWC        LOGCBI
113800940323     C                   MOVEL     D4NEWP        PTY
113900951107     C                   MOVEL     DATORA        JBUF
114000951109     C     *IN10         IFEQ      '1'
114100951109     C                   MOVEL     'S'           STJ
114200951109     C                   ENDIF
114300940323     C                   MOVEL     JBADS         LOGKPJ
114400941026     C  N99              UPDATE    KPLOG001
114500940321     C                   ENDIF
114600940318     C                   ENDIF
114700940317     C                   ENDIF
114800940317     C                   ENDDO
114900940323     C                   ELSE
115000940323     C                   SETON                                        99
115100940323     C  N50              SETON                                        89
115200940323     C                   ENDIF
115300940323     C                   ENDIF
115400940318     C   KL              SETOFF                                       KL
115500990831X0024C                   MOVE      '1'           *INKE
115600940314      * 3=Congelamento
115700940314     C     S1OPZ         WHENEQ    '3'
115800940323     C     S1NRAZ        CHAIN     KPLOG01L                           99
115900991014     C  N99LOGATT        IFEQ      'A'
116000991014     C                   SETON                                        99
116100991014     C                   END
116200940323     C  N50
116300940323     CAN 99              SETON                                        83
116400940323     C     *IN99         IFEQ      '0'
116500940321     C                   MOVE      CMD(3)        CMD2
116600940321     C                   MOVEL     LOGKPJ        JBADS
116700940321     C     TIA           IFEQ      'B'
116800940321     C                   MOVEL     LOGNME        NOMLAV
116900940321     C                   ELSE
117000940321     C                   EXSR      VALCMD
117100940321     C                   ENDIF
117200940323     C                   CALL      'QCMDEXC'                            99
117300940318     C                   PARM                    CMD2
117400951109     C                   PARM      80            LUNG
117500940323     C  N50
117600940323     CAN 99              SETON                                        83
117700940323     C     *IN99         IFEQ      '0'
117800940321     C                   MOVEL     LOGKPJ        JBADS
117900940321     C                   MOVEL     'H'           STJ
118000940321     C                   MOVEL     JBADS         LOGKPJ
118100940323     C                   UPDATE    KPLOG001
118200940321     C                   ENDIF
118300940318     C                   ENDIF
118400940314      * 4=Annullamento
118500940314     C     S1OPZ         WHENEQ    '4'
118600951124     C     D0TIAN        ANDNE     'E'
118700951114     C  NKF              SETON                                        419975
118800940323     C     S1NRAZ        CHAIN     KPLOG01L                           99
118900991014     C  N99LOGATT        IFEQ      'A'
119000991014     C                   SETON                                        99
119100991014     C                   END
119200940323     C  N50
119300940323     CAN 99              SETON                                        84
119400951106     C     *IN99         IFEQ      '0'
119500970317     C                   MOVEL     LOGKPJ        JBADS
119600951106     C     *INKF         IFEQ      '1'
119700970317     C*
119800970317     C     D0TIAN        IFEQ      'C'
119900970317     C* SE ANALISI AZIONI IN CODA ANNULLO SOLO SE NON È ATTIVO
120000970317     C                   EXSR      CHKACT
120100970317     C*
120200970317     C     ACTIVE        IFEQ      *ON
120300970317     C                   UNLOCK    KPLOG01L
120400970317     C                   GOTO      A001
120500970317     C                   ENDIF
120600970317     C                   ENDIF
120700970317     C*
120800951106     C                   MOVE      CMD(4)        CMD2
120900940321     C                   MOVEL     LOGKPJ        JBADS
121000940321     C     TIA           IFEQ      'B'
121100940321     C                   MOVEL     LOGNME        NOMLAV
121200940321     C                   ELSE
121300940321     C                   EXSR      VALCMD
121400940321     C                   ENDIF
121500951106     C                   CALL      'QCMDEXC'                            99
121600940318     C                   PARM                    CMD2
121700951109     C                   PARM      80            LUNG
121800940323     C  N50
121900940323     CAN 99              SETON                                        84
122000940318     C                   MOVEL     'A'           LOGATT
122100940318     C                   TIME                    ORA               6 0
122200940321     C                   Z-ADD     ORA           LOGORF
122300951109     C                   Z-ADD     UDATE         DATA
122400940318     C                   EXSR      INVDAT
122500940318     C                   Z-ADD     DATA          LOGDAF
122600991018      * se l' anno è inferiore a 40 assumo che siamo nel XXI° secolo
122700991018     C     GG            IFLT      40
122800991018     C                   MOVEL     1             LOGDAF
122900991018     C                   ENDIF
123000940321     C     LOGDAI        IFEQ      0
123100940321     C                   Z-ADD     LOGDAF        LOGDAI
123200940321     C                   Z-ADD     ORA           LOGORI
123300951106     C                   ENDIF
123400951106     C                   UPDATE    KPLOG001
123500951106     C                   ENDIF
123600940318     C                   ENDIF
123700970317     C     A001          TAG
123800940314      * 5=Visualizzazione
123900940314     C     S1OPZ         WHENEQ    '5'
124000941026     C     S1NRAZ        CHAIN(N)  KPLOG01L                           99
124100991014     C  N99LOGATT        IFEQ      'A'
124200991014     C                   SETON                                        99
124300991014     C                   END
124400940323     C  N50
124500940323     CAN 99              SETON                                        85
124600940323     C     *IN99         IFEQ      '0'
124700940316     C                   EXSR      VALD3
124800940315     C                   EXFMT     K850D3
124900940315     C                   ENDIF
125000940322     C   KL              SETOFF                                       KL
125100940314      * 6=Rilascio
125200940314     C     S1OPZ         WHENEQ    '6'
125300940323     C     S1NRAZ        CHAIN     KPLOG01L                           99
125400991014     C  N99LOGATT        IFEQ      'A'
125500991014     C                   SETON                                        99
125600991014     C                   END
125700940323     C  N50
125800940323     CAN 99              SETON                                        86
125900940323     C     *IN99         IFEQ      '0'
126000940321     C                   MOVE      CMD(6)        CMD2
126100940321     C                   MOVEL     LOGKPJ        JBADS
126200940321     C     TIA           IFEQ      'B'
126300940321     C                   MOVEL     LOGNME        NOMLAV
126400940321     C                   ELSE
126500940321     C                   EXSR      VALCMD
126600940321     C                   ENDIF
126700940323     C                   CALL      'QCMDEXC'                            99
126800940318     C                   PARM                    CMD2
126900951109     C                   PARM      80            LUNG
127000940323     C  N50
127100940323     CAN 99              SETON                                        86
127200940323     C     *IN99         IFEQ      '0'
127300940321     C                   MOVEL     LOGKPJ        JBADS
127400940321     C                   MOVEL     ' '           STJ
127500940321     C                   MOVEL     JBADS         LOGKPJ
127600940323     C                   UPDATE    KPLOG001
127700940321     C                   ENDIF
127800940318     C                   ENDIF
127900960206      * 7=Modifica MULTIPLA
128000960206     C     S1OPZ         WHENEQ    '7'
128100991014     C     S1ERR         IFNE      'A'
128200960206     C                   Z-ADD     7             $OPZ
128300960206B1   C     PRIMO7        IFEQ      ' '
128400960206     C                   CLEAR                   K850D4
128500960206     C                   CLEAR                   DATS
128600960206     C                   CLEAR                   ORAS
128700960206B2   C     *INKL         DOUEQ     '1'
128800960206     C     *INKC         OREQ      '1'
128900960206     C     *IN80         OREQ      '0'
129000960206     C     *IN81         ANDEQ     '0'
129100960206     C     *IN77         ANDEQ     '0'
129200960206     C     *IN78         ANDEQ     '0'
129300960206     C     *IN79         ANDEQ     '0'
129400960222     C                   SETON                                        07
129500960222     C                   EXFMT     K850D4
129600960206B3   C     *INKL         IFEQ      '0'
129700960206     C                   EXSR      CTRD4
129800960206E3   C                   ENDIF
129900960206E2   C                   ENDDO
130000960206     C*
130100960206     C     *INKL         IFEQ      '0'
130200960206     C     *INKC         ANDEQ     '0'
130300960206     C                   MOVE      '1'           PRIMO7            1
130400960206     C                   ENDIF
130500960206E1   C                   ENDIF
130600960206     C*
130700960206B1   C     *INKL         IFEQ      '0'
130800960206     C     *INKC         ANDEQ     '0'
130900960215     C     S1NRAZ        CHAIN(N)  KPLOG01L                           99
131000960206     C                   MOVE      CMD(2)        CMD2
131100960222     C                   CLEAR                   PTYBLK
131200960206     C                   MOVEL     LOGKPJ        JBADS
131300960206     C                   MOVEL     LOGNME        NOMLAV
131400960206     C                   MOVEL     D4NEWC        NEWJQ
131500960206     C                   CALL      'QCMDEXC'                            99
131600960222     C                   PARM                    CMD2
131700960206     C                   PARM      80            LUNG
131800960206     C  N50
131900960206     CAN 99              SETON                                        82
132000960206     C*
132100960206     C     S1NRAZ        CHAIN     KPLOG01L                           99
132200960206     C                   MOVEL     D4NEWC        LOGCBI
132300960206B2   C     *IN10         IFEQ      '1'
132400960206     C                   MOVEL     'S'           STJ
132500960206E2   C                   ENDIF
132600960206     C  N99              UPDATE    KPLOG001
132700960206B1   C                   ENDIF
132800960206     C   KL              SETOFF                                       KL
132900991014     C                   END
133000940323      * 8=DSPJOB
133100940314     C     S1OPZ         WHENEQ    '8'
133200941026     C     S1NRAZ        CHAIN(N)  KPLOG01L                           99
133300991014     C  N99LOGATT        IFEQ      'A'
133400991014     C                   SETON                                        99
133500991014     C                   END
133600940323     C  N50
133700940323     CAN 99              SETON                                        85
133800940323     C     *IN99         IFEQ      '0'
133900940321     C                   MOVE      CMD(5)        CMD2
134000940321     C                   MOVEL     LOGKPJ        JBADS
134100940321     C     TIA           IFEQ      'B'
134200940321     C                   MOVEL     LOGNME        NOMLAV
134300940321     C                   ELSE
134400940318     C                   EXSR      VALCMD
134500940321     C                   ENDIF
134600940323     C                   CALL      'QCMDEXC'                            99
134700940314     C                   PARM                    CMD2
134800951109     C                   PARM      80            LUNG
134900940323     C  N50
135000940323     CAN 99              SETON                                        85
135100940321     C                   ENDIF
135200940314      * 9=Riesecuzione
135300940314     C     S1OPZ         WHENEQ    '9'
135400941026     C     S1NRAZ        CHAIN(N)  KPLOG01L                           99
135500991014     C  N99LOGATT        IFEQ      'A'
135600991014     C                   SETON                                        99
135700991014     C                   END
135800940323     C  N50
135900940323     CAN 99              SETON                                        87
136000940323     C     *IN99         IFEQ      '0'
136100940321     C                   MOVEL     LOGKPJ        JBADS
136200940321     C     TIA           IFEQ      'B'
136300940323     C     LOGDAF        IFNE      0
136400940322     C                   MOVEL     KPJBA         JBADS
136500020516      *
136600940321     C                   MOVEL     LOGKPJ        KPJBA
136700960206     C                   CALL      'BAK850R1'
136800960206     C                   PARM                    KPJBU
136900960206     C                   PARM      '0'           EXIT              1
137000960206     C     EXIT          IFEQ      '0'
137100940418     C                   CALL      'BCH09'
137200940321     C                   PARM                    KPJBA
137300940418     C                   CALL      'BCH10'
137400940418     C                   PARM                    KPJBA
137500020516      *
137600960206     C                   ENDIF
137700960206     C                   MOVEL     JBADS         KPJBA
137800940321     C                   ELSE
137900940323     C                   SETON                                        99
138000940323     C  N50              SETON                                        88
138100940321     C                   ENDIF
138200940323     C                   ELSE
138300940323     C                   SETON                                        99
138400940323     C  N50              SETON                                        89
138500940323     C                   ENDIF
138600940321     C                   ENDIF
138700940314     C                   ENDSL
138800020516X0127C                   EndIf
138900940322      * Gestione Errore
139000940321     C     *IN99         IFEQ      '1'
139100940321     C  N50              Z-ADD     S1NRR         C1NRR
139200940321     C  N50              SETON                                        50
139300940321     C                   SETON                                        75
139400940321     C                   ELSE
139500951114     C  N41              MOVE      *BLANK        S1OPZ
139600940321     C                   ENDIF
139700021111     C                   exsr      kmsg
139800951114     C                   UPDATE    K850S1
139900940314     C                   READC     K850S1                                 40
140000991014     C**
140100991014     C** A FINE LETTURA DEL SFL REIMPOSTA I CAMPI SPORCATI
140200991014     C** DA UNA EVENTUALE SCELTA DI OPZIONE...
140300991014     C     *IN40         IFEQ      '1'
140400991014     C                   MOVE      SAVATT        LOGATT
140500991014     C                   MOVE      SAVNRA        LOGNRA
140600991014     C                   MOVE      SAVNME        LOGNME
140700991014     C                   MOVE      SAVCBI        LOGCBI
140800991014     C                   MOVE      SAVCBE        LOGCBE
140900991014     C                   MOVE      SAVDAC        LOGDAC
141000991014     C                   MOVE      SAVORC        LOGORC
141100991014     C                   MOVE      SAVDAI        LOGDAI
141200991014     C                   MOVE      SAVORI        LOGORI
141300991014     C                   MOVE      SAVDAF        LOGDAF
141400991014     C                   MOVE      SAVORF        LOGORF
141500991014     C                   MOVE      SAVDEA        LOGDEA
141600991014     C                   MOVEL     SAVKP1        LOGKPJ
141700991014     C                   MOVE      SAVKP2        LOGKPJ
141800991014     C                   MOVE      SAVSTP        LOGSTP
141900991014     C                   ENDIF
142000951108     C                   ENDDO
142100940314     C                   ENDSR
142200940317      *
142300940317      *----------------------------------------------------*
142400940317      * CTRD4 - CONTR. WINDOWS MODIFICA CODA E PRIORITA'   *
142500940317      *----------------------------------------------------*
142600940317      *
142700940317     C     CTRD4         BEGSR
142800951107     C*
142900951107     C                   SETOFF                                       8081
143000951107     C                   SETOFF                                       7778
143100951108     C                   SETOFF                                       7379
143200940317     C                   MOVE      CMD(1)        CMD1
143300940317     C                   MOVEL     D4NEWC        NOME
143400940317     C                   MOVEL     '*JOBQ  '     TIPO
143500940318     C                   CALL      'QCMDEXC'                            80
143600940317     C                   PARM                    CMD1
143700951109     C                   PARM      80            LUNG
143800021001      * ---------------------------------------------
143900021001      * exit x controllo se la coda va bene
144000021001      * ---------------------------------------------
144100021001     c     *in80         ifeq      '1'
144200021001av1  C                   call      'BAK850U2'                           99
144300021111av1  C                   parm                    KPJBA
144400021001av1  C                   parm                    D4NEWC
144500021127av1  C                   parm      *blanks       okko
144600021001     C     *in99         ifeq      '0'
144700021111     C     okko          ifeq      'KO'
144800021001     C                   move      '1'           *in80
144900021001     C                   end
145000021001     C                   end
145100021001     C                   end
145200021001      * ---------------------------------
145300951108      * Controllo Priorità
145400960222     C  N07D4NEWP        IFNE      '6'
145500951107     C     D4NEWP        ANDNE     '5'
145600951107     C     D4NEWP        ANDNE     '4'
145700960329     C     D4NEWP        ANDNE     '7'
145800990831X0024C     D4NEWP        ANDNE     '8'
145900990831X0024C     D4NEWP        ANDNE     '9'
146000951107     C                   SETON                                        81
146100951107     C                   ENDIF
146200951107      * Controllo Data
146300960222     C  N07D4AA          IFNE      0
146400951114     C     D4MM          ORNE      0
146500951114     C     D4GG          ORNE      0
146600951107     C     D4MM          IFGT      12
146700951107     C     D4MM          ORLT      1
146800951107     C                   SETON                                        78
146900951107     C                   ELSE
147000951107     C     D4MM          IFEQ      2
147100951107     C     D4AA          DIV       4             RIS               2 0
147200951107     C                   MVR                     RES               1 0
147300951107     C     RES           IFEQ      0
147400951107     C                   Z-ADD     29            MES(2)
147500951107     C                   ELSE
147600951107     C                   Z-ADD     28            MES(2)
147700951107     C                   ENDIF
147800951107     C                   ENDIF
147900951107     C     D4GG          IFGT      MES(D4MM)
148000951107     C     D4GG          ORLT      1
148100951107     C                   SETON                                        78
148200951107     C                   ENDIF
148300951107     C                   ENDIF
148400951114     C                   ENDIF
148500951107      * Controllo Ora
148600960222     C  N07D4ORA         IFNE      0
148700951114     C     D4MIN         ORNE      0
148800951107     C     D4ORA         IFGT      23
148900951107     C     D4MIN         ORGT      59
149000951107     C     D4SEC         ORGT      59
149100951107     C                   SETON                                        77
149200951107     C                   ENDIF
149300951114     C                   ENDIF
149400951108      *
149500960222     C  N07D4GG          IFEQ      *ZERO
149600951124     C     D4MM          ANDEQ     *ZERO
149700951124     C     D4AA          ANDEQ     *ZERO
149800951124     C     S1ERR         ANDEQ     'S'
149900951124     C                   SETON                                        73
150000951124     C                   ENDIF
150100951108      *
150200960222     C  N07D4ORA         IFEQ      *ZERO
150300951124     C     D4MIN         ANDEQ     *ZERO
150400951124     C     D4SEC         ANDEQ     *ZERO
150500951124     C     S1ERR         ANDEQ     'S'
150600951124     C                   SETON                                        79
150700951124     C                   ENDIF
150800940317     C                   ENDSR
150900940316      *
151000940316      *----------------------------------------------------*
151100940316      * VALD3 - VALORIZZAZIONE VIDEATA VISUALIZZ.          *
151200940316      *----------------------------------------------------*
151300940316      *
151400940316     C     VALD3         BEGSR
151500940316     C                   Z-ADD     LOGNRA        D3NRAZ
151600940316     C                   MOVEL     LOGDEA        D3DEAZ
151700940316     C                   MOVEL     LOGNME        D3NOML
151800940316     C                   MOVEL     LOGCBI        D3CBIN
151900940316     C                   MOVEL     LOGCBE        D3CBES
152000951107     C*
152100940316     C                   Z-ADD     LOGDAC        DATA
152200940316     C                   EXSR      INVDAT
152300940316     C                   Z-ADD     DATA          D3DINS
152400940316     C                   Z-ADD     LOGORC        D3OINS
152500951107     C*
152600960329     C                   MOVEL     LOGKPJ        JBADS
152700960329     C     JBUF          IFNE      *BLANK
152800960329     C                   MOVEL     JBUF          SCHEDS
152900960329     C                   ELSE
153000960329     C                   CLEAR                   SCHEDS
153100960329     C                   ENDIF
153200960329     C                   MOVEL     DSCHE         DATA
153300951114     C                   EXSR      INVDAT
153400951114     C                   Z-ADD     DATA          D3DSCH
153500941024     C                   MOVEL     OSCHE         D3OSCH
153600951107     C*
153700940316     C                   Z-ADD     LOGDAI        DATA
153800940316     C                   EXSR      INVDAT
153900940316     C                   Z-ADD     DATA          D3DINI
154000940316     C                   Z-ADD     LOGORI        D3OINI
154100940316     C                   Z-ADD     LOGDAF        DATA
154200940316     C                   EXSR      INVDAT
154300940316     C                   Z-ADD     DATA          D3DFIN
154400940316     C                   Z-ADD     LOGORF        D3OFIN
154500940317     C                   MOVEL     LOGKPJ        JBADS
154600940316     C                   MOVEL     CDA           D3COAZ
154700940316     C                   MOVEL     NPU           D3PRUT
154800940316     C                   MOVEL     NSI           D3SINF
154900940316     C                   Z-ADD     NRL           D3NUML
155000940316     C                   MOVEL     NTM           D3TERM
155100940316     C                   MOVEL     STJ           D3STJB
155200940316     C                   MOVEL     PTY           D3PRJB
155300960205     C*
155400960205     C                   MOVEA     JBU1          JB
155500960205     C                   EXSR      JJ
155600960206     C                   MOVEA     JB            D3JBU1
155700960205     C*
155800960205     C                   MOVEA     JBU2          JB
155900960205     C                   EXSR      JJ
156000960206     C                   MOVEA     JB            D3JBU2
156100960205     C*
156200960206     C                   MOVEA     JBU3          JB
156300960205     C                   EXSR      JJ
156400960206     C                   MOVEA     JB            D3JBU3
156500960206     C*
156600960206     C                   MOVEA     JBU4          JB
156700960206     C                   EXSR      JJ
156800960206     C                   MOVEA     JB            D3JBU4
156900960206     C                   MOVEA     JBU5          JB
157000960206     C                   EXSR      JJ
157100960206     C                   MOVEA     JB            D3JBU5
157200940316     C                   ENDSR
157300960205     C     JJ            BEGSR
157400960206     C                   DO        60            J                 3 0
157500960205     C     JB(J)         IFLT      X'40'
157600960205     C                   MOVE      X'1C'         JB(J)
157700960205     C                   ENDIF
157800960205     C                   ENDDO
157900960205     C                   ENDSR
158000940316      *
158100940316      *----------------------------------------------------*
158200940318      * VALCMD - VALORIZZ. DS COMANDI JOB (QCMDEXC)        *
158300940316      *----------------------------------------------------*
158400940316      *
158500940318     C     VALCMD        BEGSR
158600940321     C                   MOVEL     S1NUML        NUMJOB
158700940316     C                   MOVEA     *BLANK        SK1
158800940316     C                   MOVEA     S1PRUT        SK1(1)
158900940418     C                   Z-ADD     1             X                 3 0
159000940321     C     ' '           LOOKUP    SK1(X)                                 49
159100940316     C                   MOVEA     '/'           SK1(X)
159200940316     C                   ADD       1             X
159300940316     C                   MOVEA     S1NOML        SK1(X)
159400940316     C                   MOVEA     SK1           PRUNOJ
159500940316     C                   ENDSR
159600970317      *
159700970317      *----------------------------------------------------*
159800970317      * CHKACT - CONTROLLA CHE LAVORO NON SIA ATTIVO*
159900970317      *----------------------------------------------------*
160000970317      *
160100970317     C     CHKACT        BEGSR
160200970317     C                   CLEAR                   DS036
160300970317     C                   MOVEL     LOGNME        JNAM36
160400970317     C                   MOVEL     '*ALL'        UNAM36
160500970317     C                   MOVEL     '*ALL'        JNBR36
160600970317     C                   CALL      'BAK036R'                            99
160700970317     C                   PARM                    DS036
160800970317     C*
160900970317     C     JSTS36        IFEQ      '*ACTIVE'
161000970317     C                   MOVEL     *ON           ACTIVE            1
161100970317     C                   ELSE
161200970317     C* CONGELA
161300970317     C                   MOVE      CMD(3)        CMD2
161400970317     C                   MOVEL     LOGKPJ        JBADS
161500970317     C     TIA           IFEQ      'B'
161600970317     C                   MOVEL     LOGNME        NOMLAV
161700970317     C                   ELSE
161800970317     C                   EXSR      VALCMD
161900970317     C                   ENDIF
162000970317     C                   CALL      'QCMDEXC'                            99
162100970317     C                   PARM                    CMD2
162200970317     C                   PARM      80            LUNG
162300970317     C* RITESTA
162400970317     C                   MOVEL     *OFF          ACTIVE
162500970317     C                   CLEAR                   DS036
162600970317     C                   MOVEL     LOGNME        JNAM36
162700970317     C                   MOVEL     '*ALL'        UNAM36
162800970317     C                   MOVEL     '*ALL'        JNBR36
162900970317     C                   CALL      'BAK036R'                            99
163000970317     C                   PARM                    DS036
163100970317     C*
163200970317     C     JSTS36        IFEQ      '*ACTIVE'
163300970317     C                   MOVEL     *ON           ACTIVE            1
163400970317     C* SCONGELA
163500970317     C                   MOVE      CMD(6)        CMD2
163600970317     C                   MOVEL     LOGKPJ        JBADS
163700970317     C     TIA           IFEQ      'B'
163800970317     C                   MOVEL     LOGNME        NOMLAV
163900970317     C                   ELSE
164000970317     C                   EXSR      VALCMD
164100970317     C                   ENDIF
164200970317     C                   CALL      'QCMDEXC'                            99
164300970317     C                   PARM                    CMD2
164400970317     C                   PARM      80            LUNG
164500970317     C                   ENDIF
164600970317     C                   ENDIF
164700970317     C                   ENDSR
164800970317      *
164900940310      *----------------------------------------------------*
165000940310      * INVDAT - INVERSIONE DATA                           *
165100940310      *----------------------------------------------------*
165200940310      *
165300940310     C     INVDAT        BEGSR
165400940310     C                   Z-ADD     DATA          DATROV            6 0
165500940310     C                   MOVEL     AA            DATROV
165600940310     C                   MOVE      GG            DATROV
165700940310     C                   Z-ADD     DATROV        DATA
165800940310     C                   ENDSR
165900940316      *
166000951106      *----------------------------------------------------*
166100951106      * INVDAT1- INVERSIONE DATA SCHEDULAZIONE             *
166200951106      *----------------------------------------------------*
166300951106      *
166400951106     C     INVDA1        BEGSR
166500951106     C                   Z-ADD     DATS          DATRO1            6 0
166600951106     C                   MOVEL     AA4           DATRO1
166700951106     C                   MOVE      GG4           DATRO1
166800951106     C                   Z-ADD     DATRO1        DATS
166900951106     C                   ENDSR
167000951106      *
167100990910X0026 *----------------------------------------------------
167200990908  "  C* INIZIALIZZAZIONE TESTATA DEL VIDEO
167300990908  "   *----------------------------------------------------
167400990908  "  C     INZT1         BEGSR
167500990908  "   *
167600990908  "  C* Reperisco dati società
167700990908  "  C                   CLEAR                   XSOCDS
167800990908  "  C                   CALL      'BAK500R'
167900990908  "  C                   PARM                    XSOCDS
168000990908  "  C*
168100990908  "  C* Controlli dati società
168200990908  "  C     XSOCDS        IFNE      *BLANKS
168300990908  "  C                   MOVEL     XSOCDS        SOC001
168400990908  "  C                   ENDIF
168500990908  "  C*
168600990908  "  C* Valorizzazione campi univoci testate
168700990908  "  C                   MOVEL     KNSIF         NOMSIF
168800990908  "  C                   MOVEL     XSCDSI        NOMDIT
168900990908  "  C                   MOVEL     DSPGM         NOMPGM
169000990908  "   *
169100990908  "  C                   ENDSR
169200990910X0026C/EJECT
169300940309      *----------------------------------------------------*
169400940309      * DEFCAM - DEFINIZIONE CAMPI E KLIST                 *
169500940309      *----------------------------------------------------*
169600940309      *
169700930708     C     DEFCAM        BEGSR
169800991013      *
169900940309     C     *ENTRY        PLIST
170000940309     C                   PARM                    KPJBA
170100991013      *
170200990910X0026C                   EXSR      INZT1
170300991013      *
170400020516X0127X*****              MOVEL     KPJBU         PRO850            1
170500020516  "   * verifico se pgm chiamato in modalità "analisi azioni" (azione K850)
170600020516  "   *  oppure "analisi azioni utente" (azione K851)
170700020516  "  C                   If        KCDAZ = 'K851'
170800020516  "  C                   Eval      $PgmAziUte = *On
170900020516  "  C                   Else
171000020516  "  C                   Eval      $PgmAziUte = *Off
171100020516  "  C                   EndIf
171200020516X0127 *
171300951107     C                   Z-ADD     0             $OPZ              1 0
171400951108     C                   MOVE      *BLANK        $INZS1            1
171500991014      *
171600940310     C     *LIKE         DEFINE    LOGDAI        KDI
171700991014     C     *LIKE         DEFINE    LOGORI        KOI
171800940310     C     *LIKE         DEFINE    LOGDAF        KDF
171900991014     C     *LIKE         DEFINE    LOGORF        KOF
172000991014     C     *LIKE         DEFINE    LOGATT        SAVATT
172100991014     C     *LIKE         DEFINE    LOGNRA        SAVNRA
172200991014     C     *LIKE         DEFINE    LOGNME        SAVNME
172300991014     C     *LIKE         DEFINE    LOGCBI        SAVCBI
172400991014     C     *LIKE         DEFINE    LOGCBE        SAVCBE
172500991014     C     *LIKE         DEFINE    LOGDAC        SAVDAC
172600991014     C     *LIKE         DEFINE    LOGORC        SAVORC
172700991014     C     *LIKE         DEFINE    LOGDAI        SAVDAI
172800991014     C     *LIKE         DEFINE    LOGORI        SAVORI
172900991014     C     *LIKE         DEFINE    LOGDAF        SAVDAF
173000991014     C     *LIKE         DEFINE    LOGORF        SAVORF
173100991014     C     *LIKE         DEFINE    LOGDEA        SAVDEA
173200991014     C     *LIKE         DEFINE    LOGSTP        SAVSTP
173300951109     C     *LIKE         DEFINE    DATS          WWDAT
173400991013      *
173500021104av3  C     KLOG8         KLIST
173600021104av3  C                   KFLD                    KUS
173700021104av3  C                   KFLD                    KDF
173800021104av3  C                   KFLD                    KOF
173900021104av3  C                   KFLD                    KDI
174000021104av3  C                   KFLD                    KOI
174100021104av3  C     KLOG8A        KLIST
174200021104av3  C                   KFLD                    KUS
174300021104av3  C                   KFLD                    KDF
174400021104av3  C     KLOG8B        KLIST
174500021104av3  C                   KFLD                    KUS
174600021104      *
174700021104av3  C     KLOG9         KLIST
174800021104av3  C                   KFLD                    KQU
174900021104av3  C                   KFLD                    KDF
175000021104av3  C                   KFLD                    KOF
175100021104av3  C                   KFLD                    KDI
175200021104av3  C                   KFLD                    KOI
175300021104av3  C     KLOG9A        KLIST
175400021104av3  C                   KFLD                    KQU
175500021104av3  C                   KFLD                    KDF
175600021104av3  C     KLOG9B        KLIST
175700021104av3  C                   KFLD                    KQU
175800021104     C     KLOG2         KLIST
175900940310     C                   KFLD                    KDF
176000991014     C                   KFLD                    KOF
176100940310     C                   KFLD                    KDI
176200991014     C                   KFLD                    KOI
176300940311      * Default video Guida
176400020516X0127C* se pgm richiamato in modalità "analisi azioni utente"...
176500020516  "  X*****PRO850        IFEQ      'U'
176600020516  "  C     $PgmAziUte    IFEQ      *On
176700020516X0127C* - propongo il profilo
176800951107     C                   MOVEL     KNMUS         D0PRUT
176900020516X0127C* - se l'utente ha possibilità limitate --> proteggo profilo
177000020516  "  C                   If        PRFAAU = '1'
177100020516  "  C                   Eval      *In06 = *On
177200020515  "  C                   Else
177300020516  "  C                   Eval      *In06 = *Off
177400020515X0127C                   EndIf
177500951107     C                   ENDIF
177600940309     C                   Z-ADD     UDATE         DATA
177700940309     C                   Z-ADD     GG            D0GG
177800940309     C                   Z-ADD     MM            D0MM
177900940309     C                   Z-ADD     AA            D0AA
178000940421     C                   MOVE      'N'           D0ERR
178100951108     C                   MOVE      *OFF          $INZS1
178200001031X0097C                   Z-ADD     0             CURR              2 0
178300001031  "  C                   Z-ADD     0             CURC              2 0
178400001031  "  C                   Z-ADD     0             H1RIGA
178500001031X0097C                   Z-ADD     0             H1COLO
178600020515      *
178700930708     C                   ENDSR
178800001031X0097C***************************************************************
178900001031  "  C* RICERCHE
179000001031  "  C***************************************************************
179100001031X0097C     SEARCH        BEGSR
179200001031     C*
179300001031     C* determino Riga/Colonna del cursore
179400001106     C                   MOVE      BINRIG        R$$
179500001106     C                   MOVE      BINCOL        C$$
179600001031     C                   Z-ADD     RIRI          CURR
179700001031     C                   Z-ADD     COCO          CURC
179800001031     C*
179900001031     C                   SELECT
180000001031     C*
180100001031     C* Profilo
180200001031     C     H1NMFL        WHENEQ    'D0PRUT'
180300020516X0127C     *In06         ANDEQ     *Off
180400001031     C                   RESET                   K980DS
180500001031     C                   MOVE      '01'          OPZ980
180600001031     C                   MOVEL     K980DS        KPJBU
180700001031     C                   CALL      'BAK980R'
180800001031     C                   PARM                    KPJBA
180900001031     C                   MOVEL     KPJBU         K980DS
181000001031     C     PRF980        IFNE      *BLANK
181100001031     C                   MOVEL     PRF980        D0PRUT
181200001031     C                   ENDIF
181300001031     C*
181400001031     C* Sistema informativo
181500001031     C     H1NMFL        WHENEQ    'D0SINF'
181600001031     C                   RESET                   K988DS
181700001031     C                   MOVE      '01'          OPZ988
181800001031     C                   MOVEL     K988DS        KPJBU
181900001031     C                   CALL      'BAK988R'
182000001031     C                   PARM                    KPJBA
182100001031     C                   MOVEL     KPJBU         K988DS
182200001031     C     NSI988        IFNE      *BLANK
182300001031     C                   MOVEL     NSI988        D0SINF
182400001031     C                   ENDIF
182500001211X0099C*
182600001211  "  C* FMTD1 - Azione
182700001211  "  C     H1NMFL        WHENEQ    'D0AZN'
182800001211  "  C* se s.i. impostato correttamente eseguo OVRDBF in libl del s.i.
182900001211  "  C                   Eval      $OvrDB = *Off
183000001211  "  C                   If        D0SINF <> *Blank
183100001211  "  C                   CallB     'BAK003R1'
183200001211  "  C                   Parm                    D0SINF
183300001211  "  C                   Parm                    KFLRC
183400001211  "  C                   Parm                    LibL
183500001211  "  C                   Parm                    Errore
183600001211  "  C                   If        Errore = *Off
183700001211  "  C* eseguo OVRDBF file azioni
183800001211  "  C                   Eval      $OvrDB = *On
183900001211  "  C                   Eval      File = 'KFAZN10F'
184000001211  "  C                   Eval      SisInf = D0SINF
184100001211  "  C                   Eval      Lingua = KFLRC
184200001211  "  C                   Eval      Scope = 'J'
184300001211  "  C                   ExSr      ExeOVRDBF
184400001211  "  C                   EndIf
184500001211  "  C                   EndIf
184600001211  "  C* pgm ricerca azioni
184700001211  "  C                   Reset                   BAK986DS
184800001211  "  C                   Eval      Opz986 = '01'
184900001211  "  C                   MoveL     BAK986DS      KPJBU
185000001211  "  C                   CALL      'BAK986R'
185100001211  "  C                   PARM                    KPJBA
185200001211  "  C                   MoveL     KPJBU         BAK986DS
185300001211  "  C                   If        KCAZN986 <> *Blank
185400001211  "  C                   Eval      D0AZN = KCAZN986
185500001211  "  C                   EndIf
185600001211  "  C* se eseguito OVRDBF eseguo DLTOVR file azioni
185700001211  "  C                   If        $OvrDB = *On
185800001211  "  C                   ExSr      ExeDLTOVR
185900001211X0099C                   EndIf
186000001031     C*
186100001031     C* Ricerca non abilitata in questa posizione
186200001031     C                   OTHER
186300001031     C                   SETON                                        6899
186400001031     C*
186500001031     C                   ENDSL
186600001031     C* imposto pos. del cursore
186700001031     C                   Z-ADD     CURR          H1RIGA
186800001031     C                   Z-ADD     CURC          H1COLO
186900001031     C*
187000001031X0097C                   ENDSR
187100001211X0099C****************************************************************
187200001211  "  C* OVRDBF file in lista librerie S.I.                           *
187300001211  "  C****************************************************************
187400001211X0099C     ExeOVRDBF     BEGSR
187500001211     C*
187600001211     C                   Call      'BAK931R'
187700001211     C                   Parm                    File
187800001211     C                   Parm                    SisInf
187900001211     C                   Parm                    Lingua
188000001211     C                   Parm                    Scope
188100001211     C                   Parm                    Errore
188200001211     C*
188300001211X0099C                   ENDSR
188400001211X0099C****************************************************************
188500001211  "  C* DLTOVR file                                                  *
188600001211  "  C****************************************************************
188700001211X0099C     ExeDLTOVR     BEGSR
188800001211     C*
188900001211     C                   Call      'BAK932C'
189000001211     C                   Parm                    File
189100001211     C                   Parm                    Scope
189200001211     C*
189300001211X0099C                   ENDSR
189400020515X0127C****************************************************************
189500020515  "  C* Operazioni iniziali
189600020515  "  C****************************************************************
189700020515X0127C     *INZSR        BEGSR
189800020515     C*
189900020515     C* aggancio profilo per reperire possibilità dell'utente nel pgm
190000020516     C     KNMUS         Chain     KPPRF01L
190100020515     C*
190200020515X0127C                   ENDSR
190300021111X0127C****************************************************************
190400021111  "  C* Controlla se esistono messaggi
190500021111  "  C****************************************************************
190600021111X0127C     KMSG          BEGSR
190700021111      *
190800021111     C                   move      *off          *in55
190900021111av1  C                   move      '  '          okko
191000021111      *
191100021111av1  C                   call      'BAK850U4'                           99
191200021111av1  C                   parm                    S1NRAZ
191300021127av1  C                   parm      *blanks       okko
191400021111      *
191500021111     c     okko          ifeq      'OK'
191600021111     c                   move      *on           *in55
191700021111     c                   end
191800021111     C*
191900021111X0127C                   ENDSR
192000020905av1  C****************************************************************
192100020905av1  C* Controllo record in lettura
192200020905av1  C****************************************************************
192300020906av1  C     TESTCK        BEGSR
192400020905av1  C*
192500020905av1   * inserimento exit routine per controllo letture
192600020905av1  C     tcki          tag
192700020905av1  C                   setoff                                       99
192800020905av1  C                   move      '  '          okko              2
192900020905av1  C     *in72         ifeq      '0'
193000021001av1  C                   call      'BAK850U1'                           99
193100020905av1  C                   parm                    dslog
193200021127av1  C                   parm      *blanks       okko
193300020905av1  C     *in99         cabeq     '1'           tckf
193400020905av1   *
193500020905av1  C     okko          ifeq      'KO'
193600020905av1   * ri_Lettura per Tipo Analisi Azioni
193700020905av1  C                   SELECT
193800021111     C*---------------------------------------------------------------------
193900020905av1  C     D0TIAN        WHENEQ    'C'
194000021127     C                   if        d0prut<>*blank
194100021127     C                   if        d0cinp<>*blank
194200021127     C                   if        $wildQueue
194300021127     C                   READ      KPLOG09L                               72
194400021127     C                   eval      *in72=*in72 or (LOGCBE>d0cinp_hig)
194500021127     C                             or  LOGDAF<>0 or logorf<>0
194600021127     C                             or  LOGDAI<>0 or logori<>0
194700021127     C                   else
194800021127     C     KLOG9         READE     KPLOG09L                               72
194900021127     C                   endif
195000021111     C                   else
195100021127     C                   if        $wildUser
195200021127     C                   READ      KPLOG08L                               72
195300021127     C                   eval      *in72=*in72 or (LOGUSR>d0prut_hig)
195400021127     C                             or  LOGDAF<>0 or logorf<>0
195500021127     C                             or  LOGDAI<>0 or logori<>0
195600021127     C                   else
195700021104     C     KLOG8         READE     KPLOG08L                               72
195800021111     C                   endif
195900021127     C                   endif
196000021104     C                   else
196100020905av1  C     KLOG2         READE     KPLOG04L                               72
196200021104     C                   endif
196300021111     C*---------------------------------------------------------------------
196400020905av1  C     D0TIAN        WHENEQ    'A'
196500021127     C                   if        d0prut<>*blank
196600021127     C                   if        d0cinp<>*blank
196700021127     C                   if        $wildQueue
196800021127     C                   READ      KPLOG09L                               72
196900021127     C                   eval      *in72=*in72 or (LOGCBE>d0cinp_hig)
197000021127     C                   eval      *in72=*in72 or (LOGDAF>KDF       )
197100021127     C                   else
197200021127     C     KLOG9A        READE     KPLOG09L                               72
197300021127     C                   endif
197400021111     C                   else
197500021127     C                   if        $wildUser
197600021127     C                   READ      KPLOG08L                               72
197700021127     C                   eval      *in72=*in72 or (LOGUSR>d0prut_hig)
197800021127     C                   else
197900021104     C     KLOG8A        READE     KPLOG08L                               72
198000021111     C                   endif
198100021127     C                   endif
198200021104     C                   else
198300020905av1  C     KDF           READE     KPLOG04L                               72
198400021104     C                   endif
198500021111     C*---------------------------------------------------------------------
198600020905av1  C     D0TIAN        WHENEQ    'E'
198700021127     C                   if        d0prut<>*blank
198800021127     C                   if        d0cinp<>*blank
198900021127     C                   if        $wildQueue
199000021127     C                   READ      KPLOG09L                               72
199100021127     C                   eval      *in72=*in72 or (LOGCBE>d0cinp_hig)
199200021127     C                   else
199300021127     C     KLOG9B        READE     KPLOG09L                               72
199400021127     C                   endif
199500021111     C                   else
199600021127     C                   if        $wildUser
199700021127     C                   READ      KPLOG08L                               72
199800021127     C                   eval      *in72=*in72 or (LOGUSR>d0prut_hig)
199900021127     C                   else
200000021104     C     KLOG8B        READE     KPLOG08L                               72
200100021111     C                   endif
200200021127     C                   endif
200300021104     C                   else
200400020905av1  C                   READ      KPLOG04L                               72
200500021104     C                   endif
200600020905av1  C                   ENDSL
200700021111     C*---------------------------------------------------------------------
200800020905av1  C
200900020905av1  C                   goto      tcki
201000020905av1  C
201100020905av1  C                   endif
201200020905av1  C                   endif
201300020905av1   *
201400020905av1  C     tckf          tag
201500020905av1  C*
201600020905av1  C                   ENDSR
201700020905      *
201800940309** 'CMD' - Schiera per QCMDX
201900940309CHKOBJ OBJ(          ) OBJTYPE(       )
202000951109CHGJOB JOB(      /                     ) JOBPTY( )  JOBQ(          )
202100940318HLDJOB JOB(      /                     )
202200940318ENDJOB JOB(      /                     ) OPTION(*IMMED)
202300940318DSPJOB JOB(      /                     )
202400940318RLSJOB JOB(      /                     )
202500951109** 'CDS' - Schiera per QCMDX
202600951109CHGJOB JOB(      /                     ) SCDDATE(      ) SCDTIME(      )
202700940309** 'MES' - Schiera Mesi dell'anno
202800940309312831303130313130313031
